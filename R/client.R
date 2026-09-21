# Low-level TypeSafe Jev client: one 'system one' call = one state, many questions.
# Transport is pluggable (option Rjif.transport) so tests and demos run offline.
#
# Endpoint contract (POST https://api.typesafe.ai/v1/systemone):
#   headers:  Authorization: Bearer <TYPESAFE_API_KEY>, Content-Type: application/json
#   request:  {state, model, questions: {name: {type: noul|choice|score,
#                        instructions, criteria}}}
#   response: {model?,
#              answers: {name: {type,
#                               noul,                        # noul:   0-1 scalar
#                               choice, probabilities, confidence,  # choice
#                               score, legend,   confidence}},      # score
#              usage: {input_tokens, output_tokens}}
# Only input_tokens is billed; output tokens are free.
#
# The API key is never placed in any condition message, log line, or printed
# object. Error text carries the endpoint URL and status only.

.RjifEnv <- new.env(parent = emptyenv())
.RjifEnv$usage <- list(calls = 0L, input_tokens = 0L)

JEV_PRICE_PER_MTOK <- 0.042   # $/million input tokens; output tokens are free

# Contract tolerances (external audit finding 4; rationale at each constant).
# The API sends probabilities rounded to 2 decimals (live-verified
# 2026-09-19), so distribution comparisons need rounding-aware margins:
#   JEV_PROB_SUM_TOL  - |sum(probabilities) - 1| upper bound
#   JEV_WINNER_TOL    - how far the chosen choice-option may trail the top
#                       option before the answer is called self-contradictory
#                       (NOW 0: must attain the displayed maximum; exact ties
#                       pass -- see the tightening note at its definition)
#   JEV_WEIGHT_TOL    - |score - weighted mean of its distribution|
JEV_PROB_SUM_TOL <- 0.01
# Winner consistency tolerance. Was 0.02 on a "display rounding" rationale
# that external audit r2 finding 5 correctly demolished: monotone rounding
# cannot produce a displayed reversal. 40 live calls (12 tie-forced) showed
# zero reversals, so the rule is now the documentation's: the chosen option
# must attain the maximum DISPLAYED probability (exact ties pass; an absolute
# 1e-9 floating-point guard is applied before sum normalization). If the vendor ever
# starts disagreeing with its own docs, raise this with dated live evidence
# cited in the jev_answer_valid header -- see finding-4 header there.
JEV_WINNER_TOL <- 0
JEV_WEIGHT_TOL <- 0.05

# Maximum characters of an API error body echoed back to the user. The body is
# attacker/echo-controlled in principle, so it is bounded and scrubbed below.
JEV_ERROR_BODY_LIMIT <- 240L

jev_endpoint <- function() {
  getOption("Rjif.api_base", "https://api.typesafe.ai/v1/systemone")
}

jev_key <- function() {
  key <- Sys.getenv("TYPESAFE_API_KEY", "")
  if (!nzchar(key)) {
    stop("Rjif: TYPESAFE_API_KEY is not set. Get one at https://console.typesafe.ai/keys ",
         "or run offline with options(Rjif.transport = my_mock).", call. = FALSE)
  }
  key
}

# Coerce an API-supplied scalar to a single non-NA value, or NA if it is not one.
# Guards against the API returning null, a nested list, or a vector where the
# contract promises a scalar -- none of which should silently become a decision.
.as_scalar <- function(x, mode = c("double", "character")) {
  mode <- match.arg(mode)
  if (is.list(x)) x <- if (length(x)) x[[1L]] else NULL
  if (is.null(x) || !length(x)) return(NA)
  x <- x[[1L]]
  if (is.null(x)) return(NA)
  out <- suppressWarnings(if (mode == "double") as.double(x) else as.character(x))
  if (length(out) != 1L || is.na(out)) NA
  else out
}

# A returned-model identifier must be ONE non-blank string (audit r6 R6-B3).
# Missing, NA, empty, whitespace, a multi-element vector, or a non-character
# scalar are all "the vendor did not tell us" -- never silently filled in with
# the requested alias, because unknown provenance presented as known defeats
# the alias-drift check this field exists for.
.scalar_string <- function(x) {
  # With fromJSON(simplifyVector = FALSE) a VALID string field is always a
  # length-1 character vector; anything list-shaped or non-character is
  # malformed metadata, not a reported model.
  if (is.null(x) || !is.character(x) || length(x) != 1L) return(NA_character_)
  if (is.na(x) || !nzchar(trimws(x))) NA_character_ else x
}

# .bare_char() exists for R6-B1: .scrub_secrets()/.redact_key() sanitize the
# VALUES of a character vector but carry its attributes along untouched, and
# the recursive retention scrub (.redact_value) runs BEFORE the provenance
# attributes are attached in jev_eval. An attributes-carrying `model`
# argument (`structure("jev-latest", metadata = <secret>)`) therefore used to
# smuggle arbitrary strings into retained answer objects, serialized results,
# and saved caches. Every string that leaves jev_eval as provenance goes
# through here: scrub, then strip ALL attributes so the value is exactly what
# it looks like and nothing rides along in the margins.
.bare_char <- function(x) {
  s <- .scrub_secrets(.redact_key(as.character(x)))
  if (length(s) != 1L) return(NA_character_)
  attributes(s) <- NULL
  s
}

# Redact anything that looks like a bearer/secret token before echoing a
# response body back to the user.
.scrub_secrets <- function(txt) {
  txt <- gsub("(?i)bearer[[:space:]]+[A-Za-z0-9._~+/-]+", "Bearer [REDACTED]",
              txt, perl = TRUE)
  txt <- gsub("sk-[A-Za-z0-9_-]{8,}", "[REDACTED]", txt, perl = TRUE)
  txt
}

# One line, no control characters, hard-capped length. Every string that is
# about to become user-visible error/warning/persisted text passes through here,
# so this is also where the actual API key is redacted (pattern scrubbing alone
# cannot catch an opaque key echoed back by the vendor).
.clean_error_text <- function(txt, limit = JEV_ERROR_BODY_LIMIT) {
  txt <- .redact_key(txt)
  txt <- .scrub_secrets(txt)
  txt <- gsub("[\r\n\t\001-\037]", " ", txt, perl = TRUE)
  txt <- gsub("[[:space:]]{2,}", " ", txt)
  if (nchar(txt, type = "bytes") > limit) txt <- paste0(substr(txt, 1L, limit), "...")
  trimws(txt)
}

# Replace the caller's actual API key, wherever it appears in text that is about
# to become an error, warning, or persisted string. Pattern-based scrubbing
# alone cannot catch an opaque key echoed back by the vendor, so every outward
# path also runs this exact-substring redaction (.as_response enforces it at
# the parse boundary; jev_score_many guards its per-row error column too).
.redact_key <- function(txt) {
  key <- Sys.getenv("TYPESAFE_API_KEY", "")
  if (nzchar(key)) {
    txt <- gsub(key, "[REDACTED-API-KEY]", txt, fixed = TRUE)
  }
  txt
}

# Validate the parsed response object against the documented contract before
# anything is read from it. jsonlite::fromJSON(..., simplifyVector = FALSE)
# yields nested lists; R's $ does PREFIX partial matching, so a field named
# "answersx" used to satisfy raw$answers -- hence [[ always. Field names,
# scalar shapes, value ranges, option membership, and probability
# normalisation are all checked here, once, fail-closed: an invalid answer
# arrives downstream as value = NA, which jif() and jev_score_many() abstain
# on. Invalid probabilities are never clamped or truncated into decisions.
.as_response <- function(obj) {
  if (!is.list(obj)) return(obj)
  nms <- names(obj)
  if (is.null(nms)) nms <- character(0)
  if (anyDuplicated(nms)) {
    stop("Rjif: response object contains duplicate field names (malformed JSON).",
         call. = FALSE)
  }
  answers_idx <- which(nms == "answers")
  if (length(answers_idx)) {
    raw_answers <- obj[[answers_idx[[1L]]]]
    if (!is.list(raw_answers)) {
      stop("Rjif: response 'answers' was not an object.", call. = FALSE)
    }
    anms <- names(raw_answers)
    if (is.null(anms)) anms <- character(0)
    if (anyDuplicated(anms)) {
      stop("Rjif: 'answers' contains duplicate question keys with conflicting values.",
           call. = FALSE)
    }
  }
  # R4-M1/R4-B2: .as_response deliberately does NOT redact content anymore.
  # Semantic validation must run on the representation the transport actually
  # sent (scrubbing probability names before setequal made a valid distribution
  # unusable), and recursive scrubbing of classed list-like objects (POSIXlt)
  # before validation blew the C stack. Redaction now happens ONCE, at the
  # retention boundary, via .redact_value() on the object jev_eval returns.
  obj
}

# Sanitize an R object for RETENTION/PRINTING (audit R4-B2/R5-B3/R6-B1): ONE
# recursive policy applied to values AND attributes with equal force. Every
# character value, every attribute NAME, every attribute VALUE (which may
# itself be an environment holding a key), recursively, with a depth cap.
#
# Round-5's mistake was a second, shallower walker (scrub_attr) for attributes:
# an environment stashed in an attribute by an honest diagnostic transport
# never reached the replacement policy and persisted the credential through
# saveRDS (R6-B1, isolate6). There is now no attribute path that bypasses the
# value path.
#
# Unsupported reference types (functions, environments, calls, symbols,
# pairlists, weak refs, S4, external pointers, EXPRESSION vectors) are
# REPLACED by a STATIC placeholder -- never a class-derived string, because a
# key-bearing class would then leak through the placeholder itself. Raw byte
# vectors are replaced too: credential bytes hide from every text-level
# inspection while surviving serialization.
#
# Factors: the LEVELS attribute is scrubbed via the same policy while the
# integer codes stay untouched, so a secret-bearing label becomes
# [REDACTED-API-KEY] instead of NA.
#
# Runs AFTER validation, so semantic checks always see the consistent
# pre-redaction representation (R4-M1); selected_p is bound before this runs
# so label collisions here can never move a decision number (R5-B1).
# INVERTED GUARD (round 8): the blocklist was being serially discovered by the
# auditor -- R7-B1 missed "language", R8-B1 missed "bytecode" (a compiled
# expression from the standard compiler::compile(), whose constant pool carries
# literal credentials through saveRDS while print/dput conceal it), plus the
# native "promise" supplemental disclosure. From now on retention keeps ONLY
# the plain-data typeof() universe and recurses into lists/attributes; every
# other type -- including any SEXPTYPE invented after this commit -- becomes
# the placeholder. Do NOT evaluate, force or disassemble a code container to
# scrub it; replace it.
.redact_value <- function(x, depth = 0L) {
  if (depth > 24L) return("[REDACTED-DEPTH]")
  tt <- typeof(x)
  if (!tt %in% c("logical", "integer", "double", "complex", "character",
                 "raw", "list", "NULL")) {
    return("[REDACTED-UNSUPPORTED]")
  }
  if (tt == "raw") return("[REDACTED-BYTES]")
  # POSIXlt is a classed list whose elements are themselves POSIXlt and whose
  # attributes (names/levels) are self-referentially sized; re-attaching its
  # class to a partially-redacted shell raises "names attribute must be the
  # same length" (round-4's fail-closed N07/N08 residual). Replace it outright
  # -- a temporal object in response metadata is display metadata, never a
  # decision input, and losing it is strictly safer than a half-redacted one.
  if (tt == "list" && inherits(x, "POSIXlt")) return("[REDACTED-UNSUPPORTED]")
  if (tt == "NULL") return(NULL)
  if (tt %in% c("logical", "integer", "double", "complex")) {
    y <- x
  } else if (tt == "character") {
    y <- .scrub_secrets(.redact_key(x))
  } else {
    y <- lapply(x, .redact_value, depth = depth + 1L)
  }
  attrs <- attributes(x)
  if (length(attrs)) {
    clean <- list()
    for (an in names(attrs)) {
      clean[[.scrub_secrets(.redact_key(as.character(an)))]] <-
        .redact_value(attrs[[an]], depth = depth + 1L)
    }
    # class goes LAST: assigning `class<-` re-compensates an object's names
    # (data.frame, POSIXct, ...) and would otherwise clobber the scrubbed
    # names attribute we just set
    cls <- clean[["class"]]; clean[["class"]] <- NULL
    attributes(y) <- clean
    if (!is.null(cls)) y <- structure(y, class = cls)
  }
  y
}

# Answer values accepted by the client, given the question that was sent.
# Returns list(value, probs, confidence, valid); when the answer violates the
# contract, valid = FALSE with value/confidence forced NA, and reason explains
# the violation (surfaced by jif_reason()/the batch error column).
#
# CONTRACT SOURCES (external audit finding 4, 2026-09-19; all live-verified
# against the API the same evening):
#   * choice answer: `choice` is "the highest-probability option", `confidence`
#     required in [0,1], `probabilities` required over every option (docs
#     https://docs.typesafe.ai/api). The chosen option must attain the
#     displayed maximum, with exact ties allowed and a 1e-9 absolute guard
#     before sum normalization. See JEV_WINNER_TOL for the tightening history.
#   * score answer: `probabilities` and `legend` are REQUIRED and keyed by the
#     same level-index strings ("0".."k-1"); confidence required in [0,1]. The
#     score must agree with the probability-weighted mean of its own
#     distribution (weighted mean of rounded values lands within ~0.015 of the
#     reported score; tolerance 0.05).
#   * noul answer: just {type, noul}; no confidence field is sent.
# TOLERANCES: JEV_PROB_SUM_TOL (distribution normalisation, |sum-1|) and
# JEV_WINNER_TOL (choice winner margin) and JEV_WEIGHT_TOL (score weighted
# mean) are package constants, documented in the help pages, not magic numbers
# inlined in tests.

# Unwrap a JSON array that wraps exactly one scalar: ONLY an UNNAMED 1-element
# list (that is what [] decodes to under simplifyVector = FALSE). A NAMED
# 1-element list is a JSON object -- {wrong: 1} -- and is never a legitimate
# scalar wrapper (audit R3-B1: accepting named objects let
# confidence=list(wrong=1) pass as 1.0). Unsupported atomic types (factor,
# raw, complex, POSIXct...) are rejected by type, never coerced by a
# catch-all as.double/as.character.
.unwrap_scalar <- function(x) {
  if (is.list(x)) {
    if (length(x) != 1L || !is.null(names(x))) return(NULL)  # NULL = reject
    x <- x[[1L]]
  }
  if (is.null(x)) return(NULL)
  if (is.factor(x) || is.raw(x) || is.complex(x) || inherits(x, "Date") ||
      inherits(x, "POSIXt")) return(NULL)
  x
}

.confidence_scalar <- function(x) {
  # Accept a scalar numeric, an unnamed 1-element array wrapping one, or a
  # JSON null. Booleans are REJECTED, not coerced: as.double(TRUE) is 1.0, and
  # a literal confidence:true must never fabricate full certainty (R2-B1).
  # Character values are rejected too -- the contract promises a number
  # (R3-B1: "5" previously slipped through a permissive string fallback).
  if (is.null(x)) return(NA_real_)
  if (is.list(x)) {
    x <- .unwrap_scalar(x)
    if (is.null(x)) return(NA_real_)
  }
  if (is.logical(x) || is.character(x) || !is.numeric(x) ||
      length(x) != 1L || is.na(x)) return(NA_real_)
  if (!is.finite(x)) return(NA_real_)
  as.double(x)
}

# Probability vectors from the API: named list (JSON object), unnamed list
# (JSON array, positional), or numeric/character vector. Booleans anywhere are
# rejected, never coerced to 1/0 (R2-B1); named wrappers are rejected (R3-B1).
.prob_vector_values <- function(pv) {
  if (is.list(pv)) {
    bad <- FALSE
    vals <- vapply(pv, function(p) {
      if (is.list(p)) {
        p <- .unwrap_scalar(p)
        if (is.null(p)) { bad <<- TRUE; return(NA_real_) }
      }
      if (is.null(p) || is.logical(p) || is.character(p) || !is.numeric(p) ||
          length(p) != 1L || is.na(p) || !is.finite(p)) {
        bad <<- TRUE
        return(NA_real_)
      }
      as.double(p)
    }, numeric(1))
    if (bad) return(NULL)
    vals
  } else if (is.numeric(pv)) {
    # as.double() DROPS names (audit R11-B1: the score reindexing step then
    # matched every name to NA and mis-abstained a perfectly valid named
    # numeric distribution). Preserve the pairing here, once, for all callers.
    stats::setNames(as.double(pv), names(pv))
  } else {
    # R4-B1: no catch-all coercion. Character ("0.9"), complex, raw, factor,
    # logical and temporal vectors are REJECTED, not silently as.double()'d.
    # Only a numeric vector or a list of numerics is a probability distribution.
    NULL
  }
}

jev_answer_valid <- function(ans, q) {
  invalid <- function(reason) {
    # A rejected answer carries NO usable numbers: value, probabilities and
    # confidence all go NA so a contract-invalid row can never populate the
    # calibration analytics through p or confidence (audit R2-M2).
    list(value = NA, probs = NULL, confidence = NA_real_,
         valid = FALSE, reason = reason)
  }
  if (is.null(ans) || !is.list(ans)) {
    return(invalid("answer missing or not an object"))
  }
  ans_nms <- names(ans)
  if (!is.null(ans_nms) && anyDuplicated(ans_nms)) {
    return(invalid("answer object contains duplicate field names (malformed JSON)"))
  }
  if (!is.null(ans[["type"]])) {
    t1 <- ans[["type"]]
    if (!(is.character(t1) && length(t1) == 1L && !is.na(t1))) {
      return(invalid("answer 'type' field is not a single string"))
    }
    if (!identical(t1, q$type)) {
      return(invalid(paste0("answered type '", t1, "', requested '", q$type, "'")))
    }
  }
  field <- switch(q$type, noul = "noul", choice = "choice", score = "score")
  if (is.null(field)) {
    return(invalid(paste0("unknown question type '", as.character(q$type)[[1L]], "'")))
  }
  v <- .unwrap_scalar(ans[[field]])   # ONLY unnamed 1-element JSON arrays wrap a
  # scalar; a named object like {wrong: 0.9} is rejected here, not unwrapped
  # (audit R4-B1: this was the last catch-all path left from round 3).
  if (q$type == "choice") {
    if (is.null(v) || !is.character(v) || length(v) != 1L || is.na(v)) {
      return(invalid("choice is not a single non-NA string"))
    }
  } else {
    if (is.null(v) || !is.numeric(v) || length(v) != 1L || is.na(v)) {
      # is.numeric excludes logical, character ("0.9"), complex, raw, factor,
      # Date and POSIXt -- every representation-dependent coercion in the
      # round-3 audit died here (N14-N17, M07-M09).
      return(invalid(paste0(field, " is not a single non-NA number")))
    }
  }
  # supplied confidence, validated but NOT trusted for gating: jprob()/floor
  # logic use it only where the contract says so (score). exposed_conf is the
  # strict value (boolean -> NA) that the answer object will carry.
  conf <- .confidence_scalar(ans[["confidence"]])
  if (length(conf) != 1L || is.na(conf) || !is.finite(conf) ||
      conf < 0 || conf > 1) conf_valid <- NA_real_
  else conf_valid <- conf
  if (q$type == "noul") {
    v <- suppressWarnings(as.double(v))
    if (length(v) != 1L || is.na(v) || !is.finite(v) || v < 0 || v > 1) {
      return(invalid("noul value outside [0, 1] or not a finite number"))
    }
    # The API sends no confidence for noul answers; we do not invent one, and
    # we ignore any stray confidence field rather than coerce it to a number.
    return(list(value = v, probs = NULL, confidence = NA_real_,
                valid = TRUE, reason = NA_character_))
  }
  if (q$type == "choice") {
    v <- as.character(v)
    offered <- names(q$criteria)
    if (!(v %in% offered)) {
      return(invalid(paste0("chosen option '", v, "' was not offered")))
    }
    if (is.null(ans[["probabilities"]])) {
      return(invalid("choice answer carries no probability distribution"))
    }
    pv <- ans[["probabilities"]]
    pn <- names(pv)
    vals <- .prob_vector_values(pv)
    if (is.null(vals)) {
      return(invalid("probability vector contains non-numeric or boolean entries"))
    }
    if (is.null(pn)) {
      if (length(vals) == length(offered)) pn <- offered   # positional, exact length
    }
    if (is.null(pn) || any(!nzchar(pn)) || anyDuplicated(pn) ||
        anyNA(vals) || any(!is.finite(vals)) || any(vals < 0) || any(vals > 1)) {
      return(invalid("probability vector malformed (names, NA, or range)"))
    }
    if (!setequal(pn, offered)) {
      return(invalid("probability names do not cover the offered options"))
    }
    s <- sum(vals)
    if (abs(s - 1) > JEV_PROB_SUM_TOL + 1e-9) {
      return(invalid(paste0("probabilities sum to ", formatC(s, format = "f",
                                                             digits = 3), ", not ~1")))
    }
    # winner consistency (audit finding 4, probe 3: the old validator accepted
    # choice='a' with probabilities a=.01 b=.99): the docs define `choice` as
    # "the highest-probability option". TIGHTENED from a 0.02 tolerance
    # (external audit r2 finding 5): the old rationale -- the API rounds
    # probabilities for display while argmax runs on unrounded values -- does
    # not justify accepting a DISPLAYED reversal: monotone rounding can turn
    # two distinct values into a displayed TIE but can never make the
    # displayed winner trail the displayed top. Evidence (2026-09-20, live,
    # no forgeries): 40 calls over deliberately ambiguous states, including
    # 12 with duplicated criteria built to force ties, produced ZERO
    # winner-vs-argmax gaps. The rule is now: the chosen option must attain
    # the maximum displayed probability within an absolute 1e-9 guard.
    # Exact ties pass. Check the raw displayed values before normalization;
    # gaps at or below 1e-9 pass regardless of their cause. Larger reversals
    # surface as a documented contract abstention.
    # If live traffic ever shows the vendor disagreeing with its own docs,
    # relax JEV_WINNER_TOL here with dated evidence in the comment, and the
    # reason string below will name the tolerance.
    # NOTE on the collision tests (R5-B1): pn was validated all-non-empty and
    # all-distinct above, so match(v, pn) is a unique POSITION; subscript by
    # index, never by name ([[ on a duplicated name picks the first).
    gap <- max(vals) - vals[[match(v, pn)[[1L]]]]
    if (gap > JEV_WINNER_TOL + 1e-9) {
      return(invalid(paste0("chosen option '", v, "' trails the top option by ",
                            formatC(gap, format = "f", digits = 3),
                            " (> tolerance ", formatC(JEV_WINNER_TOL, format = "f",
                                                      digits = 2), ")")))
    }
    if (is.na(conf_valid)) {
      # `confidence` is required on choice answers per the API reference.
      return(invalid("choice answer lacks a confidence in [0, 1]"))
    }
    probs <- stats::setNames(as.list(lapply(vals, function(x) x / s)), pn)
    return(list(value = v, probs = probs, confidence = conf_valid,
                valid = TRUE, reason = NA_character_))
  }
  # score: a CONTINUOUS position on the level scale (live API + official docs,
  # 2026-09-19: "probability-weighted value across your levels; can land
  # between levels" -- the audit rounds' integer-index assumption was OUR
  # invention and wrongly abstained every real score answer).
  v <- suppressWarnings(as.double(v))
  k <- length(q$criteria)
  if (length(v) != 1L || is.na(v) || !is.finite(v) || v < 0 || v > k - 1) {
    return(invalid(paste0("score value outside the level range 0..", k - 1)))
  }
  if (is.na(conf_valid)) {
    return(invalid("score answer lacks a confidence in [0, 1]"))
  }
  # per-level distribution: REQUIRED on score answers (live docs 2026-09-19:
  # score, legend, probabilities, confidence all "required"; external audit
  # finding 4, probe 4 caught that this block was conditional, so a bare
  # {score, confidence} response passed validation). Keyed by level-index
  # strings; names must cover 0..k-1 exactly and match the legend keys.
  if (is.null(ans[["probabilities"]])) {
    return(invalid("score answer carries no probability distribution"))
  }
  pv <- ans[["probabilities"]]
  pn <- names(pv)
  vals <- .prob_vector_values(pv)
  want <- as.character(seq_len(k) - 1L)
  if (is.null(vals) || is.null(pn) || anyDuplicated(pn) ||
      !setequal(pn, want) || anyNA(vals) || any(!is.finite(vals)) ||
      any(vals < 0) || any(vals > 1)) {
    return(invalid("score probability vector malformed (names, NA, or range)"))
  }
  s <- sum(vals)
  if (abs(s - 1) > JEV_PROB_SUM_TOL + 1e-9) {
    return(invalid(paste0("score probabilities sum to ", formatC(s, format = "f",
                                                                 digits = 3),
                          ", not ~1")))
  }
  # legend required and its keys must cover the same level indices (docs
  # "map<string,string>", keys matching probabilities). A missing or
  # mis-keyed legend makes the answer uninterpretable.
  if (is.null(ans[["legend"]])) {
    return(invalid("score answer carries no legend"))
  }
  lg <- ans[["legend"]]
  lgn <- names(lg)
  if (is.null(lgn) || anyDuplicated(lgn) || !setequal(lgn, want)) {
    return(invalid("score legend keys do not cover the level indices 0..k-1"))
  }
  vals <- vals / s
  vals <- vals[match(pn, names(vals))]
  # trust but verify: the score must be the weighted mean of its own
  # distribution. Legend keys are level indices as strings ("0".."k-1"),
  # NOT 1-based (author bug found by the suite: an off-by-one here made
  # every valid continuous score self-contradict by exactly 1.0).
  # JEV_WEIGHT_TOL covers the API's 2-decimal rounding of probabilities
  # (live-verified: reported score is the mean of UNrounded values, so the
  # weighted mean of the rounded ones lands within ~0.015; the docs' own
  # example 0/0.95/0.05 -> 1.05 is exact).
  wm <- sum(vals * as.integer(pn))
  if (abs(wm - v) > JEV_WEIGHT_TOL + 1e-9) {
    return(invalid(paste0("score ", formatC(v, format = "f", digits = 3),
                          " contradicts its probability-weighted mean ",
                          formatC(wm, format = "f", digits = 3))))
  }
  sprob <- stats::setNames(lapply(vals, function(x) x), pn)
  return(list(value = v, probs = sprob, confidence = conf_valid,
              valid = TRUE, reason = NA_character_))
}

# Default transport over httr. Returns the parsed response list.
#
# TIMEOUTS AND RETRIES (external audit finding 5, 2026-09-19): the vendor
# documents 429 (rate limited) and 529 (overloaded) as retryable with
# exponential backoff (https://docs.typesafe.ai/api#handling-rate-limits).
# Policy implemented here:
#   * hard per-request timeout (curl -m), so a hung socket cannot stall a
#     5,000-row batch forever. Default 120s; option Rjif.timeout.
#   * bounded retries with exponential backoff plus jitter for 429/529 and
#     for transport-level errors (timeouts, DNS, connection refused), which
#     may be transient. Defaults: 3 retries, base 1s, cap 30s; options
#     Rjif.retries / Rjif.retry_base / Rjif.retry_cap.
#   * a Retry-After header, when present and sane (numeric, <= the option
#     Rjif.retry_max_wait default 120s), overrides the computed backoff.
#   * 4xx responses OTHER than 429 (401/403/422) are never retried - a bad
#     key or malformed body will not fix itself - and error immediately.
#   * 5xx statuses other than 529 are treated the same way: a server error
#     outside the documented retryable set is not guessed at. Only
#     transport-class failures (timeout/DNS/refused/reset, surfacing as
#     errors from httr rather than HTTP responses) and the two documented
#     statuses retry.
#   * the final error message reports how many attempts were made, because
#     a timed-out attempt MAY still have been processed (and billed) by the
#     vendor; we do not claim to know (same honesty rule as jev_usage()$calls).
# Retries multiply the request count, never the usage counters: jev_usage()
# counts decoded RESPONSES, so retried failures still count zero.
.transport_httr <- function(body) {
  if (!requireNamespace("httr", quietly = TRUE) ||
      !requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Rjif needs the 'httr' and 'jsonlite' packages.", call. = FALSE)
  }
  timeout <- .positive_option("Rjif.timeout", 120)
  max_retries <- .nonneg_int_option("Rjif.retries", 3L)
  base <- .positive_option("Rjif.retry_base", 1)
  cap <- .positive_option("Rjif.retry_cap", 30)
  max_wait <- .positive_option("Rjif.retry_max_wait", 120)
  # Configuration/serialization failures are not transport failures. Validate
  # once, outside the retry loop, so missing keys and invalid bodies never retry.
  key <- jev_key()
  # Envelope shape the IDENTITY is built on. The fingerprint must digest
  # EXACTLY what the transport sends -- the r6e-m2 test proves byte
  # identity on this call, so any options drift here or in JSON_OPTS fails
  # that assertion (round-6f lesson: identity that re-implements the
  # serializer, even with good options, lags it silently).
  opts <- JSON_OPTS
  payload <- tryCatch(
    as.character(do.call(jsonlite::toJSON,
                         c(list(body), opts))),
    error = function(e) {
      # round 6f (n2): a never-serializable body must say WHY before the
      # transport is entered; the generic jsonlite message ("No method
      # asJSON S3 class", "translating strings ... not allowed") is
      # expanded with the failing class or byte info. Serialization cannot
      # echo the key (the body holds no key), so no scrub is needed here.
      msg <- conditionMessage(e)
      stop(.clean_error_text(paste0(
        "Rjif: the request could not be serialized (the model or question ",
        "holds a representation the API cannot receive): ", msg)),
        call. = FALSE)
    })
  endpoint <- jev_endpoint()
  attempt <- 0L
  repeat {
    attempt <- attempt + 1L
    resp <- tryCatch(
      httr::POST(endpoint,
        httr::timeout(timeout),
        httr::add_headers(Authorization = paste("Bearer", key),
                          `Content-Type` = "application/json"),
        body = payload,
        encode = "raw"),
      error = function(e) {
        if (!inherits(e, "curl_error")) stop(e)
        structure(list(msg = .clean_error_text(conditionMessage(e))),
                  class = "jev_transport_error")
      })
    if (inherits(resp, "jev_transport_error")) {
      # connection-level failure (timeout, DNS, refused, reset): retryable if
      # attempts remain; the message is already scrubbed of anything key-like.
      if (attempt <= max_retries) {
        .retry_sleep(.backoff_wait(attempt, base, cap, NULL, max_wait))
        next
      }
      stop("Rjif: failed after ", attempt,
           " attempt(s); the vendor may bill attempts even when no answer arrived. ",
           "API call to ", endpoint, ": ", resp$msg,
           call. = FALSE)
    }
    status <- tryCatch(httr::status_code(resp), error = function(e) NA_integer_)
    ok <- tryCatch(httr::http_status(resp)$category == "Success",
                   error = function(e) FALSE)
    if (isTRUE(ok)) break
    if ((identical(status, 429L) || identical(status, 529L)) &&
        attempt <= max_retries) {
      hdrs <- tryCatch(httr::headers(resp), error = function(e) list())
      ra <- tryCatch(hdrs[["retry-after"]], error = function(e) NULL)
      .retry_sleep(.backoff_wait(attempt, base, cap, ra, max_wait))
      next
    }
    break   # non-retryable status, or attempts exhausted -> error below
  }
  status <- if (inherits(resp, "jev_transport_error")) NA_integer_ else
    tryCatch(httr::status_code(resp), error = function(e) NA_integer_)
  ok <- if (inherits(resp, "jev_transport_error")) FALSE else
    tryCatch(httr::http_status(resp)$category == "Success", error = function(e) FALSE)
  if (!isTRUE(ok)) {
    # NOTE: never interpolate jev_key() or the request headers into this message.
    haltxt <- tryCatch(rawToChar(resp$content), error = function(e) "")
    if (!nzchar(haltxt)) haltxt <- tryCatch(httr::content(resp, "text", encoding = "UTF-8"),
                                           error = function(e) "")
    stop("Rjif: failed (HTTP ", if (is.na(status)) "?" else status,
         ") after ", attempt, " attempt(s)",
         if (identical(status, 429L) || identical(status, 529L))
           "; retries exhausted; the vendor may bill attempts even when no answer arrived" else "",
         if (identical(status, 401L) || identical(status, 403L))
           "; check TYPESAFE_API_KEY" else "",
         ". API call to ", endpoint, ": ",
         .clean_error_text(paste(haltxt, collapse = " ")),
         call. = FALSE)
  }
  txt <- tryCatch(rawToChar(resp$content), error = function(e) "")
  if (!nzchar(txt)) stop("Rjif: API returned an empty body (HTTP ", status, ").",
                         call. = FALSE)
  out <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE),
                  error = function(e) NULL)
  if (!is.list(out)) stop("Rjif: API response was not a JSON object (HTTP ", status, ").",
                          call. = FALSE)
  out
}

# Backoff for attempt k (1-based), with up to 25% jitter. Retry-After may
# be delta-seconds or an HTTP date; both are bounded by the caller's budget.
.backoff_wait <- function(attempt, base, cap, retry_after, max_wait = cap) {
  ra <- suppressWarnings(as.numeric(retry_after))
  if (length(ra) == 1L && is.na(ra) && is.character(retry_after)) {
    date <- tryCatch(suppressWarnings(httr::parse_http_date(retry_after)),
                     error = function(e) NA_real_)
    if (length(date) == 1L && is.finite(as.numeric(date)))
      ra <- max(0, as.numeric(difftime(date, Sys.time(), units = "secs")))
  }
  computed <- min(cap, base * (2^(attempt - 1L)))
  if (length(ra) == 1L && is.finite(ra) && ra >= 0) {
    return(min(ra, max_wait))   # honour server hints inside the caller's budget
  }
  jitter <- computed * stats::runif(1, 0, 0.25)
  min(cap, max_wait, computed + jitter)
}

.retry_sleep <- function(seconds) {
  Sys.sleep(max(0, seconds))
}

.positive_option <- function(name, default) {
  v <- suppressWarnings(as.numeric(getOption(name, default)))
  if (length(v) != 1L || !is.finite(v) || v <= 0) default else v
}
.nonneg_int_option <- function(name, default) {
  v <- suppressWarnings(as.integer(getOption(name, default)))
  if (length(v) != 1L || is.na(v) || v < 0) default else v
}

# Question constructors -------------------------------------------------------

# noul: "is this statement true of the state?" -> 0-1
# Optional `criteria` (a named list/character vector with true = and/or
# false = descriptions) clarifies what each outcome means; live API
# https://docs.typesafe.ai/primitives/noul. Any other names are rejected:
# a typo'd key would silently change what the model sees.
jev_noul_q <- function(instructions, criteria = NULL) {
  if (!is.null(criteria)) {
    if (!(is.list(criteria) || is.character(criteria))) {
      stop("Rjif: noul criteria must be a named list with 'true'/'false' ",
           "descriptions (either or both).", call. = FALSE)
    }
    cn <- names(criteria)
    if (is.null(cn) || !all(cn %in% c("true", "false")) || !any(nzchar(cn))) {
      stop("Rjif: noul criteria names must be 'true' and/or 'false'; got: ",
           .clean_error_text(paste(cn, collapse = ", ")), ".", call. = FALSE)
    }
    if (anyDuplicated(cn)) {
      stop("Rjif: noul criteria must not repeat 'true'/'false'.", call. = FALSE)
    }
    # Named atomic vectors otherwise serialize as arrays, losing true/false.
    criteria <- as.list(criteria)
  }
  structure(list(type = "noul", instructions = instructions,
                 criteria = criteria), class = "jev_question")
}

# choice: route among named, described options -> one option + full distribution
# API limit (live docs, external audit finding 4): at most 255 options.
JEV_MAX_CHOICE_OPTIONS <- 255L
JEV_SCORE_LEVEL_RANGE <- c(2L, 10L)   # "at least two levels; up to 10" (docs)
jev_choice_q <- function(instructions, criteria) {
  if (!is.list(criteria) && !is.character(criteria)) {
    stop("Rjif: choice criteria must be a named list or character vector ",
         "(option = description).", call. = FALSE)
  }
  nms <- names(criteria)
  if (is.null(nms) || any(!nzchar(nms))) {
    stop("Rjif: choice criteria must be a named list (option = description).",
         call. = FALSE)
  }
  if (length(nms) < 2L) {
    stop("Rjif: a Choice question needs at least 2 options, not ",
         length(nms), ".", call. = FALSE)
  }
  if (length(nms) > JEV_MAX_CHOICE_OPTIONS) {
    stop("Rjif: choice criteria has ", length(nms), " options; the API allows ",
         JEV_MAX_CHOICE_OPTIONS, ". Classify a large taxonomy level by level ",
         "(chained Choice questions) instead of one giant option list.",
         call. = FALSE)
  }
  if (anyDuplicated(nms)) {
    stop(.clean_error_text(paste0(
      "Rjif: choice criteria names must be unique; duplicated: ",
      paste(unique(nms[duplicated(nms)]), collapse = ", "), ".")),
      call. = FALSE)
  }
  structure(list(type = "choice", instructions = instructions,
                 criteria = as.list(criteria)), class = "jev_question")
}

# score: rate the state on an ordered rubric (character vector of level
# descriptions, lowest severity first; or a list of structured level objects
# such as list(what = "...", examples = list("...")) - the API accepts JSON
# structure per the official docs). Levels are labelled 0..k-1 in the response
# legend, and the answer's `score` is a CONTINUOUS probability-weighted position
# that may land between levels.
# NOTE (audit R11): unlike the old behaviour, structured criteria objects are
# preserved and serialized as JSON (the live contract accepts them); plain
# string levels behave exactly as before.
jev_score_q <- function(instructions, criteria) {
  if (is.list(criteria)) {
    # structured levels: keep JSON-native structure
    crit <- criteria
  } else {
    crit <- as.character(criteria)
  }
  if (!length(crit)) {
    stop("Rjif: score criteria must be a non-empty character vector of ordered ",
         "level descriptions.", call. = FALSE)
  }
  # API level-count limits (docs: "at least two levels; the API accepts up to
  # 10"; external audit finding 4: a 1-level rubric also serialized as a JSON
  # scalar string via auto_unbox, which the API cannot read as an array).
  if (length(crit) < JEV_SCORE_LEVEL_RANGE[[1L]]) {
    stop("Rjif: a Score question needs at least ", JEV_SCORE_LEVEL_RANGE[[1L]],
         " levels, not ", length(crit),
         ". A yes/no judgment is a Noul question.", call. = FALSE)
  }
  if (length(crit) > JEV_SCORE_LEVEL_RANGE[[2L]]) {
    stop("Rjif: score criteria has ", length(crit), " levels; the API allows ",
         JEV_SCORE_LEVEL_RANGE[[2L]], ". Coarser rubrics calibrate better ",
         "anyway - merge adjacent levels.", call. = FALSE)
  }
  if (is.character(crit) && (any(!nzchar(crit)) || any(is.na(crit)))) {
    stop("Rjif: score criteria may not contain empty or NA level descriptions.",
         call. = FALSE)
  }
  if (!is.null(names(criteria))) {
    crit <- if (is.list(crit)) {
      # label each level with its name, then drop names so criteria serializes
      # as an ordered JSON ARRAY (the live contract's shape), not an object
      out <- lapply(seq_along(crit), function(i)
        if (is.character(crit[[i]])) paste0(names(criteria)[[i]], ": ", crit[[i]])
        else structure(c(list(level = names(criteria)[[i]]), crit[[i]]),
                       names = c("level", names(crit[[i]]))))
      unname(out)
    } else {
      paste0(names(criteria), ": ", crit)
    }
  } else if (is.list(crit)) {
    crit <- unname(crit)   # ordered JSON array, not an object
  }
  structure(list(type = "score", instructions = instructions, criteria = crit),
            class = "jev_question")
}

# Core call --------------------------------------------------------------------
# jev_eval(state = <character>, questions = list(a = jev_noul_q(...), ...))
# returns a 'jev_answers' list, one entry per question, in the same order.
jev_eval <- function(state, questions, model = getOption("Rjif.model", "jev-latest")) {
  # The API accepts exactly ONE state per call: a string, or structured data
  # (object/array per https://docs.typesafe.ai/api#param-state; live-verified
  # 2026-09-19 with a nested list state). A character VECTOR is still rejected:
  # it used to be silently truncated to its first element (audit M2), and one
  # call must not mean many states. For vectors use jev_score_many().
  state_ok <- (is.character(state) && length(state) == 1L && !is.na(state)) ||
    (is.list(state) && length(state) > 0L)
  if (!state_ok) {
    stop("Rjif: state must be a single non-NA string or a non-empty list ",
         "(structured state: records, chat logs, nested fields)",
         if (is.character(state) && length(state) > 1L)
           paste0(" (got a length-", length(state), " character vector; use jev_score_many() for vectors)")
         else ".",
         call. = FALSE)
  }
  if (!is.list(questions) || is.null(names(questions)) || !length(questions)) {
    stop("Rjif: questions must be a non-empty named list of jev_*_q() specs.", call. = FALSE)
  }
  if (any(!nzchar(names(questions)))) {
    stop("Rjif: every question in 'questions' must be named.", call. = FALSE)
  }
  if (anyDuplicated(names(questions))) {
    stop(.clean_error_text(paste0(
      "Rjif: question names must be unique; duplicated: ",
      paste(unique(names(questions)[duplicated(names(questions))]),
            collapse = ", "), ".")),
      call. = FALSE)
  }
  ok <- vapply(questions, inherits, logical(1), "jev_question")
  if (!all(ok)) stop("Rjif: build questions with jev_noul_q/jev_choice_q/jev_score_q.",
                     call. = FALSE)
  # Model type gate (audits r6e follow-up + r6f R6f-B2 -- read BOTH
  # before changing): the API's model field is a model NAME -- a single
  # non-NA, non-blank string. Before this check, model = NULL serialized
  # on the wire as {"model":null} and model = 5 as {"model":5}: requests
  # that can never succeed, surfaced only as opaque vendor 4xx errors.
  # (r6f-m1 correction of my own first comment here: under this
  # transport's null = "null" option, NULL renders "null", NOT {}. The
  # collapse that actually exists is NULL / character(0) / list(): each
  # reduces to character(0) before any character-level digest. "" and
  # " " keep distinct digests -- the blank rejection stands on the API's
  # scalar-name contract, not on an identity-merge claim.)
  # Class and dimension attributes are NOT dropped by is.character():
  # I("m") is character-classed ("AsIs") and posts {"model":["m"]},
  # matrix("m",1,1) posts {"model":[["m"]]}, and
  # structure("m", class = "r6f_unknown") cannot post at all. Those are
  # rejected by the digest now riding the serializer itself
  # (.model_identity, r6f R6f-B2); the is.character + length checks here
  # still stop the common mistakes with a clear message BEFORE any cache
  # work. A fully-resumed batch never reaches jev_eval, so
  # jev_score_many mirrors this gate.
  if (!(is.character(model) && length(model) == 1L && !is.na(model))) {
    stop("Rjif: model must be a single non-NA string (the model name).",
         call. = FALSE)
  }
  # Bare class (audit r6f R6f-B2): is.character() is TRUE for I("m")
  # (class "AsIs") and the wire then posts {"model":["m"]}; matrix("m",1,1)
  # posts {"model":[[...]]}; structure("m", class = anything-unknown)
  # cannot post at all ("No method asJSON"). The cache identity is now
  # built from the serializer's own bytes (r6f fix), so those forms could
  # no longer RESUME the plain model's cache even if they got this far --
  # but a fresh jev_eval would still send an array the API's scalar-name
  # contract rejects, and the r6b smuggling test proves harmless
  # attributes (names, metadata) DO survive to here and must keep
  # working, so the gate is exactly: class must be plain "character".
  if (!identical(class(model), "character")) {
    # The class names are CALLER text (an attacker can name a class after
    # the API key -- Astra round 6g R6g-B1 showed this gate echoed the key
    # verbatim, before .clean_error_text ever saw the message). Scrub the
    # interpolated text; keep the readable class list for honest users.
    stop(.clean_error_text(paste0(
           "Rjif: model must be a plain character scalar (class ",
           paste(class(model), collapse = "/"), " changes or breaks the ",
           "wire shape of the model name).")), call. = FALSE)
  }
  # Encoding check BEFORE the blank check: grepl() on an invalid-byte
  # string warns ("input string 1 is invalid") while trying to translate,
  # so a never-serializable model must be rejected first (and its error
  # says why: re-encode, not "blank").
  if (Encoding(model) == "bytes" || !validEnc(model)) {
    stop("Rjif: model is 'bytes'-marked or contains invalid byte sequences ",
         "and can never be serialized into a request; re-encode it (e.g. ",
         "stringi::str_conv or iconv) before scoring.", call. = FALSE)
  }
  if (!grepl("[^[:space:]]", model)) {
    stop("Rjif: model must not be blank (whitespace-only): the API's ",
         "model field names one model and cannot be empty.",
         call. = FALSE)
  }

  body <- list(state = state, model = model,
               questions = lapply(questions, function(q) unclass(q)))

  transport <- getOption("Rjif.transport")
  # Pre-transport serialization check (audit r6g R6g-B2: the serializer
  # lives inside .transport_httr, so with a custom Rjif.transport callback
  # jev_eval handed a NEVER-serializable body -- unknown S3 class, bytes-
  # marked text -- straight to the callback, and a callback that answers
  # it returned a "success" no wire could ever have produced; the spec's
  # 48-check direct matrix all entered the mock first). Render the actual
  # request envelope with the SAME JSON_OPTS the real transport uses,
  # HERE, before either branch: the property "a request that could not be
  # serialized never produces an answer" then holds for every transport,
  # not just the default one. The error text passes .clean_error_text so
  # an asJSON failure naming a class that carries the API key stays
  # redacted (same invariant as R6g-B1). Cost: one extra render per call
  # (microseconds against a network round trip).
  # Slot-aware scan FIRST (same helper the cache identity uses), so bytes/
  # invalid text names its slot ("Rjif: question$instructions ...") instead
  # of surfacing as jsonlite's generic "translating strings ..." error; the
  # render below is the second line of defense for what a text scan cannot
  # see (unknown S3 classes).
  for (q in body$questions) .wire_slot_scan(q, "question")
  invisible(tryCatch({
      as.character(do.call(jsonlite::toJSON, c(list(body), JSON_OPTS)))
    },
    error = function(e) {
      stop(.clean_error_text(paste0(
        "Rjif: the request could not be serialized (the model or question ",
        "holds a representation the API cannot receive): ",
        conditionMessage(e))), call. = FALSE)
    }))
  # A transport exception or warning must never carry the request (with its
  # Bearer header) into a condition message (audit R2-B2): scrub before
  # rethrowing. Errors are caught and re-thrown scrubbed; warnings run
  # through a calling handler so the response value survives.
  transport_warning <- NULL
  raw <- withCallingHandlers(
    tryCatch(
      if (is.function(transport)) transport(body) else .transport_httr(body),
      error = function(e) {
        stop(.clean_error_text(conditionMessage(e)), call. = FALSE)
      }),
    warning = function(w) {
      transport_warning <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    })
  if (!is.null(transport_warning)) {
    warning(.clean_error_text(transport_warning), call. = FALSE)
  }
  # Enforce the response contract at the parse boundary: exact field names (no
  # $ partial matching), duplicate keys rejected, and every string deep-redacted
  # against the actual API key before anything can echo it onward.
  raw <- .as_response(raw)
  if (!is.list(raw)) {
    # P32/R3-B2: class(raw) is attacker-influenced text (a pluggable transport
    # can set class to the key itself); this error is package-generated, AFTER
    # the transport wrapper, so it must be scrubbed here too.
    stop(.clean_error_text(paste0("Rjif: transport response was not a list (got ",
                                  paste(class(raw), collapse = "/"), ").")),
         call. = FALSE)
  }
  if (is.null(raw[["answers"]])) {
    stop("Rjif: response had no 'answers' field. Raw response fields: ",
         .clean_error_text(paste(names(raw), collapse = ", "), 120L), ".",
         call. = FALSE)
  }
  if (!is.list(raw[["answers"]])) {
    stop("Rjif: response 'answers' was not an object.", call. = FALSE)
  }

  # Token accounting. usage is optional (mock transports omit it). 'calls'
  # counts every call for which a response arrived and reached decoding --
  # NOT calls whose transport threw before responding (the vendor may or may
  # not have billed those; we do not claim to know). Parsing usage can NEVER
  # throw or discard a delivered answer (audit R2-M3): extraction is wrapped,
  # and counters accumulate in DOUBLE space, which holds any plausible token
  # total without R's 32-bit integer overflow to NA.
  billed <- 0
  tok <- NULL
  tryCatch({
    u <- raw[["usage"]]
    if (is.list(u) && !is.null(u[["input_tokens"]])) {
      tok <- u[["input_tokens"]]
      if (is.list(tok)) tok <- unlist(tok, recursive = TRUE)
      TRUE
    } else TRUE
  }, error = function(e) TRUE, warning = function(w) invokeRestart("muffleWarning"))
  # only a scalar numeric token count is booked; anything else (empty, multi,
  # non-numeric) is malformed usage -> counted as a call, zero tokens, no throw
  if (is.atomic(tok) && is.numeric(tok) && length(tok) == 1L &&
      !is.na(tok) && is.finite(tok) && tok >= 0) billed <- as.double(tok)
  .RjifEnv$usage$calls <- .RjifEnv$usage$calls + 1L
  .RjifEnv$usage$input_tokens <- .RjifEnv$usage$input_tokens + billed

  ans <- lapply(names(questions), function(nm) {
    a <- raw[["answers"]][[nm]]
    # R5-B2: nm may itself be a caller-supplied key-bearing question name, and
    # checked$reason may echo a key from the response; scrub before SIGNALING,
    # because scrubbing the eventual answer cannot retract an emitted warning.
    if (is.null(a)) {
      stop(.clean_error_text(paste0("Rjif: missing answer for question '", nm, "'.",
                                    collapse = "")),
           call. = FALSE)
    }
    qtype <- questions[[nm]]$type
    checked <- jev_answer_valid(a, questions[[nm]])
    if (!isTRUE(checked$valid)) {
      warning(.clean_error_text(paste0(
        "Rjif: answer for question '", nm, "' violated the API contract (",
        checked$reason, "); treated as no answer (NA).", collapse = "")),
        call. = FALSE)
    }
    # R5-B1: carry the SELECTED option's probability as an independent scalar,
    # bound before any display redaction can merge duplicate sanitized names.
    # jprob() reads this instead of re-looking-up the (possibly redacted) label,
    # which previously could return a sibling's 0.9 for a selected 0.1.
    selected_p <- if (qtype == "choice" && isTRUE(checked$valid)) {
      pv <- .prob_values_bound(checked$probs, checked$value)
      pv
    } else NA_real_
    # legend only exists on an OBJECT answer; an atomic one (.99) must not
    # crash on subsetting (audit R2-m1) -- the contract violation already
    # forced value/confidence to NA above.
    legend <- if (is.list(a)) a[["legend"]] else NULL
    structure(list(
      question   = nm,
      type       = qtype,
      value      = checked$value,
      probs      = checked$probs,
      selected_p = selected_p,
      confidence = checked$confidence,
      contract   = if (isTRUE(checked$valid)) NA_character_ else checked$reason,
      legend     = legend,
      raw        = a
    ), class = c(paste0("jev_", qtype), "jev_answer"))
  })
  # RETENTION boundary (R4-B2/R5-B3): validation ran on the consistent
  # pre-redaction representation; now scrub the live key from EVERYTHING that
  # leaves jev_eval -- answer objects, their names, and the model attribute --
  # depth-capped. Nothing key-bearing escapes into dput/serialize/print/RDS.
  ans <- lapply(ans, .redact_value)
  names(ans) <- .scrub_secrets(.redact_key(as.character(names(questions))))
  # R6-B3: the RETURNED identifier is whatever the vendor actually reported;
  # if the envelope omits it (or reports NA/blank/non-scalar), it stays NA.
  # The requested alias belongs in model_requested ONLY -- filling the unknown
  # with the alias would turn "we do not know" into "we know".
  # R6-B1: .bare_char() scrubs the live key AND drops every attribute, so an
  # attributes-carrying `model` argument cannot smuggle strings into retained
  # objects behind the recursive scrub above.
  returned_model <- .bare_char(.scalar_string(raw[["model"]]))
  requested_model <- .bare_char(model)
  # Per-answer provenance (round 3 carryover 1): the envelope attribute below
  # is dropped by the $q extraction that jif() and jev_score_many() use, so
  # the alias-drift risk was invisible per row. Attach requested AND returned
  # model to each answer object itself; both are bare scrubbed scalars, so
  # nothing key-bearing and no smuggled attributes escape.
  ans <- lapply(ans, function(a) {
    attr(a, "model_requested") <- requested_model
    attr(a, "model_returned") <- returned_model
    a
  })
  structure(ans,
            model = returned_model,
            class = "jev_answers")
}

# Probability bound to the selected label, captured BEFORE any display
# redaction (R5-B1). The list values are what jprob() uses for a choice answer.
.prob_values_bound <- function(probs, selected) {
  if (is.null(probs) || is.null(selected) || is.na(selected)) return(NA_real_)
  p <- probs[[selected]]
  # probs[[selected]] uses exact matching here; if duplicate labels existed they
  # could only have come from the TRANSPORT itself and were already rejected by
  # anyDuplicated(pn) in the validator, so this indexes a unique label.
  if (is.list(p)) p <- p[[1L]]
  p <- suppressWarnings(as.double(p))
  if (length(p) != 1L) NA_real_ else p
}

# Null/empty/NA coalesce. base R only ships `%||%` from 4.4. This variant
# mirrors base for NULL and length-0 left-hand sides, and ADDITIONALLY coalesces
# a single atomic NA -- which base 4.4 does NOT do (base returns NA). That
# extension is intentional here (jconf() treats "API sent NA" as "API sent
# nothing") but it means `%||%` is not drop-in identical to base's; if you port
# code, remember the difference. An atomic NA *vector* such as c(NA, 0.3) is
# returned untouched rather than raising R >= 4.2's length-1 coercion error.
`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0L) return(b)
  if (length(a) == 1L && is.na(a)) return(b)
  a
}

# Accessors --------------------------------------------------------------------

# The answer's value: noul -> probability in 0-1; score -> CONTINUOUS
# probability-weighted level position in 0..k-1 (can land between levels, live
# contract 2026-09-19); choice -> option name.
jvalue <- function(ans) ans$value

# Reported confidence: the API's separate confidence scalar, present only on
# choice and score answers. For a noul answer the API sends no confidence at
# all, so this is NA by design -- jprob() is the accessor that reports
# whichever probability backs the decision (noul score / chosen option p /
# score confidence). Do not threshold noul answers with jconf(); jif() and
# jev_score_many() apply confidence_floor to jprob() for exactly this reason.
jconf <- function(ans) `%||%`(ans$confidence, NA_real_)

# Probability that backs the decision: for noul the noul score, for choice the
# probability of the chosen option, for score the API confidence. For score the
# answer now also carries a per-level distribution (continuous contract);
# confidence is the vendor's DISTRIBUTION-CONCENTRATION summary -- high
# confidence means the mass is concentrated, NOT a guarantee the position is
# right (see docs /confidence). Never read it as P(answer correct).
jprob <- function(ans) {
  if (is.null(ans$type)) return(NA_real_)
  out <- switch(ans$type,
    noul   = suppressWarnings(as.double(jvalue(ans))),
    choice = {
      # R5-B1: use the scalar bound at construction time, BEFORE display
      # redaction could merge two sanitized labels; a label re-lookup could
      # return a sibling option's probability for the selected option.
      sp <- ans$selected_p
      if (!is.null(sp) && is.numeric(sp) && length(sp) == 1L) sp
      else {
        v <- jvalue(ans)
        if (is.na(v) || is.null(ans$probs)) NA_real_
        else suppressWarnings(as.double(ans$probs[[v]]))
      }
    },
    score  = jconf(ans),
    NA_real_)
  if (length(out) != 1L) NA_real_ else out
}

# Full probability distribution behind an answer (round 3 carryover 1).
# Accepts a jev_answer object (from jif()'s attr(, "answer")) or a single
# string from a jev_score_many() result's `probs_json` column. Returns a
# named numeric vector, or NULL when there is no distribution (noul answers
# carry a single scalar; an absent distribution parses to NULL). The stored
# values are the contract validator's NORMALIZED distribution, and the JSON
# is written with digits = 17 -- enough significant digits that every
# positive double re-parses bit-exactly (audit r6 R6-B2: jsonlite's
# digits = NA caps at 15 and loses bits; r6b M1: the one exception is the
# sign of negative zero, which parses back as +0 and can never carry
# decision meaning for a probability). 17 is not the shortest
# representation; for a provenance column, bit-exactness beats compactness.
jprobs <- function(x) {
  keep <- function(v) {
    if (is.null(v) || !length(v)) return(NULL)
    # unlist() keeps the level-index names; as.numeric() would silently strip
    # them, and an unnamed distribution is a provenance column with its
    # labels lost. Carry the names across the coercion by hand. NA values
    # are kept as-is: provenance reports what the vendor sent, it does not
    # filter it. Names are returned EXACTLY as stored -- duplicates
    # included: display redaction can legitimately merge two distinct
    # Choice labels, and any de-duplication on one side only made the
    # answer object and the probs_json column disagree (audit r6b
    # R6b-B2 / r6c R6c-B2; make.unique was the second failed attempt --
    # adversarial labels defeated it too). Merged names are ambiguous by
    # nature; frame$p holds the pre-redaction selected value.
    flat <- unlist(v, recursive = TRUE)
    out <- suppressWarnings(as.numeric(flat))
    names(out) <- names(flat)
    out
  }
  if (inherits(x, "jev_answer")) return(keep(x$probs))
  if (is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)) {
    parsed <- tryCatch(jsonlite::fromJSON(x, simplifyVector = FALSE),
                       error = function(e) NULL)
    # probs_json shape (see .cache stamping): {"p": [[name, value], ...]},
    # plus "named": false ONLY when the source vector had no names attribute
    # (r6d R6d-m1: null pair-names are otherwise ambiguous between "no
    # names" and "all-NA names"). A JSON object cannot honestly store
    # duplicate keys, so the distribution travels as pairs under a fixed
    # key; the fixed key also keeps a one-entry pair array from being
    # ambiguous with an object. Decode back to a named vector with the
    # names byte-exact; unnamed decodes to an UNNAMED vector so the answer
    # side and the column side agree for every input shape.
    if (is.list(parsed) && !is.null(parsed[["p"]]) && is.list(parsed[["p"]])) {
      prs <- parsed[["p"]]
      shaped <- length(prs) > 0L && all(vapply(prs, function(p)
        is.list(p) && length(p) == 2L &&
          (is.null(p[[1L]]) || (is.character(p[[1L]]) && length(p[[1L]]) == 1L)),
        logical(1)))
      if (shaped) {
        vals <- vapply(prs, function(p) {
          v <- p[[2L]]
          if (is.null(v)) NA_real_ else suppressWarnings(as.double(v[[1L]]))
        }, NA_real_)
        if (isFALSE(parsed[["named"]])) return(vals)
        nms <- vapply(prs, function(p) p[[1L]] %||% NA_character_, NA_character_)
        return(stats::setNames(vals, nms))
      }
    }
    return(keep(parsed))
  }
  NULL
}

jev_usage <- function() {
  u <- .RjifEnv$usage
  cost <- u$input_tokens / 1e6 * JEV_PRICE_PER_MTOK
  list(calls = u$calls, input_tokens = u$input_tokens, est_cost_usd = cost)
}
jev_usage_reset <- function() {
  .RjifEnv$usage <- list(calls = 0L, input_tokens = 0L)
  invisible(NULL)
}

# Scoped options: run an expression with Rjif options overridden, then restore.
# Used throughout the tests and demos to swap transports without leaking global
# state (a plain options() call is never restored on error). Returns the value
# of expr invisibly.
#
# Usage: withr_options(Rjif.transport = my_mock, jif(state, question))
# 'expr' is the first formal so the trailing unnamed argument matches it; with
# the signature (..., expr) every unnamed argument is absorbed by '...' and the
# expression would be forced before the options are set.
withr_options <- function(expr, ...) {
  new <- list(...)
  if (!length(new)) stop("Rjif: withr_options() needs at least one option.", call. = FALSE)
  if (is.null(names(new)) || any(!nzchar(names(new)))) {
    stop("Rjif: withr_options() arguments must be named (option = value).",
         call. = FALSE)
  }
  nms <- names(new)
  # NULL means "unset this option" (same convention as options())
  old <- stats::setNames(lapply(nms, function(n) getOption(n)), nms)
  on.exit({
    restore <- stats::setNames(as.list(old), nms)
    do.call(options, restore)
    invisible(NULL)
  }, add = TRUE, after = FALSE)
  do.call(options, stats::setNames(as.list(new), nms))
  invisible(force(expr))
}

print.jev_answers <- function(x, ...) {
  for (nm in names(x)) {
    a <- x[[nm]]
    v <- a$value
    vs <- if (is.na(v)) "NA" else if (is.character(v)) sprintf("'%s'", v)
                                else formatC(v, format = "f", digits = 4)
    cat(sprintf("[%s] %s: %s", a$type, nm, vs))
    if (!is.null(a$probs)) {
      pv <- vapply(a$probs, function(p) if (is.na(p)) "NA" else sprintf("%.3f", p),
                   character(1))
      cat("  (", paste0(names(pv), "=", pv, collapse = ", "), ")", sep = "")
    }
    cf <- a$confidence
    if (length(cf) == 1L && !is.na(cf)) cat("  conf=", sprintf("%.3f", cf), sep = "")
    if (!is.null(a$legend)) {
      # legend entries may be plain descriptions OR structured level objects
      # ({what, examples} in any key order -- audit R11: z[[1L]] crashed when
      # examples preceded what). Coerce to a single display label per level.
      lg <- if (is.list(a$legend)) {
        vapply(a$legend, function(z) {
          if (is.list(z)) {
            if (!is.null(z[["what"]])) as.character(z[["what"]][[1L]])
            else paste0(names(z), collapse = "/")
          } else if (length(z) == 1L) {
            as.character(z)
          } else {
            paste0("[", paste(utils::head(as.character(z), 2L), collapse = "|"),
                   "]")
          }
        }, character(1))
      } else as.character(a$legend)
      cat("\n     legend: ", paste0(names(lg), "=", lg, collapse = "; "), sep = "")
    }
    cat("\n")
  }
  invisible(x)
}
