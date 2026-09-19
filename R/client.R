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
.redact_value <- function(x, depth = 0L) {
  if (depth > 24L) return("[REDACTED-DEPTH]")
  tt <- typeof(x)
  if (tt %in% c("environment", "closure", "special", "builtin", "S4", "name",
                "symbol", "call", "expression", "pairlist", "weakref",
                "externalptr", "char", "...")) {
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
  if (is.character(x)) {
    y <- .scrub_secrets(.redact_key(x))
  } else if (is.list(x)) {
    y <- lapply(x, .redact_value, depth = depth + 1L)
  } else {
    y <- x
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
    as.double(pv)
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
    if (abs(s - 1) > 0.01) {
      return(invalid(paste0("probabilities sum to ", formatC(s, format = "f",
                                                             digits = 3), ", not ~1")))
    }
    probs <- stats::setNames(as.list(lapply(vals, function(x) x / s)), pn)
    return(list(value = v, probs = probs, confidence = conf_valid,
                valid = TRUE, reason = NA_character_))
  }
  # score: a LEVEL INDEX into the rubric, not a continuous value
  v <- suppressWarnings(as.double(v))
  k <- length(q$criteria)
  if (length(v) != 1L || is.na(v) || !is.finite(v) || abs(v - round(v)) > 1e-9 ||
      v < 0 || v > k - 1) {
    return(invalid(paste0("score value is not an integer level index in 0..", k - 1)))
  }
  if (is.na(conf_valid)) {
    return(invalid("score answer lacks a confidence in [0, 1]"))
  }
  list(value = v, probs = NULL, confidence = conf_valid,
       valid = TRUE, reason = NA_character_)
}

# Default transport over httr. Returns the parsed response list.
.transport_httr <- function(body) {
  if (!requireNamespace("httr", quietly = TRUE) ||
      !requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Rjif needs the 'httr' and 'jsonlite' packages.", call. = FALSE)
  }
  resp <- httr::POST(jev_endpoint(),
    httr::add_headers(Authorization = paste("Bearer", jev_key()),
                      `Content-Type` = "application/json"),
    body = jsonlite::toJSON(body, auto_unbox = TRUE, null = "null"),
    encode = "raw")
  status <- tryCatch(httr::status_code(resp), error = function(e) NA_integer_)
  ok <- tryCatch(httr::http_status(resp)$category == "Success", error = function(e) FALSE)
  if (!isTRUE(ok)) {
    # NOTE: never interpolate jev_key() or the request headers into this message.
    haltxt <- tryCatch(rawToChar(resp$content), error = function(e) "")
    if (!nzchar(haltxt)) haltxt <- tryCatch(httr::content(resp, "text", encoding = "UTF-8"),
                                           error = function(e) "")
    stop("Rjif: API call to ", jev_endpoint(), " failed (HTTP ",
         if (is.na(status)) "?" else status, "): ",
         .clean_error_text(paste(haltxt, collapse = " ")),
        if (identical(status, 401L) || identical(status, 403L))
          " -- check TYPESAFE_API_KEY." else "",
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

# Question constructors -------------------------------------------------------

# noul: "is this statement true of the state?" -> 0-1
jev_noul_q <- function(instructions) {
  structure(list(type = "noul", instructions = instructions), class = "jev_question")
}

# choice: route among named, described options -> one option + full distribution
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
  if (anyDuplicated(nms)) {
    stop(.clean_error_text(paste0(
      "Rjif: choice criteria names must be unique; duplicated: ",
      paste(unique(nms[duplicated(nms)]), collapse = ", "), ".")),
      call. = FALSE)
  }
  structure(list(type = "choice", instructions = instructions,
                 criteria = as.list(criteria)), class = "jev_question")
}

# score: rate the state on an ordered rubric (vector of level descriptions,
# lowest severity first). Levels are labelled 0..k-1 in the response legend.
jev_score_q <- function(instructions, criteria) {
  crit <- as.character(criteria)
  if (!length(crit)) {
    stop("Rjif: score criteria must be a non-empty character vector of ordered ",
         "level descriptions.", call. = FALSE)
  }
  if (any(!nzchar(crit)) || any(is.na(crit))) {
    stop("Rjif: score criteria may not contain empty or NA level descriptions.",
         call. = FALSE)
  }
  if (!is.null(names(criteria))) crit <- paste0(names(criteria), ": ", crit)
  structure(list(type = "score", instructions = instructions, criteria = crit),
            class = "jev_question")
}

# Core call --------------------------------------------------------------------
# jev_eval(state = <character>, questions = list(a = jev_noul_q(...), ...))
# returns a 'jev_answers' list, one entry per question, in the same order.
jev_eval <- function(state, questions, model = getOption("Rjif.model", "jev-latest")) {
  # The API accepts exactly ONE state per call. A vector here used to be
  # silently truncated to its first element (audit M2): reject it and point
  # callers at jev_score_many(), which loops properly.
  if (!is.character(state) || length(state) != 1L || is.na(state)) {
    stop("Rjif: state must be a single non-NA string",
         if (is.character(state) && length(state) > 1L)
           paste0(" (got ", length(state), "; use jev_score_many() for vectors)")
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

  body <- list(state = state, model = model,
               questions = lapply(questions, function(q) unclass(q)))

  transport <- getOption("Rjif.transport")
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
  structure(ans,
            model = .scrub_secrets(.redact_key(
              as.character(.as_scalar(raw[["model"]], "character") %||% model))),
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

# The answer's value: noul/score -> numeric 0-1 / level index; choice -> option name.
jvalue <- function(ans) ans$value

# Reported confidence: the API's separate confidence scalar, present only on
# choice and score answers. For a noul answer the API sends no confidence at
# all, so this is NA by design -- jprob() is the accessor that reports
# whichever probability backs the decision (noul score / chosen option p /
# score confidence). Do not threshold noul answers with jconf(); jif() and
# jev_score_many() apply confidence_floor to jprob() for exactly this reason.
jconf <- function(ans) `%||%`(ans$confidence, NA_real_)

# Probability that the picked answer is right: for noul the noul score, for
# choice the probability of the chosen option, for score the API confidence (a
# score answer has no per-level distribution, only a legend).
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
      lg <- if (is.list(a$legend)) vapply(a$legend, function(z) as.character(z[[1L]]),
                                          character(1))
            else as.character(a$legend)
      cat("\n     legend: ", paste0(names(lg), "=", lg, collapse = "; "), sep = "")
    }
    cat("\n")
  }
  invisible(x)
}
