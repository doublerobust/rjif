# Control-flow verbs: jif() / j_ifelse() / jmatch().
#
# The design is deliberately explicit -- no lazy magic, and we never hijack
# base `if`. jif() returns a value you can drop into `if (...)`:
#
#   if (jif(ticket, "customer is about to churn")) { ... } else { ... }
#
# Honesty contract -- the whole point of a calibrated System One interface:
#
#   * jif() distinguishes "the test decided no" from "the test could not
#     decide". Decided-false comes from evidence; undecided comes from a
#     missing answer or from confidence under confidence_floor.
#   * the undecided outcome is abstain, which DEFAULTS TO NA. `if (NA)` raises
#     an error in base R, and that error is the point: an unmade clinical
#     triage decision must stop the program rather than silently take the
#     else-branch. If you want unknowns routed to the negative branch instead,
#     say so out loud: jif(..., abstain = FALSE) is a documented policy choice,
#     not the default.
#   * the recommended branching interface is j_ifelse(), which has an explicit
#     third lane for abstentions and therefore never crashes and never guesses:
#
#       j_ifelse(jif(ticket, "wants a refund"),
#                yes = "issue return label",
#                no  = "reply asking what they want",
#                unknown = "queue for human")
#
#   * every jif() return carries attributes: $abstained (TRUE/FALSE) and
#     $answer (the full jev_answer, so jconf()/jprob() and the raw response stay
#     reachable downstream).
#
# Note on confidence_floor and noul questions: the API sends no confidence for
# noul answers, so the floor is applied to the noul score itself (which IS the
# probability that the assertion holds). For choice questions the floor is
# applied to the probability of the selected option; for score questions to the
# API confidence. jprob() gives you whichever of those applies.
#
# TWO-SIDED DECISIONS (external audit finding 3, 2026-09-19): with threshold
# 0.5, applying a single POSITIVE floor (e.g. 0.7) means only TRUE and NA are
# reachable -- P(yes)=0.01, a confident NO, abstains. For noul questions a
# floor above the threshold is therefore honoured as a two-sided decision
# window: decide TRUE when p >= confidence_floor, decide FALSE when
# p <= 1 - confidence_floor (equivalently 1-p >= floor: the SAME evidence
# standard applied to the negative side), abstain in between. This makes the
# README's auto-triage / routine-pile / human-reviewer pattern actually
# reachable. choice and score questions keep the single-sided floor (their
# "negative side" is a set of named options, not a complement -- 1-p has no
# decision meaning there), and jif_reason() spells the window out.
jif <- function(state, question, ..., threshold = 0.5,
                abstain = NA, confidence_floor = 0,
                model = getOption("Rjif.model", "jev-latest")) {
  q <- .as_question(question, ...)
  threshold <- .single_number(threshold, "threshold")
  confidence_floor <- .single_number(confidence_floor, "confidence_floor",
                                     allow_na = TRUE)
  # Keep NA as a policy value so diagnostics never expose an Inf sentinel.
  .check_floor_range(confidence_floor)
  ans <- jev_eval(state, list(q = q), model = model)$q

  # Decision policy lives in .decide_answer(), shared verbatim with
  # jev_score_many() so single-row and batch calls can never drift.
  d <- .decide_answer(ans, q, threshold, confidence_floor)
  if (is.na(d$dec)) {
    return(structure(abstain, abstained = TRUE, answer = ans,
                     abstain_reason = d$reason))
  }
  structure(d$dec, abstained = FALSE, answer = ans,
            abstain_reason = NA_character_)
}

.single_number <- function(x, what, allow_na = FALSE) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) != 1L) {
    stop("Rjif: ", what, " must be a single number",
         if (allow_na) " (or NA)" else "", ".", call. = FALSE)
  }
  if (is.na(x) && !allow_na) {
    stop("Rjif: ", what, " must not be NA.", call. = FALSE)
  }
  x
}

# A confidence floor outside [0, 1] is almost always a typo (0-100 scale?).
# Warn but keep the user's stated behaviour: <=0 never gates, >1 always gates.
.check_floor_range <- function(floor) {
  if (is.finite(floor) && (floor < 0 || floor > 1)) {
    warning("Rjif: confidence_floor ", formatC(floor, format = "f", digits = 3),
            " is outside [0, 1] (did you mean a percentage?). ",
            if (floor <= 0) "It will never trigger an abstention." else
              "It will abstain on every row.",
            call. = FALSE)
  }
  invisible(floor)
}

# Build a question from shorthand: a plain string means a Noul assertion.
# A *named* character vector/list as the first extra argument means a choice
# question (option = description). An unnamed character vector is rejected --
# it is far more likely a typo than an intention.
.as_question <- function(question, ...) {
  if (inherits(question, "jev_question")) return(question)
  if (is.character(question) && length(question) == 1L && !is.na(question)) {
    dots <- list(...)
    if (length(dots)) {
      d <- dots[[1L]]
      if (inherits(d, "jev_question")) {
        stop("Rjif: a jev_*_q() spec was passed as an extra argument after a ",
             "string shorthand; pass it as 'question' instead.", call. = FALSE)
      }
      if ((is.list(d) || is.character(d)) && !is.null(names(d)) && any(nzchar(names(d)))) {
        # names exactly within {true, false} = noul criteria clarification,
        # not a choice question (audit finding 4: jev_noul_q now exposes the
        # documented optional criteria; the shorthand must not misread them
        # as two options named "true"/"false")
        dn <- unique(names(d)[nzchar(names(d))])
        if (all(dn %in% c("true", "false"))) {
          return(jev_noul_q(question, criteria = d))
        }
        return(jev_choice_q(question, d))
      }
      if (is.list(d) || is.character(d)) {
        stop("Rjif: choice criteria must be NAMED (option = description). ",
             "Got an unnamed ", .clean_error_text(class(d)[[1L]], 40L),
             " of length ", length(d), ".",
             call. = FALSE)
      }
    }
    return(jev_noul_q(question))
  }
  stop("Rjif: question must be a single non-NA string or a ",
       "jev_noul_q/jev_choice_q/jev_score_q spec.", call. = FALSE)
}

# Shared decision core for jif() and jev_score_many() so the two can never
# drift (external audit finding 3). Given a validated answer `ans` for
# question `q`, a threshold, and a floor, returns list(dec, reason):
#   dec      TRUE/FALSE, or NA (abstain; `reason` explains).
#   reason   NA_character_ when decided; abstention text otherwise.
# Noul with floor > threshold uses the two-sided window: TRUE above the floor,
# FALSE below 1-floor, abstain in between. All other combinations keep the
# single-sided policy: gate jprob() against the floor, then threshold.
# Shared inclusive two-tail predicate. Callers select this window only when
# threshold < floor <= 1; other floors keep the single-sided p >= floor rule.
# The epsilon corrects subtraction at the negative cutoff (1 - 0.8 < 0.2).
.noul_window_decided <- function(p, f) {
  (p >= f) | (p <= 1 - f + 1e-9)
}

.decide_answer <- function(ans, q, threshold, confidence_floor) {
  if (is.na(confidence_floor)) return(list(
    dec = NA, reason = "confidence_floor is NA; refusing to decide"))
  v <- jvalue(ans)
  if (is.na(v)) return(list(dec = NA, reason = "no answer value from the API"))
  if (q$type == "noul" && confidence_floor > threshold &&
      confidence_floor <= 1) {
    p <- suppressWarnings(as.double(v))
    if (is.na(p)) return(list(dec = NA, reason = "confidence unavailable with a floor set"))
    neg_cut <- 1 - confidence_floor
    if (.noul_window_decided(p, confidence_floor)) {
      # TRUE takes precedence where the two tails overlap.
      return(list(dec = p >= confidence_floor, reason = NA_character_))
    }
    return(list(dec = NA, reason = paste0(
      "probability ", formatC(p, format = "f", digits = 3),
      " is in the uncertain middle of the two-sided confidence_floor ",
      formatC(confidence_floor, format = "f", digits = 3),
      " (negative cut ", formatC(neg_cut, format = "f", digits = 3), ")")))
  }
  if (confidence_floor > 0) {
    p <- jprob(ans)
    if (is.na(p)) return(list(dec = NA, reason = "confidence unavailable with a floor set"))
    if (p < confidence_floor) {
      return(list(dec = NA, reason = paste0(
        "probability ", formatC(p, format = "f", digits = 3),
        " < confidence_floor ", formatC(confidence_floor, format = "f", digits = 3))))
    }
  }
  dec <- switch(q$type,
    noul  = v >= threshold,
    choice = v,           # option name; the caller records it in 'option'
    score = v >= threshold,
    NA)
  list(dec = dec, reason = NA_character_)
}

# Was a jif() result an abstention? Also TRUE for a bare NA, so that
# j_ifelse() cannot mistake "no decision" for "decision: no".
jif_abstained <- function(x) isTRUE(attr(x, "abstained")) ||
  (is.atomic(x) && length(x) == 1L && is.na(x))

# Why did it abstain? Useful in logs and escalation queues.
jif_reason <- function(x) {
  r <- attr(x, "abstain_reason")
  if (is.null(r) || !length(r)) return(NA_character_)
  as.character(r[[1L]])
}

# Vectorised: evaluate one judgment over every element of a state vector.
# state_vec: character (or factor) vector of per-row states -- paste your
# columns together first.
#
# Returns a data.frame with one row per input, in the same order:
#   decision   logical NA tri-state (audit B2): TRUE/FALSE only when the
#              question was actually decided; NA when undecided. A bare `if()`
#              on an NA decision errors, which is the point -- an undecided
#              clinical row must never fall into the negative branch because a
#              batch helper wrote FALSE for it. Read abstained/error too.
#   option     character: the chosen option (choice), "true"/"false" (noul),
#              or the CONTINUOUS rubric position to 3 decimals as text (score,
#              e.g. "1.050" -- it can land between levels); NA when undecided.
#   p          the probability behind the decision (jprob(): the noul score, the
#              chosen option's probability, or the score confidence); NA when the
#              API sent nothing.
#   confidence API confidence. ALWAYS NA for noul questions -- the API sends no
#              confidence for noul, which is why confidence_floor is applied to
#              'p' for that type.
#   abstained  TRUE when the row could not be decided: no answer value, p under
#              confidence_floor (single-sided) or inside the two-sided noul
#              floor window, p unavailable while a floor was set, an invalid
#              API answer, or an NA state / transport failure for that row.
#   error      per-row failure text ("" when the row succeeded), scrubbed of any
#              control characters AND of the caller's API key before it is
#              persisted, so one bad narrative does not discard the other 4,999
#              and a printed data frame can never leak the credential.
#
# 'batch' is the internal chunk size: one HTTP request per row either way
# (the API takes ONE state per call). It changes nothing without a cache;
# with cache = <path> it bounds how much completed work an interrupt can
# lose, because the cache is rewritten once per chunk.
jev_score_many <- function(state_vec, question, ...,
                           threshold = 0.5, confidence_floor = 0,
                           batch = 16L, cache = NULL,
                           model = getOption("Rjif.model", "jev-latest")) {
  q <- .as_question(question, ...)
  threshold <- .single_number(threshold, "threshold")
  confidence_floor <- .single_number(confidence_floor, "confidence_floor",
                                     allow_na = TRUE)
  .check_floor_range(confidence_floor)
  if (!is.character(state_vec) && !is.factor(state_vec)) {
    stop("Rjif: state_vec must be a character (or factor) vector.", call. = FALSE)
  }
  state_vec <- as.character(state_vec)
  # Canonicalise to UTF-8 before BOTH the cache digest and request
  # serialisation (external audit round 3, finding R3-1): R strings carry
  # per-element encoding flags, so "é" marked UTF-8 and the same BYTES marked
  # latin1 are different text that charToRaw() -- and therefore an
  # encoding-blind digest -- cannot tell apart, while toJSON() sends them as
  # different payloads. One representation for hash and wire removes the
  # mismatch; it also unifies the same text arriving under different declared
  # encodings, which is the desirable direction for resume identity.
  # Invalid byte sequences and "bytes"-marked strings are rejected up front
  # rather than hashed. Note validEnc() is TRUE for Encoding == "bytes" and
  # enc2utf8() preserves that flag without converting it (round 4, R4-1): a
  # bytes-marked state can never be serialised by the JSON transport, so it
  # must not obtain a successful decision through a cache hit either.
  bad <- vapply(state_vec, function(s) {
    !is.na(s) && (Encoding(s) == "bytes" || !validEnc(s))
  }, logical(1))
  if (any(bad)) {
    stop("Rjif: state_vec elements ",
         paste(which(bad), collapse = ", "),
         " are 'bytes'-marked or contain invalid byte sequences; re-encode ",
         "the input (e.g. stringi::str_conv or iconv) before scoring.",
         call. = FALSE)
  }
  state_vec <- enc2utf8(state_vec)
  # Model encoding (audit r6e R6e-B1): the SAME rule must hold for the
  # model string. enc2utf8() preserves an Encoding == "bytes" flag without
  # converting, and charToRaw then digests it identically to the
  # UTF-8-marked copy of the same bytes -- but jsonlite REFUSES to
  # serialize bytes-marked strings, so such a model can never go on the
  # wire. Without this check it could still obtain a cached successful
  # decision via a redaction-collapsed resume (fixture: model y marked
  # "bytes" resumed model x's 0.9 with zero calls, while a fresh y errored
  # 'translating strings with "bytes" encoding is not allowed'). Rejecting
  # up front fails closed like the state path; same-model resume for
  # supported encodings is unaffected.
  model_bare <- as.character(unname(model))
  if (any(vapply(model_bare, function(s) {
        !is.na(s) && (Encoding(s) == "bytes" || !validEnc(s))
      }, logical(1)))) {
    stop("Rjif: model is 'bytes'-marked or contains invalid byte ",
         "sequences and can never be serialized into a request; re-encode ",
         "it (e.g. stringi::str_conv or iconv) before scoring.",
         call. = FALSE)
  }
  n <- length(state_vec)
  batch <- suppressWarnings(as.integer(batch))
  if (length(batch) != 1L || is.na(batch) || batch < 1L) batch <- 16L

  # Cache identity: full question, requested model, row count, decision
  # policy, AND an order-sensitive digest of the
  # state CONTENT. Without the digest, re-sorting or editing the extract
  # between passes silently re-attached old judgments to the wrong records
  # (Codex fixture: c("present","absent") cached 0.99/0.01, then
  # c("absent","present") replayed 0.99/0.01 with zero new calls). The digest
  # stores no plaintext states. Not a security hash: it detects accidental
  # reorders and changed extracts, it is not tamper-proof.
  cache_resumed <- 0L
  # version 5L: the frame gains per-row provenance columns (model,
  # evaluated_at, row_source, score_value, probs) -- round 3 carryover 1.
  # An old v4 cache would ALSO fail the frame-shape check, but the version
  # tag makes the reason explicit in the refusal.
  # Model identity (audits r6b R6b-B1 / r6c R6c-B1 / r6e R6e-B1 -- read ALL
  # THREE before "fixing" this again): storing the raw `model` ARGUMENT
  # smuggled attribute payloads into the saved cache (r6b); storing the
  # SCRUBBED string merged two distinct aliases whenever display redaction
  # rewrote both into one marker, letting a second alias resume the first
  # alias's decisions with zero calls (r6c); hashing raw bytes missed that
  # a string whose Encoding is "bytes" can never be serialized by the
  # transport yet shared a digest with the UTF-8-marked copy of its bytes
  # (r6e). The answer: bytes-marked and invalid-byte models are REJECTED up
  # front (before any cache lookup or write, like states are), and the
  # resume identity is .model_identity -- the whole-content digest of the
  # model after the SAME declared-encoding -> UTF-8 conversion the JSON
  # serializer performs, taken on the explicitly coerced bare string
  # (as.character(unname(.)) is the coercion that drops attributes;
  # argument passing itself RETAINS them -- auditor's identity-function
  # control, r6d). The readable entry is the scrubbed bare string,
  # display-only and explicitly NOT authoritative.
  # Question identity (audit r6e, inherited v4 path, reported alongside):
  # serialize(unclass(q)) stores native-encoded strings as locale-blind
  # BYTES, so a question text that jsonlite would post as different words
  # under two LC_CTYPE settings could resume its own cache unchanged across
  # the locale change. .question_identity normalizes every character
  # element with enc2utf8 BEFORE storing, so the fingerprint sees the same
  # text the wire sends and a locale change between fill and resume now
  # refuses.
  # Version 6L: model_id digest entry (r6c R6c-B1). Version 7L: the digest
  # input is the transport-canonicalized model/question (r6e); 5L/6L-era
  # caches predate the whole pre-release feature and are refused as stale.
  cache_fingerprint <- serialize(list(version = 7L, question = .question_identity(q),
    model = .bare_char(model), model_id = .model_identity(model),
    n = n, threshold = threshold, floor = confidence_floor,
    states = .state_digest(state_vec)),
    NULL, version = 2)
  dec <- rep(NA, n); ps <- rep(NA_real_, n); cf <- rep(NA_real_, n)
  chosen <- rep(NA_character_, n); abst <- rep(NA, n); errs <- rep("", n)
  failed <- rep(FALSE, n)
  filled <- rep(FALSE, n)
  # Per-row provenance (round 3 carryover 1). model/evaluated_at describe the
  # row's ACTUAL evaluation (persisted across resumes, so an interrupted run
  # whose 'jev-latest' alias changed underneath it can distinguish day-1 rows
  # from day-2 rows); row_source describes THIS run: "cache" (resumed),
  # "api" (attempted a call), or "none" (abstained without any call, e.g.
  # NA state). score_value keeps the exact numeric score for score batches --
  # `option` remains a 3-decimal display string and was the only carrier.
  # probs_json holds the full named distribution as a JSON string (NA for
  # noul answers, which have no distribution, and for rows without one).
  # A character column, not a list column: data.frame list columns drop NULL
  # elements silently (`d$x <- list_of_NULLs` mis-sizes), and an atomic column
  # keeps the cache frame fully round-trippable and type-checkable.
  # jprobs() parses it back to a named numeric vector.
  mdl <- rep(NA_character_, n)
  whenat <- rep(NA_character_, n)
  src <- rep("none", n)
  score_val <- rep(NA_real_, n)
  probs_json <- rep(NA_character_, n)
  if (!is.null(cache)) {
    if (!is.character(cache) || length(cache) != 1L || is.na(cache) ||
        !nzchar(cache)) {
      stop("Rjif: cache must be a single path string (or NULL).", call. = FALSE)
    }
    if (file.exists(cache)) {
      prev <- tryCatch(readRDS(cache), error = function(e) {
        # The file exists but is not a readable RDS. It may not be a cache at
        # all; refusing beats silently overwriting the caller's bytes.
        stop("Rjif: cache file '", cache, "' exists but could not be read as ",
             "an RDS (", .clean_error_text(conditionMessage(e), 120L),
             "). Nothing was written to it: point 'cache' at a different path, ",
             "or delete/rename the file if you meant to start over.",
             call. = FALSE)
      })
      if (.cache_valid(prev, n, cache_fingerprint)) {
        dec <- prev$decision; chosen <- prev$option; ps <- prev$p
        cf <- prev$confidence; abst <- prev$abstained; errs <- prev$error
        failed <- attr(prev, "row_failed", exact = TRUE)
        # Restore per-row provenance. v5 frames carry the columns; a frame
        # without them (impossible past the v5 fingerprint today, kept as a
        # forward-compatible read) falls back to NA model/time.
        mdl <- prev$model %||% mdl
        whenat <- prev$evaluated_at %||% whenat
        score_val <- prev$score_value %||% score_val
        probs_json <- prev$probs_json %||% probs_json
        rerun_err <- isTRUE(getOption("Rjif.cache_rerun_errors", TRUE))
        filled <- !is.na(abst) & (if (rerun_err) !failed else TRUE)
        cache_resumed <- sum(filled)
        # row_source describes THIS run: a reused row is "cache" UNLESS the
        # persisted row was never attempted at all (row_source "none", e.g.
        # an NA state) -- that fact survives resumes and must not be
        # laundered into "cache". Rows being (re)attempted now stay "api".
        if (any(filled)) {
          prev_src <- prev$row_source
          was_none <- if (!is.null(prev_src) && length(prev_src) == n)
            prev_src == "none" else rep(FALSE, n)
          src[filled] <- ifelse(was_none[filled], "none", "cache")
        }
      } else {
        # REFUSE rather than overwrite (external audit r2 finding 2): a
        # mismatched cache is the only prior artifact of a run that may have
        # cost money; silently replacing it (the old behavior) threw it away
        # AND re-billed everything. Stop and let the caller decide.
        stop("Rjif: cache file '", cache, "' exists but does not match this ",
             "run (state contents/order, row count, frame shape, question, ",
             "model, or decision policy). Its previous results are NOT reused ",
             "and its file is NOT overwritten: point 'cache' at a new path for ",
             "this run, or delete/rename the old file if you meant to start ",
             "over.", call. = FALSE)
      }
    }
  }

  i <- 1L
  while (i <= n) {
    e <- min(n, i + batch - 1L)
    for (j in i:e) {
      if (isTRUE(filled[[j]])) next   # resumed from cache
      # A new attempt must replace every field, including stale diagnostics.
      dec[j] <- NA; chosen[j] <- NA_character_; ps[j] <- cf[j] <- NA_real_
      abst[j] <- NA; errs[j] <- ""; failed[j] <- FALSE
      mdl[j] <- NA_character_; whenat[j] <- NA_character_
      score_val[j] <- NA_real_; probs_json[j] <- NA_character_
      st <- state_vec[[j]]
      if (is.na(st)) {
        dec[j] <- NA; abst[j] <- TRUE; errs[j] <- "state is NA"
        failed[j] <- TRUE
        next   # row_source stays "none": no call was possible
      }
      src[j] <- "api"
      # jev_eval can warn (contract violation) as well as throw; capture both.
      warns <- character(0)
      envelope <- withCallingHandlers(
        tryCatch(jev_eval(st, list(q = q), model = model),
                 error = function(err) structure(list(msg = conditionMessage(err)),
                                                 class = "jev_row_error")),
        warning = function(w) {
          warns <<- c(warns, conditionMessage(w))
          invokeRestart("muffleWarning")
        })
      ans <- if (inherits(envelope, "jev_row_error")) envelope else envelope$q
      if (inherits(ans, "jev_row_error")) {
        dec[j] <- NA; abst[j] <- TRUE; failed[j] <- TRUE
        errs[j] <- .clean_error_text(ans$msg, 300L)
        next
      }
      v <- jvalue(ans)
      p <- jprob(ans)
      ps[j] <- p
      cf[j] <- jconf(ans)
      # A response arrived: stamp its provenance (round 3 carryover 1).
      # model_returned is the vendor's resolved model for THIS row; captured
      # before display redaction merge issues can matter, and scrubbed at the
      # jev_eval boundary already.
      mdl[j] <- attr(ans, "model_returned") %||% NA_character_
      whenat[j] <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      if (q$type == "score") score_val[j] <- v
      ap <- ans[["probs"]]
      if (q$type != "noul" && !is.null(ap) && length(ap)) {
        probs_json[j] <- .probs_column_json(ap)
      }
      # identical policy to jif() via .decide_answer() (audit finding 3):
      # two-sided noul window when floor > threshold, single-sided otherwise.
      d <- .decide_answer(ans, q, threshold, confidence_floor)
      if (is.na(d$dec)) {
        abst[j] <- TRUE
        dec[j] <- NA
        chosen[j] <- NA_character_
        if (!is.na(d$reason)) errs[j] <- .clean_error_text(d$reason, 300L)
      } else {
        abst[j] <- FALSE
        if (q$type == "choice") {
          chosen[j] <- as.character(d$dec)
          dec[j] <- TRUE   # a decided choice: 'option' carries the routing
        } else if (q$type == "noul") {
          dec[j] <- d$dec
          chosen[j] <- if (isTRUE(d$dec)) "true" else "false"
        } else {
          dec[j] <- d$dec
          chosen[j] <- formatC(v, format = "f", digits = 3)
        }
      }
      # A contract-violating answer arrives here with value NA (above, it
      # abstains) and an explanatory `contract` field; surface it in the error
      # column. .clean_error_text redacts the real API key before persisting.
      note <- ans[["contract"]]
      if (!is.null(note) && !is.na(note) && nzchar(note)) {
        failed[j] <- TRUE
        errs[j] <- .clean_error_text(
          paste(c(paste0("invalid API answer: ", note), warns), collapse = "; "),
          300L)
      } else if (length(warns)) {
        errs[j] <- .clean_error_text(paste(warns, collapse = "; "), 300L)
      }
    }
    i <- e + 1L
    # persist the chunk's results BEFORE advancing, so an interrupt (or a
    # thrown outside error) between chunks loses at most one chunk of work
    if (!is.null(cache)) .cache_save(cache, dec, chosen, ps, cf, abst, errs,
                                     q, failed, cache_fingerprint,
                                     mdl, whenat, src, score_val, probs_json)
  }
  out <- data.frame(decision = dec, option = chosen, p = ps, confidence = cf,
                    abstained = abst, error = errs, stringsAsFactors = FALSE)
  out$model <- mdl
  out$evaluated_at <- whenat
  out$row_source <- src
  out$score_value <- score_val
  out$probs_json <- probs_json
  attr(out, "question_type") <- q$type
  attr(out, "n_abstained") <- sum(abst)
  if (!is.null(cache)) attr(out, "n_resumed") <- cache_resumed
  out
}

# Write a resumable jev_score_many() progress file. Saved WITHOUT row names
# noise and with the fingerprint attr so a later run can verify compatibility.
# Failure to write (disk full, read-only path, permissions) is a warning, not
# an error: a cache is an optimization and must never lose the run's answers
# that are already in memory.
# Content digest of the state vector for cache identity (external audit r2
# finding 2). Built on openssl::md5 -- NOT as a security hash (nothing here
# is tamper-proof), but as a whole-content checksum: an accidental reorder, a
# corrected narrative, a re-sorted extract, or an edit anywhere in the text
# changes it. (A hand-rolled prefix hash was the first attempt and a
# stopifnot in its own unit check caught the gap: a same-length edit beyond
# the 4 KB prefix digest invisible. md5 has no length cap.) States are joined
# with an injected NULL byte separator (0x00, illegal in the UTF-8 states the
# API accepts) plus each state's byte length, so c("a","b") and c("ab") and a
# reordered vector all digest differently. NA and empty string get distinct
# length markers. plaintext states are never stored -- the fingerprint holds
# only the 16-byte digest. openssl is a transitive dependency of httr (which
# Rjif already imports), so this adds no new install requirement.
.state_digest <- function(state_vec) {
  if (!requireNamespace("openssl", quietly = TRUE)) {
    stop("Rjif: the cache feature needs the 'openssl' package (a dependency ",
         "of 'httr', which Rjif already uses; its absence here means a broken ",
         "library).", call. = FALSE)
  }
  raws <- lapply(state_vec, function(s) if (is.na(s)) raw(0) else charToRaw(s))
  sep <- as.raw(0L)
  payload <- unlist(lapply(seq_along(raws), function(i) {
    c(raws[[i]], sep,
      charToRaw(if (is.na(state_vec[[i]])) "N" else format(length(raws[[i]]), scientific = FALSE)),
      sep)
  }), use.names = FALSE)
  # unlist(list()) is NULL, but md5 requires raw(0) for an empty vector.
  d <- openssl::md5(as.raw(payload))
  list(digest = paste(format(d), collapse = ""), n = length(state_vec))
}

# Cache identity of the model argument (audit r6c R6c-B1, residual r6d
# R6d-B1). The readable fingerprint entry must be scrubbed for display
# (r6b R6b-B1: raw attributes smuggled payloads into the saved cache),
# but the SCRUBBED string must never be the RESUME IDENTITY: display
# redaction merges distinct aliases ("Bearer option_alpha" and "Bearer
# option_beta" both become "Bearer [REDACTED]"), which used to let a
# second alias resume the first alias's decisions with zero calls.
# Identity is therefore the same whole-content digest used for states,
# computed on the model EXACTLY AS IT GOES ON THE WIRE (r6d R6d-B1):
# jsonlite serializes character values by converting from their DECLARED
# encoding to UTF-8, so two strings sharing bytes c3 a9 -- one flagged
# UTF-8 ("e-acute"), one flagged latin1 ("A-tilde e-acute") -- post
# DIFFERENT wire bodies (c3 a9 vs c3 83 c2 a9) and are different models,
# yet charToRaw of the raw argument digested them identically. enc2utf8()
# here reproduces that transport conversion on both sides, so the digest
# matches what the vendor is actually asked to score. Function argument
# evaluation does NOT drop attributes (auditor's control: an identity
# function retains them), so the explicit as.character(unname(.))
# coercion is what keeps an attributes-carrying model out of the digest;
# the readable entry stays the scrubbed bare string, display-only and
# explicitly NOT authoritative.
.model_identity <- function(model) {
  .state_digest(enc2utf8(as.character(unname(model))))
}

# Fingerprint copy of the question (audit r6e, inherited v4 path): the
# wire sends question text through jsonlite, which converts every
# character element from its DECLARED encoding to UTF-8 -- an
# "unknown"/native-marked string is therefore locale-dependent text. The
# old fingerprint stored unclass(q) verbatim; R's serializer keeps the
# bytes and the locale-blind flag, so the SAME question filled in one
# LC_CTYPE could resume its cache in another locale even though fresh
# serialization now denotes different words (auditor's cross-locale
# control: silently resumed 0.9 where a fresh call returned 0.1). Storing
# enc2utf8-normalized text makes the fingerprint see the words the
# transport sends: a locale change that reinterprets a native-marked
# string now mismatches and refuses; same-text resume costs zero calls.
# Names (option keys, criteria keys) are caller text too and are
# normalized identically. Not applied to the answer objects or the wire
# body -- only to the fingerprint copy.
.question_identity <- function(q) {
  canon_chr <- function(x) {
    nx <- names(x)
    x <- enc2utf8(unname(x))
    # setNames would strip the "UTF-8" flags it just set (it copies the
    # input's attributes), so rebuild the vector and pin names directly.
    if (!is.null(nx)) `names<-`(x, canon_chr(nx)) else x
  }
  rec <- function(x) {
    if (is.list(x)) {
      nx <- names(x)
      x <- lapply(x, rec)
      if (!is.null(nx)) names(x) <- canon_chr(nx)
      return(x)
    }
    if (is.character(x)) return(canon_chr(x))
    x
  }
  rec(unclass(q))
}

# The probs_json column's storage envelope (audits r6b R6b-B2, r6c R6c-B2,
# r6d R6d-m1, r6e R6e-m1 -- read ALL FOUR before changing it; each prior
# attempt had its own defeat mode):
# * ap is the validator's NORMALIZED distribution (probs, post /sum).
# * digits = 17: every double round-trips exactly through 17 significant
#   decimal digits (a fixed property of binary64 for positive values; -0
#   keeps its VALUE -- a valid probability cannot be below 0, and the sign
#   of a zero never enters a decision), so jprobs() reproduces the
#   validator's numbers rather than adding a second, independently rounded
#   copy. This is NOT the shortest representation -- jsonlite's digits = NA
#   caps at 15 significant digits and loses bits (audit r6 R6-B2 caught the
#   old claim). Longer text is the right trade for a provenance column: it
#   must be byte-stable AND bit-exact.
# * display redaction can legitimately MERGE two distinct option labels
#   into one name, and any suffix-renaming scheme can be defeated by
#   adversarial labels (r6c: make.unique itself output a duplicate when a
#   ".1"-suffixed label already existed, and jsonlite then re-suffixed the
#   column side differently from the answer side). A JSON object cannot
#   honestly carry duplicate keys. So the column is NOT a name-keyed
#   object: it is {"p": [[name, value], ...]} -- pairs under a fixed key
#   (a BARE pair array would be ambiguous for a one-entry distribution
#   like {"a":1}, which decodes as a pair too). Names are stored
#   byte-exactly, duplicates and all; jprobs() decodes the same pairs. The
#   two representations of one distribution are identical by construction
#   for ANY labels. After a redaction merge the names are ambiguous BY
#   NATURE; no probability may be looked up by a merged name -- frame$p
#   holds the selected value, bound before redaction, and is authoritative.
# * a pair array cannot distinguish "vector had NO names attribute" from
#   "all-NA names" -- r6d added the "named": false flag for the first
#   shape. R6e R6e-m1 caught that the writer emitted R NULL (serialized as
#   {} by jsonlite's default null policy, which na = "null" does not
#   cover) while jprobs() only accepted JSON null, so the real writer's
#   unnamed bytes fell through the pair decoder into the legacy object
#   path and picked up named=0 as a third entry -- and the round-6d smoke
#   test passed because it hand-wrote null instead of calling this
#   function. Lesson pinned by test: exercise THIS writer, never a
#   lookalike fixture. null = "null" makes every missing name (NULL or
#   NA) serialize as JSON null consistently.
.probs_column_json <- function(ap) {
  apn <- names(ap)
  dist <- list(p = lapply(seq_along(ap), function(i)
    list(apn[[i]], ap[[i]])))
  if (is.null(apn)) dist$named <- FALSE
  as.character(jsonlite::toJSON(dist, digits = 17, auto_unbox = TRUE,
                                na = "null", null = "null"))
}

.cache_valid <- function(df, n, fingerprint) {
  cols <- c("decision", "option", "p", "confidence", "abstained", "error",
            "model", "evaluated_at", "row_source", "score_value", "probs_json")
  if (!is.data.frame(df) || nrow(df) != n || !identical(names(df), cols) ||
      !identical(attr(df, "cache_fingerprint", exact = TRUE), fingerprint)) return(FALSE)
  failed <- attr(df, "row_failed", exact = TRUE)
  is.logical(df$decision) && is.character(df$option) && is.numeric(df$p) &&
    is.numeric(df$confidence) && is.logical(df$abstained) &&
    is.character(df$error) && !anyNA(df$error) &&
    is.character(df$model) && is.character(df$evaluated_at) &&
    is.character(df$row_source) && is.numeric(df$score_value) &&
    is.character(df$probs_json) &&
    is.logical(failed) && length(failed) == n && !anyNA(failed)
}

.cache_save <- function(cache, dec, chosen, ps, cf, abst, errs, q, failed,
                        fingerprint, mdl, whenat, src, score_val, probs_json) {
  df <- data.frame(decision = dec, option = chosen, p = ps, confidence = cf,
                   abstained = abst, error = errs, stringsAsFactors = FALSE)
  df$model <- mdl
  df$evaluated_at <- whenat
  df$row_source <- src
  df$score_value <- score_val
  df$probs_json <- probs_json
  attr(df, "question_type") <- q$type
  attr(df, "cache_fingerprint") <- fingerprint
  attr(df, "row_failed") <- failed
  # Write alongside the target and rename only after serialization finishes.
  # An interrupt or failed write must not truncate the last completed chunk.
  tmp <- tempfile(pattern = ".rjif-cache-", tmpdir = dirname(cache))
  on.exit(unlink(tmp), add = TRUE)
  tryCatch({
    saveRDS(df, tmp)
    if (!file.rename(tmp, cache)) stop("could not replace cache file")
  }, error = function(e) {
    warning("Rjif: could not write cache file '", cache, "' (",
            .clean_error_text(conditionMessage(e), 120L),
            "); results still returned, but this chunk was not saved.",
            call. = FALSE)
    invisible(NULL)
  })
}

# ifelse-style verb with an explicit unknown lane. test is the value returned
# by jif(); the abstention attribute is checked first, then a bare NA, so an
# undecided judgment can never leak into 'yes' or 'no'.
#
# ARM EVALUATION (external audit, 2026-09-19): `yes`/`no` are promises. The
# old routing called names(yes) to find branch labels, which FORCED the whole
# arm - a side-effecting branch (issue the refund label, page the on-call)
# could run even when routing went to the other side. Routing below uses
# substitute() to read the caller's EXPRESSION: a literal list(...)/c(...)
# yields its tags without evaluating anything, and only the winning element
# is then evaluated, in the caller's frame. Arms passed as pre-built
# variables cannot be inspected without forcing, and cannot be inspected at
# all this way - they fall back to value-based routing (identical behavior to
# the old code for those inputs; a variable's side effects already ran when
# the caller built it, so forcing it here adds none).
j_ifelse <- function(test, yes, no, unknown = NA) {
  if (isTRUE(jif_abstained(test))) return(unknown)
  if (length(test) != 1L) {
    stop("Rjif: j_ifelse() takes a single jif() result. For a whole vector, use ",
         "jev_score_many() and branch on the resulting columns.", call. = FALSE)
  }
  if (is.na(test)) return(unknown)
  if (is.character(test)) {
    env <- parent.frame()
    yx <- substitute(yes)
    nx <- substitute(no)
    yt <- .arm_tags(yx)
    nt <- .arm_tags(nx)
    # Route YES before NO (external audit r2 finding 1): the old code checked
    # all literal tags first, so a literal `no` map could win over a matching
    # prebuilt `yes` map (j_ifelse("a", yes_map, list(a = "NO_A")) returned
    # NO_A), and a prebuilt match was never consulted when any arm was a
    # literal (mixed-map calls routed to unknown). Each side is now probed in
    # its supported representation, YES first: literal expression tags (zero
    # evaluation of the loser -- the laziness guarantee stands), then, if a
    # side has no literal tags, its evaluated value's names (a variable's
    # side effects already ran when the caller built it, so forcing adds
    # none). 'any map' (literal or named-value) selects the unknown lane on
    # no match; plain scalars keep the truthy-yes rule.
    y_named <- nx_named <- NULL
    if (!is.null(yt)) {
      if (test %in% yt) return(.arm_elt(yx, test, env))
    } else if (!missing(yes)) {
      yv <- eval(yx, env); y_named <- names(yv)
      if (!is.null(y_named) && test %in% y_named) return(yv[[test]])
    }
    if (!is.null(nt)) {
      if (test %in% nt) return(.arm_elt(nx, test, env))
    } else if (!missing(no)) {
      nv <- eval(nx, env); nx_named <- names(nv)
      if (!is.null(nx_named) && test %in% nx_named) return(nv[[test]])
    }
    if (!is.null(yt) || !is.null(nt) || !is.null(y_named) || !is.null(nx_named))
      return(unknown)  # named arms on at least one side, no match anywhere
    if (missing(yes)) stop("Rjif: j_ifelse(): 'yes' is missing.", call. = FALSE)
    # Reached only when NEITHER side is any kind of named map (else the guard
    # above returned unknown), so yv is the caller's evaluated scalar `yes`:
    # an option name with no matching map is a truthy pick -> yes.
    return(yv)
  }
  if (isTRUE(test)) yes else no
}

# Internal NSE helpers for j_ifelse(). .arm_tags() returns the branch labels
# of a LITERAL, LABELED list(...)/c(...) call without evaluating it, else NULL
# (scalars and UNLABEDED lists are not branch maps -> value-based fallback).
.arm_tags <- function(expr) {
  if (!is.call(expr)) return(NULL)
  fn <- expr[[1L]]
  if (!(is.name(fn) && as.character(fn) %in% c("list", "c"))) return(NULL)
  tags <- names(as.list(expr[-1L]))
  # unnamed elements of a call's arg list carry "", never NA (no stats dep)
  if (length(tags) == 0L || all(tags == "")) NULL else tags
}

# Evaluate exactly one labeled element of a literal arm expression, in the
# caller's frame (the environment j_ifelse() was called from). Elements are
# expressions; only the winner's is ever evaluated.
.arm_elt <- function(expr, which, env) {
  elements <- as.list(expr[-1L])
  tags <- names(elements)
  if (is.null(tags)) tags <- character(length(elements))
  names(elements) <- tags
  idx <- which(!is.na(tags) & tags == which)
  if (!length(idx)) {  # defensive: tags said yes, extraction disagrees
    stop("Rjif: j_ifelse(): branch '", which, "' vanished from its arm.",
         call. = FALSE)
  }
  eval(elements[[idx[[1L]]]], env)
}

# jmatch: route a state among description-matched branches.
# branches: named list/vector of criteria descriptions (as in jev_choice_q()).
# Returns the winning branch name as a single string, or `fallback` when
#   * the API returned no option, or
#   * the probability of the winning option is under confidence_floor, or
#   * the winner is one of the explicit escape-hatch options named in
#     abstain_options (default: none, other, unspecified, human, ambiguous,
#     unsure) -- those are semantic abstentions the model chose for itself.
# Use fallback = NA_character_ (the default) and test with is.na(), or pass a
# sentinel string. Compare the floor against jprob(), i.e. the probability of
# the selected option -- not against the API's separate confidence scalar.
jmatch <- function(state, branches, ..., instructions = NULL,
                   confidence_floor = 0, fallback = NA_character_,
                   abstain_options = c("none", "other", "unspecified", "human",
                                       "ambiguous", "unsure"),
                   model = getOption("Rjif.model", "jev-latest")) {
  # Same floor policy as jif()/jev_score_many() (audit R2-m3): NA floor =
  # abstain always -> fallback; out-of-[0,1] finite floor warns.
  confidence_floor <- .single_number(confidence_floor, "confidence_floor",
                                     allow_na = TRUE)
  if (is.na(confidence_floor)) return(fallback)
  .check_floor_range(confidence_floor)
  q <- jev_choice_q(instructions %||%
    "Which single description best matches the state?", branches)
  ans <- jev_eval(state, list(q = q), model = model)$q
  v <- jvalue(ans)
  if (is.na(v)) return(fallback)
  if (tolower(v) %in% tolower(abstain_options)) return(fallback)
  if (confidence_floor > 0) {
    p <- jprob(ans)
    if (is.na(p) || p < confidence_floor) return(fallback)
  }
  v
}
