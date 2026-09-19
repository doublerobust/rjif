# Offline transport for tests, demos, and dry runs.
#
#   options(Rjif.transport = rjif_mock_transport)
#   options(Rjif.transport = rjif_mock_transport(c("my question" = 0.91)))
#
# It is NOT a Jev emulator and NOT a model. It fabricates answers so that the
# Rjif plumbing -- request construction, response parsing, the abstention lanes,
# the calibration analytics -- can be exercised with no API key and no network.
#
# Mock scores are deterministic test values, not meaningful probabilities: they
# do not measure relevance or safety, and must never be used to choose a
# threshold, to evaluate model quality, or to make any real-world decision.
# A reliability curve or ECE computed from mock output is an artefact of a hash,
# not a finding.
#
# Two modes:
#   1. Unscripted (the default, and the honest one): every noul score is a
#      deterministic hash of the question text, spread roughly uniformly over
#      (0, 1). Same input -> same output, across runs, seeds, and machines;
#      different questions -> different values, so multi-question calls and
#      batch loops are visibly exercised. Nothing here understands English,
#      negation, or clinical text -- deliberately, because a keyword heuristic
#      pretending to understand them was worse than no understanding at all:
#      it reused one token set for every row of a batch (so batch scoring could
#      not discriminate rows), scored the negation "no adverse events reported"
#      as the most safety-relevant row in the corpus, and landed obvious intents
#      at 0.48, i.e. exactly at the default threshold, so the demo's headline
#      decision was a coin flip while looking like a judgment.
#   2. Scripted: pass a named vector/list of instructions -> value and the mock
#      answers those questions with the numbers you chose, which is how a test
#      or demo asserts a *specific* decision, abstention, or calibration shape.
#      Unlisted questions fall back to the hash.
#
# Shape fidelity (this is what tests actually depend on):
#   * noul answers carry NO confidence field -- the real API does not send one;
#   * choice answers carry probabilities (summing to 1) + a confidence equal to
#     the selected option's probability;
#   * score answers carry a legend mapping level index ("0".. "k-1") to level
#     description, plus a confidence;
#   * a usage block with input_tokens is returned so cost accounting is wired.
#
# To test the API's failure shapes (missing 'answers', null values, wrong
# answer type), write your own one-line transport -- see tests/smoke.R, which
# injects each of those rather than relying on this mock to misbehave.
rjif_mock_transport <- function(body = NULL, answers = NULL) {
  # Two calling conventions. Used as a function *object* (the usual
  # options(Rjif.transport = rjif_mock_transport)) we get the request body and
  # are unscripted; called as rjif_mock_transport(script) we return a transport.
  if (is.null(body)) {
    if (is.numeric(answers) || is.list(answers)) return(.rjif_mock_with(answers))
    stop("Rjif mock: call rjif_mock_transport(script) with a named numeric ",
         "script, or pass it directly as options(Rjif.transport = ",
         "rjif_mock_transport).", call. = FALSE)
  }
  if (is.list(body) && !is.null(body$questions)) return(.rjif_mock_reply(body, NULL))
  # a bare named numeric passed as the first positional argument is the script
  if (is.numeric(body) || is.list(body)) return(.rjif_mock_with(body))
  stop("Rjif mock: transport called with something that is neither a request ",
       "body nor a named numeric script.", call. = FALSE)
}

# Build a scripted transport: rjif_mock_transport(c("q text" = 0.91)).
.rjif_mock_with <- function(script) {
  if (is.list(script)) script <- unlist(script)
  if (!is.numeric(script) || is.null(names(script)) || any(!nzchar(names(script)))) {
    stop("Rjif mock: the script must be a named numeric vector (question text -> ",
         "value in 0..1).", call. = FALSE)
  }
  script <- setNames(as.numeric(script), tolower(trimws(names(script))))
  if (any(script < 0 | script > 1, na.rm = TRUE)) {
    stop("Rjif mock: scripted values must be within [0, 1].", call. = FALSE)
  }
  function(body) .rjif_mock_reply(body, script)
}

.rjif_mock_reply <- function(body, script) {
  state_txt <- body$state %||% ""
  questions <- body$questions
  if (is.null(questions) || !length(questions)) {
    stop("Rjif mock: request body had no 'questions'.", call. = FALSE)
  }
  answers <- lapply(questions, function(q) {
    if (is.null(q$type)) stop("Rjif mock: question without a 'type' field.", call. = FALSE)
    key <- .mock_qkey(q)
    # script[[name]] errors ("subscript out of bounds") for unscripted
    # questions; those must fall through to the deterministic hash instead.
    v <- if (!is.null(script) && key %in% names(script)) script[[key]] else NULL
    .mock_answer(q, key, if (is.null(v)) NA_real_ else v)
  })
  tok_in <- suppressWarnings(nchar(state_txt, type = "bytes") %/% 4L)
  if (is.na(tok_in)) tok_in <- 0L
  list(model = body$model %||% "jev-mock", answers = answers,
       usage = list(input_tokens = tok_in, output_tokens = 0L))
}

# --- internals (not exported) -------------------------------------------------

.mock_qkey <- function(q) {
  inst <- q$instructions
  if (is.null(inst) || !length(inst) || is.na(inst[[1L]])) return("")
  tolower(trimws(as.character(inst[[1L]])))
}

# Deterministic pseudo-signal in (0, 1) from a 31-bit FNV-1a style hash of the
# text. XOR is done on 16-bit halves. NOTE (audit m3): the multiply step can
# transiently exceed 2^53 for long inputs (max observed ~3.2e16), so the double
# result is NOT guaranteed to be an exact integer before the modulo. That costs
# a tiny amount of low-bit entropy; since this feeds a *mock* whose numbers are
# explicitly meaningless, and the same input still always produces the same
# output on the same machine and R build, we accept it rather than add a
# bignum dependency. Do not use this hash anywhere correctness matters.
.mock_hash01 <- function(txt) {
  ch <- utf8ToInt(txt)
  h <- 2166136261
  if (length(ch)) for (c in ch) {
    h <- .mock_bitwXor(h, c)
    h <- (16777619 * h) %% 2147483647
  }
  0.001 + 0.998 * ((h %% 1000003) / 1000003)
}

.mock_bitwXor <- function(a, b) {
  lo <- bitwXor(as.integer(a %% 65536), as.integer(b %% 65536))
  hi <- bitwXor(as.integer((a %/% 65536) %% 32768),
                as.integer((b %/% 65536) %% 32768))
  lo + 65536 * hi
}

# Compose a per-question signal from the pieces the API actually receives.
.mock_signal <- function(...) .mock_hash01(paste0(..., collapse = "\r"))

.mock_answer <- function(q, key, scripted) {
  if (q$type == "noul") {
    v <- if (is.na(scripted)) .mock_signal(q$instructions) else scripted
    # no confidence field: the real API does not send one for noul answers
    return(list(type = "noul", noul = round(v, 4)))
  }
  if (q$type == "choice") {
    crit <- names(q$criteria)
    if (is.null(crit) || !length(crit)) {
      return(list(type = "choice", choice = NA_character_,
                  probabilities = list(), confidence = NA_real_))
    }
    raws <- vapply(crit, function(k)
      if (!is.na(scripted) && identical(tolower(trimws(k)), key)) scripted + 0.9
      else .mock_signal(k, q$criteria[[k]], q$instructions), numeric(1))
    names(raws) <- crit
    probs <- .mock_softmax(raws)
    # if the script named the whole question rather than one option, steer the
    # top-ranked option to the scripted value and renormalise the rest
    if (!is.na(scripted)) {
      top <- which.max(probs)[[1L]]
      rest <- setdiff(seq_along(probs), top)
      probs[[top]] <- scripted
      if (length(rest)) {
        s <- sum(probs[rest])
        probs[rest] <- if (s > 0) probs[rest] * (1 - scripted) / s else (1 - scripted) / length(rest)
      }
    }
    pick <- crit[[which.max(probs)[[1L]]]]
    # explicit return: without it this branch falls through to the final stop()
    return(list(type = "choice", choice = pick,
                probabilities = setNames(lapply(as.numeric(probs), round, 4), crit),
                confidence = round(probs[[pick]], 4)))
  }
  if (q$type == "score") {
    lv <- as.character(q$criteria)
    if (!length(lv)) {
      return(list(type = "score", score = NA_real_, legend = list(),
                  confidence = NA_real_))
    }
    raws <- vapply(seq_along(lv), function(i)
      .mock_signal(lv[[i]], i, q$instructions), numeric(1))
    w <- seq_along(lv) - 1L
    conf <- if (is.na(scripted)) round(min(1, max(raws)), 4) else scripted
    # The contract is an INTEGER level index into the legend (a fractional
    # "1.5th severity level" is not a thing), so the unscripted weighted mean
    # is rounded to the nearest level; a scripted value k selects level k
    # directly, clamped into range.
    sc <- if (is.na(scripted)) {
      round(sum(raws * w) / max(1e-9, sum(raws)))
    } else {
      round(min(max(0, round(scripted * (length(lv) - 1))), length(lv) - 1))
    }
    return(list(type = "score", score = as.double(sc),
                legend = setNames(as.list(lv), as.character(w)),
                confidence = conf))
  }
  stop("Rjif mock: unknown question type '", q$type, "'.", call. = FALSE)
}

.mock_softmax <- function(x) { x <- x - max(x); e <- exp(x); e / sum(e) }
