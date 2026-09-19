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

# One line, no control characters, hard-capped length.
.clean_error_text <- function(txt, limit = JEV_ERROR_BODY_LIMIT) {
  txt <- .scrub_secrets(txt)
  txt <- gsub("[\r\n\t\001-\037]", " ", txt, perl = TRUE)
  txt <- gsub("[[:space:]]{2,}", " ", txt)
  if (nchar(txt, type = "bytes") > limit) txt <- paste0(substr(txt, 1L, limit), "...")
  trimws(txt)
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
    stop("Rjif: choice criteria names must be unique; duplicated: ",
         paste(unique(nms[duplicated(nms)]), collapse = ", "), call. = FALSE)
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
  state <- .as_scalar(state, "character")
  if (is.na(state)) {
    stop("Rjif: state must be a single non-NA string.", call. = FALSE)
  }
  if (!is.list(questions) || is.null(names(questions)) || !length(questions)) {
    stop("Rjif: questions must be a non-empty named list of jev_*_q() specs.", call. = FALSE)
  }
  if (any(!nzchar(names(questions)))) {
    stop("Rjif: every question in 'questions' must be named.", call. = FALSE)
  }
  if (anyDuplicated(names(questions))) {
    stop("Rjif: question names must be unique; duplicated: ",
         paste(unique(names(questions)[duplicated(names(questions))]),
               collapse = ", "), call. = FALSE)
  }
  ok <- vapply(questions, inherits, logical(1), "jev_question")
  if (!all(ok)) stop("Rjif: build questions with jev_noul_q/jev_choice_q/jev_score_q.",
                     call. = FALSE)

  body <- list(state = state, model = model,
               questions = lapply(questions, function(q) unclass(q)))

  transport <- getOption("Rjif.transport")
  raw <- if (is.function(transport)) transport(body) else .transport_httr(body)
  if (!is.list(raw)) {
    stop("Rjif: transport response was not a list (got ",
         class(raw)[[1L]], ").", call. = FALSE)
  }
  if (is.null(raw$answers)) {
    stop("Rjif: response had no 'answers' field. Raw response fields: ",
         .clean_error_text(paste(names(raw), collapse = ", "), 120L), ".",
         call. = FALSE)
  }
  if (!is.list(raw$answers)) {
    stop("Rjif: response 'answers' was not an object.", call. = FALSE)
  }

  # Token accounting. usage is optional (mock transports omit it); 'calls'
  # counts every attempted call so cost reporting can never under-count.
  billed <- 0L
  # usage is optional AND may be malformed ("oops", a vector, a bare string):
  # only a list can carry input_tokens, and a malformed usage block must not
  # abort a call whose answer we already have.
  if (is.list(raw$usage)) {
    it <- suppressWarnings(as.double(raw$usage$input_tokens))
    if (length(it) == 1L && !is.na(it) && is.finite(it) && it >= 0) billed <- as.integer(it)
  }
  .RjifEnv$usage$calls <- .RjifEnv$usage$calls + 1L
  .RjifEnv$usage$input_tokens <- .RjifEnv$usage$input_tokens + billed

  ans <- lapply(names(questions), function(nm) {
    a <- raw$answers[[nm]]
    if (is.null(a)) stop("Rjif: missing answer for question '", nm, "'.", call. = FALSE)
    qtype <- questions[[nm]]$type
    value <- switch(qtype,
      noul   = .as_scalar(a$noul, "double"),
      choice = .as_scalar(a$choice, "character"),
      score  = .as_scalar(a$score, "double"),
      stop("Rjif: unknown question type '", qtype, "' for question '", nm, "'.",
           call. = FALSE))
    # A scored question whose answer carries a different 'type' than requested
    # means we would label the number with the wrong scale: refuse the answer.
    if (!is.null(a$type) && !identical(as.character(a$type)[[1L]], qtype)) {
      warning("Rjif: question '", nm, "' was sent as type '", qtype,
              "' but the API answered type '", as.character(a$type)[[1L]],
              "'; answer discarded as NA.", call. = FALSE)
      value <- NA
    }
    probs <- NULL
    if (!is.null(a$probabilities)) {
      if (is.list(a$probabilities)) {
        probs <- lapply(a$probabilities, function(p) suppressWarnings(as.double(p[[1L]])))
      } else {
        pn <- if (!is.null(names(a$probabilities))) names(a$probabilities) else
          if (!is.null(questions[[nm]]$criteria)) names(questions[[nm]]$criteria) else
            as.character(seq_along(a$probabilities))
        probs <- setNames(as.list(suppressWarnings(as.double(a$probabilities))), pn)
      }
    }
    conf <- suppressWarnings(as.double(a$confidence))
    if (length(conf) != 1L || is.na(conf)) conf <- NA_real_
    # NOTE: the API sends no confidence for a noul answer, and we do not invent
    # one. The noul score IS the probability the assertion holds, and jprob()
    # returns exactly that for noul answers; mirroring it into `confidence`
    # would make jconf() report a number the API never sent. Callers that need
    # a threshold on a noul answer use prob (see jif(confidence_floor=)).
    structure(list(
      question   = nm,
      type       = qtype,
      value      = if (qtype == "choice") .as_scalar(value, "character") else
                                              suppressWarnings(as.double(value)),
      probs      = probs,
      confidence = conf,
      legend     = a$legend,
      raw        = a
    ), class = c(paste0("jev_", qtype), "jev_answer"))
  })
  names(ans) <- names(questions)
  structure(ans, model = .as_scalar(raw$model, "character") %||% model,
            class = "jev_answers")
}

# Null/empty/NA coalesce. base R only ships `%||%` from 4.4, and rlang's is not
# a dependency we want, so this mirrors base exactly: a NULL or length-0 left
# side falls back; a *single* atomic NA falls back; an atomic NA *vector* such
# as c(NA, 0.3) is returned untouched rather than raising R >= 4.2's
# "length = 2 in coercion to logical(1)".
`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0L) return(b)
  if (length(a) == 1L && is.na(a)) return(b)
  a
}

# Accessors --------------------------------------------------------------------

# The answer's value: noul/score -> numeric 0-1 / level index; choice -> option name.
jvalue <- function(ans) ans$value

# Reported confidence. For a noul answer the API sends no separate confidence
# field, so this mirrors the noul score itself (see jprob()). It is NA when the
# API sent no usable value at all.
jconf <- function(ans) `%||%`(ans$confidence, NA_real_)

# Probability that the picked answer is right: for noul the noul score, for
# choice the probability of the chosen option, for score the API confidence (a
# score answer has no per-level distribution, only a legend).
jprob <- function(ans) {
  if (is.null(ans$type)) return(NA_real_)
  out <- switch(ans$type,
    noul   = suppressWarnings(as.double(jvalue(ans))),
    choice = {
      v <- jvalue(ans)
      if (is.na(v) || is.null(ans$probs)) NA_real_
      else suppressWarnings(as.double(ans$probs[[v]]))
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
