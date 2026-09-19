# Low-level TypeSafe Jev client: one 'system one' call = one state, many questions.
# Transport is pluggable (option Rjif.transport) so tests and demos run offline.

.RjifEnv <- new.env(parent = emptyenv())
.RjifEnv$usage <- list(calls = 0L, input_tokens = 0L)

JEV_PRICE_PER_MTOK <- 0.042   # $/million input tokens; output tokens are free

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
  if (httr::http_status(resp)$category != "Success") {
    stop("Rjif: API call failed (", httr::status_code(resp), "): ",
         substr(rawToChar(resp$content), 1, 300), call. = FALSE)
  }
  jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = FALSE)
}

# Question constructors -------------------------------------------------------

# noul: "is this statement true of the state?" -> 0-1
jev_noul_q <- function(instructions) {
  structure(list(type = "noul", instructions = instructions), class = "jev_question")
}

# choice: route among named, described options -> one option + full distribution
jev_choice_q <- function(instructions, criteria) {
  if (is.null(names(criteria)) || any(!nzchar(names(criteria)))) {
    stop("Rjif: choice criteria must be a named list (option = description).", call. = FALSE)
  }
  structure(list(type = "choice", instructions = instructions,
                 criteria = as.list(criteria)), class = "jev_question")
}

# score: rate the state on an ordered rubric (vector of level descriptions)
jev_score_q <- function(instructions, criteria) {
  structure(list(type = "score", instructions = instructions,
                 criteria = as.character(criteria)), class = "jev_question")
}

# Core call --------------------------------------------------------------------
# jev_eval(state = <character>, questions = list(a = jev_noul_q(...), ...))
# returns a 'jev_answers' list, one entry per question, in the same order.
jev_eval <- function(state, questions, model = getOption("Rjif.model", "jev-latest")) {
  stopifnot(is.character(state), length(state) == 1L)
  if (!is.list(questions) || is.null(names(questions)) || !length(questions)) {
    stop("Rjif: questions must be a non-empty named list of jev_*_q() specs.", call. = FALSE)
  }
  ok <- vapply(questions, inherits, logical(1), "jev_question")
  if (!all(ok)) stop("Rjif: build questions with jev_noul_q/jev_choice_q/jev_score_q.", call. = FALSE)

  body <- list(state = state, model = model,
               questions = lapply(questions, function(q) unclass(q)))

  transport <- getOption("Rjif.transport")
  raw <- if (is.function(transport)) transport(body) else .transport_httr(body)
  if (is.null(raw$answers)) stop("Rjif: response had no 'answers' field.", call. = FALSE)

  # account tokens (mock transport may omit usage)
  if (!is.null(raw$usage$input_tokens)) {
    .RjifEnv$usage$calls <- .RjifEnv$usage$calls + 1L
    .RjifEnv$usage$input_tokens <- .RjifEnv$usage$input_tokens +
      as.integer(raw$usage$input_tokens)
  }

  ans <- lapply(names(questions), function(nm) {
    a <- raw$answers[[nm]]
    if (is.null(a)) stop("Rjif: missing answer for question '", nm, "'.", call. = FALSE)
    qtype <- questions[[nm]]$type
    value <- switch(qtype,
      noul   = a$noul %||% NA_real_,
      choice = a$choice %||% NA_character_,
      score  = a$score %||% NA_real_)
    structure(list(
      question  = nm,
      type      = qtype,
      value     = value,
      probs     = a$probabilities,
      confidence = a$confidence,
      legend    = a$legend,
      raw       = a
    ), class = c(paste0("jev_", qtype), "jev_answer"))
  })
  names(ans) <- names(questions)
  structure(ans, model = raw$model %||% model, class = "jev_answers")
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L ||
                             (is.atomic(a) && is.na(a))) b else a

# Accessors --------------------------------------------------------------------

jvalue <- function(ans) ans$value
jconf  <- function(ans) ans$confidence %||% NA_real_

# probability the model assigns to whatever it picked (for noul, the noul itself)
jprob <- function(ans) {
  switch(ans$type,
    noul   = ans$value,
    choice = if (!is.na(ans$value) && !is.null(ans$probs)) ans$probs[[ans$value]] else NA_real_,
    score  = ans$confidence %||% NA_real_,
    NA_real_)
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

print.jev_answers <- function(x, ...) {
  for (nm in names(x)) {
    a <- x[[nm]]
    cat(sprintf("[%s] %s: %s", a$type, nm,
        format(a$value, digits = 4)))
    if (!is.null(a$probs)) {
      pr <- vapply(a$probs, function(p) sprintf("%.3f", p), character(1))
      cat("  (", paste0(names(pr), "=", pr, collapse = ", "), ")", sep = "")
    }
    if (!is.null(a$confidence)) cat("  conf=", sprintf("%.3f", a$confidence), sep = "")
    cat("\n")
  }
  invisible(x)
}
