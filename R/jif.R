# Control-flow verbs: jif / else / jmatch.
#
# The design (deliberately explicit, no lazy magic):
#   jif(state, question, threshold = 0.5) returns a TRUE/FALSE (or option value),
#   with abstention handled via `abstain`. Then ordinary R does the rest:
#
#     if (jif(ticket, "customer is about to churn")) { ... } else { ... }
#
#   j_ifelse() is the vectorised data-frame workhorse: one API call per row,
#   all rows scored, then dplyr-free case/when semantics on the probabilities.

# S3 generic so `else` reads naturally after jif(); we do NOT hijack `if`.
jif <- function(state, question, ..., threshold = 0.5,
                abstain = NA, confidence_floor = 0,
                model = getOption("Rjif.model", "jev-latest")) {
  q <- .as_question(question, ...)
  ans <- jev_eval(state, list(q = q), model = model)$q

  # statistical honesty: if we were told what the floor is and the model is
  # below it, the decision is "unknown", not "whatever thresholding says".
  if (!is.na(confidence_floor) && confidence_floor > 0) {
    p <- jprob(ans)
    if (is.na(p) || p < confidence_floor) {
      return(structure(abstain, abstained = TRUE, answer = ans))
    }
  }
  out <- switch(q$type,
    noul   = !is.na(ans$value) && ans$value >= threshold,
    choice = ans$value,
    score  = !is.na(ans$value) && ans$value >= threshold,
    stop("Rjif: unknown question type", call. = FALSE))
  structure(out, abstained = FALSE, answer = ans)
}

# Build a question from shorthand: a plain string means a Noul assertion.
.as_question <- function(question, ...) {
  if (inherits(question, "jev_question")) return(question)
  if (is.character(question) && length(question) == 1L) {
    dots <- list(...)
    if (length(dots) && is.list(dots[[1]]) && !inherits(dots[[1]], "jev_question")) {
      return(jev_choice_q(question, dots[[1]]))
    }
    return(jev_noul_q(question))
  }
  stop("Rjif: question must be a string or a jev_noul_q/jev_choice_q/jev_score_q spec.",
       call. = FALSE)
}

# Was the last jif() an abstention?
jif_abstained <- function(x) isTRUE(attr(x, "abstained"))

# Vectorised: evaluate one judgment over every row of a data frame.
# state_from: character vector of per-row state strings (paste your columns).
# Returns a data.frame: decision, p (chosen probability), confidence, abstained.
jev_score_many <- function(state_vec, question, ...,
                           threshold = 0.5, confidence_floor = 0,
                           batch = 16L,
                           model = getOption("Rjif.model", "jev-latest")) {
  q <- .as_question(question, ...)
  n <- length(state_vec)
  dec <- logical(n); ps <- rep(NA_real_, n); cf <- rep(NA_real_, n)
  chosen <- rep(NA_character_, n)
  for (s in seq(1L, n, by = batch)) {
    e <- min(n, s + batch - 1L)
    for (i in s:e) {
      ans <- jev_eval(state_vec[[i]], list(q = q), model = model)$q
      v <- ans$value
      if (q$type == "noul") {
        dec[i] <- !is.na(v) && v >= threshold
        ps[i]  <- v
        chosen[i] <- as.character(dec[i])
      } else if (q$type == "choice") {
        dec[i] <- !is.na(v)
        ps[i]  <- jprob(ans)
        chosen[i] <- v
      } else {
        dec[i] <- !is.na(v) && v >= threshold
        ps[i]  <- ans$confidence %||% NA_real_
        chosen[i] <- as.character(v)
      }
      cf[i] <- ans$confidence %||% NA_real_
    }
  }
  abst <- !is.na(ps) & ps < confidence_floor
  data.frame(decision = dec, option = chosen, p = ps, confidence = cf,
             abstained = abst, stringsAsFactors = FALSE)
}

# ifelse-style verb with abstention lane:  j_ifelse(test, yes, no, unknown = ...)
j_ifelse <- function(test, yes, no, unknown = NA) {
  if (isTRUE(jif_abstained(test))) return(unknown)
  if (is.na(test)) return(unknown)
  if (isTRUE(test)) yes else no
}

# jmatch: route a state among description-matched branches.
# branches: named list of criteria descriptions (like jev_choice_q).
# Returns the winning branch name; routes to `fallback` when confidence
# under `confidence_floor` or the winner is the explicitly-semantic option
# named "none"/"other".
jmatch <- function(state, branches, ..., instructions = NULL,
                   confidence_floor = 0, fallback = NA_character_,
                   model = getOption("Rjif.model", "jev-latest")) {
  q <- jev_choice_q(instructions %||%
    "Which single description best matches the state?", branches)
  ans <- jev_eval(state, list(q = q), model = model)$q
  v <- ans$value
  p <- jprob(ans)
  if (is.na(v) || (!is.na(p) && p < confidence_floor)) return(fallback)
  v
}
