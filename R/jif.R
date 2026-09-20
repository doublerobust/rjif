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

jif <- function(state, question, ..., threshold = 0.5,
                abstain = NA, confidence_floor = 0,
                model = getOption("Rjif.model", "jev-latest")) {
  q <- .as_question(question, ...)
  threshold <- .single_number(threshold, "threshold")
  confidence_floor <- .single_number(confidence_floor, "confidence_floor",
                                     allow_na = TRUE)
  # Consistent NA-floor semantics across the package (audit m1): an NA floor
  # means "refuse to decide" -> abstain always, same as jev_score_many().
  if (is.na(confidence_floor)) confidence_floor <- Inf
  .check_floor_range(confidence_floor)
  ans <- jev_eval(state, list(q = q), model = model)$q
  v <- jvalue(ans)

  # Missingness is never evidence of "false". A null/absent value abstains even
  # when confidence_floor is 0 -- there is simply nothing to threshold.
  if (is.na(v)) {
    return(structure(abstain, abstained = TRUE, answer = ans,
                     abstain_reason = "no answer value from the API"))
  }

  # Below the floor the decision is "unknown", not "whatever thresholding
  # happens to say". An NA probability with a floor set is also unknown.
  if (confidence_floor > 0) {
    p <- jprob(ans)
    if (is.na(p)) {
      return(structure(abstain, abstained = TRUE, answer = ans,
                       abstain_reason = "confidence unavailable with a floor set"))
    }
    if (p < confidence_floor) {
      return(structure(abstain, abstained = TRUE, answer = ans,
                       abstain_reason = paste0("probability ", formatC(p, format = "f",
                                                                       digits = 3),
                                               " < confidence_floor ",
                                               formatC(confidence_floor, format = "f",
                                                       digits = 3))))
    }
  }

  out <- switch(q$type,
    # noul: probability the assertion holds -> compare against the threshold
    noul   = v >= threshold,
    # choice: the selected option name (a string, not TRUE/FALSE)
    choice = v,
    # score: CONTINUOUS probability-weighted level position (0 = lowest level;
    # live contract: can land between levels). NOTE: for score questions
    # `threshold` is a LEVEL, not a probability -- with 5 levels, threshold = 2
    # means "the weighted position clears moderate or worse", not a hard class.
    score  = v >= threshold,
    # only reachable with a hand-forged question object, but scrub anyway
    # (R9-B1 precedent): a key in q$type must not leave via this message.
    stop(.clean_error_text(paste0("Rjif: unknown question type '",
                                  as.character(q$type)[[1L]], "'.")),
         call. = FALSE))

  structure(out, abstained = FALSE, answer = ans, abstain_reason = NA_character_)
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
#              confidence_floor, p unavailable while a floor was set, an invalid
#              API answer, or an NA state / transport failure for that row.
#   error      per-row failure text ("" when the row succeeded), scrubbed of any
#              control characters AND of the caller's API key before it is
#              persisted, so one bad narrative does not discard the other 4,999
#              and a printed data frame can never leak the credential.
#
# 'batch' is the number of rows processed per progress chunk. The API takes ONE
# state per call, so this loop is one HTTP request per row either way; batch
# does NOT amortise cost or latency, it only bounds how many rows are attempted
# before an unexpected transport error can surface.
jev_score_many <- function(state_vec, question, ...,
                           threshold = 0.5, confidence_floor = 0,
                           batch = 16L,
                           model = getOption("Rjif.model", "jev-latest")) {
  q <- .as_question(question, ...)
  threshold <- .single_number(threshold, "threshold")
  confidence_floor <- .single_number(confidence_floor, "confidence_floor",
                                     allow_na = TRUE)
  if (is.na(confidence_floor)) confidence_floor <- Inf  # NA floor = abstain always
  .check_floor_range(confidence_floor)
  if (!is.character(state_vec) && !is.factor(state_vec)) {
    stop("Rjif: state_vec must be a character (or factor) vector.", call. = FALSE)
  }
  state_vec <- as.character(state_vec)
  n <- length(state_vec)
  batch <- suppressWarnings(as.integer(batch))
  if (length(batch) != 1L || is.na(batch) || batch < 1L) batch <- 16L

  dec <- rep(NA, n); ps <- rep(NA_real_, n); cf <- rep(NA_real_, n)
  chosen <- rep(NA_character_, n); abst <- rep(NA, n); errs <- rep("", n)

  i <- 1L
  while (i <= n) {
    e <- min(n, i + batch - 1L)
    for (j in i:e) {
      st <- state_vec[[j]]
      if (is.na(st)) {
        dec[j] <- NA; abst[j] <- TRUE; errs[j] <- "state is NA"
        next
      }
      # jev_eval can warn (contract violation) as well as throw; capture both.
      warns <- character(0)
      ans <- withCallingHandlers(
        tryCatch(jev_eval(st, list(q = q), model = model)$q,
                 error = function(err) structure(list(msg = conditionMessage(err)),
                                                 class = "jev_row_error")),
        warning = function(w) {
          warns <<- c(warns, conditionMessage(w))
          invokeRestart("muffleWarning")
        })
      if (inherits(ans, "jev_row_error")) {
        dec[j] <- NA; abst[j] <- TRUE
        errs[j] <- .clean_error_text(ans$msg, 300L)
        next
      }
      v <- jvalue(ans)
      p <- jprob(ans)
      ps[j] <- p
      cf[j] <- jconf(ans)
      # undecided: no value at all (missing answer or contract violation),
      # under the floor, or the floor was set but no probability exists to
      # compare it against. Undecided means decision = NA, never FALSE (B2).
      undecided <- is.na(v) ||
        (confidence_floor > 0 && (is.na(p) || p < confidence_floor))
      if (undecided) {
        abst[j] <- TRUE
        dec[j] <- NA
        chosen[j] <- NA_character_
      } else {
        abst[j] <- FALSE
        if (q$type == "choice") {
          chosen[j] <- v
          dec[j] <- TRUE   # a decided choice: 'option' carries the routing
        } else if (q$type == "noul") {
          dec[j] <- (v >= threshold)
          chosen[j] <- if (dec[[j]]) "true" else "false"
        } else {
          dec[j] <- (v >= threshold)
          chosen[j] <- formatC(v, format = "f", digits = 3)
        }
      }
      # A contract-violating answer arrives here with value NA (above, it
      # abstains) and an explanatory `contract` field; surface it in the error
      # column. .clean_error_text redacts the real API key before persisting.
      note <- ans[["contract"]]
      if (!is.null(note) && !is.na(note) && nzchar(note)) {
        errs[j] <- .clean_error_text(
          paste(c(paste0("invalid API answer: ", note), warns), collapse = "; "),
          300L)
      } else if (length(warns)) {
        errs[j] <- .clean_error_text(paste(warns, collapse = "; "), 300L)
      }
    }
    i <- e + 1L
  }
  out <- data.frame(decision = dec, option = chosen, p = ps, confidence = cf,
                    abstained = abst, error = errs, stringsAsFactors = FALSE)
  attr(out, "question_type") <- q$type
  attr(out, "n_abstained") <- sum(abst)
  out
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
    # literal named arms: route by expression tags, evaluating ONLY the winner
    if (!is.null(yt) && test %in% yt) return(.arm_elt(yx, test, env))
    if (!is.null(nt) && test %in% nt) return(.arm_elt(nx, test, env))
    if (!is.null(yt) || !is.null(nt)) return(unknown)  # literal branches, no match
    # no literal named arms: fall back to value-based routing (the old path).
    if (missing(yes)) stop("Rjif: j_ifelse(): 'yes' is missing.", call. = FALSE)
    yv <- eval(yx, env)
    yn <- names(yv)
    if (!is.null(yn) && test %in% yn) return(yv[[test]])
    nv <- if (missing(no)) NULL else eval(nx, env)
    nn <- names(nv)
    if (!is.null(nn) && test %in% nn) return(nv[[test]])
    if (!is.null(yn) || !is.null(nn)) return(unknown)  # named arms, no match
    return(yv)  # plain scalars: an option name is a truthy pick -> yes
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
