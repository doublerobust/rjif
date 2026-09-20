# Offline round-15 regressions. Installed package only, including R CMD check.
library(Rjif)
cat("INSTALLED", find.package("Rjif"), as.character(packageVersion("Rjif")), "\n")
.pick <- function(n) get(n, asNamespace("Rjif"))
pass <- fail <- 0L
check <- function(label, expr) {
  ok <- tryCatch(isTRUE(force(expr)), error = function(e) {
    cat("  ERROR", conditionMessage(e), "\n"); FALSE
  })
  if (ok) { pass <<- pass + 1L; cat("PASS", label, "\n") }
  else { fail <<- fail + 1L; cat("FAIL", label, "\n") }
}
err <- function(expr) tryCatch({ force(expr); "" }, error = conditionMessage)
noul <- function(p) function(b) list(answers = list(q = list(noul = p)))
options(Rjif.transport = function(b) stop("Unexpected call in offline fixture"))

check("mixed map cross-product keeps YES precedence and first duplicate", {
  y <- list(a = "Y"); n <- list(a = "N")
  identical(j_ifelse("a", list(a = "Y"), n), "Y") &&
    identical(j_ifelse("a", y, list(a = "N")), "Y") &&
    identical(j_ifelse("a", y, n), "Y") &&
    identical(j_ifelse("a", list(a = "Y", a = stop("duplicate forced")),
                       list(a = stop("NO forced"))), "Y") &&
    identical(j_ifelse("a", c(a = "Y", a = stop("duplicate forced")),
                       c(a = stop("NO forced"))), "Y")
})
check("both literal types preserve winning siblings and losing no laziness", {
  identical(j_ifelse("b", list(a = stop("yes forced")),
                     c(b = "B", c = stop("sibling forced"))), "B") &&
    identical(j_ifelse("z", c(a = stop("yes forced")), list(b = stop("no forced")), "U"), "U")
})
check("state digest is order edit and NA sensitive", {
  digest <- .pick(".state_digest")
  v <- list(c("a", "b"), c("b", "a"), c("a", "c"), c("a", NA_character_), c("a", ""))
  length(unique(vapply(v, function(x) digest(x)$digest, ""))) == length(v)
})
check("state digest boundaries and late edits cannot alias", {
  digest <- .pick(".state_digest")
  v <- list(c("a", "b"), "ab", c("", "ab"), c("ab", ""),
            c("a1", "2b"), c("a", "12b"), character(), NA_character_, "",
            paste0(strrep("x", 5000), "A"), paste0(strrep("x", 5000), "B"))
  length(unique(vapply(v, function(x) digest(x)$digest, ""))) == length(v)
})
check("embedded NUL is not a valid R string", {
  nzchar(err(rawToChar(as.raw(c(97, 0, 98))))) &&
    nzchar(err(parse(text = paste0('"a', '\\', '0b"'))))
})
check("empty state input remains a zero-call empty frame", {
  calls <- 0L
  withr_options(Rjif.transport = function(b) { calls <<- calls + 1L; noul(.9)(b) }, {
    d <- jev_score_many(character(), "q")
    is.data.frame(d) && nrow(d) == 0L && calls == 0L && attr(d, "n_abstained") == 0
  })
})

check("cache refusals preserve exact bytes and make zero calls", local({
  path <- tempfile(); on.exit(unlink(path)); calls <- 0L
  tr <- function(b) { calls <<- calls + 1L; noul(if (b$state == "present") .99 else .01)(b) }
  withr_options(Rjif.transport = tr, {
    args <- list(state_vec = c("present", "absent"), question = "q", cache = path)
    do.call(jev_score_many, args); before <- calls; original <- readRDS(path)
    changes <- list(list(state_vec = c("absent", "present")),
                    list(state_vec = c("presenT", "absent")),
                    list(question = "other"), list(model = "other"),
                    list(threshold = .8), list(confidence_floor = .8),
                    list(state_vec = "present"))
    good <- all(vapply(changes, function(change) {
      a <- args; a[names(change)] <- change; md5 <- tools::md5sum(path)
      msg <- err(do.call(jev_score_many, a))
      grepl("does not match this run", msg, fixed = TRUE) && calls == before &&
        identical(md5, tools::md5sum(path))
    }, logical(1)))
    fp <- unserialize(attr(original, "cache_fingerprint"))
    stopifnot(fp$version == 4L)   # v4: UTF-8-canonicalised state digest (round 3, R3-1)
    fp$version <- 2L; fp$states <- NULL
    old <- original; attr(old, "cache_fingerprint") <- serialize(fp, NULL, version = 2)
    for (bad in list(old, NULL, list(not = "a cache"), data.frame(x = 1))) {
      saveRDS(bad, path); md5 <- tools::md5sum(path)
      msg <- err(do.call(jev_score_many, args))
      good <- good && grepl("does not match this run", msg, fixed = TRUE) &&
        calls == before && identical(md5, tools::md5sum(path))
    }
    writeBin(charToRaw("not an RDS"), path); md5 <- tools::md5sum(path)
    msg <- err(do.call(jev_score_many, args))
    good && grepl("could not be read as an RDS", msg, fixed = TRUE) &&
      grepl("Nothing was written to it", msg, fixed = TRUE) &&
      calls == before && identical(md5, tools::md5sum(path))
  })
}))
check("successful cache stores digest not plaintext state fields", local({
  path <- tempfile(); on.exit(unlink(path))
  states <- c("private-state-ALPHA-394", "private-state-BETA-783")
  withr_options(Rjif.transport = noul(.9), jev_score_many(states, "q", cache = path))
  df <- readRDS(path); fp <- unserialize(attr(df, "cache_fingerprint"))
  # Recursively inspect character values, names, attributes, and the decoded fingerprint.
  strings <- function(x) {
    c(if (is.character(x)) x else if (is.list(x)) unlist(lapply(x, strings)),
      if (!is.null(attributes(x))) unlist(lapply(attributes(x), strings)))
  }
  chars <- c(strings(df), strings(fp))
  !any(vapply(states, function(s) any(grepl(s, chars, fixed = TRUE)), logical(1))) &&
    identical(fp$states, .pick(".state_digest")(states))
}))

check("window predicate is literally shared by evaluator and curve", {
  all(vapply(c(".decide_answer", "selection_curve"), function(n)
    ".noul_window_decided" %in% all.names(body(.pick(n))), logical(1)))
})
check("two-sided follows evaluator at floors below and at threshold", {
  d <- structure(data.frame(p = c(.01, .2, .4, .5, .8, .99), truth = c(0, 0, 0, 1, 1, 1)), question_type = "noul")
  all(vapply(c(0, .2, .4, .5, .7, .8, 1), function(f) {
    actual <- vapply(d$p, function(p) withr_options(Rjif.transport = noul(p),
      !jif_abstained(jif("s", "q", confidence_floor = f))), logical(1))
    selection_curve(d, floor_seq = f, policy = "two_sided")$coverage == mean(actual)
  }, logical(1)))
})
check("two-sided supports explicit nondefault thresholds and outside floors", {
  d <- structure(data.frame(p = c(0, .1, .2, .3, .4, .5, .6, .8, 1), truth = rep(0, 9)), question_type = "noul")
  all(vapply(c(.3, .5, .9), function(t) all(vapply(c(-1, 0, .3, .4, .5, .8, 1, 1.01), function(f) {
    actual <- suppressWarnings(vapply(d$p, function(p) withr_options(Rjif.transport = noul(p),
      !jif_abstained(jif("s", "q", threshold = t, confidence_floor = f))), logical(1)))
    suppressWarnings(selection_curve(d, floor_seq = f, policy = "two_sided", threshold = t))$coverage == mean(actual)
  }, logical(1))), logical(1)))
})
check("positive view unchanged and policy attribute carried", {
  d <- structure(data.frame(p = c(.01, .99), truth = c(0, 1)), question_type = "noul")
  a <- selection_curve(d, floor_seq = .7); b <- selection_curve(d, floor_seq = .7, policy = "two_sided")
  a$coverage == .5 && b$coverage == 1 && identical(attr(a, "policy"), "positive") &&
    identical(attr(b, "policy"), "two_sided")
})
check("two-sided refuses non-noul even with allow_type", {
  all(vapply(c("score", "choice", ""), function(type) {
    d <- data.frame(p = .9, truth = 1); if (nzchar(type)) attr(d, "question_type") <- type
    grepl("requires a noul batch", err(selection_curve(d, policy = "two_sided", allow_type = type)), fixed = TRUE)
  }, logical(1)))
})
check("inclusive negative epsilon and adjacent positive boundaries match batch", {
  all(vapply(c(.8, .9), function(f) {
    neg <- if (f == .8) .2 else .1
    ps <- c(neg - 2e-9, neg, neg + .5e-9, neg + 2e-9, f - 2e-9, f, f + 2e-9)
    expected <- c(FALSE, FALSE, FALSE, NA, NA, TRUE, TRUE)
    a <- vapply(ps, function(p) withr_options(Rjif.transport = noul(p),
      as.logical(jif("s", "q", confidence_floor = f))), logical(1))
    b <- vapply(ps, function(p) withr_options(Rjif.transport = noul(p),
      jev_score_many("s", "q", confidence_floor = f)$decision), logical(1))
    identical(a, expected) && identical(b, expected)
  }, logical(1)))
})

valid_choice <- function(p) {
  names(p) <- letters[seq_along(p)]
  .pick("jev_answer_valid")(list(choice = "a", confidence = 1, probabilities = as.list(p)),
                            jev_choice_q("pick", setNames(names(p), names(p))))
}
check("winner strict gap ties and three-way maximum", {
  !valid_choice(c(.49, .51))$valid && valid_choice(c(.5, .5))$valid &&
    valid_choice(c(.51, .49))$valid && valid_choice(c(.4, .3, .3))$valid &&
    grepl("trails the top option by 0.020 (> tolerance 0.00)", valid_choice(c(.49, .51))$reason, fixed = TRUE)
})
check("normalization preserves ties and ordering at sum-tolerance edges", {
  all(vapply(c(.99, 1, 1.01), function(s) {
    tie <- valid_choice(c(.5, .5) * s); win <- valid_choice(c(.51, .49) * s)
    lose <- valid_choice(c(.49, .51) * s)
    tie$valid && identical(unname(unlist(tie$probs)), c(.5, .5)) &&
      win$valid && win$probs$a > win$probs$b && !lose$valid
  }, logical(1)))
})
check("winner epsilon compares displayed values BEFORE normalization", {
  p <- c(.495, .495 + .999e-9)
  a <- valid_choice(p); b <- valid_choice(c(.5, .5 + 1.01e-9))
  a$valid && (a$probs$b - a$probs$a) > 1e-9 && !b$valid
})
options(Rjif.transport = NULL)
cat(sprintf("R15 REGRESSIONS: %d passed, %d failed\n", pass, fail))
if (fail) quit(save = "no", status = 1L)
