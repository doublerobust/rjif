# Offline implementation gate. RJIF_SOURCE_TREE bypasses every installed copy.
root <- Sys.getenv("RJIF_SOURCE_TREE")
if (nzchar(root)) {
  for (f in c("client.R", "jif.R", "calibration.R", "mock.R"))
    source(file.path(root, "R", f))
  .pick <- function(n) get(n, envir = globalenv())
} else {
  library(Rjif)
  .pick <- function(n) get(n, envir = asNamespace("Rjif"))
}
fail <- pass <- 0L
check <- function(label, expr) {
  ok <- tryCatch(isTRUE(force(expr)), error = function(e) {
    cat("  ERROR:", conditionMessage(e), "\n"); FALSE
  })
  if (ok) { pass <<- pass + 1L; cat("PASS", label, "\n") }
  else { fail <<- fail + 1L; cat("FAIL", label, "\n") }
}
error_text <- function(expr) tryCatch({ force(expr); "" }, error = conditionMessage)
warned <- function(expr) {
  w <- character()
  value <- withCallingHandlers(force(expr), warning = function(e) {
    w <<- c(w, conditionMessage(e)); invokeRestart("muffleWarning")
  })
  list(value = value, warnings = w)
}
noul <- function(p) function(body) list(answers = list(q = list(noul = p)),
                                       usage = list(input_tokens = 7))
qc <- jev_choice_q("pick", c(a = "A", b = "B"))
qs <- jev_score_q("score", c("low", "high"))
forge <- function(a) function(body) list(answers = list(q = a))
for (type in c("score", "choice")) for (fn in c("reliability_curve", "ece", "selection_curve")) {
  d <- structure(data.frame(p = rep(0.8, 30), truth = rep(1, 30)), question_type = type)
  check(paste("F2 default gate", type, fn), nzchar(error_text(get(fn)(d))))
  check(paste("F2 warned opt-out", type, fn),
        length(warned(get(fn)(d, allow_type = type))$warnings) == 1L)
}
check("F2 observed_rate exact and dollar lookup", {
  r <- reliability_curve(data.frame(p = c(0, 1), truth = c(0, 1)), n_bins = 2)
  identical(r$observed_rate, c(0, 1)) && is.null(r$accuracy) && is.null(r[["accuracy"]])
})
check("F3 window boundaries and single-sided decisions", {
  ps <- c(0, 0.3, 0.5, 0.7, 1)
  a <- vapply(ps, function(p) withr_options(Rjif.transport = noul(p),
    as.logical(jif("s", "q", confidence_floor = 0.7))), logical(1))
  b <- withr_options(Rjif.transport = noul(0.4),
    jif("s", "q", threshold = 0.5, confidence_floor = 0.4))
  identical(a, c(FALSE, FALSE, NA, TRUE, TRUE)) && isFALSE(b)
})
check("F3 NA floor has identical finite-free reasons and honest usage", {
  jev_usage_reset()
  a <- withr_options(Rjif.transport = noul(0.9), jif("s", "q", confidence_floor = NA))
  b <- withr_options(Rjif.transport = noul(0.9), jev_score_many("s", "q", confidence_floor = NA))
  is.na(a) && is.na(b$decision) && identical(jif_reason(a), b$error) &&
    identical(jif_reason(a), "confidence_floor is NA; refusing to decide") &&
    !grepl("Inf", paste(capture.output(jev_usage()), jif_reason(a), collapse = " ")) &&
    jev_usage()$calls == 2 && jev_usage()$input_tokens == 14
})
check("F3 valid choice and score floors remain single-sided", {
  a <- withr_options(Rjif.transport = forge(list(choice = "a", confidence = 1,
    probabilities = list(a = 0.6, b = 0.4))), jif("s", qc, confidence_floor = 0.7))
  b <- withr_options(Rjif.transport = forge(list(score = 0, confidence = 0.2,
    probabilities = list("0" = 1, "1" = 0), legend = list("0" = "low", "1" = "high"))),
    jif("s", qs, confidence_floor = 0.7))
  jif_abstained(a) && jif_abstained(b)
})
check("F4 contradictory choice cannot route through jif", {
  a <- suppressWarnings(withr_options(Rjif.transport = forge(list(choice = "a", confidence = 1,
    probabilities = list(a = 0.01, b = 0.99))), jif("s", qc)))
  jif_abstained(a) && is.na(a) && is.na(jprob(attr(a, "answer")))
})
check("F4 winner tolerance exact boundary and just outside", {
  decide <- function(p) suppressWarnings(withr_options(Rjif.transport = forge(list(
    choice = "a", confidence = 1, probabilities = list(a = p, b = 1-p))), jif("s", qc)))
  !jif_abstained(decide(0.49)) && jif_abstained(decide(0.48999))
})
check("F4 bare score cannot decide through jif", {
  a <- suppressWarnings(withr_options(Rjif.transport = forge(list(score = 1, confidence = 1)),
    jif("s", qs)))
  jif_abstained(a) && is.na(a) && is.na(jprob(attr(a, "answer")))
})
check("F4 legend and weighted-mean validation", {
  base <- list(score = 0.5, confidence = 1, probabilities = list("1" = 0.5, "0" = 0.5),
               legend = list("1" = "high", "0" = "low"))
  result <- function(a) suppressWarnings(withr_options(Rjif.transport = forge(a), jif("s", qs)))
  bad_mean <- base; bad_mean$score <- 0.9
  bad_keys <- base; names(bad_keys$legend) <- c("1", "2")
  !jif_abstained(result(base)) && jif_abstained(result(bad_mean)) && jif_abstained(result(bad_keys))
})
check("F4 request bounds", {
  all(vapply(c(1, 11), function(n) nzchar(error_text(jev_score_q("s", rep("x", n)))), logical(1))) &&
    all(vapply(c(1, 256), function(n) nzchar(error_text(jev_choice_q("s", setNames(rep("x", n), seq_len(n))))), logical(1))) &&
    length(jev_score_q("s", rep("x", 10))$criteria) == 10 &&
    length(jev_choice_q("s", setNames(rep("x", 255), seq_len(255)))$criteria) == 255
})
check("F4 structured states and noul criteria survive shorthand on wire", {
  sent <- NULL
  tr <- function(b) { sent <<- b; noul(0.8)(b) }
  state <- list(patient = list(age = 67), events = list("fever"))
  criteria <- list(true = "present", false = "absent")
  a <- withr_options(Rjif.transport = tr, jif(state, "q", criteria))
  isTRUE(a) && identical(sent$state, state) && identical(sent$questions$q$criteria, criteria) &&
    identical(sent$questions$q$type, "noul")
})
check("F4 named character noul criteria serialize as an object", {
  sent <- NULL
  tr <- function(b) { sent <<- jsonlite::fromJSON(jsonlite::toJSON(b, auto_unbox = TRUE), simplifyVector = FALSE); noul(0.8)(b) }
  a <- withr_options(Rjif.transport = tr, jif("s", "q", c(true = "present", false = "absent")))
  isTRUE(a) && identical(sent$questions$q$criteria, list(true = "present", false = "absent"))
})
check("F4 distribution and weighted-mean tolerance boundaries", {
  a <- suppressWarnings(withr_options(Rjif.transport = forge(list(choice = "a", confidence = 1,
    probabilities = list(a = 0.5, b = 0.49))), jif("s", qc)))
  b <- suppressWarnings(withr_options(Rjif.transport = forge(list(score = 0.55, confidence = 1,
    probabilities = list("0" = 0.5, "1" = 0.5), legend = list("0" = "low", "1" = "high"))), jif("s", qs)))
  !jif_abstained(a) && !jif_abstained(b)
})
check("F5 all computed backoffs obey max_wait", {
  .pick(".backoff_wait")(2, 10, 30, NULL, max_wait = 0.1) == 0.1
})
check("F5 HTTP-date Retry-After obeys server hint and cap", {
  old <- Sys.getlocale("LC_TIME"); on.exit(Sys.setlocale("LC_TIME", old))
  Sys.setlocale("LC_TIME", "C")
  future <- format(Sys.time() + 60, "%a, %d %b %Y %H:%M:%S GMT", tz = "GMT")
  .pick(".backoff_wait")(1, 0.01, 0.02, future, max_wait = 2) == 2
})
check("F5 helper actually executes", {
  f <- .pick(".backoff_wait")
  w <- vapply(1:8, function(k) f(k, 1, 30, NULL), numeric(1))
  all(w >= 1 & w <= 30) && w[2] >= 2 && f(1, 1, 30, "7", 120) == 7 &&
    f(1, 1, 30, "9999", 120) == 120
})
check("F5 resume costs zero transport calls", local({
  cache <- tempfile(); on.exit(unlink(cache)); calls <- 0L
  tr <- function(b) { calls <<- calls + 1L; noul(0.9)(b) }
  withr_options(Rjif.transport = tr, {
    a <- jev_score_many(c("a", "b"), "q", cache = cache, batch = 1)
    first <- calls
    b <- jev_score_many(c("a", "b"), "q", cache = cache)
    first == 2 && calls == first && attr(b, "n_resumed") == 2 && identical(a[1:6], b[1:6])
  })
}))
for (change in c("question", "long-question", "criteria", "model", "rowcount", "threshold", "floor")) {
  check(paste("F5 rejects stale cache", change), local({
    cache <- tempfile(); on.exit(unlink(cache)); calls <- 0L
    tr <- function(b) { calls <<- calls + 1L; noul(0.9)(b) }
    q <- jev_noul_q(paste(rep("x", 220), collapse = ""))
    args <- list(state_vec = c("a", "b"), question = q, cache = cache, model = "m1")
    withr_options(Rjif.transport = tr, {
      do.call(jev_score_many, args)
      if (change == "question") args$question <- "different"
      if (change == "long-question") args$question$instructions <- paste0(q$instructions, "different")
      if (change == "criteria") args$question$criteria <- list(true = "new rubric")
      if (change == "model") args$model <- "m2"
      if (change == "rowcount") args$state_vec <- "a"
      if (change == "threshold") args$threshold <- 0.95
      if (change == "floor") args$confidence_floor <- 0.99
      before <- calls; out <- warned(do.call(jev_score_many, args))
      calls - before == length(args$state_vec) && attr(out$value, "n_resumed") == 0 &&
        any(grepl("does not match", out$warnings))
    })
  }))
}
check("F5 error retries clear stale diagnostics; third pass free", local({
  cache <- tempfile(); on.exit(unlink(cache)); calls <- 0L
  tr <- function(b) { calls <<- calls + 1L; if (calls == 1L) stop("transient"); noul(0.9)(b) }
  withr_options(Rjif.transport = tr, {
    a <- jev_score_many("a", "q", cache = cache)
    b <- jev_score_many("a", "q", cache = cache)
    c <- jev_score_many("a", "q", cache = cache)
    nzchar(a$error) && b$error == "" && c$error == "" && calls == 2 && attr(c, "n_resumed") == 1
  })
}))
check("F5 keep errors option avoids calls", local({
  cache <- tempfile(); on.exit(unlink(cache)); calls <- 0L
  tr <- function(b) { calls <<- calls + 1L; stop("transient") }
  withr_options(Rjif.transport = tr, {
    a <- jev_score_many("a", "q", cache = cache)
    b <- withr_options(Rjif.cache_rerun_errors = FALSE, jev_score_many("a", "q", cache = cache))
    calls == 1 && nzchar(b$error) && attr(b, "n_resumed") == 1
  })
}))
check("F5 policy abstentions are completed rows, not transport failures", local({
  cache <- tempfile(); on.exit(unlink(cache)); calls <- 0L
  tr <- function(b) { calls <<- calls + 1L; noul(0.5)(b) }
  withr_options(Rjif.transport = tr, {
    a <- jev_score_many("a", "q", confidence_floor = 0.7, cache = cache)
    b <- jev_score_many("a", "q", confidence_floor = 0.7, cache = cache)
    calls == 1 && attr(b, "n_resumed") == 1 && is.na(b$decision) && nzchar(b$error)
  })
}))
check("F5 interrupt persists completed chunks", local({
  cache <- tempfile(); on.exit(unlink(cache)); calls <- 0L
  tr <- function(b) {
    calls <<- calls + 1L
    if (calls == 3L) stop(structure(list(message = "fixture interrupt", call = NULL),
                                   class = c("interrupt", "condition")))
    noul(0.9)(b)
  }
  withr_options(Rjif.transport = tr, {
    tryCatch(jev_score_many(letters[1:4], "q", batch = 2, cache = cache), interrupt = function(e) NULL)
    before <- calls
    b <- jev_score_many(letters[1:4], "q", batch = 2, cache = cache)
    calls - before == 2 && attr(b, "n_resumed") == 2 && all(b$decision)
  })
}))

# Real HTTP transport on loopback only. A dummy credential never leaves localhost.
if (.Platform$OS.type != "windows" && requireNamespace("httpuv", quietly = TRUE)) local({
  tmp <- tempfile(); dir.create(tmp); on.exit(unlink(tmp, recursive = TRUE))
  # Do not call httpuv::randomPort() before fork: it starts libuv's thread
  # in the parent, leaving an unusable inherited thread in the child on macOS.
  port <- sample(20000:60000, 1L)
  child <- parallel::mcparallel({
    hits <- new.env(parent = emptyenv())
    server <- httpuv::startServer("127.0.0.1", port, list(call = function(req) {
      path <- req$PATH_INFO
      if (path == "/health") return(list(status = 200L, headers = list(), body = "ready"))
      n <- if (exists(path, hits, inherits = FALSE)) hits[[path]] + 1L else 1L
      hits[[path]] <- n
      cat(path, n, format(as.numeric(Sys.time()), digits = 16), "\n",
          file = file.path(tmp, "hits"), append = TRUE)
      status <- 200L; headers <- list("Content-Type" = "application/json")
      if (path %in% c("/429", "/529") && n == 1L) {
        status <- as.integer(sub("/", "", path)); headers[["Retry-After"]] <- "1"
      }
      if (path %in% c("/401", "/403", "/404", "/422", "/500", "/always429"))
        status <- if (path == "/always429") 429L else as.integer(sub("/", "", path))
      if (path == "/timeout") Sys.sleep(1)
      list(status = status, headers = headers,
           body = '{"answers":{"q":{"noul":0.9}},"usage":{"input_tokens":7}}')
    }))
    writeLines("ready", file.path(tmp, "ready"))
    repeat httpuv::service(50)
  }, silent = FALSE)
  on.exit({ tools::pskill(child$pid); suppressWarnings(parallel::mccollect(child)) }, add = TRUE)
  deadline <- Sys.time() + 10
  while (!file.exists(file.path(tmp, "ready")) && Sys.time() < deadline) Sys.sleep(0.02)
  base <- paste0("http://127.0.0.1:", port)
  stopifnot(httr::status_code(httr::GET(paste0(base, "/health"), httr::timeout(2))) == 200L)
  old_key <- Sys.getenv("TYPESAFE_API_KEY", unset = NA_character_)
  Sys.setenv(TYPESAFE_API_KEY = "offline-local-fixture-only")
  on.exit(if (is.na(old_key)) Sys.unsetenv("TYPESAFE_API_KEY") else
    Sys.setenv(TYPESAFE_API_KEY = old_key), add = TRUE)
  http_call <- function(path, ...) withr_options(Rjif.transport = NULL,
    Rjif.api_base = paste0(base, path), Rjif.retry_base = 0.01, Rjif.retry_cap = 0.02,
    Rjif.retry_max_wait = 2, ..., jev_eval("s", list(q = jev_noul_q("q"))))
  hit_count <- function(path) {
    if (!file.exists(file.path(tmp, "hits"))) return(0L)
    sum(startsWith(readLines(file.path(tmp, "hits")), paste0(path, " ")))
  }
  for (status in c(429, 529)) check(paste("F5 HTTP real Retry-After timing", status), {
    jev_usage_reset(); start <- proc.time()[["elapsed"]]
    a <- http_call(paste0("/", status), Rjif.retries = 1, Rjif.timeout = 2)
    elapsed <- proc.time()[["elapsed"]] - start
    cat("  elapsed", status, elapsed, "seconds\n")
    elapsed >= 0.9 && elapsed < 4 && hit_count(paste0("/", status)) == 2 &&
      jvalue(a$q) == 0.9 && jev_usage()$calls == 1 && jev_usage()$input_tokens == 7
  })
  for (status in c(401, 403, 404, 422, 500)) check(paste("F5 HTTP nonretryable", status), {
    msg <- error_text(http_call(paste0("/", status), Rjif.retries = 3, Rjif.timeout = 2))
    cat("  final error:", msg, "\n")
    hit_count(paste0("/", status)) == 1 && grepl("1 attempt", msg)
  })
  check("F5 HTTP exhausted retry attempts and billing caveat", {
    jev_usage_reset()
    msg <- error_text(http_call("/always429", Rjif.retries = 2, Rjif.timeout = 2))
    hit_count("/always429") == 3 && grepl("3 attempt", msg) && grepl("bill", msg) && jev_usage()$calls == 0
  })
  check("F5 HTTP hard fractional timeout and transport retries", {
    jev_usage_reset(); start <- proc.time()[["elapsed"]]
    msg <- error_text(http_call("/timeout", Rjif.retries = 1, Rjif.timeout = 0.15))
    elapsed <- proc.time()[["elapsed"]] - start
    cat("  timeout elapsed", elapsed, "seconds;", msg, "\n")
    elapsed >= 0.25 && elapsed < 0.8 && grepl("2 attempt", msg) && grepl("bill", msg) && jev_usage()$calls == 0
  })
}) else cat("SKIP loopback HTTP fixture: requires httpuv and fork support\n")
cat(sprintf("R14 REGRESSIONS: %d passed, %d failed\n", pass, fail))
if (fail) quit(save = "no", status = 1L)
