# Round 6h (audit r6h R6h-m1): the r6f/r6g suites proved the shared-opts
# plumbing (preflight render == digest render == transport render BY
# construction) but never saw the REAL wire. This test closes that gap:
# (1) capture the r6g preflight render with a recording mock transport,
# (2) make the identical call against a loopback HTTP server and compare
# the POST body the server actually received, byte for byte,
# (3) unserialize the cache fingerprint and recompute the question/model
# identities FROM THE PARSED WIRE BODY: the cached decision must describe
# exactly the request the API saw, under the API's own JSON parsing.
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
wire_render <- .pick(".wire_render")
question_identity <- .pick(".question_identity")
model_identity <- .pick(".model_identity")
bare_char <- .pick(".bare_char")

if (.Platform$OS.type != "windows" && requireNamespace("httpuv", quietly = TRUE)) local({
  tmp <- tempfile(); dir.create(tmp); on.exit(unlink(tmp, recursive = TRUE))
  # Same fork-server shape as r14 (no randomPort() before fork: it starts
  # libuv's thread in the parent and strands the child).
  port <- sample(20000:60000, 1L)
  child <- parallel::mcparallel({
    hits <- new.env(parent = emptyenv())
    server <- httpuv::startServer("127.0.0.1", port, list(call = function(req) {
      path <- req$PATH_INFO
      if (path == "/health") return(list(status = 200L, headers = list(), body = "ready"))
      # httpuv hands the raw request body through rook.input (req$body is
      # NULL in this API version -- r14's server never read bodies).
      b <- req$rook.input$read()
      n <- if (exists(path, hits, inherits = FALSE)) hits[[path]] + 1L else 1L
      hits[[path]] <- n
      writeBin(b, file.path(tmp, paste0("body-", sub("/", "", path),
                                        "-", n, ".json")))
      list(status = 200L,
           headers = list("Content-Type" = "application/json"),
           body = '{"answers":{"q":{"noul":0.9}},"usage":{"input_tokens":7}}')
    }))
    writeLines("ready", file.path(tmp, "ready"))
    repeat httpuv::service(50)
  }, silent = FALSE)
  on.exit({ tools::pskill(child$pid)
            suppressWarnings(parallel::mccollect(child)) }, add = TRUE)
  deadline <- Sys.time() + 10
  while (!file.exists(file.path(tmp, "ready")) && Sys.time() < deadline)
    Sys.sleep(0.02)

  if (!file.exists(file.path(tmp, "ready"))) {
    cat("SKIP wire-capture HTTP fixture: server did not start\n")
  } else {
    old_opts <- options(Rjif.retries = 0L,
                        Rjif.timeout = 5, Rjif.model = "wire-model")
    old_key <- Sys.getenv("TYPESAFE_API_KEY")
    Sys.setenv(TYPESAFE_API_KEY = "offline-local-fixture-only")
    on.exit({ if (nzchar(old_key)) Sys.setenv(TYPESAFE_API_KEY = old_key)
              else Sys.unsetenv("TYPESAFE_API_KEY")
              options(old_opts) }, add = TRUE)

    # ---- 1. recording mock: the r6g preflight render -----------------------
    # Fixture carries a null JSON field (criteria = NULL renders
    # "criteria":null -- tests the null="null" policy end to end) and a
    # 1.123456789 extra field (proves the oracle agreement holds under the
    # serializer's digit rounding: identity describes the WIRE value).
    qf <- jev_noul_q("q")
    qf$precision <- 1.123456789
    recorded <- new.env(parent = emptyenv())
    recorded$n <- 0L
    rec_tr <- function(body) {
      recorded$n <- recorded$n + 1L
      recorded$render <- wire_render(body, "the request")
      list(model = "wire-model", usage = list(input_tokens = 7L),
           answers = list(q = list(type = "noul", noul = 0.9)))
    }
    options(Rjif.transport = rec_tr)
    a <- jev_eval("s", list(q = qf))
    preflight_render <- recorded$render

    # ---- 2. real transport: the bytes httr::POST actually sent -------------
    options(Rjif.transport = NULL, Rjif.api_base = paste0("http://127.0.0.1:", port, "/direct"))
    b2 <- jev_eval("s", list(q = qf))
    wire <- readBin(file.path(tmp, "body-direct-1.json"), "raw", 1e6)
    wire_txt <- rawToChar(wire)
    Encoding(wire_txt) <- "UTF-8"
    check("W1 real POST body is byte-identical to the preflight render",
          identical(preflight_render, wire_txt))
    check("W2 mock transport was entered exactly once (render count is its own env)",
          identical(recorded$n, 1L))

    # ---- 3. cache identity recomputed FROM THE PARSED WIRE ------------------
    parsed <- jsonlite::fromJSON(wire_txt, simplifyVector = FALSE)
    # Independent oracle (audit r6i R6i-m1): the expected digest is built
    # HERE from the server-parsed wire object with jsonlite's serializer and
    # the documented envelope shape -- md5 over the rendered bytes -- WITHOUT
    # calling .question_identity/.model_identity on the expected side. If a
    # mutation contaminated those helpers, their output and the cache would
    # agree while BOTH left the wire; this oracle cannot cancel with them.
    indep_digest <- function(x, n) {
      payload <- as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null"))
      stopifnot(validUTF8(payload), jsonlite::validate(payload))
      d <- openssl::md5(charToRaw(enc2utf8(payload)))
      list(digest = paste(format(d), collapse = ""), n = as.integer(n))
    }
    q_expect <- indep_digest(list(q = parsed$questions[[1]]),
                             length(parsed$questions[[1]]))
    q_now <- question_identity(parsed$questions[[1]])
    check("W3 question digest == INDEPENDENT render of the API-parsed wire",
          identical(q_expect, q_now))
    m_expect <- indep_digest(list(.f = parsed$model), 1L)
    m_now <- model_identity(parsed$model)
    check("W4 model digest == INDEPENDENT render of the API-parsed wire",
          identical(m_expect, m_now))
    check("W5 wire model is the bare character the API names",
          identical(parsed$model, "wire-model"))

    # ---- 4. through the cache: fingerprint vs parsed batch wire ------------
    cf <- tempfile(fileext = ".rds")
    options(Rjif.api_base = paste0("http://127.0.0.1:", port, "/batch"))
    m <- jev_score_many("s", qf, cache = cf, quiet = TRUE)
    bwire <- readBin(file.path(tmp, "body-batch-1.json"), "raw", 1e6)
    btxt <- rawToChar(bwire); Encoding(btxt) <- "UTF-8"
    bparsed <- jsonlite::fromJSON(btxt, simplifyVector = FALSE)
    df <- readRDS(cf)
    fp <- unserialize(attr(df, "cache_fingerprint", exact = TRUE))
    check("W6 cache fingerprint is version 8", identical(fp$version, 8L))
    check("W7 cached question digest == INDEPENDENT render of the batch wire",
          identical(fp$question$digest,
                    indep_digest(list(q = bparsed$questions[[1]]),
                                 length(bparsed$questions[[1]]))$digest))
    check("W8 cached model digest == INDEPENDENT render of the batch wire",
          identical(fp$model_id$digest,
                    indep_digest(list(.f = bparsed$model), 1L)$digest))
    check("W9 cached bare model == wire model", identical(fp$model, bare_char(bparsed$model)))
    # resume must hit the fingerprint (no second POST): count batch bodies
    m2 <- jev_score_many("s", qf, cache = cf, quiet = TRUE)
    check("W10 identical rerun resumes from cache (no second batch POST)",
          !file.exists(file.path(tmp, "body-batch-2.json")) && identical(m2$decision, m$decision))
    options(old_opts)
  }
}) else cat("SKIP wire-capture HTTP fixture: requires httpuv and fork support\n")
cat(sprintf("R16 WIRE CAPTURE: %d passed, %d failed\n", pass, fail))
if (fail) quit(save = "no", status = 1L)
