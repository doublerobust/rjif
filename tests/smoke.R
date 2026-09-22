# --- bootstrap: prefer the installed package, fall back to sourcing R/ ---
loaded <- FALSE
if (!"package:Rjif" %in% search()) {
  if (requireNamespace("Rjif", quietly = TRUE)) {
    suppressPackageStartupMessages(library(Rjif))
    loaded <- TRUE
  } else if (dir.exists("R")) {
    for (f in list.files("R", pattern = "[.]R$", full.names = TRUE)) source(f)
    cat("  (Rjif not installed: sourced R/ directly)\n")
  } else {
    stop("Rjif is not installed and no R/ directory was found; ",
         "run from the package root or install the package first.")
  }
} else {
  loaded <- TRUE
}
# Internals are not on the search path once the package is attached; alias them
# so the assertions below can exercise them in either mode.
.pick <- function(nm) {
  env <- if (loaded) asNamespace("Rjif") else globalenv()
  get(nm, envir = env)
}
as_question       <- .pick(".as_question")
transport_httr    <- .pick(".transport_httr")
scrub_secrets     <- .pick(".scrub_secrets")
clean_error_text  <- .pick(".clean_error_text")
price_per_mtok    <- .pick("JEV_PRICE_PER_MTOK")
exports_attached  <- if (loaded) getNamespaceExports("Rjif") else character(0)

# --- scripted mock transport (offline, deterministic) ---
mock <- rjif_mock_transport(c(
  "the customer wants a refund" = 0.93,
  "the customer is asking about product sizing" = 0.06,
  "the customer wants to speak to a human" = 0.21,
  "the package arrived on time" = 0.12))
options(Rjif.transport = mock)

# --- a transport that fabricates whatever the caller asks for ---
transport_with_answers <- function(ans) function(body) list(answers = ans)

fail <- 0L
expect <- function(label, cond) {
  cond <- tryCatch(isTRUE(cond), error = function(e) {
    cat("      (threw: ", conditionMessage(e), ")\n", sep = ""); FALSE })
  if (isTRUE(cond)) cat(sprintf("  ok   %s\n", label))
  else { fail <<- fail + 1L; cat(sprintf("  FAIL %s\n", label)) }
}
throws <- function(expr) tryCatch({ force(expr); FALSE }, error = function(e) TRUE)
err_msg <- function(expr) tryCatch({ force(expr); "" }, error = function(e) conditionMessage(e))
warn_msg <- function(expr) {
  # capture the warning text: the previous version muffled the warning inside a
  # calling handler and then returned "", so every warning assertion failed by
  # construction rather than on the behaviour it was testing.
  msgs <- character(0)
  withCallingHandlers(
    tryCatch(force(expr), error = function(e) NULL),
    warning = function(w) {
      msgs <<- c(msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  paste(msgs, collapse = "\n")
}
capture <- function(expr) paste(capture.output(force(expr)), collapse = "\n")

# --- shared fixtures ---
# The canonical customer ticket, and a mini eCRF-ish corpus with gold labels.
ticket <- paste0("This jacket sucks! The zipper jammed the first time I wore ",
                 "it and now it won't close so I want my money back. Order 11 ",
                 "days old, return window 30 days, $100, one previous order, ",
                 "no refunds.")
narratives <- c(
  "patient reports feeling off since dose 3, daughter drove her to urgent care",
  "mild headache resolved spontaneously, no intervention",
  "refused next visit, transportation issues, will reschedule",
  "lab ALT 3x ULN noted, dose held pending repeat",
  "no adverse events reported this visit")
truth <- c(TRUE, FALSE, FALSE, TRUE, FALSE)   # "warrants safety review"
Q_REFUND <- "the customer wants a refund"
Q_SIZING <- "the customer is asking about product sizing"
Q_SAE <- "a serious adverse event is being reported"

branches3 <- list(returns = "product return or refund request",
                  quality_control = "product defect, batch quality issue",
                  human_agent = "ambiguous or needs judgment")

# ===========================================================================
cat("1. jif() / j_ifelse() triage\n")
# unscripted transport: values are hash-derived and MEANINGLESS, so only the
# contract is asserted (type, attributes, no crash), never the outcome.
decision <- jif(ticket, Q_REFUND, threshold = 0.5)
expect("returns a single logical with abstained + answer attributes",
       is.logical(decision) && length(decision) == 1L &&
       !is.null(attr(decision, "abstained")) &&
       inherits(attr(decision, "answer"), "jev_answer"))
expect("no floor set -> not abstained", isFALSE(jif_abstained(decision)))
expect("jif_abstained() is definitively FALSE/TRUE, never NA",
       is.logical(jif_abstained(decision)) && length(jif_abstained(decision)) == 1L &&
       !is.na(jif_abstained(decision)))
# scripted transport: now an outcome assertion IS a documented intent
expect("scripted 0.93 decides TRUE", isTRUE(jif(ticket, Q_REFUND)))
expect("scripted 0.06 decides FALSE", isFALSE(jif(ticket, Q_SIZING)))
expect("undecided value can be dropped into if() without crashing",
       !throws(if (jif(ticket, Q_REFUND)) 1L else 2L))
expect("j_ifelse() routes TRUE to yes",
       identical(j_ifelse(jif(ticket, Q_REFUND), "auto-issue return label",
                          "reply asking what they want", "queue for human"),
                 "auto-issue return label"))
expect("j_ifelse() routes FALSE to no",
       identical(j_ifelse(jif(ticket, Q_SIZING), "a", "b", "c"), "b"))

cat("\n2. the abstention lane (the core design value)\n")
crashes <- throws(if (NA) 1L else 2L)
expect("base R really does error on if (NA) -- why the lane exists", crashes)
low <- jif(ticket, Q_REFUND, confidence_floor = 0.99)
expect("under the floor -> abstained", isTRUE(jif_abstained(low)))
expect("under the floor -> default abstain value is NA", isTRUE(is.na(low)))
expect("jif_reason() explains the abstention",
       grepl("confidence_floor", jif_reason(low), fixed = TRUE))
expect("an abstention DOES crash a bare if() -- loud, not silent", throws(if (low) 1L else 2L))
expect("abstain = FALSE is an explicit opt-in that routes unknown -> no branch",
       { d <- jif(ticket, Q_REFUND, confidence_floor = 0.99, abstain = FALSE)
         isFALSE(d) && isTRUE(jif_abstained(d)) })
expect("a missing answer value abstains even with confidence_floor = 0",
       isTRUE(jif_abstained(withr_options(
         Rjif.transport = transport_with_answers(list(q = list(type = "noul"))),
         jif("s", jev_noul_q("x"))))))
expect("a missing answer value errors inside a bare if()",
       throws(if (jif("s", jev_noul_q("x"), confidence_floor = 0.99)) 1L else 2L))
expect("j_ifelse() sends an abstention to the unknown lane",
       identical(j_ifelse(low, "a", "b", "escalate"), "escalate"))
expect("j_ifelse() sends a bare NA to the unknown lane too",
       identical(j_ifelse(NA, "a", "b", "unknown"), "unknown"))
expect("j_ifelse() refuses a vector",
       grepl("single jif", err_msg(j_ifelse(c(TRUE, FALSE), "a", "b")), fixed = TRUE))
choice_dec <- jif(ticket, "Which team should own this inquiry?", branches3)
expect("choice question returns an option name, not TRUE/FALSE",
       is.character(choice_dec) && choice_dec %in% names(branches3))
expect("j_ifelse() maps named branches onto the returned option",
       { got <- j_ifelse(choice_dec,
                         c(returns = "label", quality_control = "defect ticket"),
                         NA_character_, "human")
         # compare on the value, not identical(): a jif() result carries
         # abstained/answer attributes, so identical() against a bare string
         # is FALSE even when the option matches.
         key <- as.character(choice_dec)
         want <- if (key == "returns") "label" else
                   if (key == "quality_control") "defect ticket" else "human"
         identical(got, want) })
expect("j_ifelse() with named branches sends an unmatched option to unknown",
       identical(j_ifelse("nothing_matching", c(returns = "label"), "no", "unknown"),
                 "unknown"))

# --- arm laziness regression (external audit 2026-09-19: names(yes) used to
# FORCE both arms; only the routed arm's winning element may ever evaluate)
lazy_hits <- new.env()
lazy_route <- j_ifelse(choice_dec,
                       yes = list(returns = { lazy_hits$y <- 1; "label" },
                                  quality_control = { lazy_hits$q <- 1; "defect" },
                                  human_agent = { lazy_hits$h <- 1; "human" }),
                       no = "ignore", unknown = "escalate")
expect("j_ifelse() evaluates the routed branch", nzchar(as.character(lazy_route)))
expect("j_ifelse() does NOT evaluate losing branches in the winning arm",
       { hitn <- sum(c(!is.null(lazy_hits$y), !is.null(lazy_hits$q),
                       !is.null(lazy_hits$h)))
         hitn == 1L &&
           switch(as.character(lazy_route), label = !is.null(lazy_hits$y),
                  defect = !is.null(lazy_hits$q), human = !is.null(lazy_hits$h),
                  FALSE) })
lazy_hits2 <- new.env()
invisible(j_ifelse("no_such_option",
                   yes = list(returns = { lazy_hits2$y <- 1; "label" }),
                   no = list(beta = { lazy_hits2$b <- 1; "B" }),
                   unknown = { lazy_hits2$u <- 1; "escalate" }))
expect("j_ifelse() no-match evaluates ONLY the unknown lane",
       is.null(lazy_hits2$y) && is.null(lazy_hits2$b) && !is.null(lazy_hits2$u))
lazy_hits3 <- new.env()
invisible(j_ifelse("beta",
                   yes = list(alpha = { lazy_hits3$a <- 1; "A" }),
                   no = list(beta = { lazy_hits3$b <- 1; "B" }),
                   unknown = "U"))
expect("j_ifelse() routing to the no-arm never evaluates the yes-arm",
       is.null(lazy_hits3$a) && !is.null(lazy_hits3$b))

cat("\n3. jev_score_many() over a mini eCRF corpus\n")
df <- jev_score_many(narratives, Q_SAE)
expect("one row per input, same order", identical(nrow(df), length(narratives)))
expect("columns present",
       all(c("decision", "option", "p", "confidence", "abstained", "error") %in% names(df)))
expect("no per-row errors", identical(df$error, rep("", length(narratives))))
expect("p is finite for every row", all(is.finite(df$p)))
expect("noul confidence column is always NA (the API sends none)",
       all(is.na(df$confidence)))
expect("option column is \"true\"/\"false\" text for noul",
       all(df$option %in% c("true", "false")))
expect("decision matches p >= threshold for every row",
       identical(df$decision, df$p >= 0.5))
expect("decision is never NA for a scored row", !any(is.na(df$decision)))
expect("abstained all FALSE with no floor set", identical(df$abstained, rep(FALSE, 5L)))
df$truth <- truth
withr_options(Rjif.transport = transport_with_answers(
  list(q = list(type = "noul", noul = NA_real_))), {
  d <- jev_score_many("some narrative", jev_noul_q("x"))
  expect("NA noul from the API -> decision NA (undecided), abstained TRUE (audit B2)",
         identical(d$decision, NA) && identical(d$abstained, TRUE) && is.na(d$p) &&
         identical(d$option, NA_character_))
})
withr_options(Rjif.transport = transport_with_answers(
  list(other = list(type = "noul", noul = 0.5))), {
  d <- jev_score_many(c("a", "b"), jev_noul_q("x"))
  expect("a missing per-row answer is recorded per row, not fatal",
         identical(unique(d$error), "Rjif: missing answer for question 'q'.") &&
         identical(d$abstained, rep(TRUE, 2L)) && identical(d$decision, rep(NA, 2L)))
})
d2 <- jev_score_many(c(narratives[[1]], NA_character_, narratives[[2]]), Q_SAE)
expect("an NA state abstains with a note and does not poison its neighbours",
       identical(d2$abstained, c(FALSE, TRUE, FALSE)) &&
       identical(d2$error[2], "state is NA") && identical(d2$error[1], ""))
counter <- 0L
withr_options(Rjif.transport = function(body) {
  counter <<- counter + 1L
  if (counter == 2L) stop("boom")
  mock(body)
}, {
  d3 <- jev_score_many(narratives[1:3], Q_SAE)
  expect("a transport error is captured per row, not fatal",
         identical(d3$abstained, c(FALSE, TRUE, FALSE)) && nzchar(d3$error[2]) &&
         identical(d3$error[1], ""))
})
expect("exactly one HTTP-equivalent call per row (batch does not amortise)",
       (identical(withr_options(Rjif.transport = function(body) {
          counter <<- counter + 1L; mock(body) },
          { counter <- 0L; jev_score_many(narratives, Q_SAE, batch = 2L); counter }),
          5L)))
dc <- jev_score_many(narratives[1:2], jev_choice_q("what happened?",
  c(infection = "infectious event", headache = "headache syndrome",
    other = "other or unspecified")))
expect("choice rows return a real option name", all(dc$option %in% names(dc$p[[1]])) ||
       all(dc$option %in% c("infection", "headache", "other")))
expect("choice p is the chosen option's probability", all(dc$p > 0 & dc$p <= 1))
ds <- jev_score_many(narratives[[4]], jev_score_q("severity",
  c("none", "mild", "moderate", "severe", "life-threatening")))
expect("score row returns a rubric level index in range",
       ds$p >= 0 && ds$p <= 4 && is.logical(ds$decision))
expect("score threshold is a LEVEL, not a probability (documented semantics)",
       isTRUE(jif(narratives[[4]], jev_score_q("severity",
                   c("none", "mild", "moderate", "severe", "life-threatening")),
                   threshold = 0)))
expect("attr n_abstained is reported", identical(attr(df, "n_abstained"), 0L))
expect("NA floor means abstain always",
       identical(jev_score_many(narratives[1:2], Q_SAE, confidence_floor = NA)$abstained,
                 rep(TRUE, 2L)))

cat("\n4. calibration analytics (hand-computable cases)\n")
rc <- reliability_curve(df, n_bins = 2L)
expect("one row per bin, empty bins kept", identical(nrow(rc), 2L))
expect("bin labels are the cut labels for the requested bins",
       identical(rc$bin, c("[0,0.5]", "(0.5,1]")))
expect("n sums to usable rows", identical(sum(rc$n), attr(rc, "n_used")))
expect("bin counts match the data (no row silently reassigned)",
       identical(rc$n, c(sum(df$p <= 0.5), sum(df$p > 0.5))))
expect("mean_p is the in-bin mean; NA exactly where the bin is empty",
       isTRUE(all.equal(rc$mean_p[[1]], mean(df$p[df$p <= 0.5]))) &&
       identical(is.na(rc$mean_p), rc$n == 0L) &&
       identical(is.na(rc$observed_rate), rc$n == 0L))
empty_bin <- reliability_curve(data.frame(p = c(0.9, 0.9), truth = c(TRUE, TRUE)),
                               n_bins = 2L)
expect("an unoccupied bin is a kept row of n = 0 with NA mean_p/observed_rate",
       identical(empty_bin$n, c(0L, 2L)) && is.na(empty_bin$mean_p[[1]]) &&
       is.na(empty_bin$observed_rate[[1]]) &&
       isTRUE(all.equal(empty_bin$mean_p[[2]], 0.9)))
default_rc <- reliability_curve(df)
expect("default n_bins gives 10 rows", identical(nrow(default_rc), 10L))
expect("only occupied bins have numeric observed_rate",
       identical(!is.na(default_rc$observed_rate), default_rc$n > 0L))
expect("the old column name is GONE, not silently NULL-as-usual (breaking rename)",
       { check <- reliability_curve(df); is.null(check$accuracy) &&
         "accuracy" %in% names(check) == FALSE })
expect("probabilities outside [0,1] are dropped and counted",
       { oo <- reliability_curve(data.frame(p = c(0.5, 1.4, -0.2, NA),
                                            truth = c(TRUE, TRUE, TRUE, TRUE)))
         identical(sum(oo$n), 1L) && identical(attr(oo, "n_out_of_range"), 2L) &&
         identical(attr(oo, "n_used"), 1L) })
expect("a truth column of 0/1 numerics is accepted",
       identical(sum(reliability_curve(data.frame(p = c(0.6, 0.8), truth = c(1, 0)))$n), 2L))
expect("a truth column of \"yes\"/\"no\" text is accepted",
       identical(sum(reliability_curve(data.frame(p = c(0.6, 0.8),
                              truth = c("yes", "no"), n_bins = 2L))$n), 2L))
expect("an uninterpretable truth column warns and yields no rows",
       { rc_bad <- NULL
         msg <- warn_msg(rc_bad <- reliability_curve(
           data.frame(p = c(0.6), truth = I(list("x")))))
         grepl("unsupported truth", msg, fixed = TRUE) &&
         identical(nrow(rc_bad), 10L) && identical(sum(rc_bad$n), 0L) })
expect("reliability_curve wants a data.frame",
       grepl("expected a data.frame", err_msg(reliability_curve(list(p = 0.5)))))
expect("a missing truth column errors helpfully",
       grepl("missing column", err_msg(reliability_curve(data.frame(p = 0.5))),
             fixed = TRUE))
expect("n_bins must be a positive integer",
       grepl("n_bins", err_msg(reliability_curve(df, n_bins = 0))))
# ECE: hand-computable. Two occupied bins, |1 - 0.95| and |0 - 0.05|.
two_bin <- data.frame(p = c(rep(0.95, 3), rep(0.05, 1)),
                      truth = c(rep(TRUE, 3), FALSE), n_bins = 2L)
expect("ece() equals the hand-computed weighted sum",
       isTRUE(all.equal(ece(data.frame(p = c(rep(0.95, 3), rep(0.05, 1)),
                                       truth = c(rep(TRUE, 3), FALSE)), n_bins = 2L),
                        0.75 * abs(1 - 0.95) + 0.25 * abs(0 - 0.05))))
expect("ece() is 0 when every occupied bin is exactly calibrated",
       isTRUE(all.equal(ece(data.frame(p = c(1, 1, 0, 0),
                                       truth = c(TRUE, TRUE, FALSE, FALSE)),
                            n_bins = 2L), 0)))
expect("over-confident predictions give a large ece()",
       ece(data.frame(p = c(0.99, 0.99), truth = c(FALSE, FALSE))) > 0.9)
expect("ece() returns NA (not a flattering 0) when nothing is usable",
       isTRUE(is.na(suppressWarnings(ece(data.frame(p = numeric(0),
                                                    truth = logical(0)))))) &&
       grepl("no scored rows", warn_msg(ece(data.frame(p = numeric(0),
                                                       truth = logical(0)))), fixed = TRUE))
expect("ece() ignores empty bins instead of re-weighting on them",
       isTRUE(all.equal(ece(data.frame(p = c(0.99, 0.99), truth = c(FALSE, FALSE)),
                            n_bins = 10L), 0.99)))
sc <- selection_curve(df)
expect("selection_curve spans the whole default floor sequence", identical(nrow(sc), 20L))
expect("columns present", all(c("floor", "n", "kept", "coverage", "pos_rate",
                               "escalated") %in% names(sc)))
expect("coverage is monotone non-increasing in the floor",
       all(diff(sc$coverage) <= 1e-12))
expect("coverage + escalated == 1",
       isTRUE(all.equal(sc$coverage + sc$escalated, rep(1, nrow(sc)))))
expect("floor 0 keeps every usable row", identical(sc$kept[[1]], attr(sc, "n_usable")))
expect("kept == round(coverage * n)", identical(sc$kept, as.integer(round(sc$coverage * sc$n))))
expect("pos_rate is NA exactly where nothing clears the floor",
       identical(is.na(sc$pos_rate), sc$kept == 0L))
sc2 <- selection_curve(data.frame(p = c(0.9, NA, NA, 0.2), truth = c(TRUE, TRUE, FALSE, FALSE)))
expect("unusable rows stay out of the coverage denominator",
       identical(attr(sc2, "n_usable"), 2L) && identical(sc2$n[[1]], 2L))
expect("selection_curve rejects a non-numeric floor_seq",
       grepl("floor_seq", err_msg(selection_curve(df, floor_seq = "a"))))
expect("selection_curve warns when there is nothing usable",
       grepl("no usable rows", warn_msg(selection_curve(data.frame(p = NA_real_,
                                                                  truth = NA)))))

cat("\n5. jmatch() routing\n")
expect("routes the refund ticket to one of the branches",
       { r <- jmatch(ticket, branches3,
                     instructions = "Which team should own this inquiry?")
         is.character(r) && length(r) == 1L && (is.na(r) || r %in% names(branches3)) })
expect("floor above the winning probability falls back",
       identical(jmatch(ticket, branches3,
                        instructions = "Which team should own this inquiry?",
                        confidence_floor = 0.95, fallback = "human"), "human"))
expect("floor below the winning probability routes",
       identical(withr_options(Rjif.transport = transport_with_answers(list(q = list(
         type = "choice", choice = "returns",
         probabilities = list(returns = 0.8, quality_control = 0.15, human_agent = 0.05),
         confidence = 0.8))),
         jmatch("s", branches3, confidence_floor = 0.05)), "returns"))
esc <- jmatch(ticket, list(refund_ok = "within return window and wants refund",
                           escalate_human = "ambiguous, needs judgment",
                           other = "other or unspecified"),
              confidence_floor = 0.999, fallback = "FALLBACK")
expect("a floor nothing clears falls back", identical(esc, "FALLBACK"))
expect("an escape-hatch winner routes to fallback even with no floor",
       identical(withr_options(Rjif.transport = transport_with_answers(
         list(q = list(type = "choice", choice = "other", confidence = 0.91,
                       probabilities = list(other = 0.91, referral = 0.09)))),
         jmatch("s", c(referral = "needs a referral", other = "other or unspecified"))),
         NA_character_))
expect("abstain_options is customisable",
       identical(withr_options(Rjif.transport = transport_with_answers(
         list(q = list(type = "choice", choice = "other", confidence = 0.91,
                       probabilities = list(other = 0.91, referral = 0.09)))),
         jmatch("s", c(referral = "needs a referral", other = "other or unspecified"),
                abstain_options = character(0))), "other"))
expect("a null choice value falls back",
       identical(withr_options(Rjif.transport = transport_with_answers(
         list(q = list(type = "choice", choice = NULL))),
         jmatch("s", branches3)), NA_character_))
expect("a missing probabilities block falls back when a floor is set",
       identical(withr_options(Rjif.transport = transport_with_answers(
         list(q = list(type = "choice", choice = "returns"))),
         jmatch("s", branches3, confidence_floor = 0.5, fallback = "F")), "F"))

cat("\n6. jev_eval() contract and error paths\n")
multi <- jev_eval(ticket, list(refund = jev_noul_q(Q_REFUND),
                               sizing = jev_noul_q(Q_SIZING)))
expect("answers are named and in the requested order",
       identical(names(multi), c("refund", "sizing")))
expect("per-answer class is jev_<type> + jev_answer",
       identical(class(multi$refund), c("jev_noul", "jev_answer")))
expect("the answers block is reported in print()",
       grepl("0.93", capture(print(multi)), fixed = TRUE))
expect("print() returns its input invisibly",
       { res <- NULL
         capture.output(res <- withVisible(print(multi)))
         identical(res$value, multi) && isFALSE(res$visible) })
expect("print() renders an NA value as NA, not a blank",
       grepl("NA", capture(print(withr_options(
         Rjif.transport = transport_with_answers(list(q = list(type = "noul"))),
         jev_eval("s", list(q = jev_noul_q("x")))))), fixed = TRUE))
expect("print() shows the choice distribution and confidence",
       { s <- capture(print(withr_options(
           Rjif.transport = transport_with_answers(list(q = list(
             type = "choice", choice = "b",
             probabilities = list(a = 0.2, b = 0.8), confidence = 0.8))),
           jev_eval("s", list(q = jev_choice_q("i", c(a = "x", b = "y")))))))
         grepl("b=0.800", s, fixed = TRUE) && grepl("conf=0.800", s, fixed = TRUE) })
expect("print() shows the score legend",
       { s <- capture(print(withr_options(
           Rjif.transport = transport_with_answers(list(q = list(
             type = "score", score = 3, confidence = 0.6,
             legend = list("0" = "none", "3" = "life-threatening")))),
           jev_eval("s", list(q = jev_score_q("i", c("none", "mild", "moderate",
                                                    "life-threatening")))))))
         grepl("life-threatening", s, fixed = TRUE) })
expect("a response with no answers field errors and names the fields it got",
       grepl("no 'answers' field", err_msg(withr_options(
         Rjif.transport = function(body) list(model = "x"),
         jev_eval("s", list(q = jev_noul_q("x"))))), fixed = TRUE))
expect("a non-list response errors cleanly",
       grepl("response was not a list", err_msg(withr_options(
         Rjif.transport = function(body) "garbage",
         jev_eval("s", list(q = jev_noul_q("x"))))), fixed = TRUE))
expect("a missing per-question answer names the question",
       grepl("missing answer for question 'q'", err_msg(withr_options(
         Rjif.transport = transport_with_answers(list(other = list(type = "noul", noul = 0.5))),
         jev_eval("s", list(q = jev_noul_q("x"))))), fixed = TRUE))
expect("a null answer value becomes NA rather than crashing",
       is.na(jvalue(withr_options(
         Rjif.transport = transport_with_answers(list(q = list(type = "noul"))),
         jev_eval("s", list(q = jev_noul_q("x")))$q))))
expect("a multi-element answer vector violates the scalar contract -> NA, not truncation (audit B1)",
       { av <- withr_options(
           Rjif.transport = transport_with_answers(list(q = list(type = "noul",
                                                                  noul = list(0.4, 0.9)))),
           suppressWarnings(jev_eval("s", list(q = jev_noul_q("x")))$q))
         is.na(jvalue(av)) && grepl("not a single", av$contract %||% "", fixed = TRUE) })
expect("a non-numeric noul value becomes NA",
       is.na(jvalue(withr_options(
         Rjif.transport = transport_with_answers(list(q = list(type = "noul",
                                                              noul = "probably"))),
         jev_eval("s", list(q = jev_noul_q("x")))$q))))
expect("an answer whose type disagrees with the question is caught",
       grepl("answered type", warn_msg(withr_options(
         Rjif.transport = transport_with_answers(list(q = list(type = "choice",
                                                              noul = 0.5))),
         jev_eval("s", list(q = jev_noul_q("x")))$q)), fixed = TRUE))
expect("probabilities arrive as a named numeric list",
       { pr <- withr_options(Rjif.transport = transport_with_answers(list(q = list(
            type = "choice", choice = "a", confidence = 0.6,
            probabilities = list(a = 0.6, b = 0.4)))),
           jev_eval("s", list(q = jev_choice_q("i", c(a = "x", b = "y"))))$q)$probs
         identical(names(pr), c("a", "b")) && identical(pr$a, 0.6) })
expect("state must be a single non-NA string",
       any(grepl("single non-NA string",
                 c(err_msg(jev_eval(c("a", "b"), list(q = jev_noul_q("x")))),
                   err_msg(jev_eval(NA_character_, list(q = jev_noul_q("x")))),
                   err_msg(jev_eval(42, list(q = jev_noul_q("x")))))), fixed = TRUE))
expect("empty questions are rejected",
       grepl("non-empty named list", err_msg(jev_eval("s", list())), fixed = TRUE))
expect("unnamed questions are rejected",
       grepl("named list", err_msg(jev_eval("s", list(jev_noul_q("x")))), fixed = TRUE))
expect("blank question names are rejected",
       grepl("must be named", err_msg(jev_eval("s", list(a = jev_noul_q("x"),
                                                        jev_noul_q("y")))), fixed = TRUE))
expect("duplicated question names are rejected",
       grepl("unique", err_msg(jev_eval("s", list(a = jev_noul_q("x"),
                                                  a = jev_noul_q("y")))), fixed = TRUE))
expect("non-question elements are rejected",
       grepl("jev_noul_q", err_msg(jev_eval("s", list(a = list(type = "noul")))),
             fixed = TRUE))
expect("choice criteria must be named",
       grepl("named list", err_msg(jev_choice_q("i", c("a", "b"))), fixed = TRUE))
expect("duplicated choice criteria names are rejected",
       grepl("unique", err_msg(jev_choice_q("i", c(a = "x", a = "y"))), fixed = TRUE))
expect("score criteria may not be empty",
       grepl("non-empty", err_msg(jev_score_q("i", character(0))), fixed = TRUE))
expect("score criteria may not contain NA or blanks",
       grepl("empty or NA", err_msg(jev_score_q("i", c("none", NA))), fixed = TRUE))
expect("named score criteria keep their label in the sent text",
       # two levels: single-level rubrics are now refused before the wire
       # (finding 4), so the label-prefix behavior is asserted at the minimum
       # legal size
       identical(unclass(jev_score_q("i", c(none = "no symptoms",
                                            mild = "bothering but daily")))$criteria,
                 c("none: no symptoms", "mild: bothering but daily")))
expect("an unknown question type is refused, never silently TRUE",
       grepl("unknown question type", err_msg(jif("s",
         structure(list(type = "vibes", instructions = "x"), class = "jev_question"))),
         fixed = TRUE))
expect("a jev_question spec is passed through untouched",
       identical(jev_noul_q("x"), as_question(jev_noul_q("x"))))
expect("string + unnamed vector is an error, not a guess",
       grepl("NAMED", err_msg(as_question("q", c("a", "b"))), fixed = TRUE))
expect("string + NA question is an error",
       grepl("single non-NA string", err_msg(as_question(NA_character_)), fixed = TRUE))
expect("threshold must be a single number",
       grepl("threshold", err_msg(jif("s", jev_noul_q("x"), threshold = c(0.4, 0.6)))))
expect("confidence_floor must be a single number",
       grepl("confidence_floor", err_msg(jif("s", jev_noul_q("x"),
                                            confidence_floor = c(0.1, 0.2)))))

cat("\n7. usage accounting and secret hygiene\n")
jev_usage_reset()
expect("reset zeroes the counters", identical(jev_usage()$calls, 0L) &&
                        identical(jev_usage()$input_tokens, 0L))
invisible(jif(ticket, Q_REFUND))
u <- jev_usage()
expect("calls counted", identical(u$calls, 1L))
expect("input tokens accumulated from the usage block",
       # counters accumulate in double space (audit R2-M3: no 32-bit overflow);
       # compare values, not storage type
       identical(as.integer(u$input_tokens), nchar(ticket, type = "bytes") %/% 4L))
expect("cost estimate uses the documented rate",
       isTRUE(all.equal(u$est_cost_usd, u$input_tokens / 1e6 * price_per_mtok)))
withr_options(Rjif.transport = function(body)
  list(answers = list(q = list(type = "noul", noul = 0.7)),
       usage = list(input_tokens = 100, output_tokens = 7)), {
    jev_usage_reset()
    jev_eval("s", list(q = jev_noul_q("x")))
})
expect("a usage block is summed", identical(as.integer(jev_usage()$input_tokens), 100L))
withr_options(Rjif.transport = transport_with_answers(list(q = list(type = "noul",
                                                                   noul = 0.5))), {
  jev_usage_reset(); jev_eval("s", list(q = jev_noul_q("x")))
})
expect("a call with no usage block still counts as a call", identical(jev_usage()$calls, 1L))
withr_options(Rjif.transport = function(body) list(answers = list(), usage = "oops"),
  err_msg(jev_eval("s", list(q = jev_noul_q("x")))))
expect("a malformed usage block does not corrupt the counters",
       is.numeric(jev_usage()$input_tokens) && identical(jev_usage()$calls, 2L))
expect("mock score answer is a CONTINUOUS score with a self-consistent per-level distribution (live API shape, 2026-09-19)",
       { a <- withr_options(Rjif.transport = rjif_mock_transport(c("i" = 0.5)),
            jev_eval("s", list(q = jev_score_q("i",
              c("none", "mild", "moderate", "severe")))))$q
         pv <- as.numeric(a$probs[c("0", "1", "2", "3")])
         wm <- sum(pv * 0:3)
         is.numeric(a$value) && !is.na(a$value) &&
           all(names(a$probs) == c("0", "1", "2", "3")) &&
           abs(sum(pv) - 1) < 1e-3 && abs(wm - a$value) < 0.05 &&
           is.numeric(jprob(a)) && !is.na(jconf(a)) })
expect("a score that contradicts its own distribution is abstained, not trusted (live-shape guard)",
       { a <- suppressWarnings(withr_options(
           Rjif.transport = function(body) list(answers = list(q = list(
             type = "score", score = 1.4, confidence = 0.9,
             legend = list("0" = "none", "1" = "mild", "2" = "severe"),
             probabilities = list("0" = 0.1, "1" = 0.8, "2" = 0.1)))),
           jev_eval("s", list(q = jev_score_q("i", c("none", "mild", "severe")))))$q)
         is.na(a$value) && grepl("contradicts", a$contract %||% "", fixed = TRUE) })
expect("R11-B1: a score distribution supplied as a NAMED NUMERIC VECTOR (not just a list) is accepted and not mis-abstained",
       { tr <- function(body) list(answers = list(q = list(
           type = "score", score = 1.05, confidence = 0.9,
           legend = list("0" = "low", "1" = "middle", "2" = "high"),
           probabilities = c("0" = 0, "1" = 0.95, "2" = 0.05))))
         a <- suppressWarnings(withr_options(Rjif.transport = tr,
               jev_eval("s", list(q = jev_score_q("severity",
                 c("low", "middle", "high")))))$q)
         ok1 <- !is.na(a$value) && abs(a$value - 1.05) < 1e-9 &&
                identical(names(a$probs), c("0", "1", "2"))
         # reordered named numeric vector keeps each value under its own level
         tr2 <- function(body) list(answers = list(q = list(
           type = "score", score = 1.05, confidence = 0.9,
           legend = list("0" = "low", "1" = "middle", "2" = "high"),
           probabilities = c("2" = 0.05, "0" = 0, "1" = 0.95))))
         a2 <- suppressWarnings(withr_options(Rjif.transport = tr2,
                jev_eval("s", list(q = jev_score_q("severity",
                  c("low", "middle", "high")))))$q)
         ok2 <- !is.na(a2$value) &&
                identical(as.numeric(a2$probs[c("0","1","2")]), c(0, 0.95, 0.05))
         # the batch consumer must not abstain either
         ds <- suppressWarnings(withr_options(
                Rjif.transport = tr,
                jev_score_many("s", jev_score_q("severity",
                  c("low", "middle", "high")), threshold = 1)))
         ok3 <- isTRUE(ds$decision[[1L]]) && !isTRUE(ds$abstained[[1L]]) &&
                identical(ds$error[[1L]], "")
         ok1 && ok2 && ok3 })
expect("structured legend entries print safely in ANY key order (R11: examples-first object crashed the printer)",
       { tr <- function(body) list(answers = list(q = list(
           type = "score", score = 1.05, confidence = 0.9,
           legend = list("0" = list(examples = list("a", "b"), what = "low"),
                         "1" = list(what = "middle", examples = list("c")),
                         "2" = "high"),
           probabilities = list("0" = 0, "1" = 0.95, "2" = 0.05))))
         out <- withr_options(Rjif.transport = tr, {
           a <- jev_eval("s", list(q = jev_score_q("i",
                    c("low", "middle", "high"))))
           capture.output(print(a))
         })
         txt <- paste(out, collapse = "\n")
         grepl("low", txt, fixed = TRUE) && grepl("middle", txt, fixed = TRUE) })
expect("structured score criteria survive the constructor and serialize as an ordered JSON array (R11)",
       { crit <- list(list(what = "low", examples = list("a", "b")),
                      list(what = "high", examples = list("c")))
         q <- jev_score_q("rate it", crit)
         json <- jsonlite::toJSON(unclass(q), auto_unbox = TRUE, null = "null")
         identical(length(q$criteria), 2L) &&
           grepl("\"criteria\":[{", json, fixed = TRUE) &&  # ordered JSON array of objects
           grepl("low", json, fixed = TRUE) })
expect("mock scripted score confidence = max emitted mass (point mass reads ~1, R11)",
       { conf_at <- function(s) {
           ans <- withr_options(
             Rjif.transport = rjif_mock_transport(setNames(s, "i")),
             jev_eval("s", list(q = jev_score_q("i",
               c("none", "mild", "severe")))))$q
           ans$confidence
         }
         conf_at(1.0) > 0.99 && conf_at(0.9999) > 0.99 })
expect("the real transport demands an API key",
       grepl("TYPESAFE_API_KEY", err_msg(withr_options(Rjif.transport = NULL,
         transport_httr(list(state = "s")))), fixed = TRUE))
expect("no exported or internal object prints the key's value",
       !any(grepl(Sys.getenv("TYPESAFE_API_KEY"), capture(jev_usage()), fixed = TRUE)) ||
       !nzchar(Sys.getenv("TYPESAFE_API_KEY")))
scr <- scrub_secrets("Bearer sk-TESTSECRET12345678 and again sk-TESTSECRET12345678")
expect("secret scrubbing redacts bearer tokens",
       !grepl("sk-TESTSECRET12345678", scr, fixed = TRUE) &&
       grepl("REDACTED", scr, fixed = TRUE))
expect("error text is length-capped and single-line",
       { t <- clean_error_text(paste(rep("x\ny", 500), collapse = ""))
         nchar(t) <= 320L && !grepl("\n", t) })
# a 401 whose body echoes a key must not re-echo it; verified against a real
# loopback HTTP server in the audit, exercised here through the same scrubber
expect("an HTTP failure message never contains the live key",
       !grepl("sk-REALSECRETKID9999",
              clean_error_text(paste0(rep("Bearer sk-REALSECRETKID9999 x", 100),
                                       collapse = "\n")), fixed = TRUE))

cat("\n8. the mock is reproducible and honest about what it is\n")
expect("mock is the documented two-faced helper: object OR factory",
       is.function(rjif_mock_transport(c("q" = 0.5))))
expect("a bad script errors instead of degrading to the hash",
       grepl("named numeric", err_msg(rjif_mock_transport(list("q" = "0.9"))), fixed = TRUE))
expect("unscripted values are deterministic across calls",
       identical(jif(ticket, "an entirely unscripted query about bone density"),
                 jif(ticket, "an entirely unscripted query about bone density")))
expect("identical inputs give identical batch output",
       identical(jev_score_many(narratives, "unscripted question about bone density")$p,
                 jev_score_many(narratives, "unscripted question about bone density")$p))
expect("the mock does not depend on the global RNG seed",
       { set.seed(42); a <- jev_score_many(narratives, "another unscripted query")$p
         set.seed(7);  b <- jev_score_many(narratives, "another unscripted query")$p
         identical(a, b) })
expect("different questions get different values (a batch can discriminate rows)",
       length(unique(jev_score_many(narratives,
         jev_noul_q(paste("unscripted question", 1:5)))$p)) > 1L ||
       length(unique(vapply(1:5, function(i)
         jev_eval(narratives[[1]], list(q = jev_noul_q(paste("unscripted", i))))$q$value,
         numeric(1)))) == 5L)
expect("noul answers from the mock carry no confidence field",
       is.na(jconf(jev_eval(ticket, list(q = jev_noul_q("an unscripted query")))$q)) ||
       is.null(jconf(jev_eval(ticket, list(q = jev_noul_q("an unscripted query")))$q)))
expect("mock choice probabilities sum to 1",
       isTRUE(all.equal(sum(unlist(withr_options(
         Rjif.transport = rjif_mock_transport(c("i" = 0.5)),
         jev_eval("s", list(q = jev_choice_q("i", c(a = "x", b = "y"))))$q)$probs)), 1,
         tolerance = 1e-4)))
expect("mock score legend is a level-index -> description map",
       identical(names(withr_options(Rjif.transport = rjif_mock_transport(c("i" = 0.5)),
         jev_eval("s", list(q = jev_score_q("i", c("none", "mild", "severe"))))$q)$legend),
         c("0", "1", "2")))
expect("the unscripted hash is spread, not concentrated (no threshold is safe by luck)",
       { v <- vapply(1:300, function(i)
           jev_eval("state", list(q = jev_noul_q(paste("q", i))))$q$value, numeric(1))
         min(v) < 0.05 && max(v) > 0.95 && length(unique(round(v, 2))) > 90 })

cat("\n9. NAMESPACE / API surface\n")
if (loaded) {
  exports_ns <- ls(envir = asNamespace("Rjif"), all.names = FALSE)
  want <- c("ece", "j_ifelse", "jconf", "jif", "jif_abstained", "jev_choice_q",
            "jev_eval", "jev_noul_q", "jev_score_many", "jev_score_q", "jev_usage",
            "jev_usage_reset", "jmatch", "jprob", "jvalue", "reliability_curve",
            "rjif_mock_transport", "selection_curve")
  missing_exports <- setdiff(want, exports_attached)
  cat("  exports visible on attach: ", length(exports_attached), "\n", sep = "")
  expect("every documented public function is exported",
         identical(missing_exports, character(0)))
  expect("print.jev_answers is registered as an S3 method",
         is.function(utils::getS3method("print", "jev_answers", optional = TRUE)))
  expect("no function that exists in R/ is missing from NAMESPACE",
         identical(setdiff(c("jif_reason", ".as_question"), c(exports_attached,
                  ls(envir = asNamespace("Rjif"), all.names = TRUE))), character(0)))
  # internals must NOT leak into the user's search path
  expect("internal helpers are not exported",
         !any(c(".as_question", ".mock_hash01", "transport_with_answers") %in%
                exports_attached))
} else {
  cat("  (skipped: exercising the sourced files, not an installed namespace)\n")
}

# ===========================================================================
cat("\n12. round-2 audit regressions (Astra R2-B1/B2/M1/M2/M3/m1/m3)\n")
fake_key <- "astra_R2_FAKE_opaque_7Qa9"
old_env_key <- Sys.getenv("TYPESAFE_API_KEY", unset = NA)
Sys.setenv(TYPESAFE_API_KEY = fake_key)
expect("boolean score confidence is rejected, not coerced to p=1 (R2-B1)",
       { a <- suppressWarnings(withr_options(
           Rjif.transport = transport_with_answers(
             list(q = list(score = 2, confidence = TRUE))),
           jev_eval("s", list(q = jev_score_q("i", c("none", "mild", "severe"))))$q))
         is.na(jprob(a)) && is.na(jvalue(a)) })
expect("boolean choice probabilities are rejected (R2-B1)",
       { a <- suppressWarnings(withr_options(
           Rjif.transport = transport_with_answers(
             list(q = list(choice = "safety",
                           probabilities = list(safety = TRUE, routine = FALSE),
                           confidence = 0.9))),
           jev_eval("s", list(q = jev_choice_q("i", list(safety = "a", routine = "b"))))$q))
         is.na(jprob(a)) })
expect("positional unnamed probability ARRAY survives redaction shape-wise (R2-M1)",
       { a <- suppressWarnings(withr_options(
           Rjif.transport = transport_with_answers(
             list(q = list(choice = "safety", probabilities = list(0.8, 0.2),
                           confidence = 0.8))),
           jev_eval("s", list(q = jev_choice_q("i", list(safety = "a", routine = "b"))))$q))
         identical(jvalue(a), "safety") && isTRUE(all.equal(jprob(a), 0.8)) })
expect("a contract-invalid answer leaves NO usable p or confidence (R2-M2)",
       { a <- suppressWarnings(withr_options(
           Rjif.transport = transport_with_answers(
             list(q = list(score = 99, confidence = 0.99))),
           jev_eval("s", list(q = jev_score_q("i", c("none", "mild", "severe"))))$q))
         is.na(jprob(a)) && is.na(jconf(a)) && !is.na(a$contract) })
expect("malformed usage cannot discard a delivered answer (R2-M3)",
       { a <- withr_options(
           Rjif.transport = function(body)
             list(answers = list(q = list(type = "noul", noul = 0.9)),
                  usage = list(input_tokens = list())),
           suppressWarnings(jev_eval("s", list(q = jev_noul_q("x")))$q))
         identical(jvalue(a), 0.9) })
expect("cumulative token counters cannot 32-bit overflow to NA (R2-M3)",
       { jev_usage_reset()
         big <- function(body) list(answers = list(q = list(type = "noul", noul = 0.9)),
                                    usage = list(input_tokens = 1.5e9))
         withr_options(Rjif.transport = big, {
           jev_eval("s", list(q = jev_noul_q("x"))); jev_eval("s", list(q = jev_noul_q("y"))) })
         u <- jev_usage()
         identical(u$input_tokens, 3e9) && is.finite(u$est_cost_usd) })
expect("transport exceptions carrying the key are scrubbed before rethrow (R2-B2)",
       { msg <- withr_options(
           Rjif.transport = function(body)
             stop("connect failed for Bearer ", fake_key, call. = FALSE),
           tryCatch(jev_eval("s", list(q = jev_noul_q("x"))),
                    error = function(e) conditionMessage(e)))
         !grepl(fake_key, msg, fixed = TRUE) &&
           grepl("[REDACTED-API-KEY]", msg, fixed = TRUE) })
expect("an atomic invalid answer yields the NA+contract object, not a crash (R2-m1)",
       { a <- suppressWarnings(withr_options(
           Rjif.transport = transport_with_answers(list(q = 0.99)),
           jev_eval("s", list(q = jev_noul_q("x")))$q))
         is.na(jvalue(a)) && !is.na(a$contract) })
expect("jmatch NA floor abstains to fallback; consistent with jif (R2-m3)",
       { got <- withr_options(
           Rjif.transport = transport_with_answers(
             list(q = list(choice = "safety", probabilities = list(safety = 0.9,
                                                                   routine = 0.1),
                           confidence = 0.9))),
           jmatch("s", list(safety = "a", routine = "b"), confidence_floor = NA))
         is.na(got) })
if (is.na(old_env_key)) Sys.unsetenv("TYPESAFE_API_KEY") else
  do.call(Sys.setenv, list(TYPESAFE_API_KEY = old_env_key))

# ===========================================================================
cat("\n13. round-3 audit regressions (Astra R3-B1/R3-B2 probes)\n")
fake_key <- "astra_R2_FAKE_opaque_7Qa9"
old_env_key <- Sys.getenv("TYPESAFE_API_KEY", unset = NA)
Sys.setenv(TYPESAFE_API_KEY = fake_key)
key_leak <- function(x) grepl(fake_key, paste(x, collapse = " "), fixed = TRUE)
expect("named character-vector answer NAMES are scrubbed, not just values (P29)",
       # when a transport names its answer object with the raw API key, the
       # redacted name no longer matches the requested question, so the
       # honest outcomes are EITHER an answer whose dput shows no key OR a
       # fail-closed 'missing answer' error whose message carries no key.
       # What must never happen: the key surviving into any dput output.
       { got <- suppressWarnings(withr_options(
           Rjif.transport = function(body)
             list(answers = setNames(list(list(type = "noul", noul = 0.9)), fake_key)),
           tryCatch(capture.output(dput(jev_eval("s", list(q = jev_noul_q("x"))))),
                    error = function(e) capture.output(dput(conditionMessage(e))))))
         !key_leak(got) })
expect("factor levels in raw metadata are scrubbed (P33)",
       { got <- suppressWarnings(withr_options(
           Rjif.transport = function(body)
             list(answers = list(q = structure(
               factor(c("a", "b"), levels = c(fake_key, "b"))))),
           tryCatch(jev_eval("s", list(q = jev_noul_q("x"))), error = function(e) e)))
         is.list(got) && !key_leak(levels(got$q$raw)) })
expect("a key-bearing class(raw) cannot leak through the not-a-list error (P32)",
       { got <- suppressWarnings(withr_options(
           Rjif.transport = function(body) { x <- "y"; class(x) <- fake_key; x },
           tryCatch(jev_eval("s", list(q = jev_noul_q("x"))),
                    error = function(e) conditionMessage(e))))
         is.character(got) && !key_leak(got) })
expect("a NAMED 1-element object cannot impersonate a scalar wrapper (P36)",
       { got <- suppressWarnings(withr_options(
           Rjif.transport = function(body)
             list(answers = list(q = list(score = 2, confidence = list(wrong = 1)))),
           jev_eval("s", list(q = jev_score_q("i", c("none", "mild", "severe"))))$q))
         is.na(jvalue(got)) && is.na(jprob(got)) && !is.na(got$contract) })
expect("valid score confidence at the endpoints still works",
       # full-contract forgery: score answers REQUIRE probabilities+legend
       # (finding 4; the old bare {score, confidence} shape is now invalid and
       # is asserted as such in the dedicated test below)
       { got <- suppressWarnings(withr_options(
           Rjif.transport = function(body)
             list(answers = list(q = list(score = 2, confidence = 0.99,
                   probabilities = list("0" = 0, "1" = 0, "2" = 1),
                   legend = list("0" = "none", "1" = "mild", "2" = "severe")))),
           jev_eval("s", list(q = jev_score_q("i", c("none", "mild", "severe"))))$q))
         identical(jvalue(got), 2) && isTRUE(all.equal(jprob(got), 0.99)) })
expect("an UNNAMED 1-element array still unwraps to its scalar",
       { got <- suppressWarnings(withr_options(
           Rjif.transport = function(body)
             list(answers = list(q = list(noul = list(0.88)))),
           jev_eval("s", list(q = jev_noul_q("x")))  ))
         identical(jvalue(got$q), 0.88) })
expect("real JSON arrays (simplifyVector=FALSE) survive the redactor end-to-end",
       { resp <- jsonlite::fromJSON(
           paste0('{"answers":{"q":{"choice":"safety",',
                  '"probabilities":[0.8,0.2],"confidence":0.8}}}'),
           simplifyVector = FALSE)
         got <- suppressWarnings(withr_options(
           Rjif.transport = function(body) resp,
           jev_eval("s", list(q = jev_choice_q("i", list(safety = "a",
                                                          routine = "b"))))$q))
         identical(jvalue(got), "safety") && isTRUE(all.equal(jprob(got), 0.8)) })
if (is.na(old_env_key)) Sys.unsetenv("TYPESAFE_API_KEY") else
  do.call(Sys.setenv, list(TYPESAFE_API_KEY = old_env_key))

# ===========================================================================
cat("\n14. round-4 audit regressions (Astra R4-B1/B2, R4-M1/M2)\n")
fake_key <- "astra_R4_FAKE_opaque_7Qa9"
old_env_key <- Sys.getenv("TYPESAFE_API_KEY", unset = NA)
Sys.setenv(TYPESAFE_API_KEY = fake_key)
key_leak4 <- function(x) any(grepl(fake_key, capture.output(dput(x)), fixed = TRUE))
expect("named 1-element OBJECT cannot impersonate a scalar noul wrapper (R4-B1)",
       { a <- suppressWarnings(withr_options(
           Rjif.transport = function(body)
             list(answers = list(q = list(noul = list(wrong = 1)))),
           jev_eval("s", list(q = jev_noul_q("x"))))$q)
         is.na(jvalue(a)) && !is.na(a$contract) })
expect("named object score value rejected; 30 such rows leave analytics with used=0 (R4-B1/M01)",
       # the WHOLE scenario runs inside one withr_options scope: withr_options
       # restores the transport when expr finishes, so calling jev_score_many
       # after the block would silently hit the outer mock transport (this was
       # a test bug, not a package bug -- the bad rows were never re-malformed)
       withr_options(
         Rjif.transport = function(body)
           list(answers = list(q = list(score = list(wrong = 2), confidence = 1,
                 probabilities = list("0" = 0, "1" = 0, "2" = 1),
                 legend = list("0" = "none", "1" = "mild", "2" = "severe")))), {
           a <- suppressWarnings(jev_eval("s", list(q = jev_score_q(
             "i", c("none", "mild", "severe"))))$q)
           rejected <- is.na(jvalue(a))
           df <- suppressWarnings(jev_score_many(rep("narr", 30),
                    jev_score_q("i", c("none", "mild", "severe"))))
           df$truth <- TRUE
           # the batch is type=score: analytics now REFUSE its p as an event
           # probability (finding 2) unless allow_type opts in; the intended
           # property stands - 30 rejected rows leave event analytics with
           # used=0. n_dropped$not_scored counts answered-but-abstained rows;
           # contract-rejected rows have p=NA (bucket p_na). Both sum to 30.
           rc <- tryCatch(reliability_curve(df), error = function(e) NULL)
           g <- suppressWarnings(reliability_curve(df, allow_type = "score"))
           rejected && is.null(rc) &&
             identical(attr(g, "n_used"), 0L) &&
             sum(unlist(attr(g, "n_dropped"))) == 30L &&
             is.na(suppressWarnings(ece(df, allow_type = "score")))
         }) )
expect("string/Date/POSIXt/raw/complex values and probability vectors are REJECTED, not coerced (R4-B1 N14-N17)",
       all(vapply(list(
         list(q = list(noul = "0.9")),
         list(q = list(noul = as.Date(0.9, origin = "1970-01-01"))),
         list(q = list(noul = as.POSIXct(0.9, origin = "1970-01-01", tz = "UTC"))),
         list(q = list(choice = "safety", probabilities = c(safety = "0.9",
                                                            routine = "0.1"))),
         list(q = list(choice = "safety", probabilities = c(safety = 1 + 4i,
                                                            routine = 0 + 0i)))),
         function(aa) {
           qs <- if (!is.null(aa$q$noul)) jev_noul_q("x") else
                   jev_choice_q("i", list(safety = "a", routine = "b"))
           a <- suppressWarnings(withr_options(
             Rjif.transport = function(body) list(answers = aa),
             jev_eval("s", list(q = qs)))$q)
           is.na(jvalue(a))
         }, logical(1))))
expect("a key-bearing class attribute cannot survive retention (R4-B2 M02)",
       { a <- suppressWarnings(withr_options(
           Rjif.transport = function(body)
             list(answers = list(q = list(noul = structure(1, class = fake_key)))),
           jev_eval("s", list(q = jev_noul_q("x")))$q))
         !key_leak4(a) })
expect("redaction runs AFTER validation: key-bearing offered option name keeps p=0.9 usable AND absent from output (R4-M1)",
       { offered <- list(safety = "a", routine = "b"); names(offered)[1] <- fake_key
         a <- suppressWarnings(withr_options(
           Rjif.transport = function(body)
             list(answers = list(q = list(choice = "routine", confidence = 0.9,
                 probabilities =
                 stats::setNames(list(0.1, 0.9), c(fake_key, "routine"))))),
           jev_eval("s", list(q = jev_choice_q("i", offered)))$q))
         identical(jvalue(a), "routine") && isTRUE(all.equal(jprob(a), 0.9)) &&
           is.na(a$contract) && !key_leak4(a) })
expect("POSIXlt in an answer cannot exhaust the redactor's stack (R4-M2)",
       { lt <- as.POSIXlt(as.POSIXct(0.9, origin = "1970-01-01", tz = "UTC"))
         got <- tryCatch(withr_options(
           Rjif.transport = function(body) list(answers = list(q = list(noul = lt))),
           jev_eval("s", list(q = jev_noul_q("x")))),
           error = function(e) conditionMessage(e))
         !(is.character(got) && grepl("C stack|stack limit", got)) })
if (is.na(old_env_key)) Sys.unsetenv("TYPESAFE_API_KEY") else
  do.call(Sys.setenv, list(TYPESAFE_API_KEY = old_env_key))

# ===========================================================================
cat("\n15. round-5 audit regressions (Astra R5-B1/B2/B3)\n")
# R5-B1: display redaction must never MERGE option identities. The vendor
# pattern scrubber turns "Bearer option_alpha"/"Bearer option_beta" into the
# same [REDACTED] label; a label re-lookup after that could hand the selected
# option a SIBLING's higher probability (fabricated certainty). selected_p is
# bound at construction, before any redaction.
fake_key <- "astra_R5_FAKE_opaque_7Qa9"
old_env_key <- Sys.getenv("TYPESAFE_API_KEY", unset = NA)
Sys.setenv(TYPESAFE_API_KEY = fake_key)
expect("R5-B1 (no-key control): pattern-colliding option names keep the SELECTED p, not a sibling's",
       { offered <- list("Bearer option_alpha" = "first", "Bearer option_beta" = "second")
         tr <- function(body) list(answers = list(q = list(
           choice = "Bearer option_beta",
           # beta is the winner (0.9) so the answer satisfies the winner-
           # consistency contract (finding 4); the R5-B1 property under test -
           # the redacted names colliding at display time - is unchanged.
           confidence = 0.9,
           probabilities = stats::setNames(list(0.1, 0.9), names(offered)))))
         a <- suppressWarnings(withr_options(Rjif.transport = tr,
               jev_eval("s", list(q = jev_choice_q("i", offered)))$q))
         isTRUE(all.equal(jprob(a), 0.9)) &&
           identical(as.character(jvalue(a)), "Bearer [REDACTED]") })
expect("R5-B1: exact-key-colliding probability names cannot inflate the selected p",
       { offered <- stats::setNames(list("first", "second"),
                                    c(fake_key, "[REDACTED-API-KEY]"))
         # winner-consistent shape (finding 4): the SELECTED option carries
         # 0.9; the property under test is that after display-time redaction
         # makes both option names identical, jprob() still returns the bound
         # 0.9 of the selected option and cannot be inflated to / stolen by a
         # sibling's value via the colliding label (R5-B1 selected_p binding).
         tr <- function(body) list(answers = list(q = list(
           choice = "[REDACTED-API-KEY]",
           confidence = 0.9,
           probabilities = stats::setNames(list(0.1, 0.9), names(offered)))))
         a <- suppressWarnings(withr_options(Rjif.transport = tr,
               jev_eval("s", list(q = jev_choice_q("i", offered)))$q))
         isTRUE(all.equal(jprob(a), 0.9)) })
expect("R5-B2: a contract warning naming a key-bearing question never emits the key",
       { msgs <- character(0)
         got <- withCallingHandlers(
           tryCatch({
             options(Rjif.transport = function(body)
                       list(answers = stats::setNames(
                         list(list(type = "noul", noul = "0.9")), fake_key)))
             jev_eval("s", stats::setNames(list(jev_noul_q("x")), fake_key))
             "done"
           }, error = function(e) conditionMessage(e)),
           warning = function(w) { msgs <<- c(msgs, conditionMessage(w))
                                  invokeRestart("muffleWarning") })
         options(Rjif.transport = NULL)
         length(msgs) >= 1L &&
           !any(grepl(fake_key, msgs, fixed = TRUE)) &&
           !grepl(fake_key, got, fixed = TRUE) })
expect("R5-B2: a missing-answer error naming a key-bearing question never emits the key",
       { msg <- withr_options(
           Rjif.transport = function(body) list(answers = list()),
           tryCatch(jev_eval("s", stats::setNames(list(jev_noul_q("x")), fake_key)),
                    error = function(e) conditionMessage(e)))
         is.character(msg) && !grepl(fake_key, msg, fixed = TRUE) })
expect("R5-B3: a key-bearing model attribute never survives dput/serialize of the answer set",
       { z <- suppressWarnings(withr_options(
           Rjif.transport = function(body)
             list(model = paste0("vendor/", fake_key),
                  answers = list(q = list(type = "noul", noul = 0.9))),
           jev_eval("s", list(q = jev_noul_q("x")))))
         bytes <- serialize(z, NULL)
         # serialized R streams contain NUL bytes: scan them with grepRaw, not
         # rawToChar (which refuses embedded NULs)
         !any(grepl(fake_key, capture.output(dput(z)), fixed = TRUE)) &&
           length(grepRaw(fake_key, bytes, fixed = TRUE)) == 0L })
expect("R5-B3: environments/functions in raw metadata are replaced, not traversed",
       { env <- new.env(); assign("secret", fake_key, envir = env)
         a <- suppressWarnings(withr_options(
           Rjif.transport = function(body)
             list(answers = list(q = list(type = "noul", noul = 0.9,
                                          diag = env))),
           jev_eval("s", list(q = jev_noul_q("x")))$q))
         grepl("REDACTED", paste(capture.output(dput(a$raw)), collapse = " ")) &&
           !any(grepl(fake_key, capture.output(dput(a)), fixed = TRUE)) })
if (is.na(old_env_key)) Sys.unsetenv("TYPESAFE_API_KEY") else
  do.call(Sys.setenv, list(TYPESAFE_API_KEY = old_env_key))

# ===========================================================================
cat("\n16. round-6 audit regressions (Astra R6-B1/B2)\n")
fake_key <- "astra_R6_FAKE_opaque_7Qa9"
old_env_key <- Sys.getenv("TYPESAFE_API_KEY", unset = NA)
Sys.setenv(TYPESAFE_API_KEY = fake_key)
# serialized streams contain NUL bytes: grepRaw on the bytes, never rawToChar
has_key_bytes <- function(x)
  length(grepRaw(fake_key, serialize(x, NULL), fixed = TRUE)) > 0L
expect("R6-B1: an environment stashed in an ATTRIBUTE by an honest diagnostic transport cannot persist the key",
       { ctx <- new.env(parent = emptyenv())
         ctx$headers <- list(Authorization = paste("Bearer", fake_key))
         tr <- function(body) list(answers = list(q = list(
           type = "noul", noul = 0.9,
           metadata = list(elapsed = structure(0.02, request_context = ctx)))))
         withr_options(Rjif.transport = tr, {
           z <- suppressWarnings(jev_eval("s", list(q = jev_noul_q("x"))))
           d <- suppressWarnings(jif("t", jev_noul_q("x")))
           !has_key_bytes(z) && !has_key_bytes(d) &&
             identical(attr(z$q$raw$metadata$elapsed, "request_context"),
                       "[REDACTED-UNSUPPORTED]")
         }) })
expect("R8-B1: a compiled expression (compiler::compile, typeof 'bytecode') attached as ordinary diagnostic metadata cannot persist the key",
       { request <- call("request",
                         Authorization = paste("Bearer", fake_key))
         compiled <- compiler::compile(request)
         stopifnot(typeof(compiled) == "bytecode")
         tr <- function(body) {
           a <- if (body$questions$q$type == "choice")
             list(choice = "alpha",
                  probabilities = list(alpha = 0.9, beta = 0.1))
           else list(noul = 0.9)
           a$metadata <- list(elapsed = structure(0.02,
                                                  compiled_request = compiled))
           list(answers = list(q = a))
         }
         withr_options(Rjif.transport = tr, {
           z <- suppressWarnings(jev_eval("s", list(q = jev_noul_q("x"))))
           d <- suppressWarnings(jif("t", jev_noul_q("x")))
           c1 <- !has_key_bytes(z) && !has_key_bytes(d) &&
             identical(attr(z$q$raw$metadata$elapsed, "compiled_request"),
                       "[REDACTED-UNSUPPORTED]")
           # and disassembly of the retained artifact reveals nothing
           retained <- attr(z$q$raw$metadata$elapsed, "compiled_request")
           c2 <- !grepl(fake_key, paste(capture.output(print(retained)),
                                        collapse = "\n"), fixed = TRUE)
           c1 && c2
         }) })
expect("R8: inverted retention guard -- every non-plain-data typeof becomes the placeholder (bytecode, promise-form symbols, closures, environments, calls, S4, externalptr); plain numeric/logical/character/NULL still retained losslessly",
       { tr2 <- function(body) list(answers = list(q = list(
           type = "noul", noul = 0.9,
           meta = list(bcd = compiler::compile(call("f", fake_key)),
                       env = emptyenv(), fn = function() fake_key,
                       sym = as.name(fake_key),
                       keep_num = 42L, keep_chr = "safe", keep_null = NULL))))
         z <- suppressWarnings(withr_options(Rjif.transport = tr2,
               jev_eval("s", list(q = jev_noul_q("x")))))
         m <- z$q$raw$meta
         !has_key_bytes(z) &&
           all(vapply(m[c("bcd", "env", "fn", "sym")], identical, logical(1),
                      "[REDACTED-UNSUPPORTED]")) &&
           identical(m$keep_num, 42L) && identical(m$keep_chr, "safe") &&
           is.null(m$keep_null) })
expect("R7-B1: a call object attached by an ordinary debugging transport (typeof 'language') cannot persist the key",
       { inv <- (function(key) match.call())(paste("Bearer", fake_key))
         tr <- function(body) list(answers = list(q = list(
           type = "noul", noul = 0.9,
           metadata = list(invocation = structure(0.02, request = inv)))))
         withr_options(Rjif.transport = tr, {
           z <- suppressWarnings(jev_eval("s", list(q = jev_noul_q("x"))))
           d <- suppressWarnings(jif("t", jev_noul_q("x")))
           !has_key_bytes(z) && !has_key_bytes(d) &&
             identical(attr(z$q$raw$metadata$invocation, "request"),
                       "[REDACTED-UNSUPPORTED]")
         }) })
expect("R6-B1: expression, raw bytes, attributes-of-attributes and key-bearing classes all scrubbed",
       { cases <- list(
           expression = list(type = "noul", noul = 0.9, meta = expression(fake_key)),
           raw_bytes  = list(type = "noul", noul = 0.9,
                             headers = charToRaw(paste("Auth:", fake_key))),
           attr_of_attr = list(type = "noul", noul = 0.9,
                               meta = structure(1.5, note = fake_key)),
           key_class  = list(type = "noul", noul = 0.9,
                             tagged = structure(list(), class = fake_key)))
         all(vapply(cases, function(ans) {
           tr <- function(body) list(answers = list(q = ans))
           z <- suppressWarnings(withr_options(Rjif.transport = tr,
                 jev_eval("s", list(q = jev_noul_q("x")))))
           !has_key_bytes(z)
         }, logical(1))) })
expect("R6-B1/N07: POSIXlt metadata is replaced cleanly (no names-length attribute error)",
       { lt <- as.POSIXlt(as.POSIXct(0.9, origin = "1970-01-01", tz = "UTC"))
         tr <- function(body) list(answers = list(q = list(type = "noul",
                                                           noul = 0.9, when = lt)))
         got <- withr_options(Rjif.transport = tr,
           tryCatch(jev_eval("s", list(q = jev_noul_q("x"))),
                    error = function(e) conditionMessage(e)))
         !(is.character(got) && grepl("names' attribute", got)) &&
           !has_key_bytes(got) })
expect("R6-B1: a secret-labelled factor keeps its rows (redacted label, not NA)",
       { fl <- factor(c("safe", fake_key), levels = c("safe", fake_key))
         tr <- function(body) list(answers = list(q = list(type = "noul",
                                                           noul = 0.9, obs = fl)))
         z <- suppressWarnings(withr_options(Rjif.transport = tr,
               jev_eval("s", list(q = jev_noul_q("x")))))
         f2 <- z$q$raw$obs
         !has_key_bytes(z) && !any(is.na(f2)) })
expect("R7/R8 supplemental: calibration column-name echo sites (.check_df_cols) never emit the LIVE key env var - all three message forms scrubbed",
       { old_k <- Sys.getenv("TYPESAFE_API_KEY", unset = NA)
         Sys.setenv(TYPESAFE_API_KEY = fake_key)
         on.exit({ if (is.na(old_k)) Sys.unsetenv("TYPESAFE_API_KEY") else
                     do.call(Sys.setenv, list(TYPESAFE_API_KEY = old_k)) })
         dfk <- data.frame(0.9, stringsAsFactors = FALSE)
         names(dfk)[1] <- fake_key
         msgs <- c(
           # requested column present, truth missing: Available: echoes the key
           tryCatch(Rjif::reliability_curve(dfk, "p", "y"),
                    error = function(e) conditionMessage(e)),
           # missing-column echo path with the key as the requested name
           tryCatch(Rjif::reliability_curve(data.frame(p = 0.5, y = 1,
                                 check.names = FALSE,
                                 stringsAsFactors = FALSE),
                      fake_key, "y"),
                    error = function(e) conditionMessage(e)),
           # both-echo path: key on both sides of the message
           tryCatch(Rjif::reliability_curve(dfk, fake_key, "y"),
                    error = function(e) conditionMessage(e)))
         all(vapply(msgs, function(m)
             is.character(m) && !grepl(fake_key, m, fixed = TRUE) &&
             !length(grepRaw(fake_key, serialize(m, NULL), fixed = TRUE)),
           logical(1))) })
expect("R9-B1: the DIRECT exported mock transport cannot echo the live key in its unknown-type diagnostic (and the wrapped control stays clean)",
       { old_k <- Sys.getenv("TYPESAFE_API_KEY", unset = NA)
         Sys.setenv(TYPESAFE_API_KEY = fake_key)
         on.exit({ if (is.na(old_k)) Sys.unsetenv("TYPESAFE_API_KEY") else
                     do.call(Sys.setenv, list(TYPESAFE_API_KEY = old_k)) })
         direct <- tryCatch(rjif_mock_transport(list(
                       state = "s",
                       questions = list(q = list(type = fake_key,
                                                 instructions = "x")))),
                     error = function(e) conditionMessage(e))
         wrapped <- tryCatch(
           withr_options(Rjif.transport = rjif_mock_transport,
             jev_eval("s", list(q = list(type = fake_key,
                                         instructions = "x")))),
           error = function(e) conditionMessage(e),
           warning = function(e) conditionMessage(e))
         cleans <- c(direct, wrapped)
         all(vapply(cleans, function(m)
             is.character(m) && !grepl(fake_key, m, fixed = TRUE) &&
             !length(grepRaw(fake_key, serialize(m, NULL), fixed = TRUE)),
           logical(1))) &&
           grepl("REDACTED", direct, fixed = TRUE) })
expect("R6-B2: duplicate question names in an error never echo the key",
       { msg <- withr_options(
           Rjif.transport = function(body) list(answers = list()),
           tryCatch(jev_eval("s", stats::setNames(list(jev_noul_q("e"),
                                                       jev_noul_q("e")),
                                                  c(fake_key, fake_key))),
                    error = function(e) conditionMessage(e)))
         is.character(msg) && !grepl(fake_key, msg, fixed = TRUE) })
expect("R6-B2: duplicate choice criteria names in an error never echo the key",
       { msg <- tryCatch(jev_choice_q("route",
                           stats::setNames(c("A", "B"), c(fake_key, fake_key))),
                         error = function(e) conditionMessage(e))
         is.character(msg) && !grepl(fake_key, msg, fixed = TRUE) })
expect("R5-B1 semantics after the attribute rework: collision keeps the SELECTED p (0.9), not a sibling's",
       { offered <- list("Bearer option_alpha" = "first",
                         "Bearer option_beta" = "second")
         tr <- function(body) list(answers = list(q = list(
           choice = "Bearer option_beta",
           confidence = 0.9,
           probabilities = stats::setNames(list(0.1, 0.9), names(offered)))))
         a <- suppressWarnings(withr_options(Rjif.transport = tr,
               jev_eval("s", list(q = jev_choice_q("i", offered)))$q))
         isTRUE(all.equal(jprob(a), 0.9)) &&
           !identical(jprob(a), 0.1) })
if (is.na(old_env_key)) Sys.unsetenv("TYPESAFE_API_KEY") else
  do.call(Sys.setenv, list(TYPESAFE_API_KEY = old_env_key))

# ===========================================================================
cat("\n17. external audit (Omen Codex) findings 2-5 regressions\n")
# --- finding 3: two-sided noul floor --------------------------------------
forged_noul <- function(p) function(body) list(
  model = "fixture", usage = list(input_tokens = 10L),
  answers = list(q = list(type = "noul", noul = p)))
expect("f3: a confident NO (p=0.01) decides FALSE under floor 0.7 (was NA)",
       withr_options(Rjif.transport = forged_noul(0.01),
                     isFALSE(jif("s", "x", confidence_floor = 0.7))))
expect("f3: floor-window middle abstains with the floor named in the reason",
       { d <- withr_options(Rjif.transport = forged_noul(0.5),
              jif("s", "x", confidence_floor = 0.7))
         isTRUE(jif_abstained(d)) &&
           grepl("two-sided confidence_floor 0.700", jif_reason(d),
                 fixed = TRUE) })
expect("f3: floor at or below threshold keeps single-sided behavior exactly",
       withr_options(Rjif.transport = forged_noul(0.4),
                     isFALSE(jif("s", "x", confidence_floor = 0.4))))
expect("f3: p=0.49 under floor 0.7 still abstains (the floor is not bypassed)",
       withr_options(Rjif.transport = forged_noul(0.49),
                     isTRUE(jif_abstained(jif("s", "x", confidence_floor = 0.7)))))
# r2 finding 4: exact negative cutoffs must be INCLUSIVE despite binary floats.
# 1-0.8 = 0.19999999999999996 and 1-0.9 = 0.09999999999999998, so p=0.2 (floor
# 0.8) and p=0.1 (floor 0.9) abstained on the pre-fix code; the .7/.3 pair
# passed only because 1-0.7 = 0.30000000000000004 lands above the cutoff.
expect("r2-4: exact negative cutoff p=0.2 under floor 0.8 decides FALSE (was NA)",
       withr_options(Rjif.transport = forged_noul(0.2),
                     isFALSE(jif("s", jev_noul_q("x"), confidence_floor = 0.8))))
expect("r2-4: exact negative cutoff p=0.1 under floor 0.9 decides FALSE (was NA)",
       withr_options(Rjif.transport = forged_noul(0.1),
                     isFALSE(jif("s", jev_noul_q("x"), confidence_floor = 0.9))))
expect("r2-4: the guard stays far below API granularity -- p=0.21/floor 0.8 still abstains",
       withr_options(Rjif.transport = forged_noul(0.21),
                     isTRUE(jif_abstained(jif("s", jev_noul_q("x"), confidence_floor = 0.8)))))
expect("r2-4: batch path shares the inclusive cutoff (jev_score_many p=0.2/floor 0.8 -> FALSE)",
       { b <- withr_options(Rjif.transport = forged_noul(0.2),
              jev_score_many("s1", jev_noul_q("x"), confidence_floor = 0.8))
         isFALSE(b$decision[[1]]) && !nzchar(b$error[[1]]) })
expect("f3: choice/score floors stay single-sided (1-p is not evidence there)",
       { tc <- function(body) list(answers = list(q = list(
           type = "choice", choice = "a", confidence = 0.4,
           probabilities = list(a = 0.4, b = 0.6))))
         withr_options(Rjif.transport = tc,
           isTRUE(jif_abstained(jif("s", jev_choice_q("i", c(a = "1", b = "2")),
                                    confidence_floor = 0.7)))) })
expect("f3: jif() and jev_score_many() agree on every window position",
       { st <- c(0.01, 0.29, 0.31, 0.5, 0.69, 0.71, 0.99)
         one <- vapply(st, function(p) withr_options(
           Rjif.transport = forged_noul(p),
           jif("s", "x", confidence_floor = 0.7, abstain = -99)), numeric(1))
         many <- local({
           i <- 0L
           tr <- function(body) { i <<- i + 1L
             list(model = "f", usage = list(input_tokens = 1L),
                  answers = list(q = list(type = "noul", noul = st[[i]]))) }
           d <- withr_options(Rjif.transport = tr,
             jev_score_many(rep("s", length(st)), "x", confidence_floor = 0.7))
           ifelse(is.na(d$decision), -99, d$decision) })
         identical(one, as.numeric(many)) })

# --- finding 2: probability-semantics gate + observed_rate rename ---------
score_frame <- local({
  tr <- function(body) list(answers = list(q = list(
    type = "score", score = 0.02, confidence = 1,
    probabilities = list("0" = 0.9998, "1" = 0.0001, "2" = 0.0001),
    legend = list("0" = "a", "1" = "b", "2" = "c"))))
   df <- withr_options(Rjif.transport = tr,
     jev_score_many(rep("narr", 30), jev_score_q("i", c("a", "b", "c")),
                    threshold = 1))
  df$truth <- 0
  df })
expect("f2: a score batch's p is REFUSED as an event probability by default",
       grepl("question_type is 'score'",
             err_msg(reliability_curve(score_frame)), fixed = TRUE))
expect("f2: ece() and selection_curve() share the same gate",
       throws(ece(score_frame)) && throws(selection_curve(score_frame)))
# r2 finding 3: with the two-sided policy live, the DEFAULT curve kept
# reporting one-sided coverage (fixture: p=.01/.99 at floor .7 -> policy
# decides BOTH rows, curve said 50%). The fix: an explicit policy argument
# that reuses the evaluator's window helper.
expect("r2-3: two_sided policy reports the evaluator's real coverage (was 0.5)",
       { d <- structure(data.frame(p = c(0.01, 0.99), truth = c(0, 1)),
                        question_type = "noul")
         sc1 <- selection_curve(d, floor_seq = 0.7)
         sc2 <- selection_curve(d, floor_seq = 0.7, policy = "two_sided")
         identical(sc1$coverage, 0.5) &&            # default view unchanged
           identical(sc2$coverage, 1.0) && identical(sc2$escalated, 0) &&
           identical(attr(sc2, "policy"), "two_sided") })
expect("r2-3: two_sided cutoffs are inclusive (p=0.2 exactly at floor 0.8 counts as decided)",
       { d <- structure(data.frame(p = 0.2, truth = 0), question_type = "noul")
         identical(selection_curve(d, floor_seq = 0.8, policy = "two_sided")$coverage, 1) })
expect("r2-3: two_sided refuses non-noul batches (1-p is not evidence for a named-option pick)",
       grepl("requires a noul batch",
             err_msg(selection_curve(structure(data.frame(p = c(0.2, 0.8), truth = c(0, 1)),
                                               question_type = "score"),
                                     policy = "two_sided"))))
expect("f2: allow_type = 'score' opts in WITH a warning naming the quantity",
       { w <- warn_msg(reliability_curve(score_frame, allow_type = "score"))
         rc <- suppressWarnings(reliability_curve(score_frame,
                                                  allow_type = "score"))
         grepl("concentration", w, fixed = TRUE) && attr(rc, "n_used") == 30L })
expect("f2: a noul batch and a hand-built frame pass ungated",
       { ok <- withr_options(Rjif.transport = forged_noul(0.9),
           jev_score_many(c("s", "s"), "x"))
         ok$truth <- c(1, 1)
         !throws(reliability_curve(ok)) &&
           !throws(reliability_curve(data.frame(p = c(0.2, 0.8),
                                                truth = c(0, 1)))) })
expect("f2: a choice batch is gated too (P(selected option) != P(event))",
       { tc <- function(body) list(answers = list(q = list(
           type = "choice", choice = "a", confidence = 0.9,
           probabilities = list(a = 0.9, b = 0.1))))
         d <- withr_options(Rjif.transport = tc,
           jev_score_many(rep("s", 3), jev_choice_q("i", c(a = "1", b = "2"))))
         d$truth <- c(1, 0, 0)
         throws(reliability_curve(d)) &&
           !throws(reliability_curve(d, allow_type = "choice")) })
expect("f2: the reliability table column is observed_rate; accuracy is GONE",
       { rc <- reliability_curve(data.frame(p = c(0.1, 0.9), truth = c(0, 1)),
                                 n_bins = 2L)
         identical(rc$observed_rate, c(0, 1)) && !("accuracy" %in% names(rc)) &&
           is.null(rc[["accuracy"]]) })

# --- finding 4: contract validators ---------------------------------------
expect("f4: choice answer contradicting its own argmax is rejected (probe 3)",
       { tr <- function(body) list(answers = list(q = list(
           type = "choice", choice = "a", confidence = 0.01,
           probabilities = list(a = 0.01, b = 0.99))))
         a <- suppressWarnings(withr_options(Rjif.transport = tr,
               jev_eval("s", list(q = jev_choice_q("i", c(a = "1", b = "2")))))$q)
         is.na(jvalue(a)) && grepl("trails the top option", a$contract,
                                   fixed = TRUE)
         # the same forged answer through jif() must abstain, not silently
         # route to the second option (a contract-violating winner is never
         # forwarded as a decision). is.na, not identical(NA): jif's abstain
         # default NA is a logical here for choice but NA_character_ for
         # score/jmatch paths, and an assertion must not pin that detail.
         { d <- suppressWarnings(withr_options(Rjif.transport = tr,
               jif("s", jev_choice_q("i", c(a = "1", b = "2")))))
           length(d) == 1L && is.na(d) } })
expect("r2-5: a strictly lower displayed winner is REJECTED (tolerance tightened to 0)",
       { tr <- function(body) list(answers = list(q = list(
           type = "choice", choice = "a", confidence = 0.5,
           probabilities = list(a = 0.49, b = 0.51))))
         a <- suppressWarnings(withr_options(Rjif.transport = tr,
               jev_eval("s", list(q = jev_choice_q("i", c(a = "1", b = "2")))))$q)
         is.na(jvalue(a)) && grepl("trails the top option", a$contract,
                                   fixed = TRUE) })
expect("r2-5: an exact displayed TIE on the maximum still decides (ties are legal argmaxes)",
       { tr <- function(body) list(answers = list(q = list(
           type = "choice", choice = "a", confidence = 0.5,
           probabilities = list(a = 0.5, b = 0.5))))
         a <- suppressWarnings(withr_options(Rjif.transport = tr,
               jev_eval("s", list(q = jev_choice_q("i", c(a = "1", b = "2")))))$q)
         identical(jvalue(a), "a") && is.na(a$contract) })
expect("f4: bare {score, confidence} without distribution/legend is rejected (probe 4)",
       { tr <- function(body) list(answers = list(q = list(
           type = "score", score = 2, confidence = 1)))
         a <- suppressWarnings(withr_options(Rjif.transport = tr,
               jev_eval("s", list(q = jev_score_q("i", c("a", "b", "c")))))$q)
         is.na(jvalue(a)) && grepl("no probability distribution", a$contract,
                                   fixed = TRUE) })
expect("f4: score legend keys must cover 0..k-1",
       { tr <- function(body) list(answers = list(q = list(
           type = "score", score = 2, confidence = 1,
           probabilities = list("0" = 0, "1" = 0, "2" = 1),
           legend = list("1" = "b", "2" = "c"))))
         a <- suppressWarnings(withr_options(Rjif.transport = tr,
               jev_eval("s", list(q = jev_score_q("i", c("a", "b", "c")))))$q)
         is.na(jvalue(a)) && grepl("legend keys", a$contract, fixed = TRUE) })
expect("f4: a score contradicting its own weighted mean is rejected",
       { tr <- function(body) list(answers = list(q = list(
           type = "score", score = 0.5, confidence = 1,
           probabilities = list("0" = 0, "1" = 0, "2" = 1),
           legend = list("0" = "a", "1" = "b", "2" = "c"))))
         a <- suppressWarnings(withr_options(Rjif.transport = tr,
               jev_eval("s", list(q = jev_score_q("i", c("a", "b", "c")))))$q)
         is.na(jvalue(a)) && grepl("contradicts", a$contract, fixed = TRUE) })
expect("f4: choice answer lacking confidence is rejected (required by docs)",
       { tr <- function(body) list(answers = list(q = list(
           type = "choice", choice = "a",
           probabilities = list(a = 1, b = 0))))
         a <- suppressWarnings(withr_options(Rjif.transport = tr,
               jev_eval("s", list(q = jev_choice_q("i", c(a = "1", b = "2")))))$q)
         is.na(jvalue(a)) && grepl("lacks a confidence", a$contract,
                                   fixed = TRUE) })
expect("f4: out-of-range request shapes are refused before transport",
       { hits <- character(0)
         tr <- function(body) { hits <<- c(hits, "CALLED")
           list(answers = list(q = list(type = "noul", noul = 0.5))) }
         e1 <- err_msg(withr_options(Rjif.transport = tr,
                        jev_score_q("i", "only-one-level")))
         e2 <- err_msg(withr_options(Rjif.transport = tr,
                        jev_score_q("i", paste0("L", 1:11))))
         e3 <- err_msg(withr_options(Rjif.transport = tr,
                        jev_choice_q("i", as.list(setNames(
                          rep("d", 256), paste0("opt", 1:256))))))
         e4 <- err_msg(withr_options(Rjif.transport = tr,
                        jev_choice_q("i", list(only = "x"))))
         length(hits) == 0L &&
           grepl("needs at least 2 levels", e1, fixed = TRUE) &&
           grepl("allows 10", e2, fixed = TRUE) &&
           grepl("allows 255", e3, fixed = TRUE) &&
           grepl("needs at least 2 options", e4, fixed = TRUE) })
expect("f4: jev_noul_q accepts documented true/false criteria and sends them",
       { sent <- NULL
         tr <- function(body) { sent <<- body
           list(answers = list(q = list(type = "noul", noul = 0.77))) }
         v <- withr_options(Rjif.transport = tr,
              jev_eval("s", list(q = jev_noul_q("x", criteria = list(
                true = "explicit present", false = "explicit absent")))))$q
         identical(sent$questions$q$criteria,
                   list(true = "explicit present", false = "explicit absent")) &&
           identical(jvalue(v), 0.77) })
expect("f4: bogus noul criteria names are refused",
       throws(jev_noul_q("x", criteria = list(yes = "y"))))
expect("f4: the true/false shorthand is noul criteria, NOT a 2-option choice",
       { q <- as_question("is this urgent", list(true = "fast", false = "slow"))
         q$type == "noul" && identical(q$criteria, list(true = "fast",
                                                        false = "slow")) })
expect("f4: structured list states reach the transport unharmed",
       { sent <- NULL
         tr <- function(body) { sent <<- body
           list(answers = list(q = list(type = "noul", noul = 0.5))) }
         st <- list(patient = list(age = 67L),
                    events = list(list(term = "FEVER", grade = 2L)))
         v <- withr_options(Rjif.transport = tr,
              jev_eval(st, list(q = jev_noul_q("x"))))
         identical(sent$state, st) && identical(names(sent$questions), "q") })
expect("f4: empty/nested-empty list states are refused locally",
       throws(jev_eval(list(), list(q = jev_noul_q("x")))))

# --- finding 5: retry policy + resumable cache ----------------------------
backoff_wait <- .pick(".backoff_wait")
expect("f5: backoff is exponential, capped, and never negative",
       { w <- vapply(1:8, function(a) backoff_wait(a, 1, 30, NULL), numeric(1))
         all(w >= 1 & w <= 30) && w[[2]] >= w[[1]] && w[[8]] == 30 })
expect("f5: a sane Retry-After overrides computed backoff",
       backoff_wait(1, 1, 30, "7") == 7 &&
         backoff_wait(1, 1, 30, "9999", max_wait = 120) == 120 &&
         # malformed/negative/NA headers fall back to computed backoff
         backoff_wait(1, 1, 30, "not an HTTP date") >= 1 &&
         backoff_wait(1, 1, 30, "-5") >= 1 &&
         backoff_wait(1, 1, 30, NA_character_) >= 1)
expect("f5: cache round-trips a full score_many frame byte-equivalently",
       local({
         cf <- tempfile(fileext = ".rds")
         on.exit(unlink(cf))
         tr <- function(body) list(model = "f",
           usage = list(input_tokens = 1L),
           answers = list(q = list(type = "noul", noul = 0.8)))
         a <- withr_options(Rjif.transport = tr,
              jev_score_many(c("s1", "s2", "s3"), "x", cache = cf))
         b <- withr_options(Rjif.transport = tr,
              jev_score_many(c("s1", "s2", "s3"), "x", cache = cf))
         calls_a <- jev_usage()$calls
         b_calls <- local({ n0 <- jev_usage()$calls
           invisible(b); jev_usage()$calls - n0 })
         identical(b[1:6], a[1:6]) && identical(attr(b, "n_resumed"), 3L)
       }))
expect("f5: a cache built for another question fingerprint is REFUSED (r2 finding 2)",
       local({
         cf <- tempfile(fileext = ".rds")
         on.exit(unlink(cf))
         tr1 <- function(body) list(model = "f", usage = list(input_tokens = 1L),
           answers = list(q = list(type = "noul", noul = 0.8)))
         invisible(withr_options(Rjif.transport = tr1,
           jev_score_many(c("s1", "s2"), "question A", cache = cf)))
         # Codex r2 finding 2: the old path warned and OVERWROTE the only prior
         # artifact (and re-billed everything). Mismatch now stops.
         e <- err_msg(withr_options(Rjif.transport = tr1,
           jev_score_many(c("s1", "s2"), "question B", cache = cf)))
         grepl("does not match", e, fixed = TRUE) &&
           grepl("NOT overwritten", e, fixed = TRUE)
       }))
expect("f5: cache digest binds to state CONTENT and order (r2 finding 2)",
       local({
         cf <- tempfile(fileext = ".rds")
         on.exit(unlink(cf))
         # state-sensitive fixture exactly like Codex's: present -> .99, absent -> .01
         sttr <- function(body) list(model = "f", usage = list(input_tokens = 1L),
           answers = list(q = list(type = "noul",
                                   noul = if (identical(body$state, "present")) 0.99 else 0.01)))
         n <- 0L
         cnt <- function(body) { n <<- n + 1L; sttr(body) }
         withr_options(Rjif.transport = cnt, {
           r1 <- jev_score_many(c("present", "absent"), "Q?", cache = cf)
           n1 <- n
           # SAME content, reordered: must NOT silently replay stale judgments
           e <- err_msg(jev_score_many(c("absent", "present"), "Q?", cache = cf))
           same_calls <- n == n1
           # identical vector again: replays for free
           r2 <- jev_score_many(c("present", "absent"), "Q?", cache = cf)
           # one corrected character: refused again
           e2 <- err_msg(jev_score_many(c("present", "absentt"), "Q?", cache = cf))
           all(unlist(r1$p) == c(0.99, 0.01)) &&
             grepl("does not match", e, fixed = TRUE) && same_calls &&
             identical(attr(r2, "n_resumed"), 2L) && n == n1 &&
             grepl("does not match", e2, fixed = TRUE)
         })
       }))
expect("f5: cache rows that previously ERRORED are re-run by default",
       local({
         cf <- tempfile(fileext = ".rds")
         on.exit(unlink(cf))
         n <- 0L
         tr <- function(body) {
           n <<- n + 1L
           if (n == 1L) stop("transient boom")
           list(model = "f", usage = list(input_tokens = 1L),
                answers = list(q = list(type = "noul", noul = 0.8)))
         }
         a <- suppressWarnings(withr_options(Rjif.transport = tr,
               jev_score_many("s1", "x", cache = cf)))
         n_after_first <- n
         b <- withr_options(Rjif.transport = tr,
               jev_score_many("s1", "x", cache = cf))
         nzchar(a$error[[1]]) && n_after_first == 1L && n == 2L &&
           isTRUE(b$decision[[1]]) && identical(attr(b, "n_resumed"), 0L)
       }))

# --- r3 finding R3-1: encoding-blind cache identity ------------------------
# R strings carry per-element encoding flags: identical stored BYTES can be
# different text (bytes c3 a9 flagged UTF-8 = e-acute; the same bytes flagged
# latin1 = A-tilde + copyright), and charToRaw() cannot see the flag. The v3
# digest therefore keyed two DIFFERENT payloads to one identity while
# toJSON() sent different JSON for each (audit probe: a latin1 run silently
# resumed the UTF-8 decision with zero new calls). Fix: canonicalise
# state_vec with enc2utf8() before both the digest and serialisation, reject
# invalid byte sequences up front, bump the fingerprint to version 4.
expect("r3-1: same bytes / different Unicode meaning are DISTINGUISHED (refusal, no false resume, cache bytes intact)",
       local({
         cf <- tempfile(fileext = ".rds"); on.exit(unlink(cf))
         u <- rawToChar(as.raw(c(0xc3, 0xa9))); Encoding(u) <- "UTF-8"
         l <- rawToChar(as.raw(c(0xc3, 0xa9))); Encoding(l) <- "latin1"
         stopifnot(!identical(enc2utf8(u), enc2utf8(l)))
         calls <- 0L
         # state-sensitive fixture like the audit's: UTF-8 e-acute -> .99,
         # the latin1 two-character string -> .01
         tr <- function(body) {
           calls <<- calls + 1L
           p <- if (identical(body$state, enc2utf8(u))) 0.99 else 0.01
           list(model = "f", usage = list(input_tokens = 1L),
                answers = list(q = list(type = "noul", noul = p)))
         }
         a <- withr_options(Rjif.transport = tr, jev_score_many(u, "x", cache = cf))
         before <- tools::md5sum(cf)
         e <- tryCatch({ withr_options(Rjif.transport = tr,
                        jev_score_many(l, "x", cache = cf)); "" },
                       error = conditionMessage)
         fresh <- withr_options(Rjif.transport = tr, jev_score_many(l, "x"))
         isTRUE(a$decision[[1]]) &&
           grepl("does not match", e, fixed = TRUE) &&
           identical(as.integer(calls), 2L) &&          # run1 + fresh run3; refusal makes no call
           identical(before, tools::md5sum(cf)) &&      # refusal left bytes intact
           identical(fresh$decision[[1]], FALSE)        # latin1 text scores on its own
       }))
expect("r3-1: same text / different declared encodings UNIFY across a resume",
       local({
         cf <- tempfile(fileext = ".rds"); on.exit(unlink(cf))
         u <- rawToChar(as.raw(c(0xc3, 0xa9))); Encoding(u) <- "UTF-8"  # "e-acute"
         m <- rawToChar(as.raw(0xe9));          Encoding(m) <- "latin1" # same char
         stopifnot(identical(enc2utf8(u), enc2utf8(m)))
         tr <- function(body) list(model = "f", usage = list(input_tokens = 1L),
           answers = list(q = list(type = "noul", noul = 0.8)))
         invisible(withr_options(Rjif.transport = tr, jev_score_many(u, "x", cache = cf)))
         n0 <- jev_usage()$calls
         b <- withr_options(Rjif.transport = tr, jev_score_many(m, "x", cache = cf))
         (jev_usage()$calls - n0) == 0L && identical(attr(b, "n_resumed"), 1L)
       }))
expect("r3-1: invalid byte sequences are rejected before any call",
       local({
         bad <- rawToChar(as.raw(c(0xed, 0xa0, 0x80)))  # surrogate half as raw
         Encoding(bad) <- "UTF-8"
         stopifnot(!validEnc(bad))
         reached <- FALSE
         tr <- function(body) { reached <<- TRUE; stop("must not be reached") }
         e <- tryCatch({ withr_options(Rjif.transport = tr,
                        jev_score_many(c("ok", bad), "x")); "" },
                       error = conditionMessage)
         grepl("invalid byte sequences", e, fixed = TRUE) && !reached
       }))
expect("r4-1: 'bytes'-marked states are rejected before digest, cache, or transport (both valid-looking and invalid bytes)",
       local({
         b <- rawToChar(as.raw(c(0xc3, 0xa9))); Encoding(b) <- "bytes"
         stopifnot(validEnc(b), Encoding(b) == "bytes")  # the bypass premises
         badb <- rawToChar(as.raw(c(0xed, 0xa0, 0x80))); Encoding(badb) <- "bytes"
         reached <- 0L
         tr <- function(body) { reached <<- reached + 1L
           list(model = "f", usage = list(input_tokens = 1L),
                answers = list(q = list(type = "noul", noul = 0.9))) }
         cf <- tempfile(fileext = ".rds"); on.exit(unlink(cf))
         # seed a cache with the UTF-8 interpretation (explicitly marked, not
         # locale-default), then try to resume with the bytes-marked string:
         # must error, zero new calls, cache bytes intact
         u8 <- local({s <- rawToChar(as.raw(c(0xc3,0xa9))); Encoding(s) <- "UTF-8"; s})
         seeded <- withr_options(Rjif.transport = tr, jev_score_many(u8, "x", cache = cf))
         calls0 <- reached; md5a <- tools::md5sum(cf)
         e1 <- tryCatch({ withr_options(Rjif.transport = tr,
                        jev_score_many(b, "x", cache = cf)); "" }, error = conditionMessage)
         e2 <- tryCatch({ withr_options(Rjif.transport = tr,
                        jev_score_many(c("ok", badb), "x")); "" }, error = conditionMessage)
         grepl("'bytes'-marked", e1, fixed = TRUE) &&
           grepl("'bytes'-marked", e2, fixed = TRUE) &&
           identical(as.integer(reached), as.integer(calls0)) &&
           identical(md5a, tools::md5sum(cf)) &&
           identical(as.integer(attr(seeded, "n_resumed")), 0L)  # seed run; the resume attempt must never produce a decision at all
       }))
expect("r3-1: an old-version cache identity is never trusted (forged on-disk; positive control resumes at the current version)",
       local({
         cf <- tempfile(fileext = ".rds"); on.exit(unlink(cf))
         calls <- 0L
         tr <- function(body) { calls <<- calls + 1L
           list(model = "f", usage = list(input_tokens = 1L),
                answers = list(q = list(type = "noul", noul = 0.8))) }
         invisible(withr_options(Rjif.transport = tr,
                        jev_score_many("s", "x", cache = cf)))
         stopifnot(calls == 1L)
         # Forge from the ON-DISK frame (complete, with row_failed), changing
         # ONLY the fingerprint version to a stale value (round 4, R4-2:
         # forging from the returned frame produced an always-invalid cache
         # that passed vacuously even on old code). The frame KEEPS the
         # current column shape, so the refusal can only come from the
         # version tag inside the identity -- the exact isolation test.
         # (Version numbers advance with each audit round; forge 8L -> 3L.)
         disk <- readRDS(cf)
         fp <- unserialize(attr(disk, "cache_fingerprint"))
         stopifnot(identical(fp$version, 8L))
         cur_version <- fp$version
         fp$version <- 3L
         old <- disk; attr(old, "cache_fingerprint") <- serialize(fp, NULL, version = 2)
         saveRDS(old, cf); md5a <- tools::md5sum(cf)
         e <- tryCatch({ withr_options(Rjif.transport = tr,
                        jev_score_many("s", "x", cache = cf)); "" },
                       error = conditionMessage)
         refused <- grepl("does not match", e, fixed = TRUE) &&
           calls == 1L && identical(md5a, tools::md5sum(cf))
         # positive control: the same frame restored to the current version
         # must RESUME (isolates version rejection from any shape error)
         back <- disk; attr(back, "cache_fingerprint") <-
           serialize(unserialize(attr(back, "cache_fingerprint")), NULL, version = 2)
         attr(back, "cache_fingerprint") <-
           serialize({ fp2 <- unserialize(attr(disk, "cache_fingerprint"))
                       fp2$version <- cur_version; fp2 }, NULL, version = 2)
         saveRDS(back, cf)
         r <- withr_options(Rjif.transport = tr, jev_score_many("s", "x", cache = cf))
         refused && calls == 1L && identical(attr(r, "n_resumed"), 1L)
       }))

expect("r3p: score batch keeps the EXACT score_value and full named distribution",
       local({
         # fixture obeys the score contract exactly: probabilities sum to 1
         # and the score IS the weighted mean (0*0 + 1*.90625 + 2*.09375 =
         # 1.09375). 1.09375 is exactly representable in binary floats, so
         # score_value can be asserted BIT-exact while `option` keeps the
         # 3-decimal display (a sqrt(2) fixture was the first attempt and
         # the contract validator correctly rejected it: no representable
         # probability triple has a non-dyadic weighted mean).
         tr <- function(body) list(model = "resolved-x", usage = list(input_tokens = 10L),
           answers = list(q = list(type = "score", score = 1.09375,
                                   confidence = 0.85,
             legend = stats::setNames(c("none", "mild", "severe"), c("0", "1", "2")),
             probabilities = list("0" = 0, "1" = 0.90625, "2" = 0.09375))))
         d <- withr_options(Rjif.transport = tr,
              jev_score_many("narr", jev_score_q("s", c("none", "mild", "severe"))))
         pb <- jprobs(d$probs_json[[1]])
         identical(d$score_value[[1]], 1.09375) &&
           identical(d$option[[1]], "1.094") &&
           identical(pb, c("0" = 0, "1" = 0.90625, "2" = 0.09375)) &&
           identical(jprobs(withr_options(Rjif.transport = tr,
              jev_eval("n", list(q = jev_score_q("s", c("none", "mild", "severe"))))$q)), pb)
       }))
expect("r3p: noul rows carry no distribution (probs_json NA, jprobs NULL)",
       local({
         tr <- function(body) list(model = "m", usage = list(input_tokens = 1L),
           answers = list(q = list(type = "noul", noul = 0.9)))
         d <- withr_options(Rjif.transport = tr, jev_score_many("s", "x"))
         is.na(d$probs_json[[1]]) && is.null(jprobs(d$probs_json[[1]])) &&
           is.na(d$score_value[[1]])
       }))
expect("r3p: provenance columns survive a cache resume (model+time persist, source flips)",
       local({
         cf <- tempfile(fileext = ".rds"); on.exit(unlink(cf))
         calls <- 0L
         tr <- function(body) { calls <<- calls + 1L
           list(model = "day-one-resolved", usage = list(input_tokens = 1L),
                answers = list(q = list(type = "noul", noul = 0.9))) }
         a <- withr_options(Rjif.transport = tr, jev_score_many(c("s1", "s2"), "x", cache = cf))
         stopifnot(calls == 2L)
         # the alias resolves DIFFERENTLY on the resume run: day-one rows must
         # keep day-one's returned model, not silently inherit day-two's
         tr2 <- function(body) { calls <<- calls + 1L
           list(model = "day-two-resolved", usage = list(input_tokens = 1L),
                answers = list(q = list(type = "noul", noul = 0.9))) }
         b <- withr_options(Rjif.transport = tr2, jev_score_many(c("s1", "s2"), "x", cache = cf))
         t <- a$evaluated_at[[1]]
         all(a$row_source == "api") && all(b$row_source == "cache") &&
           identical(b$model, a$model) && identical(b$evaluated_at, a$evaluated_at) &&
           identical(attr(b, "n_resumed"), 2L) && calls == 2L &&
           grepl("^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$", t)
       }))
expect("r3p: row_source is 'none' (not 'api') for NA states; model/evaluated_at NA there",
       local({
         tr <- function(body) list(model = "m", usage = list(input_tokens = 1L),
           answers = list(q = list(type = "noul", noul = 0.9)))
         d <- withr_options(Rjif.transport = tr, jev_score_many(c("ok", NA), "x"))
         identical(d$row_source[[2]], "none") && is.na(d$model[[2]]) &&
           is.na(d$evaluated_at[[2]]) && identical(d$row_source[[1]], "api") &&
           identical(d$model[[1]], "m")
       }))
expect("r3p: row_source 'none' survives a resume (never laundered into 'cache')",
       local({
         cf <- tempfile(fileext = ".rds"); on.exit(unlink(cf))
         calls <- 0L
         tr <- function(body) { calls <<- calls + 1L
           list(model = "m", usage = list(input_tokens = 1L),
                answers = list(q = list(type = "noul", noul = 0.9))) }
         a <- withr_options(Rjif.transport = tr,
              jev_score_many(c("s1", NA), "x", cache = cf))
         stopifnot(identical(a$row_source, c("api", "none")), calls == 1L)
         b <- withr_options(Rjif.transport = tr,
              jev_score_many(c("s1", NA), "x", cache = cf))
         # s1 is genuinely reused; the NA row was never attempted in EITHER
         # run. n_resumed counts s1 only -- a failed/NA row is never "filled"
         # (rerun-errors policy re-walks it, and it short-circuits without a
         # call, staying row_source "none").
         identical(b$row_source, c("cache", "none")) && calls == 1L &&
           identical(attr(b, "n_resumed"), 1L) &&
           is.na(b$model[[2]]) && identical(b$model[[1]], "m")
       }))
expect("r3p: jif()'s answer attribute carries requested AND returned model (alias drift visible)",
       local({
         tr <- function(body) list(model = "jev-2026-09-20-prod", usage = list(input_tokens = 1L),
           answers = list(q = list(type = "noul", noul = 0.9)))
         a <- withr_options(Rjif.transport = tr, jif("s", "q"))
         ans <- attr(a, "answer")
         identical(attr(ans, "model_requested"), "jev-latest") &&
           identical(attr(ans, "model_returned"), "jev-2026-09-20-prod") &&
           identical(withr_options(Rjif.transport = tr,
              attr(jev_eval("s", list(q = jev_noul_q("q")), model = "alias-a"), "model")),
                     "jev-2026-09-20-prod")
       }))
expect("r3p: a 'bytes'-marked state never reaches provenance stamping (rejected first)",
       local({
         b <- rawToChar(as.raw(c(0xc3, 0xa9))); Encoding(b) <- "bytes"
         reached <- 0L
         tr <- function(body) { reached <<- reached + 1L
           list(model = "m", usage = list(input_tokens = 1L),
                answers = list(q = list(type = "noul", noul = 0.9))) }
         e <- tryCatch({ withr_options(Rjif.transport = tr,
                        jev_score_many(b, "x")); "" }, error = conditionMessage)
         grepl("'bytes'-marked", e, fixed = TRUE) && reached == 0L
       }))

# ---- round-6 audit regressions (Astra BLOCK on 96c8f58) ----

expect("r6b1: attributes on the requested-model argument cannot smuggle strings into retention",
       local({
         # .scrub_secrets()/.redact_key() sanitize VALUES but preserve
         # attributes, and the recursive .redact_value ran BEFORE the
         # provenance attrs were attached -- so an attributes-carrying
         # `model` rode into answer objects, serialize() output, jif's
         # answer attr, and saved caches completely unsanitized. The fix
         # (.bare_char) strips every attribute off both provenance strings.
         # The sentinel here stands in for a bearer secret echoed as an
         # attribute (the fake key is set so the exact-substring redaction
         # path is also exercised, never as the smuggled payload itself).
         sentinel <- "R6B1FAKE_names_smuggler_9Qa7"
         keyish <- "sk-R6B1FAKEnested_9Qa7"
         bad_model <- structure("jev-latest", names = sentinel,
                                metadata = list(deep = keyish))
         tr <- function(body) list(model = "safe-returned",
                                   usage = list(input_tokens = 1L),
                                   answers = list(q = list(type = "noul", noul = 0.9)))
         o <- Sys.getenv("TYPESAFE_API_KEY"); Sys.setenv(TYPESAFE_API_KEY = "R6B1envkey_zzz")
         on.exit(Sys.setenv(TYPESAFE_API_KEY = o), add = TRUE)
         a <- withr_options(Rjif.transport = tr, jif("s", "q", model = bad_model))
         ans <- attr(a, "answer")
         mr <- attr(ans, "model_requested")
         # (1) the attribute is BARE: exactly one string, zero attributes
         bare_ok <- is.character(mr) && length(mr) == 1L && is.null(attributes(mr)) &&
           identical(unname(mr), "jev-latest") &&
           is.null(attributes(attr(ans, "model_returned")))
         # (2) the sentinels are absent from serialized retention: the jif
         #     result, its answer object, and a saved cache written through
         #     the same smuggled model argument
         cf <- tempfile(fileext = ".rds")
         d <- withr_options(Rjif.transport = tr,
              jev_score_many("s", jev_noul_q("q"), model = bad_model, cache = cf))
         need <- c(sentinel, keyish)
         scan_raw <- function(raw, needles) {
           txt <- rawToChar(raw[raw != as.raw(0)])
           any(vapply(needles, function(nd) grepl(nd, txt, fixed = TRUE), TRUE))
         }
         leak_ser <- any(need %in% c(mr, attr(ans, "model_returned"))) ||
           scan_raw(serialize(list(a, ans), NULL, version = 2), need)
         # R6b-B1: a COMPRESSED byte scan is false assurance -- the payload
         # is present once decompressed. Scan the cache CONTENT (readRDS ->
         # serialize), and pin the mechanism: the fingerprint must carry the
         # model BARE (value kept, attributes stripped).
         disk <- readRDS(cf)
         leak_cache <- scan_raw(serialize(disk, NULL, version = 2), need) ||
           scan_raw(readBin(cf, "raw", file.size(cf)), need)
         fp <- unserialize(attr(disk, "cache_fingerprint"))
         fp_bare <- identical(fp$model, "jev-latest") &&
           is.null(attributes(fp$model))
         bare_ok && fp_bare && !leak_ser && !leak_cache &&
           identical(d$model[[1L]], "safe-returned")
       }))

expect("r6b2: probs_json re-parses BIT-EXACTLY for non-dyadic normalized values, shuffled names, Score and Choice",
       local({
         # jsonlite 2.0.0's digits = NA caps at 15 significant digits, so a
         # normalized distribution like {2:.55, 0:.11, 1:.33} (sum .99, a
         # VALID contract response -- the score is the weighted mean of the
         # NORMALIZED values) lost bits through the column. The dyadic
         # 0.90625 fixture cannot see this. Fix: digits = 17, bit-exact for
         # every binary64 value by construction.
         vscore <- c("2" = 0.55, "0" = 0.11, "1" = 0.33)
         s <- sum(vscore); norm <- vscore / s
         wm <- sum(norm * as.integer(names(norm)))
         tr_s <- function(body) list(model = "m", usage = list(input_tokens = 1L),
           answers = list(q = list(type = "score", score = wm, confidence = 0.85,
             legend = stats::setNames(c("a", "b", "c"), c("0", "1", "2")),
             probabilities = as.list(vscore))))
         d1 <- withr_options(Rjif.transport = tr_s,
              jev_score_many("s", jev_score_q("x", c("a", "b", "c"))))
         a1 <- withr_options(Rjif.transport = tr_s,
              jev_eval("s", list(q = jev_score_q("x", c("a", "b", "c")))))$q
         pb_json <- jprobs(d1$probs_json[[1]])
         pb_ans <- jprobs(a1)
         # the validator normalizes the displayed values and keeps the
         # vendor's key ORDER in the answer (names carry the level indices,
         # so display order is not semantics); compare as name-keyed sets
         expect_s <- sort(norm, decreasing = TRUE)  # 2,1,0 by value -- order
         expect_s <- expect_s[c("0", "1", "2")]     # irrelevant, index by name
         score_ok <- identical(pb_json[names(expect_s)], expect_s) &&
           identical(pb_ans[names(expect_s)], expect_s) &&
           identical(names(pb_json), names(pb_ans)) &&
           identical(pb_json, pb_ans)
         vc <- c("b" = 0.33, "a" = 0.66)
         tr_c <- function(body) list(model = "m", usage = list(input_tokens = 1L),
           answers = list(q = list(type = "choice", choice = "a", confidence = 0.9,
             probabilities = as.list(vc))))
         d2 <- withr_options(Rjif.transport = tr_c,
              jev_score_many("s", jev_choice_q("x", list(a = "first", b = "second"))))
         pc <- jprobs(d2$probs_json[[1]])
         # Choice keeps the vendor's key ORDER (probs built from the
         # displayed list), only the VALUES are normalized
         expect_c <- stats::setNames(unname(vc / sum(vc)), names(vc))
         choice_ok <- identical(pc, expect_c)
         score_ok && choice_ok
       }))

expect("r6b3: a response with no usable model identifier yields NA, never the requested alias",
       local({
         # the vendor not answering "which model?" must stay visibly unknown:
         # model_returned and the frame's model column go NA for missing,
         # NULL, blank, multi-element, and non-character envelopes, while
         # model_requested keeps the alias and the ANSWER is still accepted
         # (unknown provenance is not a contract violation).
         tr_missing <- function(body) list(usage = list(input_tokens = 1L),
           answers = list(q = list(type = "noul", noul = 0.9)))
         d <- withr_options(Rjif.transport = tr_missing, jev_score_many("s", "x"))
         missing_ok <- is.na(d$model[[1L]]) && identical(d$decision[[1L]], TRUE) &&
           identical(d$row_source[[1L]], "api")
         a <- withr_options(Rjif.transport = tr_missing,
              jev_eval("s", list(q = jev_noul_q("q"))))$q
         attr_ok <- is.na(attr(a, "model_returned")) &&
           identical(attr(a, "model_requested"), "jev-latest")
         blank <- function(body) c(tr_missing(body), list(model = "   "))
         vec <- function(body) c(tr_missing(body), list(model = c("m1", "m2")))
         num <- function(body) c(tr_missing(body), list(model = 42L))
         lst <- function(body) c(tr_missing(body), list(model = list(name = "m1")))
         others_ok <- all(vapply(list(blank, vec, num, lst), function(f) {
           dd <- withr_options(Rjif.transport = f, jev_score_many("s", "x"))
           is.na(dd$model[[1L]]) && identical(dd$decision[[1L]], TRUE)
         }, TRUE))
         # and a normal string model still flows through untouched
         plain <- withr_options(Rjif.transport = tr_missing,
                   jev_eval("s", list(q = jev_noul_q("q")), model = "alias-a"))
         ident_ok <- identical(attr(plain, "model"), NA_character_)
         missing_ok && attr_ok && others_ok && ident_ok
       }))

expect("r6b2 collision: redaction-merged Choice labels give ONE consistent distribution, fresh and resumed",
       local({
         # R6b-B2: a valid Choice whose distinct labels both contain a
         # "Bearer <token>" pattern collapses to the same redacted name at
         # the display boundary. A JSON object cannot store duplicate keys,
         # so the column now holds the distribution as {"p": [[name, value],
         # ...]} pairs (r6c fix: the earlier make.unique renaming was itself
         # defeated by adversarial labels). Names are stored EXACTLY,
         # duplicates and all, so jprobs(answer) and jprobs(probs_json) are
         # identical by construction; the frame's p stays the authoritative
         # selected value, bound before redaction.
         labels <- c("Bearer option_alpha", "Bearer option_beta")
         q <- jev_choice_q("x", stats::setNames(list("d1", "d2"), labels))
         tr <- function(body) list(model = "m", usage = list(input_tokens = 1L),
           answers = list(q = list(type = "choice", choice = labels[[2L]],
             confidence = 0.9,
             probabilities = stats::setNames(list(0.1, 0.9), labels))))
         cf <- tempfile(fileext = ".rds"); on.exit(unlink(cf))
         d <- withr_options(Rjif.transport = tr, jev_score_many("n", q, cache = cf))
         a <- withr_options(Rjif.transport = tr, jev_eval("n", list(q = q)))$q
         pj <- jprobs(d$probs_json[[1]]); pa <- jprobs(a)
         # both names are the SAME merged label by design (duplication is
         # honest here; inventing ".1" was the old defect)
         same_len <- length(pj) == 2L && identical(unname(pj), c(0.1, 0.9)) &&
           identical(names(pj), names(pa))
         decision_ok <- identical(d$decision[[1L]], FALSE) || !is.na(d$decision[[1L]])
         # resume must reproduce the SAME names and values (the defective
         # JSON used to persist through the cache)
         calls <- 0L
         tr2 <- function(body) { calls <<- calls + 1L; tr(body) }
         r <- withr_options(Rjif.transport = tr2, jev_score_many("n", q, cache = cf))
         resumed <- identical(r$probs_json[[1]], d$probs_json[[1]]) &&
           identical(jprobs(r$probs_json[[1]]), pj) && calls == 0L
         identical(pj, pa) && same_len && decision_ok && resumed
       }))

expect("r6c adversarial labels: an existing '.1'-suffixed label cannot break name agreement",
       local({
         # R6c-B2's exact fixture: one label literally contains the
         # redaction marker + ".1", two more merge into it after redaction.
         # make.unique output a DUPLICATE for this input and jsonlite then
         # re-suffixed the column side differently -- the pair encoding has
         # no renaming step to defeat.
         labs <- c("\u9009\u9879 Bearer [REDACTED].1",
                   "\u9009\u9879 Bearer option_alpha",
                   "\u9009\u9879 Bearer option_beta")
         q <- jev_choice_q("x", stats::setNames(list("d1", "d2", "d3"), labs))
         tr <- function(body) list(model = "m", usage = list(input_tokens = 1L),
           answers = list(q = list(type = "choice", choice = labs[[3L]],
             confidence = 0.9,
             probabilities = stats::setNames(list(0.1, 0.2, 0.7), labs))))
         cf <- tempfile(fileext = ".rds"); on.exit(unlink(cf))
         d <- withr_options(Rjif.transport = tr, jev_score_many("n", q, cache = cf))
         a <- withr_options(Rjif.transport = tr, jev_eval("n", list(q = q)))$q
         pj <- jprobs(d$probs_json[[1]]); pa <- jprobs(a)
         agree <- identical(pj, pa) && identical(unname(pj), c(0.1, 0.2, 0.7)) &&
           identical(names(pj), names(pa))
         p_ok <- identical(d$p[[1L]], 0.7)  # selected value bound pre-redaction
         resumed <- identical(withr_options(Rjif.transport = tr,
                       jev_score_many("n", q, cache = cf))$probs_json[[1L]],
                     d$probs_json[[1L]])
         agree && p_ok && resumed
       }))

expect("r6c-B1 model identity: two redaction-merged aliases never share a cache",
       local({
         # R6c-B1: .bare_char() on the fingerprint's readable model entry
         # merged "Bearer option_alpha" and "Bearer option_beta" into one
         # resume identity, so the second alias silently picked up the
         # FIRST alias's cached decisions with zero calls. The fingerprint
         # now carries model_id = digest of the model as sent; the readable
         # entry stays scrubbed for display but is not the identity.
         seen <- 0L
         tr1 <- function(body) { seen <<- seen + 1L
           list(model = "m", usage = list(input_tokens = 1L),
                answers = list(q = list(type = "noul", noul = if (seen == 1L) 0.9 else 0.1))) }
         cf <- tempfile(fileext = ".rds"); on.exit(unlink(cf))
         d1 <- withr_options(Rjif.transport = tr1,
                jev_score_many("s", "x", model = "Bearer option_alpha", cache = cf))
         e <- tryCatch({ withr_options(Rjif.transport = tr1,
                          jev_score_many("s", "x", model = "Bearer option_beta", cache = cf))
                         "" },
                       error = conditionMessage)
         # the second, DIFFERENT alias must REFUSE the first's cache (model
         # differs) rather than resume it: error text or fresh call, never
         # n_resumed == 1 with the first alias's decision
         refused <- grepl("does not match", e, fixed = TRUE)
         # and the readable fingerprint entry is still the SCRUBBED string
         disk <- readRDS(cf)
         fp <- unserialize(attr(disk, "cache_fingerprint"))
         clean <- grepl("[REDACTED]", fp$model, fixed = TRUE) &&
           is.null(attributes(fp$model)) &&
           identical(fp$model_id$digest,
                     Rjif:::.model_identity("Bearer option_alpha")$digest)
         identical(d1$model[[1L]], "m") && refused && clean
       }))

expect("r6d-B1 model identity: same BYTES with different declared encodings are different models",
       local({
         # R6d-B1: charToRaw digests a UTF-8-flagged and a latin1-flagged
         # copy of bytes c3 a9 identically, but jsonlite serialises them to
         # DIFFERENT wire bodies (c3 a9 vs c3 83 c2 a9) -- they are distinct
         # models. The identity digest must match the transport conversion
         # (enc2utf8), not the raw bytes, or redaction collapsing the
         # readable entry lets alias y silently resume alias x's cache.
         x <- rawToChar(as.raw(c(0xc3, 0xa9))); Encoding(x) <- "UTF-8"
         y <- x; Encoding(y) <- "latin1"
         wire <- function(m) as.character(jsonlite::toJSON(list(model = m),
                                                           auto_unbox = TRUE))
         seen <- 0L
         tr <- function(body) { seen <<- seen + 1L
           list(model = "m", usage = list(input_tokens = 1L),
                answers = list(q = list(type = "noul",
                  noul = if (identical(wire(body$model), wire(x))) 0.9 else 0.1))) }
         q <- jev_noul_q("s")
         cf <- tempfile(fileext = ".rds")
         old_key <- Sys.getenv("TYPESAFE_API_KEY", unset = NA_character_)
         on.exit(if (is.na(old_key)) Sys.unsetenv("TYPESAFE_API_KEY")
                 else Sys.setenv(TYPESAFE_API_KEY = old_key), add = TRUE)
         Sys.setenv(TYPESAFE_API_KEY = x)
         a <- withr_options(Rjif.transport = tr,
                            jev_score_many("s", q, model = x, cache = cf))
         Sys.setenv(TYPESAFE_API_KEY = y)
         refused <- FALSE
         r <- tryCatch(withr_options(Rjif.transport = tr,
                         jev_score_many("s", q, model = y, cache = cf)),
                       error = function(e) { refused <<- TRUE; NULL })
         # y's own fresh run must return the fresh decision
         fresh <- withr_options(Rjif.transport = tr,
                                jev_score_many("s", q, model = y))
         # x must STILL resume its own cache (zero new calls after x's fill)
         Sys.setenv(TYPESAFE_API_KEY = x)
         a2 <- withr_options(Rjif.transport = tr,
                  jev_score_many("s", q, model = x, cache = cf))
         digests_differ <- !identical(Rjif:::.model_identity(x)$digest,
                                      Rjif:::.model_identity(y)$digest)
         fp <- unserialize(attr(readRDS(cf), "cache_fingerprint"))
         readable <- identical(fp$model, "[REDACTED-API-KEY]") &&
           is.null(attributes(fp$model))
         refused && isTRUE(fresh$p[[1L]] == 0.1) &&
           isTRUE(attr(a2, "n_resumed") == 1L) && isTRUE(a2$p[[1L]] == 0.9) &&
           digests_differ && readable
       }))

expect("r6d-m1 pair decode: no-names and all-NA-names round-trip distinctly",
       local({
         # R6d-m1 / R6e-m1: a pair array with missing names must carry ONE
         # honest syntax end to end. Round 6d's version of this test FAILED
         # to catch the real defect because it hand-wrote a null-name column
         # instead of calling the writer -- the writer was emitting {} (R
         # NULL under jsonlite's default null policy), which fell through
         # the pair decoder into the legacy object path and surfaced the
         # "named" flag as a third probability entry. Lesson pinned here:
         # exercise .probs_column_json, never a lookalike fixture.
         ap_raw <- c(0.2, 0.8)
         ap_na <- stats::setNames(c(0.2, 0.8), c(NA_character_, NA_character_))
         col_raw <- Rjif:::.probs_column_json(ap_raw)
         col_na <- Rjif:::.probs_column_json(ap_na)
         a_raw <- structure(list(probs = ap_raw), class = "jev_answer")
         a_na <- structure(list(probs = ap_na), class = "jev_answer")
         j_raw <- jprobs(col_raw); j_na <- jprobs(col_na)
         # writer emits JSON null names for BOTH shapes (one syntax) and
         # the named:false flag only for the truly unnamed one
         syntax_ok <- grepl("[[null,0.20000000000000001]", col_raw, fixed = TRUE) &&
           grepl('"named":false', col_raw, fixed = TRUE) &&
           !grepl('"named"', col_na, fixed = TRUE) &&
           !grepl("{}", col_raw, fixed = TRUE)
         # decoder restores the two shapes distinctly AND identically to
         # the answer side
         agree <- identical(j_raw, jprobs(a_raw)) && identical(j_na, jprobs(a_na)) &&
           is.null(names(j_raw)) &&
           identical(names(j_na), c(NA_character_, NA_character_)) &&
           !identical(j_raw, j_na) && identical(unname(j_raw), c(0.2, 0.8))
         syntax_ok && agree
       }))

expect("r6e-B1 bytes-marked model is rejected before any cache lookup or call",
       local({
         # R6e-B1: enc2utf8() preserves an Encoding=="bytes" flag without
         # converting, so charToRaw digested a bytes-marked model
         # identically to the UTF-8-marked copy of the same bytes -- yet
         # jsonlite REFUSES to serialize bytes-marked strings, so such a
         # model can never go on the wire. It must not obtain a cached
         # decision either (fail closed, like the state path).
         x <- rawToChar(as.raw(c(0xc3, 0xa9))); Encoding(x) <- "UTF-8"
         y <- x; Encoding(y) <- "bytes"
         seen <- 0L
         tr <- function(body) { seen <<- seen + 1L
           jsonlite::toJSON(body, auto_unbox = TRUE, null = "null")
           list(model = "m", usage = list(input_tokens = 1L),
                answers = list(q = list(type = "noul", noul = 0.9))) }
         q <- jev_noul_q("s")
         cf <- tempfile(fileext = ".rds")
         a <- withr_options(Rjif.transport = tr,
                            jev_score_many("s", q, model = x, cache = cf))
         before <- tools::md5sum(cf)
         refused <- FALSE
         msg <- tryCatch({ withr_options(Rjif.transport = tr,
                             jev_score_many("s", q, model = y, cache = cf))
                           "" },
                         error = function(e) { refused <<- TRUE
                           conditionMessage(e) })
         after <- tools::md5sum(cf)
         # also refused WITHOUT a cache, before any transport call
         refused_nocache <- FALSE
         tryCatch(withr_options(Rjif.transport = tr,
                    jev_score_many("s", q, model = y)),
                  error = function(e) refused_nocache <<- TRUE)
         refused && refused_nocache && identical(before, after) &&
           grepl("bytes", msg, fixed = TRUE) && seen == 1L &&
           isTRUE(a$p[[1L]] == 0.9)
       }))

expect("r6e-m2 question identity is encoding-canonicalized, locale change refuses",
       local({
         # R6e-m2 (inherited v4): serialize(unclass(q)) stores native-flagged
         # strings as locale-blind BYTES, so the same question filled in one
         # LC_CTYPE could resume its cache in another even though jsonlite
         # would now post different words. .question_identity stores
         # enc2utf8 text, so declared-encoding differences on the question
         # now produce DIFFERENT fingerprints (refusing), while same-input
         # resume stays stable.
         s_latin <- rawToChar(as.raw(c(0xc3, 0xa9))); Encoding(s_latin) <- "latin1"
         s_utf8 <- rawToChar(as.raw(c(0xc3, 0xa9))); Encoding(s_utf8) <- "UTF-8"
         qi <- Rjif:::.question_identity
         differs <- !identical(qi(jev_noul_q(s_latin)), qi(jev_noul_q(s_utf8)))
         # the jsonlite wire agrees with the fingerprint on WHICH questions
         # are distinct: the two declared encodings post different bodies
         wire <- function(q) as.character(jsonlite::toJSON(list(q = unclass(q)),
                                                           auto_unbox = TRUE))
         wire_differs <- !identical(wire(jev_noul_q(s_latin)),
                                    wire(jev_noul_q(s_utf8)))
         stable <- identical(qi(jev_noul_q(s_latin))$digest,
                             qi(jev_noul_q(s_latin))$digest)
         # criteria NAMES are wire text too (they become JSON keys): the
         # same name bytes under two declared encodings render to
         # different keys, so the fingerprints must differ. Under the
         # digest design there is no rebuilt object to inspect -- identity
         # IS the wire bytes, proven against the transport's own options.
         b <- rawToChar(as.raw(c(0xc3, 0xa9)))
         n_latin <- jev_choice_q("x", stats::setNames(list("d1", "d2"),
                                                      c(s_latin, "z")))
         n_utf8 <- jev_choice_q("x", stats::setNames(list("d1", "d2"),
                                                     c(s_utf8, "z")))
         names_ok <- !identical(qi(n_latin)$digest, qi(n_utf8)$digest) &&
           # byte-exactness against the REAL transport render: the digest
           # equals md5 of the exact body the request would carry
           identical(qi(n_utf8)$digest, {
             body <- list(q = unclass(n_utf8))
             paste(format(openssl::md5(charToRaw(enc2utf8(
               as.character(do.call(jsonlite::toJSON,
                                   c(list(body), Rjif:::JSON_OPTS))))))),
                   collapse = "")
           })
         # end-to-end: two flags of the same BYTES on a NON-ASCII question
         # must not share a cache even when redaction collapses the readable
         # model entry -- the question entry itself now distinguishes them
         seen <- 0L
         tr <- function(body) { seen <<- seen + 1L
           list(model = "m", usage = list(input_tokens = 1L),
                answers = list(q = list(type = "noul", noul = if (seen == 1L) 0.9 else 0.1))) }
         cf <- tempfile(fileext = ".rds")
         old_key <- Sys.getenv("TYPESAFE_API_KEY", unset = NA_character_)
         on.exit(if (is.na(old_key)) Sys.unsetenv("TYPESAFE_API_KEY")
                 else Sys.setenv(TYPESAFE_API_KEY = old_key), add = TRUE)
         Sys.setenv(TYPESAFE_API_KEY = "Bearer fixture-key")
         qa <- jev_noul_q(s_latin); qb <- jev_noul_q(s_utf8)
         a1 <- withr_options(Rjif.transport = tr, jev_score_many("s", qa, cache = cf))
         refused <- FALSE
         tryCatch(withr_options(Rjif.transport = tr,
                    jev_score_many("s", qb, cache = cf)),
                  error = function(e) refused <<- TRUE)
         differs && wire_differs && stable && names_ok && refused &&
           seen == 1L
       }))

expect("r6e-m2 question identity: bytes-marked question text fails closed, never digests",
       local({
         # R6e-B1's class, re-checked for the M2 fix itself (author audit
         # after the round-6e report): validEnc() is TRUE for Encoding ==
         # "bytes" and enc2utf8() preserves the flag, so a bytes-marked
         # question element would have digested identically to the
         # UTF-8-marked copy of its bytes while jsonlite refuses to
         # serialize it at all. The question identity now rejects such text
         # BEFORE any cache lookup or transport call.
         b <- rawToChar(as.raw(c(0xc3, 0xa9))); Encoding(b) <- "bytes"
         qi <- Rjif:::.question_identity
         e1 <- tryCatch({ qi(jev_noul_q(b)); "" }, error = conditionMessage)
         e2 <- tryCatch({ qi(jev_choice_q("x",
                       stats::setNames(list("d1", "d2"), c(b, "z")))); "" },
                     error = conditionMessage)
         grepl("bytes", e1, fixed = TRUE) && grepl("bytes", e2, fixed = TRUE) &&
           # end-to-end: no transport call, cache untouched
           identical(local({
             seen <- 0L
             tr <- function(body) { seen <<- seen + 1L
               list(model = "m", usage = list(input_tokens = 1L),
                    answers = list(q = list(type = "noul", noul = 0.9))) }
             cf <- tempfile(fileext = ".rds")
             withr_options(Rjif.transport = tr,
                           jev_score_many("s", jev_noul_q("plain"), cache = cf))
             before <- tools::md5sum(cf)
             refused <- FALSE
             tryCatch(withr_options(Rjif.transport = tr,
                        jev_score_many("s", jev_noul_q(b), cache = cf)),
                      error = function(e) refused <<- TRUE)
             refused && seen == 1L && identical(before, tools::md5sum(cf))
           }), TRUE)
       }))

expect("r6e gate: model must be a single non-NA string; never a wire shape the API cannot take",
       local({
         # Author self-audit after round 6e: before the gate, jev_eval sent
         # model=NULL as {"model":{}} and model=5 as {"model":5}, and the
         # batch cache identity merged NULL with the empty LIST
         # (charToRaw digests both as nothing) while the wire distinguishes
         # {} and []. The gate rejects every non-string, NA, multi-element,
         # bytes-marked, and invalid-byte model BEFORE any transport call;
         # strings flow through untouched (r6b3's vendor-side envelopes are
         # unaffected -- that finding is about the RESPONSE model, this is
         # about the REQUEST model).
         reached <- 0L
         tr <- function(body) { reached <<- reached + 1L
           list(model = "m", usage = list(input_tokens = 1L),
                answers = list(q = list(type = "noul", noul = 0.9))) }
         b <- rawToChar(as.raw(c(0xc3, 0xa9))); Encoding(b) <- "bytes"
         bad <- list(NULL, 5L, c("a", "b"), NA, NA_character_, "", list(),
                     list(name = "m"), b, rawToChar(as.raw(c(0xff, 0xfe))))
         refused <- vapply(bad, function(m) {
           e <- tryCatch({ withr_options(Rjif.transport = tr,
                             jev_eval("s", list(q = jev_noul_q("q")), model = m))
                           "" },
                         error = conditionMessage)
           nzchar(e)
         }, logical(1))
         # the cache path refuses too, before any call
         batch_refused <- local({
           seen <- 0L
           tr2 <- function(body) { seen <<- seen + 1L
             list(model = "m", usage = list(input_tokens = 1L),
                  answers = list(q = list(type = "noul", noul = 0.9))) }
           e <- tryCatch({ withr_options(Rjif.transport = tr2,
                             jev_score_many("s", "x", model = NULL)); "" },
                         error = conditionMessage)
           nzchar(e) && seen == 0L
         })
         # valid strings untouched (the r6b3 fixtures above prove the
         # RESPONSE side; REQUEST-side strings must keep flowing)
         ok <- withr_options(Rjif.transport = tr,
                             jev_eval("s", list(q = jev_noul_q("q")),
                                      model = "alias-a"))
         all(refused) && batch_refused && reached >= 1L
       }))

expect("r6f-B1/B2/n1: cache identity IS the serializer's output; classed/dim/factor forms cannot resume or reach the wire",
       local({
         # Astra round 6f. B1: .question_identity (a hand-canonicalized
         # rebuild) dropped nested CLASSES jsonlite dispatches on, so a
         # never-sendable question (class r6f_unknown on a criterion)
         # resumed a valid question's 0.9 with zero calls. B2: the model
         # gate let is.character() TRUE oddities through -- I("m") posts
         # ["m"], matrix("m",1,1) posts [["m"]], an unknown class cannot
         # post -- all digested equal to plain "m" and resumed its cache.
         # n1: factor LEVELS are wire text the R walk never touched.
         # Closure: identity digests the transport's OWN rendered bytes;
         # gates reject non-plain-class model scalars up front.
         n <- 0L
         tr <- function(body) { n <<- n + 1L
           list(model = "m", usage = list(input_tokens = 1L),
                answers = list(q = list(type = "noul", noul = 0.9))) }
         # B1 end-to-end
         qa <- jev_noul_q("plain"); qb <- qa
         qb$extra <- list(structure(list(x = 1), class = "r6f_unknown"))
         cf <- tempfile(fileext = ".rds")
         withr_options(Rjif.transport = tr, jev_score_many("s", qa, cache = cf))
         n0 <- n
         md5_before <- tools::md5sum(cf)
         e1 <- tryCatch({ withr_options(Rjif.transport = tr,
                             jev_score_many("s", qb, cache = cf)); "" },
                        error = conditionMessage)
         # the auditor's acceptance criterion: refuse before any call AND
         # leave the cache byte-intact (the pre-fix defect rewrote it)
         b1 <- grepl("cannot be serialized", e1, fixed = TRUE) && n == n0 &&
           identical(md5_before, tools::md5sum(cf))
         # B2 end-to-end: three wire-shape oddities refuse before calls
         refused <- vapply(list(I("m"), matrix("m", 1, 1),
                                structure("m", class = "r6f_unknown")),
             function(v) {
               n1 <- n
               e <- tryCatch({ withr_options(Rjif.transport = tr,
                                  jev_score_many("s", qa, model = v, cache = cf)); "" },
                             error = conditionMessage)
               grepl("plain character scalar", e, fixed = TRUE) && n == n1
             }, logical(1))
         # fresh jev_eval gate too (no cache involved)
         e2 <- tryCatch({ withr_options(Rjif.transport = tr,
                             jev_eval("s", list(q = qa), model = I("m"))); "" },
                        error = conditionMessage)
         # harmless attributes still survive (r6b smuggling contract kept)
         smuggled <- structure("m", names = "k", metadata = list(a = 1L))
         n2 <- n
         ok_attr <- tryCatch({ withr_options(Rjif.transport = tr,
                 jev_score_many("s", qa, model = smuggled,
                                cache = tempfile(fileext = ".rds"))); TRUE },
               error = function(e) FALSE) && n > n2
         # n1: factor levels render through the serializer, so the
         # identity is locale-blind BY CONSTRUCTION (digest of UTF-8
         # output), and unknown factor classes still fail closed
         f <- structure(1L, levels = "café", class = "factor")
         qf <- qa; qf$criteria <- list(true = f)
         d_f <- tryCatch({ Rjif:::.question_identity(qf); TRUE },
                         error = function(e) FALSE)
         b1 && all(refused) && grepl("plain character scalar", e2, fixed = TRUE) &&
           ok_attr && d_f
       }))

expect("r6f-m1: serializer-based digests separate what the wire separates and merge only true twins",
       local({
         # Astra round 6f minor 3: my dcd91c8 comments claimed NULL, "",
         # whitespace and list() shared ONE cache identity and that NULL
         # serialized as {}; both claims were wrong (the transport uses
         # null = "null", and .state_digest's length markers separate
         # those arguments). Fixed in the same commit's comments/NEWS.
         # Under the r6f design the facts are cleaner: identity is the
         # digest of the rendered JSON, so two arguments share a digest
         # EXACTLY when the wire cannot tell them apart (character(0) and
         # list() both post []) and differ whenever the wire differs
         # (NULL -> null, "" -> "", " " -> " "). This test pins THAT
         # correspondence -- identity == wire, no exceptions.
         mid <- function(v) Rjif:::.model_identity(v)$digest
         wire <- function(v) as.character(jsonlite::toJSON(list(model = v),
                                auto_unbox = TRUE, null = "null"))
         vs <- list(NULL, "", " ", character(0), list())
         # equal digest <=> equal wire bytes (plain loops: vapply over a
         # data frame iterates COLUMNS, ix$a is invalid -- my first
         # version of this test failed on its own harness, not on Rjif)
         agree <- TRUE
         for (i in seq_along(vs)) for (j in seq_along(vs)) {
           agree <- agree &&
             (identical(mid(vs[[i]]), mid(vs[[j]])) ==
              identical(wire(vs[[i]]), wire(vs[[j]])))
         }
         # the specific facts the corrected comments state: NULL posts
         # {"model":null} (NOT {}), and NULL/""/" " are three identities
         facts <- identical(wire(NULL), "{\"model\":null}") &&
           length(unique(vapply(list(NULL, "", " "), mid, character(1)))) == 3L &&
           identical(mid(character(0)), mid(list()))
         agree && facts
       }))

cat("\n")
expect("r6g-B1/n1/m1/m2/B2: caller text scrubbed at gates, used-level-only factor check, POSIXlt safe, direct path pre-serializes",
       local({
         # Astra round 6g on 3a0ff1d (evidence: rjif-r6g-astra-evidence/
         # findings-repro.R; every finding mirrored on the old build
         # first). B1: the new class gate echoed a key-named class
         # verbatim (stop() ran before .clean_error_text could see it).
         # B2: the pre-transport serialization property held only for the
         # DEFAULT transport -- a custom Rjif.transport callback was
         # entered with a never-serializable body and its answer returned.
         # m1: the factor check validated ALL levels, but jsonlite sends
         # only the USED label, so an unused bad level over-refused a
         # perfectly sendable question. m2: POSIXlt's self-nesting [[
         # recursions crashed the slot walk with an UNcatchable C stack
         # overflow. n1: criterion names (slot paths) could carry the key
         # into a scan error. Closure: gate/slot/scan messages all run
         # through .clean_error_text; the factor check sees the used
         # level; the walk descends plain lists only; jev_eval pre-
         # renders the body with JSON_OPTS before dispatching anywhere.
         KEY <- "R6G_SYNTHETIC_OPAQUE_KEY_9Q7"
         bstr <- rawToChar(as.raw(0xc3)); Encoding(bstr) <- "bytes"
         md5of <- function(f) as.character(tools::md5sum(f))
         wire <- function(x) as.character(do.call(jsonlite::toJSON,
                       c(list(x), Rjif:::JSON_OPTS)))
         entered <- 0L
         tr <- function(body) { entered <<- entered + 1L
           list(model = "m", usage = list(input_tokens = 1L),
                answers = list(q = list(type = "noul", noul = 0.9))) }

         # --- B2 + m1 + m2 inside one mock-transport scope ---
         b2 <- m1 <- m2 <- FALSE
         withr_options(Rjif.transport = tr, {
           # B2: direct jev_eval refuses a never-serializable body
           # BEFORE entering the callback...
           qb <- jev_noul_q("q")
           qb$extra <- structure(list(x = 1L), class = "never_serializable")
           e <- tryCatch({ jev_eval("s", list(q = qb), model = "m"); NA_character_ },
                         error = function(x) conditionMessage(x))
           refused <- !is.na(e) && grepl("could not be serialized", e, fixed = TRUE)
           # ...and a serializable direct call still reaches it once.
           r <- jev_eval("s", list(q = jev_noul_q("q")), model = "m")
           b2 <- refused && entered == 1L && isTRUE(jprob(r$q) == 0.9)

           # m1: bytes-marked UNUSED level renders nowhere on the wire:
           # no refusal, and the byte-identical clean twin resumes it.
           f_ok <- structure(1L, levels = c("safe", bstr), class = "factor")
           qf <- jev_noul_q("q", list(true = f_ok, false = "no"))
           qc <- jev_noul_q("q", list(true = "safe", false = "no"))
           same_wire <- identical(wire(unclass(qf)), wire(unclass(qc)))
           n0 <- entered
           e <- tryCatch({ jev_score_many("s", qf, model = "m"); NA_character_ },
                         error = function(x) conditionMessage(x))
           flows <- is.na(e) && entered == n0 + 1L
           cf <- tempfile(fileext = ".rds")
           jev_score_many("s", qc, model = "m", cache = cf)
           n1 <- entered
           e <- tryCatch({ jev_score_many("s", qf, model = "m", cache = cf); NA_character_ },
                         error = function(x) conditionMessage(x))
           twin_resume <- is.na(e) && entered == n1
           # a USED bad level still refuses, with the slot named:
           f_bad <- structure(2L, levels = c("safe", bstr), class = "factor")
           e <- tryCatch(Rjif:::.question_identity(jev_noul_q("q", list(true = f_bad, false = "no"))),
                         error = function(x) conditionMessage(x))
           used_refuses <- nzchar(e) && grepl("question$criteria$true", e, fixed = TRUE)
           m1 <- same_wire && flows && twin_resume && used_refuses

           # m2: POSIXlt digests without the walk's C stack overflow,
           # and the digest tracks the rendered wall time.
           lt <- as.POSIXlt("2024-03-10 06:30:00", tz = "UTC")
           ql <- jev_noul_q("q", list(true = lt, false = "no"))
           qi1 <- NULL
           e <- tryCatch({ qi1 <- Rjif:::.question_identity(ql); NA_character_ },
                         error = function(x) conditionMessage(x))
           digests <- is.na(e) && is.list(qi1) && nzchar(qi1$digest)
           lt2 <- as.POSIXlt("2024-03-10 06:30:01", tz = "UTC")
           qi2 <- Rjif:::.question_identity(jev_noul_q("q", list(true = lt2, false = "no")))
           tracks <- !identical(qi1$digest, qi2$digest)
           n2 <- entered
           r <- jev_score_many("s", ql, model = "m")
           m2 <- digests && tracks && entered == n2 + 1L
         })

         # --- B1 + n1: caller-named classes/slots scrubbed in errors ---
         b1 <- n1res <- FALSE
         withr_options(Rjif.transport = NULL, {
           Sys.setenv(TYPESAFE_API_KEY = KEY)
           keyed <- structure("m", class = KEY)
           ok_modes <- TRUE
           for (mode in c("direct", "fresh", "resume")) {
             cf <- tempfile(fileext = ".rds")
             if (mode == "resume") {
               jev_score_many("s", jev_noul_q("q"), model = "m", cache = cf)
             }
             before <- md5of(cf)
             e <- tryCatch({
               if (mode == "direct") jev_eval("s", list(q = jev_noul_q("q")), model = keyed)
               else jev_score_many("s", jev_noul_q("q"), model = keyed,
                                   cache = if (mode == "resume") cf else NULL)
               NA_character_
             }, error = function(x) conditionMessage(x))
             ok_modes <- ok_modes && !is.na(e) && !grepl(KEY, e, fixed = TRUE) &&
               grepl("REDACTED-API-KEY", e, fixed = TRUE) &&
               identical(before, md5of(cf))
           }
           b1 <- ok_modes
           # n1: a criterion NAMED with the key, bad used level: the slot
           # path in the scan error must carry the marker, not the key.
           qn <- jev_score_q("q", list("low", "high"))
           qn$criteria <- list(structure(1L, levels = bstr, class = "factor"))
           names(qn$criteria) <- KEY
           e <- tryCatch(Rjif:::.question_identity(qn),
                         error = function(x) conditionMessage(x))
           n1res <- nzchar(e) && !grepl(KEY, e, fixed = TRUE)
           Sys.unsetenv("TYPESAFE_API_KEY")
         })
         b1 && b2 && m1 && m2 && n1res
       }))

# ---- audit round 6h (R6h-B1 B2 m1): cache-diagnostic redaction, validated
# wire render, used-level-only factor scan. r16-wire-capture.R proves the
# real-transport half; this pins the refusals and positive controls.
expect("r6h: cache diagnostics redacted, validated render, used-level factors",
  local({
    KEYH <- "R6HSYNTHKEY9Q7"
    bstr <- "\x81\x82not valid utf-8"
    tr_ok <- function(body) list(model = "m", usage = list(input_tokens = 1L),
                                 answers = list(q = list(type = "noul", noul = 0.9)))
    entered <- 0L
    tr_count <- function(body) { entered <<- entered + 1L; tr_ok(body) }
    o <- options(Rjif.transport = tr_ok, Rjif.retries = 0L)
    Sys.setenv(TYPESAFE_API_KEY = KEYH)
    on.exit({ options(o); Sys.unsetenv("TYPESAFE_API_KEY") }, add = TRUE)

    # --- B1: key-named cache path must not echo the key (3 sites) ---
    dir.create(dnames <- tempfile()); dir.create(file.path(dnames, KEYH))
    badfile <- file.path(dnames, KEYH, paste0(KEYH, ".rds"))
    writeBin(charToRaw("not an RDS at all"), badfile)
    e1 <- err_msg(jev_score_many("s", jev_noul_q("q"), model = "m", cache = badfile))
    b1a <- nzchar(e1) && !grepl(KEYH, e1, fixed = TRUE) &&
      grepl("REDACTED-API-KEY", e1, fixed = TRUE) &&
      grepl("Nothing was written", e1, fixed = TRUE)
    # mismatch: a VALID cache whose fingerprint differs (threshold 0.7 seed,
    # called at 0.5). Lives in a dir AND file named with the key.
    seed <- file.path(tempdir(), "r6h-seed.rds")
    if (file.exists(seed)) file.remove(seed)
    jev_score_many("s", jev_noul_q("q"), model = "m", cache = seed, threshold = 0.7)
    keynamed <- file.path(dnames, KEYH, paste0("c-", KEYH, ".rds"))
    file.copy(seed, keynamed)
    e2 <- err_msg(jev_score_many("s", jev_noul_q("q"), model = "m", cache = keynamed))
    b1b <- nzchar(e2) && !grepl(KEYH, e2, fixed = TRUE) &&
      grepl("REDACTED-API-KEY", e2, fixed = TRUE) &&
      grepl("NOT overwritten", e2, fixed = TRUE)
    # write-failure warning: read-only dir named with the key (chmod the
    # KEY dir itself: .cache_save writes tmp + target INSIDE dirname(cache),
    # so chmod of the grandparent would not block it). saveRDS's own
    # low-level warning must be suppressed, the package warning redacted.
    ro <- file.mode(file.path(dnames, KEYH))
    Sys.chmod(file.path(dnames, KEYH), "0555")
    allw <- warn_msg(jev_score_many("s", jev_noul_q("q"), model = "m",
                                    cache = file.path(dnames, KEYH, "writefail.rds")))
    Sys.chmod(file.path(dnames, KEYH), ro)
    b1c <- nzchar(allw) && !grepl(KEYH, allw, fixed = TRUE)

    # --- B2: invalid UTF-8 under classed wrappers refuses, zero calls ---
    n0 <- entered
    options(Rjif.transport = tr_count)
    qI <- jev_noul_q("q"); qI$instructions <- I(bstr)
    qDF <- jev_noul_q("q"); qDF$instructions <- data.frame(x = bstr, stringsAsFactors = FALSE)
    refused <- TRUE
    for (qbad in list(qI, qDF)) {
      e <- err_msg(jev_eval("s", list(q = qbad), model = "m"))
      # either line of defense may fire first: the slot scan naming the bad
      # slot, or the validated envelope render. Both are refusals.
      refused <- refused && nzchar(e) &&
        (grepl("not valid JSON/UTF-8", e, fixed = TRUE) ||
         grepl("never be serialized", e, fixed = TRUE))
    }
    b2a <- refused && entered == n0
    # the cache identity refuses the SAME envelope the transport would
    e <- err_msg(Rjif:::.question_identity(qDF))
    b2b <- nzchar(e) && grepl("not valid JSON/UTF-8", e, fixed = TRUE)
    # canonical phrase survives on the unknown-class route (r6f grep)
    e <- err_msg(Rjif:::.wire_digest(structure("m", class = "unknownclass9"), "model"))
    b2c <- nzchar(e) && grepl("cannot be serialized", e, fixed = TRUE)

    # --- positive controls: valid payloads still flow (r6g-m1/m2) ---
    v1 <- jev_eval("s", list(q = jev_noul_q("q")), model = "m")
    flow1 <- identical(jprob(v1$q), 0.9) && entered > n0
    # factor whose UNUSED level is bytes-marked: flows (r6g-m1)
    fl <- factor("a", levels = c("a", bstr))
    qFL <- jev_score_q("q", c("yes", "no")); qFL$instructions <- list(lvl = fl)
    flow2 <- isTRUE(tryCatch({ jev_eval("s", list(q = qFL), model = "m"); TRUE },
                             error = function(x) FALSE))
    # POSIXlt nested in a plain list: no hang, flows (r6g-m2)
    qt <- jev_noul_q("q"); qt$instructions <- list(when = as.POSIXlt(Sys.time()))
    flow3 <- isTRUE(tryCatch({ jev_eval("s", list(q = qt), model = "m"); TRUE },
                             error = function(e) FALSE))
    b1a && b1b && b1c && b2a && b2b && b2c && flow1 && flow2 && flow3
  }))

if (fail > 0L) {
  cat(sprintf("SMOKE FAILED: %d assertion(s)\n", fail))
  quit(save = "no", status = 1L)
}
cat("SMOKE OK\n")
