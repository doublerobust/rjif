# Regression tests for j_ifelse() arm laziness (external audit finding 1,
# 2026-09-19): exactly ONE arm element is evaluated per routing decision.
# Run standalone (no installs, works on the source tree):
#   Rscript tests/jifelse-laziness.R
ok <- 0L; fail <- 0L
expect <- function(label, cond) {
  if (isTRUE(cond)) { ok <<- ok + 1L; cat("PASS", label, "\n") }
  else { fail <<- fail + 1L; cat("FAIL", label, "\n") }
}
# Load Rjif: installed package first (R CMD check context), else source R/
# from the repo root or the parent of tests/ (check runs with cwd = tests/).
if (!"package:Rjif" %in% search()) {
  if (requireNamespace("Rjif", quietly = TRUE)) {
    suppressPackageStartupMessages(library(Rjif))
  } else {
    root <- if (dir.exists("R")) "." else if (dir.exists("../R")) ".." else
              stop("run from the repository root or tests/ of the source tree")
    for (f in c("client.R", "mock.R", "jif.R", "calibration.R")) {
      source(file.path(root, "R", f))
    }
  }
}
mk <- function(opt) structure(opt, class = "character")  # mimics a choice jif value

# --- the reported bug: routing to `no` used to force the whole `yes` arm
ev <- new.env()
r <- j_ifelse(mk("beta"),
              yes = list(alpha = { ev$a <- 1; "A" }, beta2 = { ev$a2 <- 1; "A2" }),
              no  = list(beta  = { ev$b <- 1; "B" }))
expect("route to B", identical(as.character(r), "B"))
expect("losing yes arm never evaluated", is.null(ev$a) && is.null(ev$a2))
expect("winning no arm evaluated", identical(ev$b, 1))

# --- no-match: neither literal arm evaluated, unknown lane only
ev2 <- new.env()
r2 <- j_ifelse(mk("gamma"),
               yes = list(alpha = { ev2$a <- 1; "A" }),
               no  = list(beta  = { ev2$b <- 1; "B" }),
               unknown = { ev2$u <- 1; "U" })
expect("no-match -> unknown", identical(as.character(r2), "U"))
expect("neither arm evaluated on no-match", is.null(ev2$a) && is.null(ev2$b))
expect("unknown lane evaluated", identical(ev2$u, 1))

# --- within the winning arm: only the winning ELEMENT is evaluated
ev3 <- new.env()
r3 <- j_ifelse(mk("returns"),
               c(returns = { ev3$y <- 1; "label" },
                 quality_control = { ev3$q <- 1; "defect" }),
               NA_character_, "human")
expect("element route", identical(as.character(r3), "label"))
expect("sibling element in same arm untouched", is.null(ev3$q))

# --- scalar arms and pre-built variables keep the documented behavior
expect("scalar arms, option name is truthy -> yes",
       identical(j_ifelse(mk("billing"), "yes-scalar", "no-scalar"), "yes-scalar"))
pre <- list(returns = "label", quality_control = "defect")  # already evaluated
expect("pre-built variable arms still route by value",
       identical(j_ifelse(mk("returns"), pre, NA_character_, "human"), "label"))

# --- boolean path unchanged
expect("TRUE -> yes", identical(j_ifelse(TRUE, "y", "n"), "y"))
expect("FALSE -> no", identical(j_ifelse(FALSE, "y", "n"), "n"))
expect("bare NA -> unknown", identical(j_ifelse(NA, "y", "n", "u"), "u"))
expect("vector refused",
       grepl("single jif", tryCatch({ j_ifelse(c(TRUE, FALSE), "a", "b"); "" },
                                    error = function(e) conditionMessage(e)),
             fixed = TRUE))

# --- end-to-end with a real choice jif() over the mock transport
options(Rjif.transport = rjif_mock_transport)
test <- jif("the customer says the refund never arrived",
            "a refund is wanted",
            list(returns = "money back", quality_control = "defect ticket"))
ev5 <- new.env()
res <- j_ifelse(test,
                yes = list(returns = { ev5$r <- 1; "handle" },
                           quality_control = { ev5$q <- 1; "qc" }),
                no = "ignore", unknown = { ev5$u <- 1; "escalate" })
expect("real choice result routes into exactly one labeled element",
       is.character(res) && !is.null(ev5[[switch(res, handle = "r", qc = "q",
                                                 escalate = "u", "")]]) &&
         sum(c(!is.null(ev5$r), !is.null(ev5$q), !is.null(ev5$u))) == 1L)
options(Rjif.transport = NULL)

cat("\n", ok, "passed,", fail, "failed\n")
if (fail > 0L) quit(status = 1L)
