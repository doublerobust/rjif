#!/usr/bin/env Rscript
# Offline smoke test (uses the mock transport — no API key needed).
# Run: Rscript tests/smoke.R   (or R CMD INSTALL . first, then example below)

# source package files directly so the smoke test works pre-install
files <- c("R/client.R", "R/jif.R", "R/calibration.R", "R/mock.R")
for (f in files) source(f)

options(Rjif.transport = rjif_mock_transport)

ticket <- "This jacket sucks! The zipper jammed the first time I wore it and now it won't close so I want my money back. Order 11 days old, return window 30 days, $100, one previous order, no refunds."

# 1. j_ifelse triage ------------------------------------------------------------
q_wants_refund <- "the customer wants a refund"
decision <- jif(ticket, q_wants_refund, threshold = 0.5)
cat("wants refund? ", decision, " (abstained: ", jif_abstained(decision), ")\n", sep = "")
action <- j_ifelse(decision,
  yes = "auto-issue return label",
  no  = "reply asking what they want",
  unknown = "queue for human")
cat("action: ", action, "\n", sep = "")

# 2. batch scoring over a mini eCRF-ish data frame ------------------------------
narratives <- c(
  "patient reports feeling off since dose 3, daughter drove her to urgent care",
  "mild headache resolved spontaneously, no intervention",
  "refused next visit, transportation issues, will reschedule",
  "lab ALT 3x ULN noted, dose held pending repeat",
  "no adverse events reported this visit")
truth <- c(TRUE, FALSE, FALSE, TRUE, FALSE)   # "warrants safety review"

df <- jev_score_many(narratives, "a serious adverse event is being reported")
df$truth <- truth
print(df)

# 3. calibration analytics -------------------------------------------------------
rc <- reliability_curve(df)
print(rc)
cat("ECE: ", round(ece(df), 4), "\n", sep = "")
print(selection_curve(df))

# 4. jmatch routing --------------------------------------------------------------
branch <- jmatch(ticket,
  list(returns = "product return or refund request",
       quality_control = "product defect, batch quality issue",
       human_agent = "ambiguous or needs judgment"),
  instructions = "Which team should own this inquiry?")
cat("routed to: ", branch, "\n", sep = "")

# 5. usage accounting ------------------------------------------------------------
print(jev_usage())
cat("SMOKE OK\n")
