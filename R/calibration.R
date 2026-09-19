# Calibration analytics for judgment calls: the whole point of a System One
# interface is that its numbers can be audited. These are audit helpers.

# reliability_curve: given a data.frame from jev_score_many() (or columns
# p + truth), bin the chosen-probability and compare to empirical accuracy.
# Bins: 10 by default (ends inclusive on the right).
reliability_curve <- function(df, p = "p", truth = "truth", n_bins = 10L) {
  pv <- df[[p]]; tv <- as.logical(df[[truth]])
  keep <- !is.na(pv) & !is.na(tv)
  pv <- pv[keep]; tv <- tv[keep]
  cuts <- seq(0, 1, length.out = n_bins + 1L)
  g <- cut(pv, cuts, include.lowest = TRUE)
  # recompute cleanly
  bins <- split(seq_along(pv), g)
  out <- data.frame(
    bin      = names(bins),
    n        = vapply(bins, length, integer(1)),
    mean_p   = vapply(bins, function(i) mean(pv[i]), numeric(1)),
    accuracy = vapply(bins, function(i) mean(tv[i]), numeric(1)),
    stringsAsFactors = FALSE, row.names = NULL)
  out[out$n > 0L, , drop = FALSE]
}

# expected calibration error (sample-weighted |accuracy - mean_p|)
ece <- function(df, p = "p", truth = "truth", n_bins = 10L) {
  rc <- reliability_curve(df, p, truth, n_bins)
  with(rc, sum(n / sum(n) * abs(accuracy - mean_p)))
}

# selection curve: as you raise the confidence floor, what happens to
# coverage (fraction auto-decided) and accuracy among those kept?
# This is the RBQM-relevant view: "review top-k by confidence" vs full audit.
selection_curve <- function(df, floor_seq = seq(0, 0.95, by = 0.05),
                            p = "p", truth = "truth") {
  pv <- df[[p]]; tv <- as.logical(df[[truth]])
  keep <- !is.na(pv) & !is.na(tv)
  pv <- pv[keep]; tv <- tv[keep]
  do.call(rbind, lapply(floor_seq, function(f) {
    sel <- pv >= f
    data.frame(floor = f,
               coverage = mean(sel),
               accuracy = if (any(sel)) mean(tv[sel]) else NA_real_,
               escalated = mean(!sel),
               stringsAsFactors = FALSE)
  }))
}
