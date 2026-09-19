# Calibration analytics for judgment calls: the whole point of a System One
# interface is that its numbers can be audited. These are audit helpers.
#
# All three take a data.frame with a probability column (default "p") and a
# logical/0-1 ground-truth column (default "truth"), e.g. the output of
# jev_score_many() plus a gold-label column. Rows are dropped only when they
# carry no information (p or truth NA, or p outside [0,1]); every drop is
# counted in attr(, "n_dropped") -- buckets are mutually exclusive and add up,
# so you cannot accidentally audit a curve built from 3 of your 500 rows.
# Invalid values (a truth of 2, factor probability labels that are not numeric)
# are refused loudly rather than silently coerced into flattering numbers.
#
# Statistical notes / limitations (read before quoting a number):
#   * These are the classic equal-width bin definitions (Naeini et al. / Guo et
#     al.), NOT the calibration-error-with-bias-correction of Nixon et al. 2022
#     and no equal-mass ("adaptive") variant.
#   * Bins are half-open intervals (a, b] except the lowest, which includes 0
#     (include.lowest). Probabilities outside [0, 1] are dropped and reported in
#     attr(, "n_out_of_range").
#   * ECE is a plug-in estimator of true expected calibration error. It is NOT
#     an upper bound: there are populations (e.g. equal-mass bins at p=.01/.09
#     with deterministic truths) where the binned estimate .45 sits below a
#     true error of .54. Treat it as an estimate with a sampling distribution,
#     and quote attr(rc, "n_used") with every number.
#   * Reliability is only meaningful against *gold* truth. If "truth" came from
#     another model, you have agreement, not calibration.

reliability_curve <- function(df, p = "p", truth = "truth", n_bins = 10L) {
  .check_df_cols(df, p, truth)
  n_bins <- .as_pos_int(n_bins, "n_bins")
  # Factor probability columns store small integers (1, 2, ...) with pretty
  # labels ("0.1", "0.9"): as.numeric() on the factor would read the storage
  # codes, not the probabilities. Convert via the labels, or refuse.
  pv <- .as_probability_vector(df[[p]])
  tv <- .as_logical01(df[[truth]])

  na_p  <- is.na(pv)
  oor   <- !na_p & (pv < 0 | pv > 1)
  na_t  <- is.na(tv)
  # mutually exclusive accounting on the ORIGINAL vectors, evaluated before
  # any subsetting: every dropped row lands in exactly one bucket (priority
  # p-NA > truth-NA > out-of-range), and n_used + counts == nrow(df).
  keep      <- !na_p & !oor & !na_t
  cnt_p_na  <- sum(na_p)
  cnt_t_na  <- sum(!na_p & na_t)
  cnt_oor   <- sum(!na_p & !na_t & oor)
  pv <- pv[keep]; tv <- tv[keep]

  cuts <- seq(0, 1, length.out = n_bins + 1L)
  g <- cut(pv, cuts, include.lowest = TRUE)
  lv <- levels(g)

  # build every bin explicitly (no reliance on aggregate/drop=FALSE behaviour),
  # so an unoccupied bin is a row of zeros rather than a silently missing row.
  # (Cosmetic for ECE -- occupied-bin weighting is unchanged either way -- but
  # it makes the denominators visible, which is the point of this table.)
  counts <- tabulate(as.integer(g), nbins = length(lv))
  sums_p <- tapply(pv, g, sum, default = 0)
  sums_t <- tapply(tv, g, sum, default = 0)
  out <- data.frame(
    bin      = lv,
    n        = as.integer(counts),
    mean_p   = ifelse(counts > 0, as.numeric(sums_p) / pmax(counts, 1L), NA_real_),
    accuracy = ifelse(counts > 0, as.numeric(sums_t) / pmax(counts, 1L), NA_real_),
    stringsAsFactors = FALSE, row.names = NULL)
  attr(out, "n_used") <- length(pv)
  attr(out, "n_dropped") <- list(p_na = cnt_p_na, truth_na = cnt_t_na,
                                 out_of_range = cnt_oor,
                                 not_scored = 0L)
  # n_dropped buckets are mutually exclusive and complete:
  # n_used + p_na + truth_na + out_of_range == nrow(df).
  attr(out, "n_out_of_range") <- cnt_oor
  attr(out, "n_bins") <- n_bins
  out
}

# Expected calibration error: sample-weighted mean |accuracy - mean_p| over
# occupied bins. Returns 0 only when every occupied bin is perfectly calibrated
# (an empty curve returns NA with a warning, not a flattering 0).
# Caveat that cannot live in a scalar: ECE=0 on 1 usable row of 100 is
# arithmetically true and operationally meaningless, so ece() warns whenever
# fewer than 30 rows survive the drop filters (attr-free disclosure for a
# numeric return; use reliability_curve() for the full accounting).
ece <- function(df, p = "p", truth = "truth", n_bins = 10L) {
  rc <- reliability_curve(df, p, truth, n_bins)
  if (!any(rc$n > 0L)) {
    warning("Rjif: ece() had no scored rows in range [0,1]; returning NA.",
            call. = FALSE)
    return(NA_real_)
  }
  n_used <- sum(rc$n)
  if (n_used < 30L) {
    warning("Rjif: ece() is based on ", n_used, " usable row(s) of ",
            nrow(df), " -- a small-sample ECE mostly measures bin noise. ",
            "attr(reliability_curve(...), 'n_dropped') has the accounting.",
            call. = FALSE)
  }
  occ <- rc$n > 0L
  sum(rc$n[occ] / sum(rc$n[occ]) * abs(rc$accuracy[occ] - rc$mean_p[occ]))
}

# Selection curve: as you raise the confidence floor, what happens to coverage
# (fraction auto-decided) and to the positive-event rate among the rows kept?
#
# Naming note (audit B3): the `pos_rate` column is the mean of the EVENT-TRUTH
# column among selected rows -- a prevalence/PPV-style quantity. It is NOT
# decision accuracy: for a row selected on p >= floor, "correct" would require
# decision == truth, which conflates calibration (reliability_curve) with
# thresholding. This table answers "if I auto-decide rows above this
# confidence, what event rate am I actually acting on?" For a per-row
# correctness view, compute mean(df$decision == df$truth) yourself against
# gold labels.
#
# Coverage denominator is the number of *usable* rows (p and truth both present
# and p in [0,1]) -- i.e. it answers "of the rows we can score, how many clear
# this floor?". Rows you could not score at all are excluded from the
# denominator; attr(, "n_usable") tells you how big that denominator is.
selection_curve <- function(df, floor_seq = seq(0, 0.95, by = 0.05),
                            p = "p", truth = "truth") {
  .check_df_cols(df, p, truth)
  if (!is.numeric(floor_seq) || !length(floor_seq)) {
    stop("Rjif: floor_seq must be a non-empty numeric vector.", call. = FALSE)
  }
  if (anyNA(as.numeric(floor_seq))) {
    stop("Rjif: floor_seq must not contain NA.", call. = FALSE)
  }
  if (any(floor_seq < 0 | floor_seq > 1)) {
    warning("Rjif: floor_seq contains values outside [0, 1]; rows there select ",
            "everything (f <= 0) or nothing (f > 1).", call. = FALSE)
  }
  pv <- .as_probability_vector(df[[p]])
  tv <- .as_logical01(df[[truth]])
  keep <- !is.na(pv) & !is.na(tv) & pv >= 0 & pv <= 1
  pv <- pv[keep]; tv <- tv[keep]
  n <- length(pv)
  if (!n) {
    warning("Rjif: selection_curve() had no usable rows.", call. = FALSE)
  }
  rows <- lapply(as.numeric(floor_seq), function(f) {
    sel <- pv >= f
    k <- sum(sel)
    data.frame(floor = f,
               n = n,
               kept = as.integer(k),
               coverage = if (n) k / n else NA_real_,
               pos_rate = if (k > 0L) mean(tv[sel]) else NA_real_,
               escalated = if (n) (n - k) / n else NA_real_,
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  attr(out, "n_usable") <- n
  attr(out, "n_rows") <- nrow(df)
  rownames(out) <- NULL
  out
}

# --- internals ----------------------------------------------------------------

.check_df_cols <- function(df, p, truth) {
  if (!is.data.frame(df)) stop("Rjif: expected a data.frame.", call. = FALSE)
  miss <- setdiff(c(p, truth), names(df))
  if (length(miss)) {
    stop("Rjif: data.frame is missing column(s): ", paste(miss, collapse = ", "),
         ". Available: ", paste(names(df), collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

.as_pos_int <- function(x, what) {
  x <- suppressWarnings(as.integer(x))
  if (length(x) != 1L || is.na(x) || x < 1L) {
    stop("Rjif: ", what, " must be a single integer >= 1.", call. = FALSE)
  }
  x
}

# Accept logical, numeric 0/1, or "true"/"false"/"yes"/"no" text as ground truth.
# Anything else is refused per-value, never coerced: as.logical(2) is TRUE, and
# a truth column of 2s would otherwise manufacture a perfectly calibrated
# curve out of garbage (audit B4).
.as_logical01 <- function(x) {
  if (is.logical(x)) return(x)
  if (is.numeric(x)) {
    out <- rep(NA, length(x))
    good <- !is.na(x) & (x == 0 | x == 1)
    out[good] <- x[good] == 1
    bad <- !is.na(x) & !good
    if (any(bad)) {
      warning("Rjif: truth column has ", sum(bad),
              " value(s) that are neither 0 nor 1; treated as NA (not coerced).",
              call. = FALSE)
    }
    return(as.logical(out))
  }
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) {
    m <- tolower(trimws(x))
    out <- rep(NA, length(m))
    out[m %in% c("true", "yes", "y", "1")] <- TRUE
    out[m %in% c("false", "no", "n", "0")] <- FALSE
    if (all(is.na(out)) && any(!is.na(x))) {
      warning("Rjif: could not interpret truth column as logical; ",
              "returning NA for every row.", call. = FALSE)
    }
    return(out)
  }
  warning("Rjif: unsupported truth column type (", class(x)[[1L]],
          "); treating as NA.", call. = FALSE)
  rep(NA, length(x))
}

# Probability columns arrive as numeric (the normal case), but factor columns
# are common in hand-built audit frames and their STORAGE codes are 1..K while
# the actual probabilities live in the LABELS. as.numeric(factor("0.9")) = 1:
# silently wrong. Convert via labels; refuse anything else.
.as_probability_vector <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  if (is.factor(x)) {
    labs <- as.character(x)
    out <- suppressWarnings(as.numeric(labs))
    if (any(!is.na(labs) & is.na(out))) {
      stop("Rjif: probability column is a factor whose labels are not all ",
           "numeric (", sum(!is.na(labs) & is.na(out)), " label(s) unparseable); ",
           "supply numeric probabilities.", call. = FALSE)
    }
    return(out)
  }
  if (is.logical(x)) return(as.numeric(x))
  if (is.character(x)) {
    out <- suppressWarnings(as.numeric(x))
    if (any(!is.na(x) & is.na(out))) {
      stop("Rjif: probability column is character with non-numeric values; ",
           "supply numeric probabilities.", call. = FALSE)
    }
    return(out)
  }
  stop("Rjif: probability column must be numeric (or a factor with numeric ",
       "labels); got ", class(x)[[1L]], ".", call. = FALSE)
}
