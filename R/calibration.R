# Calibration analytics for judgment calls: the whole point of a System One
# interface is that its numbers can be audited. These are audit helpers.
#
# All three take a data.frame with a probability column (default "p") and a
# logical/0-1 ground-truth column (default "truth"), e.g. the output of
# jev_score_many() plus a gold-label column. Rows where either is NA are
# dropped silently -- they carry no information for calibration -- but the
# count is reported in attr(, "n_dropped") so you cannot accidentally audit a
# curve built from 3 of your 500 rows.
#
# Statistical notes / limitations (read before quoting a number):
#   * These are the classic equal-width bin definitions (Naeini et al. / Guo et
#     al.), NOT the calibration-error-with-bias-correction of Nixon et al. 2022
#     and no equal-mass ("adaptive") variant.
#   * ECE here is the sample-weighted mean |accuracy - mean_p| over occupied
#     bins. It is an upper bound on, not a consistent estimator of, true
#     expected calibration error; with <100 rows it mostly measures bin noise.
#   * Bins are half-open intervals (a, b] except the lowest, which includes 0
#     (include.lowest). Probabilities outside [0, 1] are dropped and reported in
#     attr(, "n_out_of_range").
#   * Reliability is only meaningful against *gold* truth. If "truth" came from
#     another model, you have agreement, not calibration.

reliability_curve <- function(df, p = "p", truth = "truth", n_bins = 10L) {
  .check_df_cols(df, p, truth)
  n_bins <- .as_pos_int(n_bins, "n_bins")
  pv <- suppressWarnings(as.numeric(df[[p]]))
  tv <- .as_logical01(df[[truth]])

  ok_p  <- !is.na(pv)
  oor   <- ok_p & (pv < 0 | pv > 1)
  keep  <- ok_p & !oor & !is.na(tv)
  pv <- pv[keep]; tv <- tv[keep]

  cuts <- seq(0, 1, length.out = n_bins + 1L)
  g <- cut(pv, cuts, include.lowest = TRUE)
  lv <- levels(g)
  bins <- split(seq_along(pv), g)

  # build every bin explicitly (no reliance on aggregate/drop=FALSE behaviour),
  # so an unoccupied bin is a row of zeros rather than a silently missing row
  # that would quietly re-weight the ECE.
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
  attr(out, "n_dropped") <- list(p_na = sum(!ok_p), truth_na = sum(!is.na(pv) & is.na(tv)),
                                 out_of_range = sum(oor),
                                 not_scored = nrow(df) - sum(ok_p | !is.na(pv)))
  # also exposed at the top level under the name used in the header comment
  attr(out, "n_out_of_range") <- sum(oor)
  attr(out, "n_bins") <- n_bins
  out
}

# Expected calibration error: sample-weighted mean |accuracy - mean_p| over
# occupied bins. Returns 0 only when every occupied bin is perfectly calibrated
# (an empty curve returns NA with a warning, not a flattering 0).
ece <- function(df, p = "p", truth = "truth", n_bins = 10L) {
  rc <- reliability_curve(df, p, truth, n_bins)
  if (!any(rc$n > 0L)) {
    warning("Rjif: ece() had no scored rows in range [0,1]; returning NA.",
            call. = FALSE)
    return(NA_real_)
  }
  occ <- rc$n > 0L
  sum(rc$n[occ] / sum(rc$n[occ]) * abs(rc$accuracy[occ] - rc$mean_p[occ]))
}

# Selection curve: as you raise the confidence floor, what happens to coverage
# (fraction auto-decided) and to accuracy among those kept? This is the
# RBQM-relevant view: "review top-k by confidence" versus auditing everything.
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
  pv <- suppressWarnings(as.numeric(df[[p]]))
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
               accuracy = if (k > 0L) mean(tv[sel]) else NA_real_,
               escalated = if (n) (n - k) / n else NA_real_,
               stringsAsFactors = FALSE)
  })
  out <- if (length(rows)) do.call(rbind, rows) else
    data.frame(floor = numeric(), n = integer(), kept = integer(),
               coverage = numeric(), accuracy = numeric(),
               escalated = numeric())[0, ]
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
.as_logical01 <- function(x) {
  if (is.logical(x)) return(x)
  if (is.numeric(x)) return(as.logical(x))
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
