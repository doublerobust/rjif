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
#   * These are the classic equal-width bin definitions (Naeini et al. 2015;
#     popularised by Guo et al. 2017), NOT the debiased estimator of Roelofs
#     et al. (AISTATS 2022) nor the adaptive ACE of Nixon et al. (arXiv
#     1904.01685, 2019), and no equal-mass ("adaptive") variant.
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

reliability_curve <- function(df, p = "p", truth = "truth", n_bins = 10L,
                              allow_type = NULL) {
  .check_df_cols(df, p, truth)
  .check_probability_semantics(df, allow_type)
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
    observed_rate = ifelse(counts > 0, as.numeric(sums_t) / pmax(counts, 1L), NA_real_),
    stringsAsFactors = FALSE, row.names = NULL)
  # COLUMN RENAMED `accuracy` -> `observed_rate` (versioned change, external
  # audit finding 2): "accuracy" misleads at low prevalence - a bin where the
  # event never occurs shows "accuracy 0.00" next to a perfectly calibrated
  # mean_p of 0.07, and statisticians reasonably read 0.00 as a failure rate.
  # This is a BREAKING rename, honest about being one: df$accuracy is NULL
  # from here on (an attribute alias would be worse: data.frame $ never sees
  # attributes, so it would pretend to work and return NULL anyway). Update
  # readers to observed_rate; the semantics were always the event frequency.
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

# Expected calibration error: sample-weighted mean |observed_rate - mean_p|
# over occupied bins. Returns 0 only when every occupied bin is perfectly
# calibrated (an empty curve returns NA with a warning, not a flattering 0).
# Caveat that cannot live in a scalar: ECE=0 on 1 usable row of 100 is
# arithmetically true and operationally meaningless, so ece() warns whenever
# fewer than 30 rows survive the drop filters (attr-free disclosure for a
# numeric return; use reliability_curve() for the full accounting).
ece <- function(df, p = "p", truth = "truth", n_bins = 10L,
                allow_type = NULL) {
  rc <- reliability_curve(df, p, truth, n_bins, allow_type = allow_type)
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
  sum(rc$n[occ] / sum(rc$n[occ]) * abs(rc$observed_rate[occ] - rc$mean_p[occ]))
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
                            p = "p", truth = "truth", allow_type = NULL,
                            policy = c("positive", "two_sided"), threshold = 0.5) {
  policy <- match.arg(policy)
  threshold <- .single_number(threshold, "threshold")
  if (policy == "two_sided" && !identical(attr(df, "question_type"), "noul")) {
    # two-sided mirrors jif()'s noul decision window; for a choice/score
    # batch (or an untagged hand-built frame) it is meaningless.
    stop("Rjif: policy = 'two_sided' requires a noul batch (the curve ",
         "mirrors jif()'s two-sided confidence_floor decision window; for a ",
         "choice/score batch use the default 'positive' selection view).",
         call. = FALSE)
  }
  .check_df_cols(df, p, truth)
  .check_probability_semantics(df, allow_type)
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
    # Match the evaluator's policy switch as well as its window predicate.
    sel <- if (policy == "two_sided" && f > threshold && f <= 1)
      .noul_window_decided(pv, f) else pv >= f
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
  attr(out, "policy") <- policy
  if (policy == "two_sided") attr(out, "threshold") <- threshold
  rownames(out) <- NULL
  out
}

# --- internals ----------------------------------------------------------------

# Probability-semantics gate (external audit finding 2, 2026-09-19).
# A batch data frame from jev_score_many() carries attr(, "question_type"),
# and its `p` column means different things per type:
#   noul   -> P(assertion true). An event probability. Calibration-valid.
#   choice -> P(chosen option). A selected-class confidence; calibration of
#             P(correct class) is a different object than P(event), so it is
#             allowed only with an explicit allow_type = "choice".
#   score  -> the vendor confidence: distribution CONCENTRATION, not the
#             probability of anything positive. Probe 5's damning case: 30
#             rows, all-negative truth, decisions all correct, score
#             confidence 1 -> ECE = 1.0 "catastrophic miscalibration" that
#             is really arithmetic on the wrong quantity. By default we
#             REFUSE and tell the user what to pass instead;
#             allow_type = "score" remains an explicit, warned opt-out for
#             anyone who genuinely wants concentration-vs-outcome curves.
.check_probability_semantics <- function(df, allow_type = NULL) {
  qt <- attr(df, "question_type")
  if (is.null(qt)) return(invisible(TRUE))   # hand-built frame: user's contract
  qt <- as.character(qt[[1L]])
  if (identical(qt, "noul")) return(invisible(TRUE))
  if (qt %in% allow_type) {
    if (qt == "score") {
      warning("Rjif: `p` here is the vendor confidence (distribution ",
              "concentration), not an event probability. A reliability curve on ",
              "it answers 'how well does concentration predict the outcome?' ",
              "- which is usually not calibration. Quote it as such.",
              call. = FALSE)
    } else {
      warning("Rjif: `p` here is the probability of the SELECTED option, not ",
              "of a fixed binary event. For selected-class calibration, truth ",
              "must indicate whether that selected option was correct.",
              call. = FALSE)
    }
    return(invisible(TRUE))
  }
  stop("Rjif: this batch's question_type is '", qt, "' and its `p` column is ",
       switch(qt,
         score = "the vendor confidence (how concentrated the level distribution is), NOT a probability of the event",
         choice = "the probability of the SELECTED option, not of a binary event",
         "not an event probability"),
       ". reliability_curve()/ece() measure event-probability calibration. ",
       "For a binary event probability use a noul question; to proceed anyway ",
       "(knowing the curve is not calibration in the usual sense) pass ",
       "allow_type = '", qt, "'.",
       call. = FALSE)
}

.check_df_cols <- function(df, p, truth) {
  if (!is.data.frame(df)) stop("Rjif: expected a data.frame.", call. = FALSE)
  miss <- setdiff(c(p, truth), names(df))
  if (length(miss)) {
    # caller-supplied column names are echoed into the message; scrub before
    # signaling so a key-bearing name never leaves the process (audit R7/R8
    # supplemental: calibration.R:159-164 unwrapped echo sites)
    stop(.clean_error_text(paste0(
      "Rjif: data.frame is missing column(s): ", paste(miss, collapse = ", "),
      ". Available: ", paste(names(df), collapse = ", "), ".")),
      call. = FALSE)
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
    bad <- !is.na(x) & is.na(out)
    if (any(bad)) {
      # per-value disclosure (audit R2-m2), symmetric with the numeric path:
      # an uninterpretable entry is dropped AND counted, never silently NA.
      warning("Rjif: truth column has ", sum(bad),
              " uninterpretable value(s) (not true/false/yes/no/0/1); ",
              "treated as NA.", call. = FALSE)
    }
    return(out)
  }
  warning(.clean_error_text(paste0(
    "Rjif: unsupported truth column type (", class(x)[[1L]],
    "); treating as NA.")), call. = FALSE)
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
  stop(.clean_error_text(paste0(
    "Rjif: probability column must be numeric (or a factor with numeric ",
    "labels); got ", class(x)[[1L]], ".")), call. = FALSE)
}
