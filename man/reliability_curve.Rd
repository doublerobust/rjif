\name{reliability_curve}
\alias{reliability_curve}
\alias{ece}
\alias{selection_curve}
\title{Descriptive Calibration Diagnostics}
\usage{
reliability_curve(df, p = "p", truth = "truth", n_bins = 10L,
                  allow_type = NULL)

ece(df, p = "p", truth = "truth", n_bins = 10L, allow_type = NULL)

selection_curve(df, floor_seq = seq(0, 0.95, by = 0.05),
                p = "p", truth = "truth", allow_type = NULL,
                policy = c("positive", "two_sided"), threshold = 0.5)
}
\arguments{
  \item{df}{a data frame with one column of predicted probabilities and one
    of event outcomes; the output of \code{\link{jev_score_many}} works with
    the default column names.}

  \item{p}{name of the probability column (values in [0, 1]).}

  \item{truth}{name of the outcome column. Accepted values: numeric 0/1,
    logicals, a factor with labels \code{\{"0"\}}/\code{\{"1"\}}, or
    character labels that mean yes/no after lowercasing and trimming:
    \code{true}, \code{yes}, \code{y}, \code{1} for events and \code{false},
    \code{no}, \code{n}, \code{0} for non-events. Numeric-looking strings
    like \code{"0.0"} are NOT parsed as numbers; they are counted as
    uninterpretable. Anything else warns per value and is treated as missing.
    Factor probability columns are read from their labels, never from R's
    internal integer codes. \code{truth} must be GOLD labels: against another
    model's labels you are measuring agreement, not calibration.}

  \item{n_bins}{number of equal-width bins. Default 10. Bins are half-open
    intervals (a, b] except the lowest, which includes 0. Below about 100
    scored rows the curve mostly measures bin noise (a rule of thumb, not an
    implemented threshold).}

  \item{floor_seq}{numeric vector of decision thresholds, no NA allowed.
    Values outside [0, 1] warn and then apply literally: a floor at or below
    0 keeps every usable row, a floor above 1 keeps none.}

  \item{allow_type}{character vector of batch types allowed to bypass the
    probability-semantics check. All three functions expect event probabilities.
    For Noul, \code{p} is P(assertion true) and passes without warning. For
    Choice, it is P(selected option). For Score, it is distribution confidence
    (concentration), not an event probability. Choice and Score therefore
    error by default. Naming either in \code{allow_type} permits computation
    with a warning. For selected-class Choice calibration, \code{truth} must
    indicate whether the selected option was correct. A Score override still
    measures concentration versus outcome, not event-probability calibration.
    Hand-built data frames without a \code{question_type} attribute are the
    caller's responsibility and pass without a type check.}

  \item{policy}{\code{selection_curve()} only: \code{"positive"} (default)
    counts rows whose probability clears the floor from above;
    \code{"two_sided"} counts rows \code{\link{jif}} would decide under its
    two-sided noul \code{confidence_floor} window (either tail), and errors
    unless the batch is a noul frame. See \strong{Value}.}
  \item{threshold}{\code{selection_curve()} only: the decision threshold to
    match when \code{policy = "two_sided"}. Defaults to 0.5, like \code{jif()}.
    Set it to the threshold used for evaluation; it is not inferred from the
    batch. It does not affect the default \code{"positive"} selection rule.}
}
\value{
  \code{reliability_curve()} returns a data frame with one row per bin
  (empty bins included, so the shape is stable): \code{bin} (the cut label),
  \code{n} (rows in bin), \code{mean_p} (mean predicted probability),
  \code{observed_rate} (the event frequency in the bin: this is the
  outcome rate, not classification accuracy). RENAMED from \code{accuracy}
  in 0.0.1 (breaking): the old name read as a failure rate in bins where the
  event never occurs, next to a perfectly calibrated \code{mean_p}.
  Attributes: \code{n_used},
  \code{n_bins}, \code{n_out_of_range} (rows dropped for a probability
  outside [0, 1]), and \code{n_dropped}, a list with mutually exclusive
  buckets:
  \code{p_na} (missing probability), \code{truth_na} (missing or
  uninterpretable truth), \code{out_of_range} (probability outside [0, 1]),
  \code{not_scored}. A dropped row is counted in exactly one bucket
  (priority p-NA, then truth-NA, then out-of-range), and
  \code{n_used + sum(unlist(n_dropped)) == nrow(df)} always holds.

  \code{ece()} returns a single numeric: the sample-size-weighted mean
  absolute gap between \code{mean_p} and observed event frequency over
  non-empty bins, or \code{NA} when nothing is scored. It warns below 30
  usable rows and states the sample it used.

  \code{selection_curve()} returns a data frame with one row per floor:
  \code{floor}, \code{n} (usable rows, the denominator for everything),
  \code{kept}, \code{coverage} (kept / usable), \code{pos_rate} (event
  prevalence among kept rows), \code{escalated} (1 - coverage). Coverage is
  a share of USABLE rows, not of all input rows; \code{attr(, "n_rows")}
  reports the full input size and \code{attr(, "n_usable")} the denominator.
  \code{attr(, "policy")} carries the selection rule used. With
  \code{policy = "two_sided"} (noul batches only; other inputs error), the
  curve uses the evaluator's shared window predicate when
  \code{threshold < floor <= 1}: keep \code{p >= floor} or
  \code{p <= 1 - floor + 1e-9}. At other floors it keeps \code{p >= floor},
  just as the evaluator's single-sided fallback does. For example, p = 0.01
  and p = 0.99 both count at floor 0.7 and threshold 0.5, but only p = 0.99
  counts at floor 0.4. \code{attr(, "threshold")} records the supplied
  threshold for this policy. With the default \code{"positive"} the
  statistic is the share of usable rows with \code{p >= floor}, a
  positive-tail selection view, not a decision coverage.
  \code{pos_rate} is not correctness: classifying \code{p = (0.2, 0.8)}
  against \code{truth = (0, 1)} at floor 0 gives prevalence 0.5 even though
  both threshold-0.5 decisions are correct. A warning fires when no row is
  usable.
}
\description{
Three descriptive tools for auditing the probabilities Jev returns on your
own traffic and your own gold labels. None of them calibrates anything or
tests a hypothesis: no standard errors, no bands, no p-values. A 10-bin
curve from 50 rows is a sketch; quote the \code{n_used} attribute next to
every number you report.
}
\details{
\code{ece()} is the classic equal-width-binning plug-in estimator of expected
calibration error (Naeini et al.; popularised by Guo et al. 2017). Two
properties to remember when quoting it:

\itemize{
  \item It is a point estimate, not an upper bound. With two equal-mass
    probability atoms, 0.01 and 0.09, and deterministic truths 1 and 0, both
    atoms fall in the first bin and the estimate reads 0.45 while the true
    calibration error is 0.54. The other direction also happens from
    sampling: 30 observed events at p = 0.5 estimate ECE 0.5 against a truly
    calibrated source. These examples show direction, not a precision
    guarantee.
  \item It differs from the adaptive calibration error of Nixon et al.
    (2019, arXiv:1904.01685) and the debiased estimator of Roelofs et al.
    (AISTATS 2022). Know which estimator a comparison is quoting.
}

The drop filters are deliberately strict because a probability outside
[0, 1] or a truth code outside the accepted labels signals a broken
pipeline, not noise; the dropped rows are counted in the \code{n_dropped}
buckets rather than silently discarded.
}
\examples{
library(Rjif)
set.seed(1)
df <- data.frame(p = runif(200), truth = rbinom(200, 1, 0.3))
df$p[df$p < 0.4] <- df$p[df$p < 0.4] * 0.4   # some overprediction at the top

rc <- reliability_curve(df)
attr(rc, "n_used")
attr(rc, "n_dropped")           # where every excluded row went, by mechanism

ece(df)
selection_curve(df)             # coverage and prevalence as the floor rises

# rows dropped for bad data are accounted for, not hidden
df2 <- rbind(df[1:50, ], data.frame(p = 1.5, truth = 2),
                      data.frame(p = NA, truth = 1))
rc2 <- suppressWarnings(reliability_curve(df2))
unlist(attr(rc2, "n_dropped")) + attr(rc2, "n_out_of_range")
}
\seealso{\code{\link{jev_score_many}}}
