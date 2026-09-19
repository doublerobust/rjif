\name{reliability_curve}
\alias{reliability_curve}
\alias{ece}
\alias{selection_curve}
\title{Descriptive Calibration Diagnostics}
\usage{
reliability_curve(df, p = "p", truth = "truth", n_bins = 10L)

ece(df, p = "p", truth = "truth", n_bins = 10L)

selection_curve(df, floor_seq = seq(0, 0.95, by = 0.05),
                p = "p", truth = "truth")
}
\arguments{
  \item{df}{a data frame with one column of predicted probabilities and one
    of event outcomes; the output of \code{\link{jev_score_many}} works with
    the default column names.}

  \item{p}{name of the probability column (values in [0, 1]).}

  \item{truth}{name of the outcome column: 0/1 numerics, logicals, a factor
    whose labels are 0/1, or numeric-looking strings (all read from labels,
    never from R's internal integer codes). Anything else is refused
    per-value with a warning and treated as missing; \code{truth} must be
    GOLD labels. Against another model's labels you are measuring agreement,
    not calibration.}

  \item{n_bins}{number of equal-width bins. Default 10; below about 100
    scored rows the curve mostly measures bin noise.}

  \item{floor_seq}{numeric vector of decision thresholds (no NA allowed),
    each in [0, 1] or the rows there abstain. Values outside [0, 1] warn.}
}
\value{
  \code{reliability_curve()} returns a data frame with one row per bin (empty
  bins included, so the shape is stable): bin cut labels, mean predicted
  probability, observed event frequency, bin size, and total row weight.
  Attributes: \code{n_used} (rows contributing), \code{n_rows} (all rows),
  and \code{dropped}, a mutually-exclusive accounting of every excluded row
  (missing probability, missing truth, out-of-range probability, out-of-range
  truth) that adds up to \code{n_rows - n_used}.

  \code{ece()} returns a single numeric: the sample-size-weighted mean absolute
  gap between predicted probability and observed frequency across non-empty
  bins, or \code{NA} when nothing is scored. It warns below 30 usable rows
  and states the sample it used.

  \code{selection_curve()} returns a data frame with one row per floor:
  \code{floor}, \code{coverage} (share of rows kept by \code{p >= floor}),
  \code{n_kept}, and \code{pos_rate}, the event prevalence among kept rows.
  This is a coverage/PPV view; accuracy would additionally need the true
  negatives. Attributes \code{n_usable} and \code{n_rows} bound it; a warning
  fires when no row is usable.
}
\description{
Three descriptive tools for auditing the probabilities Jev returns on your
own traffic and your own gold labels. None of them calibrates anything or
tests a hypothesis: no standard errors, no bands, no p-values. A 10-bin
curve from 50 rows is a sketch; quote the \code{n_used} attribute next to every
number you report.
}
\details{
\code{ece()} is the classic equal-width-binning estimator of expected
calibration error (Naeini et al.; popularised by Guo et al. 2017). Two
properties to remember when quoting it:

\itemize{
  \item It is a point estimate, not an upper bound. There are populations
    (for example, equal-mass bins at p = 0.01 and p = 0.09 with deterministic
    truths) where the binned estimate sits well below the true error, and the
    other direction happens too.
  \item It is not the bias-corrected estimator of Nixon et al. 2022. Numbers
    from the two are not comparable.
}

The drop filters are deliberately strict because a probability outside [0, 1]
or a truth code outside \code{\{0, 1, NA\}} signals a broken pipeline, not noise; the
dropped rows are counted, not silently discarded, and each exclusion bucket
names its own mechanism.
}
\examples{
library(Rjif)
set.seed(1)
df <- data.frame(p = runif(200), truth = rbinom(200, 1, 0.3))
df$p[df$p < 0.4] <- df$p[df$p < 0.4] * 0.4   # some overprediction at the top

rc <- reliability_curve(df)
attr(rc, "n_used")
rc$dropped                    # where every excluded row went

ece(df)
selection_curve(df)           # coverage and prevalence as the floor rises

# rows dropped for bad data are accounted for, not hidden
df2 <- rbind(df, data.frame(p = 1.5, truth = 2), data.frame(p = NA, truth = 1))
length(attr(reliability_curve(df2), "dropped"))
}
\seealso{\code{\link{jev_score_many}}}
