\name{Rjif-package}
\docType{package}
\alias{Rjif}
\alias{Rjif-package}
\title{Judgment-Based Branching with TypeSafe Jev}
\description{
Rjif wraps TypeSafe's Jev, a "System One" decision model that reads
free text or structured data (the state) plus typed questions and returns
probabilities instead of prose. The package exposes those probabilities as
conditionals in R, with an explicit NA lane for uncertain answers, and ships
descriptive tools to check calibration on your own labeled data before a
cutoff becomes a rule people depend on.

Three question types: a Noul question returns the probability that one
assertion holds; a Choice question picks one option from a set you define and
returns the full distribution; a Score question returns a continuous,
probability-weighted position on an ordered rubric (it can land between
levels) plus the per-level distribution.

Set the API key in the environment variable \code{TYPESAFE_API_KEY}. The key
is never printed, logged, or embedded in error messages: API responses,
abstention reasons, and package diagnostics are scrubbed of the key before
they are returned or signaled. (Request objects you build yourself are yours;
the package scrubs what comes back out.) For offline work,
\code{\link{rjif_mock_transport}} replaces the HTTP
layer with deterministic (statistically meaningless) test values.
}
\details{
Typical flow:

\enumerate{
  \item Build a question with \code{\link{jev_noul_q}},
    \code{\link{jev_choice_q}}, or \code{\link{jev_score_q}} (or use the
    string shorthand accepted by \code{\link{jif}}).
  \item Ask it with \code{\link{jif}} (single decision),
    \code{\link{jev_score_many}} (one question over a vector of states), or
    \code{\link{jev_eval}} (several mixed questions over one state).
  \item Branch with \code{\link{j_ifelse}}, which has an \code{unknown} arm
    for abstentions, or test \code{\link{jif_abstained}} directly.
  \item Audit the resulting probabilities against gold labels with
    \code{\link{reliability_curve}}, \code{\link{ece}}, and
    \code{\link{selection_curve}}.
}
}
\author{Yue Shentu}
\keyword{package}
