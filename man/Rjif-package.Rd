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
Transport and cache options (set with \code{options()}):
\itemize{
  \item \code{Rjif.timeout = 120}: hard per-request timeout in seconds;
    fractional seconds are supported.
  \item \code{Rjif.retries = 3L}: additional attempts after the first,
    for HTTP 429/529 or curl transport errors only. Zero disables retries.
  \item \code{Rjif.retry_base = 1}: initial exponential backoff in seconds.
  \item \code{Rjif.retry_cap = 30}: cap on computed backoff including jitter.
  \item \code{Rjif.retry_max_wait = 120}: ceiling on every sleep, including
    a Retry-After header (delta-seconds or HTTP date).
  \item \code{Rjif.cache_rerun_errors = TRUE}: rerun failed rows on resume.
    FALSE retains failures too. Completed policy abstentions are always reused.
}
The computed wait is \code{min(cap, base * 2^(attempt - 1))} with up to
25 percent jitter, capped again by cap and retry_max_wait. A nonnegative
numeric Retry-After replaces that wait, capped by retry_max_wait. Invalid
nonpositive or nonfinite timing options use their defaults. Retries must
coerce to a nonnegative integer, otherwise the default is used.
Timeout/retry options apply to the default HTTP transport; custom
\code{Rjif.transport} functions manage their own timing and retries.
HTTP 401/403/404/422 and other statuses outside 429/529 are not retried.
Final HTTP/transport failures report attempt counts. Exhausted retryable
failures warn that the vendor may have billed an attempt without delivering
an answer. \code{\link{jev_usage}} counts decoded responses, not attempts.

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
