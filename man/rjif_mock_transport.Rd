\name{rjif_mock_transport}
\alias{rjif_mock_transport}
\title{Offline Mock Transport (Test Values Only)}
\usage{
rjif_mock_transport(body = NULL, answers = NULL)
}
\arguments{
  \item{body}{the request body, when used directly as a transport via
    \code{options(Rjif.transport = rjif_mock_transport)}.}

  \item{answers}{a named numeric vector or list mapping question text to a
    scripted value in [0, 1], when called to build a scripted transport.}
}
\value{
  A transport function (or, when called directly with a body, the mock
  response list). Answers follow the real API's shapes: Noul answers carry a
  value and no confidence; Choice and Score answers carry full probability
  distributions, a confidence, and (for Score) a legend mapping level indices
  to descriptions.
}
\description{
A deterministic stand-in for the HTTP layer so code, tests, and examples run
with no API key and no network. The numbers are produced by a hash of the
question text, not by a model: they are statistically meaningless and must
never be used to exercise a threshold, estimate a cost, or draw a
conclusion. Scripted values (option labels or full question texts) let tests
force a particular answer; script keys are matched case- and
whitespace-insensitively against what the API would actually receive (the
instructions text).
}
\details{
Two calling conventions:

\itemize{
  \item \code{options(Rjif.transport = rjif_mock_transport)} for unscripted,
    deterministic answers.
  \item \code{options(Rjif.transport = rjif_mock_transport(c("question text"
    = 0.91)))} to steer individual questions (for a Score question the
    scripted value places the position on the level scale, so 0.91 lands at
    91 percent of the way to the top level; for a Choice question, the named
    option carries the scripted probability and the rest are renormalised).
}

The mock exists for plumbing tests (abstention lanes, contract validation,
credential redaction). Its own header comment documents each per-type rule,
including the deliberate quirks that make the test suite useful: a
deterministic hash that can concentrate 0.9996 of a Choice distribution on
one option, which is exactly what a real model sometimes does and why the
NA-lane tests need it.
}
\examples{
library(Rjif)
options(Rjif.transport = rjif_mock_transport(c("is this urgent?" = 0.91)))
jif("the patient stopped breathing", "is this urgent?")
options(Rjif.transport = NULL)
}
\seealso{\code{\link{jev_eval}}, \code{\link{withr_options}}}
