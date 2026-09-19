\name{rjif_mock_transport}
\alias{rjif_mock_transport}
\title{Offline Mock Transport (Test Values Only)}
\usage{
rjif_mock_transport(body = NULL, answers = NULL)
}
\arguments{
  \item{body}{the request body, when used directly as a transport via
    \code{options(Rjif.transport = rjif_mock_transport)}.}

  \item{answers}{a named numeric vector or list mapping question instruction
    text to a scripted value in [0, 1], when called to build a scripted
    transport.}
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
question's instruction text, not by a model: they are statistically
meaningless and must never be used to exercise a threshold, estimate a cost,
or draw a conclusion.
}
\details{
Two calling conventions:

\itemize{
  \item \code{options(Rjif.transport = rjif_mock_transport)} for unscripted,
    deterministic answers.
  \item \code{options(Rjif.transport = rjif_mock_transport(c("question text"
    = 0.91)))} to steer individual questions. Script keys are matched
    case-insensitively and ignoring leading/trailing whitespace, against the
    question's instruction text (what the API would receive); a key equal to
    a Choice OPTION label does not steer that option. For a Noul question the
    scripted value is returned directly. For a Score question the value is
    placed on the level scale (0.91 of a 3-level rubric lands at position
    1.82, mass split between the adjacent levels). For a Choice question the
    script steers whichever option is initially top-ranked by the hash to the
    scripted probability, with the rest renormalised; after renormalisation
    the winner can change.
}

Unscripted values are bounded: the hash signal lies in [0.001, 0.999) and
Choice probabilities are a softmax over it, so a two-option question can
never concentrate more than about 0.73 on one option (analytic bound
0.7307; 0.7283 was the maximum observed over 500 trials). Deterministic does
not mean extreme.

The mock exists for plumbing tests (abstention behavior, contract validation,
credential redaction). Its own header comment documents each per-type rule.
}
\examples{
library(Rjif)
options(Rjif.transport = rjif_mock_transport(c("is this urgent?" = 0.91)))
jif("the patient stopped breathing", "is this urgent?")
options(Rjif.transport = NULL)
}
\seealso{\code{\link{jev_eval}}, \code{\link{withr_options}}}
