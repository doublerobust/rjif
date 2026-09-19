\name{jev_usage}
\alias{jev_usage}
\alias{jev_usage_reset}
\alias{withr_options}
\title{Usage Counters and Scoped Options}
\usage{
jev_usage()

jev_usage_reset()

withr_options(expr, ...)
}
\arguments{
  \item{expr}{expression to evaluate under the temporary options.}

  \item{...}{named option values, e.g. \code{Rjif.transport =
    rjif_mock_transport}.}
}
\value{
  \code{jev_usage()} returns a list with \code{calls} (decoded responses
  counted since the last reset; a transport that throws, or a response
  rejected before accounting, counts zero calls, so this is not a complete
  attempt or billing ledger), \code{input_tokens}, and \code{est_cost_usd}. Token counts
  come from the API's usage block when present; the cost estimate multiplies
  input tokens by a hard-coded rate and ignores output tokens (free in the
  pricing at time of writing), so treat it as an order-of-magnitude figure
  and check the vendor's current price sheet before budgeting. A malformed
  usage block counts the call and adds zero tokens rather than corrupting the
  counters.

  \code{jev_usage_reset()} zeroes the counters invisibly.

  \code{withr_options()} evaluates \code{expr} with the given options set,
  then restores the previous values even if \code{expr} errors, and returns
  the value of \code{expr} invisibly.
}
\description{
\code{jev_usage()} accumulates call and token counters across a session so
you can measure the cost of a workflow on real traffic instead of guessing.
\code{withr_options()} runs an expression with options temporarily overridden
(the test suite uses it to swap transports without leaking global state).
}
\examples{
library(Rjif)
options(Rjif.transport = rjif_mock_transport)
jev_usage_reset()
invisible(jif("some text", "is this a question?"))
jev_usage()$calls
options(Rjif.transport = NULL)

# scoped options: the transport swap does not outlive the expression
withr_options(Rjif.transport = rjif_mock_transport, jif("a", "b"))
}
\seealso{\code{\link{jev_eval}}}
