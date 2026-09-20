\name{jev_eval}
\alias{jev_eval}
\alias{jev_noul_q}
\alias{jev_choice_q}
\alias{jev_score_q}
\alias{jvalue}
\alias{jprob}
\alias{jconf}
\title{Mixed Questions in One Call, and the Answer Accessors}
\usage{
jev_eval(state, questions, model = getOption("Rjif.model", "jev-latest"))

jev_noul_q(instructions, criteria = NULL)

jev_choice_q(instructions, criteria)

jev_score_q(instructions, criteria)

jvalue(ans)

jprob(ans)

jconf(ans)
}
\arguments{
  \item{state}{single non-NA string or a non-empty list (structured records,
    nested fields, or chat logs). Lists serialize as JSON objects or arrays
    according to their names. The API takes one state per call; a character
    vector is rejected with a pointer to \code{\link{jev_score_many}}.}

  \item{questions}{named list of question specs built with the
    \code{jev_*_q()} constructors. Names are for your code (answers come back
    under the same keys) and are not sent to the model, so put the complete
    meaning in the question itself.}

  \item{instructions}{what the model should decide, as a plain-language
    string ("which team should handle this?"). For a Noul question this is
    the yes/no assertion.}

  \item{criteria}{for Choice: a named list or character vector mapping each
    option key to its rubric description
    (\code{billing = "payments, invoicing, refunds"}); a \code{NULL}
    description is allowed. For Score: a character vector of ordered level
    descriptions, lowest severity first, or a list of structured level
    objects (\code{list(what = "...", examples = list("...")}) with the same
    fields on every level. Score takes 2 to 10 levels. For Noul: an optional
    named list or character vector with \code{true}, \code{false}, or both,
    refining what each answer means (\code{list(true = "explicitly reported
    by a clinician", false = "mentioned only as ruled out")}). The shorthand
    \code{jif(state, "assertion", list(true = "present", false = "absent"))}
    passes these criteria through. Without criteria it sends none.
    Levels are labelled \code{"0"} to \code{k-1} in the response legend.
    Choice takes 2 to 255 options.}

  \item{model}{Jev model name; defaults to the \code{Rjif.model} option, then
    \code{"jev-latest"}.}

  \item{ans}{one answer object from \code{jev_eval()} or
    \code{\link{jif}} (class \code{jev_answer}).}
}
\value{
  \code{jev_eval()} returns a \code{jev_answers} list with one answer per
  question, in the order given; printing shows the value, the probability
  vector where present, the confidence, and the legend. Session token and
  call counters accumulate in \code{\link{jev_usage}}. A contract-violating
  answer returns with value \code{NA}, \code{probs} \code{NULL}, confidence
  \code{NA}, and an explanatory \code{contract} field, so a malformed
  response can never populate downstream analytics. (A response missing a
  question's answer object outright is an error, not a NA answer; a response
  whose question types disagree with the request abstains its rows.)

  \code{jev_*_q()} return \code{jev_question} specs.

  \code{jvalue()} returns the answer: a probability in [0, 1] (Noul), the
  chosen option name (Choice), or the continuous level position in 0..k-1
  (Score). \code{jprob()} returns the probability behind the decision: P(yes)
  for Noul, P(chosen option) for Choice, the API confidence for Score.
  \code{jconf()} returns the API's confidence field, which is \code{NA} for
  Noul answers because the API sends none.
}
\description{
\code{jev_eval()} sends one state and any number of mixed questions in a
single API call. The questions are evaluated independently, so adding one
does not degrade the others. The constructors define the three judgment
types; the accessors read the three different numbers off an answer.
}
\details{
Choice answers require a normalized distribution over the offered options
and confidence in [0, 1]. The chosen option may trail the largest displayed
probability by at most 0.02 (plus a 1e-9 floating-point guard).
Score answers require confidence in [0, 1], probabilities and legend keys
covering exactly \code{0} through \code{k-1}, and a score within 0.05 of the
probability-weighted mean of the level indices. The distribution must sum to
1 within 0.01. Both comparisons include a 1e-9 floating-point guard;
accepted probabilities are normalized before the mean is
computed. Bare \code{list(score = 1, confidence = 1)} answers are rejected.

Do not confuse the accessors. \code{jconf()} summarizes how concentrated the
probability distribution is (high confidence means the mass sits on one
answer); it is not the probability that the answer is correct, and should not
be used to gate Noul decisions (use \code{jprob()} via
\code{\link{jif}}'s \code{confidence_floor}). The probability vectors over
options or levels are the more useful quantity anyway: thresholds belong on
them, and any threshold should be checked on your own data with
\code{\link{reliability_curve}} before you rely on it.

Independent questions over the same state belong in ONE \code{jev_eval()}
call, including speculative ones (code consumes only the relevant answers);
a second call is warranted only when an earlier answer changes what you would
ask.
}
\examples{
library(Rjif)
options(Rjif.transport = rjif_mock_transport)

qs <- list(
  urgent = jev_noul_q("does this convey urgency?"),
  dept   = jev_choice_q("which team should handle this?",
                        list(billing = "payments, invoicing, refunds",
                             technical = "bugs, outages, integrations")),
  frus   = jev_score_q("how frustrated is the customer?",
                       c("calm", "frustrated", "very angry")))

res <- jev_eval("Help! My payouts have been failing for 3 days.", qs)
res
jprob(res$urgent)   # P(yes) for the noul question
jvalue(res$dept)    # the chosen option
jprob(res$dept)     # P(chosen option)
jconf(res$dept)     # the API's confidence (distribution concentration)

options(Rjif.transport = NULL)
}
\seealso{\code{\link{jif}}, \code{\link{jev_score_many}},
\code{\link{jev_usage}}}
