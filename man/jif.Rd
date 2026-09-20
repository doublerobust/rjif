\name{jif}
\alias{jif}
\alias{j_ifelse}
\alias{jif_abstained}
\alias{jif_reason}
\title{A Judgment-Based If}
\usage{
jif(state, question, ..., threshold = 0.5, abstain = NA,
    confidence_floor = 0, model = getOption("Rjif.model", "jev-latest"))

j_ifelse(test, yes, no, unknown = NA)

jif_abstained(x)

jif_reason(x)
}
\arguments{
  \item{state}{single string: the text or record to judge (for example, an
    adverse-event narrative or a support ticket). The API takes one state per
    call; to score a vector of states use \code{\link{jev_score_many}}.}

  \item{question}{either a single string (read as a Noul assertion: "is this
    true of the state?") or a question spec built with
    \code{\link{jev_noul_q}}, \code{\link{jev_choice_q}}, or
    \code{\link{jev_score_q}}.}

  \item{...}{for \code{jif()}: with the string shorthand, a single named
    list or character vector of option descriptions turns the question into
    a Choice question (\code{option = description}). Nothing else is read
    from these arguments.}

  \item{threshold}{numeric. For a Noul question: the probability at or above
    which the assertion counts as TRUE (default 0.5). For a Score question
    this is a rubric LEVEL, not a probability: with five levels,
    \code{threshold = 2} means "the position reaches moderate or worse". For
    a Choice question it is unused (the answer is the option name).}

  \item{abstain}{value returned when the question cannot be decided
    (default \code{NA}). \code{NA} is deliberate: \code{if (jif(...))} then
    errors instead of routing an undecided row into the \code{else} branch.}

  \item{confidence_floor}{numeric in [0, 1] (default 0, no gating). If the
    probability behind the decision (see \code{\link{jprob}}) is below the
    floor, \code{jif()} abstains. \code{NA} means "abstain always", which is
    useful in tests. A finite value outside [0, 1] warns: \code{<= 0} never
    gates, \code{> 1} gates everything.}

  \item{model}{Jev model name; defaults to the \code{Rjif.model} option,
    then \code{"jev-latest"}.}

  \item{test}{a value returned by \code{jif()} (or any length-1 vector).}

  \item{yes, no, unknown}{values returned when \code{test} is decided TRUE,
    decided FALSE, or abstained. \code{unknown} is the escalation arm (send
    to a human, hold for review).}

  \item{x}{a value returned by \code{jif()}, or a bare \code{NA} (also
    treated as an abstention).}
}
\value{
  \code{jif()} returns TRUE/FALSE (Noul or Score questions) or the chosen
  option name (Choice questions), or \code{abstain} (default \code{NA}) when
  uncertain. The result carries attributes \code{abstained}, \code{answer}
  (the full \code{jev_answer} object, readable with \code{\link{jvalue}},
  \code{\link{jprob}}, \code{\link{jconf}}), and \code{abstain_reason}.

  \code{j_ifelse()} returns \code{unknown} if \code{test} abstained or is a
  bare \code{NA}; otherwise, when \code{test} is a string (a decided Choice
  result) and \code{yes} or \code{no} is a named vector, the element whose
  name matches the chosen option, or \code{unknown} if no branch name matches
  (a routing decision never falls through to the wrong arm); an unmatched
  string with plain scalars for \code{yes}/\code{no} returns \code{yes}. For
  TRUE/FALSE tests it returns \code{yes} or \code{no}.
  Exactly one branch expression is evaluated when arms are written as literal
  \code{list(...)} or \code{c(...)} calls: the labels are read without
  evaluation and only the winning element runs, so side effects (issuing a
  label, paging someone) in losing branches never fire. An arm supplied as a
  pre-built variable was already evaluated by the caller and is routed by
  value. Any other arm expression (for example \code{setNames(...)}) is
  forced when its labels are inspected, with the same side-effect caveat the
  old code had: write branch arms as literal \code{list()}/\code{c()} calls,
  or build them into a variable first, to keep routing lazy.

  \code{jif_abstained()} returns a single logical; TRUE also covers a bare
  \code{NA}. \code{jif_reason()} returns the abstention reason string, or
  \code{NA_character_} for a decided result.
}
\description{
\code{jif()} asks Jev one question about one state and turns the answer into
an R decision. Uncertain answers come back \code{NA}, never \code{FALSE}: a
missing answer and a low answer are different states of the world, and a
package that conflates them mis-routes exactly the cases a human would look
at. \code{j_ifelse()} branches on the result with an explicit
\code{unknown} arm.
}
\details{
Three ways \code{jif()} abstains, distinguished by \code{jif_reason()}:

\itemize{
  \item \code{"no answer value from the API"}: Jev returned nothing usable
    (a contract-violating answer, or a value the validator rejected). A
    missing datum; not evidence of "false".
  \item \code{"confidence unavailable with a floor set"}: a decision exists
    but no probability backs it, and you required one.
  \item \code{"probability 0.612 < confidence_floor 0.700"}: an answer
    exists and fell below your floor. This is your policy, not the model's
    uncertainty.
}

The first two call for better state or a relaxed floor; the third is a
threshold decision. Don't pool them in reporting.
}
\examples{
# offline demo through the mock transport (numbers are test hashes, not judgments)
library(Rjif)
options(Rjif.transport = rjif_mock_transport)

test <- jif("the patient was hospitalized for six days after the infusion",
            "a serious adverse event is being reported",
            confidence_floor = 0.7)
if (jif_abstained(test)) jif_reason(test) else test

j_ifelse(test,
  yes     = "auto-triage to the safety queue",
  no      = "leave in the routine pile",
  unknown = "send to the human reviewer")

# choice shorthand: string question + named option descriptions
route <- jif("the customer says the refund never arrived",
             "what should the agent do?",
             list(refund = "money back to the payer",
                  escalation = "hand to a supervisor",
                  clarify = "ask a follow-up question"))
route

# score: the threshold is a rubric level, and the answer is continuous
sev <- jif("the patient was hospitalized for six days after the infusion",
           jev_score_q("how severe is this event?",
                       c("none", "mild", "moderate", "severe",
                         "life-threatening")),
           threshold = 2)
sev
jvalue(attr(sev, "answer"))  # the continuous position, may land between levels

options(Rjif.transport = NULL)
}
\seealso{\code{\link{jev_eval}}, \code{\link{jev_score_many}},
\code{\link{jmatch}}, \code{\link{rjif_mock_transport}}}
