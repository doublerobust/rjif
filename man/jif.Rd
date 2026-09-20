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
  \item{state}{single non-NA string (the text or record to judge, for example
    an adverse-event narrative or a support ticket), or a non-empty
    \code{list} for structured states (records, nested fields, chat logs),
    serialized to the API as a JSON object or array, according to names.
    The API takes one state per call;
    to score a vector of states use \code{\link{jev_score_many}}. A character
    vector is always an error.}

  \item{question}{either a single string (read as a Noul assertion: "is this
    true of the state?") or a question spec built with
    \code{\link{jev_noul_q}}, \code{\link{jev_choice_q}}, or
    \code{\link{jev_score_q}}.}

  \item{...}{for \code{jif()}: with the string shorthand, a single named
    list or character vector. Names within \code{\{true, false\}} only are
    read as Noul outcome criteria (\code{true = "what a yes means"}); any
    other names make it a Choice question (\code{option = description}).
    Nothing else is read from these arguments.}

  \item{threshold}{numeric. For a Noul question: the probability at or above
    which the assertion counts as TRUE (default 0.5). For a Score question
    this is a rubric LEVEL, not a probability: with five levels,
    \code{threshold = 2} means "the position reaches moderate or worse". For
    a Choice question it is unused (the answer is the option name).}

  \item{abstain}{value returned when the question cannot be decided
    (default \code{NA}). \code{NA} is deliberate: \code{if (jif(...))} then
    errors instead of routing an undecided row into the \code{else} branch.}

  \item{confidence_floor}{numeric in [0, 1] (default 0, no gating). For
    Choice and Score questions, and for Noul when the floor is at or below
    \code{threshold}: if the probability behind the decision (see
    \code{\link{jprob}}) is below the floor, \code{jif()} abstains. For NOUL
    with \code{floor > threshold} the floor acts as a TWO-SIDED evidence
    standard: decide TRUE when P(yes) >= floor, decide FALSE when
    P(yes) <= 1 - floor, abstain in between (the middle is genuinely
    uncertain; a confident NO is not forced to abstain, which a one-sided
    floor did before). \code{NA} means "abstain always", which is useful in
    tests. A finite value outside [0, 1] warns: \code{<= 0} never
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
  \code{\link{jprob}}, \code{\link{jconf}}, and \code{\link{jprobs}}; it
  also carries \code{model_requested} and \code{model_returned} attributes
  naming the alias you asked for and the resolved model that answered, so
  provenance survives the extraction that previously dropped the envelope),
  and \code{abstain_reason}.

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
  value. When one arm is a literal map and the other a pre-built map, each is
  consulted in its supported representation, YES before NO: a tag present in
  the yes side wins over the same tag in the no side. Duplicate tags within
  an arm select the first occurrence. A match in either representation
  decides (external audit r2 finding 1 fixed a regression that
  consulted only literal tags first and misrouted mixed maps to
  \code{unknown}). Any other arm expression (for example \code{setNames(...)})
  is forced when its labels are inspected, with the same side-effect caveat
  the old code had: write branch arms as literal \code{list()}/\code{c()}
  calls, or build them into a variable first, to keep routing lazy.

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
Abstention reasons reported by \code{jif_reason()}:

\itemize{
  \item \code{"no answer value from the API"}: no usable value remained
    after contract validation. An omitted answer object is an error instead.
  \item \code{"confidence unavailable with a floor set"}: a value exists
    but no probability backs it, and a floor was required.
  \item \code{"probability 0.612 < confidence_floor 0.700"}: an answer
    failed a one-sided floor (Choice or Score, for example).
  \item \code{"probability 0.500 is in the uncertain middle of the two-sided confidence_floor 0.700 (negative cut 0.300)"}:
    a Noul answer cleared neither cutoff.
  \item \code{"confidence_floor is NA; refusing to decide"}: the caller
    explicitly refused a decision with an NA floor.
}

For a Noul floor at or below threshold, first require P(yes) >= floor, then
compare P(yes) with threshold. FALSE remains reachable. If floor > threshold
but floor < 0.5, the cutoffs overlap and TRUE takes precedence. A floor above
0.5 gives a nonempty abstention window.
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
