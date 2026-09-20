\name{jev_score_many}
\alias{jev_score_many}
\alias{jmatch}
\title{One Question Over Many States, and Branch Routing}
\usage{
jev_score_many(state_vec, question, ..., threshold = 0.5,
               confidence_floor = 0, batch = 16L, cache = NULL,
               model = getOption("Rjif.model", "jev-latest"))

jmatch(state, branches, ..., instructions = NULL, confidence_floor = 0,
       fallback = NA_character_,
       abstain_options = c("none", "other", "unspecified", "human",
                           "ambiguous", "unsure"),
       model = getOption("Rjif.model", "jev-latest"))
}
\arguments{
  \item{state_vec}{character (or factor) vector of per-row states. The API
    takes one state per call, so this loops: one HTTP request per row. Paste
    your columns together first (for example with \code{paste()}).}

  \item{question}{a question spec (\code{\link{jev_noul_q}},
    \code{\link{jev_choice_q}}, \code{\link{jev_score_q}}) or a single string
    read as a Noul assertion.}

  \item{...}{for \code{jev_score_many()}: with the string shorthand, a named
    list/vector of option descriptions makes it a Choice question (same rule
    as \code{\link{jif}}).}

  \item{threshold}{same meaning as in \code{\link{jif}}: a probability for
    Noul, a rubric level for Score, unused for Choice.}

  \item{confidence_floor}{numeric in [0, 1], or \code{NA} to abstain on every
    row (useful in tests). Same policy as \code{\link{jif}}, including the
    two-sided noul window when the floor exceeds \code{threshold}; abstained
    rows carry the reason in the \code{error} column.}

  \item{batch}{chunk size for the internal loop. Does not batch the API: each
    non-NA state is still one HTTP request, and NA states are skipped without
    a call. With \code{cache} set, it also controls how much work an interrupt
    can lose (the cache is rewritten once per chunk); without a cache it
    changes nothing the caller observes.}

  \item{cache}{\code{NULL} (default) or a path string. When set, results are
    persisted to that file after every chunk and reloaded at the start of a
    later call with the same path, question, and row count: an interrupted
    bulk run resumes instead of re-billing completed rows. Rows whose earlier
    attempt errored are re-run by default; set
    \code{options(Rjif.cache_rerun_errors = FALSE)} to keep them cached. The
    returned frame gains an \code{n_resumed} attribute. A cache file holds API
    answers in row order (states are not stored) but can still disclose your
    data: protect it accordingly.}

  \item{model}{Jev model name; defaults to the \code{Rjif.model} option, then
    \code{"jev-latest"}.}

  \item{state}{single string for \code{jmatch()} to route.}

  \item{branches}{named list or character vector of branch descriptions
    (\code{key = "what this branch covers"}); passed as the criteria of a
    Choice question.}

  \item{instructions}{the routing question; defaults to "Which single
    description best matches the state?".}

  \item{fallback}{value returned when the model abstains or picks an
    abstention option (see \code{abstain_options}), or the floor gates it
    out. Transport and API errors are NOT caught into \code{fallback};
    they propagate.}

  \item{abstain_options}{option names that mean "no route" even when the
    model picks them confidently (case-insensitive).}
}
\value{
  \code{jev_score_many()} returns a data frame with one row per input state,
  in order, with columns: \code{decision} (logical tri-state for every
  question type: TRUE/FALSE for decided Noul and Score rows, TRUE for any
  decided Choice row, NA when undecided; the chosen option name is in
  \code{option}, not in \code{decision}), \code{option} (the option name for
  Choice, "true"/"false" for Noul, or the continuous rubric position as text,
  e.g. \code{"1.050"}, for Score), \code{p} (the \code{\link{jprob}} behind
  the decision), \code{confidence} (the API confidence; \code{NA} for Noul),
  \code{abstained}, and \code{error} (per-row failure or abstention text,
  redacted of any API key; empty string when clean. Since the two-sided noul
  floor, rows policy-abstained by \code{confidence_floor} also record their
  reason here, same wording as \code{\link{jif_reason}}). Attributes
  \code{n_abstained} and \code{question_type} summarise the run,
  \code{n_resumed} states how many rows came from \code{cache}; the row count
  is \code{nrow(df)}.

  \code{jmatch()} returns the chosen branch name, or \code{fallback}.
}
\description{
\code{jev_score_many()} applies one judgment to a whole vector of states and
collects the results in a data frame, with per-row error capture: one
unparsable narrative abstains its own row instead of discarding the other
4,999. \code{jmatch()} routes one state to the best-matching branch of a set
you define.
}
\details{
The abstention column exists because a bare \code{if (df$decision[i])} on an
\code{NA} errors, which is the point: an undecided clinical row must not fall
into the negative branch because a helper wrote \code{FALSE} for it. Read
\code{abstained} and \code{error} together with \code{decision}, and hand the
abstentions to whatever your escalation queue is.

Attach gold labels to the result and the frame becomes the input to
\code{\link{reliability_curve}}, \code{\link{ece}}, and
\code{\link{selection_curve}}, whose \code{p} and \code{truth} column names
match the defaults here.
}
\examples{
library(Rjif)
options(Rjif.transport = rjif_mock_transport)

narratives <- c(
  "the patient died 12 days after the second dose",
  "mild headache resolved after acetaminophen",
  "admitted to the ICU with neutropenic fever",
  "rash on day 3, treated topically, ongoing")

df <- jev_score_many(narratives,
                     "a serious adverse event is being reported",
                     confidence_floor = 0.7)
df
sum(df$abstained)            # escalate these; do not threshold them away

# routing one ticket with jmatch()
jmatch("the customer says the refund never arrived",
       list(billing = "payments, refunds, invoices",
            technical = "bugs and outages",
            sales = "pricing and upgrades"))

options(Rjif.transport = NULL)
}
\seealso{\code{\link{jif}}, \code{\link{reliability_curve}}}
