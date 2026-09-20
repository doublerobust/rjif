# Rjif: `if` statements that use a model's judgment

Rjif is a thin R wrapper around [TypeSafe's Jev](https://typesafe.ai). Jev
reads some text or data (the "state") plus your questions, and returns
probabilities instead of prose: the probability that a statement is true, a
pick from a list of options with the full distribution, or a position on an
ordered rating scale. It's meant for the fuzzy branching you'd otherwise code
with keyword rules. Is this report about a side effect? Which team owns this
ticket? How severe is it?

The output is a predicted probability, so treat it like one: keep an explicit
home for the uncertain cases, and check calibration on your own data before
turning a cutoff into a rule.

```r
library(Rjif)

test <- jif(narrative, "a serious adverse event is being reported",
            confidence_floor = 0.7)

action <- j_ifelse(test,
  yes     = "auto-triage to the safety queue",
  no      = "leave in the routine pile",
  unknown = "send to the human reviewer")
```

## Three kinds of question

| Constructor | What it asks | What comes back |
| --- | --- | --- |
| `jev_noul_q()` | is this statement true of the state? | probability in [0, 1] (the API sends no confidence for these) |
| `jev_choice_q()` | which of these options? | chosen option, probability of each option, confidence |
| `jev_score_q()` | where on this ordered scale? | continuous position in 0..k-1 (can land between levels), probability of each level, confidence |

You can mix the three in one call. Questions are scored independently, so
adding one doesn't change the others.

## Uncertain answers are `NA`, not `FALSE`

If the model gives no usable number, or the answer fails your confidence
policy, `jif()` returns `NA`. A plain `if (jif(...))` then stops with an error.
Use `j_ifelse()` and give the `unknown` arm somewhere to go.

For a true/false (`jev_noul_q()`) question with a floor above the threshold,
the floor is two-sided. With `threshold = 0.5` and
`confidence_floor = 0.7`: `p >= 0.7` decides TRUE, `p <= 0.3` decides FALSE,
and values in between abstain. A floor at or below the threshold stays
one-sided: first require `p >= floor`, then decide using `p >= threshold`.
This can produce FALSE as well as TRUE: p = 0.4, floor = 0.4, threshold = 0.5
returns FALSE. If the two-sided cutoffs overlap (floor below 0.5), TRUE takes
precedence in the overlap. Prefer a floor above 0.5 for a nonempty abstention
window. Choice and Score always use a one-sided floor on `jprob()`.
An NA floor refuses every decision, with reason
`"confidence_floor is NA; refusing to decide"`.

`jif_reason()` says which case you're in:

- `"no answer value from the API"`: Jev returned nothing usable (the answer
  was missing, or the validator rejected it). A missing datum, not evidence
  of "false".
- `"probability 0.612 < confidence_floor 0.700"`: there is an answer, and you
  decided it's not strong enough to act on.
- `"probability 0.500 is in the uncertain middle of the two-sided
  confidence_floor 0.700 (negative cut 0.300)"`: a noul answer that cleared
  neither side of the window.
- `"confidence unavailable with a floor set"`: a decision exists but no
  probability backs it, and you required one. The validator rejects answer
  shapes without a usable probability, so in ordinary traffic you'll rarely
  see this; it's a guard, not a common path.

The first two call for different fixes (send better state, vs move the
floor). A response that omits a question's answer object entirely is an
error, not an abstention; the batch helper catches that per row.

## Confidence is spread, not accuracy

Two different numbers come back, and they answer different questions.
`jprob()` is the probability behind the decision: P(yes) for noul, P(chosen
option) for choice, the API confidence for score. `jconf()` is the API's
confidence, which its docs define as how concentrated the probability
distribution is. High confidence means the model put its mass on one answer,
not that the answer is right.

For a score question, `jif(threshold = 2)` compares against the level (the
position reaches "moderate"), not against a probability.

## Batches

The API takes one state per call, so `jev_score_many()` loops over your
vector, one HTTP request per row, and returns a data frame with `decision`,
`option`, `p`, `confidence`, `abstained`, and `error` columns. Errors are
captured per row: one unparsable narrative doesn't discard the other 4,999.
Policy abstentions land in `error` too, so a resumed batch tells you why a
row was never decided. For score rows the `option` column holds the position
as text, e.g. `"1.050"`.

For a long batch, pass `cache = "path.rds"`. Each completed chunk is saved;
a later call skips those rows without new requests. If interrupted mid-chunk,
that chunk may need to run again. Rows with failed requests or invalid answers
rerun by default (`Rjif.cache_rerun_errors = TRUE`); set it to FALSE to retain
failures. Completed policy abstentions are reused. The cache binds to the
full question and criteria, requested model, row count, threshold, floor, and
an MD5 digest of the state vector's contents and order: reorder or correct an
extract and the cache REFUSES (error, zero new calls, prior file left intact)
instead of silently re-attaching old judgments to the wrong records. Point
`cache` at a fresh path when you mean to start over. `attr(df, "n_resumed")`
counts reused rows.

Use one writer per cache path: rows are matched by position, and the digest
detects a changed vector but cannot say which row moved. The file contains
question text, criteria, run settings, and results in row order. It stores no
plaintext states (only their digest) but can still disclose your data; protect
it accordingly.

## Checking calibration

The package ships three descriptive tools. Point them at the batch data frame
after you've attached gold labels:

```r
df$truth <- gold_sae_labels   # from adjudication, not from another model

reliability_curve(df)   # per bin: mean predicted probability vs observed frequency
ece(df)                 # one summary number: expected calibration error
selection_curve(df)     # as you raise the floor: what share of rows do you
                        # keep, and what's the event rate among them?
                        # policy = "two_sided" (noul batches) instead counts
                        # the rows jif()'s two-sided floor actually DECIDES
                        # (either tail), so the curve and the decision policy
                        # can never disagree
```

Know what they are. `ece()` is a point estimate using the classic equal-width
bins (Naeini et al.; Guo et al. 2017). It is not an upper bound on the true
error, the adaptive calibration error of Nixon et al. (2019, arXiv
1904.01685), or the debiased estimator of Roelofs et al. (AISTATS 2022).
It warns under 30 usable rows; under about 100 it's mostly bin
noise. Quote `attr(rc, "n_used")` along with the number. Row accounting is
exact: every dropped row lands in exactly one `n_dropped` bucket, and
`n_used + sum(dropped) == nrow(df)`.
The `pos_rate` in `selection_curve()` is the event prevalence among kept rows,
a coverage/PPV view rather than accuracy (that would need true negatives too).
The per-bin column is called `observed_rate`, not `accuracy`: it is the
frequency of the event, and next to a perfectly calibrated `mean_p` of 0.05
it used to read as a 5% failure rate. Reliability curves only mean something
against gold truth; score Jev against another model's labels and you've
measured agreement, not calibration. And there are no standard errors, bands,
or hypothesis tests anywhere in here. A 10-bin curve from 50 rows is a sketch.

One more trap this section can't paper over: `ece()` and `reliability_curve()`
measure calibration of an event probability. The `p` column from a noul batch
is exactly that (P(assertion true)). The `p` from a score batch is the vendor
"confidence": how concentrated the level distribution is, which is not a
probability of any event, and the same number from a choice batch is P(chosen
option) -- a third thing. Calibrating a score or choice batch against event
truth errors by default and tells you why; pass
`allow_type = c("score", "choice")` if you want the numbers anyway.

## Things to know before you use it

It's a hosted API. You need a `TYPESAFE_API_KEY`, and nothing here runs
offline against a real model. Don't send patient or proprietary data until
your organization has cleared the vendor.

This package helps you check Jev's calibration. It does not fix it. Whether
the vendor's probabilities behave on your traffic is an empirical question,
and this README can't answer it.

Cost and latency grow with rows: `jev_score_many()` is one call per row, and
the `batch` argument is an internal chunk size, not request batching.

The default HTTP transport has a hard per-request timeout (`Rjif.timeout`,
120 seconds, including fractional values). HTTP 429/529 and curl connection
errors get up to `Rjif.retries` additional attempts (default 3, so at most 4
attempts). Other statuses, including 401/403/404/422, stop immediately.
The computed backoff is `min(cap, base * 2^(attempt-1))` with up to 25%
jitter, capped again by `Rjif.retry_cap`. Tune with `Rjif.retry_base` (1s),
`Rjif.retry_cap` (30s), and `Rjif.retry_max_wait` (120s, the ceiling on every
sleep). `Retry-After` (nonnegative seconds or an HTTP date) replaces the
computed wait, up to that ceiling; malformed headers fall back to backoff. Custom
transports manage their own deadlines and retries.

Final HTTP/transport failures report the attempt count. Exhausted retryable
failures also say that the vendor may have billed attempts without delivering
an answer. `jev_usage()` counts decoded responses, so failed attempts add
nothing to these counters; this is not a complete billing ledger.
`Rjif.cache_rerun_errors` defaults to TRUE. See `?Rjif-package` for all options.

`rjif_mock_transport()` is for testing with no key and no network. Its
numbers are deterministic hashes, statistically meaningless, and the source
says so.

Help pages cover the exported functions
(`?jif`, `?jev_eval`, `?reliability_curve`); inline comments carry the
rest.

## Install

```r
remotes::install_github("doublerobust/rjif")
```

Imports `httr`, `jsonlite`, `stats`; no compiled code.

## Getting a key and trying it

Get an API key at https://console.typesafe.ai/keys, then put this line in
your `~/.Renviron` file (restart R afterwards):

```
TYPESAFE_API_KEY=your-key-goes-here
```

That's the whole setup; the package reads the variable at call time and never
writes it anywhere. Then:

```r
library(Rjif)
jif("the patient was hospitalized for six days after the infusion",
    "a serious adverse event is being reported")
```

No key yet? Every example in this README and the help pages also runs
offline against a deterministic mock, so you can see the shapes first:

```r
options(Rjif.transport = rjif_mock_transport)
jif("the patient was hospitalized for six days after the infusion",
    "a serious adverse event is being reported")
```

## Tests

`Rscript tests/smoke.R` runs 227 offline assertions against the mock
transport and scripted fake responses: the abstention behavior, the API
response contract (including the live-verified continuous-score round trip),
malformed-usage handling, credential-retention boundaries, and the calibration
analytics. `tests/jifelse-laziness.R` adds 20 arm-evaluation assertions (exactly
one branch element per routing decision, including mixed literal/prebuilt
maps); `tests/r14-regressions.R` adds 51 audit-gate assertions, and on
Unix-like systems runs them against real loopback HTTP fixtures (429/529
retries, Retry-After waits, fractional timeouts). That block skips on Windows
(needs fork), so on Windows either drive the loopback server recipe from the
audit notes or let CI carry it. All three suites also run inside
`R CMD check`.

## Why "Rjif"

`jif` is short for judgment-based if. The name follows the `RJDBC`/`RJSONIO`
tradition for R bindings to a service, and reads as code: *jif, else*.
