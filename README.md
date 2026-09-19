# Rjif — `if (jif(...))` for R

Calibrated judgment as a control-flow primitive, on top of
[TypeSafe's Jev](https://typesafe.ai) "System One" model.

The premise: a huge amount of programming is fuzzy branching — is this
customer angry, does this narrative describe a safety event, which team owns
this ticket. We hard-code heuristics for those, or we burn four seconds and
half a dollar asking a chat model and hope it answers in valid JSON. Jev is a
different instrument: you hand it a state plus a set of typed questions, and it
returns decisions with probabilities instead of prose — one parallel pass, no
generation, no parsing.

Rjif makes that usable in R as an `if` statement.

```r
library(Rjif)

test <- jif(narrative, "a serious adverse event is being reported",
            confidence_floor = 0.7)

action <- j_ifelse(test,
  yes     = "auto-triage to the safety queue",
  no      = "leave in the routine pile",
  unknown = "send to the human reviewer")   # the abstention lane
```

## The abstention lane is the whole point

`jif()` does not return `FALSE` when it could not decide. It returns `NA`, so a
bare `if (jif(...))` stops with an error instead of quietly routing
"uncertain" into the negative branch. That is a deliberate design choice: for
clinical, financial, or safety triage, an honest *no decision* is not the same
thing as a decision, and a package that conflates them is dangerous. Branch
with `j_ifelse()`, whose `unknown` arm is the escalation queue.

`jif_reason(test)` tells you why it abstained (below the floor, or the API sent
no value at all — missingness is not low confidence).

## Primitives

| Constructor | Question | Returns |
| --- | --- | --- |
| `jev_noul_q()` | "is this assertion true of the state?" | score in 0-1 |
| `jev_choice_q()` | "which of these described options?" | option + full distribution |
| `jev_score_q()` | "rate the state on this ordered rubric" | continuous level position (0..k-1, may land between levels) + per-level probabilities + legend |

All three can be mixed in one API call and are evaluated independently, so
adding questions does not degrade the others.

## Batch, then audit

`jev_score_many()` evaluates one judgment across a vector of states and returns
a data frame (`decision`, `option`, `p`, `confidence`, `abstained`, `error`)
with per-row error capture, so one unparsable narrative does not discard the
other 4,999.

Then the part that actually matters:

```r
reliability_curve(df)      # bin: mean probability vs observed accuracy
ece(df)                    # expected calibration error (estimates, never bounds:
                           # a 4.5-in-10 bin can understate a true error of 54-in-100;
                           # ece() warns when <30 rows survive the drop filters)
selection_curve(df)        # coverage vs event-rate (pos_rate) as you raise the floor
```

A probability you have not audited is a superstition. `reliability_curve()`
accounts for every dropped row in mutually-exclusive buckets that add up to
`nrow(df)`, and `selection_curve()` reports the positive-event rate among kept
rows (read it as prevalence/PPV, not decision accuracy).

## Honest limitations

- **Jev is a hosted API.** Nothing here runs offline against a real model; you
  need a `TYPESAFE_API_KEY`. No clinical or proprietary data should touch it
  before your organisation has cleared the vendor.
- **This package does not calibrate Jev.** It gives you the instruments to
  measure calibration on your own traffic and your own gold labels. Whether the
  vendor's probabilities are load-bearing on your data is exactly the open
  question.
- **`ece()` is the classic equal-width estimator** (Naeini et al. / Guo et al.),
  not the bias-corrected version of Nixon et al. 2022, and with fewer than
  ~100 scored rows it mostly measures bin noise.
- **The API takes one state per call**, so `jev_score_many()` is one HTTP
  request per row. `batch` bounds progress chunks; it does not amortise cost or
  latency.
- **Documentation is inline comments, not man pages** — acceptable for a
  zero-point-zero release.
- The bundled `rjif_mock_transport()` exists to exercise the plumbing with no
  key and no network. Its numbers are deterministic hashes, statistically
  meaningless, and it says so on the tin.

## Install

```r
remotes::install_github("doublerobust/rjif")
```

Imports `httr`, `jsonlite`, `stats`; no compiled code.

## Tests

`Rscript tests/smoke.R` — 184 offline assertions against the mock transport and
a set of scripted fake responses, covering the abstention lane, the API
response contract, malformed-usage handling, and the calibration analytics.

## Why "Rjif"

`jif` = judgment-based if. The name follows the `RJDBC`/`RJSONIO` tradition for
R bindings to a service, and reads as code: *jif, else*.
