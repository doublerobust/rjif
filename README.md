# Rjif — model judgments in R's conditional lanes

Rjif wraps [TypeSafe's Jev](https://typesafe.ai), a "System One" decision
model: you send it a *state* (free text, a record, an application state) and
typed *questions*, and it returns probabilities instead of prose. The
statistical object being introduced into your code is a **predicted
probability of a semantic judgment** — the same kind of quantity a prediction
model produces, so it deserves the same discipline: an explicit uncertainty
lane, and a calibration audit against gold labels before any threshold is
load-bearing.

```r
library(Rjif)

# Is this narrative a serious adverse event report?
test <- jif(narrative, "a serious adverse event is being reported",
            confidence_floor = 0.7)

action <- j_ifelse(test,
  yes     = "auto-triage to the safety queue",
  no      = "leave in the routine pile",
  unknown = "send to the human reviewer")   # the abstention lane
```

## The three question types

| Constructor | Judgment | Answer |
| --- | --- | --- |
| `jev_noul_q()` | P(yes) for one assertion | probability in [0, 1]; the API sends **no** confidence for these |
| `jev_choice_q()` | one option from a defined set | chosen option, the full probability vector over options, confidence |
| `jev_score_q()` | position on an ordered rubric | **continuous** probability-weighted level position in 0..k-1 (it can land between levels), the per-level distribution, confidence |

Mix them freely in one call; questions are evaluated independently, so adding
one does not degrade the others.

## Uncertainty: probability, confidence, and NA

Three quantities, not interchangeable:

- `jvalue()` — the answer itself: a probability (noul), an option name
  (choice), or a level position (score). For a score, `threshold` in `jif()`
  is a *level* ("the weighted position clears moderate"), not a probability.
- `jprob()` — the probability backing the decision: the noul P(yes), P(chosen
  option), or (score) the API confidence.
- `jconf()` — the vendor's **confidence**, which the API docs define as a
  summary of distribution concentration, not an error probability. Use it to
  gate escalation; do not read it as P(answer correct). Noul answers carry no
  confidence at all, so `jconf()` is honestly `NA_real_` there.

`jif()` never resolves "uncertain" into `FALSE`. When the backing probability
is missing or below `confidence_floor`, it returns `NA` (a third state), and a
bare `if (jif(...))` stops with an error rather than silently routing an
undecided row into the negative branch. Branch with `j_ifelse()`, whose
`unknown` arm is the escalation queue. `jif_reason()` reports which mechanism
fired: `confidence unavailable with a floor set` (Jev returned nothing — a
missing datum, which is *not* low confidence) or `probability 0.612 <
confidence_floor 0.700` (low backing probability). Different missingness
mechanisms; treat them differently.

## Batch scoring

`jev_score_many()` applies one judgment over a vector of states (one call per
row; the API takes a single state) and returns a data frame — `decision`,
`option`, `p`, `confidence`, `abstained`, `error` — with per-row error
capture, so one unparsable narrative does not discard the other 4,999. For a
score question, `option` is the continuous position as text (e.g. `"1.050"`).

## Audit the probabilities before you trust a threshold

```r
df <- jev_score_many(narratives, sae_question, confidence_floor = 0.7)
df$truth <- gold_sae_labels            # from adjudication, not from Jev

reliability_curve(df)                  # per-bin mean p vs observed event rate
ece(df)                                # expected calibration error
selection_curve(df)                    # coverage vs event rate as you raise the floor
```

What these do, precisely:

- **`ece()` is a point estimate of expected calibration error** using the
  classic equal-width binning (Naeini et al.; Guo et al. 2017, ICML). It is
  *not* an upper bound — a population can sit above its ECE — and it is not
  the bias-corrected estimator of Nixon et al. 2022. Below ~30 usable rows it
  warns; below ~100 it mostly measures bin noise. `attr(rc, "n_used")`
  belongs next to every number you quote.
- **Row accounting is closed**: `reliability_curve()` assigns every dropped
  row to exactly one mutually-exclusive bucket and they sum to `nrow(df)`.
  Missing `p`, missing truth, and out-of-range truth are separate mechanisms;
  they are not silently pooled.
- **`selection_curve()` is a coverage/PPV-style diagnostic**: `pos_rate` is
  the event prevalence among kept rows as the floor rises, not decision
  accuracy. (Accuracy would additionally need true negatives.)
- Reliability is only meaningful against **gold** truth. Score Jev against
  another model's labels and you have measured agreement, not calibration.

## Honest limitations

- **Hosted API.** Nothing here runs offline against a real model; you need a
  `TYPESAFE_API_KEY`. No clinical or proprietary data should touch it before
  your organisation has cleared the vendor.
- **This package does not calibrate Jev.** It gives you the instruments to
  measure calibration on your own traffic and your own gold labels. Whether
  the vendor's probabilities are load-bearing on your data is exactly the open
  question this package refuses to answer for you.
- **No inferential machinery.** The curves are descriptive: no standard
  errors, no confidence bands, no test of miscalibration. A 10-bin reliability
  curve with 50 rows is a sketch, not evidence.
- **Cost/latency scale linearly with rows** (`jev_score_many()` is one HTTP
  request per state). The `batch` argument only bounds progress chunks.
- **The bundled `rjif_mock_transport()`** exercises the plumbing with no key
  and no network. Its numbers come from a deterministic hash: statistically
  meaningless by construction, and labeled so in the source.
- Documentation is inline comments, not man pages (accepted for 0.0.1).

## Install

```r
remotes::install_github("doublerobust/rjif")
```

Imports `httr`, `jsonlite`, `stats`; no compiled code.

## Tests

`Rscript tests/smoke.R` — 184 offline assertions against the mock transport
and scripted fake responses, covering the abstention lane, the API response
contract (including a live-verified continuous-score round trip),
malformed-usage handling, credential-retention boundaries, and the
calibration analytics.

## Why "Rjif"

`jif` = judgment-based if. The name follows the `RJDBC`/`RJSONIO` tradition
for R bindings to a service, and reads as code: *jif, else*.
