# Changelog

## 0.1.0 (2026-09-20)

Second external audit (Omen Codex, review of fc041ab) plus its follow-through.
Three changes alter documented behavior; everything else is bug-fixing inside
promises the 0.0.1 docs already made.

### Added

- Per-row provenance (round 3 carryover 1). `jev_score_many()` results gain
  columns `model` (the resolved model that produced that row's answer;
  `NA` when the API did not report one -- the requested alias is never
  substituted), `evaluated_at` (UTC, persisted across resumes), `row_source`
  ("api"/"cache"/"none"), `score_value` (the exact continuous score;
  `option` remains a 3-decimal display), and `probs_json` (the validator's
  normalized distribution, stored as 17-significant-digit JSON that
  re-parses bit-exactly). `jprobs()` parses a `probs_json`
  string or a `jev_answer` back to a named numeric vector. `jif()`'s
  `answer` attribute now carries `model_requested`/`model_returned`, which
  the `$q` extraction previously dropped with the envelope. Motivation: a
  cached run interrupted across a model-alias change can now distinguish
  day-one rows from day-two rows; before, nothing in the output revealed it.
- Cache fingerprint version 5L (old v4 caches are refused as before; the
  version tag carries the reason for anyone inspecting the serialized
  cache, while the user-facing error stays the generic "stale cache"
  message).

### Breaking

- `JEV_WINNER_TOL` tightened from 0.02 to 0: a choice answer whose selected
  option does not attain the maximum displayed probability is now rejected as
  self-contradictory (exact displayed ties still decide, with a 1e-9
  floating-point guard on the displayed values before normalization).
  The 0.02 margin
  rested on a "display rounding" rationale that cannot produce a displayed
  reversal; 40 live calls (12 tie-forced with duplicated criteria) showed
  zero reversals. Effect: answers the old validator forwarded as decisions
  now abstain.
- `jev_score_many(cache = ...)` fingerprint is now version 4: an MD5 digest
  of the full state vector (contents AND order), computed after normalising
  every state to UTF-8 (round 3 finding R3-1: with the v3 digest, bytes that
  meant different characters under different declared encodings shared one
  cache identity while the JSON payload differed, so a latin1-marked import
  silently resumed the UTF-8-marked run's decisions with zero new calls).
  Same text under different declared encodings now resumes as one identity;
  different text can no longer collide through identical raw bytes; elements
  explicitly marked "bytes" (validEnc() is TRUE for them but the JSON
  transport cannot serialise them, so a cache hit must not fabricate a
  decision) and byte sequences invalid in any encoding are rejected before
  any digest, cache lookup, or API call.
  Consequences:
  (a) a cache written by 0.0.1 no longer matches and is REFUSED; (b) a
  mismatched cache is no longer warn-and-overwrite: the call errors, makes
  zero requests, and leaves the prior file byte-intact, because silently
  re-billing a full run and destroying the only prior artifact was the
  finding's point. Pass a fresh path to start over.
- `DESCRIPTION` gains `openssl (>= 0.8)` in Imports. It is already an
  unconditional dependency of `httr`, so no new software is installed by this.

### Changed

- `selection_curve()` gains `policy = "positive" | "two_sided"` (default
  unchanged). With `two_sided` it uses the evaluator's shared window
  predicate when `threshold < floor <= 1`, and the single-sided fallback
  otherwise. Pass the evaluation threshold explicitly when it differs from
  0.5. Non-noul batches are refused. The result carries `attr(, "policy")`
  and, for `two_sided`, `attr(, "threshold")`.
- Round 15 corrected the initial implementation: the window helper was not
  actually shared, and low floors incorrectly selected both tails. The
  evaluator and curve now call the same helper, with matching policy switches.
- A corrupt or unreadable `cache` file now errors instead of warning and
  overwriting.

### Fixed

- `j_ifelse()` mixed literal/prebuilt branch maps routed to `unknown` even
  when a matching arm existed, and a literal `no` map could beat a matching
  prebuilt `yes` map (regression introduced by the 0.0.1 laziness fix). Arms
  are now probed yes-before-no in whichever representation each supports;
  the laziness guarantee is preserved and re-tested, including Codex's three
  verbatim fixtures.
- Two-sided noul negative cutoffs are now inclusive: `p = 0.2` with floor
  `0.8` decides FALSE instead of abstaining (`1 - 0.8` is
  `0.19999999999999996` in binary floating point; the `.7/.3` pair only
  worked because `1 - 0.7` lands above `0.3`).
- Cache content identity: reordering or correcting the state vector no
  longer replays stale judgments attached to the wrong records. Round 15
  also closed a refusal bypass for RDS files containing NULL and restored
  empty state vectors, which the new digest initially rejected.

## 0.0.1 (2026-09-19)

First public release, audited through 14 rounds (TypeSafe Jev wrapper:
`jif()`/`j_ifelse()`/`jmatch()` control flow, `jev_eval()`,
`jev_score_many()`, calibration diagnostics `reliability_curve()`/`ece()`/
`selection_curve()` with the `accuracy` -> `observed_rate` rename, two-sided
noul `confidence_floor` window, response-contract validators, timeout and
retry policy, resumable batch cache).
