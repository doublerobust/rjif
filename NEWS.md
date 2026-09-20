# Changelog

## 0.1.0 (2026-09-20)

Second external audit (Omen Codex, review of fc041ab) plus its follow-through.
Three changes alter documented behavior; everything else is bug-fixing inside
promises the 0.0.1 docs already made.

### Breaking

- `JEV_WINNER_TOL` tightened from 0.02 to 0: a choice answer whose selected
  option does not attain the maximum displayed probability is now rejected as
  self-contradictory (exact displayed ties still decide). The 0.02 margin
  rested on a "display rounding" rationale that cannot produce a displayed
  reversal; 40 live calls (12 tie-forced with duplicated criteria) showed
  zero reversals. Effect: answers the old validator forwarded as decisions
  now abstain.
- `jev_score_many(cache = ...)` fingerprint is now version 3 and includes an
  MD5 digest of the full state vector (contents AND order). Consequences:
  (a) a cache written by 0.0.1 no longer matches and is REFUSED; (b) a
  mismatched cache is no longer warn-and-overwrite: the call errors, makes
  zero requests, and leaves the prior file byte-intact, because silently
  re-billing a full run and destroying the only prior artifact was the
  finding's point. Pass a fresh path to start over.
- `DESCRIPTION` gains `openssl (>= 0.8)` in Imports. It is already an
  unconditional dependency of `httr`, so no new software is installed by this.

### Changed

- `selection_curve()` gains `policy = "positive" | "two_sided"` (default
  unchanged). With `two_sided` the curve reports the coverage that
  `jif()`'s two-sided noul floor actually delivers, computed by the same
  internal helper so the two can never drift again; it refuses non-noul
  batches. The result carries `attr(, "policy")`.
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
  longer replays stale judgments attached to the wrong records.

## 0.0.1 (2026-09-19)

First public release, audited through 14 rounds (TypeSafe Jev wrapper:
`jif()`/`j_ifelse()`/`jmatch()` control flow, `jev_eval()`,
`jev_score_many()`, calibration diagnostics `reliability_curve()`/`ece()`/
`selection_curve()` with the `accuracy` -> `observed_rate` rename, two-sided
noul `confidence_floor` window, response-contract validators, timeout and
retry policy, resumable batch cache).
