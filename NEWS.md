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
  The `model` argument is now type-gated on every call (author self-audit
  after round 6e, extended by round 6f): it must be a plain-class, single
  non-NA, non-blank string. Before, `NULL` went to the wire as
  `{"model":null}` and `5` as `{"model":5}` -- requests that can never
  succeed -- and classed character scalars the old check waved through
  rendered as arrays: `I("m")` posts `{"model":["m"]}`, `matrix("m",1,1)`
  posts `{"model":[["m"]]}`, and an unknown S3 class cannot post at all.
  Bytes-marked and invalid-byte models are rejected before any cache
  lookup; harmless attributes (names, metadata) stay allowed.
- Cache fingerprint version 8L (old caches are refused as before; the
  version tag carries the reason for anyone inspecting the serialized
  cache, while the user-facing error stays the generic "stale cache"
  message). v8 (audit r6f) rebuilds the model and question identity
  entries as digests of the TRANSPORT'S OWN serialized bytes -- the same
  `jsonlite::toJSON` call and options the request body uses -- instead of
  hand-canonicalized R objects: the hand recipes lagged the serializer
  twice (missed S3 class dispatch and factor levels, so a never-sendable
  question or a wire-different model could resume a valid one's cached
  decision with zero calls). Identity now equals wire representation by
  construction. v7 canonicalized the model and question text for the
  fingerprint the same way the JSON transport does (r6e: a `model` string
  marked `Encoding == "bytes"` can never be serialized and is now rejected
  before any cache lookup or call, and question text that a locale change
  would reinterpret no longer resumes its old cache). v6 added the
  `model_id` whole-content digest that makes the resume identity
  independent of the display-scrubbed model string (r6c R6c-B1); v5 had
  the per-row provenance columns; caches from those eras predate the whole
  pre-release feature and are refused as stale.

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

### Fixed (external audits of the provenance feature, rounds 6 through 6e)

- Provenance strings (`model_requested`, `model_returned`, the batch
  `model` column) are scrubbed AND stripped of all attributes before
  attachment: an attributes-carrying `model` argument can no longer
  smuggle arbitrary strings into retained answer objects, serialized
  results, or saved caches (r6 R6-B1). The cache fingerprint's model
  entry follows the same retention rule but with a different mechanism
  (r6b R6b-B1 fixed the leak; r6c R6c-B1 fixed that fix -- see below):
  the readable entry is scrubbed and bare, while the resume IDENTITY is
  a whole-content MD5 digest of the model exactly as it goes on the
  wire. Using the scrubbed string itself as identity had merged two
  distinct aliases whenever display redaction rewrote both into one
  marker, letting a second alias silently resume the first alias's
  cached decisions with zero calls (an inherited v4-era retention path
  surfaced by the new tests, not introduced by this feature). The
  digest is taken after the same encoding conversion the transport
  performs (r6d R6d-B1: two strings sharing bytes but declaring
  different encodings -- "é" UTF-8 versus the same bytes flagged
  latin1 -- go on the wire as different JSON and must not share a cache
  identity).
- `probs_json` is written with 17 significant digits: jsonlite's
  `digits = NA` caps at 15 and silently lost bits for non-dyadic
  normalized probabilities (r6 R6-B2). Positive doubles re-parses
  bit-exactly; the only exception is negative zero's sign, which cannot
  carry decision meaning for a probability (r6b R6b-M1).
- A response that reports no usable model identifier yields `NA`
  provenance instead of silently substituting the requested alias, which
  had turned "the vendor did not say" into apparently-known provenance
  (r6 R6-B3). The answer itself is still accepted; the alias remains
  visible in `model_requested`.
- Redaction-merged Choice labels no longer produce two disagreeing
  representations of one distribution. The round-6b answer (rename both
  sides with `make.unique`) was itself defeated by an adversarial label
  (r6c R6c-B2: a label already ending in the redaction marker + ".1"
  made `make.unique` output a duplicate, and the JSON column was
  renamed once more on the way out -- names diverged between the answer
  object and the cache column). The column is now a JSON array of
  [name, value] pairs under a fixed key, names stored byte-exactly,
  duplicates and all, and `jprobs()` decodes the same pairs: identical
  by construction for any labels, fresh and after resume. Merged names
  remain ambiguous by nature, so selected probabilities stay looked up
  from the pre-redaction binding (`p` column), never by a merged name.
- The pair encoding now distinguishes a distribution with NO names
  attribute from one whose names are all NA: only the former carries
  `"named": false`, and the decoder restores NULL versus explicit NA
  names so `jprobs()` agrees with the retained answer on both raw shapes
  (r6d R6d-m1; the validator always labels positional distributions, so
  this edge was reachable only through raw retained answers, not through
  an ordinary vendor Choice response). R6e R6e-m1 caught that the r6d
  writer emitted an R NULL name as `{}` (jsonlite's default null policy,
  which `na = "null"` does not cover) while the decoder accepted only
  JSON null, so the real bytes fell through into the legacy object path;
  the writer now pins `null = "null"` so every missing name serializes
  one way, and the regression test exercises the writer function
  directly rather than a hand-written column.
- A `model` string marked `Encoding == "bytes"` -- which the JSON
  transport can never serialize -- is now rejected up front, before any
  cache lookup or transport call, leaving the cache file byte-intact
  (r6e R6e-B1; the state path already refused the same shape).
  enc2utf8() preserves the `bytes` flag without converting it, so the
  r6d model digest could not tell a bytes-marked model apart from the
  UTF-8-marked copy of its bytes, and a redaction-collapsed resume handed
  the impossible model a cached success.
- Question text stored in the cache fingerprint is now canonicalized with
  enc2utf8() like the model and the states (r6e, an inherited v4 path
  reproduced on the originally-approved commit as well). A question whose
  bytes R treats as locale-dependent no longer resumes its own cache
  across an LC_CTYPE change that reinterprets it into different words;
  same-text resume still costs zero calls.

### Fixed (external audit rounds 6f through 6i, identity rebuilt on the wire)

- Round 6f (blockers R6f-B1/B2, minor R6f-n1): the cache identity is now
  the MD5 of the request envelope as `jsonlite::toJSON` renders it with
  the transport's own options (`JSON_OPTS`, single-sourced), not a
  hand-canonicalized R object. Three hand recipes in a row (6c/6d/6e)
  lagged the serializer one dispatch axis at a time -- S3 class effects
  (`I()` suppresses `auto_unbox`; unknown classes fail outright) and
  factor levels (the wire carries the LABEL text) all let a question or
  model that the real API could never answer resume a valid row's cached
  decision with zero calls. The `model` gate additionally rejects
  non-plain classes (`I("m")`, `matrix("m",1,1)`, classed scalars) up
  front, and the digest now tracks what the serializer emits so a future
  gate loosening cannot re-open the aliasing.
- Round 6g R6g-B1 (blocker, new in the 6f gate): the class-name gate
  interpolated `class(model)` -- caller text -- straight into its `stop()`
  before any scrubbing ran, so naming a class after the live API key made
  the key appear byte-for-byte in the error on all three entry modes.
  Gate messages now pass through `.clean_error_text`, which redacts the
  actual key wherever it appears.
- Round 6g R6g-B2 (major, inherited): the pre-transport serialization
  property held only against the DEFAULT transport -- with a custom
  `Rjif.transport` callback, `jev_eval()` handed the callback a
  never-serializable body and returned its answer as a success. The body
  is now slot-scanned and rendered with `JSON_OPTS` inside `jev_eval()`
  itself, before either transport branch, so a request that could never
  exist on the wire can never produce an answer through any transport.
  The refusal names the offending slot (`Rjif: question$instructions`)
  exactly like the batch path.
- Round 6g R6g-m1 (minor): the factor-level check validated every level,
  but the serializer emits only the label the value USES; an unused
  bytes-marked level over-refused a question whose wire bytes are
  identical to a clean one. The check now looks at the used level only
  (a used bad level still refuses, with the slot named).
- Round 6g R6g-m2 (minor): a `POSIXlt` value inside a question sent the
  identity walk into unbounded recursion (`[[.POSIXlt` re-yields the
  object), raising an UNcatchable C stack overflow. The walk descends
  plain lists only; a classed object's wire form is whatever its own
  `asJSON` renders, which the digest sees and the serializer wrap
  refuses if rendering fails.
- Round 6g R6g-n1 (note): criterion names become slot paths in scan
  errors; the slot text is scrubbed through `.clean_error_text` before
  interpolation, so a criterion named after the API key surfaces as
  `[REDACTED-API-KEY]` (the key already travels inside the request -- the
  invariant is that no ERROR TEXT can carry it out unredacted).
- Round 6h R6h-B1 (blocker, inherited): the three cache-path diagnostics
  (unreadable file, fingerprint mismatch, failed write) interpolated the
  caller's `cache` path into the message before any redaction ran, so a
  key-named file or directory leaked the live API key into the error text
  -- the same caller-text invariant R6g-B1 established for class names,
  at a site round 6g had not looked at. All three now pass the COMPLETED
  message through exact-key redaction; the write path additionally
  muffles `saveRDS`'s own low-level warning, which escaped the previous
  wrap entirely. Redaction for these messages never truncates: the
  contract phrases ("Nothing was written", "NOT overwritten") are tested
  intact.
- Round 6h R6h-B2 (blocker, new in the 6g gate): the pre-transport render
  proved only that `toJSON` did not THROW. Invalid UTF-8 under a classed
  wrapper (`I()`, a `data.frame`) renders without an error but emits
  malformed JSON that jsonlite's own validator rejects, so a callback
  could answer -- and a cached batch could describe -- a request the real
  transport could never have POSTed. The fix is structural: ONE helper
  (`.wire_render`) renders with `JSON_OPTS` and validates
  (`validUTF8` and `jsonlite::validate`), and the identity digests, the
  `jev_eval` preflight, and the real transport all go through it -- every
  consumer gets the same validated bytes or the same refusal.
  `tests/r16-wire-capture.R` closes the matching evidence gap from the
  same round: it captures the POST body a loopback server ACTUALLY
  receives, byte-compares it to the preflight render, and recomputes the
  cached question/model digests from the server-side JSON parse.
- Round 6h R6h-n1 (note): redaction of cache-path messages is exact-key
  plus pattern-based; credential FRAGMENTS are not detected. Documented
  in `?jev_score_many` rather than defended by a heuristic that would
  mangle honest paths.
- Round 6i R6i-B1 (blocker, finishing the 6h repair): round 6h muffled
  the WRITE side (`saveRDS`) but not the READ side -- `readRDS`'s own
  low-level warnings ("cannot open file '<path>': it is a directory",
  "probable reason 'Permission denied'") carry the raw path and fire
  BEFORE the error handler, so a key-named directory or an unreadable
  key-named file still echoed the live key. The read is now wrapped in
  `suppressWarnings`; the caller's report remains the scrubbed,
  untruncated package error. Committed tests pin the directory and
  permission-denied cases (the 6h block had only pinned bad-bytes, which
  never warns), plus a pattern-free honest path that must echo verbatim
  -- the negative control that keeps redaction from mangling normal
  paths.
- Round 6i R6i-m1 (medium, test oracle): `r16-wire-capture.R` proved
  the wire bytes but its identity checks recomputed the expected digest
  with the SAME helper under test -- a contamination of
  `.question_identity`/`.model_identity` cancelled on both sides and all
  ten checks passed (the auditor's `mut-identity` mutation demonstrated
  this). W3/W4/W7/W8 now build the expected digest INDEPENDENTLY --
  md5 over a plain `jsonlite::toJSON` render of the server-parsed wire
  object, no package helper involved. Verified against all four mutation
  libraries from round 6i: identity (4 fails), transport (1), model-wire
  (5), selective (2) -- every deliberate drift is now detected, and the
  pristine suite passes 10/10 in both modes.
- Round 6i R6i-n1 (low, documentation): the 6h help text overclaimed
  ("perform no other pattern scrubbing", "echoes it verbatim") where
  `.redact_only` does run recognized secret-pattern replacement and
  control-character collapsing. Reworded to describe exactly what the
  code does: exact configured-key redaction anywhere in the message,
  recognized bearer/sk- pattern replacement, newlines/controls to
  spaces, arbitrary fragments NOT detected, message-only (never the
  path).

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
