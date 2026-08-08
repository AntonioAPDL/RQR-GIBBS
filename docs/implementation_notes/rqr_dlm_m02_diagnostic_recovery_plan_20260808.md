# RQR-DLM M02 diagnostic recovery and maximum-run relaunch plan

## Decision

The failed diagnostic-aware maximum run must remain closed and immutable. Its
chains, partial summaries, and wave state are not inputs to a replacement
scientific run. The next maximum run may begin only from a fresh run root,
under a newly built isolated primary runtime, after the M02 diagnostic
retained-draw contract passes unit, package, reference, and production-path
validation.

This is the narrowest scientifically and operationally justified recovery.
The failure occurred after the M02 endpoint fits had completed, when training
and future diagnostic matrices with different row counts were combined. It
does not justify changing a Gibbs kernel, prior, data-generating mechanism,
seed, MCMC schedule, diagnostic threshold, or stopping rule.

## Authenticated failure

The run
`rqr_dlm_diagnostic_aware_maximum_20260807_ea8ea8d` was authorized at commit
`ea8ea8d17c6f7bb34b015472e4f60f62e547c942`. It stopped after the first of 110
canonical waves:

| quantity | observed |
|---|---:|
| terminal canonical waves | 1 / 110 |
| terminal DGP-replication tasks | 8 / 8,400 |
| completed method-replication results | 16 / 43,800 |
| completed M01 fits | 8 / 8 |
| failed M02 diagnostic constructions | 8 / 8 |
| constructed M01 diagnostics passing | 368 / 368 |
| retries or reseeds | 0 |

Every M02 failure has class
`mcmc_diagnostic_construction_failure` and message digest
`6495bf2b41dc1e26ee4112cd6e30200ca35aa20b34792f1e7f953b9a79210ef5`.
All workers stopped through the declared global-stop boundary, and no later
wave was authorized. The protected exdqlm and Q-DESN checkouts were not used
as mutable runtime sources.

## Root cause

M02 performs two endpoint-specific dynamic quantile fits. Its transition
schedule uses a multiplier of two. The diagnostic extractor correctly thinned
the training ordinate draws by that multiplier, but passed the unthinned
terminal state draws into the deterministic future-root projection. It then
attempted to append matrices with half as many training rows as future rows.
Base R rejected the combination because the row counts differed.

The failure is therefore a retained-draw identity violation in diagnostic
assembly. It is not evidence of a changed target, failed MCMC transition,
incorrect endpoint ordering, response-likelihood problem, or response
prediction problem.

## Correction contract

The M02 scalar extractor must establish exactly one retained-draw index and
apply it to every diagnostic derived from the endpoint fits:

1. lower and upper raw training ordinate matrices have identical dimensions;
2. the diagnostic thinning factor is a positive integer and divides the raw
   retained-draw count exactly;
3. training ordinates use the common retained index;
4. each terminal-state matrix has the same raw draw count as its training
   ordinate matrix;
5. terminal states use the same retained index before future propagation; and
6. the final training and future diagnostic matrices have the same retained
   draw count before column binding.

Each condition fails explicitly at the diagnostic boundary. No recycling,
implicit truncation, partial row matching, or silent endpoint-specific
thinning is allowed.

## Validation ladder

### Gate 1: source-level contract

- [x] Add one shared retained index to M02 diagnostic extraction.
- [x] Reject unequal lower/upper raw draw dimensions.
- [x] Reject non-dividing diagnostic thinning.
- [x] Reject terminal/training raw-draw disagreement.
- [x] Reject training/future retained-draw disagreement.
- [x] Add a deterministic test using the production diagnostic schema.

### Gate 2: repository validation

- [x] Parse all changed R source.
- [x] Pass the complete confirmatory contract test file with the package
  namespace loaded.
- [x] Pass native R/C++ tests and standalone contracts.
- [x] Pass package check without new errors, warnings, or notes attributable
  to the correction.
- [x] Pass manuscript and supplement builds and the smoke gate.

### Gate 3: exact runtime and non-MCMC evidence

- [ ] Commit the fail-closed implementation.
- [ ] Require a clean checkout at that complete commit.
- [ ] Build a fresh isolated `rqrgibbs` runtime from the exact Git archive.
- [ ] Reuse comparator runtimes only if their attestations and protected
  checkout guards still verify.
- [ ] Regenerate fresh confirmatory preflight and oracle-reference bundles.
- [ ] Verify recursive manifests, runtime digests, toolchain identity, task
  plan, wave plan, and maximum seed ledger.

### Gate 4: production-path M02 validation

- [ ] Run the canonical M02 correction-wave validator through the real exdqlm
  adapter and production scalar extractor.
- [ ] Require every endpoint fit to complete without retry or reseeding.
- [ ] Require exact target status, zero numerical repairs, finite ordered
  endpoints, and the complete time-local, terminal, and future-root schema.
- [ ] Require the corrected shared retained-draw count for all training and
  future functions.
- [ ] Preserve diagnostic pass/fail values as results; construction failures
  remain a hard stop.

### Gate 5: fresh maximum run

- [ ] Prepare a new diagnostic-aware authorization bound to the exact source,
  isolated runtime, preflight, reference bundle, comparator attestations,
  seed ledger, task plan, and wave plan.
- [ ] Use a new run ID, run root, and supervisor-log root.
- [ ] Verify fail-closed startup before detached execution.
- [ ] Launch only the complete maximum design; do not resume or copy the
  failed wave.
- [ ] Record frozen diagnostics without weakening thresholds. MCMC diagnostic
  failures are nonblocking metadata under the diagnostic-aware policy;
  construction, provenance, target, numerical, and resource failures remain
  hard stops.

## Reproducibility and scope

All heavy runtimes, fits, logs, and run outputs remain below ignored local
roots. Only compact plans, closeouts, validation summaries, and hashes may be
tracked. The generalized posterior remains a loss-based update; M02 future
root functions are deterministic state-derived diagnostics and do not define
posterior-predictive response draws.

The exdqlm reference checkout remains pinned at
`dffb71ee70b597d6a716ee74be1cbc99731cd453` and read-only. The Q-DESN article
repository is a style/reference source only. The separate oracle-tilt V4
workflow is not part of this recovery and must not run concurrently with the
maximum RQR-DLM launch.

## Promotion rule

Passing this recovery authorizes execution of a diagnostic-aware maximum run;
it does not by itself establish convergence, coverage calibration, forecasting
superiority, response-predictive validity, or manuscript-ready comparative
evidence. Scientific interpretation requires the completed maximum-design
analysis and its prespecified diagnostic stratification.
