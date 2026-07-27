# Third RQR-DLM confirmatory launch: wave-2 closeout

Date: 2026-07-27 UTC

## Scope and decision

The authorization-bound run
`application/runs/rqr_dlm_main_20260727_ce02915` stopped after its second
canonical wave.  The append-only state ledger records one passed wave and one
failed wave.  The coordinator is no longer running, no active start remains,
and the remaining 108 waves are blocked.  This is a valid fail-closed stop, not
an interrupted or resumable execution.

No comparative simulation estimate, coverage summary, width summary, loss
contrast, or manuscript claim from this run is used.  The failed run is used
only to diagnose implementation mechanics, fixed-schedule mixing, resource
use, and failure-publication behavior.

## Exact source and runtime boundary

- reviewed implementation:
  `8e1daf9ea7c2884b47303cf627c64db20e5909a3`;
- flag-only authorization:
  `ce02915f8e6270fb21c4cce1bdc231beeda12292`;
- run ID: `rqr_dlm_main_20260727_ce02915`;
- exact preflight gates: 22/22;
- exact oracle/reference gates: 15/15;
- exdqlm comparator: isolated CRAN 1.1.0 runtime;
- quantreg comparator: isolated CRAN 6.1 runtime;
- protected exdqlm and Q-DESN checkouts used at runtime: no.

## Terminal wave ledger

| Wave | Canonical ID | Tasks | Decision | Artifact-manifest SHA-256 |
|---|---|---:|---|---|
| 1 | `static_gaussian_T200__target0200__sentinel` | 20 | passed | `812f1d9dbd8bd5c38de1061691f5cf9f75f1770511b7c074bb34c86231f96ed2` |
| 2 | `local_level_gaussian_T200__target0200__sentinel` | 25 | failed | `418b6facad514e09dc7fe3650c8c172c88fa82cb84054b11506bc885284a039c` |

Wave 1 produced 2,312/2,312 passing MCMC diagnostic rows:
920/920 for M01, 900/900 for M02, and 492/492 for M03.  These are
computational observations only.

## Wave-2 failure diagnosis

### Singleton exdqlm state projection

The CRAN exdqlm 1.1.0 retained-state contract for a local-level model is a
`1 x T x draws` array.  The runner selected a draw with default R dimension
dropping and then called `as.matrix()`.  The valid `1 x T` state therefore
became `T x 1`, while the observation design remained `1 x T`.  The public
projection validator rejected the mismatch.

An independent isolated-runtime reproducer gave:

| Object | Dimension |
|---|---:|
| retained state array | `1 x 20 x 3` |
| observation design | `1 x 20` |
| old extracted matrix | `20 x 1` |
| dimension-preserving matrix | `1 x 20` |

This is an extraction defect in RQR-GIBBS.  It is not an exdqlm defect and
does not require changing the protected package or its CRAN 1.1.0 source.

### M01 component-scale mixing

Three recorded local-level sentinel replications failed frozen diagnostics.
Two failures were confined to bulk ESS for `log(q_1)`:

| Scenario | Replication | R-hat | Bulk ESS | Tail ESS |
|---|---:|---:|---:|---:|
| S04 | 16 | 1.007969 | 223.2070 | 476.7745 |
| S03 | 20 | 1.003457 | 209.1886 | 535.4200 |

S03 replication 28 showed materially poorer movement.  Its `log(q_1)` R-hat
was 1.173091 with bulk ESS 4.7467; mean-width R-hat was 1.078458 with bulk ESS
9.6037.  A fixed schedule increase must therefore be validated and must not
be assumed sufficient.

The correction uses the already declared 6,000-retained-draw component-scale
standard schedule as the fixed sentinel schedule.  Seeds, priors, diagnostic
thresholds, targets, and no-retry rules remain unchanged.

### Failure publication

Seven workers preserved the singleton projection exception in monitored
stderr and in the wave artifact manifest, but the exception arose while
constructing diagnostic draws after fitting.  That block was outside the
replication-level structured failure boundary, so most workers did not publish
complete compact failure ledgers.  Diagnostic construction is moved inside an
atomic, systemic-stop failure boundary.

## Resource audit

The maximum sampled worker RSS was 1,496,068 KiB against a 1,572,864 KiB
ceiling, or 95.1 percent.  No process, thread, wall-time, or RSS ceiling was
crossed.  The peak occurred while serializing a large ignored sentinel-chain
sidecar and immediately deserializing a second in-process copy.  The sidecar
is not a promotion artifact.  The correction extracts each chain's endpoint
vectors and diagnostic scalars immediately, releases the full fit before the
next chain, and atomically publishes only a compact local diagnostic sidecar.
Worst-case retained-state allocations still pass through a separate,
conservative resource gate.

## Non-reuse and next boundary

The 20 passed tasks and 25 failed-wave tasks are not carried into another
confirmatory run.  A future run must use:

1. a fail-closed reviewed implementation commit;
2. an isolated runtime built from that exact commit;
3. passing singleton/multistate comparator gates;
4. passing exact local-level M01 correction streams;
5. passing worst-case memory gates;
6. a separate flag-only authorization commit; and
7. a fresh run ID and fresh append-only ledger.
