# Oracle-tilt V4 preauthorization evidence reconciliation

## Decision requested

Determine whether the exact source at
`4b38e6a556e6848c7ed6e077dad0d22a00367382` may receive a later commit whose
only scientific-configuration change is
`execution_authorized: false -> true`.

This reconciliation does not itself authorize execution.

## Frozen design

The campaign is a prospective three-candidate screen for two single-data
illustrations: fixed-design ridge RQR and fixed-evolution RQR-DLM. Each family
uses RQR, equal-tailed, and shortest-interval population-oracle fixed tilts.
Targets within a candidate and family share the same generated response and
design. Candidate ranking is family-level and shared across all three targets.
Realized content and aesthetic judgment are excluded from ranking.

The grid contains 18 cells and 81 MCMC chains: 36 fixed-design chains and 45
DLM chains. DLM/SH retains 12,000 draws per chain; the remaining chains retain
6,000. The exercise is not a response-likelihood analysis, response-predictive
analysis, repeated-sample simulation study, or typical-performance claim.

## Exact identities

All four preauthorization bundles record:

```text
source commit:
4b38e6a556e6848c7ed6e077dad0d22a00367382

configuration SHA-256:
3fbbf25d1ad3c588bf13ce4268401601f6f567ecbdeab636d081a2e409542794

runtime-tree digest:
2dd6ec13e61e1218558cf037bd55be1338bb43586978baabd7c1d5cab02218b6
```

The source worktree and primary repository were clean during reconciliation.
The isolated-runtime attestation identifies the same source commit and a
verified archive/source-package/runtime lineage.

## Completed stages

### Preflight

All 51 candidate-level and 11 cross-candidate gates pass. The plan contains 18
cells, 81 unique chain seeds, nine candidate-specific named data streams,
target-shared data within each family/candidate, and distinct candidate data.

### Reference-only

All 29 conditional, numerical, plan, and stream gates pass.

### Hard-cell benchmark

Both production-schedule benchmark chains pass with zero numerical repairs.

| Family | Target | Elapsed | Endpoint RMSE/oracle width | Width ratio |
|---|---|---:|---:|---:|
| Fixed design | SH | 110.8 s | 0.06655 | 0.98854 |
| DLM | SH | 2993.1 s | 0.10380 | 1.06126 |

The DLM benchmark has width-contrast relative error 0.14437, seasonal-width
amplitude ratio 0.87743, and phase error 0.15166. All declared gross-recovery,
pathology, and heterogeneity checks pass.

### Full resource rehearsal

All 18 production-shaped cells ran concurrently in 18 distinct processes at
scale one. The rehearsal materialized 576,000 endpoint draws using 14.16 GiB
of worker storage. Every cell used endpoint-only storage and exactly preserved
the midpoint and width identities.

The wrapper completed in 530 seconds, reached 19 processes, 18 R processes, 19
threads, and 51.02 GiB sampled RSS, and ended with an empty process group. It
did not time out or trigger a sampled resource limit. All worker stdout and
stderr logs are empty.

The resource rehearsal is synthetic storage/concurrency evidence, not MCMC or
statistical recovery evidence.

## Integrity reconciliation

The runner and complete wrapper manifests for all four bundles were verified
against their local closed directories. The 15-GiB resource bundle was read
again independently: runner-manifest verification took 158.769 seconds and
wrapper-manifest verification took 158.403 seconds. Both passed.

The review packet omits the synthetic arrays. Its resource wrapper manifest
still records the complete closed directory, including every omitted worker
file's path, byte count, and SHA-256. The reviewer must state that the raw
15-GiB bytes were not independently available if reviewing from this packet
alone.

## Authorization and postauthorization consequences

Production currently remains fail-closed because the tracked configuration is
false and execution also requires the exact confirmation environment variable.
The runner binds every prerequisite to the current source commit,
configuration digest, and runtime-tree digest.

Changing the authorization flag changes both source commit and configuration
digest. Therefore the existing four bundles cannot authorize production at the
new commit. After a favorable review and flag-only commit, the workflow must:

1. build and attest a new isolated runtime;
2. regenerate exact preflight and reference evidence;
3. rerun the two hard benchmark chains;
4. rerun the 18-process full-scale resource rehearsal; and
5. launch production only if every regenerated bundle passes exact binding.

No manuscript figure is promoted automatically.

## Reconciliation verdict

All locally verifiable preauthorization gates pass. The remaining prerequisite
is the independent review required by the frozen protocol. This document makes
no independent-review or authorization claim.

