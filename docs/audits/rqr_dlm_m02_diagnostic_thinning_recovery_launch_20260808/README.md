# RQR-DLM M02 diagnostic-thinning recovery and maximum-study launch

Date: 2026-08-08

## Decision

The deterministic M02 diagnostic-construction defect has been corrected and
validated at exact source commit
`031595bbbca5c59673faed10087faaf450c15a5a`.  A fresh diagnostic-aware maximum
study was launched from that commit after an isolated runtime build, a real
production-schedule M02 canary, and fresh preflight and reference gates.  The
study is running in the background.  This record establishes launch identity
and initial health; it is not a completion closeout or a scientific result.

## Root cause and correction

The failed 2026-08-07 run doubled the M02 endpoint chain from 4,000 to 8,000
retained states and thinned the training ordinate diagnostics back to 4,000.
The same indices were not applied to the terminal states used for conditional
future-root diagnostics.  Binding the 4,000-row training functions to the
8,000-row future functions therefore failed with

```text
number of rows of matrices must match (see arg 2)
```

The correction constructs one retained-index vector and applies it to both
root ordinate arrays and both terminal-state arrays.  It rejects unequal raw
endpoint counts, terminal/ordinate count mismatches, excessive thinning, and
unequal post-thinning training/future counts before constructing the diagnostic
matrix.  The M02 target, sampler, transition count, seeds, forecasts,
comparators, scientific metrics, and diagnostic thresholds are unchanged.

## Exact source and runtime

| Item | Value |
|---|---|
| Source branch | `codex/rqr-dlm-m02-thinning-recovery-20260808` |
| Exact launch source | `031595bbbca5c59673faed10087faaf450c15a5a` |
| Package | `rqrgibbs 0.1.0.9034` |
| Application Git tree | `ecf8c6c03f669b97f09aa10c345c083faf6e5c20` |
| Primary runtime tree digest | `89a650024735aac6e6536a502d500e6ff14ca2f3189547d222649d9d6e160ac6` |
| Primary runtime attestation SHA-256 | `94f315a6938075be784dae7ee5bec0a061f087807171beccf1fb9f6544e9bec9` |
| exdqlm runtime tree digest | `7b855c3daa7f615cc4c2c8e3b6212ac7c4fe12b80536517829a7ddece7e1d6d6` |
| exdqlm runtime attestation SHA-256 | `cb27bc019030ebab93ec9d89550ea548ef41c4804109ee2cd5d7f788425f4423` |
| Protected exdqlm checkout used | `FALSE` |

The primary package was built from an exact Git archive and installed beneath
the ignored runtime cache.  The isolated CRAN exdqlm runtime was reused through
its attestation.  Neither the exdqlm source repository nor the Q-DESN article
repository was modified, compiled, installed, or loaded.

## Validation evidence

The validation matrix is recorded in `validation_matrix.csv`.  In particular:

- the focused production-path contract file passed 342 expectations;
- the expanded native, confirmatory, and diagnostic-aware tests passed, with
  one previously documented experimental-recursion warning and one
  capability-dependent DESN skip unrelated to this correction;
- repository smoke, package check, main PDF, supplement PDF, and literature
  manifest targets passed; and
- generated PDF timestamps and manifests produced by local validation were
  restored to their committed bytes, leaving the source worktree clean.

## Production-schedule canary

The canary used the frozen M02 production transition multiplier and one
predeclared non-sentinel task (`S02`, replication 165, cell `C02M02`).  It is a
pipeline validation, not a simulation pilot and not manuscript evidence.

| Quantity | Result |
|---|---:|
| Raw lower-root draws | 8,000 |
| Raw upper-root draws | 8,000 |
| Frozen diagnostic thinning | 2 |
| Aligned diagnostic rows | 4,000 |
| Exact diagnostic columns | 45 |
| Hard construction/provenance checks | 9/9 passed |
| Frozen diagnostics | 45/45 passed |
| Diagnostic warnings | 0 |
| Elapsed time | 179.108 seconds |
| Sampled peak RSS | 946,732 KiB |

The compact five-file artifact manifest rehashed exactly.  The canary output
is local-only at
`application/runs`' external frozen-run root and remains outside Git.

## Fresh launch evidence

The replacement run regenerated every input under a new empty root.  No file,
fit, diagnostic, seed retry, or partial result from the failed run was reused.

| Evidence | Result |
|---|---|
| Preflight gates | 23/23 passed |
| Preflight artifact-manifest SHA-256 | `4b50e4eb62d03f76e633d99e79743507d5c7c7c325a408af733deabd8f6b40fc` |
| Reference gates | 15/15 passed |
| Reference artifact-manifest SHA-256 | `a8574537a1ef696ea717ee05e4e7637c272e234a99144befc81f55a58ecc978e` |
| Authorization SHA-256 | `f7e99786445de6c0febda04fd4688ca753dba91f20f84617cecbe3cdf488a4ce` |
| Bound launch-input SHA-256 | `0774c6261631afd63b3e6d6276a1a12c1f5a670c1374ca476044a5d85563b58b` |
| Maximum design | `TRUE` |
| Precision stopping | disabled |
| Frozen diagnostic thresholds changed | `FALSE` |
| Diagnostic thresholds block execution | `FALSE` |
| Hard provenance/numerical/resource/artifact stops | enabled |

## Background launch

| Item | Value at launch receipt |
|---|---|
| Run ID | `rqr_dlm_diagnostic_aware_maximum_20260808_031595b` |
| Coordinator PID | `2784661` |
| Worker count | 8 |
| Planned canonical waves | 110 |
| Planned DGP-replication tasks | 8,400 |
| Planned method-replication results | 43,800 |
| Active wave | `static_gaussian_T200__target0200__sentinel` |
| Coordinator running | `TRUE` |
| Coordinator stderr bytes | 0 |
| Terminal waves at receipt | 0/110 |
| Terminal tasks at receipt | 0/8,400 |
| Final audit present | `FALSE` |

All eight worker R processes and their resource monitors were live when this
receipt was prepared.  Zero terminal tasks at that time means that the first
eight fits were still computing; it is not evidence of a stall.  The launch is
detached and continues independently of this Codex session.

The health check is reproducible from the exact launch checkout with:

```bash
Rscript application/scripts/21_healthcheck_rqr_dlm_confirmatory_simulation.R \
  /data/muscat_data/jaguir26/.rqr_gibbs_frozen_runs/rqr_dlm_diagnostic_aware_maximum_20260808_031595b_r2/run \
  /data/muscat_data/jaguir26/.rqr_gibbs_frozen_runs/rqr_dlm_diagnostic_aware_maximum_20260808_031595b_r2/control
```

## Excluded setup attempts

Two pre-scientific setup attempts are retained only for transparent operational
provenance and are excluded from all evidence:

1. the first canary invocation hid `Rcpp` through an incomplete isolated
   library path and stopped before loading the package or running a fit; and
2. the first evidence invocation omitted the required seed-ledger environment
   binding and stopped before generating reference evidence.

Both replacements used new empty roots.  Neither setup failure affected a
scientific result, and neither root is eligible for reuse or promotion.

## Completion boundary

No comparative claim is authorized by this launch receipt.  A valid closeout
still requires all 110 waves to reach a terminal state, all hard gates and
recursive artifact hashes to pass, and the all-results aggregate to be
distinguished from the predeclared unflagged-diagnostics sensitivity
aggregate.  Diagnostic warnings will be retained and reported rather than
used to delete replications.  Interval-root summaries will not be described as
posterior-predictive response draws.
