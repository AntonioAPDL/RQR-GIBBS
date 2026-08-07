# RQR-DLM diagnostic-aware maximum-run launch receipt

Date: 2026-08-07

## Decision and interpretation

The user explicitly authorized completion of the full frozen maximum design
without requiring every MCMC diagnostic threshold to pass.  This run therefore
uses the separately declared `diagnostic-aware-completion` policy.  It does not
weaken, suppress, or relabel the frozen R-hat, ESS, tail-ESS, or MCSE checks.
Threshold violations are retained as diagnostic warnings and the complete
finite results remain available for sensitivity analysis.

The following conditions remain hard failures: source or runtime provenance
mismatch, seed or task-plan mismatch, nonexact target status, numerical repair,
fit or DGP infrastructure failure, nonfinite or unordered interval endpoints,
diagnostic-construction failure, resource breach, and artifact-integrity
failure.  No retries, reseeding, selective extensions, or seed-based result
selection are authorized.

This is a maximum-plan scientific execution, but it is not a
convergence-validated promotion run.  Final reporting must distinguish the
all-result summaries from the sensitivity summaries that exclude flagged
method--replication rows.

## Frozen source and runtime

| Item | Frozen value |
|---|---|
| Source branch used to develop and document the lane | `codex/rqr-dlm-m11-multicomponent-recovery-20260806` |
| Exact source and authorization commit | `01059c674b455ecc697fd32652a207e34e3ec3dc` |
| Package version | `rqrgibbs 0.1.0.9033` |
| Launch checkout | `/data/muscat_data/jaguir26/.rqr_gibbs_launch_checkouts/rqr_dlm_diagnostic_aware_01059c6` |
| Launch-checkout branch | local `main` |
| Primary runtime tree digest | `bd0901deae71e80d4298b481e0efea26fe09317ee2d7991d1d69f8a7a866e613` |
| Runtime-attestation SHA-256 | `4a226cb5a609de63473b5573fc8faa77393bf5c3d18685fe8b7776d6f1501485` |
| Execution-policy SHA-256 | `efde1fe678cedb6326e64d2cdcdfa6db7e2a17f5831dd5fd75b922daafb291bd` |

The launch checkout was clean at the exact commit.  The primary package was
constructed from `HEAD:application` through the isolated Git-archive build and
attestation workflow.  The pinned exdqlm and quantreg comparator runtimes were
loaded from their existing isolated CRAN attestations.  No exdqlm or Q-DESN
source checkout was compiled, installed, loaded, or modified.

## Launch-bound validation

The exact launch source and runtime passed:

- `R CMD check --no-manual --no-build-vignettes`: `Status: OK`;
- maximum-plan preflight: 23 of 23 gates;
- oracle and comparator reference stage: 15 of 15 gates;
- primary source/runtime binding;
- protected-checkout exclusion;
- complete seed-ledger and canonical task/wave-plan validation; and
- process-group resource monitoring for both reference stages.

| Artifact | SHA-256 |
|---|---|
| Preflight recursive manifest | `34e2cf69e1e097d9a2dca5f47b1705046ccabb55fc4ab7d7752ffab63dd2a857` |
| Reference recursive manifest | `abd47761c0a176664f493fbd00ca8d6fc9d4df275dbd3df9e36ae89755014d8c` |
| Diagnostic-aware authorization | `d647c6718d89f61755f3fb8d65f026dfa90262cfda25f7e95df84e2e59f772de` |
| Launch-input bundle | `442abbda0bacb630634119964d4821c9cd516764e39d0c95a76c5553388f4cc2` |
| Canonical wave plan | `c45ece172d89a366fede5264a757c45161a3d9578a892ef1f8dfb920cbc51838` |

## Complete maximum design

| Quantity | Count |
|---|---:|
| DGP replication tasks | 8,400 |
| Execution waves | 110 |
| Included method--scenario cells | 89 |
| Method interval evaluations | 43,800 |
| Logical endpoint or model fits | 49,200 |
| Standard MCMC chain executions | 38,400 |
| Preselected sentinel chains | 2,538 |
| Total MCMC chain executions | 40,938 |

Precision stopping is disabled for this lane: every authorized batch proceeds
to the frozen maximum replication count unless a hard failure stops the run.

## Background execution

| Item | Value |
|---|---|
| Run ID | `rqr_dlm_diagnostic_aware_maximum_20260807_01059c6` |
| Run root | `/data/muscat_data/jaguir26/.rqr_gibbs_frozen_runs/rqr_dlm_diagnostic_aware_maximum_20260807_01059c6/run` |
| Control root | `/data/muscat_data/jaguir26/.rqr_gibbs_frozen_runs/rqr_dlm_diagnostic_aware_maximum_20260807_01059c6/control` |
| Coordinator PID at launch | `2752671` |
| Host | `jerez.be.ucsc.edu` |
| Coordinator start | `2026-08-07 20:20:12 UTC` |
| Available disk immediately before launch | 369 GiB |

The detached coordinator entered canonical wave 1,
`static_gaussian_T200__target0200__sentinel`, and started all eight authorized
sentinel workers.  At this initial health check, the coordinator and workers
were live, worker stderr logs were empty, and no resource failure had been
recorded.  The result count was still zero because the first sentinel fits had
not yet published.

## Monitoring and closeout

Run the health check from the exact launch checkout:

```bash
Rscript application/scripts/21_healthcheck_rqr_dlm_confirmatory_simulation.R \
  /data/muscat_data/jaguir26/.rqr_gibbs_frozen_runs/rqr_dlm_diagnostic_aware_maximum_20260807_01059c6/run \
  /data/muscat_data/jaguir26/.rqr_gibbs_frozen_runs/rqr_dlm_diagnostic_aware_maximum_20260807_01059c6/control
```

Do not interpret the absence of a diagnostic warning as empirical calibration,
and do not interpret a warning as permission to discard a replication.  At
completion, audit all 110 waves, all hard-failure ledgers, warning counts by
scenario/method/estimand, exact-target and repair fields, recursive hashes,
resources, and the all-results versus unflagged sensitivity comparison before
updating manuscript evidence.
