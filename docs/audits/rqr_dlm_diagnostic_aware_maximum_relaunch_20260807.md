# RQR-DLM diagnostic-aware maximum-run relaunch receipt

Date: 2026-08-07

## Status

The diagnostic-aware maximum RQR-DLM study was relaunched from a fresh source,
runtime, evidence, authorization, run, and control root after the first launch
stopped before any fit.  The first failure is documented in
`rqr_dlm_diagnostic_aware_maximum_launch_20260807.md`; none of its outputs are
reused here.

The replacement coordinator is running in the background.  It passed the
worker-side authorization boundary that stopped the first attempt, created all
eight first-wave worker processes, and entered scientific computation with
empty worker stderr logs.  This receipt records a live launch, not a completed
study.

## Corrected source boundary

| Item | Value |
|---|---|
| Exact source and authorization commit | `ea8ea8d17c6f7bb34b015472e4f60f62e547c942` |
| Development branch | `codex/rqr-dlm-m11-multicomponent-recovery-20260806` |
| Package version | `rqrgibbs 0.1.0.9034` |
| Launch checkout | `/data/muscat_data/jaguir26/.rqr_gibbs_launch_checkouts/rqr_dlm_diagnostic_aware_authfix_ea8ea8d` |
| Primary runtime tree digest | `c33d23f76a71a52e3c4e0d7f1a65f79c0955562a9607d5a36c8a2ec1a2bd21cc` |
| Runtime-attestation SHA-256 | `53d1267b61b6ec7bd2af31ade660374013e1b1397263edd176d96cab4d4ce10c` |
| Execution-policy SHA-256 | `efde1fe678cedb6326e64d2cdcdfa6db7e2a17f5831dd5fd75b922daafb291bd` |

The correction routes the worker runner's second authorization boundary
through the diagnostic-aware validator and then compares the actually observed
source cleanliness, primary runtime, preflight and reference manifests, seed
ledger, canonical task plan, comparator source packages, comparator dependency
runtimes, toolchain, and protected-checkout status with the authorization
bundle.  The legacy confirmatory lane retains its original flag-only validator.

Regression coverage was added for the observed-input binding.  The focused
diagnostic-aware suite and the complete confirmatory-contract suite passed with
the package source loaded.  The exact corrected source also passed
`R CMD check --no-manual --no-build-vignettes` with `Status: OK`.

## Launch-bound gates and hashes

| Gate or artifact | Result |
|---|---|
| Maximum-plan preflight | 23/23 passed |
| Oracle/comparator reference | 15/15 passed |
| Preflight recursive-manifest SHA-256 | `8afd90b7405bd05bba6e7a1d64cd2bba69ffb594b27b7c4c752bef0fef0fba03` |
| Reference recursive-manifest SHA-256 | `0cd944fbe5221ef0a9ed9c6104ea047f89c209681a5ad4f31227522157f968c3` |
| Authorization SHA-256 | `d803c56aaaa035fe39d3638e5261b8f745348ac0151a7843cab6f0d977daaf9a` |
| Launch-input SHA-256 | `ea3c05765c02badc78b9dc41508838688e840db6e0276b342b2144deddb0c86c` |
| Canonical wave-plan SHA-256 | `c45ece172d89a366fede5264a757c45161a3d9578a892ef1f8dfb920cbc51838` |

The isolated exdqlm and quantreg CRAN runtimes remained unchanged.  No exdqlm
or Q-DESN source checkout was mutated or used as a runtime.

## Execution identity

| Item | Value |
|---|---|
| Run ID | `rqr_dlm_diagnostic_aware_maximum_20260807_ea8ea8d` |
| Run root | `/data/muscat_data/jaguir26/.rqr_gibbs_frozen_runs/rqr_dlm_diagnostic_aware_maximum_20260807_ea8ea8d/run` |
| Control root | `/data/muscat_data/jaguir26/.rqr_gibbs_frozen_runs/rqr_dlm_diagnostic_aware_maximum_20260807_ea8ea8d/control` |
| Coordinator PID at launch | `2772700` |
| Host | `jerez.be.ucsc.edu` |
| Start time | `2026-08-07 20:36:29 UTC` |
| First wave | `static_gaussian_T200__target0200__sentinel` |
| First-wave workers | 8 |

The complete maximum design contains 8,400 DGP replication tasks, 110 waves,
43,800 method interval evaluations, 49,200 logical endpoint or model fits, and
40,938 total MCMC chain executions.  Diagnostic threshold violations are
recorded and nonblocking; hard scientific, provenance, numerical, resource,
and artifact failures remain blocking.  Precision stopping remains disabled,
so an otherwise valid run proceeds through every frozen maximum batch.

## Health check

```bash
Rscript application/scripts/21_healthcheck_rqr_dlm_confirmatory_simulation.R \
  /data/muscat_data/jaguir26/.rqr_gibbs_frozen_runs/rqr_dlm_diagnostic_aware_maximum_20260807_ea8ea8d/run \
  /data/muscat_data/jaguir26/.rqr_gibbs_frozen_runs/rqr_dlm_diagnostic_aware_maximum_20260807_ea8ea8d/control
```

The run is diagnostic-aware and is not automatically convergence validated.
Final closeout must report every diagnostic warning and publish both the
all-results aggregate and the predeclared unflagged sensitivity aggregate.
