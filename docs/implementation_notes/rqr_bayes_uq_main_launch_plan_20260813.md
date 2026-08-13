# RQR Bayesian UQ Main Launch Plan

Date: 2026-08-13

Config: `application/config/rqr_bayes_uq_validation_main_20260813.json`

Worker: `application/scripts/69_validate_rqr_bayes_uq.R`

Wave manager: `application/scripts/71_manage_rqr_bayes_uq_main_waves.R`

Detached launcher: `application/scripts/72_launch_rqr_bayes_uq_main_waves.sh`

## Purpose

Run the main iid univariate validation for the authoritative Bayesian-scan
tolerance action after the completed moderate pilot.  In this launch, coverage
levels are guaranteed population contents, not regression or dynamic coverage
claims.

## Scope

- IID univariate continuous DGPs only.
- No regression tolerance claims.
- No temporal or dynamic tolerance claims.
- No posterior endpoint-coverage claim.
- The scan count is fixed before Bayesian posterior content is evaluated.

## Main Grid

- Sample sizes: `500`, `1000`.
- Guaranteed contents: `0.90`, `0.95`, `0.99`.
- Repeated-sample tolerance confidence: `0.95`.
- Posterior confidence thresholds: `0.90`, `0.95`, `0.99`.
- Replications: `100`.
- Paired threshold seeds: enabled, so posterior-threshold comparisons reuse the
  same generated sample within each DGP, sample size, content, and replication.
- Wave split: one wave per DGP, sample size, guaranteed content, and tolerance
  confidence.  With the current grid this is `42` waves.
- Result rows: `88,200` in the main confirmatory launch
  (`12,600` datasets times `7` methods, including the non-deployable oracle).

## DGPs

- Standard normal.
- Centered standardized lognormal.
- Hard centered standardized lognormal.
- Standardized separated normal mixture.
- Standardized sharp separated normal mixture.
- Standardized five-percent scale-contaminated normal.
- Standardized Student t with three degrees of freedom.

## Methods

- `oracle_sh`: true population shortest interval oracle with exact mean tilt.
  This is a synthetic-DGP benchmark, not a deployable fitted method.
- `hdp_s_mc`: authoritative hybrid direct-DP Bayesian-scan shortest interval
  with MC scan calibration.
- `tcsp_mc`: scan-only TCSP shortest interval with MC scan calibration.
- `tcsp_dkw`: conservative DKW scan stress comparator.
- `split_empirical_shortest`: split exact-spacing empirical-shortest action.
- `wilks_minmax`: full-sample min-max comparator.
- `bb_shortest_diag`: Bayesian bootstrap diagnostic.

Standalone `dp_bayes` and `dpm_bayes` are not included inline in the main
launch.  The authoritative hybrid action already evaluates direct-DP posterior
content after the fixed scan count, while the all-order-statistic standalone
Bayesian diagnostics are too slow at `n = 500/1000` for the main grid.  They
remain available in the smaller `dpm_companion` mode with `10` replications,
four DGPs, two sample sizes, and contents `0.90` and `0.95`.

## Diagnostics

The runner records:

- retained fraction `k/n`;
- scan content buffer `k/n - c`;
- scan certified lower probability;
- content gap `true_content - c`;
- posterior threshold excess;
- posterior constraint status and binding rate;
- candidate feasible count and candidates evaluated;
- width ratio and width difference relative to `tcsp_mc`;
- oracle shortest interval width, oracle mean tilt, and certificate digest;
- width ratio and width excess relative to the true `oracle_sh` interval;
- infeasible rate and cell-level success-rate gap against tolerance confidence.

The operational diagnostic reference remains `tcsp_mc`.  The oracle is used
only to quantify the efficiency gap against the true population shortest
interval in synthetic iid DGPs.

## Preflight And Wave Execution

The main launch is no longer run as one serial job.  A preflight stage freezes
the config, writes a canonical wave plan, precomputes shared scan calibrations,
and precomputes the oracle certificates.  The frozen run directory contains:

- `config_frozen.json`;
- `wave_plan.csv`;
- `scan_calibration_cache.rds` and `scan_calibration_summary.csv`;
- `oracle_cache.rds` and `oracle_reference.csv`;
- `preflight_manifest.json`;
- `preflight_artifact_hashes.csv`.

The shared scan cache avoids repeating identical MC scan calibrations across
waves.  High-content DKW infeasibility is recorded as an infeasible calibration
object rather than stopping the launch.

## Launch

Smoke:

```sh
make rqr-bayes-uq-main-smoke
```

Detached main launch:

```sh
make launch-rqr-bayes-uq-main
```

This prepares a fresh run directory if `RQR_BAYES_UQ_MAIN_RUN_DIR` is unset,
then starts the wave scheduler in the background.  The default scheduler limit
is `6` concurrent waves:

```sh
make launch-rqr-bayes-uq-main \
  RQR_BAYES_UQ_MAIN_WAVE_MAX_CONCURRENT=6
```

Prepare only:

```sh
make prepare-rqr-bayes-uq-main-waves
```

Detached DPM companion launch:

```sh
make launch-rqr-bayes-uq-main-dpm-companion
```

Health check after completion:

```sh
make health-rqr-bayes-uq-main \
  RQR_BAYES_UQ_MAIN_RUN_DIR=application/runs/rqr_bayes_uq_validation_main_20260813/<run_id>
```

The same health target also works while waves are running.  It reports complete,
running, pending, failed, completed rows, expected rows, and remaining rows.

Manual collection, if the scheduler was stopped after all waves completed:

```sh
make collect-rqr-bayes-uq-main \
  RQR_BAYES_UQ_MAIN_RUN_DIR=application/runs/rqr_bayes_uq_validation_main_20260813/<run_id>
```

## Post-Run Decision Rules

- If `hdp_s_mc` and `tcsp_mc` are identical across most cells, posterior UQ is
  diagnostic rather than binding under this launch and should be tuned next.
- If `hdp_s_mc` is wider than `tcsp_mc` only in hard DGP/high-content cells,
  promote that as the main Bayesian-UQ contribution.
- If MC scan calibration has high infeasibility at `c = 0.99`, report the
  infeasible cells explicitly and tune the admissible high-content design after
  this launch.
- Treat `dpm_bayes`, `dp_bayes`, and `bb_shortest_diag` as diagnostic
  competitors unless they are wrapped by a formal scan/tolerance action.
- Treat `oracle_sh` as a non-deployable population benchmark.  It can guide
  tuning of the fitted methods, but it cannot be used by an application method.

## Superseded Serial Attempt

The earlier serial confirmatory attempt was stopped after only a few datasets
and was not collected or promoted as evidence.  It should be treated as a
runtime diagnosis showing that the main study needs shared calibration and
wave-based execution.
