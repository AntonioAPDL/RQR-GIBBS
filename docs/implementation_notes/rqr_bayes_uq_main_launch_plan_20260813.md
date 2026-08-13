# RQR Bayesian UQ Main Launch Plan

Date: 2026-08-13

Config: `application/config/rqr_bayes_uq_validation_main_20260813.json`

Runner: `application/scripts/69_validate_rqr_bayes_uq.R`

Launcher: `application/scripts/70_launch_rqr_bayes_uq_overnight.sh`

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
- Result rows: `75,600` in the main confirmatory launch
  (`12,600` datasets times `6` methods).

## DGPs

- Standard normal.
- Centered standardized lognormal.
- Hard centered standardized lognormal.
- Standardized separated normal mixture.
- Standardized sharp separated normal mixture.
- Standardized five-percent scale-contaminated normal.
- Standardized Student t with three degrees of freedom.

## Methods

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
- infeasible rate and cell-level success-rate gap against tolerance confidence.

## Launch

Smoke:

```sh
make rqr-bayes-uq-main-smoke
```

Detached main launch:

```sh
make launch-rqr-bayes-uq-main
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
