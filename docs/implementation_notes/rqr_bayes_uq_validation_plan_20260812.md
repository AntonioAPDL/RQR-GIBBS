# RQR Bayesian UQ Validation Plan

Date: 2026-08-12

Config: `application/config/rqr_bayes_uq_validation_v1.json`

Runner: `application/scripts/69_validate_rqr_bayes_uq.R`

Launcher: `application/scripts/70_launch_rqr_bayes_uq_overnight.sh`

## Purpose

Validate the new full-distribution Bayesian UQ layer before any confirmatory
simulation campaign. The study measures population content and width under
known iid univariate DGPs. It is empirical validation evidence, not a proof of
posterior endpoint coverage or scan finite-sample validity.

## Methods

- `hdp_s`: hybrid direct-DP Bayesian-scan shortest interval. This is the primary
  action for the new UQ story.
- `dp_bayes`: direct-DP Bayesian order-statistic action without the scan count.
- `dpm_bayes`: Gaussian DPM Bayesian order-statistic action with Monte Carlo
  posterior content probabilities.
- `bb_shortest_diag`: Bayesian-bootstrap posterior shortest diagnostic.
- `tcsp_dkw`: scan-only TCSP shortest window with conservative DKW calibration.
- `split_empirical_shortest`: pilot/main split exact-spacing comparator.
- `wilks_minmax`: full-range nonparametric order-statistic comparator.

## Modes

- `smoke`: fast end-to-end validation of config, APIs, artifact publication, and
  health checks.
- `moderate`: overnight pilot. This is authorized by the config and is the
  largest run this branch launches automatically.
- `confirmatory`: disabled. A future confirmatory study requires a new reviewed
  config and source commit.

## Reproducibility Contract

- The runner writes only under ignored output roots.
- The launcher refuses to start from a dirty source tree.
- The manifest records the git commit, config path, row counts, claim flags, and
  artifact hashes.
- Health checks are read-only and require all declared artifacts to exist.
- The scan count is fixed before posterior computation and is never resampled.
- MT-RQR plug-in summaries remain generalized-Bayes fixed-target comparators.

## Next Study Gate

After the overnight moderate pilot finishes, inspect:

- infeasible rates by method/content/confidence;
- empirical success rates for formal tolerance actions;
- width ratios against scan-only and split exact-spacing comparators;
- DPM elapsed time and posterior-probability stability;
- whether posterior thresholds are binding or redundant for `hdp_s`.

Only after those diagnostics pass should a confirmatory validation design be
specified.
