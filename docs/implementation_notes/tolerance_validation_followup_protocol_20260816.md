# Tolerance Validation Follow-up Protocol, 2026-08-16

This note records the final follow-up validation design after the primary iid
tolerance study completed.

## Primary Evidence Reused

The completed primary run is
`application/runs/rqr_bayes_uq_validation_main_20260813/wave_main_20260813T103232Z`.
It covers seven iid continuous DGPs, sample sizes `500` and `1000`, contents
`0.90`, `0.95`, and `0.99`, tolerance confidence `0.95`, three posterior
content thresholds, and 100 replications per cell. The article table is
generated from
`final_combined_grid_complete_method_summary_with_young_mathew.csv` by
`tables/generate_tolerance_validation_summary_table.R`.

## Follow-up Lanes

The follow-up config is
`application/config/rqr_bayes_uq_followup_20260816.json`.

- `ecm200_audit`: reruns the primary grid only for methods needed to audit the
  fixed-target MTI ECM row at 200 iterations. It records initial/final objective,
  relative objective drop, final stationarity, and trace length.
- `paper_matched_90`: uses the full-range threshold cells `(38, 0.90)`,
  `(77, 0.95)`, and `(388, 0.99)` at tolerance confidence `0.90`, matching the
  feasibility convention used in the calibrated Bayesian nonparametric tolerance
  interval paper.
- `small_sample_95`: uses the corresponding `0.95`-confidence thresholds
  `(46, 0.90)`, `(93, 0.95)`, and `(473, 0.99)`, plus practical `n = 50` and
  `n = 100` stress cells. These cells are paired explicitly in the config to
  avoid an unintended Cartesian product.

## Feasibility Check

For the two-sided full-range interval, the finite-sample content/confidence
probability is

```text
1 - n * (1 - c) * c^(n - 1) - c^n.
```

The smallest sample sizes are `38, 77, 388` for contents `0.90, 0.95, 0.99` at
confidence `0.90`, and `46, 93, 473` at confidence `0.95`.

## Launch Policy

The largest follow-up run should be launched through the wave manager with a
recorded clean commit. On Jerez, use a combined concurrency budget near 40 only
when CPU load is low. The default make target is
`launch-rqr-bayes-uq-followup`, with mode selected by
`RQR_BAYES_UQ_FOLLOWUP_WAVE_MODE`.
