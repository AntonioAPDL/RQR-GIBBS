# Completed Calibrated MTI-Gibbs Assessment

## Statistical Role

The primary article comparison remains MTI-ECM, Young--Mathew, and Wilks.
The supplementary calibrated-Gibbs assessment uses a separate common
Monte Carlo cohort. It evaluates the complete procedure consisting of ECM
candidate selection, a fixed empirical interior-tilt rule, fixed-target
Gibbs posterior means, and independently calibrated interval half-widths.
This comparison does not isolate posterior means versus modes at identical
targets; the historical matched-target assessment does that separately.

Independent Normal(0, 2^2) raw-root priors use fixed simulation coordinates.
The learning rate is one. The DP has concentration one and Normal(0, 4^2)
base. Its interval probability supplies a candidate eligibility threshold,
not a response likelihood for the loss-based root posterior. Selection
changes that threshold only for n=500,c=.90. The selected q and projected
tilt are fixed within each four-chain marginal slice-within-Gibbs fit.
Posterior draws describe interval roots rather than future responses.

The completed source is `e8456e7717e2a863acfe2917efc303a206cd890d`, with
72,000 independent calibration and 72,000 independent validation datasets
following 7200 developmental datasets. Each stage uses eight distributions,
nine sample-size/content designs, and p=.95. Radius calibration uses the
973rd of 1000 required-scale observations per law and the maximum across
laws within each design. Its simultaneous confidence budget protects 144
baseline/proposed rule/case statements with error .05. This assurance is
conditional on development and confined to the specified simulation panel.
Neither the selected thresholds nor the radius factors are transferred to
the pharmaceutical application.

## Reproduction

`application/config/mti_gibbs_decision_reporting_20261118.json` pins the
completed source and evidence receipt. The reporting driver first verifies
the completed evidence, re-derives 432 case/method summaries from 432,000
interval records, and checks 144,000 saved posterior numerical summaries.
This needs no posterior refitting. To import the completed study:

```bash
Rscript application/scripts/135_report_mti_gibbs_decisions.R prepare /path/to/completed/output
make report-mti-gibbs-decisions
make test-mti-gibbs-decision-report
make verify-mti-gibbs-decision-report
```

The tracked compact evidence suffices for `render` and `verify` on another
machine without fitted objects or the original scratch directory. The input
hashes retain the provenance of the original interval and posterior records.
The new numerical macros use the `MTIDecision` prefix, preserving the earlier
primary simulation and application numbers. Heavy fits remain local-only.

Every figure compares methods on the same fresh samples. Width segments
are within-law empirical 2.5%--97.5% quantiles, crosses are means, and widths
remain in raw units on logarithmic axes. Attainment segments are pointwise
exact binomial confidence intervals, with an axis of .85--1. They are
different quantities from width ranges and from across-law attainment
ranges. Wilks segments are solid black. Population shortest-content markers
appear only on width figures and do not represent sample-based tolerance
procedures. All nine designs have separate width and attainment figures;
the supplement includes the n=500,c=.99 pair and one compact design table.

The completed calibrated rule attains content in 96.4%--100% of validation
samples across cases. All 72 pointwise lower 95% confidence limits exceed
.95, and 69 one-sided simultaneous lower limits exceed .95. Mean widths
are smaller than ECM in 9 cases, Young--Mathew in 38, and Wilks in 51.
All 144,000 posterior summaries meet the numerical policy. The maximum
Rhat is 1.009363, and no more than 2000 retained draws per chain are needed.

## Separate Unscaled Experiment

The [bounded unscaled comparison](mti_gibbs_joint.md) is a distinct ongoing
experiment. It compares fixed root/DP priors and event-based thresholds
against this frozen calibrated reference. Its development and any conditional
fresh assessment are not included in the completed tables or manuscript
claims above. No new method is promoted before independent assessment.
