# Final MTI Calibration and Independent Comparison

## Statistical Purpose

For a sample of size n, the target is a data-dependent interval with population
content at least c in at least a fraction p of repeated samples. Fitted content
q in the MTI loss and the DP posterior probability threshold gamma are separate
quantities. The study fixes p=.95 and chooses one gamma per (n,c,p), shared by
all distributions. The generating CDF assesses content; it is never used to
fit an interval or select its tilt within a sample.

The numerical procedure profiles the fixed-target MTI criterion and selects
the shortest eligible candidate from a finite content/tilt menu. It is not an
ECM iteration and is not relabelled as one. The refined menu retains every
original q and adds targets in previously unrepresented positive effective-mass
rank cells. The root prior, learning rate, DP base and concentration, tilt menu,
solver, and deterministic tie rules remain unchanged. Neither continuous outer
optimality nor universal frequentist validity is claimed.

## Fixed Design and Data Roles

The source specification is
`application/config/mti_q_grid_refinement_final_20261022.json`. It retains the
eight distributions and nine (n,c) settings of the completed comparison:
Gaussian, Student t(3), exponential, asymmetric Laplace, two-piece Gaussian,
Beta(1,8), Gamma(0.5,1), and log-normal. Distribution parameters and sample-size
settings are inherited from the hash-checked original specification.

The completed numerical screen has 72 test samples and source commit
`8748e9e1bbc2d20c0a7368932d973b99c8bdf78f`. Its record and every numerical package
source are checked before reuse. It supplies numerical evidence only.

Each of 72 distribution/design cases receives 32 new threshold-development
samples and 24,000 independent calibration-assessment samples: 1,730,304 samples
in total. Within a design setting, 25 empirical quantiles of the development
posterior probabilities and the requested confidence p define at most 26
thresholds. Probabilities used to develop thresholds are strictly between zero and one.
Development values define the menu before any assessment outcomes are used.

For every distribution and threshold, the exact one-sided Clopper--Pearson
lower bound uses alpha=.05/(9*8*26). A threshold passes only when every
assessment returns an interval and every distribution's lower bound is at
least p. The smallest passing threshold is frozen. At 24,000 assessments,
22,935 content attainments are required for this binomial comparison. This
calculation concerns Monte Carlo precision; it does not assume MTI attainment.

The 24,000-sample budget was fixed after examining precision of the completed
study and before generating the new calibration samples. It is not an optional
stopping rule. There is no automatic sample increase or replacement of a sample
after a poor result. Any numerical failure prevents a passing final calibration.
These bounds concern the stated finite distribution panel, not every possible F.

Only if all nine settings pass does the same authorized run continue to 1,000
new samples per case. The 72,000 validation samples give 288,000 paired
evaluations: refined MTI, frozen profiled MTI, Young--Mathew, and sample-range
Wilks. Validation seeds are disjoint from development, calibration, screen,
earlier validation, and retired seed ranges. Selection is never retuned on
validation outcomes. An unresolved calibration ends the run without validation.

## Execution and Preservation

The runner is `application/scripts/99_run_mti_q_grid_study.R`; the launcher is
`application/scripts/100_manage_mti_q_grid_study.sh`. The new helpers are
`application/scripts/lib/mti_final_study.R` and `mti_final_reporting.R`.
Existing Make targets are unchanged; `*-mti-final-study` targets use the new
fixed specification. Supply absolute preserved input directories:

```bash
export MTI_Q_BASELINE_RUN=/absolute/path/to/completed/independent_comparison
export MTI_Q_SCREEN_RUN=/absolute/path/to/completed/numerical_screen
make prepare-mti-q-grid
make test-mti-q-grid test-mti-final-study
make preflight-mti-final-study
make precision-mti-final-study
make launch-mti-final-study MTI_Q_FINAL_WORKERS=40
make health-mti-final-study
```

The launcher requires clean committed source and materializes this repository's
package from `git archive` under ignored `application/cache/`. It records the
source commit, package/native hashes, installed R and tolerance versions,
configured exdqlm commit, seeds, protected input hashes, and upstream evidence.
No external source checkout is built, loaded, or modified. The current configured
exdqlm pin is `dffb71ee70b597d6a716ee74be1cbc99731cd453`.

Up to 40 single-threaded workers process 400-sample batches. Cached immutable
inputs reproduce uncached candidates exactly, including RNG state. Checkpoints
retain candidate endpoints, q, tilt, objective diagnostics, counts, DP
probabilities, and true simulation content. Assessment uses two bounded-memory
passes; both passes verify task identity and hashes. Sample regeneration checks
closed-interval counts and CDF contents independently. This changes resource
use, not candidate selection or binomial assessment. Tests compare the bounded
assessment with the original in-memory calculation exactly.

The initial resource check requires available CPU headroom, at least 16 GiB
available memory, and free disk exceeding three times the estimated calibration
checkpoint size. It saves the estimate and machine state. A stage lock prevents
duplicate launches. Task errors are saved, halt subsequent batches, and remain
errors on resume. Completed stages and reviews verify their result hashes.
Terminal receipts require explicit review before manually resuming a stage.

## Independent Reporting and Adoption

Validation reports unconditional content attainment, pointwise binomial
intervals, simultaneous primary-method intervals, nonreturns, numerical
diagnostics, paired comparisons, and raw widths. Width means and empirical
2.5%--97.5% quantiles are computed within each distribution/design/method over
returned intervals; absent widths remain undefined. An across-distribution
attainment range is distinct from a Monte Carlo confidence interval.

The separate `review/` directory contains a three-method comparison of refined
MTI, Young--Mathew, and Wilks, grouped per-distribution width tables, attainment
tables, and figures. The frozen MTI comparator remains in the complete validation
assessment. Wilks identifiers are mapped explicitly to the plotting convention,
with original identifiers retained in the width CSV. Figure limits include all
observed attainment rates. Width plots retain raw 95% ranges, mean marks, and
the existing population shortest-content reference; they do not normalize by TCSP.

No manuscript, canonical table, figure, application result, or historical
checkpoint is replaced automatically. Scientific adoption requires reviewing
independent attainment and its uncertainty, width changes, paired losses and
gains, numerical failures, and unresolved limitations. A negative refinement
result is a valid final outcome. The application helper `mpq_frozen_action()`
accepts only an explicitly supported frozen policy setting and does not require
the true F. A full application rerun and article revision remain conditional on
adoption. Neither calibration nor numerical success guarantees returnability
for every possible future sample.

## Verification

Focused tests cover fixed budgets and seed separation, cached numerical parity
across all eight distributions, exact bounded/in-memory calibration agreement,
checkpoint and count alteration, task-error preservation, unresolved-calibration
stopping, conditional validation, supported application settings, and review
tables/figures. Existing profile, fixed-policy, figure, and manuscript checks
are retained. Plans and execution checklists remain under ignored `.codex_work/`;
this file documents the reproducible implemented study.
