# Independent Assessment of the MTI Intercept Revision

The numerical revision is described in `mti_intercept_profile_20261021.md`.
This separate reporting implementation reads its saved calculation without
changing the optimizer, candidate menu, calibration, data, or article inputs.

## Statistical Quantities

For a returned interval `[l,u]`, population content in the simulation is
`F(u)-F(l)`, evaluated under the specified continuous data-generating law.
Content attainment is the proportion of **all** simulated datasets for which
this quantity is at least the requested `c`. A nonreturned interval counts
as a failure in this proportion. This estimates the repeated-sampling
probability compared with the requested tolerance confidence `p`.

Width summaries are computed separately for each distribution, sample size,
content, confidence, and method. They contain the mean and the 2.5%/97.5%
empirical quantiles among returned intervals, with the return count retained.
The empirical range measures variation across datasets. A separate Monte
Carlo standard error describes uncertainty in the estimated mean.

Paired comparisons require identical dataset identities and seeds. They
report discordant attainment counts and paired attainment changes. Width
changes are evaluated among jointly returned pairs, with this denominator
explicit. Raw widths are never pooled across distributions or divided by a
scan width. No median-time or timing columns are added to the article.

## Calibration and Validation Decisions

The report specification is
`application/config/mti_profile_review_20261021.json`.
It does not alter the original calibration criterion or validation stop.
The historical threshold 0.995 is retained only to verify the previously
declared development grid; it is not a guessed replacement setting.

Calibration checks independently reconstruct the grid from the separate
threshold-development samples, reselect candidates at every declared
threshold, and reproduce returned counts, success counts, and mean widths.
The exact one-sided binomial lower bounds and least-passing-threshold
decisions are checked independently. A complete dataset count and a passed
statistical assessment are distinct states.

If validation eventually completes, the report computes pointwise exact
binomial intervals and simultaneous two-sided bounds for the primary method
over the full family/design panel. Bonferroni tail probabilities use the
confidence level already specified by the study. No noninferiority margin
or alternative delivery target is introduced.

The primary-method classifications are:

- `strong_panel_evidence`: the simultaneous lower bound is at least `p`.
- `evidence_of_shortfall`: the simultaneous upper bound is below `p`.
- `inconclusive`: the interval includes `p`.
- `numerical_issue` or `nonreturns_observed`: computational/return diagnostics
  require assessment before any adoption decision.

Strong evidence on the selected simulation panel is not a distribution-free
tolerance theorem. The report never promotes a method, changes a threshold,
or replaces manuscript results. Numerical accuracy can justify improved
computation even if widths are unchanged; a width advantage requires its
own empirical evidence.

## Precision Planning

For continuous iid observations, the content of the sample minimum--maximum
interval follows `Beta(n-1,2)`. The report uses this known law as an explicit
**planning reference only**, not as an assertion about the MTI procedure.
It computes the exact probability that a binomial lower bound exceeds `p`
over a grid of additional replication budgets. The grid begins at the
existing replication count and doubles; no new simulations are launched.

Two error allocations are displayed: the original multi-threshold
assessment and a hypothetical new assessment with one fixed policy per
design cell. The latter removes the threshold-search factor only when the
policy is frozen before fresh sampling. Its probabilities describe one
family/design case, not the probability that every case passes together.
Neither an MTI attainment rate nor a new replication budget is assumed.

A new complete-panel assessment would support one coherent confidence
statement. Combining selected old confidence bounds with repeated fresh
assessments needs a declared joint error allocation. A failed or inconclusive
assessment cannot be repeated until it passes under the same nominal claim.

## Reproduction

Set the explicit frozen run directory; do not infer it from the newest folder.

```bash
make test-mti-profile-review
make health-mti-profile-review MTI_PROFILE_RUN_DIR=/absolute/path/to/frozen/run
make review-mti-profile-revision MTI_PROFILE_RUN_DIR=/absolute/path/to/frozen/run
```

Reports are local-only under `application/outputs/mti_profile_review_20261021/`.
Use a fresh output directory for every full report, through
`MTI_PROFILE_REVIEW_DIR`; prior reports are not overwritten. Full reports
require an inactive study. During execution use the fast health snapshot or
queue the completion-dependent report to avoid mixing stage transitions.
They include `REPORT.md`, `assessment.json`, progress and dataset audits,
threshold and cell summaries, precision-reference calculations, and a PDF.
When independent validation is available, per-family raw-width and attainment
summaries, paired comparisons, and diagnostic figures are added. The Wilks
lines are solid black. Figure text defines the distinction between empirical
width ranges and uncertainty intervals for attainment probabilities.

For a still-active study, queue exactly one completion-dependent report:

```bash
make queue-mti-profile-review MTI_PROFILE_RUN_DIR=/absolute/path/to/frozen/run
```

The small detached job waits on the existing launch lock, without polling,
refitting, or continuous monitoring. It runs once after the fitting/assessment
process exits, including an inconclusive calibration exit. It records its
own PID, log, and terminal status in the separate report directory. If the
study has already ended, reporting starts immediately. Queueing requires a
clean committed reporting checkout.

## Integrity and Tests

Checks cover frozen source and package hashes, seed schedules, data roles,
duplicate tasks, complete replication counts, endpoint/width consistency,
the attainment event, objective-gap flags, and frozen-policy identity. Final
validation content is recomputed from the frozen DGP CDFs. Known historical
article inputs are hashed before and after reporting. Report output cannot
be nested within the study directory, including through existing symlinks.

Tests use small artificial fixtures, exact binomial-test comparisons,
independent threshold selection, deliberately corrupted records, and PDF
rendering. They require no new fitted models. Repository reporting scripts
remain excluded from the R package; `make test-mti-profile-review` exercises
these tests directly, while the existing package check remains unchanged.
