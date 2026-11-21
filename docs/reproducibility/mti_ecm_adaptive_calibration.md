# MTI-ECM Calibration Using Fitted Candidates

This exploratory study investigates shorter MTI-ECM intervals without
adjusting their endpoints after estimation. Both manuscripts and all reported
application, ECM and Gibbs results remain unchanged. A completed computation
does not automatically replace a reported procedure.

## Statistical Aim

For independent continuous observations from F, the desired probability is

    A_F = P_F^n{an interval is returned and F(U)-F(L) >= c} >= p.

Here c is required population content and p is tolerance confidence. The
fitted content q and placement tilt delta index generalized-Bayes MTI root
estimates. A direct DP model for F supplies a separate posterior probability
that a candidate interval contains at least c. Its eligibility cutoff gamma
is not the tolerance confidence p.

The primary objective is empirical attainment near p with shorter intervals.
Monte Carlo confidence intervals describe uncertainty about that attainment.
Their lower endpoints are not all required to exceed p. Compatibility with p
also does not establish that the true attainment exceeds p. Reports show
point estimates, exact intervals and clear deficits separately. The evidence
concerns the specified distributional panel, not a distribution-free theorem.

## Fixed Model and Designs

Root priors remain independent Normal(0,2^2), inverse loss scale is one,
DP concentration is one and the base distribution is Normal(0,4^2). The
simulation coordinates are independently specified as center zero and
scale one. No analyzed sample specifies a prior, DP base or loss scale.
There is no endpoint-radius multiplier, substitute interval, or Gibbs
sampling in this experiment. ECM estimates modes of a loss-based generalized
posterior; it does not draw posterior predictive responses.

All eight current distributions are retained: Normal, Student t3,
Exponential, asymmetric Laplace at tau .10, two-piece Normal with scale
ratio 1:12, Beta(1,8), Gamma(.5,1), and the existing skewed lognormal law.
Exact transformations are read from the frozen data generators. The designs
at p=.95 are n=50 with c=.90; n=100 with c=.90,.95; and n=500,1000 with
c=.90,.95,.99. There are 72 distribution/design cases. Numerical controls
and C++ fitting remain those of the current fixed-scale ECM calculation.

## Why This Study Is Separate

The preceding root/DP study already considered root SD 1 or 2, three DP
concentrations and three DP base scales. The completed Gamma-scale screen
produced no shorter selected intervals. The previous relaxed-cutoff study
had some materially low attainment on independent assessment. None of
these searches is repeated as though it were new evidence.

The already inspected 72000-sample assessment is now explicitly development
data. Its sampling commit is 9df0b9cb3f487f34e2a5909ecb0153c2dab7c6b6 and
its successful separate finalization commit is
63c0a584197f20580b9c5ad1725fb784b11085b9. Its original historical export
failure and its later successful finalization are preserved separately.
Every reused fit is checked against its receipt, exact observation hash,
independent prior and numerical accuracy. Closed-interval counts and DP
probabilities are independently reconstructed.

## Development and Stopping Rules

The controller carries out the following stages:

1. Audit all 72000 saved samples without refitting them. Store compact
   selection paths and observable sample features, not copies of all fits.
2. Compare constant cutoffs with one split on absolute Bowley skewness or
   log interdecile-range/IQR. Quantiles use type 8. Cutoff pairs are selected
   from at most 64 rank-spaced observed probability events plus the exact
   best eligible constant-event region and the reference cutoff. This is a
   bounded search, not an exhaustive optimum over arbitrary rules.
3. Use five folds grouped by dataset and stratified through replication
   number. Each fold learns its split, cutoff menu and cutoffs from its
   training samples only. A proposal advances within a design only when
   held-out point attainment reaches p in every distribution, mean width
   decreases by at least 1% in at least one distribution, and at least
   three folds show a width gain. A constant cutoff is preferred unless
   the feature rule has a further meaningful advantage.
4. If enabled, fit at most 24 new q/delta pairs on each of the first 100
   existing samples per case. These are intermediate fitted contents and
   tilts near competitive candidates. Original candidates are retained
   exactly. New candidates use the existing C++ row fitter followed by the
   unchanged numerical refinement and exact conditional-root checks.
5. Extend refinement to all 1000 development samples only in designs with
   meaningful search headroom. The screening statistic uses known F to
   identify potentially useful added candidates. It is an oracle-assisted
   development diagnostic, not performance of a deployable procedure.
   The fitter itself receives no CDF, distribution label or attained content.
6. Compare refined-grid/original-cutoff and calibrated-grid alternatives.
   Choose the simpler procedure within 1% of the best internal mean-log
   width-ratio score. Ratios are used only for development comparisons;
   reported width summaries remain raw and distribution specific.
7. Retain the original ECM rule in designs without a supported improvement.
   This is a frozen design-level choice within MTI, not a substitute interval
   from another method. If no design advances, stop without fresh fitting
   and record the negative result.

The design-level choice is a deliberate refinement of the planning document:
requiring improvement in every design would discard useful supported changes
elsewhere and invite unnecessary retuning of already near-nominal cases.
Every design is still included in final assessment whenever any design
advances. The original rule is not relabeled as a newly improved fit.

Gamma never exceeds the original ECM cutoff. With retained reference
candidates, a proposed interval cannot be wider on a sample where the
reference returns an interval. This inclusion argument says nothing by
itself about population-content attainment. Attainment is not assumed
monotone in gamma, and no binary search uses such an assumption.

The sample-feature rule uses the original cutoff when its feature is
undefined because of ties or zero interquartile range. Additional-candidate
construction does not change when replaying a different proposed cutoff.
If no original interval is eligible on a fresh sample, the available fitted
candidates are still assessed at the frozen proposed cutoff; no sample is
replaced and no interval from a different method is inserted. An ineligible
reference does not trigger its proposed local refinement.

## Fresh Independent Assessment

After development, freeze one complete design-dependent procedure before
generating 1000 new datasets per case, or 72000 dataset jobs. Retain the
current ECM, Young--Mathew and Wilks on each of these same samples. Fits use
the union of necessary candidates once; cutoff comparisons do not refit
the same roots. Historical observations and all recorded random-seed
allocations are checked for overlap. Final observation hashes are checked
against development and other final samples.

This implementation uses the economical one-proposal route. It does not
insert a second fresh selection panel or automatically perform precision
extensions. The alternative two-finalist selection route in the local plan
is not needed after selecting one development-frozen proposal, and requests
to enable it are rejected rather than silently treated as implemented.

A numerical error stops the controller and preserves the failed job.
No eligible interval is a distinct statistical result and counts as
nonattainment. Classical comparator errors in the declared feasible designs
also stop for review. No dataset is omitted or regenerated to improve results.

## Commands

From the implementation worktree:

```sh
make test-mti-ecm-adaptive-calibration
make prepare-mti-ecm-adaptive-calibration
make check-mti-ecm-adaptive-calibration
make launch-mti-ecm-adaptive-calibration
make health-mti-ecm-adaptive-calibration
make verify-mti-ecm-adaptive-calibration
```

The installed numerical library defaults to
`application/cache/gamma_scale_library`; set
`MTI_ECM_CALIBRATION_R_LIBRARY` to a source-matching installed library if
needed. Never load a package directly from an external source checkout.
The integrated check recomputes four historical samples spanning small n,
skewness and c=.99, verifies reference reproduction, and tests refinement.
It also checks the selected targets from two additional root initializations
per sample; a better objective or unresolved numerical calculation stops the
check for review. It uses development observations only.

The launcher requires committed source, creates an isolated detached
checkout, copies the verified installed library, and records exact source,
package and external reference commits. The external exdqlm reference is
dffb71ee70b597d6a716ee74be1cbc99731cd453. The launcher does not mutate it.
Outputs default to `/var/tmp/jaguir26-mti-studies` to avoid the nearly full
`/data` filesystem. `MTI_ECM_CALIBRATION_STORAGE_ROOT` changes this location.
The local pointer is `.codex_work/mti_ecm_adaptive_calibration_latest.json`.

Up to 40 workers are used when CPU allocation, load and free memory allow,
with one numerical-library thread per process. Full fitting stages use one
dataset job per worker. A small design-level development stage has at most
nine independent jobs and does not create unnecessary nested parallelism.
At least 32 GiB free disk is reserved, and observed file sizes are used to
check projected storage. Completed jobs have atomic RDS files and receipts;
resumption verifies them and does not overwrite an unresolved failure.

## Saved Results

The main outputs are:

- `audit/summary.json` and the audited case records;
- `original_development/` and conditional `refined_development/` records,
  including held-out scores and the rule learned in each fold;
- `refinement_screen/case_summary.csv` and design continuation decisions;
- `development_case_scores.csv`, `selected_development_rule.rds` and JSON;
- conditional `frozen_rule.rds`, `frozen_rule.json` and `validation_tasks.rds`;
- conditional `validation/interval_results.csv`, `comparison_summary.csv`,
  `paired_comparison.csv`, `method_overview.csv`, and
  `ecm_calibration_diagnostics.pdf`;
- `study_summary.json`, `verification.json`, controller log and exit status.

Per-case results contain unconditional attainment, exact Monte Carlo
confidence intervals and SEs, return counts, raw width means and empirical
2.5th--97.5th width percentiles, population-content summaries, and paired
raw-width changes. Raw widths are not averaged across distributions.
Widths are conditional on return and carry their own denominator. Their
empirical ranges are not credible intervals or confidence intervals for
the mean. Reports distinguish uncertain nominal attainment from a clearly
low probability; no claim of a new guarantee follows from compatibility.

Selected and nearby added-candidate traces retain roots, objectives, width
and final numerical information. Every added pair and numerical seed is
saved, allowing exact reconstruction without retaining all full traces.
Original candidates and their traces are not overwritten. Interval CSVs
preserve 17 significant digits and are checked by reading them back.

The final verification reconstructs every old observation, every independent
sample, frozen selection and population-content score. It checks that
manuscript hashes and installed numerical files remain unchanged. Any
manuscript or application inclusion requires a subsequent scientific review
under `STYLE_PROFILE.md`; neither follows automatically from job completion.
