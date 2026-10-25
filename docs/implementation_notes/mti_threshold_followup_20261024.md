# Focused MTI Threshold Calibration and Independent Comparison

## Purpose

The completed study at source commit
`7135928ce8969d106d5f7497cd5ddddb3fda8270` fitted 1,730,304 datasets without
numerical failures. Seven settings passed its finite-panel calibration.
The n=50,c=.90 and n=100,c=.95 settings remained unresolved, and the
predeclared controller correctly withheld independent validation.

Saved-candidate diagnostics identified gaps in the developed posterior
probability thresholds. Higher, untested thresholds within those gaps raised
observed attainment while returning intervals for all saved samples. Because
those diagnostic thresholds used assessment data, they are development
information, not new calibration or independent validation results.

This follow-up changes threshold development for those two settings only.
It preserves every numerical candidate input, q target, prior, learning rate,
DP construction, and tie rule. The implementation profiles the MTI criterion;
it is not an ECM iteration. Fitted content q and posterior-probability cutoff
gamma have distinct roles. One gamma is selected per (n,c,p), shared across
the eight generating distributions, with p=.95.

## Independent Development and Assessment

The configuration is `application/config/mti_threshold_followup_20261024.json`.
Each unresolved setting receives 32 fresh development samples and 24,000
fresh assessment samples per distribution: 384,512 datasets total. Seeds are
separate from the completed study, original profile study, preserved screen,
retired validation ranges, and the forthcoming validation samples.

Development starts with the existing 25 empirical quantiles of candidate
posterior probabilities and the requested confidence p. It also includes
the smallest, across development samples, of each sample's largest candidate
probability when that value is strictly between zero and one. This observable
quantity provides information about candidate eligibility without using the
generating CDF or assessment outcomes.

Every adjacent gap is divided into four intervals on the logit scale: two
fixed dyadic refinement levels. Original knots are preserved, duplicate knots
are removed, and at most 105 thresholds result. This is a bounded numerical
resolution, not a prescribed posterior cutoff or a claim of continuous
optimality. The actual independently developed menu size determines the
simultaneous assessment adjustment. No threshold is added after assessment.

The full fixed budget is completed before any threshold is selected. Every
sample must return an interval; every distribution's one-sided exact-binomial
lower bound must be at least p. The smallest passing developed threshold is
selected. Numerical failures or unresolved settings stop the follow-up.
There is no replacement of samples, automatic budget increase, or repeated
assessment until passing.

## Simultaneous Error Control

The original passing subset was selected after observing the original
assessment. Its error allowance cannot simply be reassigned in proportion
to the seven retained settings. Instead, divide the total error allowance
delta=.05 equally between original and follow-up evidence.

Recompute the original bounds for all nine settings, eight distributions,
and 26 originally declared thresholds with per-comparison error
delta/(2*9*8*26). Retain the seven originally selected thresholds exactly;
each still passes under these tighter bounds. The smallest retained lower
bound is about .950584. This rechecking does not select new original cutoffs.

For a follow-up setting with independently developed menu size M, use
delta/(2*2*8*M). Conditional on all earlier data and the fresh development
data, the fresh assessment has total error at most delta/2. The simultaneous
original event also has error at most delta/2 across all original comparisons,
including settings later left unresolved. A union bound therefore controls
the error for the combined retained and newly assessed claims at delta.
This remains valid when the follow-up settings were chosen from the original
results. At M=105 and 24,000 assessments, 22,939 successes are required for
one comparison. Requirements are calculated from the actual M, not hardcoded.

These bounds concern Monte Carlo assessment over the specified finite panel.
They establish neither unrestricted distribution-free tolerance validity
nor returnability for every future sample.

## Execution and Preservation

The existing runner and launcher accept a separate `followup` mode:
`application/scripts/99_run_mti_q_grid_study.R` and
`application/scripts/100_manage_mti_q_grid_study.sh`. The follow-up helpers
are in `application/scripts/lib/mti_threshold_followup.R`.
Original `final` mode and its unresolved-calibration stopping rule remain.

Supply absolute preserved input directories:

```bash
export MTI_Q_BASELINE_RUN=/absolute/path/to/completed/frozen_comparison
export MTI_Q_SCREEN_RUN=/absolute/path/to/completed/numerical_screen
export MTI_Q_PARENT_RUN=/absolute/path/to/completed/final_20261022
make prepare-mti-q-grid
make test-mti-threshold-followup test-mti-q-grid test-mti-final-study
make preflight-mti-threshold-followup
make precision-mti-threshold-followup
make launch-mti-threshold-followup MTI_Q_FINAL_WORKERS=40
make health-mti-threshold-followup
```

The launcher requires clean committed source and installs this repository's
package from `git archive` under ignored cache. It records the exact source
commit, installed package/native hashes, configured exdqlm pin, R and tolerance
versions, parent evidence, seeds, and protected article inputs. It builds or
loads no external source checkout. A lock prevents duplicate stages; atomic
400-sample checkpoints preserve completed work. Terminal records require
review before an explicit manual resume.

Parent policy/completion hashes are pinned. Parent source, article inputs,
numerical package inputs, profile specification, and candidate-fitting
function bodies are checked. The parent remains unchanged in its original
worktree. A resource check limits fitting to available headroom, with at most
40 single-threaded workers. Bounded assessment retains the original independent
sample-count and generating-CDF checks and verifies checkpoint hashes between
passes.

## Conditional Validation and Reporting

If both settings pass, `combined/calibration/` contains the nine-setting
policy: seven exactly retained cutoffs and two fresh cutoffs, with evidence
sources and the error allocation recorded separately. Completed combined
policies are verified on resume rather than silently regenerated.

Only then does `combined/validation/` receive 1,000 new datasets per
distribution/design case: 72,000 datasets and 288,000 paired evaluations of
refined MTI, frozen MTI, Young--Mathew, and Wilks. The policy remains fixed.
Local review tables and figures are exported by the existing reporting code.
They show raw mean widths and empirical 2.5%--97.5% ranges within each case,
alongside attainment and binomial uncertainty. Widths are not pooled across
distributions or normalized by TCSP.

No manuscript, application, canonical figure, or earlier result is replaced.
Adoption remains conditional on scientific review of independent attainment,
paired widths, numerical diagnostics, and limitations. Plans and audits remain
under ignored `.codex_work/`; this document describes the reproducible study.
