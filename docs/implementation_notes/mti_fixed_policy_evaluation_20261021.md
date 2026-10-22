# Independent Evaluation of the Fixed MTI Selection Rule

## Purpose

The numerical revision has been checked against independent objective
calculations. Its calibration completed 74304 development and assessment
datasets without numerical failures. Six of nine design cells met the
declared simultaneous lower-bound criterion. The cells `(50,0.90)`,
`(100,0.95)`, and `(500,0.99)`, all with tolerance confidence 0.95, remain
inconclusive. Completion of fitting and satisfaction of the statistical
criterion are different conclusions.

This experiment implements a separate empirical comparison after the user
approved that change of protocol. It leaves the original calibration and its
validation stop unchanged. It does not retrospectively declare the three
cells successful. No further model variants, application-specific tuning,
fallback interval, or analytical boundary extension are introduced.

## Fixed Selection Rule

The versioned specification is
`application/config/mti_profile_fixed_policy_evaluation_20261021.json`.
The nine thresholds are in `mti_profile_fixed_policy_20261021.csv`, with
compact selection evidence in `mti_profile_fixed_policy_thresholds_20261021.csv`
and source hashes in `mti_profile_fixed_policy_provenance_20261021.json`.

For the six passing cells, preserve the original selected threshold exactly.
For each unresolved cell, consider only the already assessed thresholds that
returned an interval on every calibration sample in every family. Select the
threshold with the largest minimum family-specific lower bound; choose the
smallest threshold if this maximum is tied. The same cell-specific threshold
is used for every family. The three choices remain exploratory until assessed
on independent samples. Complete observed returns do not guarantee returns
on future samples.

This rule uses no guessed threshold and requires no new calibration fits.
The empirical success values used for calibration are evaluated under the
known generating distributions. On a validation sample, interval selection
uses the observations, fixed menu, loss, prior, and DP content probability;
the true CDF is used only for evaluation. Neither the true validation content
nor the distribution label is used to choose a fitted interval.

The threshold controls a posterior content-probability condition. It is not
itself frequentist tolerance confidence. All earlier assumptions, candidate
menus, learning rate, priors, tilt coordinates, C++ profile calculations, and
objective-gap tolerance remain fixed. The historical ECM comparator retains
its original policy and algorithm, with its selected convergence flag,
iteration count, and stationarity recorded without reclassification.
Nonfinite historical candidates and candidates without the original convergence
flag are counted separately; a strict convergence flag is not silently treated
as either a numerical exception or a proof of accurate minimization.

## Sampling Design

The study retains the eight distributions and nine design cells in the
frozen validation specification. The design cells are:

| Sample Size | Requested Contents | Tolerance Confidence |
| ---: | --- | ---: |
| 50 | 0.90 | 0.95 |
| 100 | 0.90, 0.95 | 0.95 |
| 500 | 0.90, 0.95, 0.99 | 0.95 |
| 1000 | 0.90, 0.95, 0.99 | 0.95 |

Each of the 72 distribution/design cases has 1000 fresh iid samples. The
same sample is used by revised profiled MTI, historical MTI-ECM,
Young--Mathew, and Wilks: 72000 independent datasets and 288000 method
evaluations. A task generates its sample once and evaluates the four methods
sequentially. Tasks, rather than method blocks using independently generated
data, are distributed across workers. This preserves pairing and avoids
duplicate sampling.

The previously declared independent-validation seed schedule was unused
because the original runner stopped before validation. That schedule is
retained; checks require that its seeds are unique and disjoint from all
development and calibration seeds. The integration test uses a separate
test-only schedule. Every nonreturn and computational failure remains
recorded. No replacement samples are drawn.

Small-sample boundary cases are retained as limitations to be examined,
rather than removed after observing performance. Observed finite intervals
and accurate objective minimization do not establish a tolerance theorem.

## Quantities and Interpretation

For each sample, attainment is the event that the returned interval has true
population content at least the requested `c`. A nonreturn contributes a
failure. Its probability is the frequentist quantity compared with `p`.
Exact pointwise binomial intervals quantify Monte Carlo uncertainty; separate
two-sided simultaneous bounds cover the primary method over the 72 cases.
The confidence level comes from the existing study specification. The old
threshold-search factor is unnecessary for this fresh assessment because
the policy is fixed before new sampling.

Primary-case classifications retain the independently tested distinctions:
strong panel evidence, evidence of shortfall, inconclusive evidence, numerical
issues, and observed nonreturns. An interval containing `p` is inconclusive;
a lower bound below `p` alone is not evidence of underattainment. Simultaneous
evidence on this finite panel is not distribution-free validity.

Raw widths are summarized separately by distribution, sample size, content,
confidence, and method. Report means and empirical 2.5%/97.5% quantiles, with
returned counts. Empirical width ranges describe sample-to-sample variation,
not uncertainty in the mean. Paired comparisons retain the jointly returned
denominator and report width changes, Monte Carlo standard errors, and
discordant attainment counts. There is no pooled cross-distribution raw
width, relative-width column, guessed noninferiority margin, or post hoc
acceptance threshold.

The study reports all cases regardless of the outcome. It does not add
replications until a favorable bound appears or choose new thresholds using
validation results. A numerical improvement may be useful without reducing
width; a statistical width advantage needs evidence at comparable attainment.

## Integrity and Execution

The launcher requires clean committed source. It reuses the exact archived
rqrgibbs 0.1.0.9037 runtime from computation commit
`5798cca1e322f5757b1c0bb5f65b56f823f65d4e`, with verified R bytecode and native
library hashes, R version, tolerance 3.0.0, and the configured external exdqlm
pin. It does not compile or load any external source checkout.

The independent audit at source commit
`565b980ee63e477600efeb775795eef1bf3725c0` supplies the calibration evidence.
Its receipt and input hashes, original completion status, unused validation
directory, and complete policy reconstruction are checked before launch.
Source, policy, runtime, and task identities protect checkpoint resumption.
Historical manuscript, table, figure, application, and policy hashes are
checked before and after execution. New output must be beneath the dedicated
ignored output root and disjoint from both upstream directories, including
through symlinks.

Use at most 40 single-threaded workers, chosen after checking available host
capacity. Other projects must not be stopped or modified. One detached
process performs fitting, integrity checks, summaries, and PDF generation.
It records a PID, log, terminal receipt, and batch progress. It performs no
continuous monitoring and no automatic article replacement.

## Commands

Set explicit upstream directories and choose an absent output directory:

```bash
export MTI_FIXED_CALIBRATION_RUN=/absolute/path/to/frozen/calibration/run
export MTI_FIXED_CALIBRATION_REVIEW=/absolute/path/to/final_verified_v2
export MTI_FIXED_RUN_DIR=application/outputs/mti_fixed_policy_evaluation_20261021/independent_20261021
export MTI_FIXED_WORKERS=16
make test-mti-fixed-policy
make test-mti-profile-review
make preflight-mti-fixed-policy
make launch-mti-fixed-policy
make health-mti-fixed-policy
```

The installed-runtime integration test uses the original archived R library:

```bash
R_LIBS=/absolute/path/to/pinned/library make test-mti-fixed-policy-runtime
```

It exercises all nine design cells under normal and exponential laws, all
four methods, checkpoint resumption, CDF checks, paired summaries, and PDF
creation using test-only seeds. It does not inspect the independent study.

After fitting, the same detached process creates `REPORT.md`,
`mti_profile_assessment.pdf`, per-family summaries, paired comparisons,
historical ECM diagnostics, checkpoint hashes, and `evaluation.json`. The
run manifest records the exact source commit, frozen policy hash, package
identity, external pin, seeds through the source configuration, and workers.
Use `make report-mti-fixed-policy` only to reconstruct reporting for the same
unchanged run identity. Reporting cannot retune, refit, or promote results.

Following interruption, preserve the terminal receipt and review the error.
The explicit `resume` launcher mode requires a reason, an unsuccessful attempt,
the same committed identity, and no active launcher. It reuses completed
checkpoints. It cannot fix stored computational errors by resampling or using
a different algorithm. A source change requires a new experiment directory.

## Application and Article Decision

Keep the existing pharmaceutical batch application. Its repeated partitions
illustrate placement and finite-data stability, rather than independent
frequentist tolerance confidence. It is not replaced by a more favorable
dataset. If the independent study supports adopting the revised method,
recompute its application fits using the same responses and paired partition
definitions; retain Young--Mathew and Wilks and compare the numerical flags.

Before article replacement, review all cases, nonreturns, numerical accuracy,
attainment uncertainty, and paired width differences. Any later adoption must
state which rule was evaluated and retain the unresolved original calibration
in the reproduction record. If there is no useful gain, report that conclusion
and close the revision rather than starting another broad tuning campaign.
