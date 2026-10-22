# Sample-Size-Aware MTI Content Grid

## Statistical Definition

The fitted content `q` indexes the MTI loss; requested population content is
`c`, and tolerance confidence is `p`. These are distinct quantities. At fixed
`q`, tilt, loss learning rate `omega`, and common Gaussian root precision
`lambda`, the intercept solver uses effective observation mass

```text
s = n*q - lambda/omega.
```

For positive `s`, its radius has order-statistic rank `ceiling(s)`. At an
integer mass the solver selects the lower minimizing radius of the plateau.
Thus rank `k` corresponds to `(k-1,k]`. The optional exported function
`rqr_mti_refine_content_grid()` preserves every supplied `q` exactly and adds
`(k-1/2+lambda/omega)/n` for each unrepresented positive-mass cell inside the
original range. It neither widens the search range nor changes the tilt menu,
loss, prior, posterior-content calculation, or numerical solver.

For example, with `n=50`, `c=.90`, `lambda=1e-4`, and `omega=1`, the grid
`c(.93,.995)` gains `.950002` and `.970002`. These are derived from the
effective-mass cells. They are not guessed coverage levels. The same rule
applies to other sample sizes, contents, and prior-to-rate ratios.

The function returns the original and enlarged grids and a record of the
represented cells. An optional `max_added_points` rejects an excessive search
before allocating it; it never returns a silently truncated grid. Collapsed
nonpositive masses do not generate extra targets. Existing package defaults
are unchanged. An exhaustive set of radius ranks does not establish a global
optimum over continuous `q` and tilt: roots and admissible tilts can vary within
these cells. No tolerance theorem follows merely from refining the grid.

## Comparison and Data Roles

The specification is
`application/config/mti_q_grid_refinement_20261022.json`. It retains the eight
distributions and nine `(n,c,p)` settings of the existing fixed-policy study.
All new data roles have disjoint seed schedules, also separate from the old
development, calibration, and independent comparison schedules.

| Stage | Draws Per Distribution/Setting | Total Datasets | Purpose |
| --- | ---: | ---: | --- |
| Numerical screen | 1 | 72 | Check numerical integrity and search changes |
| Threshold development | 32 | 2304 | Construct candidate posterior thresholds |
| Calibration assessment | 1000 | 72000 | Assess the fixed threshold menu |
| Independent validation | 1000 | 72000 | Compare frozen methods on new data |

The numerical screen uses the old posterior threshold solely for paired
search diagnosis. It must have no numerical failures or refined-method
nonreturns, preserve the nested-search width inequality, and show a
beyond-roundoff change before new calibration can start. It estimates neither
tolerance confidence nor general comparative performance.

New posterior thresholds are derived from quantiles of the separate
development probabilities together with `p`. No historical posterior
threshold is inherited by the refined method. For each `(n,c,p)` setting,
calibration selects the smallest threshold whose assessment has complete
observed returns and one-sided exact-binomial lower bounds at least `p` for
every distribution. Bonferroni allocation covers all 72 distribution/settings
and at most `threshold_quantile_count+1` thresholds per setting. Development
and assessment draws are independent. This supplies finite-panel calibration
evidence, not a distribution-free claim for arbitrary populations.

The stated budget may be insufficient near `p`. Inconclusive settings remain
unresolved and block validation; there is no automatic increase in draws,
relaxation of the requirement, or replacement of difficult samples.
`precision_reference.csv` gives exact binomial probabilities of passing over
hypothetical attainment levels from each `p` to one and several replication
budgets. These are per-comparison power curves, not joint power or assumed
method attainment. They do not select a new budget or a desired attainment.
Any budget change needs a prospectively revised specification and new output
directory, before inspecting the associated assessment outcomes.

Independent validation requires all nine newly calibrated settings, a frozen
policy hash, and unchanged source. It compares refined-grid MTI, frozen-grid
profiled MTI, Young--Mathew, and Wilks on the same sample in each replication.
The two profiled MTI methods share a single union fit; extracting the original
targets reproduces the original candidate calculations and selection order.
The expensive historical iterative ECM is not fitted again. A failed refined
search is recorded without substituting another method.

Reports retain nonreturns in the unconditional content-attainment denominator.
Population content is evaluated using the generating distribution's CDF.
Raw widths, mean widths, and empirical 2.5% and 97.5% width quantiles are
reported separately by distribution and setting. Paired comparisons and
binomial uncertainty distinguish a numerical search improvement from a
supported statistical advantage. Validation completion alone is not an
adoption decision. The original application and article remain unchanged.

## Reproduction

Work in a separate clean committed checkout. The original comparison must
retain its own worktree, package library, policy, and outputs.

```bash
export MTI_Q_BASELINE_RUN=/absolute/path/to/original/independent_20261021
export MTI_Q_RUN_DIR=application/outputs/mti_q_grid_refinement_20261022/prepared_20261022
make prepare-mti-q-grid
make test-mti-q-grid
make preflight-mti-q-grid
make precision-mti-q-grid
make queue-mti-q-grid-screen MTI_Q_WORKERS=4
make health-mti-q-grid
```

Preparation materializes this repository's committed package with
`git archive` under ignored `application/cache/`, builds and installs version
`0.1.0.9038` there, and records package and native-library hashes. Study
identities record the source commit, R and benchmark versions, configured
exdqlm pin, baseline policy hash, inputs, and data roles. No exdqlm source
checkout is built, installed, or loaded.

The queued screen waits once on the original launch lock, without polling.
It starts only after the original comparison has a successful exit receipt
and complete evaluation report. Four single-threaded workers limit contention
with other users. No new large calibration runs concurrently with that study.
The launcher supports up to 40 workers when resources are available.

After reviewing the original comparison, screen, and precision curves, an
explicit new calibration command is:

```bash
bash application/scripts/100_manage_mti_q_grid_study.sh calibration \
  "$MTI_Q_BASELINE_RUN" "$MTI_Q_RUN_DIR" 4 \
  --acknowledge-new-calibration --acknowledge-precision \
  --rationale="Record the scientific reason and reviewed precision here."
```

Only if that calibration resolves every setting may validation be requested:

```bash
bash application/scripts/100_manage_mti_q_grid_study.sh validation \
  "$MTI_Q_BASELINE_RUN" "$MTI_Q_RUN_DIR" 4
```

Each detached stage writes `launch.log`, `launcher.pid`, a stage-specific exit
receipt, checkpoints, and `completion.json` plus `REPORT.md`. The screen also
writes paired `results.csv`; calibration writes `policy.csv` and
`threshold_assessment.csv`; validation adds family summaries, paired
comparisons, and `mti_profile_assessment.pdf`. All are ignored local outputs.
A terminal receipt prevents accidental duplicate launches. If a stage was
interrupted, first inspect its receipt and confirm no process holds the lock.
Preserve the receipt under a distinct archival name before explicitly invoking
that same stage again. The same committed identity and task schedule reuse
completed checkpoints; changes to source, configuration, or validation policy
require a new directory. Failed samples must not be resampled.

## Verification and Limits

Tests cover effective-mass boundaries, prior/rate adjustment, preservation,
idempotence, invalid inputs and allocation limits; disjoint data roles;
baseline completion; independent threshold construction and exact binomial
bounds; unresolved or malformed policies; paired candidate identity; numerical
failure handling; report generation; and checkpoint resumption.

During implementation, a malformed-policy regression test exposed a missing
required-column check before validation. The check was added and tested. A
provisional validation seed base `822610220` was retired because the temporary
test entered that schedule before the check was fixed. Production validation
uses `922610220`; the retired schedule is explicitly excluded from all new
data roles. Current integration tests use separate test-only seeds. No old
study data, settings, or results changed.

Method adoption requires reviewing attainment uncertainty and paired widths
across every distribution/setting, without a guessed acceptable shortfall or
noninferiority margin. Better loss minimization or more targets need not
improve tolerance performance. A decision to update the existing application,
tables, figures, or manuscript must be made separately after the independent
evidence is complete.
