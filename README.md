# Mean-Tilted Intervals: Short Tolerance Intervals

This repository contains the tolerance-interval manuscript and companion R
implementation for mean-preserving intervals (MPI), mean-tilted intervals (MTI),
and calibrated MTI-ECM tolerance intervals.

The manuscript in `main.tex` is now focused on fixed-content interval geometry,
the MPI and MTI targets, the calibrated MTI procedure, and iid tolerance
validation against Young--Mathew and Wilks. It includes fixed-target Gibbs and
ECM derivations. Broader endpoint-regression, basis-expansion, and dynamic development is in the
companion manuscript repository `AntonioAPDL/MTI-EXTENSIONS`.

Earlier versions used the Relaxed Quantile Regression (RQR) name and the
`rqr_*` API. The current manuscript uses MPI and MTI terminology. Legacy names
remain in the package and scripts where needed for reproducibility and
attribution to the original residual-product criterion of Pouplin et al.

## Repository Contents

- `main.tex`: main tolerance-interval article.
- `rqr-gibbs-supplement.tex`: population proofs, generalized-posterior propriety,
  complete Gibbs conditionals, ECM derivations, direct DP content-probability
  checks, feasibility thresholds, and validation tables.
- `refs.bib`: bibliography.
- `figures/` and `tables/`: figure and table generators plus tracked manuscript
  inputs.
- `application/`: R package, C++ code, simulation scripts, configurations, and
  tests.
- `docs/`: implementation notes, theory records, and validation records.
- `.codex_work/`: ignored local planning notes.

Large generated objects, fitted model outputs, logs, run directories, local
caches, and local literature PDFs are excluded from version control.

## Statistical Framing

MPI and MTI are loss-defined interval targets. Endpoint summaries from a
generalized posterior describe uncertainty for the specified interval-root
functional. They are not posterior predictive response draws.

Under proper Gaussian root priors, normal-exponential augmentation gives
conditionally conjugate Gaussian root and GIG scale updates for MPI and
fixed-tilt MTI. Gibbs sampling targets that fixed-target generalized posterior;
ECM uses the same augmentation for mode estimation. Fitted content and tilt are
selected separately. A completed fixed-target computation study compares
the two samplers and examines posterior uncertainty alongside finite ECM
estimates. Its findings are reported separately from the tolerance comparison.

MTI-ECM is the proposed tolerance procedure reported in the article. It searches
finite-iteration endpoint estimates under a generalized-Bayes criterion over fitted content and tilt,
then selects the shortest fitted interval meeting a direct Dirichlet-process
posterior content check. Its calibration rule is fixed before independent
repeated-sampling validation. The simulation and pharmaceutical results report
MTI-ECM, Young--Mathew, and Wilks. The simulation comparison uses the genuine
archived ECM study; its inexpensive comparators are recomputed on exactly the
same samples. Stopping flags are distinct from interval return and population
content attainment. See
[the reproducibility description](docs/reproducibility/mti_ecm_recovery_20261111.md).
The user has explicitly authorized the staged fixed-target Gibbs comparison.
Each launch still requires the exact committed article-source SHA; authorization
does not establish completed Gibbs results or permit automatic article replacement.

The scan reference (TCSP in the code) is retained for constructing the
initial fitted-content grid. It is
not a reported comparator. Scan algorithms, original simulation configurations,
and historical analysis outputs remain available for reproducibility. The
selected MTI interval has finite-panel empirical tolerance validation; a
distribution-free theorem for this selection rule remains open.

## Authorized Gibbs Comparison

`application/config/mti_gibbs_ecm_comparison_20261111.json` specifies four
dispersed chains with 2000 burn-in cycles and 4000 retained draws per chain for
the diagnostic and full stages. Diagnostics combine base cases with
deterministic maximum-q and maximum-absolute-tilt stress samples in each
simulation case. The measured diagnostic design contains 174 datasets:
82 base cases plus 92 additional deterministic stress cases, giving
696 independent chains, all now completed. Their assessment is summarized below.
Short timing benchmarks use eligible worker counts from 32, 40, and 50 before
selecting the diagnostic worker count; they are not convergence evidence.

Script 106 creates or reuses the detached exact-commit launch worktree under
`.codex_work/`, links its cache to the RQR-owned ignored cache, and calls the
existing `04_prepare_primary_runtime.R` builder. That builder archives, builds,
and installs the package under `application/cache/mti_gibbs_attested/<commit>/`
with a full v5 runtime attestation. The compatibility path
`application/cache/mti_gibbs_runtime_<commit>/library` is a symlink to that
library; `attestation_path.txt` beside it points to the attestation. The sampler
receives the attested source root, commit, and attestation, and accepted fits
must have `reproducibility_eligible = TRUE` as well as numerical eligibility.

The standalone 107 wrapper reuses the frozen worktree and shared cache, sets
thread limits before R starts, and runs a detached campaign controller.
From the clean committed checkout, set `commit` to the explicitly approved
40-character article-source SHA:

```bash
commit=CONFIRMED_40_CHARACTER_ARTICLE_COMMIT
make prepare-mti-gibbs-runtime
bash application/scripts/107_launch_mti_gibbs.sh --dry-run \
  --confirm-article-commit="$commit" --workers=50 --allow-full=true
# Launch only when ready; omitting --allow-full=true stops after diagnostics.
bash application/scripts/107_launch_mti_gibbs.sh \
  --confirm-article-commit="$commit" --workers=50 --allow-full=true
```

Full sampling requires a complete, passing diagnostic assessment with matching
source, settings, design, inputs, and runtime identity. Compact full-stage files
retain all ordered-endpoint draws and objective summaries; diagnostic files
also retain warmup roots, loss and objective traces, and the sampler's last
state. Validated completed chains are reused on resumption. External source
checkouts are not compiled, installed, or loaded, and no article inputs are
replaced automatically. See the
[launch, assessment, and resumption instructions](docs/reproducibility/mti_ecm_recovery_20261111.md#gibbs-study-authorized-staged-launch).

### Practical Fixed-Target Assessment

The original campaign at `44afea3` completed all 696 diagnostic chains with
zero execution failures or repairs. Twenty of 174 datasets met every original
diagnostic criterion, so the full stage did not start. The saved targets
include limiting and outside-range empirical tilts; proper Gaussian priors
keep their posteriors proper but can strongly influence outward endpoints.

The separate `mti_gibbs_practical_20261112.json` study preserves that evidence.
It reassesses saved draws, verifies exact Gaussian/GIG continuation, and
performs one bounded extension for borderline cases. An experimental
intercept-only marginal slice-within-Gibbs sampler checks the same root
posterior with latent scales integrated out. It is distinct from the original
conditionally Gaussian/GIG implementation and does not redefine MTI-ECM.
The balanced comparison has five replications per simulation cell and ten
application partitions: 370 datasets. Eighty-six additional diagnostic stress
datasets make 456 distinct datasets and 1,824 chains, without duplicating
overlapping cases. The smaller Gibbs study examines computation and
conditional posterior uncertainty; the 1,000-replication ECM validation is
unchanged. No Gibbs tolerance-confidence claim or article replacement follows
automatically. See the [practical assessment description](docs/reproducibility/mti_gibbs_practical_20261112.md).

The completed review verifies all 1,824 marginal chains, 696 original chains,
and 72 continuation segments. All 456 marginal-sampler datasets meet the
preferred mixing criteria (maximum rank-normalized R-hat 1.0032, minimum bulk
ESS 1,781). The extra response-unit precision requirement is met for every
primary summary in 125 datasets; the others retain their Monte Carlo errors.
Matched original chains with adequate diagnostics agree within Monte Carlo
uncertainty. Posterior widths can nevertheless be much larger than ECM
widths near empirical limiting tilts. Independent objective minimization also
identifies appreciable gaps in 96 finite ECM fits, 94 of them outside the
empirical tilt limits. The article now explains the marginal sampler, these
limits, and the distinction between optimization and posterior dispersion.

See the [completed numerical review](docs/reproducibility/mti_gibbs_review_20261112.md).
`make review-mti-gibbs` reconstructs the assessment from frozen local chains;
`make verify-mti-gibbs-review` verifies the tracked compact evidence without
those chains. No full Gibbs tolerance campaign or new calibration is started
by this analysis.

## Build and Checks

### Full Fixed-Target Gibbs Comparison

The full comparison now has a separate marginal slice-within-Gibbs controller.
It preserves archived q/delta and priors, verifies the completed diagnostic
study, replays 12 integration examples, and automatically proceeds to all
75,004 datasets (300,016 chains) only after its checks pass. Compact records
retain all endpoint draws and exact continuation states. One bounded
continuation is available for datasets failing the practical numerical screen.
Posterior-mean intervals are evaluated as a separately specified summary;
they do not inherit the ECM procedure's tolerance calibration.

Use `make test-mti-gibbs-full` and `make preflight-mti-gibbs-full` before the
explicitly authorized detached launch. The [full comparison instructions](docs/reproducibility/mti_gibbs_full_comparison_20261111.md)
describe launch, health checks, graceful stop, exact-commit resumption, storage,
and generated reports. These calculations do not automatically replace article
results. The original Gaussian/GIG campaign remains reproducible and unchanged.

`make review-mti-gibbs-full-report GIBBS_OUTPUT_DIR=/absolute/campaign/directory`
checks completed report identities, diagnostic summaries, paired comparisons,
and per-design attainment and width summaries. During sampling it returns
`pending` without reading unfinished result files. This read-only review does
not reassess chains, restart sampling, or change article results. Its focused
tests are available through `make test-mti-gibbs-full-report-review`.

```bash
make test-theory-figures
make test-theory-tables
make test-manuscript-language
make pdf
make supplement
```

Package checks are available through the `application/` targets in the
`Makefile`. Heavy simulations should record the Git commit and write their
outputs under ignored simulation-output directories.

## Pharmaceutical Application

The current application uses
`application/config/pharma_tolerance_application_mti_only_20261020.json`.
MTI-ECM is the sole proposed procedure; Young--Mathew and Wilks are external
benchmarks. It uses the adopted simulation policy without application tuning.
Both responses share 1000
random partitions, two chronological cases, 250 production-month partitions,
and 250 API-batch partitions. All partitions use 100 training and 87 held-out
batches. Grouped partitions keep whole groups together.

The complete, portable reporting inputs are tracked under
`figures/data/pharma_mti_application_20261020/`. Rebuild the report without
fitting any models:

```bash
make pharma-mti-application-report
make test-pharma-mti-application
make test-manuscript-language
make all-pdf
```

For a new analysis, use fresh output directories and run these stages in order:

```bash
make pharma-mti-application-runtime
make pharma-mti-application-data
make pharma-mti-application-preflight
make pharma-mti-application-confirmatory PHARMA_MTI_WORKERS=32 PHARMA_MTI_RUN_DIR=application/outputs/pharma_mti_application_20261020/confirmatory_new
make pharma-mti-application-health PHARMA_MTI_RUN_DIR=application/outputs/pharma_mti_application_20261020/confirmatory_new
```

The runtime target installs this repository's package in its ignored local
library. It does not install or load the external exdqlm checkout. Raw source
data are downloaded only when absent and must pass byte-count, MD5, and SHA256
checks. Method seeds do not depend on worker scheduling; each worker uses one
BLAS/OpenMP thread. Never replace the tracked canonical directory silently.
The explicit `pharma-mti-application-publish` target requires a verified,
complete run and an absent destination.

Held-out empirical content describes these finite, overlapping partitions; it
does not estimate tolerance confidence from independent replications. ECM
iterations, stopping flags, and stationarity are reported explicitly.
The separate convex-profile application remains preserved under
`figures/data/pharma_mti_profile_application_20261025/`; its results are not
relabeled as ECM. The source and campaign sensitivity summaries for the current
ECM article use `application/config/pharma_source_structure_ecm_20261111.json`.
Historical `pharma-application-*` targets retain the previous analysis.

## ECM Numerical Diagnostics

The separate diagnostic study compares the frozen pharmaceutical fits with
longer fixed-target ECM runs and an independent piecewise-quadratic objective
reference. It does not replace article results or retune calibration.
Portable evidence covers all 3004 saved MTI intervals and 28 prespecified
replay cases, including their complete candidate menus and iteration traces.

```bash
make pharma-ecm-audit-report
make test-pharma-ecm-audit
```

These commands reuse tracked evidence without fitting models. The generated
29-page PDF is under
`application/outputs/pharma_ecm_numerical_audit_20261020/review/`.
For new diagnostics, prepare the local library with
`make pharma-mti-application-runtime`, then use
`make pharma-ecm-audit-preflight`, `make pharma-ecm-audit-smoke`, and
`make pharma-ecm-audit-run`. Each run requires a fresh output directory.

## Profiled Intercept Revision

The opt-in `rqr_mti_intercept_fit()` minimizes the fixed-content, fixed-tilt
intercept criterion using a convex midpoint profile, with R and C++
implementations and an objective lower-bound check. It is restricted to a
fixed learning rate and equal positive Gaussian root precisions with a common
fixed prior mean. `rqr_mti_fractional_window()` constructs a mass-consistent
empirical tilt; `rqr_mti_dp_profile()` searches the specified fitted-content
and tilt menu and applies a separate direct DP posterior content check.

These functions preserve the historical ECM API and its defaults. The separate
profile study and its results remain available as a numerical reference;
the current article reports the archived ECM results. A DP posterior probability alone does not
establish a frequentist tolerance guarantee. The separate revision study
first checks numerical accuracy and then develops and assesses a new
calibration on independent data. Fresh validation starts only after the
prespecified checks pass. No manuscript replacement is automatic.

```bash
make test-mti-profile
make mti-profile-runtime
make mti-profile-preflight
make launch-mti-profile-revision MTI_PROFILE_WORKERS=24
make health-mti-profile-revision
```

Runtime preparation and launching require a clean committed checkout. Output,
checkpoints, logs, and trace PDFs are local-only. See
[the numerical specification](docs/implementation_notes/mti_intercept_profile_20261021.md)
for the method, study stages, interpretation, and resumption commands.

## Independent Revision Assessment

The separate review reports fitting progress and statistical decisions without
changing the active study. It verifies checkpoint identities, numerical
diagnostics, independently reconstructed threshold decisions, and eventual
paired validation results. Inconclusive calibration also produces a complete
report; it cannot silently authorize validation or article replacement.

```bash
make test-mti-profile-review
make health-mti-profile-review MTI_PROFILE_RUN_DIR=/absolute/path/to/frozen/run
make review-mti-profile-revision MTI_PROFILE_RUN_DIR=/absolute/path/to/frozen/run
```

Use `make queue-mti-profile-review` with the same explicit run argument to
produce one report after a still-running study exits. This job waits on the
study lock without polling or fitting. Reports, CSVs, and the diagnostic PDF
remain under ignored `application/outputs/`. See
[the assessment specification](docs/implementation_notes/mti_profile_independent_assessment_20261021.md)
for statistical definitions, uncertainty, integrity checks, and precision
planning. Manuscript inputs are unchanged until a separate scientific decision.
See [the numerical diagnostic record](docs/implementation_notes/pharma_ecm_numerical_diagnostics_20261020.md)
for assumptions, numerical checks, findings, and limits.

## Fixed-Policy Independent Evaluation

The original revision calibration passed six of nine design cells. A separate
empirical comparison preserves those six settings and freezes three explicitly
exploratory settings from the completed calibration. It uses 72000 fresh
datasets shared by revised MTI, historical MTI-ECM, Young--Mathew, and Wilks.
The original stopping decision and all manuscript/application inputs remain
unchanged. No statistical or numerical result is automatically promoted.

Set `MTI_FIXED_CALIBRATION_RUN` and `MTI_FIXED_CALIBRATION_REVIEW` to the explicit
upstream directories, then use `make preflight-mti-fixed-policy`,
`make launch-mti-fixed-policy MTI_FIXED_WORKERS=16`, and
`make health-mti-fixed-policy`. Completed checkpoints can be resumed with the
same policy and source identity. Summaries and a diagnostic PDF are produced
once when fitting ends, without continuous monitoring. See
[the evaluation specification](docs/implementation_notes/mti_fixed_policy_evaluation_20261021.md)
for selection, seeds, uncertainty, tests, reproduction, and adoption criteria.

## Content-Grid Refinement

The optional `rqr_mti_refine_content_grid()` function retains the original fitted
contents and adds targets according to the observation count, learning rate,
and root prior precision. Existing fitting defaults and manuscript results are
unchanged. A separate study compares this finite-grid refinement with the
frozen profiled MTI method, Young--Mathew, and Wilks on paired samples.

Use `make prepare-mti-q-grid` and `make test-mti-q-grid` from a clean committed
checkout. With `MTI_Q_BASELINE_RUN` set to the original comparison directory,
`make preflight-mti-q-grid` checks the isolated specification and
`make precision-mti-q-grid` writes hypothetical calibration power curves.
`make queue-mti-q-grid-screen` waits for the original study to finish before
running 72 numerical test samples with four single-threaded workers. This
screen provides no tolerance-performance evidence. New calibration and
validation require separate commands and complete upstream evidence; neither
large runs nor manuscript replacement is automatic. See
[the refinement specification](docs/implementation_notes/mti_q_grid_refinement_20261022.md)
for the derivation, data roles, commands, and statistical limitations.

## Final Calibration and Independent Comparison

The final, fixed-budget study uses 24,000 calibration assessments and 32
independent threshold-development samples per distribution/design case.
The completed 72-sample numerical screen is reused after checking its source,
configuration, numerical package, and recorded results. All nine design
settings must pass the simultaneous binomial calibration requirement before
the frozen rule is evaluated on 1,000 fresh samples per case. An unresolved
setting stops the study; there is no automatic budget increase or resampling.

Set `MTI_Q_BASELINE_RUN` and `MTI_Q_SCREEN_RUN` to the preserved comparison and
screen directories. From a clean committed checkout, run:

```bash
make prepare-mti-q-grid
make test-mti-final-study
make preflight-mti-final-study
make precision-mti-final-study
make launch-mti-final-study MTI_Q_FINAL_WORKERS=40
make health-mti-final-study
```

The focused threshold follow-up is specified in
`application/config/mti_threshold_followup_20261024.json` and documented in
`docs/implementation_notes/mti_threshold_followup_20261024.md`. It preserves
the completed study, tightens the simultaneous bounds for seven retained
policies, and freshly assesses only the two unresolved settings with a finer
development-only probability menu. Set `MTI_Q_PARENT_RUN` in addition to the
preserved baseline and screen paths, then use
`make preflight-mti-threshold-followup`, `make precision-mti-threshold-followup`,
and `make launch-mti-threshold-followup`. The launcher uses at most 40
single-threaded workers and starts the full independent comparison only if
both settings pass. `make health-mti-threshold-followup` reports progress.
Neither run replaces the application or manuscript automatically.

The launch checks available resources, uses one thread per worker, and retains
every completed batch. Calibration assessment reads one batch at a time.
Independent comparison tables and figures are written only to the new run's
`review/` directory. Existing article results, application results, and both
manuscripts remain unchanged until a separate scientific review. See
[the final study specification](docs/implementation_notes/mti_final_study_20261022.md)
for the fixed design, decision rules, reproducibility records, and limitations.
