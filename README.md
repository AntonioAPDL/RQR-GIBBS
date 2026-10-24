# Mean-Tilted Intervals: Short Tolerance Intervals

This repository contains the tolerance-interval manuscript and companion R
implementation for mean-preserving intervals (MPI), mean-tilted intervals (MTI),
and calibrated MTI-ECM tolerance intervals.

The manuscript in `main.tex` is now focused on fixed-content interval geometry,
the MPI and MTI targets, the calibrated MTI-ECM procedure, and iid tolerance
validation against Young--Mathew and Wilks. The broader endpoint-regression,
basis-expansion, Gibbs, ECM, and dynamic endpoint development is split into the
companion manuscript repository `AntonioAPDL/MTI-EXTENSIONS`.

Earlier versions used the Relaxed Quantile Regression (RQR) name and the
`rqr_*` API. The current manuscript uses MPI and MTI terminology. Legacy names
remain in the package and scripts where needed for reproducibility and
attribution to the original residual-product criterion of Pouplin et al.

## Repository Contents

- `main.tex`: main tolerance-interval article.
- `rqr-gibbs-supplement.tex`: proofs, fixed-target MTI-ECM details, direct DP
  content-probability checks, feasibility thresholds, and validation tables.
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

MTI-ECM is the proposed tolerance procedure reported in the article. It profiles
fixed-target endpoint estimates under a generalized-Bayes criterion over fitted content and tilt,
then selects the shortest fitted interval meeting a direct Dirichlet-process
posterior content check. Its calibration rule is fixed before independent
repeated-sampling validation. The simulation and pharmaceutical results report
MTI-ECM, Young--Mathew, and Wilks.

The scan reference (TCSP in the code) is retained for constructing the
fitted-content grid and for the calibration width-stability criterion. It is
not a reported comparator. Scan algorithms, original simulation configurations,
and historical analysis outputs remain available for reproducibility. The
selected MTI-ECM interval has empirical tolerance validation; a
distribution-free theorem for this selection rule remains open.

## Build and Checks

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

The revised application uses
`application/config/pharma_tolerance_application_mti_only_20261020.json`.
MTI-ECM is the sole proposed procedure; Young--Mathew and Wilks are external
benchmarks. The frozen calibration is unchanged. Both responses share 1000
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
make pharma-mti-application-smoke PHARMA_MTI_RUN_DIR=application/outputs/pharma_mti_application_20261020/smoke_new
make pharma-mti-application-confirmatory PHARMA_MTI_WORKERS=40 PHARMA_MTI_RUN_DIR=application/outputs/pharma_mti_application_20261020/confirmatory_new
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
does not estimate tolerance confidence from independent replications. Selected
ECM convergence flags are retained and reported, including conditional
comparisons on the same convergence-flagged partitions for all methods.
See [the implementation record](docs/implementation_notes/pharma_mti_application_revision_20261020.md)
for provenance, diagnostics, endpoint-precision recovery, and all commands.
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

These functions do not replace the historical ECM algorithm, its defaults,
calibration, or article results. A DP posterior probability alone does not
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
