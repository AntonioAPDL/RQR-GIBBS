# MTI-INTERVALS

Research implementation of mean-preserving interval (MPI) and mean-tilted
interval (MTI) losses, generalized-Bayesian root regression, scan-calibrated
tolerance actions, and Bayesian nonparametric shortest-interval uncertainty.

Earlier versions used the Relaxed Quantile Regression (RQR) name and the
`rqr_*` API. The canonical terminology is now MPI/MTI: MPI is the zero-tilt
member of the MTI family. Legacy wrappers and historical filenames remain
temporarily for reproducibility, and the package is still installed as
`rqrgibbs` during this staged migration. RQR is retained for attribution to
Pouplin et al., published variant names, legacy APIs, and frozen evidence.

The project studies interval-root functionals: two regression roots are learned
directly under a content-targeted loss rather than obtained by inverting a
response likelihood. For tolerance UQ, the authoritative Bayesian layer now
models the full response distribution with a direct Dirichlet-process posterior
or a smooth truncated Gaussian DPM. The hybrid Bayesian-scan action keeps the
scan-calibrated retained count fixed and selects the shortest order-statistic
interval that also satisfies a posterior content-probability constraint. The
older TCSP-MTI MCMC/ECM attachment remains available as fixed-target
plug-in UQ after the empirical shortest window has selected content and tilt.
A separate split exact-spacing TCSP path uses an independent pilot for
placement and an independent main sample for a fixed Beta-calibrated
order-statistic spacing. The manuscript also develops the
population loss geometry, pseudo-asymmetric-Laplace augmentation,
fixed-design Gibbs samplers, regularized regression extensions, frozen-feature
DESN readouts, and dynamic linear root models.

## Repository contents

- `main.tex` is the main manuscript.
- `rqr-gibbs-supplement.tex` contains derivations and reproducibility details.
- `refs.bib` stores the bibliography.
- `figures/` and `tables/` contain source generators and tracked main-text
  outputs.
- `application/` contains the R package, C++ FFBS kernel, scripts, simulation
  configurations, and tests.
- `docs/` contains implementation notes, validation records, and design
  contracts.
- `literature/` contains tracked placeholders only; local PDFs are ignored.

Large generated objects, fitted models, logs, run directories, local caches,
and local literature PDFs are intentionally excluded from version control.

## Statistical scope

MPI/MTI regression is treated as a loss-based generalized-Bayes update.
Interval-root draws are summaries of a generalized posterior over interval
functionals; they are not posterior-predictive response draws. In the TCSP
extension, tolerance
confidence comes from the external scan-calibrated retained-count action, not
from posterior credibility, the learning rate, or an MTI response likelihood.
The direct-DP and Gaussian-DPM engines are ordinary response-distribution
models for \(F\); their posterior content probabilities are separate from MTI
generalized posteriors.

The two root blocks are exchangeable under the symmetric MPI and MTI targets.
Raw fields such as `samp.beta_root1` and
`samp.beta_root2` are therefore MCMC labels, not lower- and upper-endpoint
coefficient estimates after label mixing. Interval endpoints are always
available by pointwise sorting. Coefficient-level lower/upper summaries are
reported only when a post-processing audit finds one globally separated
lower/upper chart on a declared design; otherwise the package fails closed and
keeps only raw-label coefficients plus ordered endpoint functionals. Dynamic
MTI-DLM paths follow the same whole-root principle: swaps exchange complete
root trajectories, not individual times.

The implemented MPI paths currently include:

1. fixed-design ridge regression;
2. regularized-horseshoe regression through the Nishimura--Suchard
   augmentation;
3. frozen-feature DESN readouts as a fixed-design specialization; and
4. dynamic linear root models with fixed, frozen-discount-template, and
   component-scale evolution modes.

Mean-tilt and Cornish--Fisher calculations in the manuscript are population
geometry and initialization diagnostics. The main article's CF figure and
cross-law population table deliberately show both near-Normal accuracy and
breakdown under stronger skewness or support-boundary geometry; they do not
enter the exact-oracle fits. The package now exposes a bounded
fixed-tilt MCMC path for fixed-rate ridge readouts: fixed-design regression,
frozen-feature DESN readouts through the same fixed-design kernel, and MTI-DLM
models with fixed or pre-frozen discount-template evolution. Nonzero tilt is
not yet implemented for learned inverse-loss scales, RHS-NS priors,
component-scale/adaptive dynamic evolution, VB/CAVI, or data-driven tilt
selection.

The deterministic fixed-target ECM layer exposes canonical `mti_ecm_fit()` and
`mti_ecm_path()` wrappers, with legacy `rqr_ecm_fit()` and `rqr_ecm_path()`
names retained. ECM uses exact inverse latent-scale moments and conditional
Gaussian root solves to compute a mode of the fixed loss-defined target. It is
not an EM algorithm for a response likelihood, does not return posterior
draws, and does not create tolerance validity.

The TCSP scan layer currently exposes canonical `tcsp_*` helpers for
adaptive Clopper-Pearson and conservative scan calibration, canonical
closed-window selection, shortest-path tilt metadata, path continuation
diagnostics, and action-contract validation. `tcsp_plugin_mti_fit()` names the
fixed-target MTI
plug-in path. `tcsp_hybrid_bayes_fit()` is the authoritative
full-distribution Bayesian UQ interface. The direct-DP helpers are `dp_*`; the
smooth DPM helpers are `dpm_*`; and `bayesian_bootstrap_shortest_draws()` is a
diagnostic comparator. Legacy `rqr_*` names remain available for this
migration cycle. The split exact-spacing layer adds `tcsp_exact_spacing_gap()`
and `tcsp_split_exact_fit()` for continuous iid univariate pilot/main-split
actions. Exact scan recursion, Tier-1 finite-sample proof promotion,
posterior endpoint-coverage transfer, and regression-family tolerance theorems
remain pending; see `docs/theory/tcsp_mti_proof_ledger_20260811.md`,
`docs/theory/full_bayes_shortest_uq_theory_ledger_20260812.md`, and
`docs/theory/mti_ecm_monotonicity_and_scope_20260812.md`.

## External reference implementation

The pinned exdqlm branch is used only as a read-only reference for legacy
RQR-DESN and RHS-NS compatibility:

```text
repo:    https://github.com/AntonioAPDL/exdqlm
branch:  feature/rqr-desn-readout-20260716
commit:  dffb71ee70b597d6a716ee74be1cbc99731cd453
```

This repository should not compile, install, or load a package namespace
directly from an exdqlm source checkout. Reference runtimes are materialized
from exact Git archives under ignored local cache directories.

## Basic build commands

```bash
make smoke
make test-theory-figures
make test-theory-tables
make test-manuscript-language
make pdf
make supplement
make package-install
make test-native
make test-native-mean-tilt
make test-standalone-contracts
make test-ecm
make test-tcsp
make test-bayes-uq
make rqr-ecm-validation-smoke
make tcsp-split-exact-validation-smoke
make rqr-bayes-uq-validation-smoke
make rqr-bayes-uq-refined-smoke
make prepare-exdqlm-runtime
make test-exdqlm-rqr
make literature-manifest
```

Long-running simulation and validation targets require explicit reviewed
configuration and should write only under ignored local output roots.
The authoritative tolerance validation for the manuscript is configured in
`application/config/rqr_bayes_uq_validation_main_3method_skewstress_dgps_20260820.json`.
It compares TCSP, Young--Mathew, and Wilks over the feasible iid tolerance
grid at tolerance confidence 0.95, with 1000 paired replications per
distribution, sample-size/content cell, and method. The panel uses two
symmetric anchors and six skew-stress laws. The completed manuscript run is
`application/runs/rqr_bayes_uq_validation_main_3method_skewstress_dgps_20260820/wave_confirmatory_skewstress_dgps_20260821T005632Z`.
Use `make rqr-bayes-uq-skewstress-smoke` before
`make launch-rqr-bayes-uq-skewstress`; pass
`RQR_BAYES_UQ_SKEWSTRESS_RUN_DIR=<run_dir>` to the matching health, collect,
and stop targets.

The earlier refined-panel validation remains available for audit under
`application/config/rqr_bayes_uq_validation_main_3method_refined_dgps_20260820.json`
and
`application/runs/rqr_bayes_uq_validation_main_3method_refined_dgps_20260820/wave_confirmatory_refined_dgps_20260820T221539Z`,
but it is no longer the default source for article tables and figures.

The 95% single-data oracle-tilt illustrations are rendered from the corrected
version-5 compact evidence bundle under
`figures/data/oracle_tilt_c095_v5_exact_delta/`. The command

```bash
make model-illustration-figures
```

then verifies that evidence and renders the supplemental diagnostic figures
without fitting a model. All 27 chains and six cells completed with exact
source/runtime binding and zero repairs. Five cells are strict passes. DLM/SH
is a diagnostic-aware pass: five of 137 rows retain bulk-ESS warnings, while
all R-hat, tail-ESS, MCSE/SD, hard-computational, and broad-recovery
requirements pass. No threshold was relabeled, no seed was replaced, and no
chain was selectively extended. See
`docs/audits/oracle_tilt_c095_v5_promotion_reconciliation_20260810.md`.

The version-3 non-promotion decision and its original 0.20 failure remain
available under
`docs/audits/oracle_tilt_c095_v3_nonpromotion_evidence_20260805/`; see
`docs/audits/oracle_tilt_c095_v3_nonpromotion_closeout_20260805.md` for that
historical decision. Versions 2 and 3 remain validated prior bundles but are
superseded for rendering. Current `main` blocks another same-data heavy run.
All campaigns are single-data method illustrations, not
repeated-sample coverage studies or response-predictive analyses.

A subsequent oracle audit found that the historical illustration helper
stored an unnormalized truncated first moment where the manuscript's recovery
definition requires the conditional retained mean minus the population mean.
The sampler's fixed-tilt update was already correctly scaled; the correction
is confined to the oracle inputs. The append-only V5 source and schema-2
certificate are implemented, and the completed corrected V5 campaign is now
the rendered evidence. Strict R-hat, ESS, MCSE, and narrow recovery violations
remain visible warnings; they were not relabeled, reseeded, or used to abort
later cells.
The correction protocol and the separate repeated-DGP validation protocol are
in `docs/implementation_notes/exact_mean_tilt_oracle_and_v5_correction_protocol_20260810.md`
and `docs/implementation_notes/oracle_mean_tilt_validation_v1_protocol_20260810.md`.

## arXiv source package

The selected exact-target `rootwise2_ASIS2` transition repeats the symmetric
rootwise partially collapsed component-scale composition twice and then runs
two centered--noncentered ASIS cycles. The complete exact-runtime promotion
passed all 4,423 frozen diagnostic rows across M01, M02, and fixed-design
gates, all 16 horizon checks, the dynamic endpoint check, and the resource
envelope without relaxed thresholds, replacement seeds, or selective
extension. A later generic package test exposed and correctly stopped an
installation-isolation defect; it occurred after the heavy gates and did not
change their results. Package checks are now installed into a disjoint
library, document checks build from an isolated Git archive, and the corrected
hermetic validation matrix preserves both the attested runtime digest and a
clean source checkout.

The transition changes MCMC efficiency, not the generalized posterior or
response interpretation. Development and promotion outputs remain distinct
from confirmatory outputs. A separate flag-only authorization commit, fresh
exact runtime, preflight, oracle-reference bundle, and new append-only run root
are still required before the confirmatory study begins. The authenticated
promotion closeout is
`docs/audits/rqr_dlm_exact_promotion_rootwise2_ASIS2_20260730/README.md`.
The earlier failure history and recovery boundary are recorded in
`docs/audits/rqr_dlm_main_third_launch_wave2_closeout_20260727.md`,
`docs/audits/rqr_dlm_second_wave_component_scale_diagnosis_20260727.md`,
`docs/audits/rqr_dlm_exact_promotion_e9c8068_closeout_20260727.md`,
`docs/audits/rqr_dlm_main_correction_budget_20260727.csv`, and
`docs/audits/rqr_dlm_relaunch_readiness_audit_20260727.md`, together with
`docs/implementation_notes/rqr_dlm_main_third_launch_recovery_plan_20260727.md`
and
`docs/implementation_notes/rqr_dlm_two_ASIS_finish_plan_20260727.md`.

The main article source package is generated with:

```bash
make test-theory-figures
make test-theory-tables
make pdf
make supplement
make arxiv-source
```

`make arxiv-source` creates a compact zip under
`application/cache/arxiv_preprint_<stamp>/`. The zip contains `main.tex`,
`main.bbl`, `refs.bib`, the main table input, required figure files,
`SOURCE_MANIFEST.txt`, and `README.txt`.

The supplement is a separate TeX document. If it is submitted with the
preprint, upload it separately or export a combined source bundle after
verifying that both PDFs correspond to the same commit.

Recommended arXiv starting point:

- primary archive: `stat`
- primary subject class: `stat.ME`
- possible cross-lists: `stat.CO` for computation or `stat.TH` for theory,
  depending on the final abstract and emphasis.

See `docs/implementation_notes/arxiv_preprint_submission_checklist_20260727.md`
for the current pre-submission checklist.

## Local-only workspaces

The following paths are for local data, caches, generated outputs, and working
notes:

```text
application/data_local/
application/cache/
application/runs/
application/logs/
application/outputs/
literature/pdfs/
literature/notes/
local_trackers/
```

Do not commit heavy fitted objects, raw simulation outputs, TeX build logs,
local PDFs, or temporary review handoffs.
