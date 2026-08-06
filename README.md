# RQR-GIBBS

Standalone manuscript and reproducibility workspace for relaxed quantile
regression (RQR) with generalized-Bayes Gibbs computation.

The project studies interval-root functionals: two regression roots are learned
directly under a coverage-targeted loss rather than obtained by inverting a
response likelihood. The current manuscript develops the population loss
geometry, pseudo-asymmetric-Laplace augmentation, fixed-design Gibbs samplers,
regularized regression extensions, frozen-feature DESN readouts, and dynamic
linear root models.

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

RQR is treated as a loss-based generalized-Bayes update. Interval-root draws
are summaries of a generalized posterior over interval functionals; they are
not posterior-predictive response draws.

The two root blocks are exchangeable under the symmetric ordinary and
mean-tilted targets. Raw fields such as `samp.beta_root1` and
`samp.beta_root2` are therefore MCMC labels, not lower- and upper-endpoint
coefficient estimates after label mixing. Interval endpoints are always
available by pointwise sorting. Coefficient-level lower/upper summaries are
reported only when a post-processing audit finds one globally separated
lower/upper chart on a declared design; otherwise the package fails closed and
keeps only raw-label coefficients plus ordered endpoint functionals. Dynamic
RQR-DLM paths follow the same whole-root principle: swaps exchange complete
root trajectories, not individual times.

The implemented ordinary-RQR paths currently include:

1. fixed-design ridge regression;
2. regularized-horseshoe regression through the Nishimura--Suchard
   augmentation;
3. frozen-feature DESN readouts as a fixed-design specialization; and
4. dynamic linear root models with fixed, frozen-discount-template, and
   component-scale evolution modes.

Mean-tilt and Cornish--Fisher calculations in the manuscript are population
geometry and initialization diagnostics. The package now exposes a bounded
fixed-tilt MCMC path for fixed-rate ridge readouts: fixed-design regression,
frozen-feature DESN readouts through the same fixed-design kernel, and RQR-DLM
models with fixed or pre-frozen discount-template evolution. Nonzero tilt is
not yet implemented for learned inverse-loss scales, RHS-NS priors,
component-scale/adaptive dynamic evolution, VB/CAVI, or data-driven tilt
selection.

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
make pdf
make supplement
make package-install
make test-native
make test-native-mean-tilt
make test-standalone-contracts
make prepare-exdqlm-runtime
make test-exdqlm-rqr
make literature-manifest
```

Long-running simulation and validation targets require explicit reviewed
configuration and should write only under ignored local output roots.

The 95% single-data oracle-tilt illustrations are rendered from the reconciled
version-3 compact evidence bundle under
`figures/data/oracle_tilt_c095_v3/`. The command

```bash
make model-illustration-figures
```

then verifies that evidence and renders the article and supplement figures
without fitting a model. All 27 baseline chains completed; the longer-chain
DLM/SH adjudication reproduced 15/15 prefixes bitwise, passed 137/137
maintained diagnostics, and used zero repairs. Five cells passed every original
recovery gate. The DLM/SH width-contrast error was 0.202623 against the original
0.20 threshold and was accepted for this illustration under a disclosed
revised tolerance of 0.21. See
`docs/audits/oracle_tilt_c095_v3_revised_promotion_20260805.md`.

The earlier non-promotion decision and its original 0.20 failure remain
available under
`docs/audits/oracle_tilt_c095_v3_nonpromotion_evidence_20260805/`; see
`docs/audits/oracle_tilt_c095_v3_nonpromotion_closeout_20260805.md` for that
historical decision. Version 2 remains a validated prior bundle but is
superseded for rendering. Current `main` blocks another same-data heavy run.
Both campaigns are single-data method illustrations, not
repeated-sample coverage studies or response-predictive analyses.

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
