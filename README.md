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

## arXiv source package

The complete second-wave development gate then showed that schedule matching
alone was insufficient: all 49 M01 fits completed, but only 1,131 of 1,150
diagnostics passed and 12 of 25 tasks failed at least one gate, predominantly
for the shared component scale.  The current fail-closed correction preserves
singleton state arrays as
`p`-by-`T` matrices, matches component-scale and M02 sentinel schedules to
their already frozen standard schedules, holds the full M02 DLM target common
across chains while supplying distinct target-preserving MCMC warm starts
through the CRAN interface, moves diagnostic construction inside the
structured failure boundary, and retains only compact endpoint/diagnostic
objects instead of accumulating full sentinel fits. It also adds exact
rootwise partially collapsed component-scale transitions: a deterministic C++
Kalman marginal integrates and redraws each root in turn before the existing
centered--noncentered ASIS transition. The symmetric second block was added
after a clean one-root promotion gate still missed six of 1,150 wave-2
diagnostics; none of that failed gate is reused. A subsequent one-cycle
symmetric development wave still missed three of 1,150 diagnostics, so the
current candidate uses the selected `rootwise2_ASIS2` composition: two exact
rootwise component-scale cycles followed by two exact ASIS cycles before any
new promotion gate. This changes the transition, not the generalized posterior
or iteration-count budget. Its development outputs are not scientific or
promotion evidence. The maximum contract now
contains 205,658,000 MCMC iterations: 74.8257 percent above the original
Output-15 budget and 3.2949 percent above the previously launched ASIS-corrected
budget.  The execution flag remains false until exact-source projection, M01
and M02 mixing, resource, package, and document gates pass.
The exact third-run closeout and recovery boundary are recorded in
`docs/audits/rqr_dlm_main_third_launch_wave2_closeout_20260727.md`,
`docs/audits/rqr_dlm_second_wave_component_scale_diagnosis_20260727.md`,
`docs/audits/rqr_dlm_exact_promotion_e9c8068_closeout_20260727.md`,
`docs/audits/rqr_dlm_main_correction_budget_20260727.csv`, and
`docs/audits/rqr_dlm_relaunch_readiness_audit_20260727.md`, together with
`docs/implementation_notes/rqr_dlm_main_third_launch_recovery_plan_20260727.md`
and
`docs/implementation_notes/rqr_dlm_two_ASIS_finish_plan_20260727.md`.
A replacement coordinator may start only from a fresh exact-commit
authorization and a new ignored run root after the complete first-wave and
affected-wave correction gates pass.

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
`main.bbl`, `refs.bib`, the main table input, required PNG figures,
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
