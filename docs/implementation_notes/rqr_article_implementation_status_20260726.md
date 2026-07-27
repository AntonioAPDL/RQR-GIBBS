# RQR article implementation status moved from the manuscript

Date: 2026-07-26

This note preserves implementation and validation material that was removed
from the statistical article during the editorial pass. The article now
separates the population functional, generalized-Bayes computation, dynamic
model, and evaluation strategy from repository governance. This file is a
status record, not statistical evidence.

The reader-facing model hierarchy is static interval-root regression, an
ordinary-only native regularized-horseshoe implementation based on the
Nishimura–Suchard augmentation (RHS-NS), frozen-feature DESN as a static-design
specialization, and dynamic linear root states. DESN reuses the static
coefficient sampler; the DLM is the first architecture that replaces it with
root-specific FFBS. The pinned exdqlm runtime remains a read-only design
materializer and parity reference; it is not the ordinary RHS-NS inference
engine.

## Scope ledger at the editorial freeze

| Component | Mathematical scope | Software status | Article evidence status |
|---|---|---|---|
| Ordinary static ridge RQR | Loss target and Gaussian/GIG coefficient blocks | Implemented natively | Small target checks exist; matched results are not reported in the manuscript |
| Ordinary static full-Gaussian RQR | Proper multivariate Gaussian coefficient prior with a declared mean and positive-definite precision | Implemented natively; ridge is recovered by zero mean and scalar precision | Dense conditional and ridge-equivalence checks exist; matched results are not reported in the manuscript |
| Ordinary static RHS-NS RQR | Conditional-Gaussian regularization with separate root-specific prior states; fixed or declared learned \(\kappa\) | Implemented natively under the declared RHS-NS joint kernel | Conditional, parity, continuation, and bounded release gates are defined; no nonzero-tilt RHS-NS claim |
| Ordinary RQR-DESN | Frozen deterministic-feature specialization of static regression | Native readout implemented; the promotion fixture is materialized through a pinned isolated exdqlm reference runtime | Design/readout checks exist; matched DESN evidence remains deferred |
| Ordinary RQR-DLM | Alternating root-specific FFBS for declared fixed-joint modes | Implemented | Reference and bounded checks exist; comparative results remain governed by the simulation protocol |
| Static mean-tilted RQR | Population target and fixed-rate information shift under proper Gaussian/ridge priors | Derived, not implemented | No empirical evidence |
| DESN mean-tilted RQR | Frozen-feature specialization under a proper Gaussian/ridge readout | Not implemented | No empirical evidence; RHS-NS propriety remains unresolved |
| Dynamic mean-tilted RQR | Conditional information-shift algebra only | Not implemented | No empirical evidence |
| Data-driven tilt selection | Population motivation only | Not implemented | No empirical evidence |
| Variational inference | Future target and derivation project | Not implemented | No evidence |

## Main-simulation correction status

The first authorized main-study wave at
`b8b7748ab181a006611b602f64d4edf5be591de6` stopped fail-closed. The partial
run is not comparative evidence and will not be resumed. It identified two
computational defects within RQR-GIBBS:

- a centered-only shared component-scale transition with inadequate
  `log_q_1` mixing under the predeclared gates; and
- an M02 adapter that flattened a multistate CRAN `exdqlm` posterior mean
  rather than projecting it through the observation design.

The first correction composed the centered inverse-Gamma update with an exact
noncentered log-scale slice transition. A complete second-wave development
gate later finished all 49 M01 chains but failed 19 of 1,150 diagnostics
across 12 of 25 tasks, principally because the component scale retained
substantial trajectory dependence. The next prospective kernel therefore
adds an exact one-root partial collapse, evaluated by a deterministic C++
Kalman marginal and independently checked in R, before the root-specific FFBS
and existing ASIS move. A shortened four-profile computational comparison
selected three slice sweeps before the complete exact-source gates; it is
transition-selection evidence and not a scientific result. The M02 adapter
now returns one projected ordinate per
time and supplies genuinely distinct target-preserving warm starts while
holding its prior and evolution target common across chains. Uniform schedules
were fixed from computational diagnostics only; there is no realized-metric
selection, adaptive extension, or retry. The scientific design, generalized
posterior, priors, seed ledger, target coverages, estimands, and diagnostic
thresholds remain unchanged.

The correction derivation and failure reconciliation are
`docs/implementation_notes/rqr_dlm_component_scale_interweaving_20260726.md`
and
`docs/audits/rqr_dlm_main_wave1_scale_and_projection_failure_20260726.md`,
with the second-wave diagnosis in
`docs/audits/rqr_dlm_second_wave_component_scale_diagnosis_20260727.md`.
The replacement full study remains unauthorized until exact isolated-runtime
M01 and M02 gates for both affected waves, the full repository validation
matrix, and fresh preflight/oracle evidence pass. Any replacement must use a
new authorization, run identifier, and output root.

## Evidence interpretation

The bounded normalized learned-scale work is a target-and-implementation check
for a declared small fixture. It is not evidence of empirical coverage
calibration, endpoint uncertainty calibration, response-predictive validity,
or comparative forecasting performance.

Promotion-grade dynamic work keeps the following distinctions explicit:

- fixed evolution covariances, frozen discount templates, and shared
  component scales define fixed-joint targets;
- adaptive conditional discount recursion is a working method, not an exact
  Gibbs sampler for a demonstrated joint density;
- a learned inverse-loss scale belongs to the declared normalized generalized
  target and is not a response variance;
- root-path draws are draws of interval endpoint functions, not
  posterior-predictive responses.

## Reproducibility location

Exact launch commits, runtime attestations, manifests, seeds, validation gates,
continuation records, and compact closeouts remain in `docs/audits/`,
`docs/implementation_notes/`, and the tracked application configuration.
Heavy fits, state paths, logs, and simulation outputs remain under ignored
local directories.

This ledger should be updated when a methodological component changes status.
It should not be copied back into the manuscript as a software inventory.
