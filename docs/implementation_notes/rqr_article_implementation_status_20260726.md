# RQR article implementation status moved from the manuscript

Date: 2026-07-26

This note preserves implementation and validation material that was removed
from the statistical article during the editorial pass. The article now
separates the population functional, generalized-Bayes computation, dynamic
model, and evaluation strategy from repository governance. This file is a
status record, not statistical evidence.

The reader-facing model hierarchy is static interval-root regression,
an ordinary-only regularized-horseshoe adapter based on the
Nishimura–Suchard augmentation (RHS-NS), frozen-feature DESN as a
static-design specialization, and dynamic linear root states. DESN reuses the
static coefficient sampler; the DLM is the first architecture that replaces it
with root-specific FFBS.

## Scope ledger at the editorial freeze

| Component | Mathematical scope | Software status | Article evidence status |
|---|---|---|---|
| Ordinary static ridge RQR | Loss target and Gaussian/GIG coefficient blocks | Implemented natively | Small target checks exist; matched results are not reported in the manuscript |
| Ordinary static RHS-NS RQR | Conditional-Gaussian regularization with separate root-specific prior states; fixed or declared learned \(\kappa\) | Implemented through the pinned, isolated exdqlm adapter | Adapter tests exist; no nonzero-tilt or standalone native-RHS claim |
| Ordinary RQR-DESN | Frozen deterministic-feature specialization of static regression | Implemented | Design/readout checks exist; matched DESN evidence remains deferred |
| Ordinary RQR-DLM | Alternating root-specific FFBS for declared fixed-joint modes | Implemented | Reference and bounded checks exist; comparative results remain governed by the simulation protocol |
| Static mean-tilted RQR | Population target and fixed-rate information shift under proper Gaussian/ridge priors | Implemented for fixed-rate ridge | Algebra and zero-tilt checks exist; broader empirical validation remains deferred |
| DESN mean-tilted RQR | Frozen-feature specialization under a proper Gaussian/ridge readout | Implemented for fixed-rate ridge readouts | Design-shell and static-readout wiring checks exist; matched evidence and RHS-NS propriety remain unresolved |
| Dynamic mean-tilted RQR | Fixed-rate root-specific canonical shifts for fixed \(W_t\) and frozen templates | Implemented for fixed-joint modes | FFBS algebra checks exist; adaptive-discount, shared-scale, learned-rate, and empirical validation remain deferred |
| Data-driven tilt selection | Population motivation only | Not implemented | No empirical evidence |
| Variational inference | Future target and derivation project; no CAVI/ELBO claim | Zero-tilt coordinate-Gaussian sidecar only | No calibrated uncertainty evidence and no nonzero-tilt VB |

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
substantial trajectory dependence. The next kernel added an exact one-root
partial collapse, evaluated by a deterministic C++ Kalman marginal and
independently checked in R, before the root-specific FFBS and existing ASIS
move. Its first exact-runtime wave-2 gate still missed six of 1,150
diagnostics, all involving one-chain scale dependence (with one accompanying
loss failure). The current symmetric correction applies the same invariant
marginal-update/conditional-redraw block to each root in turn and composes a
second exact centered--noncentered ASIS cycle after a one-cycle symmetric
development wave still missed three diagnostics. A shortened four-profile
computational comparison selected three slice sweeps per rootwise scale block
before the complete exact-source gates; it is transition-selection evidence
and not a scientific result. The M02 adapter
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
