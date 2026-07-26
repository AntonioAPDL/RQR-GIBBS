# RQR article implementation status moved from the manuscript

Date: 2026-07-26

This note preserves implementation and validation material that was removed
from the statistical article during the editorial pass. The article now
separates the population functional, generalized-Bayes computation, dynamic
model, and evaluation strategy from repository governance. This file is a
status record, not statistical evidence.

## Scope ledger at the editorial freeze

| Component | Mathematical scope | Software status | Article evidence status |
|---|---|---|---|
| Ordinary fixed-design RQR | Loss target and Gaussian/GIG blocks | Implemented | Small target checks exist; matched results are not reported in the manuscript |
| Ordinary RQR-DLM | Alternating root-specific FFBS for declared fixed-joint modes | Implemented | Reference and bounded checks exist; comparative results remain governed by the simulation protocol |
| Ordinary RQR-DESN | Deterministic-feature readout | Implemented | Design/readout checks exist; matched DESN evidence remains deferred |
| Fixed-design mean-tilted RQR | Population target and fixed-rate information shift | Derived, not implemented | No empirical evidence |
| Dynamic mean-tilted RQR | Conditional information-shift algebra only | Not implemented | No empirical evidence |
| DESN mean-tilted RQR | Fixed-feature algebra only | Not implemented | No empirical evidence |
| Data-driven tilt selection | Population motivation only | Not implemented | No empirical evidence |
| Variational inference | Future target and derivation project | Not implemented | No evidence |

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
