# MPI/MTI Phase-1 Renaming Audit

Date: 2026-08-13  
Branch: `feature/bayesian-uq-authoritative-report6-20260812`

## Decision

PRO report 7 recommends a complete repository and R package migration from
RQR-centered terminology to MPI/MTI terminology. The statistical recommendation
is correct: the zero-tilt target is best described as the Mean-Preserving
Interval (MPI) loss, and the retained-mean-shifted family is the Mean-Tilted
Interval (MTI) loss.

The operational recommendation is staged. Immediately before the main
validation launch, renaming the R package and source-file spine would require
coordinated Rcpp registration, `useDynLib`, tarball, installed-library,
Makefile, runtime-attestation, and historical-cache changes. That is the wrong
risk profile for an overnight validation run.

This phase therefore makes MPI/MTI authoritative in active public wrappers,
class vectors, launch method labels, README, current theory docs, and
manuscript language while retaining `rqrgibbs`, `rqr_*`, and old paths as
compatibility names.

## Current Scope

- Canonical API wrappers are added in `application/R/mti_api_compatibility.R`.
- Canonical classes are placed before legacy classes for new fitted objects.
- Active Bayesian UQ launch scripts canonicalize old `tcsp_mtrqr_*` method IDs
  to `tcsp_mti_*` on config read.
- Direct DP, Gaussian DPM, and Bayesian-bootstrap helpers expose non-RQR
  canonical wrappers because they are response-distribution UQ engines, not
  MTI generalized posteriors.
- Current theory ledgers were renamed to
  `docs/theory/tcsp_mti_proof_ledger_20260811.md` and
  `docs/theory/mti_ecm_monotonicity_and_scope_20260812.md`.

## Deferred Scope

- R package rename: `rqrgibbs` to `mtiintervals`.
- GitHub repository rename: `RQR-GIBBS` to `MTI-INTERVALS`.
- Full source/test/script filename migration.
- New `mtiintervals_*` serialized schema namespace and object upgrader.
- TeX supplement filename rename and arXiv packaging update.

These are best handled after the validation launch and after the simulation
study has stable method IDs and output schemas.
