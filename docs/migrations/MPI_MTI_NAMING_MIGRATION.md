# MPI/MTI Naming Migration

Date: 2026-08-13  
Status: Phase-1 authoritative terminology and compatibility migration

## Authoritative Terms

The active manuscript and package documentation now use:

- `MPI`: Mean-Preserving Interval.
- `MTI`: Mean-Tilted Interval.
- `MPI loss`: the zero-tilt residual-product loss.
- `MTI loss`: the retained-mean-tilted residual-product loss.
- `TCSP-MTI`: the scan-calibrated tolerance-calibrated shortest-path MTI
  plug-in construction.

MPI is the zero-tilt member of the MTI family. Mean preservation refers to the
retained distribution, \(E(Y \mid a < Y < b) = E(Y)\), not to the interval
midpoint.

## Historical Attribution

Pouplin et al. introduced the residual-product criterion under the name
Relaxed Quantile Regression (RQR). The RQR name remains in the repository only
for:

- attribution to Pouplin et al. and their published RQR-W/RQR-O variants;
- legacy `rqr_*` public wrappers and existing serialized schemas;
- frozen audits, evidence, run manifests, and external branch names;
- compatibility paths that would otherwise break reproducibility.

## Implemented Phase

This phase keeps the installed package name as `rqrgibbs` and adds canonical
MPI/MTI wrappers over the validated implementation:

- core algebra: `interval_check_loss()`, `interval_residual_product()`,
  `mpi_loss()`, `mti_loss()`, `mti_constants()`, `mti_gig_params()`;
- fixed-target computation: `mti_mcmc_fit()`, `mti_ecm_fit()`,
  `mti_ecm_path()`, `mti_vb_fit()`, `mti_posterior_draws()`;
- model wrappers: `mti_regression()`, `mti_desn_fit()`, `mti_as_dlm_model()`,
  `mti_dlm_fit()`, `mti_forecast_roots()`, `mti_evolution_*()`,
  `mti_ffbs_*()`;
- generic interval utilities: `interval_order_endpoints()`,
  `interval_canonicalize_root_draws()`,
  `interval_canonicalize_root_paths()`;
- TCSP wrappers: `tcsp_*()` and `tcsp_plugin_mti_fit()`;
- response-distribution UQ wrappers: `dp_*()`, `dpm_*()`,
  `bayesian_bootstrap_shortest_draws()`, and `weighted_shortest_interval()`.

New fitted objects place canonical classes before legacy classes, for example
`c("mti_mcmc", "rqr_mcmc", "mti_fit", "rqr_fit")`. Legacy classes remain so
old methods and serialized objects continue to dispatch.

## Deferred Phases

The PRO report recommends eventually renaming the package to `mtiintervals`,
renaming source files, and renaming the GitHub repository to `MTI-INTERVALS`.
Those steps are intentionally deferred until after the current validation
launch because they require coordinated changes to Rcpp registration,
`useDynLib`, tarball names, installed-package caches, Makefile targets, and
runtime attestations.

The current phase is therefore a conservative compatibility migration:
canonical terminology and public wrappers are available now, while the package
and historical paths remain stable for reproducibility.
