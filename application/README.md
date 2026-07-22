# Application package

This directory is the development R package **rqrgibbs** and the reproducibility
layer for the standalone article.

## Native package layout

- **R/rqr_dlm_model.R** provides model builders and composition compatible with
  the public exdqlm FF, GG, m0, C0, df, and dim.df concepts.
- **R/rqr_ffbs.R** provides pure-R reference filtering, smoothing, and FFBS.
- **src/rqr_ffbs.cpp** provides the C++17/RcppArmadillo bottleneck.
- **R/rqr_dlm_fit.R** provides the partially collapsed RQR-DLM sampler and an
  explicit future root-state forecasting contract with exact continuation.
- **R/rqr_evolution.R** provides shared component-specific evolution scales,
  sampled time-zero states, and conjugate inverse-Gamma updates.
- **R/rqr_numerics.R** provides Cholesky diagnostics and the native GIG(1/2)
  sampler.
- The remaining R files provide fixed-design, DESN, forecasting, VB-screening,
  and oracle routines promoted from the implementation seed.
- **tests/** contains native package gates and copied pinned-exdqlm reference
  tests.
- **scripts/** contains preflight, manifest, simulation, collection, and audit
  scripts.

Install and run the native gates from the repository root:

    make package-install
    make test-native

The pseudo-AL representation augments a loss and is not a response likelihood.
The fixed-W, discount-template, and component-scale modes are exact for their
declared Gaussian evolution priors. Adaptive conditional discounting is
mathematically incompatible in general with the advertised pair of simple
Gaussian full conditionals; it remains experimental, and its fit objects
record **exact_joint_target = FALSE**.

Use `rqr_evolution_fixed()` for an explicit fixed prior,
`rqr_freeze_discount_template()` for a pre-MCMC exdqlm-compatible template,
`rqr_evolution_component_scale()` for the exact hierarchical alternative, and
`rqr_evolution_adaptive_working()` only when the experimental status is
intentional.

The default numerical policy fails on any covariance factorization requiring
repair. The optional audit policy records each repair and prevents the fit from
being marked promotion eligible. Full state-path storage defaults to off;
terminal state draws remain available to `rqr_forecast_roots()`. Fit objects
include a versioned provenance and RNG checkpoint, and `rqr_dlm_continue()`
restarts the exact native Markov stream on the same platform.

The heavy directories **data_local**, **cache**, **runs**, **logs**, and
**outputs** are ignored by git.
