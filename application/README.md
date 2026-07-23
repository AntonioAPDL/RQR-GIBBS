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

The default numerical policy fails on any Gaussian factorization requiring
repair, including a negative-eigenvalue projection. The optional audit policy
records each repair. Mathematical/numerical eligibility is separate from
reproducibility eligibility; promotion additionally requires a clean checkout
at an explicitly expected commit. Full state-path storage defaults to off;
terminal state draws remain available to `rqr_forecast_roots()`, which can use
either explicit future covariances or saved component-scale draws with fixed
future templates. Fit objects include a versioned provenance and RNG
checkpoint. `rqr_dlm_continue()` verifies schema, checkpoint integrity,
complete model/target/evolution digests, package, R, compiler, BLAS/LAPACK,
dependencies, RNG kind, and source commits before claiming exact
same-environment continuation. Any explicit environment override is stored in
the returned segment and removes reproducibility and promotion eligibility.
Numerical-repair counts and promotion eligibility are cumulative across
segments, and `backend="auto"` records both the requested and resolved backend.
RQR-DESN and RHS promotion also requires the executing exdqlm namespace to
match an isolated-library attestation for the clean pinned source. A direct
source-tree namespace is intentionally ineligible. Run
`make prepare-exdqlm-runtime` to create the ignored local runtime from a Git
archive. Preparation and reference testing use read-only Git operations,
execute only from RQR-owned cache paths, verify the archive and installed-tree
digests, and fail if any tracked, untracked, or ignored file in the external
checkout changes.

The heavy directories **data_local**, **cache**, **runs**, **logs**, and
**outputs** are ignored by git.
