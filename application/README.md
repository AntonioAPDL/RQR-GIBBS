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
Numerical-repair counts, environment mismatch/override history, and promotion
eligibility are stored per generation in a separately digested cumulative
continuation contract. Its validator reconstructs parent-checkpoint links,
repair totals, exactness, reproducibility, promotion, and the mismatch ledger
across every generation. `backend="auto"` records both the requested and
resolved backend. Promotion requires the executing `rqrgibbs` namespace to
come from a verified isolated-runtime attestation; direct `pkgload` execution
is exploratory or test-only.
RQR-DESN and RHS promotion also requires the executing exdqlm namespace to
match an isolated-library attestation for the clean pinned source. A direct
source-tree namespace is intentionally ineligible. Run `make
prepare-primary-runtime` and `make prepare-exdqlm-runtime` with the reviewed
primary commit in `RQR_EXPECTED_PRIMARY_COMMIT`. Version-5 attestations
reconstruct and compare each archive entry's Git mode, blob identifier, and
path with the declared commit tree, compare the complete expected and built
source-package file sets, rehash post-command build and installation receipts
and logs, require one successful full-package installation, and bind both
pre-marker and final installed-runtime digests. The
protected exdqlm checkout remains read-only and is checked for any source-state
change.

The heavy directories **data_local**, **cache**, **runs**, **logs**, and
**outputs** are ignored by git.

The next exact-mode dynamic validation config is
`config/rqr_dlm/rqr_dlm_bounded_dynamic_fixtures_20260723.R`. Its preflight
uses the same canonical constructor as its tests and instantiates every model,
missing-response vector, evolution object, and future contract. It remains
non-production and excludes the adaptive working recursion.
The four-mode runner
`scripts/08_run_rqr_dlm_bounded_validation.sh` provides construction
preflight, expanded reference-only validation, a representative full
four-chain one-cell benchmark, and a separately gated execution path. The
committed config keeps the 24-fit path disabled. Any future authorization must
bind the complete recursive artifact manifest from a passing reference run and
the identical isolated runtime and toolchain. The monitor uses PGID sampling,
an idempotent signal/error finalizer, fault-injection tests, and a final group
sweep. It terminates on timeout or an observed process/thread/RSS limit and
still writes a structured failure ledger, closeout, resource summary, and
recursive hash manifest. The sampled maxima are telemetry, not kernel-hard
peaks. `make test-dlm-monitor` exercises five failure modes.

The frozen execution schedule is four chains with 2,000 burn-in and 6,000
retained draws per chain, thinning one, and a 240-minute whole-grid ceiling.
Every chain must match an independently constructed ordered estimand schema.
Primary future mixing targets are deterministic conditional-mean root
functionals that preserve retained-draw identity. Stochastic future root-state
draws are retained as a sidecar and do not imply a response-simulation
contract. Local chain RDS files are read back and checked for class, exact
object identity, checkpoint digest, continuation history, byte count, and
SHA-256 before their atomic publication.
