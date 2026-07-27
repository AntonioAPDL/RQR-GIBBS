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
when it is enabled, exact fixed-W and frozen-template fits complete each
retained path with a draw from the Gaussian time-zero conditional. Component-
scale fits retain the same time-zero states because their innovation-scale
update conditions on them.
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
first launch failed closed on a fixed-W time-zero estimand-schema mismatch.
After correction and independent review, a fresh exact-source launch completed
all 24 fits and passed all 897 diagnostics with zero numerical repairs. The
committed config is again disabled; the successful evidence remains tied to
its one-time launch commit. The shared estimand extractor applies to all six
fixture/mode continuation cells and requires complete retained time-zero
states. Any authorization must bind the complete recursive artifact manifest
from a passing reference run and the identical isolated runtime and toolchain.
The monitor uses PGID sampling, an idempotent signal/error
finalizer, fault-injection tests, and a final group sweep. It terminates on
timeout or an observed process/thread/RSS limit and still writes a structured
failure ledger, closeout, resource summary, and recursive hash manifest. The
sampled maxima are telemetry, not kernel-hard peaks. `make test-dlm-monitor`
exercises eight failure modes.

The frozen execution schedule is four chains with 2,000 burn-in and 6,000
retained draws per chain, thinning one, and a 240-minute whole-grid ceiling.
Every chain must match an independently constructed ordered estimand schema.
Primary future mixing targets are deterministic conditional-mean root
functionals that preserve retained-draw identity. Stochastic future root-state
draws are retained as a sidecar and do not imply a response-simulation
contract. Local chain RDS files are read back and checked for class, exact
object identity, checkpoint digest, continuation history, byte count, and
SHA-256 before their atomic publication.

`scripts/11_promote_rqr_dlm_bounded_evidence.R` independently verifies a
completed ignored run, reopens every fit object, and promotes only compact
evidence. The preliminary matched-simulation config is
`config/rqr_dlm/rqr_dlm_main_simulation_preliminary_20260724.R`; both of its
execution authorizations are false.

The reviewed confirmatory contract is
`config/rqr_dlm/rqr_dlm_main_simulation_20260724.R`. Its runner
`scripts/15_run_rqr_dlm_confirmatory_simulation.R` implements fail-closed
preflight, oracle-reference, embedded-sentinel, confirmatory, collection, and
audit modes. `scripts/17_launch_rqr_dlm_confirmatory_wave.R` partitions one
canonical precision-batch wave across the frozen worker slots. Its append-only
state permits only the next wave, requires a same-batch sentinel pass before
standard work, and binds later batches to the preceding verified precision
decision. `scripts/18_orchestrate_rqr_dlm_confirmatory_simulation.R` advances
that state and performs collection at every batch boundary;
`scripts/19_prepare_rqr_dlm_confirmatory_authorization.R` constructs the exact
post-review authorization bundle; `scripts/20_launch_rqr_dlm_confirmatory_simulation.sh`
starts the complete study under a detached supervisor; and
`scripts/21_healthcheck_rqr_dlm_confirmatory_simulation.R` reports read-only
progress. Every worker still passes through the process-group monitor and the
commit-bound authorization boundary. The collector verifies exact task sets,
recursive artifact hashes, and a common source/runtime/seed bundle before
producing an analysis. Both execution flags remain false pending independent
review.

The third fresh main-study attempt stopped fail-closed after one passed and
one failed canonical wave. The failed local-level sentinel wave identified
one-state exdqlm dimension dropping and M01 component-scale mixing failures;
the protected exdqlm source was not changed. A later complete development gate
finished all 49 M01 chains but failed 19 of 1,150 diagnostics across 12 of 25
tasks, so the fixed 6,000-draw schedule is not treated as sufficient under
ASIS alone. The current correction adds dimension-preserving state extraction,
an exact one-root partially collapsed scale update evaluated by a deterministic
C++ Kalman marginal, and fixed 6,000-draw and 9,000-draw
component-scale sentinel schedules. Three slice sweeps were selected in a
shortened, development-only four-profile comparison before the exact complete
wave gates. The correction also adds a fixed 4,000-draw-per-endpoint M02
sentinel schedule, one common M02 DLM prior/evolution target across chains,
distinct target-preserving M02 initial state paths and scales,
structured publication of post-fitting diagnostic exceptions, and per-chain
compaction before atomic sentinel-diagnostic serialization.
`scripts/22_validate_rqr_dlm_wave1_corrections.R` and
`scripts/23_validate_rqr_dlm_wave1_comparator_projection.R` accept a frozen
`RQR_CORRECTION_WAVE_ID`, while
`scripts/25_validate_rqr_dlm_resource_envelope.R` exercises the largest
planned retained-state shapes.  The execution flag remains false until these
gates pass from a clean exact isolated runtime.
