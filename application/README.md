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

    make package-document
    make package-install
    make test-native

`make package-document` regenerates both the Rcpp registration wrappers and
the roxygen2 namespace/help files. Commit those generated files with their
source changes; CI reruns the target and rejects any drift.

## Ordinary zero-tilt RQR version 1

The fixed-design sampler, ridge/full-Gaussian/RHS-NS priors, missing-response
contract, checkpoint continuation, frozen-DESN wrapper, and future-design
contracts are native to `rqrgibbs`. Promotion-grade DESN validation first
materializes one D02 design without fitting a readout:

    export RQR_EXPECTED_PRIMARY_COMMIT="$(git rev-parse HEAD)"
    make materialize-ordinary-v1-desn
    make preflight-ordinary-v1
    make reference-ordinary-v1
    make test-ordinary-v1-dlm-companion
    export RQR_DLM_REFERENCE_DIR="/path/to/dlm-reference"
    export RQR_DLM_WAVE1_M01_DIR="/path/to/wave1-M01"
    export RQR_DLM_WAVE1_M02_DIR="/path/to/wave1-M02"
    export RQR_DLM_WAVE2_M01_DIR="/path/to/wave2-M01"
    export RQR_DLM_WAVE2_M02_DIR="/path/to/wave2-M02"
    export RQR_DLM_HORIZON_M03_DIR="/path/to/horizon-M03"
    export RQR_DLM_RESOURCE_ENVELOPE_DIR="/path/to/resource-envelope"
    export RQR_ORDINARY_V1_DLM_COMPANION_OUTPUT_DIR="/new/ignored/path"
    make bundle-ordinary-v1-dlm-companion
    RQR_ORDINARY_V1_BENCHMARK_CONFIRM=I_CONFIRM_ORDINARY_V1_ONE_CELL_BENCHMARK \
      make benchmark-ordinary-v1-one-cell
    make test-ordinary-v1-monitor

The versioned fitted/output surface is:

| Family | Fit | Posterior root draws | Interval/root evaluation |
|---|---|---|---|
| Fixed design | `rqrgibbs_static_fit/1.2.0` | `rqrgibbs_static_draws/1.0.0` | `rqrgibbs_interval_prediction/2.0.0` |
| Frozen-design DESN readout | `rqrgibbs_desn_fit/1.2.0` | `rqrgibbs_desn_draws/1.0.0` | `rqrgibbs_desn_prediction/1.0.0` |
| DLM | `rqrgibbs_fit/1.14.0` | `rqrgibbs_dlm_draws/1.0.0` | `rqrgibbs_dlm_prediction/1.0.0` fitted; `rqrgibbs_dlm_forecast/1.0.0` future |

Static output provenance is further typed by
`rqrgibbs_static_draw_source/1.0.0`,
`rqrgibbs_static_draw_selection/1.0.0`, and
`rqrgibbs_static_prediction_source/1.0.0`. DLM output provenance is typed by
`rqrgibbs_dlm_draw_source/1.0.0`, `rqrgibbs_dlm_rng_binding/1.0.0`,
`rqrgibbs_dlm_forecast_source/1.0.0`, and
`rqrgibbs_dlm_future_contract/1.0.0`. DESN draws use
`rqrgibbs_desn_draw_source/1.0.0`. The DESN surface is deliberately a readout
conditional on a frozen deterministic feature design; it does not construct
or refit a reservoir during MCMC. Every listed output describes interval-root
functionals under a loss-based generalized posterior, not response draws.

The materializer loads both packages only from exact archive-built isolated
runtimes, guards the protected exdqlm checkout including ignored files, and
writes a receipt-v3 design plus compact hashes below ignored directories. The
receipt explicitly records source/runtime lineage, the
materializer-argument digest, and the complete materialized-payload digest;
the tracked configuration and compact evidence retain the raw effective
arguments and seed. The wrapper discovers the unique design for the current
source SHA in `reference-only`, `benchmark-one-cell`, and `execute-bounded`;
an explicit `RQR_ORDINARY_V1_ATTESTED_DESN_DESIGN_RDS` can override discovery.

The four validator modes are `preflight`, `reference-only`,
`benchmark-one-cell`, and `execute-bounded`. BENCH01 is exactly four D02 DESN
fixed-shoulder RHS-NS chains under the normalized learned rate. It is a
resource/mechanics benchmark, not part of the 48-fit grid or a scientific
pilot, and it requires the exact confirmation shown above while bounded
execution remains disabled.

`reference-only` makes eight explicit top-level fit calls: four learned-rate
F01 ridge chains for the sampler-to-quadrature oracle and four tiny attested
DESN reference fits. Source-bound test files also exercise additional tiny
fits and continuations; those internal calls are intentionally identified as
uninstrumented rather than folded into the top-level fit count.

BENCH01 and the protected-DLM companion are generated from the reviewed,
execution-disabled source candidate. A later authorization commit may change
only the execution flag and the recorded reviewed SHA, after which the exact
primary runtime is rebuilt and `reference-only` is regenerated at that
authorization commit. Before the 48-fit grid starts, the runner verifies the
candidate benchmark through the strict flag-only source delta, unchanged
package/toolchain contract, exact pinned exdqlm runtime, hashes, and attested
D02 design. The primary runtime-tree digest is allowed to change across the
two exact builds because its lineage records different source commits. The
refreshed reference bundle must instead match the authorization runtime and
toolchain exactly.

The companion-v2 collector binds seven independently completed DLM evidence
roles: the reference suite, wave-1 M01 and M02, wave-2 M01 and M02, the
horizon-M03 checks, and the resource envelope. It validates 55 input artifacts
against 23 semantic gates, does not fit a model or deserialize/copy heavy RDS
inputs, and publishes exactly five compact files under the new ignored output
directory. Each role must be supplied separately through the environment
variables shown above; substituting one wave directory for another is rejected.
Every M01 fit is bound to a complete independently frozen role contract. The
static-regression and local-level full digests differ only because their
collapsed coordinates are named `regression` and `level`; cross-wave agreement
uses a versioned invariant that excludes only this label. The independently
hashed resource toolchain must also match the reference
R/compiler/BLAS/LAPACK and shared-package facts exactly.

Bounded execution remains disabled in source candidates. Authorization uses
two commits: a reviewed, fail-closed implementation commit and a later
config-only commit that changes only the execution flag and records the
reviewed implementation SHA. The runner reconstructs and verifies that entire
delta before it can accept `RQR_ORDINARY_V1_CONFIRM=YES`.

Final authorization also requires the exact hashed reference bundle and both
benchmark bundles, together with the compact protected-DLM companion. Their
locations are supplied through
`RQR_ORDINARY_V1_REFERENCE_DIR`, `RQR_ORDINARY_V1_BENCHMARK_DIR`, and
`RQR_ORDINARY_V1_BENCHMARK_MONITOR_DIR`; the companion is supplied through
`RQR_ORDINARY_V1_DLM_COMPANION_DIR`. The benchmark output binds four
passing fits, diagnostics, DESN training provenance, conditional future-root
contract evidence, and local chain hashes; its monitor bundle binds sampled
resources, wrapper closeout, and the wrapper artifact manifest.
Bounded execution retains an exact validated copy of the companion's five
compact files under `protected_dlm_companion/`. Successful mode outputs use
closed filename sets, and the wrapper rehashes every R-output byte after the R
process exits. Failed modes may publish only a mode-specific compact subset
with valid terminal failure/status rows; approved progress tables and partial
companion bytes remain recursively hash-bound. The monitor directory itself
is closed to five declared pre-manifest files plus its final wrapper manifest,
and the ten-scenario fault suite rejects hidden additions and filenames that
imitate atomic-manifest temporaries.

Per-fit provenance remains strict. Native fixed-design fits bind only the
isolated primary runtime and require no external repository. Promotion-grade
D02 DESN fits require exactly the pinned isolated exdqlm runtime and a
receipt-v3 design reverified against that executing runtime.

The receipt-v3 object attests the materialized training design. A versioned
future-design object separately validates the parent feature schema, feature
order, declared precomputed/teacher-forced/external-driver semantics, and
future-row content digest. It does not attest the process that generated those
future rows, and the training receipt does not transfer to them. Until a
future-specific materialization receipt is available, returned DESN
future-root functionals are nonpromotable even when their future contract
validates. Bounded future checks concern conditional root propagation on
frozen rows, not scientific forecast provenance and not future responses.

Exact static continuation is available through `rqr_mcmc_continue()`.
Continuation-only initialization fields cannot be injected through
`rqr_mcmc_fit()`: a private token-bound worker is reached only after checkpoint
and continuation-history validation. Unknown or ambiguous `init` and
`mcmc_control` fields fail instead of being silently ignored.

`lambda_initial` and fresh observed-site `latent_v` values initialize a valid
scan but are not separate overdispersed-start dimensions. The mandatory
learned-rate sweep first redraws the rate from its collapsed conditional and
then refreshes every observed latent scale; fixed-rate sweeps also refresh
those scales before the first root update. The four-chain initialization
profiles therefore disperse root coefficients and, where applicable, complete
RHS-NS prior states.

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
at an explicitly expected commit. Full state-path storage defaults to off, but
storage never changes the Markov transition or RNG stream. Every accepted
exact DLM mode (fixed W, frozen discount template, and component scale)
completes and retains the Gaussian time-zero state for both roots;
`store_state_draws` controls only the full `p x T` path arrays. The
experimental adaptive working mode omits time-zero completion and records that
omission in its transition contract. Terminal state draws remain available to
`rqr_forecast_roots()`, which can use
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
is exploratory or test-only. Native fixed-design inference, native RHS-NS
updates, and a fit conditional on a serialized `rqr_desn_design` do not load
exdqlm. Only the optional exdqlm reference materializer requires an
isolated-library attestation for the clean pinned source; a namespace loaded
from that source checkout is intentionally ineligible. Run `make
prepare-primary-runtime` and, when using that materializer,
`make prepare-exdqlm-runtime` with the reviewed primary commit in
`RQR_EXPECTED_PRIMARY_COMMIT`. Version-5 attestations
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
