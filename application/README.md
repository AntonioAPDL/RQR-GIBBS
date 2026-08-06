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
  oracle, and mean-tilt-initialization routines promoted from the implementation
  seed.
- **tests/** contains native package gates and copied pinned-exdqlm reference
  tests.
- **scripts/** contains preflight, manifest, simulation, collection, and audit
  scripts.
- **scripts/32_run_oracle_tilt_illustrations.R** is a lightweight, local-only
  illustration runner for a single fixed-design and RQR-DLM data set under
  population-oracle RQR, equal-tailed, and shortest-interval tilts. It writes
  compact summaries and figures under ignored output roots and is not a
  simulation study.
- **scripts/33_run_oracle_tilt_forensics.R** is the fail-closed diagnostic
  workflow for high-content oracle-tilt illustrations. Its preflight records
  population admissibility margins, the Gaussian dynamic prior's response to
  the linear tilt, and an escaping-slope target profile. Its separately
  authorized bounded execution retains compact per-chain state, loss, prior,
  and latent-scale traces and checks selected conditional path distributions
  against dense Gaussian references.
- **scripts/34_run_oracle_tilt_publication.R** executes the balanced,
  clean-source 95% fixed-design and fixed-W DLM illustration grid. It requires
  an isolated runtime bound to a complete clean `main` commit and writes raw
  worker envelopes only under ignored output roots.
- **scripts/35_package_oracle_tilt_evidence.R** promotes only hard-passing,
  compact illustration summaries to `figures/data/oracle_tilt_c095/`.
  **figures/generate_oracle_tilt_model_figures.R** then verifies those hashes
  and renders the manuscript figures without loading fits or launching MCMC.
- **scripts/40_run_oracle_tilt_publication_v2.R** implements the replacement
  95% illustration contract based on standardized `AL_0.80(0,1)` innovations,
  exact population tilts, a 1,200-point empirical orthogonal quadratic design,
  and a 1,200-step fixed-horizon local-linear DLM. Its four modes separate
  deterministic preflight, independent conditional references, representative
  resource benchmarking, and the 27-chain execution.
- **scripts/41_run_oracle_tilt_publication_v2.sh** applies process-group
  monitoring, fixed thread settings, sampled resource limits, signal cleanup,
  and recursive artifact hashing. **scripts/41_package_oracle_tilt_v2_evidence.R**
  accepts only a fully monitored, strict-passing execute bundle.
- **scripts/42_run_oracle_tilt_publication_v3.R** defines the richer
  single-data replacement: an exactly representable nonlinear,
  heteroscedastic cubic-spline regression and a four-state local-linear plus
  regularized-seasonal DLM with varying population scale. Its 24-gate
  reference suite includes an innovation-coordinate Gaussian oracle that
  supports positive-semidefinite reference problems and verifies actual
  four-state path sampling under the strict numerical policy. Static chains
  use four frozen, data-derived moment-pilot starts; the pilot uses the known
  innovation first absolute moment and standardized endpoint anchors but not
  the population endpoint curves, and it does not change the target.
- **scripts/43_run_oracle_tilt_publication_v3.sh** enforces the fail-closed
  process-group, thread, sampled-RSS, timeout, free-space, and artifact-hash
  contract. Execute mode delegates to
  **scripts/44_orchestrate_oracle_tilt_v3_execute.sh**, which reads the single
  authoritative `cell_plan.csv`, starts each of its six family/target cells in
  a fresh R process, admits at most two chain workers, validates an atomic cell
  receipt, and requires the cell process to disappear before continuing.
  Every worker records compact primary-runtime provenance at entry, from the
  fit object, and again at exit; all three snapshots are required for
  promotion and name any failed lineage subgate. Worker files retain only
  ordered lower and upper endpoint draws; midpoint and width draws are
  reconstructed exactly during cell summarization. Failure ledgers support an
  empty first write, sequence every record, and are included in the monitored
  wrapper's recursive hash manifest. The production-shape, non-MCMC lifecycle check is
  implemented by **scripts/44_run_oracle_tilt_v3_resource_rehearsal.sh** and
  **scripts/44_oracle_tilt_v3_resource_cell.R**.
  **scripts/43_package_oracle_tilt_v3_evidence.R** can publish only a complete
  strict-passing, process-isolated 27-chain run and never copies fitted objects.
  After exact-source authorization,
  **scripts/45_launch_oracle_tilt_v3_overnight.sh** starts the run in a
  user-level `systemd` scope with independent time, task, and memory ceilings;
  **scripts/45_launch_oracle_tilt_v3_acceptance.sh** first exercises the full
  four-chain fixed-design RQR cell under those same ceilings and stops before
  the remaining cells;
  **scripts/45_oracle_tilt_v3_health.sh** reports read-only cell, chain,
  process, RSS, time, and storage progress.
- **scripts/52_run_oracle_tilt_publication_v4.R** and
  **scripts/52_oracle_tilt_publication_v4_utils.R** implement a prospectively
  frozen three-candidate illustration screen without changing closed V3
  evidence. Each candidate contains target-shared fixed-design and DLM data,
  named L'Ecuyer streams, 18 family/target/candidate cells, and 81 unique chain
  seeds. **scripts/53_orchestrate_oracle_tilt_v4_execute.sh** starts the 18
  single-worker cells concurrently while chains remain sequential within each
  process. **scripts/53_run_oracle_tilt_publication_v4.sh** enforces exact
  runtime binding, process-group limits, headroom, and fail-closed execution.
  Its host-exclusion guard recognizes the complete V4 runner range even when
  R records a relative `--file=application/scripts/...` path. Every downstream
  stage rehashes the monitored wrapper's complete file inventory in addition
  to the runner-owned compact manifest, so changed telemetry, logs, manifests,
  or unrecorded files invalidate the bundle.
  The production-shape concurrency contract is exercised separately by
  **scripts/54_run_oracle_tilt_v4_resource_rehearsal.sh**. Read-only health,
  deterministic family-level selector replay, compact review packaging, and
  the disabled overnight launcher are provided by scripts 55--57. The tracked
  V4 config has `execution_authorized=false`; no source target launches the
  production grid until an independently reviewed flag-only commit. See
  `docs/implementation_notes/oracle_tilt_c095_v4_seed_screen_protocol_20260805.md`.

Install and run the native gates from the repository root:

    make package-install
    make test-native

The pseudo-AL representation augments a loss and is not a response likelihood.
The mean-tilt initializer functions build deterministic fixed-tilt anchors
from Cornish--Fisher skewness approximations and empirical order-statistic
windows. Cornish--Fisher objects now carry moment, probability-window, boundary,
and bootstrap diagnostics so that they are used as first-order near-Normal
anchors rather than exact empirical-window estimators. Nonzero
fixed-response-scale tilt is implemented only for fixed-rate
MCMC targets where the tilt enters as a Gaussian canonical-vector shift:
fixed-design ridge regression, DESN readouts delegated to that ridge kernel,
and RQR-DLM fits with fixed-W or pre-frozen discount-template evolution.
Learned inverse-loss scales, RHS-NS priors, component-scale/adaptive dynamic
evolution, VB/CAVI, and automatic tilt selection remain explicitly gated until
their separate target and propriety contracts are derived and tested.
Static MCMC stores raw exchangeable root labels and, when requested, attempts
post-processing canonicalization of complete root coefficient blocks on a
declared audit design. The canonical coefficient fields are populated only
when that audit passes; otherwise endpoint summaries should use pointwise
sorting through `predict_interval()`. `rqr_canonicalize_root_paths()` provides
the analogous complete-path post-processing diagnostic for DLM root ordinates.
For exploratory single-data examples, use `make oracle-tilt-illustrations-dry-run`
to inspect the fit plan or `make oracle-tilt-illustrations` to run the compact
fixed-rate illustration workflow. Use `make model-illustration-figures` to
regenerate the manuscript-ready fixed-design and RQR-DLM illustration figures
under `figures/generated/`. The paper-figure path uses the declared
four-chain illustration contract, writes deterministic seed, runtime,
chain-summary, maintained-diagnostic, endpoint-error-by-index, and artifact
hash ledgers, and keeps full fit objects under ignored local output roots.
The publication configuration uses standardized asymmetric-Laplace response
innovations with quantile index `0.99` for both families. Its RQR, equal-tail,
and shortest-window tilts are computed from exact population quantiles and
truncated first moments (`uses_cornish_fisher = FALSE`). The fixed-design
illustration retains 2,000 draws per chain; the more autocorrelated DLM
illustration retains 10,000 per chain under its family-specific paper control.
These checks support reproducibility and visual quality for a single
illustrative data set; they are not a coverage-calibration simulation study.
Full fitted objects are not published by the illustration runner. Compact
curves, error summaries, diagnostics, source/runtime state, and artifact hashes
are written under ignored output roots.

The publication-grade replacement is governed by
`docs/implementation_notes/oracle_tilt_c095_publication_protocol_20260731.md`.
Run `make oracle-tilt-publication-preflight` before freezing source. Execute
mode is intentionally fail-closed and must use the exact isolated runtime.
After a hard-passing run, set `ORACLE_TILT_RUN_DIR` and run
`make oracle-tilt-package-evidence`; `make model-illustration-figures` then
renders only from the tracked compact evidence. The six cells share their DGP,
dynamic prior, iteration budget, population-oracle construction, and gate
definitions. An ESS-only warning can support a didactic illustration only when
all hard validity, R-hat, MCSE, conditional-reference, and pathology gates
pass; it is never presented as strict convergence.

The version-2 replacement leaves the preceding run and evidence immutable. It
addresses tail-information and grid-scaling limitations diagnosed in that run:
the innovation index is `0.80`, both families contain at least ten expected
observations in the rarer endpoint tail for every exact-oracle target, and the
DLM uses the exact discretization

```text
G(dt) = [1 dt; 0 1],
W(dt) = [q_l dt + q_s dt^3/3, q_s dt^2/2;
         q_s dt^2/2,          q_s dt]
```

on the fixed physical horizon `[0,1]`. The fixed-design ridge scale and DLM
state/evolution scales are selected by frozen, data-independent
prior-predictive rules. The complete frozen contract is recorded in
`docs/implementation_notes/oracle_tilt_c095_v2_protocol_20260731.md`. Run the
stages in order:

```bash
make test-oracle-tilt-publication-v2
make oracle-tilt-v2-preflight
make oracle-tilt-v2-reference
make oracle-tilt-v2-benchmark
make oracle-tilt-v2-execute
make oracle-tilt-v2-package-evidence \
  ORACLE_TILT_V2_RUN_DIR=application/outputs/.../execute
```

Promotion-grade stages must execute from an isolated package runtime bound to
the complete reviewed `main` SHA. Benchmarking additionally requires
`RQR_ORACLE_TILT_V2_BENCHMARK_CONFIRM=YES`; execution requires a reviewed local
configuration with `execution_authorized=true` and
`RQR_ORACLE_TILT_V2_CONFIRM=YES`. The benchmark binds passing exact-runtime
preflight and reference directories; execution also binds the passing
benchmark directory. Raw chains remain under ignored output storage. No v2
result enters the manuscript until all six family/target cells pass the frozen
computational and recovery gates and the compact evidence packager succeeds.
The fixed-design v2 scan composes two complete exact Gibbs transitions between
its 6,000 retained draws; the DLM scan retains one transition per iteration.
This distinction is explicit in the configuration and fit provenance.
Within a cell, execution uses deterministic two-chain batches and fully reaps
one batch before starting the next, avoiding transient worker overlap. The
shell monitor permits at most three R processes, seven total OS processes, and
eight total threads so short-lived provenance helpers are counted rather than
mislabeled as chain workers.

The prospective version-3 illustration protocol is frozen in
`docs/implementation_notes/oracle_tilt_c095_v3_protocol_20260801.md`. It does
not modify the validated version-2 figures unless its source tests, exact-
runtime preflight, 24 independent conditional references, representative
benchmark, all six model/target cells, compact-evidence packager, and figure
regeneration pass. The response law remains standardized `AL_0.80`, content
remains `0.95`, and all three tilts remain exact population-oracle values; the
new construction changes only the informativeness of the covariate/time
patterns and their prespecified recovery checks. A fail-closed first static
RQR cell exposed inadequate generic initialization for the eight-dimensional
nonlinear target. Its diagnosis and the common target-preserving moment-start
correction are recorded in
`docs/audits/oracle_tilt_c095_v3_static_mixing_reconciliation_20260801.md`.

The completed version-3 execution retained all 27 chains but failed closed at
the final DLM/SH cell: one bulk ESS was 998.79 against 1,000, and the fitted
high-to-low width-contrast error was 20.104% against 20%. The one-shot
adjudication in
`docs/implementation_notes/oracle_tilt_dlm_sh_adjudication_protocol_20260805.md`
recomputes the same five chains to 12,000 retained draws and requires bitwise
identity for each original 6,000-draw prefix. It changes neither the model nor
the gates. A non-strict closeout can request descriptive review, but cannot
authorize automatic figure promotion or another automatic rerun.

The first adjudication execution produced no worker artifact because its
post-fit validator compared a logical storage-contract predicate with a text
label. The bounded replacement is defined in
`config/oracle_tilt_c095_dlm_sh_adjudication_recovery_20260805.json` and
`docs/implementation_notes/oracle_tilt_dlm_sh_adjudication_recovery_protocol_20260805.md`.
It treats the failed job as a software execution rather than a statistical
attempt, validates chain 1 and its bitwise prefix before starting chains 2--5,
and retains every scientific and decision threshold unchanged.

The replacement completed all five 12,000-draw chains, reproduced all 15
original saved-chain prefixes bitwise, used zero numerical repairs, and passed
all 137 maintained diagnostics. Its width-contrast relative error was 0.202623,
just above the original 0.20 threshold. A disclosed review accepted it for the
single-data illustration under a revised tolerance of 0.21; the original
failure remains recorded and is not described as a prespecified strict pass.
The machine-readable decision is in
`config/oracle_tilt_illustration_campaign_registry_20260805.json`, the audit is
`docs/audits/oracle_tilt_c095_v3_revised_promotion_20260805.md`, and the active
compact manuscript evidence is under `figures/data/oracle_tilt_c095_v3/`.
Current `main` permits lightweight audit, rendering, and testing but blocks
further version-3 benchmark, acceptance, execution, and adjudication actions
through `scripts/49_oracle_tilt_campaign_gate.R`.

The tracked high-content forensic configuration is
`config/oracle_tilt_forensics_20260730.json`; it keeps execution disabled.
The exact revised DLM-SH acceptance template is
`config/oracle_tilt_dlm_sh_acceptance_20260730.json`; it is also disabled and
uses one worker so that a constrained host publishes each completed chain
before starting the next. Select either template with
`ORACLE_TILT_FORENSIC_CONFIG=...`.
Run `make oracle-tilt-forensics-preflight` for its non-MCMC geometry checks.
A bounded MCMC execution requires an ignored local copy with
`execution_authorized=true` plus
`RQR_ORACLE_TILT_FORENSICS_CONFIRM=YES`. The forensic workflow never replaces
the manuscript figures automatically, and its traces remain local-only.
Fork-capable systems may set `dlm$workers` for bounded chain parallelism.
Completed worker traces and result envelopes are written atomically and are
resumed only when their source/config contract digest and trace hash match
exactly. The 2026-07-30 reconciliation accepted the fixed-design SH and DLM-ET
checks, diagnosed the original DLM-SH failure as a near-boundary tilt/prior
interaction, and left the revised DLM-SH ESS gate open; see
`docs/audits/oracle_tilt_high_content_forensic_reconciliation_20260730.md`.
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
exact symmetric rootwise partially collapsed scale updates evaluated by a
deterministic C++ Kalman marginal, and fixed 6,000-draw and 9,000-draw
component-scale sentinel schedules. Three slice sweeps per rootwise scale
block were selected in a shortened, development-only four-profile comparison,
and a second exact centered--noncentered ASIS cycle is now the prospective
candidate after one symmetric one-cycle wave still missed fixed scale
diagnostics. The correction also adds a fixed 4,000-draw-per-endpoint M02
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
