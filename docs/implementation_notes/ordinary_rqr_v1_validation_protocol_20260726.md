# Ordinary RQR version-1 bounded validation protocol

Date: 2026-07-26

Status: source candidate implemented on an isolated branch; package,
reference, benchmark, and bounded release validation remain fail-closed

Implementation contract:
`docs/implementation_notes/ordinary_rqr_v1_contract_20260726.md`

Source state from which development began:
`8e1daf9ea7c2884b47303cf627c64db20e5909a3`

## Decision governed by this protocol

This protocol determines whether ordinary, zero-tilt RQR is implemented
correctly for:

1. native fixed-design regression with ridge, full Gaussian, and native
   RHS-NS priors;
2. native RQR readout conditional on a frozen DESN design; and
3. the already validated exact RQR-DLM modes.

It validates a loss-based generalized posterior and its computation. It does
not validate a response likelihood, a response-simulation distribution,
empirical coverage calibration, or comparative forecasting performance.

A passing result authorizes documentation of the ordinary version-1
implementation and the later design of a separate matched simulation. It does
not authorize that simulation.

## No-heavy-run rule

This document does not authorize:

- the RQR-DLM confirmatory simulation;
- the 24-fit bounded DLM execution;
- any previous DESN broad or article-congruent simulation;
- a new DESN simulation grid;
- a matched fixed-design, DESN, and DLM study;
- nonzero-tilt MCMC;
- VB, CAVI, or ELBO experiments; or
- unattended execution whose prospective fit count is not listed here.

Only deterministic oracles, package tests, tiny exact-continuation fixtures,
and the explicitly bounded diagnostic grid described below may be
implemented. The bounded diagnostic grid must remain disabled until its
preflight, reference bundle, source review, and process limits pass. Creating
this protocol is not execution authorization.

All runtime libraries, logs, chains, temporary builds, and generated designs
must remain under ignored `application/cache/`, `application/logs/`,
`application/runs/`, or `application/outputs/` paths. No fitted object or
generated PDF may be committed.

## Frozen scope

### Accepted target modes

- `fixed_rate`;
- `learned_pseudoresidual_normalized`.

The normalized learned-rate conditional must use

\[
\lambda\mid\Theta,y_{\mathcal O}
\sim
\operatorname{Gamma}\left(
  a_\lambda+n_{\mathcal O},
  b_\lambda+L_c(\Theta;y_{\mathcal O})/s_L
\right).
\]

### Rejected promotion modes

- `learned_pure`;
- any nonzero tilt;
- any VB mode;
- adaptive conditional discounts;
- response-predictive or response-simulation output.

Legacy code may retain these labels, but a fit using one of them must be
ineligible for ordinary-version-1 promotion and must not enter the validation
grid.

## Implemented validation entry points

The implementation provides one tracked configuration, a source-bound runner,
a process-group wrapper and fault suite, and a design-only materializer:

```text
application/config/rqr_ordinary_v1/
  rqr_ordinary_v1_bounded_validation_20260726.R

application/scripts/
  25_validate_rqr_ordinary_v1.R
  26_run_rqr_ordinary_v1_validation.sh
  27_test_rqr_ordinary_v1_monitor.sh
  28_materialize_rqr_ordinary_v1_desn_design.R
```

The runner has exactly four modes:

```text
preflight
reference-only
benchmark-one-cell
execute-bounded
```

`execute-bounded` must fail unless all of the following are supplied:

- the complete 40-character current authorization commit;
- a reviewed implementation commit that is a strict ancestor of that
  authorization commit;
- an object-level proof that the only implementation-to-authorization change
  sets `ordinary_v1_execute_enabled=TRUE` and records the reviewed
  implementation SHA in the frozen config;
- an isolated primary-runtime attestation for that commit;
- the pinned isolated exdqlm runtime and one receipt-v2 D02 design
  materialized from the same current primary source;
- the exact hashed output and process-monitor bundles from the reviewed
  implementation commit's passing `benchmark-one-cell` run;
- the exact hashed `reference-only` bundle from the same runtime and
  toolchain;
- `ordinary_v1_execute_enabled=TRUE` in the reviewed configuration; and
- `RQR_ORDINARY_V1_CONFIRM=YES` in the process environment.

The implementation commit must set `ordinary_v1_execute_enabled=FALSE` and
`reviewed_implementation_commit=NA`. A later authorization commit may change
only those two fields. This avoids the impossible requirement that a Git
commit contain its own SHA.

The benchmark runs at the disabled implementation commit, before the
authorization commit exists. It requires

```text
RQR_ORDINARY_V1_BENCHMARK_CONFIRM=
  I_CONFIRM_ORDINARY_V1_ONE_CELL_BENCHMARK
```

and refuses to run if `ordinary_v1_execute_enabled` is true. The later
authorization commit records that implementation SHA. `execute-bounded`
accepts the benchmark only when its source row names that reviewed SHA, its
source status is `source_candidate_execution_disabled`, its four fits and
diagnostics pass, and both its output and monitor manifests verify. The
`reference-only` bundle used for final authorization is regenerated at the
flag-only authorization commit so its configuration, source, runtime, and
toolchain bytes match the prospective execution.

The Makefile exposes:

```text
materialize-ordinary-v1-desn
preflight-ordinary-v1
reference-ordinary-v1
benchmark-ordinary-v1-one-cell
test-ordinary-v1-monitor
execute-ordinary-v1-bounded
```

No existing DLM execution target may become a dependency of these commands.

## Source and runtime preflight

Preflight must stop unless:

- the primary checkout is clean;
- its branch and complete commit match `RQR_EXPECTED_PRIMARY_COMMIT`;
- an enabled execution config is the exact two-field, config-only delta from
  its strict-ancestor reviewed implementation;
- the executing `rqrgibbs` namespace comes from the isolated archive-built
  runtime for that commit;
- the package source, source package, installation receipt, and executing
  runtime satisfy the current lineage contract;
- the pinned exdqlm source is
  `dffb71ee70b597d6a716ee74be1cbc99731cd453`;
- any exdqlm materializer runs from its isolated attested archive build;
- neither protected reference checkout changes before versus after the run;
- `RNGkind()` and dependency versions are recorded;
- all BLAS and OpenMP thread limits equal one before R starts; and
- the configuration, source, runtime, and fixture digests are complete.

The protected exdqlm checkout must not be compiled, installed, loaded, or
written. The before/after guard must include ignored files.

The wrapper requires a complete `RQR_EXPECTED_PRIMARY_COMMIT` and fresh,
mutually disjoint output, run, and monitor directories. Their optional
overrides are `RQR_ORDINARY_V1_OUTPUT_DIR`, `RQR_ORDINARY_V1_RUN_DIR`, and
`RQR_ORDINARY_V1_MONITOR_DIR`. For every mode except `preflight`, the wrapper
either consumes `RQR_ORDINARY_V1_ATTESTED_DESN_DESIGN_RDS` or discovers
exactly one source-SHA-specific D02 object. Runtime roots and attestations use
`RQR_PRIMARY_RUNTIME_ROOT`, `RQR_EXDQLM_REFERENCE_ROOT`,
`RQR_EXDQLM_RUNTIME_ROOT`, and `RQR_EXDQLM_RUNTIME_ATTESTATION`.

The frozen runner schemas are:

```text
configuration              rqrgibbs_ordinary_v1_validation/1.0.0
compact R evidence         rqrgibbs_ordinary_v1_evidence/1.0.0
process-wrapper closeout   rqrgibbs_ordinary_v1_wrapper/1.0.0
DESN design                rqrgibbs_desn_design/1.0.0
DESN receipt               rqrgibbs_desn_materialization_receipt/2.0.0
DESN receipt status        rqrgibbs_desn_materialization_receipt_status/1.0.0
DESN live verification     rqrgibbs_desn_materialization_verification/1.0.0
DESN fit                   rqrgibbs_desn_fit/1.1.0
DESN future design         rqrgibbs_desn_future_design/1.1.0
DESN future verification   rqrgibbs_desn_future_verification/1.0.0
```

Every R-generated CSV is atomically written, read back, and assigned the
compact-evidence schema unless it already carries a more specific schema.
The materializer publishes only validated regular files through a
same-directory rename, rejects symbolic-link and nonregular destinations, and
never unlinks prior evidence before replacement.

## Deterministic seed ledger

The following seeds are fixed. They may not be replaced after observing a
failure.

| Purpose | Seed or seeds |
|---|---:|
| Static deterministic fixture generation | `82601` |
| Ridge fixed-rate chains | `82611:82614` |
| Ridge learned-rate chains | `82621:82624` |
| Full-Gaussian fixed-rate chains | `82631:82634` |
| Full-Gaussian learned-rate chains | `82641:82644` |
| RHS-NS sampled-shoulder fixed-rate chains | `82651:82654` |
| RHS-NS sampled-shoulder learned-rate chains | `82661:82664` |
| RHS-NS fixed-shoulder fixed-rate chains | `82671:82674` |
| RHS-NS fixed-shoulder learned-rate chains | `82681:82684` |
| Frozen DESN materialization | `82701` |
| Frozen DESN ridge fixed-rate chains | `82711:82714` |
| Frozen DESN ridge learned-rate chains | `82731:82734` |
| Frozen DESN RHS-NS fixed-rate chains | `82721:82724` |
| Frozen DESN RHS-NS learned-rate chains | `82741:82744` |
| Static continuation checks | `82801:82808` |
| DESN continuation checks | `82821:82824` |
| Attested D02 end-to-end reference cells | `82831:82834` |
| Future-design draw selection | `82901` |
| Pinned-reference RHS-NS parity | `82921` |
| Representative BENCH01 chains | `82961:82964` |

The configuration must materialize a one-row-per-use seed ledger and reject
duplicates. Deterministic integrations and algebraic oracles must record
`NA` rather than consuming a nominal seed. The shell-only process-monitor
fixtures are deterministic, do not invoke R or an RNG, and therefore consume
no seed-ledger rows.

## Fixed fixtures

### F01: intercept-only normalized learned rate

Reuse the established twelve-observation fixture:

```r
y <- c(
  -2.0, -1.3, -0.8, -0.4, -0.1, 0.1,
   0.35, 0.7, 1.1, 1.6, 2.2, 3.0
)
X <- matrix(1, 12, 1, dimnames = list(NULL, "intercept"))
```

Set:

```text
coverage_level = 0.80
s_L            = 1
beta variance  = 25
lambda prior   = Gamma(shape=4, rate=4)
```

The corrected tracked quadrature references remain authoritative for this
exact fixture. The new native static sampler must reproduce the established
means and five event probabilities within four maintained Monte Carlo
standard errors if the bounded grid is later authorized. The
`reference-only` mode must first reproduce the deterministic quadrature
values and their artifact hashes without running MCMC.

### F02: full-Gaussian regression with missing responses

Use nine rows and three named columns:

```r
x <- seq(-1, 1, length.out = 9)
z <- c(-1, 1, 0, -0.5, 0.5, 1.5, -1.5, 0.25, -0.25)
X <- cbind(intercept = 1, x = x, z = z)
y <- 0.4 - 0.7 * x + 0.25 * z +
  c(-0.15, 0.05, 0.10, -0.05, 0, 0.08, -0.12, 0.03, 0.06)
y[c(3, 7)] <- NA_real_
```

Use the shared root prior

\[
\beta_k\sim N_3(m,Q^{-1}),
\quad
m=(0.2,-0.1,0.3)^\top,
\]

with

\[
Q=
\begin{pmatrix}
2.0&0.3&-0.1\\
0.3&1.5&0.2\\
-0.1&0.2&1.2
\end{pmatrix}.
\]

Run both accepted rate modes. This fixture supplies the dense Gaussian
root-conditional oracle and the public missing-response contract.

### F03: ridge/full-Gaussian equivalence

Use the complete version of F02 and compare:

```text
ridge tau2 = 4
Gaussian mean = 0
Gaussian precision = 0.25 I
```

With the same initial state and RNG state, all transition draws must be
bitwise identical. Provenance and prior-type labels may differ.

### F04: native RHS-NS, sampled shoulder

Use ten rows and four named columns:

```r
x1 <- seq(-1, 1, length.out = 10)
x2 <- cos(seq_len(10) / 3)
x3 <- sin(seq_len(10) / 4)
X <- cbind(intercept = 1, x1 = x1, x2 = x2, x3 = x3)
y <- 0.2 + 0.6 * x1 - 0.25 * x2 + 0.1 * x3
y[c(4, 8)] <- NA_real_
```

Use:

```text
intercept_name      = "intercept"
intercept_mean      = 0
intercept_precision = 0.04
tau0                = 0.7
a_zeta              = 2.5
b_zeta              = 1.3
zeta2_fixed         = NULL
```

Run both accepted rate modes.

### F05: native RHS-NS, fixed shoulder

Reuse F04 with `zeta2_fixed=3`. Run both accepted rate modes. The shoulder
must remain bitwise fixed through fitting, label swaps, checkpointing, and
continuation.

### D01: frozen DESN readout

Construct one small deterministic design with:

- twelve aligned training rows;
- one verified constant-one intercept;
- two reservoir-derived feature columns;
- strictly increasing time indices;
- a fixed reservoir/configuration digest;
- a fixed builder identity and version;
- a one-step-lag causal declaration; and
- terminal-state and lag-buffer digests.

Create a missing-response variant at training rows 3 and 9. Fit ridge, full
Gaussian, and RHS-NS readouts by passing the frozen design to `rqrgibbs`.
Given the same design, target, prior, initial state, and RNG state, every
readout draw must equal a direct `rqr_mcmc_fit()` draw.

D01 is a deterministic package-level reference fixture. It exercises the
native frozen-design wrapper without loading exdqlm, but it cannot stand in
for an attested materialization in a promotion run.

### D02: pinned exdqlm materializer

Use a minimal reservoir configuration and seed `82701`.
`application/scripts/28_materialize_rqr_ordinary_v1_desn_design.R`
materializes the design once from exact isolated primary and pinned exdqlm
runtimes. It writes only below ignored cache/output roots under evidence
schema `rqrgibbs_ordinary_v1_desn_materialization/1.0.0`; it cannot fit a
readout, run MCMC or VB, or simulate a response. The receipt-v2 object
explicitly records:

- pinned package version and source commit/tree digest;
- isolated runtime-tree digest;
- runtime-attestation schema and SHA-256;
- materializer-argument digest;
- materialized-design-payload digest; and
- runtime-source-match and reproducibility-eligibility flags.

The tracked configuration and compact materialization evidence retain the raw
effective arguments, materializer seed, and response-history digest. The
receipt's argument digest binds those effective arguments without duplicating
them. Its payload digest covers the complete serialized design payload,
including the response and feature matrix, feature schema and order, aligned
time indices, builder and reservoir declarations, driver and causal
declarations, time contract, and terminal-state metadata.

`fit_readout=FALSE` must not bypass attestation. Metadata overrides must not
replace protected builder, source, runtime, argument, or reservoir fields.
A supplied hand-constructed design may be used for tests, but is not
promotion eligible unless its materialization provenance satisfies the same
contract.

All sixteen bounded DESN fits use this one immutable D02 materialization.
The materializer seed is included in the effective-argument digest. Before a
fit, the runner re-verifies the serialized receipt against the currently
executing pinned isolated exdqlm runtime and binds that external state into
the native fixed-design fit provenance.

The supported command is:

```bash
RQR_EXPECTED_PRIMARY_COMMIT="$(git rev-parse HEAD)" \
  make materialize-ordinary-v1-desn
```

For `reference-only`, `benchmark-one-cell`, and `execute-bounded`, the process
wrapper discovers the unique design for that exact source SHA. An explicit
`RQR_ORDINARY_V1_ATTESTED_DESN_DESIGN_RDS` may be supplied instead; zero or
multiple implicit candidates fail closed.

### D03: future frozen DESN designs

Use two future rows and validate separately:

- an origin-fixed precomputed design;
- rolling one-step teacher-forced evaluation;
- an origin-fixed external-driver path.

The precomputed and external-driver modes must not use realized post-origin
responses. Teacher-forced output must be labeled rolling one-step evaluation,
not an origin-fixed forecast. Future feature names, order, intercept,
reservoir metadata, parent digest, time ordering, and driver-path digest must
be immutable.

An explicitly named `X_future` with the exact parent feature order remains a
legacy convenience only and is not promotion eligible; unnamed matrices are
rejected. Promotion evidence must use a validated future-design object.

## Reference-only gates

### Loss and augmentation

- Evaluate \(L_c\) directly at fixed root values and compare with an
  independently coded sign-partition implementation.
- Verify the pseudo-AL kernel equality up to constants independent of the
  sampled block.
- Verify the latent GIG parameters at negative, zero, and positive
  pseudo-residual products.
- Verify that the normalized learned-rate power is exactly
  \(n_{\mathcal O}\).
- Reject any scan that updates observed latent scales before a new collapsed
  \(\lambda\) draw but does not refresh them afterward.

### Gaussian root conditionals

For F02, hold the other root, latent scales, and learning rate fixed. Assemble
the conditional precision and information vector independently. Require:

```text
maximum absolute mean error       <= 1e-12
maximum absolute covariance error <= 1e-12
```

Repeat for the ridge and full-Gaussian parameterizations in F03.

### RHS-NS hierarchy

For F04 and F05:

- compare the coefficient canonical precision to the two normalized Gaussian
  factors in the declared joint kernel;
- compare every inverse-Gamma shape and rate with an independent scalar
  calculation;
- compare log-kernel ratios with the corresponding full-conditional ratios;
- reproduce one full hyperstate sweep under seed `82921`;
- compare the mapped native transition with the pinned reference behavior;
- verify the named intercept is unshrunk;
- reject a missing, duplicated, nonconstant, or ambiguously positioned
  intercept;
- reject stochastic underflow or overflow rather than silently clipping; and
- verify that two roots retain independent hyperstates.

Reference parity is secondary to the explicit joint-kernel oracle. A
disagreement must be investigated; the pinned implementation is not accepted
as proof by itself.

### Missing responses

For F02 and F04:

- the loss equals the loss computed on observed rows only;
- the learned-rate shape uses the observed count;
- root precision and information omit missing rows;
- no random latent draw is consumed for a missing row;
- the observed-site RNG stream is unchanged by missing-row storage;
- leading, interior, trailing, and multiple missing patterns pass;
- complete-data behavior remains unchanged; and
- an all-missing response is rejected.

The canonical data object and every derived field must be reconstructed during
continuation. Mutating `observed_index`, `n_observed`, row identifiers, or
column order must be rejected even if an attacker recomputes an ordinary
outer digest.

### Exact continuation

For each of the eight fixture-by-rate cells

```text
F02 Gaussian     x {fixed, normalized learned}
F03 ridge        x {fixed, normalized learned}
F04 RHS sampled  x {fixed, normalized learned}
F05 RHS fixed    x {fixed, normalized learned}
```

compare one uninterrupted six-transition run with `2+2+2` continuation.
Store latent and prior-state draws for this gate. Require bitwise identity for:

- both coefficient sequences;
- \(\lambda\);
- every observed latent scale;
- both complete RHS-NS hyperstates, when present;
- final RNG state;
- final checkpoint;
- completed-iteration count; and
- every stochastic output field.

RHS-NS `update_count` must continue cumulatively; reconstructing a checkpoint
through an initializer that resets this count is a failure.

Mutation tests must alter and, where relevant, rehash:

- each coefficient block;
- \(\lambda\);
- an observed latent scale;
- every prior hyperstate field;
- RNG state;
- completed iterations using fractional, negative, infinite, and overflow
  values;
- data, observed mask, design, prior, target, and embedding digests;
- generation-zero and generation-one history facts; and
- resolved backend and environment override fields.

Every semantically impossible history must be rejected after recomputing its
ordinary SHA-256 digest.

Continuation-only fields are private to the validated continuation path.
`rqr_mcmc_fit()` must reject a user-supplied continuation marker, parent
history, parent checkpoint, completed-iteration count, or inherited repair
status. `rqr_mcmc_continue()` may reach the internal worker only after
checkpoint and history validation and through the package-private
continuation token. The public fit and continuation interfaces must reject
unknown control names and ambiguous alias pairs rather than silently ignoring
them.

### Complete root swaps

Under swap probabilities 0, 0.5, and 1:

- consume the documented number of uniform variates;
- exchange coefficient and complete prior-state blocks together;
- retain label-invariant endpoints and losses;
- preserve fixed shoulders; and
- attribute numerical-repair records to the block on which the repair
  occurred, not to a post-swap label.

### DESN design and readout

- Validate every component and semantic digest.
- Reject protected materializer metadata overrides.
- Reject an unattested design-only reference materialization.
- Require exact direct-static/readout draw equality for D01.
- Require exact continuation for ridge and RHS-NS DESN readouts.
- Reject design, parent-link, terminal-state, lag-buffer, reservoir, feature,
  or time mutations.
- Require full future reservoir metadata equality with the parent, not only
  equality of one nested digest field.
- Require origin-fixed semantics for `precomputed_design`.
- Reject response-simulation language and boolean authorization at every
  public boundary.
- Validate a supplied noninteger horizon before testing whether a future
  design is present.

## Bounded diagnostic grid

The prospective grid contains:

```text
4 static prior fixtures x 2 rate modes x 4 chains = 32 fits
2 DESN readout priors  x 2 rate modes x 4 chains = 16 fits
total                                             = 48 fits
```

Here the four static prior fixtures are ridge, full Gaussian, sampled-shoulder
RHS-NS, and fixed-shoulder RHS-NS. The two DESN priors are ridge and RHS-NS.
All DESN grid cells use D02; D01 remains a direct-wrapper reference fixture.
The origin-fixed two-row future design for D02 is a deterministic, versioned
mechanics-only extension of its last feature row, not a recursive response
forecast or empirical forecasting design. These fits are mechanics and mixing
checks, not a simulation study.

Freeze:

```text
coverage_level          = 0.80
fixed learning rate     = 1
loss reference scale    = 1
lambda prior            = Gamma(4,4)
numerical policy        = fail
root-swap probability   = 0.5
chains                  = 4
burn-in                 = 1000
retained draws          = 3000
thinning                = 1
latent draw storage     = false
prior-state storage     = RHS-NS chains only
execution order         = sequential
```

The four initialization profiles vary only quantities that survive until a
root or RHS-NS update:

| Profile | Root midpoint shift | Root half-width | RHS scale multiplier |
|---|---:|---:|---:|
| `low_wide` | -1.50 | 2.50 | 0.50 |
| `high_wide` | 1.50 | 2.50 | 2.00 |
| `low_narrow` | -0.75 | 0.50 | 4.00 |
| `high_narrow` | 0.75 | 0.50 | 1.00 |

The RHS multiplier is applied only to RHS-NS cells; the manifest records it as
missing for ridge and Gaussian cells. `lambda_initial` is not a profile
dimension because the normalized learned-rate scan replaces it with its first
collapsed draw, and fixed-rate scans fix the rate by contract. The runner uses
the required public-API placeholder `lambda_initial = 1` and does not place a
lambda or latent-scale value in the fit initialization object. A single
constructor builds that exact object for both execution and evidence
generation. `initialization_manifest.csv` records whether RHS states were
initialized, whether prior-state draws were retained, and a SHA-256 digest of
the exact object passed to the fit, including both root-specific RHS-NS states.
This avoids claiming dispersion in values that are refreshed before use.

The future runner must diagnose each four-chain cell before starting the next
cell and stop on the first failure. No seed replacement, additional draws,
or tuning change is allowed after results are observed. A protocol revision
and new review are required instead.

Primary label-invariant estimands are:

- ordered roots at every design row;
- midpoint and width at every design row;
- total observed-data loss;
- `log(lambda)` in learned mode;
- selected coefficient linear functionals;
- `log(tau2)`, `log(zeta2)`, and selected `log(lambda2_j)` for RHS-NS; and
- future root, midpoint, and width functionals for validated DESN future
  designs.

Use `posterior (>= 1.7.0)` for:

```text
rank-normalized R-hat <= 1.01
bulk ESS             >= 1000
tail ESS             >= 1000
MCSE                  = posterior::mcse_mean()
```

Fixed learning rates and fixed shoulders are checked by exact identity and
must not be sent to stochastic diagnostics. Root-swap counts are sidecars, not
primary evidence of mixing.

Every fit must have:

- finite retained draws;
- zero numerical repairs;
- an exact fixed-joint target;
- an intact checkpoint and continuation history;
- a matching isolated primary runtime;
- no forbidden target mode; and
- no response-prediction claim.

Per-fit provenance is family-specific. Every fit requires the exact isolated
primary runtime. Static cells must record an empty required-external set and
must not inherit exdqlm merely because the same runner also validates DESN
cells. DESN cells must require exactly `exdqlm`, record its isolated runtime
digest and source match, and retain a valid D02 materialization receipt and
live external-state binding. A fixed-design fit that requires exdqlm, or a
DESN fit that does not, fails.

Again, the 48-fit grid remains disabled under this initial protocol.

## DLM compatibility and protected-regression gate

Ordinary static/DESN work should not alter the DLM target or transition
without a separately reviewed statistical change. The following files remain
protected because they contain, configure, or launch the validated DLM path:

```text
application/R/rqr_dlm_fit.R
application/R/rqr_dlm_model.R
application/R/rqr_evolution.R
application/R/rqr_ffbs.R
application/R/rqr_utils.R
application/R/rqr_numerics.R
application/src/rqr_ffbs.cpp
application/config/rqr_dlm/rqr_dlm_bounded_dynamic_fixtures_20260723.R
application/config/rqr_dlm/rqr_dlm_main_simulation_20260724.R
application/config/rqr_dlm/rqr_dlm_main_simulation_preliminary_20260724.R
application/config/rqr_dlm/rqr_dlm_main_simulation_preliminary_methods_20260724.csv
application/config/rqr_dlm/rqr_dlm_main_simulation_preliminary_scenarios_20260724.csv
application/config/rqr_dlm/rqr_dlm_output13_bounded_expected_bundle_20260724.json
application/scripts/15_run_rqr_dlm_confirmatory_simulation.R
application/scripts/15_run_rqr_dlm_confirmatory_simulation.sh
application/scripts/17_launch_rqr_dlm_confirmatory_wave.R
application/DESCRIPTION
application/NAMESPACE
application/R/RcppExports.R
application/src/RcppExports.cpp
application/src/rqr_interweave.cpp
application/src/Makevars
application/src/Makevars.win
```

Before merge, record a baseline-versus-candidate table containing:

- file SHA-256 for every protected DLM source/config/launcher;
- formals and body digests for each exported DLM constructor, fit,
  continuation, and forecast function;
- `rqrgibbs_fit/1.11.0`;
- `rqrgibbs_continuation_history/4.1.0`;
- R/C++ registration symbols; and
- the active confirmatory authorization state.

Generate that evidence with the read-only boundary auditor. It uses
`git cat-file` and `git ls-tree`; it does not check out either revision:

```bash
Rscript application/scripts/29_audit_rqr_ordinary_v1_dlm_boundary.R \
  --repo-root /absolute/path/to/RQR-GIBBS \
  --baseline BASELINE_FULL_SHA \
  --candidate CANDIDATE_FULL_SHA \
  --output-dir application/cache/ordinary_v1_dlm_boundary/CANDIDATE_FULL_SHA \
  --promotion YES
```

Promotion mode requires full 40-character inputs, a clean repository at the
candidate `HEAD`, and a strict ancestral baseline. During implementation only,
`--candidate WORKTREE --promotion NO` is permitted. Such output is labeled
`development_nonpromotion_evidence` and cannot authorize a merge, benchmark,
or bounded execution. An output directory inside the repository must be
ignored; an external temporary directory is also accepted.

The auditor emits compact, schema-tagged comparisons for all 23 exact file
bytes and SHA-256 values, the 18 public DLM function formals and bodies,
schema strings, every NAMESPACE directive, compiled registration symbols,
DESCRIPTION metadata, and confirmatory-authorization fields. It also writes a
summary, source-state metadata, and an artifact-hash manifest using
validated same-directory atomic renames. This is a boundary-difference
inventory, not statistical validation: the evidence states that no fit,
simulation, external-repository mutation, or target-validation claim was
performed.

Target, augmentation, FFBS, evolution, component-scale, and confirmatory-
launcher entries must be identical unless the change is explicitly classified
as a DLM statistical change and the stronger rerun rule below is followed. A
bounded public-method correction---for example, stricter validation of stored
fitted-root draws---must be enumerated rather than hidden as
``noninterference.'' It requires regenerated protected hashes and the complete
protected DLM regression matrix before prior evidence can be cited; it does
not, by itself, imply that the target or transition changed.

The frozen configuration binds the exact ordered inventory above, including
the two shared R helpers, rather than accepting an arbitrary replacement list
with the same length. Changes to DESCRIPTION, NAMESPACE, RNG helpers,
provenance, schemas, or serialization are also shared-infrastructure changes
even if the edited lines appear static-specific. The preferred ordinary
implementation is additive:

- keep `rqr_beta_prior()` as the preferred closure-free native constructor;
- retain `beta_prior()` as a documented compatibility boundary for the three
  native prior contracts, while rejecting the unsupported generic exdqlm
  `rhs` path explicitly rather than loading the protected namespace;
- leave obsolete private adapters unused rather than editing shared DLM
  utilities; and
- add static schemas without replacing DLM schemas.

If the candidate retains a shared-infrastructure or bounded DLM method-
boundary change, this protocol cannot transfer prior DLM evidence by
assertion. Before merge it must rebuild the exact isolated runtime and pass:

- all native model, FFBS, sampler, and history-mutation tests;
- all six DLM `6=2+2+2` continuation cells;
- R/C++ FFBS parity;
- missing and future-state references;
- component-scale and interweaving references;
- package check;
- monitor and fail-closed launcher tests; and
- current M01, M02, horizon, and M03 correction gates.

If a target, transition, FFBS, evolution, or component-scale function changes,
the complete 24-fit bounded DLM validation and a new independent audit are
required. Those reruns are outside the authorization supplied by this
protocol.

The active confirmatory simulation must not be stopped, restarted, or have its
source/runtime changed as a side effect of ordinary-version-1 development.

## Package-integration gates

Before package validation:

1. Generate NAMESPACE and Rd files from the reviewed roxygen source.
2. Export the intended public functions:

   ```text
   rqr_beta_prior
   rqr_mcmc_continue
   rqr_desn_design
   rqr_validate_desn_design
   rqr_desn_future_design
   rqr_validate_desn_future_design
   rqr_desn_continue
   ```

3. Retain the existing S3 registrations for `rqr_mcmc` and `rqr_desn_fit`.
4. Test exported functions through the public namespace; reserve `:::` for
   explicit internal-oracle tests.
5. Bump the development package version after the API and schemas are frozen.
6. Keep exdqlm in `Suggests`, not `Imports`; a supplied frozen design must run
   without loading exdqlm.
7. Add an ordinary-version-1 Makefile test target without changing any DLM
   execution target.
8. Verify that `application/R/README.md` identifies native `rqrgibbs` code as
   the inference authority and confines exdqlm to the optional, isolated DESN
   design-materialization role.
9. Reject stale Rd pages that still describe native RHS-NS as an exdqlm
   adapter or list excluded modes as version-1 options.

Required checks are:

```text
parse every application/R source file
install from a clean source-package build
run native ordinary-version-1 tests
run native DLM regression tests
run standalone DLM contract tests
R CMD check --no-manual
make smoke
make pdf
make supplement
git diff --check
```

No test should pass solely because it calls an exdqlm implementation instead
of the native `rqrgibbs` function under test.

## Resource and failure contract

The runner must:

- set thread limits before R starts;
- execute in a process group or cgroup;
- install `EXIT`, `INT`, `TERM`, and `HUP` cleanup handlers;
- enforce a hard wall-time ceiling of 45 minutes for the entire bounded grid;
- run sequentially;
- sample process-tree RSS, process count, and thread count;
- label sampled maxima as telemetry rather than kernel-hard peaks;
- write one structured failure record before exit;
- perform a final process-group sweep; and
- never retry a failed cell automatically.

The provisional ceilings are:

```text
wall time       45 minutes
process count   3
thread count    4
artifact bytes  1 GiB under the ignored run directory
```

A representative one-cell benchmark must pass before the disabled execution
flag can be reviewed. BENCH01 is exactly four chains of the D02 DESN readout
with the fixed-shoulder RHS-NS prior, normalized learned rate, and seeds
`82961:82964`. It exercises the full bounded schedule and the same attested-
runtime, D02 materialization, future-root, diagnostic, serialization, and
process-group-monitor contracts later enforced by execution. Its evidence is
source-bound to the reviewed, execution-disabled implementation commit;
execution separately revalidates a design and runtime for the flag-only
authorization commit. BENCH01 is a resource and mechanics benchmark, not one
of the 48 validation fits and not a scientific pilot.

The benchmark requires the exact confirmation

```text
RQR_ORDINARY_V1_BENCHMARK_CONFIRM=
  I_CONFIRM_ORDINARY_V1_ONE_CELL_BENCHMARK
```

while the tracked execution flag remains false. Its R-output bundle must
contain and hash `run_status.csv`, `source_state.csv`,
`runtime_attestations.csv`, `fit_plan.csv`, `fit_plan_status.csv`,
`bounded_diagnostics.csv`, `provenance_checks.csv`,
`desn_future_checks.csv`, and `local_chain_hashes.csv`. The corresponding
monitor directory must contain `process_group_resource_summary.csv`,
`wrapper_closeout.csv`, and `wrapper_artifact_hashes.csv`. Final execution
reads those directories from `RQR_ORDINARY_V1_BENCHMARK_DIR` and
`RQR_ORDINARY_V1_BENCHMARK_MONITOR_DIR`, rehashes the required bytes, and
requires four passing fits, passing modern diagnostics, a promotable parent
fit based on the attested training design, verified future-contract evidence
that remains explicitly nonpromotable because future-row materialization is
not separately attested, exact runtime provenance, passing resource rows, and
an empty final process group.

If BENCH01 reaches a ceiling, revise the protocol rather than increasing the
limit during execution.

The shell-only fault suite must independently exercise normal completion,
runner failure, TERM-to-KILL timeout cleanup, cumulative artifact overflow,
process-count overflow, aggregate-thread-count overflow, sampled-telemetry
failure, and an externally delivered TERM. Each failing fixture must preserve
regular atomic wrapper evidence, record the specific gate that failed, leave
the monitored process group empty, and leave an unrelated sentinel process
alive. The process and thread fixtures set the other ceiling high enough to
show that the intended gate, rather than a correlated limit, caused failure.
These deterministic fixtures do not authorize the benchmark or bounded fits.

## Artifact contract

### Tracked compact evidence

A successful validation should promote only compact files under a dated
`docs/audits/` directory:

```text
source_state.csv
runtime_attestations.csv
validation_config_digest.csv
fixture_manifest.csv
seed_ledger.csv
reference_gates.csv
oracle_comparisons.csv
missingness_checks.csv
rhs_ns_conditional_checks.csv
continuation_checks.csv
history_mutation_checks.csv
desn_design_checks.csv
desn_future_checks.csv
protected_dlm_hashes.csv
package_checks.csv
fit_plan.csv
initialization_manifest.csv
fit_plan_status.csv
bounded_diagnostics.csv
compact_posterior_summaries.csv
checkpoint_manifest.csv
provenance_checks.csv
root_swap_sidecar.csv
local_chain_hashes.csv
resource_summary.csv
failure_log.csv
run_status.csv
session_info.txt
artifact_hashes.csv
closeout.md
```

Every CSV requires an explicit schema identifier. `artifact_hashes.csv` must
list relative path, byte count, and SHA-256 for every other promoted artifact.
The closeout must state the precise claim supported and the claims not
supported. Benchmark authorization additionally binds the three compact
process-wrapper files listed above; raw monitor samples remain ignored.
The initialization manifest has the exact columns
`cell_id`, `chain`, `seed`, `fixture_id`, `prior_id`,
`learning_rate_mode`, `profile`, `n_features`, `midpoint_shift`,
`initial_half_width`, `rhs_prior_state_initialized`,
`rhs_scale_multiplier`, `prior_state_draws_retained`,
`initial_state_digest`, and `design_digest`, after the common
`schema_version` column. It contains no `initial_lambda` field.

### Ignored evidence

The following remain local:

```text
full chains
full latent draws
full prior-state draws
installed package libraries
source-package archives
build and install logs
monitor sample streams
temporary quadrature objects
reference DESN shells and reservoir states
generated PDFs
```

Compact evidence must record hashes for any ignored chain or reference object
on which a reported gate depends. Files must be written to temporary paths,
read back and validated, then atomically renamed.

## Pass rule

The ordinary version-1 implementation passes only if:

- every preflight and reference gate passes;
- all public static and DESN contracts are package wired;
- the two accepted rate modes are the only promoted modes;
- ridge, full Gaussian, and native RHS-NS targets pass their independent
  oracles;
- missing responses are omitted exactly;
- all static and DESN continuation cells are bitwise exact;
- all checkpoint, history, design, and future-link mutations are rejected;
- the native static/DESN path does not call private exdqlm sampling functions;
- DESN materializer lineage is attested rather than asserted;
- numerical repair count is zero;
- the protected DLM compatibility and regression gate passes;
- package and manuscript checks pass; and
- the complete compact artifact bundle verifies.

Any failed or missing gate is a no-go. A no-go must preserve its failure
artifacts and must not trigger a larger run.

## Initial review findings and implementation disposition

The initial source review identified the following integration issues. The
isolated implementation branch now resolves each in source and focused tests;
the final release claim still requires the exact-runtime validation matrix:

1. Public exports and roxygen source are package-wired; generated help and
   package checks remain part of the release gate.
2. `application/R/README.md` identifies `rqrgibbs` as the native authority.
3. Shared-helper edits trigger the complete protected-DLM hash and regression
   matrix.
4. Receipt v2 records lineage and argument/payload digests, and live
   verification rechecks them against the executing isolated runtime.
5. Protected materializer fields cannot be overridden.
6. Fits retain the frozen data-only design contract, not a heavy reference
   model shell.
7. Future designs bind parent, reservoir, causal, time and driver semantics.
8. Public horizon validation and legacy-matrix nonpromotion are tested.
9. Continuation preserves the complete RHS-NS state and update count.
10. Repair labels are fixed before any complete root-state swap.
11. Checkpoints reconstruct the complete canonical data contract.
12. `learned_pure` and VB are explicitly outside ordinary-v1 promotion.

These are source dispositions, not substitutes for the pending exact-runtime
package, manuscript, reference-only, BENCH01, and 48-fit execution evidence.
