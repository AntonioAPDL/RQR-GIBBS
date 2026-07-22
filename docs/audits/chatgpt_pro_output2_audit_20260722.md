# ChatGPT Pro Output 2: Independent Audit and Corrective Implementation

Date: 2026-07-22

Status: corrective implementation complete; production simulation remains gated

Input: local ignored file `Chatgpt_output2`

Input SHA-256: `295fb8f9263e7d793a4baf0ee55e1122753085b46506eb0b56020898fc76af3c`

Input size: 2,577 lines, 94,893 bytes

## Scope and decision

The Pro response was treated as an external technical review, not as an
authoritative patch. Every mathematical and implementation claim was checked
against the source, the manuscript, the cited papers available locally, and
focused counterexamples. The review was substantially correct. Its principal
negative result is accepted: the state-dependent exdqlm-style adaptive
discount recursions are generally incompatible with a single positive smooth
joint density having the two advertised Gaussian root-path conditionals.

The corrective work therefore preserves three distinct contracts:

1. `fixed_W` and a precomputed `discount_template` are fixed Gaussian state
   priors and support exact conditional FFBS for the declared generalized
   posterior, subject to a zero-repair numerical execution.
2. `component_scale` is a new exact hierarchical alternative with fixed SPD
   component templates and shared inverse-Gamma multipliers.
3. `adaptive_discount` remains available only as an experimental
   working/sequential recursion. It is not eligible for a production claim of
   exact Gibbs sampling or for the promotion-grade method set.

No heavy simulation, VB derivation, CAVI implementation, or ELBO work was
performed in this corrective pass.

## Source state

The correction began from these exact states:

| Repository | Branch | Commit | State and role |
|---|---|---|---|
| RQR-GIBBS | `main` | `f40c092521c7d4298d6fefb9295858c4de0145a2` | Clean baseline; only repository changed |
| pinned exdqlm | `feature/rqr-desn-readout-20260716` | `dffb71ee70b597d6a716ee74be1cbc99731cd453` | Clean, read-only implementation reference |
| Q-DESN article | `main` | `f9f22804eff3871bb5350c8add04b7c9f4d4957b` | Clean, read-only style reference |

The final RQR-GIBBS commits are recorded in the handoff section after the
implementation is committed. The protected reference repositories remained
unmodified.

## Mathematical audit

### Population target

For fixed covariates and distinct ordered endpoints `a < b`, direct
differentiation of

```text
E[rho_c{(Y-a)(Y-b)} | X=x]
```

confirms that an interior pointwise stationary solution, under finite second
moment, endpoint continuity, and differentiation assumptions, satisfies both

```text
P(a < Y < b | X=x) = c
E[Y 1(a < Y < b) | X=x] = c E(Y | X=x).
```

The coverage implication is attributable to Pouplin et al.; the conditional
first-moment consequence is identified as an additional result rather than
attributed to the original article. With endpoint atoms, the correct local
minimum statement is

```text
P(a < Y < b | X=x) <= c <= P(a <= Y <= b | X=x).
```

Coincident roots reduce the loss to `c(Y-a)^2` and target the conditional mean,
not a nontrivial interval. Under restricted linear or nonlinear root classes,
the result becomes a pair of design-weighted score equations and does not
guarantee pointwise conditional coverage.

The Pro critique of the original finite-sample unbiasedness argument was also
confirmed. Fitted endpoints are functions of the same observations, so an
argument that treats their induced interval as fixed is not valid without a
separate conditioning, sample-splitting, stability, or empirical-process
result. The manuscript no longer relies on that theorem.

### Generalized target and asymmetric-Laplace augmentation

The following three target modes are now public, canonical, and nonoverlapping:

| Mode | Declared target contribution |
|---|---|
| `fixed_rate` | `exp{-omega_R L(theta)}` |
| `learned_pseudoresidual_normalized` | `lambda^n exp{-lambda L(theta)/s_L}` |
| `learned_pure` | `exp{-lambda L(theta)/s_L}` |

The normalized power is exactly one per observed pseudo-residual; the pure
target power is zero. A user can no longer silently alter the target with a
`power` or `nu` field while retaining a standard label. In fixed mode,
`learning_rate` is now exactly `omega_R`; internally `lambda=s_L omega_R`.
Learned modes use a separate `lambda_initial`.

The normal--exponential mixture constants, the `GIG(1/2,a,b)` latent-scale
conditional, the `b=0` Gamma limit, the reciprocal inverse-Gaussian identity,
and the collapsed Gamma update for `lambda` were rederived and confirmed. The
implemented order

```text
collapsed lambda -> complete v refresh -> root 1 -> root 2
```

is a valid partially collapsed transition. The augmentation normalizes a
kernel in the abstract pseudo-residual; it does not define a response density
for `Y`. Same-data learning of `lambda` is presented as an analyst-declared
hierarchical generalized target, not as an automatically calibrated response
parameter. Fixed-rate sensitivity remains primary.

### Dynamic evolution

The standard fixed-covariance filtering and FFBS equations were independently
checked. The repaired backward covariance issue identified by Pro was real:
the previous code inverted a repaired `R*` but subtracted the unrepaired `R` in
the smoothing and path covariance. Both R and C++ now use `R*` consistently.

The adaptive incompatibility proof was checked directly in the scalar
two-time case. Root 1's advertised conditional has a generically nonzero mixed
derivative with respect to the other root's time-1 state and its own time-2
state, while the corresponding derivative from root 2's advertised
conditional is zero. Equality required by a common twice-differentiable joint
density fails except in degenerate cases such as no discount inflation.

The exact replacement is

```text
W_t(q) = blockdiag(q_1 Q_1t, ..., q_J Q_Jt),
q_j ~ Inverse-Gamma(a_j,b_j),
```

with fixed SPD templates. The same `q_j` is used by both roots to preserve
prior exchangeability. Sampling each time-zero state restores the complete
innovation factorization. For component dimension `d_j`, the implemented
conditional is

```text
q_j | ... ~ Inverse-Gamma(
  a_j + T d_j,
  b_j + 0.5 sum_{k=1}^2 sum_{t=1}^T
              d_kjt' Q_jt^{-1} d_kjt
).
```

The `T d_j` increment correctly combines `T d_j/2` from each of two roots.

## Claim-by-claim code disposition

| ID | Finding | Audit result | Corrective disposition |
|---|---|---|---|
| C1 | Normalized mode allowed arbitrary `lambda` power | Confirmed, high severity | Canonical modes lock powers; public `power`/`nu` are rejected; target formula stored |
| C2 | Fixed `learning_rate` was divided by `s_L` | Confirmed, high severity | Fixed argument now means `omega_R`; learned initialization separated as `lambda_initial` in fixed-design, DESN, and DLM APIs |
| C3 | Backward inverse used repaired `R*`, covariance used `R` | Confirmed, high severity | R and C++ smoothing/path formulas use the same repaired matrix throughout |
| C4 | Backward repairs were omitted from diagnostics | Confirmed | Every filter, backward, and draw repair records stage, time, strategy, jitter, relative jitter, minimum eigenvalue, and clamp count |
| C5 | Mathematical exactness ignored numerical alterations | Confirmed | Fit metadata separates `target_contract`, `exact_joint_target`, repair count, `numerically_exact_transition`, and `promotion_eligible`; fail-fast is default |
| C6 | `Inf`/`NaN` were silently skipped as missing | Confirmed | Only R `NA` denotes missingness; `NaN` and infinities fail in R and C++ |
| C7 | `reference_variance` silently recycled | Confirmed | Length is checked as scalar or `n_time` before expansion |
| C8 | Component metadata accepted malformed dimensions/names | Confirmed | Positive integer dimensions must sum to state dimension; names must match, be nonempty, and unique; fractional dimensions and wrongly sized priors/initial scales fail |
| C9 | Future covariance validation/diagnostics were inadequate | Confirmed | Fixed and future covariance cubes are checked for finiteness, symmetry, and material positive semidefiniteness; forecast repairs are returned explicitly |
| C10 | Extreme GIG behavior was unvalidated | Confirmed as a validation gap | Reciprocal-IG simulation now works on the log scale with stable moderate/large branches; a `10^-300` to `10^300` cross-grid must remain finite and positive |
| C11 | MCMC controls accepted missing/fractional values poorly | Confirmed | One strict scalar-integer validator covers burn-in, retained draws, thinning, seed, progress, and draw counts |
| C12 | Template recursion hid covariance failures | Confirmed | Joseph covariance update, per-time Cholesky gate, repair ledger, and construction audit added |
| C13 | Elementwise nonnegativity was used as covariance test | Confirmed test defect | Tests now use symmetry and eigenvalue/Cholesky criteria |
| C14 | Full state storage default was too large | Confirmed | Full paths are opt-in; terminal state draws are always retained for forecasting |
| C15 | Deterministic root order was not label-symmetric | Confirmed as mixing concern, not target error | A probability-one-half global swap moves all root-specific states and hyperstates and is recorded |
| C16 | `coverage_in_sample` was ambiguous | Confirmed | Replaced by posterior-mean-endpoint coverage, mean draw-wise coverage, and draw-wise coverage quantiles |
| C17 | Provenance and restart state were incomplete | Confirmed, high severity | Versioned schema, package/Git/R/compiler/BLAS/LAPACK/dependency/RNG metadata, SHA-256 object digests, complete Markov state, and exact continuation helper added |
| C18 | Historical sweep wording integrated out the wrong block | Confirmed | Audit wording now states that `v` is integrated out for the collapsed `lambda` draw; roots remain conditioned upon |

## Implementation map

The principal source changes are:

- `application/R/rqr_utils.R`: canonical target modes, locked powers, strict
  integer validation, exact target strings, coverage summaries, RNG helpers,
  provenance, and SHA-256 object digests;
- `application/R/rqr_numerics.R`: explicit fail/repair policy, complete
  factorization diagnostics, covariance-cube validation, and log-domain
  inverse-Gaussian/GIG simulation;
- `application/R/rqr_evolution.R`: exact component-scale specification,
  time-zero conditional, analytic scale conditional, and scale sampler;
- `application/R/rqr_dlm_model.R`: strict exdqlm-compatible model/component
  validation and audited frozen-template construction;
- `application/R/rqr_ffbs.R` and `application/src/rqr_ffbs.cpp`: identical
  repaired-`R*` semantics, strict missing-value contract, and repair ledgers;
- `application/R/rqr_dlm_fit.R`: four evolution contracts, exact component
  scale sweep, compact storage, global swap, provenance, checkpointing,
  continuation, promotion eligibility, and explicit root-forecast diagnostics;
- `application/R/rqr_mcmc_fit.R` and `application/R/rqr_desn_fit.R`: matching
  fixed/learned scale semantics and labels;
- package metadata, exports, generated Rd documentation, READMEs, tests, the
  manuscript, supplement, design note, and bibliography.

The package title now uses “Generalized Bayes Interval-Root Regression with
Gibbs Sampling.” This is factual, distinguishes the method from
interval-censored regression, and keeps “RQR” as historical provenance rather
than forcing “relaxed quantile” as the only descriptive name.

## Validation evidence

The final validation matrix is:

| Gate | Result |
|---|---|
| `make smoke` | Passed on R 4.5.3; required tools and packages present |
| `make pdf` | Passed; final log has no warnings, undefined references, box warnings, or errors |
| `make supplement` | Passed with the same clean-log criteria |
| `make test-native` | Passed: FFBS, model, sampler, target, restart, evolution, and numerical tests |
| pinned `make test-exdqlm-rqr` | Passed all focused algebra, fixed-design, learned-scale, DESN parity, and forecast-contract tests |
| `make literature-manifest` | Passed for the 18 local-only PDFs |
| `make package-check` | `rqrgibbs` 0.1.0.9001, `R CMD check --no-manual`: status OK |
| `git diff --check` | Passed |
| protected repo audit | Both reference repositories clean and unchanged |

Specific new regression evidence includes:

- R/C++ FFBS agreement plus an independent dense Gaussian posterior reference;
- a rank-deficient repaired-`R*` fixture and complete repair records;
- fail-fast versus record-repair behavior;
- explicit `NA` handling and rejection of `NaN`/`Inf`;
- strict fixed/future covariance validation;
- learned target powers and fixed-rate argument semantics;
- extreme `a,b` combinations from `10^-300` through `10^300`, all finite and
  positive;
- analytic component-scale conditional checks and a component-scale end-to-end
  fit with zero repairs;
- a full six-draw chain identical to a three-draw fit followed by exact
  three-draw continuation, including RNG restoration;
- mathematical-target status separated from deliberate numerical repair;
- compact default storage with forecast-capable terminal states.

Generated PDFs, TeX logs, package archives/check trees, compiled objects, fitted
models, and local literature remain ignored. No generated heavy object is
tracked.

## What is solved, and what is not

### Solved

All C1--C18 findings have an implemented and tested disposition. The crucial
target-label bug, fixed-rate scaling bug, repaired-backward inconsistency,
hidden-repair problem, adaptive-discount overclaim, component-scale evolution
gap, and restart/provenance gap are closed at the source and documentation
levels.

The component-scale path is an exact Gibbs construction for its declared
hierarchical state prior when its numerical repair count is zero. The
adaptive-discount path is not “fixed”; it is intentionally and truthfully
classified as a different experimental object.

### Still intentionally open

This audit does not constitute empirical evidence that any RQR method has good
frequentist coverage, calibration, mixing, or predictive performance. Those
claims require the frozen matched simulation protocol and real application.
In particular:

- no production or promotion-grade simulation has run;
- a tiny bounded matrix should still compare collapsed and fully augmented
  updates within Monte Carlo error before learned-scale results are promoted;
- an external trusted GIG implementation was not available on Jerez for an
  independent distributional cross-check. The exact reciprocal identity,
  analytic moment checks, and the extreme finite-positive grid pass, but a
  second implementation remains a desirable validation layer;
- exact checkpoint equality is established on the same software/platform
  stack. Cross-platform bitwise equality is not promised;
- the matched simulation manifest, seeds, Monte Carlo precision targets,
  stopping rules, and failure policy still need to be frozen before a bounded
  pilot;
- VB/CAVI and its ELBO remain a future, separately derived target. They should
  not be inferred by analogy from the Gibbs code.

These are validation and evidence gates, not unresolved defects in the fixed
or component-scale target implementations.

## Next authorized gate

Before any production run, prepare a small deterministic manifest containing:

1. an intercept-only fixed-design case;
2. a local-level dynamic case;
3. a static-limit case;
4. a missing-response case;
5. one learned-scale case with collapsed/fully augmented comparison;
6. one multicomponent exact-scale case;
7. fixed commits, seeds, tolerances, MCSE criteria, repair policy, and expected
   object-schema version.

The gate must use `numerical_policy="fail"`, require zero repair records, keep
all fit objects under ignored local output roots, and exclude
`adaptive_discount` from the exact method set. Only after that bounded gate is
reviewed should the full matched simulation study be launched.

## Commit handoff

Implementation commit: `07e89f1c503ec58b3327e4e2d5e0f92bdd300ffb`

Audit/handoff commit: the commit containing this audit; verify it as the final
`origin/main` commit in the handoff.
