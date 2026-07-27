# Ordinary RQR version-1 implementation contract

Date: 2026-07-26

Status: frozen implementation and acceptance boundary

Source state audited: `8e1daf9ea7c2884b47303cf627c64db20e5909a3`

Working branch: `codex/ordinary-rqr-v1-20260726`

## Purpose

This document defines the smallest complete version-1 implementation of
ordinary, zero-tilt RQR across three predictor classes:

1. fixed-design regression;
2. a frozen-design DESN readout; and
3. dynamic linear root states.

The inferential object is a generalized posterior formed from the RQR loss. It
is not an ordinary response-likelihood posterior. Its draws describe two root
functions and the induced ordered interval. They are not posterior-predictive
response draws.

The contract separates four questions:

- the target that must be sampled;
- the model-specific representation of each root;
- the algorithms and stored state required for exact continuation; and
- the evidence required before an implementation is described as complete.

The contract was frozen against the source state named above and is updated
below with the implementation-branch disposition. A source implementation is
not a release claim: the isolated-runtime reference suite, representative
four-chain benchmark, bounded 48-fit mechanics grid, package check, and
protected DLM regression gates must still pass before promotion. The
source-bound validator implements four distinct modes: `preflight`,
`reference-only`, `benchmark-one-cell`, and `execute-bounded`. The final mode
remains disabled in the source candidate.

## Version-1 boundary

### Included

| Area | Required version-1 support |
|---|---|
| Loss | Ordinary RQR loss, equivalently the zero-tilt member of the larger family |
| Learning rate | Positive fixed rate and normalized learned inverse-loss scale |
| Fixed design | Ridge, full Gaussian, and native RHS-NS coefficient priors |
| Missing responses | Exact omission through a declared observed-data mask |
| Static continuation | Exact segmented continuation with integrity validation |
| DESN | Frozen-design RQR readout built from an attested exdqlm reference materializer |
| DLM | Fixed \(W_t\), pre-frozen discount templates, and shared component-scale evolution |
| Computation | Exact blocked Gibbs transitions for the declared fixed joint targets |
| Forecast output | Root and ordered-interval functionals conditional on an explicit future design/evolution contract |

### Excluded

The following objects are outside ordinary RQR version 1:

- nonzero mean tilt;
- `learned_pure`, because it omits the normalized per-observation scale
  contribution;
- variational Bayes, CAVI, and ELBO implementations;
- online or conditionally adaptive discount recursion;
- a response-simulation distribution or response-predictive samples;
- automatic selection or calibration of the learning rate from empirical
  coverage;
- a native DESN reservoir generator;
- stochastic future DESN inputs unless a separate, explicit driver supplies
  the future design;
- matched or production simulation claims beyond the evidence actually run.

Excluded modes may remain in exploratory or compatibility code, but they must
either be rejected by the version-1 entry points or return objects that are
explicitly non-promotable. They must not appear in version-1 promotion
evidence.

## Common ordinary-RQR target

Let \(\mathcal O=\{i:y_i\ \text{is observed}\}\) and
\(n_{\mathcal O}=|\mathcal O|\). For two root functions
\(\eta_{1i}\) and \(\eta_{2i}\), define

\[
e_i=(y_i-\eta_{1i})(y_i-\eta_{2i}), \qquad
\rho_c(u)=u\{c-\mathbbm 1(u<0)\},
\]

and

\[
L_c(\eta_1,\eta_2;y)
  =\sum_{i\in\mathcal O}\rho_c(e_i),
  \qquad 0<c<1.
\]

Root labels are not ordered inside the sampler. Reported endpoints are

\[
\ell_i=\min(\eta_{1i},\eta_{2i}), \qquad
u_i=\max(\eta_{1i},\eta_{2i}).
\]

### Fixed learning rate

For a declared positive rate \(\omega_R\), the target is

\[
\Pi(d\Theta\mid y)
\propto
\pi(d\Theta)\exp\{-\omega_R L_c(\Theta;y)\}.
\]

The fixed rate is part of the target specification. It is not a response
precision unless a separate response model is introduced.

### Normalized learned inverse-loss scale

Let \(s_L>0\) be a fixed reference scale and
\(\lambda\sim\operatorname{Gamma}(a_\lambda,b_\lambda)\), using shape and rate.
The supported learned-scale target is

\[
\Pi(d\Theta,d\lambda\mid y)
\propto
\pi(d\Theta)\pi_\lambda(\lambda)
\lambda^{n_{\mathcal O}}
\exp\{-\lambda L_c(\Theta;y)/s_L\}.
\]

Consequently,

\[
\lambda\mid\Theta,y
\sim
\operatorname{Gamma}\left(
  a_\lambda+n_{\mathcal O},
  b_\lambda+L_c(\Theta;y)/s_L
\right).
\]

The effective generalized-Bayes rate is \(\omega_R=\lambda/s_L\).
Missing responses contribute neither to \(L_c\) nor to the power of
\(\lambda\).

### Pseudo-AL augmentation

The pseudo-AL representation is an augmentation of the loss update for the
pseudo-residual product \(e_i\). It is not an asymmetric-Laplace response
likelihood. Conditional on the latent scales and the other root, each root has
a Gaussian full conditional. Jointly in both roots, the augmented measurement
term is quartic; therefore one ordinary Gaussian draw of a stacked two-root
state is not a valid replacement for the alternating root-specific updates.

For the normalized learned-rate scan, the required partial-collapse order is:

1. draw \(\lambda\) after integrating out the latent pseudo-AL scales;
2. refresh every observed latent scale under the new \(\lambda\);
3. draw root 1 conditional on root 2;
4. draw the root-1 prior/evolution hyperstate;
5. draw root 2 conditional on the refreshed root 1;
6. draw the root-2 prior/evolution hyperstate; and
7. optionally swap the complete root blocks.

The swap must move each coefficient or state path together with every
root-specific prior state. A swap of coefficients alone is invalid.

This order also determines which starting values can provide meaningful
between-chain dispersion. `lambda_initial` is replaced by the first collapsed
rate draw, and every observed `latent_v` placeholder is immediately refreshed
before a root update. They are therefore not independent overdispersed-start
dimensions. The ordinary-v1 validation runner supplies the fixed public-API
placeholder `lambda_initial = 1`, supplies no fresh latent placeholder, and
excludes both quantities from its initialization profiles and evidence
manifest. Its four profiles disperse only root midpoint and half-width and,
under RHS-NS, the complete root-specific prior initialization states. One
constructor creates the exact initialization object used by a fit and hashed
in `initialization_manifest.csv`; the digest therefore includes both RHS-NS
root states whenever that prior is active.

## Fixed-design regression

For \(X\in\mathbb R^{n\times p}\),

\[
\eta_{ki}=x_i^\top\beta_k,\qquad k\in\{1,2\}.
\]

The implementation must reject nonfinite design entries and inconsistent
dimensions. It must permit `NA` only in the response and must reject an
all-missing response.

### Gaussian coefficient prior

The general Gaussian option is

\[
\beta_k\sim N_p(m_{0k},C_{0k}),
\]

where each \(C_{0k}\) is finite, symmetric, and positive definite. Ridge is the
special case

\[
m_{0k}=0,\qquad C_{0k}=\tau_\beta^2 I_p,\qquad \tau_\beta^2>0.
\]

The default root priors must be exchangeable. If distinct root priors are
allowed, the global label-swap move must be disabled unless the complete prior
law is invariant to that swap.

The implementation branch now exposes ridge and canonical full-Gaussian prior
objects. The version-1 public fit accepts one shared exchangeable prior
specification and creates independent root-specific states from it. It does
not expose two different root-prior specifications. The constructor validates
dimensions, names, symmetry, and strict positive definiteness before a
transition is attempted.

Coefficient binding follows one of two explicit contracts. If either the
Gaussian mean or covariance/precision matrix carries coefficient names, the
matrix must have complete, unique, identical row and column names, a supplied
mean must carry those same names, and that order must exactly match
`colnames(X)`. Partial naming, duplicate names, and silent reordering are
rejected. Only when every Gaussian-prior input is unnamed does the
implementation use an order-based binding.

### Native RHS-NS prior

The version-1 RHS-NS implementation must be native to `rqrgibbs`; ordinary RQR
sampling must not call private exdqlm functions at run time. The pinned exdqlm
source may be used as the read-only behavioral reference for the port.

For root \(k\), let \(\mathcal A\) denote the declared set of shrunk
coefficients and \(m=|\mathcal A|\). The default intercept is excluded from
\(\mathcal A\) and has

\[
\beta_{k0}\sim N(0,d_0^{-1}),
\]

where \(d_0>0\) is declared. For \(j\in\mathcal A\), the reference augmented
joint kernel is

\[
N(\beta_{kj};0,\tau_k^2\lambda_{kj}^2)\,
N(\beta_{kj};0,\zeta_k^2),
\]

with

\[
\begin{aligned}
\lambda_{kj}^2\mid\nu_{kj}
  &\sim \operatorname{IG}(1/2,1/\nu_{kj}),\\
\nu_{kj}
  &\sim \operatorname{IG}(1/2,1),\\
\tau_k^2\mid\xi_k
  &\sim \operatorname{IG}(1/2,1/\xi_k),\\
\xi_k
  &\sim \operatorname{IG}(1/2,1/\tau_0^2),\\
\zeta_k^2
  &\sim \operatorname{IG}(a_\zeta,b_\zeta),
\end{aligned}
\]

or \(\zeta_k^2\) is fixed at a declared positive value. Here
\(\operatorname{IG}(a,b)\) denotes the inverse-Gamma distribution with density
proportional to \(x^{-a-1}\exp(-b/x)\).

The coefficient full-conditional prior precision contribution is

\[
d_{kj}
=
\frac{1}{\tau_k^2\lambda_{kj}^2}
+
\frac{1}{\zeta_k^2}.
\]

The corresponding scale updates are

\[
\begin{aligned}
\lambda_{kj}^2\mid\cdot
&\sim
\operatorname{IG}\left(
  1,\frac{1}{\nu_{kj}}+\frac{\beta_{kj}^2}{2\tau_k^2}
\right),\\
\nu_{kj}\mid\cdot
&\sim
\operatorname{IG}\left(1,1+\frac{1}{\lambda_{kj}^2}\right),\\
\tau_k^2\mid\cdot
&\sim
\operatorname{IG}\left(
  \frac{m+1}{2},
  \frac{1}{\xi_k}
  +\frac12\sum_{j\in\mathcal A}
    \frac{\beta_{kj}^2}{\lambda_{kj}^2}
\right),\\
\xi_k\mid\cdot
&\sim
\operatorname{IG}\left(
  1,\frac{1}{\tau_0^2}+\frac{1}{\tau_k^2}
\right),\\
\zeta_k^2\mid\cdot
&\sim
\operatorname{IG}\left(
  a_\zeta+\frac m2,
  b_\zeta+\frac12\sum_{j\in\mathcal A}\beta_{kj}^2
\right).
\end{aligned}
\]

This joint-kernel statement matters. The product of the two Gaussian factors
is proportional to a Gaussian density with precision \(d_{kj}\), but its
normalizing factor depends on \(\tau_k^2\lambda_{kj}^2\) and \(\zeta_k^2\).
Replacing the factorized joint kernel by a normalized Gaussian conditional
with precision \(d_{kj}\), while retaining the scale updates above, changes the
joint target. The native port must either implement the factorized joint
kernel and these full conditionals exactly or provide a separate derivation
and validation for a different normalized hierarchy. It must not switch
between the two interpretations silently.

The two roots require separate RHS-NS states. A label swap must exchange
\((\beta_k,\lambda_k^2,\nu_k,\tau_k^2,\xi_k,\zeta_k^2)\) as one block.

The implementation branch now carries this RHS-NS hierarchy natively. Static
and frozen-design DESN readouts do not call exdqlm for prior-state updates.
The pinned exdqlm source remains a read-only parity reference and an optional
DESN feature materializer only.

### Missing-response contract

The static sampler must construct one immutable observed-data mask before
initialization. For \(i\notin\mathcal O\):

- no loss term is evaluated;
- no latent pseudo-AL scale is allocated or drawn;
- no pseudo-observation or precision contribution enters either root update;
- no learned-scale power is added; and
- replacement of the unused response storage by any finite placeholder leaves
  the chain unchanged under a fixed RNG state.

The returned fit records \(n\), \(n_{\mathcal O}\), the mask digest, and the
observed response/design digest. The implementation branch accepts `NA`,
rejects `NaN`, infinities, and all-missing responses, and omits missing sites
from the stochastic transition without consuming latent-scale RNG.

## Exact static continuation

`rqr_mcmc_continue()` is required for version 1. Continuation must resume from
the exact terminal transition state, not merely from posterior means or the
last retained coefficients.

The digested checkpoint stores the transition state required for bitwise
continuation:

- both coefficient vectors;
- the full prior state for each root;
- the current \(\lambda\), when learned;
- all observed latent pseudo-AL scales;
- the RNG kind and complete RNG state;
- completed raw iterations, burn-in status, and thinning phase;
- target, data, design, and prior digests.

The checkpoint is one part of the complete fit envelope. The separately
digested data contract, continuation history, numerical-repair ledger,
segment schedule, and provenance contract collectively bind the immutable
observed-data mask, numerical policy and repairs, resolved backend, runtime,
package, and source state. These fields need not be duplicated inside the
transition checkpoint, but every continuation consumer must validate the
checkpoint and the complete enclosing contracts before restoring the RNG.

For a fixed target, backend, runtime, and numerical policy, an uninterrupted
run and a segmented run must be bitwise identical in every saved stochastic
field and final checkpoint. An explicit environment override must be recorded
in the cumulative continuation ledger and must remove reproducibility and
promotion eligibility.

The implementation branch stores a digested checkpoint, a versioned
continuation-history contract, and exposes `rqr_mcmc_continue()`. It restores
both root/prior blocks, observed latent scales, the learned rate, and the full
RNG state. Release still requires the frozen uninterrupted-versus-`2+2+2`
matrix and adversarial mutation suite to pass from the exact isolated runtime.

The public fitting boundary cannot be used to forge a continuation.
`rqr_mcmc_fit()` rejects continuation-only initialization fields. The public
`rqr_mcmc_continue()` first validates the checkpoint, cumulative history,
target, data, prior, runtime, and any recorded environment mismatch, then
passes the restored state through a private token-bound worker. Both `init`
and `mcmc_control` use allowlists: unknown names, ambiguous legacy/canonical
aliases, and simultaneous `seed`/`rng_seed` or
`precision_beta`/`precision` declarations fail rather than being ignored.

## Frozen-design RQR-DESN

The version-1 DESN object is a structured fixed-design regression:

\[
\eta_k=X_{\mathrm{DESN}}\beta_k.
\]

The reservoir and feature construction may be materialized by the pinned
exdqlm reference implementation. Version 1 does not require a native reservoir
builder. It does require the following separation:

1. the materializer creates one frozen design; its receipt records the exact
   source/runtime lineage, the materializer-argument digest, and the complete
   materialized-payload digest;
2. the ordinary `rqrgibbs` static sampler fits both RQR roots to that design;
3. no exdqlm readout sampler or private RHS-NS update is invoked by the native
   readout; and
4. the fit records the exact exdqlm commit and the attested isolated runtime
   used to materialize the design.

The raw effective arguments and their seed are frozen in the tracked
configuration and compact materialization evidence. The receipt does not
duplicate those raw values. Its argument digest binds them, while its payload
digest covers the serialized design payload: response and feature matrix,
time index, feature schema and order, builder metadata, reservoir declaration,
driver and causal declarations, time contract, and terminal-state metadata.

The exdqlm source checkout remains read-only. It must be archived at the pinned
commit, built, installed, and loaded only below the ignored
`application/cache/` tree. The before/after checkout guard must include
ignored files.

Given identical \(y\), \(X_{\mathrm{DESN}}\), target, prior, initial state, and
RNG state, the DESN readout and a direct fixed-design fit must be bitwise
identical. DESN-specific code must not alter the loss or coefficient sampler.

Future interval roots require an explicit versioned future-design contract.
That contract verifies feature alignment and order, declared forecast
semantics, the parent-design link, and the content digest of the supplied
future rows. It does not attest how those rows were generated. In particular,
the receipt that materialized and verified the training design cannot be
transferred to future rows. The legacy named `X_future` matrix path remains
available but is non-promotable, and a versioned future contract is also
nonpromotable until a separate future-specific materialization receipt exists.
The pseudo-AL augmentation does not authorize recursive future response
simulation. Teacher-forced or companion-model drivers are separate
application contracts.

The implementation branch now routes ridge, Gaussian, and native RHS-NS DESN
readouts through the same static transition and inherits its missing-data and
continuation contracts. Hand-built frozen designs remain useful but
non-promotable by default. Promotion requires a versioned materialization
receipt reverified against the currently executing pinned isolated exdqlm
runtime; this external state is bound into the native fit provenance. This
requirement promotes the training-design fit only. It does not promote
functionals evaluated on separately supplied future rows.

Provenance is deliberately family-specific. A standalone fixed-design fit
binds only the isolated primary `rqrgibbs` runtime and must have no required
external repository. A promotion-grade D02 DESN fit binds that same primary
runtime and requires exactly the pinned, isolated, attested `exdqlm` runtime
that materialized the design. The validation runner rejects either provenance
contract if those roles are reversed or conflated.

## RQR-DLM

For each root,

\[
\theta_{k,t}=G_t\theta_{k,t-1}+\omega_{k,t},
\qquad
\eta_{k,t}=F_t^\top\theta_{k,t}.
\]

The stacked two-root prior is Gaussian, but the augmented measurement kernel is
not jointly Gaussian in the stacked state. Exact version-1 sampling therefore
uses alternating root-specific FFBS updates. It does not perform one
simultaneous Gaussian FFBS draw of the stacked roots.

The accepted evolution modes are:

1. explicitly supplied positive-semidefinite \(W_t\);
2. a discount template frozen before MCMC, with the resulting \(W_t\) sequence
   fixed throughout the chain; and
3. fixed component templates with shared, sampled component scales and their
   validated interweaving step.

The conditionally adaptive discount recursion is a working procedure rather
than exact Gibbs sampling for one fixed joint target and is excluded from
version 1.

Existing public construction and fitting functions form the accepted DLM
surface:

- `rqr_as_dlm_model()`;
- `rqr_polytrend()`;
- `rqr_seasonal()`;
- `rqr_regression()`;
- `rqr_evolution_fixed()`;
- `rqr_freeze_discount_template()`;
- `rqr_evolution_component_scale()`;
- `rqr_dlm_fit()`;
- `rqr_dlm_continue()`; and
- `rqr_forecast_roots()`.

Forecasts propagate future root states and report ordered-root functionals.
They do not simulate future responses.

The public DLM fitting boundary accepts only fresh-state initialization.
Completed-iteration, parent-history, promotion, and schedule fields are
private to `rqr_dlm_continue()` and reach the sampler only through its
package-private token. Every fresh or continued fit carries a versioned,
digested segment schedule that binds burn-in, retained draws, thinning, raw
iteration endpoints, and the parent checkpoint chain. Before return, the DLM
fit envelope validates that `last` equals the authoritative checkpoint; that
the final retained root paths, terminal and time-zero states, latent scales,
loss rate, and component scales agree with that checkpoint when stored; and
that fixed-rate loss scales are exact constants. Continuation and public
read-only fit consumers apply the same envelope validation. Minimal
state-only forecast fixtures remain explicitly unbound and are not represented
as validated fitted objects.

The audited DLM path has the most complete exact-target, numerical, provenance,
and continuation infrastructure in the repository. Its fixed-\(W_t\),
pre-frozen-discount, and component-scale modes have bounded validation
evidence. That evidence does not remove the need to preserve its gates when
static and DESN code is completed, and it does not establish production
forecasting performance.

## Required public static and DESN surface

Version 1 should expose the following behavior without requiring users to call
internal functions:

| Function | Required behavior |
|---|---|
| `rqr_beta_prior("ridge", ...)` | Preferred isotropic zero-mean Gaussian prior |
| `rqr_beta_prior("gaussian", ...)` | Preferred validated full mean and covariance |
| `rqr_beta_prior("rhs_ns", ...)` | Preferred native factorized RHS-NS joint kernel |
| `beta_prior(...)` | Backward-compatible constructor for the same three native contracts |
| `rqr_mcmc_fit()` | Fixed-design exact MCMC for the two accepted rate modes |
| `rqr_mcmc_continue()` | Integrity-checked exact continuation |
| `rqr_posterior_draws()` | Root-coefficient and declared hyperparameter draws |
| `predict_interval()` | Ordered root, midpoint, and width functionals |
| `application/scripts/28_materialize_rqr_ordinary_v1_desn_design.R` | Promotion-grade design-only materialization from exact isolated runtimes |
| `rqr_desn_fit(..., fit_readout=FALSE)` | Low-level design return; promotion still requires receipt-v2 orchestration and live revalidation |
| `rqr_desn_fit(..., fit_readout=TRUE)` | Native ordinary-RQR fit on that design |
| `forecast_paths()` | Root functionals from an explicit future design |

The version-1 DESN methods must reject `inference="vb"`, nonzero tilt,
observation weighting based on square-root premultiplication, and any request
for response-predictive draws. For backward compatibility,
`rqr_mcmc_fit()` may execute `learning_rate_mode="learned_pure"` only when the
fit is explicitly non-promotable and non-continuable. Fixed rate and normalized
learned rate are the two ordinary-v1 targets.

## Schema and provenance requirements

The following identifiers define the current ordinary-family contracts
implemented on the isolated branch:

| Object | Current schema |
|---|---|
| Common ordinary target | `rqrgibbs_ordinary_target/1.0.0` |
| Coefficient prior | `rqrgibbs_beta_prior/1.0.0` |
| Coefficient prior state | `rqrgibbs_beta_prior_state/1.0.0` |
| RHS-NS state | `rqrgibbs_rhs_ns_state/1.0.0` |
| Fixed-design data | `rqrgibbs_fixed_design_data/1.0.0` |
| Fixed-design transition | `rqrgibbs_fixed_design_transition/1.0.0` |
| Static fit | `rqrgibbs_static_fit/1.0.0` |
| Static checkpoint | `rqrgibbs_static_checkpoint/1.0.0` |
| Interval prediction | `rqrgibbs_interval_prediction/1.0.0` |
| Frozen DESN design | `rqrgibbs_desn_design/1.0.0` |
| DESN feature schema | `rqrgibbs_desn_feature_schema/1.0.0` |
| Frozen future DESN design | `rqrgibbs_desn_future_design/1.1.0` |
| DESN materialization receipt | `rqrgibbs_desn_materialization_receipt/2.0.0` |
| DESN receipt validation status | `rqrgibbs_desn_materialization_receipt_status/1.0.0` |
| DESN materialization verification | `rqrgibbs_desn_materialization_verification/1.0.0` |
| DESN fit envelope | `rqrgibbs_desn_fit/1.1.0` |
| DESN future verification | `rqrgibbs_desn_future_verification/1.0.0` |

The tracked validation configuration and compact evidence use
`rqrgibbs_ordinary_v1_validation/1.0.0` and
`rqrgibbs_ordinary_v1_evidence/1.0.0`; the process wrapper reports
`rqrgibbs_ordinary_v1_wrapper/1.0.0`.

The candidate DLM fit and continuation schemas are
`rqrgibbs_fit/1.11.0` and
`rqrgibbs_continuation_history/4.1.0`. The fit schema is incremented because
the ordinary-v1 integration adds explicit scope/continuation fields and
hardens fitted-draw and forecast boundaries; the target and transition
mathematics are unchanged. The history structure remains version 4.1.0.
Schema identifiers must not be changed merely to make static names match.
Reuse of the continuation-history schema is permitted only if the static
object satisfies its complete validator without weakening any DLM invariant.

Every promotable fit must bind:

- the complete primary source commit;
- the source, source-package, installed-runtime, and executing-runtime
  digests from the isolated build;
- the exact target and prior schema;
- data, observed mask, design, and future-design digests;
- external materializer identity, when used;
- RNG and initialization contracts;
- numerical policy and complete repair ledger; and
- continuation history and checkpoint digests.

For static fits, the external-materializer item is absent and the required
external-repository set is empty. For D02 DESN fits, the required set is
exactly `exdqlm`, and the training-design fit must retain the verified receipt,
live external-state match, and runtime-attestation binding. A future-root
object additionally records its versioned future contract, but is not
promotion eligible unless a future-specific receipt verifies the
materialization of those rows. The bounded future gates test conditional root
mechanics and contract integrity; they are not scientific forecast-provenance
evidence.

A fit with numerical repairs, an unverified runtime, a mutated checkpoint, an
unattested external materializer, or an environment override is not promotion
eligible.

## Acceptance gates

### Common target and augmentation

- Direct numerical evaluation of the RQR loss agrees with the augmented
  kernel after accounting for constants independent of the sampled block.
- Conditional Gaussian moments agree with independently assembled dense
  calculations.
- Fixed-rate and normalized learned-rate intercept-only fixtures agree with
  stable quadrature for means and event probabilities.
- The learned-rate shape uses \(n_{\mathcal O}\), not the allocated row count.
- Scan-order mutation tests reject invalid partial-collapse orders.
- Root-label invariant summaries are unchanged by a complete block swap.

### Static Gaussian and ridge

- Full Gaussian inputs reject nonsymmetric, indefinite, dimensionally invalid,
  or nonfinite covariance matrices.
- Ridge and its equivalent full-Gaussian specification are identical under a
  fixed RNG state.
- Direct dense conditional means and covariances agree at small dimensions.
- Both rate modes pass four-chain diagnostics on a bounded fixture.

### Native RHS-NS

- No native static fit calls an exdqlm private function.
- The native state contains every local, global, auxiliary, and slab variable
  for both roots.
- Each inverse-Gamma conditional agrees with independent scalar calculations.
- Fixed-seed transition parity or distributional-moment parity is established
  against the pinned reference implementation.
- Intercept exclusion and the declared active set are tested explicitly.
- Fixed and sampled slab scales are tested separately.
- Complete root-block swaps include all RHS-NS state.
- Segmented continuation is bitwise identical for coefficients and every
  stored RHS-NS variable.
- The factorized-joint versus normalized-conditional distinction is tested and
  documented; one target is selected explicitly.

### Missing responses

- A missing row is absent from loss, latent-scale, precision, and learned-rate
  calculations.
- Placeholder invariance holds under the same RNG state.
- Leading, trailing, interior, and multiple missing sites are covered.
- Complete-data behavior is unchanged.
- An all-missing response is rejected.
- Continuation verifies the immutable mask and observed-data digest.

### DESN

- The frozen design is reproducible from the pinned materializer configuration
  and seed.
- Design dimensions, column order, numeric bytes, and digest match the
  reference materializer.
- A DESN readout is identical to a direct static fit supplied the same design.
- Ridge, full Gaussian, and native RHS-NS readouts pass.
- Missing responses and both accepted learning-rate modes pass.
- Future designs are explicit, dimension checked, and provenance bound.
- No method labels root draws as response-predictive draws.

### DLM regression

- R and C++ FFBS implementations retain their dense-Gaussian mean and
  covariance references.
- Fixed \(W_t\), frozen-discount, and component-scale modes remain exact fixed
  joint targets with zero numerical repairs in promotion runs.
- Missing-measurement omission, future-root moments, component-scale
  conditionals, interweaving, and exact continuation remain covered.
- The six fixture-by-rate continuation cells remain bitwise identical under
  uninterrupted and segmented execution.
- The 24-fit bounded evidence remains reproducible from a newly attested
  runtime whenever a DLM-relevant source change requires rerunning it.

### Package and documentation

- Native package tests call `rqrgibbs` implementations; reference parity tests
  are separately labeled and may call the attested reference runtime.
- `R CMD check` passes from a clean isolated build.
- Rd files identify the two accepted version-1 modes and may mention an
  excluded compatibility mode only when it is explicitly labeled diagnostic,
  nonpromotable, and noncontinuable.
- README and package documentation state the loss-based interpretation.
- A family vignette distinguishes fixed design, frozen-design DESN, and
  dynamic linear roots without presenting them as different inferential
  targets.
- Manuscript and supplement builds pass without untracked generated products.

## Protection of the validated DLM path

Ordinary static and DESN work must be additive wherever practicable. The
active DLM validation and its evidence are not a development sandbox for the
static port.

### Changes confined to new static or DESN files

If a patch changes only family-specific static/DESN files and does not alter a
shared utility, schema, serialization format, or package interface used by the
DLM, run at minimum:

- parsing and package namespace checks;
- native static and DESN tests;
- standalone DLM model and sampler tests;
- package check;
- monitor and fail-closed launcher tests; and
- article, supplement, and theory checks affected by the patch.

### Changes to shared infrastructure

If a patch changes `rqr_utils.R`, `rqr_numerics.R`, package metadata,
provenance, schema validation, RNG handling, continuation history, checkpoint
serialization, or a shared numerical helper, rebuild the exact isolated
runtime and rerun:

- all six DLM continuation cells;
- recomputed-digest mutation tests;
- R/C++ FFBS parity;
- missing and future-state references;
- component-scale and interweaving references; and
- the exact confirmatory-correction gates, including M01, M02, horizon, and
  M03 checks.

Prior evidence cannot be transferred automatically across a shared-code
change.

The same rule applies to an intentional correction at a public DLM method
boundary. The ordinary-v1 candidate retains `beta_prior()` as a documented
native compatibility wrapper rather than preserving its former exdqlm-backed
bytes, and it hardens fitted-root draw validation without changing the DLM
loss, augmentation, FFBS, or evolution transition. Those compatibility and
method-boundary choices must be listed in the reconciliation, followed by
regenerated protected hashes and the complete DLM regression matrix above.
They must not be summarized as byte-identical DLM noninterference.

### Changes to target or transition code

If a patch changes the loss, pseudo-AL augmentation, scan order, learning-rate
conditional, state evolution, FFBS transition, or component-scale transition,
rerun the complete 24-fit bounded DLM validation and obtain an independent
promotion audit before restoring any execution authorization.

No ordinary-version-1 implementation change may be folded into a flag-only
DLM authorization commit. Development must remain on an isolated branch until
the relevant regression gates pass.

## Current implementation-branch status

| Capability | Source status | Remaining promotion gate |
|---|---|---|
| Static ridge, both rates | Implemented | Exact isolated reference and 48-fit gates |
| Static full Gaussian | Implemented | Dense-reference and bounded-chain gates |
| Static RHS-NS | Native implementation | Conditional/parity and bounded-chain gates |
| Static missing responses | Implemented | Complete mask/RNG mutation matrix |
| Static exact continuation | Implemented | Full `6` versus `2+2+2` and mutation matrix |
| Frozen-design RQR-DESN | Implemented | Attested D02 end-to-end reference and 16 fits |
| RQR-DLM fixed \(W_t\) | Implemented and bounded-validated | Preserve active confirmatory evidence |
| RQR-DLM frozen discount | Implemented and bounded-validated | Adaptive recursion remains excluded |
| RQR-DLM component scale | Implemented and bounded-validated | Preserve component-scale/interweaving gates |
| Root/interval prediction | Implemented | Package/reference checks and no-response flags; DESN future outputs remain nonpromotable pending a future-specific receipt |

Thus, the ordinary fixed-design and frozen-DESN source surface is implemented
on the isolated branch. The exact RQR-DLM target and transition remain
unchanged, while its fitted-draw public boundary is intentionally hardened;
that interface correction requires regenerated protected hashes and the
mandatory DLM regression matrix before merge. The family is not yet
release-complete: generated documentation, package checking, isolated-runtime
reference evidence, monitor fault tests, the representative one-cell
benchmark, the disabled 48-fit mechanics grid, and final DLM compatibility
and regression checks remain mandatory. Mean-tilted RQR stays deferred until
those gates close.
