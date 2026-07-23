# Native RQR-DLM design and validation gates

Date: 2026-07-22  
Status: post-audit implementation contract; bounded pilot still gated

## Purpose

This note freezes the audited native state-space contract for the standalone
RQR-GIBBS repository. It separates fixed-joint generalized posteriors from the
exdqlm-compatible adaptive discount recursion, which a two-time
mixed-derivative counterexample shows is generally incompatible with the
advertised pair of Gaussian full conditionals.

The RQR update is loss based. Its normal--exponential representation is an
augmentation of an exponentiated loss kernel; it is not a response likelihood
for `y`, and ordered root draws are not posterior predictive response draws.

## Loss and scale convention

For coverage `c` and roots `eta1[t]`, `eta2[t]`, define

```
e[t] = (y[t] - eta1[t]) * (y[t] - eta2[t])
rho_c(e) = e * (c - 1(e < 0))
L = sum_t rho_c(e[t])
```

The pseudo-residual-normalized learned-scale target implemented here is

```
pi(theta1, theta2, lambda | y)
  proportional to
pi(theta1) pi(theta2) pi(lambda)
lambda^n exp{-lambda L / s_L},
```

with `lambda ~ Gamma(a_lambda, b_lambda)` in shape--rate form. Hence the
collapsed update is

```
lambda | theta1, theta2, y
  ~ Gamma(a_lambda + n, b_lambda + L / s_L).
```

The effective generalized-Bayes learning rate is `omega = lambda / s_L`.
The optional pure-loss convention omits the `lambda^n` term and therefore uses
shape `a_lambda`. The convention is always stored in the fitted object.

With

```
xi_c  = (1 - 2*c) / (c*(1-c))
phi_c = 2 / (c*(1-c)),
```

the latent scale convention is

```
v[t] | ... ~ GIG(1/2, a, b[t])
a    = lambda / {2*s_L*c*(1-c)}
b[t] = lambda*c*(1-c)*e[t]^2 / (2*s_L),
```

where `GIG(nu,a,b)` has density proportional to
`v^(nu-1) exp{-(a*v+b/v)/2}`. At `b=0`, the limiting draw is
`Gamma(1/2, rate=a/2)`. The implementation samples the `nu=1/2` law through
the reciprocal inverse-Gaussian identity and does not require an external GIG
package.

The public target modes are locked:

- `fixed_rate`: `learning_rate` is exactly `omega_R`, independent of `s_L`;
- `learned_pseudoresidual_normalized`: the power is exactly `lambda^n`;
- `learned_pure`: the power is exactly zero and the mode is diagnostic.

Custom powers are rejected. `lambda_initial` initializes learned modes without
changing fixed-rate semantics. Fixed-rate sensitivity is primary; the learned
scale is an additional declared generalized-target convention, not a calibrated
response parameter.

## Dynamic root equations

For roots `k=1,2`,

```
eta[k,t]       = F[t]' theta[k,t]
theta[k,t]     = G[t] theta[k,t-1] + epsilon[k,t]
epsilon[k,t]  ~ N(0, W[k,t]).
```

Conditional on root 2 and the latent scales, the exact root-1 pseudo-observation
is

```
H[1,t] = (y[t] - eta[2,t]) * F[t]'
z[1,t] = y[t] * (y[t] - eta[2,t]) - xi_c * v[t]
V[t]   = phi_c * s_L * v[t] / lambda.
```

Root 2 is obtained by exchanging labels. Each conditional path draw is a
scalar-observation Gaussian state-space update and is sampled by FFBS.

One partially collapsed scan is ordered as follows:

1. Evaluate the loss from the current two paths.
2. Draw `lambda` from its collapsed conditional.
3. Refresh every observed `v[t]` using the new `lambda` and current paths.
4. Draw the complete root-1 path by FFBS conditional on root 2 and `v`.
5. Draw the complete root-2 path by FFBS conditional on the new root 1 and `v`.

The latent scales must be refreshed after the collapsed scale draw. Changing
this order changes the transition kernel.

## Evolution modes

The native API exposes four modes rather than treating all discount-factor
uses as equivalent.

### `fixed_W`

`W[t]` is fixed before MCMC. This defines an ordinary Gaussian state prior and
the alternating FFBS blocks are exact full-conditional updates for the stated
generalized posterior.

### `discount_template`

Component-specific discounts and block dimensions use the exdqlm 1.1.0
interface. For component `j`, `0 < delta[j] <= 1`. The block matrix `D` has
entries `(1-delta[j])/delta[j]` within component `j` and zero between
components. A declared reference covariance recursion constructs

```
P[t] = G[t] C[t-1] G[t]'
W[t] = D elementwise-multiplied by P[t].
```

The resulting `W[1:T]` is then frozen before MCMC. This preserves the familiar
component-specific setup while defining a fixed Gaussian state prior. The
reference observation design and variances are stored with the fit.
If those reference quantities are estimated from the training responses, the
template is an empirical-Bayes prior specification; conditional FFBS remains
exact for the frozen template, but the data dependence must be disclosed.

### `component_scale`

For model component dimensions `d[j]`, fixed SPD templates `Q[j,t]`, and
shared positive scales `q[j]`, the state covariance is

```
W[t] = blockdiag(q[1] Q[1,t], ..., q[J] Q[J,t]).
q[j] ~ Inverse-Gamma(a[j], b[j]).
```

The same scales are shared across the two exchangeable roots. The sampler draws
the integrated time-zero state after each root path, then uses all innovations
from both roots in the exact conditional

```
q[j] | ... ~ Inverse-Gamma(
  a[j] + T*d[j],
  b[j] + 0.5 * sum_{k,t} d[k,j,t]' solve(Q[j,t]) d[k,j,t]
).
```

This provides a coherent component-specific dynamic-variance path without
assigning a posterior interpretation to adaptive filter covariances.

### `adaptive_discount`

At every conditional FFBS call, the exdqlm recursion computes
`W[t] = D * P[t]` from the current filter covariance. Because the RQR
pseudo-design and observation variance depend on the other root, `v`, and
`lambda`, this recursion also makes the implied evolution covariance depend on
those blocks. The scalar `T=2` mixed-derivative counterexample in the supplement
establishes that the two simple conditional FFBS densities are generically
incompatible with a common positive smooth joint density. Accordingly:

- this mode is labeled experimental and working/sequential;
- fitted objects set `exact_joint_target = FALSE`;
- manuscript claims do not present it as exact Gibbs for a fixed joint target;
- production comparisons cannot present it as the exact RQR-DLM; the
  `component_scale` mode is the coherent component-specific alternative.

Both roots use the same component discounts by default to preserve prior
exchangeability.

The matching public constructors are `rqr_evolution_fixed()`,
`rqr_freeze_discount_template()`, `rqr_evolution_component_scale()`, and the
deliberately explicit `rqr_evolution_adaptive_working()`.

## exdqlm 1.1.0 compatibility audit

The CRAN source package used for the compatibility audit was
`exdqlm_1.1.0.tar.gz`, SHA-256
`51bc968f617721c9ab1dcfc6ec14857d30827fcd36659f3de45337cc3c82bd14`,
published 2026-07-09. The relevant public contract is:

- model lists contain `FF`, `GG`, `m0`, and `C0`;
- component constructors combine by vertically stacking `FF`, concatenating
  `m0`, and block-diagonalizing `GG` and `C0`;
- `df` supplies component discounts and `dim.df` supplies their state-block
  dimensions;
- the adaptive covariance recursion is `P = G C G'`, `W = D * P`, and
  `R = P + W`, where `*` is an elementwise product.

Independent, well-conditioned multiblock checks reproduced the package C++
filter/smoother to approximately `1e-15`. The standalone implementation keeps
this matrix contract but adds explicit discount validation and Cholesky-based
positive-definiteness diagnostics. It does not copy exdqlm's silent SVD
regularization behavior.

The pinned development repository remains read-only at
`/data/muscat_data/jaguir26/exdqlm__wt__qdesn_0p4p0_integration`.

## Native package boundary

`application/` is promoted to the development R package `rqrgibbs`.

- `R/rqr_dlm_model.R`: exdqlm-compatible model constructors and composition;
- `R/rqr_ffbs.R`: pure-R reference FFBS and backend dispatcher;
- `src/rqr_ffbs.cpp`: C++17/RcppArmadillo FFBS bottleneck;
- `R/rqr_evolution.R`: exact shared component-scale evolution prior;
- `R/rqr_dlm_fit.R`: partially collapsed RQR-DLM sampler, provenance schema,
  compact terminal-state storage, and exact checkpoint continuation;
- existing fixed-design and DESN files remain available while private exdqlm
  dependencies are retired incrementally;
- `tests/testthat/` contains deterministic parity, discount, invariance, and
  small end-to-end gates.

The C++ boundary receives only Gaussian state-space objects (`z`, `H`, `V`,
`G`, the prior, and an evolution specification). R constructs RQR-specific
pseudo-data. This keeps the numerical kernel reusable and prevents the C++
layer from silently redefining the statistical target.

## Gates before a heavy run

1. Package installation and all native tests pass in a clean R session.
2. Pure-R and C++ filters/smoothers agree with an independent dense Gaussian
   reference on fixed and multiblock fixtures; seeded path draws satisfy
   distributional rather than bitwise checks.
3. Root-label exchangeability is checked under identical root priors.
4. The learned-scale collapsed update and post-lambda latent-scale refresh are
   tested directly.
5. Component-scale inverse-Gamma conditionals, time-zero states, extreme-scale
   GIG draws, restart continuation, local-level cases, missing observations,
   and numerical diagnostics pass focused tests.
6. The simulation protocol freezes common data-generating mechanisms, sample
   sizes, train/test windows, seeds, methods, and scoring rules.
7. Every run manifest records this repository commit, the pinned exdqlm
   commit, package/session information, configuration, and seeds.
8. Fit objects distinguish unavailable Git state from a clean checkout and
   record schema, Git/package/R/compiler/BLAS/LAPACK provenance, RNG state,
   data and matrix digests, and numerical-repair status.
9. Promotion-grade fixtures use the fail-fast numerical policy, record zero
   repairs, and match a clean checkout to the exact commit declared in the run
   manifest.
10. Heavy objects remain under ignored local directories.

## External-review resolution

The adaptive conditional-discount question is resolved negatively for the
advertised simple kernels. The exact shared `component_scale` mode implements
the recommended coherent alternative. The next external review should audit
the proof statement, draw-specific component-scale forecasting, strict
negative-eigenvalue policy, complete repair ledgers, and enforced
continuation/provenance schema before any bounded pilot is authorized.
