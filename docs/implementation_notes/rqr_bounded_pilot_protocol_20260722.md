# Frozen bounded-pilot protocol

## Scope

This protocol is the first post-audit validation gate for the standalone RQR
implementation. It is not a production simulation, an application analysis, or
evidence of response-predictive calibration. Its purpose is to test the
normalized learned-scale generalized-Bayes target and the exact numerical and
provenance contracts on a small deterministic fixture.

The launcher is `application/scripts/05_run_rqr_bounded_pilot.R`. It refuses to
run unless:

- `RQR_BOUNDED_PILOT_CONFIRM=YES`;
- the primary, pinned exdqlm, and Q-DESN reference repositories are at their
  declared branches and commits and are clean;
- the isolated exdqlm runtime and its local attestation match the pinned
  checkout; and
- the native deterministic test suite passes.

All artifacts are written under the ignored `application/outputs/` tree. The
run uses at most one R worker, has a declared 90-minute wall-time budget, and
does not read an application dataset.

## Fixed learned-scale fixture

The response and design are

```r
y <- c(
  -2.0, -1.3, -0.8, -0.4, -0.1, 0.1,
   0.35, 0.7, 1.1, 1.6, 2.2, 3.0
)
X <- matrix(1, 12, 1)
```

The coverage level is `0.80`, the reference loss scale is `s_L=1`, both
intercept roots have independent `N(0,25)` priors, and
`lambda ~ Gamma(shape=4, rate=4)`. The target is

```text
pi(beta_1,beta_2,lambda | y)
  proportional to
pi(beta_1) pi(beta_2) pi(lambda)
lambda^n exp{-lambda L(beta_1,beta_2)/s_L}.
```

This is a normalized loss-based generalized posterior. It is not an ordinary
response likelihood.

## Three independent calculations

The comparison contains:

1. the production partially collapsed `rqr_mcmc_fit()` scan, with
   `lambda | beta_1,beta_2,y ~ Gamma(4+n, 4+L/s_L)`;
2. a validation-only fully augmented scan, with
   `lambda | beta_1,beta_2,v,y ~ Gamma(4+3n/2, 4+A/s_L)`, where
   `A` is the declared normal--exponential augmentation exponent; and
3. deterministic two-dimensional adaptive quadrature over the independent
   Gaussian root prior, after analytically integrating lambda.

The quadrature uses probability-scale coordinates and splits both axes at every
observed response value. This keeps the sign pattern of the residual-product
loss fixed inside each integration cell. Its requested relative tolerance is
`1e-10`, stricter than the `1e-9` gate.

The collapsed seeds are `73201:73204`; the augmented seeds are
`73301:73304`. Every chain uses 5,000 burn-in iterations and retains 20,000
unthinned draws. No seed replacement or post-run retuning is allowed.

## Predeclared estimands and gates

The label-invariant estimands are lambda, `lambda/s_L`, ordered lower root,
ordered upper root, width, midpoint, and total RQR loss. Rank-normalized split
R-hat must not exceed `1.01`; bulk and tail effective sample sizes must each be
at least `2,000`. Draws must be finite and every production precision update
must complete without a numerical repair.

For every mean, collapsed and augmented estimates must differ by no more than
four combined Monte Carlo standard errors. Each sampler mean must also be
within four of its own Monte Carlo standard errors of the quadrature reference.
The following five CDF comparisons are fixed in advance and use the analogous
combined Monte Carlo error rule:

| Estimand | Threshold |
|---|---:|
| lambda | 1.0 |
| lower root | -1.5 |
| upper root | 2.5 |
| width | 4.0 |
| midpoint | 0.5 |

The deterministic matrix contains the exact checks requested in the Output-5
audit: scale-relative covariance validation, cumulative continuation
eligibility, resolved backend behavior, runtime-source binding, dense-Gaussian
FFBS moments, component-scale forecast moments, missing and zero
pseudo-observations, and PSD/indefinite covariance behavior.

Any failed gate makes the pilot a no-go. It does not authorize a production
simulation. A passing pilot authorizes design work for the matched simulation
study while retaining a separate production manifest and explicit user
approval.
