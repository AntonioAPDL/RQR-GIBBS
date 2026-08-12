# MT-RQR-ECM Derivation and Contract

Date: 2026-08-12
Branch: `feature/mt-rqr-ecm-split-exact-tcsp-20260812`
Report source: local `report5.md`
Report SHA-256: `65fbc98f64d255c7813ac76e230ec95cb11b825ae22081f7a89ebcee5d5cf2da`
Baseline audited locally: `4ca59085630ead755f5f09f8013dc287a993f71e`

## Role

MT-RQR-ECM is a deterministic optimizer for a fixed generalized-posterior
target. It computes a mode or penalized M-estimator after the content `q`, tilt
`delta`, learning rate `omega`, design, and ridge prior have been fixed.

It is not:

- a response-likelihood EM algorithm;
- a posterior sampler;
- a tolerance action;
- a replacement for the empirical TCSP order-statistic interval.

## Target

For fixed `q`, `delta`, and `omega`, the minimized objective is

```text
J(beta1,beta2)
  = omega * sum_i [
      rho_q((y_i - eta1_i)(y_i - eta2_i))
      - q * delta_i * (eta1_i + eta2_i - 2 y_i)
    ]
    + 0.5 beta1' P beta1
    + 0.5 beta2' P beta2.
```

The implementation evaluates this exact observed objective through
`rqr_mean_tilt_loss()` and stores product-loss, tilt, and prior components.

## E-Step Moment

Under the repository GIG convention

```text
f(v) proportional to v^(p-1) exp{-(a v + b/v)/2},
```

the pseudo-AL latent scale has `p = 1/2`. For nonzero residual product `e_i`,

```text
tau_i = E(V_i^{-1} | current roots)
      = 1 / [q (1 - q) |e_i|].
```

The ECM root update uses this inverse moment. It does not use the current VB
latent mean `E(V_i)` and does not use `1 / E(V_i)`.

## Conditional Maximization Systems

Holding root 2 fixed,

```text
eta2 = X beta2
A2   = diag(y - eta2) X
c2   = y^2 - y * eta2

Lambda1 = P + A2' diag(tau)/(phi*sigma) A2
h1      = A2' [diag(tau)c2 - xi*1]/(phi*sigma)
          + omega*q*X'delta
beta1   = solve(Lambda1, h1).
```

Root 2 uses the updated `beta1` and the same outer E-step `tau`:

```text
eta1 = X beta1
A1   = diag(y - eta1) X
c1   = y^2 - y * eta1

Lambda2 = P + A1' diag(tau)/(phi*sigma) A1
h2      = A1' [diag(tau)c1 - xi*1]/(phi*sigma)
          + omega*q*X'delta
beta2   = solve(Lambda2, h2).
```

The shared helper `.rqr_root_gaussian_system()` now builds the same algebra for
MCMC draw mode and ECM inverse-moment mode.

## Safeguard

The exact inverse moment diverges when `e_i = 0`. The implemented default is a
declared `safeguarded_ecm_mm` run with a response-product-scale floor and exact
observed-objective backtracking. A run records:

- floor type and schedule;
- exact objective trace;
- accepted backtracking count;
- zero-residual encounters;
- whether exact ECM was eligible away from the safeguard;
- stationarity diagnostics using midpoint subgradients at zero residuals.

No exact monotonicity theorem is claimed for the safeguarded run. Monotonicity
is checked computationally after each accepted cycle.

## API

Public additions:

- `rqr_ecm_fit()`;
- `rqr_ecm_path()`;
- `print.rqr_ecm()`;
- `summary.rqr_ecm()`;
- `coef.rqr_ecm()`;
- `predict_interval.rqr_ecm()`.

No `rqr_posterior_draws.rqr_ecm()` method is provided because ECM does not
define posterior samples.

## Current Scope

Implemented:

- fixed design;
- fixed content;
- fixed scalar or row-specific tilt;
- fixed learning rate;
- ridge/Gaussian roots;
- deterministic multi-start;
- optional warm starts for fixed-target paths.

Not implemented or promoted:

- RHS-NS ECM;
- learned-rate ECM;
- dynamic/adaptive/component-scale ECM;
- VB replacement;
- tolerance validity for ECM endpoints;
- posterior-action equivalence.
