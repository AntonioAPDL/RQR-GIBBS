# Exact component-scale interweaving for RQR-DLM

Date: 2026-07-26

## Purpose

The first authorized main-simulation wave exposed poor mixing in the shared
component evolution scale. Every attempted M01 fit failed its predeclared
`log_q_1` diagnostic even though the ordered endpoint, midpoint, width, and
loss functionals usually passed. The worst retained 2,000-draw chain had bulk
ESS 3.46. Extending that chain until bulk ESS 400 under linear ESS scaling
would require more than 230,000 retained draws. Chain extension would
therefore be an inefficient response and would violate the frozen no-extension
rule.

This note specifies the exact reparameterization used instead. It changes the
Markov transition kernel, not the generalized posterior, component-scale
prior, learning rate, data-generating process, seed ledger, or diagnostic
thresholds.

## Centered update

For root \(k\), component \(j\), and time \(t\), write

\[
d_{kjt}
=S_j\{\theta_{kt}-G_t\theta_{k,t-1}\},\qquad
d_{kjt}\mid q_j\sim N(0,q_jQ_{jt}).
\]

The multiplier \(q_j\) is shared by both roots and has inverse-Gamma prior
\(\operatorname{IG}(a_j,b_j)\), with density proportional to
\(q_j^{-a_j-1}\exp(-b_j/q_j)\). Its centered full conditional is

\[
q_j\mid\cdot\sim\operatorname{IG}\left[
a_j+Td_j,\
b_j+\frac12\sum_{k=1}^2\sum_{t=1}^T
d_{kjt}^{\mathsf T}Q_{jt}^{-1}d_{kjt}
\right].
\]

This conjugate update remains the first component-scale update in every
iteration.

## Noncentered coordinates

After the centered draw, define

\[
u_{kjt}=d_{kjt}/\sqrt{q_j}.
\]

Conditional on the two time-zero states, both state paths are deterministic
functions of \(\boldsymbol u_1,\boldsymbol u_2\), and
\(\boldsymbol q\). The implementation precomputes the linear path basis for
each component, so a proposed scale vector requires only a linear
reconstruction rather than a new state filter or repeated template
factorization. It also projects that basis through the observation design
once; slice-density evaluations then use ordinate-level vector arithmetic, and
full state paths are reconstructed only after a scale proposal is accepted.

Across the two roots, the transformation from standardized innovations to
centered state innovations contributes the Jacobian
\(\prod_j q_j^{Td_j}\). This cancels the
\(\prod_j q_j^{-Td_j}\) scale term in the two Gaussian evolution densities.
For \(x_j=\log q_j\), the inverse-Gamma prior and the log-scale Jacobian leave

\[
-\sum_j\{a_jx_j+b_j\exp(-x_j)\}.
\]

Conditional on the RQR latent scales, the remaining noncentered log kernel is

\[
\log \pi(\boldsymbol x\mid\cdot)
=-\sum_j\{a_jx_j+b_j\exp(-x_j)\}
-\frac12\sum_{t:y_t\ {\rm observed}}
\frac{\{e_t(\boldsymbol x)-\xi_cv_t\}^2}{V_t}
+\text{constant},
\]

where \(e_t(\boldsymbol x)\) is the residual product after reconstructing both
root paths and \(V_t=\phi_cs_Lv_t/\lambda\). This is the augmented
generalized-Bayes loss kernel. It is not a response likelihood.

## Slice transition and exactness

Each log scale is updated conditionally by a univariate stepping-out slice
transition. The implementation uses a positive fixed width, randomized
left--right allocation of a finite stepping-out budget, and shrinkage about
the current value. Nonfinite proposed scales have log density negative
infinity. A nonfinite density at the current value or exhaustion of the
shrinkage budget stops the fit.

One interweaving cycle is:

1. draw every \(q_j\) from its centered inverse-Gamma full conditional;
2. transform both root paths to the standardized innovations
   \(u_{kjt}\);
3. update \(x_1,\ldots,x_J\) by the frozen number of coordinate slice sweeps;
4. reconstruct both state paths at the updated scales; and
5. recompute the centered conditional shape and rate recorded with a retained
   draw.

The centered Gibbs transition and the noncentered transition are each
invariant for the same joint generalized posterior. Their composition is an
ancillarity--sufficiency interweaving transition. The time-zero states remain
fixed during the noncentered step. The optional global root-label swap follows
the completed interweaving transition.

## Exact one-root partial collapse

The complete second-wave development gate subsequently showed that ASIS alone
does not uniformly remove the scale--trajectory dependence. All 49 fits
completed under the 6,000-retained schedule, but only 1,131 of 1,150
diagnostics passed. Twelve of the 25 tasks failed at least one gate. Most
failures involved \(\log q_1\); lag-one autocorrelation reached approximately
0.98, and one deliberately overdispersed chain had not settled after the
fixed 1,000-iteration burn-in. This is computational diagnostic evidence, not
a comparative simulation result.

The replacement transition adds an exact partially collapsed block before the
ordinary root-specific FFBS steps. Condition on root 2, including its time-zero
state, and integrate root 1 and its time-zero state. Let
\(\ell_1(\boldsymbol q)\) be the Gaussian Kalman-filter log marginal for the
root-1 pseudo-observation equation conditional on root 2 and the current RQR
latent scales. For the conditioned root-2 path, define

\[
 E_{2j}=\frac12\sum_{t=1}^T
 d_{2jt}^{\mathsf T}Q_{jt}^{-1}d_{2jt}.
\]

On the log scale \(x_j=\log q_j\), the partially collapsed kernel is

\[
 \log\pi(\boldsymbol x\mid
   \boldsymbol\theta_2,\theta_{20},\boldsymbol v,\ldots)
 =
 \ell_1(\exp\boldsymbol x)
 -\sum_j\left[
   \left(a_j+\frac{Td_j}{2}\right)x_j+
   (b_j+E_{2j})e^{-x_j}
 \right]+\text{constant}.
\]

The \(Td_j/2\) contribution is from the single conditioned root. The
integrated root contributes through the Kalman marginal rather than through a
second innovation sum. After coordinate slice sampling from this kernel, the
algorithm draws root 1 by FFBS at the accepted scale and completes its
time-zero state conditionally. It then draws root 2 conditionally and applies
the existing centered--noncentered ASIS transition. Thus the new ordering
composes:

1. a target-invariant slice transition for the marginal scale kernel followed
   by exact draws of \((\boldsymbol\theta_1,\theta_{10})\) conditional on the
   accepted scale, root 2, and the remaining augmented variables;
2. an exact root-2 FFBS and time-zero completion conditional on root 1;
3. the exact centered--noncentered scale interweave; and
4. the global root-label swap.

Each block leaves the same augmented generalized posterior invariant. The
root-label swap makes the nominal choice to integrate root 1 symmetric over
successive iterations. No response likelihood or response-predictive
distribution is introduced. The deterministic marginal calculation is
implemented in C++ because it is evaluated repeatedly inside the scale slice
step; the R implementation is retained as an independent parity reference.

## Public and continuation contracts

The historical centered-only kernel remains available through
`mcmc_control$component_scale_interweave = FALSE`, and the partial collapse is
separately controlled by
`mcmc_control$component_scale_collapsed_update`. The main confirmatory
contract enables both moves for every component-scale method, including the
common-scale ablation. It freezes:

```text
one-root partial collapse = TRUE
integrated root           = root 1
slice width       = 1
interweave cycles = 1
sweeps per cycle  = 3
step-out budget   = 100
shrinkage budget  = 1000
target change     = FALSE
```

The three-sweep value was selected before any new exact-commit wave gate.
On the diagnosed S03 replication 28, a development-only four-profile
comparison used 1,500 retained draws per chain. Two sweeps gave
\(\widehat R=1.0054\), bulk ESS 381.0, and tail ESS 667.3 for
\(\log q_1\), narrowly missing the unchanged bulk-ESS gate of 400. Three
sweeps gave \(\widehat R=1.0014\), bulk ESS 405.4, tail ESS 892.6, and
MCSE/SD 0.0497; all seven accompanying endpoint, loss, and terminal-state
estimands also passed. Six sweeps failed several gates, including
\(\widehat R=1.0172\) and bulk ESS 84.0 for \(\log q_1\), so the choice is not
based on an assumption that more transitions are uniformly better.
Mean elapsed time rose from 459.2 seconds at two sweeps to 471.8 seconds at
three. These dirty-source development fits can reject or select a prospective
transition but cannot authorize execution or enter the scientific analysis.
The complete first- and second-wave exact-runtime gates remain decisive.

Fixed-rate standard and four-chain sentinel component-scale fits both retain
6,000 draws after 1,000 burn-in iterations. The learned-scale counterpart
retains 9,000 draws after 1,500 burn-in iterations. These are uniform
role-specific schedules, not adaptive chain extensions or retries. The
partial collapse changes the transition, not the number of MCMC iterations;
the iteration-count budget therefore remains unchanged, while a separate
measured resource envelope must account for its per-iteration cost. Thinning,
priors, seeds, targets, diagnostic thresholds, and data-generating mechanisms
are unchanged.

The projection-correct M02 gate separately showed that five one-chain dynamic
quantile comparator tasks missed the common bulk-ESS gate at 2,000 retained
draws. Both the standard and four-chain sentinel roles are therefore frozen
at 4,000 retained draws per endpoint. The complete corrected second-wave
development gate then passed all 1,125 diagnostics from 49 interval chains and
98 endpoint fits while verifying a common target and distinct
target-preserving starts. This is the same uniform role-specific,
no-extension rule and does not modify exdqlm.

The fit, checkpoint, and continuation metadata record these choices. The
checkpoint digest binds the transition-kernel contract, and continuation
rejects a mismatch between checkpoint, fit metadata, and requested resumed
kernel. Fit schema `rqrgibbs_fit/1.11.0` and development package version
`0.1.0.9023` identify the partially collapsed checkpoint contract.

## Provenance-cost boundary

The source-worktree sidecar digest, and the runtime digest only when the
runtime is a direct source load of that same tree, exclude the five declared
local output roots under `application/`: `cache/`, `data_local/`, `logs/`,
`outputs/`, and `runs/`. These trees cannot enter an exact Git archive and
must not be rehashed after every fit as a run tree grows. Executable and
build-relevant ignored files remain in scope; in particular, an ignored object
or shared library under `application/src/` still changes both direct-source
digests. Promotion eligibility continues to require the isolated runtime built
from the exact Git archive, whose complete installed tree is hashed without
these exclusions.

## Required validation before authorization

Authorization remains closed until all of the following pass:

- state-innovation standardization and reconstruction round trips;
- dense-Gaussian equality for the Kalman log marginal in R and C++;
- equality between the partially collapsed log kernel and an independently
  assembled dense-Gaussian target, up to an additive constant;
- equality between recursive and precomputed-basis reconstruction;
- a maintained deterministic slice-sampler distribution check;
- analytic component-scale conditional checks after interweaving;
- uninterrupted and continued-chain byte equality under the new kernel;
- transition-kernel checkpoint mutation rejection;
- uninterrupted and continued-chain byte equality with the partial collapse
  enabled;
- the complete native R and C++ test suite;
- `R CMD check --no-manual`;
- the full confirmatory contract tests;
- exact reruns of every M01 chain represented in both the first and second
  canonical waves, using the original seed ledger and fixed schedules, showing
  that every predeclared diagnostic passes;
- an exact rerun of every first-wave M02 interval chain through the attested
  CRAN exdqlm 1.1.0 runtime, showing that the corrected state-to-ordinate
  projection has length \(T\) and that every predeclared M02 diagnostic passes;
- main article and supplement PDF builds; and
- fresh isolated-runtime preflight and oracle/reference gates at the eventual
  authorization commit.

The failed run tree remains immutable local evidence. No draw, fit, or compact
result from it is eligible for reuse in the replacement run.
