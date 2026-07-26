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
the completed interweaving transition. The confirmatory kernel uses one
centered--noncentered cycle with two coordinate sweeps.

## Public and continuation contracts

The historical centered-only kernel remains available through
`mcmc_control$component_scale_interweave = FALSE`. The main confirmatory
contract sets the option to `TRUE` for every component-scale method, including
the common-scale ablation. It freezes:

```text
slice width       = 1
interweave cycles = 1
sweeps per cycle  = 2
step-out budget   = 100
shrinkage budget  = 1000
target change     = FALSE
```

The four-chain sentinels retain 2,000 draws per chain. Standard
component-scale fits use a separately frozen 6,000-draw one-chain schedule;
the learned-scale counterpart uses 9,000 draws. This is a uniform
role-specific schedule, not adaptive chain extension or retry. Burn-in,
thinning, priors, seeds, targets, diagnostic thresholds, and data-generating
mechanisms are unchanged.

The projection-correct M02 gate separately showed that five one-chain dynamic
quantile comparator tasks missed the common bulk-ESS gate at 2,000 retained
draws, although all four-chain sentinels passed. Their standard schedule is
therefore frozen at 4,000 retained draws; the sentinel schedule remains 2,000
per chain. Re-executing those five original seed streams at 4,000 retained
draws produced bulk ESS at least 247.6 and MCSE/SD at most 0.0634. This is the
same uniform role-specific, no-extension rule and does not modify exdqlm.

The fit, checkpoint, and continuation metadata record these choices. The
checkpoint digest binds the transition-kernel contract, and continuation
rejects a mismatch between checkpoint, fit metadata, and requested resumed
kernel. Fit schema `rqrgibbs_fit/1.10.0` and development package version
`0.1.0.9021` identify the new checkpoint contract.

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
- equality between recursive and precomputed-basis reconstruction;
- a maintained deterministic slice-sampler distribution check;
- analytic component-scale conditional checks after interweaving;
- uninterrupted and continued-chain byte equality under the new kernel;
- transition-kernel checkpoint mutation rejection;
- the complete native R and C++ test suite;
- `R CMD check --no-manual`;
- the full confirmatory contract tests;
- an exact rerun of the frozen S02/replication-85 four-chain sentinel and the
  complete original first-wave M01 task set, using the original seed ledger and
  schedules, showing that every predeclared diagnostic passes;
- an exact rerun of every first-wave M02 interval chain through the attested
  CRAN exdqlm 1.1.0 runtime, showing that the corrected state-to-ordinate
  projection has length \(T\) and that every predeclared M02 diagnostic passes;
- main article and supplement PDF builds; and
- fresh isolated-runtime preflight and oracle/reference gates at the eventual
  authorization commit.

The failed run tree remains immutable local evidence. No draw, fit, or compact
result from it is eligible for reuse in the replacement run.
