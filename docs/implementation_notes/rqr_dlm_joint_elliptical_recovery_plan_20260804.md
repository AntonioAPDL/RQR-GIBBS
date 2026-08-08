# RQR-DLM joint-path recovery plan

## Decision and scope

The completed whole-scan comparison at source commit
`5086fac191255a79514475f6dbacddfae4c328ed` is terminal development
evidence. All 93 jobs completed on the exact target with zero numerical
repairs, but only M01, M02, M06, and M09 selected an eligible x2 transition
schedule. M10 and M11 remained ineligible. The main simulation therefore
remains closed.

This recovery changes transition kernels only. It does not change the
generalized-Bayes target, priors, response laws, seed ledger, hard or guard
replications, diagnostic thresholds, simulation design, or scientific
estimands. Failed and development outputs cannot be reused as confirmatory
outputs.

## Corrected diagnosis

The four-chain sentinel bulk-ESS threshold is 400, not 1,000. At x8, M10 is
close but still fails for `log_q_1` and mean width (bulk ESS about 384 and
388). M11 remains materially deficient: the best diagnostic count is 137/141
at x4, while x8 restores R-hat near one but leaves bulk ESS around 167--225
in its hard four-chain case and creates three failures in a one-chain hard
case.

The compact forensics show a persistent component-scale/root direction.
Across candidates, the median correlation between `log_q_1` and mean width is
about -0.46 for M10 and -0.70 for M11. For M11, the corresponding
`log_q_1`--`log_lambda` correlation is only about 0.39. Increasing complete
scan distance reduces some short-lag dependence but is nonmonotone across
hard cases. A further blind x16 candidate is therefore neither the best
diagnostic nor a responsible production correction.

## Exact joint-path transition

Conditional on the component scale, pseudo-AL latent variables, and loss
rate, each root has a Gaussian state prior. Stacking both roots therefore
gives a Gaussian prior even though the augmented observation term is quartic
jointly in the root states. This distinction is essential: one simultaneous
Gaussian FFBS draw is unavailable, but an elliptical-slice transition is
valid.

Let `x` stack the time-zero state and states 1 through T for both roots, with
Gaussian prior mean `mu` and covariance `Sigma(q)`. Conditional on the current
latent variables, the path-dependent log kernel is

```text
-1/2 sum_t [ {(y_t-eta_1t)(y_t-eta_2t)-xi v_t}^2 / V_t ].
```

The transition draws a zero-mean prior direction `nu ~ N(0,Sigma(q))` and
searches the ellipse

```text
x(theta) = mu + (x-mu) cos(theta) + nu sin(theta)
```

with the ordinary slice bracket. It updates the two roots jointly without
linearizing the product residual. The Gaussian prior cancels from the slice
acceptance calculation, and no response-likelihood or response-simulation
interpretation is introduced. The move is composed with, not substituted for,
the existing sequential rootwise FFBS and centered/noncentered component-scale
updates.

## Predeclared development comparison

The candidate family is fixed before execution:

| Order | Candidate | Complete-scan multiplier | Joint path cycles | Composition |
|---:|---|---:|---:|---|
| 1 | `joint_ess1_x1` | 1 | 1 | rootwise then interweave then joint path |
| 2 | `joint_ess1_x2` | 2 | 1 | rootwise then interweave then joint path |
| 3 | `joint_ess2_x2` | 2 | 2 | rootwise then interweave then joint path |
| 4 | `joint_ess1_reverse_x2` | 2 | 1 | interweave then rootwise then joint path |

Only the already frozen M10/M11 cases are used:

- M10: S05 replications 104 (one-chain hard) and 129 (four-chain guard);
- M11: S05 replications 74 (four-chain hard), 172 (one-chain hard), and 190
  (one-chain guard).

This gives 44 jobs: four candidates times eleven fixed chain jobs. Candidate
selection is lexicographic in the table order. A candidate is eligible only
if every hard and guard diagnostic passes unchanged, every fit succeeds under
`numerical_policy="fail"`, the exact joint target is retained, repairs are
zero, and no retry or reseeding occurs.

## Validation gates

Before the 44 jobs, the implementation must pass:

1. direct equality of the implemented augmented log kernel and its manual
   expression, including omitted observations;
2. dimensional and numerical-policy boundary tests;
3. recorded exact-move diagnostics and zero repairs;
4. uninterrupted versus continued bitwise equality with the move enabled;
5. native package tests and a clean committed-source preflight bound to the
   maximum seed ledger and isolated exdqlm attestation.

If no candidate passes, the run remains closed and the next bounded project
must derive a different exact joint scale/path move. Replication replacement,
threshold relaxation, selective extension, and x16 escalation are prohibited.

If a candidate passes, the next gates are, in order:

1. apply the selected M10/M11 transition uniformly by method;
2. apply the already selected x2 schedule to M01, M02, M06, and M09;
3. rerun all 35 tasks in the local-level-skewed affected wave from a new root;
4. recompute the production resource envelope, particularly for the
   high-incidence M01, M02, and M06 methods;
5. build a fresh isolated runtime and rerun the complete exact-promotion suite;
6. create a separate flag-only authorization commit; and
7. launch a fresh 8,400-task study from wave 1 under a new run ID.

No article result, comparison table, coverage claim, or scientific promotion
is authorized by this development comparison.
