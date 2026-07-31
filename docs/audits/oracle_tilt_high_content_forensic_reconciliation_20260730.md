# Oracle-Tilt High-Content Forensic Reconciliation

Date: 2026-07-30  
Repository baseline: `581b36896852937b5f08135ff90627bcc2313fdb`  
Scope: one fixed simulated data set, content \(c=0.95\), standardized
asymmetric-Laplace innovations with index \(0.99\), exact population-oracle
tilts, and fixed learning rate.

## Decision

The forensic implementation is complete and its focused tests pass.
Fixed-design SH and DLM-ET are accepted for this bounded fixture. The original
DLM-SH specification is rejected because the near-boundary SH tilt interacts
with a diffuse initial-slope prior to create a remote, scientifically
uninformative posterior mode. A scale-consistent candidate with initial-slope
variance \(0.001\) removes that mode and has zero numerical repairs, but its
strict ESS acceptance remains open because the longer run did not complete
inside the declared resource envelope.

No \(c=0.95\) manuscript figure is promoted by this reconciliation.

## What was audited

The audit followed the complete path from the population target to retained
chain summaries:

1. exact population tilt admissibility and boundary margins;
2. response-scale transformation of the tilt;
3. omission of both pseudo-observation and canonical tilt sites at missing
   responses;
4. the Gaussian dynamic prior's response to the accumulated linear tilt;
5. the complete product-check, tilt, and Gaussian-prior target decomposition;
6. four dispersed starts plus a prior-shift stress start;
7. pure-R FFBS, C++ FFBS, and independently assembled dense Gaussian
   conditional references;
8. rank-normalized R-hat, bulk ESS, tail ESS, and maintained MCSE;
9. scale-relative endpoint and width diagnostics;
10. atomic, hash-bound, resumable worker artifacts.

These are interval-root generalized-posterior checks. They are not a response
likelihood validation, posterior-predictive check, repeated-sampling coverage
study, or comparison of forecasting methods.

## Compact health table

| Check | Completed | Result | Disposition |
|---|---:|---|---|
| Population geometry | 3 targets | SH tilt \(0.148266\), upper boundary \(0.149782\) | SH is admissible but only about 1% of the tilt range from the upper boundary |
| Fixed-design SH extension | 4 chains / 24,000 draws | zero repairs; all 14 diagnostics pass | accepted for this fixture |
| DLM-ET dispersed starts | 4 chains / 10,000 draws | zero repairs; all 20 diagnostics pass | accepted for this fixture |
| Original DLM-SH | 4 chains / 10,000 draws | remote mode; mean width about 10,611 versus oracle 1.664 | rejected |
| Conditional Gaussian references | all audited DLM chains | R/C++ equality and scale-relative dense agreement; zero repairs | implementation check passes |
| Initial-slope prior grid | 4 variances / 2 stress profiles | `1` and `0.1` remote; `0.01` start-sensitive; `0.001` stable | `0.001` is the only supported candidate |
| DLM-SH candidate, short acceptance | 5 chains / 30,000 draws | remote fraction zero; zero repairs; max R-hat 1.017; only local ESS gates fail | target scale stabilized; more effective draws required |
| DLM-SH candidate, long acceptance | requested 5 chains / 120,000 draws | no chain completed before the fixed 180-minute cutoff | computationally inconclusive |

## Crucial diagnosis

### The original DLM-SH behavior is not a plotting or numerical artifact

For the original `C0 = diag(4, 1)` specification, the fitted remote path aligns
almost perfectly with the prior response to the accumulated canonical tilt.
The complete target profile shows that the linear tilt gain overwhelms weak
initial-slope regularization until a finite remote mode. The Gaussian target
remains proper over the finite horizon, but propriety does not make this mode
scientifically useful.

The following checks rule out the main implementation alternatives:

- no covariance repair occurred;
- missing response times contribute neither measurement nor tilt sites;
- pure-R and C++ conditional smoothers agree;
- dense conditional residuals and covariance differences pass
  scale-relative tolerances;
- exact population tilts are used; no Cornish--Fisher approximation enters
  the fit.

### Why `0.001` is a defensible candidate

The candidate is not chosen merely because it makes the plot attractive.
Its initial-slope prior standard deviation is about \(0.0316\), compared with
a true initial slope magnitude \(0.015\) and slope-innovation standard
deviation \(0.006\). It remains broad relative to the fixture's state scale
while removing the effectively unconstrained long-horizon direction.

The candidate is still a model change. It therefore requires its own
acceptance run and must be disclosed if it is ever used in a manuscript
illustration.

## Completed implementation

The tracked implementation adds:

- a fail-closed forensic configuration and runner;
- a separate fail-closed DLM-SH acceptance template that freezes the
  supported prior, five starts, draw budget, and resource-safe one-worker
  schedule;
- reusable target-geometry, dense-reference, prior-shift, trace, target
  decomposition, and scale-pathology helpers;
- configurable proper initial level and slope variances for the illustration
  DLM constructor;
- a prior-shift stress initialization;
- scale-relative dense Gaussian reference checks;
- vectorized state-prior quadratic evaluation, regression-tested against its
  scalar definition;
- separate scale-stability and MCMC-diagnostic decisions for prior
  sensitivity;
- bounded fork-based chain parallelism;
- atomic trace/result publication and exact-contract worker resumption;
- focused tests for geometry, missing-site behavior, configurable priors,
  initialization dispersion, target decomposition, R/C++/dense agreement,
  vectorization, atomic checkpoints, fail-closed execution, and compact
  preflight artifacts;
- Make targets and application documentation.

All heavy traces and fitted objects remain under ignored application output
roots.

## Validation matrix

| Validation | Result |
|---|---|
| Environment smoke preflight | pass |
| Focused oracle-tilt illustration tests | pass |
| Focused oracle-tilt forensic tests | pass; one non-statistical `timedatectl` sandbox warning |
| Static-to-dynamic prior-quadratic vectorization regression | pass to \(10^{-10}\) tolerance |
| R/C++/dense conditional reference tests | pass with zero repairs |
| Fail-closed forensic preflight and artifact manifest | pass |
| Main manuscript build | pass; 21 pages |
| Supplement build | pass; 27 pages |
| `R CMD check --no-manual` | `Status: OK` |
| Git whitespace check | pass |

The protected exdqlm and Q-DESN repositories were not loaded, installed,
compiled, or modified during this work.

## Long-run cutoff interpretation

The longer candidate run requested five chains with 3,000 burn-in and 24,000
retained draws per chain. It ran from approximately 08:22 to 11:22 UTC. The
five concurrent workers produced no complete chain envelope before the
predeclared 180-minute cutoff. The process was interrupted, leaving only
preflight artifacts.

This result means:

- zero additional retained draws can be claimed;
- no ESS, R-hat, MCSE, or fit-quality decision can be computed from that run;
- the run is not evidence against the candidate target;
- the resource strategy was inefficient under the available CPU quota;
- the next acceptance attempt should use the same scientific contract with a
  scheduler allocation or lower bounded concurrency, relying on atomic
  per-chain checkpoints.

## Remaining gate and optimal next action

Only one scientific-computational gate remains for these \(c=0.95\)
illustrations: complete the five-start DLM-SH candidate acceptance run and
pass every declared R-hat, ESS, MCSE, zero-repair, conditional-reference, and
scale-relative criterion.

The next run should:

1. freeze `C0[2,2]=0.001`, the exact SH oracle, data, seeds, five starts,
   diagnostics, and numerical policy;
2. use a reviewed CPU allocation or one/two workers rather than five workers
   competing under a single-core-like quota;
3. retain the 24,000-draw per-chain budget unless a predeclared sequential ESS
   rule is implemented and tested first;
4. resume only from worker envelopes whose complete source/config digest and
   trace hash match;
5. stop without manuscript promotion if any target, scale, conditional,
   repair, or convergence gate fails.

Until that run passes, retain the existing manuscript figures. Even after it
passes, the result supports only this illustrative fixture and does not
establish empirical coverage calibration.
