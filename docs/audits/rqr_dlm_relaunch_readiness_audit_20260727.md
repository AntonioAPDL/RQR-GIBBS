# RQR-DLM relaunch readiness audit

Date: 2026-07-27 UTC

## Executive decision

The third main-study run is terminal and must not be resumed. Its second
canonical wave exposed a singleton projection defect, inadequate
component-scale mixing, incomplete diagnostic-exception publication, and an
avoidable memory duplication. The source corrections are implemented with the
execution flag still false. The new component-scale transition leaves the same
fixed generalized posterior invariant and is supported by dense-Gaussian,
R/C++, continuation, and shortened mixing checks.

The source is ready for a clean implementation commit and exact-runtime
promotion gates. It is not yet eligible for a flag-only authorization commit.
The main study may be relaunched only if the complete first- and second-wave
M01 and M02 gates, horizon, resource, package, provenance, and document gates
all pass from the same clean implementation commit.

## Authenticated health state

| Boundary | Current state | Interpretation |
|---|---:|---|
| Failed run | `rqr_dlm_main_20260727_ce02915` | immutable local evidence |
| Authorization commit | `ce02915f8e6270fb21c4cce1bdc231beeda12292` | superseded after failure |
| Canonical waves terminal | 2 / 110 | 1 passed, 1 failed |
| Canonical waves remaining | 108 | permanently blocked in this run |
| Active wave or coordinator | none | terminal fail-closed stop |
| Current wave artifacts | 1.800 GiB | ignored; not reused |
| Final audit | absent | no scientific result is promotable |
| Current source execution flag | false | launch remains closed |
| exdqlm reference | `dffb71ee70b597d6a716ee74be1cbc99731cd453` | tracked-clean, read-only; pre-existing ignored compiler files unchanged |
| Q-DESN reference | `f9f22804eff3871bb5350c8add04b7c9f4d4957b` | clean, read-only |

The failed-wave artifact-manifest SHA-256 is
`418b6facad514e09dc7fe3650c8c172c88fa82cb84054b11506bc885284a039c`.
No earlier fit, draw, endpoint estimate, or scientific metric is copied into a
replacement run.

The exdqlm source checkout contains pre-existing ignored compiler artifacts.
The protected-checkout guard includes ignored files and confirmed that their
complete state did not change while the pinned commit was materialized,
built, installed, and tested below the RQR-GIBBS ignored cache.

## Finding disposition

| Finding | Root cause | Implemented correction | Relaunch gate |
|---|---|---|---|
| singleton exdqlm projection | base-R extraction dropped the state dimension when \(p=1\) | preserve `p x T` and `p x draws`, then project through `FF` | synthetic and actual isolated CRAN 1.1.0 checks |
| M01 component-scale failure | strong scale--trajectory dependence remained after centered--noncentered ASIS | integrate one root in an exact partial-collapse scale transition, redraw it by FFBS, then update the other root and apply ASIS | complete M01 gates for canonical waves 1 and 2 |
| M02 invalid multi-chain comparison | initialization profiles changed `m0`, hence changed the target | hold every target field fixed and use only CRAN's precomputed MCMC-state warm-start interface | common-target and distinct-initialization digests in both wave gates |
| post-fit diagnostic exception | diagnostic construction was outside the structured failure boundary | publish a hashed failure row, erase method metrics, and stop systemically | failure-injection and source-contract tests |
| worker RSS at 95.1% of ceiling | full sentinel fits and an immediate RDS readback coexisted | compact each chain immediately, release it, and validate RDS bytes in a clean process | conservative retained-state resource envelope |
| possible silent relaunch drift | correction touches transition, schedules, and evidence schema | keep execution false; bind config, seed, runtime, and artifacts to exact commits | isolated runtime plus flag-only authorization |

## Statistical audit of the transition

Condition on root 2, its time-zero state, and the current latent RQR scales.
The root-1 conditional pseudo-observation model is Gaussian. Let
\(\ell_{1,\mathrm K}(\boldsymbol q)\) be its Kalman log marginal after
integrating root 1 and its time-zero state. For component \(j\), let

\[
E_{2j}=\frac12\sum_{t=1}^T
d_{2jt}^{\mathsf T}Q_{jt}^{-1}d_{2jt}.
\]

With \(x_j=\log q_j\), the scale kernel is

\[
\ell_{1,\mathrm K}(\exp\boldsymbol x)
-\sum_j\left[
\left(a_j+\frac{Td_j}{2}\right)x_j+
(b_j+E_{2j})e^{-x_j}
\right].
\]

The coefficient \(Td_j/2\) and energy \(E_{2j}\) come from the one
conditioned root. The integrated root contributes through the Kalman
marginal. Coordinate slice steps are finite Markov transitions that leave
this marginal invariant; they are not described as independent exact draws.
FFBS then redraws the integrated root at the accepted scale. The root-2
conditional update, centered inverse-Gamma update, noncentered ASIS update,
and global label swap are each invariant for the same augmented generalized
posterior. Their composition changes the transition but not the target.

This is an augmented generalized-Bayes loss update. The Gaussian calculation
is not a response likelihood, and root-state draws are not
posterior-predictive response draws.

## Prospective transition selection

Selection used only computational diagnostics on the already diagnosed S03
replication 28. It did not use coverage, width, loss contrasts, endpoint
recovery, or any other comparative scientific metric.

| Kernel | Retained draws | Chains | `log_q_1` R-hat | Bulk ESS | Tail ESS | MCSE / SD | Mean elapsed |
|---|---:|---:|---:|---:|---:|---:|---:|
| partial collapse, 2 sweeps | 1,500 | 4 | 1.0054 | 381.0 | 667.3 | 0.0513 | 459.2 s |
| partial collapse, 3 sweeps | 1,500 | 4 | 1.0014 | 405.4 | 892.6 | 0.0497 | 471.8 s |
| partial collapse, 6 sweeps | 1,500 | 4 | 1.0172 | 84.0 | 66.5 | 0.1162 | 478.4 s |

Three sweeps are frozen because they pass every checked estimand, cross the
unchanged bulk-ESS threshold, and add only 2.7% mean elapsed time relative to
two sweeps. Six sweeps demonstrate that more transitions are not assumed to
improve finite-chain behavior. These dirty-source development results are
selection evidence only and cannot authorize execution.

The ignored benchmark artifacts are authenticated by:

| Artifact | SHA-256 |
|---|---|
| single-chain benchmark CSV | `3732d4e45b7572be2a77f0089450ee76eb5f5a397da1383a132e0c1734eb3b15` |
| extended-sweep benchmark CSV | `bd59b3f60a3fa925e5ad18e5ab171bddd7acc4849a265755031d2661a9b51d34` |
| four-chain diagnostic CSV | `9bd742bdf8c4ac8da99987fd4cea82a8b50b9786e05a3afdf3e1e4ff349bbc66` |
| four-chain timing CSV | `e178f89367604c6dbb54cdef1cbe7a2b4f41123b1479ea99599413267ea8afad` |

## Completed source validation

| Gate | Result | Scope |
|---|---|---|
| native R/C++ tests | pass | algebra, FFBS, log marginal, sampler, continuation |
| standalone DLM/main contracts | pass | frozen design, schedules, artifacts, wave state |
| `R CMD check --no-manual` | pass | status OK |
| main article build | pass | 20 pages; no final undefined references |
| supplement build | pass | 24 pages; no final undefined references |
| environment smoke | pass | R, Git, TeX, required packages |
| literature manifest | pass | 18 local-only PDFs |
| pinned exdqlm smoke | pass | immutable Git archive; protected checkout unchanged |
| monitor fault test | pass | process-group cleanup and fail-closed exit |
| forbidden execution tests | pass | both direct and wave execution rejected |
| conservative resource envelope | pass | 612,688 KiB maximum versus 1,258,291 KiB margin gate |

These checks were run on the complete working source before commit. Promotion
still requires the affected computational gates from a clean isolated runtime
built from the final implementation commit.

## Workload and capacity

The maximum contract remains 110 canonical waves, 8,400 replication tasks,
40,938 MCMC chains, and 205,658,000 MCMC iterations. No diagnostic-dependent
extension, retry, or reseed is allowed. Component-scale methods account for
approximately 42.5%, 38.9%, and 37.6% of iterations in the initial, central,
and maximum plans.

For capacity planning only, applying the measured hard-case three-sweep timing
factor to every method gives conservative 32-worker envelopes of 156.1,
280.5, and 408.0 wall hours for the initial, central, and maximum plans. The
actual design proceeds in complete paired batches and may stop only under its
predeclared precision rule. These values are upper envelopes rather than
completion promises.

## Mechanical promotion and relaunch plan

1. Commit and push the false-flag implementation, tests, derivations, budget,
   closeout, and this audit.
2. Build `rqrgibbs` from `<commit>:application` with `git archive` into an
   isolated library outside the repository and verify the complete lineage.
3. Run complete M01 and M02 gates for canonical waves 1 and 2 in fresh ignored
   roots, with original seed states, frozen schedules, zero repairs, and every
   diagnostic threshold unchanged.
4. Run the exact-runtime horizon and resource gates; verify the actual CRAN
   comparator runtimes and protected-checkout guards.
5. Require every gate to pass. A single failure leaves execution false and
   prohibits selective retry or chain extension.
6. Create a separate commit whose only source difference is
   `confirmatory_execution_authorized = TRUE`.
7. Rebuild the isolated runtime from the authorization commit; regenerate
   preflight, oracle/reference, task-plan, runtime, and authorization bundles.
8. Start the complete coordinator under a fresh run ID and ignored run root.
   Do not copy any earlier wave.
9. Monitor only through the append-only wave ledger and process-group
   telemetry. A live PID is not evidence of a valid wave.
10. After all canonical decisions, run collection and final audit before
    producing tables, figures, comparisons, or manuscript claims.

## Decision rule

| Evidence state | Action |
|---|---|
| any exact-runtime, MCMC, projection, resource, package, provenance, or document failure | remain closed |
| every promotion gate passes | create the flag-only authorization commit |
| authorization preflight/reference mismatch | remain closed |
| authorization bundle passes exactly | start a fresh detached main coordinator |
| any launched wave fails | stop permanently and diagnose; never resume that run |
