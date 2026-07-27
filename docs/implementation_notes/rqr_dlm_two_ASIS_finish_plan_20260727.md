# RQR-DLM two-ASIS finish plan

Date: 2026-07-27

## Purpose

This note records the current finish plan for the RQR-DLM main simulation
after the third launch stopped fail-closed and after the first exact promotion
of the one-root partially collapsed correction failed the complete M01
second-wave gate. It is an implementation and evidence plan, not a scientific
result.

The protected `exdqlm` and Q-DESN repositories remain references only. No
source file in either protected repository is modified by this plan.

## Current source state

The public `main` branch is anchored at
`e9c8068b4d9f135b7d717c3b072754f3b13f1e1a`, which is fail-closed:
`confirmatory_execution_authorized = FALSE`. The working tree contains a
candidate transition update with package version `0.1.0.9025` and fit schema
`rqrgibbs_fit/1.13.0`. The candidate is not yet a promotion commit.

The candidate transition leaves the same augmented generalized-Bayes target
invariant. It does not change the DGPs, priors, seeds, learning-rate modes,
diagnostic thresholds, MCMC iteration budgets, comparators, estimands, or
response interpretation.

## Evidence already closed

The third main-study launch
`application/runs/rqr_dlm_main_20260727_ce02915` is terminal and must not be
resumed. It passed one wave and failed the local-level sentinel wave. The
remaining 108 waves are blocked. No scientific comparison or partial
simulation metric from that run is admissible.

Commit `e9c8068b4d9f135b7d717c3b072754f3b13f1e1a` repaired the singleton
exdqlm projection, compact failure publication, memory sidecar behavior, M02
same-target warm starts, and the first component-scale partial-collapse
transition. Its exact promotion gates passed M01 wave 1, both M02 waves, the
horizon/fixed-design gate, and the resource gate. M01 wave 2 failed
6 of 1,150 diagnostics, all from exact and reproducibility-eligible fits.
That failure is closed in
`docs/audits/rqr_dlm_exact_promotion_e9c8068_closeout_20260727.md`.

A subsequent one-cycle symmetric rootwise development wave improved the same
affected M01 wave-2 gate to 1,147 of 1,150 diagnostics. It is useful
diagnostic evidence but not promotion evidence. The current candidate therefore
keeps the symmetric rootwise partial collapse and prospectively composes a
second exact centered--noncentered ASIS cycle.

## Statistical diagnosis

The one-root partially collapsed update integrates root 1 while conditioning
on root 2, then redraws root 1. This is target-invariant, but the scale can
remain strongly coupled to the conditioned path within a sweep. A global label
swap after the sweep preserves exchangeability but does not remove that
within-sweep dependence.

The symmetric rootwise update composes two invariant blocks:

1. update the component scales under the root-1 Kalman marginal conditional on
   root 2, then redraw root 1 and its time-zero state;
2. update the component scales under the root-2 Kalman marginal conditional on
   the refreshed root 1, then redraw root 2 and its time-zero state.

The two-ASIS candidate then applies two exact centered--noncentered
interweaving cycles. This is still a Markov transition for the same augmented
generalized posterior. It is not a response likelihood, and interval-root
draws remain root-state functionals rather than posterior-predictive response
draws.

## Live development gate

The current live development gate is:

```text
output root:
application/cache/rqr_dlm_wave2_two_ASIS_cycles_dev2_20260727

log:
application/logs/rqr_dlm_wave2_two_ASIS_cycles_dev2_20260727.log

process group:
3746762

workers:
32

wave:
local_level_gaussian_T200__target0200__sentinel

method:
M01 only
```

This is a development gate. It can reject the candidate. It cannot by itself
authorize the main study.

The development gate must finish with:

```text
49 chains
25 tasks
1150/1150 diagnostics passing
all fits succeeded
zero numerical repairs
resource margin pass
no reuse of failed outputs
```

If it fails any diagnostic, the execution flag remains false. The next action
is diagnosis of the failing estimands and chain roles; no threshold weakening,
selective chain extension, or result-triggered retry is allowed.

## Promotion path if the development gate passes

If the live development gate passes, the following source and evidence steps
are required before any authorization commit:

1. Record the development manifest, diagnostics, summary, and artifact hashes
   in the tracked closeout documents.
2. Update the correction budget with measured two-ASIS timing and resource
   evidence; update the corresponding config digest.
3. Re-run focused native sampler and confirmatory-contract tests.
4. Re-run the standalone DLM tests, `R CMD check --no-manual`, `make smoke`,
   `make pdf`, `make supplement`, and `make literature-manifest`.
5. Verify the protected exdqlm and Q-DESN checkout states with read-only Git
   commands and optional locks disabled.
6. Commit and push a fail-closed implementation commit. The execution flag
   must remain false.
7. Build a fresh isolated primary runtime from that exact commit.
8. Run exact promotion gates from the isolated runtime:
   M01 wave 1, M01 wave 2, M02 wave 1, M02 wave 2, horizon/fixed-design, and
   resource envelope.
9. If every exact gate passes, create a separate flag-only authorization
   commit whose only source change is `confirmatory_execution_authorized =
   TRUE`.
10. Rebuild the exact authorization runtime, regenerate preflight and oracle
    bundles, materialize the authorization bundle, and start one fresh
    detached coordinator under a new run ID.

No output from earlier failed or development runs may be copied into the main
run root.

## Launch rule

The main simulation can launch only after all exact promotion gates pass from
a clean implementation commit and after a verified flag-only authorization
commit. A running process is not success. Progress is read from the append-only
wave ledger, and any failed wave permanently blocks later waves in that run.

Scientific tables, figures, and manuscript claims are deferred until a final
run audit verifies the intention-to-run denominator, failure ledgers, artifact
hashes, and compact summaries.
