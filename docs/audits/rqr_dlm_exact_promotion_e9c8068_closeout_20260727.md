# RQR-DLM exact-promotion closeout at `e9c8068`

Date: 2026-07-27 UTC

## Decision

The false-flag implementation commit
`e9c8068b4d9f135b7d717c3b072754f3b13f1e1a` is not eligible for
authorization. Five of its six exact-runtime promotion gates passed, but the
complete second-wave M01 gate failed 6 of 1,150 fixed diagnostics. The
execution flag remained false, no authorization commit was created, and no
main-study process was launched.

This is a computational mixing failure, not a source-lineage, numerical,
fitting, memory, or statistical-target failure.

## Exact gate disposition

| Gate | Work completed | Result |
|---|---:|---:|
| M01 wave 1 | 44 chains; 20 tasks; 920 diagnostics | pass, 920/920 |
| M01 wave 2 | 49 chains; 25 tasks; 1,150 diagnostics | fail, 1,144/1,150 |
| M02 wave 1 | 44 interval chains; 88 endpoint fits; 900 diagnostics | pass, 900/900 |
| M02 wave 2 | 49 interval chains; 98 endpoint fits; 1,125 diagnostics | pass, 1,125/1,125 |
| horizon/fixed design | 16 horizon scenarios; 328 M03 diagnostics; dynamic endpoint | pass |
| resource envelope | four conservative retained-state shapes | pass, 657,396 KiB |

Every fit in the failed M01 gate succeeded, was exact-target and
reproducibility eligible, used the same isolated runtime tree, and remained
below the resource margin. The failures were:

| Replication | Estimand | Bulk ESS | Tail ESS | MCSE/SD |
|---:|---|---:|---:|---:|
| 13 | observed loss | 169.28 | 419.18 | 0.0782 |
| 13 | `log_q_1` | 94.42 | 244.56 | 0.1032 |
| 55 | `log_q_1` | 175.93 | 464.52 | 0.0758 |
| 94 | `log_q_1` | 117.16 | 304.94 | 0.0926 |
| 165 | `log_q_1` | 126.12 | 424.44 | 0.0888 |
| 166 | `log_q_1` | 167.15 | 535.15 | 0.0772 |

All five affected tasks were ordinary one-chain standard-role fits. Thresholds
were not weakened, no chain was extended, and no failed result is reused.

## Diagnosis and narrow correction

The one-root partially collapsed transition integrated root 1 while
conditioning on root 2. It substantially improved the four-chain sentinel
boundary, but it left the component scale coupled to the conditioned root
within each iteration. A global root-label swap after the completed sweep
preserves exchangeability but does not remove that within-sweep dependence.

The next transition composes the same exact partially collapsed block in both
orientations:

1. update the scale under the root-1 Kalman marginal conditional on root 2,
   then redraw root 1 and its time-zero state;
2. update the scale under the root-2 Kalman marginal conditional on the
   refreshed root 1, then redraw root 2 and its time-zero state;
3. apply the existing centered inverse-Gamma and noncentered ASIS transition;
4. optionally swap both complete root labels.

Each rootwise marginal-update/conditional-redraw block leaves the same
augmented generalized posterior invariant. Their composition changes the
transition, not the generalized posterior, priors, seeds, schedules,
estimands, or diagnostic thresholds. This remains a loss-based update and
does not define posterior-predictive response draws.

## Authenticated ignored evidence

| Artifact | SHA-256 |
|---|---|
| M01 wave-1 manifest | `830c8b05d856e9ba43c32500e47818209bf6ea1cb963f78b7ed403a6416844ba` |
| M01 wave-2 manifest | `ad16c7dc0cd5c125e4336d5acf52419026367a99bb651be60826d2e8e1a41150` |
| M01 wave-2 diagnostics | `333dae9324206ff449271a3da4048ccbe3cbabe963b8a6f5bd40f66c56dba44b` |
| M01 wave-2 summary | `29936a17e98b441a2784ad77ff0f4ad01ddd9ed69afbb51f63ca3425d6c8a9a9` |
| M02 wave-1 manifest | `341cc1149e4815bd3dace59705f43d9b5b3752d0511569485970e142ab01df81` |
| M02 wave-2 manifest | `45920cc4a590f2f61bbfb59ea0a6d15918902250ec568fa25bd620da7bdb7dff` |
| horizon manifest | `41ab9af29cd5fab6dffa3f96ebc9b51bf54cd653efe9bb2057c5ecf34783b947` |
| resource manifest | `e81213469d2431f7be6f910b740f23f68da1f89565e33b5b8e2e3bd7167b7a25` |

The complete ignored gate root is
`application/cache/rqr_dlm_promotion_e9c8068_20260727`. Heavy chain evidence
remains local-only.

## Relaunch rule

The symmetric rootwise, two-ASIS correction must pass development validation,
complete source/package/document tests, and a new clean exact-runtime
promotion. The current finish plan is recorded in
`docs/implementation_notes/rqr_dlm_two_ASIS_finish_plan_20260727.md`. Any
failure keeps execution false. A flag-only authorization and fresh background
main launch remain prohibited until that sequence is complete.
