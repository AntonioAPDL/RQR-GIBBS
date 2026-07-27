# RQR-DLM second-wave horizon failure closeout

Date: 2026-07-26

## Decision

The run rooted at
`application/runs/rqr_dlm_main_20260726_bb96629` is terminal and must not be
resumed. Its authorization source was
`bb966299bb298ee31ec65d167edf53c44ce48b03`. The first canonical wave passed
its operational wave contract, and the second canonical wave failed. The
append-only state ledger permanently blocks all later waves.

No interval-performance summary, comparative estimate, or scientific metric
from either wave is eligible for reuse. The preserved files are diagnostic
evidence only. A replacement study must use a fresh authorization commit,
exact runtime, run identifier, run root, and supervisor-log root.

## Authenticated stopped-run inventory

| Item | Audited value |
|---|---|
| Run identifier | `rqr_dlm_main_20260726_bb96629` |
| Authorization commit | `bb966299bb298ee31ec65d167edf53c44ce48b03` |
| Runtime tree digest | `022f00f8ce775b0334d52cfbe78535259109e7f269466a7e131121f9fbd470d6` |
| Canonical wave contract | 110 waves |
| Terminal wave records | 2 |
| Passed waves | 1 |
| Failed waves | 1 |
| Later waves | 108, permanently blocked in this run |
| Wave-1 artifact-manifest SHA-256 | `57d951945b7ae52797a6b6bbc3abc16a23488e33b0817e431e542a4644072b88` |
| Wave-2 artifact-manifest SHA-256 | `e7371c51bf6813fc160f41f01fbc850a8a3d3321cd1461997477649e6efa7b19` |
| Replication folders materialized | 36 |
| Method-result rows | 100 |
| Completed rows | 80 |
| Diagnostic-failure rows | 4 |
| Execution-failure rows | 16 |
| Preserved run size | 1,931,123,851 bytes |

Wave 1,
`static_gaussian_T200__target0200__sentinel`, completed 20 task bundles and
published a pass record. Wave 2,
`local_level_gaussian_T200__target0200__sentinel`, planned 25 task bundles.
Sixteen bundles reached their first M01 execution and failed; the remaining
nine were not attempted. Every worker exited nonzero, without retry or
reseed.

The health checker now validates this state without a false schema error and
reports:

```text
state:                 stopped_after_failed_wave
terminal waves:        2 / 110
passed waves:          1
failed waves:          1
next canonical wave:   blocked_by_failed_wave
final audit present:   FALSE
```

The missing final audit is expected because this is a failed-run closeout, not
a completed-study closeout.

## Root cause

The failure is at the model/forecast interface, not in the generalized-Bayes
target, Gibbs transition, FFBS implementation, or component-scale update.
Time-invariant DLM constructors represent \(F_t\) by one observation-design
column. Training fits expand that column internally. The public root forecast,
however, defines its horizon from `ncol(FF_future)`. The confirmatory model
builder passed the one-column representation directly, so a requested
20-step forecast returned one endpoint per root.

The runner’s previous compound boundary grouped length, finiteness, and root
ordering in one error. It therefore reported the one-step result as a generic
nonfinite/unordered primary-output failure. All 16 wave-2 M01 failures have the
same message digest:

```text
c8f7ed064ce97e82bd2d4941eb8726a2ec338992c19b6cd8eda2eaa1bbfef3bb
```

Eleven of the 16 frozen scenarios use a time-invariant observation design and
were exposed to the same latent error: S03--S09 and S13--S16. The first wave
uses a regression design with one column per time point, so it did not expose
the defect.

## Corrections

The replacement implementation makes the horizon contract explicit:

1. every model is materialized separately at \(T\), \(H\), and \(T+H\);
2. a one-column time-invariant `FF` is repeated to the exact requested
   horizon;
3. incompatible time-varying `FF` or `GG` inputs fail before fitting;
4. the training and future designs must exactly partition the full design;
5. endpoint length, nonfiniteness, and ordering have separate failures; and
6. an endpoint-contract failure stops the affected cell immediately.

All 16 scenario structures passed their exact \(T/H/(T+H)\) contracts in the
development gate. The exact failed-wave seed stream S03/replication 13 then
completed the public M01 fit-and-forecast path with 200 training endpoints,
20 future endpoints, zero numerical repairs, and an exact-joint-target flag.
This development result must be repeated from the clean isolated
implementation runtime before authorization.

## Additional wave-1 computational finding

The passed wave contained four M03 one-chain standard fits that missed the
fixed diagnostic gates at 1,500 retained draws. This does not revoke the
wave’s operational record; a sentinel wave may contain task-level diagnostic
failures and still complete its method-sentinel purpose. It does prohibit
using the same standard schedule in a replacement study.

| M03 role | Tasks | Diagnostics passed | Minimum bulk ESS | Minimum tail ESS | Maximum MCSE/SD |
|---|---:|---:|---:|---:|---:|
| Four-chain sentinels, 1,500 retained | 4 | 164/164 | 554.795 | 1,203.598 | 0.0427 |
| One-chain standard, 1,500 retained | 8 | 292/328 | 120.634 | 210.221 | 0.0935 |
| One-chain standard, fixed 3,000 retained | 8 | 328/328 | 316.191 | 572.912 | 0.0574 |

Replications 72, 92, 114, and 151 caused the 36 original misses. Re-executing
all eight standard streams, rather than only those four, passed every
diagnostic at the fixed 3,000-draw schedule. The sentinel schedule remains
unchanged. This is a role-wide schedule fixed before the replacement run, not
an adaptive retry rule.

## Resource and protected-scope findings

No numerical-thread, sampled process-group thread, memory, or wall-time
ceiling was breached. Across 16 worker monitors, the maximum sampled
process-group RSS was 1,389,676 KiB and the maximum sampled thread count was
four, equal to the declared operating-system envelope. Numerical-library
threads remained fixed at one.

At closeout, 408 GiB remained available under `/data`. The pinned exdqlm
checkout remained at
`dffb71ee70b597d6a716ee74be1cbc99731cd453`, and the Q-DESN article remained
at `f9f22804eff3871bb5350c8add04b7c9f4d4957b`. Both were clean and were used
read-only.

## Corrected budget

The design remains 110 canonical waves, 8,400 task bundles, and 40,938 MCMC
chain executions at the maximum plan. Exact role accounting gives:

| Planning stage | Original iterations | Corrected iterations |
|---|---:|---:|
| Initial | 43,332,000 | 74,182,000 |
| Central | 80,484,000 | 136,640,000 |
| Maximum | 117,636,000 | 199,098,000 |

The complete machine-readable overlay is
`docs/audits/rqr_dlm_main_correction_budget_20260726.csv`. The maximum
32-worker safeguarded wall-time envelope is 355.8 hours. It is a conservative
ceiling, not a completion-time promise.

## Relaunch boundary

The stopped run remains diagnostic evidence only. Relaunch requires:

1. a clean fail-closed implementation commit;
2. complete source, package, manuscript, and monitor gates;
3. an isolated runtime from that exact implementation;
4. exact-runtime M01, M02, horizon, M03, and second-wave endpoint gates;
5. a separate flag-only authorization commit;
6. an isolated runtime and fresh preflight/reference evidence for that exact
   authorization commit; and
7. a fresh authorization bundle and output namespace.

Only the new run may contribute to the final confirmatory analysis.
