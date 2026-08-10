# Oracle-tilt version-4 source implementation closeout

## Decision

The prospective three-candidate illustration-screen source is implemented and
passes its source, package, exact-runtime preflight, and independent-reference
gates at commit
`f5a79ef7edbc6d1e33340fe4ab40f098a9cfd2a0`.

This closeout does **not** authorize the 18-cell production execution. The
tracked configuration retains `execution_authorized=false`. No version-4 MCMC
candidate fit, candidate selection, evidence promotion, figure change, or
manuscript change was performed in this implementation pass.

## Implemented contract

| Item | Frozen result |
|---|---:|
| Candidate data sets | 3 per model family |
| Candidate/family/target cells | 18 |
| Fixed-design chains | 36 |
| DLM chains | 45 |
| Total chains | 81 |
| Concurrent fit processes | at most 18 |
| Chain workers per cell | 1, sequential |
| Named DGP streams | 9 unique L'Ecuyer-CMRG states |
| Selection unit | one candidate per family across RQR, ET, and SH |
| DLM/SH retained draws | 12,000 per chain |
| Other retained draws | 6,000 per chain |
| Automatic manuscript promotion | prohibited |

The implementation includes canonical target-shared DGP envelopes, full RNG
state/digest manifests, source/config/runtime-bound worker contracts, atomic
and resumable endpoint-only worker artifacts, 18-process orchestration,
process-group monitoring, structured failures, read-only health reporting,
deterministic selector replay, and compact review packaging.

The resource audit raised the total process/thread ceiling from the original
draft value to 64 while retaining a hard limit of 19 R processes and 18 fit
workers. This distinction is necessary because simultaneous runtime-lineage
checks can briefly create Git or shell helper processes. A 24-process or
30-task ceiling could falsely terminate an otherwise compliant 18-fit run.
The user-level launcher therefore uses `TasksMax=72` while the monitored
wrapper still enforces 19 R processes, single-thread numerical libraries, and
the 18-cell fit plan.

## Validation matrix

| Gate | Result | Scope |
|---|---|---|
| R source parsing | pass | all new V4 R scripts and tests |
| Shell syntax | pass | orchestrator, wrapper, rehearsal, health, launcher |
| Focused V4 test file | pass | 60 expectations |
| V3 regression test file | pass | unchanged V3 construction |
| Package build/check | pass | `R CMD check --no-manual`, status OK |
| Exploratory monitored preflight | pass | wrapper lifecycle smoke |
| Exploratory monitored reference | pass | 29/29 gates |
| Scaled DLM/SH storage cell | pass | endpoint-only identity and manifest |
| Production launcher fail-closed test | pass | exit 65; authorization false |
| Exact isolated runtime build | pass | package `0.1.0.9030` |
| Exact-runtime monitored preflight | pass | final implementation commit |
| Exact-runtime monitored reference | pass | 29/29 gates |
| Full production benchmark | pending | intentionally not run in this pass |
| Full-scale 18-process rehearsal | pending | blocked while another heavy RQR validation is active |
| 18-cell/81-chain production execution | not authorized | not launched |

The scaled resource-cell smoke is only a source test. It is not acceptable as
the resource input to production; the resource closeout sets compact evidence
eligibility only when `scale=1` and all 18 full-shape cells pass.

## Exact source and runtime evidence

```text
source commit:
f5a79ef7edbc6d1e33340fe4ab40f098a9cfd2a0

application Git tree:
80cd0db668ce5a7817d360966f25df4fd9c5b4c9

configuration SHA-256:
3fbbf25d1ad3c588bf13ce4268401601f6f567ecbdeab636d081a2e409542794

isolated runtime tree digest:
e35c2ecd6a0f1e69b6df9808bb15a76a8796d53a52adf45bbfec5622f9185386
```

Exact preflight evidence:

```text
closeout SHA-256:
b13b7f6b39fde06e9336e2da67a796e66ad7f709f3ffe18d4121c0a2f27c31a3

artifact manifest SHA-256:
3123da13888d966d5501c07aff3786b8045b710f88b876e0af00876415cd9e85

wrapper manifest SHA-256:
5702c6e33daa21d3732610436c08bb5c5247c023c728d15a73e7c249773fcd5b
```

Exact reference evidence:

```text
reference gates: 29/29

closeout SHA-256:
4b2fd2372bf697255a1fe9aa248dfa87ab70081691eded5323eb0b87159b8fee

artifact manifest SHA-256:
6a30442b286dcc853576872776130c0e0bd22ff6df7d5091de63fdde753134fd

wrapper manifest SHA-256:
bb8b259464d2652d457c4719cf64c0bb30f15013d91609183d7b8013707cfe40
```

Raw runtime libraries and validation artifacts remain outside tracked source
or under ignored `application/cache/` storage.

## Required next stages

1. Wait until the separate RQR-DLM validation releases its active workers.
2. Independently review this exact source commit and frozen configuration.
3. Run the complete two-cell benchmark using the exact preflight and reference
   bundles.
4. Run the full-scale 18-process resource rehearsal and fault tests on an idle
   host.
5. Review benchmark timing, storage, process, thread, and RSS evidence.
6. If accepted, create a commit changing only `execution_authorized` from
   `false` to `true`, rebuild the isolated runtime, and repeat the bound
   preflight/reference/benchmark/rehearsal chain at that authorization commit.
7. Launch the 18 cells once. Do not add candidates or alter selection rules.
8. Replay the selector independently, package compact results, and review the
   selected family-level candidates before any figure or manuscript change.

The production study remains a prospectively screened single-data
illustration. Completion would not justify repeated-sample coverage,
calibration, posterior-predictive, or typical-performance claims.
