# Reconciliation of the first v2 oracle-tilt execution

## Decision

The first version-2 execution stopped correctly after the fixed-design RQR
cell. No later family/target cell was launched, and no result was promoted to
the manuscript. The population recovery, numerical, and provenance evidence
was favorable; the sole failure was a modest local bulk effective-sample-size
shortfall. The frozen correction composes two complete exact Gibbs transitions
between retained fixed-design draws. It does not change the generalized
posterior, its learning rate, its prior, its data, its seeds, or any recovery
or diagnostic threshold.

This is a single-data interval-root validation. It is not a repeated-sample
simulation study, and the root draws are not posterior-predictive response
draws.

## Source and immutable inputs

The failed cell used scientific source commit
`764a35fe795e27312f21aa378fd29a3763ac650a`, configuration SHA-256
`55279051d817bc7ca18bdf5d970fca9e45a0213984a60eeb649b14c0e8c2d73b`,
and an isolated runtime built from that exact Git archive. Its preflight passed
all eight design gates, and its reference stage passed all twelve independent
Gaussian-conditional and R/C++ gates with zero repairs.

An earlier benchmark attempt was rejected because a parallel workflow advanced
a shared checkout during the long DLM chain. The fit-level provenance guard
detected the change. The benchmark was rerun from a standalone immutable clone
and passed both SH cells. The promotion-eligible immutable benchmark recorded:

| Family | Relative endpoint RMSE | Width ratio | Repairs | Pathology | Result |
|---|---:|---:|---:|---:|---|
| Fixed design | 0.0406 | 0.9545 | 0 | none | pass |
| Dynamic linear roots | 0.0231 | 1.0136 | 0 | none | pass |

## First-cell result

Four fixed-design RQR chains each retained 6,000 draws after 1,500 warm-up
iterations. The cell passed all recovery criteria:

| Quantity | Value |
|---|---:|
| Endpoint RMSE / oracle width | 0.0627 |
| Mean-width ratio | 1.0187 |
| Pointwise joint oracle-endpoint inclusion | 1.0000 |
| Lower bias / oracle width | -0.0295 |
| Upper bias / oracle width | -0.0107 |
| Edge/center endpoint-RMSE ratio | 1.4823 |
| Realized content in the frozen data | 0.9542 |

Every checked variable satisfied rank-normalized `R-hat <= 1.01`, tail ESS at
least 1,000, and `MCSE/SD <= 0.05`. Eleven local lower-endpoint, midpoint, or
width ordinates had bulk ESS between 885.8 and 995.9, below the unchanged
threshold of 1,000. Their R-hats were between 1.0050 and 1.0068, their tail ESS
values exceeded 2,600, and their `MCSE/SD` values were about 0.032--0.034. All
fits had zero numerical repairs and exact source/runtime provenance.

## Correction rationale

Lowering the ESS threshold would be a post hoc weakening and is rejected.
Changing the data seed, prior, target, or learning rate would change the
scientific illustration and is also rejected. Merely storing more adjacent
draws would increase memory while retaining the same autocorrelation
mechanism.

The package exposes `kernel_repetitions`, whose default value one is bitwise
identical to the historical transition. Setting it to two composes two complete
invariant Gibbs kernels before each retained fixed-design draw. A composition
of invariant kernels leaves the same declared augmented target invariant. It
records the repetition count in the fit schema and provenance, keeps 6,000
retained draws per chain, and avoids increasing the already substantial
curve-summary memory footprint. A focused nonzero-tilt test guards this use
directly. Replica exchange remains disabled.

The DLM scan, all population targets, the exact tilt calculations, and all
predeclared gates remain unchanged. The corrected source must receive a new
isolated runtime, preflight, reference, and benchmark bundle before execution.
As before, execution stops after the first failing family/target cell.

## Process and toolchain-envelope correction before relaunch

The first corrected launch was stopped by the resource monitor before its
first cell was summarized. A 0.2-second telemetry sample observed five
processes and six threads, versus the initial ceilings of three and four. Only
two chain workers were active, no cell diagnostic had been evaluated, and no
later cell had started. Peak sampled RSS was about 1.61 GiB, so memory was not
the cause. Deterministic `2+2` and `2+2+1` chain batches were introduced to
make the chain-worker ceiling explicit.

A second exact-source attempt demonstrated that batching alone did not remove
the transient. Two read-only forensic reproductions sampled process names at
the boundary. In each case the process group contained the parent R process
and exactly two R chain workers. One worker briefly owned a two-process
administrative subtree while recording provenance: one reproduction captured
`timedatectl`, and another captured the shell process used by a read-only
toolchain command. Thus the five-process sample did not represent four
simultaneous chains or unreaped workers.

The final wrapper distinguishes the substantive concurrency limit from the
complete process-group envelope. It enforces at most three R processes (one
parent plus two chain workers), while allowing at most seven total processes
and eight threads for the two workers' short-lived provenance/toolchain
helpers. `TZ=UTC` is exported before R starts to remove host-dependent timezone
discovery. The 12-GiB RSS ceiling and the two-worker statistical concurrency
remain unchanged. Chain seeds, targets, transitions, diagnostics, and recovery
gates are unchanged. The broader process/thread envelope is an explicit bound
on administrative descendants, not authorization for additional MCMC workers.
