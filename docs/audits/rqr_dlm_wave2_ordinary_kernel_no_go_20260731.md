# RQR-DLM wave-2 ordinary-kernel comparison: no-go closeout

Date: 2026-07-31  
Scope: development-only M03/M08 computational diagnostics  
Source commit executed: `b4bfb94e29e0819057819ec81c5efbc618ecacfb`  
Authorization state: fail-closed

## Decision

The ordinary-kernel comparison does not authorize a schedule correction or a
confirmatory relaunch. The complete output manifest verifies, and all 20 M03
jobs completed with the exact joint target and zero numerical repairs. None of
the five predeclared M03 candidates resolved S03 replication 117. The six M08
jobs did not reach fitting because the development script's reduced seed subset
omitted the required forecast RNG keys. That M08 harness defect is corrected in
source and must be rerun from a new ignored output root; it is not evidence
against M08.

The generalized-Bayes target, response streams, priors, estimands, and frozen
diagnostic thresholds were unchanged. No scientific response-performance
quantity was used for selection. The failed confirmatory outputs and these
development outputs remain excluded from the confirmatory analysis.

## M03 disposition

All three guard replications (S03 replications 13, 90, and 185) passed every
diagnostic under every candidate. S03 replication 117 remained separated into
two basins under every ordinary transition schedule.

| Candidate | Burn | Retained | Complete kernels | Failed hard-case diagnostics | Minimum bulk ESS | Maximum R-hat | Maximum MCSE/SD |
|---|---:|---:|---:|---:|---:|---:|---:|
| Current | 500 | 1,500 | 1 | 40/41 | 3.0125 | 1.2620 | 0.6524 |
| Burn diagnostic | 3,000 | 1,500 | 1 | 41/41 | 3.1535 | 1.2487 | 0.6625 |
| Retention diagnostic | 500 | 6,000 | 1 | 40/41 | 3.5007 | 1.2199 | 0.6553 |
| Uniform long | 3,000 | 6,000 | 1 | 40/41 | 3.6580 | 1.2077 | 0.6539 |
| Two-kernel composition | 1,500 | 3,000 | 2 | 40/41 | 3.6512 | 1.2083 | 0.6535 |

Profile B remains in a higher-loss basin: its mean observed loss is about 148,
compared with about 126 for profiles A, C, and D. Increasing burn-in, retained
draws, or complete-kernel repetitions does not remove that separation. The
baseline candidate reproduces all 164 original M03 diagnostic rows for the
four frozen cases exactly: R-hat, bulk ESS, tail ESS, MCSE, MCSE/SD, and pass
flags have zero discrepancy.

This pattern rejects the hypothesis that the failure is ordinary marginal
autocorrelation. It supports a separate exact mode-bridging transition. The
next development comparison therefore uses likelihood-tempered replica
exchange and does not continue increasing the same Gibbs scan.

## M08 harness diagnosis

Every M08 error occurred after model fitting was requested but before a
diagnostic table could be constructed. The reduced ledger contained each
`method|...` state but not the corresponding `forecast|...` state required by
`rqr_confirm_dynamic_fit()`. Error digests differed by replication and repeated
across the two schedules, consistent with the missing replication-specific
forecast key. The corrected script now includes and hashes both method and
forecast states. Only the original six predeclared M08 jobs will be rerun.

## Integrity record

- jobs declared: 26;
- successful jobs: 20;
- M03 fit executions: 80;
- M08 fit executions reaching completion: 0;
- selected M03 candidate: none;
- selected M08 candidate: none;
- numerical repairs in successful jobs: zero;
- candidate `artifact_hashes.csv`: verified independently after completion;
- ignored chain-evidence SHA-256:
  `fa3dcd20e58bad75720067102c91125d5dcc68ab4449f70438d07417e89931c3`;
- candidate-decision SHA-256:
  `593e5a4fbe45f54788859580831ddc05a86313783bd66f391e2aa93329872422`;
- diagnostics SHA-256:
  `c1b4d72a50499b7bd8c2c1e9cf96d24a47a0eab236813fdf11a253f8841c58b9`.

The ignored source output is
`application/cache/rqr_dlm_wave2_failure_candidates_b4bfb94_execute_20260731`.
It is development evidence only and must not be copied into a confirmatory run.

