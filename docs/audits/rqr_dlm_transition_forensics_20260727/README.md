# RQR-DLM transition failure forensic audit

Date: 2026-07-27

This compact audit digests the three completed M01 wave-2 evidence roots.
It is read-only: it does not relaunch fits, change thresholds, or reuse
failed/development outputs as promotion evidence.

## Attempt-level diagnostic summary

| Attempt | Diagnostics | Failed tasks | min bulk ESS log_q_1 | median bulk ESS log_q_1 | max MCSE/SD log_q_1 |
|---|---:|---:|---:|---:|---:|
| one_root_exact_e9c8068 | 1144/1150 | 5 | 94.42 | 342.78 | 0.1032 |
| symmetric_rootwise_one_ASIS | 1147/1150 | 2 | 108.33 | 398.17 | 0.0964 |
| symmetric_rootwise_two_ASIS | 1147/1150 | 2 | 117.73 | 437.29 | 0.0923 |

## Persistent hard cases

The persistent failures remain concentrated in ordinary one-chain S03
replications 13 and 94.  The guard replication 55 fails only in the
one-root exact attempt and is retained for the next development comparison.

## Output files

- `attempts.csv`: declared input evidence roots.
- `input_artifact_hashes.csv`: SHA-256 hashes for consumed artifacts.
- `manifest_summary.csv`: compact manifest fields by attempt.
- `chain_manifest.csv`: one row per retained scalar-chain object.
- `diagnostic_failures.csv`: failed diagnostics with method and role.
- `diagnostic_summary_by_attempt.csv`: attempt-level pass/fail counts.
- `effective_draws_per_second.csv`: log-q ESS normalized by wall time.
- `scalar_quantiles.csv`: quantiles for selected retained scalar draws.
- `autocorrelation_summary.csv`: ACF at lags 1, 5, 10, 25, and 50.
- `response_path_features.csv`: regenerated frozen DGP path summaries.
- `feature_failure_panel.csv`: path features joined to diagnostic status.

## Scope

These artifacts support computational transition diagnosis only. They are
not scientific simulation results, do not define response-predictive draws,
and do not authorize a main simulation launch.
