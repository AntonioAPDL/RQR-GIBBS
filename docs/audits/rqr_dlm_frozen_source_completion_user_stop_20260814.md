# RQR-DLM frozen-source completion run: user-requested stop

Date: 2026-08-14  
Run ID: `rqr_dlm_frozen_source_completion_20260811_3e8e50b`  
Authorization/source commit: `3e8e50b262d3101bf9d253620961207579748b59`  
Repository branch at closeout: `feature/bayesian-uq-authoritative-report6-20260812`  
Repository commit before this closeout: `e51ef2835b2cf4cc617ec45ff237b05fb6103f11`

## Scope

This note documents a user-requested stop of the detached RQR-DLM
frozen-source completion launch.  The stop was not a statistical failure, a
resource-envelope failure, or a numerical failure.  The partial run outputs are
preserved under the ignored run root for possible later audit or controlled
resumption.

No fitted objects, chains, logs, caches, or generated heavy artifacts are
promoted by this note.

## Paths

| Object | Path |
| --- | --- |
| Run root | `/data/muscat_data/jaguir26/.rqr_gibbs_frozen_runs/rqr_dlm_frozen_source_completion_20260811_3e8e50b/run` |
| Supervisor logs | `/data/muscat_data/jaguir26/.rqr_gibbs_frozen_runs/rqr_dlm_frozen_source_completion_20260811_3e8e50b/logs/supervisor` |
| Detached launch source | `/data/muscat_data/jaguir26/.rqr_gibbs_launch_sources/rqr_dlm_3e8e50b262d3101bf9d253620961207579748b59` |

## Stop action

The coordinator PID recorded by the run was `269449`.  The launch was stopped
by sending `SIGTERM` to the coordinator process group.  A follow-up process
check found no remaining matching simulation processes, so no `SIGKILL` was
required.

Process stop summary:

| Field | Value |
| --- | ---: |
| Processes matching the run before `SIGTERM` | 31 |
| Coordinator process group | `269449` |
| Processes after initial `SIGTERM` wait | 0 |
| Processes after second `SIGTERM` wait | 0 |
| Final matching simulation processes | 0 |

The post-stop process check used a run-ID/coordinator-PID pattern that excludes
the inspection command itself and returned no matches.

## Post-stop health snapshot

The project healthcheck script was run after the stop:

```text
RQR-DLM diagnostic-aware completion health check
  state: running_or_active
  run ID: rqr_dlm_frozen_source_completion_20260811_3e8e50b
  authorization commit: 3e8e50b262d3101bf9d253620961207579748b59
  coordinator PID: 269449
  coordinator running: FALSE
  terminal waves: 4 / 110
  passed waves: 4
  precision-stop skips: 0
  failed waves: 0
  active canonical wave: trend_seasonal_skewed_T200__target0200__sentinel
  next canonical wave: trend_regression_unequal_T200__target0200__sentinel
  terminal DGP-replication tasks: 98 / 8400
  remaining DGP-replication tasks: 8302
  completed method-replication results: 611 / 43800
  frozen diagnostics passed: 20416 / 20764
  result rows with diagnostic warnings: 28
  latest collection: none
  current wave artifact GiB: 0.599
  final audit present: FALSE
```

The `running_or_active` state is the healthcheck's conservative label for an
unclosed active-wave start record.  The same healthcheck reports
`coordinator running: FALSE`, and the direct process check reports no matching
run processes.

## Wave status at interruption

| Wave | Status | DGP/wave ID | Terminal DGP-replication tasks | Notes |
| ---: | --- | --- | ---: | --- |
| 1 | Passed | `static_gaussian_T200__target0200__sentinel` | 20 | Terminal completion record exists. |
| 2 | Passed | `local_level_gaussian_T200__target0200__sentinel` | 25 | Terminal completion record exists. |
| 3 | Passed with diagnostic warnings in results | `local_level_skewed_T200__target0200__sentinel` | 35 | Terminal completion record exists; no wave failure. |
| 4 | Passed with diagnostic warnings in results | `trend_seasonal_gaussian_T200__target0200__sentinel` | 15 | Terminal completion record exists; no wave failure. |
| 5 | Interrupted | `trend_seasonal_skewed_T200__target0200__sentinel` | 13 observed so far | Start record exists but no terminal completion record. |

Completed wave records:

| Completion | Wave ID | Decision | Task count | Completed UTC |
| ---: | --- | --- | ---: | --- |
| 1 | `static_gaussian_T200__target0200__sentinel` | `passed` | 20 | 2026-08-12 16:43:30 |
| 2 | `local_level_gaussian_T200__target0200__sentinel` | `passed` | 25 | 2026-08-13 01:48:36 |
| 3 | `local_level_skewed_T200__target0200__sentinel` | `passed` | 35 | 2026-08-14 05:38:28 |
| 4 | `trend_seasonal_gaussian_T200__target0200__sentinel` | `passed` | 15 | 2026-08-14 14:38:30 |

Unclosed active start:

| Start | Wave ID | Task count | Started UTC |
| ---: | --- | ---: | --- |
| 5 | `trend_seasonal_skewed_T200__target0200__sentinel` | 28 | 2026-08-14 14:40:12 |

## Diagnostic patterns in partial outputs

The run was intentionally diagnostic-aware: MCMC diagnostic warnings can be
recorded without causing a wave-level execution failure.  At the user-requested
stop, 28 method-result rows carried diagnostic-warning status.  These warnings
are not a stop cause for this closeout, but they remain relevant for later
interpretation.

Wave-level summaries at stop:

| Wave | Replication result files | Method rows | Diagnostic rows | Non-passing diagnostics | Rows marked failed/warning |
| --- | ---: | ---: | ---: | ---: | ---: |
| `static_gaussian_T200` | 20 | 84 | 2312 | 0 | 0 |
| `local_level_gaussian_T200` | 25 | 143 | 4502 | 0 | 0 |
| `local_level_skewed_T200` | 35 | 278 | 9935 | 55 | 10 |
| `trend_seasonal_gaussian_T200` | 15 | 90 | 3390 | 224 | 15 |
| `trend_seasonal_skewed_T200` | 13 | 70 | 2490 | 125 | 13 |

Main warning concentrations:

- `local_level_skewed_T200`: primarily `S05/M10` and `S05/M11`, with common
  estimands including `log_q_1`, `mean_width`, `mean_upper`, `mean_midpoint`,
  and `observed_loss`.
- `trend_seasonal_gaussian_T200`: primarily `S07/M03`, then `S07/M01` and
  `S07/M06`, with repeated endpoint/width diagnostics at selected training
  times.
- `trend_seasonal_skewed_T200`: partial interrupted wave; warnings were
  concentrated in `S08/M03`, `S09/M01`, `S08/M06`, and `S08/M01`.

These patterns should be treated as partial-run diagnostics, not final
simulation conclusions.

## Resume and promotion implications

The current run should be treated as interrupted and incomplete.

- Completed waves 1--4 have terminal completion records and may be useful for
  forensic summaries, planning, or controlled resumption checks.
- Wave 5 is partial and should not be interpreted as a terminal wave without an
  explicit partial-output analysis.
- There is no final collection and no final audit.
- No result from this run should be used as a final promoted simulation result
  unless a later documented protocol explicitly resumes or reconciles the
  interrupted run.
- If work resumes from this run root, use a controlled resume/continuation path
  that checks the existing wave state, run contract, source commit, runtime
  digest, and artifact hashes before launching more work.
- If a clean final run is preferred, use a new run ID and link this stop note as
  the reason the previous launch was abandoned.

The launch flag and execution authorization should not be changed by this
closeout.  A later resume or relaunch should be recorded as a separate
decision.

## Repository state note

At the time of this closeout, the worktree contained unrelated untracked local
files:

```text
academic_style_wickle.md
academic_style_wickle.txt
interpretation_coeffients.md
```

They were not staged or committed by this stop documentation pass.
