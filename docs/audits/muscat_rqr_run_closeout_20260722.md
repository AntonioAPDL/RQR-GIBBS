# Muscat RQR Run Closeout

Date: 2026-07-22

## Purpose

This note records the safe closeout of the old Muscat RQR-DESN article-congruent
simulation after the RQR work was moved to the standalone `RQR-GIBBS` project.
The run was stopped because the scientific target changed: the standalone paper
will plan RQR-DESN and RQR-DLM together on Jerez rather than continuing the
partial Q-DESN-article integration run on Muscat.

## Closed Run

```text
session: rqr_article_n300_tau1em6_lambda1em3_20260719
run root: /data/jaguir26/local/src/exdqlm__wt__qdesn_0p4p0_integration/reports/rqr_desn_article_congruent_simulation/run_package_ready_full_n300_tau1em6_lambda1em3_20260719_git_dffb71ee70b5
config: /data/jaguir26/local/src/exdqlm__wt__qdesn_0p4p0_integration/config/rqr_desn/rqr_desn_article_congruent_n300_tau1em6_lambda1em3_20260719.R
```

## Pre-Stop State

| Quantity | Value |
|---|---:|
| Selected launch rows | 8,040 |
| Scenario status files | 415 |
| Completed scenario statuses | 415 |
| Interval metric files | 417 |
| MCMC diagnostic files | 417 |
| Nonempty failure logs | 0 |
| Aggregate metric summaries | 0 |
| `.rds/.rda/.RData/.rdata` payloads | 0 |
| Run root size | 72 MB |

The run was therefore partial and not a promotion-grade simulation artifact.

## Stop Action

The tmux session was stopped with `Ctrl-C` through:

```text
tmux send-keys -t rqr_article_n300_tau1em6_lambda1em3_20260719 C-c
```

Post-stop checks confirmed that the tmux session was gone and that no matching
`run_rqr_desn_article_congruent_simulation.R` process remained.

## Retention Decision

No output files were deleted. The partial run is small, contains no heavy model
objects, and may remain as traceability for why the Muscat path was retired.
Future production simulations should be launched from the standalone
`RQR-GIBBS` workflow on Jerez after the matched RQR-DESN/RQR-DLM design is
frozen there.
