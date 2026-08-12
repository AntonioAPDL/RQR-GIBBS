# Failed confirmatory RQR-DLM run closeout

Run `rqr_dlm_diagnostic_aware_completion_20260810_ca4575e` is terminally failed.
It is retained only as immutable computational failure evidence.
No partial result is promoted or reused as confirmatory evidence.

The run stopped at canonical wave 2; 14 of 16 workers passed.
The worker logs contain 1 distinct authenticated error signature(s).
See worker_error_signatures.csv for the compact failure evidence.
Diagnostic thresholds, seeds, targets, and response laws were not changed
during this closeout.

All 439 entries in the completed wave manifests were rehashed and
verified from local bytes. Any stale coordinator lock was observed but
left untouched as part of the failed-run evidence. Collection and final
audit artifacts remain absent after the wave failure.

See root_cause_and_relaunch_plan.md for the recovered provenance-gate
message, root-cause diagnosis, and detached launch-source relaunch contract.

The generalized-Bayes update remains a loss update for interval roots;
it is not an ordinary response likelihood and does not define posterior-
predictive response draws.
