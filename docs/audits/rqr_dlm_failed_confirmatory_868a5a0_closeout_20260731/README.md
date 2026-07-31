# Failed confirmatory RQR-DLM run closeout

Run `rqr_dlm_confirmatory_868a5a0_20260730` is terminally failed.
It is retained only as immutable computational failure evidence.
No partial result is promoted or reused as confirmatory evidence.

The first canonical wave passed and the second failed. The failure was
diagnostic rather than numerical or resource-related: M08/S03 replication
13 had one marginal one-chain bulk-ESS failure, whereas M03/S03 replication
117 showed severe four-chain disagreement. Diagnostic thresholds, seeds,
targets, and response laws were not changed during this closeout.

All 442 entries in the two completed wave manifests were rehashed and
verified from local bytes. The stale coordinator lock was observed but
left untouched as part of the failed-run evidence. Collection and final
audit artifacts are absent, as required after a wave failure.

The generalized-Bayes update remains a loss update for interval roots;
it is not an ordinary response likelihood and does not define posterior-
predictive response draws.
