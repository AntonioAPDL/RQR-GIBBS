# Failed diagnostic-aware maximum RQR-DLM run

## Disposition

Run `rqr_dlm_diagnostic_aware_maximum_20260807_ea8ea8d` is closed and is not
eligible for continuation, reuse, or scientific promotion. The coordinator is
not running. The first canonical wave failed through the declared global-stop
boundary, so all 109 later waves remained unauthorized.

The run was bound to source commit
`ea8ea8d17c6f7bb34b015472e4f60f62e547c942`. Its heavy local artifacts remain
under the ignored frozen-run root and are represented here only by compact
counts and hashes.

## Evidence summary

| quantity | value |
|---|---:|
| terminal waves | 1 / 110 |
| passed waves | 0 |
| failed waves | 1 |
| terminal DGP-replication tasks | 8 / 8,400 |
| remaining DGP-replication tasks | 8,392 |
| method-replication results | 16 / 43,800 |
| M01 results completed | 8 |
| M02 results failed during diagnostic construction | 8 |
| M01 diagnostics passed | 368 / 368 |
| retries | 0 |

All eight failures used the same class and message digest. The common pattern
is consistent with one deterministic diagnostic-assembly defect rather than
eight unrelated fits. The correction and relaunch gates are specified in
`docs/implementation_notes/rqr_dlm_m02_diagnostic_recovery_plan_20260808.md`.

No claim is made from the partial scientific outputs. In particular, this
run does not establish coverage, interval width, comparative performance, or
response-predictive validity.
