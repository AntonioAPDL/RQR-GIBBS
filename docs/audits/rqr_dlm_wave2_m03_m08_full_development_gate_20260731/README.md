# RQR-DLM wave-2 M03/M08 full development gate

Date: 2026-07-31 UTC

## Decision

The complete affected-wave development gate passes and supports advancing the
fail-closed M03/M08 correction to exact-runtime promotion.

The gate ran from clean source commit
`5945c9417b222801f4761c27aa2a050d4c1adc8b`, after reconciliation with the
then-current `origin/main`. It used the frozen maximum seed ledger with
SHA-256
`3dc8483f4a777ab766704b901997295bed1c89db0590429a70f3116b233e948f`,
unchanged diagnostic thresholds, seven single-threaded workers, and the
production fit constructors.

This is computational development evidence, not a comparative simulation
result. It does not estimate empirical coverage or forecasting performance,
does not reinterpret the RQR update as an ordinary response likelihood, and
does not authorize the main study.

## Complete result

| Quantity | Result |
|---|---:|
| cases | 34 / 34 succeeded |
| chain fits | 52 / 52 succeeded |
| diagnostics | 1,462 / 1,462 passed |
| M03 diagnostics | 697 / 697 passed |
| M08 diagnostics | 765 / 765 passed |
| exact-target cases | 34 / 34 |
| cases with zero numerical repairs | 34 / 34 |
| operational M03 replica-exchange cases | 17 / 17 |

For M03, the minimum bulk ESS was 587.96, the minimum tail ESS was
973.42, the largest MCSE/SD was 0.04114, and the largest four-chain
rank-normalized R-hat was 1.00391. Across every chain and adjacent edge, swap
acceptance ranged from 0.342 to 0.493; every chain completed at least 152
round trips.

For M08, the minimum bulk ESS was 385.56, the minimum tail ESS was
1,057.56, the largest MCSE/SD was 0.05217, and the largest four-chain R-hat
was 1.00223. The minimum occurs at the prospectively retained hard case,
S03 replication 13, for the training-time lower root at time 200. It clears
the unchanged bulk-ESS, tail-ESS, and relative-MCSE requirements.

## Cross-run identity

The 299 scalar diagnostic rows shared by the earlier targeted gate and this
full-wave gate are numerically and logically identical in chain count, R-hat,
bulk ESS, tail ESS, MCSE, MCSE/SD, and pass status. Their `case_role` labels
differ intentionally (`targeted_*` versus `full_wave_*`). This identity is an
important routing check: the complete gate used the selected production
transition and schedules without introducing a separate validation-only
implementation.

## Integrity and scope

The local ignored evidence root is:

```text
application/cache/rqr_dlm_wave2_full_stress_5945c94_execute_20260731
```

Its seven-file recursive artifact manifest was independently rehashed; every
byte count and SHA-256 matched. The 33 MiB chain-evidence object remains
ignored. Compact identities and worst-case summaries are tracked in this
directory.

The source-loaded development manifest correctly records
`promotion_evidence = false`. Promotion now requires a fresh isolated runtime
built from the exact clean `main` commit and the complete M01, M02,
horizon/fixed-design, M03/M08, resource, package/native, TeX, and literature
matrix. The launch flag remains false throughout that process.

## Next gate

1. Commit and push this compact closeout while keeping execution fail-closed.
2. Integrate the recovery branch into clean `main` without discarding newer
   article work.
3. Build a fresh isolated primary runtime from the resulting full SHA.
4. Run the complete exact promotion matrix from fresh ignored output roots.
5. Stop on any failed diagnostic, numerical repair, lineage mismatch,
   resource failure, source drift, or document/package failure.
6. Only a complete exact-promotion pass can support a later, separate
   flag-only authorization commit and a fresh 8,400-task launch.
