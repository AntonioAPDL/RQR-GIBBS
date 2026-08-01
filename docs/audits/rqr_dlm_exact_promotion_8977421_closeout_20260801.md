# RQR-DLM exact-promotion closeout

Date: 2026-08-01 UTC

## Decision

The corrected, fail-closed RQR-DLM implementation is eligible for a separate
flag-only authorization commit. The heavy statistical evidence at
`89774216c5dcc55b936e5a9a16eaa453c5d54c25`, its reconciled-source identity at
`d103f1f6a495e91314e91bc9255f5128f52d8a1c`, and the final hermetic checks all
pass. This closeout does not itself authorize or launch the main study.

Both execution flags remained false throughout promotion. No main-study task
was run, no failed or development output was promoted, and no comparative
simulation metric was inspected during transition selection or promotion.

## Statistical promotion evidence

| Gate | Work | Result |
|---|---:|---:|
| M01 wave 1 | 20 tasks; 44 chains; 920 diagnostics | 920/920 pass |
| M01 wave 2 | 25 tasks; 49 chains; 1,150 diagnostics | 1,150/1,150 pass |
| M02 wave 1 | 20 tasks; 88 endpoint fits; 900 diagnostics | 900/900 pass |
| M02 wave 2 | 25 tasks; 98 endpoint fits; 1,125 diagnostics | 1,125/1,125 pass |
| horizon/fixed design | 16 horizons; 8 fits; 328 diagnostics | 328/328 pass |
| M03/M08 targeted | 7 cases; 19 fits; 299 diagnostics | 299/299 pass |
| M03/M08 full wave | 34 cases; 52 fits; 1,462 diagnostics | 1,462/1,462 pass |
| resource envelope | four retained-state shapes | pass |

Across the seven diagnostic files, all 6,184 recorded gate evaluations passed.
The targeted M03/M08 rows are intentionally represented again in the full-wave
gate. Maximum rank-normalized R-hat was 1.003904 and maximum MCSE/SD was
0.06789831. M03 replica-exchange acceptance ranged from approximately .342 to
.493, with at least 152 complete round trips. Thresholds were unchanged,
required numerical repairs were zero, and all promotion fits used exact-target
transitions.

The largest observed worker peak was 921,032 KiB against the 1,572,864 KiB
ceiling. The resource-envelope fixtures passed the required margin and every
retained object deserialized successfully.

## Evidence integrity

The heavy evidence root is local-only:

```text
application/cache/rqr_dlm_exact_recovery_8977421_20260731_20260731T230841Z
```

All 41 entries declared by its eight recursive artifact manifests were
rehashed and byte-count checked. The supervisor gate-status SHA-256 is
`22068f241452200271d2cba8ec5ff80194f24fc3961832629f25ad5432e57280`.
Heavy fits and chain evidence remain ignored.

The compact tracked validation matrix records each manifest identity and the
diagnostic extrema. The source-equivalence ledger contains 34 launch-critical
Git objects or trees; all 34 match exactly between the heavy source and
reconciled main.

## Disposition of the exit-126 supervisor stop

The first supervisor completed its three runtime preparations, seven
statistical gates, and resource gate, then invoked
`application/scripts/28_run_rqr_dlm_promotion_checks.sh` as an executable.
That tracked file has Git mode `100644`; the shell returned permission denied
before executing its body.

The reconciled rerun invoked the identical script through `bash`. This is a
transport correction, not an inferential or package-source change. All 13
hermetic stages then passed:

- isolated check-runtime installation and environment smoke test;
- standalone DLM contracts;
- native, native mean-tilt, and oracle-illustration tests;
- `R CMD check --no-manual`;
- pinned exdqlm archive/build guard and RQR smoke test;
- isolated article and supplement builds;
- literature manifest;
- runtime-digest invariance, clean source, and zero compiler artifacts.

The hermetic evidence root is local-only:

```text
application/cache/rqr_dlm_final_hermetic_d103f1f_20260801
```

Its 1,021 manifest entries all rehash and byte-count verify. Key hashes are:

| Artifact | SHA-256 |
|---|---|
| gate status | `6220eb231c7200cf9e315651216221c0a53b6d57ed31ca644ff6c0602cc2c344` |
| runtime binding | `f0952219406486ec03dd65c27e9bd12604c6ce25924a6c717d78bdd01d4738ce` |
| recursive artifact manifest | `48c84de9d159c3f4cb35b7a4f5cdecdd6edf471545b74ade745deabdfec67f78` |

The primary runtime digest was
`aa24a1541ea04f98be8a3c392826a307620eb9420493d96fd723fc4a721c3951`
before and after the hermetic checks.

## Protected scope and interpretation

The exdqlm checkout was read-only. The pinned source was materialized by Git
archive and built only under ignored storage; its before/after guard passed.
The Q-DESN repository was not accessed or modified by this promotion.

The validated RQR-DLM remains a loss-based generalized-Bayes interval-root
model. These checks do not create a response likelihood or a
posterior-predictive response simulation contract.

## Authorization boundary

The commit containing this closeout is the reviewed implementation parent.
The next commit may change only
`confirmatory_execution_authorized = FALSE` to `TRUE`. After that change, a
fresh exact-runtime preflight and oracle-reference bundle must pass before the
8,400-task study starts under a new run ID. Any additional diff or failed gate
voids authorization.
