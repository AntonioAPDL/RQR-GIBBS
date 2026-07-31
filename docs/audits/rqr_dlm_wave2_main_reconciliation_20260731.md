# RQR-DLM wave-2 recovery/main reconciliation

Date: 2026-07-31 UTC

## Decision

The passing M03/M08 development evidence remains applicable after merging
`origin/main` at `764a35fe795e27312f21aa378fd29a3763ac650a` into the
recovery lane.

The mainline delta added the separate oracle-tilt v2 workflow and associated
documentation. Its only path-level overlap with the recovery changes was the
Makefile. The conflict was resolved by retaining the union of the oracle-tilt
v2 targets and the DLM M03/M08 validation targets. Both Makefile routes parse
successfully.

## DLM source identity

The complete affected-wave gate ran from
`5945c9417b222801f4761c27aa2a050d4c1adc8b`; its compact closeout was committed
at `b6f0de2ee84d7147cca47087d30db299a55f4b9c`. The reconciliation merge is
`a10132c7dca27587f71a156aaa67dc68a2ce58ca`.

Every launch-critical DLM implementation, configuration, validation, and test
object listed in `rqr_dlm_wave2_main_reconciliation_objects_20260731.csv` has
the same Git blob identity at `b6f0de2` and `a10132c`. Therefore the mainline
merge cannot change the chains or diagnostic outcomes represented by the
1,462/1,462 development-gate pass.

## Reconciled-source checks

The following suites passed from the merged source:

- `test-rqr-dlm-confirmatory-contract.R`;
- `test-rqr-native-sampler.R`; and
- `test-rqr-oracle-tilt-publication-v2.R`.

Compiler outputs produced by source loading were moved to the ignored cache
directory
`application/cache/compiler_artifacts_from_reconciled_tests_a10132c_20260731`.
No compiler object remains under `application/src/`, and the tracked worktree
is clean.

## Promotion boundary

This reconciliation does not convert development evidence into promotion
evidence. The launch flag remains false. A fresh isolated primary runtime must
be built from the final clean `main` commit, and the complete exact-promotion
matrix must pass before a separate authorization commit is considered.
