# RQR-DLM M02 seed-binding recovery validation

Date: 2026-08-02 UTC

## Scope and decision

Implementation commit
`dd783ec7598d7cdfcfe85487668813b7b26ca6b1` repairs the production
provenance serializer that stopped run `rqr_dlm_main_20260801_281802e`.
The correction is locally validated and remains fail closed. It does not
authorize a run by itself.

The failure was deterministic and post-fit: M02 consumed separate lower and
upper method/forecast RNG streams, but its compact diagnostic object requested
a nonexistent joint `interval` method key. The repaired object records every
stream it actually consumes as a structured table of stream kind, endpoint,
chain, task key, and state digest. The same endpoint/key constructors now drive
ledger planning, execution, preflight, wave validation, and serialization.

## Exhaustive planning result

The fresh maximum-design preflight resolved all 76,080 consumed stochastic
bindings before model execution. M02 contributes method and forecast bindings
for both `lower` and `upper`; it contributes no `interval` binding. M03 binds
only its method stream because its training and future endpoint summaries are
deterministic functions of the same posterior coefficient draws.

The maximum seed-ledger file remains byte-identical to the previous canonical
contract:

```text
rows including header: 388,958
SHA-256: 3dc8483f4a777ab766704b901997295bed1c89db0590429a70f3116b233e948f
```

## Source-equivalence result

The package inference source, compiled source, package metadata, 208-row
incidence matrix, run budget, and launch gates are Git-object-identical between
baseline `cebc77210716f29b201f1c0dfc1e133b6cf33347` and the recovery
implementation. The patch changes only fail-closed configuration provenance,
orchestration/provenance code, validation tests, shell entry-point robustness,
and audit documentation. It does not change the generalized-Bayes target,
transition kernel, schedules, seeds, scenarios, estimands, or diagnostic
thresholds. Exact object IDs are in `source_equivalence.csv`.

## Validation result

All current local gates passed:

- complete standalone DLM contracts;
- native R/C++ tests, with one documented DESN-only skip and one expected
  warning for the explicitly experimental adaptive-discount mode;
- monitor fault injection and process-group cleanup;
- fail-closed simulation and wave entry points;
- environment smoke test and maximum-design preflight;
- `R CMD check --no-manual` with status `OK`;
- article and supplement builds; and
- literature manifest generation.

The ignored local logs and preflight bundle are bound by SHA-256 in
`validation_matrix.csv`. They are development evidence, not substitutes for
the exact-runtime preflight, oracle-reference, and authorization bundle that
must be regenerated from the eventual flag-only authorization commit.

## Launch rule

After this fail-closed state is merged and pushed, authorization must be a
separate one-line flag-only commit. The launch runtime and all authorization
artifacts must be built from that exact commit. A fresh run ID must start from
wave 1. Wave 1 is the production-path qualification gate: the coordinator may
not start wave 2 unless all 20 sentinel tasks, eight workers, diagnostics,
hashes, provenance checks, and resource checks pass. No artifact from the
failed run is eligible for scientific analysis.

The posterior remains a generalized loss update for interval roots. These
root summaries are not posterior-predictive response draws.
