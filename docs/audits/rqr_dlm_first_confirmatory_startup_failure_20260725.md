# First confirmatory startup failure

Date: 2026-07-25

## Status

The first detached coordinator was started with run ID:

```text
rqr-dlm-main-20260725-5e36468
```

It used authorization commit:

```text
5e36468c2d1c645a53c54ec76c55fab7d0742d17
```

The exact-runtime preflight passed 22 of 22 gates and the oracle/reference
stage passed 15 of 15 gates. The coordinator then stopped while initializing
the first canonical wave. No wave-state contract, simulation fit, or model
output was created.

The preserved ignored evidence is under:

```text
application/runs/rqr_dlm_main_20260725_5e36468/
application/logs/rqr_dlm_main_20260725_5e36468_supervisor/
```

The terminal error was a JSON lexical error. The run root is treated as
non-resumable and will not be reused.

## Cause

The detached supervisor had been corrected to read the official primary
runtime attestation as RDS. The direct wave launcher still called
`jsonlite::read_json()` on that same RDS file. This failed before the launcher
created the append-only wave-state root, drew an RNG value, or started a
worker.

The subsequent digest comparison also referred to the JSON-style field
`runtime_tree_digest`; the official RDS schema
`rqrgibbs_runtime_attestation/5.0.0` stores
`runtime_package_tree_digest`.

## Correction

The execution flag is reset to `FALSE` in the correction commit. The direct
wave launcher now:

1. reads the primary attestation with `readRDS()`;
2. requires schema `rqrgibbs_runtime_attestation/5.0.0`;
3. requires the attested source commit to equal the exact authorization
   commit;
4. compares `runtime_package_tree_digest` with the authorization bundle;
5. requires the installed primary runtime path to exist; and
6. requires the executing `rqrgibbs` namespace to come from that exact
   attested path.

The confirmatory contract suite now checks that the direct wave launcher uses
the RDS interface and the correct digest field.

Because the source is commit-bound, the correction requires a new one-line
authorization commit, isolated runtime, preflight, oracle/reference bundle,
authorization bundle, and fresh run ID. The failed run supplies no reusable
scientific result.
