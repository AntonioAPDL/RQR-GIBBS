# Output-17 launch-boundary correction

Date: 2026-07-25

## Disposition

The Output-17 wave-replay correction passed its source validation and the
authorization-bound preflight and oracle/reference stages:

```text
preflight:        22/22 gates passed
oracle/reference: 15/15 gates passed
```

Both stages used the isolated `rqrgibbs 0.1.0.9018` runtime built from exact
authorization commit
`5a400ffbd9123e2925206cdc88949ca24429cb48`.

Before starting the detached coordinator, an interface mismatch was found in
`20_launch_rqr_dlm_confirmatory_simulation.sh`. The official primary-runtime
builder writes an RDS attestation whose installed package path is stored as
`runtime_package_path`. The launcher incorrectly attempted to parse that RDS
file as JSON and read a field named `runtime_path`. The comparator attestations
are JSON and correctly use `runtime_path`; the primary attestation is
intentionally a different format.

No confirmatory wave or simulation fit started. The failed initial
oracle-reference invocation was also fail closed: it stopped because the
preflight seed-ledger environment variable was absent, published no reference
directory, and was rerun successfully with the exact ledger.

## Correction

The confirmatory flag is reset to `FALSE` in this implementation commit. The
detached launcher now:

1. reads the official primary attestation with `readRDS()`;
2. retrieves `runtime_package_path`;
3. requires one existing directory;
4. normalizes the path; and
5. continues to parse the exdqlm and quantreg JSON attestations with `jq`.

The contract suite includes a source-boundary regression test that requires
the primary RDS extraction and rejects a return to `jq` for that block.

After validation, a new authorization commit may again change only
`confirmatory_execution_authorized = FALSE` to `TRUE`. Because the isolated
runtime and evidence are commit-bound, the primary runtime, preflight, and
oracle/reference artifacts must be rebuilt for that new authorization commit
before the main coordinator starts.
