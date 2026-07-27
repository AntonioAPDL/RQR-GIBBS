# Ordinary RQR v1 protected-DLM companion evidence contract

## Purpose

The ordinary-RQR v1 release ladder must acknowledge the existing native
RQR-DLM implementation without rerunning, copying, or interpreting heavy
dynamic fits inside the ordinary static validation runner. The companion
collector provides that boundary. It validates four fresh, closed evidence
directories and publishes only a compact, recursively hashed summary.

The collector does not fit a model, read a fitted-model RDS object, or define a
response likelihood or response-simulation distribution. It records evidence
for the loss-based generalized posterior and its interval-root state
algorithms.

## Authoritative inputs

The collector accepts exactly one directory for each role:

1. the `reference-only` output of
   `application/scripts/08_run_rqr_dlm_bounded_validation.R`;
2. the M01 correction bundle produced by
   `application/scripts/22_validate_rqr_dlm_wave1_corrections.R`;
3. the M02 comparator-projection bundle produced by
   `application/scripts/23_validate_rqr_dlm_wave1_comparator_projection.R`;
4. the horizon/M03 bundle produced by
   `application/scripts/24_validate_rqr_dlm_horizon_and_fixed_design.R`.

Every directory is a closed regular-file set. A missing file, extra file,
nested directory, symbolic link, size mismatch, or SHA-256 mismatch is fatal.
The collector captures the complete closed ledger before semantic reads, then
rechecks both directory closure and every captured byte/hash immediately before
publication.
The reference bundle must contain all 43 gates, all six
fixture-by-learning-rate continuation cells, all 27 rehashed continuation
mutations, the canonical missing-data and public future-root checks, and the
component-scale conditional checks. The correction bundles must establish the
M01 interweaving kernel, M02 projection, 16 horizon scenarios, eight M03
standard tasks, and the dynamic endpoint boundary.

Diagnostic `pass` flags are not trusted by themselves. The collector
recomputes the frozen one-chain and four-chain R-hat, bulk-ESS, tail-ESS, and
MCSE/SD rules; requires each exact estimand grid; and reconciles the M01
`log(q)` sidecars and the M02/M03 diagnostic extrema.

All four bundles must identify one exact primary source commit, package
version, isolated primary runtime tree, and runtime attestation. M01, M02, and
horizon/M03 must also share the exact configuration, incidence, and seed-ledger
digests. Historical bundles from different source commits are useful audit
records but cannot be combined into release evidence.

## Invocation

Run the collector from a clean `main` checkout at the full expected commit:

```text
Rscript application/scripts/30_bundle_rqr_ordinary_v1_protected_dlm_evidence.R \
  <expected-commit> \
  <reference-only-directory> \
  <M01-directory> \
  <M02-directory> \
  <horizon-M03-directory> \
  <new-ignored-output-directory>
```

The output directory must not exist and must be ignored by Git. The command
validates that `HEAD` is the expected full SHA, the branch is `main`, and the
worktree is clean before it reads evidence.

For source-only unit tests, set:

```text
RQR_DLM_COMPANION_SOURCE_ONLY=YES
```

The reusable entry point is:

```text
rqr_dlm_companion_bundle()
```

The four role-specific validators are:

```text
rqr_dlm_companion_validate_reference()
rqr_dlm_companion_validate_m01()
rqr_dlm_companion_validate_m02()
rqr_dlm_companion_validate_horizon()
```

The cross-bundle validator is:

```text
rqr_dlm_companion_cross_validate()
```

The reusable validator for the published compact directory is:

```text
rqr_dlm_companion_validate_compact(
  directory,
  expected_commit,
  expected_runtime_tree_digest,
  expected_runtime_attestation_sha256,
  expected_package_version,
  expected_collector_commit = expected_commit
)
```

This function is the intended release-runner integration boundary. It performs
no fitting and does not require the four original evidence directories.

## Compact output schema

The collector atomically publishes one directory containing exactly:

| File | Role |
|---|---|
| `bundle_manifest.json` | Versioned source, runtime, configuration, scope, and no-fit contract |
| `input_bundle_summary.csv` | One summary row per protected input role |
| `input_artifact_hashes.csv` | Size and SHA-256 for every input artifact, including heavy artifacts by hash only |
| `semantic_gates.csv` | Sixteen explicit semantic acceptance gates |
| `artifact_hashes.csv` | Recursive manifest for the other four compact output files |

The schema is:

```text
rqrgibbs_ordinary_v1_protected_dlm_companion/1.0.0
```

The output manifest explicitly records:

```text
fits_executed_by_collector = false
heavy_input_artifacts_copied = false
generalized_bayes = true
response_likelihood = false
response_prediction_contract = false
```

The final directory rename is atomic on the target filesystem. Existing output
paths are never overwritten.

## Integration boundary

The ordinary-RQR release runner consumes the compact companion directory, not
the four source evidence directories and not their RDS files. It verifies the
companion schema, expected source and collector commits, the disabled
candidate runtime attestation and tree digest recorded by BENCH01, all 16
semantic gates, and the recursive output manifest. The release runner does
not silently construct the companion evidence or relax any of its gates.
Before the first bounded fit it atomically retains the exact five validated
files under `protected_dlm_companion/` in the final compact output, validates
that copy again, and binds it through both the R artifact manifest and the
post-R process-wrapper manifest.

The compact validator closes the five-file set, rehashes all four payload
files, verifies the 16-gate identity and order, checks every
source/runtime/scope field, and reconciles the four-row input summary against
the 39-row input-artifact ledger. A rehashed internal inconsistency remains a
failure.

This companion is evidence that the protected RQR-DLM implementation passed its
bounded mechanical, numerical, continuation, and correction checks at the
identified source/runtime state. It is not empirical coverage validation,
forecasting superiority evidence, a matched simulation result, or a
posterior-predictive response contract.

## Focused validation

The focused test
`application/tests/testthat/test-rqr-native-ordinary-v1-protected-dlm-companion.R`
builds small synthetic bundles without running a fit. It covers the positive
contract and adversarial cases for extra files, symbolic links, failed
reference gates, incomplete continuation cells, altered component-scale
conditionals, disabled interweaving, runtime mismatch, failed horizon evidence,
source-commit mismatch, incomplete or duplicated diagnostic grids, malformed
resource evidence, invalid endpoint dimensions, false diagnostic pass flags,
summary-sidecar drift, input-ledger drift, and rehashed compact-bundle
inconsistencies. The validators have also been read-only checked against the
historical reference, M01, M02, and horizon/M03 directory schemas.
