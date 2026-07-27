# Ordinary RQR v1 protected-DLM companion evidence contract

## Purpose

The ordinary-RQR v1 release ladder must acknowledge the existing native
RQR-DLM implementation without rerunning, copying, or interpreting heavy
dynamic fits inside the ordinary static validation runner. The companion
collector provides that boundary. It validates seven fresh, closed evidence
directories and publishes only a compact, recursively hashed summary.

The collector does not fit a model, read a fitted-model RDS object, or define a
response likelihood or response-simulation distribution. It records evidence
for the loss-based generalized posterior and its interval-root state
algorithms.

## Authoritative inputs

The collector accepts exactly one distinct directory for each role:

1. the `reference-only` output of
   `application/scripts/08_run_rqr_dlm_bounded_validation.R`;
2. the wave-1 static-Gaussian M01 correction bundle produced by
   `application/scripts/22_validate_rqr_dlm_wave1_corrections.R`;
3. the wave-1 static-Gaussian M02 comparator-projection bundle produced by
   `application/scripts/23_validate_rqr_dlm_wave1_comparator_projection.R`;
4. the wave-2 local-level M01 correction bundle produced by script 22;
5. the wave-2 local-level M02 comparator-projection bundle produced by
   script 23;
6. the horizon/M03 bundle produced by
   `application/scripts/24_validate_rqr_dlm_horizon_and_fixed_design.R`;
7. the resource-envelope bundle produced by
   `application/scripts/25_validate_rqr_dlm_resource_envelope.R`.

The exact role identifiers are:

```text
dlm_reference
wave1_M01_static_gaussian
wave1_M02_static_gaussian
wave2_M01_local_level
wave2_M02_local_level
horizon_M03
resource_envelope
```

| Role | Closed artifact count | Opaque heavy RDS count |
|---|---:|---:|
| `dlm_reference` | 22 | 2 |
| `wave1_M01_static_gaussian` | 5 | 1 |
| `wave1_M02_static_gaussian` | 5 | 1 |
| `wave2_M01_local_level` | 5 | 1 |
| `wave2_M02_local_level` | 5 | 1 |
| `horizon_M03` | 7 | 1 |
| `resource_envelope` | 6 | 0 |

Every directory is a closed regular-file set. A missing file, extra file,
nested directory, symbolic link, size mismatch, or SHA-256 mismatch is fatal.
The collector captures the complete closed ledger before semantic reads, then
rechecks both directory closure and every captured byte/hash immediately before
publication. Across the seven roles, the closed input ledger contains exactly
55 artifacts. Seven are heavy RDS artifacts. They are treated as opaque bytes:
the collector hashes them but never deserializes or copies them.
The reference bundle must contain all 43 gates, all six
fixture-by-learning-rate continuation cells, all 27 rehashed continuation
mutations, the canonical missing-data and public future-root checks, and the
component-scale conditional checks. Every M01 fit must record its complete
transition contract and recomputed digest in the compact summary, and must
match an independently frozen role-specific contract before its producer can
pass. The static-regression role freezes the collapsed coordinate label
`regression`; the local-level role freezes `level`. Their full digests are
therefore intentionally different. Cross-wave agreement is evaluated through
`rqrgibbs_dlm_transition_kernel_invariant/1.0.0`, which removes only that
scientifically meaningful component label and retains every other transition
field. The correction bundles must also establish M02 projection and schedule
contracts, 16 horizon scenarios, eight M03 standard tasks, the dynamic
endpoint boundary, and the resource-envelope contract.

Diagnostic `pass` flags are not trusted by themselves. The collector
recomputes the frozen one-chain and four-chain R-hat, bulk-ESS, tail-ESS, and
MCSE/SD rules; requires each exact estimand grid; and reconciles the M01
`log(q)` sidecars and the M02/M03 diagnostic extrema.

The seven roles must reconcile to one exact primary source commit, package
version, isolated primary runtime tree, and runtime attestation. The relevant
M01, M02, and horizon/M03 evidence must also share the exact configuration,
incidence, seed-ledger, transition-kernel, exdqlm-runtime, and comparator
schedule contracts required by their roles. The resource role must bind the
same primary source/runtime state and the confirmatory configuration. Its
independently hashed toolchain table is also normalized and compared exactly
with the reference runtime's R version, platform, compiler, BLAS, LAPACK, and
`digest`, `jsonlite`, `posterior`, and `rqrgibbs` versions. Matching runtime
tree and attestation hashes alone are not accepted as a substitute.
The compact manifest retains both the reference runtime-toolchain digest and
the resource toolchain-table digest after that semantic comparison.
Historical bundles from different source commits are useful audit records but
cannot be combined into release evidence.

## Invocation

Run the collector from a clean `main` checkout at the full expected commit:

```text
Rscript application/scripts/30_bundle_rqr_ordinary_v1_protected_dlm_evidence.R \
  <expected-commit> \
  <reference-only-directory> \
  <wave1-M01-directory> \
  <wave1-M02-directory> \
  <wave2-M01-directory> \
  <wave2-M02-directory> \
  <horizon-M03-directory> \
  <resource-envelope-directory> \
  <new-ignored-output-directory>
```

The output directory must not exist and must be ignored by Git. The command
validates that `HEAD` is the expected full SHA, the branch is `main`, and the
worktree is clean before it reads evidence.

The Makefile target exposes the same ordered contract through:

```text
RQR_EXPECTED_PRIMARY_COMMIT
RQR_DLM_REFERENCE_DIR
RQR_DLM_WAVE1_M01_DIR
RQR_DLM_WAVE1_M02_DIR
RQR_DLM_WAVE2_M01_DIR
RQR_DLM_WAVE2_M02_DIR
RQR_DLM_HORIZON_M03_DIR
RQR_DLM_RESOURCE_ENVELOPE_DIR
RQR_ORDINARY_V1_DLM_COMPANION_OUTPUT_DIR
```

After setting all nine values, run:

```text
make bundle-ordinary-v1-dlm-companion
```

For source-only unit tests, set:

```text
RQR_DLM_COMPANION_SOURCE_ONLY=YES
```

The reusable entry point is:

```text
rqr_dlm_companion_bundle()
```

The five role-family validators are:

```text
rqr_dlm_companion_validate_reference()
rqr_dlm_companion_validate_m01()
rqr_dlm_companion_validate_m02()
rqr_dlm_companion_validate_horizon()
rqr_dlm_companion_validate_resource()
```

The M01 and M02 validators each receive the exact wave role and reject
cross-wave substitution. M01 additionally verifies every per-fit contract
ledger entry, the complete role contract, its digest, and the versioned
cross-wave invariant.

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
no fitting and does not require the seven original evidence directories.

## Compact output schema

The collector atomically publishes one directory containing exactly:

| File | Role |
|---|---|
| `bundle_manifest.json` | Versioned source, runtime, configuration, scope, and no-fit contract |
| `input_bundle_summary.csv` | One summary row per protected input role |
| `input_artifact_hashes.csv` | Size and SHA-256 for every input artifact, including heavy artifacts by hash only |
| `semantic_gates.csv` | Twenty-three explicit semantic acceptance gates |
| `artifact_hashes.csv` | Recursive manifest for the other four compact output files |

The schema is:

```text
rqrgibbs_ordinary_v1_protected_dlm_companion/2.1.0
```

The output manifest explicitly records:

```text
fits_executed_by_collector = false
heavy_input_artifacts_deserialized = false
heavy_input_artifacts_copied = false
generalized_bayes = true
response_likelihood = false
response_prediction_contract = false
```

The final directory rename is atomic on the target filesystem. Existing output
paths are never overwritten.

## Integration boundary

The ordinary-RQR release runner consumes the compact companion directory, not
the seven source evidence directories and not their RDS files. It verifies the
companion schema, expected source and collector commits, the disabled
candidate runtime attestation and tree digest recorded by BENCH01, all 23
semantic gates, and the recursive output manifest. The release runner does
not silently construct the companion evidence or relax any of its gates.
Before the first bounded fit it atomically retains the exact five validated
files under `protected_dlm_companion/` in the final compact output, validates
that copy again, and binds it through both the R artifact manifest and the
post-R process-wrapper manifest.

The compact validator closes the five-file set, rehashes all four payload
files, verifies the 23-gate identity and order, checks every
source/runtime/scope field, and reconciles the seven-row input summary against
the 55-row input-artifact ledger. It also requires the reviewed 29-file
protected source inventory, including the correction producers, confirmatory
helper, and correction-budget contract. The two role-specific transition
digests, their invariant digest, and both runtime/resource toolchain digests
are cross-bound to their semantic-gate details. A rehashed internal
inconsistency remains a failure.

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
summary-sidecar drift, cross-wave substitution, transition-kernel or schedule
drift, per-fit transition-ledger drift, independently rehashed resource
toolchain disagreement, input-ledger drift, and rehashed compact-bundle
inconsistencies. The
validators have also been read-only checked against the reference, both M01
and M02 waves, horizon/M03, and resource-envelope directory schemas.
