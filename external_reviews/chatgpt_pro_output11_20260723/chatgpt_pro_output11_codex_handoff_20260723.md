# Copy-paste-ready Codex handoff after ChatGPT Pro Output-11

Treat this handoff and the accompanying audit as claims to reproduce. Modify only `AntonioAPDL/RQR-GIBBS`. Do not modify exdqlm or the Q-DESN article repository.

## Reviewed exact state

```text
implementation:
  e24feb411b2e30586d1bfdc18bf6acb1fb568c70

evidence:
  99b6f92911a5cd323b735598063da72766ad9095

package:
  rqrgibbs 0.1.0.9010

schemas:
  fit 1.8.0
  continuation 4.1.0
  runtime attestation 5.0.0
  bounded fixtures 5.0.0
  run 3.0.0
  reference bundle 2.0.0
  estimands 1.0.0
```

Protected read-only references:

```text
exdqlm:
  dffb71ee70b597d6a716ee74be1cbc99731cd453

Q-DESN article:
  f9f22804eff3871bb5350c8add04b7c9f4d4957b
```

## Pro decision

```text
Statistical target:                    PASS
Runtime lineage v5:                    PASS
Estimand schema:                       PASS
Future diagnostics:                    PASS
Continuation/RNG boundary:             PASS for bounded use
Varying-scale reference:               PASS
43 reference gates:                    PASS as tracked
6,000-draw benchmark:                  PASS, 150/150
Shell final evidence publication:      PARTIAL — blocker
RDS post-rename rollback:              PARTIAL — blocker
Enable 24 fits now:                    NO-GO
Matched/production simulation:         NO-GO
CAVI/ELBO:                             DEFER
RQR-DESN:                              DEFER
```

Keep the 24-fit execution flag `FALSE`. Do not run any member of the bounded grid in this patch.

## Mandatory patch 1: make wrapper evidence publication fail closed

File:

```text
application/scripts/08_run_rqr_dlm_bounded_validation.sh
```

### A. Propagate enumeration-pipeline failure

The current construct

```bash
while ...; do ...; done < <(find ... -print0 | sort -z)
```

does not propagate the process-substitution producer status. Replace it with a checked intermediate NUL-delimited path list or another construct whose pipeline status is observed in the parent shell. A failed `find` or `sort` must set `finalizer_error=TRUE`, append a structured failure row, and force nonzero wrapper status. A header-only or partial manifest must never be accepted as success.

### B. Reject canonical-path type collisions

Before running, require every canonical output target to be absent or a regular replaceable file according to the chosen contract. Do not allow `resource_summary.csv`, `wrapper_closeout.csv`, `artifact_hashes.csv`, or other canonical file paths to be directories or ambiguous symlinks. Use checked target-as-file publication (`mv -T` on the Jerez GNU host is one option), and check all redirections, maxima parsing, and moves while the finalizer is in nonfatal shell mode.

A deterministic negative test should precreate a canonical target as a directory and require nonzero status with structured evidence at a separately safe fallback location, or require the wrapper to reject the output directory before launching anything.

### C. Verify exact artifact file-set equality

File:

```text
application/scripts/10_test_rqr_dlm_monitor_wrapper.sh
```

The test currently rehashes only rows that exist. Add an exact comparison between:

```text
manifest paths
actual regular files under the scenario output, excluding artifact_hashes.csv
```

Require all required files to be represented and at least the expected row count. Add a controlled producer-pipeline failure test. Retain the existing five PGID scenarios.

Suggested additional fault cases:

```text
artifact enumeration command exits nonzero
canonical target is a directory
hash/stat command fails after at least one row
```

## Mandatory patch 2: roll back every failed RDS publication

File:

```text
application/scripts/lib/rqr_dlm_bounded_diagnostics.R
```

`rqr_bounded_publish_fit_rds()` must remove the final path if any exception occurs after rename. Use explicit `renamed` and `committed` flags in `on.exit`, compute object-only evidence before rename, and set `committed=TRUE` only immediately before successful return.

Add an injectable or mocked post-rename hash/metadata failure test proving:

```text
temporary absent
final absent
error recorded by the caller
```

Retain class/object identity, checkpoint digest, continuation validation, size/hash before and after rename, and no-overwrite behavior. No additional provenance reconstruction is required inside the publisher because the caller already gates exact target/runtime provenance and exact object identity preserves it.

## Optional hardening

Validate `.Random.seed` kind code and kind-specific expected vector length, not only numeric/integral/range and `length >= 2`. This is not a bounded-grid blocker.

## Required validation after the patch

At one exact implementation commit, with the execution flag still false:

```text
shell syntax and git diff checks
make test-dlm-monitor
make test-native
R CMD check --no-manual
main and supplement PDF builds
literature manifest
pinned exdqlm archive-runtime tests
exact isolated primary runtime
bounded preflight
all 43 reference gates with a fresh recursive bundle
unchanged 2,000-burn-in + 6,000-retained benchmark
fail-closed execute negative test with correct binding and phrase
```

Do not change seeds, starts, thinning, R-hat/ESS thresholds, resource ceilings, or the 6,000-retained schedule. Do not retry or retune after seeing results.

Commit and push the implementation plus compact reconciliation evidence. Prepare the next independent-review prompt. Do not create the authorization commit in the same pass.

## Authorization sequence after a clean review

Only after the patch evidence receives another independent pass:

1. create a separate reviewed commit changing the execution flag;
2. build a new exact isolated runtime at that authorization commit;
3. regenerate preflight and the exact reference bundle because the config digest changed;
4. obtain explicit user confirmation;
5. execute the 24 fits sequentially with cell-level fail-fast and no retries/retuning;
6. audit the bounded result before matched simulation.

Matched/production RQR-DLM, CAVI/ELBO, and RQR-DESN remain out of scope.
