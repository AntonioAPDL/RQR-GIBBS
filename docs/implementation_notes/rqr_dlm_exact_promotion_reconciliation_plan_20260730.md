# RQR-DLM exact-promotion reconciliation plan

Date: 2026-07-30

## Purpose

This plan closes the software and provenance boundary between the successful
rootwise2-ASIS2 promotion gates and a fresh confirmatory RQR-DLM simulation.
It does not alter the generalized-Bayes target, priors, seeds, schedules,
diagnostic thresholds, incidence matrix, comparator definitions, or response
interpretation.

The main simulation remains fail-closed until a separate flag-only
authorization commit is created. No failed or development output may be reused
as confirmatory output.

## Evidence already established

Commit `38c008030d888593701c3769dbad3cdf53bd84b5` completed all six heavy
promotion gates under exact isolated runtimes:

| Gate | Tasks | Diagnostic rows | Result |
|---|---:|---:|---:|
| M01 static Gaussian | 20 | 920 | 920/920 pass |
| M01 local-level Gaussian | 25 | 1,150 | 1,150/1,150 pass |
| M02 static Gaussian | 20 | 900 | 900/900 pass |
| M02 local-level Gaussian | 25 | 1,125 | 1,125/1,125 pass |
| fixed-design RQR | 8 | 328 | 328/328 pass |
| horizon and dynamic-endpoint contracts | 17 checks | -- | all pass |

The resource envelope passed. All heavy-gate artifact manifests were
independently rehashed successfully. The exact-runtime M01 local-level
diagnostic file is byte-identical to the retained development diagnostic file,
which confirms deterministic reproduction under the frozen seeds and
transition.

## Failure diagnosis

The heavy gates completed before the post-computation package checks. The next
target, `make test-standalone-contracts`, inherited
`RQR_PRIMARY_RUNTIME_ATTESTATION` and installed the package into the first
library on `R_LIBS`. That library was the already-attested primary runtime.

The installation changed the runtime tree digest from

```text
6b66e2e41617a496e141c4d46230bb4c462a69a13469393eca2a350c02081c6e
```

to

```text
c2a42702ac7efcce8f2f43f121105477061dad17da4a855c40a4a74ae8995573
```

and left ignored compiler objects in the source checkout. The subsequent
fail-closed binding check correctly rejected the mutated runtime. One test
expected the later missing-runtime message and therefore reported a message
assertion failure.

This is a post-gate test-isolation defect. It is not a sampler, target,
diagnostic, comparator, horizon, or resource failure. The timing and
authenticated manifests show that the heavy evidence preceded the mutation.

## Corrections

### Controlled failure-path test

The preliminary diagnostic-preflight test explicitly clears
`RQR_EXPECTED_PRIMARY_COMMIT` and `RQR_PRIMARY_RUNTIME_ATTESTATION` in its child
process. The test therefore exercises the intended missing-runtime branch
independently of its parent environment.

### Protected package installation

The Makefile:

1. refuses package installation when an attested runtime is active and no
   disjoint `RQR_PACKAGE_LIBRARY` is supplied;
2. accepts an explicit test library; and
3. uses both `--preclean` and `--clean`.

### Hermetic promotion checks

`application/scripts/28_run_rqr_dlm_promotion_checks.sh`:

1. requires clean `main` at a full expected SHA;
2. verifies the primary attestation and runtime digest before testing;
3. installs the attested source package into a fresh ignored check library;
4. clears exact-runtime variables only for generic failure-path tests;
5. runs the complete source, native, package, exdqlm, PDF, supplement, and
   literature matrix;
6. verifies that the attested runtime digest is unchanged afterward;
7. rejects compiler artifacts or source-worktree changes; and
8. writes logs, a gate ledger, runtime binding evidence, and recursive hashes.

The script never compiles, installs, or loads directly from the protected
exdqlm checkout. The existing pinned-runtime preparer continues to materialize
the pinned commit with `git archive` under ignored cache.

## Source-drift reconciliation

The heavy evidence may be transferred to a later closeout commit only if all
of the following are true:

1. `application/R`, `application/src`, `DESCRIPTION`, and `NAMESPACE` have the
   same Git object identities as the validated implementation.
2. The RQR-DLM configuration, incidence, seed ledger, schedules, promotion
   scripts, and confirmatory runner are unchanged.
3. Any intervening files are classified explicitly and do not affect the
   fitted target or executable runtime.
4. A fresh isolated runtime is built from the final clean `main`.
5. Its installed runtime digest equals the validated runtime digest, or any
   difference is explained by a non-executable packaging change and receives
   a new bounded validation. An unexplained executable difference requires
   rerunning the affected heavy gates.
6. The hermetic promotion-check matrix passes from the final commit.

## Closeout and launch sequence

1. Commit and push the fail-closed correction with both execution flags false.
2. Build and attest the exact runtime from that commit.
3. Materialize a compact source-drift and heavy-evidence reconciliation.
4. Run the hermetic post-computation check matrix.
5. Commit and push the authenticated promotion closeout.
6. Create a separate commit changing only
   `confirmatory_execution_authorized = FALSE` to `TRUE`.
7. Build a fresh runtime for the authorization commit.
8. Run fresh confirmatory preflight and oracle-reference bundles.
9. Prepare the authorization bundle using the explicit user-confirmation
   token.
10. Launch a fresh append-only 110-wave run under a new run ID.

The coordinator remains cell-level fail-closed. Earlier failed, development,
and promotion outputs are not confirmatory outputs and do not reduce the
planned 8,400 replication tasks.

## Decision rule

Authorization is permitted only when the source-drift reconciliation,
runtime-equivalence check, complete hermetic check matrix, and compact
closeout all pass. Otherwise both execution flags remain false and the
simulation is not launched.
