# RQR-DLM rootwise2 ASIS2 exact-promotion integration plan

Date: 2026-07-28

## Purpose

This note freezes the next safe implementation path after the
rootwise2-ASIS2 transition comparison and the fresh local-level M01 wave-2
development gate.  It is a reproducibility and launch-boundary plan, not a
scientific result.

## Current source state audited

The DLM validation lane is
`codex/rqr-dlm-transition-forensics-20260727` at
`a7ca31a04901d00973b0c5dd56d5cc5f2ba454ef`.  That branch is clean and
pushed.  It contains:

- forensic evidence for the one-root, rootwise one-ASIS, and rootwise
  two-ASIS failures;
- a targeted transition comparison over S03 replications 13, 55, and 94;
- the selected rootwise2-ASIS2 transition, with two symmetric rootwise
  partially collapsed component-scale cycles followed by two
  centered--noncentered interweaving cycles; and
- a fresh M01 local-level wave-2 development gate with 49/49 chains,
  25/25 tasks, and 1,150/1,150 diagnostics passing.

The development gate intentionally used source loading for development
iteration.  It does not replace exact promotion, which still requires an
isolated runtime built from the exact committed source.

Current `origin/main` is
`b522a80c79c68cd771c26c58c0e23de4e64f7192`, which includes separate
mean-tilt, article, figure, and package updates.  Because the exact primary
runtime builder requires a clean checkout on branch `main`, promotion cannot
be launched directly from the DLM branch without first reconciling this
mainline divergence.

## Diagnosis

The old hard failure pattern was not a response-likelihood or DGP-design
problem.  It was a Markov-transition problem concentrated in the shared
component-evolution scale for the local-level DGP, especially one-chain
`log_q_1` diagnostics in S03 replications 13 and 94.  The symmetric rootwise
correction improved the failure count, but adding only more ASIS cycles did
not reliably remove the hard floor.  The targeted comparison showed that the
selected rootwise2-ASIS2 composition cleared the known hard cases and the
guard case without increasing the retained schedule or weakening diagnostics.

The remaining risk is source-integration drift: current main contains valid
non-DLM work that must be preserved, while the DLM lane contains launch
critical sampler, schema, config, and validation-script changes.  A blind
"ours" or "theirs" merge would be unsafe.

## Integration rule

Reconcile the DLM lane with current `origin/main` before exact promotion.
The integrated source must preserve all of the following:

1. the rootwise2-ASIS2 DLM transition and fit schema
   `rqrgibbs_fit/1.15.0`;
2. the mean-tilt fixed-design additions currently on `origin/main`;
3. the fail-closed confirmatory configuration, with
   `confirmatory_execution_authorized = FALSE`;
4. the selected correction schema
   `rqrgibbs_dlm_main_correction/1.10.0`;
5. all compact DLM forensic and development evidence already committed;
6. all mainline manuscript, figure, arXiv, and mean-tilt documentation unless
   a concrete conflict requires a minimal reconciliation edit; and
7. no mutation of the protected exdqlm or Q-DESN repositories.

The package development version should advance beyond both sides of the
divergence after integration.  The exact promotion runtime will then bind the
integrated application subtree, not either parent in isolation.

## Exact-promotion gate sequence

After the integrated fail-closed source is clean and pushed to `main`:

1. Build a fresh isolated primary `rqrgibbs` runtime with
   `application/scripts/04_prepare_primary_runtime.R`, using the full
   integrated `main` commit as `RQR_EXPECTED_PRIMARY_COMMIT`.
2. Run M01 wave 1 through `22_validate_rqr_dlm_wave1_corrections.R`.
3. Run M01 wave 2 through `22_validate_rqr_dlm_wave1_corrections.R`.
4. Run M02 wave 1 through `23_validate_rqr_dlm_wave1_comparator_projection.R`
   with the isolated CRAN `exdqlm` 1.1.0 runtime attestation.
5. Run M02 wave 2 through the same M02 gate.
6. Run the horizon/fixed-design gate through
   `24_validate_rqr_dlm_horizon_and_fixed_design.R`.
7. Run the resource envelope gate through
   `25_validate_rqr_dlm_resource_envelope.R`.
8. Run package/native/TeX/literature checks in proportion to the merged
   source changes.
9. Publish a compact exact-promotion closeout under `docs/audits/`, including
   source commit, runtime attestation hash, gate output roots, validation
   summaries, and artifact hashes.

Only if every gate passes should a separate flag-only authorization commit set
the launch flag to `TRUE`.  The main simulation must then start under a fresh
run id and must not reuse failed or development outputs.

## Stop conditions

Stop fail-closed if any of the following occurs:

- merge conflicts cannot be resolved while preserving both DLM and mean-tilt
  contracts;
- the launch flag becomes `TRUE` before exact promotion finishes;
- an exact gate runs from a non-isolated primary runtime;
- an exact gate reports any failed fit, failed diagnostic, nonzero numerical
  repair, reproducibility ineligibility, or resource-envelope failure;
- exdqlm or Q-DESN source checkouts are modified; or
- previously failed/development outputs are treated as promotion outputs.

## Expected deliverable before launch

The expected deliverable of this pass is an integrated, fail-closed source on
`main`, a fresh exact isolated runtime, exact-promotion gate outputs, and a
tracked closeout.  It is not the main confirmatory simulation itself unless a
later flag-only authorization commit is created after the gates pass.
