# Output-17 reconciliation and launch disposition

Date: 2026-07-25

## Scope and authenticated review input

This reconciliation audits the complete Output-17 response supplied as:

```text
/home/jaguir26/.codex/attachments/
48fee513-429a-4e07-9000-9c408d6bcc1f/pasted-text.txt
```

The input contains 449 lines and 19,877 bytes. Its SHA-256 is:

```text
d7ed83e1a9716a8261de2434c733e77e92f012a16c40f46473601b123acf2616
```

Output-17 reviewed reconciliation commit
`48fec40ad04594f92f6b2640b2fd29eab0b24b0b` and identified
`aa1ded8c5b4db2257c6985c73626e1a0a252fc72` as the authoritative
scientific implementation. The latter is an ancestor of the current branch.
Before this correction, the later commits changed manuscript or workflow
documentation, not the confirmatory application implementation.

The review passed the statistical target, generalized-Bayes interpretation,
diagnostic-completeness contract, and frozen confirmatory design. Its sole
launch blocker, `WAV-001`, was a replay path in the wave-output contract.

## Finding disposition

`WAV-001` is accepted. Deleting both JSON state records while retaining a
completed wave directory could make the prior implementation treat the wave as
unseen. A caller could then invoke the direct wave launcher with another fresh
output path. The state transition was canonical, but the output namespace was
not part of the immutable run binding and was not inventoried independently.

The correction does not alter a DGP, estimand, method, tuning rule, seed,
replication budget, stopping rule, MCMC schedule, or statistical gate.

## Implemented correction

The wave-run contract is advanced from
`rqrgibbs_dlm_wave_run/1.0.0` to
`rqrgibbs_dlm_wave_run/1.1.0` and now includes the normalized canonical
wave-output base directory in its integrity digest.

For every catalog row, the only permitted output path is derived as:

```text
<bound-wave-output-base>/<four-digit-index>__<canonical-wave-id>
```

The direct launcher:

1. requires the bound output base through
   `RQR_CONFIRMATORY_WAVE_OUTPUT_BASE`;
2. includes that base in the immutable run contract;
3. rejects any supplied output path that is not the derived canonical path;
4. performs this rejection before publishing a start record, creating the
   output directory, or starting a worker; and
5. retains the pre-existing freshness requirement for the canonical path.

The coordinator supplies the canonical base. The health checker and
coordinator resume validator retain it as a required binding field.

The state validator now inventories the bound output base independently of the
JSON state directories. Every launched terminal record must point to its exact
derived output directory and verified artifact manifest. The set of observed
wave directories must equal the set of launched terminal records. Skipped
precision-stop waves remain terminal state records without output directories.
Unexpected files, symbolic links, orphaned directories, and missing launched
directories fail closed.

## Adversarial coverage

The contract test now demonstrates all of the following:

- a valid canonical launched-wave record and artifact manifest passes;
- an incomplete start, altered record, or detached manifest fails;
- deleting both start and completion records while leaving the original
  output directory fails as orphaned history;
- an alternate fresh output path is rejected and is not created; and
- the direct launcher calls the canonical-path guard before both start-record
  construction and output-directory creation.

This directly closes the Output-17 replay counterexample. No output or worker
can be created through the alternate-path attempt.

## Validation evidence

The corrected working tree passed:

```text
focused confirmatory contract test:  PASS
complete standalone DLM contracts:   PASS
complete native R/C++ tests:         PASS
R CMD check --no-manual:             PASS (Status: OK)
git diff --check:                    PASS
```

The package under test is `rqrgibbs 0.1.0.9018`.

No confirmatory fit was executed while the correction was being validated.
The committed configuration remained fail closed with
`confirmatory_execution_authorized = FALSE`.

## Launch decision

The sole Output-17 source blocker is resolved in the corrected implementation.
The next admissible steps are:

1. commit this implementation and reconciliation together;
2. create a separate authorization commit whose only source change flips
   `confirmatory_execution_authorized` from `FALSE` to `TRUE`;
3. build a fresh isolated primary runtime from that exact authorization
   commit;
4. rerun the authorization-bound preflight and oracle/reference suites;
5. materialize and verify the authorization bundle; and
6. start the complete confirmatory coordinator only if every gate passes.

The resulting run is the frozen main confirmatory simulation, including its
embedded sentinel waves. It is not a separate tuning pilot and does not
authorize later candidate-method tuning.
