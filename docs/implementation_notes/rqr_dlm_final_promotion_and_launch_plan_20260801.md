# RQR-DLM final promotion and launch plan

Date: 2026-08-01 UTC  
Scope: confirmatory RQR-DLM simulation only

## Objective

Close the exact-promotion boundary without repeating validated computation,
then authorize and start the frozen 8,400-task ADEMP study from a fresh run
root. The RQR update remains a generalized-Bayes loss update for interval
roots; no step below treats its draws as posterior-predictive response draws.

## Evidence already earned

The clean implementation at
`89774216c5dcc55b936e5a9a16eaa453c5d54c25` completed every statistical and
resource gate. Across the seven diagnostic files, 6,184 of 6,184 gate
evaluations passed. The targeted M03/M08 gate is intentionally nested in the
full affected-wave gate. All 41 compact heavy-evidence manifest entries were
rehash-verified. These results are promotion evidence, not comparative
simulation results.

The original supervisor stopped only because it invoked the tracked,
non-executable `28_run_rqr_dlm_promotion_checks.sh` directly. Exit 126 occurred
before the script body ran. The correct transport-independent invocation is
`bash application/scripts/28_run_rqr_dlm_promotion_checks.sh ...`.

## Source reconciliation rule

Heavy gates need not be repeated when every launch-critical implementation,
configuration, design, orchestration, and validation object retains the same
Git object identity. The tracked source-equivalence ledger compares the heavy
source with reconciled main `d103f1f6a495e91314e91bc9255f5128f52d8a1c`.
Any mismatch in that ledger would invalidate this shortcut and require an
affected-gate rerun.

## Ordered promotion workflow

1. Reconcile a dedicated, clean `main` worktree to `origin/main`.
2. Verify the DLM Git-object ledger and preserve both full source identities.
3. Build `rqrgibbs` from an exact Git archive into a disjoint ignored library.
4. Run hermetic package, native, protected-dependency, document, and literature
   checks through `bash`, writing only to a fresh ignored evidence root.
5. Rehash every heavy and hermetic manifest entry and record the results in a
   tracked closeout while both execution flags remain false.
6. Commit and push the closeout. That commit is the reviewed implementation
   parent for the authorization transition.
7. Create a separate commit whose only diff changes
   `confirmatory_execution_authorized` from `FALSE` to `TRUE`.
8. Build a fresh isolated runtime from the exact authorization commit.
9. Generate fresh authorization-commit preflight and oracle-reference bundles,
   then bind them to the comparator runtimes and the explicit confirmation
   token.
10. Launch the canonical maximum wave plan under a new run ID with `nohup` and
    `setsid`; never resume a failed or development run root.

## Stop conditions

Stop before authorization or launch for any of the following:

- DLM source-object mismatch;
- dirty source or compiler artifact in `application/src`;
- runtime/source attestation mismatch;
- failed package, native, TeX, literature, or protected-checkout guard;
- artifact hash or byte-count mismatch;
- authorization diff containing anything beyond the one flag;
- preflight or oracle-reference failure;
- missing canonical seed, task, or wave plan;
- unexpected use of the protected exdqlm checkout; or
- reuse of a prior failed/development output root.

## Background-run contract

The detached coordinator owns an append-only run contract, executes canonical
waves in order, stops after a failed wave, and can be inspected with
`21_healthcheck_rqr_dlm_confirmatory_simulation.R`. Heavy fits remain ignored;
only compact, hashed summaries and eventual article-facing results are eligible
for promotion after the run closes.
