# Oracle-tilt v3 evidence hardening

Date: 2026-08-04

## Scope

This note governs the single-data, 95% oracle-tilt illustrations only. The
workflow compares the ordinary RQR, equal-tailed, and shortest-window fixed
population tilts for the fixed-design ridge regression and fixed-W RQR-DLM.
It is not a repeated-sample simulation study. Its generalized posterior is a
loss update and does not define posterior-predictive response draws.

No statistical target, DGP, prior, MCMC transition, seed, initialization,
diagnostic threshold, or recovery gate is changed by this hardening pass.

## Failure diagnosis

The exact-commit run at `06401f99e8b6403c82da3cac2fdc32cf4188d190`
completed four fixed-design RQR chains with zero repairs, 93 of 93 maintained
diagnostics passing, and all recovery and heterogeneity gates passing. All
four fits nevertheless recorded a false primary-runtime source match. The
first failure-ledger append then failed because a scalar missing value was
assigned to a zero-row data frame.

The source checkout and isolated runtime remained unchanged. Independent
single-worker, two-worker, and concurrent lineage rechecks passed after the
run. The compact failed workers did not retain the individual lineage
subgates, so the original false aggregate cannot be decomposed
retrospectively.

Earlier execution attempts show that process isolation is necessary. A
monolithic attempt obtained a strict-passing fixed-design RQR cell but crossed
the sampled 12-GiB RSS threshold. Fresh cell processes reduced the observed
peak to approximately 7.4 GiB and left no residual R process. Process
isolation is therefore retained.

## Corrective contracts

### Failure ledger

`oti_rbind_fill()` preserves the row count of each input, including zero-row
data frames. `otv3_append_failure()` writes a versioned, monotonically
sequenced ledger atomically. The outer monitored wrapper includes that ledger
in its final recursive artifact manifest.

### Three-phase provenance

Each chain records one compact row at each phase:

1. `worker_entry`: full primary repository/runtime verification before MCMC;
2. `fit_recorded`: the primary state stored by the fitted object;
3. `worker_exit`: a new full verification after fitting and prediction.

The audit includes Git state, expected commit, runtime and attestation paths,
source/runtime tree digests, and every archive, source-package, build, install,
marker, receipt, isolation, and source-match gate. A chain is promotable only
when all three snapshots and the fit's original promotion fields pass. A false
snapshot is evidence to diagnose; it is never overwritten by a later true
snapshot.

### One authoritative cell plan

The R runner derives `cell_plan.csv` from the frozen 27-chain fit plan. The
shell orchestrator validates and executes those rows in their recorded order.
There is no independent hard-coded family/target loop. Status aggregation and
finalization use the same in-memory plan.

### Monitored acceptance cell

The acceptance launcher uses the same user-level systemd memory, task, working
directory, exact-runtime, library, process-group, and thread contracts as the
full execution. It executes prepare plus the first authoritative cell
(`fixed_design/RQR`) and stops with a versioned acceptance closeout. It does
not authorize or silently continue the remaining cells.

## Validation order

All paths below are ignored local output/cache roots.

1. Run the focused v3 tests, complete native tests, package check, and shell
   syntax checks.
2. Commit the correction and fast-forward clean `main`.
3. Build the isolated primary runtime from that complete `main` SHA.
4. Run fresh preflight and reference-only gates.
5. Run the representative fixed-design/DLM benchmark.
6. Run the production-shape resource rehearsal.
7. Launch the monitored four-chain acceptance cell and require:
   - all three provenance phases for every chain;
   - zero repairs and exact-target eligibility;
   - maintained R-hat, bulk/tail ESS, and MCSE gates;
   - recovery and heterogeneity gates;
   - no residual process, timeout, signal, or resource breach;
   - valid worker, cell, and wrapper manifests.
8. Only after acceptance, launch a fresh 27-chain run from an empty output
   root. Prior workers with false provenance are not resumed.
9. Package only a complete six-cell strict-passing bundle. Raw worker RDS
   files remain ignored and are never copied into manuscript evidence.

## Stop rules

The workflow stops immediately when a chain, provenance snapshot, cell gate,
process-lifecycle check, or sampled resource limit fails. A failure must leave
a structured ledger and final wrapper manifest. Statistical thresholds are
not weakened to rescue an illustration. DLM or fixed-design retuning requires
a separate diagnostic justification; infrastructure failures alone do not
justify changing the statistical target.
