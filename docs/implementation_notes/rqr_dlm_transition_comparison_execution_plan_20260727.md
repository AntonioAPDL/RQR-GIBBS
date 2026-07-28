# RQR-DLM transition-comparison execution plan

Date: 2026-07-27

## Purpose

This note records the next bounded development action after the failed
two-ASIS RQR-DLM candidate.  It is a computational transition-diagnosis plan,
not a scientific simulation result and not an authorization to launch the main
simulation.

The main simulation remains fail-closed:

```text
confirmatory_execution_authorized = FALSE
```

No result from the failed main run, the exact-promotion failure, or the
development gates may be reused as promotion output.

## Current state

The current branch is:

```text
codex/rqr-dlm-transition-forensics-20260727
```

The current forensic commit is:

```text
74fe5b55fe399794abac343cf9cd5f5af592c336
```

The tracked forensic audit is:

```text
docs/audits/rqr_dlm_transition_forensics_20260727/
```

That audit digests three completed M01 wave-2 evidence roots:

1. one-root exact promotion at `e9c8068`;
2. symmetric rootwise partial collapse with one ASIS cycle;
3. symmetric rootwise partial collapse with two ASIS cycles.

The audit is read-only and hash-manifested.  It does not relaunch fits,
change thresholds, or create promotion evidence.

## Diagnosis from the completed evidence

The failure is a local-level M01 component-scale mixing boundary.  It is not
a generalized-Bayes target failure, not a response-likelihood issue, not an
exdqlm defect, not a numerical-repair failure, and not a resource failure.

The three completed attempts give:

| Attempt | Diagnostics | Failed tasks | Minimum `log_q_1` bulk ESS | Median `log_q_1` bulk ESS | Maximum `log_q_1` MCSE/SD |
|---|---:|---:|---:|---:|---:|
| one-root exact `e9c8068` | 1144 / 1150 | 5 | 94.42 | 342.78 | 0.1032 |
| symmetric rootwise + one ASIS | 1147 / 1150 | 2 | 108.33 | 398.17 | 0.0964 |
| symmetric rootwise + two ASIS | 1147 / 1150 | 2 | 117.73 | 437.29 | 0.0923 |

The persistent ordinary one-chain hard cases are S03 replications 13 and 94.
S03 replication 55 is retained as a guard case because it failed under the
one-root exact transition but passed under both symmetric-rootwise attempts.

The two-ASIS attempt improved the overall distribution but did not dominate
the hard cases.  In particular, S03 replication 13 had better `log_q_1`
movement under one ASIS cycle than under two ASIS cycles.  Therefore the next
step must compare transition composition and order, not blindly add more ASIS
cycles.

## Exact next action

Run the development-only targeted comparison implemented in:

```text
application/scripts/27_compare_rqr_dlm_transition_kernels.R
```

The comparison fits only:

```text
S03 replications 13, 94, and 55
```

against five fixed, target-preserving candidate schedules:

1. current symmetric rootwise partial collapse plus two ASIS cycles;
2. two complete symmetric rootwise partial-collapse compositions plus one
   ASIS cycle;
3. two complete symmetric rootwise partial-collapse compositions plus two
   ASIS cycles;
4. one ASIS cycle before two complete symmetric rootwise partial-collapse
   compositions;
5. the current transition under a prospective uniform 9000-retained-draw
   schedule, used only as a diagnostic comparator.

This is:

```text
5 candidates x 3 replications = 15 development fits
```

The run is allowed to finish with failed diagnostics.  A diagnostic failure is
information for transition selection, not a process failure.  The process
should fail only if a fit fails, a target status is invalid, or numerical
repairs occur.

## Launch command

The intended launch command is:

```bash
mkdir -p application/logs application/cache

out_root=application/cache/rqr_dlm_transition_comparison_20260727_<UTCSTAMP>
log_file=application/logs/rqr_dlm_transition_comparison_20260727_<UTCSTAMP>.log

env \
  OMP_NUM_THREADS=1 \
  OPENBLAS_NUM_THREADS=1 \
  MKL_NUM_THREADS=1 \
  VECLIB_MAXIMUM_THREADS=1 \
  NUMEXPR_NUM_THREADS=1 \
  RQR_TRANSITION_COMPARISON_WORKERS=6 \
  setsid Rscript application/scripts/27_compare_rqr_dlm_transition_kernels.R \
    --output-root="${out_root}" \
    > "${log_file}" 2>&1 &
```

The output root must be new.  The output is intentionally under
`application/cache/` and remains local-only.  The log remains under
`application/logs/` and is local-only.

## Expected local outputs

The comparison should write:

```text
transition_candidates.csv
targeted_jobs.csv
targeted_diagnostics.csv
targeted_summary.csv
transition_comparison_scalar_evidence.rds
transition_comparison_manifest.json
artifact_hashes.csv
```

The manifest must record:

```text
main_simulation_launch = FALSE
target_change = FALSE
threshold_change = FALSE
adaptive_extension = FALSE
confirmatory_execution_authorized = FALSE
```

## Selection rule

A correction can be selected only if it clears S03 replications 13 and 94 and
does not create a failure in guard replication 55.  Preference goes to the
lowest-cost exact transition that improves `log_q_1`, observed loss, and
effective draws per second without creating numerical repairs or changing the
target.

If no exact transition variant clears the hard cases, a uniform schedule
increase may be considered prospectively for the whole affected component-scale
role.  It must not be applied selectively to failed replications.

## Required follow-up after the comparison

After the comparison finishes, the next tracked artifact should be a compact
closeout under `docs/audits/` summarizing:

- candidate-by-replication pass/fail status;
- `log_q_1` ESS, tail ESS, MCSE/SD, and effective draws per second;
- observed-loss diagnostics;
- fit elapsed time and peak RSS sidecars;
- exact-target and numerical-repair status;
- artifact hashes for the local output root.

Only after selecting a correction should the complete affected M01 wave-2 gate
be rerun from a fresh ignored root.  That gate must pass all 1150 diagnostics
before a new fail-closed implementation commit, exact isolated-runtime
promotion, flag-only authorization commit, and fresh main simulation run can be
considered.

