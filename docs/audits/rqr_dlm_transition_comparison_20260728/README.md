# RQR-DLM transition comparison closeout

Date: 2026-07-28

## Executive decision

The targeted development comparison completed successfully and supports a new
M01 component-scale transition candidate for the next full affected-wave gate.
The result does **not** authorize the main simulation and is not a scientific
performance result.  It is development evidence for selecting an exact
target-preserving MCMC transition.

The preferred next candidate is:

```text
rootwise2_ASIS2
```

This candidate repeats the full symmetric rootwise partial-collapse
composition twice and then applies two centered--noncentered ASIS cycles.  It
keeps the same generalized-Bayes target, priors, DGPs, seeds, estimands,
diagnostic thresholds, response interpretation, and launch authorization state.

The main simulation remains fail-closed:

```text
confirmatory_execution_authorized = FALSE
```

## Source and run boundary

The targeted comparison was run from:

```text
source commit: 0f064a2a5f03c061c060fc95b710b89b017a5692
package:       rqrgibbs 0.1.0.9026
fit schema:    rqrgibbs_fit/1.14.0
```

The local ignored output root was:

```text
application/cache/rqr_dlm_transition_comparison_20260727_20260728T055715Z
```

The run completed at:

```text
2026-07-28 07:44:20 UTC
```

It used 6 workers and one numerical thread per worker.

## Validation status

| Check | Result |
|---|---:|
| Development jobs | 15 / 15 completed |
| Fits succeeded | true |
| Exact joint target | true |
| Numerical repairs | 0 |
| Main simulation launched | false |
| Adaptive extension | false |
| Target changed | false |
| Thresholds changed | false |
| Diagnostics passed | 687 / 690 |

All three failed diagnostics belong to the replicated failed baseline
candidate `current_rootwise1_ASIS2`; none belongs to a new transition
candidate.

## Candidate comparison

| Candidate | Jobs passed | Min `log_q_1` bulk ESS | Min observed-loss bulk ESS | Max `log_q_1` MCSE/SD | Sum elapsed sec | Peak RSS KiB |
|---|---:|---:|---:|---:|---:|---:|
| `current_rootwise1_ASIS2` | 1 / 3 | 117.73 | 188.78 | 0.0923 | 5130.36 | 669856 |
| `rootwise2_ASIS1` | 3 / 3 | 226.15 | 329.72 | 0.0676 | 6563.59 | 671456 |
| `rootwise2_ASIS2` | 3 / 3 | 290.33 | 422.24 | 0.0587 | 7065.65 | 706904 |
| `ASIS1_then_rootwise2` | 3 / 3 | 225.28 | 280.53 | 0.0672 | 6826.13 | 684140 |
| `current_rootwise1_ASIS2_uniform9000` | 3 / 3 | 203.22 | 307.58 | 0.0702 | 6781.22 | 804964 |

The key pattern is that repeating the full rootwise partial-collapse
composition resolves the hard cases more effectively than simply increasing
the retained schedule.  The uniform 9000-draw comparator passes but is less
efficient and has a larger memory sidecar than `rootwise2_ASIS2`.

## Diagnosis

The earlier failures were caused by slow component-scale mixing in ordinary
one-chain M01 local-level S03 cases, especially replications 13 and 94.  This
was not a target, numerical, resource, or protected-dependency failure.

The current comparison confirms that the bottleneck is transition composition:

- the failed baseline repeats ASIS but performs only one full symmetric
  rootwise partial-collapse composition;
- `rootwise2_ASIS1` passes after repeating the full rootwise composition;
- `rootwise2_ASIS2` gives the strongest diagnostic margin among the exact
  target-preserving candidates;
- the 9000-draw schedule comparator passes but is less attractive than
  strengthening the transition itself.

This supports fixing the MCMC transition rather than weakening diagnostics,
selectively extending failed replications, or relaunching the main simulation
with known failed evidence.

## Why `rootwise2_ASIS2` is preferred

`rootwise2_ASIS1` is the lowest-cost passing transition and remains a viable
fallback.  However, `rootwise2_ASIS2` is preferred for the next complete
affected-wave gate because it gives materially larger margins in the two
persistent hard cases:

- higher minimum `log_q_1` bulk ESS;
- higher minimum observed-loss bulk ESS;
- lower maximum `log_q_1` MCSE/SD;
- no numerical repairs;
- only a modest elapsed-time increase relative to the other exact candidates;
- lower peak RSS than the 9000-draw schedule comparator.

Because the full M01 wave-2 gate has 25 tasks and 1150 diagnostics, the
stronger margin is worth the modest additional cost.  This is still an exact
transition for the same augmented generalized posterior; it does not change
the scientific target.

## Next implementation plan

1. Promote `rootwise2_ASIS2` from development control to the prospective M01
   component-scale transition for the affected validation path:

   ```text
   component_scale_collapsed_cycles = 2
   component_scale_interweave_cycles = 2
   component_scale_transition_order = "rootwise_then_interweave"
   ```

2. Keep the execution flag false and update the correction schema/version so
   old checkpoints are not silently continuation-compatible.

3. Add or update tests asserting that the chosen transition kernel is recorded
   in the fit object, continuation contract, and confirmatory configuration.

4. Run focused package/native tests for the sampler and confirmatory contract.

5. Rerun the complete M01 local-level wave-2 development gate from a fresh
   ignored root.  Required result:

   ```text
   49 chains
   25 tasks
   1150 / 1150 diagnostics passing
   all fits succeeded
   zero numerical repairs
   exact target status
   resource margin pass
   ```

6. If that complete affected-wave gate passes, commit a new fail-closed
   implementation and build a fresh isolated runtime from that exact commit.

7. Rerun exact promotion gates:

   - M01 wave 1;
   - M01 wave 2;
   - M02 wave 1;
   - M02 wave 2;
   - horizon/fixed-design;
   - resource envelope;
   - package/native/TeX/literature checks.

8. Only after all exact promotion gates pass, create a separate flag-only
   authorization commit and launch a fresh main simulation under a new run ID.

## Actions explicitly not authorized

- Do not merge failed two-ASIS evidence as validated implementation.
- Do not weaken ESS, MCSE/SD, tail-ESS, or R-hat thresholds.
- Do not selectively extend S03 replications 13 or 94.
- Do not reuse development outputs as promotion outputs.
- Do not relaunch the main simulation from this branch.
- Do not modify the protected exdqlm or Q-DESN repositories.

## Tracked compact artifacts

- `transition_candidates.csv`
- `targeted_jobs.csv`
- `targeted_summary.csv`
- `targeted_diagnostics.csv`
- `candidate_summary.csv`
- `failed_diagnostics.csv`
- `transition_comparison_manifest.json`
- `source_artifact_hashes.csv`
- `artifact_hashes.csv`

The heavy scalar evidence RDS remains local-only under `application/cache/`.

