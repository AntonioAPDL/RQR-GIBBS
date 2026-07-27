# RQR-DLM confirmatory relaunch recovery plan

Date: 2026-07-26

## Objective

Replace the terminal run
`rqr_dlm_main_20260726_bb96629` with a fresh, exact-commit confirmatory run
after closing the forecast-horizon, M03 schedule, state-observer, and shell
invocation boundaries. The statistical design, generalized-Bayes target,
priors, data-generating mechanisms, methods, estimands, seeds, replication
counts, diagnostic thresholds, and precision-stopping rules remain frozen.

This is the main confirmatory simulation. Its early sentinel waves are
embedded validation and stopping gates, not a separate tuning pilot.

## Audit conclusion

The optimal correction is narrow:

- materialize the observation design at every public horizon;
- distinguish endpoint length, finiteness, and ordering failures;
- use the fixed 3,000-draw M03 schedule for all one-chain standard tasks;
- preserve the reviewed 1,500-draw M03 four-chain sentinel schedule;
- compute iteration budgets by mutually exclusive standard and sentinel roles;
- normalize vector names at the append-only JSON state boundary;
- report failed runs as terminal in the health checker; and
- invoke shell workers explicitly through `bash`, independent of transported
  executable mode bits.

There is no evidence supporting a redesign of the Gibbs sampler, FFBS kernel,
component-specific evolution scales, comparator definitions, or simulation
estimands. Changing those objects now would mix implementation repair with
scientific redesign and invalidate the frozen confirmatory contract.

## Stage 1: fail-closed source

1. Integrate the latest manuscript-only `origin/main` changes without
   modifying the protected repositories.
2. Keep `confirmatory_execution_authorized = FALSE`.
3. Commit the correction source, tests, budget overlay, failed-run closeout,
   and this recovery plan.
4. Push the clean implementation commit.

Required static gates:

```text
R parse and git diff --check
focused confirmatory contract tests
complete native R/C++ tests
complete standalone DLM contract tests
R CMD check --no-manual
make smoke
make pdf
make supplement
make test-exdqlm-rqr
make literature-manifest
theory-figure oracle tests
confirmatory monitor fault test
fail-closed direct and wave launch tests
```

Generated PDFs, TeX auxiliaries, fitted objects, and raw chain evidence remain
untracked.

## Stage 2: exact implementation validation

Build and attest an isolated `rqrgibbs` runtime from the clean fail-closed
implementation SHA. With all numerical-library thread variables fixed to one,
run:

1. the complete 44-chain first-wave M01 correction gate;
2. the complete 44-interval-chain/88-endpoint M02 projection gate; and
3. `24_validate_rqr_dlm_horizon_and_fixed_design.R`, which checks:
   - all 16 exact model horizons and full-model partitions;
   - all eight first-wave one-chain M03 standard seed streams;
   - 328 M03 diagnostics under the fixed schedule; and
   - the S03/replication-13 M01 public fit-and-forecast endpoint path.

Every fit must use the exact isolated runtime, report zero numerical repairs,
retain the exact-joint-target status where applicable, and be reproducibility
eligible. Compact summaries and recursive artifact manifests are promoted;
full chain objects stay under ignored cache roots.

Any failure returns the source to Stage 1. No seed, threshold, or schedule may
be changed in response to an individual validation result.

## Stage 3: flag-only authorization

After Stage 2 passes:

1. create one commit whose only source difference from the reviewed
   implementation is
   `confirmatory_execution_authorized = FALSE` to `TRUE`;
2. verify the flag-only diff with the repository helper;
3. build a new isolated runtime from the full authorization SHA;
4. run fresh authorization-bound preflight and oracle/reference modes;
5. require every gate to pass and all source, runtime, dependency, toolchain,
   config, incidence, seed-ledger, task-plan, wave-plan, and budget hashes to
   agree; and
6. materialize a fresh authorization bundle with the user’s standing explicit
   execution confirmation.

The implementation-runtime evidence cannot substitute for the
authorization-runtime evidence because the configuration flag changes the
committed package subtree.

## Stage 4: fresh detached launch

Use a new run identifier derived from the authorization SHA and fresh ignored
roots:

```text
application/runs/<new-run-id>/
application/logs/<new-run-id>/
```

Start the coordinator with
`20_launch_rqr_dlm_confirmatory_simulation.sh`. Immediately verify:

- the detached PID is alive;
- the checkout is the clean authorization SHA;
- the run contract binds the exact runtime and canonical wave-output base;
- the first start record is append-only and canonical;
- the eight worker monitors are active within their RSS/thread ceilings; and
- the health checker reports the correct active wave.

The old run is never resumed and no old fit is copied into the new namespace.

## Stage 5: monitoring and final analysis

For each wave:

1. require a terminal append-only state record;
2. verify the recursive artifact manifest;
3. enforce same-batch sentinel and prior-batch decisions;
4. stop permanently on a failed wave;
5. collect only after the applicable canonical wave set is complete; and
6. report progress as terminal waves, passed waves, precision-stop skips,
   failed waves, active wave, completed task rows, and remaining canonical
   work.

After all required waves are terminal, run the final audit before producing
article tables or figures. The analysis must distinguish:

- loss-based generalized posterior summaries;
- interval coverage, width, and interval-score performance;
- latent-root recovery where the DGP defines it; and
- any future response-simulation object, which is not defined by the current
  RQR update.

No result enters the article until the final audit passes and the compact
analysis artifacts are bound to the successful run.
