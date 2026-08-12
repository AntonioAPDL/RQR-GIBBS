# TCSP/ECM Split-Exact Validation Plan

Date: 2026-08-12
Status: launch-prep plan; full launch not authorized
Supersedes next-step execution in: `tcsp_validation_forward_execution_plan_20260812.md`

## Diagnosis

Report5 changes the next validation step. The repository should not relaunch a
full TCSP pilot until the new deterministic ECM and split exact-spacing paths
pass their own smoke checks. The formal TCSP action is still empirical and
scan-calibrated; ECM and Gibbs are fixed-target summaries. Split exact-spacing
adds a second univariate tolerance-action family based on an independent pilot
and an exact main-sample Beta spacing law.

## Stage 1: Source and Contract Gates

Required before any pilot launch:

- `make test-ecm`;
- `make test-tcsp`;
- `make smoke`;
- `make test-manuscript-language`;
- `git diff --check`.

Passing these gates means only that source contracts are coherent. It is not
performance evidence.

## Stage 2: ECM Optimizer Smoke

Run:

```bash
make rqr-ecm-validation-smoke
```

Purpose:

- compare ECM objective values with direct small-problem optimization;
- record gaps to current VB and small MCMC posterior-mean references;
- check convergence codes, selected starts, backtracking, and runtime;
- write compact artifacts under `application/outputs/rqr_ecm_validation_v1`.

Go/no-go:

- direct objective gaps should be near numerical tolerance on the small
  intercept-only cells;
- no systematic precision repair or backtracking stalls;
- VB differences must be recorded as approximation differences, not failures.

## Stage 3: Split Exact-Spacing Smoke

Run:

```bash
make tcsp-split-exact-validation-smoke
```

Purpose:

- verify Beta spacing indexing in repeated-sample code;
- compare scan TCSP, split empirical-shortest, split ECM, split CF, and min-max
  comparators;
- record validity before width;
- verify pilot/main split reproducibility and failure accounting.

Go/no-go:

- split methods must preserve iid-univariate scope wording;
- infeasible scan cells remain in denominators;
- ECM pilot failures cannot remove the formal empirical/split actions from the
  method registry.

## Stage 4: Moderate Pilot Prep

After Stage 2 and Stage 3 smoke results are reviewed, the next prepared run is
moderate-only:

```bash
Rscript application/scripts/67_validate_rqr_ecm_fixed_target.R \
  --mode=moderate \
  --config=application/config/rqr_ecm_validation_v1.json \
  --output-dir=application/outputs/rqr_ecm_validation_v1/moderate_<stamp>

Rscript application/scripts/68_validate_tcsp_split_exact.R \
  --mode=moderate \
  --config=application/config/tcsp_split_exact_validation_v1.json \
  --output-dir=application/outputs/tcsp_split_exact_validation_v1/moderate_<stamp>
```

This stage prepares the design for a later launch. It does not authorize a
full pilot or confirmatory simulation.

## Stage 5: Full Pilot Decision

A full pilot should be considered only after:

- smoke and moderate artifacts pass health checks;
- the method registry explicitly marks which competitors are enabled;
- run-time and memory envelopes are known;
- manuscript claims remain limited to implemented protocol descriptions;
- the user explicitly authorizes the compute budget and launch.

The future full pilot should retain common random numbers, report validity
before width, and include pilot-fraction sensitivity. It should not promote
ECM endpoints as formal tolerance intervals.

## Deferred

- exact scan recursion;
- regression-family finite-candidate tolerance theorem;
- conditional tolerance claims;
- posterior-action transfer;
- heavy confirmatory campaign;
- title/model/loss renaming.
