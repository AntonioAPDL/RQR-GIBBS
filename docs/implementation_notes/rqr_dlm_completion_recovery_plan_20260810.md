# RQR-DLM diagnostic-aware completion recovery plan

Date: 2026-08-10

## Decision

The stopped diagnostic-aware maximum run must not be resumed or combined with
any earlier partial run. A fresh run is justified only after correcting three
execution-contract defects and rebuilding the isolated runtime from the exact
clean correction commit. The generalized-Bayes target, priors, data-generating
mechanisms, seed ledger, MCMC schedules, retained-draw counts, and frozen
diagnostic thresholds remain unchanged.

The correction is deliberately fail closed. The base confirmatory launch flags
remain false. The diagnostic-aware policy continues to retain primary metrics
when a frozen MCMC diagnostic fails, while provenance, target, numerical,
endpoint, artifact, process, and resource failures remain hard stops.

## Authenticated failed-run boundary

The immutable input is the run rooted at

```text
/data/muscat_data/jaguir26/.rqr_gibbs_frozen_runs/
rqr_dlm_diagnostic_aware_maximum_20260808_031595b_r2/run
```

It is bound to source commit
`031595bbbca5c59673faed10087faaf450c15a5a` and runtime-tree digest
`89a650024735aac6e6536a502d500e6ff14ca2f3189547d222649d9d6e160ac6`.
The coordinator stopped after canonical wave 4. The completed evidence contains
89 of 8,400 planned DGP--replication tasks, 547 of 43,800 planned method rows,
and 17,262 frozen diagnostic rows, of which 17,144 passed. All 914 published
wave-manifest entries independently rehash. These partial results are failure
evidence only and are not confirmatory results.

## Root-cause disposition

### M11: invalid scope for directional scale interweaving

All 23 M11 failures in the local-level skewed wave have the same pre-sampling
error: the diagnostic-aware policy requests a directional component-scale
update that requires at least two evolution components, whereas the local-level
model has one. This is not an MCMC mixing failure.

The policy must therefore be resolved against the constructed model:

- for two or more components, retain the selected one-sweep directional update;
- for one component, use the already validated coordinate interweave plus the
  joint-state elliptical update;
- record the requested and effective transition, component count, minimum
  dimension, and fallback in each fit's diagnostics.

This changes neither the target nor the schedule. In one dimension a random
direction supplies no multicomponent geometry beyond the coordinate update.

### M03: activity telemetry is not a target invariant

The exact four-replica transition currently combines structural validity with
finite-run activity. Its hard predicate requires every adjacent pair to accept
at least once, more than one label to visit the cold replica, and at least one
complete round trip. A valid finite exact run can fail those activity events.

The correction separates:

- hard invariants: configured temperatures, array dimensions, positive attempt
  counts, admissible accept counts, consistent finite acceptance fractions,
  valid label traces and round-trip counts, exact cold target, and zero repairs;
- activity sidecars: positive acceptance in every adjacent pair, multiple cold
  labels, and at least one round trip.

Structural, target, or numerical failures remain hard stops. Under the explicit
diagnostic-aware completion policy, an activity shortfall is hashed, retained
with raw telemetry, and reported as a nonblocking diagnostic warning. Under the
ordinary fail-closed policy it remains a diagnostic failure.

### Resource ceiling: insufficient engineering margin

Two workers crossed the 1.5 GiB sampled process-group ceiling by approximately
2.44 MiB (0.159%) and 7.33 MiB (0.477%). Fits were not implicated, the host had
ample memory, and the events were wrapper-enforced rather than kernel OOMs.
The uniform per-worker ceiling is raised prospectively to 2.0 GiB. With eight
wave workers the nominal aggregate ceiling is 16 GiB, far below host capacity.
Worker count, compute threads, poll interval, and all statistical settings stay
fixed.

### Health and closeout accounting

Missing failure-class fields must be treated as empty rather than allowing
`NA` to propagate into the warning count. The read-only failed-run closeout
must recognize diagnostic-warning task statuses as completed artifacts, verify
every published wave manifest from local bytes, record input hashes, and state
explicitly that partial scientific results are not reusable.

## Implementation gates

1. Unit-test model-aware M11 resolution for one- and multicomponent models.
2. Unit-test M03 structural validation independently from activity outcomes,
   including zero-acceptance and zero-round-trip valid fixtures.
3. Persist per-chain operational sidecars in the compact MCMC diagnostic object
   without retaining heavy fits or latent paths.
4. Pin the 2.0 GiB resource value in the configuration validator and wrapper.
5. Test NA-safe health accounting and failed-closeout status accounting.
6. Run the focused test files, native/package checks proportionate to the
   affected code, and the repository smoke gate.
7. Commit and push the correction while the base launch remains fail closed.
8. Build an isolated primary runtime from that exact clean commit; rerun
   preflight and non-MCMC oracle/reference gates; bind a fresh authorization
   bundle to their recursive manifests.
9. Start a fresh maximum-design run under a new run ID and new output root.
   Never copy, resume, or aggregate the failed run's outputs.

## Interpretation of the eventual run

The new run remains a diagnostic-aware maximum study. A completed row with a
diagnostic warning contributes its predeclared primary simulation metrics and
is separately identified in warning-stratified sensitivity summaries. This
does not declare the chain convergence-validated. The RQR update remains a
loss-based generalized-Bayes update for interval-root functions; it is not an
ordinary response likelihood and does not define posterior-predictive response
draws.
