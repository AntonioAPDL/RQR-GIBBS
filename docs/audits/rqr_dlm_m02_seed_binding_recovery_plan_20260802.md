# RQR-DLM M02 seed-binding recovery and relaunch plan

Date: 2026-08-02 UTC

## Decision

The confirmatory run `rqr_dlm_main_20260801_281802e` is terminally failed and
must not be resumed. Its first canonical sentinel wave stopped before atomic
task publication, so zero of 8,400 replication tasks are admissible. The next
launch must use a fresh run ID, exact runtime, preflight bundle, authorization
bundle, and output root.

This is a production provenance defect, not evidence against the RQR-DLM
target, transition kernel, numerical implementation, or frozen diagnostic
thresholds. No target, prior, seed, response law, schedule, estimand, or
threshold is changed by this recovery.

## Authenticated failure

All eight workers stopped with the same message:

```text
Error: A compact diagnostic object lacks a bound method seed.
```

The source run, worker logs, resource summaries, and wave manifest are
authenticated in
`docs/audits/rqr_dlm_failed_confirmatory_281802e_closeout_20260801/`.
All 60 entries from the completed wave manifest rehashed successfully.

M02 fits the lower and upper dynamic quantile ordinates separately. The seed
ledger and the M02 executor therefore correctly use endpoint-specific keys,
for example:

```text
method|C01M02|39|lower|1
method|C01M02|39|upper|1
forecast|C01M02|39|lower|1
forecast|C01M02|39|upper|1
```

The post-fit compact-diagnostic block instead requested the nonexistent key
`method|C01M02|39|interval|1`. Every worker encountered M02 immediately after
M01 in its first assigned task, producing the deterministic common stop.

## Corrective architecture

One source of truth now defines method endpoint IDs and method/forecast RNG
keys. Joint-interval methods use `interval`; M02 uses `lower` and `upper`.
Ledger construction, method execution, forecast execution, preflight, wave
launch, and compact diagnostic serialization consume that same contract.

Compact diagnostics use schema
`rqrgibbs_dlm_compact_mcmc_diagnostics/1.2.0`. They retain a structured RNG
binding table with stream kind, endpoint, chain, task key, and state digest.
An M02 four-chain sentinel must bind 16 rows: two stream kinds times two
endpoints times four chains. A one-chain joint-interval fit binds two rows.

The exhaustive preflight enumerates every planned stochastic method cell,
replication, endpoint, chain role, and method/forecast stream. It rejects any
missing binding before a fit starts. The wave launcher repeats this validation
against the authorization-bound ledger.

## Validation ladder

1. Parse and unit-test the endpoint/key constructors and compact serializer.
2. Validate the complete canonical binding grid against a newly generated
   full-state seed ledger, including a negative missing-M02-endpoint test.
3. Run the complete native test suite, standalone contracts, monitor tests,
   package check, TeX builds, and literature manifest.
4. Prove that inference-bearing package objects, MCMC transitions, schedules,
   diagnostic thresholds, incidence design, and master seed did not change.
5. Commit and push a fail-closed implementation state.
6. Build a fresh isolated primary runtime from that exact commit and regenerate
   the preflight and oracle-reference bundles.
7. Create and push a separate flag-only authorization commit only after the
   fail-closed implementation and local validation evidence are complete.
   Build the launch runtime from that exact authorization commit and bind fresh
   preflight, oracle-reference, and authorization bundles to it.
8. Launch all 110 waves from zero under a new append-only run ID. The first
   canonical sentinel wave is also the production-path requalification: the
   coordinator must stop before wave 2 unless all 20 tasks, eight workers,
   diagnostics, provenance checks, hashes, and resource gates pass.
9. Publish the wave-1 qualification evidence and continue only through the
   coordinator's existing fail-closed progression. Never reuse failed-run
   outputs as confirmatory results.

Running a second development copy of the full 20-task wave immediately before
the main launch would repeat the same deterministic workload, yet would not
exercise the final authorization runtime or append-only run root. The in-run
wave-1 gate is therefore the stronger and more efficient qualification: it
uses the exact launch artifacts and already prevents any later wave from
starting after a failure.

If any inference-bearing source changes during correction, the complete heavy
promotion suite must be rerun. If exact Git-object comparison confirms that
only provenance binding, fail-closed orchestration, tests, and documentation
changed, the previously passed heavy sampler evidence may be transferred,
subject to the fresh production wave-1 qualification.

## Promotion gates

- both execution flags are false during correction and qualification;
- no retry, reseeding, selective extension, or threshold relaxation;
- no change to the generalized-Bayes loss interpretation;
- no response-likelihood or posterior-predictive-response claim;
- zero promotion-blocking numerical repairs;
- exact isolated runtime and dependency attestations;
- complete endpoint-aware RNG binding before execution;
- all wave-1 workers and diagnostics pass;
- compact outputs and recursive artifact hashes verify;
- separate flag-only authorization commit;
- fresh run ID and output root.

The old run remains failure evidence only. It contributes no scientific
performance estimate and no denominator reduction to the new study.
