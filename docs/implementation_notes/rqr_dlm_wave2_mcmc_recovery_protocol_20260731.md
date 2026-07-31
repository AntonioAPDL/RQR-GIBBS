# RQR-DLM wave-2 MCMC recovery protocol

Date: 2026-07-31  
Scope: confirmatory RQR-DLM simulation only  
Authorization state: fail-closed

## Decision and scientific scope

The first confirmatory launch is terminally failed. Its first canonical wave
passed, but the second wave exposed two distinct MCMC deficiencies: a marginal
one-chain M08 bulk-ESS failure in S03 replication 13 and severe four-chain M03
disagreement in S03 replication 117. The failed run is immutable diagnostic
evidence; none of its partial results can enter the confirmatory analysis.

The recovery changes transition effort, not the inferential target. The RQR
object remains a generalized-Bayes loss update for interval roots. It is not
an ordinary response likelihood and does not provide posterior-predictive
response draws. The response laws, data streams, priors, estimands, and frozen
diagnostic thresholds are unchanged.

## Why this recovery path is preferable

The M08 failure is just below the one-chain bulk-ESS threshold and passes its
tail-ESS and MCSE/SD gates. A uniform retained-draw increase is therefore a
proportionate target-preserving correction.

The M03 failure is qualitatively different. Forty of 41 label-invariant
four-chain diagnostics fail, with high rank-normalized R-hat and extremely low
bulk and tail ESS. More label swaps cannot solve this because the monitored
functionals already sort the roots pointwise. Longer retention alone also
cannot distinguish slow burn-in from poor local movement. The recovery matrix
therefore varies burn-in, retention, and complete-kernel composition
separately.

Repeating the complete M03 transition is exact: if `K` preserves the declared
generalized posterior, then the composition `K^k` also preserves it. One
complete repetition contains the latent pseudo-AL scale refresh, the root-1
conditional draw, the root-2 conditional draw, and the global root-label swap.
Repeating only part of this scan is not the candidate implemented here.

## Immutable failed-run identities

| Object | Identity |
|---|---|
| Authorization commit | `868a5a0039fa37a2ed6a235d26035907f89e9ef3` |
| Reviewed implementation | `71682cee4d678cbd4e4d33b6068645f49b01bd0e` |
| Runtime tree digest | `1d7309a27fd0064a7ef389400b58371508ff93ff14cd63ec83b275b161757fcb` |
| Config SHA-256 | `dc7fbd4497dec331ac77adf84a79a6619793c7daebf0b7876e0ad83c511d2678` |
| Incidence SHA-256 | `b95aeeda3aef3fd1b69d4e98294e920ffa94ea7093d5c2c506a1ce3ef25b97c3` |
| Seed ledger SHA-256 | `3dc8483f4a777ab766704b901997295bed1c89db0590429a70f3116b233e948f` |
| Task plan SHA-256 | `66ab61d3ebb31c1c38798d71889f9b648a1c31c027f17bff0909f759bb1f2c28` |
| Wave plan SHA-256 | `c45ece172d89a366fede5264a757c45161a3d9578a892ef1f8dfb920cbc51838` |

The compact closeout independently verifies all 442 files declared by the two
completed wave manifests.

## Implemented hardening

The recovery lane is based on the current main source but immediately restores
`confirmatory_execution_authorized = FALSE`. It adds the following safeguards:

1. Every diagnostic failure now retains a compact RDS containing scalar chains,
   diagnostics, chain profiles, exact seed-state digests, schedules, and source,
   config, incidence, seed-ledger, and runtime identities.
2. A failed sentinel is serialized before the cell-stop `break`; non-sentinel
   diagnostic failures receive the same evidence treatment.
3. Full fitted objects, latent arrays, and state paths remain excluded.
4. The coordinator owns its lock inside a function, so `on.exit()` executes on
   normal R errors. Large state validators are evaluated invisibly, preventing
   seed-ledger dumps in stdout.
5. Fixed-design MCMC accepts `kernel_repetitions`, with default one. The value
   is validated and recorded in provenance, model specification, diagnostics,
   and fit metadata. Explicit one is bitwise identical to the default.
6. Package and fit schemas advance because the transition contract is now
   explicit; old fit/checkpoint objects are not silently treated as the new
   schema.

## Frozen development comparison

The candidate comparison is development evidence, not promotion evidence. It
requires a clean committed checkout, a fail-closed config, the exact reviewed
maximum seed ledger, numerical thread variables fixed at one, and a fresh
ignored output root.

### M03 candidates

| Candidate | Burn | Retained draws per chain | Complete-kernel repetitions | Primitive transition cost |
|---|---:|---:|---:|---:|
| Current baseline | 500 | 1,500 | 1 | 2,000 |
| Burn diagnostic | 3,000 | 1,500 | 1 | 4,500 |
| Retention diagnostic | 500 | 6,000 | 1 | 6,500 |
| Uniform long schedule | 3,000 | 6,000 | 1 | 9,000 |
| Exact composition | 1,500 | 3,000 | 2 | 9,000 |

All five candidates run four profiles on S03 replications 117, 13, 90, and
185. Replication 117 uses its original four ledger states. The original main
ledger has only a standard chain for the three guards, so profiles B--D use
predeclared sequential `nextRNGSubStream()` descendants of each guard's chain-1
state. Their full RNG states and SHA-256 digests are recorded before fitting.
This makes the guard test stronger without pretending the derived states were
part of the original authorization ledger.

### M08 candidates

| Candidate | Burn | Retained draws | Cases |
|---|---:|---:|---|
| Current baseline | 1,000 | 2,000 | S03 reps 13, 55, 94 |
| Uniform extension | 1,000 | 4,000 | S03 reps 13, 55, 94 |

M08 uses the original standard-chain states. No replication-specific extension
or replacement seed is permitted.

### Selection rule

A candidate is eligible only if every hard and guard diagnostic passes the
unchanged role-specific thresholds, every fit uses the exact joint target, and
the total numerical-repair count is zero. Among eligible candidates, selection
uses minimum primitive transition cost and then the predeclared candidate
order. Scientific response-performance quantities are not used.

This least-cost rule is preferable to maximizing observed ESS on the small
development set: it avoids overtuning transition effort to a few frozen paths
once the computational contract is satisfied. Autocorrelation and effective
draws per second are retained as explanatory sidecars.

If no M03 candidate passes, this protocol stops. It does not keep increasing
iterations until a chance pass occurs. An exact mode-bridging transition, with
replica exchange as the leading option, would require a separate derivation,
detailed-balance tests, toy-target validation, and a newly frozen comparison.

## Integration and promotion gates

After a candidate is selected, the source will apply it uniformly to its method
role and update iteration budgets. A fresh local-level wave-2 development gate
must pass before promotion. Promotion then requires a clean isolated runtime
built from the exact fail-closed commit and all of the following:

1. package, native sampler, confirmatory-contract, and failure-evidence tests;
2. M01 static and local-level gates;
3. M02 static and local-level gates;
4. horizon and fixed-design references;
5. a four-chain M03 S03 stress gate containing replication 117 and guards;
6. an M08 S03 stress gate containing replication 13 and guards;
7. the complete first two canonical waves from fresh promotion-only roots;
8. resource, artifact, TeX, and literature checks;
9. zero repairs, exact target status, complete hashes, and every frozen
   diagnostic passing.

Only a tracked fail-closed promotion closeout can precede authorization. The
authorization commit must then change the single execution flag and nothing
else. The new confirmatory run starts from zero under a fresh run ID; the failed
run is never resumed.

## Reproducible outputs

The recovery tracks source code, tests, the protocol, compact failed-run
closeout tables, selected candidate summaries, and promotion manifests. Raw
scalar-chain evidence, fitted objects, runtimes, logs, caches, and simulation
outputs stay under ignored local roots. The exdqlm and Q-DESN repositories are
read-only references and are not modified by this work.
