# RQR-DLM joint-ESS promotion and affected-wave plan

Date: 2026-08-05

Scope: RQR-DLM computational validation only

Scientific simulation authorization: `FALSE`

## Decision

The bounded joint elliptical-slice comparison recovered the two unresolved
component-scale methods without changing the generalized-Bayes target, priors,
seeds, diagnostics, or replication roles. The comparison selected:

| Method | Frozen transition | Complete-scan multiplier | Joint cycles |
|---|---|---:|---:|
| M10 | `joint_ess1_x1` | 1 | 1 |
| M11 | `joint_ess1_x2` | 2 | 1 |

The earlier whole-scan comparison had already selected multiplier two for
M01, M02, M06, and M09. M07 and M08 retain their previously declared
schedules. These policies are method-wide: they do not depend on a scenario,
replication, chain result, or observed diagnostic.

The candidate evidence is necessary but insufficient for scientific
promotion. It contains 44 successful jobs, exact-target status throughout,
zero numerical repairs, and 910 of 932 diagnostics passing across the whole
candidate set. The 22 failures occur only in rejected candidates. The selected
M10 and M11 rows pass 92/92 and 141/141 diagnostics, respectively.

## Why this transition is the appropriate correction

The remaining problem was persistence in component-scale and path directions,
not a mismatch in the declared posterior target. Repeating only ASIS had
reached a nonmonotone floor on the hard cases. A joint state-path elliptical
slice move is exact conditional on the other Gibbs blocks, respects the
Gaussian state prior, and evaluates the same augmented loss kernel. It
therefore improves movement along the coupled path direction without changing
the target or pretending that the generalized loss update is a response
likelihood.

The implementation retains the exact symmetric rootwise partially collapsed
scale update and centered/noncentered interweaving. The joint move is an
additional invariant transition. M10 uses the minimum eligible cost. M11 uses
the next predeclared complete-scan distance because the base-distance candidate
failed its frozen diagnostics.

## Reproducibility architecture

The integration is based on current `origin/main`, preserving its normalized
source-tree runtime digest and isolated-build lineage checks. The fit schema is
`rqrgibbs_fit/1.18.0`; the package development version is `0.1.0.9032`; and the
simulation correction contract is
`rqrgibbs_dlm_main_correction/1.15.0`.

All effective production transitions are derived from one frozen
`method_transition_policies` table. There are no hidden worker overrides.
Native fits implement a multiplier by multiplying burn-in and thinning while
holding the number of diagnostic draws fixed. The CRAN exdqlm comparator does
not expose thinning, so M02 runs the multiplied retained transition count and
uses a fixed deterministic diagnostic thinning. Point summaries may use all
retained comparator draws; convergence diagnostics use the predeclared 4,000
draws per endpoint.

The corrected primitive-replica iteration budgets are:

| Planning design | Total iterations |
|---|---:|
| Initial | 161,046,000 |
| Central | 301,698,000 |
| Maximum | 442,350,000 |

The maximum design still contains 8,400 replication tasks and 40,938 MCMC
chain executions. The increased transition distance changes computation, not
the ADEMP incidence matrix.

## Validation ladder

### 1. Successful candidate closeout

The tracked compact closeout authenticates all 54 input artifacts and records
both method selections. Heavy job objects remain ignored. Candidate outputs
are development evidence and are never reused as affected-wave or scientific
outputs.

### 2. Source and package gates

Before any development wave:

1. require a clean committed source;
2. build an isolated runtime from an exact Git archive of that commit;
3. verify source-package and installed-runtime lineage;
4. run the confirmatory contract suite and native R/C++ suite;
5. run `R CMD check` and the fail-closed execution probes.

### 3. Higher-dimensional S10 guard

The selected policies are first evaluated on preselected S10 sentinel cases:

| Method | Scenario | Replication | Chains |
|---|---|---:|---:|
| M10 | S10, trend plus regression | 77 | 4 |
| M11 | S10, trend plus regression | 166 | 4 |

The guard requires all frozen R-hat, bulk-ESS, tail-ESS, and MCSE gates, exact
target status, zero repairs, no retry, no reseeding, and the isolated runtime.
It is a pass/fail validation of the already selected policy, not another
selection exercise.

### 4. Complete affected S05/S06 sentinel wave

Only after the S10 guard passes, run the exact canonical wave
`local_level_skewed_T200__target0200__sentinel`:

- 35 replication tasks;
- 278 method evaluations;
- eight frozen worker slots;
- every task an embedded four-chain sentinel where applicable;
- no changed seeds, thresholds, targets, or selective extensions.

The new `development-affected-wave` runner mode is deliberately separate from
`sentinel-core` and `execute-confirmatory`. It requires a fail-closed config,
an explicit development-gate environment value, the exact affected-wave task
set for one declared worker slot, and no authorization bundle. Its run manifest
sets `development_outputs_reusable=FALSE` and `scientific_promotion=FALSE`.
The scientific collector rejects this mode.

Each worker is process-group monitored with one numerical thread, a 1.5-GiB
RSS ceiling, a four-thread operating-system envelope, and a 72-hour timeout.
The coordinator runs all eight slots once and performs no retry or reseeding.

### 5. Promotion after the affected wave

If and only if all 35 tasks and every frozen diagnostic pass:

1. promote a compact authenticated affected-wave closeout;
2. run the complete Gaussian and skewed exact-promotion ladder;
3. repeat component-scale, comparator, fixed-design, horizon/future-root,
   continuation, seed-binding, and resource gates;
4. run package, native, TeX, and literature checks;
5. commit the final fail-closed implementation;
6. create a separate flag-only authorization commit;
7. launch a fresh 8,400-task simulation under a new run ID.

No development, failed, or candidate fit becomes a scientific result. The
article may use results only after the fresh scientific run is complete and
its coverage, loss, width, endpoint-recovery, and sensitivity summaries have
passed their declared analysis contract.

## Operational files

- `application/scripts/51_validate_rqr_dlm_s10_transition_guard.R`
- `application/scripts/51_prepare_rqr_dlm_affected_wave_validation.R`
- `application/scripts/52_orchestrate_rqr_dlm_affected_wave_validation.sh`
- `application/scripts/52_launch_rqr_dlm_affected_wave_validation.sh`
- `application/scripts/52_closeout_rqr_dlm_affected_wave_validation.R`
- `application/scripts/53_healthcheck_rqr_dlm_affected_wave_validation.R`

These scripts write heavy or live artifacts only under ignored
`application/cache/` and `application/logs/` roots. Only compact reviewed
closeouts are candidates for later tracking.
