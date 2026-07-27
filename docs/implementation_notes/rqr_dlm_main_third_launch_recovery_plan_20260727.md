# RQR-DLM third-launch recovery and relaunch plan

## Objective

Repair the singleton comparator projection and local-level component-scale
mixing boundary without changing the generalized-Bayes target, priors,
diagnostic thresholds, scenario design, seeds, estimands, comparator versions,
or no-retry policy.  Relaunch only after exact-runtime correction gates pass.

## Exact-promotion update

The first clean promotion at
`e9c8068b4d9f135b7d717c3b072754f3b13f1e1a` passed M01 wave 1, both M02
waves, the horizon/fixed-design gate, and the resource gate. M01 wave 2
completed all 49 fits but passed only 1,144 of 1,150 diagnostics. Five
ordinary one-chain tasks retained inadequate component-scale ESS. The
one-root transition is therefore superseded by a symmetric composition that
integrates and redraws root 1 conditional on root 2 and then integrates and
redraws root 2 conditional on the refreshed root 1. Each block is invariant
for the same target. A one-cycle symmetric development wave improved the
second-wave boundary but still passed only 1,147 of 1,150 diagnostics, so the
current candidate composes a second exact centered--noncentered ASIS cycle.
The failed gates are not reused, and execution remains false. Exact values
and hashes are recorded in
`docs/audits/rqr_dlm_exact_promotion_e9c8068_closeout_20260727.md`; the
current finish sequence is summarized in
`docs/implementation_notes/rqr_dlm_two_ASIS_finish_plan_20260727.md`.

## Independent diagnosis and design decision

| Observed boundary | Diagnosis | Selected correction | Why this is the narrow correction |
|---|---|---|---|
| one-state exdqlm projection | base-R dimension dropping changed `1 x T` to `T x 1` | dimension-preserving array extraction followed by the existing `FF` projection | preserves the CRAN 1.1.0 object and observation contract |
| full second-wave M01 gate | 49 of 49 fits completed at 6,000 retained draws, but only 1,131 of 1,150 diagnostics passed; 12 of 25 tasks failed, principally because \(\log q_1\) retained autocorrelation of 0.93--0.98 | add exact symmetric rootwise partially collapsed scale transitions and two centered--noncentered ASIS cycles | integrates each root path in turn and interweaves the centered and noncentered scale parameterizations while preserving the target, prior, threshold, seed, and fixed schedule |
| prospective learned component-scale cells | the same role mismatch existed at 3,000 versus 9,000 retained draws | match both learned component-scale roles at 9,000 retained draws before observing those cells | avoids a post-result, method-specific schedule decision |
| M02 between-chain disagreement | the four supposed initialization profiles shifted `model$m0`, which is a prior mean and therefore part of the target; moreover, CRAN exdqlm 1.1.0 ignores `sig.init` when `init.from.vb=FALSE` | hold `m0`, `C0`, evolution, discounts, and priors common; supply distinct initial state paths and scales through the existing `vb_init_fit` MCMC-initialization interface | restores a valid same-target, genuinely overdispersed multi-chain diagnostic without modifying exdqlm |
| M02 role schedule mismatch | the standard role was already frozen at 4,000 retained draws per endpoint while the sentinel role used 2,000 | match the complete sentinel role to the 4,000-draw standard role | removes a role-only computational difference prospectively |
| missing compact failure rows | the exception arose after fitting but outside the structured diagnostic boundary | convert the exception to a hashed, atomic failure record and systemic stop | preserves intention-to-run accounting and fail-closed behavior |
| 95.1-percent RSS use | four full sentinel fits were accumulated and their local-only RDS was immediately duplicated in memory | extract endpoint/diagnostic scalars per chain, release each full fit, and atomically store only compact local diagnostics | restores resource margin without weakening promoted compact evidence |

The following alternatives are rejected:

1. **Resume the stopped run.**  This would mix source states and retain selected
   passed tasks from a failed wave sequence.
2. **Patch exdqlm.**  The retained object is valid under CRAN 1.1.0; the defect
   is solely in RQR-GIBBS extraction.  The protected repository remains
   read-only.
3. **Extend only failed chains.**  A diagnostic-triggered extension would make
   the schedule outcome dependent and violate the frozen no-retry contract.
4. **Validate only replications 16, 20, and 28.**  The complete 25-task
   local-level wave is the smallest design-level gate that checks both
   sentinel and ordinary roles without selecting only known failures.
5. **Raise the memory ceiling.**  The duplicate allocation is avoidable, so a
   larger ceiling would conceal rather than correct the implementation issue.
6. **Launch before exact-source validation.**  Development results can reject
   a correction but cannot authorize a commit-bound scientific execution.
7. **Increase the M01 schedule again.**  The complete fixed-schedule gate
   establishes that residual scale--trajectory dependence, not merely the
   number of retained draws, is the remaining boundary.  A transition
   correction is more principled and more efficient than another uniform
   chain-length inflation.
8. **Use a threshold or replication-specific remedy.**  Threshold weakening,
   selective restarts, and failure-triggered extensions remain prohibited.

## Work packages

### A. Fail-closed source correction

1. Set `confirmatory_execution_authorized = FALSE`.
2. Preserve `p x T` and `p x draws` shapes explicitly when retained state
   dimension `p` equals one.
3. Exercise retained singleton draws in the actual isolated exdqlm 1.1.0
   oracle/reference gate, not only in synthetic matrix tests.
4. Publish diagnostic-construction exceptions through the compact failure
   ledger before a systemic stop.
5. Reduce every completed chain immediately to endpoint vectors and diagnostic
   scalars, release the full fit before the next chain, atomically hash the
   compact local diagnostic sidecar, and release all task-local bindings before
   a sequential worker advances.
6. Add a deterministic C++ Kalman log-marginal calculation with an independent
   R implementation and dense-Gaussian parity test.
7. Compose the exact partially collapsed blocks in both root orientations:
   update \((q,\theta_1,\theta_{10})\) conditional on root 2, then update
   \((q,\theta_2,\theta_{20})\) conditional on the refreshed root 1.
8. Compose two exact centered--noncentered ASIS cycles and the global label
   swap without changing the generalized posterior.

### B. Fixed schedule

Use 6,000 retained draws for every fixed-rate component-scale chain in both
standard and sentinel roles.  Use 9,000 retained draws for every learned-rate
component-scale chain in both roles.  This matches the already declared
standard schedules and is fixed before correction validation.

Use 4,000 retained draws per endpoint for M02 in both standard and sentinel
roles.  The dimension-correct development wave finished all 98 endpoint fits,
but its between-chain diagnostic exposed that profile-specific shifts to
`model$m0` had changed the time-zero prior mean.  Those R-hat and ESS values
cannot be interpreted as same-target mixing evidence.  The target correction
removes every `m0` shift, records one common-target digest, and permits only
the target-preserving warm-start state and RNG stream to differ.  The
4,000-draw schedule is fixed for the whole M02 sentinel role to match the
already frozen standard role, not extended for selected replications.

The RQR-GIBBS adapter constructs only the three warm-start moments consumed by
the CRAN 1.1.0 DQLM branch: the initial state path, scale, and latent-scale
vector.  Their four digests must be distinct at every sentinel.  These
algorithmic values are excluded from the common-target digest; `model$m0`,
`model$C0`, `FF`, `GG`, the response vector, discounts, component dimensions,
`PriorSigma`, and the quantile probability must be identical across chains.

No chain is extended in response to its diagnostics.  There is no retry,
reseed, threshold reduction, or failed-scientific-metric selection.

The partial collapse leaves these iteration counts unchanged.  It does add
deterministic filter evaluations within each component-scale iteration.
Consequently, launch readiness requires a new measured per-iteration timing
and process-tree resource envelope; iteration counts alone are not presented
as a wall-clock forecast.

A development-only, shortened comparison on the diagnosed S03 replication 28
selected three slice sweeps per rootwise scale block before the exact wave
gates. Across four
overdispersed profiles and 1,500 retained draws per chain, three sweeps passed
all checked diagnostics; for \(\log q_1\), \(\widehat R=1.0014\), bulk
ESS \(=405.4\), tail ESS \(=892.6\), and MCSE/SD \(=0.0497\). Two sweeps
narrowly missed the unchanged bulk-ESS gate at 381.0, while six sweeps failed.
Three sweeps cost 471.8 seconds per chain on average versus 459.2 seconds for
two. These dirty-source fits are transition-selection evidence only and cannot
authorize execution or contribute scientific metrics.

A complete one-cycle symmetric development wave later reduced the failed
second-wave M01 rows from six to three, but it did not satisfy the unchanged
diagnostic contract. The current candidate therefore composes a second ASIS
cycle prospectively and must pass the complete affected wave before it can
enter an exact-runtime promotion.

The exact maximum workload becomes:

- 110 canonical waves;
- 8,400 replication tasks;
- 40,938 MCMC chain executions;
- 205,658,000 MCMC iterations.

The maximum iteration count is 74.8257 percent above the original Output-15
budget of 117,636,000 iterations.  It is 1.5164 percent above the
component-scale-only contract of 202,586,000 iterations and 3.2949 percent
above the previously launched ASIS-corrected contract of 199,098,000
iterations.  The comparison with the original study is the appropriate
total-cost statement; the smaller increments isolate the successive
fixed-schedule corrections.

Applying the selected hard-case timing factor to the entire workload, rather
than only to the 37.6--42.5 percent of iterations that use component-scale
methods, gives a deliberately conservative 32-worker envelope of 156.1,
280.5, and 408.0 wall hours for the initial, central, and maximum plans. These
are capacity bounds, not completion-time promises. The exact complete-wave
gates provide the final representative timing evidence before launch.

### C. Promotion correction gates

The correction must pass:

1. synthetic singleton and multistate state-array extraction;
2. an actual isolated exdqlm one-state fit and retained-draw projection;
3. all M02 interval-chain jobs in both the first and failed local-level
   sentinel waves;
4. dense-Gaussian and R/C++ equality for the partially collapsed scale
   log target, plus byte-identical continuation under its checkpoint contract;
5. all M01 jobs in both waves under the fixed schedules and the prospectively
   frozen three-sweep symmetric rootwise, two-ASIS transition;
6. all frozen R-hat, bulk ESS, tail ESS, and MCSE gates;
7. zero numerical repairs and exact-target status;
8. a worst-case resource set covering high state dimension, long training
   horizon, and learned component scale;
9. native tests, standalone contracts, `R CMD check`, smoke, literature
   manifest, protected exdqlm validation, and both TeX builds.

If any exact M01 stream still fails, the relaunch remains closed. The result
must not be converted into an adaptive extension, looser gate, or selective
retry.

### D. Review and relaunch

Commit and push the fail-closed implementation and compact evidence.  Build a
fresh isolated primary runtime from that exact commit.  After review, create a
commit whose only difference is the false-to-true execution flag.  Rebuild the
runtime, regenerate preflight and oracle/reference bundles, create a new
authorization bundle, and start a fresh detached coordinator.

No output from any earlier failed run is copied into the new run root.

The relaunch decision is mechanical:

| Gate state | Decision |
|---|---|
| any projection, MCMC diagnostic, exact-target, provenance, resource, test, package, or TeX failure | remain closed; do not create an authorization commit |
| every gate passes at the clean implementation commit | create and verify a flag-only authorization commit |
| authorization preflight or oracle/reference mismatch | remain closed; do not start a coordinator |
| authorization bundle passes exactly | start one detached coordinator under a fresh run ID |

The detached launch is not considered successful merely because its process is
alive.  Progress is read only from the append-only wave ledger.  Any failed
wave permanently blocks later waves in that run; completion requires all
canonical decisions plus the final audit.

### E. Completion and analysis

Only a run with a verified final audit can supply scientific results.
Thereafter:

1. verify the intention-to-run denominator and every failure ledger;
2. compute paired ADEMP summaries and Monte Carlo errors;
3. evaluate coverage, width, RQR loss, endpoint recovery, and horizon effects;
4. apply the frozen Holm adjustment to directional contrast families;
5. produce compact tables and figures from hashed summaries; and
6. update the manuscript with claims restricted to the verified protocol.
