# RQR-DLM skewed-wave recovery plan

## Decision

The confirmatory run `rqr_dlm_main_20260802_32f6745` is terminal and must not
be resumed.  Its first two canonical waves passed, but the third wave failed
the frozen MCMC diagnostic contract for the local-level skewed response law.
The run is useful only as immutable computational evidence.  No interval-loss,
coverage, width, or method-comparison result from it is eligible for scientific
promotion.

The recovery must remain fail closed.  A fresh confirmatory launch is allowed
only after a correction is selected using development-only evidence and then
passes the complete affected wave and the full exact-promotion suite.

## Authenticated facts

- Authorization commit: `32f6745369b83040c0b1c4bd385c17072ee912d8`.
- Reviewed implementation commit: `fa17e3602c565cabc056ebfcf0457cf87b92e877`.
- Isolated runtime tree digest:
  `e6c42d648dffc8f3c4e98a2559e59bf4374db611a9565733979bc74c8bca3499`.
- Maximum seed-ledger digest:
  `3dc8483f4a777ab766704b901997295bed1c89db0590429a70f3116b233e948f`.
- Canonical plan: 110 waves and 8,400 replication tasks.
- Passed waves: static Gaussian and local-level Gaussian.
- Failed wave: local-level skewed, with 72 failed diagnostic rows in 20
  method evaluations.
- The failure class is exclusively `mcmc_diagnostic_failure`; artifacts,
  resource ceilings, exact-target checks, and numerical-repair checks did not
  cause the stop.

## Diagnosis

The failed wave contains two computational regimes.

1. `M01`, `M02`, `M06`, and `M09` have mostly near-boundary one-chain ESS or
   MCSE failures.  These are compatible with insufficient uniform transition
   length, although this explanation must be checked on exact replicated
   chains.
2. `M10` and `M11` have severe persistence.  In the four-chain S05/M11
   replication 74 sentinel, rank-normalized R-hat reaches 1.029 and bulk ESS
   falls to about 31 despite 9,000 retained draws per chain.  The component
   scale, interval width, midpoint, observed loss, and learned loss rate are
   strongly coupled.  A small schedule increase or an additional ASIS-only
   substep is not an adequate prespecified remedy.

The Gaussian waves passing under the same runtime shows that the production
RNG serializer and infrastructure correction worked.  The new failure is a
skewness-sensitive transition-mixing limitation, not a recurrence of the RNG
bug.

## Recovery stages and gates

### Stage 1: immutable closeout

Run the read-only failed-run closeout against the original run tree.  Rehash
every completed wave artifact and publish only compact audit tables.  The
source run must remain byte-for-byte unchanged.

Gate: every recursive manifest verifies; the closeout records no scientific
promotion and no reuse of partial outputs.

### Stage 2: reproducible forensics

Create a source-controlled forensic script that consumes the failed run,
closeout, and exact maximum seed ledger.  It must report:

- failures by wave, scenario, replication, method, chain role, and estimand;
- margins relative to the unchanged R-hat, bulk-ESS, tail-ESS, and MCSE gates;
- autocorrelation at lags 1, 5, 10, 25, and 50 for retained sentinel scalars;
- effective draws per second, clearly labeled as process-time estimates;
- component-scale, loss-rate, root-width, midpoint, and loss correlations;
- response and latent-path features for failed and passing skewed
  replications; and
- SHA-256 values for every direct input.

Gate: all inputs authenticate and no model is fitted by the forensic script.

### Stage 3: predeclared development comparison

Use exact target-preserving candidates only.  Candidate selection occurs in a
fresh ignored output root and cannot authorize a confirmatory run.

The candidate family addresses **whole-scan distance**.  It increases the
number of complete target-preserving transitions between retained draws.  A
complete native scan refreshes the loss rate where learned, pseudo-AL latent
variables, both root states, and evolution scale.  The exdqlm comparator is
run for the corresponding number of complete transitions and then thinned
back to the frozen retained size.  Thus all candidates contain the same number
of diagnostic draws.

This is preferable to another component-scale-only kernel edit at this stage.
The skewed-wave failures occur not only for component-scale RQR-DLM methods,
but also for the exdqlm dynamic-quantile comparator and the frozen-discount
RQR-DLM.  The cross-kernel pattern implicates transition distance under the
skewed response law rather than one component-scale substep.  The three
predeclared candidates are complete-scan multipliers 2, 4, and 8.  If none
passes, the run remains closed and a method-specific transition redesign is a
new bounded project.

Use one fixed hard cell for every failed method, plus both the standard-chain
and four-chain regimes.  The hard cells are S05/M01/replication 186,
S06/M02/replication 194, S06/M06/replication 196,
S05/M09/replication 77, S05/M10/replication 104, and
S05/M11/replications 74 and 172.  Replication 74 preserves the original
four-chain sentinel failure; the others preserve the original one-chain
standard role.  Each method also has a fixed passing guard in the opposite
diagnostic role.  Guards were selected before candidate execution as the
nearest fully passing replication in the same DGP after standardizing response
mean, standard deviation, skewness, excess kurtosis, maximum absolute
standardized response, latent range, latent-increment standard deviation, and
maximum absolute latent increment.  The comparison therefore comprises 93
fits: three candidates over 31 fixed chain jobs.

Selection is lexicographic:

1. every fit succeeds under `numerical_policy="fail"`;
2. every fit retains the exact joint target with zero repairs;
3. all hard and guard diagnostics pass unchanged thresholds;
4. among eligible candidates for each method, select the smallest complete-scan
   multiplier (elapsed time is only a recorded sidecar);
5. no replication-specific schedule, reseeding, retry, or post-hoc extension
   is permitted.

The preflight and execution are bound to one clean source commit, the reviewed
maximum seed ledger, and the attested exdqlm CRAN 1.1.0 runtime.  Per-job RDS
objects are written atomically under the ignored cache tree, which makes an
interrupted comparison resumable without retries or reseeding.  The detached
launcher places PID, process-group, and logs under the ignored log tree; the
read-only health checker reports exact published and remaining job counts.

### Stage 4: complete affected-wave gate

Apply the selected correction uniformly by method role.  Re-execute all 35
tasks in the local-level skewed sentinel wave from a fresh ignored root using
the reviewed maximum seed ledger.

Gate: 35/35 tasks, all method evaluations, and every frozen diagnostic pass;
all resource, artifact, provenance, exact-target, and zero-repair gates pass.

### Stage 5: exact promotion

Commit the correction with `confirmatory_execution_authorized=FALSE`, build a
fresh isolated runtime from that exact commit, and rerun:

- the canonical Gaussian and skewed sentinel waves;
- component-scale, dynamic-quantile, fixed-design, horizon, and future-root
  gates;
- byte-level continuation and production seed-binding checks;
- the resource envelope;
- package/native tests, `R CMD check`, manuscript and supplement builds, and
  the literature manifest.

Gate: all checks pass from one exact clean commit and one attested runtime.

### Stage 6: authorization and fresh launch

Create a separate commit whose only semantic change is flipping the
confirmatory flag to `TRUE`.  Build/verify its exact isolated runtime, create a
new run ID, and launch from wave 1.  The 45 previously passed tasks are not
mixed with the corrected run.

The generalized-Bayes construction remains a loss update for interval roots.
Neither the development diagnostics nor a successful relaunch defines an
ordinary response likelihood or posterior-predictive response distribution.

## Stop conditions

Remain fail closed if any of the following occurs:

- no predeclared candidate clears both hard and guard cells;
- the complete skewed wave has any failed diagnostic;
- a numerical repair, target mismatch, runtime mismatch, artifact mismatch,
  retry, reseed, or resource-ceiling event occurs;
- the correction requires a replication-specific exception; or
- the projected full-study resource budget becomes infeasible.

In that case, redesign the transition kernel as a separate bounded project;
do not weaken the frozen diagnostic gates and do not launch the main study.
