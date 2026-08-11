# RQR-DLM M02 diagnostic-thinning recovery plan

Date: 2026-08-08

## Objective

Recover the diagnostic-aware maximum RQR-DLM study after the deterministic M02
training/future diagnostic draw-count mismatch, without changing any model,
generalized-Bayes target, comparator target, random-number binding, MCMC
transition, frozen performance metric, or convergence threshold.

## Design principles

1. **One retained-index contract.** Construct one deterministic retained-index
   vector from the raw M02 draw count and frozen diagnostic thinning factor.
   Apply it to both endpoint ordinate arrays and both terminal-state arrays.
2. **Fail before recycling or binding.** Require equal lower/upper raw draw
   counts, require each terminal array to match the raw ordinate count, and
   require all thinned training and future arrays to have the same count.
3. **No inferential change.** The correction is a diagnostic projection fix.
   It does not change the MCMC kernel or the fitted/forecast endpoint summaries.
4. **Production-path coverage.** Tests must exercise the frozen M02 multiplier
   of two rather than relying only on schedule overrides that imply thinning
   one.
5. **Fresh evidence only.** The terminal failed run remains immutable.  No fit,
   seed retry, metric, or partial output is imported into the replacement run.

## Implementation stages and gates

### Stage 1: deterministic source correction

- Validate equal raw endpoint draw counts.
- Validate that thinning does not exceed the available retained draws.
- Apply the common retained indices to training ordinates and terminal states.
- Validate equal thinned training/future draw counts before constructing the
  exact diagnostic schema.

Gate: a synthetic exdqlm object with eight raw draws and thinning two must
produce four aligned training, terminal, and horizon-1 through horizon-20
diagnostic draws.  Unequal endpoint counts and excessive thinning must stop.

### Stage 2: source validation

- Run the complete confirmatory-contract test file.
- Run the complete package test suite and native R/C++ tests.
- Run the diagnostic-aware completion tests.
- Run `R CMD check --no-manual --no-build-vignettes`.
- Run repository smoke, manuscript, supplement, literature-manifest, and
  protected exdqlm reference tests where supported by the Makefile.

Gate: no failure or dirty generated source artifact.  Any pre-existing,
scope-unrelated warning or platform-dependent skip must be identified rather
than silently counted as new correction evidence.

### Stage 3: exact runtime and real M02 canary

- Commit the corrected fail-closed source with execution still unauthorized.
- Build and install the package from an exact Git archive into a new ignored
  isolated runtime root.
- Use the existing isolated CRAN exdqlm runtime; never load or compile the
  protected exdqlm checkout.
- Execute one fixed, predeclared S01/M02 production-schedule chain with the
  transition multiplier two.
- Require exactly 8,000 raw endpoint state draws, 4,000 scalar diagnostic rows,
  the complete exact diagnostic schema, finite future-root functions, zero
  diagnostic-construction failures, and bound runtime/source provenance.

This canary is an execution-path check, not a simulation pilot and not article
evidence.

### Stage 4: fresh launch evidence

- Generate maximum-plan preflight evidence from the corrected exact commit.
- Regenerate oracle/comparator references and recursive hashes.
- Prepare a new diagnostic-aware authorization bound to the exact source,
  runtime, preflight, reference, comparator runtimes, toolchain, seed ledger,
  and execution policy.
- Verify that authorization remains fail closed until the separate launch
  boundary is explicitly materialized.

Gate: all hard preflight/reference/authorization checks pass and all artifacts
rehash exactly.

### Stage 5: complete maximum run

- Create a new immutable run ID and empty run/control roots.
- Launch the full maximum design: 110 waves, 8,400 DGP-replication tasks, and
  43,800 method-replication results.
- Keep precision stopping disabled.
- Keep frozen R-hat, ESS, tail-ESS, and MCSE thresholds unchanged and reported,
  but nonblocking under the diagnostic-aware policy.
- Retain hard stops for provenance, numerical repair, fit construction,
  endpoint validity, diagnostic construction, resources, and artifact
  integrity.
- Allow canonical wave 1 to serve as the real launch-path gate; later waves are
  authorized only after its successful terminal record.

### Stage 6: closeout

At completion, verify every wave and recursive manifest, publish the all-results
aggregate and the predeclared unflagged-diagnostics sensitivity aggregate,
summarize warnings without deleting replications, and only then determine which
results support the manuscript.  RQR interval-root summaries must remain
distinct from posterior-predictive response draws.

## Stop conditions

Do not continue the maximum run after any hard failure.  Do not weaken frozen
thresholds, retry or reseed a failed replication, selectively extend a chain,
resume the failed 2026-08-07 root, or mutate exdqlm/Q-DESN source repositories.
