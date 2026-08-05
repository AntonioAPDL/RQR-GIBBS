# DLM/SH Oracle-Tilt Illustration Adjudication Protocol

## Purpose

The version-3, 95% single-data illustration completed all 27 planned chains.
Five family/target cells passed every frozen gate. The dynamic-linear
shortest-interval cell completed all five chains without repairs or pathological
draws, but missed one bulk-ESS threshold and one width-heterogeneity threshold
by narrow margins. This protocol defines one—and only one—adjudication run.

The exercise concerns interval-root generalized posteriors. It is not a
response-likelihood analysis, a posterior-predictive response calculation, or
a repeated-sample simulation study.

## Diagnosis

The original DLM/SH result had bulk ESS 998.79 for the mean upper endpoint,
against the predeclared minimum of 1,000. Its high-to-low scale width-contrast
error was 20.104%, against the predeclared maximum of 20%. R-hat, tail ESS,
MCSE, endpoint recovery, mean width, realized single-data content, numerical
exactness, conditional R/C++ parity, provenance, and pathology checks passed.

Chain- and draw-block summaries fluctuate around the width-contrast boundary,
while the seasonal width amplitude is consistently attenuated. Consequently,
blindly adding a small number of draws or altering the tolerance would not be
a defensible correction. There is no evidence supporting a change to the DGP,
oracle tilt, state evolution, prior, or sampler transition.

## Frozen adjudication

The adjudication keeps the following objects unchanged:

- the standardized asymmetric-Laplace innovation law with source index 0.80;
- 95% target content and the exact population shortest-interval tilt;
- the simulated response, missingness pattern, model matrices, fixed evolution
  covariance, priors, five initialization profiles, and five chain seeds;
- the C++ backend, numerical fail policy, learning rate, and all diagnostic and
  recovery thresholds.

Only the number of retained draws changes, from 6,000 to 12,000 per chain.
Burn-in remains 2,500 and thinning remains one. The chains are recomputed from
their original seeds because the compact publication artifacts intentionally
exclude heavy continuation checkpoints.

## Prefix requirement

For every chain, the first 6,000 recomputed retained draws must be bitwise
identical to the original lower-endpoint, upper-endpoint, and scalar diagnostic
draws. The immutable baseline manifest binds all five original worker files,
their byte counts, hashes, worker-contract digests, seeds, profiles, DGP digest,
target digest, source state, runtime binding, cell receipt, and wrapper manifest.

Any prefix mismatch is a hard failure. It cannot be overridden.

## Decision rule

Automatic promotion requires the extended DLM/SH cell to pass every original
diagnostic, recovery, heterogeneity, provenance, parity, and pathology gate.
The adjudication does not relax or reinterpret those gates.

If hard integrity checks pass but the cell remains non-strict, the closeout is
`descriptive_review_required`. That label is not a validation pass and cannot
authorize automatic figure promotion. No second automatic adjudication is
allowed. A later manuscript decision may either report the non-strict result
transparently as an illustration limitation or omit the DLM/SH panel.

## Reproducible workflow

1. Verify the immutable baseline and run adjudication preflight.
2. Commit the adjudication source and build an isolated runtime from that exact
   full SHA.
3. Launch the five-chain run under the monitored process-group wrapper.
4. Require zero residual processes, no timeout, no resource-limit event, exact
   runtime provenance, all five worker artifacts, and 15/15 prefix checks.
5. If and only if strict status is obtained, reconcile the five original strict
   cells with the extended DLM/SH cell and render manuscript figures from the
   compact reconciled evidence.

Large worker objects remain under ignored output roots. Only source, frozen
contracts, compact summaries, hashes, and documentation are eligible for Git.
