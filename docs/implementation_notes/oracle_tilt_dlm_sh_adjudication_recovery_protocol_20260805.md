# DLM/SH adjudication software-recovery protocol

## Objective

This protocol permits one replacement execution after the original job failed
before publishing any worker result. It preserves one statistical adjudication
while distinguishing two software executions: the invalidated execution and
the bounded replacement. The replacement is explicitly authorized; it is not
an automatic rerun.

## Frozen scientific contract

The replacement retains the immutable version-3 DLM/SH data, missingness,
population shortest-interval tilt, 95% target content, standardized
asymmetric-Laplace innovation law with source index 0.80, model matrices,
evolution covariance, prior, learning rate, C++ backend, numerical fail policy,
five seeds, and five initialization profiles. Each chain uses 2,500 burn-in
iterations, 12,000 retained draws, and no thinning. The first 6,000 retained
lower-endpoint, upper-endpoint, and scalar draws must reproduce the baseline
bitwise.

No response distribution is inferred from the generalized posterior. The
stored draws describe interval roots, not posterior-predictive responses.

## Versioned execution identity

The recovery configuration records execution attempt two of at most two,
statistical attempt one of one, the invalidated source commit, and hashes of
the failed wrapper evidence. It records zero invalidated worker artifacts and
declares that neither the scientific contract nor the statistical-attempt
count changed.

## Staged fail-closed execution

1. Bind the immutable baseline, DGP, target, source commit, and isolated
   runtime.
2. Run a synthetic production-shaped worker-contract self-test. A valid
   12,000-draw endpoint-only envelope must pass; a payload containing a
   forbidden midpoint matrix must fail.
3. Run chain 1 alone. Atomically save it, re-read it, revalidate it, and require
   all three baseline-prefix comparisons to pass.
4. Only after step 3, run chains 2--5 in bounded two-worker batches. Singleton
   and parallel workers use the same captured-error representation.
5. Re-read and revalidate all five saved workers. Require 15/15 prefix checks,
   then calculate the unchanged diagnostics, recovery measures, block
   stability summaries, and scientific decision.
6. Preserve stage status, chain-specific failures, worker hashes, source and
   runtime identity, resource telemetry, and final closeout atomically.

The staged order changes wall-clock orchestration only. Chain-specific seeds
and RNG streams are unchanged, so the first 6,000-draw bitwise contract also
tests that staging did not alter any Markov chain.

## Acceptance rule

Strict promotion requires the original diagnostic, recovery, heterogeneity,
provenance, conditional-parity, pathology, numerical-repair, and resource
gates, plus all prefix and staging gates. A computationally sound but
non-strict result is labeled `descriptive_review_required`; it is not a strict
pass. A hard-integrity failure stops the workflow. No additional automatic
execution is authorized.

## Validation order

The implementation must pass, in order:

1. R parse and shell syntax checks;
2. focused adjudication regression tests;
3. version-3 illustration tests;
4. complete native package tests and `R CMD check --no-manual`;
5. exact isolated-runtime construction and provenance verification;
6. exact adjudication preflight, including the worker-contract self-test; and
7. the monitored staged replacement execution.

Large worker objects and live logs remain ignored. Only source, contracts,
audits, manifests, summaries, and reconciliation evidence are eligible for
Git.
