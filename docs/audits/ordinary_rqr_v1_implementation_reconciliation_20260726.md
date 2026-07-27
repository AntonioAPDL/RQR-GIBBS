# Ordinary RQR version-1 implementation reconciliation

Date: 2026-07-26

Implementation candidate:
`bc482e5eb922859bf5d55cccba4e775ca071ef7d`

Protected RQR-DLM baseline:
`ce02915f8e6270fb21c4cce1bdc231beeda12292`

Pinned exdqlm reference:
`dffb71ee70b597d6a716ee74be1cbc99731cd453`

Q-DESN article reference:
`f9f22804eff3871bb5350c8add04b7c9f4d4957b`

## Decision

The ordinary, zero-tilt RQR source implementation is complete enough to enter
exact-commit release validation. It is not yet a release claim and it does not
authorize the 48-fit mechanics study. The checked-in ordinary-v1 execution
flag remains `FALSE`, the reviewed implementation SHA remains unset, and no
ordinary-v1 validation fit has been launched.

The implementation preserves the intended scientific interpretation:

- the update is a generalized-Bayes loss update;
- the pseudo-asymmetric-Laplace representation augments the RQR loss applied
  to the product residual;
- retained draws describe two root functions and their induced ordered
  interval;
- those draws are not posterior-predictive response draws; and
- neither the learned inverse-loss scale nor the fixed loss rate is described
  as a response variance or precision.

## Audited architecture

The final architecture uses the smallest common computational core that
preserves the genuinely different model structures.

| Layer | Authority | Reason |
|---|---|---|
| Static fixed design | Native `rqrgibbs` alternating-root Gibbs engine | It is the common regression target for ridge, full Gaussian, and RHS-NS priors. |
| Regularized regression | A modular RHS-NS prior transition inside the static engine | It avoids a second sampler and keeps prior state, swaps, continuation, and diagnostics explicit. |
| DESN | Frozen, attested design adapter delegating to the static engine | Conditional on a reservoir design, the readout is a fixed-design RQR regression; duplicating the sampler would create avoidable target drift. |
| DLM | Native sequential root-specific FFBS engine | The stacked prior is Gaussian but the augmented two-root observation kernel is jointly quartic. One simultaneous Gaussian FFBS draw is therefore unavailable, whereas alternating root-specific FFBS steps are exact for the declared fixed-joint modes. |

Four tempting alternatives were rejected:

1. a second DESN-specific Gibbs sampler, because it would duplicate the
   static target and continuation logic;
2. a single Gaussian FFBS draw for both stacked roots, because the augmented
   joint observation term is not Gaussian;
3. immediate nonzero mean-tilt generalization, because it would expand the
   target before the ordinary special case is independently validated; and
4. a matched or production simulation before bounded target-mechanics
   evidence, because empirical performance cannot repair a transition,
   continuation, or provenance defect.

This separation is therefore intentional rather than an incomplete attempt to
force every model through one implementation.

## Implemented model boundary

The candidate contains:

- fixed-design RQR with ridge, full Gaussian, fixed-shoulder RHS-NS, and
  sampled-shoulder RHS-NS priors;
- fixed and normalized learned inverse-loss-rate modes;
- exact observed-mask omission for missing responses;
- full fitted-object, checkpoint, continuation-history, and segment-schedule
  integrity contracts;
- strict root/interval prediction boundaries and explicit draw binding;
- a frozen-design DESN interface with attested exdqlm materialization and no
  exdqlm inference dependency;
- DLM fixed-\(W_t\), frozen-discount-template, and shared
  component-evolution-scale modes;
- root-only future functionals for static, DESN, and DLM objects;
- exact estimand schemas, maintained convergence diagnostics, atomic compact
  artifacts, fail-stop cell execution, and a process-group monitor; and
- package documentation, a vignette, CI, manuscript text, and supplement
  derivations consistent with the source boundary.

Nonzero mean tilt, variational inference, learning-rate calibration, a
response-simulation distribution, conditionally adaptive discount evolution,
and matched empirical comparisons remain outside ordinary RQR version 1.

## Crucial audit corrections

| Finding | Resolution |
|---|---|
| Static seed and checkpoint ambiguity | Every seed alias now conflicts with an explicit RNG state, and RNG installation is deferred until all late controls validate. |
| Static/DESN continuation integrity | Versioned checkpoints, histories, and schedules bind data, design, target, prior state, terminal state, RNG state, retained draws, and iteration arithmetic. |
| RHS diagnostics | Label-invariant ordered local/global/shoulder summaries are primary; root-specific traces are sidecars. Fixed shoulder values are checked by exact identity and excluded from stochastic diagnostics. |
| DLM public continuation bypass | Continuation-only initialization is private and reachable only after complete fit validation. |
| DLM fitted-object integrity | Fresh, continued, and read-only paths validate the full fit envelope, history/schedule recursion, terminal and retained state binding, transition digests, latent variables, rate draws, and component scales. |
| DLM schedule integrity | A versioned, digested segment schedule records generation, iteration, burn-in, retention, thinning, and parent-checkpoint linkage. |
| DESN source authority | exdqlm is used only to materialize an exact pinned frozen design under an isolated Git-archive runtime. Sampling is native to `rqrgibbs`. |
| Public prediction ambiguity | Predictors validate feature names, dimensions, draw provenance, indices, rates, and future contracts, and return root/interval functionals only. |
| Release evidence ambiguity | Preflight, reference-only, one-cell benchmark, and 48-fit execution are distinct fail-closed modes. A disabled source candidate cannot launch the full grid. |

## Source validation completed

The candidate passed the local source/package/document matrix recorded in
`ordinary_rqr_v1_implementation_evidence_20260726/validation_matrix.csv`.
Highlights are:

- all 109 R files parsed;
- every shell script passed `bash -n`;
- `git diff --check` passed;
- the static sampler, fixed-design, RHS-NS, DESN, runner, materializer,
  reference-cell, package-integration, boundary-audit, and mutation suites
  passed;
- the process-monitor fault suite passed all eight scenarios;
- roxygen output was generated twice with identical tracked bytes;
- a temporary source package installed and its complete installed native
  suite passed;
- `R CMD check --no-manual --no-vignettes` had no error or note;
- its two warnings were the expected consequence of deliberately not building
  the vignette in an environment without Pandoc;
- the vignette was evaluated directly with `knitr::knit()` successfully;
- the 20-page main manuscript and 25-page supplement built without undefined
  references, undefined citations, TeX warnings, or overfull boxes; and
- no package archive, PDF, TeX log, chain, fitted object, or simulation output
  is tracked.

The local package check used R 4.5.3 on
`x86_64-redhat-linux-gnu`, GCC/G++ 8.5.0, and package version
`rqrgibbs 0.1.0.9022`. Exact-commit runtime construction and reference-only
validation are intentionally separate pending gates.

## Protected DLM boundary

The read-only boundary auditor compared the candidate with
`ce02915f8e6270fb21c4cce1bdc231beeda12292` without checking out or executing
historical source. It covered 23 protected files, public function formals and
bodies, schema occurrences, NAMESPACE directives, compiled registration,
package metadata, and the existing confirmatory authorization state.

| Boundary | Total | Changed |
|---|---:|---:|
| Protected files | 23 | 5 |
| Public DLM functions | 18 | 6 |
| Schema occurrences | 29 | 3 |
| NAMESPACE directives | 56 | 10 |
| Compiled registrations | 8 | 0 |
| Package metadata fields | 19 | 7 |
| Existing DLM confirmatory authorization fields | 15 | 0 |

The changed protected files are `rqr_dlm_fit.R`, `rqr_utils.R`,
`rqr_numerics.R`, `DESCRIPTION`, and `NAMESPACE`. The changes harden fitted
objects, continuation, numerical/public boundaries, S3 interfaces, and package
wiring. The FFBS C++ implementation, compiled registration entries, evolution
constructors, DLM study configurations, and existing DLM confirmatory
authorization fields were not altered by this integration.

## Protected repositories and parallel work

All ordinary-v1 edits were made in an ignored isolated clone. The primary
checkout remained clean at `ce02915...` while the implementation was built and
tested. At reconciliation time no RQR/RQR-DLM process was active. The
implementation did not stop, restart, retarget, or write into any prior DLM
run directory.

The exdqlm and Q-DESN reference worktrees are clean at the exact commits shown
above. They were inspected with optional Git locks disabled and were not
mutated. Future DESN materialization must continue to use a Git archive and an
isolated runtime under ignored `application/cache/`; it must never load or
compile the protected exdqlm checkout directly.

## Remaining release ladder

The correct next sequence is:

1. build a fresh isolated `rqrgibbs` runtime from the exact candidate commit;
2. build and attest the pinned exdqlm runtime from its exact Git archive;
3. materialize the canonical D02 DESN design and verify its complete receipt;
4. run exact preflight and reference-only validation with zero scientific
   fits outside the small analytic/reference cells;
5. publish the still-disabled candidate and compact evidence for independent
   review;
6. run the four-chain BENCH01 representative cell from that disabled
   candidate;
7. obtain explicit independent authorization for a configuration-only
   enablement commit;
8. rebuild and re-attest the authorization commit and refresh reference-only
   evidence; and
9. run the 48-fit bounded mechanics/mixing grid sequentially, diagnosing each
   four-chain cell before the next.

A failure at any stage is a no-go for later stages. A successful 48-fit grid
would support ordinary-RQR implementation mechanics, numerical behavior,
continuation, provenance, and bounded mixing under the frozen fixtures. It
would not by itself establish coverage calibration, response-predictive
validity, forecasting superiority, or readiness for the later matched
simulation.
