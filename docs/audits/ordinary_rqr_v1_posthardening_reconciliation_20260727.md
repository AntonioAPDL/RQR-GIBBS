# Ordinary RQR version-1 post-hardening reconciliation

Date: 2026-07-27

Audited ordinary-v1 source commit:
`9d8b23d5f99ff9dc05567adb5323f08b7cc3e151`

Draft integration branch:
`codex/ordinary-rqr-v1-20260726`

Draft pull request:
`https://github.com/AntonioAPDL/RQR-GIBBS/pull/2`

Protected RQR-DLM base currently merged into the branch:
`ce02915f8e6270fb21c4cce1bdc231beeda12292`

Pinned exdqlm reference:
`dffb71ee70b597d6a716ee74be1cbc99731cd453`

Q-DESN article reference:
`f9f22804eff3871bb5350c8add04b7c9f4d4957b`

## Purpose and supersession

This record supersedes the source-status claims in
`ordinary_rqr_v1_implementation_reconciliation_20260726.md`, which remains a
historical record of candidate `bc482e5...`, package `0.1.0.9022`, 109 R
files, and the earlier eight-scenario monitor. It does not rewrite that
earlier evidence.

This reconciliation covers source completeness for ordinary, zero-tilt RQR.
It is not a release authorization, a completed bounded-validation claim, or
evidence of empirical coverage or forecasting performance.

## Executive decision

The ordinary zero-tilt source is complete for the declared version-1 model
family:

1. native fixed-design regression with ridge, full-Gaussian, and native
   Nishimura--Suchard shrunken-shoulder horseshoe coefficient priors;
2. frozen-design DESN readout inference that delegates to the same static
   Gibbs engine; and
3. the separately structured dynamic linear root-state model using
   alternating root-specific FFBS for its declared fixed-joint modes.

The update remains a generalized-Bayes loss update. The pseudo-AL variables
augment the RQR loss applied to the product residual; they do not define an
ordinary response likelihood. Root and interval draws are not
posterior-predictive response draws.

The source is not yet the final disabled release candidate. A concurrent
RQR-DLM recovery pass is still changing the protected DLM evidence boundary
on `main`. The ordinary branch must merge that final clean commit and rebuild
the protected companion before exact-runtime evidence or bounded execution.

## Findings closed after the earlier reconciliation

| Finding | Resolution at `9d8b23d...` |
|---|---|
| Artifact manifests accepted loose schemas or indirect roots | Persisted manifests now require one exact ordered schema, canonical unique paths, exact sizes and hashes, and nonsymlink roots. |
| Failed runs could retain ambiguous partial evidence | Each mode has a closed partial-failure allowlist and requires one valid nonempty failure row plus a matching failed run-status row. |
| Monitor evidence admitted undeclared files | The wrapper closes over exactly five pre-manifest files and one final manifest; ten deterministic fault scenarios include hidden and temporary-lookalike extras. |
| F01 lacked a native sampler gate | The deterministic collapsed quadrature is paired with a four-chain native learned-rate sampler oracle for six means and five corrected event probabilities. |
| Protected DLM evidence was not retained inside the ordinary bundle | A compact five-file companion collector and recursive validator were added for the pre-recovery DLM evidence schema. |
| Fixed-design malformed column names could survive fitting and fail later prediction | A design is now either unnamed or has complete, nonempty, unique column names; prediction preserves the same named/unnamed contract. |
| F03 described ridge/Gaussian equality without an end-to-end draw test | Both accepted rate modes now require bitwise equality of transition draws for ridge `tau2=4` and zero-mean Gaussian precision `0.25 I` under identical initial and RNG states. |
| Exact continuation evidence used only unit thinning | Both accepted rate modes now pass uninterrupted six-draw versus `2+2+2` continuation with `thin=2`, representing 12 raw transitions. |
| The implementation ledger omitted full Gaussian regression | A distinct full-Gaussian static row now records the implemented source and its bounded evidence scope. |
| Bounded CI omitted the F01 quadrature dependency | `pracma` is explicitly installed in the bounded GitHub job. |

## Exact validation at the audited source commit

GitHub Actions run `30246484885` executed from
`9d8b23d5f99ff9dc05567adb5323f08b7cc3e151` under R 4.5.3 on Ubuntu 24.04.
Both jobs passed:

- bounded native checks, job `89914610944`;
- Pandoc-enabled roxygen drift and `R CMD check`, job `89914610809`.

The remote check regenerated package interfaces, rejected tracked
documentation drift, built the source package with vignettes, and completed
the source-package check. The bounded job installed a fresh package namespace
and ran the ordinary static, Gaussian, RHS-NS, DESN, F01, materializer,
protected-companion, runner, boundary, monitor, and package-integration
checks.

Local focused checks additionally used a fresh isolated package installation
for the fixed-design boundary round trip and a source-loaded run of the
complete reference-cell suite. Both passed. The branch contains 114 tracked R
files, package version `0.1.0.9023`, a frozen 48-fit plan, 82 unique seed-ledger
rows, and `ordinary_v1_execute_enabled=FALSE`.

The exact gate inventory is recorded in
`ordinary_rqr_v1_posthardening_evidence_20260727/validation_matrix.csv`.

## Independent traceability verdict

A separate read-only claim-to-test audit found no remaining ordinary-v1
model, prior, export, or API blocker after the post-hardening corrections.
The audit verified:

- one and only one fixed-design transition engine;
- ridge, arbitrary proper full Gaussian, sampled-shoulder RHS-NS, and
  fixed-shoulder RHS-NS prior paths;
- fixed and normalized learned generalized-Bayes rates;
- observed-mask missingness;
- exact static and DESN continuation, including non-unit thinning;
- DESN delegation without a duplicate sampler;
- strict draw, prediction, future-root, checkpoint, and provenance
  boundaries; and
- generated Rd/S3 wiring plus an executable model-family vignette.

The executable `learned_pure` compatibility mode remains diagnostic,
noncontinuable, and nonpromotable. Nonzero mean tilt, CAVI/ELBO, empirical
learning-rate calibration, response simulation, and the matched simulation
remain outside this source claim.

## Concurrent DLM integration boundary

The primary checkout remains owned by the active RQR-DLM recovery task. The
ordinary work used only the ignored isolated clone and did not edit, stop, or
retarget that run.

A read-only three-way integration audit predicts only two textual conflict
regions after the DLM work lands:

- the `Makefile` `.PHONY` union; and
- the package `Version:` line.

The important reconciliation is semantic rather than textual. The current
ordinary companion covers four roles and 39 input artifacts. The recovered
DLM workflow requires seven roles:

1. DLM reference;
2. M01 wave 1;
3. M02 wave 1;
4. M01 wave 2;
5. M02 wave 2;
6. horizon/M03; and
7. resource envelope.

Under the current closed DLM contracts, these comprise 55 input artifacts.
The companion may retain five compact outputs, but its schema, semantic
checks, counts, collector hash, tests, documentation, and authorization gate
must advance atomically. The protected inventory must include recovery
validators 22, 23, and 25 and the confirmatory helper library. Package version
`0.1.0.9024` or newer must identify the integrated state.

Development recovery outputs are not reusable promotion evidence because
they are generated from a dirty source state and lack final runtime
attestations.

## Protected repositories

The pinned exdqlm and Q-DESN reference repositories were rechecked read-only.
Both are clean at the commits recorded above; neither was mutated. Any future
exdqlm runtime must still be materialized with `git archive` and built only
under ignored `application/cache/`.

## Remaining release ladder

The next steps are deliberately ordered:

1. wait for the DLM recovery to finish, commit, push, and leave a clean
   primary checkout;
2. merge that exact commit into the ordinary branch and preserve both target
   implementations;
3. advance the package version and seven-role protected companion;
4. rerun the complete ordinary and DLM native suites, standalone contracts,
   documentation drift, package build/check, smoke, literature manifest, and
   both TeX builds;
5. build exact isolated `rqrgibbs` and pinned exdqlm runtimes from Git
   archives;
6. regenerate D02, reference-only evidence, all protected DLM gates, and the
   compact companion;
7. run BENCH01 from the still-disabled candidate;
8. obtain independent review of that exact candidate and evidence;
9. make only the reviewed configuration authorization change; and
10. run the 48 bounded fits sequentially with a four-chain cell stop rule.

A successful bounded grid will validate mechanics, numerical behavior,
continuation, provenance, and mixing on the frozen fixtures. It will not by
itself establish empirical calibration or comparative performance.
