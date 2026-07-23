# ChatGPT Pro Output-8 reconciliation

Date: 2026-07-23
Implementation commit: `ec0ed5f57772fbc5d26a3aae1e19943db41b2194`
Package: `rqrgibbs 0.1.0.9008`
Fit schema: `rqrgibbs_fit/1.6.0`
Continuation schema: `rqrgibbs_continuation_history/3.0.0`
Runtime-attestation schema: `rqrgibbs_runtime_attestation/4.0.0`
Bounded-fixture schema: `rqrgibbs_dlm_bounded_fixtures/3.0.0`

## Executive decision

Output-8 was treated as a review whose claims required source inspection,
independent reproduction, and tests. It was not treated as executable
authority. The principal residual findings were reproducible:

1. the version-3 generic runtime verifier could accept an archive, alleged
   source package, and installed runtime without a sufficiently constrained
   local build/install lineage;
2. version-2 continuation history reconstructed several recursions among
   asserted booleans rather than deriving every status from raw per-segment
   facts;
3. the pilot and prospective dynamic runner needed maintained mean MCSE;
4. the dynamic protocol needed explicit starts, state storage, seeds,
   continuation semantics, resource enforcement, and an actual reviewed
   runner.

All four areas were corrected. The resulting source, isolated runtime,
canonical preflight, and 14 reference-only gates pass. The 24-fit execution
path remains deliberately disabled and was tested to fail closed.

```text
Corrected intercept-only CDF references:       PASS; no patch required
Runtime lineage version 4:                     PASS for controlled local lineage
Continuation history version 3:                PASS for structural/semantic integrity
Three-fixture construction:                    PASS
Reference-only dynamic gates:                  PASS, 14/14
Execute the 24 bounded fits:                    NO-GO pending independent review
Matched or production RQR-DLM simulation:      NO-GO
CAVI/ELBO:                                     DEFER
RQR-DESN:                                      DEFER
```

The pass is a computational and target-contract result for interval-root
generalized Bayes. It is not evidence of empirical coverage calibration and
does not define posterior-predictive response draws.

## Input integrity and protected repositories

The four supplied Output-8 files are exported chat/project wrappers rather
than the standalone downloads whose SHA-256 values appeared in the Pro
response. Their observed hashes are:

| Local input | Observed SHA-256 | Pro-reported SHA-256 |
|---|---|---|
| `chatgpt_pro_output8_audit_20260723.md` | `d17f69e7e203293d6a6d428e3c3f14710988be4149383020cb4b52ef343988ec` | `5b506f7cc8c50a6163faecfc6217f784d11bf2959c7d5a23f9c181bad57dd0a9` |
| `chatgpt_pro_output8_codex_handoff_20260723.md` | `520b3895c878f6ed7b730003bf56ddbef9a928bd5c4c4ff9dc05a01b83702a64` | `f27e020cb29008b443e1072cf5b2eb61caaec646d3f487024da534490da49d12` |
| `output8_independent_cdf_check.csv` | `ce53afcc46c79c3656145686af1bfffae314937484825b3287aaed272442ece8` | `258ad64e16e88dfb92c6b5b21aee91ae8a0b70b3948da53f7d7f26c1ec6dc187` |
| `chatgpt_pro_output8_artifact_hashes.csv` | `e9f253a15a2cd71d0e0b157c8239431e583a9a25badb4cf725365bd6128f0335` | `e4a1ecad5faf6eff736079c2d55c387ee41dac7363ba8f345de78192207cc2bd` |

The hash mismatch was not silently waived. The exports remain ignored, and
their substantive claims were checked against repository source and
independent executions. Output-8's CDF values agree with the already tracked
event-boundary-aware references to the displayed precision, so no CDF source
change was justified.

Only RQR-GIBBS was modified. The protected repositories finished clean at:

| Repository | Branch | Commit |
|---|---|---|
| exdqlm pinned reference | `feature/rqr-desn-readout-20260716` | `dffb71ee70b597d6a716ee74be1cbc99731cd453` |
| Q-DESN article reference | `main` | `f9f22804eff3871bb5350c8add04b7c9f4d4957b` |

exdqlm was archived read-only and built only under RQR-GIBBS's ignored cache.
Its full before/after guard was identical:
`0b24b9b9136935d47510fc4fa10c324514918f69a44d6d54bb9f126cf22f332c`.
Nothing was compiled, installed, or loaded from the exdqlm checkout.

## Finding disposition

| Output-8 finding | Disposition | Correction or evidence |
|---|---|---|
| Corrected CDF mathematics | Confirmed pass | The tracked event-boundary generator and values remain correct; no change made. |
| Primary isolated-runtime rule | Confirmed pass | An expected commit requires an exact isolated attestation; direct `pkgload` execution is not promotion eligible. |
| Runtime lineage v3 was partial | Corrected | Version 4 binds the verified Git archive, built source contents, command receipts, logs, install input, runtime marker, and executing tree. |
| Continuation history v2 was partial | Corrected | Version 3 records raw segment facts and reconstructs every numerical, target, environment, backend, cumulative, and promotion status. |
| Canonical fixtures | Confirmed pass | The shared constructor remains authoritative; all three training/future objects pass public validation. |
| Modern R-hat and ESS | Confirmed pass | `posterior 1.7.0` remains the maintained provider. |
| MCSE patch required | Corrected | Continuous and indicator mean MCSE use `posterior::mcse_mean()`; fixed-rate lambda uses exact identity. |
| Resource claims | Corrected and enforced | A wrapper monitors the process group and terminates on timeout or process/thread/RSS ceiling breach. |
| Stale protocol wording | Corrected | Manuscript, supplement, README, and bounded-pilot protocol state the actual isolated-runtime and resource contracts. |
| Dynamic runner absent | Corrected | A three-mode runner implements preflight, reference-only, and separately gated execution. |

## Runtime lineage version 4

Both controlled builders now:

- create an exact Git archive outside the protected source;
- compare Git mode--blob--path entries with extracted archive entries;
- build from the extracted archive and preserve the actual source package;
- compare built package contents with the verified source, allowing only
  documented `R CMD build` DESCRIPTION transformations and text
  normalization;
- record and rehash exact build/install commands, working directories, input
  paths, standard-output logs, and standard-error logs;
- install only that source package into a disjoint library;
- add an installed runtime marker binding the source-package hash, built
  manifest, and install receipt; and
- hash the executing runtime with file kind, permissions or executable mode,
  symlink target, relative path, and content.

The native test now performs a real miniature Git archive, `R CMD build`,
isolated `R CMD INSTALL`, and loaded-runtime positive test. A mixed-lineage
archive-A/source-package-B/runtime-C construction is rejected.

The exact primary runtime used for validation records:

```text
source commit:
  ec0ed5f57772fbc5d26a3aae1e19943db41b2194
application tree:
  afbf672257c990add2002d1b937c0039e411c46d
source archive SHA-256:
  3eb73821ee9207ce6eeea847c27f0352b947a1ccdae38417f222d7e7545f1c99
source package SHA-256:
  5a76afbf679913288134c10bed421a058bcc47b15e043a06cc3a7a00f2514c8d
built-source manifest SHA-256:
  7d6c53dcdace23446e09c75b3bf180e62185d67a90465442d66265b9fa5e24c7
installed runtime-tree SHA-256:
  49169e5c49f54dfca4d8336aa6b4a4aa80c69e34171577b8d384214905c515dc
installation receipt SHA-256:
  80698628cc92e306251485e3a14e86192343984dac1d921d66aee24def8b8fbc
```

This is a controlled local-build integrity contract. It does not claim
cryptographic authenticity or bit-for-bit reproducible binaries across
compilers and platforms.

## Continuation history version 3

Every segment stores raw repair count, exact-joint-target status,
target-contract digest, environment-base eligibility, mismatches, override,
requested and resolved backends, and parent checkpoint/backend links. The
validator derives:

- segment numerical exactness from zero repairs;
- target numerical eligibility from exact target and zero repairs;
- environment eligibility from base state, mismatch set, and override;
- backend-change status and its required mismatch/override;
- cumulative repairs, exactness, target eligibility, environment
  reproducibility, promotion, and override state; and
- the complete mismatch ledger.

All target digests must agree with the fitted target object, every segment's
exact-target fact must agree with the fitted model, and the final reconstructed
values must agree with checkpoint, provenance, and model metadata.

Three-generation tests mutate generation zero or one, recompute the ordinary
history digest and relevant redundant values, and still require rejection.
The reference runner additionally rejects five recomputed-digest mutations
covering repair/exactness, target status, mismatch/override, backend, and
target digest. This is a consistency and accidental-corruption contract, not a
keyed signature against an actor able to rewrite an entire fit object.

## Frozen runner contract

`application/scripts/08_run_rqr_dlm_bounded_validation.sh` sets thread
controls before R starts, isolates the R process group, samples the full
descendant tree every 0.2 seconds, and enforces:

```text
wall-clock timeout:       45 minutes
maximum summed RSS:       4 GiB
maximum threads:          4
maximum processes:        3
execution:                sequential
```

The R entry point has three modes:

- `preflight`: construct and hash the three canonical fixtures;
- `reference-only`: run small dense, analytic, continuation, and process
  monitor gates;
- `execute-bounded`: the prospective `3 x 2 x 4 = 24` fits.

Execution requires all of the following: a full reviewed primary SHA, the
exact confirmation phrase, a passing reference manifest and passing resource
summary from the same directory with separately supplied SHA-256 values, an
active monitor, and a reviewed config whose execution flag is true. The
committed flag is false.

The frozen MCMC contract now also specifies four starts, four chain seeds,
reference and forecast seeds, state-path storage, no latent-path storage,
three continuation segments with generation indices 0, 1, and 2, maintained
MCSE, and exact fixed-rate-lambda identity.

## Preflight and reference-only results

Preflight constructed:

| Fixture | Dimension | Observed/missing | Future | Evolution |
|---|---:|---:|---:|---|
| fixed-W local level | 1 | 22/2 | 4 | fixed `W` |
| trend plus seasonal | 5 | 36/0 | 4 | frozen component-discount template |
| trend plus regression | 3 | 29/1 | 3 | shared component scales |

Reference-only validation passed 14 of 14 gates. The complete compact table is
`docs/audits/rqr_dlm_bounded_reference_gates_20260723.csv`. Selected results:

| Gate | Result |
|---|---:|
| Dense conditional mean maximum error | `1.11e-15` |
| Dense conditional covariance maximum error | `7.77e-16` |
| R/C++ smoother maximum difference | `0` |
| 1,500-path FFBS mean maximum standardized error | `0.935` |
| Future-state mean maximum standardized error | `0.760` |
| Future-state covariance standardized error | `2.173` |
| Component-scale inverse-Gamma shape | `6` exactly |
| Component-scale inverse-Gamma rate error | `0` |
| Full six draws versus `2+2+2` | bitwise identical |
| Recomputed-digest history mutations rejected | `5/5` |

The monitored reference run used at most 3 processes, 4 threads, and 162,012
KiB summed RSS. It did not trigger the 45-minute timeout or any resource
ceiling. Compact ignored artifacts and their hashes are recorded in
`docs/audits/rqr_dlm_bounded_reference_artifact_hashes_20260723.csv`.

The execute-mode negative test returned status 1 and wrote
`status=blocked_by_execution_contract`. Zero members of the 24-fit grid ran.

## Validation matrix

The following completed successfully:

- parse checks and `git diff --check`;
- environment smoke;
- package installation and the complete native R/C++ suite;
- real-build positive and mixed-lineage negative runtime tests;
- three-generation continuation semantic-corruption tests;
- `R CMD check --no-manual` with `Status: OK`;
- main manuscript and supplement PDF builds;
- literature manifest regeneration for 18 local PDFs;
- version-4 exdqlm isolated-runtime build and focused RQR tests;
- protected exdqlm before/after source guard;
- version-4 primary isolated-runtime build;
- monitored canonical preflight;
- monitored 14-gate reference-only validation; and
- fail-closed execute-mode negative test.

No heavy object, package archive, compiled object, PDF, TeX log, or simulation
output is tracked.

## Remaining work

The source is ready for an independent review of the implementation commit and
the reference artifacts. The next decision is whether to enable only the
bounded 24-fit validation. That decision should not be conflated with approval
for the matched simulation.

If the independent review grants a go, create a new reviewed commit changing
only the explicit bounded-execution authorization and any review-mandated
fixes, rebuild the exact isolated runtime, rerun preflight/reference-only, and
then execute the 24 fits sequentially under the same monitor. Any failed
mixing, repair, source, continuation, forecast, or resource gate remains a
scientific failure to diagnose rather than a reason to weaken thresholds.

After the bounded fits pass, freeze the matched RQR-DLM simulation protocol,
then run the matched study. CAVI/ELBO derivation and implementation follow the
stable MCMC contract. RQR-DESN remains deferred until the RQR-DLM work is
complete.
