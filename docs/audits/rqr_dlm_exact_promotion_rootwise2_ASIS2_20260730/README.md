# RQR-DLM rootwise2-ASIS2 exact-promotion closeout

Date: 2026-07-30 UTC

## Decision

The exact-target `rootwise2_ASIS2` transition is eligible for a separate
flag-only authorization commit and a fresh confirmatory launch.

This decision combines:

1. the complete heavy promotion run at
   `38c008030d888593701c3769dbad3cdf53bd84b5`;
2. exact Git-object identity for the package implementation, DLM
   configuration, confirmatory library, and promotion validators through
   fail-closed closeout commit
   `a9eaadcbf3ac5bc584c2124e04aad90438158fe8`; and
3. a fresh hermetic post-computation matrix under an isolated runtime built
   from `a9eaadcbf3ac5bc584c2124e04aad90438158fe8`.

Both execution flags were false while this evidence was produced. Failed,
development, and promotion artifacts are not confirmatory outputs and do not
reduce the planned main-study work.

## Scientific scope

The selected transition repeats the symmetric rootwise partially collapsed
composition twice and then performs two centered/noncentered ASIS cycles.
These are exact target-preserving transition changes. They do not alter the
RQR generalized-Bayes loss, priors, learning-rate modes, data-generating
mechanisms, seeds, schedules, estimands, diagnostic thresholds, or method
incidence matrix.

The evidence validates computation and mixing under the frozen promotion
contract. It does not turn the generalized update into a response likelihood,
does not make interval-root draws posterior-predictive response draws, and is
not itself evidence of empirical coverage or forecasting performance.

## Complete heavy-gate result

| Gate | Tasks | Diagnostics | Result | Minimum bulk ESS | Minimum tail ESS | Maximum MCSE/SD | Maximum R-hat |
|---|---:|---:|---:|---:|---:|---:|---:|
| M01 static Gaussian | 20 | 920 | 920/920 | 498.75 | 876.70 | 0.04481 | 1.00218 |
| M01 local-level Gaussian | 25 | 1,150 | 1,150/1,150 | 290.33 | 608.20 | 0.05875 | 1.00142 |
| M02 static Gaussian | 20 | 900 | 900/900 | 244.88 | 503.71 | 0.06380 | 1.00271 |
| M02 local-level Gaussian | 25 | 1,125 | 1,125/1,125 | 230.56 | 445.54 | 0.06790 | 1.00296 |
| Fixed-design RQR | 8 | 328 | 328/328 | 316.19 | 572.91 | 0.05738 | not applicable |

All 4,423 diagnostic rows passed without threshold relaxation, selective
extension, or seed replacement. The horizon suite passed 16 of 16 scenarios,
and the dynamic endpoint contract passed. All fits succeeded, were exact
target and reproducibility eligible, and recorded no promotion-blocking
numerical repair. The largest observed wave-level process peak was 860,676
KiB, below the declared 1,572,864 KiB ceiling. The serialization-envelope
fixtures also passed.

The M01 local-level diagnostics reproduce the retained development evidence
byte for byte:

```text
ab16e5245995814874b34d8a0e577e2fd8e66ce7c2a57aa35b31a6a6851dea58
```

## Post-gate failure and correction

The original supervisor completed every heavy gate before a later generic
package-test target failed. That target installed the package into the first
library on `R_LIBS`, which was the already-attested primary runtime. The
runtime digest changed from

```text
6b66e2e41617a496e141c4d46230bb4c462a69a13469393eca2a350c02081c6e
```

to

```text
c2a42702ac7efcce8f2f43f121105477061dad17da4a855c40a4a74ae8995573
```

and compilation left ignored objects in `application/src`. Runtime binding
then failed closed, as designed. The one reported test failure concerned the
expected error-message branch, not the sampler or any heavy-gate result.

The correction:

- blocks installation into an attested runtime unless a disjoint library is
  supplied;
- clears exact-runtime variables explicitly in the child failure-path test;
- installs package checks into a fresh ignored library;
- resolves the protected exdqlm checkout correctly from linked worktrees;
- builds document targets from an exact isolated Git archive; and
- verifies the attested runtime digest, source cleanliness, and absence of
  compiler artifacts after all checks.

The corrected post-computation matrix passed all 13 stages. Its runtime digest
was identical before and after:

```text
c3afe4e376cd2c5c06a8b66a0b0d6a3ca4e13eaff20f32d0302b0d3c67936387
```

The primary attestation uses schema `rqrgibbs_runtime_attestation/5.0.0`,
R 4.5.3, and platform `x86_64-redhat-linux-gnu`. The package check reported
`0 errors | 0 warnings | 0 notes`.

## Source-drift disposition

The heavy implementation and closeout commits have identical Git objects for:

- `application/R`;
- `application/src`;
- `application/DESCRIPTION`;
- `application/NAMESPACE`;
- `application/config/rqr_dlm`;
- `application/scripts/lib/rqr_dlm_confirmatory_simulation.R`; and
- all four promotion validators.

Intervening changes are confined to paper/oracle illustrations and their
tests, documentation, the controlled failure-path assertion, and
post-computation build/provenance infrastructure. The installed-runtime
digest is not asserted to be bitwise reproducible across separate builds:
the source archive, source package, installation metadata, and lineage marker
bind each build to its own commit. Instead, transfer of the heavy evidence is
based on the exact DLM object identities above and a fresh complete native,
contract, package, and source-integrity validation at the closeout commit.

## Evidence integrity

All 27 files listed by the six original per-gate artifact manifests were
independently rehashed from local bytes and passed. All 856 files in the
hermetic post-check manifest were likewise independently rehashed and passed.
The compact copies in this directory exclude chain evidence and fitted model
objects; those remain under ignored cache.

The local heavy evidence root is:

```text
application/cache/rqr_dlm_exact_promotion_38c0080_20260729_20260729T233057Z
```

The local hermetic-check root is:

```text
application/cache/rqr_dlm_promotion_checks_a9eaadc_20260730
```

`artifact_hashes.csv` authenticates every compact tracked closeout file other
than itself.

## Launch boundary

The next commit may change only
`confirmatory_execution_authorized = FALSE` to `TRUE`. After that commit, the
workflow must build a fresh exact runtime, regenerate preflight and
oracle-reference evidence, prepare the authorization bundle using the
explicit confirmation token, and launch under a new append-only run ID.

Any failure in that sequence remains launch-blocking. The matched simulation
is the next stage; CAVI/ELBO and RQR-DESN remain outside this launch.
