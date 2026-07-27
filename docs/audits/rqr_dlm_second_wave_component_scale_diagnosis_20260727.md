# RQR-DLM second-wave component-scale diagnosis

Date: 2026-07-27

## Decision

The 6,000-retained ASIS-only component-scale schedule is rejected for the main
study. The development gate for the complete second canonical wave completed
all 49 chains and remained within the declared resource margin, but it failed
the frozen diagnostic contract. This is a transition-kernel finding. It is not
a comparative scientific result, and no fit, draw, endpoint estimate, or
metric from this gate is eligible for the confirmatory run.

The confirmatory launch remains closed. The prospective correction is an exact
one-root partially collapsed scale transition composed with the existing
root-specific FFBS and centered--noncentered ASIS transitions. Priors,
generalized-Bayes targets, learning rates, DGPs, seeds, diagnostic thresholds,
and fixed schedules are unchanged.

A subsequent development-only comparison selected three coordinate slice
sweeps before the exact-source gates. On the diagnosed S03 replication 28,
four overdispersed chains with 1,500 retained draws each gave
\(\widehat R=1.0014\), bulk ESS 405.4, tail ESS 892.6, and MCSE/SD 0.0497
for \(\log q_1\), with all other checked functionals passing. Two sweeps
narrowly missed bulk ESS at 381.0, and six sweeps failed. This prospective
choice is not promotion evidence; the complete two-wave gates remain required.

## Authenticated development evidence

| Item | Value |
|---|---|
| Source commit recorded by gate | `ce02915f8e6270fb21c4cce1bdc231beeda12292` |
| Source clean | no; development evidence only |
| Package version recorded by gate | `0.1.0.9022` |
| Wave | `local_level_gaussian_T200__target0200__sentinel` |
| Tasks | 25 |
| Chain jobs | 49 |
| Completed fits | 49 |
| Passed diagnostics | 1,131 / 1,150 |
| Tasks passing all diagnostics | 13 / 25 |
| Tasks failing at least one diagnostic | 12 / 25 |
| Total fit CPU-equivalent elapsed time | 50,108.984 seconds |
| Maximum observed process peak RSS | 699,236 KiB |
| Declared per-worker ceiling | 1,572,864 KiB |
| Resource-margin decision | pass |
| Scientific-output reuse | prohibited |

The ignored evidence root is:

```text
application/cache/rqr_dlm_wave2_m01_correction_dev3_20260727
```

Its compact authentication values are:

| File | SHA-256 |
|---|---|
| `artifact_hashes.csv` | `1531ad23ed627c8d50637a30d4b6e4e7fb8bafd89b42329902912cd7b56c4b8f` |
| `validation_manifest.json` | `5f5b1178956c584e80014cf99b6bc5499527add6924093515f23d01a780ae2f9` |
| `wave2_M01_summary.csv` | `785ba127ada16367d8f37c9322c5948ec2d14ec88f886ab30457ef329b1fd926` |
| `wave2_M01_diagnostics.csv` | `71cc5e0cc306868d6d0a381218572f830311469e7e1179382ce629b049ac2182` |

## Failure anatomy

Of the 19 failed diagnostic rows, 12 were `log_q_1`, two were observed-data
loss, and five were ordered-root summaries in two S03 replications. The
ordered-root failures coincided with the worst scale mixing rather than with a
numerical repair or fit exception.

The most informative embedded sentinel was S03 replication 28:

| Estimand | R-hat | Bulk ESS | Tail ESS | MCSE / SD |
|---|---:|---:|---:|---:|
| `log_q_1` | 1.0342 | 38.0 | 33.6 | 0.1868 |
| mean lower endpoint | 1.0166 | 74.7 | 107.2 | 0.1177 |
| mean width | 1.0202 | 61.5 | 75.4 | 0.1336 |

Three scale chains were stationary around the same region, with lag-one
autocorrelation near 0.93. The deliberately overdispersed profile-B chain moved
between distinct regions during the retained window; its four consecutive
1,500-draw means were approximately -3.12, -3.32, -3.87, and -3.91. This
explains both the poor multi-chain diagnostics and the associated endpoint
failures. Across difficult one-chain tasks, lag-one scale autocorrelation
reached approximately 0.98 and bulk ESS fell as low as 63.4 among the reported
failures.

The result rules out the premise that matching the sentinel role to the
6,000-draw standard role is sufficient. It does not invalidate the
component-scale generalized posterior or the ASIS derivation.

## Comparator boundary

The corresponding complete M02 development gate passed all 1,125
diagnostics across 49 interval chains and 98 endpoint fits. It verified one
common target across the four initialization profiles, four distinct
target-preserving warm-start digests, and the corrected singleton projection.
Its maximum process peak RSS was 850,032 KiB. This result isolates the
remaining blocker to M01 component-scale mixing; it does not authorize either
method because the source was dirty and unattested.

The ignored M02 evidence root is:

```text
application/cache/rqr_dlm_wave2_m02_correction_dev8_20260727
```

The SHA-256 of its `artifact_hashes.csv` is
`398ee3f12082a6d85bdaaf2001a8a80231d0062d06dacdb7b98437f2b597a3e1`.

## Selected prospective transition

For one conditioned root, the scale log kernel integrates the other root by a
Gaussian Kalman filter. If root 2 is conditioned on and root 1 is integrated,
then, up to a constant,

\[
\log \pi(\boldsymbol x\mid\cdot)=
\ell_1(\exp\boldsymbol x)
-\sum_j\left[
\left(a_j+\frac{Td_j}{2}\right)x_j+
(b_j+E_{2j})e^{-x_j}
\right],
\qquad x_j=\log q_j.
\]

Here \(\ell_1\) is the root-1 Gaussian pseudo-observation log marginal and
\(E_{2j}\) is half the conditioned root's component innovation energy. A
coordinate slice transition targets this marginal scale kernel. Root 1 and
its time-zero state are then redrawn conditionally, followed by root 2, the
existing ASIS transition, and the global label swap.

This is a partially collapsed augmented generalized-Bayes transition, not a
response-likelihood calculation. The C++ filter is deterministic and is
cross-checked against an R recursion and an independently assembled dense
Gaussian marginal. The transition and its controls are included in the
checkpoint digest so continuation cannot silently change kernels.

## Alternatives rejected

| Alternative | Reason for rejection |
|---|---|
| increase only failed chains | diagnostic-dependent computation violates the no-extension contract |
| increase every M01 chain again | treats a scale--trajectory dependence as a chain-length problem and materially expands an already large budget |
| weaken ESS, R-hat, or MCSE gates | converts a computational failure into an unsupported pass |
| reuse the 13 passing development tasks | mixes source states and conditions inclusion on realized diagnostics |
| modify exdqlm | unrelated to the native M01 transition and outside protected scope |
| proceed to later waves | violates cross-wave fail-closed stopping |

## Required authorization evidence

Before a new main launch, all of the following must pass at one clean exact
implementation commit and its isolated runtime:

1. dense-Gaussian equality for the R and C++ log marginals;
2. direct equality of the collapsed log kernel to the independently assembled
   target, up to an additive constant;
3. byte-identical uninterrupted and continued chains;
4. complete first- and second-wave M01 gates under the fixed schedule;
5. complete first- and second-wave M02 gates through the isolated CRAN 1.1.0
   runtime;
6. every frozen R-hat, bulk ESS, tail ESS, and MCSE threshold;
7. zero numerical repairs and exact-target status;
8. representative timing, process-tree, memory, and storage gates;
9. native and contract tests, `R CMD check`, smoke checks, and both TeX builds;
10. unchanged read-only guards for the pinned exdqlm and Q-DESN repositories.

Only after these gates pass may a flag-only authorization commit and a fresh
run root be created.
