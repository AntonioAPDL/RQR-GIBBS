# RQR model-family architecture review reconciliation

Date: 2026-07-26

> **Historical-scope note.** This reconciliation records the model-family
> architecture pass exactly as implemented at `55799cbf...`, including its
> then-current \(\tau_{\mathrm{AL}}=0.99\) illustrative law. The subsequent
> AL-illustration revision replaces that benchmark with
> \(\operatorname{AL}_{0.8}(0,1)\), treats \(\tau_{\mathrm{AL}}\) and interval
> content \(c\) as independent inputs, and is documented in
> `docs/audits/rqr_al08_illustration_revision_20260726.md`. The historical
> values below are retained for provenance and are not the current manuscript
> specification.

## Scope and source identity

This pass independently audited the ChatGPT Pro model-family architecture
packet and implemented only article-facing changes. Work was performed in the
isolated worktree
`.codex_work/rqr_model_family_review_20260726` from base commit
`ae13441ca52fa3ab5f20e43aa4c63fa6330e44f4`. The scientific implementation
commit is:

```text
55799cbf1a37544bef5d82e547900019d01af5e9
```

The primary checkout remained on the source state used by the active
confirmatory simulation, `bb966299bb298ee31ec65d167edf53c44ce48b03`. No
tracked or ignored simulation artifact was read as manuscript evidence, and no
`application/` source, configuration, test, runtime, run, log, cache, or output
file was changed.

The protected reference repositories were read only:

```text
exdqlm:
  branch feature/rqr-desn-readout-20260716
  commit dffb71ee70b597d6a716ee74be1cbc99731cd453

Article-Q-DESN---Version-2:
  branch main
  commit f9f22804eff3871bb5350c8add04b7c9f4d4957b
```

Pre-existing ignored compiled objects in the exdqlm checkout were left
untouched. No package was compiled, installed, or loaded from that checkout.

## Review-bundle verification

The uploaded archive was:

```text
application/cache/chatgpt_pro_handoffs/
  chatgpt_pro_rqr_model_family_architecture_review_bundle_20260726.zip

bytes: 71120
SHA-256: 2a24f776b80e1b58199160debf5697bf941209397325fa5953b2c5f62d266591
members: 11
```

`unzip -t` passed. All member names were normalized before extraction. The
archive contained no absolute paths, traversal components, duplicate
normalized paths, symlinks, executables, nested archives, or heavy artifacts.
It was extracted below the ignored handoff cache. The ten review deliverables
and the nonrecursive `artifact_hashes.csv` were the complete member set.
Recomputed byte counts and SHA-256 values matched every manifest row. Every
deliverable was read in full before implementation.

## Independent disposition

The complete row-level ledger is
`docs/audits/rqr_model_family_architecture_review_disposition_20260726.csv`.
It contains 40 unique findings:

| Disposition | Count |
|---|---:|
| accept | 24 |
| accept with modification | 10 |
| defer | 3 |
| reject | 3 |

Important independent corrections to the packet were:

- the two root trajectories may be stacked for prior notation, but their
  augmented observation term is quartic jointly; the exact fixed-joint Gibbs
  construction therefore alternates root-specific Gaussian FFBS draws;
- the dynamic tilt is a canonical information-vector shift and is stated
  without division by the other root's observation coefficient;
- the noncentered shared-component-scale tilt term uses the generic
  \(V_t=\phi_c\sigma_{\mathrm R}v_t\); the current random-scale ASIS kernel is
  explicitly ordinary-RQR only and rejects nonzero tilt;
- the normalized learned-\(\kappa\) convention remains ordinary-only and
  preserves the mandatory collapsed-\(\kappa\), latent-refresh, root-1,
  root-2 order;
- exact fixed \(W_t\), exact frozen templates, exact ordinary shared component
  scales, and adaptive working discounts remain distinct;
- root-label swaps require matched prior, initial-state, evolution, and
  hyperstate specifications;
- finite-root interior targets are separated from qualified support-boundary
  infima;
- an unsupported regularized-horseshoe hierarchy was not invented;
- the existing complete convex-order proof was retained rather than removed;
  and
- the dependency-light algorithm environment was retained instead of adding
  unnecessary algorithm packages.

Production nonzero-tilt APIs, their numerical validation, learned-rate tilted
normalization, nonzero-tilt random-scale ASIS, a fully evidenced
regularized-horseshoe hierarchy, and CAVI/ELBO work remain separate future
tasks.

## Architecture and algorithm decisions

The article now has one target-to-computation hierarchy:

1. fixed-content interval functionals;
2. ordinary RQR as the established zero-tilt foundation;
3. mean-tilted RQR as the umbrella fixed-target family;
4. fixed-design generalized-Bayes computation;
5. MT-RQR-DLM as the principal structured extension;
6. frozen-feature MT-RQR-DESN as a fixed-design readout specialization;
7. evaluation and evidence scope; and
8. discussion.

The compact model-family table separates target, predictor architecture,
rate, conditional status, and implementation evidence. The nonduplicative
algorithm set is:

- Algorithm 1: fixed-rate MT-RQR fixed-design scan, with ordinary RQR recovered
  exactly at zero tilt;
- Supplement Algorithm S1: the separate ordinary learned-\(\kappa\) partially
  collapsed scan; and
- Algorithm 2: fixed-rate, root-blocked MT-RQR-DLM for fixed \(W_t\) or a
  frozen evolution template.

The DESN section supplies only the frozen-feature and prior-adapter contract
needed to reuse Algorithm 1. It does not imply posterior reservoir-state
inference or a response-simulation distribution.

Throughout both documents, the pseudo-asymmetric-Laplace construction is an
augmentation of an exponentiated loss in a pseudo-residual. It is not an
ordinary response likelihood. Ordered root draws, widths, and midpoints are
interval-root functionals, not posterior-predictive response draws.

## Figure decisions

The former main-text computational Figure 3 and its TikZ source were removed.
Its useful content is now conveyed more precisely by the model-family/status
table and the two main algorithms. The mathematically distinct supplementary
blocked-state schematic was retained because it explains the quartic
obstruction to a simultaneous Gaussian two-root FFBS draw.

Figures 1, 2, and S1 now use deterministic population calculations for the
quantile-regression asymmetric-Laplace convention

```text
mu_AL = 0
s_AL = 1
tau_AL = 0.99
c = 0.80
```

This law is strongly left-skewed; its raw location is its 0.99 quantile.
Targets are computed on the raw population and only then displayed under
mean/standard-deviation standardization. Analytic moments used by the oracle
checks are:

```text
mean = -98.989898989899
variance = 10001.020304050607
standard deviation = 100.005101390132
```

All ET, RQR, and SH interval segments are solid. Target identity is redundantly
encoded by direct labels, vertical order, glyphs (ET square, RQR circle, SH
triangle), and color. The generator, oracle tests, README, captions, assets,
and provenance receipt were updated together. The release receipt is bound to
the clean implementation commit above and records
`source_identity_consistent=TRUE`.

## Validation

The final validation matrix passed:

```text
make smoke
make test-theory-figures
make pdf
make supplement
git diff --check
```

The deterministic figure suite checks asymmetric-Laplace CDF/quantile
inversion, support, mass, analytic moments, truncated moments, ET/RQR/SH
oracles, retained-mean identities, positive-affine and reflection behavior,
adaptive domains, glyph/order/solid-line grammar, deterministic bytes, and
provenance hashes.

The final article has 18 pages and the supplement has 23 pages. Their final
logs contain no TeX warnings, undefined citations or references, multiply
defined labels, or overfull/underfull boxes. A source audit found:

```text
bibliography keys: 99 unique
cited keys:        24, all present
labels:            51 unique
references:        35, all resolved
```

All PDF fonts are embedded and subsetted. The embedded publication PNGs are
333 ppi at final size. Every page of both PDFs was rendered and visually
inspected, including the title, abstract, all statements and proofs,
algorithms, figures, tables, transitions, bibliography, page breaks, and
floats. All scientific figures were also inspected in color, grayscale,
protanopia, deuteranopia, and tritanopia simulations; no scientific distinction
depends on color.

## Tracked changes

The implementation commit changes only:

```text
main.tex
rqr-gibbs-supplement.tex
refs.bib
figures/README.md
figures/generate_rqr_theory_figures.R
figures/test_rqr_theory_figure_oracles.R
figures/generated/fig01_three_balance_principles.png
figures/generated/fig02_mean_tilt_recovery_map.png
figures/generated/figS01_cross_distribution_recovery.png
figures/generated/rqr_theory_figure_provenance.csv
figures/rqr_gibbs_computational_schematic.tex  [removed]
docs/audits/rqr_model_family_architecture_review_disposition_20260726.csv
docs/implementation_notes/rqr_model_family_architecture_section_map_20260726.md
```

Generated PDFs, TeX intermediates, rendered pages, accessibility variants,
review ZIPs, extracted review files, and full local figure manifests remain
ignored.
