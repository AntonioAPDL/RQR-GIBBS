# AL(0.8) illustration revision

Date: 2026-07-26

## Scope

This pass changes only the deterministic population illustration used in
Figures 1, 2, and S1 from the earlier
\(\operatorname{AL}_{0.99}(0,1)\) law to
\(\operatorname{AL}_{0.8}(0,1)\). It does not change the RQR loss, interval
content, generalized posterior, Gibbs sampler, RQR-DLM implementation,
simulation configuration, or any empirical result.

The validated scientific implementation commit is:

```text
d45ab4f82b926eb5abf126318b70c23b338b24a3
```

Work was performed in the isolated manuscript worktree
`.codex_work/rqr_model_family_review_20260726`. The primary checkout remained
at `bb966299bb298ee31ec65d167edf53c44ce48b03`, the source state used by the
active confirmatory simulation. No tracked `application/` file or protected
reference repository was modified.

## Statistical specification

The illustrative response law follows the quantile-regression
asymmetric-Laplace convention

```text
(mu_AL, s_AL, tau_AL) = (0, 1, 0.8),
Pr(Y <= mu_AL) = tau_AL.
```

The interval content remains the separately declared input \(c=0.8\).
The numerical equality \(\tau_{\mathrm{AL}}=c\) is incidental: the former
controls the illustration's distributional asymmetry, while the latter
controls interval probability content. Generator constants, plot labels,
manuscript captions, tests, and the publication receipt preserve these as
separate fields. Perturbation tests change each input independently to prevent
an accidental software coupling.

For the raw illustrative law,

```text
mean     = -3.75
variance = 26.5625
SD       = 5.153882032022076
```

Independent analytic and numerical checks give the following population
oracles, ordered as shortest contiguous (SH), equal-tailed (ET), and ordinary
RQR:

| Target | Lower-tail index | Raw lower | Raw upper | Standardized tilt | Standardized width |
|---|---:|---:|---:|---:|---:|
| SH | 0.160000000000 | -8.047189562171 | 2.011797390543 | 0.292759522537 | 1.951730150247 |
| ET | 0.100000000000 | -10.397207708399 | 0.866433975700 | 0.140204357045 | 2.185467500831 |
| RQR | 0.053432565674 | -13.530956617480 | 0.388539676790 | 0.000000000000 | 2.700778987137 |

Every interval has population content \(0.8\). The general shortest-window
optimizer agrees with the asymmetric-Laplace crossing-mode identity
\(u_{\mathrm{SH}}=\tau_{\mathrm{AL}}(1-c)=0.16\), and the ordinary-RQR retained
mean agrees with the population mean.

## Figure and manuscript changes

- Figures 1 and 2 now identify the response law as
  \(\operatorname{AL}_{0.8}(0,1)\) and state inside the figure that
  \(\tau_{\mathrm{AL}}\) and \(c\) are separate inputs.
- Figure S1 uses the same asymmetric-Laplace panel and retains Normal,
  Lognormal, and Beta comparison laws.
- All interval segments remain solid; glyph, direct-label, vertical-position,
  and color encodings remain redundant.
- The AL plotting domain now uses the common adaptive probability contract
  rather than a residual `0.99`-specific tail override.
- Figure 1 and 2 outer titles were moved inward. Their first non-white pixel is
  now six pixels below the top boundary rather than touching it.
- Main-text and supplementary captions define the distributional index and
  explicitly state that it was not selected to match interval content.
- The earlier architecture reconciliation retains its historical
  \(\tau_{\mathrm{AL}}=0.99\) values but now points to this superseding record.

## Deterministic assets and source binding

The clean-tree publication receipt binds all four tracked figure rows to
`d45ab4f82b926eb5abf126318b70c23b338b24a3`, with
`repository_clean=TRUE`, the same full declared source commit, and
`source_identity_consistent=TRUE`.

```text
generator SHA-256:
  0ec57f772ebb292ea7bdad3882312ed655926af4c40b3df806abac6dfb0c5fa3

fig01_three_balance_principles.png:
  96245 bytes
  e720371419a6ca19cf5d63bd2c9c3113b92d548ab42fb70e8f0a5e199b8fa1ae

fig02_mean_tilt_recovery_map.png:
  140806 bytes
  2a62554c2ae49026828d2c3f6f6dd4934728f672727b074aa80866c4d5134c1a

figS01_cross_distribution_recovery.png:
  213297 bytes
  83c807cb7f1c771eff6df96e473586c8b6ca3e7ea22d83df274d8968aa810b94
```

Figure S2 is scientifically unchanged; its receipt row is regenerated because
the generator hash and receipt schema apply to the complete publication set.

## Validation

The following checks passed:

- `git diff --check`;
- `make smoke`;
- `make test-theory-figures`;
- two independent deterministic generator runs with byte-identical PNG and
  panel-CSV outputs;
- analytic AL mass, inversion, moment, truncated-moment, shortest-window,
  retained-mean, affine-equivariance, and frozen-oracle checks;
- independent perturbations of `tau_AL` and `c`;
- stale `tau_AL=0.99` rejection in current publication metadata;
- adaptive-domain and direct-label containment checks;
- `make pdf`;
- `make supplement`;
- clean final TeX logs with no LaTeX/package warnings, overfull or underfull
  boxes, undefined references or citations, multiply defined labels, or rerun
  notices.

The article has 18 letter-sized pages and the supplement has 23. Both PDFs
contain 23 embedded, subset fonts. Their local, intentionally untracked
validation hashes are:

```text
main.pdf:
  3d87fa23ca0675a7a867ed04f4680ed5e4c3d657ea3bfa6745f8bd04e10a2a50

rqr-gibbs-supplement.pdf:
  75025a321aec4b096665a7a6a1daa6e79af6f4b87bd41a2efff2fb4f64c09f37
```

All affected publication figures and their rendered manuscript pages were
inspected at final size. Color, grayscale, protanopia, deuteranopia, and
tritanopia views preserve the scientific distinctions through direct labels,
glyphs, position, and solid segment geometry rather than color alone. Embedded
raster figures are 333 ppi. No clipping, overlap, or illegible annotation was
observed.
