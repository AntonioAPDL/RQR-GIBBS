# Publication illustration label simplification

Date: 2026-07-26

## Decision

The exact family and parameters of the left-skewed population fixture are no
longer shown in Figures 1, 2, or S1 or in their captions. The distribution is
arbitrary and serves only to make the interval-placement principles visibly
different. Naming it in the article risked suggesting that it was part of the
RQR construction or that its distributional index was related to interval
content.

The publication now uses:

```text
Figures 1 and 2:
  Left-skewed illustration; interval content c = 0.8

Figure S1, second column:
  Left-skewed
```

The captions refer to a continuous left-skewed population illustration and
state that its distributional specification is not part of RQR.

## Reproducibility boundary

The exact numerical fixture remains fully reproducible. Its family,
parameterization, analytic moments, interval endpoints, recovery tilts, and
hash-bound source identity remain in:

- `figures/generate_rqr_theory_figures.R`;
- `figures/test_rqr_theory_figure_oracles.R`;
- `figures/README.md`;
- generated panel data and the full local manifest;
- `figures/generated/rqr_theory_figure_provenance.csv`;
- `docs/audits/rqr_al08_illustration_revision_20260726.md`.

The clean scientific implementation commit is:

```text
dbcd2ac4963de75c64fae4057bcac9bda09c00d7
```

Its publication receipt records the exact internal fixture, interval content,
and distributional index in separate fields while binding all rows to the
full commit with `repository_clean=TRUE` and
`source_identity_consistent=TRUE`.

The pseudo-asymmetric-Laplace terminology in the Gibbs sections was
intentionally retained. Those passages describe the normal--exponential
augmentation of the exponentiated pseudo-residual loss kernel; they are part
of the computational derivation and are distinct from the unnamed response
population used in the figures.

## Numerical invariance

Only reader-facing labels and captions changed. Every panel-data CSV was
compared byte for byte with the preceding validated
\(\operatorname{AL}_{0.8}(0,1)\) figure build. All hashes were identical, so
the densities, interval endpoints, lower-tail indices, tilts, widths, and
oracle calculations are unchanged.

The tests now enforce both sides of the boundary:

- rendered display fields and the three publication figure blocks reject the
  internal fixture terms `AL`, `asymmetric-Laplace`, and
  `tau_{\mathrm{AL}}`;
- generator metadata and the publication receipt must still identify the exact
  internal fixture;
- `interval_content` and `al_quantile_index` remain separate receipt fields;
- the complete analytic and numerical oracle suite remains active.

## Tracked publication assets

```text
generator SHA-256:
  2b425692cbd7bc730029bb6ccdb82b49d997ed3a79f505703d89f0d75b55e63f

fig01_three_balance_principles.png:
  94583 bytes
  36546ef9c59a1a26dc0437be0de912db667ce3937667adcef409629961a37e96

fig02_mean_tilt_recovery_map.png:
  139194 bytes
  42d01641b1c182ef7433b2676e5e56c279b6038052c44d2ad7b9701ca05dfb6b

figS01_cross_distribution_recovery.png:
  211783 bytes
  b0bd0e5adda71de04d7e5ed52e4ba2e2ca664557c636c4ec210f6e89e7773eef
```

Figure S2 is numerically and visually unchanged. Its receipt row was refreshed
because the generator hash applies to the complete tracked figure set.

## Validation

The following checks passed:

- `git diff --check`;
- `make smoke`;
- `make test-theory-figures`;
- two independent deterministic figure-generation runs;
- byte-for-byte comparison of all panel-data CSVs with the prior validated
  fixture build;
- reader-facing fixture-name anti-leak checks;
- all analytic and numerical distribution, moment, interval, tilt,
  standardization, and provenance tests;
- `make pdf`;
- `make supplement`;
- final-size visual inspection of Figures 1, 2, and S1 and their rendered
  article/supplement pages;
- grayscale, protanopia, deuteranopia, and tritanopia inspection;
- font, raster-resolution, clipping, and TeX-log checks.

The article remains 18 letter-sized pages and the supplement remains 23. Both
contain 23 embedded, subset fonts. The publication raster figures are 333 ppi.
The shortened outer labels have 14 white pixels above their first non-white
pixel, and no clipping or overlap was observed.

The local, intentionally untracked validation PDF hashes are:

```text
main.pdf:
  615dffbe1141d06fa960dec3b4aacd1f44a8b32c7a648152e602f388e0183500

rqr-gibbs-supplement.pdf:
  3ce202e065d253cf4238cf595c10bf4ac97953672af50932fca2e0b951bac778
```

No tracked `application/` file, simulation configuration, fitted object, or
protected reference repository was modified.
