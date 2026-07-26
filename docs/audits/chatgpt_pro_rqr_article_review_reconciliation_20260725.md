# ChatGPT Pro RQR article-review reconciliation

Date: 2026-07-25

Article branch: `codex/rqr-article-review-20260725`

Base RQR-GIBBS commit: `224758ebb6000c332eaf5ddf173d3e5eb2e5e6c3`

Authenticated review-source commit: `f7422c9205e9b73f5b47097fc486ae79edfa7bfe`

## Review input

The local review archive
`chatgpt_pro_rqr_article_review_bundle_20260725.zip` has SHA-256
`9507792d06840b8700cb5f7b5d431f45e571f6a0c22c09da2b65e82ae02caf1b`.
Its internal artifact manifest and every listed artifact hash were verified.
The reviewed article, supplement, bibliography, and implementation files were
then matched to the authenticated Git source. The archive and extracted review
files remain under the ignored `application/cache/` tree.

## Accepted article corrections

- The title now identifies the method as generalized Bayesian.
- The abstract is shorter and separates fixed-tilt theory from the currently
  implemented ordinary-RQR target.
- The mean-tilt proposition distinguishes interior solutions from endpoint
  windows obtained by closure and one-sided boundary conditions.
- The supplement uses distinct notation for forward affine transformation and
  response standardization.
- The article and supplement state the limited Gaussian-prior propriety
  argument and do not claim a general nonzero-tilt propriety theorem.
- The main validation prose retains the promotion requirements without
  reproducing repository-attestation mechanics in the statistical narrative.
- The preliminary, calibration-screened RQR-DESN table is withheld until the
  matched standalone study is complete.
- Wu and Martin is updated to the 2023 *Bayesian Analysis* article, and the
  Shaby DOI is corrected.
- The peripheral ESN-as-state-space preprint is no longer used to support the
  core dynamic-model sentence.

## Figure implementation

The reviewed candidate generator was not promoted verbatim. An end-to-end run
exposed a tail-adjacent numerical-integration failure for the Lognormal
distribution that its candidate test did not exercise. The repository
generator instead uses analytic truncated first moments for all declared
families and validates them against independent response-space integration,
including windows next to both probability boundaries.

The tracked implementation produces:

1. three interval-selection principles under skewness;
2. symmetry versus right skewness;
3. the mean-tilt recovery map;
4. a four-distribution recovery matrix; and
5. RQR loss geometry.

Two vector-native TikZ sources record:

1. the generalized-target, pseudo-residual augmentation, and Gibbs scan; and
2. the RQR-DLM stacked-prior, quartic-observation, sequential-FFBS logic.

The generated PDF, PNG, CSV, and manifest artifacts stay under
`application/cache/rqr_theory_figures/`. The TeX build targets regenerate them
automatically. Captions label every numerical panel as deterministic
population/oracle theory and explicitly exclude empirical or predictive
interpretations.

## Scope deliberately deferred

No file under `application/R/`, `application/src/`,
`application/scripts/`, `application/tests/`, or the active simulation
configuration was changed in this article pass. In particular, the pass does
not:

- add a nonzero-tilt package API;
- reuse the ordinary learned-scale Gamma update for nonzero tilt;
- generalize the active simulation runners;
- claim finite-sample recovery or data-driven tilt selection;
- add dynamic or DESN mean-tilt evidence; or
- alter the separately authorized RQR-DLM simulation launch.

Those changes require a separate fixed-rate implementation, zero-tilt
equivalence tests, quadrature and dense-Gaussian validation, and only then
finite-sample study design.

## Reproducibility gates

The article pass is accepted only if all of the following hold:

```text
make test-theory-figures
make pdf
make supplement
```

The final build audit also checks for unresolved citations/references,
overfull boxes, tracked generated PDFs, and accidental changes to active
simulation source.

## Validation result

All article-pass gates completed successfully on Jerez:

| Gate | Result |
| --- | --- |
| `make smoke` | Pass |
| `make test-theory-figures` | Pass, including two-run CSV/PNG byte checks |
| `make pdf` | Pass with BibTeX; 13 pages |
| `make supplement` | Pass with BibTeX; 18 pages |
| Final TeX log scan | No unresolved citation/reference, overfull-box, or fatal-error match |
| Visual page inspection | Pass for all generated panels and both TikZ schematics |
| Tracked generated PDFs | None |
| Changes under `application/R`, `application/src`, `application/scripts`, `application/tests`, or `application/config` | None |
