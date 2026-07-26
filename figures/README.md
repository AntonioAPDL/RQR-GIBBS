# RQR article figures

This directory contains tracked source for the article's deterministic
population figures and algebraic schematics. It does not contain fitted
models, MCMC output, simulation evidence, or a response-generating model.

Run the numerical oracle and reproducibility checks with:

```bash
make test-theory-figures
```

Generate the article-ready PDF figures, PNG previews, panel-level CSV files,
and the hash manifest with:

```bash
make theory-figures
```

The default output root is:

```text
application/cache/rqr_theory_figures/
```

That root is intentionally ignored. `make pdf` and `make supplement` regenerate
the complete figure-audit bundle before compiling the documents.

The TeX sources use the publication PNGs tracked under:

```text
figures/generated/
```

These 300-dpi assets make a clean Git or Overleaf checkout self-contained.
They are generated from the same audited source with:

```bash
Rscript figures/generate_rqr_theory_figures.R \
  --output-dir=figures/generated
```

For an immutable source archive or a release checkout, explicit source identity
can be supplied without pretending that Git established cleanliness:

```bash
Rscript figures/generate_rqr_theory_figures.R \
  --output-dir=figures/generated \
  --source-commit=<full-git-object-id> \
  --source-archive-sha256=<full-sha256>
```

If Git is available, a declared commit must match `HEAD`. If either Git query
fails, repository cleanliness is recorded as unknown (`NA`), never as clean.
An explicit commit or archive digest is recorded separately from detected Git
state.

The nested `.gitignore` retains only the four publication PNGs and a lightweight
provenance receipt. Redundant PDF, panel CSV, and full-manifest outputs remain
ignored. The receipt records each publication PNG's byte count and SHA-256,
the generator hash, and the detected and declared source identities. The full
local audit bundle remains under `application/cache/`, and the repository also
retains the generator, tests, and vector-native TikZ schematics.

The generator uses analytic truncated first moments for its declared
distributions and independent response-space integration in the tests. The
asymmetric-Laplace population benchmark uses the quantile-regression
parameterization
`(mu_AL, s_AL, tau_AL) = (0, 1, 0.99)`. Under this convention it is strongly
left-skewed. Its interval targets are solved on the raw response scale and only
then mapped to mean/standard-deviation standardized coordinates. This
population benchmark is distinct from the pseudo-asymmetric-Laplace
loss-kernel augmentation applied to the RQR pseudo-residual.

The manifest records the source revision, generator hash, configuration,
numerical tolerances, dependencies, panel-data hashes, and output hashes. CSV
and PNG bytes are checked across two independent runs. Base-R PDF devices embed
generation timestamps, so PDF byte identity is not claimed.

Every target interval segment is solid. Target identity remains redundant
through direct labels, stable ordering, color, and filled endpoint glyphs:
equal-tailed uses squares, ordinary RQR uses circles, and shortest contiguous
uses triangles. The symmetric Normal benchmark marks exact three-way
coincidence without horizontal jitter. Distribution-panel axes are computed
from deterministic quantiles, all target endpoints, the population mean,
density knots, and label extents; generation fails rather than silently
clipping required geometry.

The current base-R vector PDFs use unembedded device fonts, so TeX deliberately
continues to use the audited 300-dpi PNGs. A future vector transition must use
an embedded-font device and pass the same font and provenance gates before the
TeX inputs change.

Every generated panel is explicitly classified as a deterministic population
illustration. It cannot support claims about finite-sample
recovery, calibration, comparative performance, MCMC accuracy, or
posterior-predictive responses.
