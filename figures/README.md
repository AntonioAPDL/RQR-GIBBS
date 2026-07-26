# RQR article figures

This directory contains tracked source for the article's deterministic
population/oracle figures and algebraic schematics. It does not contain fitted
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

The nested `.gitignore` retains only the five publication PNGs and ignores the
redundant PDF, CSV, and manifest outputs in that directory. The full local audit
bundle remains under `application/cache/`, and the repository also retains the
generator, tests, and vector-native TikZ schematics.

The generator uses analytic truncated first moments for its declared
distributions and independent response-space integration in the tests. Its
manifest records the source revision, generator hash, configuration, numerical
tolerances, dependencies, panel-data hashes, and output hashes. CSV and PNG
bytes are checked across two independent runs. Base-R PDF devices embed
generation timestamps, so PDF byte identity is not claimed.

Every generated panel is explicitly classified as deterministic
population/oracle theory. It cannot support claims about finite-sample
recovery, calibration, comparative performance, MCMC accuracy, or
posterior-predictive responses.
