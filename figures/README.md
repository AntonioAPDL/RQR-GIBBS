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
the figure assets before compiling the documents. A standalone submission or
Overleaf bundle must include the generated PDF assets at the paths referenced
by the TeX sources, while the Git repository retains the audited generator,
tests, and vector-native TikZ sources.

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
