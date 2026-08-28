# MTI-INTERVALS

This repository contains the tolerance-interval manuscript and companion R
implementation for mean-preserving intervals (MPI), mean-tilted intervals (MTI),
and tolerance-calibrated shortest-path (TCSP) intervals.

The manuscript in `main.tex` is now focused on fixed-content interval geometry,
the MPI and MTI targets, the TCSP tolerance action, the calibrated MTI-ECM
comparator, and iid tolerance validation. The broader endpoint-regression,
fixed-feature, Gibbs, ECM, and dynamic endpoint development is split into the
companion manuscript repository `AntonioAPDL/MTI-EXTENSIONS`.

Earlier versions used the Relaxed Quantile Regression (RQR) name and the
`rqr_*` API. The current manuscript uses MPI and MTI terminology. Legacy names
remain in the package and scripts where needed for reproducibility and
attribution to the original residual-product criterion of Pouplin et al.

## Repository Contents

- `main.tex`: main tolerance-interval article.
- `rqr-gibbs-supplement.tex`: proofs, fixed-target MTI-ECM details, direct DP
  content screening, feasibility thresholds, and validation tables.
- `refs.bib`: bibliography.
- `figures/` and `tables/`: figure and table generators plus tracked manuscript
  inputs.
- `application/`: R package, C++ code, simulation scripts, configurations, and
  tests.
- `docs/`: implementation notes, theory records, and validation records.
- `.codex_work/`: ignored local planning notes.

Large generated objects, fitted model outputs, logs, run directories, local
caches, and local literature PDFs are excluded from version control.

## Statistical Framing

MPI and MTI are loss-defined interval targets. Endpoint summaries from a
generalized posterior describe uncertainty for the specified interval-root
functional. They are not posterior predictive response draws.

TCSP is the primary tolerance action in the article. It calibrates a retained
order-statistic count from the uniform scan statistic and then reports the
shortest closed order-statistic window at that count. The iid tolerance
confidence statement comes from this scan calibration. The MTI-ECM comparator is
a calibrated fixed-target generalized-Bayes mode calculation evaluated by
independent repeated-sampling validation.

## Build and Checks

```bash
make test-theory-figures
make test-theory-tables
make test-manuscript-language
make pdf
make supplement
```

Package checks are available through the `application/` targets in the
`Makefile`. Heavy simulation runs should record the Git commit and write their
outputs under ignored run or output directories.
