# Mean-Tilted Intervals: Short Tolerance Intervals

This repository contains the tolerance-interval manuscript and companion R
implementation for mean-preserving intervals (MPI), mean-tilted intervals (MTI),
and calibrated MTI-ECM tolerance intervals.

The manuscript in `main.tex` is now focused on fixed-content interval geometry,
the MPI and MTI targets, the calibrated MTI-ECM procedure, and iid tolerance
validation against Young--Mathew and Wilks. The broader endpoint-regression,
basis-expansion, Gibbs, ECM, and dynamic endpoint development is split into the
companion manuscript repository `AntonioAPDL/MTI-EXTENSIONS`.

Earlier versions used the Relaxed Quantile Regression (RQR) name and the
`rqr_*` API. The current manuscript uses MPI and MTI terminology. Legacy names
remain in the package and scripts where needed for reproducibility and
attribution to the original residual-product criterion of Pouplin et al.

## Repository Contents

- `main.tex`: main tolerance-interval article.
- `rqr-gibbs-supplement.tex`: proofs, fixed-target MTI-ECM details, direct DP
  content-probability checks, feasibility thresholds, and validation tables.
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

MTI-ECM is the proposed tolerance procedure reported in the article. It profiles
fixed-target generalized-Bayes endpoint modes over fitted content and tilt,
then selects the shortest fitted interval meeting a direct Dirichlet-process
posterior content check. Its calibration rule is fixed before independent
repeated-sampling validation. The simulation and pharmaceutical results report
MTI-ECM, Young--Mathew, and Wilks.

The scan reference (TCSP in the code) is retained for constructing the
fitted-content grid and for the calibration width-stability criterion. It is
not a reported comparator. Scan algorithms, original simulation configurations,
and historical analysis outputs remain available for reproducibility. The
selected MTI-ECM interval has empirical tolerance validation; a
distribution-free theorem for this selection rule remains open.

## Build and Checks

```bash
make test-theory-figures
make test-theory-tables
make test-manuscript-language
make pdf
make supplement
```

Package checks are available through the `application/` targets in the
`Makefile`. Heavy simulations should record the Git commit and write their
outputs under ignored simulation-output directories.
