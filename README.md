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

## Pharmaceutical Application

The revised application uses
`application/config/pharma_tolerance_application_mti_only_20261020.json`.
MTI-ECM is the sole proposed procedure; Young--Mathew and Wilks are external
benchmarks. The frozen calibration is unchanged. Both responses share 1000
random partitions, two chronological cases, 250 production-month partitions,
and 250 API-batch partitions. All partitions use 100 training and 87 held-out
batches. Grouped partitions keep whole groups together.

The complete, portable reporting inputs are tracked under
`figures/data/pharma_mti_application_20261020/`. Rebuild the report without
fitting any models:

```bash
make pharma-mti-application-report
make test-pharma-mti-application
make test-manuscript-language
make all-pdf
```

For a new analysis, use fresh output directories and run these stages in order:

```bash
make pharma-mti-application-runtime
make pharma-mti-application-data
make pharma-mti-application-preflight
make pharma-mti-application-smoke PHARMA_MTI_RUN_DIR=application/outputs/pharma_mti_application_20261020/smoke_new
make pharma-mti-application-confirmatory PHARMA_MTI_WORKERS=40 PHARMA_MTI_RUN_DIR=application/outputs/pharma_mti_application_20261020/confirmatory_new
make pharma-mti-application-health PHARMA_MTI_RUN_DIR=application/outputs/pharma_mti_application_20261020/confirmatory_new
```

The runtime target installs this repository's package in its ignored local
library. It does not install or load the external exdqlm checkout. Raw source
data are downloaded only when absent and must pass byte-count, MD5, and SHA256
checks. Method seeds do not depend on worker scheduling; each worker uses one
BLAS/OpenMP thread. Never replace the tracked canonical directory silently.
The explicit `pharma-mti-application-publish` target requires a verified,
complete run and an absent destination.

Held-out empirical content describes these finite, overlapping partitions; it
does not estimate tolerance confidence from independent replications. Selected
ECM convergence flags are retained and reported, including conditional
comparisons on the same convergence-flagged partitions for all methods.
See [the implementation record](docs/implementation_notes/pharma_mti_application_revision_20261020.md)
for provenance, diagnostics, endpoint-precision recovery, and all commands.
Historical `pharma-application-*` targets retain the previous analysis.
