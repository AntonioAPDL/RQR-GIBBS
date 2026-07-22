# RQR-GIBBS

Standalone manuscript and reproducibility workspace for Bayesian relaxed
quantile regression (RQR) with Gibbs sampling, fixed nonlinear DESN readouts,
and a planned linear dynamic/state-space extension.

## Purpose

The project separates RQR from the Q-DESN article because RQR has a different
inferential target. Q-DESN estimates conditional quantile ordinates. RQR
directly estimates two interval roots under a relaxed quantile loss and a
generalized-Bayes update. The Gibbs construction arises from a pseudo-AL
augmentation of the residual-product loss.

## Current Status

This repository is scaffolded from the Q-DESN article style and the pushed
exdqlm RQR implementation branch. It contains:

- `main.tex`: standalone article scaffold;
- `rqr-gibbs-supplement.tex`: derivation and reproducibility supplement;
- `STYLE_PROFILE.md`: academic writing standards inherited from the Q-DESN
  manuscript workflow;
- `application/R/`: synchronized RQR implementation seed files from exdqlm;
- `application/scripts/`: RQR simulation and audit scripts copied from the
  exdqlm RQR branch, plus repository preflight utilities;
- `application/tests/`: package-side RQR tests copied for reference;
- `docs/implementation_notes/`: implementation notes and run-design documents;
- `tables/`: migrated RQR evidence table from the Q-DESN supplement.

The copied RQR code is a seed/reference layer. The validated implementation
source of truth remains the pinned exdqlm branch until this repository promotes
a native standalone implementation.

## External Code Dependency

Expected exdqlm RQR branch:

```text
repo: https://github.com/AntonioAPDL/exdqlm
branch: feature/rqr-desn-readout-20260716
expected commit: dffb71ee70b597d6a716ee74be1cbc99731cd453
```

On Jerez, clone it beside this repo:

```bash
BASE=/data/muscat_data/jaguir26
cd "$BASE"
git clone https://github.com/AntonioAPDL/exdqlm.git exdqlm__wt__qdesn_0p4p0_integration
cd exdqlm__wt__qdesn_0p4p0_integration
git fetch origin
git checkout feature/rqr-desn-readout-20260716
git pull --ff-only origin feature/rqr-desn-readout-20260716
```

## Local-Only Workspaces

The following directories are intentionally ignored:

- `literature/pdfs/`
- `literature/notes/`
- `application/data_local/`
- `application/cache/`
- `application/runs/`
- `application/logs/`
- `application/outputs/`
- `.codex_work/`

Use `application/scripts/01_build_literature_manifest.R` to create local PDF
checksums after copying PDFs into `literature/pdfs/`.

## Basic Commands

```bash
make smoke
make pdf
make supplement
make test-exdqlm-rqr
make literature-manifest
```

`make pdf` uses `latexmk` when available and otherwise falls back to
`pdflatex` plus `bibtex`.

## Jerez Bootstrap

Target server:

```text
jaguir26@jerez.be.ucsc.edu
/data/muscat_data/jaguir26
```

The first Jerez Codex chat should validate:

1. this repo is at the pushed scaffold commit;
2. exdqlm is at the expected RQR branch commit;
3. R 4.5.3 can load required packages;
4. focused RQR package-side tests pass;
5. `pdflatex` can compile the article scaffold.

Do not launch heavy simulations until those gates pass.
