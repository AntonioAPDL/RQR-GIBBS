# Application Workspace

This directory contains the reproducibility layer for the standalone
RQR-GIBBS article.

Current layout:

- `R/`: synchronized RQR implementation seed files copied from the exdqlm RQR
  branch.
- `scripts/`: preflight, manifest, simulation, collection, and audit scripts.
- `tests/`: copied package-side RQR tests for reference and future standalone
  adaptation.
- `config/`: frozen RQR-DESN simulation configuration seeds.
- `manifests/`: lightweight committed manifest documentation; generated CSV,
  JSON, and RDS manifests are local-only.

The heavy directories `data_local`, `cache`, `runs`, `logs`, and `outputs` are
ignored by git.

