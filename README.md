# RQR-GIBBS

Standalone manuscript and reproducibility workspace for Bayesian relaxed
quantile regression (RQR) with Gibbs sampling, fixed nonlinear DESN readouts,
and a native linear dynamic/state-space extension.

## Purpose

The project separates RQR from the Q-DESN article because RQR has a different
inferential target. Q-DESN estimates conditional quantile ordinates. RQR
directly estimates two interval roots under a residual-product loss and a
generalized-Bayes update. The Gibbs construction arises from a pseudo-AL
augmentation of that loss.

## Current status

The repository contains a manuscript, derivation supplement, and development R
package under **application/**:

- **main.tex** states the fixed-design and dynamic targets.
- **rqr-gibbs-supplement.tex** gives the population, augmentation,
  learned-scale, FFBS, and component-discount derivations.
- **application/R/** contains fixed-design utilities, exdqlm-compatible DLM
  model builders, pure-R FFBS, the RQR-DLM sampler, and DESN adapters.
- **application/src/** contains the C++17/RcppArmadillo FFBS kernel.
- **application/tests/** contains native package gates and copied exdqlm
  reference tests.
- **docs/implementation_notes/rqr_dlm_native_design_20260722.md** freezes the
  exact and experimental evolution-mode contracts.

Fixed evolution covariances and frozen discount templates define exact samplers
for the stated generalized posterior. The exdqlm-style adaptive discount
recursion is available only as an explicitly experimental working update
because its fixed-joint-target interpretation has not been established.

The pinned exdqlm branch remains the read-only implementation reference for
RQR-DESN and RHS-family compatibility.

## Pinned external reference

Expected exdqlm RQR branch:

    repo: https://github.com/AntonioAPDL/exdqlm
    branch: feature/rqr-desn-readout-20260716
    expected commit: dffb71ee70b597d6a716ee74be1cbc99731cd453

## Local-only workspaces

The literature PDFs, generated manifests, ChatGPT Pro handoff files, and heavy
application data, caches, runs, logs, and outputs are intentionally ignored.
Use **application/scripts/01_build_literature_manifest.R** to recreate the local
PDF inventory and checksums.

## Basic commands

    make smoke
    make pdf
    make supplement
    make package-install
    make test-native
    make test-exdqlm-rqr
    make literature-manifest

No heavy simulation should be launched until the mathematical, native-package,
and frozen-protocol gates pass.
