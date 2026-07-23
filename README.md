# RQR-GIBBS

Standalone manuscript and reproducibility workspace for Bayesian relaxed
quantile regression (RQR) with Gibbs sampling, fixed nonlinear DESN readouts,
and a native linear dynamic/state-space extension. The working descriptor is
coverage-targeted interval-root regression under the RQR loss.

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
  model builders, pure-R FFBS, exact component-scale evolution, the RQR-DLM
  sampler, restart helpers, and DESN adapters.
- **application/src/** contains the C++17/RcppArmadillo FFBS kernel.
- **application/tests/** contains native package gates and copied exdqlm
  reference tests.
- **docs/implementation_notes/rqr_dlm_native_design_20260722.md** freezes the
  exact and experimental evolution-mode contracts.

Fixed evolution covariances, frozen discount templates, and shared
component-specific inverse-Gamma evolution scales define exact samplers for
their stated generalized posteriors. A mixed-derivative audit shows that the
exdqlm-style adaptive conditional-discount kernels are not generally compatible
with a common smooth joint density while retaining their simple FFBS forms.
That mode remains available only as an explicitly experimental working update.
The public constructor is deliberately named
`rqr_evolution_adaptive_working()`; exact alternatives use
`rqr_evolution_fixed()`, `rqr_freeze_discount_template()`, or
`rqr_evolution_component_scale()`.

The package defaults to fail-fast Gaussian factorizations and rejects any
negative-eigenvalue projection. An audit mode can record repairs, but
mathematical target status, numerical execution status, and reproducibility
eligibility are reported separately. Promotion requires an exact target, zero
repairs, and a clean checkout at an explicitly expected commit. Compact fit
objects retain terminal state draws, integrity-digested RNG checkpoints, a
versioned schema, Git/R/compiler/BLAS provenance, and complete
data/model/target/evolution digests; full paths are opt-in. A portability
override is durable and removes reproducibility and promotion eligibility.
Continuation also carries a separately digested cumulative history contract,
including every mismatch/override generation, and compares the requested and
resolved FFBS backends before making a bitwise claim. Promotion binds the
executing `rqrgibbs` namespace to the clean primary checkout (or a verified
isolated-runtime attestation), not merely to an installed version string.
The version-4 attestation links the exact Git archive to the built source
package, rehashes the actual command receipts and logs, and requires a lineage
marker in the executing installed runtime. Continuation history derives
per-segment exactness, target eligibility, mismatch/override effects, and
promotion status from raw facts under a versioned target digest.
Component-scale forecasts can combine saved evolution-scale draws with fixed
future component templates.

The pinned exdqlm branch remains the read-only implementation reference for
RQR-DESN and RHS-family compatibility. Promotion of either path additionally
requires the executing exdqlm namespace to come from an isolated, attested
runtime of the exact pinned commit. Direct source-tree loading is prohibited.
`make prepare-exdqlm-runtime` uses read-only Git access to create an archive,
then builds entirely under the ignored `application/cache/` tree. Its
versioned attestation binds the source commit and tree, archive checksum,
installed package digest, and disjoint archive/runtime paths while recording
that the full external checkout—including ignored files—was unchanged. The
RQR adapters also refuse a namespace whose package path contains Git checkout
metadata. The reference smoke tests are extracted from the attested archive
and never execute from the exdqlm checkout.

The corrected frozen learned-scale bounded pilot passed at source commit
`3a37c5ee42973fd0ba1fa4792f609d1a48bcc98f`: four production collapsed chains,
four independently coded fully augmented chains, and adaptive root quadrature
agreed under the predeclared MCSE gates, with all R-hat/ESS gates satisfied and
zero numerical repairs. This is a computational target check, not evidence of
empirical coverage calibration. Exact results and the one pre-scientific
diagnostic-infrastructure failure are recorded in
`docs/audits/chatgpt_pro_output5_audit_20260722.md`.

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
    make prepare-exdqlm-runtime
    make test-exdqlm-rqr
    make literature-manifest
    RQR_EXPECTED_PRIMARY_COMMIT=<reviewed-full-sha> \
      RQR_BOUNDED_PILOT_CONFIRM=YES make bounded-pilot
    RQR_EXPECTED_PRIMARY_COMMIT=<reviewed-full-sha> \
      make preflight-dlm-bounded
    RQR_EXPECTED_PRIMARY_COMMIT=<reviewed-full-sha> \
      make reference-dlm-bounded

No production simulation should be launched until its matched protocol is
frozen and explicitly approved. The bounded pilot does not provide that
approval, and the committed bounded-dynamic execution flag is false.
`make test-exdqlm-rqr` and `make bounded-pilot` prepare the isolated
runtime automatically; neither target compiles or writes inside an exdqlm
repository.
