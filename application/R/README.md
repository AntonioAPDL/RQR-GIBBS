# `rqrgibbs` source map

This directory contains the standalone implementation of ordinary
(zero-mean-tilt) RQR. The stochastic update is generalized Bayes under the RQR
loss. The pseudo-AL variables augment that loss; they do not define a response
likelihood or posterior-predictive response distribution.

## Shared ordinary-RQR layer

- `rqr_utils.R` defines the loss, learning-rate, provenance, and shared
  continuation-history utilities.
- `rqr_numerics.R` contains native Gaussian and GIG numerical kernels.
- `rqr_beta_prior.R` defines closure-free shared ridge, full-Gaussian, and
  RHS-NS prior contracts.
- `rqr_rhs_ns.R` implements the native Nishimura--Suchard
  shrunken-shoulder/Makalic--Schmidt Gibbs hierarchy.
- `rqr_fixed_design_data.R` validates observed-mask data and freezes static
  target and prediction schemas.
- `rqr_fixed_design_transition.R` is the single static Gibbs transition.
- `rqr_fixed_design_checkpoint.R` validates checkpoint, target, prior, data,
  environment, and continuation integrity.
- `rqr_mcmc_fit.R` handles storage, reporting, exact continuation, and
  interval-root evaluation.

The two roots always use independent states from one shared exchangeable prior
specification. A label-swap move exchanges each coefficient/prior-state block
as a unit.

## Frozen-feature DESN specialization

- `rqr_desn_design.R` defines immutable training and future feature-design
  contracts.
- `rqr_desn_fit.R` fits the shared fixed-design transition conditional on a
  validated design.
- `rqr_forecast.R` evaluates future interval roots conditional on an explicit
  precomputed, rolling teacher-forced, or external-driver design.

The preferred fitting route needs no executing exdqlm namespace. A separately
identified `exdqlm_reference` adapter may materialize a frozen design from the
pinned source/runtime. It is a read-only design builder, not the RQR inference
engine. Its receipt attests only the training rows. A validated future-design
contract freezes feature alignment, declared semantics, and content, but
remains nonpromotable until a future-specific materialization receipt attests
the origin of those rows. Future-root checks are conditional mechanics and do
not define future responses.

## Dynamic roots

- `rqr_dlm_model.R`, `rqr_evolution.R`, `rqr_ffbs.R`, and
  `rqr_dlm_fit.R` implement the separate root-specific FFBS transition.
- `rqr_ffbs.cpp` and `rqr_interweave.cpp` provide the computational kernels.

Exact dynamic modes use fixed evolution covariances, frozen discount
templates, or declared component-scale priors. Adaptive conditional discounts
remain a working sequential method and are not labeled exact Gibbs for a fixed
joint target.

## Scope

Ordinary v1 includes fixed and normalized learned loss-rate targets. The
`learned_pure` mode remains a non-promotable diagnostic compatibility target.
The current `rqr_vb_fit.R` is experimental and outside ordinary-v1 promotion.
Nonzero mean tilt will be implemented only through these shared transitions
after the ordinary target and validation artifacts are frozen.

Initial learned-rate and latent-scale values do not provide independent chain
dispersion: the mandatory partial-collapse order redraws the rate and then
refreshes every observed latent scale before either root update. Validation
profiles disperse the roots and, for RHS-NS, their complete prior states.

The pinned exdqlm branch remains a protected, read-only compatibility and
comparator reference:

```text
branch: feature/rqr-desn-readout-20260716
commit: dffb71ee70b597d6a716ee74be1cbc99731cd453
```

The executable RQR and RQR-DLM implementation is now native to the standalone
`rqrgibbs` package in this repository. The pinned exdqlm commit is a read-only
compatibility and comparator reference; it is not an implementation target and
is never compiled, installed, or loaded directly from its checkout. Validation
materializes that exact commit with `git archive` and builds it only below the
ignored `application/cache/` tree.

Changes to the native sampler, state-space utilities, or public contracts must
be made here and covered by the package tests. They must not be propagated into
an exdqlm source checkout as a side effect of this project.
