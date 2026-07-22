# Jerez Native RQR-DLM Implementation Audit

Date: 2026-07-22

## Purpose

This audit records the first native standalone RQR implementation and the
initial linear dynamic/state-space extension. It closes the implementation
gate before any promotion-grade simulation study is launched.

## Source Provenance

| Repository | Branch | Commit | Role | State |
|---|---|---|---|---|
| RQR-GIBBS | agent/ignore-chatgpt-pro-prompt | de8925bd18f41e905de06630e07627f39ff9aaa7 | Native implementation and manuscript | Clean after implementation commit |
| exdqlm reference | feature/rqr-desn-readout-20260716 | dffb71ee70b597d6a716ee74be1cbc99731cd453 | Read-only RQR-DESN and DQLM interface reference | Clean and unmodified |
| Q-DESN article | main | f9f22804eff3871bb5350c8add04b7c9f4d4957b | Read-only writing/style reference | Clean and unmodified |

The RQR-GIBBS implementation commit above was created before this audit note;
the final handoff commit therefore follows it. The local ChatGPT Pro prompt and
the imported ChatGPT output are ignored and are not part of repository history.

## Native Package Surface

The companion package now lives under `application/` and provides:

- native fixed-design RQR Gibbs sampling with independent ridge priors;
- normalized learned-scale and pure-loss scale modes;
- exact GIG(1/2) latent-variable simulation via an inverse-Gaussian transform;
- exdqlm-compatible `FF`, `GG`, `m0`, and `C0` state-space construction;
- polynomial, Fourier seasonal, regression, and composed DLM components;
- scalar-observation Kalman filtering, smoothing, and FFBS in pure R and C++;
- fixed component discounts, frozen discount templates, and an explicitly
  approximate adaptive-discount mode;
- missing-observation handling and future interval-root state forecasting;
- an RQR-DESN wrapper whose reported model family is distinct from the native
  fixed-design implementation.

The two roots use the same state evolution and discount specification by
default. A frozen, data-derived discount template is labeled empirical Bayes.
The adaptive mode is not represented as an exact joint-target Gibbs update:
the returned metadata sets `exact_joint_target = FALSE` and the fit warns.

## Dynamic Gibbs Contract

The implementation uses a partially collapsed sweep:

1. evaluate the loss at the current root paths and draw learned `lambda` from
   its conditional with the latent mixing path integrated out;
2. refresh the full latent mixing path using the new `lambda`;
3. sample root 1 by FFBS conditional on root 2 and the refreshed scales;
4. sample root 2 by FFBS conditional on the updated root 1 and the refreshed
   scales.

The code and manuscript call this an asymmetric-Laplace augmentation of a loss
update, not an ordinary response likelihood or a posterior predictive response
model. Future simulation produces interval-root forecasts unless a separate
response-simulation contract is explicitly supplied.

## Validation Gates

The following commands passed on Jerez after the implementation:

```text
make smoke
make pdf
make supplement
make test-exdqlm-rqr
make literature-manifest
make test-native
make package-check
```

Observed results:

- R 4.5.3 environment smoke test passed;
- the article and supplement compiled without TeX warnings, undefined
  references, underfull/overfull boxes, or errors in the final logs;
- all focused tests against the pinned exdqlm RQR branch passed;
- the local-only literature manifest covered 18 PDFs;
- native R and C++ FFBS parity tests passed;
- model composition, component discounts, GIG moments, learned-scale updates,
  missing observations, adaptive-mode warnings, frozen-template labeling,
  future root forecasts, and the static zero-evolution case passed;
- `R CMD check` completed with status OK.

Generated PDFs, TeX logs, package archives, check directories, compiled
objects, fitted models, and simulation outputs remain ignored. No heavy
simulation was launched.

This document records the first implementation gate. The later ChatGPT Pro
output 2 audit and corrective implementation supersede its open
adaptive-discount and numerical-repair questions; they do not change the
historical validation facts recorded here.

## Handoff Decision

This review was subsequently completed. Its resolved findings and remaining
bounded-pilot gate are recorded in the dedicated output 2 audit rather than
retroactively folded into this historical snapshot.
