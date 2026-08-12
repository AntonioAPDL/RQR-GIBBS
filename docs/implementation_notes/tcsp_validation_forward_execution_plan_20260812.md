# TCSP Validation Forward Execution Plan

Date: 2026-08-12
Baseline source: `main@8d7b959aa664a491448e814f6939cd246ad6a3d3`
Status: staged pilot and competitor-readiness plan

Report5 addendum: the next-step execution path is superseded by
`docs/implementation_notes/tcsp_ecm_split_exact_validation_plan_20260812.md`.
After adding MT-RQR-ECM and pilot-selected exact-spacing TCSP, smoke validation
for those two new components must pass before any full-pilot relaunch is
considered.

## Purpose

This plan turns the P0 hardening pass into a reproducible validation sequence.
It deliberately separates implementation validation, competitor expansion, and
any future full-pilot or confirmatory execution.

## Diagnosis

The repository is now hardened for the most important TCSP risk: Monte Carlo
scan critical counts are selected from one simulated Uniform scan-statistic
distribution using a simultaneous Massart-DKW empirical-CDF lower band. The
formal action remains the first global shortest closed order-statistic window.
Fixed-target MT-RQR MCMC cannot override the calibrated target through
`mcmc_args`, and full-range empirical actions fail closed for MCMC.

The remaining risks are operational and comparative rather than P0 correctness
risks:

1. A hardened pilot must prove that the new calibration metadata, manifests,
   failure accounting, and width-ratio summaries work together in a clean run.
2. Competitors must be added only through a documented registry and optional
   wrappers. External methods should not be silently promoted when their
   assumptions, package versions, or certificates are unknown.
3. Full-pilot and confirmatory execution must remain separate decisions. A
   pilot may justify the next implementation stage, but it is not manuscript
   performance evidence by itself.

## Stage Gates

### Stage 1: Hardened Pilot

Run `pilot` from a clean committed source state at `main@8d7b959`. The run
must write under ignored `application/outputs/tcsp_validation_v1`.

Required pass conditions:

- run manifest verifies by byte count and SHA-256;
- recorded source commit is `8d7b959aa664a491448e814f6939cd246ad6a3d3`;
- recorded source status is clean for tracked validation source;
- MC calibration health uses `massart_dkw_empirical_cdf_lower_band`;
- DKW infeasible cells remain failures in denominators;
- audit bundle includes `width_ratio_summary.csv`;
- confirmatory execution remains disabled.

### Stage 2: Pilot Audit Decision

If the pilot audit fails a required gate, stop and repair the failing source
contract before adding competitors. If it passes required gates but has
promotion blockers, document them and keep the run as a pilot-only diagnostic.

### Stage 3: Essential Competitor Wiring

Only after the hardened pilot passes, add a benchmark registry and wrappers for
methods whose implementation can be tested locally. The initial essential
scope is:

- exact/order-statistic Wilks variants already present in the harness;
- optional `tolerance` package wrappers for Young-Mathew, Wald, Hahn-Meeker,
  and normal-family tolerance intervals when the package is installed;
- explicit disabled entries for methods without a validated local dependency,
  including calibrated BNP Gibbs comparisons.

No wrapper may be treated as a formal competitor unless it records method id,
source package/version, target content, tolerance confidence, assumptions,
certificate status, and failure behavior.

### Stage 4: Wrapper Smoke Validation

Run unit tests and a tiny validation grid with any newly enabled wrappers.
The tiny run checks API normalization, failure accounting, and manifest wiring;
it is not performance evidence.

## Deferred

The following remain out of scope for this plan:

- full-pilot relaunch;
- confirmatory config authorization;
- exact scan recursion;
- Bayesian BNP/`gibbsTI` comparisons;
- manuscript performance claims;
- article title/model/loss renaming.

Those decisions should happen only after the hardened pilot and wrapper smoke
stage are reviewed.
