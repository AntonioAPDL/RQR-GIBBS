# Publication protocol for the 95% oracle-tilt illustrations

Date: 2026-07-31

## Purpose and inferential scope

This protocol governs two single-data illustrations: a fixed-design ridge
root regression and a fixed-
\(W\) dynamic linear root model. Each illustration compares ordinary RQR,
equal-tailed recovery, and shortest-interval recovery at content
\(c=0.95\). The recovery tilts are calculated exactly from the known
population innovation law. Cornish--Fisher approximations are not used.

The fits are generalized-posterior updates based on the declared RQR loss.
They do not specify a response likelihood, do not produce posterior-predictive
response draws, and do not constitute a repeated-sample simulation study.
Finite-sample endpoint error and realized coverage are descriptive properties
of the two frozen data sets, not calibration or superiority claims.

## Frozen design

Both data sets use standardized asymmetric-Laplace innovations with quantile
index \(0.99\). The fixed-design example has \(n=540\), a quadratic mean, a
smoothly changing response scale, and a ridge prior with variance parameter
250. The dynamic example has 100 times, a local-linear state, fixed evolution
covariance, and missing responses at times 35, 36, and 70. All three dynamic
targets share the initial prior

\[
C_0=\operatorname{diag}(4,0.001).
\]

This common prior is essential: target comparisons must not be confounded by
different state priors.

The complete grid contains 27 chains:

| Family | Targets | Chains per target | Burn-in | Retained per chain |
|---|---:|---:|---:|---:|
| Fixed design | 3 | 4 | 1,000 | 6,000 |
| Dynamic linear roots | 3 | 5 | 2,000 | 6,000 |

The five dynamic initializations are default, oracle-centered, narrow, wide,
and slope-stress. The slope perturbation is determined by the frozen prior
scale and is identical across targets. It is therefore a mixing stress test,
not a target-specific intervention.

## Execution and provenance contract

`application/scripts/34_run_oracle_tilt_publication.R` has separate
`preflight` and `execute` modes. Execute mode requires:

1. a clean `main` checkout at a complete expected commit;
2. an isolated `rqrgibbs` runtime built from that commit;
3. a verified runtime attestation;
4. the explicit environment confirmation
   `RQR_ORACLE_TILT_PUBLICATION_CONFIRM=YES`; and
5. the frozen tracked configuration
   `application/config/oracle_tilt_c095_publication_20260731.json`.

Each worker result is written atomically and bound to the source commit,
configuration hash, runtime-tree digest, family, target, chain, initialization,
and seed. A valid worker envelope can be resumed; a mismatched envelope is
recomputed. Raw worker objects remain under ignored output roots.

## Validation and disposition

The strict diagnostic thresholds are rank-normalized
\(\widehat R\le 1.05\), bulk ESS at least 400, tail ESS at least 200, and
MCSE/SD at most 0.10. Every fit must also have zero numerical repairs, an exact
fixed-joint target, matching isolated-runtime provenance, and promotion-
eligible package metadata. Dynamic fits additionally require the selected
conditional Gaussian checks to agree with dense references and must show no
remote-scale pathology.

A cell receives one of three dispositions:

- `strict_pass`: every hard and ESS gate passes;
- `illustration_warning_ess_only`: every hard gate, R-hat gate, MCSE gate,
  conditional reference, and pathology gate passes, but at least one ESS
  threshold fails; or
- `fail`: a hard validity requirement fails.

Only the first two dispositions may contribute to an illustration, and the
second must be disclosed. Endpoint RMSE, width RMSE, realized coverage, and
visual appearance are never used to waive a hard gate.

## Evidence and rendering

`application/scripts/35_package_oracle_tilt_evidence.R` accepts only a closed
run with all six family/target cells passing the hard gate. It copies an
explicit compact allowlist to `figures/data/oracle_tilt_c095/`, removes local
runtime paths, and writes byte counts and SHA-256 hashes. Raw chains, full fit
objects, latent variables, and traces are excluded.

`figures/generate_oracle_tilt_model_figures.R` verifies every evidence hash
before rendering. It cannot fit a model. The renderer produces vector PDF
figures for the article and supplement, PNG previews, a compact summary table,
and a figure manifest. Thus a document build is deterministic from tracked
compact evidence and never launches MCMC.

## Promotion sequence

1. Validate the source and commit it.
2. Build the isolated runtime from the exact clean commit.
3. Run preflight and the 27-chain execution.
4. Audit the six dispositions and the compact artifact manifest.
5. Package the compact evidence.
6. Render the figures and table from that evidence.
7. Update the manuscript with the exact disposition and evidence scope.
8. Rebuild both TeX documents and run the package and focused test gates.

No matched simulation, empirical coverage study, automatic tilt selection, or
response-predictive interpretation is authorized by this protocol.
