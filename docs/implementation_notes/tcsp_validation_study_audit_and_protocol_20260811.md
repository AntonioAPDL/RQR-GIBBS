# TCSP Validation Study Audit and Protocol

Date: 2026-08-11
Schema: `rqrgibbs_tcsp_validation_study_protocol/1.0.0`
Status: implementation protocol, pilot harness, and full-pilot wiring

## Audit Summary

The repository is ready to implement a TCSP validation study, but not to
promote a heavy confirmatory run without a frozen validation contract. The
core TCSP action is implemented as a closed shortest order-statistic window
after a retained-count calibration. The manuscript and proof ledger correctly
separate the formal tolerance action from generalized-posterior endpoint
summaries. The existing DLM and oracle-tilt infrastructure shows the preferred
repo pattern: frozen configs, deterministic seed streams, explicit execution
modes, source-state records, artifact manifests, and fail-closed promotion
gates.

The TCSP study should therefore start with iid univariate repeated-sample
validation. Regression, dynamic, DESN, posterior-action equivalence, and exact
finite-sample scan-theorem promotion remain out of scope. This is the optimal
first validation layer because the formal TCSP certificate is currently
univariate and distribution-free, while all wider model families still require
additional theory or externally validated protocols.

## Diagnosed Risks

1. The validation target can be confused with ordinary predictive coverage.
   The implemented protocol evaluates true population content
   \(F(U)-F(L)\), not posterior-predictive response coverage.
2. A Monte Carlo scan critical count can be mislabeled as exact or certified
   only pointwise after selection. The protocol records the numerical
   confidence level and uses a simultaneous Massart-DKW empirical-CDF lower
   band over the simulated Uniform scan-statistic distribution.
3. Competitor formulas can be under-specified. The current implementation
   includes exact beta-spacing Wilks competitors, a declared normal-theory
   approximation, and an oracle width reference. Young-Mathew and calibrated
   BNP Gibbs competitors are explicitly disabled until a tracked exact
   implementation or vetted dependency is added.
4. Heavy runs can drift if launched from mutable scripts. The new config keeps
   confirmatory execution disabled and requires a new config before production.
5. Generated output can pollute source control. Validation outputs are written
   under `application/outputs/tcsp_validation_v1`, which is a local-only
   workspace under the repository policy.

## Implemented Design

The frozen config is
`application/config/tcsp_validation_v1.json`.

Primary DGPs:

- standard normal;
- centered standardized lognormal;
- variance-standardized Student \(t_5\);
- standardized separated normal mixture;
- bounded right-skewed beta.

The pilot sample-size grid includes \(n=80,250,600\). This is deliberate:
the DKW scan fallback is very conservative for high content and high tolerance
confidence, so very small samples mostly test fail-closed behavior rather than
usable intervals. The grid keeps \(n=80\) as an infeasibility stress point and
adds larger sizes where DKW-calibrated TCSP actions can be exercised.
The default pilot is a resource rehearsal: one tolerance-confidence level,
three DGPs, four data-dependent methods, eight replications, and a modest
Monte Carlo scan-calibration budget. The population oracle width reference
remains in preflight and tiny checks. The full-pilot mode expands the
calibration budget, DGP grid, confidence grid, method grid, and replication
schedule after the compact pilot audit.

Active methods:

- TCSP shortest window with DKW calibration;
- TCSP shortest window with conservative Uniform Monte Carlo calibration;
- exact symmetric Wilks/order-statistic interval;
- Wilks min-max interval;
- equal-tailed empirical interval at the TCSP-DKW retained count;
- normal-theory Howe approximate interval;
- population shortest interval as an oracle width reference.

Disabled methods:

- Young-Mathew interpolation, because an exact tracked implementation is not
  present and `tolerance` is not a declared dependency;
- calibrated Bayesian nonparametric Gibbs tolerance intervals, because no
  repository implementation or validated learning-rate calibration is present.

Primary metrics:

- true content \(F(U)-F(L)\);
- tolerance success indicator \(1\{F(U)-F(L)\ge c\}\);
- one-sided binomial lower confidence bound for repeated-sample success;
- width mean, median, and upper quantile;
- lower and upper omitted mass;
- content deficit;
- failure and infeasibility rates;
- runtime.

## Execution Stages

1. `preflight`: validates the config, writes DGP and method contracts, records
   the design grid, and computes non-random critical-count contracts.
2. `tiny`: runs a small repeated-sample end-to-end validation over a reduced
   grid and writes replication-level and cell-level summaries.
3. `pilot`: runs the configured pilot grid. This is empirical validation
   evidence only; it does not promote a theorem.
4. `full_pilot`: runs the expanded iid univariate pilot grid with all tracked
   DGPs, both tolerance-confidence levels, a larger MC scan-calibration budget,
   and `n=1200` added for high-content DKW diagnostics.
5. `health-check-read-only`: verifies the artifact manifest of an existing run.
6. `audit`: publishes a compact source-controlled audit bundle from a closed
   run while leaving raw replication outputs local-only.
7. Confirmatory execution remains fail-closed and requires a new config after
   pilot review.

## Reproducibility Contract

Each run writes:

- `config.json`;
- `source_state.json`;
- `design_grid.csv`;
- `critical_counts.csv`;
- `replication_results.csv` for repeated-sample modes;
- `cell_summary.csv` for repeated-sample modes;
- `failure_log.csv`;
- `closeout.json`;
- `artifact_manifest.csv` with SHA-256 checksums.

The deterministic seed contract separates calibration, data, method, and audit
streams. The same dataset is reused across methods for each DGP, sample size,
and replication.

## Why This Is The Right Next Step

The current TCSP theorem ledger still has the exact finite-sample scan theorem
and posterior-action equivalence marked as blocking. A validation study cannot
close those proofs, but it can determine whether the implemented action,
critical-count choices, and competitors behave as intended under known DGPs.
Starting with iid univariate DGPs matches the scope of the formal action and
prevents premature claims about regression or dynamic tolerance. The pilot
harness is therefore the highest-value next implementation: it provides
auditable empirical evidence while preserving the article's current proof
discipline.
