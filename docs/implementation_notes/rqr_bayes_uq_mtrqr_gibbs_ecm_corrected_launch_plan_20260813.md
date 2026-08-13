# Corrected MTI Gibbs/ECM Bayesian-UQ Launch Plan

Date: 2026-08-13

Branch audited: `feature/bayesian-uq-authoritative-report6-20260812`

Current HEAD audited: pre-commit working tree after correcting the MTI
Gibbs/ECM launcher and MC terminal-range calibration.

Active run audited:
`application/runs/rqr_bayes_uq_validation_main_20260813/wave_main_20260813T045254Z`

## Executive Decision

The active main run should be treated as a superseded scan/DP baseline run, not
as the final MTI comparison.  It launched the scan-calibrated TCSP action but
did not launch explicit MTI Gibbs or MTI ECM/EM competitors.

The corrected launch should add explicit method IDs for the fixed-target
MTI Gibbs and ECM layers after scan calibration, while preserving the
distinction between:

1. scan-certified tolerance actions;
2. fixed-target generalized-Bayes posterior or MAP summaries;
3. response-likelihood Bayesian distribution models.

This is the optimal path because it fixes the actual omission without weakening
the current theory.  The scan-certified empirical interval remains the only
finite-sample TCSP action.  Gibbs and ECM summaries are evaluated as fitted
diagnostic competitors unless and until a separate theorem or calibration study
promotes them to formal tolerance actions.

## Audit Findings

| Area | Current State | Diagnosis | Required Correction |
|---|---|---|---|
| Active launch methods | `oracle_sh`, `hdp_s_mc`, `tcsp_mc`, `tcsp_dkw`, `split_empirical_shortest`, `wilks_minmax`, `bb_shortest_diag` | No explicit fixed-target MTI Gibbs or ECM rows are in the confirmatory grid. | Add explicit Gibbs/ECM method IDs to config and runner. |
| TCSP Gibbs implementation | `rqr_tcsp_fit_univariate(..., fit_mcmc = TRUE)` calls `rqr_mcmc_fit()` at calibrated `q=k/n` and frozen tilt. | Implemented and contract-tested, but not used by `69_validate_rqr_bayes_uq.R`. | Wire launcher methods to this path or an equivalent audited helper. |
| TCSP ECM implementation | `rqr_tcsp_fit_univariate(..., fit_ecm = TRUE)` calls `rqr_ecm_fit()` at calibrated `q=k/n` and frozen tilt. | Implemented and contract-tested, but not used by the main launch. | Add ECM method rows and runner extraction of MAP interval summaries. |
| Split exact-spacing ECM | `rqr_tcsp_split_exact_fit(..., pilot_method = "ecm_fixed_tilt")` exists and smoke tests passed. | The active main run only uses `split_empirical_shortest`. | Add `split_ecm_fixed_tilt` after a smoke runtime gate. |
| Action semantics | Current result columns only have one `lower/upper/width` interval. | That is insufficient once a method has both a formal scan guard and a fitted posterior/MAP summary. | Add fields for `selected_interval_source`, `formal_action_*`, and `fitted_summary_*`. |
| Posterior thresholds | Data are paired across posterior-confidence values, but many methods do not depend on posterior confidence. | Adding MCMC without caching would repeat the same expensive fit across posterior thresholds. | Cache fixed-target fits by dataset/method/target, independent of posterior threshold where appropriate. |
| Oracle tilt | `oracle_sh` stores the population shortest interval and exact mean tilt. Fitted TCSP currently uses sample shortest-window tilt. | For simulation, oracle tilt is useful as a non-deployable diagnostic; for application, sample/fitted tilt is deployable. | Include oracle-tilt Gibbs/ECM as optional diagnostic variants or columns, not as deployable methods. |
| MC high-content calibration | MC scan calibration at `n=500`, `c=0.99`, confidence `0.95` can fail to certify the terminal full-range event under a finite simulation CDF band. | The terminal event has an exact Uniform range formula, so treating it as MC-infeasible is unnecessarily conservative. | Implement exact terminal-range rescue for MC at `k=n`; keep DKW as the stress comparator. |
| DGP coverage | Current grid covers symmetric, skewed, hard skewed, mixture, sharp mixture, contamination, and Student-t. | It lacks bounded/support-boundary and near-nonunique shortest cases from the PRO report. | Add only after extending CDF/oracle support and tests. |
| DPM/DP scope | Direct DP hybrid is in main; standalone DP/DPM are in companion mode. | This is reasonable for runtime. It does not address the MTI Gibbs/ECM omission. | Keep DPM/DP companion separate; do not block corrected MTI relaunch on DPM expansion. |

## Corrected Method Taxonomy

### Primary scan-certified action lane

These rows are formal tolerance actions or non-deployable references:

- `oracle_sh`: true population shortest interval and exact mean tilt.
- `tcsp_mc`: empirical shortest window with conservative MC scan calibration.
- `hdp_s_mc`: direct-DP hybrid Bayesian-scan action using the same scan guard.
- `split_empirical_shortest`: split exact-spacing empirical pilot action.
- `split_ecm_fixed_tilt`: split exact-spacing action with ECM pilot placement.
- `wilks_minmax`: full-range comparator.
- `tcsp_dkw`: DKW stress comparator, expected to be infeasible often at high
  content.

### Fixed-target MTI diagnostic competitor lane

These rows use the two-stage target construction:

1. scan calibration determines `k` and `q=k/n`;
2. the shortest empirical window determines the fixed target and tilt;
3. Gibbs or ECM fits the fixed MTI generalized-Bayes target.

Recommended explicit method IDs:

- `tcsp_mti_gibbs_median_mc`: fixed-target MTI Gibbs; evaluated interval is
  the posterior median endpoint summary.
- `tcsp_mti_gibbs_mean_mc`: fixed-target MTI Gibbs; evaluated interval is
  the posterior mean endpoint summary. Keep this in smoke/moderate unless it is
  materially different from the median summary.
- `tcsp_mti_ecm_map_mc`: fixed-target MTI ECM/MAP; evaluated interval is
  `predict_interval.rqr_ecm()` on the intercept-only design.

All three must carry:

- `formal_tolerance_action = false`;
- `generalized_bayes = true`;
- `response_likelihood = false`;
- `scan_calibration_used = true`;
- `posterior_endpoint_coverage_claim_available = false`;
- `selected_interval_source` equal to the fitted summary source;
- `formal_action_lower`, `formal_action_upper`, and `formal_action_width` equal
  to the underlying TCSP scan action for auditability.

### Oracle-tilt diagnostic lane

For simulation only, add optional variants:

- `tcsp_mti_gibbs_median_oracle_tilt_mc`;
- `tcsp_mti_ecm_map_oracle_tilt_mc`.

These use the population `oracle_sh` mean tilt after the same scan calibration.
They are non-deployable and should be used to isolate algorithmic behavior from
sample-tilt variability.  They should be in smoke and moderate runs first.  If
they are stable and informative, include them in the final main run as
diagnostics, not as deployable competitors.

## Runner Changes

1. Bump the Bayesian-UQ validation schema to `rqrgibbs_bayes_uq_validation/1.2.0`.
2. Add method metadata fields:
   - `action_lane`;
   - `selected_interval_source`;
   - `scan_method`;
   - `uq_engine`;
   - `tilt_source`;
   - `deployable`;
   - `formal_tolerance_action`.
3. Extend `scan_method_for()` so all `*_mc` scan-calibrated MTI methods share
   the same MC scan calibration cache as `tcsp_mc`.
4. Add a helper in the worker, for example `fit_tcsp_mti_plugin_method()`, that:
   - retrieves the cached scan calibration;
   - returns fail-closed infeasible rows if `k > n` or `q >= 1`;
   - computes the empirical shortest window;
   - chooses sample tilt or oracle tilt according to method metadata;
   - fits `rqr_tcsp_fit_univariate(..., fit_mcmc = TRUE)` or
     `rqr_tcsp_fit_univariate(..., fit_ecm = TRUE)`, or calls the fixed-target
     engines directly with an equivalent post-fit target audit;
   - extracts fitted endpoint summaries;
   - records the unchanged scan-certified formal action.
5. Add result columns:
   - `selected_interval_source`;
   - `formal_action_lower`, `formal_action_upper`, `formal_action_width`;
   - `fitted_summary_lower`, `fitted_summary_upper`,
     `fitted_summary_width`;
   - `uq_engine`;
   - `tilt_source`;
   - `target_content`;
   - `target_mean_tilt`;
   - `posterior_draws`;
   - `mcmc_n_burn`, `mcmc_n_mcmc`, `mcmc_thin`;
   - `ecm_converged`, `ecm_iterations`, `ecm_objective`;
   - `target_audit_digest`;
   - `fit_reused_across_posterior_thresholds`.
6. Cache Gibbs/ECM fits within each worker by
   `(dgp_id, n, c, tolerance_confidence, replication, method_id, tilt_source)`.
   Do not rerun the same MCMC three times only because `posterior_confidence`
   changes.
7. Keep `lower`, `upper`, and `width` as the evaluated interval for summary
   metrics, but make the source explicit through `selected_interval_source`.
8. For MC scan calibration, use the exact terminal Uniform range certificate
   for the `k=n` candidate. This is a finite-sample certificate for the
   full-range action only; it is not promoted as exact scan recursion.

## Config Changes

The corrected confirmatory method grid should be:

```text
oracle_sh
hdp_s_mc
tcsp_mc
tcsp_mti_gibbs_median_mc
tcsp_mti_ecm_map_mc
split_empirical_shortest
split_ecm_fixed_tilt
wilks_minmax
bb_shortest_diag
tcsp_dkw
```

Optional, after smoke:

```text
tcsp_mti_gibbs_median_oracle_tilt_mc
tcsp_mti_ecm_map_oracle_tilt_mc
```

Recommended fixed-target controls for the first corrected smoke:

```json
{
  "mti_gibbs": {
    "learning_rate": 1,
    "mcmc_control": {"n_burn": 20, "n_mcmc": 40, "thin": 2}
  },
  "mti_ecm": {
    "learning_rate": 1,
    "ecm_control": {
      "max_iter": 80,
      "stable_iterations": 2,
      "tol_stationarity": 1e-6
    }
  }
}
```

Main-run MCMC controls should be chosen only after the corrected smoke reports
runtime and stability.  A reasonable starting target is enough posterior draws
to stabilize median endpoints, not a full posterior endpoint-coverage claim.

## DGP Plan

Keep the current seven DGPs for continuity:

- normal;
- lognormal;
- lognormal hard;
- separated mixture;
- sharp separated mixture;
- contaminated normal;
- Student `t_3`.

Add a second DGP patch only if tests are added at the same time:

- centered exponential or centered gamma support-boundary case;
- asymmetric Laplace skewed case;
- one near-nonunique shortest case, preferably a flat or nearly flat density
  over a central region.

The first two can reuse existing oracle-family support after adding
`dgp_meta()` and `oracle_spec_from_dgp()` mappings.  The near-nonunique case
needs an oracle audit before it enters confirmatory mode.

## Tests And Gates

Required unit tests before any corrected launch:

1. Main config contains the MTI Gibbs and ECM method IDs.
2. `scan_method_for()` maps all `*_mc` MTI methods to
   `monte_carlo_conservative`.
3. Worker smoke produces at least one row with `fit_class` containing
   `rqr_mcmc`.
4. Worker smoke produces at least one row with `fit_class` containing
   `rqr_ecm`.
5. Gibbs/ECM rows record both fitted summaries and formal TCSP action columns.
6. Target audits verify `q`, learning rate, response-likelihood flag, prior
   type, intercept-only design, and mean tilt.
7. `q >= 1` cells fail closed for Gibbs/ECM fitting while preserving the
   scan-action metadata.
8. Repeated posterior thresholds reuse fixed-target Gibbs/ECM fits or otherwise
   record why reuse was impossible.
9. `split_ecm_fixed_tilt` is present in smoke output and has no target override.
10. Updated README/launch docs state that MTI Gibbs/ECM rows are diagnostic
    fitted summaries, not scan-certified tolerance actions.

Required run gates:

```sh
make test-tcsp
make test-ecm
make test-bayes-uq
make rqr-bayes-uq-main-smoke
```

Then run a corrected mini-pilot:

```sh
Rscript application/scripts/69_validate_rqr_bayes_uq.R \
  --mode=smoke \
  --config=application/config/rqr_bayes_uq_validation_main_20260813.json \
  --output-dir=application/outputs/rqr_bayes_uq_validation_main_20260813/smoke_mti_gibbs_ecm_<stamp>
```

Only after the smoke passes should the old active run be stopped and the new
wave run prepared.

## Active Run Handling

The current active run should not be merged into article evidence.  It can be
kept as a baseline diagnostic because it is still early and contains valid
scan/DP rows, but it should be marked superseded before starting the corrected
launch.

Stop sequence:

1. record current `wave_status.csv`, `health.json`, and scheduler PID;
2. terminate the scheduler process;
3. terminate active wave worker PIDs listed in `pids/`;
4. write a local ignored supersession note under the run directory;
5. do not collect or promote the partial artifacts as confirmatory evidence.

The current wave manager has no explicit stop action, so stopping must be done
with process management unless a stop action is implemented first.  A stop
action is preferable and should be added if time permits.

## Relaunch Sequence

1. Implement the method-grid and runner corrections.
2. Add tests and update launch documentation.
3. Run the smoke gates.
4. Stop and mark the active run superseded.
5. Prepare a fresh run directory using the corrected config.
6. Launch with conservative concurrency until MCMC runtime is measured.
7. Health-check after the first few waves.
8. If Gibbs runtime is prohibitive, reduce Gibbs rows to smoke/moderate and keep
   ECM plus split-ECM in the main run.
9. Collect only after all corrected waves finish with zero failed waves.
10. Update manuscript/supplement language after results are available.

## Implementation Status

Implemented in the working tree:

- corrected config schema `rqrgibbs_bayes_uq_validation/1.2.0`;
- confirmatory method grid with `tcsp_mti_gibbs_median_mc`,
  `tcsp_mti_ecm_map_mc`, and `split_ecm_fixed_tilt`;
- worker result columns for formal scan actions, fitted summaries, target
  audits, MCMC draw counts, ECM diagnostics, and fit reuse;
- worker caching of fixed-target MTI fits across paired posterior thresholds;
- explicit split exact-spacing fail-closed rows for exact-spacing infeasibility;
- exact MC terminal-range rescue for `k=n`;
- wave-manager stop action plus `make stop-rqr-bayes-uq-main`.

Validation completed before launch:

- `make package-install`;
- `Rscript application/scripts/66_run_testthat_file_strict.R application/tests/testthat/test-rqr-tcsp-scan-calibration.R`;
- `Rscript application/scripts/66_run_testthat_file_strict.R application/tests/testthat/test-rqr-bayes-uq-main-validation-config.R`;
- `make test-tcsp`;
- `make test-bayes-uq`;
- corrected smoke output under
  `application/outputs/rqr_bayes_uq_validation_main_20260813/smoke_20260813T062551Z`;
- direct `n=500`, `c=0.99` contract check under
  `application/outputs/rqr_bayes_uq_validation_main_20260813/n500c99_contract_20260813T064734Z`;
- corrected preflight under
  `application/runs/rqr_bayes_uq_validation_main_20260813/wave_main_corrected_terminal_20260813T064601Z`.

The corrected preflight has `42` waves, `12,600` datasets, and `126,000`
expected result rows. MC scan calibration is feasible for all configured
`n/content` cells after the terminal rescue. DKW remains infeasible for
`n=500` at all three contents and for `n=1000` at `c=0.95` and `c=0.99`, as a
conservative stress comparator.

## Manuscript Integration Plan

Do not rewrite claims before the corrected evidence exists.  After the corrected
run completes:

- Methods section: explicitly separate scan-certified TCSP, MTI Gibbs
  plug-in summaries, MTI ECM/MAP summaries, and response-likelihood DP/DPM
  models.
- Simulation section: report validity before width; mark MTI Gibbs/ECM rows
  as diagnostic fitted summaries unless promoted by a separate theorem.
- Supplement: add target-audit tables for `q`, tilt source, learning rate,
  fit class, and scan calibration digest.
- Bibliography: keep Gibbs quantile-regression and calibrated generalized-Bayes
  references, but do not cite them as proof of TCSP posterior endpoint coverage.

## Final Recommendation

Do not continue to spend cluster time on the active seven-method launch as the
final main study.  The corrected launch should be implemented and smoked first,
then the active run should be stopped and superseded.  The corrected main study
must include explicit MTI Gibbs and ECM method IDs, action-source metadata,
fit audits, and cached execution so the article can accurately say which of the
author's methods were tested.
