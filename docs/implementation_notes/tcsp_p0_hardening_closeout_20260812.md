# TCSP P0 Hardening Closeout

Date: 2026-08-12
Status: implemented on `feature/tcsp-p0-hardening-20260812`
Source review input: local `report2.md` from PRO

## Verdict

The PRO report was directionally correct on the main promotion risk, but too
broad to implement as written. The essential change was to harden the TCSP
calibration and fixed-target MCMC contract before launching any new full pilot.
The pre-hardening full pilot was stopped and should be treated as obsolete
runtime evidence only.

No confirmatory or full-pilot relaunch is authorized by this note. The next run
must start from a committed source state that includes this hardening pass.

## Implemented Decisions

1. Monte Carlo scan calibration now simulates the Uniform scan statistic
   distribution once per `n`/content cell and applies a simultaneous
   Massart-DKW empirical-CDF lower band over all retained-count candidates.
   This replaces post-selection pointwise Clopper-Pearson wording and avoids
   treating adaptive `k` selection as if it were pointwise certified.

2. `rqr_tcsp_scan_distribution()` and `rqr_tcsp_scan_cdf_band()` expose the
   calibration objects used by `rqr_tcsp_scan_probability()` and
   `rqr_tcsp_calibrate_count()`. Calibration metadata records the band method,
   radius, numerical confidence, histogram digest, and CDF-band digest.

3. TCSP fixed-target MCMC now rejects reserved `mcmc_args` names that would
   override the calibrated target: `y`, `X`, `coverage_level`, `mean_tilt`,
   `learning_rate`, `learning_rate_mode`, `beta_prior_obj`, and
   `response_likelihood`.

4. If the formal empirical action has `target_content >= 1`, the action remains
   valid as an empirical order-statistic window but `fit_mcmc = TRUE` fails
   closed before calling `rqr_mcmc_fit()`, which requires a coverage level in
   `(0, 1)`.

5. The current manuscript already has the correct signed shortest-path
   endpoint derivative geometry on `origin/main`. The stale Monte Carlo
   calibration wording was updated to the simultaneous-band contract.

6. The validation harness now carries MC band metadata through
   `critical_counts` and audit health outputs. Audit bundles also include
   `width_ratio_summary.csv`, which reports within-cell paired width ratios and
   differences against the oracle reference instead of relying on global raw
   width averages.

## Deferred Decisions

The following PRO suggestions remain deferred until after the hardened tiny and
pilot runs pass:

- exact scan recursion;
- Young-Mathew, Wald, and Hahn-Meeker wrappers;
- `gibbsTI` or calibrated BNP Gibbs comparisons;
- broad DGP expansion beyond the tracked iid univariate validation config;
- confirmatory authorization;
- manuscript performance claims from simulation output.

## Next Launch Boundary

The next valid execution order is:

1. run focused TCSP tests and package checks from this branch;
2. commit and push this hardening pass;
3. relaunch `tiny` and then `pilot` from the committed source;
4. audit the pilot bundle;
5. only then decide whether a new full pilot is justified.

The stopped pre-hardening full pilot must not be promoted or merged into a
manuscript evidence bundle.
