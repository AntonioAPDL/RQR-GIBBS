# Preliminary ADEMP specification for the matched RQR-DLM simulation.
# This object is for independent review and implementation planning only.
# It cannot authorize a pilot, confirmatory run, or manuscript claim.

rqr_dlm_main_simulation_preliminary <- list(
  schema_version = "rqrgibbs_dlm_main_simulation_preliminary/0.1.0",
  config_id = "rqr_dlm_main_simulation_preliminary_20260724",
  status = "draft_for_independent_review",
  production_simulation_authorized = FALSE,
  pilot_execution_authorized = FALSE,
  generalized_bayes = TRUE,
  response_likelihood = FALSE,
  response_prediction_contract = FALSE,
  interval_root_summary = "posterior_mean_of_ordered_root_functions",
  scope = list(
    primary = "RQR-DLM MCMC operating characteristics",
    includes = c(
      "fixed-design RQR ablation",
      "quantile-derived dynamic and static intervals",
      "empirical interval baseline",
      "optional time-series-valid conformal sensitivity"
    ),
    excludes = c(
      "RQR-DESN",
      "CAVI or ELBO",
      "production application",
      "response-density or posterior-predictive claims"
    )
  ),
  aims = c(
    "assess held-out RQR loss and interval coverage under symmetric and asymmetric conditional distributions",
    "compare interval width only at comparable empirical coverage",
    "assess recovery of population RQR roots when those roots are known",
    "isolate the value of dynamic roots and component-specific evolution",
    "measure sensitivity to the generalized-Bayes learning-rate convention",
    "report numerical failures, Monte Carlo error, and computation without deleting failed fits"
  ),
  design = list(
    coverage_levels = c(0.80, 0.90),
    core_training_horizon = 200L,
    core_forecast_horizon = 20L,
    reported_forecast_horizons = c(1L, 5L, 10L, 20L),
    sensitivity_training_horizons = c(100L, 400L),
    training_only_standardization = TRUE,
    common_random_numbers_across_methods = TRUE,
    test_data_used_for_tuning = FALSE,
    primary_dgp_ids = c(
      "static_gaussian_negative_control",
      "local_level_gaussian",
      "local_level_skewed",
      "trend_seasonal_skewed",
      "trend_regression_unequal_evolution",
      "structural_break_heavy_tail_stress"
    ),
    sensitivity_dgp_ids = c(
      "heteroscedastic_known_scale_covariate",
      "independent_root_prior_alignment"
    )
  ),
  estimands = c(
    "population RQR lower and upper roots",
    "RQR-root midpoint and width",
    "marginal held-out coverage",
    "forecast-horizon-specific coverage",
    "population equal-tailed endpoints for comparator interpretation"
  ),
  methods = list(
    primary = c(
      "rqr_dlm_component_scale_fixed_rate",
      "dynamic_equal_tailed_quantile_interval"
    ),
    ablations = c(
      "rqr_dlm_frozen_component_discount",
      "fixed_design_rqr",
      "static_equal_tailed_quantile_regression"
    ),
    simple_baseline = "rolling_empirical_equal_tailed_interval",
    oracle_references = c(
      "population_rqr_roots",
      "population_equal_tailed_roots",
      "rqr_dlm_true_fixed_W_selected_cells"
    ),
    optional_sensitivity = c(
      "rqr_dlm_component_scale_learned_normalized_rate",
      "gaussian_dlm_response_interval",
      "time_series_valid_conformal_interval"
    )
  ),
  performance_measures = list(
    primary = c(
      "mean held-out RQR loss",
      "empirical coverage and signed coverage error",
      "width among methods meeting the coverage-comparability rule",
      "lower-root RMSE",
      "upper-root RMSE"
    ),
    secondary = c(
      "midpoint RMSE",
      "width RMSE",
      "coverage by horizon and predeclared scale/state strata",
      "central interval score with equal-tailed-target caveat",
      "fit failure rate",
      "elapsed time and peak sampled memory",
      "MCMC ESS per second on a diagnostic subset"
    ),
    forbidden = c(
      "response log score for RQR",
      "response posterior-predictive density score for RQR",
      "unqualified width ranking when coverage differs"
    )
  ),
  monte_carlo = list(
    framework = "ADEMP",
    pilot_replications_per_core_cell = 25L,
    confirmatory_minimum_replications = 500L,
    confirmatory_batch_size = 250L,
    confirmatory_maximum_replications = 2500L,
    stopping_uses_performance_rank = FALSE,
    stopping_uses_monte_carlo_precision_only = TRUE,
    target_coverage_mcse = 0.01,
    report_mcse_for_every_primary_summary = TRUE,
    paired_method_contrasts = TRUE,
    replication_is_independent_unit = TRUE
  ),
  mcmc = list(
    production_schedule_frozen = FALSE,
    diagnostic_pilot_chains = 4L,
    candidate_confirmatory_chains_per_fit = 1L,
    preselected_multichain_sentinel_fraction = 0.05,
    backend = "cpp",
    numerical_policy = "fail",
    no_post_failure_reseed = TRUE,
    no_outcome_driven_chain_extension = TRUE,
    fixed_rate_primary = TRUE,
    learned_rate_is_sensitivity_not_calibration = TRUE
  ),
  tuning = list(
    uses_training_data_only = TRUE,
    fixed_before_test_evaluation = TRUE,
    discount_grid_preliminary = c(0.90, 0.95, 0.98, 0.99),
    generalized_rate_sensitivity_preliminary = c(0.5, 1, 2),
    primary_standardized_scale_rate = 1,
    oracle_settings_excluded_from_competitive_rankings = TRUE
  ),
  reproducibility = list(
    exact_source_commit_required = TRUE,
    isolated_primary_runtime_required = TRUE,
    protected_exdqlm_checkout_read_only = TRUE,
    full_seed_ledger_required = TRUE,
    atomic_replication_artifacts_required = TRUE,
    heavy_fit_objects_ignored = TRUE,
    compact_results_and_recursive_hashes_tracked = TRUE,
    failed_fits_retained_in_denominator = TRUE
  ),
  review_questions = c(
    "Is the location-scale DGP family sufficient to identify the RQR-specific message without favoring the method?",
    "Should the core dynamic comparator use CRAN exdqlm 1.1.0 DQLM or a separately validated native implementation?",
    "Is one chain per confirmatory replication plus preselected multichain sentinels adequate after the bounded validation?",
    "Are the replication bounds and Monte Carlo precision targets proportionate to the computational cost?",
    "Should a time-series-valid conformal method be core, sensitivity-only, or omitted?",
    "Which discount-selection rule is fair and reproducible across component structures?"
  )
)
