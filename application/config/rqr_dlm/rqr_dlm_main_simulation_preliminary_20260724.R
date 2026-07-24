# Versioned ADEMP contract for the matched RQR-DLM simulation.
#
# This configuration authorizes deterministic design/reference checks only.
# It cannot authorize a diagnostic pilot or confirmatory simulation.

rqr_dlm_main_simulation_preliminary <- list(
  schema_version = "rqrgibbs_dlm_main_simulation_preliminary/0.2.0",
  config_id = "rqr_dlm_main_simulation_preliminary_20260724",
  status = "design_revision_and_reference_implementation",
  runner_modes_implemented = c(
    "preflight", "oracle-reference", "tiny-end-to-end",
    "diagnostic-pilot-preflight"
  ),
  runner_modes_forbidden = c(
    "diagnostic-pilot", "execute-confirmatory"
  ),
  diagnostic_pilot_execution_authorized = FALSE,
  confirmatory_execution_authorized = FALSE,
  generalized_bayes = TRUE,
  response_likelihood = FALSE,
  response_prediction_contract = FALSE,
  interval_root_summary = "posterior_mean_of_ordered_root_functions",
  scope = list(
    primary = "RQR-DLM MCMC operating characteristics",
    includes = c(
      "fixed-design RQR ablation",
      "dynamic and static equal-tailed quantile intervals",
      "rolling empirical equal-tailed interval",
      "common-evolution RQR-DLM ablation"
    ),
    excludes = c(
      "RQR-DESN", "CAVI or ELBO", "production application",
      "response-density or posterior-predictive claims",
      "conformal methods in the first diagnostic pilot"
    )
  ),
  aims = c(
    "compare target-aligned endpoint recovery for RQR and quantile methods",
    "assess held-out RQR loss, repeated-sampling coverage, and width",
    "separate dynamic tracking from asymmetric-error effects",
    "separate multicomponent dynamics from asymmetric-error effects",
    "identify component-specific evolution through a common-evolution ablation",
    "report failures, Monte Carlo error, computation, and provenance"
  ),
  oracle = list(
    schema_version = "rqrgibbs_rqr_oracle_reference/1.0.0",
    unrestricted_solver = "two_dimensional_multi_start",
    independent_certificate = "coverage_profile_all_detected_basins",
    profile_parameterization =
      "a(u)=F^{-1}(u); b(u)=F^{-1}(u+c); u in [0,1-c]",
    event_boundary_aware_integration = TRUE,
    location_scale_equivariance_required = TRUE,
    record_distribution_digest = TRUE,
    record_solver_digest = TRUE,
    record_coverage_and_moment_residuals = TRUE,
    record_objective_and_global_gap = TRUE,
    record_local_curvature = TRUE,
    record_estimated_quadrature_error = TRUE,
    call_numerical_error_a_rigorous_bound = FALSE,
    unresolved_minimizer_policy =
      "use_excess_population_risk_and_distance_to_minimizer_set",
    endpoint_rmse_requires_unique_minimizer = TRUE
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
    dgp_adjustment_uses_method_performance = FALSE,
    primary_dgp_ids = c(
      "static_gaussian_negative_control",
      "local_level_gaussian",
      "local_level_skewed",
      "trend_seasonal_gaussian",
      "trend_seasonal_skewed",
      "trend_regression_unequal_evolution",
      "structural_break_heavy_tail_stress"
    ),
    sensitivity_dgp_ids = c(
      "heteroscedastic_known_scale_covariate",
      "independent_root_prior_alignment"
    ),
    structural_stress_claim =
      "composite_break_and_heavy_tail_stress_only"
  ),
  estimands = list(
    rqr_method_endpoint_target = "population_RQR_roots",
    quantile_method_endpoint_target =
      "population_equal_tailed_quantile_endpoints",
    common_operating_measures = c(
      "held_out_RQR_loss", "empirical_response_coverage",
      "interval_width", "central_interval_score",
      "fit_failure", "elapsed_time", "sampled_memory"
    ),
    cross_target_measure =
      "quantile_interval_distance_to_RQR_roots",
    cross_target_measure_label = "cross_target_distance_not_bias",
    rqr_loss_role = "RQR_home_target_measure",
    central_interval_score_role =
      "secondary_equal_tailed_targeted_measure"
  ),
  coverage_width = list(
    practical_equivalence_margin = 0.02,
    confidence_level = 0.90,
    qualification = "TOST_interval_wholly_inside_margin",
    narrower_width_requires_both_coverage_qualified = TRUE,
    paired_width_interval_must_support_direction = TRUE,
    coverage_width_frontier_required = TRUE,
    post_hoc_test_width_calibration_forbidden = TRUE,
    nominal_replications_for_margin = c(
      coverage_0.80 = 1083L, coverage_0.90 = 609L
    )
  ),
  methods = list(
    primary = c(
      "rqr_dlm_component_scale_fixed_rate",
      "dynamic_equal_tailed_quantile_interval"
    ),
    ablations = c(
      "rqr_dlm_common_evolution_ablation",
      "rqr_dlm_frozen_component_discount",
      "fixed_design_rqr",
      "static_equal_tailed_quantile_regression"
    ),
    simple_baseline = "rolling_empirical_equal_tailed_interval",
    oracle_references = c(
      "population_RQR_roots", "population_equal_tailed_roots",
      "rqr_dlm_true_fixed_W_selected_cells"
    ),
    sensitivities = c(
      "rqr_dlm_component_scale_learned_rate",
      "rqr_dlm_fixed_rate_0p5", "rqr_dlm_fixed_rate_2",
      "gaussian_dlm_response_interval"
    ),
    conformal_first_diagnostic_pilot = "omitted",
    dynamic_quantile_engine = "CRAN_exdqlm_1.1.0_reduced_AL_DQLM_MCMC",
    static_quantile_engine = "quantreg_rq",
    quantile_crossing_rule =
      "store_raw_lower_upper_then_order_for_interval_scoring",
    external_source = list(
      package = "exdqlm",
      version = "1.1.0",
      url = "https://cran.r-project.org/src/contrib/exdqlm_1.1.0.tar.gz",
      sha256 =
        "51bc968f617721c9ab1dcfc6ec14857d30827fcd36659f3de45337cc3c82bd14",
      protected_checkout_used = FALSE,
      isolated_runtime_required = TRUE
    ),
    static_external_source = list(
      package = "quantreg",
      version = "6.1",
      url = "https://cran.r-project.org/src/contrib/quantreg_6.1.tar.gz",
      sha256 =
        "f42292c5ab25a15f39295b93391deafef192fe09eefde563399a64eba7e0169a",
      isolated_runtime_required = TRUE
    )
  ),
  tuning = list(
    uses_training_data_only = TRUE,
    fixed_before_test_evaluation = TRUE,
    rolling_validation_origins = c(140L, 160L, 180L),
    validation_horizon = 10L,
    common_discount_grid = c(0.90, 0.95, 0.98, 0.99),
    block_specific_discount_grid = c(0.90, 0.95, 0.98, 0.99),
    maximum_discount_combinations = 16L,
    rqr_validation_criterion = "mean_validation_RQR_loss",
    quantile_validation_criterion = "mean_validation_check_loss",
    deterministic_tie_break =
      "largest_discount_then_lexicographic_component_order",
    failed_candidate_rule = "worst_validation_value",
    refit_rule = "refit_full_training_window_at_selected_setting",
    equal_search_budget_required = TRUE,
    fixed_literature_sensitivity = 0.95,
    generalized_rate_sensitivity = c(0.5, 1, 2),
    primary_standardized_scale_rate = 1,
    test_coverage_tuning_forbidden = TRUE
  ),
  monte_carlo = list(
    framework = "ADEMP",
    diagnostic_pilot_replications_per_mechanism_coverage_method = 2L,
    confirmatory_minimum_replications = 500L,
    confirmatory_batch_size = 250L,
    confirmatory_maximum_replications = 2500L,
    target_coverage_mcse = 0.01,
    approximate_replications_for_coverage_mcse = c(
      coverage_0.80 = 1600L, coverage_0.90 = 900L
    ),
    endpoint_midpoint_mcse_denominator = "training_response_sd",
    endpoint_midpoint_mcse_fraction = 0.02,
    width_mcse_denominator = "mean_oracle_RQR_width",
    width_mcse_fraction = 0.02,
    standardized_rqr_loss_mcse_absolute = 0.01,
    near_zero_oracle_width_policy = "fail_DGP",
    stopping_uses_performance_rank = FALSE,
    stopping_uses_sign_or_significance = FALSE,
    stopping_uses_monte_carlo_precision_only = TRUE,
    report_mcse_for_every_primary_summary = TRUE,
    paired_method_contrasts = TRUE,
    replication_is_independent_unit = TRUE,
    failed_fits_retained_in_denominator = TRUE
  ),
  mcmc = list(
    confirmatory_schedule_frozen = FALSE,
    diagnostic_pilot_chains = 4L,
    diagnostic_pilot_preselected_replications = 2L,
    candidate_confirmatory_chains_per_fit = 1L,
    per_fit_within_chain_ess_required = TRUE,
    preselected_multichain_sentinel_fraction = 0.05,
    minimum_sentinels_per_250_replication_stratum = 2L,
    sentinel_strata = c(
      "mechanism", "coverage", "method", "replication_batch"
    ),
    sentinel_selected_before_data_generation = TRUE,
    sentinel_failure_action = "stop_declared_cell",
    backend = "cpp",
    numerical_policy = "fail",
    zero_repairs_required = TRUE,
    exact_provenance_required = TRUE,
    no_post_failure_reseed = TRUE,
    no_outcome_driven_chain_extension = TRUE,
    learned_rate_is_sensitivity_not_calibration = TRUE
  ),
  forecast = list(
    realized_root_path =
      "root_path_after_generated_future_state_innovations",
    oracle_conditional_mean_root =
      "conditional_expectation_given_forecast_origin_and_DGP_parameters",
    endpoint_bias_rmse_target = "oracle_conditional_mean_root",
    realized_forecast_error_target = "realized_root_path",
    empirical_coverage_target = "generated_future_response",
    empirical_coverage_interpretation =
      "repeated_sampling_operating_characteristic",
    posterior_predictive_coverage_claim = FALSE,
    true_W_method_competitive = FALSE
  ),
  seed_contract = list(
    schema_version = "rqrgibbs_dlm_main_simulation_seeds/1.0.0",
    master_seed = 2026072401L,
    streams = c(
      "data", "method", "initialization", "forecast", "sentinel",
      "oracle", "tuning"
    ),
    independent_streams_required = TRUE,
    mapping = "sha256_keyed_31bit_integer"
  ),
  reproducibility = list(
    exact_source_commit_required = TRUE,
    isolated_primary_runtime_required = TRUE,
    protected_exdqlm_checkout_read_only = TRUE,
    external_comparator_archive_required = TRUE,
    full_seed_ledger_required = TRUE,
    atomic_replication_artifacts_required = TRUE,
    rollback_fault_test_required = TRUE,
    process_group_monitor_required = TRUE,
    heavy_fit_objects_ignored = TRUE,
    compact_results_and_recursive_hashes_tracked = TRUE,
    exact_failure_denominator_required = TRUE,
    two_replication_byte_reproduction_required = TRUE
  )
)
