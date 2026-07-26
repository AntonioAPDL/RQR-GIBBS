# Frozen ADEMP contract for the confirmatory RQR-DLM simulation.
#
# This file is intentionally fail closed.  A later, separately reviewed commit
# may change confirmatory_execution_authorized from FALSE to TRUE; no other
# source or design change may accompany that transition.

rqr_dlm_main_simulation <- list(
  schema_version = "rqrgibbs_dlm_main_simulation/1.0.0",
  config_id = "rqr_dlm_main_simulation_20260724",
  status = "corrected_main_study_contract",
  generalized_bayes = TRUE,
  response_likelihood = FALSE,
  response_prediction_contract = FALSE,
  implementation_correction = list(
    schema_version = "rqrgibbs_dlm_main_correction/1.1.0",
    failed_authorization_commit =
      "b8b7748ab181a006611b602f64d4edf5be591de6",
    failed_wave_id =
      "static_gaussian_T200__target0200__sentinel",
    failed_wave_artifact_hashes_sha256 =
      "c003675b037311f30df05a8ed4e9992997e4ae0cb308b93ef44592a9a871b80f",
    failed_outputs_reused = FALSE,
    comparative_simulation_metrics_used = FALSE,
    failed_wave_diagnostics_used_for_computational_correction = TRUE,
    failed_wave_scientific_metrics_used_for_correction = FALSE,
    correction_validation_role =
      "computational_transition_and_fixed_schedule_only",
    uniform_role_specific_schedule_no_adaptive_extension = TRUE,
    fresh_relaunch_required = TRUE,
    comparator_projection_correction =
      "project each p_by_T state mean through FF to one ordinate per time",
    component_scale_correction =
      "exact centered_noncentered_interweaving",
    provenance_cost_correction =
      "exclude declared local output roots from the source-worktree sidecar digest",
    comparator_standard_schedule_correction =
      "retain_4000_after_projection_correct_full_wave_diagnostic_gate",
    correction_budget_path =
      "docs/audits/rqr_dlm_main_correction_budget_20260726.csv",
    correction_budget_sha256 =
      "1c8a80e2d1b764a031afbec89b7a5447f6233cc63de138b3dcc94aa9d650db2e",
    target_prior_seed_or_diagnostic_threshold_changed = FALSE,
    mcmc_transition_and_standard_schedule_changed = TRUE
  ),
  diagnostic_pilot_execution_authorized = FALSE,
  confirmatory_execution_authorized = FALSE,
  implemented_modes = c(
    "preflight", "oracle-reference", "sentinel-core",
    "execute-confirmatory", "collect", "audit"
  ),
  review_contract = list(
    review = "ChatGPT Pro Output-15",
    review_branch_tip = "ba37b56b7027cb954a9b74753389c58373261b8e",
    incidence_path = paste0(
      "external_reviews/chatgpt_pro_output15_20260724/",
      "chatgpt_pro_output15_final_design_matrix_20260724.csv"
    ),
    incidence_sha256 =
      "b95aeeda3aef3fd1b69d4e98294e920ffa94ea7093d5c2c506a1ce3ef25b97c3",
    budget_path = paste0(
      "external_reviews/chatgpt_pro_output15_20260724/",
      "chatgpt_pro_output15_run_budget_20260724.csv"
    ),
    budget_sha256 =
      "85eed381bf152a5c4158b0931b080eda9b16a374fdf047fe204b202f87299b64",
    gates_path = paste0(
      "external_reviews/chatgpt_pro_output15_20260724/",
      "chatgpt_pro_output15_launch_gates_20260724.csv"
    ),
    gates_sha256 =
      "e908d37a0c02d2ff9a50f23af8b81ba6c65648820b049e0657e5f1d74e5db16d",
    incidence_rows = 208L,
    included_rows = 89L,
    omitted_rows = 119L,
    inclusion_rule = "role != 'O' and replication_rule != '0'",
    inclusion_markers = c(precision_extended = "i", frozen_first_200 = "x")
  ),
  design = list(
    framework = "ADEMP",
    coverage_levels = c(0.80, 0.90),
    training_horizons = c(short = 100L, core = 200L, long = 400L),
    forecast_horizon = 20L,
    reported_horizons = c(1L, 5L, 10L, 20L),
    future_subreplications = 20L,
    training_standardization_only = TRUE,
    common_random_numbers = TRUE,
    candidate_tuning_fits = 0L,
    replication_is_independent_unit = TRUE,
    failed_fits_remain_in_denominator = TRUE
  ),
  scenarios = list(
    S01 = list(dgp = "static_gaussian", T = 200L, coverage = 0.80,
               pair = "static_gaussian_T200",
               batch_group = "static_gaussian_T200"),
    S02 = list(dgp = "static_gaussian", T = 200L, coverage = 0.90,
               pair = "static_gaussian_T200",
               batch_group = "static_gaussian_T200"),
    S03 = list(dgp = "local_level_gaussian", T = 200L, coverage = 0.80,
               pair = "local_level_gaussian_T200",
               batch_group = "local_level_gaussian_T200"),
    S04 = list(dgp = "local_level_gaussian", T = 200L, coverage = 0.90,
               pair = "local_level_gaussian_T200",
               batch_group = "local_level_gaussian_T200"),
    S05 = list(dgp = "local_level_skewed", T = 200L, coverage = 0.80,
               pair = "local_level_skewed_T200",
               batch_group = "local_level_skewed_T200"),
    S06 = list(dgp = "local_level_skewed", T = 200L, coverage = 0.90,
               pair = "local_level_skewed_T200",
               batch_group = "local_level_skewed_T200"),
    S07 = list(dgp = "trend_seasonal_gaussian", T = 200L, coverage = 0.80,
               pair = "trend_seasonal_state_T200",
               batch_group = "trend_seasonal_gaussian_T200"),
    S08 = list(dgp = "trend_seasonal_skewed", T = 200L, coverage = 0.80,
               pair = "trend_seasonal_state_T200",
               batch_group = "trend_seasonal_skewed_T200"),
    S09 = list(dgp = "trend_seasonal_skewed", T = 200L, coverage = 0.90,
               pair = "trend_seasonal_state_T200",
               batch_group = "trend_seasonal_skewed_T200"),
    S10 = list(dgp = "trend_regression_unequal", T = 200L, coverage = 0.80,
               pair = "trend_regression_unequal_T200",
               batch_group = "trend_regression_unequal_T200"),
    S11 = list(dgp = "trend_regression_unequal", T = 200L, coverage = 0.90,
               pair = "trend_regression_unequal_T200",
               batch_group = "trend_regression_unequal_T200"),
    S12 = list(dgp = "break_heavy_tail", T = 200L, coverage = 0.80,
               pair = "break_heavy_tail_T200",
               batch_group = "break_heavy_tail_T200"),
    S13 = list(dgp = "heteroscedastic_t5", T = 200L, coverage = 0.80,
               pair = "heteroscedastic_t5_T200",
               batch_group = "heteroscedastic_t5_T200"),
    S14 = list(dgp = "root_alignment", T = 200L, coverage = 0.80,
               pair = "root_alignment_T200",
               batch_group = "root_alignment_T200"),
    S15 = list(dgp = "local_level_skewed", T = 100L, coverage = 0.80,
               pair = "local_level_skewed_T100",
               batch_group = "local_level_skewed_T100"),
    S16 = list(dgp = "local_level_skewed", T = 400L, coverage = 0.80,
               pair = "local_level_skewed_T400",
               batch_group = "local_level_skewed_T400")
  ),
  dgp = list(
    local_level_variance = 0.02,
    trend_level_variance = 0.005,
    trend_slope_variance = 0.0005,
    seasonal_period = 12L,
    seasonal_variance = 0.002,
    seasonal_initial_covariance = 0.60^2,
    regression_variance = 0.05,
    regression_predictor_ar = 0.70,
    regression_predictor_innovation_variance = 0.51,
    heteroscedastic_log_scale_ar = 0.80,
    heteroscedastic_log_scale_innovation_variance = 0.36,
    heteroscedastic_log_scale_coefficient = 0.25,
    scale_floor = 0.35,
    root_alignment = list(
      reference_coverage = 0.80, lower_initial = -2,
      upper_initial = 2, lower_variance = 0.001,
      upper_variance = 0.001, minimum_separation = 0.10
    )
  ),
  methods = list(
    M01 = "rqr_dlm_component_scale_fixed_rate",
    M02 = "dynamic_equal_tailed_quantile_interval",
    M03 = "fixed_design_rqr",
    M04 = "static_equal_tailed_quantile_regression",
    M05 = "rolling_empirical_equal_tailed_interval",
    M06 = "rqr_dlm_frozen_component_discount",
    M07 = "rqr_dlm_common_evolution_ablation",
    M08 = "rqr_dlm_true_fixed_W_selected_cells",
    M09 = "rqr_dlm_fixed_rate_0p5",
    M10 = "rqr_dlm_fixed_rate_2",
    M11 = "rqr_dlm_component_scale_learned_rate",
    M12 = "gaussian_dlm_response_interval",
    M13 = "time_series_valid_conformal_interval"
  ),
  frozen_tuning = list(
    component_scale_prior = list(shape = 2.5, rate = 0.025),
    common_scale_prior = list(shape = 2.5, rate = 0.025),
    component_scale_kernel = list(
      centered_inverse_gamma = TRUE,
      noncentered_slice_interweave = TRUE,
      interweave_cycles = 1L,
      slice_width = 1,
      slice_sweeps_per_cycle = 2L,
      slice_max_steps = 100L,
      slice_max_shrink = 1000L,
      target_change = FALSE
    ),
    fixed_design_ridge_variance = 25,
    empirical_window = 100L,
    discounts = list(
      local_level = c(level = 0.95),
      trend_seasonal = c(trend = 0.98, seasonal = 0.95),
      trend_regression = c(trend = 0.98, regression = 0.90),
      break_regression = c(level = 0.98, regression = 0.90)
    ),
    fixed_rates = c(0.5, 1, 2),
    learned_rate_role = "normalized_inverse_loss_scale_not_calibration"
  ),
  schedules = list(
    dynamic_rqr = list(burn = 1000L, retain = 2000L, thin = 1L),
    dynamic_rqr_component_scale_standard =
      list(burn = 1000L, retain = 6000L, thin = 1L),
    learned_dynamic_rqr =
      list(burn = 1500L, retain = 3000L, thin = 1L),
    learned_dynamic_rqr_component_scale_standard =
      list(burn = 1500L, retain = 9000L, thin = 1L),
    dynamic_quantile_endpoint =
      list(burn = 1000L, retain = 2000L, thin = 1L),
    dynamic_quantile_endpoint_standard =
      list(burn = 1000L, retain = 4000L, thin = 1L),
    fixed_design_rqr = list(burn = 500L, retain = 1500L, thin = 1L)
  ),
  batching = list(
    core = list(initial = 200L, increment = 100L, maximum = 600L),
    sensitivity = list(initial = 100L, increment = 50L, maximum = 300L),
    frozen = list(initial = 200L, increment = 0L, maximum = 200L),
    sentinel_replications_per_batch = 2L,
    sentinel_chains = 4L,
    standard_chains = 1L,
    no_retry = TRUE,
    no_reseed = TRUE,
    no_chain_extension = TRUE
  ),
  diagnostics = list(
    sentinel_rhat_max = 1.01,
    sentinel_bulk_ess_min = 400,
    sentinel_tail_ess_min = 400,
    single_bulk_ess_min = 200,
    single_tail_ess_min = 100,
    single_mcse_sd_max = 0.08,
    numerical_repairs_required = 0L
  ),
  standard_initialization = list(
    midpoint_rule = "training_median",
    midpoint_shift_training_sd = 0,
    half_width_multiplier = 1.00,
    lambda_initial = 1,
    component_scale_multiplier = 1,
    component_scale_reference = "inverse_gamma_prior_median"
  ),
  initialization_profiles = list(
    A = list(
      midpoint_rule = "empirical_interval_midpoint",
      midpoint_shift_training_sd = 0,
      half_width_multiplier = 1.00,
      lambda_initial = 0.5, component_scale_multiplier = 0.5
    ),
    B = list(
      midpoint_rule = "empirical_interval_midpoint",
      midpoint_shift_training_sd = -0.5,
      half_width_multiplier = 0.75,
      lambda_initial = 1, component_scale_multiplier = 1
    ),
    C = list(
      midpoint_rule = "empirical_interval_midpoint",
      midpoint_shift_training_sd = 0.5,
      half_width_multiplier = 1.25,
      lambda_initial = 2, component_scale_multiplier = 2
    ),
    D = list(
      midpoint_rule = "training_median",
      midpoint_shift_training_sd = 0,
      half_width_multiplier = 1.75,
      lambda_initial = 4, component_scale_multiplier = 4
    )
  ),
  precision = list(
    core_aggregate_coverage_mcse = 0.010,
    core_horizon_coverage_mcse = 0.020,
    sensitivity_aggregate_coverage_mcse = 0.015,
    sensitivity_horizon_coverage_mcse = 0.030,
    endpoint_midpoint_training_sd_fraction = 0.02,
    width_oracle_width_fraction = 0.02,
    standardized_rqr_loss_contrast_mcse = 0.010,
    uses_performance_sign = FALSE,
    uses_TOST = FALSE
  ),
  analysis = list(
    coverage_equivalence_margin = 0.02,
    coverage_equivalence_confidence = 0.90,
    width_contrast_confidence = 0.95,
    directional_family_adjustment = "Holm",
    response_log_score_for_rqr = FALSE,
    response_crps_for_rqr = FALSE
  ),
  oracle = list(
    schema_version = "rqrgibbs_rqr_oracle_reference/2.0.0",
    profile_grid_size = 1601L,
    higher_precision_grid_size = 3201L,
    primary_tolerance = 1e-10,
    higher_precision_tolerance = 1e-11,
    higher_precision_objective_max = 1e-8,
    adaptive_basin_refinement = TRUE,
    unrestricted_multistart = TRUE,
    higher_precision_crosscheck = TRUE,
    coverage_residual_max = 1e-8,
    moment_residual_max = 1e-7,
    objective_gap_max = 1e-8,
    numerical_error_label = "estimated_not_rigorous_bound",
    unresolved_uniqueness_policy =
      "excess_risk_and_distance_to_minimizer_set"
  ),
  reference = list(
    byte_reproduction_scenario = "S05",
    byte_reproduction_replications = c(1L, 2L),
    serialization_version = 3L
  ),
  rng = list(
    schema_version = "rqrgibbs_dlm_main_simulation_seeds/2.0.0",
    kind = "L'Ecuyer-CMRG",
    master_seed = 2026072401,
    task_assignment = paste(
      "canonical_sorted_sentinel_selection_keys_then",
      "canonical_sorted_remaining_keys_via_nextRNGStream",
      sep = "_"
    ),
    future_assignment = "nextRNGSubStream",
    full_state_sha256_required = TRUE
  ),
  comparator = list(
    exdqlm = list(
      version = "1.1.0",
      source_sha256 =
        "51bc968f617721c9ab1dcfc6ec14857d30827fcd36659f3de45337cc3c82bd14",
      dqlm.ind = TRUE, init.from.vb = FALSE, fix.sigma = FALSE,
      sig.init = 1, response_predictive_draws = FALSE
    ),
    quantreg = list(
      version = "6.1",
      source_sha256 =
        "f42292c5ab25a15f39295b93391deafef192fe09eefde563399a64eba7e0169a",
      method = "br", response_predictive_draws = FALSE
    ),
    protected_source_checkout_used = FALSE
  ),
  authorization_contract = list(
    schema_version =
      "rqrgibbs_dlm_confirmatory_authorization/1.0.0",
    reviewed_implementation_commit_required = TRUE,
    authorization_commit_must_equal_runtime_commit = TRUE,
    authorization_diff_must_only_flip_confirmatory_flag = TRUE,
    explicit_user_confirmation_required = TRUE,
    primary_clean_worktree_required = TRUE,
    bound_artifacts = c(
      "primary_runtime_tree_digest",
      "preflight_artifact_hashes_sha256",
      "reference_artifact_hashes_sha256",
      "seed_ledger_sha256", "task_plan_sha256",
      "exdqlm_source_sha256", "quantreg_source_sha256"
    ),
    runtime_equality_gates = c(
      "reference_runtime_bundle_match",
      "comparator_dependency_runtime_match",
      "toolchain_match"
    ),
    protected_checkout_used = FALSE
  ),
  resources = list(
    workers = 32L, sentinel_workers = 8L, threads_per_worker = 1L,
    sampled_process_group_thread_ceiling = 4L,
    sampled_reference_process_group_thread_ceiling = 4L,
    sampled_thread_ceiling_role =
      "hard_OS_thread_envelope_not_compute_parallelism",
    per_worker_memory_GiB = 1.5, free_space_required_GiB = 50,
    free_space_recommended_GiB = 100,
    process_wave_ceiling_hours = 14 * 24,
    resumable_only_at_completed_batch_boundaries = TRUE,
    maximum_plan_may_require_multiple_process_waves = TRUE
  )
)
