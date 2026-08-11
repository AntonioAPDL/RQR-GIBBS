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
    schema_version = "rqrgibbs_dlm_main_correction/1.17.0",
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
    second_failed_authorization_commit =
      "bb966299bb298ee31ec65d167edf53c44ce48b03",
    second_failed_wave_id =
      "local_level_gaussian_T200__target0200__sentinel",
    second_failed_outputs_reused = FALSE,
    second_failed_scientific_metrics_used = FALSE,
    forecast_horizon_correction =
      "materialize FF at T, H, and T_plus_H before fitting or forecasting",
    health_state_correction =
      "validate unname predecessor digests and report failed runs as terminal",
    fixed_design_standard_schedule_correction =
      "retain_3000_for_one_chain_standard_fits_after_computational_diagnostic_gate",
    script_invocation_correction =
      "invoke monitored shell workers through bash independent of Git mode transport",
    third_failed_authorization_commit =
      "ce02915f8e6270fb21c4cce1bdc231beeda12292",
    third_failed_wave_id =
      "local_level_gaussian_T200__target0200__sentinel",
    third_failed_wave_artifact_hashes_sha256 =
      "418b6facad514e09dc7fe3650c8c172c88fa82cb84054b11506bc885284a039c",
    third_failed_outputs_reused = FALSE,
    third_failed_scientific_metrics_used = FALSE,
    singleton_state_projection_correction =
      "preserve p_by_T shape when p_equals_one and project through FF",
    component_scale_sentinel_schedule_correction =
      "match fixed component_scale standard schedule without adaptive extension",
    dynamic_quantile_sentinel_schedule_correction =
      "match fixed M02 standard schedule after the complete projection_correct wave gate",
    dynamic_quantile_target_correction = paste(
      "hold m0_C0_discount_and_priors common across chains;",
      "vary only RNG and target_preserving vb_init_fit state"
    ),
    diagnostic_exception_correction =
      "publish structured atomic failure evidence before fail_closed stop",
    sentinel_serialization_correction =
      "persist compact scalar diagnostics without accumulating full sentinel fits",
    second_wave_component_scale_gate_role =
      "full development gate exposed residual q mixing and is not promotion evidence",
    component_scale_collapsed_correction = paste(
      "exact symmetric rootwise partially_collapsed q updates;",
      "integrate and redraw each root in turn before ASIS"
    ),
    first_exact_promotion_failure =
      "wave2_standard_single_chain_q_ESS_at_e9c8068",
    first_exact_promotion_outputs_reused = FALSE,
    symmetric_rootwise_correction_reason = paste(
      "one-root collapse left q coupled to the conditioned root;",
      "compose two exact rootwise collapsed blocks"
    ),
    symmetric_rootwise_development_failure =
      "wave2_S03_rep0013_and_rep0094_scale_mixing_with_one_ASIS_cycle",
    symmetric_rootwise_development_outputs_reused = FALSE,
    two_ASIS_cycle_correction_reason = paste(
      "retain both exact rootwise collapsed blocks;",
      "compose a second exact centered_noncentered scale transition"
    ),
    component_scale_transition_selection =
      "rootwise2_ASIS2_selected_after_targeted_transition_comparison",
    rootwise2_ASIS2_selection_reason = paste(
      "repeat the full symmetric rootwise partial-collapse composition twice;",
      "retain two centered-noncentered ASIS cycles for the strongest",
      "hard-case diagnostic margin"
    ),
    rootwise2_ASIS2_development_evidence_path =
      "docs/audits/rqr_dlm_transition_comparison_20260728/candidate_summary.csv",
    rootwise2_ASIS2_development_evidence_sha256 =
      "007ec6c84db930119858b16b49b398516001c353937e26310e91eb0a7818989c",
    rootwise2_ASIS2_development_outputs_reused = FALSE,
    component_scale_transition_benchmark_role =
      "development_only_no_scientific_metrics_no_promotion",
    wave2_recovery_selection_source_commit =
      "c2d560d761aae35554cadfe417e11a65ef540043",
    wave2_recovery_seed_ledger_sha256 =
      "3dc8483f4a777ab766704b901997295bed1c89db0590429a70f3116b233e948f",
    wave2_recovery_selection_path = paste0(
      "docs/audits/rqr_dlm_wave2_candidate_selection_20260731/",
      "candidate_decisions.csv"
    ),
    wave2_recovery_selection_sha256 =
      "12bf449b5de7b7c8badb794f8a5e894969963b26612e8b7f5255781ad05d39bc",
    wave2_recovery_development_outputs_reused = FALSE,
    wave2_recovery_scientific_metrics_used = FALSE,
    fixed_design_replica_exchange_correction = paste(
      "exact four-replica loss-tempered transition;",
      "retain only the inverse-temperature-one target"
    ),
    fixed_design_replica_exchange_selection =
      "M03_REX4_B500_R1500_minimum_cost_eligible_candidate",
    fixed_design_replica_exchange_target_change = FALSE,
    true_fixed_W_schedule_correction =
      "M08_uniform_B1000_R4000_method_level_schedule",
    true_fixed_W_schedule_applies_to_M06 = FALSE,
    production_seed_binding_failed_authorization_commit =
      "281802e9a51f4e739bdb9b5211bbea5d1fd8ba29",
    production_seed_binding_failed_run_id =
      "rqr_dlm_main_20260801_281802e",
    production_seed_binding_failed_wave_id =
      "static_gaussian_T200__target0200__sentinel",
    production_seed_binding_failed_wave_artifact_hashes_sha256 =
      "2d2e357798c8652e678292216885ca8015e7215865eecd2834e206d5d22d62f8",
    production_seed_binding_failure = paste(
      "M02 correctly consumed endpoint-specific lower and upper RNG streams;",
      "the post-fit compact diagnostic serializer incorrectly requested",
      "a nonexistent interval stream"
    ),
    production_seed_binding_failed_outputs_reused = FALSE,
    production_seed_binding_failed_scientific_metrics_used = FALSE,
    production_seed_binding_fresh_relaunch_required = TRUE,
    skewed_wave_failed_authorization_commit =
      "32f6745369b83040c0b1c4bd385c17072ee912d8",
    skewed_wave_failed_run_id = "rqr_dlm_main_20260802_32f6745",
    skewed_wave_failed_wave_id =
      "local_level_skewed_T200__target0200__sentinel",
    skewed_wave_failed_artifact_hashes_sha256 =
      "40583d1273290034411f89e1ab466a4ef8e224702b3f640cea99efcfcc28236a",
    skewed_wave_closeout_manifest_sha256 =
      "45e26c4ba643bb38fcb56beb77f421643e3493fa286588deac4ae4c70a10df74",
    skewed_wave_forensic_manifest_sha256 =
      "5a9b31135ef78e55c7b71b0b00ad45198d4f3c9a75537cc9e3b22c9a4d2ea5d3",
    skewed_wave_failure = paste(
      "local_level_skewed sentinel exposed systematic frozen MCMC",
      "mixing failures concentrated in M10 and M11 with additional",
      "near-boundary endpoint-method failures"
    ),
    skewed_wave_failed_outputs_reused = FALSE,
    skewed_wave_failed_scientific_metrics_used = FALSE,
    skewed_wave_diagnostics_used_for_transition_correction = TRUE,
    skewed_wave_fresh_relaunch_required = TRUE,
    skewed_wave_recovery_status =
      "joint_elliptical_selected_s10_guard_failed",
    skewed_whole_scan_candidate_source_commit =
      "5086fac191255a79514475f6dbacddfae4c328ed",
    skewed_whole_scan_candidate_jobs = 93L,
    skewed_whole_scan_candidate_diagnostics = 1791L,
    skewed_whole_scan_candidate_failed_diagnostics = 32L,
    skewed_whole_scan_selected_methods = c("M01", "M02", "M06", "M09"),
    skewed_whole_scan_unresolved_methods = c("M10", "M11"),
    skewed_whole_scan_candidate_closeout_path = paste0(
      "docs/audits/rqr_dlm_skewed_candidate_closeout_20260804/",
      "closeout.json"
    ),
    skewed_whole_scan_candidate_closeout_sha256 =
      "f8b82ef8e0ba51fe4fe9a7ccb34bf62c04013e918c5d88518a3401f4b7e19f4f",
    skewed_whole_scan_candidate_artifact_manifest_sha256 =
      "198e131e234563f9503bb1cb8fee27d81f9d9494461afdf9a79e0529907b7274",
    skewed_joint_elliptical_plan_path = paste0(
      "docs/implementation_notes/",
      "rqr_dlm_joint_elliptical_recovery_plan_20260804.md"
    ),
    skewed_joint_elliptical_plan_sha256 =
      "879c5c7b5672aadbb9f8d491e96c4412f35e57158be30b65ac34489a9c001f32",
    skewed_joint_elliptical_target_change = FALSE,
    skewed_joint_elliptical_outputs_reusable = FALSE,
    skewed_joint_elliptical_candidate_source_commit =
      "2901770ec25fb6042cbc2c8227478a31bdb0dc1a",
    skewed_joint_elliptical_candidate_jobs = 44L,
    skewed_joint_elliptical_candidate_diagnostics = 932L,
    skewed_joint_elliptical_candidate_failed_diagnostics = 22L,
    skewed_joint_elliptical_selected_methods = c("M10", "M11"),
    skewed_joint_elliptical_unresolved_methods = character(),
    skewed_joint_elliptical_selection = list(
      M10 = list(
        candidate = "joint_ess1_x1", transition_multiplier = 1L,
        joint_elliptical_cycles = 1L
      ),
      M11 = list(
        candidate = "joint_ess1_x2", transition_multiplier = 2L,
        joint_elliptical_cycles = 1L
      )
    ),
    skewed_joint_elliptical_candidate_closeout_path = paste0(
      "docs/audits/rqr_dlm_joint_elliptical_candidate_closeout_20260805/",
      "closeout.json"
    ),
    skewed_joint_elliptical_candidate_closeout_sha256 =
      "ffbd98d36afce2a6a64352e75d7eca17c48074435c744d9a52f4916b8ddf4a26",
    skewed_joint_elliptical_candidate_artifact_manifest_sha256 =
      "6551e96f940dd0ddccf6ea3adc6e42902162cf3d288fd802a1c911fc53679b02",
    skewed_joint_elliptical_candidate_decisions_sha256 =
      "ff5395261176a06287af460a1beb243f494bc03035e4c92609e05ccb10d08ad0",
    skewed_joint_elliptical_development_outputs_reused = FALSE,
    skewed_joint_elliptical_scientific_metrics_used = FALSE,
    skewed_affected_wave_required_before_promotion = TRUE,
    higher_dimensional_guard_source_commit =
      "73f9918deb91539f06ced88c7803877a3065f42f",
    higher_dimensional_guard_status =
      "failed_closed_before_affected_wave",
    higher_dimensional_guard_jobs = 8L,
    higher_dimensional_guard_diagnostics = 95L,
    higher_dimensional_guard_failed_diagnostics = 16L,
    higher_dimensional_guard_failed_method = "M11",
    higher_dimensional_guard_failed_estimand = "log_q_2",
    higher_dimensional_guard_closeout_path = paste0(
      "docs/audits/rqr_dlm_s10_guard_failure_20260806/",
      "closeout.json"
    ),
    higher_dimensional_guard_closeout_sha256 =
      "d33d7119d49c15154512dc711ec32a1684d25e13f44b60f7898a38d93e07c372",
    higher_dimensional_guard_artifact_manifest_sha256 =
      "2fa819c0e991eba7a3574bcf081f38f715dce13d49527798e541ade0356bd78d",
    higher_dimensional_guard_outputs_reused = FALSE,
    higher_dimensional_guard_scientific_metrics_used = FALSE,
    higher_dimensional_recovery_status =
      "predeclared_exact_candidate_comparison_pending",
    diagnostic_aware_failed_authorization_commit =
      "ea8ea8d17c6f7bb34b015472e4f60f62e547c942",
    diagnostic_aware_failed_run_id =
      "rqr_dlm_diagnostic_aware_maximum_20260807_ea8ea8d",
    diagnostic_aware_failed_wave_id =
      "static_gaussian_T200__target0200__sentinel",
    diagnostic_aware_failed_wave_artifact_hashes_sha256 =
      "1f62f7a31780bede379d50cca4d0f8170211c72a6b3390a608ef97dbfc50715c",
    diagnostic_aware_failure_class =
      "mcmc_diagnostic_construction_failure",
    diagnostic_aware_failure_message_digest =
      "6495bf2b41dc1e26ee4112cd6e30200ca35aa20b34792f1e7f953b9a79210ef5",
    diagnostic_aware_m01_diagnostics_passed = 368L,
    diagnostic_aware_m01_diagnostics_total = 368L,
    diagnostic_aware_m02_construction_failures = 8L,
    diagnostic_aware_failed_outputs_reused = FALSE,
    diagnostic_aware_failed_scientific_metrics_used = FALSE,
    diagnostic_aware_fresh_relaunch_required = TRUE,
    dynamic_quantile_retained_draw_correction = paste(
      "apply one validated diagnostic-thinning index to training ordinates,",
      "terminal states, and deterministic future-root functions"
    ),
    correction_budget_path =
      "docs/audits/rqr_dlm_main_correction_budget_20260727.csv",
    correction_budget_sha256 =
      "fdee8c042a7ebcc9a1a9c1f8d704be530487fcbc098cdc0572f09daadfd9cdb9",
    completion_recovery_failed_authorization_commit =
      "031595bbbca5c59673faed10087faaf450c15a5a",
    completion_recovery_failed_run_id =
      "rqr_dlm_diagnostic_aware_maximum_20260808_031595b",
    completion_recovery_failed_wave_id =
      "trend_seasonal_gaussian_T200__target0200__sentinel",
    completion_recovery_closeout_artifact_hashes_sha256 =
      "e9d6197b813904b51f9da88f7511e673d33f7fb131f6b8cd0260281d27ab778b",
    completion_recovery_failed_outputs_reused = FALSE,
    completion_recovery_scientific_metrics_used = FALSE,
    component_scale_directional_scope_correction = paste(
      "apply the selected directional update only to models with at least",
      "two evolution components; retain coordinate interweaving and joint",
      "state elliptical updates for one-component M11 models"
    ),
    fixed_design_replica_exchange_activity_correction = paste(
      "keep exact structure target and zero-repair checks as hard invariants;",
      "record finite-run acceptance label and round-trip activity as",
      "diagnostic-aware sidecars"
    ),
    sampled_worker_memory_ceiling_correction =
      "uniform prospective increase from 1.5 GiB to 2.0 GiB",
    completion_recovery_fresh_relaunch_required = TRUE,
    target_prior_seed_or_diagnostic_threshold_changed = FALSE,
    mcmc_transition_and_fixed_role_schedule_changed = TRUE
  ),
  diagnostic_pilot_execution_authorized = FALSE,
  confirmatory_execution_authorized = FALSE,
  implemented_modes = c(
    "preflight", "oracle-reference", "sentinel-core",
    "execute-confirmatory", "development-affected-wave",
    "collect", "audit"
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
      symmetric_rootwise_partially_collapsed = TRUE,
      collapsed_integrated_roots = c("root1", "root2"),
      collapsed_cycles = 2L,
      centered_inverse_gamma = TRUE,
      noncentered_slice_interweave = TRUE,
      interweave_cycles = 2L,
      transition_order = "rootwise_then_interweave",
      selected_candidate = "rootwise2_ASIS2",
      slice_width = 1,
      slice_sweeps_per_cycle = 3L,
      slice_max_steps = 100L,
      slice_max_shrink = 1000L,
      target_change = FALSE
    ),
    method_transition_policies = list(
      M01 = list(
        transition_multiplier = 2L,
        joint_state_elliptical_slice = FALSE,
        joint_state_elliptical_cycles = 0L,
        selected_candidate = "whole_scan_x2"
      ),
      M02 = list(
        transition_multiplier = 2L,
        joint_state_elliptical_slice = FALSE,
        joint_state_elliptical_cycles = 0L,
        selected_candidate = "whole_scan_x2"
      ),
      M06 = list(
        transition_multiplier = 2L,
        joint_state_elliptical_slice = FALSE,
        joint_state_elliptical_cycles = 0L,
        selected_candidate = "whole_scan_x2"
      ),
      M07 = list(
        transition_multiplier = 1L,
        joint_state_elliptical_slice = FALSE,
        joint_state_elliptical_cycles = 0L,
        selected_candidate = "unchanged"
      ),
      M08 = list(
        transition_multiplier = 1L,
        joint_state_elliptical_slice = FALSE,
        joint_state_elliptical_cycles = 0L,
        selected_candidate = "unchanged"
      ),
      M09 = list(
        transition_multiplier = 2L,
        joint_state_elliptical_slice = FALSE,
        joint_state_elliptical_cycles = 0L,
        selected_candidate = "whole_scan_x2"
      ),
      M10 = list(
        transition_multiplier = 1L,
        joint_state_elliptical_slice = TRUE,
        joint_state_elliptical_cycles = 1L,
        selected_candidate = "joint_ess1_x1"
      ),
      M11 = list(
        transition_multiplier = 2L,
        joint_state_elliptical_slice = TRUE,
        joint_state_elliptical_cycles = 1L,
        selected_candidate = "joint_ess1_x2"
      )
    ),
    fixed_design_replica_exchange = list(
      enabled = TRUE,
      inverse_temperatures = c(1, 0.45, 0.20, 0.09),
      swap_every = 1L,
      pairing = "alternating_adjacent",
      store_energy_trace = FALSE,
      hot_initialization_profiles = c("A", "C", "D"),
      selected_candidate = "M03_REX4_B500_R1500",
      exact_cold_target = TRUE,
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
    dynamic_rqr_true_fixed_W =
      list(burn = 1000L, retain = 4000L, thin = 1L),
    dynamic_rqr_component_scale_standard =
      list(burn = 1000L, retain = 6000L, thin = 1L),
    dynamic_rqr_component_scale_sentinel =
      list(burn = 1000L, retain = 6000L, thin = 1L),
    learned_dynamic_rqr =
      list(burn = 1500L, retain = 3000L, thin = 1L),
    learned_dynamic_rqr_component_scale_standard =
      list(burn = 1500L, retain = 9000L, thin = 1L),
    learned_dynamic_rqr_component_scale_sentinel =
      list(burn = 1500L, retain = 9000L, thin = 1L),
    dynamic_quantile_endpoint =
      list(burn = 1000L, retain = 2000L, thin = 1L),
    dynamic_quantile_endpoint_standard =
      list(burn = 1000L, retain = 4000L, thin = 1L),
    dynamic_quantile_endpoint_sentinel =
      list(burn = 1000L, retain = 4000L, thin = 1L),
    fixed_design_rqr = list(burn = 500L, retain = 1500L, thin = 1L),
    fixed_design_rqr_standard =
      list(burn = 500L, retain = 3000L, thin = 1L)
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
    per_worker_memory_GiB = 2.0, free_space_required_GiB = 50,
    free_space_recommended_GiB = 100,
    process_wave_ceiling_hours = 14 * 24,
    resumable_only_at_completed_batch_boundaries = TRUE,
    maximum_plan_may_require_multiple_process_waves = TRUE
  )
)
