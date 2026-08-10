# Execution policy for completing the maximum RQR-DLM design while retaining
# every frozen MCMC diagnostic as scientific metadata.
#
# This is deliberately separate from the confirmatory authorization flag.  A
# completed run under this policy is diagnostic-aware evidence; it is not a
# declaration that every retained Markov chain passed the frozen convergence
# and Monte Carlo precision thresholds.

rqr_dlm_diagnostic_aware_completion <- list(
  schema_version = "rqrgibbs_dlm_diagnostic_aware_completion/1.1.0",
  policy_id = "rqr_dlm_diagnostic_aware_completion_20260807",
  execution_authorized = TRUE,
  explicit_user_direction_recorded = TRUE,
  user_direction_date = "2026-08-07",
  interpretation = "diagnostic_aware_not_convergence_validated",
  generalized_bayes = TRUE,
  response_likelihood = FALSE,
  response_prediction_contract = FALSE,
  complete_maximum_design = TRUE,
  precision_stopping_disabled = TRUE,
  diagnostic_thresholds_changed = FALSE,
  diagnostic_failures = "record_and_continue",
  diagnostic_construction_failures = "global_stop",
  hard_stop_classes = c(
    "DGP_generation_failure",
    "provenance_or_runtime_mismatch",
    "source_seed_or_artifact_mismatch",
    "nonfinite_or_unordered_primary_output",
    "nonexact_target_or_numerical_repair",
    "resource_or_process_failure"
  ),
  selected_transition = list(
    method = "M11",
    candidate = "directional1_joint1",
    transition_multiplier = 2L,
    joint_state_elliptical_cycles = 1L,
    component_scale_directional_interweave = TRUE,
    component_scale_directional_sweeps = 1L,
    component_scale_directional_min_components = 2L,
    single_component_fallback =
      "coordinate_interweave_plus_joint_state_elliptical",
    target_change = FALSE,
    schedule_change = FALSE
  ),
  bounded_evidence = list(
    source_commit = "c6fd8b05ec839cc75873f30af7244c501dc8fa6c",
    comparison_manifest_sha256 =
      "0793c2d137bf3d296809b04d8fbaeed8d53d196952d95a50d3f0149ff3f6da47",
    candidate_summary_sha256 =
      "e845d6baf0047ed130b7a2e10605eb1814464fcdd0a1863da0a26c7e6c133ea6",
    artifact_hashes_sha256 =
      "74d05a5fc4f20c69cceb0e334310157a3939b99e1a61be9d5a2fb8d8ab54caf3",
    jobs_succeeded = 48L,
    jobs_planned = 48L,
    diagnostics_passed = 523L,
    diagnostics_total = 572L,
    selected_candidate_diagnostics_passed = 140L,
    selected_candidate_diagnostics_total = 143L,
    all_exact_joint_target = TRUE,
    all_zero_repairs = TRUE,
    all_resource_gates_passed = TRUE
  ),
  result_contract = list(
    keep_primary_metrics_when_diagnostics_fail = TRUE,
    failure_class = "mcmc_diagnostic_warning",
    worker_status = "completed_with_diagnostic_warning",
    stage_status = "completed_with_diagnostic_warnings",
    include_warning_rows_in_primary_aggregate = TRUE,
    publish_warning_stratified_sensitivity = TRUE,
    scientific_promotion = FALSE
  )
)
