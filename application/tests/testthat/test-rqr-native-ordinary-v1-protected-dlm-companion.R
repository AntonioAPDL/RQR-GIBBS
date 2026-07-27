ordinary_v1_dlm_companion_environment <- function() {
  path <- testthat::test_path(
    "..", "..", "scripts",
    "30_bundle_rqr_ordinary_v1_protected_dlm_evidence.R"
  )
  old <- Sys.getenv(
    "RQR_DLM_COMPANION_SOURCE_ONLY", unset = NA_character_
  )
  on.exit({
    if (is.na(old)) {
      Sys.unsetenv("RQR_DLM_COMPANION_SOURCE_ONLY")
    } else {
      Sys.setenv(RQR_DLM_COMPANION_SOURCE_ONLY = old)
    }
  }, add = TRUE)
  Sys.setenv(RQR_DLM_COMPANION_SOURCE_ONLY = "YES")
  environment <- new.env(parent = globalenv())
  sys.source(path, envir = environment)
  environment
}

ordinary_v1_dlm_companion_write_json <- function(value, path) {
  jsonlite::write_json(
    value, path, auto_unbox = TRUE, pretty = TRUE, null = "null"
  )
  invisible(path)
}

ordinary_v1_dlm_companion_write_csv <- function(value, path) {
  utils::write.csv(value, path, row.names = FALSE, quote = TRUE)
  invisible(path)
}

ordinary_v1_dlm_companion_hash <- function(path) {
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

ordinary_v1_dlm_companion_refresh_artifacts <- function(
    environment, directory, role) {
  files <- sort(setdiff(
    environment$rqr_dlm_companion_input_files(role),
    "artifact_hashes.csv"
  ))
  value <- data.frame(
    path = files,
    bytes = as.numeric(file.info(file.path(directory, files))$size),
    sha256 = vapply(
      file.path(directory, files),
      ordinary_v1_dlm_companion_hash, character(1L)
    ),
    stringsAsFactors = FALSE
  )
  if (identical(role, "dlm_reference")) {
    value <- value[, c("sha256", "bytes", "path"), drop = FALSE]
    ordinary_v1_dlm_companion_write_csv(
      value, file.path(directory, "artifact_hashes.csv")
    )
  } else {
    ordinary_v1_dlm_companion_write_csv(
      value, file.path(directory, "artifact_hashes.csv")
    )
  }
  invisible(directory)
}

ordinary_v1_dlm_companion_refresh_reference <- function(
    environment, directory) {
  bundle_path <- file.path(directory, "reference_bundle.json")
  bundle <- jsonlite::read_json(bundle_path, simplifyVector = FALSE)
  for (name in environment$rqr_dlm_companion_reference_bundle_file_names()) {
    bundle$files[[name]] <- ordinary_v1_dlm_companion_hash(
      file.path(directory, name)
    )
  }
  ordinary_v1_dlm_companion_write_json(bundle, bundle_path)
  run_path <- file.path(directory, "run_manifest.json")
  run <- jsonlite::read_json(run_path, simplifyVector = FALSE)
  run$reference_gates_sha256 <- ordinary_v1_dlm_companion_hash(
    file.path(directory, "reference_gates.csv")
  )
  run$reference_bundle_sha256 <- ordinary_v1_dlm_companion_hash(
    bundle_path
  )
  ordinary_v1_dlm_companion_write_json(run, run_path)
  ordinary_v1_dlm_companion_refresh_artifacts(
    environment, directory, "dlm_reference"
  )
}

ordinary_v1_dlm_companion_make_reference <- function(
    environment, directory, contract) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  fixtures <- data.frame(
    fixture_id = c(
      "fixed_W_local_level", "frozen_trend_seasonal_discount",
      "shared_component_scale_trend_regression"
    ),
    state_dimension = c(1, 5, 3),
    component_dims = c("1", "2,3", "2,1"),
    component_names = c(
      "level", "trend,seasonal", "trend,regression"
    ),
    observed_count = c(22, 36, 29),
    missing_count = c(2, 0, 1),
    training_horizon = c(24, 36, 30),
    future_horizon = c(4, 4, 3),
    evolution_mode = c(
      "fixed_W", "discount_template", "component_scale"
    ),
    exact_joint_target = TRUE,
    extension_reproduces_training = c(NA, TRUE, NA),
    model_digest = contract$digest_a,
    evolution_digest = contract$digest_b,
    missing_response_digest = contract$digest_c,
    future_digest = contract$digest_d,
    stringsAsFactors = FALSE
  )
  ordinary_v1_dlm_companion_write_csv(
    fixtures, file.path(directory, "fixture_construction.csv")
  )
  gates <- data.frame(
    gate = environment$rqr_dlm_companion_reference_gate_names(),
    pass = TRUE, value = 0, tolerance = 0, detail = "synthetic",
    stringsAsFactors = FALSE
  )
  ordinary_v1_dlm_companion_write_csv(
    gates, file.path(directory, "reference_gates.csv")
  )
  ordinary_v1_dlm_companion_write_csv(
    data.frame(
      fixture_id = c(
        "fixed_W_local_level",
        "shared_component_scale_trend_regression"
      ),
      expected_missing_indices = c("6,17", "11"),
      detected_missing_indices = c("6,17", "11"),
      maximum_placeholder_invariance_error = 0,
      pass = TRUE,
      stringsAsFactors = FALSE
    ),
    file.path(directory, "canonical_missing_checks.csv")
  )
  ordinary_v1_dlm_companion_write_csv(
    data.frame(
      fixture_id = fixtures$fixture_id,
      mean_standardized_error = c(1, 1, 1),
      variance_standardized_error = c(1, 1, 1),
      repair_count = 0,
      interpretation_pass = TRUE,
      pass = TRUE,
      stringsAsFactors = FALSE
    ),
    file.path(directory, "public_future_root_checks.csv")
  )
  ordinary_v1_dlm_companion_write_csv(
    data.frame(
      scale_profile = 1:2,
      scale_values = c("0.025,0.025", "0.1,0.1"),
      mean_standardized_error = c(1, 0.5),
      variance_standardized_error = c(1, 0.5),
      stringsAsFactors = FALSE
    ),
    file.path(directory, "varying_component_scale_future_checks.csv")
  )
  ordinary_v1_dlm_companion_write_csv(
    data.frame(
      draw = rep(1:3, each = 2),
      component = rep(c("trend", "regression"), 3),
      saved_shape = c(3, 2, 3, 2, 3, 2),
      recomputed_shape = c(3, 2, 3, 2, 3, 2),
      saved_rate = c(2, 1, 2, 1, 2, 1),
      recomputed_rate = c(2, 1, 2, 1, 2, 1),
      stringsAsFactors = FALSE
    ),
    file.path(directory, "component_scale_conditionals.csv")
  )
  cells <- expand.grid(
    learning_rate_mode = c(
      "fixed_rate", "learned_pseudoresidual_normalized"
    ),
    fixture_id = fixtures$fixture_id,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  cells <- cells[, c("fixture_id", "learning_rate_mode")]
  cells$seed <- seq_len(nrow(cells))
  cells$all_saved_stochastic_fields_bitwise <- TRUE
  cells$time0_draws_complete <- TRUE
  cells$estimand_schema_complete <- TRUE
  cells$final_checkpoint_bitwise <- TRUE
  cells$three_segment_history <- TRUE
  cells$full_checkpoint_digest <- contract$digest_a
  cells$segmented_checkpoint_digest <- contract$digest_a
  cells$continuation_history_digest <- contract$digest_b
  ordinary_v1_dlm_companion_write_csv(
    cells, file.path(directory, "continuation_cells.csv")
  )
  ordinary_v1_dlm_companion_write_csv(
    rbind(
      do.call(rbind, lapply(0:1, function(generation) {
        do.call(rbind, lapply(
          c(
            "generation", "segment_numerical_repair_count",
            "cumulative_numerical_repair_count"
          ),
          function(field) {
            data.frame(
              generation = generation, field = field,
              value = c("0.5", "-0.5", "Inf", "2147483648"),
              rejected = TRUE, stringsAsFactors = FALSE
            )
          }
        ))
      })),
      data.frame(
        generation = NA_integer_,
        field = c(
          "generation0_target_status",
          "generation0_mismatch_without_override",
          "generation1_backend_without_mismatch"
        ),
        value = "semantic", rejected = TRUE,
        stringsAsFactors = FALSE
      )
    ),
    file.path(directory, "continuation_history_mutations.csv")
  )
  ordinary_v1_dlm_companion_write_csv(
    data.frame(
      recorded_at = character(), mode = character(), stage = character(),
      fixture_id = character(), learning_rate_mode = character(),
      chain = integer(), message = character(),
      stringsAsFactors = FALSE
    ),
    file.path(directory, "failure_log.csv")
  )
  ordinary_v1_dlm_companion_write_csv(
    data.frame(
      metric = c(
        "sampled_process_group_peak_processes",
        "sampled_process_group_peak_threads",
        "sampled_process_group_peak_rss_kib",
        "hard_timeout_triggered", "sampled_limit_triggered",
        "monitor_error", "pgid_query_error", "finalizer_error",
        "signal_received", "final_pgid_empty", "runner_exit_status",
        "monitor_fault_test_pass", "pgid_kill_escalation_used",
        "kernel_hard_memory_ceiling"
      ),
      value = c(
        "1", "1", "110", rep("FALSE", 5), "NONE", "TRUE", "0",
        "TRUE", "FALSE", "FALSE"
      ),
      limit = c(
        "3", "4", "4194304", rep("FALSE", 5), "NONE", "TRUE", "0",
        "TRUE", "FALSE", "FALSE"
      ),
      pass = TRUE,
      stringsAsFactors = FALSE
    ),
    file.path(directory, "resource_summary.csv")
  )
  ordinary_v1_dlm_companion_write_csv(
    data.frame(
      metric = c(
        "leader_exit_status", "descendant_seen_after_leader_exit",
        "pgid_drain_completed", "kill_escalation_used",
        "final_pgid_empty"
      ),
      value = c("17", "TRUE", "TRUE", "TRUE", "TRUE"),
      expected = c("17", "TRUE", "TRUE", "TRUE", "TRUE"),
      pass = TRUE,
      stringsAsFactors = FALSE
    ),
    file.path(directory, "monitor_fault_test.csv")
  )
  ordinary_v1_dlm_companion_write_csv(
    data.frame(
      elapsed_seconds = 0:2, processes = 1, threads = 1,
      rss_kib = c(100, 110, 105)
    ),
    file.path(directory, "process_group_monitor.csv")
  )
  closeout_fields <- c(
    "schema_version", "mode", "expected_primary_commit",
    "process_group_id", "runner_exit_status", "resource_pass",
    "monitor_kind", "kernel_hard_memory_ceiling", "signal_received",
    "final_pgid_empty", "finalizer_error", "completed_at"
  )
  closeout_values <- c(
    "rqrgibbs_dlm_wrapper_closeout/2.1.0", "reference-only",
    contract$commit, "123", "0", "TRUE", "pgid_sampled_fallback",
    "FALSE", "NONE", "TRUE", "FALSE", "2026-07-26T00:00:00Z"
  )
  ordinary_v1_dlm_companion_write_csv(
    data.frame(
      field = closeout_fields, value = closeout_values,
      stringsAsFactors = FALSE
    ),
    file.path(directory, "wrapper_closeout.csv")
  )
  saveRDS(
    list(kind = "synthetic dense reference"),
    file.path(directory, "dense_ffbs_reference.rds")
  )
  saveRDS(
    list(kind = "synthetic continuation reference"),
    file.path(directory, "continuation_reference_digests.rds")
  )
  writeLines("synthetic session", file.path(directory, "session_info.txt"))
  writeLines("synthetic stdout", file.path(directory, "runner.stdout.log"))
  writeLines("", file.path(directory, "runner.stderr.log"))

  runtime <- list(
    schema_version = "rqrgibbs_runtime_toolchain/1.0.0",
    R_version = "R synthetic", platform = "synthetic",
    compiler = NULL, BLAS = "synthetic", LAPACK = "synthetic",
    dependency_versions = list(
      digest = "0.6.39", jsonlite = "2.0.0",
      posterior = "1.7.0", rqrgibbs = contract$package_version
    ),
    primary_runtime_tree_digest = contract$runtime_tree,
    primary_runtime_attestation_sha256 = contract$attestation,
    digest = contract$toolchain
  )
  ordinary_v1_dlm_companion_write_json(
    runtime, file.path(directory, "runtime_toolchain.json")
  )
  bundle_files <- environment$rqr_dlm_companion_reference_bundle_file_names()
  bundle <- list(
    schema_version = "rqrgibbs_reference_bundle/2.0.0",
    primary_commit = contract$commit,
    config_digest = contract$reference_config,
    runtime_tree_digest = contract$runtime_tree,
    runtime_attestation_sha256 = contract$attestation,
    runtime_toolchain_digest = contract$toolchain,
    estimand_schema_version = "rqrgibbs_dlm_bounded_estimands/1.0.0",
    files = setNames(
      lapply(bundle_files, function(name) {
        ordinary_v1_dlm_companion_hash(file.path(directory, name))
      }),
      bundle_files
    )
  )
  ordinary_v1_dlm_companion_write_json(
    bundle, file.path(directory, "reference_bundle.json")
  )
  runtime_gates <- setNames(
    as.list(rep(TRUE, 10)),
    c(
      "runtime_attestation_match", "source_archive_tree_match",
      "source_package_verified", "source_package_archive_match",
      "build_evidence_verified", "install_evidence_verified",
      "runtime_lineage_marker_match", "runtime_install_receipt_match",
      "runtime_source_match", "reproducibility_eligible"
    )
  )
  run <- list(
    schema_version = "rqrgibbs_dlm_bounded_run/3.0.0",
    mode = "reference-only",
    config_id = "synthetic_reference",
    config_digest = contract$reference_config,
    primary_commit = contract$commit,
    primary_application_tree = contract$application_tree,
    primary_runtime_path = "/synthetic/isolated/runtime",
    primary_runtime_tree_digest = contract$runtime_tree,
    primary_runtime_attestation = "/synthetic/attestation.json",
    primary_runtime_attestation_schema =
      "rqrgibbs_runtime_attestation/5.0.0",
    primary_runtime_attestation_sha256 = contract$attestation,
    runtime_toolchain_digest = contract$toolchain,
    runtime_gates = runtime_gates,
    requested_fit_count = 24L,
    mcmc_schedule = list(
      chains = 4L, burn_in = 2000L,
      retained_per_chain = 6000L, thin = 1L
    ),
    estimand_schema_version = "rqrgibbs_dlm_bounded_estimands/1.0.0",
    future_primary_estimands =
      "deterministic_conditional_mean_interval_roots",
    stochastic_future_draws_role = "sidecar_only",
    full_chain_files_ignored = TRUE,
    generalized_bayes = TRUE,
    response_likelihood = FALSE,
    response_prediction_contract = FALSE,
    production_simulation = FALSE,
    process_tree_monitor_active = TRUE,
    process_tree_monitor_kind = "pgid_sampled_fallback",
    kernel_hard_memory_ceiling = FALSE,
    recorded_at = "2026-07-26T00:00:00Z",
    status = "passed",
    fixture_construction_passed = TRUE,
    reference_gates_executed = TRUE,
    reference_gate_count = 43L,
    reference_gate_pass_count = 43L,
    reference_gates_sha256 = ordinary_v1_dlm_companion_hash(
      file.path(directory, "reference_gates.csv")
    ),
    reference_bundle_sha256 = ordinary_v1_dlm_companion_hash(
      file.path(directory, "reference_bundle.json")
    ),
    bounded_dynamic_execution_authorized = FALSE
  )
  ordinary_v1_dlm_companion_write_json(
    run, file.path(directory, "run_manifest.json")
  )
  ordinary_v1_dlm_companion_refresh_artifacts(
    environment, directory, "dlm_reference"
  )
  invisible(directory)
}

ordinary_v1_dlm_companion_thread_environment <- function() {
  as.list(setNames(
    rep("1", 5),
    c(
      "OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS",
      "VECLIB_MAXIMUM_THREADS", "NUMEXPR_NUM_THREADS"
    )
  ))
}

ordinary_v1_dlm_companion_make_m01 <- function(
    environment, directory, contract) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  tasks <- seq_len(20)
  sentinel <- tasks > 12
  chains <- ifelse(sentinel, 4L, 1L)
  DGP <- rep(c("S01", "S02"), 10)
  estimands <- environment$rqr_dlm_companion_estimand_names("M01")
  diagnostics <- do.call(rbind, lapply(tasks, function(task) {
    data.frame(
      estimand = estimands, chains = chains[[task]],
      rhat = if (sentinel[[task]]) 1 else NA_real_,
      ess_bulk = 2000, ess_tail = 2000,
      mcse_mean = 0.01,
      mcse_over_sd = if (sentinel[[task]]) 0.1 else 0.01,
      pass = TRUE,
      DGP = DGP[[task]], replication = task,
      sentinel = sentinel[[task]], stringsAsFactors = FALSE
    )
  }))
  rownames(diagnostics) <- NULL
  summary <- data.frame(
    DGP = DGP, replication = tasks,
    sentinel = sentinel, chains = chains,
    diagnostics = length(estimands),
    diagnostics_passed = length(estimands), all_pass = TRUE,
    fit_elapsed_seconds = 1,
    log_q_1_rhat = ifelse(sentinel, 1, NA_real_),
    log_q_1_ess_bulk = 2000, log_q_1_ess_tail = 2000,
    log_q_1_mcse_over_sd = ifelse(sentinel, 0.1, 0.01),
    stringsAsFactors = FALSE
  )
  ordinary_v1_dlm_companion_write_csv(
    diagnostics, file.path(directory, "wave1_M01_diagnostics.csv")
  )
  ordinary_v1_dlm_companion_write_csv(
    summary, file.path(directory, "wave1_M01_summary.csv")
  )
  saveRDS(
    list(kind = "synthetic M01 evidence"),
    file.path(directory, "wave1_M01_chain_evidence.rds")
  )
  manifest <- list(
    schema_version =
      "rqrgibbs_dlm_wave1_correction_validation/1.0.0",
    source_commit = contract$commit, source_clean = TRUE,
    package_version = contract$package_version,
    primary_runtime_attestation_sha256 = contract$attestation,
    config_digest = contract$confirmatory_config,
    incidence_digest = contract$incidence,
    seed_ledger_digest = contract$seed,
    wave_id = "static_gaussian_T200__target0200__sentinel",
    wave_task_count = 20L, chain_job_count = 44L, workers = 8L,
    thread_environment =
      ordinary_v1_dlm_companion_thread_environment(),
    component_scale_kernel = list(
      centered_inverse_gamma = TRUE,
      noncentered_slice_interweave = TRUE,
      interweave_cycles = 1L, slice_width = 1,
      slice_sweeps_per_cycle = 2L, slice_max_steps = 100L,
      slice_max_shrink = 1000L, target_change = FALSE
    ),
    standard_component_scale_schedule =
      list(burn = 1000L, retain = 6000L, thin = 1L),
    sentinel_component_scale_schedule =
      list(burn = 1000L, retain = 2000L, thin = 1L),
    exact_target_preserving_kernel = TRUE,
    comparative_simulation_metrics_used = FALSE,
    failed_outputs_reused = FALSE, all_fits_succeeded = TRUE,
    all_fits_reproducibility_eligible = TRUE,
    unique_runtime_tree_digests = contract$runtime_tree,
    total_fit_elapsed_seconds = 20,
    all_diagnostics_passed = TRUE,
    started_at_utc = "2026-07-26T00:00:00Z",
    completed_at_utc = "2026-07-26T00:01:00Z"
  )
  ordinary_v1_dlm_companion_write_json(
    manifest, file.path(directory, "validation_manifest.json")
  )
  ordinary_v1_dlm_companion_refresh_artifacts(
    environment, directory, "M01"
  )
  invisible(directory)
}

ordinary_v1_dlm_companion_make_m02 <- function(
    environment, directory, contract) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  tasks <- seq_len(20)
  sentinel <- tasks > 12
  chains <- ifelse(sentinel, 4L, 1L)
  DGP <- rep(c("S01", "S02"), 10)
  estimands <- environment$rqr_dlm_companion_estimand_names("M02")
  diagnostics <- do.call(rbind, lapply(tasks, function(task) {
    data.frame(
      estimand = estimands, chains = chains[[task]],
      rhat = if (sentinel[[task]]) 1 else NA_real_,
      ess_bulk = 2000, ess_tail = 2000,
      mcse_mean = 0.01, mcse_over_sd = 0.01, pass = TRUE,
      DGP = DGP[[task]], replication = task,
      sentinel = sentinel[[task]], stringsAsFactors = FALSE
    )
  }))
  rownames(diagnostics) <- NULL
  summary <- data.frame(
    DGP = DGP, replication = tasks,
    sentinel = sentinel, chains = chains,
    diagnostics = length(estimands),
    diagnostics_passed = length(estimands), all_pass = TRUE,
    fit_elapsed_seconds = 1, minimum_bulk_ess = 2000,
    minimum_tail_ess = 2000, maximum_mcse_over_sd = 0.01,
    stringsAsFactors = FALSE
  )
  ordinary_v1_dlm_companion_write_csv(
    diagnostics, file.path(directory, "wave1_M02_diagnostics.csv")
  )
  ordinary_v1_dlm_companion_write_csv(
    summary, file.path(directory, "wave1_M02_summary.csv")
  )
  saveRDS(
    list(kind = "synthetic M02 evidence"),
    file.path(directory, "wave1_M02_chain_evidence.rds")
  )
  manifest <- list(
    schema_version =
      "rqrgibbs_dlm_wave1_comparator_projection_validation/1.0.0",
    source_commit = contract$commit, source_clean = TRUE,
    package_version = contract$package_version,
    primary_runtime_attestation_sha256 = contract$attestation,
    primary_reproducibility_eligible = TRUE,
    exdqlm_runtime_attestation_sha256 = contract$exdqlm_attestation,
    exdqlm_runtime_tree_digest = contract$exdqlm_tree,
    exdqlm_source_package_sha256 = contract$exdqlm_package,
    config_digest = contract$confirmatory_config,
    incidence_digest = contract$incidence,
    seed_ledger_digest = contract$seed,
    wave_id = "static_gaussian_T200__target0200__sentinel",
    wave_task_count = 20L, interval_chain_job_count = 44L,
    logical_endpoint_fit_count = 88L, workers = 8L,
    thread_environment =
      ordinary_v1_dlm_companion_thread_environment(),
    comparator_projection =
      "colSums(FF * posterior_state_mean_or_draw)",
    comparative_simulation_metrics_used = FALSE,
    failed_outputs_reused = FALSE, all_fits_succeeded = TRUE,
    all_diagnostics_passed = TRUE, total_fit_elapsed_seconds = 20,
    started_at_utc = "2026-07-26T00:00:00Z",
    completed_at_utc = "2026-07-26T00:01:00Z"
  )
  ordinary_v1_dlm_companion_write_json(
    manifest, file.path(directory, "validation_manifest.json")
  )
  ordinary_v1_dlm_companion_refresh_artifacts(
    environment, directory, "M02"
  )
  invisible(directory)
}

ordinary_v1_dlm_companion_make_horizon <- function(
    environment, directory, contract) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  horizon <- data.frame(
    DGP = sprintf("S%02d", 1:16),
    state_dimension = c(
      2, 2, 1, 1, 1, 1, 4, 4, 4, 3, 3, 2, 1, 1, 1, 1
    ),
    training_horizon_expected = c(rep(200, 14), 100, 400),
    training_horizon_observed = c(rep(200, 14), 100, 400),
    future_horizon_expected = 20,
    future_horizon_observed = 20,
    full_horizon_expected = c(rep(220, 14), 120, 420),
    full_horizon_observed = c(rep(220, 14), 120, 420),
    training_partition_exact = TRUE,
    future_partition_exact = TRUE, pass = TRUE,
    stringsAsFactors = FALSE
  )
  estimands <- environment$rqr_dlm_companion_estimand_names("M03")
  replications <- seq(1L, 15L, by = 2L)
  diagnostics <- do.call(rbind, lapply(seq_len(8), function(task) {
    data.frame(
      estimand = estimands, chains = 1L, rhat = NA,
      ess_bulk = 2000, ess_tail = 2000, mcse_mean = 0.01,
      mcse_over_sd = 0.01, pass = TRUE, DGP = "S01",
      replication = replications[[task]], stringsAsFactors = FALSE
    )
  }))
  rownames(diagnostics) <- NULL
  summary <- data.frame(
    DGP = "S01", replication = replications,
    fit_succeeded = TRUE, diagnostics = length(estimands),
    diagnostics_passed = length(estimands),
    all_diagnostics_passed = TRUE, minimum_bulk_ess = 2000,
    minimum_tail_ess = 2000, maximum_mcse_over_sd = 0.01,
    elapsed_seconds = 1,
    stringsAsFactors = FALSE
  )
  endpoint <- data.frame(
    DGP = "S03", replication = 13, method = "M01",
    training_horizon_expected = 200,
    training_lower_length = 200, training_upper_length = 200,
    future_horizon_expected = 20,
    future_lower_length = 20, future_upper_length = 20,
    numerical_repairs = 0, exact_joint_target = TRUE,
    reproducibility_eligible = TRUE, elapsed_seconds = 1,
    error_message = "", pass = TRUE,
    stringsAsFactors = FALSE
  )
  ordinary_v1_dlm_companion_write_csv(
    horizon, file.path(directory, "horizon_checks.csv")
  )
  ordinary_v1_dlm_companion_write_csv(
    diagnostics, file.path(directory, "fixed_design_diagnostics.csv")
  )
  ordinary_v1_dlm_companion_write_csv(
    summary, file.path(directory, "fixed_design_summary.csv")
  )
  ordinary_v1_dlm_companion_write_csv(
    endpoint, file.path(directory, "dynamic_endpoint_check.csv")
  )
  saveRDS(
    list(kind = "synthetic fixed-design evidence"),
    file.path(directory, "fixed_design_chain_evidence.rds")
  )
  manifest <- list(
    schema_version =
      "rqrgibbs_dlm_horizon_fixed_design_validation/1.0.0",
    source_commit = contract$commit, source_clean = TRUE,
    package_version = contract$package_version,
    primary_runtime_attestation_sha256 = contract$attestation,
    config_digest = contract$confirmatory_config,
    incidence_digest = contract$incidence,
    seed_ledger_digest = contract$seed,
    horizon_scenarios = 16L, horizon_checks_passed = TRUE,
    fixed_design_standard_tasks = 8L,
    fixed_design_standard_schedule =
      list(burn = 500L, retain = 3000L, thin = 1L),
    fixed_design_fits_succeeded = TRUE,
    fixed_design_diagnostics_passed = TRUE,
    fixed_design_reproducibility_eligible = TRUE,
    dynamic_endpoint_check_passed = TRUE,
    comparative_simulation_metrics_used = FALSE,
    failed_outputs_reused = FALSE, workers = 8L,
    thread_environment =
      ordinary_v1_dlm_companion_thread_environment(),
    started_at_utc = "2026-07-26T00:00:00Z",
    completed_at_utc = "2026-07-26T00:01:00Z"
  )
  ordinary_v1_dlm_companion_write_json(
    manifest, file.path(directory, "validation_manifest.json")
  )
  ordinary_v1_dlm_companion_refresh_artifacts(
    environment, directory, "horizon_M03"
  )
  invisible(directory)
}

ordinary_v1_dlm_companion_contract <- function() {
  list(
    commit = paste(rep("a", 40), collapse = ""),
    package_version = "0.1.0.9999",
    runtime_tree = paste(rep("b", 64), collapse = ""),
    attestation = paste(rep("c", 64), collapse = ""),
    toolchain = paste(rep("d", 64), collapse = ""),
    application_tree = paste(rep("1", 40), collapse = ""),
    reference_config = paste(rep("2", 64), collapse = ""),
    confirmatory_config = paste(rep("3", 64), collapse = ""),
    incidence = paste(rep("4", 64), collapse = ""),
    seed = paste(rep("5", 64), collapse = ""),
    exdqlm_attestation = paste(rep("6", 64), collapse = ""),
    exdqlm_tree = paste(rep("7", 64), collapse = ""),
    exdqlm_package = paste(rep("8", 64), collapse = ""),
    digest_a = paste(rep("d", 64), collapse = ""),
    digest_b = paste(rep("e", 64), collapse = ""),
    digest_c = paste(rep("f", 64), collapse = ""),
    digest_d = paste(rep("a", 64), collapse = "")
  )
}

ordinary_v1_dlm_companion_fixture <- function(
    environment, root = tempfile("protected-dlm-companion-")) {
  contract <- ordinary_v1_dlm_companion_contract()
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  paths <- list(
    root = root,
    reference = file.path(root, "reference"),
    M01 = file.path(root, "M01"),
    M02 = file.path(root, "M02"),
    horizon = file.path(root, "horizon_M03"),
    output = file.path(root, "output")
  )
  ordinary_v1_dlm_companion_make_reference(
    environment, paths$reference, contract
  )
  ordinary_v1_dlm_companion_make_m01(
    environment, paths$M01, contract
  )
  ordinary_v1_dlm_companion_make_m02(
    environment, paths$M02, contract
  )
  ordinary_v1_dlm_companion_make_horizon(
    environment, paths$horizon, contract
  )
  list(paths = paths, contract = contract)
}

ordinary_v1_dlm_companion_run_fixture <- function(
    environment, fixture) {
  environment$rqr_dlm_companion_bundle(
    repo_root = normalizePath(
      testthat::test_path("..", "..", ".."),
      winslash = "/", mustWork = TRUE
    ),
    expected_commit = fixture$contract$commit,
    reference_directory = fixture$paths$reference,
    m01_directory = fixture$paths$M01,
    m02_directory = fixture$paths$M02,
    horizon_directory = fixture$paths$horizon,
    output_directory = fixture$paths$output,
    collector_source_commit = fixture$contract$commit,
    require_ignored_output = FALSE
  )
}

ordinary_v1_dlm_companion_refresh_compact <- function(
    environment, directory) {
  files <- sort(setdiff(
    environment$rqr_dlm_companion_compact_files(),
    "artifact_hashes.csv"
  ))
  value <- data.frame(
    schema_version = environment$rqr_dlm_companion_schema(),
    relative_path = files,
    byte_count = as.numeric(file.info(file.path(directory, files))$size),
    sha256 = vapply(
      file.path(directory, files),
      ordinary_v1_dlm_companion_hash, character(1L)
    ),
    stringsAsFactors = FALSE
  )
  ordinary_v1_dlm_companion_write_csv(
    value, file.path(directory, "artifact_hashes.csv")
  )
  invisible(directory)
}

testthat::test_that(
  "protected-DLM companion emits compact, closed, non-fitting evidence", {
    testthat::skip_if_not_installed("digest")
    testthat::skip_if_not_installed("jsonlite")
    environment <- ordinary_v1_dlm_companion_environment()
    fixture <- ordinary_v1_dlm_companion_fixture(environment)
    on.exit(unlink(fixture$paths$root, recursive = TRUE, force = TRUE))

    result <- ordinary_v1_dlm_companion_run_fixture(environment, fixture)
    expected <- c(
      "artifact_hashes.csv", "bundle_manifest.json",
      "input_artifact_hashes.csv", "input_bundle_summary.csv",
      "semantic_gates.csv"
    )
    testthat::expect_identical(
      sort(list.files(result$output_directory)), expected
    )
    manifest <- jsonlite::read_json(
      file.path(result$output_directory, "bundle_manifest.json"),
      simplifyVector = FALSE
    )
    testthat::expect_identical(
      manifest$schema_version, environment$rqr_dlm_companion_schema()
    )
    testthat::expect_true(manifest$all_semantic_gates_passed)
    testthat::expect_false(manifest$fits_executed_by_collector)
    testthat::expect_false(manifest$heavy_input_artifacts_copied)
    testthat::expect_true(manifest$generalized_bayes)
    testthat::expect_false(manifest$response_likelihood)
    testthat::expect_false(manifest$response_prediction_contract)
    input_hashes <- utils::read.csv(
      file.path(result$output_directory, "input_artifact_hashes.csv"),
      stringsAsFactors = FALSE
    )
    testthat::expect_equal(nrow(input_hashes), 39L)
    testthat::expect_true(any(grepl("\\.rds$", input_hashes$relative_path)))
    testthat::expect_false(any(grepl(
      "\\.rds$", list.files(result$output_directory)
    )))
    gates <- utils::read.csv(
      file.path(result$output_directory, "semantic_gates.csv"),
      stringsAsFactors = FALSE
    )
    testthat::expect_equal(nrow(gates), 16L)
    testthat::expect_true(all(gates$status == "pass"))
    compact <- environment$rqr_dlm_companion_validate_compact(
      result$output_directory, fixture$contract$commit,
      fixture$contract$runtime_tree, fixture$contract$attestation,
      fixture$contract$package_version
    )
    testthat::expect_identical(
      compact$manifest$expected_primary_commit, fixture$contract$commit
    )
    withr::local_envvar(
      c(RQR_ORDINARY_V1_SOURCE_ONLY = "YES")
    )
    runner <- new.env(parent = globalenv())
    repo_root <- normalizePath(
      testthat::test_path("..", "..", ".."),
      winslash = "/", mustWork = TRUE
    )
    sys.source(
      file.path(
        repo_root, "application", "scripts",
        "25_validate_rqr_ordinary_v1.R"
      ),
      envir = runner
    )
    config <- runner$rqr_ordinary_v1_load_config(repo_root)
    config$ordinary_v1_execute_enabled <- TRUE
    config$reviewed_implementation_commit <- fixture$contract$commit
    benchmark_directory <- file.path(fixture$paths$root, "benchmark")
    dir.create(benchmark_directory)
    utils::write.csv(
      data.frame(
        package = "rqrgibbs",
        version = fixture$contract$package_version,
        source_commit = fixture$contract$commit,
        runtime_tree_digest = fixture$contract$runtime_tree,
        runtime_source_match = TRUE,
        reproducibility_eligible = TRUE,
        attestation_sha256 = fixture$contract$attestation,
        stringsAsFactors = FALSE
      ),
      file.path(benchmark_directory, "runtime_attestations.csv"),
      row.names = FALSE
    )
    integrated <- runner$rqr_ordinary_v1_validate_dlm_companion(
      config, repo_root, benchmark_directory,
      result$output_directory
    )
    testthat::expect_identical(integrated$status, "pass")
    testthat::expect_identical(
      integrated$compact_artifact_manifest_sha256,
      ordinary_v1_dlm_companion_hash(file.path(
        result$output_directory, "artifact_hashes.csv"
      ))
    )
    output_hashes <- utils::read.csv(
      file.path(result$output_directory, "artifact_hashes.csv"),
      stringsAsFactors = FALSE
    )
    testthat::expect_identical(
      as.character(output_hashes$relative_path), expected[-1L]
    )
    testthat::expect_error(
      ordinary_v1_dlm_companion_run_fixture(environment, fixture),
      "must be one new"
    )
    ignored_output <- tempfile(
      "protected-dlm-ignored-boundary-",
      tmpdir = file.path(repo_root, "application", "cache")
    )
    testthat::expect_identical(
      environment$rqr_dlm_companion_prepare_output(
        repo_root, ignored_output, require_ignored = TRUE
      ),
      ignored_output
    )
  }
)

testthat::test_that(
  "protected-DLM companion rejects structural and semantic tampering", {
    testthat::skip_if_not_installed("digest")
    testthat::skip_if_not_installed("jsonlite")
    environment <- ordinary_v1_dlm_companion_environment()
    roots <- character()
    on.exit(unlink(roots, recursive = TRUE, force = TRUE), add = TRUE)

    make_case <- function() {
      fixture <- ordinary_v1_dlm_companion_fixture(environment)
      roots <<- c(roots, fixture$paths$root)
      fixture
    }

    fixture <- make_case()
    writeLines("extra", file.path(fixture$paths$reference, "extra.txt"))
    testthat::expect_error(
      ordinary_v1_dlm_companion_run_fixture(environment, fixture),
      "missing, extra"
    )

    fixture <- make_case()
    unlink(file.path(fixture$paths$reference, "runner.stdout.log"))
    testthat::expect_true(file.symlink(
      "/dev/null", file.path(fixture$paths$reference, "runner.stdout.log")
    ))
    testthat::expect_error(
      ordinary_v1_dlm_companion_run_fixture(environment, fixture),
      "symlinked"
    )

    fixture <- make_case()
    gates_path <- file.path(fixture$paths$reference, "reference_gates.csv")
    gates <- utils::read.csv(gates_path, stringsAsFactors = FALSE)
    gates$pass[[1L]] <- FALSE
    ordinary_v1_dlm_companion_write_csv(gates, gates_path)
    ordinary_v1_dlm_companion_refresh_reference(
      environment, fixture$paths$reference
    )
    testthat::expect_error(
      ordinary_v1_dlm_companion_run_fixture(environment, fixture),
      "gates are incomplete"
    )

    fixture <- make_case()
    continuation_path <- file.path(
      fixture$paths$reference, "continuation_cells.csv"
    )
    continuation <- utils::read.csv(
      continuation_path, stringsAsFactors = FALSE
    )
    ordinary_v1_dlm_companion_write_csv(
      continuation[-1L, ], continuation_path
    )
    ordinary_v1_dlm_companion_refresh_reference(
      environment, fixture$paths$reference
    )
    testthat::expect_error(
      ordinary_v1_dlm_companion_run_fixture(environment, fixture),
      "six complete"
    )

    fixture <- make_case()
    mutation_path <- file.path(
      fixture$paths$reference, "continuation_history_mutations.csv"
    )
    mutations <- utils::read.csv(
      mutation_path, stringsAsFactors = FALSE
    )
    mutations[] <- mutations[rep(1L, nrow(mutations)), ]
    ordinary_v1_dlm_companion_write_csv(mutations, mutation_path)
    ordinary_v1_dlm_companion_refresh_reference(
      environment, fixture$paths$reference
    )
    testthat::expect_error(
      ordinary_v1_dlm_companion_run_fixture(environment, fixture),
      "history-mutation gate"
    )

    fixture <- make_case()
    missing_path <- file.path(
      fixture$paths$reference, "canonical_missing_checks.csv"
    )
    missing <- utils::read.csv(missing_path, stringsAsFactors = FALSE)
    missing[2L, ] <- missing[1L, ]
    ordinary_v1_dlm_companion_write_csv(missing, missing_path)
    ordinary_v1_dlm_companion_refresh_reference(
      environment, fixture$paths$reference
    )
    testthat::expect_error(
      ordinary_v1_dlm_companion_run_fixture(environment, fixture),
      "missing, future, or component-scale"
    )

    fixture <- make_case()
    conditional_path <- file.path(
      fixture$paths$reference, "component_scale_conditionals.csv"
    )
    conditional <- utils::read.csv(
      conditional_path, stringsAsFactors = FALSE
    )
    conditional$recomputed_rate[[1L]] <- 999
    ordinary_v1_dlm_companion_write_csv(conditional, conditional_path)
    ordinary_v1_dlm_companion_refresh_reference(
      environment, fixture$paths$reference
    )
    testthat::expect_error(
      ordinary_v1_dlm_companion_run_fixture(environment, fixture),
      "component-scale semantic"
    )

    fixture <- make_case()
    m01_manifest_path <- file.path(
      fixture$paths$M01, "validation_manifest.json"
    )
    m01_manifest <- jsonlite::read_json(
      m01_manifest_path, simplifyVector = FALSE
    )
    m01_manifest$component_scale_kernel$
      noncentered_slice_interweave <- FALSE
    ordinary_v1_dlm_companion_write_json(
      m01_manifest, m01_manifest_path
    )
    ordinary_v1_dlm_companion_refresh_artifacts(
      environment, fixture$paths$M01, "M01"
    )
    testthat::expect_error(
      ordinary_v1_dlm_companion_run_fixture(environment, fixture),
      "interweaving gate failed"
    )

    fixture <- make_case()
    m01_manifest_path <- file.path(
      fixture$paths$M01, "validation_manifest.json"
    )
    m01_manifest <- jsonlite::read_json(
      m01_manifest_path, simplifyVector = FALSE
    )
    m01_manifest$wave_task_count <- "20"
    m01_manifest$component_scale_kernel$slice_width <- 0L
    m01_manifest$component_scale_kernel$slice_max_steps <- 0L
    m01_manifest$component_scale_kernel$slice_max_shrink <- 0L
    ordinary_v1_dlm_companion_write_json(
      m01_manifest, m01_manifest_path
    )
    ordinary_v1_dlm_companion_refresh_artifacts(
      environment, fixture$paths$M01, "M01"
    )
    testthat::expect_error(
      ordinary_v1_dlm_companion_run_fixture(environment, fixture),
      "interweaving gate failed"
    )

    fixture <- make_case()
    m01_diagnostics_path <- file.path(
      fixture$paths$M01, "wave1_M01_diagnostics.csv"
    )
    m01_diagnostics <- utils::read.csv(
      m01_diagnostics_path, stringsAsFactors = FALSE
    )
    ordinary_v1_dlm_companion_write_csv(
      m01_diagnostics[-1L, ], m01_diagnostics_path
    )
    ordinary_v1_dlm_companion_refresh_artifacts(
      environment, fixture$paths$M01, "M01"
    )
    testthat::expect_error(
      ordinary_v1_dlm_companion_run_fixture(environment, fixture),
      "diagnostic grid"
    )

    fixture <- make_case()
    m01_diagnostics_path <- file.path(
      fixture$paths$M01, "wave1_M01_diagnostics.csv"
    )
    m01_diagnostics <- utils::read.csv(
      m01_diagnostics_path, stringsAsFactors = FALSE
    )
    m01_diagnostics$rhat[m01_diagnostics$chains == 4L] <- 99
    m01_diagnostics$ess_bulk <- 0
    m01_diagnostics$ess_tail <- 0
    m01_diagnostics$mcse_over_sd <- 99
    ordinary_v1_dlm_companion_write_csv(
      m01_diagnostics, m01_diagnostics_path
    )
    ordinary_v1_dlm_companion_refresh_artifacts(
      environment, fixture$paths$M01, "M01"
    )
    testthat::expect_error(
      ordinary_v1_dlm_companion_run_fixture(environment, fixture),
      "frozen pass rules"
    )

    fixture <- make_case()
    m01_summary_path <- file.path(
      fixture$paths$M01, "wave1_M01_summary.csv"
    )
    m01_summary <- utils::read.csv(
      m01_summary_path, stringsAsFactors = FALSE
    )
    m01_summary$log_q_1_ess_bulk[[1L]] <- 999
    ordinary_v1_dlm_companion_write_csv(m01_summary, m01_summary_path)
    ordinary_v1_dlm_companion_refresh_artifacts(
      environment, fixture$paths$M01, "M01"
    )
    testthat::expect_error(
      ordinary_v1_dlm_companion_run_fixture(environment, fixture),
      "log-q summary sidecars"
    )

    fixture <- make_case()
    m02_diagnostics_path <- file.path(
      fixture$paths$M02, "wave1_M02_diagnostics.csv"
    )
    m02_diagnostics <- utils::read.csv(
      m02_diagnostics_path, stringsAsFactors = FALSE
    )
    m02_diagnostics[2L, ] <- m02_diagnostics[1L, ]
    ordinary_v1_dlm_companion_write_csv(
      m02_diagnostics, m02_diagnostics_path
    )
    ordinary_v1_dlm_companion_refresh_artifacts(
      environment, fixture$paths$M02, "M02"
    )
    testthat::expect_error(
      ordinary_v1_dlm_companion_run_fixture(environment, fixture),
      "diagnostic grid"
    )

    fixture <- make_case()
    m02_summary_path <- file.path(
      fixture$paths$M02, "wave1_M02_summary.csv"
    )
    m02_summary <- utils::read.csv(
      m02_summary_path, stringsAsFactors = FALSE
    )
    m02_summary$minimum_bulk_ess[[1L]] <- 999
    ordinary_v1_dlm_companion_write_csv(m02_summary, m02_summary_path)
    ordinary_v1_dlm_companion_refresh_artifacts(
      environment, fixture$paths$M02, "M02"
    )
    testthat::expect_error(
      ordinary_v1_dlm_companion_run_fixture(environment, fixture),
      "diagnostic extrema"
    )

    fixture <- make_case()
    m02_summary_path <- file.path(
      fixture$paths$M02, "wave1_M02_summary.csv"
    )
    m02_diagnostics_path <- file.path(
      fixture$paths$M02, "wave1_M02_diagnostics.csv"
    )
    m02_summary <- utils::read.csv(
      m02_summary_path, stringsAsFactors = FALSE
    )
    m02_diagnostics <- utils::read.csv(
      m02_diagnostics_path, stringsAsFactors = FALSE
    )
    changed_replication <- m02_summary$replication[[1L]]
    m02_summary$DGP[[1L]] <- "S02"
    selected <- m02_diagnostics$replication == changed_replication &
      m02_diagnostics$DGP == "S01"
    m02_diagnostics$DGP[selected] <- "S02"
    ordinary_v1_dlm_companion_write_csv(
      m02_summary, m02_summary_path
    )
    ordinary_v1_dlm_companion_write_csv(
      m02_diagnostics, m02_diagnostics_path
    )
    ordinary_v1_dlm_companion_refresh_artifacts(
      environment, fixture$paths$M02, "M02"
    )
    testthat::expect_error(
      ordinary_v1_dlm_companion_run_fixture(environment, fixture),
      "task-key evidence is inconsistent"
    )

    fixture <- make_case()
    m02_manifest_path <- file.path(
      fixture$paths$M02, "validation_manifest.json"
    )
    m02_manifest <- jsonlite::read_json(
      m02_manifest_path, simplifyVector = FALSE
    )
    m02_manifest$primary_runtime_attestation_sha256 <-
      paste(rep("f", 64), collapse = "")
    ordinary_v1_dlm_companion_write_json(
      m02_manifest, m02_manifest_path
    )
    ordinary_v1_dlm_companion_refresh_artifacts(
      environment, fixture$paths$M02, "M02"
    )
    testthat::expect_error(
      ordinary_v1_dlm_companion_run_fixture(environment, fixture),
      "runtime"
    )

    fixture <- make_case()
    horizon_path <- file.path(fixture$paths$horizon, "horizon_checks.csv")
    horizon <- utils::read.csv(horizon_path, stringsAsFactors = FALSE)
    horizon$pass[[1L]] <- FALSE
    ordinary_v1_dlm_companion_write_csv(horizon, horizon_path)
    ordinary_v1_dlm_companion_refresh_artifacts(
      environment, fixture$paths$horizon, "horizon_M03"
    )
    testthat::expect_error(
      ordinary_v1_dlm_companion_run_fixture(environment, fixture),
      "semantic evidence failed"
    )

    fixture <- make_case()
    m03_summary_path <- file.path(
      fixture$paths$horizon, "fixed_design_summary.csv"
    )
    m03_summary <- utils::read.csv(
      m03_summary_path, stringsAsFactors = FALSE
    )
    m03_summary$maximum_mcse_over_sd[[1L]] <- 999
    ordinary_v1_dlm_companion_write_csv(m03_summary, m03_summary_path)
    ordinary_v1_dlm_companion_refresh_artifacts(
      environment, fixture$paths$horizon, "horizon_M03"
    )
    testthat::expect_error(
      ordinary_v1_dlm_companion_run_fixture(environment, fixture),
      "diagnostic extrema"
    )

    fixture <- make_case()
    endpoint_path <- file.path(
      fixture$paths$horizon, "dynamic_endpoint_check.csv"
    )
    endpoint <- utils::read.csv(
      endpoint_path, stringsAsFactors = FALSE
    )
    endpoint$training_horizon_expected <- 999L
    endpoint$training_lower_length <- 0L
    endpoint$training_upper_length <- 1L
    endpoint$error_message <- "bogus success"
    ordinary_v1_dlm_companion_write_csv(endpoint, endpoint_path)
    ordinary_v1_dlm_companion_refresh_artifacts(
      environment, fixture$paths$horizon, "horizon_M03"
    )
    testthat::expect_error(
      ordinary_v1_dlm_companion_run_fixture(environment, fixture),
      "dynamic-endpoint semantic evidence"
    )

    fixture <- make_case()
    resource_path <- file.path(
      fixture$paths$reference, "resource_summary.csv"
    )
    resource <- utils::read.csv(resource_path, stringsAsFactors = FALSE)
    resource$metric <- paste0("fake_", seq_len(nrow(resource)))
    resource$pass <- 1
    ordinary_v1_dlm_companion_write_csv(resource, resource_path)
    ordinary_v1_dlm_companion_refresh_reference(
      environment, fixture$paths$reference
    )
    testthat::expect_error(
      ordinary_v1_dlm_companion_run_fixture(environment, fixture),
      "wrapper, resource"
    )

    fixture <- make_case()
    fixture$paths$output <- file.path(
      fixture$paths$reference, "nested-output"
    )
    testthat::expect_error(
      ordinary_v1_dlm_companion_run_fixture(environment, fixture),
      "must not be nested"
    )

    fixture <- make_case()
    m01_manifest_path <- file.path(
      fixture$paths$M01, "validation_manifest.json"
    )
    m01_manifest <- jsonlite::read_json(
      m01_manifest_path, simplifyVector = FALSE
    )
    m01_manifest$source_commit <- paste(rep("b", 40), collapse = "")
    ordinary_v1_dlm_companion_write_json(
      m01_manifest, m01_manifest_path
    )
    ordinary_v1_dlm_companion_refresh_artifacts(
      environment, fixture$paths$M01, "M01"
    )
    testthat::expect_error(
      ordinary_v1_dlm_companion_run_fixture(environment, fixture),
      "source"
    )

    ledger_root <- tempfile("protected-dlm-ledger-")
    dir.create(ledger_root)
    roots <- c(roots, ledger_root)
    ledger_path <- file.path(ledger_root, "evidence.txt")
    writeLines("before", ledger_path)
    ledger <- data.frame(
      input_role = "role",
      relative_path = "evidence.txt",
      byte_count = as.numeric(file.info(ledger_path)$size),
      sha256 = ordinary_v1_dlm_companion_hash(ledger_path),
      stringsAsFactors = FALSE
    )
    writeLines("after", ledger_path)
    testthat::expect_error(
      environment$rqr_dlm_companion_assert_input_ledger(
        list(role = list(directory = ledger_root)), ledger
      ),
      "changed during collection"
    )
  }
)

testthat::test_that(
  "compact companion validator rejects rehashed internal inconsistencies", {
    testthat::skip_if_not_installed("digest")
    testthat::skip_if_not_installed("jsonlite")
    environment <- ordinary_v1_dlm_companion_environment()
    fixture <- ordinary_v1_dlm_companion_fixture(environment)
    on.exit(unlink(fixture$paths$root, recursive = TRUE, force = TRUE))
    result <- ordinary_v1_dlm_companion_run_fixture(environment, fixture)

    validate <- function(directory) {
      environment$rqr_dlm_companion_validate_compact(
        directory, fixture$contract$commit,
        fixture$contract$runtime_tree, fixture$contract$attestation,
        fixture$contract$package_version
      )
    }
    copy_case <- function(name) {
      directory <- file.path(fixture$paths$root, name)
      dir.create(directory)
      files <- list.files(
        result$output_directory, full.names = TRUE, all.files = TRUE,
        no.. = TRUE
      )
      stopifnot(all(file.copy(files, directory)))
      directory
    }

    testthat::expect_silent(validate(result$output_directory))

    directory <- copy_case("compact-extra")
    writeLines("extra", file.path(directory, "extra.txt"))
    testthat::expect_error(validate(directory), "missing, extra")

    directory <- copy_case("compact-unhashed-change")
    writeLines(
      c(readLines(file.path(directory, "semantic_gates.csv")), "tamper"),
      file.path(directory, "semantic_gates.csv")
    )
    testthat::expect_error(validate(directory), "recursive hash mismatch")

    directory <- copy_case("compact-manifest")
    path <- file.path(directory, "bundle_manifest.json")
    manifest <- jsonlite::read_json(path, simplifyVector = FALSE)
    manifest$fits_executed_by_collector <- TRUE
    ordinary_v1_dlm_companion_write_json(manifest, path)
    ordinary_v1_dlm_companion_refresh_compact(environment, directory)
    testthat::expect_error(validate(directory), "scope, or count gate")

    directory <- copy_case("compact-summary")
    path <- file.path(directory, "input_bundle_summary.csv")
    summary <- utils::read.csv(path, stringsAsFactors = FALSE)
    summary$input_role[[2L]] <- summary$input_role[[1L]]
    ordinary_v1_dlm_companion_write_csv(summary, path)
    ordinary_v1_dlm_companion_refresh_compact(environment, directory)
    testthat::expect_error(validate(directory), "input summary")

    directory <- copy_case("compact-artifact-ledger")
    path <- file.path(directory, "input_artifact_hashes.csv")
    ledger <- utils::read.csv(path, stringsAsFactors = FALSE)
    ledger$listed_in_source_manifest[[1L]] <- TRUE
    ordinary_v1_dlm_companion_write_csv(ledger, path)
    ordinary_v1_dlm_companion_refresh_compact(environment, directory)
    testthat::expect_error(validate(directory), "input-artifact ledger")

    directory <- copy_case("compact-semantic")
    path <- file.path(directory, "semantic_gates.csv")
    semantic <- utils::read.csv(path, stringsAsFactors = FALSE)
    semantic$gate_id[[2L]] <- semantic$gate_id[[1L]]
    ordinary_v1_dlm_companion_write_csv(semantic, path)
    ordinary_v1_dlm_companion_refresh_compact(environment, directory)
    testthat::expect_error(validate(directory), "semantic gate table")

    testthat::expect_error(
      environment$rqr_dlm_companion_validate_compact(
        result$output_directory, fixture$contract$commit,
        paste(rep("f", 64), collapse = ""),
        fixture$contract$attestation, fixture$contract$package_version
      ),
      "source, runtime"
    )
  }
)
