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
    compiler = "compiler synthetic",
    BLAS = "synthetic", LAPACK = "synthetic",
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
    environment, directory, contract, role) {
  spec <- environment$rqr_dlm_companion_wave_spec(role)
  transition_kernel <-
    environment$rqr_dlm_companion_expected_m01_transition_kernel(role)
  transition_kernel_digest <- digest::digest(
    transition_kernel, algo = "sha256", serialize = TRUE
  )
  transition_invariant <-
    environment$rqr_dlm_companion_transition_kernel_invariant(
      transition_kernel
    )
  transition_invariant_digest <- digest::digest(
    transition_invariant, algo = "sha256", serialize = TRUE
  )
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  tasks <- seq_len(spec$task_count)
  sentinel <- tasks > spec$task_count - spec$sentinel_count
  chains <- ifelse(sentinel, 4L, 1L)
  DGP <- rep(spec$DGP, length.out = spec$task_count)
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
    maximum_peak_RSS_KiB = 1000,
    log_q_1_rhat = ifelse(sentinel, 1, NA_real_),
    log_q_1_ess_bulk = 2000, log_q_1_ess_tail = 2000,
    log_q_1_mcse_over_sd = ifelse(sentinel, 0.1, 0.01),
    transition_kernel_fit_count = chains,
    transition_kernel_schemas = vapply(
      chains, function(count) {
        paste(
          rep("rqrgibbs_dlm_transition_kernel/1.0.0", count),
          collapse = "|"
        )
      }, character(1L)
    ),
    transition_kernel_digests = vapply(
      chains, function(count) {
        paste(rep(transition_kernel_digest, count), collapse = "|")
      }, character(1L)
    ),
    transition_kernel_contract_digests = vapply(
      chains, function(count) {
        paste(rep(transition_kernel_digest, count), collapse = "|")
      }, character(1L)
    ),
    transition_kernel_contract_matches = vapply(
      chains, function(count) {
        paste(rep("TRUE", count), collapse = "|")
      }, character(1L)
    ),
    stringsAsFactors = FALSE
  )
  ordinary_v1_dlm_companion_write_csv(
    diagnostics,
    file.path(directory, paste0(spec$tag, "_M01_diagnostics.csv"))
  )
  ordinary_v1_dlm_companion_write_csv(
    summary, file.path(directory, paste0(spec$tag, "_M01_summary.csv"))
  )
  saveRDS(
    list(kind = "synthetic M01 evidence"),
    file.path(directory, paste0(spec$tag, "_M01_chain_evidence.rds"))
  )
  manifest <- list(
    schema_version =
      "rqrgibbs_dlm_wave_correction_validation/2.2.0",
    source_commit = contract$commit, source_clean = TRUE,
    package_version = contract$package_version,
    primary_runtime_attestation_sha256 = contract$attestation,
    config_digest = contract$confirmatory_config,
    incidence_digest = contract$incidence,
    seed_ledger_digest = contract$seed,
    wave_id = spec$wave_id,
    wave_task_count = spec$task_count,
    chain_job_count = spec$chain_count, workers = 8L,
    thread_environment =
      ordinary_v1_dlm_companion_thread_environment(),
    component_scale_kernel = list(
      one_root_partially_collapsed = TRUE,
      collapsed_integrated_root = "root1",
      centered_inverse_gamma = TRUE,
      noncentered_slice_interweave = TRUE,
      interweave_cycles = 1L, slice_width = 1,
      slice_sweeps_per_cycle = 3L, slice_max_steps = 100L,
      slice_max_shrink = 1000L, target_change = FALSE
    ),
    standard_component_scale_schedule =
      list(burn = 1000L, retain = 6000L, thin = 1L),
    sentinel_component_scale_schedule =
      list(burn = 1000L, retain = 6000L, thin = 1L),
    exact_target_preserving_kernel = TRUE,
    transition_kernel_schema =
      "rqrgibbs_dlm_transition_kernel/1.0.0",
    unique_transition_kernel_digests = transition_kernel_digest,
    expected_transition_kernel_contract = transition_kernel,
    expected_transition_kernel_contract_digest =
      transition_kernel_digest,
    transition_kernel_invariant_schema =
      "rqrgibbs_dlm_transition_kernel_invariant/1.0.0",
    expected_transition_kernel_invariant = transition_invariant,
    expected_transition_kernel_invariant_digest =
      transition_invariant_digest,
    all_fit_transition_contracts_complete = TRUE,
    all_fit_transition_contracts_match_expected = TRUE,
    comparative_simulation_metrics_used = FALSE,
    failed_outputs_reused = FALSE, all_fits_succeeded = TRUE,
    all_fits_reproducibility_eligible = TRUE,
    unique_runtime_tree_digests = contract$runtime_tree,
    total_fit_elapsed_seconds = spec$task_count,
    maximum_process_peak_RSS_KiB = 1000,
    declared_worker_memory_ceiling_KiB = 1048576,
    resource_margin_pass = TRUE,
    all_diagnostics_passed = TRUE,
    started_at_utc = "2026-07-26T00:00:00Z",
    completed_at_utc = "2026-07-26T00:01:00Z"
  )
  ordinary_v1_dlm_companion_write_json(
    manifest, file.path(directory, "validation_manifest.json")
  )
  ordinary_v1_dlm_companion_refresh_artifacts(
    environment, directory, role
  )
  invisible(directory)
}

ordinary_v1_dlm_companion_make_m02 <- function(
    environment, directory, contract, role) {
  spec <- environment$rqr_dlm_companion_wave_spec(role)
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  tasks <- seq_len(spec$task_count)
  sentinel <- tasks > spec$task_count - spec$sentinel_count
  chains <- ifelse(sentinel, 4L, 1L)
  DGP <- rep(spec$DGP, length.out = spec$task_count)
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
    sentinel = sentinel,
    schedule_role = ifelse(sentinel, "sentinel", "standard"),
    configured_burn = 1000L, configured_retain = 4000L,
    configured_thin = 1L, applied_burn = 1000L,
    applied_retain = 4000L, applied_thin = 1L,
    realized_state_draw_dimensions = "1x200x4000",
    realized_scale_draw_lengths = "4000",
    schedule_contract_pass = TRUE, chains = chains,
    diagnostics = length(estimands),
    diagnostics_passed = length(estimands), all_pass = TRUE,
    fit_elapsed_seconds = 1, maximum_peak_RSS_KiB = 1000,
    minimum_bulk_ess = 2000,
    minimum_tail_ess = 2000, maximum_mcse_over_sd = 0.01,
    stringsAsFactors = FALSE
  )
  ordinary_v1_dlm_companion_write_csv(
    diagnostics,
    file.path(directory, paste0(spec$tag, "_M02_diagnostics.csv"))
  )
  ordinary_v1_dlm_companion_write_csv(
    summary, file.path(directory, paste0(spec$tag, "_M02_summary.csv"))
  )
  saveRDS(
    list(kind = "synthetic M02 evidence"),
    file.path(directory, paste0(spec$tag, "_M02_chain_evidence.rds"))
  )
  manifest <- list(
    schema_version =
      "rqrgibbs_dlm_wave_comparator_projection_validation/2.1.0",
    source_commit = contract$commit, source_clean = TRUE,
    package_version = contract$package_version,
    primary_runtime_attestation_sha256 = contract$attestation,
    primary_reproducibility_eligible = TRUE,
    primary_runtime_tree_digest = contract$runtime_tree,
    exdqlm_runtime_attestation_sha256 = contract$exdqlm_attestation,
    exdqlm_runtime_tree_digest = contract$exdqlm_tree,
    exdqlm_source_package_sha256 = contract$exdqlm_package,
    config_digest = contract$confirmatory_config,
    incidence_digest = contract$incidence,
    seed_ledger_digest = contract$seed,
    wave_id = spec$wave_id,
    wave_task_count = spec$task_count,
    interval_chain_job_count = spec$chain_count,
    logical_endpoint_fit_count = spec$endpoint_count, workers = 8L,
    thread_environment =
      ordinary_v1_dlm_companion_thread_environment(),
    comparator_projection =
      "colSums(FF * posterior_state_mean_or_draw)",
    common_target_across_initialization_profiles = TRUE,
    overdispersed_initialization_profiles_verified = TRUE,
    frozen_schedules = list(
      standard = list(burn = 1000L, retain = 4000L, thin = 1L),
      sentinel = list(burn = 1000L, retain = 4000L, thin = 1L)
    ),
    applied_schedule_evidence = list(
      standard = list(
        configured_schedule =
          list(burn = 1000L, retain = 4000L, thin = 1L),
        interval_chain_job_count =
          spec$chain_count - 4L * spec$sentinel_count,
        realized_state_draw_dimensions = "1x200x4000",
        realized_scale_draw_lengths = 4000L,
        all_applied = TRUE
      ),
      sentinel = list(
        configured_schedule =
          list(burn = 1000L, retain = 4000L, thin = 1L),
        interval_chain_job_count = 4L * spec$sentinel_count,
        realized_state_draw_dimensions = "1x200x4000",
        realized_scale_draw_lengths = 4000L,
        all_applied = TRUE
      )
    ),
    all_applied_schedules_verified = TRUE,
    schedule_evidence_fields = c(
      "schedule_role", "applied_schedule", "schedule_applied",
      "state_draw_dimensions", "scale_draw_lengths"
    ),
    initialization_contract =
      "target_preserving_precomputed_mcmc_state",
    target_fields_held_fixed = paste0(
      "y;m0;C0;FF;GG;discounts;component_dimensions;",
      "dqlm_ind;fix_sigma;PriorSigma;quantile_probability"
    ),
    comparative_simulation_metrics_used = FALSE,
    failed_outputs_reused = FALSE, all_fits_succeeded = TRUE,
    all_diagnostics_passed = TRUE,
    total_fit_elapsed_seconds = spec$task_count,
    maximum_process_peak_RSS_KiB = 1000,
    declared_worker_memory_ceiling_KiB = 1048576,
    resource_margin_pass = TRUE,
    started_at_utc = "2026-07-26T00:00:00Z",
    completed_at_utc = "2026-07-26T00:01:00Z"
  )
  ordinary_v1_dlm_companion_write_json(
    manifest, file.path(directory, "validation_manifest.json")
  )
  ordinary_v1_dlm_companion_refresh_artifacts(
    environment, directory, role
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

ordinary_v1_dlm_companion_make_resource <- function(
    environment, directory, contract) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  shape <- data.frame(
    field = c(
      "samp.theta_root1", "samp.theta_root2",
      "samp.theta_terminal_root1", "samp.theta_terminal_root2",
      "samp.theta0_root1", "samp.theta0_root2",
      "samp.eta_root1", "samp.eta_root2", "samp.lambda",
      "samp.evolution_scale", "samp.evolution_scale_shape",
      "samp.evolution_scale_rate"
    ),
    orientation = c(
      "state_by_time_by_draw", "state_by_time_by_draw",
      "state_by_draw", "state_by_draw",
      "state_by_draw", "state_by_draw", "time_by_draw", "time_by_draw",
      "draw", "draw_by_component", "draw_by_component",
      "draw_by_component"
    ),
    dimension_formula = c(
      "p x T x retained", "p x T x retained",
      "p x retained", "p x retained",
      "p x retained", "p x retained", "T x retained", "T x retained",
      "retained", "retained x components", "retained x components",
      "retained x components"
    ),
    stringsAsFactors = FALSE
  )
  envelope <- data.frame(
    case = c(
      "four_state_component_scale",
      "three_state_learned_component_scale",
      "long_horizon_single_state"
    ),
    state_dimension = c(4L, 3L, 1L),
    training_horizon = c(200L, 200L, 400L),
    retained_draws = c(6000L, 9000L, 6000L),
    component_count = c(2L, 2L, 1L), chains = 4L,
    writer_peak_RSS_KiB = c(500000, 600000, 400000),
    clean_reader_peak_RSS_KiB = c(510000, 610000, 410000),
    bytes = c(100L, 200L, 300L),
    sha256 = c(contract$digest_a, contract$digest_b, contract$digest_c),
    writer_shape_valid = TRUE, clean_deserialization_valid = TRUE,
    declared_worker_memory_ceiling_KiB = 1048576,
    eighty_percent_margin_KiB = 0.8 * 1048576,
    serialization_margin_pass = TRUE,
    stringsAsFactors = FALSE
  )
  toolchain <- data.frame(
    key = c(
      "R_version", "platform", "R_compiler", "architecture",
      "operating_system", "CC", "CXX17", "R_Makeconf_sha256",
      "BLAS", "LAPACK", "package_rqrgibbs", "package_posterior",
      "package_digest", "package_jsonlite", "package_Rcpp",
      "package_RcppArmadillo"
    ),
    value = c(
      "R synthetic", "synthetic", "compiler synthetic",
      "synthetic architecture", "synthetic operating system",
      "synthetic CC", "synthetic CXX17", contract$digest_a,
      "synthetic", "synthetic", contract$package_version,
      "1.7.0", "0.6.39", "2.0.0", "1.1.0", "14.6.3-1"
    ),
    stringsAsFactors = FALSE
  )
  toolchain_digest <- digest::digest(
    toolchain, algo = "sha256", serialize = TRUE
  )
  ordinary_v1_dlm_companion_write_csv(
    shape, file.path(directory, "fit_shape_contract.csv")
  )
  ordinary_v1_dlm_companion_write_csv(
    envelope, file.path(directory, "resource_envelope.csv")
  )
  ordinary_v1_dlm_companion_write_csv(
    toolchain, file.path(directory, "toolchain_manifest.csv")
  )
  manifest <- list(
    schema_version =
      "rqrgibbs_dlm_resource_envelope_validation/2.0.0",
    source_commit = contract$commit, source_clean = TRUE,
    package_version = contract$package_version,
    expected_commit = contract$commit,
    config_digest = contract$confirmatory_config,
    incidence_digest = contract$incidence,
    maximum_seed_ledger_digest = contract$seed,
    primary_runtime_attestation_sha256 = contract$attestation,
    primary_runtime_tree_digest = contract$runtime_tree,
    primary_runtime_source_match = TRUE,
    primary_reproducibility_eligible = TRUE,
    exact_commit_attestation_pair_verified = TRUE,
    promotion_evidence_eligible = TRUE, development_execution = FALSE,
    configured_workers = 32L, configured_sentinel_workers = 8L,
    resource_gate_worker_processes = 1L,
    modeled_chains_per_worker = 4L,
    thread_environment =
      ordinary_v1_dlm_companion_thread_environment(),
    toolchain_manifest_digest = toolchain_digest,
    toolchain_manifest_complete = TRUE,
    scientific_metrics_used = FALSE,
    response_prediction_contract = FALSE,
    fit_shape_contract =
      "p_by_T_by_draw;T_by_draw;p_by_draw;draw_by_component",
    synthetic_heavy_objects_retained = FALSE,
    writer_measurement_process = "fresh_Rscript",
    reader_measurement_process = "fresh_Rscript",
    distinct_clean_processes_verified = TRUE,
    telemetry_complete = TRUE,
    maximum_writer_or_clean_reader_peak_RSS_KiB = 610000,
    declared_worker_memory_ceiling_KiB = 1048576,
    required_margin_fraction = 0.8, resource_margin_pass = TRUE,
    all_writer_shapes_valid = TRUE,
    all_clean_deserializations_valid = TRUE,
    completed_at_utc = "2026-07-26T00:01:00Z"
  )
  ordinary_v1_dlm_companion_write_json(
    manifest, file.path(directory, "validation_manifest.json")
  )
  closeout <- data.frame(
    schema_version =
      "rqrgibbs_dlm_resource_envelope_closeout/1.0.0",
    source_commit = contract$commit,
    package_version = contract$package_version,
    primary_runtime_tree_digest = contract$runtime_tree,
    primary_runtime_attestation_sha256 = contract$attestation,
    config_digest = contract$confirmatory_config,
    incidence_digest = contract$incidence,
    maximum_seed_ledger_digest = contract$seed,
    toolchain_manifest_digest = toolchain_digest,
    exact_commit_attestation_pair_verified = TRUE,
    promotion_evidence_eligible = TRUE, telemetry_complete = TRUE,
    resource_margin_pass = TRUE, all_writer_shapes_valid = TRUE,
    all_clean_deserializations_valid = TRUE,
    maximum_writer_or_clean_reader_peak_RSS_KiB = 610000,
    status = "passed", stringsAsFactors = FALSE
  )
  ordinary_v1_dlm_companion_write_csv(
    closeout, file.path(directory, "resource_closeout.csv")
  )
  ordinary_v1_dlm_companion_refresh_artifacts(
    environment, directory, "resource_envelope"
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
    transition_kernel = paste(rep("9", 64), collapse = ""),
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
    wave1_M01 = file.path(root, "wave1_M01"),
    wave1_M02 = file.path(root, "wave1_M02"),
    wave2_M01 = file.path(root, "wave2_M01"),
    wave2_M02 = file.path(root, "wave2_M02"),
    horizon = file.path(root, "horizon_M03"),
    resource = file.path(root, "resource_envelope"),
    output = file.path(root, "output")
  )
  ordinary_v1_dlm_companion_make_reference(
    environment, paths$reference, contract
  )
  ordinary_v1_dlm_companion_make_m01(
    environment, paths$wave1_M01, contract,
    "wave1_M01_static_gaussian"
  )
  ordinary_v1_dlm_companion_make_m02(
    environment, paths$wave1_M02, contract,
    "wave1_M02_static_gaussian"
  )
  ordinary_v1_dlm_companion_make_m01(
    environment, paths$wave2_M01, contract,
    "wave2_M01_local_level"
  )
  ordinary_v1_dlm_companion_make_m02(
    environment, paths$wave2_M02, contract,
    "wave2_M02_local_level"
  )
  ordinary_v1_dlm_companion_make_horizon(
    environment, paths$horizon, contract
  )
  ordinary_v1_dlm_companion_make_resource(
    environment, paths$resource, contract
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
    wave1_m01_directory = fixture$paths$wave1_M01,
    wave1_m02_directory = fixture$paths$wave1_M02,
    wave2_m01_directory = fixture$paths$wave2_M01,
    wave2_m02_directory = fixture$paths$wave2_M02,
    horizon_directory = fixture$paths$horizon,
    resource_directory = fixture$paths$resource,
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

    environment$readRDS <- function(...) {
      stop("The companion collector must not deserialize heavy RDS inputs.")
    }
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
    testthat::expect_false(manifest$heavy_input_artifacts_deserialized)
    testthat::expect_false(manifest$heavy_input_artifacts_copied)
    testthat::expect_identical(manifest$heavy_input_artifact_count, 7L)
    testthat::expect_identical(
      manifest$protected_source_inventory_count, 29L
    )
    testthat::expect_true(manifest$generalized_bayes)
    testthat::expect_false(manifest$response_likelihood)
    testthat::expect_false(manifest$response_prediction_contract)
    input_hashes <- utils::read.csv(
      file.path(result$output_directory, "input_artifact_hashes.csv"),
      stringsAsFactors = FALSE
    )
    testthat::expect_equal(nrow(input_hashes), 55L)
    testthat::expect_true(any(grepl("\\.rds$", input_hashes$relative_path)))
    testthat::expect_false(any(grepl(
      "\\.rds$", list.files(result$output_directory)
    )))
    gates <- utils::read.csv(
      file.path(result$output_directory, "semantic_gates.csv"),
      stringsAsFactors = FALSE
    )
    testthat::expect_equal(nrow(gates), 23L)
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
    config$protected_dlm_companion$schema_version <-
      environment$rqr_dlm_companion_schema()
    config$protected_dlm_companion$collector_sha256 <-
      ordinary_v1_dlm_companion_hash(file.path(
        repo_root, config$protected_dlm_companion$collector_path
      ))
    config$protected_dlm_companion$semantic_gate_count <- 23L
    config$protected_dlm_companion$input_role_count <- 7L
    config$protected_dlm_companion$input_artifact_count <- 55L
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
      fixture$paths$wave1_M01, "validation_manifest.json"
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
      environment, fixture$paths$wave1_M01, "wave1_M01_static_gaussian"
    )
    testthat::expect_error(
      ordinary_v1_dlm_companion_run_fixture(environment, fixture),
      "interweaving gate failed"
    )

    fixture <- make_case()
    m01_manifest_path <- file.path(
      fixture$paths$wave1_M01, "validation_manifest.json"
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
      environment, fixture$paths$wave1_M01, "wave1_M01_static_gaussian"
    )
    testthat::expect_error(
      ordinary_v1_dlm_companion_run_fixture(environment, fixture),
      "interweaving gate failed"
    )

    fixture <- make_case()
    m01_diagnostics_path <- file.path(
      fixture$paths$wave1_M01, "wave1_M01_diagnostics.csv"
    )
    m01_diagnostics <- utils::read.csv(
      m01_diagnostics_path, stringsAsFactors = FALSE
    )
    ordinary_v1_dlm_companion_write_csv(
      m01_diagnostics[-1L, ], m01_diagnostics_path
    )
    ordinary_v1_dlm_companion_refresh_artifacts(
      environment, fixture$paths$wave1_M01, "wave1_M01_static_gaussian"
    )
    testthat::expect_error(
      ordinary_v1_dlm_companion_run_fixture(environment, fixture),
      "diagnostic grid"
    )

    fixture <- make_case()
    m01_diagnostics_path <- file.path(
      fixture$paths$wave1_M01, "wave1_M01_diagnostics.csv"
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
      environment, fixture$paths$wave1_M01, "wave1_M01_static_gaussian"
    )
    testthat::expect_error(
      ordinary_v1_dlm_companion_run_fixture(environment, fixture),
      "frozen pass rules"
    )

    fixture <- make_case()
    m01_summary_path <- file.path(
      fixture$paths$wave1_M01, "wave1_M01_summary.csv"
    )
    m01_summary <- utils::read.csv(
      m01_summary_path, stringsAsFactors = FALSE
    )
    m01_summary$log_q_1_ess_bulk[[1L]] <- 999
    ordinary_v1_dlm_companion_write_csv(m01_summary, m01_summary_path)
    ordinary_v1_dlm_companion_refresh_artifacts(
      environment, fixture$paths$wave1_M01, "wave1_M01_static_gaussian"
    )
    testthat::expect_error(
      ordinary_v1_dlm_companion_run_fixture(environment, fixture),
      "log-q summary sidecars"
    )

    fixture <- make_case()
    wave2_m01_manifest_path <- file.path(
      fixture$paths$wave2_M01, "validation_manifest.json"
    )
    wave2_m01_manifest <- jsonlite::read_json(
      wave2_m01_manifest_path, simplifyVector = FALSE
    )
    wave2_m01_manifest$unique_transition_kernel_digests <-
      paste(rep("f", 64), collapse = "")
    ordinary_v1_dlm_companion_write_json(
      wave2_m01_manifest, wave2_m01_manifest_path
    )
    ordinary_v1_dlm_companion_refresh_artifacts(
      environment, fixture$paths$wave2_M01,
      "wave2_M01_local_level"
    )
    testthat::expect_error(
      ordinary_v1_dlm_companion_run_fixture(environment, fixture),
      "M01 source"
    )

    fixture <- make_case()
    m01_manifest_path <- file.path(
      fixture$paths$wave1_M01, "validation_manifest.json"
    )
    m01_manifest <- jsonlite::read_json(
      m01_manifest_path, simplifyVector = FALSE
    )
    changed_kernel <- m01_manifest$expected_transition_kernel_contract
    changed_kernel$scan_order[[2L]] <- "latent_v_refresh_changed"
    changed_kernel_digest <- digest::digest(
      changed_kernel, algo = "sha256", serialize = TRUE
    )
    changed_invariant <-
      environment$rqr_dlm_companion_transition_kernel_invariant(
        environment$rqr_dlm_companion_normalize_transition_kernel(
          changed_kernel, "test changed kernel"
        )
      )
    changed_invariant_digest <- digest::digest(
      changed_invariant, algo = "sha256", serialize = TRUE
    )
    m01_manifest$unique_transition_kernel_digests <-
      changed_kernel_digest
    m01_manifest$expected_transition_kernel_contract <- changed_kernel
    m01_manifest$expected_transition_kernel_contract_digest <-
      changed_kernel_digest
    m01_manifest$expected_transition_kernel_invariant <-
      changed_invariant
    m01_manifest$expected_transition_kernel_invariant_digest <-
      changed_invariant_digest
    ordinary_v1_dlm_companion_write_json(
      m01_manifest, m01_manifest_path
    )
    m01_summary_path <- file.path(
      fixture$paths$wave1_M01, "wave1_M01_summary.csv"
    )
    m01_summary <- utils::read.csv(
      m01_summary_path, stringsAsFactors = FALSE
    )
    rewrite_fit_digests <- function(value) {
      count <- length(strsplit(value, "|", fixed = TRUE)[[1L]])
      paste(rep(changed_kernel_digest, count), collapse = "|")
    }
    m01_summary$transition_kernel_digests <- vapply(
      m01_summary$transition_kernel_digests,
      rewrite_fit_digests, character(1L)
    )
    m01_summary$transition_kernel_contract_digests <- vapply(
      m01_summary$transition_kernel_contract_digests,
      rewrite_fit_digests, character(1L)
    )
    ordinary_v1_dlm_companion_write_csv(
      m01_summary, m01_summary_path
    )
    ordinary_v1_dlm_companion_refresh_artifacts(
      environment, fixture$paths$wave1_M01,
      "wave1_M01_static_gaussian"
    )
    testthat::expect_error(
      ordinary_v1_dlm_companion_run_fixture(environment, fixture),
      "M01 source"
    )

    fixture <- make_case()
    m02_diagnostics_path <- file.path(
      fixture$paths$wave1_M02, "wave1_M02_diagnostics.csv"
    )
    m02_diagnostics <- utils::read.csv(
      m02_diagnostics_path, stringsAsFactors = FALSE
    )
    m02_diagnostics[2L, ] <- m02_diagnostics[1L, ]
    ordinary_v1_dlm_companion_write_csv(
      m02_diagnostics, m02_diagnostics_path
    )
    ordinary_v1_dlm_companion_refresh_artifacts(
      environment, fixture$paths$wave1_M02, "wave1_M02_static_gaussian"
    )
    testthat::expect_error(
      ordinary_v1_dlm_companion_run_fixture(environment, fixture),
      "diagnostic grid"
    )

    fixture <- make_case()
    m02_summary_path <- file.path(
      fixture$paths$wave1_M02, "wave1_M02_summary.csv"
    )
    m02_summary <- utils::read.csv(
      m02_summary_path, stringsAsFactors = FALSE
    )
    m02_summary$minimum_bulk_ess[[1L]] <- 999
    ordinary_v1_dlm_companion_write_csv(m02_summary, m02_summary_path)
    ordinary_v1_dlm_companion_refresh_artifacts(
      environment, fixture$paths$wave1_M02, "wave1_M02_static_gaussian"
    )
    testthat::expect_error(
      ordinary_v1_dlm_companion_run_fixture(environment, fixture),
      "diagnostic extrema"
    )

    fixture <- make_case()
    m02_summary_path <- file.path(
      fixture$paths$wave1_M02, "wave1_M02_summary.csv"
    )
    m02_diagnostics_path <- file.path(
      fixture$paths$wave1_M02, "wave1_M02_diagnostics.csv"
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
      environment, fixture$paths$wave1_M02, "wave1_M02_static_gaussian"
    )
    testthat::expect_error(
      ordinary_v1_dlm_companion_run_fixture(environment, fixture),
      "task-key evidence is inconsistent"
    )

    fixture <- make_case()
    m02_manifest_path <- file.path(
      fixture$paths$wave1_M02, "validation_manifest.json"
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
      environment, fixture$paths$wave1_M02, "wave1_M02_static_gaussian"
    )
    testthat::expect_error(
      ordinary_v1_dlm_companion_run_fixture(environment, fixture),
      "runtime"
    )

    fixture <- make_case()
    wave2_m02_manifest_path <- file.path(
      fixture$paths$wave2_M02, "validation_manifest.json"
    )
    wave2_m02_manifest <- jsonlite::read_json(
      wave2_m02_manifest_path, simplifyVector = FALSE
    )
    wave2_m02_manifest$applied_schedule_evidence$standard$
      interval_chain_job_count <- 16L
    ordinary_v1_dlm_companion_write_json(
      wave2_m02_manifest, wave2_m02_manifest_path
    )
    ordinary_v1_dlm_companion_refresh_artifacts(
      environment, fixture$paths$wave2_M02,
      "wave2_M02_local_level"
    )
    testthat::expect_error(
      ordinary_v1_dlm_companion_run_fixture(environment, fixture),
      "projection, fit, or diagnostic gate failed"
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
    closeout_path <- file.path(
      fixture$paths$resource, "resource_closeout.csv"
    )
    closeout <- utils::read.csv(
      closeout_path, stringsAsFactors = FALSE
    )
    closeout$promotion_evidence_eligible <- FALSE
    ordinary_v1_dlm_companion_write_csv(closeout, closeout_path)
    ordinary_v1_dlm_companion_refresh_artifacts(
      environment, fixture$paths$resource, "resource_envelope"
    )
    testthat::expect_error(
      ordinary_v1_dlm_companion_run_fixture(environment, fixture),
      "closeout is not bound"
    )

    fixture <- make_case()
    resource_toolchain_path <- file.path(
      fixture$paths$resource, "toolchain_manifest.csv"
    )
    resource_toolchain <- utils::read.csv(
      resource_toolchain_path, stringsAsFactors = FALSE
    )
    resource_toolchain$value[
      resource_toolchain$key == "R_version"
    ] <- "different R build"
    ordinary_v1_dlm_companion_write_csv(
      resource_toolchain, resource_toolchain_path
    )
    resource_toolchain <- utils::read.csv(
      resource_toolchain_path, stringsAsFactors = FALSE
    )
    changed_toolchain_digest <- digest::digest(
      resource_toolchain, algo = "sha256", serialize = TRUE
    )
    resource_manifest_path <- file.path(
      fixture$paths$resource, "validation_manifest.json"
    )
    resource_manifest <- jsonlite::read_json(
      resource_manifest_path, simplifyVector = FALSE
    )
    resource_manifest$toolchain_manifest_digest <-
      changed_toolchain_digest
    ordinary_v1_dlm_companion_write_json(
      resource_manifest, resource_manifest_path
    )
    resource_closeout_path <- file.path(
      fixture$paths$resource, "resource_closeout.csv"
    )
    resource_closeout <- utils::read.csv(
      resource_closeout_path, stringsAsFactors = FALSE,
      numerals = "no.loss"
    )
    resource_closeout$toolchain_manifest_digest <-
      changed_toolchain_digest
    ordinary_v1_dlm_companion_write_csv(
      resource_closeout, resource_closeout_path
    )
    ordinary_v1_dlm_companion_refresh_artifacts(
      environment, fixture$paths$resource, "resource_envelope"
    )
    testthat::expect_error(
      ordinary_v1_dlm_companion_run_fixture(environment, fixture),
      "toolchain facts do not match"
    )

    fixture <- make_case()
    transition_summary_path <- file.path(
      fixture$paths$wave1_M01, "wave1_M01_summary.csv"
    )
    transition_summary <- utils::read.csv(
      transition_summary_path, stringsAsFactors = FALSE
    )
    transition_summary$transition_kernel_contract_digests[[1L]] <-
      paste(rep("f", 64), collapse = "")
    ordinary_v1_dlm_companion_write_csv(
      transition_summary, transition_summary_path
    )
    ordinary_v1_dlm_companion_refresh_artifacts(
      environment, fixture$paths$wave1_M01,
      "wave1_M01_static_gaussian"
    )
    testthat::expect_error(
      ordinary_v1_dlm_companion_run_fixture(environment, fixture),
      "per-fit transition-contract evidence"
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
      fixture$paths$wave1_M01, "validation_manifest.json"
    )
    m01_manifest <- jsonlite::read_json(
      m01_manifest_path, simplifyVector = FALSE
    )
    m01_manifest$source_commit <- paste(rep("b", 40), collapse = "")
    ordinary_v1_dlm_companion_write_json(
      m01_manifest, m01_manifest_path
    )
    ordinary_v1_dlm_companion_refresh_artifacts(
      environment, fixture$paths$wave1_M01, "wave1_M01_static_gaussian"
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

    directory <- copy_case("compact-transition-contract")
    path <- file.path(directory, "bundle_manifest.json")
    manifest <- jsonlite::read_json(path, simplifyVector = FALSE)
    manifest$wave1_transition_kernel_digest <-
      paste(rep("f", 64), collapse = "")
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

testthat::test_that(
  "Makefile wires all seven companion roles in collector order", {
    repo_root <- normalizePath(
      testthat::test_path("..", "..", ".."),
      winslash = "/", mustWork = TRUE
    )
    lines <- readLines(file.path(repo_root, "Makefile"), warn = FALSE)
    guard_start <- grep(
      "^guard-bundle-ordinary-v1-dlm-companion:", lines
    )
    bundle_start <- grep(
      "^bundle-ordinary-v1-dlm-companion:", lines
    )
    benchmark_start <- grep("^guard-benchmark-ordinary-v1:", lines)
    testthat::expect_length(guard_start, 1L)
    testthat::expect_length(bundle_start, 1L)
    testthat::expect_length(benchmark_start, 1L)
    guard_block <- paste(
      lines[guard_start:(bundle_start - 1L)],
      collapse = " "
    )
    recipe <- paste(
      lines[(bundle_start + 1L):(benchmark_start - 1L)],
      collapse = " "
    )
    roles <- c(
      "RQR_DLM_REFERENCE_DIR",
      "RQR_DLM_WAVE1_M01_DIR", "RQR_DLM_WAVE1_M02_DIR",
      "RQR_DLM_WAVE2_M01_DIR", "RQR_DLM_WAVE2_M02_DIR",
      "RQR_DLM_HORIZON_M03_DIR", "RQR_DLM_RESOURCE_ENVELOPE_DIR",
      "RQR_ORDINARY_V1_DLM_COMPANION_OUTPUT_DIR"
    )
    positions <- vapply(
      roles,
      function(role) regexpr(
        paste0("\\$\\$\\{", role, "\\}"),
        recipe, perl = TRUE
      )[[1L]],
      integer(1L)
    )
    testthat::expect_true(all(vapply(
      roles,
      grepl, logical(1L), x = guard_block, fixed = TRUE
    )))
    testthat::expect_true(all(positions > 0L))
    testthat::expect_true(all(diff(positions) > 0L))
    testthat::expect_false(grepl(
      "RQR_DLM_M01_DIR|RQR_DLM_M02_DIR",
      paste(guard_block, recipe)
    ))
  }
)

testthat::test_that(
  "M01 role labels differ while the versioned invariant is exact", {
    environment <- ordinary_v1_dlm_companion_environment()
    wave1 <- environment$rqr_dlm_companion_expected_m01_transition_kernel(
      "wave1_M01_static_gaussian"
    )
    wave2 <- environment$rqr_dlm_companion_expected_m01_transition_kernel(
      "wave2_M01_local_level"
    )
    testthat::expect_identical(
      wave1$collapsed_log_q_coordinate_order, "regression"
    )
    testthat::expect_identical(
      wave2$collapsed_log_q_coordinate_order, "level"
    )
    testthat::expect_false(identical(wave1, wave2))
    testthat::expect_false(identical(
      digest::digest(wave1, algo = "sha256", serialize = TRUE),
      digest::digest(wave2, algo = "sha256", serialize = TRUE)
    ))
    testthat::expect_identical(
      environment$rqr_dlm_companion_transition_kernel_invariant(wave1),
      environment$rqr_dlm_companion_transition_kernel_invariant(wave2)
    )
  }
)
