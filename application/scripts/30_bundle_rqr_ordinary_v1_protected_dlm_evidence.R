#!/usr/bin/env Rscript

# Bind the protected RQR-DLM reference and correction evidence required by the
# ordinary-RQR version-1 release gate.  This collector does not fit a model,
# load a chain object, or rerun any source calculation.  It validates four
# already completed, exact-commit input directories and publishes only compact
# semantic summaries and hashes below an ignored output root.

`%||%` <- function(x, y) if (is.null(x)) y else x

rqr_dlm_companion_schema <- function() {
  "rqrgibbs_ordinary_v1_protected_dlm_companion/1.0.0"
}

rqr_dlm_companion_fail <- function(...) {
  stop(paste0(...), call. = FALSE)
}

rqr_dlm_companion_is_hex <- function(x, width = 64L) {
  is.character(x) && length(x) == 1L && !is.na(x) &&
    grepl(sprintf("^[0-9a-f]{%d}$", width), tolower(x))
}

rqr_dlm_companion_is_integerish <- function(
    x, minimum = 0, maximum = .Machine$integer.max) {
  is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x) &&
    x == floor(x) && x >= minimum && x <= maximum
}

rqr_dlm_companion_scalar_true <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x) && isTRUE(x)
}

rqr_dlm_companion_scalar_false <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x) && identical(x, FALSE)
}

rqr_dlm_companion_logical_column <- function(x, length_expected = NULL) {
  is.logical(x) && !anyNA(x) &&
    (is.null(length_expected) || length(x) == length_expected)
}

rqr_dlm_companion_integer_column <- function(
    x, length_expected = NULL, minimum = 0,
    maximum = .Machine$integer.max) {
  is.numeric(x) && !anyNA(x) && all(is.finite(x)) &&
    all(x == floor(x)) && all(x >= minimum) && all(x <= maximum) &&
    (is.null(length_expected) || length(x) == length_expected)
}

rqr_dlm_companion_finite_column <- function(
    x, length_expected = NULL, minimum = -Inf, allow_na = FALSE) {
  is.numeric(x) &&
    (isTRUE(allow_na) || !anyNA(x)) &&
    all(is.finite(x[!is.na(x)])) &&
    all(x[!is.na(x)] >= minimum) &&
    (is.null(length_expected) || length(x) == length_expected)
}

rqr_dlm_companion_is_symlink <- function(path) {
  value <- Sys.readlink(path)
  !is.na(value) & nzchar(value)
}

rqr_dlm_companion_sha256 <- function(path) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    rqr_dlm_companion_fail("The digest package is required.")
  }
  if (!file.exists(path) || !all(utils::file_test("-f", path)) ||
      any(rqr_dlm_companion_is_symlink(path))) {
    rqr_dlm_companion_fail(
      "Cannot hash an absent, nonregular, or symbolic-link artifact: ", path
    )
  }
  tolower(digest::digest(
    file = path, algo = "sha256", serialize = FALSE
  ))
}

rqr_dlm_companion_read_json <- function(path, fields, label) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    rqr_dlm_companion_fail("The jsonlite package is required.")
  }
  value <- tryCatch(
    jsonlite::read_json(path, simplifyVector = FALSE),
    error = function(error) {
      rqr_dlm_companion_fail(
        "Could not parse ", label, ": ", conditionMessage(error)
      )
    }
  )
  if (!is.list(value) || !identical(names(value), fields)) {
    rqr_dlm_companion_fail(label, " does not have its exact schema.")
  }
  value
}

rqr_dlm_companion_read_csv <- function(path, fields, label) {
  value <- tryCatch(
    utils::read.csv(
      path, stringsAsFactors = FALSE, check.names = FALSE,
      na.strings = "NA"
    ),
    error = function(error) {
      rqr_dlm_companion_fail(
        "Could not parse ", label, ": ", conditionMessage(error)
      )
    }
  )
  if (!is.data.frame(value) || !identical(names(value), fields)) {
    rqr_dlm_companion_fail(label, " does not have its exact schema.")
  }
  value
}

rqr_dlm_companion_input_files <- function(role) {
  switch(
    role,
    dlm_reference = c(
      "artifact_hashes.csv", "canonical_missing_checks.csv",
      "component_scale_conditionals.csv", "continuation_cells.csv",
      "continuation_history_mutations.csv",
      "continuation_reference_digests.rds",
      "dense_ffbs_reference.rds", "failure_log.csv",
      "fixture_construction.csv", "monitor_fault_test.csv",
      "process_group_monitor.csv", "public_future_root_checks.csv",
      "reference_bundle.json", "reference_gates.csv",
      "resource_summary.csv", "run_manifest.json",
      "runner.stderr.log", "runner.stdout.log",
      "runtime_toolchain.json", "session_info.txt",
      "varying_component_scale_future_checks.csv",
      "wrapper_closeout.csv"
    ),
    M01 = c(
      "artifact_hashes.csv", "validation_manifest.json",
      "wave1_M01_chain_evidence.rds", "wave1_M01_diagnostics.csv",
      "wave1_M01_summary.csv"
    ),
    M02 = c(
      "artifact_hashes.csv", "validation_manifest.json",
      "wave1_M02_chain_evidence.rds", "wave1_M02_diagnostics.csv",
      "wave1_M02_summary.csv"
    ),
    horizon_M03 = c(
      "artifact_hashes.csv", "dynamic_endpoint_check.csv",
      "fixed_design_chain_evidence.rds",
      "fixed_design_diagnostics.csv", "fixed_design_summary.csv",
      "horizon_checks.csv", "validation_manifest.json"
    ),
    rqr_dlm_companion_fail("Unknown companion input role: ", role)
  )
}

rqr_dlm_companion_validate_directory <- function(path, role) {
  if (!is.character(path) || length(path) != 1L || is.na(path) ||
      !nzchar(path) || !dir.exists(path) ||
      any(rqr_dlm_companion_is_symlink(path))) {
    rqr_dlm_companion_fail(
      role, " input must be one existing nonsymlink directory."
    )
  }
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  entries <- list.files(
    path, recursive = TRUE, all.files = TRUE, no.. = TRUE,
    include.dirs = TRUE, full.names = TRUE
  )
  relative <- substring(entries, nchar(path) + 2L)
  info <- file.info(entries)
  if (anyNA(info$isdir) || any(info$isdir) ||
      !all(utils::file_test("-f", entries)) ||
      any(rqr_dlm_companion_is_symlink(entries)) ||
      !identical(sort(relative), sort(rqr_dlm_companion_input_files(role)))) {
    rqr_dlm_companion_fail(
      role,
      " input has a missing, extra, nested, nonregular, or symlinked artifact."
    )
  }
  path
}

rqr_dlm_companion_verify_artifact_manifest <- function(
    directory, role, column_order) {
  manifest_path <- file.path(directory, "artifact_hashes.csv")
  manifest <- rqr_dlm_companion_read_csv(
    manifest_path,
    column_order,
    paste0(role, " artifact_hashes.csv")
  )
  expected <- sort(setdiff(
    rqr_dlm_companion_input_files(role), "artifact_hashes.csv"
  ))
  observed <- as.character(manifest[["path"]])
  if (!identical(observed, expected) || anyNA(observed) ||
      anyDuplicated(observed) ||
      any(grepl("(^|/)\\.\\.(/|$)|^/|\\\\", observed))) {
    rqr_dlm_companion_fail(
      role, " artifact manifest is not its exact closed file set."
    )
  }
  bytes <- suppressWarnings(as.numeric(manifest[["bytes"]]))
  hashes <- tolower(as.character(manifest[["sha256"]]))
  if (anyNA(bytes) || any(!is.finite(bytes)) || any(bytes < 0) ||
      any(bytes != floor(bytes)) ||
      anyNA(hashes) || any(!grepl("^[0-9a-f]{64}$", hashes))) {
    rqr_dlm_companion_fail(
      role, " artifact manifest contains an invalid size or SHA-256."
    )
  }
  for (index in seq_along(observed)) {
    artifact <- file.path(directory, observed[[index]])
    if (!identical(as.numeric(file.info(artifact)$size), bytes[[index]]) ||
        !identical(rqr_dlm_companion_sha256(artifact), hashes[[index]])) {
      rqr_dlm_companion_fail(
        role, " artifact manifest does not match current bytes: ",
        observed[[index]]
      )
    }
  }
  data.frame(
    relative_path = observed,
    byte_count = bytes,
    sha256 = hashes,
    stringsAsFactors = FALSE
  )
}

rqr_dlm_companion_reference_gate_names <- function() {
  fixtures <- c(
    "fixed_W_local_level", "frozen_trend_seasonal_discount",
    "shared_component_scale_trend_regression"
  )
  rates <- c("fixed_rate", "learned_pseudoresidual_normalized")
  continuation <- unlist(lapply(fixtures, function(fixture) {
    unlist(lapply(rates, function(rate) {
      c(
        paste0(
          "all_saved_fields_6_vs_2_plus_2_plus_2__", fixture, "__", rate
        ),
        paste0(
          "checkpoint_6_vs_2_plus_2_plus_2__", fixture, "__", rate
        ),
        paste0(
          "history_shape_2_plus_2_plus_2__", fixture, "__", rate
        )
      )
    }), use.names = FALSE)
  }), use.names = FALSE)
  c(
    "dense_conditional_mean", "dense_conditional_covariance",
    "R_cpp_smoother_parity", "cpp_sampled_mean",
    "cpp_sampled_full_cross_time_covariance",
    "cpp_sampled_adjacent_time_covariance",
    "missing_measurement_omission",
    "canonical_missing_placeholder_invariance__fixed_W_local_level",
    paste0(
      "canonical_missing_placeholder_invariance__",
      "shared_component_scale_trend_regression"
    ),
    "public_future_mean__fixed_W_local_level",
    "public_future_variance__fixed_W_local_level",
    "public_future_mean__frozen_trend_seasonal_discount",
    "public_future_variance__frozen_trend_seasonal_discount",
    "public_future_mean__shared_component_scale_trend_regression",
    "public_future_variance__shared_component_scale_trend_regression",
    "varying_component_scale_future_mean",
    "varying_component_scale_future_variance",
    "varying_component_scale_future_orientation",
    "component_scale_inverse_gamma_shape",
    "component_scale_inverse_gamma_rate",
    "canonical_component_scale_shape",
    "canonical_component_scale_rate",
    "canonical_component_scale_orientation",
    continuation,
    "rehashed_early_history_raw_and_semantic_mutations",
    "active_process_tree_monitor"
  )
}

rqr_dlm_companion_estimand_names <- function(kind) {
  root_functions <- c("lower", "midpoint", "upper", "width")
  future <- unlist(lapply(
    c(1L, 5L, 10L, 20L),
    function(horizon) {
      paste0("future_h", sprintf("%02d", horizon), "_", root_functions)
    }
  ), use.names = FALSE)
  training <- unlist(lapply(
    c(1L, 50L, 100L, 150L, 200L),
    function(index) {
      paste0("training_t", sprintf("%04d", index), "_", root_functions)
    }
  ), use.names = FALSE)
  common <- c(
    future, paste0("mean_", root_functions), "observed_loss", training
  )
  switch(
    kind,
    M01 = sort(c(common, paste0("terminal_", root_functions), "log_q_1")),
    M02 = sort(c(common, paste0("terminal_", root_functions))),
    M03 = sort(common),
    rqr_dlm_companion_fail("Unknown diagnostic estimand kind: ", kind)
  )
}

rqr_dlm_companion_validate_diagnostic_grid <- function(
    diagnostics, summary, kind, has_sentinel) {
  estimands <- rqr_dlm_companion_estimand_names(kind)
  summary_key <- paste(summary$DGP, summary$replication, sep = "::")
  diagnostic_key <- paste(
    diagnostics$DGP, diagnostics$replication, sep = "::"
  )
  matched <- match(diagnostic_key, summary_key)
  summary_chains <- if ("chains" %in% names(summary)) {
    summary$chains
  } else {
    rep(1, nrow(summary))
  }
  split_estimands <- split(
    as.character(diagnostics$estimand),
    factor(diagnostic_key, levels = summary_key)
  )
  if (anyDuplicated(summary_key) ||
      anyDuplicated(paste(diagnostic_key, diagnostics$estimand, sep = "::")) ||
      nrow(diagnostics) != nrow(summary) * length(estimands) ||
      anyNA(matched) ||
      length(split_estimands) != nrow(summary) ||
      !all(vapply(
        split_estimands,
        function(value) identical(sort(value), estimands),
        logical(1L)
      )) ||
      !rqr_dlm_companion_integer_column(
        diagnostics$replication, nrow(diagnostics), minimum = 1
      ) ||
      !rqr_dlm_companion_integer_column(
        diagnostics$chains, nrow(diagnostics), minimum = 1
      ) ||
      any(diagnostics$chains != summary_chains[matched]) ||
      !rqr_dlm_companion_logical_column(
        diagnostics$pass, nrow(diagnostics)
      ) ||
      !all(diagnostics$pass) ||
      !(
        rqr_dlm_companion_finite_column(
          diagnostics$rhat, nrow(diagnostics),
          minimum = 0, allow_na = TRUE
        ) ||
        (
          is.logical(diagnostics$rhat) &&
          length(diagnostics$rhat) == nrow(diagnostics) &&
          all(is.na(diagnostics$rhat))
        )
      ) ||
      !rqr_dlm_companion_finite_column(
        diagnostics$ess_bulk, nrow(diagnostics), minimum = 0
      ) ||
      !rqr_dlm_companion_finite_column(
        diagnostics$ess_tail, nrow(diagnostics), minimum = 0
      ) ||
      !rqr_dlm_companion_finite_column(
        diagnostics$mcse_mean, nrow(diagnostics), minimum = 0
      ) ||
      !rqr_dlm_companion_finite_column(
        diagnostics$mcse_over_sd, nrow(diagnostics), minimum = 0
      )) {
    rqr_dlm_companion_fail(
      kind, " diagnostic grid is incomplete, duplicated, or invalid."
    )
  }
  if (isTRUE(has_sentinel) &&
      (!rqr_dlm_companion_logical_column(
         diagnostics$sentinel, nrow(diagnostics)
       ) ||
       any(diagnostics$sentinel != summary$sentinel[matched]))) {
    rqr_dlm_companion_fail(kind, " diagnostic sentinel map is invalid.")
  }
  single_chain <- diagnostics$chains == 1
  four_chain <- diagnostics$chains == 4
  if (any(!(single_chain | four_chain)) ||
      any(!is.na(diagnostics$rhat[single_chain])) ||
      any(is.na(diagnostics$rhat[four_chain])) ||
      any(diagnostics$rhat[four_chain] > 1.01) ||
      any(diagnostics$ess_bulk[single_chain] < 200) ||
      any(diagnostics$ess_tail[single_chain] < 100) ||
      any(diagnostics$ess_bulk[four_chain] < 400) ||
      any(diagnostics$ess_tail[four_chain] < 400) ||
      any(diagnostics$mcse_over_sd[single_chain] > 0.08)) {
    rqr_dlm_companion_fail(
      kind, " diagnostics do not satisfy the frozen pass rules."
    )
  }
  invisible(TRUE)
}

rqr_dlm_companion_reference_manifest_fields <- function() {
  c(
    "schema_version", "mode", "config_id", "config_digest",
    "primary_commit", "primary_application_tree", "primary_runtime_path",
    "primary_runtime_tree_digest", "primary_runtime_attestation",
    "primary_runtime_attestation_schema",
    "primary_runtime_attestation_sha256", "runtime_toolchain_digest",
    "runtime_gates", "requested_fit_count", "mcmc_schedule",
    "estimand_schema_version", "future_primary_estimands",
    "stochastic_future_draws_role", "full_chain_files_ignored",
    "generalized_bayes", "response_likelihood",
    "response_prediction_contract", "production_simulation",
    "process_tree_monitor_active", "process_tree_monitor_kind",
    "kernel_hard_memory_ceiling", "recorded_at", "status",
    "fixture_construction_passed", "reference_gates_executed",
    "reference_gate_count", "reference_gate_pass_count",
    "reference_gates_sha256", "reference_bundle_sha256",
    "bounded_dynamic_execution_authorized"
  )
}

rqr_dlm_companion_reference_runtime_fields <- function() {
  c(
    "schema_version", "R_version", "platform", "compiler", "BLAS",
    "LAPACK", "dependency_versions", "primary_runtime_tree_digest",
    "primary_runtime_attestation_sha256", "digest"
  )
}

rqr_dlm_companion_reference_bundle_fields <- function() {
  c(
    "schema_version", "primary_commit", "config_digest",
    "runtime_tree_digest", "runtime_attestation_sha256",
    "runtime_toolchain_digest", "estimand_schema_version", "files"
  )
}

rqr_dlm_companion_reference_bundle_file_names <- function() {
  c(
    "fixture_construction.csv", "runtime_toolchain.json",
    "session_info.txt", "failure_log.csv", "reference_gates.csv",
    "dense_ffbs_reference.rds", "canonical_missing_checks.csv",
    "public_future_root_checks.csv",
    "varying_component_scale_future_checks.csv",
    "component_scale_conditionals.csv", "continuation_cells.csv",
    "continuation_history_mutations.csv",
    "continuation_reference_digests.rds"
  )
}

rqr_dlm_companion_assert_thread_environment <- function(value, label) {
  expected <- c(
    "OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS",
    "VECLIB_MAXIMUM_THREADS", "NUMEXPR_NUM_THREADS"
  )
  if (!is.list(value) || !identical(names(value), expected) ||
      any(vapply(value, function(x) {
        !is.character(x) || length(x) != 1L || !identical(x, "1")
      }, logical(1L)))) {
    rqr_dlm_companion_fail(label, " thread environment is not exactly one.")
  }
}

rqr_dlm_companion_validate_reference <- function(
    directory, expected_commit) {
  directory <- rqr_dlm_companion_validate_directory(
    directory, "dlm_reference"
  )
  artifacts <- rqr_dlm_companion_verify_artifact_manifest(
    directory, "dlm_reference", c("sha256", "bytes", "path")
  )
  run <- rqr_dlm_companion_read_json(
    file.path(directory, "run_manifest.json"),
    rqr_dlm_companion_reference_manifest_fields(),
    "DLM reference run_manifest.json"
  )
  runtime <- rqr_dlm_companion_read_json(
    file.path(directory, "runtime_toolchain.json"),
    rqr_dlm_companion_reference_runtime_fields(),
    "DLM reference runtime_toolchain.json"
  )
  bundle <- rqr_dlm_companion_read_json(
    file.path(directory, "reference_bundle.json"),
    rqr_dlm_companion_reference_bundle_fields(),
    "DLM reference reference_bundle.json"
  )
  runtime_gate_names <- c(
    "runtime_attestation_match", "source_archive_tree_match",
    "source_package_verified", "source_package_archive_match",
    "build_evidence_verified", "install_evidence_verified",
    "runtime_lineage_marker_match", "runtime_install_receipt_match",
    "runtime_source_match", "reproducibility_eligible"
  )
  if (!identical(run$schema_version, "rqrgibbs_dlm_bounded_run/3.0.0") ||
      !identical(run$mode, "reference-only") ||
      !identical(tolower(run$primary_commit), expected_commit) ||
      !rqr_dlm_companion_is_hex(run$config_digest) ||
      !rqr_dlm_companion_is_hex(run$primary_application_tree, 40L) ||
      !is.character(run$primary_runtime_path) ||
      length(run$primary_runtime_path) != 1L ||
      is.na(run$primary_runtime_path) || !nzchar(run$primary_runtime_path) ||
      !rqr_dlm_companion_is_hex(run$primary_runtime_tree_digest) ||
      !identical(
        run$primary_runtime_attestation_schema,
        "rqrgibbs_runtime_attestation/5.0.0"
      ) ||
      !rqr_dlm_companion_is_hex(
        run$primary_runtime_attestation_sha256
      ) ||
      !rqr_dlm_companion_is_hex(run$runtime_toolchain_digest) ||
      !is.list(run$runtime_gates) ||
      !identical(names(run$runtime_gates), runtime_gate_names) ||
      !all(vapply(
        run$runtime_gates, rqr_dlm_companion_scalar_true, logical(1L)
      )) ||
      !rqr_dlm_companion_is_integerish(
        run$requested_fit_count, 24, 24
      ) ||
      !is.list(run$mcmc_schedule) ||
      !identical(
        names(run$mcmc_schedule),
        c("chains", "burn_in", "retained_per_chain", "thin")
      ) ||
      !all(vapply(
        run$mcmc_schedule, rqr_dlm_companion_is_integerish,
        logical(1L), minimum = 1
      )) ||
      !identical(
        unname(as.numeric(unlist(run$mcmc_schedule))),
        c(4, 2000, 6000, 1)
      ) ||
      !identical(
        run$estimand_schema_version,
        "rqrgibbs_dlm_bounded_estimands/1.0.0"
      ) ||
      !identical(
        run$future_primary_estimands,
        "deterministic_conditional_mean_interval_roots"
      ) ||
      !identical(run$stochastic_future_draws_role, "sidecar_only") ||
      !rqr_dlm_companion_scalar_true(run$full_chain_files_ignored) ||
      !rqr_dlm_companion_scalar_true(run$generalized_bayes) ||
      !rqr_dlm_companion_scalar_false(run$response_likelihood) ||
      !rqr_dlm_companion_scalar_false(
        run$response_prediction_contract
      ) ||
      !rqr_dlm_companion_scalar_false(run$production_simulation) ||
      !rqr_dlm_companion_scalar_true(run$process_tree_monitor_active) ||
      !identical(run$process_tree_monitor_kind, "pgid_sampled_fallback") ||
      !rqr_dlm_companion_scalar_false(run$kernel_hard_memory_ceiling) ||
      !identical(run$status, "passed") ||
      !rqr_dlm_companion_scalar_true(run$fixture_construction_passed) ||
      !rqr_dlm_companion_scalar_true(run$reference_gates_executed) ||
      !rqr_dlm_companion_is_integerish(
        run$reference_gate_count, 43, 43
      ) ||
      !rqr_dlm_companion_is_integerish(
        run$reference_gate_pass_count, 43, 43
      ) ||
      !rqr_dlm_companion_scalar_false(
        run$bounded_dynamic_execution_authorized
      )) {
    rqr_dlm_companion_fail(
      "DLM reference run manifest did not pass its exact source/runtime gate."
    )
  }
  if (!identical(
        runtime$schema_version, "rqrgibbs_runtime_toolchain/1.0.0"
      ) ||
      !identical(
        runtime$primary_runtime_tree_digest,
        run$primary_runtime_tree_digest
      ) ||
      !identical(
        runtime$primary_runtime_attestation_sha256,
        run$primary_runtime_attestation_sha256
      ) ||
      !identical(runtime$digest, run$runtime_toolchain_digest) ||
      !is.list(runtime$dependency_versions) ||
      !is.character(runtime$dependency_versions$rqrgibbs %||% NULL) ||
      length(runtime$dependency_versions$rqrgibbs) != 1L) {
    rqr_dlm_companion_fail(
      "DLM reference runtime toolchain is not internally bound."
    )
  }
  if (!identical(bundle$schema_version, "rqrgibbs_reference_bundle/2.0.0") ||
      !identical(tolower(bundle$primary_commit), expected_commit) ||
      !identical(bundle$config_digest, run$config_digest) ||
      !identical(
        bundle$runtime_tree_digest, run$primary_runtime_tree_digest
      ) ||
      !identical(
        bundle$runtime_attestation_sha256,
        run$primary_runtime_attestation_sha256
      ) ||
      !identical(
        bundle$runtime_toolchain_digest, run$runtime_toolchain_digest
      ) ||
      !identical(
        bundle$estimand_schema_version, run$estimand_schema_version
      ) ||
      !is.list(bundle$files) ||
      !identical(
        names(bundle$files),
        rqr_dlm_companion_reference_bundle_file_names()
      )) {
    rqr_dlm_companion_fail(
      "DLM reference bundle is not internally bound."
    )
  }
  for (name in names(bundle$files)) {
    if (!rqr_dlm_companion_is_hex(bundle$files[[name]]) ||
        !identical(
          tolower(bundle$files[[name]]),
          rqr_dlm_companion_sha256(file.path(directory, name))
        )) {
      rqr_dlm_companion_fail(
        "DLM reference inner bundle hash mismatch: ", name
      )
    }
  }
  if (!identical(
        run$reference_gates_sha256,
        rqr_dlm_companion_sha256(
          file.path(directory, "reference_gates.csv")
        )
      ) ||
      !identical(
        run$reference_bundle_sha256,
        rqr_dlm_companion_sha256(
          file.path(directory, "reference_bundle.json")
        )
      )) {
    rqr_dlm_companion_fail(
      "DLM reference run manifest hashes do not match current bytes."
    )
  }

  fixtures <- rqr_dlm_companion_read_csv(
    file.path(directory, "fixture_construction.csv"),
    c(
      "fixture_id", "state_dimension", "component_dims",
      "component_names", "observed_count", "missing_count",
      "training_horizon", "future_horizon", "evolution_mode",
      "exact_joint_target", "extension_reproduces_training",
      "model_digest", "evolution_digest", "missing_response_digest",
      "future_digest"
    ),
    "DLM reference fixture_construction.csv"
  )
  expected_fixtures <- data.frame(
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
    stringsAsFactors = FALSE
  )
  digest_columns <- c(
    "model_digest", "evolution_digest", "missing_response_digest",
    "future_digest"
  )
  if (nrow(fixtures) != 3L ||
      !identical(
        as.character(fixtures$fixture_id),
        expected_fixtures$fixture_id
      ) ||
      !identical(
        as.numeric(fixtures$state_dimension),
        expected_fixtures$state_dimension
      ) ||
      !identical(
        as.character(fixtures$component_dims),
        expected_fixtures$component_dims
      ) ||
      !identical(
        as.character(fixtures$component_names),
        expected_fixtures$component_names
      ) ||
      !identical(
        as.numeric(fixtures$observed_count),
        expected_fixtures$observed_count
      ) ||
      !identical(
        as.numeric(fixtures$missing_count),
        expected_fixtures$missing_count
      ) ||
      !identical(
        as.numeric(fixtures$training_horizon),
        expected_fixtures$training_horizon
      ) ||
      !identical(
        as.numeric(fixtures$future_horizon),
        expected_fixtures$future_horizon
      ) ||
      !identical(
        as.character(fixtures$evolution_mode),
        expected_fixtures$evolution_mode
      ) ||
      !identical(fixtures$exact_joint_target, rep(TRUE, 3L)) ||
      !identical(
        fixtures$extension_reproduces_training, c(NA, TRUE, NA)
      ) ||
      any(!vapply(
        unlist(fixtures[, digest_columns], use.names = FALSE),
        rqr_dlm_companion_is_hex, logical(1L)
      ))) {
    rqr_dlm_companion_fail(
      "DLM reference fixture construction is incomplete or nonexact."
    )
  }

  gates <- rqr_dlm_companion_read_csv(
    file.path(directory, "reference_gates.csv"),
    c("gate", "pass", "value", "tolerance", "detail"),
    "DLM reference reference_gates.csv"
  )
  if (!identical(
        as.character(gates$gate),
        rqr_dlm_companion_reference_gate_names()
      ) ||
      nrow(gates) != 43L ||
      !rqr_dlm_companion_logical_column(gates$pass, 43L) ||
      !all(gates$pass)) {
    rqr_dlm_companion_fail(
      "DLM reference gates are incomplete, reordered, or failed."
    )
  }
  continuation <- rqr_dlm_companion_read_csv(
    file.path(directory, "continuation_cells.csv"),
    c(
      "fixture_id", "learning_rate_mode", "seed",
      "all_saved_stochastic_fields_bitwise", "time0_draws_complete",
      "estimand_schema_complete", "final_checkpoint_bitwise",
      "three_segment_history", "full_checkpoint_digest",
      "segmented_checkpoint_digest", "continuation_history_digest"
    ),
    "DLM reference continuation_cells.csv"
  )
  expected_cells <- do.call(rbind, lapply(
    c(
      "fixed_W_local_level", "frozen_trend_seasonal_discount",
      "shared_component_scale_trend_regression"
    ),
    function(fixture) {
      data.frame(
        fixture_id = fixture,
        learning_rate_mode = c(
          "fixed_rate", "learned_pseudoresidual_normalized"
        ),
        stringsAsFactors = FALSE
      )
    }
  ))
  if (nrow(continuation) != 6L ||
      !identical(
        continuation[, names(expected_cells), drop = FALSE],
        expected_cells
      ) ||
      !rqr_dlm_companion_integer_column(
        continuation$seed, 6L, minimum = 0
      ) ||
      anyDuplicated(continuation$seed) ||
      !rqr_dlm_companion_logical_column(
        continuation$all_saved_stochastic_fields_bitwise, 6L
      ) ||
      !all(continuation$all_saved_stochastic_fields_bitwise) ||
      !rqr_dlm_companion_logical_column(
        continuation$time0_draws_complete, 6L
      ) ||
      !all(continuation$time0_draws_complete) ||
      !rqr_dlm_companion_logical_column(
        continuation$estimand_schema_complete, 6L
      ) ||
      !all(continuation$estimand_schema_complete) ||
      !rqr_dlm_companion_logical_column(
        continuation$final_checkpoint_bitwise, 6L
      ) ||
      !all(continuation$final_checkpoint_bitwise) ||
      !rqr_dlm_companion_logical_column(
        continuation$three_segment_history, 6L
      ) ||
      !all(continuation$three_segment_history) ||
      any(
        continuation$full_checkpoint_digest !=
          continuation$segmented_checkpoint_digest
      ) ||
      any(!grepl(
        "^[0-9a-f]{64}$",
        c(
          continuation$full_checkpoint_digest,
          continuation$segmented_checkpoint_digest,
          continuation$continuation_history_digest
        )
      ))) {
    rqr_dlm_companion_fail(
      "DLM reference does not contain six complete 6=2+2+2 cells."
    )
  }
  mutations <- rqr_dlm_companion_read_csv(
    file.path(directory, "continuation_history_mutations.csv"),
    c("generation", "field", "value", "rejected"),
    "DLM reference continuation_history_mutations.csv"
  )
  mutation_values <- c("0.5", "-0.5", "Inf", "2147483648")
  expected_mutations <- rbind(
    do.call(rbind, lapply(0:1, function(generation) {
      do.call(rbind, lapply(
        c(
          "generation", "segment_numerical_repair_count",
          "cumulative_numerical_repair_count"
        ),
        function(field) {
          data.frame(
            generation = generation, field = field,
            value = mutation_values, rejected = TRUE,
            stringsAsFactors = FALSE
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
  )
  rownames(expected_mutations) <- NULL
  mutation_core <- mutations
  mutation_core$generation <- as.integer(mutation_core$generation)
  mutation_core$field <- as.character(mutation_core$field)
  mutation_core$value <- as.character(mutation_core$value)
  if (nrow(mutations) != 27L ||
      !rqr_dlm_companion_logical_column(mutations$rejected, 27L) ||
      !identical(mutation_core, expected_mutations) ||
      !all(mutations$rejected)) {
    rqr_dlm_companion_fail(
      "DLM reference history-mutation gate is incomplete or failed."
    )
  }
  missing <- rqr_dlm_companion_read_csv(
    file.path(directory, "canonical_missing_checks.csv"),
    c(
      "fixture_id", "expected_missing_indices",
      "detected_missing_indices", "maximum_placeholder_invariance_error",
      "pass"
    ),
    "DLM reference canonical_missing_checks.csv"
  )
  future <- rqr_dlm_companion_read_csv(
    file.path(directory, "public_future_root_checks.csv"),
    c(
      "fixture_id", "mean_standardized_error",
      "variance_standardized_error", "repair_count",
      "interpretation_pass", "pass"
    ),
    "DLM reference public_future_root_checks.csv"
  )
  varying_future <- rqr_dlm_companion_read_csv(
    file.path(directory, "varying_component_scale_future_checks.csv"),
    c(
      "scale_profile", "scale_values", "mean_standardized_error",
      "variance_standardized_error"
    ),
    "DLM reference varying_component_scale_future_checks.csv"
  )
  conditionals <- rqr_dlm_companion_read_csv(
    file.path(directory, "component_scale_conditionals.csv"),
    c(
      "draw", "component", "saved_shape", "recomputed_shape",
      "saved_rate", "recomputed_rate"
    ),
    "DLM reference component_scale_conditionals.csv"
  )
  expected_missing <- data.frame(
    fixture_id = c(
      "fixed_W_local_level",
      "shared_component_scale_trend_regression"
    ),
    expected_missing_indices = c("6,17", "11"),
    detected_missing_indices = c("6,17", "11"),
    stringsAsFactors = FALSE
  )
  expected_future_fixtures <- c(
    "fixed_W_local_level", "frozen_trend_seasonal_discount",
    "shared_component_scale_trend_regression"
  )
  scale_values <- tryCatch(
    t(vapply(
      strsplit(as.character(varying_future$scale_values), ",", fixed = TRUE),
      function(value) {
        parsed <- suppressWarnings(as.numeric(value))
        if (length(parsed) != 2L) stop("invalid scale profile")
        parsed
      },
      numeric(2L)
    )),
    error = function(error) matrix(NA_real_, nrow = 2L, ncol = 2L)
  )
  expected_conditional_grid <- data.frame(
    draw = rep(1:3, each = 2),
    component = rep(c("trend", "regression"), 3),
    stringsAsFactors = FALSE
  )
  if (nrow(missing) != 2L ||
      !identical(
        missing[, names(expected_missing), drop = FALSE],
        expected_missing
      ) ||
      !rqr_dlm_companion_logical_column(missing$pass, 2L) ||
      !all(missing$pass) ||
      any(missing$expected_missing_indices !=
            missing$detected_missing_indices) ||
      any(missing$maximum_placeholder_invariance_error != 0) ||
      nrow(future) != 3L ||
      !identical(
        as.character(future$fixture_id), expected_future_fixtures
      ) ||
      !rqr_dlm_companion_logical_column(future$pass, 3L) ||
      !all(future$pass) ||
      !rqr_dlm_companion_logical_column(
        future$interpretation_pass, 3L
      ) ||
      !all(future$interpretation_pass) ||
      !rqr_dlm_companion_integer_column(
        future$repair_count, 3L, minimum = 0
      ) ||
      any(future$repair_count != 0) ||
      !rqr_dlm_companion_finite_column(
        future$mean_standardized_error, 3L, minimum = 0
      ) ||
      !rqr_dlm_companion_finite_column(
        future$variance_standardized_error, 3L, minimum = 0
      ) ||
      any(future$mean_standardized_error > 5) ||
      any(future$variance_standardized_error > 6) ||
      nrow(varying_future) != 2L ||
      !identical(as.numeric(varying_future$scale_profile), c(1, 2)) ||
      !identical(
        unname(scale_values),
        matrix(c(0.025, 0.025, 0.1, 0.1), nrow = 2L, byrow = TRUE)
      ) ||
      !rqr_dlm_companion_finite_column(
        varying_future$mean_standardized_error, 2L, minimum = 0
      ) ||
      !rqr_dlm_companion_finite_column(
        varying_future$variance_standardized_error, 2L, minimum = 0
      ) ||
      any(varying_future$mean_standardized_error > 5) ||
      any(varying_future$variance_standardized_error > 6) ||
      nrow(conditionals) != 6L ||
      !identical(
        conditionals[, names(expected_conditional_grid), drop = FALSE],
        expected_conditional_grid
      ) ||
      !rqr_dlm_companion_finite_column(
        conditionals$saved_shape, 6L, minimum = .Machine$double.xmin
      ) ||
      !rqr_dlm_companion_finite_column(
        conditionals$recomputed_shape, 6L,
        minimum = .Machine$double.xmin
      ) ||
      !rqr_dlm_companion_finite_column(
        conditionals$saved_rate, 6L, minimum = .Machine$double.xmin
      ) ||
      !rqr_dlm_companion_finite_column(
        conditionals$recomputed_rate, 6L,
        minimum = .Machine$double.xmin
      ) ||
      any(conditionals$saved_shape != conditionals$recomputed_shape) ||
      any(conditionals$saved_rate != conditionals$recomputed_rate)) {
    rqr_dlm_companion_fail(
      "DLM missing, future, or component-scale semantic evidence failed."
    )
  }
  failure <- rqr_dlm_companion_read_csv(
    file.path(directory, "failure_log.csv"),
    c(
      "recorded_at", "mode", "stage", "fixture_id",
      "learning_rate_mode", "chain", "message"
    ),
    "DLM reference failure_log.csv"
  )
  resource <- rqr_dlm_companion_read_csv(
    file.path(directory, "resource_summary.csv"),
    c("metric", "value", "limit", "pass"),
    "DLM reference resource_summary.csv"
  )
  monitor <- rqr_dlm_companion_read_csv(
    file.path(directory, "monitor_fault_test.csv"),
    c("metric", "value", "expected", "pass"),
    "DLM reference monitor_fault_test.csv"
  )
  process_monitor <- rqr_dlm_companion_read_csv(
    file.path(directory, "process_group_monitor.csv"),
    c("elapsed_seconds", "processes", "threads", "rss_kib"),
    "DLM reference process_group_monitor.csv"
  )
  closeout <- rqr_dlm_companion_read_csv(
    file.path(directory, "wrapper_closeout.csv"),
    c("field", "value"), "DLM reference wrapper_closeout.csv"
  )
  closeout_values <- setNames(closeout$value, closeout$field)
  expected_closeout <- c(
    "schema_version", "mode", "expected_primary_commit",
    "process_group_id", "runner_exit_status", "resource_pass",
    "monitor_kind", "kernel_hard_memory_ceiling", "signal_received",
    "final_pgid_empty", "finalizer_error", "completed_at"
  )
  expected_resource_metrics <- c(
    "sampled_process_group_peak_processes",
    "sampled_process_group_peak_threads",
    "sampled_process_group_peak_rss_kib",
    "hard_timeout_triggered", "sampled_limit_triggered",
    "monitor_error", "pgid_query_error", "finalizer_error",
    "signal_received", "final_pgid_empty", "runner_exit_status",
    "monitor_fault_test_pass", "pgid_kill_escalation_used",
    "kernel_hard_memory_ceiling"
  )
  expected_resource_tail <- data.frame(
    value = c(
      "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "NONE", "TRUE",
      "0", "TRUE", "FALSE", "FALSE"
    ),
    limit = c(
      "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "NONE", "TRUE",
      "0", "TRUE", "FALSE", "FALSE"
    ),
    stringsAsFactors = FALSE
  )
  resource_peak_values <- suppressWarnings(as.numeric(resource$value[1:3]))
  resource_peak_limits <- suppressWarnings(as.numeric(resource$limit[1:3]))
  expected_monitor_metrics <- c(
    "leader_exit_status", "descendant_seen_after_leader_exit",
    "pgid_drain_completed", "kill_escalation_used", "final_pgid_empty"
  )
  expected_monitor_values <- c("17", "TRUE", "TRUE", "TRUE", "TRUE")
  expected_monitor_expected <- c("17", "TRUE", "TRUE", "TRUE", "TRUE")
  if (nrow(failure) != 0L ||
      nrow(resource) != 14L ||
      !identical(as.character(resource$metric), expected_resource_metrics) ||
      !rqr_dlm_companion_logical_column(resource$pass, 14L) ||
      !all(resource$pass) ||
      anyNA(resource_peak_values) || anyNA(resource_peak_limits) ||
      !identical(resource_peak_limits, c(3, 4, 4194304)) ||
      any(resource_peak_values < 0) ||
      any(resource_peak_values > resource_peak_limits) ||
      !identical(
        data.frame(
          value = as.character(resource$value[4:14]),
          limit = as.character(resource$limit[4:14]),
          stringsAsFactors = FALSE
        ),
        expected_resource_tail
      ) ||
      nrow(monitor) != 5L ||
      !identical(as.character(monitor$metric), expected_monitor_metrics) ||
      !identical(as.character(monitor$value), expected_monitor_values) ||
      !identical(
        as.character(monitor$expected), expected_monitor_expected
      ) ||
      !rqr_dlm_companion_logical_column(monitor$pass, 5L) ||
      !all(monitor$pass) ||
      nrow(process_monitor) < 1L ||
      !rqr_dlm_companion_integer_column(
        process_monitor$elapsed_seconds, minimum = 0
      ) ||
      !rqr_dlm_companion_integer_column(
        process_monitor$processes, minimum = 0
      ) ||
      !rqr_dlm_companion_integer_column(
        process_monitor$threads, minimum = 0
      ) ||
      !rqr_dlm_companion_integer_column(
        process_monitor$rss_kib, minimum = 0
      ) ||
      !identical(
        as.numeric(max(process_monitor$processes)),
        resource_peak_values[[1L]]
      ) ||
      !identical(
        as.numeric(max(process_monitor$threads)),
        resource_peak_values[[2L]]
      ) ||
      !identical(
        as.numeric(max(process_monitor$rss_kib)),
        resource_peak_values[[3L]]
      ) ||
      !identical(as.character(closeout$field), expected_closeout) ||
      !identical(
        closeout_values[["schema_version"]],
        "rqrgibbs_dlm_wrapper_closeout/2.1.0"
      ) ||
      !identical(closeout_values[["mode"]], "reference-only") ||
      !identical(
        tolower(closeout_values[["expected_primary_commit"]]),
        expected_commit
      ) ||
      !identical(closeout_values[["runner_exit_status"]], "0") ||
      !identical(closeout_values[["resource_pass"]], "TRUE") ||
      !identical(
        closeout_values[["monitor_kind"]], "pgid_sampled_fallback"
      ) ||
      !identical(
        closeout_values[["kernel_hard_memory_ceiling"]], "FALSE"
      ) ||
      !identical(closeout_values[["signal_received"]], "NONE") ||
      !identical(closeout_values[["final_pgid_empty"]], "TRUE") ||
      !identical(closeout_values[["finalizer_error"]], "FALSE")) {
    rqr_dlm_companion_fail(
      "DLM reference wrapper, resource, or failure evidence failed."
    )
  }
  list(
    role = "dlm_reference", directory = directory, artifacts = artifacts,
    schema_version = run$schema_version, source_commit = expected_commit,
    package_version = runtime$dependency_versions$rqrgibbs,
    runtime_tree_digest = run$primary_runtime_tree_digest,
    runtime_attestation_sha256 =
      run$primary_runtime_attestation_sha256,
    config_digest = run$config_digest,
    incidence_digest = NA_character_, seed_ledger_digest = NA_character_,
    semantic_counts = c(
      reference_gates = nrow(gates), continuation_cells = nrow(continuation),
      history_mutations = nrow(mutations)
    )
  )
}

rqr_dlm_companion_m01_manifest_fields <- function() {
  c(
    "schema_version", "source_commit", "source_clean",
    "package_version", "primary_runtime_attestation_sha256",
    "config_digest", "incidence_digest", "seed_ledger_digest",
    "wave_id", "wave_task_count", "chain_job_count", "workers",
    "thread_environment", "component_scale_kernel",
    "standard_component_scale_schedule",
    "sentinel_component_scale_schedule",
    "exact_target_preserving_kernel",
    "comparative_simulation_metrics_used", "failed_outputs_reused",
    "all_fits_succeeded", "all_fits_reproducibility_eligible",
    "unique_runtime_tree_digests", "total_fit_elapsed_seconds",
    "all_diagnostics_passed", "started_at_utc", "completed_at_utc"
  )
}

rqr_dlm_companion_m02_manifest_fields <- function() {
  c(
    "schema_version", "source_commit", "source_clean",
    "package_version", "primary_runtime_attestation_sha256",
    "primary_reproducibility_eligible",
    "exdqlm_runtime_attestation_sha256",
    "exdqlm_runtime_tree_digest", "exdqlm_source_package_sha256",
    "config_digest", "incidence_digest", "seed_ledger_digest",
    "wave_id", "wave_task_count", "interval_chain_job_count",
    "logical_endpoint_fit_count", "workers", "thread_environment",
    "comparator_projection", "comparative_simulation_metrics_used",
    "failed_outputs_reused", "all_fits_succeeded",
    "all_diagnostics_passed", "total_fit_elapsed_seconds",
    "started_at_utc", "completed_at_utc"
  )
}

rqr_dlm_companion_horizon_manifest_fields <- function() {
  c(
    "schema_version", "source_commit", "source_clean",
    "package_version", "primary_runtime_attestation_sha256",
    "config_digest", "incidence_digest", "seed_ledger_digest",
    "horizon_scenarios", "horizon_checks_passed",
    "fixed_design_standard_tasks", "fixed_design_standard_schedule",
    "fixed_design_fits_succeeded",
    "fixed_design_diagnostics_passed",
    "fixed_design_reproducibility_eligible",
    "dynamic_endpoint_check_passed",
    "comparative_simulation_metrics_used", "failed_outputs_reused",
    "workers", "thread_environment", "started_at_utc",
    "completed_at_utc"
  )
}

rqr_dlm_companion_validate_m01 <- function(
    directory, expected_commit, reference) {
  directory <- rqr_dlm_companion_validate_directory(directory, "M01")
  artifacts <- rqr_dlm_companion_verify_artifact_manifest(
    directory, "M01", c("path", "bytes", "sha256")
  )
  manifest <- rqr_dlm_companion_read_json(
    file.path(directory, "validation_manifest.json"),
    rqr_dlm_companion_m01_manifest_fields(),
    "M01 validation_manifest.json"
  )
  rqr_dlm_companion_assert_thread_environment(
    manifest$thread_environment, "M01"
  )
  kernel <- manifest$component_scale_kernel
  standard <- manifest$standard_component_scale_schedule
  sentinel <- manifest$sentinel_component_scale_schedule
  if (!identical(
        manifest$schema_version,
        "rqrgibbs_dlm_wave1_correction_validation/1.0.0"
      ) ||
      !identical(tolower(manifest$source_commit), expected_commit) ||
      !rqr_dlm_companion_scalar_true(manifest$source_clean) ||
      !identical(manifest$package_version, reference$package_version) ||
      !identical(
        manifest$primary_runtime_attestation_sha256,
        reference$runtime_attestation_sha256
      ) ||
      !identical(
        manifest$unique_runtime_tree_digests,
        reference$runtime_tree_digest
      ) ||
      !identical(
        manifest$wave_id,
        "static_gaussian_T200__target0200__sentinel"
      ) ||
      !rqr_dlm_companion_is_integerish(manifest$wave_task_count, 20, 20) ||
      !rqr_dlm_companion_is_integerish(manifest$chain_job_count, 44, 44) ||
      !rqr_dlm_companion_is_integerish(manifest$workers, 8, 8) ||
      !rqr_dlm_companion_finite_column(
        manifest$total_fit_elapsed_seconds, 1L, minimum = 0
      ) ||
      !rqr_dlm_companion_scalar_true(
        manifest$exact_target_preserving_kernel
      ) ||
      !rqr_dlm_companion_scalar_false(
        manifest$comparative_simulation_metrics_used
      ) ||
      !rqr_dlm_companion_scalar_false(manifest$failed_outputs_reused) ||
      !rqr_dlm_companion_scalar_true(manifest$all_fits_succeeded) ||
      !rqr_dlm_companion_scalar_true(
        manifest$all_fits_reproducibility_eligible
      ) ||
      !rqr_dlm_companion_scalar_true(manifest$all_diagnostics_passed) ||
      !is.list(kernel) ||
      !identical(
        names(kernel),
        c(
          "centered_inverse_gamma", "noncentered_slice_interweave",
          "interweave_cycles", "slice_width", "slice_sweeps_per_cycle",
          "slice_max_steps", "slice_max_shrink", "target_change"
        )
      ) ||
      !rqr_dlm_companion_scalar_true(kernel$centered_inverse_gamma) ||
      !rqr_dlm_companion_scalar_true(
        kernel$noncentered_slice_interweave
      ) ||
      !rqr_dlm_companion_is_integerish(
        kernel$interweave_cycles, 1, 1
      ) ||
      !rqr_dlm_companion_is_integerish(kernel$slice_width, 1, 1) ||
      !rqr_dlm_companion_is_integerish(
        kernel$slice_sweeps_per_cycle, 2, 2
      ) ||
      !rqr_dlm_companion_is_integerish(
        kernel$slice_max_steps, 100, 100
      ) ||
      !rqr_dlm_companion_is_integerish(
        kernel$slice_max_shrink, 1000, 1000
      ) ||
      !rqr_dlm_companion_scalar_false(kernel$target_change) ||
      !is.list(standard) ||
      !identical(names(standard), c("burn", "retain", "thin")) ||
      !all(vapply(
        standard, rqr_dlm_companion_is_integerish, logical(1L),
        minimum = 1
      )) ||
      !identical(
        unname(as.numeric(unlist(standard))),
        c(1000, 6000, 1)
      ) ||
      !is.list(sentinel) ||
      !identical(names(sentinel), c("burn", "retain", "thin")) ||
      !all(vapply(
        sentinel, rqr_dlm_companion_is_integerish, logical(1L),
        minimum = 1
      )) ||
      !identical(
        unname(as.numeric(unlist(sentinel))),
        c(1000, 2000, 1)
      )) {
    rqr_dlm_companion_fail(
      "M01 source, runtime, target, fit, diagnostic, or interweaving gate failed."
    )
  }
  diagnostics <- rqr_dlm_companion_read_csv(
    file.path(directory, "wave1_M01_diagnostics.csv"),
    c(
      "estimand", "chains", "rhat", "ess_bulk", "ess_tail",
      "mcse_mean", "mcse_over_sd", "pass", "DGP", "replication",
      "sentinel"
    ),
    "M01 diagnostics"
  )
  summary <- rqr_dlm_companion_read_csv(
    file.path(directory, "wave1_M01_summary.csv"),
    c(
      "DGP", "replication", "sentinel", "chains", "diagnostics",
      "diagnostics_passed", "all_pass", "fit_elapsed_seconds",
      "log_q_1_rhat", "log_q_1_ess_bulk", "log_q_1_ess_tail",
      "log_q_1_mcse_over_sd"
    ),
    "M01 summary"
  )
  if (nrow(summary) != 20L ||
      any(!summary$DGP %in% c("S01", "S02")) ||
      !rqr_dlm_companion_integer_column(
        summary$replication, 20L, minimum = 1
      ) ||
      !rqr_dlm_companion_logical_column(summary$sentinel, 20L) ||
      sum(summary$sentinel) != 8L ||
      !rqr_dlm_companion_integer_column(
        summary$chains, 20L, minimum = 1
      ) ||
      any(summary$chains != ifelse(summary$sentinel, 4, 1)) ||
      sum(summary$chains) != 44L ||
      !rqr_dlm_companion_integer_column(
        summary$diagnostics, 20L, minimum = 46, maximum = 46
      ) ||
      !rqr_dlm_companion_integer_column(
        summary$diagnostics_passed, 20L, minimum = 46, maximum = 46
      ) ||
      !rqr_dlm_companion_logical_column(summary$all_pass, 20L) ||
      !all(summary$all_pass) ||
      !rqr_dlm_companion_finite_column(
        summary$fit_elapsed_seconds, 20L, minimum = 0
      ) ||
      any(summary$diagnostics_passed != summary$diagnostics)) {
    rqr_dlm_companion_fail(
      "M01 diagnostic and summary tables are incomplete or failed."
    )
  }
  rqr_dlm_companion_validate_diagnostic_grid(
    diagnostics, summary, "M01", has_sentinel = TRUE
  )
  summary_key <- paste(summary$DGP, summary$replication, sep = "::")
  log_q <- diagnostics[diagnostics$estimand == "log_q_1", ]
  log_q_key <- paste(log_q$DGP, log_q$replication, sep = "::")
  log_q <- log_q[match(summary_key, log_q_key), , drop = FALSE]
  if (!identical(
        as.numeric(summary$log_q_1_rhat), as.numeric(log_q$rhat)
      ) ||
      !identical(
        as.numeric(summary$log_q_1_ess_bulk),
        as.numeric(log_q$ess_bulk)
      ) ||
      !identical(
        as.numeric(summary$log_q_1_ess_tail),
        as.numeric(log_q$ess_tail)
      ) ||
      !identical(
        as.numeric(summary$log_q_1_mcse_over_sd),
        as.numeric(log_q$mcse_over_sd)
      )) {
    rqr_dlm_companion_fail(
      "M01 log-q summary sidecars do not match diagnostics."
    )
  }
  list(
    role = "M01", directory = directory, artifacts = artifacts,
    schema_version = manifest$schema_version,
    source_commit = expected_commit,
    package_version = manifest$package_version,
    runtime_tree_digest = reference$runtime_tree_digest,
    runtime_attestation_sha256 =
      manifest$primary_runtime_attestation_sha256,
    config_digest = manifest$config_digest,
    incidence_digest = manifest$incidence_digest,
    seed_ledger_digest = manifest$seed_ledger_digest,
    task_keys = sort(paste(summary$DGP, summary$replication, sep = "::")),
    semantic_counts = c(
      wave_tasks = 20, chain_jobs = 44,
      diagnostics = nrow(diagnostics)
    )
  )
}

rqr_dlm_companion_validate_m02 <- function(
    directory, expected_commit, reference) {
  directory <- rqr_dlm_companion_validate_directory(directory, "M02")
  artifacts <- rqr_dlm_companion_verify_artifact_manifest(
    directory, "M02", c("path", "bytes", "sha256")
  )
  manifest <- rqr_dlm_companion_read_json(
    file.path(directory, "validation_manifest.json"),
    rqr_dlm_companion_m02_manifest_fields(),
    "M02 validation_manifest.json"
  )
  rqr_dlm_companion_assert_thread_environment(
    manifest$thread_environment, "M02"
  )
  if (!identical(
        manifest$schema_version,
        "rqrgibbs_dlm_wave1_comparator_projection_validation/1.0.0"
      ) ||
      !identical(tolower(manifest$source_commit), expected_commit) ||
      !rqr_dlm_companion_scalar_true(manifest$source_clean) ||
      !identical(manifest$package_version, reference$package_version) ||
      !identical(
        manifest$primary_runtime_attestation_sha256,
        reference$runtime_attestation_sha256
      ) ||
      !rqr_dlm_companion_scalar_true(
        manifest$primary_reproducibility_eligible
      ) ||
      !rqr_dlm_companion_is_hex(
        manifest$exdqlm_runtime_attestation_sha256
      ) ||
      !rqr_dlm_companion_is_hex(manifest$exdqlm_runtime_tree_digest) ||
      !rqr_dlm_companion_is_hex(
        manifest$exdqlm_source_package_sha256
      ) ||
      !identical(
        manifest$wave_id,
        "static_gaussian_T200__target0200__sentinel"
      ) ||
      !rqr_dlm_companion_is_integerish(manifest$wave_task_count, 20, 20) ||
      !rqr_dlm_companion_is_integerish(
        manifest$interval_chain_job_count, 44, 44
      ) ||
      !rqr_dlm_companion_is_integerish(
        manifest$logical_endpoint_fit_count, 88, 88
      ) ||
      !rqr_dlm_companion_is_integerish(manifest$workers, 8, 8) ||
      !rqr_dlm_companion_finite_column(
        manifest$total_fit_elapsed_seconds, 1L, minimum = 0
      ) ||
      !identical(
        manifest$comparator_projection,
        "colSums(FF * posterior_state_mean_or_draw)"
      ) ||
      !rqr_dlm_companion_scalar_false(
        manifest$comparative_simulation_metrics_used
      ) ||
      !rqr_dlm_companion_scalar_false(manifest$failed_outputs_reused) ||
      !rqr_dlm_companion_scalar_true(manifest$all_fits_succeeded) ||
      !rqr_dlm_companion_scalar_true(manifest$all_diagnostics_passed)) {
    rqr_dlm_companion_fail(
      "M02 source, runtime, projection, fit, or diagnostic gate failed."
    )
  }
  diagnostics <- rqr_dlm_companion_read_csv(
    file.path(directory, "wave1_M02_diagnostics.csv"),
    c(
      "estimand", "chains", "rhat", "ess_bulk", "ess_tail",
      "mcse_mean", "mcse_over_sd", "pass", "DGP", "replication",
      "sentinel"
    ),
    "M02 diagnostics"
  )
  summary <- rqr_dlm_companion_read_csv(
    file.path(directory, "wave1_M02_summary.csv"),
    c(
      "DGP", "replication", "sentinel", "chains", "diagnostics",
      "diagnostics_passed", "all_pass", "fit_elapsed_seconds",
      "minimum_bulk_ess", "minimum_tail_ess", "maximum_mcse_over_sd"
    ),
    "M02 summary"
  )
  if (nrow(summary) != 20L ||
      any(!summary$DGP %in% c("S01", "S02")) ||
      !rqr_dlm_companion_integer_column(
        summary$replication, 20L, minimum = 1
      ) ||
      !rqr_dlm_companion_logical_column(summary$sentinel, 20L) ||
      sum(summary$sentinel) != 8L ||
      !rqr_dlm_companion_integer_column(
        summary$chains, 20L, minimum = 1
      ) ||
      any(summary$chains != ifelse(summary$sentinel, 4, 1)) ||
      sum(summary$chains) != 44L ||
      !rqr_dlm_companion_integer_column(
        summary$diagnostics, 20L, minimum = 45, maximum = 45
      ) ||
      !rqr_dlm_companion_integer_column(
        summary$diagnostics_passed, 20L, minimum = 45, maximum = 45
      ) ||
      !rqr_dlm_companion_logical_column(summary$all_pass, 20L) ||
      !all(summary$all_pass) ||
      !rqr_dlm_companion_finite_column(
        summary$fit_elapsed_seconds, 20L, minimum = 0
      ) ||
      any(summary$diagnostics_passed != summary$diagnostics)) {
    rqr_dlm_companion_fail(
      "M02 diagnostic and summary tables are incomplete or failed."
    )
  }
  rqr_dlm_companion_validate_diagnostic_grid(
    diagnostics, summary, "M02", has_sentinel = TRUE
  )
  summary_key <- paste(summary$DGP, summary$replication, sep = "::")
  diagnostic_key <- factor(
    paste(diagnostics$DGP, diagnostics$replication, sep = "::"),
    levels = summary_key
  )
  minimum_bulk <- vapply(
    split(diagnostics$ess_bulk, diagnostic_key), min, numeric(1L)
  )
  minimum_tail <- vapply(
    split(diagnostics$ess_tail, diagnostic_key), min, numeric(1L)
  )
  maximum_mcse <- vapply(
    split(diagnostics$mcse_over_sd, diagnostic_key), max, numeric(1L)
  )
  if (!identical(
        as.numeric(summary$minimum_bulk_ess), unname(minimum_bulk)
      ) ||
      !identical(
        as.numeric(summary$minimum_tail_ess), unname(minimum_tail)
      ) ||
      !identical(
        as.numeric(summary$maximum_mcse_over_sd), unname(maximum_mcse)
      )) {
    rqr_dlm_companion_fail(
      "M02 diagnostic extrema do not match their summary sidecars."
    )
  }
  list(
    role = "M02", directory = directory, artifacts = artifacts,
    schema_version = manifest$schema_version,
    source_commit = expected_commit,
    package_version = manifest$package_version,
    runtime_tree_digest = reference$runtime_tree_digest,
    runtime_attestation_sha256 =
      manifest$primary_runtime_attestation_sha256,
    config_digest = manifest$config_digest,
    incidence_digest = manifest$incidence_digest,
    seed_ledger_digest = manifest$seed_ledger_digest,
    task_keys = sort(paste(summary$DGP, summary$replication, sep = "::")),
    semantic_counts = c(
      wave_tasks = 20, interval_chains = 44,
      endpoint_fits = 88, diagnostics = nrow(diagnostics)
    )
  )
}

rqr_dlm_companion_validate_horizon <- function(
    directory, expected_commit, reference) {
  directory <- rqr_dlm_companion_validate_directory(
    directory, "horizon_M03"
  )
  artifacts <- rqr_dlm_companion_verify_artifact_manifest(
    directory, "horizon_M03", c("path", "bytes", "sha256")
  )
  manifest <- rqr_dlm_companion_read_json(
    file.path(directory, "validation_manifest.json"),
    rqr_dlm_companion_horizon_manifest_fields(),
    "horizon/M03 validation_manifest.json"
  )
  rqr_dlm_companion_assert_thread_environment(
    manifest$thread_environment, "horizon/M03"
  )
  schedule <- manifest$fixed_design_standard_schedule
  if (!identical(
        manifest$schema_version,
        "rqrgibbs_dlm_horizon_fixed_design_validation/1.0.0"
      ) ||
      !identical(tolower(manifest$source_commit), expected_commit) ||
      !rqr_dlm_companion_scalar_true(manifest$source_clean) ||
      !identical(manifest$package_version, reference$package_version) ||
      !identical(
        manifest$primary_runtime_attestation_sha256,
        reference$runtime_attestation_sha256
      ) ||
      !rqr_dlm_companion_is_integerish(
        manifest$horizon_scenarios, 16, 16
      ) ||
      !rqr_dlm_companion_scalar_true(manifest$horizon_checks_passed) ||
      !rqr_dlm_companion_is_integerish(
        manifest$fixed_design_standard_tasks, 8, 8
      ) ||
      !rqr_dlm_companion_is_integerish(manifest$workers, 8, 8) ||
      !is.list(schedule) ||
      !identical(names(schedule), c("burn", "retain", "thin")) ||
      !all(vapply(
        schedule, rqr_dlm_companion_is_integerish, logical(1L),
        minimum = 1
      )) ||
      !identical(
        unname(as.numeric(unlist(schedule))), c(500, 3000, 1)
      ) ||
      !rqr_dlm_companion_scalar_true(
        manifest$fixed_design_fits_succeeded
      ) ||
      !rqr_dlm_companion_scalar_true(
        manifest$fixed_design_diagnostics_passed
      ) ||
      !rqr_dlm_companion_scalar_true(
        manifest$fixed_design_reproducibility_eligible
      ) ||
      !rqr_dlm_companion_scalar_true(
        manifest$dynamic_endpoint_check_passed
      ) ||
      !rqr_dlm_companion_scalar_false(
        manifest$comparative_simulation_metrics_used
      ) ||
      !rqr_dlm_companion_scalar_false(manifest$failed_outputs_reused)) {
    rqr_dlm_companion_fail(
      "Horizon/M03 source, runtime, fit, or diagnostic gate failed."
    )
  }
  horizon <- rqr_dlm_companion_read_csv(
    file.path(directory, "horizon_checks.csv"),
    c(
      "DGP", "state_dimension", "training_horizon_expected",
      "training_horizon_observed", "future_horizon_expected",
      "future_horizon_observed", "full_horizon_expected",
      "full_horizon_observed", "training_partition_exact",
      "future_partition_exact", "pass"
    ),
    "horizon checks"
  )
  diagnostics <- rqr_dlm_companion_read_csv(
    file.path(directory, "fixed_design_diagnostics.csv"),
    c(
      "estimand", "chains", "rhat", "ess_bulk", "ess_tail",
      "mcse_mean", "mcse_over_sd", "pass", "DGP", "replication"
    ),
    "M03 diagnostics"
  )
  summary <- rqr_dlm_companion_read_csv(
    file.path(directory, "fixed_design_summary.csv"),
    c(
      "DGP", "replication", "fit_succeeded", "diagnostics",
      "diagnostics_passed", "all_diagnostics_passed",
      "minimum_bulk_ess", "minimum_tail_ess", "maximum_mcse_over_sd",
      "elapsed_seconds"
    ),
    "M03 summary"
  )
  endpoint <- rqr_dlm_companion_read_csv(
    file.path(directory, "dynamic_endpoint_check.csv"),
    c(
      "DGP", "replication", "method", "training_horizon_expected",
      "training_lower_length", "training_upper_length",
      "future_horizon_expected", "future_lower_length",
      "future_upper_length", "numerical_repairs", "exact_joint_target",
      "reproducibility_eligible", "elapsed_seconds", "error_message",
      "pass"
    ),
    "dynamic endpoint check"
  )
  expected_horizon_dgp <- sprintf("S%02d", 1:16)
  expected_horizon_dimension <- c(
    2, 2, 1, 1, 1, 1, 4, 4, 4, 3, 3, 2, 1, 1, 1, 1
  )
  expected_training_horizon <- c(rep(200, 14), 100, 400)
  if (nrow(horizon) != 16L ||
      !identical(as.character(horizon$DGP), expected_horizon_dgp) ||
      !identical(
        as.numeric(horizon$state_dimension),
        expected_horizon_dimension
      ) ||
      !identical(
        as.numeric(horizon$training_horizon_expected),
        expected_training_horizon
      ) ||
      !identical(
        as.numeric(horizon$future_horizon_expected), rep(20, 16)
      ) ||
      any(
        horizon$full_horizon_expected !=
          horizon$training_horizon_expected +
            horizon$future_horizon_expected
      ) ||
      !rqr_dlm_companion_logical_column(horizon$pass, 16L) ||
      !all(horizon$pass) ||
      any(horizon$training_horizon_expected !=
            horizon$training_horizon_observed) ||
      any(horizon$future_horizon_expected !=
            horizon$future_horizon_observed) ||
      any(horizon$full_horizon_expected !=
            horizon$full_horizon_observed) ||
      !rqr_dlm_companion_logical_column(
        horizon$training_partition_exact, 16L
      ) ||
      !all(horizon$training_partition_exact) ||
      !rqr_dlm_companion_logical_column(
        horizon$future_partition_exact, 16L
      ) ||
      !all(horizon$future_partition_exact) ||
      nrow(summary) != 8L ||
      any(summary$DGP != "S01") ||
      !rqr_dlm_companion_integer_column(
        summary$replication, 8L, minimum = 1
      ) ||
      anyDuplicated(summary$replication) ||
      !rqr_dlm_companion_logical_column(summary$fit_succeeded, 8L) ||
      !all(summary$fit_succeeded) ||
      !rqr_dlm_companion_integer_column(
        summary$diagnostics, 8L, minimum = 41, maximum = 41
      ) ||
      !rqr_dlm_companion_integer_column(
        summary$diagnostics_passed, 8L, minimum = 41, maximum = 41
      ) ||
      !rqr_dlm_companion_logical_column(
        summary$all_diagnostics_passed, 8L
      ) ||
      !all(summary$all_diagnostics_passed) ||
      any(summary$diagnostics_passed != summary$diagnostics) ||
      !rqr_dlm_companion_finite_column(
        summary$elapsed_seconds, 8L, minimum = 0
      ) ||
      nrow(endpoint) != 1L ||
      !identical(endpoint$DGP[[1L]], "S03") ||
      !identical(as.numeric(endpoint$replication[[1L]]), 13) ||
      !identical(endpoint$method[[1L]], "M01") ||
      !identical(
        as.numeric(endpoint$training_horizon_expected[[1L]]), 200
      ) ||
      any(as.numeric(unlist(endpoint[1L, c(
        "training_lower_length", "training_upper_length"
      )], use.names = FALSE)) != 200) ||
      !identical(
        as.numeric(endpoint$future_horizon_expected[[1L]]), 20
      ) ||
      any(as.numeric(unlist(endpoint[1L, c(
        "future_lower_length", "future_upper_length"
      )], use.names = FALSE)) != 20) ||
      !rqr_dlm_companion_integer_column(
        endpoint$numerical_repairs, 1L, minimum = 0, maximum = 0
      ) ||
      !rqr_dlm_companion_logical_column(endpoint$pass, 1L) ||
      !all(endpoint$pass) ||
      !rqr_dlm_companion_logical_column(
        endpoint$exact_joint_target, 1L
      ) ||
      !all(endpoint$exact_joint_target) ||
      !rqr_dlm_companion_logical_column(
        endpoint$reproducibility_eligible, 1L
      ) ||
      !all(endpoint$reproducibility_eligible) ||
      !rqr_dlm_companion_finite_column(
        endpoint$elapsed_seconds, 1L, minimum = 0
      ) ||
      !(is.na(endpoint$error_message[[1L]]) ||
        identical(endpoint$error_message[[1L]], ""))) {
    rqr_dlm_companion_fail(
      "Horizon, M03, or dynamic-endpoint semantic evidence failed."
    )
  }
  rqr_dlm_companion_validate_diagnostic_grid(
    diagnostics, summary, "M03", has_sentinel = FALSE
  )
  summary_key <- paste(summary$DGP, summary$replication, sep = "::")
  diagnostic_key <- factor(
    paste(diagnostics$DGP, diagnostics$replication, sep = "::"),
    levels = summary_key
  )
  minimum_bulk <- vapply(
    split(diagnostics$ess_bulk, diagnostic_key), min, numeric(1L)
  )
  minimum_tail <- vapply(
    split(diagnostics$ess_tail, diagnostic_key), min, numeric(1L)
  )
  maximum_mcse <- vapply(
    split(diagnostics$mcse_over_sd, diagnostic_key), max, numeric(1L)
  )
  if (!identical(
        as.numeric(summary$minimum_bulk_ess), unname(minimum_bulk)
      ) ||
      !identical(
        as.numeric(summary$minimum_tail_ess), unname(minimum_tail)
      ) ||
      !identical(
        as.numeric(summary$maximum_mcse_over_sd), unname(maximum_mcse)
      )) {
    rqr_dlm_companion_fail(
      "M03 diagnostic extrema do not match their summary sidecars."
    )
  }
  list(
    role = "horizon_M03", directory = directory, artifacts = artifacts,
    schema_version = manifest$schema_version,
    source_commit = expected_commit,
    package_version = manifest$package_version,
    runtime_tree_digest = reference$runtime_tree_digest,
    runtime_attestation_sha256 =
      manifest$primary_runtime_attestation_sha256,
    config_digest = manifest$config_digest,
    incidence_digest = manifest$incidence_digest,
    seed_ledger_digest = manifest$seed_ledger_digest,
    task_keys = sort(paste(summary$DGP, summary$replication, sep = "::")),
    semantic_counts = c(
      horizon_scenarios = 16, M03_tasks = 8,
      M03_diagnostics = nrow(diagnostics), dynamic_endpoint_checks = 1
    )
  )
}

rqr_dlm_companion_cross_validate <- function(inputs, expected_commit) {
  if (!identical(names(inputs), c(
        "dlm_reference", "M01", "M02", "horizon_M03"
      ))) {
    rqr_dlm_companion_fail("The companion input-role set is incomplete.")
  }
  if (any(vapply(
        inputs,
        function(x) !identical(x$source_commit, expected_commit),
        logical(1L)
      ))) {
    rqr_dlm_companion_fail("A companion source commit does not match.")
  }
  runtime_attestations <- vapply(
    inputs, `[[`, character(1L), "runtime_attestation_sha256"
  )
  runtime_trees <- vapply(
    inputs, `[[`, character(1L), "runtime_tree_digest"
  )
  if (length(unique(runtime_attestations)) != 1L ||
      length(unique(runtime_trees)) != 1L) {
    rqr_dlm_companion_fail(
      "The protected-DLM inputs do not share one exact primary runtime."
    )
  }
  correction <- inputs[c("M01", "M02", "horizon_M03")]
  for (field in c(
      "config_digest", "incidence_digest", "seed_ledger_digest"
    )) {
    values <- vapply(correction, `[[`, character(1L), field)
    if (any(!grepl("^[0-9a-f]{64}$", values)) ||
        length(unique(values)) != 1L) {
      rqr_dlm_companion_fail(
        "The M01/M02/horizon-M03 ", field, " values do not match."
      )
    }
  }
  if (!identical(inputs$M01$task_keys, inputs$M02$task_keys) ||
      any(!inputs$horizon_M03$task_keys %in% inputs$M01$task_keys)) {
    rqr_dlm_companion_fail(
      "The M01, M02, and M03 task-key evidence is inconsistent."
    )
  }
  invisible(TRUE)
}

rqr_dlm_companion_capture_input_ledger <- function(directories) {
  roles <- c("dlm_reference", "M01", "M02", "horizon_M03")
  if (!identical(names(directories), roles)) {
    rqr_dlm_companion_fail(
      "Cannot capture an incomplete protected-DLM input-role set."
    )
  }
  do.call(rbind, lapply(roles, function(role) {
    directory <- rqr_dlm_companion_validate_directory(
      directories[[role]], role
    )
    files <- sort(rqr_dlm_companion_input_files(role))
    paths <- file.path(directory, files)
    data.frame(
      input_role = role, relative_path = files,
      byte_count = as.numeric(file.info(paths)$size),
      sha256 = vapply(
        paths, rqr_dlm_companion_sha256, character(1L)
      ),
      stringsAsFactors = FALSE
    )
  }))
}

rqr_dlm_companion_assert_input_closure <- function(inputs) {
  for (role in names(inputs)) {
    rqr_dlm_companion_validate_directory(inputs[[role]]$directory, role)
  }
  invisible(TRUE)
}

rqr_dlm_companion_assert_input_ledger <- function(inputs, ledger) {
  for (index in seq_len(nrow(ledger))) {
    role <- ledger$input_role[[index]]
    if (!role %in% names(inputs)) {
      rqr_dlm_companion_fail("Input ledger contains an unknown role.")
    }
    path <- file.path(
      inputs[[role]]$directory, ledger$relative_path[[index]]
    )
    if (!file.exists(path) || !utils::file_test("-f", path) ||
        any(rqr_dlm_companion_is_symlink(path)) ||
        !identical(
          as.numeric(file.info(path)$size),
          as.numeric(ledger$byte_count[[index]])
        ) ||
        !identical(
          rqr_dlm_companion_sha256(path),
          tolower(ledger$sha256[[index]])
        )) {
      rqr_dlm_companion_fail(
        "A protected-DLM input changed during collection: ",
        role, "/", ledger$relative_path[[index]]
      )
    }
  }
  invisible(TRUE)
}

rqr_dlm_companion_atomic_csv <- function(value, path) {
  if (file.exists(path) || any(rqr_dlm_companion_is_symlink(path))) {
    rqr_dlm_companion_fail(
      "Refusing to overwrite or follow an output artifact: ", path
    )
  }
  temporary <- tempfile(
    paste0(".", basename(path), "-"), tmpdir = dirname(path)
  )
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  utils::write.csv(value, temporary, row.names = FALSE, quote = TRUE)
  read_back <- utils::read.csv(
    temporary, stringsAsFactors = FALSE, check.names = FALSE
  )
  if (!identical(names(read_back), names(value)) ||
      nrow(read_back) != nrow(value)) {
    rqr_dlm_companion_fail("Atomic CSV read-back validation failed.")
  }
  rqr_dlm_companion_sha256(temporary)
  if (!file.rename(temporary, path)) {
    rqr_dlm_companion_fail("Atomic CSV publication failed.")
  }
  invisible(path)
}

rqr_dlm_companion_atomic_json <- function(value, path) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    rqr_dlm_companion_fail("The jsonlite package is required.")
  }
  if (file.exists(path) || any(rqr_dlm_companion_is_symlink(path))) {
    rqr_dlm_companion_fail(
      "Refusing to overwrite or follow an output artifact: ", path
    )
  }
  temporary <- tempfile(
    paste0(".", basename(path), "-"), tmpdir = dirname(path)
  )
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  jsonlite::write_json(
    value, temporary, auto_unbox = TRUE, pretty = TRUE, null = "null"
  )
  jsonlite::read_json(temporary, simplifyVector = FALSE)
  rqr_dlm_companion_sha256(temporary)
  if (!file.rename(temporary, path)) {
    rqr_dlm_companion_fail("Atomic JSON publication failed.")
  }
  invisible(path)
}

rqr_dlm_companion_recursive_manifest <- function(directory) {
  paths <- list.files(
    directory, recursive = TRUE, all.files = TRUE, no.. = TRUE,
    include.dirs = FALSE, full.names = TRUE
  )
  relative <- substring(paths, nchar(directory) + 2L)
  order <- order(relative, method = "radix")
  paths <- paths[order]
  relative <- relative[order]
  if (!all(utils::file_test("-f", paths)) ||
      any(rqr_dlm_companion_is_symlink(paths)) ||
      anyDuplicated(relative)) {
    rqr_dlm_companion_fail(
      "The output staging tree is not a unique regular-file set."
    )
  }
  data.frame(
    schema_version = rqr_dlm_companion_schema(),
    relative_path = relative,
    byte_count = as.numeric(file.info(paths)$size),
    sha256 = vapply(paths, rqr_dlm_companion_sha256, character(1L)),
    stringsAsFactors = FALSE
  )
}

rqr_dlm_companion_semantic_gate_names <- function() {
  c(
    "exact_source_commit", "exact_primary_runtime",
    "reference_artifact_closure", "reference_R_cpp_parity",
    "reference_missing_future", "reference_component_scale",
    "reference_six_continuation_cells", "reference_history_mutations",
    "reference_process_monitor", "M01_fits_and_diagnostics",
    "M01_interweaving_kernel",
    "M02_fits_diagnostics_and_projection", "horizon_scenarios",
    "M03_fits_and_diagnostics", "dynamic_endpoint_boundary",
    "confirmatory_contract_digests"
  )
}

rqr_dlm_companion_compact_files <- function() {
  c(
    "artifact_hashes.csv", "bundle_manifest.json",
    "input_artifact_hashes.csv", "input_bundle_summary.csv",
    "semantic_gates.csv"
  )
}

rqr_dlm_companion_compact_manifest_fields <- function() {
  c(
    "schema_version", "expected_primary_commit",
    "collector_source_commit", "primary_runtime_tree_digest",
    "primary_runtime_attestation_sha256", "package_version",
    "reference_config_digest", "confirmatory_config_digest",
    "confirmatory_incidence_digest",
    "confirmatory_seed_ledger_digest", "input_roles",
    "input_artifact_count", "semantic_gate_count",
    "semantic_gate_pass_count", "all_semantic_gates_passed",
    "fits_executed_by_collector", "heavy_input_artifacts_copied",
    "generalized_bayes", "response_likelihood",
    "response_prediction_contract", "created_at_utc"
  )
}

rqr_dlm_companion_validate_compact <- function(
    directory, expected_commit, expected_runtime_tree_digest,
    expected_runtime_attestation_sha256, expected_package_version,
    expected_collector_commit = expected_commit) {
  if (!rqr_dlm_companion_is_hex(expected_commit, 40L) ||
      !rqr_dlm_companion_is_hex(expected_collector_commit, 40L) ||
      !rqr_dlm_companion_is_hex(expected_runtime_tree_digest) ||
      !rqr_dlm_companion_is_hex(
        expected_runtime_attestation_sha256
      ) ||
      !is.character(expected_package_version) ||
      length(expected_package_version) != 1L ||
      is.na(expected_package_version) || !nzchar(expected_package_version)) {
    rqr_dlm_companion_fail(
      "Compact companion expectations are incomplete or malformed."
    )
  }
  expected_commit <- tolower(expected_commit)
  expected_collector_commit <- tolower(expected_collector_commit)
  expected_runtime_tree_digest <- tolower(
    expected_runtime_tree_digest
  )
  expected_runtime_attestation_sha256 <- tolower(
    expected_runtime_attestation_sha256
  )
  if (!is.character(directory) || length(directory) != 1L ||
      is.na(directory) || !nzchar(directory) || !dir.exists(directory) ||
      any(rqr_dlm_companion_is_symlink(directory))) {
    rqr_dlm_companion_fail(
      "Compact companion input must be one existing nonsymlink directory."
    )
  }
  directory <- normalizePath(directory, winslash = "/", mustWork = TRUE)
  entries <- list.files(
    directory, recursive = TRUE, all.files = TRUE, no.. = TRUE,
    include.dirs = TRUE, full.names = TRUE
  )
  relative <- substring(entries, nchar(directory) + 2L)
  if (!identical(sort(relative), rqr_dlm_companion_compact_files()) ||
      !all(utils::file_test("-f", entries)) ||
      any(rqr_dlm_companion_is_symlink(entries))) {
    rqr_dlm_companion_fail(
      "Compact companion has a missing, extra, nested, nonregular, or symlinked artifact."
    )
  }

  output_hashes <- rqr_dlm_companion_read_csv(
    file.path(directory, "artifact_hashes.csv"),
    c("schema_version", "relative_path", "byte_count", "sha256"),
    "compact companion artifact_hashes.csv"
  )
  expected_hashed_files <- sort(setdiff(
    rqr_dlm_companion_compact_files(), "artifact_hashes.csv"
  ))
  if (nrow(output_hashes) != 4L ||
      !identical(
        as.character(output_hashes$schema_version),
        rep(rqr_dlm_companion_schema(), 4L)
      ) ||
      !identical(
        as.character(output_hashes$relative_path), expected_hashed_files
      ) ||
      !rqr_dlm_companion_integer_column(
        output_hashes$byte_count, 4L, minimum = 0
      ) ||
      any(!vapply(
        as.character(output_hashes$sha256),
        rqr_dlm_companion_is_hex, logical(1L)
      ))) {
    rqr_dlm_companion_fail(
      "Compact companion recursive artifact manifest is malformed."
    )
  }
  for (index in seq_len(nrow(output_hashes))) {
    path <- file.path(directory, output_hashes$relative_path[[index]])
    if (!identical(
          as.numeric(file.info(path)$size),
          as.numeric(output_hashes$byte_count[[index]])
        ) ||
        !identical(
          rqr_dlm_companion_sha256(path),
          tolower(output_hashes$sha256[[index]])
        )) {
      rqr_dlm_companion_fail(
        "Compact companion recursive hash mismatch: ",
        output_hashes$relative_path[[index]]
      )
    }
  }

  manifest <- rqr_dlm_companion_read_json(
    file.path(directory, "bundle_manifest.json"),
    rqr_dlm_companion_compact_manifest_fields(),
    "compact companion bundle_manifest.json"
  )
  roles <- c("dlm_reference", "M01", "M02", "horizon_M03")
  manifest_roles <- unname(unlist(manifest$input_roles, use.names = FALSE))
  digest_fields <- c(
    "reference_config_digest", "confirmatory_config_digest",
    "confirmatory_incidence_digest",
    "confirmatory_seed_ledger_digest"
  )
  if (!identical(manifest$schema_version, rqr_dlm_companion_schema()) ||
      !identical(
        tolower(manifest$expected_primary_commit), expected_commit
      ) ||
      !identical(
        tolower(manifest$collector_source_commit),
        expected_collector_commit
      ) ||
      !identical(
        tolower(manifest$primary_runtime_tree_digest),
        expected_runtime_tree_digest
      ) ||
      !identical(
        tolower(manifest$primary_runtime_attestation_sha256),
        expected_runtime_attestation_sha256
      ) ||
      !identical(manifest$package_version, expected_package_version) ||
      !identical(manifest_roles, roles) ||
      any(!vapply(
        manifest[digest_fields],
        rqr_dlm_companion_is_hex, logical(1L)
      )) ||
      !rqr_dlm_companion_is_integerish(
        manifest$input_artifact_count, 39, 39
      ) ||
      !rqr_dlm_companion_is_integerish(
        manifest$semantic_gate_count, 16, 16
      ) ||
      !rqr_dlm_companion_is_integerish(
        manifest$semantic_gate_pass_count, 16, 16
      ) ||
      !rqr_dlm_companion_scalar_true(
        manifest$all_semantic_gates_passed
      ) ||
      !rqr_dlm_companion_scalar_false(
        manifest$fits_executed_by_collector
      ) ||
      !rqr_dlm_companion_scalar_false(
        manifest$heavy_input_artifacts_copied
      ) ||
      !rqr_dlm_companion_scalar_true(manifest$generalized_bayes) ||
      !rqr_dlm_companion_scalar_false(manifest$response_likelihood) ||
      !rqr_dlm_companion_scalar_false(
        manifest$response_prediction_contract
      ) ||
      !is.character(manifest$created_at_utc) ||
      length(manifest$created_at_utc) != 1L ||
      is.na(manifest$created_at_utc) || !nzchar(manifest$created_at_utc)) {
    rqr_dlm_companion_fail(
      "Compact companion manifest source, runtime, scope, or count gate failed."
    )
  }

  summary <- rqr_dlm_companion_read_csv(
    file.path(directory, "input_bundle_summary.csv"),
    c(
      "schema_version", "input_role", "input_schema", "source_commit",
      "package_version", "primary_runtime_tree_digest",
      "primary_runtime_attestation_sha256", "runtime_binding_basis",
      "artifact_count", "artifact_bytes",
      "artifact_manifest_sha256", "semantic_status"
    ),
    "compact companion input_bundle_summary.csv"
  )
  expected_input_schemas <- c(
    "rqrgibbs_dlm_bounded_run/3.0.0",
    "rqrgibbs_dlm_wave1_correction_validation/1.0.0",
    "rqrgibbs_dlm_wave1_comparator_projection_validation/1.0.0",
    "rqrgibbs_dlm_horizon_fixed_design_validation/1.0.0"
  )
  expected_binding <- c(
    "direct_runtime_tree_and_attestation",
    "direct_tree_plus_matching_reference_attestation",
    "matching_reference_attestation",
    "matching_reference_attestation"
  )
  if (nrow(summary) != 4L ||
      !identical(
        as.character(summary$schema_version),
        rep(rqr_dlm_companion_schema(), 4L)
      ) ||
      !identical(as.character(summary$input_role), roles) ||
      !identical(
        as.character(summary$input_schema), expected_input_schemas
      ) ||
      !identical(
        tolower(as.character(summary$source_commit)),
        rep(expected_commit, 4L)
      ) ||
      !identical(
        as.character(summary$package_version),
        rep(expected_package_version, 4L)
      ) ||
      !identical(
        tolower(as.character(summary$primary_runtime_tree_digest)),
        rep(expected_runtime_tree_digest, 4L)
      ) ||
      !identical(
        tolower(as.character(
          summary$primary_runtime_attestation_sha256
        )),
        rep(expected_runtime_attestation_sha256, 4L)
      ) ||
      !identical(
        as.character(summary$runtime_binding_basis), expected_binding
      ) ||
      !rqr_dlm_companion_integer_column(
        summary$artifact_count, 4L, minimum = 0
      ) ||
      !identical(as.numeric(summary$artifact_count), c(22, 5, 5, 7)) ||
      !rqr_dlm_companion_integer_column(
        summary$artifact_bytes, 4L, minimum = 0
      ) ||
      any(!vapply(
        as.character(summary$artifact_manifest_sha256),
        rqr_dlm_companion_is_hex, logical(1L)
      )) ||
      !identical(
        as.character(summary$semantic_status), rep("pass", 4L)
      )) {
    rqr_dlm_companion_fail(
      "Compact companion input summary is incomplete or inconsistent."
    )
  }

  input_artifacts <- rqr_dlm_companion_read_csv(
    file.path(directory, "input_artifact_hashes.csv"),
    c(
      "schema_version", "input_role", "relative_path", "byte_count",
      "sha256", "listed_in_source_manifest"
    ),
    "compact companion input_artifact_hashes.csv"
  )
  expected_role <- unlist(lapply(roles, function(role) {
    rep(role, length(rqr_dlm_companion_input_files(role)))
  }), use.names = FALSE)
  expected_relative <- unlist(lapply(roles, function(role) {
    sort(rqr_dlm_companion_input_files(role))
  }), use.names = FALSE)
  if (nrow(input_artifacts) != 39L ||
      !identical(
        as.character(input_artifacts$schema_version),
        rep(rqr_dlm_companion_schema(), 39L)
      ) ||
      !identical(as.character(input_artifacts$input_role), expected_role) ||
      !identical(
        as.character(input_artifacts$relative_path), expected_relative
      ) ||
      !rqr_dlm_companion_integer_column(
        input_artifacts$byte_count, 39L, minimum = 0
      ) ||
      any(!vapply(
        as.character(input_artifacts$sha256),
        rqr_dlm_companion_is_hex, logical(1L)
      )) ||
      !rqr_dlm_companion_logical_column(
        input_artifacts$listed_in_source_manifest, 39L
      ) ||
      !identical(
        input_artifacts$listed_in_source_manifest,
        input_artifacts$relative_path != "artifact_hashes.csv"
      )) {
    rqr_dlm_companion_fail(
      "Compact companion input-artifact ledger is incomplete or invalid."
    )
  }
  for (index in seq_along(roles)) {
    selected <- input_artifacts$input_role == roles[[index]]
    artifact_manifest_row <- selected &
      input_artifacts$relative_path == "artifact_hashes.csv"
    if (sum(selected) != summary$artifact_count[[index]] ||
        !identical(
          as.numeric(sum(input_artifacts$byte_count[selected])),
          as.numeric(summary$artifact_bytes[[index]])
        ) ||
        sum(artifact_manifest_row) != 1L ||
        !identical(
          input_artifacts$sha256[artifact_manifest_row],
          summary$artifact_manifest_sha256[[index]]
        )) {
      rqr_dlm_companion_fail(
        "Compact companion input ledger does not reconcile role ",
        roles[[index]], "."
      )
    }
  }

  semantic <- rqr_dlm_companion_read_csv(
    file.path(directory, "semantic_gates.csv"),
    c("schema_version", "gate_id", "status", "detail"),
    "compact companion semantic_gates.csv"
  )
  if (nrow(semantic) != 16L ||
      !identical(
        as.character(semantic$schema_version),
        rep(rqr_dlm_companion_schema(), 16L)
      ) ||
      !identical(
        as.character(semantic$gate_id),
        rqr_dlm_companion_semantic_gate_names()
      ) ||
      !identical(as.character(semantic$status), rep("pass", 16L)) ||
      anyNA(semantic$detail) || any(!nzchar(semantic$detail))) {
    rqr_dlm_companion_fail(
      "Compact companion semantic gate table is incomplete or failed."
    )
  }
  invisible(list(
    directory = directory, manifest = manifest,
    input_summary = summary, input_artifacts = input_artifacts,
    semantic_gates = semantic, artifact_hashes = output_hashes
  ))
}

rqr_dlm_companion_prepare_output <- function(
    repo_root, output_directory, require_ignored = TRUE) {
  if (!is.character(output_directory) ||
      length(output_directory) != 1L || is.na(output_directory) ||
      !nzchar(output_directory) ||
      file.exists(output_directory) || dir.exists(output_directory) ||
      any(rqr_dlm_companion_is_symlink(output_directory))) {
    rqr_dlm_companion_fail(
      "The companion output directory must be one new nonsymlink path."
    )
  }
  parent <- dirname(output_directory)
  if (!dir.exists(parent) || any(rqr_dlm_companion_is_symlink(parent))) {
    rqr_dlm_companion_fail(
      "The companion output parent must already exist and not be a symlink."
    )
  }
  parent <- normalizePath(parent, winslash = "/", mustWork = TRUE)
  output <- file.path(parent, basename(output_directory))
  if (isTRUE(require_ignored)) {
    git <- Sys.which("git")
    if (!nzchar(git)) {
      rqr_dlm_companion_fail(
        "Git is required to verify the ignored output boundary."
      )
    }
    status <- suppressWarnings(system2(
      git,
      c(
        "-C", shQuote(repo_root), "check-ignore", "--no-index", "-q",
        "--", shQuote(output)
      ),
      stdout = FALSE, stderr = FALSE
    ))
    if (!identical(as.integer(status), 0L)) {
      rqr_dlm_companion_fail(
        "The companion output path is not ignored by the repository."
      )
    }
  }
  output
}

rqr_dlm_companion_bundle <- function(
    repo_root, expected_commit, reference_directory, m01_directory,
    m02_directory, horizon_directory, output_directory,
    collector_source_commit = expected_commit,
    require_ignored_output = TRUE) {
  if (!rqr_dlm_companion_is_hex(expected_commit, 40L) ||
      !rqr_dlm_companion_is_hex(collector_source_commit, 40L)) {
    rqr_dlm_companion_fail(
      "Expected and collector commits must be complete SHA-1 values."
    )
  }
  expected_commit <- tolower(expected_commit)
  collector_source_commit <- tolower(collector_source_commit)
  directories <- c(
    dlm_reference = reference_directory, M01 = m01_directory,
    M02 = m02_directory, horizon_M03 = horizon_directory
  )
  normalized_inputs <- vapply(
    directories,
    normalizePath, character(1L), winslash = "/", mustWork = TRUE
  )
  if (anyDuplicated(normalized_inputs)) {
    rqr_dlm_companion_fail(
      "Each protected-DLM input role must use a distinct directory."
    )
  }
  output <- rqr_dlm_companion_prepare_output(
    repo_root, output_directory, require_ignored_output
  )
  if (any(startsWith(
        paste0(output, "/"), paste0(normalized_inputs, "/")
      ))) {
    rqr_dlm_companion_fail(
      "The companion output must not be nested inside an input directory."
    )
  }
  initial_input_ledger <- rqr_dlm_companion_capture_input_ledger(
    normalized_inputs
  )

  reference <- rqr_dlm_companion_validate_reference(
    reference_directory, expected_commit
  )
  m01 <- rqr_dlm_companion_validate_m01(
    m01_directory, expected_commit, reference
  )
  m02 <- rqr_dlm_companion_validate_m02(
    m02_directory, expected_commit, reference
  )
  horizon <- rqr_dlm_companion_validate_horizon(
    horizon_directory, expected_commit, reference
  )
  inputs <- list(
    dlm_reference = reference, M01 = m01, M02 = m02,
    horizon_M03 = horizon
  )
  rqr_dlm_companion_cross_validate(inputs, expected_commit)
  rqr_dlm_companion_assert_input_closure(inputs)
  rqr_dlm_companion_assert_input_ledger(inputs, initial_input_ledger)

  semantic_gates <- data.frame(
    schema_version = rqr_dlm_companion_schema(),
    gate_id = rqr_dlm_companion_semantic_gate_names(),
    status = "pass",
    detail = c(
      expected_commit,
      paste(
        reference$runtime_tree_digest,
        reference$runtime_attestation_sha256, sep = "|"
      ),
      "22 exact regular artifacts; recursive manifest verified",
      "dense conditional, full cross-time covariance, and R/C++ parity",
      "canonical missingness and public future-root checks",
      "inverse-Gamma conditionals, orientation, and future scales",
      "3 fixtures x 2 rate modes; every saved field and checkpoint",
      "27 rehashed raw/semantic mutations rejected",
      "wrapper, resource, fault, final-PGID, and zero-failure gates",
      paste0(m01$semantic_counts[["chain_jobs"]], " exact M01 chains"),
      "centered inverse-Gamma plus noncentered slice interweaving",
      paste0(m02$semantic_counts[["endpoint_fits"]], " M02 endpoint fits"),
      paste0(horizon$semantic_counts[["horizon_scenarios"]], " scenarios"),
      paste0(horizon$semantic_counts[["M03_tasks"]], " M03 standard tasks"),
      "one exact M01 training/future endpoint check",
      m01$config_digest
    ),
    stringsAsFactors = FALSE
  )
  input_summary <- do.call(rbind, lapply(inputs, function(input) {
    files <- rqr_dlm_companion_input_files(input$role)
    paths <- file.path(input$directory, files)
    data.frame(
      schema_version = rqr_dlm_companion_schema(),
      input_role = input$role,
      input_schema = input$schema_version,
      source_commit = input$source_commit,
      package_version = input$package_version,
      primary_runtime_tree_digest = input$runtime_tree_digest,
      primary_runtime_attestation_sha256 =
        input$runtime_attestation_sha256,
      runtime_binding_basis = if (identical(
        input$role, "dlm_reference"
      )) {
        "direct_runtime_tree_and_attestation"
      } else if (identical(input$role, "M01")) {
        "direct_tree_plus_matching_reference_attestation"
      } else {
        "matching_reference_attestation"
      },
      artifact_count = length(files),
      artifact_bytes = sum(as.numeric(file.info(paths)$size)),
      artifact_manifest_sha256 = rqr_dlm_companion_sha256(
        file.path(input$directory, "artifact_hashes.csv")
      ),
      semantic_status = "pass",
      stringsAsFactors = FALSE
    )
  }))
  input_artifacts <- data.frame(
    schema_version = rqr_dlm_companion_schema(),
    initial_input_ledger,
    listed_in_source_manifest =
      initial_input_ledger$relative_path != "artifact_hashes.csv",
    stringsAsFactors = FALSE
  )
  manifest <- list(
    schema_version = rqr_dlm_companion_schema(),
    expected_primary_commit = expected_commit,
    collector_source_commit = collector_source_commit,
    primary_runtime_tree_digest = reference$runtime_tree_digest,
    primary_runtime_attestation_sha256 =
      reference$runtime_attestation_sha256,
    package_version = reference$package_version,
    reference_config_digest = reference$config_digest,
    confirmatory_config_digest = m01$config_digest,
    confirmatory_incidence_digest = m01$incidence_digest,
    confirmatory_seed_ledger_digest = m01$seed_ledger_digest,
    input_roles = names(inputs),
    input_artifact_count = nrow(input_artifacts),
    semantic_gate_count = nrow(semantic_gates),
    semantic_gate_pass_count = sum(semantic_gates$status == "pass"),
    all_semantic_gates_passed = TRUE,
    fits_executed_by_collector = FALSE,
    heavy_input_artifacts_copied = FALSE,
    generalized_bayes = TRUE,
    response_likelihood = FALSE,
    response_prediction_contract = FALSE,
    created_at_utc = format(
      Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"
    )
  )

  rqr_dlm_companion_assert_input_closure(inputs)
  rqr_dlm_companion_assert_input_ledger(inputs, initial_input_ledger)
  staging <- tempfile(
    ".protected-dlm-companion-", tmpdir = dirname(output)
  )
  if (!dir.create(staging, recursive = FALSE, showWarnings = FALSE)) {
    rqr_dlm_companion_fail(
      "Could not create the companion staging directory."
    )
  }
  on.exit(unlink(staging, recursive = TRUE, force = TRUE), add = TRUE)
  rqr_dlm_companion_atomic_csv(
    input_summary, file.path(staging, "input_bundle_summary.csv")
  )
  rqr_dlm_companion_atomic_csv(
    input_artifacts, file.path(staging, "input_artifact_hashes.csv")
  )
  rqr_dlm_companion_atomic_csv(
    semantic_gates, file.path(staging, "semantic_gates.csv")
  )
  rqr_dlm_companion_atomic_json(
    manifest, file.path(staging, "bundle_manifest.json")
  )
  output_hashes <- rqr_dlm_companion_recursive_manifest(staging)
  rqr_dlm_companion_atomic_csv(
    output_hashes, file.path(staging, "artifact_hashes.csv")
  )
  rqr_dlm_companion_validate_compact(
    directory = staging,
    expected_commit = expected_commit,
    expected_runtime_tree_digest = reference$runtime_tree_digest,
    expected_runtime_attestation_sha256 =
      reference$runtime_attestation_sha256,
    expected_package_version = reference$package_version,
    expected_collector_commit = collector_source_commit
  )
  rqr_dlm_companion_assert_input_closure(inputs)
  rqr_dlm_companion_assert_input_ledger(inputs, initial_input_ledger)
  expected_output_files <- rqr_dlm_companion_compact_files()
  if (!identical(sort(list.files(staging)), expected_output_files) ||
      !file.rename(staging, output)) {
    rqr_dlm_companion_fail(
      "Atomic companion-directory publication failed."
    )
  }
  invisible(list(
    output_directory = output, manifest = manifest,
    input_summary = input_summary, semantic_gates = semantic_gates
  ))
}

rqr_dlm_companion_git_value <- function(repo_root, arguments) {
  git <- Sys.which("git")
  if (!nzchar(git)) rqr_dlm_companion_fail("Git is required.")
  output <- system2(
    git, c("-C", shQuote(repo_root), arguments),
    stdout = TRUE, stderr = TRUE,
    env = c("GIT_OPTIONAL_LOCKS=0", "GIT_TERMINAL_PROMPT=0")
  )
  status <- attr(output, "status") %||% 0L
  if (!identical(as.integer(status), 0L)) {
    rqr_dlm_companion_fail("Could not read the collector Git state.")
  }
  paste(output, collapse = "\n")
}

rqr_dlm_companion_validate_repository <- function(
    repo_root, expected_commit) {
  actual <- tolower(rqr_dlm_companion_git_value(
    repo_root, c("rev-parse", "HEAD")
  ))
  branch <- rqr_dlm_companion_git_value(
    repo_root, c("rev-parse", "--abbrev-ref", "HEAD")
  )
  status <- rqr_dlm_companion_git_value(
    repo_root, c("status", "--porcelain=v2", "--untracked-files=all")
  )
  if (!identical(actual, expected_commit) ||
      !identical(branch, "main") || nzchar(status)) {
    rqr_dlm_companion_fail(
      "Companion collection requires clean main at the exact expected SHA."
    )
  }
  actual
}

rqr_dlm_companion_main <- function(
    arguments = commandArgs(trailingOnly = TRUE)) {
  if (length(arguments) != 6L) {
    rqr_dlm_companion_fail(
      paste(
        "Usage: 30_bundle_rqr_ordinary_v1_protected_dlm_evidence.R",
        "<expected-commit> <reference-dir> <M01-dir> <M02-dir>",
        "<horizon-M03-dir> <new-ignored-output-dir>"
      )
    )
  }
  repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  if (!file.exists(file.path(repo_root, "application", "DESCRIPTION"))) {
    rqr_dlm_companion_fail(
      "Run the protected-DLM companion collector from the repository root."
    )
  }
  expected_commit <- tolower(arguments[[1L]])
  rqr_dlm_companion_validate_repository(repo_root, expected_commit)
  result <- rqr_dlm_companion_bundle(
    repo_root = repo_root,
    expected_commit = expected_commit,
    reference_directory = arguments[[2L]],
    m01_directory = arguments[[3L]],
    m02_directory = arguments[[4L]],
    horizon_directory = arguments[[5L]],
    output_directory = arguments[[6L]],
    collector_source_commit = expected_commit,
    require_ignored_output = TRUE
  )
  message(
    "Protected-DLM companion evidence PASS: ",
    result$output_directory
  )
  invisible(result$output_directory)
}

if (!identical(
      Sys.getenv("RQR_DLM_COMPANION_SOURCE_ONLY", unset = ""), "YES"
    )) {
  rqr_dlm_companion_main()
}
