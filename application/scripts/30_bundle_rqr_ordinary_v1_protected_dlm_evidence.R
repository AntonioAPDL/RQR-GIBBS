#!/usr/bin/env Rscript

# Bind the protected RQR-DLM reference and correction evidence required by the
# ordinary-RQR version-1 release gate.  This collector does not fit a model,
# load a chain object, or rerun any source calculation.  It validates seven
# already completed, exact-commit input directories and publishes only compact
# semantic summaries and hashes below an ignored output root.

`%||%` <- function(x, y) if (is.null(x)) y else x

rqr_dlm_companion_schema <- function() {
  "rqrgibbs_ordinary_v1_protected_dlm_companion/2.1.0"
}

rqr_dlm_companion_roles <- function() {
  c(
    "dlm_reference",
    "wave1_M01_static_gaussian",
    "wave1_M02_static_gaussian",
    "wave2_M01_local_level",
    "wave2_M02_local_level",
    "horizon_M03",
    "resource_envelope"
  )
}

rqr_dlm_companion_wave_spec <- function(role) {
  values <- list(
    wave1_M01_static_gaussian = list(
      kind = "M01", tag = "wave1",
      wave_id = "static_gaussian_T200__target0200__sentinel",
      DGP = c("S01", "S02"), task_count = 20L, chain_count = 44L,
      endpoint_count = NA_integer_, sentinel_count = 8L
    ),
    wave1_M02_static_gaussian = list(
      kind = "M02", tag = "wave1",
      wave_id = "static_gaussian_T200__target0200__sentinel",
      DGP = c("S01", "S02"), task_count = 20L, chain_count = 44L,
      endpoint_count = 88L, sentinel_count = 8L
    ),
    wave2_M01_local_level = list(
      kind = "M01", tag = "wave2",
      wave_id = "local_level_gaussian_T200__target0200__sentinel",
      DGP = c("S03", "S04"), task_count = 25L, chain_count = 49L,
      endpoint_count = NA_integer_, sentinel_count = 8L
    ),
    wave2_M02_local_level = list(
      kind = "M02", tag = "wave2",
      wave_id = "local_level_gaussian_T200__target0200__sentinel",
      DGP = c("S03", "S04"), task_count = 25L, chain_count = 49L,
      endpoint_count = 98L, sentinel_count = 8L
    )
  )
  value <- values[[role]]
  if (is.null(value)) {
    rqr_dlm_companion_fail("Unknown companion wave role: ", role)
  }
  value
}

rqr_dlm_companion_protected_source_paths <- function() {
  c(
    "application/R/rqr_dlm_fit.R",
    "application/R/rqr_dlm_model.R",
    "application/R/rqr_evolution.R",
    "application/R/rqr_ffbs.R",
    "application/R/rqr_utils.R",
    "application/R/rqr_numerics.R",
    "application/src/rqr_ffbs.cpp",
    "application/config/rqr_dlm/rqr_dlm_bounded_dynamic_fixtures_20260723.R",
    "application/config/rqr_dlm/rqr_dlm_main_simulation_20260724.R",
    "application/config/rqr_dlm/rqr_dlm_main_simulation_preliminary_20260724.R",
    paste0(
      "application/config/rqr_dlm/",
      "rqr_dlm_main_simulation_preliminary_methods_20260724.csv"
    ),
    paste0(
      "application/config/rqr_dlm/",
      "rqr_dlm_main_simulation_preliminary_scenarios_20260724.csv"
    ),
    paste0(
      "application/config/rqr_dlm/",
      "rqr_dlm_output13_bounded_expected_bundle_20260724.json"
    ),
    "application/scripts/15_run_rqr_dlm_confirmatory_simulation.R",
    "application/scripts/15_run_rqr_dlm_confirmatory_simulation.sh",
    "application/scripts/17_launch_rqr_dlm_confirmatory_wave.R",
    "application/DESCRIPTION",
    "application/NAMESPACE",
    "application/R/RcppExports.R",
    "application/src/RcppExports.cpp",
    "application/src/rqr_interweave.cpp",
    "application/src/Makevars",
    "application/src/Makevars.win",
    "application/scripts/22_validate_rqr_dlm_wave1_corrections.R",
    "application/scripts/23_validate_rqr_dlm_wave1_comparator_projection.R",
    "application/scripts/24_validate_rqr_dlm_horizon_and_fixed_design.R",
    "application/scripts/25_validate_rqr_dlm_resource_envelope.R",
    "application/scripts/lib/rqr_dlm_confirmatory_simulation.R",
    "docs/audits/rqr_dlm_main_correction_budget_20260727.csv"
  )
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

rqr_dlm_companion_protected_source_inventory <- function(repo_root) {
  paths <- rqr_dlm_companion_protected_source_paths()
  if (length(paths) != 29L || anyDuplicated(paths) ||
      !identical(paths, unique(paths))) {
    rqr_dlm_companion_fail(
      "The protected source inventory is not the reviewed 29-file set."
    )
  }
  absolute <- file.path(repo_root, paths)
  if (any(!file.exists(absolute)) ||
      any(!utils::file_test("-f", absolute)) ||
      any(rqr_dlm_companion_is_symlink(absolute))) {
    rqr_dlm_companion_fail(
      "A protected DLM source/config/helper file is absent or nonregular."
    )
  }
  value <- data.frame(
    relative_path = paths,
    byte_count = as.numeric(file.info(absolute)$size),
    sha256 = vapply(
      absolute, rqr_dlm_companion_sha256, character(1L)
    ),
    stringsAsFactors = FALSE
  )
  list(
    files = value,
    digest = digest::digest(value, algo = "sha256", serialize = TRUE)
  )
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
      na.strings = "NA", numerals = "no.loss"
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
  if (role %in% c(
      "wave1_M01_static_gaussian", "wave2_M01_local_level"
    )) {
    tag <- rqr_dlm_companion_wave_spec(role)$tag
    return(c(
      "artifact_hashes.csv", "validation_manifest.json",
      paste0(tag, "_M01_chain_evidence.rds"),
      paste0(tag, "_M01_diagnostics.csv"),
      paste0(tag, "_M01_summary.csv")
    ))
  }
  if (role %in% c(
      "wave1_M02_static_gaussian", "wave2_M02_local_level"
    )) {
    tag <- rqr_dlm_companion_wave_spec(role)$tag
    return(c(
      "artifact_hashes.csv", "validation_manifest.json",
      paste0(tag, "_M02_chain_evidence.rds"),
      paste0(tag, "_M02_diagnostics.csv"),
      paste0(tag, "_M02_summary.csv")
    ))
  }
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
    horizon_M03 = c(
      "artifact_hashes.csv", "dynamic_endpoint_check.csv",
      "fixed_design_chain_evidence.rds",
      "fixed_design_diagnostics.csv", "fixed_design_summary.csv",
      "horizon_checks.csv", "validation_manifest.json"
    ),
    resource_envelope = c(
      "artifact_hashes.csv", "fit_shape_contract.csv",
      "resource_closeout.csv", "resource_envelope.csv",
      "toolchain_manifest.csv", "validation_manifest.json"
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

rqr_dlm_companion_normalize_reference_toolchain <- function(runtime) {
  scalar <- function(value, label, null_value = NULL) {
    if (is.null(value) && !is.null(null_value)) {
      return(null_value)
    }
    if (!is.character(value) || length(value) != 1L ||
        is.na(value) || !nzchar(value)) {
      rqr_dlm_companion_fail(
        "DLM reference runtime toolchain has invalid ", label, "."
      )
    }
    value
  }
  dependencies <- c("digest", "jsonlite", "posterior", "rqrgibbs")
  if (!is.list(runtime$dependency_versions) ||
      !identical(names(runtime$dependency_versions), dependencies)) {
    rqr_dlm_companion_fail(
      "DLM reference dependency-version contract changed."
    )
  }
  dependency_versions <- setNames(
    lapply(dependencies, function(package) {
      scalar(
        runtime$dependency_versions[[package]],
        paste0("dependency_versions$", package)
      )
    }),
    dependencies
  )
  list(
    R_version = scalar(runtime$R_version, "R_version"),
    platform = scalar(runtime$platform, "platform"),
    compiler = scalar(
      runtime$compiler, "compiler", null_value = "unavailable"
    ),
    BLAS = scalar(runtime$BLAS, "BLAS"),
    LAPACK = scalar(runtime$LAPACK, "LAPACK"),
    dependency_versions = dependency_versions
  )
}

rqr_dlm_companion_normalize_resource_toolchain <- function(toolchain) {
  required <- c(
    "R_version", "platform", "R_compiler", "BLAS", "LAPACK",
    "package_digest", "package_jsonlite", "package_posterior",
    "package_rqrgibbs"
  )
  if (anyDuplicated(toolchain$key) ||
      any(!required %in% toolchain$key)) {
    rqr_dlm_companion_fail(
      "Resource-envelope toolchain keys are missing or duplicated."
    )
  }
  value <- function(key) {
    observed <- as.character(toolchain$value[toolchain$key == key])
    if (length(observed) != 1L || is.na(observed) ||
        !nzchar(observed)) {
      rqr_dlm_companion_fail(
        "Resource-envelope toolchain value is invalid: ", key
      )
    }
    observed
  }
  dependencies <- c("digest", "jsonlite", "posterior", "rqrgibbs")
  list(
    R_version = value("R_version"),
    platform = value("platform"),
    compiler = value("R_compiler"),
    BLAS = value("BLAS"),
    LAPACK = value("LAPACK"),
    dependency_versions = setNames(
      lapply(dependencies, function(package) {
        value(paste0("package_", package))
      }),
      dependencies
    )
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
  runtime_toolchain_facts <-
    rqr_dlm_companion_normalize_reference_toolchain(runtime)
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
    runtime_toolchain_digest = run$runtime_toolchain_digest,
    runtime_toolchain_facts = runtime_toolchain_facts,
    semantic_counts = c(
      reference_gates = nrow(gates), continuation_cells = nrow(continuation),
      history_mutations = nrow(mutations)
    )
  )
}

rqr_dlm_companion_transition_kernel_fields <- function() {
  c(
    "schema_version", "evolution_mode", "learning_rate_mode",
    "time0_state_completion", "one_root_partially_collapsed",
    "collapsed_integrated_root", "collapsed_conditioned_root",
    "collapsed_log_q_coordinate_order", "scan_order",
    "collapsed_slice_width", "collapsed_slice_sweeps",
    "collapsed_slice_max_steps", "collapsed_slice_max_shrink",
    "centered_inverse_gamma", "noncentered_slice_interweave",
    "interweave_cycles", "interweave_slice_width",
    "interweave_slice_sweeps_per_cycle",
    "interweave_slice_max_steps", "interweave_slice_max_shrink",
    "global_root_swap_probability", "target_change"
  )
}

rqr_dlm_companion_expected_m01_transition_kernel <- function(role) {
  component_name <- switch(
    role,
    wave1_M01_static_gaussian = "regression",
    wave2_M01_local_level = "level",
    rqr_dlm_companion_fail(
      "No frozen M01 transition contract exists for role: ", role
    )
  )
  list(
    schema_version = "rqrgibbs_dlm_transition_kernel/1.0.0",
    evolution_mode = "component_scale",
    learning_rate_mode = "fixed_rate",
    time0_state_completion = TRUE,
    one_root_partially_collapsed = TRUE,
    collapsed_integrated_root = "root1",
    collapsed_conditioned_root = "root2",
    collapsed_log_q_coordinate_order = component_name,
    scan_order = c(
      "lambda_fixed", "latent_v_refresh",
      "component_scale_root1_collapsed", "root1_ffbs",
      "root1_time0", "root2_ffbs", "root2_time0",
      "component_scale_centered_noncentered_cycles_1",
      "global_root_swap"
    ),
    collapsed_slice_width = 1,
    collapsed_slice_sweeps = 3L,
    collapsed_slice_max_steps = 100L,
    collapsed_slice_max_shrink = 1000L,
    centered_inverse_gamma = TRUE,
    noncentered_slice_interweave = TRUE,
    interweave_cycles = 1L,
    interweave_slice_width = 1,
    interweave_slice_sweeps_per_cycle = 3L,
    interweave_slice_max_steps = 100L,
    interweave_slice_max_shrink = 1000L,
    global_root_swap_probability = 0.5,
    target_change = FALSE
  )
}

rqr_dlm_companion_normalize_transition_kernel <- function(value, label) {
  fields <- rqr_dlm_companion_transition_kernel_fields()
  if (!is.list(value) || !identical(names(value), fields)) {
    rqr_dlm_companion_fail(label, " has an incomplete transition contract.")
  }
  strings <- c(
    "schema_version", "evolution_mode", "learning_rate_mode",
    "collapsed_integrated_root", "collapsed_conditioned_root"
  )
  if (any(!vapply(value[strings], function(x) {
      is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
    }, logical(1L)))) {
    rqr_dlm_companion_fail(label, " has malformed transition strings.")
  }
  coordinate_order <- unname(unlist(
    value$collapsed_log_q_coordinate_order, use.names = FALSE
  ))
  scan_order <- unname(unlist(value$scan_order, use.names = FALSE))
  if (!is.character(coordinate_order) || anyNA(coordinate_order) ||
      any(!nzchar(coordinate_order)) || anyDuplicated(coordinate_order) ||
      !is.character(scan_order) || !length(scan_order) ||
      anyNA(scan_order) || any(!nzchar(scan_order))) {
    rqr_dlm_companion_fail(label, " has malformed transition ordering.")
  }
  logicals <- c(
    "time0_state_completion", "one_root_partially_collapsed",
    "centered_inverse_gamma", "noncentered_slice_interweave",
    "target_change"
  )
  if (any(!vapply(value[logicals], function(x) {
      is.logical(x) && length(x) == 1L && !is.na(x)
    }, logical(1L)))) {
    rqr_dlm_companion_fail(label, " has malformed transition flags.")
  }
  integer_fields <- c(
    "collapsed_slice_sweeps", "collapsed_slice_max_steps",
    "collapsed_slice_max_shrink", "interweave_cycles",
    "interweave_slice_sweeps_per_cycle",
    "interweave_slice_max_steps", "interweave_slice_max_shrink"
  )
  if (any(!vapply(
      value[integer_fields],
      rqr_dlm_companion_is_integerish, logical(1L), minimum = 0
    ))) {
    rqr_dlm_companion_fail(label, " has malformed transition counts.")
  }
  numeric_fields <- c(
    "collapsed_slice_width", "interweave_slice_width",
    "global_root_swap_probability"
  )
  if (any(!vapply(value[numeric_fields], function(x) {
      is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x)
    }, logical(1L)))) {
    rqr_dlm_companion_fail(label, " has malformed transition scalars.")
  }
  list(
    schema_version = value$schema_version,
    evolution_mode = value$evolution_mode,
    learning_rate_mode = value$learning_rate_mode,
    time0_state_completion = value$time0_state_completion,
    one_root_partially_collapsed =
      value$one_root_partially_collapsed,
    collapsed_integrated_root = value$collapsed_integrated_root,
    collapsed_conditioned_root = value$collapsed_conditioned_root,
    collapsed_log_q_coordinate_order = coordinate_order,
    scan_order = scan_order,
    collapsed_slice_width = as.numeric(value$collapsed_slice_width),
    collapsed_slice_sweeps =
      as.integer(value$collapsed_slice_sweeps),
    collapsed_slice_max_steps =
      as.integer(value$collapsed_slice_max_steps),
    collapsed_slice_max_shrink =
      as.integer(value$collapsed_slice_max_shrink),
    centered_inverse_gamma = value$centered_inverse_gamma,
    noncentered_slice_interweave =
      value$noncentered_slice_interweave,
    interweave_cycles = as.integer(value$interweave_cycles),
    interweave_slice_width =
      as.numeric(value$interweave_slice_width),
    interweave_slice_sweeps_per_cycle =
      as.integer(value$interweave_slice_sweeps_per_cycle),
    interweave_slice_max_steps =
      as.integer(value$interweave_slice_max_steps),
    interweave_slice_max_shrink =
      as.integer(value$interweave_slice_max_shrink),
    global_root_swap_probability =
      as.numeric(value$global_root_swap_probability),
    target_change = value$target_change
  )
}

rqr_dlm_companion_transition_kernel_invariant <- function(contract) {
  list(
    schema_version =
      "rqrgibbs_dlm_transition_kernel_invariant/1.0.0",
    transition_kernel = contract[
      setdiff(
        names(contract), "collapsed_log_q_coordinate_order"
      )
    ]
  )
}

rqr_dlm_companion_normalize_transition_invariant <- function(
    value, expected_contract, label) {
  expected_fields <- setdiff(
    rqr_dlm_companion_transition_kernel_fields(),
    "collapsed_log_q_coordinate_order"
  )
  if (!is.list(value) ||
      !identical(
        names(value), c("schema_version", "transition_kernel")
      ) ||
      !identical(
        value$schema_version,
        "rqrgibbs_dlm_transition_kernel_invariant/1.0.0"
      ) ||
      !is.list(value$transition_kernel) ||
      !identical(names(value$transition_kernel), expected_fields)) {
    rqr_dlm_companion_fail(label, " has a malformed kernel invariant.")
  }
  complete <- setNames(
    lapply(rqr_dlm_companion_transition_kernel_fields(), function(field) {
      if (identical(field, "collapsed_log_q_coordinate_order")) {
        expected_contract[[field]]
      } else {
        value$transition_kernel[[field]]
      }
    }),
    rqr_dlm_companion_transition_kernel_fields()
  )
  normalized <- rqr_dlm_companion_normalize_transition_kernel(
    complete, paste0(label, " transition_kernel")
  )
  rqr_dlm_companion_transition_kernel_invariant(normalized)
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
    "transition_kernel_schema", "unique_transition_kernel_digests",
    "expected_transition_kernel_contract",
    "expected_transition_kernel_contract_digest",
    "transition_kernel_invariant_schema",
    "expected_transition_kernel_invariant",
    "expected_transition_kernel_invariant_digest",
    "all_fit_transition_contracts_complete",
    "all_fit_transition_contracts_match_expected",
    "comparative_simulation_metrics_used", "failed_outputs_reused",
    "all_fits_succeeded", "all_fits_reproducibility_eligible",
    "unique_runtime_tree_digests", "total_fit_elapsed_seconds",
    "maximum_process_peak_RSS_KiB",
    "declared_worker_memory_ceiling_KiB", "resource_margin_pass",
    "all_diagnostics_passed", "started_at_utc", "completed_at_utc"
  )
}

rqr_dlm_companion_m02_manifest_fields <- function() {
  c(
    "schema_version", "source_commit", "source_clean",
    "package_version", "primary_runtime_attestation_sha256",
    "primary_reproducibility_eligible",
    "primary_runtime_tree_digest",
    "exdqlm_runtime_attestation_sha256",
    "exdqlm_runtime_tree_digest", "exdqlm_source_package_sha256",
    "config_digest", "incidence_digest", "seed_ledger_digest",
    "wave_id", "wave_task_count", "interval_chain_job_count",
    "logical_endpoint_fit_count", "workers", "thread_environment",
    "comparator_projection",
    "common_target_across_initialization_profiles",
    "overdispersed_initialization_profiles_verified",
    "frozen_schedules", "applied_schedule_evidence",
    "all_applied_schedules_verified", "schedule_evidence_fields",
    "initialization_contract", "target_fields_held_fixed",
    "comparative_simulation_metrics_used",
    "failed_outputs_reused", "all_fits_succeeded",
    "all_diagnostics_passed", "total_fit_elapsed_seconds",
    "maximum_process_peak_RSS_KiB",
    "declared_worker_memory_ceiling_KiB", "resource_margin_pass",
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

rqr_dlm_companion_resource_manifest_fields <- function() {
  c(
    "schema_version", "source_commit", "source_clean",
    "package_version", "expected_commit", "config_digest",
    "incidence_digest", "maximum_seed_ledger_digest",
    "primary_runtime_attestation_sha256",
    "primary_runtime_tree_digest", "primary_runtime_source_match",
    "primary_reproducibility_eligible",
    "exact_commit_attestation_pair_verified",
    "promotion_evidence_eligible", "development_execution",
    "configured_workers", "configured_sentinel_workers",
    "resource_gate_worker_processes", "modeled_chains_per_worker",
    "thread_environment", "toolchain_manifest_digest",
    "toolchain_manifest_complete", "scientific_metrics_used",
    "response_prediction_contract", "fit_shape_contract",
    "synthetic_heavy_objects_retained", "writer_measurement_process",
    "reader_measurement_process", "distinct_clean_processes_verified",
    "telemetry_complete",
    "maximum_writer_or_clean_reader_peak_RSS_KiB",
    "declared_worker_memory_ceiling_KiB",
    "required_margin_fraction", "resource_margin_pass",
    "all_writer_shapes_valid", "all_clean_deserializations_valid",
    "completed_at_utc"
  )
}

rqr_dlm_companion_validate_resource <- function(
    directory, expected_commit, reference) {
  role <- "resource_envelope"
  directory <- rqr_dlm_companion_validate_directory(directory, role)
  artifacts <- rqr_dlm_companion_verify_artifact_manifest(
    directory, role, c("path", "bytes", "sha256")
  )
  manifest <- rqr_dlm_companion_read_json(
    file.path(directory, "validation_manifest.json"),
    rqr_dlm_companion_resource_manifest_fields(),
    "resource-envelope validation_manifest.json"
  )
  rqr_dlm_companion_assert_thread_environment(
    manifest$thread_environment, "resource envelope"
  )
  digest_fields <- c(
    "config_digest", "incidence_digest", "maximum_seed_ledger_digest",
    "primary_runtime_attestation_sha256",
    "primary_runtime_tree_digest", "toolchain_manifest_digest"
  )
  if (!identical(
        manifest$schema_version,
        "rqrgibbs_dlm_resource_envelope_validation/2.0.0"
      ) ||
      !identical(tolower(manifest$source_commit), expected_commit) ||
      !identical(tolower(manifest$expected_commit), expected_commit) ||
      !rqr_dlm_companion_scalar_true(manifest$source_clean) ||
      !identical(manifest$package_version, reference$package_version) ||
      any(!vapply(
        manifest[digest_fields],
        rqr_dlm_companion_is_hex, logical(1L)
      )) ||
      !identical(
        manifest$primary_runtime_attestation_sha256,
        reference$runtime_attestation_sha256
      ) ||
      !identical(
        manifest$primary_runtime_tree_digest,
        reference$runtime_tree_digest
      ) ||
      !rqr_dlm_companion_scalar_true(
        manifest$primary_runtime_source_match
      ) ||
      !rqr_dlm_companion_scalar_true(
        manifest$primary_reproducibility_eligible
      ) ||
      !identical(
        manifest$primary_runtime_tree_digest,
        reference$runtime_tree_digest
      ) ||
      !rqr_dlm_companion_scalar_true(
        manifest$exact_commit_attestation_pair_verified
      ) ||
      !rqr_dlm_companion_scalar_true(
        manifest$promotion_evidence_eligible
      ) ||
      !rqr_dlm_companion_scalar_false(manifest$development_execution) ||
      !rqr_dlm_companion_is_integerish(
        manifest$configured_workers, 1, 64
      ) ||
      !rqr_dlm_companion_is_integerish(
        manifest$configured_sentinel_workers, 1, 64
      ) ||
      !rqr_dlm_companion_is_integerish(
        manifest$resource_gate_worker_processes, 1, 1
      ) ||
      !identical(
        unname(as.numeric(unlist(
          manifest$modeled_chains_per_worker, use.names = FALSE
        ))),
        4
      ) ||
      !rqr_dlm_companion_scalar_true(
        manifest$toolchain_manifest_complete
      ) ||
      !rqr_dlm_companion_scalar_false(manifest$scientific_metrics_used) ||
      !rqr_dlm_companion_scalar_false(
        manifest$response_prediction_contract
      ) ||
      !identical(
        manifest$fit_shape_contract,
        "p_by_T_by_draw;T_by_draw;p_by_draw;draw_by_component"
      ) ||
      !rqr_dlm_companion_scalar_false(
        manifest$synthetic_heavy_objects_retained
      ) ||
      !identical(manifest$writer_measurement_process, "fresh_Rscript") ||
      !identical(manifest$reader_measurement_process, "fresh_Rscript") ||
      !rqr_dlm_companion_scalar_true(
        manifest$distinct_clean_processes_verified
      ) ||
      !rqr_dlm_companion_scalar_true(manifest$telemetry_complete) ||
      !rqr_dlm_companion_finite_column(
        manifest$maximum_writer_or_clean_reader_peak_RSS_KiB,
        1L, minimum = 1
      ) ||
      !rqr_dlm_companion_finite_column(
        manifest$declared_worker_memory_ceiling_KiB, 1L, minimum = 1
      ) ||
      !identical(as.numeric(manifest$required_margin_fraction), 0.8) ||
      manifest$maximum_writer_or_clean_reader_peak_RSS_KiB >
        manifest$required_margin_fraction *
          manifest$declared_worker_memory_ceiling_KiB ||
      !rqr_dlm_companion_scalar_true(manifest$resource_margin_pass) ||
      !rqr_dlm_companion_scalar_true(
        manifest$all_writer_shapes_valid
      ) ||
      !rqr_dlm_companion_scalar_true(
        manifest$all_clean_deserializations_valid
      )) {
    rqr_dlm_companion_fail(
      "Resource-envelope source/runtime/telemetry gate failed."
    )
  }

  shape <- rqr_dlm_companion_read_csv(
    file.path(directory, "fit_shape_contract.csv"),
    c("field", "orientation", "dimension_formula"),
    "resource-envelope fit_shape_contract.csv"
  )
  expected_shape <- data.frame(
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
  if (!identical(shape, expected_shape)) {
    rqr_dlm_companion_fail("The resource fit-shape contract changed.")
  }

  envelope <- rqr_dlm_companion_read_csv(
    file.path(directory, "resource_envelope.csv"),
    c(
      "case", "state_dimension", "training_horizon", "retained_draws",
      "component_count", "chains", "writer_peak_RSS_KiB",
      "clean_reader_peak_RSS_KiB", "bytes", "sha256",
      "writer_shape_valid", "clean_deserialization_valid",
      "declared_worker_memory_ceiling_KiB",
      "eighty_percent_margin_KiB", "serialization_margin_pass"
    ),
    "resource-envelope resource_envelope.csv"
  )
  if (nrow(envelope) != 3L ||
      !identical(
        as.character(envelope$case),
        c(
          "four_state_component_scale",
          "three_state_learned_component_scale",
          "long_horizon_single_state"
        )
      ) ||
      !identical(as.numeric(envelope$state_dimension), c(4, 3, 1)) ||
      !identical(as.numeric(envelope$training_horizon), c(200, 200, 400)) ||
      !identical(as.numeric(envelope$retained_draws), c(6000, 9000, 6000)) ||
      !identical(as.numeric(envelope$component_count), c(2, 2, 1)) ||
      !identical(as.numeric(envelope$chains), rep(4, 3)) ||
      !rqr_dlm_companion_finite_column(
        envelope$writer_peak_RSS_KiB, 3L, minimum = 1
      ) ||
      !rqr_dlm_companion_finite_column(
        envelope$clean_reader_peak_RSS_KiB, 3L, minimum = 1
      ) ||
      !rqr_dlm_companion_integer_column(envelope$bytes, 3L, minimum = 1) ||
      any(!vapply(
        as.character(envelope$sha256),
        rqr_dlm_companion_is_hex, logical(1L)
      )) ||
      !rqr_dlm_companion_logical_column(
        envelope$writer_shape_valid, 3L
      ) ||
      !all(envelope$writer_shape_valid) ||
      !rqr_dlm_companion_logical_column(
        envelope$clean_deserialization_valid, 3L
      ) ||
      !all(envelope$clean_deserialization_valid) ||
      any(envelope$declared_worker_memory_ceiling_KiB !=
            manifest$declared_worker_memory_ceiling_KiB) ||
      any(envelope$eighty_percent_margin_KiB !=
            0.8 * manifest$declared_worker_memory_ceiling_KiB) ||
      !rqr_dlm_companion_logical_column(
        envelope$serialization_margin_pass, 3L
      ) ||
      !all(envelope$serialization_margin_pass) ||
      !identical(
        max(c(
          envelope$writer_peak_RSS_KiB,
          envelope$clean_reader_peak_RSS_KiB
        )),
        as.numeric(
          manifest$maximum_writer_or_clean_reader_peak_RSS_KiB
        )
      )) {
    rqr_dlm_companion_fail(
      "Resource-envelope shape, deserialization, or margin evidence failed."
    )
  }

  toolchain <- rqr_dlm_companion_read_csv(
    file.path(directory, "toolchain_manifest.csv"),
    c("key", "value"), "resource-envelope toolchain_manifest.csv"
  )
  if (nrow(toolchain) < 10L || anyNA(toolchain) ||
      any(!nzchar(toolchain$key)) || any(!nzchar(toolchain$value)) ||
      !identical(
        digest::digest(toolchain, algo = "sha256", serialize = TRUE),
        manifest$toolchain_manifest_digest
      )) {
    rqr_dlm_companion_fail(
      "Resource-envelope toolchain manifest is incomplete or unbound."
    )
  }
  resource_toolchain_facts <-
    rqr_dlm_companion_normalize_resource_toolchain(toolchain)

  closeout <- rqr_dlm_companion_read_csv(
    file.path(directory, "resource_closeout.csv"),
    c(
      "schema_version", "source_commit", "package_version",
      "primary_runtime_tree_digest",
      "primary_runtime_attestation_sha256", "config_digest",
      "incidence_digest", "maximum_seed_ledger_digest",
      "toolchain_manifest_digest",
      "exact_commit_attestation_pair_verified",
      "promotion_evidence_eligible", "telemetry_complete",
      "resource_margin_pass", "all_writer_shapes_valid",
      "all_clean_deserializations_valid",
      "maximum_writer_or_clean_reader_peak_RSS_KiB", "status"
    ),
    "resource-envelope resource_closeout.csv"
  )
  binding <- c(
    source_commit = manifest$source_commit,
    package_version = manifest$package_version,
    primary_runtime_tree_digest = manifest$primary_runtime_tree_digest,
    primary_runtime_attestation_sha256 =
      manifest$primary_runtime_attestation_sha256,
    config_digest = manifest$config_digest,
    incidence_digest = manifest$incidence_digest,
    maximum_seed_ledger_digest = manifest$maximum_seed_ledger_digest,
    toolchain_manifest_digest = manifest$toolchain_manifest_digest
  )
  if (nrow(closeout) != 1L ||
      !identical(
        closeout$schema_version[[1L]],
        "rqrgibbs_dlm_resource_envelope_closeout/1.0.0"
      ) ||
      !identical(
        as.character(unlist(closeout[1L, names(binding)])),
        unname(binding)
      ) ||
      !all(vapply(
        closeout[1L, c(
          "exact_commit_attestation_pair_verified",
          "promotion_evidence_eligible", "telemetry_complete",
          "resource_margin_pass", "all_writer_shapes_valid",
          "all_clean_deserializations_valid"
        )],
        rqr_dlm_companion_scalar_true, logical(1L)
      )) ||
      !identical(
        as.numeric(
          closeout$maximum_writer_or_clean_reader_peak_RSS_KiB[[1L]]
        ),
        as.numeric(
          manifest$maximum_writer_or_clean_reader_peak_RSS_KiB
        )
      ) ||
      !identical(closeout$status[[1L]], "passed")) {
    rqr_dlm_companion_fail(
      "Resource-envelope closeout is not bound to the passing manifest."
    )
  }

  list(
    role = role, directory = directory, artifacts = artifacts,
    schema_version = manifest$schema_version,
    source_commit = expected_commit,
    package_version = manifest$package_version,
    runtime_tree_digest = reference$runtime_tree_digest,
    runtime_attestation_sha256 =
      manifest$primary_runtime_attestation_sha256,
    config_digest = manifest$config_digest,
    incidence_digest = manifest$incidence_digest,
    seed_ledger_digest = manifest$maximum_seed_ledger_digest,
    toolchain_digest = manifest$toolchain_manifest_digest,
    runtime_toolchain_facts = resource_toolchain_facts,
    semantic_counts = c(
      cases = nrow(envelope), shape_fields = nrow(shape),
      toolchain_fields = nrow(toolchain)
    )
  )
}

rqr_dlm_companion_validate_m01 <- function(
    directory, expected_commit, reference, role) {
  spec <- rqr_dlm_companion_wave_spec(role)
  if (!identical(spec$kind, "M01")) {
    rqr_dlm_companion_fail(role, " is not an M01 role.")
  }
  directory <- rqr_dlm_companion_validate_directory(directory, role)
  artifacts <- rqr_dlm_companion_verify_artifact_manifest(
    directory, role, c("path", "bytes", "sha256")
  )
  manifest <- rqr_dlm_companion_read_json(
    file.path(directory, "validation_manifest.json"),
    rqr_dlm_companion_m01_manifest_fields(),
    paste0(role, " validation_manifest.json")
  )
  rqr_dlm_companion_assert_thread_environment(
    manifest$thread_environment, role
  )
  kernel <- manifest$component_scale_kernel
  standard <- manifest$standard_component_scale_schedule
  sentinel <- manifest$sentinel_component_scale_schedule
  expected_transition_kernel <-
    rqr_dlm_companion_expected_m01_transition_kernel(role)
  observed_expected_transition_kernel <-
    rqr_dlm_companion_normalize_transition_kernel(
      manifest$expected_transition_kernel_contract,
      paste0(role, " expected transition contract")
    )
  expected_transition_kernel_digest <- digest::digest(
    expected_transition_kernel, algo = "sha256", serialize = TRUE
  )
  expected_transition_invariant <-
    rqr_dlm_companion_transition_kernel_invariant(
      expected_transition_kernel
    )
  observed_expected_transition_invariant <-
    rqr_dlm_companion_normalize_transition_invariant(
      manifest$expected_transition_kernel_invariant,
      expected_transition_kernel,
      paste0(role, " expected transition invariant")
    )
  expected_transition_invariant_digest <- digest::digest(
    expected_transition_invariant, algo = "sha256", serialize = TRUE
  )
  if (!identical(
        manifest$schema_version,
        "rqrgibbs_dlm_wave_correction_validation/2.2.0"
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
        manifest$wave_id, spec$wave_id
      ) ||
      !rqr_dlm_companion_is_integerish(
        manifest$wave_task_count, spec$task_count, spec$task_count
      ) ||
      !rqr_dlm_companion_is_integerish(
        manifest$chain_job_count, spec$chain_count, spec$chain_count
      ) ||
      !rqr_dlm_companion_is_integerish(manifest$workers, 8, 8) ||
      !rqr_dlm_companion_finite_column(
        manifest$total_fit_elapsed_seconds, 1L, minimum = 0
      ) ||
      !rqr_dlm_companion_scalar_true(
        manifest$exact_target_preserving_kernel
      ) ||
      !identical(
        manifest$transition_kernel_schema,
        "rqrgibbs_dlm_transition_kernel/1.0.0"
      ) ||
      !rqr_dlm_companion_is_hex(
        manifest$unique_transition_kernel_digests
      ) ||
      !identical(
        observed_expected_transition_kernel,
        expected_transition_kernel
      ) ||
      !identical(
        manifest$expected_transition_kernel_contract_digest,
        expected_transition_kernel_digest
      ) ||
      !identical(
        manifest$unique_transition_kernel_digests,
        expected_transition_kernel_digest
      ) ||
      !identical(
        manifest$transition_kernel_invariant_schema,
        "rqrgibbs_dlm_transition_kernel_invariant/1.0.0"
      ) ||
      !identical(
        observed_expected_transition_invariant,
        expected_transition_invariant
      ) ||
      !identical(
        manifest$expected_transition_kernel_invariant_digest,
        expected_transition_invariant_digest
      ) ||
      !rqr_dlm_companion_scalar_true(
        manifest$all_fit_transition_contracts_complete
      ) ||
      !rqr_dlm_companion_scalar_true(
        manifest$all_fit_transition_contracts_match_expected
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
      !rqr_dlm_companion_finite_column(
        manifest$maximum_process_peak_RSS_KiB, 1L, minimum = 1
      ) ||
      !rqr_dlm_companion_finite_column(
        manifest$declared_worker_memory_ceiling_KiB, 1L, minimum = 1
      ) ||
      manifest$maximum_process_peak_RSS_KiB >
        0.80 * manifest$declared_worker_memory_ceiling_KiB ||
      !rqr_dlm_companion_scalar_true(manifest$resource_margin_pass) ||
      !is.list(kernel) ||
      !identical(
        names(kernel),
        c(
          "one_root_partially_collapsed",
          "collapsed_integrated_root",
          "centered_inverse_gamma", "noncentered_slice_interweave",
          "interweave_cycles", "slice_width", "slice_sweeps_per_cycle",
          "slice_max_steps", "slice_max_shrink", "target_change"
        )
      ) ||
      !rqr_dlm_companion_scalar_true(
        kernel$one_root_partially_collapsed
      ) ||
      !identical(kernel$collapsed_integrated_root, "root1") ||
      !rqr_dlm_companion_scalar_true(kernel$centered_inverse_gamma) ||
      !rqr_dlm_companion_scalar_true(
        kernel$noncentered_slice_interweave
      ) ||
      !rqr_dlm_companion_is_integerish(
        kernel$interweave_cycles, 1, 1
      ) ||
      !rqr_dlm_companion_is_integerish(kernel$slice_width, 1, 1) ||
      !rqr_dlm_companion_is_integerish(
        kernel$slice_sweeps_per_cycle, 3, 3
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
        c(1000, 6000, 1)
      )) {
    rqr_dlm_companion_fail(
      "M01 source, runtime, target, fit, diagnostic, or interweaving gate failed."
    )
  }
  diagnostics <- rqr_dlm_companion_read_csv(
    file.path(directory, paste0(spec$tag, "_M01_diagnostics.csv")),
    c(
      "estimand", "chains", "rhat", "ess_bulk", "ess_tail",
      "mcse_mean", "mcse_over_sd", "pass", "DGP", "replication",
      "sentinel"
    ),
    "M01 diagnostics"
  )
  summary <- rqr_dlm_companion_read_csv(
    file.path(directory, paste0(spec$tag, "_M01_summary.csv")),
    c(
      "DGP", "replication", "sentinel", "chains", "diagnostics",
      "diagnostics_passed", "all_pass", "fit_elapsed_seconds",
      "maximum_peak_RSS_KiB",
      "log_q_1_rhat", "log_q_1_ess_bulk", "log_q_1_ess_tail",
      "log_q_1_mcse_over_sd", "transition_kernel_fit_count",
      "transition_kernel_schemas", "transition_kernel_digests",
      "transition_kernel_contract_digests",
      "transition_kernel_contract_matches"
    ),
    "M01 summary"
  )
  if (nrow(summary) != spec$task_count ||
      any(!summary$DGP %in% spec$DGP) ||
      !rqr_dlm_companion_integer_column(
        summary$replication, spec$task_count, minimum = 1
      ) ||
      !rqr_dlm_companion_logical_column(
        summary$sentinel, spec$task_count
      ) ||
      sum(summary$sentinel) != spec$sentinel_count ||
      !rqr_dlm_companion_integer_column(
        summary$chains, spec$task_count, minimum = 1
      ) ||
      any(summary$chains != ifelse(summary$sentinel, 4, 1)) ||
      sum(summary$chains) != spec$chain_count ||
      !rqr_dlm_companion_integer_column(
        summary$diagnostics, spec$task_count,
        minimum = 46, maximum = 46
      ) ||
      !rqr_dlm_companion_integer_column(
        summary$diagnostics_passed, spec$task_count,
        minimum = 46, maximum = 46
      ) ||
      !rqr_dlm_companion_logical_column(
        summary$all_pass, spec$task_count
      ) ||
      !all(summary$all_pass) ||
      !rqr_dlm_companion_finite_column(
        summary$fit_elapsed_seconds, spec$task_count, minimum = 0
      ) ||
      !rqr_dlm_companion_finite_column(
        summary$maximum_peak_RSS_KiB, spec$task_count, minimum = 1
      ) ||
      !rqr_dlm_companion_integer_column(
        summary$transition_kernel_fit_count, spec$task_count,
        minimum = 1, maximum = 4
      ) ||
      any(summary$transition_kernel_fit_count != summary$chains) ||
      any(summary$diagnostics_passed != summary$diagnostics)) {
    rqr_dlm_companion_fail(
      "M01 diagnostic and summary tables are incomplete or failed."
    )
  }
  split_fit_values <- function(value) {
    strsplit(as.character(value), "|", fixed = TRUE)[[1L]]
  }
  for (row in seq_len(nrow(summary))) {
    expected_count <- as.integer(summary$chains[[row]])
    schemas <- split_fit_values(
      summary$transition_kernel_schemas[[row]]
    )
    digests <- split_fit_values(
      summary$transition_kernel_digests[[row]]
    )
    contract_digests <- split_fit_values(
      summary$transition_kernel_contract_digests[[row]]
    )
    matches <- split_fit_values(
      summary$transition_kernel_contract_matches[[row]]
    )
    if (length(schemas) != expected_count ||
        length(digests) != expected_count ||
        length(contract_digests) != expected_count ||
        length(matches) != expected_count ||
        any(schemas != "rqrgibbs_dlm_transition_kernel/1.0.0") ||
        any(digests != expected_transition_kernel_digest) ||
        any(contract_digests != expected_transition_kernel_digest) ||
        any(matches != "TRUE")) {
      rqr_dlm_companion_fail(
        "M01 per-fit transition-contract evidence is incomplete or changed."
      )
    }
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
    role = role, directory = directory, artifacts = artifacts,
    schema_version = manifest$schema_version,
    source_commit = expected_commit,
    package_version = manifest$package_version,
    runtime_tree_digest = reference$runtime_tree_digest,
    runtime_attestation_sha256 =
      manifest$primary_runtime_attestation_sha256,
    config_digest = manifest$config_digest,
    incidence_digest = manifest$incidence_digest,
    seed_ledger_digest = manifest$seed_ledger_digest,
    transition_kernel_schema = manifest$transition_kernel_schema,
    transition_kernel_digest =
      manifest$unique_transition_kernel_digests,
    transition_kernel_contract = expected_transition_kernel,
    transition_kernel_invariant_schema =
      manifest$transition_kernel_invariant_schema,
    transition_kernel_invariant =
      expected_transition_invariant,
    transition_kernel_invariant_digest =
      manifest$expected_transition_kernel_invariant_digest,
    task_keys = sort(paste(summary$DGP, summary$replication, sep = "::")),
    semantic_counts = c(
      wave_tasks = spec$task_count, chain_jobs = spec$chain_count,
      diagnostics = nrow(diagnostics)
    )
  )
}

rqr_dlm_companion_validate_m02 <- function(
    directory, expected_commit, reference, role) {
  spec <- rqr_dlm_companion_wave_spec(role)
  if (!identical(spec$kind, "M02")) {
    rqr_dlm_companion_fail(role, " is not an M02 role.")
  }
  directory <- rqr_dlm_companion_validate_directory(directory, role)
  artifacts <- rqr_dlm_companion_verify_artifact_manifest(
    directory, role, c("path", "bytes", "sha256")
  )
  manifest <- rqr_dlm_companion_read_json(
    file.path(directory, "validation_manifest.json"),
    rqr_dlm_companion_m02_manifest_fields(),
    paste0(role, " validation_manifest.json")
  )
  rqr_dlm_companion_assert_thread_environment(
    manifest$thread_environment, role
  )
  schedules <- manifest$frozen_schedules
  schedule_evidence <- manifest$applied_schedule_evidence
  valid_schedule <- function(value) {
    is.list(value) &&
      identical(names(value), c("burn", "retain", "thin")) &&
      all(vapply(
        value, rqr_dlm_companion_is_integerish, logical(1L),
        minimum = 1
      )) &&
      identical(
        unname(as.numeric(unlist(value))), c(1000, 4000, 1)
      )
  }
  expected_schedule_jobs <- c(
    standard = spec$chain_count - 4L * spec$sentinel_count,
    sentinel = 4L * spec$sentinel_count
  )
  valid_schedule_evidence <- is.list(schedule_evidence) &&
    identical(names(schedule_evidence), c("standard", "sentinel")) &&
    all(vapply(names(schedule_evidence), function(name) {
      value <- schedule_evidence[[name]]
      is.list(value) &&
        identical(
          names(value),
          c(
            "configured_schedule", "interval_chain_job_count",
            "realized_state_draw_dimensions",
            "realized_scale_draw_lengths", "all_applied"
          )
        ) &&
        valid_schedule(value$configured_schedule) &&
        rqr_dlm_companion_is_integerish(
          value$interval_chain_job_count,
          expected_schedule_jobs[[name]], expected_schedule_jobs[[name]]
        ) &&
        is.character(value$realized_state_draw_dimensions) &&
        length(value$realized_state_draw_dimensions) >= 1L &&
        all(grepl("^[1-9][0-9]*x[1-9][0-9]*x4000$",
                  value$realized_state_draw_dimensions)) &&
        is.numeric(value$realized_scale_draw_lengths) &&
        length(value$realized_scale_draw_lengths) >= 1L &&
        all(value$realized_scale_draw_lengths == 4000) &&
        rqr_dlm_companion_scalar_true(value$all_applied)
    }, logical(1L)))
  if (!identical(
        manifest$schema_version,
        "rqrgibbs_dlm_wave_comparator_projection_validation/2.1.0"
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
        manifest$wave_id, spec$wave_id
      ) ||
      !rqr_dlm_companion_is_integerish(
        manifest$wave_task_count, spec$task_count, spec$task_count
      ) ||
      !rqr_dlm_companion_is_integerish(
        manifest$interval_chain_job_count,
        spec$chain_count, spec$chain_count
      ) ||
      !rqr_dlm_companion_is_integerish(
        manifest$logical_endpoint_fit_count,
        spec$endpoint_count, spec$endpoint_count
      ) ||
      !rqr_dlm_companion_is_integerish(manifest$workers, 8, 8) ||
      !rqr_dlm_companion_finite_column(
        manifest$total_fit_elapsed_seconds, 1L, minimum = 0
      ) ||
      !identical(
        manifest$comparator_projection,
        "colSums(FF * posterior_state_mean_or_draw)"
      ) ||
      !rqr_dlm_companion_scalar_true(
        manifest$common_target_across_initialization_profiles
      ) ||
      !rqr_dlm_companion_scalar_true(
        manifest$overdispersed_initialization_profiles_verified
      ) ||
      !is.list(schedules) ||
      !identical(names(schedules), c("standard", "sentinel")) ||
      !all(vapply(schedules, valid_schedule, logical(1L))) ||
      !valid_schedule_evidence ||
      !rqr_dlm_companion_scalar_true(
        manifest$all_applied_schedules_verified
      ) ||
      !identical(
        unname(unlist(
          manifest$schedule_evidence_fields, use.names = FALSE
        )),
        c(
          "schedule_role", "applied_schedule", "schedule_applied",
          "state_draw_dimensions", "scale_draw_lengths"
        )
      ) ||
      !identical(
        manifest$initialization_contract,
        "target_preserving_precomputed_mcmc_state"
      ) ||
      !identical(
        manifest$target_fields_held_fixed,
        paste0(
          "y;m0;C0;FF;GG;discounts;component_dimensions;",
          "dqlm_ind;fix_sigma;PriorSigma;quantile_probability"
        )
      ) ||
      !rqr_dlm_companion_scalar_false(
        manifest$comparative_simulation_metrics_used
      ) ||
      !rqr_dlm_companion_scalar_false(manifest$failed_outputs_reused) ||
      !rqr_dlm_companion_scalar_true(manifest$all_fits_succeeded) ||
      !rqr_dlm_companion_scalar_true(manifest$all_diagnostics_passed) ||
      !rqr_dlm_companion_finite_column(
        manifest$maximum_process_peak_RSS_KiB, 1L, minimum = 1
      ) ||
      !rqr_dlm_companion_finite_column(
        manifest$declared_worker_memory_ceiling_KiB, 1L, minimum = 1
      ) ||
      manifest$maximum_process_peak_RSS_KiB >
        0.80 * manifest$declared_worker_memory_ceiling_KiB ||
      !rqr_dlm_companion_scalar_true(manifest$resource_margin_pass)) {
    rqr_dlm_companion_fail(
      "M02 source, runtime, projection, fit, or diagnostic gate failed."
    )
  }
  diagnostics <- rqr_dlm_companion_read_csv(
    file.path(directory, paste0(spec$tag, "_M02_diagnostics.csv")),
    c(
      "estimand", "chains", "rhat", "ess_bulk", "ess_tail",
      "mcse_mean", "mcse_over_sd", "pass", "DGP", "replication",
      "sentinel"
    ),
    "M02 diagnostics"
  )
  summary <- rqr_dlm_companion_read_csv(
    file.path(directory, paste0(spec$tag, "_M02_summary.csv")),
    c(
      "DGP", "replication", "sentinel", "schedule_role",
      "configured_burn", "configured_retain", "configured_thin",
      "applied_burn", "applied_retain", "applied_thin",
      "realized_state_draw_dimensions", "realized_scale_draw_lengths",
      "schedule_contract_pass", "chains", "diagnostics",
      "diagnostics_passed", "all_pass", "fit_elapsed_seconds",
      "maximum_peak_RSS_KiB",
      "minimum_bulk_ess", "minimum_tail_ess", "maximum_mcse_over_sd"
    ),
    "M02 summary"
  )
  if (nrow(summary) != spec$task_count ||
      any(!summary$DGP %in% spec$DGP) ||
      !rqr_dlm_companion_integer_column(
        summary$replication, spec$task_count, minimum = 1
      ) ||
      !rqr_dlm_companion_logical_column(
        summary$sentinel, spec$task_count
      ) ||
      sum(summary$sentinel) != spec$sentinel_count ||
      !identical(
        as.character(summary$schedule_role),
        ifelse(summary$sentinel, "sentinel", "standard")
      ) ||
      !identical(
        as.numeric(summary$configured_burn),
        rep(1000, spec$task_count)
      ) ||
      !identical(
        as.numeric(summary$configured_retain),
        rep(4000, spec$task_count)
      ) ||
      !identical(
        as.numeric(summary$configured_thin),
        rep(1, spec$task_count)
      ) ||
      !identical(
        as.numeric(summary$applied_burn),
        rep(1000, spec$task_count)
      ) ||
      !identical(
        as.numeric(summary$applied_retain),
        rep(4000, spec$task_count)
      ) ||
      !identical(
        as.numeric(summary$applied_thin),
        rep(1, spec$task_count)
      ) ||
      any(!grepl(
        paste0(
          "^[1-9][0-9]*x[1-9][0-9]*x4000",
          "(;[1-9][0-9]*x[1-9][0-9]*x4000)*$"
        ),
        summary$realized_state_draw_dimensions
      )) ||
      any(!grepl(
        "^4000(;4000)*$", summary$realized_scale_draw_lengths
      )) ||
      !rqr_dlm_companion_logical_column(
        summary$schedule_contract_pass, spec$task_count
      ) ||
      !all(summary$schedule_contract_pass) ||
      !rqr_dlm_companion_integer_column(
        summary$chains, spec$task_count, minimum = 1
      ) ||
      any(summary$chains != ifelse(summary$sentinel, 4, 1)) ||
      sum(summary$chains) != spec$chain_count ||
      !rqr_dlm_companion_integer_column(
        summary$diagnostics, spec$task_count,
        minimum = 45, maximum = 45
      ) ||
      !rqr_dlm_companion_integer_column(
        summary$diagnostics_passed, spec$task_count,
        minimum = 45, maximum = 45
      ) ||
      !rqr_dlm_companion_logical_column(
        summary$all_pass, spec$task_count
      ) ||
      !all(summary$all_pass) ||
      !rqr_dlm_companion_finite_column(
        summary$fit_elapsed_seconds, spec$task_count, minimum = 0
      ) ||
      !rqr_dlm_companion_finite_column(
        summary$maximum_peak_RSS_KiB, spec$task_count, minimum = 1
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
    role = role, directory = directory, artifacts = artifacts,
    schema_version = manifest$schema_version,
    source_commit = expected_commit,
    package_version = manifest$package_version,
    runtime_tree_digest = manifest$primary_runtime_tree_digest,
    runtime_attestation_sha256 =
      manifest$primary_runtime_attestation_sha256,
    config_digest = manifest$config_digest,
    incidence_digest = manifest$incidence_digest,
    seed_ledger_digest = manifest$seed_ledger_digest,
    exdqlm_runtime_attestation_sha256 =
      manifest$exdqlm_runtime_attestation_sha256,
    exdqlm_runtime_tree_digest = manifest$exdqlm_runtime_tree_digest,
    exdqlm_source_package_sha256 =
      manifest$exdqlm_source_package_sha256,
    schedule_contract_digest = digest::digest(
      list(
        configured = manifest$frozen_schedules,
        fields = manifest$schedule_evidence_fields
      ),
      algo = "sha256", serialize = TRUE
    ),
    task_keys = sort(paste(summary$DGP, summary$replication, sep = "::")),
    semantic_counts = c(
      wave_tasks = spec$task_count,
      interval_chains = spec$chain_count,
      endpoint_fits = spec$endpoint_count, diagnostics = nrow(diagnostics),
      schedule_roles = 2
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
  roles <- rqr_dlm_companion_roles()
  if (!identical(names(inputs), roles)) {
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
  if (!identical(
      inputs$resource_envelope$runtime_toolchain_facts,
      inputs$dlm_reference$runtime_toolchain_facts
    )) {
    rqr_dlm_companion_fail(
      paste(
        "The resource-envelope toolchain facts do not match the",
        "reference runtime toolchain."
      )
    )
  }
  correction_roles <- setdiff(roles, "dlm_reference")
  correction <- inputs[correction_roles]
  for (field in c(
      "config_digest", "incidence_digest", "seed_ledger_digest"
    )) {
    values <- vapply(correction, `[[`, character(1L), field)
    if (any(!grepl("^[0-9a-f]{64}$", values)) ||
        length(unique(values)) != 1L) {
      rqr_dlm_companion_fail(
        "The M01/M02/horizon/resource ", field, " values do not match."
      )
    }
  }
  wave_pairs <- list(
    c("wave1_M01_static_gaussian", "wave1_M02_static_gaussian"),
    c("wave2_M01_local_level", "wave2_M02_local_level")
  )
  if (any(vapply(wave_pairs, function(pair) {
      !identical(inputs[[pair[[1L]]]]$task_keys,
                 inputs[[pair[[2L]]]]$task_keys)
    }, logical(1L))) ||
      any(!inputs$horizon_M03$task_keys %in%
            inputs$wave1_M01_static_gaussian$task_keys)) {
    rqr_dlm_companion_fail(
      "The wave-specific M01/M02 and M03 task-key evidence is inconsistent."
    )
  }
  m01 <- inputs[c(
    "wave1_M01_static_gaussian", "wave2_M01_local_level"
  )]
  if (!identical(
        m01[[1L]]$transition_kernel_contract$
          collapsed_log_q_coordinate_order,
        "regression"
      ) ||
      !identical(
        m01[[2L]]$transition_kernel_contract$
          collapsed_log_q_coordinate_order,
        "level"
      ) ||
      !identical(
        m01[[1L]]$transition_kernel_invariant_schema,
        m01[[2L]]$transition_kernel_invariant_schema
      ) ||
      !identical(
        m01[[1L]]$transition_kernel_invariant,
        m01[[2L]]$transition_kernel_invariant
      ) ||
      !identical(
        m01[[1L]]$transition_kernel_invariant_digest,
        m01[[2L]]$transition_kernel_invariant_digest
      ) ||
      identical(
        m01[[1L]]$transition_kernel_digest,
        m01[[2L]]$transition_kernel_digest
      )) {
    rqr_dlm_companion_fail(
      paste(
        "The two M01 waves do not share the exact versioned transition",
        "invariant with their frozen regression/level coordinate labels."
      )
    )
  }
  m02 <- inputs[c(
    "wave1_M02_static_gaussian", "wave2_M02_local_level"
  )]
  for (field in c(
      "exdqlm_runtime_attestation_sha256",
      "exdqlm_runtime_tree_digest", "exdqlm_source_package_sha256",
      "schedule_contract_digest"
    )) {
    if (length(unique(vapply(m02, `[[`, character(1L), field))) != 1L) {
      rqr_dlm_companion_fail(
        "The two M02 waves do not share one exact ", field, "."
      )
    }
  }
  invisible(TRUE)
}

rqr_dlm_companion_capture_input_ledger <- function(directories) {
  roles <- rqr_dlm_companion_roles()
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
    "protected_source_inventory",
    "reference_artifact_closure", "reference_R_cpp_parity",
    "reference_missing_future", "reference_component_scale",
    "reference_six_continuation_cells", "reference_history_mutations",
    "reference_process_monitor",
    "wave1_M01_fits_and_diagnostics",
    "wave1_M01_transition_kernel",
    "wave1_M02_fits_diagnostics_and_projection",
    "wave1_M02_schedule_contract",
    "wave2_M01_fits_and_diagnostics",
    "wave2_M01_transition_kernel",
    "wave2_M02_fits_diagnostics_and_projection",
    "wave2_M02_schedule_contract",
    "cross_wave_transition_kernel",
    "cross_wave_exdqlm_runtime_and_schedules",
    "horizon_scenarios_and_M03", "dynamic_endpoint_boundary",
    "resource_envelope_and_confirmatory_contract"
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
    "confirmatory_seed_ledger_digest",
    "protected_source_inventory_count",
    "protected_source_inventory_digest",
    "wave1_transition_kernel_digest",
    "wave2_transition_kernel_digest",
    "transition_kernel_invariant_schema",
    "transition_kernel_invariant_digest",
    "exdqlm_runtime_attestation_sha256",
    "exdqlm_runtime_tree_digest", "exdqlm_source_package_sha256",
    "comparator_schedule_contract_digest",
    "reference_runtime_toolchain_digest",
    "resource_toolchain_manifest_digest", "input_roles",
    "input_artifact_count", "semantic_gate_count",
    "semantic_gate_pass_count", "all_semantic_gates_passed",
    "heavy_input_artifact_count", "fits_executed_by_collector",
    "heavy_input_artifacts_deserialized",
    "heavy_input_artifacts_copied",
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
  roles <- rqr_dlm_companion_roles()
  manifest_roles <- unname(unlist(manifest$input_roles, use.names = FALSE))
  compact_wave1_kernel <-
    rqr_dlm_companion_expected_m01_transition_kernel(
      "wave1_M01_static_gaussian"
    )
  compact_wave2_kernel <-
    rqr_dlm_companion_expected_m01_transition_kernel(
      "wave2_M01_local_level"
    )
  compact_wave1_kernel_digest <- digest::digest(
    compact_wave1_kernel, algo = "sha256", serialize = TRUE
  )
  compact_wave2_kernel_digest <- digest::digest(
    compact_wave2_kernel, algo = "sha256", serialize = TRUE
  )
  compact_kernel_invariant_digest <- digest::digest(
    rqr_dlm_companion_transition_kernel_invariant(
      compact_wave1_kernel
    ),
    algo = "sha256", serialize = TRUE
  )
  digest_fields <- c(
    "reference_config_digest", "confirmatory_config_digest",
    "confirmatory_incidence_digest",
    "confirmatory_seed_ledger_digest",
    "protected_source_inventory_digest",
    "wave1_transition_kernel_digest",
    "wave2_transition_kernel_digest",
    "transition_kernel_invariant_digest",
    "exdqlm_runtime_attestation_sha256",
    "exdqlm_runtime_tree_digest", "exdqlm_source_package_sha256",
    "comparator_schedule_contract_digest",
    "reference_runtime_toolchain_digest",
    "resource_toolchain_manifest_digest"
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
        manifest$protected_source_inventory_count, 29, 29
      ) ||
      !identical(
        manifest$transition_kernel_invariant_schema,
        "rqrgibbs_dlm_transition_kernel_invariant/1.0.0"
      ) ||
      !identical(
        manifest$wave1_transition_kernel_digest,
        compact_wave1_kernel_digest
      ) ||
      !identical(
        manifest$wave2_transition_kernel_digest,
        compact_wave2_kernel_digest
      ) ||
      !identical(
        manifest$transition_kernel_invariant_digest,
        compact_kernel_invariant_digest
      ) ||
      !rqr_dlm_companion_is_integerish(
        manifest$input_artifact_count, 55, 55
      ) ||
      !rqr_dlm_companion_is_integerish(
        manifest$semantic_gate_count, 23, 23
      ) ||
      !rqr_dlm_companion_is_integerish(
        manifest$semantic_gate_pass_count, 23, 23
      ) ||
      !rqr_dlm_companion_scalar_true(
        manifest$all_semantic_gates_passed
      ) ||
      !rqr_dlm_companion_is_integerish(
        manifest$heavy_input_artifact_count, 7, 7
      ) ||
      !rqr_dlm_companion_scalar_false(
        manifest$fits_executed_by_collector
      ) ||
      !rqr_dlm_companion_scalar_false(
        manifest$heavy_input_artifacts_deserialized
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
    "rqrgibbs_dlm_wave_correction_validation/2.2.0",
    "rqrgibbs_dlm_wave_comparator_projection_validation/2.1.0",
    "rqrgibbs_dlm_wave_correction_validation/2.2.0",
    "rqrgibbs_dlm_wave_comparator_projection_validation/2.1.0",
    "rqrgibbs_dlm_horizon_fixed_design_validation/1.0.0",
    "rqrgibbs_dlm_resource_envelope_validation/2.0.0"
  )
  expected_binding <- c(
    "direct_runtime_tree_and_attestation",
    "direct_tree_plus_matching_reference_attestation",
    "matching_reference_attestation",
    "direct_tree_plus_matching_reference_attestation",
    "matching_reference_attestation",
    "matching_reference_attestation",
    "direct_runtime_tree_and_attestation"
  )
  if (nrow(summary) != 7L ||
      !identical(
        as.character(summary$schema_version),
        rep(rqr_dlm_companion_schema(), 7L)
      ) ||
      !identical(as.character(summary$input_role), roles) ||
      !identical(
        as.character(summary$input_schema), expected_input_schemas
      ) ||
      !identical(
        tolower(as.character(summary$source_commit)),
        rep(expected_commit, 7L)
      ) ||
      !identical(
        as.character(summary$package_version),
        rep(expected_package_version, 7L)
      ) ||
      !identical(
        tolower(as.character(summary$primary_runtime_tree_digest)),
        rep(expected_runtime_tree_digest, 7L)
      ) ||
      !identical(
        tolower(as.character(
          summary$primary_runtime_attestation_sha256
        )),
        rep(expected_runtime_attestation_sha256, 7L)
      ) ||
      !identical(
        as.character(summary$runtime_binding_basis), expected_binding
      ) ||
      !rqr_dlm_companion_integer_column(
        summary$artifact_count, 7L, minimum = 0
      ) ||
      !identical(
        as.numeric(summary$artifact_count), c(22, 5, 5, 5, 5, 7, 6)
      ) ||
      !rqr_dlm_companion_integer_column(
        summary$artifact_bytes, 7L, minimum = 0
      ) ||
      any(!vapply(
        as.character(summary$artifact_manifest_sha256),
        rqr_dlm_companion_is_hex, logical(1L)
      )) ||
      !identical(
        as.character(summary$semantic_status), rep("pass", 7L)
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
  if (nrow(input_artifacts) != 55L ||
      !identical(
        as.character(input_artifacts$schema_version),
        rep(rqr_dlm_companion_schema(), 55L)
      ) ||
      !identical(as.character(input_artifacts$input_role), expected_role) ||
      !identical(
        as.character(input_artifacts$relative_path), expected_relative
      ) ||
      !rqr_dlm_companion_integer_column(
        input_artifacts$byte_count, 55L, minimum = 0
      ) ||
      any(!vapply(
        as.character(input_artifacts$sha256),
        rqr_dlm_companion_is_hex, logical(1L)
      )) ||
      !rqr_dlm_companion_logical_column(
        input_artifacts$listed_in_source_manifest, 55L
      ) ||
      !identical(
        input_artifacts$listed_in_source_manifest,
        input_artifacts$relative_path != "artifact_hashes.csv"
      ) ||
      sum(grepl("\\.rds$", input_artifacts$relative_path)) !=
        manifest$heavy_input_artifact_count) {
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
  if (nrow(semantic) != 23L ||
      !identical(
        as.character(semantic$schema_version),
        rep(rqr_dlm_companion_schema(), 23L)
      ) ||
      !identical(
        as.character(semantic$gate_id),
        rqr_dlm_companion_semantic_gate_names()
      ) ||
      !identical(as.character(semantic$status), rep("pass", 23L)) ||
      anyNA(semantic$detail) || any(!nzchar(semantic$detail))) {
    rqr_dlm_companion_fail(
      "Compact companion semantic gate table is incomplete or failed."
    )
  }
  semantic_detail <- stats::setNames(
    as.character(semantic$detail), as.character(semantic$gate_id)
  )
  expected_bound_details <- c(
    wave1_M01_transition_kernel = paste(
      "rqrgibbs_dlm_transition_kernel/1.0.0",
      manifest$wave1_transition_kernel_digest, sep = "|"
    ),
    wave2_M01_transition_kernel = paste(
      "rqrgibbs_dlm_transition_kernel/1.0.0",
      manifest$wave2_transition_kernel_digest, sep = "|"
    ),
    cross_wave_transition_kernel =
      manifest$transition_kernel_invariant_digest,
    resource_envelope_and_confirmatory_contract = paste(
      manifest$reference_runtime_toolchain_digest,
      manifest$resource_toolchain_manifest_digest,
      3L, manifest$confirmatory_config_digest, sep = "|"
    )
  )
  if (!identical(
      unname(semantic_detail[names(expected_bound_details)]),
      unname(expected_bound_details)
    )) {
    rqr_dlm_companion_fail(
      "Compact companion semantic details are not manifest-bound."
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
    repo_root, expected_commit, reference_directory,
    wave1_m01_directory, wave1_m02_directory,
    wave2_m01_directory, wave2_m02_directory,
    horizon_directory, resource_directory, output_directory,
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
    dlm_reference = reference_directory,
    wave1_M01_static_gaussian = wave1_m01_directory,
    wave1_M02_static_gaussian = wave1_m02_directory,
    wave2_M01_local_level = wave2_m01_directory,
    wave2_M02_local_level = wave2_m02_directory,
    horizon_M03 = horizon_directory,
    resource_envelope = resource_directory
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
  wave1_m01 <- rqr_dlm_companion_validate_m01(
    wave1_m01_directory, expected_commit, reference,
    "wave1_M01_static_gaussian"
  )
  wave1_m02 <- rqr_dlm_companion_validate_m02(
    wave1_m02_directory, expected_commit, reference,
    "wave1_M02_static_gaussian"
  )
  wave2_m01 <- rqr_dlm_companion_validate_m01(
    wave2_m01_directory, expected_commit, reference,
    "wave2_M01_local_level"
  )
  wave2_m02 <- rqr_dlm_companion_validate_m02(
    wave2_m02_directory, expected_commit, reference,
    "wave2_M02_local_level"
  )
  horizon <- rqr_dlm_companion_validate_horizon(
    horizon_directory, expected_commit, reference
  )
  resource <- rqr_dlm_companion_validate_resource(
    resource_directory, expected_commit, reference
  )
  inputs <- list(
    dlm_reference = reference,
    wave1_M01_static_gaussian = wave1_m01,
    wave1_M02_static_gaussian = wave1_m02,
    wave2_M01_local_level = wave2_m01,
    wave2_M02_local_level = wave2_m02,
    horizon_M03 = horizon,
    resource_envelope = resource
  )
  rqr_dlm_companion_cross_validate(inputs, expected_commit)
  rqr_dlm_companion_assert_input_closure(inputs)
  rqr_dlm_companion_assert_input_ledger(inputs, initial_input_ledger)
  protected_source <- rqr_dlm_companion_protected_source_inventory(
    repo_root
  )

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
      paste0(
        nrow(protected_source$files), " files|", protected_source$digest
      ),
      "22 exact regular artifacts; recursive manifest verified",
      "dense conditional, full cross-time covariance, and R/C++ parity",
      "canonical missingness and public future-root checks",
      "inverse-Gamma conditionals, orientation, and future scales",
      "3 fixtures x 2 rate modes; every saved field and checkpoint",
      "27 rehashed raw/semantic mutations rejected",
      "wrapper, resource, fault, final-PGID, and zero-failure gates",
      paste0(
        wave1_m01$semantic_counts[["chain_jobs"]],
        " exact wave-1 M01 chains"
      ),
      paste(
        wave1_m01$transition_kernel_schema,
        wave1_m01$transition_kernel_digest, sep = "|"
      ),
      paste0(
        wave1_m02$semantic_counts[["endpoint_fits"]],
        " exact wave-1 M02 endpoint fits"
      ),
      wave1_m02$schedule_contract_digest,
      paste0(
        wave2_m01$semantic_counts[["chain_jobs"]],
        " exact wave-2 M01 chains"
      ),
      paste(
        wave2_m01$transition_kernel_schema,
        wave2_m01$transition_kernel_digest, sep = "|"
      ),
      paste0(
        wave2_m02$semantic_counts[["endpoint_fits"]],
        " exact wave-2 M02 endpoint fits"
      ),
      wave2_m02$schedule_contract_digest,
      wave1_m01$transition_kernel_invariant_digest,
      paste(
        wave1_m02$exdqlm_runtime_tree_digest,
        wave1_m02$exdqlm_runtime_attestation_sha256,
        wave1_m02$schedule_contract_digest, sep = "|"
      ),
      paste0(
        horizon$semantic_counts[["horizon_scenarios"]],
        " horizon scenarios|",
        horizon$semantic_counts[["M03_tasks"]], " M03 tasks"
      ),
      "one exact M01 training/future endpoint check",
      paste(
        reference$runtime_toolchain_digest,
        resource$toolchain_digest,
        resource$semantic_counts[["cases"]],
        resource$config_digest, sep = "|"
      )
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
      } else if (grepl("_M01_", input$role, fixed = TRUE)) {
        "direct_tree_plus_matching_reference_attestation"
      } else if (identical(input$role, "resource_envelope")) {
        "direct_runtime_tree_and_attestation"
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
    confirmatory_config_digest = wave1_m01$config_digest,
    confirmatory_incidence_digest = wave1_m01$incidence_digest,
    confirmatory_seed_ledger_digest = wave1_m01$seed_ledger_digest,
    protected_source_inventory_count = nrow(protected_source$files),
    protected_source_inventory_digest = protected_source$digest,
    wave1_transition_kernel_digest =
      wave1_m01$transition_kernel_digest,
    wave2_transition_kernel_digest =
      wave2_m01$transition_kernel_digest,
    transition_kernel_invariant_schema =
      wave1_m01$transition_kernel_invariant_schema,
    transition_kernel_invariant_digest =
      wave1_m01$transition_kernel_invariant_digest,
    exdqlm_runtime_attestation_sha256 =
      wave1_m02$exdqlm_runtime_attestation_sha256,
    exdqlm_runtime_tree_digest =
      wave1_m02$exdqlm_runtime_tree_digest,
    exdqlm_source_package_sha256 =
      wave1_m02$exdqlm_source_package_sha256,
    comparator_schedule_contract_digest =
      wave1_m02$schedule_contract_digest,
    reference_runtime_toolchain_digest =
      reference$runtime_toolchain_digest,
    resource_toolchain_manifest_digest = resource$toolchain_digest,
    input_roles = names(inputs),
    input_artifact_count = nrow(input_artifacts),
    semantic_gate_count = nrow(semantic_gates),
    semantic_gate_pass_count = sum(semantic_gates$status == "pass"),
    all_semantic_gates_passed = TRUE,
    heavy_input_artifact_count =
      sum(grepl("\\.rds$", initial_input_ledger$relative_path)),
    fits_executed_by_collector = FALSE,
    heavy_input_artifacts_deserialized = FALSE,
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
  if (length(arguments) != 9L) {
    rqr_dlm_companion_fail(
      paste(
        "Usage: 30_bundle_rqr_ordinary_v1_protected_dlm_evidence.R",
        "<expected-commit> <reference-dir>",
        "<wave1-M01-dir> <wave1-M02-dir>",
        "<wave2-M01-dir> <wave2-M02-dir>",
        "<horizon-M03-dir> <resource-envelope-dir>",
        "<new-ignored-output-dir>"
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
    wave1_m01_directory = arguments[[3L]],
    wave1_m02_directory = arguments[[4L]],
    wave2_m01_directory = arguments[[5L]],
    wave2_m02_directory = arguments[[6L]],
    horizon_directory = arguments[[7L]],
    resource_directory = arguments[[8L]],
    output_directory = arguments[[9L]],
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
