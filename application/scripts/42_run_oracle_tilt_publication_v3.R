#!/usr/bin/env Rscript

arguments <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_path <- sub(file_arg, "", arguments[startsWith(arguments, file_arg)][1L])
if (!length(script_path) || is.na(script_path)) {
  script_path <- "application/scripts/42_run_oracle_tilt_publication_v3.R"
}
script_path <- normalizePath(script_path, winslash = "/", mustWork = TRUE)
script_dir <- dirname(script_path)
source(file.path(script_dir, "32_oracle_tilt_illustration_utils.R"))
source(file.path(script_dir, "33_oracle_tilt_forensic_utils.R"))
source(file.path(script_dir, "34_oracle_tilt_publication_utils.R"))
source(file.path(script_dir, "42_oracle_tilt_publication_v3_utils.R"))

trailing <- commandArgs(trailingOnly = TRUE)
arg_value <- function(prefix, default = NULL) {
  hit <- trailing[startsWith(trailing, prefix)]
  if (length(hit)) sub(prefix, "", hit[1L], fixed = TRUE) else default
}
mode <- tolower(arg_value("--mode=", "preflight"))
allowed_modes <- c("preflight", "reference-only", "benchmark", "execute")
if (!mode %in% allowed_modes) {
  oti_stop("--mode must be preflight, reference-only, benchmark, or execute.")
}
execute_stage <- tolower(arg_value("--execute-stage=", ""))
execute_family <- tolower(arg_value("--family=", ""))
execute_target <- toupper(arg_value("--target=", ""))
if (identical(mode, "execute")) {
  if (!execute_stage %in% c("prepare", "cell", "finalize")) {
    oti_stop(
      "execute must be invoked through the process-isolated orchestrator ",
      "with --execute-stage=prepare, cell, or finalize."
    )
  }
  if (identical(execute_stage, "cell") &&
      (!execute_family %in% c("fixed_design", "dlm") ||
       !execute_target %in% c("RQR", "ET", "SH"))) {
    oti_stop("A cell stage requires a valid --family and --target.")
  }
} else if (nzchar(execute_stage) || nzchar(execute_family) ||
           nzchar(execute_target)) {
  oti_stop("Internal execute-stage arguments are valid only in execute mode.")
}

repo_root <- normalizePath(file.path(script_dir, "..", ".."), mustWork = TRUE)
setwd(repo_root)
source(file.path(script_dir, "49_oracle_tilt_campaign_gate.R"))
otcg_assert_action(repo_root, "publication_v3", mode)
config_path <- normalizePath(
  arg_value(
    "--config=",
    file.path(
      repo_root, "application", "config",
      "oracle_tilt_c095_publication_v3_20260801.json"
    )
  ),
  winslash = "/", mustWork = TRUE
)
config <- oti_read_json(config_path)
otv3_validate_config(config)

for (package in c("rqrgibbs", "jsonlite")) {
  if (!requireNamespace(package, quietly = TRUE)) {
    oti_stop("The ", package, " package is required.")
  }
}
if (identical(mode, "execute") &&
    !requireNamespace("posterior", quietly = TRUE)) {
  oti_stop("The posterior package is required for maintained diagnostics.")
}

atomic_json <- function(value, path) {
  oti_ensure_dir(dirname(path))
  temporary <- tempfile(paste0(".", basename(path), "-"), dirname(path))
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  jsonlite::write_json(
    value, temporary, pretty = TRUE, auto_unbox = TRUE,
    digits = NA, null = "null", na = "null"
  )
  if (!file.rename(temporary, path)) oti_stop("Atomic JSON write failed: ", path)
  invisible(path)
}

expected_commit <- tolower(Sys.getenv("RQR_EXPECTED_PRIMARY_COMMIT", ""))
attestation_path <- Sys.getenv("RQR_PRIMARY_RUNTIME_ATTESTATION", "")
exact_runtime_requested <- nzchar(expected_commit) || nzchar(attestation_path)
if (exact_runtime_requested &&
    (!grepl("^[0-9a-f]{40}$", expected_commit) || !nzchar(attestation_path))) {
  oti_stop("A runtime binding requires a full source SHA and attestation.")
}
require_exact_runtime <- mode %in% c("benchmark", "execute")
if (require_exact_runtime && !exact_runtime_requested) {
  oti_stop(mode, " requires a full source SHA and isolated-runtime attestation.")
}

provenance_control <- list()
runtime_binding <- list(
  schema_version = "rqrgibbs_oracle_tilt_runtime_binding/3.0.0",
  binding_kind = "exploratory_installed_namespace",
  runtime_source_match = FALSE,
  promotion_eligible = FALSE,
  package_version = as.character(utils::packageVersion("rqrgibbs")),
  runtime_tree_digest = NA_character_,
  expected_commit = if (nzchar(expected_commit)) expected_commit else NA_character_
)
if (exact_runtime_requested) {
  source(file.path(
    repo_root, "application", "scripts", "lib", "isolated_runtime_lineage.R"
  ))
  source(file.path(
    repo_root, "application", "scripts", "lib", "rqr_dlm_main_simulation.R"
  ))
  source(file.path(
    repo_root, "application", "scripts", "lib",
    "rqr_dlm_confirmatory_simulation.R"
  ))
  runtime_binding <- rqr_main_primary_runtime_binding(
    repo_root, expected_commit, attestation_path
  )
  provenance_control <- rqr_confirm_primary_provenance_control(
    repo_root, expected_commit, attestation_path
  )
  if (!isTRUE(runtime_binding$match)) {
    oti_stop("The isolated primary runtime does not match the reviewed source.")
  }
}
runtime_match <- isTRUE(runtime_binding$match) ||
  isTRUE(runtime_binding$runtime_source_match)

confirmation <- switch(
  mode,
  benchmark = c("RQR_ORACLE_TILT_V3_BENCHMARK_CONFIRM", "YES"),
  execute = c("RQR_ORACLE_TILT_V3_CONFIRM", "YES"),
  NULL
)
if (length(confirmation) &&
    !identical(Sys.getenv(confirmation[1L]), confirmation[2L])) {
  oti_stop(
    mode, " is fail-closed; set ", confirmation[1L], "=",
    confirmation[2L], " only after review."
  )
}
if (identical(mode, "execute") && !isTRUE(config$execution_authorized)) {
  oti_stop("execute is disabled in the tracked configuration.")
}

default_output <- file.path(
  repo_root, "application", "outputs", "oracle_tilt_c095_publication_v3",
  paste0(mode, "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
)
output_root <- normalizePath(
  arg_value("--output-dir=", default_output), winslash = "/", mustWork = FALSE
)
existing_entries <- if (dir.exists(output_root)) {
  list.files(output_root, all.files = TRUE, no.. = TRUE)
} else character(0)
wrapper_placeholders <- c(
  "process_group_monitor.csv", "runner.stdout.log", "runner.stderr.log",
  "process_lifecycle.csv", "current_stage.csv",
  "orchestrator_failure_log.csv"
)
unexpected_entries <- setdiff(existing_entries, wrapper_placeholders)
resume <- identical(mode, "execute") && length(unexpected_entries) > 0L
if (dir.exists(output_root) && file.exists(file.path(output_root, "closeout.json"))) {
  oti_stop("The requested output root is already closed.")
}
if (dir.exists(output_root) && !resume && length(unexpected_entries) > 0L) {
  oti_stop("Non-execution modes require a fresh output directory.")
}
oti_ensure_dir(output_root)
worker_root <- oti_ensure_dir(file.path(output_root, "worker_results"))

source_commit <- if (nzchar(expected_commit)) expected_commit else
  oti_git_state(repo_root)$commit
config_sha256 <- oti_file_sha256(config_path)
runtime_digest <- runtime_binding$runtime_tree_digest %||% NA_character_
source_state <- list(
  schema_version = otv3_schema(), mode = mode,
  started_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  repository = oti_git_state(repo_root), source_commit = source_commit,
  config_sha256 = config_sha256,
  runtime_tree_digest = runtime_digest,
  exact_runtime_bound = runtime_match,
  resumed = resume,
  interpretation = paste(
    "Single-data interval-root generalized-posterior illustration;",
    "not a response likelihood, response-predictive analysis, or",
    "repeated-sample simulation study."
  )
)
if (resume) {
  prior_source <- jsonlite::read_json(
    file.path(output_root, "source_state.json"), simplifyVector = TRUE
  )
  checks <- c(
    identical(prior_source$source_commit, source_commit),
    identical(prior_source$config_sha256, config_sha256),
    identical(prior_source$runtime_tree_digest, runtime_digest)
  )
  if (!all(checks)) oti_stop("Resume source/config/runtime binding changed.")
} else {
  atomic_json(source_state, file.path(output_root, "source_state.json"))
  binding_public <- runtime_binding
  binding_public$runtime_path <- NULL
  atomic_json(binding_public, file.path(output_root, "runtime_binding.json"))
  atomic_json(config, file.path(output_root, "config.json"))
}

preflight <- otv3_design_preflight(config)
if (!preflight$pass) oti_stop("The v3 design preflight did not pass.")
authoritative_cell_plan <- otv3_cell_plan(preflight$plan)

write_preflight_artifacts <- function(root, preflight) {
  design <- data.frame(
    family = c("fixed_design", "dlm"),
    n_index = c(length(preflight$fixed_dgp$y), length(preflight$dlm_dgp$y)),
    n_observed = c(
      sum(preflight$fixed_dgp$observed), sum(preflight$dlm_dgp$observed)
    ),
    n_missing = c(0L, sum(!preflight$dlm_dgp$observed)),
    dgp_seed = c(preflight$fixed_dgp$seed, preflight$dlm_dgp$seed),
    dgp_digest = c(
      otf_object_sha256(preflight$fixed_dgp),
      otf_object_sha256(preflight$dlm_dgp)
    ),
    target_digest = c(
      otf_object_sha256(preflight$fixed_targets),
      otf_object_sha256(preflight$dlm_targets)
    ),
    stringsAsFactors = FALSE
  )
  basis <- preflight$fixed_dgp$basis
  basis_audit <- data.frame(
    rank = basis$rank,
    maximum_gram_absolute_error = max(abs(
      basis$gram - diag(ncol(basis$X))
    )),
    maximum_reconstruction_error = basis$maximum_reconstruction_error,
    minimum_row_norm = min(basis$row_norm),
    center_row_norm = basis$row_norm[which.min(abs(preflight$fixed_dgp$x))],
    maximum_row_norm = max(basis$row_norm)
  )
  transform <- as.data.frame(basis$transform)
  transform$raw_term <- rownames(basis$transform)
  transform <- transform[, c("raw_term", setdiff(names(transform), "raw_term"))]
  time_contract <- data.frame(
    T = length(preflight$dlm_dgp$time),
    delta = preflight$dlm_dgp$delta,
    time_min = min(preflight$dlm_dgp$time),
    time_max = max(preflight$dlm_dgp$time),
    n_missing = sum(!preflight$dlm_dgp$observed),
    n_observed = sum(preflight$dlm_dgp$observed),
    missing_indices = paste(preflight$dlm_dgp$missing_times, collapse = ";"),
    stringsAsFactors = FALSE
  )
  values <- list(
    design_contract.csv = design,
    oracle_targets.csv = preflight$oracle,
    tail_information.csv = preflight$tail_information,
    static_basis_audit.csv = basis_audit,
    static_basis_transform.csv = transform,
    static_projection_audit.csv = preflight$projection_audit,
    static_initialization_audit.csv =
      preflight$fixed_initialization_audit,
    dynamic_projection_audit.csv = preflight$dynamic_projection_audit,
    static_prior_predictive.csv = preflight$static_prior_audit,
    dlm_prior_predictive.csv = preflight$dlm_prior_audit,
    seasonal_prior_predictive.csv = preflight$seasonal_prior_audit,
    dlm_time_contract.csv = time_contract,
    fixed_horizon_audit.csv = preflight$fixed_horizon_audit,
    seasonal_covariance_audit.csv = preflight$seasonal_covariance_audit,
    dynamic_observability_audit.csv = preflight$observability_audit,
    scale_information.csv = preflight$scale_information,
    fit_plan.csv = preflight$plan,
    preflight_gates.csv = preflight$gates
  )
  for (name in names(values)) {
    otf_atomic_write_csv(values[[name]], file.path(root, name))
  }
  invisible(values)
}
if (!resume) write_preflight_artifacts(output_root, preflight)

close_and_manifest <- function(extra) {
  closeout <- c(list(
    schema_version = otv3_schema(), mode = mode,
    source_commit = source_commit, config_sha256 = config_sha256,
    runtime_tree_digest = runtime_digest,
    exact_runtime_bound = runtime_match,
    finished_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    exact_population_oracle_tilts = TRUE, cornish_fisher_used = FALSE,
    response_predictive_analysis = FALSE, simulation_study = FALSE,
    manuscript_promotion_authorized = FALSE
  ), extra)
  atomic_json(closeout, file.path(output_root, "closeout.json"))
  compact <- file.path(output_root, otv3_compact_files())
  compact <- compact[file.exists(compact)]
  manifest <- oti_file_hashes(compact, output_root)
  names(manifest)[names(manifest) == "relative_path"] <- "path"
  otf_atomic_write_csv(manifest, file.path(output_root, "artifact_manifest.csv"))
  otp_verify_manifest(output_root)
  invisible(closeout)
}

if (identical(mode, "preflight")) {
  close_and_manifest(list(
    pass = TRUE, planned_chains = nrow(preflight$plan), completed_chains = 0L,
    compact_evidence_eligible = runtime_match
  ))
  message("[oracle-tilt-v3] preflight passed: ", output_root)
  quit(save = "no", status = 0L)
}

if (identical(mode, "reference-only")) {
  reference <- otv3_reference_suite(config)
  otf_atomic_write_csv(reference, file.path(output_root, "reference_gates.csv"))
  passed <- nrow(reference) == 24L && all(reference$pass)
  close_and_manifest(list(
    pass = passed, reference_gates = nrow(reference),
    reference_gates_passed = sum(reference$pass), completed_chains = 0L,
    compact_evidence_eligible = passed &&
      runtime_match
  ))
  if (!passed) oti_stop("One or more conditional-reference gates failed.")
  message("[oracle-tilt-v3] reference suite passed: ", output_root)
  quit(save = "no", status = 0L)
}

verify_input_bundle <- function(path, expected_mode) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  otp_verify_manifest(path)
  wrapper_files <- c(
    "resource_summary.csv", "wrapper_closeout.csv",
    "wrapper_artifact_manifest.csv"
  )
  if (any(!file.exists(file.path(path, wrapper_files)))) {
    oti_stop("Input bundle lacks monitored-wrapper evidence: ", expected_mode)
  }
  resource <- utils::read.csv(
    file.path(path, "resource_summary.csv"), stringsAsFactors = FALSE
  )
  wrapper <- utils::read.csv(
    file.path(path, "wrapper_closeout.csv"), stringsAsFactors = FALSE
  )
  if (nrow(resource) != 1L || nrow(wrapper) != 1L ||
      !isTRUE(resource$pass) || !isTRUE(resource$final_pgid_empty) ||
      !isTRUE(wrapper$wrapper_pass) || resource$runner_status != 0L ||
      wrapper$runner_status != 0L) {
    oti_stop("Input bundle failed its monitored-wrapper gate: ", expected_mode)
  }
  wrapper_hashes <- utils::read.csv(
    file.path(path, "wrapper_artifact_manifest.csv"),
    stringsAsFactors = FALSE
  )
  wrapper_hash_pass <- vapply(seq_len(nrow(wrapper_hashes)), function(index) {
    artifact <- file.path(path, wrapper_hashes$path[index])
    file.exists(artifact) && !dir.exists(artifact) &&
      unname(file.info(artifact)$size) == wrapper_hashes$bytes[index] &&
      identical(oti_file_sha256(artifact), wrapper_hashes$sha256[index])
  }, logical(1L))
  if (!nrow(wrapper_hashes) || !all(wrapper_hash_pass)) {
    oti_stop("Input wrapper-artifact manifest failed: ", expected_mode)
  }
  closeout <- jsonlite::read_json(
    file.path(path, "closeout.json"), simplifyVector = TRUE
  )
  checks <- c(
    identical(closeout$mode, expected_mode), isTRUE(closeout$pass),
    identical(closeout$source_commit, source_commit),
    identical(closeout$config_sha256, config_sha256),
    identical(closeout$runtime_tree_digest, runtime_digest),
    isTRUE(closeout$exact_runtime_bound),
    isTRUE(closeout$compact_evidence_eligible)
  )
  if (!all(checks)) oti_stop("Input bundle failed binding: ", expected_mode)
  data.frame(
    mode = expected_mode, path = path,
    closeout_sha256 = oti_file_sha256(file.path(path, "closeout.json")),
    artifact_manifest_sha256 =
      oti_file_sha256(file.path(path, "artifact_manifest.csv")),
    wrapper_artifact_manifest_sha256 =
      oti_file_sha256(file.path(path, "wrapper_artifact_manifest.csv")),
    source_commit = source_commit, config_sha256 = config_sha256,
    runtime_tree_digest = runtime_digest, stringsAsFactors = FALSE
  )
}

required_bundles <- if (identical(mode, "benchmark")) {
  c(preflight = "RQR_ORACLE_TILT_V3_PREFLIGHT_DIR",
    `reference-only` = "RQR_ORACLE_TILT_V3_REFERENCE_DIR")
} else {
  c(preflight = "RQR_ORACLE_TILT_V3_PREFLIGHT_DIR",
    `reference-only` = "RQR_ORACLE_TILT_V3_REFERENCE_DIR",
    benchmark = "RQR_ORACLE_TILT_V3_BENCHMARK_DIR")
}
bundle_bindings <- lapply(names(required_bundles), function(bundle_mode) {
  variable <- unname(required_bundles[[bundle_mode]])
  value <- Sys.getenv(variable, "")
  if (!nzchar(value)) oti_stop(variable, " is required for ", mode, ".")
  verify_input_bundle(value, bundle_mode)
})
bundle_bindings <- do.call(rbind, bundle_bindings)
otf_atomic_write_csv(
  bundle_bindings, file.path(output_root, "input_bundle_binding.csv")
)
reference_row <- bundle_bindings$mode == "reference-only"
reference_file <- file.path(
  bundle_bindings$path[reference_row], "reference_gates.csv"
)
if (length(reference_file) != 1L || !file.exists(reference_file)) {
  oti_stop("The bound reference bundle lacks reference_gates.csv.")
}
otf_atomic_write_csv(
  utils::read.csv(reference_file, stringsAsFactors = FALSE),
  file.path(output_root, "reference_gates.csv")
)
if (identical(mode, "execute")) {
  benchmark_row <- bundle_bindings$mode == "benchmark"
  benchmark_file <- file.path(
    bundle_bindings$path[benchmark_row], "benchmark_summary.csv"
  )
  if (length(benchmark_file) != 1L || !file.exists(benchmark_file)) {
    oti_stop("The bound benchmark bundle lacks benchmark_summary.csv.")
  }
  otf_atomic_write_csv(
    utils::read.csv(benchmark_file, stringsAsFactors = FALSE),
    file.path(output_root, "benchmark_summary.csv")
  )
}

if (identical(mode, "benchmark")) {
  specifications <- list(
    list(family = "fixed_design", target = config$benchmark$fixed_design_target),
    list(family = "dlm", target = config$benchmark$dlm_target)
  )
  rows <- lapply(specifications, function(specification) {
    started <- Sys.time()
    result <- if (identical(specification$family, "fixed_design")) {
      otv3_fixed_chain(
        config, preflight$fixed_dgp, preflight$fixed_targets,
        specification$target, as.integer(config$benchmark$chain),
        provenance_control,
        preflight$fixed_initializations[[specification$target]]$profiles
      )
    } else {
      otv3_dlm_chain(
        config, preflight$dlm_dgp, preflight$dlm_targets,
        specification$target, as.integer(config$benchmark$chain),
        provenance_control
      )
    }
    path <- file.path(
      worker_root, paste0(specification$family, "_",
                          tolower(specification$target), "_benchmark.rds")
    )
    otf_atomic_save_rds(result, path, compress = FALSE)
    assessment <- otv3_benchmark_assessment(
      family = specification$family,
      target = specification$target,
      result = result,
      dgp = if (identical(specification$family, "fixed_design")) {
        preflight$fixed_dgp
      } else {
        preflight$dlm_dgp
      },
      targets = if (identical(specification$family, "fixed_design")) {
        preflight$fixed_targets
      } else {
        preflight$dlm_targets
      },
      config = config
    )
    cbind(data.frame(
      family = specification$family, target = specification$target,
      chain = as.integer(config$benchmark$chain),
      elapsed_seconds = as.numeric(difftime(Sys.time(), started, units = "secs")),
      worker_bytes = file.info(path)$size,
      numerical_repair_count = result$chain_summary$numerical_repair_count,
      promotion_eligible = result$chain_summary$promotion_eligible,
      worker_sha256 = oti_file_sha256(path), stringsAsFactors = FALSE
    ), assessment[, setdiff(names(assessment), c("family", "target")),
                   drop = FALSE])
  })
  benchmark <- do.call(rbind, rows)
  benchmark$pass <- with(
    benchmark,
    elapsed_seconds <= config$benchmark$maximum_elapsed_seconds &
      worker_bytes <= config$benchmark$maximum_worker_bytes &
      numerical_repair_count == 0L & promotion_eligible &
      gross_recovery_pass & pathology_pass & heterogeneity_pass
  )
  otf_atomic_write_csv(benchmark, file.path(output_root, "benchmark_summary.csv"))
  passed <- nrow(benchmark) == 2L && all(benchmark$pass)
  close_and_manifest(list(
    pass = passed, benchmark_cells = nrow(benchmark),
    benchmark_cells_passed = sum(benchmark$pass), completed_chains = 2L,
    compact_evidence_eligible = passed
  ))
  if (!passed) oti_stop("The representative benchmark did not pass.")
  message("[oracle-tilt-v3] representative benchmark passed: ", output_root)
  quit(save = "no", status = 0L)
}

worker_contract_base <- list(
  schema_version = otv3_worker_schema(), source_commit = source_commit,
  config_sha256 = config_sha256, runtime_tree_digest = runtime_digest,
  package_version = runtime_binding$package_version,
  coverage_level = config$coverage_level,
  learning_rate = config$learning_rate,
  prediction_storage_contract = "ordered_endpoints_only"
)

cell_components <- c(
  "fit_summary", "fit_curves", "endpoint_error_density",
  "endpoint_error_summary", "endpoint_error_by_index", "chain_summary",
  "provenance_audit", "mcmc_diagnostics", "conditional_parity",
  "pathology_summary",
  "recovery_summary", "heterogeneity_summary"
)

cell_key <- function(family, target) {
  paste(family, tolower(target), sep = "_")
}

cell_directory <- function(family, target) {
  file.path(output_root, "cells", cell_key(family, target))
}

cell_dgp <- function(family) {
  if (identical(family, "fixed_design")) preflight$fixed_dgp else
    preflight$dlm_dgp
}

cell_targets <- function(family) {
  if (identical(family, "fixed_design")) preflight$fixed_targets else
    preflight$dlm_targets
}

expected_worker_contract <- function(family, target, chain) {
  selected <- preflight$plan$family == family &
    preflight$plan$target == target & preflight$plan$chain == chain
  if (sum(selected) != 1L) oti_stop("Worker plan selection is not unique.")
  c(worker_contract_base, list(
    family = family, target = target, chain = as.integer(chain),
    profile = preflight$plan$profile[selected][1L],
    seed = as.integer(preflight$plan$seed[selected][1L]),
    dgp_digest = otf_object_sha256(cell_dgp(family)),
    target_digest = otf_object_sha256(cell_targets(family))
  ))
}

validate_worker_envelope <- function(envelope, contract, path) {
  tryCatch(
    otv3_validate_worker_artifact(envelope, contract),
    error = function(error) oti_stop(
      "Worker artifact contract failed: ", path, "; ",
      conditionMessage(error)
    )
  )
  invisible(envelope)
}

run_worker <- function(family, target, chain) {
  contract <- expected_worker_contract(family, target, chain)
  digest <- otf_object_sha256(contract)
  path <- file.path(
    worker_root,
    sprintf("%s_%s_chain%02d.rds", family, tolower(target), chain)
  )
  if (file.exists(path)) {
    existing <- tryCatch(readRDS(path), error = function(error) NULL)
    validate_worker_envelope(existing, contract, path)
    return(list(path = path, resumed = TRUE))
  }
  entry_provenance <- otv3_live_provenance_snapshot(
    provenance_control, "worker_entry", family, target, chain
  )
  if (!isTRUE(entry_provenance$snapshot_pass)) {
    oti_stop(
      "Worker-entry provenance failed: ",
      entry_provenance$failed_gates
    )
  }
  result <- if (identical(family, "fixed_design")) {
    otv3_fixed_chain(
      config, cell_dgp(family), cell_targets(family), target, chain,
      provenance_control, preflight$fixed_initializations[[target]]$profiles
    )
  } else {
    otv3_dlm_chain(
      config, cell_dgp(family), cell_targets(family), target, chain,
      provenance_control
    )
  }
  exit_provenance <- otv3_live_provenance_snapshot(
    provenance_control, "worker_exit", family, target, chain
  )
  result$provenance_audit <- oti_rbind_fill(list(
    entry_provenance, result$provenance_audit, exit_provenance
  ))
  otv3_validate_provenance_audit(
    result$provenance_audit, family, target, chain
  )
  identity_fields <- c(
    "git_commit", "expected_git_commit", "runtime_package_path",
    "runtime_package_version", "runtime_attestation",
    "runtime_attestation_schema", "source_tree_digest",
    "runtime_package_tree_digest"
  )
  phase_identity_match <- all(vapply(identity_fields, function(name) {
    value <- as.character(result$provenance_audit[[name]])
    !anyNA(value) && all(nzchar(value)) && length(unique(value)) == 1L
  }, logical(1L)))
  phase_pass <- setNames(
    result$provenance_audit$snapshot_pass,
    result$provenance_audit$phase
  )
  result$chain_summary$worker_entry_provenance_match <-
    phase_identity_match && isTRUE(phase_pass[["worker_entry"]])
  result$chain_summary$fit_recorded_provenance_match <-
    phase_identity_match && isTRUE(phase_pass[["fit_recorded"]])
  result$chain_summary$worker_exit_provenance_match <-
    phase_identity_match && isTRUE(phase_pass[["worker_exit"]])
  result <- otv3_compact_chain_result(result)
  envelope <- list(
    schema_version = otv3_worker_schema(), contract = contract,
    contract_digest = digest, result = result
  )
  validate_worker_envelope(envelope, contract, path)
  otf_atomic_save_rds(envelope, path, compress = FALSE)
  list(path = path, resumed = FALSE)
}

append_failure <- function(family, target, chain, stage, message) {
  path <- file.path(output_root, "failure_log.csv")
  otv3_append_failure(
    path, family, target, chain, stage, as.character(message)
  )
}

process_peak_rss_kib <- function() {
  status <- tryCatch(readLines("/proc/self/status", warn = FALSE),
                     error = function(error) character(0))
  line <- status[startsWith(status, "VmHWM:")]
  if (!length(line)) return(NA_real_)
  as.numeric(sub("^VmHWM:[[:space:]]*([0-9]+).*", "\\1", line[1L]))
}

cell_contract_from_manifest <- function(family, target, manifest) {
  otv3_build_cell_contract(
    source_commit, config_sha256, runtime_digest, family, target, manifest
  )
}

validate_cell_bundle <- function(family, target, require_eligible = TRUE) {
  root <- cell_directory(family, target)
  required <- c(
    "cell_receipt.json", "worker_manifest.csv", "fit_summary.csv",
    "artifact_manifest.csv"
  )
  if (!dir.exists(root) || any(!file.exists(file.path(root, required)))) {
    oti_stop("Cell bundle is incomplete: ", family, "/", target)
  }
  otp_verify_manifest(root)
  receipt <- jsonlite::read_json(
    file.path(root, "cell_receipt.json"), simplifyVector = TRUE
  )
  manifest <- utils::read.csv(
    file.path(root, "worker_manifest.csv"), stringsAsFactors = FALSE
  )
  expected_rows <- preflight$plan$family == family &
    preflight$plan$target == target
  expected_chains <- as.integer(preflight$plan$chain[expected_rows])
  checks <- c(
    identical(receipt$schema_version, otv3_cell_schema()),
    identical(receipt$source_commit, source_commit),
    identical(receipt$config_sha256, config_sha256),
    identical(receipt$runtime_tree_digest, runtime_digest),
    identical(receipt$family, family), identical(receipt$target, target),
    identical(as.integer(manifest$chain), expected_chains),
    nrow(manifest) == length(expected_chains), !anyDuplicated(manifest$path),
    identical(receipt$prediction_storage_contract,
              "ordered_endpoints_only")
  )
  worker_checks <- vapply(seq_len(nrow(manifest)), function(index) {
    path <- file.path(output_root, manifest$path[index])
    if (!file.exists(path) || dir.exists(path) ||
        unname(file.info(path)$size) != manifest$bytes[index] ||
        !identical(oti_file_sha256(path), manifest$sha256[index])) {
      return(FALSE)
    }
    envelope <- tryCatch(readRDS(path), error = function(error) NULL)
    contract <- expected_worker_contract(
      family, target, as.integer(manifest$chain[index])
    )
    tryCatch({
      validate_worker_envelope(envelope, contract, path)
      identical(envelope$contract_digest, manifest$contract_digest[index])
    }, error = function(error) FALSE)
  }, logical(1L))
  contract <- cell_contract_from_manifest(family, target, manifest)
  checks <- c(
    checks, all(worker_checks),
    identical(receipt$cell_contract_digest, otf_object_sha256(contract)),
    !require_eligible || isTRUE(receipt$eligible)
  )
  if (!all(checks)) oti_stop("Cell bundle validation failed: ", family, "/", target)
  list(root = root, receipt = receipt, worker_manifest = manifest)
}

refresh_run_status <- function() {
  rows <- list()
  for (plan_index in seq_len(nrow(authoritative_cell_plan))) {
      family <- authoritative_cell_plan$family[plan_index]
      target <- authoritative_cell_plan$target[plan_index]
      root <- cell_directory(family, target)
      receipt_path <- file.path(root, "cell_receipt.json")
      receipt <- if (file.exists(receipt_path)) {
        jsonlite::read_json(receipt_path, simplifyVector = TRUE)
      } else NULL
      rows[[length(rows) + 1L]] <- data.frame(
        family = family, target = target,
        status = if (is.null(receipt)) "pending" else "completed",
        chains_completed = if (is.null(receipt)) 0L else
          as.integer(receipt$completed_chains),
        elapsed_seconds = if (is.null(receipt)) 0 else
          as.numeric(receipt$elapsed_seconds),
        computational_pass = if (is.null(receipt)) NA else
          isTRUE(receipt$computational_pass),
        recovery_pass = if (is.null(receipt)) NA else
          isTRUE(receipt$recovery_pass),
        disposition = if (is.null(receipt)) "pending" else
          as.character(receipt$disposition),
        stringsAsFactors = FALSE
      )
  }
  status <- do.call(rbind, rows)
  otf_atomic_write_csv(status, file.path(output_root, "run_status.csv"))
  status
}

write_cell_bundle <- function(family, target, cell, manifest, elapsed) {
  final <- cell_directory(family, target)
  if (dir.exists(final)) oti_stop("Cell bundle already exists: ", final)
  cells_root <- oti_ensure_dir(file.path(output_root, "cells"))
  stage <- tempfile(paste0(".", cell_key(family, target), "-"), cells_root)
  if (!dir.create(stage, recursive = TRUE, showWarnings = FALSE)) {
    oti_stop("Could not create the cell staging directory.")
  }
  on.exit(unlink(stage, recursive = TRUE, force = TRUE), add = TRUE)
  for (name in cell_components) {
    value <- cell[[name]]
    if (is.data.frame(value) && nrow(value)) {
      otf_atomic_write_csv(value, file.path(stage, paste0(name, ".csv")))
    }
  }
  otf_atomic_write_csv(manifest, file.path(stage, "worker_manifest.csv"))
  cell_contract <- cell_contract_from_manifest(family, target, manifest)
  eligible <- isTRUE(
    cell$fit_summary$manuscript_illustration_evidence_eligible
  )
  receipt <- list(
    schema_version = otv3_cell_schema(), source_commit = source_commit,
    config_sha256 = config_sha256, runtime_tree_digest = runtime_digest,
    family = family, target = target,
    expected_chains = nrow(manifest), completed_chains = nrow(manifest),
    elapsed_seconds = elapsed,
    cell_parent_peak_rss_kib = process_peak_rss_kib(),
    computational_pass = isTRUE(cell$fit_summary$computational_pass),
    recovery_pass = isTRUE(cell$fit_summary$recovery_pass),
    heterogeneity_pass = isTRUE(cell$fit_summary$heterogeneity_pass),
    disposition = as.character(cell$fit_summary$disposition),
    eligible = eligible,
    prediction_storage_contract = "ordered_endpoints_only",
    cell_contract_digest = otf_object_sha256(cell_contract),
    finished_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
  )
  atomic_json(receipt, file.path(stage, "cell_receipt.json"))
  files <- list.files(stage, full.names = TRUE, recursive = TRUE)
  hashes <- oti_file_hashes(files, stage)
  names(hashes)[names(hashes) == "relative_path"] <- "path"
  otf_atomic_write_csv(hashes, file.path(stage, "artifact_manifest.csv"))
  otp_verify_manifest(stage)
  if (!file.rename(stage, final)) oti_stop("Atomic cell publication failed.")
  on.exit(NULL, add = FALSE)
  validate_cell_bundle(family, target, require_eligible = FALSE)
}

if (identical(execute_stage, "prepare")) {
  otf_atomic_write_csv(
    authoritative_cell_plan, file.path(output_root, "cell_plan.csv")
  )
  refresh_run_status()
  message("[oracle-tilt-v3] process-isolated execution prepared: ", output_root)
  quit(save = "no", status = 0L)
}

if (identical(execute_stage, "cell")) {
  family <- execute_family
  target <- execute_target
  final <- cell_directory(family, target)
  if (dir.exists(final)) {
    validate_cell_bundle(family, target)
    refresh_run_status()
    message("[oracle-tilt-v3] validated existing cell: ", family, "/", target)
    quit(save = "no", status = 0L)
  }
  rows <- preflight$plan$family == family & preflight$plan$target == target
  chains <- as.integer(preflight$plan$chain[rows])
  workers <- min(as.integer(config[[family]]$workers), length(chains))
  message(
    "[oracle-tilt-v3] isolated cell ", family, "/", target, ": ",
    length(chains), " chains; workers=", workers
  )
  started <- Sys.time()
  run_one <- function(chain) {
    tryCatch(
      run_worker(family, target, chain),
      error = function(error) structure(
        list(message = conditionMessage(error)), class = "otv3_worker_error"
      )
    )
  }
  batches <- otv3_chain_batches(chains, workers)
  returns <- unlist(lapply(batches, function(batch) {
    if (length(batch) > 1L && .Platform$OS.type != "windows") {
      parallel::mclapply(
        batch, run_one, mc.cores = length(batch), mc.preschedule = TRUE,
        mc.set.seed = FALSE
      )
    } else lapply(batch, run_one)
  }), recursive = FALSE, use.names = FALSE)
  failed <- which(vapply(
    returns, inherits, logical(1L), what = "otv3_worker_error"
  ))
  if (length(failed)) {
    for (index in failed) {
      append_failure(
        family, target, chains[index], "fit", returns[[index]]$message
      )
    }
    oti_stop("A chain failed; later cells were not launched.")
  }
  paths <- vapply(returns, `[[`, character(1L), "path")
  envelopes <- Map(function(path, chain) {
    envelope <- readRDS(path)
    validate_worker_envelope(
      envelope, expected_worker_contract(family, target, chain), path
    )
    envelope
  }, paths, chains)
  results <- lapply(envelopes, `[[`, "result")
  manifest <- data.frame(
    family = family, target = target, chain = chains,
    profile = preflight$plan$profile[rows],
    seed = as.integer(preflight$plan$seed[rows]),
    resumed = vapply(returns, `[[`, logical(1L), "resumed"),
    path = file.path("worker_results", basename(paths)),
    bytes = unname(file.info(paths)$size),
    sha256 = vapply(paths, oti_file_sha256, character(1L)),
    contract_digest = vapply(
      envelopes, `[[`, character(1L), "contract_digest"
    ),
    storage_contract = "ordered_endpoints_only",
    stringsAsFactors = FALSE
  )
  cell <- otv3_summarize_cell(
    family, target, results, cell_dgp(family), cell_targets(family), config
  )
  elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  write_cell_bundle(family, target, cell, manifest, elapsed)
  refresh_run_status()
  eligible <- isTRUE(
    cell$fit_summary$manuscript_illustration_evidence_eligible
  )
  rm(envelopes, results, cell, returns)
  invisible(gc(full = TRUE))
  if (!eligible) {
    append_failure(
      family, target, NA_integer_, "cell_gate",
      "The completed process-isolated cell did not pass all gates."
    )
    oti_stop("Cell gates failed; later cells were not launched.")
  }
  message("[oracle-tilt-v3] isolated cell passed: ", family, "/", target)
  quit(save = "no", status = 0L)
}

cell_bundles <- list()
cell_manifest_rows <- list()
for (plan_index in seq_len(nrow(authoritative_cell_plan))) {
    family <- authoritative_cell_plan$family[plan_index]
    target <- authoritative_cell_plan$target[plan_index]
    bundle <- validate_cell_bundle(family, target)
    cell_bundles[[paste(family, target, sep = "/")]] <- bundle
    cell_manifest_rows[[length(cell_manifest_rows) + 1L]] <- data.frame(
      family = family, target = target,
      path = file.path("cells", cell_key(family, target)),
      cell_receipt_sha256 = oti_file_sha256(
        file.path(bundle$root, "cell_receipt.json")
      ),
      artifact_manifest_sha256 = oti_file_sha256(
        file.path(bundle$root, "artifact_manifest.csv")
      ),
      cell_contract_digest = bundle$receipt$cell_contract_digest,
      eligible = isTRUE(bundle$receipt$eligible),
      stringsAsFactors = FALSE
    )
}

bind_cell_component <- function(name) {
  values <- lapply(cell_bundles, function(bundle) {
    path <- file.path(bundle$root, paste0(name, ".csv"))
    if (file.exists(path)) {
      utils::read.csv(path, stringsAsFactors = FALSE)
    } else data.frame()
  })
  values <- Filter(function(value) is.data.frame(value) && nrow(value), values)
  if (length(values)) oti_rbind_fill(values) else data.frame()
}

for (name in cell_components) {
  value <- bind_cell_component(name)
  if (nrow(value)) {
    otf_atomic_write_csv(value, file.path(output_root, paste0(name, ".csv")))
  }
}
fit_summary <- bind_cell_component("fit_summary")
cell_disposition <- fit_summary[, c(
  "family", "target", "provenance_pass", "provenance_snapshots_pass",
  "strict_diagnostics_pass",
  "conditional_parity_pass", "pathology_pass", "computational_pass",
  "recovery_pass", "heterogeneity_pass", "disposition",
  "manuscript_illustration_evidence_eligible"
)]
otf_atomic_write_csv(
  cell_disposition, file.path(output_root, "cell_disposition.csv")
)
worker_manifest <- do.call(
  rbind, lapply(cell_bundles, `[[`, "worker_manifest")
)
cell_manifest <- do.call(rbind, cell_manifest_rows)
otf_atomic_write_csv(worker_manifest, file.path(output_root, "worker_manifest.csv"))
otf_atomic_write_csv(cell_manifest, file.path(output_root, "cell_manifest.csv"))
run_status <- refresh_run_status()

all_completed <- nrow(worker_manifest) == nrow(preflight$plan) &&
  all(run_status$status == "completed")
all_passed <- nrow(fit_summary) == 6L &&
  all(fit_summary$manuscript_illustration_evidence_eligible) &&
  all(cell_manifest$eligible)
close_and_manifest(list(
  pass = all_completed && all_passed,
  planned_chains = nrow(preflight$plan), completed_chains = nrow(worker_manifest),
  all_chains_completed = all_completed, all_cells_pass = all_passed,
  passed_cells = sum(fit_summary$disposition == "strict_pass"),
  failed_cells = sum(fit_summary$disposition == "fail"),
  process_isolated_cells = TRUE,
  prediction_storage_contract = "ordered_endpoints_only",
  compact_evidence_eligible = all_completed && all_passed
))
if (!all_completed || !all_passed) oti_stop("The v3 execution did not pass.")
message("[oracle-tilt-v3] process-isolated execution passed: ", output_root)
