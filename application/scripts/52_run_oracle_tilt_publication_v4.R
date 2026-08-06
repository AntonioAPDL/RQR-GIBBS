#!/usr/bin/env Rscript

arguments <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_path <- sub(file_arg, "", arguments[startsWith(arguments, file_arg)][1L])
if (!length(script_path) || is.na(script_path)) {
  script_path <- "application/scripts/52_run_oracle_tilt_publication_v4.R"
}
script_path <- normalizePath(script_path, winslash = "/", mustWork = TRUE)
script_dir <- dirname(script_path)
source(file.path(script_dir, "32_oracle_tilt_illustration_utils.R"))
source(file.path(script_dir, "33_oracle_tilt_forensic_utils.R"))
source(file.path(script_dir, "34_oracle_tilt_publication_utils.R"))
source(file.path(script_dir, "42_oracle_tilt_publication_v3_utils.R"))
source(file.path(script_dir, "52_oracle_tilt_publication_v4_utils.R"))

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
execute_candidate <- tolower(arg_value("--candidate=", ""))
execute_family <- tolower(arg_value("--family=", ""))
execute_target <- toupper(arg_value("--target=", ""))
if (mode == "execute") {
  if (!execute_stage %in% c("prepare", "cell", "finalize")) {
    oti_stop("Execute must use the V4 orchestrator prepare/cell/finalize stages.")
  }
  if (execute_stage == "cell" &&
      (!execute_candidate %in% otv4_candidate_ids() ||
       !execute_family %in% c("fixed_design", "dlm") ||
       !execute_target %in% c("RQR", "ET", "SH"))) {
    oti_stop("A V4 cell requires a valid candidate, family, and target.")
  }
} else if (any(nzchar(c(
  execute_stage, execute_candidate, execute_family, execute_target
)))) {
  oti_stop("Execute-stage arguments are internal to V4 execute mode.")
}

repo_root <- normalizePath(file.path(script_dir, "..", ".."), mustWork = TRUE)
setwd(repo_root)
config_path <- normalizePath(
  arg_value(
    "--config=",
    file.path(
      repo_root, "application", "config",
      "oracle_tilt_c095_publication_v4_seed_screen_20260805.json"
    )
  ), winslash = "/", mustWork = TRUE
)
config <- oti_read_json(config_path)
otv4_validate_config(config)

for (package in c("rqrgibbs", "jsonlite", "digest")) {
  if (!requireNamespace(package, quietly = TRUE)) {
    oti_stop("The ", package, " package is required.")
  }
}
if (mode %in% c("benchmark", "execute") &&
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
if (mode %in% c("benchmark", "execute") && !exact_runtime_requested) {
  oti_stop(mode, " requires a full source SHA and isolated runtime.")
}

provenance_control <- list()
runtime_binding <- list(
  schema_version = "rqrgibbs_oracle_tilt_runtime_binding/4.0.0",
  binding_kind = "exploratory_installed_namespace",
  runtime_source_match = FALSE, match = FALSE,
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
    oti_stop("The isolated primary runtime does not match reviewed V4 source.")
  }
}
runtime_match <- isTRUE(runtime_binding$match) ||
  isTRUE(runtime_binding$runtime_source_match)

confirmation <- switch(
  mode,
  benchmark = c("RQR_ORACLE_TILT_V4_BENCHMARK_CONFIRM", "YES"),
  execute = c("RQR_ORACLE_TILT_V4_CONFIRM", "YES"),
  NULL
)
if (length(confirmation) &&
    !identical(Sys.getenv(confirmation[1L]), confirmation[2L])) {
  oti_stop(mode, " is fail-closed; set ", confirmation[1L], "=YES after review.")
}
if (mode == "execute" && !isTRUE(config$execution_authorized)) {
  oti_stop("V4 production execution is disabled in the tracked config.")
}

default_output <- file.path(
  repo_root, "application", "outputs",
  "oracle_tilt_c095_publication_v4_seed_screen",
  paste0(mode, "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
)
output_root <- normalizePath(
  arg_value("--output-dir=", default_output), winslash = "/", mustWork = FALSE
)
wrapper_placeholders <- c(
  "process_group_monitor.csv", "runner.stdout.log", "runner.stderr.log",
  "process_lifecycle.csv", "current_stage.csv", "orchestrator_failure_log.csv"
)
existing_entries <- if (dir.exists(output_root)) {
  list.files(output_root, all.files = TRUE, no.. = TRUE)
} else character(0)
unexpected_entries <- setdiff(existing_entries, wrapper_placeholders)
resume <- mode == "execute" && length(unexpected_entries) > 0L
if (dir.exists(output_root) && file.exists(file.path(output_root, "closeout.json"))) {
  oti_stop("The requested V4 output root is already closed.")
}
if (dir.exists(output_root) && !resume && length(unexpected_entries)) {
  oti_stop("Non-execution V4 modes require a fresh output directory.")
}
oti_ensure_dir(output_root)
worker_root <- oti_ensure_dir(file.path(output_root, "worker_results"))

source_commit <- if (nzchar(expected_commit)) expected_commit else
  oti_git_state(repo_root)$commit
config_sha256 <- oti_file_sha256(config_path)
runtime_digest <- runtime_binding$runtime_tree_digest %||% NA_character_
source_state <- list(
  schema_version = otv4_schema(), mode = mode,
  started_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  repository = oti_git_state(repo_root), source_commit = source_commit,
  config_sha256 = config_sha256, runtime_tree_digest = runtime_digest,
  exact_runtime_bound = runtime_match, resumed = resume,
  interpretation = paste(
    "Prospectively seed-screened single-data interval-root generalized",
    "posterior illustrations; not a response likelihood, response-predictive",
    "analysis, typical-performance claim, or repeated-sample simulation study."
  )
)
if (resume) {
  prior_source <- jsonlite::read_json(
    file.path(output_root, "source_state.json"), simplifyVector = TRUE
  )
  if (!all(c(
    identical(prior_source$source_commit, source_commit),
    identical(prior_source$config_sha256, config_sha256),
    identical(prior_source$runtime_tree_digest, runtime_digest)
  ))) oti_stop("V4 resume source/config/runtime binding changed.")
} else {
  atomic_json(source_state, file.path(output_root, "source_state.json"))
  public_binding <- runtime_binding
  public_binding$runtime_path <- NULL
  atomic_json(public_binding, file.path(output_root, "runtime_binding.json"))
  atomic_json(config, file.path(output_root, "config.json"))
}

close_and_manifest <- function(extra) {
  closeout <- c(list(
    schema_version = otv4_schema(), mode = mode,
    source_commit = source_commit, config_sha256 = config_sha256,
    runtime_tree_digest = runtime_digest, exact_runtime_bound = runtime_match,
    finished_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    candidate_count = 3L, planned_cells = 18L, planned_chains = 81L,
    exact_population_oracle_tilts = TRUE, cornish_fisher_used = FALSE,
    response_predictive_analysis = FALSE, simulation_study = FALSE,
    prospective_seed_screen = TRUE, typical_performance_claim = FALSE,
    manuscript_promotion_authorized = FALSE
  ), extra)
  atomic_json(closeout, file.path(output_root, "closeout.json"))
  compact <- file.path(output_root, otv4_compact_files())
  compact <- compact[file.exists(compact)]
  manifest <- oti_file_hashes(compact, output_root)
  names(manifest)[names(manifest) == "relative_path"] <- "path"
  otf_atomic_write_csv(manifest, file.path(output_root, "artifact_manifest.csv"))
  otp_verify_manifest(output_root)
  invisible(closeout)
}

with_candidate <- function(value, candidate_id) {
  if (!is.data.frame(value) || !nrow(value)) return(data.frame())
  cbind(candidate_id = candidate_id, value, stringsAsFactors = FALSE)
}

write_preflight_artifacts <- function(preflight) {
  otf_atomic_write_csv(
    preflight$seed_manifest,
    file.path(output_root, "candidate_seed_manifest.csv")
  )
  otf_atomic_write_csv(
    preflight$dgp_manifest, file.path(output_root, "dgp_manifest.csv")
  )
  otf_atomic_write_csv(preflight$plan, file.path(output_root, "fit_plan.csv"))
  otf_atomic_write_csv(
    preflight$cell_plan, file.path(output_root, "cell_plan.csv")
  )
  otf_atomic_write_csv(
    preflight$candidate_gates,
    file.path(output_root, "candidate_preflight_gates.csv")
  )
  otf_atomic_write_csv(
    preflight$cross_candidate_gates,
    file.path(output_root, "cross_candidate_gates.csv")
  )
  components <- list(
    oracle_targets = "oracle",
    tail_information = "tail_information",
    static_projection_audit = "projection_audit",
    dynamic_projection_audit = "dynamic_projection_audit",
    static_initialization_audit = "fixed_initialization_audit",
    scale_information = "scale_information",
    static_prior_predictive = "static_prior_audit",
    dlm_prior_predictive = "dlm_prior_audit",
    seasonal_prior_predictive = "seasonal_prior_audit",
    fixed_horizon_audit = "fixed_horizon_audit",
    seasonal_covariance_audit = "seasonal_covariance_audit",
    dynamic_observability_audit = "observability_audit"
  )
  for (output_name in names(components)) {
    member <- components[[output_name]]
    value <- do.call(rbind, lapply(preflight$candidates, function(candidate) {
      with_candidate(candidate[[member]], candidate$candidate_id)
    }))
    otf_atomic_write_csv(value, file.path(output_root, paste0(output_name, ".csv")))
  }
  design <- do.call(rbind, lapply(preflight$candidates, function(candidate) {
    data.frame(
      candidate_id = candidate$candidate_id,
      family = c("fixed_design", "dlm"),
      n_index = c(length(candidate$fixed_dgp$y), length(candidate$dlm_dgp$y)),
      n_observed = c(
        sum(candidate$fixed_dgp$observed), sum(candidate$dlm_dgp$observed)
      ),
      n_missing = c(0L, sum(!candidate$dlm_dgp$observed)),
      dgp_digest = c(
        otf_object_sha256(candidate$fixed_dgp),
        otf_object_sha256(candidate$dlm_dgp)
      ),
      target_digest = c(
        otf_object_sha256(candidate$fixed_targets),
        otf_object_sha256(candidate$dlm_targets)
      ), stringsAsFactors = FALSE
    )
  }))
  otf_atomic_write_csv(design, file.path(output_root, "design_contract.csv"))
}

needs_full_preflight <- mode %in% c("preflight", "reference-only", "benchmark") ||
  (mode == "execute" && execute_stage == "prepare")
preflight <- if (needs_full_preflight) otv4_design_preflight(config) else NULL
if (!is.null(preflight) && !preflight$pass) {
  oti_stop("The V4 design preflight did not pass.")
}
if (!resume && !is.null(preflight)) write_preflight_artifacts(preflight)

if (mode == "preflight") {
  close_and_manifest(list(
    pass = TRUE, completed_chains = 0L,
    compact_evidence_eligible = runtime_match
  ))
  message("[oracle-tilt-v4] three-candidate preflight passed: ", output_root)
  quit(save = "no", status = 0L)
}

if (mode == "reference-only") {
  reference <- otv3_reference_suite(otv4_v3_template(config))
  extra <- data.frame(
    gate = c(
      "v4_candidate_count", "v4_cell_count", "v4_chain_count",
      "v4_unique_chain_seeds", "v4_unique_dgp_streams"
    ),
    value = c(
      length(preflight$candidates), nrow(preflight$cell_plan),
      nrow(preflight$plan), length(unique(preflight$plan$seed)),
      length(unique(preflight$seed_manifest$state_digest))
    ),
    threshold = c(3, 18, 81, 81, 9), comparison = rep("==", 5L),
    stringsAsFactors = FALSE
  )
  extra$pass <- mapply(
    otv4_gate_pass, extra$value, extra$threshold, extra$comparison
  )
  reference <- rbind(reference, extra)
  otf_atomic_write_csv(reference, file.path(output_root, "reference_gates.csv"))
  passed <- nrow(reference) == 29L && all(reference$pass)
  close_and_manifest(list(
    pass = passed, reference_gates = nrow(reference),
    reference_gates_passed = sum(reference$pass), completed_chains = 0L,
    compact_evidence_eligible = passed && runtime_match
  ))
  if (!passed) oti_stop("One or more V4 reference gates failed.")
  message("[oracle-tilt-v4] reference suite passed: ", output_root)
  quit(save = "no", status = 0L)
}

verify_input_bundle <- function(path, expected_mode, require_wrapper = TRUE) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  otp_verify_manifest(path)
  closeout_path <- file.path(path, "closeout.json")
  if (!file.exists(closeout_path)) oti_stop("Input bundle lacks closeout: ", expected_mode)
  closeout <- jsonlite::read_json(closeout_path, simplifyVector = TRUE)
  checks <- c(
    identical(closeout$mode, expected_mode), isTRUE(closeout$pass),
    identical(closeout$source_commit, source_commit),
    identical(closeout$config_sha256, config_sha256),
    identical(closeout$runtime_tree_digest, runtime_digest),
    isTRUE(closeout$exact_runtime_bound),
    isTRUE(closeout$compact_evidence_eligible)
  )
  if (!all(checks)) oti_stop("Input bundle failed exact binding: ", expected_mode)
  if (require_wrapper) {
    resource_path <- file.path(path, "resource_summary.csv")
    wrapper_path <- file.path(path, "wrapper_closeout.csv")
    if (!file.exists(resource_path) || !file.exists(wrapper_path)) {
      oti_stop("Input bundle lacks monitored-wrapper evidence: ", expected_mode)
    }
    resource <- utils::read.csv(resource_path, stringsAsFactors = FALSE)
    wrapper <- utils::read.csv(wrapper_path, stringsAsFactors = FALSE)
    if (nrow(resource) != 1L || nrow(wrapper) != 1L ||
        !isTRUE(resource$pass) || !isTRUE(resource$final_pgid_empty) ||
        !isTRUE(wrapper$wrapper_pass)) {
      oti_stop("Input monitored-wrapper evidence failed: ", expected_mode)
    }
  }
  data.frame(
    mode = expected_mode, path = path,
    closeout_sha256 = oti_file_sha256(closeout_path),
    artifact_manifest_sha256 = oti_file_sha256(
      file.path(path, "artifact_manifest.csv")
    ), source_commit = source_commit, config_sha256 = config_sha256,
    runtime_tree_digest = runtime_digest, stringsAsFactors = FALSE
  )
}

required_bundles <- if (mode == "benchmark") {
  c(preflight = "RQR_ORACLE_TILT_V4_PREFLIGHT_DIR",
    `reference-only` = "RQR_ORACLE_TILT_V4_REFERENCE_DIR")
} else if (mode == "execute") {
  c(preflight = "RQR_ORACLE_TILT_V4_PREFLIGHT_DIR",
    `reference-only` = "RQR_ORACLE_TILT_V4_REFERENCE_DIR",
    benchmark = "RQR_ORACLE_TILT_V4_BENCHMARK_DIR",
    `resource-rehearsal` = "RQR_ORACLE_TILT_V4_RESOURCE_DIR")
} else character(0)
bundle_bindings <- if (length(required_bundles)) {
  do.call(rbind, lapply(names(required_bundles), function(bundle_mode) {
    variable <- unname(required_bundles[[bundle_mode]])
    value <- Sys.getenv(variable, "")
    if (!nzchar(value)) oti_stop(variable, " is required for ", mode, ".")
    verify_input_bundle(value, bundle_mode)
  }))
} else data.frame()
if (nrow(bundle_bindings)) {
  otf_atomic_write_csv(
    bundle_bindings, file.path(output_root, "input_bundle_binding.csv")
  )
  reference_path <- file.path(
    bundle_bindings$path[bundle_bindings$mode == "reference-only"],
    "reference_gates.csv"
  )
  if (length(reference_path) != 1L || !file.exists(reference_path)) {
    oti_stop("The bound V4 reference bundle is incomplete.")
  }
  otf_atomic_write_csv(
    utils::read.csv(reference_path, stringsAsFactors = FALSE),
    file.path(output_root, "reference_gates.csv")
  )
}

run_chain <- function(candidate_id, family, target, chain, dgp_envelope) {
  candidate_config <- otv4_candidate_config(config, candidate_id, target)
  RNGkind("L'Ecuyer-CMRG", "Inversion", "Rejection")
  if (family == "fixed_design") {
    otv3_fixed_chain(
      candidate_config, dgp_envelope$dgp, dgp_envelope$targets,
      target, chain, provenance_control,
      dgp_envelope$initializations[[target]]
    )
  } else {
    otv3_dlm_chain(
      candidate_config, dgp_envelope$dgp, dgp_envelope$targets,
      target, chain, provenance_control
    )
  }
}

if (mode == "benchmark") {
  candidate_id <- as.character(config$benchmark$candidate_id)
  candidate_preflight <- preflight$candidates[[candidate_id]]
  specifications <- list(
    list(family = "fixed_design", target = config$benchmark$fixed_design_target),
    list(family = "dlm", target = config$benchmark$dlm_target)
  )
  rows <- lapply(specifications, function(specification) {
    envelope <- otv4_dgp_envelope(
      candidate_preflight, specification$family,
      source_commit, config_sha256
    )
    started <- Sys.time()
    result <- run_chain(
      candidate_id, specification$family, specification$target,
      as.integer(config$benchmark$chain), envelope
    )
    path <- file.path(
      worker_root, paste0(candidate_id, "_", specification$family, "_",
                         tolower(specification$target), "_benchmark.rds")
    )
    otf_atomic_save_rds(result, path, compress = FALSE)
    assessment <- otv3_benchmark_assessment(
      specification$family, specification$target, result,
      envelope$dgp, envelope$targets,
      otv4_candidate_config(config, candidate_id, specification$target)
    )
    cbind(data.frame(
      candidate_id = candidate_id, family = specification$family,
      target = specification$target, chain = as.integer(config$benchmark$chain),
      elapsed_seconds = as.numeric(difftime(Sys.time(), started, units = "secs")),
      worker_bytes = unname(file.info(path)$size),
      numerical_repair_count = result$chain_summary$numerical_repair_count,
      promotion_eligible = result$chain_summary$promotion_eligible,
      worker_sha256 = oti_file_sha256(path), stringsAsFactors = FALSE
    ), assessment[, setdiff(names(assessment), c("family", "target")), drop = FALSE])
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
    compact_evidence_eligible = passed && runtime_match
  ))
  if (!passed) oti_stop("The representative V4 benchmark did not pass.")
  message("[oracle-tilt-v4] representative benchmark passed: ", output_root)
  quit(save = "no", status = 0L)
}

# Execute mode begins here.  Production is unreachable while the tracked
# configuration remains execution_authorized=false.
fit_plan <- otv4_plan(config)
cell_plan <- otv4_cell_plan(fit_plan)
dgp_root <- oti_ensure_dir(file.path(output_root, "dgp_objects"))
cells_root <- oti_ensure_dir(file.path(output_root, "cells"))

dgp_path <- function(candidate_id, family) {
  file.path(dgp_root, paste(candidate_id, family, sep = "_"), "dgp.rds")
}
read_dgp <- function(candidate_id, family) {
  path <- dgp_path(candidate_id, family)
  if (!file.exists(path)) oti_stop("Canonical V4 DGP object is missing: ", path)
  value <- readRDS(path)
  otv4_validate_dgp_envelope(
    value, candidate_id, family, source_commit, config_sha256
  )
  value
}
cell_directory <- function(candidate_id, family, target) {
  file.path(cells_root, otv4_cell_key(candidate_id, family, target))
}

if (execute_stage == "prepare") {
  write_preflight_artifacts(preflight)
  for (candidate in preflight$candidates) {
    for (family in c("fixed_design", "dlm")) {
      envelope <- otv4_dgp_envelope(
        candidate, family, source_commit, config_sha256
      )
      path <- dgp_path(candidate$candidate_id, family)
      if (file.exists(path)) {
        existing <- readRDS(path)
        otv4_validate_dgp_envelope(
          existing, candidate$candidate_id, family,
          source_commit, config_sha256
        )
        if (!identical(existing$contract_digest, envelope$contract_digest)) {
          oti_stop("Existing canonical V4 DGP digest changed.")
        }
      } else {
        oti_ensure_dir(dirname(path))
        otf_atomic_save_rds(envelope, path, compress = FALSE)
      }
    }
  }
  status <- cell_plan[, c("candidate_id", "family", "target", "order")]
  status$status <- "pending"
  status$chains_completed <- 0L
  otf_atomic_write_csv(status, file.path(output_root, "run_status.csv"))
  message("[oracle-tilt-v4] prepared 18 target-shared cells: ", output_root)
  quit(save = "no", status = 0L)
}

cell_components <- c(
  "fit_summary", "fit_curves", "endpoint_error_density",
  "endpoint_error_summary", "endpoint_error_by_index", "chain_summary",
  "provenance_audit", "mcmc_diagnostics", "conditional_parity",
  "pathology_summary", "recovery_summary", "heterogeneity_summary"
)

expected_worker_contract <- function(candidate_id, family, target, chain,
                                     dgp_envelope) {
  selected <- fit_plan$candidate_id == candidate_id &
    fit_plan$family == family & fit_plan$target == target &
    fit_plan$chain == chain
  if (sum(selected) != 1L) oti_stop("V4 worker-plan selection is not unique.")
  list(
    schema_version = otv4_worker_schema(), source_commit = source_commit,
    config_sha256 = config_sha256, runtime_tree_digest = runtime_digest,
    package_version = runtime_binding$package_version,
    candidate_id = candidate_id, family = family, target = target,
    chain = as.integer(chain), profile = fit_plan$profile[selected],
    seed = as.integer(fit_plan$seed[selected]),
    n_burn = as.integer(fit_plan$n_burn[selected]),
    n_mcmc = as.integer(fit_plan$n_mcmc[selected]),
    dgp_contract_digest = dgp_envelope$contract_digest,
    dgp_digest = dgp_envelope$contract$dgp_digest,
    target_digest = dgp_envelope$contract$target_digest,
    coverage_level = config$coverage_level,
    learning_rate = config$learning_rate,
    rng_kind = "L'Ecuyer-CMRG", prediction_storage_contract =
      "ordered_endpoints_only"
  )
}

worker_path <- function(candidate_id, family, target, chain) {
  file.path(
    worker_root,
    sprintf("%s_%s_%s_chain%02d.rds", candidate_id, family,
            tolower(target), chain)
  )
}

run_worker <- function(candidate_id, family, target, chain, dgp_envelope) {
  contract <- expected_worker_contract(
    candidate_id, family, target, chain, dgp_envelope
  )
  path <- worker_path(candidate_id, family, target, chain)
  if (file.exists(path)) {
    existing <- readRDS(path)
    otv4_validate_worker_artifact(existing, contract)
    return(list(path = path, resumed = TRUE))
  }
  entry <- otv3_live_provenance_snapshot(
    provenance_control, "worker_entry", family, target, chain
  )
  if (!isTRUE(entry$snapshot_pass)) {
    oti_stop("Worker-entry provenance failed: ", entry$failed_gates)
  }
  result <- run_chain(candidate_id, family, target, chain, dgp_envelope)
  exit <- otv3_live_provenance_snapshot(
    provenance_control, "worker_exit", family, target, chain
  )
  result$provenance_audit <- oti_rbind_fill(list(
    entry, result$provenance_audit, exit
  ))
  otv3_validate_provenance_audit(
    result$provenance_audit, family, target, chain
  )
  phase_pass <- setNames(
    result$provenance_audit$snapshot_pass, result$provenance_audit$phase
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
  result$chain_summary$worker_entry_provenance_match <-
    phase_identity_match && isTRUE(phase_pass[["worker_entry"]])
  result$chain_summary$fit_recorded_provenance_match <-
    phase_identity_match && isTRUE(phase_pass[["fit_recorded"]])
  result$chain_summary$worker_exit_provenance_match <-
    phase_identity_match && isTRUE(phase_pass[["worker_exit"]])
  result <- otv3_compact_chain_result(result)
  envelope <- list(
    schema_version = otv4_worker_schema(), contract = contract,
    contract_digest = otf_object_sha256(contract), result = result
  )
  otv4_validate_worker_artifact(envelope, contract)
  otf_atomic_save_rds(envelope, path, compress = FALSE)
  list(path = path, resumed = FALSE)
}

validate_cell_bundle <- function(candidate_id, family, target) {
  root <- cell_directory(candidate_id, family, target)
  required <- c(
    "cell_receipt.json", "worker_manifest.csv", "fit_summary.csv",
    "artifact_manifest.csv"
  )
  if (!dir.exists(root) || any(!file.exists(file.path(root, required)))) {
    oti_stop("V4 cell bundle is incomplete: ", candidate_id, "/", family, "/", target)
  }
  otp_verify_manifest(root)
  receipt <- jsonlite::read_json(
    file.path(root, "cell_receipt.json"), simplifyVector = TRUE
  )
  manifest <- utils::read.csv(
    file.path(root, "worker_manifest.csv"), stringsAsFactors = FALSE
  )
  dgp_envelope <- read_dgp(candidate_id, family)
  planned <- fit_plan$candidate_id == candidate_id &
    fit_plan$family == family & fit_plan$target == target
  expected_chains <- as.integer(fit_plan$chain[planned])
  checks <- c(
    identical(receipt$schema_version, otv4_cell_schema()),
    identical(receipt$candidate_id, candidate_id),
    identical(receipt$family, family), identical(receipt$target, target),
    identical(receipt$source_commit, source_commit),
    identical(receipt$config_sha256, config_sha256),
    identical(receipt$runtime_tree_digest, runtime_digest),
    identical(as.integer(manifest$chain), expected_chains),
    nrow(manifest) == length(expected_chains)
  )
  worker_checks <- vapply(seq_len(nrow(manifest)), function(i) {
    path <- file.path(output_root, manifest$path[i])
    if (!file.exists(path) ||
        unname(file.info(path)$size) != manifest$bytes[i] ||
        !identical(oti_file_sha256(path), manifest$sha256[i])) return(FALSE)
    envelope <- tryCatch(readRDS(path), error = function(error) NULL)
    contract <- expected_worker_contract(
      candidate_id, family, target, as.integer(manifest$chain[i]), dgp_envelope
    )
    tryCatch({
      otv4_validate_worker_artifact(envelope, contract)
      identical(envelope$contract_digest, manifest$contract_digest[i])
    }, error = function(error) FALSE)
  }, logical(1L))
  if (!all(checks) || !all(worker_checks)) {
    oti_stop("V4 cell bundle validation failed: ", candidate_id, "/", family, "/", target)
  }
  list(root = root, receipt = receipt, worker_manifest = manifest)
}

write_cell_bundle <- function(candidate_id, family, target, cell, manifest,
                              elapsed) {
  final <- cell_directory(candidate_id, family, target)
  if (dir.exists(final)) oti_stop("V4 cell bundle already exists: ", final)
  stage <- tempfile(
    paste0(".", otv4_cell_key(candidate_id, family, target), "-"), cells_root
  )
  if (!dir.create(stage, recursive = TRUE, showWarnings = FALSE)) {
    oti_stop("Could not create V4 cell staging directory.")
  }
  on.exit(unlink(stage, recursive = TRUE, force = TRUE), add = TRUE)
  for (name in cell_components) {
    value <- cell[[name]]
    if (is.data.frame(value) && nrow(value)) {
      if (!"candidate_id" %in% names(value)) {
        value <- cbind(candidate_id = candidate_id, value,
                       stringsAsFactors = FALSE)
      }
      otf_atomic_write_csv(value, file.path(stage, paste0(name, ".csv")))
    }
  }
  otf_atomic_write_csv(manifest, file.path(stage, "worker_manifest.csv"))
  fit_summary <- cell$fit_summary
  gross <- otv4_gross_cell_eligibility(fit_summary, config)
  receipt <- list(
    schema_version = otv4_cell_schema(), source_commit = source_commit,
    config_sha256 = config_sha256, runtime_tree_digest = runtime_digest,
    candidate_id = candidate_id, family = family, target = target,
    expected_chains = nrow(manifest), completed_chains = nrow(manifest),
    elapsed_seconds = elapsed,
    computational_pass = isTRUE(fit_summary$computational_pass),
    strict_recovery_pass = isTRUE(fit_summary$recovery_pass) &&
      isTRUE(fit_summary$heterogeneity_pass),
    gross_recovery_eligible = isTRUE(gross$gross_recovery_eligible),
    selection_eligible = isTRUE(gross$selection_eligible),
    prediction_storage_contract = "ordered_endpoints_only",
    finished_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
  )
  atomic_json(receipt, file.path(stage, "cell_receipt.json"))
  files <- list.files(stage, full.names = TRUE, recursive = TRUE)
  hashes <- oti_file_hashes(files, stage)
  names(hashes)[names(hashes) == "relative_path"] <- "path"
  otf_atomic_write_csv(hashes, file.path(stage, "artifact_manifest.csv"))
  otp_verify_manifest(stage)
  if (!file.rename(stage, final)) oti_stop("Atomic V4 cell publication failed.")
  on.exit(NULL, add = FALSE)
  validate_cell_bundle(candidate_id, family, target)
}

if (execute_stage == "cell") {
  final <- cell_directory(execute_candidate, execute_family, execute_target)
  if (dir.exists(final)) {
    validate_cell_bundle(execute_candidate, execute_family, execute_target)
    message("[oracle-tilt-v4] validated existing cell: ",
            execute_candidate, "/", execute_family, "/", execute_target)
    quit(save = "no", status = 0L)
  }
  dgp_envelope <- read_dgp(execute_candidate, execute_family)
  rows <- fit_plan$candidate_id == execute_candidate &
    fit_plan$family == execute_family & fit_plan$target == execute_target
  chains <- as.integer(fit_plan$chain[rows])
  started <- Sys.time()
  returns <- vector("list", length(chains))
  for (index in seq_along(chains)) {
    returns[[index]] <- tryCatch(
      run_worker(
        execute_candidate, execute_family, execute_target,
        chains[index], dgp_envelope
      ),
      error = function(error) structure(
        list(message = conditionMessage(error)), class = "otv4_worker_error"
      )
    )
    if (inherits(returns[[index]], "otv4_worker_error")) break
  }
  failed <- which(vapply(
    returns, inherits, logical(1L), what = "otv4_worker_error"
  ))
  if (length(failed)) {
    failure <- data.frame(
      schema_version = "rqrgibbs_oracle_tilt_v4_failure/1.0.0",
      recorded_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
      candidate_id = execute_candidate, family = execute_family,
      target = execute_target, chain = chains[failed[1L]], stage = "fit",
      message = returns[[failed[1L]]]$message, stringsAsFactors = FALSE
    )
    otf_atomic_write_csv(
      failure,
      file.path(output_root, paste0(
        "failure_", otv4_cell_key(
          execute_candidate, execute_family, execute_target
        ), ".csv"
      ))
    )
    oti_stop("A V4 chain failed; the cell has a structured failure record.")
  }
  paths <- vapply(returns, `[[`, character(1L), "path")
  envelopes <- Map(function(path, chain) {
    envelope <- readRDS(path)
    otv4_validate_worker_artifact(
      envelope,
      expected_worker_contract(
        execute_candidate, execute_family, execute_target,
        chain, dgp_envelope
      )
    )
    envelope
  }, paths, chains)
  results <- lapply(envelopes, `[[`, "result")
  manifest <- data.frame(
    candidate_id = execute_candidate, family = execute_family,
    target = execute_target, chain = chains,
    profile = fit_plan$profile[rows], seed = as.integer(fit_plan$seed[rows]),
    resumed = vapply(returns, `[[`, logical(1L), "resumed"),
    path = file.path("worker_results", basename(paths)),
    bytes = unname(file.info(paths)$size),
    sha256 = vapply(paths, oti_file_sha256, character(1L)),
    contract_digest = vapply(
      envelopes, `[[`, character(1L), "contract_digest"
    ), storage_contract = "ordered_endpoints_only", stringsAsFactors = FALSE
  )
  candidate_config <- otv4_candidate_config(
    config, execute_candidate, execute_target
  )
  cell <- otv3_summarize_cell(
    execute_family, execute_target, results,
    dgp_envelope$dgp, dgp_envelope$targets, candidate_config
  )
  elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  write_cell_bundle(
    execute_candidate, execute_family, execute_target,
    cell, manifest, elapsed
  )
  message("[oracle-tilt-v4] completed cell: ", execute_candidate, "/",
          execute_family, "/", execute_target)
  quit(save = "no", status = 0L)
}

# Finalize only after all 18 concurrent cells have terminal bundles.
bundles <- list()
for (index in seq_len(nrow(cell_plan))) {
  row <- cell_plan[index, ]
  key <- paste(row$candidate_id, row$family, row$target, sep = "/")
  bundles[[key]] <- validate_cell_bundle(
    row$candidate_id, row$family, row$target
  )
}
bind_component <- function(name) {
  values <- lapply(bundles, function(bundle) {
    path <- file.path(bundle$root, paste0(name, ".csv"))
    if (file.exists(path)) utils::read.csv(path, stringsAsFactors = FALSE) else
      data.frame()
  })
  values <- Filter(function(value) nrow(value), values)
  if (length(values)) oti_rbind_fill(values) else data.frame()
}
for (name in cell_components) {
  value <- bind_component(name)
  if (nrow(value)) {
    otf_atomic_write_csv(value, file.path(output_root, paste0(name, ".csv")))
  }
}
fit_summary <- bind_component("fit_summary")
selection <- otv4_select_candidates(fit_summary, config)
otf_atomic_write_csv(selection$cell_audit, file.path(output_root, "cell_audit.csv"))
otf_atomic_write_csv(
  selection$score_components,
  file.path(output_root, "selection_score_components.csv")
)
otf_atomic_write_csv(
  selection$family_ranking, file.path(output_root, "family_ranking.csv")
)
otf_atomic_write_csv(
  selection$selected, file.path(output_root, "selected_candidates.csv")
)
worker_manifest <- do.call(rbind, lapply(bundles, `[[`, "worker_manifest"))
cell_manifest <- do.call(rbind, lapply(bundles, function(bundle) {
  data.frame(
    candidate_id = bundle$receipt$candidate_id,
    family = bundle$receipt$family, target = bundle$receipt$target,
    path = file.path("cells", basename(bundle$root)),
    receipt_sha256 = oti_file_sha256(file.path(bundle$root, "cell_receipt.json")),
    manifest_sha256 = oti_file_sha256(file.path(bundle$root, "artifact_manifest.csv")),
    computational_pass = isTRUE(bundle$receipt$computational_pass),
    selection_eligible = isTRUE(bundle$receipt$selection_eligible),
    stringsAsFactors = FALSE
  )
}))
otf_atomic_write_csv(worker_manifest, file.path(output_root, "worker_manifest.csv"))
otf_atomic_write_csv(cell_manifest, file.path(output_root, "cell_manifest.csv"))
status <- cell_plan[, c("candidate_id", "family", "target", "order")]
status$status <- "completed"
status$chains_completed <- vapply(
  bundles, function(bundle) as.integer(bundle$receipt$completed_chains), integer(1L)
)
status$computational_pass <- vapply(
  bundles, function(bundle) isTRUE(bundle$receipt$computational_pass), logical(1L)
)
status$selection_eligible <- vapply(
  bundles, function(bundle) isTRUE(bundle$receipt$selection_eligible), logical(1L)
)
otf_atomic_write_csv(status, file.path(output_root, "run_status.csv"))
complete <- nrow(fit_summary) == 18L && nrow(worker_manifest) == 81L &&
  selection$complete
close_and_manifest(list(
  pass = complete, completed_cells = nrow(cell_manifest),
  completed_chains = nrow(worker_manifest),
  family_winners = nrow(selection$selected),
  selection_complete = selection$complete,
  target_specific_selection_prohibited = TRUE,
  compact_evidence_eligible = complete && runtime_match,
  automatic_manuscript_promotion = FALSE
))
if (!complete) oti_stop("V4 execution completed without two eligible family winners.")
message("[oracle-tilt-v4] execution and deterministic selection complete: ", output_root)
