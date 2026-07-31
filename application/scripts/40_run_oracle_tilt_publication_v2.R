#!/usr/bin/env Rscript

arguments <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_path <- sub(file_arg, "", arguments[startsWith(arguments, file_arg)][1L])
if (!length(script_path) || is.na(script_path)) {
  script_path <- "application/scripts/40_run_oracle_tilt_publication_v2.R"
}
script_path <- normalizePath(script_path, winslash = "/", mustWork = TRUE)
script_dir <- dirname(script_path)
source(file.path(script_dir, "32_oracle_tilt_illustration_utils.R"))
source(file.path(script_dir, "33_oracle_tilt_forensic_utils.R"))
source(file.path(script_dir, "34_oracle_tilt_publication_utils.R"))
source(file.path(script_dir, "40_oracle_tilt_publication_v2_utils.R"))

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

repo_root <- normalizePath(file.path(script_dir, "..", ".."), mustWork = TRUE)
setwd(repo_root)
config_path <- normalizePath(
  arg_value(
    "--config=",
    file.path(
      repo_root, "application", "config",
      "oracle_tilt_c095_publication_v2_20260731.json"
    )
  ),
  winslash = "/", mustWork = TRUE
)
config <- oti_read_json(config_path)
otv2_validate_config(config)

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
  schema_version = "rqrgibbs_oracle_tilt_runtime_binding/2.0.0",
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
  benchmark = c("RQR_ORACLE_TILT_V2_BENCHMARK_CONFIRM", "YES"),
  execute = c("RQR_ORACLE_TILT_V2_CONFIRM", "YES"),
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
  repo_root, "application", "outputs", "oracle_tilt_c095_publication_v2",
  paste0(mode, "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
)
output_root <- normalizePath(
  arg_value("--output-dir=", default_output), winslash = "/", mustWork = FALSE
)
existing_entries <- if (dir.exists(output_root)) {
  list.files(output_root, all.files = TRUE, no.. = TRUE)
} else character(0)
wrapper_placeholders <- c(
  "process_group_monitor.csv", "runner.stdout.log", "runner.stderr.log"
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
  schema_version = otv2_schema(), mode = mode,
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

preflight <- otv2_design_preflight(config)
if (!preflight$pass) oti_stop("The v2 design preflight did not pass.")

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
    maximum_gram_absolute_error = max(abs(basis$gram - diag(3L))),
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
    static_prior_predictive.csv = preflight$static_prior_audit,
    dlm_prior_predictive.csv = preflight$dlm_prior_audit,
    dlm_time_contract.csv = time_contract,
    fixed_horizon_audit.csv = preflight$fixed_horizon_audit,
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
    schema_version = otv2_schema(), mode = mode,
    source_commit = source_commit, config_sha256 = config_sha256,
    runtime_tree_digest = runtime_digest,
    exact_runtime_bound = runtime_match,
    finished_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    exact_population_oracle_tilts = TRUE, cornish_fisher_used = FALSE,
    response_predictive_analysis = FALSE, simulation_study = FALSE,
    manuscript_promotion_authorized = FALSE
  ), extra)
  atomic_json(closeout, file.path(output_root, "closeout.json"))
  compact <- file.path(output_root, otv2_compact_files())
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
  message("[oracle-tilt-v2] preflight passed: ", output_root)
  quit(save = "no", status = 0L)
}

if (identical(mode, "reference-only")) {
  reference <- otv2_reference_suite(config)
  otf_atomic_write_csv(reference, file.path(output_root, "reference_gates.csv"))
  passed <- nrow(reference) == 12L && all(reference$pass)
  close_and_manifest(list(
    pass = passed, reference_gates = nrow(reference),
    reference_gates_passed = sum(reference$pass), completed_chains = 0L,
    compact_evidence_eligible = passed &&
      runtime_match
  ))
  if (!passed) oti_stop("One or more conditional-reference gates failed.")
  message("[oracle-tilt-v2] reference suite passed: ", output_root)
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
  c(preflight = "RQR_ORACLE_TILT_V2_PREFLIGHT_DIR",
    `reference-only` = "RQR_ORACLE_TILT_V2_REFERENCE_DIR")
} else {
  c(preflight = "RQR_ORACLE_TILT_V2_PREFLIGHT_DIR",
    `reference-only` = "RQR_ORACLE_TILT_V2_REFERENCE_DIR",
    benchmark = "RQR_ORACLE_TILT_V2_BENCHMARK_DIR")
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
      otv2_fixed_chain(
        config, preflight$fixed_dgp, preflight$fixed_targets,
        specification$target, as.integer(config$benchmark$chain),
        provenance_control
      )
    } else {
      otv2_dlm_chain(
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
    data.frame(
      family = specification$family, target = specification$target,
      chain = as.integer(config$benchmark$chain),
      elapsed_seconds = as.numeric(difftime(Sys.time(), started, units = "secs")),
      worker_bytes = file.info(path)$size,
      numerical_repair_count = result$chain_summary$numerical_repair_count,
      promotion_eligible = result$chain_summary$promotion_eligible,
      worker_sha256 = oti_file_sha256(path), stringsAsFactors = FALSE
    )
  })
  benchmark <- do.call(rbind, rows)
  benchmark$pass <- with(
    benchmark,
    elapsed_seconds <= config$benchmark$maximum_elapsed_seconds &
      worker_bytes <= config$benchmark$maximum_worker_bytes &
      numerical_repair_count == 0L & promotion_eligible
  )
  otf_atomic_write_csv(benchmark, file.path(output_root, "benchmark_summary.csv"))
  passed <- nrow(benchmark) == 2L && all(benchmark$pass)
  close_and_manifest(list(
    pass = passed, benchmark_cells = nrow(benchmark),
    benchmark_cells_passed = sum(benchmark$pass), completed_chains = 2L,
    compact_evidence_eligible = passed
  ))
  if (!passed) oti_stop("The representative benchmark did not pass.")
  message("[oracle-tilt-v2] representative benchmark passed: ", output_root)
  quit(save = "no", status = 0L)
}

worker_contract_base <- list(
  schema_version = otv2_schema(), source_commit = source_commit,
  config_sha256 = config_sha256, runtime_tree_digest = runtime_digest,
  package_version = runtime_binding$package_version,
  coverage_level = config$coverage_level,
  learning_rate = config$learning_rate
)

run_worker <- function(family, target, chain) {
  selected <- preflight$plan$family == family &
    preflight$plan$target == target & preflight$plan$chain == chain
  profile <- preflight$plan$profile[selected][1L]
  seed <- preflight$plan$seed[selected][1L]
  dgp <- if (identical(family, "fixed_design")) {
    preflight$fixed_dgp
  } else preflight$dlm_dgp
  target_values <- if (identical(family, "fixed_design")) {
    preflight$fixed_targets
  } else preflight$dlm_targets
  contract <- c(worker_contract_base, list(
    family = family, target = target, chain = chain, profile = profile,
    seed = seed, dgp_digest = otf_object_sha256(dgp),
    target_digest = otf_object_sha256(target_values)
  ))
  digest <- otf_object_sha256(contract)
  path <- file.path(
    worker_root,
    sprintf("%s_%s_chain%02d.rds", family, tolower(target), chain)
  )
  if (file.exists(path)) {
    existing <- tryCatch(readRDS(path), error = function(error) NULL)
    if (is.list(existing) && identical(existing$contract_digest, digest)) {
      return(list(path = path, resumed = TRUE))
    }
    oti_stop("A worker result exists with a mismatched contract: ", path)
  }
  result <- if (identical(family, "fixed_design")) {
    otv2_fixed_chain(
      config, dgp, target_values, target, chain, provenance_control
    )
  } else {
    otv2_dlm_chain(
      config, dgp, target_values, target, chain, provenance_control
    )
  }
  otf_atomic_save_rds(list(
    contract = contract, contract_digest = digest, result = result
  ), path, compress = FALSE)
  list(path = path, resumed = FALSE)
}

cell_results <- list()
worker_manifest_rows <- list()
run_status <- data.frame()
failure_log <- data.frame()
for (family in c("fixed_design", "dlm")) {
  for (target in c("RQR", "ET", "SH")) {
    rows <- preflight$plan$family == family & preflight$plan$target == target
    chains <- preflight$plan$chain[rows]
    workers <- min(
      as.integer(config[[family]]$workers), length(chains)
    )
    message(
      "[oracle-tilt-v2] ", family, "/", target, ": ", length(chains),
      " chains; workers=", workers
    )
    started <- Sys.time()
    returns <- if (workers > 1L && .Platform$OS.type != "windows") {
      parallel::mclapply(
        chains,
        function(chain) tryCatch(
          run_worker(family, target, chain),
          error = function(error) structure(
            list(message = conditionMessage(error)), class = "otv2_worker_error"
          )
        ),
        mc.cores = workers, mc.preschedule = FALSE, mc.set.seed = FALSE
      )
    } else {
      lapply(chains, function(chain) tryCatch(
        run_worker(family, target, chain),
        error = function(error) structure(
          list(message = conditionMessage(error)), class = "otv2_worker_error"
        )
      ))
    }
    failed <- which(vapply(
      returns, inherits, logical(1L), what = "otv2_worker_error"
    ))
    if (length(failed)) {
      failure_log <- rbind(failure_log, data.frame(
        recorded_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
        family = family, target = target, chain = chains[failed],
        stage = "fit", message = vapply(
          returns[failed], `[[`, character(1L), "message"
        ), stringsAsFactors = FALSE
      ))
      otf_atomic_write_csv(failure_log, file.path(output_root, "failure_log.csv"))
      oti_stop("A chain failed; later cells were not launched.")
    }
    paths <- vapply(returns, `[[`, character(1L), "path")
    envelopes <- lapply(paths, readRDS)
    results <- lapply(envelopes, `[[`, "result")
    manifest_rows <- data.frame(
      family = family, target = target, chain = chains,
      profile = preflight$plan$profile[rows],
      resumed = vapply(returns, `[[`, logical(1L), "resumed"),
      path = file.path("worker_results", basename(paths)),
      bytes = unname(file.info(paths)$size),
      sha256 = vapply(paths, oti_file_sha256, character(1L)),
      contract_digest = vapply(
        envelopes, `[[`, character(1L), "contract_digest"
      ), stringsAsFactors = FALSE
    )
    worker_manifest_rows[[length(worker_manifest_rows) + 1L]] <- manifest_rows
    cell <- otv2_summarize_cell(
      family, target, results,
      if (identical(family, "fixed_design")) preflight$fixed_dgp else
        preflight$dlm_dgp,
      if (identical(family, "fixed_design")) preflight$fixed_targets else
        preflight$dlm_targets,
      config
    )
    cell_results[[paste(family, target, sep = "/")]] <- cell
    run_status <- rbind(run_status, data.frame(
      family = family, target = target, status = "completed",
      chains_completed = length(chains),
      elapsed_seconds = as.numeric(difftime(Sys.time(), started, units = "secs")),
      computational_pass = cell$fit_summary$computational_pass,
      recovery_pass = cell$fit_summary$recovery_pass,
      disposition = cell$fit_summary$disposition,
      stringsAsFactors = FALSE
    ))
    otf_atomic_write_csv(run_status, file.path(output_root, "run_status.csv"))
    if (!isTRUE(cell$fit_summary$manuscript_illustration_evidence_eligible)) {
      failure_log <- rbind(failure_log, data.frame(
        recorded_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
        family = family, target = target, chain = NA_integer_,
        stage = "cell_gate",
        message = "The completed four/five-chain cell did not pass all gates.",
        stringsAsFactors = FALSE
      ))
      otf_atomic_write_csv(failure_log, file.path(output_root, "failure_log.csv"))
      oti_stop("Cell gates failed; later cells were not launched.")
    }
  }
}

bind_component <- function(name) {
  values <- lapply(cell_results, `[[`, name)
  values <- Filter(function(value) is.data.frame(value) && nrow(value), values)
  if (length(values)) oti_rbind_fill(values) else data.frame()
}
components <- c(
  "fit_summary", "fit_curves", "endpoint_error_density",
  "endpoint_error_summary", "endpoint_error_by_index", "chain_summary",
  "mcmc_diagnostics", "conditional_parity", "pathology_summary",
  "recovery_summary"
)
for (name in components) {
  value <- bind_component(name)
  if (nrow(value)) {
    otf_atomic_write_csv(value, file.path(output_root, paste0(name, ".csv")))
  }
}
fit_summary <- bind_component("fit_summary")
cell_disposition <- fit_summary[, c(
  "family", "target", "provenance_pass", "strict_diagnostics_pass",
  "conditional_parity_pass", "pathology_pass", "computational_pass",
  "recovery_pass", "disposition",
  "manuscript_illustration_evidence_eligible"
)]
otf_atomic_write_csv(
  cell_disposition, file.path(output_root, "cell_disposition.csv")
)
worker_manifest <- do.call(rbind, worker_manifest_rows)
otf_atomic_write_csv(worker_manifest, file.path(output_root, "worker_manifest.csv"))

all_completed <- nrow(worker_manifest) == nrow(preflight$plan)
all_passed <- nrow(fit_summary) == 6L &&
  all(fit_summary$manuscript_illustration_evidence_eligible)
close_and_manifest(list(
  pass = all_completed && all_passed,
  planned_chains = nrow(preflight$plan), completed_chains = nrow(worker_manifest),
  all_chains_completed = all_completed, all_cells_pass = all_passed,
  passed_cells = sum(fit_summary$disposition == "strict_pass"),
  failed_cells = sum(fit_summary$disposition == "fail"),
  compact_evidence_eligible = all_completed && all_passed
))
if (!all_completed || !all_passed) oti_stop("The v2 execution did not pass.")
message("[oracle-tilt-v2] execution passed and closed: ", output_root)
