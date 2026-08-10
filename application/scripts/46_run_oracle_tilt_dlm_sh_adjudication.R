#!/usr/bin/env Rscript

arguments <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_path <- sub(file_arg, "", arguments[startsWith(arguments, file_arg)][1L])
if (!length(script_path) || is.na(script_path)) {
  script_path <- "application/scripts/46_run_oracle_tilt_dlm_sh_adjudication.R"
}
script_path <- normalizePath(script_path, winslash = "/", mustWork = TRUE)
script_dir <- dirname(script_path)
for (file in c(
  "32_oracle_tilt_illustration_utils.R",
  "33_oracle_tilt_forensic_utils.R",
  "34_oracle_tilt_publication_utils.R",
  "42_oracle_tilt_publication_v3_utils.R",
  "46_oracle_tilt_dlm_sh_adjudication_utils.R"
)) source(file.path(script_dir, file))

trailing <- commandArgs(trailingOnly = TRUE)
arg_value <- function(prefix, default = NULL) {
  hit <- trailing[startsWith(trailing, prefix)]
  if (length(hit)) sub(prefix, "", hit[1L], fixed = TRUE) else default
}
mode <- tolower(arg_value("--mode=", "preflight"))
if (!mode %in% c("preflight", "execute")) {
  oti_stop("--mode must be preflight or execute.")
}
repo_root <- normalizePath(file.path(script_dir, "..", ".."), mustWork = TRUE)
setwd(repo_root)
source(file.path(script_dir, "49_oracle_tilt_campaign_gate.R"))
otcg_assert_action(
  repo_root, "publication_v3_dlm_sh_adjudication", mode
)
config_path <- normalizePath(arg_value(
  "--config=",
  file.path(
    repo_root, "application", "config",
    "oracle_tilt_c095_dlm_sh_adjudication_recovery_20260805.json"
  )
), winslash = "/", mustWork = TRUE)
config <- oti_read_json(config_path)
paths <- otad_validate_config(config, repo_root)
base_config <- oti_read_json(paths$base_path)
otv3_validate_config(base_config)
baseline_manifest <- otad_read_baseline_manifest(paths$manifest_path)
baseline_root <- normalizePath(
  arg_value(
    "--baseline-dir=",
    Sys.getenv("RQR_ORACLE_TILT_V3_BASELINE_DIR", "")
  ), winslash = "/", mustWork = TRUE
)
baseline <- otad_verify_baseline(baseline_root, baseline_manifest, config)

for (package in c("rqrgibbs", "jsonlite", "posterior")) {
  if (!requireNamespace(package, quietly = TRUE)) {
    oti_stop("The ", package, " package is required.")
  }
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
exact_requested <- nzchar(expected_commit) || nzchar(attestation_path)
if (exact_requested &&
    (!grepl("^[0-9a-f]{40}$", expected_commit) || !nzchar(attestation_path))) {
  oti_stop("An exact runtime binding requires a full SHA and attestation.")
}
if (identical(mode, "execute") && !exact_requested) {
  oti_stop("Execute mode requires an exact isolated-runtime binding.")
}
if (identical(mode, "execute") &&
    !identical(
      Sys.getenv("RQR_ORACLE_TILT_DLM_SH_ADJUDICATION_CONFIRM"), "YES"
    )) {
  oti_stop("DLM/SH adjudication is fail-closed until explicitly confirmed.")
}
if (identical(mode, "execute") && !isTRUE(config$execution_authorized)) {
  oti_stop("The tracked adjudication configuration is not authorized.")
}

provenance_control <- list()
runtime_binding <- list(
  schema_version = "rqrgibbs_oracle_tilt_runtime_binding/3.0.0",
  binding_kind = "exploratory_installed_namespace", match = FALSE,
  runtime_source_match = FALSE, promotion_eligible = FALSE,
  package_version = as.character(utils::packageVersion("rqrgibbs")),
  runtime_tree_digest = NA_character_,
  expected_commit = if (nzchar(expected_commit)) expected_commit else NA_character_
)
if (exact_requested) {
  source(file.path(script_dir, "lib", "isolated_runtime_lineage.R"))
  source(file.path(script_dir, "lib", "rqr_dlm_main_simulation.R"))
  source(file.path(script_dir, "lib", "rqr_dlm_confirmatory_simulation.R"))
  runtime_binding <- rqr_main_primary_runtime_binding(
    repo_root, expected_commit, attestation_path
  )
  provenance_control <- rqr_confirm_primary_provenance_control(
    repo_root, expected_commit, attestation_path
  )
  if (!isTRUE(runtime_binding$match)) {
    oti_stop("The isolated runtime does not match the adjudication source.")
  }
}

default_output <- file.path(
  repo_root, "application", "outputs", "oracle_tilt_dlm_sh_adjudication",
  paste0(mode, "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
)
output_root <- normalizePath(
  arg_value("--output-dir=", default_output), winslash = "/", mustWork = FALSE
)
wrapper_placeholders <- c(
  "process_group_monitor.csv", "resource_summary.csv", "wrapper_closeout.csv",
  "wrapper_artifact_manifest.csv", "runner.stdout.log", "runner.stderr.log",
  "wrapper_failure_log.csv"
)
existing <- if (dir.exists(output_root)) {
  list.files(output_root, all.files = TRUE, no.. = TRUE)
} else character(0)
if (length(setdiff(existing, wrapper_placeholders))) {
  oti_stop("The adjudication output directory must be fresh.")
}
oti_ensure_dir(output_root)
worker_root <- oti_ensure_dir(file.path(output_root, "worker_results"))

source_commit <- if (nzchar(expected_commit)) expected_commit else
  oti_git_state(repo_root)$commit
config_sha256 <- oti_file_sha256(config_path)
runtime_digest <- runtime_binding$runtime_tree_digest %||% NA_character_
source_state <- list(
  schema_version = otad_closeout_schema(), mode = mode,
  started_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  repository = oti_git_state(repo_root), source_commit = source_commit,
  adjudication_config_sha256 = config_sha256,
  execution_attempt = config$execution_attempt,
  statistical_attempt = config$statistical_attempt,
  invalidated_execution_source_commit =
    config$software_recovery_contract$invalidated_execution_source_commit,
  software_recovery_failure_class =
    config$software_recovery_contract$failure_class,
  base_source_commit = config$base_source_commit,
  base_config_sha256 = config$base_config_sha256,
  runtime_tree_digest = runtime_digest,
  exact_runtime_bound = isTRUE(runtime_binding$match),
  baseline_root_recorded = basename(baseline$root),
  interpretation = paste(
    "One-shot single-data DLM/SH interval-root adjudication; not a response",
    "likelihood, posterior-predictive response analysis, or simulation study."
  )
)
atomic_json(source_state, file.path(output_root, "source_state.json"))
binding_public <- runtime_binding
binding_public$runtime_path <- NULL
atomic_json(binding_public, file.path(output_root, "runtime_binding.json"))
atomic_json(config, file.path(output_root, "adjudication_config.json"))
otf_atomic_write_csv(baseline$audit, file.path(output_root, "baseline_audit.csv"))

law <- otv3_law(base_config)
preflight <- otv3_design_preflight(base_config)
dgp <- preflight$dlm_dgp
targets <- preflight$dlm_targets
dgp_digest <- otf_object_sha256(dgp)
target_digest <- otf_object_sha256(targets)
design_binding <- data.frame(
  object = c("base_config", "dlm_dgp", "population_targets"),
  expected_digest = c(
    config$base_config_sha256, config$base_dgp_digest,
    config$base_target_digest
  ),
  observed_digest = c(
    oti_file_sha256(paths$base_path), dgp_digest, target_digest
  ), stringsAsFactors = FALSE
)
design_binding$pass <- design_binding$expected_digest ==
  design_binding$observed_digest
otf_atomic_write_csv(
  design_binding, file.path(output_root, "design_binding.csv")
)
expected_contract <- function(chain) {
  row <- baseline_manifest[baseline_manifest$chain == chain, , drop = FALSE]
  otad_build_worker_contract(
    source_commit, config_sha256, runtime_digest, config$base_config_sha256,
    chain, row$seed, row$profile, dgp_digest, target_digest,
    config$mcmc_override, row
  )
}
worker_contract_self_test <- otad_worker_contract_self_test(
  expected_contract(config$staging_contract$acceptance_chain)
)
otf_atomic_write_csv(
  worker_contract_self_test,
  file.path(output_root, "worker_contract_self_test.csv")
)
preflight_gates <- rbind(
  data.frame(gate = "baseline_binding", pass = all(baseline$audit$pass)),
  data.frame(gate = "design_binding", pass = all(design_binding$pass)),
  data.frame(
    gate = "single_statistical_attempt",
    pass = config$statistical_attempt == 1L &&
      config$maximum_statistical_attempts == 1L
  ),
  data.frame(
    gate = "bounded_software_recovery",
    pass = config$execution_attempt == 2L &&
      config$maximum_execution_attempts == 2L &&
      !isTRUE(
        config$software_recovery_contract$
          failed_execution_is_statistical_attempt
      ) &&
      !isTRUE(
        config$software_recovery_contract$replacement_is_automatic_rerun
      )
  ),
  data.frame(
    gate = "frozen_scientific_specification",
    pass = all(vapply(config$decision_contract, isTRUE, logical(1L)))
  ),
  data.frame(
    gate = "extended_draw_contract",
    pass = config$mcmc_override$n_mcmc == 12000L &&
      config$baseline_retained_draws == 6000L
  ),
  worker_contract_self_test
)
otf_atomic_write_csv(
  preflight_gates, file.path(output_root, "preflight_gates.csv")
)
if (!all(preflight_gates$pass)) oti_stop("The adjudication preflight failed.")

if (identical(mode, "preflight")) {
  closeout <- list(
    schema_version = otad_closeout_schema(), mode = mode, pass = TRUE,
    source_commit = source_commit,
    adjudication_config_sha256 = config_sha256,
    baseline_bound = TRUE, design_bound = TRUE,
    execution_performed = FALSE,
    finished_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
  )
  atomic_json(closeout, file.path(output_root, "closeout.json"))
  files <- list.files(output_root, full.names = TRUE, recursive = TRUE)
  hashes <- oti_file_hashes(files, output_root)
  names(hashes)[names(hashes) == "relative_path"] <- "path"
  otf_atomic_write_csv(hashes, file.path(output_root, "artifact_manifest.csv"))
  otp_verify_manifest(output_root)
  message("[dlm-sh-adjudication] preflight passed: ", output_root)
  quit(save = "no", status = 0L)
}

chains <- seq_len(config$n_chains)

run_chain <- function(chain) {
  contract <- expected_contract(chain)
  path <- file.path(worker_root, sprintf("dlm_sh_chain%02d.rds", chain))
  entry <- otv3_live_provenance_snapshot(
    provenance_control, "worker_entry", "dlm", "SH", chain
  )
  if (!isTRUE(entry$snapshot_pass)) oti_stop("Worker-entry provenance failed.")
  result <- otv3_dlm_chain(
    base_config, dgp, targets, "SH", chain, provenance_control,
    mcmc_override = config$mcmc_override
  )
  exit <- otv3_live_provenance_snapshot(
    provenance_control, "worker_exit", "dlm", "SH", chain
  )
  result$provenance_audit <- oti_rbind_fill(list(
    entry, result$provenance_audit, exit
  ))
  otv3_validate_provenance_audit(
    result$provenance_audit, "dlm", "SH", chain
  )
  phase_pass <- setNames(
    result$provenance_audit$snapshot_pass,
    result$provenance_audit$phase
  )
  identity_fields <- c(
    "git_commit", "expected_git_commit", "runtime_package_path",
    "runtime_package_version", "runtime_attestation",
    "runtime_attestation_schema", "source_tree_digest",
    "runtime_package_tree_digest"
  )
  identity_match <- all(vapply(identity_fields, function(name) {
    value <- as.character(result$provenance_audit[[name]])
    !anyNA(value) && all(nzchar(value)) && length(unique(value)) == 1L
  }, logical(1L)))
  result$chain_summary$worker_entry_provenance_match <-
    identity_match && isTRUE(phase_pass[["worker_entry"]])
  result$chain_summary$fit_recorded_provenance_match <-
    identity_match && isTRUE(phase_pass[["fit_recorded"]])
  result$chain_summary$worker_exit_provenance_match <-
    identity_match && isTRUE(phase_pass[["worker_exit"]])
  result <- otv3_compact_chain_result(result)
  envelope <- list(
    schema_version = otad_worker_schema(), contract = contract,
    contract_digest = otf_object_sha256(contract), result = result
  )
  otad_validate_worker(envelope, contract)
  otf_atomic_save_rds(envelope, path, compress = FALSE)
  list(
    chain = as.integer(chain), path = path,
    contract_digest = envelope$contract_digest
  )
}

started <- Sys.time()
failure_path <- file.path(output_root, "failure_log.csv")
stage_status_path <- file.path(output_root, "stage_status.csv")
stage_status <- data.frame()
append_failure <- function(rows) {
  prior <- if (file.exists(failure_path)) {
    utils::read.csv(failure_path, stringsAsFactors = FALSE)
  } else data.frame()
  otf_atomic_write_csv(
    oti_rbind_fill(list(prior, rows)), failure_path
  )
}
record_stage <- function(stage, stage_chains, status, stage_started,
                         worker_files = 0L, prefix_checks = 0L,
                         prefix_pass = FALSE) {
  stage_status <<- oti_rbind_fill(list(stage_status, data.frame(
    stage = stage, chains = paste(stage_chains, collapse = ";"),
    status = status,
    started_at_utc = format(stage_started, tz = "UTC", usetz = TRUE),
    finished_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    worker_files = as.integer(worker_files),
    prefix_checks = as.integer(prefix_checks),
    prefix_pass = isTRUE(prefix_pass), stringsAsFactors = FALSE
  )))
  otf_atomic_write_csv(stage_status, stage_status_path)
}
verify_saved_chain <- function(value) {
  chain <- value$chain
  envelope <- readRDS(value$path)
  otad_validate_worker(envelope, expected_contract(chain))
  manifest_row <- baseline_manifest[
    baseline_manifest$chain == chain, , drop = FALSE
  ]
  baseline_envelope <- readRDS(file.path(baseline$root, manifest_row$path))
  parity <- otad_prefix_parity(
    baseline_envelope, envelope, chain, config$baseline_retained_draws
  )
  list(value = value, parity = parity)
}
run_stage <- function(stage, stage_chains, workers) {
  stage_started <- Sys.time()
  returns <- otad_run_batches(stage_chains, workers, run_chain, stage)
  failed <- vapply(returns, inherits, logical(1L), "otad_worker_error")
  if (any(failed)) {
    errors <- returns[failed]
    append_failure(data.frame(
      recorded_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
      chain = vapply(errors, `[[`, integer(1L), "chain"),
      stage = vapply(errors, `[[`, character(1L), "stage"),
      message = vapply(errors, `[[`, character(1L), "message"),
      stringsAsFactors = FALSE
    ))
    record_stage(
      stage, stage_chains, "worker_failed", stage_started,
      worker_files = sum(!failed)
    )
    oti_stop("One or more adjudication workers failed during ", stage, ".")
  }
  verified <- lapply(returns, function(value) {
    tryCatch(
      verify_saved_chain(value),
      error = function(error) otad_worker_error(
        value$chain, paste0(stage, "_saved_artifact"), error
      )
    )
  })
  invalid <- vapply(verified, inherits, logical(1L), "otad_worker_error")
  if (any(invalid)) {
    errors <- verified[invalid]
    append_failure(data.frame(
      recorded_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
      chain = vapply(errors, `[[`, integer(1L), "chain"),
      stage = vapply(errors, `[[`, character(1L), "stage"),
      message = vapply(errors, `[[`, character(1L), "message"),
      stringsAsFactors = FALSE
    ))
    record_stage(
      stage, stage_chains, "saved_artifact_failed", stage_started,
      worker_files = length(returns)
    )
    oti_stop("Saved adjudication artifacts failed validation during ", stage, ".")
  }
  parity <- do.call(rbind, lapply(verified, `[[`, "parity"))
  if (!all(parity$pass)) {
    append_failure(data.frame(
      recorded_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
      chain = unique(parity$chain[!parity$pass]),
      stage = paste0(stage, "_prefix"),
      message = "The retained-draw prefix was not bitwise identical.",
      stringsAsFactors = FALSE
    ))
    record_stage(
      stage, stage_chains, "prefix_failed", stage_started,
      worker_files = length(returns), prefix_checks = nrow(parity)
    )
    oti_stop("The adjudication prefix failed during ", stage, ".")
  }
  record_stage(
    stage, stage_chains, "completed", stage_started,
    worker_files = length(returns), prefix_checks = nrow(parity),
    prefix_pass = TRUE
  )
  list(returns = returns, parity = parity)
}

acceptance_chain <- config$staging_contract$acceptance_chain
acceptance <- run_stage("acceptance", acceptance_chain, 1L)
if (!all(acceptance$parity$pass)) {
  oti_stop("The acceptance-chain prefix gate failed closed.")
}
remaining_chains <- as.integer(unlist(
  config$staging_contract$remaining_chains
))
remaining <- run_stage(
  "remaining", remaining_chains,
  min(config$workers, length(remaining_chains))
)
returns <- c(acceptance$returns, remaining$returns)
return_chains <- vapply(returns, `[[`, integer(1L), "chain")
if (!identical(sort(return_chains), chains)) {
  oti_stop("The staged adjudication did not return all five chains exactly once.")
}
returns <- returns[match(chains, return_chains)]
prefix <- rbind(acceptance$parity, remaining$parity)
prefix <- prefix[order(prefix$chain, prefix$object), , drop = FALSE]
rownames(prefix) <- NULL
otf_atomic_write_csv(prefix, file.path(output_root, "prefix_parity.csv"))
if (nrow(prefix) != 15L || !all(prefix$pass)) {
  oti_stop("The complete adjudication prefix contract failed.")
}
elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))

extended <- lapply(returns, function(value) {
  envelope <- readRDS(value$path)
  otad_validate_worker(envelope, expected_contract(value$chain))
  envelope
})

chain_results <- lapply(extended, `[[`, "result")
cell <- otv3_summarize_cell("dlm", "SH", chain_results, dgp, targets, base_config)
for (name in names(cell)) {
  value <- cell[[name]]
  if (is.data.frame(value) && nrow(value)) {
    otf_atomic_write_csv(value, file.path(output_root, paste0(name, ".csv")))
  }
}
stability <- otad_block_stability(chain_results, dgp, targets)
otf_atomic_write_csv(stability, file.path(output_root, "block_stability.csv"))
decision <- otad_decision(cell, prefix)
otf_atomic_write_csv(decision, file.path(output_root, "decision.csv"))

worker_manifest <- do.call(rbind, lapply(chains, function(chain) {
  path <- returns[[chain]]$path
  data.frame(
    chain = chain, path = file.path("worker_results", basename(path)),
    sha256 = oti_file_sha256(path), bytes = unname(file.info(path)$size),
    contract_digest = extended[[chain]]$contract_digest,
    stringsAsFactors = FALSE
  )
}))
otf_atomic_write_csv(
  worker_manifest, file.path(output_root, "worker_manifest.csv")
)
baseline_summary <- utils::read.csv(
  file.path(baseline$root, "cells", "dlm_sh", "fit_summary.csv"),
  stringsAsFactors = FALSE
)
metrics <- c(
  "retained_draws", "endpoint_rmse_over_oracle_width", "mean_width_ratio",
  "width_contrast_relative_error", "seasonal_width_amplitude_ratio",
  "seasonal_width_phase_error"
)
comparison <- data.frame(
  metric = metrics,
  baseline = vapply(metrics, function(name) baseline_summary[[name]][1L], numeric(1L)),
  adjudication = vapply(metrics, function(name) cell$fit_summary[[name]][1L], numeric(1L)),
  stringsAsFactors = FALSE
)
comparison$difference <- comparison$adjudication - comparison$baseline
otf_atomic_write_csv(
  comparison, file.path(output_root, "baseline_comparison.csv")
)

closeout <- list(
  schema_version = otad_closeout_schema(), mode = mode,
  source_commit = source_commit,
  adjudication_config_sha256 = config_sha256,
  execution_attempt = config$execution_attempt,
  statistical_attempt = config$statistical_attempt,
  invalidated_execution_source_commit =
    config$software_recovery_contract$invalidated_execution_source_commit,
  software_recovery = TRUE,
  runtime_tree_digest = runtime_digest,
  exact_runtime_bound = isTRUE(runtime_binding$match),
  baseline_bound = all(baseline$audit$pass),
  design_bound = all(design_binding$pass),
  completed_chains = length(chain_results),
  completed_stages = as.character(stage_status$stage),
  staged_execution_pass = nrow(stage_status) == 2L &&
    all(stage_status$status == "completed") &&
    all(stage_status$prefix_pass),
  retained_draws_per_chain = config$mcmc_override$n_mcmc,
  prefix_draws_per_chain = config$baseline_retained_draws,
  prefix_parity_pass = all(prefix$pass),
  strict_pass = isTRUE(decision$strict_pass),
  automatic_promotion_eligible = isTRUE(decision$automatic_promotion_eligible),
  descriptive_review_required = isTRUE(decision$descriptive_review_required),
  disposition = as.character(decision$disposition),
  additional_automatic_rerun_authorized = FALSE,
  elapsed_seconds = elapsed,
  response_predictive_analysis = FALSE, simulation_study = FALSE,
  finished_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
)
atomic_json(closeout, file.path(output_root, "closeout.json"))

internal_files <- list.files(
  output_root, full.names = TRUE, recursive = TRUE
)
internal_files <- internal_files[
  !basename(internal_files) %in% c(wrapper_placeholders, "artifact_manifest.csv")
]
hashes <- oti_file_hashes(internal_files, output_root)
names(hashes)[names(hashes) == "relative_path"] <- "path"
otf_atomic_write_csv(hashes, file.path(output_root, "artifact_manifest.csv"))
otp_verify_manifest(output_root)
if (!isTRUE(decision$hard_integrity_pass)) {
  oti_stop("The adjudication failed a hard integrity gate.")
}
message(
  "[dlm-sh-adjudication] completed with disposition=",
  decision$disposition, ": ", output_root
)
