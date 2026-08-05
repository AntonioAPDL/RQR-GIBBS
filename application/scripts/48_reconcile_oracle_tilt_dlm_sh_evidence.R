#!/usr/bin/env Rscript

arguments <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_path <- sub(file_arg, "", arguments[startsWith(arguments, file_arg)][1L])
if (!length(script_path) || is.na(script_path)) {
  script_path <- "application/scripts/48_reconcile_oracle_tilt_dlm_sh_evidence.R"
}
script_dir <- dirname(normalizePath(script_path, mustWork = TRUE))
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
replace <- any(trailing == "--replace")
repo_root <- normalizePath(file.path(script_dir, "..", ".."), mustWork = TRUE)
baseline <- normalizePath(
  arg_value("--baseline-dir=", ""), winslash = "/", mustWork = TRUE
)
adjudication <- normalizePath(
  arg_value("--adjudication-dir=", ""), winslash = "/", mustWork = TRUE
)
output <- normalizePath(
  arg_value(
    "--output-dir=",
    file.path(repo_root, "figures", "data", "oracle_tilt_c095_v3")
  ), winslash = "/", mustWork = FALSE
)

verify_hash_manifest <- function(root, name, path_column = "path") {
  manifest <- utils::read.csv(
    file.path(root, name), stringsAsFactors = FALSE, check.names = FALSE
  )
  if (!all(c(path_column, "sha256", "bytes") %in% names(manifest))) {
    oti_stop("Invalid manifest: ", file.path(root, name))
  }
  pass <- vapply(seq_len(nrow(manifest)), function(index) {
    path <- file.path(root, manifest[[path_column]][index])
    file.exists(path) && !dir.exists(path) &&
      unname(file.info(path)$size) == manifest$bytes[index] &&
      identical(oti_file_sha256(path), manifest$sha256[index])
  }, logical(1L))
  if (!nrow(manifest) || !all(pass)) oti_stop("Manifest verification failed: ", name)
  manifest
}

baseline_source <- jsonlite::read_json(
  file.path(baseline, "source_state.json"), simplifyVector = TRUE
)
if (!identical(
      baseline_source$source_commit,
      "99a088fbdd7c3f3ed18f99197294038f62dbfe41"
    ) || !isTRUE(baseline_source$exact_runtime_bound)) {
  oti_stop("The expected immutable baseline source is unavailable.")
}
verify_hash_manifest(baseline, "wrapper_artifact_manifest.csv")
adj_closeout <- jsonlite::read_json(
  file.path(adjudication, "closeout.json"), simplifyVector = TRUE
)
if (!identical(adj_closeout$schema_version, otad_closeout_schema()) ||
    adj_closeout$execution_attempt != 2L ||
    adj_closeout$statistical_attempt != 1L ||
    !isTRUE(adj_closeout$software_recovery) ||
    !isTRUE(adj_closeout$staged_execution_pass) ||
    !isTRUE(adj_closeout$strict_pass) ||
    !isTRUE(adj_closeout$automatic_promotion_eligible) ||
    !isTRUE(adj_closeout$prefix_parity_pass) ||
    !isTRUE(adj_closeout$exact_runtime_bound) ||
    adj_closeout$completed_chains != 5L) {
  oti_stop("The DLM/SH adjudication is not a strict promotable result.")
}
stage_status <- utils::read.csv(
  file.path(adjudication, "stage_status.csv"), stringsAsFactors = FALSE
)
self_test <- utils::read.csv(
  file.path(adjudication, "worker_contract_self_test.csv"),
  stringsAsFactors = FALSE
)
if (nrow(stage_status) != 2L ||
    !identical(stage_status$stage, c("acceptance", "remaining")) ||
    !all(stage_status$status == "completed") ||
    !all(stage_status$prefix_pass) ||
    nrow(self_test) != 2L || !all(self_test$pass)) {
  oti_stop("The staged software-recovery gates did not pass.")
}
verify_hash_manifest(adjudication, "artifact_manifest.csv")
verify_hash_manifest(adjudication, "wrapper_artifact_manifest.csv")
adj_wrapper <- utils::read.csv(
  file.path(adjudication, "wrapper_closeout.csv"), stringsAsFactors = FALSE
)
adj_resource <- utils::read.csv(
  file.path(adjudication, "resource_summary.csv"), stringsAsFactors = FALSE
)
if (nrow(adj_wrapper) != 1L || nrow(adj_resource) != 1L ||
    !isTRUE(adj_wrapper$wrapper_pass) || !isTRUE(adj_resource$pass) ||
    !isTRUE(adj_resource$final_pgid_empty)) {
  oti_stop("The adjudication process/resource wrapper did not pass.")
}

baseline_cells <- c(
  "fixed_design_rqr", "dlm_rqr", "fixed_design_et", "dlm_et",
  "fixed_design_sh"
)
for (cell in baseline_cells) {
  root <- file.path(baseline, "cells", cell)
  otp_verify_manifest(root)
  receipt <- jsonlite::read_json(
    file.path(root, "cell_receipt.json"), simplifyVector = TRUE
  )
  if (!isTRUE(receipt$eligible) ||
      !identical(receipt$disposition, "strict_pass")) {
    oti_stop("A baseline cell is not a strict pass: ", cell)
  }
}

stage <- tempfile(paste0(".", basename(output), "-"), dirname(output))
if (!dir.create(stage, recursive = TRUE, showWarnings = FALSE)) {
  oti_stop("Could not create the reconciliation staging directory.")
}
on.exit(unlink(stage, recursive = TRUE, force = TRUE), add = TRUE)

global_files <- c(
  "config.json", "design_contract.csv", "oracle_targets.csv",
  "tail_information.csv", "scale_information.csv", "preflight_gates.csv",
  "reference_gates.csv", "benchmark_summary.csv", "static_basis_audit.csv",
  "static_projection_audit.csv", "dynamic_projection_audit.csv",
  "dlm_time_contract.csv", "fixed_horizon_audit.csv",
  "seasonal_covariance_audit.csv", "dynamic_observability_audit.csv"
)
for (file in global_files) {
  if (!file.copy(file.path(baseline, file), file.path(stage, file))) {
    oti_stop("Could not copy baseline compact artifact: ", file)
  }
}

components <- c(
  "fit_summary", "fit_curves", "endpoint_error_density",
  "endpoint_error_summary", "endpoint_error_by_index", "chain_summary",
  "provenance_audit", "mcmc_diagnostics", "conditional_parity",
  "pathology_summary", "recovery_summary", "heterogeneity_summary"
)
for (component in components) {
  rows <- lapply(baseline_cells, function(cell) {
    path <- file.path(baseline, "cells", cell, paste0(component, ".csv"))
    if (file.exists(path)) utils::read.csv(path, stringsAsFactors = FALSE)
      else NULL
  })
  adj_path <- file.path(adjudication, paste0(component, ".csv"))
  if (file.exists(adj_path)) {
    rows[[length(rows) + 1L]] <- utils::read.csv(
      adj_path, stringsAsFactors = FALSE
    )
  }
  rows <- Filter(Negate(is.null), rows)
  if (length(rows)) {
    otf_atomic_write_csv(
      oti_rbind_fill(rows), file.path(stage, paste0(component, ".csv"))
    )
  }
}
fit <- utils::read.csv(
  file.path(stage, "fit_summary.csv"), stringsAsFactors = FALSE
)
if (nrow(fit) != 6L || !all(fit$disposition == "strict_pass") ||
    !all(fit$manuscript_illustration_evidence_eligible)) {
  oti_stop("The reconciled six-cell fit summary is not strictly eligible.")
}

support <- c(
  "baseline_audit.csv", "design_binding.csv", "prefix_parity.csv",
  "worker_contract_self_test.csv", "stage_status.csv",
  "block_stability.csv", "decision.csv", "baseline_comparison.csv",
  "worker_manifest.csv", "adjudication_config.json", "source_state.json",
  "runtime_binding.json", "closeout.json", "resource_summary.csv",
  "wrapper_closeout.csv"
)
for (file in support) {
  source <- file.path(adjudication, file)
  destination <- file.path(stage, paste0("adjudication_", file))
  if (!file.copy(source, destination)) {
    oti_stop("Could not copy adjudication support artifact: ", file)
  }
}

receipt <- list(
  schema_version = "rqrgibbs_oracle_tilt_evidence/3.1.0",
  baseline_source_commit = baseline_source$source_commit,
  adjudication_source_commit = adj_closeout$source_commit,
  adjudication_execution_attempt = adj_closeout$execution_attempt,
  adjudication_statistical_attempt = adj_closeout$statistical_attempt,
  adjudication_software_recovery = adj_closeout$software_recovery,
  base_config_sha256 = baseline_source$config_sha256,
  adjudication_config_sha256 = adj_closeout$adjudication_config_sha256,
  baseline_runtime_tree_digest = baseline_source$runtime_tree_digest,
  adjudication_runtime_tree_digest = adj_closeout$runtime_tree_digest,
  coverage_level = 0.95,
  innovation_contract = "affinely standardized AL_0.80(0,1)",
  target_cells = 6L, completed_chains = 27L,
  adjudication_chains = 5L,
  all_cells_strict_pass = TRUE,
  dlm_sh_prefix_parity_pass = TRUE,
  exact_population_oracle_tilts = TRUE,
  cornish_fisher_used = FALSE,
  response_predictive_analysis = FALSE,
  simulation_study = FALSE,
  manuscript_illustration_evidence_eligible = TRUE
)
jsonlite::write_json(
  receipt, file.path(stage, "evidence_receipt.json"),
  pretty = TRUE, auto_unbox = TRUE, digits = NA
)
writeLines(c(
  "# Reconciled oracle-tilt illustration evidence",
  "",
  paste(
    "This compact directory combines five strict cells from the immutable",
    "version-3 illustration run with the one-shot strict DLM/SH adjudication."
  ),
  paste(
    "The DLM/SH chains reproduce the original 6,000 retained draws bitwise",
    "and extend each chain to 12,000 retained draws under the unchanged model."
  ),
  paste(
    "All summaries concern interval-root generalized posteriors and do not",
    "define posterior-predictive response distributions."
  )
), file.path(stage, "README.md"))

files <- list.files(stage, full.names = TRUE, recursive = TRUE)
manifest <- oti_file_hashes(files, stage)
names(manifest)[names(manifest) == "relative_path"] <- "path"
utils::write.csv(
  manifest, file.path(stage, "evidence_manifest.csv"), row.names = FALSE
)
if (file.exists(output) || dir.exists(output)) {
  if (!replace) oti_stop("Evidence output exists; use --replace explicitly.")
  backup <- paste0(output, ".backup-", format(Sys.time(), "%Y%m%d%H%M%S"))
  if (!file.rename(output, backup)) oti_stop("Could not preserve prior evidence.")
}
if (!file.rename(stage, output)) oti_stop("Could not publish reconciled evidence.")
on.exit(NULL, add = FALSE)
message("[dlm-sh-reconciliation] published: ", output)
