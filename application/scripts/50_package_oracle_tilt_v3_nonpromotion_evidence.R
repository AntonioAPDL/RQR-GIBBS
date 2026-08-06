#!/usr/bin/env Rscript

arguments <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_path <- sub(file_arg, "", arguments[startsWith(arguments, file_arg)][1L])
if (!length(script_path) || is.na(script_path)) {
  script_path <-
    "application/scripts/50_package_oracle_tilt_v3_nonpromotion_evidence.R"
}
script_dir <- dirname(normalizePath(script_path, winslash = "/", mustWork = TRUE))
source(file.path(script_dir, "32_oracle_tilt_illustration_utils.R"))
source(file.path(script_dir, "49_oracle_tilt_campaign_gate.R"))

trailing <- commandArgs(trailingOnly = TRUE)
arg_value <- function(prefix, default = "") {
  hit <- trailing[startsWith(trailing, prefix)]
  if (length(hit)) sub(prefix, "", hit[1L], fixed = TRUE) else default
}
replace <- any(trailing == "--replace")
repo_root <- normalizePath(file.path(script_dir, "..", ".."), mustWork = TRUE)
otcg_assert_action(repo_root, "publication_v3", "package-nonpromotion")
otcg_assert_action(
  repo_root, "publication_v3_dlm_sh_adjudication", "package-nonpromotion"
)

for (package in c("jsonlite", "digest")) {
  if (!requireNamespace(package, quietly = TRUE)) {
    oti_stop("The ", package, " package is required.")
  }
}

baseline_dir <- arg_value("--baseline-dir=")
adjudication_dir <- arg_value("--adjudication-dir=")
if (!nzchar(baseline_dir) || !nzchar(adjudication_dir)) {
  oti_stop("--baseline-dir and --adjudication-dir are required.")
}
baseline_dir <- normalizePath(baseline_dir, winslash = "/", mustWork = TRUE)
adjudication_dir <- normalizePath(
  adjudication_dir, winslash = "/", mustWork = TRUE
)
output_dir <- normalizePath(
  arg_value(
    "--output-dir=",
    file.path(
      repo_root, "docs", "audits",
      "oracle_tilt_c095_v3_nonpromotion_evidence_20260805"
    )
  ),
  winslash = "/", mustWork = FALSE
)
audit_root <- normalizePath(
  file.path(repo_root, "docs", "audits"), winslash = "/", mustWork = TRUE
)
if (!startsWith(output_dir, paste0(audit_root, "/")) ||
    identical(output_dir, audit_root)) {
  oti_stop("The compact output must be a child of docs/audits.")
}

read_csv <- function(root, relative) {
  path <- file.path(root, relative)
  if (!file.exists(path)) oti_stop("Required evidence is missing: ", path)
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}
read_json <- function(root, relative) {
  path <- file.path(root, relative)
  if (!file.exists(path)) oti_stop("Required evidence is missing: ", path)
  jsonlite::read_json(path, simplifyVector = TRUE)
}
assert_true <- function(value, message) {
  if (!isTRUE(value)) oti_stop(message)
}

registry <- otcg_read_registry(repo_root)
v2 <- registry$campaigns$publication_v2
v3 <- registry$campaigns$publication_v3
adj <- registry$campaigns$publication_v3_dlm_sh_adjudication

v2_receipt <- read_json(
  repo_root, file.path(
    registry$active_manuscript_evidence_directory, "evidence_receipt.json"
  )
)
assert_true(
  identical(v2_receipt$source_commit, v2$source_commit) &&
    isTRUE(v2_receipt$all_cells_strict_pass) &&
    isTRUE(v2_receipt$manuscript_illustration_evidence_eligible) &&
    as.integer(v2_receipt$completed_chains) == 27L,
  "The active version-2 manuscript evidence is not the validated bundle."
)

baseline_source <- read_json(baseline_dir, "source_state.json")
baseline_status <- read_csv(baseline_dir, "run_status.csv")
assert_true(
  identical(baseline_source$source_commit, v3$source_commit) &&
    identical(baseline_source$config_sha256, v3$config_sha256) &&
    identical(baseline_source$runtime_tree_digest, v3$runtime_tree_digest) &&
    isTRUE(baseline_source$exact_runtime_bound),
  "The version-3 baseline source/runtime binding changed."
)
assert_true(
  nrow(baseline_status) == 6L &&
    sum(baseline_status$chains_completed) == 27L &&
    sum(baseline_status$disposition == "strict_pass") == 5L &&
    identical(
      baseline_status$family[baseline_status$disposition == "fail"], "dlm"
    ) &&
    identical(
      baseline_status$target[baseline_status$disposition == "fail"], "SH"
    ),
  "The version-3 six-cell non-promotion disposition changed."
)

cell_paths <- file.path(
  baseline_dir, "cells",
  c(
    "fixed_design_rqr", "fixed_design_et", "fixed_design_sh",
    "dlm_rqr", "dlm_et", "dlm_sh"
  )
)
if (any(!dir.exists(cell_paths))) oti_stop("One or more compact cell roots are missing.")
bind_cell_file <- function(file) {
  values <- lapply(cell_paths, function(path) read_csv(path, file))
  names_union <- Reduce(union, lapply(values, names))
  values <- lapply(values, function(value) {
    missing <- setdiff(names_union, names(value))
    for (name in missing) value[[name]] <- NA
    value[names_union]
  })
  do.call(rbind, values)
}
baseline_fit <- bind_cell_file("fit_summary.csv")
baseline_diagnostics <- bind_cell_file("mcmc_diagnostics.csv")
baseline_recovery <- bind_cell_file("recovery_summary.csv")
baseline_heterogeneity <- bind_cell_file("heterogeneity_summary.csv")
assert_true(
  nrow(baseline_fit) == 6L &&
    sum(baseline_fit$manuscript_illustration_evidence_eligible) == 5L &&
    nrow(baseline_diagnostics) > 0L &&
    sum(!baseline_diagnostics$pass) == 1L,
  "The compact version-3 cell summaries do not reproduce the closeout."
)

adjudication_source <- read_json(adjudication_dir, "source_state.json")
adjudication_decision <- read_csv(adjudication_dir, "decision.csv")
adjudication_fit <- read_csv(adjudication_dir, "fit_summary.csv")
prefix <- read_csv(adjudication_dir, "prefix_parity.csv")
stages <- read_csv(adjudication_dir, "stage_status.csv")
diagnostics <- read_csv(adjudication_dir, "mcmc_diagnostics.csv")
resource <- read_csv(adjudication_dir, "resource_summary.csv")
assert_true(
  identical(adjudication_source$source_commit, adj$source_commit) &&
    identical(adjudication_source$base_source_commit, v3$source_commit) &&
    identical(adjudication_source$adjudication_config_sha256,
              adj$config_sha256) &&
    identical(adjudication_source$runtime_tree_digest,
              adj$runtime_tree_digest) &&
    isTRUE(adjudication_source$exact_runtime_bound),
  "The adjudication source/runtime binding changed."
)
assert_true(
  nrow(adjudication_decision) == 1L &&
    isTRUE(adjudication_decision$prefix_parity_pass) &&
    isTRUE(adjudication_decision$hard_integrity_pass) &&
    isTRUE(adjudication_decision$strict_diagnostics_pass) &&
    !isTRUE(adjudication_decision$heterogeneity_pass) &&
    !isTRUE(adjudication_decision$strict_pass) &&
    !isTRUE(adjudication_decision$automatic_promotion_eligible) &&
    isTRUE(adjudication_decision$descriptive_review_required),
  "The adjudication decision is not the reviewed descriptive closeout."
)
assert_true(
  nrow(adjudication_fit) == 1L && adjudication_fit$n_chains == 5L &&
    adjudication_fit$retained_draws == 60000L &&
    adjudication_fit$numerical_repair_count == 0L &&
    adjudication_fit$width_contrast_relative_error >
      adj$maximum_width_contrast_relative_error &&
    abs(adjudication_fit$width_contrast_relative_error -
          adj$width_contrast_relative_error) < 1e-12,
  "The adjudication recovery summary changed."
)
assert_true(
  nrow(prefix) == 15L && all(prefix$pass) &&
    all(prefix$bitwise_identical) &&
    all(prefix$maximum_absolute_difference == 0) &&
    nrow(stages) == 2L && all(stages$status == "completed") &&
    sum(stages$prefix_checks) == 15L && all(stages$prefix_pass) &&
    nrow(diagnostics) == 137L && all(diagnostics$pass) &&
    nrow(resource) == 1L && isTRUE(resource$pass) &&
    isTRUE(resource$final_pgid_empty) && !isTRUE(resource$timed_out) &&
    !isTRUE(resource$sampled_limit_triggered),
  "The adjudication integrity, diagnostics, or resource evidence changed."
)

baseline_failure <- read_csv(baseline_dir, "failure_log.csv")
baseline_orchestrator_failure <- read_csv(
  baseline_dir, "orchestrator_failure_log.csv"
)
baseline_wrapper_failure <- read_csv(baseline_dir, "wrapper_failure_log.csv")
assert_true(
  nrow(baseline_failure) == 1L &&
    identical(baseline_failure$family, "dlm") &&
    identical(baseline_failure$target, "SH") &&
    identical(baseline_failure$stage, "cell_gate") &&
    nrow(baseline_orchestrator_failure) == 1L &&
    identical(baseline_orchestrator_failure$family, "dlm") &&
    identical(baseline_orchestrator_failure$target, "SH") &&
    nrow(baseline_wrapper_failure) == 1L &&
    baseline_wrapper_failure$runner_status == 1L &&
    !isTRUE(baseline_wrapper_failure$timed_out) &&
    !isTRUE(baseline_wrapper_failure$limit_triggered),
  "The baseline failure ledgers no longer isolate the DLM/SH gate failure."
)
adjudication_failure_path <- file.path(
  adjudication_dir, "wrapper_failure_log.csv"
)
assert_true(
  !file.exists(adjudication_failure_path) ||
    file.info(adjudication_failure_path)$size == 0,
  "The adjudication recorded a wrapper failure."
)

if (dir.exists(output_dir)) {
  if (!replace) oti_stop("Output exists; use --replace to rebuild it.")
  unlink(output_dir, recursive = TRUE, force = TRUE)
}
parent <- dirname(output_dir)
dir.create(parent, recursive = TRUE, showWarnings = FALSE)
stage <- tempfile(".oracle-tilt-v3-nonpromotion-", tmpdir = parent)
dir.create(stage, recursive = TRUE, showWarnings = FALSE)
on.exit(if (dir.exists(stage)) unlink(stage, recursive = TRUE, force = TRUE),
        add = TRUE)

atomic_csv <- function(value, path) {
  temporary <- tempfile(paste0(".", basename(path), "-"), dirname(path))
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  utils::write.csv(value, temporary, row.names = FALSE, na = "")
  if (!file.rename(temporary, path)) oti_stop("Atomic CSV write failed: ", path)
}
atomic_json <- function(value, path) {
  temporary <- tempfile(paste0(".", basename(path), "-"), dirname(path))
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  jsonlite::write_json(
    value, temporary, pretty = TRUE, auto_unbox = TRUE,
    digits = NA, null = "null", na = "null"
  )
  if (!file.rename(temporary, path)) oti_stop("Atomic JSON write failed: ", path)
}
copy_required <- function(root, relative, output_name = relative) {
  source <- file.path(root, relative)
  destination <- file.path(stage, output_name)
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  if (!file.copy(source, destination, overwrite = FALSE, copy.mode = TRUE)) {
    oti_stop("Could not copy compact evidence: ", source)
  }
}

atomic_csv(baseline_status, file.path(stage, "v3_baseline_run_status.csv"))
atomic_csv(baseline_fit, file.path(stage, "v3_baseline_fit_summary.csv"))
atomic_csv(
  baseline_diagnostics, file.path(stage, "v3_baseline_mcmc_diagnostics.csv")
)
atomic_csv(
  baseline_recovery, file.path(stage, "v3_baseline_recovery_summary.csv")
)
atomic_csv(
  baseline_heterogeneity,
  file.path(stage, "v3_baseline_heterogeneity_summary.csv")
)
for (file in c(
  "source_state.json", "runtime_binding.json", "resource_summary.csv",
  "wrapper_closeout.csv", "preflight_gates.csv", "reference_gates.csv",
  "benchmark_summary.csv"
)) copy_required(baseline_dir, file, paste0("v3_baseline_", file))

adjudication_files <- c(
  "source_state.json", "runtime_binding.json", "adjudication_config.json",
  "decision.csv", "fit_summary.csv", "baseline_comparison.csv",
  "block_stability.csv", "chain_summary.csv", "conditional_parity.csv",
  "heterogeneity_summary.csv", "mcmc_diagnostics.csv", "pathology_summary.csv",
  "prefix_parity.csv", "preflight_gates.csv", "recovery_summary.csv",
  "resource_summary.csv", "stage_status.csv", "worker_contract_self_test.csv",
  "wrapper_closeout.csv", "closeout.json"
)
for (file in adjudication_files) {
  copy_required(adjudication_dir, file, paste0("adjudication_", file))
}

receipt <- list(
  schema_version = "rqrgibbs_oracle_tilt_v3_nonpromotion_evidence/1.0.0",
  active_manuscript_campaign = "publication_v2",
  active_manuscript_source_commit = v2$source_commit,
  baseline_campaign = "publication_v3",
  baseline_source_commit = v3$source_commit,
  baseline_completed_chains = 27L,
  baseline_strict_pass_cells = 5L,
  baseline_target_cells = 6L,
  failed_cell = "dlm/SH",
  adjudication_source_commit = adj$source_commit,
  adjudication_completed_chains = 5L,
  adjudication_retained_draws = 60000L,
  adjudication_prefix_checks_passed = 15L,
  adjudication_prefix_checks_total = 15L,
  adjudication_numerical_repairs = 0L,
  adjudication_strict_diagnostics_pass = TRUE,
  adjudication_heterogeneity_pass = FALSE,
  adjudication_width_contrast_relative_error =
    adjudication_fit$width_contrast_relative_error,
  adjudication_maximum_width_contrast_relative_error =
    adj$maximum_width_contrast_relative_error,
  automatic_promotion_eligible = FALSE,
  manuscript_illustration_evidence_eligible = FALSE,
  raw_chain_objects_included = FALSE,
  response_predictive_analysis = FALSE,
  simulation_study = FALSE
)
atomic_json(receipt, file.path(stage, "evidence_receipt.json"))

readme <- c(
  "# Version-3 oracle-tilt non-promotion evidence",
  "",
  "This compact bundle records the completed version-3 single-data",
  "illustration and its one prespecified DLM/SH adjudication. It is an audit",
  "record, not manuscript evidence and not a repeated-sample simulation study.",
  "",
  "The baseline completed all 27 chains. Five of six cells passed strictly;",
  "DLM/SH failed. Exact continuation to 12,000 retained draws per chain",
  "reproduced all 15 saved prefixes bitwise, used zero numerical repairs, and",
  "passed all 137 maintained MCMC diagnostics. The width-contrast relative",
  sprintf(
    "error was %.6f, exceeding the frozen %.2f limit.",
    adjudication_fit$width_contrast_relative_error,
    adj$maximum_width_contrast_relative_error
  ),
  "",
  "Therefore the residual discrepancy is treated as model-fit evidence rather",
  "than Monte Carlo error. Automatic promotion, gate relaxation, and another",
  "same-data rerun are prohibited. The validated version-2 bundle remains the",
  "source for manuscript figures and tables.",
  "",
  "The bundle contains only compact CSV/JSON evidence. Raw chains, fitted",
  "objects, runtime libraries, and logs are intentionally excluded."
)
writeLines(readme, file.path(stage, "README.md"), useBytes = TRUE)

relative <- sort(list.files(stage, recursive = TRUE, all.files = FALSE))
manifest <- data.frame(
  path = relative,
  bytes = as.numeric(file.info(file.path(stage, relative))$size),
  sha256 = vapply(
    file.path(stage, relative), digest::digest, character(1L),
    algo = "sha256", file = TRUE, serialize = FALSE
  ),
  stringsAsFactors = FALSE
)
atomic_csv(manifest, file.path(stage, "artifact_manifest.csv"))
if (!file.rename(stage, output_dir)) {
  oti_stop("Could not atomically publish the non-promotion evidence directory.")
}
cat("Packaged compact non-promotion evidence at", output_dir, "\n")
