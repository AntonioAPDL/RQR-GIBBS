#!/usr/bin/env Rscript

arguments <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_path <- sub(file_arg, "", arguments[startsWith(arguments, file_arg)][1L])
if (!length(script_path) || is.na(script_path)) {
  script_path <-
    "application/scripts/51_promote_oracle_tilt_v3_revised_evidence.R"
}
script_dir <- dirname(normalizePath(script_path, winslash = "/", mustWork = TRUE))
for (file in c(
  "32_oracle_tilt_illustration_utils.R",
  "33_oracle_tilt_forensic_utils.R",
  "34_oracle_tilt_publication_utils.R",
  "42_oracle_tilt_publication_v3_utils.R"
)) source(file.path(script_dir, file))

trailing <- commandArgs(trailingOnly = TRUE)
arg_value <- function(prefix, default = "") {
  hit <- trailing[startsWith(trailing, prefix)]
  if (length(hit)) sub(prefix, "", hit[1L], fixed = TRUE) else default
}
repo_root <- normalizePath(file.path(script_dir, "..", ".."), mustWork = TRUE)
baseline_root <- normalizePath(
  arg_value("--baseline-dir="), winslash = "/", mustWork = TRUE
)
adjudication_root <- normalizePath(
  arg_value("--adjudication-dir="), winslash = "/", mustWork = TRUE
)
policy_path <- normalizePath(
  arg_value(
    "--policy=",
    file.path(
      repo_root, "application", "config",
      "oracle_tilt_c095_v3_revised_illustration_acceptance_20260805.json"
    )
  ), winslash = "/", mustWork = TRUE
)
output_argument <- arg_value(
  "--output-dir=",
  file.path(repo_root, "figures", "data", "oracle_tilt_c095_v3")
)
if (!startsWith(output_argument, "/")) {
  output_argument <- file.path(repo_root, output_argument)
}
output_root <- normalizePath(
  output_argument, winslash = "/", mustWork = FALSE
)
replace <- any(trailing == "--replace")
allowed_output_parent <- normalizePath(
  file.path(repo_root, "figures", "data"), mustWork = TRUE
)
if (!startsWith(output_root, paste0(allowed_output_parent, "/"))) {
  oti_stop("The promoted evidence must be a child of figures/data.")
}

read_csv <- function(root, relative) {
  path <- file.path(root, relative)
  if (!file.exists(path)) oti_stop("Missing compact evidence: ", path)
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}
read_json <- function(root, relative) {
  path <- file.path(root, relative)
  if (!file.exists(path)) oti_stop("Missing compact evidence: ", path)
  jsonlite::read_json(path, simplifyVector = TRUE)
}
assert_true <- function(value, message) {
  if (!isTRUE(value)) oti_stop(message)
}
scalar_number <- function(value, label) {
  if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
      !is.finite(value)) oti_stop(label, " must be one finite number.")
  as.numeric(value)
}

policy <- jsonlite::read_json(policy_path, simplifyVector = TRUE)
expected_policy_schema <-
  "rqrgibbs_oracle_tilt_revised_illustration_acceptance/1.0.0"
assert_true(
  identical(policy$schema_version, expected_policy_schema) &&
    identical(
      policy$baseline_source_commit,
      "99a088fbdd7c3f3ed18f99197294038f62dbfe41"
    ) &&
    identical(
      policy$adjudication_source_commit,
      "a3b39b394c6aa928eb38e9ed461281cdf743d00b"
    ) &&
    scalar_number(policy$original_width_contrast_relative_error_max,
                  "original threshold") == 0.20 &&
    scalar_number(policy$revised_width_contrast_relative_error_max,
                  "revised threshold") == 0.21 &&
    !isTRUE(policy$original_gate_passed) &&
    isTRUE(policy$post_hoc_revision_disclosed) &&
    isTRUE(policy$revised_illustration_tolerance_passed) &&
    isTRUE(policy$promotion_authorized),
  "The revised illustration-acceptance policy is invalid."
)

baseline_source <- read_json(baseline_root, "source_state.json")
assert_true(
  identical(baseline_source$source_commit, policy$baseline_source_commit) &&
    identical(baseline_source$config_sha256,
              policy$baseline_config_sha256) &&
    isTRUE(baseline_source$exact_runtime_bound),
  "The baseline source/config/runtime identity changed."
)
baseline_status <- read_csv(baseline_root, "run_status.csv")
assert_true(
  nrow(baseline_status) == 6L &&
    sum(baseline_status$chains_completed) == 27L &&
    sum(baseline_status$disposition == "strict_pass") == 5L &&
    identical(
      paste(
        baseline_status$family[baseline_status$disposition == "fail"],
        baseline_status$target[baseline_status$disposition == "fail"],
        sep = "/"
      ), "dlm/SH"
    ),
  "The baseline cell disposition changed."
)

baseline_cells <- c(
  "fixed_design_rqr", "fixed_design_et", "fixed_design_sh",
  "dlm_rqr", "dlm_et"
)
for (cell in baseline_cells) {
  root <- file.path(baseline_root, "cells", cell)
  otp_verify_manifest(root)
  receipt <- read_json(root, "cell_receipt.json")
  assert_true(
    isTRUE(receipt$eligible) && identical(receipt$disposition, "strict_pass"),
    paste0("Baseline cell is not a strict pass: ", cell)
  )
}

adjudication_source <- read_json(adjudication_root, "source_state.json")
decision <- read_csv(adjudication_root, "decision.csv")
fit_adjudication <- read_csv(adjudication_root, "fit_summary.csv")
diagnostics <- read_csv(adjudication_root, "mcmc_diagnostics.csv")
prefix <- read_csv(adjudication_root, "prefix_parity.csv")
resource <- read_csv(adjudication_root, "resource_summary.csv")
stages <- read_csv(adjudication_root, "stage_status.csv")
assert_true(
  identical(adjudication_source$source_commit,
            policy$adjudication_source_commit) &&
    identical(adjudication_source$base_source_commit,
              policy$baseline_source_commit) &&
    identical(adjudication_source$adjudication_config_sha256,
              policy$adjudication_config_sha256) &&
    isTRUE(adjudication_source$exact_runtime_bound),
  "The adjudication source/config/runtime identity changed."
)
observed_error <- scalar_number(
  fit_adjudication$width_contrast_relative_error,
  "observed width-contrast error"
)
assert_true(
  nrow(fit_adjudication) == 1L &&
    identical(fit_adjudication$family, "dlm") &&
    identical(fit_adjudication$target, "SH") &&
    fit_adjudication$n_chains == 5L &&
    fit_adjudication$retained_draws == 60000L &&
    fit_adjudication$numerical_repair_count == 0L &&
    observed_error > policy$original_width_contrast_relative_error_max &&
    observed_error <= policy$revised_width_contrast_relative_error_max &&
    abs(observed_error - policy$observed_width_contrast_relative_error) < 1e-12,
  "The DLM/SH result does not satisfy the disclosed revised tolerance."
)
assert_true(
  nrow(decision) == 1L && isTRUE(decision$hard_integrity_pass) &&
    isTRUE(decision$strict_diagnostics_pass) &&
    !isTRUE(decision$heterogeneity_pass) && !isTRUE(decision$strict_pass) &&
    nrow(diagnostics) == 137L && all(diagnostics$pass) &&
    nrow(prefix) == 15L && all(prefix$pass) &&
    all(prefix$bitwise_identical) &&
    all(prefix$maximum_absolute_difference == 0) &&
    nrow(stages) == 2L && all(stages$status == "completed") &&
    sum(stages$prefix_checks) == 15L && all(stages$prefix_pass) &&
    nrow(resource) == 1L && isTRUE(resource$pass) &&
    isTRUE(resource$final_pgid_empty) && !isTRUE(resource$timed_out) &&
    !isTRUE(resource$sampled_limit_triggered),
  "The adjudication computational-integrity contract did not pass."
)

# Raw worker objects were intentionally pruned after closeout. Verify every
# compact adjudication file used for promotion against its original manifest.
adjudication_allowlist <- c(
  "baseline_audit.csv", "baseline_comparison.csv", "block_stability.csv",
  "chain_summary.csv", "conditional_parity.csv", "decision.csv",
  "design_binding.csv", "endpoint_error_by_index.csv",
  "endpoint_error_density.csv", "endpoint_error_summary.csv",
  "fit_curves.csv", "fit_summary.csv", "heterogeneity_summary.csv",
  "mcmc_diagnostics.csv", "pathology_summary.csv", "prefix_parity.csv",
  "preflight_gates.csv", "provenance_audit.csv", "recovery_summary.csv",
  "runtime_binding.json", "source_state.json", "stage_status.csv",
  "worker_contract_self_test.csv", "adjudication_config.json",
  "resource_summary.csv", "wrapper_closeout.csv", "closeout.json"
)
original_manifest <- read_csv(
  adjudication_root, "wrapper_artifact_manifest.csv"
)
manifest_rows <- match(adjudication_allowlist, original_manifest$path)
assert_true(
  !anyNA(manifest_rows) && all(vapply(seq_along(adjudication_allowlist),
    function(index) {
      path <- file.path(adjudication_root, adjudication_allowlist[index])
      row <- manifest_rows[index]
      file.exists(path) &&
        file.info(path)$size == original_manifest$bytes[row] &&
        identical(oti_file_sha256(path), original_manifest$sha256[row])
    }, logical(1L))),
  "A compact adjudication file failed its original hash receipt."
)

if (file.exists(output_root) || dir.exists(output_root)) {
  if (!replace) oti_stop("Output exists; use --replace explicitly.")
  backup <- paste0(
    output_root, ".backup-", format(Sys.time(), "%Y%m%d%H%M%S")
  )
  if (!file.rename(output_root, backup)) {
    oti_stop("Could not preserve the prior promoted evidence.")
  }
}
stage <- tempfile(paste0(".", basename(output_root), "-"), dirname(output_root))
if (!dir.create(stage, recursive = TRUE, showWarnings = FALSE)) {
  oti_stop("Could not create the evidence staging directory.")
}
on.exit(if (dir.exists(stage)) unlink(stage, recursive = TRUE, force = TRUE),
        add = TRUE)

global_files <- c(
  "config.json", "design_contract.csv", "oracle_targets.csv",
  "tail_information.csv", "scale_information.csv", "preflight_gates.csv",
  "reference_gates.csv", "benchmark_summary.csv", "static_basis_audit.csv",
  "static_projection_audit.csv", "dynamic_projection_audit.csv",
  "dlm_time_contract.csv", "fixed_horizon_audit.csv",
  "seasonal_covariance_audit.csv", "dynamic_observability_audit.csv"
)
for (file in global_files) {
  if (!file.copy(file.path(baseline_root, file), file.path(stage, file))) {
    oti_stop("Could not stage baseline artifact: ", file)
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
    path <- file.path(
      baseline_root, "cells", cell, paste0(component, ".csv")
    )
    if (file.exists(path)) {
      utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
    } else NULL
  })
  adjudication_path <- file.path(
    adjudication_root, paste0(component, ".csv")
  )
  if (file.exists(adjudication_path)) {
    rows[[length(rows) + 1L]] <- utils::read.csv(
      adjudication_path, stringsAsFactors = FALSE, check.names = FALSE
    )
  }
  rows <- Filter(Negate(is.null), rows)
  if (length(rows)) {
    combined <- oti_rbind_fill(rows)
    if (identical(component, "fit_summary")) {
      combined$original_disposition <- combined$disposition
      combined$original_manuscript_illustration_evidence_eligible <-
        combined$manuscript_illustration_evidence_eligible
      dlm_sh <- combined$family == "dlm" & combined$target == "SH"
      combined$promotion_disposition <- "strict_pass"
      combined$promotion_disposition[dlm_sh] <-
        "accepted_revised_tolerance"
      combined$promotion_policy <- "original_frozen_contract"
      combined$promotion_policy[dlm_sh] <-
        "post_hoc_revised_illustration_tolerance_0.21"
      combined$original_width_contrast_relative_error_max <- 0.20
      combined$revised_width_contrast_relative_error_max <- 0.21
      combined$manuscript_illustration_evidence_eligible <- TRUE
      combined$disposition <- combined$promotion_disposition
    }
    otf_atomic_write_csv(
      combined, file.path(stage, paste0(component, ".csv"))
    )
  }
}

fit <- read_csv(stage, "fit_summary.csv")
assert_true(
  nrow(fit) == 6L && all(fit$manuscript_illustration_evidence_eligible) &&
    sum(fit$promotion_disposition == "strict_pass") == 5L &&
    sum(fit$promotion_disposition == "accepted_revised_tolerance") == 1L,
  "The reconciled six-cell promotion disposition is invalid."
)

support <- c(
  "baseline_audit.csv", "design_binding.csv", "prefix_parity.csv",
  "worker_contract_self_test.csv", "stage_status.csv",
  "block_stability.csv", "decision.csv", "baseline_comparison.csv",
  "adjudication_config.json", "source_state.json", "runtime_binding.json",
  "closeout.json", "resource_summary.csv", "wrapper_closeout.csv"
)
for (file in support) {
  if (!file.copy(
    file.path(adjudication_root, file),
    file.path(stage, paste0("adjudication_", file))
  )) oti_stop("Could not stage adjudication support artifact: ", file)
}
if (!file.copy(policy_path, file.path(stage, "acceptance_policy.json"))) {
  oti_stop("Could not stage the revised acceptance policy.")
}

receipt <- list(
  schema_version = "rqrgibbs_oracle_tilt_evidence/3.2.0",
  baseline_source_commit = baseline_source$source_commit,
  adjudication_source_commit = adjudication_source$source_commit,
  baseline_config_sha256 = baseline_source$config_sha256,
  adjudication_config_sha256 =
    adjudication_source$adjudication_config_sha256,
  acceptance_policy_sha256 = oti_file_sha256(policy_path),
  coverage_level = 0.95,
  innovation_contract = "affinely standardized AL_0.80(0,1)",
  fixed_design_n = 2400L,
  dlm_T = 1200L,
  dlm_n_observed = 1178L,
  target_cells = 6L,
  completed_chains = 27L,
  original_strict_pass_cells = 5L,
  revised_tolerance_accepted_cells = 1L,
  all_cells_original_strict_pass = FALSE,
  all_cells_accepted_for_illustration = TRUE,
  original_width_contrast_relative_error_max = 0.20,
  revised_width_contrast_relative_error_max = 0.21,
  observed_width_contrast_relative_error = observed_error,
  post_hoc_revision_disclosed = TRUE,
  dlm_sh_prefix_parity_pass = TRUE,
  maintained_diagnostics_passed = 137L,
  maintained_diagnostics_total = 137L,
  numerical_repair_count = 0L,
  exact_population_oracle_tilts = TRUE,
  cornish_fisher_used = FALSE,
  response_predictive_analysis = FALSE,
  simulation_study = FALSE,
  manuscript_illustration_evidence_eligible = TRUE,
  raw_chain_objects_included = FALSE
)
jsonlite::write_json(
  receipt, file.path(stage, "evidence_receipt.json"), pretty = TRUE,
  auto_unbox = TRUE, digits = NA
)
writeLines(c(
  "# Reconciled version-3 oracle-tilt illustration evidence",
  "",
  paste(
    "This compact bundle combines five original strict-pass cells with the",
    "completed longer-chain DLM/SH adjudication. All computational-integrity",
    "requirements passed, including 15/15 bitwise prefixes, 137/137 maintained",
    "diagnostics, zero numerical repairs, and the monitored resource contract."
  ),
  "",
  paste(
    "The original DLM/SH width-contrast tolerance was 0.20 and did not pass:",
    sprintf("the observed relative error was %.6f.", observed_error)
  ),
  paste(
    "A disclosed post hoc manuscript-illustration tolerance of 0.21 accepts",
    "this result. The original failure remains recorded in the source evidence",
    "and is not relabeled as a prespecified strict pass."
  ),
  "",
  paste(
    "These summaries concern interval-root generalized posteriors for two",
    "single frozen data sets. They are not response-predictive distributions",
    "and do not constitute a repeated-sample simulation study."
  )
), file.path(stage, "README.md"), useBytes = TRUE)

files <- sort(list.files(stage, full.names = TRUE, recursive = TRUE))
manifest <- oti_file_hashes(files, stage)
names(manifest)[names(manifest) == "relative_path"] <- "path"
utils::write.csv(
  manifest, file.path(stage, "evidence_manifest.csv"), row.names = FALSE
)
if (!file.rename(stage, output_root)) {
  oti_stop("Could not publish the reconciled version-3 evidence.")
}
on.exit(NULL, add = FALSE)
message("[oracle-tilt-v3-promotion] published: ", output_root)
