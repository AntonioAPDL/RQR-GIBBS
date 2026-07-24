#!/usr/bin/env Rscript

# Validate one completed bounded RQR-DLM run and promote only its compact
# evidence. Full fitted objects, process-level telemetry, and retained
# component-scale conditional rows remain in the ignored run directory.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) {
  stop(
    "Usage: 11_promote_rqr_dlm_bounded_evidence.R <run_dir> <evidence_dir>",
    call. = FALSE
  )
}

run_dir <- normalizePath(args[[1L]], mustWork = TRUE)
evidence_dir <- normalizePath(
  args[[2L]], mustWork = FALSE
)

if (dir.exists(evidence_dir) &&
    length(list.files(evidence_dir, all.files = TRUE, no.. = TRUE)) > 0L) {
  stop("Evidence directory must not exist or must be empty.", call. = FALSE)
}
dir.create(evidence_dir, recursive = TRUE, showWarnings = FALSE)

sha256_file <- function(path) {
  tolower(digest::digest(
    file = path, algo = "sha256", serialize = FALSE
  ))
}

read_csv <- function(name) {
  utils::read.csv(
    file.path(run_dir, name),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

source_manifest <- read_csv("artifact_hashes.csv")
stopifnot(
  identical(names(source_manifest), c("sha256", "bytes", "path")),
  !anyDuplicated(source_manifest$path),
  all(grepl("^[0-9a-f]{64}$", source_manifest$sha256)),
  all(is.finite(source_manifest$bytes)),
  all(source_manifest$bytes >= 0)
)

actual_files <- list.files(
  run_dir, recursive = TRUE, full.names = FALSE,
  all.files = TRUE, no.. = TRUE
)
actual_files <- actual_files[
  file.info(file.path(run_dir, actual_files))$isdir %in% FALSE
]
actual_files <- sort(setdiff(actual_files, "artifact_hashes.csv"))
stopifnot(identical(sort(source_manifest$path), actual_files))

source_index <- match(actual_files, source_manifest$path)
source_paths <- file.path(run_dir, actual_files)
stopifnot(
  identical(
    as.numeric(file.info(source_paths)$size),
    as.numeric(source_manifest$bytes[source_index])
  ),
  identical(
    unname(vapply(source_paths, sha256_file, character(1L))),
    source_manifest$sha256[source_index]
  )
)

run_manifest <- jsonlite::read_json(
  file.path(run_dir, "run_manifest.json"), simplifyVector = TRUE
)
stopifnot(
  identical(run_manifest$schema_version, "rqrgibbs_dlm_bounded_run/3.0.0"),
  identical(run_manifest$mode, "execute-bounded"),
  identical(run_manifest$status, "passed"),
  identical(run_manifest$requested_fit_count, 24L),
  identical(run_manifest$bounded_fit_count, 24L),
  identical(run_manifest$diagnostic_count, 897L),
  identical(run_manifest$diagnostic_pass_count, 897L),
  isTRUE(run_manifest$reference_binding_verified),
  isTRUE(run_manifest$generalized_bayes),
  !isTRUE(run_manifest$response_likelihood),
  !isTRUE(run_manifest$response_prediction_contract),
  !isTRUE(run_manifest$production_simulation)
)

diagnostics <- read_csv("chain_diagnostics.csv")
fit_audit <- read_csv("fit_audit.csv")
checkpoints <- read_csv("checkpoint_manifest.csv")
failures <- read_csv("failure_log.csv")
future_checks <- read_csv("missing_future_checks.csv")
provenance <- read_csv("provenance_checks.csv")
run_status <- read_csv("run_status.csv")
resource <- read_csv("resource_summary.csv")
local_hashes <- read_csv("local_chain_hashes.csv")
component_conditionals <- read_csv("component_scale_conditionals.csv")

stopifnot(
  nrow(diagnostics) == 897L,
  all(diagnostics$pass),
  max(diagnostics$rhat) <= 1.01,
  min(diagnostics$ess_bulk) >= 1000,
  min(diagnostics$ess_tail) >= 1000,
  nrow(fit_audit) == 24L,
  all(fit_audit$forecast_repair_count == 0),
  all(fit_audit$numerical_repair_count == 0),
  all(fit_audit$exact_joint_target),
  all(fit_audit$target_numerical_eligible),
  all(fit_audit$reproducibility_eligible),
  all(fit_audit$promotion_eligible),
  nrow(checkpoints) == 24L,
  all(checkpoints$completed_iterations == 8000L),
  !anyDuplicated(checkpoints$checkpoint_digest),
  nrow(failures) == 0L,
  nrow(future_checks) == 24L,
  all(future_checks$missing_ordinates_finite),
  all(future_checks$future_values_finite),
  all(future_checks$future_draw_index_sequential),
  all(future_checks$future_repair_count == 0),
  !any(future_checks$response_simulation_contract),
  nrow(provenance) == 24L,
  all(provenance$primary_runtime_source_match),
  all(provenance$expected_commit_match),
  all(provenance$reproducibility_eligible),
  nrow(run_status) == 24L,
  all(run_status$status == "completed"),
  nrow(local_hashes) == 24L,
  nrow(component_conditionals) == 96000L,
  identical(
    sort(unique(component_conditionals$component)),
    c("regression", "trend")
  )
)

resource_map <- stats::setNames(resource$value, resource$metric)
stopifnot(
  identical(resource_map[["runner_exit_status"]], "0"),
  identical(resource_map[["final_pgid_empty"]], "TRUE"),
  identical(resource_map[["monitor_fault_test_pass"]], "TRUE"),
  identical(resource_map[["sampled_limit_triggered"]], "FALSE"),
  identical(resource_map[["hard_timeout_triggered"]], "FALSE")
)

# Reopen every ignored fit, verify its file identity, and compare its saved
# integrity fields with the compact manifests.
for (index in seq_len(nrow(local_hashes))) {
  local_path <- file.path(run_dir, local_hashes$relative_path[[index]])
  stopifnot(
    file.exists(local_path),
    identical(
      as.numeric(file.info(local_path)$size),
      as.numeric(local_hashes$bytes[[index]])
    ),
    identical(sha256_file(local_path), local_hashes$sha256[[index]])
  )
  fit <- readRDS(local_path)
  checkpoint_index <- match(
    local_hashes$fit_id[[index]], checkpoints$fit_id
  )
  stopifnot(
    inherits(fit, "rqr_dlm_mcmc"),
    identical(
      fit$checkpoint_digest,
      local_hashes$checkpoint_digest[[index]]
    ),
    identical(
      fit$continuation_history_digest,
      local_hashes$history_digest[[index]]
    ),
    identical(
      local_hashes$checkpoint_digest[[index]],
      checkpoints$checkpoint_digest[[checkpoint_index]]
    ),
    identical(
      local_hashes$history_digest[[index]],
      checkpoints$history_digest[[checkpoint_index]]
    ),
    identical(
      digest::digest(fit, algo = "sha256", serialize = TRUE),
      checkpoints$published_object_digest[[checkpoint_index]]
    )
  )
  rm(fit)
}

component_conditionals$learning_rate_mode <- sub(
  "^shared_component_scale_trend_regression__(.*?)__chain[0-9]+$",
  "\\1",
  component_conditionals$fit_id
)
split_key <- interaction(
  component_conditionals$learning_rate_mode,
  component_conditionals$component,
  drop = TRUE, lex.order = TRUE
)
component_summary <- do.call(
  rbind,
  lapply(split(component_conditionals, split_key), function(block) {
    data.frame(
      learning_rate_mode = block$learning_rate_mode[[1L]],
      component = block$component[[1L]],
      chains = length(unique(block$fit_id)),
      retained_draws = nrow(block),
      scale_mean = mean(block$scale),
      scale_sd = stats::sd(block$scale),
      scale_q05 = unname(stats::quantile(block$scale, 0.05)),
      scale_q50 = unname(stats::quantile(block$scale, 0.50)),
      scale_q95 = unname(stats::quantile(block$scale, 0.95)),
      posterior_shape_min = min(block$posterior_shape),
      posterior_shape_max = max(block$posterior_shape),
      posterior_rate_min = min(block$posterior_rate),
      posterior_rate_max = max(block$posterior_rate),
      stringsAsFactors = FALSE
    )
  })
)
rownames(component_summary) <- NULL

elapsed <- fit_audit$elapsed_seconds
validation_summary <- data.frame(
  metric = c(
    "bounded_fits_completed",
    "bounded_fits_failed",
    "diagnostics_passed",
    "diagnostics_total",
    "maximum_rhat",
    "minimum_bulk_ess",
    "minimum_tail_ess",
    "numerical_repairs",
    "forecast_repairs",
    "exact_joint_target_fits",
    "reproducibility_eligible_fits",
    "promotion_eligible_fits",
    "full_chain_bytes_ignored",
    "fit_elapsed_seconds_total",
    "fit_elapsed_seconds_minimum",
    "fit_elapsed_seconds_maximum",
    "sampled_peak_rss_kib",
    "final_process_group_empty"
  ),
  value = c(
    nrow(fit_audit),
    nrow(failures),
    sum(diagnostics$pass),
    nrow(diagnostics),
    max(diagnostics$rhat),
    min(diagnostics$ess_bulk),
    min(diagnostics$ess_tail),
    sum(fit_audit$numerical_repair_count),
    sum(fit_audit$forecast_repair_count),
    sum(fit_audit$exact_joint_target),
    sum(fit_audit$reproducibility_eligible),
    sum(fit_audit$promotion_eligible),
    sum(local_hashes$bytes),
    sum(elapsed),
    min(elapsed),
    max(elapsed),
    resource_map[["sampled_process_group_peak_rss_kib"]],
    resource_map[["final_pgid_empty"]]
  ),
  stringsAsFactors = FALSE
)

compact_files <- c(
  "artifact_hashes.csv",
  "chain_diagnostics.csv",
  "checkpoint_manifest.csv",
  "estimand_schema.csv",
  "failure_log.csv",
  "fit_audit.csv",
  "fit_plan.csv",
  "fixture_construction.csv",
  "future_root_summaries.csv",
  "initialization_manifest.csv",
  "local_chain_hashes.csv",
  "missing_future_checks.csv",
  "monitor_fault_test.csv",
  "posterior_summaries.csv",
  "provenance_checks.csv",
  "resource_summary.csv",
  "root_swap_sidecar.csv",
  "run_manifest.json",
  "run_status.csv",
  "runtime_toolchain.json",
  "session_info.txt",
  "wrapper_closeout.csv"
)
stopifnot(all(file.exists(file.path(run_dir, compact_files))))
copied <- file.copy(
  file.path(run_dir, compact_files),
  file.path(evidence_dir, compact_files),
  overwrite = FALSE,
  copy.mode = TRUE,
  copy.date = TRUE
)
stopifnot(all(copied))

utils::write.csv(
  component_summary,
  file.path(evidence_dir, "component_scale_conditional_summary.csv"),
  row.names = FALSE
)
utils::write.csv(
  validation_summary,
  file.path(evidence_dir, "validation_summary.csv"),
  row.names = FALSE
)

evidence_files <- sort(list.files(
  evidence_dir, recursive = TRUE, full.names = TRUE
))
evidence_manifest <- data.frame(
  sha256 = vapply(evidence_files, sha256_file, character(1L)),
  bytes = as.numeric(file.info(evidence_files)$size),
  path = basename(evidence_files),
  stringsAsFactors = FALSE
)
utils::write.csv(
  evidence_manifest,
  file.path(evidence_dir, "evidence_manifest.csv"),
  row.names = FALSE,
  quote = TRUE
)

cat("Bounded RQR-DLM evidence promoted.\n")
cat("  source:", run_dir, "\n")
cat("  target:", evidence_dir, "\n")
cat("  source artifacts verified:", nrow(source_manifest), "\n")
cat("  ignored fits reopened:", nrow(local_hashes), "\n")
cat("  diagnostics:", sum(diagnostics$pass), "/", nrow(diagnostics), "\n")
cat("  compact files:", nrow(evidence_manifest), "\n")
