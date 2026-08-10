#!/usr/bin/env Rscript

arguments <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_path <- sub(file_arg, "", arguments[startsWith(arguments, file_arg)][1L])
if (!length(script_path) || is.na(script_path)) {
  script_path <- "application/scripts/62_package_oracle_mean_tilt_validation.R"
}
script_dir <- dirname(normalizePath(script_path, mustWork = TRUE))
repo_root <- normalizePath(file.path(script_dir, "..", ".."), mustWork = TRUE)
setwd(repo_root)
source(file.path(script_dir, "60_oracle_mean_tilt_validation_utils.R"))

trailing <- commandArgs(trailingOnly = TRUE)
arg_value <- function(prefix, default = NULL) {
  hit <- trailing[startsWith(trailing, prefix)]
  if (length(hit)) sub(prefix, "", hit[[1L]], fixed = TRUE) else default
}
run_dir <- normalizePath(
  arg_value("--run-dir=", ""), winslash = "/", mustWork = TRUE
)
output_dir <- normalizePath(
  arg_value(
    "--output-dir=",
    file.path("figures", "data", "oracle_mean_tilt_validation_v1")
  ), winslash = "/", mustWork = FALSE
)
if (dir.exists(output_dir) || file.exists(output_dir)) {
  omtv_stop("Compact validation evidence is append-only; output already exists.")
}
closeout_path <- file.path(run_dir, "validation_closeout.json")
if (!file.exists(closeout_path)) omtv_stop("The validation closeout is missing.")
closeout <- jsonlite::read_json(closeout_path, simplifyVector = TRUE)
if (!identical(
      closeout$schema_version,
      "rqrgibbs_oracle_mean_tilt_closeout/1.0.0"
    ) || !isTRUE(closeout$pass) ||
    !isTRUE(closeout$terminal_accounting_complete) ||
    !isTRUE(closeout$failed_fits_retained_in_denominator) ||
    !identical(as.integer(closeout$integrity_failures), 0L)) {
  omtv_stop("The repeated-DGP run is not eligible for compact packaging.")
}
required <- c(
  "config.json", "source_state.json", "input_bundle_binding.csv",
  "fit_plan.csv", "rng_ledger.csv", "run_status.csv",
  "replication_estimands.csv",
  "mcmc_diagnostics.csv", "endpoint_summaries.csv",
  "failure_ledger.csv", "validation_closeout.json",
  "task_artifact_manifest.csv", "collection_artifact_manifest.csv"
)
missing <- required[!file.exists(file.path(run_dir, required))]
if (length(missing)) {
  omtv_stop("Required compact artifacts are missing: ", paste(missing, collapse = ", "))
}
verify_manifest <- function(manifest_path, root) {
  manifest <- utils::read.csv(manifest_path, stringsAsFactors = FALSE)
  if (!nrow(manifest) || anyDuplicated(manifest$path) ||
      !all(c("path", "bytes", "sha256") %in% names(manifest)) ||
      !all(vapply(seq_len(nrow(manifest)), function(ii) {
        candidate <- file.path(root, manifest$path[[ii]])
        file.exists(candidate) && !dir.exists(candidate) &&
          unname(file.info(candidate)$size) == manifest$bytes[[ii]] &&
          identical(
            digest::digest(file = candidate, algo = "sha256"),
            manifest$sha256[[ii]]
          )
      }, logical(1L)))) {
    omtv_stop("Artifact-manifest verification failed: ", manifest_path)
  }
  invisible(manifest)
}
verify_manifest(file.path(run_dir, "task_artifact_manifest.csv"), run_dir)
verify_manifest(
  file.path(run_dir, "collection_artifact_manifest.csv"), run_dir
)
precision_files <- list.files(
  run_dir, pattern = "^precision_decision_[0-9]{4}\\.csv$", full.names = TRUE
)
if (!length(precision_files)) {
  omtv_stop("At least one frozen-checkpoint precision decision is required.")
}
precision <- utils::read.csv(
  precision_files[[length(precision_files)]], stringsAsFactors = FALSE
)
if (!nrow(precision) || !all(precision$primary_precision_pass)) {
  omtv_stop("The final repeated-sample precision decision is not passing.")
}
estimands <- utils::read.csv(
  file.path(run_dir, "replication_estimands.csv"), stringsAsFactors = FALSE
)
status <- utils::read.csv(
  file.path(run_dir, "run_status.csv"), stringsAsFactors = FALSE
)
if (anyDuplicated(status$task_id) || nrow(status) != closeout$planned_tasks) {
  omtv_stop("Run-status accounting is inconsistent with the closeout.")
}

metric_names <- c(
  "lower_bias", "upper_bias", "lower_mae", "upper_mae",
  "endpoint_rmse", "midpoint_bias", "width_bias", "width_ratio",
  "conditional_content_error", "mean_excess_target_risk",
  "lower_credible_inclusion", "upper_credible_inclusion",
  "elapsed_seconds"
)
groups <- split(
  estimands,
  interaction(
    estimands$scenario_id, estimands$model_family, estimands$target,
    drop = TRUE
  )
)
aggregate_rows <- lapply(groups, function(data) {
  key <- data[1L, c("scenario_id", "model_family", "target"), drop = FALSE]
  values <- unlist(lapply(metric_names, function(metric) {
    x <- data[[metric]]
    c(mean = mean(x), sd = stats::sd(x), mcse = stats::sd(x) / sqrt(length(x)))
  }))
  names(values) <- unlist(lapply(metric_names, function(metric) {
    paste(metric, c("mean", "sd", "mcse"), sep = "_")
  }))
  cbind(key, as.data.frame(as.list(values)), replications = nrow(data))
})
aggregate_table <- do.call(rbind, aggregate_rows)
failure_summary <- stats::aggregate(
  cbind(planned = rep(1L, nrow(status)), failed = as.integer(!status$pass)),
  by = status[c("scenario_id", "model_family", "target")], sum
)
failure_summary$failure_rate <- failure_summary$failed /
  failure_summary$planned

stage <- tempfile(paste0(".", basename(output_dir), "-"), dirname(output_dir))
dir.create(stage, recursive = TRUE, showWarnings = FALSE)
on.exit(unlink(stage, recursive = TRUE, force = TRUE), add = TRUE)
for (name in required) {
  if (!file.copy(file.path(run_dir, name), file.path(stage, name))) {
    omtv_stop("Could not stage compact artifact: ", name)
  }
}
utils::write.csv(precision, file.path(stage, "final_precision_decision.csv"),
                 row.names = FALSE)
utils::write.csv(aggregate_table, file.path(stage, "aggregate_estimands.csv"),
                 row.names = FALSE)
utils::write.csv(failure_summary, file.path(stage, "failure_summary.csv"),
                 row.names = FALSE)
receipt <- list(
  schema_version = "rqrgibbs_oracle_mean_tilt_evidence/1.0.0",
  campaign_id = closeout$campaign_id,
  planned_tasks = closeout$planned_tasks,
  completed_fit_artifacts = closeout$completed_fit_artifacts,
  structured_failures = closeout$structured_failures,
  target_triplets_paired_by_dgp_replication = TRUE,
  exact_population_tilts = TRUE,
  tilt_definition = omtv_tilt_definition(),
  oracle_schema = omtv_oracle_schema(),
  failed_fits_retained_in_denominator = TRUE,
  response_likelihood = FALSE,
  response_predictive_draws = FALSE,
  cornish_fisher_used = FALSE,
  exal_used = FALSE,
  desn_included = FALSE,
  packaged_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
)
jsonlite::write_json(
  receipt, file.path(stage, "evidence_receipt.json"), pretty = TRUE,
  auto_unbox = TRUE, digits = NA
)
files <- list.files(stage, full.names = TRUE, recursive = TRUE)
manifest <- data.frame(
  path = substring(files, nchar(stage) + 2L),
  bytes = unname(file.info(files)$size),
  sha256 = vapply(files, digest::digest, character(1L),
                  file = TRUE, algo = "sha256"),
  stringsAsFactors = FALSE
)
utils::write.csv(manifest, file.path(stage, "artifact_manifest.csv"),
                 row.names = FALSE)
if (!file.rename(stage, output_dir)) {
  omtv_stop("Atomic publication of compact validation evidence failed.")
}
message("[oracle-mean-tilt-validation] compact evidence packaged: ", output_dir)
