#!/usr/bin/env Rscript

arguments <- commandArgs(FALSE)
path <- sub("--file=", "", arguments[startsWith(arguments, "--file=")][1], fixed = TRUE)
repo_root <- normalizePath(file.path(dirname(path), "..", ".."), mustWork = TRUE)
setwd(repo_root)
source("application/scripts/lib/pharma_tolerance_application.R")
source("application/scripts/lib/pharma_mti_application.R")
args <- commandArgs(TRUE)
config <- pma_read_config(pta_arg_value(args, "--config=", pma_default_config))
if(!is.null(config$mti_profile))for(name in c("mti_profile_revision.R","mti_profile_review.R",
  "mti_fixed_policy_evaluation.R","mti_q_grid_study.R","mti_final_reporting.R","mti_profile_adoption.R"))
  source(file.path("application/scripts/lib",name))
mode <- pta_arg_value(args, "--mode=", "preflight")
if (!mode %in% c("data", "preflight", "smoke", "confirmatory", "publish", "health")) {
  pta_stop("Unsupported revised application mode: ", mode)
}
if (mode %in% c("publish", "health")) {
  run_dir <- pta_arg_value(args, "--run-dir=", "")
  if (!nzchar(run_dir)) pta_stop("--run-dir is required.")
  manifest <- pma_verify_run(run_dir, config)
  if (mode == "publish") print(pma_publish(run_dir, config, repo_root)) else
    print(manifest[c("mode", "expected_result_rows", "observed_result_rows", "return_counts")])
} else {
  context <- if(is.null(config$mti_profile))pma_context(config,repo_root) else mpa_application_context(config)
  if (mode %in% c("data", "preflight")) {
    splits <- pma_make_splits(context$data, config, "smoke")
    cat("Verified raw data, frozen calibration inputs, and split construction.\n")
    cat("Internal retained count:", context$scan$retained_count, "\n")
  } else {
    out <- pta_arg_value(args, "--output-dir=", file.path(config$paths$output_root,
      paste0(mode, "_", format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"))))
    workers <- as.integer(pta_arg_value(args, "--workers=", as.character(config$execution$workers)))
    print(pma_run(context, mode, out, workers))
  }
}
