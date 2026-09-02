#!/usr/bin/env Rscript

default_script <- "application/scripts/85_run_pharma_tolerance_application.R"
arguments <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_path <- sub(file_arg, "", arguments[startsWith(arguments, file_arg)][1L])
if (!length(script_path) || is.na(script_path)) script_path <- default_script
script_path <- normalizePath(script_path, winslash = "/", mustWork = TRUE)
repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."),
                           winslash = "/", mustWork = TRUE)
setwd(repo_root)

source(file.path(repo_root, "application", "scripts", "lib",
                 "pharma_tolerance_application.R"))

args <- commandArgs(trailingOnly = TRUE)
mode <- tolower(pta_arg_value(args, "--mode=", "smoke"))
config_path <- pta_arg_value(
  args,
  "--config=",
  file.path("application", "config",
            "pharma_tolerance_application_20260902.json")
)
workers <- as.integer(pta_arg_value(args, "--workers=", "1"))
if (!is.finite(workers) || workers < 1L) workers <- 1L

config <- pta_read_config(config_path)

if (identical(mode, "health-check-read-only")) {
  output_dir <- normalizePath(
    pta_arg_value(args, "--output-dir=", pta_arg_value(args, "--run-dir=", "")),
    winslash = "/",
    mustWork = TRUE
  )
  required <- c(
    "pharma_application_results.csv",
    "pharma_application_summary.csv",
    "pharma_application_dependence_sensitivity.csv",
    "manifest.json"
  )
  missing <- required[!file.exists(file.path(output_dir, required))]
  if (length(missing)) {
    pta_stop("Application run is missing file(s): ",
             paste(missing, collapse = ", "))
  }
  manifest <- jsonlite::read_json(file.path(output_dir, "manifest.json"),
                                  simplifyVector = TRUE)
  results <- pta_read_csv(file.path(output_dir, "pharma_application_results.csv"))
  returned <- mean(results$interval_returned)
  cat("Pharmaceutical application health check passed.\n")
  cat("  run: ", output_dir, "\n", sep = "")
  cat("  mode: ", manifest$mode, "\n", sep = "")
  cat("  rows: ", nrow(results), "\n", sep = "")
  cat("  interval-production rate: ",
      formatC(100 * returned, digits = 1, format = "f"), "%\n", sep = "")
  quit(status = 0L)
}

if (!mode %in% c("preflight", "smoke", "confirmatory")) {
  pta_stop("Unsupported application mode: ", mode)
}

if (identical(mode, "preflight")) {
  pta_prepare_data(config, repo_root, overwrite = FALSE)
  pta_scan_calibration(config, repo_root)
  pta_mti_policy_row(config, repo_root)
  pta_mti_profile_config(config, repo_root)
  cat("Pharmaceutical application preflight passed.\n")
  quit(status = 0L)
}

default_output <- file.path(
  config$paths$output_root,
  paste0(mode, "_", format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"))
)
output_dir <- normalizePath(
  pta_arg_value(args, "--output-dir=", default_output),
  winslash = "/",
  mustWork = FALSE
)
if (dir.exists(output_dir) || file.exists(output_dir)) {
  pta_stop("The output directory must be fresh: ", output_dir)
}

out <- pta_run_application(
  config = config,
  repo_root = repo_root,
  mode = mode,
  output_dir = output_dir,
  workers = workers
)

cat("Pharmaceutical application run completed.\n")
cat("  output: ", out, "\n", sep = "")
