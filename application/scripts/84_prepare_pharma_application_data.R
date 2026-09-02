#!/usr/bin/env Rscript

default_script <- "application/scripts/84_prepare_pharma_application_data.R"
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
config_path <- pta_arg_value(
  args,
  "--config=",
  file.path("application", "config",
            "pharma_tolerance_application_20260902.json")
)
mode <- tolower(pta_arg_value(args, "--mode=", "preflight"))
overwrite <- tolower(pta_arg_value(args, "--overwrite=", "false")) %in%
  c("true", "t", "1", "yes", "y")

if (!mode %in% c("preflight", "prepare")) {
  pta_stop("Unsupported data-preparation mode: ", mode)
}

config <- pta_read_config(config_path)
result <- pta_prepare_data(config, repo_root, overwrite = overwrite)

cat("Pharmaceutical application data check passed.\n")
cat("  raw file: ", result$raw_path, "\n", sep = "")
cat("  clean file: ", result$clean_path, "\n", sep = "")
cat("  product rows: ", result$provenance$product_rows[[1L]], "\n", sep = "")
cat("  diagnostics: tables/pharma_application_response_diagnostics.csv\n")
