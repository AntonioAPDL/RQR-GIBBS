#!/usr/bin/env Rscript

args <- commandArgs(TRUE)
arguments <- commandArgs(FALSE)
path <- sub("--file=", "", arguments[startsWith(arguments, "--file=")][1], fixed = TRUE)
root <- normalizePath(file.path(dirname(path), ".."), mustWork = TRUE)
setwd(root)
for (name in c("pharma_tolerance_application.R", "pharma_mti_application.R", "pharma_mti_reporting.R")) {
  source(file.path("application/scripts/lib", name))
}
config <- pma_read_config(pta_arg_value(args, "--config=", pma_default_config))
output <- pta_arg_value(args, "--output-dir=", "tables")
results <- pta_arg_value(args, "--results-csv=", file.path(pma_canonical_dir, "results.csv"))
explicit_results <- any(startsWith(args, "--results-csv="))
committed <- identical(pta_arg_value(args, "--use-committed-inputs=", "false"), "true")
if (!file.exists(results) && committed && !explicit_results) {
  files <- list.files("tables", "^pharma_application_.*\\.(tex|csv)$", full.names = TRUE)
  required <- file.path("tables", c("pharma_application_primary_summary.tex",
    "pharma_application_supplement_summary.tex", "pharma_application_numbers.tex"))
  if (!all(file.exists(required))) pta_stop("Explicit committed manuscript inputs are incomplete.")
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  if (normalizePath(output) != normalizePath("tables")) {
    if (!all(file.copy(files, output, overwrite = TRUE))) pta_stop("Cannot copy committed tables.")
  }
  cat("Using explicitly requested committed application inputs.\n")
} else {
  inputs <- pma_read_report_inputs(config, results)
  pma_generate_tables(inputs, output)
  cat("Generated pharmaceutical MTI application tables from:", results, "\n")
}
