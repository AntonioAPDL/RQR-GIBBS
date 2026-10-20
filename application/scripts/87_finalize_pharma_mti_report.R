#!/usr/bin/env Rscript

args <- commandArgs(TRUE)
arguments <- commandArgs(FALSE)
path <- sub("--file=", "", arguments[startsWith(arguments, "--file=")][1], fixed = TRUE)
root <- normalizePath(file.path(dirname(path), "..", ".."), mustWork = TRUE)
setwd(root)
for (name in c("pharma_tolerance_application.R", "pharma_mti_application.R", "pharma_mti_reporting.R")) {
  source(file.path("application/scripts/lib", name))
}
config <- pma_read_config(pta_arg_value(args, "--config=", pma_default_config))
results <- pta_arg_value(args, "--results-csv=", file.path(pma_canonical_dir, "results.csv"))
inputs <- pma_read_report_inputs(config, results)
figures <- pta_read_csv("figures/generated/pharma_application_figure_manifest.csv")
if (nrow(figures) != 3L ||
    any(figures$method_ids != paste(pma_methods, collapse = ";")) ||
    any(figures$source_results_sha256 != pta_file_sha256(results))) {
  pta_stop("Figure records do not match the current canonical result.")
}
for (i in seq_len(nrow(figures))) pma_verify_hash(figures$relative_path[i], figures$sha256[i])
tables <- list.files("tables", "^pharma_application_.*\\.(tex|csv)$", full.names = TRUE)
paths <- c(tables, figures$relative_path, "figures/generated/pharma_application_figure_manifest.csv",
           "figures/generated/pharma_application_plotted_intervals.csv",
           "figures/data/pharma_application_data_provenance.csv")
manifest <- inputs$manifest
manifest$derived_tables_and_figures <- pma_file_record(paths)
manifest$reporting_sources <- pma_file_record(c(
  "application/scripts/lib/pharma_mti_reporting.R", "tables/generate_pharma_mti_application_tables.R",
  "figures/generate_pharma_mti_application_figures.R", "application/scripts/87_finalize_pharma_mti_report.R"))
manifest$reporting_commit <- pma_git("rev-parse", "HEAD")
manifest$reporting_status <- pma_git("status", "--porcelain")
pta_atomic_write_json(manifest, file.path(dirname(results), "manifest.json"))
cat("Verified and recorded all pharmaceutical table and figure hashes.\n")
