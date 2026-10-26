#!/usr/bin/env Rscript
arguments <- commandArgs(FALSE)
script <- sub("--file=", "", arguments[startsWith(arguments, "--file=")][1], fixed = TRUE)
setwd(normalizePath(file.path(dirname(script), "..", ".."), mustWork = TRUE))
for (name in c("pharma_tolerance_application.R", "pharma_mti_application.R",
  "pharma_mti_reporting.R", "pharma_source_structure.R")) source(file.path("application/scripts/lib", name))
args <- commandArgs(TRUE)
config_path <- pta_arg_value(args, "--config=", psa_default_config)
config <- pta_read_config(config_path)
attr(config, "config_path") <- config_path
mode <- pta_arg_value(args, "--mode=", "verify")
if (mode == "prepare") {
  psa_run(config,
    pta_arg_value(args, "--laboratory-file=", file.path(config$cache_dir, "Laboratory.csv")),
    pta_arg_value(args, "--process-archive=", file.path(config$cache_dir, "Process.zip")),
    download = identical(pta_arg_value(args, "--download=", "false"), "true"))
} else if (mode == "tables") {
  psa_render(config$evidence_dir, pta_arg_value(args, "--output-dir=", "tables"))
} else if (mode == "verify") {
  psa_verify(config)
} else pta_stop("Supported modes are prepare, tables, and verify; no fitting mode exists.")
cat("Pharmaceutical source and campaign checks completed without fitting models.\n")
