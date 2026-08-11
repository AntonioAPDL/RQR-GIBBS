#!/usr/bin/env Rscript

arguments <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_path <- sub(file_arg, "", arguments[startsWith(arguments, file_arg)][1L])
if (!length(script_path) || is.na(script_path)) {
  script_path <- "application/scripts/65_audit_tcsp_validation_pilot.R"
}
script_path <- normalizePath(script_path, winslash = "/", mustWork = TRUE)
script_dir <- dirname(script_path)
repo_root <- normalizePath(file.path(script_dir, "..", ".."),
                           winslash = "/", mustWork = TRUE)
setwd(repo_root)
source(file.path(script_dir, "lib", "tcsp_validation_study.R"))

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(prefix, default = NULL) {
  hit <- args[startsWith(args, prefix)]
  if (length(hit)) sub(prefix, "", hit[[1L]], fixed = TRUE) else default
}

parse_flag <- function(value) {
  tolower(as.character(value %||% "false")) %in% c("true", "1", "yes", "y")
}

run_dir <- normalizePath(arg_value(
  "--run-dir=",
  file.path("application", "outputs", "tcsp_validation_v1",
            "pilot_codex_20260811")
), winslash = "/", mustWork = TRUE)
output_dir <- normalizePath(arg_value(
  "--output-dir=",
  file.path("docs", "audits", "tcsp_validation_pilot_20260811")
), winslash = "/", mustWork = FALSE)
replace <- parse_flag(arg_value("--replace=", "false"))

tcspv_write_pilot_audit(run_dir, output_dir, replace = replace)
cat("TCSP validation pilot audit published:", output_dir, "\n")
