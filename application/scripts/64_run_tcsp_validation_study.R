#!/usr/bin/env Rscript

arguments <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_path <- sub(file_arg, "", arguments[startsWith(arguments, file_arg)][1L])
if (!length(script_path) || is.na(script_path)) {
  script_path <- "application/scripts/64_run_tcsp_validation_study.R"
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

mode <- tolower(arg_value("--mode=", "preflight"))
allowed <- c("preflight", "tiny", "pilot", "health-check-read-only")
if (!mode %in% allowed) tcspv_stop("Unsupported TCSP validation mode: ", mode)

config_path <- normalizePath(arg_value(
  "--config=", file.path("application", "config", "tcsp_validation_v1.json")
), winslash = "/", mustWork = TRUE)
config <- tcspv_read_config(config_path)
tcspv_validate_config(config)

if (identical(mode, "tiny") && !isTRUE(config$execution$tiny_authorized)) {
  tcspv_stop("Tiny TCSP validation is not authorized.")
}
if (identical(mode, "pilot") && !isTRUE(config$execution$pilot_authorized)) {
  tcspv_stop("Pilot TCSP validation is not authorized.")
}
if (identical(mode, "preflight") &&
    !isTRUE(config$execution$preflight_authorized)) {
  tcspv_stop("TCSP preflight is not authorized.")
}

if (identical(mode, "health-check-read-only")) {
  run_dir <- normalizePath(arg_value("--run-dir=", ""),
                           winslash = "/", mustWork = TRUE)
  manifest <- tcspv_verify_run(run_dir)
  cat("TCSP validation health check passed:", run_dir, "\n")
  cat("Artifacts:", nrow(manifest), "\n")
  quit(save = "no", status = 0L)
}

default_output <- file.path(
  config$outputs$root,
  paste0(mode, "_", format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"))
)
output_dir <- normalizePath(arg_value("--output-dir=", default_output),
                            winslash = "/", mustWork = FALSE)
if (file.exists(output_dir) || dir.exists(output_dir)) {
  tcspv_stop("The output directory must be fresh: ", output_dir)
}
dir.create(dirname(output_dir), recursive = TRUE, showWarnings = FALSE)
staging <- tempfile(paste0(".", basename(output_dir), "-"),
                    tmpdir = dirname(output_dir))
dir.create(staging, recursive = TRUE, showWarnings = FALSE)
published <- FALSE
on.exit({
  if (!published) unlink(staging, recursive = TRUE, force = TRUE)
}, add = TRUE)

if (identical(mode, "preflight")) {
  tcspv_write_preflight(config, mode, staging, repo_root)
} else {
  tcspv_write_run(config, mode, staging, repo_root)
}
if (!file.rename(staging, output_dir)) {
  tcspv_stop("Could not publish TCSP validation output.")
}
published <- TRUE
cat("TCSP validation", mode, "completed:", output_dir, "\n")
