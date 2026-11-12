#!/usr/bin/env Rscript
arguments <- commandArgs(FALSE)
script <- sub("--file=", "", arguments[startsWith(arguments, "--file=")][1], fixed = TRUE)
setwd(normalizePath(file.path(dirname(script), "..", ".."), mustWork = TRUE))
RNGkind("Mersenne-Twister", "Inversion", "Rejection")
for (file in c("pharma_tolerance_application.R", "pharma_mti_application.R",
    "mti_profile_revision.R", "mti_profile_review.R", "mti_final_reporting.R",
    "mti_ecm_recovery.R", "pinned_exdqlm_runtime.R", "mti_gibbs_comparison.R",
    "mti_gibbs_practical.R", "mti_gibbs_review.R", "mti_gibbs_full_comparison.R"))
  source(file.path("application/scripts/lib", file))
args <- commandArgs(TRUE)
cfg <- mgc_json(pta_arg_value(args, "--config=", "application/config/mti_gibbs_full_comparison_20261111.json"))
base <- mgc_json(cfg$base_config); practical <- mgc_json(cfg$practical_config)
mode <- pta_arg_value(args, "--mode=", "preflight")
directory <- pta_arg_value(args, "--output-dir=", "")
if (mode == "health") { print(mgf_health(directory)); quit(status = 0) }
if (mode == "stop") {
  mgf_output_guard(directory)
  mpr_require(dir.exists(directory), "Unknown campaign directory.")
  mgf_json_write(list(requested_utc = mgc_time()), file.path(directory, "STOP_REQUESTED"))
  cat("Stop requested; the current batch may finish. Resume requires removing STOP_REQUESTED.\n")
  quit(status = 0)
}
workers <- mgf_workers(cfg, base, as.integer(pta_arg_value(args, "--workers=", "50")))
library <- file.path(mgf_root(), ".codex_work", paste0("mti_gibbs_launch_", practical$parent_commit),
  "application/cache", paste0("mti_gibbs_runtime_", practical$parent_commit), "library")
.libPaths(c(normalizePath(library, mustWork = TRUE), .libPaths()))
if (mode == "preflight") {
  jobs <- mgc_design(base, "full"); mgf_settings(cfg, base, practical, jobs)
  print(list(datasets = length(unique(jobs$dataset_id)), chains = nrow(jobs), workers = workers,
    storage = mgf_space(cfg, nrow(jobs), if (nzchar(directory)) directory else "application/outputs"),
    sampler = cfg$sampler, reference_runtime = mgc_runtime(practical$parent_commit)$sha256))
  quit(status = 0)
}
stopifnot(mode %in% c("campaign", "resume", "report"), nzchar(directory))
if (mode == "report") { print(mgf_report(cfg, base, directory)); quit(status = 0) }
mgc_authorized(pta_arg_value(args, "--confirm-article-commit=", ""), pma_git("rev-parse", "HEAD"))
stopifnot(identical(pta_arg_value(args, "--allow-full=", "false"), "true"))
tryCatch(mgf_campaign(cfg, base, practical, directory, workers, resume = mode == "resume"),
  error = function(e) {
    state <- if (grepl("Graceful stop requested", conditionMessage(e), fixed = TRUE))
      "stopped_on_request" else "stopped_with_error"
    mgf_status(directory, state, error = conditionMessage(e), commit = pma_git("rev-parse", "HEAD"))
    stop(e)
  })
