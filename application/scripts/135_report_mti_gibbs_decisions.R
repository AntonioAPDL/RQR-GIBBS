#!/usr/bin/env Rscript
for (name in c("mti_gibbs_full_report_review", "mti_gibbs_full_publication", "mti_gibbs_width_diagnosis",
  "mti_gibbs_decision_study", "mti_gibbs_decision_reporting"))
  source(file.path("application/scripts/lib", paste0(name, ".R")))
data.table::setDTthreads(1L)
cfg <- mwd_json("application/config/mti_gibbs_decision_reporting_20261118.json")
args <- commandArgs(TRUE)
mode <- if (length(args)) args[1] else "verify"
if (mode == "prepare") {
  mwd_require(length(args) == 2, "Provide the completed decision-study output directory.")
  mdr_prepare(cfg, normalizePath(args[2], mustWork = TRUE))
} else if (mode == "render") mdr_render(cfg) else if (mode == "verify") mdr_verify(cfg) else
  stop("Use prepare DIRECTORY, render, or verify.")
