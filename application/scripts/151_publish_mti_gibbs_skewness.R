#!/usr/bin/env Rscript
args <- commandArgs(TRUE)
mode <- if (length(args)) args[1] else "verify"
if (!mode %in% c("collect", "render", "verify")) stop("Use collect, render, or verify.", call. = FALSE)
for (name in c("mti_gibbs_practical", "mti_gibbs_full_report_review", "mti_gibbs_full_publication", "mti_gibbs_width_diagnosis",
  "pharma_tolerance_application", "pharma_mti_application", "mti_fixed_prior_publication",
  "mti_fixed_prior_reporting", "mti_skewness_distributions", "mti_skewness_reporting",
  "mti_skewness_publication", "mti_gibbs_skewness", "mti_gibbs_skewness_reporting",
  "mti_gibbs_skewness_publication")) source(file.path("application/scripts/lib", paste0(name, ".R")))
data.table::setDTthreads(1L)
cfg <- mgsp_config(if (length(args) >= 2L) args[2] else "application/config/mti_gibbs_skewness_publication_20261122.json")
if (mode == "collect") {
  pointer <- mwd_json(cfg$study_pointer)
  mgsp_collect(cfg, pointer$output_root)
} else if (mode == "render") mgsp_render(cfg) else {
  mgsp_verify(cfg, rendered = TRUE)
  cat("Verified 108 fixed-target Gibbs cases; primary, calibrated Gibbs and application comparisons are unchanged.\n")
}
