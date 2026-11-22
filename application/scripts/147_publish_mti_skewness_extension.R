#!/usr/bin/env Rscript
args <- commandArgs(TRUE)
mode <- if (length(args)) args[1] else "verify"
if (!mode %in% c("collect", "render", "verify")) stop("Use collect, render, or verify.", call. = FALSE)
configuration <- if (length(args) >= 2L) args[2] else "application/config/mti_skewness_publication_20261122.json"
cfg <- jsonlite::read_json(configuration, simplifyVector = TRUE)
if (mode == "collect") {
  pointer <- jsonlite::read_json(cfg$study_pointer, simplifyVector = TRUE)
  private <- file.path(pointer$checkout, "application/cache/skewness_library")
  .libPaths(c(normalizePath(private, mustWork = TRUE), .libPaths()))
}
for (name in c("mti_gibbs_practical", "mti_gibbs_full_report_review", "mti_gibbs_full_publication",
  "mti_gibbs_width_diagnosis", "pharma_tolerance_application", "pharma_mti_application",
  "mti_fixed_prior_publication", "mti_fixed_prior_reporting", "mti_skewness_distributions",
  "mti_skewness_extension", "mti_skewness_reporting", "mti_skewness_publication",
  "mti_skewness_publication_figures")) source(file.path("application/scripts/lib", paste0(name, ".R")))
data.table::setDTthreads(1L)
cfg <- msu_config(configuration)
if (mode == "collect") msu_collect(cfg, pointer$output_root, pointer$checkout) else
  if (mode == "render") msu_render(cfg) else {
    msu_verify(cfg, rendered = TRUE)
    cat("Verified the expanded primary comparison; original Gibbs and application evidence is unchanged.\n")
  }
