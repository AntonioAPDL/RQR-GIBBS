#!/usr/bin/env Rscript
for (name in c("mti_gibbs_practical", "mti_gibbs_full_report_review", "mti_gibbs_full_publication",
  "mti_gibbs_full_publication_figures", "mti_gibbs_width_diagnosis", "pharma_tolerance_application",
  "pharma_mti_application", "mti_fixed_prior_publication", "mti_fixed_prior_reporting", "mti_fixed_prior_reporting_figures"))
  source(file.path("application/scripts/lib", paste0(name, ".R")))
data.table::setDTthreads(1L)
args <- commandArgs(TRUE); mode <- if (length(args)) args[1] else "verify"
cfg <- mwd_json("application/config/mti_fixed_prior_reporting_20261115.json")
if (length(args) >= 2L) {
  cfg$finalization_commit <- args[2]
  cfg$finalization_worktree <- paste0(".codex_work/mti_fixed_prior_finalization_", args[2])
}
if (mode == "collect") mfr_collect(cfg) else if (mode == "render") mfr_render(cfg) else
  if (mode == "verify") mfr_verify(cfg, rendered = TRUE) else stop("Use collect, render, or verify.")
