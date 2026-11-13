#!/usr/bin/env Rscript
arguments <- commandArgs(FALSE)
script <- sub("--file=", "", arguments[startsWith(arguments, "--file=")][1], fixed = TRUE)
setwd(normalizePath(file.path(dirname(script), "..", ".."), mustWork = TRUE))
data.table::setDTthreads(1L)
source("application/scripts/lib/mti_gibbs_full_report_review.R")
source("application/scripts/lib/mti_gibbs_full_publication.R")
source("application/scripts/lib/mti_gibbs_full_publication_figures.R")
args <- commandArgs(TRUE)
mgfa_require(length(args) <= 1L && all(args %in% paste0("--mode=", c("verify", "verify-prior", "analyze", "render"))),
  "Use --mode=verify, verify-prior, analyze, or render.")
mode <- if (length(args)) substring(args, 8L) else "verify"
cfg <- mgfa_json("application/config/mti_gibbs_full_reporting_20261113.json")
if (mode == "verify-prior") {
  mgfp_prior_verify(cfg)
  cat("Verified the historical 456-dataset assessment, including ordered file hashes.\n")
} else {
  if (mode == "analyze") {
    tracked <- strsplit(mgfp_git("ls-files", "--", mgfp_programs), "\n", fixed = TRUE)[[1]]
    mgfa_require(setequal(tracked, mgfp_programs) &&
      system2("git", c("diff", "--quiet", "HEAD", "--", mgfp_programs)) == 0L,
      "Commit the reporting programs and configuration before collecting publication evidence.")
    mgfp_collect(cfg)
  }
  if (mode %in% c("analyze", "render")) mgfp_render(cfg)
  mgfp_verify(cfg)
  cat("Verified the completed full-study numerical evidence, tables, and figures.\n")
}
