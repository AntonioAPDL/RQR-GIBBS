#!/usr/bin/env Rscript
arguments <- commandArgs(FALSE)
script <- sub("--file=","",arguments[startsWith(arguments,"--file=")][1],fixed=TRUE)
setwd(normalizePath(file.path(dirname(script),"..",".."),mustWork=TRUE))
for(file in c("pharma_tolerance_application.R","pharma_mti_application.R",
  "mti_profile_revision.R","mti_profile_review.R","mti_final_reporting.R","mti_ecm_recovery.R"))
  source(file.path("application/scripts/lib",file))
args <- commandArgs(TRUE)
cfg <- mre_config(pta_arg_value(args,"--config=",mre_default))
mode <- pta_arg_value(args,"--mode=","tables")
if(mode=="recover") {
  mpr_require(mpr_git_clean(pma_git("status","--porcelain")),
    "Commit calculation source before recovering paired comparison results.")
  root <- pta_arg_value(args,"--archive-root=",getwd())
  mre_results(cfg,root,as.integer(pta_arg_value(args,"--workers=","32")))
} else if(mode=="tables") {
  mre_tables(cfg,pta_arg_value(args,"--output-dir=","tables"))
} else stop("Supported modes are recover and tables; this script does not launch Gibbs sampling.")
