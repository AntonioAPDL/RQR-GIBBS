#!/usr/bin/env Rscript
arguments <- commandArgs(FALSE)
script <- sub("--file=","",arguments[startsWith(arguments,"--file=")][1],fixed=TRUE)
setwd(normalizePath(file.path(dirname(script),"..",".."),mustWork=TRUE))
Sys.setenv(OMP_NUM_THREADS=1,OPENBLAS_NUM_THREADS=1,MKL_NUM_THREADS=1)
for(file in c("pharma_tolerance_application.R","pharma_mti_application.R",
  "mti_profile_revision.R","mti_profile_review.R","mti_final_reporting.R",
  "mti_ecm_recovery.R","pinned_exdqlm_runtime.R","mti_gibbs_comparison.R"))
  source(file.path("application/scripts/lib",file))
args <- commandArgs(TRUE)
cfg <- jsonlite::read_json(pta_arg_value(args,"--config=",mgc_default),simplifyVector=TRUE)
mode <- pta_arg_value(args,"--mode=","preflight")
stage <- pta_arg_value(args,"--stage=","diagnostic")
if(mode=="health") {
  print(mgc_health(pta_arg_value(args,"--output-dir=",file.path(cfg$output_root,stage))))
} else if(mode=="assess") {
  mgc_assess(cfg,pta_arg_value(args,"--output-dir=",file.path(cfg$output_root,stage)))
} else {
  jobs <- mgc_design(cfg,stage)
  cat(sprintf("Prepared %d datasets and %d chains for stage %s. No sampling during preflight.\n",
    length(unique(jobs$dataset_id)),nrow(jobs),stage))
  if(mode=="run") {
    mgc_authorized(pta_arg_value(args,"--confirm-article-commit=",""),pma_git("rev-parse","HEAD"))
    mgc_run(cfg,jobs,stage,pta_arg_value(args,"--output-dir=",file.path(cfg$output_root,stage)),
      as.integer(pta_arg_value(args,"--workers=","32")),
      identical(pta_arg_value(args,"--resume=","false"),"true"))
  } else if(mode!="preflight")stop("Unknown comparison mode.")
}
