#!/usr/bin/env Rscript
arguments <- commandArgs(FALSE)
script <- sub("--file=","",arguments[startsWith(arguments,"--file=")][1],fixed=TRUE)
setwd(normalizePath(file.path(dirname(script),"..",".."),mustWork=TRUE))
Sys.setenv(OMP_NUM_THREADS=1,OPENBLAS_NUM_THREADS=1,MKL_NUM_THREADS=1)
RNGkind("Mersenne-Twister", "Inversion", "Rejection")
for(file in c("pharma_tolerance_application.R","pharma_mti_application.R",
  "mti_profile_revision.R","mti_profile_review.R","mti_final_reporting.R",
  "mti_ecm_recovery.R","pinned_exdqlm_runtime.R","mti_gibbs_comparison.R"))
  source(file.path("application/scripts/lib",file))
args <- commandArgs(TRUE)
cfg <- jsonlite::read_json(pta_arg_value(args,"--config=",mgc_default),simplifyVector=TRUE)
mode <- pta_arg_value(args,"--mode=","preflight")
stage <- pta_arg_value(args,"--stage=","diagnostic")
if(mode=="runtime") {
  print(mgc_runtime())
} else if(mode=="health") {
  print(mgc_health(pta_arg_value(args,"--output-dir=",file.path(cfg$output_root,stage))))
} else if(mode=="assess") {
  output <- pta_arg_value(args,"--output-dir=",file.path(cfg$output_root,stage))
  lock <- mgc_lock(output)
  tryCatch(mgc_assess(cfg,output,workers=cfg$assessment_workers),
    finally=unlink(lock,recursive=TRUE))
} else if(mode=="report") {
  mgc_report(pta_arg_value(args,"--output-dir=",file.path(cfg$output_root,stage)))
} else if(mode %in% c("smoke","campaign")) {
  mgc_authorized(pta_arg_value(args,"--confirm-article-commit=",""),pma_git("rev-parse","HEAD"))
  output <- pta_arg_value(args,"--output-dir=",file.path(cfg$output_root,"campaign"))
  if(mode=="smoke")mgc_smoke(cfg,output) else {
    status <- tryCatch(mgc_campaign(cfg,output,
      identical(pta_arg_value(args,"--allow-full=","false"),"true"),
      as.integer(pta_arg_value(args,"--workers=","50"))),error=function(e) {
        pta_atomic_write_json(list(status="stopped_with_error",error=conditionMessage(e),
          time_utc=mgc_time()),file.path(output,"campaign_status.json"))
        message(conditionMessage(e)); 1L
      })
    quit(status=status)
  }
} else {
  jobs <- mgc_design(cfg,stage)
  cat(sprintf("Prepared %d datasets and %d chains for stage %s.\n",
    length(unique(jobs$dataset_id)),nrow(jobs),stage))
  if(mode=="run") {
    mgc_authorized(pta_arg_value(args,"--confirm-article-commit=",""),pma_git("rev-parse","HEAD"))
    mgc_run(cfg,jobs,stage,pta_arg_value(args,"--output-dir=",file.path(cfg$output_root,stage)),
      as.integer(pta_arg_value(args,"--workers=","32")),
      identical(pta_arg_value(args,"--resume=","false"),"true"),
      diagnostic_dir=pta_arg_value(args,"--diagnostic-dir=",file.path(cfg$output_root,"diagnostic")))
  } else if(mode!="preflight")stop("Unknown comparison mode.")
}
