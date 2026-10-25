#!/usr/bin/env Rscript
arguments <- commandArgs(FALSE)
script <- sub("--file=","",arguments[startsWith(arguments,"--file=")][1],fixed=TRUE)
setwd(normalizePath(file.path(dirname(script),"..",".."),mustWork=TRUE))
for(file in c("pharma_tolerance_application.R","pharma_mti_application.R",
  "mti_profile_revision.R","mti_profile_review.R","mti_fixed_policy_evaluation.R",
  "mti_q_grid_study.R","mti_final_reporting.R","mti_profile_adoption.R"))
  source(file.path("application/scripts/lib",file))
args <- commandArgs(TRUE)
mode <- pta_arg_value(args,"--mode=","verify")
if(mode=="import") {
  mpa_import(pta_arg_value(args,"--study-run=",""))
} else if(mode=="tables") {
  mpa_export_tables(pta_arg_value(args,"--output-dir=","tables"))
} else if(mode=="application") {
  cfg <- pma_read_config(pta_arg_value(args,"--config=",mpa_application_default))
  mpr_require(mpr_git_clean(pma_git("status","--porcelain")),"Commit calculation source before application evaluation.")
  adopted <- mpa_read(cfg$mti_profile$adoption_config)
  mpa_check_runtime(adopted)
  context <- mpa_application_context(cfg)
  output <- pta_arg_value(args,"--output-dir=",file.path(cfg$paths$output_root,"confirmatory"))
  workers <- as.integer(pta_arg_value(args,"--workers=","32"))
  mpr_require(workers>=1L && workers<=40L,"Application worker count must be between 1 and 40.")
  print(pma_run(context,"confirmatory",output,workers))
} else if(mode=="verify") {
  print(mpa_read()$policy)
} else stop("Unknown adoption mode.")
