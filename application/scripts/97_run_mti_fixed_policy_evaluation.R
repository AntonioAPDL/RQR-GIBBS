#!/usr/bin/env Rscript
arguments <- commandArgs(FALSE)
script <- sub("--file=","",arguments[startsWith(arguments,"--file=")][1],fixed=TRUE)
setwd(normalizePath(file.path(dirname(script),"..",".."),mustWork=TRUE))
for(file in c("pharma_tolerance_application.R","pharma_mti_application.R",
    "mti_profile_revision.R","mti_profile_review.R","mti_fixed_policy_evaluation.R"))
  source(file.path("application/scripts/lib",file))
args <- commandArgs(TRUE)
spec_path <- pta_arg_value(args,"--config=",
  "application/config/mti_profile_fixed_policy_evaluation_20261021.json")
spec <- jsonlite::read_json(spec_path,simplifyVector=FALSE)
cfg <- jsonlite::read_json(spec$revision_config,simplifyVector=FALSE)
mode <- pta_arg_value(args,"--mode=","preflight")
output <- pta_arg_value(args,"--run-dir=",file.path(spec$output_root,"independent_20261021"))
frozen <- jsonlite::read_json(cfg$validation_config,simplifyVector=FALSE)
design <- mpr_design(frozen)
if(mode=="health") {
  print(mpf_health(output,design,cfg),row.names=FALSE)
  if(file.exists(file.path(output,"launcher_exit.json")))
    print(mpr_read_json(file.path(output,"launcher_exit.json")))
  quit(status=0)
}
options(warn=2)
upstream <- pta_arg_value(args,"--calibration-run=","")
review <- pta_arg_value(args,"--calibration-review=","")
mpr_require(nzchar(upstream) && nzchar(review),"Explicit calibration run and reviewed assessment are required.")
calibration_repo <- normalizePath(pta_arg_value(args,"--calibration-repo=",
  file.path(upstream,"..","..","..","..")),mustWork=TRUE)
verified <- mpf_verify_upstream(spec,cfg,normalizePath(upstream),normalizePath(review),calibration_repo)
mpf_check_seeds(design,cfg)
if(mode=="freeze") {
  mpf_freeze(spec,verified)
  cat("Frozen calibration-only policy; original calibration remains unresolved.\n")
  quit(status=0)
}
mpf_check_policy(spec,verified)
pta_require_packages(c("rqrgibbs","tolerance","jsonlite","digest"))
runtime <- mpf_check_runtime(upstream)
workers <- as.integer(pta_arg_value(args,"--workers=","1"))
mpr_require(is.finite(workers) && workers>=1 && workers<=spec$execution$maximum_workers &&
  spec$execution$threads_per_worker==1,"Invalid worker controls.")
output <- mpf_check_output(output,spec,upstream,review)
if(mode=="preflight") {
  cat("Verified numerical runtime, policy, original calibration stop, unused independent seeds, and output isolation.\n")
  print(verified$selection$policy,row.names=FALSE)
  cat(nrow(design)*cfg$validation$replications,"fresh datasets;",
    nrow(design)*cfg$validation$replications*length(unlist(cfg$validation$methods)),"method evaluations.\n")
  quit(status=0)
}
mpr_require(mode %in% c("run","report"),"Unknown evaluation mode.")
mpr_require(mpr_git_clean(pma_git("status","--porcelain")),"Commit reviewed source before evaluation.")
identity <- mpf_identity(spec_path,spec,cfg,runtime,upstream,review,workers)
path <- file.path(output,"identity.rds")
if(file.exists(path)) {
  mpr_require(identical(identity,readRDS(path)),"Evaluation identity changed; do not reuse checkpoints.")
} else {
  mpr_require(mode=="run","Missing evaluation identity.")
  dir.create(output,recursive=TRUE,showWarnings=FALSE)
  existing <- list.files(output,all.files=TRUE,no..=TRUE)
  mpr_require(all(existing %in% c("launcher.pid","launch.log","launch.lock","launcher_exit.json")),
    "Output contains an earlier experiment; choose a new directory.")
  mp_atomic_rds(identity,path)
  pta_atomic_write_json(c(identity,list(workers=workers,
    started_utc=format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC"))),file.path(output,"manifest.json"))
  mp_atomic_rds(identity$protected_inputs,file.path(output,"protected_inputs.rds"))
}
protected <- readRDS(file.path(output,"protected_inputs.rds"))
mpr_require(identical(protected,identity$protected_inputs),"Protected-input identity changed.")
guard <- function() {
  mpr_require(identical(identity$sources,pma_file_record(identity$sources$path)),
    "Calculation sources or policy changed during evaluation.")
  mpr_require(identical(protected,pma_file_record(protected$path)),"Historical article inputs changed.")
}
guard()
tryCatch({
  if(mode=="run" && !file.exists(file.path(output,"validation","completion.json"))) {
    directory <- file.path(output,"validation")
    dir.create(directory,recursive=TRUE,showWarnings=FALSE)
    result <- mp_simulation_stage(cfg,"validation",directory,output,workers,
      validation_policy_path=spec$policy_csv)
    result$original_calibration_passed <- FALSE
    result$interpretation <- spec$purpose
    pta_atomic_write_json(result,file.path(directory,"completion.json"))
  }
  mpr_require(file.exists(file.path(output,"validation","completion.json")),
    "Complete the independent fits before reporting.")
  result <- mpf_report(output,design,cfg,spec,identity)
  guard()
  cat("Independent evaluation and report completed; article and application unchanged.\n")
},error=function(e) {
  pta_atomic_write_json(list(error=conditionMessage(e),article_inputs_replaced=FALSE,
    completed_utc=format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC")),
    file.path(output,"evaluation_error.json"))
  guard()
  stop(conditionMessage(e),call.=FALSE)
})
