#!/usr/bin/env Rscript
arguments <- commandArgs(FALSE)
script <- sub("--file=","",arguments[startsWith(arguments,"--file=")][1],fixed=TRUE)
setwd(normalizePath(file.path(dirname(script),"..",".."),mustWork=TRUE))
for(file in c("pharma_tolerance_application.R","pharma_mti_application.R",
              "mti_profile_revision.R")) source(file.path("application/scripts/lib",file))
args <- commandArgs(TRUE)
config_path <- pta_arg_value(args,"--config=","application/config/mti_profile_revision_20261021.json")
cfg <- jsonlite::read_json(config_path,simplifyVector=FALSE)
stage <- pta_arg_value(args,"--stage=","preflight")
output <- pta_arg_value(args,"--run-dir=",file.path(cfg$output_root,"revision_20261021"))
workers <- as.integer(pta_arg_value(args,"--workers=",as.character(cfg$execution$workers)))
if(!is.finite(workers) || workers<1 || cfg$execution$threads_per_worker!=1) stop("Invalid worker controls.")
if(isTRUE(cfg$publication$replace_article_results) || isTRUE(cfg$publication$allow_historical_policy_transfer))
  stop("This revision study cannot replace article results or transfer a calibration policy.")
if(stage=="health") {
  paths <- list.files(output,"progress.json|completion.json",recursive=TRUE,full.names=TRUE)
  for(path in paths) { cat(path,"\n"); print(jsonlite::read_json(path,simplifyVector=TRUE)) }
  quit(status=0)
}
pta_require_packages(c("rqrgibbs","jsonlite","digest"))
if(!"rqr_mti_intercept_fit" %in% getNamespaceExports("rqrgibbs")) stop("Install the new profile runtime first.")
inputs <- mp_inputs(cfg)
if(stage=="preflight") {
  cat("Verified historical evidence, new API, configuration, and preserved inputs.\n")
  print(table(inputs$cases$response_id,inputs$cases$split_scheme))
  quit(status=0)
}
if(!stage %in% c("mechanism","saved","calibration","validation","pipeline","collect")) stop("Unknown stage.")
Sys.setenv(OMP_NUM_THREADS="1",OPENBLAS_NUM_THREADS="1",MKL_NUM_THREADS="1")
dir.create(output,recursive=TRUE,showWarnings=FALSE)
source_paths <- c(config_path,"application/scripts/92_run_mti_profile_revision.R",
  "application/scripts/lib/pharma_tolerance_application.R",
  "application/scripts/lib/pharma_mti_application.R",
  "application/scripts/lib/mti_profile_revision.R","application/R/rqr_mti_intercept_profile.R",
  "application/R/rqr_mti_profile_selection.R","application/src/rqr_mti_intercept_profile.cpp",
  "application/scripts/69_validate_rqr_bayes_uq.R",cfg$historical_policy,cfg$validation_config)
identity <- list(commit=pma_git("rev-parse","HEAD"),sources=pma_file_record(source_paths),
  config_sha256=mp_sha(config_path),package_version=as.character(packageVersion("rqrgibbs")),
  shared_library_sha256=mp_sha(file.path(find.package("rqrgibbs"),"libs","rqrgibbs.so")),
  package_R_hashes=pma_file_record(list.files(file.path(find.package("rqrgibbs"),"R"),
    full.names=TRUE)),benchmark_version=as.character(packageVersion("tolerance")),
  R_version=R.version.string)
identity_path <- file.path(output,"identity.rds")
if(file.exists(identity_path)) {
  if(!identical(identity,readRDS(identity_path))) stop("Run identity changed; use a fresh output directory.")
} else {
  mp_atomic_rds(identity,identity_path)
  pta_atomic_write_json(c(identity,list(git_status=pma_git("status","--porcelain"),
    configured_exdqlm_commit=getFromNamespace(".rqr_pinned_exdqlm_commit","rqrgibbs")(),
    exdqlm_loaded="exdqlm" %in% loadedNamespaces(),workers=workers,
    started_utc=format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC"))),file.path(output,"manifest.json"))
}
protected <- pma_file_record(c(list.files(cfg$application_evidence,full.names=TRUE),
  cfg$historical_policy,cfg$validation_config,"tables/tolerance_validation_article_scan_calibration.csv"))
run_stage <- function(name,operation) {
  path <- file.path(output,name)
  dir.create(path,showWarnings=FALSE)
  completion <- file.path(path,"completion.json")
  if(file.exists(completion)) {
    result <- jsonlite::read_json(completion,simplifyVector=TRUE)
    if(!isTRUE(result$passed)) stop("Previously unresolved stage: ",name,
      ". Review its diagnostics; do not reuse it as a passed stage.")
    return(result)
  }
  started <- proc.time()[["elapsed"]]
  result <- operation(path)
  result$elapsed_sec <- proc.time()[["elapsed"]]-started
  result$completed_utc <- format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC")
  pta_atomic_write_json(result,completion)
  if(!isTRUE(result$passed)) stop("Stage did not pass: ",name)
  print(result)
  result
}
require_stage <- function(name) {
  path <- file.path(output,name,"completion.json")
  if(!file.exists(path) || !isTRUE(jsonlite::read_json(path)$passed))
    stop("A passed ",name," stage is required first.")
}
if(stage %in% c("mechanism","pipeline")) {
  run_stage("mechanism",function(path) {
    x <- mp_mechanism(cfg,path,workers)
    mp_report(path)
    mp_report(path,20L)
    x
  })
}
if(stage %in% c("saved","pipeline")) {
  require_stage("mechanism")
  run_stage("saved",function(path) mp_saved_cases(cfg,path,workers))
}
if(stage %in% c("calibration","validation","pipeline","collect")) {
  if(!exists("mp_simulation_stage",mode="function")) stop("Statistical stage implementation is not available.")
  if(stage=="collect") mp_collect(cfg,output)
  else {
    require_stage("mechanism")
    require_stage("saved")
    if(stage %in% c("calibration","pipeline")) run_stage("calibration",
      function(path) mp_simulation_stage(cfg,"calibration",path,output,workers))
    if(stage %in% c("validation","pipeline")) {
      require_stage("calibration")
      run_stage("validation",function(path) mp_simulation_stage(cfg,"validation",path,output,workers))
    }
    mp_collect(cfg,output)
  }
}
if(!identical(protected,pma_file_record(protected$path))) stop("Historical scientific inputs changed.")
cat("Preserved historical results and calibration. Completed requested stage:",stage,"\n")
