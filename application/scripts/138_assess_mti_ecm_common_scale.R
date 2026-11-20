#!/usr/bin/env Rscript
Sys.setenv(OMP_NUM_THREADS=1,OPENBLAS_NUM_THREADS=1,MKL_NUM_THREADS=1)
private<-Sys.getenv("MTI_COMMON_SCALE_R_LIBRARY", "application/cache/common_scale_library")
.libPaths(c(normalizePath(private,mustWork=TRUE),.libPaths()))
libraries<-c("mti_gibbs_practical","mti_gibbs_full_report_review","mti_gibbs_full_publication",
  "mti_profile_revision","mti_gibbs_width_diagnosis","mti_prior_screen","pharma_tolerance_application",
  "mti_fixed_prior","mti_gibbs_boundary_calibration","mti_gibbs_boundary_study",
  "mti_gibbs_decision_calibration","mti_gibbs_decision_study","mti_fixed_prior_finalization",
  "mti_gibbs_joint","mti_gibbs_joint_precision","mti_gibbs_joint_study","mti_gibbs_joint_reuse",
  "mti_ecm_nominal","mti_ecm_common_scale_study")
for(name in libraries)source(file.path("application/scripts/lib",paste0(name,".R")))
data.table::setDTthreads(1L);options(datatable.optimize=0L)
RNGkind("Mersenne-Twister","Inversion","Rejection")
args<-commandArgs(TRUE);mode<-if(length(args))args[1] else "prepare"
configuration<-"application/config/mti_ecm_common_scale_20261119.json"
cfg<-mwd_json(configuration);msc_validate(cfg)
pointer<-file.path(mgfp_common(),".codex_work/mti_ecm_common_scale_latest.json")
cfg$output_root<-Sys.getenv("MTI_COMMON_SCALE_OUTPUT_ROOT",file.path("/var/tmp/jaguir26-mti-studies",
  paste0("mti_ecm_common_scale_",mgfp_git("rev-parse","HEAD"))))
if(mode=="health") {
  if(file.exists(pointer))cfg$output_root<-mwd_json(pointer)$output_root
  for(name in c("startup_status.json","screening/progress.json","study_summary.json","verification.json","controller_exit.json")) {
    p<-file.path(cfg$output_root,name);if(file.exists(p)){cat(name,"\n");print(mwd_json(p))}
  }
  quit(save="no")
}
mwd_require(mode %in% c("prepare","check","screen","verify"),"Unknown common-scale assessment command.")
dir.create(cfg$output_root,recursive=TRUE,showWarnings=FALSE)
context<-msc_context(cfg,cfg$output_root)
tasks<-context$development[context$development$replication<=cfg$screening_replications,,drop=FALSE]
tasks$source_job_id<-tasks$job_id;tasks$job_id<-seq_len(nrow(tasks))
mwd_require(nrow(tasks)==cfg$expected_cells*cfg$screening_replications,"Screening design differs.")
programs<-unique(c(configuration,"application/scripts/138_assess_mti_ecm_common_scale.R",
  "application/scripts/139_launch_mti_ecm_common_scale.sh",file.path("application/scripts/lib",paste0(libraries,".R")),
  list.files("application/R",full.names=TRUE),list.files("application/src",pattern="[.](cpp|h)$",full.names=TRUE)))
identity<-list(source_commit=mgfp_git("rev-parse","HEAD"),configuration=cfg,programs=mgfp_hashes(programs),
  package=context$package,common_scale_source=mwd_file_hash("application/R/rqr_ecm_common_scale.R"),
  generators=context$generator_sha256,tasks_sha256=mwd_hash(tasks),
  exdqlm_reference_commit=cfg$exdqlm_reference_commit,article_files=mgfp_hashes(c("main.tex","rqr-gibbs-supplement.tex")),
  reused_development_only=TRUE,R=R.version.string)
workers<-min(cfg$maximum_workers,parallelly::availableCores(),max(1,floor(parallel::detectCores()-as.numeric(strsplit(readLines("/proc/loadavg",n=1)," ")[[1]][1]))))
requested<-suppressWarnings(as.integer(Sys.getenv("MTI_COMMON_SCALE_WORKERS","")))
if(is.finite(requested))workers<-min(workers,max(1L,requested))
if(mode=="prepare") {
  mps_disk_check(cfg$output_root,cfg$disk_reserve_gib)
  print(list(datasets=nrow(tasks),maximum_workers=workers,scale_ranges=cfg$scale_ranges,priors_use_sample=FALSE,
    fresh_validation_launched=FALSE,Gibbs=FALSE));quit(save="no")
}
if(mode=="check") {
  chosen<-tasks[tasks$replication==1 & ((tasks$n==100 & tasks$content==.9 & tasks$response_id=="exponential") |
    (tasks$n==500 & tasks$content==.99 & tasks$response_id=="normal")),,drop=FALSE]
  values<-lapply(seq_len(nrow(chosen)),function(i)msc_job(chosen[i,,drop=FALSE],cfg,context))
  mwd_require(length(values)==2 && all(vapply(values,function(z)z$ok,FALSE)),"Real-sample rehearsal requires review.")
  saveRDS(values,file.path(cfg$output_root,"rehearsal.rds"))
  mgfp_json_write(list(passed=TRUE,datasets=2,used_development_only=TRUE,no_gibbs_sampling=TRUE,
    rows=as.data.frame(data.table::rbindlist(lapply(values,`[[`,"rows")))),file.path(cfg$output_root,"rehearsal.json"))
  print(lapply(values,function(z)z$rows));quit(save="no")
}
manifest<-file.path(cfg$output_root,"manifest.json")
if(file.exists(manifest))mwd_require(mwd_json(manifest)$identity_hash==mwd_hash(identity),"Study inputs changed.") else {
  mwd_require(mode=="screen" && !nzchar(mgfp_git("status","--porcelain")),"Commit source before the screening study.")
  mgfp_json_write(list(identity=identity,identity_hash=mwd_hash(identity),started=mgfp_time()),manifest)
}
if(mode=="verify"){msc_verify(cfg,context,tasks,identity);quit(save="no")}
mgfp_json_write(list(output_root=normalizePath(cfg$output_root),source_commit=identity$source_commit,
  checkout=normalizePath(".")),pointer)
tryCatch({
  queue<-mjs_queue(tasks,"screening",cfg,identity,workers,function(task)msc_job(task,cfg,context))
  values<-msc_verify(cfg,context,tasks,identity)
  print(msc_report(values,cfg,context))
},error=function(e){mgfp_json_write(list(status="stopped_requires_numerical_review",reason=conditionMessage(e),
  article_replaced=FALSE,no_gibbs_sampling=TRUE,time=mgfp_time()),file.path(cfg$output_root,"study_summary.json"));stop(e)})
