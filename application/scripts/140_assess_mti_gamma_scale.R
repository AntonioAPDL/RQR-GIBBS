#!/usr/bin/env Rscript
Sys.setenv(OMP_NUM_THREADS=1,OPENBLAS_NUM_THREADS=1,MKL_NUM_THREADS=1)
private<-Sys.getenv("MTI_GAMMA_SCALE_R_LIBRARY","application/cache/gamma_scale_library")
.libPaths(c(normalizePath(private,mustWork=TRUE),.libPaths()))
libraries<-c("mti_gibbs_practical","mti_gibbs_full_report_review","mti_gibbs_full_publication",
  "mti_profile_revision","mti_gibbs_width_diagnosis","mti_prior_screen","pharma_tolerance_application",
  "mti_fixed_prior","mti_gibbs_boundary_calibration","mti_gibbs_boundary_study",
  "mti_gibbs_decision_calibration","mti_gibbs_decision_study","mti_fixed_prior_finalization",
  "mti_gibbs_joint","mti_gibbs_joint_precision","mti_gibbs_joint_study","mti_gibbs_joint_reuse",
  "mti_ecm_nominal","mti_ecm_common_scale_study","mti_gamma_scale_study")
for(name in libraries)source(file.path("application/scripts/lib",paste0(name,".R")))
data.table::setDTthreads(1L);options(datatable.optimize=0L)
RNGkind("Mersenne-Twister","Inversion","Rejection")
args<-commandArgs(TRUE);mode<-if(length(args))args[1] else "prepare"
configuration<-"application/config/mti_gamma_scale_20261119.json"
cfg<-mwd_json(configuration);mgsc_validate(cfg)
pointer<-file.path(mgfp_common(),".codex_work/mti_gamma_scale_latest.json")
cfg$output_root<-Sys.getenv("MTI_GAMMA_SCALE_OUTPUT_ROOT",file.path("/var/tmp/jaguir26-mti-studies",
  paste0("mti_gamma_scale_",mgfp_git("rev-parse","HEAD"))))
if(mode=="health") {
  if(file.exists(pointer))cfg$output_root<-mwd_json(pointer)$output_root
  for(name in c("startup_status.json","audit/assessment.json","rehearsal/progress.json",
    "screening/progress.json","screening/verification.json","study_summary.json","controller_exit.json")) {
    path<-file.path(cfg$output_root,name)
    if(file.exists(path)){cat(name,"\n");print(mwd_json(path))}
  }
  quit(save="no")
}
mwd_require(mode %in% c("prepare","check","pipeline","verify"),"Unknown Gamma study command.")
if(mode=="verify") {
  mwd_require(file.exists(pointer),"No launched Gamma study.")
  p<-mwd_json(pointer);cfg$output_root<-p$output_root
  mwd_require(p$source_commit==mgfp_git("rev-parse","HEAD"),"Verify from the pinned Gamma source checkout.")
}
dir.create(cfg$output_root,recursive=TRUE,showWarnings=FALSE)
context<-msc_context(cfg,cfg$output_root);previous<-mgsc_previous(cfg,context)
tasks<-previous$tasks
workers<-min(cfg$maximum_workers,parallelly::availableCores(),max(1,floor(parallel::detectCores()-
  as.numeric(strsplit(readLines("/proc/loadavg",n=1)," ")[[1]][1]))))
requested<-suppressWarnings(as.integer(Sys.getenv("MTI_GAMMA_SCALE_WORKERS","")))
if(is.finite(requested))workers<-min(workers,max(1L,requested))
programs<-unique(c(configuration,"application/scripts/140_assess_mti_gamma_scale.R",
  "application/scripts/141_launch_mti_gamma_scale.sh",file.path("application/scripts/lib",paste0(libraries,".R")),
  list.files("application/R",full.names=TRUE),list.files("application/src",pattern="[.](cpp|h)$",full.names=TRUE)))
identity<-list(source_commit=mgfp_git("rev-parse","HEAD"),configuration=cfg,programs=mgfp_hashes(programs),
  package=context$package,R=R.version.string,rng=RNGkind(),generators=context$generator_sha256,
  previous_source=previous$pointer,previous_manifest_sha256=mwd_file_hash(file.path(previous$pointer$output_root,"manifest.json")),
  tasks_sha256=mwd_hash(tasks),exdqlm_reference_commit=cfg$exdqlm_reference_commit,
  article_files=mgfp_hashes(c("main.tex","rqr-gibbs-supplement.tex")),priors_estimated_from_sample=FALSE)
if(mode=="prepare") {
  mps_disk_check(cfg$output_root,cfg$disk_reserve_gib)
  print(list(datasets=nrow(tasks),new_profile_fits=nrow(tasks)*length(cfg$prior_specs),
    maximum_workers=workers,priors=cfg$prior_specs,launch_ceiling=cfg$launch_ceiling,
    previous_study_finished=TRUE,prior_uses_analysis_data=FALSE));quit(save="no")
}
manifest<-file.path(cfg$output_root,"manifest.json")
if(mode=="verify") {
  mwd_require(mwd_json(manifest)$identity_hash==mwd_hash(identity),"Gamma study inputs changed.")
  records<-mgsc_verify(tasks,"screening",cfg,context,identity,previous)
  summary<-mwd_json(file.path(cfg$output_root,"study_summary.json"))
  mwd_require(summary$datasets==length(records) && summary$remaining_datasets==0 && summary$numerical_pass,
    "Gamma study is not complete.")
  mgfp_check_hashes(mwd_json(file.path(cfg$output_root,"audit/assessment.json"))$outputs)
  receipt<-mwd_json(file.path(cfg$output_root,"report_receipt.json"))
  mwd_require(receipt$source_commit==identity$source_commit,"Report source differs.")
  mgfp_check_hashes(receipt$outputs)
  cat("Gamma study evidence verified.\n");quit(save="no")
}
rehearsal<-tasks[tasks$replication==1 & ((tasks$n==50 & tasks$content==.9) |
  (tasks$n==100 & tasks$content==.95) | (tasks$n==500 & tasks$content==.99)),,drop=FALSE]
mwd_require(nrow(rehearsal)==24,"Integrated rehearsal must cover eight laws and three designs.")
if(mode=="check") {
  queue<-mjs_queue(rehearsal,"rehearsal",cfg,identity,workers,function(task)mgsc_job(task,cfg,context,previous))
  mgsc_verify(rehearsal,"rehearsal",cfg,context,identity,previous)
  cat("Gamma integrated rehearsal passed.\n");quit(save="no")
}
mwd_require(!nzchar(mgfp_git("status","--porcelain")),"Commit source before a Gamma study launch.")
if(file.exists(manifest))mwd_require(mwd_json(manifest)$identity_hash==mwd_hash(identity),"Study inputs changed.") else
  mgfp_json_write(list(identity=identity,identity_hash=mwd_hash(identity),started=mgfp_time()),manifest)
mgfp_json_write(list(output_root=normalizePath(cfg$output_root),source_commit=identity$source_commit,
  checkout=normalizePath(".")),pointer)
stage<-"audit"
tryCatch({
  mgfp_json_write(list(stage=stage,time=mgfp_time()),file.path(cfg$output_root,"startup_status.json"))
  audit<-mgsc_audit(cfg,context,previous)
  stage<-"rehearsal"
  mgfp_json_write(list(stage=stage,time=mgfp_time()),file.path(cfg$output_root,"startup_status.json"))
  queue<-mjs_queue(rehearsal,stage,cfg,identity,workers,function(task)mgsc_job(task,cfg,context,previous))
  mgsc_verify(rehearsal,stage,cfg,context,identity,previous)
  stage<-"screening"
  mgfp_json_write(list(stage=stage,time=mgfp_time()),file.path(cfg$output_root,"startup_status.json"))
  queue<-mjs_queue(tasks,stage,cfg,identity,workers,function(task) {
    path<-file.path(cfg$output_root,"rehearsal/fits",sprintf("fit_%06d.rds",task$job_id))
    if(file.exists(paste0(path,".json"))) {
      value<-mbc_read_fit(path,mwd_hash(list(identity,"rehearsal",rehearsal)),task)
      value$reused_rehearsal<-TRUE;return(value)
    }
    mgsc_job(task,cfg,context,previous)
  })
  records<-mgsc_verify(tasks,stage,cfg,context,identity,previous)
  print(mgsc_report(records,cfg,context))
},error=function(e) {
  mgfp_json_write(list(status="stopped_requires_numerical_review",stage=stage,reason=conditionMessage(e),
    article_replaced=FALSE,no_gibbs_sampling=TRUE,time=mgfp_time()),file.path(cfg$output_root,"study_summary.json"))
  stop(e)
})
