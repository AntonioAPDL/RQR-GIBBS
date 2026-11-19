#!/usr/bin/env Rscript
Sys.setenv(OMP_NUM_THREADS=1,OPENBLAS_NUM_THREADS=1,MKL_NUM_THREADS=1)
private <- Sys.getenv("MTI_PRIOR_R_LIBRARY")
if(nzchar(private)).libPaths(c(normalizePath(private,mustWork=TRUE),.libPaths()))
libraries <- c("mti_gibbs_practical","mti_gibbs_full_report_review","mti_gibbs_full_publication",
  "mti_profile_revision","mti_gibbs_width_diagnosis","mti_prior_screen","pharma_tolerance_application",
  "mti_fixed_prior","mti_gibbs_boundary_calibration","mti_gibbs_boundary_study",
  "mti_gibbs_decision_calibration","mti_gibbs_decision_study","mti_fixed_prior_finalization",
  "mti_gibbs_joint","mti_gibbs_joint_precision","mti_gibbs_joint_study","mti_gibbs_joint_reuse","mti_ecm_nominal")
for(name in libraries)source(file.path("application/scripts/lib",paste0(name,".R")))
data.table::setDTthreads(1L)
options(datatable.optimize=0L)
RNGkind("Mersenne-Twister","Inversion","Rejection")
args <- commandArgs(TRUE);mode <- if(length(args))args[1] else "prepare"
configuration <- "application/config/mti_ecm_nominal_20261119.json"
cfg <- mwd_json(configuration);mec_validate(cfg)
override <- Sys.getenv("MTI_ECM_NOMINAL_OUTPUT_ROOT")
if(nzchar(override))cfg$output_root <- override
pointer <- file.path(mgfp_common(),".codex_work/mti_ecm_nominal_latest.json")
if(mode=="health") {
  if(file.exists(pointer))cfg$output_root <- mwd_json(pointer)$output_root
  for(name in c("manifest.json","development/progress.json","validation/progress.json","study_summary.json","controller_exit.json")) {
    file <- file.path(cfg$output_root,name)
    if(file.exists(file)){cat(name,"\n");print(if(name=="manifest.json")mwd_json(file)$started else mwd_json(file))}
  }
  quit(save="no")
}
mwd_require(mode %in% c("prepare","check","pipeline","verify"),"Unknown ECM exploration command.")
dir.create(cfg$output_root,recursive=TRUE,showWarnings=FALSE)
dependency_file <- file.path(cfg$output_root,"dependency.json")
source <- if(file.exists(dependency_file))mwd_json(dependency_file) else
  mwd_json(file.path(mgfp_common(),".codex_work/mti_gibbs_joint_latest.json"))
source_manifest <- mwd_json(file.path(source$output_root,"manifest.json"))
mwd_require(source$source_commit==cfg$source_study_commit &&
  source_manifest$identity$source_commit==cfg$source_study_commit &&
  mjs_dependency_status(source$output_root)=="ready","The completed source study differs or is not complete.")
if(!is.null(source$manifest_sha256))mwd_require(source$manifest_sha256==mwd_file_hash(file.path(source$output_root,"manifest.json")),
  "The frozen source manifest changed.")
source$dependency <- source_manifest$identity$dependency
source$mode_identity <- mwd_json(file.path(source$output_root,"modes/manifest.json"))$analysis_id
joint_cfg <- source_manifest$identity$configuration;joint_cfg$output_root <- cfg$output_root
context <- mjs_context(joint_cfg,source$dependency)
mwd_require(context$dp$concentration==cfg$dp_concentration && context$dp$base$sd==cfg$dp_base_sd &&
  context$dp$base$mean==0 && !context$prior$simulation_reference$estimated_from_analysis_sample,
  "The fixed DP or prior coordinates differ.")
development <- mec_development_tasks(file.path(cfg$output_root,"source_development_tasks.rds"),
  source_manifest$identity$seed_audit$tasks_sha256$development,
  function()mjs_seed_audit(mjs_designs(joint_cfg,context),joint_cfg,context)$designs$development)
mwd_require(mwd_hash(development)==source_manifest$identity$seed_audit$tasks_sha256$development,
  "Canonical development tasks do not match saved source receipts.")
plan <- mec_design(cfg,context,development,source)
designs <- unique(development[,c("n","content","tolerance_confidence")]);rownames(designs)<-NULL
designs$job_id <- seq_len(nrow(designs))
workers <- min(cfg$maximum_workers,parallelly::availableCores())
requested <- suppressWarnings(as.integer(Sys.getenv("MTI_ECM_NOMINAL_WORKERS","")))
if(is.finite(requested))workers <- min(workers,max(1L,requested))
programs <- unique(c(configuration,"application/scripts/136_assess_mti_ecm_nominal.R",
  "application/scripts/137_launch_mti_ecm_nominal.sh",file.path("application/scripts/lib",paste0(libraries,".R")),mps_programs))
identity <- list(source_commit=mgfp_git("rev-parse","HEAD"),configuration=cfg,programs=mgfp_hashes(programs),
  package=context$package,R=R.version.string,rng=RNGkind(),generators=context$generator_sha256,
  seed_audit=plan$audit,development_tasks_sha256=mwd_hash(development),source=source,
  exdqlm_reference_commit="dffb71ee70b597d6a716ee74be1cbc99731cd453",
  article_files=mgfp_hashes(c("main.tex","rqr-gibbs-supplement.tex")))
if(mode=="prepare") {
  print(list(reused_development_datasets=nrow(development),designs=nrow(designs),fresh_validation_datasets=nrow(plan$tasks),
    maximum_workers=workers,prior_sd=cfg$prior_sd,DP=context$dp,seed_audit=plan$audit))
  mps_disk_check(cfg$output_root,cfg$disk_reserve_gib);quit(save="no")
}
if(mode=="check") {
  tasks <- development[development$replication==1 &
    ((development$n==100 & development$content==.9 & development$response_id=="exponential") |
     (development$n==500 & development$content==.99 & development$response_id=="normal")),,drop=FALSE]
  checks <- lapply(seq_len(nrow(tasks)),function(i) {
    task <- tasks[i,,drop=FALSE]
    policy <- data.frame(n=task$n,content=task$content,method=c("ecm_primary","ecm_check_lower_threshold"),
      gamma=c(mec_primary(context,task$n,task$content),task$tolerance_confidence),available=TRUE)
    tic <- proc.time()[3];a <- mec_job(task,policy,cfg,context)
    file <- file.path(cfg$output_root,paste0("rehearsal_",i,".rds"))
    mbc_atomic_fit(a,file,"ecm_rehearsal",task)
    b <- mbc_read_fit(file,"ecm_rehearsal",task)
    mwd_require(identical(unserialize(serialize(a,NULL)),b) && !length(a$fits) && all(a$rows$numerically_accurate),
      "The real-sample ECM-only rehearsal requires review.")
    list(task=task,elapsed_seconds=unname(proc.time()[3]-tic),rows=as.data.frame(a$rows))
  })
  design <- designs[designs$n==100 & designs$content==.9,,drop=FALSE]
  replay <- mec_profile(design,development,source,context,cfg)
  saveRDS(replay,file.path(cfg$output_root,"rehearsal_development_profile.rds"))
  mwd_require(replay$ok && replay$reused_datasets==800 && replay$primary_gamma==mec_primary(context,100,.9),
    "The real cached-candidate threshold rehearsal requires review.")
  mgfp_json_write(list(passed=TRUE,checks=checks,replayed_development_datasets=replay$reused_datasets,
    used_development_observations_only=TRUE,
    no_gibbs_sampling=TRUE,time=mgfp_time()),file.path(cfg$output_root,"rehearsal.json"))
  cat("ECM-only real-sample and receipt rehearsal passed.\n");quit(save="no")
}
if(mode=="verify") {
  mec_verify(cfg,context);quit(save="no")
}
mwd_require(!nzchar(mgfp_git("status","--porcelain")),"Commit source before the independent exploration.")
manifest <- file.path(cfg$output_root,"manifest.json")
if(file.exists(manifest))mwd_require(mwd_json(manifest)$identity_hash==mwd_hash(identity),"Study identity changed.") else
  mgfp_json_write(list(identity=identity,identity_hash=mwd_hash(identity),started=mgfp_time()),manifest)
stage <- "development"
tryCatch({
  profile <- mjs_queue(designs,stage,cfg,identity,workers,
    function(design)mec_profile(design,development,source,context,cfg))
  pieces <- lapply(seq_len(nrow(designs)),function(i)mjs_read(profile,designs,i))
  curve <- data.table::rbindlist(lapply(pieces,`[[`,"rows"))
  policy <- data.table::rbindlist(lapply(pieces,`[[`,"selection"))
  mwd_write(curve,file.path(cfg$output_root,"development/threshold_curves.csv"))
  mwd_write(policy,file.path(cfg$output_root,"frozen_selection.csv"))
  frozen <- file.path(cfg$output_root,"frozen_selection.rds")
  if(file.exists(frozen))mwd_require(identical(serialize(readRDS(frozen),NULL),serialize(policy,NULL)),
    "Frozen ECM rules changed.") else saveRDS(policy,frozen)
  tasks_file <- file.path(cfg$output_root,"validation_tasks.rds")
  if(file.exists(tasks_file))mwd_require(identical(readRDS(tasks_file),plan$tasks),"Frozen assessment tasks changed.") else saveRDS(plan$tasks,tasks_file)
  freeze <- list(selection_sha256=mwd_file_hash(frozen),tasks_sha256=mwd_file_hash(tasks_file),
    source_commit=identity$source_commit,frozen_before_fresh_data=TRUE,
    development_only_selection=TRUE,automatic_article_replacement=FALSE,
    unavailable_rules=as.data.frame(policy[!available,.(n,content,method,reason)]))
  freeze_file <- file.path(cfg$output_root,"frozen_policy.json")
  if(file.exists(freeze_file)) {
    old <- mwd_json(freeze_file)
    mwd_require(old$selection_sha256==freeze$selection_sha256 && old$tasks_sha256==freeze$tasks_sha256,
      "Frozen policy or independent task hashes changed.")
  } else mgfp_json_write(freeze,freeze_file)
  stage <- "validation"
  assessment <- mjs_queue(plan$tasks,stage,cfg,list(identity,freeze),workers,
    function(task)mec_job(task,policy,cfg,context))
  rows <- mjs_collect(assessment,plan$tasks)
  mwd_write(rows,file.path(cfg$output_root,"validation/interval_results.csv"))
  mec_report(rows,cfg)
  mec_verify(cfg,context)
  mgfp_json_write(list(status="complete_exploratory_requires_scientific_review",article_replaced=FALSE,
    development_datasets_reused=nrow(development),validation_datasets=nrow(plan$tasks),
    numerical_pass=TRUE,source_commit=identity$source_commit,time=mgfp_time()),file.path(cfg$output_root,"study_summary.json"))
},error=function(e) {
  mgfp_json_write(list(status="stopped_requires_review",stage=stage,reason=conditionMessage(e),
    article_replaced=FALSE,time=mgfp_time()),file.path(cfg$output_root,"study_summary.json"));stop(e)
})
