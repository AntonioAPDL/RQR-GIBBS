#!/usr/bin/env Rscript
Sys.setenv(OMP_NUM_THREADS=1,OPENBLAS_NUM_THREADS=1,MKL_NUM_THREADS=1,BLIS_NUM_THREADS=1)
texbin<-Sys.getenv("MTI_SKEWNESS_TEX_BIN",file.path(Sys.getenv("HOME"),"texlive-slides/2025/bin/x86_64-linux"))
if(!nzchar(Sys.which("latexmk")) && file.exists(file.path(texbin,"latexmk")))
  Sys.setenv(PATH=paste(texbin,Sys.getenv("PATH"),sep=.Platform$path.sep))
private<-Sys.getenv("MTI_SKEWNESS_R_LIBRARY","application/cache/skewness_library")
.libPaths(c(normalizePath(private,mustWork=TRUE),.libPaths()))
libraries<-c("mti_gibbs_practical","mti_gibbs_full_report_review","mti_gibbs_full_publication",
  "mti_profile_revision","mti_gibbs_width_diagnosis","mti_prior_screen","pharma_tolerance_application",
  "mti_fixed_prior","mti_gibbs_decision_calibration","mti_skewness_distributions",
  "mti_skewness_extension","mti_skewness_reporting","mti_skewness_checks")
for(name in libraries)source(file.path("application/scripts/lib",paste0(name,".R")))
data.table::setDTthreads(1L);options(datatable.optimize=0L)
RNGkind("Mersenne-Twister","Inversion","Rejection")
if(Sys.getenv("MTI_SKEWNESS_NO_MAIN")!="1") {
  args<-commandArgs(TRUE);mode<-if(length(args))args[1] else "prepare"
  mwd_require(mode %in% c("admission","prepare","check","run","health","verify","export"),"Unknown skewness-study command.")
  cfg<-msx_configuration("application/config/mti_skewness_extension_20261121.json")
  commit<-mgfp_git("rev-parse","HEAD")
  directory<-Sys.getenv("MTI_SKEWNESS_OUTPUT_ROOT",file.path(
    Sys.getenv("MTI_SKEWNESS_STORAGE_ROOT","/var/tmp/jaguir26-mti-studies"),paste0(cfg$study_id,"_",commit)))
  pointer<-file.path(mgfp_common(),".codex_work/mti_skewness_extension_latest.json")
  if(mode=="health") {
    if(!nzchar(Sys.getenv("MTI_SKEWNESS_OUTPUT_ROOT")) && file.exists(pointer))directory<-mwd_json(pointer)$output_root
    cat("Study:",directory,"\n")
    for(name in c("study_status.json","progress.json","prelaunch_checks.json","verification.json","controller_exit.json")) {
      path<-file.path(directory,name)
      if(file.exists(path)){
        x<-mwd_json(path)
        if(name=="prelaunch_checks.json")x<-x[c("passed","article_reference_fixtures","new_law_design_checks")]
        print(setNames(list(x),name))
      }
    }
    cat("Saved dataset receipts:",length(list.files(file.path(directory,"fits"),pattern="[.]rds[.]json$")),"\n")
    quit(save="no")
  }
  dir.create(directory,recursive=TRUE,showWarnings=FALSE)
  if(mode=="admission"){
    z<-msx_admission(cfg);mwd_write(z,file.path(directory,"distribution_admission.csv"));print(z);quit(save="no")
  }
  workers<-as.integer(Sys.getenv("MTI_SKEWNESS_WORKERS",cfg$maximum_workers))
  mwd_require(length(workers)==1L && !is.na(workers) && workers>=1L && workers<=cfg$maximum_workers,
    "Worker count must be between one and the configured maximum.")
  workers<-min(workers,parallel::detectCores())
  memory<-readLines("/proc/meminfo")
  available_memory<-as.numeric(strsplit(trimws(sub("MemAvailable:","",grep("^MemAvailable:",memory,value=TRUE))),"[[:space:]]+")[[1]][1])/1024^2
  load<-as.numeric(strsplit(readLines("/proc/loadavg")," ")[[1]][1])
  workers<-min(workers,max(0L,floor(parallel::detectCores()-load)),
    max(0L,floor((available_memory-cfg$memory_reserve_gib)/cfg$memory_per_worker_gib)))
  mwd_require(workers>=1L,"Insufficient currently available CPU/memory for a worker.")
  mps_disk_check(directory,cfg$disk_reserve_gib)
  context<-msx_context(cfg,directory)
  if(mode=="check") {print(msx_check(cfg,context,directory,workers)[c("passed","article_reference_fixtures","new_law_design_checks")]);quit(save="no")}
  if(mode=="run")mwd_require(!nzchar(mgfp_git("status","--porcelain")),"Commit source before assessment.")
  phase<-"preparing"
  stage<-function(name){phase<<-name;mgfp_json_write(list(stage=name,source_commit=commit,workers=workers,
    article_replaced=FALSE,time=mgfp_time()),file.path(directory,"study_status.json"))}
  if(mode %in% c("run","verify","export"))options(error=function(){
    mgfp_json_write(list(stage="stopped_requires_review",failed_phase=phase,reason=geterrmessage(),
      source_commit=commit,article_replaced=FALSE,time=mgfp_time()),file.path(directory,"study_status.json"))
    quit(save="no",status=1L)
  })
  prepared<-msx_prepare(cfg,context,directory)
  if(mode=="prepare"){
    print(prepared$admission);print(list(datasets=nrow(prepared$design$tasks),methods=cfg$methods,
      workers=workers,output_root=directory));quit(save="no")
  }
  if(mode=="export"){print(msx_export(cfg,context,directory));quit(save="no")}
  if(mode=="run"){
    stage("prelaunch_checks")
    checks<-file.path(directory,"prelaunch_checks.json")
    valid<-FALSE
    if(file.exists(checks)){
      previous<-mwd_json(checks)
      valid<-isTRUE(previous$passed) && identical(previous$source_commit,commit) &&
        identical(previous$implementation,mgfp_hashes(names(previous$implementation)))
    }
    if(!valid)msx_check(cfg,context,directory,workers)
    unit_status<-system2(file.path(R.home("bin"),"Rscript"),"application/tests/test_skewness_extension.R",
      stdout=file.path(directory,"unit_tests.log"),stderr=file.path(directory,"unit_tests.log"))
    Sys.setenv(MTI_SKEWNESS_CHECK_ROOT=directory)
    reporting_status<-system2(file.path(R.home("bin"),"Rscript"),"application/tests/test_skewness_reporting.R",
      stdout=file.path(directory,"reporting_tests.log"),stderr=file.path(directory,"reporting_tests.log"))
    mwd_require(unit_status==0L && reporting_status==0L,"Frozen-source unit/reporting tests failed; assessment not started.")
    mgfp_json_write(list(passed=TRUE,unit_exit=unit_status,reporting_exit=reporting_status,
      source_commit=commit,time=mgfp_time()),file.path(directory,"execution_tests.json"))
    expected_hashes<-mwd_json(checks)$sample_hashes
    assessment_hashes<-vapply(seq_len(min(54,nrow(prepared$design$tasks))),function(i){
      t<-prepared$design$tasks[i,,drop=FALSE]
      msx_generate(t,context$laws[[t$law_id]],prepared$design$streams[[i]])$sample_sha256
    },"")
    mwd_require(!length(intersect(expected_hashes,assessment_hashes)),
      "Numerical-test and assessment identifiers unexpectedly overlap.")
    stage("fitting")
    msx_run(cfg,context,prepared,directory,workers)
  }
  stage("verifying")
  msx_verify(cfg,context,prepared,directory,workers)
  stage("reporting")
  print(msx_export(cfg,context,directory))
  stage("complete_requires_scientific_review")
}
