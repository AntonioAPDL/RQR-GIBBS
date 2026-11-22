msx_check <- function(cfg,context,directory,workers) {
  admission<-msx_admission(cfg)
  historical_path<-names(context$historical$inputs)
  historical_path<-historical_path[grepl("/validation/interval_results.csv$",historical_path)]
  mwd_require(length(historical_path)==1L && file.exists(historical_path),"Article-reference observations are unavailable.")
  mwd_require(mwd_file_hash(historical_path)==context$historical$inputs[[historical_path]],"Article-reference source hash differs.")
  old<-data.table::fread(historical_path)[method=="ecm" & replication==1L]
  fixtures<-do.call(rbind,lapply(seq_len(nrow(cfg$design)),function(i){
    design<-cfg$design[i,]
    d<-old[n==design$n & content==design$content]
    wanted<-c("normal","exponential","gamma05","lognormal_hard","student_t3","beta18","normal","exponential","lognormal_hard")[i]
    d[response_id==wanted]
  }))
  mwd_require(nrow(fixtures)==9L,"Nine article-reference fixtures are required.")
  reference<-parallel::mclapply(seq_len(nrow(fixtures)),function(i){
    target<-fixtures[i];RNGkind("Mersenne-Twister","Inversion","Rejection");set.seed(target$data_seed)
    y<-context$old_laws[[target$response_id]]$r(target$n)
    task<-as.data.frame(target[,.(n,content,tolerance_confidence,retained_fraction)])
    task$ecm_seed<-as.integer(1660000000+100*target$job_id)
    fit<-msx_fit(y,task,context,cfg)
    mwd_require(isTRUE(fit$returned) && fit$unresolved_candidates==0L,"An article-reference fit is unresolved.")
    selected<-fit$selected
    error<-max(abs(c(selected$lower-target$lower,selected$upper-target$upper)))
    mwd_require(error<1e-8 && abs(selected$target_content-target$selected_q)<1e-12 &&
      abs(selected$mean_tilt-target$selected_delta)<1e-12 &&
      abs(selected$posterior_content_probability-target$dp_content_probability)<1e-12,
      "New runtime does not reproduce the article's selected ECM target/endpoints.")
    msx_dp_check(y,fit$candidates,task$content)
    comparison<-mfp_comparators(y,task$content,task$tolerance_confidence)
    ym<-as.data.frame(tolerance::nptol.int(y,alpha=1-task$tolerance_confidence,P=task$content,side=2,method="YM"))
    mwd_require(nrow(ym)==1L && all(c("2-sided.lower","2-sided.upper") %in% names(ym)) &&
      identical(as.numeric(ym$`2-sided.lower`),comparison$lower[comparison$method=="young_mathew"]) &&
      identical(as.numeric(ym$`2-sided.upper`),comparison$upper[comparison$method=="young_mathew"]) &&
      comparison$lower[comparison$method=="wilks"]==min(y) &&
      comparison$upper[comparison$method=="wilks"]==max(y),"Comparator calculations differ.")
    data.frame(n=task$n,content=task$content,law_id=target$response_id,article_job_id=target$job_id,
      sample_sha256=mwd_hash(y),maximum_endpoint_difference=error,candidates=nrow(fit$candidates))
  },mc.cores=min(workers,9L),mc.set.seed=FALSE)
  mwd_require(all(vapply(reference,is.data.frame,FALSE)),paste("Historical compatibility check failed:",
    paste(vapply(reference[!vapply(reference,is.data.frame,FALSE)],as.character,""),collapse="; ")))
  # A separate stream allocation is used only for numerical checks, never assessment.
  check_cfg<-cfg;check_cfg$data_stream_seed<-cfg$data_stream_seed-1L
  check_cfg$replications<-1L
  design<-msx_design(check_cfg,admission)
  design$tasks$ecm_seed<-as.integer(1900000000+10*design$tasks$job_id)
  dir.create(file.path(directory,"checks"),showWarnings=FALSE)
  checked<-parallel::mclapply(seq_len(nrow(design$tasks)),function(i){
    task<-design$tasks[i,,drop=FALSE]
    value<-msx_job(task,design$streams[[i]],context,cfg)
    mwd_require(value$resolved,"A new-law correctness fit is numerically unresolved.")
    mp_atomic_rds(value,file.path(directory,"checks",sprintf("test_%03d.rds",i)))
    list(task_key=task$task_key,sample_sha256=value$sample$sample_sha256,
      elapsed_seconds=value$elapsed_seconds,bytes=as.numeric(object.size(value)),
      rows=as.data.frame(value$rows),warnings=value$warnings)
  },mc.cores=min(workers,nrow(design$tasks)),mc.set.seed=FALSE)
  mwd_require(all(vapply(checked,is.list,FALSE)),paste("New-law numerical tests failed:",
    paste(vapply(checked[!vapply(checked,is.list,FALSE)],as.character,""),collapse="; ")))
  mwd_write(do.call(rbind,reference),file.path(directory,"compatibility_fixtures.csv"))
  mwd_write(data.table::rbindlist(lapply(checked,`[[`,"rows")),file.path(directory,"correctness_test_intervals.csv"))
  mwd_write(admission,file.path(directory,"distribution_admission.csv"))
  record<-list(passed=TRUE,source_commit=mgfp_git("rev-parse","HEAD"),
    implementation=mgfp_hashes(c("application/config/mti_skewness_extension_20261121.json",
      file.path("application/scripts/lib",paste0(msx_sources,".R")))),
    article_reference_fixtures=length(reference),new_law_design_checks=length(checked),
    admission=admission,test_samples_excluded_from_assessment=TRUE,
    seconds_per_task=vapply(checked,`[[`,0,"elapsed_seconds"),
    saved_bytes_per_task=vapply(checked,`[[`,0,"bytes"),
    sample_hashes=vapply(checked,`[[`,"","sample_sha256"),
    prior_uses_analysis_sample=FALSE,sample_gibbs=FALSE,time=mgfp_time())
  mgfp_json_write(record,file.path(directory,"prelaunch_checks.json"))
  record
}
