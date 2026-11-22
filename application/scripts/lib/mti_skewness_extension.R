msx_configuration <- function(path) {
  cfg <- mwd_json(path)
  cfg$design <- read.csv(cfg$scan_csv)
  stopifnot(cfg$replications==1000L,cfg$maximum_workers<=40L,cfg$maximum_workers>=1L,
    cfg$prior_sd==2,cfg$learning_rate==1,cfg$radius_multiplier==1,
    cfg$dp_concentration==1,cfg$dp_base_sd==4,!cfg$sample_gibbs,
    cfg$generalized_bayes,!cfg$response_likelihood,!cfg$posterior_predictive_response_draws,
    !cfg$automatic_article_replacement,!cfg$automatic_prior_tuning,
    identical(cfg$methods,c("ecm","young_mathew","wilks")),nrow(cfg$design)==9L,
    all(cfg$design$tolerance_confidence==.95),
    all(pbeta(cfg$design$content,cfg$design$n-1,2,lower.tail=FALSE)>=.95))
  cfg
}

msx_sources <- c("mti_gibbs_practical","mti_gibbs_full_report_review","mti_gibbs_full_publication",
  "mti_profile_revision","mti_gibbs_width_diagnosis","mti_prior_screen","pharma_tolerance_application",
  "mti_fixed_prior","mti_gibbs_decision_calibration","mti_skewness_distributions",
  "mti_skewness_extension","mti_skewness_reporting","mti_skewness_checks")

msx_context <- function(cfg, directory) {
  dir.create(directory,recursive=TRUE,showWarnings=FALSE)
  provenance <- mwd_json(file.path(cfg$historical_evidence,"provenance.json"))
  for (f in names(provenance$data_files)) mwd_require(identical(
    mwd_file_hash(file.path(cfg$historical_evidence,f)),provenance$data_files[[f]]),"Article evidence changed.")
  v <- mwd_json(cfg$validation_config)
  context <- list(profile=v$engine_defaults$mti_ecm_dp_profile,dp=v$engine_defaults$direct_dp,
    policy=read.csv(cfg$policy_csv),prior=mwd_json(cfg$prior_config),
    policy_diagnostics=mwd_json("application/config/mti_gibbs_practical_20261112.json")$policy,
    historical=provenance)
  mwd_require(context$prior$primary_kappa==2 && !context$prior$simulation_reference$estimated_from_analysis_sample &&
    context$prior$simulation_reference$center==0 && context$prior$simulation_reference$scale==1 &&
    context$dp$concentration==1 && context$dp$base$mean==0 && context$dp$base$sd==4,
    "The fixed independent prior specification differs.")
  article <- mwd_json("application/config/mti_ecm_article_20261111.json")
  generator <- file.path(directory,"historical_generators.R")
  original <- system2("git",c("--no-optional-locks","show",
    shQuote(paste0(article$archived_commit,":application/scripts/69_validate_rqr_bayes_uq.R"))),stdout=TRUE)
  mwd_require(is.null(attr(original,"status")),"Cannot read historical generator commit.")
  writeLines(original,generator)
  env <- mp_dgp_environment(generator)
  old <- jsonlite::read_json(cfg$validation_config,simplifyVector=FALSE)$dgps
  context$old_laws <- setNames(lapply(old,env$dgp_meta),vapply(old,`[[`,"","dgp_id"))
  specs <- cfg$laws
  context$laws <- setNames(lapply(seq_len(nrow(specs)),function(i)msx_law(as.list(specs[i,]))),specs$id)
  context
}

msx_package_record <- function() {
  packages <- c("rqrgibbs","tolerance","actuar","statmod","expint","Rcpp","data.table","jsonlite","digest")
  mwd_require(as.character(packageVersion("tolerance"))=="3.0.0","The YM package version changed.")
  records <- setNames(lapply(packages,function(p) {
    root <- find.package(p); files <- list.files(root,full.names=TRUE,recursive=TRUE)
    list(version=as.character(packageVersion(p)),files=mgfp_hashes(files))
  }),packages)
  # Compare the installed R implementation without sourcing or compiling external repositories.
  ns <- asNamespace("rqrgibbs"); e <- new.env()
  for (f in c("rqr_ecm_fit.R","rqr_ecm_utils.R","rqr_mti_ecm_profile.R","rqr_mti_ecm_adaptive.R"))
    sys.source(file.path("application/R",f),e)
  for (name in ls(e,all.names=TRUE)) mwd_require(exists(name,ns,inherits=FALSE) &&
    identical(deparse(e[[name]]),deparse(get(name,ns))),paste("Installed fixed-target implementation differs:",name))
  inventory_file<-file.path(dirname(.libPaths()[1]),"skewness_runtime.json")
  inventory<-mwd_json(inventory_file)
  actual<-mgfp_hashes(file.path(.libPaths()[1],names(inventory$installed_hashes)))
  mwd_require(identical(unname(unlist(actual)),unname(unlist(inventory$installed_hashes))),
    "The isolated dependency inventory differs.")
  records$runtime_inventory<-inventory
  records
}

msx_identity <- function(cfg,context) {
  files <- c("application/config/mti_skewness_extension_20261121.json",cfg$validation_config,
    cfg$policy_csv,cfg$scan_csv,cfg$prior_config,"application/config/mti_gibbs_practical_20261112.json",
    "application/scripts/144_run_mti_skewness_extension.R","application/scripts/145_launch_mti_skewness_extension.sh",
    "application/scripts/146_prepare_mti_skewness_runtime.R",
    "application/tests/test_skewness_extension.R","application/tests/test_skewness_reporting.R",
    file.path("application/scripts/lib",paste0(msx_sources,".R")))
  list(source_commit=mgfp_git("rev-parse","HEAD"),exdqlm_reference_commit=cfg$exdqlm_reference_commit,
    numerical_package_source_commit=cfg$source_reference_commit,
    configuration=cfg,programs=mgfp_hashes(files),packages=msx_package_record(),R=R.version.string,
    history=mgfp_hashes(file.path(cfg$historical_evidence,c("provenance.json","simulation_cells.csv","primary_widths.csv"))),
    fitting=list(profile=context$profile,dp=context$dp,policy=context$policy,prior=context$prior),
    sample_gibbs=FALSE,automatic_article_replacement=FALSE)
}

msx_design <- function(cfg,admission) {
  ids <- sort(admission$law_id[admission$admitted])
  cases <- do.call(rbind,lapply(ids,function(id) cbind(law_id=id,cfg$design)))
  cases$case_id <- sprintf("%s_n%04d_c%03d_p%03d",cases$law_id,cases$n,
    round(100*cases$content),round(100*cases$tolerance_confidence))
  index <- rep(seq_len(nrow(cases)),cfg$replications)
  tasks <- cases[index,]; rownames(tasks)<-NULL
  tasks$replication <- rep(seq_len(cfg$replications),each=nrow(cases))
  tasks$job_id <- seq_len(nrow(tasks)); tasks$study_id<-cfg$study_id
  tasks$task_key <- paste(cfg$study_id,tasks$case_id,tasks$replication,sep=":")
  tasks$ecm_seed <- as.integer(cfg$ecm_seed_base+10*tasks$job_id)
  stopifnot(!anyDuplicated(tasks$task_key),!anyDuplicated(tasks$ecm_seed),!anyNA(tasks$ecm_seed))
  RNGkind("L'Ecuyer-CMRG","Inversion","Rejection");set.seed(cfg$data_stream_seed)
  streams <- vector("list",nrow(tasks)); state <- .Random.seed
  for(i in seq_along(streams)){streams[[i]]<-state;state<-parallel::nextRNGStream(state)}
  RNGkind("Mersenne-Twister","Inversion","Rejection")
  list(tasks=tasks,streams=streams)
}

msx_generate <- function(task,law,state) {
  RNGkind("L'Ecuyer-CMRG","Inversion","Rejection")
  assign(".Random.seed",state,envir=.GlobalEnv)
  raw <- law$raw_r(task$n); y <- (raw-law$mean)/law$sd
  RNGkind("Mersenne-Twister","Inversion","Rejection")
  mwd_require(length(y)==task$n && all(is.finite(y)),"Nonfinite generated observations.")
  list(raw=raw,y=y,sample_sha256=mwd_hash(y))
}

msx_dp_check <- function(y,candidates,content) {
  z<-sort(y)
  count<-findInterval(candidates$upper,z)-findInterval(candidates$lower,z,left.open=TRUE)
  mass<-pnorm(candidates$upper,sd=4)-pnorm(candidates$lower,sd=4)
  right<-candidates$lower>0
  mass[right]<-pnorm(candidates$lower[right],sd=4,lower.tail=FALSE)-
    pnorm(candidates$upper[right],sd=4,lower.tail=FALSE)
  a<-count+mass;b<-length(y)-count+1-mass
  probability<-pbeta(content,a,b,lower.tail=FALSE)
  mwd_require(all(count==candidates$observed_count) &&
    all(is.finite(probability)) && max(abs(probability-candidates$posterior_content_probability))<1e-12,
    "Independent closed-interval DP probability differs.")
  invisible(TRUE)
}

msx_fit <- function(y,task,context,cfg) {
  rule <- context$policy[context$policy$n==task$n & context$policy$content==task$content &
    context$policy$tolerance_confidence==task$tolerance_confidence,,drop=FALSE]
  mwd_require(nrow(rule)==1L,"The fixed design probability rule is missing.")
  mfp_fit_data(y,task$content,task$tolerance_confidence,task$retained_fraction,cfg$prior_sd,
    context$prior$simulation_reference,context$profile,context$dp,as.list(rule[1,]),
    context$prior$numerical,list(),task$ecm_seed,context$policy_diagnostics,sample_gibbs=FALSE)
}

msx_methods <- function(y,task,anchor,context,cfg) {
  failure<-anchor$reason %||% ""
  accurate<-!is.null(anchor$candidates) && identical(as.integer(anchor$unresolved_candidates),0L) &&
    all(anchor$candidates$numerically_accurate)
  cutoff<-context$policy$posterior_confidence[context$policy$n==task$n & context$policy$content==task$content]
  selected<-if(accurate)mdc_select(anchor$candidates,cutoff) else data.frame()
  returned<-nrow(selected)==1L
  row<-data.frame(method="ecm",returned=returned,lower=if(returned)selected$lower else NA_real_,
    upper=if(returned)selected$upper else NA_real_,width=if(returned)selected$width else NA_real_,
    q=if(returned)selected$target_content else NA_real_,delta=if(returned)selected$mean_tilt else NA_real_,
    dp_probability=if(returned)selected$posterior_content_probability else NA_real_,
    gamma=cutoff,numerically_accurate=accurate,
    failure_reason=if(returned)"" else if(nzchar(failure))failure else "no_eligible_candidate")
  comparator<-mfp_comparators(y,task$content,task$tolerance_confidence)
  comparator$numerically_accurate<-comparator$returned
  rows<-data.table::rbindlist(list(row,comparator),fill=TRUE)
  if(accurate)msx_dp_check(y,anchor$candidates,task$content)
  if(returned)mwd_require(mfp_accuracy(y,c(selected$lower,selected$upper),selected$target_content,
    selected$mean_tilt,4,context$prior$numerical)$pass,"Selected ECM numerical accuracy differs.")
  rows
}

msx_job <- function(task,state,context,cfg) {
  start<-proc.time()[3]; warnings<-character()
  sample<-msx_generate(task,context$laws[[task$law_id]],state)
  anchor<-tryCatch(withCallingHandlers(msx_fit(sample$y,task,context,cfg),
    warning=function(w){warnings<<-c(warnings,conditionMessage(w));invokeRestart("muffleWarning")}),
    error=function(e)list(reason=conditionMessage(e)))
  rows<-tryCatch(msx_methods(sample$y,task,anchor,context,cfg),error=function(e)e)
  if(inherits(rows,"error")){
    anchor$reason<-conditionMessage(rows)
    comparator<-mfp_comparators(sample$y,task$content,task$tolerance_confidence)
    comparator$numerically_accurate<-comparator$returned
    rows<-data.table::rbindlist(list(data.frame(method="ecm",returned=FALSE,lower=NA_real_,upper=NA_real_,
      width=NA_real_,q=NA_real_,delta=NA_real_,dp_probability=NA_real_,gamma=NA_real_,
      numerically_accurate=FALSE,failure_reason=anchor$reason),comparator),fill=TRUE)
  }
  law<-context$laws[[task$law_id]]
  rows[,population_content:=msx_content(law,lower,upper)]
  rows[,attained:=returned & !is.na(population_content) & population_content>=task$content]
  rows[,resolved:=numerically_accurate]
  for(name in c("study_id","task_key","job_id","law_id","case_id","n","content","tolerance_confidence","replication"))
    data.table::set(rows,j=name,value=task[[name]])
  rows[,sample_sha256:=sample$sample_sha256]
  keep<-intersect(names(anchor),c("prior","candidates","selected","selected_ecm_trace","selected_knot_adjustments",
    "unresolved_candidates","reason","unresolved_traces","initial_candidate_traces","failed_call"))
  list(task=task,sample=sample,anchor=anchor[keep],rows=rows,resolved=all(rows$resolved),
    warnings=warnings,elapsed_seconds=unname(proc.time()[3]-start))
}

msx_save <- function(value,path,identity) {
  mp_atomic_rds(value,path)
  mgfp_json_write(list(identity=identity,task_sha256=mwd_hash(value$task),
    sha256=mwd_file_hash(path),resolved=value$resolved),paste0(path,".json"))
}

msx_read <- function(path,identity,task) {
  receipt<-mwd_json(paste0(path,".json"))
  mwd_require(identical(receipt$identity,identity) && identical(receipt$task_sha256,mwd_hash(task)) &&
    identical(receipt$sha256,mwd_file_hash(path)),"Saved fit identity/checksum differs.")
  value<-readRDS(path)
  mwd_require(identical(value$task,task),"Saved task differs.")
  value
}

msx_prepare <- function(cfg,context,directory) {
  admission<-msx_admission(cfg)
  design<-msx_design(cfg,admission)
  identity<-msx_identity(cfg,context)
  id<-mwd_hash(list(identity,admission,design))
  file<-file.path(directory,"manifest.json")
  if(file.exists(file)) mwd_require(mwd_json(file)$identity_hash==id,"Study inputs changed; use a new study identity.") else {
    mwd_write(admission,file.path(directory,"distribution_admission.csv"))
    mp_atomic_rds(design,file.path(directory,"design.rds"))
    mwd_write(design$tasks,file.path(directory,"tasks.csv"))
    mgfp_json_write(list(identity=identity,identity_hash=id,admission=admission,
      tasks=nrow(design$tasks),methods=cfg$methods,started=mgfp_time()),file)
  }
  list(design=design,identity=id,admission=admission)
}

msx_run <- function(cfg,context,prepared,directory,workers,job=msx_job) {
  dir.create(file.path(directory,"fits"),showWarnings=FALSE)
  tasks<-prepared$design$tasks;streams<-prepared$design$streams
  paths<-file.path(directory,"fits",sprintf("fit_%06d.rds",tasks$job_id))
  done<-which(file.exists(paste0(paths,".json")))
  # Do not overwrite a partially published object; review it before resuming.
  mwd_require(!any(file.exists(paths) & !file.exists(paste0(paths,".json"))),"An incomplete saved object requires review.")
  unresolved<-0L
  for(i in done){old<-msx_read(paths[i],prepared$identity,tasks[i,,drop=FALSE]);unresolved<-unresolved+!old$resolved}
  pending<-setdiff(seq_len(nrow(tasks)),done);started<-Sys.time();completed<-0L
  for(batch in split(pending,ceiling(seq_along(pending)/(4L*workers)))) {
    mps_disk_check(directory,cfg$disk_reserve_gib)
    results<-parallel::mclapply(batch,function(i){
      value<-job(tasks[i,,drop=FALSE],streams[[i]],context,cfg)
      msx_save(value,paths[i],prepared$identity)
      list(resolved=value$resolved,seconds=value$elapsed_seconds)
    },mc.cores=min(workers,length(batch)),mc.preschedule=FALSE,mc.set.seed=FALSE)
    mwd_require(all(vapply(results,is.list,FALSE)),"A worker failed before saving; review the controller log.")
    unresolved<-unresolved+sum(!vapply(results,`[[`,FALSE,"resolved"))
    completed<-completed+length(batch)
    elapsed<-as.numeric(difftime(Sys.time(),started,units="secs"))
    mgfp_json_write(list(completed_datasets=length(done)+completed,expected_datasets=nrow(tasks),
      remaining_datasets=nrow(tasks)-length(done)-completed,unresolved_datasets=unresolved,
      workers=workers,eta_seconds=elapsed/completed*(nrow(tasks)-length(done)-completed),time=mgfp_time()),
      file.path(directory,"progress.json"))
    cat(mgfp_time(),length(done)+completed,"/",nrow(tasks),"datasets saved; unresolved",unresolved,"\n");flush.console()
  }
  invisible(paths)
}

msx_verify <- function(cfg,context,prepared,directory,workers) {
  tasks<-prepared$design$tasks;streams<-prepared$design$streams
  groups<-split(seq_len(nrow(tasks)),tasks$case_id)
  pieces<-parallel::mclapply(groups,function(ids){
    data.table::rbindlist(lapply(ids,function(i){
      task<-tasks[i,,drop=FALSE]
      value<-msx_read(file.path(directory,"fits",sprintf("fit_%06d.rds",task$job_id)),prepared$identity,task)
      sample<-msx_generate(task,context$laws[[task$law_id]],streams[[i]])
      mwd_require(value$resolved && identical(sample,value$sample) &&
        value$anchor$prior$prior_sd==2 && !value$anchor$prior$data_adaptive_prior,
        "Unresolved fit, changed observations or changed prior.")
      expected<-msx_methods(sample$y,task,value$anchor,context,cfg)
      actual<-value$rows[match(expected$method,value$rows$method),names(expected),with=FALSE]
      mwd_require(isTRUE(all.equal(as.data.frame(expected),as.data.frame(actual),tolerance=0,check.attributes=FALSE)),
        "Stored method endpoints or probabilities differ.")
      mass<-msx_content(context$laws[[task$law_id]],value$rows$lower,value$rows$upper)
      mwd_require(isTRUE(all.equal(mass,value$rows$population_content,tolerance=0,check.attributes=FALSE)) &&
        all(value$rows$attained==(value$rows$returned & mass>=task$content)) && nrow(value$rows)==3L &&
        !anyDuplicated(value$rows$method),"Stored true content/attainment differs.")
      value$rows
    }))
  },mc.cores=min(workers,length(groups)),mc.set.seed=FALSE)
  mwd_require(all(vapply(pieces,is.data.frame,FALSE)),"Verification has unresolved cases.")
  rows<-data.table::rbindlist(pieces)
  mwd_require(nrow(rows)==3L*nrow(tasks) && !anyDuplicated(rows[,.(task_key,method)]) &&
    all(rows[,.N,by=.(case_id,method)]$N==cfg$replications),"Incomplete assessment counts.")
  mwd_write(rows,file.path(directory,"interval_results.csv"))
  mgfp_json_write(list(passed=TRUE,identity_hash=prepared$identity,datasets=nrow(tasks),
    method_records=nrow(rows),numerical_problems=0,sample_gibbs=FALSE,prior_uses_analysis_data=FALSE,
    article_replaced=FALSE,interval_results_sha256=mwd_file_hash(file.path(directory,"interval_results.csv")),
    time=mgfp_time()),file.path(directory,"verification.json"))
  rows
}
