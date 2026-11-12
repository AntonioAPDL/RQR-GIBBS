mgc_default <- "application/config/mti_gibbs_ecm_comparison_20261111.json"

mgc_authorized <- function(confirmation,commit) {
  mpr_require(nchar(commit)==40L && identical(confirmation,commit),
    "Gibbs sampling is not authorized: explicit confirmation of the article commit is required.")
  invisible(TRUE)
}

mgc_design <- function(cfg,stage="diagnostic") {
  mpr_require(stage %in% c("diagnostic","full"),"Unknown comparison stage.")
  article <- mre_config(cfg$ecm_article_config)
  provenance <- jsonlite::read_json(file.path(article$evidence_dir,"provenance.json"))
  pma_verify_hash(file.path(article$cache_dir,"results.csv"),provenance$results_sha256)
  evidence <- read.csv(file.path(article$cache_dir,"results.csv"))
  evidence <- evidence[evidence$method==article$primary_method,]
  if(stage=="diagnostic")evidence <- evidence[evidence$replication<=
    cfg$diagnostic_replications_per_case,]
  simulation <- data.frame(source="simulation",case_id=paste0("sim_",evidence$cell_index,
    "_",evidence$replication),response_id=evidence$dgp_id,n=evidence$n,
    content=evidence$content,tolerance_confidence=evidence$tolerance_confidence,
    replication=evidence$replication,data_seed=evidence$seed,
    dataset_sha256=evidence$dataset_sha256,q=evidence$selected_q,
    delta=evidence$selected_delta,ecm_lower=evidence$lower,ecm_upper=evidence$upper,
    split_scheme="",split_id=NA_integer_)
  app_cfg <- pma_read_config(cfg$application_config)
  pma_verify_run(app_cfg$paths$canonical_dir,app_cfg)
  results <- pta_read_csv(file.path(app_cfg$paths$canonical_dir,"results.csv"))
  app <- results[results$method_id==pma_methods[1],]
  if(stage=="diagnostic")app <- app[app$split_id<=cfg$diagnostic_replications_per_case,]
  application <- data.frame(source="application",case_id=paste("app",app$response_id,
    app$split_scheme,app$split_id,sep="_"),response_id=app$response_id,n=app$train_n,
    content=app$content,tolerance_confidence=app$tolerance_confidence,
    replication=app$split_id,data_seed=NA_integer_,dataset_sha256="",
    q=app$mti_selected_q,delta=app$mti_selected_tilt,
    ecm_lower=app$lower,ecm_upper=app$upper,split_scheme=app$split_scheme,split_id=app$split_id)
  tasks <- rbind(simulation,application)
  mpr_require(!anyDuplicated(tasks$case_id) && all(is.finite(tasks$q) & tasks$q<1 &
    tasks$q>tasks$content & is.finite(tasks$delta)),"Invalid fixed-target study design.")
  tasks$dataset_id <- seq_len(nrow(tasks))
  jobs <- tasks[rep(seq_len(nrow(tasks)),each=cfg$chains),]
  jobs$chain <- rep(seq_len(cfg$chains),nrow(tasks))
  jobs$seed <- cfg$seed_base+cfg$stage_seed_offsets[[stage]]+17L*jobs$dataset_id+jobs$chain
  jobs$job_id <- seq_len(nrow(jobs))
  mpr_require(!anyDuplicated(jobs$seed) && !any(jobs$seed %in% simulation$data_seed),
    "Chain seeds overlap or collide with dataset seeds.")
  jobs
}

mgc_sample <- function(task,context) {
  if(task$source=="simulation") {
    set.seed(task$data_seed)
    y <- context$laws[[task$response_id]]$r(task$n)
    mpr_require(identical(digest::digest(y,algo="sha256"),task$dataset_sha256),
      "Gibbs sample differs from the paired ECM sample.")
    return(y)
  }
  m <- context$membership
  use <- m$split_scheme==task$split_scheme & m$split_id==task$split_id & m$training
  ids <- m$analysis_row_id[use]
  index <- match(ids,context$observed$.analysis_row)
  mpr_require(length(ids)==task$n && !anyNA(index),"Application training membership is incomplete.")
  as.numeric(context$observed[[task$response_id]][index])
}

mgc_run <- function(cfg,jobs,stage,output,workers,resume=FALSE) {
  commit <- pma_git("rev-parse","HEAD")
  mpr_require(mpr_git_clean(pma_git("status","--porcelain")),
    "Commit the study source before Gibbs sampling.")
  runtime <- file.path("application/cache",paste0("mti_gibbs_runtime_",commit))
  mpr_require(file.exists(file.path(runtime,"commit.txt")) &&
    identical(readLines(file.path(runtime,"commit.txt")),commit),
    "Prepare the installed runtime from this exact commit first.")
  expected_library <- normalizePath(file.path(runtime,"library"),mustWork=TRUE)
  mpr_require(identical(normalizePath(dirname(find.package("rqrgibbs"))),expected_library),
    "Use R_LIBS pointing to the archived-commit runtime library.")
  mpr_require(workers>=1L && workers<=cfg$maximum_workers &&
    workers<=parallel::detectCores(),"Invalid worker count.")
  mpr_require(!dir.exists(output) || isTRUE(resume),
    "Use a new output directory or explicitly resume the identical study.")
  mpr_require(length(cfg$initial_midpoint_offsets_sd)==cfg$chains &&
    length(cfg$initial_halfwidth_multipliers)==cfg$chains &&
    all(cfg$initial_halfwidth_multipliers>0),"Incomplete dispersed-chain initializations.")
  external <- Sys.getenv("EXDQLM_RQR_REPO",
    "/data/muscat_data/jaguir26/exdqlm__wt__qdesn_0p4p0_integration")
  before <- rqr_capture_external_checkout(external)
  on.exit(rqr_assert_external_checkout_unchanged(before),add=TRUE)
  mpr_require(identical(before$commit,cfg$exdqlm_reference_commit),
    "The external reference is not at the pinned commit; do not change it automatically.")
  if(stage=="full") {
    diagnostic <- file.path(cfg$output_root,"diagnostic", "assessment.json")
    mpr_require(file.exists(diagnostic),"Assess the diagnostic stage before a full comparison.")
    assessment <- jsonlite::read_json(diagnostic,simplifyVector=TRUE)
    mpr_require(isTRUE(assessment$passed) && identical(assessment$commit,commit),
      "The diagnostic stage has not passed for this commit.")
  }
  dir.create(file.path(output,"chains"),recursive=TRUE,showWarnings=FALSE)
  article <- mre_config(cfg$ecm_article_config); f <- mre_frozen_functions(article)
  validation <- jsonlite::read_json(article$validation_config,simplifyVector=FALSE)
  laws <- setNames(lapply(validation$dgps,f$laws$dgp_meta),vapply(validation$dgps,`[[`,"","dgp_id"))
  app <- pma_read_config(cfg$application_config)
  pma_verify_run(app$paths$canonical_dir,app)
  context <- list(laws=laws,
    observed=pta_read_csv(file.path(app$paths$canonical_dir,"observed_batches.csv")),
    membership=pta_read_csv(file.path(app$paths$canonical_dir,"split_membership.csv.gz")))
  manifest_path <- file.path(output,"manifest.json")
  settings_sha256 <- digest::digest(cfg,algo="sha256")
  if(file.exists(manifest_path)) {
    old <- jsonlite::read_json(manifest_path,simplifyVector=TRUE)
    mpr_require(identical(old$commit,commit) && identical(old$settings_sha256,settings_sha256) &&
      identical(old$stage,stage) && isTRUE(all.equal(jobs,read.csv(file.path(output,"jobs.csv")),
        check.attributes=FALSE)),"The requested resume changes the frozen study.")
  } else {
    pma_write_csv(jobs,file.path(output,"jobs.csv"))
    pta_atomic_write_json(list(commit=commit,stage=stage,workers=workers,
    threads_per_worker=1,chains=nrow(jobs),datasets=length(unique(jobs$dataset_id)),
    settings=cfg,settings_sha256=settings_sha256,runtime=expected_library,
    session=capture.output(sessionInfo()),
    external_reference=before,exdqlm_loaded=FALSE,
    inputs=pma_file_record(c(cfg$ecm_article_config,cfg$application_config,
      file.path(article$evidence_dir,"provenance.json")))),manifest_path)
  }
  pta_atomic_write_json(list(workers=workers,pid=Sys.getpid(),
    started_utc=format(Sys.time(),tz="UTC",usetz=TRUE)),file.path(output,"execution.json"))
  work <- function(i) {
    task <- jobs[i,]; started <- proc.time()[[3]]
    path <- file.path(output,"chains",sprintf("chain_%06d.rds",i))
    tryCatch({
      if(file.exists(path)) {
        previous <- readRDS(path)
        mpr_require(isTRUE(previous$passed) && isTRUE(all.equal(previous$task,task,
          check.attributes=FALSE)),"A completed chain has an incompatible target or seed.")
        return(TRUE)
      }
      y <- mgc_sample(task,context); scale <- max(sd(y),1e-4)
      midpoint <- (task$ecm_lower+task$ecm_upper)/2+
        cfg$initial_midpoint_offsets_sd[task$chain]*scale
      halfwidth <- (task$ecm_upper-task$ecm_lower)/2*
        cfg$initial_halfwidth_multipliers[task$chain]
      fit <- rqrgibbs::rqr_mcmc_fit(y,matrix(1,length(y),1),task$q,
        learning_rate=cfg$learning_rate,mean_tilt=task$delta,
        beta_prior_obj=rqrgibbs::beta_prior("ridge",ridge=list(tau2=cfg$prior_variance)),
        numerical_policy="fail",mcmc_control=list(n_burn=cfg$n_burn,n_mcmc=cfg$n_mcmc,
          thin=cfg$thin,seed=task$seed,verbose=FALSE,store_latent_draws=FALSE),
        init=list(beta1=midpoint-halfwidth,beta2=midpoint+halfwidth))
      mpr_require(isTRUE(fit$model_spec$target_numerical_eligible) &&
        fit$model_spec$numerical_repair_count==0L,"The sampler reports a numerical repair.")
      a <- as.numeric(fit$samp.beta_root1); b <- as.numeric(fit$samp.beta_root2)
      mpr_require(length(a)==cfg$n_mcmc && length(b)==cfg$n_mcmc &&
        all(is.finite(a) & is.finite(b)),"Incomplete or nonfinite posterior root draws.")
      draws <- cbind(lower=pmin(a,b),upper=pmax(a,b),width=abs(b-a))
      target_loss <- fit$diagnostics$mean_tilted_target_loss_trace
      retained <- cfg$n_burn+seq_len(cfg$n_mcmc)*cfg$thin
      objective <- cfg$learning_rate*target_loss[retained]+(a^2+b^2)/(2*cfg$prior_variance)
      record <- list(task=task,draws=draws,target_loss_trace=target_loss,
        retained_objective=objective,
        repairs=fit$diagnostics$numerical_repairs,
        elapsed_seconds=proc.time()[[3]]-started,
        sample_sha256=digest::digest(y,algo="sha256"),passed=TRUE)
      saveRDS(record,paste0(path,".tmp")); stopifnot(file.rename(paste0(path,".tmp"),path))
      TRUE
    },error=function(e) {
      pta_atomic_write_json(list(job=i,error=conditionMessage(e),task=task),paste0(path,".error.json"))
      FALSE
    })
  }
  passed <- parallel::mclapply(seq_len(nrow(jobs)),work,mc.cores=workers,
    mc.preschedule=FALSE,mc.set.seed=FALSE)
  mpr_require(all(vapply(passed,isTRUE,logical(1))),"Some chains failed; inspect preserved chain error records.")
  mgc_assess(cfg,output)
}

mgc_assess <- function(cfg,output) {
  jobs <- read.csv(file.path(output,"jobs.csv"))
  rows <- lapply(split(jobs,jobs$dataset_id),function(s) {
    files <- file.path(output,"chains",sprintf("chain_%06d.rds",s$job_id))
    mpr_require(all(file.exists(files)),"A dataset has incomplete chains.")
    fits <- lapply(files,readRDS)
    a <- array(NA_real_,c(cfg$n_mcmc,cfg$chains,3),dimnames=list(NULL,NULL,c("lower","upper","width")))
    for(i in seq_along(fits))a[,i,] <- fits[[i]]$draws
    d <- posterior::summarise_draws(posterior::as_draws_array(a))
    data.frame(case_id=s$case_id[1],source=s$source[1],q=s$q[1],delta=s$delta[1],d,
      ecm_value=c(s$ecm_lower[1],s$ecm_upper[1],s$ecm_upper[1]-s$ecm_lower[1]),
      elapsed_seconds=sum(vapply(fits,`[[`,numeric(1),"elapsed_seconds")),
      diagnostics_pass=all(is.finite(d$rhat) & d$rhat<=cfg$rhat_limit &
        d$ess_bulk>=cfg$minimum_bulk_ess & d$ess_tail>=cfg$minimum_tail_ess))
  })
  result <- do.call(rbind,rows)
  pma_write_csv(result,file.path(output,"posterior_summary.csv"))
  manifest <- jsonlite::read_json(file.path(output,"manifest.json"),simplifyVector=TRUE)
  pta_atomic_write_json(list(commit=manifest$commit,passed=all(result$diagnostics_pass),
    datasets=length(unique(result$case_id)),chains=nrow(jobs),
    new_tolerance_guarantee=FALSE,article_replaced=FALSE),file.path(output,"assessment.json"))
  invisible(result)
}

mgc_health <- function(output) {
  jobs <- read.csv(file.path(output,"jobs.csv"))
  completed <- list.files(file.path(output,"chains"),pattern="^chain_[0-9]+[.]rds$")
  names <- sprintf("chain_%06d.rds",jobs$job_id)
  complete_ids <- jobs$job_id[names %in% completed]
  n <- length(complete_ids)
  remaining <- nrow(jobs)-n
  selected <- if(n)unique(round(seq(1,n,length.out=min(n,32L)))) else integer()
  paths <- file.path(output,"chains",sprintf("chain_%06d.rds",complete_ids[selected]))
  elapsed <- if(n)vapply(lapply(paths,readRDS),`[[`,numeric(1),"elapsed_seconds") else numeric()
  manifest <- jsonlite::read_json(file.path(output,"manifest.json"),simplifyVector=TRUE)
  execution_path <- file.path(output,"execution.json")
  workers <- if(file.exists(execution_path))jsonlite::read_json(execution_path)$workers else manifest$workers
  data.frame(completed_chains=n,expected_chains=nrow(jobs),remaining_chains=remaining,
    completed_datasets=sum(table(jobs$dataset_id[jobs$job_id %in% complete_ids])==manifest$settings$chains),
    estimated_remaining_hours=if(n)remaining*mean(elapsed)/workers/3600 else NA_real_,
    assessment_complete=file.exists(file.path(output,"assessment.json")))
}
