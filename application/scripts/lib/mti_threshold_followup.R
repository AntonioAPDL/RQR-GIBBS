mpq_followup_design <- function(design,spec) {
  mpr_require(!is.null(spec$followup) &&
    identical(spec$threshold_rule,"independent_development_logit_gap_refinement") &&
    identical(as.numeric(spec$followup$original_error_fraction),.5),"Invalid threshold follow-up.")
  requested <- do.call(rbind,lapply(spec$followup$reassess_cells,as.data.frame))
  mpr_require(nrow(requested)==2L && !anyDuplicated(requested),"Expected two distinct follow-up settings.")
  selected <- vapply(seq_len(nrow(design)),function(i)
    any(requested$n==design$n[i] & requested$content==design$content[i]),logical(1))
  result <- design[selected,,drop=FALSE]
  mpr_require(nrow(unique(result[c("n","content")]))==nrow(requested) &&
    nrow(result)==nrow(requested)*length(unique(design$dgp_id)),"Incomplete follow-up design.")
  result
}

mpq_followup_retained <- function(policy,assessment,cfg,spec) {
  cells <- nrow(policy); families <- length(unique(assessment$dgp_id))
  original_alpha <- (1-cfg$calibration$familywise_confidence)/
    (cells*families*(spec$threshold_quantile_count+1L))
  mpr_require(all(abs(assessment$alpha-original_alpha)<1e-14),"Unexpected original error allocation.")
  a <- assessment
  a$alpha <- original_alpha*spec$followup$original_error_fraction
  a$lower_bound <- mapply(mp_binom_lower,a$successes,a$replications,a$alpha)
  keep <- policy[is.finite(policy$threshold),,drop=FALSE]
  mpr_require(nrow(keep)==spec$followup$retained_settings &&
    all(keep$decision=="passed_new_calibration"),"Unexpected retained policy settings.")
  for(i in seq_len(nrow(keep))) {
    z <- a[mpr_cell_key(a)==mpr_cell_key(keep[i,]) & a$threshold==keep$threshold[i],]
    mpr_require(nrow(z)==families && all(z$returned==z$replications &
      z$lower_bound>=keep$tolerance_confidence[i]),"A retained threshold fails the tightened original bounds.")
  }
  list(policy=keep,assessment=a[mpr_cell_key(a) %in% mpr_cell_key(keep),,drop=FALSE],
    original_familywise_error=(1-cfg$calibration$familywise_confidence)*spec$followup$original_error_fraction)
}

mpq_followup_seed_check <- function(design,cfg,parent_spec) {
  current <- unlist(lapply(c("calibration","validation"),function(s)mpq_tasks(design,cfg,s)$seed),use.names=FALSE)
  previous <- mpq_config(parent_spec)
  old <- unlist(lapply(c("screen","calibration","validation"),function(s)
    mpq_tasks(design,previous,s)$seed),use.names=FALSE)
  mpr_require(!anyDuplicated(current) && !any(current %in% old),"Follow-up seeds overlap the completed parent study.")
  invisible(TRUE)
}

mpq_followup_reference <- function(run,spec,cfg,design) {
  run <- normalizePath(run,mustWork=TRUE)
  root <- normalizePath(file.path(run,"../../../.."),mustWork=TRUE)
  expected <- spec$followup
  identity <- readRDS(file.path(run,"identity.rds"))
  config <- file.path(root,expected$parent_config)
  mpr_require(identical(identity$commit,expected$parent_commit) &&
    identical(mp_sha(config),expected$parent_config_sha256),"Unexpected completed parent source.")
  for(records in list(identity$sources,identity$protected_inputs))for(i in seq_len(nrow(records))) {
    path <- records$path[i]
    if(!startsWith(path,"/"))path <- file.path(root,path)
    mpr_require(identical(mp_sha(path),records$sha256[i]),"Completed parent source or article inputs changed.")
  }
  parent_spec <- jsonlite::read_json(config,simplifyVector=FALSE)
  for(field in c("primary_method","refinement_rule","threshold_quantile_count",
    "baseline_commit","baseline_policy_sha256","screen_reference"))
    mpr_require(identical(spec[[field]],parent_spec[[field]]),"Follow-up changes the preserved numerical specification.")
  parent_cfg <- mpq_config(parent_spec)
  mpr_require(identical(cfg$profile,parent_cfg$profile) &&
    identical(mp_sha(cfg$validation_config),mp_sha(file.path(root,parent_cfg$validation_config))) &&
    identical(cfg$calibration$familywise_confidence,parent_cfg$calibration$familywise_confidence),
    "Follow-up changes the profile, distribution panel, or total error allowance.")
  numerical <- c(list.files("application/R",full.names=TRUE),
    list.files("application/src",pattern="[.](cpp|h)$|^Makevars",full.names=TRUE),
    "application/NAMESPACE","application/DESCRIPTION",
    "application/scripts/lib/pharma_mti_application.R",cfg$historical_policy,
    "tables/tolerance_validation_article_scan_calibration.csv")
  for(path in numerical)mpr_require(identical(mp_sha(path),mp_sha(file.path(root,path))),
    "Follow-up changes a numerical package, prior, or candidate input.")
  old_functions <- new.env(parent=globalenv())
  sys.source(file.path(root,"application/scripts/lib/mti_q_grid_study.R"),old_functions)
  for(name in c("mpq_fit","mpq_interval","mpq_one"))
    mpr_require(identical(body(get(name,old_functions)),body(get(name))) &&
      identical(formals(get(name,old_functions)),formals(get(name))),"Follow-up changes candidate fitting or selection.")
  complete_path <- file.path(run,"calibration/completion.json")
  complete <- mpr_read_json(complete_path)
  terminal <- mpr_read_json(file.path(run,"final_exit.json"))
  final <- mpr_read_json(file.path(run,"final_study_completion.json"))
  mpr_require(!is.null(complete) && !isTRUE(complete$passed) &&
    complete$numerical_failure_datasets==0 && !is.null(terminal) && terminal$exit_status==0 &&
    identical(final$decision,"stopped_unresolved_calibration") && !isTRUE(final$validation_started) &&
    identical(mp_sha(complete_path),expected$parent_completion_sha256),"Parent study did not finish in the required unresolved state.")
  files <- c(policy="policy.csv",assessment="threshold_assessment.csv",checkpoint_manifest="checkpoint_manifest.csv")
  for(name in names(files))mpr_require(identical(mp_sha(file.path(run,"calibration",files[[name]])),
    complete[[paste0(name,"_sha256")]]),"Completed parent calibration evidence changed.")
  policy_path <- file.path(run,"calibration/policy.csv")
  mpr_require(identical(mp_sha(policy_path),expected$parent_policy_sha256),"Unexpected original policy.")
  policy <- read.csv(policy_path)
  assessment <- read.csv(file.path(run,"calibration/threshold_assessment.csv"))
  mpr_require(nrow(policy)==nrow(unique(design[c("n","content","tolerance_confidence")])) &&
    !anyDuplicated(mpr_cell_key(policy)) && setequal(mpr_cell_key(policy),mpr_cell_key(design)),
    "Parent policy does not cover the full design.")
  subset <- mpq_followup_design(design,spec)
  mpr_require(setequal(mpr_cell_key(policy[is.na(policy$threshold),]),mpr_cell_key(subset)),
    "Requested follow-up settings do not equal the original unresolved settings.")
  retained <- mpq_followup_retained(policy,assessment,cfg,spec)
  mpq_followup_seed_check(design,cfg,parent_spec)
  records <- pma_file_record(c(config,file.path(run,c("identity.rds","calibration/policy.csv",
    "calibration/threshold_assessment.csv","calibration/completion.json","calibration/checkpoint_manifest.csv",
    "final_study_completion.json","final_exit.json"))))
  list(run=run,source_commit=identity$commit,records=records,retained=retained,
    reassess_design=subset,parent_policy=policy,total_familywise_error=1-cfg$calibration$familywise_confidence)
}

mpq_followup_combined <- function(output,cfg,spec,reference,design) {
  fresh_path <- file.path(output,"calibration")
  fresh <- mpr_read_json(file.path(fresh_path,"completion.json"))
  mpr_require(!is.null(fresh) && isTRUE(fresh$passed) && fresh$expected_cells==2L &&
    fresh$numerical_failure_datasets==0,"Fresh follow-up calibration must pass first.")
  for(name in c("policy","assessment"))mpr_require(identical(
    mp_sha(file.path(fresh_path,if(name=="policy")"policy.csv" else "threshold_assessment.csv")),
    fresh[[paste0(name,"_sha256")]]),"Fresh follow-up evidence changed.")
  policy <- read.csv(file.path(fresh_path,"policy.csv"))
  assessment <- read.csv(file.path(fresh_path,"threshold_assessment.csv"))
  expected <- unique(reference$reassess_design[c("n","content","tolerance_confidence")])
  mpr_require(nrow(policy)==nrow(expected) && !anyDuplicated(mpr_cell_key(policy)) &&
    setequal(mpr_cell_key(policy),mpr_cell_key(expected)) &&
    all(policy$decision=="passed_new_calibration" & is.finite(policy$threshold)),"Unexpected fresh policy settings.")
  cells <- length(unique(mpr_cell_key(assessment))); families <- length(unique(assessment$dgp_id))
  for(key in unique(mpr_cell_key(assessment))) {
    z <- assessment[mpr_cell_key(assessment)==key,]
    alpha <- mpq_calibration_alpha(cfg,spec,cells,families,length(unique(z$threshold)))
    mpr_require(all(abs(z$alpha-alpha)<1e-14) &&
      all(abs(z$lower_bound-mapply(mp_binom_lower,z$successes,z$replications,z$alpha))<1e-12),
      "Fresh follow-up error allocation or bounds changed.")
    choice <- policy$threshold[mpr_cell_key(policy)==key]
    selected <- z[z$threshold==choice,]
    mpr_require(nrow(selected)==families && all(selected$returned==selected$replications &
      selected$lower_bound>=selected$tolerance_confidence),"Fresh selected threshold does not pass.")
  }
  retained <- reference$retained$policy
  combined <- rbind(retained,policy)
  order <- match(mpr_cell_key(reference$parent_policy),mpr_cell_key(combined))
  mpr_require(!anyNA(order) && !anyDuplicated(mpr_cell_key(combined)) &&
    setequal(mpr_cell_key(combined),mpr_cell_key(design)),"Combined policy is incomplete or duplicated.")
  combined <- combined[order,,drop=FALSE]; rownames(combined) <- NULL
  for(i in seq_len(nrow(retained)))mpr_require(identical(
    combined$threshold[mpr_cell_key(combined)==mpr_cell_key(retained[i,])],retained$threshold[i]),
    "A retained threshold changed.")
  target <- file.path(output,"combined"); path <- file.path(target,"calibration")
  done <- mpr_read_json(file.path(path,"completion.json"))
  if(!is.null(done)) {
    mpr_require(identical(done$parent_policy_sha256,spec$followup$parent_policy_sha256) &&
      identical(done$followup_policy_sha256,fresh$policy_sha256) &&
      identical(mp_sha(file.path(path,"policy.csv")),done$policy_sha256) &&
      identical(mp_sha(file.path(path,"threshold_assessment.csv")),done$assessment_sha256) &&
      isTRUE(all.equal(read.csv(file.path(path,"policy.csv")),combined,check.attributes=FALSE)),
      "Completed combined policy or evidence changed.")
    return(target)
  }
  dir.create(path,recursive=TRUE,showWarnings=FALSE)
  all_assessment <- rbind(reference$retained$assessment,assessment)
  result <- mpq_write_calibration(combined,all_assessment,path)
  result$parent_policy_sha256 <- spec$followup$parent_policy_sha256
  result$followup_policy_sha256 <- fresh$policy_sha256
  result$total_familywise_error <- reference$total_familywise_error
  result$original_error_fraction <- spec$followup$original_error_fraction
  result$numerical_failure_datasets <- 0L
  result$interpretation <- "Finite-panel simultaneous bounds combining tightened original and fresh follow-up evidence."
  pta_atomic_write_json(result,file.path(path,"completion.json"))
  pma_write_csv(data.frame(combined[c("n","content","tolerance_confidence")],
    evidence=ifelse(mpr_cell_key(combined) %in% mpr_cell_key(retained),"retained_tightened_original","fresh_followup")),
    file.path(path,"policy_sources.csv"))
  target
}

mpq_followup_run <- function(output,cfg,spec,design,workers,guard,reference) {
  mpr_require(isTRUE(spec$final_study$validation_after_passing_calibration) &&
    !isTRUE(spec$final_study$automatic_adoption),"Unsupported follow-up adoption settings.")
  calibration <- mpq_stage("calibration",output,cfg,spec,reference$reassess_design,workers)
  guard()
  if(!isTRUE(calibration$passed)) {
    status <- list(decision="stopped_unresolved_followup",calibration=calibration,
      retained_settings=nrow(reference$retained$policy),validation_started=FALSE,article_inputs_replaced=FALSE)
  } else {
    combined <- mpq_followup_combined(output,cfg,spec,reference,design)
    guard()
    validation <- mpq_stage("validation",combined,cfg,spec,design,workers)
    guard()
    review <- mpq_export_review(combined,file.path(combined,"review"),cfg,spec,design)
    guard()
    status <- list(decision="complete_requires_scientific_adoption_review",calibration=calibration,
      retained_settings=nrow(reference$retained$policy),combined_run=combined,validation=validation,
      validation_started=TRUE,review=review,article_inputs_replaced=FALSE)
  }
  pta_atomic_write_json(status,file.path(output,"followup_study_completion.json"))
  status
}
