mpq_config <- function(spec) {
  integer_count <- function(x,minimum=1L)is.numeric(x) && length(x)==1L &&
    is.finite(x) && x>=minimum && x==floor(x)
  mpr_require(identical(spec$refinement_rule,"one_target_per_unrepresented_positive_effective_mass_cell") &&
    spec$threshold_rule %in% c("independent_development_quantiles_and_requested_confidence",
      "independent_development_logit_gap_refinement") &&
    integer_count(spec$threshold_quantile_count,2L),"Unsupported grid or threshold specification.")
  if(identical(spec$threshold_rule,"independent_development_logit_gap_refinement"))
    mpr_require(integer_count(spec$threshold_gap_levels) && spec$threshold_gap_levels<=4 &&
      !is.null(spec$followup) && identical(as.numeric(spec$followup$original_error_fraction),.5),
      "Invalid predeclared follow-up menu or error split.")
  for(stage in c("screen","calibration","validation")) {
    x <- spec[[stage]]
    mpr_require(integer_count(x$replications) && integer_count(x$cell_seed_stride) &&
      integer_count(x$seed_base),"Invalid stage size or seed specification.")
  }
  mpr_require(integer_count(spec$calibration$threshold_development_replications) &&
    is.numeric(spec$calibration$familywise_confidence) &&
    length(spec$calibration$familywise_confidence)==1L &&
    is.finite(spec$calibration$familywise_confidence) &&
    spec$calibration$familywise_confidence>0 && spec$calibration$familywise_confidence<1 &&
    integer_count(spec$execution$checkpoint_batch) &&
    setequal(unlist(spec$validation$methods),c(spec$primary_method,
      "mti_profile_fractional_admissible_v1","young_mathew","wilks")),
    "Invalid calibration precision, checkpoint size, or comparison methods.")
  cfg <- jsonlite::read_json(spec$revision_config,simplifyVector=FALSE)
  for(stage in c("screen","calibration","validation"))cfg[[stage]] <- spec[[stage]]
  cfg$calibration$threshold_quantile_count <- spec$threshold_quantile_count
  cfg$execution$checkpoint_batch <- spec$execution$checkpoint_batch
  cfg$retired_validation_seed_base <- spec$retired_validation_seed_base
  cfg
}

mpq_threshold_menu <- function(probabilities,confidence,spec,ceilings=numeric()) {
  mpr_require(is.numeric(confidence) && length(confidence)==1L && is.finite(confidence) &&
    confidence>0 && confidence<1,"Invalid requested posterior confidence.")
  p <- probabilities[is.finite(probabilities) & probabilities>0 & probabilities<1]
  mpr_require(length(p)>0,"No independent development probabilities.")
  knots <- sort(unique(c(confidence,as.numeric(stats::quantile(p,
    seq(0,1,length.out=spec$threshold_quantile_count),type=8,names=FALSE)))))
  if(identical(spec$threshold_rule,"independent_development_logit_gap_refinement")) {
    levels <- spec$threshold_gap_levels
    mpr_require(is.numeric(levels) && length(levels)==1L && is.finite(levels) &&
      levels>=1 && levels<=4 && levels==floor(levels),"Invalid threshold gap resolution.")
    mpr_require(length(ceilings)>0 && all(is.finite(ceilings) & ceilings>=0 & ceilings<=1),
      "Development eligibility probabilities are missing or invalid.")
    ceiling <- min(ceilings)
    if(ceiling>0 && ceiling<1)knots <- sort(unique(c(knots,ceiling)))
    fractions <- seq_len(2^levels-1L)/2^levels
    logits <- stats::qlogis(knots)
    extra <- unlist(lapply(seq_len(length(knots)-1L),function(i)
      stats::plogis(logits[i]+fractions*(logits[i+1L]-logits[i]))),use.names=FALSE)
    knots <- sort(unique(c(knots,extra)))
    maximum <- (spec$threshold_quantile_count+1L)*2^levels+1L
    mpr_require(length(knots)<=maximum && all(knots>0 & knots<1),"Invalid refined threshold menu.")
  }
  knots
}

mpq_calibration_alpha <- function(cfg,spec,cells,families,menu_size) {
  error <- 1-cfg$calibration$familywise_confidence
  if(identical(spec$threshold_rule,"independent_development_logit_gap_refinement")) {
    fraction <- spec$followup$original_error_fraction
    mpr_require(is.numeric(fraction) && length(fraction)==1L &&
      identical(as.numeric(fraction),.5),"Follow-up requires the fixed equal error split.")
    error <- error*(1-fraction)
    return(error/(cells*families*menu_size))
  }
  error/(cells*families*(spec$threshold_quantile_count+1L))
}

mpq_tasks <- function(design,cfg,stage) {
  settings <- cfg[[stage]]
  dev <- if(stage=="calibration")settings$threshold_development_replications else 0L
  do.call(rbind,lapply(seq_len(nrow(design)),function(i) {
    z <- design[i,,drop=FALSE]; rep <- seq_len(settings$replications+dev)
    data.frame(z[rep(1L,length(rep)),,drop=FALSE],replication=rep,
      role=if(stage=="screen")"numerical_screen" else if(stage=="validation")"independent_validation" else
        ifelse(rep<=dev,"threshold_development","calibration_assessment"),
      seed=settings$seed_base+z$cell_index*settings$cell_seed_stride+rep)
  }))
}

mpq_seed_check <- function(design,cfg,old_cfg) {
  seeds <- lapply(c("screen","calibration","validation"),function(s)mpq_tasks(design,cfg,s)$seed)
  previous <- lapply(c("calibration","validation"),function(s)mpq_tasks(design,old_cfg,s)$seed)
  if(!is.null(cfg$retired_validation_seed_base)) {
    retired <- cfg
    retired$validation$seed_base <- cfg$retired_validation_seed_base
    previous[[length(previous)+1L]] <- mpq_tasks(design,retired,"validation")$seed
  }
  combined <- unlist(seeds,use.names=FALSE)
  mpr_require(!anyDuplicated(combined) && !any(combined %in% unlist(previous)) &&
    all(is.finite(combined) & combined>0 & combined<.Machine$integer.max),
    "New data-role seeds overlap or exceed R's seed range.")
  for(s in c("screen","calibration","validation")) {
    x <- cfg[[s]]
    count <- x$replications+if(s=="calibration")x$threshold_development_replications else 0L
    mpr_require(x$cell_seed_stride>count,"Seed stride does not separate cases.")
  }
  invisible(TRUE)
}

mpq_baseline_check <- function(run,spec,complete=FALSE) {
  run <- normalizePath(run,mustWork=TRUE)
  root <- normalizePath(file.path(run,"..","..","..",".."),mustWork=TRUE)
  identity <- readRDS(file.path(run,"identity.rds"))
  mpr_require(identical(identity$commit,spec$baseline_commit) &&
    identical(identity$policy_sha256,spec$baseline_policy_sha256) &&
    identical(mp_sha(spec$baseline_policy),spec$baseline_policy_sha256),"Unexpected frozen baseline.")
  for(i in seq_len(nrow(identity$sources))) {
    s <- identity$sources[i,]
    mpr_require(identical(mp_sha(file.path(root,s$path)),s$sha256),"Frozen baseline source changed.")
  }
  for(i in seq_len(nrow(identity$protected_inputs))) {
    s <- identity$protected_inputs[i,]
    mpr_require(identical(mp_sha(file.path(root,s$path)),s$sha256),"Frozen baseline article inputs changed.")
  }
  # This branch adds a grid API; the numerical criterion and solver stay identical.
  for(path in c("application/R/rqr_mti_intercept_profile.R",
                "application/R/rqr_mti_profile_selection.R",
                "application/src/rqr_mti_intercept_profile.cpp"))
    mpr_require(identical(mp_sha(path),mp_sha(file.path(root,path))),"Fixed-target solver changed.")
  if(complete) {
    receipt <- mpr_read_json(file.path(run,"evaluation.json"))
    exit <- mpr_read_json(file.path(run,"launcher_exit.json"))
    mpr_require(!mpr_active(run) && !is.null(receipt) && !is.null(exit) && exit$exit_status==0 &&
      receipt$expected_datasets==identity$expected_datasets &&
      receipt$method_evaluations==identity$expected_datasets*length(identity$methods),
      "The complete baseline comparison and report are required first.")
  }
  identity
}

mpq_context <- function(cfg) {
  profile <- mp_frozen_profile(cfg)
  generators <- mp_dgp_environment()
  meta <- setNames(profile$validation$dgps,
    vapply(profile$validation$dgps,`[[`,character(1),"dgp_id"))
  list(profile=profile,laws=lapply(meta,generators$dgp_meta),
    scan=read.csv("tables/tolerance_validation_article_scan_calibration.csv"))
}

mpq_fit <- function(y,task,cfg,context=NULL) {
  profile <- if(is.null(context))mp_frozen_profile(cfg) else context$profile
  scan_q <- if(is.null(context))mp_scan_fraction(task$n,task$content,task$tolerance_confidence) else {
    s <- context$scan
    s <- s[mpr_cell_key(s)==mpr_cell_key(task),,drop=FALSE]
    mpr_require(nrow(s)==1L,"No unique cached scan-reference cell.")
    s$retained_count/task$n
  }
  menu <- mp_menu(y,task$content,task$tolerance_confidence,profile,
    scan_q)
  grid <- rqrgibbs::rqr_mti_refine_content_grid(menu$q_grid,length(y),task$content,
    learning_rate=cfg$profile$learning_rate,prior_precision=cfg$profile$prior_precision)
  fit <- mp_profile(y,grid$q_grid,task$tolerance_confidence,cfg,content=task$content)
  fit$candidates$from_original_grid <- fit$candidates$target_content %in% grid$original_q_grid
  list(candidates=fit$candidates,baseline=fit$candidates[fit$candidates$from_original_grid,],grid=grid)
}

mpq_interval <- function(task,method,candidates,threshold,law,grid) {
  s <- mp_select(candidates,threshold)
  returned <- nrow(s)==1L
  value <- function(name)if(returned)s[[name]][1L] else NA_real_
  content <- if(returned)law$p(s$upper)-law$p(s$lower) else NA_real_
  data.frame(task,method=method,returned=returned,
    status=if(any(!candidates$profile_converged))"candidate_numerical_failure" else
      if(returned)"returned" else "no_eligible_interval",
    lower=value("lower"),upper=value("upper"),width=value("width"),true_content=content,
    content_attained=returned && content>=task$content,selected_q=value("target_content"),
    selected_tilt=value("mean_tilt"),posterior_content_probability=value("posterior_content_probability"),
    relative_objective_gap=value("relative_objective_gap"),
    selected_objective=value("objective"),selected_iterations=value("iterations"),
    selected_observed_count=value("observed_count"),
    selected_midpoint=if(returned)(s$lower+s$upper)/2 else NA_real_,
    selected_half_width=if(returned)(s$upper-s$lower)/2 else NA_real_,
    numerical_failure_candidates=sum(!candidates$profile_converged),candidate_count=nrow(candidates),
    original_q_count=length(grid$original_q_grid),added_q_count=length(grid$added_q),
    posterior_threshold=threshold)
}

mpq_one <- function(task,stage,cfg,spec,baseline_policy,new_policy=NULL,context=NULL) {
  if(is.null(context))context <- mpq_context(cfg)
  set.seed(task$seed)
  law <- context$laws[[task$dgp_id]]
  mpr_require(!is.null(law),"Unknown simulation family.")
  y <- law$r(task$n)
  fit <- mpq_fit(y,task,cfg,context)
  if(stage=="calibration") {
    z <- fit$candidates; valid <- z$profile_converged
    z$true_content <- NA_real_
    z$true_content[valid] <- law$p(z$upper[valid])-law$p(z$lower[valid])
    return(list(identity=task,candidates=z))
  }
  key <- mpr_cell_key(task)
  old <- baseline_policy[mpr_cell_key(baseline_policy)==key,]
  mpr_require(nrow(old)==1 && is.finite(old$threshold),"Missing baseline comparison threshold.")
  threshold <- old$threshold
  if(stage=="validation") {
    choice <- new_policy[mpr_cell_key(new_policy)==key,]
    mpr_require(nrow(choice)==1 && is.finite(choice$threshold),"Missing fresh calibrated threshold.")
    threshold <- choice$threshold
  }
  rows <- list(mpq_interval(task,spec$primary_method,fit$candidates,threshold,law,fit$grid),
    mpq_interval(task,"mti_profile_fractional_admissible_v1",fit$baseline,old$threshold,law,fit$grid))
  if(stage=="validation") {
    ym <- pta_fit_young_mathew(y,task$content,task$tolerance_confidence)
    add <- function(method,lower,upper,returned) {
      z <- rows[[1]]; z$method <- method; z$returned <- returned
      z$status <- if(returned)"returned" else "benchmark_failure"
      z$lower <- lower; z$upper <- upper; z$width <- if(returned)upper-lower else NA_real_
      z$true_content <- if(returned)law$p(upper)-law$p(lower) else NA_real_
      z$content_attained <- returned && z$true_content>=task$content
      for(field in c("selected_q","selected_tilt","posterior_content_probability",
        "relative_objective_gap","candidate_count","posterior_threshold","selected_objective",
        "selected_iterations","selected_observed_count"))z[[field]] <- NA_real_
      z$selected_midpoint <- if(returned)(lower+upper)/2 else NA_real_
      z$selected_half_width <- if(returned)(upper-lower)/2 else NA_real_
      z$numerical_failure_candidates <- 0L
      z
    }
    rows[[3]] <- add("young_mathew",ym$lower,ym$upper,isTRUE(ym$returned))
    rows[[4]] <- add("wilks",min(y),max(y),TRUE)
  }
  do.call(rbind,rows)
}

mpq_screen_assessment <- function(z,spec) {
  a <- z[z$method==spec$primary_method,]
  b <- z[z$method=="mti_profile_fractional_admissible_v1",]
  b <- b[match(mpr_task_key(a),mpr_task_key(b)),]
  mpr_require(nrow(a)==nrow(b) && !anyNA(b$seed) && all(a$seed==b$seed),"Screen pairs do not match.")
  jointly <- a$returned & b$returned
  epsilon <- 1000*.Machine$double.eps*(1+abs(a$lower)+abs(a$upper)+abs(b$lower)+abs(b$upper))
  nested <- !jointly | a$width<=b$width+epsilon
  failures <- sum(a$numerical_failure_candidates>0)
  list(passed=failures==0 && all(nested) && all(a$returned),
    completed_datasets=nrow(a),numerical_failure_datasets=failures,
    nonreturned=sum(!a$returned),nested_search_checks_passed=all(nested),
    strictly_shorter_test_intervals=sum(jointly & a$width<b$width-epsilon,na.rm=TRUE),
    target_additions=sum(a$added_q_count),
    tolerance_validity_assessed=FALSE,threshold_reused_for_diagnosis_only=TRUE)
}

mpq_calibrate <- function(values,cfg,spec,output) {
  tasks <- do.call(rbind,lapply(values,`[[`,"identity"))
  cells <- unique(tasks[c("n","content","tolerance_confidence")])
  families <- unique(tasks$dgp_id)
  policies <- assessments <- list()
  for(i in seq_len(nrow(cells))) {
    cell <- cells[i,]; use <- which(mpr_cell_key(tasks)==mpr_cell_key(cell))
    dev <- use[tasks$role[use]=="threshold_development"]
    p <- unlist(lapply(values[dev],function(x)x$candidates$posterior_content_probability),use.names=FALSE)
    p <- p[is.finite(p) & p>0 & p<1]
    ceilings <- vapply(values[dev],function(x)max(x$candidates$posterior_content_probability),numeric(1))
    thresholds <- mpq_threshold_menu(p,cell$tolerance_confidence,spec,ceilings)
    alpha <- mpq_calibration_alpha(cfg,spec,nrow(cells),length(families),length(thresholds))
    rows <- lapply(families,function(family) {
      idx <- use[tasks$role[use]=="calibration_assessment" & tasks$dgp_id[use]==family]
      mpr_require(length(idx)==cfg$calibration$replications,"Incomplete calibration family.")
      decisions <- lapply(values[idx],function(x)mpr_candidate_decisions(x$candidates,thresholds,cell$content))
      totals <- lapply(c("returned","successes","width_sum"),function(field)
        Reduce(`+`,lapply(decisions,`[[`,field)))
      data.frame(cell[rep(1L,length(thresholds)),,drop=FALSE],dgp_id=family,threshold=thresholds,
        replications=length(idx),returned=totals[[1]],successes=totals[[2]],
        content_attainment=totals[[2]]/length(idx),
        lower_bound=vapply(totals[[2]],function(k)mp_binom_lower(k,length(idx),alpha),numeric(1)),
        mean_width=ifelse(totals[[1]]>0,totals[[3]]/pmax(1,totals[[1]]),NA_real_),alpha=alpha)
    })
    a <- do.call(rbind,rows)
    passing <- vapply(thresholds,function(t) {
      s <- a[a$threshold==t,]
      all(s$returned==s$replications & s$lower_bound>=cell$tolerance_confidence)
    },logical(1))
    policies[[i]] <- data.frame(cell,threshold=if(any(passing))min(thresholds[passing]) else NA_real_,
      method_id=spec$primary_method,decision=if(any(passing))"passed_new_calibration" else "unresolved")
    assessments[[i]] <- a
  }
  mpq_write_calibration(do.call(rbind,policies),do.call(rbind,assessments),output)
}

mpq_write_calibration <- function(policy,assessment,output) {
  pma_write_csv(policy,file.path(output,"policy.csv"))
  pma_write_csv(assessment,file.path(output,"threshold_assessment.csv"))
  list(passed=all(is.finite(policy$threshold)),calibrated_cells=sum(is.finite(policy$threshold)),
    expected_cells=nrow(policy),policy_sha256=mp_sha(file.path(output,"policy.csv")),
    assessment_sha256=mp_sha(file.path(output,"threshold_assessment.csv")),
    interpretation="Finite-panel calibration; inconclusive cells remain unresolved.")
}

mpq_stage <- function(stage,output,cfg,spec,design,workers) {
  tasks <- mpq_tasks(design,cfg,stage)
  context <- mpq_context(cfg)
  baseline <- read.csv(spec$baseline_policy)
  new_policy <- NULL
  policy_hash <- NULL
  if(stage=="validation") {
    completion <- mpr_read_json(file.path(output,"calibration","completion.json"))
    mpr_require(!is.null(completion) && isTRUE(completion$passed),"New calibration is unresolved or absent.")
    policy_path <- file.path(output,"calibration","policy.csv")
    policy_hash <- mp_sha(policy_path)
    mpr_require(identical(completion$policy_sha256,policy_hash),"New calibration policy hash changed.")
    new_policy <- read.csv(policy_path)
    expected_cells <- unique(design[c("n","content","tolerance_confidence")])
    mpr_require(all(c("n","content","tolerance_confidence","threshold","method_id","decision") %in% names(new_policy)) &&
      nrow(new_policy)==nrow(expected_cells) && !anyDuplicated(mpr_cell_key(new_policy)) &&
      setequal(mpr_cell_key(new_policy),mpr_cell_key(expected_cells)) &&
      all(new_policy$method_id==spec$primary_method) &&
      all(new_policy$decision=="passed_new_calibration"),"New policy identity or design cells are invalid.")
    mpr_require(all(is.finite(new_policy$threshold) & new_policy$threshold>0 & new_policy$threshold<1),
      "New calibrated policy has unresolved or invalid cells.")
    stage_identity <- file.path(output,"validation_policy_identity.rds")
    if(file.exists(stage_identity))mpr_require(identical(readRDS(stage_identity),policy_hash),
      "Validation policy changed; existing checkpoints cannot be reused.") else mp_atomic_rds(policy_hash,stage_identity)
  }
  path <- file.path(output,stage); dir.create(path,recursive=TRUE,showWarnings=FALSE)
  old <- mpr_read_json(file.path(path,"completion.json"))
  if(!is.null(old)) {
    if(stage=="calibration") {
      files <- c(policy="policy.csv",assessment="threshold_assessment.csv",
        checkpoint_manifest="checkpoint_manifest.csv")
      for(name in names(files)) {
        hash <- old[[paste0(name,"_sha256")]]
        if(!is.null(hash))mpr_require(identical(hash,mp_sha(file.path(path,files[[name]]))),
          "Completed calibration evidence changed.")
      }
    }
    if(stage=="validation")mpr_require(identical(old$frozen_policy_sha256,policy_hash),"Completed validation policy changed.")
    if(!is.null(old$results_sha256))mpr_require(identical(old$results_sha256,mp_sha(file.path(path,"results.csv"))),
      "Completed stage results changed.")
    return(old)
  }
  values <- mp_checkpoint_jobs(tasks,function(task)mpq_one(task,stage,cfg,spec,baseline,new_policy,context),
    file.path(path,"checkpoints"),workers,spec$execution$checkpoint_batch,collect=stage!="calibration",
    stop_on_task_error=TRUE)
  if(stage=="calibration") {
    result <- mpq_calibrate_checkpoints(tasks,cfg,spec,path,context)
  } else {
    errors <- vapply(values,function(x)is.list(x) && !is.data.frame(x) && !is.null(x$error),logical(1))
    if(any(errors)) {
      mp_atomic_rds(values[errors],file.path(path,"errors.rds"))
      stop("Grid study has task errors; records retained, no samples replaced.")
    }
    z <- do.call(rbind,values)
    pma_write_csv(z,file.path(path,"results.csv"))
    if(stage=="screen")result <- mpq_screen_assessment(z,spec) else {
      generators <- mp_dgp_environment()
      frozen <- mp_frozen_profile(cfg)$validation
      meta <- setNames(frozen$dgps,vapply(frozen$dgps,`[[`,character(1),"dgp_id"))
      mpr_validate_results(z,design,cfg,lapply(meta,function(x)generators$dgp_meta(x)$p),
        primary_method=spec$primary_method)
      selected <- z[z$method==spec$primary_method & z$returned,,drop=FALSE]
      expected <- new_policy$threshold[match(mpr_cell_key(selected),mpr_cell_key(new_policy))]
      mpr_require(all(is.finite(expected) & selected$posterior_threshold==expected &
        selected$posterior_content_probability>=expected & selected$selected_q>selected$content &
        selected$selected_q<1 & is.finite(selected$selected_tilt)),
        "Returned validation settings disagree with the frozen calibrated rule.")
      review <- list(primary_method=spec$primary_method)
      summary <- mpr_validation_summary(z,design,cfg,review)
      pma_write_csv(summary,file.path(path,"family_case_summary.csv"))
      pma_write_csv(mpr_paired_summary(z,review),file.path(path,"paired_comparisons.csv"))
      mpr_report_pdf(path,list(decision="independent_grid_comparison_requires_scientific_review"),summary=summary,
        method_labels=c(mti_profile_mass_cells_v1="Refined-grid MTI",
          mti_profile_fractional_admissible_v1="Frozen-grid MTI"),
        method_colors=c(mti_profile_mass_cells_v1="#009E73"))
      mpr_require(identical(mp_sha(policy_path),policy_hash),"New policy changed during validation.")
      result <- list(passed=TRUE,method_evaluations=nrow(z),article_results_replaced=FALSE,
        frozen_policy_sha256=policy_hash,nonreturned=sum(!z$returned),
        primary_numerical_failure_datasets=sum(z$method==spec$primary_method & z$numerical_failure_candidates>0))
    }
    result$results_sha256 <- mp_sha(file.path(path,"results.csv"))
  }
  result$completed_datasets <- nrow(tasks)
  result$completed_utc <- format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC")
  mpq_stage_report(stage,result,path)
  pta_atomic_write_json(result,file.path(path,"completion.json"))
  result
}

mpq_stage_report <- function(stage,result,path) {
  lines <- c(paste("# MTI Content-Grid",tools::toTitleCase(stage)),"",
    sprintf("Completed datasets: %d. Stage assessment: %s.",result$completed_datasets,
      if(isTRUE(result$passed))"passed" else "unresolved"),"")
  if(stage=="screen")lines <- c(lines,
    sprintf("Numerical failures: %d; nonreturns: %d; shorter paired test intervals: %d.",
      result$numerical_failure_datasets,result$nonreturned,result$strictly_shorter_test_intervals),
    "The frozen threshold is used only to diagnose the change in numerical search.",
    "One draw per distribution and design case provides no tolerance-performance evidence.",
    "A search improvement still requires fresh calibration and independent validation.")
  if(stage=="calibration")lines <- c(lines,
    sprintf("Resolved design cells: %d of %d; datasets with numerical failures: %d.",
      result$calibrated_cells,result$expected_cells,result$numerical_failure_datasets),
    "Thresholds come only from independent development samples and the requested confidence.",
    "Assessment uses simultaneous one-sided exact binomial bounds across all declared comparisons.",
    "An unresolved cell cannot authorize validation. No replication budget or criterion was changed.",
    "Passing this finite distribution panel does not establish a distribution-free tolerance theorem.")
  if(stage=="validation")lines <- c(lines,
    sprintf("Paired method evaluations: %d; nonreturns: %d.",result$method_evaluations,result$nonreturned),
    "Completion records numerical execution, not a statistical acceptance decision.",
    "Review family_case_summary.csv and paired_comparisons.csv alongside mti_profile_assessment.pdf.",
    "Width ranges are empirical 2.5% and 97.5% quantiles within each family and design case.",
    "Binomial intervals describe uncertainty in unconditional content attainment across samples.")
  writeLines(c(lines,"","No manuscript, application, or historical result has been replaced."),
    file.path(path,"REPORT.md"),useBytes=TRUE)
}

mpq_precision_reference <- function(design,cfg,spec) {
  rows <- list()
  for(cell in split(unique(design[c("n","content","tolerance_confidence")]),
    mpr_cell_key(unique(design[c("n","content","tolerance_confidence")])))) {
    probabilities <- seq(cell$tolerance_confidence,1,length.out=spec$threshold_quantile_count)
    menu_size <- if(identical(spec$threshold_rule,"independent_development_logit_gap_refinement"))
      (spec$threshold_quantile_count+1L)*2^spec$threshold_gap_levels+1L else
      spec$threshold_quantile_count+1L
    alpha <- if(identical(spec$threshold_rule,"independent_development_logit_gap_refinement"))
      mpq_calibration_alpha(cfg,spec,
        nrow(unique(design[c("n","content","tolerance_confidence")])),
        length(unique(design$dgp_id)),menu_size) else
      (1-cfg$calibration$familywise_confidence)/(nrow(design)*(spec$threshold_quantile_count+1L))
    budgets <- if(is.null(spec$precision_replications))cfg$calibration$replications*2^(0:4) else
      unlist(spec$precision_replications,use.names=FALSE)
    for(n in budgets) {
      required <- stats::qbinom(1-alpha,n,cell$tolerance_confidence)+1
      rows[[length(rows)+1L]] <- data.frame(cell[rep(1L,length(probabilities)),,drop=FALSE],
        replications=n,hypothetical_attainment=probabilities,alpha=alpha,required_successes=required,
        probability_of_passing=if(required>n)0 else stats::pbinom(required-1,n,probabilities,lower.tail=FALSE),
        reference_only=TRUE)
    }
  }
  do.call(rbind,rows)
}
