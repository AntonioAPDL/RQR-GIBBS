mp_sha <- function(path) digest::digest(file = path, algo = "sha256", serialize = FALSE)

mp_atomic_rds <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile("checkpoint_", tmpdir = dirname(path))
  saveRDS(x, temporary)
  if (!file.rename(temporary, path)) stop("Could not save checkpoint: ", path)
}

mp_profile <- function(y, q_grid, threshold, cfg, historical = FALSE, traces = FALSE,
                       content = .9) {
  p <- cfg$profile
  rqrgibbs::rqr_mti_dp_profile(y, content = content, posterior_confidence = threshold,
    q_grid = q_grid, tilt_rule = if (historical) "historical" else "fractional_admissible",
    allocation = p$allocation, tilt_positions = seq(0,1,length.out=p$tilt_position_count),
    historical_tilt_control = list(tilt_offsets_sd=c(-.45,-.30,-.15,0,.15,.30,.45),
      include_zero_tilt=TRUE,max_abs_tilt_sd=2),
    learning_rate=p$learning_rate,prior_precision=p$prior_precision,prior_mean=p$prior_mean,
    backend=p$backend,control=list(objective_tolerance=p$objective_tolerance),
    return_candidate_traces=traces)
}

mp_inputs <- function(cfg) {
  app <- jsonlite::read_json(cfg$application_config, simplifyVector=TRUE)
  if (mp_sha(file.path(cfg$application_evidence,"results.csv")) !=
      cfg$expected_application_results_sha256) stop("Historical application hash mismatch.")
  list(app=app,
    observed=read.csv(file.path(cfg$application_evidence,"observed_batches.csv")),
    membership=read.csv(gzfile(file.path(cfg$application_evidence,"split_membership.csv.gz"))),
    cases=read.csv(file.path(cfg$diagnostic_evidence,"selected_cases.csv")),
    candidates=read.csv(file.path(cfg$diagnostic_evidence,"candidates.csv")),
    references=read.csv(file.path(cfg$diagnostic_evidence,"references.csv")),
    results=read.csv(file.path(cfg$application_evidence,"results.csv")))
}

mp_sample <- function(r, inputs) {
  s <- inputs$membership[inputs$membership$split_scheme==r$split_scheme &
    inputs$membership$split_id==r$split_id & inputs$membership$training,]
  inputs$observed[[r$response_id]][match(s$analysis_row_id,inputs$observed$.analysis_row)]
}

mp_frozen_profile <- function(cfg) {
  v <- jsonlite::read_json(cfg$validation_config,simplifyVector=FALSE)
  list(config=v$engine_defaults$mti_ecm_dp_profile,
       direct_dp=v$engine_defaults$direct_dp,
       policy=read.csv(cfg$historical_policy),validation=v)
}

mp_policy <- function(profile,n,c,p) {
  z <- profile$policy
  z <- z[z$n==n & abs(z$content-c)<1e-12 & abs(z$tolerance_confidence-p)<1e-12,]
  if(nrow(z)!=1L) stop("No unique historical calibration cell.")
  as.list(z)
}

mp_menu <- function(y,c,p,profile,scan_q) {
  rqrgibbs::rqr_mti_ecm_adaptive_profile_menu(y,c,p,scan_q,
    policy="cell",policy_config=profile$config,
    calibration_rule=mp_policy(profile,length(y),c,p))
}

mp_ecm_fit <- function(y,q,delta,init,profile,trace=TRUE) {
  controls <- profile$config$ecm_control
  controls$store_iteration_trace <- trace
  rqrgibbs::rqr_ecm_fit(y,matrix(1,length(y),1),q,
    learning_rate=profile$config$learning_rate,mean_tilt=delta,
    beta_prior_obj=rqrgibbs::beta_prior("ridge",ridge=list(tau2=profile$config$beta_ridge_tau2)),
    ecm_control=controls,init=list(beta1=init[1],beta2=init[2]))
}

mp_fixed_row <- function(fit,variant,case_id,q,delta) {
  if (inherits(fit,"rqr_mti_intercept_fit")) {
    l <- fit$lower; u <- fit$upper; trace <- fit$trace
    converged <- fit$converged; gap <- fit$relative_objective_gap
  } else {
    interval <- rqrgibbs::predict_interval(fit,matrix(1,1,1))
    l <- interval$lower[1]; u <- interval$upper[1]
    trace <- fit$objective_trace; converged <- fit$converged; gap <- NA_real_
  }
  list(row=data.frame(case_id=case_id,variant=variant,q=q,delta=delta,
    lower=l,upper=u,width=u-l,objective=fit$objective,
    converged=converged,relative_objective_gap=gap,iterations=fit$iterations),trace=trace)
}

mp_mechanism <- function(cfg, output, workers) {
  inputs <- mp_inputs(cfg); profile <- mp_frozen_profile(cfg)
  old_tilt <- getFromNamespace(".rqr_mti_profile_tilt_values","rqrgibbs")
  cases <- lapply(cfg$observed_case_ids,function(id) {
    z <- inputs$cases[inputs$cases$case_id==id,]
    if(nrow(z)!=1) stop("Missing prespecified case.")
    list(case_id=id,y=mp_sample(z,inputs),q=z$mti_selected_q,delta=z$mti_selected_tilt,
      expected_lower=z$lower,expected_upper=z$upper)
  })
  for(z in cfg$synthetic_cases) {
    set.seed(z$seed)
    y <- if(z$family=="normal") rnorm(z$n) else rexp(z$n)
    cases[[length(cases)+1L]] <- list(case_id=z$case_id,y=y,q=z$q,
      delta=old_tilt(y,z$q)$central_tilt)
  }
  rows <- list(); traces <- list(); menus <- list()
  for(case in cases) {
    y <- case$y; q <- case$q
    old <- old_tilt(y,q)
    init <- old$endpoints+(case$delta-old$central_tilt)
    coherent <- rqrgibbs::rqr_mti_fractional_window(y,q,cfg$profile$allocation)
    a <- mp_ecm_fit(y,q,case$delta,init,profile)
    if(!is.null(case$expected_lower)) {
      interval <- rqrgibbs::predict_interval(a,matrix(1,1,1))
      if(max(abs(c(interval$lower,interval$upper)-c(case$expected_lower,case$expected_upper)))>1e-8)
        stop("Historical endpoint replay failed: ",case$case_id)
    }
    fits <- list(A=mp_fixed_row(a,"A",case$case_id,q,case$delta),
      B=mp_fixed_row(rqrgibbs::rqr_mti_intercept_fit(y,q,case$delta,
        control=list(store_trace=TRUE)),"B",case$case_id,q,case$delta),
      C=mp_fixed_row(mp_ecm_fit(y,q,coherent$delta_raw,init,profile),
        "C",case$case_id,q,coherent$delta_raw),
      D=mp_fixed_row(rqrgibbs::rqr_mti_intercept_fit(y,q,coherent$delta_raw,
        control=list(store_trace=TRUE)),"D",case$case_id,q,coherent$delta_raw))
    for(name in names(fits)) {
      key <- paste(case$case_id,name,sep="_")
      rows[[key]] <- fits[[name]]$row
      trace <- fits[[name]]$trace
      trace$case_id <- case$case_id; trace$variant <- name
      traces[[key]] <- trace
    }
    grid <- mp_menu(y,.9,.95,profile,.98)$q_grid
    menu <- mp_profile(y,grid,.995,cfg)
    if(any(!menu$candidates$profile_converged)) {
      pma_write_csv(menu$candidates,file.path(output,paste0("failed_menu_",case$case_id,".csv")))
      stop("Revised menu numerical failure: ",case$case_id)
    }
    z <- menu$candidates; z$case_id <- case$case_id; menus[[case$case_id]] <- z
  }
  pma_write_csv(do.call(rbind,rows),file.path(output,"mechanism.csv"))
  for(key in names(traces)) pma_write_csv(traces[[key]],
    file.path(output,paste0("trace_",key,".csv")))
  pma_write_csv(do.call(rbind,menus),file.path(output,"mechanism_menus.csv"))

  one <- function(id) {
    r <- inputs$cases[inputs$cases$case_id==id,]; y <- mp_sample(r,inputs)
    old <- inputs$candidates[inputs$candidates$case_id==id,]
    refs <- inputs$references[inputs$references$case_id==id,]
    rows <- lapply(seq_len(nrow(old)),function(i) {
      z <- old[i,]
      fit <- rqrgibbs::rqr_mti_intercept_fit(y,z$target_content,z$mean_tilt)
      ref <- refs[refs$candidate_index==z$candidate_index,]
      data.frame(case_id=id,candidate_index=z$candidate_index,
        q=z$target_content,delta=z$mean_tilt,lower=fit$lower,upper=fit$upper,
        objective=fit$objective,reference_objective=ref$objective,
        objective_difference=fit$objective-ref$objective,
        relative_objective_gap=fit$relative_objective_gap,converged=fit$converged)
    })
    grid <- sort(unique(old$target_content))
    menu <- mp_profile(y,grid,.995,cfg)
    if(any(!menu$candidates$profile_converged)) {
      pma_write_csv(menu$candidates,file.path(output,paste0("failed_menu_",id,".csv")))
      stop("Revised menu failed: ",id)
    }
    list(reference=do.call(rbind,rows),menu=transform(menu$candidates,case_id=id))
  }
  ids <- inputs$cases$case_id
  answers <- if(workers>1) parallel::mclapply(ids,one,mc.cores=min(workers,length(ids)),
    mc.set.seed=FALSE) else lapply(ids,one)
  if(any(vapply(answers,inherits,logical(1),"try-error"))) stop("A reference task failed.")
  references <- do.call(rbind,lapply(answers,`[[`,"reference"))
  pma_write_csv(references,file.path(output,"all_candidate_reference_checks.csv"))
  pma_write_csv(do.call(rbind,lapply(answers,`[[`,"menu")),
    file.path(output,"all_diagnostic_revised_menus.csv"))
  valid <- all(abs(references$objective_difference)<=
    cfg$profile$objective_tolerance*(1+abs(references$reference_objective)))
  if(!valid) stop("Full-menu objective references disagree.")
  list(passed=TRUE,cases=length(cases),reference_targets=nrow(references),
       maximum_absolute_reference_difference=max(abs(references$objective_difference)))
}

mp_dgp_environment <- function(path="application/scripts/69_validate_rqr_bayes_uq.R") {
  e <- new.env(parent=baseenv())
  e$`%||%` <- function(x,y) if(is.null(x)) y else x
  e$stopf <- function(...) stop(paste0(...),call.=FALSE)
  wanted <- c("standardized_asymmetric_laplace_meta","standardized_two_piece_normal_meta","dgp_meta")
  for(expr in parse(path)) {
    if(is.call(expr) && identical(expr[[1]],as.name("<-")) &&
        is.symbol(expr[[2]]) && as.character(expr[[2]]) %in% wanted) eval(expr,e)
  }
  if(!all(vapply(wanted,exists,logical(1),envir=e,inherits=FALSE))) stop("Missing DGP definitions.")
  e
}

mp_scan_fraction <- function(n,c,p) {
  z <- read.csv("tables/tolerance_validation_article_scan_calibration.csv")
  row <- z[z$n==n & abs(z$content-c)<1e-12 &
    abs(z$tolerance_confidence-p)<1e-12,]
  if(nrow(row)!=1L) stop("No unique frozen scan-reference cell.")
  as.numeric(row$retained_count/n)
}

mp_select <- function(candidates,threshold) {
  z <- candidates[!is.na(candidates$posterior_content_probability) &
    candidates$profile_converged & candidates$posterior_content_probability>=threshold,]
  if(!nrow(z) || any(!candidates$profile_converged)) return(z[0,,drop=FALSE])
  z <- z[order(z$width,z$target_content,abs(z$mean_tilt-z$central_tilt),
    z$lower,z$candidate_index),]
  head(z,1)
}

mp_binom_lower <- function(successes,trials,alpha) {
  if(successes==0) 0 else stats::qbeta(alpha,successes,trials-successes+1)
}

mp_calibration_policy <- function(values,cfg,output) {
  first <- do.call(rbind,lapply(values,function(x)x$identity))
  cells <- unique(first[c("n","content","tolerance_confidence")])
  families <- unique(first$dgp_id)
  max_thresholds <- cfg$calibration$threshold_quantile_count+2L
  alpha <- (1-cfg$calibration$familywise_confidence)/(nrow(cells)*length(families)*max_thresholds)
  diagnostics <- list(); policies <- list()
  for(i in seq_len(nrow(cells))) {
    cell <- cells[i,]
    use <- which(first$n==cell$n & abs(first$content-cell$content)<1e-12 &
      abs(first$tolerance_confidence-cell$tolerance_confidence)<1e-12)
    development <- use[first$role[use]=="threshold_development"]
    assessment <- use[first$role[use]=="calibration_assessment"]
    probabilities <- unlist(lapply(values[development],function(x)
      x$candidates$posterior_content_probability),use.names=FALSE)
    probabilities <- probabilities[is.finite(probabilities) & probabilities>0 & probabilities<1]
    if(!length(probabilities)) stop("No development probabilities.")
    thresholds <- sort(unique(c(cell$tolerance_confidence,.995,
      as.numeric(quantile(probabilities,seq(0,1,length.out=cfg$calibration$threshold_quantile_count),
        names=FALSE,type=8)))))
    thresholds <- thresholds[thresholds>0 & thresholds<1]
    summaries <- list()
    for(threshold in thresholds) for(family in families) {
      idx <- assessment[first$dgp_id[assessment]==family]
      selected <- lapply(values[idx],function(x)mp_select(x$candidates,threshold))
      returned <- vapply(selected,nrow,integer(1))==1L
      success <- vapply(selected,function(z) nrow(z)==1L && z$true_content>=cell$content,logical(1))
      widths <- vapply(selected,function(z) if(nrow(z))z$width else NA_real_,numeric(1))
      summaries[[length(summaries)+1L]] <- data.frame(n=cell$n,content=cell$content,
        tolerance_confidence=cell$tolerance_confidence,dgp_id=family,threshold=threshold,
        replications=length(idx),returned=sum(returned),successes=sum(success),
        content_attainment=mean(success),
        lower_bound=mp_binom_lower(sum(success),length(idx),alpha),
        mean_width=if(any(returned))mean(widths,na.rm=TRUE) else NA_real_,
        alpha=alpha)
    }
    summaries <- do.call(rbind,summaries)
    passing <- vapply(thresholds,function(threshold) {
      s <- summaries[summaries$threshold==threshold,]
      all(s$returned==s$replications & s$lower_bound>=cell$tolerance_confidence)
    },logical(1))
    chosen <- if(any(passing)) min(thresholds[passing]) else NA_real_
    policies[[i]] <- data.frame(n=cell$n,content=cell$content,
      tolerance_confidence=cell$tolerance_confidence,threshold=chosen,
      method_id="mti_profile_fractional_admissible_v1",
      decision=if(is.finite(chosen))"calibrated_on_independent_assessment" else
        "inconclusive_or_no_admissible_threshold",
      development_replications=cfg$calibration$threshold_development_replications,
      assessment_replications=cfg$calibration$replications,
      familywise_confidence=cfg$calibration$familywise_confidence)
    diagnostics[[i]] <- summaries
  }
  policy <- do.call(rbind,policies)
  pma_write_csv(policy,file.path(output,"policy.csv"))
  pma_write_csv(do.call(rbind,diagnostics),file.path(output,"threshold_assessment.csv"))
  policy
}

mp_simulation_stage <- function(cfg,stage,output,run_root,workers,validation_policy_path=NULL) {
  profile <- mp_frozen_profile(cfg)
  settings <- cfg[[stage]]
  validation <- profile$validation
  cells <- validation$modes$confirmatory$design_cells
  dgps <- validation$dgps
  ids <- unlist(validation$modes$confirmatory$dgp_ids)
  dgps <- setNames(dgps,vapply(dgps,`[[`,character(1),"dgp_id"))[ids]
  generators <- mp_dgp_environment()
  if(stage=="validation") {
    policy_path <- if(is.null(validation_policy_path))
      file.path(run_root,"calibration","policy.csv") else validation_policy_path
    if(!file.exists(policy_path)) stop("A frozen new calibration policy is required.")
    policy <- read.csv(policy_path)
    if(any(!is.finite(policy$threshold))) stop("Calibration is unresolved; validation cannot start.")
    policy_hash <- mp_sha(policy_path)
  }
  tasks <- list(); cell_index <- 0L
  for(cell in cells) for(family in ids) {
    cell_index <- cell_index+1L
    reps <- settings$replications
    dev <- if(stage=="calibration") settings$threshold_development_replications else 0L
    rep <- seq_len(reps+dev)
    tasks[[length(tasks)+1L]] <- data.frame(cell_index=cell_index,n=cell$n,
      content=cell$guaranteed_content,tolerance_confidence=cell$tolerance_confidence,
      dgp_id=family,replication=rep,
      role=ifelse(rep<=dev,"threshold_development",
        if(stage=="calibration")"calibration_assessment" else "independent_validation"),
      seed=settings$seed_base+cell_index*settings$cell_seed_stride+rep)
  }
  tasks <- do.call(rbind,tasks)
  one <- function(task) {
    set.seed(task$seed)
    dgp <- generators$dgp_meta(dgps[[task$dgp_id]])
    y <- dgp$r(task$n)
    scan <- mp_scan_fraction(task$n,task$content,task$tolerance_confidence)
    menu <- mp_menu(y,task$content,task$tolerance_confidence,profile,scan)
    fit <- mp_profile(y,menu$q_grid,task$tolerance_confidence,cfg,content=task$content)
    candidates <- fit$candidates
    valid <- candidates$profile_converged
    candidates$true_content <- NA_real_
    candidates$true_content[valid] <- dgp$p(candidates$upper[valid])-dgp$p(candidates$lower[valid])
    if(stage=="calibration") return(list(identity=task,candidates=candidates))
    row <- policy[policy$n==task$n & abs(policy$content-task$content)<1e-12 &
      abs(policy$tolerance_confidence-task$tolerance_confidence)<1e-12,]
    if(nrow(row)!=1L) stop("Ambiguous new policy cell.")
    selected <- mp_select(candidates,row$threshold)
    interval <- function(method,l=NA_real_,u=NA_real_,status="returned",selected=NULL,
                         numerical_failures=0L,candidate_count=NA_integer_,
                         ecm_candidate_nonconvergence=NA_integer_) {
      returned <- is.finite(l) && is.finite(u) && l<=u
      content <- if(returned)dgp$p(u)-dgp$p(l) else NA_real_
      quantity <- function(name) {
        if(!is.null(selected) && nrow(selected)==1L && name %in% names(selected))
          selected[[name]][1L] else NA_real_
      }
      data.frame(task,method=method,returned=returned,status=status,lower=l,upper=u,
        width=if(returned)u-l else NA_real_,true_content=content,
        content_attained=returned && content>=task$content,
        selected_q=quantity("target_content"),selected_tilt=quantity("mean_tilt"),
        posterior_content_probability=quantity("posterior_content_probability"),
        relative_objective_gap=quantity("relative_objective_gap"),
        ecm_converged=as.logical(quantity("ecm_converged")),
        ecm_iterations=quantity("ecm_iterations"),
        ecm_final_stationarity=quantity("ecm_final_stationarity"),
        ecm_candidate_nonconvergence=ecm_candidate_nonconvergence,
        numerical_failure_candidates=numerical_failures,candidate_count=candidate_count)
    }
    rows <- list(interval("mti_profile_fractional_admissible_v1",
      if(nrow(selected))selected$lower else NA_real_,
      if(nrow(selected))selected$upper else NA_real_,
      if(any(!candidates$profile_converged))"candidate_numerical_failure" else
        if(nrow(selected))"returned" else "no_eligible_interval",
      selected,sum(!candidates$profile_converged),nrow(candidates)))
    if("mti_ecm_historical" %in% unlist(settings$methods)) {
      base <- profile$direct_dp$base
      old <- rqrgibbs::rqr_mti_ecm_dp_profile_action(y,task$content,
        menu$posterior_confidence,dp_concentration=menu$dp_concentration,
        dp_base_measure=rqrgibbs::rqr_dp_base_normal(base$mean,base$sd),
        q_grid=menu$q_grid,tilt_grid_control=profile$config$tilt_grid_control,
        learning_rate=profile$config$learning_rate,
        beta_prior_obj=rqrgibbs::beta_prior("ridge",ridge=list(tau2=profile$config$beta_ridge_tau2)),
        ecm_control=profile$config$ecm_control,
        expand_if_empty=isTRUE(profile$config$expand_if_empty))
      s <- old$selected
      rows[[length(rows)+1L]] <- interval("mti_ecm_historical",
        if(nrow(s))s$lower else NA_real_,if(nrow(s))s$upper else NA_real_,
        if(nrow(s))"returned" else "no_eligible_interval",selected=s,
        numerical_failures=sum(!old$candidates$finite_interval),
        candidate_count=nrow(old$candidates),
        ecm_candidate_nonconvergence=sum(!old$candidates$ecm_converged))
    }
    if("young_mathew" %in% unlist(settings$methods)) {
      ym <- pta_fit_young_mathew(y,task$content,task$tolerance_confidence)
      rows[[length(rows)+1L]] <- interval("young_mathew",ym$lower,ym$upper,
        if(isTRUE(ym$returned))"returned" else "benchmark_failure")
    }
    if("wilks" %in% unlist(settings$methods)) rows[[length(rows)+1L]] <-
      interval("wilks",min(y),max(y))
    do.call(rbind,rows)
  }
  values <- mp_checkpoint_jobs(tasks,one,file.path(output,"checkpoints"),workers,
    cfg$execution$checkpoint_batch)
  errors <- vapply(values,function(x)is.list(x) && !is.data.frame(x) && !is.null(x$error),logical(1))
  if(any(errors)) { mp_atomic_rds(values[errors],file.path(output,"errors.rds")); stop("Simulation tasks failed; errors retained.") }
  if(stage=="calibration") {
    numerical <- sum(vapply(values,function(x)any(!x$candidates$profile_converged),logical(1)))
    policy <- mp_calibration_policy(values,cfg,output)
    return(list(passed=numerical==0 && all(is.finite(policy$threshold)),
      completed_datasets=nrow(tasks),numerical_failure_datasets=numerical,
      calibrated_cells=sum(is.finite(policy$threshold)),expected_cells=nrow(policy),
      interpretation="Finite-panel calibration; unresolved bounds are not evidence of a universal failure."))
  }
  results <- do.call(rbind,values)
  pma_write_csv(results,file.path(output,"results.csv"))
  if(mp_sha(policy_path)!=policy_hash) stop("New calibration policy changed during validation.")
  list(passed=TRUE,completed_datasets=nrow(tasks),method_fits=nrow(results),
    frozen_policy_sha256=policy_hash,nonreturned=sum(!results$returned))
}

mp_collect <- function(cfg,output) {
  file <- file.path(output,"validation","results.csv")
  if(!file.exists(file)) {
    cat("Independent validation has not completed; no manuscript inputs replaced.\n")
    return(invisible(NULL))
  }
  z <- read.csv(file)
  key <- interaction(z$n,z$content,z$tolerance_confidence,z$dgp_id,z$method,drop=TRUE)
  summary <- do.call(rbind,lapply(split(z,key),function(s) {
    w <- s$width[s$returned]
    data.frame(n=s$n[1],content=s$content[1],tolerance_confidence=s$tolerance_confidence[1],
      dgp_id=s$dgp_id[1],method=s$method[1],replications=nrow(s),returned=sum(s$returned),
      content_attainment=mean(s$content_attained),
      attainment_lower=mp_binom_lower(sum(s$content_attained),nrow(s),.025),
      attainment_upper=if(all(s$content_attained))1 else stats::qbeta(.975,
        sum(s$content_attained)+1,nrow(s)-sum(s$content_attained)),
      mean_width=if(length(w))mean(w) else NA_real_,
      width_q025=if(length(w))as.numeric(quantile(w,.025,type=8)) else NA_real_,
      width_q975=if(length(w))as.numeric(quantile(w,.975,type=8)) else NA_real_)
  }))
  pma_write_csv(summary,file.path(output,"validation_summary.csv"))
  pta_atomic_write_json(list(completed=TRUE,article_results_replaced=FALSE,
    method_decision="Independent results require scientific assessment before manuscript replacement.",
    family_design_cells=length(unique(interaction(z$n,z$content,z$dgp_id)))),
    file.path(output,"study_completion.json"))
}

mp_checkpoint_jobs <- function(tasks,one,directory,workers,batch_size=100L) {
  dir.create(directory,recursive=TRUE,showWarnings=FALSE)
  batches <- split(seq_len(nrow(tasks)),ceiling(seq_len(nrow(tasks))/batch_size))
  for(b in seq_along(batches)) {
    path <- file.path(directory,sprintf("batch_%05d.rds",b))
    if(file.exists(path)) {
      saved <- readRDS(path)
      if(!identical(saved$tasks,tasks[batches[[b]],,drop=FALSE])) stop("Checkpoint task mismatch.")
      next
    }
    indices <- batches[[b]]
    safe <- function(i) tryCatch(one(tasks[i,,drop=FALSE]),
      error=function(e) list(error=conditionMessage(e),task=tasks[i,,drop=FALSE]))
    values <- if(workers>1) parallel::mclapply(indices,safe,mc.cores=min(workers,length(indices)),
      mc.set.seed=FALSE) else lapply(indices,safe)
    mp_atomic_rds(list(tasks=tasks[indices,,drop=FALSE],values=values),path)
    pta_atomic_write_json(list(completed_tasks=max(indices),expected_tasks=nrow(tasks),
      completed_batches=b,expected_batches=length(batches)),file.path(directory,"progress.json"))
  }
  unlist(lapply(seq_along(batches),function(b) readRDS(file.path(directory,
    sprintf("batch_%05d.rds",b)))$values),recursive=FALSE)
}

mp_saved_cases <- function(cfg,output,workers) {
  inputs <- mp_inputs(cfg); profile <- mp_frozen_profile(cfg)
  z <- inputs$results[inputs$results$method_id=="mti_ecm_adaptive_cell",]
  groups <- split(inputs$membership,paste(inputs$membership$split_scheme,
    inputs$membership$split_id,sep="|"))
  one <- function(task) {
    r <- z[task$index,]
    s <- groups[[paste(r$split_scheme,r$split_id,sep="|")]]
    y <- inputs$observed[[r$response_id]][match(s$analysis_row_id[s$training],inputs$observed$.analysis_row)]
    grid <- mp_menu(y,r$content,r$tolerance_confidence,profile,r$scan_reference_retained_fraction)$q_grid
    fit <- mp_profile(y,grid,r$mti_posterior_confidence,cfg,content=r$content)
    selected <- fit$selected
    data.frame(response_id=r$response_id,split_scheme=r$split_scheme,split_id=r$split_id,
      status=fit$status,returned=fit$interval_returned,
      numerical_failures=sum(!fit$candidates$profile_converged),
      lower=if(nrow(selected)) selected$lower else NA_real_,
      upper=if(nrow(selected)) selected$upper else NA_real_,
      width=if(nrow(selected)) selected$width else NA_real_,
      historical_width=r$width,
      historical_screen_for_diagnostic_only=r$mti_posterior_confidence,
      candidate_count=nrow(fit$candidates),
      maximum_relative_objective_gap=max(abs(fit$candidates$relative_objective_gap),na.rm=TRUE))
  }
  values <- mp_checkpoint_jobs(data.frame(index=seq_len(nrow(z))),one,
    file.path(output,"checkpoints"),workers)
  errors <- vapply(values,function(x)!is.null(x$error),logical(1))
  if(any(errors)) { mp_atomic_rds(values[errors],file.path(output,"errors.rds")); stop("Saved-case tasks failed.") }
  result <- do.call(rbind,values)
  pma_write_csv(result,file.path(output,"saved_case_diagnostics.csv"))
  list(passed=all(result$numerical_failures==0),completed=nrow(result),
    returned=sum(result$returned),numerical_failures=sum(result$numerical_failures))
}

mp_report <- function(output,tail_iterations=NULL) {
  path <- file.path(output,"mechanism.csv")
  if(!file.exists(path)) return(invisible(NULL))
  z <- read.csv(path)
  filename <- if(is.null(tail_iterations))"mti_profile_mechanism.pdf" else
    paste0("mti_profile_mechanism_final_",tail_iterations,"_iterations.pdf")
  grDevices::pdf(file.path(output,filename),width=11,height=8.5)
  on.exit(grDevices::dev.off(),add=TRUE)
  plot.new()
  title("MTI fixed-target numerical assessment")
  explanation <- c("Each observed or synthetic sample has four calculations:",
    "A: historical ECM iteration and historical tilt",
    "B: convex-profile minimization and historical tilt",
    "C: historical ECM iteration and mass-consistent fractional tilt",
    "D: convex-profile minimization and mass-consistent fractional tilt",
    "The ECM initialization is matched between A and C.",
    "Endpoint and penalized-objective traces follow for each sample.",
    "These are deterministic numerical iterations, not MCMC traces.",
    "The fitted content q and each tilt remain fixed within a calculation.",
    if(is.null(tail_iterations))"Early profile iterations bisect a wide theoretical midpoint bracket."
      else paste("Only the final",tail_iterations,"available iterations are shown."),
    "This diagnostic report does not establish tolerance confidence.")
  text(.05,seq(.85,.15,length.out=length(explanation)),explanation,adj=0,cex=1)
  for(id in unique(z$case_id)) {
    s <- z[z$case_id==id,]
    par(mfrow=c(2,2),mar=c(4,4,3,1),oma=c(0,0,3,0))
    for(name in s$variant) {
      t <- read.csv(file.path(output,paste0("trace_",id,"_",name,".csv")))
      if(!is.null(tail_iterations))t <- utils::tail(t,tail_iterations)
      if("root1" %in% names(t)) {
        matplot(t$iteration,cbind(t$root1,t$root2),type="l",
          lty=1,col=c("#0072B2","#D55E00"),xlab="Iteration",ylab="Endpoint",
          main=paste("Calculation",name))
      } else {
        matplot(t$iteration,cbind(t$lower,t$upper),type="l",lty=1,
          col=c("#0072B2","#D55E00"),xlab="Iteration",ylab="Endpoint",
          main=paste("Calculation",name))
      }
      legend("topright",c("Lower","Upper"),lty=1,col=c("#0072B2","#D55E00"),bty="n")
    }
    mtext(id,outer=TRUE,cex=1.2)
    par(mfrow=c(2,2),mar=c(4,4,3,1),oma=c(0,0,3,0))
    for(name in s$variant) {
      t <- read.csv(file.path(output,paste0("trace_",id,"_",name,".csv")))
      if(!is.null(tail_iterations))t <- utils::tail(t,tail_iterations)
      value <- if("exact_objective" %in% names(t))t$exact_objective else t$objective
      plot(t$iteration,value,type="l",col="#0072B2",xlab="Iteration",
        ylab="Penalized objective",main=paste("Calculation",name))
      abline(h=s$objective[s$variant==name],lty=2,col="#D55E00")
    }
    mtext(paste(id,"- objective"),outer=TRUE,cex=1.2)
  }
}
