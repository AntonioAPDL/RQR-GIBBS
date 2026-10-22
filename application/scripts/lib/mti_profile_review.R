mpr_require <- function(ok, message) {
  if(!isTRUE(ok)) stop(message,call.=FALSE)
}

mpr_read_json <- function(path) {
  if(file.exists(path)) jsonlite::read_json(path,simplifyVector=TRUE) else NULL
}

mpr_git_clean <- function(status) {
  code <- attr(status,"status")
  (is.null(code) || identical(as.integer(code),0L)) && !anyNA(status) && all(!nzchar(status))
}

mpr_design <- function(frozen) {
  families <- unlist(frozen$modes$confirmatory$dgp_ids,use.names=FALSE)
  rows <- list()
  for(cell in frozen$modes$confirmatory$design_cells) for(family in families) {
    rows[[length(rows)+1L]] <- data.frame(cell_index=length(rows)+1L,n=cell$n,
      content=cell$guaranteed_content,tolerance_confidence=cell$tolerance_confidence,
      dgp_id=family)
  }
  do.call(rbind,rows)
}

mpr_task_key <- function(z) paste(z$cell_index,z$replication,sep=":")

mpr_cell_key <- function(z) paste(z$n,sprintf("%.17g",z$content),
  sprintf("%.17g",z$tolerance_confidence),sep=":")

mpr_check_tasks <- function(tasks,design,cfg,stage) {
  fields <- c("cell_index","n","content","tolerance_confidence","dgp_id",
    "replication","role","seed")
  mpr_require(all(fields %in% names(tasks)),"Task identity fields are missing.")
  mpr_require(nrow(tasks)>0 && !anyNA(tasks[fields]),"Missing task identity.")
  settings <- cfg[[stage]]
  development <- if(stage=="calibration")settings$threshold_development_replications else 0L
  mpr_require(all(tasks$cell_index %in% design$cell_index) &&
    all(tasks$replication==as.integer(tasks$replication) & tasks$replication>=1 &
      tasks$replication<=settings$replications+development),"Unexpected task index.")
  expected <- design[match(tasks$cell_index,design$cell_index),]
  for(field in c("n","content","tolerance_confidence","dgp_id")) {
    mpr_require(all(tasks[[field]]==expected[[field]]),paste("Unexpected task",field))
  }
  role <- if(stage=="validation")rep("independent_validation",nrow(tasks)) else
    ifelse(tasks$replication<=development,"threshold_development","calibration_assessment")
  seed <- settings$seed_base+tasks$cell_index*settings$cell_seed_stride+tasks$replication
  mpr_require(all(tasks$seed==seed) && all(tasks$role==role),"Task seed or data role changed.")
  invisible(TRUE)
}

mpr_check_candidates <- function(z,cfg) {
  required <- c("profile_converged","relative_objective_gap","lower","upper",
    "width","posterior_content_probability","true_content")
  mpr_require(is.data.frame(z) && nrow(z)>0 && all(required %in% names(z)),
    "Candidate diagnostics are missing.")
  mpr_require(is.logical(z$profile_converged) && !anyNA(z$profile_converged),
    "Candidate convergence flags are missing.")
  valid <- z$profile_converged
  if(any(valid)) {
    mpr_require(all(is.finite(z$relative_objective_gap[valid])) &&
      all(abs(z$relative_objective_gap[valid])<=cfg$profile$objective_tolerance),
      "A converged candidate fails its objective-bound requirement.")
    mpr_require(all(is.finite(z$lower[valid]) & is.finite(z$upper[valid]) &
      z$lower[valid]<=z$upper[valid]),"Invalid converged candidate endpoints.")
    mpr_require(all(is.finite(z$true_content[valid]) & z$true_content[valid]>=0 &
      z$true_content[valid]<=1),"Invalid candidate population content.")
    mpr_require(all(is.finite(z$posterior_content_probability[valid]) &
      z$posterior_content_probability[valid]>=0 & z$posterior_content_probability[valid]<=1),
      "Invalid candidate DP posterior probability.")
    difference <- z$upper[valid]-z$lower[valid]
    error <- 1000*.Machine$double.eps*(1+abs(z$lower[valid])+abs(z$upper[valid]))
    mpr_require(all(is.finite(z$width[valid]) & abs(z$width[valid]-difference)<=error),
      "Candidate widths disagree with endpoints.")
  }
  sum(!valid)
}

mpr_candidate_decisions <- function(z,thresholds,content) {
  returned <- success <- rep(FALSE,length(thresholds))
  width <- rep(0,length(thresholds))
  if(!any(!z$profile_converged)) {
    z <- z[order(z$width,z$target_content,abs(z$mean_tilt-z$central_tilt),
      z$lower,z$candidate_index),]
    index <- vapply(thresholds,function(t)
      match(TRUE,!is.na(z$posterior_content_probability) & z$posterior_content_probability>=t,
        nomatch=0L),integer(1))
    returned <- index>0
    success[returned] <- z$true_content[index[returned]]>=content
    width[returned] <- z$width[index[returned]]
  }
  list(returned=as.integer(returned),successes=as.integer(success),width_sum=width)
}

mpr_checkpoint_audit <- function(run,stage,design,cfg,threshold_assessment=NULL) {
  directory <- file.path(run,stage,"checkpoints")
  files <- sort(list.files(directory,"^batch_[0-9]+[.]rds$",full.names=TRUE))
  rows <- vector("list",length(files))
  archives <- vector("list",length(files))
  development <- list()
  counts <- list()
  if(!is.null(threshold_assessment)) for(i in seq_len(nrow(design))) {
    z <- design[i,]
    thresholds <- sort(unique(threshold_assessment$threshold[
      mpr_cell_key(threshold_assessment)==mpr_cell_key(z)]))
    counts[[as.character(z$cell_index)]] <- data.frame(z[rep(1L,length(thresholds)),,drop=FALSE],threshold=thresholds,
      replications=0L,returned=0L,successes=0L,width_sum=0)
  }
  for(b in seq_along(files)) {
    digest <- mp_sha(files[b])
    saved <- readRDS(files[b])
    tasks <- saved$tasks
    mpr_check_tasks(tasks,design,cfg,stage)
    mpr_require(!anyDuplicated(mpr_task_key(tasks)),"Duplicate dataset within checkpoint.")
    mpr_require(length(saved$values)==nrow(tasks),"Checkpoint result count mismatch.")
    errors <- failures <- integer(nrow(tasks))
    for(i in seq_len(nrow(tasks))) {
      value <- saved$values[[i]]
      if(is.list(value) && !is.null(value$error)) {
        errors[i] <- 1L
        mpr_require(identical(as.list(value$task),as.list(tasks[i,,drop=FALSE])),
          "Failed task identity mismatch.")
      } else if(stage=="calibration") {
        mpr_require(identical(as.list(value$identity),as.list(tasks[i,,drop=FALSE])),
          "Candidate result identity mismatch.")
        failures[i] <- mpr_check_candidates(value$candidates,cfg)
        if(tasks$role[i]=="threshold_development") {
          key <- mpr_cell_key(tasks[i,])
          probabilities <- value$candidates$posterior_content_probability
          if(is.null(development[[key]]))development[[key]] <- list()
          development[[key]][[length(development[[key]])+1L]] <- probabilities[
            is.finite(probabilities) & probabilities>0 & probabilities<1]
        } else if(length(counts)) {
          key <- as.character(tasks$cell_index[i])
          current <- counts[[key]]
          decision <- mpr_candidate_decisions(value$candidates,current$threshold,tasks$content[i])
          current$replications <- current$replications+1L
          for(field in names(decision))current[[field]] <- current[[field]]+decision[[field]]
          counts[[key]] <- current
        }
      } else {
        mpr_require(is.data.frame(value) && nrow(value)>0,"Validation task rows are missing.")
        for(field in names(tasks)) mpr_require(all(value[[field]]==tasks[[field]][i]),
          "Validation checkpoint identity mismatch.")
      }
    }
    rows[[b]] <- data.frame(tasks,task_errors=errors,numerical_failed_candidates=failures)
    mpr_require(identical(digest,mp_sha(files[b])),"Checkpoint changed during assessment.")
    archives[[b]] <- data.frame(path=files[b],sha256=digest,bytes=file.info(files[b])$size)
  }
  if(!length(rows)) return(data.frame())
  result <- do.call(rbind,rows)
  mpr_require(!anyDuplicated(mpr_task_key(result)),"Dataset repeated across checkpoints.")
  attr(result,"development_probabilities") <- lapply(development,unlist,use.names=FALSE)
  attr(result,"checkpoint_records") <- do.call(rbind,archives)
  if(length(counts))attr(result,"independent_threshold_counts") <- do.call(rbind,counts)
  result
}

mpr_binomial_bounds <- function(successes,trials,alpha) {
  c(lower=if(successes==0)0 else stats::qbeta(alpha,successes,trials-successes+1),
    upper=if(successes==trials)1 else stats::qbeta(1-alpha,successes+1,trials-successes))
}

mpr_validate_results <- function(z,design,cfg,cdf_by_family=NULL,
                                 primary_method="mti_profile_fractional_admissible_v1") {
  required <- c("method","returned","status","lower","upper","width","true_content",
    "content_attained","numerical_failure_candidates","relative_objective_gap")
  mpr_require(all(required %in% names(z)),"Validation result fields are missing.")
  mpr_check_tasks(z,design,cfg,"validation")
  mpr_require(!anyDuplicated(paste(mpr_task_key(z),z$method)),"Duplicate dataset/method row.")
  methods <- unlist(cfg$validation$methods,use.names=FALSE)
  mpr_require(setequal(unique(z$method),methods),"Unexpected validation methods.")
  expected <- nrow(design)*cfg$validation$replications*length(methods)
  mpr_require(nrow(z)==expected,"Independent validation is incomplete.")
  for(method in methods) mpr_require(sum(z$method==method)==expected/length(methods),
    "Unpaired validation methods.")
  mpr_require(is.logical(z$returned) && !anyNA(z$returned) &&
    is.logical(z$content_attained) && !anyNA(z$content_attained),"Invalid validation flags.")
  good <- z$returned
  mpr_require(all(is.finite(z$width[good]) & z$width[good]>=0 &
    is.finite(z$lower[good]) & is.finite(z$upper[good])),"Invalid returned endpoints or widths.")
  error <- 1000*.Machine$double.eps*(1+abs(z$lower[good])+abs(z$upper[good]))
  mpr_require(all(abs(z$width[good]-(z$upper[good]-z$lower[good]))<=error),
    "Validation widths disagree with endpoints.")
  mpr_require(all(is.finite(z$true_content[good]) & z$true_content[good]>=0 &
    z$true_content[good]<=1) && all(is.na(z$true_content[!good]) & is.na(z$width[!good])),
    "Invalid population content or nonreturned width.")
  mpr_require(all(z$content_attained==(good & !is.na(z$true_content) &
    z$true_content>=z$content)),"Content attainment uses the wrong event or denominator.")
  mpr_require(all((z$status=="returned")==good),"Return status conflicts with returned flag.")
  primary <- z$method==primary_method
  mpr_require(all(is.finite(z$numerical_failure_candidates) & z$numerical_failure_candidates>=0),
    "Missing numerical failure counts.")
  valid_primary <- primary & good
  mpr_require(all(z$numerical_failure_candidates[valid_primary]==0) &&
    all(is.finite(z$relative_objective_gap[valid_primary]) &
      abs(z$relative_objective_gap[valid_primary])<=cfg$profile$objective_tolerance),
    "A returned primary interval fails numerical requirements.")
  if(!is.null(cdf_by_family)) for(family in unique(z$dgp_id)) {
    use <- which(z$dgp_id==family & good)
    computed <- cdf_by_family[[family]](z$upper[use])-cdf_by_family[[family]](z$lower[use])
    mpr_require(all(is.finite(computed) & abs(computed-z$true_content[use])<=1e-12),
      "Stored population content disagrees with the frozen DGP CDF.")
  }
  calibration_seeds <- unlist(lapply(design$cell_index,function(i)
    cfg$calibration$seed_base+i*cfg$calibration$cell_seed_stride+
      seq_len(cfg$calibration$replications+cfg$calibration$threshold_development_replications)))
  mpr_require(!any(unique(z$seed) %in% calibration_seeds),"Calibration and validation seeds overlap.")
  invisible(TRUE)
}

mpr_quantiles <- function(x) {
  if(length(x))as.numeric(stats::quantile(x,c(.025,.975),type=8,names=FALSE)) else c(NA_real_,NA_real_)
}

mpr_validation_summary <- function(z,design,cfg,review) {
  groups <- split(z,interaction(z$cell_index,z$method,drop=TRUE))
  confidence <- cfg$calibration$familywise_confidence
  simultaneous_alpha <- (1-confidence)/(2*nrow(design))
  do.call(rbind,lapply(groups,function(s) {
    n <- nrow(s); k <- sum(s$content_attained)
    pointwise <- mpr_binomial_bounds(k,n,(1-confidence)/2)
    simultaneous <- if(s$method[1]==review$primary_method)
      mpr_binomial_bounds(k,n,simultaneous_alpha) else c(lower=NA_real_,upper=NA_real_)
    widths <- s$width[s$returned]
    population <- s$true_content[s$returned]
    wq <- mpr_quantiles(widths); cq <- mpr_quantiles(population)
    status <- if(s$method[1]!=review$primary_method)"benchmark" else
      if(any(s$numerical_failure_candidates>0))"numerical_issue" else
      if(any(!s$returned))"nonreturns_observed" else
      if(simultaneous["upper"]<s$tolerance_confidence[1])"evidence_of_shortfall" else
      if(simultaneous["lower"]>=s$tolerance_confidence[1])"strong_panel_evidence" else
        "inconclusive"
    data.frame(s[1,c("cell_index","n","content","tolerance_confidence","dgp_id","method")],
      replications=n,returned=sum(s$returned),successes=k,attainment=k/n,
      attainment_mcse=sqrt((k/n)*(1-k/n)/n),attainment_lower=pointwise["lower"],
      attainment_upper=pointwise["upper"],simultaneous_lower=simultaneous["lower"],
      simultaneous_upper=simultaneous["upper"],evidence=status,
      width_mean=if(length(widths))mean(widths) else NA_real_,
      width_mean_mcse=if(length(widths)>1)stats::sd(widths)/sqrt(length(widths)) else NA_real_,
      width_q025=wq[1],width_q975=wq[2],population_content_mean=if(length(population))
        mean(population) else NA_real_,population_content_q025=cq[1],population_content_q975=cq[2])
  }))
}

mpr_paired_summary <- function(z,review) {
  primary <- z[z$method==review$primary_method,]
  rows <- list()
  for(method in setdiff(unique(z$method),review$primary_method)) {
    baseline <- z[z$method==method,]
    index <- match(mpr_task_key(primary),mpr_task_key(baseline))
    mpr_require(!anyNA(index) && !anyDuplicated(index),"Paired dataset identities do not match.")
    baseline <- baseline[index,]
    mpr_require(all(primary$seed==baseline$seed),"Paired method seeds do not match.")
    for(cell in unique(primary$cell_index)) {
      a <- primary[primary$cell_index==cell,]; b <- baseline[baseline$cell_index==cell,]
      change <- as.numeric(a$content_attained)-as.numeric(b$content_attained)
      jointly <- a$returned & b$returned
      difference <- a$width[jointly]-b$width[jointly]
      quantiles <- mpr_quantiles(difference)
      rows[[length(rows)+1L]] <- data.frame(a[1,c("cell_index","n","content",
        "tolerance_confidence","dgp_id")],comparator=method,replications=nrow(a),
        primary_only_successes=sum(a$content_attained & !b$content_attained),
        comparator_only_successes=sum(!a$content_attained & b$content_attained),
        attainment_change=mean(change),attainment_change_mcse=stats::sd(change)/sqrt(length(change)),
        jointly_returned=sum(jointly),width_change_mean=if(length(difference))mean(difference) else NA_real_,
        width_change_mean_mcse=if(length(difference)>1)stats::sd(difference)/sqrt(length(difference)) else NA_real_,
        width_change_median=if(length(difference))stats::median(difference) else NA_real_,
        width_change_q025=quantiles[1],width_change_q975=quantiles[2])
    }
  }
  if(length(rows))do.call(rbind,rows) else data.frame()
}

mpr_stage_state <- function(run,stage,completed,expected,active) {
  completion <- mpr_read_json(file.path(run,stage,"completion.json"))
  status <- if(!is.null(completion)) {
    mpr_require(completed==expected,"Completed stage has an incomplete dataset count.")
    if(isTRUE(completion$passed))"passed" else if(stage=="calibration")
      if(!is.null(completion$numerical_failure_datasets) && completion$numerical_failure_datasets>0)
        "numerical_issue" else "calibration_unresolved" else "unresolved_or_failed"
  } else if(completed==expected) {
    if(active)"assessment_running" else "assessment_incomplete"
  } else if(completed>0) {
    if(active)"fitting_running" else "interrupted_or_incomplete"
  } else "not_started"
  data.frame(stage=stage,completed_datasets=completed,expected_datasets=expected,
    remaining_fits=expected-completed,status=status)
}

mpr_calibration_check <- function(assessment,policy,design,cfg,review,development,
                                  independent_counts=NULL) {
  required <- c("n","content","tolerance_confidence","dgp_id","threshold",
    "replications","returned","successes","content_attainment","lower_bound","alpha")
  mpr_require(all(required %in% names(assessment)) &&
    all(c("n","content","tolerance_confidence","threshold") %in% names(policy)),
    "Calibration assessment or policy fields are missing.")
  cells <- unique(design[c("n","content","tolerance_confidence")])
  mpr_require(nrow(policy)==nrow(cells) && !anyDuplicated(mpr_cell_key(policy)),
    "Calibration policy cell count is incorrect.")
  alpha <- (1-cfg$calibration$familywise_confidence)/
    (nrow(design)*(cfg$calibration$threshold_quantile_count+2L))
  mpr_require(all(assessment$replications==cfg$calibration$replications) &&
    all(assessment$successes==as.integer(assessment$successes) &
      assessment$returned==as.integer(assessment$returned) &
      assessment$successes>=0 & assessment$successes<=assessment$returned &
      assessment$returned<=assessment$replications),"Invalid calibration success counts.")
  lower <- mapply(function(k,n)mpr_binomial_bounds(k,n,alpha)["lower"],
    assessment$successes,assessment$replications)
  mpr_require(all(abs(assessment$alpha-alpha)<1e-15) &&
    all(abs(assessment$lower_bound-lower)<1e-12) &&
    all(abs(assessment$content_attainment-assessment$successes/assessment$replications)<1e-12),
    "Calibration bounds or attainment disagree with binomial counts.")
  if(!is.null(independent_counts)) {
    key <- function(z)paste(mpr_cell_key(z),z$dgp_id,sprintf("%.17g",z$threshold),sep="|")
    index <- match(key(assessment),key(independent_counts))
    mpr_require(!anyNA(index) && !anyDuplicated(index) && nrow(independent_counts)==nrow(assessment),
      "Independent candidate-selection assessment is incomplete.")
    counts <- independent_counts[index,]
    mpr_require(all(assessment$replications==counts$replications &
      assessment$returned==counts$returned & assessment$successes==counts$successes),
      "Calibration counts disagree with independently selected candidates.")
    valid <- counts$returned>0
    mpr_require(all(abs(assessment$mean_width[valid]-counts$width_sum[valid]/
      counts$returned[valid])<1e-10*(1+abs(assessment$mean_width[valid]))),
      "Calibration width means disagree with independently selected candidates.")
  }
  diagnostics <- list()
  for(i in seq_len(nrow(cells))) {
    cell <- cells[i,]; key <- mpr_cell_key(cell)
    s <- assessment[mpr_cell_key(assessment)==key,]
    p <- policy[mpr_cell_key(policy)==key,]
    probabilities <- development[[key]]
    mpr_require(length(probabilities)>0,"Independent threshold-development data are absent.")
    thresholds <- sort(unique(c(cell$tolerance_confidence,review$historical_reference_threshold,
      as.numeric(stats::quantile(probabilities,seq(0,1,
        length.out=cfg$calibration$threshold_quantile_count),type=8,names=FALSE)))))
    thresholds <- thresholds[thresholds>0 & thresholds<1]
    observed <- sort(unique(s$threshold))
    mpr_require(length(observed)==length(thresholds) && all(abs(observed-thresholds)<1e-14),
      "Threshold grid disagrees with independent development data.")
    families <- design$dgp_id[mpr_cell_key(design)==key]
    mpr_require(nrow(s)==length(thresholds)*length(families) &&
      !anyDuplicated(paste(s$threshold,s$dgp_id)) && setequal(unique(s$dgp_id),families),
      "Calibration family/threshold assessment is incomplete.")
    passing <- vapply(thresholds,function(threshold) {
      z <- s[s$threshold==threshold,]
      all(z$returned==z$replications & z$lower_bound>=cell$tolerance_confidence)
    },logical(1))
    chosen <- if(any(passing))min(thresholds[passing]) else NA_real_
    mpr_require(nrow(p)==1 && identical(is.finite(p$threshold),is.finite(chosen)) &&
      (!is.finite(chosen) || abs(p$threshold-chosen)<1e-14),
      "Policy does not use the least passing prespecified threshold.")
    point_possible <- any(vapply(thresholds,function(t) {
      z <- s[s$threshold==t,]
      all(z$returned==z$replications & z$content_attainment>=cell$tolerance_confidence)
    },logical(1)))
    qualities <- do.call(rbind,lapply(thresholds,function(t) {
      z <- s[s$threshold==t,]
      data.frame(threshold=t,worst_attainment=min(z$content_attainment),
        worst_lower=min(z$lower_bound),all_returned=all(z$returned==z$replications))
    }))
    available <- qualities[qualities$all_returned,]
    diagnostic <- if(nrow(available))available[order(-available$worst_lower,
      available$threshold),][1,] else qualities[0,]
    diagnostics[[i]] <- data.frame(cell,threshold=chosen,thresholds_assessed=length(thresholds),
      diagnostic_only_threshold=if(nrow(diagnostic))diagnostic$threshold else NA_real_,
      diagnostic_worst_attainment=if(nrow(diagnostic))diagnostic$worst_attainment else NA_real_,
      diagnostic_worst_lower=if(nrow(diagnostic))diagnostic$worst_lower else NA_real_,
      passing_thresholds=sum(passing),interpretation=if(is.finite(chosen))"passed" else
        if(point_possible)"simultaneous_evidence_inconclusive" else
          "no_grid_threshold_meets_point_attainment_and_return_requirements")
  }
  do.call(rbind,diagnostics)
}

mpr_active <- function(run) {
  if(file.exists(file.path(run,"launcher_exit.json")))return(FALSE)
  path <- file.path(run,"launcher.pid")
  if(!file.exists(path))return(FALSE)
  pid <- suppressWarnings(as.integer(readLines(path,n=1L)))
  if(length(pid)!=1 || is.na(pid) || pid<1)return(FALSE)
  command <- suppressWarnings(system2("ps",c("-p",pid,"-o","args="),stdout=TRUE,stderr=FALSE))
  length(command)>0 && any(grepl(run,command,fixed=TRUE))
}

mpr_output_path <- function(path) {
  suffix <- character()
  while(!file.exists(path) && !dir.exists(path)) {
    suffix <- c(basename(path),suffix)
    parent <- dirname(path)
    mpr_require(!identical(parent,path),"Cannot resolve output directory.")
    path <- parent
  }
  mpr_require(dir.exists(path),"Output parent is not a directory.")
  do.call(file.path,as.list(c(normalizePath(path,mustWork=TRUE),suffix)))
}

mpr_health <- function(run,cfg,frozen) {
  design <- mpr_design(frozen)
  active <- mpr_active(run)
  rows <- do.call(rbind,lapply(c("calibration","validation"),function(stage) {
    progress <- mpr_read_json(file.path(run,stage,"checkpoints","progress.json"))
    completed <- if(is.null(progress))0 else progress$completed_tasks
    development <- if(stage=="calibration")cfg$calibration$threshold_development_replications else 0L
    mpr_stage_state(run,stage,completed,nrow(design)*(cfg[[stage]]$replications+development),active)
  }))
  complete <- mpr_read_json(file.path(run,"calibration","completion.json"))
  if(!is.null(complete) && !isTRUE(complete$passed) && rows$completed_datasets[2]==0)
    rows$status[2] <- "not_authorized_after_calibration"
  rows
}

mpr_check_study_config <- function(path,identity) {
  mpr_require(identical(mp_sha(path),identity$config_sha256),
    "Reporting configuration does not match the frozen study configuration.")
}

mpr_provenance <- function(run,cfg,repo,config_path) {
  identity <- readRDS(file.path(run,"identity.rds"))
  mpr_check_study_config(config_path,identity)
  manifest <- mpr_read_json(file.path(run,"manifest.json"))
  mpr_require(identical(identity$commit,manifest$commit),"Run source identities disagree.")
  sources <- identity$sources
  for(i in seq_len(nrow(sources))) {
    path <- file.path(repo,sources$path[i])
    mpr_require(file.exists(path) && identical(mp_sha(path),sources$sha256[i]),
      paste("Frozen calculation source differs:",sources$path[i]))
  }
  runtime <- identity$package_R_hashes
  for(i in seq_len(nrow(runtime))) mpr_require(file.exists(runtime$path[i]) &&
    identical(mp_sha(runtime$path[i]),runtime$sha256[i]),"Frozen R runtime changed or is absent.")
  library <- file.path(dirname(dirname(runtime$path[1L])),"libs","rqrgibbs.so")
  mpr_require(file.exists(library) && identical(mp_sha(library),identity$shared_library_sha256),
    "Frozen native runtime changed or is absent.")
  mpr_require(identical(mp_sha(file.path(repo,cfg$application_evidence,"results.csv")),
    cfg$expected_application_results_sha256),"Historical application results changed.")
  mpr_require(!isTRUE(manifest$exdqlm_loaded),"Unexpected external runtime in the study.")
  list(study_commit=identity$commit,benchmark_version=identity$benchmark_version,
    configured_exdqlm_commit=manifest$configured_exdqlm_commit,
    shared_library_sha256=identity$shared_library_sha256,verified=TRUE)
}

mpr_report_pdf <- function(output,status,calibration=NULL,assessment=NULL,summary=NULL,
                           method_labels=NULL,method_colors=NULL) {
  grDevices::pdf(file.path(output,"mti_profile_assessment.pdf"),width=12,height=8.5)
  on.exit(grDevices::dev.off(),add=TRUE)
  plot.new(); title("MTI revision: independent assessment")
  explanation <- c(paste("State:",status$decision),
    "Content attainment: fraction of all datasets with population content at least c.",
    "A nonreturned interval contributes a failure to this fraction.",
    "Pointwise intervals describe binomial Monte Carlo uncertainty within a case.",
    "Simultaneous bounds assess the primary method over the prespecified panel.",
    "Width ranges are the 2.5% and 97.5% empirical quantiles across returned intervals.",
    "Width ranges are distinct from uncertainty intervals for the mean width.",
    "All width comparisons remain within a distribution and design case.",
    "The report does not replace article results or establish distribution-free validity.")
  explanation <- c(explanation,
    "Calibration curves include unsuccessful candidate settings; extreme thresholds can cause nonreturns.")
  text(.04,seq(.88,.15,length.out=length(explanation)),explanation,adj=0,cex=.95)
  family_labels <- c(normal="Normal",student_t3="Student t (3 df)",
    exponential="Exponential",asym_laplace_tau010="Asymmetric Laplace (tau = 0.10)",
    two_piece_normal_1_12="Two-piece normal (1:12)",beta18="Beta(1, 8)",
    gamma05="Gamma (shape = 0.5)",lognormal_hard="Log-normal (log SD = 1.25)")
  if(!is.null(assessment)) for(key in unique(mpr_cell_key(assessment))) {
    s <- assessment[mpr_cell_key(assessment)==key,]
    families <- unique(s$dgp_id); colors <- grDevices::hcl.colors(length(families),"Dark 3")
    layout(matrix(1:3,ncol=1),heights=c(5,1.1,.5))
    par(mar=c(4,5,3,1),xpd=FALSE)
    x <- -log10(1-s$threshold)
    plot(range(x),range(c(s$lower_bound,s$content_attainment,s$tolerance_confidence)),
      type="n",xaxt="n",xlab="DP posterior threshold (log scale for 1 - threshold)",
      ylab="Probability of attaining content c",
      main=paste0("Calibration: n = ",s$n[1],", c = ",s$content[1],", p = ",s$tolerance_confidence[1]))
    ticks <- sort(unique(c(seq.int(floor(min(x)),ceiling(max(x))),
      -log10(1-s$tolerance_confidence[1]))))
    tick_labels <- vapply(ticks,function(t)if(t<=4)format(1-10^-t,trim=TRUE,digits=6)
      else paste0("1 - 1e-",format(t,trim=TRUE)),character(1))
    axis(1,at=ticks,labels=tick_labels,cex.axis=.85)
    abline(h=s$tolerance_confidence[1],lty=2,col="#B2182B")
    for(i in seq_along(families)) {
      z <- s[s$dgp_id==families[i],];z <- z[order(z$threshold),]
      lines(-log10(1-z$threshold),z$content_attainment,col=colors[i],lwd=1.3)
      lines(-log10(1-z$threshold),z$lower_bound,col=colors[i],lty=3)
    }
    par(mar=c(0,0,0,0));plot.new()
    labels <- family_labels[families]
    labels[is.na(labels)] <- families[is.na(labels)]
    legend("center",legend=labels,col=colors,lty=1,bty="n",cex=.85,ncol=2)
    plot.new()
    legend("center",legend=c("Attainment","Simultaneous lower bound","Requested p"),
      lty=c(1,3,2),col=c("black","black","#B2182B"),bty="n",cex=.85,ncol=3)
  }
  if(!is.null(summary)) {
    layout(1)
    labels <- c(mti_profile_fractional_admissible_v1="Profiled MTI",
      mti_ecm_historical="Historical MTI-ECM",young_mathew="Young-Mathew",wilks="Wilks")
    colors <- c(mti_profile_fractional_admissible_v1="#0072B2",mti_ecm_historical="#CC79A7",
      young_mathew="#D55E00",wilks="black")
    labels[names(method_labels)] <- method_labels
    colors[names(method_colors)] <- method_colors
    for(family in unique(summary$dgp_id)) {
      s <- summary[summary$dgp_id==family,]
      cells <- unique(s[c("cell_index","n","content")]); cells <- cells[order(cells$n,cells$content),]
      methods <- unique(s$method); offsets <- seq(-.25,.25,length.out=length(methods))
      par(mfrow=c(2,1),mar=c(4,5,3,1),oma=c(1,0,2,0))
      for(quantity in c("attainment","width")) {
        limits <- if(quantity=="attainment")range(c(.85,1,s$attainment_lower)) else
          range(c(0,s$width_q025,s$width_q975),na.rm=TRUE)
        if(diff(limits)==0)limits <- limits+c(-.01,.01)
        plot(c(.5,nrow(cells)+.5),limits,type="n",xaxt="n",xlab="Sample size n / content c",
          ylab=if(quantity=="attainment")"Probability of attaining content c" else "Interval width")
        axis(1,at=seq_len(nrow(cells)),labels=paste(cells$n,cells$content,sep=" / "),cex.axis=.85)
        if(quantity=="attainment")abline(h=unique(s$tolerance_confidence),lty=2,col="#B2182B")
        for(i in seq_along(methods)) {
          z <- s[s$method==methods[i],];x <- match(z$cell_index,cells$cell_index)+offsets[i]
          lo <- if(quantity=="attainment")z$attainment_lower else z$width_q025
          hi <- if(quantity=="attainment")z$attainment_upper else z$width_q975
          value <- if(quantity=="attainment")z$attainment else z$width_mean
          segments(x,lo,x,hi,col=colors[methods[i]],lwd=1.5)
          points(x,value,pch=if(quantity=="attainment")16 else 4,col=colors[methods[i]],cex=.8)
        }
        if(quantity=="attainment")legend("bottomleft",legend=labels[methods],col=colors[methods],
          pch=16,lty=1,ncol=2,bty="n",cex=.8)
      }
      mtext(family,outer=TRUE,cex=1.1)
    }
  }
}

mpr_precision_reference <- function(calibration,design,cfg) {
  cells <- unique(design[c("n","content","tolerance_confidence")])
  cells <- cells[mpr_cell_key(cells) %in% mpr_cell_key(calibration[calibration$interpretation!="passed",]),]
  if(!nrow(cells))return(data.frame())
  confidence <- cfg$calibration$familywise_confidence
  rows <- list()
  for(i in seq_len(nrow(cells))) {
    cell <- cells[i,]
    # For continuous iid observations, sample-range content is Beta(n-1,2).
    reference <- stats::pbeta(cell$content,cell$n-1,2,lower.tail=FALSE)
    for(family in c("original_grid_assessment","new_fixed_policy_assessment")) {
      count <- nrow(design)*if(family=="original_grid_assessment")
        cfg$calibration$threshold_quantile_count+2L else 1L
      alpha <- (1-confidence)/count
      for(replications in cfg$calibration$replications*2^(0:7)) {
        critical <- stats::qbinom(1-alpha,replications,cell$tolerance_confidence)+1
        power <- if(critical>replications)0 else stats::pbinom(critical-1,replications,
          reference,lower.tail=FALSE)
        rows[[length(rows)+1L]] <- data.frame(cell,assessment=family,
          replications=replications,alpha=alpha,sample_range_reference_attainment=reference,
          required_successes=critical,reference_probability_of_passing=power,reference_only=TRUE)
      }
    }
  }
  do.call(rbind,rows)
}

mpr_write_markdown <- function(output,status,stages,calibration=NULL,summary=NULL) {
  lines <- c("# MTI Revision Assessment","",paste("Decision:",status$decision),"",
    "This report preserves the fitted study and all historical article inputs.","",
    "| Stage | Completed Fits | Expected Fits | Remaining Fits | State |",
    "| --- | ---: | ---: | ---: | --- |")
  for(i in seq_len(nrow(stages)))lines <- c(lines,sprintf("| %s | %d | %d | %d | %s |",
    stages$stage[i],stages$completed_datasets[i],stages$expected_datasets[i],
    stages$remaining_fits[i],stages$status[i]))
  if(!is.null(status$error))lines <- c(lines,"",paste("Integrity issue:",status$error))
  if(!is.null(calibration)) {
    lines <- c(lines,"","## Calibration","",
      "| n | c | p | Selected Threshold | Interpretation |","| ---: | ---: | ---: | ---: | --- |")
    for(i in seq_len(nrow(calibration))) {
      z <- calibration[i,]
      lines <- c(lines,sprintf("| %d | %.2f | %.2f | %s | %s |",z$n,z$content,
        z$tolerance_confidence,if(is.finite(z$threshold))sprintf("%.8g",z$threshold) else "Unresolved",
        z$interpretation))
    }
    unresolved <- calibration[calibration$interpretation!="passed",]
    if(nrow(unresolved)) {
      lines <- c(lines,"","### Unresolved Cells","",
        "The following diagnostic maxima are taken over the declared threshold grid with full observed returns in every family. They are not replacement policy settings.","",
        "| n | c | Worst-Family Attainment at Diagnostic Maximum | Simultaneous Lower Bound |",
        "| ---: | ---: | ---: | ---: |")
      for(i in seq_len(nrow(unresolved))) {
        z <- unresolved[i,]
        lines <- c(lines,sprintf("| %d | %.2f | %.4f | %.4f |",z$n,z$content,
          z$diagnostic_worst_attainment,z$diagnostic_worst_lower))
      }
    }
  }
  lines <- c(lines,"","## Interpretation","",
    "Content attainment uses all replications, including failures to return an interval.",
    "Binomial intervals quantify Monte Carlo uncertainty about this probability.",
    "Raw width ranges use the 2.5% and 97.5% empirical quantiles within each distribution and case.",
    "They describe variation across returned intervals, rather than uncertainty about the mean width.",
    "Paired width changes retain the number of jointly returned intervals.","",
    "An inconclusive confidence bound is distinct from a demonstrated shortfall or a numerical failure.",
    "The simultaneous validation bounds concern the prespecified finite panel; they do not establish distribution-free validity.",
    "No noninferiority margin is assumed, and this report cannot promote a method or replace manuscript inputs.")
  if(!is.null(calibration) && any(calibration$interpretation!="passed")) {
    lines <- c(lines,"","## Next Decision","",
      "Retain the all-cell validation stop. Do not loosen the criterion, replace missing thresholds, or retune from validation outcomes.",
      "First assess whether the unresolved cells have adequate point attainment but insufficient Monte Carlo precision.",
      "The precision-reference CSV uses the exact sample-range content law only as a planning reference.",
      "It does not assume that the revised MTI rule equals Wilks or has that known attainment probability.",
      "A future fixed-policy assessment would freeze one threshold per cell before fresh sampling, removing the threshold-search factor.",
      "Such a fresh assessment should cover the complete panel if one common confidence statement is desired.",
      "Do not combine selected old confidence bounds and repeated new assessments without an explicit error allocation.",
      "No extra replications or alternative threshold policy have been launched by this report.")
  }
  if(!is.null(summary))lines <- c(lines,"","## Validation Evidence","",
    paste("Primary evidence classifications:",paste(names(table(summary$evidence)),
      as.integer(table(summary$evidence)),collapse="; ")),
    "Review paired attainment and raw-width changes before deciding whether the numerical improvement warrants adoption.")
  writeLines(lines,file.path(output,"REPORT.md"),useBytes=TRUE)
}

mpr_build_report <- function(run,output,cfg,review,repo,config_path,review_path) {
  run <- normalizePath(run,mustWork=TRUE)
  output <- mpr_output_path(output)
  mpr_require(!mpr_active(run),"Study is active; use a health snapshot or queue the completion-dependent report.")
  mpr_require(!startsWith(paste0(output,"/"),paste0(run,"/")) &&
    !startsWith(paste0(run,"/"),paste0(output,"/")),"Report and study directories must be disjoint.")
  mpr_require(!isTRUE(review$automatic_promotion) && !isTRUE(review$modify_study_or_article_inputs) &&
    is.null(review$noninferiority_margin),"Unsupported report or promotion controls.")
  existing <- list.files(output,all.files=TRUE,no..=TRUE)
  mpr_require(all(existing %in% c("report.pid","report.log","report_job.lock","report_exit.json")),
    "Report directory contains earlier artifacts; choose a fresh output directory.")
  dir.create(output,recursive=TRUE,showWarnings=FALSE)
  historical <- c("main.tex","rqr-gibbs-supplement.tex","refs.bib",cfg$historical_policy,
    list.files(cfg$application_evidence,full.names=TRUE),
    list.files("tables",full.names=TRUE),list.files("figures/generated",full.names=TRUE))
  historical <- historical[file.exists(historical) & !dir.exists(historical)]
  protected <- pma_file_record(historical)
  report_sources <- pma_file_record(c("application/scripts/95_review_mti_profile_revision.R",
    "application/scripts/lib/mti_profile_review.R",review_path,config_path))
  status <- list(decision="pending",article_inputs_replaced=FALSE,
    report_source_clean=mpr_git_clean(pma_git("status","--porcelain")),R_version=R.version.string,
    checked_utc=format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC"))
  stages <- data.frame(stage=c("calibration","validation"),completed_datasets=0L,
    expected_datasets=NA_integer_,remaining_fits=NA_integer_,status="not_audited")
  calibration <- assessment <- summary <- NULL
  tryCatch({
    status$provenance <- mpr_provenance(run,cfg,repo,config_path)
    frozen <- jsonlite::read_json(cfg$validation_config,simplifyVector=FALSE)
    design <- mpr_design(frozen)
    calibration_completion <- mpr_read_json(file.path(run,"calibration","completion.json"))
    if(!is.null(calibration_completion))assessment <- read.csv(
      file.path(run,"calibration","threshold_assessment.csv"))
    data <- mpr_checkpoint_audit(run,"calibration",design,cfg,assessment)
    expected <- nrow(design)*(cfg$calibration$replications+
      cfg$calibration$threshold_development_replications)
    active <- mpr_active(run)
    stages <- mpr_stage_state(run,"calibration",nrow(data),expected,active)
    validation_tasks <- mpr_checkpoint_audit(run,"validation",design,cfg)
    stages <- rbind(stages,mpr_stage_state(run,"validation",nrow(validation_tasks),
      nrow(design)*cfg$validation$replications,active))
    pma_write_csv(data,file.path(output,"calibration_dataset_audit.csv"))
    if(nrow(data))pma_write_csv(attr(data,"checkpoint_records"),
      file.path(output,"calibration_checkpoint_manifest.csv"))
    errors <- if(nrow(data))sum(data$task_errors) else 0
    failures <- if(nrow(data))sum(data$numerical_failed_candidates>0) else 0
    status$calibration_task_errors <- errors
    status$calibration_numerical_failure_datasets <- failures
    complete <- mpr_read_json(file.path(run,"calibration","completion.json"))
    if(!is.null(complete) && !isTRUE(complete$passed) && stages$completed_datasets[2]==0)
      stages$status[2] <- "not_authorized_after_calibration"
    if(!is.null(complete)) {
      policy <- read.csv(file.path(run,"calibration","policy.csv"))
      calibration <- mpr_calibration_check(assessment,policy,design,cfg,review,
        attr(data,"development_probabilities"),attr(data,"independent_threshold_counts"))
      mpr_require(identical(isTRUE(complete$passed),errors==0 && failures==0 &&
        all(calibration$interpretation=="passed")),"Calibration decision conflicts with audited counts.")
      pma_write_csv(calibration,file.path(output,"calibration_cell_assessment.csv"))
      pma_write_csv(assessment,file.path(output,"calibration_threshold_assessment.csv"))
      reference <- mpr_precision_reference(calibration,design,cfg)
      if(nrow(reference))pma_write_csv(reference,file.path(output,"precision_reference.csv"))
      status$calibrated_cells <- sum(calibration$interpretation=="passed")
      status$expected_cells <- nrow(calibration)
    }
    status$decision <- if(errors>0 || failures>0)"numerical_or_task_issue" else
      if(is.null(complete))"calibration_pending" else
      if(!isTRUE(complete$passed))"calibration_unresolved_validation_not_authorized" else
        "validation_pending"
    validation <- mpr_read_json(file.path(run,"validation","completion.json"))
    if(!is.null(validation)) {
      mpr_require(isTRUE(complete$passed),"Validation followed an unresolved calibration.")
      mpr_require(isTRUE(validation$passed) && identical(validation$frozen_policy_sha256,
        mp_sha(file.path(run,"calibration","policy.csv"))),"Validation policy changed or stage failed.")
      z <- read.csv(file.path(run,"validation","results.csv"))
      generators <- mp_dgp_environment()
      family_meta <- setNames(frozen$dgps,vapply(frozen$dgps,`[[`,character(1),"dgp_id"))
      cdfs <- lapply(family_meta,function(meta)generators$dgp_meta(meta)$p)
      mpr_validate_results(z,design,cfg,cdfs)
      mpr_require(nrow(validation_tasks)>0 && !any(validation_tasks$task_errors>0),
        "Validation checkpoints contain task failures.")
      summary <- mpr_validation_summary(z,design,cfg,review)
      paired <- mpr_paired_summary(z,review)
      pma_write_csv(summary,file.path(output,"validation_family_case_summary.csv"))
      pma_write_csv(paired,file.path(output,"validation_paired_comparisons.csv"))
      primary <- summary[summary$method==review$primary_method,]
      status$decision <- if(all(primary$evidence=="strong_panel_evidence"))
        "ready_for_scientific_review_not_automatic_promotion" else
          "validation_requires_scientific_review"
    }
  },error=function(e) {
    status$decision <<- "integrity_issue"
    status$error <<- conditionMessage(e)
  })
  pma_write_csv(stages,file.path(output,"progress.csv"))
  mpr_write_markdown(output,status,stages,calibration,summary)
  mpr_report_pdf(output,status,calibration,assessment,summary)
  mpr_require(identical(protected,pma_file_record(protected$path)),"Historical article inputs changed.")
  status$report_source_commit <- pma_git("rev-parse","HEAD")
  status$review_specification_sha256 <- mp_sha(review_path)
  status$study_config_sha256 <- mp_sha(config_path)
  mpr_require(identical(report_sources,pma_file_record(report_sources$path)),
    "Report calculation source changed during assessment.")
  status$report_sources <- report_sources
  status$input_records <- pma_file_record(c(file.path(run,"identity.rds"),
    file.path(run,"manifest.json"),list.files(file.path(run,"calibration"),
      pattern="[.]csv$|completion[.]json$",full.names=TRUE)))
  status$output_records <- pma_file_record(list.files(output,
    pattern="[.]csv$|[.]pdf$|REPORT[.]md$",full.names=TRUE))
  pta_atomic_write_json(status,file.path(output,"assessment.json"))
  if(identical(status$decision,"integrity_issue"))stop(status$error,call.=FALSE)
  status
}
