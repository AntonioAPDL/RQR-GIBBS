# This experiment changes eligibility thresholds, not the fixed-target model.
mec_validate <- function(cfg) {
  mwd_require(cfg$prior_sd == 2 && cfg$learning_rate == 1 && cfg$radius_multiplier == 1 &&
    cfg$coordinates$center == 0 && cfg$coordinates$scale == 1 &&
    identical(cfg$coordinates$estimated_from_analysis_sample, FALSE) &&
    cfg$dp_concentration == 1 && cfg$dp_base_sd == 4 &&
    cfg$development_replications == 100 && cfg$validation_replications == 1000 && cfg$expected_cells == 72 &&
    cfg$maximum_workers >= 1 && cfg$maximum_workers <= 40 &&
    setequal(cfg$rules, c("point_target", "compatible_target", "closest_nominal")) &&
    cfg$compatibility_error > 0 && cfg$compatibility_error < 1 &&
    cfg$report_error > 0 && cfg$report_error < 1 &&
    isTRUE(cfg$require_nonexpanding_development_mean_widths) &&
    !cfg$sample_gibbs && !cfg$automatic_article_replacement && !cfg$application_transfer,
    "The fixed-prior, ECM-only exploratory specification changed.")
  invisible(TRUE)
}

mec_primary <- function(context, n, content) {
  p <- context$policy[context$policy$n == n & context$policy$content == content, , drop = FALSE]
  mwd_require(nrow(p) == 1, "Primary ECM policy is missing or duplicated.")
  gamma <- p$posterior_confidence %||% p$screen
  mwd_require(length(gamma) == 1 && is.finite(gamma) && gamma > 0 && gamma < 1,
    "Invalid primary ECM probability cutoff.")
  as.numeric(gamma)
}

mec_development_tasks <- function(path,expected_hash,reconstruct) {
  if(file.exists(path))tasks <- readRDS(path) else {
    tasks <- reconstruct()
    mwd_require(identical(mwd_hash(tasks),expected_hash),"Canonical reconstructed task hash differs.")
    temporary <- tempfile(".ecm-tasks-",tmpdir=dirname(path));on.exit(unlink(temporary))
    saveRDS(tasks,temporary)
    mwd_require(identical(mwd_hash(readRDS(temporary)),expected_hash),"Canonical task serialization changed.")
    mwd_require(file.rename(temporary,path),"Cannot preserve canonical development tasks.")
  }
  mwd_require(identical(mwd_hash(tasks),expected_hash),"Canonical saved task hash differs.")
  tasks
}

mec_upper <- function(successes, samples, error) {
  samples <- rep(samples,length.out=length(successes))
  mwd_require(all(is.finite(successes) & is.finite(samples) & samples >= 1 &
    successes >= 0 & successes <= samples & successes == round(successes) & samples == round(samples)) &&
    length(error)==1 && is.finite(error) && error>0 && error<1, "Invalid binomial counts or error probability.")
  out <- rep(1, length(successes)); i <- successes < samples
  out[i] <- qbeta(1-error, successes[i]+1, samples[i]-successes[i])
  out
}

mec_curve <- function(paths, reference_widths, reference_gamma, content, confidence, error) {
  regions <- mjc_regions(paths, reference_gamma)
  laws <- unique(vapply(paths, function(z) z$response_id[1], ""))
  width <- attainment <- upper <- returned <- matrix(NA_real_, nrow(regions), length(laws))
  counts <- numeric(length(laws))
  for (j in seq_along(laws)) {
    pieces <- paths[vapply(paths, function(z) z$response_id[1] == laws[j], FALSE)]
    counts[j] <- length(pieces)
    mwd_require(counts[j] > 0 && all(vapply(pieces,nrow,1L)>0), "An ECM development path is empty.")
    initial_width <- sum(vapply(pieces,function(z)z$width[1],0))
    initial_success <- sum(vapply(pieces,function(z)as.numeric(z$population_content[1]>=content),0))
    events <- data.table::rbindlist(lapply(pieces,function(z)data.frame(
      breakpoint=z$posterior_content_probability, dw=diff(c(z$width,0)),
      da=diff(c(as.numeric(z$population_content>=content),0)), dr=c(rep(0,nrow(z)-1L),-1))))
    events <- events[,.(dw=sum(dw),da=sum(da),dr=sum(dr)),by=breakpoint][order(breakpoint)]
    index <- findInterval(regions$gamma,events$breakpoint,left.open=TRUE)
    successes <- round(initial_success+c(0,cumsum(events$da))[index+1L])
    width[,j] <- (initial_width+c(0,cumsum(events$dw))[index+1L])/counts[j]/reference_widths[[laws[j]]]
    attainment[,j] <- successes/counts[j]
    upper[,j] <- mec_upper(successes,rep(counts[j],length(successes)),error)
    returned[,j] <- (counts[j]+c(0,cumsum(events$dr))[index+1L])/counts[j]
  }
  complete <- apply(returned,1,min)==1
  width[!complete,] <- Inf
  cbind(regions,complete=complete,laws=length(laws),samples_per_law=min(counts),
    worst_ratio=apply(width,1,max),mean_log_ratio=rowMeans(log(width)),
    minimum_attainment=apply(attainment,1,min),maximum_attainment=apply(attainment,1,max),
    maximum_absolute_deviation=apply(abs(attainment-confidence),1,max),
    mean_absolute_deviation=rowMeans(abs(attainment-confidence)),minimum_upper_bound=apply(upper,1,min),
    reference_threshold=regions$gamma==reference_gamma)
}

mec_select_rules <- function(curve, confidence, cfg) {
  reference <- curve[curve$reference_threshold,,drop=FALSE]
  mwd_require(nrow(reference)==1 && reference$complete, "The actual primary ECM reference is incomplete.")
  eligible <- curve[curve$complete & is.finite(curve$worst_ratio) &
    (curve$reference_threshold | curve$worst_ratio <= 1+64*.Machine$double.eps),,drop=FALSE]
  reference$method <- "ecm_primary"; reference$available <- TRUE; reference$reason <- ""
  out <- list(reference)
  for (rule in cfg$rules) {
    d <- eligible
    if(rule=="point_target")d <- d[d$minimum_attainment>=confidence-1e-12,,drop=FALSE]
    if(rule=="compatible_target")d <- d[d$minimum_upper_bound>=confidence-1e-12,,drop=FALSE]
    if(nrow(d)) {
      score <- if(rule=="closest_nominal")
        list(signif(d$maximum_absolute_deviation,12),signif(d$mean_absolute_deviation,12),signif(d$worst_ratio,12),
          signif(d$mean_log_ratio,12),!d$reference_threshold,-d$region_length,-d$gamma) else
        list(signif(d$worst_ratio,12),signif(d$mean_log_ratio,12),
          signif(d$maximum_absolute_deviation,12),!d$reference_threshold,-d$region_length,-d$gamma)
      row <- d[do.call(order,score)[1],,drop=FALSE]
      row$available <- TRUE;row$reason <- ""
    } else {
      row <- reference[,setdiff(names(reference),c("method","available","reason")),drop=FALSE]
      for(name in names(row))row[[name]] <- NA
      row$available <- FALSE;row$reason <- "no_complete_nonexpanding_development_rule"
    }
    row$method <- paste0("ecm_",rule);out[[length(out)+1L]] <- row
  }
  data.table::rbindlist(out,fill=TRUE)
}

mec_freeze <- function(policy,tasks,directory,commit) {
  frozen <- file.path(directory,"frozen_selection.rds")
  if(file.exists(frozen))mwd_require(identical(serialize(readRDS(frozen),NULL),serialize(policy,NULL)),
    "Frozen ECM rules changed.") else saveRDS(policy,frozen)
  tasks_file <- file.path(directory,"validation_tasks.rds")
  if(file.exists(tasks_file))mwd_require(identical(readRDS(tasks_file),tasks),"Frozen assessment tasks changed.") else
    saveRDS(tasks,tasks_file)
  d <- as.data.frame(policy)
  freeze <- list(selection_sha256=mwd_file_hash(frozen),tasks_sha256=mwd_file_hash(tasks_file),
    source_commit=commit,frozen_before_fresh_data=TRUE,development_only_selection=TRUE,
    automatic_article_replacement=FALSE,
    unavailable_rules=d[!d$available,c("n","content","method","reason"),drop=FALSE])
  path <- file.path(directory,"frozen_policy.json")
  if(file.exists(path)) {
    old <- mwd_json(path)
    mwd_require(old$selection_sha256==freeze$selection_sha256 && old$tasks_sha256==freeze$tasks_sha256 &&
      old$source_commit==commit,"Frozen policy or independent task hashes changed.")
  } else {
    mwd_require(!length(Sys.glob(file.path(directory,"validation/fits/*.rds"))),
      "Cannot declare a rule frozen after fresh fitting has started.")
    mgfp_json_write(freeze,path)
  }
  freeze
}

mec_profile <- function(design, tasks, source, context, cfg) {
  selected <- tasks[tasks$n==design$n & tasks$content==design$content,,drop=FALSE]
  primary <- mec_primary(context,design$n,design$content)
  paths <- list();widths <- list();receipts <- character(nrow(selected))
  for(i in seq_len(nrow(selected))) {
    task <- selected[i,,drop=FALSE]
    path <- file.path(source$output_root,"modes/fits",sprintf("fit_%06d.rds",task$job_id))
    value <- mbc_read_fit(path,source$mode_identity,task)
    anchor <- value$anchors[["2"]]
    set.seed(task$data_seed);y <- context$laws[[task$response_id]]$r(task$n)
    mwd_require(isTRUE(value$ok) && anchor$prior$prior_sd==cfg$prior_sd &&
      identical(anchor$sample_sha256,mwd_hash(y)) && all(anchor$candidates$numerically_accurate),
      "A saved development table has different observations, prior, or numerical accuracy.")
    reference <- mdc_select(anchor$candidates,primary)
    mwd_require(nrow(reference)==1,"Primary ECM returned no development interval.")
    p <- mdc_path(anchor$candidates);p$response_id <- task$response_id
    p$population_content <- context$laws[[task$response_id]]$p(p$upper)-context$laws[[task$response_id]]$p(p$lower)
    paths[[i]] <- p; widths[[i]] <- data.frame(response_id=task$response_id,width=reference$width)
    receipts[i] <- mwd_file_hash(paste0(path,".json"))
  }
  mwd_require(nrow(selected)==8*cfg$development_replications,"Incomplete development design.")
  baseline <- data.table::rbindlist(widths)[,.(width=mean(width)),by=response_id]
  curve <- mec_curve(paths,setNames(baseline$width,baseline$response_id),primary,
    design$content,design$tolerance_confidence,cfg$compatibility_error)
  rule <- mec_select_rules(curve,design$tolerance_confidence,cfg)
  for(name in c("n","content","tolerance_confidence")) {
    curve[[name]] <- design[[name]];rule[[name]] <- design[[name]]
  }
  list(ok=TRUE,task=design,rows=curve,selection=rule,reused_datasets=nrow(selected),
    reused_receipts_sha256=mwd_hash(receipts),primary_gamma=primary)
}

mec_design <- function(cfg,context,development,source_tasks) {
  tasks <- mps_design(list(expected_cells=cfg$expected_cells,replications_per_cell=cfg$validation_replications,
    data_seed_base=cfg$data_seed_base,kappa_grid=cfg$prior_sd),context)
  tasks$ecm_seed <- as.integer(cfg$ecm_seed_base+10*tasks$job_id)
  paths <- unique(c(context$parent,context$boundary,source_tasks$output_root,
    source_tasks$dependency$output_root))
  occupied <- c(development$data_seed,development$ecm_seed)
  for(directory in paths)for(stage in c("development","calibration","validation","pilot","shortlist","confirmation")) {
    file <- file.path(directory,stage,"tasks.csv")
    if(file.exists(file)) {
      old <- data.table::fread(file)
      for(field in intersect(names(old),c("data_seed","ecm_seed","seed")))occupied <- c(occupied,old[[field]])
    }
  }
  historical <- mwd_json(mwd_json("application/config/mti_gibbs_joint_20261117.json")$predecessor_config)
  original <- mwd_json(historical$boundary_config)
  old_jobs <- file.path(mgfp_common(),mwd_json(original$diagnostic_config)$campaign,"jobs.rds")
  if(file.exists(old_jobs)) {
    jobs <- readRDS(old_jobs);occupied <- c(occupied,jobs$data_seed,jobs$seed)
  }
  seeds <- c(tasks$data_seed,tasks$ecm_seed)
  mwd_require(all(is.finite(seeds)&seeds>0&seeds<.Machine$integer.max) && !anyDuplicated(seeds) &&
    !length(intersect(seeds,occupied)),"The independent ECM seed allocation overlaps earlier streams.")
  list(tasks=tasks,audit=list(fresh_tasks_sha256=mwd_hash(tasks),streams=length(seeds),
    occupied_seed_set_sha256=mwd_hash(sort(unique(occupied))),fresh_seed_set_sha256=mwd_hash(sort(seeds)),
    no_recorded_overlap=TRUE))
}

mec_fit <- function(y,task,cfg,context) {
  rule <- context$policy[context$policy$n==task$n & context$policy$content==task$content,,drop=FALSE]
  result <- mfp_fit_data(y,task$content,task$tolerance_confidence,task$retained_fraction,cfg$prior_sd,
    context$prior$simulation_reference,context$profile,context$dp,rule,context$prior$numerical,
    list(),task$ecm_seed,context$policy_diagnostics,sample_gibbs=FALSE)
  mwd_require(result$unresolved_candidates==0 &&
    result$prior$prior_sd==cfg$prior_sd && !result$prior$data_adaptive_prior &&
    identical(result$sample_sha256,mwd_hash(y)) && all(result$candidates$numerically_accurate),
    paste("An ECM-only candidate fit requires numerical review:",result$reason %||% "incomplete candidates"))
  if(!isTRUE(result$returned))mwd_require(identical(result$reason,"no_accurate_eligible_candidate"),
    "The ECM-only fit failed for a reason other than candidate eligibility.")
  result
}

mec_intervals <- function(y,task,anchor,policy,cfg,controls) {
  selected <- policy[policy$n==task$n & policy$content==task$content & policy$available,,drop=FALSE]
  mwd_require(nrow(selected)>=1 && "ecm_primary" %in% selected$method,"No frozen primary rule.")
  rows <- lapply(seq_len(nrow(selected)),function(i) {
    rule <- selected[i,,drop=FALSE];z <- mdc_select(anchor$candidates,rule$gamma)
    returned <- nrow(z)==1
    accurate <- if(returned)mfp_accuracy(y,c(z$lower,z$upper),z$target_content,z$mean_tilt,
      cfg$prior_sd^2,controls)$pass else TRUE
    mwd_require(accurate,"Selected ECM roots fail the unchanged final numerical criteria.")
    data.frame(method=rule$method,gamma=rule$gamma,returned=returned,
      lower=if(returned)z$lower else NA_real_,upper=if(returned)z$upper else NA_real_,
      width=if(returned)z$width else NA_real_,q=if(returned)z$target_content else NA_real_,
      delta=if(returned)z$mean_tilt else NA_real_,candidate_index=if(returned)z$candidate_index else NA_integer_,
      dp_probability=if(returned)z$posterior_content_probability else NA_real_,numerically_accurate=accurate,
      failure_reason=if(returned)"" else "no_eligible_candidate",prior_sd=cfg$prior_sd,radius_multiplier=1)
  })
  data.table::rbindlist(rows)
}

mec_job <- function(task,policy,cfg,context) {
  set.seed(task$data_seed);y <- context$laws[[task$response_id]]$r(task$n)
  anchor <- mec_fit(y,task,cfg,context)
  rows <- mec_intervals(y,task,anchor,policy,cfg,context$prior$numerical)
  benchmark <- data.table::as.data.table(mfp_comparators(y,task$content,task$tolerance_confidence))
  mwd_require(all(benchmark$returned & is.finite(benchmark$lower) & is.finite(benchmark$upper) & benchmark$width>0),
    "A classical comparator failed in the prespecified feasible design.")
  benchmark[,numerically_accurate:=TRUE]
  rows <- data.table::rbindlist(list(rows,benchmark),fill=TRUE)
  rows[,population_content:=context$laws[[task$response_id]]$p(upper)-context$laws[[task$response_id]]$p(lower)]
  rows[,attained:=returned & population_content>=task$content]
  for(name in c("job_id","response_id","n","content","tolerance_confidence","replication","data_seed"))
    data.table::set(rows,j=name,value=task[[name]])
  list(ok=TRUE,task=task,rows=rows,sample_sha256=mwd_hash(y),candidates=anchor$candidates,
    baseline_selected_ecm_trace=anchor$selected_ecm_trace,baseline_selected=anchor$selected,
    prior=anchor$prior,unresolved_candidates=anchor$unresolved_candidates)
}

mec_summary <- function(rows,cfg) {
  d <- data.table::as.data.table(rows)
  size <- nrow(unique(d[,.(n,content,response_id,method)]))
  d[,{
    successes <- sum(attained);m <- .N;w <- width[returned & is.finite(width)]
    .(samples=m,returned=sum(returned),attained=successes,attainment=successes/m,
      mcse=sqrt((successes/m)*(1-successes/m)/m),
      lower=if(successes==0)0 else qbeta(cfg$report_error/2,successes,m-successes+1),
      upper=if(successes==m)1 else qbeta(1-cfg$report_error/2,successes+1,m-successes),
      simultaneous_lower=if(successes==0)0 else qbeta(cfg$report_error/(2*size),successes,m-successes+1),
      simultaneous_upper=if(successes==m)1 else qbeta(1-cfg$report_error/(2*size),successes+1,m-successes),
      mean_width=if(length(w))mean(w) else NA_real_,
      width_q025=if(length(w))unname(quantile(w,.025)) else NA_real_,
      width_q975=if(length(w))unname(quantile(w,.975)) else NA_real_)
  },by=.(n,content,tolerance_confidence,response_id,method)]
}

mec_pairs <- function(rows) {
  d <- data.table::as.data.table(rows)
  reference <- d[method=="ecm_primary",.(job_id,reference_width=width,reference_attained=attained,reference_returned=returned)]
  pairs <- merge(d[method!="ecm_primary"],reference,by="job_id")
  pairs[,{
    good <- returned & reference_returned;difference <- width[good]-reference_width[good]
    se <- if(length(difference)>1)sd(difference)/sqrt(length(difference)) else NA_real_
    .(samples=.N,complete_pairs=sum(good),mean_width_change=if(length(difference))mean(difference) else NA_real_,
      width_change_mcse=se,width_change_lower=if(length(difference)>1)mean(difference)-qt(.975,length(difference)-1)*se else NA_real_,
      width_change_upper=if(length(difference)>1)mean(difference)+qt(.975,length(difference)-1)*se else NA_real_,
      new_attainment=mean(attained),reference_attainment=mean(reference_attained),
      newly_attained=sum(attained & !reference_attained),lost_attainment=sum(!attained & reference_attained))
  },by=.(n,content,response_id,method)]
}

mec_report <- function(rows,cfg) {
  d <- data.table::as.data.table(rows);summary <- mec_summary(d,cfg)
  paired <- mec_pairs(d)
  mwd_write(summary,file.path(cfg$output_root,"validation/comparison_summary.csv"))
  mwd_write(paired,file.path(cfg$output_root,"validation/paired_comparison.csv"))
  labels <- c(ecm_primary="Primary ECM",ecm_point_target="ECM: point target",ecm_compatible_target="ECM: compatible",
    ecm_closest_nominal="ECM: closest nominal",young_mathew="Young-Mathew",wilks="Wilks")
  colors <- c(ecm_primary="#0072B2",ecm_point_target="#CC79A7",ecm_compatible_target="#C68500",
    ecm_closest_nominal="#009E73",young_mathew="#D55E00",wilks="black")
  grDevices::pdf(file.path(cfg$output_root,"validation/exploratory_comparisons.pdf"),width=14,height=10,useDingbats=FALSE)
  on.exit(grDevices::dev.off())
  designs <- unique(summary[,.(n,content)])
  for(i in seq_len(nrow(designs)))for(kind in c("width","attainment")) {
    cells <- summary[n==designs$n[i] & content==designs$content[i]]
    laws <- unique(cells$response_id)
    for(group in split(laws,ceiling(seq_along(laws)/4))) {
      par(mfrow=c(2,2),mar=c(4.5,10,3,1.5),oma=c(3,0,3,0),mgp=c(2.7,.8,0),las=1)
      for(law in group) {
        z <- cells[response_id==law];z <- z[order(match(method,names(labels)))];y <- rev(seq_len(nrow(z)))
        estimate <- if(kind=="width")z$mean_width else z$attainment
        lo <- if(kind=="width")z$width_q025 else z$lower
        hi <- if(kind=="width")z$width_q975 else z$upper
        limits <- if(kind=="width")range(c(lo,hi),finite=TRUE) else c(0,1.005)
        plot(estimate,y,type="n",xlim=limits,ylim=c(.5,nrow(z)+.5),yaxt="n",ylab="",
          xlab=if(kind=="width")"Interval width" else "Probability of attaining required population content",
          main=gsub("_"," ",law),cex.main=1)
        abline(h=y,col="grey92");if(kind=="attainment")abline(v=cells$tolerance_confidence[1],lty=2,col="grey35")
        segments(lo,y,hi,y,col=colors[z$method],lwd=2)
        points(estimate,y,pch=if(kind=="width")4 else 16,col=colors[z$method],cex=1.1)
        axis(2,at=y,labels=labels[z$method],tick=FALSE,cex.axis=.9)
      }
      mtext(sprintf("Exploratory ECM assessment: n = %d, c = %.2f, p = %.2f",designs$n[i],designs$content[i],cells$tolerance_confidence[1]),
        outer=TRUE,side=3,line=1,font=2)
      mtext(if(kind=="width")"Bars: empirical 2.5th--97.5th width percentiles; crosses: means." else
        "Bars: pointwise exact 95% confidence intervals; dots: empirical attainment.",outer=TRUE,side=1,line=1)
    }
  }
  invisible(list(summary=summary,paired=paired))
}

mec_verify <- function(cfg,context) {
  directory <- cfg$output_root
  manifest <- mwd_json(file.path(directory,"manifest.json"))$identity
  mgfp_check_hashes(manifest$programs);mgfp_check_hashes(manifest$article_files)
  freeze <- mwd_json(file.path(directory,"frozen_policy.json"))
  mwd_require(freeze$frozen_before_fresh_data && freeze$development_only_selection &&
    !freeze$automatic_article_replacement &&
    freeze$selection_sha256==mwd_file_hash(file.path(directory,"frozen_selection.rds")) &&
    freeze$tasks_sha256==mwd_file_hash(file.path(directory,"validation_tasks.rds")),"Frozen assessment inputs changed.")
  tasks <- readRDS(file.path(directory,"validation_tasks.rds"))
  policy <- readRDS(file.path(directory,"frozen_selection.rds"))
  d <- data.table::fread(file.path(directory,"validation/interval_results.csv"))
  mwd_require(nrow(tasks)==cfg$expected_cells*cfg$validation_replications &&
    !anyDuplicated(d[,.(job_id,method)]) && setequal(d$job_id,tasks$job_id) &&
    all(d$numerically_accurate) && all(d$attained==(d$returned & d$population_content>=d$content)),
    "Independent ECM results are missing, duplicated, or numerically unresolved.")
  expected <- merge(unique(data.table::as.data.table(tasks)[,.(n,content,response_id)]),
    policy[available==TRUE,.(n,content,method)],by=c("n","content"),allow.cartesian=TRUE)
  base <- unique(data.table::as.data.table(tasks)[,.(n,content,response_id)])
  classical <- data.table::rbindlist(lapply(c("young_mathew","wilks"),function(method_name) {
    z <- data.table::copy(base);z[,method:=method_name];z
  }))
  expected <- data.table::rbindlist(list(expected,classical),use.names=TRUE)
  actual <- d[,.(samples=.N),by=.(n,content,response_id,method)]
  key <- function(z)paste(z$n,z$content,z$response_id,z$method)
  mwd_require(setequal(key(expected),key(actual)) && all(actual$samples==cfg$validation_replications),
    "An unavailable rule was substituted or a method/distribution count changed.")
  d[,reconstructed:=context$laws[[.BY$response_id]]$p(upper)-context$laws[[.BY$response_id]]$p(lower),by=response_id]
  mwd_require(all(is.finite(d$population_content[d$returned])) &&
    max(abs(d$reconstructed-d$population_content),na.rm=TRUE)<1e-12 &&
    max(abs(d$width-(d$upper-d$lower)),na.rm=TRUE)<1e-10,"Population contents or widths do not reproduce.")
  observed <- data.table::fread(file.path(directory,"validation/comparison_summary.csv"))
  reproduced <- mec_summary(d,cfg)
  keys <- c("n","content","response_id","method")
  data.table::setorderv(observed,keys);data.table::setorderv(reproduced,keys)
  mwd_require(isTRUE(all.equal(as.data.frame(observed),as.data.frame(reproduced),check.attributes=FALSE,tolerance=1e-10)),
    "The independent ECM summary does not reproduce.")
  paired <- data.table::fread(file.path(directory,"validation/paired_comparison.csv"))
  reproduced_pairs <- mec_pairs(d)
  data.table::setorderv(paired,keys);data.table::setorderv(reproduced_pairs,keys)
  mwd_require(isTRUE(all.equal(as.data.frame(paired),as.data.frame(reproduced_pairs),check.attributes=FALSE,tolerance=1e-10)),
    "The independent paired comparison does not reproduce.")
  mgfp_json_write(list(passed=TRUE,datasets=nrow(tasks),interval_rows=nrow(d),method_cases=nrow(actual),
    maximum_content_error=max(abs(d$reconstructed-d$population_content),na.rm=TRUE),
    article_unchanged=TRUE,no_gibbs_sampling=TRUE,prior_uses_analysis_data=FALSE,
    frozen_rules_sha256=freeze$selection_sha256,time=mgfp_time()),file.path(directory,"verification.json"))
  cat("Independent ECM exploration evidence checks passed.\n")
  invisible(TRUE)
}
