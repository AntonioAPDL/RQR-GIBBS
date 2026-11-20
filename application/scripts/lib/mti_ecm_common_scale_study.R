msc_validate <- function(cfg) {
  mwd_require(cfg$prior_sd==2 && cfg$coordinates$center==0 && cfg$coordinates$scale==1 &&
    identical(cfg$coordinates$estimated_from_analysis_sample,FALSE) && cfg$dp_concentration==1 &&
    cfg$dp_base_sd==4 && cfg$radius_multiplier==1 && !cfg$sample_gibbs &&
    !cfg$automatic_article_replacement && !cfg$application_transfer && cfg$expected_cells==72 &&
    cfg$screening_replications==12 && cfg$maximum_workers>=1 && cfg$maximum_workers<=40 &&
    identical(names(cfg$scale_ranges),c("narrow","primary","broad")),"The independent common-scale specification changed.")
  for (r in cfg$scale_ranges) mwd_require(length(r)==2 && all(is.finite(r)) && r[1]>0 && r[1]<1 && r[2]>1,
    "Each common-scale prior requires proper finite support containing rate one.")
  invisible(TRUE)
}

msc_context <- function(cfg,directory) {
  pointer <- mwd_json(file.path(mgfp_common(),".codex_work/mti_ecm_nominal_latest.json"))
  original <- mwd_json(file.path(pointer$output_root,"manifest.json"))$identity
  final <- mwd_json(file.path(mgfp_common(),".codex_work/mti_ecm_nominal_finalized_latest.json"))
  verified <- mwd_json(file.path(final$output_root,"verification.json"))
  mwd_require(pointer$source_commit==cfg$source_assessment_commit &&
    final$analysis_commit==cfg$source_analysis_commit && verified$passed &&
    verified$datasets==72000 && verified$prior_uses_analysis_data==FALSE,
    "The completed assessment or its independent-prior evidence differs.")
  source <- original$source
  prior<-mwd_json("application/config/mti_fixed_prior_20261114.json")
  context<-mps_context(prior,directory)
  mwd_require(identical(context$generator_sha256,original$generators) &&
    prior$primary_kappa==cfg$prior_sd && !prior$simulation_reference$estimated_from_analysis_sample,
    "The frozen generator or independent root prior differs.")
  # A new native algorithm has its own attestation; old receipts retain theirs.
  context$package<-mps_package_check()
  common<-new.env()
  sys.source("application/R/rqr_ecm_common_scale.R",common)
  for(name in ls(common,all.names=TRUE))mwd_require(
    identical(deparse(common[[name]]),deparse(getFromNamespace(name,"rqrgibbs"))),
    paste("Installed common-scale computation differs:",name))
  context$package$common_scale_source_sha256<-mwd_file_hash("application/R/rqr_ecm_common_scale.R")
  context$prior<-prior
  context$source <- source
  context$development <- readRDS(file.path(pointer$output_root,"source_development_tasks.rds"))
  mwd_require(nrow(context$development)==7200 && context$dp$concentration==1 &&
    context$dp$base$sd==4,"The reused development panel or DP prior differs.")
  context
}

msc_fit <- function(y,task,cfg,context,bounds) {
  rule <- context$policy[context$policy$n==task$n & context$policy$content==task$content,,drop=FALSE]
  mwd_require(nrow(rule)==1,"A primary ECM design rule is missing.")
  menu <- rqrgibbs::rqr_mti_ecm_adaptive_profile_menu(y,task$content,task$tolerance_confidence,
    scan_target_content=task$retained_fraction,policy="cell",policy_config=context$profile,calibration_rule=as.list(rule))
  ec <- context$profile$ecm_control;ec$max_iter<-cfg$initial_iterations;ec$multistart<-FALSE
  ec$ecm_backend<-"cpp";ec$seed<-task$ecm_seed;ec$store_iteration_trace<-TRUE
  ec$scale_control<-list(log_rate_bounds=log(bounds),update=cfg$update,polish_cycles=cfg$polish_cycles)
  prior<-rqrgibbs::beta_prior("ridge",ridge=list(tau2=cfg$prior_sd^2))
  action<-rqrgibbs::rqr_mti_ecm_dp_profile_action(y,task$content,menu$posterior_confidence,
    dp_concentration=cfg$dp_concentration,dp_base_measure=rqrgibbs::rqr_dp_base_normal(0,cfg$dp_base_sd),
    strict_bayes=TRUE,q_grid=menu$q_grid,tilt_grid_control=menu$tilt_grid_control,
    beta_prior_obj=prior,ecm_control=ec,expand_if_empty=FALSE,return_candidate_traces=TRUE)
  d<-action$candidates;trace<-action$candidate_traces;trace$attempt<-1L
  bad<-which(!d$finite_interval | !d$numerically_accurate)
  if(length(bad)) {
    ec$max_iter<-cfg$continuation_iterations
    ec$scale_control$polish_cycles<-cfg$continuation_polish_cycles
    rowfit<-getFromNamespace(".rqr_mti_profile_ecm_row","rqrgibbs")
    centerfit<-getFromNamespace(".rqr_mti_profile_tilt_values","rqrgibbs")
    for(i in bad) {
      center<-do.call(centerfit,c(list(y=y,target_content=d$target_content[i]),menu$tilt_grid_control))
      result<-rowfit(y,matrix(1,length(y),1),d$target_content[i],d$mean_tilt[i],center,
        1,prior,ec,d$candidate_index[i],return_trace=TRUE)
      for(name in intersect(names(result$row),names(d)))d[[name]][i]<-result$row[[name]]
      if(nrow(result$trace)){result$trace$attempt<-2L;trace<-data.table::rbindlist(list(trace,result$trace),fill=TRUE)}
    }
  }
  finite<-which(d$finite_interval)
  probability<-rqrgibbs::rqr_dp_content_probability(action$posterior_distribution_fit,
    d$lower[finite],d$upper[finite],task$content)
  d$posterior_content_probability[finite]<-probability$posterior_content_probability
  d$posterior_constraint_satisfied<-d$numerically_accurate & is.finite(d$posterior_content_probability) &
    d$posterior_content_probability>=menu$posterior_confidence
  selected<-mdc_select(d,menu$posterior_confidence)
  if(nrow(selected))trace<-trace[trace$candidate_index==selected$candidate_index,,drop=FALSE]
  else trace<-head(trace,0)
  list(candidates=d,selected=selected,trace=as.data.frame(trace),gamma=menu$posterior_confidence,
    accurate=all(d$finite_interval & d$numerically_accurate),bounds=bounds)
}

msc_row <- function(selected,method,task,gamma) {
  returned<-nrow(selected)==1L
  out<-data.frame(method=method,gamma=gamma,returned=returned,
    lower=if(returned)selected$lower else NA_real_,upper=if(returned)selected$upper else NA_real_,
    width=if(returned)selected$width else NA_real_,
    q=if(returned)selected$target_content else NA_real_,delta=if(returned)selected$mean_tilt else NA_real_,
    candidate_index=if(returned)selected$candidate_index else NA_integer_,
    rate=if(returned && "learning_rate" %in% names(selected))selected$learning_rate else if(returned)1 else NA_real_,
    at_upper_bound=returned && isTRUE(selected$scale_at_upper_bound),
    negative_loss=returned && isTRUE(selected$tilted_loss<=0))
  for(name in c("job_id","response_id","n","content","tolerance_confidence","replication","data_seed"))out[[name]]<-task[[name]]
  out
}

msc_job <- function(task,cfg,context) {
  set.seed(task$data_seed);y<-context$laws[[task$response_id]]$r(task$n)
  file<-file.path(context$source$output_root,"modes/fits",sprintf("fit_%06d.rds",task$source_job_id))
  original<-context$development[context$development$job_id==task$source_job_id,,drop=FALSE]
  saved<-mbc_read_fit(file,context$source$mode_identity,original)
  baseline<-saved$anchors[["2"]]
  mwd_require(saved$ok && baseline$sample_sha256==mwd_hash(y) && baseline$prior$prior_sd==2 &&
    !baseline$prior$data_adaptive_prior && all(baseline$candidates$numerically_accurate),"A reused development fit differs.")
  gamma<-mec_primary(context,task$n,task$content)
  chosen<-mdc_select(baseline$candidates,gamma)
  mwd_require(nrow(chosen)==1,"The actual primary reference has no interval.")
  rows<-list(msc_row(chosen,"fixed",task,gamma)); models<-list()
  for(name in names(cfg$scale_ranges)) {
    result<-msc_fit(y,task,cfg,context,cfg$scale_ranges[[name]])
    models[[name]]<-result
    rows[[length(rows)+1L]]<-msc_row(result$selected,name,task,result$gamma)
  }
  classical<-mfp_comparators(y,task$content,task$tolerance_confidence)
  mwd_require(all(classical$returned),"A classical comparator failed in a feasible design.")
  for(name in c("job_id","response_id","n","content","tolerance_confidence","replication","data_seed"))classical[[name]]<-task[[name]]
  rows<-data.table::rbindlist(c(rows,list(classical)),fill=TRUE)
  rows[,population_content:=context$laws[[task$response_id]]$p(upper)-context$laws[[task$response_id]]$p(lower)]
  rows[,attained:=returned & population_content>=task$content]
  list(ok=all(vapply(models,function(z)z$accurate,FALSE)),task=task,rows=rows,models=models,
    sample_sha256=mwd_hash(y),baseline_candidates=baseline$candidates,
    baseline_receipt_sha256=mwd_file_hash(paste0(file,".json")))
}

msc_bound_review <- function(rows,cfg) {
  d<-data.table::as.data.table(rows)[method=="primary"]
  n<-sum(d$returned);k<-sum(d$at_upper_bound & d$returned)
  lower<-if(n==0)0 else max(0,k/n-sqrt(log(1/cfg$boundary_error)/(2*n)))
  a<-data.table::as.data.table(rows)[method=="primary" & returned,.(job_id,lower,upper,width)]
  b<-data.table::as.data.table(rows)[method=="broad" & returned,.(job_id,lower,upper)]
  paired<-merge(a,b,by="job_id",suffixes=c("_primary","_broad"))
  changed<-abs(paired$lower_primary-paired$lower_broad)>1e-5*pmax(1,paired$width) |
    abs(paired$upper_primary-paired$upper_broad)>1e-5*pmax(1,paired$width)
  majority<-lower>cfg$boundary_majority
  list(primary_returned=n,primary_upper_bound=k,upper_bound_fraction=if(n)k/n else NA_real_,
    panel_lower_bound=lower,bound_method="one_sided_Hoeffding_independent_heterogeneous_Bernoulli",
    majority_bound=majority,paired_ranges=nrow(paired),changed_range_pairs=sum(changed),
    stop_bound_sensitive=majority && any(changed),not_a_tolerance_test=TRUE)
}

msc_calibration <- function(records,cfg,context) {
  curves<-selections<-list();issues<-list();index<-0L
  designs<-unique(context$development[c("n","content","tolerance_confidence")])
  for(i in seq_len(nrow(designs)))for(model in c("fixed",names(cfg$scale_ranges))) {
    design<-designs[i,,drop=FALSE]
    pieces<-records[vapply(records,function(z)z$task$n==design$n && z$task$content==design$content,FALSE)]
    paths<-lapply(pieces,function(z) {
      d<-if(model=="fixed")z$baseline_candidates else z$models[[model]]$candidates
      p<-mdc_path(d);p$response_id<-rep(z$task$response_id,nrow(p))
      p$population_content<-context$laws[[z$task$response_id]]$p(p$upper)-context$laws[[z$task$response_id]]$p(p$lower);p
    })
    baseline<-data.table::rbindlist(lapply(pieces,function(z)z$rows[z$rows$method=="fixed",]))
    widths<-baseline[,.(w=mean(width)),by=response_id]
    gamma<-mec_primary(context,design$n,design$content)
    result<-tryCatch({
      curve<-mec_curve(paths,setNames(widths$w,widths$response_id),gamma,design$content,design$tolerance_confidence,cfg$compatibility_error)
      list(curve=curve,selection=mec_select_rules(curve,design$tolerance_confidence,cfg))
    },error=function(e)e)
    index<-index+1L
    if(inherits(result,"error")) {issues[[length(issues)+1L]]<-data.frame(n=design$n,content=design$content,model=model,reason=conditionMessage(result));next}
    for(name in names(design)){result$curve[[name]]<-design[[name]];result$selection[[name]]<-design[[name]]}
    result$curve$model<-model;result$selection$model<-model
    curves[[index]]<-result$curve;selections[[index]]<-result$selection
  }
  list(curves=data.table::rbindlist(curves,fill=TRUE),selection=data.table::rbindlist(selections,fill=TRUE),
    issues=data.table::rbindlist(issues,fill=TRUE),screening_only=TRUE)
}

msc_paired_comparison <- function(rows) {
  d<-data.table::as.data.table(rows)
  mwd_require(!anyDuplicated(d[,.(job_id,method)]),"Duplicate paired method results.")
  keys<-c("job_id","response_id","n","content","data_seed")
  reference<-d[method=="fixed",c(keys,"returned","lower","upper","width","attained"),with=FALSE]
  pieces<-lapply(intersect(c("narrow","primary","broad"),unique(d$method)),function(model) {
    paired<-merge(d[method==model],reference,by=keys,suffixes=c("_new","_fixed"))
    mwd_require(nrow(paired)==nrow(reference),"Paired methods have different observations.")
    tol<-1e-5*pmax(1,paired$width_fixed);both<-paired$returned_new & paired$returned_fixed
    paired[,`:=`(model=model,pair_returned=both,
      same_endpoints=both & abs(lower_new-lower_fixed)<=tol & abs(upper_new-upper_fixed)<=tol,
      narrower=both & width_new<width_fixed-tol,wider=both & width_new>width_fixed+tol,
      width_difference=width_new-width_fixed,attainment_changed=attained_new!=attained_fixed)]
    paired
  })
  paired<-data.table::rbindlist(pieces,fill=TRUE)
  totals<-paired[,.(datasets=.N,returned_pairs=sum(pair_returned),same_endpoints=sum(same_endpoints),
    narrower=sum(narrower),wider=sum(wider),attainment_events_changed=sum(attainment_changed)),by=model]
  casewise<-paired[,.(samples=.N,returned_pairs=sum(pair_returned),same_endpoints=sum(same_endpoints),
    narrower=sum(narrower),wider=sum(wider),mean_width_difference=mean(width_difference,na.rm=TRUE),
    attainment_events_changed=sum(attainment_changed)),by=.(model,response_id,n,content)]
  list(totals=totals,casewise=casewise)
}

msc_report <- function(records,cfg,context) {
  rows<-data.table::rbindlist(lapply(records,`[[`,"rows"),fill=TRUE)
  cells<-rows[,.(samples=.N,returned=sum(returned),successes=sum(attained),attainment=mean(attained),
    mean_width=mean(width,na.rm=TRUE),width_q025=quantile(width,.025,na.rm=TRUE),width_q975=quantile(width,.975,na.rm=TRUE),
    upper_bound_fraction=mean(at_upper_bound,na.rm=TRUE),median_rate=median(rate,na.rm=TRUE)),
    by=.(response_id,n,content,method)]
  cells[,lower_95:=ifelse(successes==0,0,qbeta(.025,successes,samples-successes+1))]
  cells[,upper_95:=ifelse(successes==samples,1,qbeta(.975,successes+1,samples-successes))]
  mec_write_intervals(rows,file.path(cfg$output_root,"screening/interval_results.csv"))
  mec_write_intervals(cells,file.path(cfg$output_root,"screening/case_summary.csv"))
  paired<-msc_paired_comparison(rows)
  mec_write_intervals(paired$casewise,file.path(cfg$output_root,"screening/paired_comparison.csv"))
  review<-msc_bound_review(rows,cfg);calibration<-msc_calibration(records,cfg,context)
  mec_write_intervals(calibration$curves,file.path(cfg$output_root,"screening/threshold_curves.csv"))
  mec_write_intervals(calibration$selection,file.path(cfg$output_root,"screening/exploratory_rules.csv"))
  if(nrow(calibration$issues))mec_write_intervals(calibration$issues,file.path(cfg$output_root,"screening/unavailable_calibrations.csv"))
  msc_figures(rows,cells,records,cfg)
  status<-if(review$stop_bound_sensitive)"scientific_stop_bound_sensitive" else "screen_complete_requires_development_review"
  result<-list(status=status,datasets=length(records),remaining_screening_datasets=0,
    numerical_pass=all(vapply(records,function(z)z$ok,FALSE)),prior_uses_analysis_data=FALSE,
    boundary_review=review,relaxed_rules_are_screening_only=TRUE,
    paired_endpoint_summary=as.data.frame(paired$totals),
    no_gibbs_sampling=TRUE,article_replaced=FALSE,time=mgfp_time())
  mgfp_json_write(result,file.path(cfg$output_root,"study_summary.json"));result
}

msc_figures <- function(rows,cells,records,cfg) {
  path<-file.path(cfg$output_root,"screening/common_scale_diagnostics.pdf")
  grDevices::pdf(path,width=12,height=8,onefile=TRUE);on.exit(grDevices::dev.off())
  methods<-c("fixed","narrow","primary","broad","young_mathew","wilks")
  method_labels<-c("Fixed scale","Narrow range","Primary range","Broad range","Young-Mathew","Wilks")
  law_labels<-c(normal="Normal",student_t3="Student t (3 df)",exponential="Exponential",
    asym_laplace_tau010="Asymmetric Laplace",two_piece_normal_1_12="Two-piece Normal",
    beta18="Beta(1,8)",gamma05="Gamma(0.5,1)",lognormal_hard="Lognormal")
  colors<-c("#0072B2","#009E73","#D55E00","#CC79A7","#777777","black")
  for(law in unique(cells$response_id)) {
    data<-cells[response_id==law];design<-unique(data[,.(n,content)])
    graphics::par(mfrow=c(1,2),mar=c(5,8,4,1))
    for(quantity in c("attainment","mean_width")) {
      limits<-if(quantity=="attainment")c(0,1) else range(c(data$width_q025,data$width_q975),finite=TRUE)
      graphics::plot(NA,xlim=limits,ylim=c(-.8,nrow(design)+1),yaxt="n",ylab="",
        xlab=if(quantity=="attainment")"Content attainment (Monte Carlo 95% CI)" else "Raw interval width (empirical 95% range)",
        main=paste(law_labels[law],if(quantity=="attainment")"attainment" else "width"))
      graphics::axis(2,at=seq_len(nrow(design)),labels=sprintf("n=%d, c=%.2f",design$n,design$content),las=1,cex.axis=.85)
      if(quantity=="attainment")graphics::abline(v=.95,lty=2,col="gray50")
      for(j in seq_along(methods)) {
        z<-merge(design[,position:=seq_len(.N)],data[method==methods[j]],by=c("n","content"))
        pos<-z$position+(j-3.5)*.10
        lo<-if(quantity=="attainment")z$lower_95 else z$width_q025
        hi<-if(quantity=="attainment")z$upper_95 else z$width_q975
        graphics::segments(lo,pos,hi,pos,col=colors[j],lwd=1.5)
        graphics::points(z[[quantity]],pos,pch=4,col=colors[j],cex=.7)
      }
      graphics::legend("bottom",legend=method_labels,col=colors,lty=1,cex=.65,ncol=3,bty="n")
    }
    graphics::par(mfrow=c(1,1),mar=c(5,5,4,1))
    z<-rows[response_id==law & method %in% c("narrow","primary","broad") & returned]
    groups<-interaction(z$method,z$n,z$content,drop=TRUE)
    if(nrow(z)) {
      graphics::boxplot(log(z$rate)~groups,las=2,cex.axis=.65,ylab="Estimated log inverse loss scale",
        main=paste(law_labels[law],"selected scale; exploratory screening"))
      graphics::abline(h=0,col="gray50",lty=2)
    } else {graphics::plot.new();graphics::title(main=paste(law_labels[law],"no eligible common-scale intervals"))}
  }
  for(index in which(vapply(records,function(z)z$task$replication==1,FALSE))) {
    record<-records[[index]];task<-record$task
    for(model in names(cfg$scale_ranges)) {
      t<-record$models[[model]]$trace
      if(!nrow(t))next
      graphics::par(mfrow=c(2,2),mar=c(4,4,3,1))
      variable_labels<-c(root1="Root a",root2="Root b",log_learning_rate="Log inverse loss scale",objective="Negative log joint density")
      for(variable in names(variable_labels)) {
        graphics::plot(seq_len(nrow(t)),t[[variable]],type="l",xlab="Recorded update",ylab=variable_labels[variable],
          main=sprintf("%s, n=%d, c=%.2f",law_labels[task$response_id],task$n,task$content),
          sub=sprintf("%s range; q=%.4f, delta=%.3f",model,record$models[[model]]$selected$target_content,
            record$models[[model]]$selected$mean_tilt))
      }
    }
  }
  invisible(path)
}

msc_verify <- function(cfg,context,tasks,identity,write_verification=TRUE) {
  paths<-file.path(cfg$output_root,"screening/fits",sprintf("fit_%06d.rds",tasks$job_id))
  stage<-mwd_json(file.path(cfg$output_root,"screening/manifest.json"))$analysis_id
  mwd_require(identical(stage,mwd_hash(list(identity,"screening",tasks))),"Screening identity changed.")
  records<-lapply(seq_len(nrow(tasks)),function(i) {
    z<-mbc_read_fit(paths[i],stage,tasks[i,,drop=FALSE]);task<-z$task
    set.seed(task$data_seed);y<-context$laws[[task$response_id]]$r(task$n)
    mwd_require(z$ok && identical(z$sample_sha256,mwd_hash(y)),"Saved observations or numerical status differ.")
    actual<-context$laws[[task$response_id]]$p(z$rows$upper)-context$laws[[task$response_id]]$p(z$rows$lower)
    good<-z$rows$returned
    mwd_require(all(abs(actual[good]-z$rows$population_content[good])<1e-12) &&
      all(z$rows$attained==(good & actual>=task$content)),"Saved population contents differ.")
    for(name in names(z$models)) {
      d<-z$models[[name]]$candidates
      mwd_require(all(d$numerically_accurate),"A saved candidate is not numerically accurate.")
      selected<-z$models[[name]]$selected
      if(nrow(selected)) {
        accuracy<-getFromNamespace(".rqr_cs_accuracy","rqrgibbs")(y,c(selected$lower,selected$upper),
          selected$target_content,selected$mean_tilt,selected$log_learning_rate,1/cfg$prior_sd^2,
          list(log_rate_bounds=log(cfg$scale_ranges[[name]])))
        mwd_require(accuracy$coordinate_gap<=1e-6 && accuracy$endpoint_gap<=1e-5*max(1,selected$width) &&
          accuracy$scale_gap<=1e-6,"Selected common-scale roots fail independent accuracy checks.")
      }
    }
    z
  })
  manifest<-mwd_json(file.path(cfg$output_root,"manifest.json"))$identity
  mgfp_check_hashes(manifest$article_files)
  if(write_verification)mgfp_json_write(list(passed=TRUE,datasets=length(records),article_unchanged=TRUE,
    no_gibbs_sampling=TRUE,prior_uses_analysis_data=FALSE,time=mgfp_time()),file.path(cfg$output_root,"verification.json"))
  records
}

msc_review <- function(study,cfg) {
  original<-mwd_json(file.path(study$output_root,"manifest.json"))
  old<-original$identity$configuration
  progress<-mwd_json(file.path(study$output_root,"screening/progress.json"))
  mwd_require(file.exists(file.path(study$output_root,"controller_exit.json")),
    "Wait for the original controller to finish before separate review.")
  mwd_require(progress$remaining_datasets==0 && progress$numerical_pass &&
    progress$completed_datasets==old$expected_cells*old$screening_replications,
    "The original screening fits are incomplete.")
  commit<-paste(system2("git",c("--no-optional-locks","-C",shQuote(study$checkout),"rev-parse","HEAD"),stdout=TRUE),collapse="")
  mwd_require(commit==study$source_commit && commit==original$identity$source_commit,"The original source checkout changed.")
  mgfp_check_hashes(original$identity$programs,file.path(study$checkout,names(original$identity$programs)))
  mgfp_check_hashes(original$identity$package$libraries)
  inputs<-list.files(study$output_root,recursive=TRUE,full.names=TRUE)
  inputs<-inputs[grepl("[.](json|csv|pdf)$",inputs)]
  before<-mgfp_hashes(inputs)
  current<-mgfp_git("rev-parse","HEAD")
  mwd_require(!nzchar(mgfp_git("status","--porcelain")),"Commit review source before producing revised reports.")
  target<-paste0(study$output_root,"_review_",current)
  dir.create(file.path(target,"screening"),recursive=TRUE,showWarnings=FALSE)
  context<-msc_context(cfg,target)
  tasks<-context$development[context$development$replication<=old$screening_replications,,drop=FALSE]
  tasks$source_job_id<-tasks$job_id;tasks$job_id<-seq_len(nrow(tasks))
  records<-msc_verify(old,context,tasks,original$identity,write_verification=FALSE)
  reviewed<-old;reviewed$output_root<-target
  if(file.exists(file.path(target,"review.json"))) {
    prior<-mwd_json(file.path(target,"review.json"))
    mgfp_check_hashes(prior$outputs)
    mgfp_check_hashes(prior$inputs)
    mwd_require(prior$source_commit==commit && prior$analysis_commit==current,"Reviewed source changed.")
    return(prior)
  }
  result<-msc_report(records,reviewed,context)
  mgfp_check_hashes(before)
  files<-list.files(target,recursive=TRUE,full.names=TRUE)
  evidence<-list(source_commit=commit,analysis_commit=current,output_root=target,
    sampling_output_root=study$output_root,datasets=length(records),passed=TRUE,
    no_refitting=TRUE,original_outputs_preserved=TRUE,
    correction="Heterogeneous panel bound uses Hoeffding; within-case binomial intervals unchanged.",
    result=result,inputs=before,outputs=mgfp_hashes(files),time=mgfp_time())
  mgfp_json_write(evidence,file.path(target,"review.json"));evidence
}
