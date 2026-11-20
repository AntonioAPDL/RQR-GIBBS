mgsc_validate <- function(cfg) {
  mwd_require(cfg$prior_sd==2 && cfg$dp_concentration==1 && cfg$dp_base_sd==4 &&
    cfg$coordinates$center==0 && cfg$coordinates$scale==1 &&
    identical(cfg$coordinates$estimated_from_analysis_sample,FALSE) &&
    cfg$expected_cells==72 && cfg$screening_replications==12 &&
    cfg$maximum_workers>=1 && cfg$maximum_workers<=40 && cfg$radius_multiplier==1 &&
    !cfg$sample_gibbs && !cfg$unrestricted_nonzero_tilt &&
    !cfg$automatic_article_replacement && !cfg$application_transfer &&
    identical(cfg$launch_ceiling,"screening"),"The declared Gamma study changed.")
  mwd_require(identical(names(cfg$prior_specs),c("G4","G16","G64","G16_N","G16_W","G16_Z")),
    "The predeclared Gamma comparison differs.")
  expected<-list(G4=c(4,4,.25,4),G16=c(16,16,.25,4),G64=c(64,64,.25,4),
    G16_N=c(16,16,.5,2),G16_W=c(16,16,.0625,16),G16_Z=c(16,16,0,4))
  for(name in names(cfg$prior_specs)) {
    spec<-cfg$prior_specs[[name]]
    getFromNamespace(".rqr_cs_spec","rqrgibbs")(mgsc_control(spec,cfg))
    mwd_require(spec$shape==spec$rate && spec$shape %in% c(4,16,64) &&
      length(spec$rate_bounds)==2 && spec$rate_bounds[1]>=0 &&
      is.finite(spec$rate_bounds[2]) &&
      identical(as.numeric(c(spec$shape,spec$rate,spec$rate_bounds)),expected[[name]]),
      "Invalid independent Gamma prior.")
  }
  invisible(TRUE)
}

mgsc_control <- function(spec,cfg) list(prior="gamma_rate",shape=spec$shape,rate=spec$rate,
  log_rate_bounds=log(spec$rate_bounds),update=cfg$update,polish_cycles=cfg$polish_cycles)

mgsc_previous <- function(cfg,context) {
  pointer<-mwd_json(file.path(mgfp_common(),".codex_work/mti_ecm_common_scale_latest.json"))
  manifest<-mwd_json(file.path(pointer$output_root,"manifest.json"))
  mwd_require(pointer$source_commit==cfg$source_common_scale_commit &&
    manifest$identity$source_commit==pointer$source_commit,"Historical screening source differs.")
  exit<-file.path(pointer$output_root,"controller_exit.json")
  mwd_require(file.exists(exit) && mwd_json(exit)$exit_code==0,"The previous study has not completed successfully.")
  mgfp_check_hashes(manifest$identity$programs,file.path(pointer$checkout,names(manifest$identity$programs)))
  mgfp_check_hashes(manifest$identity$package$libraries)
  tasks<-context$development[context$development$replication<=cfg$screening_replications,,drop=FALSE]
  tasks$source_job_id<-tasks$job_id;tasks$job_id<-seq_len(nrow(tasks))
  stage<-mwd_json(file.path(pointer$output_root,"screening/manifest.json"))$analysis_id
  mwd_require(stage==mwd_hash(list(manifest$identity,"screening",tasks)),"Historical task identity differs.")
  list(pointer=pointer,identity=manifest$identity,stage=stage,tasks=tasks)
}

mgsc_read_previous <- function(task,previous) {
  path<-file.path(previous$pointer$output_root,"screening/fits",sprintf("fit_%06d.rds",task$job_id))
  value<-mbc_read_fit(path,previous$stage,task)
  mwd_require(value$ok,"Historical numerical evidence is incomplete.")
  value
}

mgsc_audit <- function(cfg,context,previous) {
  directory<-file.path(cfg$output_root,"audit");dir.create(directory,recursive=TRUE,showWarnings=FALSE)
  receipt<-file.path(directory,"assessment.json")
  if(file.exists(receipt)) {
    old<-mwd_json(receipt);mgfp_check_hashes(old$outputs)
    mwd_require(old$source_commit==mgfp_git("rev-parse","HEAD") && old$passed,"Audit source differs.")
    return(old)
  }
  diagnosis<-getFromNamespace(".rqr_cs_tilt_domain","rqrgibbs")
  pieces<-lapply(seq_len(nrow(previous$tasks)),function(i) {
    task<-previous$tasks[i,,drop=FALSE];value<-mgsc_read_previous(task,previous)
    set.seed(task$data_seed);y<-context$laws[[task$response_id]]$r(task$n)
    mwd_require(value$sample_sha256==mwd_hash(y),"Historical observation digest differs.")
    d<-value$models$primary$candidates
    checks<-lapply(seq_len(nrow(d)),function(j)diagnosis(y,d$target_content[j],d$mean_tilt[j]))
    d$positive_ray_slope<-vapply(checks,`[[`,0,"positive_ray_slope")
    d$negative_ray_slope<-vapply(checks,`[[`,0,"negative_ray_slope")
    d$unbounded_loss_ray<-vapply(checks,`[[`,FALSE,"unbounded_loss_ray")
    d$retained_tilt_lower<-vapply(checks,`[[`,0,"retained_tilt_lower")
    d$retained_tilt_upper<-vapply(checks,`[[`,0,"retained_tilt_upper")
    d$job_id<-task$job_id;d$response_id<-task$response_id;d$n<-task$n;d$content<-task$content
    d$selected<-d$candidate_index %in% value$models$primary$selected$candidate_index
    d[,intersect(names(d),c("job_id","response_id","n","content","candidate_index","target_content",
      "mean_tilt","central_tilt","tilted_loss","positive_ray_slope","negative_ray_slope",
      "unbounded_loss_ray","retained_tilt_lower","retained_tilt_upper","selected")),drop=FALSE]
  })
  d<-data.table::rbindlist(pieces)
  mec_write_intervals(d,file.path(directory,"candidate_loss_domain.csv"))
  priors<-data.table::rbindlist(lapply(names(cfg$prior_specs),function(name) {
    s<-cfg$prior_specs[[name]];lo<-pgamma(s$rate_bounds[1],s$shape,rate=s$rate)
    hi<-pgamma(s$rate_bounds[2],s$shape,rate=s$rate)
    q<-qgamma(lo+c(.025,.5,.975)*(hi-lo),s$shape,rate=s$rate)
    data.frame(method=name,shape=s$shape,rate=s$rate,normalizing_mass=hi-lo,
      omega_q025=q[1],omega_median=q[2],omega_q975=q[3],
      sigma_q025=1/q[3],sigma_median=1/q[2],sigma_q975=1/q[1])
  }))
  mec_write_intervals(priors,file.path(directory,"prior_quantiles.csv"))
  result<-list(passed=TRUE,datasets=nrow(previous$tasks),candidates=nrow(d),
    negative_losses=sum(d$tilted_loss<0),unbounded_ray_candidates=sum(d$unbounded_loss_ray),
    negative_selected_losses=sum(d$tilted_loss[d$selected]<0),
    unrestricted_nonzero_tilt_enabled=FALSE,
    reason="A Gamma conditional at selected roots is insufficient; bounded Gamma and zero-tilt unrestricted controls are proved proper.",
    previous_source=previous$pointer$source_commit,prior_uses_analysis_data=FALSE,
    source_commit=mgfp_git("rev-parse","HEAD"),
    outputs=mgfp_hashes(file.path(directory,c("candidate_loss_domain.csv","prior_quantiles.csv"))),time=mgfp_time())
  mgfp_json_write(result,receipt);result
}

mgsc_job <- function(task,cfg,context,previous) {
  set.seed(task$data_seed);y<-context$laws[[task$response_id]]$r(task$n)
  old<-mgsc_read_previous(task,previous)
  mwd_require(old$sample_sha256==mwd_hash(y),"Paired observations differ.")
  rows<-old$rows[old$rows$method %in% c("fixed","primary","young_mathew","wilks"),]
  rows$method[rows$method=="primary"]<-"log_uniform"
  models<-list(log_uniform=old$models$primary)
  for(name in names(cfg$prior_specs)) {
    fitted<-msc_fit_control(y,task,cfg,context,mgsc_control(cfg$prior_specs[[name]],cfg))
    models[[name]]<-fitted
    rows<-data.table::rbindlist(list(rows,msc_row(fitted$selected,name,task,fitted$gamma)),fill=TRUE)
  }
  rows$population_content<-context$laws[[task$response_id]]$p(rows$upper)-context$laws[[task$response_id]]$p(rows$lower)
  rows$attained<-rows$returned & rows$population_content>=task$content
  list(ok=all(vapply(models,function(z)isTRUE(z$accurate),FALSE)),task=task,rows=rows,models=models,
    sample_sha256=mwd_hash(y),baseline_candidates=old$baseline_candidates,
    historical_fit_sha256=mwd_file_hash(file.path(previous$pointer$output_root,"screening/fits",sprintf("fit_%06d.rds",task$job_id))))
}

mgsc_verify <- function(tasks,stage,cfg,context,identity,previous) {
  id<-mwd_hash(list(identity,stage,tasks));directory<-file.path(cfg$output_root,stage)
  mwd_require(mwd_json(file.path(directory,"manifest.json"))$analysis_id==id,"Stage identity changed.")
  accuracy<-getFromNamespace(".rqr_cs_accuracy","rqrgibbs")
  records<-lapply(seq_len(nrow(tasks)),function(i) {
    task<-tasks[i,,drop=FALSE]
    value<-mbc_read_fit(file.path(directory,"fits",sprintf("fit_%06d.rds",task$job_id)),id,task)
    set.seed(task$data_seed);y<-context$laws[[task$response_id]]$r(task$n)
    old<-mgsc_read_previous(task,previous)
    mwd_require(value$ok && value$sample_sha256==mwd_hash(y) && old$sample_sha256==value$sample_sha256,
      "Saved fitting status or observations differ.")
    mwd_require(value$historical_fit_sha256==mwd_file_hash(file.path(previous$pointer$output_root,
      "screening/fits",sprintf("fit_%06d.rds",task$job_id))) &&
      identical(mwd_hash(value$baseline_candidates),mwd_hash(old$baseline_candidates)),"Historical paired inputs changed.")
    actual<-context$laws[[task$response_id]]$p(value$rows$upper)-context$laws[[task$response_id]]$p(value$rows$lower)
    good<-value$rows$returned
    mwd_require(nrow(value$rows)==length(cfg$prior_specs)+4 && !anyDuplicated(value$rows$method) &&
      all(abs(actual[good]-value$rows$population_content[good])<1e-12) &&
      identical(as.logical(value$rows$attained),as.logical(good & actual>=task$content)),"Interval results differ.")
    for(name in names(cfg$prior_specs)) {
      model<-value$models[[name]];s<-model$selected
      mwd_require(model$accurate && all(model$candidates$numerically_accurate) &&
        identical(model$scale_control,mgsc_control(cfg$prior_specs[[name]],cfg)),"A saved Gamma prior or candidate differs.")
      selected_again<-mdc_select(model$candidates,model$gamma)
      mwd_require(isTRUE(all.equal(as.data.frame(s),as.data.frame(selected_again),check.attributes=FALSE)),
        "Saved candidate selection differs from its declared probability rule.")
      if(nrow(s)) {
        row<-value$rows[value$rows$method==name,]
        mwd_require(nrow(row)==1 && row$returned && row$lower==s$lower && row$upper==s$upper &&
          row$width==s$width && row$rate==s$learning_rate,"Selected interval export differs.")
        check<-accuracy(y,c(s$lower,s$upper),s$target_content,s$mean_tilt,s$log_learning_rate,
          1/cfg$prior_sd^2,mgsc_control(cfg$prior_specs[[name]],cfg))
        mwd_require(check$coordinate_gap<=1e-6 && check$endpoint_gap<=1e-5*max(1,s$width) &&
          check$scale_gap<=1e-6 && check$scale_displacement<=1e-5,"Selected Gamma mode failed independent checks.")
      }
    }
    value
  })
  mgfp_check_hashes(identity$programs);mgfp_check_hashes(identity$article_files)
  mgfp_check_hashes(identity$package$libraries)
  mgfp_json_write(list(passed=TRUE,datasets=length(records),prior_uses_analysis_data=FALSE,
    article_unchanged=TRUE,time=mgfp_time()),file.path(directory,"verification.json"))
  records
}

mgsc_pairs <- function(rows,methods) {
  d<-data.table::as.data.table(rows)
  mwd_require(!anyDuplicated(d[,.(job_id,method)]),"Duplicate paired results.")
  keys<-c("job_id","response_id","n","content","data_seed")
  ref<-d[method=="fixed",c(keys,"returned","width","lower","upper","attained"),with=FALSE]
  out<-data.table::rbindlist(lapply(methods,function(model) {
    p<-merge(d[method==model],ref,by=keys,suffixes=c("_new","_fixed"))
    mwd_require(nrow(p)==nrow(ref),"Paired observations differ.")
    both<-p$returned_new & p$returned_fixed;tol<-1e-5*pmax(1,p$width_fixed)
    p[,`:=`(model=model,both=both,narrower=both & width_new<width_fixed-tol,
      wider=both & width_new>width_fixed+tol,
      same_endpoints=both & abs(lower_new-lower_fixed)<=tol & abs(upper_new-upper_fixed)<=tol,
      width_difference=width_new-width_fixed,
      gained=attained_new & !attained_fixed,lost=!attained_new & attained_fixed)]
    p
  }),fill=TRUE)
  list(totals=out[,.(datasets=.N,returned_pairs=sum(both),same_endpoints=sum(same_endpoints),
    narrower=sum(narrower),wider=sum(wider),attainment_gained=sum(gained),attainment_lost=sum(lost)),by=model],
    casewise=out[,.(samples=.N,returned_pairs=sum(both),narrower=sum(narrower),wider=sum(wider),
      mean_width_difference=mean(width_difference),attainment_gained=sum(gained),attainment_lost=sum(lost)),
      by=.(model,response_id,n,content)])
}

mgsc_decision <- function(totals) {
  t<-data.table::as.data.table(totals)
  complete<-all(t$returned_pairs==t$datasets)
  changes<-any(t$narrower>0 | t$wider>0)
  gains<-any(t$narrower>0)
  list(status=if(!complete)"scientific_review_missing_intervals" else if(!changes)
      "scientific_stop_no_endpoint_effect" else if(!gains)"scientific_stop_no_width_gain" else
      "screening_complete_requires_prior_and_support_review",
    complete_pairs=complete,any_narrower=gains,automatic_full_validation=FALSE,
    reason="Screening cannot select a prior and establish tolerance on the same samples; subsequent development, independent calibration and validation remain conditional.")
}

mgsc_figures <- function(rows,cells,records,cfg) {
  path<-file.path(cfg$output_root,"screening/gamma_scale_diagnostics.pdf")
  grDevices::pdf(path,width=13,height=9);on.exit(grDevices::dev.off())
  methods<-c("fixed","log_uniform",names(cfg$prior_specs),"young_mathew","wilks")
  labels<-c("Fixed scale","Log-uniform","Gamma 4","Gamma 16","Gamma 64",
    "Gamma 16, narrow","Gamma 16, wide","Gamma 16, no lower bound","Young-Mathew","Wilks")
  colors<-c("#0072B2","#999999","#009E73","#D55E00","#CC79A7","#E69F00","#56B4E9","#666633","#882255","black")
  law_labels<-c(normal="Normal",student_t3="Student t (3 df)",exponential="Exponential",
    asym_laplace_tau010="Asymmetric Laplace",two_piece_normal_1_12="Two-piece Normal",
    beta18="Beta(1,8)",gamma05="Gamma(0.5,1)",lognormal_hard="Lognormal")
  designs<-unique(cells[,.(n,content)])
  for(i in seq_len(nrow(designs))) {
    design<-designs[i]
    for(quantity in c("attainment","mean_width")) {
      graphics::par(mfrow=c(2,4),mar=c(9,4,3,1))
      for(law in unique(cells$response_id)) {
        z<-cells[n==design$n & content==design$content & response_id==law][match(methods,method)]
        lo<-if(quantity=="attainment")z$lower else z$width_q025
        hi<-if(quantity=="attainment")z$upper else z$width_q975
        lim<-if(quantity=="attainment")c(0,1) else range(c(lo,hi),finite=TRUE)
        if(!all(is.finite(lim)))lim<-c(0,1)
        graphics::plot(seq_along(methods),z[[quantity]],ylim=lim,pch=4,col=colors,xaxt="n",xlab="",
          ylab=if(quantity=="attainment")"Content attainment (95% MC CI)" else "Width (empirical 95% range)",
          main=sprintf("%s; n=%d, c=%.2f",law_labels[law],design$n,design$content),cex.main=.85)
        graphics::segments(seq_along(methods),lo,seq_along(methods),hi,col=colors)
        graphics::axis(1,seq_along(methods),labels=labels,las=2,cex.axis=.65)
        if(quantity=="attainment")graphics::abline(h=.95,col="gray50",lty=2)
      }
    }
  }
  chosen<-which(vapply(records,function(z)z$task$replication==1,FALSE))
  for(i in chosen)for(model in names(cfg$prior_specs)) {
    z<-records[[i]];t<-z$models[[model]]$trace;if(!nrow(t))next
    graphics::par(mfrow=c(2,2),mar=c(5.3,5,3,1))
    variables<-c(root1="Root a",root2="Root b",log_learning_rate="Log inverse loss scale",objective="Negative log joint density")
    for(v in names(variables)) {
      graphics::plot(seq_len(nrow(t)),t[[v]],type="l",yaxt="n",xlab="Recorded update",ylab=variables[v],
        main=sprintf("%s, n=%d, c=%.2f",law_labels[z$task$response_id],z$task$n,z$task$content),
        sub=sprintf("%s; q=%.4f, delta=%.3f",model,z$models[[model]]$selected$target_content,z$models[[model]]$selected$mean_tilt))
      ticks<-graphics::axTicks(2);graphics::axis(2,ticks,format(ticks,digits=10,trim=TRUE),cex.axis=.8)
    }
  }
  invisible(path)
}

mgsc_report <- function(records,cfg,context) {
  receipt<-file.path(cfg$output_root,"report_receipt.json")
  if(file.exists(receipt)) {
    old<-mwd_json(receipt);mgfp_check_hashes(old$outputs)
    mwd_require(old$source_commit==mgfp_git("rev-parse","HEAD"),"Report source differs.")
    return(mwd_json(file.path(cfg$output_root,"study_summary.json")))
  }
  rows<-data.table::rbindlist(lapply(records,`[[`,"rows"),fill=TRUE)
  cells<-mec_summary(rows,cfg);directory<-file.path(cfg$output_root,"screening")
  pairs<-mgsc_pairs(rows,names(cfg$prior_specs))
  for(item in list(list(rows,"interval_results.csv"),list(cells,"case_summary.csv"),
    list(pairs$totals,"paired_totals.csv"),list(pairs$casewise,"paired_comparison.csv")))
    mec_write_intervals(item[[1]],file.path(directory,item[[2]]))
  replay<-cfg;replay$scale_ranges<-lapply(c(list(log_uniform=list(rate_bounds=c(.25,4))),cfg$prior_specs),`[[`,"rate_bounds")
  calibration<-msc_calibration(records,replay,context)
  mec_write_intervals(calibration$curves,file.path(directory,"threshold_curves.csv"))
  mec_write_intervals(calibration$selection,file.path(directory,"exploratory_rules.csv"))
  if(nrow(calibration$issues))mec_write_intervals(calibration$issues,file.path(directory,"unavailable_calibrations.csv"))
  boundary<-rows[method %in% names(cfg$prior_specs),.(samples=.N,returned=sum(returned),
    upper_bound_modes=sum(at_upper_bound),negative_selected_losses=sum(negative_loss),
    rate_min=min(rate,na.rm=TRUE),rate_median=median(rate,na.rm=TRUE),rate_max=max(rate,na.rm=TRUE)),by=method]
  mec_write_intervals(boundary,file.path(directory,"scale_summary.csv"))
  mgsc_figures(rows,cells,records,cfg)
  result<-c(mgsc_decision(pairs$totals),list(source_commit=mgfp_git("rev-parse","HEAD"),
    datasets=length(records),remaining_datasets=0,
    numerical_pass=TRUE,paired_comparison=as.data.frame(pairs$totals),scale_summary=as.data.frame(boundary),
    calibration_is_exploratory=TRUE,prior_uses_analysis_data=FALSE,no_gibbs_sampling=TRUE,
    article_replaced=FALSE,time=mgfp_time()))
  mgfp_json_write(result,file.path(cfg$output_root,"study_summary.json"))
  mgfp_json_write(list(status=result$status,development_datasets=7200,maximum_prior_finalists=2,
    initial_fresh_calibration_datasets=14400,fresh_validation_datasets=72000,
    next_action=cfg$next_stage_decision,not_launched=TRUE,
    plan=".codex_work/mti_gamma_scale_research_plan_20261119.md"),file.path(cfg$output_root,"followup_decision.json"))
  files<-c(list.files(directory,pattern="[.](csv|pdf)$",full.names=TRUE),
    file.path(cfg$output_root,c("study_summary.json","followup_decision.json")))
  mgfp_json_write(list(source_commit=result$source_commit,outputs=mgfp_hashes(files)),receipt)
  result
}
