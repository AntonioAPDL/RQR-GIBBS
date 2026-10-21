pea_read_audit <- function(directory) {
  evidence_path <- file.path(directory,"evidence_manifest.json")
  if(file.exists(evidence_path)) {
    evidence <- jsonlite::read_json(evidence_path,simplifyVector=TRUE)
    for(i in seq_len(nrow(evidence$files))) {
      z <- evidence$files[i,]; pma_verify_hash(file.path(directory,z$path),z$sha256)
    }
  }
  manifest <- jsonlite::read_json(file.path(directory,"manifest.json"),simplifyVector=TRUE)
  for(i in seq_len(nrow(manifest$artifact_records))) {
    z <- manifest$artifact_records[i,]
    pma_verify_hash(file.path(directory,z$path),z$sha256)
  }
  cfg <- pta_read_config(file.path(directory,"config_frozen.json"))
  summary <- pta_read_csv(file.path(directory,"summary.csv"))
  cases <- pta_read_csv(file.path(directory,"selected_cases.csv"))
  traces <- pta_read_csv(file.path(directory,"iteration_traces.csv.gz"))
  refs <- pta_read_csv(file.path(directory,"references.csv"))
  candidates <- pta_read_csv(file.path(directory,"candidates.csv"))
  all <- pta_read_csv(file.path(directory,"all_saved_diagnostics.csv"))
  if (nrow(cases)!=manifest$expected_cases || nrow(summary)!=manifest$expected_budget_rows ||
      nrow(traces)!=manifest$iteration_trace_rows ||
      anyDuplicated(paste(summary$case_id,summary$multiplier)) ||
      anyDuplicated(paste(refs$case_id,refs$candidate_index)) ||
      !isTRUE(manifest$protected_unchanged) || isTRUE(manifest$article_results_replaced)) {
    stop("Incomplete or inconsistent diagnostic evidence.")
  }
  for(i in seq_len(nrow(manifest$protected_before))) {
    z <- manifest$protected_before[i,]
    pma_verify_hash(z$path,z$sha256)
  }
  observed <- pta_read_csv(file.path(cfg$canonical_directory,"observed_batches.csv"))
  membership <- pta_read_csv(file.path(cfg$canonical_directory,"split_membership.csv.gz"))
  ranges <- do.call(rbind,lapply(seq_len(nrow(cases)),function(i) {
    r <- cases[i,]
    m <- membership[membership$split_scheme==r$split_scheme & membership$split_id==r$split_id,]
    y <- observed[[r$response_id]][m$analysis_row_id[m$training]]
    data.frame(case_id=r$case_id,minimum=min(y),maximum=max(y),sd=sd(y))
  }))
  list(config=cfg,manifest=manifest,summary=summary,cases=cases,traces=traces,
       references=refs,candidates=candidates,all=all,ranges=ranges)
}

pea_decisions <- function(inputs) {
  s <- inputs$summary; cfg <- inputs$config
  first <- s[s$multiplier==min(s$multiplier),]
  last <- s[s$multiplier==max(s$multiplier),]
  last <- last[match(first$case_id,last$case_id),]
  ranges <- inputs$ranges[match(first$case_id,inputs$ranges$case_id),]
  menu <- do.call(rbind,lapply(seq_len(nrow(first)),function(i) {
    z <- inputs$references[inputs$references$case_id==first$case_id[i] &
      inputs$references$candidate_index==first$menu_reference_candidate[i],]
    if(nrow(z)!=1L) stop("Counterfactual reference selection is missing.")
    z
  }))
  data.frame(case_id=first$case_id,response_id=first$response_id,split_scheme=first$split_scheme,
    split_id=first$split_id,original_flag=first$original_flag,
    initial_relative_objective_gap=first$relative_objective_gap,
    final_relative_objective_gap=last$relative_objective_gap,
    initial_endpoint_difference_sd=first$endpoint_difference_sd,
    final_endpoint_difference_sd=last$endpoint_difference_sd,
    long_budget_endpoint_change_sd=last$endpoint_change_sd,
    initial_midpoint_stationarity=first$midpoint_stationarity,
    nearest_kink_displacement_sd=first$nearest_kink_displacement_sd,
    nearest_kink_stationarity_distance=first$nearest_kink_stationarity_distance,
    original_screen_pass=first$passes_screen,final_screen_pass=last$passes_screen,
    baseline_width=first$width,long_budget_width=last$width,
    menu_reference_width=first$menu_reference_width,
    menu_reference_width_difference=first$menu_reference_width-first$width,
    menu_reference_width_ratio=first$menu_reference_width/first$width,
    reference_extent_outside_training_sd=pmax(ranges$minimum-first$reference_lower,
      first$reference_upper-ranges$maximum,0)/ranges$sd,
    menu_reference_probability=menu$posterior_content_probability,
    original_target_content=first$target_content,original_tilt=first$mean_tilt,
    menu_reference_target_content=menu$target_content,menu_reference_tilt=menu$mean_tilt,
    menu_selected_candidate_changed=first$menu_reference_candidate!=first$original_candidate,
    objective_discrepancy=first$relative_objective_gap>cfg$comparisons$objective_relative_tolerance,
    persistent_objective_discrepancy=last$relative_objective_gap>cfg$comparisons$objective_relative_tolerance,
    endpoint_sensitivity=last$endpoint_change_sd>cfg$comparisons$endpoint_training_sd_tolerance,
    screen_changed=first$passes_screen!=last$passes_screen)
}

pea_report_pdf <- function(inputs,path) {
  dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE)
  d <- pea_decisions(inputs); s <- inputs$summary
  colors <- c("1"="#0072B2","5"="#D55E00","20"="#009E73")
  pdf(path,width=11,height=8.5,onefile=TRUE,useDingbats=FALSE)
  on.exit(dev.off())
  par(mfrow=c(2,2),mar=c(4.4,4.5,2.5,1),oma=c(1,0,3,0),cex=.85)
  plot(seq_len(nrow(d)),pmax(d$initial_relative_objective_gap,1e-16),log="y",pch=16,
    xlab="Prespecified diagnostic case",ylab="Relative objective gap",main="Fixed-target reference comparison")
  points(seq_len(nrow(d)),pmax(d$final_relative_objective_gap,1e-16),pch=4,col="#009E73")
  abline(h=inputs$config$comparisons$objective_relative_tolerance,lty=2,col="grey50")
  legend("topleft",c("Original budget","20x budget"),pch=c(16,4),col=c("black","#009E73"),bty="n")
  plot(seq_len(nrow(d)),pmax(d$long_budget_endpoint_change_sd,1e-16),log="y",pch=16,
    xlab="Prespecified diagnostic case",ylab="Endpoint change / training SD",main="Effect of a longer iteration budget")
  abline(h=inputs$config$comparisons$endpoint_training_sd_tolerance,lty=2,col="grey50")
  a <- inputs$all
  plot(pmax(a$midpoint_stationarity,1e-16),pmax(a$nearest_kink_stationarity_distance,1e-16),
    log="xy",pch=16,col=adjustcolor("black",.12),xlab="Saved endpoint midpoint gradient norm",
    ylab="Nearest-kink stationarity distance",main="All saved MTI intervals: backward-error diagnostic")
  plot(d$baseline_width,d$menu_reference_width,pch=16,
    col=ifelse(d$response_id=="fct_tensile","#0072B2","#D55E00"),
    xlab="Frozen-profile interval width",ylab="Reference-profile interval width",
    main="Counterfactual menu comparison (not promoted)")
  abline(0,1,lty=2,col="grey50")
  legend("topleft",c("Tensile strength","Weight RSD"),pch=16,
    col=c("#0072B2","#D55E00"),bty="n")
  mtext("MTI-ECM pharmaceutical numerical diagnostics",outer=TRUE,line=1,font=2,cex=1.2)
  for (id in inputs$cases$case_id) {
    r <- s[s$case_id==id & s$multiplier==min(s$multiplier),]
    t <- inputs$traces[inputs$traces$case_id==id,]
    par(mfrow=c(2,3),mar=c(4.2,4.5,2.4,.7),oma=c(2,0,4,0),cex=.8)
    panel <- function(variable,title,ylab,reference=NULL,log_y=FALSE) {
      values <- t[[variable]]
      if(log_y) values <- pmax(values,1e-16)
      bounds <- range(c(values,if(log_y) pmax(reference,1e-16) else reference),finite=TRUE)
      if(diff(bounds)==0) bounds <- bounds+c(-1,1)*max(abs(bounds[1])*.01,.01)
      if(log_y) bounds <- pmax(bounds,1e-16)
      plot(NA,xlim=range(log10(t$iteration+1)),ylim=bounds,log=if(log_y) "y" else "",
        xlab="log10(iteration + 1)",ylab=ylab,main=title)
      for (m in sort(unique(t$multiplier),decreasing=TRUE)) {
        z <- t[t$multiplier==m,]
        v <- z[[variable]]; if(log_y) v <- pmax(v,1e-16)
        lines(log10(z$iteration+1),v,col=colors[as.character(m)],lwd=1.3)
      }
      if(!is.null(reference)) abline(h=reference,lty=2,col="grey40")
    }
    panel("root1","Lower endpoint","Lower endpoint",r$reference_lower)
    panel("root2","Upper endpoint","Upper endpoint",r$reference_upper)
    panel("width","Interval width","Width",r$reference_width)
    panel("objective_gap","Fixed-target objective gap","Objective minus reference",log_y=TRUE)
    panel("stationarity_distance","Exact-point stationarity","Distance to gradient interval",log_y=TRUE)
    panel("posterior_content_probability","Posterior content probability","Posterior probability",r$screen_threshold)
    legend("bottomleft",paste(c(1,5,20),"x iteration budget"),col=colors,lty=1,bty="n",cex=.75)
    mtext(sprintf("%s: %s / %s / split %d",id,r$response_id,r$split_scheme,r$split_id),outer=TRUE,line=2,font=2)
    mtext(sprintf("Fixed q = %.6f; tilt = %.6g; original convergence flag = %s",r$target_content,r$mean_tilt,r$original_flag),
      outer=TRUE,line=.7)
    mtext(sprintf("Nearest-kink displacement / SD = %.3g; stationarity distance there = %.3g. Dashed: reference or probability threshold.",
      r$nearest_kink_displacement_sd,r$nearest_kink_stationarity_distance),outer=TRUE,side=1,line=.6,cex=.8)
  }
}

pea_write_findings <- function(inputs,directory,pdf_path) {
  d <- pea_decisions(inputs)
  pma_write_csv(d,file.path(directory,"case_decisions.csv"))
  n <- nrow(d)
  lines <- c("# Pharmaceutical ECM Numerical Findings","",
    "This diagnostic study does not replace or recalibrate the published procedure.","",
    sprintf("Completed %d prespecified cases; %d fixed-target budget fits; %d saved-interval diagnostics.",
      n,nrow(inputs$summary),nrow(inputs$all)),
    sprintf("Baseline relative objective gaps above %.3g: %d/%d.",
      inputs$config$comparisons$objective_relative_tolerance,sum(d$objective_discrepancy),n),
    sprintf("Persistent gaps at the longest budget: %d/%d.",sum(d$persistent_objective_discrepancy),n),
    sprintf("Endpoint changes above %.3g training SD: %d/%d.",
      inputs$config$comparisons$endpoint_training_sd_tolerance,sum(d$endpoint_sensitivity),n),
    sprintf("Posterior screen pass/fail changes: %d/%d.",sum(d$screen_changed),n),"",
    "## Interpretation","",
    "A small reference objective gap supports numerical near-optimality only for the fixed q and tilt studied. A nonzero midpoint gradient is not a nonsmooth optimality test. Nearest-kink calculations are backward-error diagnostics and never replace saved intervals.",
    "",
    "Candidate-menu reference comparisons preserve the original q/tilt menu and DP threshold. They are counterfactual numerical checks, not new validation evidence. Objective levels are compared only within the same q/tilt target.",
    "",
    if(any(d$objective_discrepancy | d$endpoint_sensitivity | d$screen_changed))
      "Some prespecified comparisons detect numerical sensitivity. Investigate its size and cause before any algorithm change; a revised selection rule needs independent simulation validation. Do not tune calibration to these responses." else
      "No material sensitivity was detected at the prespecified tolerances in the sampled cases. Retain the frozen procedure; explain the diagnostic limitations without relabeling historical flags.",
    "",
    "Trace plots show actual endpoint/width/criterion paths. Fitted content and tilt are fixed within each ECM run and therefore appear in headings rather than fictitious parameter traces.",
    "",
    paste0("Figure report: ",basename(pdf_path)),
    "Detailed continuous values: case_decisions.csv, summary.csv, references.csv, candidates.csv, all_saved_diagnostics.csv, iteration_traces.csv.gz.")
  writeLines(lines,file.path(directory,"findings.md"))
}
