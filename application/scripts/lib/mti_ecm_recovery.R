mre_default <- "application/config/mti_ecm_article_20261111.json"

mre_config <- function(path=mre_default) {
  cfg <- jsonlite::read_json(path,simplifyVector=TRUE)
  mpr_require(identical(cfg$computation,"finite_iteration_ecm") &&
    identical(cfg$primary_method,"mti_ecm_adaptive_cell"),
    "This recovery accepts only the archived ECM calculation.")
  pma_verify_hash(cfg$validation_config,cfg$validation_config_sha256)
  pma_verify_hash(cfg$policy_csv,cfg$policy_sha256)
  cfg
}

mre_check_identity <- function(z,cfg) {
  mpr_require(nrow(z)==cfg$expected_datasets && all(z$method_id==cfg$primary_method) &&
    all(z$ecm_backend=="cpp") && all(is.na(z$posterior_draws) | z$posterior_draws==0) &&
    all(z$uq_engine=="mti_ecm_direct_dp"),"Archived rows are not the specified ECM study.")
  key <- paste(z$dgp_id,z$n,z$guaranteed_content,z$tolerance_confidence,z$replication)
  mpr_require(!anyDuplicated(key) && all(table(paste(z$dgp_id,z$n,z$guaranteed_content))==
    cfg$replications),"Archived dataset identities are incomplete or duplicated.")
  returned <- !z$infeasible & is.finite(z$lower) & is.finite(z$upper) & z$upper>=z$lower
  mpr_require(all(returned) && all(z$success==(z$content>=z$guaranteed_content)),
    "Return or population-content records disagree.")
  invisible(TRUE)
}

mre_frozen_functions <- function(cfg) {
  dir.create(cfg$cache_dir,recursive=TRUE,showWarnings=FALSE)
  paths <- c("application/scripts/69_validate_rqr_bayes_uq.R",
    "application/R/rqr_mti_ecm_adaptive.R")
  exported <- file.path(cfg$cache_dir,basename(paths))
  for(i in seq_along(paths)) {
    status <- system2("git",c("show",shQuote(paste0(cfg$archived_commit,":",paths[i]))),
      stdout=exported[i])
    mpr_require(status==0L,"Cannot read the pinned historical source.")
  }
  laws <- mp_dgp_environment(exported[1])
  e <- new.env(parent=baseenv())
  e$.rqr_bayes_clean_y <- function(y,na_rm=FALSE) {
    stopifnot(all(is.finite(y))); as.numeric(y)
  }
  for(expr in parse(exported[2]))if(is.call(expr) && identical(expr[[1]],as.name("<-")) &&
    is.symbol(expr[[2]]) && as.character(expr[[2]])=="rqr_mti_ecm_sample_diagnostics")eval(expr,e)
  list(laws=laws,diagnostics=e$rqr_mti_ecm_sample_diagnostics,
    source_files=pma_file_record(exported))
}

mre_content_check <- function(lower,upper,content,cdf) {
  # Decimal endpoint storage is amplified by a density singularity at a boundary.
  epsilon <- 32*.Machine$double.eps*pmax(1,abs(lower),abs(upper))
  lo <- cdf(upper-epsilon)-cdf(lower+epsilon)
  hi <- cdf(upper+epsilon)-cdf(lower-epsilon)
  all(content>=lo-1e-14 & content<=hi+1e-14)
}

mre_replay <- function(row,law,diagnostics) {
  set.seed(row$seed)
  y <- law$r(row$n)
  d <- diagnostics(y)
  mpr_require(abs(d$bowley_skewness-row$sample_bowley_skewness)<1e-10 &&
    abs(d$robust_tail_ratio-row$sample_tail_ratio)<1e-10,
    "Replayed sample does not match archived sample diagnostics.")
  mpr_require(mre_content_check(row$lower,row$upper,row$content,law$p),
    "Archived population content disagrees with the pinned distribution.")
  y
}

mre_results <- function(cfg,archive_root,workers=32L) {
  mpr_require(workers>=1L && workers<=40L,"Recovery workers must be between 1 and 40.")
  mpr_require(identical(as.character(packageVersion("tolerance")),cfg$tolerance_package_version),
    "The pinned tolerance package version is required.")
  path <- file.path(archive_root,cfg$archived_results)
  pma_verify_hash(path,cfg$archived_results_sha256)
  if(file.exists(file.path(cfg$evidence_dir,"provenance.json")) &&
    file.exists(file.path(cfg$cache_dir,"results.csv"))) {
    mre_verify(cfg,full=TRUE)
    message("Verified the complete recovery; no fits were repeated.")
    return(invisible(read.csv(file.path(cfg$evidence_dir,"family_case_summary.csv"))))
  }
  z <- as.data.frame(data.table::fread(path,nThread=1))
  mre_check_identity(z,cfg)
  frozen <- mre_frozen_functions(cfg)
  validation <- jsonlite::read_json(cfg$validation_config,simplifyVector=FALSE)
  laws <- setNames(lapply(validation$dgps,frozen$laws$dgp_meta),
    vapply(validation$dgps,`[[`,"", "dgp_id"))
  design <- unique(data.frame(n=z$n,content=z$guaranteed_content,
    tolerance_confidence=z$tolerance_confidence,dgp_id=z$dgp_id))
  design <- design[order(design$n,design$content,design$dgp_id),]
  design$cell_index <- seq_len(nrow(design))
  key <- function(x)paste(x$dgp_id,x$n,x$content,x$tolerance_confidence)
  index <- match(paste(z$dgp_id,z$n,z$guaranteed_content,z$tolerance_confidence),key(design))
  chunks <- file.path(cfg$cache_dir,sprintf("case_%03d.rds",design$cell_index))
  RNGkind(kind=cfg$rng_kind[1],normal.kind=cfg$rng_kind[2],sample.kind=cfg$rng_kind[3])
  calculation_commit <- pma_git("rev-parse","HEAD")
  identity <- list(archived_results_sha256=cfg$archived_results_sha256,
    validation_config_sha256=mp_sha(cfg$validation_config),policy_sha256=mp_sha(cfg$policy_csv),
    sources=frozen$source_files,calculation_commit=calculation_commit,
    tolerance_version=as.character(packageVersion("tolerance")),rng=RNGkind())
  identity_path <- file.path(cfg$cache_dir,"identity.rds")
  if(file.exists(identity_path)) {
    old <- readRDS(identity_path)
    mpr_require(identical(old,identity),"Preserve the previous recovery: its calculation identity changed.")
  } else saveRDS(identity,identity_path)
  work <- function(cell) {
    if(file.exists(chunks[cell]))return(chunks[cell])
    s <- z[index==cell,]; s <- s[order(s$replication),]
    law <- laws[[s$dgp_id[1]]]
    rows <- lapply(seq_len(nrow(s)),function(i) {
      row <- s[i,]; y <- mre_replay(row,law,frozen$diagnostics)
      ym <- pta_fit_young_mathew(y,row$guaranteed_content,row$tolerance_confidence)
      mpr_require(isTRUE(ym$returned),paste("Young-Mathew failed:",ym$failure_reason))
      lower <- c(row$lower,ym$lower,min(y)); upper <- c(row$upper,ym$upper,max(y))
      actual <- c(row$content,law$p(upper[-1])-law$p(lower[-1]))
      data.frame(cell_index=cell,n=row$n,content=row$guaranteed_content,
        tolerance_confidence=row$tolerance_confidence,dgp_id=row$dgp_id,
        replication=row$replication,seed=row$seed,
        dataset_sha256=digest::digest(y,algo="sha256"),
        method=c(cfg$primary_method,"young_mathew","wilks"),returned=TRUE,
        lower=lower,upper=upper,width=upper-lower,true_content=actual,
        content_attained=actual>=row$guaranteed_content,
        numerical_failure_candidates=0L,
        ecm_converged=c(row$ecm_converged,NA,NA),
        ecm_iterations=c(row$ecm_iterations,NA,NA),
        ecm_stationarity=c(row$ecm_final_stationarity,NA,NA),
        selected_q=c(row$target_content,NA,NA),selected_delta=c(row$target_mean_tilt,NA,NA))
    })
    out <- do.call(rbind,rows)
    temp <- paste0(chunks[cell],".tmp")
    saveRDS(out,temp); stopifnot(file.rename(temp,chunks[cell]))
    chunks[cell]
  }
  answers <- parallel::mclapply(design$cell_index,work,mc.cores=workers,mc.preschedule=FALSE,
    mc.set.seed=FALSE)
  mpr_require(!any(vapply(answers,inherits,logical(1),"try-error")) &&
    all(file.exists(chunks)),"At least one recovery case failed; completed cases were retained.")
  result <- do.call(rbind,lapply(chunks,readRDS))
  mpr_require(nrow(result)==3L*cfg$expected_datasets,"Incomplete comparison results.")
  result_path <- file.path(cfg$cache_dir,"results.csv")
  data.table::fwrite(result,result_path,nThread=1)
  report_cfg <- list(calibration=list(familywise_confidence=.95))
  summary <- mpr_validation_summary(result,design,report_cfg,cfg)
  paired <- mpr_paired_summary(result,cfg)
  diagnostics <- do.call(rbind,lapply(split(result[result$method==cfg$primary_method,],
    result$cell_index[result$method==cfg$primary_method]),function(s)
      data.frame(s[1,c("cell_index","n","content","dgp_id")],
        replications=nrow(s),ecm_flag_passed=sum(s$ecm_converged),
        iteration_limit=sum(s$ecm_iterations>=100),
        stationarity_median=median(s$ecm_stationarity),
        stationarity_q975=unname(quantile(s$ecm_stationarity,.975,type=8)),
        selected_q_min=min(s$selected_q),selected_q_max=max(s$selected_q),
        selected_delta_min=min(s$selected_delta),selected_delta_max=max(s$selected_delta))))
  dir.create(cfg$evidence_dir,recursive=TRUE,showWarnings=FALSE)
  pma_write_csv(summary,file.path(cfg$evidence_dir,"family_case_summary.csv"))
  pma_write_csv(paired,file.path(cfg$evidence_dir,"paired_comparisons.csv"))
  pma_write_csv(diagnostics,file.path(cfg$evidence_dir,"ecm_diagnostics.csv"))
  pma_write_csv(design,file.path(cfg$evidence_dir,"design.csv"))
  pma_write_csv(read.csv(cfg$policy_csv),file.path(cfg$evidence_dir,"policy.csv"))
  evidence <- pma_file_record(list.files(cfg$evidence_dir,full.names=TRUE))
  pta_atomic_write_json(c(identity,list(datasets=cfg$expected_datasets,
    method_evaluations=nrow(result),results_sha256=mp_sha(result_path),
    all_samples_replayed_and_checked=TRUE,paired_samples=TRUE,
    ecm_refitted=FALSE,gibbs_run=FALSE,artifacts=evidence)),
    file.path(cfg$evidence_dir,"provenance.json"))
  invisible(summary)
}

mre_tables <- function(cfg,output="tables") {
  provenance <- jsonlite::read_json(file.path(cfg$evidence_dir,"provenance.json"),simplifyVector=TRUE)
  for(i in seq_len(nrow(provenance$artifacts)))pma_verify_hash(
    provenance$artifacts$path[i],provenance$artifacts$sha256[i])
  summary <- read.csv(file.path(cfg$evidence_dir,"family_case_summary.csv"))
  mpq_preview_tables(summary,cfg,output)
  map <- c(scenario_ranges="tolerance_validation_article_scenario_ranges",
    dgp_width_ranges="tolerance_validation_article_dgp_width_ranges",
    dgp_attainment="tolerance_validation_article_dgp_delivery")
  for(name in names(map))for(ext in c("csv","tex")) {
    source <- file.path(output,paste0(name,".",ext))
    if(file.exists(source))stopifnot(file.rename(source,file.path(output,paste0(map[[name]],".",ext))))
  }
  lines <- c("\\begingroup","\\footnotesize","\\setlength{\\tabcolsep}{3pt}",
    "\\begin{longtable}{@{}rrlrr@{}}",
    "\\caption{\\textbf{Content attainment by distribution.} Each estimate uses 1000 paired samples. Brackets give pointwise exact 95\\% Monte Carlo confidence intervals.}\\label{tab:supp-primary-dgp-delivery}\\\\",
    "\\toprule","\\(n\\) & \\(c\\) & Method & Attainment (\\%) & 95\\% confidence interval\\\\",
    "\\midrule","\\endfirsthead","\\toprule",
    "\\(n\\) & \\(c\\) & Method & Attainment (\\%) & 95\\% confidence interval\\\\",
    "\\midrule","\\endhead")
  labels <- setNames(c("MTI-ECM","Young--Mathew","Wilks"),c(cfg$primary_method,"young_mathew","wilks"))
  widths <- read.csv(file.path(output,"tolerance_validation_article_dgp_width_ranges.csv"))
  for(family in unique(summary$dgp_id)) {
    d <- summary[summary$dgp_id==family,]; keys <- mpr_cell_key(d)
    lines <- c(lines,sprintf("\\multicolumn{5}{@{}l}{\\textit{%s}}\\\\",
      widths$dgp[match(family,widths$dgp_id)]))
    for(key in unique(keys)) {
      rows <- d[keys==key,]; rows <- rows[match(names(labels),rows$method),]
      for(i in seq_len(nrow(rows)))lines <- c(lines,sprintf("%s & %s & %s & %.1f & [%.1f, %.1f] \\\\",
        if(i==1L)rows$n[i] else "",if(i==1L)sprintf("%.2f",rows$content[i]) else "",
        labels[rows$method[i]],100*rows$attainment[i],100*rows$attainment_lower[i],100*rows$attainment_upper[i]))
      lines <- c(lines,"\\addlinespace[0.2em]")
    }
  }
  writeLines(c(lines,"\\bottomrule","\\end{longtable}","\\endgroup"),
    file.path(output,"tolerance_validation_article_dgp_delivery.tex"))
  policy <- read.csv(cfg$policy_csv)
  lines <- c("\\begin{table}[!htbp]","\\centering",
    "\\caption{\\textbf{Fixed MTI-ECM probability cutoffs.} The cutoffs were selected before independent validation. Calibration lower bounds are the smallest pointwise one-sided bounds across the eight distributions; they do not adjust for selection among cutoffs. Validation ranges span eight independent estimates.}\\label{tab:supp-mti-profile-policy}",
    "\\TableStyle","\\begin{tabular}{@{}rrrrr@{}}","\\toprule",
    "\\(n\\) & \\(c\\) & Cutoff \\(\\gamma\\) & Calibration lower bound & Validation attainment (\\%)\\\\","\\midrule")
  for(i in seq_len(nrow(policy))) {
    r <- policy[i,]; v <- summary[summary$method==cfg$primary_method &
      mpr_cell_key(summary)==mpr_cell_key(r),]
    lines <- c(lines,sprintf("%d & %.2f & %.6f & %.4f & %.1f--%.1f \\\\",
      r$n,r$content,r$screen,r$min_delivery_lower_bound,100*min(v$attainment),100*max(v$attainment)))
  }
  writeLines(c(lines,"\\bottomrule","\\end{tabular}","\\end{table}"),
    file.path(output,"tolerance_validation_mti_profile_policy.tex"))
  invisible(summary)
}

mre_verify <- function(cfg,full=FALSE) {
  provenance <- jsonlite::read_json(file.path(cfg$evidence_dir,"provenance.json"),simplifyVector=TRUE)
  mpr_require(identical(provenance$archived_results_sha256,cfg$archived_results_sha256) &&
    identical(provenance$validation_config_sha256,cfg$validation_config_sha256) &&
    identical(provenance$policy_sha256,cfg$policy_sha256) &&
    isTRUE(provenance$paired_samples) && !provenance$gibbs_run,
    "Recovery evidence is not the specified ECM comparison.")
  for(i in seq_len(nrow(provenance$artifacts)))pma_verify_hash(
    provenance$artifacts$path[i],provenance$artifacts$sha256[i])
  s <- read.csv(file.path(cfg$evidence_dir,"family_case_summary.csv"))
  mpr_require(nrow(s)==216L && all(s$replications==cfg$replications) &&
    all(s$returned==s$replications) && all(s$attainment==s$successes/s$replications) &&
    all(is.finite(s$width_mean) & s$width_mean>=0 &
      s$width_q025>=0 & s$width_q025<=s$width_q975),
    "The distribution-level evidence is incomplete or inconsistent.")
  if(full) {
    path <- file.path(cfg$cache_dir,"results.csv")
    pma_verify_hash(path,provenance$results_sha256)
    z <- as.data.frame(data.table::fread(path,nThread=1))
    key <- paste(z$cell_index,z$replication)
    primary <- z[z$method==cfg$primary_method,]
    for(method in c("young_mathew","wilks")) {
      b <- z[z$method==method,]; index <- match(paste(primary$cell_index,primary$replication),
        paste(b$cell_index,b$replication))
      mpr_require(!anyNA(index) && !anyDuplicated(index) &&
        all(primary$seed==b$seed[index]) && all(primary$dataset_sha256==b$dataset_sha256[index]),
        "A comparator is not paired to the ECM observations.")
    }
    mpr_require(nrow(z)==provenance$method_evaluations && all(table(key)==3L) &&
      !anyDuplicated(paste(key,z$method)),"Replicate/method identities are duplicated or missing.")
    for(i in seq_len(nrow(s))) {
      r <- s[i,]; a <- z[z$cell_index==r$cell_index & z$method==r$method,]
      q <- quantile(a$width,c(.025,.975),type=8,names=FALSE)
      mpr_require(nrow(a)==r$replications && sum(a$content_attained)==r$successes &&
        max(abs(q-c(r$width_q025,r$width_q975)))<1e-11,
        "Published counts or width quantiles disagree with replication-level results.")
    }
    frozen <- mre_frozen_functions(cfg)
    validation <- jsonlite::read_json(cfg$validation_config,simplifyVector=FALSE)
    for(dgp in validation$dgps) {
      law <- frozen$laws$dgp_meta(dgp); a <- z[z$dgp_id==dgp$dgp_id,]
      actual <- law$p(a$upper)-law$p(a$lower)
      mpr_require(mre_content_check(a$lower,a$upper,a$true_content,law$p) &&
        all((actual>=a$content)==a$content_attained),
        "Population-CDF evaluation disagrees with recorded attainment.")
    }
  }
  invisible(TRUE)
}
