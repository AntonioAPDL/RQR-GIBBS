mpq_frozen_action <- function(y,content,confidence,policy,cfg) {
  task <- data.frame(n=length(y),content=content,tolerance_confidence=confidence)
  required <- c("n","content","tolerance_confidence","threshold","method_id","decision")
  mpr_require(all(required %in% names(policy)),"Incomplete frozen MTI policy.")
  chosen <- policy[mpr_cell_key(policy)==mpr_cell_key(task),,drop=FALSE]
  mpr_require(nrow(chosen)==1L && chosen$method_id=="mti_profile_mass_cells_v1" &&
    chosen$decision=="passed_new_calibration" && is.finite(chosen$threshold) &&
    chosen$threshold>0 && chosen$threshold<1,"No calibrated policy for this application setting.")
  context <- list(profile=mp_frozen_profile(cfg),
    scan=read.csv("tables/tolerance_validation_article_scan_calibration.csv"))
  fit <- mpq_fit(y,task,cfg,context)
  selected <- mp_select(fit$candidates,chosen$threshold)
  list(method_id=chosen$method_id,computation="fixed_target_profile",selected=selected,
    returned=nrow(selected)==1L,posterior_threshold=chosen$threshold,
    candidates=fit$candidates,continuous_optimum_claim=FALSE,
    universal_frequentist_guarantee=FALSE)
}

mpq_preview_tables <- function(summary,spec,output) {
  source_methods <- c(spec$primary_method,"young_mathew","wilks")
  methods <- c(spec$primary_method,"young_mathew","wilks_minmax")
  labels <- setNames(c("MTI","Young--Mathew","Wilks"),methods)
  families <- c(normal="Gaussian",student_t3="Student t(3)",exponential="Exponential",
    asym_laplace_tau010="Asymmetric Laplace (tau=0.10)",
    two_piece_normal_1_12="Two-piece Normal (1:12)",beta18="Beta(1,8)",
    gamma05="Gamma(0.5,1)",lognormal_hard="Log-normal (log sd 1.25)")
  s <- summary[summary$method %in% source_methods,,drop=FALSE]
  mpr_require(setequal(unique(s$method),source_methods) && all(s$dgp_id %in% names(families)),
    "Unexpected publication methods or distributions.")
  s$source_method_id <- s$method
  s$method_id <- methods[match(s$method,source_methods)]
  s$method <- unname(labels[s$method_id])
  s$dgp <- unname(families[s$dgp_id])
  s$mean_width <- s$width_mean
  pma_write_csv(s,file.path(output,"dgp_width_ranges.csv"))
  pma_write_csv(s[c("dgp_id","dgp","n","content","method_id","replications","successes",
    "returned","attainment","attainment_lower","attainment_upper","simultaneous_lower",
    "simultaneous_upper","evidence")],file.path(output,"dgp_attainment.csv"))
  ranges <- do.call(rbind,lapply(split(s,interaction(s$n,s$content,s$method_id,drop=TRUE)),function(x)
    data.frame(n=x$n[1],content=x$content[1],method_id=x$method_id[1],method=x$method[1],
      delivery_min=min(x$attainment),delivery_max=max(x$attainment))))
  ranges <- ranges[order(ranges$n,ranges$content,match(ranges$method_id,methods)),]
  pma_write_csv(ranges,file.path(output,"scenario_ranges.csv"))
  interval <- function(lo,hi,digits=1L) {
    if(is.na(lo) || is.na(hi))return("--")
    paste0(formatC(lo,format="f",digits=digits),"--",formatC(hi,format="f",digits=digits))
  }
  lines <- c("\\begin{tabular}{@{}l@{\\hspace{0.75em}}l@{\\hspace{0.75em}}r@{}}",
    "\\toprule","Design & Method & Content-attainment range (\\%)\\\\","\\midrule")
  keys <- paste(ranges$n,ranges$content)
  for(key in unique(keys)) {
    rows <- ranges[keys==key,]
    for(i in seq_len(nrow(rows)))lines <- c(lines,sprintf("%s & %s & %s \\\\",
      if(i==1L)sprintf("\\(n=%d,c=%.2f\\)",rows$n[i],rows$content[i]) else "",
      rows$method[i],interval(100*rows$delivery_min[i],100*rows$delivery_max[i])))
    lines <- c(lines,"\\addlinespace[0.25em]")
  }
  writeLines(c(lines,"\\bottomrule","\\end{tabular}"),file.path(output,"scenario_ranges.tex"))
  lines <- c("\\begingroup","\\scriptsize","\\setlength{\\tabcolsep}{3pt}",
    "\\begin{longtable}{@{}rrlrr@{}}",
    "\\caption{\\textbf{Interval widths by distribution and design.} Width ranges are empirical 2.5\\%--97.5\\% quantiles within each case and method.}\\\\",
    "\\toprule","\\(n\\) & \\(c\\) & Method & Attainment (\\%) & Width 95\\% range\\\\",
    "\\midrule","\\endfirsthead","\\toprule",
    "\\(n\\) & \\(c\\) & Method & Attainment (\\%) & Width 95\\% range\\\\",
    "\\midrule","\\endhead")
  for(family in names(families)) {
    d <- s[s$dgp_id==family,]; d <- d[order(d$n,d$content,match(d$method_id,methods)),]
    lines <- c(lines,sprintf("\\multicolumn{5}{@{}l}{\\textit{%s}}\\\\",families[family]))
    key <- paste(d$n,d$content)
    for(k in unique(key)) {
      rows <- d[key==k,]
      for(i in seq_len(nrow(rows)))lines <- c(lines,sprintf("%s & %s & %s & %.1f & %s \\\\",
        if(i==1L)as.character(rows$n[i]) else "",if(i==1L)sprintf("%.2f",rows$content[i]) else "",
        rows$method[i],100*rows$attainment[i],interval(rows$width_q025[i],rows$width_q975[i],3L)))
      lines <- c(lines,"\\addlinespace[0.25em]")
    }
  }
  writeLines(c(lines,"\\bottomrule","\\end{longtable}","\\endgroup"),file.path(output,"dgp_width_ranges.tex"))
  invisible(ranges)
}

mpq_export_review <- function(run,output,cfg,spec,design,figures=TRUE) {
  run <- normalizePath(run,mustWork=TRUE)
  output <- mpr_output_path(output)
  mpr_require(identical(output,file.path(run,"review")),"Review output must be the isolated run/review directory.")
  calibration <- mpr_read_json(file.path(run,"calibration","completion.json"))
  validation <- mpr_read_json(file.path(run,"validation","completion.json"))
  mpr_require(!is.null(calibration) && isTRUE(calibration$passed) &&
    !is.null(validation) && isTRUE(validation$passed),"Complete calibration and independent validation are required.")
  policy <- file.path(run,"calibration","policy.csv")
  hash <- mp_sha(policy)
  mpr_require(identical(hash,calibration$policy_sha256) &&
    identical(hash,validation$frozen_policy_sha256),"Review policy is not the independently validated policy.")
  if(!is.null(validation$results_sha256))mpr_require(identical(validation$results_sha256,
    mp_sha(file.path(run,"validation","results.csv"))),"Independent validation results changed.")
  if(dir.exists(output)) {
    old <- mpr_read_json(file.path(output,"review_manifest.json"))
    mpr_require(!is.null(old) && identical(old$policy_sha256,hash) &&
      identical(old$method_id,spec$primary_method),"Preserve the incomplete or incompatible previous review.")
    for(records in list(old$inputs,old$outputs))for(i in seq_len(nrow(records)))
      mpr_require(identical(mp_sha(records$path[i]),records$sha256[i]),"Completed review evidence changed.")
    return(invisible(output))
  }
  z <- read.csv(file.path(run,"validation","results.csv"))
  context <- mpq_context(cfg)
  mpr_validate_results(z,design,cfg,lapply(context$laws,`[[`,"p"),primary_method=spec$primary_method)
  summary <- mpr_validation_summary(z,design,cfg,list(primary_method=spec$primary_method))
  dir.create(output,recursive=TRUE)
  mpq_preview_tables(summary,spec,output)
  if(figures) {
    common <- c(paste0("--output-dir=",output),paste0("--mti-method-id=",spec$primary_method),"--mti-label=MTI")
    calls <- list(c("figures/generate_tolerance_validation_primary_figure.R","--primary-results=",
      "--mti-ecm-results=",paste0("--scenario-range-csv=",file.path(output,"scenario_ranges.csv")),
      paste0("--delivery-y-min=",min(.85,summary$attainment)),common),
      c("figures/generate_tolerance_validation_width_figure.R",
        paste0("--width-csv=",file.path(output,"dgp_width_ranges.csv")),common))
    for(args in calls)mpr_require(system2("Rscript",shQuote(args))==0L,"A review figure could not be generated.")
  }
  records <- pma_file_record(c(policy,file.path(run,"validation","results.csv"),
    file.path(run,c("calibration/completion.json","validation/completion.json"))))
  writeLines(c("# Independent MTI Comparison: Publication Review","",
    "These tables and figures are local review copies. No manuscript input has been replaced.",
    sprintf("Independent datasets: %d; method evaluations: %d; nonreturned intervals: %d.",
      nrow(design)*cfg$validation$replications,nrow(z),sum(!z$returned)),
    sprintf("Family/method cases without a defined width range: %d.",sum(is.na(summary$width_q025))),
    "Attainment ranges describe variation across distributions; width ranges are empirical 95% ranges within a distribution and case.",
    "Review the independent family-level binomial intervals, calibration outcomes, and paired differences before an adoption decision.",
    "The numerical procedure uses the profiled fixed-target MTI solver. Its empirical calibration does not establish a distribution-free theorem."),
    file.path(output,"REPORT.md"))
  pta_atomic_write_json(list(method_id=spec$primary_method,policy_sha256=hash,inputs=records,
    main_methods=list(spec$primary_method,"young_mathew","wilks"),
    publication_replaced=FALSE,adoption_requires_scientific_review=TRUE,
    outputs=pma_file_record(list.files(output,full.names=TRUE))),file.path(output,"review_manifest.json"))
  invisible(output)
}
