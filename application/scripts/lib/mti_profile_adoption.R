mpa_default <- "application/config/mti_profile_adopted_20261025.json"
mpa_application_default <- "application/config/pharma_tolerance_application_profiled_20261025.json"

mpa_import <- function(run) {
  run <- normalizePath(run,mustWork=TRUE)
  done <- mpr_read_json(file.path(run,"followup_study_completion.json"))
  exit <- mpr_read_json(file.path(run,"followup_exit.json"))
  mpr_require(identical(done$decision,"complete_requires_scientific_adoption_review") &&
    exit$exit_status==0 && !done$article_inputs_replaced,"Incomplete follow-up study.")
  combined <- file.path(run,"combined")
  cal <- mpr_read_json(file.path(combined,"calibration/completion.json"))
  val <- mpr_read_json(file.path(combined,"validation/completion.json"))
  paths <- c(policy=file.path(combined,"calibration/policy.csv"),
    assessment=file.path(combined,"calibration/threshold_assessment.csv"),
    results=file.path(combined,"validation/results.csv"))
  stopifnot(cal$passed,val$passed,val$completed_datasets==72000L,
    identical(mp_sha(paths[["policy"]]),cal$policy_sha256),
    identical(cal$policy_sha256,val$frozen_policy_sha256),
    identical(mp_sha(paths[["assessment"]]),cal$assessment_sha256),
    identical(mp_sha(paths[["results"]]),val$results_sha256))
  spec_path <- "application/config/mti_threshold_followup_20261024.json"
  spec <- jsonlite::read_json(spec_path,simplifyVector=FALSE)
  cfg <- mpq_config(spec); design <- mpr_design(mp_frozen_profile(cfg)$validation)
  z <- read.csv(paths[["results"]]); context <- mpq_context(cfg)
  mpr_validate_results(z,design,cfg,lapply(context$laws,`[[`,"p"),spec$primary_method)
  summary <- mpr_validation_summary(z,design,cfg,list(primary_method=spec$primary_method))
  paired <- mpr_paired_summary(z,list(primary_method=spec$primary_method))
  primary <- z[z$method==spec$primary_method,]
  mpr_require(all(primary$returned) && all(primary$numerical_failure_candidates==0),
    "Independent validation has unresolved computational failures.")
  policy <- read.csv(paths[["policy"]]); assessment <- read.csv(paths[["assessment"]])
  selected <- assessment[assessment$threshold==policy$threshold[
    match(mpr_cell_key(assessment),mpr_cell_key(policy))],]
  mpr_require(nrow(policy)==9L && nrow(selected)==72L &&
    all(selected$returned==selected$replications & selected$lower_bound>=.95),
    "Selected calibration evidence does not pass.")
  out <- "figures/data/mti_profile_validation_20261025"
  mpr_require(!dir.exists(out),"Preserve existing adopted evidence; import only once.")
  dir.create(out,recursive=TRUE)
  # Copy only small evidence; full simulation checkpoints remain local.
  for(name in c("policy","assessment"))file.copy(paths[[name]],file.path(out,paste0(name,".csv")))
  pma_write_csv(selected,file.path(out,"selected_calibration.csv"))
  keep <- summary$method %in% c(spec$primary_method,"young_mathew","wilks")
  pma_write_csv(summary[keep,],file.path(out,"family_case_summary.csv"))
  pma_write_csv(paired[paired$comparator %in% c("young_mathew","wilks"),],
    file.path(out,"paired_comparisons.csv"))
  pma_write_csv(read.csv(file.path(combined,"calibration/policy_sources.csv")),
    file.path(out,"policy_sources.csv"))
  frozen <- readRDS(file.path(run,"identity.rds"))
  record <- list(study_commit=frozen$commit,policy_sha256=cal$policy_sha256,
    results_sha256=val$results_sha256,assessment_sha256=cal$assessment_sha256,
    datasets=72000L,reported_method_evaluations=216000L,
    full_method_evaluations=288000L,calibration_familywise_error=.05,
    independent_validation=TRUE,paired_comparison=TRUE,
    primary_evidence=as.list(table(summary$evidence[summary$method==spec$primary_method])),
    distribution_free_tolerance_theorem=FALSE,artifacts=pma_file_record(list.files(out,full.names=TRUE)))
  pta_atomic_write_json(record,file.path(out,"provenance.json"))
  numerical <- c(list.files("application/R",full.names=TRUE),
    list.files("application/src",pattern="[.](cpp|h)$|^Makevars",full.names=TRUE),
    cfg$validation_config,cfg$historical_policy,spec_path,spec$revision_config,
    "tables/tolerance_validation_article_scan_calibration.csv")
  adoption <- list(schema_version="mti_profile_adoption/1.0.0",method_id=spec$primary_method,
    study_spec=spec_path,policy_csv=file.path(out,"policy.csv"),policy_sha256=cal$policy_sha256,
    evidence_dir=out,study_commit=frozen$commit,computation="fixed_target_profile",
    native_sha256=frozen$native_sha256,evidence_provenance_sha256=mp_sha(file.path(out,"provenance.json")),
    numerical_inputs=pma_file_record(numerical),
    interpretation="Finite-panel simulation calibration; no unrestricted tolerance theorem.")
  pta_atomic_write_json(adoption,mpa_default)
  app <- jsonlite::read_json("application/config/pharma_tolerance_application_mti_only_20261020.json",
    simplifyVector=FALSE)
  app$study_id <- "pharma_mti_profile_application_20261025"
  app$schema_version <- "rqrgibbs_pharma_mti_application/3.0.0"
  app$mti_ecm <- NULL
  app$mti_profile <- list(adoption_config=mpa_default,adoption_sha256=mp_sha(mpa_default))
  app$method_labels$mti_ecm_adaptive_cell <- "MTI"
  app$paths$output_root <- "application/outputs/pharma_mti_profile_application_20261025"
  app$paths$canonical_dir <- "figures/data/pharma_mti_profile_application_20261025"
  app$historical_evidence <- "figures/data/pharma_mti_application_20261020"
  app$historical_results_sha256 <- mp_sha(file.path(app$historical_evidence,"results.csv"))
  app$observed_sha256 <- mp_sha(file.path(app$historical_evidence,"observed_batches.csv"))
  app$membership_sha256 <- mp_sha(file.path(app$historical_evidence,"split_membership.csv.gz"))
  pta_atomic_write_json(app,mpa_application_default)
  invisible(out)
}

mpa_read <- function(path=mpa_default) {
  a <- mpr_read_json(path)
  mpr_require(!is.null(a) && a$method_id=="mti_profile_mass_cells_v1" &&
    a$computation=="fixed_target_profile","Unknown adopted MTI specification.")
  pma_verify_hash(a$policy_csv,a$policy_sha256)
  provenance <- file.path(a$evidence_dir,"provenance.json")
  pma_verify_hash(provenance,a$evidence_provenance_sha256)
  evidence <- mpr_read_json(provenance)$artifacts
  for(i in seq_len(nrow(evidence)))pma_verify_hash(evidence$path[i],evidence$sha256[i])
  for(i in seq_len(nrow(a$numerical_inputs)))
    pma_verify_hash(a$numerical_inputs$path[i],a$numerical_inputs$sha256[i])
  spec <- jsonlite::read_json(a$study_spec,simplifyVector=FALSE)
  cfg <- mpq_config(spec); policy <- read.csv(a$policy_csv)
  design <- mpr_design(mp_frozen_profile(cfg)$validation)
  mpr_require(nrow(policy)==9L && !anyDuplicated(mpr_cell_key(policy)) &&
    setequal(mpr_cell_key(policy),mpr_cell_key(design)) &&
    all(policy$method_id==a$method_id & policy$decision=="passed_new_calibration" &
      is.finite(policy$threshold) & policy$threshold>0 & policy$threshold<1),
    "Adopted policy is incomplete or unresolved.")
  list(adoption=a,spec=spec,cfg=cfg,policy=policy,context=mpq_context(cfg))
}

mpa_application_context <- function(config) {
  pma_validate_config(config)
  pma_verify_hash(config$mti_profile$adoption_config,config$mti_profile$adoption_sha256)
  adopted <- mpa_read(config$mti_profile$adoption_config)
  root <- config$historical_evidence
  files <- file.path(root,c("results.csv","observed_batches.csv","split_membership.csv.gz"))
  for(i in seq_along(files))pma_verify_hash(files[i],
    c(config$historical_results_sha256,config$observed_sha256,config$membership_sha256)[i])
  data <- read.csv(files[2]); splits <- pma_make_splits(data,config,"confirmatory")
  previous <- read.csv(gzfile(files[3]))
  mpr_require(isTRUE(all.equal(pma_membership(splits,data),previous,check.attributes=FALSE)),
    "New application split membership differs from historical splits.")
  adopted$calculation_paths <- c("application/scripts/lib/mti_profile_adoption.R",
    "application/scripts/lib/mti_final_reporting.R","application/scripts/lib/mti_q_grid_study.R",
    "application/scripts/lib/mti_profile_revision.R","application/scripts/101_adopt_mti_profile.R")
  adopted$input_paths <- c(files,config$mti_profile$adoption_config,
    adopted$adoption$policy_csv,adopted$adoption$numerical_inputs$path)
  scan <- read.csv(config$scan_reference$scan_calibration_csv)
  scan <- scan[scan$n==100 & abs(scan$content-.9)<1e-12,]
  scan$retained_fraction <- scan$retained_count/scan$n
  list(config=config,data=data,repo_root=getwd(),scan=scan,adopted=adopted)
}

mpa_check_runtime <- function(adopted) {
  mpr_require(as.character(packageVersion("rqrgibbs"))=="0.1.0.9038",
    "Install the frozen numerical package version before evaluation.")
  reference <- new.env(parent=asNamespace("rqrgibbs"))
  files <- c(rqr_mti_intercept_fit="rqr_mti_intercept_profile.R",
    rqr_mti_dp_profile="rqr_mti_profile_selection.R",
    rqr_mti_refine_content_grid="rqr_mti_content_grid.R")
  for(name in names(files)) {
    sys.source(file.path("application/R",files[[name]]),reference)
    installed <- getFromNamespace(name,"rqrgibbs")
    expected <- get(name,reference)
    mpr_require(identical(formals(installed),formals(expected)) &&
      identical(deparse(body(installed)),deparse(body(expected))),
      "Installed numerical R code differs from the frozen source.")
  }
  # A rebuilt native binary need not have the original machine's byte hash.
  for(y in list(seq(-2,2,length.out=50),-log(seq(.01,.99,length.out=100)))) {
    fits <- lapply(c("R","cpp"),function(backend)
      rqrgibbs::rqr_mti_intercept_fit(y,.98,0,backend=backend))
    mpr_require(all(vapply(fits,function(f)isTRUE(f$converged),logical(1))) &&
      abs(fits[[1]]$objective-fits[[2]]$objective)<=
        adopted$cfg$profile$objective_tolerance*(1+abs(fits[[1]]$objective)),
      "Rebuilt native and R profile calculations disagree.")
  }
  invisible(TRUE)
}

mpa_application_fit <- function(y,context) {
  a <- context$adopted; cfg <- context$config
  fit <- mpq_frozen_action(y,cfg$analysis$content,cfg$analysis$tolerance_confidence,
    a$policy,a$cfg,a$context)
  candidates <- fit$candidates
  good <- all(candidates$profile_converged) &&
    all(is.finite(candidates$relative_objective_gap)) &&
    all(abs(candidates$relative_objective_gap)<=a$cfg$profile$objective_tolerance)
  mpr_require(good,"A profiled application candidate fails its objective requirement.")
  s <- fit$selected
  if(!fit$returned)return(list(returned=FALSE,computation=fit$computation,
    failure_reason="No eligible interval under the fixed calibrated cutoff"))
  list(returned=TRUE,lower=s$lower,upper=s$upper,width=s$width,
    selected_q=s$target_content,selected_tilt=s$mean_tilt,central_tilt=s$central_tilt,
    posterior_content_probability=s$posterior_content_probability,
    posterior_confidence=fit$posterior_threshold,candidates_evaluated=nrow(candidates),
    feasible_count=sum(candidates$posterior_content_probability>=fit$posterior_threshold),
    computation=fit$computation,profile_converged=TRUE,profile_iterations=s$iterations,
    relative_objective_gap=s$relative_objective_gap,
    maximum_candidate_objective_gap=max(abs(candidates$relative_objective_gap)),failure_reason="")
}

mpa_export_tables <- function(output="tables") {
  a <- mpa_read(); out <- a$adoption$evidence_dir
  summary <- read.csv(file.path(out,"family_case_summary.csv"))
  mpq_preview_tables(summary,a$spec,output)
  map <- c(scenario_ranges="tolerance_validation_article_scenario_ranges",
    dgp_width_ranges="tolerance_validation_article_dgp_width_ranges",
    dgp_attainment="tolerance_validation_article_dgp_delivery")
  for(name in names(map))for(ext in c("csv","tex")) {
    source <- file.path(output,paste0(name,".",ext))
    if(file.exists(source))file.rename(source,file.path(output,paste0(map[[name]],".",ext)))
  }
  s <- summary[order(summary$dgp_id,summary$n,summary$content,summary$method),]
  lines <- c("\\begingroup","\\footnotesize","\\setlength{\\tabcolsep}{3pt}",
    "\\begin{longtable}{@{}rrlrr@{}}",
    "\\caption{\\textbf{Content attainment by distribution.} Each estimate uses 1000 paired samples. Brackets give pointwise exact 95\\% Monte Carlo confidence intervals.}\\label{tab:supp-primary-dgp-delivery}\\\\",
    "\\toprule","\\(n\\) & \\(c\\) & Method & Attainment (\\%) & 95\\% confidence interval\\\\",
    "\\midrule","\\endfirsthead","\\toprule",
    "\\(n\\) & \\(c\\) & Method & Attainment (\\%) & 95\\% confidence interval\\\\",
    "\\midrule","\\endhead")
  labels <- setNames(c("MTI","Young--Mathew","Wilks"),c(a$spec$primary_method,"young_mathew","wilks"))
  for(family in unique(s$dgp_id)) {
    d <- s[s$dgp_id==family,]
    family_label <- read.csv(file.path(output,"tolerance_validation_article_dgp_width_ranges.csv"))
    family_label <- family_label$dgp[match(family,family_label$dgp_id)]
    lines <- c(lines,sprintf("\\multicolumn{5}{@{}l}{\\textit{%s}}\\\\",family_label))
    keys <- mpr_cell_key(d)
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
  cal <- read.csv(file.path(out,"selected_calibration.csv"))
  policy <- a$policy
  lines <- c("\\begin{tabular}{@{}rrrrr@{}}","\\toprule",
    "\\(n\\) & \\(c\\) & Cutoff \\(\\gamma\\) & Calibration lower bound & Validation attainment (\\%)\\\\","\\midrule")
  for(i in seq_len(nrow(policy))) {
    r <- policy[i,]; v <- summary[summary$method==a$spec$primary_method &
      mpr_cell_key(summary)==mpr_cell_key(r),]; c <- cal[mpr_cell_key(cal)==mpr_cell_key(r),]
    lines <- c(lines,sprintf("%d & %.2f & %.6f & %.4f & %.1f--%.1f \\\\",
      r$n,r$content,r$threshold,min(c$lower_bound),100*min(v$attainment),100*max(v$attainment)))
  }
  writeLines(c("\\begin{table}[!htbp]","\\centering",
    "\\caption{\\textbf{Calibrated MTI probability cutoffs.} The lower bound is the smallest simultaneous one-sided calibration bound across eight distributions, with 24000 assessment samples per distribution. Validation ranges span the eight independent estimates.}\\label{tab:supp-mti-profile-policy}",
    "\\TableStyle",lines,"\\bottomrule","\\end{tabular}","\\end{table}"),
    file.path(output,"tolerance_validation_mti_profile_policy.tex"))
  invisible(output)
}
