mpf_policy <- function(cells,assessment,design) {
  expected <- unique(design[c("n","content","tolerance_confidence")])
  mpr_require(nrow(cells)==nrow(expected) && !anyDuplicated(mpr_cell_key(cells)) &&
    setequal(mpr_cell_key(cells),mpr_cell_key(expected)),"Policy cells do not match the study.")
  policies <- evidence <- list()
  for(i in seq_len(nrow(expected))) {
    cell <- expected[i,]; key <- mpr_cell_key(cell)
    old <- cells[mpr_cell_key(cells)==key,]
    s <- assessment[mpr_cell_key(assessment)==key,]
    families <- design$dgp_id[mpr_cell_key(design)==key]
    mpr_require(nrow(s)>0 && !anyDuplicated(paste(s$dgp_id,sprintf("%.17g",s$threshold))),
      "Missing or duplicate threshold assessment.")
    rows <- lapply(sort(unique(s$threshold)),function(threshold) {
      z <- s[s$threshold==threshold,]
      mpr_require(setequal(z$dgp_id,families) && nrow(z)==length(families),
        "A threshold is missing family assessments.")
      mpr_require(all(is.finite(z$lower_bound) & z$lower_bound>=0 & z$lower_bound<=1) &&
        all(z$replications>0 & z$successes>=0 & z$successes<=z$returned &
          z$returned<=z$replications),"Invalid threshold counts or bounds.")
      data.frame(cell,threshold=threshold,all_families_returned=all(z$returned==z$replications),
        worst_attainment=min(z$successes/z$replications),worst_lower_bound=min(z$lower_bound),
        families=nrow(z))
    })
    z <- do.call(rbind,rows)
    eligible <- z[z$all_families_returned,]
    mpr_require(nrow(eligible)>0,"No threshold has full observed returns in every family.")
    passed <- identical(old$interpretation,"passed")
    if(passed) {
      chosen <- eligible[eligible$threshold==old$threshold,]
      mpr_require(nrow(chosen)==1L && chosen$worst_lower_bound>=cell$tolerance_confidence,
        "A previously passed threshold is inconsistent.")
    } else {
      mpr_require(!is.finite(old$threshold),"An unresolved cell contains a passed threshold.")
      chosen <- eligible[order(-eligible$worst_lower_bound,eligible$threshold),][1L,]
    }
    mpr_require(is.finite(chosen$threshold) && chosen$threshold>0 && chosen$threshold<1,
      "Invalid selected threshold.")
    policies[[i]] <- data.frame(cell,threshold=chosen$threshold,
      method_id="mti_profile_fractional_admissible_v1",
      selection=if(passed)"preserved_original_pass" else "exploratory_calibration_maximin",
      original_calibration_passed=passed,calibration_worst_attainment=chosen$worst_attainment,
      calibration_worst_lower_bound=chosen$worst_lower_bound)
    z$selected <- z$threshold==chosen$threshold
    evidence[[i]] <- z
  }
  list(policy=do.call(rbind,policies),evidence=do.call(rbind,evidence))
}

mpf_verify_upstream <- function(spec,revision,run,review,calibration_repo) {
  mpr_require(identical(spec$purpose,"independent_empirical_comparison_not_original_calibration_pass") &&
    !isTRUE(spec$automatic_article_replacement) && !isTRUE(spec$automatic_application_rerun) &&
    !isTRUE(spec$original_calibration_reclassified),"Unsupported study interpretation.")
  mpr_require(identical(mp_sha(spec$revision_config),spec$revision_config_sha256),
    "The frozen revision configuration changed.")
  for(name in names(spec$upstream_inputs)) mpr_require(
    file.exists(file.path(review,name)) && identical(mp_sha(file.path(review,name)),
      spec$upstream_inputs[[name]]),paste("Upstream review hash mismatch:",name))
  audited <- mpr_read_json(file.path(review,"assessment.json"))
  mpr_require(isTRUE(audited$provenance$verified) && isTRUE(audited$report_source_clean) &&
    identical(audited$report_source_commit,spec$upstream_review_commit) &&
    audited$calibration_task_errors==0 && audited$calibration_numerical_failure_datasets==0,
    "Upstream audit is incomplete or has numerical failures.")
  provenance <- mpr_provenance(run,revision,calibration_repo,spec$revision_config)
  mpr_require(identical(provenance$study_commit,spec$upstream_study_commit),
    "Unexpected calibration study commit.")
  completion <- mpr_read_json(file.path(run,"calibration","completion.json"))
  mpr_require(!is.null(completion) && !isTRUE(completion$passed) &&
    completion$numerical_failure_datasets==0 && !mpr_active(run),
    "The original calibration must be complete, unresolved, numerical-error-free, and inactive.")
  mpr_require(length(list.files(file.path(run,"validation"),recursive=TRUE))==0L,
    "Independent validation seeds were already used by the original study.")
  for(row in seq_len(nrow(audited$input_records))) {
    r <- audited$input_records[row,]
    mpr_require(file.exists(r$path) && identical(mp_sha(r$path),r$sha256),
      "An audited calibration input changed.")
  }
  frozen <- jsonlite::read_json(revision$validation_config,simplifyVector=FALSE)
  design <- mpr_design(frozen)
  expected <- nrow(design)*(revision$calibration$replications+
    revision$calibration$threshold_development_replications)
  mpr_require(completion$completed_datasets==expected,"Incomplete original calibration.")
  list(design=design,frozen=frozen,provenance=provenance,
    selection=mpf_policy(read.csv(file.path(review,"calibration_cell_assessment.csv")),
      read.csv(file.path(review,"calibration_threshold_assessment.csv")),design))
}

mpf_freeze <- function(spec,verified) {
  paths <- c(spec$policy_csv,spec$threshold_evidence_csv,spec$policy_receipt)
  mpr_require(!any(file.exists(paths)),"Frozen policy files already exist; do not overwrite them.")
  pma_write_csv(verified$selection$policy,spec$policy_csv)
  pma_write_csv(verified$selection$evidence,spec$threshold_evidence_csv)
  pta_atomic_write_json(list(study_id=spec$study_id,rule=spec$policy_rule,
    upstream_study_commit=spec$upstream_study_commit,
    upstream_review_commit=spec$upstream_review_commit,upstream_inputs=spec$upstream_inputs,
    original_calibration_passed=FALSE,policy_sha256=mp_sha(spec$policy_csv),
    threshold_evidence_sha256=mp_sha(spec$threshold_evidence_csv),
    frozen_before_independent_sampling=TRUE),spec$policy_receipt)
}

mpf_check_policy <- function(spec,verified) {
  mpr_require(all(file.exists(c(spec$policy_csv,spec$threshold_evidence_csv,spec$policy_receipt))),
    "Freeze the complete policy before starting an evaluation.")
  receipt <- mpr_read_json(spec$policy_receipt)
  mpr_require(identical(receipt$policy_sha256,mp_sha(spec$policy_csv)) &&
    identical(receipt$threshold_evidence_sha256,mp_sha(spec$threshold_evidence_csv)) &&
    identical(receipt$rule,spec$policy_rule) && isTRUE(receipt$frozen_before_independent_sampling) &&
    identical(receipt$original_calibration_passed,FALSE),"Frozen policy receipt mismatch.")
  for(pair in list(list(spec$policy_csv,verified$selection$policy),
                   list(spec$threshold_evidence_csv,verified$selection$evidence))) {
    mpr_require(isTRUE(all.equal(read.csv(pair[[1L]]),pair[[2L]],check.attributes=FALSE,
      tolerance=1e-14)),"Policy differs from the declared calibration-only selection rule.")
  }
  invisible(TRUE)
}

mpf_check_seeds <- function(design,cfg) {
  make <- function(stage)unlist(lapply(design$cell_index,function(i) {
    s <- cfg[[stage]]
    count <- s$replications+if(stage=="calibration")s$threshold_development_replications else 0L
    mpr_require(s$cell_seed_stride>count,"Seed stride does not separate cells.")
    s$seed_base+i*s$cell_seed_stride+seq_len(count)
  }),use.names=FALSE)
  a <- make("calibration"); b <- make("validation")
  mpr_require(!anyDuplicated(a) && !anyDuplicated(b) && !any(b %in% a) &&
    all(is.finite(b) & b>0 & b<.Machine$integer.max),"Independent seed schedules overlap or are invalid.")
  invisible(TRUE)
}

mpf_check_runtime <- function(run) {
  old <- readRDS(file.path(run,"identity.rds"))
  installed <- find.package("rqrgibbs")
  source <- old$package_R_hashes
  expected <- file.path(installed,"R",basename(source$path))
  mpr_require(all(file.exists(expected)) && identical(vapply(expected,mp_sha,character(1)),
    setNames(source$sha256,expected)),"Installed R runtime differs from calibration.")
  mpr_require(identical(mp_sha(file.path(installed,"libs","rqrgibbs.so")),
    old$shared_library_sha256) && identical(as.character(packageVersion("rqrgibbs")),
      old$package_version),"Installed native runtime differs from calibration.")
  mpr_require(identical(as.character(packageVersion("tolerance")),old$benchmark_version) &&
    identical(R.version.string,old$R_version),"Benchmark or R version differs from calibration.")
  mpr_require(!"exdqlm" %in% loadedNamespaces(),"Unexpected external package namespace.")
  list(package_version=old$package_version,library=installed,
    shared_library_sha256=old$shared_library_sha256,benchmark_version=old$benchmark_version,
    configured_exdqlm_commit=getFromNamespace(".rqr_pinned_exdqlm_commit","rqrgibbs")())
}

mpf_protected <- function(cfg) {
  paths <- c("main.tex","rqr-gibbs-supplement.tex","refs.bib",cfg$historical_policy,
    cfg$validation_config,list.files(cfg$application_evidence,full.names=TRUE),
    list.files("tables",full.names=TRUE),list.files("figures/generated",full.names=TRUE))
  pma_file_record(paths[file.exists(paths) & !dir.exists(paths)])
}

mpf_check_output <- function(output,spec,upstream,review) {
  output <- mpr_output_path(output)
  root <- mpr_output_path(spec$output_root)
  mpr_require(startsWith(output,paste0(root,"/")),"Use a fresh directory beneath the specified output root.")
  for(other in c(upstream,review)) mpr_require(
    !startsWith(paste0(output,"/"),paste0(normalizePath(other),"/")) &&
    !startsWith(paste0(normalizePath(other),"/"),paste0(output,"/")),
    "Independent outputs must be disjoint from calibration and review.")
  output
}

mpf_identity <- function(spec_path,spec,revision,runtime,upstream,review,workers) {
  sources <- c(spec_path,spec$revision_config,spec$review_config,spec$policy_csv,
    spec$threshold_evidence_csv,spec$policy_receipt,revision$historical_policy,
    revision$validation_config,"tables/tolerance_validation_article_scan_calibration.csv",
    "application/scripts/97_run_mti_fixed_policy_evaluation.R",
    "application/scripts/lib/mti_fixed_policy_evaluation.R",
    "application/scripts/lib/mti_profile_revision.R",
    "application/scripts/lib/mti_profile_review.R",
    "application/scripts/lib/pharma_tolerance_application.R",
    "application/scripts/lib/pharma_mti_application.R",
    "application/scripts/69_validate_rqr_bayes_uq.R")
  list(study_id=spec$study_id,commit=pma_git("rev-parse","HEAD"),
    source_clean=mpr_git_clean(pma_git("status","--porcelain")),sources=pma_file_record(sources),
    policy_sha256=mp_sha(spec$policy_csv),runtime=runtime,R_version=R.version.string,
    protected_inputs=mpf_protected(revision),
    calibration_run=normalizePath(upstream),calibration_review=normalizePath(review),
    original_calibration_passed=FALSE,
    expected_datasets=length(unlist(jsonlite::read_json(revision$validation_config)$modes$
      confirmatory$dgp_ids))*length(jsonlite::read_json(revision$validation_config)$modes$
        confirmatory$design_cells)*revision$validation$replications,
    methods=unlist(revision$validation$methods),threads_per_worker=1L)
}

mpf_health <- function(output,design,cfg) {
  progress <- mpr_read_json(file.path(output,"validation","checkpoints","progress.json"))
  completed <- if(is.null(progress))0L else progress$completed_tasks
  expected <- nrow(design)*cfg$validation$replications
  terminal <- mpr_read_json(file.path(output,"validation","completion.json"))
  receipt <- mpr_read_json(file.path(output,"evaluation.json"))
  active <- mpr_active(output)
  data.frame(completed_datasets=completed,expected_datasets=expected,
    remaining_datasets=expected-completed,
    checkpointed_method_evaluations=completed*length(unlist(cfg$validation$methods)),
    expected_method_evaluations=expected*length(unlist(cfg$validation$methods)),
    state=if(!is.null(receipt))"completed_for_scientific_review" else
      if(!is.null(terminal))if(active)"reporting_running" else "fitting_complete_reporting_pending" else
      if(active)"fitting_running" else if(completed>0)"interrupted_or_incomplete" else "not_started")
}

mpf_report <- function(output,design,cfg,spec,identity) {
  z <- read.csv(file.path(output,"validation","results.csv"))
  review <- jsonlite::read_json(spec$review_config,simplifyVector=FALSE)
  frozen <- jsonlite::read_json(cfg$validation_config,simplifyVector=FALSE)
  generators <- mp_dgp_environment()
  meta <- setNames(frozen$dgps,vapply(frozen$dgps,`[[`,character(1),"dgp_id"))
  cdfs <- lapply(meta,function(x)generators$dgp_meta(x)$p)
  mpr_validate_results(z,design,cfg,cdfs)
  tasks <- mpr_checkpoint_audit(output,"validation",design,cfg)
  mpr_require(nrow(tasks)==nrow(design)*cfg$validation$replications &&
    all(tasks$task_errors==0),"Incomplete validation checkpoints or task errors.")
  policy <- read.csv(spec$policy_csv)
  primary <- z[z$method==review$primary_method & z$returned,]
  thresholds <- policy$threshold[match(mpr_cell_key(primary),mpr_cell_key(policy))]
  mpr_require(all(is.finite(thresholds) & primary$posterior_content_probability>=thresholds) &&
    all(is.finite(primary$selected_q) & primary$selected_q>0 & primary$selected_q<1 &
      is.finite(primary$selected_tilt)),"Selected MTI settings fail their frozen requirements.")
  summary <- mpr_validation_summary(z,design,cfg,review)
  paired <- mpr_paired_summary(z,review)
  pma_write_csv(summary,file.path(output,"validation_family_case_summary.csv"))
  pma_write_csv(paired,file.path(output,"validation_paired_comparisons.csv"))
  pma_write_csv(tasks,file.path(output,"validation_dataset_audit.csv"))
  pma_write_csv(attr(tasks,"checkpoint_records"),file.path(output,"validation_checkpoint_manifest.csv"))
  historical <- z[z$method=="mti_ecm_historical",]
  diagnostics <- do.call(rbind,lapply(split(historical,historical$cell_index),function(s) {
    data.frame(s[1,c("cell_index","n","content","dgp_id")],returned=sum(s$returned),
      convergence_flags_recorded=sum(!is.na(s$ecm_converged)),
      convergence_flags_true=sum(s$ecm_converged,na.rm=TRUE),
      nonfinite_candidates=sum(s$numerical_failure_candidates),
      candidates_not_flagged_converged=sum(s$ecm_candidate_nonconvergence,na.rm=TRUE),
      stationarity_median=if(any(is.finite(s$ecm_final_stationarity)))
        median(s$ecm_final_stationarity,na.rm=TRUE) else NA_real_)
  }))
  pma_write_csv(diagnostics,file.path(output,"historical_ecm_diagnostics.csv"))
  status <- list(decision="completed_independent_comparison_requires_scientific_review",
    article_inputs_replaced=FALSE,original_calibration_passed=FALSE,
    expected_datasets=nrow(tasks),method_evaluations=nrow(z),
    primary_evidence=as.list(table(summary$evidence[summary$method==review$primary_method])),
    primary_numerical_failures=sum(z$numerical_failure_candidates[z$method==review$primary_method]>0),
    nonreturned_by_method=as.list(tapply(!z$returned,z$method,sum)),
    source_commit=identity$commit,policy_sha256=identity$policy_sha256,
    interpretation="Finite-panel empirical evaluation, including three exploratory calibration settings; no distribution-free assertion.")
  mpr_report_pdf(output,status,summary=summary)
  lines <- c("# Independent Fixed-Policy MTI Evaluation","",
    "The original calibration remains unresolved in three cells. This is a separate,",
    "prospectively specified empirical comparison, not a retrospective calibration pass.","",
    sprintf("Completed: %d independent datasets; %d paired method evaluations.",nrow(tasks),nrow(z)),
    "Every method uses the same generated sample within each task. Nonreturns remain in attainment denominators.",
    "Widths are summarized separately by distribution and case with empirical 2.5% and 97.5% quantiles.",
    "The PDF shows raw width ranges and binomial uncertainty for attainment; these are different quantities.","",
    "## Primary Evidence","","| Classification | Cases |","| --- | ---: |")
  for(name in names(status$primary_evidence))lines <- c(lines,
    sprintf("| %s | %d |",name,status$primary_evidence[[name]]))
  lines <- c(lines,"","## Next Decision","",
    "Review attainment and paired width differences together, including all unresolved cases and nonreturns.",
    "A numerical improvement is distinct from a statistically supported width advantage.",
    "Do not select a new policy or repeat independent assessments until a favorable result appears.",
    "Only a later explicit adoption decision can update the existing pharmaceutical application or article.",
    "No article tables, figures, application data, or calibration inputs were replaced.")
  writeLines(lines,file.path(output,"REPORT.md"),useBytes=TRUE)
  status$output_records <- pma_file_record(list.files(output,pattern="[.]csv$|[.]pdf$|REPORT[.]md$",full.names=TRUE))
  pta_atomic_write_json(status,file.path(output,"evaluation.json"))
  status
}
