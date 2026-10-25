#!/usr/bin/env Rscript
arguments <- commandArgs(FALSE)
script <- sub("--file=","",arguments[startsWith(arguments,"--file=")][1],fixed=TRUE)
setwd(normalizePath(file.path(dirname(script),"..",".."),mustWork=TRUE))
for(file in c("pharma_tolerance_application.R","pharma_mti_application.R",
  "mti_profile_revision.R","mti_profile_review.R","mti_fixed_policy_evaluation.R","mti_q_grid_study.R",
  "mti_final_study.R","mti_final_reporting.R","mti_threshold_followup.R"))
  source(file.path("application/scripts/lib",file))
args <- commandArgs(TRUE)
spec_path <- pta_arg_value(args,"--config=","application/config/mti_q_grid_refinement_20261022.json")
spec <- jsonlite::read_json(spec_path,simplifyVector=FALSE)
cfg <- mpq_config(spec)
design <- mpr_design(mp_frozen_profile(cfg)$validation)
mode <- pta_arg_value(args,"--mode=","preflight")
output <- pta_arg_value(args,"--run-dir=",file.path(spec$output_root,"prepared_20261022"))
if(mode=="health") {
  for(stage in c("screen","calibration","validation")) {
    stage_root <- if(!is.null(spec$followup) && stage=="validation")file.path(output,"combined") else output
    stage_design <- if(!is.null(spec$followup) && stage=="calibration")mpq_followup_design(design,spec) else design
    p <- mpr_read_json(file.path(stage_root,stage,"checkpoints","progress.json"))
    done <- if(is.null(p))0L else p$completed_tasks
    n <- nrow(mpq_tasks(stage_design,cfg,stage))
    complete <- mpr_read_json(file.path(stage_root,stage,"completion.json"))
    reused <- FALSE
    if(stage=="screen" && !is.null(spec$screen_reference)) {
      preserved <- pta_arg_value(args,"--screen-run=","")
      if(nzchar(preserved)) {
        complete <- mpr_read_json(file.path(preserved,"screen","completion.json"))
        if(!is.null(complete)) {done <- complete$completed_datasets; reused <- TRUE}
      }
    }
    cat(stage,":",done,"/",n,"datasets; remaining",n-done,
      if(is.null(complete))"not_completed" else if(isTRUE(complete$passed))"passed" else "unresolved",
      if(reused)"(preserved screen)" else "","\n")
  }
  assessment <- mpr_read_json(file.path(output,"calibration","assessment_progress.json"))
  if(!is.null(assessment))cat("Calibration assessment:",assessment$phase,
    assessment$completed_batches,"/",assessment$expected_batches,"batches\n")
  final <- mpr_read_json(file.path(output,"final_study_completion.json"))
  if(!is.null(final))cat("Final study:",final$decision,"\n")
  terminal <- mpr_read_json(file.path(output,"final_exit.json"))
  if(!is.null(terminal))cat("Final launcher exit:",terminal$exit_status,"at",terminal$completed_utc,"\n")
  if(!is.null(spec$followup)) {
    final <- mpr_read_json(file.path(output,"followup_study_completion.json"))
    if(!is.null(final))cat("Threshold follow-up:",final$decision,"\n")
    terminal <- mpr_read_json(file.path(output,"followup_exit.json"))
    if(!is.null(terminal))cat("Follow-up launcher exit:",terminal$exit_status,"at",terminal$completed_utc,"\n")
  }
  quit(status=0)
}
options(warn=2)
mpr_require(!isTRUE(spec$automatic_large_launch) && !isTRUE(spec$automatic_promotion) &&
  !isTRUE(spec$modify_baseline_or_article),"Unsupported launch or promotion controls.")
baseline <- pta_arg_value(args,"--baseline-run=","")
mpr_require(nzchar(baseline),"Provide the explicit frozen baseline run.")
base_identity <- mpq_baseline_check(baseline,spec,complete=mode %in% c("screen","calibration","validation","final","followup"))
mpq_seed_check(design,cfg,jsonlite::read_json(spec$revision_config,simplifyVector=FALSE))
pta_require_packages(c("rqrgibbs","jsonlite","digest","tolerance"))
mpr_require(as.character(packageVersion("rqrgibbs"))=="0.1.0.9038" &&
  "rqr_mti_refine_content_grid" %in% getNamespaceExports("rqrgibbs"),"Install the committed grid runtime first.")
mpr_require(!"exdqlm" %in% loadedNamespaces(),"Unexpected external namespace.")
workers <- as.integer(pta_arg_value(args,"--workers=",as.character(spec$execution$screen_workers)))
mpr_require(is.finite(workers) && workers>=1 && workers<=spec$execution$maximum_workers &&
  spec$execution$threads_per_worker==1,"Invalid worker settings.")
output <- mpr_output_path(output)
allowed <- mpr_output_path(spec$output_root)
mpr_require(startsWith(output,paste0(allowed,"/")),"Output must be under the dedicated ignored study root.")
for(other in c(normalizePath(baseline),base_identity$calibration_run,base_identity$calibration_review))
  mpr_require(!startsWith(paste0(output,"/"),paste0(other,"/")) &&
    !startsWith(paste0(other,"/"),paste0(output,"/")),"Output overlaps preserved evidence.")
screen_reference <- NULL
if(!is.null(spec$screen_reference)) {
  screen_run <- pta_arg_value(args,"--screen-run=","")
  mpr_require(nzchar(screen_run),"Provide the explicit preserved screen run.")
  screen_reference <- mpq_screen_reference(screen_run,spec,cfg,design)
  mpr_require(!startsWith(paste0(output,"/"),paste0(screen_reference$run,"/")) &&
    !startsWith(paste0(screen_reference$run,"/"),paste0(output,"/")),"Output overlaps the preserved screen.")
}
parent_reference <- NULL
if(!is.null(spec$followup)) {
  parent_run <- pta_arg_value(args,"--parent-run=","")
  mpr_require(nzchar(parent_run),"Provide the explicit completed parent calibration run.")
  parent_reference <- mpq_followup_reference(parent_run,spec,cfg,design)
  mpr_require(!startsWith(paste0(output,"/"),paste0(parent_reference$run,"/")) &&
    !startsWith(paste0(parent_reference$run,"/"),paste0(output,"/")),"Output overlaps preserved parent evidence.")
}
calibration_design <- if(is.null(parent_reference))design else parent_reference$reassess_design
if(mode=="preflight") {
  cat("Verified separate grid specification, frozen baseline sources, numerical runtime, and disjoint seeds.\n")
  cat("Screen:",nrow(mpq_tasks(design,cfg,"screen")),"test-only datasets; no tolerance-performance claim.\n")
  cat("Large calibration requires a complete baseline, numerical screen, precision review, and explicit command.\n")
  cat("Calibration:",nrow(calibration_design)*(cfg$calibration$replications+
    cfg$calibration$threshold_development_replications),"datasets; validation:",
    nrow(design)*cfg$validation$replications,"datasets.\n")
  if(!is.null(parent_reference))cat("Retained thresholds:",nrow(parent_reference$retained$policy),
    "unchanged; original and new error allowances:",parent_reference$retained$original_familywise_error,
    parent_reference$total_familywise_error-parent_reference$retained$original_familywise_error,"\n")
  quit(status=0)
}
mpr_require(mode %in% c("screen","calibration","validation","precision","final","followup"),"Unknown grid study stage.")
mpr_require(is.null(spec$followup) || mode %in% c("precision","followup"),
  "Follow-up specifications require the separately authorized follow-up controller.")
if(mode %in% c("calibration","final","followup")) {
  screen <- if(is.null(screen_reference))mpr_read_json(file.path(output,"screen","completion.json")) else
    screen_reference$assessment
  mpr_require(!is.null(screen) && isTRUE(screen$passed) && screen$strictly_shorter_test_intervals>0,
    "A passed numerical screen with a search change is required before calibration.")
  rationale <- pta_arg_value(args,"--rationale=","")
  mpr_require("--acknowledge-new-calibration" %in% args && "--acknowledge-precision" %in% args &&
    nzchar(trimws(rationale)),"Review baseline results and precision; provide explicit acknowledgements and a rationale.")
}
if(mode=="final")mpr_require("--authorize-fixed-final-study" %in% args &&
  !is.null(screen_reference) && isTRUE(spec$final_study$validation_after_passing_calibration),
  "The bounded final study needs its explicit authorization and preserved screen.")
if(mode=="followup")mpr_require("--authorize-threshold-followup" %in% args &&
  !is.null(parent_reference) && !is.null(screen_reference),
  "The threshold follow-up requires explicit authorization and preserved parent evidence.")
mpr_require(mpr_git_clean(pma_git("status","--porcelain")),"Commit reviewed source before a grid study stage.")
dir.create(output,recursive=TRUE,showWarnings=FALSE)
sources <- c(spec_path,spec$revision_config,spec$baseline_policy,cfg$validation_config,
  "tables/tolerance_validation_article_scan_calibration.csv",cfg$historical_policy,
  "Makefile","application/scripts/100_manage_mti_q_grid_study.sh",
  "application/scripts/99_run_mti_q_grid_study.R","application/scripts/lib/mti_q_grid_study.R",
  "application/scripts/lib/mti_final_study.R",
  "application/scripts/lib/mti_final_reporting.R",
  "application/scripts/lib/mti_threshold_followup.R",
  "application/scripts/lib/mti_profile_revision.R","application/scripts/lib/mti_profile_review.R",
  "application/scripts/lib/mti_fixed_policy_evaluation.R","application/scripts/69_validate_rqr_bayes_uq.R",
  "application/scripts/lib/pharma_tolerance_application.R","application/scripts/lib/pharma_mti_application.R",
  "application/R/rqr_mti_content_grid.R","figures/generate_tolerance_validation_primary_figure.R",
  "figures/generate_tolerance_validation_width_figure.R","figures/data/tolerance_population_shortest_content_oracle.csv")
identity <- list(commit=pma_git("rev-parse","HEAD"),sources=pma_file_record(sources),
  package_version=as.character(packageVersion("rqrgibbs")),
  package_R=pma_file_record(list.files(file.path(find.package("rqrgibbs"),"R"),full.names=TRUE)),
  native_sha256=mp_sha(file.path(find.package("rqrgibbs"),"libs","rqrgibbs.so")),
  baseline_run=normalizePath(baseline),baseline_commit=base_identity$commit,
  benchmark_version=as.character(packageVersion("tolerance")),R_version=R.version.string,
  configured_exdqlm_commit=getFromNamespace(".rqr_pinned_exdqlm_commit","rqrgibbs")(),
  screen_reference=screen_reference,
  parent_reference=parent_reference,
  protected_inputs=mpf_protected(cfg))
path <- file.path(output,"identity.rds")
if(file.exists(path))mpr_require(identical(readRDS(path),identity),"Grid study identity changed; use a new directory.") else {
  existing <- list.files(output,all.files=TRUE,no..=TRUE)
  mpr_require(all(existing %in% c("launch.lock","launch.log","launcher.pid","launcher_exit.json")),
    "Output contains an earlier experiment without its identity.")
  mp_atomic_rds(identity,path)
  pta_atomic_write_json(c(identity,list(workers=workers,threads_per_worker=1,
    started_utc=format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC"))),file.path(output,"manifest.json"))
}
guard <- function() {
  mpr_require(identical(identity$sources,pma_file_record(identity$sources$path)),"Grid study source changed.")
  mpr_require(identical(identity$protected_inputs,pma_file_record(identity$protected_inputs$path)),"Historical article inputs changed.")
  mpq_baseline_check(baseline,spec)
  if(!is.null(screen_reference)) {
    mpr_require(identical(screen_reference$records,pma_file_record(screen_reference$records$path)),
      "Preserved screen evidence changed.")
  }
  if(!is.null(parent_reference))mpr_require(identical(parent_reference$records,
    mpq_followup_reference(parent_reference$run,spec,cfg,design)$records),"Preserved parent evidence changed.")
  invisible(TRUE)
}
guard()
if(mode=="precision") {
  pma_write_csv(mpq_precision_reference(calibration_design,cfg,spec),file.path(output,"precision_reference.csv"))
  cat("Hypothetical binomial power curves only; no MTI attainment assumed or replication budget selected.\n")
} else {
  if(mode %in% c("calibration","final","followup")) {
    if(!is.null(screen_reference)) {
      resources <- mpq_resource_check(base_identity,cfg,spec,calibration_design,screen_reference,workers)
      pta_atomic_write_json(resources,file.path(output,"resource_check.json"))
    }
    pta_atomic_write_json(list(rationale=rationale,precision_reviewed=TRUE,
    assessment_replications=cfg$calibration$replications,
    expected_calibration_datasets=nrow(calibration_design)*(cfg$calibration$replications+
      cfg$calibration$threshold_development_replications),
    validation_after_pass=mode %in% c("final","followup"),
    baseline_report_sha256=mp_sha(file.path(baseline,"evaluation.json"))),
      file.path(output,"calibration_authorization.json"))
  }
  result <- if(mode=="followup")mpq_followup_run(output,cfg,spec,design,workers,guard,parent_reference) else
    if(mode=="final")mpq_final_run(output,cfg,spec,design,workers,guard) else
      mpq_stage(mode,output,cfg,spec,design,workers)
  print(result)
}
guard()
cat("Requested preparation stage completed; original study and article inputs preserved.\n")
