#!/usr/bin/env Rscript
Sys.setenv(OMP_NUM_THREADS = 1, OPENBLAS_NUM_THREADS = 1, MKL_NUM_THREADS = 1)
private <- Sys.getenv("MTI_PRIOR_R_LIBRARY")
if (nzchar(private)) .libPaths(c(normalizePath(private, mustWork = TRUE), .libPaths()))
libraries <- c("mti_gibbs_practical", "mti_gibbs_full_report_review", "mti_gibbs_full_publication",
  "mti_profile_revision", "mti_gibbs_width_diagnosis", "mti_prior_screen", "pharma_tolerance_application",
  "mti_fixed_prior", "mti_gibbs_boundary_calibration", "mti_gibbs_boundary_study",
  "mti_gibbs_decision_calibration", "mti_gibbs_decision_study")
for (name in libraries) source(file.path("application/scripts/lib", paste0(name, ".R")))
data.table::setDTthreads(1L)
RNGkind("Mersenne-Twister", "Inversion", "Rejection")
args <- commandArgs(TRUE); mode <- if (length(args)) args[1] else "prepare"
configuration <- "application/config/mti_gibbs_decision_calibration_20261116.json"
cfg <- mwd_json(configuration)
if (mode == "health") { mdc_health(cfg); quit(save = "no") }
if (mode == "verify") {
  directory <- if(length(args)>1) args[2] else mdc_health(cfg)
  mdc_verify_evidence(directory);quit(save="no")
}
mwd_require(mode %in% c("prepare", "check", "replay", "pipeline"), "Unknown decision-study command.")
dir.create(cfg$output_root, recursive = TRUE, showWarnings = FALSE)
context <- mdc_context(cfg)
designs <- mdc_designs(cfg, context)
workers <- min(cfg$maximum_workers, parallelly::availableCores())
seed_plan <- mdc_seed_plan(designs, cfg, context)
designs <- seed_plan$designs; seed_audit <- seed_plan$audit
programs <- unique(c(configuration, cfg$boundary_config, "application/scripts/131_assess_mti_gibbs_decisions.R",
  "application/scripts/132_launch_mti_gibbs_decisions.sh", file.path("application/scripts/lib", paste0(libraries, ".R")),
  mps_programs))
identity <- list(source_commit = mgfp_git("rev-parse", "HEAD"), programs = mgfp_hashes(programs),
  configuration = cfg, package = context$package, R = R.version.string, rng = RNGkind(),
  exdqlm_reference_commit = mwd_json(mwd_json(cfg$boundary_config)$diagnostic_config)$exdqlm_reference_commit,
  generators = context$generator_sha256, seed_audit = seed_audit,
  old_evidence = mgfp_hashes(file.path(context$boundary, c("manifest.json", "calibration/manifest.json",
    "calibration/tasks.csv", "calibration_policy.csv", "development_policy.csv"))),
  input_rules = mgfp_hashes(c(mwd_json(mwd_json(mwd_json(cfg$boundary_config)$diagnostic_config)$article_config)$policy_csv,
    "tables/tolerance_validation_article_scan_calibration.csv")))
if (mode == "prepare") {
  print(vapply(designs, nrow, 1L)); print(seed_audit)
  print(mbc_rank(cfg$stages$calibration$replications, unique(designs$calibration$tolerance_confidence),
    cfg$calibration_error, cfg$protected_comparisons))
  cat("Available workers:", workers, "\n")
  mps_disk_check(cfg$output_root, cfg$disk_reserve_gib)
  quit(save = "no")
}
if (mode == "check") {
  d <- designs$shortlist
  d <- d[d$replication == 1 & ((d$n == 500 & d$content == .99 & d$response_id %in% c("normal", "exponential")) |
    (d$n == 1000 & d$content == .9 & d$response_id == "lognormal_hard") |
    (d$n == 50 & d$content == .9 & d$response_id == "beta18")), ]
  checks <- parallel::mclapply(seq_len(nrow(d)), function(i) {
    old <- mdc_old(d[i, ], context)
    baseline <- context$policy$screen[context$policy$n == d$n[i] & context$policy$content == d$content[i]]
    gamma <- min(mdc_path(old$anchor$candidates)$posterior_content_probability)
    choice <- data.frame(n = d$n[i], content = d$content[i], rule = c("baseline", "alternative1"),
      gamma = c(baseline, gamma))
    mdc_job(d[i, ], "shortlist", choice, cfg, context)
  }, mc.cores = min(workers, nrow(d)), mc.set.seed = FALSE)
  saveRDS(checks, file.path(cfg$output_root, "rehearsal.rds"))
  mwd_require(!any(vapply(checks, inherits, FALSE, "try-error")) &&
    all(vapply(checks, function(x) isTRUE(x$ok), FALSE)), "Posterior-selection rehearsal requires review.")
  # Refit an already exploratory sample to exercise fresh ECM-to-Gibbs calculation.
  # Its streams remain in the exploratory allocation, outside both future stages.
  task <- d[d$response_id == "exponential", ][1, ]
  task$ecm_seed <- as.integer(task$chain_base3+9)
  gamma <- context$policy$screen[context$policy$n == task$n & context$policy$content == task$content]
  refit <- mdc_job(task,"rehearsal",data.frame(n=task$n,content=task$content,rule="baseline",gamma=gamma),cfg,context)
  mwd_require(refit$ok,"Complete ECM-to-Gibbs rehearsal requires review.")
  saveRDS(refit,file.path(cfg$output_root,"rehearsal_refit.rds"))
  decisions<-mdc_final_rows(refit,task,data.frame(n=task$n,content=task$content,rule="baseline",multiplier=1),
    data.frame(n=task$n,content=task$content,rule="baseline",advance=TRUE),cfg,context)
  mwd_write(decisions,file.path(cfg$output_root,"rehearsal_reported_intervals.csv"))
  traces<-c(checks,list(refit))
  for(i in seq_along(traces)) names(traces[[i]]$records)<-vapply(traces[[i]]$records,function(record)
    paste("q =",format(record$parameters$q[1],digits=6),"delta =",format(record$parameters$delta[1],digits=4)),"")
  mbc_rehearsal_report(traces,file.path(cfg$output_root,"rehearsal_traces.pdf"))
  mwd_write(data.table::rbindlist(lapply(checks, `[[`, "rows"), fill = TRUE), file.path(cfg$output_root, "rehearsal.csv"))
  mgfp_json_write(list(identity = identity, datasets = length(checks),
    new_targets = sum(vapply(checks, `[[`, 1L, "new_targets")), complete_refit_passed=TRUE,
    passed = TRUE), file.path(cfg$output_root, "rehearsal.json"))
  cat("Real-sample rehearsal passed.\n"); quit(save = "no")
}
if (mode == "replay") {
  mdc_replay(designs$shortlist,cfg,context,workers); mdc_radius_review(cfg,context); quit(save="no")
}
mwd_require(!nzchar(mgfp_git("status", "--porcelain")), "Commit source before detached execution.")
manifest <- file.path(cfg$output_root, "manifest.json")
if (file.exists(manifest)) mwd_require(mwd_json(manifest)$identity_hash == mwd_hash(identity),
  "Study source changed; use a distinct output directory.") else
  mgfp_json_write(list(identity = identity, identity_hash = mwd_hash(identity),
    actual_storage = normalizePath(cfg$output_root), started = mgfp_time()), manifest)
stage <- "replay"
tryCatch({
  shortlist <- mdc_replay(designs$shortlist, cfg, context, workers)
  mdc_radius_review(cfg,context)
  saveRDS(as.data.frame(shortlist), file.path(cfg$output_root, "shortlist_policy.rds"))
  stage <- "shortlist"
  exploratory <- mdc_queue(designs$shortlist, stage, shortlist, cfg, context, identity, workers)
  gate <- mdc_gate(exploratory$rows, context$old_radius, cfg)
  for (name in c("policy", "scores", "cells"))
    mwd_write(gate[[name]], file.path(cfg$output_root, paste0("exploratory_", name, ".csv")))
  mgfp_json_write(list(advancing_designs = sum(gate$policy$advance),
    exploratory_probability = gate$exploratory_probability, materiality = cfg$materiality,
    exploratory_only = TRUE), file.path(cfg$output_root, "materiality_decision.json"))
  if (!any(gate$policy$advance)) {
    mdc_preserve_evidence(cfg,identity)
    mgfp_json_write(list(status = "complete_no_material_gain", stages_completed = c("replay", "shortlist", "materiality"),
      new_posterior_targets = exploratory$targets, article_replaced = FALSE, time = mgfp_time()),
      file.path(cfg$output_root, "study_summary.json"))
  } else {
    frozen <- mdc_frozen_selection(gate, shortlist)
    frozen_path <- file.path(cfg$output_root, "frozen_selection.rds")
    if (file.exists(frozen_path)) mwd_require(identical(readRDS(frozen_path), frozen), "Frozen selection changed.") else saveRDS(frozen, frozen_path)
    mwd_write(frozen, file.path(cfg$output_root, "frozen_selection.csv"))
    frozen_hash <- mwd_file_hash(frozen_path)
    mgfp_json_write(list(selection_sha256 = frozen_hash, source_commit = identity$source_commit,
      selected_before_fresh_data = TRUE, protected_comparisons = cfg$protected_comparisons,
      article_replaced = FALSE), file.path(cfg$output_root, "frozen_policy.json"))
    stage <- "calibration"
    calibration <- mdc_queue(designs$calibration, stage, frozen, cfg, context, identity, workers)
    calibrated <- mdc_calibrate(calibration$rows, cfg)
    for (name in names(calibrated)) mwd_write(calibrated[[name]], file.path(cfg$output_root, paste0("calibration_", name, ".csv")))
    calibrated_path <- file.path(cfg$output_root, "calibrated_radius.rds")
    saveRDS(calibrated$policy, calibrated_path)
    calibrated_hash <- mwd_file_hash(calibrated_path)
    mgfp_json_write(list(selection_sha256 = frozen_hash, radius_sha256 = calibrated_hash,
      frozen_before_validation = TRUE), file.path(cfg$output_root, "frozen_radius.json"))
    stage <- "validation"
    validation <- mdc_queue(designs$validation, stage, frozen, cfg, context, identity, workers)
    mwd_require(mwd_file_hash(frozen_path) == frozen_hash && mwd_file_hash(calibrated_path) == calibrated_hash,
      "A frozen statistical rule changed during validation.")
    stage <- "report"
    summary <- mdc_report(designs$validation, "validation", identity, frozen, calibrated$policy, cfg, context)
    mdc_preserve_evidence(cfg,identity);mdc_verify_evidence(cfg$output_root)
    mgfp_json_write(list(status = "complete_requires_scientific_review", article_replaced = FALSE,
      completed_datasets = sum(vapply(designs, nrow, 1L)),
      new_posterior_targets = exploratory$targets + calibration$targets + validation$targets,
      validation_summaries = nrow(summary), time = mgfp_time()), file.path(cfg$output_root, "study_summary.json"))
  }
}, error = function(e) {
  mgfp_json_write(list(status = "stopped_requires_review", stage = stage, reason = conditionMessage(e),
    time = mgfp_time(), article_replaced = FALSE), file.path(cfg$output_root, "study_summary.json"))
  stop(e)
})
