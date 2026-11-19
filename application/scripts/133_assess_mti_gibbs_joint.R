#!/usr/bin/env Rscript
Sys.setenv(OMP_NUM_THREADS = 1, OPENBLAS_NUM_THREADS = 1, MKL_NUM_THREADS = 1)
private <- Sys.getenv("MTI_PRIOR_R_LIBRARY")
if (nzchar(private)) .libPaths(c(normalizePath(private, mustWork = TRUE), .libPaths()))
libraries <- c("mti_gibbs_practical", "mti_gibbs_full_report_review", "mti_gibbs_full_publication",
  "mti_profile_revision", "mti_gibbs_width_diagnosis", "mti_prior_screen", "pharma_tolerance_application",
  "mti_fixed_prior", "mti_gibbs_boundary_calibration", "mti_gibbs_boundary_study",
  "mti_gibbs_decision_calibration", "mti_gibbs_decision_study", "mti_fixed_prior_finalization",
  "mti_gibbs_joint", "mti_gibbs_joint_precision", "mti_gibbs_joint_study", "mti_gibbs_joint_reuse")
for (name in libraries) source(file.path("application/scripts/lib", paste0(name, ".R")))
data.table::setDTthreads(1L)
RNGkind("Mersenne-Twister", "Inversion", "Rejection")
args <- commandArgs(TRUE); mode <- if (length(args)) args[1] else "prepare"
configuration <- "application/config/mti_gibbs_joint_20261117.json"
cfg <- mwd_json(configuration)
scratch <- Sys.getenv("MTI_JOINT_OUTPUT_ROOT")
if (nzchar(scratch)) cfg$output_root <- scratch
pointer <- file.path(mgfp_common(), ".codex_work/mti_gibbs_joint_latest.json")
if (mode == "dependency-state") {
  mwd_require(length(args) == 3, "Dependency state requires predecessor and status paths.")
  state <- mjs_dependency_status(args[2])
  mgfp_json_write(list(status = state, predecessor = args[2], time = mgfp_time()), args[3])
  cat(state); quit(save = "no")
}
if (mode == "health") {
  if (file.exists(pointer)) cfg$output_root <- mwd_json(pointer)$output_root
  cat("Outputs:", cfg$output_root, "\n")
  for (p in c("scheduler_status.json", "study_summary.json", "modes/progress.json", "development/progress.json",
    "refinement/progress.json", "validation/progress.json", "controller_exit.json")) {
    path <- file.path(cfg$output_root, p)
    if (file.exists(path)) { cat(p, "\n"); print(mwd_json(path)) }
  }
  quit(save = "no")
}
if (mode == "verify") {
  directory <- if (length(args) > 1) args[2] else cfg$output_root
  mjs_verify(directory, cfg); quit(save = "no")
}
mwd_require(mode %in% c("prepare", "check", "pipeline"), "Unknown joint-study command.")
dir.create(cfg$output_root, recursive = TRUE, showWarnings = FALSE)
dep_path <- file.path(cfg$output_root, "dependency.json")
dependency <- if (file.exists(dep_path)) mwd_json(dep_path) else {
  p <- mwd_json(file.path(mgfp_common(), ".codex_work/mti_gibbs_decision_latest.json"))
  p$selection_sha256 <- mwd_file_hash(file.path(p$output_root, "frozen_selection.rds")); p
}
context <- mjs_context(cfg, dependency)
designs <- mjs_designs(cfg, context)
seed_plan <- mjs_seed_audit(designs, cfg, context)
designs <- seed_plan$designs; audit <- seed_plan$audit
workers <- min(cfg$maximum_workers, parallelly::availableCores())
override <- suppressWarnings(as.integer(Sys.getenv("MTI_JOINT_WORKERS", "")))
if (is.finite(override)) workers <- min(workers, max(1L, override))
programs <- unique(c(configuration, cfg$predecessor_config, "application/scripts/133_assess_mti_gibbs_joint.R",
  "application/scripts/134_launch_mti_gibbs_joint.sh", file.path("application/scripts/lib", paste0(libraries, ".R")), mps_programs))
identity <- list(source_commit = mgfp_git("rev-parse", "HEAD"), programs = mgfp_hashes(programs),
  configuration = cfg, package = context$package, R = R.version.string, rng = RNGkind(),
  generators = context$generator_sha256, seed_audit = audit, dependency = dependency,
  exdqlm_reference_commit = mwd_json(mwd_json(mwd_json(cfg$predecessor_config)$boundary_config)$diagnostic_config)$exdqlm_reference_commit)
reuse_path <- file.path(cfg$output_root, "reuse_source.json")
if (file.exists(reuse_path)) identity$mode_reuse <- mwd_json(reuse_path)
mwd_require(identity$exdqlm_reference_commit == "dffb71ee70b597d6a716ee74be1cbc99731cd453", "Pinned reference changed.")
if (mode == "prepare") {
  print(vapply(designs, nrow, 1L)); print(audit)
  cat("Model specifications:", nrow(mjc_models(cfg)), "\nMaximum workers:", workers, "\n")
  mps_disk_check(cfg$output_root, cfg$disk_reserve_gib); quit(save = "no")
}
if (mode == "check") {
  # Paired developmental observations only; these are not fresh validation data.
  d <- designs$development
  d <- d[d$replication == 1 & ((d$n == 50 & d$content == .9 & d$response_id == "exponential") |
    (d$n == 500 & d$content == .99 & d$response_id == "normal")), ]
  checks <- lapply(seq_len(nrow(d)), function(i) {
    task <- d[i, ]; task$chain_seed <- as.integer(cfg$seeds$rehearsal_chain_base + 20 * i)
    modes <- mjs_modes(task, cfg, context)
    rules <- as.data.frame(context$reference[n == task$n & content == task$content])
    a <- rules; a$rule <- "check_sd1"; a$prior_sd <- 1
    a$gamma <- min(mdc_path(modes$anchors[["1"]]$candidates)$posterior_content_probability)
    rules$slot <- 1L; a$slot <- 2L
    value <- mjs_gibbs(task, modes$anchors, rbind(rules, a), cfg, context, old = mjs_old(task, context))
    saveRDS(list(task = task, modes = modes, result = value), file.path(cfg$output_root, paste0("rehearsal_sample_", i, ".rds")))
    mwd_require(value$ok && all(value$rows$returned) && all(value$rows$radius_multiplier == 1),
      "The real-sample independent-prior rehearsal requires review.")
    list(task = task, modes = modes, result = value)
  })
  saveRDS(checks, file.path(cfg$output_root, "rehearsal.rds"))
  traces <- lapply(checks, function(z) {
    v <- z$result
    names(v$records) <- vapply(v$records, function(r) paste("prior SD =", r$prior_sd %||% 2,
      "q =", signif(r$parameters$q[1], 5), "delta =", signif(r$parameters$delta[1], 4)), "")
    v
  })
  mbc_rehearsal_report(traces, file.path(cfg$output_root, "rehearsal_traces.pdf"))
  replay_cfg <- cfg; replay_cfg$output_root <- file.path(cfg$output_root, "rehearsal_profile")
  dir.create(replay_cfg$output_root, showWarnings = FALSE)
  replay_tasks <- do.call(rbind, lapply(checks, `[[`, "task"))
  replay_paths <- file.path(replay_cfg$output_root, paste0("modes_", seq_along(checks), ".rds"))
  for (i in seq_along(checks)) mbc_atomic_fit(checks[[i]]$modes, replay_paths[i], "rehearsal", replay_tasks[i, ])
  replay <- mjs_replay(replay_tasks, list(paths = replay_paths, analysis_id = "rehearsal"), replay_cfg, context, 1L)
  mwd_require(nrow(replay$selection) > 0, "The real-sample joint DP nomination failed.")
  mgfp_json_write(list(passed = TRUE, datasets = length(checks), both_root_priors_checked = TRUE,
    actual_unscaled_means_checked = TRUE, analytic_dp_nomination_checked = TRUE,
    identity = identity, time = mgfp_time()), file.path(cfg$output_root, "rehearsal.json"))
  cat("Real-sample joint-prior rehearsal passed.\n"); quit(save = "no")
}
mwd_require(!nzchar(mgfp_git("status", "--porcelain")), "Commit study source before execution.")
exit <- mwd_json(file.path(dependency$output_root, "controller_exit.json"))
status <- mwd_json(file.path(dependency$output_root, "study_summary.json"))
mwd_require(exit$exit_code == 0 && status$status == "complete_requires_scientific_review",
  "The predecessor has not completed successfully.")
mdc_verify_evidence(dependency$output_root)
manifest <- file.path(cfg$output_root, "manifest.json")
if (file.exists(manifest)) mwd_require(mwd_json(manifest)$identity_hash == mwd_hash(identity), "Study identity changed.") else
  mgfp_json_write(list(identity = identity, identity_hash = mwd_hash(identity), started = mgfp_time()), manifest)
stage <- "modes"
tryCatch({
  tasks <- designs$development
  mjs_import_modes(tasks, cfg, identity, context)
  modes <- mjs_queue(tasks, stage, cfg, identity, workers, function(task) mjs_modes(task, cfg, context))
  stage <- "replay"
  comparison <- mjs_replay(tasks, modes, cfg, context, workers)
  selection <- comparison$selection
  stage <- "development"
  mjs_import_development(tasks, selection, modes, cfg, identity, context)
  mode_index <- setNames(seq_len(nrow(tasks)), tasks$job_id)
  development <- mjs_queue(tasks, stage, cfg, list(identity, selection), workers, function(task) {
    a <- mjs_read(modes, tasks, mode_index[[as.character(task$job_id)]])$anchors
    mjs_gibbs(task, a, selection, cfg, context, old = mjs_old(task, context))
  })
  rows <- mjs_collect(development, tasks)
  scores <- mjc_scores(rows, cfg, context$radius)
  refinement <- mjs_refinement(comparison$curves, selection, scores$scores, cfg)
  mwd_write(refinement, file.path(cfg$output_root, "refinement_selection.csv"))
  if (nrow(refinement)) {
    stage <- "refinement"
    selected_tasks <- tasks[paste(tasks$n, tasks$content) %in% paste(refinement$n, refinement$content), ]
    refined <- mjs_queue(selected_tasks, stage, cfg, list(identity, selection, refinement), workers, function(task) {
      index <- mode_index[[as.character(task$job_id)]]
      mjs_gibbs(task, mjs_read(modes, tasks, index)$anchors, refinement, cfg, context,
        previous = mjs_read(development, tasks, index), old = mjs_old(task, context))
    })
    rows <- data.table::rbindlist(list(rows, mjs_collect(refined, selected_tasks)), fill = TRUE)
    selection <- data.table::rbindlist(list(selection, refinement), fill = TRUE)
    scores <- mjc_scores(rows, cfg, context$radius)
  }
  mwd_write(rows, file.path(cfg$output_root, "development_results.csv"))
  mwd_write(scores$scores, file.path(cfg$output_root, "development_scores.csv"))
  mwd_write(scores$cells, file.path(cfg$output_root, "development_cells.csv"))
  frozen <- mjs_freeze(scores$scores, selection, cfg)
  frozen_path <- file.path(cfg$output_root, "frozen_selection.rds")
  if (file.exists(frozen_path)) mwd_require(identical(readRDS(frozen_path), frozen), "Frozen rules changed.") else saveRDS(frozen, frozen_path)
  mwd_write(frozen, file.path(cfg$output_root, "frozen_selection.csv"))
  frozen_sha <- mwd_file_hash(frozen_path)
  radius_path <- file.path(dependency$output_root, "calibrated_radius.rds")
  frozen_radius <- context$frozen_radius
  mwd_require(frozen_radius$radius_sha256 == mwd_file_hash(radius_path), "Frozen predecessor radius changed.")
  eligible <- mjs_validation_domain(designs$validation, frozen)
  eligible_path <- file.path(cfg$output_root, "eligible_validation_tasks.rds")
  if (file.exists(eligible_path)) mwd_require(identical(readRDS(eligible_path), eligible),
    "Frozen validation observations changed.") else saveRDS(eligible, eligible_path)
  policy_record <- list(selection_sha256 = frozen_sha, predecessor_radius_sha256 = frozen_radius$radius_sha256,
    width_reference = cfg$width_reference, validation_domain = cfg$validation_domain,
    validation_tasks_sha256 = mwd_hash(eligible), validation_datasets = nrow(eligible),
    assurance_comparisons = cfg$assurance$comparisons,
    frozen_before_fresh_data = TRUE, advancing_designs = sum(frozen$advanced), source_commit = identity$source_commit)
  policy_path <- file.path(cfg$output_root, "frozen_policy.json")
  if (file.exists(policy_path)) {
    saved <- mwd_json(policy_path)
    mwd_require(saved$validation_tasks_sha256 == policy_record$validation_tasks_sha256 &&
      saved$selection_sha256 == frozen_sha && saved$predecessor_radius_sha256 == frozen_radius$radius_sha256 &&
      saved$source_commit == identity$source_commit, "Frozen independent-assessment policy changed.")
  } else mgfp_json_write(policy_record, policy_path)
  if (!any(frozen$advanced)) {
    mgfp_json_write(list(status = "complete_no_developmental_gain", stage = "development", article_replaced = FALSE,
      reason = "No complete unscaled rule met the prespecified attainment and paired width criteria.", time = mgfp_time()),
      file.path(cfg$output_root, "study_summary.json"))
  } else if (cfg$selection$automatic_fresh_validation_if_any_design_advances) {
    stage <- "validation"
    validation <- mjs_queue(eligible, stage, cfg, list(identity, frozen_sha, frozen_radius, mwd_hash(eligible)), workers,
      function(task) mjs_final_job(task, frozen, cfg, context))
    mwd_require(mwd_file_hash(frozen_path) == frozen_sha && mwd_file_hash(radius_path) == frozen_radius$radius_sha256,
      "A fixed rule changed during independent assessment.")
    final <- mjs_collect(validation, eligible)
    mwd_write(final, file.path(cfg$output_root, "validation/interval_results.csv"))
    mwd_write(mjc_summary(final, cfg), file.path(cfg$output_root, "validation/comparison_summary.csv"))
    mjs_report(final, cfg)
    mgfp_json_write(list(status = "complete_requires_scientific_review", article_replaced = FALSE,
      advancing_designs = sum(frozen$advanced), validation_datasets = nrow(eligible), time = mgfp_time()),
      file.path(cfg$output_root, "study_summary.json"))
  } else {
    mgfp_json_write(list(status = "complete_development_requires_scientific_review", article_replaced = FALSE,
      advancing_designs = sum(frozen$advanced), independent_assessment_started = FALSE, time = mgfp_time()),
      file.path(cfg$output_root, "study_summary.json"))
  }
  mjs_preserve(cfg, identity); mjs_verify(cfg$output_root, cfg)
}, error = function(e) {
  mgfp_json_write(list(status = "stopped_requires_review", stage = stage, reason = conditionMessage(e),
    article_replaced = FALSE, time = mgfp_time()), file.path(cfg$output_root, "study_summary.json"))
  stop(e)
})
