#!/usr/bin/env Rscript
Sys.setenv(OMP_NUM_THREADS = 1, OPENBLAS_NUM_THREADS = 1, MKL_NUM_THREADS = 1)
default_library <- if (dir.exists("application/cache/adaptive_calibration_library"))
  "application/cache/adaptive_calibration_library" else "application/cache/gamma_scale_library"
private <- Sys.getenv("MTI_ECM_CALIBRATION_R_LIBRARY", default_library)
if (nzchar(private)) .libPaths(c(normalizePath(private, mustWork = TRUE), .libPaths()))
libraries <- c("mti_gibbs_practical", "mti_gibbs_full_report_review", "mti_gibbs_full_publication",
  "mti_profile_revision", "mti_gibbs_width_diagnosis", "mti_prior_screen", "pharma_tolerance_application",
  "mti_fixed_prior", "mti_gibbs_boundary_calibration", "mti_gibbs_boundary_study",
  "mti_gibbs_decision_calibration", "mti_gibbs_decision_study", "mti_fixed_prior_finalization",
  "mti_gibbs_joint", "mti_gibbs_joint_precision", "mti_gibbs_joint_study", "mti_gibbs_joint_reuse",
  "mti_ecm_nominal", "mti_ecm_nominal_finalization", "mti_ecm_adaptive_calibration", "mti_ecm_adaptive_study")
for (name in libraries) source(file.path("application/scripts/lib", paste0(name, ".R")))
data.table::setDTthreads(1L)
options(datatable.optimize = 0L)
RNGkind("Mersenne-Twister", "Inversion", "Rejection")
args <- commandArgs(TRUE); mode <- if (length(args)) args[1] else "prepare"
mwd_require(mode %in% c("prepare", "check", "pipeline", "verify", "health"), "Unknown adaptive ECM command.")
configuration <- "application/config/mti_ecm_adaptive_calibration_20261120.json"
cfg <- mwd_json(configuration); mac_validate(cfg)
commit <- mgfp_git("rev-parse", "HEAD")
cfg$output_root <- Sys.getenv("MTI_ECM_CALIBRATION_OUTPUT_ROOT", file.path(
  Sys.getenv("MTI_ECM_CALIBRATION_STORAGE_ROOT", "/var/tmp/jaguir26-mti-studies"), paste0(cfg$study_id, "_", commit)))
pointer <- file.path(mgfp_common(), ".codex_work/mti_ecm_adaptive_calibration_latest.json")
if (mode == "health") {
  if (file.exists(pointer)) {p <- mwd_json(pointer); cfg$output_root <- p$output_root; print(p)}
  progress <- list()
  for (stage in c("audit", "original_development", "refinement_screen", "refined_development_fits", "refined_development", "validation")) {
    file <- file.path(cfg$output_root, stage, "progress.json")
    if (file.exists(file)) {
      z <- mwd_json(file)
      unit <- if (stage == "audit") "case audits" else if (stage %in% c("original_development", "refined_development"))
        "design assessments" else "dataset fits"
      progress[[stage]] <- data.frame(stage = stage, unit = unit, completed = z$completed_datasets,
        expected = z$expected_datasets, remaining = z$remaining_datasets, workers = z$workers,
        current_stage_eta_seconds = z$eta_seconds)
    }
  }
  if (length(progress)) print(data.table::rbindlist(progress))
  for (name in c("study_summary.json", "verification.json", "controller_exit.json")) {
    file <- file.path(cfg$output_root, name)
    if (file.exists(file)) {cat(name, "\n"); print(mwd_json(file))}
  }
  quit(save = "no")
}
dir.create(cfg$output_root, recursive = TRUE, showWarnings = FALSE)
mps_disk_check(cfg$output_root, cfg$disk_reserve_gib)
context <- mac_context(cfg, cfg$output_root)
workers <- mac_workers(cfg)
programs <- unique(c(configuration, "application/scripts/142_assess_mti_ecm_adaptive_calibration.R",
  "application/scripts/143_launch_mti_ecm_adaptive_calibration.sh",
  file.path("application/scripts/lib", paste0(libraries, ".R")), mps_programs))
identity <- list(source_commit = commit, configuration = cfg, programs = mgfp_hashes(programs),
  package = context$package, R = R.version.string, rng = RNGkind(), generators = context$generator_sha256,
  sources = context$sources, source_tasks_sha256 = mwd_hash(context$source_tasks),
  exdqlm_reference_commit = "dffb71ee70b597d6a716ee74be1cbc99731cd453",
  article_files = mgfp_hashes(c("main.tex", "rqr-gibbs-supplement.tex")))
if (mode == "prepare") {
  print(list(reused_development_datasets = nrow(context$source_tasks), workers = workers,
    additional_pair_budget = cfg$refinement$maximum_new_pairs,
    conditional_refinement_screen_datasets = cfg$expected_cells*cfg$refinement$screen_replications,
    conditional_independent_validation_datasets = cfg$expected_cells*cfg$validation_replications,
    priors_use_analysis_data = FALSE, article_replacement = FALSE, outputs = cfg$output_root))
  quit(save = "no")
}
if (mode == "check") {print(mac_check(cfg, context)); quit(save = "no")}
if (mode == "verify") {print(mac_verify(cfg, context, workers)); quit(save = "no")}
mwd_require(!nzchar(mgfp_git("status", "--porcelain")), "Commit the study source before a full assessment.")
manifest <- file.path(cfg$output_root, "manifest.json")
if (file.exists(manifest)) mwd_require(mwd_json(manifest)$identity_hash == mwd_hash(identity), "Study inputs changed.") else
  mgfp_json_write(list(identity = identity, identity_hash = mwd_hash(identity), started = mgfp_time(), workers = workers), manifest)
check <- file.path(cfg$output_root, "rehearsal.json")
if (!file.exists(check)) mac_check(cfg, context) else
  mwd_require(mwd_json(check)$passed && mwd_json(check)$source_commit == commit, "The integrated check belongs to another source.")
tryCatch({
  mac_pipeline(cfg, context, identity, workers)
  print(mac_verify(cfg, context, workers))
}, error = function(e) {
  path <- file.path(cfg$output_root, "study_summary.json")
  previous <- if (file.exists(path)) mwd_json(path) else list()
  if (!identical(previous$status, "stopped_requires_review")) mgfp_json_write(
    list(status = "stopped_requires_review", stage = "verification", reason = conditionMessage(e),
      previous_status = previous$status, article_replaced = FALSE, time = mgfp_time()), path)
  stop(e)
})
