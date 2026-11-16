#!/usr/bin/env Rscript
Sys.setenv(OMP_NUM_THREADS = 1, OPENBLAS_NUM_THREADS = 1, MKL_NUM_THREADS = 1)
private <- Sys.getenv("MTI_PRIOR_R_LIBRARY")
if (nzchar(private)) .libPaths(c(normalizePath(private, mustWork = TRUE), .libPaths()))
libraries <- c("mti_gibbs_practical", "mti_gibbs_full_report_review", "mti_gibbs_full_publication",
  "mti_profile_revision", "mti_gibbs_width_diagnosis", "mti_prior_screen", "pharma_tolerance_application",
  "mti_fixed_prior", "mti_gibbs_boundary_calibration", "mti_gibbs_boundary_study", "mti_gibbs_boundary_closeout")
for (name in libraries) source(file.path("application/scripts/lib", paste0(name, ".R")))
data.table::setDTthreads(1L)
RNGkind("Mersenne-Twister", "Inversion", "Rejection")
args <- commandArgs(TRUE); mode <- if (length(args)) args[1] else "prepare"
configuration <- "application/config/mti_gibbs_boundary_closeout_20261116.json"
cfg <- mwd_json(configuration)
if (mode %in% c("render", "verify")) {
  source("application/scripts/lib/mti_gibbs_boundary_closeout_figures.R")
  if (mode == "render") mbx_render(cfg) else mbx_verify(cfg)
  quit(save = "no")
}
mwd_require(mode %in% c("prepare", "run"), "Use prepare, run, render, or verify.")
dir.create(cfg$output_root, recursive = TRUE, showWarnings = FALSE)
context <- mbx_context(cfg)
replay_tasks <- mbx_replay_tasks(cfg, context)
confirmation_tasks <- mbx_confirmation_tasks(cfg, context)
old_seeds <- unlist(lapply(c("development", "calibration", "validation"), function(stage)
  data.table::fread(file.path(context$parent, stage, "tasks.csv"), select = "data_seed")$data_seed))
for (stage in c("calibration", "pilot")) old_seeds <- c(old_seeds,
  data.table::fread(file.path(context$completed, stage, "tasks.csv"), select = "data_seed")$data_seed)
for (stage in names(context$prior$stages)) {
  jobs <- data.table::fread(file.path(context$parent, stage, "tasks.csv"), select = "job_id")$job_id
  old_seeds <- c(old_seeds, as.vector(outer(context$prior$stages[[stage]]$chain_seed_base+100*jobs, 1:4, "+")))
}
for (stage in c("calibration", "pilot")) {
  jobs <- data.table::fread(file.path(context$completed, stage, "tasks.csv"), select = "job_id")$job_id
  for (v in 1:3) old_seeds <- c(old_seeds, as.vector(outer(
    context$boundary$stages[[stage]]$chain_seed_base+100*jobs+10*v, 1:4, "+")))
}
new_seeds <- c(confirmation_tasks$data_seed,
  cfg$confirmation$engine_seed_base+100*confirmation_tasks$job_id)
mwd_require(!anyDuplicated(new_seeds) && !any(new_seeds %in% old_seeds) &&
  all(new_seeds > 0 & new_seeds < .Machine$integer.max), "Confirmation seed streams overlap or are invalid.")
workers <- min(cfg$maximum_workers, parallelly::availableCores())
if (mode == "prepare") {
  print(c(replay = nrow(replay_tasks), confirmation = nrow(confirmation_tasks), workers = workers))
  mps_disk_check(cfg$output_root, cfg$disk_reserve_gib); quit(save = "no")
}
mwd_require(!nzchar(mgfp_git("status", "--porcelain")), "Commit closeout source before numerical execution.")
programs <- unique(c(configuration, "application/scripts/128_close_mti_gibbs_boundary_study.R",
  "application/scripts/129_run_mti_gibbs_boundary_closeout.sh",
  file.path("application/scripts/lib", paste0(libraries, ".R")), mps_programs))
identity <- list(source_commit = mgfp_git("rev-parse", "HEAD"), programs = mgfp_hashes(programs),
  configuration = cfg, package = context$package, R = R.version.string, rng = RNGkind(),
  exdqlm_reference_commit = mwd_json(context$boundary$diagnostic_config)$exdqlm_reference_commit,
  completed_manifest_sha256 = mwd_file_hash(file.path(context$completed, "manifest.json")))
path <- file.path(cfg$output_root, "manifest.json")
if (file.exists(path)) mwd_require(mwd_json(path)$identity_hash == mwd_hash(identity),
  "Closeout source or inputs changed; preserve this experiment separately.") else
  mgfp_json_write(list(identity = identity, identity_hash = mwd_hash(identity), started = mgfp_time()), path)
replay <- mbx_queue(replay_tasks, "replay", cfg, context, identity, workers)
confirmation <- mbx_queue(confirmation_tasks, "confirmation", cfg, context, identity, workers)
summary <- mbx_collect(cfg, context, replay, confirmation, identity)
mgfp_json_write(list(status = "complete", replay_datasets = length(replay),
  confirmation_datasets = length(confirmation), finished = mgfp_time()), file.path(cfg$output_root, "status.json"))
print(summary$confirmation)
