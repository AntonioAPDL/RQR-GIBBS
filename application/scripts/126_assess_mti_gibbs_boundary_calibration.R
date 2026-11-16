#!/usr/bin/env Rscript
Sys.setenv(OMP_NUM_THREADS = 1, OPENBLAS_NUM_THREADS = 1, MKL_NUM_THREADS = 1)
private <- Sys.getenv("MTI_PRIOR_R_LIBRARY")
if (nzchar(private)) .libPaths(c(normalizePath(private, mustWork = TRUE), .libPaths()))
libraries <- c("mti_gibbs_practical", "mti_gibbs_full_report_review", "mti_gibbs_full_publication",
  "mti_profile_revision", "mti_gibbs_width_diagnosis", "mti_prior_screen", "pharma_tolerance_application",
  "mti_fixed_prior", "mti_gibbs_boundary_calibration", "mti_gibbs_boundary_study")
for (name in libraries)
  source(file.path("application/scripts/lib", paste0(name, ".R")))
data.table::setDTthreads(1L)
RNGkind("Mersenne-Twister", "Inversion", "Rejection")
args <- commandArgs(TRUE); mode <- if (length(args)) args[1] else "prepare"
configuration <- "application/config/mti_gibbs_boundary_calibration_20261115.json"
cfg <- mwd_json(configuration)
dir.create(cfg$output_root, recursive = TRUE, showWarnings = FALSE)
if (mode == "health") {
  pointer <- file.path(mgfp_common(), ".codex_work/mti_gibbs_boundary_latest.json")
  output <- cfg$output_root
  if (file.exists(pointer)) {
    latest <- mwd_json(pointer)
    mwd_require(identical(latest$study_id, cfg$study_id), "Latest study identifier differs.")
    output <- latest$output_root
    cat("Frozen checkout:", latest$checkout, "\nSource:", latest$source_commit, "\n")
  }
  cat("Outputs:", output, "\n")
  for (path in c("audit/summary.json", "study_summary.json", "controller_exit.json", "development/progress.json",
    "calibration/progress.json", "pilot/progress.json")) {
    file <- file.path(cfg$output_root, path)
    if (file.exists(file)) { cat(path, "\n"); print(mwd_json(file)) }
  }
  quit(save = "no")
}
mwd_require(mode %in% c("prepare", "check", "audit", "pipeline"), "Unknown boundary study command.")
context <- mbc_context(cfg)
designs <- mbc_designs(cfg, context)
workers <- min(cfg$maximum_workers, parallelly::availableCores())
programs <- unique(c(configuration, cfg$prior_config, "application/scripts/126_assess_mti_gibbs_boundary_calibration.R",
  "application/scripts/127_launch_mti_gibbs_boundary_calibration.sh",
  file.path("application/scripts/lib", paste0(libraries, ".R")), mps_programs))
identity <- list(source_commit = mgfp_git("rev-parse", "HEAD"), programs = mgfp_hashes(programs),
  configuration = cfg, package = context$package, R = R.version.string, rng = RNGkind(),
  exdqlm_reference_commit = mwd_json(cfg$diagnostic_config)$exdqlm_reference_commit,
  generators = context$generator_sha256,
  inputs = mgfp_hashes(c(mwd_json(mwd_json(cfg$diagnostic_config)$article_config)$validation_config,
    mwd_json(mwd_json(cfg$diagnostic_config)$article_config)$policy_csv,
    "tables/tolerance_validation_article_scan_calibration.csv",
    file.path(context$parent, "development/tasks.csv"))),
  parent_development_manifest = mwd_file_hash(file.path(context$parent, "development/manifest.json")),
  dependencies = as.list(setNames(vapply(c("posterior", "digest", "jsonlite", "data.table", "tolerance"),
    function(x) as.character(packageVersion(x)), ""), c("posterior", "digest", "jsonlite", "data.table", "tolerance"))))
if (mode == "prepare") {
  print(vapply(designs, nrow, 1L)); print(mbc_rank(cfg$stages$calibration$replications, .95,
    cfg$calibration_error, cfg$expected_cells)); cat("Available workers:", workers, "\n")
  mps_disk_check(cfg$output_root, cfg$disk_reserve_gib); quit(save = "no")
}
if (mode == "check") {
  d <- designs$development
  choose <- d$replication == 1 & ((d$n == 1000 & d$content == .9 & d$response_id %in% c("normal", "exponential")) |
    (d$n == 500 & d$content == .99 & d$response_id == "student_t3") |
    (d$n == 50 & d$content == .9 & d$response_id == "lognormal_hard"))
  checks <- parallel::mclapply(seq_len(sum(choose)), function(j) {
    task <- d[choose, ][j, ]; started <- proc.time()[3]
    value <- mbc_job(task, "development", cfg$variants, cfg, context)
    value$elapsed_seconds <- unname(proc.time()[3] - started)
    value
  }, mc.cores = min(workers, sum(choose)), mc.set.seed = FALSE)
  mwd_require(!any(vapply(checks, inherits, FALSE, "try-error")) &&
    all(vapply(checks, function(x) isTRUE(x$ok), FALSE)), "The real-sample rehearsal requires review.")
  for (j in seq_along(checks)) saveRDS(checks[[j]], file.path(cfg$output_root, paste0("checked_", j, ".rds")))
  task <- designs$calibration[designs$calibration$response_id == "exponential" &
    designs$calibration$n == 1000 & designs$calibration$content == .9, ][1, ]
  task$job_id <- 1L; task$data_seed <- cfg$rehearsal$data_seed; task$stage <- "rehearsal"
  check_cfg <- cfg; check_cfg$stages$rehearsal <- cfg$rehearsal
  started <- proc.time()[3]
  fresh <- mbc_job(task, "rehearsal", "sqrt_rank", check_cfg, context)
  fresh$elapsed_seconds <- unname(proc.time()[3] - started)
  mwd_require(isTRUE(fresh$ok), "The fresh-data anchor and sampler rehearsal requires review.")
  saveRDS(fresh, file.path(cfg$output_root, "checked_fresh.rds"))
  mbc_rehearsal_report(c(checks, list(fresh)), file.path(cfg$output_root, "rehearsal_traces.pdf"))
  rows <- data.table::rbindlist(lapply(checks, `[[`, "rows"))
  mwd_write(rows, file.path(cfg$output_root, "checked_posteriors.csv"))
  mgfp_json_write(list(source = identity, elapsed_seconds = vapply(checks, `[[`, 0, "elapsed_seconds"),
    adequate = all(rows$adequate), fresh_task_adequate = fresh$ok,
    fresh_task_seconds = fresh$elapsed_seconds, retained_per_chain = unique(rows$retained_per_chain)),
    file.path(cfg$output_root, "rehearsal.json"))
  print(rows[, .(response_id, n, content, variant, original_margin, outward_slope, width, max_rhat)])
  quit(save = "no")
}
if (mode == "audit") { mbc_audit(cfg, context, workers); quit(save = "no") }
mwd_require(!nzchar(mgfp_git("status", "--porcelain")), "Commit study source before background execution.")
manifest <- file.path(cfg$output_root, "manifest.json")
if (file.exists(manifest)) mwd_require(mwd_json(manifest)$identity_hash == mwd_hash(identity),
  "Controller source or inputs changed; use a distinct study directory.") else
  mgfp_json_write(list(identity = identity, identity_hash = mwd_hash(identity), started = mgfp_time()), manifest)
stage <- "audit"
tryCatch({
  mbc_audit(cfg, context, workers)
  stage <- "development"
  development <- mbc_queue(designs$development, stage, cfg, context, identity, workers)
  choice <- mbc_development_choice(development$rows, cfg$variants)
  for (name in names(choice)) mwd_write(choice[[name]], file.path(cfg$output_root, paste0("development_", name, ".csv")))
  remaining <- sum(vapply(designs[c("calibration", "pilot")], nrow, 1L))
  mbc_capacity(cfg, designs$development, development$analysis_id, remaining)
  stage <- "calibration"
  calibration <- mbc_queue(designs$calibration, stage, cfg, context, identity, workers, choice$policy)
  frozen <- mbc_calibrate(calibration$rows, cfg)
  for (name in names(frozen)) mwd_write(frozen[[name]], file.path(cfg$output_root, paste0("calibration_", name, ".csv")))
  mgfp_json_write(list(target_policy = mgfp_hashes(file.path(cfg$output_root, "development_policy.csv")),
    interval_policy = mgfp_hashes(file.path(cfg$output_root, "calibration_policy.csv")),
    identity_hash = mwd_hash(identity), frozen_before_pilot = TRUE), file.path(cfg$output_root, "frozen_policy.json"))
  stage <- "pilot"
  pilot <- mbc_queue(designs$pilot, stage, cfg, context, identity, workers, frozen$policy)
  receipt <- mwd_json(file.path(cfg$output_root, "frozen_policy.json"))
  mgfp_check_hashes(receipt$target_policy); mgfp_check_hashes(receipt$interval_policy)
  mbc_pilot_report(cfg, context, designs$pilot, pilot$analysis_id, frozen$policy)
}, error = function(e) {
  mgfp_json_write(list(status = "stopped_requires_review", stage = stage, reason = conditionMessage(e),
    time = mgfp_time(), article_replaced = FALSE), file.path(cfg$output_root, "study_summary.json"))
  stop(e)
})
