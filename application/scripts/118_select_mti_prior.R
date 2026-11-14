#!/usr/bin/env Rscript
Sys.setenv(OMP_NUM_THREADS = 1, OPENBLAS_NUM_THREADS = 1, MKL_NUM_THREADS = 1)
private_library <- Sys.getenv("MTI_PRIOR_R_LIBRARY")
if (nzchar(private_library)) .libPaths(c(normalizePath(private_library, mustWork = TRUE), .libPaths()))
for (file in c("mti_gibbs_practical", "mti_gibbs_full_report_review", "mti_gibbs_full_publication",
  "mti_profile_revision", "mti_gibbs_width_diagnosis", "mti_prior_screen"))
  source(file.path("application/scripts/lib", paste0(file, ".R")))
data.table::setDTthreads(1L)
cfg <- mwd_json("application/config/mti_prior_screen_20261113.json")
args <- commandArgs(TRUE); mode <- if (length(args)) args[1] else "run"
output <- cfg$output_dir
dir.create(output, recursive = TRUE, showWarnings = FALSE)
context <- mps_context(cfg, output)
tasks <- mps_design(cfg, context)
if (mode == "health") {
  file <- file.path(output, if (file.exists(file.path(output, "status.json"))) "status.json" else "progress.json")
  mwd_require(file.exists(file), "The study has not produced a progress report yet.")
  print(mwd_json(file)); quit(save = "no")
}
mwd_require(mode %in% c("run", "check"), "Mode must be run, check, or health.")
package <- mps_package_check()
mps_disk_check(output, cfg$disk_reserve_gib)
if (mode == "check") {
  sample <- tasks[tasks$response_id == "gamma05" & tasks$n == 1000 & tasks$content == .9 & tasks$kappa == 3, ][1, ]
  started <- Sys.time(); r <- mps_fit(sample, cfg, context)
  print(r$rows); cat("Elapsed seconds:", as.numeric(difftime(Sys.time(), started, units = "secs")), "\n")
  quit(save = "no", status = if (isTRUE(r$returned)) 0L else 1L)
}
mwd_require(!nzchar(mgfp_git("status", "--porcelain")), "Commit the study source before launching.")
dc <- mwd_json(cfg$diagnostic_config)
old <- readRDS(file.path(mgfp_common(), dc$campaign, "jobs.rds"))
mwd_require(!any(tasks$data_seed %in% old$data_seed), "Development and original sample seeds overlap.")
chain_seeds <- outer(100L * tasks$job_id + cfg$chain_seed_base, seq_len(cfg$chains), "+")
mwd_require(!any(chain_seeds %in% c(old$seed, old$data_seed, tasks$data_seed)), "Random streams overlap.")
inputs <- c(mwd_json(dc$article_config)$validation_config, mwd_json(dc$article_config)$policy_csv,
  "tables/tolerance_validation_article_scan_calibration.csv")
identity <- list(source_commit = mgfp_git("rev-parse", "HEAD"), configuration = cfg,
  programs = mgfp_hashes(mps_programs), inputs = mgfp_hashes(inputs), package = package,
  exdqlm_reference_commit = dc$exdqlm_reference_commit, rng = RNGkind(), R = R.version.string,
  dependencies = as.list(setNames(vapply(c("Rcpp", "posterior", "data.table", "digest", "jsonlite"),
    function(p) as.character(packageVersion(p)), ""), c("Rcpp", "posterior", "data.table", "digest", "jsonlite"))))
manifest <- file.path(output, "manifest.json")
if (file.exists(manifest)) mwd_require(identical(mwd_json(manifest)$identity_sha256, mwd_hash(identity)),
  "The screening study identity changed.") else
  mgfp_json_write(list(identity = identity, identity_sha256 = mwd_hash(identity), started = mgfp_time()), manifest)
saveRDS(tasks, file.path(output, "tasks.rds")); mwd_write(tasks, file.path(output, "tasks.csv"))
dir.create(file.path(output, "fits"), showWarnings = FALSE)
paths <- file.path(output, "fits", sprintf("fit_%06d.rds", tasks$job_id))
workers <- min(cfg$maximum_workers, parallelly::availableCores())
pending <- which(!file.exists(paste0(paths, ".json")))
batches <- split(pending, ceiling(seq_along(pending) / workers))
for (batch in batches) {
  mps_disk_check(output, cfg$disk_reserve_gib)
  result <- parallel::mclapply(batch, function(j) {
    r <- tryCatch(mps_fit(tasks[j, ], cfg, context), error = function(e)
      list(returned = FALSE, task = tasks[j, ], reason = conditionMessage(e)))
    temporary <- paste0(paths[j], ".tmp"); saveRDS(r, temporary)
    mwd_require(file.rename(temporary, paths[j]), "Cannot preserve a prior screening fit.")
    mgfp_json_write(list(sha256 = mwd_file_hash(paths[j]), identity_sha256 = mwd_hash(identity)), paste0(paths[j], ".json"))
    TRUE
  }, mc.cores = workers, mc.set.seed = FALSE)
  mwd_require(all(vapply(result, isTRUE, FALSE)), "A worker did not preserve its result.")
  cat(mgfp_time(), ": saved", sum(file.exists(paths)), "/", nrow(tasks), "paired prior fits\n"); flush.console()
  mgfp_json_write(list(time = mgfp_time(), completed = sum(file.exists(paths)), expected = nrow(tasks),
    workers = workers, selected_prior = NULL, ready_for_full_rerun = FALSE), file.path(output, "progress.json"))
}
mps_collect(tasks, output, cfg, complete = TRUE)
cat("Prior suitability screening complete. No prior was automatically promoted.\n")
