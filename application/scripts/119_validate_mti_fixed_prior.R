#!/usr/bin/env Rscript
Sys.setenv(OMP_NUM_THREADS = 1, OPENBLAS_NUM_THREADS = 1, MKL_NUM_THREADS = 1)
private <- Sys.getenv("MTI_PRIOR_R_LIBRARY")
if (nzchar(private)) .libPaths(c(normalizePath(private, mustWork = TRUE), .libPaths()))
for (file in c("mti_gibbs_practical", "mti_gibbs_full_report_review", "mti_gibbs_full_publication",
  "mti_profile_revision", "mti_gibbs_width_diagnosis", "mti_prior_screen", "pharma_tolerance_application", "mti_fixed_prior"))
  source(file.path("application/scripts/lib", paste0(file, ".R")))
data.table::setDTthreads(1L)
RNGkind("Mersenne-Twister", "Inversion", "Rejection")
args <- commandArgs(TRUE); mode <- if (length(args)) args[1] else "check"
configuration <- "application/config/mti_fixed_prior_20261114.json"
cfg <- mwd_json(configuration)
dir.create(cfg$output_root, recursive = TRUE, showWarnings = FALSE)
if (mode == "health") {
  for (stage in c(names(cfg$stages), "application")) {
    directory <- file.path(cfg$output_root, stage)
    file <- file.path(directory, if (file.exists(file.path(directory, "status.json"))) "status.json" else "progress.json")
    if (file.exists(file)) { cat(stage, "\n"); print(mwd_json(file)) }
  }
  quit(save = "no")
}
mwd_require(mode %in% c("check", "prepare", "pipeline", "application", names(cfg$stages)), "Unknown fixed-prior command.")
context <- mps_context(cfg, cfg$output_root)
mfp_check_designs(cfg, context)
package <- mps_package_check()
mps_disk_check(cfg$output_root, cfg$disk_reserve_gib)
programs <- unique(c(configuration, "application/scripts/119_validate_mti_fixed_prior.R",
  "application/scripts/lib/mti_fixed_prior.R", "application/scripts/lib/pharma_tolerance_application.R", mps_programs))
mwd_require(identical(as.character(packageVersion("tolerance")), "3.0.0"), "The pinned tolerance 3.0.0 benchmark is required.")
identity <- list(source_commit = mgfp_git("rev-parse", "HEAD"), programs = mgfp_hashes(programs),
  configuration = cfg, package = package, exdqlm_reference_commit = mwd_json(cfg$diagnostic_config)$exdqlm_reference_commit,
  inputs = mgfp_hashes(c(mwd_json(mwd_json(cfg$diagnostic_config)$article_config)$validation_config,
    mwd_json(mwd_json(cfg$diagnostic_config)$article_config)$policy_csv,
    "tables/tolerance_validation_article_scan_calibration.csv")),
  generators = context$generator_sha256, R = R.version.string,
  rng = RNGkind(),
  tolerance_installation = mgfp_hashes(list.files(system.file(package = "tolerance"), recursive = TRUE, full.names = TRUE)),
  application_data = mgfp_hashes(c("figures/data/pharma_mti_application_20261020/observed_batches.csv",
    "figures/data/pharma_mti_application_20261020/split_membership.csv.gz")),
  dependencies = as.list(setNames(vapply(c("Rcpp", "posterior", "digest", "jsonlite", "data.table", "tolerance"),
    function(x) as.character(packageVersion(x)), ""), c("Rcpp", "posterior", "digest", "jsonlite", "data.table", "tolerance"))))
if (mode == "prepare") {
  for (stage in names(cfg$stages)) {
    d <- mfp_design(stage, cfg, context)
    cat(stage, ":", nrow(d), "paired attempts,", length(unique(d$data_seed)), "datasets\n")
  }
  cat("application:", nrow(mfp_application_design(cfg, context)), "paired attempts\n")
  print(mfp_spec(c(-8, 12), cfg$primary_kappa, cfg$simulation_reference)[c("prior_sd", "learning_rate", "data_adaptive_prior")])
  quit(save = "no")
}
if (mode == "check") {
  tasks <- mfp_design("development", cfg, context)
  task <- tasks[tasks$response_id == "gamma05" & tasks$n == 1000 & tasks$content == .9 & tasks$kappa == 2, ][1, ]
  started <- Sys.time(); result <- mfp_job(task, cfg, context)
  saveRDS(result, file.path(cfg$output_root, "checked_fit.rds"))
  print(result$rows); cat("Unresolved candidates:", result$unresolved_candidates,
    "Elapsed seconds:", as.numeric(difftime(Sys.time(), started, units = "secs")), "\n")
  application_tasks <- mfp_application_design(cfg, context)
  application_checks <- lapply(unique(application_tasks$response_id), function(response) {
    task <- application_tasks[application_tasks$response_id == response, ][1, ]
    fit <- mfp_application_job(task, cfg, context)
    saveRDS(fit, file.path(cfg$output_root, paste0("checked_", response, ".rds")))
    cat("Application response:", response, "Returned:", fit$returned, "\n")
    print(fit$rows)
    isTRUE(fit$returned) && all(fit$rows$ecm_coordinate_ok & fit$rows$gibbs_adequate)
  })
  quit(save = "no", status = if (isTRUE(result$returned) &&
    all(result$rows$ecm_coordinate_ok & result$rows$gibbs_adequate) && all(unlist(application_checks))) 0L else 1L)
}
mwd_require(!nzchar(mgfp_git("status", "--porcelain")), "Commit fixed-prior study source before launching.")
run_stage <- function(stage) {
  tasks <- if (stage == "application") mfp_application_design(cfg, context) else mfp_design(stage, cfg, context)
  directory <- file.path(cfg$output_root, stage)
  dir.create(file.path(directory, "fits"), recursive = TRUE, showWarnings = FALSE)
  id <- mwd_hash(list(identity = identity, stage = stage))
  manifest <- file.path(directory, "manifest.json")
  if (file.exists(manifest)) mwd_require(identical(mwd_json(manifest)$identity_sha256, id),
    "The fixed-prior study identity differs; use a separate output directory.") else
    mgfp_json_write(list(identity = identity, stage = stage, identity_sha256 = id, started = mgfp_time()), manifest)
  saveRDS(tasks, file.path(directory, "tasks.rds")); mwd_write(tasks, file.path(directory, "tasks.csv"))
  if (!stage %in% c("development", "application")) {
    seeds <- unique(tasks$data_seed)
    for (other in setdiff(names(cfg$stages), stage))
      mwd_require(!any(seeds %in% mfp_design(other, cfg, context)$data_seed), "Independent stage data seeds overlap.")
    old <- readRDS(file.path(mgfp_common(), mwd_json(cfg$diagnostic_config)$campaign, "jobs.rds"))
    mwd_require(!any(seeds %in% old$data_seed), "New stage overlaps original validation data seeds.")
  }
  paths <- file.path(directory, "fits", sprintf("fit_%06d.rds", tasks$job_id))
  present <- which(file.exists(paste0(paths, ".json")))
  for (j in present) {
    receipt <- mwd_json(paste0(paths[j], ".json"))
    mwd_require(file.exists(paths[j]) && identical(receipt$identity, id) &&
      identical(receipt$sha256, mwd_file_hash(paths[j])), "An existing task cannot be reused safely.")
  }
  pending <- setdiff(seq_len(nrow(tasks)), present)
  if (stage != "development") mfp_capacity(directory, tasks, cfg)
  workers <- min(cfg$maximum_workers, parallelly::availableCores())
  for (batch in split(pending, ceiling(seq_along(pending) / workers))) {
    mps_disk_check(directory, cfg$disk_reserve_gib)
    done <- parallel::mclapply(batch, function(j) {
      r <- tryCatch(if (stage == "application") mfp_application_job(tasks[j, ], cfg, context) else
        mfp_job(tasks[j, ], cfg, context), error = function(e)
        list(returned = FALSE, task = tasks[j, ], reason = conditionMessage(e), call = deparse(conditionCall(e))))
      tmp <- paste0(paths[j], ".tmp"); saveRDS(r, tmp)
      mwd_require(file.rename(tmp, paths[j]), "Cannot save a fixed-prior fit.")
      mgfp_json_write(list(identity = id, sha256 = mwd_file_hash(paths[j])), paste0(paths[j], ".json"))
      TRUE
    }, mc.cores = workers, mc.set.seed = FALSE)
    mwd_require(all(vapply(done, isTRUE, FALSE)), "A fixed-prior worker did not preserve its result.")
    completed <- sum(file.exists(paste0(paths, ".json")))
    mgfp_json_write(list(stage = stage, completed = completed, expected = nrow(tasks), workers = workers,
      prior_uses_analysis_data = FALSE, article_replaced = FALSE, time = mgfp_time()),
      file.path(directory, "progress.json"))
    cat(mgfp_time(), stage, completed, "/", nrow(tasks), "paired attempts saved\n"); flush.console()
  }
  if (stage == "application") return(mfp_collect_application(tasks, directory, id))
  status <- mfp_collect(tasks, directory, id, cfg)
  mfp_review(directory, cfg); status
}
if (mode != "pipeline") { print(run_stage(mode)); quit(save = "no") }
tryCatch({
  development <- run_stage("development")
  if (!development$numerical_pass) {
    mgfp_json_write(list(status = "development_requires_numerical_review", development = development,
      no_final_validation_launched = TRUE), file.path(cfg$output_root, "pipeline_status.json"))
    quit(save = "no")
  }
  calibration <- run_stage("calibration")
  if (!calibration$ready_for_final_validation) {
    mgfp_json_write(list(status = "calibration_requires_review", calibration = calibration,
      no_final_validation_launched = TRUE), file.path(cfg$output_root, "pipeline_status.json"))
    quit(save = "no")
  }
  validation <- run_stage("validation")
  application <- if (isTRUE(cfg$application_after_simulation_validation) && validation$numerical_pass)
    run_stage("application") else NULL
  mgfp_json_write(list(status = "simulation_validation_complete", validation = validation,
    application_reference = cfg$application_reference, application = application,
    application_not_yet_evaluated = is.null(application),
    article_replaced = FALSE), file.path(cfg$output_root, "pipeline_status.json"))
}, error = function(e) {
  mgfp_json_write(list(status = "stopped_with_preserved_results", reason = conditionMessage(e),
    time = mgfp_time(), article_replaced = FALSE), file.path(cfg$output_root, "pipeline_status.json"))
  stop(e)
})
