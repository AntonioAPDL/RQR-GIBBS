#!/usr/bin/env Rscript
Sys.setenv(OMP_NUM_THREADS = 1, OPENBLAS_NUM_THREADS = 1, MKL_NUM_THREADS = 1, BLIS_NUM_THREADS = 1)
args <- commandArgs(TRUE); mode <- if (length(args)) args[1] else "health"
if (!mode %in% c("check", "prepare", "run", "health", "report", "verify")) stop("Unknown Gibbs extension command.")
common <- dirname(normalizePath(system2("git", c("--no-optional-locks", "rev-parse", "--git-common-dir"), stdout = TRUE)))
pointer_file <- file.path(common, ".codex_work/mti_gibbs_skewness_latest.json")
if (mode == "health") {
  if (!file.exists(pointer_file)) {cat("The Gibbs distributional extension has not been launched.\n"); quit(save = "no")}
  pointer <- jsonlite::read_json(pointer_file); cat("Study:", pointer$output_root, "\nSource:", pointer$source_commit, "\n")
  for (name in c("study_status.json", "qualification.json", "progress.json", "verification.json", "controller_exit.json")) {
    path <- file.path(pointer$output_root, name)
    if (file.exists(path)) {
      value <- jsonlite::read_json(path)
      if (name == "verification.json") value$files <- NULL
      print(setNames(list(value), name))
    }
  }
  count <- length(list.files(file.path(pointer$output_root, "assessments"), pattern = "[.]rds[.]json$", recursive = TRUE))
  cat("Saved datasets:", count, "/ 36000; remaining:", 36000L - count, "\n")
  quit(save = "no")
}
private <- Sys.getenv("MTI_GIBBS_SKEWNESS_LIBRARY", "application/cache/gibbs_skewness_library")
.libPaths(c(normalizePath(private, mustWork = TRUE), .libPaths()))
libraries <- c("mti_gibbs_practical", "mti_gibbs_full_report_review", "mti_gibbs_full_publication",
  "mti_gibbs_width_diagnosis", "pharma_tolerance_application", "pharma_mti_application",
  "mti_profile_revision", "mti_profile_review", "mti_gibbs_full_comparison", "mti_skewness_distributions",
  "mti_skewness_extension", "mti_skewness_reporting", "mti_gibbs_skewness",
  "mti_gibbs_skewness_checks", "mti_gibbs_skewness_reporting")
for (name in libraries) source(file.path("application/scripts/lib", paste0(name, ".R")))
invisible(lapply(c("rqrgibbs", "posterior", "data.table"), loadNamespace))
data.table::setDTthreads(1L); RNGkind("Mersenne-Twister", "Inversion", "Rejection")
cfg <- mgs_config()
parent_pointer <- mwd_json(file.path(common, cfg$parent_pointer))
parent <- Sys.getenv("MTI_GIBBS_SKEWNESS_PARENT", parent_pointer$output_root)
directory <- Sys.getenv("MTI_GIBBS_SKEWNESS_OUTPUT_ROOT", "application/outputs/mti_gibbs_skewness_checks_20261122")
dir.create(directory, recursive = TRUE, showWarnings = FALSE)
context <- mgs_context(cfg, parent)
if (mode == "check") {
  mgs_runtime(private)
  print(mgs_check(cfg, context, directory)[c("passed", "cases", "mean_chain_seconds", "sampler_unchanged")])
  quit(save = "no")
}
phase <- "preparing"
stage <- function(name, ...) {
  phase <<- name
  mgfp_json_write(c(list(stage = name, source_commit = mgfp_git("rev-parse", "HEAD"),
    expected_datasets = cfg$expected_datasets, article_replaced = FALSE, time = mgfp_time()), list(...)),
    file.path(directory, "study_status.json"))
}
options(error = function() {
  mgfp_json_write(list(stage = "stopped_requires_review", failed_phase = phase, reason = geterrmessage(),
    article_replaced = FALSE, time = mgfp_time()), file.path(directory, "study_status.json"))
  quit(save = "no", status = 1L)
})
if (mode == "run") mwd_require(!nzchar(mgfp_git("status", "--porcelain")), "Commit source before sampling.")
stage("verifying_parent_and_runtime")
prepared <- mgs_prepare(cfg, context, directory, private)
if (mode == "prepare") {print(list(datasets = nrow(prepared$tasks), chains = 4L * nrow(prepared$tasks))); quit(save = "no")}
if (mode == "run") {
  checks <- file.path(directory, "prelaunch_checks.json")
  valid <- file.exists(checks) && isTRUE(mwd_json(checks)$passed) &&
    identical(mwd_json(checks)$inputs, mgfp_hashes(c(mgs_sources, cfg$configuration_path)))
  if (!valid) {stage("saved_target_checks"); mgs_check(cfg, context, directory)}
  stage("frozen_source_tests")
  status <- system2(file.path(R.home("bin"), "Rscript"),
    c("application/scripts/66_run_testthat_file_strict.R", "application/tests/testthat/test-mti-gibbs-skewness.R"),
    stdout = file.path(directory, "unit_tests.log"), stderr = file.path(directory, "unit_tests.log"),
    env = paste0("R_LIBS=", shQuote(normalizePath(private))))
  mwd_require(status == 0L, "Frozen-source tests failed; sampling not started.")
  requested <- as.integer(Sys.getenv("MTI_GIBBS_SKEWNESS_WORKERS", cfg$maximum_workers))
  resources <- mgs_resources(cfg, directory, nrow(prepared$tasks), requested)
  mgfp_json_write(resources, file.path(directory, "launch_resources.json"))
  stage("qualifying_saved_targets", workers = resources$workers)
  qualification <- mgs_qualify(prepared, cfg, context, directory, resources$workers)
  stage("sampling", workers = resources$workers, qualification = qualification)
  mgs_run_jobs(prepared$tasks, cfg, context, directory, prepared$identity, resources$workers)
}
stage("verifying_and_reporting")
report <- mgs_report(prepared, cfg, context, directory)
stage(if (report$numerically_adequate) "complete_requires_scientific_review" else "complete_numerical_review_required",
  numerically_adequate = report$numerically_adequate, report = report$report)
print(report)
