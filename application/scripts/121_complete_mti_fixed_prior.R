#!/usr/bin/env Rscript
private <- Sys.getenv("MTI_PRIOR_R_LIBRARY")
if (nzchar(private)) .libPaths(c(normalizePath(private, mustWork = TRUE), .libPaths()))
for (name in c("mti_gibbs_practical", "mti_gibbs_full_report_review", "mti_gibbs_full_publication",
  "mti_profile_revision", "mti_gibbs_width_diagnosis", "mti_prior_screen", "pharma_tolerance_application",
  "mti_fixed_prior", "mti_fixed_prior_closure"))
  source(file.path("application/scripts/lib", paste0(name, ".R")))
data.table::setDTthreads(1L)
RNGkind("Mersenne-Twister", "Inversion", "Rejection")
args <- commandArgs(TRUE); mode <- if (length(args)) args[1] else "prepare"
configuration <- "application/config/mti_fixed_prior_closure_20261115.json"
cfg <- mwd_json(configuration); prior <- mwd_json(cfg$prior_config)
dir.create(cfg$output_root, recursive = TRUE, showWarnings = FALSE)
if (mode == "health") {
  for (path in c("status.json", "validation/status.json", "application/progress.json", "application/status.json",
    "diagnostics/progress.json", "diagnostics/status.json")) {
    file <- file.path(cfg$output_root, path)
    if (file.exists(file)) { cat(path, "\n"); print(mwd_json(file)) }
  }
  quit(save = "no")
}
mwd_require(mode %in% c("prepare", "check", "run"), "Unknown continuation command.")
parent_source <- normalizePath(file.path(mgfp_common(), cfg$parent_worktree), mustWork = TRUE)
parent <- normalizePath(file.path(parent_source, cfg$parent_output), mustWork = TRUE)
parent_manifest <- mwd_json(file.path(parent, "development/manifest.json"))
mwd_require(identical(parent_manifest$identity$source_commit, cfg$parent_commit), "The parent source differs.")
mfc_settings_check(prior, parent_manifest)
mgfp_check_hashes(parent_manifest$identity$inputs)
mgfp_check_hashes(parent_manifest$identity$application_data)
package <- mps_package_check()
mwd_require(identical(unname(unlist(package$installed_files)),
  unname(unlist(parent_manifest$identity$package$installed_files))), "The installed computation differs from the parent.")
mwd_require(identical(R.version.string, parent_manifest$identity$R), "The R runtime differs from the parent.")
for (name in names(parent_manifest$identity$dependencies))
  mwd_require(identical(as.character(packageVersion(name)), parent_manifest$identity$dependencies[[name]]),
    paste("A dependency differs from the parent:", name))
benchmark_installation <- mgfp_hashes(list.files(system.file(package = "tolerance"),
  full.names = TRUE, recursive = TRUE))
mwd_require(identical(unname(unlist(benchmark_installation)),
  unname(unlist(parent_manifest$identity$tolerance_installation))),
  "The installed benchmark computation differs from the parent.")
context <- mps_context(prior, cfg$output_root)
mwd_require(identical(context$generator_sha256, parent_manifest$identity$generators), "The data generators differ.")
policy <- mwd_json(mwd_json(prior$diagnostic_config)$practical_config)$policy
tasks <- mfp_design("validation", prior, context)
if (mode == "prepare") {
  cat("Parent:", cfg$parent_commit, "\nValidation pairs:", nrow(tasks),
    "\nApplication pairs:", nrow(mfp_application_design(prior, context)),
    "\nDiagnostic cases: at most", cfg$diagnostics$per_pattern * length(cfg$diagnostics$patterns),
    "\nPrior mean: 0; SD: 2; learning rate: 1; no sample-estimated prior.\n")
  quit(save = "no")
}
if (mode == "check") {
  original_path <- file.path(parent, "validation/fits/fit_038352.rds")
  original <- readRDS(original_path); before <- mwd_file_hash(original_path)
  repaired <- mfp_job(original$task, prior, context)
  saveRDS(repaired, file.path(cfg$output_root, "checked_repair.rds"))
  mfc_repair_check(original, repaired, original$task, prior)
  mwd_require(identical(before, mwd_file_hash(original_path)), "The original result was modified.")
  print(repaired$rows)
  cat("Exact-sample repair passed; the original file is unchanged.\n")
  app_tasks <- mfp_application_design(prior, context)
  for (response in unique(app_tasks$response_id)) {
    task <- app_tasks[app_tasks$response_id == response, ][1, ]
    result <- mfp_application_job(task, prior, context)
    saveRDS(result, file.path(cfg$output_root, paste0("checked_", response, ".rds")))
    mwd_require(isTRUE(result$returned), paste("Application computation failed for", response))
    mfc_result_check(result, task, prior)
    mwd_require(all(result$rows$ecm_coordinate_ok & result$rows$gibbs_adequate),
      paste("Application numerical checks failed for", response))
    cat("Application check:", response, "fixed independent prior and numerical checks passed.\n")
  }
  quit(save = "no")
}
mwd_require(!nzchar(mgfp_git("status", "--porcelain")), "Commit continuation source before launch.")
programs <- unique(c(configuration, cfg$prior_config, "application/scripts/121_complete_mti_fixed_prior.R",
  "application/scripts/lib/mti_fixed_prior_closure.R", "application/scripts/lib/mti_fixed_prior.R",
  "application/scripts/lib/pharma_tolerance_application.R", mps_programs))
identity <- list(source_commit = mgfp_git("rev-parse", "HEAD"), configuration = cfg,
  statistical_configuration = prior, programs = mgfp_hashes(programs), package = package,
  benchmark_installation = benchmark_installation,
  parent_development_manifest_sha256 = mwd_file_hash(file.path(parent, "development/manifest.json")),
  parent_commit = cfg$parent_commit, exdqlm_reference_commit = parent_manifest$identity$exdqlm_reference_commit,
  parent_runtime = parent_manifest$identity[c("R", "dependencies", "rng", "inputs", "application_data")])
id <- mwd_hash(identity); manifest_path <- file.path(cfg$output_root, "manifest.json")
if (file.exists(manifest_path)) mwd_require(identical(mwd_json(manifest_path)$identity_sha256, id),
  "The continuation identity differs; preserve this output and choose a new directory.") else
  mgfp_json_write(list(identity = identity, identity_sha256 = id), manifest_path)
phase <- function(name, ...) mgfp_json_write(c(list(phase = name, time = mgfp_time(),
  prior_uses_analysis_data = FALSE, article_replaced = FALSE), list(...)), file.path(cfg$output_root, "status.json"))
tryCatch({
  phase("waiting_for_parent")
  cat("Waiting for the original controller to finish; no simulation is restarted.\n"); flush.console()
  start <- Sys.time()
  while (!mfc_parent_ready(parent, nrow(tasks))) {
    mwd_require(as.numeric(difftime(Sys.time(), start, units = "hours")) < cfg$maximum_wait_hours,
      "The parent-completion wait limit was reached; completed results are preserved.")
    Sys.sleep(cfg$wait_seconds)
  }
  parent_paths <- file.path(parent, "validation", c("manifest.json", "status.json", "tasks.rds",
    "interval_results.csv", "comparator_results.csv"))
  parent_hashes <- mgfp_hashes(parent_paths)
  original_rows <- data.table::fread(file.path(parent, "validation/interval_results.csv"))
  mwd_require(nrow(original_rows) == 2L * nrow(tasks) &&
    setequal(original_rows$job_id, tasks$job_id), "The completed parent design differs.")
  parent_validation_id <- mwd_json(file.path(parent, "validation/manifest.json"))$identity_sha256
  failed_ids <- unique(original_rows$job_id[!original_rows$returned])
  phase("repairing_numerical_failures", expected_repairs = length(failed_ids))
  replacements <- provenance <- list()
  for (job in failed_ids) {
    task <- tasks[match(job, tasks$job_id), ]
    original_path <- file.path(parent, "validation/fits", sprintf("fit_%06d.rds", job))
    original <- mfc_read(original_path, parent_validation_id, task)
    new_path <- file.path(cfg$output_root, "repairs", sprintf("fit_%06d.rds", job))
    if (file.exists(paste0(new_path, ".json"))) repaired <- mfc_read(new_path, id, task) else {
      repaired <- mfp_job(task, prior, context)
      mfc_save(repaired, new_path, id)
    }
    mfc_repair_check(original, repaired, task, prior)
    replacements[[length(replacements) + 1L]] <- repaired
    provenance[[length(provenance) + 1L]] <- data.frame(job_id = job,
      original_path = original_path, original_sha256 = mwd_file_hash(original_path),
      repaired_path = normalizePath(new_path), repaired_sha256 = mwd_file_hash(new_path),
      original_source = cfg$parent_commit, repaired_source = identity$source_commit)
  }
  if (length(provenance)) mwd_write(data.table::rbindlist(provenance), file.path(cfg$output_root, "repair_provenance.csv"))
  merged <- mfc_merge(original_rows, replacements)
  validation_dir <- file.path(cfg$output_root, "validation")
  dir.create(validation_dir, recursive = TRUE, showWarnings = FALSE)
  mwd_write(merged, file.path(validation_dir, "interval_results.csv"))
  benchmarks <- data.table::fread(file.path(parent, "validation/comparator_results.csv"))
  mwd_write(benchmarks, file.path(validation_dir, "comparator_results.csv"))
  validation <- mfc_summary(validation_dir, prior)
  validation$original_failed_pairs <- length(failed_ids); validation$repaired_pairs <- length(replacements)
  mgfp_json_write(validation, file.path(validation_dir, "status.json"))
  mgfp_json_write(list(parent_files = parent_hashes, original_failed_pairs = length(failed_ids),
    combined_rows_attributed_to_both_sources = TRUE), file.path(cfg$output_root, "parent_completion.json"))
  mwd_require(identical(parent_hashes, mgfp_hashes(parent_paths)), "The parent summaries changed during collection.")
  mwd_require(validation$numerical_pass, "Remaining numerical problems require review; application fitting was not started.")
  phase("application", expected_pairs = nrow(mfp_application_design(prior, context)))
  app_tasks <- mfp_application_design(prior, context)
  app_dir <- file.path(cfg$output_root, "application")
  dir.create(app_dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(app_tasks, file.path(app_dir, "tasks.rds"))
  mgfp_json_write(list(identity_sha256 = id, source_commit = identity$source_commit,
    fixed_reference = prior$application_reference), file.path(app_dir, "manifest.json"))
  storage <- prior; storage$output_root <- parent
  mfp_capacity(app_dir, app_tasks, storage)
  mfc_run_tasks(app_tasks, app_dir, id, cfg$maximum_workers, prior$disk_reserve_gib,
    function(task) {
      result <- mfp_application_job(task, prior, context)
      if (isTRUE(result$returned)) mfc_result_check(result, task, prior)
      result
    })
  application <- mfp_collect_application(app_tasks, app_dir, id)
  mfc_application_review(app_dir)
  phase("fixed_target_diagnostics")
  diagnostic_tasks <- mfc_diagnostic_design(merged, cfg$diagnostics)
  diagnostic_dir <- file.path(cfg$output_root, "diagnostics")
  dir.create(diagnostic_dir, recursive = TRUE, showWarnings = FALSE)
  mwd_write(diagnostic_tasks, file.path(diagnostic_dir, "tasks.csv"))
  original_fit <- function(job) file.path(parent, "validation/fits", sprintf("fit_%06d.rds", job))
  mfc_run_tasks(diagnostic_tasks, diagnostic_dir, id, cfg$diagnostics$maximum_workers, prior$disk_reserve_gib,
    function(task) mfc_diagnostic_job(task, original_fit(task$job_id), prior, context, cfg$diagnostics,
      policy, parent_validation_id))
  diagnostics <- lapply(seq_len(nrow(diagnostic_tasks)), function(j)
    mfc_read(file.path(diagnostic_dir, "fits", sprintf("fit_%06d.rds", diagnostic_tasks$job_id[j])),
      id, diagnostic_tasks[j, ]))
  good <- diagnostics[vapply(diagnostics, function(r) isTRUE(r$returned), FALSE)]
  if (length(good)) {
    comparisons <- data.table::rbindlist(lapply(good, function(r) cbind(job_id = r$task$job_id, r$comparison)))
    mwd_write(comparisons, file.path(diagnostic_dir, "mean_comparison.csv"))
    mwd_write(data.table::rbindlist(lapply(good, function(r) data.frame(job_id = r$task$job_id,
      pattern = r$task$pattern, q = r$q, delta = r$delta_reference, adequate = r$adequate,
      original_content = r$original_population_content, diagnostic_content = r$population_content,
      lower = r$lower, upper = r$upper, width = r$width,
      original_dp_probability = r$original_dp_probability, diagnostic_dp_probability = r$dp_probability,
      ecm_contains_diagnostic = r$ecm_contains_diagnostic, diagnostic_contains_ecm = r$diagnostic_contains_ecm,
      requested_content = r$requested_content))), file.path(diagnostic_dir, "case_summary.csv"))
    mfc_diagnostic_pdf(good, file.path(diagnostic_dir, "fixed_target_traces.pdf"))
  }
  diagnostic_status <- list(completed = length(diagnostics), expected = nrow(diagnostic_tasks),
    returned = length(good), all_adequate = length(good) == length(diagnostics) &&
      all(vapply(good, function(r) isTRUE(r$adequate), FALSE)), diagnostic_only = TRUE,
    prior_uses_analysis_data = FALSE, article_replaced = FALSE)
  mgfp_json_write(diagnostic_status, file.path(diagnostic_dir, "status.json"))
  mfc_completion_report(cfg$output_root, validation, application, diagnostic_status,
    cfg$parent_commit, identity$source_commit)
  phase(if (application$numerical_pass && diagnostic_status$all_adequate) "complete_requires_scientific_review" else
    "completed_attempts_require_numerical_review", validation_numerical_pass = validation$numerical_pass,
    application_numerical_pass = application$numerical_pass, diagnostics = diagnostic_status,
    pointwise_underattainment_warnings = validation$pointwise_underattainment_warnings)
}, error = function(e) {
  phase("stopped_with_preserved_results", reason = conditionMessage(e))
  stop(e)
})
