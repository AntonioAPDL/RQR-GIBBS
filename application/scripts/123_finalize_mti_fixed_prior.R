#!/usr/bin/env Rscript
private <- Sys.getenv("MTI_PRIOR_R_LIBRARY")
if (nzchar(private)) .libPaths(c(normalizePath(private, mustWork = TRUE), .libPaths()))
for (name in c("mti_gibbs_practical", "mti_gibbs_full_report_review", "mti_gibbs_full_publication",
  "mti_profile_revision", "mti_gibbs_width_diagnosis", "mti_prior_screen", "pharma_tolerance_application",
  "mti_fixed_prior", "mti_fixed_prior_closure", "mti_fixed_prior_finalization"))
  source(file.path("application/scripts/lib", paste0(name, ".R")))
data.table::setDTthreads(1L)
RNGkind("Mersenne-Twister", "Inversion", "Rejection")
args <- commandArgs(TRUE); mode <- if (length(args)) args[1] else "prepare"
configuration <- "application/config/mti_fixed_prior_finalization_20261115.json"
cfg <- mwd_json(configuration); prior <- mwd_json(cfg$prior_config)
dir.create(cfg$output_root, recursive = TRUE, showWarnings = FALSE)
if (mode == "health") {
  path <- file.path(cfg$output_root, "status.json")
  if (file.exists(path)) print(mwd_json(path))
  quit(save = "no")
}
mwd_require(mode %in% c("prepare", "check", "run"), "Unknown finalization command.")
continuation <- file.path(mgfp_common(), cfg$continuation_worktree, cfg$continuation_output)
manifest <- mwd_json(file.path(continuation, "manifest.json"))
mwd_require(identical(manifest$identity$source_commit, cfg$continuation_commit) &&
  identical(mwd_hash(prior), mwd_hash(manifest$identity$statistical_configuration)) &&
  !cfg$automatic_prior_tuning && !cfg$automatic_article_replacement,
  "The original fixed statistical specification is required.")
mgfp_check_hashes(manifest$identity$programs)
mgfp_check_hashes(manifest$identity$parent_runtime$inputs)
mgfp_check_hashes(manifest$identity$parent_runtime$application_data)
package <- mps_package_check()
mwd_require(identical(unname(unlist(package$installed_files)),
  unname(unlist(manifest$identity$package$installed_files))), "The installed numerical computation changed.")
mwd_require(identical(R.version.string, manifest$identity$parent_runtime$R), "The R runtime changed.")
for (name in names(manifest$identity$parent_runtime$dependencies))
  mwd_require(identical(as.character(packageVersion(name)), manifest$identity$parent_runtime$dependencies[[name]]),
    paste("A dependency changed:", name))
context <- mps_context(prior, cfg$output_root)
policy <- mwd_json(mwd_json(prior$diagnostic_config)$practical_config)$policy
if (mode == "prepare") {
  cat("Same-target precision budgets:", cfg$retained_budgets, "\nPrior SD:", prior$primary_kappa,
    "\nPrior and target selection will not be changed.\n")
  quit(save = "no")
}
if (mode == "check") {
  path <- file.path(continuation, "application/fits/fit_000006.rds")
  original <- mfc_read(path, manifest$identity_sha256)
  hash <- mwd_file_hash(path)
  refined <- mff_extend(original, prior, context, cfg$retained_budgets, policy)
  saveRDS(refined, file.path(cfg$output_root, "checked_precision_refinement.rds"))
  mwd_require(all(refined$rows$gibbs_adequate) && identical(hash, mwd_file_hash(path)),
    "The precision check failed or the original file changed.")
  print(refined$sampling_history)
  cat("Saved chain prefixes, targets, priors, and original file are unchanged.\n")
  quit(save = "no")
}
mwd_require(!nzchar(mgfp_git("status", "--porcelain")), "Commit the finalization source before launch.")
programs <- unique(c(configuration, cfg$prior_config, "application/scripts/123_finalize_mti_fixed_prior.R",
  "application/scripts/lib/mti_fixed_prior_finalization.R", "application/scripts/lib/mti_fixed_prior_publication.R",
  "application/scripts/lib/mti_fixed_prior.R", "application/scripts/lib/mti_fixed_prior_closure.R", mps_programs))
identity <- list(source_commit = mgfp_git("rev-parse", "HEAD"), configuration = cfg,
  statistical_configuration = prior, programs = mgfp_hashes(programs), package = package,
  continuation_manifest_sha256 = mwd_file_hash(file.path(continuation, "manifest.json")),
  continuation_commit = cfg$continuation_commit,
  exdqlm_reference_commit = manifest$identity$exdqlm_reference_commit)
id <- mwd_hash(identity); path <- file.path(cfg$output_root, "manifest.json")
if (file.exists(path)) mwd_require(identical(mwd_json(path)$identity_sha256, id), "Finalization identity changed.") else
  mgfp_json_write(list(identity = identity, identity_sha256 = id), path)
phase <- function(name, ...) mgfp_json_write(c(list(phase = name, time = mgfp_time(),
  prior_uses_analysis_data = FALSE, article_replaced = FALSE), list(...)), file.path(cfg$output_root, "status.json"))
tryCatch({
  phase("waiting_for_continuation")
  start <- Sys.time()
  while (!mff_ready(continuation)) {
    mwd_require(as.numeric(difftime(Sys.time(), start, units = "hours")) < cfg$maximum_wait_hours,
      "The completion wait limit was reached; original results are preserved.")
    Sys.sleep(cfg$wait_seconds)
  }
  inputs <- file.path(continuation, c("status.json", "application/status.json", "application/tasks.rds",
    "application/interval_results.csv", "application/comparator_results.csv", "diagnostics/status.json",
    "diagnostics/case_summary.csv", "diagnostics/mean_comparison.csv", "validation/status.json",
    "validation/interval_results.csv", "validation/comparator_results.csv", "repair_provenance.csv"))
  input_hashes <- mgfp_hashes(inputs)
  tasks <- readRDS(file.path(continuation, "application/tasks.rds"))
  original_path <- function(job) file.path(continuation, "application/fits", sprintf("fit_%06d.rds", job))
  eligible <- integer()
  for (j in seq_len(nrow(tasks))) {
    original <- mfc_read(original_path(tasks$job_id[j]), manifest$identity_sha256, tasks[j, ])
    if (isTRUE(original$returned)) mfc_result_check(original, tasks[j, ], prior)
    if (mff_precision_only(original)) eligible <- c(eligible, j)
  }
  phase("precision_refinement", expected = length(eligible))
  directory <- file.path(cfg$output_root, "precision")
  reuse <- if (!is.null(cfg$precision_reuse_worktree))
    file.path(mgfp_common(), cfg$precision_reuse_worktree, cfg$output_root) else NULL
  if (!is.null(reuse)) {
    reuse_manifest <- mwd_json(file.path(reuse, "manifest.json"))
    mwd_require(identical(reuse_manifest$identity$source_commit, cfg$precision_reuse_commit) &&
      identical(reuse_manifest$identity$continuation_manifest_sha256, identity$continuation_manifest_sha256) &&
      identical(mwd_hash(reuse_manifest$identity$statistical_configuration), mwd_hash(prior)),
      "Saved precision refinements have different source data or priors.")
    saved_programs <- reuse_manifest$identity$programs
    names(saved_programs) <- file.path(mgfp_common(), cfg$precision_reuse_worktree, names(saved_programs))
    mgfp_check_hashes(saved_programs)
  }
  mfc_run_tasks(tasks[eligible, , drop = FALSE], directory, id, cfg$maximum_workers, prior$disk_reserve_gib,
    function(task) {
      original <- mfc_read(original_path(task$job_id), manifest$identity_sha256, task)
      saved <- if (!is.null(reuse)) file.path(reuse, "precision/fits", sprintf("fit_%06d.rds", task$job_id)) else ""
      if (file.exists(saved)) {
        r <- mfc_read(saved, reuse_manifest$identity_sha256, task)
        mfc_result_check(r, task, prior)
        mff_prefix_check(original$fits, r$fits)
        fields <- c("task", "prior", "sample_sha256", "candidates", "selected", "selected_ecm_trace",
          "selected_knot_adjustments", "q", "delta_reference", "dp_prior", "comparators")
        mwd_require(identical(mwd_hash(original[fields]), mwd_hash(r[fields])), "A saved precision target changed.")
        return(r)
      }
      mff_extend(original, prior, context, cfg$retained_budgets, policy)
    })
  refinements <- lapply(eligible, function(j) mfc_read(file.path(directory, "fits",
    sprintf("fit_%06d.rds", tasks$job_id[j])), id, tasks[j, ]))
  mwd_require(all(vapply(refinements, function(r) isTRUE(r$returned) &&
    isTRUE(r$precision_refinement$original_prefix_preserved), FALSE)),
    "A precision refinement needs review; no publication evidence was promoted.")
  rows <- data.table::fread(file.path(continuation, "application/interval_results.csv"))
  combined <- mff_merge(rows, refinements)
  application <- file.path(cfg$output_root, "application")
  dir.create(application, recursive = TRUE, showWarnings = FALSE)
  mwd_write(combined, file.path(application, "interval_results.csv"))
  mwd_write(data.table::fread(file.path(continuation, "application/comparator_results.csv")),
    file.path(application, "comparator_results.csv"))
  mfc_application_review(application)
  numerical <- isTRUE(all(combined$returned & combined$ecm_coordinate_ok & combined$gibbs_adequate))
  provenance <- data.frame(job_id = tasks$job_id,
    original_path = file.path(continuation, "application/fits", sprintf("fit_%06d.rds", tasks$job_id)),
    original_sha256 = vapply(tasks$job_id, function(job) mwd_file_hash(file.path(continuation,
      "application/fits", sprintf("fit_%06d.rds", job))), ""),
    refined_path = NA_character_, refined_sha256 = NA_character_,
    original_source = cfg$continuation_commit, refinement_source = identity$source_commit)
  for (j in eligible) {
    p <- file.path(directory, "fits", sprintf("fit_%06d.rds", tasks$job_id[j]))
    provenance$refined_path[j] <- normalizePath(p); provenance$refined_sha256[j] <- mwd_file_hash(p)
  }
  mwd_write(provenance, file.path(cfg$output_root, "application_fit_provenance.csv"))
  mgfp_json_write(list(completed = nrow(tasks), expected = nrow(tasks), refined_pairs = length(eligible),
    numerical_pass = numerical), file.path(application, "status.json"))
  source(file.path("application/scripts/lib", "mti_fixed_prior_publication.R"))
  phase("preparing_reporting_evidence", application_numerical_pass = numerical)
  evidence <- mff_prepare_evidence(cfg, prior, context, continuation, application, identity)
  mwd_require(identical(input_hashes, mgfp_hashes(inputs)), "Original summaries changed during finalization.")
  mgfp_json_write(list(original_inputs = input_hashes, all_original_files_preserved = TRUE),
    file.path(cfg$output_root, "source_checks.json"))
  diagnostic <- mwd_json(file.path(continuation, "diagnostics/status.json"))
  phase(if (numerical && diagnostic$all_adequate) "ready_for_manuscript_review" else "numerical_review_required",
    application_numerical_pass = numerical, refined_pairs = length(eligible),
    diagnostic_numerical_pass = diagnostic$all_adequate, evidence_dir = evidence)
}, error = function(e) {
  phase("stopped_with_preserved_results", reason = conditionMessage(e)); stop(e)
})
