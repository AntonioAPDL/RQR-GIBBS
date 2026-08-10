#!/usr/bin/env Rscript

# Authenticate and summarize a completed eight-worker S05/S06 affected-wave
# development gate. The compact closeout remains local until a later explicit
# promotion step copies reviewed evidence into docs/audits/.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L || !dir.exists(args[[1L]])) {
  stop("Usage: 52_closeout_rqr_dlm_affected_wave_validation.R <run-root>",
       call. = FALSE)
}
repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
run_root <- normalizePath(args[[1L]], winslash = "/", mustWork = TRUE)
closeout_root <- file.path(run_root, "compact_closeout")
if (file.exists(closeout_root) || dir.exists(closeout_root)) {
  stop("The compact closeout directory must be new.", call. = FALSE)
}

suppressPackageStartupMessages(library(rqrgibbs))
source(file.path(
  repo_root, "application", "scripts", "lib",
  "rqr_dlm_confirmatory_simulation.R"
))
contract <- rqr_confirm_read_contract(repo_root)
rqr_confirm_validate_contract(contract, require_closed = TRUE)

required_roots <- c(
  preflight = file.path(run_root, "preflight"),
  reference = file.path(run_root, "oracle_reference"),
  preparation = file.path(run_root, "task_preparation"),
  guard = file.path(run_root, "s10_guard")
)
if (any(!dir.exists(required_roots))) {
  stop("The affected-wave run is missing a required validation stage.",
       call. = FALSE)
}
for (path in required_roots) {
  rqr_confirm_verify_recursive_manifest(path)
}
guard <- jsonlite::read_json(
  file.path(required_roots[["guard"]], "guard_manifest.json"),
  simplifyVector = TRUE
)
preparation <- jsonlite::read_json(
  file.path(required_roots[["preparation"]], "preparation_manifest.json"),
  simplifyVector = TRUE
)
expected_commit <- tolower(preparation$source_commit)
if (!grepl("^[0-9a-f]{40}$", expected_commit) ||
    !isTRUE(guard$selected_policies_passed) ||
    !isTRUE(guard$all_exact_joint_target) ||
    !isTRUE(guard$all_zero_repairs) ||
    !identical(tolower(guard$source_commit), expected_commit) ||
    !identical(as.integer(preparation$tasks), 35L) ||
    !identical(as.integer(preparation$method_evaluations), 278L) ||
    isTRUE(preparation$confirmatory_execution_authorized) ||
    isTRUE(preparation$scientific_promotion) ||
    isTRUE(preparation$development_outputs_reusable)) {
  stop("The guard or task-preparation contract did not pass.",
       call. = FALSE)
}

worker_roots <- file.path(run_root, sprintf("worker_%02d", 1:8))
if (any(!dir.exists(worker_roots))) {
  stop("All eight affected-wave workers are required.", call. = FALSE)
}
status_rows <- result_rows <- diagnostic_rows <- failure_rows <- list()
for (slot in seq_along(worker_roots)) {
  worker <- worker_roots[[slot]]
  rqr_confirm_verify_recursive_manifest(worker)
  manifest <- jsonlite::read_json(
    file.path(worker, "run_manifest.json"), simplifyVector = TRUE
  )
  if (!identical(manifest$mode, "development-affected-wave") ||
      !identical(tolower(manifest$source_commit), expected_commit) ||
      !isTRUE(manifest$development_validation) ||
      isTRUE(manifest$development_outputs_reusable) ||
      isTRUE(manifest$scientific_promotion) ||
      isTRUE(manifest$confirmatory_execution_authorized) ||
      !identical(manifest$status, "passed")) {
    stop("An affected-wave worker manifest is not eligible.",
         call. = FALSE)
  }
  status_rows[[slot]] <- utils::read.csv(
    file.path(worker, "run_status.csv"), stringsAsFactors = FALSE,
    check.names = FALSE
  )
  result_rows[[slot]] <- utils::read.csv(
    file.path(worker, "replication_results.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  diagnostic_path <- file.path(worker, "fit_diagnostics.csv")
  if (!file.exists(diagnostic_path)) {
    stop("An affected-wave worker omitted fit diagnostics.",
         call. = FALSE)
  }
  diagnostic_rows[[slot]] <- utils::read.csv(
    diagnostic_path, stringsAsFactors = FALSE, check.names = FALSE
  )
  failure_rows[[slot]] <- utils::read.csv(
    file.path(worker, "failure_log.csv"), stringsAsFactors = FALSE,
    check.names = FALSE
  )
}
status <- do.call(rbind, status_rows)
results <- do.call(rbind, result_rows)
diagnostics <- do.call(rbind, diagnostic_rows)
failures <- do.call(rbind, failure_rows)
rownames(status) <- rownames(results) <- rownames(diagnostics) <- NULL
if (nrow(status) != 35L || anyDuplicated(status$replication_task_id) ||
    !all(status$status == "completed") ||
    nrow(results) != 278L ||
    any(results$status != "completed") ||
    nrow(diagnostics) == 0L || !all(diagnostics$pass) ||
    nrow(failures) != 0L) {
  stop("The affected-wave task, method, or diagnostic gate failed.",
       call. = FALSE)
}

prepared_plan <- utils::read.csv(
  file.path(required_roots[["preparation"]], "affected_wave_plan.csv"),
  stringsAsFactors = FALSE, check.names = FALSE
)
if (!setequal(status$replication_task_id,
              prepared_plan$replication_task_id)) {
  stop("Worker outputs do not cover the exact prepared task set.",
       call. = FALSE)
}

dir.create(closeout_root, recursive = TRUE, showWarnings = FALSE)
rqr_confirm_atomic_write_csv(status, file.path(closeout_root, "run_status.csv"))
rqr_confirm_atomic_write_csv(
  diagnostics, file.path(closeout_root, "fit_diagnostics.csv")
)
diagnostic_summary <- do.call(rbind, lapply(
  split(diagnostics, diagnostics$method),
  function(value) data.frame(
    method = value$method[[1L]], diagnostics = nrow(value),
    diagnostics_passed = sum(value$pass),
    max_rhat = suppressWarnings(max(value$rhat, na.rm = TRUE)),
    min_bulk_ess = suppressWarnings(min(value$ess_bulk, na.rm = TRUE)),
    min_tail_ess = suppressWarnings(min(value$ess_tail, na.rm = TRUE)),
    max_mcse_over_sd = suppressWarnings(
      max(value$mcse_over_sd, na.rm = TRUE)
    ),
    stringsAsFactors = FALSE
  )
))
rqr_confirm_atomic_write_csv(
  diagnostic_summary,
  file.path(closeout_root, "diagnostic_summary.csv")
)
input_hashes <- data.frame(
  stage = names(required_roots),
  artifact_manifest_sha256 = vapply(
    required_roots,
    function(path) rqr_confirm_sha256(file.path(path, "artifact_hashes.csv")),
    character(1L)
  ),
  stringsAsFactors = FALSE
)
rqr_confirm_atomic_write_csv(
  input_hashes, file.path(closeout_root, "input_artifact_hashes.csv")
)
closeout <- list(
  schema_version = "rqrgibbs_dlm_affected_wave_closeout/1.0.0",
  source_commit = expected_commit,
  wave_id = preparation$wave_id,
  S10_guard_passed = TRUE,
  workers = 8L, tasks = nrow(status),
  method_evaluations = nrow(results),
  diagnostics = nrow(diagnostics),
  diagnostics_passed = sum(diagnostics$pass),
  failures = nrow(failures), retries = 0L, reseeding = FALSE,
  all_exact_joint_target = TRUE, all_zero_repairs = TRUE,
  scientific_metrics_used = FALSE, scientific_promotion = FALSE,
  confirmatory_launch_authorized = FALSE,
  affected_wave_passed = TRUE,
  completed_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
)
rqr_confirm_atomic_write_json(
  closeout, file.path(closeout_root, "closeout.json")
)
hashes <- rqr_confirm_recursive_manifest(closeout_root)
rqr_confirm_atomic_write_csv(
  hashes, file.path(closeout_root, "artifact_hashes.csv")
)
cat(sprintf(
  "Affected-wave closeout passed: %d tasks, %d methods, %d diagnostics.\n",
  nrow(status), nrow(results), nrow(diagnostics)
))
