#!/usr/bin/env Rscript

# Read-only closeout of a terminally failed confirmatory RQR-DLM run. The run
# tree is never changed. This script independently verifies each completed
# wave manifest and publishes only compact, reproducible audit tables.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2L || length(args) > 3L) {
  stop(paste(
    "Usage: 38_closeout_failed_rqr_dlm_confirmatory_run.R",
    "<failed-run-root> <fresh-output-dir> [supervisor-log-dir]"
  ), call. = FALSE)
}
repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(repo_root, "application", "DESCRIPTION"))) {
  stop("Run this script from the RQR-GIBBS repository root.", call. = FALSE)
}
source(file.path(
  repo_root, "application", "scripts", "lib",
  "rqr_dlm_confirmatory_simulation.R"
))
run_root <- normalizePath(args[[1L]], winslash = "/", mustWork = TRUE)
output_dir <- normalizePath(args[[2L]], winslash = "/", mustWork = FALSE)
supervisor_dir <- if (length(args) == 3L && nzchar(args[[3L]])) {
  normalizePath(args[[3L]], winslash = "/", mustWork = TRUE)
} else {
  ""
}
if (file.exists(output_dir) || dir.exists(output_dir)) {
  stop("The failed-run closeout output directory must be new.",
       call. = FALSE)
}
required <- c(
  "wave_state/run_contract.json", "wave_state/completions", "waves"
)
if (any(!file.exists(file.path(run_root, required)) &
        !dir.exists(file.path(run_root, required)))) {
  stop("The supplied run root is not a confirmatory run tree.",
       call. = FALSE)
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

read_csv_set <- function(paths) {
  if (!length(paths)) return(NULL)
  do.call(rbind, lapply(paths, function(path) {
    value <- utils::read.csv(
      path, stringsAsFactors = FALSE, check.names = FALSE
    )
    value$source_file <- rep(
      substring(path, nchar(run_root) + 2L), nrow(value)
    )
    value
  }))
}

wave_dirs <- sort(
  list.dirs(file.path(run_root, "waves"), recursive = FALSE),
  method = "radix"
)
if (!length(wave_dirs)) {
  stop("The failed run contains no completed wave directory.",
       call. = FALSE)
}
wave_rows <- vector("list", length(wave_dirs))
all_results <- all_diagnostics <- all_status <- all_failures <- list()
result_index <- diagnostic_index <- status_index <- failure_index <- 0L
artifact_rows <- vector("list", length(wave_dirs))

for (index in seq_along(wave_dirs)) {
  wave <- wave_dirs[[index]]
  rqr_confirm_verify_recursive_manifest(
    wave, manifest_name = "wave_artifact_hashes.csv"
  )
  wave_manifest <- jsonlite::read_json(
    file.path(wave, "wave_manifest.json"), simplifyVector = TRUE
  )
  artifact_manifest <- utils::read.csv(
    file.path(wave, "wave_artifact_hashes.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  artifact_rows[[index]] <- data.frame(
    wave_id = wave_manifest$wave_id,
    manifest_entries = nrow(artifact_manifest),
    verified_entries = nrow(artifact_manifest),
    all_verified = TRUE,
    manifest_sha256 = rqr_confirm_sha256(file.path(
      wave, "wave_artifact_hashes.csv"
    )),
    stringsAsFactors = FALSE
  )
  worker_root <- file.path(wave, "workers")
  worker_dirs <- sort(
    list.dirs(worker_root, recursive = FALSE), method = "radix"
  )
  result_paths <- file.path(worker_dirs, "replication_results.csv")
  result_paths <- result_paths[file.exists(result_paths)]
  diagnostic_paths <- file.path(worker_dirs, "fit_diagnostics.csv")
  diagnostic_paths <- diagnostic_paths[file.exists(diagnostic_paths)]
  status_paths <- file.path(worker_dirs, "run_status.csv")
  status_paths <- status_paths[file.exists(status_paths)]
  failure_paths <- file.path(worker_dirs, "failure_log.csv")
  failure_paths <- failure_paths[file.exists(failure_paths)]
  result <- read_csv_set(result_paths)
  diagnostics <- read_csv_set(diagnostic_paths)
  status <- read_csv_set(status_paths)
  failures <- read_csv_set(failure_paths)
  if (!is.null(result) && nrow(result)) {
    result$wave_id <- wave_manifest$wave_id
    result_index <- result_index + 1L
    all_results[[result_index]] <- result
  }
  if (!is.null(diagnostics) && nrow(diagnostics)) {
    diagnostics$wave_id <- wave_manifest$wave_id
    diagnostic_index <- diagnostic_index + 1L
    all_diagnostics[[diagnostic_index]] <- diagnostics
  }
  if (!is.null(status) && nrow(status)) {
    status$wave_id <- wave_manifest$wave_id
    status_index <- status_index + 1L
    all_status[[status_index]] <- status
  }
  if (!is.null(failures) && nrow(failures)) {
    failures$wave_id <- wave_manifest$wave_id
    failure_index <- failure_index + 1L
    all_failures[[failure_index]] <- failures
  }
  completion_path <- file.path(
    run_root, "wave_state", "completions",
    sprintf("%04d__%s.json", index, wave_manifest$wave_id)
  )
  completion <- jsonlite::read_json(
    completion_path, simplifyVector = TRUE
  )
  wave_rows[[index]] <- data.frame(
    canonical_wave_index = wave_manifest$canonical_wave_index,
    wave_id = wave_manifest$wave_id,
    phase = wave_manifest$phase,
    batch_group = wave_manifest$batch_group,
    task_count = wave_manifest$task_count,
    workers_used = wave_manifest$workers_used,
    decision = completion$decision,
    all_workers_passed = wave_manifest$all_workers_passed,
    source_commit = wave_manifest$source_commit,
    runtime_tree_digest = wave_manifest$runtime_tree_digest,
    completion_sha256 = rqr_confirm_sha256(completion_path),
    artifact_manifest_sha256 =
      artifact_rows[[index]]$manifest_sha256,
    stringsAsFactors = FALSE
  )
}

wave_summary <- do.call(rbind, wave_rows)
artifact_verification <- do.call(rbind, artifact_rows)
if (!any(wave_summary$decision == "failed")) {
  stop("The supplied run has no terminal failed wave.", call. = FALSE)
}
results <- if (length(all_results)) do.call(rbind, all_results) else NULL
diagnostics <- if (length(all_diagnostics)) {
  do.call(rbind, all_diagnostics)
} else {
  NULL
}
status <- if (length(all_status)) do.call(rbind, all_status) else NULL
failures <- if (length(all_failures)) do.call(rbind, all_failures) else NULL

contract <- rqr_confirm_read_contract(repo_root)
planned_tasks <- nrow(rqr_confirm_replication_plan(
  contract, planning = "maximum"
))
canonical_wave_count <- nrow(rqr_confirm_wave_catalog(
  contract, planning = "maximum"
))
budget <- rqr_confirm_budget_summary(contract, planning = "maximum")
planned_evaluations <- budget$value[
  budget$item == "method_interval_evaluations"
]
planned_chains <- budget$value[
  budget$item == "total_MCMC_chain_executions"
]
fully_completed_task_status <- if (is.null(status)) logical() else {
  status$status %in% c("completed", "completed_with_fit_failure")
}
mcmc_group <- if (is.null(diagnostics)) NULL else unique(diagnostics[c(
  "wave_id", "DGP", "replication", "method", "chains"
)])
progress <- data.frame(
  item = c(
    "canonical_waves", "passed_waves", "failed_waves",
    "not_started_waves", "replication_tasks_with_artifacts",
    "fully_completed_tasks", "method_evaluations",
    "MCMC_method_attempts", "MCMC_chain_executions",
    "diagnostic_rows", "diagnostic_rows_passed",
    "diagnostic_rows_failed"
  ),
  observed = c(
    nrow(wave_summary), sum(wave_summary$decision == "passed"),
    sum(wave_summary$decision == "failed"),
    canonical_wave_count - nrow(wave_summary),
    if (is.null(status)) 0L else nrow(status),
    sum(fully_completed_task_status),
    if (is.null(results)) 0L else nrow(results),
    if (is.null(mcmc_group)) 0L else nrow(mcmc_group),
    if (is.null(mcmc_group)) 0L else sum(mcmc_group$chains),
    if (is.null(diagnostics)) 0L else nrow(diagnostics),
    if (is.null(diagnostics)) 0L else sum(diagnostics$pass),
    if (is.null(diagnostics)) 0L else sum(!diagnostics$pass)
  ),
  denominator = c(
    canonical_wave_count, canonical_wave_count,
    canonical_wave_count, canonical_wave_count,
    planned_tasks, planned_tasks,
    planned_evaluations, NA, planned_chains, NA, NA, NA
  ),
  stringsAsFactors = FALSE
)

artifact_schemas <- rqr_confirm_artifact_schemas()
empty_schema <- function(fields) {
  as.data.frame(
    setNames(
      replicate(length(fields), character(), simplify = FALSE),
      fields
    ),
    stringsAsFactors = FALSE, check.names = FALSE
  )
}
failed_diagnostics <- if (is.null(diagnostics)) {
  empty_schema(artifact_schemas$fit_diagnostics)
} else {
  diagnostics[!diagnostics$pass, setdiff(
    names(diagnostics), "source_file"
  ), drop = FALSE]
}
failure_ledger <- if (is.null(failures)) {
  empty_schema(artifact_schemas$failure_ledger)
} else {
  failures
}

resource_paths <- unlist(lapply(wave_dirs, function(wave) {
  list.files(
    file.path(wave, "resource_monitor"),
    pattern = "-summary\\.csv$", recursive = TRUE,
    full.names = TRUE
  )
}), use.names = FALSE)
resources <- read_csv_set(resource_paths)
resource_summary <- data.frame(
  workers = length(resource_paths),
  all_runner_status_zero = !is.null(resources) &&
    all(resources$runner_status == 0L),
  max_elapsed_seconds = if (is.null(resources)) NA_real_ else
    max(resources$elapsed_seconds),
  max_sampled_processes = if (is.null(resources)) NA_real_ else
    max(resources$peak_processes),
  max_sampled_rss_kib = if (is.null(resources)) NA_real_ else
    max(resources$peak_rss_kib),
  max_sampled_threads = if (is.null(resources)) NA_real_ else
    max(resources$peak_threads),
  any_ceiling_reason = if (is.null(resources)) NA else
    any(resources$ceiling_reason != "none"),
  resource_ceiling_pass = !is.null(resources) &&
    all(resources$ceiling_reason == "none"),
  stringsAsFactors = FALSE
)

error_paths <- unlist(lapply(wave_dirs, function(wave) {
  list.files(
    file.path(wave, "resource_monitor"),
    pattern = "\\.stderr\\.log$", recursive = TRUE,
    full.names = TRUE
  )
}), use.names = FALSE)
worker_error_signatures <- if (!length(error_paths)) {
  data.frame(
    source_file = character(), error_message = character(),
    message_digest = character(), file_sha256 = character(),
    stringsAsFactors = FALSE
  )
} else {
  do.call(rbind, lapply(error_paths, function(path) {
    lines <- readLines(path, warn = FALSE)
    messages <- grep("^Error:", lines, value = TRUE)
    message <- if (length(messages)) tail(messages, 1L) else
      "no_R_error_line_recorded"
    data.frame(
      source_file = substring(path, nchar(run_root) + 2L),
      error_message = message,
      message_digest = digest::digest(
        message, algo = "sha256", serialize = FALSE
      ),
      file_sha256 = rqr_confirm_sha256(path),
      stringsAsFactors = FALSE
    )
  }))
}
passed_workers <- if (is.null(resources)) 0L else
  sum(resources$runner_status == 0L)
observed_workers <- if (is.null(resources)) 0L else nrow(resources)

lock_path <- file.path(run_root, ".coordinator.lock", "owner.json")
lock_owner <- if (file.exists(lock_path)) {
  jsonlite::read_json(lock_path, simplifyVector = TRUE)
} else {
  list(pid = NA_integer_, host = NA_character_)
}
owner_alive <- FALSE
if (is.finite(as.numeric(lock_owner$pid %||% NA_real_)) &&
    identical(as.character(lock_owner$host), Sys.info()[["nodename"]])) {
  owner_alive <- identical(
    system2("kill", c("-0", as.integer(lock_owner$pid)),
            stdout = FALSE, stderr = FALSE),
    0L
  )
}
coordinator_lifecycle <- data.frame(
  lock_present = file.exists(lock_path),
  owner_pid = as.integer(lock_owner$pid %||% NA_integer_),
  owner_host = as.character(lock_owner$host %||% NA_character_),
  owner_alive = owner_alive,
  stale_lock_preserved_as_input_evidence =
    file.exists(lock_path) && !owner_alive,
  coordinator_closeout_present = file.exists(file.path(
    run_root, "coordinator", "coordinator_closeout.json"
  )),
  final_audit_present = dir.exists(file.path(run_root, "final_audit")),
  stringsAsFactors = FALSE
)

input_paths <- c(
  file.path(run_root, "wave_state", "run_contract.json"),
  list.files(
    file.path(run_root, "wave_state", "completions"),
    pattern = "\\.json$", full.names = TRUE
  ),
  file.path(wave_dirs, "wave_manifest.json"),
  file.path(wave_dirs, "wave_artifact_hashes.csv")
)
if (nzchar(supervisor_dir)) {
  input_paths <- c(
    input_paths,
    file.path(supervisor_dir, c(
      "coordinator.pid", "coordinator.stderr.log",
      "coordinator.stdout.log", "launch_environment.csv"
    ))
  )
}
input_paths <- input_paths[file.exists(input_paths)]
input_hashes <- data.frame(
  path = normalizePath(input_paths, winslash = "/", mustWork = TRUE),
  bytes = as.numeric(file.info(input_paths)$size),
  sha256 = vapply(input_paths, rqr_confirm_sha256, character(1L)),
  stringsAsFactors = FALSE
)

rqr_confirm_atomic_write_csv(
  wave_summary, file.path(output_dir, "wave_summary.csv")
)
rqr_confirm_atomic_write_csv(
  artifact_verification,
  file.path(output_dir, "artifact_verification.csv")
)
rqr_confirm_atomic_write_csv(
  progress, file.path(output_dir, "progress_summary.csv")
)
rqr_confirm_atomic_write_csv(
  failed_diagnostics,
  file.path(output_dir, "failed_diagnostics.csv")
)
rqr_confirm_atomic_write_csv(
  failure_ledger, file.path(output_dir, "failure_ledger.csv")
)
rqr_confirm_atomic_write_csv(
  resource_summary, file.path(output_dir, "resource_summary.csv")
)
rqr_confirm_atomic_write_csv(
  worker_error_signatures,
  file.path(output_dir, "worker_error_signatures.csv")
)
rqr_confirm_atomic_write_csv(
  coordinator_lifecycle,
  file.path(output_dir, "coordinator_lifecycle.csv")
)
rqr_confirm_atomic_write_csv(
  input_hashes, file.path(output_dir, "input_artifact_hashes.csv")
)

run_contract_path <- file.path(run_root, "wave_state", "run_contract.json")
run_contract <- jsonlite::read_json(
  run_contract_path, simplifyVector = TRUE
)
failed_wave_index <- min(wave_summary$canonical_wave_index[
  wave_summary$decision == "failed"
])
closeout <- list(
  schema_version = "rqrgibbs_dlm_failed_run_closeout/1.1.0",
  run_id = run_contract$run_id,
  authorization_commit = run_contract$authorization_commit,
  reviewed_implementation_commit =
    run_contract$reviewed_implementation_commit,
  runtime_tree_digest = run_contract$runtime_tree_digest,
  config_sha256 = run_contract$config_sha256,
  incidence_sha256 = run_contract$incidence_sha256,
  seed_ledger_sha256 = run_contract$seed_ledger_sha256,
  task_plan_sha256 = run_contract$task_plan_sha256,
  wave_plan_sha256 = run_contract$wave_plan_sha256,
  run_contract_sha256 = rqr_confirm_sha256(run_contract_path),
  terminal_status = sprintf(
    "failed_at_canonical_wave_%d", failed_wave_index
  ),
  completed_waves = nrow(wave_summary),
  passed_waves = sum(wave_summary$decision == "passed"),
  failed_waves = sum(wave_summary$decision == "failed"),
  artifact_entries_verified = sum(artifact_verification$verified_entries),
  all_artifacts_verified = all(artifact_verification$all_verified),
  collection_present = length(list.dirs(
    file.path(run_root, "collections"), recursive = FALSE
  )) > 0L,
  final_audit_present = coordinator_lifecycle$final_audit_present,
  scientific_promotion = FALSE,
  partial_results_reusable_as_confirmatory_evidence = FALSE,
  generalized_bayes = TRUE,
  response_likelihood = FALSE,
  source_run_mutated = FALSE,
  completed_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
)
rqr_confirm_atomic_write_json(
  closeout, file.path(output_dir, "closeout.json")
)

readme <- c(
  "# Failed confirmatory RQR-DLM run closeout",
  "",
  sprintf("Run `%s` is terminally failed.", closeout$run_id),
  "It is retained only as immutable computational failure evidence.",
  "No partial result is promoted or reused as confirmatory evidence.",
  "",
  sprintf(
    "The run stopped at canonical wave %d; %d of %d workers passed.",
    failed_wave_index, passed_workers, observed_workers
  ),
  sprintf(
    "The worker logs contain %d distinct authenticated error signature(s).",
    length(unique(worker_error_signatures$message_digest))
  ),
  "See worker_error_signatures.csv for the compact failure evidence.",
  "Diagnostic thresholds, seeds, targets, and response laws were not changed",
  "during this closeout.",
  "",
  sprintf(
    "All %d entries in the completed wave manifests were rehashed and",
    closeout$artifact_entries_verified
  ),
  "verified from local bytes. Any stale coordinator lock was observed but",
  "left untouched as part of the failed-run evidence. Collection and final",
  "audit artifacts remain absent after the wave failure.",
  "",
  "The generalized-Bayes update remains a loss update for interval roots;",
  "it is not an ordinary response likelihood and does not define posterior-",
  "predictive response draws."
)
temporary <- tempfile(".failed-closeout-", tmpdir = output_dir)
writeLines(readme, temporary, useBytes = TRUE)
if (!file.rename(temporary, file.path(output_dir, "README.md"))) {
  stop("Could not atomically publish the closeout README.", call. = FALSE)
}
hashes <- rqr_confirm_recursive_manifest(output_dir)
rqr_confirm_atomic_write_csv(
  hashes, file.path(output_dir, "artifact_hashes.csv")
)
invisible(rqr_confirm_verify_recursive_manifest(output_dir))
cat("Failed-run closeout completed without mutating the run tree.\n")
cat("  output:", output_dir, "\n")
cat("  verified wave artifacts:", closeout$artifact_entries_verified, "\n")
