#!/usr/bin/env Rscript

# Read-only health check for a detached confirmatory run.

arguments <- commandArgs(trailingOnly = TRUE)
if (!length(arguments) %in% c(1L, 2L)) {
  stop(
    paste(
      "Usage: 21_healthcheck_rqr_dlm_confirmatory_simulation.R",
      "<run-root> [supervisor-log-root]"
    ),
    call. = FALSE
  )
}
repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
run_root <- normalizePath(
  arguments[[1L]], winslash = "/", mustWork = TRUE
)
log_root <- if (length(arguments) == 2L) {
  normalizePath(arguments[[2L]], winslash = "/", mustWork = TRUE)
} else {
  ""
}
sys.source(
  file.path(
    repo_root, "application", "scripts", "lib",
    "rqr_dlm_confirmatory_simulation.R"
  ),
  envir = environment()
)
contract <- rqr_confirm_read_contract(repo_root)
catalog <- rqr_confirm_wave_catalog(contract, planning = "maximum")
state_root <- file.path(run_root, "wave_state")
run_contract_path <- file.path(state_root, "run_contract.json")
if (!file.exists(run_contract_path)) {
  cat("State: not started\n")
  quit(save = "no", status = 0L, runLast = FALSE)
}
binding <- jsonlite::read_json(
  run_contract_path, simplifyVector = TRUE
)
binding_fields <- c(
  "schema_version", "run_id", "authorization_commit",
  "reviewed_implementation_commit", "runtime_tree_digest",
  "config_sha256", "incidence_sha256", "seed_ledger_sha256",
  "task_plan_sha256", "wave_plan_sha256", "wave_output_base",
  "binding_digest"
)
if ("execution_policy_sha256" %in% names(binding)) {
  binding_fields <- c(binding_fields, "execution_policy_sha256")
}
binding <- binding[binding_fields]
records <- rqr_confirm_wave_state_records(
  state_root, catalog, binding, allow_active_start = TRUE
)
values <- records$completion_values
active_start <- records$active_start
decisions <- if (length(values)) {
  vapply(
    values, function(value) as.character(value$decision),
    character(1L)
  )
} else {
  character()
}
completed <- length(values)
failed <- any(decisions == "failed")
active_wave <- if (!is.null(active_start)) {
  as.character(active_start$wave_id)
} else {
  "none"
}
next_index <- completed + if (is.null(active_start)) 1L else 2L
next_wave <- if (failed) {
  "blocked_by_failed_wave"
} else if (next_index <= nrow(catalog)) {
  catalog$wave_id[[next_index]]
} else {
  "none"
}
pid <- NA_integer_
running <- FALSE
if (nzchar(log_root)) {
  pid_path <- file.path(log_root, "coordinator.pid")
  if (file.exists(pid_path)) {
    raw_pid <- suppressWarnings(as.numeric(readLines(
      pid_path, warn = FALSE, n = 1L
    )))
    if (length(raw_pid) == 1L && is.finite(raw_pid) &&
        raw_pid == floor(raw_pid) && raw_pid > 0L) {
      pid <- as.integer(raw_pid)
      running <- identical(
        suppressWarnings(tools::pskill(pid, signal = 0L)),
        TRUE
      )
    }
  }
}
run_state <- if (failed) {
  "stopped_after_failed_wave"
} else if (!is.null(active_start) || running) {
  "running_or_active"
} else if (completed == nrow(catalog)) {
  "all_waves_terminal"
} else {
  "stopped_before_next_wave"
}
wave_files <- list.files(
  file.path(run_root, "waves"), recursive = TRUE,
  all.files = TRUE, no.. = TRUE, full.names = TRUE
)
wave_bytes <- if (length(wave_files)) {
  sum(file.info(wave_files)$size, na.rm = TRUE)
} else {
  0
}
worker_status_paths <- list.files(
  file.path(run_root, "waves"), pattern = "^run_status\\.csv$",
  recursive = TRUE, full.names = TRUE
)
worker_status <- if (length(worker_status_paths)) {
  do.call(rbind, lapply(worker_status_paths, function(path) {
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  }))
} else {
  data.frame()
}
terminal_task_statuses <- c(
  "completed", "completed_with_fit_failure",
  "completed_with_diagnostic_warning", "cell_stop_failure",
  "global_stop_failure", "infrastructure_invalid"
)
planned_tasks <- nrow(rqr_confirm_replication_plan(
  contract, planning = "maximum"
))
terminal_tasks <- if (nrow(worker_status)) {
  sum(worker_status$status %in% terminal_task_statuses)
} else {
  0L
}
result_paths <- list.files(
  file.path(run_root, "waves"),
  pattern = "^replication_results\\.csv$",
  recursive = TRUE, full.names = TRUE
)
result_paths <- result_paths[grepl("/replications/", result_paths, fixed = TRUE)]
result_tables <- lapply(result_paths, function(path) {
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
})
result_rows <- if (length(result_tables)) sum(vapply(
  result_tables, nrow, integer(1L)
)) else 0L
planned_result_rows <- sum(rqr_confirm_fit_plan(
  contract, planning = "maximum"
)$replications)
diagnostic_paths <- list.files(
  file.path(run_root, "waves"), pattern = "^fit_diagnostics\\.csv$",
  recursive = TRUE, full.names = TRUE
)
diagnostic_paths <- diagnostic_paths[
  grepl("/replications/", diagnostic_paths, fixed = TRUE)
]
diagnostic_counts <- if (length(diagnostic_paths)) {
  rows <- lapply(diagnostic_paths, function(path) {
    value <- utils::read.csv(
      path, stringsAsFactors = FALSE, check.names = FALSE
    )
    c(total = nrow(value), passed = sum(as.logical(value$pass)))
  })
  colSums(do.call(rbind, rows))
} else {
  c(total = 0L, passed = 0L)
}
warning_rows <- if (length(result_tables)) sum(vapply(
  result_tables,
  function(value) sum(value$failure_class == "mcmc_diagnostic_warning"),
  integer(1L)
)) else 0L
wave_manifest_paths <- list.files(
  file.path(run_root, "waves"), pattern = "^wave_manifest\\.json$",
  recursive = TRUE, full.names = TRUE
)
diagnostic_aware <- if (length(wave_manifest_paths)) {
  isTRUE(jsonlite::read_json(
    wave_manifest_paths[[1L]], simplifyVector = TRUE
  )$diagnostic_aware_completion)
} else {
  FALSE
}
latest_collection <- sort(
  list.files(
    file.path(run_root, "collections"),
    pattern = "^batch-[0-9]{4}$", full.names = TRUE
  ),
  method = "radix"
)
latest_collection <- if (length(latest_collection)) {
  basename(tail(latest_collection, 1L))
} else {
  "none"
}
cat(if (diagnostic_aware) {
  "RQR-DLM diagnostic-aware completion health check\n"
} else {
  "RQR-DLM confirmatory health check\n"
})
cat("  state:", run_state, "\n")
cat("  run ID:", binding$run_id, "\n")
cat("  authorization commit:", binding$authorization_commit, "\n")
if (!is.na(pid)) {
  cat("  coordinator PID:", pid, "\n")
  cat("  coordinator running:", running, "\n")
}
cat("  terminal waves:", completed, "/", nrow(catalog), "\n")
cat("  passed waves:", sum(decisions == "passed"), "\n")
cat("  precision-stop skips:",
    sum(decisions == "skipped_precision_stop"), "\n")
cat("  failed waves:", sum(decisions == "failed"), "\n")
cat("  active canonical wave:", active_wave, "\n")
cat("  next canonical wave:", next_wave, "\n")
cat("  terminal DGP-replication tasks:", terminal_tasks, "/",
    planned_tasks, "\n")
cat("  remaining DGP-replication tasks:",
    planned_tasks - terminal_tasks, "\n")
cat("  completed method-replication results:", result_rows, "/",
    planned_result_rows, "\n")
cat("  frozen diagnostics passed:", diagnostic_counts[["passed"]],
    "/", diagnostic_counts[["total"]], "\n")
cat("  result rows with diagnostic warnings:", warning_rows, "\n")
cat("  latest collection:", latest_collection, "\n")
cat("  current wave artifact GiB:",
    sprintf("%.3f", wave_bytes / 1024^3), "\n")
cat("  final audit present:",
    dir.exists(file.path(run_root, "final_audit")), "\n")
