#!/usr/bin/env Rscript

# Read-only health check for the chained S10 guard and S05/S06 affected-wave
# development validation.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) {
  stop("Usage: 53_healthcheck_rqr_dlm_affected_wave_validation.R <run-root>",
       call. = FALSE)
}
run_root <- normalizePath(args[[1L]], winslash = "/", mustWork = FALSE)
status_path <- file.path(run_root, "control", "coordinator_status.tsv")
coordinator <- if (file.exists(status_path)) {
  utils::read.delim(status_path, stringsAsFactors = FALSE,
                    check.names = FALSE)
} else {
  data.frame(field = character(), value = character())
}
field <- function(name, default = "not_started") {
  value <- coordinator$value[coordinator$field == name]
  if (length(value)) value[[length(value)]] else default
}

guard_status_path <- file.path(run_root, "s10_guard", "job_status.csv")
guard <- if (file.exists(guard_status_path)) {
  utils::read.csv(guard_status_path, stringsAsFactors = FALSE,
                  check.names = FALSE)
} else data.frame()
guard_manifest_path <- file.path(
  run_root, "s10_guard", "guard_manifest.json"
)
guard_manifest <- if (file.exists(guard_manifest_path)) {
  jsonlite::read_json(guard_manifest_path, simplifyVector = TRUE)
} else NULL
worker_status_paths <- file.path(
  run_root, sprintf("worker_%02d", 1:8), "run_status.csv"
)
worker_status <- do.call(rbind, lapply(
  worker_status_paths[file.exists(worker_status_paths)],
  function(path) utils::read.csv(
    path, stringsAsFactors = FALSE, check.names = FALSE
  )
))
if (is.null(worker_status)) worker_status <- data.frame()
compact_closeout <- file.exists(file.path(
  run_root, "compact_closeout", "closeout.json"
))
coordinator_pid <- suppressWarnings(as.integer(field(
  "coordinator_pid", NA_character_
)))
coordinator_pgid <- suppressWarnings(as.integer(field(
  "coordinator_pgid", NA_character_
)))
coordinator_live <- length(coordinator_pid) == 1L &&
  !is.na(coordinator_pid) && dir.exists(sprintf("/proc/%d", coordinator_pid))
process_table <- tryCatch(
  utils::read.table(
    pipe("ps -eo pid=,pgid=,stat="),
    col.names = c("pid", "pgid", "stat"), stringsAsFactors = FALSE
  ),
  error = function(error) data.frame()
)
live_count <- if (nrow(process_table) &&
    length(coordinator_pgid) == 1L && !is.na(coordinator_pgid)) {
  sum(
    process_table$pgid == coordinator_pgid &
      !grepl("^Z", process_table$stat)
  )
} else {
  as.integer(coordinator_live)
}
guard_failed <- !is.null(guard_manifest) &&
  identical(guard_manifest$selected_policies_passed, FALSE)
effective_status <- if (guard_failed) "failed" else field("status")
effective_stage <- if (guard_failed) "s10_guard" else field("stage")

completed_tasks <- if (nrow(worker_status)) {
  sum(worker_status$status == "completed")
} else 0L
failed_tasks <- if (nrow(worker_status)) {
  sum(grepl("failure|failed", worker_status$status))
} else 0L
summary <- data.frame(
  item = c(
    "coordinator_status", "coordinator_stage", "coordinator_live",
    "live_process_group_members",
    "S10_guard_jobs_succeeded", "S10_guard_jobs_planned",
    "S10_guard_diagnostics_passed", "S10_guard_diagnostics_planned",
    "affected_tasks_completed", "affected_tasks_planned",
    "affected_tasks_failed", "affected_tasks_remaining",
    "worker_outputs_published", "compact_closeout_present"
  ),
  value = c(
    effective_status, effective_stage, coordinator_live, live_count,
    if (nrow(guard)) sum(guard$ok) else 0L,
    8L,
    if (is.null(guard_manifest)) 0L else guard_manifest$diagnostics_passed,
    if (is.null(guard_manifest)) 95L else guard_manifest$diagnostics,
    completed_tasks, 35L, failed_tasks,
    max(0L, 35L - completed_tasks - failed_tasks),
    sum(file.exists(worker_status_paths)), compact_closeout
  ),
  stringsAsFactors = FALSE
)
print(summary, row.names = FALSE)

if (compact_closeout) {
  closeout <- jsonlite::read_json(
    file.path(run_root, "compact_closeout", "closeout.json"),
    simplifyVector = TRUE
  )
  cat(sprintf(
    "\nTerminal decision: affected_wave_passed=%s; diagnostics=%d/%d.\n",
    closeout$affected_wave_passed,
    closeout$diagnostics_passed, closeout$diagnostics
  ))
} else if (guard_failed || failed_tasks > 0L ||
           identical(effective_status, "failed")) {
  cat("\nTerminal decision: FAIL-CLOSED.\n")
} else {
  cat("\nTerminal decision: validation is incomplete.\n")
}
