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
live <- system2(
  "pgrep", c("-af", shQuote(basename(run_root))),
  stdout = TRUE, stderr = FALSE
)
live_status <- attr(live, "status")
if (is.null(live_status)) live_status <- 0L
live_count <- if (identical(as.integer(live_status), 0L)) length(live) else 0L

completed_tasks <- if (nrow(worker_status)) {
  sum(worker_status$status == "completed")
} else 0L
failed_tasks <- if (nrow(worker_status)) {
  sum(grepl("failure|failed", worker_status$status))
} else 0L
summary <- data.frame(
  item = c(
    "coordinator_status", "coordinator_stage", "live_matching_processes",
    "S10_guard_jobs_succeeded", "S10_guard_jobs_planned",
    "affected_tasks_completed", "affected_tasks_planned",
    "affected_tasks_failed", "affected_tasks_remaining",
    "worker_outputs_published", "compact_closeout_present"
  ),
  value = c(
    field("status"), field("stage"), live_count,
    if (nrow(guard)) sum(guard$ok) else 0L,
    8L, completed_tasks, 35L, failed_tasks,
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
} else if (failed_tasks > 0L || identical(field("status"), "failed")) {
  cat("\nTerminal decision: FAIL-CLOSED.\n")
} else {
  cat("\nTerminal decision: validation is incomplete.\n")
}
