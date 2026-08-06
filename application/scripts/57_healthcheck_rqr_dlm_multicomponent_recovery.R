#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L || !dir.exists(args[[1L]])) {
  stop(
    "Usage: 57_healthcheck_rqr_dlm_multicomponent_recovery.R <control-root>",
    call. = FALSE
  )
}
control_root <- normalizePath(args[[1L]], winslash = "/", mustWork = TRUE)
status_path <- file.path(control_root, "coordinator_status.tsv")
status <- if (file.exists(status_path)) {
  utils::read.delim(
    status_path, stringsAsFactors = FALSE, check.names = FALSE,
    colClasses = "character"
  )
} else {
  data.frame(field = character(), value = character())
}
field <- function(name, default = NA_character_) {
  value <- status$value[status$field == name]
  if (length(value)) value[[length(value)]] else default
}
output_root <- field("output_root")
if (is.na(output_root)) {
  path <- file.path(control_root, "output_root.txt")
  if (file.exists(path)) output_root <- readLines(path, n = 1L)
}
pid <- suppressWarnings(as.integer(field("coordinator_pid")))
pgid <- suppressWarnings(as.integer(field("coordinator_pgid")))
coordinator_live <- length(pid) == 1L && !is.na(pid) &&
  dir.exists(sprintf("/proc/%d", pid))
process_table <- tryCatch(
  utils::read.table(
    pipe("ps -eo pid=,pgid=,stat="),
    col.names = c("pid", "pgid", "stat"), stringsAsFactors = FALSE
  ),
  error = function(error) data.frame()
)
group_live <- if (nrow(process_table) && length(pgid) == 1L && !is.na(pgid)) {
  sum(process_table$pgid == pgid & !grepl("^Z", process_table$stat))
} else {
  as.integer(coordinator_live)
}
job_status_path <- if (!is.na(output_root)) {
  file.path(output_root, "job_status.csv")
} else {
  NA_character_
}
jobs <- if (!is.na(job_status_path) && file.exists(job_status_path)) {
  utils::read.csv(
    job_status_path, stringsAsFactors = FALSE, check.names = FALSE
  )
} else {
  data.frame()
}
manifest_path <- if (!is.na(output_root)) {
  file.path(output_root, "comparison_manifest.json")
} else {
  NA_character_
}
manifest <- if (!is.na(manifest_path) && file.exists(manifest_path)) {
  jsonlite::read_json(manifest_path, simplifyVector = TRUE)
} else {
  NULL
}
diagnostics_path <- if (!is.na(output_root)) {
  file.path(output_root, "candidate_diagnostics.csv")
} else {
  NA_character_
}
diagnostics <- if (!is.na(diagnostics_path) && file.exists(diagnostics_path)) {
  utils::read.csv(
    diagnostics_path, stringsAsFactors = FALSE, check.names = FALSE
  )
} else {
  data.frame()
}

summary <- data.frame(
  item = c(
    "coordinator_status", "coordinator_stage", "coordinator_live",
    "live_process_group_members", "candidate_jobs_published",
    "candidate_jobs_planned", "candidate_jobs_succeeded",
    "candidate_jobs_remaining", "diagnostics_passed",
    "diagnostics_planned", "selected_candidate",
    "comparison_manifest_present", "main_launch_authorized"
  ),
  value = c(
    field("status", "not_started"), field("stage", "unknown"),
    coordinator_live, group_live, nrow(jobs), 48L,
    if (nrow(jobs) && "ok" %in% names(jobs)) sum(jobs$ok) else 0L,
    max(0L, 48L - nrow(jobs)),
    if (nrow(diagnostics) && "pass" %in% names(diagnostics)) {
      sum(diagnostics$pass)
    } else {
      0L
    },
    572L,
    if (is.null(manifest)) "pending" else manifest$selected_candidate,
    !is.null(manifest), FALSE
  ),
  stringsAsFactors = FALSE
)
print(summary, row.names = FALSE)
if (!is.null(manifest)) {
  cat(sprintf(
    paste0(
      "\nTerminal comparison: %d/%d jobs, %d/%d diagnostics; ",
      "selected=%s. Main launch remains unauthorized.\n"
    ),
    manifest$jobs_succeeded, manifest$jobs,
    manifest$diagnostics_passed, manifest$diagnostics,
    manifest$selected_candidate
  ))
} else if (identical(field("status"), "failed")) {
  cat("\nTerminal decision: recovery comparison failed closed.\n")
} else {
  cat("\nThe bounded recovery comparison is incomplete.\n")
}
