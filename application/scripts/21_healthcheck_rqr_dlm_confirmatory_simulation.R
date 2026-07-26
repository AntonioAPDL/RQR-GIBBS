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
binding <- binding[binding_fields]
records <- rqr_confirm_wave_state_records(
  state_root, catalog, binding
)
values <- records$completion_values
decisions <- if (length(values)) {
  vapply(
    values, function(value) as.character(value$decision),
    character(1L)
  )
} else {
  character()
}
completed <- length(values)
next_wave <- if (completed < nrow(catalog)) {
  catalog$wave_id[[completed + 1L]]
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
wave_files <- list.files(
  file.path(run_root, "waves"), recursive = TRUE,
  all.files = TRUE, no.. = TRUE, full.names = TRUE
)
wave_bytes <- if (length(wave_files)) {
  sum(file.info(wave_files)$size, na.rm = TRUE)
} else {
  0
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
cat("RQR-DLM confirmatory health check\n")
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
cat("  next canonical wave:", next_wave, "\n")
cat("  latest collection:", latest_collection, "\n")
cat("  current wave artifact GiB:",
    sprintf("%.3f", wave_bytes / 1024^3), "\n")
cat("  final audit present:",
    dir.exists(file.path(run_root, "final_audit")), "\n")
