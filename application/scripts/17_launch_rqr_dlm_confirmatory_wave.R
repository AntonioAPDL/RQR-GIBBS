#!/usr/bin/env Rscript

# Launch one authorization-bound confirmatory task wave.  This coordinator
# only partitions a reviewed canonical wave across its frozen worker slots.
# Every worker still enters the fail-closed runner and process-group monitor.

arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) != 4L) {
  stop(
    paste(
      "Usage: 17_launch_rqr_dlm_confirmatory_wave.R",
      "<sentinel-core|execute-confirmatory> <wave-plan.csv>",
      "<wave-id> <fresh-output-root>"
    ),
    call. = FALSE
  )
}
mode <- arguments[[1L]]
wave_plan_argument <- arguments[[2L]]
wave_id <- arguments[[3L]]
output_root <- normalizePath(
  arguments[[4L]], winslash = "/", mustWork = FALSE
)
if (!mode %in% c("sentinel-core", "execute-confirmatory")) {
  stop("The wave launcher accepts execution modes only.", call. = FALSE)
}

repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(repo_root, "application", "DESCRIPTION"))) {
  stop("Run the wave launcher from the RQR-GIBBS root.", call. = FALSE)
}
sys.source(
  file.path(
    repo_root, "application", "scripts", "lib",
    "rqr_dlm_confirmatory_simulation.R"
  ),
  envir = environment()
)
contract <- rqr_confirm_read_contract(repo_root)
rqr_confirm_validate_contract(contract)
rqr_confirm_validate_budget(contract)
if (!isTRUE(contract$config$confirmatory_execution_authorized)) {
  rqr_confirm_authorized(
    contract, mode, expected_commit = "",
    authorization_bundle = NULL
  )
}
wave_plan_path <- normalizePath(
  wave_plan_argument, winslash = "/", mustWork = TRUE
)
reviewed_plan <- rqr_confirm_wave_plan(contract, planning = "maximum")
supplied_plan <- utils::read.csv(
  wave_plan_path, stringsAsFactors = FALSE, check.names = FALSE
)
rownames(reviewed_plan) <- rownames(supplied_plan) <- NULL
if (!identical(supplied_plan, reviewed_plan)) {
  stop("The supplied wave plan is not the complete canonical plan.",
       call. = FALSE)
}
selected <- supplied_plan[supplied_plan$wave_id == wave_id, , drop = FALSE]
if (!nrow(selected) || length(unique(selected$mode)) != 1L ||
    !identical(unique(selected$mode), mode)) {
  stop("The requested wave ID and runner mode are inconsistent.",
       call. = FALSE)
}
if (file.exists(output_root) || dir.exists(output_root)) {
  stop("The wave output root must be fresh.", call. = FALSE)
}
dir.create(output_root, recursive = TRUE, showWarnings = FALSE)

task_fields <- names(rqr_confirm_replication_plan(
  contract, planning = "maximum"
))
wave_tasks <- selected[task_fields]
rqr_confirm_validate_task_subset(wave_tasks, contract)
rqr_confirm_atomic_write_csv(
  wave_tasks, file.path(output_root, "wave_task_plan.csv")
)
rqr_confirm_atomic_write_csv(
  selected, file.path(output_root, "wave_assignment.csv")
)

wrapper <- file.path(
  repo_root, "application", "scripts",
  "15_run_rqr_dlm_confirmatory_simulation.sh"
)
if (!file.exists(wrapper) ||
    file.access(wrapper, mode = 1L) != 0L) {
  stop("The monitored confirmatory worker wrapper is unavailable.",
       call. = FALSE)
}
worker_slots <- sort(unique(selected$worker_slot), method = "radix")
worker_root <- file.path(output_root, "workers")
shard_root <- file.path(output_root, "task_shards")
log_root <- file.path(output_root, "launcher_logs")
monitor_root <- file.path(output_root, "resource_monitor")
dir.create(worker_root)
dir.create(shard_root)
dir.create(log_root)
dir.create(monitor_root)

run_worker <- function(worker_slot) {
  assigned <- selected[selected$worker_slot == worker_slot, , drop = FALSE]
  shard <- assigned[task_fields]
  shard_path <- file.path(
    shard_root, sprintf("worker-%02d.csv", worker_slot)
  )
  rqr_confirm_atomic_write_csv(shard, shard_path)
  worker_output <- file.path(
    worker_root, sprintf("worker-%02d", worker_slot)
  )
  stdout_path <- file.path(
    log_root, sprintf("worker-%02d.stdout.log", worker_slot)
  )
  stderr_path <- file.path(
    log_root, sprintf("worker-%02d.stderr.log", worker_slot)
  )
  worker_monitor_root <- file.path(
    monitor_root, sprintf("worker-%02d", worker_slot)
  )
  status <- system2(
    wrapper, c(mode, worker_output),
    stdout = stdout_path, stderr = stderr_path,
    env = c(
      sprintf("RQR_CONFIRM_TASK_FILE=%s", shard_path),
      sprintf(
        "RQR_CONFIRMATORY_MONITOR_ROOT=%s",
        worker_monitor_root
      ),
      sprintf(
        "RQR_MAX_PROCESS_GROUP_THREADS=%d",
        contract$config$resources$sampled_process_group_thread_ceiling
      )
    ),
    wait = TRUE
  )
  summary_paths <- list.files(
    worker_monitor_root, pattern = "-summary\\.csv$",
    full.names = TRUE
  )
  telemetry_paths <- list.files(
    worker_monitor_root, pattern = "\\.csv$",
    full.names = TRUE
  )
  telemetry_paths <- setdiff(telemetry_paths, summary_paths)
  if (length(summary_paths) != 1L || length(telemetry_paths) != 1L) {
    stop("A worker did not publish exactly one resource evidence pair.",
         call. = FALSE)
  }
  data.frame(
    wave_id = wave_id, mode = mode, worker_slot = worker_slot,
    task_count = nrow(shard), exit_status = as.integer(status),
    output_directory = worker_output,
    task_file_sha256 = rqr_confirm_sha256(shard_path),
    stdout_sha256 = rqr_confirm_sha256(stdout_path),
    stderr_sha256 = rqr_confirm_sha256(stderr_path),
    resource_summary_sha256 = rqr_confirm_sha256(summary_paths[[1L]]),
    resource_telemetry_sha256 = rqr_confirm_sha256(telemetry_paths[[1L]]),
    stringsAsFactors = FALSE
  )
}

worker_results <- parallel::mclapply(
  worker_slots, run_worker,
  mc.preschedule = FALSE, mc.cores = length(worker_slots)
)
worker_error <- vapply(
  worker_results, inherits, logical(1L), what = "try-error"
)
if (any(worker_error)) {
  messages <- vapply(
    worker_results[worker_error], as.character, character(1L)
  )
  rqr_confirm_atomic_write_json(
    list(
      schema_version = "rqrgibbs_dlm_wave_failure/1.0.0",
      wave_id = wave_id, mode = mode,
      message_digests = vapply(
        messages,
        digest::digest, character(1L),
        algo = "sha256", serialize = FALSE
      )
    ),
    file.path(output_root, "wave_launcher_failure.json")
  )
  stop("At least one confirmatory worker could not be launched.",
       call. = FALSE)
}
worker_results <- do.call(rbind, worker_results)
rqr_confirm_atomic_write_csv(
  worker_results, file.path(output_root, "wave_worker_status.csv")
)
rqr_confirm_atomic_write_json(
  list(
    schema_version = "rqrgibbs_dlm_wave/1.0.0",
    wave_id = wave_id, mode = mode,
    worker_limit = unique(selected$worker_limit),
    workers_used = length(worker_slots),
    task_count = nrow(selected),
    all_workers_passed = all(worker_results$exit_status == 0L),
    no_retry = TRUE, no_reseed = TRUE,
    source_commit = trimws(system2(
      "git", c("-C", shQuote(repo_root), "rev-parse", "HEAD"),
      stdout = TRUE, env = c("GIT_OPTIONAL_LOCKS=0")
    ))
  ),
  file.path(output_root, "wave_manifest.json")
)
hashes <- rqr_confirm_recursive_manifest(output_root)
rqr_confirm_atomic_write_csv(
  hashes, file.path(output_root, "wave_artifact_hashes.csv")
)
if (any(worker_results$exit_status != 0L)) {
  stop("A confirmatory worker failed; no later wave is authorized.",
       call. = FALSE)
}
cat("Confirmatory wave completed without a worker failure.\n")
cat("  wave:", wave_id, "\n")
cat("  mode:", mode, "\n")
cat("  workers:", length(worker_slots), "\n")
cat("  output:", output_root, "\n")
