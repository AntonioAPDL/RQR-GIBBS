#!/usr/bin/env Rscript

# Resume-safe coordinator for the complete confirmatory RQR-DLM study.
#
# This script does not choose tasks or make scientific decisions. It advances
# the canonical append-only wave state one wave at a time, runs a complete
# integrity/precision collection after every declared batch boundary, and
# supplies that verified decision bundle to the next batch. Sentinel
# replications are part of the confirmatory study rather than a separate pilot.

arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) != 2L) {
  stop(
    paste(
      "Usage: 18_orchestrate_rqr_dlm_confirmatory_simulation.R",
      "<canonical-wave-plan.csv> <fresh-or-resumable-run-root>"
    ),
    call. = FALSE
  )
}

repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(repo_root, "application", "DESCRIPTION"))) {
  stop("Run the coordinator from the RQR-GIBBS root.", call. = FALSE)
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
  stop("The confirmatory execution flag remains false.", call. = FALSE)
}

wave_plan_path <- normalizePath(
  arguments[[1L]], winslash = "/", mustWork = TRUE
)
run_root <- normalizePath(
  arguments[[2L]], winslash = "/", mustWork = FALSE
)
canonical_plan <- rqr_confirm_wave_plan(contract, planning = "maximum")
supplied_plan <- utils::read.csv(
  wave_plan_path, stringsAsFactors = FALSE, check.names = FALSE
)
rownames(canonical_plan) <- rownames(supplied_plan) <- NULL
if (!identical(canonical_plan, supplied_plan)) {
  stop("The coordinator requires the exact canonical wave plan.",
       call. = FALSE)
}
catalog <- rqr_confirm_wave_catalog(contract, planning = "maximum")
if (!grepl(
    "^[A-Za-z0-9][A-Za-z0-9._-]{0,127}$",
    Sys.getenv("RQR_CONFIRMATORY_RUN_ID", unset = "")
  )) {
  stop("RQR_CONFIRMATORY_RUN_ID is missing or invalid.", call. = FALSE)
}

if (!dir.exists(run_root)) {
  dir.create(run_root, recursive = TRUE, showWarnings = FALSE)
}
run_root <- normalizePath(run_root, winslash = "/", mustWork = TRUE)
wave_root <- file.path(run_root, "waves")
state_root <- file.path(run_root, "wave_state")
collection_root <- file.path(run_root, "collections")
monitor_root <- file.path(run_root, "coordinator_monitor")
coordinator_root <- file.path(run_root, "coordinator")
for (directory in c(
    wave_root, collection_root, monitor_root, coordinator_root
  )) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
}

validate_wave_state <- function(require_present = FALSE) {
  run_contract_path <- file.path(state_root, "run_contract.json")
  if (!file.exists(run_contract_path)) {
    if (isTRUE(require_present)) {
      stop("The coordinator expected an append-only run contract.",
           call. = FALSE)
    }
    return(invisible(NULL))
  }
  stored <- jsonlite::read_json(
    run_contract_path, simplifyVector = TRUE
  )
  binding_fields <- c(
    "schema_version", "run_id", "authorization_commit",
    "reviewed_implementation_commit", "runtime_tree_digest",
    "config_sha256", "incidence_sha256", "seed_ledger_sha256",
    "task_plan_sha256", "wave_plan_sha256", "wave_output_base",
    "binding_digest"
  )
  if (!all(binding_fields %in% names(stored))) {
    stop("The append-only run contract is incomplete.", call. = FALSE)
  }
  rqr_confirm_wave_state_records(
    state_root, catalog, stored[binding_fields]
  )
}

closeout_path <- file.path(
  coordinator_root, "coordinator_closeout.json"
)
if (file.exists(closeout_path)) {
  closeout <- jsonlite::read_json(
    closeout_path, simplifyVector = TRUE
  )
  final_audit <- file.path(run_root, "final_audit")
  if (!identical(
      as.character(closeout$schema_version),
      "rqrgibbs_dlm_coordinator_closeout/1.0.0"
    ) ||
      !identical(
        as.character(closeout$run_id),
        Sys.getenv("RQR_CONFIRMATORY_RUN_ID")
      ) ||
      !dir.exists(final_audit)) {
    stop("The existing coordinator closeout is invalid.",
         call. = FALSE)
  }
  validate_wave_state(require_present = TRUE)
  rqr_confirm_verify_recursive_manifest(final_audit)
  cat("The complete confirmatory run was already closed out.\n")
  cat("  run root:", run_root, "\n")
  cat("  final audit:", final_audit, "\n")
  quit(save = "no", status = 0L, runLast = FALSE)
}

lock_path <- file.path(run_root, ".coordinator.lock")
if (!dir.create(lock_path, showWarnings = FALSE)) {
  stop(
    "The run root is already coordinated or has an unresolved stale lock.",
    call. = FALSE
  )
}
on.exit(unlink(lock_path, recursive = TRUE, force = TRUE), add = TRUE)
rqr_confirm_atomic_write_json(
  list(
    schema_version = "rqrgibbs_dlm_coordinator_lock/1.0.0",
    pid = Sys.getpid(),
    host = Sys.info()[["nodename"]],
    started_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
  ),
  file.path(lock_path, "owner.json")
)

Sys.setenv(
  RQR_CONFIRMATORY_WAVE_STATE_ROOT = state_root,
  RQR_CONFIRMATORY_WAVE_OUTPUT_BASE = wave_root,
  RQR_CONFIRMATORY_WAVE_PLAN = wave_plan_path
)
launcher <- file.path(
  repo_root, "application", "scripts",
  "17_launch_rqr_dlm_confirmatory_wave.R"
)
wrapper <- file.path(
  repo_root, "application", "scripts",
  "15_run_rqr_dlm_confirmatory_simulation.sh"
)
if (!file.exists(launcher) || !file.exists(wrapper)) {
  stop("The reviewed launcher or monitored wrapper is unavailable.",
       call. = FALSE)
}

completion_paths <- function() {
  sort(
    list.files(
      file.path(state_root, "completions"),
      pattern = "\\.json$", full.names = TRUE
    ),
    method = "radix"
  )
}
completion_values <- function() {
  lapply(completion_paths(), function(path) {
    jsonlite::read_json(path, simplifyVector = TRUE)
  })
}
passed_wave_ids <- function() {
  values <- completion_values()
  vapply(
    values[
      vapply(
        values,
        function(value) identical(
          as.character(value$decision), "passed"
        ),
        logical(1L)
      )
    ],
    function(value) as.character(value$wave_id),
    character(1L)
  )
}
collection_path <- function(batch_sequence) {
  file.path(
    collection_root, sprintf("batch-%04d", batch_sequence)
  )
}
latest_decision_path <- function() {
  candidates <- list.files(
    collection_root, pattern = "^batch-[0-9]{4}$",
    full.names = TRUE
  )
  candidates <- sort(candidates, method = "radix")
  candidates <- file.path(candidates, "batch_decisions.csv")
  candidates <- candidates[file.exists(candidates)]
  if (length(candidates)) tail(candidates, 1L) else ""
}

run_collection <- function(batch_sequence) {
  output <- collection_path(batch_sequence)
  if (dir.exists(output) || file.exists(output)) {
    decision <- file.path(output, "batch_decisions.csv")
    if (!file.exists(decision)) {
      stop("An existing batch collection is incomplete.",
           call. = FALSE)
    }
    rqr_confirm_verify_recursive_manifest(output)
    return(invisible(decision))
  }
  wave_ids <- passed_wave_ids()
  selected <- canonical_plan[
    canonical_plan$wave_id %in% wave_ids, , drop = FALSE
  ]
  selected <- selected[
    order(selected$execution_order, method = "radix"), , drop = FALSE
  ]
  task_fields <- names(rqr_confirm_replication_plan(
    contract, planning = "maximum"
  ))
  tasks <- selected[task_fields]
  rqr_confirm_validate_task_subset(tasks, contract)
  task_path <- file.path(
    coordinator_root,
    sprintf("batch-%04d-collection-task-plan.csv", batch_sequence)
  )
  if (!file.exists(task_path)) {
    rqr_confirm_atomic_write_csv(tasks, task_path)
  } else {
    existing <- utils::read.csv(
      task_path, stringsAsFactors = FALSE, check.names = FALSE
    )
    rownames(existing) <- rownames(tasks) <- NULL
    if (!identical(existing, tasks)) {
      stop("A resumed collection task plan changed.", call. = FALSE)
    }
  }
  Sys.setenv(
    RQR_CONFIRMATORY_RUN_ROOT = wave_root,
    RQR_CONFIRMATORY_COLLECTION_TASK_PLAN = task_path,
    RQR_CONFIRMATORY_MONITOR_ROOT = file.path(
      monitor_root, sprintf("collect-%04d", batch_sequence)
    )
  )
  status <- system2(
    "bash", c(wrapper, "collect", output),
    stdout = "", stderr = "", wait = TRUE
  )
  if (!identical(as.integer(status), 0L)) {
    stop(
      sprintf("Batch %d collection failed.", batch_sequence),
      call. = FALSE
    )
  }
  decision <- file.path(output, "batch_decisions.csv")
  if (!file.exists(decision)) {
    stop("A completed batch omitted its decision artifact.",
         call. = FALSE)
  }
  invisible(decision)
}

ensure_completed_boundary_collection <- function() {
  completed <- length(completion_paths())
  if (!completed) return(invisible(""))
  batch <- catalog$batch_sequence[[completed]]
  final_in_batch <- max(which(catalog$batch_sequence == batch))
  if (completed == final_in_batch) {
    return(run_collection(batch))
  }
  invisible("")
}

validate_wave_state()
ensure_completed_boundary_collection()
repeat {
  completed <- length(completion_paths())
  if (completed >= nrow(catalog)) break
  next_index <- completed + 1L
  current <- catalog[next_index, , drop = FALSE]
  prior_decision <- latest_decision_path()
  if (!is.na(current$prior_batch_target)) {
    if (!nzchar(prior_decision)) {
      stop("A later canonical batch lacks its prior decision.",
           call. = FALSE)
    }
    Sys.setenv(
      RQR_CONFIRMATORY_PRIOR_BATCH_DECISION = prior_decision
    )
  } else {
    Sys.unsetenv("RQR_CONFIRMATORY_PRIOR_BATCH_DECISION")
  }
  wave_output <- file.path(
    wave_root,
    sprintf("%04d__%s", next_index, current$wave_id)
  )
  status <- system2(
    "Rscript",
    c(
      launcher, current$mode, wave_plan_path,
      current$wave_id, wave_output
    ),
    stdout = "", stderr = "", wait = TRUE
  )
  if (!identical(as.integer(status), 0L)) {
    stop(
      sprintf(
        "Canonical wave %d (%s) failed.",
        next_index, current$wave_id
      ),
      call. = FALSE
    )
  }
  if (length(completion_paths()) != next_index) {
    stop("A wave returned success without one terminal state record.",
         call. = FALSE)
  }
  validate_wave_state(require_present = TRUE)
  final_in_batch <- max(which(
    catalog$batch_sequence == current$batch_sequence
  ))
  if (next_index == final_in_batch) {
    run_collection(current$batch_sequence)
  }
}

validate_wave_state(require_present = TRUE)
final_tasks <- canonical_plan[
  canonical_plan$wave_id %in% passed_wave_ids(), , drop = FALSE
]
final_task_path <- file.path(
  coordinator_root, "final-collection-task-plan.csv"
)
final_tasks <- final_tasks[names(rqr_confirm_replication_plan(
  contract, planning = "maximum"
))]
if (!file.exists(final_task_path)) {
  rqr_confirm_atomic_write_csv(final_tasks, final_task_path)
} else {
  existing <- utils::read.csv(
    final_task_path, stringsAsFactors = FALSE, check.names = FALSE
  )
  rownames(existing) <- rownames(final_tasks) <- NULL
  if (!identical(existing, final_tasks)) {
    stop("The resumed final task plan changed.", call. = FALSE)
  }
}
final_audit <- file.path(run_root, "final_audit")
Sys.setenv(
  RQR_CONFIRMATORY_RUN_ROOT = wave_root,
  RQR_CONFIRMATORY_COLLECTION_TASK_PLAN = final_task_path,
  RQR_CONFIRMATORY_MONITOR_ROOT =
    file.path(monitor_root, "final-audit")
)
if (!dir.exists(final_audit)) {
  audit_status <- system2(
    "bash", c(wrapper, "audit", final_audit),
    stdout = "", stderr = "", wait = TRUE
  )
  if (!identical(as.integer(audit_status), 0L)) {
    stop("The final confirmatory audit failed.", call. = FALSE)
  }
} else {
  rqr_confirm_verify_recursive_manifest(final_audit)
  audit_manifest <- jsonlite::read_json(
    file.path(final_audit, "audit_manifest.json"),
    simplifyVector = TRUE
  )
  if (!isTRUE(audit_manifest$analysis_complete) ||
      !identical(
        as.character(audit_manifest$status),
        "integrity_and_analysis_complete"
      )) {
    stop("The resumed final audit is incomplete.", call. = FALSE)
  }
}
rqr_confirm_atomic_write_json(
  list(
    schema_version = "rqrgibbs_dlm_coordinator_closeout/1.0.0",
    run_id = Sys.getenv("RQR_CONFIRMATORY_RUN_ID"),
    canonical_wave_count = nrow(catalog),
    terminal_wave_count = length(completion_paths()),
    passed_wave_count = length(passed_wave_ids()),
    skipped_wave_count = nrow(catalog) - length(passed_wave_ids()),
    final_audit = normalizePath(final_audit, winslash = "/"),
    completed_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
  ),
  closeout_path
)
cat("The complete confirmatory RQR-DLM study reached final audit.\n")
cat("  run root:", run_root, "\n")
cat("  final audit:", final_audit, "\n")
