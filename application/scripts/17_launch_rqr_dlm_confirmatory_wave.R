#!/usr/bin/env Rscript

# Launch exactly the next authorization-bound confirmatory task wave.
#
# The append-only state contract prevents arbitrary wave selection, replay,
# skipping the same-batch sentinel, or expanding a replication batch without
# a verified prior batch decision. A recorded start without a terminal
# completion is intentionally non-retryable.

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
if (!grepl("^[A-Za-z0-9][A-Za-z0-9._-]{0,191}$", wave_id)) {
  stop("The requested wave ID is not path safe.", call. = FALSE)
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

required_environment <- c(
  expected_commit = "RQR_EXPECTED_PRIMARY_COMMIT",
  authorization = "RQR_CONFIRMATORY_AUTHORIZATION_BUNDLE",
  primary_attestation = "RQR_PRIMARY_RUNTIME_ATTESTATION",
  canonical_task_plan = "RQR_CONFIRMATORY_CANONICAL_TASK_PLAN",
  seed_ledger = "RQR_CONFIRM_SEED_LEDGER",
  state_root = "RQR_CONFIRMATORY_WAVE_STATE_ROOT",
  wave_output_base = "RQR_CONFIRMATORY_WAVE_OUTPUT_BASE",
  run_id = "RQR_CONFIRMATORY_RUN_ID"
)
environment_values <- vapply(
  required_environment, Sys.getenv, character(1L), unset = ""
)
if (any(!nzchar(environment_values))) {
  stop(
    paste(
      "The wave launcher requires the commit, authorization, runtime,",
      "canonical-plan, seed-ledger, state-root, canonical wave-output,",
      "and run-ID environment."
    ),
    call. = FALSE
  )
}
expected_commit <- tolower(environment_values[["expected_commit"]])
if (!grepl("^[0-9a-f]{40}$", expected_commit)) {
  stop("RQR_EXPECTED_PRIMARY_COMMIT must be a complete SHA.",
       call. = FALSE)
}
required_file_fields <- c(
  "authorization", "primary_attestation",
  "canonical_task_plan", "seed_ledger"
)
required_files <- normalizePath(
  environment_values[required_file_fields],
  winslash = "/", mustWork = TRUE
)
names(required_files) <- required_file_fields
state_root <- normalizePath(
  environment_values[["state_root"]], winslash = "/", mustWork = FALSE
)
wave_output_base <- normalizePath(
  environment_values[["wave_output_base"]],
  winslash = "/", mustWork = TRUE
)

git_output <- function(arguments) {
  output <- system2(
    "git", c("-C", shQuote(repo_root), arguments),
    stdout = TRUE, stderr = TRUE,
    env = c("GIT_OPTIONAL_LOCKS=0", "GIT_TERMINAL_PROMPT=0")
  )
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  if (!identical(as.integer(status), 0L)) {
    stop("A required read-only Git query failed.", call. = FALSE)
  }
  output
}
source_commit <- tolower(trimws(git_output(c("rev-parse", "HEAD"))[[1L]]))
git_status <- git_output(c(
  "status", "--porcelain=v2", "--untracked-files=all"
))
if (!identical(source_commit, expected_commit) || length(git_status)) {
  stop("The wave launcher requires the exact clean authorization commit.",
       call. = FALSE)
}

authorization <- jsonlite::read_json(
  required_files[["authorization"]], simplifyVector = TRUE
)
primary_attestation <- readRDS(
  required_files[["primary_attestation"]]
)
required_authorization <- c(
  "schema_version", "reviewed_implementation_commit",
  "authorization_commit", "authorization_diff_only_flag",
  "explicit_user_confirmation", "all_reference_gates_pass",
  "primary_worktree_clean", "primary_runtime_tree_digest",
  "seed_ledger_sha256", "task_plan_sha256"
)
if (!all(required_authorization %in% names(authorization)) ||
    !identical(
      authorization$schema_version,
      "rqrgibbs_dlm_confirmatory_authorization/1.0.0"
    ) ||
    !identical(
      tolower(as.character(authorization$authorization_commit)),
      expected_commit
    ) ||
    !isTRUE(authorization$authorization_diff_only_flag) ||
    !isTRUE(authorization$explicit_user_confirmation) ||
    !isTRUE(authorization$all_reference_gates_pass) ||
    !isTRUE(authorization$primary_worktree_clean) ||
    !identical(
      as.character(primary_attestation$schema_version),
      "rqrgibbs_runtime_attestation/5.0.0"
    ) ||
    !identical(
      tolower(as.character(primary_attestation$source_commit)),
      expected_commit
    ) ||
    !identical(
      tolower(as.character(
        primary_attestation$runtime_package_tree_digest
      )),
      tolower(as.character(authorization$primary_runtime_tree_digest))
    )) {
  stop("The launcher authorization or isolated runtime binding is invalid.",
       call. = FALSE)
}
primary_runtime_path <- normalizePath(
  primary_attestation$runtime_package_path,
  winslash = "/", mustWork = TRUE
)
if (!requireNamespace("rqrgibbs", quietly = TRUE) ||
    !identical(
      normalizePath(
        find.package("rqrgibbs"), winslash = "/", mustWork = TRUE
      ),
      primary_runtime_path
    )) {
  stop("The wave launcher did not load the attested primary runtime.",
       call. = FALSE)
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
canonical_tasks <- rqr_confirm_replication_plan(
  contract, planning = "maximum"
)
supplied_tasks <- utils::read.csv(
  required_files[["canonical_task_plan"]],
  stringsAsFactors = FALSE, check.names = FALSE
)
rownames(canonical_tasks) <- rownames(supplied_tasks) <- NULL
if (!identical(supplied_tasks, canonical_tasks)) {
  stop("The authorization-bound canonical task plan changed.",
       call. = FALSE)
}
rqr_confirm_validate_seed_ledger(
  utils::read.csv(
    required_files[["seed_ledger"]],
    stringsAsFactors = FALSE, check.names = FALSE
  ),
  contract, planning = "maximum", require_complete = TRUE
)

config_path <- file.path(
  repo_root, "application", "config", "rqr_dlm",
  "rqr_dlm_main_simulation_20260724.R"
)
catalog <- rqr_confirm_wave_catalog(contract, planning = "maximum")
binding <- rqr_confirm_wave_binding(
  run_id = environment_values[["run_id"]],
  expected_commit = expected_commit,
  authorization = authorization,
  config_sha256 = rqr_confirm_sha256(config_path),
  incidence_sha256 = rqr_confirm_sha256(
    contract$paths$incidence_path
  ),
  seed_ledger_sha256 = rqr_confirm_sha256(
    required_files[["seed_ledger"]]
  ),
  task_plan_sha256 = rqr_confirm_sha256(
    required_files[["canonical_task_plan"]]
  ),
  wave_plan_sha256 = rqr_confirm_sha256(wave_plan_path),
  wave_output_base = wave_output_base
)

if (!dir.exists(state_root)) {
  dir.create(state_root, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(state_root, "starts"))
  dir.create(file.path(state_root, "completions"))
  run_contract <- c(
    binding,
    list(
      canonical_wave_count = nrow(catalog),
      created_at_utc = format(
        Sys.time(), tz = "UTC", usetz = TRUE
      )
    )
  )
  rqr_confirm_atomic_write_json(
    run_contract, file.path(state_root, "run_contract.json")
  )
}
records <- rqr_confirm_wave_state_records(
  state_root, catalog, binding
)
next_index <- if (is.null(records$completions)) {
  1L
} else {
  nrow(records$completions) + 1L
}
if (next_index > nrow(catalog)) {
  stop("Every canonical confirmatory wave is already terminal.",
       call. = FALSE)
}
current <- catalog[next_index, , drop = FALSE]
prior_decision <- NULL
if (!is.na(current$prior_batch_target)) {
  prior_path <- Sys.getenv(
    "RQR_CONFIRMATORY_PRIOR_BATCH_DECISION", unset = ""
  )
  if (!nzchar(prior_path)) {
    stop("The next wave requires a verified prior batch decision.",
         call. = FALSE)
  }
  prior_decision <- rqr_confirm_read_prior_batch_decision(
    prior_path, current, binding, records$completion_values
  )
}
transition <- rqr_confirm_wave_state_transition(
  catalog, records$completions, wave_id, binding$binding_digest,
  prior_batch_decision = prior_decision
)
if (!identical(as.character(transition$current$mode), mode)) {
  stop("The requested runner mode differs from the next canonical wave.",
       call. = FALSE)
}
output_root <- rqr_confirm_require_wave_output_root(
  output_root, binding, current
)
if (file.exists(output_root) || dir.exists(output_root)) {
  stop("The wave output root must be fresh.", call. = FALSE)
}

selected <- supplied_plan[
  supplied_plan$wave_id == wave_id, , drop = FALSE
]
if (!nrow(selected) ||
    length(unique(selected$mode)) != 1L ||
    !identical(unique(selected$mode), mode)) {
  stop("The requested wave ID and runner mode are inconsistent.",
       call. = FALSE)
}
task_fields <- names(canonical_tasks)
wave_tasks <- selected[task_fields]
rqr_confirm_validate_task_subset(wave_tasks, contract)
temporary_task_plan <- tempfile("wave-task-plan-", tmpdir = state_root)
on.exit(unlink(temporary_task_plan, force = TRUE), add = TRUE)
utils::write.csv(
  wave_tasks, temporary_task_plan,
  row.names = FALSE, quote = TRUE
)
wave_task_plan_sha256 <- rqr_confirm_sha256(temporary_task_plan)
timestamp_utc <- function() {
  format(Sys.time(), tz = "UTC", usetz = TRUE)
}
state_filename <- sprintf("%04d__%s.json", next_index, wave_id)
start_path <- file.path(state_root, "starts", state_filename)
completion_path <- file.path(
  state_root, "completions", state_filename
)
start_record <- list(
  schema_version = "rqrgibbs_dlm_wave_start/1.0.0",
  canonical_wave_index = next_index,
  wave_id = wave_id, mode = mode,
  phase = as.character(current$phase),
  batch_group = as.character(current$batch_group),
  batch_target = as.integer(current$batch_target),
  binding_digest = binding$binding_digest,
  action = transition$action,
  required_predecessor_wave_ids =
    as.character(transition$required_predecessor_wave_ids),
  predecessor_completion_sha256 =
    as.character(transition$predecessor_completion_sha256),
  predecessor_artifact_manifest_sha256 =
    as.character(
      transition$predecessor_artifact_manifest_sha256
    ),
  same_batch_sentinel_pass =
    transition$same_batch_sentinel_pass,
  prior_batch_decision_sha256 =
    transition$prior_batch_decision_sha256,
  prior_batch_next_action =
    transition$prior_batch_next_action,
  worker_limit = as.integer(current$worker_limit),
  task_count = as.integer(current$task_count),
  wave_task_plan_sha256 = wave_task_plan_sha256,
  output_root = output_root,
  started_at_utc = timestamp_utc()
)
rqr_confirm_atomic_write_json(start_record, start_path)

write_completion <- function(
    decision, all_workers_passed, workers_used,
    wave_artifact_hashes_sha256 = "") {
  completion <- list(
    schema_version = "rqrgibbs_dlm_wave_completion/1.0.0",
    canonical_wave_index = next_index,
    wave_id = wave_id, mode = mode,
    phase = as.character(current$phase),
    batch_group = as.character(current$batch_group),
    batch_target = as.integer(current$batch_target),
    binding_digest = binding$binding_digest,
    action = transition$action,
    decision = decision,
    start_sha256 = rqr_confirm_sha256(start_path),
    required_predecessor_wave_ids =
      as.character(transition$required_predecessor_wave_ids),
    predecessor_completion_sha256 =
      as.character(transition$predecessor_completion_sha256),
    predecessor_artifact_manifest_sha256 =
      as.character(
        transition$predecessor_artifact_manifest_sha256
      ),
    same_batch_sentinel_pass =
      transition$same_batch_sentinel_pass,
    prior_batch_decision_sha256 =
      transition$prior_batch_decision_sha256,
    prior_batch_next_action =
      transition$prior_batch_next_action,
    worker_limit = as.integer(current$worker_limit),
    workers_used = as.integer(workers_used),
    task_count = as.integer(current$task_count),
    wave_task_plan_sha256 = wave_task_plan_sha256,
    output_root = output_root,
    wave_artifact_hashes_sha256 =
      wave_artifact_hashes_sha256,
    all_workers_passed = isTRUE(all_workers_passed),
    completed_at_utc = timestamp_utc()
  )
  rqr_confirm_atomic_write_json(completion, completion_path)
}

if (identical(transition$action, "skip")) {
  write_completion(
    "skipped_precision_stop", all_workers_passed = FALSE,
    workers_used = 0L
  )
  cat("Confirmatory wave recorded as a precision-stop skip.\n")
  cat("  wave:", wave_id, "\n")
  cat("  state:", state_root, "\n")
  quit(save = "no", status = 0L, runLast = FALSE)
}

dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
rqr_confirm_atomic_write_csv(
  wave_tasks, file.path(output_root, "wave_task_plan.csv")
)
if (!identical(
    rqr_confirm_sha256(file.path(output_root, "wave_task_plan.csv")),
    wave_task_plan_sha256
  )) {
  stop("The published wave task plan differs from its start record.",
       call. = FALSE)
}
rqr_confirm_atomic_write_csv(
  selected, file.path(output_root, "wave_assignment.csv")
)

wrapper <- file.path(
  repo_root, "application", "scripts",
  "15_run_rqr_dlm_confirmatory_simulation.sh"
)
if (!file.exists(wrapper)) {
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
  assigned <- selected[
    selected$worker_slot == worker_slot, , drop = FALSE
  ]
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
    "bash", c(wrapper, mode, worker_output),
    stdout = stdout_path, stderr = stderr_path,
    env = c(
      sprintf("RQR_CONFIRM_TASK_FILE=%s", shard_path),
      sprintf(
        "RQR_CONFIRMATORY_MONITOR_ROOT=%s",
        worker_monitor_root
      ),
      sprintf(
        "RQR_MAX_PROCESS_GROUP_THREADS=%d",
        contract$config$resources$
          sampled_process_group_thread_ceiling
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
  if (length(summary_paths) != 1L ||
      length(telemetry_paths) != 1L) {
    stop(
      "A worker did not publish exactly one resource evidence pair.",
      call. = FALSE
    )
  }
  data.frame(
    wave_id = wave_id, mode = mode,
    worker_slot = worker_slot,
    task_count = nrow(shard),
    exit_status = as.integer(status),
    output_directory = worker_output,
    task_file_sha256 = rqr_confirm_sha256(shard_path),
    stdout_sha256 = rqr_confirm_sha256(stdout_path),
    stderr_sha256 = rqr_confirm_sha256(stderr_path),
    resource_summary_sha256 =
      rqr_confirm_sha256(summary_paths[[1L]]),
    resource_telemetry_sha256 =
      rqr_confirm_sha256(telemetry_paths[[1L]]),
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
        messages, digest::digest, character(1L),
        algo = "sha256", serialize = FALSE
      )
    ),
    file.path(output_root, "wave_launcher_failure.json")
  )
  worker_results <- worker_results[!worker_error]
}
worker_results <- if (length(worker_results)) {
  do.call(rbind, worker_results)
} else {
  data.frame()
}
if (nrow(worker_results)) {
  rqr_confirm_atomic_write_csv(
    worker_results, file.path(output_root, "wave_worker_status.csv")
  )
}
all_workers_passed <- !any(worker_error) &&
  nrow(worker_results) == length(worker_slots) &&
  all(worker_results$exit_status == 0L)
rqr_confirm_atomic_write_json(
  list(
    schema_version = "rqrgibbs_dlm_wave/2.0.0",
    canonical_wave_index = next_index,
    wave_id = wave_id, mode = mode,
    phase = as.character(current$phase),
    batch_group = as.character(current$batch_group),
    batch_target = as.integer(current$batch_target),
    binding_digest = binding$binding_digest,
    start_sha256 = rqr_confirm_sha256(start_path),
    same_batch_sentinel_pass =
      transition$same_batch_sentinel_pass,
    prior_batch_decision_sha256 =
      transition$prior_batch_decision_sha256,
    worker_limit = unique(selected$worker_limit),
    workers_used = length(worker_slots),
    task_count = nrow(selected),
    all_workers_passed = all_workers_passed,
    no_retry = TRUE, no_reseed = TRUE,
    source_commit = source_commit,
    runtime_tree_digest = binding$runtime_tree_digest
  ),
  file.path(output_root, "wave_manifest.json")
)
hashes <- rqr_confirm_recursive_manifest(output_root)
rqr_confirm_atomic_write_csv(
  hashes, file.path(output_root, "wave_artifact_hashes.csv")
)
wave_hash <- rqr_confirm_sha256(
  file.path(output_root, "wave_artifact_hashes.csv")
)
write_completion(
  if (all_workers_passed) "passed" else "failed",
  all_workers_passed = all_workers_passed,
  workers_used = length(worker_slots),
  wave_artifact_hashes_sha256 = wave_hash
)
if (!all_workers_passed) {
  stop("A confirmatory worker failed; no later wave is authorized.",
       call. = FALSE)
}
cat("Confirmatory wave completed without a worker failure.\n")
cat("  wave:", wave_id, "\n")
cat("  mode:", mode, "\n")
cat("  workers:", length(worker_slots), "\n")
cat("  output:", output_root, "\n")
cat("  state:", state_root, "\n")
