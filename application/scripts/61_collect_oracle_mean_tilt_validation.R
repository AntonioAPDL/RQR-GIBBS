#!/usr/bin/env Rscript

arguments <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_path <- sub(file_arg, "", arguments[startsWith(arguments, file_arg)][1L])
if (!length(script_path) || is.na(script_path)) {
  script_path <- "application/scripts/61_collect_oracle_mean_tilt_validation.R"
}
script_dir <- dirname(normalizePath(script_path, mustWork = TRUE))
repo_root <- normalizePath(file.path(script_dir, "..", ".."), mustWork = TRUE)
setwd(repo_root)
source(file.path(script_dir, "60_oracle_mean_tilt_validation_utils.R"))

trailing <- commandArgs(trailingOnly = TRUE)
arg_value <- function(prefix, default = NULL) {
  hit <- trailing[startsWith(trailing, prefix)]
  if (length(hit)) sub(prefix, "", hit[[1L]], fixed = TRUE) else default
}
run_dir <- normalizePath(
  arg_value("--run-dir=", ""), winslash = "/", mustWork = TRUE
)
config_path <- normalizePath(
  arg_value("--config=", file.path(run_dir, "config.json")),
  winslash = "/", mustWork = TRUE
)
config <- omtv_read_config(config_path)
omtv_validate_config(config)
if (!isTRUE(config$execution_authorized) ||
    !isTRUE(config$replication_schedule_frozen)) {
  omtv_stop("Collection requires the frozen authorized run configuration.")
}
plan <- omtv_task_plan(config)
if (!requireNamespace("digest", quietly = TRUE)) {
  omtv_stop("digest is required for collection integrity checks.")
}
source_state_path <- file.path(run_dir, "source_state.json")
fit_plan_path <- file.path(run_dir, "fit_plan.csv")
rng_ledger_path <- file.path(run_dir, "rng_ledger.csv")
input_binding_path <- file.path(run_dir, "input_bundle_binding.csv")
required_contracts <- c(
  source_state_path, fit_plan_path, rng_ledger_path, input_binding_path
)
if (any(!file.exists(required_contracts))) {
  omtv_stop("The run-level source, plan, RNG, or input-binding contract is missing.")
}
source_state <- jsonlite::read_json(source_state_path, simplifyVector = TRUE)
if (!isTRUE(source_state$source_clean) ||
    !isTRUE(source_state$exact_runtime_bound) ||
    !grepl("^[0-9a-f]{40}$", source_state$source_commit) ||
    !grepl("^[0-9a-f]{64}$", source_state$config_sha256) ||
    !grepl("^[0-9a-f]{64}$", source_state$runtime_tree_digest)) {
  omtv_stop("The run source/runtime contract is not promotion eligible.")
}
recorded_plan <- utils::read.csv(fit_plan_path, stringsAsFactors = FALSE)
if (!isTRUE(all.equal(recorded_plan, plan, check.attributes = FALSE))) {
  omtv_stop("The recorded fit plan differs from the frozen config-derived plan.")
}
rng_ledger <- utils::read.csv(rng_ledger_path, stringsAsFactors = FALSE)
expected_ledger <- omtv_rng_ledger(config, max(plan$replication))
if (!isTRUE(all.equal(rng_ledger, expected_ledger, check.attributes = FALSE))) {
  omtv_stop("The recorded RNG ledger differs from the frozen config-derived ledger.")
}

atomic_csv <- function(value, path) {
  temporary <- tempfile(paste0(".", basename(path), "-"), dirname(path))
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  utils::write.csv(value, temporary, row.names = FALSE, na = "")
  if (!file.rename(temporary, path)) omtv_stop("Atomic CSV write failed: ", path)
}
atomic_json <- function(value, path) {
  temporary <- tempfile(paste0(".", basename(path), "-"), dirname(path))
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  jsonlite::write_json(
    value, temporary, pretty = TRUE, auto_unbox = TRUE, digits = NA,
    null = "null", na = "null"
  )
  if (!file.rename(temporary, path)) omtv_stop("Atomic JSON write failed: ", path)
}

task_files <- list.files(
  file.path(run_dir, "tasks"), pattern = "\\.rds$", full.names = TRUE
)
envelopes <- lapply(task_files, function(path) {
  value <- tryCatch(readRDS(path), error = function(error) error)
  if (inherits(value, "error") || !is.list(value) ||
      !identical(
        value$schema_version, "rqrgibbs_oracle_mean_tilt_task/1.0.0"
      ) || !is.list(value$result) ||
      !identical(
        value$result$schema_version,
        "rqrgibbs_oracle_mean_tilt_fit/1.0.0"
      )) {
    omtv_stop("A task artifact is malformed: ", path)
  }
  value
})
if (length(envelopes)) {
  task_ids <- vapply(envelopes, `[[`, character(1L), "task_id")
  task_keys <- vapply(envelopes, `[[`, character(1L), "task_key")
  if (anyDuplicated(task_ids) || anyDuplicated(task_keys) ||
      any(!task_ids %in% plan$task_id)) {
    omtv_stop("Completed task identities are duplicated or outside the plan.")
  }
} else {
  task_ids <- task_keys <- character(0L)
}
if (length(envelopes)) {
  envelope_contract_pass <- vapply(envelopes, function(value) {
    task <- plan[plan$task_id == value$task_id, , drop = FALSE]
    dgp_row <- rng_ledger[
      rng_ledger$scenario_id == task$scenario_id &
        rng_ledger$model_family == task$model_family &
        rng_ledger$target == "DGP" &
        rng_ledger$replication == task$replication, , drop = FALSE
    ]
    mcmc_row <- rng_ledger[
      rng_ledger$scenario_id == task$scenario_id &
        rng_ledger$model_family == task$model_family &
        rng_ledger$target == task$target &
        rng_ledger$replication == task$replication, , drop = FALSE
    ]
    nrow(task) == 1L && nrow(dgp_row) == 1L && nrow(mcmc_row) == 1L &&
      identical(value$task_key, task$task_key[[1L]]) &&
      identical(value$source_commit, source_state$source_commit) &&
      identical(value$config_sha256, source_state$config_sha256) &&
      identical(
        value$runtime_tree_digest, source_state$runtime_tree_digest
      ) &&
      identical(value$dgp_stream_digest, dgp_row$seed_digest[[1L]]) &&
      identical(value$mcmc_stream_digest, mcmc_row$seed_digest[[1L]]) &&
      identical(
        value$worker_seconds_eligible,
        value$result$elapsed_seconds <=
          config$resources$maximum_worker_seconds
      ) &&
      identical(value$result$scenario_id, task$scenario_id[[1L]]) &&
      identical(value$result$family, task$model_family[[1L]]) &&
      identical(value$result$target, task$target[[1L]])
  }, logical(1L))
  if (!all(envelope_contract_pass)) {
    omtv_stop("At least one task artifact fails its source, plan, or RNG contract.")
  }
}

wave_status_files <- list.files(
  file.path(run_dir, "waves"), pattern = "wave_status\\.csv$",
  recursive = TRUE, full.names = TRUE
)
wave_manifest_files <- list.files(
  file.path(run_dir, "waves"), pattern = "artifact_manifest\\.csv$",
  recursive = TRUE, full.names = TRUE
)
if (length(wave_status_files) != length(wave_manifest_files)) {
  omtv_stop("Every closed wave must have exactly one artifact manifest.")
}
for (manifest_path in wave_manifest_files) {
  root <- dirname(manifest_path)
  manifest <- utils::read.csv(manifest_path, stringsAsFactors = FALSE)
  if (!nrow(manifest) || anyDuplicated(manifest$path) ||
      !all(vapply(seq_len(nrow(manifest)), function(ii) {
        candidate <- file.path(root, manifest$path[[ii]])
        file.exists(candidate) && !dir.exists(candidate) &&
          unname(file.info(candidate)$size) == manifest$bytes[[ii]] &&
          identical(
            digest::digest(file = candidate, algo = "sha256"),
            manifest$sha256[[ii]]
          )
      }, logical(1L)))) {
    omtv_stop("A wave artifact manifest failed verification: ", manifest_path)
  }
}
wave_status <- if (length(wave_status_files)) {
  do.call(rbind, lapply(wave_status_files, function(path) {
    utils::read.csv(path, stringsAsFactors = FALSE)
  }))
} else data.frame()
if (nrow(wave_status) && anyDuplicated(wave_status$task_id)) {
  omtv_stop("Wave status contains duplicate task identities.")
}
if (nrow(wave_status) && any(!wave_status$task_id %in% plan$task_id)) {
  omtv_stop("Wave status contains a task outside the frozen plan.")
}
artifact_status <- if (nrow(wave_status)) {
  wave_status[wave_status$status %in% c(
    "completed", "resumed_complete", "diagnostic_failure",
    "resource_failure"
  ), , drop = FALSE]
} else data.frame()
if (!setequal(task_ids, artifact_status$task_id)) {
  omtv_stop(
    paste(
      "Task artifacts and artifact-bearing terminal status rows differ;",
      "resume or repair the interrupted wave before collection."
    )
  )
}
if (length(envelopes)) {
  envelope_pass <- vapply(envelopes, function(value) {
    isTRUE(value$result$pass) &&
      isTRUE(value$result$reproducibility_eligible) &&
      isTRUE(value$worker_seconds_eligible)
  }, logical(1L))
  names(envelope_pass) <- task_ids
  status_pass <- artifact_status$pass
  names(status_pass) <- artifact_status$task_id
  if (!identical(
    unname(envelope_pass[sort(names(envelope_pass))]),
    unname(status_pass[sort(names(status_pass))])
  )) {
    omtv_stop("Task-artifact pass states disagree with wave status.")
  }
}
terminal_failure <- if (nrow(wave_status)) {
  wave_status[!wave_status$pass, , drop = FALSE]
} else data.frame()
terminal_ids <- wave_status$task_id
unknown_terminal <- setdiff(terminal_ids, plan$task_id)
if (length(unknown_terminal)) {
  omtv_stop("A terminal record falls outside the frozen task plan.")
}

estimands <- if (length(envelopes)) {
  do.call(rbind, lapply(envelopes, function(value) {
    row <- value$result$estimands
    row$task_id <- value$task_id
    row$task_key <- value$task_key
    row$fit_pass <- isTRUE(value$result$pass)
    row$reproducibility_eligible <-
      isTRUE(value$result$reproducibility_eligible)
    row$elapsed_seconds <- value$result$elapsed_seconds
    row$numerical_repair_count <- value$result$numerical_repair_count
    row$exact_joint_target <- value$result$exact_joint_target
    row$target_numerical_eligible <-
      value$result$target_numerical_eligible
    row
  }))
} else data.frame()
diagnostics <- if (length(envelopes)) {
  do.call(rbind, lapply(envelopes, function(value) {
    out <- value$result$diagnostics
    attr(out, "draw_matrix") <- NULL
    out$task_id <- value$task_id
    out$task_key <- value$task_key
    out
  }))
} else data.frame()
endpoint_summary <- if (length(envelopes)) {
  do.call(rbind, lapply(envelopes, function(value) {
    out <- value$result$endpoint_summary
    out$task_id <- value$task_id
    out$task_key <- value$task_key
    out
  }))
} else data.frame()

status <- merge(
  plan, if (nrow(wave_status)) wave_status else data.frame(
    task_id = character(), task_key = character(), status = character(),
    pass = logical(), message = character()
  ), by = c("task_id", "task_key"), all.x = TRUE, sort = FALSE
)
status$status[is.na(status$status)] <- "not_started"
status$pass[is.na(status$pass)] <- FALSE
status$message[is.na(status$message)] <- ""
status <- status[match(plan$task_id, status$task_id), , drop = FALSE]

atomic_csv(status, file.path(run_dir, "run_status.csv"))
atomic_csv(estimands, file.path(run_dir, "replication_estimands.csv"))
atomic_csv(diagnostics, file.path(run_dir, "mcmc_diagnostics.csv"))
atomic_csv(endpoint_summary, file.path(run_dir, "endpoint_summaries.csv"))
atomic_csv(terminal_failure, file.path(run_dir, "failure_ledger.csv"))

complete <- length(terminal_ids) == nrow(plan) &&
  setequal(terminal_ids, plan$task_id)
integrity_failures <- if (nrow(terminal_failure)) {
  sum(terminal_failure$status %in% c(
    "contract_failure", "infrastructure_failure"
  ))
} else 0L
closeout <- list(
  schema_version = "rqrgibbs_oracle_mean_tilt_closeout/1.0.0",
  campaign_id = config$campaign_id,
  planned_tasks = nrow(plan), terminal_tasks = length(terminal_ids),
  completed_fit_artifacts = length(envelopes),
  structured_failures = nrow(terminal_failure),
  integrity_failures = integrity_failures,
  terminal_accounting_complete = complete,
  failed_fits_retained_in_denominator = TRUE,
  seed_selection_used = FALSE,
  failed_replication_replacement_used = FALSE,
  response_likelihood = FALSE,
  response_predictive_draws = FALSE,
  closed_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  pass = complete && integrity_failures == 0L
)
atomic_json(closeout, file.path(run_dir, "validation_closeout.json"))
task_manifest <- data.frame(
  path = substring(task_files, nchar(run_dir) + 2L),
  bytes = unname(file.info(task_files)$size),
  sha256 = vapply(
    task_files, digest::digest, character(1L), file = TRUE, algo = "sha256"
  ),
  stringsAsFactors = FALSE
)
atomic_csv(task_manifest, file.path(run_dir, "task_artifact_manifest.csv"))
collection_files <- file.path(run_dir, c(
  "config.json", "source_state.json", "input_bundle_binding.csv",
  "fit_plan.csv", "rng_ledger.csv", "run_status.csv",
  "replication_estimands.csv", "mcmc_diagnostics.csv",
  "endpoint_summaries.csv", "failure_ledger.csv",
  "validation_closeout.json", "task_artifact_manifest.csv"
))
if (any(!file.exists(collection_files))) {
  omtv_stop("A required collection artifact is missing before final hashing.")
}
collection_manifest <- data.frame(
  path = basename(collection_files),
  bytes = unname(file.info(collection_files)$size),
  sha256 = vapply(
    collection_files, digest::digest, character(1L),
    file = TRUE, algo = "sha256"
  ),
  stringsAsFactors = FALSE
)
atomic_csv(
  collection_manifest, file.path(run_dir, "collection_artifact_manifest.csv")
)
message(
  "[oracle-mean-tilt-validation] terminal tasks: ", length(terminal_ids),
  "/", nrow(plan), "; structured failures: ", nrow(terminal_failure)
)
if (!isTRUE(closeout$pass)) quit(save = "no", status = 3L)
