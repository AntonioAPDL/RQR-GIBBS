#!/usr/bin/env Rscript

# Bind the explicit user-authorized diagnostic-aware maximum run to one clean
# source commit, one isolated runtime, and passing non-MCMC reference gates.

arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) != 7L) {
  stop(
    paste(
      "Usage: 58_prepare_rqr_dlm_diagnostic_aware_authorization.R",
      "<source-SHA> <primary-runtime-attestation.rds>",
      "<preflight-directory> <reference-directory>",
      "<exdqlm-attestation.json> <quantreg-attestation.json>",
      "<fresh-authorization.json>"
    ),
    call. = FALSE
  )
}
source_commit <- tolower(arguments[[1L]])
if (!grepl("^[0-9a-f]{40}$", source_commit)) {
  stop("The source identity must be one complete Git SHA.", call. = FALSE)
}
if (!identical(
    Sys.getenv("RQR_EXPLICIT_USER_CONFIRMATION", unset = ""),
    "CONFIRM-RQR-DLM-DIAGNOSTIC-AWARE-MAXIMUM-RUN"
  )) {
  stop("The exact diagnostic-aware confirmation token is absent.",
       call. = FALSE)
}

repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
paths <- normalizePath(arguments[2:6], winslash = "/", mustWork = TRUE)
names(paths) <- c("primary", "preflight", "reference", "exdqlm", "quantreg")
output <- normalizePath(arguments[[7L]], winslash = "/", mustWork = FALSE)
if (file.exists(output) || dir.exists(output)) {
  stop("The authorization output path must be fresh.", call. = FALSE)
}
for (helper in c(
    "rqr_dlm_confirmatory_simulation.R",
    "isolated_runtime_lineage.R", "rqr_dlm_main_simulation.R",
    "rqr_dlm_diagnostic_aware_completion.R"
  )) {
  sys.source(
    file.path(repo_root, "application", "scripts", "lib", helper),
    envir = environment()
  )
}
contract <- rqr_confirm_read_contract(repo_root)
rqr_confirm_validate_contract(contract)
rqr_confirm_validate_budget(contract)
policy_record <- rqr_completion_read_policy(repo_root)
policy <- policy_record$policy
if (!isTRUE(policy$execution_authorized) ||
    isTRUE(contract$config$confirmatory_execution_authorized) ||
    isTRUE(contract$config$diagnostic_pilot_execution_authorized)) {
  stop("The base and diagnostic-aware execution policies conflict.",
       call. = FALSE)
}

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
head_commit <- tolower(trimws(git_output(c("rev-parse", "HEAD"))[[1L]]))
git_status <- git_output(c(
  "status", "--porcelain=v2", "--untracked-files=all"
))
if (!identical(head_commit, source_commit) || length(git_status)) {
  stop("Authorization requires the exact clean source commit.", call. = FALSE)
}
if (!requireNamespace("rqrgibbs", quietly = TRUE)) {
  stop("The isolated primary runtime is not on the library path.",
       call. = FALSE)
}
primary_binding <- rqr_main_primary_runtime_binding(
  repo_root, source_commit, paths[["primary"]]
)
if (!isTRUE(primary_binding$match)) {
  stop("The isolated primary runtime does not match source.", call. = FALSE)
}

preflight_hashes <- rqr_confirm_verify_recursive_manifest(paths[["preflight"]])
reference_hashes <- rqr_confirm_verify_recursive_manifest(paths[["reference"]])
for (stage in c("preflight", "reference")) {
  directory <- paths[[stage]]
  gates_name <- if (stage == "preflight") {
    "preflight_gates.csv"
  } else {
    "reference_gates.csv"
  }
  gates <- utils::read.csv(
    file.path(directory, gates_name), stringsAsFactors = FALSE,
    check.names = FALSE
  )
  manifest <- jsonlite::read_json(
    file.path(directory, "run_manifest.json"), simplifyVector = TRUE
  )
  if (!identical(names(gates), c("gate", "value")) ||
      !all(as.logical(gates$value)) ||
      !identical(tolower(as.character(manifest$source_commit)),
                 source_commit) ||
      !identical(as.character(manifest$status), "passed") ||
      !isTRUE(manifest$diagnostic_aware_completion) ||
      !identical(
        tolower(as.character(manifest$execution_policy_sha256)),
        rqr_confirm_sha256(policy_record$path)
      ) ||
      !identical(
        as.character(
          manifest$primary_runtime_binding$runtime_tree_digest
        ),
        primary_binding$runtime_tree_digest
      )) {
    stop("A preflight/reference bundle failed its source-policy-runtime gate.",
         call. = FALSE)
  }
}

runtime_bundle <- jsonlite::read_json(
  file.path(paths[["reference"]], "runtime_bundle.json"),
  simplifyVector = TRUE
)
exdqlm <- rqr_confirm_read_attestation(
  paths[["exdqlm"]], "exdqlm",
  contract$config$comparator$exdqlm$version,
  contract$config$comparator$exdqlm$source_sha256
)
quantreg <- rqr_confirm_read_attestation(
  paths[["quantreg"]], "quantreg",
  contract$config$comparator$quantreg$version,
  contract$config$comparator$quantreg$source_sha256
)
runtime_match <- identical(
  runtime_bundle$primary$runtime_tree_digest,
  primary_binding$runtime_tree_digest
) && identical(
  runtime_bundle$exdqlm$source_package_sha256,
  exdqlm$source_package_sha256
) && identical(
  runtime_bundle$exdqlm$runtime_tree_digest,
  exdqlm$runtime_tree_digest
) && identical(
  runtime_bundle$quantreg$source_package_sha256,
  quantreg$source_package_sha256
) && identical(
  runtime_bundle$quantreg$runtime_tree_digest,
  quantreg$runtime_tree_digest
) && !isTRUE(runtime_bundle$exdqlm$protected_checkout_used) &&
  !isTRUE(runtime_bundle$quantreg$protected_checkout_used) &&
  !isTRUE(exdqlm$protected_exdqlm_checkout_used) &&
  !isTRUE(quantreg$protected_exdqlm_checkout_used)
if (!runtime_match) {
  stop("The comparator/reference runtime bundle is ineligible.",
       call. = FALSE)
}

seed_path <- file.path(paths[["preflight"]], "seed_ledger_maximum.csv")
task_path <- file.path(paths[["preflight"]], "replication_plan_maximum.csv")
wave_path <- file.path(paths[["preflight"]], "execution_wave_plan_maximum.csv")
if (any(!file.exists(c(seed_path, task_path, wave_path)))) {
  stop("A canonical maximum-design launch artifact is missing.",
       call. = FALSE)
}
authorization <- list(
  schema_version = "rqrgibbs_dlm_diagnostic_aware_authorization/1.0.0",
  source_commit = source_commit,
  reviewed_implementation_commit = source_commit,
  authorization_commit = source_commit,
  explicit_user_confirmation = TRUE,
  all_reference_gates_pass = TRUE,
  execution_policy_sha256 = rqr_confirm_sha256(policy_record$path),
  execution_policy_id = policy$policy_id,
  diagnostic_thresholds_changed = FALSE,
  complete_maximum_design = TRUE,
  precision_stopping_disabled = TRUE,
  primary_worktree_clean = TRUE,
  primary_runtime_tree_digest = primary_binding$runtime_tree_digest,
  preflight_artifact_hashes_sha256 = rqr_confirm_sha256(
    file.path(paths[["preflight"]], "artifact_hashes.csv")
  ),
  reference_artifact_hashes_sha256 = rqr_confirm_sha256(
    file.path(paths[["reference"]], "artifact_hashes.csv")
  ),
  seed_ledger_sha256 = rqr_confirm_sha256(seed_path),
  task_plan_sha256 = rqr_confirm_sha256(task_path),
  exdqlm_source_sha256 = exdqlm$source_package_sha256,
  quantreg_source_sha256 = quantreg$source_package_sha256,
  reference_runtime_bundle_match = TRUE,
  comparator_dependency_runtime_match = TRUE,
  toolchain_match = TRUE,
  protected_checkout_used = FALSE,
  scientific_promotion = FALSE
)
rqr_completion_authorized(policy_record, source_commit, authorization)
rqr_confirm_atomic_write_json(authorization, output)
rqr_confirm_atomic_write_json(
  list(
    schema_version =
      "rqrgibbs_dlm_diagnostic_aware_launch_inputs/1.0.0",
    reviewed_implementation_commit = source_commit,
    authorization_commit = source_commit,
    primary_runtime_attestation = paths[["primary"]],
    preflight_directory = paths[["preflight"]],
    reference_directory = paths[["reference"]],
    exdqlm_attestation = paths[["exdqlm"]],
    quantreg_attestation = paths[["quantreg"]],
    authorization_bundle = normalizePath(output, winslash = "/"),
    seed_ledger = seed_path,
    canonical_task_plan = task_path,
    canonical_wave_plan = wave_path,
    execution_policy = policy_record$path,
    complete_maximum_design = TRUE,
    preflight_manifest_rows = nrow(preflight_hashes),
    reference_manifest_rows = nrow(reference_hashes)
  ),
  paste0(output, ".launch-inputs.json")
)
cat("Diagnostic-aware maximum-run authorization prepared.\n")
cat("  source:", source_commit, "\n")
cat("  authorization:", normalizePath(output), "\n")
