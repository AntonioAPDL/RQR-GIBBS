#!/usr/bin/env Rscript

# Materialize the compact authorization bundle after an independently reviewed
# implementation receives its one-line false-to-true authorization commit.
# This script performs no fitting and refuses any source change beyond that
# exact flag flip.

arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) != 8L) {
  stop(
    paste(
      "Usage: 19_prepare_rqr_dlm_confirmatory_authorization.R",
      "<reviewed-implementation-SHA> <authorization-SHA>",
      "<primary-runtime-attestation.json> <preflight-directory>",
      "<reference-directory> <exdqlm-attestation.json>",
      "<quantreg-attestation.json> <fresh-authorization.json>"
    ),
    call. = FALSE
  )
}
reviewed_commit <- tolower(arguments[[1L]])
authorization_commit <- tolower(arguments[[2L]])
if (any(!grepl(
    "^[0-9a-f]{40}$", c(reviewed_commit, authorization_commit)
  ))) {
  stop("Both source identities must be complete Git SHAs.",
       call. = FALSE)
}
confirmation <- Sys.getenv(
  "RQR_EXPLICIT_USER_CONFIRMATION", unset = ""
)
if (!identical(confirmation, "CONFIRM-RQR-DLM-MAIN-RUN")) {
  stop("The exact explicit-confirmation token is absent.",
       call. = FALSE)
}

repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
paths <- normalizePath(
  arguments[3:7], winslash = "/", mustWork = TRUE
)
names(paths) <- c(
  "primary", "preflight", "reference", "exdqlm", "quantreg"
)
output <- normalizePath(
  arguments[[8L]], winslash = "/", mustWork = FALSE
)
if (file.exists(output) || dir.exists(output)) {
  stop("The authorization output path must be fresh.", call. = FALSE)
}
sys.source(
  file.path(
    repo_root, "application", "scripts", "lib",
    "rqr_dlm_confirmatory_simulation.R"
  ),
  envir = environment()
)
sys.source(
  file.path(
    repo_root, "application", "scripts", "lib",
    "isolated_runtime_lineage.R"
  ),
  envir = environment()
)
sys.source(
  file.path(
    repo_root, "application", "scripts", "lib",
    "rqr_dlm_main_simulation.R"
  ),
  envir = environment()
)
contract <- rqr_confirm_read_contract(repo_root)
rqr_confirm_validate_contract(contract)
if (!isTRUE(contract$config$confirmatory_execution_authorized) ||
    isTRUE(contract$config$diagnostic_pilot_execution_authorized)) {
  stop("The configuration is not the one-line main-run authorization.",
       call. = FALSE)
}
head_commit <- tolower(trimws(system2(
  "git", c("-C", shQuote(repo_root), "rev-parse", "HEAD"),
  stdout = TRUE,
  env = c("GIT_OPTIONAL_LOCKS=0", "GIT_TERMINAL_PROMPT=0")
))[[1L]])
git_status <- system2(
  "git",
  c(
    "-C", shQuote(repo_root), "status", "--porcelain=v2",
    "--untracked-files=all"
  ),
  stdout = TRUE, stderr = TRUE,
  env = c("GIT_OPTIONAL_LOCKS=0", "GIT_TERMINAL_PROMPT=0")
)
status_code <- attr(git_status, "status")
if (is.null(status_code)) status_code <- 0L
flag_only <- rqr_confirm_flag_only_authorization_diff(
  repo_root, reviewed_commit, authorization_commit
)
if (!identical(head_commit, authorization_commit) ||
    !identical(as.integer(status_code), 0L) ||
    length(git_status) || !isTRUE(flag_only)) {
  stop("The authorization checkout is not exact, clean, and flag-only.",
       call. = FALSE)
}

if (!requireNamespace("rqrgibbs", quietly = TRUE)) {
  stop("The isolated authorization runtime is not on the library path.",
       call. = FALSE)
}
primary_binding <- rqr_main_primary_runtime_binding(
  repo_root, authorization_commit, paths[["primary"]]
)
if (!isTRUE(primary_binding$match)) {
  stop("The isolated primary runtime does not match authorization.",
       call. = FALSE)
}
preflight_hashes <- rqr_confirm_verify_recursive_manifest(
  paths[["preflight"]]
)
reference_hashes <- rqr_confirm_verify_recursive_manifest(
  paths[["reference"]]
)
preflight_gates <- utils::read.csv(
  file.path(paths[["preflight"]], "preflight_gates.csv"),
  stringsAsFactors = FALSE, check.names = FALSE
)
reference_gates <- utils::read.csv(
  file.path(paths[["reference"]], "reference_gates.csv"),
  stringsAsFactors = FALSE, check.names = FALSE
)
if (!identical(names(preflight_gates), c("gate", "value")) ||
    !identical(names(reference_gates), c("gate", "value")) ||
    !all(as.logical(preflight_gates$value)) ||
    !all(as.logical(reference_gates$value))) {
  stop("Preflight or reference gates did not all pass.",
       call. = FALSE)
}
for (directory in paths[c("preflight", "reference")]) {
  manifest <- jsonlite::read_json(
    file.path(directory, "run_manifest.json"),
    simplifyVector = TRUE
  )
  if (!identical(as.character(manifest$source_commit),
                 authorization_commit) ||
      !identical(
        as.character(
          manifest$primary_runtime_binding$runtime_tree_digest
        ),
        primary_binding$runtime_tree_digest
      ) ||
      !identical(as.character(manifest$status), "passed")) {
    stop("A gate bundle has the wrong source, runtime, or status.",
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
reference_runtime_match <-
  identical(
    runtime_bundle$primary$runtime_tree_digest,
    primary_binding$runtime_tree_digest
  ) &&
  identical(
    runtime_bundle$exdqlm$source_package_sha256,
    exdqlm$source_package_sha256
  ) &&
  identical(
    runtime_bundle$exdqlm$runtime_tree_digest,
    exdqlm$runtime_tree_digest
  ) &&
  identical(
    runtime_bundle$quantreg$source_package_sha256,
    quantreg$source_package_sha256
  ) &&
  identical(
    runtime_bundle$quantreg$runtime_tree_digest,
    quantreg$runtime_tree_digest
  ) &&
  !isTRUE(runtime_bundle$exdqlm$protected_checkout_used) &&
  !isTRUE(runtime_bundle$quantreg$protected_checkout_used)
if (!reference_runtime_match ||
    isTRUE(exdqlm$protected_exdqlm_checkout_used) ||
    isTRUE(quantreg$protected_exdqlm_checkout_used)) {
  stop("The comparator or reference runtime bundle is ineligible.",
       call. = FALSE)
}

seed_path <- file.path(
  paths[["preflight"]], "seed_ledger_maximum.csv"
)
task_path <- file.path(
  paths[["preflight"]], "replication_plan_maximum.csv"
)
wave_path <- file.path(
  paths[["preflight"]], "execution_wave_plan_maximum.csv"
)
for (path in c(seed_path, task_path, wave_path)) {
  if (!file.exists(path)) {
    stop("A canonical preflight launch artifact is missing.",
         call. = FALSE)
  }
}
authorization <- list(
  schema_version =
    "rqrgibbs_dlm_confirmatory_authorization/1.0.0",
  reviewed_implementation_commit = reviewed_commit,
  authorization_commit = authorization_commit,
  authorization_diff_only_flag = TRUE,
  explicit_user_confirmation = TRUE,
  all_reference_gates_pass = TRUE,
  primary_worktree_clean = TRUE,
  primary_runtime_tree_digest =
    primary_binding$runtime_tree_digest,
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
  protected_checkout_used = FALSE
)
rqr_confirm_atomic_write_json(authorization, output)
rqr_confirm_atomic_write_json(
  list(
    schema_version =
      "rqrgibbs_dlm_confirmatory_launch_inputs/1.0.0",
    reviewed_implementation_commit = reviewed_commit,
    authorization_commit = authorization_commit,
    primary_runtime_attestation = paths[["primary"]],
    preflight_directory = paths[["preflight"]],
    reference_directory = paths[["reference"]],
    exdqlm_attestation = paths[["exdqlm"]],
    quantreg_attestation = paths[["quantreg"]],
    authorization_bundle = normalizePath(output, winslash = "/"),
    seed_ledger = seed_path,
    canonical_task_plan = task_path,
    canonical_wave_plan = wave_path,
    preflight_manifest_rows = nrow(preflight_hashes),
    reference_manifest_rows = nrow(reference_hashes)
  ),
  paste0(output, ".launch-inputs.json")
)
cat("Confirmatory authorization bundle finalized.\n")
cat("  bundle:", output, "\n")
cat("  launch inputs:", paste0(output, ".launch-inputs.json"), "\n")
