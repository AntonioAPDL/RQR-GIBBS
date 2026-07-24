#!/usr/bin/env Rscript

# Promote only compact, exact-runtime-bound evidence from the four authorized
# preliminary main-simulation reference stages. Full fits and heavy outputs are
# outside this contract.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L) {
  stop(
    paste(
      "Usage: 14_promote_rqr_dlm_main_simulation_references.R",
      "<source_root> <evidence_dir> <expected_primary_commit>"
    ),
    call. = FALSE
  )
}
repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
source_root <- normalizePath(args[[1L]], winslash = "/", mustWork = TRUE)
evidence_parent <- normalizePath(
  dirname(args[[2L]]), winslash = "/", mustWork = TRUE
)
evidence_basename <- basename(args[[2L]])
if (!nzchar(evidence_basename) ||
    evidence_basename %in% c(".", "..")) {
  stop("The evidence directory name is invalid.", call. = FALSE)
}
evidence_dir <- file.path(evidence_parent, evidence_basename)
expected_commit <- tolower(args[[3L]])
if (!grepl("^[0-9a-f]{40}$", expected_commit)) {
  stop("The expected primary commit must be a full SHA.", call. = FALSE)
}
expected_evidence_parent <- normalizePath(
  file.path(repo_root, "docs", "audits"),
  winslash = "/", mustWork = TRUE
)
if (!startsWith(
      paste0(evidence_dir, "/"), paste0(expected_evidence_parent, "/")
    )) {
  stop("Promoted evidence must remain under docs/audits/.", call. = FALSE)
}
if (dir.exists(evidence_dir) || file.exists(evidence_dir)) {
  stop("The evidence directory already exists.", call. = FALSE)
}
sys.source(
  file.path(
    repo_root, "application", "scripts", "lib",
    "isolated_runtime_lineage.R"
  ),
  envir = environment()
)

stages <- c(
  "preflight", "oracle-reference", "tiny-end-to-end",
  "diagnostic-pilot-preflight"
)
stage_rows <- vector("list", length(stages))
binding_keys <- NULL

verify_stage <- function(stage) {
  stage_dir <- file.path(source_root, stage)
  required <- c("artifact_hashes.csv", "run_manifest.json")
  if (!dir.exists(stage_dir) ||
      !all(file.exists(file.path(stage_dir, required)))) {
    stop(sprintf("The %s stage is incomplete.", stage), call. = FALSE)
  }
  listed <- utils::read.csv(
    file.path(stage_dir, "artifact_hashes.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  if (!identical(names(listed), c("sha256", "bytes", "path")) ||
      anyDuplicated(listed$path) ||
      any(grepl("(^|/)\\.\\.?(/|$)", listed$path)) ||
      any(grepl("^/", listed$path))) {
    stop(sprintf("The %s artifact manifest is invalid.", stage),
         call. = FALSE)
  }
  actual_paths <- list.files(
    stage_dir, recursive = TRUE, all.files = TRUE,
    include.dirs = FALSE, no.. = TRUE
  )
  actual_paths <- sort(setdiff(
    gsub("\\\\", "/", actual_paths), "artifact_hashes.csv"
  ))
  if (!identical(sort(listed$path), actual_paths)) {
    stop(sprintf("The %s artifact set differs from its manifest.", stage),
         call. = FALSE)
  }
  for (index in seq_len(nrow(listed))) {
    path <- file.path(stage_dir, listed$path[[index]])
    if (!identical(rqr_file_sha256(path), listed$sha256[[index]]) ||
        !identical(as.numeric(file.info(path)$size),
                   as.numeric(listed$bytes[[index]]))) {
      stop(sprintf("A %s artifact failed byte verification.", stage),
           call. = FALSE)
    }
  }
  manifest <- jsonlite::read_json(
    file.path(stage_dir, "run_manifest.json"), simplifyVector = TRUE
  )
  binding <- manifest$primary_runtime_binding
  if (!identical(manifest$mode, stage) ||
      !identical(manifest$status, "passed") ||
      isTRUE(manifest$diagnostic_pilot_execution_authorized) ||
      isTRUE(manifest$confirmatory_execution_authorized) ||
      !isTRUE(manifest$generalized_bayes) ||
      isTRUE(manifest$response_likelihood) ||
      isTRUE(manifest$response_prediction_contract) ||
      !isTRUE(manifest$primary_runtime_binding_verified) ||
      !is.list(binding) ||
      !isTRUE(binding$match) ||
      !identical(tolower(binding$expected_commit), expected_commit)) {
    stop(sprintf("The %s run manifest is not promotion eligible.", stage),
         call. = FALSE)
  }
  keys <- unlist(binding[c(
    "expected_commit", "application_tree", "runtime_tree_digest",
    "runtime_attestation_schema", "runtime_attestation_sha256",
    "package_version", "R_version", "platform"
  )], use.names = TRUE)
  if (is.null(binding_keys)) {
    binding_keys <<- keys
  } else if (!identical(keys, binding_keys)) {
    stop("The four stages do not use an identical primary runtime.",
         call. = FALSE)
  }

  if (identical(stage, "preflight")) {
    gates <- utils::read.csv(
      file.path(stage_dir, "preflight_gates.csv"),
      stringsAsFactors = FALSE
    )
    if (!nrow(gates) || !all(gates$pass)) {
      stop("Preflight gates did not all pass.", call. = FALSE)
    }
  } else if (identical(stage, "oracle-reference")) {
    oracle <- utils::read.csv(
      file.path(stage_dir, "oracle_references.csv"),
      stringsAsFactors = FALSE
    )
    if (nrow(oracle) != 8L ||
        !all(oracle$unique_minimizer) ||
        any(abs(oracle$global_objective_gap) > 1e-8) ||
        any(abs(oracle$coverage_residual) > 1e-8) ||
        any(abs(oracle$moment_residual) > 1e-7) ||
        any(oracle$quadrature_error_is_rigorous_bound)) {
      stop("The oracle-reference gates failed.", call. = FALSE)
    }
  } else if (identical(stage, "tiny-end-to-end")) {
    byte_check <- utils::read.csv(
      file.path(stage_dir, "byte_reproduction.csv"),
      stringsAsFactors = FALSE
    )
    tiny <- utils::read.csv(
      file.path(stage_dir, "tiny_results.csv"),
      stringsAsFactors = FALSE
    )
    if (nrow(byte_check) != 1L ||
        !isTRUE(byte_check$byte_identical[[1L]]) ||
        !identical(
          byte_check$first_sha256[[1L]],
          byte_check$repeated_sha256[[1L]]
        ) ||
        any(tiny$numerical_repairs != 0L) ||
        any(tiny$forecast_repairs != 0L)) {
      stop("The tiny end-to-end gates failed.", call. = FALSE)
    }
  } else {
    gates <- utils::read.csv(
      file.path(stage_dir, "diagnostic_pilot_preflight_gates.csv"),
      stringsAsFactors = FALSE
    )
    plan <- utils::read.csv(
      file.path(stage_dir, "diagnostic_pilot_plan.csv"),
      stringsAsFactors = FALSE
    )
    adapters <- utils::read.csv(
      file.path(stage_dir, "comparator_adapter.csv"),
      stringsAsFactors = FALSE
    )
    runtimes <- utils::read.csv(
      file.path(stage_dir, "comparator_runtime.csv"),
      stringsAsFactors = FALSE
    )
    if (!nrow(gates) || !all(gates$pass) ||
        !nrow(plan) || any(plan$execution_authorized) ||
        !setequal(adapters$package, c("exdqlm", "quantreg")) ||
        !all(adapters$adapter_pass) ||
        !setequal(runtimes$package, c("exdqlm", "quantreg")) ||
        any(runtimes$protected_checkout_used)) {
      stop("The diagnostic-pilot preflight gates failed.",
           call. = FALSE)
    }
  }
  list(stage_dir = stage_dir, manifest = manifest, artifacts = listed)
}

verified <- lapply(stages, verify_stage)
staging <- paste0(evidence_dir, ".staging")
if (dir.exists(staging)) {
  stop("The evidence staging directory already exists.", call. = FALSE)
}
dir.create(staging, recursive = TRUE, showWarnings = FALSE)
published <- FALSE
on.exit({
  if (!published && dir.exists(staging)) {
    unlink(staging, recursive = TRUE, force = TRUE)
  }
}, add = TRUE)
for (index in seq_along(stages)) {
  stage <- stages[[index]]
  destination <- file.path(staging, stage)
  dir.create(destination, recursive = TRUE, showWarnings = FALSE)
  files <- list.files(
    verified[[index]]$stage_dir, recursive = TRUE, all.files = TRUE,
    include.dirs = FALSE, no.. = TRUE, full.names = TRUE
  )
  relative <- substring(
    files, nchar(verified[[index]]$stage_dir) + 2L
  )
  for (file_index in seq_along(files)) {
    target <- file.path(destination, relative[[file_index]])
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(files[[file_index]], target, copy.mode = TRUE)) {
      stop("Could not copy a verified evidence artifact.", call. = FALSE)
    }
  }
  stage_rows[[index]] <- data.frame(
    stage = stage,
    status = "passed",
    artifact_count = length(files),
    primary_commit = expected_commit,
    primary_runtime_tree_digest =
      verified[[index]]$manifest$primary_runtime_binding$
        runtime_tree_digest,
    execution_authorized = FALSE,
    stringsAsFactors = FALSE
  )
}
summary <- do.call(rbind, stage_rows)
utils::write.csv(
  summary, file.path(staging, "evidence_summary.csv"),
  row.names = FALSE, quote = TRUE
)
all_files <- list.files(
  staging, recursive = TRUE, all.files = TRUE,
  include.dirs = FALSE, no.. = TRUE
)
all_files <- sort(gsub("\\\\", "/", all_files))
hashes <- data.frame(
  sha256 = vapply(
    file.path(staging, all_files), rqr_file_sha256, character(1L)
  ),
  bytes = as.numeric(file.info(file.path(staging, all_files))$size),
  path = all_files,
  stringsAsFactors = FALSE
)
utils::write.csv(
  hashes, file.path(staging, "artifact_hashes.csv"),
  row.names = FALSE, quote = TRUE
)
if (!file.rename(staging, evidence_dir)) {
  stop("Could not atomically publish the evidence directory.",
       call. = FALSE)
}
published <- TRUE
cat("Promoted exact-runtime preliminary main-simulation references.\n")
cat("  primary commit:", expected_commit, "\n")
cat("  evidence:", normalizePath(evidence_dir), "\n")
