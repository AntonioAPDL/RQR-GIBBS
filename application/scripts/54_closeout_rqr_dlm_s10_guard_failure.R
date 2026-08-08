#!/usr/bin/env Rscript

# Authenticate and compact the failed higher-dimensional S10 transition guard.
# Heavy per-chain objects remain in ignored storage and are represented only by
# their recorded byte counts and SHA-256 digests.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L || !dir.exists(args[[1L]]) ||
    file.exists(args[[2L]]) || dir.exists(args[[2L]])) {
  stop(
    paste(
      "Usage: 54_closeout_rqr_dlm_s10_guard_failure.R",
      "<s10-guard-root> <fresh-compact-output>"
    ),
    call. = FALSE
  )
}
input_root <- normalizePath(args[[1L]], winslash = "/", mustWork = TRUE)
output_root <- normalizePath(args[[2L]], winslash = "/", mustWork = FALSE)

atomic_csv <- function(value, path) {
  temporary <- tempfile(paste0(".", basename(path), "-"), dirname(path))
  utils::write.csv(value, temporary, row.names = FALSE, na = "")
  if (!file.rename(temporary, path)) {
    stop("Could not atomically publish a compact CSV.", call. = FALSE)
  }
}
atomic_json <- function(value, path) {
  temporary <- tempfile(paste0(".", basename(path), "-"), dirname(path))
  jsonlite::write_json(
    value, temporary, pretty = TRUE, auto_unbox = TRUE, na = "null"
  )
  if (!file.rename(temporary, path)) {
    stop("Could not atomically publish compact JSON.", call. = FALSE)
  }
}
sha256 <- function(path) {
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

required <- c(
  "artifact_hashes.csv", "guard_manifest.json", "job_status.csv",
  "fit_diagnostics.csv", "cases.csv", "jobs.csv"
)
if (any(!file.exists(file.path(input_root, required)))) {
  stop("The S10 guard bundle is incomplete.", call. = FALSE)
}
input_hashes <- utils::read.csv(
  file.path(input_root, "artifact_hashes.csv"),
  stringsAsFactors = FALSE, check.names = FALSE
)
if (!identical(names(input_hashes), c("path", "bytes", "sha256")) ||
    anyDuplicated(input_hashes$path) ||
    any(!file.exists(file.path(input_root, input_hashes$path)))) {
  stop("The S10 artifact manifest is malformed.", call. = FALSE)
}
actual_hash <- vapply(
  input_hashes$path,
  function(path) sha256(file.path(input_root, path)),
  character(1L)
)
actual_bytes <- as.numeric(file.info(
  file.path(input_root, input_hashes$path)
)$size)
verification <- transform(
  input_hashes,
  observed_bytes = actual_bytes,
  observed_sha256 = actual_hash,
  bytes_match = as.numeric(bytes) == actual_bytes,
  hash_match = sha256 == actual_hash
)
if (!all(verification$bytes_match) || !all(verification$hash_match)) {
  stop("An S10 guard artifact failed authentication.", call. = FALSE)
}

manifest <- jsonlite::read_json(
  file.path(input_root, "guard_manifest.json"), simplifyVector = TRUE
)
status <- utils::read.csv(
  file.path(input_root, "job_status.csv"),
  stringsAsFactors = FALSE, check.names = FALSE
)
diagnostics <- utils::read.csv(
  file.path(input_root, "fit_diagnostics.csv"),
  stringsAsFactors = FALSE, check.names = FALSE
)
expected_commit <- "73f9918deb91539f06ced88c7803877a3065f42f"
if (!identical(manifest$schema_version,
               "rqrgibbs_dlm_s10_transition_guard/1.0.0") ||
    !identical(tolower(manifest$source_commit), expected_commit) ||
    nrow(status) != 8L || !all(status$ok) ||
    !all(status$exact_joint_target) ||
    any(status$numerical_repair_count != 0L) ||
    nrow(diagnostics) != 95L || sum(diagnostics$pass) != 79L ||
    isTRUE(manifest$selected_policies_passed) ||
    !isTRUE(manifest$all_workers_within_memory_ceiling) ||
    manifest$retries != 0L || isTRUE(manifest$reseeding)) {
  stop("The observed failed-guard contract was not reproduced.",
       call. = FALSE)
}

failed <- diagnostics[!diagnostics$pass, , drop = FALSE]
diagnostic_summary <- do.call(rbind, lapply(
  split(diagnostics, diagnostics$method),
  function(value) data.frame(
    method = value$method[[1L]], diagnostics = nrow(value),
    diagnostics_passed = sum(value$pass),
    diagnostics_failed = sum(!value$pass),
    max_rhat = max(value$rhat, na.rm = TRUE),
    min_bulk_ess = min(value$ess_bulk, na.rm = TRUE),
    min_tail_ess = min(value$ess_tail, na.rm = TRUE),
    max_mcse_over_sd = max(value$mcse_over_sd, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
))
rownames(diagnostic_summary) <- NULL

result_files <- sort(list.files(
  file.path(input_root, "job_results"), pattern = "\\.rds$",
  full.names = TRUE
))
if (length(result_files) != 8L) {
  stop("Exactly eight authenticated guard job objects are required.",
       call. = FALSE)
}
objects <- lapply(result_files, readRDS)
m11 <- objects[vapply(
  objects, function(value) identical(value$job$method[[1L]], "M11"),
  logical(1L)
)]
if (length(m11) != 4L ||
    any(vapply(m11, function(value) !is.matrix(value$scalars), logical(1L)))) {
  stop("The four M11 scalar-chain objects are unavailable.", call. = FALSE)
}
scalar_chains <- lapply(m11, `[[`, "scalars")
core <- intersect(
  c(
    "log_q_1", "log_q_2", "log_lambda", "mean_lower",
    "mean_upper", "mean_midpoint", "mean_width", "observed_loss",
    "future_h20_upper", "future_h20_midpoint", "future_h20_width"
  ),
  colnames(scalar_chains[[1L]])
)
chain_location <- do.call(rbind, lapply(seq_along(scalar_chains), function(i) {
  chain <- scalar_chains[[i]]
  do.call(rbind, lapply(core, function(estimand) {
    value <- chain[, estimand]
    midpoint <- length(value) %/% 2L
    data.frame(
      method = "M11", DGP = "S10", replication = 166L,
      chain = i, profile = c("A", "B", "C", "D")[[i]],
      estimand = estimand, mean = mean(value), sd = stats::sd(value),
      q05 = unname(stats::quantile(value, 0.05)),
      median = stats::median(value),
      q95 = unname(stats::quantile(value, 0.95)),
      first_half_mean = mean(value[seq_len(midpoint)]),
      second_half_mean = mean(value[(midpoint + 1L):length(value)]),
      stringsAsFactors = FALSE
    )
  }))
}))

late_windows <- c(9000L, 6750L, 4500L, 2250L, 1000L)
late_window_diagnostics <- do.call(rbind, lapply(late_windows, function(n) {
  do.call(rbind, lapply(core, function(estimand) {
    values <- do.call(cbind, lapply(
      scalar_chains,
      function(chain) utils::tail(chain[, estimand], n)
    ))
    draws <- array(
      values, dim = c(nrow(values), ncol(values), 1L),
      dimnames = list(NULL, NULL, estimand)
    )
    data.frame(
      retained_window = n, estimand = estimand,
      rhat = posterior::rhat(draws),
      ess_bulk = posterior::ess_bulk(draws),
      ess_tail = posterior::ess_tail(draws),
      stringsAsFactors = FALSE
    )
  }))
}))

dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
atomic_csv(status, file.path(output_root, "job_status.csv"))
atomic_csv(failed, file.path(output_root, "failed_diagnostics.csv"))
atomic_csv(
  diagnostic_summary, file.path(output_root, "diagnostic_summary.csv")
)
atomic_csv(chain_location, file.path(output_root, "chain_location.csv"))
atomic_csv(
  late_window_diagnostics,
  file.path(output_root, "late_window_diagnostics.csv")
)
atomic_csv(
  verification, file.path(output_root, "artifact_verification.csv")
)
input_identity <- data.frame(
  artifact = c(
    "input_artifact_manifest", "guard_manifest", "fit_diagnostics",
    "job_status"
  ),
  sha256 = vapply(
    c(
      "artifact_hashes.csv", "guard_manifest.json",
      "fit_diagnostics.csv", "job_status.csv"
    ),
    function(path) sha256(file.path(input_root, path)), character(1L)
  ),
  stringsAsFactors = FALSE
)
atomic_csv(
  input_identity, file.path(output_root, "input_artifact_hashes.csv")
)
closeout <- list(
  schema_version = "rqrgibbs_dlm_s10_guard_failure_closeout/1.0.0",
  source_commit = expected_commit,
  input_artifacts = nrow(input_hashes),
  input_artifacts_verified = sum(verification$hash_match),
  input_bytes = sum(input_hashes$bytes),
  jobs = nrow(status), jobs_succeeded = sum(status$ok),
  diagnostics = nrow(diagnostics),
  diagnostics_passed = sum(diagnostics$pass),
  diagnostics_failed = sum(!diagnostics$pass),
  failed_methods = unique(failed$method),
  primary_failed_estimand = "log_q_2",
  exact_joint_target = all(status$exact_joint_target),
  numerical_repairs = sum(status$numerical_repair_count),
  maximum_worker_RSS_KiB = max(status$peak_RSS_KiB),
  memory_ceiling_KiB = manifest$per_worker_memory_ceiling_KiB,
  retries = 0L, reseeding = FALSE,
  affected_wave_started = FALSE,
  development_outputs_reusable = FALSE,
  scientific_promotion = FALSE,
  confirmatory_launch_authorized = FALSE,
  guard_decision = "failed_closed",
  completed_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
)
atomic_json(closeout, file.path(output_root, "closeout.json"))

tracked <- sort(list.files(output_root, full.names = TRUE))
hashes <- data.frame(
  path = basename(tracked),
  bytes = as.numeric(file.info(tracked)$size),
  sha256 = vapply(tracked, sha256, character(1L)),
  stringsAsFactors = FALSE
)
atomic_csv(hashes, file.path(output_root, "artifact_hashes.csv"))
cat(sprintf(
  "Closed failed S10 guard: %d/%d diagnostics passed; %d artifacts verified.\n",
  sum(diagnostics$pass), nrow(diagnostics), nrow(input_hashes)
))
