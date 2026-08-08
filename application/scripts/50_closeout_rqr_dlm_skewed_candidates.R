#!/usr/bin/env Rscript

# Read-only closeout and forensic summary for a completed skewed-wave
# transition comparison. Heavy per-chain objects remain under an ignored root;
# only compact, authenticated summaries are written to the requested output.

args <- commandArgs(trailingOnly = TRUE)
parse_arg <- function(name, default = NULL) {
  prefix <- paste0("--", name, "=")
  value <- grep(paste0("^", prefix), args, value = TRUE)
  if (!length(value)) return(default)
  sub(prefix, "", value[[length(value)]], fixed = TRUE)
}
if (any(args %in% c("-h", "--help"))) {
  cat(paste(
    "Usage: 50_closeout_rqr_dlm_skewed_candidates.R",
    "  --input-root=<completed ignored candidate root>",
    "  --output-root=<new compact closeout directory>",
    "  --expected-source-commit=<full SHA>",
    "  --expected-candidate-family=<whole_scan|joint_elliptical>",
    sep = "\n"
  ), "\n")
  quit(save = "no", status = 0L, runLast = FALSE)
}

input_root <- parse_arg("input-root", "")
output_root <- parse_arg("output-root", "")
expected_source <- tolower(parse_arg("expected-source-commit", ""))
expected_family <- parse_arg("expected-candidate-family", "")
if (!nzchar(input_root) || !dir.exists(input_root) ||
    !nzchar(output_root) || !grepl("^[0-9a-f]{40}$", expected_source) ||
    !expected_family %in% c("whole_scan", "joint_elliptical")) {
  stop(paste(
    "Existing input, new output, a full source SHA, and an allowed",
    "candidate family are required."
  ),
       call. = FALSE)
}
input_root <- normalizePath(input_root, winslash = "/", mustWork = TRUE)
output_root <- normalizePath(output_root, winslash = "/", mustWork = FALSE)
if (file.exists(output_root) || dir.exists(output_root)) {
  stop("The compact closeout output root must be new.", call. = FALSE)
}

family_contract <- switch(
  expected_family,
  whole_scan = list(
    jobs = 93L, diagnostics = 1791L, diagnostics_failed = 32L,
    selected_methods = 4L, methods = 6L, all_methods_selected = FALSE
  ),
  joint_elliptical = list(
    jobs = 44L, diagnostics = 932L, diagnostics_failed = 22L,
    selected_methods = 2L, methods = 2L, all_methods_selected = TRUE
  )
)

sha256 <- function(path) {
  value <- system2("sha256sum", shQuote(path), stdout = TRUE, stderr = TRUE)
  status <- attr(value, "status")
  if (!is.null(status) && status != 0L) {
    stop(sprintf("Could not hash %s.", path), call. = FALSE)
  }
  strsplit(value[[1L]], "[[:space:]]+")[[1L]][[1L]]
}
atomic_csv <- function(value, path) {
  temporary <- tempfile(paste0(".", basename(path), "-"), dirname(path))
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  utils::write.csv(value, temporary, row.names = FALSE, na = "")
  if (!file.rename(temporary, path)) stop("Atomic CSV write failed.", call. = FALSE)
}
atomic_json <- function(value, path) {
  temporary <- tempfile(paste0(".", basename(path), "-"), dirname(path))
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  jsonlite::write_json(value, temporary, auto_unbox = TRUE, pretty = TRUE,
                       digits = NA, null = "null")
  if (!file.rename(temporary, path)) stop("Atomic JSON write failed.", call. = FALSE)
}
atomic_lines <- function(value, path) {
  temporary <- tempfile(paste0(".", basename(path), "-"), dirname(path))
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  writeLines(value, temporary, useBytes = TRUE)
  if (!file.rename(temporary, path)) stop("Atomic text write failed.", call. = FALSE)
}

required <- c(
  "comparison_manifest.json", "candidate_artifact_hashes.csv",
  "candidate_decisions.csv", "candidate_diagnostics.csv",
  "candidate_summary.csv", "job_status.csv", "jobs.csv"
)
if (any(!file.exists(file.path(input_root, required)))) {
  stop("The completed candidate bundle is incomplete.", call. = FALSE)
}
manifest <- jsonlite::read_json(
  file.path(input_root, "comparison_manifest.json"), simplifyVector = TRUE
)
observed_family <- if (is.null(manifest$candidate_family)) {
  "whole_scan"
} else {
  manifest$candidate_family
}
if (!identical(tolower(manifest$source_commit), expected_source) ||
    !identical(observed_family, expected_family) ||
    !identical(as.integer(manifest$jobs), family_contract$jobs) ||
    !identical(as.integer(manifest$jobs_succeeded), family_contract$jobs) ||
    !isTRUE(manifest$all_exact_joint_target) ||
    !isTRUE(manifest$all_zero_repairs) ||
    !identical(isTRUE(manifest$all_methods_selected),
               family_contract$all_methods_selected) ||
    isTRUE(manifest$threshold_changes) || isTRUE(manifest$target_changes) ||
    as.integer(manifest$retries) != 0L || isTRUE(manifest$reseeding) ||
    isTRUE(manifest$scientific_metrics_used) ||
    isTRUE(manifest$scientific_promotion) ||
    isTRUE(manifest$confirmatory_launch_authorized)) {
  stop("The candidate comparison does not match the fail-closed contract.",
       call. = FALSE)
}

hashes <- utils::read.csv(
  file.path(input_root, "candidate_artifact_hashes.csv"),
  stringsAsFactors = FALSE, check.names = FALSE
)
if (!identical(names(hashes), c("path", "bytes", "sha256")) ||
    anyDuplicated(hashes$path) || any(grepl("^/|(^|/)\\.\\.(/|$)", hashes$path))) {
  stop("The candidate artifact manifest is malformed.", call. = FALSE)
}
verification <- do.call(rbind, lapply(seq_len(nrow(hashes)), function(index) {
  path <- file.path(input_root, hashes$path[[index]])
  present <- file.exists(path) && !dir.exists(path)
  observed_bytes <- if (present) as.numeric(file.info(path)$size) else NA_real_
  observed_sha <- if (present) sha256(path) else NA_character_
  data.frame(
    path = hashes$path[[index]], present = present,
    expected_bytes = as.numeric(hashes$bytes[[index]]),
    observed_bytes = observed_bytes,
    expected_sha256 = hashes$sha256[[index]],
    observed_sha256 = observed_sha,
    pass = present && identical(observed_bytes, as.numeric(hashes$bytes[[index]])) &&
      identical(observed_sha, hashes$sha256[[index]]),
    stringsAsFactors = FALSE
  )
}))
if (!all(verification$pass)) {
  stop("At least one candidate artifact failed byte/hash verification.",
       call. = FALSE)
}

diagnostics <- utils::read.csv(
  file.path(input_root, "candidate_diagnostics.csv"),
  stringsAsFactors = FALSE, check.names = FALSE
)
summary <- utils::read.csv(
  file.path(input_root, "candidate_summary.csv"),
  stringsAsFactors = FALSE, check.names = FALSE
)
decisions <- utils::read.csv(
  file.path(input_root, "candidate_decisions.csv"),
  stringsAsFactors = FALSE, check.names = FALSE
)
status <- utils::read.csv(
  file.path(input_root, "job_status.csv"),
  stringsAsFactors = FALSE, check.names = FALSE
)
jobs <- utils::read.csv(
  file.path(input_root, "jobs.csv"),
  stringsAsFactors = FALSE, check.names = FALSE
)
if (nrow(status) != family_contract$jobs ||
    nrow(jobs) != family_contract$jobs || any(!status$ok) ||
    any(!status$exact_joint_target) || any(status$numerical_repair_count != 0L) ||
    any(status$retry_count != 0L) || any(status$reseeded) ||
    nrow(diagnostics) != family_contract$diagnostics ||
    sum(!diagnostics$pass) != family_contract$diagnostics_failed ||
    nrow(decisions) != family_contract$methods ||
    sum(decisions$selected_candidate != "none") !=
      family_contract$selected_methods) {
  stop("Candidate tables disagree with the authenticated comparison.",
       call. = FALSE)
}

result_paths <- file.path(
  input_root, "job_results", paste0(jobs$job_id, ".rds")
)
results <- lapply(result_paths, readRDS)
if (any(!vapply(results, function(x) isTRUE(x$ok), logical(1L)))) {
  stop("An authenticated candidate job result is not successful.",
       call. = FALSE)
}

lags <- c(1L, 5L, 10L, 25L, 50L)
forensic_vars <- c(
  "log_q_1", "log_lambda", "mean_width", "mean_midpoint",
  "mean_upper", "observed_loss"
)
acf_rows <- list()
cor_rows <- list()
location_rows <- list()
counter_acf <- counter_cor <- counter_location <- 0L
for (index in seq_along(results)) {
  result <- results[[index]]
  job <- result$job
  if (!job$method %in% c("M10", "M11")) next
  draws <- as.matrix(result$scalars)
  variables <- intersect(forensic_vars, colnames(draws))
  for (variable in variables) {
    ac <- stats::acf(
      draws[, variable], lag.max = max(lags), plot = FALSE,
      demean = TRUE
    )$acf
    counter_acf <- counter_acf + 1L
    acf_rows[[counter_acf]] <- data.frame(
      candidate_id = job$candidate_id, method = job$method,
      DGP = job$DGP, replication = job$replication,
      case_role = job$case_role, diagnostic_role = job$diagnostic_role,
      chain = job$chain, estimand = variable, lag = lags,
      autocorrelation = as.numeric(ac[lags + 1L]),
      stringsAsFactors = FALSE
    )
    split_point <- floor(nrow(draws) / 2L)
    counter_location <- counter_location + 1L
    location_rows[[counter_location]] <- data.frame(
      candidate_id = job$candidate_id, method = job$method,
      DGP = job$DGP, replication = job$replication,
      case_role = job$case_role, diagnostic_role = job$diagnostic_role,
      chain = job$chain, estimand = variable,
      mean = mean(draws[, variable]), sd = stats::sd(draws[, variable]),
      first_half_mean = mean(draws[seq_len(split_point), variable]),
      second_half_mean = mean(draws[(split_point + 1L):nrow(draws), variable]),
      split_shift_in_sd = (
        mean(draws[(split_point + 1L):nrow(draws), variable]) -
          mean(draws[seq_len(split_point), variable])
      ) / stats::sd(draws[, variable]),
      stringsAsFactors = FALSE
    )
  }
  if ("log_q_1" %in% variables) {
    for (variable in setdiff(variables, "log_q_1")) {
      counter_cor <- counter_cor + 1L
      cor_rows[[counter_cor]] <- data.frame(
        candidate_id = job$candidate_id, method = job$method,
        DGP = job$DGP, replication = job$replication,
        case_role = job$case_role, diagnostic_role = job$diagnostic_role,
        chain = job$chain, estimand_x = "log_q_1", estimand_y = variable,
        correlation = stats::cor(draws[, "log_q_1"], draws[, variable]),
        stringsAsFactors = FALSE
      )
    }
  }
}
autocorrelation <- do.call(rbind, acf_rows)
coupling <- do.call(rbind, cor_rows)
chain_location <- do.call(rbind, location_rows)

diagnostic_key <- paste(
  diagnostics$candidate_id, diagnostics$method, diagnostics$DGP,
  diagnostics$replication, diagnostics$diagnostic_role, sep = "|"
)
status_key <- paste(
  status$candidate_id, status$method, status$DGP, status$replication,
  status$diagnostic_role, sep = "|"
)
elapsed_by_group <- tapply(status$elapsed_seconds, status_key, sum)
efficiency <- diagnostics
efficiency$elapsed_seconds_group <- as.numeric(elapsed_by_group[diagnostic_key])
efficiency$bulk_effective_draws_per_second <-
  efficiency$ess_bulk / efficiency$elapsed_seconds_group
efficiency$tail_effective_draws_per_second <-
  efficiency$ess_tail / efficiency$elapsed_seconds_group

dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
atomic_csv(verification, file.path(output_root, "artifact_verification.csv"))
atomic_csv(summary, file.path(output_root, "candidate_summary.csv"))
atomic_csv(decisions, file.path(output_root, "candidate_decisions.csv"))
atomic_csv(diagnostics[!diagnostics$pass, , drop = FALSE],
           file.path(output_root, "failed_diagnostics.csv"))
atomic_csv(autocorrelation, file.path(output_root, "autocorrelation.csv"))
atomic_csv(coupling, file.path(output_root, "coupling_correlations.csv"))
atomic_csv(chain_location, file.path(output_root, "chain_location.csv"))
atomic_csv(efficiency, file.path(output_root, "diagnostic_efficiency.csv"))
input_files <- file.path(input_root, required)
input_hashes <- data.frame(
  path = required,
  bytes = as.numeric(file.info(input_files)$size),
  sha256 = vapply(input_files, sha256, character(1L)),
  stringsAsFactors = FALSE
)
atomic_csv(input_hashes, file.path(output_root, "input_artifact_hashes.csv"))
closeout <- list(
  schema_version = "rqrgibbs_dlm_skewed_candidate_closeout/1.1.0",
  candidate_family = expected_family,
  candidate_source_commit = expected_source,
  input_artifact_manifest_sha256 = sha256(file.path(
    input_root, "candidate_artifact_hashes.csv"
  )),
  artifacts_verified = nrow(verification),
  artifacts_failed = sum(!verification$pass),
  jobs_planned = nrow(jobs), jobs_succeeded = sum(status$ok),
  diagnostics = nrow(diagnostics),
  diagnostics_passed = sum(diagnostics$pass),
  diagnostics_failed = sum(!diagnostics$pass),
  selected_methods = sum(decisions$selected_candidate != "none"),
  unresolved_methods = as.list(decisions$method[
    decisions$selected_candidate == "none"
  ]),
  all_exact_joint_target = all(status$exact_joint_target),
  all_zero_repairs = all(status$numerical_repair_count == 0L),
  all_methods_selected = family_contract$all_methods_selected,
  thresholds_changed = FALSE, target_changed = FALSE,
  scientific_promotion = FALSE,
  confirmatory_launch_authorized = FALSE,
  completed_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
)
atomic_json(closeout, file.path(output_root, "closeout.json"))
atomic_lines(c(
  "# Skewed-wave transition comparison closeout",
  "",
  sprintf("- Candidate source: `%s`.", expected_source),
  sprintf("- Jobs: %d/%d successful; all exact-target with zero repairs.",
          sum(status$ok), nrow(status)),
  sprintf("- Diagnostics: %d/%d passed; %d failed.",
          sum(diagnostics$pass), nrow(diagnostics), sum(!diagnostics$pass)),
  sprintf("- Methods selected: %d/%d; unresolved: %s.",
          sum(decisions$selected_candidate != "none"), nrow(decisions),
          if (any(decisions$selected_candidate == "none")) {
            paste(decisions$method[decisions$selected_candidate == "none"],
                  collapse = ", ")
          } else {
            "none"
          }),
  "- Thresholds, target, seeds, and replication roles were unchanged.",
  "- No fit, retry, reseeding, scientific promotion, or launch authorization",
  "  was performed by this read-only closeout.",
  "",
  if (family_contract$all_methods_selected) {
    paste(
      "The joint-elliptical development comparison selected exact,",
      "method-wide transitions for all targeted methods. This is bounded",
      "computational evidence, not scientific promotion or launch authority."
    )
  } else {
    paste(
      "The whole-scan comparison left computational mixing unresolved in",
      "M10/M11 and therefore remained fail closed."
    )
  },
  "The compact tables retain the exact failed rows, long-lag",
  "autocorrelation, scale/root correlations, split-chain location summaries,",
  "effective draws per second, and complete input/artifact hashes. Heavy",
  "per-chain objects remain ignored."
), file.path(output_root, "README.md"))

compact_files <- sort(list.files(output_root), method = "radix")
compact_paths <- file.path(output_root, compact_files)
compact_hashes <- data.frame(
  path = compact_files,
  bytes = as.numeric(file.info(compact_paths)$size),
  sha256 = vapply(compact_paths, sha256, character(1L)),
  stringsAsFactors = FALSE
)
atomic_csv(compact_hashes, file.path(output_root, "artifact_hashes.csv"))
cat(sprintf(
  paste0(
    "Candidate closeout complete: %d/%d jobs, %d/%d diagnostics, ",
    "%d/%d methods selected.\n"
  ),
  sum(status$ok), nrow(status), sum(diagnostics$pass), nrow(diagnostics),
  sum(decisions$selected_candidate != "none"), nrow(decisions)
))
