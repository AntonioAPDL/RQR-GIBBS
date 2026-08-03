#!/usr/bin/env Rscript

# Read-only forensic audit of the terminal local-level-skewed RQR-DLM wave.
# The script authenticates completed artifacts, regenerates only deterministic
# DGP features from the reviewed seed ledger, and writes compact audit tables.
# It does not fit a model, change a threshold, or authorize execution.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 4L) {
  stop(paste(
    "Usage: 46_audit_rqr_dlm_skewed_wave_failures.R",
    "<failed-run-root> <failed-closeout-dir>",
    "<seed-ledger-maximum.csv> <fresh-output-dir>"
  ), call. = FALSE)
}

repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(repo_root, "application", "DESCRIPTION"))) {
  stop("Run this script from the RQR-GIBBS repository root.", call. = FALSE)
}
run_root <- normalizePath(args[[1L]], winslash = "/", mustWork = TRUE)
closeout_dir <- normalizePath(args[[2L]], winslash = "/", mustWork = TRUE)
seed_ledger_path <- normalizePath(
  args[[3L]], winslash = "/", mustWork = TRUE
)
output_dir <- normalizePath(args[[4L]], winslash = "/", mustWork = FALSE)
if (file.exists(output_dir) || dir.exists(output_dir)) {
  stop("The forensic output directory must be new.", call. = FALSE)
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  pkgload::load_all(file.path(repo_root, "application"), quiet = FALSE)
})
source(file.path(
  repo_root, "application", "scripts", "lib",
  "rqr_dlm_confirmatory_simulation.R"
))

expected <- list(
  authorization_commit =
    "32f6745369b83040c0b1c4bd385c17072ee912d8",
  runtime_tree_digest =
    "e6c42d648dffc8f3c4e98a2559e59bf4374db611a9565733979bc74c8bca3499",
  seed_ledger_sha256 =
    "3dc8483f4a777ab766704b901997295bed1c89db0590429a70f3116b233e948f",
  canonical_waves = 110L,
  replication_tasks = 8400L
)

run_contract_path <- file.path(run_root, "wave_state", "run_contract.json")
run_contract <- jsonlite::read_json(
  run_contract_path, simplifyVector = TRUE
)
closeout_path <- file.path(closeout_dir, "closeout.json")
closeout <- jsonlite::read_json(closeout_path, simplifyVector = TRUE)
if (!identical(run_contract$authorization_commit,
               expected$authorization_commit) ||
    !identical(run_contract$runtime_tree_digest,
               expected$runtime_tree_digest) ||
    !identical(as.integer(run_contract$canonical_wave_count),
               expected$canonical_waves) ||
    !identical(rqr_confirm_sha256(seed_ledger_path),
               expected$seed_ledger_sha256) ||
    !isTRUE(closeout$all_artifacts_verified) ||
    isTRUE(closeout$scientific_promotion)) {
  stop("The failed-run forensic input contract did not authenticate.",
       call. = FALSE)
}
invisible(rqr_confirm_verify_recursive_manifest(closeout_dir))

wave_dirs <- sort(
  list.dirs(file.path(run_root, "waves"), recursive = FALSE),
  method = "radix"
)
if (length(wave_dirs) != 3L) {
  stop("The terminal run must contain exactly three completed wave roots.",
       call. = FALSE)
}
invisible(lapply(wave_dirs, function(path) {
  rqr_confirm_verify_recursive_manifest(
    path, manifest_name = "wave_artifact_hashes.csv"
  )
}))

read_worker_set <- function(wave, filename) {
  paths <- Sys.glob(file.path(wave, "workers", "worker-*", filename))
  if (!length(paths)) return(NULL)
  value <- do.call(rbind, lapply(paths, function(path) {
    x <- utils::read.csv(
      path, stringsAsFactors = FALSE, check.names = FALSE
    )
    x$source_file <- rep(
      substring(path, nchar(run_root) + 2L), nrow(x)
    )
    x
  }))
  value$wave_id <- rep(
    sub("^[0-9]+__", "", basename(wave)), nrow(value)
  )
  value
}

results <- do.call(rbind, lapply(
  wave_dirs, read_worker_set, filename = "replication_results.csv"
))
diagnostics <- do.call(rbind, lapply(
  wave_dirs, read_worker_set, filename = "fit_diagnostics.csv"
))
statuses <- do.call(rbind, lapply(
  wave_dirs, read_worker_set, filename = "run_status.csv"
))
failures <- do.call(rbind, lapply(
  wave_dirs, read_worker_set, filename = "failure_log.csv"
))
if (is.null(failures) || nrow(failures) != 20L ||
    any(failures$failure_class != "mcmc_diagnostic_failure") ||
    any(failures$retry_count != 0L) ||
    any(!failures$intention_to_run_denominator)) {
  stop("The failure ledger differs from the authenticated terminal state.",
       call. = FALSE)
}

failed_diagnostics <- diagnostics[!diagnostics$pass, , drop = FALSE]
if (nrow(failed_diagnostics) != 72L) {
  stop("The failed diagnostic count differs from the terminal record.",
       call. = FALSE)
}
failed_diagnostics$chain_role <- ifelse(
  failed_diagnostics$sentinel,
  "four_chain_sentinel", "one_chain_standard"
)
failed_diagnostics$bulk_threshold <- ifelse(
  failed_diagnostics$sentinel, 400, 200
)
failed_diagnostics$tail_threshold <- ifelse(
  failed_diagnostics$sentinel, 400, 100
)
failed_diagnostics$rhat_threshold <- ifelse(
  failed_diagnostics$sentinel, 1.01, NA_real_
)
failed_diagnostics$mcse_sd_threshold <- ifelse(
  failed_diagnostics$sentinel, NA_real_, 0.08
)
failed_diagnostics$bulk_fraction <-
  failed_diagnostics$ess_bulk / failed_diagnostics$bulk_threshold
failed_diagnostics$tail_fraction <-
  failed_diagnostics$ess_tail / failed_diagnostics$tail_threshold

failed_cells <- aggregate(
  rep(1L, nrow(failed_diagnostics)),
  failed_diagnostics[c(
    "DGP", "replication", "method", "sentinel", "chain_role"
  )],
  sum
)
names(failed_cells)[[ncol(failed_cells)]] <- "failed_diagnostics"
cell_key <- paste(
  failed_diagnostics$DGP, failed_diagnostics$replication,
  failed_diagnostics$method, sep = "|"
)
failed_cells$minimum_bulk_ess <- vapply(
  paste(failed_cells$DGP, failed_cells$replication,
        failed_cells$method, sep = "|"),
  function(key) min(failed_diagnostics$ess_bulk[cell_key == key]),
  numeric(1L)
)
failed_cells$minimum_tail_ess <- vapply(
  paste(failed_cells$DGP, failed_cells$replication,
        failed_cells$method, sep = "|"),
  function(key) min(failed_diagnostics$ess_tail[cell_key == key]),
  numeric(1L)
)
failed_cells$maximum_mcse_over_sd <- vapply(
  paste(failed_cells$DGP, failed_cells$replication,
        failed_cells$method, sep = "|"),
  function(key) max(failed_diagnostics$mcse_over_sd[cell_key == key]),
  numeric(1L)
)
failed_cells$maximum_rhat <- vapply(
  paste(failed_cells$DGP, failed_cells$replication,
        failed_cells$method, sep = "|"),
  function(key) {
    value <- failed_diagnostics$rhat[cell_key == key]
    if (all(is.na(value))) NA_real_ else max(value, na.rm = TRUE)
  },
  numeric(1L)
)
failed_cells <- failed_cells[order(
  failed_cells$DGP, failed_cells$replication, failed_cells$method
), , drop = FALSE]

method_levels <- sort(unique(results$method), method = "radix")
method_summary <- do.call(rbind, lapply(method_levels, function(method) {
  value <- results[results$method == method, , drop = FALSE]
  diag_value <- diagnostics[diagnostics$method == method, , drop = FALSE]
  data.frame(
    method = method,
    evaluations = nrow(value),
    completed = sum(value$status == "completed"),
    diagnostic_failed = sum(value$status == "diagnostic_failed"),
    diagnostic_rows = nrow(diag_value),
    diagnostic_rows_failed = sum(!diag_value$pass),
    failure_rate = mean(value$status == "diagnostic_failed"),
    stringsAsFactors = FALSE
  )
}))

wave_summary <- do.call(rbind, lapply(seq_along(wave_dirs), function(i) {
  completion <- jsonlite::read_json(file.path(
    run_root, "wave_state", "completions",
    sprintf("%04d__%s.json", i, sub("^[0-9]+__", "", basename(wave_dirs[[i]])))
  ), simplifyVector = TRUE)
  task <- statuses[statuses$wave_id == completion$wave_id, , drop = FALSE]
  diag_value <- diagnostics[
    diagnostics$wave_id == completion$wave_id, , drop = FALSE
  ]
  data.frame(
    canonical_wave_index = i,
    wave_id = completion$wave_id,
    decision = completion$decision,
    planned_tasks = completion$task_count,
    attempted_tasks = sum(!is.na(task$started_at)),
    fully_completed_tasks = sum(task$status == "completed"),
    tasks_with_fit_failure = sum(
      task$status %in% c("completed_with_fit_failure", "cell_stop_failure")
    ),
    not_run_tasks = sum(task$status == "not_run_cell_stop"),
    diagnostic_rows = nrow(diag_value),
    diagnostic_rows_failed = sum(!diag_value$pass),
    stringsAsFactors = FALSE
  )
}))

ledger <- utils::read.csv(
  seed_ledger_path, stringsAsFactors = FALSE, check.names = FALSE
)
contract <- rqr_confirm_read_contract(repo_root)
skew_status <- statuses[grepl("local_level_skewed", statuses$wave_id), ]
failed_task_key <- paste(failures$cell_id, failures$replication, sep = "|")
response_features <- do.call(rbind, lapply(
  seq_len(nrow(skew_status)), function(index) {
    row <- skew_status[index, ]
    generated <- rqr_confirm_generate_dgp(
      contract, row$DGP, row$replication, ledger
    )
    y <- generated$training_y
    mu <- generated$training_mu
    centered <- (y - mean(y)) / stats::sd(y)
    root_width <- generated$training_roots[, 2L] -
      generated$training_roots[, 1L]
    data.frame(
      DGP = row$DGP,
      replication = row$replication,
      task_status = row$status,
      any_method_failure = any(
        failures$replication == row$replication &
          startsWith(failures$cell_id, paste0("C", substring(row$DGP, 2L)))
      ),
      response_mean = mean(y),
      response_sd = stats::sd(y),
      response_skewness = mean(centered^3),
      response_excess_kurtosis = mean(centered^4) - 3,
      response_min = min(y),
      response_max = max(y),
      response_max_abs_z = max(abs(centered)),
      latent_range = diff(range(mu)),
      latent_increment_sd = stats::sd(diff(mu)),
      latent_max_abs_increment = max(abs(diff(mu))),
      oracle_mean_width = mean(root_width),
      stringsAsFactors = FALSE
    )
  }
))
response_features <- unique(response_features)
response_features <- response_features[order(
  response_features$DGP, response_features$replication
), , drop = FALSE]

sentinel_paths <- Sys.glob(file.path(
  wave_dirs[[3L]], "workers", "worker-*", "replications",
  "S*", "rep*", "sentinel_diagnostics_ignored.rds"
))
acf_lags <- c(1L, 5L, 10L, 25L, 50L)
selected <- c(
  "log_q_1", "log_lambda", "mean_midpoint", "mean_width",
  "observed_loss", "terminal_midpoint", "terminal_width"
)
chain_rows <- list()
correlation_rows <- list()
chain_index <- correlation_index <- 0L
for (path in sentinel_paths) {
  bundle <- readRDS(path)
  for (method in names(bundle)) {
    value <- bundle[[method]]
    elapsed <- results$elapsed_seconds[
      results$DGP == value$DGP &
        results$replication == value$replication &
        results$method == method
    ]
    elapsed_per_chain <- if (length(elapsed) == 1L) {
      elapsed / length(value$scalar_chains)
    } else NA_real_
    for (chain in seq_along(value$scalar_chains)) {
      scalars <- as.matrix(value$scalar_chains[[chain]])
      variables <- intersect(selected, colnames(scalars))
      for (variable in variables) {
        draws <- scalars[, variable]
        acf_value <- stats::acf(
          draws, lag.max = max(acf_lags), plot = FALSE
        )$acf[acf_lags + 1L]
        chain_index <- chain_index + 1L
        chain_rows[[chain_index]] <- data.frame(
          DGP = value$DGP,
          replication = value$replication,
          method = method,
          profile = value$chain_profiles[[chain]],
          variable = variable,
          draws = length(draws),
          mean = mean(draws),
          sd = stats::sd(draws),
          q05 = unname(stats::quantile(draws, 0.05, type = 8)),
          q50 = unname(stats::quantile(draws, 0.50, type = 8)),
          q95 = unname(stats::quantile(draws, 0.95, type = 8)),
          acf_lag01 = acf_value[[1L]],
          acf_lag05 = acf_value[[2L]],
          acf_lag10 = acf_value[[3L]],
          acf_lag25 = acf_value[[4L]],
          acf_lag50 = acf_value[[5L]],
          ess_bulk = posterior::ess_bulk(draws),
          ess_tail = posterior::ess_tail(draws),
          approximate_ess_bulk_per_second =
            posterior::ess_bulk(draws) / elapsed_per_chain,
          elapsed_seconds_per_chain_estimate = elapsed_per_chain,
          diagnostic_pass = value$diagnostic_pass,
          stringsAsFactors = FALSE
        )
      }
      variables <- intersect(selected, colnames(scalars))
      if (length(variables) > 1L) {
        correlation <- stats::cor(scalars[, variables, drop = FALSE])
        pairs <- which(upper.tri(correlation), arr.ind = TRUE)
        for (pair in seq_len(nrow(pairs))) {
          correlation_index <- correlation_index + 1L
          correlation_rows[[correlation_index]] <- data.frame(
            DGP = value$DGP,
            replication = value$replication,
            method = method,
            profile = value$chain_profiles[[chain]],
            variable1 = variables[[pairs[pair, 1L]]],
            variable2 = variables[[pairs[pair, 2L]]],
            correlation = correlation[pairs[pair, 1L], pairs[pair, 2L]],
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
}
sentinel_chain_forensics <- if (length(chain_rows)) {
  do.call(rbind, chain_rows)
} else data.frame()
sentinel_correlations <- if (length(correlation_rows)) {
  do.call(rbind, correlation_rows)
} else data.frame()

direct_inputs <- c(
  run_contract_path, closeout_path,
  file.path(closeout_dir, "artifact_hashes.csv"),
  seed_ledger_path,
  file.path(wave_dirs, "wave_manifest.json"),
  file.path(wave_dirs, "wave_artifact_hashes.csv")
)
input_hashes <- data.frame(
  path = normalizePath(direct_inputs, winslash = "/", mustWork = TRUE),
  bytes = as.numeric(file.info(direct_inputs)$size),
  sha256 = vapply(direct_inputs, rqr_confirm_sha256, character(1L)),
  stringsAsFactors = FALSE
)

rqr_confirm_atomic_write_csv(
  wave_summary, file.path(output_dir, "wave_summary.csv")
)
rqr_confirm_atomic_write_csv(
  method_summary, file.path(output_dir, "method_summary.csv")
)
rqr_confirm_atomic_write_csv(
  failed_cells, file.path(output_dir, "failed_cells.csv")
)
rqr_confirm_atomic_write_csv(
  failed_diagnostics, file.path(output_dir, "failed_diagnostics.csv")
)
rqr_confirm_atomic_write_csv(
  response_features, file.path(output_dir, "response_path_features.csv")
)
rqr_confirm_atomic_write_csv(
  sentinel_chain_forensics,
  file.path(output_dir, "sentinel_chain_forensics.csv")
)
rqr_confirm_atomic_write_csv(
  sentinel_correlations,
  file.path(output_dir, "sentinel_chain_correlations.csv")
)
rqr_confirm_atomic_write_csv(
  input_hashes, file.path(output_dir, "input_artifact_hashes.csv")
)

manifest <- list(
  schema_version = "rqrgibbs_dlm_skewed_failure_forensics/1.0.0",
  run_id = run_contract$run_id,
  authorization_commit = run_contract$authorization_commit,
  runtime_tree_digest = run_contract$runtime_tree_digest,
  seed_ledger_sha256 = rqr_confirm_sha256(seed_ledger_path),
  completed_waves = nrow(wave_summary),
  failed_wave = wave_summary$wave_id[wave_summary$decision == "failed"],
  failed_method_evaluations = nrow(failures),
  failed_diagnostic_rows = nrow(failed_diagnostics),
  model_fits_executed = 0L,
  threshold_changes = FALSE,
  scientific_metrics_used_for_selection = FALSE,
  scientific_promotion = FALSE,
  generalized_bayes = TRUE,
  response_likelihood = FALSE,
  source_run_mutated = FALSE,
  completed_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
)
rqr_confirm_atomic_write_json(
  manifest, file.path(output_dir, "forensic_manifest.json")
)
hashes <- rqr_confirm_recursive_manifest(output_dir)
rqr_confirm_atomic_write_csv(
  hashes, file.path(output_dir, "artifact_hashes.csv")
)
invisible(rqr_confirm_verify_recursive_manifest(output_dir))

cat(sprintf(
  "Skewed-wave forensic audit complete: %d failed cells, %d failed diagnostics.\n",
  nrow(failed_cells), nrow(failed_diagnostics)
))
