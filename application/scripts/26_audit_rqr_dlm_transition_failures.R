#!/usr/bin/env Rscript

# Read-only forensic audit for the RQR-DLM M01 wave-2 component-scale
# transition failures.  The script consumes already-completed ignored evidence
# roots and writes compact, tracked CSV/Markdown summaries.  It does not fit
# models, relaunch waves, change thresholds, or mutate local cache artifacts.

args <- commandArgs(trailingOnly = TRUE)
repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(repo_root, "application", "DESCRIPTION"))) {
  stop("Run this script from the RQR-GIBBS repository root.", call. = FALSE)
}

parse_arg <- function(name, default = NULL) {
  prefix <- paste0("--", name, "=")
  matched <- grep(paste0("^", prefix), args, value = TRUE)
  if (!length(matched)) return(default)
  sub(prefix, "", matched[[length(matched)]])
}

if (any(args %in% c("-h", "--help"))) {
  cat(paste(
    "Usage:",
    "  Rscript application/scripts/26_audit_rqr_dlm_transition_failures.R",
    "    [--output-dir=docs/audits/rqr_dlm_transition_forensics_20260727]",
    "",
    "Inputs are the three ignored M01 wave-2 evidence roots declared inside",
    "this script. Outputs are compact CSV/Markdown audit artifacts only.",
    sep = "\n"
  ))
  quit(status = 0L)
}

output_dir <- parse_arg(
  "output-dir",
  file.path(repo_root, "docs", "audits",
            "rqr_dlm_transition_forensics_20260727")
)
output_dir <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
if (file.exists(output_dir) || dir.exists(output_dir)) {
  stop(
    sprintf("Refusing to overwrite existing audit output directory: %s",
            output_dir),
    call. = FALSE
  )
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  if (requireNamespace("pkgload", quietly = TRUE)) {
    pkgload::load_all(file.path(repo_root, "application"), quiet = TRUE)
  } else {
    library(rqrgibbs)
  }
})
source(file.path(
  repo_root, "application", "scripts", "lib",
  "rqr_dlm_confirmatory_simulation.R"
))

strict_file <- function(path) {
  path <- file.path(repo_root, path)
  if (!file.exists(path)) {
    stop(sprintf("Required input artifact is missing: %s", path),
         call. = FALSE)
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

attempts <- data.frame(
  attempt_id = c(
    "one_root_exact_e9c8068",
    "symmetric_rootwise_one_ASIS",
    "symmetric_rootwise_two_ASIS"
  ),
  attempt_order = c(1L, 2L, 3L),
  transition_label = c(
    "one-root partially collapsed scale update",
    "symmetric rootwise partial collapse + one ASIS cycle",
    "symmetric rootwise partial collapse + two ASIS cycles"
  ),
  evidence_root = c(
    "application/cache/rqr_dlm_promotion_e9c8068_20260727/m01_wave2",
    "application/cache/rqr_dlm_wave2_symmetric_rootwise_dev_20260727",
    "application/cache/rqr_dlm_wave2_two_ASIS_cycles_dev2_20260727"
  ),
  diagnostics_file = c(
    "wave2_M01_diagnostics.csv",
    "wave2_M01_diagnostics.csv",
    "wave2_M01_diagnostics.csv"
  ),
  summary_file = c(
    "wave2_M01_summary.csv",
    "wave2_M01_summary.csv",
    "wave2_M01_summary.csv"
  ),
  chain_evidence_file = c(
    "wave2_M01_chain_evidence.rds",
    "wave2_M01_chain_evidence.rds",
    "wave2_M01_chain_evidence.rds"
  ),
  manifest_file = rep("validation_manifest.json", 3L),
  stringsAsFactors = FALSE
)

attempts$evidence_root_abs <- vapply(seq_len(nrow(attempts)), function(i) {
  root <- normalizePath(
    file.path(repo_root, attempts$evidence_root[[i]]),
    winslash = "/", mustWork = TRUE
  )
  required <- file.path(root, attempts$diagnostics_file[[i]])
  if (!file.exists(required)) {
    stop(sprintf("Required input artifact is missing: %s", required),
         call. = FALSE)
  }
  root
}, character(1L))

sha256_file <- function(path) {
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

file_record <- function(attempt, relative) {
  path <- file.path(attempt$evidence_root_abs, relative)
  if (!file.exists(path)) {
    stop(sprintf("Missing input artifact: %s", path), call. = FALSE)
  }
  info <- file.info(path)
  data.frame(
    attempt_id = attempt$attempt_id,
    attempt_order = attempt$attempt_order,
    transition_label = attempt$transition_label,
    relative_path = sub(
      paste0("^", gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", repo_root), "/?"),
      "",
      normalizePath(path, winslash = "/", mustWork = TRUE)
    ),
    bytes = as.numeric(info$size),
    sha256 = sha256_file(path),
    stringsAsFactors = FALSE
  )
}

read_attempt <- function(row) {
  root <- row$evidence_root_abs
  diagnostics_path <- file.path(root, row$diagnostics_file)
  summary_path <- file.path(root, row$summary_file)
  evidence_path <- file.path(root, row$chain_evidence_file)
  manifest_path <- file.path(root, row$manifest_file)
  diagnostics <- utils::read.csv(
    diagnostics_path, stringsAsFactors = FALSE, check.names = FALSE
  )
  summary <- utils::read.csv(
    summary_path, stringsAsFactors = FALSE, check.names = FALSE
  )
  evidence <- readRDS(evidence_path)
  manifest <- jsonlite::read_json(manifest_path, simplifyVector = TRUE)
  required_evidence <- c("jobs", "results", "diagnostics")
  if (!is.list(evidence) ||
      !all(required_evidence %in% names(evidence)) ||
      !identical(nrow(diagnostics), nrow(evidence$diagnostics)) ||
      !identical(length(evidence$results), length(evidence$jobs))) {
    stop(sprintf("Evidence bundle failed shape checks for %s.",
                 row$attempt_id), call. = FALSE)
  }
  diagnostics$attempt_id <- row$attempt_id
  diagnostics$attempt_order <- row$attempt_order
  diagnostics$transition_label <- row$transition_label
  diagnostics$method <- "M01"
  diagnostics$chain_role <- ifelse(
    diagnostics$sentinel, "four_chain_sentinel", "one_chain_standard"
  )
  summary$attempt_id <- row$attempt_id
  summary$attempt_order <- row$attempt_order
  summary$transition_label <- row$transition_label
  summary$method <- "M01"
  summary$chain_role <- ifelse(
    summary$sentinel, "four_chain_sentinel", "one_chain_standard"
  )
  list(
    attempt = row,
    diagnostics = diagnostics,
    summary = summary,
    evidence = evidence,
    manifest = manifest,
    file_hashes = do.call(rbind, lapply(
      c(row$diagnostics_file, row$summary_file, row$chain_evidence_file,
        row$manifest_file, "artifact_hashes.csv"),
      function(relative) {
        path <- file.path(root, relative)
        if (file.exists(path)) file_record(row, relative) else NULL
      }
    ))
  )
}

attempt_list <- lapply(
  split(attempts, seq_len(nrow(attempts))),
  function(one) read_attempt(one[1L, , drop = FALSE])
)

all_diagnostics <- do.call(
  rbind, lapply(attempt_list, `[[`, "diagnostics")
)
all_summary <- do.call(
  rbind, lapply(attempt_list, `[[`, "summary")
)
input_hashes <- do.call(
  rbind, lapply(attempt_list, `[[`, "file_hashes")
)

selected_variables <- c(
  "log_q_1", "observed_loss", "mean_width", "mean_midpoint",
  "terminal_width", "terminal_midpoint"
)
acf_lags <- c(1L, 5L, 10L, 25L, 50L)
quantile_probs <- c(0, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 1)
quantile_names <- paste0(
  "q", sprintf("%03d", as.integer(round(100 * quantile_probs)))
)

job_table <- function(result) {
  data.frame(
    DGP = result$job$scenario_id,
    replication = result$job$replication,
    chain = result$job$chain,
    sentinel = result$job$sentinel,
    profile = result$job$profile,
    ok = isTRUE(result$ok),
    fit_elapsed_seconds =
      as.numeric(result$fit_elapsed_seconds %||% NA_real_),
    peak_RSS_KiB = as.numeric(result$peak_RSS_KiB %||% NA_real_),
    reproducibility_eligible =
      as.logical(result$reproducibility_eligible %||% NA),
    runtime_tree_digest =
      as.character(result$runtime_tree_digest %||% NA_character_),
    stringsAsFactors = FALSE
  )
}

summarize_quantiles <- function(attempt_id, attempt_order, transition_label,
                                result) {
  scalars <- result$scalars
  if (is.null(scalars)) return(NULL)
  variables <- intersect(selected_variables, colnames(scalars))
  if (!length(variables)) return(NULL)
  rows <- lapply(variables, function(variable) {
    values <- as.numeric(scalars[, variable])
    qs <- stats::quantile(
      values, probs = quantile_probs, names = FALSE, type = 8
    )
    data.frame(
      attempt_id = attempt_id,
      attempt_order = attempt_order,
      transition_label = transition_label,
      DGP = result$job$scenario_id,
      replication = result$job$replication,
      chain = result$job$chain,
      sentinel = result$job$sentinel,
      profile = result$job$profile,
      variable = variable,
      n = length(values),
      mean = mean(values),
      sd = stats::sd(values),
      as.list(stats::setNames(as.numeric(qs), quantile_names)),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

summarize_acf <- function(attempt_id, attempt_order, transition_label,
                          result) {
  scalars <- result$scalars
  if (is.null(scalars)) return(NULL)
  variables <- intersect(selected_variables, colnames(scalars))
  if (!length(variables)) return(NULL)
  rows <- lapply(variables, function(variable) {
    values <- as.numeric(scalars[, variable])
    acf_values <- rep(NA_real_, length(acf_lags))
    if (length(values) > max(acf_lags) && stats::sd(values) > 0) {
      raw <- stats::acf(
        values, lag.max = max(acf_lags), plot = FALSE,
        na.action = stats::na.pass
      )$acf
      acf_values <- as.numeric(raw[acf_lags + 1L])
    }
    data.frame(
      attempt_id = attempt_id,
      attempt_order = attempt_order,
      transition_label = transition_label,
      DGP = result$job$scenario_id,
      replication = result$job$replication,
      chain = result$job$chain,
      sentinel = result$job$sentinel,
      profile = result$job$profile,
      variable = variable,
      lag = acf_lags,
      acf = acf_values,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

chain_rows <- vector("list", 0L)
quantile_rows <- vector("list", 0L)
acf_rows <- vector("list", 0L)
row_index <- 0L
for (attempt in attempt_list) {
  row <- attempt$attempt
  for (result in attempt$evidence$results) {
    row_index <- row_index + 1L
    chain_rows[[row_index]] <- cbind(
      attempt_id = row$attempt_id,
      attempt_order = row$attempt_order,
      transition_label = row$transition_label,
      job_table(result),
      stringsAsFactors = FALSE
    )
    quantile_rows[[row_index]] <- summarize_quantiles(
      row$attempt_id, row$attempt_order, row$transition_label, result
    )
    acf_rows[[row_index]] <- summarize_acf(
      row$attempt_id, row$attempt_order, row$transition_label, result
    )
  }
}
chain_manifest <- do.call(rbind, chain_rows)
scalar_quantiles <- do.call(rbind, quantile_rows)
acf_summary <- do.call(rbind, acf_rows)

diagnostic_pass <- as.logical(all_diagnostics$pass)
diagnostic_pass[is.na(diagnostic_pass)] <- FALSE
failures <- all_diagnostics[!diagnostic_pass, , drop = FALSE]
failures <- failures[order(
  failures$attempt_order, failures$DGP, failures$replication,
  failures$estimand
), , drop = FALSE]
rownames(failures) <- NULL

logq_diag <- all_diagnostics[
  all_diagnostics$estimand == "log_q_1", , drop = FALSE
]
obs_diag <- all_diagnostics[
  all_diagnostics$estimand == "observed_loss", , drop = FALSE
]
attempt_summary <- do.call(rbind, lapply(
  split(all_diagnostics, all_diagnostics$attempt_id),
  function(rows) {
    first <- rows[1L, ]
    logq <- rows[rows$estimand == "log_q_1", , drop = FALSE]
    data.frame(
      attempt_id = first$attempt_id,
      attempt_order = first$attempt_order,
      transition_label = first$transition_label,
      diagnostics_total = nrow(rows),
      diagnostics_passed = sum(rows$pass),
      diagnostics_failed = sum(!rows$pass),
      tasks_total = length(unique(paste(rows$DGP, rows$replication))),
      tasks_failed = length(unique(paste(
        rows$DGP[!rows$pass], rows$replication[!rows$pass]
      ))),
      log_q_1_bulk_min = min(logq$ess_bulk, na.rm = TRUE),
      log_q_1_bulk_median = stats::median(logq$ess_bulk, na.rm = TRUE),
      log_q_1_tail_min = min(logq$ess_tail, na.rm = TRUE),
      log_q_1_mcse_over_sd_max =
        max(logq$mcse_over_sd, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }
))
attempt_summary <- attempt_summary[
  order(attempt_summary$attempt_order), , drop = FALSE
]
rownames(attempt_summary) <- NULL

ess_speed <- merge(
  logq_diag[
    , c(
      "attempt_id", "attempt_order", "transition_label", "DGP",
      "replication", "sentinel", "ess_bulk", "ess_tail", "mcse_over_sd",
      "pass"
    ),
    drop = FALSE
  ],
  all_summary[
    , c(
      "attempt_id", "DGP", "replication", "fit_elapsed_seconds",
      "chain_role"
    ),
    drop = FALSE
  ],
  by = c("attempt_id", "DGP", "replication"),
  all.x = TRUE,
  sort = FALSE
)
ess_speed$method <- "M01"
ess_speed$estimand <- "log_q_1"
ess_speed$bulk_ess_per_second <- with(
  ess_speed,
  ifelse(is.finite(fit_elapsed_seconds) & fit_elapsed_seconds > 0,
         ess_bulk / fit_elapsed_seconds, NA_real_)
)
ess_speed <- ess_speed[order(
  ess_speed$attempt_order, ess_speed$DGP, ess_speed$replication
), , drop = FALSE]

contract <- rqr_confirm_read_contract(repo_root)
rqr_confirm_validate_contract(contract, require_closed = TRUE)
ledger <- rqr_confirm_seed_ledger(contract, planning = "maximum")
task_keys <- unique(all_summary[c("DGP", "replication")])
response_rows <- lapply(seq_len(nrow(task_keys)), function(index) {
  generated <- rqr_confirm_generate_dgp(
    contract, task_keys$DGP[[index]], task_keys$replication[[index]],
    ledger
  )
  y <- as.numeric(generated$training_y)
  dy <- diff(y)
  root_width <- generated$training_roots[, "upper"] -
    generated$training_roots[, "lower"]
  within_oracle <- y >= generated$training_roots[, "lower"] &
    y <= generated$training_roots[, "upper"]
  oracle_loss <- sum(rqr_check_loss(
    rqr_residual_product(
      y,
      generated$training_roots[, "lower"],
      generated$training_roots[, "upper"]
    ),
    generated$coverage_level
  ))
  data.frame(
    DGP = generated$scenario_id,
    replication = generated$replication,
    dgp = generated$dgp,
    coverage_level = generated$coverage_level,
    T = generated$T,
    training_response_mean = mean(y),
    training_response_sd = stats::sd(y),
    training_response_iqr = stats::IQR(y),
    training_response_min = min(y),
    training_response_max = max(y),
    training_response_range = diff(range(y)),
    training_response_max_abs_z =
      max(abs((y - mean(y)) / stats::sd(y))),
    training_response_lag1_acf = if (length(y) > 2L &&
        stats::sd(y) > 0) {
      as.numeric(stats::acf(y, lag.max = 1L, plot = FALSE)$acf[2L])
    } else {
      NA_real_
    },
    response_diff_sd = if (length(dy) > 1L) stats::sd(dy) else NA_real_,
    response_diff_mean_abs =
      if (length(dy)) mean(abs(dy)) else NA_real_,
    response_diff_max_abs =
      if (length(dy)) max(abs(dy)) else NA_real_,
    latent_level_sd =
      if (!is.null(generated$latent$level)) {
        stats::sd(generated$latent$level)
      } else {
        NA_real_
      },
    latent_scale_mean = mean(generated$training_scale),
    latent_scale_sd = stats::sd(generated$training_scale),
    oracle_mean_width = mean(root_width),
    oracle_min_width = min(root_width),
    oracle_max_width = max(root_width),
    oracle_training_coverage = mean(within_oracle),
    oracle_observed_loss = oracle_loss,
    generalized_bayes_target = TRUE,
    response_likelihood = FALSE,
    response_prediction_contract = FALSE,
    stringsAsFactors = FALSE
  )
})
response_features <- do.call(rbind, response_rows)

task_attempt_flags <- all_summary[
  , c(
    "attempt_id", "attempt_order", "transition_label", "DGP",
    "replication", "sentinel", "chains", "all_pass",
    "diagnostics_passed", "diagnostics", "log_q_1_ess_bulk",
    "log_q_1_ess_tail", "log_q_1_mcse_over_sd"
  ),
  drop = FALSE
]
observed_loss_rows <- obs_diag[
  , c("attempt_id", "DGP", "replication", "ess_bulk", "ess_tail",
      "mcse_over_sd", "pass"),
  drop = FALSE
]
names(observed_loss_rows)[names(observed_loss_rows) == "ess_bulk"] <-
  "observed_loss_ess_bulk"
names(observed_loss_rows)[names(observed_loss_rows) == "ess_tail"] <-
  "observed_loss_ess_tail"
names(observed_loss_rows)[names(observed_loss_rows) == "mcse_over_sd"] <-
  "observed_loss_mcse_over_sd"
names(observed_loss_rows)[names(observed_loss_rows) == "pass"] <-
  "observed_loss_pass"
feature_failure_panel <- merge(
  task_attempt_flags, observed_loss_rows,
  by = c("attempt_id", "DGP", "replication"),
  all.x = TRUE, sort = FALSE
)
feature_failure_panel <- merge(
  feature_failure_panel, response_features,
  by = c("DGP", "replication"), all.x = TRUE, sort = FALSE
)
feature_failure_panel$failed_estimands <- vapply(
  seq_len(nrow(feature_failure_panel)),
  function(index) {
    matched <- failures[
      failures$attempt_id == feature_failure_panel$attempt_id[[index]] &
        failures$DGP == feature_failure_panel$DGP[[index]] &
        failures$replication == feature_failure_panel$replication[[index]],
      "estimand"
    ]
    if (!length(matched)) "" else paste(sort(unique(matched)), collapse = ";")
  },
  character(1L)
)
feature_failure_panel <- feature_failure_panel[order(
  feature_failure_panel$attempt_order,
  feature_failure_panel$DGP,
  feature_failure_panel$replication
), , drop = FALSE]

manifest_rows <- lapply(attempt_list, function(attempt) {
  manifest <- attempt$manifest
  row <- attempt$attempt
  data.frame(
    attempt_id = row$attempt_id,
    attempt_order = row$attempt_order,
    transition_label = row$transition_label,
    schema_version = as.character(manifest$schema_version %||% NA),
    source_commit = as.character(manifest$source_commit %||% NA),
    source_clean = as.logical(manifest$source_clean %||% NA),
    package_version = as.character(manifest$package_version %||% NA),
    fit_schema = "from_chain_evidence_scalar_contract",
    wave_id = as.character(manifest$wave_id %||% NA),
    wave_task_count = as.integer(manifest$wave_task_count %||% NA),
    chain_job_count = as.integer(manifest$chain_job_count %||% NA),
    workers = as.integer(manifest$workers %||% NA),
    all_fits_succeeded =
      as.logical(manifest$all_fits_succeeded %||% NA),
    all_diagnostics_passed =
      as.logical(manifest$all_diagnostics_passed %||% NA),
    resource_margin_pass =
      as.logical(manifest$resource_margin_pass %||% NA),
    total_fit_elapsed_seconds =
      as.numeric(manifest$total_fit_elapsed_seconds %||% NA_real_),
    maximum_process_peak_RSS_KiB =
      as.numeric(manifest$maximum_process_peak_RSS_KiB %||% NA_real_),
    comparative_simulation_metrics_used =
      as.logical(manifest$comparative_simulation_metrics_used %||% NA),
    failed_outputs_reused =
      as.logical(manifest$failed_outputs_reused %||% NA),
    stringsAsFactors = FALSE
  )
})
manifest_summary <- do.call(rbind, manifest_rows)

write_csv <- function(value, file) {
  utils::write.csv(value, file.path(output_dir, file), row.names = FALSE,
                   quote = TRUE, na = "")
}

write_csv(attempts[
  , c("attempt_id", "attempt_order", "transition_label", "evidence_root"),
  drop = FALSE
], "attempts.csv")
write_csv(input_hashes, "input_artifact_hashes.csv")
write_csv(manifest_summary, "manifest_summary.csv")
write_csv(chain_manifest, "chain_manifest.csv")
write_csv(failures, "diagnostic_failures.csv")
write_csv(attempt_summary, "diagnostic_summary_by_attempt.csv")
write_csv(ess_speed, "effective_draws_per_second.csv")
write_csv(scalar_quantiles, "scalar_quantiles.csv")
write_csv(acf_summary, "autocorrelation_summary.csv")
write_csv(response_features, "response_path_features.csv")
write_csv(feature_failure_panel, "feature_failure_panel.csv")

report_path <- file.path(output_dir, "README.md")
hard_cases <- failures[
  failures$DGP == "S03" & failures$replication %in% c(13L, 94L),
  , drop = FALSE
]
report_lines <- c(
  "# RQR-DLM transition failure forensic audit",
  "",
  paste("Date:", format(Sys.Date())),
  "",
  "This compact audit digests the three completed M01 wave-2 evidence roots.",
  "It is read-only: it does not relaunch fits, change thresholds, or reuse",
  "failed/development outputs as promotion evidence.",
  "",
  "## Attempt-level diagnostic summary",
  "",
  paste(
    "| Attempt | Diagnostics | Failed tasks | min bulk ESS log_q_1 | median bulk ESS log_q_1 | max MCSE/SD log_q_1 |",
    "|---|---:|---:|---:|---:|---:|",
    sep = "\n"
  ),
  apply(attempt_summary, 1L, function(row) {
    sprintf(
      "| %s | %s/%s | %s | %.2f | %.2f | %.4f |",
      row[["attempt_id"]],
      row[["diagnostics_passed"]], row[["diagnostics_total"]],
      row[["tasks_failed"]],
      as.numeric(row[["log_q_1_bulk_min"]]),
      as.numeric(row[["log_q_1_bulk_median"]]),
      as.numeric(row[["log_q_1_mcse_over_sd_max"]])
    )
  }),
  "",
  "## Persistent hard cases",
  "",
  "The persistent failures remain concentrated in ordinary one-chain S03",
  "replications 13 and 94.  The guard replication 55 fails only in the",
  "one-root exact attempt and is retained for the next development comparison.",
  "",
  "## Output files",
  "",
  "- `attempts.csv`: declared input evidence roots.",
  "- `input_artifact_hashes.csv`: SHA-256 hashes for consumed artifacts.",
  "- `manifest_summary.csv`: compact manifest fields by attempt.",
  "- `chain_manifest.csv`: one row per retained scalar-chain object.",
  "- `diagnostic_failures.csv`: failed diagnostics with method and role.",
  "- `diagnostic_summary_by_attempt.csv`: attempt-level pass/fail counts.",
  "- `effective_draws_per_second.csv`: log-q ESS normalized by wall time.",
  "- `scalar_quantiles.csv`: quantiles for selected retained scalar draws.",
  "- `autocorrelation_summary.csv`: ACF at lags 1, 5, 10, 25, and 50.",
  "- `response_path_features.csv`: regenerated frozen DGP path summaries.",
  "- `feature_failure_panel.csv`: path features joined to diagnostic status.",
  "",
  "## Scope",
  "",
  "These artifacts support computational transition diagnosis only. They are",
  "not scientific simulation results, do not define response-predictive draws,",
  "and do not authorize a main simulation launch."
)
writeLines(report_lines, report_path)

output_hashes <- data.frame(
  relative_path = sub(
    paste0("^", gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", repo_root), "/?"),
    "",
    list.files(output_dir, recursive = TRUE, full.names = TRUE)
  ),
  stringsAsFactors = FALSE
)
output_hashes$bytes <- vapply(
  file.path(repo_root, output_hashes$relative_path),
  function(path) as.numeric(file.info(path)$size),
  numeric(1L)
)
output_hashes$sha256 <- vapply(
  file.path(repo_root, output_hashes$relative_path),
  sha256_file, character(1L)
)
utils::write.csv(
  output_hashes, file.path(output_dir, "artifact_hashes.csv"),
  row.names = FALSE, quote = TRUE, na = ""
)

cat(sprintf(
  paste0(
    "RQR-DLM transition forensic audit wrote %d files to %s. ",
    "Failure rows: %d.\n"
  ),
  nrow(output_hashes) + 1L, output_dir, nrow(failures)
))
