#!/usr/bin/env Rscript

arguments <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_path <- sub(file_arg, "", arguments[startsWith(arguments, file_arg)][1L])
if (!length(script_path) || is.na(script_path)) {
  script_path <- "application/scripts/75_collect_rqr_bayes_uq_with_young_mathew.R"
}
script_path <- normalizePath(script_path, winslash = "/", mustWork = TRUE)
repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."),
                           winslash = "/", mustWork = TRUE)
setwd(repo_root)

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(prefix, default = NULL) {
  hit <- args[startsWith(args, prefix)]
  if (length(hit)) sub(prefix, "", hit[[1L]], fixed = TRUE) else default
}
stopf <- function(...) stop(paste0(...), call. = FALSE)
`%||%` <- function(a, b) if (is.null(a)) b else a

for (package in c("jsonlite", "digest")) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stopf("Required package is not installed: ", package)
  }
}

main_run_dir <- normalizePath(arg_value(
  "--main-run-dir=",
  file.path("application", "runs", "rqr_bayes_uq_validation_main_20260813",
            "wave_main_20260813T103232Z")
), winslash = "/", mustWork = TRUE)
ym_run_dir <- normalizePath(arg_value("--young-mathew-dir=", ""),
                            winslash = "/", mustWork = TRUE)
output_dir <- normalizePath(arg_value(
  "--output-dir=",
  file.path(main_run_dir, paste0(
    "combined_with_young_mathew_",
    format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
  ))
), winslash = "/", mustWork = FALSE)

required_main <- c(
  "bayes_uq_validation_results.csv",
  "bayes_uq_validation_summary.csv",
  "manifest.json"
)
missing_main <- required_main[
  !file.exists(file.path(main_run_dir, required_main))
]
if (length(missing_main)) {
  stopf(
    "Main Bayesian-UQ run is not collected yet; missing: ",
    paste(missing_main, collapse = ", ")
  )
}
required_ym <- c(
  "bayes_uq_validation_results.csv",
  "bayes_uq_validation_summary.csv",
  "manifest.json"
)
missing_ym <- required_ym[!file.exists(file.path(ym_run_dir, required_ym))]
if (length(missing_ym)) {
  stopf("Young-Mathew add-on is incomplete; missing: ",
        paste(missing_ym, collapse = ", "))
}
if (file.exists(output_dir) || dir.exists(output_dir)) {
  stopf("The output directory must be fresh: ", output_dir)
}
dir.create(dirname(output_dir), recursive = TRUE, showWarnings = FALSE)
staging <- tempfile(paste0(".", basename(output_dir), "-"),
                    tmpdir = dirname(output_dir))
dir.create(staging, recursive = TRUE, showWarnings = FALSE)
published <- FALSE
on.exit({
  if (!published) unlink(staging, recursive = TRUE, force = TRUE)
}, add = TRUE)

main_results <- utils::read.csv(
  file.path(main_run_dir, "bayes_uq_validation_results.csv"),
  stringsAsFactors = FALSE, check.names = FALSE
)
ym_results <- utils::read.csv(
  file.path(ym_run_dir, "bayes_uq_validation_results.csv"),
  stringsAsFactors = FALSE, check.names = FALSE
)
if (!nrow(ym_results) || !all(ym_results$method_id == "young_mathew")) {
  stopf("Young-Mathew add-on results do not contain only young_mathew rows.")
}

bind_fill <- function(a, b) {
  cols <- union(names(a), names(b))
  add_missing <- function(x) {
    missing <- setdiff(cols, names(x))
    for (name in missing) x[[name]] <- NA
    x[cols]
  }
  rbind(add_missing(a), add_missing(b))
}
results <- bind_fill(main_results, ym_results)

reference_method_id <- "tcsp_mc"
reference_keys <- c("mode", "dgp_id", "n", "guaranteed_content",
                    "tolerance_confidence", "posterior_confidence",
                    "replication", "seed")
reference_rows <- results[results$method_id == reference_method_id,
                          c(reference_keys, "width"), drop = FALSE]
if (!nrow(reference_rows)) {
  stopf("Reference method not present in combined results: ",
        reference_method_id)
}
names(reference_rows)[names(reference_rows) == "width"] <- "reference_width"
results$reference_width <- NULL
results <- merge(results, reference_rows, by = reference_keys, all.x = TRUE,
                 sort = FALSE)
results$reference_method_id <- reference_method_id
results$width_ratio_to_reference <- ifelse(
  is.finite(results$reference_width) & results$reference_width > 0,
  results$width / results$reference_width,
  NA_real_
)
results$width_diff_to_reference <- results$width - results$reference_width
results$posterior_constraint_binding <- ifelse(
  results$method_id %in% c("hdp_s", "hdp_s_mc"),
  results$posterior_constraint_status == "binding",
  NA
)

oracle_rows <- results[results$method_id == "oracle_sh",
                       c(reference_keys, "lower", "upper", "width",
                         "oracle_mean_tilt", "oracle_certificate_digest"),
                       drop = FALSE]
if (nrow(oracle_rows)) {
  names(oracle_rows)[names(oracle_rows) == "lower"] <-
    "oracle_reference_lower"
  names(oracle_rows)[names(oracle_rows) == "upper"] <-
    "oracle_reference_upper"
  names(oracle_rows)[names(oracle_rows) == "width"] <-
    "oracle_reference_width"
  names(oracle_rows)[names(oracle_rows) == "oracle_mean_tilt"] <-
    "oracle_reference_mean_tilt"
  names(oracle_rows)[names(oracle_rows) == "oracle_certificate_digest"] <-
    "oracle_reference_certificate_digest"
  results$oracle_sh_lower <- NULL
  results$oracle_sh_upper <- NULL
  results$oracle_sh_width <- NULL
  results <- merge(results, oracle_rows, by = reference_keys, all.x = TRUE,
                   sort = FALSE)
  results$oracle_sh_lower <- results$oracle_reference_lower
  results$oracle_sh_upper <- results$oracle_reference_upper
  results$oracle_sh_width <- results$oracle_reference_width
  results$oracle_mean_tilt <- ifelse(
    is.na(results$oracle_mean_tilt),
    results$oracle_reference_mean_tilt,
    results$oracle_mean_tilt
  )
  results$oracle_certificate_digest <- ifelse(
    is.na(results$oracle_certificate_digest),
    results$oracle_reference_certificate_digest,
    results$oracle_certificate_digest
  )
  results$width_ratio_to_oracle_sh <- ifelse(
    is.finite(results$oracle_sh_width) & results$oracle_sh_width > 0,
    results$width / results$oracle_sh_width,
    NA_real_
  )
  results$width_excess_vs_oracle_sh <- results$width - results$oracle_sh_width
  results$oracle_reference_lower <- NULL
  results$oracle_reference_upper <- NULL
  results$oracle_reference_width <- NULL
  results$oracle_reference_mean_tilt <- NULL
  results$oracle_reference_certificate_digest <- NULL
}

results <- results[order(
  results$dgp_id, results$n, results$guaranteed_content,
  results$tolerance_confidence, results$posterior_confidence,
  results$replication, results$method_id
), , drop = FALSE]
write.csv(results, file.path(staging, "bayes_uq_validation_results.csv"),
          row.names = FALSE)

mean_or_na <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_real_)
  mean(x)
}
median_or_na <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_real_)
  stats::median(x)
}
keys <- c("mode", "dgp_id", "n", "guaranteed_content",
          "tolerance_confidence", "posterior_confidence", "method_id")
summary_rows <- lapply(split(results, interaction(results[keys], drop = TRUE,
                                                  lex.order = TRUE)),
                       function(df) {
  ok <- !is.na(df$success)
  data.frame(
    mode = df$mode[[1L]],
    dgp_id = df$dgp_id[[1L]],
    n = df$n[[1L]],
    guaranteed_content = df$guaranteed_content[[1L]],
    tolerance_confidence = df$tolerance_confidence[[1L]],
    posterior_confidence = df$posterior_confidence[[1L]],
    method_id = df$method_id[[1L]],
    replications = nrow(df),
    infeasible_rate = mean(df$infeasible),
    success_rate = if (any(ok)) mean(df$success[ok]) else NA_real_,
    success_rate_minus_tolerance_confidence =
      if (any(ok)) mean(df$success[ok]) - df$tolerance_confidence[[1L]]
      else NA_real_,
    mean_content = mean_or_na(df$content),
    mean_content_gap = mean_or_na(df$content_gap),
    median_width = median_or_na(df$width),
    mean_width = mean_or_na(df$width),
    median_formal_action_width = median_or_na(df$formal_action_width),
    mean_formal_action_content = mean_or_na(df$formal_action_content),
    formal_action_success_rate = mean_or_na(df$formal_action_success),
    median_fitted_summary_width = median_or_na(df$fitted_summary_width),
    mean_target_content = mean_or_na(df$target_content),
    mean_target_mean_tilt = mean_or_na(df$target_mean_tilt),
    mean_posterior_draws = mean_or_na(df$posterior_draws),
    mcmc_fit_reuse_rate =
      mean_or_na(df$fit_reused_across_posterior_thresholds),
    ecm_convergence_rate = mean_or_na(df$ecm_converged),
    mean_ecm_iterations = mean_or_na(df$ecm_iterations),
    median_width_ratio_to_reference =
      median_or_na(df$width_ratio_to_reference),
    mean_width_ratio_to_reference =
      mean_or_na(df$width_ratio_to_reference),
    median_width_diff_to_reference =
      median_or_na(df$width_diff_to_reference),
    median_width_ratio_to_oracle_sh =
      median_or_na(df$width_ratio_to_oracle_sh),
    mean_width_ratio_to_oracle_sh =
      mean_or_na(df$width_ratio_to_oracle_sh),
    median_width_excess_vs_oracle_sh =
      median_or_na(df$width_excess_vs_oracle_sh),
    mean_width_excess_vs_oracle_sh =
      mean_or_na(df$width_excess_vs_oracle_sh),
    mean_retained_fraction = mean_or_na(df$retained_fraction),
    mean_content_buffer = mean_or_na(df$content_buffer),
    mean_scan_certified_lower_probability =
      mean_or_na(df$scan_certified_lower_probability),
    mean_posterior_probability =
      mean_or_na(df$posterior_probability),
    mean_posterior_threshold_excess =
      mean_or_na(df$posterior_threshold_excess),
    posterior_binding_rate =
      mean_or_na(df$posterior_constraint_binding),
    mean_candidate_feasible_count =
      mean_or_na(df$candidate_feasible_count),
    mean_candidates_evaluated =
      mean_or_na(df$candidates_evaluated),
    mean_elapsed_sec = mean_or_na(df$elapsed_sec)
  )
})
summary <- do.call(rbind, summary_rows)
write.csv(summary, file.path(staging, "bayes_uq_validation_summary.csv"),
          row.names = FALSE)

readme <- c(
  "# Bayesian-UQ Validation Combined with Young-Mathew",
  "",
  paste0("- Main run directory: `", main_run_dir, "`"),
  paste0("- Young-Mathew add-on directory: `", ym_run_dir, "`"),
  paste0("- Result rows: `", nrow(results), "`"),
  paste0("- Summary rows: `", nrow(summary), "`"),
  "- Diagnostic reference method: `tcsp_mc`",
  "",
  "This directory binds the collected main validation results to the separate Young-Mathew add-on.",
  "Reference-width and oracle-shortest diagnostics are recomputed on the combined table."
)
writeLines(readme, file.path(staging, "README.md"))

artifact_files <- c(
  "bayes_uq_validation_results.csv",
  "bayes_uq_validation_summary.csv",
  "README.md"
)
artifact_hashes <- data.frame(
  file = artifact_files,
  sha256 = vapply(file.path(staging, artifact_files), digest::digest,
                  character(1L), algo = "sha256", file = TRUE),
  stringsAsFactors = FALSE
)
write.csv(artifact_hashes, file.path(staging, "artifact_hashes.csv"),
          row.names = FALSE)

manifest <- list(
  schema_version = "rqr_bayes_uq_validation/combined_young_mathew",
  main_manifest = jsonlite::read_json(file.path(main_run_dir, "manifest.json"),
                                      simplifyVector = FALSE),
  young_mathew_manifest = jsonlite::read_json(
    file.path(ym_run_dir, "manifest.json"), simplifyVector = FALSE
  ),
  output_dir = output_dir,
  n_result_rows = nrow(results),
  n_summary_rows = nrow(summary),
  diagnostic_reference_method_id = reference_method_id,
  young_mathew_rows = sum(results$method_id == "young_mathew"),
  created_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  artifact_hashes = artifact_hashes
)
jsonlite::write_json(manifest, file.path(staging, "manifest.json"),
                     pretty = TRUE, auto_unbox = TRUE)

if (!file.rename(staging, output_dir)) {
  stopf("Could not publish combined Bayesian-UQ validation output.")
}
published <- TRUE
cat("Combined Bayesian-UQ validation completed:", output_dir, "\n")
