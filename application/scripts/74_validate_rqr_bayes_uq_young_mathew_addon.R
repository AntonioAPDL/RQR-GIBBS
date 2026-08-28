#!/usr/bin/env Rscript

arguments <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_path <- sub(file_arg, "", arguments[startsWith(arguments, file_arg)][1L])
if (!length(script_path) || is.na(script_path)) {
  script_path <- "application/scripts/74_validate_rqr_bayes_uq_young_mathew_addon.R"
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
csv_values <- function(value) {
  if (is.null(value) || !nzchar(value)) return(NULL)
  trimws(strsplit(value, ",", fixed = TRUE)[[1L]])
}

for (package in c("jsonlite", "digest", "tolerance")) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stopf("Required package is not installed: ", package)
  }
}

mode <- tolower(arg_value("--mode=", "confirmatory"))
main_run_dir <- normalizePath(arg_value(
  "--main-run-dir=",
  file.path("application", "runs", "rqr_bayes_uq_validation_main_20260813",
            "wave_main_20260813T103232Z")
), winslash = "/", mustWork = TRUE)
config_path <- normalizePath(arg_value(
  "--config=", file.path(main_run_dir, "config_frozen.json")
), winslash = "/", mustWork = TRUE)
output_dir <- normalizePath(arg_value(
  "--output-dir=",
  file.path(main_run_dir, paste0(
    "young_mathew_addon_", format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
  ))
), winslash = "/", mustWork = FALSE)
workers <- as.integer(arg_value("--workers=", "8"))[1L]
replication_override <- arg_value("--replications=", NULL)
if (!is.finite(workers) || workers < 1L) stopf("--workers must be positive.")

filters <- list(
  dgp_ids = csv_values(arg_value("--dgp-ids=", NULL)),
  sample_sizes = csv_values(arg_value("--sample-sizes=", NULL)),
  guaranteed_contents = csv_values(arg_value("--guaranteed-contents=", NULL)),
  tolerance_confidences = csv_values(
    arg_value("--tolerance-confidences=", NULL)
  ),
  posterior_confidences = csv_values(
    arg_value("--posterior-confidences=", NULL)
  )
)

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

config <- jsonlite::read_json(config_path, simplifyVector = FALSE)
if (!mode %in% names(config$modes)) stopf("Mode not found in config: ", mode)
if (!isTRUE(config$execution[[paste0(mode, "_authorized")]])) {
  stopf("Bayesian uncertainty-validation mode is not authorized: ", mode)
}
mode_cfg <- config$modes[[mode]]
dgp_by_id <- setNames(config$dgps, vapply(config$dgps, `[[`,
                                         character(1L), "dgp_id"))

filter_character <- function(values, requested, label) {
  values <- as.character(values)
  if (is.null(requested)) return(values)
  keep <- values[values %in% requested]
  if (!length(keep)) stopf("Filter for ", label, " selected no values.")
  keep
}
filter_numeric <- function(values, requested, label) {
  values <- as.numeric(values)
  if (is.null(requested)) return(values)
  requested <- as.numeric(requested)
  if (any(!is.finite(requested))) {
    stopf("Filter for ", label, " contains a nonnumeric value.")
  }
  keep <- values[vapply(values, function(x) {
    any(abs(x - requested) <= 100 * .Machine$double.eps * max(1, abs(x)))
  }, logical(1L))]
  if (!length(keep)) stopf("Filter for ", label, " selected no values.")
  keep
}

mode_cfg$dgp_ids <- filter_character(mode_cfg$dgp_ids, filters$dgp_ids,
                                     "dgp_ids")
mode_cfg$sample_sizes <- filter_numeric(
  mode_cfg$sample_sizes, filters$sample_sizes, "sample_sizes"
)
mode_cfg$guaranteed_contents <- filter_numeric(
  mode_cfg$guaranteed_contents, filters$guaranteed_contents,
  "guaranteed_contents"
)
mode_cfg$tolerance_confidences <- filter_numeric(
  mode_cfg$tolerance_confidences, filters$tolerance_confidences,
  "tolerance_confidences"
)
mode_cfg$posterior_confidences <- filter_numeric(
  mode_cfg$posterior_confidences, filters$posterior_confidences,
  "posterior_confidences"
)
if (!is.null(replication_override)) {
  mode_cfg$replications <- as.integer(replication_override)[1L]
}
if (!is.finite(as.integer(mode_cfg$replications)) ||
    as.integer(mode_cfg$replications) < 1L) {
  stopf("The replication count must be positive.")
}

git_commit <- tryCatch(
  system2("git", c("rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE)[[1L]],
  error = function(e) NA_character_
)
script_sha256 <- digest::digest(script_path, algo = "sha256", file = TRUE)

hash_to_seed <- function(text, base = 862100L) {
  bytes <- as.integer(charToRaw(as.character(text)))
  value <- as.integer(base)
  for (byte in bytes) {
    value <- as.integer((as.double(value) * 131 + byte) %% 2147483647)
  }
  if (value <= 0L) value <- value + 1L
  value
}

dgp_meta <- function(dgp) {
  if (identical(dgp$family, "normal")) {
    return(list(r = function(n) stats::rnorm(n), p = stats::pnorm))
  }
  if (identical(dgp$family, "standardized_lognormal")) {
    logsd <- as.numeric(dgp$logsd %||% 0.75)[1L]
    mean_raw <- exp(logsd^2 / 2)
    sd_raw <- sqrt((exp(logsd^2) - 1) * exp(logsd^2))
    return(list(
      r = function(n) (stats::rlnorm(n, 0, logsd) - mean_raw) / sd_raw,
      p = function(x) stats::plnorm(x * sd_raw + mean_raw, 0, logsd)
    ))
  }
  if (identical(dgp$family, "standardized_normal_mixture")) {
    weights <- as.numeric(dgp$weights)
    means <- as.numeric(dgp$means)
    sds <- as.numeric(dgp$sds)
    mean_mix <- sum(weights * means)
    second <- sum(weights * (sds^2 + means^2))
    sd_mix <- sqrt(second - mean_mix^2)
    return(list(
      r = function(n) {
        comp <- sample.int(length(weights), n, replace = TRUE, prob = weights)
        (stats::rnorm(n, means[comp], sds[comp]) - mean_mix) / sd_mix
      },
      p = function(x) {
        raw <- x * sd_mix + mean_mix
        out <- numeric(length(raw))
        for (j in seq_along(weights)) {
          out <- out + weights[[j]] * stats::pnorm(raw, means[[j]], sds[[j]])
        }
        out
      }
    ))
  }
  if (identical(dgp$family, "standardized_student_t")) {
    df <- as.numeric(dgp$df %||% 3)[1L]
    if (!is.finite(df) || df <= 2) {
      stopf("standardized_student_t requires df > 2 for finite variance.")
    }
    sd_raw <- sqrt(df / (df - 2))
    return(list(
      r = function(n) stats::rt(n, df = df) / sd_raw,
      p = function(x) stats::pt(x * sd_raw, df = df)
    ))
  }
  stopf("Unsupported DGP family: ", dgp$family)
}

true_content <- function(lower, upper, cdf) {
  if (!is.finite(lower) || !is.finite(upper) || upper < lower) {
    return(NA_real_)
  }
  max(0, min(1, as.numeric(cdf(upper) - cdf(lower))))
}

base_seed <- as.integer(config$base_seed %||% 862100)
dataset_seed_for <- function(dgp_id, n, c_target, tol_conf, post_conf,
                             rep, counter) {
  if (isTRUE(mode_cfg$paired_thresholds)) {
    key <- paste("data", dgp_id, n, c_target, rep, sep = "|")
    return(hash_to_seed(key, base = base_seed))
  }
  base_seed + counter
}

extract_young_mathew <- function(raw) {
  raw_df <- as.data.frame(raw, check.names = FALSE)
  lower_cols <- grep("lower", names(raw_df), ignore.case = TRUE, value = TRUE)
  upper_cols <- grep("upper", names(raw_df), ignore.case = TRUE, value = TRUE)
  if (!length(lower_cols) || !length(upper_cols) || !nrow(raw_df)) {
    stopf("Could not identify two-sided interval columns in tolerance output.")
  }
  list(
    lower = as.numeric(raw_df[[lower_cols[[1L]]]][[1L]]),
    upper = as.numeric(raw_df[[upper_cols[[1L]]]][[1L]]),
    raw = raw_df,
    selected_row = 1L,
    output_rows = nrow(raw_df)
  )
}

fit_young_mathew <- function(y, c_target, tol_conf) {
  alpha <- 1 - tol_conf
  tryCatch({
    raw <- tolerance::nptol.int(
      x = y, alpha = alpha, P = c_target, side = 2, method = "YM"
    )
    interval <- extract_young_mathew(raw)
    lower <- interval$lower
    upper <- interval$upper
    if (!is.finite(lower) || !is.finite(upper) || upper < lower) {
      stopf("Young-Mathew interval is invalid.")
    }
    list(
      lower = lower,
      upper = upper,
      width = upper - lower,
      infeasible = FALSE,
      message = "",
      fit_class = "tolerance_nptol_int|young_mathew",
      target_audit_digest = digest::digest(
        list(
          package = "tolerance",
          package_version = as.character(utils::packageVersion("tolerance")),
          call = list(alpha = alpha, P = c_target, side = 2, method = "YM"),
          selected_row = interval$selected_row,
          output_rows = interval$output_rows,
          raw = interval$raw
        ),
        algo = "sha256", serialize = TRUE
      )
    )
  }, error = function(e) {
    list(
      lower = NA_real_,
      upper = NA_real_,
      width = NA_real_,
      infeasible = TRUE,
      message = conditionMessage(e),
      fit_class = "tolerance_nptol_int|young_mathew|error",
      target_audit_digest = NA_character_
    )
  })
}

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

write_progress <- function(status, current = list(), rows_completed = 0L) {
  total_rows <- length(mode_cfg$dgp_ids) *
    length(mode_cfg$sample_sizes) *
    length(mode_cfg$guaranteed_contents) *
    length(mode_cfg$tolerance_confidences) *
    length(mode_cfg$posterior_confidences) *
    as.integer(mode_cfg$replications)
  jsonlite::write_json(
    list(
      schema_version = paste0(config$schema_version, "/young_mathew_progress"),
      study_id = config$study_id,
      mode = mode,
      method_id = "young_mathew",
      status = status,
      main_run_dir = main_run_dir,
      output_dir = output_dir,
      git_commit = git_commit,
      script_sha256 = script_sha256,
      workers = workers,
      rows_completed = as.integer(rows_completed),
      rows_expected = as.integer(total_rows),
      rows_remaining = as.integer(total_rows - rows_completed),
      updated_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      current = current
    ),
    file.path(staging, "progress.json"),
    pretty = TRUE,
    auto_unbox = TRUE
  )
}

cell_grid <- expand.grid(
  dgp_id = as.character(mode_cfg$dgp_ids),
  n = as.integer(mode_cfg$sample_sizes),
  guaranteed_content = as.numeric(mode_cfg$guaranteed_contents),
  tolerance_confidence = as.numeric(mode_cfg$tolerance_confidences),
  replication = seq_len(as.integer(mode_cfg$replications)),
  stringsAsFactors = FALSE
)
cell_grid <- cell_grid[order(
  cell_grid$dgp_id, cell_grid$n, cell_grid$guaranteed_content,
  cell_grid$tolerance_confidence, cell_grid$replication
), , drop = FALSE]
write.csv(cell_grid, file.path(staging, "young_mathew_cell_plan.csv"),
          row.names = FALSE)
write_progress("running")

run_cell <- function(ii) {
  cell <- cell_grid[ii, , drop = FALSE]
  dgp_id <- cell$dgp_id[[1L]]
  n <- as.integer(cell$n[[1L]])
  c_target <- as.numeric(cell$guaranteed_content[[1L]])
  tol_conf <- as.numeric(cell$tolerance_confidence[[1L]])
  rep <- as.integer(cell$replication[[1L]])
  post_conf_first <- as.numeric(mode_cfg$posterior_confidences)[[1L]]
  seed <- dataset_seed_for(dgp_id, n, c_target, tol_conf, post_conf_first,
                           rep, ii)
  meta <- dgp_meta(dgp_by_id[[dgp_id]])
  set.seed(seed)
  y <- meta$r(n)
  timing <- system.time({
    fit <- fit_young_mathew(y, c_target = c_target, tol_conf = tol_conf)
  })
  content <- true_content(fit$lower, fit$upper, meta$p)
  success <- if (isTRUE(fit$infeasible) || is.na(content)) NA else
    content >= c_target - 1e-12
  rows <- lapply(as.numeric(mode_cfg$posterior_confidences), function(post_conf) {
    data.frame(
      mode = mode,
      dgp_id = dgp_id,
      n = n,
      guaranteed_content = c_target,
      tolerance_confidence = tol_conf,
      posterior_confidence = post_conf,
      replication = rep,
      seed = seed,
      method_id = "young_mathew",
      formal_tolerance_action = TRUE,
      response_likelihood = FALSE,
      generalized_bayes = FALSE,
      action_lane = "external_nonparametric_tolerance",
      selected_interval_source = "tolerance_nptol_int_YM",
      success = success,
      content = content,
      lower = fit$lower,
      upper = fit$upper,
      width = fit$width,
      formal_action_lower = fit$lower,
      formal_action_upper = fit$upper,
      formal_action_width = fit$width,
      formal_action_content = content,
      formal_action_success = success,
      fitted_summary_lower = NA_real_,
      fitted_summary_upper = NA_real_,
      fitted_summary_width = NA_real_,
      posterior_probability = NA_real_,
      retained_count = NA_integer_,
      retained_fraction = NA_real_,
      content_gap = content - c_target,
      posterior_threshold_excess = NA_real_,
      scan_critical_method = "young_mathew_package_nominal",
      content_buffer = NA_real_,
      scan_certified_lower_probability = NA_real_,
      posterior_constraint_status = NA_character_,
      candidate_feasible_count = NA_integer_,
      candidates_evaluated = NA_integer_,
      reference_method_id = NA_character_,
      reference_width = NA_real_,
      width_ratio_to_reference = NA_real_,
      width_diff_to_reference = NA_real_,
      posterior_constraint_binding = NA,
      oracle_target = NA_character_,
      oracle_mean_tilt = NA_real_,
      oracle_certificate_digest = NA_character_,
      oracle_lower_probability = NA_real_,
      oracle_upper_probability = NA_real_,
      oracle_sh_lower = NA_real_,
      oracle_sh_upper = NA_real_,
      oracle_sh_width = NA_real_,
      width_ratio_to_oracle_sh = NA_real_,
      width_excess_vs_oracle_sh = NA_real_,
      uq_engine = NA_character_,
      tilt_source = NA_character_,
      target_content = c_target,
      target_mean_tilt = NA_real_,
      target_audit_digest = fit$target_audit_digest,
      posterior_draws = NA_integer_,
      mcmc_n_burn = NA_integer_,
      mcmc_n_mcmc = NA_integer_,
      mcmc_thin = NA_integer_,
      ecm_converged = NA,
      ecm_iterations = NA_integer_,
      ecm_objective = NA_real_,
      fit_reused_across_posterior_thresholds = TRUE,
      infeasible = isTRUE(fit$infeasible),
      message = fit$message,
      fit_class = fit$fit_class,
      elapsed_sec = unname(timing[["elapsed"]]),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

indices <- seq_len(nrow(cell_grid))
if (workers > 1L && .Platform$OS.type == "unix") {
  result_list <- parallel::mclapply(
    indices, run_cell, mc.cores = min(workers, length(indices)),
    mc.preschedule = FALSE
  )
} else {
  result_list <- lapply(indices, run_cell)
}
results <- do.call(rbind, result_list)
results <- results[order(
  results$dgp_id, results$n, results$guaranteed_content,
  results$tolerance_confidence, results$posterior_confidence,
  results$replication
), , drop = FALSE]
write.csv(results, file.path(staging, "bayes_uq_validation_results.csv"),
          row.names = FALSE)

keys <- c("mode", "dgp_id", "n", "guaranteed_content",
          "tolerance_confidence", "posterior_confidence", "method_id")
split_key <- interaction(results[keys], drop = TRUE, lex.order = TRUE)
summary_rows <- lapply(split(results, split_key), function(df) {
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
  "# Young-Mathew Bayesian Uncertainty Add-On",
  "",
  paste0("- Main run directory: `", main_run_dir, "`"),
  paste0("- Mode: `", mode, "`"),
  paste0("- Method: `young_mathew` via `tolerance::nptol.int(method = \"YM\")`"),
  paste0("- Git commit: `", git_commit, "`"),
  paste0("- Script SHA-256: `", script_sha256, "`"),
  paste0("- Result rows: `", nrow(results), "`"),
  paste0("- Summary rows: `", nrow(summary), "`"),
  paste0("- Workers: `", workers, "`"),
  "",
  "This add-on uses the frozen Bayesian uncertainty confirmatory grid and the same paired dataset seed rule as the main validator.",
  "The Young-Mathew interval is treated as an external nonparametric tolerance comparator.",
  "Its package-level nominal construction is recorded as validation evidence, not as an independently audited finite-sample scan theorem."
)
writeLines(readme, file.path(staging, "README.md"))
write_progress("complete", rows_completed = nrow(results))

artifact_files <- c(
  "bayes_uq_validation_results.csv",
  "bayes_uq_validation_summary.csv",
  "young_mathew_cell_plan.csv",
  "progress.json",
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
  schema_version = paste0(config$schema_version, "/young_mathew_addon"),
  study_id = config$study_id,
  mode = mode,
  method_id = "young_mathew",
  main_run_dir = main_run_dir,
  config_path = config_path,
  output_dir = output_dir,
  git_commit = git_commit,
  script_sha256 = script_sha256,
  n_result_rows = nrow(results),
  n_summary_rows = nrow(summary),
  source_package = "tolerance",
  source_package_version = as.character(utils::packageVersion("tolerance")),
  tolerance_function = "nptol.int",
  tolerance_method = "YM",
  formal_tolerance_action = TRUE,
  response_likelihood = FALSE,
  generalized_bayes = FALSE,
  independently_audited_certificate = FALSE,
  paired_dataset_seed_rule = isTRUE(mode_cfg$paired_thresholds),
  workers = workers,
  created_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  artifact_hashes = artifact_hashes
)
jsonlite::write_json(manifest, file.path(staging, "manifest.json"),
                     pretty = TRUE, auto_unbox = TRUE)

if (!file.rename(staging, output_dir)) {
  stopf("Could not publish Young-Mathew add-on output.")
}
published <- TRUE
cat("Young-Mathew add-on completed:", output_dir, "\n")
