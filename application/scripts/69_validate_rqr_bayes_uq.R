#!/usr/bin/env Rscript

arguments <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_path <- sub(file_arg, "", arguments[startsWith(arguments, file_arg)][1L])
if (!length(script_path) || is.na(script_path)) {
  script_path <- "application/scripts/69_validate_rqr_bayes_uq.R"
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

for (package in c("rqrgibbs", "jsonlite", "digest")) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stopf("Required package is not installed: ", package)
  }
}
library(rqrgibbs)

mode <- tolower(arg_value("--mode=", "smoke"))
allowed <- c("smoke", "moderate", "health-check-read-only")
if (!mode %in% allowed) stopf("Unsupported Bayesian UQ validation mode: ", mode)

config_path <- normalizePath(arg_value(
  "--config=", file.path("application", "config",
                         "rqr_bayes_uq_validation_v1.json")
), winslash = "/", mustWork = TRUE)
config <- jsonlite::read_json(config_path, simplifyVector = FALSE)

if (identical(mode, "health-check-read-only")) {
  run_dir <- normalizePath(arg_value("--run-dir=", ""),
                           winslash = "/", mustWork = TRUE)
  required <- c(
    "bayes_uq_validation_results.csv",
    "bayes_uq_validation_summary.csv",
    "artifact_hashes.csv",
    "manifest.json",
    "README.md"
  )
  missing <- required[!file.exists(file.path(run_dir, required))]
  if (length(missing)) {
    stopf("Bayesian UQ run is missing artifact(s): ",
          paste(missing, collapse = ", "))
  }
  manifest <- jsonlite::read_json(file.path(run_dir, "manifest.json"),
                                  simplifyVector = TRUE)
  cat("Bayesian UQ validation health check passed:", run_dir, "\n")
  cat("Study:", manifest$study_id, "\n")
  cat("Mode:", manifest$mode, "\n")
  cat("Rows:", manifest$n_result_rows, "\n")
  quit(save = "no", status = 0L)
}

if (!mode %in% names(config$modes)) {
  stopf("Mode not found in config: ", mode)
}
if (!isTRUE(config$execution[[paste0(mode, "_authorized")]])) {
  stopf("Bayesian UQ validation mode is not authorized: ", mode)
}

default_output <- file.path(
  "application", "outputs", "rqr_bayes_uq_validation_v1",
  paste0(mode, "_", format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"))
)
output_dir <- normalizePath(arg_value("--output-dir=", default_output),
                            winslash = "/", mustWork = FALSE)
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

git_commit <- tryCatch(
  system2("git", c("rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE)[[1L]],
  error = function(e) NA_character_
)
mode_cfg <- config$modes[[mode]]
dgp_by_id <- setNames(config$dgps, vapply(config$dgps, `[[`,
                                         character(1L), "dgp_id"))
method_by_id <- setNames(config$methods, vapply(config$methods, `[[`,
                                               character(1L), "method_id"))

dgp_meta <- function(dgp) {
  if (identical(dgp$family, "normal")) {
    return(list(
      r = function(n) stats::rnorm(n),
      p = stats::pnorm
    ))
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
        rowSums(vapply(seq_along(weights), function(j) {
          weights[[j]] * stats::pnorm(raw, means[[j]], sds[[j]])
        }, numeric(length(raw))))
      }
    ))
  }
  stopf("Unsupported DGP family: ", dgp$family)
}

true_content <- function(lower, upper, cdf) {
  as.numeric(cdf(upper) - cdf(lower))
}

dp_base_from_config <- function() {
  base <- config$engine_defaults$direct_dp$base
  if (!identical(base$family, "normal")) {
    stopf("Unsupported direct-DP base family in config: ", base$family)
  }
  rqr_dp_base_normal(mean = as.numeric(base$mean)[1L],
                     sd = as.numeric(base$sd)[1L])
}

selected_interval <- function(selected) {
  if (is.null(selected) || !nrow(selected)) {
    return(list(lower = NA_real_, upper = NA_real_, width = NA_real_,
                posterior_probability = NA_real_, retained_count = NA_integer_,
                infeasible = TRUE))
  }
  list(
    lower = selected$lower[[1L]],
    upper = selected$upper[[1L]],
    width = selected$width[[1L]],
    posterior_probability =
      selected$posterior_content_probability[[1L]] %||% NA_real_,
    retained_count = selected$observed_count[[1L]] %||% NA_integer_,
    infeasible = FALSE
  )
}

fit_method <- function(method_id, y, c_target, tol_conf, post_conf, seed) {
  direct <- config$engine_defaults$direct_dp
  base <- dp_base_from_config()
  if (identical(method_id, "hdp_s")) {
    fit <- rqr_tcsp_hybrid_bayes_fit(
      y,
      guaranteed_content = c_target,
      tolerance_confidence = tol_conf,
      posterior_confidence = post_conf,
      distribution_engine = "direct_dp",
      scan_method = "dkw_conservative",
      distribution_args = list(
        concentration = as.numeric(direct$concentration)[1L],
        base_measure = base
      ),
      action_control = list(
        n_shortest_draws = 0
      )
    )
    return(list(
      lower = fit$formal_tolerance_action$lower_endpoint,
      upper = fit$formal_tolerance_action$upper_endpoint,
      width = fit$formal_tolerance_action$width,
      posterior_probability = fit$posterior_content_probability,
      retained_count = fit$formal_tolerance_action$retained_count,
      infeasible = is.na(fit$formal_tolerance_action$lower_endpoint),
      fit_class = paste(class(fit), collapse = "|")
    ))
  }
  if (identical(method_id, "dp_bayes")) {
    fit <- rqr_dp_fit(
      y,
      concentration = as.numeric(direct$concentration)[1L],
      base_measure = base
    )
    action <- rqr_dp_bayes_tolerance_action(
      fit, content = c_target, posterior_confidence = post_conf
    )
    out <- selected_interval(action$selected)
    out$fit_class <- paste(class(action), collapse = "|")
    return(out)
  }
  if (identical(method_id, "dpm_bayes")) {
    dpm <- config$engine_defaults$gaussian_dpm
    control <- if (identical(mode, "smoke")) {
      dpm$smoke_mcmc_control
    } else {
      dpm$moderate_mcmc_control
    }
    control$seed <- seed
    fit <- rqr_dpm_fit(
      y,
      truncation_level = as.integer(dpm$truncation_level)[1L],
      concentration = 1,
      mcmc_control = control
    )
    action <- rqr_dpm_bayes_tolerance_action(
      fit, content = c_target, posterior_confidence = post_conf
    )
    out <- selected_interval(action$selected)
    out$fit_class <- paste(class(action), collapse = "|")
    return(out)
  }
  if (identical(method_id, "bb_shortest_diag")) {
    bb <- config$engine_defaults$bayesian_bootstrap
    draws <- rqr_bayesian_bootstrap_draws(
      y, n_draws = as.integer(bb$draws)[1L], seed = seed
    )
    sh <- rqr_dp_shortest_draws(draws, target_content = c_target)
    lower <- stats::median(sh$lower)
    upper <- stats::median(sh$upper)
    return(list(
      lower = lower,
      upper = upper,
      width = upper - lower,
      posterior_probability = NA_real_,
      retained_count = NA_integer_,
      infeasible = FALSE,
      fit_class = "rqr_bayesian_bootstrap_draws|diagnostic"
    ))
  }
  if (identical(method_id, "tcsp_dkw")) {
    fit <- rqr_tcsp_plugin_fit_univariate(
      y, c_target, tol_conf, scan_method = "dkw_conservative"
    )
    return(list(
      lower = fit$contract$lower_endpoint,
      upper = fit$contract$upper_endpoint,
      width = fit$contract$width,
      posterior_probability = NA_real_,
      retained_count = fit$contract$retained_count,
      infeasible = FALSE,
      fit_class = paste(class(fit), collapse = "|")
    ))
  }
  if (identical(method_id, "split_empirical_shortest")) {
    fit <- rqr_tcsp_split_exact_fit(
      y,
      guaranteed_content = c_target,
      tolerance_confidence = tol_conf,
      pilot_fraction = as.numeric(config$engine_defaults$split$pilot_fraction)[1L],
      pilot_method = "empirical_shortest",
      split_seed = seed
    )
    return(list(
      lower = fit$contract$lower_endpoint,
      upper = fit$contract$upper_endpoint,
      width = fit$contract$width,
      posterior_probability = NA_real_,
      retained_count = fit$contract$retained_count %||% NA_integer_,
      infeasible = FALSE,
      fit_class = paste(class(fit), collapse = "|")
    ))
  }
  if (identical(method_id, "wilks_minmax")) {
    return(list(
      lower = min(y),
      upper = max(y),
      width = diff(range(y)),
      posterior_probability = NA_real_,
      retained_count = length(y),
      infeasible = FALSE,
      fit_class = "wilks_minmax_comparator"
    ))
  }
  stopf("Unsupported method_id: ", method_id)
}

rows <- list()
counter <- 0L
base_seed <- as.integer(config$base_seed %||% 862100)
for (dgp_id in as.character(mode_cfg$dgp_ids)) {
  dgp <- dgp_by_id[[dgp_id]]
  meta <- dgp_meta(dgp)
  for (n in as.integer(mode_cfg$sample_sizes)) {
    for (c_target in as.numeric(mode_cfg$guaranteed_contents)) {
      for (tol_conf in as.numeric(mode_cfg$tolerance_confidences)) {
        for (post_conf in as.numeric(mode_cfg$posterior_confidences)) {
          for (rep in seq_len(as.integer(mode_cfg$replications))) {
            counter <- counter + 1L
            seed <- base_seed + counter
            set.seed(seed)
            y <- meta$r(n)
            method_ids <- as.character(mode_cfg$method_ids)
            for (method_id in method_ids) {
              method_seed <- seed + match(method_id, method_ids) * 10000L
              method_meta <- method_by_id[[method_id]]
              timing <- system.time({
                fit <- tryCatch(
                  fit_method(
                    method_id, y, c_target, tol_conf, post_conf, method_seed
                  ),
                  error = function(e) e
                )
              })
              if (inherits(fit, "error")) {
                rows[[length(rows) + 1L]] <- data.frame(
                  mode = mode,
                  dgp_id = dgp_id,
                  n = n,
                  guaranteed_content = c_target,
                  tolerance_confidence = tol_conf,
                  posterior_confidence = post_conf,
                  replication = rep,
                  seed = seed,
                  method_id = method_id,
                  formal_tolerance_action =
                    isTRUE(method_meta$formal_tolerance_action),
                  response_likelihood = isTRUE(method_meta$response_likelihood),
                  generalized_bayes = isTRUE(method_meta$generalized_bayes),
                  success = NA,
                  content = NA_real_,
                  width = NA_real_,
                  posterior_probability = NA_real_,
                  retained_count = NA_integer_,
                  infeasible = TRUE,
                  message = conditionMessage(fit),
                  fit_class = "error",
                  elapsed_sec = unname(timing[["elapsed"]])
                )
              } else {
                content <- true_content(fit$lower, fit$upper, meta$p)
                rows[[length(rows) + 1L]] <- data.frame(
                  mode = mode,
                  dgp_id = dgp_id,
                  n = n,
                  guaranteed_content = c_target,
                  tolerance_confidence = tol_conf,
                  posterior_confidence = post_conf,
                  replication = rep,
                  seed = seed,
                  method_id = method_id,
                  formal_tolerance_action =
                    isTRUE(method_meta$formal_tolerance_action),
                  response_likelihood = isTRUE(method_meta$response_likelihood),
                  generalized_bayes = isTRUE(method_meta$generalized_bayes),
                  success = if (isTRUE(fit$infeasible)) NA else
                    content >= c_target - 1e-12,
                  content = content,
                  width = fit$width,
                  posterior_probability = fit$posterior_probability,
                  retained_count = as.integer(fit$retained_count),
                  infeasible = isTRUE(fit$infeasible),
                  message = "",
                  fit_class = fit$fit_class,
                  elapsed_sec = unname(timing[["elapsed"]])
                )
              }
            }
          }
        }
      }
    }
  }
}
results <- do.call(rbind, rows)
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
    mean_content = mean(df$content, na.rm = TRUE),
    median_width = stats::median(df$width, na.rm = TRUE),
    mean_width = mean(df$width, na.rm = TRUE),
    mean_posterior_probability =
      mean(df$posterior_probability, na.rm = TRUE),
    mean_elapsed_sec = mean(df$elapsed_sec, na.rm = TRUE)
  )
})
summary <- do.call(rbind, summary_rows)
write.csv(summary, file.path(staging, "bayes_uq_validation_summary.csv"),
          row.names = FALSE)

readme <- c(
  paste0("# ", config$study_id),
  "",
  paste0("- Mode: `", mode, "`"),
  paste0("- Git commit: `", git_commit, "`"),
  paste0("- Result rows: `", nrow(results), "`"),
  paste0("- Summary rows: `", nrow(summary), "`"),
  "",
  "This pilot separates response-distribution Bayesian UQ from RQR generalized-Bayes plug-in summaries.",
  "The hybrid direct-DP scan method fixes the scan count before evaluating posterior content probability.",
  "These artifacts are validation evidence only; they do not prove posterior endpoint coverage."
)
writeLines(readme, file.path(staging, "README.md"))

artifact_paths <- file.path(staging, c(
  "bayes_uq_validation_results.csv",
  "bayes_uq_validation_summary.csv",
  "README.md"
))
artifact_hashes <- data.frame(
  file = basename(artifact_paths),
  sha256 = vapply(artifact_paths, digest::digest, character(1L),
                  algo = "sha256", file = TRUE)
)
write.csv(artifact_hashes, file.path(staging, "artifact_hashes.csv"),
          row.names = FALSE)

manifest <- list(
  schema_version = config$schema_version,
  study_id = config$study_id,
  mode = mode,
  git_commit = git_commit,
  config_path = config_path,
  output_dir = output_dir,
  n_result_rows = nrow(results),
  n_summary_rows = nrow(summary),
  started_from_response_likelihood_engines = TRUE,
  generalized_bayes_plugin_comparators_present = TRUE,
  posterior_endpoint_coverage_claim_available = FALSE,
  scan_count_fixed_not_resampled = TRUE,
  created_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  artifact_hashes = artifact_hashes
)
jsonlite::write_json(
  manifest, file.path(staging, "manifest.json"),
  pretty = TRUE, auto_unbox = TRUE
)

if (!file.rename(staging, output_dir)) {
  stopf("Could not publish Bayesian UQ validation output.")
}
published <- TRUE
cat("Bayesian UQ validation", mode, "completed:", output_dir, "\n")
