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
allowed <- c("smoke", "moderate", "confirmatory", "dpm_companion",
             "health-check-read-only")
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
                infeasible = TRUE, posterior_constraint_status =
                  "infeasible_within_candidate_class",
                candidate_feasible_count = 0L,
                candidates_evaluated = NA_integer_))
  }
  list(
    lower = selected$lower[[1L]],
    upper = selected$upper[[1L]],
    width = selected$width[[1L]],
    posterior_probability =
      selected$posterior_content_probability[[1L]] %||% NA_real_,
    retained_count = selected$observed_count[[1L]] %||% NA_integer_,
    infeasible = FALSE,
    posterior_constraint_status = NA_character_,
    candidate_feasible_count = NA_integer_,
    candidates_evaluated = NA_integer_
  )
}

scan_method_for <- function(method_id) {
  method_meta <- method_by_id[[method_id]]
  configured <- method_meta$scan_method %||% NULL
  if (!is.null(configured)) return(as.character(configured)[1L])
  if (method_id %in% c("hdp_s_mc", "tcsp_mc")) {
    return("monte_carlo_conservative")
  }
  "dkw_conservative"
}

scan_args_for <- function(method_id, n, c_target, tol_conf) {
  method_meta <- method_by_id[[method_id]]
  scan_cfg <- config$scan_calibration %||% list()
  n_sim <- method_meta$scan_n_sim %||%
    mode_cfg$scan_n_sim %||%
    scan_cfg[[paste0(mode, "_n_sim")]] %||%
    scan_cfg$n_sim %||%
    20000L
  numerical_confidence <- method_meta$scan_numerical_confidence %||%
    mode_cfg$scan_numerical_confidence %||%
    scan_cfg[[paste0(mode, "_numerical_confidence")]] %||%
    scan_cfg$numerical_confidence %||%
    0.999
  seed_base <- as.integer(
    scan_cfg$seed %||% (as.integer(config$base_seed %||% 862100L) + 500000L)
  )
  key <- paste(mode, scan_method_for(method_id), n, c_target, tol_conf, n_sim,
               numerical_confidence, sep = "|")
  list(
    n_sim = as.integer(n_sim)[1L],
    numerical_confidence = as.numeric(numerical_confidence)[1L],
    seed = hash_to_seed(key, base = seed_base)
  )
}

scan_calibration_cache <- new.env(parent = emptyenv())
get_scan_calibration <- function(method_id, n, c_target, tol_conf) {
  method <- scan_method_for(method_id)
  args <- scan_args_for(method_id, n, c_target, tol_conf)
  key <- paste(method, n, c_target, tol_conf, args$n_sim,
               args$numerical_confidence, args$seed, sep = "|")
  if (!exists(key, envir = scan_calibration_cache, inherits = FALSE)) {
    cal <- rqr_tcsp_calibrate_count(
      n = n,
      guaranteed_content = c_target,
      tolerance_confidence = tol_conf,
      method = method,
      n_sim = args$n_sim,
      numerical_confidence = args$numerical_confidence,
      seed = args$seed
    )
    assign(key, cal, envir = scan_calibration_cache)
  }
  get(key, envir = scan_calibration_cache, inherits = FALSE)
}

dataset_seed_for <- function(dgp_id, n, c_target, tol_conf, post_conf,
                             rep, counter) {
  if (isTRUE(mode_cfg$paired_thresholds)) {
    key <- paste("data", dgp_id, n, c_target, rep, sep = "|")
    return(hash_to_seed(key, base = base_seed))
  }
  base_seed + counter
}

empty_fit_result <- function(
    lower = NA_real_, upper = NA_real_, width = NA_real_,
    posterior_probability = NA_real_, retained_count = NA_integer_,
    infeasible = FALSE, message = "", fit_class = NA_character_,
    scan_critical_method = NA_character_, content_buffer = NA_real_,
    scan_certified_lower_probability = NA_real_,
    posterior_constraint_status = NA_character_,
    candidate_feasible_count = NA_integer_,
    candidates_evaluated = NA_integer_) {
  list(
    lower = lower,
    upper = upper,
    width = width,
    posterior_probability = posterior_probability,
    retained_count = retained_count,
    infeasible = infeasible,
    message = message,
    fit_class = fit_class,
    scan_critical_method = scan_critical_method,
    content_buffer = content_buffer,
    scan_certified_lower_probability = scan_certified_lower_probability,
    posterior_constraint_status = posterior_constraint_status,
    candidate_feasible_count = candidate_feasible_count,
    candidates_evaluated = candidates_evaluated
  )
}

fit_method <- function(method_id, y, c_target, tol_conf, post_conf, seed) {
  direct <- config$engine_defaults$direct_dp
  base <- dp_base_from_config()
  if (method_id %in% c("hdp_s", "hdp_s_mc")) {
    calibration <- get_scan_calibration(method_id, length(y), c_target,
                                        tol_conf)
    if (isTRUE(calibration$infeasible) ||
        calibration$retained_count > length(y)) {
      return(empty_fit_result(
        infeasible = TRUE,
        message = "Hybrid Bayesian-scan calibration is infeasible for this sample size.",
        fit_class = "rqr_hybrid_bayes_tcsp_calibration_infeasible",
        scan_critical_method = calibration$scan_critical_method,
        content_buffer = calibration$content_buffer,
        retained_count = calibration$retained_count,
        scan_certified_lower_probability =
          calibration$scan_probability$certified_lower_probability %||%
            NA_real_,
        posterior_constraint_status = "infeasible_scan_count"
      ))
    }
    fit <- rqr_tcsp_hybrid_bayes_fit(
      y,
      guaranteed_content = c_target,
      tolerance_confidence = tol_conf,
      posterior_confidence = post_conf,
      distribution_engine = "direct_dp",
      scan_method = scan_method_for(method_id),
      distribution_args = list(
        concentration = as.numeric(direct$concentration)[1L],
        base_measure = base
      ),
      scan_args = list(calibration = calibration),
      action_control = list(
        n_shortest_draws = 0
      )
    )
    return(empty_fit_result(
      lower = fit$formal_tolerance_action$lower_endpoint,
      upper = fit$formal_tolerance_action$upper_endpoint,
      width = fit$formal_tolerance_action$width,
      posterior_probability = fit$posterior_content_probability,
      retained_count = fit$formal_tolerance_action$retained_count,
      infeasible = is.na(fit$formal_tolerance_action$lower_endpoint),
      fit_class = paste(class(fit), collapse = "|"),
      scan_critical_method =
        fit$scan_contract$calibration$scan_critical_method,
      content_buffer = fit$scan_contract$calibration$content_buffer,
      scan_certified_lower_probability =
        fit$scan_contract$calibration$scan_probability$
          certified_lower_probability %||% NA_real_,
      posterior_constraint_status = fit$posterior_constraint_status,
      candidate_feasible_count =
        fit$hybrid_bayesian_scan_action$feasible_count %||% NA_integer_,
      candidates_evaluated =
        fit$hybrid_bayesian_scan_action$candidates_evaluated %||% NA_integer_
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
    out$posterior_constraint_status <- action$posterior_constraint_status
    out$candidate_feasible_count <- action$feasible_count
    out$candidates_evaluated <- action$candidates_evaluated
    out$fit_class <- paste(class(action), collapse = "|")
    return(out)
  }
  if (identical(method_id, "dpm_bayes")) {
    dpm <- config$engine_defaults$gaussian_dpm
    control <- dpm[[paste0(mode, "_mcmc_control")]] %||%
      dpm$moderate_mcmc_control
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
    out$posterior_constraint_status <- action$posterior_constraint_status
    out$candidate_feasible_count <- action$feasible_count
    out$candidates_evaluated <- action$candidates_evaluated
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
  if (method_id %in% c("tcsp_dkw", "tcsp_mc")) {
    calibration <- get_scan_calibration(method_id, length(y), c_target,
                                        tol_conf)
    if (isTRUE(calibration$infeasible) ||
        calibration$retained_count > length(y)) {
      return(empty_fit_result(
        infeasible = TRUE,
        message = "TCSP calibration is infeasible for this sample size and target.",
        fit_class = "rqr_tcsp_calibration_infeasible",
        scan_critical_method = calibration$scan_critical_method,
        content_buffer = calibration$content_buffer,
        retained_count = calibration$retained_count,
        scan_certified_lower_probability =
          calibration$scan_probability$certified_lower_probability %||%
            NA_real_
      ))
    }
    window <- rqr_tcsp_shortest_window(
      y, retained_count = calibration$retained_count, na_rm = FALSE
    )
    return(empty_fit_result(
      lower = window$lower_endpoint,
      upper = window$upper_endpoint,
      width = window$width,
      posterior_probability = NA_real_,
      retained_count = calibration$retained_count,
      infeasible = FALSE,
      fit_class = "rqr_tcsp_shortest_window|cached_calibration",
      scan_critical_method = calibration$scan_critical_method,
      content_buffer = calibration$content_buffer,
      scan_certified_lower_probability =
        calibration$scan_probability$certified_lower_probability %||% NA_real_
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
total_datasets <- length(mode_cfg$dgp_ids) *
  length(mode_cfg$sample_sizes) *
  length(mode_cfg$guaranteed_contents) *
  length(mode_cfg$tolerance_confidences) *
  length(mode_cfg$posterior_confidences) *
  as.integer(mode_cfg$replications)
total_rows <- total_datasets * length(mode_cfg$method_ids)
progress_path <- file.path(staging, "progress.json")
partial_results_path <- file.path(staging, "partial_results.csv")
checkpoint_every_datasets <- as.integer(
  mode_cfg$checkpoint_every_datasets %||%
    config$execution$checkpoint_every_datasets %||%
    0L
)[1L]
write_partial_results <- function() {
  if (!length(rows)) return(invisible(FALSE))
  write.csv(do.call(rbind, rows), partial_results_path, row.names = FALSE)
  invisible(TRUE)
}
write_progress <- function(status, current = list()) {
  jsonlite::write_json(
    list(
      schema_version = paste0(config$schema_version, "/progress"),
      study_id = config$study_id,
      mode = mode,
      status = status,
      git_commit = git_commit,
      datasets_completed = as.integer(counter),
      total_datasets = as.integer(total_datasets),
      rows_completed = as.integer(length(rows)),
      total_rows = as.integer(total_rows),
      rows_remaining = as.integer(total_rows - length(rows)),
      checkpoint_path = if (file.exists(partial_results_path))
        partial_results_path else NA_character_,
      updated_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      current = current
    ),
    progress_path,
    pretty = TRUE,
    auto_unbox = TRUE
  )
}
write_progress("running")
for (dgp_id in as.character(mode_cfg$dgp_ids)) {
  dgp <- dgp_by_id[[dgp_id]]
  meta <- dgp_meta(dgp)
  for (n in as.integer(mode_cfg$sample_sizes)) {
    for (c_target in as.numeric(mode_cfg$guaranteed_contents)) {
      for (tol_conf in as.numeric(mode_cfg$tolerance_confidences)) {
        for (post_conf in as.numeric(mode_cfg$posterior_confidences)) {
          for (rep in seq_len(as.integer(mode_cfg$replications))) {
            counter <- counter + 1L
            seed <- dataset_seed_for(dgp_id, n, c_target, tol_conf, post_conf,
                                     rep, counter)
            set.seed(seed)
            y <- meta$r(n)
            method_ids <- as.character(mode_cfg$method_ids)
            for (method_id in method_ids) {
              method_seed <- hash_to_seed(
                paste("method", seed, method_id, tol_conf, post_conf, sep = "|"),
                base = base_seed + 10000L
              )
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
                  retained_fraction = NA_real_,
                  content_gap = NA_real_,
                  posterior_threshold_excess = NA_real_,
                  scan_critical_method = NA_character_,
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
                  retained_fraction =
                    as.numeric(fit$retained_count %||% NA_real_) / n,
                  content_gap = content - c_target,
                  posterior_threshold_excess =
                    as.numeric(fit$posterior_probability %||% NA_real_) -
                      post_conf,
                  scan_critical_method =
                    fit$scan_critical_method %||% NA_character_,
                  content_buffer = fit$content_buffer %||% NA_real_,
                  scan_certified_lower_probability =
                    fit$scan_certified_lower_probability %||% NA_real_,
                  posterior_constraint_status =
                    fit$posterior_constraint_status %||% NA_character_,
                  candidate_feasible_count =
                    as.integer(fit$candidate_feasible_count %||%
                                 NA_integer_),
                  candidates_evaluated =
                    as.integer(fit$candidates_evaluated %||% NA_integer_),
                  reference_method_id = NA_character_,
                  reference_width = NA_real_,
                  width_ratio_to_reference = NA_real_,
                  width_diff_to_reference = NA_real_,
                  posterior_constraint_binding = NA,
                  infeasible = isTRUE(fit$infeasible),
                  message = fit$message %||% "",
                  fit_class = fit$fit_class,
                  elapsed_sec = unname(timing[["elapsed"]])
                )
              }
            }
            if (checkpoint_every_datasets > 0L &&
                counter %% checkpoint_every_datasets == 0L) {
              write_partial_results()
            }
            write_progress("running", current = list(
              dgp_id = dgp_id,
              n = n,
              guaranteed_content = c_target,
              tolerance_confidence = tol_conf,
              posterior_confidence = post_conf,
              replication = rep
            ))
          }
        }
      }
    }
}
}
results <- do.call(rbind, rows)
reference_method_id <- mode_cfg$reference_method_id %||%
  config$diagnostics$reference_method_id %||%
  if ("tcsp_mc" %in% as.character(mode_cfg$method_ids)) {
    "tcsp_mc"
  } else {
    "tcsp_dkw"
  }
results$row_id <- seq_len(nrow(results))
reference_keys <- c("mode", "dgp_id", "n", "guaranteed_content",
                    "tolerance_confidence", "posterior_confidence",
                    "replication", "seed")
reference_rows <- results[results$method_id == reference_method_id,
                          c(reference_keys, "width"), drop = FALSE]
if (nrow(reference_rows)) {
  names(reference_rows)[names(reference_rows) == "width"] <-
    "reference_width"
  results <- merge(
    results,
    reference_rows,
    by = reference_keys,
    all.x = TRUE,
    sort = FALSE,
    suffixes = c("", ".diagnostic_reference")
  )
  if ("reference_width.diagnostic_reference" %in% names(results)) {
    results$reference_width <- results$reference_width.diagnostic_reference
    results$reference_width.diagnostic_reference <- NULL
  }
  results <- results[order(results$row_id), , drop = FALSE]
  results$reference_method_id <- reference_method_id
  results$width_ratio_to_reference <- ifelse(
    is.finite(results$reference_width) & results$reference_width > 0,
    results$width / results$reference_width,
    NA_real_
  )
  results$width_diff_to_reference <- results$width - results$reference_width
  is_hybrid <- results$method_id %in% c("hdp_s", "hdp_s_mc")
  results$posterior_constraint_binding <- ifelse(
    is_hybrid,
    results$posterior_constraint_status == "binding",
    NA
  )
}
results$row_id <- NULL
write.csv(results, file.path(staging, "bayes_uq_validation_results.csv"),
          row.names = FALSE)
if (file.exists(partial_results_path)) unlink(partial_results_path)

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
    mean_content = mean(df$content, na.rm = TRUE),
    mean_content_gap = mean(df$content_gap, na.rm = TRUE),
    median_width = stats::median(df$width, na.rm = TRUE),
    mean_width = mean(df$width, na.rm = TRUE),
    median_width_ratio_to_reference =
      stats::median(df$width_ratio_to_reference, na.rm = TRUE),
    mean_width_ratio_to_reference =
      mean(df$width_ratio_to_reference, na.rm = TRUE),
    median_width_diff_to_reference =
      stats::median(df$width_diff_to_reference, na.rm = TRUE),
    mean_retained_fraction = mean(df$retained_fraction, na.rm = TRUE),
    mean_content_buffer = mean(df$content_buffer, na.rm = TRUE),
    mean_scan_certified_lower_probability =
      mean(df$scan_certified_lower_probability, na.rm = TRUE),
    mean_posterior_probability =
      mean(df$posterior_probability, na.rm = TRUE),
    mean_posterior_threshold_excess =
      mean(df$posterior_threshold_excess, na.rm = TRUE),
    posterior_binding_rate =
      mean(df$posterior_constraint_binding, na.rm = TRUE),
    mean_candidate_feasible_count =
      mean(df$candidate_feasible_count, na.rm = TRUE),
    mean_candidates_evaluated =
      mean(df$candidates_evaluated, na.rm = TRUE),
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
  paste0("- Diagnostic reference method: `", reference_method_id, "`"),
  "",
  "This pilot separates response-distribution Bayesian UQ from RQR generalized-Bayes plug-in summaries.",
  "The hybrid direct-DP scan method fixes the scan count before evaluating posterior content probability.",
  "These artifacts are validation evidence only; they do not prove posterior endpoint coverage.",
  "Width-ratio and posterior-binding diagnostics are included for post-run method tuning."
)
writeLines(readme, file.path(staging, "README.md"))
write_progress("complete")

artifact_paths <- file.path(staging, c(
  "bayes_uq_validation_results.csv",
  "bayes_uq_validation_summary.csv",
  "progress.json",
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
  diagnostic_reference_method_id = reference_method_id,
  checkpoint_every_datasets = checkpoint_every_datasets,
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
