#!/usr/bin/env Rscript

arguments <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_path <- sub(file_arg, "", arguments[startsWith(arguments, file_arg)][1L])
if (!length(script_path) || is.na(script_path)) {
  script_path <- "application/scripts/71_manage_rqr_bayes_uq_main_waves.R"
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

action <- tolower(arg_value("--action=", "health"))
allowed_actions <- c("prepare", "launch", "health", "collect")
if (!action %in% allowed_actions) {
  stopf("Unsupported Bayesian UQ wave action: ", action)
}
mode <- tolower(arg_value("--mode=", "confirmatory"))
config_path <- normalizePath(arg_value(
  "--config=", file.path("application", "config",
                         "rqr_bayes_uq_validation_main_20260813.json")
), winslash = "/", mustWork = TRUE)
run_root <- normalizePath(arg_value(
  "--run-root=", file.path("application", "runs",
                           "rqr_bayes_uq_validation_main_20260813")
), winslash = "/", mustWork = FALSE)
run_id <- arg_value(
  "--run-id=", paste0("wave_main_", format(Sys.time(), "%Y%m%dT%H%M%SZ",
                                           tz = "UTC"))
)
run_dir_arg <- arg_value("--run-dir=", NULL)
max_concurrent <- as.integer(arg_value("--max-concurrent=", "6"))[1L]
poll_seconds <- as.integer(arg_value("--poll-seconds=", "60"))[1L]
require_clean <- !identical(tolower(arg_value("--require-clean=", "true")),
                            "false")

if (!is.finite(max_concurrent) || max_concurrent < 1L) {
  stopf("--max-concurrent must be a positive integer.")
}
if (!is.finite(poll_seconds) || poll_seconds < 5L) {
  stopf("--poll-seconds must be at least 5.")
}

config <- jsonlite::read_json(config_path, simplifyVector = FALSE)
if (!mode %in% names(config$modes)) stopf("Mode not found in config: ", mode)
mode_cfg <- config$modes[[mode]]
dgp_by_id <- setNames(config$dgps, vapply(config$dgps, `[[`,
                                         character(1L), "dgp_id"))
method_by_id <- setNames(config$methods, vapply(config$methods, `[[`,
                                               character(1L), "method_id"))
git_commit <- tryCatch(
  system2("git", c("rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE)[[1L]],
  error = function(e) NA_character_
)

sha256_file <- function(path) {
  digest::digest(path, algo = "sha256", file = TRUE)
}

hash_to_seed <- function(text, base = 862100L) {
  bytes <- as.integer(charToRaw(as.character(text)))
  value <- as.integer(base)
  for (byte in bytes) {
    value <- as.integer((as.double(value) * 131 + byte) %% 2147483647)
  }
  if (value <= 0L) value <- value + 1L
  value
}

safe_slug <- function(x) {
  x <- gsub("[^A-Za-z0-9]+", "_", as.character(x))
  x <- gsub("^_+|_+$", "", x)
  tolower(x)
}

format_probability <- function(x) {
  gsub("\\.", "", sprintf("%.3f", as.numeric(x)))
}

scan_method_for <- function(method_id) {
  method_meta <- method_by_id[[method_id]]
  configured <- method_meta$scan_method %||% NULL
  if (!is.null(configured)) return(as.character(configured)[1L])
  if (method_id %in% c("hdp_s_mc", "tcsp_mc")) {
    return("monte_carlo_conservative")
  }
  if (method_id %in% c("tcsp_dkw")) return("dkw_conservative")
  NA_character_
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

scan_cache_key <- function(method_id, n, c_target, tol_conf) {
  method <- scan_method_for(method_id)
  args <- scan_args_for(method_id, n, c_target, tol_conf)
  paste(method, n, c_target, tol_conf, args$n_sim, args$numerical_confidence,
        args$seed, sep = "|")
}

calibrate_safely <- function(method_id, n, c_target, tol_conf) {
  method <- scan_method_for(method_id)
  args <- scan_args_for(method_id, n, c_target, tol_conf)
  tryCatch(
    rqr_tcsp_calibrate_count(
      n = n,
      guaranteed_content = c_target,
      tolerance_confidence = tol_conf,
      method = method,
      n_sim = args$n_sim,
      numerical_confidence = args$numerical_confidence,
      seed = args$seed
    ),
    error = function(e) {
      list(
        schema_version = paste0(config$schema_version, "/scan_calibration"),
        method = "scan_calibrated_tcsp_mt_rqr",
        scan_critical_method = method,
        n = as.integer(n),
        guaranteed_content = as.numeric(c_target),
        tolerance_confidence = as.numeric(tol_conf),
        retained_count = as.integer(n + 1L),
        target_content = NA_real_,
        content_buffer = NA_real_,
        scan_probability = list(
          certified_lower_probability = NA_real_,
          numerical_confidence = args$numerical_confidence,
          n_sim = args$n_sim
        ),
        finite_sample_claim_available = FALSE,
        asymptotic_claim_available = FALSE,
        infeasible = TRUE,
        message = conditionMessage(e)
      )
    }
  )
}

oracle_spec_from_dgp <- function(dgp) {
  if (identical(dgp$family, "normal")) {
    return(list(family = "gaussian", params = list(mean = 0, sd = 1)))
  }
  if (identical(dgp$family, "standardized_lognormal")) {
    return(list(
      family = "centered_standardized_lognormal",
      params = list(logmean = 0, logsd = as.numeric(dgp$logsd %||% 0.75)[1L])
    ))
  }
  if (identical(dgp$family, "standardized_normal_mixture")) {
    weights <- as.numeric(dgp$weights)
    weights <- weights / sum(weights)
    means <- as.numeric(dgp$means)
    sds <- as.numeric(dgp$sds)
    mean_mix <- sum(weights * means)
    second <- sum(weights * (sds^2 + means^2))
    sd_mix <- sqrt(second - mean_mix^2)
    return(list(
      family = "gaussian_mixture",
      params = list(
        weights = weights,
        means = (means - mean_mix) / sd_mix,
        sds = sds / sd_mix,
        center = FALSE
      )
    ))
  }
  if (identical(dgp$family, "standardized_student_t")) {
    df <- as.numeric(dgp$df %||% 3)[1L]
    return(list(
      family = "student_t",
      params = list(df = df, scale = sqrt((df - 2) / df))
    ))
  }
  stopf("Unsupported oracle DGP family: ", dgp$family)
}

oracle_key <- function(dgp_id, c_target) {
  paste(dgp_id, formatC(c_target, digits = 12, format = "fg"), sep = "|")
}

build_wave_plan <- function() {
  out <- list()
  index <- 0L
  for (dgp_id in as.character(mode_cfg$dgp_ids)) {
    for (n in as.integer(mode_cfg$sample_sizes)) {
      for (c_target in as.numeric(mode_cfg$guaranteed_contents)) {
        for (tol_conf in as.numeric(mode_cfg$tolerance_confidences)) {
          index <- index + 1L
          wave_id <- sprintf(
            "w%03d_%s_n%04d_c%s_t%s",
            index, safe_slug(dgp_id), n, format_probability(c_target),
            format_probability(tol_conf)
          )
          expected_datasets <- length(mode_cfg$posterior_confidences) *
            as.integer(mode_cfg$replications)
          out[[index]] <- data.frame(
            wave_id = wave_id,
            mode = mode,
            dgp_id = dgp_id,
            n = n,
            guaranteed_content = c_target,
            tolerance_confidence = tol_conf,
            posterior_confidences = paste(
              as.numeric(mode_cfg$posterior_confidences), collapse = ";"
            ),
            replications = as.integer(mode_cfg$replications),
            method_count = length(mode_cfg$method_ids),
            expected_datasets = expected_datasets,
            expected_result_rows =
              expected_datasets * length(mode_cfg$method_ids),
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  do.call(rbind, out)
}

run_dir <- if (!is.null(run_dir_arg) && nzchar(run_dir_arg)) {
  normalizePath(run_dir_arg, winslash = "/", mustWork = FALSE)
} else {
  file.path(run_root, run_id)
}

wave_output_dir <- function(run_dir, wave_id) {
  file.path(run_dir, "waves", wave_id)
}

wave_log_file <- function(run_dir, wave_id) {
  file.path(run_dir, "logs", paste0(wave_id, ".log"))
}

wave_pid_file <- function(run_dir, wave_id) {
  file.path(run_dir, "pids", paste0(wave_id, ".pid"))
}

pid_alive <- function(pid) {
  pid <- suppressWarnings(as.integer(pid)[1L])
  if (!is.finite(pid) || pid <= 0L) return(FALSE)
  identical(system2("kill", c("-0", as.character(pid)),
                    stdout = FALSE, stderr = FALSE), 0L)
}

read_wave_plan <- function(run_dir) {
  utils::read.csv(file.path(run_dir, "wave_plan.csv"),
                  stringsAsFactors = FALSE, check.names = FALSE)
}

latest_wave_progress <- function(run_dir, wave_id) {
  candidates <- Sys.glob(file.path(run_dir, "waves", paste0(".", wave_id, "-*"),
                                   "progress.json"))
  if (!length(candidates)) return(NULL)
  info <- file.info(candidates)
  path <- candidates[[which.max(info$mtime)]]
  tryCatch(
    jsonlite::read_json(path, simplifyVector = TRUE),
    error = function(e) NULL
  )
}

wave_status <- function(run_dir) {
  plan <- read_wave_plan(run_dir)
  rows <- lapply(seq_len(nrow(plan)), function(ii) {
    wave <- plan[ii, , drop = FALSE]
    output_dir <- wave_output_dir(run_dir, wave$wave_id)
    manifest_path <- file.path(output_dir, "manifest.json")
    pid_path <- wave_pid_file(run_dir, wave$wave_id)
    pid <- if (file.exists(pid_path)) {
      pid_lines <- readLines(pid_path, warn = FALSE)
      if (length(pid_lines)) pid_lines[[1L]] else NA_character_
    } else {
      NA_character_
    }
    complete <- file.exists(manifest_path)
    running <- !complete && !is.na(pid) && pid_alive(pid)
    status <- if (complete) {
      "complete"
    } else if (running) {
      "running"
    } else if (!is.na(pid)) {
      "failed"
    } else {
      "pending"
    }
    progress <- latest_wave_progress(run_dir, wave$wave_id)
    datasets_completed <- if (!is.null(progress)) {
      as.integer(progress$datasets_completed %||% 0L)
    } else if (complete) {
      as.integer(wave$expected_datasets)
    } else {
      0L
    }
    rows_completed <- if (complete) {
      manifest <- jsonlite::read_json(manifest_path, simplifyVector = TRUE)
      as.integer(manifest$n_result_rows)
    } else if (!is.null(progress)) {
      as.integer(progress$rows_completed %||% 0L)
    } else {
      0L
    }
    data.frame(
      wave_id = wave$wave_id,
      dgp_id = wave$dgp_id,
      n = wave$n,
      guaranteed_content = wave$guaranteed_content,
      tolerance_confidence = wave$tolerance_confidence,
      status = status,
      pid = pid,
      datasets_completed = datasets_completed,
      expected_datasets = as.integer(wave$expected_datasets),
      rows_completed = rows_completed,
      expected_result_rows = as.integer(wave$expected_result_rows),
      progress_updated_at_utc =
        if (!is.null(progress)) progress$updated_at_utc %||% NA_character_
        else NA_character_,
      output_dir = output_dir,
      log_file = wave_log_file(run_dir, wave$wave_id),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

write_health <- function(run_dir) {
  status <- wave_status(run_dir)
  total_rows <- sum(status$expected_result_rows)
  rows_completed <- sum(status$rows_completed)
  counts <- table(factor(
    status$status, levels = c("complete", "running", "pending", "failed")
  ))
  health <- list(
    schema_version = paste0(config$schema_version, "/wave_health"),
    study_id = config$study_id,
    mode = mode,
    run_dir = run_dir,
    git_commit = git_commit,
    waves_total = nrow(status),
    waves_complete = unname(counts[["complete"]]),
    waves_running = unname(counts[["running"]]),
    waves_pending = unname(counts[["pending"]]),
    waves_failed = unname(counts[["failed"]]),
    datasets_completed = sum(status$datasets_completed),
    datasets_expected = sum(status$expected_datasets),
    rows_completed = rows_completed,
    rows_expected = total_rows,
    rows_remaining = total_rows - rows_completed,
    final_artifacts_present = file.exists(file.path(run_dir, "manifest.json")),
    updated_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
  utils::write.csv(status, file.path(run_dir, "wave_status.csv"),
                   row.names = FALSE)
  jsonlite::write_json(health, file.path(run_dir, "health.json"),
                       pretty = TRUE, auto_unbox = TRUE)
  cat(sprintf(
    "Bayesian UQ waves: complete=%d running=%d pending=%d failed=%d rows=%d/%d\n",
    health$waves_complete, health$waves_running, health$waves_pending,
    health$waves_failed, health$rows_completed, health$rows_expected
  ))
  invisible(health)
}

prepare_run <- function() {
  if (require_clean) {
    git_status <- system2("git", c("status", "--short"),
                          stdout = TRUE, stderr = FALSE)
    if (length(git_status)) {
      stopf("Refusing to prepare from a dirty source tree:\n",
            paste(git_status, collapse = "\n"))
    }
  }
  if (file.exists(run_dir) || dir.exists(run_dir)) {
    stopf("The wave run directory must be fresh: ", run_dir)
  }
  dir.create(file.path(run_dir, "waves"), recursive = TRUE)
  dir.create(file.path(run_dir, "logs"), recursive = TRUE)
  dir.create(file.path(run_dir, "pids"), recursive = TRUE)

  frozen_config <- file.path(run_dir, "config_frozen.json")
  file.copy(config_path, frozen_config)
  wave_plan <- build_wave_plan()
  utils::write.csv(wave_plan, file.path(run_dir, "wave_plan.csv"),
                   row.names = FALSE)

  scan_method_ids <- as.character(mode_cfg$method_ids)
  scan_method_ids <- scan_method_ids[
    !is.na(vapply(scan_method_ids, scan_method_for, character(1L)))
  ]
  calibrations <- list()
  calibration_rows <- list()
  calibration_index <- 0L
  for (method_id in scan_method_ids) {
    for (n in unique(as.integer(mode_cfg$sample_sizes))) {
      for (c_target in unique(as.numeric(mode_cfg$guaranteed_contents))) {
        for (tol_conf in unique(as.numeric(mode_cfg$tolerance_confidences))) {
          key <- scan_cache_key(method_id, n, c_target, tol_conf)
          if (key %in% names(calibrations)) next
          calibration <- calibrate_safely(method_id, n, c_target, tol_conf)
          calibrations[[key]] <- calibration
          calibration_index <- calibration_index + 1L
          calibration_rows[[calibration_index]] <- data.frame(
            cache_key = key,
            method = calibration$scan_critical_method,
            n = n,
            guaranteed_content = c_target,
            tolerance_confidence = tol_conf,
            retained_count = as.integer(calibration$retained_count),
            content_buffer = calibration$content_buffer %||% NA_real_,
            certified_lower_probability =
              calibration$scan_probability$certified_lower_probability %||%
              NA_real_,
            infeasible = isTRUE(calibration$infeasible),
            message = calibration$message %||% "",
            calibration_digest = digest::digest(calibration, algo = "sha256",
                                                serialize = TRUE),
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  calibration_table <- do.call(rbind, calibration_rows)
  utils::write.csv(calibration_table,
                   file.path(run_dir, "scan_calibration_summary.csv"),
                   row.names = FALSE)
  saveRDS(
    list(
      schema_version = paste0(config$schema_version, "/scan_cache"),
      mode = mode,
      git_commit = git_commit,
      calibrations = calibrations,
      summary = calibration_table
    ),
    file.path(run_dir, "scan_calibration_cache.rds")
  )

  certificates <- list()
  oracle_rows <- list()
  oracle_index <- 0L
  for (dgp_id in unique(as.character(mode_cfg$dgp_ids))) {
    dgp <- dgp_by_id[[dgp_id]]
    spec <- oracle_spec_from_dgp(dgp)
    for (c_target in unique(as.numeric(mode_cfg$guaranteed_contents))) {
      key <- oracle_key(dgp_id, c_target)
      certificate <- rqr_interval_oracle(
        family = spec$family,
        coverage_level = c_target,
        target = "SH",
        params = spec$params,
        tol = as.numeric((config$oracle %||% list())$tol %||% 1e-10)[1L],
        grid_size = as.integer(
          (config$oracle %||% list())$grid_size %||% 1601L
        )[1L]
      )
      certificates[[key]] <- certificate
      oracle_index <- oracle_index + 1L
      oracle_rows[[oracle_index]] <- data.frame(
        cache_key = key,
        dgp_id = dgp_id,
        guaranteed_content = c_target,
        family = spec$family,
        lower = certificate$lower_root,
        upper = certificate$upper_root,
        width = certificate$width,
        lower_probability = certificate$lower_probability,
        upper_probability = certificate$upper_probability,
        mean_tilt = certificate$mean_tilt,
        content_residual = certificate$content_residual,
        density_residual = certificate$density_residual,
        unique_minimizer = isTRUE(certificate$unique_minimizer),
        certificate_digest = certificate$certificate_digest,
        stringsAsFactors = FALSE
      )
    }
  }
  oracle_table <- do.call(rbind, oracle_rows)
  utils::write.csv(oracle_table, file.path(run_dir, "oracle_reference.csv"),
                   row.names = FALSE)
  saveRDS(
    list(
      schema_version = paste0(config$schema_version, "/oracle_cache"),
      mode = mode,
      git_commit = git_commit,
      certificates = certificates,
      summary = oracle_table
    ),
    file.path(run_dir, "oracle_cache.rds")
  )

  artifact_files <- c(
    "config_frozen.json", "wave_plan.csv", "scan_calibration_summary.csv",
    "scan_calibration_cache.rds", "oracle_reference.csv", "oracle_cache.rds"
  )
  artifact_hashes <- data.frame(
    file = artifact_files,
    sha256 = vapply(file.path(run_dir, artifact_files), sha256_file,
                    character(1L)),
    stringsAsFactors = FALSE
  )
  utils::write.csv(artifact_hashes,
                   file.path(run_dir, "preflight_artifact_hashes.csv"),
                   row.names = FALSE)
  manifest <- list(
    schema_version = paste0(config$schema_version, "/wave_preflight"),
    study_id = config$study_id,
    mode = mode,
    git_commit = git_commit,
    run_dir = run_dir,
    n_waves = nrow(wave_plan),
    expected_result_rows = sum(wave_plan$expected_result_rows),
    expected_datasets = sum(wave_plan$expected_datasets),
    scan_calibrations = nrow(calibration_table),
    oracle_certificates = nrow(oracle_table),
    oracle_sh_is_deployable_method = FALSE,
    created_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    artifact_hashes = artifact_hashes
  )
  jsonlite::write_json(manifest, file.path(run_dir, "preflight_manifest.json"),
                       pretty = TRUE, auto_unbox = TRUE)
  write_health(run_dir)
  cat("RUN_DIR=", run_dir, "\n", sep = "")
}

start_wave <- function(run_dir, wave) {
  worker <- file.path(repo_root, "application", "scripts",
                      "69_validate_rqr_bayes_uq.R")
  output_dir <- wave_output_dir(run_dir, wave$wave_id)
  log_file <- wave_log_file(run_dir, wave$wave_id)
  pid_file <- wave_pid_file(run_dir, wave$wave_id)
  if (dir.exists(output_dir)) {
    stopf("Refusing to relaunch wave with existing output: ", output_dir)
  }
  worker_args <- c(
    worker,
    paste0("--mode=", wave$mode),
    paste0("--config=", file.path(run_dir, "config_frozen.json")),
    paste0("--output-dir=", output_dir),
    paste0("--wave-id=", wave$wave_id),
    paste0("--wave-dgp=", wave$dgp_id),
    paste0("--wave-n=", wave$n),
    paste0("--wave-content=", wave$guaranteed_content),
    paste0("--wave-tolerance-confidence=", wave$tolerance_confidence),
    paste0("--scan-calibration-cache=",
           file.path(run_dir, "scan_calibration_cache.rds")),
    paste0("--oracle-cache=", file.path(run_dir, "oracle_cache.rds"))
  )
  writeLines(c(
    paste0("wave_id: ", wave$wave_id),
    paste0("started_at_utc: ",
           format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
    paste("command: Rscript", paste(shQuote(worker_args), collapse = " ")),
    ""
  ), log_file)
  launcher <- if (nzchar(Sys.which("setsid"))) "nohup setsid" else "nohup"
  inner <- paste("exec Rscript", paste(shQuote(worker_args), collapse = " "))
  command <- paste(
    launcher, "bash -c", shQuote(inner),
    ">>", shQuote(log_file), "2>&1 & echo $!"
  )
  pid <- system(command, intern = TRUE)
  if (!length(pid) || !nzchar(pid[[1L]])) {
    stopf("Failed to launch wave: ", wave$wave_id)
  }
  writeLines(pid[[1L]], pid_file)
  pid[[1L]]
}

collect_run <- function(run_dir) {
  status <- wave_status(run_dir)
  if (any(status$status != "complete")) {
    stopf("Cannot collect before all waves are complete.")
  }
  result_paths <- file.path(status$output_dir, "bayes_uq_validation_results.csv")
  summary_paths <- file.path(status$output_dir, "bayes_uq_validation_summary.csv")
  results <- do.call(rbind, lapply(result_paths, utils::read.csv,
                                   stringsAsFactors = FALSE,
                                   check.names = FALSE))
  summary <- do.call(rbind, lapply(summary_paths, utils::read.csv,
                                   stringsAsFactors = FALSE,
                                   check.names = FALSE))
  utils::write.csv(results, file.path(run_dir, "bayes_uq_validation_results.csv"),
                   row.names = FALSE)
  utils::write.csv(summary, file.path(run_dir, "bayes_uq_validation_summary.csv"),
                   row.names = FALSE)
  readme <- c(
    paste0("# ", config$study_id),
    "",
    paste0("- Mode: `", mode, "`"),
    paste0("- Git commit: `", git_commit, "`"),
    paste0("- Waves: `", nrow(status), "`"),
    paste0("- Result rows: `", nrow(results), "`"),
    paste0("- Summary rows: `", nrow(summary), "`"),
    "- Diagnostic reference method: `tcsp_mc`",
    "- Oracle shortest reference present: `TRUE`",
    "",
    "This is the collected wave run for the iid univariate Bayesian UQ main validation.",
    "The `oracle_sh` method is a non-deployable synthetic-DGP benchmark for the true population shortest interval and exact mean tilt.",
    "The operational reference remains `tcsp_mc`; oracle-relative gaps are efficiency diagnostics only."
  )
  writeLines(readme, file.path(run_dir, "README.md"))
  artifact_files <- c(
    "bayes_uq_validation_results.csv",
    "bayes_uq_validation_summary.csv",
    "README.md",
    "wave_plan.csv",
    "wave_status.csv",
    "scan_calibration_summary.csv",
    "oracle_reference.csv",
    "preflight_manifest.json",
    "preflight_artifact_hashes.csv"
  )
  artifact_hashes <- data.frame(
    file = artifact_files,
    sha256 = vapply(file.path(run_dir, artifact_files), sha256_file,
                    character(1L)),
    stringsAsFactors = FALSE
  )
  utils::write.csv(artifact_hashes, file.path(run_dir, "artifact_hashes.csv"),
                   row.names = FALSE)
  manifest <- list(
    schema_version = config$schema_version,
    study_id = config$study_id,
    mode = mode,
    git_commit = git_commit,
    run_dir = run_dir,
    n_waves = nrow(status),
    n_result_rows = nrow(results),
    n_summary_rows = nrow(summary),
    posterior_endpoint_coverage_claim_available = FALSE,
    oracle_sh_reference_present = TRUE,
    oracle_sh_is_deployable_method = FALSE,
    scan_count_fixed_not_resampled = TRUE,
    diagnostic_reference_method_id = "tcsp_mc",
    wave_plan_sha256 = sha256_file(file.path(run_dir, "wave_plan.csv")),
    scan_calibration_cache_sha256 =
      sha256_file(file.path(run_dir, "scan_calibration_cache.rds")),
    oracle_cache_sha256 = sha256_file(file.path(run_dir, "oracle_cache.rds")),
    created_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    artifact_hashes = artifact_hashes
  )
  jsonlite::write_json(manifest, file.path(run_dir, "manifest.json"),
                       pretty = TRUE, auto_unbox = TRUE)
  write_health(run_dir)
  cat("Collected Bayesian UQ wave run:", run_dir, "\n")
}

launch_run <- function(run_dir) {
  if (require_clean) {
    git_status <- system2("git", c("status", "--short"),
                          stdout = TRUE, stderr = FALSE)
    if (length(git_status)) {
      stopf("Refusing to launch from a dirty source tree:\n",
            paste(git_status, collapse = "\n"))
    }
  }
  repeat {
    status <- wave_status(run_dir)
    if (any(status$status == "failed")) {
      write_health(run_dir)
      stopf("At least one Bayesian UQ wave failed; inspect wave_status.csv.")
    }
    if (all(status$status == "complete")) {
      collect_run(run_dir)
      break
    }
    running <- sum(status$status == "running")
    slots <- max(0L, max_concurrent - running)
    pending <- status[status$status == "pending", , drop = FALSE]
    if (slots > 0L && nrow(pending)) {
      plan <- read_wave_plan(run_dir)
      to_launch <- head(pending$wave_id, slots)
      for (wave_id in to_launch) {
        wave <- plan[plan$wave_id == wave_id, , drop = FALSE]
        pid <- start_wave(run_dir, wave)
        cat(sprintf("Launched %s pid=%s\n", wave_id, pid))
      }
    }
    write_health(run_dir)
    Sys.sleep(poll_seconds)
  }
}

if (identical(action, "prepare")) {
  prepare_run()
} else if (identical(action, "launch")) {
  launch_run(run_dir)
} else if (identical(action, "health")) {
  write_health(run_dir)
} else if (identical(action, "collect")) {
  collect_run(run_dir)
}
