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
arg_present <- function(prefix) any(startsWith(args, prefix))
stopf <- function(...) stop(paste0(...), call. = FALSE)
`%||%` <- function(a, b) if (is.null(a)) b else a

legacy_method_id_map <- c(
  tcsp_mtrqr_gibbs_median_mc = "tcsp_mti_gibbs_median_mc",
  tcsp_mtrqr_gibbs_mean_mc = "tcsp_mti_gibbs_mean_mc",
  tcsp_mtrqr_ecm_map_mc = "tcsp_mti_ecm_map_mc",
  tcsp_mtrqr_ecm_boundary_mc = "tcsp_mti_ecm_boundary_mc",
  tcsp_mtrqr_gibbs_median_oracle_tilt_mc =
    "tcsp_mti_gibbs_median_oracle_tilt_mc",
  tcsp_mtrqr_ecm_map_oracle_tilt_mc = "tcsp_mti_ecm_map_oracle_tilt_mc"
)
canonical_method_id <- function(x) {
  x <- as.character(x)
  mapped <- unname(legacy_method_id_map[x])
  ifelse(is.na(mapped), x, mapped)
}
canonical_engine_id <- function(x) {
  if (is.null(x)) return(x)
  sub("^mtrqr_", "mti_", as.character(x))
}
canonicalize_text_id <- function(x) {
  if (is.null(x)) return(x)
  gsub("mtrqr", "mti", as.character(x), fixed = TRUE)
}
canonicalize_bayes_uq_config <- function(config) {
  for (ii in seq_along(config$methods)) {
    config$methods[[ii]]$method_id <- canonical_method_id(
      config$methods[[ii]]$method_id
    )
    config$methods[[ii]]$action_lane <- canonicalize_text_id(
      config$methods[[ii]]$action_lane
    )
    config$methods[[ii]]$selected_interval_source <- canonicalize_text_id(
      config$methods[[ii]]$selected_interval_source
    )
    config$methods[[ii]]$uq_engine <- canonical_engine_id(
      config$methods[[ii]]$uq_engine
    )
  }
  for (mode_name in names(config$modes)) {
    config$modes[[mode_name]]$method_ids <- as.list(canonical_method_id(
      unlist(config$modes[[mode_name]]$method_ids, use.names = FALSE)
    ))
  }
  engines <- config$engine_defaults %||% list()
  if (!is.null(engines$mtrqr_gibbs) && is.null(engines$mti_gibbs)) {
    engines$mti_gibbs <- engines$mtrqr_gibbs
  }
  if (!is.null(engines$mtrqr_ecm) && is.null(engines$mti_ecm)) {
    engines$mti_ecm <- engines$mtrqr_ecm
  }
  config$engine_defaults <- engines
  config
}

for (package in c("rqrgibbs", "jsonlite", "digest")) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stopf("Required package is not installed: ", package)
  }
}
library(rqrgibbs)

action <- tolower(arg_value("--action=", "health"))
allowed_actions <- c("prepare", "launch", "health", "collect", "stop")
if (!action %in% allowed_actions) {
  stopf("Unsupported Bayesian UQ wave action: ", action)
}
mode <- tolower(arg_value("--mode=", "confirmatory"))
run_dir_arg <- arg_value("--run-dir=", NULL)
default_config_path <- file.path(
  "application", "config", "rqr_bayes_uq_validation_main_20260813.json"
)
config_path_raw <- arg_value("--config=", default_config_path)
if (!arg_present("--config=") && !is.null(run_dir_arg)) {
  frozen_config_path <- file.path(run_dir_arg, "config_frozen.json")
  if (file.exists(frozen_config_path)) {
    config_path_raw <- frozen_config_path
  }
}
config_path <- normalizePath(config_path_raw, winslash = "/", mustWork = TRUE)
run_root <- normalizePath(arg_value(
  "--run-root=", file.path("application", "runs",
                           "rqr_bayes_uq_validation_main_20260813")
), winslash = "/", mustWork = FALSE)
run_id <- arg_value(
  "--run-id=", paste0("wave_main_", format(Sys.time(), "%Y%m%dT%H%M%SZ",
                                           tz = "UTC"))
)
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
config <- canonicalize_bayes_uq_config(config)
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

character_values <- function(x) {
  as.character(unlist(x, use.names = FALSE))
}
numeric_values <- function(x) {
  as.numeric(unlist(x, use.names = FALSE))
}
integer_values <- function(x) {
  as.integer(numeric_values(x))
}

mode_design_cells <- function(mode_cfg) {
  if (!is.null(mode_cfg$design_cells)) {
    cells <- mode_cfg$design_cells
    rows <- lapply(seq_along(cells), function(ii) {
      cell <- cells[[ii]]
      content <- cell$guaranteed_content %||% cell$content
      confidence <- cell$tolerance_confidence %||% cell$confidence
      data.frame(
        cell_id = as.character(cell$cell_id %||% sprintf("cell%03d", ii)),
        n = as.integer(cell$n)[1L],
        guaranteed_content = as.numeric(content)[1L],
        tolerance_confidence = as.numeric(confidence)[1L],
        stringsAsFactors = FALSE
      )
    })
    out <- do.call(rbind, rows)
  } else {
    out <- expand.grid(
      cell_id = NA_character_,
      n = integer_values(mode_cfg$sample_sizes),
      guaranteed_content = numeric_values(mode_cfg$guaranteed_contents),
      tolerance_confidence = numeric_values(mode_cfg$tolerance_confidences),
      stringsAsFactors = FALSE
    )
    out$cell_id <- sprintf(
      "n%04d_c%s_t%s",
      out$n,
      gsub("\\.", "", sprintf("%.3f", out$guaranteed_content)),
      gsub("\\.", "", sprintf("%.3f", out$tolerance_confidence))
    )
  }
  if (!nrow(out) ||
      any(!is.finite(out$n)) ||
      any(out$n < 2L) ||
      any(!is.finite(out$guaranteed_content)) ||
      any(out$guaranteed_content <= 0 | out$guaranteed_content >= 1) ||
      any(!is.finite(out$tolerance_confidence)) ||
      any(out$tolerance_confidence <= 0 | out$tolerance_confidence >= 1)) {
    stopf("Mode design cells must have finite n >= 2 and probabilities in (0, 1).")
  }
  out <- out[order(out$n, out$guaranteed_content,
                   out$tolerance_confidence, out$cell_id), , drop = FALSE]
  rownames(out) <- NULL
  out
}

design_cells <- mode_design_cells(mode_cfg)
split_waves_by_method <- isTRUE(mode_cfg$split_waves_by_method)

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
  scan_cfg <- config$scan_calibration %||% list()
  global <- scan_cfg$method %||% NULL
  if (method_id %in% c(
    "hdp_s_mc", "tcsp_mc", "tcsp_mti_gibbs_median_mc",
    "tcsp_mti_gibbs_mean_mc", "tcsp_mti_ecm_map_mc",
    "tcsp_mti_ecm_boundary_mc",
    "tcsp_mti_gibbs_median_oracle_tilt_mc",
    "tcsp_mti_ecm_map_oracle_tilt_mc"
  )) {
    if (!is.null(global)) return(as.character(global)[1L])
    return("monte_carlo_conservative")
  }
  if (method_id %in% c("hdp_s", "tcsp_dkw")) return("dkw_conservative")
  NA_character_
}

scan_adaptive_control_for <- function(method_id) {
  method_meta <- method_by_id[[method_id]]
  scan_cfg <- config$scan_calibration %||% list()
  control <- scan_cfg$adaptive_control %||% list()
  mode_control <- mode_cfg$scan_adaptive_control %||%
    mode_cfg$adaptive_control %||% list()
  method_control <- method_meta$scan_adaptive_control %||%
    method_meta$adaptive_control %||% list()
  control <- utils::modifyList(control, mode_control)
  utils::modifyList(control, method_control)
}

scan_args_for <- function(method_id, n, c_target, tol_conf) {
  method_meta <- method_by_id[[method_id]]
  scan_cfg <- config$scan_calibration %||% list()
  method <- scan_method_for(method_id)
  adaptive_control <- scan_adaptive_control_for(method_id)
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
  key_parts <- c(mode, method, n, c_target, tol_conf, n_sim,
                 numerical_confidence)
  if (identical(method, "monte_carlo_cp_adaptive") ||
      length(adaptive_control)) {
    key_parts <- c(key_parts, digest::digest(adaptive_control, algo = "sha256",
                                             serialize = TRUE))
  }
  key <- paste(key_parts, collapse = "|")
  list(
    n_sim = as.integer(n_sim)[1L],
    numerical_confidence = as.numeric(numerical_confidence)[1L],
    seed = hash_to_seed(key, base = seed_base),
    adaptive_control = adaptive_control
  )
}

scan_cache_key <- function(method_id, n, c_target, tol_conf) {
  method <- scan_method_for(method_id)
  args <- scan_args_for(method_id, n, c_target, tol_conf)
  key_parts <- c(method, n, c_target, tol_conf, args$n_sim,
                 args$numerical_confidence, args$seed)
  if (identical(method, "monte_carlo_cp_adaptive") ||
      length(args$adaptive_control)) {
    key_parts <- c(key_parts, digest::digest(args$adaptive_control,
                                             algo = "sha256",
                                             serialize = TRUE))
  }
  paste(key_parts, collapse = "|")
}

calibrate_safely <- function(method_id, n, c_target, tol_conf) {
  method <- scan_method_for(method_id)
  args <- scan_args_for(method_id, n, c_target, tol_conf)
  tryCatch(
    tcsp_calibrate_count(
      n = n,
      guaranteed_content = c_target,
      tolerance_confidence = tol_conf,
      method = method,
      n_sim = args$n_sim,
      numerical_confidence = args$numerical_confidence,
      seed = args$seed,
      adaptive_control = args$adaptive_control
    ),
    error = function(e) {
      list(
        schema_version = paste0(config$schema_version, "/scan_calibration"),
        method = "scan_calibrated_tcsp_mti",
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
        adaptive_control = args$adaptive_control,
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
  if (identical(dgp$family, "standardized_laplace")) {
    return(list(
      family = "laplace",
      params = list(location = 0, scale = 1 / sqrt(2))
    ))
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
  if (identical(dgp$family, "standardized_gamma")) {
    shape <- as.numeric(dgp$shape %||% 2)[1L]
    scale <- as.numeric(dgp$scale %||% 1)[1L]
    if (!is.finite(shape) || shape <= 0 ||
        !is.finite(scale) || scale <= 0) {
      stopf("standardized_gamma requires positive shape and scale.")
    }
    return(list(
      family = "centered_gamma",
      params = list(shape = shape, scale = 1 / sqrt(shape))
    ))
  }
  if (identical(dgp$family, "centered_exponential") ||
      identical(dgp$family, "standardized_exponential")) {
    rate <- as.numeric(dgp$rate %||% 1)[1L]
    if (!is.finite(rate) || rate <= 0) {
      stopf("centered_exponential requires positive rate.")
    }
    return(list(
      family = "centered_exponential",
      params = list(shape = 1, scale = 1)
    ))
  }
  if (identical(dgp$family, "standardized_beta")) {
    return(list(
      family = "standardized_beta",
      params = list(
        shape1 = as.numeric(dgp$shape1 %||% dgp$a %||% 2)[1L],
        shape2 = as.numeric(dgp$shape2 %||% dgp$b %||% 5)[1L]
      )
    ))
  }
  if (identical(dgp$family, "standardized_asymmetric_laplace")) {
    return(list(
      family = "asymmetric_laplace",
      params = list(
        tau = as.numeric(dgp$tau %||% dgp$p %||% dgp$p0 %||% 0.10)[1L],
        scale = as.numeric(dgp$scale %||% 1)[1L],
        variance_standardized = TRUE
      )
    ))
  }
  if (identical(dgp$family, "standardized_two_piece_normal")) {
    return(list(
      family = "standardized_two_piece_normal",
      params = list(
        left_scale = as.numeric(
          dgp$left_scale %||% dgp$scale_left %||%
            dgp$sigma_left %||% dgp$left_sd %||% 1
        )[1L],
        right_scale = as.numeric(
          dgp$right_scale %||% dgp$scale_right %||%
            dgp$sigma_right %||% dgp$right_sd %||% 12
        )[1L],
        variance_standardized = TRUE
      )
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
  for (dgp_id in character_values(mode_cfg$dgp_ids)) {
    for (cell_index in seq_len(nrow(design_cells))) {
      n <- as.integer(design_cells$n[[cell_index]])
      c_target <- as.numeric(design_cells$guaranteed_content[[cell_index]])
      tol_conf <- as.numeric(design_cells$tolerance_confidence[[cell_index]])
      method_groups <- if (split_waves_by_method) {
        as.list(character_values(mode_cfg$method_ids))
      } else {
        list(character_values(mode_cfg$method_ids))
      }
      for (method_ids in method_groups) {
        index <- index + 1L
        method_slug <- if (split_waves_by_method) {
          paste0("_", safe_slug(method_ids[[1L]]))
        } else {
          ""
        }
        wave_id <- sprintf(
          "w%03d_%s_n%04d_c%s_t%s%s",
          index, safe_slug(dgp_id), n, format_probability(c_target),
          format_probability(tol_conf), method_slug
        )
        expected_datasets <-
          length(numeric_values(mode_cfg$posterior_confidences)) *
          as.integer(mode_cfg$replications)
        out[[index]] <- data.frame(
          wave_id = wave_id,
          mode = mode,
          dgp_id = dgp_id,
          n = n,
          guaranteed_content = c_target,
          tolerance_confidence = tol_conf,
          cell_id = design_cells$cell_id[[cell_index]],
          posterior_confidences = paste(
            numeric_values(mode_cfg$posterior_confidences), collapse = ";"
          ),
          replications = as.integer(mode_cfg$replications),
          method_ids = paste(method_ids, collapse = ";"),
          method_count = length(method_ids),
          expected_datasets = expected_datasets,
          expected_result_rows = expected_datasets * length(method_ids),
          stringsAsFactors = FALSE
        )
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

  scan_method_ids <- character_values(mode_cfg$method_ids)
  scan_method_ids <- scan_method_ids[
    !is.na(vapply(scan_method_ids, scan_method_for, character(1L)))
  ]
  calibrations <- list()
  calibration_rows <- list()
  calibration_index <- 0L
  calibration_cells <- unique(design_cells[
    c("n", "guaranteed_content", "tolerance_confidence")
  ])
  for (method_id in scan_method_ids) {
    for (cell_index in seq_len(nrow(calibration_cells))) {
      n <- as.integer(calibration_cells$n[[cell_index]])
      c_target <- as.numeric(calibration_cells$guaranteed_content[[cell_index]])
      tol_conf <- as.numeric(calibration_cells$tolerance_confidence[[cell_index]])
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
        probability_estimate =
          calibration$scan_probability$probability_estimate %||% NA_real_,
        n_sim_total = calibration$scan_probability$n_sim %||% NA_integer_,
        point_estimate_crossing_k =
          calibration$point_estimate_crossing_k %||% NA_integer_,
        terminal_exact_probability =
          calibration$terminal_probability %||% NA_real_,
        structural_status =
          calibration$structural_status %||% "",
        infeasible = isTRUE(calibration$infeasible),
        message = calibration$message %||% "",
        calibration_digest = digest::digest(calibration, algo = "sha256",
                                            serialize = TRUE),
        stringsAsFactors = FALSE
      )
    }
  }
  calibration_table <- if (length(calibration_rows)) {
    do.call(rbind, calibration_rows)
  } else {
    data.frame(
      cache_key = character(),
      method = character(),
      n = integer(),
      guaranteed_content = numeric(),
      tolerance_confidence = numeric(),
      retained_count = integer(),
      content_buffer = numeric(),
      certified_lower_probability = numeric(),
      probability_estimate = numeric(),
      n_sim_total = integer(),
      point_estimate_crossing_k = integer(),
      terminal_exact_probability = numeric(),
      structural_status = character(),
      infeasible = logical(),
      message = character(),
      calibration_digest = character(),
      stringsAsFactors = FALSE
    )
  }
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
  for (dgp_id in unique(character_values(mode_cfg$dgp_ids))) {
    dgp <- dgp_by_id[[dgp_id]]
    spec <- oracle_spec_from_dgp(dgp)
    for (c_target in unique(design_cells$guaranteed_content)) {
      key <- oracle_key(dgp_id, c_target)
      certificate <- mti_interval_oracle(
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
    paste0("--wave-method=", wave$method_ids),
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
  method_ids_present <- sort(unique(as.character(results$method_id)))
  method_notes <- c(
    paste0("- Methods present: `", paste(method_ids_present,
                                        collapse = "`, `"), "`")
  )
  if ("mti_ecm_dp_profile" %in% method_ids_present) {
    method_notes <- c(
      method_notes,
      "The `mti_ecm_dp_profile` rows use MTI-ECM endpoint candidates screened by direct-DP fixed-interval content probabilities.",
      "For this method, `target_content` is the fitted MTI content, `target_mean_tilt` is the selected mean tilt, and `posterior_probability` is the direct-DP probability that the fixed interval has at least the requested population content.",
      "This method is evaluated by repeated-sample validation; it is not an exact distribution-free scan certificate."
    )
  }
  if (any(grepl("^mti_ecm_dp_profile_tune_", method_ids_present))) {
    method_notes <- c(
      method_notes,
      "The `mti_ecm_dp_profile_tune_*` rows are MTI-ECM direct-DP profile tuning variants with method-specific screen levels and profile grids.",
      "For these variants, `effective_posterior_confidence` records the direct-DP content-screen level actually used.",
      "`direct_dp_concentration` records the direct-DP concentration used for the response-distribution content screen."
    )
  }
  if (any(grepl("^tcsp_mti_", method_ids_present))) {
    method_notes <- c(
      method_notes,
      "The `tcsp_mti_*` rows are fixed-target MTI summaries after scan calibration; `formal_action_*` records the associated scan action and `fitted_summary_*` records the MTI endpoint summary."
    )
  }
  if ("hdp_s_mc" %in% method_ids_present) {
    method_notes <- c(
      method_notes,
      "The hybrid direct-DP scan method fixes the scan count before evaluating direct-DP content probability."
    )
  }
  if ("oracle_sh" %in% method_ids_present) {
    method_notes <- c(
      method_notes,
      "The `oracle_sh` rows are non-deployable synthetic-DGP references for the population shortest interval and oracle mean tilt."
    )
  }
  readme <- c(
    paste0("# ", config$study_id),
    "",
    paste0("- Mode: `", mode, "`"),
    paste0("- Git commit: `", git_commit, "`"),
    paste0("- Waves: `", nrow(status), "`"),
    paste0("- Result rows: `", nrow(results), "`"),
    paste0("- Summary rows: `", nrow(summary), "`"),
    paste0("- Diagnostic reference method: `",
           config$diagnostics$reference_method_id %||% "tcsp_mc", "`"),
    paste0("- Oracle reference table present: `",
           file.exists(file.path(run_dir, "oracle_reference.csv")), "`"),
    "",
    "This is the collected wave run for iid univariate validation.",
    "It separates response-distribution Bayesian uncertainty quantification from loss-based generalized-Bayes MTI endpoint construction.",
    method_notes,
    "Oracle-relative gaps are efficiency diagnostics only."
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

stop_run <- function(run_dir) {
  status <- wave_status(run_dir)
  utils::write.csv(status, file.path(run_dir, "superseded_wave_status_before_stop.csv"),
                   row.names = FALSE)
  pid_files <- list.files(file.path(run_dir, "pids"), pattern = "[.]pid$",
                          full.names = TRUE)
  pids <- unique(suppressWarnings(as.integer(unlist(lapply(
    pid_files, readLines, warn = FALSE
  )))))
  pids <- pids[is.finite(pids) & pids > 0]
  alive <- pids[vapply(pids, pid_alive, logical(1L))]
  if (length(alive)) {
    system2("kill", c("-TERM", as.character(alive)))
    Sys.sleep(5)
  }
  alive <- pids[vapply(pids, pid_alive, logical(1L))]
  if (length(alive)) {
    system2("kill", c("-KILL", as.character(alive)))
    Sys.sleep(1)
  }
  alive <- pids[vapply(pids, pid_alive, logical(1L))]
  stopped_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  note <- list(
    schema_version = paste0(config$schema_version, "/supersession"),
    study_id = config$study_id,
    mode = mode,
    status = if (length(alive)) "stop_incomplete_processes_still_alive" else
      "superseded_stopped",
    reason = paste(
      "Superseded by corrected method grid with explicit MTI Gibbs and",
      "MTI ECM competitor rows."
    ),
    stopped_at_utc = stopped_at,
    pids_seen = pids,
    pids_alive_after_stop = alive,
    final_artifacts_present = file.exists(file.path(run_dir, "manifest.json")),
    promote_as_confirmatory_evidence = FALSE
  )
  jsonlite::write_json(note, file.path(run_dir, "superseded.json"),
                       pretty = TRUE, auto_unbox = TRUE)
  writeLines(c(
    "# Superseded Bayesian UQ Wave Run",
    "",
    paste0("Stopped at UTC: `", stopped_at, "`"),
    "",
    "Reason: this run was superseded by the corrected method grid with explicit MTI Gibbs and MTI ECM competitor rows.",
    "",
    "Do not collect or promote these partial artifacts as final confirmatory evidence."
  ), file.path(run_dir, "SUPERSEDED.md"))
  write_health(run_dir)
  cat(sprintf("Stopped Bayesian UQ wave run: %s; alive_after_stop=%d\n",
              run_dir, length(alive)))
  invisible(note)
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
} else if (identical(action, "stop")) {
  stop_run(run_dir)
}
