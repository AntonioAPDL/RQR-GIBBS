#!/usr/bin/env Rscript

arguments <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_path_override <- Sys.getenv("RQRGIBBS_ADJUDICATION_SCRIPT_PATH", unset = "")
script_path <- if (nzchar(script_path_override)) {
  script_path_override
} else {
  sub(file_arg, "", arguments[startsWith(arguments, file_arg)][1L])
}
if (!nzchar(script_path_override) &&
    (identical(Sys.getenv("RQRGIBBS_ADJUDICATION_SOURCE_ONLY"), "true") ||
    !length(script_path) || is.na(script_path) ||
    !identical(basename(script_path),
               "76_adjudicate_tolerance_validation_results.R"))) {
  script_path <- "application/scripts/76_adjudicate_tolerance_validation_results.R"
}
script_path <- normalizePath(script_path, winslash = "/", mustWork = TRUE)
repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."),
                           winslash = "/", mustWork = TRUE)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

tva_stop <- function(...) {
  stop(paste0(...), call. = FALSE)
}

tva_arg_value <- function(args, prefix, default = NULL) {
  hit <- args[startsWith(args, prefix)]
  if (length(hit)) sub(prefix, "", hit[[1L]], fixed = TRUE) else default
}

tva_bool_arg <- function(value, default = FALSE) {
  if (is.null(value) || !nzchar(value)) return(default)
  value <- tolower(trimws(value))
  if (value %in% c("true", "t", "1", "yes", "y")) return(TRUE)
  if (value %in% c("false", "f", "0", "no", "n")) return(FALSE)
  tva_stop("Invalid logical argument value: ", value)
}

tva_require_packages <- function(packages) {
  for (package in packages) {
    if (!requireNamespace(package, quietly = TRUE)) {
      tva_stop("Required package is not installed: ", package)
    }
  }
}

tva_git_commit <- function(root = repo_root) {
  tryCatch(
    system2("git", c("-C", root, "rev-parse", "HEAD"),
            stdout = TRUE, stderr = FALSE)[[1L]],
    error = function(e) NA_character_
  )
}

tva_git_status <- function(root = repo_root) {
  tryCatch(
    paste(system2("git", c("-C", root, "status", "--short", "--branch"),
                  stdout = TRUE, stderr = FALSE), collapse = "\n"),
    error = function(e) NA_character_
  )
}

tva_normalize_dir <- function(path, label, must_work = TRUE) {
  if (is.null(path) || !nzchar(path)) {
    tva_stop("Missing required directory argument: ", label)
  }
  normalizePath(path, winslash = "/", mustWork = must_work)
}

tva_required_file <- function(dir, file) {
  path <- file.path(dir, file)
  if (!file.exists(path)) {
    tva_stop("Missing required file: ", path)
  }
  path
}

tva_read_csv <- function(path) {
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

tva_read_json <- function(path, simplify = TRUE) {
  jsonlite::read_json(path, simplifyVector = simplify)
}

tva_hash_file <- function(path) {
  digest::digest(path, algo = "sha256", file = TRUE)
}

tva_as_logical <- function(x) {
  if (is.logical(x)) return(x)
  if (is.numeric(x)) return(ifelse(is.na(x), NA, x != 0))
  x <- tolower(trimws(as.character(x)))
  out <- rep(NA, length(x))
  out[x %in% c("true", "t", "1", "yes", "y")] <- TRUE
  out[x %in% c("false", "f", "0", "no", "n")] <- FALSE
  out
}

tva_num <- function(x) {
  suppressWarnings(as.numeric(x))
}

tva_mean <- function(x) {
  x <- tva_num(x)
  x <- x[is.finite(x)]
  if (length(x)) mean(x) else NA_real_
}

tva_median <- function(x) {
  x <- tva_num(x)
  x <- x[is.finite(x)]
  if (length(x)) stats::median(x) else NA_real_
}

tva_quantile <- function(x, probability) {
  x <- tva_num(x)
  x <- x[is.finite(x)]
  if (length(x)) {
    as.numeric(stats::quantile(x, probability, names = FALSE, type = 8))
  } else {
    NA_real_
  }
}

tva_bool_mean <- function(x) {
  x <- tva_as_logical(x)
  if (any(!is.na(x))) mean(x, na.rm = TRUE) else NA_real_
}

tva_first_nonmissing <- function(x, default = NA) {
  if (!length(x)) return(default)
  ok <- !(is.na(x) | !nzchar(as.character(x)))
  if (any(ok)) x[which(ok)[[1L]]] else default
}

tva_bind_fill <- function(...) {
  frames <- list(...)
  frames <- frames[vapply(frames, nrow, integer(1L)) > 0L]
  if (!length(frames)) return(data.frame())
  cols <- Reduce(union, lapply(frames, names))
  aligned <- lapply(frames, function(x) {
    missing <- setdiff(cols, names(x))
    for (name in missing) x[[name]] <- NA
    x[, cols, drop = FALSE]
  })
  do.call(rbind, aligned)
}

tva_full_range_certificate <- function(n, content) {
  n <- as.integer(n)
  content <- as.numeric(content)
  ifelse(
    is.finite(n) & n >= 2L & is.finite(content) & content > 0 & content < 1,
    1 - n * (1 - content) * content^(n - 1) - content^n,
    NA_real_
  )
}

tva_wilks_certificate_frame <- function(n, content, confidence) {
  out <- unique(data.frame(
    n = as.integer(n),
    guaranteed_content = as.numeric(content),
    tolerance_confidence = as.numeric(confidence),
    stringsAsFactors = FALSE
  ))
  out <- out[order(out$n, out$guaranteed_content,
                   out$tolerance_confidence), , drop = FALSE]
  out$wilks_full_range_certificate <- tva_full_range_certificate(
    out$n, out$guaranteed_content
  )
  out$certifies_requested_statement <- with(
    out, wilks_full_range_certificate + 1e-12 >= tolerance_confidence
  )
  out$certificate_status <- "exact_beta_full_range"
  rownames(out) <- NULL
  out
}

tva_assert_complete_health <- function(run_dir, allow_incomplete = FALSE) {
  health_path <- tva_required_file(run_dir, "health.json")
  health <- tva_read_json(health_path, simplify = TRUE)
  checks <- c(
    isTRUE(health$final_artifacts_present),
    is.null(health$rows_remaining) || identical(as.integer(health$rows_remaining), 0L),
    is.null(health$waves_failed) || identical(as.integer(health$waves_failed), 0L)
  )
  if (!allow_incomplete && !all(checks)) {
    tva_stop("Run health is not complete: ", run_dir)
  }
  health
}

tva_input_hash_rows <- function(run_label, run_dir) {
  files <- c(
    "health.json", "manifest.json", "config_frozen.json",
    "bayes_uq_validation_results.csv", "bayes_uq_validation_summary.csv",
    "wave_plan.csv", "wave_status.csv", "scan_calibration_summary.csv"
  )
  rows <- lapply(files, function(file) {
    path <- file.path(run_dir, file)
    if (!file.exists(path)) return(NULL)
    data.frame(
      run_label = run_label,
      artifact = file,
      path = normalizePath(path, winslash = "/", mustWork = TRUE),
      size_bytes = file.info(path)$size,
      sha256 = tva_hash_file(path),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

tva_find_young_mathew_addon <- function(main_run_dir) {
  dirs <- list.dirs(main_run_dir, recursive = FALSE, full.names = TRUE)
  dirs <- dirs[grepl("young_mathew_addon_", basename(dirs), fixed = TRUE)]
  dirs[file.exists(file.path(dirs, "bayes_uq_validation_results.csv"))]
}

tva_read_run_results <- function(run_label, run_dir, allow_incomplete = FALSE,
                                 include_young_mathew_addon = TRUE) {
  run_dir <- tva_normalize_dir(run_dir, run_label, must_work = TRUE)
  health <- tva_assert_complete_health(run_dir, allow_incomplete)
  invisible(lapply(c("manifest.json", "config_frozen.json",
                     "bayes_uq_validation_summary.csv"), function(file) {
    tva_required_file(run_dir, file)
  }))
  results_path <- tva_required_file(run_dir, "bayes_uq_validation_results.csv")
  results <- tva_read_csv(results_path)
  results$run_label <- run_label
  results$run_component <- "primary"
  results$source_run_dir <- run_dir
  input_hashes <- tva_input_hash_rows(run_label, run_dir)

  addon_dirs <- character()
  if (include_young_mathew_addon) {
    addon_dirs <- tva_find_young_mathew_addon(run_dir)
  }
  if (length(addon_dirs)) {
    addon_frames <- lapply(addon_dirs, function(addon_dir) {
      addon_health <- if (file.exists(file.path(addon_dir, "health.json"))) {
        tva_assert_complete_health(addon_dir, allow_incomplete)
      } else {
        NULL
      }
      addon <- tva_read_csv(file.path(addon_dir, "bayes_uq_validation_results.csv"))
      addon$run_label <- run_label
      addon$run_component <- basename(addon_dir)
      addon$source_run_dir <- normalizePath(addon_dir, winslash = "/",
                                            mustWork = TRUE)
      addon
    })
    results <- tva_bind_fill(results, do.call(tva_bind_fill, addon_frames))
    addon_hashes <- do.call(rbind, lapply(addon_dirs, function(addon_dir) {
      tva_input_hash_rows(paste(run_label, basename(addon_dir), sep = "/"),
                          addon_dir)
    }))
    input_hashes <- rbind(input_hashes, addon_hashes)
  }
  list(
    results = results,
    health = health,
    input_hashes = input_hashes,
    addon_dirs = addon_dirs
  )
}

tva_primary_analysis_rows <- function(results) {
  if (!("posterior_confidence" %in% names(results))) return(results)
  post <- tva_num(results$posterior_confidence)
  if (any(abs(post - 0.95) < 1e-12, na.rm = TRUE)) {
    results[is.na(post) | abs(post - 0.95) < 1e-12, , drop = FALSE]
  } else {
    results
  }
}

tva_add_reference_widths <- function(results, reference_method_id = "tcsp_mc") {
  keys <- c("run_label", "mode", "dgp_id", "n", "guaranteed_content",
            "tolerance_confidence", "posterior_confidence", "replication",
            "seed")
  keys <- intersect(keys, names(results))
  if (!all(c("method_id", "width") %in% names(results)) ||
      !all(keys %in% names(results))) {
    return(results)
  }
  ref <- results[results$method_id == reference_method_id,
                 c(keys, "width"), drop = FALSE]
  if (!nrow(ref)) return(results)
  names(ref)[names(ref) == "width"] <- "adjudicated_reference_width"
  results$adjudicated_reference_width <- NULL
  results <- merge(results, ref, by = keys, all.x = TRUE, sort = FALSE)
  results$adjudicated_width_ratio_to_reference <- ifelse(
    is.finite(tva_num(results$adjudicated_reference_width)) &
      tva_num(results$adjudicated_reference_width) > 0,
    tva_num(results$width) / tva_num(results$adjudicated_reference_width),
    NA_real_
  )
  results
}

tva_classify_rows <- function(results) {
  if (!nrow(results)) return(results)
  if (!("method_id" %in% names(results))) {
    tva_stop("Results must include method_id.")
  }
  if (!("infeasible" %in% names(results))) results$infeasible <- NA
  if (!("success" %in% names(results))) results$success <- NA
  if (!("scan_certified_lower_probability" %in% names(results))) {
    results$scan_certified_lower_probability <- NA_real_
  }
  if (!("fit_class" %in% names(results))) results$fit_class <- NA_character_
  if (!("message" %in% names(results))) results$message <- NA_character_
  if (!("posterior_draws" %in% names(results))) results$posterior_draws <- NA
  if (!("ecm_final_stationarity" %in% names(results))) {
    results$ecm_final_stationarity <- NA_real_
  }

  n <- tva_num(results$n)
  content <- tva_num(results$guaranteed_content)
  confidence <- tva_num(results$tolerance_confidence)
  infeasible <- tva_as_logical(results$infeasible)
  success <- tva_as_logical(results$success)
  method <- as.character(results$method_id)

  results$order_statistic_certificate <- NA_real_
  is_wilks <- method == "wilks_minmax"
  results$order_statistic_certificate[is_wilks] <-
    tva_full_range_certificate(n[is_wilks], content[is_wilks])

  results$adjudicated_certificate <- NA_real_
  scan_methods <- method %in% c("tcsp_mc", "hdp_s_mc", "tcsp_dkw",
                               "split_empirical_shortest",
                               "split_ecm_fixed_tilt")
  results$adjudicated_certificate[scan_methods] <-
    tva_num(results$scan_certified_lower_probability[scan_methods])
  results$adjudicated_certificate[is_wilks] <-
    results$order_statistic_certificate[is_wilks]
  results$adjudicated_certificate[method == "oracle_sh"] <- 1

  results$certifies_requested_statement <- ifelse(
    is.finite(results$adjudicated_certificate),
    results$adjudicated_certificate + 1e-12 >= confidence,
    NA
  )

  results$certificate_status <- "not_applicable"
  results$certificate_status[method == "oracle_sh"] <- "oracle_known_dgp"
  results$certificate_status[method == "tcsp_mc"] <-
    "monte_carlo_scan_calibration"
  results$certificate_status[method == "hdp_s_mc"] <-
    "hybrid_monte_carlo_scan_calibration"
  results$certificate_status[method == "tcsp_dkw"] <-
    "dkw_conservative_scan_bound"
  results$certificate_status[is_wilks] <- "exact_beta_full_range"
  results$certificate_status[method == "young_mathew"] <-
    "tolerance_package_young_mathew_nominal"
  results$certificate_status[method %in% c("bb_shortest_diag", "dpm_bayes",
                                           "dp_bayes")] <-
    "diagnostic_not_formal_tolerance_certificate"
  results$certificate_status[method %in% c("tcsp_mti_ecm_map_mc",
                                           "tcsp_mti_gibbs_median_mc",
                                           "tcsp_mti_gibbs_mean_mc")] <-
    "conditional_fixed_target_mti_summary"

  results$failure_taxonomy <- "returned_interval"
  results$failure_taxonomy[method == "oracle_sh"] <- "oracle_reference"
  results$failure_taxonomy[method %in% c("bb_shortest_diag", "dpm_bayes",
                                         "dp_bayes")] <- "diagnostic_only"
  results$failure_taxonomy[method == "tcsp_dkw" & infeasible %in% TRUE] <-
    "conservative_bound_infeasible"
  mti <- method %in% c("tcsp_mti_ecm_map_mc", "tcsp_mti_gibbs_median_mc",
                      "tcsp_mti_gibbs_mean_mc")
  results$failure_taxonomy[mti & infeasible %in% TRUE] <-
    "fixed_target_mti_infeasible"
  results$failure_taxonomy[method == "young_mathew" & infeasible %in% TRUE] <-
    "package_method_failed"
  results$failure_taxonomy[method == "wilks_minmax" &
                             !is.na(results$certifies_requested_statement) &
                             !results$certifies_requested_statement] <-
    "returned_uncertified"
  results$failure_taxonomy[scan_methods & infeasible %in% TRUE &
                             method != "tcsp_dkw"] <-
    "scan_calibration_infeasible"
  results$failure_taxonomy[!is.na(success) & !success & !(infeasible %in% TRUE)] <-
    ifelse(results$failure_taxonomy[!is.na(success) & !success &
                                      !(infeasible %in% TRUE)] ==
             "returned_interval",
           "returned_interval_missed_content",
           results$failure_taxonomy[!is.na(success) & !success &
                                      !(infeasible %in% TRUE)])

  draws <- tva_num(results$posterior_draws)
  results$mti_gibbs_screening_budget <- method == "tcsp_mti_gibbs_median_mc" &
    is.finite(draws) & draws < 1000
  results$ecm_stationarity_pass_1e3 <-
    is.finite(tva_num(results$ecm_final_stationarity)) &
    tva_num(results$ecm_final_stationarity) <= 1e-3
  results
}

tva_group_split <- function(df, keys) {
  keys <- intersect(keys, names(df))
  split(df, interaction(df[keys], drop = TRUE, lex.order = TRUE))
}

tva_summarize_taxonomy <- function(results) {
  df <- tva_primary_analysis_rows(results)
  keys <- c("run_label", "mode", "dgp_id", "n", "guaranteed_content",
            "tolerance_confidence", "method_id", "failure_taxonomy",
            "certificate_status")
  rows <- lapply(tva_group_split(df, keys), function(z) {
    certifies <- tva_as_logical(z$certifies_requested_statement)
    data.frame(
      run_label = z$run_label[[1L]],
      mode = z$mode[[1L]],
      dgp_id = z$dgp_id[[1L]],
      n = z$n[[1L]],
      guaranteed_content = z$guaranteed_content[[1L]],
      tolerance_confidence = z$tolerance_confidence[[1L]],
      method_id = z$method_id[[1L]],
      failure_taxonomy = z$failure_taxonomy[[1L]],
      certificate_status = z$certificate_status[[1L]],
      rows = nrow(z),
      infeasible_rate = tva_bool_mean(z$infeasible),
      success_rate = tva_bool_mean(z$success),
      certifies_rate = if (any(!is.na(certifies))) {
        mean(certifies, na.rm = TRUE)
      } else {
        NA_real_
      },
      median_width = tva_median(z$width),
      median_width_ratio_to_tcsp = tva_median(
        z$adjudicated_width_ratio_to_reference %||% z$width_ratio_to_reference
      ),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out <- out[order(out$run_label, out$mode, out$dgp_id, out$n,
                   out$guaranteed_content, out$method_id,
                   out$failure_taxonomy), , drop = FALSE]
  rownames(out) <- NULL
  out
}

tva_young_mathew_contract_audit <- function(results) {
  df <- tva_primary_analysis_rows(results)
  df <- df[df$method_id == "young_mathew", , drop = FALSE]
  if (!nrow(df)) return(data.frame())
  rows <- lapply(
    tva_group_split(df, c("run_label", "mode", "dgp_id", "n",
                          "guaranteed_content", "tolerance_confidence")),
    function(z) {
      data.frame(
        run_label = z$run_label[[1L]],
        mode = z$mode[[1L]],
        dgp_id = z$dgp_id[[1L]],
        n = z$n[[1L]],
        guaranteed_content = z$guaranteed_content[[1L]],
        tolerance_confidence = z$tolerance_confidence[[1L]],
        package = "tolerance",
        package_version = if (requireNamespace("tolerance", quietly = TRUE)) {
          as.character(utils::packageVersion("tolerance"))
        } else {
          NA_character_
        },
        function_name = "nptol.int",
        method = "YM",
        side = 2L,
        alpha = 1 - as.numeric(z$tolerance_confidence[[1L]]),
        P = as.numeric(z$guaranteed_content[[1L]]),
        rows = nrow(z),
        infeasible_rate = tva_bool_mean(z$infeasible),
        success_rate = tva_bool_mean(z$success),
        median_width = tva_median(z$width),
        median_width_ratio_to_tcsp = tva_median(
          z$adjudicated_width_ratio_to_reference %||% z$width_ratio_to_reference
        ),
        target_audit_digest_present_rate = mean(
          !is.na(z$target_audit_digest) & nzchar(as.character(z$target_audit_digest))
        ),
        endpoint_order_pass_rate = mean(
          is.finite(tva_num(z$lower)) & is.finite(tva_num(z$upper)) &
            tva_num(z$lower) <= tva_num(z$upper)
        ),
        certificate_status = "tolerance_package_young_mathew_nominal",
        stringsAsFactors = FALSE
      )
    }
  )
  out <- do.call(rbind, rows)
  out <- out[order(out$run_label, out$mode, out$dgp_id, out$n,
                   out$guaranteed_content), , drop = FALSE]
  rownames(out) <- NULL
  out
}

tva_mti_ecm_stationarity_audit <- function(results) {
  df <- tva_primary_analysis_rows(results)
  df <- df[df$method_id == "tcsp_mti_ecm_map_mc", , drop = FALSE]
  if (!nrow(df)) return(data.frame())
  rows <- lapply(
    tva_group_split(df, c("run_label", "mode", "dgp_id", "n",
                          "guaranteed_content", "tolerance_confidence")),
    function(z) {
      feasible <- !(tva_as_logical(z$infeasible) %in% TRUE)
      data.frame(
        run_label = z$run_label[[1L]],
        mode = z$mode[[1L]],
        dgp_id = z$dgp_id[[1L]],
        n = z$n[[1L]],
        guaranteed_content = z$guaranteed_content[[1L]],
        tolerance_confidence = z$tolerance_confidence[[1L]],
        rows = nrow(z),
        feasible_rows = sum(feasible, na.rm = TRUE),
        infeasible_rate = tva_bool_mean(z$infeasible),
        success_rate = tva_bool_mean(z$success),
        ecm_convergence_rate = tva_bool_mean(z$ecm_converged),
        mean_ecm_iterations = tva_mean(z$ecm_iterations),
        median_ecm_relative_objective_drop =
          tva_median(z$ecm_relative_objective_drop),
        mean_ecm_final_stationarity = tva_mean(z$ecm_final_stationarity),
        p95_ecm_final_stationarity =
          tva_quantile(z$ecm_final_stationarity, 0.95),
        max_ecm_final_stationarity = {
          x <- tva_num(z$ecm_final_stationarity)
          x <- x[is.finite(x)]
          if (length(x)) max(x) else NA_real_
        },
        stationarity_pass_rate_1e3 =
          tva_bool_mean(z$ecm_stationarity_pass_1e3),
        median_width_ratio_to_tcsp = tva_median(
          z$adjudicated_width_ratio_to_reference %||% z$width_ratio_to_reference
        ),
        hard_cell_needs_sensitivity =
          is.finite(tva_quantile(z$ecm_final_stationarity, 0.95)) &&
          tva_quantile(z$ecm_final_stationarity, 0.95) > 1e-3,
        stringsAsFactors = FALSE
      )
    }
  )
  out <- do.call(rbind, rows)
  out <- out[order(out$run_label, out$mode, out$dgp_id, out$n,
                   out$guaranteed_content), , drop = FALSE]
  rownames(out) <- NULL
  out
}

tva_mti_gibbs_budget_audit <- function(results) {
  df <- tva_primary_analysis_rows(results)
  df <- df[df$method_id == "tcsp_mti_gibbs_median_mc", , drop = FALSE]
  if (!nrow(df)) return(data.frame())
  rows <- lapply(
    tva_group_split(df, c("run_label", "mode", "dgp_id", "n",
                          "guaranteed_content", "tolerance_confidence")),
    function(z) {
      draws <- tva_num(z$posterior_draws)
      data.frame(
        run_label = z$run_label[[1L]],
        mode = z$mode[[1L]],
        dgp_id = z$dgp_id[[1L]],
        n = z$n[[1L]],
        guaranteed_content = z$guaranteed_content[[1L]],
        tolerance_confidence = z$tolerance_confidence[[1L]],
        rows = nrow(z),
        infeasible_rate = tva_bool_mean(z$infeasible),
        success_rate = tva_bool_mean(z$success),
        mean_posterior_draws = tva_mean(z$posterior_draws),
        min_posterior_draws = {
          x <- draws[is.finite(draws)]
          if (length(x)) min(x) else NA_real_
        },
        median_mcmc_n_burn = tva_median(z$mcmc_n_burn),
        median_mcmc_n_mcmc = tva_median(z$mcmc_n_mcmc),
        median_mcmc_thin = tva_median(z$mcmc_thin),
        screening_budget_rate = tva_bool_mean(z$mti_gibbs_screening_budget),
        needs_targeted_gibbs_diagnostic =
          any(z$mti_gibbs_screening_budget, na.rm = TRUE) &&
          tva_bool_mean(z$infeasible) < 1,
        median_width_ratio_to_tcsp = tva_median(
          z$adjudicated_width_ratio_to_reference %||% z$width_ratio_to_reference
        ),
        stringsAsFactors = FALSE
      )
    }
  )
  out <- do.call(rbind, rows)
  out <- out[order(out$run_label, out$mode, out$dgp_id, out$n,
                   out$guaranteed_content), , drop = FALSE]
  rownames(out) <- NULL
  out
}

tva_cell_level_summary <- function(results) {
  df <- tva_primary_analysis_rows(results)
  focus <- c("tcsp_mc", "tcsp_mti_ecm_map_mc", "tcsp_mti_gibbs_median_mc",
             "wilks_minmax", "young_mathew", "tcsp_dkw", "hdp_s_mc",
             "split_empirical_shortest", "split_ecm_fixed_tilt",
             "oracle_sh")
  df <- df[df$method_id %in% focus, , drop = FALSE]
  rows <- lapply(
    tva_group_split(df, c("run_label", "mode", "n", "guaranteed_content",
                          "tolerance_confidence", "method_id")),
    function(z) {
      data.frame(
        run_label = z$run_label[[1L]],
        mode = z$mode[[1L]],
        n = z$n[[1L]],
        guaranteed_content = z$guaranteed_content[[1L]],
        tolerance_confidence = z$tolerance_confidence[[1L]],
        method_id = z$method_id[[1L]],
        dgp_count = length(unique(z$dgp_id)),
        rows = nrow(z),
        infeasible_rate = tva_bool_mean(z$infeasible),
        success_rate = tva_bool_mean(z$success),
        certifies_rate = tva_bool_mean(z$certifies_requested_statement),
        median_width = tva_median(z$width),
        median_width_ratio_to_tcsp = tva_median(
          z$adjudicated_width_ratio_to_reference %||% z$width_ratio_to_reference
        ),
        median_width_ratio_to_oracle = tva_median(z$width_ratio_to_oracle_sh),
        median_elapsed_sec = tva_median(z$elapsed_sec),
        dominant_failure_taxonomy = names(sort(table(z$failure_taxonomy),
                                               decreasing = TRUE))[[1L]],
        certificate_status = tva_first_nonmissing(z$certificate_status,
                                                  NA_character_),
        stringsAsFactors = FALSE
      )
    }
  )
  out <- do.call(rbind, rows)
  out <- out[order(out$run_label, out$mode, out$guaranteed_content,
                   out$n, out$method_id), , drop = FALSE]
  rownames(out) <- NULL
  out
}

tva_design_cell_role <- function(n, content, confidence) {
  n <- as.integer(n)
  content <- as.numeric(content)
  confidence <- as.numeric(confidence)
  threshold_n <- vapply(seq_along(content), function(i) {
    k <- 2L
    repeat {
      if (tva_full_range_certificate(k, content[[i]]) >= confidence[[i]]) {
        return(k)
      }
      k <- k + 1L
    }
  }, integer(1L))
  ifelse(n == threshold_n, "threshold",
         ifelse(content >= 0.99 & n %in% c(500L, 1000L), "hard_large",
                ifelse(n >= 500L, "large_reference", "stress")))
}

tva_main_candidate_table <- function(cell_summary) {
  if (!nrow(cell_summary)) return(cell_summary)
  methods <- c("tcsp_mc", "young_mathew", "wilks_minmax",
               "tcsp_mti_ecm_map_mc", "tcsp_mti_gibbs_median_mc",
               "tcsp_dkw")
  out <- cell_summary[cell_summary$method_id %in% methods, , drop = FALSE]
  out$design_cell_role <- tva_design_cell_role(
    out$n, out$guaranteed_content, out$tolerance_confidence
  )
  keep <- out$design_cell_role %in% c("threshold", "hard_large") |
    (out$n == 1000 & abs(out$guaranteed_content - 0.90) < 1e-12)
  out <- out[keep, , drop = FALSE]
  out$formal_status <- ifelse(
    out$method_id %in% c("tcsp_mc", "tcsp_dkw"),
    "scan-certified action",
    ifelse(out$method_id == "wilks_minmax",
           "exact full-range order statistic",
           ifelse(out$method_id == "young_mathew",
                  "external nonparametric tolerance comparator",
                  "conditional MTI fitted summary"))
  )
  out$main_claim_ready <- with(out,
    (method_id %in% c("tcsp_mc", "wilks_minmax", "young_mathew") &
       (is.na(infeasible_rate) | infeasible_rate < 1)) |
      (method_id == "tcsp_mti_ecm_map_mc" &
         (is.na(infeasible_rate) | infeasible_rate < 1)) |
      (method_id == "tcsp_mti_gibbs_median_mc" &
         dominant_failure_taxonomy != "fixed_target_mti_infeasible" &
         !grepl("screening", certificate_status, fixed = TRUE))
  )
  out <- out[order(out$guaranteed_content, out$n, out$method_id,
                   out$run_label), , drop = FALSE]
  rownames(out) <- NULL
  out
}

tva_write_plot_wilks <- function(wilks, path) {
  if (!nrow(wilks)) return(FALSE)
  grDevices::png(path, width = 1100, height = 780, res = 120)
  on.exit(grDevices::dev.off(), add = TRUE)
  par(mar = c(5, 5, 3, 2))
  x <- wilks$n
  y <- wilks$wilks_full_range_certificate
  col <- ifelse(wilks$certifies_requested_statement, "#1B7837", "#B2182B")
  pch <- ifelse(wilks$tolerance_confidence >= 0.95, 19, 17)
  plot(x, y, log = "x", pch = pch, col = col, cex = 1.3,
       xlab = "Sample size n", ylab = "Exact full-range certificate",
       main = "Wilks Min-Max Certificate By Content And Confidence",
       ylim = c(0, 1))
  abline(h = unique(wilks$tolerance_confidence), col = "gray70", lty = 2)
  labels <- sprintf("c=%.2f, conf=%.2f", wilks$guaranteed_content,
                    wilks$tolerance_confidence)
  text(x, pmin(0.98, y + 0.035), labels = labels, cex = 0.62)
  legend("bottomright", bty = "n",
         legend = c("certifies", "does not certify"),
         col = c("#1B7837", "#B2182B"), pch = 19)
  TRUE
}

tva_write_plot_success_width <- function(cell_summary, path) {
  df <- cell_summary[cell_summary$method_id %in% c(
    "tcsp_mc", "young_mathew", "wilks_minmax", "tcsp_mti_ecm_map_mc",
    "tcsp_mti_gibbs_median_mc"
  ), , drop = FALSE]
  df <- df[is.finite(df$success_rate) &
             is.finite(df$median_width_ratio_to_tcsp), , drop = FALSE]
  if (!nrow(df)) return(FALSE)
  methods <- sort(unique(df$method_id))
  cols <- setNames(c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E",
                     "#E6AB02")[seq_along(methods)], methods)
  grDevices::png(path, width = 1200, height = 850, res = 120)
  on.exit(grDevices::dev.off(), add = TRUE)
  contents <- sort(unique(df$guaranteed_content))
  par(mfrow = c(1, length(contents)), mar = c(5, 5, 3, 1))
  for (cc in contents) {
    z <- df[abs(df$guaranteed_content - cc) < 1e-12, , drop = FALSE]
    plot(z$median_width_ratio_to_tcsp, z$success_rate,
         xlab = "Median width ratio to TCSP",
         ylab = "Empirical success",
         main = sprintf("Content %.2f", cc),
         xlim = range(c(0.85, z$median_width_ratio_to_tcsp), na.rm = TRUE),
         ylim = range(c(0.85, 1.01, z$success_rate), na.rm = TRUE),
         pch = 19, col = cols[z$method_id], cex = 1.2)
    abline(h = unique(z$tolerance_confidence), col = "gray70", lty = 2)
    text(z$median_width_ratio_to_tcsp, z$success_rate,
         labels = paste(z$method_id, z$n, sep = "\n"),
         pos = 4, cex = 0.55)
  }
  par(mfrow = c(1, 1))
  TRUE
}

tva_write_plot_ecm <- function(ecm, path) {
  ecm <- ecm[is.finite(ecm$mean_ecm_final_stationarity), , drop = FALSE]
  if (!nrow(ecm)) return(FALSE)
  grDevices::png(path, width = 1100, height = 780, res = 120)
  on.exit(grDevices::dev.off(), add = TRUE)
  par(mar = c(9, 5, 3, 2))
  labels <- paste(ecm$mode, ecm$dgp_id, ecm$n,
                  sprintf("c=%.2f", ecm$guaranteed_content), sep = "\n")
  plot(seq_len(nrow(ecm)), ecm$mean_ecm_final_stationarity,
       ylim = range(c(0, ecm$max_ecm_final_stationarity, 1e-3), na.rm = TRUE),
       xaxt = "n", xlab = "", ylab = "Final stationarity",
       main = "MTI ECM Stationarity Audit", pch = 19, col = "#1B9E77")
  segments(seq_len(nrow(ecm)), ecm$p95_ecm_final_stationarity,
           seq_len(nrow(ecm)), ecm$max_ecm_final_stationarity,
           col = "#1B9E77")
  abline(h = 1e-3, col = "#B2182B", lty = 2)
  axis(1, at = seq_len(nrow(ecm)), labels = labels, las = 2, cex.axis = 0.45)
  TRUE
}

tva_write_plot_taxonomy <- function(taxonomy, path) {
  if (!nrow(taxonomy)) return(FALSE)
  tab <- xtabs(rows ~ failure_taxonomy + method_id, data = taxonomy)
  if (!length(tab)) return(FALSE)
  grDevices::png(path, width = 1200, height = 850, res = 120)
  on.exit(grDevices::dev.off(), add = TRUE)
  par(mar = c(9, 5, 3, 10), xpd = TRUE)
  barplot(tab, las = 2, col = grDevices::hcl.colors(nrow(tab), "Dark 3"),
          ylab = "Primary-analysis rows",
          main = "Tolerance Validation Failure Taxonomy")
  legend("topright", inset = c(-0.28, 0), legend = rownames(tab),
         fill = grDevices::hcl.colors(nrow(tab), "Dark 3"), bty = "n",
         cex = 0.75)
  TRUE
}

tva_artifact_hashes <- function(output_dir, files) {
  data.frame(
    artifact = files,
    path = normalizePath(file.path(output_dir, files), winslash = "/",
                         mustWork = TRUE),
    size_bytes = file.info(file.path(output_dir, files))$size,
    sha256 = vapply(file.path(output_dir, files), tva_hash_file, character(1L)),
    stringsAsFactors = FALSE
  )
}

tva_write_readme <- function(output_dir) {
  lines <- c(
    "# Tolerance Validation Adjudication Bundle",
    "",
    "This ignored bundle adjudicates completed tolerance-validation outputs.",
    "It separates returned intervals, formal certification, empirical repeated-sample success, and numerical diagnostics.",
    "",
    "Core files:",
    "",
    "- `method_failure_taxonomy.csv`: method/cell classification by failure or evidence type.",
    "- `wilks_certificate_audit.csv`: exact full-range Wilks certificates.",
    "- `young_mathew_contract_audit.csv`: package-call contract summary for the Young-Mathew comparator.",
    "- `mti_ecm_stationarity_audit.csv`: ECM stationarity and objective diagnostics.",
    "- `mti_gibbs_budget_audit.csv`: Gibbs budget and screening-evidence flags.",
    "- `cell_level_scientific_summary.csv`: collapsed method/cell evidence summary.",
    "- `main_text_candidate_table.csv`: compact candidate table for manuscript discussion.",
    "- `supplement_candidate_table.csv`: more granular candidate table for supplement use.",
    "",
    "The bundle is diagnostic. Manuscript source should be updated only after the decision gates are reviewed."
  )
  writeLines(lines, file.path(output_dir, "README.md"))
}

tva_run_adjudication <- function(main_run_dir, ecm_run_dir, small95_run_dir,
                                 paper90_run_dir, output_dir,
                                 allow_incomplete = FALSE,
                                 require_clean = FALSE,
                                 source_commit = tva_git_commit(repo_root)) {
  tva_require_packages(c("jsonlite", "digest"))
  if (require_clean) {
    status <- tva_git_status(repo_root)
    if (length(grep("^\\?\\?|^ M|^M |^A |^ A|^D |^ D", strsplit(status, "\n")[[1L]]))) {
      tva_stop("Working tree is not clean.")
    }
  }
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
  if (file.exists(output_dir) || dir.exists(output_dir)) {
    tva_stop("The output directory must be fresh: ", output_dir)
  }
  dir.create(dirname(output_dir), recursive = TRUE, showWarnings = FALSE)
  staging <- tempfile(paste0(".", basename(output_dir), "-"),
                      tmpdir = dirname(output_dir))
  dir.create(staging, recursive = TRUE, showWarnings = FALSE)
  published <- FALSE
  on.exit({
    if (!published) unlink(staging, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  inputs <- list(
    main = tva_read_run_results("main", main_run_dir, allow_incomplete,
                                include_young_mathew_addon = TRUE),
    ecm200 = tva_read_run_results("ecm200", ecm_run_dir, allow_incomplete,
                                  include_young_mathew_addon = FALSE),
    small95 = tva_read_run_results("small95", small95_run_dir,
                                   allow_incomplete,
                                   include_young_mathew_addon = FALSE),
    paper90 = tva_read_run_results("paper90", paper90_run_dir,
                                   allow_incomplete,
                                   include_young_mathew_addon = FALSE)
  )
  results <- do.call(tva_bind_fill, lapply(inputs, `[[`, "results"))
  results <- tva_add_reference_widths(results)
  results <- tva_classify_rows(results)
  input_hashes <- do.call(rbind, lapply(inputs, `[[`, "input_hashes"))

  observed_cells <- unique(results[, c("n", "guaranteed_content",
                                       "tolerance_confidence"), drop = FALSE])
  wilks <- tva_wilks_certificate_frame(
    observed_cells$n, observed_cells$guaranteed_content,
    observed_cells$tolerance_confidence
  )
  wilks <- merge(observed_cells, wilks,
                 by = c("n", "guaranteed_content", "tolerance_confidence"),
                 all.x = TRUE, sort = FALSE)
  wilks <- wilks[order(wilks$guaranteed_content, wilks$tolerance_confidence,
                       wilks$n), , drop = FALSE]

  taxonomy <- tva_summarize_taxonomy(results)
  ym <- tva_young_mathew_contract_audit(results)
  ecm <- tva_mti_ecm_stationarity_audit(results)
  gibbs <- tva_mti_gibbs_budget_audit(results)
  cell_summary <- tva_cell_level_summary(results)
  main_table <- tva_main_candidate_table(cell_summary)
  supplement_table <- cell_summary

  utils::write.csv(input_hashes, file.path(staging, "input_artifact_hashes.csv"),
                   row.names = FALSE)
  utils::write.csv(taxonomy, file.path(staging, "method_failure_taxonomy.csv"),
                   row.names = FALSE)
  utils::write.csv(wilks, file.path(staging, "wilks_certificate_audit.csv"),
                   row.names = FALSE)
  utils::write.csv(ym, file.path(staging, "young_mathew_contract_audit.csv"),
                   row.names = FALSE)
  utils::write.csv(ecm, file.path(staging, "mti_ecm_stationarity_audit.csv"),
                   row.names = FALSE)
  utils::write.csv(gibbs, file.path(staging, "mti_gibbs_budget_audit.csv"),
                   row.names = FALSE)
  utils::write.csv(cell_summary,
                   file.path(staging, "cell_level_scientific_summary.csv"),
                   row.names = FALSE)
  utils::write.csv(main_table, file.path(staging, "main_text_candidate_table.csv"),
                   row.names = FALSE)
  utils::write.csv(supplement_table,
                   file.path(staging, "supplement_candidate_table.csv"),
                   row.names = FALSE)

  plot_files <- character()
  if (tva_write_plot_wilks(wilks, file.path(staging, "wilks_certificate_plot.png"))) {
    plot_files <- c(plot_files, "wilks_certificate_plot.png")
  }
  if (tva_write_plot_success_width(cell_summary,
                                   file.path(staging, "success_width_tradeoff.png"))) {
    plot_files <- c(plot_files, "success_width_tradeoff.png")
  }
  if (tva_write_plot_ecm(ecm, file.path(staging, "mti_ecm_stationarity_plot.png"))) {
    plot_files <- c(plot_files, "mti_ecm_stationarity_plot.png")
  }
  if (tva_write_plot_taxonomy(taxonomy,
                              file.path(staging, "failure_taxonomy_plot.png"))) {
    plot_files <- c(plot_files, "failure_taxonomy_plot.png")
  }
  tva_write_readme(staging)

  artifact_files <- c(
    "README.md",
    "input_artifact_hashes.csv",
    "method_failure_taxonomy.csv",
    "wilks_certificate_audit.csv",
    "young_mathew_contract_audit.csv",
    "mti_ecm_stationarity_audit.csv",
    "mti_gibbs_budget_audit.csv",
    "cell_level_scientific_summary.csv",
    "main_text_candidate_table.csv",
    "supplement_candidate_table.csv",
    plot_files
  )
  artifact_hashes <- tva_artifact_hashes(staging, artifact_files)
  utils::write.csv(artifact_hashes, file.path(staging, "artifact_hashes.csv"),
                   row.names = FALSE)

  gates <- list(
    all_runs_complete = all(vapply(inputs, function(x) {
      h <- x$health
      isTRUE(h$final_artifacts_present) &&
        (is.null(h$rows_remaining) || as.integer(h$rows_remaining) == 0L) &&
        (is.null(h$waves_failed) || as.integer(h$waves_failed) == 0L)
    }, logical(1L))),
    wilks_certificates_present = nrow(wilks) > 0L &&
      all(is.finite(wilks$wilks_full_range_certificate)),
    young_mathew_contract_present = nrow(ym) > 0L,
    mti_ecm_stationarity_present = nrow(ecm) > 0L &&
      any(is.finite(ecm$mean_ecm_final_stationarity)),
    mti_gibbs_screening_budget_detected = nrow(gibbs) > 0L &&
      any(gibbs$needs_targeted_gibbs_diagnostic, na.rm = TRUE),
    ecm_hard_cell_sensitivity_needed = nrow(ecm) > 0L &&
      any(ecm$hard_cell_needs_sensitivity, na.rm = TRUE)
  )
  manifest <- list(
    schema_version = "rqrgibbs_tolerance_validation_adjudication/1.0.0",
    generated_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    repo_root = repo_root,
    source_commit = source_commit,
    git_status = tva_git_status(repo_root),
    script_path = script_path,
    script_sha256 = tva_hash_file(script_path),
    package_versions = list(
      rqrgibbs = if (requireNamespace("rqrgibbs", quietly = TRUE)) {
        as.character(utils::packageVersion("rqrgibbs"))
      } else {
        NA_character_
      },
      tolerance = if (requireNamespace("tolerance", quietly = TRUE)) {
        as.character(utils::packageVersion("tolerance"))
      } else {
        NA_character_
      },
      jsonlite = as.character(utils::packageVersion("jsonlite")),
      digest = as.character(utils::packageVersion("digest"))
    ),
    run_dirs = list(
      main = normalizePath(main_run_dir, winslash = "/", mustWork = TRUE),
      ecm200 = normalizePath(ecm_run_dir, winslash = "/", mustWork = TRUE),
      small95 = normalizePath(small95_run_dir, winslash = "/",
                              mustWork = TRUE),
      paper90 = normalizePath(paper90_run_dir, winslash = "/",
                              mustWork = TRUE)
    ),
    rows = list(
      combined_raw_rows = nrow(results),
      primary_analysis_rows = nrow(tva_primary_analysis_rows(results)),
      taxonomy_rows = nrow(taxonomy),
      wilks_certificate_rows = nrow(wilks),
      young_mathew_contract_rows = nrow(ym),
      ecm_audit_rows = nrow(ecm),
      gibbs_audit_rows = nrow(gibbs),
      main_text_candidate_rows = nrow(main_table)
    ),
    gates = gates,
    recommendation = if (isTRUE(gates$mti_gibbs_screening_budget_detected)) {
      "Run targeted MTI Gibbs diagnostics before using Gibbs in main manuscript claims."
    } else {
      "No targeted MTI Gibbs diagnostics required by this adjudication."
    },
    artifact_hashes = artifact_hashes
  )
  jsonlite::write_json(manifest, file.path(staging, "adjudication_manifest.json"),
                       pretty = TRUE, auto_unbox = TRUE)
  artifact_files <- c(artifact_files, "adjudication_manifest.json")
  artifact_hashes <- tva_artifact_hashes(staging, artifact_files)
  utils::write.csv(artifact_hashes, file.path(staging, "artifact_hashes.csv"),
                   row.names = FALSE)

  file.rename(staging, output_dir)
  published <- TRUE
  output_dir
}

tva_main <- function(args = commandArgs(trailingOnly = TRUE)) {
  tva_require_packages(c("jsonlite", "digest"))
  setwd(repo_root)
  default_output <- file.path(
    "application", "outputs", "tolerance_validation_adjudication",
    paste0("adjudication_", format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"))
  )
  main_run_dir <- tva_arg_value(args, "--main-run-dir=", NULL)
  ecm_run_dir <- tva_arg_value(args, "--ecm-run-dir=", NULL)
  small95_run_dir <- tva_arg_value(args, "--small95-run-dir=", NULL)
  paper90_run_dir <- tva_arg_value(args, "--paper90-run-dir=", NULL)
  output_dir <- tva_arg_value(args, "--output-dir=", default_output)
  allow_incomplete <- tva_bool_arg(tva_arg_value(args, "--allow-incomplete=", NULL),
                                   default = FALSE)
  require_clean <- tva_bool_arg(tva_arg_value(args, "--require-clean=", NULL),
                                default = FALSE)
  source_commit <- tva_arg_value(args, "--source-commit=", tva_git_commit())
  out <- tva_run_adjudication(
    main_run_dir = main_run_dir,
    ecm_run_dir = ecm_run_dir,
    small95_run_dir = small95_run_dir,
    paper90_run_dir = paper90_run_dir,
    output_dir = output_dir,
    allow_incomplete = allow_incomplete,
    require_clean = require_clean,
    source_commit = source_commit
  )
  cat("Tolerance validation adjudication written to:", out, "\n")
  invisible(out)
}

if (!identical(Sys.getenv("RQRGIBBS_ADJUDICATION_SOURCE_ONLY"), "true")) {
  tva_main()
}
