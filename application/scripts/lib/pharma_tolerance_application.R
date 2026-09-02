`%||%` <- function(a, b) if (is.null(a)) b else a

pta_stop <- function(...) stop(paste0(...), call. = FALSE)

pta_arg_value <- function(args, prefix, default = NULL) {
  hit <- args[startsWith(args, prefix)]
  if (length(hit)) sub(prefix, "", hit[[1L]], fixed = TRUE) else default
}

pta_script_root <- function(default_script) {
  arguments <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  script_path <- sub(file_arg, "", arguments[startsWith(arguments, file_arg)][1L])
  if (!length(script_path) || is.na(script_path)) script_path <- default_script
  script_path <- normalizePath(script_path, winslash = "/", mustWork = TRUE)
  normalizePath(file.path(dirname(script_path), "..", ".."),
                winslash = "/", mustWork = TRUE)
}

pta_require_packages <- function(packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1L),
                              quietly = TRUE)]
  if (length(missing)) {
    pta_stop("Required package(s) are not installed: ",
             paste(missing, collapse = ", "))
  }
  invisible(TRUE)
}

pta_read_config <- function(path) {
  pta_require_packages("jsonlite")
  jsonlite::read_json(normalizePath(path, winslash = "/", mustWork = TRUE),
                      simplifyVector = FALSE)
}

pta_config_digest <- function(path) {
  if (!requireNamespace("digest", quietly = TRUE)) return(NA_character_)
  digest::digest(normalizePath(path, winslash = "/", mustWork = TRUE),
                 algo = "sha256", file = TRUE)
}

pta_file_sha256 <- function(path) {
  if (!requireNamespace("digest", quietly = TRUE)) return(NA_character_)
  digest::digest(path, algo = "sha256", file = TRUE)
}

pta_file_md5 <- function(path) unname(tools::md5sum(path))

pta_atomic_write_csv <- function(x, path, row.names = FALSE) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(paste0(".", basename(path), "-"), tmpdir = dirname(path))
  utils::write.csv(x, tmp, row.names = row.names)
  if (file.exists(path)) unlink(path)
  ok <- file.rename(tmp, path)
  if (!isTRUE(ok)) pta_stop("Could not write file: ", path)
  invisible(path)
}

pta_atomic_write_json <- function(x, path) {
  pta_require_packages("jsonlite")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(paste0(".", basename(path), "-"), tmpdir = dirname(path))
  jsonlite::write_json(x, tmp, pretty = TRUE, auto_unbox = TRUE, digits = NA)
  if (file.exists(path)) unlink(path)
  ok <- file.rename(tmp, path)
  if (!isTRUE(ok)) pta_stop("Could not write file: ", path)
  invisible(path)
}

pta_read_csv <- function(path) {
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

pta_path <- function(repo_root, path) {
  normalizePath(file.path(repo_root, path), winslash = "/", mustWork = FALSE)
}

pta_numeric <- function(x) suppressWarnings(as.numeric(x))

pta_quantile <- function(x, prob) {
  x <- pta_numeric(x)
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  unname(stats::quantile(x, prob, names = FALSE, type = 8))
}

pta_mean <- function(x) {
  x <- pta_numeric(x)
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  mean(x)
}

pta_median <- function(x) {
  x <- pta_numeric(x)
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  stats::median(x)
}

pta_skewness <- function(x) {
  x <- pta_numeric(x)
  x <- x[is.finite(x)]
  if (length(x) < 3L) return(NA_real_)
  s <- stats::sd(x)
  if (!is.finite(s) || s <= 0) return(NA_real_)
  mean(((x - mean(x)) / s)^3)
}

pta_escape_latex <- function(x) {
  x <- as.character(x)
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([_%&#])", "\\\\\\1", x, perl = TRUE)
  x
}

pta_format_num <- function(x, digits = 3) {
  x <- as.numeric(x)
  ifelse(is.finite(x), formatC(x, digits = digits, format = "f"), "--")
}

pta_format_pct <- function(x, digits = 1) {
  x <- as.numeric(x)
  ifelse(is.finite(x), formatC(100 * x, digits = digits, format = "f"), "--")
}

pta_format_range <- function(lo, hi, digits = 3) {
  paste0(pta_format_num(lo, digits), "--", pta_format_num(hi, digits))
}

pta_format_pct_range <- function(lo, hi, digits = 1) {
  paste0(pta_format_pct(lo, digits), "--", pta_format_pct(hi, digits))
}

pta_download_data <- function(config, repo_root, overwrite = FALSE) {
  raw_path <- pta_path(repo_root, config$paths$raw_file)
  dir.create(dirname(raw_path), recursive = TRUE, showWarnings = FALSE)
  if (!file.exists(raw_path) || isTRUE(overwrite)) {
    utils::download.file(
      url = config$data_source$download_url,
      destfile = raw_path,
      mode = "wb",
      quiet = TRUE
    )
  }
  expected_bytes <- as.numeric(config$data_source$expected_bytes)[1L]
  actual_bytes <- file.info(raw_path)$size
  if (!is.finite(actual_bytes) || actual_bytes != expected_bytes) {
    pta_stop("Downloaded file has unexpected byte count: ", actual_bytes,
             " instead of ", expected_bytes, ".")
  }
  expected_md5 <- as.character(config$data_source$expected_md5)
  actual_md5 <- pta_file_md5(raw_path)
  if (!identical(tolower(actual_md5), tolower(expected_md5))) {
    pta_stop("Downloaded file has unexpected MD5: ", actual_md5)
  }
  raw_path
}

pta_load_raw_data <- function(path) {
  data <- utils::read.csv(path, sep = ";", stringsAsFactors = FALSE,
                          check.names = FALSE)
  required <- c(
    "batch", "code", "strength", "size", "start", "api_code", "api_batch",
    "fct_tensile", "tbl_rsd_weight", "fct_av_hardness"
  )
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    pta_stop("Laboratory data are missing required column(s): ",
             paste(missing, collapse = ", "))
  }
  numeric_cols <- setdiff(names(data), c("strength", "start"))
  for (col in numeric_cols) data[[col]] <- pta_numeric(data[[col]])
  data$.source_row <- seq_len(nrow(data))
  data
}

pta_code23_data <- function(config, raw_data) {
  product_code <- as.integer(config$analysis$product_code)[1L]
  data <- raw_data[as.integer(raw_data$code) == product_code, , drop = FALSE]
  if (!nrow(data)) pta_stop("No rows found for product code ", product_code, ".")
  data <- data[order(data$batch, data$.source_row), , drop = FALSE]
  data$.analysis_row <- seq_len(nrow(data))
  q <- stats::quantile(data$batch, probs = seq(0, 1, length.out = 5),
                       names = FALSE, type = 1, na.rm = TRUE)
  data$batch_order_quartile <- cut(
    data$batch,
    breaks = unique(q),
    include.lowest = TRUE,
    labels = FALSE
  )
  if (length(unique(q)) < 5L) {
    data$batch_order_quartile <- ceiling(seq_len(nrow(data)) /
                                           ceiling(nrow(data) / 4))
    data$batch_order_quartile <- pmin(data$batch_order_quartile, 4L)
  }
  data$batch_order_quartile <- paste0("Q", data$batch_order_quartile)
  data
}

pta_response_diagnostics <- function(data, config) {
  responses <- c(
    primary = config$analysis$primary_response,
    supplement = config$analysis$supplement_response,
    excluded = config$analysis$excluded_response
  )
  rows <- lapply(names(responses), function(role) {
    response <- responses[[role]]
    y <- pta_numeric(data[[response]])
    observed <- y[is.finite(y)]
    tie_tab <- table(observed)
    data.frame(
      response_id = response,
      role = role,
      n = length(y),
      missing = sum(!is.finite(y)),
      distinct_values = length(unique(observed)),
      largest_tie = if (length(tie_tab)) max(as.integer(tie_tab)) else NA_integer_,
      minimum = min(observed),
      q25 = pta_quantile(observed, 0.25),
      median = stats::median(observed),
      mean = mean(observed),
      q75 = pta_quantile(observed, 0.75),
      maximum = max(observed),
      sd = stats::sd(observed),
      skewness = pta_skewness(observed),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

pta_write_response_diagnostics_tex <- function(diagnostics, path) {
  role_label <- c(
    primary = "Main",
    supplement = "Supplement",
    excluded = "Excluded"
  )
  body <- vapply(seq_len(nrow(diagnostics)), function(ii) {
    sprintf(
      "%s & %s & %d & %d & %d & %s & %s & %s \\\\",
      pta_escape_latex(role_label[diagnostics$role[[ii]]]),
      pta_escape_latex(diagnostics$response_id[[ii]]),
      as.integer(diagnostics$n[[ii]]),
      as.integer(diagnostics$distinct_values[[ii]]),
      as.integer(diagnostics$largest_tie[[ii]]),
      pta_format_num(diagnostics$median[[ii]], 3),
      pta_format_num(diagnostics$sd[[ii]], 3),
      pta_format_num(diagnostics$skewness[[ii]], 2)
    )
  }, character(1L))
  lines <- c(
    "\\begin{tabular}{@{}llrrrrrr@{}}",
    "\\toprule",
    "Role & Response & Batches & Distinct & Largest tie & Median & SD & Skewness\\\\",
    "\\midrule",
    body,
    "\\bottomrule",
    "\\end{tabular}"
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, path)
}

pta_prepare_data <- function(config, repo_root, overwrite = FALSE) {
  raw_path <- pta_download_data(config, repo_root, overwrite = overwrite)
  raw_data <- pta_load_raw_data(raw_path)
  data <- pta_code23_data(config, raw_data)
  clean_path <- pta_path(repo_root, config$paths$clean_file)
  pta_atomic_write_csv(data, clean_path)

  diagnostics <- pta_response_diagnostics(data, config)
  table_dir <- pta_path(repo_root, config$paths$table_dir)
  figure_data_dir <- pta_path(repo_root, config$paths$figure_data_dir)
  pta_atomic_write_csv(
    diagnostics,
    file.path(table_dir, "pharma_application_response_diagnostics.csv")
  )
  pta_write_response_diagnostics_tex(
    diagnostics,
    file.path(table_dir, "pharma_application_response_diagnostics.tex")
  )
  provenance <- data.frame(
    source_name = config$data_source$name,
    source_url = config$data_source$figshare_item_url,
    download_url = config$data_source$download_url,
    doi = config$data_source$doi,
    license = config$data_source$license,
    bytes = file.info(raw_path)$size,
    md5 = pta_file_md5(raw_path),
    sha256 = pta_file_sha256(raw_path),
    raw_rows = nrow(raw_data),
    raw_columns = length(setdiff(names(raw_data), ".source_row")),
    product_code = as.integer(config$analysis$product_code)[1L],
    product_rows = nrow(data),
    stringsAsFactors = FALSE
  )
  pta_atomic_write_csv(
    provenance,
    file.path(figure_data_dir, "pharma_application_data_provenance.csv")
  )
  list(
    raw_path = raw_path,
    clean_path = clean_path,
    diagnostics = diagnostics,
    provenance = provenance
  )
}

pta_load_clean_data <- function(config, repo_root) {
  clean_path <- pta_path(repo_root, config$paths$clean_file)
  if (!file.exists(clean_path)) {
    pta_prepare_data(config, repo_root, overwrite = FALSE)
  }
  pta_read_csv(clean_path)
}

pta_scan_calibration <- function(config, repo_root) {
  path <- pta_path(repo_root, config$tcsp$scan_calibration_csv)
  tab <- pta_read_csv(path)
  n <- as.integer(config$analysis$training_size)[1L]
  content <- as.numeric(config$analysis$content)[1L]
  confidence <- as.numeric(config$analysis$tolerance_confidence)[1L]
  content_col <- if ("content" %in% names(tab)) "content" else
    "guaranteed_content"
  conf_col <- if ("tolerance_confidence" %in% names(tab)) {
    "tolerance_confidence"
  } else {
    "confidence"
  }
  hit <- tab[
    as.integer(tab$n) == n &
      abs(as.numeric(tab[[content_col]]) - content) < 1e-12 &
      abs(as.numeric(tab[[conf_col]]) - confidence) < 1e-12,
    ,
    drop = FALSE
  ]
  if (nrow(hit) != 1L) {
    pta_stop("Expected one TCSP calibration row for n=", n,
             ", content=", content, ", confidence=", confidence, ".")
  }
  retained <- as.integer(hit$retained_count[[1L]])
  expected <- as.integer(config$tcsp$expected_retained_count)[1L]
  if (retained != expected) {
    pta_stop("TCSP retained count mismatch: ", retained,
             " instead of configured ", expected, ".")
  }
  lower_col <- intersect(c("lower_confidence_bound",
                           "certified_lower_probability"), names(hit))
  lower_bound <- if (length(lower_col)) as.numeric(hit[[lower_col[[1L]]]][[1L]])
  else NA_real_
  if (is.finite(lower_bound) &&
      lower_bound < as.numeric(config$tcsp$expected_lower_confidence_bound_min)) {
    pta_stop("TCSP scan lower bound is below the configured minimum.")
  }
  list(
    retained_count = retained,
    retained_fraction = retained / n,
    content_buffer = retained / n - content,
    lower_confidence_bound = lower_bound,
    source_path = path,
    source_sha256 = pta_file_sha256(path)
  )
}

pta_mti_policy_row <- function(config, repo_root) {
  path <- pta_path(repo_root, config$mti_ecm$policy_csv)
  tab <- pta_read_csv(path)
  n <- as.integer(config$analysis$training_size)[1L]
  content <- as.numeric(config$analysis$content)[1L]
  confidence <- as.numeric(config$analysis$tolerance_confidence)[1L]
  hit <- tab[
    tab$method_id == config$mti_ecm$method_id &
      as.integer(tab$n) == n &
      abs(as.numeric(tab$content) - content) < 1e-12 &
      abs(as.numeric(tab$tolerance_confidence) - confidence) < 1e-12,
    ,
    drop = FALSE
  ]
  if (nrow(hit) != 1L) {
    pta_stop("Expected one frozen MTI-ECM policy row for the application cell.")
  }
  if (!identical(as.character(hit$policy_id[[1L]]),
                 as.character(config$mti_ecm$policy_id))) {
    pta_stop("Frozen MTI-ECM policy id does not match the application config.")
  }
  if (as.numeric(hit$screen[[1L]]) <
      as.numeric(config$mti_ecm$required_screen)[1L] - 1e-12) {
    pta_stop("Frozen MTI-ECM policy uses a weaker screen than requested.")
  }
  if (as.numeric(hit$delivery_lower_bound[[1L]]) <
      as.numeric(config$mti_ecm$required_delivery_lower_bound_min)[1L]) {
    pta_stop("Frozen MTI-ECM policy lower bound is below the requested level.")
  }
  row <- as.list(hit[1L, , drop = FALSE])
  row$calibration_table_path <- path
  row$calibration_table_digest <- pta_file_sha256(path)
  row
}

pta_mti_profile_config <- function(config, repo_root) {
  validation_path <- pta_path(repo_root, config$mti_ecm$validation_config)
  validation <- pta_read_config(validation_path)
  methods <- setNames(validation$methods, vapply(validation$methods, `[[`,
                                                character(1L), "method_id"))
  base <- validation$engine_defaults$mti_ecm_dp_profile %||% list()
  source_cfg <- methods[[config$mti_ecm$source_method_id]]$profile_config %||%
    list()
  method_cfg <- methods[[config$mti_ecm$method_id]]$profile_config %||% list()
  cfg <- utils::modifyList(base, source_cfg)
  cfg <- utils::modifyList(cfg, method_cfg)
  cfg$policy_id <- cfg$adaptive_policy_id %||% config$mti_ecm$policy_id
  cfg$menu_id <- cfg$policy_id
  list(
    config = cfg,
    direct_dp = validation$engine_defaults$direct_dp,
    validation_path = validation_path,
    validation_digest = pta_file_sha256(validation_path)
  )
}

pta_make_splits <- function(N, train_n, replications, seed) {
  set.seed(as.integer(seed))
  lapply(seq_len(replications), function(ii) {
    train <- sort(sample.int(N, size = train_n, replace = FALSE))
    list(
      split_id = ii,
      seed = as.integer(seed + ii),
      train = train,
      heldout = setdiff(seq_len(N), train)
    )
  })
}

pta_extract_tolerance_interval <- function(raw) {
  df <- as.data.frame(raw, stringsAsFactors = FALSE)
  lower_cols <- grep("(^|\\.)[0-9.]*\\.lower$|lower", names(df),
                     ignore.case = TRUE, value = TRUE)
  upper_cols <- grep("(^|\\.)[0-9.]*\\.upper$|upper", names(df),
                     ignore.case = TRUE, value = TRUE)
  lower_cols <- lower_cols[vapply(df[lower_cols], is.numeric, logical(1L))]
  upper_cols <- upper_cols[vapply(df[upper_cols], is.numeric, logical(1L))]
  if (!length(lower_cols) || !length(upper_cols)) {
    pta_stop("Could not find numeric lower and upper columns in tolerance output.")
  }
  candidates <- expand.grid(lower_col = lower_cols, upper_col = upper_cols,
                            stringsAsFactors = FALSE)
  for (ii in seq_len(nrow(candidates))) {
    lower <- as.numeric(df[[candidates$lower_col[[ii]]]][[1L]])
    upper <- as.numeric(df[[candidates$upper_col[[ii]]]][[1L]])
    if (is.finite(lower) && is.finite(upper) && upper >= lower) {
      return(list(lower = lower, upper = upper, raw = df))
    }
  }
  pta_stop("No valid interval found in tolerance output.")
}

pta_fit_young_mathew <- function(y, content, confidence) {
  if (!requireNamespace("tolerance", quietly = TRUE)) {
    return(list(returned = FALSE, lower = NA_real_, upper = NA_real_,
                width = NA_real_,
                failure_reason = "tolerance package unavailable"))
  }
  out <- tryCatch({
    raw <- tolerance::nptol.int(
      x = y, alpha = 1 - confidence, P = content, side = 2, method = "YM"
    )
    interval <- pta_extract_tolerance_interval(raw)
    list(
      returned = TRUE,
      lower = interval$lower,
      upper = interval$upper,
      width = interval$upper - interval$lower,
      failure_reason = "",
      source_version = as.character(utils::packageVersion("tolerance"))
    )
  }, error = function(e) {
    list(returned = FALSE, lower = NA_real_, upper = NA_real_,
         width = NA_real_, failure_reason = conditionMessage(e),
         source_version = as.character(utils::packageVersion("tolerance")))
  })
  out
}

pta_fit_tcsp <- function(y, scan) {
  out <- tryCatch({
    window <- rqrgibbs::rqr_tcsp_shortest_window(
      y, retained_count = scan$retained_count, na_rm = FALSE
    )
    list(
      returned = TRUE,
      lower = window$lower_endpoint,
      upper = window$upper_endpoint,
      width = window$width,
      retained_count = scan$retained_count,
      retained_fraction = scan$retained_fraction,
      content_buffer = scan$content_buffer,
      lower_confidence_bound = scan$lower_confidence_bound,
      shortest_window_start = window$shortest_window_start,
      shortest_window_end = window$shortest_window_end,
      failure_reason = ""
    )
  }, error = function(e) {
    list(returned = FALSE, lower = NA_real_, upper = NA_real_,
         width = NA_real_, retained_count = scan$retained_count,
         retained_fraction = scan$retained_fraction,
         content_buffer = scan$content_buffer,
         lower_confidence_bound = scan$lower_confidence_bound,
         shortest_window_start = NA_integer_, shortest_window_end = NA_integer_,
         failure_reason = conditionMessage(e))
  })
  out
}

pta_fit_mti_ecm <- function(y, content, confidence, scan, policy_row,
                            profile, seed) {
  out <- tryCatch({
    cfg <- profile$config
    direct_dp <- profile$direct_dp
    base <- direct_dp$base
    if (!identical(base$family, "normal")) {
      pta_stop("Only the normal direct-DP base is supported here.")
    }
    menu <- rqrgibbs::rqr_mti_ecm_adaptive_profile_menu(
      y = y,
      content = content,
      tolerance_confidence = confidence,
      scan_target_content = scan$retained_fraction,
      policy = cfg$adaptive_policy %||% "cell",
      policy_config = cfg,
      calibration_rule = policy_row,
      na_rm = FALSE
    )
    ecm_control <- cfg$ecm_control %||% list()
    ecm_control$seed <- as.integer(seed)
    action <- rqrgibbs::rqr_mti_ecm_dp_profile_action(
      y = y,
      content = content,
      posterior_confidence = menu$posterior_confidence,
      dp_concentration = menu$dp_concentration,
      dp_base_measure = rqrgibbs::rqr_dp_base_normal(
        mean = as.numeric(base$mean)[1L],
        sd = as.numeric(base$sd)[1L]
      ),
      strict_bayes = isTRUE(cfg$strict_bayes %||% TRUE),
      scan_target_content = scan$retained_fraction,
      q_grid = menu$q_grid,
      q_grid_control = list(),
      tilt_grid_control = menu$tilt_grid_control,
      learning_rate = as.numeric(cfg$learning_rate %||% 1)[1L],
      beta_prior_obj = rqrgibbs::beta_prior(
        "ridge",
        ridge = list(tau2 = as.numeric(cfg$beta_ridge_tau2 %||% 1e4)[1L])
      ),
      ecm_control = ecm_control,
      expand_if_empty = isTRUE(cfg$expand_if_empty %||% TRUE),
      na_rm = FALSE
    )
    selected <- action$selected
    if (!nrow(selected)) {
      return(list(
        returned = FALSE,
        lower = NA_real_, upper = NA_real_, width = NA_real_,
        failure_reason = action$posterior_constraint_status,
        selected_q = NA_real_, selected_tilt = NA_real_,
        central_tilt = NA_real_, posterior_content_probability = NA_real_,
        feasible_count = action$feasible_count,
        candidates_evaluated = action$candidates_evaluated,
        posterior_confidence = action$posterior_confidence,
        ecm_converged = NA, ecm_iterations = NA_integer_,
        ecm_final_stationarity = NA_real_
      ))
    }
    list(
      returned = TRUE,
      lower = selected$lower[[1L]],
      upper = selected$upper[[1L]],
      width = selected$width[[1L]],
      failure_reason = "",
      selected_q = selected$target_content[[1L]],
      selected_tilt = selected$mean_tilt[[1L]],
      central_tilt = selected$central_tilt[[1L]],
      posterior_content_probability =
        selected$posterior_content_probability[[1L]],
      feasible_count = action$feasible_count,
      candidates_evaluated = action$candidates_evaluated,
      posterior_confidence = action$posterior_confidence,
      ecm_converged = isTRUE(selected$ecm_converged[[1L]]),
      ecm_iterations = as.integer(selected$ecm_iterations[[1L]]),
      ecm_final_stationarity =
        as.numeric(selected$ecm_final_stationarity[[1L]])
    )
  }, error = function(e) {
    list(
      returned = FALSE,
      lower = NA_real_, upper = NA_real_, width = NA_real_,
      failure_reason = conditionMessage(e),
      selected_q = NA_real_, selected_tilt = NA_real_,
      central_tilt = NA_real_, posterior_content_probability = NA_real_,
      feasible_count = NA_integer_, candidates_evaluated = NA_integer_,
      posterior_confidence = as.numeric(policy_row$posterior_confidence %||%
                                          NA_real_)[1L],
      ecm_converged = NA, ecm_iterations = NA_integer_,
      ecm_final_stationarity = NA_real_
    )
  })
  out
}

pta_fit_method <- function(method_id, y, content, confidence, scan, policy_row,
                           profile, seed) {
  if (identical(method_id, "tcsp_mc")) return(pta_fit_tcsp(y, scan))
  if (identical(method_id, "mti_ecm_adaptive_cell")) {
    return(pta_fit_mti_ecm(y, content, confidence, scan, policy_row,
                           profile, seed))
  }
  if (identical(method_id, "young_mathew")) {
    return(pta_fit_young_mathew(y, content, confidence))
  }
  if (identical(method_id, "wilks_minmax")) {
    lower <- min(y)
    upper <- max(y)
    return(list(returned = TRUE, lower = lower, upper = upper,
                width = upper - lower, failure_reason = ""))
  }
  pta_stop("Unsupported method id: ", method_id)
}

pta_group_sensitivity <- function(data, heldout, lower, upper, variables,
                                  min_group_n = 3L) {
  if (!is.finite(lower) || !is.finite(upper)) return(data.frame())
  rows <- lapply(variables, function(variable) {
    group <- data[[variable]][heldout]
    y <- data$.current_response[heldout]
    keep <- is.finite(y) & !is.na(group)
    group <- as.character(group[keep])
    y <- y[keep]
    if (!length(y)) return(data.frame())
    values <- tapply(y >= lower & y <= upper, group, function(z) {
      if (length(z) >= min_group_n) mean(z) else NA_real_
    })
    values <- as.numeric(values)
    values <- values[is.finite(values)]
    if (!length(values)) return(data.frame())
    data.frame(
      grouping = variable,
      groups_used = length(values),
      minimum_group_content = min(values),
      maximum_group_content = max(values),
      group_content_range = max(values) - min(values),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

pta_run_one_task <- function(task, data, methods, content, confidence, scan,
                             policy_row, profile, method_labels) {
  split <- task$split
  response <- task$response
  role <- task$role
  y <- pta_numeric(data[[response]])
  data$.current_response <- y
  train_y <- y[split$train]
  heldout_y <- y[split$heldout]
  if (any(!is.finite(train_y)) || any(!is.finite(heldout_y))) {
    pta_stop("Response ", response, " contains nonfinite values in split ",
             split$split_id, ".")
  }
  result_rows <- list()
  sensitivity_rows <- list()
  for (method_id in methods) {
    start_time <- proc.time()[["elapsed"]]
    fit <- pta_fit_method(
      method_id = method_id,
      y = train_y,
      content = content,
      confidence = confidence,
      scan = scan,
      policy_row = policy_row,
      profile = profile,
      seed = split$seed + match(method_id, methods) * 100000L
    )
    elapsed <- proc.time()[["elapsed"]] - start_time
    eval <- if (isTRUE(fit$returned)) {
      rqrgibbs::rqr_interval_empirical_content(
        heldout_y, fit$lower, fit$upper, na_rm = FALSE
      )
    } else {
      data.frame(
        n = length(heldout_y), lower = NA_real_, upper = NA_real_,
        width = NA_real_, empirical_content = NA_real_,
        lower_omitted = NA_real_, upper_omitted = NA_real_
      )
    }
    result_rows[[length(result_rows) + 1L]] <- data.frame(
      split_id = as.integer(split$split_id),
      split_seed = as.integer(split$seed),
      response_id = response,
      response_role = role,
      method_id = method_id,
      method = unname(method_labels[[method_id]] %||% method_id),
      train_n = length(train_y),
      heldout_n = length(heldout_y),
      content = content,
      tolerance_confidence = confidence,
      interval_returned = isTRUE(fit$returned),
      lower = fit$lower,
      upper = fit$upper,
      width = fit$width,
      heldout_empirical_content = eval$empirical_content[[1L]],
      heldout_lower_omitted = eval$lower_omitted[[1L]],
      heldout_upper_omitted = eval$upper_omitted[[1L]],
      heldout_attains_content =
        isTRUE(fit$returned) && eval$empirical_content[[1L]] >= content,
      tcsp_retained_count = fit$retained_count %||% NA_integer_,
      tcsp_retained_fraction = fit$retained_fraction %||% NA_real_,
      tcsp_content_buffer = fit$content_buffer %||% NA_real_,
      tcsp_lower_confidence_bound =
        fit$lower_confidence_bound %||% NA_real_,
      mti_selected_q = fit$selected_q %||% NA_real_,
      mti_selected_tilt = fit$selected_tilt %||% NA_real_,
      mti_central_tilt = fit$central_tilt %||% NA_real_,
      mti_posterior_content_probability =
        fit$posterior_content_probability %||% NA_real_,
      mti_posterior_confidence = fit$posterior_confidence %||% NA_real_,
      mti_feasible_count = fit$feasible_count %||% NA_integer_,
      mti_candidates_evaluated = fit$candidates_evaluated %||% NA_integer_,
      mti_ecm_converged = fit$ecm_converged %||% NA,
      mti_ecm_iterations = fit$ecm_iterations %||% NA_integer_,
      mti_ecm_final_stationarity = fit$ecm_final_stationarity %||% NA_real_,
      failure_reason = fit$failure_reason %||% "",
      elapsed_sec = elapsed,
      stringsAsFactors = FALSE
    )
    sens <- pta_group_sensitivity(
      data = data,
      heldout = split$heldout,
      lower = fit$lower,
      upper = fit$upper,
      variables = c("start", "api_batch", "batch_order_quartile")
    )
    if (nrow(sens)) {
      sens$split_id <- as.integer(split$split_id)
      sens$response_id <- response
      sens$response_role <- role
      sens$method_id <- method_id
      sens$method <- unname(method_labels[[method_id]] %||% method_id)
      sensitivity_rows[[length(sensitivity_rows) + 1L]] <- sens
    }
  }
  list(
    results = do.call(rbind, result_rows),
    sensitivity = if (length(sensitivity_rows)) {
      do.call(rbind, sensitivity_rows)
    } else {
      data.frame()
    }
  )
}

pta_bind_fill <- function(frames) {
  frames <- frames[vapply(frames, nrow, integer(1L)) > 0L]
  if (!length(frames)) return(data.frame())
  cols <- Reduce(union, lapply(frames, names))
  frames <- lapply(frames, function(x) {
    missing <- setdiff(cols, names(x))
    for (col in missing) x[[col]] <- NA
    x[, cols, drop = FALSE]
  })
  do.call(rbind, frames)
}

pta_run_application <- function(config, repo_root, mode, output_dir, workers) {
  pta_require_packages(c("rqrgibbs", "digest", "jsonlite"))
  if (identical(mode, "confirmatory")) {
    pta_require_packages("tolerance")
  }
  data <- pta_load_clean_data(config, repo_root)
  train_n <- as.integer(config$analysis$training_size)[1L]
  heldout_n <- as.integer(config$analysis$heldout_size)[1L]
  if (nrow(data) - train_n != heldout_n) {
    pta_stop("Configured held-out size does not match the selected stratum.")
  }
  replications <- if (identical(mode, "smoke")) {
    as.integer(config$analysis$smoke_splits)[1L]
  } else {
    as.integer(config$analysis$confirmatory_splits)[1L]
  }
  content <- as.numeric(config$analysis$content)[1L]
  confidence <- as.numeric(config$analysis$tolerance_confidence)[1L]
  scan <- pta_scan_calibration(config, repo_root)
  policy_row <- pta_mti_policy_row(config, repo_root)
  profile <- pta_mti_profile_config(config, repo_root)
  methods <- as.character(unlist(config$methods, use.names = FALSE))
  method_labels <- config$method_labels
  splits <- pta_make_splits(
    N = nrow(data),
    train_n = train_n,
    replications = replications,
    seed = as.integer(config$analysis$base_seed)[1L] +
      if (identical(mode, "smoke")) 11L else 101L
  )
  responses <- data.frame(
    response = c(config$analysis$primary_response,
                 config$analysis$supplement_response),
    role = c("primary", "supplement"),
    stringsAsFactors = FALSE
  )
  tasks <- list()
  for (split in splits) {
    for (ii in seq_len(nrow(responses))) {
      tasks[[length(tasks) + 1L]] <- list(
        split = split,
        response = responses$response[[ii]],
        role = responses$role[[ii]]
      )
    }
  }
  workers <- max(1L, min(as.integer(workers)[1L], length(tasks)))
  Sys.setenv(
    OMP_NUM_THREADS = "1",
    OPENBLAS_NUM_THREADS = "1",
    MKL_NUM_THREADS = "1",
    VECLIB_MAXIMUM_THREADS = "1",
    NUMEXPR_NUM_THREADS = "1"
  )
  run_task <- function(task) {
    pta_run_one_task(
      task = task,
      data = data,
      methods = methods,
      content = content,
      confidence = confidence,
      scan = scan,
      policy_row = policy_row,
      profile = profile,
      method_labels = method_labels
    )
  }
  blocks <- if (workers > 1L && .Platform$OS.type != "windows") {
    parallel::mclapply(tasks, run_task, mc.cores = workers)
  } else {
    lapply(tasks, run_task)
  }
  results <- pta_bind_fill(lapply(blocks, `[[`, "results"))
  sensitivity <- pta_bind_fill(lapply(blocks, `[[`, "sensitivity"))
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  pta_atomic_write_csv(results, file.path(output_dir, "pharma_application_results.csv"))
  pta_atomic_write_csv(
    pta_summarize_results(results),
    file.path(output_dir, "pharma_application_summary.csv")
  )
  pta_atomic_write_csv(
    pta_summarize_sensitivity(sensitivity),
    file.path(output_dir, "pharma_application_dependence_sensitivity.csv")
  )
  manifest <- list(
    schema_version = config$schema_version,
    study_id = config$study_id,
    mode = mode,
    output_dir = output_dir,
    created_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    replications = replications,
    response_count = nrow(responses),
    method_count = length(methods),
    result_rows = nrow(results),
    sensitivity_rows = nrow(sensitivity),
    workers = workers,
    repo_commit = tryCatch(
      system2("git", c("rev-parse", "HEAD"), stdout = TRUE,
              stderr = FALSE)[[1L]],
      error = function(e) NA_character_
    ),
    git_status_short = tryCatch(
      system2("git", c("status", "--short"), stdout = TRUE,
              stderr = FALSE),
      error = function(e) NA_character_
    ),
    config_digest = pta_config_digest(
      pta_path(repo_root, "application/config/pharma_tolerance_application_20260902.json")
    ),
    data_sha256 = pta_file_sha256(pta_path(repo_root, config$paths$raw_file)),
    tcsp_scan_source = scan$source_path,
    tcsp_scan_source_sha256 = scan$source_sha256,
    mti_policy_source = policy_row$calibration_table_path,
    mti_policy_source_sha256 = policy_row$calibration_table_digest,
    mti_validation_config = profile$validation_path,
    mti_validation_config_sha256 = profile$validation_digest
  )
  pta_atomic_write_json(manifest, file.path(output_dir, "manifest.json"))
  output_dir
}

pta_summarize_results <- function(results) {
  if (!nrow(results)) return(data.frame())
  split_key <- interaction(results$response_id, results$method_id, drop = TRUE)
  rows <- lapply(split(results, split_key), function(z) {
    z <- z[order(z$split_id), , drop = FALSE]
    returned <- z[z$interval_returned, , drop = FALSE]
    data.frame(
      response_id = z$response_id[[1L]],
      response_role = z$response_role[[1L]],
      method_id = z$method_id[[1L]],
      method = z$method[[1L]],
      splits = nrow(z),
      interval_return_rate = mean(z$interval_returned),
      heldout_content_median =
        pta_median(returned$heldout_empirical_content),
      heldout_content_q025 =
        pta_quantile(returned$heldout_empirical_content, 0.025),
      heldout_content_q975 =
        pta_quantile(returned$heldout_empirical_content, 0.975),
      heldout_attainment_rate =
        mean(z$heldout_attains_content & z$interval_returned),
      lower_omitted_median = pta_median(returned$heldout_lower_omitted),
      upper_omitted_median = pta_median(returned$heldout_upper_omitted),
      lower_median = pta_median(returned$lower),
      upper_median = pta_median(returned$upper),
      lower_q025 = pta_quantile(returned$lower, 0.025),
      lower_q975 = pta_quantile(returned$lower, 0.975),
      upper_q025 = pta_quantile(returned$upper, 0.025),
      upper_q975 = pta_quantile(returned$upper, 0.975),
      width_median = pta_median(returned$width),
      width_q025 = pta_quantile(returned$width, 0.025),
      width_q975 = pta_quantile(returned$width, 0.975),
      tcsp_retained_count =
        pta_median(returned$tcsp_retained_count),
      mti_selected_q_median = pta_median(returned$mti_selected_q),
      mti_selected_tilt_median = pta_median(returned$mti_selected_tilt),
      mti_posterior_content_probability_median =
        pta_median(returned$mti_posterior_content_probability),
      mti_feasible_count_median = pta_median(returned$mti_feasible_count),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  method_order <- c("tcsp_mc", "mti_ecm_adaptive_cell",
                    "young_mathew", "wilks_minmax")
  out[order(out$response_role, match(out$method_id, method_order)), ,
      drop = FALSE]
}

pta_summarize_sensitivity <- function(sensitivity) {
  if (!nrow(sensitivity)) return(data.frame())
  key <- interaction(sensitivity$response_id, sensitivity$method_id,
                     sensitivity$grouping, drop = TRUE)
  rows <- lapply(split(sensitivity, key), function(z) {
    data.frame(
      response_id = z$response_id[[1L]],
      response_role = z$response_role[[1L]],
      method_id = z$method_id[[1L]],
      method = z$method[[1L]],
      grouping = z$grouping[[1L]],
      splits = length(unique(z$split_id)),
      median_minimum_group_content = pta_median(z$minimum_group_content),
      median_maximum_group_content = pta_median(z$maximum_group_content),
      median_group_content_range = pta_median(z$group_content_range),
      q975_group_content_range = pta_quantile(z$group_content_range, 0.975),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}
