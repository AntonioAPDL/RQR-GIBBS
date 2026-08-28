#!/usr/bin/env Rscript

arguments <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_path_override <- Sys.getenv(
  "RQRGIBBS_TCSP_ARTICLE_COMPOSER_SCRIPT_PATH", unset = ""
)
script_path <- if (nzchar(script_path_override)) {
  script_path_override
} else {
  sub(file_arg, "", arguments[startsWith(arguments, file_arg)][1L])
}
source_only <- identical(
  Sys.getenv("RQRGIBBS_TCSP_ARTICLE_COMPOSER_SOURCE_ONLY"), "true"
)
if (!nzchar(script_path_override) &&
    (source_only || !length(script_path) || is.na(script_path) ||
     !identical(basename(script_path),
                "81_compose_adaptive_tcsp_article_inputs.R"))) {
  script_path <- "application/scripts/81_compose_adaptive_tcsp_article_inputs.R"
}
script_path <- normalizePath(script_path, winslash = "/", mustWork = TRUE)
repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."),
                           winslash = "/", mustWork = TRUE)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

compose_stop <- function(...) stop(paste0(...), call. = FALSE)

compose_arg_value <- function(args, prefix, default = NULL) {
  hit <- args[startsWith(args, prefix)]
  if (length(hit)) sub(prefix, "", hit[[1L]], fixed = TRUE) else default
}

compose_bool_arg <- function(value, default = FALSE) {
  if (is.null(value) || !nzchar(value)) return(default)
  value <- tolower(trimws(value))
  if (value %in% c("true", "t", "1", "yes", "y")) return(TRUE)
  if (value %in% c("false", "f", "0", "no", "n")) return(FALSE)
  compose_stop("Invalid logical argument value: ", value)
}

compose_hash_file <- function(path) {
  digest::digest(path, algo = "sha256", file = TRUE)
}

compose_git_commit <- function(root = repo_root) {
  out <- tryCatch(
    system2("git", c("-C", root, "rev-parse", "HEAD"),
            stdout = TRUE, stderr = FALSE),
    error = function(e) character()
  )
  if (length(out)) out[[1L]] else NA_character_
}

compose_git_status <- function(root = repo_root) {
  out <- tryCatch(
    system2("git", c("-C", root, "status", "--short", "--branch"),
            stdout = TRUE, stderr = FALSE),
    error = function(e) character()
  )
  paste(out, collapse = "\n")
}

compose_read_csv <- function(path) {
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

compose_required_file <- function(dir, file) {
  path <- file.path(dir, file)
  if (!file.exists(path)) compose_stop("Missing required file: ", path)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

compose_assert_complete <- function(run_dir) {
  health_path <- file.path(run_dir, "health.json")
  if (!file.exists(health_path)) return(invisible(FALSE))
  health <- jsonlite::read_json(health_path, simplifyVector = TRUE)
  rows_remaining <- as.integer(health$rows_remaining %||% 0L)
  waves_failed <- as.integer(health$waves_failed %||% 0L)
  final_present <- isTRUE(health$final_artifacts_present %||% TRUE)
  if (!final_present || rows_remaining != 0L || waves_failed != 0L) {
    compose_stop("Run health is not complete: ", run_dir)
  }
  invisible(TRUE)
}

compose_align_frame <- function(frame, columns) {
  missing <- setdiff(columns, names(frame))
  for (name in missing) frame[[name]] <- NA
  frame[, columns, drop = FALSE]
}

compose_key <- function(frame, columns) {
  interaction(frame[columns], drop = TRUE, lex.order = TRUE)
}

compose_replace_rows <- function(baseline, adaptive, key_columns) {
  missing_base <- setdiff(key_columns, names(baseline))
  missing_adaptive <- setdiff(key_columns, names(adaptive))
  if (length(missing_base)) {
    compose_stop("Baseline rows are missing key column(s): ",
                 paste(missing_base, collapse = ", "))
  }
  if (length(missing_adaptive)) {
    compose_stop("Adaptive rows are missing key column(s): ",
                 paste(missing_adaptive, collapse = ", "))
  }
  baseline_key <- as.character(compose_key(baseline, key_columns))
  adaptive_key <- as.character(compose_key(adaptive, key_columns))
  if (anyDuplicated(adaptive_key)) {
    compose_stop("Adaptive replacement rows contain duplicate keys.")
  }
  replace <- baseline_key %in% adaptive_key
  if (sum(replace) != length(adaptive_key)) {
    compose_stop(
      "Adaptive replacement row count does not match baseline keys: ",
      sum(replace), " matched for ", length(adaptive_key), " adaptive rows."
    )
  }
  adaptive_order <- match(baseline_key[replace], adaptive_key)
  out <- baseline
  out[replace, ] <- compose_align_frame(adaptive[adaptive_order, , drop = FALSE],
                                        names(baseline))
  list(rows = out, replaced = replace)
}

compose_input_hashes <- function(paths) {
  data.frame(
    artifact = names(paths),
    path = normalizePath(unname(paths), winslash = "/", mustWork = TRUE),
    sha256 = vapply(unname(paths), compose_hash_file, character(1L)),
    bytes = as.numeric(file.info(unname(paths))$size),
    stringsAsFactors = FALSE
  )
}

compose_adaptive_tcsp_article_inputs <- function(
    baseline_run_dir,
    adaptive_run_dir,
    output_dir,
    overwrite = FALSE) {
  for (package in c("jsonlite", "digest")) {
    if (!requireNamespace(package, quietly = TRUE)) {
      compose_stop("Required package is not installed: ", package)
    }
  }
  baseline_run_dir <- normalizePath(baseline_run_dir, winslash = "/",
                                    mustWork = TRUE)
  adaptive_run_dir <- normalizePath(adaptive_run_dir, winslash = "/",
                                    mustWork = TRUE)
  compose_assert_complete(baseline_run_dir)
  compose_assert_complete(adaptive_run_dir)

  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
  if ((file.exists(output_dir) || dir.exists(output_dir)) && !overwrite) {
    compose_stop("The output directory already exists: ", output_dir)
  }
  if (dir.exists(output_dir) && overwrite) {
    unlink(output_dir, recursive = TRUE, force = TRUE)
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  baseline_results_path <- compose_required_file(
    baseline_run_dir, "bayes_uq_validation_results.csv"
  )
  adaptive_results_path <- compose_required_file(
    adaptive_run_dir, "bayes_uq_validation_results.csv"
  )
  baseline_scan_path <- compose_required_file(
    baseline_run_dir, "scan_calibration_summary.csv"
  )
  adaptive_scan_path <- compose_required_file(
    adaptive_run_dir, "scan_calibration_summary.csv"
  )

  baseline_results <- compose_read_csv(baseline_results_path)
  adaptive_results <- compose_read_csv(adaptive_results_path)
  adaptive_tcsp <- adaptive_results[
    adaptive_results$method_id == "tcsp_mc" &
      adaptive_results$scan_critical_method == "monte_carlo_cp_adaptive",
    ,
    drop = FALSE
  ]
  if (!nrow(adaptive_tcsp)) {
    compose_stop("No adaptive TCSP rows found in adaptive run.")
  }

  result_keys <- c(
    "dgp_id", "n", "guaranteed_content", "tolerance_confidence",
    "posterior_confidence", "replication", "seed", "method_id"
  )
  replacement <- compose_replace_rows(
    baseline = baseline_results,
    adaptive = compose_align_frame(adaptive_tcsp, names(baseline_results)),
    key_columns = result_keys
  )
  article_results <- replacement$rows

  baseline_scan <- compose_read_csv(baseline_scan_path)
  adaptive_scan <- compose_read_csv(adaptive_scan_path)
  adaptive_scan <- adaptive_scan[
    adaptive_scan$method == "monte_carlo_cp_adaptive",
    ,
    drop = FALSE
  ]
  scan_replacement <- compose_replace_rows(
    baseline = baseline_scan,
    adaptive = compose_align_frame(adaptive_scan, names(baseline_scan)),
    key_columns = c("n", "guaranteed_content", "tolerance_confidence")
  )
  article_scan <- scan_replacement$rows

  results_out <- file.path(output_dir, "bayes_uq_validation_results.csv")
  scan_out <- file.path(output_dir, "scan_calibration_summary.csv")
  replacement_out <- file.path(output_dir, "adaptive_replacement_summary.csv")
  input_hash_out <- file.path(output_dir, "input_hashes.csv")
  manifest_out <- file.path(output_dir, "manifest.json")
  readme_out <- file.path(output_dir, "README.md")

  utils::write.csv(article_results, results_out, row.names = FALSE)
  utils::write.csv(article_scan, scan_out, row.names = FALSE)

  replaced_rows <- baseline_results[replacement$replaced, , drop = FALSE]
  replacement_summary <- aggregate(
    list(replaced_rows = replacement$replaced[replacement$replaced]),
    by = replaced_rows[c("n", "guaranteed_content", "tolerance_confidence")],
    FUN = length
  )
  replacement_summary$adaptive_retained_count <- vapply(
    seq_len(nrow(replacement_summary)),
    function(ii) {
      hit <- adaptive_scan[
        adaptive_scan$n == replacement_summary$n[[ii]] &
          adaptive_scan$guaranteed_content ==
            replacement_summary$guaranteed_content[[ii]] &
          adaptive_scan$tolerance_confidence ==
            replacement_summary$tolerance_confidence[[ii]],
        ,
        drop = FALSE
      ]
      if (nrow(hit)) as.integer(hit$retained_count[[1L]]) else NA_integer_
    },
    integer(1L)
  )
  utils::write.csv(replacement_summary, replacement_out, row.names = FALSE)

  hashes <- compose_input_hashes(c(
    baseline_results = baseline_results_path,
    adaptive_results = adaptive_results_path,
    baseline_scan = baseline_scan_path,
    adaptive_scan = adaptive_scan_path
  ))
  utils::write.csv(hashes, input_hash_out, row.names = FALSE)

  manifest <- list(
    schema_version =
      "rqrgibbs_bayes_uq_validation/1.3.0/adaptive_tcsp_article_composite",
    generated_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    git_commit = compose_git_commit(),
    git_status = compose_git_status(),
    baseline_run_dir = baseline_run_dir,
    adaptive_run_dir = adaptive_run_dir,
    output_dir = output_dir,
    article_results_rows = nrow(article_results),
    baseline_results_rows = nrow(baseline_results),
    adaptive_tcsp_rows_available = nrow(adaptive_tcsp),
    tcsp_rows_replaced = sum(replacement$replaced),
    scan_rows_replaced = sum(scan_replacement$replaced),
    replacement_summary = replacement_summary,
    input_hashes = hashes,
    output_hashes = compose_input_hashes(c(
      article_results = results_out,
      article_scan = scan_out,
      replacement_summary = replacement_out
    ))
  )
  jsonlite::write_json(manifest, manifest_out, pretty = TRUE,
                       auto_unbox = TRUE)
  writeLines(c(
    "# Adaptive TCSP Article Composite",
    "",
    "This ignored local record assembles manuscript tolerance-validation inputs.",
    "It starts from the completed 3-method confirmatory run and replaces only",
    "the TCSP rows for cells where adaptive Clopper-Pearson scan calibration",
    "sharpened the retained count.",
    "",
    paste0("- Baseline rows: `", nrow(baseline_results), "`"),
    paste0("- Adaptive TCSP rows available: `", nrow(adaptive_tcsp), "`"),
    paste0("- TCSP rows replaced: `", sum(replacement$replaced), "`"),
    paste0("- Scan rows replaced: `", sum(scan_replacement$replaced), "`")
  ), readme_out)

  cat("Composed adaptive TCSP article inputs:\n")
  cat("  output_dir: ", output_dir, "\n", sep = "")
  cat("  tcsp_rows_replaced: ", sum(replacement$replaced), "\n", sep = "")
  invisible(manifest)
}

if (!source_only) {
  args <- commandArgs(trailingOnly = TRUE)
  baseline_run_dir <- compose_arg_value(
    args, "--baseline-run-dir=",
    compose_arg_value(
      args, "--old-run-dir=",
      file.path(
        "application", "runs",
        "rqr_bayes_uq_validation_main_3method_1000_20260819",
        "wave_confirmatory_3method1000_20260819T090047Z"
      )
    )
  )
  adaptive_run_dir <- compose_arg_value(
    args, "--adaptive-run-dir=",
    file.path(
      "application", "runs",
      "rqr_bayes_uq_validation_tcsp_adaptive_targeted_20260820",
      "wave_confirmatory_tcsp_adaptive_targeted_20260820T071540Z"
    )
  )
  output_dir <- compose_arg_value(
    args, "--output-dir=",
    file.path(
      "application", "outputs",
      "rqr_bayes_uq_validation_article_adaptive_tcsp_20260820"
    )
  )
  overwrite <- compose_bool_arg(
    compose_arg_value(args, "--overwrite=", "false"),
    default = FALSE
  )
  compose_adaptive_tcsp_article_inputs(
    baseline_run_dir = baseline_run_dir,
    adaptive_run_dir = adaptive_run_dir,
    output_dir = output_dir,
    overwrite = overwrite
  )
}
