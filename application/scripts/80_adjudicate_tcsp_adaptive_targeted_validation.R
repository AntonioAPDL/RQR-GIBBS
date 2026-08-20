#!/usr/bin/env Rscript

arguments <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_path_override <- Sys.getenv(
  "RQRGIBBS_TCSP_ADAPTIVE_ADJUDICATION_SCRIPT_PATH", unset = ""
)
script_path <- if (nzchar(script_path_override)) {
  script_path_override
} else {
  sub(file_arg, "", arguments[startsWith(arguments, file_arg)][1L])
}
source_only <- identical(
  Sys.getenv("RQRGIBBS_TCSP_ADAPTIVE_ADJUDICATION_SOURCE_ONLY"), "true"
)
if (!nzchar(script_path_override) &&
    (source_only || !length(script_path) || is.na(script_path) ||
     !identical(basename(script_path),
                "80_adjudicate_tcsp_adaptive_targeted_validation.R"))) {
  script_path <- "application/scripts/80_adjudicate_tcsp_adaptive_targeted_validation.R"
}
script_path <- normalizePath(script_path, winslash = "/", mustWork = TRUE)
repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."),
                           winslash = "/", mustWork = TRUE)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

tcsp_stop <- function(...) stop(paste0(...), call. = FALSE)

tcsp_arg_value <- function(args, prefix, default = NULL) {
  hit <- args[startsWith(args, prefix)]
  if (length(hit)) sub(prefix, "", hit[[1L]], fixed = TRUE) else default
}

tcsp_bool <- function(x) {
  if (is.logical(x)) return(x)
  if (is.numeric(x)) return(ifelse(is.na(x), NA, x != 0))
  x <- tolower(trimws(as.character(x)))
  out <- rep(NA, length(x))
  out[x %in% c("true", "t", "1", "yes", "y")] <- TRUE
  out[x %in% c("false", "f", "0", "no", "n")] <- FALSE
  out
}

tcsp_num <- function(x) suppressWarnings(as.numeric(x))

tcsp_mean <- function(x) {
  x <- tcsp_num(x)
  x <- x[is.finite(x)]
  if (length(x)) mean(x) else NA_real_
}

tcsp_quantile <- function(x, probability) {
  x <- tcsp_num(x)
  x <- x[is.finite(x)]
  if (length(x)) {
    as.numeric(stats::quantile(x, probability, names = FALSE, type = 8))
  } else {
    NA_real_
  }
}

tcsp_bool_mean <- function(x) {
  x <- tcsp_bool(x)
  if (any(!is.na(x))) mean(x, na.rm = TRUE) else NA_real_
}

tcsp_file_hash <- function(path) {
  digest::digest(path, algo = "sha256", file = TRUE)
}

tcsp_git_commit <- function(root = repo_root) {
  out <- tryCatch(
    system2("git", c("-C", root, "rev-parse", "HEAD"),
            stdout = TRUE, stderr = FALSE),
    error = function(e) character()
  )
  if (length(out)) out[[1L]] else NA_character_
}

tcsp_git_status <- function(root = repo_root) {
  out <- tryCatch(
    system2("git", c("-C", root, "status", "--short", "--branch"),
            stdout = TRUE, stderr = FALSE),
    error = function(e) character()
  )
  paste(out, collapse = "\n")
}

tcsp_required_file <- function(dir, file) {
  path <- file.path(dir, file)
  if (!file.exists(path)) tcsp_stop("Missing required file: ", path)
  path
}

tcsp_read_csv <- function(path) {
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

tcsp_read_json <- function(path) {
  jsonlite::read_json(path, simplifyVector = TRUE)
}

tcsp_assert_complete <- function(run_dir, allow_incomplete = FALSE) {
  health_path <- file.path(run_dir, "health.json")
  if (!file.exists(health_path)) {
    if (allow_incomplete) return(list())
    tcsp_stop("Missing health.json in run directory: ", run_dir)
  }
  health <- tcsp_read_json(health_path)
  rows_remaining <- as.integer(health$rows_remaining %||% 0L)
  waves_failed <- as.integer(health$waves_failed %||% 0L)
  final_present <- isTRUE(health$final_artifacts_present %||% TRUE)
  if (!allow_incomplete &&
      (!final_present || rows_remaining != 0L || waves_failed != 0L)) {
    tcsp_stop("Run health is not complete: ", run_dir)
  }
  health
}

tcsp_read_run <- function(run_dir, run_label, allow_incomplete = FALSE) {
  run_dir <- normalizePath(run_dir, winslash = "/", mustWork = TRUE)
  health <- tcsp_assert_complete(run_dir, allow_incomplete = allow_incomplete)
  results_path <- tcsp_required_file(run_dir, "bayes_uq_validation_results.csv")
  summary_path <- file.path(run_dir, "bayes_uq_validation_summary.csv")
  manifest_path <- file.path(run_dir, "manifest.json")
  out <- list(
    label = run_label,
    run_dir = run_dir,
    health = health,
    results_path = results_path,
    results = tcsp_read_csv(results_path),
    input_hashes = data.frame(
      run_label = run_label,
      artifact = basename(c(results_path, summary_path, manifest_path)),
      path = normalizePath(c(results_path, summary_path, manifest_path),
                           winslash = "/", mustWork = FALSE),
      exists = file.exists(c(results_path, summary_path, manifest_path)),
      sha256 = vapply(c(results_path, summary_path, manifest_path),
                      function(path) {
                        if (file.exists(path)) tcsp_file_hash(path) else
                          NA_character_
                      },
                      character(1L)),
      stringsAsFactors = FALSE
    )
  )
  out
}

tcsp_filter_rows <- function(results, scan_method, method_id = "tcsp_mc") {
  rows <- results[results$method_id == method_id, , drop = FALSE]
  if ("scan_critical_method" %in% names(rows)) {
    rows <- rows[rows$scan_critical_method == scan_method, , drop = FALSE]
  }
  rows
}

tcsp_pair_rows <- function(old_rows, adaptive_rows) {
  keys <- c(
    "dgp_id", "n", "guaranteed_content", "tolerance_confidence",
    "posterior_confidence", "replication", "seed"
  )
  missing_old <- setdiff(keys, names(old_rows))
  missing_new <- setdiff(keys, names(adaptive_rows))
  if (length(missing_old)) {
    tcsp_stop("Old results are missing key columns: ",
              paste(missing_old, collapse = ", "))
  }
  if (length(missing_new)) {
    tcsp_stop("Adaptive results are missing key columns: ",
              paste(missing_new, collapse = ", "))
  }
  old_key <- interaction(old_rows[keys], drop = TRUE, lex.order = TRUE)
  new_key <- interaction(adaptive_rows[keys], drop = TRUE, lex.order = TRUE)
  if (anyDuplicated(old_key)) tcsp_stop("Old TCSP rows contain duplicate keys.")
  if (anyDuplicated(new_key)) {
    tcsp_stop("Adaptive TCSP rows contain duplicate keys.")
  }
  paired <- merge(
    old_rows, adaptive_rows, by = keys, all = TRUE,
    suffixes = c("_old", "_adaptive"), sort = FALSE
  )
  paired$pair_status <- ifelse(
    is.na(paired$method_id_old), "missing_old",
    ifelse(is.na(paired$method_id_adaptive), "missing_adaptive", "paired")
  )
  paired
}

tcsp_group_key <- function(df, columns) {
  interaction(df[columns], drop = TRUE, lex.order = TRUE)
}

tcsp_summarise_group <- function(df) {
  old_success <- df$success_old
  adaptive_success <- df$success_adaptive
  old_infeasible <- df$infeasible_old
  adaptive_infeasible <- df$infeasible_adaptive
  old_width <- df$width_old
  adaptive_width <- df$width_adaptive
  old_returned <- !tcsp_bool(old_infeasible) & is.finite(tcsp_num(old_width))
  adaptive_returned <- !tcsp_bool(adaptive_infeasible) &
    is.finite(tcsp_num(adaptive_width))
  old_mean_width <- tcsp_mean(old_width)
  adaptive_mean_width <- tcsp_mean(adaptive_width)
  data.frame(
    paired_rows = nrow(df),
    old_return_rate = mean(old_returned, na.rm = TRUE),
    adaptive_return_rate = mean(adaptive_returned, na.rm = TRUE),
    old_delivery_rate = tcsp_bool_mean(old_success),
    adaptive_delivery_rate = tcsp_bool_mean(adaptive_success),
    delivery_rate_delta =
      tcsp_bool_mean(adaptive_success) - tcsp_bool_mean(old_success),
    old_retained_count = suppressWarnings(as.integer(stats::median(
      unique(tcsp_num(df$retained_count_old)), na.rm = TRUE
    ))),
    adaptive_retained_count = suppressWarnings(as.integer(stats::median(
      unique(tcsp_num(df$retained_count_adaptive)), na.rm = TRUE
    ))),
    old_mean_width = old_mean_width,
    adaptive_mean_width = adaptive_mean_width,
    mean_width_delta = adaptive_mean_width - old_mean_width,
    mean_width_ratio = adaptive_mean_width / old_mean_width,
    old_width_q025 = tcsp_quantile(old_width, 0.025),
    old_width_q975 = tcsp_quantile(old_width, 0.975),
    adaptive_width_q025 = tcsp_quantile(adaptive_width, 0.025),
    adaptive_width_q975 = tcsp_quantile(adaptive_width, 0.975),
    stringsAsFactors = FALSE
  )
}

tcsp_summarise_by <- function(paired, columns) {
  ok <- paired[paired$pair_status == "paired", , drop = FALSE]
  if (!nrow(ok)) return(data.frame())
  pieces <- lapply(split(ok, tcsp_group_key(ok, columns)), function(df) {
    id <- df[1L, columns, drop = FALSE]
    cbind(id, tcsp_summarise_group(df))
  })
  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  out[do.call(order, out[columns]), , drop = FALSE]
}

tcsp_adaptive_gate <- function(dgp_summary, paired, expected_rows,
                               min_delivery, max_delivery_drop,
                               min_width_nonincrease_fraction) {
  adaptive_width_delta <- dgp_summary$mean_width_delta
  gates <- data.frame(
    gate = c(
      "expected_rows_complete",
      "all_rows_paired",
      "adaptive_all_returned",
      "adaptive_min_delivery",
      "delivery_drop_controlled",
      "width_nonincrease_fraction"
    ),
    pass = c(
      nrow(paired[paired$pair_status == "paired", , drop = FALSE]) ==
        expected_rows,
      all(paired$pair_status == "paired"),
      all(dgp_summary$adaptive_return_rate == 1),
      min(dgp_summary$adaptive_delivery_rate, na.rm = TRUE) >= min_delivery,
      min(dgp_summary$delivery_rate_delta, na.rm = TRUE) >= -max_delivery_drop,
      mean(adaptive_width_delta <= 0, na.rm = TRUE) >=
        min_width_nonincrease_fraction
    ),
    value = c(
      nrow(paired[paired$pair_status == "paired", , drop = FALSE]),
      paste(table(paired$pair_status), collapse = ";"),
      min(dgp_summary$adaptive_return_rate, na.rm = TRUE),
      min(dgp_summary$adaptive_delivery_rate, na.rm = TRUE),
      min(dgp_summary$delivery_rate_delta, na.rm = TRUE),
      mean(adaptive_width_delta <= 0, na.rm = TRUE)
    ),
    threshold = c(
      expected_rows,
      "all paired",
      1,
      min_delivery,
      paste0(">=", -max_delivery_drop),
      min_width_nonincrease_fraction
    ),
    stringsAsFactors = FALSE
  )
  list(
    gate_table = gates,
    gate_status = if (all(gates$pass)) {
      "promote_adaptive_tcsp"
    } else {
      "hold_adaptive_tcsp"
    }
  )
}

tcsp_adaptive_adjudicate <- function(
    old_run_dir,
    new_run_dir,
    output_dir,
    expected_rows = 28000L,
    min_delivery = 0.94,
    max_delivery_drop = 0.01,
    min_width_nonincrease_fraction = 0.75,
    allow_incomplete = FALSE) {
  for (package in c("jsonlite", "digest")) {
    if (!requireNamespace(package, quietly = TRUE)) {
      tcsp_stop("Required package is not installed: ", package)
    }
  }
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
  if (file.exists(output_dir) || dir.exists(output_dir)) {
    tcsp_stop("The output directory must be fresh: ", output_dir)
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  old <- tcsp_read_run(old_run_dir, "old_tcsp_conservative",
                       allow_incomplete = allow_incomplete)
  adaptive <- tcsp_read_run(new_run_dir, "adaptive_tcsp_cp",
                            allow_incomplete = allow_incomplete)
  old_rows <- tcsp_filter_rows(
    old$results, "monte_carlo_conservative", method_id = "tcsp_mc"
  )
  adaptive_rows <- tcsp_filter_rows(
    adaptive$results, "monte_carlo_cp_adaptive", method_id = "tcsp_mc"
  )
  if (!nrow(old_rows)) tcsp_stop("No old conservative TCSP rows found.")
  if (!nrow(adaptive_rows)) tcsp_stop("No adaptive TCSP rows found.")

  adaptive_cells <- unique(adaptive_rows[
    c("dgp_id", "n", "guaranteed_content", "tolerance_confidence",
      "posterior_confidence")
  ])
  old_rows <- merge(
    old_rows, adaptive_cells,
    by = c("dgp_id", "n", "guaranteed_content", "tolerance_confidence",
           "posterior_confidence"),
    all = FALSE, sort = FALSE
  )

  paired <- tcsp_pair_rows(old_rows, adaptive_rows)
  dgp_summary <- tcsp_summarise_by(
    paired,
    c("dgp_id", "n", "guaranteed_content", "tolerance_confidence",
      "posterior_confidence")
  )
  cell_summary <- tcsp_summarise_by(
    paired,
    c("n", "guaranteed_content", "tolerance_confidence",
      "posterior_confidence")
  )
  gate <- tcsp_adaptive_gate(
    dgp_summary = dgp_summary,
    paired = paired,
    expected_rows = as.integer(expected_rows)[1L],
    min_delivery = as.numeric(min_delivery)[1L],
    max_delivery_drop = as.numeric(max_delivery_drop)[1L],
    min_width_nonincrease_fraction =
      as.numeric(min_width_nonincrease_fraction)[1L]
  )

  input_hashes <- rbind(old$input_hashes, adaptive$input_hashes)
  utils::write.csv(paired, file.path(output_dir, "paired_tcsp_rows.csv"),
                   row.names = FALSE)
  utils::write.csv(dgp_summary,
                   file.path(output_dir, "adaptive_tcsp_dgp_summary.csv"),
                   row.names = FALSE)
  utils::write.csv(cell_summary,
                   file.path(output_dir, "adaptive_tcsp_cell_summary.csv"),
                   row.names = FALSE)
  utils::write.csv(gate$gate_table,
                   file.path(output_dir, "adaptive_tcsp_gate_table.csv"),
                   row.names = FALSE)
  utils::write.csv(input_hashes, file.path(output_dir, "input_hashes.csv"),
                   row.names = FALSE)

  manifest <- list(
    schema_version =
      "rqrgibbs_bayes_uq_validation/1.3.0/tcsp_adaptive_adjudication",
    generated_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    git_commit = tcsp_git_commit(),
    git_status = tcsp_git_status(),
    old_run_dir = old$run_dir,
    adaptive_run_dir = adaptive$run_dir,
    output_dir = output_dir,
    expected_rows = as.integer(expected_rows)[1L],
    old_tcsp_rows_used = nrow(old_rows),
    adaptive_tcsp_rows_used = nrow(adaptive_rows),
    paired_rows = nrow(paired[paired$pair_status == "paired", , drop = FALSE]),
    gate_status = gate$gate_status,
    gates = gate$gate_table,
    input_hashes = input_hashes
  )
  jsonlite::write_json(manifest, file.path(output_dir, "manifest.json"),
                       pretty = TRUE, auto_unbox = TRUE)
  writeLines(c(
    "# Targeted Adaptive-TCSP Adjudication",
    "",
    paste0("- Gate status: `", gate$gate_status, "`"),
    paste0("- Expected paired rows: `", expected_rows, "`"),
    paste0("- Paired rows observed: `", manifest$paired_rows, "`"),
    "- Width summaries are raw interval widths, not TCSP-relative ratios.",
    "- Delivery rates are repeated-sample validation rates for the requested population-content statement."
  ), file.path(output_dir, "README.md"))
  cat("Targeted adaptive TCSP adjudication:", gate$gate_status, "\n")
  cat("OUTPUT_DIR=", output_dir, "\n", sep = "")
  invisible(manifest)
}

if (!source_only) {
  args <- commandArgs(trailingOnly = TRUE)
  old_run_dir <- tcsp_arg_value(
    args, "--old-run-dir=",
    file.path(
      "application", "runs",
      "rqr_bayes_uq_validation_main_3method_1000_20260819",
      "wave_confirmatory_3method1000_20260819T090047Z"
    )
  )
  new_run_dir <- tcsp_arg_value(args, "--new-run-dir=", NULL)
  if (is.null(new_run_dir) || !nzchar(new_run_dir)) {
    tcsp_stop("Missing required --new-run-dir argument.")
  }
  output_dir <- tcsp_arg_value(
    args, "--output-dir=",
    file.path(
      "application", "outputs", "tcsp_adaptive_targeted_adjudication",
      paste0("tcsp_adaptive_targeted_",
             format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"))
    )
  )
  tcsp_adaptive_adjudicate(
    old_run_dir = old_run_dir,
    new_run_dir = new_run_dir,
    output_dir = output_dir,
    expected_rows = as.integer(
      tcsp_arg_value(args, "--expected-rows=", "28000")
    )[1L],
    min_delivery = as.numeric(
      tcsp_arg_value(args, "--min-delivery=", "0.94")
    )[1L],
    max_delivery_drop = as.numeric(
      tcsp_arg_value(args, "--max-delivery-drop=", "0.01")
    )[1L],
    min_width_nonincrease_fraction = as.numeric(
      tcsp_arg_value(args, "--min-width-nonincrease-fraction=", "0.75")
    )[1L],
    allow_incomplete = identical(tolower(
      tcsp_arg_value(args, "--allow-incomplete=", "false")
    ), "true")
  )
}
