#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(prefix, default = NULL) {
  hit <- args[startsWith(args, prefix)]
  if (length(hit)) sub(prefix, "", hit[[1L]], fixed = TRUE) else default
}
stopf <- function(...) stop(paste0(...), call. = FALSE)

arguments <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_path <- sub(file_arg, "", arguments[startsWith(arguments, file_arg)][1L])
if (!length(script_path) || is.na(script_path)) {
  script_path <- "tables/generate_tolerance_validation_stratified_table.R"
}
script_path <- normalizePath(script_path, winslash = "/", mustWork = TRUE)
repo_root <- normalizePath(file.path(dirname(script_path), ".."),
                           winslash = "/", mustWork = TRUE)
setwd(repo_root)

default_summary_path <- Sys.getenv(
  "RQR_BAYES_UQ_PRIMARY_BY_N_CONTENT",
  unset = file.path(
    "application", "runs", "rqr_bayes_uq_validation_main_20260813",
    "wave_main_20260813T103232Z",
    "final_combined_method_by_n_content_with_young_mathew.csv"
  )
)
summary_path_arg <- arg_value("--summary-csv=", default_summary_path)
default_results_path <- file.path(
  "application", "runs", "rqr_bayes_uq_validation_main_20260813",
  "wave_main_20260813T103232Z",
  "bayes_uq_validation_results.csv"
)
default_young_mathew_results <- file.path(
  "application", "runs", "rqr_bayes_uq_validation_main_20260813",
  "wave_main_20260813T103232Z", "young_mathew_addon_20260815T064224Z",
  "bayes_uq_validation_results.csv"
)
results_path_arg <- arg_value(
  "--results-csv=",
  Sys.getenv("RQR_BAYES_UQ_PRIMARY_RESULTS", unset = default_results_path)
)
young_mathew_path_arg <- arg_value(
  "--young-mathew-results=",
  Sys.getenv("RQR_BAYES_UQ_PRIMARY_YM_RESULTS",
             unset = default_young_mathew_results)
)
output_dir <- normalizePath(arg_value("--output-dir=", "tables"),
                            winslash = "/", mustWork = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

csv_path <- file.path(output_dir, "tolerance_validation_by_n_content.csv")
tex_path <- file.path(output_dir, "tolerance_validation_by_n_content.tex")
committed_csv <- file.path(repo_root, "tables",
                           "tolerance_validation_by_n_content.csv")
committed_tex <- file.path(repo_root, "tables",
                           "tolerance_validation_by_n_content.tex")
per_n_tex_names <- c(
  "tolerance_validation_by_n_500_content.tex",
  "tolerance_validation_by_n_1000_content.tex"
)
committed_per_n_tex <- file.path(repo_root, "tables", per_n_tex_names)
target_per_n_tex <- file.path(output_dir, per_n_tex_names)

if (!file.exists(summary_path_arg)) {
  if (file.exists(committed_csv) && file.exists(committed_tex) &&
      all(file.exists(committed_per_n_tex))) {
    if (!identical(normalizePath(dirname(csv_path), winslash = "/",
                                 mustWork = FALSE),
                   normalizePath(dirname(committed_csv), winslash = "/",
                                 mustWork = TRUE))) {
      file.copy(committed_csv, csv_path, overwrite = TRUE)
      file.copy(committed_tex, tex_path, overwrite = TRUE)
      file.copy(committed_per_n_tex, target_per_n_tex, overwrite = TRUE)
    }
    cat("Using committed stratified tolerance validation table;",
        "set --summary-csv to regenerate from run output.\n")
    quit(status = 0)
  }
  stopf("Missing stratified summary CSV: ", summary_path_arg)
}

summary_path <- normalizePath(summary_path_arg, winslash = "/", mustWork = TRUE)
summary <- utils::read.csv(summary_path, stringsAsFactors = FALSE,
                           check.names = FALSE)
required <- c(
  "method_id", "n", "c", "infeasible_rate", "success_rate"
)
missing <- setdiff(required, names(summary))
if (length(missing)) {
  stopf("Stratified summary CSV is missing required columns: ",
        paste(missing, collapse = ", "))
}

method_order <- c(
  "tcsp_mc",
  "hdp_s_mc",
  "tcsp_mti_ecm_map_mc",
  "young_mathew",
  "wilks_minmax",
  "tcsp_dkw"
)
method_labels <- c(
  tcsp_mc = "TCSP scan",
  hdp_s_mc = "Hybrid DP--scan",
  tcsp_mti_ecm_map_mc = "MTI ECM",
  young_mathew = "Young--Mathew",
  wilks_minmax = "Wilks min--max",
  tcsp_dkw = "DKW scan"
)

selected <- summary[summary$method_id %in% method_order, , drop = FALSE]
missing_methods <- setdiff(method_order, unique(selected$method_id))
if (length(missing_methods)) {
  stopf("Stratified summary CSV is missing method(s): ",
        paste(missing_methods, collapse = ", "))
}

truthy <- function(x) !is.na(x) & x
format_pct <- function(x) {
  x <- as.numeric(x)
  ifelse(is.finite(x), sprintf("%.1f", 100 * x), "--")
}
format_num <- function(x, digits = 3) {
  x <- as.numeric(x)
  out <- ifelse(is.finite(x), sprintf(paste0("%.", digits, "f"), x), "--")
  sub("\\.?0+$", "", out)
}
format_width_range <- function(lower, upper) {
  lower <- as.numeric(lower)
  upper <- as.numeric(upper)
  out <- rep("--", length(lower))
  ok <- is.finite(lower) & is.finite(upper)
  same <- ok & abs(lower - upper) < 5e-4
  out[same] <- format_num(lower[same], 2)
  out[ok & !same] <- paste0(format_num(lower[ok & !same], 2), "--",
                            format_num(upper[ok & !same], 2))
  out
}
escape_latex <- function(x) {
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([_%&#])", "\\\\\\1", x, perl = TRUE)
  x
}

selected$n <- as.integer(selected$n)
selected$c <- as.numeric(selected$c)
selected$content <- selected$c
selected$infeasible_rate <- as.numeric(selected$infeasible_rate)
selected$success_rate <- as.numeric(selected$success_rate)

width_ranges_from_results <- function(primary_path, young_mathew_path) {
  paths <- c(primary_path, young_mathew_path)
  paths <- paths[file.exists(paths)]
  if (!length(paths)) return(data.frame())
  frames <- lapply(paths, function(path) {
    out <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
    required <- c("method_id", "n", "guaranteed_content", "width")
    missing <- setdiff(required, names(out))
    if (length(missing)) {
      stopf("Raw result CSV is missing required column(s): ",
            paste(missing, collapse = ", "))
    }
    out[, required, drop = FALSE]
  })
  raw <- do.call(rbind, frames)
  raw <- raw[raw$method_id %in% method_order, , drop = FALSE]
  raw$n <- as.integer(raw$n)
  raw$content <- as.numeric(raw$guaranteed_content)
  raw$width <- as.numeric(raw$width)
  key <- paste(raw$n, sprintf("%.4f", raw$content), raw$method_id, sep = "||")
  rows <- lapply(split(raw, key), function(df) {
    width <- df$width[is.finite(df$width)]
    data.frame(
      n = df$n[[1L]],
      content = df$content[[1L]],
      method_id = df$method_id[[1L]],
      width_q025 = if (length(width)) {
        unname(stats::quantile(width, 0.025, names = FALSE, type = 8))
      } else {
        NA_real_
      },
      width_q975 = if (length(width)) {
        unname(stats::quantile(width, 0.975, names = FALSE, type = 8))
      } else {
        NA_real_
      },
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

if (!all(c("width_q025", "width_q975") %in% names(selected))) {
  width_summary <- width_ranges_from_results(results_path_arg,
                                             young_mathew_path_arg)
  if (nrow(width_summary)) {
    selected <- merge(selected, width_summary,
                      by = c("n", "content", "method_id"),
                      all.x = TRUE, sort = FALSE)
  } else if ("median_width" %in% names(selected)) {
    selected$width_q025 <- as.numeric(selected$median_width)
    selected$width_q975 <- as.numeric(selected$median_width)
  } else {
    selected$width_q025 <- NA_real_
    selected$width_q975 <- NA_real_
  }
}

feasible_fraction <- pmax(0, 1 - selected$infeasible_rate)
delivery <- feasible_fraction * selected$success_rate
delivery[!is.finite(delivery) & feasible_fraction == 0] <- 0
returned_success <- ifelse(feasible_fraction > 0, selected$success_rate, NA_real_)

out <- data.frame(
  n = selected$n,
  content = selected$c,
  method_id = selected$method_id,
  method = unname(method_labels[selected$method_id]),
  infeasible_rate = selected$infeasible_rate,
  delivery_success = delivery,
  returned_success = returned_success,
  width_q025 = selected$width_q025,
  width_q975 = selected$width_q975,
  stringsAsFactors = FALSE
)
out <- out[order(
  out$n, out$content, match(out$method_id, method_order)
), ]

utils::write.csv(out, csv_path, row.names = FALSE)

make_tex_lines <- function(data, include_sample_size) {
  lines <- c(
    "\\begin{tabularx}{\\textwidth}{@{}>{\\raggedright\\arraybackslash}Xrrrr@{}}",
    "\\toprule",
    "Method & Infeasible (\\%) & Delivery (\\%) & Returned success (\\%) & Width 95\\% range\\\\",
    "\\midrule"
  )
  for (nn in sort(unique(data$n))) {
    for (cc in sort(unique(data$content[data$n == nn]))) {
      block <- data[
        data$n == nn & abs(data$content - cc) < 1e-12,
        ,
        drop = FALSE
      ]
      group_label <- if (isTRUE(include_sample_size)) {
        sprintf("Sample size $n=%s$, target content $c=%.2f$", nn, cc)
      } else {
        sprintf("Target content $c=%.2f$", cc)
      }
      lines <- c(
        lines,
        "\\addlinespace[0.35em]",
        sprintf("\\multicolumn{5}{@{}l}{\\textit{%s}}\\\\", group_label)
      )
      body <- vapply(seq_len(nrow(block)), function(ii) {
        sprintf(
          "%s & %s & %s & %s & %s \\\\",
          escape_latex(block$method[[ii]]),
          format_pct(block$infeasible_rate[[ii]]),
          format_pct(block$delivery_success[[ii]]),
          format_pct(block$returned_success[[ii]]),
          format_width_range(block$width_q025[[ii]], block$width_q975[[ii]])
        )
      }, character(1L))
      lines <- c(lines, body)
    }
  }
  c(lines, "\\bottomrule", "\\end{tabularx}")
}

writeLines(make_tex_lines(out, include_sample_size = TRUE), tex_path)
for (nn in sort(unique(out$n))) {
  per_n_path <- file.path(
    output_dir,
    sprintf("tolerance_validation_by_n_%s_content.tex", nn)
  )
  block <- out[out$n == nn, , drop = FALSE]
  writeLines(make_tex_lines(block, include_sample_size = FALSE), per_n_path)
}

cat("Wrote stratified tolerance validation table:\n")
cat("  csv: ", csv_path, "\n", sep = "")
cat("  tex: ", tex_path, "\n", sep = "")
