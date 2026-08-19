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
  script_path <- "tables/generate_tolerance_validation_summary_table.R"
}
script_path <- normalizePath(script_path, winslash = "/", mustWork = TRUE)
repo_root <- normalizePath(file.path(dirname(script_path), ".."),
                           winslash = "/", mustWork = TRUE)
setwd(repo_root)

default_run_dir <- file.path(
  "application", "runs", "rqr_bayes_uq_validation_main_20260813",
  "wave_main_20260813T103232Z"
)
results_path <- normalizePath(arg_value(
  "--results-csv=",
  file.path(default_run_dir, "bayes_uq_validation_results.csv")
), winslash = "/", mustWork = TRUE)
ym_default <- file.path(
  default_run_dir, "young_mathew_addon_20260815T064224Z",
  "bayes_uq_validation_results.csv"
)
ym_arg <- arg_value("--young-mathew-results-csv=", ym_default)
ym_results_path <- if (nzchar(ym_arg) && file.exists(ym_arg)) {
  normalizePath(ym_arg, winslash = "/", mustWork = TRUE)
} else {
  ""
}
output_dir <- normalizePath(arg_value("--output-dir=", "tables"),
                            winslash = "/", mustWork = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

read_results <- function(path) {
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}
bind_fill <- function(a, b) {
  cols <- union(names(a), names(b))
  add_missing <- function(x) {
    missing <- setdiff(cols, names(x))
    for (name in missing) x[[name]] <- NA
    x[cols]
  }
  rbind(add_missing(a), add_missing(b))
}
results <- read_results(results_path)
if (nzchar(ym_results_path)) {
  ym_results <- read_results(ym_results_path)
  if (!nrow(ym_results) || !all(ym_results$method_id == "young_mathew")) {
    stopf("Young-Mathew results must contain only young_mathew rows.")
  }
  if (!any(results$method_id == "young_mathew")) {
    results <- bind_fill(results, ym_results)
  }
}

required <- c("method_id", "infeasible", "success", "width", "elapsed_sec")
missing <- setdiff(required, names(results))
if (length(missing)) {
  stopf("Results CSV is missing required columns: ", paste(missing, collapse = ", "))
}

as_bool <- function(x) {
  if (is.logical(x)) return(x)
  if (is.numeric(x)) return(!is.na(x) & x != 0)
  y <- tolower(trimws(as.character(x)))
  ifelse(y %in% c("true", "t", "1", "yes"), TRUE,
         ifelse(y %in% c("false", "f", "0", "no"), FALSE, NA))
}
finite_quantile <- function(x, probability) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x)) {
    as.numeric(stats::quantile(x, probability, names = FALSE, type = 8))
  } else {
    NA_real_
  }
}
median_or_na <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x)) stats::median(x) else NA_real_
}

method_order <- c("tcsp_mc", "hdp_s_mc", "young_mathew", "wilks_minmax")
labels <- c(
  tcsp_mc = "TCSP scan",
  hdp_s_mc = "Hybrid DP--scan",
  young_mathew = "Young--Mathew",
  wilks_minmax = "Wilks min--max"
)
roles <- c(
  tcsp_mc = "Shortest empirical scan action",
  hdp_s_mc = "Scan action with DP content screen",
  young_mathew = "External tolerance comparator",
  wilks_minmax = "Full-range order-statistic baseline"
)
missing_methods <- setdiff(method_order, unique(results$method_id))
if (length(missing_methods)) {
  stopf("Results are missing main method(s): ",
        paste(missing_methods, collapse = ", "))
}

summarize_method <- function(method_id) {
  df <- results[results$method_id == method_id, , drop = FALSE]
  infeasible <- as_bool(df$infeasible)
  success <- as_bool(df$success)
  returned <- !is.na(infeasible) & !infeasible & !is.na(success)
  width <- as.numeric(df$width)
  data.frame(
    method_id = method_id,
    method = unname(labels[[method_id]]),
    role = unname(roles[[method_id]]),
    planned_rows = nrow(df),
    returned_interval_rate = mean(returned, na.rm = TRUE),
    content_success_returned = if (any(returned)) {
      mean(success[returned], na.rm = TRUE)
    } else {
      NA_real_
    },
    tolerance_delivery = mean(returned & success, na.rm = TRUE),
    width_q025 = finite_quantile(width[returned], 0.025),
    width_median = finite_quantile(width[returned], 0.5),
    width_q975 = finite_quantile(width[returned], 0.975),
    median_elapsed_sec = median_or_na(df$elapsed_sec[returned]),
    stringsAsFactors = FALSE
  )
}
out <- do.call(rbind, lapply(method_order, summarize_method))

format_pct <- function(x) sprintf("%.1f", 100 * as.numeric(x))
format_num <- function(x, digits = 3) {
  sprintf(paste0("%.", digits, "f"), as.numeric(x))
}
format_sec <- function(x) {
  x <- as.numeric(x)
  ifelse(x < 0.01, sprintf("%.3f", x), format_num(x, 3))
}
format_width <- function(median, lower, upper) {
  sprintf(
    "%s [%s, %s]",
    format_num(median, 3),
    format_num(lower, 3),
    format_num(upper, 3)
  )
}
escape_latex <- function(x) {
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([_%&#])", "\\\\\\1", x, perl = TRUE)
  x
}

out$width_median_q025_q975 <- mapply(
  format_width, out$width_median, out$width_q025, out$width_q975,
  USE.NAMES = FALSE
)

csv_path <- file.path(output_dir, "tolerance_validation_main_summary.csv")
tex_path <- file.path(output_dir, "tolerance_validation_main_summary.tex")
utils::write.csv(out, csv_path, row.names = FALSE)

body <- vapply(seq_len(nrow(out)), function(ii) {
  sprintf(
    "%s: %s & %s & %s & %s & %s & %s \\\\",
    escape_latex(out$method[[ii]]),
    escape_latex(out$role[[ii]]),
    format_pct(out$returned_interval_rate[[ii]]),
    format_pct(out$content_success_returned[[ii]]),
    format_pct(out$tolerance_delivery[[ii]]),
    escape_latex(out$width_median_q025_q975[[ii]]),
    format_sec(out$median_elapsed_sec[[ii]])
  )
}, character(1L))

lines <- c(
  "\\begin{tabularx}{\\textwidth}{@{}>{\\raggedright\\arraybackslash}Xrrrrr@{}}",
  "\\toprule",
  "Method and role & Returned (\\%) & Content success (\\%) & Delivery (\\%) & Width & Median sec\\\\",
  "\\midrule",
  body,
  "\\bottomrule",
  "\\end{tabularx}"
)
writeLines(lines, tex_path)

cat("Wrote tolerance validation table:\n")
cat("  csv: ", csv_path, "\n", sep = "")
cat("  tex: ", tex_path, "\n", sep = "")
