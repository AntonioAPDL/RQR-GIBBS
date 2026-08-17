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
default_summary_path <- Sys.getenv(
  "RQR_BAYES_UQ_PRIMARY_SUMMARY",
  unset = file.path(default_run_dir,
                    "final_combined_grid_complete_method_summary_with_young_mathew.csv")
)
summary_path_arg <- arg_value("--summary-csv=", default_summary_path)
output_dir <- normalizePath(arg_value("--output-dir=", "tables"),
                            winslash = "/", mustWork = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

csv_path <- file.path(output_dir, "tolerance_validation_main_summary.csv")
tex_path <- file.path(output_dir, "tolerance_validation_main_summary.tex")
committed_csv <- file.path(repo_root, "tables", "tolerance_validation_main_summary.csv")
committed_tex <- file.path(repo_root, "tables", "tolerance_validation_main_summary.tex")

if (!file.exists(summary_path_arg)) {
  if (file.exists(committed_csv) && file.exists(committed_tex)) {
    if (!identical(normalizePath(dirname(csv_path), winslash = "/", mustWork = FALSE),
                   normalizePath(dirname(committed_csv), winslash = "/", mustWork = TRUE))) {
      file.copy(committed_csv, csv_path, overwrite = TRUE)
      file.copy(committed_tex, tex_path, overwrite = TRUE)
    }
    cat("Using committed tolerance validation table; set --summary-csv to regenerate from run output.\n")
    cat("  csv: ", csv_path, "\n", sep = "")
    cat("  tex: ", tex_path, "\n", sep = "")
    quit(status = 0)
  }
  stopf("Missing summary CSV: ", summary_path_arg)
}

summary_path <- normalizePath(summary_path_arg, winslash = "/", mustWork = TRUE)

summary <- utils::read.csv(summary_path, stringsAsFactors = FALSE,
                           check.names = FALSE)
required <- c(
  "method_id", "dataset_thresholds", "infeasible_rate",
  "conditional_success", "grid_delivery_success",
  "median_width_ratio_to_tcsp", "median_elapsed_sec"
)
missing <- setdiff(required, names(summary))
if (length(missing)) {
  stopf("Summary CSV is missing required columns: ", paste(missing, collapse = ", "))
}

method_order <- c(
  "tcsp_mc",
  "hdp_s_mc",
  "tcsp_mti_gibbs_median_mc",
  "tcsp_mti_ecm_map_mc",
  "split_empirical_shortest",
  "split_ecm_fixed_tilt",
  "young_mathew",
  "wilks_minmax",
  "tcsp_dkw"
)
labels <- c(
  tcsp_mc = "TCSP scan",
  hdp_s_mc = "Hybrid DP--scan",
  tcsp_mti_gibbs_median_mc = "MTI Gibbs",
  tcsp_mti_ecm_map_mc = "MTI ECM",
  split_empirical_shortest = "Split empirical",
  split_ecm_fixed_tilt = "Split ECM",
  young_mathew = "Young--Mathew",
  wilks_minmax = "Wilks min--max",
  tcsp_dkw = "DKW scan"
)
roles <- c(
  tcsp_mc = "Shortest empirical scan action",
  hdp_s_mc = "Scan action with direct-DP content screen",
  tcsp_mti_gibbs_median_mc = "Fixed-target generalized-Bayes summary",
  tcsp_mti_ecm_map_mc = "Fixed-target generalized-Bayes mode",
  split_empirical_shortest = "Exact-spacing split action",
  split_ecm_fixed_tilt = "Exact-spacing split action with ECM pilot",
  young_mathew = "Classical interpolated comparator",
  wilks_minmax = "Classical full-range comparator",
  tcsp_dkw = "Conservative scan fallback"
)

selected <- summary[match(method_order, summary$method_id), , drop = FALSE]
if (any(is.na(selected$method_id))) {
  stopf("Summary CSV is missing method(s): ",
        paste(method_order[is.na(selected$method_id)], collapse = ", "))
}

format_pct <- function(x) sprintf("%.1f", 100 * as.numeric(x))
format_num <- function(x, digits = 3) {
  out <- sprintf(paste0("%.", digits, "f"), as.numeric(x))
  sub("\\.?0+$", "", out)
}
format_sec <- function(x) {
  x <- as.numeric(x)
  ifelse(x < 0.01, sprintf("%.3f", x), format_num(x, 3))
}
escape_latex <- function(x) {
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([_%&#])", "\\\\\\1", x, perl = TRUE)
  x
}

out <- data.frame(
  method_id = selected$method_id,
  method = unname(labels[selected$method_id]),
  role = unname(roles[selected$method_id]),
  dataset_thresholds = selected$dataset_thresholds,
  infeasible_rate = selected$infeasible_rate,
  grid_delivery_success = selected$grid_delivery_success,
  conditional_success = selected$conditional_success,
  median_width_ratio_to_tcsp = selected$median_width_ratio_to_tcsp,
  median_elapsed_sec = selected$median_elapsed_sec,
  stringsAsFactors = FALSE
)

utils::write.csv(out, csv_path, row.names = FALSE)

body <- vapply(seq_len(nrow(out)), function(ii) {
  sprintf(
    "%s & %s & %s & %s & %s & %s \\\\",
    escape_latex(out$method[[ii]]),
    escape_latex(out$role[[ii]]),
    format_pct(out$grid_delivery_success[[ii]]),
    format_pct(out$conditional_success[[ii]]),
    format_num(out$median_width_ratio_to_tcsp[[ii]], 3),
    format_sec(out$median_elapsed_sec[[ii]])
  )
}, character(1L))

lines <- c(
  "\\begin{tabularx}{\\textwidth}{@{}l>{\\raggedright\\arraybackslash}Xrrrr@{}}",
  "\\toprule",
  "Method & Role & Delivery (\\%) & Feasible success (\\%) & Width/TCSP & Median sec\\\\",
  "\\midrule",
  body,
  "\\bottomrule",
  "\\end{tabularx}"
)
writeLines(lines, tex_path)

cat("Wrote tolerance validation table:\n")
cat("  csv: ", csv_path, "\n", sep = "")
cat("  tex: ", tex_path, "\n", sep = "")
