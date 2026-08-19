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
  "conditional_success", "grid_delivery_success"
)
missing <- setdiff(required, names(summary))
if (length(missing)) {
  stopf("Summary CSV is missing required columns: ", paste(missing, collapse = ", "))
}

method_order <- c(
  "tcsp_mc",
  "hdp_s_mc",
  "young_mathew",
  "wilks_minmax"
)
labels <- c(
  tcsp_mc = "TCSP scan",
  hdp_s_mc = "Hybrid DP--scan",
  young_mathew = "Young--Mathew",
  wilks_minmax = "Wilks min--max"
)
roles <- c(
  tcsp_mc = "Shortest empirical scan action",
  hdp_s_mc = "Scan action with direct-DP content screen",
  young_mathew = "Classical interpolated comparator",
  wilks_minmax = "Classical full-range comparator"
)

selected <- summary[match(method_order, summary$method_id), , drop = FALSE]
if (any(is.na(selected$method_id))) {
  stopf("Summary CSV is missing method(s): ",
        paste(method_order[is.na(selected$method_id)], collapse = ", "))
}

format_pct <- function(x) sprintf("%.1f", 100 * as.numeric(x))
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
  returned_success = selected$conditional_success,
  stringsAsFactors = FALSE
)

utils::write.csv(out, csv_path, row.names = FALSE)

body <- vapply(seq_len(nrow(out)), function(ii) {
  sprintf(
    "%s & %s & %s & %s \\\\",
    escape_latex(out$method[[ii]]),
    escape_latex(out$role[[ii]]),
    format_pct(out$grid_delivery_success[[ii]]),
    format_pct(out$returned_success[[ii]])
  )
}, character(1L))

lines <- c(
  "\\begin{tabularx}{\\textwidth}{@{}l>{\\raggedright\\arraybackslash}Xrr@{}}",
  "\\toprule",
  "Method & Role & Delivery (\\%) & Returned success (\\%)\\\\",
  "\\midrule",
  body,
  "\\bottomrule",
  "\\end{tabularx}"
)
writeLines(lines, tex_path)

cat("Wrote tolerance validation table:\n")
cat("  csv: ", csv_path, "\n", sep = "")
cat("  tex: ", tex_path, "\n", sep = "")
