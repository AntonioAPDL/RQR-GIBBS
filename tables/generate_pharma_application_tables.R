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
  script_path <- "tables/generate_pharma_application_tables.R"
}
script_path <- normalizePath(script_path, winslash = "/", mustWork = TRUE)
repo_root <- normalizePath(file.path(dirname(script_path), ".."),
                           winslash = "/", mustWork = TRUE)
setwd(repo_root)

source(file.path(repo_root, "application", "scripts", "lib",
                 "pharma_tolerance_application.R"))

results_path <- arg_value(
  "--results-csv=",
  Sys.getenv("RQR_PHARMA_APPLICATION_RESULTS", unset = "")
)
output_dir <- normalizePath(arg_value("--output-dir=", "tables"),
                            winslash = "/", mustWork = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

summary_csv <- file.path(output_dir, "pharma_application_summary.csv")
primary_csv <- file.path(output_dir, "pharma_application_primary_summary.csv")
primary_tex <- file.path(output_dir, "pharma_application_primary_summary.tex")
supp_csv <- file.path(output_dir, "pharma_application_supplement_summary.csv")
supp_tex <- file.path(output_dir, "pharma_application_supplement_summary.tex")
sens_csv <- file.path(output_dir,
                      "pharma_application_dependence_sensitivity.csv")
sens_tex <- file.path(output_dir,
                      "pharma_application_dependence_sensitivity.tex")

committed_required <- file.path("tables", c(
  "pharma_application_summary.csv",
  "pharma_application_primary_summary.csv",
  "pharma_application_primary_summary.tex",
  "pharma_application_supplement_summary.csv",
  "pharma_application_supplement_summary.tex",
  "pharma_application_dependence_sensitivity.csv",
  "pharma_application_dependence_sensitivity.tex"
))

if (!nzchar(results_path) || !file.exists(results_path)) {
  if (all(file.exists(committed_required))) {
    target_required <- file.path(output_dir, basename(committed_required))
    if (!identical(normalizePath(output_dir, winslash = "/", mustWork = FALSE),
                   normalizePath("tables", winslash = "/", mustWork = TRUE))) {
      file.copy(committed_required, target_required, overwrite = TRUE)
    }
    cat("Using committed pharmaceutical application tables.\n")
    quit(status = 0L)
  }
  stopf("Missing application results CSV. Set --results-csv or ",
        "RQR_PHARMA_APPLICATION_RESULTS.")
}

results <- pta_read_csv(results_path)
required <- c(
  "response_id", "response_role", "method_id", "method",
  "split_id", "interval_returned", "lower", "upper", "width",
  "heldout_empirical_content", "heldout_lower_omitted",
  "heldout_upper_omitted", "heldout_attains_content"
)
missing <- setdiff(required, names(results))
if (length(missing)) {
  stopf("Application results are missing column(s): ",
        paste(missing, collapse = ", "))
}

summary <- pta_summarize_results(results)
pta_atomic_write_csv(summary, summary_csv)

method_order <- c("tcsp_mc", "mti_ecm_adaptive_cell",
                  "young_mathew", "wilks_minmax")
format_interval <- function(lower, upper) {
  paste0("[", pta_format_num(lower, 3), ", ", pta_format_num(upper, 3), "]")
}
format_width <- function(median, lo, hi) {
  paste0(pta_format_num(median, 3), " [",
         pta_format_num(lo, 3), ", ", pta_format_num(hi, 3), "]")
}
format_content <- function(median, lo, hi) {
  paste0(pta_format_pct(median, 1), " [",
         pta_format_pct(lo, 1), ", ", pta_format_pct(hi, 1), "]")
}
format_omission <- function(lower, upper) {
  paste0(pta_format_pct(lower, 1), " / ", pta_format_pct(upper, 1))
}

write_method_table <- function(rows, csv_path, tex_path, caption_label) {
  rows <- rows[order(match(rows$method_id, method_order)), , drop = FALSE]
  out <- data.frame(
    method_id = rows$method_id,
    method = rows$method,
    splits = rows$splits,
    interval_return_rate = rows$interval_return_rate,
    median_interval = format_interval(rows$lower_median, rows$upper_median),
    width_summary = format_width(rows$width_median, rows$width_q025,
                                 rows$width_q975),
    heldout_content_summary = format_content(
      rows$heldout_content_median,
      rows$heldout_content_q025,
      rows$heldout_content_q975
    ),
    lower_upper_omitted = format_omission(
      rows$lower_omitted_median,
      rows$upper_omitted_median
    ),
    stringsAsFactors = FALSE
  )
  pta_atomic_write_csv(out, csv_path)
  body <- vapply(seq_len(nrow(out)), function(ii) {
    sprintf(
      "%s & %s & %s & %s & %s \\\\",
      pta_escape_latex(out$method[[ii]]),
      pta_escape_latex(out$median_interval[[ii]]),
      pta_escape_latex(out$width_summary[[ii]]),
      pta_escape_latex(out$heldout_content_summary[[ii]]),
      pta_escape_latex(out$lower_upper_omitted[[ii]])
    )
  }, character(1L))
  lines <- c(
    "\\begin{tabularx}{\\textwidth}{@{}l>{\\raggedright\\arraybackslash}X>{\\raggedright\\arraybackslash}X>{\\raggedright\\arraybackslash}Xr@{}}",
    "\\toprule",
    "Method & Median interval & Width median [95\\% range] & Held-out content median [95\\% range] & Omitted below/above\\\\",
    "\\midrule",
    body,
    "\\bottomrule",
    "\\end{tabularx}"
  )
  writeLines(lines, tex_path)
  invisible(out)
}

primary <- summary[summary$response_role == "primary", , drop = FALSE]
supp <- summary[summary$response_role == "supplement", , drop = FALSE]
if (nrow(primary) != length(method_order) || nrow(supp) != length(method_order)) {
  stopf("Expected four methods for both application responses.")
}
write_method_table(primary, primary_csv, primary_tex, "primary")
write_method_table(supp, supp_csv, supp_tex, "supplement")

sensitivity_path <- arg_value(
  "--sensitivity-csv=",
  sub("pharma_application_results.csv",
      "pharma_application_dependence_sensitivity.csv",
      results_path,
      fixed = TRUE)
)
if (nzchar(sensitivity_path) && file.exists(sensitivity_path)) {
  sensitivity <- pta_read_csv(sensitivity_path)
  pta_atomic_write_csv(sensitivity, sens_csv)
  group_labels <- c(
    start = "Production month",
    api_batch = "API batch",
    batch_order_quartile = "Batch-order quartile"
  )
  keep <- sensitivity[sensitivity$response_role %in% c("primary", "supplement"),
                      , drop = FALSE]
  keep <- keep[order(keep$response_role, keep$grouping,
                     match(keep$method_id, method_order)), , drop = FALSE]
  body <- vapply(seq_len(nrow(keep)), function(ii) {
    sprintf(
      "%s & %s & %s & %s & %s \\\\",
      pta_escape_latex(ifelse(keep$response_role[[ii]] == "primary",
                              "Tensile strength", "Weight RSD")),
      pta_escape_latex(group_labels[keep$grouping[[ii]]] %||%
                         keep$grouping[[ii]]),
      pta_escape_latex(keep$method[[ii]]),
      pta_format_pct(keep$median_group_content_range[[ii]], 1),
      pta_format_pct(keep$q975_group_content_range[[ii]], 1)
    )
  }, character(1L))
  lines <- c(
    "\\begin{tabularx}{\\textwidth}{@{}lllrr@{}}",
    "\\toprule",
    "Response & Grouping & Method & Median range & 97.5\\% range\\\\",
    "\\midrule",
    body,
    "\\bottomrule",
    "\\end{tabularx}"
  )
  writeLines(lines, sens_tex)
}

cat("Wrote pharmaceutical application tables:\n")
cat("  ", primary_tex, "\n", sep = "")
cat("  ", supp_tex, "\n", sep = "")
