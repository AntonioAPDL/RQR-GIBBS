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
  script_path <- "tables/generate_tolerance_validation_followup_tables.R"
}
script_path <- normalizePath(script_path, winslash = "/", mustWork = TRUE)
repo_root <- normalizePath(file.path(dirname(script_path), ".."),
                           winslash = "/", mustWork = TRUE)
setwd(repo_root)

default_root <- file.path(
  "application", "runs", "rqr_bayes_uq_followup_20260816"
)
default_paths <- c(
  ecm200 = file.path(
    default_root, "wave_ecm200_audit_20260817T005025Z",
    "bayes_uq_validation_results.csv"
  ),
  paper90 = file.path(
    default_root, "wave_paper_matched_90_20260817T005119Z",
    "bayes_uq_validation_results.csv"
  ),
  small95 = file.path(
    default_root, "wave_small_sample_95_20260817T005145Z",
    "bayes_uq_validation_results.csv"
  ),
  ecm500 = file.path(
    "application", "outputs", "tolerance_ecm_sensitivity",
    "ecm500_lognormal_hard_n1000_c099_20260818T013747Z",
    "bayes_uq_validation_results.csv"
  )
)

paths <- c(
  ecm200 = arg_value("--ecm200-results=", default_paths[["ecm200"]]),
  paper90 = arg_value("--paper90-results=", default_paths[["paper90"]]),
  small95 = arg_value("--small95-results=", default_paths[["small95"]])
)
ecm500_path <- arg_value("--ecm500-results=", default_paths[["ecm500"]])
ecm500_requested <- any(startsWith(args, "--ecm500-results="))
output_dir <- normalizePath(arg_value("--output-dir=", "tables"),
                            winslash = "/", mustWork = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
stationarity_tol <- as.numeric(arg_value("--ecm-stationarity-tol=", "1e-3"))[1L]
if (!is.finite(stationarity_tol) || stationarity_tol < 0) {
  stopf("--ecm-stationarity-tol must be finite and nonnegative.")
}

outputs <- c(
  "tolerance_validation_followup_design_summary.csv",
  "tolerance_validation_followup_design_summary.tex",
  "tolerance_validation_ecm_diagnostics.csv",
  "tolerance_validation_ecm_diagnostics.tex",
  "tolerance_validation_small_sample_content_summary.csv",
  "tolerance_validation_small_sample_content_summary.tex"
)
committed_outputs <- file.path(repo_root, "tables", outputs)
target_outputs <- file.path(output_dir, outputs)

missing_inputs <- paths[!file.exists(paths)]
if (length(missing_inputs)) {
  if (all(file.exists(committed_outputs))) {
    if (!identical(normalizePath(output_dir, winslash = "/", mustWork = FALSE),
                   normalizePath(file.path(repo_root, "tables"),
                                 winslash = "/", mustWork = TRUE))) {
      file.copy(committed_outputs, target_outputs, overwrite = TRUE)
    }
    cat("Using committed tolerance follow-up tables; provide run paths to regenerate.\n")
    quit(status = 0)
  }
  stopf("Missing follow-up result file(s): ",
        paste(names(missing_inputs), missing_inputs, sep = "=", collapse = "; "))
}
if (file.exists(ecm500_path)) {
  paths <- c(paths, ecm500 = ecm500_path)
} else if (ecm500_requested) {
  stopf("Missing ECM 500 sensitivity result file: ", ecm500_path)
}

truthy <- function(x) !is.na(x) & x
mean_or_na <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x)) mean(x) else NA_real_
}
median_or_na <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x)) stats::median(x) else NA_real_
}
quantile_or_na <- function(x, prob) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x)) {
    unname(stats::quantile(x, prob, names = FALSE, type = 8))
  } else {
    NA_real_
  }
}
format_pct <- function(x) {
  x <- as.numeric(x)
  ifelse(is.finite(x), sprintf("%.1f", 100 * x), "--")
}
format_num <- function(x, digits = 3) {
  x <- as.numeric(x)
  out <- ifelse(is.finite(x), sprintf(paste0("%.", digits, "f"), x), "--")
  sub("\\.?0+$", "", out)
}
format_sci <- function(x, digits = 2) {
  x <- as.numeric(x)
  ifelse(is.finite(x), sprintf(paste0("%.", digits, "e"), x), "--")
}
format_int <- function(x) {
  x <- as.numeric(x)
  ifelse(is.finite(x), format(round(x), big.mark = ",", scientific = FALSE),
         "--")
}
escape_latex <- function(x) {
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([_%&#])", "\\\\\\1", x, perl = TRUE)
  x
}

method_labels <- c(
  oracle_sh = "Oracle width reference",
  hdp_s_mc = "Hybrid DP--scan",
  tcsp_mc = "TCSP scan",
  tcsp_mti_gibbs_median_mc = "MTI Gibbs",
  tcsp_mti_ecm_map_mc = "MTI ECM",
  young_mathew = "Young--Mathew",
  wilks_minmax = "Wilks min--max",
  tcsp_dkw = "DKW scan"
)
design_labels <- c(
  ecm200_audit = "ECM 200-iteration diagnostic",
  paper_matched_90 = "Minimal 90% confidence design",
  small_sample_95 = "Small-sample 95% confidence design",
  ecm500_sensitivity = "ECM 500-iteration sensitivity"
)
public_design_ids <- c(
  ecm200_audit = "ecm_200_iteration_diagnostic",
  paper_matched_90 = "minimal_full_range_90_confidence",
  small_sample_95 = "small_sample_95_confidence",
  ecm500_sensitivity = "ecm_500_iteration_sensitivity"
)

read_design <- function(path, design_id) {
  out <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  required <- c(
    "mode", "method_id", "success", "infeasible", "width",
    "guaranteed_content", "ecm_final_stationarity",
    "ecm_relative_objective_drop", "ecm_trace_length"
  )
  missing <- setdiff(required, names(out))
  if (length(missing)) {
    stopf(design_id, " results are missing column(s): ",
          paste(missing, collapse = ", "))
  }
  out$design_id <- design_id
  out
}

results <- do.call(rbind, Map(read_design, paths, names(paths)))
results$method_label <- unname(method_labels[results$method_id])
results$method_label[is.na(results$method_label)] <-
  results$method_id[is.na(results$method_label)]
results$design_label <- unname(design_labels[results$mode])
results$design_label[is.na(results$design_label)] <-
  results$mode[is.na(results$design_label)]

summarize_group <- function(df) {
  feasible <- !truthy(df$infeasible)
  data.frame(
    replications = nrow(df),
    infeasible_rate = mean(truthy(df$infeasible)),
    delivery_success = mean(truthy(df$success)),
    returned_success = if (any(feasible)) {
      mean(truthy(df$success[feasible]))
    } else {
      NA_real_
    },
    stringsAsFactors = FALSE
  )
}

split_keys <- interaction(results$mode, results$method_id, drop = TRUE,
                          sep = "||")
design_summary <- do.call(rbind, lapply(split(results, split_keys), summarize_group))
keys <- do.call(rbind, strsplit(rownames(design_summary), "||", fixed = TRUE))
design_summary$mode <- keys[, 1L]
design_summary$method_id <- keys[, 2L]
rownames(design_summary) <- NULL
design_summary$design <- unname(design_labels[design_summary$mode])
design_summary$design_id <- unname(public_design_ids[design_summary$mode])
design_summary$method <- unname(method_labels[design_summary$method_id])
design_summary$design[is.na(design_summary$design)] <-
  design_summary$mode[is.na(design_summary$design)]
design_summary$design_id[is.na(design_summary$design_id)] <-
  design_summary$mode[is.na(design_summary$design_id)]
design_summary$method[is.na(design_summary$method)] <-
  design_summary$method_id[is.na(design_summary$method)]
design_order <- c("ecm200_audit", "paper_matched_90", "small_sample_95")
method_order <- c(
  "oracle_sh", "tcsp_mc", "hdp_s_mc", "tcsp_mti_ecm_map_mc",
  "young_mathew", "wilks_minmax", "tcsp_dkw"
)
design_order <- c(design_order, "ecm500_sensitivity")
design_summary <- design_summary[
  design_summary$method_id %in% method_order,
  ,
  drop = FALSE
]
design_summary <- design_summary[order(
  match(design_summary$mode, design_order),
  match(design_summary$method_id, method_order)
), ]
design_summary <- design_summary[c(
  "design_id", "design", "method_id", "method", "replications", "infeasible_rate",
  "delivery_success", "returned_success"
)]

ecm_rows <- results[results$method_id == "tcsp_mti_ecm_map_mc", , drop = FALSE]
ecm_diag <- do.call(rbind, lapply(split(ecm_rows, ecm_rows$mode), function(df) {
  stat <- as.numeric(df$ecm_final_stationarity)
  has_stat <- is.finite(stat)
  data.frame(
    mode = df$mode[[1L]],
    design_id = unname(public_design_ids[df$mode[[1L]]]),
    design = unname(design_labels[df$mode[[1L]]]),
    rows = nrow(df),
    rows_with_stationarity = sum(has_stat),
    stationarity_pass_rate = if (any(has_stat)) {
      mean(stat[has_stat] <= stationarity_tol)
    } else {
      NA_real_
    },
    median_stationarity = median_or_na(stat),
    p95_stationarity = quantile_or_na(stat, 0.95),
    max_stationarity = if (any(has_stat)) max(stat[has_stat]) else NA_real_,
    median_abs_relative_objective_drop =
      median_or_na(abs(df$ecm_relative_objective_drop)),
    median_trace_length = median_or_na(df$ecm_trace_length),
    stringsAsFactors = FALSE
  )
}))
ecm_diag$design[is.na(ecm_diag$design)] <-
  ecm_diag$mode[is.na(ecm_diag$design)]
ecm_diag$design_id[is.na(ecm_diag$design_id)] <-
  ecm_diag$mode[is.na(ecm_diag$design_id)]
ecm_diag <- ecm_diag[order(match(ecm_diag$mode, design_order)), ]
ecm_diag <- ecm_diag[c(
  "design_id", "design", "rows", "rows_with_stationarity",
  "stationarity_pass_rate", "median_stationarity", "p95_stationarity",
  "max_stationarity", "median_abs_relative_objective_drop",
  "median_trace_length"
)]

small_methods <- c(
  "tcsp_mc", "tcsp_mti_ecm_map_mc", "young_mathew", "wilks_minmax",
  "tcsp_dkw"
)
small <- results[
  results$mode == "small_sample_95" & results$method_id %in% small_methods,
  ,
  drop = FALSE
]
small_key <- interaction(
  sprintf("%.2f", as.numeric(small$guaranteed_content)),
  small$method_id,
  drop = TRUE,
  sep = "||"
)
small_content <- do.call(rbind, lapply(split(small, small_key), summarize_group))
small_keys <- do.call(rbind, strsplit(rownames(small_content), "||", fixed = TRUE))
small_content$guaranteed_content <- as.numeric(small_keys[, 1L])
small_content$method_id <- small_keys[, 2L]
rownames(small_content) <- NULL
small_content$method <- unname(method_labels[small_content$method_id])
small_content <- small_content[order(
  small_content$guaranteed_content,
  match(small_content$method_id, small_methods)
), ]
small_content <- small_content[c(
  "guaranteed_content", "method_id", "method", "replications",
  "infeasible_rate", "delivery_success", "returned_success"
)]

write_table <- function(df, name) {
  utils::write.csv(df, file.path(output_dir, paste0(name, ".csv")),
                   row.names = FALSE)
}

write_table(design_summary, "tolerance_validation_followup_design_summary")
write_table(ecm_diag, "tolerance_validation_ecm_diagnostics")
write_table(small_content, "tolerance_validation_small_sample_content_summary")

design_lines <- c(
  "\\begin{tabularx}{\\textwidth}{@{}>{\\raggedright\\arraybackslash}Xrrr@{}}",
  "\\toprule",
  "Method & Infeasible (\\%) & Content-attainment (\\%) & Interval-production rate (\\%)\\\\",
  "\\midrule"
)
for (design_name in unique(design_summary$design)) {
  block <- design_summary[
    design_summary$design == design_name,
    ,
    drop = FALSE
  ]
  design_lines <- c(
    design_lines,
    "\\addlinespace[0.35em]",
    sprintf("\\multicolumn{4}{@{}l}{\\textit{%s}}\\\\",
            escape_latex(design_name))
  )
  body <- vapply(seq_len(nrow(block)), function(ii) {
    sprintf(
      "%s & %s & %s & %s \\\\",
      escape_latex(block$method[[ii]]),
      format_pct(block$infeasible_rate[[ii]]),
      format_pct(block$delivery_success[[ii]]),
      format_pct(block$returned_success[[ii]])
    )
  }, character(1L))
  design_lines <- c(design_lines, body)
}
writeLines(c(
  design_lines,
  "\\bottomrule",
  "\\end{tabularx}"
), file.path(output_dir, "tolerance_validation_followup_design_summary.tex"))

ecm_body <- vapply(seq_len(nrow(ecm_diag)), function(ii) {
  sprintf(
    "%s & %s & %s & %s & %s & %s & %s \\\\",
    escape_latex(ecm_diag$design[[ii]]),
    format_int(ecm_diag$rows_with_stationarity[[ii]]),
    format_pct(ecm_diag$stationarity_pass_rate[[ii]]),
    format_num(ecm_diag$median_stationarity[[ii]], 5),
    format_num(ecm_diag$p95_stationarity[[ii]], 5),
    format_num(ecm_diag$max_stationarity[[ii]], 5),
    format_sci(ecm_diag$median_abs_relative_objective_drop[[ii]], 2)
  )
}, character(1L))
writeLines(c(
  "\\begin{tabular}{@{}lrrrrrr@{}}",
  "\\toprule",
  "Design & ECM rows & Pass (\\%) & Median stat. & P95 stat. & Max stat. & Median rel. obj.\\\\",
  "\\midrule",
  ecm_body,
  "\\bottomrule",
  "\\end{tabular}"
), file.path(output_dir, "tolerance_validation_ecm_diagnostics.tex"))

small_lines <- c(
  "\\begin{tabularx}{\\textwidth}{@{}>{\\raggedright\\arraybackslash}Xrrr@{}}",
  "\\toprule",
  "Method & Infeasible (\\%) & Content-attainment (\\%) & Interval-production rate (\\%)\\\\",
  "\\midrule"
)
for (cc in sort(unique(small_content$guaranteed_content))) {
  block <- small_content[
    abs(small_content$guaranteed_content - cc) < 1e-12,
    ,
    drop = FALSE
  ]
  small_lines <- c(
    small_lines,
    "\\addlinespace[0.35em]",
    sprintf("\\multicolumn{4}{@{}l}{\\textit{Target content $c=%.2f$}}\\\\",
            cc)
  )
  body <- vapply(seq_len(nrow(block)), function(ii) {
    sprintf(
      "%s & %s & %s & %s \\\\",
      escape_latex(block$method[[ii]]),
      format_pct(block$infeasible_rate[[ii]]),
      format_pct(block$delivery_success[[ii]]),
      format_pct(block$returned_success[[ii]])
    )
  }, character(1L))
  small_lines <- c(small_lines, body)
}
writeLines(c(
  small_lines,
  "\\bottomrule",
  "\\end{tabularx}"
), file.path(output_dir, "tolerance_validation_small_sample_content_summary.tex"))

cat("Wrote tolerance follow-up tables to: ", output_dir, "\n", sep = "")
