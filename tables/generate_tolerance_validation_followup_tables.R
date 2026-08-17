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
  )
)

paths <- c(
  ecm200 = arg_value("--ecm200-results=", default_paths[["ecm200"]]),
  paper90 = arg_value("--paper90-results=", default_paths[["paper90"]]),
  small95 = arg_value("--small95-results=", default_paths[["small95"]])
)
output_dir <- normalizePath(arg_value("--output-dir=", "tables"),
                            winslash = "/", mustWork = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
stationarity_tol <- as.numeric(arg_value("--ecm-stationarity-tol=", "1e-3"))[1L]
if (!is.finite(stationarity_tol) || stationarity_tol < 0) {
  stopf("--ecm-stationarity-tol must be finite and nonnegative.")
}

outputs <- c(
  "tolerance_validation_followup_lane_summary.csv",
  "tolerance_validation_followup_lane_summary.tex",
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
  if (length(x)) unname(stats::quantile(x, prob, names = FALSE)) else NA_real_
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
format_sec <- function(x) {
  x <- as.numeric(x)
  ifelse(!is.finite(x), "--", ifelse(x < 0.01, sprintf("%.3f", x),
                                     format_num(x, 3)))
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
  oracle_sh = "Oracle SH",
  hdp_s_mc = "Hybrid DP--scan",
  tcsp_mc = "TCSP scan",
  tcsp_mti_gibbs_median_mc = "MTI Gibbs",
  tcsp_mti_ecm_map_mc = "MTI ECM",
  young_mathew = "Young--Mathew",
  wilks_minmax = "Wilks min--max",
  tcsp_dkw = "DKW scan"
)
lane_labels <- c(
  ecm200_audit = "ECM-200 audit",
  paper_matched_90 = "Paper-matched 90%",
  small_sample_95 = "Small-sample 95%"
)

read_lane <- function(path, lane_id) {
  out <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  required <- c(
    "mode", "method_id", "success", "infeasible", "width_ratio_to_reference",
    "width_ratio_to_oracle_sh", "elapsed_sec", "guaranteed_content",
    "ecm_final_stationarity", "ecm_relative_objective_drop",
    "ecm_trace_length"
  )
  missing <- setdiff(required, names(out))
  if (length(missing)) {
    stopf(lane_id, " results are missing column(s): ",
          paste(missing, collapse = ", "))
  }
  out$lane_id <- lane_id
  out
}

results <- do.call(rbind, Map(read_lane, paths, names(paths)))
results$method_label <- unname(method_labels[results$method_id])
results$method_label[is.na(results$method_label)] <-
  results$method_id[is.na(results$method_label)]
results$lane_label <- unname(lane_labels[results$mode])
results$lane_label[is.na(results$lane_label)] <-
  results$mode[is.na(results$lane_label)]

summarize_group <- function(df) {
  feasible <- !truthy(df$infeasible)
  data.frame(
    replications = nrow(df),
    infeasible_rate = mean(truthy(df$infeasible)),
    delivery_success = mean(truthy(df$success)),
    feasible_success = if (any(feasible)) {
      mean(truthy(df$success[feasible]))
    } else {
      NA_real_
    },
    median_width_ratio_to_tcsp = median_or_na(df$width_ratio_to_reference),
    median_width_ratio_to_oracle = median_or_na(df$width_ratio_to_oracle_sh),
    median_elapsed_sec = median_or_na(df$elapsed_sec),
    stringsAsFactors = FALSE
  )
}

split_keys <- interaction(results$mode, results$method_id, drop = TRUE,
                          sep = "||")
lane_summary <- do.call(rbind, lapply(split(results, split_keys), summarize_group))
keys <- do.call(rbind, strsplit(rownames(lane_summary), "||", fixed = TRUE))
lane_summary$mode <- keys[, 1L]
lane_summary$method_id <- keys[, 2L]
rownames(lane_summary) <- NULL
lane_summary$lane <- unname(lane_labels[lane_summary$mode])
lane_summary$method <- unname(method_labels[lane_summary$method_id])
lane_summary$lane[is.na(lane_summary$lane)] <- lane_summary$mode[is.na(lane_summary$lane)]
lane_summary$method[is.na(lane_summary$method)] <- lane_summary$method_id[is.na(lane_summary$method)]
lane_order <- c("ecm200_audit", "paper_matched_90", "small_sample_95")
method_order <- c(
  "oracle_sh", "tcsp_mc", "hdp_s_mc", "tcsp_mti_gibbs_median_mc",
  "tcsp_mti_ecm_map_mc", "young_mathew", "wilks_minmax", "tcsp_dkw"
)
lane_summary <- lane_summary[order(
  match(lane_summary$mode, lane_order),
  match(lane_summary$method_id, method_order)
), ]
lane_summary <- lane_summary[c(
  "mode", "lane", "method_id", "method", "replications", "infeasible_rate",
  "delivery_success", "feasible_success", "median_width_ratio_to_tcsp",
  "median_width_ratio_to_oracle", "median_elapsed_sec"
)]

ecm_rows <- results[results$method_id == "tcsp_mti_ecm_map_mc", , drop = FALSE]
ecm_diag <- do.call(rbind, lapply(split(ecm_rows, ecm_rows$mode), function(df) {
  stat <- as.numeric(df$ecm_final_stationarity)
  has_stat <- is.finite(stat)
  data.frame(
    mode = df$mode[[1L]],
    lane = unname(lane_labels[df$mode[[1L]]]),
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
ecm_diag$lane[is.na(ecm_diag$lane)] <- ecm_diag$mode[is.na(ecm_diag$lane)]
ecm_diag <- ecm_diag[order(match(ecm_diag$mode, lane_order)), ]

small_methods <- c(
  "tcsp_mc", "tcsp_mti_gibbs_median_mc", "tcsp_mti_ecm_map_mc",
  "young_mathew", "wilks_minmax", "tcsp_dkw"
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
  "infeasible_rate", "delivery_success", "feasible_success",
  "median_width_ratio_to_tcsp", "median_elapsed_sec"
)]

write_table <- function(df, name) {
  utils::write.csv(df, file.path(output_dir, paste0(name, ".csv")),
                   row.names = FALSE)
}

write_table(lane_summary, "tolerance_validation_followup_lane_summary")
write_table(ecm_diag, "tolerance_validation_ecm_diagnostics")
write_table(small_content, "tolerance_validation_small_sample_content_summary")

lane_body <- vapply(seq_len(nrow(lane_summary)), function(ii) {
  sprintf(
    "%s & %s & %s & %s & %s & %s & %s \\\\",
    escape_latex(lane_summary$lane[[ii]]),
    escape_latex(lane_summary$method[[ii]]),
    format_pct(lane_summary$infeasible_rate[[ii]]),
    format_pct(lane_summary$delivery_success[[ii]]),
    format_pct(lane_summary$feasible_success[[ii]]),
    format_num(lane_summary$median_width_ratio_to_tcsp[[ii]], 3),
    format_sec(lane_summary$median_elapsed_sec[[ii]])
  )
}, character(1L))
writeLines(c(
  "\\begin{tabularx}{\\textwidth}{@{}l>{\\raggedright\\arraybackslash}Xrrrrr@{}}",
  "\\toprule",
  "Lane & Method & Infeasible (\\%) & Delivery (\\%) & Feasible success (\\%) & Width/TCSP & Median sec\\\\",
  "\\midrule",
  lane_body,
  "\\bottomrule",
  "\\end{tabularx}"
), file.path(output_dir, "tolerance_validation_followup_lane_summary.tex"))

ecm_body <- vapply(seq_len(nrow(ecm_diag)), function(ii) {
  sprintf(
    "%s & %s & %s & %s & %s & %s & %s \\\\",
    escape_latex(ecm_diag$lane[[ii]]),
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
  "Lane & ECM rows & Pass (\\%) & Median stat. & P95 stat. & Max stat. & Median rel. obj.\\\\",
  "\\midrule",
  ecm_body,
  "\\bottomrule",
  "\\end{tabular}"
), file.path(output_dir, "tolerance_validation_ecm_diagnostics.tex"))

small_body <- vapply(seq_len(nrow(small_content)), function(ii) {
  sprintf(
    "%.2f & %s & %s & %s & %s & %s & %s \\\\",
    small_content$guaranteed_content[[ii]],
    escape_latex(small_content$method[[ii]]),
    format_pct(small_content$infeasible_rate[[ii]]),
    format_pct(small_content$delivery_success[[ii]]),
    format_pct(small_content$feasible_success[[ii]]),
    format_num(small_content$median_width_ratio_to_tcsp[[ii]], 3),
    format_sec(small_content$median_elapsed_sec[[ii]])
  )
}, character(1L))
writeLines(c(
  "\\begin{tabular}{@{}llrrrrr@{}}",
  "\\toprule",
  "Content & Method & Infeasible (\\%) & Delivery (\\%) & Feasible success (\\%) & Width/TCSP & Median sec\\\\",
  "\\midrule",
  small_body,
  "\\bottomrule",
  "\\end{tabular}"
), file.path(output_dir, "tolerance_validation_small_sample_content_summary.tex"))

cat("Wrote tolerance follow-up tables to: ", output_dir, "\n", sep = "")
