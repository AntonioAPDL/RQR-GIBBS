#!/usr/bin/env Rscript

script <- normalizePath("tables/generate_tolerance_validation_stratified_table.R",
                        winslash = "/", mustWork = TRUE)
work_dir <- tempfile("tolerance-stratified-table-test-")
dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
on.exit(unlink(work_dir, recursive = TRUE, force = TRUE), add = TRUE)

methods <- c(
  "tcsp_mc",
  "young_mathew",
  "wilks_minmax"
)
summary <- expand.grid(
  method_id = methods,
  n = c(50L, 500L, 1000L),
  c = c(0.90, 0.99),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)
summary$rows <- 10L
summary$dataset_thresholds <- "synthetic"
summary$infeasible_rate <- 0
summary$success_rate <- 0.96
summary$success_gap_vs_095 <- 0.01
summary$mean_content_gap <- 0.02
summary$median_width <- 1
summary_path <- file.path(work_dir, "summary.csv")
utils::write.csv(summary, summary_path, row.names = FALSE)
out_dir <- file.path(work_dir, "out")

status <- system2(
  "Rscript",
  c(script, paste0("--summary-csv=", summary_path),
    paste0("--output-dir=", out_dir)),
  stdout = TRUE,
  stderr = TRUE
)
if (!identical(attr(status, "status"), NULL)) {
  cat(status, sep = "\n")
  stop("Stratified tolerance validation table generator failed.",
       call. = FALSE)
}

csv_path <- file.path(out_dir, "tolerance_validation_by_n_content.csv")
tex_path <- file.path(out_dir, "tolerance_validation_by_n_content.tex")
tex_50_path <- file.path(out_dir, "tolerance_validation_by_n_50_content.tex")
tex_500_path <- file.path(out_dir, "tolerance_validation_by_n_500_content.tex")
tex_1000_path <- file.path(out_dir, "tolerance_validation_by_n_1000_content.tex")
stopifnot(file.exists(csv_path), file.exists(tex_path))
stopifnot(file.exists(tex_50_path), file.exists(tex_500_path),
          file.exists(tex_1000_path))

tab <- utils::read.csv(csv_path, stringsAsFactors = FALSE)
stopifnot(nrow(tab) ==
            length(methods) * length(unique(summary$n)) *
            length(unique(summary$c)))
stopifnot(!"hdp_s_mc" %in% tab$method_id)
stopifnot(!"tcsp_mti_gibbs_median_mc" %in% tab$method_id)
stopifnot(!"tcsp_mti_ecm_map_mc" %in% tab$method_id)
stopifnot(!"tcsp_dkw" %in% tab$method_id)
stopifnot(!"width_q025" %in% names(tab))
stopifnot(!"width_q975" %in% names(tab))
stopifnot(!"median_width" %in% names(tab))
stopifnot(!"median_width_ratio_to_tcsp" %in% names(tab))
stopifnot(!"median_elapsed_sec" %in% names(tab))
tex <- paste(readLines(tex_path, warn = FALSE), collapse = "\n")
stopifnot(grepl("Sample size", tex, fixed = TRUE))
stopifnot(grepl("Returned success", tex, fixed = TRUE))
stopifnot(!grepl("Width 95\\% range", tex, fixed = TRUE))
stopifnot(!grepl("Median width", tex, fixed = TRUE))
stopifnot(!grepl("Median sec", tex, fixed = TRUE))
stopifnot(!grepl("Width/TCSP", tex, fixed = TRUE))
stopifnot(!grepl("paper-matched", tex, ignore.case = TRUE))
stopifnot(!grepl("lane", tex, ignore.case = TRUE))
stopifnot(!grepl("Feasible success", tex, fixed = TRUE))
stopifnot(!grepl("posterior predictive", tex, fixed = TRUE))

tex_500 <- paste(readLines(tex_500_path, warn = FALSE), collapse = "\n")
stopifnot(grepl("Target content", tex_500, fixed = TRUE))
stopifnot(!grepl("Sample size", tex_500, fixed = TRUE))

cat("Stratified tolerance validation table test passed.\n")
