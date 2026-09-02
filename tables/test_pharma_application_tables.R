#!/usr/bin/env Rscript

script <- normalizePath("tables/generate_pharma_application_tables.R",
                        winslash = "/", mustWork = TRUE)
work_dir <- tempfile("pharma-table-test-")
dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
on.exit(unlink(work_dir, recursive = TRUE, force = TRUE), add = TRUE)

row <- function(response, role, method, split, lower, upper, content) {
  data.frame(
    response_id = response,
    response_role = role,
    method_id = method,
    method = c(
      tcsp_mc = "TCSP",
      mti_ecm_adaptive_cell = "MTI-ECM",
      young_mathew = "Young--Mathew",
      wilks_minmax = "Wilks"
    )[[method]],
    split_id = split,
    interval_returned = TRUE,
    lower = lower,
    upper = upper,
    width = upper - lower,
    heldout_empirical_content = content,
    heldout_lower_omitted = max(0, (1 - content) / 2),
    heldout_upper_omitted = max(0, (1 - content) / 2),
    heldout_attains_content = content >= 0.90,
    stringsAsFactors = FALSE
  )
}
methods <- c("tcsp_mc", "mti_ecm_adaptive_cell",
             "young_mathew", "wilks_minmax")
results <- do.call(rbind, lapply(c("primary", "supplement"), function(role) {
  response <- if (role == "primary") "fct_tensile" else "tbl_rsd_weight"
  do.call(rbind, lapply(methods, function(method) {
    do.call(rbind, lapply(1:4, function(split) {
      width <- switch(method, tcsp_mc = 0.40,
                      mti_ecm_adaptive_cell = 0.36,
                      young_mathew = 0.42,
                      wilks_minmax = 0.60) + split / 100
      row(response, role, method, split, lower = 1, upper = 1 + width,
          content = 0.91 + split / 100)
    }))
  }))
}))
sensitivity <- expand.grid(
  response_id = c("fct_tensile", "tbl_rsd_weight"),
  response_role = c("primary", "supplement"),
  method_id = methods,
  grouping = c("start", "api_batch", "batch_order_quartile"),
  stringsAsFactors = FALSE
)
sensitivity$method <- c(
  tcsp_mc = "TCSP",
  mti_ecm_adaptive_cell = "MTI-ECM",
  young_mathew = "Young--Mathew",
  wilks_minmax = "Wilks"
)[sensitivity$method_id]
sensitivity$splits <- 4L
sensitivity$median_minimum_group_content <- 0.85
sensitivity$median_maximum_group_content <- 0.98
sensitivity$median_group_content_range <- 0.13
sensitivity$q975_group_content_range <- 0.20

results_path <- file.path(work_dir, "results.csv")
sensitivity_path <- file.path(work_dir, "sensitivity.csv")
utils::write.csv(results, results_path, row.names = FALSE)
utils::write.csv(sensitivity, sensitivity_path, row.names = FALSE)
out_dir <- file.path(work_dir, "tables")
status <- system2(
  "Rscript",
  c(
    script,
    paste0("--results-csv=", results_path),
    paste0("--sensitivity-csv=", sensitivity_path),
    paste0("--output-dir=", out_dir)
  ),
  stdout = TRUE,
  stderr = TRUE
)
if (!identical(attr(status, "status"), NULL)) {
  cat(status, sep = "\n")
  stop("Pharmaceutical application table generator failed.", call. = FALSE)
}

required <- file.path(out_dir, c(
  "pharma_application_summary.csv",
  "pharma_application_primary_summary.csv",
  "pharma_application_primary_summary.tex",
  "pharma_application_supplement_summary.csv",
  "pharma_application_supplement_summary.tex",
  "pharma_application_dependence_sensitivity.csv",
  "pharma_application_dependence_sensitivity.tex"
))
stopifnot(all(file.exists(required)))
tab <- read.csv(file.path(out_dir, "pharma_application_primary_summary.csv"),
                stringsAsFactors = FALSE)
stopifnot(nrow(tab) == 4L)
stopifnot(all(methods %in% tab$method_id))
tex <- paste(readLines(file.path(out_dir,
                                 "pharma_application_primary_summary.tex"),
                       warn = FALSE), collapse = "\n")
stopifnot(grepl("Held-out content median", tex, fixed = TRUE))
stopifnot(grepl("Omitted below/above", tex, fixed = TRUE))
stopifnot(!grepl("metadata", tex, ignore.case = TRUE))
stopifnot(!grepl("pipeline", tex, ignore.case = TRUE))
cat("Pharmaceutical application table test passed.\n")
