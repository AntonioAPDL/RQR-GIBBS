#!/usr/bin/env Rscript

script <- normalizePath("tables/generate_tolerance_validation_followup_tables.R",
                        winslash = "/", mustWork = TRUE)
work_dir <- tempfile("tolerance-followup-table-test-")
dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
on.exit(unlink(work_dir, recursive = TRUE, force = TRUE), add = TRUE)

template <- data.frame(
  mode = character(),
  method_id = character(),
  success = logical(),
  infeasible = logical(),
  width = numeric(),
  width_ratio_to_reference = numeric(),
  width_ratio_to_oracle_sh = numeric(),
  guaranteed_content = numeric(),
  ecm_final_stationarity = numeric(),
  ecm_relative_objective_drop = numeric(),
  ecm_trace_length = numeric(),
  stringsAsFactors = FALSE
)

row <- function(mode, method_id, success, infeasible, content,
                width = 2, width_ref = 1, width_oracle = 1.2,
                stat = NA_real_, rel_drop = NA_real_, trace = NA_real_) {
  data.frame(
    mode = mode,
    method_id = method_id,
    success = success,
    infeasible = infeasible,
    width = width,
    width_ratio_to_reference = width_ref,
    width_ratio_to_oracle_sh = width_oracle,
    guaranteed_content = content,
    ecm_final_stationarity = stat,
    ecm_relative_objective_drop = rel_drop,
    ecm_trace_length = trace,
    stringsAsFactors = FALSE
  )
}

ecm200 <- rbind(
  row("ecm200_audit", "tcsp_mc", TRUE, FALSE, 0.90),
  row("ecm200_audit", "tcsp_mti_ecm_map_mc", TRUE, FALSE, 0.90,
      stat = 5e-4, rel_drop = 1e-12, trace = 201),
  row("ecm200_audit", "tcsp_mti_ecm_map_mc", TRUE, FALSE, 0.90,
      stat = 2e-3, rel_drop = 2e-12, trace = 201)
)
paper90 <- rbind(
  row("paper_matched_90", "tcsp_mc", TRUE, FALSE, 0.90),
  row("paper_matched_90", "tcsp_mti_ecm_map_mc", FALSE, TRUE, 0.90,
      width_ref = NA, width_oracle = NA)
)
small95 <- rbind(
  row("small_sample_95", "tcsp_mc", TRUE, FALSE, 0.90),
  row("small_sample_95", "young_mathew", TRUE, FALSE, 0.90, width_ref = 0.99),
  row("small_sample_95", "wilks_minmax", FALSE, FALSE, 0.99),
  row("small_sample_95", "tcsp_mti_ecm_map_mc", TRUE, FALSE, 0.90,
      stat = 7e-4, rel_drop = 1e-13, trace = 201),
  row("small_sample_95", "tcsp_mti_gibbs_median_mc", TRUE, FALSE, 0.90,
      width_ref = 1.2),
  row("small_sample_95", "tcsp_dkw", FALSE, TRUE, 0.95,
      width_ref = NA, width_oracle = NA)
)
ecm500 <- rbind(
  row("ecm500_sensitivity", "tcsp_mc", TRUE, FALSE, 0.99),
  row("ecm500_sensitivity", "tcsp_mti_ecm_map_mc", TRUE, FALSE, 0.99,
      stat = 1.2e-3, rel_drop = 1e-13, trace = 501),
  row("ecm500_sensitivity", "young_mathew", TRUE, FALSE, 0.99,
      width_ref = 1.1),
  row("ecm500_sensitivity", "wilks_minmax", TRUE, FALSE, 0.99,
      width_ref = 1.4)
)

paths <- file.path(
  work_dir, c("ecm200.csv", "paper90.csv", "small95.csv", "ecm500.csv")
)
utils::write.csv(ecm200, paths[[1L]], row.names = FALSE)
utils::write.csv(paper90, paths[[2L]], row.names = FALSE)
utils::write.csv(small95, paths[[3L]], row.names = FALSE)
utils::write.csv(ecm500, paths[[4L]], row.names = FALSE)

out_dir <- file.path(work_dir, "out")
status <- system2(
  "Rscript",
  c(
    script,
    paste0("--ecm200-results=", paths[[1L]]),
    paste0("--paper90-results=", paths[[2L]]),
    paste0("--small95-results=", paths[[3L]]),
    paste0("--ecm500-results=", paths[[4L]]),
    paste0("--output-dir=", out_dir)
  ),
  stdout = TRUE,
  stderr = TRUE
)
if (!identical(attr(status, "status"), NULL)) {
  cat(status, sep = "\n")
  stop("Tolerance follow-up table generator failed.", call. = FALSE)
}

required <- file.path(out_dir, c(
  "tolerance_validation_followup_design_summary.csv",
  "tolerance_validation_followup_design_summary.tex",
  "tolerance_validation_ecm_diagnostics.csv",
  "tolerance_validation_ecm_diagnostics.tex",
  "tolerance_validation_small_sample_content_summary.csv",
  "tolerance_validation_small_sample_content_summary.tex"
))
stopifnot(all(file.exists(required)))

diag <- read.csv(file.path(out_dir, "tolerance_validation_ecm_diagnostics.csv"),
                 stringsAsFactors = FALSE)
design <- read.csv(
  file.path(out_dir, "tolerance_validation_followup_design_summary.csv"),
  stringsAsFactors = FALSE
)
stopifnot(!"median_elapsed_sec" %in% names(design))
stopifnot(!"median_width" %in% names(design))
stopifnot(!"median_width_ratio_to_tcsp" %in% names(design))
stopifnot(!"median_width_ratio_to_oracle" %in% names(design))
stopifnot(!"width_q025" %in% names(design))
stopifnot(!"width_q975" %in% names(design))
ecm200_diag <- diag[
  diag$design_id == "ecm_200_iteration_diagnostic",
  ,
  drop = FALSE
]
stopifnot(nrow(ecm200_diag) == 1L)
stopifnot(abs(ecm200_diag$stationarity_pass_rate - 0.5) < 1e-12)
stopifnot(any(diag$design_id == "ecm_500_iteration_sensitivity"))

small <- read.csv(
  file.path(out_dir, "tolerance_validation_small_sample_content_summary.csv"),
  stringsAsFactors = FALSE
)
stopifnot(any(small$guaranteed_content == 0.99))
stopifnot(any(small$method_id == "wilks_minmax"))
stopifnot(!any(small$method_id == "tcsp_mti_gibbs_median_mc"))
stopifnot(!"median_elapsed_sec" %in% names(small))
stopifnot(!"median_width" %in% names(small))
stopifnot(!"width_q025" %in% names(small))
stopifnot(!"width_q975" %in% names(small))

tex <- paste(readLines(
  file.path(out_dir, "tolerance_validation_ecm_diagnostics.tex"),
  warn = FALSE
), collapse = "\n")
stopifnot(grepl("Median rel. obj.", tex, fixed = TRUE))
stopifnot(!grepl("posterior predictive", tex, fixed = TRUE))

design_tex <- paste(readLines(
  file.path(out_dir, "tolerance_validation_followup_design_summary.tex"),
  warn = FALSE
), collapse = "\n")
stopifnot(grepl("Method & Infeasible", design_tex, fixed = TRUE))
stopifnot(grepl("Minimal 90\\% confidence design", design_tex, fixed = TRUE))
stopifnot(grepl("ECM 500-iteration sensitivity", design_tex, fixed = TRUE))
stopifnot(grepl("Returned success", design_tex, fixed = TRUE))
stopifnot(!grepl("Width 95\\% range", design_tex, fixed = TRUE))
stopifnot(!grepl("Median sec", design_tex, fixed = TRUE))
stopifnot(!grepl("Width/TCSP", design_tex, fixed = TRUE))
stopifnot(!grepl("MTI Gibbs", design_tex, fixed = TRUE))
stopifnot(!grepl("Lane", design_tex, fixed = TRUE))
stopifnot(!grepl("Paper-matched", design_tex, fixed = TRUE))
stopifnot(!grepl("Feasible success", design_tex, fixed = TRUE))

small_tex <- paste(readLines(
  file.path(out_dir, "tolerance_validation_small_sample_content_summary.tex"),
  warn = FALSE
), collapse = "\n")
stopifnot(grepl("Target content", small_tex, fixed = TRUE))
stopifnot(grepl("Returned success", small_tex, fixed = TRUE))
stopifnot(!grepl("Width 95\\% range", small_tex, fixed = TRUE))
stopifnot(!grepl("Median sec", small_tex, fixed = TRUE))
stopifnot(!grepl("Width/TCSP", small_tex, fixed = TRUE))
stopifnot(!grepl("Feasible success", small_tex, fixed = TRUE))

cat("Tolerance follow-up table test passed.\n")
