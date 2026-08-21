#!/usr/bin/env Rscript

script <- normalizePath(
  "tables/generate_tolerance_validation_scenario_tables.R",
  winslash = "/",
  mustWork = TRUE
)
work_dir <- tempfile("tolerance-scenario-table-test-")
dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
on.exit(unlink(work_dir, recursive = TRUE, force = TRUE), add = TRUE)

row <- function(dgp, n, content, rep, method, success = TRUE,
                infeasible = FALSE, width = 2) {
  data.frame(
    dgp_id = dgp,
    n = n,
    guaranteed_content = content,
    tolerance_confidence = 0.95,
    posterior_confidence = 0.95,
    replication = rep,
    method_id = method,
    success = success,
    infeasible = infeasible,
    width = width,
    stringsAsFactors = FALSE
  )
}

methods <- c(
  "tcsp_mc", "hdp_s_mc", "tcsp_mti_ecm_map_mc", "wilks_minmax", "tcsp_dkw"
)
primary <- do.call(rbind, lapply(c("normal", "student_t3"), function(dgp) {
  do.call(rbind, lapply(c(500L, 1000L), function(n) {
    do.call(rbind, lapply(1:2, function(rep) {
      rbind(
        row(dgp, n, 0.90, rep, "tcsp_mc", width = 2),
        row(dgp, n, 0.90, rep, "hdp_s_mc", width = 2),
        row(dgp, n, 0.90, rep, "tcsp_mti_ecm_map_mc",
            success = dgp == "normal", width = 2),
        row(dgp, n, 0.90, rep, "wilks_minmax", width = 3),
        row(dgp, n, 0.90, rep, "tcsp_dkw", infeasible = TRUE,
            success = FALSE, width = NA_real_)
      )
    }))
  }))
}))
young <- do.call(rbind, lapply(c("normal", "student_t3"), function(dgp) {
  do.call(rbind, lapply(c(500L, 1000L), function(n) {
    do.call(rbind, lapply(1:2, function(rep) {
      row(dgp, n, 0.90, rep, "young_mathew",
          success = rep == 1L || dgp == "normal", width = 1.9)
    }))
  }))
}))
small <- do.call(rbind, lapply(c("normal", "student_t3", "beta_left"), function(dgp) {
  do.call(rbind, lapply(1:2, function(rep) {
    rbind(
      row(dgp, 50, 0.90, rep, "tcsp_mc", width = 2),
      row(dgp, 50, 0.90, rep, "tcsp_mti_ecm_map_mc",
          infeasible = TRUE, success = FALSE, width = NA_real_),
      row(dgp, 50, 0.90, rep, "young_mathew", width = 1.95),
      row(dgp, 50, 0.90, rep, "wilks_minmax", width = 2.5),
      row(dgp, 50, 0.90, rep, "tcsp_dkw",
          infeasible = TRUE, success = FALSE, width = NA_real_)
    )
  }))
}))
primary <- rbind(primary, transform(primary, posterior_confidence = 0.99))
young <- rbind(young, transform(young, posterior_confidence = 0.99))
small <- rbind(small, transform(small, posterior_confidence = 0.99))

primary_path <- file.path(work_dir, "primary.csv")
young_path <- file.path(work_dir, "young.csv")
small_path <- file.path(work_dir, "small.csv")
scan_path <- file.path(work_dir, "scan.csv")
utils::write.csv(primary, primary_path, row.names = FALSE)
utils::write.csv(young, young_path, row.names = FALSE)
utils::write.csv(small, small_path, row.names = FALSE)
utils::write.csv(data.frame(
  n = c(500L, 1000L),
  guaranteed_content = c(0.90, 0.90),
  tolerance_confidence = c(0.95, 0.95),
  retained_count = c(470L, 940L),
  content_buffer = c(0.04, 0.04),
  certified_lower_probability = c(0.951, 0.952),
  infeasible = c(FALSE, FALSE),
  stringsAsFactors = FALSE
), scan_path, row.names = FALSE)

out_dir <- file.path(work_dir, "out")
status <- system2(
  "Rscript",
  c(
    script,
    paste0("--primary-results=", primary_path),
    paste0("--young-mathew-results=", young_path),
    paste0("--small95-results=", small_path),
    paste0("--scan-calibration-csv=", scan_path),
    paste0("--output-dir=", out_dir)
  ),
  stdout = TRUE,
  stderr = TRUE
)
if (!identical(attr(status, "status"), NULL)) {
  cat(status, sep = "\n")
  stop("Scenario table generator failed.", call. = FALSE)
}

required <- file.path(out_dir, c(
  "tolerance_validation_article_scenario_ranges.csv",
  "tolerance_validation_article_scenario_ranges.tex",
  "tolerance_validation_article_scenario_ranges_n500.tex",
  "tolerance_validation_article_scenario_ranges_n1000.tex",
  "tolerance_validation_article_small_sample_boundary.csv",
  "tolerance_validation_article_small_sample_boundary.tex",
  "tolerance_validation_article_dgp_delivery.csv",
  "tolerance_validation_article_dgp_delivery.tex",
  "tolerance_validation_article_dgp_width_ranges.csv",
  "tolerance_validation_article_dgp_width_ranges.tex",
  "tolerance_validation_article_scan_calibration.csv",
  "tolerance_validation_article_scan_calibration.tex",
  "tolerance_validation_article_small_sample_dgp_delivery.csv",
  "tolerance_validation_article_small_sample_dgp_delivery.tex",
  "tolerance_validation_article_small_sample_dgp_width_ranges.csv",
  "tolerance_validation_article_small_sample_dgp_width_ranges.tex",
  "tolerance_validation_article_scenario_details.csv",
  "tolerance_validation_article_small_sample_scenario_details.csv"
))
stopifnot(all(file.exists(required)))

primary_range <- read.csv(
  file.path(out_dir, "tolerance_validation_article_scenario_ranges.csv"),
  stringsAsFactors = FALSE
)
stopifnot(nrow(primary_range) ==
            length(unique(primary_range$n)) *
            length(unique(primary_range$content)) * 3L)
stopifnot(!"hdp_s_mc" %in% primary_range$method_id)
stopifnot(!"tcsp_mti_gibbs_median_mc" %in% primary_range$method_id)
stopifnot(!"tcsp_mti_ecm_map_mc" %in% primary_range$method_id)
stopifnot(!"tcsp_dkw" %in% primary_range$method_id)
stopifnot(any(primary_range$method_id == "young_mathew"))
stopifnot(!"dgp_cells" %in% names(primary_range))
stopifnot(!"fail_closed_dgp_cells" %in% names(primary_range))
stopifnot(!"partial_infeasible_dgp_cells" %in% names(primary_range))
stopifnot(!"width_q025" %in% names(primary_range))
stopifnot(!"width_q975" %in% names(primary_range))
stopifnot(!"median_width" %in% names(primary_range))
stopifnot(!"median_width_ratio_to_tcsp" %in% names(primary_range))
stopifnot(!"median_elapsed_sec" %in% names(primary_range))

primary_widths <- read.csv(
  file.path(out_dir, "tolerance_validation_article_dgp_width_ranges.csv"),
  stringsAsFactors = FALSE
)
stopifnot(all(c("dgp_id", "dgp", "n", "content", "method_id",
                "replications", "mean_width", "width_q025", "width_q975") %in%
                names(primary_widths)))
stopifnot(all(primary_widths$replications == 2L))
stopifnot(all(is.finite(primary_widths$mean_width)))
stopifnot(any(primary_widths$method_id == "young_mathew"))
stopifnot(!any(primary_widths$method_id == "tcsp_dkw"))

small_boundary <- read.csv(
  file.path(out_dir, "tolerance_validation_article_small_sample_boundary.csv"),
  stringsAsFactors = FALSE
)
stopifnot(all(small_boundary$n %in% c(50L, 100L)))
stopifnot(!"tcsp_mti_ecm_map_mc" %in% small_boundary$method_id)
stopifnot(!"tcsp_dkw" %in% small_boundary$method_id)
stopifnot(any(small_boundary$method_id == "wilks_minmax"))

tex <- paste(readLines(
  file.path(out_dir, "tolerance_validation_article_scenario_ranges.tex"),
  warn = FALSE
), collapse = "\n")
stopifnot(grepl("Delivery success range", tex, fixed = TRUE))
stopifnot(grepl("\\begin{tabular}{@{}l@{\\hspace{0.75em}}l",
                tex, fixed = TRUE))
stopifnot(!grepl("\\begin{tabularx}", tex, fixed = TRUE))
stopifnot(!grepl("Returned-success range", tex, fixed = TRUE))
stopifnot(grepl("Young--Mathew", tex, fixed = TRUE))
stopifnot(!grepl("Width 95\\% range", tex, fixed = TRUE))
stopifnot(!grepl("Median sec", tex, fixed = TRUE))
stopifnot(!grepl("DGPs", tex, fixed = TRUE))
stopifnot(!grepl("Fail-closed", tex, fixed = TRUE))
stopifnot(!grepl("Width/TCSP", tex, fixed = TRUE))
stopifnot(!grepl("MTI ECM", tex, fixed = TRUE))
stopifnot(!grepl("DKW", tex, fixed = TRUE))
stopifnot(!grepl("posterior predictive", tex, fixed = TRUE))
stopifnot(!grepl("MTI Gibbs", tex, fixed = TRUE))

supp_tex <- paste(readLines(
  file.path(out_dir, "tolerance_validation_article_dgp_delivery.tex"),
  warn = FALSE
), collapse = "\n")
stopifnot(grepl("Student t3", supp_tex, fixed = TRUE))
stopifnot(!grepl("Hybrid DP--scan", supp_tex, fixed = TRUE))
stopifnot(!grepl("MTI ECM", supp_tex, fixed = TRUE))
stopifnot(!grepl("DKW", supp_tex, fixed = TRUE))

width_tex <- paste(readLines(
  file.path(out_dir, "tolerance_validation_article_dgp_width_ranges.tex"),
  warn = FALSE
), collapse = "\n")
stopifnot(grepl("Width 95\\% range", width_tex, fixed = TRUE))
stopifnot(grepl("summarized within, rather than pooled across, distributions",
                width_tex, fixed = TRUE))
stopifnot(!grepl("Returned (\\%)", width_tex, fixed = TRUE))

scan_tex <- paste(readLines(
  file.path(out_dir, "tolerance_validation_article_scan_calibration.tex"),
  warn = FALSE
), collapse = "\n")
stopifnot(grepl("Retained count", scan_tex, fixed = TRUE))
stopifnot(grepl("0.951", scan_tex, fixed = TRUE))

out_dir_no_small <- file.path(work_dir, "out-no-small")
status <- system2(
  "Rscript",
  c(
    script,
    paste0("--primary-results=", primary_path),
    paste0("--young-mathew-results=", young_path),
    paste0("--small95-results=", file.path(work_dir, "missing-small.csv")),
    paste0("--scan-calibration-csv=", scan_path),
    paste0("--output-dir=", out_dir_no_small)
  ),
  stdout = TRUE,
  stderr = TRUE
)
if (!identical(attr(status, "status"), NULL)) {
  cat(status, sep = "\n")
  stop("Scenario table generator failed without small-sample input.",
       call. = FALSE)
}
stopifnot(file.exists(file.path(
  out_dir_no_small, "tolerance_validation_article_scenario_ranges.csv"
)))
stopifnot(!file.exists(file.path(
  out_dir_no_small, "tolerance_validation_article_small_sample_boundary.csv"
)))
stopifnot(!file.exists(file.path(
  out_dir_no_small,
  "tolerance_validation_article_small_sample_dgp_width_ranges.tex"
)))

cat("Scenario-aware tolerance validation table test passed.\n")
