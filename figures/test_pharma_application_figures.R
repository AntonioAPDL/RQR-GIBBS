#!/usr/bin/env Rscript

script <- normalizePath("figures/generate_pharma_application_figures.R",
                        winslash = "/", mustWork = TRUE)
work_dir <- tempfile("pharma-figure-test-")
dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
on.exit(unlink(work_dir, recursive = TRUE, force = TRUE), add = TRUE)

rows <- expand.grid(
  response_role = c("primary", "supplement"),
  method_id = c("tcsp_mc", "mti_ecm_adaptive_cell",
                "young_mathew", "wilks_minmax"),
  stringsAsFactors = FALSE
)
rows$response_id <- ifelse(rows$response_role == "primary",
                           "fct_tensile", "tbl_rsd_weight")
rows$method <- c(
  tcsp_mc = "TCSP",
  mti_ecm_adaptive_cell = "MTI-ECM",
  young_mathew = "Young--Mathew",
  wilks_minmax = "Wilks"
)[rows$method_id]
rows$splits <- 4L
rows$interval_return_rate <- 1
rows$heldout_content_median <- 0.93
rows$width_median <- c(0.40, 0.36, 0.42, 0.60,
                       0.70, 0.64, 0.74, 1.10)
rows$lower_median <- c(1.0, 1.02, 0.98, 0.90,
                       0.6, 0.65, 0.58, 0.45)
rows$upper_median <- rows$lower_median + rows$width_median
rows$lower_q025 <- rows$lower_median - 0.04
rows$lower_q975 <- rows$lower_median + 0.04
rows$upper_q025 <- rows$upper_median - 0.05
rows$upper_q975 <- rows$upper_median + 0.05

summary_path <- file.path(work_dir, "summary.csv")
utils::write.csv(rows, summary_path, row.names = FALSE)
out_dir <- file.path(work_dir, "figures")
status <- system2(
  "Rscript",
  c(script, paste0("--summary-csv=", summary_path),
    paste0("--output-dir=", out_dir)),
  stdout = TRUE,
  stderr = TRUE
)
if (!identical(attr(status, "status"), NULL)) {
  cat(status, sep = "\n")
  stop("Pharmaceutical application figure generator failed.", call. = FALSE)
}

pngs <- file.path(out_dir, c(
  "fig06_pharma_application_tensile_intervals.png",
  "figS04_pharma_application_rsd_intervals.png"
))
stopifnot(all(file.exists(pngs)))
stopifnot(all(file.info(pngs)$size > 10000))
manifest <- file.path(out_dir, "pharma_application_figure_manifest.csv")
stopifnot(file.exists(manifest))
cat("Pharmaceutical application figure test passed.\n")
