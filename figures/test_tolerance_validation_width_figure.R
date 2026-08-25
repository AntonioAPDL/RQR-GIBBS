#!/usr/bin/env Rscript

script <- normalizePath("figures/generate_tolerance_validation_width_figure.R",
                        winslash = "/", mustWork = TRUE)
work_dir <- tempfile("tolerance-width-figure-test-")
dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
on.exit(unlink(work_dir, recursive = TRUE, force = TRUE), add = TRUE)

rows <- expand.grid(
  dgp = c("Gaussian", "Student t3"),
  n = c(50L, 500L),
  content = c(0.90, 0.99),
  method_id = c(
    "tcsp_mc",
    "mti_ecm_adaptive_cell",
    "young_mathew",
    "wilks_minmax"
  ),
  stringsAsFactors = FALSE
)
rows$width_q025 <- seq_len(nrow(rows)) / 10 + 1
rows$width_q975 <- rows$width_q025 + 0.5
rows$mean_width <- rows$width_q025 + 0.25
rows$dgp_id <- ifelse(rows$dgp == "Gaussian", "normal", "student_t3")
rows$method <- c(
  tcsp_mc = "TCSP",
  mti_ecm_adaptive_cell = "MTI-ECM",
  young_mathew = "Young--Mathew",
  wilks_minmax = "Wilks"
)[rows$method_id]
rows$replications <- 2L
rows$delivery_success <- 0.95
rows$returned_success <- 0.95

width_csv <- file.path(work_dir, "widths.csv")
utils::write.csv(rows, width_csv, row.names = FALSE)
out_dir <- file.path(work_dir, "figures")

status <- system2(
  "Rscript",
  c(script, paste0("--width-csv=", width_csv),
    paste0("--output-dir=", out_dir)),
  stdout = TRUE,
  stderr = TRUE
)
if (!identical(attr(status, "status"), NULL)) {
  cat(status, sep = "\n")
  stop("Tolerance validation width figure generator failed.", call. = FALSE)
}

png_path <- file.path(out_dir, "fig05_tolerance_validation_width_ranges.png")
manifest_path <- file.path(
  out_dir, "tolerance_validation_width_figure_manifest.csv"
)
stopifnot(file.exists(png_path), file.exists(manifest_path))
stopifnot(file.info(png_path)$size > 10000)

manifest <- utils::read.csv(manifest_path, stringsAsFactors = FALSE)
stopifnot(any(grepl("fig05_tolerance_validation_width_ranges.png",
                    manifest$relative_path, fixed = TRUE)))
stopifnot(any(grepl("widths.csv", manifest$relative_path, fixed = TRUE)))

cat("Tolerance validation width figure test passed.\n")
