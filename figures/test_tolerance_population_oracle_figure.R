#!/usr/bin/env Rscript

script <- normalizePath("figures/generate_tolerance_population_oracle_figure.R",
                        winslash = "/", mustWork = TRUE)
work_dir <- tempfile("tolerance-population-oracle-figure-test-")
dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
on.exit(unlink(work_dir, recursive = TRUE, force = TRUE), add = TRUE)

rows <- expand.grid(
  dgp_id = c("normal", "student_t3"),
  content = c(0.90, 0.99),
  stringsAsFactors = FALSE
)
rows$dgp_label <- ifelse(rows$dgp_id == "normal", "Gaussian", "Student t3")
rows$lower_probability <- (1 - rows$content) / 2
rows$upper_probability <- rows$lower_probability + rows$content
rows$lower <- -seq_len(nrow(rows)) / 10 - rows$content
rows$upper <- seq_len(nrow(rows)) / 10 + rows$content
rows$width <- rows$upper - rows$lower
rows$mean_tilt <- 0
rows$status <- "ok"

oracle_csv <- file.path(work_dir, "population_shortest.csv")
utils::write.csv(rows, oracle_csv, row.names = FALSE)
out_dir <- file.path(work_dir, "figures")

status <- system2(
  "Rscript",
  c(script, paste0("--oracle-width-csv=", oracle_csv),
    paste0("--output-dir=", out_dir)),
  stdout = TRUE,
  stderr = TRUE
)
if (!identical(attr(status, "status"), NULL)) {
  cat(status, sep = "\n")
  stop("Population shortest-content figure generator failed.", call. = FALSE)
}

png_path <- file.path(out_dir, "figS03_tolerance_population_shortest_intervals.png")
manifest_path <- file.path(
  out_dir, "tolerance_population_shortest_figure_manifest.csv"
)
stopifnot(file.exists(png_path), file.exists(manifest_path))
stopifnot(file.info(png_path)$size > 10000)

manifest <- utils::read.csv(manifest_path, stringsAsFactors = FALSE)
stopifnot(any(grepl("figS03_tolerance_population_shortest_intervals.png",
                    manifest$relative_path, fixed = TRUE)))
stopifnot(any(grepl("population_shortest.csv", manifest$relative_path,
                    fixed = TRUE)))

cat("Population shortest-content figure test passed.\n")
