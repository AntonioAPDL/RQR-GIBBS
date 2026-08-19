#!/usr/bin/env Rscript

script <- normalizePath("tables/generate_tolerance_validation_summary_table.R",
                        winslash = "/", mustWork = TRUE)
out_dir <- tempfile("tolerance-validation-table-")
on.exit(unlink(out_dir, recursive = TRUE, force = TRUE), add = TRUE)

status <- system2("Rscript", c(script, paste0("--output-dir=", out_dir)),
                  stdout = TRUE, stderr = TRUE)
if (!identical(attr(status, "status"), NULL)) {
  cat(status, sep = "\n")
  stop("Tolerance validation table generator failed.", call. = FALSE)
}

csv_path <- file.path(out_dir, "tolerance_validation_main_summary.csv")
tex_path <- file.path(out_dir, "tolerance_validation_main_summary.tex")
stopifnot(file.exists(csv_path), file.exists(tex_path))

tab <- read.csv(csv_path, stringsAsFactors = FALSE)
expected_methods <- c("tcsp_mc", "hdp_s_mc", "young_mathew", "wilks_minmax")
stopifnot(nrow(tab) == length(expected_methods))
stopifnot(identical(tab$method_id, expected_methods))

required_columns <- c(
  "returned_interval_rate",
  "content_success_returned",
  "tolerance_delivery",
  "width_q025",
  "width_median",
  "width_q975",
  "width_median_q025_q975",
  "median_elapsed_sec"
)
stopifnot(all(required_columns %in% names(tab)))
stopifnot(all(is.finite(tab$returned_interval_rate)))
stopifnot(all(is.finite(tab$content_success_returned)))
stopifnot(all(is.finite(tab$tolerance_delivery)))
stopifnot(all(is.finite(tab$width_q025)))
stopifnot(all(is.finite(tab$width_median)))
stopifnot(all(is.finite(tab$width_q975)))
stopifnot(!any(grepl("tcsp_mti|split|dkw|bb|oracle", tab$method_id)))

tex <- paste(readLines(tex_path, warn = FALSE), collapse = "\n")
stopifnot(grepl("\\\\begin\\{tabularx\\}", tex))
stopifnot(grepl("Young--Mathew", tex, fixed = TRUE))
stopifnot(grepl("Returned", tex, fixed = TRUE))
stopifnot(grepl("Content success", tex, fixed = TRUE))
stopifnot(grepl("Delivery", tex, fixed = TRUE))
stopifnot(grepl("Width", tex, fixed = TRUE))
stopifnot(!grepl("Width/TCSP", tex, fixed = TRUE))
stopifnot(!grepl("posterior predictive", tex, fixed = TRUE))

cat("Tolerance validation table test passed.\n")
