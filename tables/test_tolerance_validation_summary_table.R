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
stopifnot(nrow(tab) == 6L)
stopifnot(all(c("tcsp_mc", "hdp_s_mc", "tcsp_mti_ecm_map_mc",
                "young_mathew", "wilks_minmax") %in%
                tab$method_id))
stopifnot(!"tcsp_mti_gibbs_median_mc" %in% tab$method_id)
stopifnot(all(is.finite(tab$grid_delivery_success)))
stopifnot(all(is.finite(tab$returned_success)))
stopifnot(all(is.finite(tab$median_width_ratio_to_tcsp)))

tex <- paste(readLines(tex_path, warn = FALSE), collapse = "\n")
stopifnot(grepl("\\\\begin\\{tabularx\\}", tex))
stopifnot(grepl("Young--Mathew", tex, fixed = TRUE))
stopifnot(grepl("Returned success", tex, fixed = TRUE))
stopifnot(!grepl("Feasible success", tex, fixed = TRUE))
stopifnot(!grepl("posterior predictive", tex, fixed = TRUE))

cat("Tolerance validation table test passed.\n")
