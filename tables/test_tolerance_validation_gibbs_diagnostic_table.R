#!/usr/bin/env Rscript

script <- normalizePath(
  "tables/generate_tolerance_validation_gibbs_diagnostic_table.R",
  winslash = "/", mustWork = TRUE
)
work_dir <- tempfile("tolerance-gibbs-diagnostic-table-test-")
dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
on.exit(unlink(work_dir, recursive = TRUE, force = TRUE), add = TRUE)

diagnostics <- expand.grid(
  cell_role = c("hard_feasible_large", "small_feasible"),
  dgp_id = c("normal", "student_t3"),
  estimand = c("lower", "upper", "width"),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)
diagnostics$mode_source <- ifelse(
  diagnostics$cell_role == "hard_feasible_large", "main", "followup"
)
diagnostics$n <- ifelse(diagnostics$cell_role == "hard_feasible_large",
                        1000L, 100L)
diagnostics$guaranteed_content <- ifelse(
  diagnostics$cell_role == "hard_feasible_large", 0.99, 0.90
)
diagnostics$tolerance_confidence <- 0.95
diagnostics$replication <- 1L
diagnostics$chains <- 4L
diagnostics$draws_per_chain <- 5000L
diagnostics$rhat <- ifelse(diagnostics$dgp_id == "student_t3", 1.45, 1.02)
diagnostics$ess <- ifelse(diagnostics$dgp_id == "student_t3", 25, 250)

diagnostics_path <- file.path(work_dir, "diagnostics.csv")
utils::write.csv(diagnostics, diagnostics_path, row.names = FALSE)
out_dir <- file.path(work_dir, "out")

status <- system2(
  "Rscript",
  c(script, paste0("--diagnostics-csv=", diagnostics_path),
    paste0("--output-dir=", out_dir)),
  stdout = TRUE,
  stderr = TRUE
)
if (!identical(attr(status, "status"), NULL)) {
  cat(status, sep = "\n")
  stop("MTI Gibbs diagnostic table generator failed.", call. = FALSE)
}

csv_path <- file.path(out_dir, "tolerance_validation_gibbs_diagnostics.csv")
tex_path <- file.path(out_dir, "tolerance_validation_gibbs_diagnostics.tex")
stopifnot(file.exists(csv_path), file.exists(tex_path))

tab <- utils::read.csv(csv_path, stringsAsFactors = FALSE)
stopifnot(nrow(tab) == 4L)
stopifnot(any(tab$status == "Passed"))
stopifnot(any(tab$status == "Diagnostic only"))
stopifnot(all(tab$chains == 4L))
stopifnot(all(tab$draws_per_chain == 5000L))

tex <- paste(readLines(tex_path, warn = FALSE), collapse = "\n")
stopifnot(grepl("Max \\(\\hat R\\)", tex, fixed = TRUE))
stopifnot(grepl("Diagnostic only", tex, fixed = TRUE))
stopifnot(!grepl("posterior predictive", tex, fixed = TRUE))
stopifnot(!grepl("paper-matched", tex, ignore.case = TRUE))

cat("MTI Gibbs diagnostic table test passed.\n")
