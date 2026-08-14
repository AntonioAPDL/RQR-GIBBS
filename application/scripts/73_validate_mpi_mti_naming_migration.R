#!/usr/bin/env Rscript

`%||%` <- function(x, y) if (is.null(x)) y else x

args <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_path <- sub(file_arg, "", args[startsWith(args, file_arg)][1L])
if (!length(script_path) || is.na(script_path)) {
  script_path <- "application/scripts/73_validate_mpi_mti_naming_migration.R"
}
repo_root <- normalizePath(
  file.path(dirname(normalizePath(script_path, mustWork = TRUE)), "..", ".."),
  winslash = "/", mustWork = TRUE
)
setwd(repo_root)

read_lines <- function(path) {
  readLines(path, warn = FALSE, encoding = "UTF-8")
}

line_hits <- function(path, pattern, ignore = character()) {
  x <- read_lines(path)
  hit <- grepl(pattern, x, perl = TRUE)
  for (pat in ignore) {
    hit <- hit & !grepl(pat, x, perl = TRUE)
  }
  if (!any(hit)) {
    return(data.frame())
  }
  data.frame(
    file = path,
    line = which(hit),
    text = x[hit],
    stringsAsFactors = FALSE
  )
}

failures <- list()

active_old_terms <- paste(
  c(
    "ordinary RQR", "Ordinary RQR", "mean-tilted RQR", "Mean-tilted RQR",
    "MT-RQR", "TCSP-MT-RQR", "RQR-DESN", "RQR-DLM", "RQR-FD",
    "RQR loss", "RQR target", "RQR generalized", "RQR latent",
    "RQR root", "RQR roots", "RQR oracle", "RQR risk", "RQR article",
    "RQR 0\\.00"
  ),
  collapse = "|"
)

manuscript_allow <- c(
  "Relaxed Quantile Regression \\(RQR\\)",
  "RQR-W",
  "RQR-O",
  "ordinary response",
  "ordinary unknown",
  "ordinary likelihood",
  "ordinary parameter"
)
for (path in c("main.tex", "rqr-gibbs-supplement.tex")) {
  failures[[length(failures) + 1L]] <- line_hits(
    path, active_old_terms, ignore = manuscript_allow
  )
}

failures[[length(failures) + 1L]] <- line_hits(
  "README.md",
  paste(c("ordinary RQR", "mean-tilted RQR", "MT-RQR", "TCSP-MT-RQR"), collapse = "|")
)

for (path in c(
  "application/config/rqr_bayes_uq_validation_main_20260813.json",
  "docs/implementation_notes/rqr_bayes_uq_main_launch_plan_20260813.md",
  "docs/implementation_notes/rqr_bayes_uq_mtrqr_gibbs_ecm_corrected_launch_plan_20260813.md",
  "figures/README.md"
)) {
  failures[[length(failures) + 1L]] <- line_hits(
    path,
    paste(c("ordinary RQR", "Ordinary RQR", "mean-tilted RQR", "Mean-tilted RQR",
            "MT-RQR", "TCSP-MT-RQR", "RQR article", "RQR 0\\.00"), collapse = "|")
  )
}

launch_files <- c(
  "application/scripts/69_validate_rqr_bayes_uq.R",
  "application/scripts/71_manage_rqr_bayes_uq_main_waves.R"
)
for (path in launch_files) {
  hits <- line_hits(
    path,
    paste(c("tcsp_mtrqr", "mtrqr_gibbs", "mtrqr_ecm", "mtrqr_"), collapse = "|")
  )
  if (nrow(hits)) {
    hits <- hits[!(hits$line <= 80L), , drop = FALSE]
  }
  failures[[length(failures) + 1L]] <- hits
}

required_patterns <- list(
  "main.tex" = c(
    "Mean-Preserving Interval Loss",
    "Mean-Tilted Interval Family",
    "Scan-Calibrated Tolerance Actions"
  ),
  "rqr-gibbs-supplement.tex" = c(
    "MPI Loss",
    "Mean-Tilted Interval",
    "RQR-W and Width Regularization"
  ),
  "application/config/rqr_bayes_uq_validation_main_20260813.json" = c(
    "tcsp_mti_gibbs_median_mc",
    "tcsp_mti_ecm_map_mc",
    "fixed_target_mti_plugin"
  ),
  "application/R/mti_api_compatibility.R" = c(
    "mpi_loss",
    "mti_mcmc_fit",
    "tcsp_hybrid_bayes_fit"
  ),
  "docs/migrations/MPI_MTI_NAMING_MIGRATION.md" = c(
    "Mean-Preserving Interval",
    "Mean-Tilted Interval",
    "compatibility"
  )
)

for (path in names(required_patterns)) {
  x <- paste(read_lines(path), collapse = "\n")
  present <- vapply(
    required_patterns[[path]], grepl, logical(1L),
    x = x, perl = TRUE, USE.NAMES = FALSE
  )
  missing <- required_patterns[[path]][!present]
  if (length(missing)) {
    failures[[length(failures) + 1L]] <- data.frame(
      file = path,
      line = NA_integer_,
      text = paste("Missing required pattern:", paste(missing, collapse = "; ")),
      stringsAsFactors = FALSE
    )
  }
}

failures <- do.call(rbind, failures)
if (nrow(failures)) {
  message("MPI/MTI naming migration check failed:")
  for (ii in seq_len(nrow(failures))) {
    message(sprintf("%s:%s: %s", failures$file[ii], failures$line[ii], failures$text[ii]))
  }
  quit(status = 1L)
}

message("PASS: MPI/MTI naming migration check completed.")
