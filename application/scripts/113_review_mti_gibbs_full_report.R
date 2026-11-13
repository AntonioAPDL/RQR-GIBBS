#!/usr/bin/env Rscript
arguments <- commandArgs(FALSE)
script <- sub("--file=", "", arguments[startsWith(arguments, "--file=")][1], fixed = TRUE)
setwd(normalizePath(file.path(dirname(script), "..", ".."), mustWork = TRUE))
source("application/scripts/lib/mti_gibbs_full_report_review.R")
args <- commandArgs(TRUE)
mgfa_require(all(startsWith(args, "--output-dir=") | startsWith(args, "--expect-commit=") |
  args == "--require-complete"), "Unknown report-review argument.")
value <- function(prefix) {
  found <- args[startsWith(args, prefix)]
  mgfa_require(length(found) <= 1L, paste("Repeated argument:", prefix))
  if (length(found) == 1L) substring(found, nchar(prefix) + 1L) else NULL
}
directory <- value("--output-dir=")
mgfa_require(!is.null(directory) && nzchar(directory), "Provide --output-dir=/absolute/campaign/directory.")
result <- mgfa_review(directory, value("--expect-commit="))
result$reviewer <- list(script_sha256 = mgfa_hash("application/scripts/113_review_mti_gibbs_full_report.R"),
  helper_sha256 = mgfa_hash("application/scripts/lib/mti_gibbs_full_report_review.R"),
  r_version = R.version.string)
cat(jsonlite::toJSON(result, pretty = TRUE, auto_unbox = TRUE, digits = NA, null = "null"), "\n")
if ("--require-complete" %in% args && result$status == "pending") quit(status = 2L)
