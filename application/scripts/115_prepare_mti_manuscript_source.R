#!/usr/bin/env Rscript
arguments <- commandArgs(FALSE)
script <- sub("--file=", "", arguments[startsWith(arguments, "--file=")][1], fixed = TRUE)
root <- normalizePath(file.path(dirname(script), "..", ".."), mustWork = TRUE)
source(file.path(root, "application/scripts/lib/manuscript_source_package.R"))
args <- commandArgs(TRUE)
msp_require(all(grepl("^--(include-supplement=(true|false)|stamp=[A-Za-z0-9_-]+)$", args)) &&
  !anyDuplicated(sub("=.*", "", args)), "Use --include-supplement=true|false and optional --stamp=NAME.")
include <- "--include-supplement=true" %in% args
stamp <- args[startsWith(args, "--stamp=")]
if (length(stamp)) stamp <- substring(stamp, 9L) else stamp <- Sys.getenv("RQR_ARXIV_STAMP",
  format(Sys.time(), "%Y%m%d_%H%M%S", tz = "America/Los_Angeles"))
result <- msp_prepare(root, include, stamp)
cat("Compiled committed manuscript source without raw chains or simulation caches.\n",
  "Source commit: ", result$source_commit, "\nSource ZIP: ", result$zip,
  "\nReview PDFs and completed-study reference: ", result$directory, "\n", sep = "")
