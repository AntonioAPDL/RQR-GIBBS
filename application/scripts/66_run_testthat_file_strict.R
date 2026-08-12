#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) {
  stop("Usage: 66_run_testthat_file_strict.R <test-file>", call. = FALSE)
}

path <- args[[1L]]
if (!file.exists(path)) stop("Test file does not exist: ", path, call. = FALSE)

suppressPackageStartupMessages(library(rqrgibbs))

results <- testthat::test_file(path, reporter = "summary")
failed <- unlist(lapply(results, function(test) {
  vapply(test$results, function(expectation) {
    inherits(expectation, "expectation_failure") ||
      inherits(expectation, "expectation_error")
  }, logical(1L))
}), use.names = FALSE)

if (any(failed)) {
  quit(status = 1L, save = "no")
}
