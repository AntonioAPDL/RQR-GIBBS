#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
parse_arg <- function(name, default = NULL) {
  prefix <- paste0("--", name, "=")
  value <- grep(paste0("^", prefix), args, value = TRUE)
  if (!length(value)) return(default)
  sub(prefix, "", value[[length(value)]], fixed = TRUE)
}
if (any(args %in% c("-h", "--help"))) {
  cat(paste(
    "Usage: 49_healthcheck_rqr_dlm_skewed_candidates.R",
    "  --output-root=<candidate output directory>",
    "  --control-root=<launcher control directory>",
    sep = "\n"
  ), "\n")
  quit(save = "no", status = 0L, runLast = FALSE)
}

output_root <- parse_arg("output-root", "")
control_root <- parse_arg("control-root", "")
if (!nzchar(output_root) || !dir.exists(output_root) ||
    !nzchar(control_root) || !dir.exists(control_root)) {
  stop("Existing output and control roots are required.", call. = FALSE)
}
output_root <- normalizePath(output_root, winslash = "/", mustWork = TRUE)
control_root <- normalizePath(control_root, winslash = "/", mustWork = TRUE)

jobs_path <- file.path(output_root, "jobs.csv")
if (!file.exists(jobs_path)) {
  stop("The candidate job plan is missing.", call. = FALSE)
}
jobs <- utils::read.csv(
  jobs_path, stringsAsFactors = FALSE, check.names = FALSE
)
result_paths <- file.path(
  output_root, "job_results", paste0(jobs$job_id, ".rds")
)
published <- file.exists(result_paths)
read_status <- lapply(result_paths[published], function(path) {
  tryCatch({
    value <- readRDS(path)
    if (isTRUE(value$ok)) "succeeded" else "failed"
  }, error = function(error) "unreadable")
})
read_status <- unlist(read_status, use.names = FALSE)

pid_path <- file.path(control_root, "runner.pid")
pid <- if (file.exists(pid_path)) {
  suppressWarnings(as.integer(readLines(pid_path, warn = FALSE)[[1L]]))
} else {
  NA_integer_
}
process_live <- is.finite(pid) && dir.exists(file.path("/proc", pid))

comparison_manifest_path <- file.path(
  output_root, "comparison_manifest.json"
)
comparison_complete <- file.exists(comparison_manifest_path)
all_methods_selected <- NA
if (comparison_complete) {
  manifest <- jsonlite::read_json(
    comparison_manifest_path, simplifyVector = TRUE
  )
  all_methods_selected <- isTRUE(manifest$all_methods_selected)
}

status <- data.frame(
  item = c(
    "runner_process_live", "comparison_manifest_present",
    "planned_jobs", "published_job_results", "succeeded_jobs",
    "failed_jobs", "unreadable_jobs", "remaining_jobs",
    "progress_percent", "all_methods_selected"
  ),
  value = c(
    process_live, comparison_complete, nrow(jobs), sum(published),
    sum(read_status == "succeeded"), sum(read_status == "failed"),
    sum(read_status == "unreadable"), sum(!published),
    sprintf("%.1f", 100 * sum(published) / nrow(jobs)),
    if (is.na(all_methods_selected)) "pending" else all_methods_selected
  ),
  stringsAsFactors = FALSE
)
print(status, row.names = FALSE, right = FALSE)

by_method <- data.frame(
  method = sort(unique(jobs$method), method = "radix"),
  planned = 0L, published = 0L, remaining = 0L,
  stringsAsFactors = FALSE
)
for (index in seq_len(nrow(by_method))) {
  selected <- jobs$method == by_method$method[[index]]
  by_method$planned[[index]] <- sum(selected)
  by_method$published[[index]] <- sum(published[selected])
  by_method$remaining[[index]] <- sum(!published[selected])
}
cat("\nProgress by method:\n")
print(by_method, row.names = FALSE, right = FALSE)

stderr_path <- file.path(control_root, "runner.stderr.log")
if (!process_live && !comparison_complete && file.exists(stderr_path)) {
  lines <- readLines(stderr_path, warn = FALSE)
  if (length(lines)) {
    cat("\nLast runner stderr lines:\n")
    cat(tail(lines, 20L), sep = "\n")
    cat("\n")
  }
}
