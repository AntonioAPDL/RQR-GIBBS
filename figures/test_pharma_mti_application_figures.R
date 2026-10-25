#!/usr/bin/env Rscript

for (name in c("pharma_tolerance_application.R", "pharma_mti_application.R", "pharma_mti_reporting.R")) {
  source(file.path("application/scripts/lib", name))
}
config <- pma_read_config("application/config/pharma_tolerance_application_profiled_20261025.json")
results <- file.path(config$paths$canonical_dir,"results.csv")
inputs <- pma_read_report_inputs(config,results)
output <- tempfile("pharma-mti-figure-test-")
dir.create(output)
on.exit(unlink(output, recursive = TRUE), add = TRUE)
status <- system2("Rscript", c("figures/generate_pharma_mti_application_figures.R",
                               paste0("--config=",attr(config,"config_path")),
                               paste0("--results-csv=",results),
                               paste0("--output-dir=", output)), stdout = TRUE, stderr = TRUE)
if (!is.null(attr(status,"status"))) stop(paste(status, collapse = "\n"))
manifest <- read.csv(file.path(output,"pharma_application_figure_manifest.csv"))
stopifnot(nrow(manifest) == 3L,
  all(manifest$method_ids == paste(pma_methods, collapse = ";")),
  all(manifest$source_results_sha256 == pta_file_sha256(inputs$results_path)))
for (i in seq_len(nrow(manifest))) {
  path <- file.path(output, basename(manifest$relative_path[i]))
  stopifnot(file.exists(path), file.info(path)$size > 30000,
            pta_file_sha256(path) == manifest$sha256[i])
  committed <- file.path("figures/generated", basename(path))
  if (file.exists(committed)) stopifnot(pta_file_sha256(path) == pta_file_sha256(committed))
}
plotted <- read.csv(file.path(output,"pharma_application_plotted_intervals.csv"))
stopifnot(nrow(plotted) == 6L, !any(plotted$method_id == "tcsp_mc"))
for (i in seq_len(nrow(plotted))) {
  z <- plotted[i, ]
  rep <- inputs$representatives$split_id[inputs$representatives$response_id == z$response_id]
  actual <- inputs$results[inputs$results$response_id == z$response_id & inputs$results$split_scheme == "random" &
                            inputs$results$split_id == rep & inputs$results$method_id == z$method_id, ]
  stopifnot(z$split_id == rep, z$lower == actual$lower, z$upper == actual$upper,
            z$width == actual$width, z$method_role == actual$method_role)
  record <- manifest[manifest$response == z$response_id, ]
  stopifnot(record$representative_split == z$split_id)
}
stopifnot(all(manifest$plotted_intervals_sha256 == pta_file_sha256(file.path(output,"pharma_application_plotted_intervals.csv"))))
cat("Pharmaceutical MTI figures, hashes, methods, and representative fitted intervals verified.\n")
