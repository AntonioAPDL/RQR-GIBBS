#!/usr/bin/env Rscript

script <- normalizePath("figures/generate_tolerance_validation_primary_figure.R",
                        winslash = "/", mustWork = TRUE)
work_dir <- tempfile("tolerance-primary-figure-test-")
dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
on.exit(unlink(work_dir, recursive = TRUE, force = TRUE), add = TRUE)

methods <- c(
  "tcsp_mc",
  "tcsp_mti_ecm_map_mc",
  "young_mathew",
  "wilks_minmax",
  "tcsp_dkw"
)
stratified <- expand.grid(
  n = c(500L, 1000L),
  content = c(0.90, 0.95, 0.99),
  method_id = methods,
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)
stratified$method <- stratified$method_id
stratified$infeasible_rate <- 0
stratified$delivery_success <- 0.96
stratified$returned_success <- 0.96
stratified$median_width_ratio_to_tcsp <- 1
stratified$median_elapsed_sec <- 0.01
stratified$delivery_success[stratified$method_id == "wilks_minmax"] <- 0.99
stratified$median_width_ratio_to_tcsp[
  stratified$method_id == "wilks_minmax"
] <- 1.45
stratified$delivery_success[stratified$method_id == "tcsp_dkw"] <- 0.50
stratified$median_width_ratio_to_tcsp[
  stratified$method_id == "young_mathew"
] <- 0.98

csv_path <- file.path(work_dir, "stratified.csv")
utils::write.csv(stratified, csv_path, row.names = FALSE)
out_dir <- file.path(work_dir, "figures")

status <- system2(
  "Rscript",
  c(script, paste0("--stratified-csv=", csv_path),
    paste0("--output-dir=", out_dir)),
  stdout = TRUE,
  stderr = TRUE
)
if (!identical(attr(status, "status"), NULL)) {
  cat(status, sep = "\n")
  stop("Tolerance validation primary figure generator failed.",
       call. = FALSE)
}

png_path <- file.path(out_dir, "fig04_tolerance_validation_primary.png")
manifest_path <- file.path(
  out_dir, "tolerance_validation_primary_figure_manifest.csv"
)
stopifnot(file.exists(png_path), file.exists(manifest_path))
stopifnot(file.info(png_path)$size > 10000)

manifest <- utils::read.csv(manifest_path, stringsAsFactors = FALSE)
stopifnot(any(grepl("fig04_tolerance_validation_primary.png",
                    manifest$relative_path, fixed = TRUE)))
stopifnot(any(grepl("stratified.csv", manifest$relative_path, fixed = TRUE)))

cat("Tolerance validation primary figure test passed.\n")
