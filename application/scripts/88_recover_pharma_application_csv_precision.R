#!/usr/bin/env Rscript

# Recover only benchmark endpoint bits lost by the initial CSV writer; no MTI fit.
args <- commandArgs(TRUE)
for (name in c("pharma_tolerance_application.R", "pharma_mti_application.R")) {
  source(file.path("application/scripts/lib", name))
}
config <- pma_read_config(pta_arg_value(args, "--config=", pma_default_config))
input <- pta_arg_value(args, "--run-dir=", "")
output <- pta_arg_value(args, "--output-dir=", "")
if (!nzchar(input) || !nzchar(output) || file.exists(output)) {
  pta_stop("Provide an existing --run-dir and a fresh --output-dir.")
}
manifest <- pma_verify_run(input, config)
if (as.character(utils::packageVersion("tolerance")) != manifest$package_versions$tolerance) {
  pta_stop("Endpoint recovery requires the original tolerance package version.")
}
results <- pta_read_csv(file.path(input, "results.csv"))
observed <- pta_read_csv(file.path(input, "observed_batches.csv"))
splits <- pma_make_splits(observed, config, "confirmatory")
changes <- list()
for (s in splits) {
  rows <- which(results$split_scheme == s$scheme & results$split_id == s$split_id &
                  results$interval_returned)
  for (i in rows) {
    r <- results[i, ]
    y <- observed[[r$response_id]]
    evaluate <- function(lower, upper) c(mean(y[s$heldout] >= lower & y[s$heldout] <= upper),
      mean(y[s$heldout] < lower), mean(y[s$heldout] > upper))
    stored <- unlist(r[c("heldout_empirical_content", "heldout_lower_omitted", "heldout_upper_omitted")])
    if (max(abs(evaluate(r$lower, r$upper) - stored)) <= 1e-12) next
    if (r$method_id != "young_mathew") pta_stop("Unexpected non-benchmark serialization discrepancy.")
    set.seed(r$fit_seed)
    fit <- pta_fit_young_mathew(y[s$train], r$content, r$tolerance_confidence)
    if (!isTRUE(fit$returned) ||
        max(abs(c(fit$lower-r$lower, fit$upper-r$upper))) > 1e-13 ||
        max(abs(evaluate(fit$lower, fit$upper) - stored)) > 1e-12) {
      pta_stop("Benchmark reproduction does not explain the original empirical evaluation.")
    }
    changes[[length(changes)+1L]] <- data.frame(
      response_id = r$response_id, split_scheme = r$split_scheme, split_id = r$split_id,
      method_id = r$method_id, old_lower = r$lower, recovered_lower = fit$lower,
      old_upper = r$upper, recovered_upper = fit$upper,
      old_width = r$width, recovered_width = fit$upper-fit$lower)
    results[i, c("lower", "upper", "width")] <- c(fit$lower, fit$upper, fit$upper-fit$lower)
  }
}
if (!length(changes)) pta_stop("No endpoint serialization discrepancies were found.")
pma_validate_results(results, splits)
dir.create(output, recursive = TRUE)
if (!all(file.copy(list.files(input, full.names = TRUE), output))) pta_stop("Copy failed.")
pma_write_csv(results, file.path(output, "results.csv"))
pma_write_csv(do.call(rbind, changes), file.path(output, "endpoint_precision_recovery.csv"))
representatives <- pma_representative(results)
records <- lapply(seq_len(nrow(representatives)), function(i) {
  r <- as.list(representatives[i, ])
  s <- Filter(function(x) x$scheme == "random" && x$split_id == r$split_id, splits)[[1L]]
  r$training_indices <- s$train
  r$heldout_indices <- s$heldout
  r$canonical_results_sha256 <- pta_file_sha256(file.path(output, "results.csv"))
  r$split_membership_sha256 <- pta_file_sha256(file.path(output, "split_membership.csv.gz"))
  r
})
pta_atomic_write_json(records, file.path(output, "representative_splits.json"))
pma_write_csv(representatives, file.path(output, "representative_splits.csv"))
pma_write_csv(pma_summary(results), file.path(output, "method_summary.csv"))
pma_write_csv(pma_paired_summary(results), file.path(output, "paired_summary.csv"))
pma_write_csv(pma_diagnostics(results), file.path(output, "numerical_diagnostics.csv"))
pma_write_csv(pma_convergence_sensitivity(results), file.path(output, "convergence_sensitivity.csv"))
manifest$csv_precision_recovery <- list(
  original_run = normalizePath(input),
  original_manifest_sha256 = pta_file_sha256(file.path(input, "manifest.json")),
  original_results_sha256 = pta_file_sha256(file.path(input, "results.csv")),
  changed_rows = length(changes), methods = "young_mathew", mti_fits_repeated = 0L,
  empirical_evaluations_changed = 0L, tolerance_version = manifest$package_versions$tolerance,
  explanation = "Restore original endpoint precision by reproducing only affected deterministic benchmark fits; retain every original empirical evaluation and MTI result.",
  sources = pma_file_record(c("application/scripts/88_recover_pharma_application_csv_precision.R",
                            "application/scripts/lib/pharma_mti_application.R")))
artifacts <- setdiff(list.files(output, full.names = TRUE), file.path(output, "manifest.json"))
manifest$artifacts <- pma_file_record(artifacts)
manifest$artifacts$path <- basename(manifest$artifacts$path)
manifest$derived_tables_and_figures <- list(status = "pending_reporting")
manifest$ecm_convergence_counts <- list(converged = sum(results$mti_ecm_converged, na.rm = TRUE),
  not_converged = sum(!results$mti_ecm_converged, na.rm = TRUE),
  not_applicable = sum(results$method_id != pma_methods[1]),
  missing_mti_flag = sum(results$method_id == pma_methods[1] & is.na(results$mti_ecm_converged)))
pta_atomic_write_json(manifest, file.path(output, "manifest.json"))
invisible(pma_verify_run(output, config))
cat("Recovered", length(changes), "benchmark endpoints; no MTI fits or empirical evaluations changed.\n")
