root <- normalizePath(if (file.exists("application/scripts/lib/mti_gibbs_full_report_review.R")) "." else "../../..")
source(file.path(root, "application/scripts/lib/mti_gibbs_full_report_review.R"))

report_fixture <- function(unresolved = FALSE) {
  frozen <- data.frame(dataset_id = 1:2, case_id = c("sim_1_1", "app_1"),
    source = c("simulation", "application"), response_id = c("normal", "response"),
    n = 50L, content = .9, tolerance_confidence = .95, replication = 1L,
    dataset_sha256 = c("sample1", "sample2"), q = .98, delta = .01,
    ecm_lower = c(-1, 0), ecm_upper = c(3, 4), split_scheme = c("", "random"),
    split_id = c(NA_integer_, 1L))
  jobs <- frozen[rep(1:2, each = 4L), ]; jobs$chain <- rep(1:4, 2L)
  jobs$job_id <- seq_len(nrow(jobs)); jobs$seed <- jobs$job_id + 100L
  d <- frozen; d$mean_lower <- c(-1, 0); d$mean_upper <- c(3, 4); d$mean_width <- 4
  d$mean_content <- c(.96, .94); d$mean_attained <- TRUE
  d$median_lower <- c(-.8, .2); d$median_upper <- c(2.8, 3.8); d$median_interval_width <- 3.6
  d$posterior_width_q025 <- 2.5; d$posterior_width_median <- 3.8; d$posterior_width_q975 <- 6
  d$extended <- c(FALSE, TRUE); d$retained_per_chain <- c(4000L, 8000L)
  d$screen_pass <- d$preferred_pass <- c(TRUE, !unresolved)
  d$midpoint_screen_pass <- d$all_primary_precision <- TRUE
  d$maximum_rhat <- c(1.002, if (unresolved) 1.2 else 1.002)
  d$minimum_bulk_ess <- 600; d$minimum_tail_ess <- 700
  p <- data.frame(dataset_id = rep(1:2, each = 4L), variable = rep(c("lower", "upper", "width", "midpoint"), 2L),
    q = .98, delta = .01, mean = c(-1, 3, 4, 1, 0, 4, 4, 2),
    median = c(-.8, 2.8, 3.8, 1, .2, 3.8, 3.8, 2), rhat = 1.002,
    ess_bulk = 600, ess_tail = 700, mcse_sd_ratio = .02, screen_pass = TRUE,
    preferred_pass = TRUE, mean_precision = TRUE, median_precision = TRUE,
    interval_precision = TRUE, lower_025 = 2.5, upper_975 = 6)
  if (unresolved) { p$rhat[6] <- 1.2; p$screen_pass[6] <- p$preferred_pass[6] <- FALSE }
  intervals <- do.call(rbind, lapply(1:2, function(i) {
    z <- frozen[rep(i, 4L), c("dataset_id", "case_id", "source", "response_id", "n", "content",
      "tolerance_confidence", "replication", "split_scheme")]
    z$method <- c("mti_gibbs_mean", "mti_ecm_adaptive_cell", "young_mathew",
      if (i == 1) "wilks" else "wilks_minmax")
    z$lower <- d$mean_lower[i]; z$upper <- d$mean_upper[i] + c(0, .1, .2, .3)
    z$width <- z$upper - z$lower; z$observed_content <- d$mean_content[i]
    z$attained <- z$available <- TRUE; z
  }))
  paired <- d[1, c("dataset_id", "case_id", "response_id", "n", "content", "replication",
    "mean_width", "mean_content", "mean_attained", "screen_pass", "preferred_pass")]
  for (method in c("mti_ecm_adaptive_cell", "young_mathew", "wilks")) {
    z <- intervals[intervals$source == "simulation" & intervals$method == method, ]
    paired[[paste0(method, "_width")]] <- z$width
    paired[[paste0(method, "_attained")]] <- z$attained
    paired[[paste0("width_difference_", method)]] <- paired$mean_width - z$width
  }
  final <- list(datasets = 2L, simulation_datasets = 1L, application_datasets = 1L,
    preferred_datasets = sum(d$preferred_pass), practical_datasets = sum(d$screen_pass),
    unresolved_datasets = sum(!d$screen_pass), all_primary_precision_datasets = 2L,
    extended_datasets = 1L, maximum_rhat = max(p$rhat), minimum_bulk_ess = 600,
    minimum_tail_ess = 700, new_tolerance_guarantee = FALSE, article_replaced = FALSE)
  list(manifest = list(identity = list(commit = strrep("a", 40),
    design_sha256 = digest::digest(jobs, algo = "sha256")),
    expected_chains = 8L, expected_datasets = 2L,
    configuration = list(expected_simulation_datasets = 1L, expected_application_datasets = 1L,
      extension_draws = 4000L, practical_config = "application/config/policy.json",
      target_selection = "hold_archived_ecm_selected_q_and_delta_fixed"),
    base_configuration = list(chains = 4L, n_mcmc = 4000L),
    sampler = "intercept_marginal_slice_within_gibbs", new_tolerance_guarantee = FALSE,
    article_replaced = FALSE), jobs = jobs, datasets = d, parameters = p, intervals = intervals,
    paired = paired, summary = mgfa_cells(intervals), final = final,
    policy = list(exploratory_rhat = 1.05, exploratory_ess = 100, exploratory_mcse_ratio = .1,
      preferred_rhat = 1.01, preferred_ess = 400, preferred_mcse_ratio = .05))
}

write_report_fixture <- function(x, root) {
  directory <- file.path(root, "application/outputs/study")
  dir.create(directory, recursive = TRUE)
  policy <- file.path(root, x$manifest$configuration$practical_config)
  dir.create(dirname(policy), recursive = TRUE)
  json <- function(value, path) jsonlite::write_json(value, path, auto_unbox = TRUE, null = "null", digits = NA)
  json(list(policy = x$policy), policy)
  x$manifest$inputs <- mgfa_hash(policy)
  json(x$manifest, file.path(directory, "manifest.json"))
  json(list(status = "calculation_complete_review_required", commit = x$manifest$identity$commit,
    completed_base_chains = 8L, completed_extension_chains = 4L),
    file.path(directory, "campaign_status.json"))
  json(list(exit_code = 0L), file.path(directory, "controller_exit.json"))
  json(list(completed_chains = 8L, remaining_chains = 0L, completed_datasets = 2L),
    file.path(directory, "base_progress.json"))
  saveRDS(x$jobs, file.path(directory, "jobs.rds"))
  tables <- c(datasets = "dataset_summary", parameters = "parameter_summary",
    intervals = "interval_comparison", paired = "paired_simulation_comparison", summary = "comparison_summary")
  for (field in names(tables))
    data.table::fwrite(x[[field]], file.path(directory, paste0(tables[[field]], ".csv")), na = "NA")
  writeLines(c("%PDF-1.4", "%%EOF"), file.path(directory, "gibbs_ecm_comparison.pdf"))
  write.csv(data.frame(case_id = "example"), file.path(directory, "numerical_reference_examples.csv"), row.names = FALSE)
  json(x$final, file.path(directory, "final_summary.json"))
  files <- c("dataset_summary.csv", "parameter_summary.csv", "interval_comparison.csv",
    "paired_simulation_comparison.csv", "comparison_summary.csv", "gibbs_ecm_comparison.pdf",
    "numerical_reference_examples.csv", "final_summary.json")
  json(list(source_commit = x$manifest$identity$commit,
    manifest_sha256 = mgfa_hash(file.path(directory, "manifest.json")),
    artifacts = setNames(vapply(file.path(directory, files), mgfa_hash, ""), files)),
    file.path(directory, "report_provenance.json"))
  directory
}

testthat::test_that("independent report checks preserve distinct endpoint and width summaries", {
  x <- report_fixture()
  result <- do.call(mgfa_tables, x)
  testthat::expect_equal(result$datasets, 2L)
  testthat::expect_equal(result$extended_datasets, 1L)
  testthat::expect_false(x$datasets$posterior_width_median[1] == x$datasets$median_interval_width[1])
})

testthat::test_that("unresolved diagnostics are retained without changing the scientific comparison", {
  result <- do.call(mgfa_tables, report_fixture(unresolved = TRUE))
  testthat::expect_equal(result$unresolved_datasets, 1L)
  testthat::expect_equal(result$practical_datasets, 1L)
})

testthat::test_that("changed targets, duplicates, widths, and false diagnostic flags are rejected", {
  x <- report_fixture(); x$datasets$q[1] <- .99
  testthat::expect_error(do.call(mgfa_tables, x), "frozen dataset identity")
  x <- report_fixture(); x$datasets$dataset_id[2] <- 1L
  testthat::expect_error(do.call(mgfa_tables, x), "Dataset set")
  x <- report_fixture(); x$datasets$mean_width[1] <- 4.5
  testthat::expect_error(do.call(mgfa_tables, x), "mean interval width")
  x <- report_fixture(); x$parameters$rhat[1] <- 1.2
  testthat::expect_error(do.call(mgfa_tables, x), "practical parameter flags")
  x <- report_fixture(); x$datasets$retained_per_chain[1] <- 8000L
  testthat::expect_error(do.call(mgfa_tables, x), "Retained draw counts")
  x <- report_fixture(); x$parameters <- x$parameters[-1, ]
  testthat::expect_error(do.call(mgfa_tables, x), "Parameter set")
  x <- report_fixture(); x$jobs$chain[1] <- 2L
  testthat::expect_error(do.call(mgfa_tables, x), "missing or duplicated chains")
  x <- report_fixture(); x$final$datasets <- 3L
  testthat::expect_error(do.call(mgfa_tables, x), "final summary datasets")
})

testthat::test_that("paired differences and raw width quantiles are independently checked", {
  x <- report_fixture(); x$paired$width_difference_young_mathew <- 0
  testthat::expect_error(do.call(mgfa_tables, x), "paired width difference")
  x <- report_fixture(); x$summary$width_q025[1] <- 9
  testthat::expect_error(do.call(mgfa_tables, x), "recomputed cell summaries")
  x <- report_fixture(); x$summary$lower_95[x$summary$source == "application"] <- .8
  testthat::expect_error(do.call(mgfa_tables, x), "recomputed cell summaries")
  x <- report_fixture(); x$intervals <- x$intervals[-1, ]
  testthat::expect_error(do.call(mgfa_tables, x), "method set differs|missing datasets")
})

testthat::test_that("a running campaign remains pending without reading or changing result files", {
  path <- tempfile("report-pending-"); on.exit(unlink(path, recursive = TRUE))
  x <- report_fixture(); directory <- write_report_fixture(x, path)
  jsonlite::write_json(list(status = "base_running", commit = x$manifest$identity$commit),
    file.path(directory, "campaign_status.json"), auto_unbox = TRUE)
  writeLines("not a completed report", file.path(directory, "dataset_summary.csv"))
  before <- vapply(list.files(directory, full.names = TRUE), mgfa_hash, "")
  result <- mgfa_review(directory)
  testthat::expect_identical(result$status, "pending")
  testthat::expect_false(result$full_results_checked)
  testthat::expect_identical(before, vapply(list.files(directory, full.names = TRUE), mgfa_hash, ""))
  testthat::expect_error(mgfa_review(directory, strrep("b", 40)), "commit differs")
})

testthat::test_that("completed-report identities, file hashes, and incomplete states are checked", {
  path <- tempfile("report-complete-"); on.exit(unlink(path, recursive = TRUE))
  x <- report_fixture(); directory <- write_report_fixture(x, path)
  result <- mgfa_review(directory, x$manifest$identity$commit)
  testthat::expect_identical(result$status, "report_consistency_passed")
  testthat::expect_true(result$scientific_review_required)
  testthat::expect_false(result$new_tolerance_guarantee)
  testthat::expect_false(result$raw_chains_reassessed)
  writeLines("changed", file.path(directory, "comparison_summary.csv"))
  testthat::expect_error(mgfa_review(directory), "file hash differs")
  unlink(file.path(directory, "final_summary.json"))
  testthat::expect_error(mgfa_review(directory), "missing or empty")
})

testthat::test_that("stopped campaigns and integration-only examples cannot pass as full completion", {
  path <- tempfile("report-stop-"); on.exit(unlink(path, recursive = TRUE))
  x <- report_fixture(); directory <- write_report_fixture(x, path)
  jsonlite::write_json(list(status = "stopped_with_error", commit = x$manifest$identity$commit,
    error = "fixture failure"), file.path(directory, "campaign_status.json"), auto_unbox = TRUE)
  testthat::expect_error(mgfa_review(directory), "stopped_with_error")
  x$manifest$role <- "integration_bridge_only"
  jsonlite::write_json(x$manifest, file.path(directory, "manifest.json"), auto_unbox = TRUE)
  testthat::expect_error(mgfa_review(directory), "not a completed full campaign")
})

testthat::test_that("finishing, unsuccessful, and incomplete controllers cannot pass as completed", {
  path <- tempfile("report-exit-"); on.exit(unlink(path, recursive = TRUE))
  x <- report_fixture(); directory <- write_report_fixture(x, path)
  unlink(file.path(directory, "controller_exit.json"))
  testthat::expect_identical(mgfa_review(directory)$campaign_state, "awaiting_controller_exit")
  jsonlite::write_json(list(exit_code = 1L), file.path(directory, "controller_exit.json"), auto_unbox = TRUE)
  testthat::expect_error(mgfa_review(directory), "did not exit successfully")
  jsonlite::write_json(list(exit_code = 0L), file.path(directory, "controller_exit.json"), auto_unbox = TRUE)
  jsonlite::write_json(list(completed_chains = 4L, remaining_chains = 4L, completed_datasets = 1L),
    file.path(directory, "base_progress.json"), auto_unbox = TRUE)
  testthat::expect_error(mgfa_review(directory), "Base queue is incomplete")
})

testthat::test_that("the frozen design and diagnostic policy cannot change after sampling", {
  path <- tempfile("report-frozen-"); on.exit(unlink(path, recursive = TRUE))
  x <- report_fixture(); directory <- write_report_fixture(x, path)
  changed <- x$jobs; changed$delta[1] <- .5
  saveRDS(changed, file.path(directory, "jobs.rds"))
  testthat::expect_error(mgfa_review(directory), "Frozen design hash differs")
  saveRDS(x$jobs, file.path(directory, "jobs.rds"))
  jsonlite::write_json(list(policy = list(preferred_rhat = 2)),
    file.path(path, x$manifest$configuration$practical_config), auto_unbox = TRUE)
  testthat::expect_error(mgfa_review(directory), "policy is missing or changed")
})

testthat::test_that("nonfinite diagnostic summaries remain explicit unresolved results", {
  path <- tempfile("report-nonfinite-"); on.exit(unlink(path, recursive = TRUE))
  x <- report_fixture(unresolved = TRUE)
  x$parameters$rhat[6] <- NA_real_; x$datasets$maximum_rhat[2] <- NA_real_
  x$final$maximum_rhat <- NA_real_
  directory <- write_report_fixture(x, path)
  result <- mgfa_review(directory)
  testthat::expect_equal(result$results$unresolved_datasets, 1L)
  testthat::expect_true(is.na(result$results$maximum_rhat))
})

testthat::test_that("the command can require completion without starting further work", {
  path <- tempfile("report-cli-"); on.exit(unlink(path, recursive = TRUE))
  x <- report_fixture(); directory <- write_report_fixture(x, path)
  jsonlite::write_json(list(status = "base_running", commit = x$manifest$identity$commit),
    file.path(directory, "campaign_status.json"), auto_unbox = TRUE)
  output <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
    c(shQuote(file.path(root, "application/scripts/113_review_mti_gibbs_full_report.R")),
      shQuote(paste0("--output-dir=", directory)), "--require-complete"), stdout = TRUE, stderr = TRUE))
  testthat::expect_equal(attr(output, "status"), 2L)
  testthat::expect_identical(jsonlite::fromJSON(paste(output, collapse = "\n"))$status, "pending")
})
