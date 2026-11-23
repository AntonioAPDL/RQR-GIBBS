root <- normalizePath(if (file.exists("application/scripts/lib/mti_gibbs_skewness_publication.R")) "." else "../../..")
for (name in c("mti_gibbs_full_report_review", "mti_gibbs_full_publication", "mti_gibbs_width_diagnosis",
  "pharma_tolerance_application", "pharma_mti_application", "mti_fixed_prior_publication",
  "mti_fixed_prior_reporting", "mti_skewness_distributions", "mti_skewness_reporting",
  "mti_skewness_publication", "mti_gibbs_skewness", "mti_gibbs_skewness_reporting",
  "mti_gibbs_skewness_publication")) source(file.path(root, "application/scripts/lib", paste0(name, ".R")))

testthat::test_that("expanded Gibbs evidence preserves methods, targets and all primary estimates", {
  withr::local_dir(root)
  cfg <- mgsp_config()
  testthat::expect_silent(mgsp_verify(cfg, rendered = TRUE))
  d <- mgsp_read(cfg, "combined_cells")
  testthat::expect_equal(nrow(d), 432L)
  testthat::expect_equal(unique(d$family_size), 108L)
  testthat::expect_equal(d[method == "gibbs", sum(samples)], 108000L)
  testthat::expect_true(all(d$returned == d$samples))
  p <- mwd_json(file.path(cfg$evidence_dir, "provenance.json"))
  for (field in c("prior_uses_analysis_data", "targets_resampled", "fits_rerun",
    "primary_comparison_changed", "calibrated_gibbs_study_changed")) testthat::expect_false(p[[field]])
  testthat::expect_equal(p$study_source_commit, cfg$study_source_commit)
  testthat::expect_equal(p$posterior_tail_precision_failures, 82969L)
  n <- mgsp_read(cfg, "numerical_summary")
  testthat::expect_equal(sum(n$adequate), 36000L)
  testthat::expect_equal(sum(n$errors + n$warnings), 0L)
  testthat::expect_equal(sum(n$retained_1000), 31841L)
  testthat::expect_true(all(n$max_rhat <= 1.01))
})

testthat::test_that("offline verification requires no study pointer or raw fits and rejects corruption", {
  withr::local_dir(root)
  cfg <- mgsp_config(); directory <- withr::local_tempdir()
  testthat::expect_true(all(file.copy(list.files(cfg$evidence_dir, full.names = TRUE), directory)))
  cfg$evidence_dir <- directory
  cfg$study_pointer <- file.path(directory, "absent.json")
  cfg$local_output <- file.path(directory, "absent")
  testthat::expect_silent(mgsp_verify(cfg, rendered = TRUE))
  p <- mwd_json(file.path(directory, "provenance.json")); p$targets_resampled <- TRUE
  mgfp_json_write(p, file.path(directory, "provenance.json"))
  testthat::expect_error(mgsp_verify(cfg), "interpretation changed")
  p$targets_resampled <- FALSE; mgfp_json_write(p, file.path(directory, "provenance.json"))
  d <- mgsp_read(cfg, "combined_cells"); d[1, attainment := 0]
  msu_write(d, file.path(directory, "combined_cells.csv"))
  testthat::expect_error(mgsp_verify(cfg), "publication data changed")
})

testthat::test_that("incorrect bounds, missing designs and changed historical estimates are rejected", {
  withr::local_dir(root)
  cfg <- mgsp_config(); d <- mgsp_read(cfg, "combined_cells")
  testthat::expect_error(mgsp_validate(d[-1], cfg), "Incomplete")
  changed <- data.table::copy(d); changed[1, family_size := 72L]
  testthat::expect_error(mgsp_validate(changed, cfg), "Monte Carlo family")
  changed <- data.table::copy(d); changed[1, simultaneous_lower := lower_95]
  testthat::expect_error(mgsp_validate(changed, cfg), "summaries differ")
  changed <- data.table::copy(d); changed[law_id == "normal" & method == "gibbs", mean_width := mean_width + 1]
  testthat::expect_error(mgsp_validate(changed, cfg), "summaries differ")
})

testthat::test_that("endpoint decomposition uses each case's own widths and known support only for assessment", {
  withr::local_dir(root)
  cfg <- mgsp_config()
  tasks <- data.frame(job_id = 1:2, q = .95, delta = 0)
  d <- data.table::CJ(job_id = 1:2, method = c("ecm", "gibbs"))
  d[, `:=`(law_id = "invgauss_shape025", n = c(50L, 100L)[job_id], content = .9,
    q = .95, delta = 0, lower = ifelse(method == "gibbs", -.8, -.4),
    upper = ifelse(method == "gibbs", 1.1, 1), attained = TRUE,
    dp_probability = .99, ecm_probability_threshold = .995, lower_below_support = method == "gibbs")]
  d[, width := upper - lower]
  result <- mgsp_endpoints(d, tasks, cfg)
  testthat::expect_equal(result$mean_width_difference, rep(.5, 2))
  testthat::expect_equal(result$lower_share, rep(.8, 2))
  testthat::expect_equal(result$below_support_share, rep(.6, 2))
  testthat::expect_equal(result$mean_width_ratio, rep(1.9 / 1.4, 2))
  d[method == "gibbs", q := .96]
  testthat::expect_error(mgsp_endpoints(d, tasks, cfg), "targets changed")
})

testthat::test_that("displayed counts preserve the old exception and all 108 comparison cases", {
  withr::local_dir(root)
  cfg <- mgsp_config(); numbers <- mgsp_numbers(cfg)
  testthat::expect_equal(numbers[["MTIGibbsExpandedCases"]], "108")
  testthat::expect_equal(numbers[["MTIGibbsExpandedWider"]], "106")
  testthat::expect_equal(numbers[["MTIGibbsAddedMin"]], "99.2")
  testthat::expect_equal(numbers[["MTIGibbsAddedWidthMax"]], "125.1")
  testthat::expect_equal(numbers[["MTIGibbsAddedBelowCutoff"]], "12,576")
  testthat::expect_equal(numbers[["MTIGibbsAddedBelowNominal"]], "0")
  g <- mgsp_read(cfg, "combined_cells")[method == "gibbs"]
  testthat::expect_equal(nrow(g[attainment < .95]), 1L)
  testthat::expect_equal(g[attainment < .95, attained], 931L)
  testthat::expect_true(g[attainment < .95, upper_95] < .95)
  testthat::expect_true(g[attainment < .95, simultaneous_upper] > .95)
})

testthat::test_that("supplement uses complete statistical displays and keeps calibrated Gibbs separate", {
  withr::local_dir(root)
  cfg <- mgsp_config()
  table <- paste(readLines(file.path(cfg$table_dir, "simulation_comparison.tex")), collapse = "\n")
  testthat::expect_equal(length(gregexpr("\\begin{longtable}", table, fixed = TRUE)[[1]]), 12L)
  testthat::expect_equal(length(gregexpr("MTI-Gibbs &", table, fixed = TRUE)[[1]]), 108L)
  testthat::expect_false(grepl("DGPs|Fail-closed|Median|Width/|Time|seconds|pipeline|contract|article-facing", table))
  testthat::expect_equal(mgsp_colors[["wilks"]], "black")
  paths <- file.path(cfg$figure_dir, c("attainment_ranges.pdf", sprintf("width_ranges_%02d.pdf", 1:6)))
  testthat::expect_true(all(file.info(paths)$size > 5000))
  for (path in paths) testthat::expect_equal(rawToChar(readBin(path, "raw", 4)), "%PDF")
  main <- paste(readLines("main.tex"), collapse = "\n")
  supplement <- paste(readLines("rqr-gibbs-supplement.tex"), collapse = "\n")
  testthat::expect_true(grepl("same twelve distributions", main, fixed = TRUE))
  testthat::expect_false(grepl("no Gibbs fits are added", supplement, fixed = TRUE))
  testthat::expect_true(grepl(paste0(cfg$table_dir, "/simulation_comparison.tex"), supplement, fixed = TRUE))
  testthat::expect_false(grepl("mti_fixed_prior_20261115/simulation_comparison.tex", supplement, fixed = TRUE))
  testthat::expect_true(grepl("72-case design", supplement, fixed = TRUE))
  make <- readLines("Makefile")
  testthat::expect_true(any(grepl("pdf supplement arxiv-source article-handoff: verify-mti-gibbs-skewness-publication", make, fixed = TRUE)))
})
