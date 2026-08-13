`%||%` <- function(a, b) if (is.null(a)) b else a

test_that("Bayesian UQ main config freezes high-coverage iid launch scope", {
  config <- jsonlite::read_json(
    testthat::test_path(
      "..", "..", "config", "rqr_bayes_uq_validation_main_20260813.json"
    ),
    simplifyVector = FALSE
  )

  expect_true(config$execution$confirmatory_authorized)
  expect_true(config$execution$dpm_companion_authorized)
  expect_true(config$claim_scope$iid_univariate_continuous_only)
  expect_false(config$claim_scope$regression_tolerance)
  expect_false(config$claim_scope$dynamic_tolerance)
  expect_true(config$claim_scope$coverage_levels_are_guaranteed_contents)
  expect_identical(config$diagnostics$reference_method_id, "tcsp_mc")

  main <- config$modes$confirmatory
  expect_equal(as.numeric(main$sample_sizes), c(500, 1000))
  expect_equal(as.numeric(main$guaranteed_contents), c(0.90, 0.95, 0.99))
  expect_equal(as.numeric(main$tolerance_confidences), 0.95)
  expect_equal(as.numeric(main$posterior_confidences), c(0.90, 0.95, 0.99))
  expect_true(isTRUE(main$paired_thresholds))
  expect_false("dpm_bayes" %in% as.character(main$method_ids))
  expect_false("dp_bayes" %in% as.character(main$method_ids))

  dpm <- config$modes$dpm_companion
  expect_equal(as.numeric(dpm$sample_sizes), c(500, 1000))
  expect_equal(as.numeric(dpm$guaranteed_contents), c(0.90, 0.95))
  expect_true("dpm_bayes" %in% as.character(dpm$method_ids))

  methods <- vapply(config$methods, `[[`, character(1), "method_id")
  expect_true(all(c("hdp_s_mc", "tcsp_mc", "tcsp_dkw", "dpm_bayes") %in%
                    methods))

  dgps <- vapply(config$dgps, `[[`, character(1), "dgp_id")
  expect_true(all(c("lognormal_hard", "sharp_mixture",
                    "contaminated_normal", "student_t3") %in% dgps))
})

test_that("Bayesian UQ main smoke runner emits launch diagnostics", {
  output_dir <- tempfile("rqr-bayes-uq-main-smoke-")
  on.exit(unlink(output_dir, recursive = TRUE, force = TRUE), add = TRUE)

  script <- normalizePath(
    testthat::test_path(
      "..", "..", "scripts", "69_validate_rqr_bayes_uq.R"
    ),
    winslash = "/",
    mustWork = TRUE
  )
  config <- normalizePath(
    testthat::test_path(
      "..", "..", "config", "rqr_bayes_uq_validation_main_20260813.json"
    ),
    winslash = "/",
    mustWork = TRUE
  )
  status <- system2(
    "Rscript",
    c(script, "--mode=smoke", paste0("--config=", config),
      paste0("--output-dir=", output_dir)),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_equal(attr(status, "status") %||% 0L, 0L)

  results <- read.csv(file.path(output_dir, "bayes_uq_validation_results.csv"))
  summary <- read.csv(file.path(output_dir, "bayes_uq_validation_summary.csv"))
  manifest <- jsonlite::read_json(file.path(output_dir, "manifest.json"),
                                  simplifyVector = TRUE)

  expect_equal(nrow(results), 48L)
  expect_true(all(c(
    "retained_fraction", "content_gap", "posterior_threshold_excess",
    "scan_critical_method", "content_buffer",
    "scan_certified_lower_probability", "width_ratio_to_reference",
    "width_diff_to_reference", "posterior_constraint_binding"
  ) %in% names(results)))
  expect_true(all(c(
    "success_rate_minus_tolerance_confidence",
    "mean_width_ratio_to_reference", "posterior_binding_rate"
  ) %in% names(summary)))
  expect_identical(manifest$diagnostic_reference_method_id, "tcsp_mc")
  expect_true(any(results$method_id == "hdp_s_mc"))
  expect_true(any(results$method_id == "tcsp_mc"))
})
