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
  expect_true("oracle_sh" %in% as.character(main$method_ids))
  expect_false("dpm_bayes" %in% as.character(main$method_ids))
  expect_false("dp_bayes" %in% as.character(main$method_ids))

  dpm <- config$modes$dpm_companion
  expect_equal(as.numeric(dpm$sample_sizes), c(500, 1000))
  expect_equal(as.numeric(dpm$guaranteed_contents), c(0.90, 0.95))
  expect_true("dpm_bayes" %in% as.character(dpm$method_ids))

  methods <- vapply(config$methods, `[[`, character(1), "method_id")
  expect_true(all(c("oracle_sh", "hdp_s_mc", "tcsp_mc", "tcsp_dkw",
                    "dpm_bayes") %in% methods))

  dgps <- vapply(config$dgps, `[[`, character(1), "dgp_id")
  expect_true(all(c("lognormal_hard", "sharp_mixture",
                    "contaminated_normal", "student_t3") %in% dgps))

  expect_identical(config$oracle$target, "SH")
  expect_false(isTRUE(config$methods[[which(methods == "oracle_sh")]]$deployable))
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

  expect_equal(nrow(results), 54L)
  expect_true(all(c(
    "lower", "upper", "retained_fraction", "content_gap",
    "posterior_threshold_excess",
    "scan_critical_method", "content_buffer",
    "scan_certified_lower_probability", "width_ratio_to_reference",
    "width_diff_to_reference", "posterior_constraint_binding",
    "oracle_mean_tilt", "oracle_certificate_digest", "oracle_sh_width",
    "width_ratio_to_oracle_sh", "width_excess_vs_oracle_sh"
  ) %in% names(results)))
  expect_true(all(c(
    "success_rate_minus_tolerance_confidence",
    "mean_width_ratio_to_reference", "posterior_binding_rate",
    "mean_width_ratio_to_oracle_sh", "mean_width_excess_vs_oracle_sh"
  ) %in% names(summary)))
  expect_identical(manifest$diagnostic_reference_method_id, "tcsp_mc")
  expect_true(manifest$oracle_sh_reference_present)
  expect_true(any(results$method_id == "hdp_s_mc"))
  expect_true(any(results$method_id == "tcsp_mc"))
  expect_true(any(results$method_id == "oracle_sh"))
  oracle_rows <- results[results$method_id == "oracle_sh", , drop = FALSE]
  expect_true(all(abs(oracle_rows$content_gap) < 1e-8))
  expect_true(all(abs(oracle_rows$width_ratio_to_oracle_sh - 1) < 1e-8))
})

test_that("Bayesian UQ worker can execute a deterministic one-cell wave", {
  output_dir <- tempfile("rqr-bayes-uq-wave-smoke-")
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
      paste0("--output-dir=", output_dir), "--wave-id=test_wave",
      "--wave-dgp=normal", "--wave-n=80", "--wave-content=0.90"),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_equal(attr(status, "status") %||% 0L, 0L)

  results <- read.csv(file.path(output_dir, "bayes_uq_validation_results.csv"))
  manifest <- jsonlite::read_json(file.path(output_dir, "manifest.json"),
                                  simplifyVector = TRUE)
  expect_equal(nrow(results), 9L)
  expect_equal(unique(results$dgp_id), "normal")
  expect_equal(unique(results$n), 80L)
  expect_equal(unique(results$guaranteed_content), 0.90)
  expect_identical(manifest$wave_id, "test_wave")
})

test_that("Bayesian UQ wave manager prepares frozen reproducible inputs", {
  run_root <- tempfile("rqr-bayes-uq-wave-preflight-")
  on.exit(unlink(run_root, recursive = TRUE, force = TRUE), add = TRUE)

  script <- normalizePath(
    testthat::test_path(
      "..", "..", "scripts", "71_manage_rqr_bayes_uq_main_waves.R"
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
    c(script, "--action=prepare", "--mode=smoke",
      paste0("--config=", config), paste0("--run-root=", run_root),
      "--run-id=test_preflight", "--require-clean=false"),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_equal(attr(status, "status") %||% 0L, 0L)

  run_dir <- file.path(run_root, "test_preflight")
  wave_plan <- read.csv(file.path(run_dir, "wave_plan.csv"))
  oracle_reference <- read.csv(file.path(run_dir, "oracle_reference.csv"))
  preflight <- jsonlite::read_json(
    file.path(run_dir, "preflight_manifest.json"), simplifyVector = TRUE
  )

  expect_equal(nrow(wave_plan), 6L)
  expect_equal(preflight$n_waves, 6L)
  expect_equal(preflight$expected_result_rows, 54L)
  expect_equal(nrow(oracle_reference), 6L)
  expect_true(file.exists(file.path(run_dir, "scan_calibration_cache.rds")))
  expect_true(file.exists(file.path(run_dir, "oracle_cache.rds")))
})
