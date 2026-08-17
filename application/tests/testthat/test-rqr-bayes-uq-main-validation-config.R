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
  expect_true("tcsp_mti_gibbs_median_mc" %in% as.character(main$method_ids))
  expect_true("tcsp_mti_ecm_map_mc" %in% as.character(main$method_ids))
  expect_true("split_ecm_fixed_tilt" %in% as.character(main$method_ids))
  expect_false("dpm_bayes" %in% as.character(main$method_ids))
  expect_false("dp_bayes" %in% as.character(main$method_ids))

  dpm <- config$modes$dpm_companion
  expect_equal(as.numeric(dpm$sample_sizes), c(500, 1000))
  expect_equal(as.numeric(dpm$guaranteed_contents), c(0.90, 0.95))
  expect_true("dpm_bayes" %in% as.character(dpm$method_ids))

  methods <- vapply(config$methods, `[[`, character(1), "method_id")
  expect_true(all(c("oracle_sh", "hdp_s_mc", "tcsp_mc", "tcsp_dkw",
                    "tcsp_mti_gibbs_median_mc",
                    "tcsp_mti_ecm_map_mc",
                    "split_ecm_fixed_tilt", "dpm_bayes") %in% methods))

  mti <- config$methods[methods %in% c(
    "tcsp_mti_gibbs_median_mc", "tcsp_mti_ecm_map_mc"
  )]
  expect_true(all(vapply(mti, function(x) {
    isFALSE(x$formal_tolerance_action) &&
      isTRUE(x$generalized_bayes) &&
      isFALSE(x$response_likelihood) &&
      identical(x$scan_method, "monte_carlo_conservative")
  }, logical(1))))

  dgps <- vapply(config$dgps, `[[`, character(1), "dgp_id")
  expect_true(all(c("lognormal_hard", "sharp_mixture",
                    "contaminated_normal", "student_t3") %in% dgps))

  expect_identical(config$oracle$target, "SH")
  expect_equal(
    config$engine_defaults$mti_ecm$confirmatory_ecm_control$tol_stationarity,
    1e-3
  )
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

  expect_equal(nrow(results), 72L)
  expect_true(all(c(
    "lower", "upper", "retained_fraction", "content_gap",
    "posterior_threshold_excess",
    "scan_critical_method", "content_buffer",
    "scan_certified_lower_probability", "width_ratio_to_reference",
    "width_diff_to_reference", "posterior_constraint_binding",
    "oracle_mean_tilt", "oracle_certificate_digest", "oracle_sh_width",
    "width_ratio_to_oracle_sh", "width_excess_vs_oracle_sh",
    "action_lane", "selected_interval_source",
    "formal_action_lower", "formal_action_upper", "formal_action_width",
    "formal_action_content", "formal_action_success",
    "fitted_summary_lower", "fitted_summary_upper",
    "fitted_summary_width", "uq_engine", "tilt_source",
    "target_content", "target_mean_tilt", "target_audit_digest",
    "posterior_draws", "mcmc_n_burn", "mcmc_n_mcmc", "mcmc_thin",
    "ecm_converged", "ecm_iterations", "ecm_objective",
    "ecm_trace_length", "ecm_initial_objective", "ecm_final_objective",
    "ecm_relative_objective_drop", "ecm_final_stationarity",
    "fit_reused_across_posterior_thresholds"
  ) %in% names(results)))
  expect_true(all(c(
    "success_rate_minus_tolerance_confidence",
    "mean_width_ratio_to_reference", "posterior_binding_rate",
    "mean_width_ratio_to_oracle_sh", "mean_width_excess_vs_oracle_sh",
    "median_formal_action_width", "mean_formal_action_content",
    "formal_action_success_rate", "median_fitted_summary_width",
    "mean_target_content", "mean_target_mean_tilt",
    "mean_posterior_draws", "mcmc_fit_reuse_rate",
    "ecm_convergence_rate", "mean_ecm_iterations",
    "mean_ecm_trace_length", "median_ecm_relative_objective_drop",
    "median_ecm_final_stationarity"
  ) %in% names(summary)))
  expect_identical(manifest$diagnostic_reference_method_id, "tcsp_mc")
  expect_true(manifest$oracle_sh_reference_present)
  expect_true(any(results$method_id == "hdp_s_mc"))
  expect_true(any(results$method_id == "tcsp_mc"))
  expect_true(any(results$method_id == "oracle_sh"))
  expect_true(any(results$method_id == "tcsp_mti_gibbs_median_mc"))
  expect_true(any(results$method_id == "tcsp_mti_ecm_map_mc"))
  expect_true(any(results$method_id == "split_ecm_fixed_tilt"))
  split_rows <- results[
    results$method_id %in% c("split_empirical_shortest", "split_ecm_fixed_tilt"),
    ,
    drop = FALSE
  ]
  expect_false(any(split_rows$fit_class == "error"))
  expect_true(any(
    split_rows$fit_class == "rqr_tcsp_split_exact_calibration_infeasible"
  ))
  gibbs_rows <- results[
    results$method_id == "tcsp_mti_gibbs_median_mc" &
      results$guaranteed_content == 0.90 &
      !results$infeasible,
    ,
    drop = FALSE
  ]
  ecm_rows <- results[
    results$method_id == "tcsp_mti_ecm_map_mc" &
      results$guaranteed_content == 0.90 &
      !results$infeasible,
    ,
    drop = FALSE
  ]
  expect_gt(nrow(gibbs_rows), 0L)
  expect_gt(nrow(ecm_rows), 0L)
  expect_true(any(grepl("mti_mcmc", gibbs_rows$fit_class, fixed = TRUE)))
  expect_true(any(grepl("rqr_mcmc", gibbs_rows$fit_class, fixed = TRUE)))
  expect_true(any(grepl("mti_ecm", ecm_rows$fit_class, fixed = TRUE)))
  expect_true(any(grepl("rqr_ecm", ecm_rows$fit_class, fixed = TRUE)))
  expect_true(all(is.finite(gibbs_rows$formal_action_width)))
  expect_true(all(is.finite(gibbs_rows$fitted_summary_width)))
  expect_true(all(is.finite(ecm_rows$formal_action_width)))
  expect_true(all(is.finite(ecm_rows$fitted_summary_width)))
  expect_true(all(gibbs_rows$response_likelihood == FALSE))
  expect_true(all(ecm_rows$response_likelihood == FALSE))
  expect_true(all(gibbs_rows$generalized_bayes == TRUE))
  expect_true(all(ecm_rows$generalized_bayes == TRUE))
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
      "--wave-dgp=normal", "--wave-n=120", "--wave-content=0.90"),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_equal(attr(status, "status") %||% 0L, 0L)

  results <- read.csv(file.path(output_dir, "bayes_uq_validation_results.csv"))
  manifest <- jsonlite::read_json(file.path(output_dir, "manifest.json"),
                                  simplifyVector = TRUE)
  expect_equal(nrow(results), 12L)
  expect_equal(unique(results$dgp_id), "normal")
  expect_equal(unique(results$n), 120L)
  expect_equal(unique(results$guaranteed_content), 0.90)
  expect_identical(manifest$wave_id, "test_wave")
  expect_true(any(results$method_id == "tcsp_mti_gibbs_median_mc"))
  expect_true(any(results$method_id == "tcsp_mti_ecm_map_mc"))
  expect_true(any(results$method_id == "split_ecm_fixed_tilt"))
})

test_that("Bayesian UQ worker reuses fixed-target MTI fits across paired thresholds", {
  output_dir <- tempfile("rqr-bayes-uq-cache-smoke-")
  config_path <- tempfile("rqr-bayes-uq-cache-config-", fileext = ".json")
  on.exit(unlink(c(output_dir, config_path), recursive = TRUE, force = TRUE),
          add = TRUE)

  config <- jsonlite::read_json(
    testthat::test_path(
      "..", "..", "config", "rqr_bayes_uq_validation_main_20260813.json"
    ),
    simplifyVector = FALSE
  )
  config$modes$smoke$replications <- 1L
  config$modes$smoke$sample_sizes <- list(120L)
  config$modes$smoke$guaranteed_contents <- list(0.90)
  config$modes$smoke$tolerance_confidences <- list(0.90)
  config$modes$smoke$posterior_confidences <- list(0.90, 0.95, 0.99)
  config$modes$smoke$dgp_ids <- list("normal")
  config$modes$smoke$method_ids <- list(
    "tcsp_mti_gibbs_median_mc", "tcsp_mti_ecm_map_mc"
  )
  config$scan_calibration$smoke_n_sim <- 200L
  jsonlite::write_json(config, config_path, pretty = TRUE, auto_unbox = TRUE)

  script <- normalizePath(
    testthat::test_path(
      "..", "..", "scripts", "69_validate_rqr_bayes_uq.R"
    ),
    winslash = "/",
    mustWork = TRUE
  )
  status <- system2(
    "Rscript",
    c(script, "--mode=smoke", paste0("--config=", config_path),
      paste0("--output-dir=", output_dir), "--wave-id=cache_test"),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_equal(attr(status, "status") %||% 0L, 0L)

  results <- read.csv(file.path(output_dir, "bayes_uq_validation_results.csv"))
  expect_equal(nrow(results), 6L)
  expect_true(all(!results$infeasible))
  expect_equal(
    sum(results$fit_reused_across_posterior_thresholds, na.rm = TRUE),
    4L
  )
  expect_equal(sum(grepl("mti_mcmc", results$fit_class, fixed = TRUE)), 3L)
  expect_equal(sum(grepl("rqr_mcmc", results$fit_class, fixed = TRUE)), 3L)
  expect_equal(sum(grepl("mti_ecm", results$fit_class, fixed = TRUE)), 3L)
  expect_equal(sum(grepl("rqr_ecm", results$fit_class, fixed = TRUE)), 3L)
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
  expect_equal(preflight$expected_result_rows, 72L)
  expect_equal(nrow(oracle_reference), 6L)
  expect_true(file.exists(file.path(run_dir, "scan_calibration_cache.rds")))
  expect_true(file.exists(file.path(run_dir, "oracle_cache.rds")))
})

test_that("Bayesian UQ wave manager stop writes supersession markers", {
  run_root <- tempfile("rqr-bayes-uq-wave-stop-")
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
  prepare <- system2(
    "Rscript",
    c(script, "--action=prepare", "--mode=smoke",
      paste0("--config=", config), paste0("--run-root=", run_root),
      "--run-id=test_stop", "--require-clean=false"),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_equal(attr(prepare, "status") %||% 0L, 0L)

  run_dir <- file.path(run_root, "test_stop")
  stop_status <- system2(
    "Rscript",
    c(script, "--action=stop", "--mode=smoke",
      paste0("--config=", config), paste0("--run-dir=", run_dir),
      "--require-clean=false"),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_equal(attr(stop_status, "status") %||% 0L, 0L)
  expect_true(file.exists(file.path(run_dir, "superseded.json")))
  expect_true(file.exists(file.path(run_dir, "SUPERSEDED.md")))
})
