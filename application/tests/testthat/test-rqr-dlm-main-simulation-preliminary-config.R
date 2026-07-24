test_that("preliminary main RQR-DLM simulation config is fail closed", {
  config_path <- testthat::test_path(
    "..", "..", "config", "rqr_dlm",
    "rqr_dlm_main_simulation_preliminary_20260724.R"
  )
  environment <- new.env(parent = baseenv())
  sys.source(config_path, envir = environment)
  config <- environment$rqr_dlm_main_simulation_preliminary

  expect_identical(
    config$schema_version,
    "rqrgibbs_dlm_main_simulation_preliminary/0.1.0"
  )
  expect_identical(config$status, "draft_for_independent_review")
  expect_false(config$production_simulation_authorized)
  expect_false(config$pilot_execution_authorized)
  expect_true(config$generalized_bayes)
  expect_false(config$response_likelihood)
  expect_false(config$response_prediction_contract)
  expect_identical(length(config$design$primary_dgp_ids), 6L)
  expect_identical(config$design$coverage_levels, c(0.80, 0.90))
  expect_true(config$design$training_only_standardization)
  expect_false(config$design$test_data_used_for_tuning)
  expect_identical(config$monte_carlo$framework, "ADEMP")
  expect_true(config$monte_carlo$report_mcse_for_every_primary_summary)
  expect_false(config$monte_carlo$stopping_uses_performance_rank)
  expect_true(config$monte_carlo$stopping_uses_monte_carlo_precision_only)
  expect_false(config$mcmc$production_schedule_frozen)
  expect_identical(config$mcmc$backend, "cpp")
  expect_identical(config$mcmc$numerical_policy, "fail")
  expect_true(config$mcmc$learned_rate_is_sensitivity_not_calibration)
  expect_true(config$reproducibility$isolated_primary_runtime_required)
  expect_true(config$reproducibility$failed_fits_retained_in_denominator)
  expect_true("RQR-DESN" %in% config$scope$excludes)
  expect_true("CAVI or ELBO" %in% config$scope$excludes)
})
