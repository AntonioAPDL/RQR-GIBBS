test_that("bounded dynamic fixture configuration is exact and non-production", {
  config_path <- testthat::test_path(
    "..", "..", "config", "rqr_dlm",
    "rqr_dlm_bounded_dynamic_fixtures_20260722.R"
  )
  environment <- new.env(parent = baseenv())
  sys.source(config_path, envir = environment)
  config <- environment$rqr_dlm_bounded_dynamic_fixtures

  expect_identical(
    config$schema_version, "rqrgibbs_dlm_bounded_fixtures/1.0.0"
  )
  expect_true(config$generalized_bayes)
  expect_false(config$response_likelihood)
  expect_false(config$response_prediction_contract)
  expect_false(config$production_simulation_authorized)
  expect_identical(config$mcmc$chains, 4L)
  expect_identical(config$mcmc$burn_in, 2000L)
  expect_identical(config$mcmc$retained_per_chain, 4000L)
  expect_length(unique(config$mcmc$seeds), 4L)
  expect_identical(config$continuation$generations, 2L)
  expect_setequal(
    vapply(
      config$fixtures, function(fixture) fixture$evolution_mode,
      character(1L)
    ),
    c("fixed_W", "discount_template", "component_scale")
  )
  expect_false(any(vapply(
    config$fixtures,
    function(fixture) identical(
      fixture$evolution_mode, "adaptive_discount"
    ),
    logical(1L)
  )))
  expect_true(all(vapply(
    config$fixtures,
    function(fixture) length(fixture$y) == fixture$n_time,
    logical(1L)
  )))
  expect_identical(
    config$fixtures$frozen_trend_seasonal_discount$dim_df,
    c(2L, 3L)
  )
  expect_identical(
    length(
      config$fixtures$
        shared_component_scale_trend_regression$component_templates
    ),
    2L
  )
})
