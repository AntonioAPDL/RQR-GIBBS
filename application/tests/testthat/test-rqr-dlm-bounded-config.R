test_that("bounded dynamic fixture configuration is exact and non-production", {
  config_path <- testthat::test_path(
    "..", "..", "config", "rqr_dlm",
    "rqr_dlm_bounded_dynamic_fixtures_20260723.R"
  )
  environment <- new.env(parent = baseenv())
  sys.source(config_path, envir = environment)
  config <- environment$rqr_dlm_bounded_dynamic_fixtures

  expect_identical(
    config$schema_version, "rqrgibbs_dlm_bounded_fixtures/2.0.0"
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
  expect_identical(
    dim(
      config$fixtures$frozen_trend_seasonal_discount$
        state_components[[1L]]$C0
    ),
    c(2L, 2L)
  )
  expect_identical(
    config$gates$root_swap_activity_role, "sidecar_only"
  )
  helper_path <- testthat::test_path(
    "..", "..", "scripts", "lib", "rqr_dlm_bounded_fixtures.R"
  )
  helper_environment <- new.env(parent = globalenv())
  sys.source(helper_path, envir = helper_environment)
  constructed <- helper_environment$rqr_build_all_bounded_dlm_fixtures(
    config
  )
  expect_length(constructed, 3L)
  expect_true(all(vapply(
    constructed,
    function(item) isTRUE(item$evolution$exact_joint_target),
    logical(1L)
  )))
  expect_true(
    constructed$frozen_trend_seasonal_discount$
      construction_audit$extension_reproduces_training
  )
  expect_identical(
    dim(
      constructed$shared_component_scale_trend_regression$
        future$component_templates[[1L]]
    ),
    c(2L, 2L, 3L)
  )
})

test_that("intercept CDF references are versioned and generator-bound", {
  reference_path <- testthat::test_path(
    "..", "..", "inst", "extdata",
    "output7_corrected_cdf_references.csv"
  )
  generator_path <- testthat::test_path(
    "..", "..", "scripts",
    "07_generate_intercept_cdf_references.R"
  )
  reference <- utils::read.csv(
    reference_path, stringsAsFactors = FALSE
  )
  expect_identical(nrow(reference), 5L)
  expect_true(all(
    reference$reference_schema ==
      "rqrgibbs_intercept_cdf_reference/2.0.0"
  ))
  expect_setequal(
    reference$estimand,
    c("lambda", "lower_root", "upper_root", "width", "midpoint")
  )
  expect_true(all(reference$order_convergence_difference <= 5e-10))
  expect_identical(
    unique(reference$generator_sha256),
    digest::digest(
      file = generator_path, algo = "sha256", serialize = FALSE
    )
  )
  expect_equal(
    reference$reference_value[
      match(
        c("lambda", "lower_root", "upper_root", "width", "midpoint"),
        reference$estimand
      )
    ],
    c(
      0.347247584303805, 0.408193003274045,
      0.562140568140968, 0.573003849468578,
      0.489059519337561
    ),
    tolerance = 1e-15
  )
})
