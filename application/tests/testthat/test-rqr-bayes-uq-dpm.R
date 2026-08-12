test_that("Gaussian DPM Gibbs fit returns posterior response-distribution draws", {
  set.seed(1001)
  y <- c(rnorm(12, -1, 0.4), rnorm(12, 1, 0.4))

  fit <- rqr_dpm_fit(
    y,
    truncation_level = 3,
    concentration = 1,
    mcmc_control = list(n_iter = 18, burn_in = 8, thin = 5, seed = 1002)
  )

  expect_s3_class(fit, "rqr_dpm_mcmc")
  expect_true(fit$response_likelihood)
  expect_false(fit$generalized_bayes)
  expect_equal(length(fit$parameters), 2L)
  expect_true(all(vapply(fit$parameters, function(p) {
    abs(sum(p$weights) - 1) < 1e-10 && all(p$lambda > 0)
  }, logical(1L))))
  expect_true(all(is.finite(fit$trace$log_likelihood)))
})

test_that("Gaussian DPM content, CDF, density, and quantile functionals are usable", {
  y <- seq(-2, 2, length.out = 18)
  fit <- rqr_dpm_fit(
    y,
    truncation_level = 3,
    concentration = 1,
    mcmc_control = list(n_iter = 18, burn_in = 8, thin = 5, seed = 2001)
  )

  dens <- rqr_dpm_density(fit, x = c(-1, 0, 1))
  cdf <- rqr_dpm_cdf(fit, x = c(-1, 0, 1))
  q <- rqr_dpm_quantile(fit, p = c(0.25, 0.75), draw = 1)
  prob <- rqr_dpm_content_probability(
    fit, lower = min(y), upper = max(y), content = 0.5
  )

  expect_true(all(is.finite(dens$density)))
  expect_true(all(dens$density >= 0))
  expect_true(all(diff(cdf$cdf) >= -1e-10))
  expect_true(all(diff(q$quantile) > 0))
  expect_true(prob$posterior_content_probability >= 0)
  expect_true(prob$posterior_content_probability <= 1)
})

test_that("Gaussian DPM shortest draws and hybrid action are wired", {
  y <- seq(-2, 2, length.out = 24)

  fit <- rqr_dpm_fit(
    y,
    truncation_level = 3,
    concentration = 1,
    mcmc_control = list(n_iter = 18, burn_in = 8, thin = 5, seed = 3001)
  )
  shortest <- rqr_dpm_shortest_draws(
    fit, target_content = 0.5, grid_size = 7, optimize = FALSE
  )
  expect_equal(nrow(shortest), length(fit$parameters))
  expect_true(all(shortest$retained_mass >= 0.5 - 1e-5))
  expect_true(all(is.finite(shortest$width)))

  hybrid <- rqr_tcsp_hybrid_bayes_fit(
    y,
    guaranteed_content = 0.35,
    tolerance_confidence = 0.50,
    posterior_confidence = 0.50,
    distribution_engine = "gaussian_dpm",
    scan_method = "dkw_conservative",
    distribution_args = list(
      truncation_level = 3,
      concentration = 1,
      mcmc_control = list(n_iter = 18, burn_in = 8, thin = 5, seed = 3002)
    ),
    action_control = list(n_shortest_draws = 1)
  )
  expect_s3_class(hybrid, "rqr_hybrid_bayes_tcsp")
  expect_identical(hybrid$distribution_engine, "gaussian_dpm")
  expect_true(hybrid$finite_sample_scan_guard_available)
  expect_false(hybrid$posterior_endpoint_coverage_claim_available)
})

test_that("Gaussian DPM ECM is deterministic diagnostic MAP, not posterior UQ", {
  y <- seq(-2, 2, length.out = 24)
  fit <- rqr_dpm_ecm_fit(
    y,
    truncation_level = 3,
    concentration = 1,
    ecm_control = list(max_iter = 12, seed = 4001)
  )

  expect_s3_class(fit, "rqr_dpm_ecm")
  expect_true(fit$response_likelihood)
  expect_false(fit$generalized_bayes)
  expect_false(fit$posterior_predictive_distribution_available)
  expect_identical(fit$uq_role, "deterministic_map_diagnostic_not_posterior_uq")
  expect_true(all(diff(fit$trace$objective) >= -1e-7))
  expect_equal(sum(fit$parameters$weights), 1, tolerance = 1e-10)
})
