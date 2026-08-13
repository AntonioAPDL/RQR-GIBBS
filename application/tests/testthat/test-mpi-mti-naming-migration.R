test_that("MPI is the zero-tilt member of the MTI loss", {
  y <- c(-1.2, -0.1, 0.4, 1.7)
  eta1 <- c(-1.5, -0.3, 0.1, 1.1)
  eta2 <- c(-0.2, 0.8, 1.0, 2.0)
  content <- 0.8
  mean_tilt <- c(-0.2, 0.0, 0.15, 0.25)

  expect_equal(
    interval_check_loss(
      interval_residual_product(y, eta1, eta2),
      content = content
    ),
    rqr_check_loss(rqr_residual_product(y, eta1, eta2), content)
  )
  expect_equal(
    mpi_loss(y, eta1, eta2, content = content),
    mti_loss(y, eta1, eta2, content = content, mean_tilt = 0)
  )
  expect_equal(
    mti_loss(y, eta1, eta2, content = content, mean_tilt = mean_tilt),
    rqr_mean_tilt_loss(
      y, eta1, eta2, coverage_level = content, mean_tilt = mean_tilt
    )
  )
  expect_equal(
    mti_constants(content = content),
    rqr_constants(coverage_level = content)
  )
})

test_that("canonical MTI wrappers retain legacy object compatibility", {
  y <- c(-1.1, -0.3, 0.2, 0.55, 1.1)
  X <- cbind(1, seq(-1, 1, length.out = length(y)))
  prior <- beta_prior("ridge", ridge = list(tau2 = 25))

  fit <- mti_mcmc_fit(
    y = y,
    X = X,
    content = 0.75,
    mean_tilt = seq(-0.05, 0.05, length.out = length(y)),
    beta_prior_obj = prior,
    mcmc_control = list(n_burn = 2, n_mcmc = 4, thin = 1, seed = 9101)
  )
  expect_s3_class(fit, "mti_mcmc")
  expect_s3_class(fit, "rqr_mcmc")
  draws <- mti_posterior_draws(fit)
  expect_equal(draws$nd, 4L)

  ecm <- mti_ecm_fit(
    y = y,
    X = X,
    content = 0.75,
    mean_tilt = 0,
    beta_prior_obj = prior,
    ecm_control = list(
      max_iter = 8,
      stable_iterations = 1,
      tol_stationarity = 1e6,
      starts = c("quantile")
    )
  )
  expect_s3_class(ecm, "mti_ecm")
  expect_s3_class(ecm, "rqr_ecm")
})

test_that("canonical TCSP names expose MTI action metadata", {
  y <- c(-2, -1, -0.2, 0.1, 0.4, 0.7, 2)
  calibration <- tcsp_calibrate_count(
    n = length(y),
    guaranteed_content = 0.4,
    tolerance_confidence = 0.5,
    method = "monte_carlo_conservative",
    n_sim = 100,
    numerical_confidence = 0.8,
    seed = 9102
  )
  expect_identical(calibration$method, "scan_calibrated_tcsp_mti")
  expect_identical(calibration$legacy_method, "scan_calibrated_tcsp_mt_rqr")

  window <- tcsp_shortest_window(y, retained_count = calibration$retained_count)
  expect_s3_class(window, "tcsp_window")
  expect_s3_class(window, "rqr_tcsp_window")
  tilt <- tcsp_tilt_from_window(window)
  expect_true(is.finite(tilt$delta_raw))
  expect_true(is.finite(tilt$delta_standardized))
})

test_that("canonical DP, DPM, and bootstrap names are response-UQ separated", {
  y <- seq(-1.5, 1.5, length.out = 12)

  base <- dp_base_normal(mean = 0, sd = 2)
  expect_s3_class(base, "dp_base_measure")
  dp <- dp_fit(y, concentration = 1, base_measure = base)
  expect_s3_class(dp, "dp_fit")
  expect_s3_class(dp, "rqr_dp_fit")
  expect_true(dp$response_likelihood)
  expect_false(dp$generalized_bayes)
  prob <- dp_content_probability(dp, lower = -1, upper = 1, content = 0.5)
  expect_true(prob$posterior_content_probability >= 0)
  expect_true(prob$posterior_content_probability <= 1)

  boot <- bayesian_bootstrap_shortest_draws(y, n_draws = 2, seed = 9103)
  expect_s3_class(boot, "bayesian_bootstrap_shortest_draws")
  shortest <- dp_shortest_draws(boot, target_content = 0.5)
  expect_equal(nrow(shortest), 2L)

  dpm <- dpm_fit(
    y,
    truncation_level = 2,
    concentration = 1,
    mcmc_control = list(n_iter = 10, burn_in = 4, thin = 3, seed = 9104)
  )
  expect_s3_class(dpm, "dpm_mcmc")
  expect_s3_class(dpm, "rqr_dpm_mcmc")
  expect_true(dpm$response_likelihood)
  expect_false(dpm$generalized_bayes)
})
