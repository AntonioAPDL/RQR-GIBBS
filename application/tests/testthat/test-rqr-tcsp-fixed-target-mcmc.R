test_that("TCSP fixed-target MCMC uses calibrated q and frozen shortest-path tilt", {
  y <- seq(-2, 2, length.out = 120)
  fit <- rqr_tcsp_fit_univariate(
    y,
    guaranteed_content = 0.35,
    tolerance_confidence = 0.60,
    scan_method = "dkw_conservative",
    fit_mcmc = TRUE,
    learning_rate = 0.8,
    mcmc_args = list(mcmc_control = list(n_burn = 2, n_mcmc = 3, seed = 9103))
  )

  expect_s3_class(fit$posterior_fit, "rqr_mcmc")
  expect_equal(fit$posterior_fit$model_spec$coverage_level,
               fit$contract$target_content)
  expect_identical(fit$posterior_fit$model_spec$mean_tilt_mode,
                   "fixed_response_scale")
  expect_equal(unique(fit$posterior_fit$model_spec$mean_tilt),
               fit$contract$delta_raw)
})

test_that("TCSP fixed-target MCMC gates unsupported nonzero-tilt modes", {
  y <- seq(-2, 2, length.out = 120)

  expect_error(
    rqr_tcsp_fit_univariate(
      y, 0.35, 0.60,
      scan_method = "dkw_conservative",
      fit_mcmc = TRUE,
      mcmc_args = list(learning_rate_mode = "learned_pure")
    ),
    "reserves"
  )
  expect_error(
    rqr_tcsp_fit_univariate(
      y, 0.35, 0.60,
      scan_method = "dkw_conservative",
      fit_mcmc = TRUE,
      mcmc_args = list(beta_prior_obj = list(type = "rhs_ns"))
    ),
    "reserves"
  )
})

test_that("TCSP fixed-target MCMC rejects reserved target overrides", {
  y <- seq(-2, 2, length.out = 120)
  reserved_args <- list(
    list(y = rev(y)),
    list(X = matrix(1, length(y), 1L)),
    list(coverage_level = 0.2),
    list(mean_tilt = 0),
    list(learning_rate = 2),
    list(response_likelihood = TRUE)
  )

  for (arg in reserved_args) {
    expect_error(
      rqr_tcsp_fit_univariate(
        y, 0.35, 0.60,
        scan_method = "dkw_conservative",
        fit_mcmc = TRUE,
        mcmc_args = arg
      ),
      "reserves"
    )
  }
})

test_that("TCSP full-range empirical action fails closed for MCMC", {
  y <- seq(-2, 2, length.out = 100)

  expect_error(
    rqr_tcsp_fit_univariate(
      y, 0.80, 0.68,
      scan_method = "dkw_conservative",
      fit_mcmc = TRUE,
      mcmc_args = list(mcmc_control = list(n_burn = 1, n_mcmc = 1))
    ),
    "target_content >= 1"
  )
})
