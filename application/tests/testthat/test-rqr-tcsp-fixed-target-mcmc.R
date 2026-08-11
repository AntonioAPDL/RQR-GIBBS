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
    "fixed_rate"
  )
  expect_error(
    rqr_tcsp_fit_univariate(
      y, 0.35, 0.60,
      scan_method = "dkw_conservative",
      fit_mcmc = TRUE,
      mcmc_args = list(beta_prior_obj = list(type = "rhs_ns"))
    ),
    "ridge"
  )
})
