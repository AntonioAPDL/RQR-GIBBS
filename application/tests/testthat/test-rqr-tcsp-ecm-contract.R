test_that("TCSP ECM preserves the empirical formal action", {
  y <- seq(-3, 3, length.out = 150)^3 / 12 + seq(-3, 3, length.out = 150)
  no_engine <- rqr_tcsp_fit_univariate(
    y, 0.40, 0.60, scan_method = "dkw_conservative"
  )
  with_ecm <- rqr_tcsp_fit_univariate(
    y, 0.40, 0.60,
    scan_method = "dkw_conservative",
    fit_ecm = TRUE,
    ecm_args = list(ecm_control = list(
      max_iter = 20,
      stable_iterations = 1,
      tol_stationarity = 1e6
    ))
  )

  expect_s3_class(with_ecm$ecm_fit, "rqr_ecm")
  expect_equal(with_ecm$contract$lower_endpoint, no_engine$contract$lower_endpoint)
  expect_equal(with_ecm$contract$upper_endpoint, no_engine$contract$upper_endpoint)
  expect_identical(with_ecm$contract$ecm_map_action,
                   "not_formal_tolerance_action")
  expect_true(with_ecm$contract$ecm_conditional_on_selected_tcsp_target)
  expect_true(with_ecm$contract$ecm_fit_available)
  expect_equal(with_ecm$ecm_fit$model_spec$coverage_level,
               with_ecm$contract$target_content)
  expect_equal(unique(with_ecm$ecm_fit$model_spec$mean_tilt),
               with_ecm$contract$delta_raw)
})

test_that("TCSP ECM rejects target overrides", {
  y <- seq(-2, 2, length.out = 120)
  reserved_args <- list(
    list(y = rev(y)),
    list(X = matrix(1, length(y), 1L)),
    list(coverage_level = 0.2),
    list(mean_tilt = 0),
    list(learning_rate = 2),
    list(beta_prior_obj = beta_prior("ridge")),
    list(response_likelihood = TRUE)
  )

  for (arg in reserved_args) {
    expect_error(
      rqr_tcsp_fit_univariate(
        y, 0.35, 0.60,
        scan_method = "dkw_conservative",
        fit_ecm = TRUE,
        ecm_args = arg
      ),
      "reserves"
    )
  }
})

test_that("TCSP full-range action returns engines unavailable without weakening q", {
  y <- seq(-2, 2, length.out = 100)
  fit <- rqr_tcsp_fit_univariate(
    y, 0.80, 0.68,
    scan_method = "dkw_conservative",
    fit_ecm = TRUE,
    fit_mcmc = TRUE,
    mcmc_args = list(mcmc_control = list(n_burn = 1, n_mcmc = 1)),
    ecm_args = list(ecm_control = list(max_iter = 2))
  )

  expect_s3_class(fit, "rqr_tcsp_fit")
  expect_equal(fit$contract$target_content, 1)
  expect_equal(fit$contract$retained_count, length(y))
  expect_null(fit$posterior_fit)
  expect_null(fit$ecm_fit)
  expect_false(fit$contract$posterior_fit_available)
  expect_false(fit$contract$ecm_fit_available)
  expect_match(fit$contract$engine_unavailable_reasons$ecm,
               "target_content >= 1")
  expect_match(fit$contract$engine_unavailable_reasons$posterior,
               "target_content >= 1")
})
