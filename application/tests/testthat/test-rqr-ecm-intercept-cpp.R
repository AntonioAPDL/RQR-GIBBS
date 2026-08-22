test_that("C++ intercept ECM matches the R backend on deterministic data", {
  y <- sort(c(-2.0, -1.3, -0.4, 0.2, 0.8, 1.4, 2.2, 3.0))
  X <- matrix(1, length(y), 1L, dimnames = list(NULL, "(Intercept)"))
  control <- list(
    max_iter = 60,
    stable_iterations = 1,
    tol_stationarity = 1e6,
    residual_product_floor = 1e-8,
    multistart = FALSE
  )
  fit_r <- rqr_ecm_fit(
    y, X,
    coverage_level = 0.82,
    mean_tilt = 0.1,
    ecm_control = modifyList(control, list(ecm_backend = "R"))
  )
  fit_cpp <- rqr_ecm_fit(
    y, X,
    coverage_level = 0.82,
    mean_tilt = 0.1,
    ecm_control = modifyList(control, list(ecm_backend = "cpp"))
  )

  expect_s3_class(fit_cpp, "rqr_ecm")
  expect_equal(fit_cpp$ecm_backend, "cpp")
  expect_equal(fit_cpp$model_spec$ecm_backend, "cpp")
  expect_match(fit_cpp$algorithm_variant, "cpp_intercept")
  expect_equal(fit_cpp$beta_root1, fit_r$beta_root1, tolerance = 1e-7)
  expect_equal(fit_cpp$beta_root2, fit_r$beta_root2, tolerance = 1e-7)
  expect_equal(fit_cpp$objective, fit_r$objective, tolerance = 1e-7)
  expect_true(all(diff(fit_cpp$objective_trace$objective) <= 1e-8))
})

test_that("C++ intercept ECM supports nonzero boundary-continuation tilts", {
  set.seed(6191)
  y <- sort(stats::rlnorm(40, sdlog = 0.8))
  y <- (y - mean(y)) / stats::sd(y)
  X <- matrix(1, length(y), 1L, dimnames = list(NULL, "(Intercept)"))
  q <- 1 - 1 / (2 * length(y))
  tilt <- rqr_tcsp_fractional_tilt(y, q)
  fit <- rqr_ecm_fit(
    y, X,
    coverage_level = q,
    mean_tilt = tilt$delta_raw,
    ecm_control = list(
      max_iter = 80,
      stable_iterations = 1,
      tol_stationarity = 1e6,
      residual_product_floor = 1e-8,
      ecm_backend = "cpp"
    )
  )

  pred <- predict_interval(fit, X_new = X[1L, , drop = FALSE])
  expect_true(is.finite(pred$lower))
  expect_true(is.finite(pred$upper))
  expect_gt(pred$upper, pred$lower)
  expect_equal(fit$coverage_level, q)
  expect_equal(unique(fit$mean_tilt), tilt$delta_raw)
})

test_that("C++ ECM backend rejects non-intercept designs explicitly", {
  y <- seq(-1, 1, length.out = 12)
  X <- cbind(1, seq_along(y))

  expect_error(
    rqr_ecm_fit(
      y, X,
      coverage_level = 0.8,
      ecm_control = list(ecm_backend = "cpp")
    ),
    "intercept-only"
  )
})
