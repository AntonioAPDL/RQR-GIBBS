test_that("accepted safeguarded ECM cycles do not increase exact objective", {
  set.seed(8121)
  y <- sort(rnorm(40))
  X <- matrix(1, length(y), 1L, dimnames = list(NULL, "(Intercept)"))
  fit <- rqr_ecm_fit(
    y, X, coverage_level = 0.8,
    ecm_control = list(
      max_iter = 40,
      stable_iterations = 1,
      tol_stationarity = 1e6,
      residual_product_floor = 1e-8
    )
  )

  trace <- fit$objective_trace$objective
  expect_true(all(diff(trace) <= 1e-8))
  expect_true(fit$safeguard_used)
  expect_identical(fit$response_likelihood, FALSE)
  expect_identical(fit$formal_tolerance_action, FALSE)
})

test_that("ECM default convergence flag uses operational stationarity tolerance", {
  control <- rqrgibbs:::.rqr_ecm_assert_control(list())

  expect_equal(control$tol_stationarity, 1e-3)
  expect_equal(control$stable_iterations, 2L)
  expect_false(control$fail_on_nonconvergence)
})

test_that("ECM multistart selects the smallest exact objective", {
  set.seed(8122)
  y <- c(rnorm(20, -1), rnorm(20, 1))
  X <- matrix(1, length(y), 1L, dimnames = list(NULL, "(Intercept)"))
  fit <- rqr_ecm_fit(
    y, X, 0.75,
    ecm_control = list(
      max_iter = 50,
      stable_iterations = 1,
      tol_stationarity = 1e6,
      jitter_starts = 2,
      seed = 100
    )
  )

  expect_equal(fit$objective, min(fit$multistart_summary$objective))
  expect_true(nrow(fit$multistart_summary) >= 3)
  expect_match(fit$root_label_contract, "complete roots")
})

test_that("ECM deterministic endpoints agree with direct one-dimensional optimization", {
  y <- sort(c(-1.8, -1.2, -0.7, 0.1, 0.4, 1.0, 1.8, 2.4))
  X <- matrix(1, length(y), 1L, dimnames = list(NULL, "(Intercept)"))
  q <- 0.65
  prior <- beta_prior("ridge", ridge = list(tau2 = 1e6))
  fit <- rqr_ecm_fit(
    y, X, q,
    beta_prior_obj = prior,
    ecm_control = list(
      max_iter = 80,
      stable_iterations = 1,
      tol_stationarity = 1e6,
      residual_product_floor = 1e-9
    )
  )
  constants <- rqr_constants(q, 1)
  prior_prec <- rqrgibbs:::.rqr_prior_precision(prior, list(), 1L)
  objective_fn <- function(par) {
    rqrgibbs:::.rqr_ecm_objective(
      y, X, par[[1L]], par[[2L]], constants,
      mean_tilt_observed = rep(0, length(y)),
      prior_prec1 = prior_prec,
      prior_prec2 = prior_prec
    )$total
  }
  opt <- stats::optim(
    par = c(fit$beta_root1, fit$beta_root2),
    fn = objective_fn,
    method = "Nelder-Mead",
    control = list(maxit = 1000, reltol = 1e-12)
  )

  expect_lte(fit$objective, opt$value + 1e-4)
})

test_that("ECM exposes deterministic prediction and no posterior draw method", {
  y <- seq(-2, 2, length.out = 25)
  X <- matrix(1, length(y), 1L, dimnames = list(NULL, "(Intercept)"))
  fit <- rqr_ecm_fit(
    y, X, 0.8,
    ecm_control = list(max_iter = 20, stable_iterations = 1,
                       tol_stationarity = 1e6)
  )
  pred <- predict_interval(fit, X_new = X[1:3, , drop = FALSE])

  expect_length(pred$lower, 3)
  expect_true(pred$deterministic)
  expect_error(rqr_posterior_draws(fit), "no applicable method")
})
