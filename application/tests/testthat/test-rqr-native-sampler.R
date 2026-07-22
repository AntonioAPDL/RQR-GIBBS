test_that("native GIG half sampler has the declared limiting and mean contracts", {
  set.seed(992)
  zero <- rqr_sample_gig_half(rep(0, 2000), a = 2)
  expect_true(all(is.finite(zero) & zero > 0))
  expect_equal(mean(zero), 0.5, tolerance = 0.06)

  b <- 1.5
  a <- 2.2
  draw <- rqr_sample_gig_half(rep(b, 20000), a)
  expected <- sqrt(b / a) + 1 / a
  expect_equal(mean(draw), expected, tolerance = 0.04)
})

test_that("dynamic learned-scale sampler follows the partially collapsed contract", {
  set.seed(993)
  T <- 16L
  y <- sin(seq_len(T) / 4) + rnorm(T, sd = 0.08)
  fit <- rqr_dlm_fit(
    y, rqr_polytrend(1L, C0 = 5), coverage_level = 0.8,
    evolution_mode = "fixed_W", W = 0.02,
    learning_rate_mode = "learned_scale",
    lambda_prior = list(shape = 4, rate = 3),
    mcmc_control = list(n_burn = 5, n_mcmc = 12, seed = 993, backend = "cpp")
  )
  expect_s3_class(fit, "rqr_dlm_mcmc")
  expect_true(fit$model_spec$exact_joint_target)
  expect_identical(
    fit$diagnostics$partial_collapse_order,
    c("lambda_collapsed", "latent_v_refresh", "root1_ffbs", "root2_ffbs")
  )
  expect_true(all(na.omit(fit$diagnostics$lambda_post_shape_trace) == 4 + T))
  expect_true(all(is.finite(fit$samp.lambda) & fit$samp.lambda > 0))
  interval <- predict_interval(fit)
  expect_true(all(interval$upper_draws >= interval$lower_draws))
})

test_that("native fixed-design ridge MCMC has no private exdqlm dependency", {
  set.seed(997)
  X <- cbind(1, seq(-1, 1, length.out = 12))
  y <- 0.2 + 0.3 * X[, 2] + rnorm(12, sd = 0.1)
  fit <- rqr_mcmc_fit(
    y, X, coverage_level = 0.8,
    beta_prior_obj = beta_prior("ridge", ridge = list(tau2 = 5)),
    mcmc_control = list(n_burn = 3, n_mcmc = 6, seed = 997)
  )
  expect_s3_class(fit, "rqr_mcmc")
  expect_identical(fit$model_spec$family, "rqr_fixed_design")
  expect_true(all(is.finite(fit$samp.beta_root1)))
  expect_true(all(fit$summary$upper_mean >= fit$summary$lower_mean))
})

test_that("RQR-DLM skips missing response measurements", {
  y <- c(rnorm(6), NA_real_, rnorm(5))
  fit <- rqr_dlm_fit(
    y, rqr_polytrend(1L, C0 = 2), coverage_level = 0.8,
    evolution_mode = "fixed_W", W = 0.03,
    mcmc_control = list(n_burn = 2, n_mcmc = 4, seed = 998, backend = "cpp")
  )
  expect_true(all(is.finite(fit$samp.eta_root1)))
  expect_false(fit$misc$observed[7])
})

test_that("adaptive discount is explicitly marked non-exact", {
  y <- rnorm(10)
  expect_warning(
    fit <- rqr_dlm_fit(
      y, rqr_polytrend(1L, C0 = 2), coverage_level = 0.75,
      evolution_mode = "adaptive_discount", df = 0.95, dim.df = 1L,
      mcmc_control = list(n_burn = 1, n_mcmc = 2, seed = 994, backend = "cpp")
    ),
    "experimental working/sequential"
  )
  expect_false(fit$model_spec$exact_joint_target)
})

test_that("frozen discount templates remain exact during MCMC", {
  y <- rnorm(10)
  fit <- rqr_dlm_fit(
    y, rqr_polytrend(1L, C0 = 2), coverage_level = 0.75,
    evolution_mode = "discount_template", df = 0.95, dim.df = 1L,
    reference_variance = 1,
    mcmc_control = list(n_burn = 1, n_mcmc = 3, seed = 999, backend = "cpp")
  )
  expect_true(fit$model_spec$exact_joint_target)
  expect_true(fit$evolution$frozen_before_mcmc)
  expect_identical(fit$evolution$mode, "discount_template")
  expect_identical(fit$evolution$reference_source, "user_supplied")
  expect_false(fit$evolution$empirical_bayes)
})

test_that("future root forecasting is explicit and does not simulate responses", {
  y <- rnorm(12)
  fit <- rqr_dlm_fit(
    y, rqr_polytrend(1L, C0 = 2), coverage_level = 0.8,
    evolution_mode = "fixed_W", W = 0.03,
    mcmc_control = list(n_burn = 2, n_mcmc = 4, seed = 995, backend = "cpp")
  )
  fc <- rqr_forecast_roots(fit, FF_future = matrix(1, 1, 3),
                           GG_future = 1, W_future = 0.03, seed = 996)
  expect_equal(dim(fc$lower_draws), c(3L, 4L))
  expect_true(all(fc$upper_draws >= fc$lower_draws))
  expect_match(fc$interpretation, "no response simulation")
})
