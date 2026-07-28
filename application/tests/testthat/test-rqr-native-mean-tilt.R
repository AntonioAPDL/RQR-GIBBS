test_that("mean-tilted RQR loss decomposes into ordinary product loss and linear tilt", {
  y <- c(-1, 0.25, NA_real_, 1.5)
  eta1 <- c(-1.5, -0.1, 0.2, 0.8)
  eta2 <- c(0.4, 1.2, 0.5, 2.3)
  delta <- c(0.1, -0.2, NA_real_, 0.05)
  alpha <- 0.8

  ordinary <- rqr_check_loss(
    rqr_residual_product(y[!is.na(y)], eta1[!is.na(y)], eta2[!is.na(y)]),
    alpha
  )
  detailed <- rqr_mean_tilt_loss(
    y, eta1, eta2, alpha, mean_tilt = delta, details = TRUE
  )

  expect_equal(detailed$product_loss[!is.na(y)], ordinary)
  expect_equal(detailed$product_loss[is.na(y)], 0)
  expect_equal(detailed$linear_tilt[is.na(y)], 0)
  expect_equal(detailed$total, detailed$product_loss - detailed$linear_tilt)
  expect_equal(
    rqr_mean_tilt_loss(y, eta1, eta2, alpha, mean_tilt = 0),
    rqr_mean_tilt_loss(y, eta1, eta2, alpha, mean_tilt = rep(0, length(y)))
  )
  expect_error(
    rqr_mean_tilt_loss(y, eta1, eta2, alpha, mean_tilt = c(0, 1)),
    "mean_tilt"
  )
})

test_that("fixed-design ridge MCMC records fixed nonzero mean tilt and gates unsupported targets", {
  y <- c(-1.1, -0.3, 0.2, 0.55, 1.1)
  X <- cbind(1, seq(-1, 1, length.out = length(y)))
  delta <- seq(-0.08, 0.08, length.out = length(y))

  fit <- rqr_mcmc_fit(
    y = y,
    X = X,
    coverage_level = 0.75,
    learning_rate = 0.9,
    mean_tilt = delta,
    beta_prior_obj = beta_prior("ridge", ridge = list(tau2 = 25)),
    mcmc_control = list(n_burn = 4, n_mcmc = 6, thin = 1, seed = 8201)
  )

  expect_s3_class(fit, "rqr_mcmc")
  expect_identical(fit$model_spec$mean_tilt_mode, "fixed_response_scale")
  expect_identical(fit$model_spec$loss_name, "mean_tilted_rqr_product_check_loss")
  expect_equal(fit$model_spec$mean_tilt, delta)
  expect_true(all(is.finite(fit$samp.beta_root1)))
  expect_equal(
    fit$diagnostics$mean_tilted_target_loss_trace,
    fit$diagnostics$ordinary_product_check_loss_trace -
      fit$diagnostics$tilt_linear_trace
  )

  expect_error(
    rqr_mcmc_fit(
      y = y, X = X, coverage_level = 0.75, mean_tilt = delta,
      learning_rate_mode = "learned_pseudoresidual_normalized",
      mcmc_control = list(n_burn = 0, n_mcmc = 1, seed = 8202)
    ),
    "fixed_rate"
  )
  expect_error(
    rqr_vb_fit(
      y = y, X = X, coverage_level = 0.75, mean_tilt = delta,
      vb_control = list(max_iter = 2, n_draws = 2, seed = 8203)
    ),
    "not implemented"
  )
})

test_that("zero mean tilt is an explicit no-op for fixed-design MCMC draws", {
  y <- c(-0.9, -0.4, 0.1, 0.8)
  X <- matrix(1, length(y), 1)
  ctrl <- list(n_burn = 3, n_mcmc = 5, thin = 1, seed = 8204)

  scalar_zero <- rqr_mcmc_fit(
    y, X, coverage_level = 0.8, mean_tilt = 0,
    mcmc_control = ctrl
  )
  vector_zero <- rqr_mcmc_fit(
    y, X, coverage_level = 0.8, mean_tilt = rep(0, length(y)),
    mcmc_control = ctrl
  )

  expect_equal(scalar_zero$samp.beta_root1, vector_zero$samp.beta_root1)
  expect_equal(scalar_zero$samp.beta_root2, vector_zero$samp.beta_root2)
  expect_equal(scalar_zero$samp.lambda, vector_zero$samp.lambda)
  expect_identical(scalar_zero$model_spec$mean_tilt_mode, "zero")
  expect_identical(vector_zero$model_spec$mean_tilt_mode, "zero")
  expect_identical(
    scalar_zero$model_spec$mean_tilt_digest,
    vector_zero$model_spec$mean_tilt_digest
  )
})

test_that("FFBS canonical shifts match a direct one-step Gaussian calculation", {
  z <- 0
  H <- matrix(1, 1, 1)
  V <- 1
  GG <- array(1, c(1, 1, 1))
  m0 <- 0
  C0 <- matrix(1, 1, 1)
  evolution <- list(mode = "fixed_W", W = matrix(0, 1, 1))
  shift <- matrix(2, 1, 1)

  ref <- rqr_ffbs_smooth(
    z, H, V, GG, m0, C0, evolution, backend = "R",
    canonical_shift = shift
  )
  got <- rqr_ffbs_smooth(
    z, H, V, GG, m0, C0, evolution, backend = "cpp",
    canonical_shift = shift
  )

  expect_equal(ref$filter_mean[1, 1], 1, tolerance = 1e-12)
  expect_equal(ref$filter_cov[1, 1, 1], 0.5, tolerance = 1e-12)
  expect_equal(got$filter_mean, ref$filter_mean, tolerance = 1e-12)
  expect_equal(got$smooth_mean, ref$smooth_mean, tolerance = 1e-12)
  expect_true(isTRUE(ref$diagnostics$canonical_shift))
  expect_true(isTRUE(got$diagnostics$canonical_shift))

  expect_error(
    rqr_ffbs_smooth(
      NA_real_, H, V, GG, m0, C0, evolution, backend = "R",
      canonical_shift = shift
    ),
    "missing-observation"
  )
})

test_that("RQR-DLM accepts fixed-rate mean tilt only for fixed-W and frozen-template modes", {
  y <- c(-0.8, -0.2, NA_real_, 0.4, 0.9)
  model <- rqr_polytrend(1L, C0 = 2)
  delta <- c(-0.05, -0.02, NA_real_, 0.02, 0.05)
  ctrl <- list(n_burn = 3, n_mcmc = 4, thin = 1, seed = 8205, backend = "R")

  fixed_fit <- rqr_dlm_fit(
    y = y,
    model = model,
    coverage_level = 0.8,
    evolution_mode = "fixed_W",
    W = matrix(0.03, 1, 1),
    learning_rate = 1,
    mean_tilt = delta,
    mcmc_control = ctrl
  )
  expect_s3_class(fixed_fit, "rqr_dlm_mcmc")
  expect_identical(fixed_fit$model_spec$mean_tilt_mode, "fixed_response_scale")
  expect_identical(fixed_fit$model_spec$loss_name, "mean_tilted_rqr_product_check_loss")
  expect_true(all(is.finite(fixed_fit$samp.eta_root1)))
  expect_equal(
    fixed_fit$diagnostics$mean_tilted_target_loss_trace,
    fixed_fit$diagnostics$ordinary_product_check_loss_trace -
      fixed_fit$diagnostics$tilt_linear_trace
  )

  template <- rqr_freeze_discount_template(
    model, n_time = length(y), df = 0.95, dim.df = 1L,
    reference_variance = rep(0.7, length(y))
  )
  frozen_fit <- rqr_dlm_fit(
    y = y,
    model = model,
    coverage_level = 0.8,
    evolution_mode = "discount_template",
    evolution_spec = template,
    learning_rate = 1,
    mean_tilt = delta,
    mcmc_control = ctrl
  )
  expect_s3_class(frozen_fit, "rqr_dlm_mcmc")
  expect_identical(frozen_fit$model_spec$evolution_mode, "discount_template")

  expect_error(
    rqr_dlm_fit(
      y = y, model = model, coverage_level = 0.8,
      evolution_mode = "fixed_W", W = matrix(0.03, 1, 1),
      learning_rate_mode = "learned_pseudoresidual_normalized",
      mean_tilt = delta,
      mcmc_control = ctrl
    ),
    "fixed_rate"
  )
  expect_error(
    rqr_dlm_fit(
      y = y, model = model, coverage_level = 0.8,
      evolution_mode = "adaptive_discount", df = 0.95, dim.df = 1L,
      mean_tilt = delta,
      mcmc_control = ctrl
    ),
    "fixed_W and frozen discount_template"
  )
})

test_that("RQR-DESN rejects nonzero tilt in unsupported readout modes before design fitting", {
  y <- seq(-1, 1, length.out = 6)
  delta <- rep(0.1, length(y))

  expect_error(
    rqr_desn_fit(
      y = y,
      coverage_level = 0.8,
      inference = "vb",
      mean_tilt = delta,
      fit_readout = TRUE
    ),
    "VB readouts"
  )
  expect_error(
    rqr_desn_fit(
      y = y,
      coverage_level = 0.8,
      learning_rate_mode = "learned_pseudoresidual_normalized",
      mean_tilt = delta,
      fit_readout = TRUE
    ),
    "fixed-rate RQR-DESN"
  )
  expect_error(
    rqr_desn_fit(
      y = y,
      coverage_level = 0.8,
      mean_tilt = delta,
      mcmc_args = list(mean_tilt = delta),
      fit_readout = TRUE
    ),
    "top level"
  )
})
