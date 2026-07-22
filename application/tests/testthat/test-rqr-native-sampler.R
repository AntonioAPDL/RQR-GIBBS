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

test_that("GIG half sampler remains finite across representable extreme scales", {
  set.seed(1202)
  grid <- expand.grid(
    a = 10^c(-300, -150, 0, 150, 300),
    b = 10^c(-300, -150, 0, 150, 300)
  )
  for (ii in seq_len(nrow(grid))) {
    draw <- rqr_sample_gig_half(rep(grid$b[ii], 200), grid$a[ii])
    expect_true(all(is.finite(draw) & draw > 0), info = paste(grid[ii, ], collapse = ","))
  }
})

test_that("learning-rate targets are locked and fixed rate means omega_R", {
  expect_error(
    rqrgibbs:::.rqr_lambda_prior(
      list(shape = 4, rate = 4, power = 0.3),
      "learned_pseudoresidual_normalized"
    ),
    "not accepted"
  )
  normalized <- rqrgibbs:::.rqr_lambda_posterior_params(
    2, 10, list(shape = 4, rate = 4), "learned_pseudoresidual_normalized"
  )
  pure <- rqrgibbs:::.rqr_lambda_posterior_params(
    2, 10, list(shape = 4, rate = 4), "learned_pure"
  )
  expect_equal(normalized$shape, 14)
  expect_equal(pure$shape, 4)

  fit <- rqr_dlm_fit(
    rnorm(8), rqr_polytrend(1L, C0 = 2), coverage_level = 0.8,
    evolution_mode = "fixed_W", W = 0.02,
    learning_rate_mode = "fixed_rate", learning_rate = 2,
    loss_reference_scale = 4,
    mcmc_control = list(n_burn = 0, n_mcmc = 2, seed = 1203, backend = "cpp")
  )
  expect_equal(fit$model_spec$effective_learning_rate, 2)
  expect_true(all(fit$samp.lambda == 8))
  expect_match(fit$model_spec$inferential_target, "omega_R")
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
    c(
      "lambda_collapsed", "latent_v_refresh", "root1_ffbs", "root2_ffbs",
      "global_root_swap"
    )
  )
  expect_true(all(na.omit(fit$diagnostics$lambda_post_shape_trace) == 4 + T))
  expect_true(all(is.finite(fit$samp.lambda) & fit$samp.lambda > 0))
  expect_identical(fit$model_spec$learning_rate_mode, "learned_pseudoresidual_normalized")
  expect_true(fit$model_spec$promotion_eligible)
  expect_equal(fit$model_spec$numerical_repair_count, 0L)
  expect_null(fit$samp.theta_root1)
  expect_equal(dim(fit$samp.theta_terminal_root1), c(1L, 12L))
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
  expect_equal(fc$diagnostics$repair_count, 0L)
  expect_error(
    rqr_forecast_roots(
      fit, FF_future = matrix(1, 1, 2), GG_future = 1,
      W_future = -0.01
    ),
    "indefinite"
  )
})

test_that("exact component scales use the analytic shared inverse-Gamma conditional", {
  evolution <- rqr_evolution_component_scale(
    templates = list(matrix(2, 1, 1)), component_dims = 1,
    prior = list(shape = 3, rate = 4), initial = 0.5,
    component_names = "level"
  )
  theta1 <- matrix(c(1, 2, 4), 1, 3)
  theta2 <- matrix(c(-1, 0, 1), 1, 3)
  posterior <- rqrgibbs:::.rqr_component_scale_posterior(
    theta1, theta2, theta01 = 0, theta02 = 0, GG = 1, evolution = evolution
  )
  innovations <- c(1, 1, 2, -1, 1, 1)
  expect_equal(posterior$shape, 3 + 3)
  expect_equal(posterior$rate, 4 + 0.5 * sum(innovations^2 / 2))

  fit <- rqr_dlm_fit(
    rnorm(10), rqr_polytrend(1L, C0 = 2), coverage_level = 0.8,
    evolution_mode = "component_scale",
    component_templates = list(matrix(1, 1, 1)),
    evolution_scale_prior = list(shape = 3, rate = 2),
    mcmc_control = list(n_burn = 1, n_mcmc = 4, seed = 1204, backend = "cpp")
  )
  expect_identical(fit$model_spec$evolution_mode, "component_scale")
  expect_true(fit$model_spec$exact_joint_target)
  expect_true(fit$model_spec$promotion_eligible)
  expect_equal(dim(fit$samp.evolution_scale), c(4L, 1L))
  expect_equal(dim(fit$samp.theta0_root1), c(1L, 4L))
  expect_true(all(is.finite(fit$samp.evolution_scale) & fit$samp.evolution_scale > 0))
})

test_that("DLM checkpoints continue with the same RNG stream", {
  y <- sin(seq_len(9) / 3)
  model <- rqr_polytrend(1L, C0 = 2)
  full <- rqr_dlm_fit(
    y, model, 0.8, evolution_mode = "fixed_W", W = 0.04,
    mcmc_control = list(n_burn = 0, n_mcmc = 6, seed = 1205, backend = "cpp")
  )
  first <- rqr_dlm_fit(
    y, model, 0.8, evolution_mode = "fixed_W", W = 0.04,
    mcmc_control = list(n_burn = 0, n_mcmc = 3, seed = 1205, backend = "cpp")
  )
  second <- rqr_dlm_continue(first, n_mcmc = 3)
  expect_identical(
    full$samp.eta_root1,
    cbind(first$samp.eta_root1, second$samp.eta_root1)
  )
  expect_identical(
    full$samp.eta_root2,
    cbind(first$samp.eta_root2, second$samp.eta_root2)
  )
  expect_equal(second$checkpoint_state$completed_iterations, 6L)
  expect_identical(second$provenance$schema_version, "rqrgibbs_fit/1.0.0")
  expect_true(nzchar(second$provenance$data_digest))
  expect_null(second$provenance$initial_seed)
  expect_true(all(c("FF", "GG", "C0", "evolution_W") %in%
                    names(first$provenance$matrix_digests)))
  expect_equal(first$provenance$initial_seed, 1205L)
  expect_identical(first$model_spec$loss_name, "rqr_residual_product_check_loss")
})

test_that("iteration controls fail with actionable scalar-integer errors", {
  expect_error(
    rqr_dlm_fit(
      rnorm(6), rqr_polytrend(1L), 0.8,
      evolution_mode = "fixed_W", W = 0.1,
      mcmc_control = list(n_burn = NA_integer_, n_mcmc = 2)
    ),
    "mcmc_control\\$n_burn"
  )
})

test_that("mathematical target status is separate from numerical repairs", {
  model <- rqr_as_dlm_model(list(
    FF = matrix(c(1, 0), 2, 1),
    GG = diag(c(1, 0)),
    m0 = c(0, 0), C0 = diag(2),
    component_dims = c(1, 1), component_names = c("level", "degenerate")
  ))
  fit <- rqr_dlm_fit(
    rnorm(6), model, 0.8,
    evolution_mode = "fixed_W", W = matrix(0, 2, 2),
    numerical_policy = "record_repair",
    mcmc_control = list(n_burn = 0, n_mcmc = 2, seed = 1206, backend = "cpp")
  )
  expect_true(fit$model_spec$exact_joint_target)
  expect_gt(fit$model_spec$numerical_repair_count, 0L)
  expect_false(fit$model_spec$numerically_exact_transition)
  expect_false(fit$model_spec$promotion_eligible)
  expect_gt(nrow(fit$diagnostics$numerical_repairs), 0L)
  expect_true(all(is.finite(fit$diagnostics$numerical_repairs$matrix_scale)))
})
