test_that("native GIG half sampler has the declared limiting and mean contracts", {
  set.seed(992)
  zero <- rqr_sample_gig_half(rep(0, 2000), a = 2)
  expect_true(all(is.finite(zero) & zero > 0))
  expect_lte(abs(mean(zero) - 0.5), 4 * stats::sd(zero) / sqrt(length(zero)))

  b <- 1.5
  a <- 2.2
  draw <- rqr_sample_gig_half(rep(b, 20000), a)
  expected <- sqrt(b / a) + 1 / a
  expect_lte(abs(mean(draw) - expected), 4 * stats::sd(draw) / sqrt(length(draw)))

  normalizer <- 2 * (b / a)^0.25 * besselK(sqrt(a * b), 0.5)
  cutoff <- expected
  probability <- integrate(
    function(v) v^(-0.5) * exp(-0.5 * (a * v + b / v)) / normalizer,
    lower = 0, upper = cutoff, rel.tol = 1e-10
  )$value
  empirical <- mean(draw <= cutoff)
  empirical_mcse <- sqrt(probability * (1 - probability) / length(draw))
  expect_lte(abs(empirical - probability), 4 * empirical_mcse + 1 / length(draw))
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
    "unsupported fields"
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
  expect_true(fit$model_spec$ordinary_v1_scope_eligible)
  expect_true(fit$model_spec$continuation_supported)
  expect_true(is.list(fit$continuation_history_contract))
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
  expect_true(fit$model_spec$ordinary_v1_scope_eligible)
  expect_true(fit$model_spec$continuation_supported)
  expect_true(fit$model_spec$target_numerical_eligible)
  expect_false(fit$model_spec$reproducibility_eligible)
  expect_false(fit$model_spec$promotion_eligible)
  expect_equal(fit$model_spec$numerical_repair_count, 0L)
  expect_null(fit$samp.theta_root1)
  expect_equal(dim(fit$samp.theta_terminal_root1), c(1L, 12L))
  interval <- predict_interval(fit)
  expect_true(all(interval$upper_draws >= interval$lower_draws))
})

test_that("dynamic learned_pure is executable diagnostic compatibility only", {
  fit <- rqr_dlm_fit(
    y = c(-1.1, -0.35, 0.1, 0.65, 1.2),
    model = rqr_polytrend(1L, C0 = 2),
    coverage_level = 0.8,
    evolution_mode = "fixed_W",
    W = 0.03,
    learning_rate_mode = "learned_pure",
    lambda_prior = list(shape = 4, rate = 3),
    numerical_policy = "fail",
    mcmc_control = list(
      n_burn = 0L, n_mcmc = 2L, seed = 1204L, backend = "R"
    )
  )

  expect_s3_class(fit, "rqr_dlm_mcmc")
  expect_identical(
    fit$model_spec$learning_rate_mode, "learned_pure"
  )
  expect_false(fit$model_spec$ordinary_v1_scope_eligible)
  expect_false(fit$model_spec$continuation_supported)
  expect_false(fit$model_spec$promotion_eligible)
  expect_true(fit$model_spec$exact_joint_target)
  expect_true(all(is.finite(fit$samp.lambda) & fit$samp.lambda > 0))
  expect_null(fit$continuation_history_contract)
  expect_identical(
    fit$continuation_history_digest, NA_character_
  )
  expect_error(
    rqr_dlm_continue(fit, n_mcmc = 1L),
    "diagnostic legacy target and is not continuable"
  )
  expect_error(
    rqr_dlm_continue(fit, n_mcmc = 0L),
    "diagnostic legacy target and is not continuable"
  )
})

test_that("RQR-DLM draw and fitted-interval boundaries fail closed", {
  fit <- structure(
    list(
      y = c(-0.5, NA_real_, 0.75),
      samp.eta_root1 = matrix(
        c(-1.2, -0.9, -0.6, -1.0, -0.7, -0.4),
        nrow = 3L
      ),
      samp.eta_root2 = matrix(
        c(0.4, 0.7, 1.0, 0.6, 0.9, 1.2),
        nrow = 3L
      ),
      samp.lambda = c(1.5, 2.0),
      model_spec = list(
        coverage_level = 0.8,
        evolution_mode = "fixed_W",
        target_contract = "fixed_joint_exact",
        numerical_repair_count = 0L,
        promotion_eligible = FALSE
      )
    ),
    class = c("rqr_dlm_mcmc", "rqr_fit")
  )

  set.seed(1211)
  rng_before <- .Random.seed
  all_draws <- rqr_posterior_draws(fit)
  expect_identical(.Random.seed, rng_before)
  expect_identical(
    names(all_draws),
    c("eta_root1", "eta_root2", "lambda", "index", "nd")
  )
  expect_identical(all_draws$index, 1:2)
  expect_identical(all_draws$nd, 2L)
  expect_silent(rqrgibbs:::.rqr_validate_dlm_draws(fit, all_draws))

  expect_error(
    rqr_posterior_draws(fit, seed = 1212),
    "seed must be NULL when nd is NULL"
  )
  expect_identical(.Random.seed, rng_before)
  expect_error(
    rqr_posterior_draws(fit, unsupported = TRUE),
    "unsupported arguments"
  )
  expect_error(
    rqrgibbs:::rqr_posterior_draws.rqr_dlm_mcmc(unclass(fit)),
    "Expected an rqr_dlm_mcmc"
  )
  expect_error(
    rqr_posterior_draws(fit, nd = 0L, seed = 1212),
    "nd must be one finite integer"
  )
  expect_identical(.Random.seed, rng_before)

  sampled_a <- rqr_posterior_draws(fit, nd = 3L, seed = 1213)
  sampled_b <- rqr_posterior_draws(fit, nd = 3L, seed = 1213)
  expect_identical(sampled_a, sampled_b)
  expect_equal(dim(sampled_a$eta_root1), c(3L, 3L))
  expect_length(sampled_a$lambda, 3L)
  expect_true(all(sampled_a$index %in% 1:2))
  expect_true(anyDuplicated(sampled_a$index) > 0L)
  expect_silent(rqrgibbs:::.rqr_validate_dlm_draws(fit, sampled_a))

  bad <- fit
  bad$samp.eta_root2 <- bad$samp.eta_root2[-1L, , drop = FALSE]
  expect_error(
    rqr_posterior_draws(bad),
    "matching nonempty finite plain numeric matrices"
  )
  bad <- fit
  bad$samp.eta_root1[1L, 1L] <- Inf
  expect_error(rqr_posterior_draws(bad), "finite plain numeric matrices")
  bad <- fit
  class(bad$samp.eta_root1) <- c("custom_matrix", "matrix")
  expect_error(rqr_posterior_draws(bad), "plain numeric matrices")
  bad <- fit
  bad$y <- matrix(bad$y, ncol = 1L)
  expect_error(rqr_posterior_draws(bad), "fitted response length")
  bad <- fit
  bad$y[] <- NA_real_
  expect_error(rqr_posterior_draws(bad), "fitted response length")
  bad <- fit
  bad$samp.lambda <- matrix(bad$samp.lambda, nrow = 1L)
  expect_error(rqr_posterior_draws(bad), "lambda draws")
  bad <- fit
  bad$samp.lambda[1L] <- 0
  expect_error(rqr_posterior_draws(bad), "finite, positive")

  known <- all_draws
  canonical_known <- rqrgibbs:::.rqr_validate_dlm_draws(fit, known)
  expect_identical(canonical_known, known)
  pred_known <- predict_interval(fit, draws = known)
  expect_equal(dim(pred_known$lower_draws), c(3L, 2L))
  expect_true(all(pred_known$upper_draws >= pred_known$lower_draws))
  expect_identical(pred_known$draws, known)
  expect_identical(pred_known$draw_index, 1:2)
  expect_false(pred_known$response_predictive_draws)
  expect_match(pred_known$interpretation, "no response draw")

  external <- list(
    eta_root1 = matrix(
      as.integer(c(-1, -1, -1, -2, -2, -2)),
      nrow = 3L
    ),
    eta_root2 = matrix(
      as.integer(c(1, 1, 1, 2, 2, 2)),
      nrow = 3L
    )
  )
  canonical_external <- rqrgibbs:::.rqr_validate_dlm_draws(fit, external)
  expect_identical(
    names(canonical_external),
    c("eta_root1", "eta_root2", "lambda", "index", "nd")
  )
  expect_type(canonical_external$eta_root1, "double")
  expect_null(canonical_external$lambda)
  expect_identical(
    canonical_external$index, rep(NA_integer_, 2L)
  )
  expect_identical(canonical_external$nd, 2L)
  pred_external <- predict_interval(fit, draws = external)
  expect_identical(pred_external$draws, canonical_external)
  expect_identical(
    pred_external$draw_index, rep(NA_integer_, 2L)
  )
  expect_silent(
    predict_interval(fit, draws = canonical_external)
  )

  unbound_with_metadata <- c(
    external,
    list(
      lambda = c(2.5, 3.5),
      index = rep(NA_integer_, 2L),
      nd = 2L
    )
  )
  canonical_unbound <- rqrgibbs:::.rqr_validate_dlm_draws(
    fit, unbound_with_metadata
  )
  expect_identical(
    canonical_unbound$index, rep(NA_integer_, 2L)
  )
  expect_identical(canonical_unbound$lambda, c(2.5, 3.5))

  set.seed(1214)
  rng_before_explicit <- .Random.seed
  expect_error(
    predict_interval(fit, draws = external, nd = 1L),
    "nd and seed must be NULL"
  )
  expect_error(
    predict_interval(fit, draws = external, seed = 1214),
    "nd and seed must be NULL"
  )
  expect_identical(.Random.seed, rng_before_explicit)
  expect_error(
    predict_interval(fit, draws = external, unsupported = TRUE),
    "unsupported arguments"
  )
  expect_error(
    rqrgibbs:::predict_interval.rqr_dlm_mcmc(
      unclass(fit), draws = external
    ),
    "Expected an rqr_dlm_mcmc"
  )

  invalid <- external
  invalid$typo <- TRUE
  expect_error(
    rqrgibbs:::.rqr_validate_dlm_draws(fit, invalid),
    "unsupported fields: typo"
  )
  invalid <- unname(external)
  expect_error(
    rqrgibbs:::.rqr_validate_dlm_draws(fit, invalid),
    "fully named"
  )
  invalid <- external
  names(invalid) <- rep("eta_root1", 2L)
  expect_error(
    rqrgibbs:::.rqr_validate_dlm_draws(fit, invalid),
    "duplicate fields"
  )
  invalid <- external
  class(invalid) <- "custom_draw_list"
  expect_error(
    rqrgibbs:::.rqr_validate_dlm_draws(fit, invalid),
    "plain named list"
  )
  expect_error(
    rqrgibbs:::.rqr_validate_dlm_draws(
      fit, external["eta_root1"]
    ),
    "both root-ordinate matrices"
  )
  invalid <- external
  invalid$eta_root1 <- as.data.frame(invalid$eta_root1)
  expect_error(
    predict_interval(fit, draws = invalid),
    "plain numeric matrices"
  )
  invalid <- external
  class(invalid$eta_root1) <- c("custom_matrix", "matrix")
  expect_error(
    predict_interval(fit, draws = invalid),
    "plain numeric matrices"
  )
  invalid <- external
  invalid$eta_root2[1L, 1L] <- NA_real_
  expect_error(predict_interval(fit, draws = invalid), "finite numeric")
  invalid <- external
  invalid$eta_root1 <- invalid$eta_root1[, FALSE, drop = FALSE]
  invalid$eta_root2 <- invalid$eta_root2[, FALSE, drop = FALSE]
  expect_error(predict_interval(fit, draws = invalid), "one row per fitted time")
  invalid <- external
  invalid$lambda <- c(1, -1)
  expect_error(predict_interval(fit, draws = invalid), "lambda draws")
  invalid <- external
  invalid$lambda <- matrix(c(1, 2), nrow = 1L)
  expect_error(predict_interval(fit, draws = invalid), "numeric vector")
  invalid <- external
  invalid$index <- c(1, 2)
  expect_error(predict_interval(fit, draws = invalid), "integer vector")
  invalid <- external
  invalid$index <- c(1L, NA_integer_)
  expect_error(predict_interval(fit, draws = invalid), "retained-draw range")
  invalid <- external
  invalid$index <- c(1L, 3L)
  expect_error(predict_interval(fit, draws = invalid), "retained-draw range")
  invalid <- known
  invalid$index <- c(1L, 1L)
  invalid$eta_root1 <- fit$samp.eta_root1[, c(1L, 1L), drop = FALSE]
  invalid$eta_root2 <- fit$samp.eta_root2[, c(1L, 1L), drop = FALSE]
  invalid$lambda <- fit$samp.lambda[c(1L, 1L)]
  expect_error(predict_interval(fit, draws = invalid), "must be unique")
  invalid <- known
  invalid$eta_root1[1L, 1L] <- invalid$eta_root1[1L, 1L] + 0.1
  expect_error(
    predict_interval(fit, draws = invalid),
    "do not identify the supplied DLM root-ordinate draws"
  )
  invalid <- known
  invalid$lambda[1L] <- invalid$lambda[1L] + 0.1
  expect_error(
    predict_interval(fit, draws = invalid),
    "do not identify the supplied DLM lambda draws"
  )
  invalid <- external
  invalid$nd <- 1L
  expect_error(predict_interval(fit, draws = invalid), "nd must equal")
  invalid <- external
  invalid$nd <- matrix(2L, nrow = 1L)
  expect_error(
    predict_interval(fit, draws = invalid),
    "nd must be one plain numeric scalar"
  )

  repeated <- list(
    eta_root1 = fit$samp.eta_root1[
      , c(1L, 2L, 1L), drop = FALSE
    ],
    eta_root2 = fit$samp.eta_root2[
      , c(1L, 2L, 1L), drop = FALSE
    ],
    lambda = fit$samp.lambda[c(1L, 2L, 1L)],
    index = c(1L, 2L, 1L),
    nd = 3L
  )
  expect_silent(rqrgibbs:::.rqr_validate_dlm_draws(fit, repeated))

  expect_error(print(fit, unsupported = TRUE), "unsupported arguments")
  expect_output(print(fit), "RQR dynamic MCMC fit")
  corrupt <- fit
  corrupt$samp.eta_root1[1L, 1L] <- NA_real_
  expect_error(print(corrupt), "finite plain numeric matrices")
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
  expect_identical(fit$model_spec$numerical_policy, "fail")
  expect_equal(fit$model_spec$numerical_repair_count, 0L)
  expect_true(fit$model_spec$target_numerical_eligible)
  expect_false(fit$model_spec$reproducibility_eligible)
  expect_false(fit$model_spec$promotion_eligible)
  expect_error(rqr_posterior_draws(fit, nd = 2.9), "nd")
  expect_error(rqr_posterior_draws(fit, seed = 1.5), "seed")

  repaired <- rqrgibbs:::.rqr_sample_mvnorm_precision(
    rhs = c(0, 0), precision = matrix(c(1, 1, 1, 1), 2, 2),
    jitter_ladder = c(0, 1e-8)
  )
  expect_identical(repaired$info$strategy, "cholesky_jitter")
  expect_true(all(c(
    "jitter", "relative_jitter", "min_eigenvalue", "matrix_scale",
    "clamped_eigenvalues"
  ) %in% names(repaired$info)))
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
  expect_identical(fc$draw_index, seq_len(ncol(fit$samp.eta_root1)))
  expect_identical(fc$draw_binding_status, "fit_retained_draws")
  expect_identical(
    fc$diagnostics$draw_binding_status, "fit_retained_draws"
  )
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
    mcmc_control = list(
      n_burn = 1, n_mcmc = 4, seed = 1204, backend = "cpp",
      store_state_draws = TRUE
    )
  )
  expect_identical(fit$model_spec$evolution_mode, "component_scale")
  expect_true(fit$model_spec$exact_joint_target)
  expect_true(fit$model_spec$target_numerical_eligible)
  expect_false(fit$model_spec$promotion_eligible)
  expect_equal(dim(fit$samp.evolution_scale), c(4L, 1L))
  expect_equal(
    dim(fit$samp.evolution_scale_shape),
    dim(fit$samp.evolution_scale)
  )
  expect_equal(
    dim(fit$samp.evolution_scale_rate),
    dim(fit$samp.evolution_scale)
  )
  recomputed <- lapply(seq_len(4L), function(draw) {
    rqrgibbs:::.rqr_component_scale_posterior(
      matrix(
        fit$samp.theta_root1[, , draw],
        nrow = fit$expanded_model$p
      ),
      matrix(
        fit$samp.theta_root2[, , draw],
        nrow = fit$expanded_model$p
      ),
      fit$samp.theta0_root1[, draw],
      fit$samp.theta0_root2[, draw],
      fit$expanded_model$GG,
      fit$evolution
    )
  })
  expect_equal(
    unname(fit$samp.evolution_scale_shape),
    do.call(rbind, lapply(recomputed, `[[`, "shape"))
  )
  expect_equal(
    unname(fit$samp.evolution_scale_rate),
    do.call(rbind, lapply(recomputed, `[[`, "rate"))
  )
  expect_equal(dim(fit$samp.theta0_root1), c(1L, 4L))
  expect_true(all(is.finite(fit$samp.evolution_scale) & fit$samp.evolution_scale > 0))

  forecast <- rqr_forecast_roots(
    fit, FF_future = matrix(1, 1, 2), GG_future = 1,
    component_templates_future = list(matrix(1, 1, 1)), nd = 3, seed = 1207
  )
  expect_identical(forecast$diagnostics$future_evolution_mode, "component_scale")
  expect_equal(dim(forecast$lower_draws), c(2L, 3L))
  expect_equal(
    forecast$diagnostics$component_scale_draws,
    fit$samp.evolution_scale[forecast$draw_index, , drop = FALSE]
  )
  expect_error(
    rqr_forecast_roots(
      fit, FF_future = matrix(1, 1, 1), GG_future = 1, W_future = 1,
      component_templates_future = list(matrix(1, 1, 1))
    ),
    "not both"
  )
})

test_that("source digests exclude declared local output roots only", {
  root <- tempfile("rqr-source-digest-")
  dir.create(file.path(root, "R"), recursive = TRUE)
  dir.create(file.path(root, "cache"), recursive = TRUE)
  dir.create(file.path(root, "runs"), recursive = TRUE)
  dir.create(file.path(root, "src"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  writeLines("source", file.path(root, "R", "source.R"))
  writeLines("cache-a", file.path(root, "cache", "fit.rds"))
  writeLines("run-a", file.path(root, "runs", "status.csv"))
  writeLines("binary-a", file.path(root, "src", "rqrgibbs.so"))
  exclusions <- c("cache", "runs")
  initial <- rqrgibbs:::.rqr_directory_digest(
    root, exclude_relative = exclusions
  )
  writeLines("cache-b", file.path(root, "cache", "fit.rds"))
  writeLines("run-b", file.path(root, "runs", "status.csv"))
  expect_identical(
    rqrgibbs:::.rqr_directory_digest(
      root, exclude_relative = exclusions
    ),
    initial
  )
  writeLines("binary-b", file.path(root, "src", "rqrgibbs.so"))
  expect_false(identical(
    rqrgibbs:::.rqr_directory_digest(
      root, exclude_relative = exclusions
    ),
    initial
  ))
})

test_that("component-scale interweaving is an exact noncentered reparameterization", {
  evolution <- rqr_evolution_component_scale(
    templates = list(
      diag(c(1, 2)),
      array(c(1, 0.5), dim = c(1, 1, 2))
    ),
    component_dims = c(2, 1),
    prior = list(shape = c(2.5, 3), rate = c(0.1, 0.2)),
    initial = c(0.2, 0.4),
    component_names = c("trend", "regression")
  )
  GG <- array(rep(diag(3), 2), c(3, 3, 2))
  GG[1, 2, ] <- 1
  theta0 <- c(0.2, -0.1, 0.3)
  q <- c(0.2, 0.4)
  standardized <- matrix(
    c(0.2, -0.5, 0.7, 1.1, -0.3, 0.4),
    nrow = 3
  )
  theta <- rqrgibbs:::.rqr_reconstruct_component_path(
    standardized, theta0, GG, evolution, q
  )
  recovered <- rqrgibbs:::.rqr_component_noncentered_innovations(
    theta, theta0, GG, evolution, q
  )
  expect_equal(recovered, standardized, tolerance = 1e-13)
  expect_equal(
    rqrgibbs:::.rqr_reconstruct_component_path(
      recovered, theta0, GG, evolution, q
    ),
    theta,
    tolerance = 1e-13
  )
  expect_equal(
    rqrgibbs:::.rqr_component_path_from_basis(
      rqrgibbs:::.rqr_component_path_basis(
        recovered, theta0, GG, evolution
      ),
      q
    ),
    theta,
    tolerance = 1e-13
  )
  native_basis <- rqrgibbs:::rqr_noncentered_basis_cpp(
    theta, theta0, GG, as.integer(evolution$component_dims), q
  )
  expect_equal(native_basis$standardized, recovered, tolerance = 1e-13)
  expect_equal(
    native_basis$baseline,
    rqrgibbs:::.rqr_component_path_basis(
      recovered, theta0, GG, evolution
    )$baseline,
    tolerance = 1e-13
  )
  expect_equal(
    native_basis$basis,
    rqrgibbs:::.rqr_component_path_basis(
      recovered, theta0, GG, evolution
    )$basis,
    tolerance = 1e-13
  )

  path_basis <- list(
    baseline = native_basis$baseline,
    basis = native_basis$basis
  )
  FF <- matrix(c(1, 0.2, -0.1, 1, 0.3, 0.4), nrow = 3)
  y <- c(-0.3, 0.8)
  observed <- c(TRUE, TRUE)
  v <- c(0.7, 1.1)
  xi <- -0.4
  obs_variance <- c(0.8, 1.3)
  ordinate_basis <- rqrgibbs:::.rqr_component_ordinate_basis(
    FF, path_basis
  )
  expect_equal(
    as.numeric(
      ordinate_basis$baseline +
        ordinate_basis$basis %*% sqrt(q)
    ),
    colSums(FF * theta),
    tolerance = 1e-13
  )
  log_density <- function(candidate_q) {
    rqrgibbs:::.rqr_component_noncentered_log_density(
      log(candidate_q), ordinate_basis, ordinate_basis, y, observed,
      v, xi, obs_variance, evolution
    )
  }
  original_transformed_density <- function(candidate_q) {
    candidate_theta <- rqrgibbs:::.rqr_component_path_from_basis(
      path_basis, candidate_q
    )
    eta <- colSums(FF * candidate_theta)
    residual <- rqr_residual_product(y, eta, eta) - xi * v
    prior <- sum(
      -(evolution$prior$shape + 1) * log(candidate_q) -
        evolution$prior$rate / candidate_q
    )
    evolution_scale_terms <- -sum(
      2 * ncol(candidate_theta) *
        evolution$component_dims / 2 * log(candidate_q)
    )
    state_jacobian <- sum(
      ncol(candidate_theta) *
        evolution$component_dims * log(candidate_q)
    )
    log_scale_jacobian <- sum(log(candidate_q))
    prior + evolution_scale_terms + state_jacobian +
      log_scale_jacobian -
      0.5 * sum(residual^2 / obs_variance)
  }
  candidate_a <- c(0.15, 0.8)
  candidate_b <- c(0.6, 0.25)
  expect_equal(
    log_density(candidate_a) - log_density(candidate_b),
    original_transformed_density(candidate_a) -
      original_transformed_density(candidate_b),
    tolerance = 1e-12
  )

  set.seed(1221)
  draws <- numeric(4000)
  current <- 0
  for (index in seq_along(draws)) {
    current <- rqrgibbs:::.rqr_slice_log_coordinate(
      current, function(value) -0.5 * value^2,
      width = 1, max_steps = 100L, max_shrink = 1000L
    )$value
    draws[[index]] <- current
  }
  draws <- tail(draws, 3000)
  expect_lt(abs(mean(draws)), 0.08)
  expect_lt(abs(stats::sd(draws) - 1), 0.08)

  fit <- rqr_dlm_fit(
    y = c(-1.2, -0.4, 0.1, 0.8, 1.4),
    model = rqr_polytrend(1L, C0 = 2),
    coverage_level = 0.8,
    evolution_mode = "component_scale",
    component_templates = list(matrix(1, 1, 1)),
    evolution_scale_prior = list(shape = 3, rate = 0.2),
    numerical_policy = "fail",
    mcmc_control = list(
      n_burn = 2, n_mcmc = 5, seed = 1222,
      backend = "cpp", store_state_draws = TRUE,
      component_scale_interweave = TRUE,
      component_scale_interweave_cycles = 2L,
      component_scale_slice_sweeps = 1L
    )
  )
  expect_true(fit$misc$component_scale_interweave)
  expect_identical(
    fit$diagnostics$partial_collapse_order,
    c(
      "lambda_collapsed", "latent_v_refresh", "root1_ffbs",
      "root1_time0", "root2_ffbs", "root2_time0",
      "component_scale_centered_noncentered_cycles_2",
      "global_root_swap"
    )
  )
  expect_identical(
    nrow(fit$diagnostics$component_scale_interweave),
    14L
  )
  expect_true(all(
    fit$diagnostics$component_scale_interweave$
      exact_noncentered_slice
  ))
  expect_true(all(
    fit$diagnostics$component_scale_interweave$
      sweeps_per_cycle == 1L
  ))
  expect_identical(
    fit$diagnostics$component_scale_interweave$cycle,
    rep(1:2, 7L)
  )
  recomputed <- lapply(seq_len(5L), function(draw) {
    rqrgibbs:::.rqr_component_scale_posterior(
      matrix(
        fit$samp.theta_root1[, , draw],
        nrow = fit$expanded_model$p
      ),
      matrix(
        fit$samp.theta_root2[, , draw],
        nrow = fit$expanded_model$p
      ),
      fit$samp.theta0_root1[, draw],
      fit$samp.theta0_root2[, draw],
      fit$expanded_model$GG,
      fit$evolution
    )
  })
  expect_equal(
    unname(fit$samp.evolution_scale_shape),
    do.call(rbind, lapply(recomputed, `[[`, "shape"))
  )
  expect_equal(
    unname(fit$samp.evolution_scale_rate),
    do.call(rbind, lapply(recomputed, `[[`, "rate"))
  )
  continued <- rqr_dlm_continue(fit, n_mcmc = 2)
  expect_true(continued$misc$component_scale_interweave)
  uninterrupted <- rqr_dlm_fit(
    y = c(-1.2, -0.4, 0.1, 0.8, 1.4),
    model = rqr_polytrend(1L, C0 = 2),
    coverage_level = 0.8,
    evolution_mode = "component_scale",
    component_templates = list(matrix(1, 1, 1)),
    evolution_scale_prior = list(shape = 3, rate = 0.2),
    numerical_policy = "fail",
    mcmc_control = list(
      n_burn = 2, n_mcmc = 7, seed = 1222,
      backend = "cpp", store_state_draws = TRUE,
      component_scale_interweave = TRUE,
      component_scale_interweave_cycles = 2L,
      component_scale_slice_sweeps = 1L
    )
  )
  expect_identical(
    continued$samp.eta_root1,
    uninterrupted$samp.eta_root1[, 6:7, drop = FALSE]
  )
  expect_identical(
    continued$samp.eta_root2,
    uninterrupted$samp.eta_root2[, 6:7, drop = FALSE]
  )
  expect_identical(
    continued$samp.evolution_scale,
    uninterrupted$samp.evolution_scale[6:7, , drop = FALSE]
  )
  expect_identical(
    continued$samp.theta_root1,
    uninterrupted$samp.theta_root1[, , 6:7, drop = FALSE]
  )
  expect_identical(
    continued$samp.theta_root2,
    uninterrupted$samp.theta_root2[, , 6:7, drop = FALSE]
  )
  expect_identical(
    continued$checkpoint_state$theta_root1,
    uninterrupted$checkpoint_state$theta_root1
  )
  expect_identical(
    continued$checkpoint_state$theta_root2,
    uninterrupted$checkpoint_state$theta_root2
  )
  expect_identical(
    continued$checkpoint_state$evolution_scale,
    uninterrupted$checkpoint_state$evolution_scale
  )
  expect_identical(
    continued$checkpoint_state$rng_state,
    uninterrupted$checkpoint_state$rng_state
  )
})

test_that("fixed-W state storage completes retained paths at time zero", {
  fit <- rqr_dlm_fit(
    y = c(-1, -0.2, 0.5, 1.1),
    model = rqr_polytrend(1L, C0 = 2),
    coverage_level = 0.8,
    evolution_mode = "fixed_W",
    W = 0.05,
    numerical_policy = "fail",
    mcmc_control = list(
      n_burn = 1, n_mcmc = 4, seed = 1208,
      backend = "cpp", store_state_draws = TRUE
    )
  )
  expect_identical(dim(fit$samp.theta0_root1), c(1L, 4L))
  expect_identical(dim(fit$samp.theta0_root2), c(1L, 4L))
  expect_true(all(is.finite(fit$samp.theta0_root1)))
  expect_true(all(is.finite(fit$samp.theta0_root2)))
})

test_that("time-zero completion supports an exact singular transition", {
  set.seed(1209)
  draw <- rqrgibbs:::.rqr_draw_initial_state(
    theta1 = c(1, 0),
    G1 = diag(c(1, 0)),
    m0 = c(0, 0),
    C0 = diag(2),
    W1 = matrix(0, 2, 2)
  )
  expect_equal(draw[1L], 1, tolerance = 1e-12)
  expect_true(all(is.finite(draw)))
})

test_that("component-scale root forecasts match analytic state moments", {
  n_save <- 1L
  q <- 0.4
  terminal <- 2
  fixture <- structure(list(
    samp.theta_terminal_root1 = matrix(terminal, 1, n_save),
    samp.theta_terminal_root2 = matrix(terminal, 1, n_save),
    samp.evolution_scale = matrix(
      q, n_save, 1, dimnames = list(NULL, "level")
    ),
    evolution = rqr_evolution_component_scale(
      templates = list(matrix(1, 1, 1)),
      component_dims = 1L,
      prior = list(shape = 2, rate = 1),
      initial = 1,
      component_names = "level"
    ),
    model_spec = list(
      evolution_mode = "component_scale",
      numerical_policy = "fail"
    ),
    misc = list(jitter_ladder = 0)
  ), class = c("rqr_dlm_mcmc", "rqr_fit"))

  n_draw <- 4000L
  H <- 3L
  forecast <- rqr_forecast_roots(
    fixture,
    FF_future = matrix(1, 1, H),
    GG_future = 1,
    component_templates_future = list(matrix(1, 1, 1)),
    nd = n_draw,
    seed = 1210
  )
  analytic_mean <- rep(terminal, H)
  analytic_variance <- seq_len(H) * q
  empirical_mean <- rowMeans(forecast$eta_root1)
  empirical_variance <- apply(forecast$eta_root1, 1L, stats::var)
  mean_mcse <- sqrt(analytic_variance / n_draw)
  variance_mcse <- sqrt(2 * analytic_variance^2 / (n_draw - 1L))
  expect_lte(max(abs(empirical_mean - analytic_mean) / mean_mcse), 5)
  expect_lte(max(abs(empirical_variance - analytic_variance) / variance_mcse), 5)
  expect_equal(
    forecast$diagnostics$component_scale_draws,
    matrix(q, n_draw, 1, dimnames = list(NULL, "level"))
  )
  expect_equal(forecast$diagnostics$repair_count, 0L)
  expect_identical(
    forecast$draw_binding_status, "unbound_external_state_fixture"
  )
})

test_that("all-draw component forecasts preserve varying saved scale rows", {
  n_save <- 2000L
  q <- rep(c(0.1, 0.9), each = n_save / 2L)
  fixture <- structure(list(
    samp.theta_terminal_root1 = matrix(0, 1L, n_save),
    samp.theta_terminal_root2 = matrix(0, 1L, n_save),
    samp.evolution_scale = matrix(
      q, n_save, 1L, dimnames = list(NULL, "level")
    ),
    evolution = rqr_evolution_component_scale(
      templates = list(matrix(1, 1, 1)),
      component_dims = 1L,
      prior = list(shape = 2, rate = 1),
      initial = 1,
      component_names = "level"
    ),
    model_spec = list(
      evolution_mode = "component_scale",
      numerical_policy = "fail"
    ),
    misc = list(jitter_ladder = 0)
  ), class = c("rqr_dlm_mcmc", "rqr_fit"))
  forecast <- rqr_forecast_roots(
    fixture,
    FF_future = matrix(1, 1L, 1L),
    GG_future = 1,
    component_templates_future = list(matrix(1, 1, 1)),
    nd = NULL,
    seed = 1212
  )
  expect_identical(forecast$draw_index, seq_len(n_save))
  expect_identical(
    unname(forecast$diagnostics$component_scale_draws[, 1L]),
    q
  )
  for (value in unique(q)) {
    draws <- forecast$eta_root1[, q == value, drop = FALSE]
    empirical_variance <- stats::var(as.numeric(draws))
    variance_mcse <- sqrt(
      2 * value^2 / (length(draws) - 1L)
    )
    expect_lte(
      abs(empirical_variance - value) / variance_mcse,
      6
    )
  }
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
  expect_identical(second$provenance$schema_version, "rqrgibbs_fit/1.11.0")
  expect_true(nzchar(second$provenance$data_digest))
  expect_null(second$provenance$initial_seed)
  expect_true(all(c("FF", "GG", "C0", "evolution_W") %in%
                    names(first$provenance$matrix_digests)))
  expect_identical(
    names(first$provenance$object_digests),
    c("model", "target", "evolution")
  )
  expect_identical(
    first$checkpoint_digest,
    rqrgibbs:::.rqr_digest(first$checkpoint_state)
  )
  expect_error(
    rqrgibbs:::.rqr_restore_rng(c(10403.5, 1)),
    "complete integer"
  )
  expect_error(
    rqrgibbs:::.rqr_restore_rng(c(10403, Inf)),
    "complete integer"
  )
  truncated_rng <- first$checkpoint_state$rng_state[
    -length(first$checkpoint_state$rng_state)
  ]
  expect_error(
    rqrgibbs:::.rqr_restore_rng(truncated_rng),
    "complete integer"
  )
  unknown_rng <- first$checkpoint_state$rng_state
  unknown_rng[1L] <- 10499L
  expect_error(
    rqrgibbs:::.rqr_restore_rng(unknown_rng),
    "complete integer"
  )
  expect_equal(first$provenance$initial_seed, 1205L)
  expect_identical(first$model_spec$loss_name, "rqr_residual_product_check_loss")
  expect_true(second$continuation_contract$continued_from_checkpoint)
  expect_false(second$continuation_contract$bitwise_continuation_claim)
  expect_identical(second$provenance$backend_requested, "cpp")
  expect_identical(second$provenance$backend_resolved, "cpp")
  expect_false(second$continuation_contract$environment_override_used)
  expect_identical(
    second$continuation_contract$parent_checkpoint_digest,
    first$checkpoint_digest
  )

  altered_data <- first
  altered_data$y[1] <- altered_data$y[1] + 1
  expect_error(rqr_dlm_continue(altered_data, 1), "data digest")
  altered_schema <- first
  altered_schema$checkpoint_state$schema_version <- "rqrgibbs_fit/0.0.0"
  expect_error(rqr_dlm_continue(altered_schema, 1), "requires schema")
  altered_environment <- first
  altered_environment$provenance$package_version <- "0.0.0"
  expect_error(rqr_dlm_continue(altered_environment, 1), "environment differs")
  expect_warning(
    portable <- rqr_dlm_continue(
      altered_environment, 1, allow_environment_mismatch = TRUE
    ),
    "not claimed"
  )
  expect_true(portable$continuation_contract$environment_override_used)
  expect_false(portable$continuation_contract$bitwise_continuation_claim)
  expect_true("package_version" %in%
                portable$continuation_contract$environment_mismatches)
  expect_false(portable$model_spec$reproducibility_eligible)
  expect_false(portable$model_spec$promotion_eligible)
})

test_that("continuation inherits numerical and source history cumulatively", {
  skip_if(Sys.which("git") == "", "git is required for provenance fixtures")
  primary <- tempfile("rqr-primary-")
  dir.create(primary)
  system2("git", c("-C", primary, "init", "--quiet"))
  system2("git", c("-C", primary, "config", "user.email", "test@example.org"))
  system2("git", c("-C", primary, "config", "user.name", "RQR Test"))
  writeLines("fixture", file.path(primary, "fixture.txt"))
  system2("git", c("-C", primary, "add", "fixture.txt"))
  system2("git", c("-C", primary, "commit", "--quiet", "-m", "fixture"))
  commit <- trimws(system2(
    "git", c("-C", primary, "rev-parse", "HEAD"), stdout = TRUE
  )[1L])

  fit <- rqr_dlm_fit(
    y = c(-1, -0.5, 0, 0.5, 1),
    model = rqr_polytrend(1L, C0 = 2),
    coverage_level = 0.8,
    evolution_mode = "fixed_W",
    W = 0.05,
    numerical_policy = "fail",
    provenance_control = list(
      repo_root = primary, expected_git_commit = commit
    ),
    mcmc_control = list(
      n_burn = 0, n_mcmc = 2, seed = 1220, backend = "auto"
    )
  )
  expect_false(fit$model_spec$promotion_eligible)
  expect_true(fit$provenance$primary_repository$require_isolated_runtime)
  expect_false(fit$provenance$primary_runtime_source_match)
  expect_identical(fit$provenance$backend_requested, "auto")
  expect_identical(fit$provenance$backend_resolved, "cpp")

  altered_history <- fit
  altered_history$model_spec$cumulative_numerical_repair_count <- 1L
  expect_error(
    rqr_dlm_continue(altered_history, n_mcmc = 1),
    "history contract"
  )

  child <- rqr_dlm_continue(fit, n_mcmc = 1)
  grandchild <- rqr_dlm_continue(child, n_mcmc = 1)
  expect_identical(grandchild$continuation_history_contract$generation, 2L)
  expect_length(grandchild$continuation_history_contract$segments, 3L)
  expect_identical(
    grandchild$continuation_history_digest,
    rqrgibbs:::.rqr_digest(grandchild$continuation_history_contract)
  )
  altered_generation0 <- grandchild
  altered_generation0$continuation_history_contract$segments[[1L]]$
    cumulative_numerical_repair_count <- 1L
  altered_generation0$continuation_history_digest <- rqrgibbs:::.rqr_digest(
    altered_generation0$continuation_history_contract
  )
  expect_error(
    rqr_dlm_continue(altered_generation0, n_mcmc = 1),
    "violates cumulative recursion"
  )
  altered_generation1 <- grandchild
  altered_generation1$continuation_history_contract$segments[[2L]]$
    parent_checkpoint_digest <- paste(rep("0", 64), collapse = "")
  altered_generation1$continuation_history_digest <- rqrgibbs:::.rqr_digest(
    altered_generation1$continuation_history_contract
  )
  expect_error(
    rqr_dlm_continue(altered_generation1, n_mcmc = 1),
    "structurally invalid"
  )
  aggregate_fields <- c(
    "chain_history_numerically_exact", "target_numerical_eligible",
    "promotion_eligible", "reproducibility_eligible",
    "cumulative_environment_override_used"
  )
  invalid_aggregate_values <- list(1, NA, c(TRUE, FALSE))
  for (field in aggregate_fields) {
    for (value in invalid_aggregate_values) {
      invalid_aggregate <- grandchild
      invalid_aggregate$continuation_history_contract[[field]] <- value
      invalid_aggregate$continuation_history_digest <-
        rqrgibbs:::.rqr_digest(
          invalid_aggregate$continuation_history_contract
        )
      expect_error(
        rqr_dlm_continue(invalid_aggregate, n_mcmc = 1),
        "aggregate statuses",
        info = field
      )
    }
  }

  impossible_repairs <- grandchild
  impossible_repairs$continuation_history_contract$segments[[1L]]$
    segment_numerical_repair_count <- 1L
  for (index in seq_along(
      impossible_repairs$continuation_history_contract$segments
    )) {
    impossible_repairs$continuation_history_contract$segments[[index]]$
      cumulative_numerical_repair_count <- 1L
  }
  impossible_repairs$continuation_history_contract$
    cumulative_numerical_repair_count <- 1L
  impossible_repairs$model_spec$cumulative_numerical_repair_count <- 1L
  impossible_repairs$continuation_history_digest <- rqrgibbs:::.rqr_digest(
    impossible_repairs$continuation_history_contract
  )
  expect_error(
    rqr_dlm_continue(impossible_repairs, n_mcmc = 1),
    "derived-status semantics"
  )

  impossible_mismatch <- grandchild
  impossible_mismatch$continuation_history_contract$segments[[1L]]$
    environment_mismatches <- "package_version"
  impossible_mismatch$continuation_history_contract$
    cumulative_environment_mismatch_ledger <- list(list(
      generation = 0L,
      checkpoint_digest =
        impossible_mismatch$continuation_history_contract$
          segments[[1L]]$checkpoint_digest,
      environment_mismatches = "package_version",
      environment_override_used = FALSE
    ))
  impossible_mismatch$continuation_history_digest <- rqrgibbs:::.rqr_digest(
    impossible_mismatch$continuation_history_contract
  )
  expect_error(
    rqr_dlm_continue(impossible_mismatch, n_mcmc = 1),
    "derived-status semantics"
  )

  impossible_backend <- grandchild
  impossible_backend$continuation_history_contract$segments[[2L]]$
    backend_resolved <- "R"
  impossible_backend$continuation_history_contract$segments[[2L]]$
    backend_changed <- TRUE
  impossible_backend$continuation_history_contract$segments[[3L]]$
    parent_backend_resolved <- "R"
  impossible_backend$continuation_history_contract$segments[[3L]]$
    backend_changed <- TRUE
  impossible_backend$continuation_history_digest <- rqrgibbs:::.rqr_digest(
    impossible_backend$continuation_history_contract
  )
  expect_error(
    rqr_dlm_continue(impossible_backend, n_mcmc = 1),
    "derived-status semantics"
  )

  impossible_target <- grandchild
  impossible_target$continuation_history_contract$segments[[1L]]$
    segment_exact_joint_target <- FALSE
  impossible_target$continuation_history_contract$segments[[1L]]$
    segment_target_numerical_eligible <- FALSE
  for (index in seq_along(
      impossible_target$continuation_history_contract$segments
    )) {
    impossible_target$continuation_history_contract$segments[[index]]$
      target_numerical_eligible <- FALSE
    impossible_target$continuation_history_contract$segments[[index]]$
      promotion_eligible <- FALSE
  }
  impossible_target$continuation_history_contract$
    target_numerical_eligible <- FALSE
  impossible_target$continuation_history_contract$promotion_eligible <- FALSE
  impossible_target$model_spec$target_numerical_eligible <- FALSE
  impossible_target$model_spec$promotion_eligible <- FALSE
  impossible_target$continuation_history_digest <- rqrgibbs:::.rqr_digest(
    impossible_target$continuation_history_contract
  )
  expect_error(
    rqr_dlm_continue(impossible_target, n_mcmc = 1),
    "conflicts with redundant fit metadata"
  )

  invalid_counts <- c(0.5, -0.5, Inf, .Machine$integer.max + 1)
  for (generation_index in 1:2) {
    for (field in c(
        "generation", "segment_numerical_repair_count",
        "cumulative_numerical_repair_count"
      )) {
      for (value in invalid_counts) {
        invalid <- grandchild
        invalid$continuation_history_contract$
          segments[[generation_index]][[field]] <- value
        invalid$continuation_history_digest <- rqrgibbs:::.rqr_digest(
          invalid$continuation_history_contract
        )
        expect_error(
          rqr_dlm_continue(invalid, n_mcmc = 1),
          "structurally invalid|cumulative recursion",
          info = paste(generation_index - 1L, field, value)
        )
      }
    }
  }
  invalid_completed <- grandchild
  invalid_completed$checkpoint_state$completed_iterations <- 0.5
  invalid_completed$checkpoint_digest <- rqrgibbs:::.rqr_digest(
    invalid_completed$checkpoint_state
  )
  final_segment <- length(
    invalid_completed$continuation_history_contract$segments
  )
  invalid_completed$continuation_history_contract$
    segments[[final_segment]]$checkpoint_digest <-
      invalid_completed$checkpoint_digest
  invalid_completed$continuation_history_digest <- rqrgibbs:::.rqr_digest(
    invalid_completed$continuation_history_contract
  )
  expect_error(
    rqr_dlm_continue(invalid_completed, n_mcmc = 1),
    "completed_iterations"
  )

  altered_source <- fit
  altered_source$provenance$git_status_available <- FALSE
  expect_error(
    rqr_dlm_continue(altered_source, n_mcmc = 1),
    "environment differs"
  )
  portable_source <- suppressWarnings(
    rqr_dlm_continue(
      altered_source, n_mcmc = 1, allow_environment_mismatch = TRUE
    )
  )
  expect_false(portable_source$continuation_contract$bitwise_continuation_claim)
  expect_false(portable_source$provenance$reproducibility_eligible)
  expect_false(portable_source$model_spec$promotion_eligible)

  altered_backend <- fit
  altered_backend$provenance$backend_resolved <- "R"
  expect_error(
    rqr_dlm_continue(altered_backend, n_mcmc = 1),
    "history contract"
  )
})

test_that("DLM continuation rejects every target and checkpoint mutation", {
  y <- cos(seq_len(7) / 4)
  model <- rqr_as_dlm_model(list(
    FF = matrix(1, 1, 1), GG = matrix(1, 1, 1), m0 = 0, C0 = 2,
    component_dims = 1L, component_names = "level"
  ))
  fit <- rqr_dlm_fit(
    y, model, 0.8,
    evolution_mode = "component_scale",
    component_templates = list(matrix(1, 1, 1)),
    evolution_scale_prior = list(shape = 3, rate = 2),
    learning_rate_mode = "learned_pseudoresidual_normalized",
    lambda_prior = list(shape = 4, rate = 5),
    mcmc_control = list(
      n_burn = 0, n_mcmc = 2, seed = 1211, backend = "cpp",
      store_state_draws = TRUE, store_latent_draws = TRUE
    )
  )

  target_mutations <- list(
    m0 = function(x) {
      x$model$m0[1] <- x$model$m0[1] + 1
      x
    },
    component_name = function(x) {
      x$model$component_names[1] <- "changed"
      x
    },
    coverage = function(x) {
      x$model_spec$coverage_level <- 0.7
      x
    },
    loss_scale = function(x) {
      x$model_spec$loss_reference_scale <- 2
      x
    },
    lambda_prior = function(x) {
      x$model_spec$lambda_prior$rate <- x$model_spec$lambda_prior$rate + 1
      x
    },
    evolution_prior = function(x) {
      x$evolution$prior$rate[1] <- x$evolution$prior$rate[1] + 1
      x
    },
    numerical_ladder = function(x) {
      x$misc$jitter_ladder <- c(0, 1e-9)
      x
    }
  )
  for (name in names(target_mutations)) {
    altered <- target_mutations[[name]](fit)
    expect_error(
      rqr_dlm_continue(altered, 1),
      "model, target, or evolution digest",
      info = name
    )
  }
  altered_kernel <- fit
  altered_kernel$misc$component_scale_interweave <- TRUE
  expect_error(
    rqr_dlm_continue(altered_kernel, 1),
    "transition-kernel contract"
  )

  checkpoint_mutations <- list(
    root1 = function(x) {
      x$checkpoint_state$theta_root1[1, 1] <-
        x$checkpoint_state$theta_root1[1, 1] + 1
      x
    },
    root2 = function(x) {
      x$checkpoint_state$theta_root2[1, 1] <-
        x$checkpoint_state$theta_root2[1, 1] + 1
      x
    },
    latent_v = function(x) {
      x$checkpoint_state$latent_v[1] <-
        x$checkpoint_state$latent_v[1] + 1
      x
    },
    lambda = function(x) {
      x$checkpoint_state$lambda <- x$checkpoint_state$lambda + 1
      x
    },
    component_scale = function(x) {
      x$checkpoint_state$evolution_scale[1] <-
        x$checkpoint_state$evolution_scale[1] + 1
      x
    },
    theta0_root1 = function(x) {
      x$checkpoint_state$theta0_root1[1] <-
        x$checkpoint_state$theta0_root1[1] + 1
      x
    },
    theta0_root2 = function(x) {
      x$checkpoint_state$theta0_root2[1] <-
        x$checkpoint_state$theta0_root2[1] + 1
      x
    },
    iteration = function(x) {
      x$checkpoint_state$completed_iterations <-
        x$checkpoint_state$completed_iterations + 1L
      x
    },
    rng = function(x) {
      set.seed(999)
      x$checkpoint_state$rng_state <- .Random.seed
      x
    },
    transition_kernel = function(x) {
      x$checkpoint_state$transition_kernel$
        noncentered_slice_interweave <- TRUE
      x
    }
  )
  for (name in names(checkpoint_mutations)) {
    altered <- checkpoint_mutations[[name]](fit)
    expect_error(
      rqr_dlm_continue(altered, 1),
      "checkpoint digest",
      info = name
    )
  }

  history_mutations <- list(
    cumulative_repairs = function(x) {
      x$continuation_history_contract$
        cumulative_numerical_repair_count <- 99L
      x
    },
    promotion = function(x) {
      x$continuation_history_contract$promotion_eligible <-
        !x$continuation_history_contract$promotion_eligible
      x
    },
    mismatch_ledger = function(x) {
      x$continuation_history_contract$
        cumulative_environment_mismatch_ledger <- list(list(
          generation = 0L, environment_mismatches = "fabricated"
        ))
      x
    },
    stored_digest = function(x) {
      x$continuation_history_digest <- paste(rep("0", 64), collapse = "")
      x
    }
  )
  for (name in names(history_mutations)) {
    altered <- history_mutations[[name]](fit)
    expect_error(
      rqr_dlm_continue(altered, 1),
      "history contract or digest",
      info = name
    )
  }

  envelope_mutations <- list(
    last_alias = function(x) {
      x$last$lambda <- x$last$lambda + 1
      x
    },
    retained_root = function(x) {
      x$samp.eta_root1[1L, ncol(x$samp.eta_root1)] <-
        x$samp.eta_root1[1L, ncol(x$samp.eta_root1)] + 1
      x
    },
    retained_terminal_state = function(x) {
      x$samp.theta_terminal_root1[
        1L, ncol(x$samp.theta_terminal_root1)
      ] <- x$samp.theta_terminal_root1[
        1L, ncol(x$samp.theta_terminal_root1)
      ] + 1
      x
    },
    retained_full_state = function(x) {
      index <- dim(x$samp.theta_root1)[3L]
      x$samp.theta_root1[1L, 1L, index] <-
        x$samp.theta_root1[1L, 1L, index] + 1
      x
    },
    retained_lambda = function(x) {
      x$samp.lambda[length(x$samp.lambda)] <-
        x$samp.lambda[length(x$samp.lambda)] + 1
      x
    },
    retained_latent = function(x) {
      x$samp.latent_v[1L, ncol(x$samp.latent_v)] <-
        x$samp.latent_v[1L, ncol(x$samp.latent_v)] + 1
      x
    },
    retained_scale = function(x) {
      x$samp.evolution_scale[
        nrow(x$samp.evolution_scale), 1L
      ] <- x$samp.evolution_scale[
        nrow(x$samp.evolution_scale), 1L
      ] + 1
      x
    },
    scale_shape_nonpositive = function(x) {
      x$samp.evolution_scale_shape[1L, 1L] <- 0
      x
    },
    scale_rate_names = function(x) {
      colnames(x$samp.evolution_scale_rate) <- "wrong"
      x
    },
    one_sided_time0 = function(x) {
      x$samp.theta0_root2 <- NULL
      x
    },
    schedule_digest = function(x) {
      x$segment_schedule_digest <- strrep("0", 64L)
      x
    },
    schedule_arithmetic = function(x) {
      last_index <- length(x$segment_schedule_contract$segments)
      x$segment_schedule_contract$segments[[last_index]]$
        end_completed_iterations <-
        x$segment_schedule_contract$segments[[last_index]]$
          end_completed_iterations + 1L
      x$segment_schedule_digest <- rqrgibbs:::.rqr_digest(
        x$segment_schedule_contract
      )
      x
    }
  )
  for (name in names(envelope_mutations)) {
    altered <- envelope_mutations[[name]](fit)
    expect_error(
      rqr_posterior_draws(altered),
      "DLM|checkpoint|schedule|retained|state|scale|latent",
      info = paste("read", name)
    )
    expect_error(
      rqr_dlm_continue(altered, 1L),
      "DLM|checkpoint|schedule|retained|state|scale|latent",
      info = paste("continue", name)
    )
  }

  forged_last <- envelope_mutations$last_alias(fit)
  expect_error(
    predict_interval(forged_last),
    "last-state alias"
  )
  expect_error(
    rqr_forecast_roots(
      forged_last,
      FF_future = matrix(1, 1L, 1L),
      GG_future = 1, W_future = 0.1
    ),
    "last-state alias"
  )
  expect_error(print(forged_last), "last-state alias")

  fixed_fit <- rqr_dlm_fit(
    y, rqr_polytrend(1L, C0 = 2), 0.8,
    evolution_mode = "fixed_W", W = 0.05,
    learning_rate = 1.25, loss_reference_scale = 2,
    learning_rate_mode = "fixed_rate",
    mcmc_control = list(
      n_burn = 0L, n_mcmc = 2L, seed = 1212L, backend = "R"
    )
  )
  fixed_fit$samp.lambda[1L] <- fixed_fit$samp.lambda[1L] + 1
  expect_error(
    rqr_posterior_draws(fixed_fit),
    "Fixed-rate DLM lambda"
  )
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

test_that("DLM public controls reject unknown fields and silent coercions", {
  base <- list(
    y = seq(-0.4, 0.4, length.out = 6L),
    model = rqr_polytrend(1L, C0 = 2),
    coverage_level = 0.8,
    evolution_mode = "fixed_W",
    W = 0.05
  )
  call_fit <- function(mcmc_control = list(
                         n_burn = 0L, n_mcmc = 1L, backend = "R"
                       ),
                       init = list(), ...) {
    do.call(
      rqr_dlm_fit,
      c(
        base,
        list(mcmc_control = mcmc_control, init = init),
        list(...)
      )
    )
  }

  expect_error(
    call_fit(mcmc_control = list(
      n_burn = 0L, n_mcmc = 1L, backend = "R", typo = TRUE
    )),
    "unsupported fields: typo"
  )
  expect_error(
    call_fit(init = list(unknown_state = 1)),
    "unsupported fields: unknown_state"
  )
  expect_error(
    call_fit(init = list(
      state_root1 = matrix(0, 1, 6),
      state_root2 = matrix(0, 1, 6),
      theta1 = matrix(0, 1, 6)
    )),
    "canonical state paths and their legacy aliases"
  )
  expect_error(
    call_fit(mcmc_control = list(
      n_burn = 0L, n_mcmc = 1L, backend = "r"
    )),
    "must be exactly one of"
  )
  expect_error(
    call_fit(mcmc_control = list(
      n_burn = 0L, n_mcmc = 1L, backend = "R",
      store_state_draws = 1
    )),
    "store_state_draws must be TRUE or FALSE"
  )
  expect_error(
    call_fit(mcmc_control = list(
      n_burn = 0L, n_mcmc = 1L, backend = "R",
      component_scale_slice_width = c(1, 2)
    )),
    "component_scale_slice_width must be one finite numeric scalar"
  )
  expect_error(
    call_fit(mcmc_control = list(
      n_burn = 0L, n_mcmc = 1L, backend = "R",
      jitter_ladder = "0"
    )),
    "jitter_ladder must be a nonempty plain numeric"
  )
  expect_error(
    call_fit(learning_rate = c(1, 2)),
    "learning_rate must be one finite numeric scalar"
  )
  continuation_only <- list(
    completed_iterations = 1L,
    continued_from_checkpoint = TRUE,
    parent_cumulative_numerical_repair_count = 0L,
    parent_chain_history_numerically_exact = TRUE,
    parent_promotion_eligible = TRUE,
    continuation_control = list()
  )
  for (field in names(continuation_only)) {
    expect_error(
      call_fit(init = stats::setNames(
        list(continuation_only[[field]]), field
      )),
      "Continuation-only init fields",
      info = field
    )
  }
  expect_false(
    ".continuation_token" %in% names(formals(rqr_dlm_fit))
  )
  expect_error(
    do.call(
      rqrgibbs:::.rqr_dlm_fit_impl,
      c(
        base,
        list(
          mcmc_control = list(
            n_burn = 0L, n_mcmc = 1L, backend = "R"
          ),
          init = continuation_only,
          .continuation_token = new.env(parent = emptyenv())
        )
      )
    ),
    "private validated.*worker boundary"
  )
  bad_y <- base
  bad_y$y <- factor(seq_len(6L))
  bad_y$mcmc_control <- list(
    n_burn = 0L, n_mcmc = 1L, backend = "R"
  )
  expect_error(
    do.call(rqr_dlm_fit, bad_y),
    "plain numeric vector"
  )

  set.seed(1213)
  rng_before <- .Random.seed
  expect_error(
    call_fit(mcmc_control = list(
      n_burn = 0L, n_mcmc = 1L, seed = 99L, backend = "R",
      verbose = 1
    )),
    "verbose must be TRUE or FALSE"
  )
  expect_identical(.Random.seed, rng_before)
  expect_error(
    call_fit(
      mcmc_control = list(
        n_burn = 0L, n_mcmc = 1L, seed = 99L, backend = "R"
      ),
      init = list(rng_state = rng_before)
    ),
    "not both"
  )

  valid <- call_fit()
  expect_s3_class(valid, "rqr_dlm_mcmc")
  expect_error(
    rqr_dlm_continue(valid, 1L, store_state_draws = 1),
    "store_state_draws must be TRUE or FALSE"
  )
})

test_that("future roots validate stored states and draw bindings", {
  fit <- rqr_dlm_fit(
    y = seq(-0.4, 0.4, length.out = 7L),
    model = rqr_polytrend(1L, C0 = 2),
    coverage_level = 0.8,
    evolution_mode = "fixed_W",
    W = 0.04,
    mcmc_control = list(
      n_burn = 0L, n_mcmc = 3L, seed = 1214L,
      backend = "R", store_state_draws = TRUE
    )
  )
  forecast_args <- list(
    FF_future = matrix(1, 1, 2),
    GG_future = 1,
    W_future = 0.04,
    seed = 1215L
  )
  valid <- do.call(rqr_forecast_roots, c(list(object = fit), forecast_args))
  expect_identical(valid$draw_index, seq_len(3L))
  expect_identical(valid$draw_binding_status, "fit_retained_draws")

  full_only <- fit
  full_only$samp.theta_terminal_root1 <- NULL
  full_only$samp.theta_terminal_root2 <- NULL
  fallback <- do.call(
    rqr_forecast_roots, c(list(object = full_only), forecast_args)
  )
  expect_identical(fallback$draw_index, seq_len(3L))

  mutations <- list(
    missing_second_terminal = function(x) {
      x$samp.theta_terminal_root2 <- NULL
      x
    },
    nonfinite_terminal = function(x) {
      x$samp.theta_terminal_root1[1L, 1L] <- NA_real_
      x
    },
    retained_draw_mismatch = function(x) {
      x$samp.theta_terminal_root1 <-
        x$samp.theta_terminal_root1[, -1L, drop = FALSE]
      x$samp.theta_terminal_root2 <-
        x$samp.theta_terminal_root2[, -1L, drop = FALSE]
      x$samp.theta_root1 <- NULL
      x$samp.theta_root2 <- NULL
      x
    },
    terminal_path_mismatch = function(x) {
      x$samp.theta_terminal_root1[1L, 1L] <-
        x$samp.theta_terminal_root1[1L, 1L] + 1
      x
    },
    ordinate_binding_mismatch = function(x) {
      x$samp.theta_root1 <- NULL
      x$samp.theta_root2 <- NULL
      x$samp.theta_terminal_root1 <-
        x$samp.theta_terminal_root1[, c(2L, 1L, 3L), drop = FALSE]
      x
    },
    partial_fitted_draws = function(x) {
      x$samp.eta_root2 <- NULL
      x
    },
    missing_expanded_model = function(x) {
      x$expanded_model <- NULL
      x
    },
    wrong_state_dimension = function(x) {
      x$expanded_model$p <- 2L
      x
    }
  )
  for (label in names(mutations)) {
    bad <- mutations[[label]](fit)
    set.seed(1216)
    rng_before <- .Random.seed
    expect_error(
      do.call(
        rqr_forecast_roots,
        c(list(object = bad), forecast_args)
      ),
      info = label
    )
    expect_identical(.Random.seed, rng_before, info = label)
  }

  component_fit <- rqr_dlm_fit(
    y = seq(-0.3, 0.3, length.out = 6L),
    model = rqr_polytrend(1L, C0 = 2),
    coverage_level = 0.8,
    evolution_mode = "component_scale",
    component_templates = list(matrix(1, 1, 1)),
    mcmc_control = list(
      n_burn = 0L, n_mcmc = 3L, seed = 1217L, backend = "R"
    )
  )
  component_args <- list(
    FF_future = matrix(1, 1, 1),
    GG_future = 1,
    component_templates_future = list(matrix(1, 1, 1)),
    seed = 1218L
  )
  component_valid <- do.call(
    rqr_forecast_roots,
    c(list(object = component_fit), component_args)
  )
  expect_identical(
    component_valid$diagnostics$component_scale_draws,
    component_fit$samp.evolution_scale
  )

  transposed <- component_fit
  transposed$samp.evolution_scale <-
    t(transposed$samp.evolution_scale)
  expect_error(
    do.call(
      rqr_forecast_roots,
      c(list(object = transposed), component_args)
    ),
    "retained-draw-by-component"
  )
  unnamed <- component_fit
  colnames(unnamed$samp.evolution_scale) <- NULL
  expect_error(
    do.call(
      rqr_forecast_roots,
      c(list(object = unnamed), component_args)
    ),
    "component-name order"
  )
  nonfinite_scale <- component_fit
  nonfinite_scale$samp.evolution_scale[1L, 1L] <- Inf
  expect_error(
    do.call(
      rqr_forecast_roots,
      c(list(object = nonfinite_scale), component_args)
    ),
    "finite positive"
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

test_that("unknown Git status is distinct from a clean checkout", {
  provenance <- rqrgibbs:::.rqr_provenance(
    data = list(y = 1:3), matrices = list(X = diag(3)),
    repo_root = tempdir()
  )
  expect_false(provenance$git_commit_available)
  expect_false(provenance$git_status_available)
  expect_true(is.na(provenance$git_dirty))
  expect_false(provenance$provenance_complete)
  expect_false(provenance$reproducibility_eligible)
})

test_that("strict provenance includes toolchain and required external repositories", {
  skip_if(Sys.which("git") == "", "git is required for provenance fixtures")
  make_repo <- function(label) {
    path <- tempfile(label)
    dir.create(path)
    system2("git", c("-C", path, "init", "--quiet"))
    system2("git", c("-C", path, "config", "user.email", "test@example.org"))
    system2("git", c("-C", path, "config", "user.name", "RQR Test"))
    writeLines(label, file.path(path, "fixture.txt"))
    system2("git", c("-C", path, "add", "fixture.txt"))
    system2("git", c("-C", path, "commit", "--quiet", "-m", "fixture"))
    list(
      root = normalizePath(path, mustWork = TRUE),
      commit = trimws(system2(
        "git", c("-C", path, "rev-parse", "HEAD"), stdout = TRUE
      )[1])
    )
  }
  primary <- make_repo("primary")
  external <- make_repo("external")
  control <- rqrgibbs:::.rqr_require_external_repository(
    list(
      repo_root = primary$root,
      expected_git_commit = primary$commit,
      external_repositories = list(
        exdqlm = list(
          repo_root = external$root,
          expected_git_commit = external$commit
        )
      )
    ),
    "exdqlm", external$commit
  )
  provenance <- rqrgibbs:::.rqr_provenance(
    data = list(y = 1:3),
    matrices = list(X = diag(3)),
    repo_root = control$repo_root,
    expected_git_commit = control$expected_git_commit,
    backend = "test_backend",
    external_repositories = control$external_repositories,
    required_external_repositories = control$required_external_repositories
  )
  expect_true(all(c("compiler", "BLAS", "LAPACK", "backend", "RNGkind") %in%
                    names(provenance)))
  expect_true(all(vapply(
    provenance[c("compiler", "BLAS", "LAPACK", "backend")],
    rqrgibbs:::.rqr_nonmissing_text,
    logical(1L)
  )))
  expect_true("exdqlm" %in% names(provenance$dependency_versions))
  expect_true(provenance$external_repositories$exdqlm$provenance_complete)
  expect_true(provenance$external_repositories$exdqlm$reproducibility_eligible)

  missing_external <- rqrgibbs:::.rqr_provenance(
    data = list(y = 1:3), matrices = list(X = diag(3)),
    repo_root = primary$root, backend = "test_backend",
    required_external_repositories = "exdqlm"
  )
  expect_false(missing_external$provenance_complete)
  expect_false(missing_external$reproducibility_eligible)
  expect_error(
    rqrgibbs:::.rqr_require_external_repository(
      list(external_repositories = list(
        exdqlm = list(expected_git_commit = paste(rep("0", 40), collapse = ""))
      )),
      "exdqlm",
      rqrgibbs:::.rqr_pinned_exdqlm_commit()
    ),
    "pinned commit"
  )
})

test_that("runtime lineage binds one complete build and install", {
  skip_if(Sys.which("git") == "", "git is required for provenance fixtures")
  skip_if(Sys.which("R") == "", "R is required for package-build fixtures")
  package <- "rqrlineagefixture"
  source <- tempfile("runtime-source-")
  artifacts <- tempfile("runtime-artifacts-")
  staging <- file.path(artifacts, "staging")
  library <- file.path(artifacts, "library")
  dir.create(file.path(source, "R"), recursive = TRUE)
  dir.create(file.path(source, "inst", "extdata"), recursive = TRUE)
  dir.create(staging, recursive = TRUE)
  dir.create(library, recursive = TRUE)
  writeLines(
    c(
      paste0("Package: ", package),
      "Type: Package",
      "Title: Runtime Lineage Fixture",
      "Version: 0.0.1",
      "Authors@R: person('RQR', 'Test', role=c('aut','cre'), email='test@example.org')",
      "Description: A minimal package used to verify the runtime lineage contract.",
      "License: MIT",
      "Encoding: UTF-8"
    ),
    file.path(source, "DESCRIPTION")
  )
  writeLines("export(lineage_value)", file.path(source, "NAMESPACE"))
  writeLines(
    "lineage_value <- function() 'archive-A'",
    file.path(source, "R", "lineage.R")
  )
  writeLines(
    "required,lineage\nTRUE,archive-A",
    file.path(source, "inst", "extdata", "required.csv")
  )
  git <- function(args, stdout = FALSE) {
    system2(
      "git", c("-C", source, args),
      stdout = stdout,
      env = c("GIT_OPTIONAL_LOCKS=0", "GIT_TERMINAL_PROMPT=0")
    )
  }
  git(c("init", "--quiet"))
  git(c("config", "user.email", "test@example.org"))
  git(c("config", "user.name", "RQR Test"))
  git(c("add", "."))
  git(c("commit", "--quiet", "-m", "fixture"))
  commit <- tolower(trimws(git(c("rev-parse", "HEAD"), TRUE)[1L]))
  tree <- tolower(trimws(git(c("rev-parse", "HEAD^{tree}"), TRUE)[1L]))
  snapshot <- digest::digest(
    paste(
      git(c("rev-parse", "HEAD"), TRUE),
      git(c("status", "--porcelain=v2", "--untracked-files=all"), TRUE),
      git(c("show-ref", "--head", "--dereference"), TRUE),
      collapse = "\n"
    ),
    algo = "sha256", serialize = FALSE
  )

  source_archive <- file.path(artifacts, "source-A.tar.gz")
  expect_identical(
    git(c(
      "archive", "--format=tar.gz",
      paste0("--prefix=", package, "/"),
      "-o", source_archive, commit
    )),
    0L
  )
  utils::untar(source_archive, exdir = staging)
  file_sha <- function(path) digest::digest(
    file = path, algo = "sha256", serialize = FALSE
  )
  source_archive_sha <- file_sha(source_archive)
  r_bin <- file.path(R.home("bin"), "R")
  directory_sha <- rqrgibbs:::.rqr_directory_digest
  command_receipt <- function(
      path, phase, executable, arguments, workdir,
      input_path, input_sha, output_path, output_sha,
      stdout_path, stderr_path, exit_status, started_at, ended_at,
      library_path = NA_character_) {
    receipt <- list(
      schema_version = "rqrgibbs_command_receipt/2.0.0",
      phase = phase,
      executable = normalizePath(executable, winslash = "/", mustWork = TRUE),
      arguments = arguments,
      working_directory = normalizePath(
        workdir, winslash = "/", mustWork = TRUE
      ),
      input_paths = normalizePath(
        input_path, winslash = "/", mustWork = TRUE
      ),
      input_sha256 = input_sha,
      output_path = normalizePath(
        output_path, winslash = "/", mustWork = TRUE
      ),
      output_sha256 = output_sha,
      library_path = if (is.na(library_path)) {
        NA_character_
      } else {
        normalizePath(library_path, winslash = "/", mustWork = TRUE)
      },
      stdout_path = normalizePath(
        stdout_path, winslash = "/", mustWork = TRUE
      ),
      stdout_sha256 = file_sha(stdout_path),
      stderr_path = normalizePath(
        stderr_path, winslash = "/", mustWork = TRUE
      ),
      stderr_sha256 = file_sha(stderr_path),
      exit_status = as.integer(exit_status),
      started_at = started_at,
      ended_at = ended_at,
      elapsed_seconds = ended_at - started_at
    )
    saveRDS(receipt, path, version = 3)
    list(
      path = normalizePath(path, winslash = "/", mustWork = TRUE),
      sha256 = digest::digest(
        file = path, algo = "sha256", serialize = FALSE
      ),
      receipt = receipt
    )
  }
  build_stdout <- file.path(artifacts, "build.stdout.log")
  build_stderr <- file.path(artifacts, "build.stderr.log")
  build_arguments <- c(
    "CMD", "build", "--no-manual", "--no-build-vignettes", package
  )
  build_input <- file.path(staging, package)
  build_input_sha <- directory_sha(build_input)
  old <- setwd(staging)
  on.exit(setwd(old), add = TRUE)
  build_started <- as.numeric(Sys.time())
  build_status <- system2(
    r_bin, build_arguments,
    stdout = build_stdout, stderr = build_stderr
  )
  build_ended <- as.numeric(Sys.time())
  expect_identical(
    build_status,
    0L
  )
  setwd(old)
  source_package <- file.path(
    staging, paste0(package, "_0.0.1.tar.gz")
  )
  source_package_sha <- digest::digest(
    file = source_package, algo = "sha256", serialize = FALSE
  )
  build_receipt <- command_receipt(
    file.path(artifacts, "build.command.rds"), "build",
    r_bin, build_arguments, staging,
    build_input, build_input_sha, source_package, source_package_sha,
    build_stdout, build_stderr, build_status, build_started, build_ended
  )
  source_lineage <- rqrgibbs:::.rqr_source_package_lineage(
    source_archive, package, source_package
  )
  expect_true(source_lineage$match)
  install_stdout <- file.path(artifacts, "install.stdout.log")
  install_stderr <- file.path(artifacts, "install.stderr.log")
  install_arguments <- c(
    "CMD", "INSTALL", "--preclean", "--clean",
    paste0("--library=", shQuote(library)), shQuote(source_package)
  )
  install_started <- as.numeric(Sys.time())
  install_status <- system2(
    r_bin, install_arguments,
    stdout = install_stdout, stderr = install_stderr
  )
  install_ended <- as.numeric(Sys.time())
  expect_identical(
    install_status,
    0L
  )
  runtime_path <- normalizePath(
    file.path(library, package), winslash = "/", mustWork = TRUE
  )
  runtime_pre_marker_digest <- directory_sha(runtime_path)
  install_receipt <- command_receipt(
    file.path(artifacts, "install.command.rds"), "install",
    r_bin, install_arguments, artifacts,
    source_package, source_package_sha,
    runtime_path, runtime_pre_marker_digest,
    install_stdout, install_stderr, install_status,
    install_started, install_ended, library
  )
  marker_path <- file.path(runtime_path, "RQR-RUNTIME-LINEAGE.rds")
  marker <- list(
    schema_version = "rqrgibbs_runtime_lineage_marker/2.0.0",
    package = package,
    package_version = "0.0.1",
    source_package_sha256 = source_package_sha,
    built_source_manifest_digest =
      source_lineage$built_source_manifest_digest,
    install_command_receipt_sha256 = install_receipt$sha256,
    installed_tree_pre_marker_digest = runtime_pre_marker_digest
  )
  saveRDS(marker, marker_path, version = 3)
  marker_sha <- digest::digest(
    file = marker_path, algo = "sha256", serialize = FALSE
  )
  runtime_digest <- rqrgibbs:::.rqr_directory_digest(runtime_path)
  loadNamespace(package, lib.loc = library)
  on.exit({
    if (package %in% loadedNamespaces()) unloadNamespace(package)
  }, add = TRUE)
  git_manifest <- rqrgibbs:::.rqr_git_manifest_payload(
    source, commit, "."
  )
  archive_manifest <- rqrgibbs:::.rqr_archive_manifest_payload(
    source_archive, package
  )
  attestation <- list(
    schema_version = "rqrgibbs_runtime_attestation/5.0.0",
    package = package,
    package_version = "0.0.1",
    source_commit = commit,
    source_tree_digest = tree,
    source_repo_root = normalizePath(source, winslash = "/", mustWork = TRUE),
    source_subdir = ".",
    source_access_mode = "git_archive_read_only",
    source_archive_prefix = package,
    source_checkout_snapshot_before = snapshot,
    source_checkout_snapshot_after = snapshot,
    source_checkout_unchanged = TRUE,
    source_archive_path = source_archive,
    source_archive_sha256 = source_archive_sha,
    source_git_manifest_digest = digest::digest(
      git_manifest, algo = "sha256", serialize = FALSE
    ),
    source_archive_manifest_digest = digest::digest(
      archive_manifest, algo = "sha256", serialize = FALSE
    ),
    source_archive_tree_match = TRUE,
    source_archive_isolated_from_source = TRUE,
    source_package_path = source_package,
    source_package_sha256 = source_package_sha,
    source_package_archive_match = TRUE,
    expected_source_manifest_digest =
      source_lineage$expected_source_manifest_digest,
    expected_source_manifest_entries =
      source_lineage$expected_source_manifest_entries,
    build_input_tree_digest = build_input_sha,
    built_source_manifest_digest =
      source_lineage$built_source_manifest_digest,
    built_source_manifest_entries =
      source_lineage$built_source_manifest_entries,
    build_stdout_path = build_stdout,
    build_stdout_sha256 = file_sha(build_stdout),
    build_stderr_path = build_stderr,
    build_stderr_sha256 = file_sha(build_stderr),
    install_stdout_path = install_stdout,
    install_stdout_sha256 = file_sha(install_stdout),
    install_stderr_path = install_stderr,
    install_stderr_sha256 = file_sha(install_stderr),
    build_command_receipt_path = build_receipt$path,
    build_command_receipt_sha256 = build_receipt$sha256,
    build_executable = normalizePath(r_bin, winslash = "/", mustWork = TRUE),
    build_arguments = build_arguments,
    build_working_directory = normalizePath(
      staging, winslash = "/", mustWork = TRUE
    ),
    build_input_path = normalizePath(
      file.path(staging, package), winslash = "/", mustWork = TRUE
    ),
    install_command_receipt_path = install_receipt$path,
    install_command_receipt_sha256 = install_receipt$sha256,
    install_executable = normalizePath(r_bin, winslash = "/", mustWork = TRUE),
    install_arguments = install_arguments,
    install_working_directory = normalizePath(
      artifacts, winslash = "/", mustWork = TRUE
    ),
    install_input_path = normalizePath(
      source_package, winslash = "/", mustWork = TRUE
    ),
    install_library_path = normalizePath(
      library, winslash = "/", mustWork = TRUE
    ),
    runtime_package_path = runtime_path,
    runtime_lineage_marker_path = marker_path,
    runtime_lineage_marker_sha256 = marker_sha,
    runtime_pre_marker_tree_digest = runtime_pre_marker_digest,
    runtime_package_tree_digest = runtime_digest,
    runtime_isolated_from_source = TRUE,
    R_version = R.version.string,
    platform = R.version$platform
  )
  receipt_args <- list(
    source_archive_sha256 = source_archive_sha,
    source_package_sha256 = source_package_sha,
    built_source_manifest_digest =
      source_lineage$built_source_manifest_digest,
    runtime_pre_marker_tree_digest = runtime_pre_marker_digest,
    runtime_package_tree_digest = runtime_digest,
    build_stdout_sha256 = file_sha(build_stdout),
    build_stderr_sha256 = file_sha(build_stderr),
    install_stdout_sha256 = file_sha(install_stdout),
    install_stderr_sha256 = file_sha(install_stderr),
    build_command_receipt_sha256 = build_receipt$sha256,
    install_command_receipt_sha256 = install_receipt$sha256,
    runtime_lineage_marker_sha256 = marker_sha,
    R_version = R.version.string,
    platform = R.version$platform
  )
  attestation$runtime_install_receipt_digest <- do.call(
    rqrgibbs:::.rqr_runtime_install_receipt_digest, receipt_args
  )
  attestation_path <- file.path(artifacts, "attestation.rds")
  saveRDS(attestation, attestation_path, version = 3)
  matched <- rqrgibbs:::.rqr_repository_provenance(list(
    repo_root = source,
    expected_git_commit = commit,
    runtime_package = package,
    runtime_attestation = attestation_path,
    require_isolated_runtime = TRUE
  ))
  expect_true(matched$runtime_attestation_match)
  expect_true(matched$source_package_archive_match)
  expect_true(matched$build_evidence_verified)
  expect_true(matched$install_evidence_verified)
  expect_true(matched$runtime_lineage_marker_match)
  expect_true(matched$runtime_source_match)
  expect_true(matched$reproducibility_eligible)

  subset_root <- file.path(artifacts, "subset")
  dir.create(subset_root)
  utils::untar(source_package, exdir = subset_root)
  unlink(file.path(
    subset_root, package, "inst", "extdata", "required.csv"
  ))
  subset_package <- file.path(artifacts, "source-subset.tar.gz")
  expect_identical(
    system2(
      "tar",
      c("-czf", subset_package, "-C", subset_root, package)
    ),
    0L
  )
  subset_lineage <- rqrgibbs:::.rqr_source_package_lineage(
    source_archive, package, subset_package
  )
  expect_false(subset_lineage$match)
  expect_identical(
    subset_lineage$missing_expected_entries,
    "inst/extdata/required.csv"
  )
  expect_false(rqrgibbs:::.rqr_command_shape_verified(
    "install",
    c(install_arguments, shQuote(subset_package)),
    source_package,
    library
  ))
  failed_receipt <- install_receipt$receipt
  failed_receipt$exit_status <- 1L
  failed_path <- file.path(artifacts, "failed-install.command.rds")
  saveRDS(failed_receipt, failed_path, version = 3)
  failed_sha <- file_sha(failed_path)
  expect_false(rqrgibbs:::.rqr_command_receipt_verified(
    failed_path, failed_sha, "install",
    source_package, source_package_sha,
    runtime_path, runtime_pre_marker_digest, library
  ))

  mixed_root <- file.path(artifacts, "mixed")
  dir.create(mixed_root)
  utils::untar(source_package, exdir = mixed_root)
  writeLines(
    "lineage_value <- function() 'source-package-B'",
    file.path(mixed_root, package, "R", "lineage.R")
  )
  mixed_package <- file.path(artifacts, "source-B.tar.gz")
  expect_identical(
    system2(
      "tar",
      c("-czf", mixed_package, "-C", mixed_root, package)
    ),
    0L
  )
  mixed <- attestation
  mixed$source_package_path <- mixed_package
  mixed$source_package_sha256 <- file_sha(mixed_package)
  saveRDS(mixed, attestation_path, version = 3)
  rejected <- rqrgibbs:::.rqr_repository_provenance(list(
    repo_root = source,
    expected_git_commit = commit,
    runtime_package = package,
    runtime_attestation = attestation_path,
    require_isolated_runtime = TRUE
  ))
  expect_true(rejected$source_package_verified)
  expect_false(rejected$source_package_archive_match)
  expect_false(rejected$runtime_lineage_marker_match)
  expect_false(rejected$runtime_attestation_match)
  expect_false(rejected$runtime_source_match)
  expect_false(rejected$reproducibility_eligible)
})

test_that("runtime-backed external adapters require isolated attestation", {
  control <- rqrgibbs:::.rqr_require_external_repository(
    list(), "exdqlm", rqrgibbs:::.rqr_pinned_exdqlm_commit(),
    runtime_package = "exdqlm"
  )
  expect_true(
    control$external_repositories$exdqlm$require_isolated_runtime
  )
  expect_identical(
    control$external_repositories$exdqlm$runtime_package,
    "exdqlm"
  )
})

test_that("DESN forecast horizon rejects fractional values", {
  object <- structure(list(), class = "rqr_desn_fit")
  expect_error(
    forecast_paths.rqr_desn_fit(object, H = 2.9),
    "H must be one finite integer"
  )
})

test_that("VB draw and iteration controls reject fractional values", {
  X <- cbind(1, seq(-1, 1, length.out = 8))
  y <- seq(-0.5, 0.5, length.out = 8)
  fit <- rqr_vb_fit(
    y, X, 0.8,
    vb_control = list(max_iter = 2, n_draws = 20, seed = 1208)
  )
  expect_error(rqr_posterior_draws(fit, nd = 2.5), "nd")
  expect_error(rqr_posterior_draws(fit, seed = 1.5), "seed")
  expect_error(
    rqr_vb_fit(y, X, 0.8, vb_control = list(max_iter = 2.5, n_draws = 20)),
    "max_iter"
  )
})
