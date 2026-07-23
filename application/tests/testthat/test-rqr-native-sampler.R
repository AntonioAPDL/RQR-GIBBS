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
  expect_true(fit$model_spec$target_numerical_eligible)
  expect_false(fit$model_spec$reproducibility_eligible)
  expect_false(fit$model_spec$promotion_eligible)
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
  expect_true(fit$model_spec$target_numerical_eligible)
  expect_false(fit$model_spec$promotion_eligible)
  expect_equal(dim(fit$samp.evolution_scale), c(4L, 1L))
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

test_that("component-scale root forecasts match analytic state moments", {
  n_save <- 1L
  q <- 0.4
  terminal <- 2
  fixture <- structure(list(
    samp.theta_terminal_root1 = matrix(terminal, 1, n_save),
    samp.theta_terminal_root2 = matrix(terminal, 1, n_save),
    samp.evolution_scale = matrix(q, n_save, 1),
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
    matrix(q, n_draw, 1)
  )
  expect_equal(forecast$diagnostics$repair_count, 0L)
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
  expect_identical(second$provenance$schema_version, "rqrgibbs_fit/1.5.0")
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
    mcmc_control = list(n_burn = 0, n_mcmc = 2, seed = 1211, backend = "cpp")
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

test_that("runtime package provenance binds code to source or an attestation", {
  skip_if(Sys.which("git") == "", "git is required for provenance fixtures")
  source <- tempfile("runtime-source-")
  dir.create(source)
  writeLines(
    c(
      "Package: rqrgibbs",
      paste0("Version: ", as.character(utils::packageVersion("rqrgibbs")))
    ),
    file.path(source, "DESCRIPTION")
  )
  system2("git", c("-C", source, "init", "--quiet"))
  system2("git", c("-C", source, "config", "user.email", "test@example.org"))
  system2("git", c("-C", source, "config", "user.name", "RQR Test"))
  system2("git", c("-C", source, "add", "DESCRIPTION"))
  system2("git", c("-C", source, "commit", "--quiet", "-m", "fixture"))
  commit <- tolower(trimws(system2(
    "git", c("-C", source, "rev-parse", "HEAD"), stdout = TRUE
  )[1L]))
  tree <- tolower(trimws(system2(
    "git", c("-C", source, "rev-parse", "HEAD^{tree}"), stdout = TRUE
  )[1L]))

  mismatch <- rqrgibbs:::.rqr_repository_provenance(list(
    repo_root = source,
    expected_git_commit = commit,
    runtime_package = "rqrgibbs"
  ))
  expect_true(mismatch$provenance_complete)
  expect_false(mismatch$runtime_direct_source_path_match)
  expect_false(mismatch$runtime_source_match)
  expect_false(mismatch$reproducibility_eligible)

  runtime_path <- normalizePath(
    getNamespaceInfo(asNamespace("rqrgibbs"), "path"),
    winslash = "/", mustWork = TRUE
  )
  attestation_path <- tempfile(fileext = ".rds")
  source_archive_path <- tempfile(fileext = ".tar.gz")
  source_package_path <- tempfile(fileext = ".tar.gz")
  archive_status <- system2(
    "git",
    c(
      "-C", source, "archive", "--format=tar.gz",
      "--prefix=fixture/", "-o", source_archive_path, commit
    )
  )
  expect_identical(archive_status, 0L)
  expect_true(file.copy(source_archive_path, source_package_path))
  checkout_snapshot <- paste(rep("b", 64), collapse = "")
  git_manifest <- rqrgibbs:::.rqr_git_manifest_payload(
    source, commit, "."
  )
  archive_manifest <- rqrgibbs:::.rqr_archive_manifest_payload(
    source_archive_path, "fixture"
  )
  expect_identical(git_manifest, archive_manifest)
  source_archive_sha256 <- digest::digest(
    file = source_archive_path, algo = "sha256", serialize = FALSE
  )
  source_package_sha256 <- digest::digest(
    file = source_package_path, algo = "sha256", serialize = FALSE
  )
  runtime_tree_digest <- rqrgibbs:::.rqr_directory_digest(runtime_path)
  build_log_sha256 <- paste(rep("c", 64), collapse = "")
  install_log_sha256 <- paste(rep("d", 64), collapse = "")
  attestation <- list(
    schema_version = "rqrgibbs_runtime_attestation/3.0.0",
    package = "rqrgibbs",
    package_version = as.character(utils::packageVersion("rqrgibbs")),
    source_commit = commit,
    source_tree_digest = tree,
    source_repo_root = normalizePath(source, winslash = "/", mustWork = TRUE),
    source_subdir = ".",
    source_access_mode = "git_archive_read_only",
    source_archive_prefix = "fixture",
    source_checkout_snapshot_before = checkout_snapshot,
    source_checkout_snapshot_after = checkout_snapshot,
    source_checkout_unchanged = TRUE,
    source_archive_path = source_archive_path,
    source_archive_sha256 = source_archive_sha256,
    source_git_manifest_digest = digest::digest(
      git_manifest, algo = "sha256", serialize = FALSE
    ),
    source_archive_manifest_digest = digest::digest(
      archive_manifest, algo = "sha256", serialize = FALSE
    ),
    source_archive_tree_match = TRUE,
    source_archive_isolated_from_source = TRUE,
    source_package_path = source_package_path,
    source_package_sha256 = source_package_sha256,
    build_log_sha256 = build_log_sha256,
    install_log_sha256 = install_log_sha256,
    runtime_package_path = runtime_path,
    runtime_package_tree_digest = runtime_tree_digest,
    runtime_isolated_from_source = TRUE,
    runtime_install_receipt_digest =
      rqrgibbs:::.rqr_runtime_install_receipt_digest(
        source_archive_sha256,
        source_package_sha256,
        runtime_tree_digest,
        build_log_sha256,
        install_log_sha256,
        R.version.string,
        R.version$platform
      ),
    R_version = R.version.string,
    platform = R.version$platform
  )
  saveRDS(attestation, attestation_path)
  matched <- rqrgibbs:::.rqr_repository_provenance(list(
    repo_root = source,
    expected_git_commit = commit,
    runtime_package = "rqrgibbs",
    runtime_attestation = attestation_path,
    require_isolated_runtime = TRUE
  ))
  expect_true(matched$runtime_attestation_available)
  expect_true(matched$runtime_attestation_match)
  expect_identical(
    matched$runtime_attestation_schema,
    "rqrgibbs_runtime_attestation/3.0.0"
  )
  expect_identical(matched$source_access_mode, "git_archive_read_only")
  expect_true(matched$source_archive_verified)
  expect_true(matched$source_archive_tree_match)
  expect_true(matched$source_package_verified)
  expect_true(matched$runtime_install_receipt_match)
  expect_true(matched$source_archive_isolated_from_source)
  expect_true(matched$source_checkout_unchanged)
  expect_true(matched$runtime_isolated_from_source)
  expect_true(matched$require_isolated_runtime)
  expect_true(matched$runtime_source_match)
  expect_true(matched$reproducibility_eligible)

  writeBin(charToRaw("arbitrary archive fixture"), source_archive_path)
  archive_tampered <- rqrgibbs:::.rqr_repository_provenance(list(
    repo_root = source,
    expected_git_commit = commit,
    runtime_package = "rqrgibbs",
    runtime_attestation = attestation_path,
    require_isolated_runtime = TRUE
  ))
  expect_false(archive_tampered$source_archive_verified)
  expect_false(archive_tampered$source_archive_tree_match)
  expect_false(archive_tampered$runtime_attestation_match)
  expect_false(archive_tampered$runtime_source_match)
  expect_false(archive_tampered$reproducibility_eligible)

  archive_status <- system2(
    "git",
    c(
      "-C", source, "archive", "--format=tar.gz",
      "--prefix=fixture/", "-o", source_archive_path, commit
    )
  )
  expect_identical(archive_status, 0L)
  attestation$runtime_package_tree_digest <- paste(rep("0", 64), collapse = "")
  attestation$runtime_install_receipt_digest <-
    rqrgibbs:::.rqr_runtime_install_receipt_digest(
      source_archive_sha256,
      source_package_sha256,
      attestation$runtime_package_tree_digest,
      build_log_sha256,
      install_log_sha256,
      R.version.string,
      R.version$platform
    )
  saveRDS(attestation, attestation_path)
  tampered <- rqrgibbs:::.rqr_repository_provenance(list(
    repo_root = source,
    expected_git_commit = commit,
    runtime_package = "rqrgibbs",
    runtime_attestation = attestation_path,
    require_isolated_runtime = TRUE
  ))
  expect_false(tampered$runtime_attestation_match)
  expect_false(tampered$runtime_source_match)
  expect_false(tampered$reproducibility_eligible)
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
