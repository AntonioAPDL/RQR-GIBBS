.rqr_missingness_fixture <- function() {
  x <- seq(-1.2, 1.2, length.out = 7L)
  X <- cbind(
    intercept = 1,
    x = x,
    z = c(-0.5, 0.8, -1.0, 0.3, 1.1, -0.7, 0.4)
  )
  rownames(X) <- sprintf("row_%02d", seq_len(nrow(X)))
  y <- 0.25 + 0.55 * x - 0.20 * X[, "z"] +
    c(-0.10, 0.08, -0.05, 0.12, -0.03, 0.06, -0.08)
  list(y = y, X = X)
}

.rqr_missingness_static_fit <- function(
    y, X, seed, init = list(), n_mcmc = 2L) {
  rqr_mcmc_fit(
    y = y,
    X = X,
    coverage_level = 0.8,
    learning_rate = 1.25,
    lambda_initial = 1.1,
    loss_reference_scale = 1.7,
    learning_rate_mode = "learned_pseudoresidual_normalized",
    lambda_prior = list(shape = 3.25, rate = 2.1),
    beta_prior_obj = rqr_beta_prior(
      "ridge", ridge = list(tau2 = 6)
    ),
    numerical_policy = "fail",
    root_swap_probability = 0.5,
    mcmc_control = list(
      n_burn = 0L,
      n_mcmc = n_mcmc,
      thin = 1L,
      seed = seed,
      store_latent_draws = TRUE
    ),
    init = init
  )
}

.rqr_missingness_desn_design <- function(y) {
  n <- length(y)
  X <- cbind(
    intercept = 1,
    h_last_001 = seq(-0.75, 0.75, length.out = n),
    reduced_h_001 = sin(seq_len(n) / 2.5)
  )
  rqr_desn_design(
    X = X,
    y = y,
    time_index = 200 + seq_len(n),
    intercept = "intercept",
    builder = list(
      id = "ordinary_v1_missingness_test",
      version = "1.0.0"
    ),
    reservoir = list(
      digest = rqrgibbs:::.rqr_digest(
        list(seed = 28401L, dimension = 2L)
      ),
      seed = 28401L,
      depth = 2L
    ),
    driver = list(
      type = "observed_history",
      response_simulation = FALSE,
      history_digest = rqrgibbs:::.rqr_digest(y)
    ),
    causal = list(
      uses_current_response = FALSE,
      uses_future_response = FALSE,
      minimum_response_lag = 1L
    ),
    terminal = list(
      available = TRUE,
      state_digest = rqrgibbs:::.rqr_digest(c(0.1, -0.2)),
      lag_buffer_digest = rqrgibbs:::.rqr_digest(
        utils::tail(y[!is.na(y)], 2L)
      )
    )
  )
}

test_that("fixed-design masks equal their observed-row loss/augmentation fit", {
  fixture <- .rqr_missingness_fixture()
  n <- length(fixture$y)
  masks <- list(
    leading = 1L,
    trailing = n,
    interior = 4L,
    multiple = c(1L, 4L, n)
  )

  for (index in seq_along(masks)) {
    mask <- masks[[index]]
    y_missing <- fixture$y
    y_missing[mask] <- NA_real_
    observed <- !is.na(y_missing)
    seed <- 28410L + index

    retained <- .rqr_missingness_static_fit(
      y = y_missing,
      X = fixture$X,
      seed = seed
    )
    dropped <- .rqr_missingness_static_fit(
      y = y_missing[observed],
      X = fixture$X[observed, , drop = FALSE],
      seed = seed
    )

    expect_identical(
      retained$model_spec$n_observed,
      as.integer(sum(observed)),
      info = names(masks)[[index]]
    )
    expect_identical(
      retained$model_spec$missing_response_count,
      as.integer(length(mask)),
      info = names(masks)[[index]]
    )
    expect_identical(
      which(!retained$data_contract$observed),
      as.integer(mask),
      info = names(masks)[[index]]
    )
    expect_identical(
      retained$samp.beta_root1,
      dropped$samp.beta_root1,
      info = paste(names(masks)[[index]], "root 1")
    )
    expect_identical(
      retained$samp.beta_root2,
      dropped$samp.beta_root2,
      info = paste(names(masks)[[index]], "root 2")
    )
    expect_identical(
      retained$samp.lambda,
      dropped$samp.lambda,
      info = paste(names(masks)[[index]], "lambda")
    )
    expect_identical(
      retained$diagnostics$loss_trace,
      dropped$diagnostics$loss_trace,
      info = paste(names(masks)[[index]], "loss")
    )
    expect_identical(
      retained$diagnostics$lambda_post_shape_trace,
      dropped$diagnostics$lambda_post_shape_trace,
      info = paste(names(masks)[[index]], "lambda shape")
    )
    expect_identical(
      retained$diagnostics$lambda_post_rate_trace,
      dropped$diagnostics$lambda_post_rate_trace,
      info = paste(names(masks)[[index]], "lambda rate")
    )
    expect_identical(
      unname(
        retained$samp.latent_v[
          , retained$data_contract$observed, drop = FALSE
        ]
      ),
      unname(dropped$samp.latent_v),
      info = paste(names(masks)[[index]], "observed latent scales")
    )
    expect_true(all(
      retained$samp.latent_v[, !observed, drop = FALSE] > 0
    ))

    # This equality concerns only generalized-Bayes loss and augmentation
    # contributions. The retained fit still evaluates interval roots at every
    # design row, so its full data contract and fitted summaries intentionally
    # differ from those of the row-dropped object.
    expect_false(identical(
      retained$data_contract$data_digest,
      dropped$data_contract$data_digest
    ))
    expect_identical(
      retained$model_spec$response_likelihood,
      FALSE
    )
  }
})

test_that("fixed-design missing-site latent placeholders are RNG invariant", {
  fixture <- .rqr_missingness_fixture()
  missing <- c(1L, 4L, 7L)
  fixture$y[missing] <- NA_real_
  common <- list(
    beta_root1 = c(-0.2, 0.3, -0.1),
    beta_root2 = c(0.4, -0.15, 0.2),
    lambda = 1.35
  )
  latent_a <- rep(0.8, length(fixture$y))
  latent_b <- latent_a
  latent_b[missing] <- c(Inf, -100, NaN)

  fit_a <- .rqr_missingness_static_fit(
    fixture$y, fixture$X, seed = 28420L,
    init = c(common, list(latent_v = latent_a))
  )
  fit_b <- .rqr_missingness_static_fit(
    fixture$y, fixture$X, seed = 28420L,
    init = c(common, list(latent_v = latent_b))
  )

  expect_identical(
    fit_a$initialization_contract,
    fit_b$initialization_contract
  )
  expect_identical(fit_a$initialization_digest, fit_b$initialization_digest)
  expect_identical(fit_a$samp.beta_root1, fit_b$samp.beta_root1)
  expect_identical(fit_a$samp.beta_root2, fit_b$samp.beta_root2)
  expect_identical(fit_a$samp.lambda, fit_b$samp.lambda)
  expect_identical(fit_a$samp.latent_v, fit_b$samp.latent_v)
  expect_identical(fit_a$diagnostics$loss_trace, fit_b$diagnostics$loss_trace)
  expect_identical(fit_a$checkpoint_state, fit_b$checkpoint_state)
  expect_identical(fit_a$checkpoint_digest, fit_b$checkpoint_digest)
})

test_that("ordinary fixed-design and frozen DESN reject all-missing responses", {
  fixture <- .rqr_missingness_fixture()
  all_missing <- rep(NA_real_, length(fixture$y))

  expect_error(
    .rqr_missingness_static_fit(
      all_missing, fixture$X, seed = 28430L, n_mcmc = 1L
    ),
    "At least one response must be observed"
  )
  expect_error(
    .rqr_missingness_desn_design(all_missing),
    "at least one observed value"
  )
  expect_error(
    rqr_dlm_fit(
      y = all_missing,
      model = rqr_polytrend(1L, C0 = matrix(1.5, 1L, 1L)),
      coverage_level = 0.8,
      evolution_spec = rqr_evolution_fixed(
        matrix(0.03, 1L, 1L)
      ),
      learning_rate_mode = "fixed_rate",
      numerical_policy = "fail",
      mcmc_control = list(
        n_burn = 0L, n_mcmc = 1L, seed = 28431L,
        backend = "R"
      )
    ),
    "at least one finite observation|At least one response must be observed"
  )
})

test_that("all exact DLM modes and rates accept every bounded NA-mask shape", {
  fixture <- .rqr_missingness_fixture()
  n_time <- length(fixture$y)
  model <- rqr_polytrend(
    1L, m0 = 0, C0 = matrix(1.5, 1L, 1L), name = "level"
  )
  masks <- list(
    leading = 1L,
    trailing = n_time,
    interior = 4L,
    multiple = c(1L, 4L, n_time)
  )
  rate_modes <- c(
    "fixed_rate", "learned_pseudoresidual_normalized"
  )
  evolution_modes <- c(
    "fixed_W", "discount_template", "component_scale"
  )
  make_evolution <- function(mode) {
    switch(
      mode,
      fixed_W = rqr_evolution_fixed(matrix(0.03, 1L, 1L)),
      discount_template = rqr_freeze_discount_template(
        model = model,
        n_time = n_time,
        df = 0.92,
        dim.df = 1L,
        reference_variance = 0.7,
        numerical_policy = "fail"
      ),
      component_scale = rqr_evolution_component_scale(
        templates = list(matrix(0.03, 1L, 1L)),
        component_dims = 1L,
        prior = list(shape = 3, rate = 2),
        initial = 1,
        component_names = "level"
      )
    )
  }

  cell <- 0L
  for (evolution_mode in evolution_modes) {
    for (rate_mode in rate_modes) {
      for (mask_name in names(masks)) {
        cell <- cell + 1L
        mask <- masks[[mask_name]]
        y <- fixture$y
        y[mask] <- NA_real_
        fit <- rqr_dlm_fit(
          y = y,
          model = model,
          coverage_level = 0.8,
          evolution_spec = make_evolution(evolution_mode),
          learning_rate = 1.25,
          lambda_initial = 1.1,
          loss_reference_scale = 1.7,
          learning_rate_mode = rate_mode,
          lambda_prior = list(shape = 3.25, rate = 2.1),
          numerical_policy = "fail",
          mcmc_control = list(
            n_burn = 0L,
            n_mcmc = 1L,
            thin = 1L,
            seed = 28460L + cell,
            backend = "R",
            store_state_draws = FALSE,
            store_latent_draws = FALSE
          )
        )
        info <- paste(evolution_mode, rate_mode, mask_name)
        expect_identical(
          which(!fit$misc$observed), as.integer(mask), info = info
        )
        expect_identical(
          fit$model_spec$evolution_mode, evolution_mode, info = info
        )
        expect_identical(
          fit$model_spec$learning_rate_mode, rate_mode, info = info
        )
        expect_identical(
          fit$model_spec$exact_joint_target, TRUE, info = info
        )
        expect_identical(
          fit$model_spec$numerical_repair_count, 0L, info = info
        )
        expect_true(all(is.finite(fit$samp.eta_root1)), info = info)
        expect_true(all(is.finite(fit$samp.eta_root2)), info = info)
        expect_true(all(is.finite(fit$samp.lambda)), info = info)
        expect_identical(
          fit$model_spec$response_likelihood, FALSE, info = info
        )
      }
    }
  }
  expect_identical(cell, 24L)
})

test_that("DLM missing-site latent placeholders are canonical and RNG inert", {
  fixture <- .rqr_missingness_fixture()
  missing <- c(1L, 4L, 7L)
  fixture$y[missing] <- NA_real_
  observed <- !is.na(fixture$y)
  canonical <- rep(0.9, length(fixture$y))
  arbitrary <- canonical
  arbitrary[missing] <- c(Inf, -100, NaN)
  observed_only <- canonical[observed]
  fit_with <- function(latent) {
    rqr_dlm_fit(
      y = fixture$y,
      model = rqr_polytrend(
        1L, m0 = 0, C0 = matrix(1.5, 1L, 1L), name = "level"
      ),
      coverage_level = 0.8,
      evolution_spec = rqr_evolution_fixed(
        matrix(0.03, 1L, 1L)
      ),
      lambda_initial = 1.1,
      loss_reference_scale = 1.7,
      learning_rate_mode = "learned_pseudoresidual_normalized",
      lambda_prior = list(shape = 3.25, rate = 2.1),
      numerical_policy = "fail",
      mcmc_control = list(
        n_burn = 0L, n_mcmc = 2L, thin = 1L,
        seed = 28490L, backend = "R",
        store_state_draws = TRUE,
        store_latent_draws = TRUE
      ),
      init = list(latent_v = latent)
    )
  }
  canonical_fit <- fit_with(canonical)
  arbitrary_fit <- fit_with(arbitrary)
  observed_fit <- fit_with(observed_only)

  for (candidate in list(arbitrary_fit, observed_fit)) {
    expect_identical(
      candidate$initialization_contract,
      canonical_fit$initialization_contract
    )
    expect_identical(
      candidate$initialization_digest,
      canonical_fit$initialization_digest
    )
    expect_identical(
      candidate$samp.eta_root1, canonical_fit$samp.eta_root1
    )
    expect_identical(
      candidate$samp.eta_root2, canonical_fit$samp.eta_root2
    )
    expect_identical(
      candidate$samp.lambda, canonical_fit$samp.lambda
    )
    expect_identical(
      candidate$samp.latent_v, canonical_fit$samp.latent_v
    )
    expect_identical(
      candidate$checkpoint_state, canonical_fit$checkpoint_state
    )
    expect_identical(
      candidate$checkpoint_digest, canonical_fit$checkpoint_digest
    )
  }
})

test_that("frozen DESN delegates missing rows to the static ordinary target", {
  y <- c(-0.55, -0.15, NA_real_, 0.25, 0.60, NA_real_, 0.95)
  design <- .rqr_missingness_desn_design(y)
  prior <- rqr_beta_prior("ridge", ridge = list(tau2 = 5))
  control <- list(
    n_burn = 0L,
    n_mcmc = 2L,
    thin = 1L,
    seed = 28440L,
    store_latent_draws = TRUE
  )
  wrapped <- rqr_desn_fit(
    design = design,
    coverage_level = 0.8,
    design_engine = "frozen",
    inference = "mcmc",
    learning_rate_mode = "fixed_rate",
    mcmc_args = list(
      beta_prior_obj = prior,
      mcmc_control = control
    )
  )
  direct <- rqr_mcmc_fit(
    y = design$y,
    X = design$X,
    coverage_level = 0.8,
    learning_rate_mode = "fixed_rate",
    beta_prior_obj = prior,
    mcmc_control = control,
    embedding_contract = wrapped$fit$embedding_contract
  )

  expect_identical(wrapped$fit$samp.beta_root1, direct$samp.beta_root1)
  expect_identical(wrapped$fit$samp.beta_root2, direct$samp.beta_root2)
  expect_identical(wrapped$fit$samp.lambda, direct$samp.lambda)
  expect_identical(wrapped$fit$samp.latent_v, direct$samp.latent_v)
  expect_identical(
    wrapped$fit$diagnostics$loss_trace,
    direct$diagnostics$loss_trace
  )
  expect_identical(
    which(!wrapped$fit$data_contract$observed),
    c(3L, 6L)
  )
  expect_identical(wrapped$model_spec$generalized_bayes, TRUE)
  expect_identical(wrapped$model_spec$response_likelihood, FALSE)
  expect_identical(
    wrapped$model_spec$response_prediction_contract,
    FALSE
  )
  expect_identical(wrapped$meta$response_simulation, FALSE)

  draws <- rqr_posterior_draws(wrapped)
  prediction <- predict_interval(wrapped, X_new = design$X)
  expect_identical(draws$response_predictive_draws, FALSE)
  expect_identical(prediction$response_predictive_draws, FALSE)
  expect_match(
    prediction$interpretation,
    "no future response distribution"
  )
})

test_that("frozen-discount DLM omits missing measurements exactly", {
  y <- c(-0.45, -0.10, NA_real_, 0.20, 0.55, NA_real_, 0.85)
  n_time <- length(y)
  model <- rqr_polytrend(
    order = 1L,
    m0 = 0,
    C0 = matrix(1.5, 1L, 1L),
    name = "level"
  )
  evolution <- rqr_freeze_discount_template(
    model = model,
    n_time = n_time,
    df = 0.92,
    dim.df = 1L,
    reference_variance = 0.7,
    numerical_policy = "fail"
  )

  z <- c(0.15, -0.05, NA_real_, 0.30, 0.25, NA_real_, 0.50)
  H <- matrix(1, 1L, n_time)
  V <- c(0.8, 0.7, 0.9, 0.75, 0.85, 0.95, 0.8)
  GG <- array(1, c(1L, 1L, n_time))
  smooth_r <- rqr_ffbs_smooth(
    z = z,
    H = H,
    V = V,
    GG = GG,
    m0 = 0,
    C0 = matrix(1.5, 1L, 1L),
    evolution = evolution,
    backend = "R",
    numerical_policy = "fail"
  )

  expected_prior_mean <- expected_filter_mean <-
    matrix(NA_real_, 1L, n_time)
  expected_prior_cov <- expected_filter_cov <-
    array(NA_real_, c(1L, 1L, n_time))
  previous_mean <- 0
  previous_cov <- 1.5
  for (tt in seq_len(n_time)) {
    prior_mean <- previous_mean
    prior_cov <- previous_cov + evolution$W[1L, 1L, tt]
    filtered_mean <- prior_mean
    filtered_cov <- prior_cov
    if (!is.na(z[[tt]])) {
      forecast_variance <- prior_cov + V[[tt]]
      gain <- prior_cov / forecast_variance
      filtered_mean <- prior_mean +
        gain * (z[[tt]] - prior_mean)
      filtered_cov <- (1 - gain)^2 * prior_cov +
        V[[tt]] * gain^2
    }
    expected_prior_mean[1L, tt] <- prior_mean
    expected_prior_cov[1L, 1L, tt] <- prior_cov
    expected_filter_mean[1L, tt] <- filtered_mean
    expected_filter_cov[1L, 1L, tt] <- filtered_cov
    previous_mean <- filtered_mean
    previous_cov <- filtered_cov
  }

  expect_equal(
    smooth_r$prior_mean, expected_prior_mean,
    tolerance = 1e-14
  )
  expect_equal(
    smooth_r$prior_cov, expected_prior_cov,
    tolerance = 1e-14
  )
  expect_equal(
    smooth_r$filter_mean, expected_filter_mean,
    tolerance = 1e-14
  )
  expect_equal(
    smooth_r$filter_cov, expected_filter_cov,
    tolerance = 1e-14
  )
  for (tt in which(is.na(z))) {
    expect_identical(
      smooth_r$filter_mean[, tt],
      smooth_r$prior_mean[, tt]
    )
    expect_identical(
      smooth_r$filter_cov[, , tt],
      smooth_r$prior_cov[, , tt]
    )
    expect_true(is.na(smooth_r$forecast_variance[[tt]]))
    expect_true(is.na(smooth_r$residual[[tt]]))
  }

  if (exists(
      "rqr_ffbs_cpp",
      envir = asNamespace("rqrgibbs"),
      inherits = FALSE
    )) {
    smooth_cpp <- rqr_ffbs_smooth(
      z = z,
      H = H,
      V = V,
      GG = GG,
      m0 = 0,
      C0 = matrix(1.5, 1L, 1L),
      evolution = evolution,
      backend = "cpp",
      numerical_policy = "fail"
    )
    expect_equal(
      smooth_cpp$filter_mean, smooth_r$filter_mean,
      tolerance = 1e-13
    )
    expect_equal(
      smooth_cpp$filter_cov, smooth_r$filter_cov,
      tolerance = 1e-13
    )
    expect_identical(
      is.na(smooth_cpp$forecast_variance),
      is.na(z)
    )
  }

  fit <- rqr_dlm_fit(
    y = y,
    model = model,
    coverage_level = 0.8,
    evolution_spec = evolution,
    learning_rate_mode = "fixed_rate",
    numerical_policy = "fail",
    mcmc_control = list(
      n_burn = 0L,
      n_mcmc = 2L,
      thin = 1L,
      seed = 28450L,
      backend = "R",
      store_state_draws = TRUE
    )
  )

  expect_identical(fit$evolution$mode, "discount_template")
  expect_identical(fit$evolution$frozen_before_mcmc, TRUE)
  expect_identical(fit$model_spec$exact_joint_target, TRUE)
  expect_identical(fit$model_spec$target_contract, "fixed_joint_exact")
  expect_identical(which(!fit$misc$observed), c(3L, 6L))
  expect_true(all(is.finite(fit$samp.eta_root1)))
  expect_true(all(is.finite(fit$samp.eta_root2)))
  expect_true(all(is.finite(fit$samp.theta_root1)))
  expect_true(all(is.finite(fit$samp.theta_root2)))
  expect_identical(fit$model_spec$response_likelihood, FALSE)
  expect_identical(
    fit$model_spec$response_prediction_contract,
    FALSE
  )
  expect_identical(fit$model_spec$numerical_repair_count, 0L)
})
