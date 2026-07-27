ordinary_v1_reference_config <- function() {
  path <- testthat::test_path(
    "..", "..", "config", "rqr_ordinary_v1",
    "rqr_ordinary_v1_bounded_validation_20260726.R"
  )
  environment <- new.env(parent = baseenv())
  sys.source(path, envir = environment)
  environment$rqr_ordinary_v1_bounded_validation
}

ordinary_v1_reference_prior <- function(config, prior_id) {
  switch(
    prior_id,
    gaussian = rqr_beta_prior(
      "gaussian", gaussian = config$fixtures$F02$prior[-1L]
    ),
    ridge = rqr_beta_prior(
      "ridge", ridge = list(tau2 = config$fixtures$F03$ridge$tau2)
    ),
    rhs_ns_sampled = rqr_beta_prior(
      "rhs_ns", rhs_ns = config$fixtures$F04$prior[-1L]
    ),
    rhs_ns_fixed = rqr_beta_prior(
      "rhs_ns", rhs_ns = config$fixtures$F05$prior[-1L]
    )
  )
}

ordinary_v1_reference_fixture <- function(config, prior_id) {
  switch(
    prior_id,
    gaussian = config$fixtures$F02,
    ridge = config$fixtures$F03,
    rhs_ns_sampled = config$fixtures$F04,
    rhs_ns_fixed = config$fixtures$F05
  )
}

ordinary_v1_reference_fit <- function(
    y, X, prior, mode, seed = NULL, n_mcmc = 2L,
    embedding_contract = NULL, thin = 1L, init = list()) {
  rqr_mcmc_fit(
    y = y, X = X, coverage_level = 0.8,
    learning_rate = 1, lambda_initial = 1,
    loss_reference_scale = 1,
    learning_rate_mode = mode,
    lambda_prior = list(shape = 4, rate = 4),
    beta_prior_obj = prior,
    numerical_policy = "fail",
    root_swap_probability = 0.5,
    mcmc_control = list(
      n_burn = 0L, n_mcmc = n_mcmc, thin = thin,
      seed = seed, store_latent_draws = TRUE,
      store_prior_state_draws = identical(prior$type, "rhs_ns")
    ),
    embedding_contract = embedding_contract,
    init = init
  )
}

ordinary_v1_reference_bind <- function(segments, field) {
  values <- lapply(segments, `[[`, field)
  if (is.matrix(values[[1L]])) do.call(rbind, values) else
    unlist(values, recursive = FALSE, use.names = FALSE)
}

ordinary_v1_expect_exact_continuation <- function(
    y, X, prior, mode, seed, embedding_contract = NULL, thin = 1L) {
  full <- ordinary_v1_reference_fit(
    y, X, prior, mode, seed = seed, n_mcmc = 6L,
    embedding_contract = embedding_contract, thin = thin
  )
  first <- ordinary_v1_reference_fit(
    y, X, prior, mode, seed = seed, n_mcmc = 2L,
    embedding_contract = embedding_contract, thin = thin
  )
  second <- rqr_mcmc_continue(
    first, n_mcmc = 2L, thin = thin, store_latent_draws = TRUE,
    store_prior_state_draws = identical(prior$type, "rhs_ns")
  )
  third <- rqr_mcmc_continue(
    second, n_mcmc = 2L, thin = thin, store_latent_draws = TRUE,
    store_prior_state_draws = identical(prior$type, "rhs_ns")
  )
  segments <- list(first, second, third)

  expect_identical(
    ordinary_v1_reference_bind(segments, "samp.beta_root1"),
    full$samp.beta_root1
  )
  expect_identical(
    ordinary_v1_reference_bind(segments, "samp.beta_root2"),
    full$samp.beta_root2
  )
  expect_identical(
    unname(ordinary_v1_reference_bind(segments, "samp.lambda")),
    unname(full$samp.lambda)
  )
  expect_identical(
    ordinary_v1_reference_bind(segments, "samp.latent_v"),
    full$samp.latent_v
  )
  if (identical(prior$type, "rhs_ns")) {
    expect_identical(
      ordinary_v1_reference_bind(
        segments, "samp.beta_prior_state_root1"
      ),
      full$samp.beta_prior_state_root1
    )
    expect_identical(
      ordinary_v1_reference_bind(
        segments, "samp.beta_prior_state_root2"
      ),
      full$samp.beta_prior_state_root2
    )
  }
  expect_identical(
    unlist(lapply(
      segments, function(x) x$diagnostics$root_swap_trace
    ), use.names = FALSE),
    full$diagnostics$root_swap_trace
  )
  expect_identical(third$checkpoint_state, full$checkpoint_state)
  expect_identical(third$checkpoint_digest, full$checkpoint_digest)
  expect_identical(
    third$checkpoint_state$rng_state,
    full$checkpoint_state$rng_state
  )
  expect_identical(
    third$checkpoint_state$completed_iterations, 6L * thin
  )
  expect_identical(
    third$continuation_history_contract$generation, 2L
  )
  expect_identical(
    vapply(
      third$segment_schedule_contract$segments,
      `[[`, integer(1L), "thin"
    ),
    rep(as.integer(thin), 3L)
  )
  expect_silent(
    rqrgibbs:::.rqr_validate_continuation_history(third)
  )
  invisible(list(full = full, segments = segments))
}

ordinary_v1_reference_desn_design <- function(config) {
  specification <- config$fixtures$D01
  sha <- function(x) digest::digest(x, algo = "sha256", serialize = TRUE)
  rqr_desn_design(
    X = specification$X, y = specification$y,
    time_index = specification$time_index,
    intercept = specification$intercept,
    builder = specification$builder,
    reservoir = list(
      digest = sha(list(
        seed = specification$reservoir_seed,
        depth = specification$reservoir_depth,
        feature_names = colnames(specification$X)
      )),
      seed = specification$reservoir_seed,
      depth = specification$reservoir_depth
    ),
    driver = list(
      type = "observed_history", response_simulation = FALSE,
      history_digest = sha(specification$y)
    ),
    causal = specification$causal,
    terminal = list(
      available = TRUE,
      state_digest = sha(tail(specification$X[, -1L, drop = FALSE], 1L)),
      lag_buffer_digest = sha(tail(
        specification$y[!is.na(specification$y)], 2L
      ))
    )
  )
}

test_that("all eight static fixture-rate cells are exact under 6 versus 2+2+2", {
  config <- ordinary_v1_reference_config()
  cells <- data.frame(
    prior_id = rep(
      c("gaussian", "ridge", "rhs_ns_sampled", "rhs_ns_fixed"), each = 2L
    ),
    mode = rep(
      c("fixed_rate", "learned_pseudoresidual_normalized"), 4L
    ),
    seed = 82801:82808,
    stringsAsFactors = FALSE
  )
  for (index in seq_len(nrow(cells))) {
    prior_id <- cells$prior_id[[index]]
    fixture <- ordinary_v1_reference_fixture(config, prior_id)
    prior <- ordinary_v1_reference_prior(config, prior_id)
    ordinary_v1_expect_exact_continuation(
      fixture$y, fixture$X, prior, cells$mode[[index]],
      cells$seed[[index]]
    )
  }
})

test_that("F03 ridge and zero-mean Gaussian transitions are bitwise identical", {
  config <- ordinary_v1_reference_config()
  fixture <- config$fixtures$F03
  ridge <- rqr_beta_prior(
    "ridge", ridge = list(tau2 = fixture$ridge$tau2)
  )
  gaussian <- rqr_beta_prior(
    "gaussian", gaussian = fixture$gaussian[-1L]
  )
  explicit_init <- list(
    beta1 = c(intercept = -0.6, x = 0.15, z = -0.1),
    beta2 = c(intercept = 0.9, x = 0.55, z = 0.2),
    lambda = 1.25,
    latent_v = seq(0.7, 1.6, length.out = length(fixture$y))
  )
  fields <- c(
    "samp.beta_root1", "samp.beta_root2", "samp.lambda",
    "samp.latent_v"
  )
  diagnostic_fields <- c(
    "loss_trace", "scaled_loss_trace", "weighted_loss_trace",
    "lambda_trace", "effective_learning_rate_trace",
    "lambda_post_shape_trace", "lambda_post_rate_trace",
    "precision_strategy_root1", "precision_strategy_root2",
    "root_swap_trace"
  )

  for (mode_index in seq_along(c(
      "fixed_rate", "learned_pseudoresidual_normalized"
    ))) {
    mode <- c(
      "fixed_rate", "learned_pseudoresidual_normalized"
    )[[mode_index]]
    seed <- 82840L + mode_index
    ridge_fit <- ordinary_v1_reference_fit(
      fixture$y, fixture$X, ridge, mode,
      seed = seed, n_mcmc = 4L, init = explicit_init
    )
    gaussian_fit <- ordinary_v1_reference_fit(
      fixture$y, fixture$X, gaussian, mode,
      seed = seed, n_mcmc = 4L, init = explicit_init
    )
    for (field in fields) {
      expect_identical(
        ridge_fit[[field]], gaussian_fit[[field]],
        info = paste(mode, field)
      )
    }
    for (field in diagnostic_fields) {
      expect_identical(
        ridge_fit$diagnostics[[field]],
        gaussian_fit$diagnostics[[field]],
        info = paste(mode, "diagnostic", field)
      )
    }
    for (field in c(
        "beta_root1", "beta_root2", "lambda", "latent_v",
        "rng_state", "completed_iterations"
      )) {
      expect_identical(
        ridge_fit$checkpoint_state[[field]],
        gaussian_fit$checkpoint_state[[field]],
        info = paste(mode, "checkpoint", field)
      )
    }
  }
})

test_that("static continuation is exact with thinning greater than one", {
  config <- ordinary_v1_reference_config()
  fixture <- config$fixtures$F03
  ridge <- ordinary_v1_reference_prior(config, "ridge")
  for (mode_index in seq_along(c(
      "fixed_rate", "learned_pseudoresidual_normalized"
    ))) {
    mode <- c(
      "fixed_rate", "learned_pseudoresidual_normalized"
    )[[mode_index]]
    result <- ordinary_v1_expect_exact_continuation(
      fixture$y, fixture$X, ridge, mode,
      seed = 82850L + mode_index, thin = 2L
    )
    expect_identical(
      result$full$checkpoint_state$completed_iterations, 12L
    )
  }
})

test_that("all four DESN prior-rate cells equal direct static draws and continue", {
  config <- ordinary_v1_reference_config()
  design <- ordinary_v1_reference_desn_design(config)
  materialization_verification <-
    rqrgibbs:::.rqr_desn_materialization_verification(design)
  embedding <- rqrgibbs:::.rqr_desn_embedding_contract(
    design = design,
    design_engine = "frozen",
    materialization_verification = materialization_verification
  )
  cells <- data.frame(
    prior_id = rep(c("ridge", "rhs_ns_fixed"), each = 2L),
    mode = rep(
      c("fixed_rate", "learned_pseudoresidual_normalized"), 2L
    ),
    seed = 82821:82824,
    stringsAsFactors = FALSE
  )
  for (index in seq_len(nrow(cells))) {
    prior <- ordinary_v1_reference_prior(
      config, cells$prior_id[[index]]
    )
    direct <- ordinary_v1_reference_fit(
      design$y, design$X, prior, cells$mode[[index]],
      seed = cells$seed[[index]], n_mcmc = 2L,
      embedding_contract = embedding
    )
    wrapper <- rqr_desn_fit(
      design = design, coverage_level = 0.8,
      design_engine = "frozen", inference = "mcmc",
      learning_rate = 1, lambda_initial = 1,
      loss_reference_scale = 1,
      learning_rate_mode = cells$mode[[index]],
      lambda_prior = list(shape = 4, rate = 4),
      numerical_policy = "fail",
      mcmc_args = list(
        beta_prior_obj = prior,
        root_swap_probability = 0.5,
        n_burn = 0L, n_mcmc = 2L, thin = 1L,
        seed = cells$seed[[index]],
        store_latent_draws = TRUE,
        store_prior_state_draws = identical(prior$type, "rhs_ns")
      )
    )
    expect_identical(wrapper$fit$samp.beta_root1, direct$samp.beta_root1)
    expect_identical(wrapper$fit$samp.beta_root2, direct$samp.beta_root2)
    expect_identical(wrapper$fit$samp.lambda, direct$samp.lambda)
    expect_identical(wrapper$fit$samp.latent_v, direct$samp.latent_v)
    expect_identical(
      wrapper$fit$samp.beta_prior_state_root1,
      direct$samp.beta_prior_state_root1
    )
    expect_identical(
      wrapper$fit$samp.beta_prior_state_root2,
      direct$samp.beta_prior_state_root2
    )
    ordinary_v1_expect_exact_continuation(
      design$y, design$X, prior, cells$mode[[index]],
      cells$seed[[index]], embedding_contract = embedding
    )
  }
})
