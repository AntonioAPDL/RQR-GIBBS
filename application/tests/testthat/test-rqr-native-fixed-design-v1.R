fd_v1_get <- function(name) {
  get(name, envir = asNamespace("rqrgibbs"), inherits = FALSE)
}

fd_v1_fixture <- function(missing = FALSE) {
  x <- seq(-1.25, 1.25, length.out = 10L)
  z <- c(-0.4, 0.9, -1.1, 0.2, 1.3, -0.7, 0.5, -1.4, 0.8, 0.1)
  X <- cbind("(Intercept)" = 1, x = x, z = z)
  rownames(X) <- sprintf("row_%02d", seq_len(nrow(X)))
  y <- 0.35 + 0.7 * x - 0.25 * z +
    c(-0.30, 0.15, -0.10, 0.25, -0.05, 0.12, -0.18, 0.28, -0.08, 0.20)
  if (missing) y[c(3L, 8L)] <- NA_real_
  list(y = y, X = X)
}

fd_v1_fit <- function(
    fixture, prior, mode = "fixed_rate", n_mcmc = 4L, seed = 761L,
    n_burn = 0L) {
  fd_v1_get("rqr_mcmc_fit")(
    y = fixture$y,
    X = fixture$X,
    coverage_level = 0.8,
    learning_rate = 1.7,
    lambda_initial = 1.2,
    loss_reference_scale = 2.5,
    learning_rate_mode = mode,
    lambda_prior = list(shape = 3.5, rate = 2.25),
    beta_prior_obj = prior,
    numerical_policy = "fail",
    root_swap_probability = 0.5,
    mcmc_control = list(
      n_burn = n_burn,
      n_mcmc = n_mcmc,
      thin = 1L,
      seed = seed,
      store_latent_draws = TRUE,
      store_prior_state_draws = TRUE
    )
  )
}

fd_v1_bind_matrix <- function(segments, field) {
  do.call(rbind, lapply(segments, `[[`, field))
}

fd_v1_bind_vector <- function(segments, field) {
  unlist(lapply(segments, `[[`, field), recursive = TRUE, use.names = FALSE)
}

fd_v1_bind_diagnostic <- function(segments, field) {
  unlist(
    lapply(segments, function(segment) segment$diagnostics[[field]]),
    recursive = TRUE,
    use.names = FALSE
  )
}

fd_v1_bind_list <- function(segments, field, diagnostic = FALSE) {
  values <- lapply(segments, function(segment) {
    if (diagnostic) segment$diagnostics[[field]] else segment[[field]]
  })
  unlist(values, recursive = FALSE, use.names = FALSE)
}

fd_v1_expect_exact_2_plus_2_plus_2 <- function(prior, mode, seed) {
  fixture <- fd_v1_fixture(missing = TRUE)
  continue_fit <- fd_v1_get("rqr_mcmc_continue")
  full <- fd_v1_fit(
    fixture, prior, mode = mode, n_mcmc = 6L, seed = seed
  )
  segment0 <- fd_v1_fit(
    fixture, prior, mode = mode, n_mcmc = 2L, seed = seed
  )
  segment1 <- continue_fit(segment0, n_mcmc = 2L)
  segment2 <- continue_fit(segment1, n_mcmc = 2L)
  segments <- list(segment0, segment1, segment2)

  for (field in c(
      "samp.beta_root1", "samp.beta_root2", "samp.latent_v"
    )) {
    expect_identical(
      full[[field]],
      fd_v1_bind_matrix(segments, field),
      info = paste("matrix field", field)
    )
  }
  expect_identical(
    full$samp.lambda,
    fd_v1_bind_vector(segments, "samp.lambda")
  )
  for (field in c(
      "samp.beta_prior_state_root1", "samp.beta_prior_state_root2"
    )) {
    expect_identical(
      full[[field]],
      fd_v1_bind_list(segments, field),
      info = paste("prior-state field", field)
    )
  }

  vector_diagnostics <- c(
    "loss_trace", "scaled_loss_trace", "weighted_loss_trace",
    "lambda_trace", "effective_learning_rate_trace",
    "lambda_post_shape_trace", "lambda_post_rate_trace",
    "precision_strategy_root1", "precision_strategy_root2",
    "root_swap_trace"
  )
  for (field in vector_diagnostics) {
    expect_identical(
      full$diagnostics[[field]],
      fd_v1_bind_diagnostic(segments, field),
      info = paste("diagnostic field", field)
    )
  }
  for (field in c("prior_stats_root1", "prior_stats_root2")) {
    expect_identical(
      full$diagnostics[[field]],
      fd_v1_bind_list(segments, field, diagnostic = TRUE),
      info = paste("prior diagnostic field", field)
    )
  }

  expect_identical(
    full$diagnostics$numerical_repairs,
    segments[[1L]]$diagnostics$numerical_repairs
  )
  expect_identical(
    full$diagnostics$prior_numerical_repair_count,
    sum(vapply(
      segments,
      function(segment) {
        segment$diagnostics$prior_numerical_repair_count
      },
      integer(1L)
    ))
  )
  expect_identical(full$checkpoint_state, segment2$checkpoint_state)
  expect_identical(full$last, segment2$last)
  expect_identical(full$checkpoint_digest, segment2$checkpoint_digest)
  expect_identical(
    full$summary,
    fd_v1_get(".rqr_static_fit_summary")(
      full$data_contract,
      fd_v1_bind_matrix(segments, "samp.beta_root1"),
      fd_v1_bind_matrix(segments, "samp.beta_root2")
    )
  )
  expect_identical(
    segment2$continuation_history_contract$generation, 2L
  )
  expect_length(segment2$continuation_history_contract$segments, 3L)
  expect_silent(fd_v1_get(".rqr_validate_continuation_history")(segment2))
  expect_identical(
    segment2$checkpoint_state$completed_iterations, 6L
  )
  expect_identical(
    segment2$segment_schedule_contract$schema_version,
    "rqrgibbs_static_segment_schedule/1.0.0"
  )
  expect_identical(
    segment2$segment_schedule_contract$generation, 2L
  )
  expect_length(segment2$segment_schedule_contract$segments, 3L)
  expect_identical(
    vapply(
      segment2$segment_schedule_contract$segments,
      `[[`, integer(1L), "start_completed_iterations"
    ),
    c(0L, 2L, 4L)
  )
  expect_identical(
    vapply(
      segment2$segment_schedule_contract$segments,
      `[[`, integer(1L), "end_completed_iterations"
    ),
    c(2L, 4L, 6L)
  )
  expect_true(all(vapply(
    segment2$segment_schedule_contract$segments,
    `[[`, logical(1L), "ends_on_retained_draw"
  )))
  expect_silent(
    fd_v1_get(".rqr_validate_static_fit_envelope")(segment2)
  )
  invisible(list(full = full, segments = segments))
}

test_that("ordinary fixed-design public constructors and continuation are exported", {
  exports <- getNamespaceExports("rqrgibbs")
  expect_true("rqr_beta_prior" %in% exports)
  expect_true("rqr_mcmc_continue" %in% exports)
  expect_identical(
    fd_v1_get(".rqr_static_target_schema")(),
    "rqrgibbs_ordinary_target/1.0.0"
  )
  expect_identical(
    fd_v1_get(".rqr_static_fit_schema")(),
    "rqrgibbs_static_fit/1.0.0"
  )
  expect_identical(
    fd_v1_get(".rqr_static_checkpoint_schema")(),
    "rqrgibbs_static_checkpoint/1.0.0"
  )
  expect_identical(
    fd_v1_get(".rqr_static_schedule_schema")(),
    "rqrgibbs_static_segment_schedule/1.0.0"
  )
})

test_that("complete and missing-data fixed-design fits are finite and omit NA rows", {
  make_prior <- fd_v1_get("rqr_beta_prior")
  prior <- make_prior("ridge", ridge = list(tau2 = 3))
  complete <- fd_v1_fixture(missing = FALSE)
  missing <- fd_v1_fixture(missing = TRUE)

  fixed <- fd_v1_fit(
    complete, prior, mode = "fixed_rate",
    n_mcmc = 5L, n_burn = 2L, seed = 762L
  )
  expect_s3_class(fixed, "rqr_mcmc")
  expect_identical(fixed$schema_version, "rqrgibbs_static_fit/1.0.0")
  expect_identical(fixed$model_spec$tilt, 0)
  expect_identical(fixed$model_spec$learning_rate_mode, "fixed_rate")
  expect_true(all(is.finite(fixed$samp.beta_root1)))
  expect_true(all(is.finite(fixed$samp.beta_root2)))
  expect_true(all(is.finite(fixed$samp.latent_v)))
  expect_true(all(fixed$samp.latent_v > 0))
  expect_true(all(fixed$samp.lambda == 1.7 * 2.5))
  expect_true(all(
    fixed$diagnostics$effective_learning_rate_trace == 1.7
  ))
  expect_true(all(is.na(fixed$diagnostics$lambda_post_shape_trace)))
  expect_true(all(is.finite(fixed$diagnostics$loss_trace)))
  expect_true(all(fixed$summary$upper_mean >= fixed$summary$lower_mean))
  expect_true(fixed$model_spec$generalized_bayes)
  expect_false(fixed$model_spec$response_likelihood)
  expect_false(fixed$model_spec$response_prediction_contract)
  expect_identical(fixed$model_spec$n_observed, nrow(complete$X))

  learned <- fd_v1_fit(
    missing, prior,
    mode = "learned_pseudoresidual_normalized",
    n_mcmc = 5L, n_burn = 1L, seed = 763L
  )
  expect_identical(
    learned$model_spec$learning_rate_mode,
    "learned_pseudoresidual_normalized"
  )
  expect_identical(learned$model_spec$n_total, 10L)
  expect_identical(learned$model_spec$n_observed, 8L)
  expect_identical(learned$model_spec$missing_response_count, 2L)
  expect_identical(which(!learned$data_contract$observed), c(3L, 8L))
  expect_true(all(is.finite(learned$samp.beta_root1)))
  expect_true(all(is.finite(learned$samp.beta_root2)))
  expect_true(all(is.finite(learned$samp.lambda) & learned$samp.lambda > 0))
  expect_true(all(is.finite(learned$samp.latent_v)))
  expect_true(all(learned$samp.latent_v > 0))
  expect_equal(
    unique(learned$diagnostics$lambda_post_shape_trace),
    3.5 + 8
  )
  expect_true(all(
    is.finite(learned$diagnostics$lambda_post_rate_trace) &
      learned$diagnostics$lambda_post_rate_trace > 2.25
  ))
  expect_identical(learned$model_spec$lambda_power, 8)
  expect_match(learned$model_spec$inferential_target, "lambda\\^n_obs")

  observed <- missing$y[!is.na(missing$y)]
  X_observed <- missing$X[!is.na(missing$y), , drop = FALSE]
  dropped <- fd_v1_fit(
    list(y = observed, X = X_observed),
    prior,
    mode = "learned_pseudoresidual_normalized",
    n_mcmc = 5L, n_burn = 1L, seed = 763L
  )
  expect_identical(learned$samp.beta_root1, dropped$samp.beta_root1)
  expect_identical(learned$samp.beta_root2, dropped$samp.beta_root2)
  expect_identical(learned$samp.lambda, dropped$samp.lambda)
  expect_identical(
    unname(learned$samp.latent_v[
      , learned$data_contract$observed, drop = FALSE
    ]),
    unname(dropped$samp.latent_v)
  )
})

test_that("full nonzero Gaussian prior gives the dense analytic beta conditional", {
  make_prior <- fd_v1_get("rqr_beta_prior")
  validate_prior <- fd_v1_get(".rqr_prior_validate")
  initialize_prior <- fd_v1_get(".rqr_prior_initialize")
  make_data <- fd_v1_get(".rqr_fixed_design_data")
  update_beta <- fd_v1_get(".rqr_fixed_design_beta_update")

  X <- cbind(
    a = c(1, 1, 1, 1, 1),
    b = c(-1.2, -0.3, 0.4, 1.1, 1.6),
    c = c(0.7, -0.8, 1.3, -0.2, 0.5)
  )
  y <- c(-0.9, NA_real_, 0.15, 0.8, 1.25)
  data <- make_data(y, X)
  prior_precision <- matrix(c(
    2.4, 0.35, -0.15,
    0.35, 1.8, 0.25,
    -0.15, 0.25, 1.35
  ), 3L, 3L, byrow = TRUE)
  prior_mean <- c(0.6, -0.45, 0.3)
  prior <- validate_prior(
    make_prior(
      "gaussian",
      gaussian = list(
        mean = prior_mean,
        precision = prior_precision
      )
    ),
    X
  )
  prior_state <- initialize_prior(prior)
  beta_other <- c(-0.2, 0.4, -0.1)
  latent <- c(0.8, 999, 1.2, 0.65, 1.5)
  constants <- rqr_constants(0.75, learning_rate = 1.4)
  observed <- data$observed
  y_observed <- y[observed]
  X_observed <- X[observed, , drop = FALSE]
  eta_other <- drop(X_observed %*% beta_other)
  design <- X_observed * as.numeric(y_observed - eta_other)
  pseudo_response <-
    y_observed * (y_observed - eta_other) -
    constants$xi * latent[observed]
  weight <- 1 / (
    constants$phi * constants$sigma * latent[observed]
  )
  expected_precision <-
    crossprod(design * sqrt(weight)) + prior_precision
  expected_information <- as.numeric(
    crossprod(design, weight * pseudo_response)
  ) + drop(prior_precision %*% prior_mean)
  expected_mean <- drop(
    solve(expected_precision, expected_information)
  )
  expected_covariance <- solve(expected_precision)

  set.seed(764L)
  actual <- update_beta(
    data = data,
    beta_other = beta_other,
    latent_v = latent,
    constants = constants,
    prior = prior,
    prior_state = prior_state,
    precision_beta_cfg = list(jitter_ladder = 0),
    iteration = 1L,
    root = "root1"
  )
  set.seed(764L)
  expected_draw <- expected_mean +
    backsolve(chol(expected_precision), stats::rnorm(ncol(X)))

  expect_equal(actual$mean, unname(expected_mean), tolerance = 1e-14)
  expect_equal(actual$draw, unname(expected_draw), tolerance = 1e-14)
  expect_equal(
    actual$prior_canonical$precision,
    prior_precision,
    tolerance = 1e-15
  )
  expect_equal(
    actual$prior_canonical$information,
    drop(prior_precision %*% prior_mean),
    tolerance = 1e-15
  )
  expect_equal(
    unname(expected_covariance %*% expected_precision),
    diag(ncol(X)),
    tolerance = 1e-14
  )
})

test_that("native RHS-NS runs without an exdqlm runtime dependency", {
  make_prior <- fd_v1_get("rqr_beta_prior")
  fixture <- fd_v1_fixture(missing = TRUE)
  prior <- make_prior(
    "rhs_ns",
    rhs_ns = list(
      intercept_name = "(Intercept)",
      intercept_mean = 0.1,
      intercept_precision = 0.05,
      tau0 = 0.6,
      a_zeta = 2.5,
      b_zeta = 1.4
    )
  )
  fit <- fd_v1_fit(
    fixture, prior, mode = "fixed_rate",
    n_mcmc = 5L, n_burn = 0L, seed = 765L
  )

  expect_identical(fit$model_spec$beta_prior_type, "rhs_ns")
  expect_identical(fit$beta_prior$implementation, "native")
  expect_identical(
    fit$provenance$required_external_repositories, character(0)
  )
  expect_length(fit$provenance$external_repositories, 0L)
  expect_true(all(is.finite(fit$samp.beta_root1)))
  expect_true(all(is.finite(fit$samp.beta_root2)))
  expect_length(fit$samp.beta_prior_state_root1, 5L)
  expect_length(fit$samp.beta_prior_state_root2, 5L)
  expect_identical(
    vapply(
      fit$samp.beta_prior_state_root1,
      `[[`, integer(1L), "update_count"
    ),
    seq_len(5L)
  )
  expect_identical(
    vapply(
      fit$samp.beta_prior_state_root2,
      `[[`, integer(1L), "update_count"
    ),
    seq_len(5L)
  )
  for (state in c(
      fit$samp.beta_prior_state_root1,
      fit$samp.beta_prior_state_root2
    )) {
    expect_identical(state$active_names, c("x", "z"))
    expect_true(all(is.finite(c(
      state$lambda2, state$nu, state$tau2,
      state$xi, state$zeta2
    ))))
    expect_true(all(c(
      state$lambda2, state$nu, state$tau2,
      state$xi, state$zeta2
    ) > 0))
    expect_identical(state$numerical_repair_count, 0L)
  }
})

test_that("ridge normalized target is bitwise exact under 6 versus 2+2+2", {
  prior <- fd_v1_get("rqr_beta_prior")(
    "ridge", ridge = list(tau2 = 4.5)
  )
  fd_v1_expect_exact_2_plus_2_plus_2(
    prior = prior,
    mode = "learned_pseudoresidual_normalized",
    seed = 766L
  )
})

test_that("RHS fixed target is bitwise exact under 6 versus 2+2+2", {
  prior <- fd_v1_get("rqr_beta_prior")(
    "rhs_ns",
    rhs_ns = list(
      intercept_name = "(Intercept)",
      intercept_mean = -0.05,
      intercept_precision = 0.03,
      tau0 = 0.55,
      a_zeta = 2.2,
      b_zeta = 1.6
    )
  )
  fd_v1_expect_exact_2_plus_2_plus_2(
    prior = prior,
    mode = "fixed_rate",
    seed = 767L
  )
})

test_that("static checkpoints and histories reject mutation", {
  prior <- fd_v1_get("rqr_beta_prior")(
    "ridge", ridge = list(tau2 = 3)
  )
  fit <- fd_v1_fit(
    fd_v1_fixture(missing = TRUE),
    prior,
    mode = "learned_pseudoresidual_normalized",
    n_mcmc = 2L,
    seed = 768L
  )
  continue_fit <- fd_v1_get("rqr_mcmc_continue")
  digest_object <- fd_v1_get(".rqr_digest")

  checkpoint_mutations <- list(
    beta_root1 = function(object) {
      object$checkpoint_state$beta_root1[1L] <-
        object$checkpoint_state$beta_root1[1L] + 1
      object
    },
    beta_root2 = function(object) {
      object$checkpoint_state$beta_root2[1L] <-
        object$checkpoint_state$beta_root2[1L] - 1
      object
    },
    lambda = function(object) {
      object$checkpoint_state$lambda <-
        object$checkpoint_state$lambda * 2
      object
    },
    latent = function(object) {
      object$checkpoint_state$latent_v[1L] <-
        object$checkpoint_state$latent_v[1L] * 2
      object
    },
    prior_state = function(object) {
      object$checkpoint_state$beta_prior_state1$dimension <- 999L
      object
    },
    iteration = function(object) {
      object$checkpoint_state$completed_iterations <-
        object$checkpoint_state$completed_iterations + 1L
      object
    },
    rng = function(object) {
      set.seed(999L)
      object$checkpoint_state$rng_state <- .Random.seed
      object
    }
  )
  for (name in names(checkpoint_mutations)) {
    altered <- checkpoint_mutations[[name]](fit)
    expect_error(
      continue_fit(altered, n_mcmc = 1L),
      "checkpoint digest",
      info = name
    )
  }

  history_mutations <- list(
    generation = function(object) {
      object$continuation_history_contract$generation <- 99L
      object
    },
    promotion = function(object) {
      object$continuation_history_contract$promotion_eligible <-
        !object$continuation_history_contract$promotion_eligible
      object
    },
    ledger = function(object) {
      object$continuation_history_contract$
        cumulative_environment_mismatch_ledger <- list("fabricated")
      object
    },
    digest = function(object) {
      object$continuation_history_digest <- paste(rep("0", 64), collapse = "")
      object
    }
  )
  for (name in names(history_mutations)) {
    altered <- history_mutations[[name]](fit)
    expect_error(
      continue_fit(altered, n_mcmc = 1L),
      "history contract or digest",
      info = name
    )
  }

  impossible_history <- fit
  impossible_history$continuation_history_contract$segments[[1L]]$
    generation <- 1L
  impossible_history$continuation_history_digest <- digest_object(
    impossible_history$continuation_history_contract
  )
  expect_error(
    continue_fit(impossible_history, n_mcmc = 1L),
    "structurally invalid"
  )

  fractional_iteration <- fit
  fractional_iteration$checkpoint_state$completed_iterations <- 0.5
  fractional_iteration$checkpoint_digest <- digest_object(
    fractional_iteration$checkpoint_state
  )
  final_segment <- length(
    fractional_iteration$continuation_history_contract$segments
  )
  fractional_iteration$continuation_history_contract$
    segments[[final_segment]]$checkpoint_digest <-
      fractional_iteration$checkpoint_digest
  fractional_iteration$continuation_history_digest <- digest_object(
    fractional_iteration$continuation_history_contract
  )
  expect_error(
    continue_fit(fractional_iteration, n_mcmc = 1L),
    "completed_iterations"
  )

  invalid_rng <- fit
  invalid_rng$checkpoint_state$rng_state <- c(403L, 1L)
  invalid_rng$checkpoint_digest <- digest_object(
    invalid_rng$checkpoint_state
  )
  invalid_rng$last <- invalid_rng$checkpoint_state
  invalid_rng$continuation_history_contract$
    segments[[final_segment]]$checkpoint_digest <-
      invalid_rng$checkpoint_digest
  invalid_rng$continuation_history_digest <- digest_object(
    invalid_rng$continuation_history_contract
  )
  invalid_rng$segment_schedule_contract$
    segments[[final_segment]]$checkpoint_digest <-
      invalid_rng$checkpoint_digest
  invalid_rng$segment_schedule_digest <- digest_object(
    invalid_rng$segment_schedule_contract
  )
  expect_error(
    continue_fit(invalid_rng, n_mcmc = 1L),
    "rng_state"
  )

  changed_data <- fit
  changed_data$y[1L] <- changed_data$y[1L] + 0.25
  expect_error(
    continue_fit(changed_data, n_mcmc = 1L),
    "data or design digest"
  )
  changed_target <- fit
  changed_target$model_spec$coverage_level <- 0.7
  expect_error(
    continue_fit(changed_target, n_mcmc = 1L),
    "model, target, or prior digest"
  )

  changed_mask <- fit
  changed_mask$data_contract$observed[1L] <- FALSE
  expect_error(
    continue_fit(changed_mask, n_mcmc = 1L),
    "data or design digest"
  )

  rehash_checkpoint_and_history <- function(object) {
    object$checkpoint_digest <- digest_object(object$checkpoint_state)
    object$last <- object$checkpoint_state
    last_index <- length(
      object$continuation_history_contract$segments
    )
    object$continuation_history_contract$segments[[last_index]]$
      checkpoint_digest <- object$checkpoint_digest
    object$continuation_history_digest <- digest_object(
      object$continuation_history_contract
    )
    object$segment_schedule_contract$segments[[last_index]]$
      checkpoint_digest <- object$checkpoint_digest
    object$segment_schedule_digest <- digest_object(
      object$segment_schedule_contract
    )
    object
  }

  fixed_fit <- fd_v1_fit(
    fd_v1_fixture(missing = TRUE),
    prior,
    mode = "fixed_rate",
    n_mcmc = 2L,
    seed = 771L
  )
  impossible_fixed_lambda <- fixed_fit
  impossible_fixed_lambda$checkpoint_state$lambda <-
    2 * impossible_fixed_lambda$checkpoint_state$lambda
  impossible_fixed_lambda <- rehash_checkpoint_and_history(
    impossible_fixed_lambda
  )
  expect_error(
    continue_fit(impossible_fixed_lambda, n_mcmc = 1L),
    "fixed.*lambda|lambda.*fixed|terminal checkpoint roots or lambda"
  )

  impossible_missing_placeholder <- fixed_fit
  missing_index <- which(
    !impossible_missing_placeholder$data_contract$observed
  )[1L]
  impossible_missing_placeholder$checkpoint_state$
    latent_v[missing_index] <-
      impossible_missing_placeholder$checkpoint_state$
        latent_v[missing_index] * 2
  impossible_missing_placeholder <- rehash_checkpoint_and_history(
    impossible_missing_placeholder
  )
  expect_error(
    continue_fit(
      impossible_missing_placeholder, n_mcmc = 1L
    ),
    "Missing-site checkpoint latent placeholders"
  )

  impossible_ridge_state <- fixed_fit
  impossible_ridge_state$checkpoint_state$
    beta_prior_state1$fabricated <- TRUE
  impossible_ridge_state <- rehash_checkpoint_and_history(
    impossible_ridge_state
  )
  expect_error(
    continue_fit(impossible_ridge_state, n_mcmc = 1L),
    "non-stateful coefficient-prior state"
  )

  rhs_prior <- fd_v1_get("rqr_beta_prior")(
    "rhs_ns",
    rhs_ns = list(
      intercept_name = "(Intercept)",
      tau0 = 0.5,
      a_zeta = 2,
      b_zeta = 1
    )
  )
  rhs_fit <- fd_v1_fit(
    fd_v1_fixture(missing = TRUE),
    rhs_prior,
    mode = "fixed_rate",
    n_mcmc = 2L,
    seed = 772L
  )
  impossible_rhs_count <- rhs_fit
  impossible_rhs_count$checkpoint_state$
    beta_prior_state1$update_count <- 0L
  impossible_rhs_count <- rehash_checkpoint_and_history(
    impossible_rhs_count
  )
  expect_error(
    continue_fit(impossible_rhs_count, n_mcmc = 1L),
    "update_count|completed_iterations"
  )

  recomputed_beta <- fit
  recomputed_beta$checkpoint_state$beta_root1[1L] <-
    recomputed_beta$checkpoint_state$beta_root1[1L] + 0.5
  recomputed_beta <- rehash_checkpoint_and_history(recomputed_beta)
  expect_error(
    fd_v1_get(".rqr_validate_static_fit_envelope")(recomputed_beta),
    "terminal checkpoint roots or lambda"
  )

  recomputed_lambda <- fit
  recomputed_lambda$checkpoint_state$lambda <-
    recomputed_lambda$checkpoint_state$lambda * 1.25
  recomputed_lambda <- rehash_checkpoint_and_history(
    recomputed_lambda
  )
  expect_error(
    fd_v1_get(".rqr_validate_static_fit_envelope")(
      recomputed_lambda
    ),
    "terminal checkpoint roots or lambda"
  )

  recomputed_completed <- fit
  recomputed_completed$checkpoint_state$completed_iterations <-
    recomputed_completed$checkpoint_state$completed_iterations + 1L
  recomputed_completed <- rehash_checkpoint_and_history(
    recomputed_completed
  )
  expect_error(
    fd_v1_get(".rqr_validate_static_fit_envelope")(
      recomputed_completed
    ),
    "final segment schedule"
  )

  changed_schedule <- fit
  changed_schedule$segment_schedule_contract$
    segments[[1L]]$thin <- 2L
  changed_schedule$segment_schedule_digest <- digest_object(
    changed_schedule$segment_schedule_contract
  )
  expect_error(
    fd_v1_get(".rqr_validate_static_fit_envelope")(changed_schedule),
    "segment schedule 0 is structurally invalid"
  )

  changed_final_draw <- fit
  changed_final_draw$samp.beta_root2[
    nrow(changed_final_draw$samp.beta_root2), 1L
  ] <- changed_final_draw$samp.beta_root2[
    nrow(changed_final_draw$samp.beta_root2), 1L
  ] - 0.25
  expect_error(
    fd_v1_get(".rqr_validate_static_fit_envelope")(
      changed_final_draw
    ),
    "terminal checkpoint roots or lambda"
  )

  changed_latent_draw <- fit
  changed_latent_draw$samp.latent_v[
    nrow(changed_latent_draw$samp.latent_v), 1L
  ] <- changed_latent_draw$samp.latent_v[
    nrow(changed_latent_draw$samp.latent_v), 1L
  ] * 1.25
  expect_error(
    fd_v1_get(".rqr_validate_static_fit_envelope")(
      changed_latent_draw
    ),
    "stored terminal latent state"
  )

  changed_prior_draw <- fit
  final_prior_index <- length(
    changed_prior_draw$samp.beta_prior_state_root1
  )
  changed_prior_draw$samp.beta_prior_state_root1[[final_prior_index]] <-
    utils::modifyList(
      changed_prior_draw$samp.beta_prior_state_root1[[final_prior_index]],
      list(fabricated = TRUE)
    )
  expect_error(
    fd_v1_get(".rqr_validate_static_fit_envelope")(
      changed_prior_draw
    ),
    "stored terminal coefficient-prior state"
  )
})

test_that("prediction requires exact fitted feature names and valid named draws", {
  make_prior <- fd_v1_get("rqr_beta_prior")
  fit <- fd_v1_fit(
    fd_v1_fixture(missing = FALSE),
    make_prior("ridge", ridge = list(tau2 = 3)),
    mode = "fixed_rate",
    n_mcmc = 3L,
    seed = 769L
  )
  predict_fit <- fd_v1_get("predict_interval.rqr_mcmc")
  X_new <- rbind(
    first = c("(Intercept)" = 1, x = -0.25, z = 0.4),
    second = c("(Intercept)" = 1, x = 0.75, z = -0.6)
  )
  beta1 <- rbind(
    c("(Intercept)" = 0.1, x = 0.5, z = -0.2),
    c("(Intercept)" = -0.2, x = 0.3, z = 0.4)
  )
  beta2 <- rbind(
    c("(Intercept)" = 1.1, x = -0.1, z = 0.2),
    c("(Intercept)" = 0.7, x = 0.6, z = -0.3)
  )
  draws <- list(beta_root1 = beta1, beta_root2 = beta2)
  prediction <- predict_fit(fit, X_new = X_new, draws = draws)
  eta1 <- X_new %*% t(beta1)
  eta2 <- X_new %*% t(beta2)
  expect_identical(
    prediction$schema_version,
    "rqrgibbs_interval_prediction/1.0.0"
  )
  expect_identical(prediction$lower_draws, pmin(eta1, eta2))
  expect_identical(prediction$upper_draws, pmax(eta1, eta2))
  expect_true(all(prediction$width_draws >= 0))
  expect_false(prediction$response_predictive_draws)
  expect_identical(prediction$fit_checkpoint_digest, fit$checkpoint_digest)
  expect_match(prediction$interpretation, "no response draw")
  expect_identical(
    names(prediction$draws),
    c("beta_root1", "beta_root2", "lambda", "draw_index", "nd")
  )
  expect_null(prediction$draws$lambda)
  expect_identical(prediction$draws$draw_index, rep(NA_integer_, 2L))
  expect_identical(prediction$draws$nd, 2L)
  expect_silent(
    predict_fit(fit, X_new = X_new, draws = prediction$draws)
  )

  expect_error(
    predict_fit(fit, X_new = X_new[, c(1L, 3L, 2L)], draws = draws),
    "exact fitted order"
  )
  expect_error(
    predict_fit(
      fit,
      X_new = unname(X_new),
      draws = draws
    ),
    "exact fitted order"
  )
  expect_error(
    predict_fit(
      fit,
      X_new = as.data.frame(X_new),
      draws = draws
    ),
    "numeric matrix"
  )

  reversed_draws <- list(
    beta_root1 = beta1[, c(1L, 3L, 2L), drop = FALSE],
    beta_root2 = beta2[, c(1L, 3L, 2L), drop = FALSE]
  )
  expect_error(
    predict_fit(fit, X_new = X_new, draws = reversed_draws),
    "fitted (column )?order"
  )
  empty_draws <- list(
    beta_root1 = beta1[FALSE, , drop = FALSE],
    beta_root2 = beta2[FALSE, , drop = FALSE]
  )
  expect_error(
    predict_fit(fit, X_new = X_new, draws = empty_draws),
    "root-coefficient draws are invalid"
  )
})

test_that("static draw extraction uses seeds only for subsampling", {
  prior <- fd_v1_get("rqr_beta_prior")(
    "ridge", ridge = list(tau2 = 3)
  )
  fit <- fd_v1_fit(
    fd_v1_fixture(missing = FALSE),
    prior,
    mode = "fixed_rate",
    n_mcmc = 3L,
    seed = 778L
  )
  posterior_draws <- fd_v1_get("rqr_posterior_draws.rqr_mcmc")
  validate_draws <- fd_v1_get(".rqr_validate_static_draws")

  set.seed(1778L)
  rng_before <- .Random.seed
  expect_error(
    posterior_draws(fit, seed = 91L),
    "seed must be NULL when nd is NULL"
  )
  expect_identical(.Random.seed, rng_before)

  all_draws <- posterior_draws(fit)
  expect_identical(
    names(all_draws),
    c("beta_root1", "beta_root2", "lambda", "draw_index", "nd")
  )
  expect_identical(all_draws$draw_index, seq_len(3L))
  expect_identical(all_draws$nd, 3L)
  expect_silent(validate_draws(fit, all_draws))

  sampled <- posterior_draws(fit, nd = 5L, seed = 92L)
  expect_identical(sampled$nd, 5L)
  expect_length(sampled$draw_index, 5L)
  expect_true(all(sampled$draw_index %in% seq_len(3L)))
  expect_true(anyDuplicated(sampled$draw_index) > 0L)
  expect_silent(validate_draws(fit, sampled))
})

test_that("static explicit draws fail closed and canonicalize minimal inputs", {
  prior <- fd_v1_get("rqr_beta_prior")(
    "ridge", ridge = list(tau2 = 3)
  )
  fixture <- fd_v1_fixture(missing = FALSE)
  fit <- fd_v1_fit(
    fixture,
    prior,
    mode = "fixed_rate",
    n_mcmc = 3L,
    seed = 779L
  )
  predict_fit <- fd_v1_get("predict_interval.rqr_mcmc")
  validate_draws <- fd_v1_get(".rqr_validate_static_draws")
  X_new <- fixture$X[1:2, , drop = FALSE]
  minimal <- list(
    beta_root1 = fit$samp.beta_root1[1:2, , drop = FALSE],
    beta_root2 = fit$samp.beta_root2[1:2, , drop = FALSE]
  )

  canonical <- validate_draws(fit, minimal)
  expect_identical(
    names(canonical),
    c("beta_root1", "beta_root2", "lambda", "draw_index", "nd")
  )
  expect_null(canonical$lambda)
  expect_identical(canonical$draw_index, rep(NA_integer_, 2L))
  expect_identical(canonical$nd, 2L)
  expect_silent(predict_fit(fit, X_new = X_new, draws = canonical))

  set.seed(1779L)
  rng_before <- .Random.seed
  expect_error(
    predict_fit(
      fit, X_new = X_new, draws = minimal, seed = 99L
    ),
    "nd and seed must be NULL"
  )
  expect_identical(.Random.seed, rng_before)
  expect_error(
    predict_fit(
      fit, X_new = X_new, draws = minimal, nd = 1L
    ),
    "nd and seed must be NULL"
  )

  with_index <- c(
    minimal,
    list(
      lambda = fit$samp.lambda[1:2],
      draw_index = 1:2,
      nd = 2L
    )
  )
  expect_silent(validate_draws(fit, with_index))

  repeated <- list(
    beta_root1 = fit$samp.beta_root1[c(1L, 2L, 3L, 1L), , drop = FALSE],
    beta_root2 = fit$samp.beta_root2[c(1L, 2L, 3L, 1L), , drop = FALSE],
    lambda = fit$samp.lambda[c(1L, 2L, 3L, 1L)],
    draw_index = c(1L, 2L, 3L, 1L),
    nd = 4L
  )
  expect_silent(validate_draws(fit, repeated))

  expect_error(
    validate_draws(fit, c(minimal, list(typo = TRUE))),
    "unsupported fields: typo"
  )
  unnamed <- unname(minimal)
  expect_error(validate_draws(fit, unnamed), "fully named")
  duplicated <- minimal
  names(duplicated) <- c("beta_root1", "beta_root1")
  expect_error(validate_draws(fit, duplicated), "duplicate fields")

  data_frame_draws <- minimal
  data_frame_draws$beta_root1 <- as.data.frame(
    data_frame_draws$beta_root1
  )
  expect_error(
    validate_draws(fit, data_frame_draws),
    "plain numeric matrices"
  )
  classed_draws <- minimal
  class(classed_draws$beta_root1) <- c("custom_matrix", "matrix")
  expect_error(
    validate_draws(fit, classed_draws),
    "plain numeric matrices"
  )
  matrix_lambda <- c(
    minimal,
    list(lambda = matrix(fit$samp.lambda[1:2], ncol = 1L))
  )
  expect_error(validate_draws(fit, matrix_lambda), "numeric vector")
  bad_lambda <- c(minimal, list(lambda = c(1, 0)))
  expect_error(validate_draws(fit, bad_lambda), "finite positive")

  expect_error(
    validate_draws(
      fit, c(minimal, list(draw_index = c(1, 2), nd = 2L))
    ),
    "integer vector"
  )
  expect_error(
    validate_draws(
      fit, c(minimal, list(draw_index = c(1L, 1L), nd = 2L))
    ),
    "must be unique"
  )
  expect_error(
    validate_draws(
      fit, c(minimal, list(draw_index = c(1L, 4L), nd = 2L))
    ),
    "retained-draw range"
  )
  expect_error(
    validate_draws(
      fit, c(minimal, list(draw_index = c(1L, NA_integer_), nd = 2L))
    ),
    "retained-draw range"
  )
  expect_error(
    validate_draws(
      fit, c(minimal, list(draw_index = 1:2, nd = 1L))
    ),
    "must equal the number"
  )

  wrong_index <- with_index
  wrong_index$beta_root1[1L, 1L] <-
    wrong_index$beta_root1[1L, 1L] + 0.1
  expect_error(
    validate_draws(fit, wrong_index),
    "do not identify the supplied coefficient draws"
  )
  wrong_lambda <- with_index
  wrong_lambda$lambda[1L] <- wrong_lambda$lambda[1L] + 0.1
  expect_error(
    validate_draws(fit, wrong_lambda),
    "do not identify the supplied lambda draws"
  )
})

test_that("static read-only consumers enforce envelope integrity without environment checks", {
  prior <- fd_v1_get("rqr_beta_prior")(
    "ridge", ridge = list(tau2 = 3)
  )
  fixture <- fd_v1_fixture(missing = FALSE)
  fit <- fd_v1_fit(
    fixture, prior, mode = "fixed_rate",
    n_mcmc = 3L, seed = 776L
  )
  posterior_draws <- fd_v1_get("rqr_posterior_draws.rqr_mcmc")
  predict_fit <- fd_v1_get("predict_interval.rqr_mcmc")
  print_fit <- fd_v1_get("print.rqr_mcmc")

  changed_environment_label <- fit
  changed_environment_label$provenance$R_version <-
    "environment-label-used-only-by-continuation"
  expect_silent(posterior_draws(changed_environment_label))
  expect_silent(
    predict_fit(
      changed_environment_label,
      X_new = fixture$X[1L, , drop = FALSE]
    )
  )
  expect_output(
    print_fit(changed_environment_label),
    "Ordinary RQR fixed-design MCMC fit"
  )

  corrupt <- fit
  corrupt$samp.beta_root1[nrow(corrupt$samp.beta_root1), 1L] <-
    corrupt$samp.beta_root1[nrow(corrupt$samp.beta_root1), 1L] + 1
  expect_error(
    posterior_draws(corrupt),
    "terminal checkpoint roots or lambda"
  )
  expect_error(
    predict_fit(corrupt, X_new = fixture$X[1L, , drop = FALSE]),
    "terminal checkpoint roots or lambda"
  )
  expect_error(
    print_fit(corrupt),
    "terminal checkpoint roots or lambda"
  )
})

test_that("learned_pure remains diagnostic, nonpromotable, and noncontinuable", {
  prior <- fd_v1_get("rqr_beta_prior")(
    "ridge", ridge = list(tau2 = 3)
  )
  fit <- fd_v1_fit(
    fd_v1_fixture(missing = TRUE),
    prior,
    mode = "learned_pure",
    n_mcmc = 3L,
    seed = 770L
  )

  expect_identical(fit$model_spec$learning_rate_mode, "learned_pure")
  expect_false(fit$model_spec$ordinary_v1_scope_eligible)
  expect_false(fit$model_spec$continuation_supported)
  expect_false(fit$model_spec$promotion_eligible)
  expect_null(fit$continuation_history_contract)
  expect_true(is.na(fit$continuation_history_digest))
  expect_true(all(fit$diagnostics$lambda_post_shape_trace == 3.5))
  expect_identical(fit$model_spec$lambda_power, 0)
  expect_match(fit$model_spec$inferential_target, "exp\\{-lambda")
  expect_error(
    fd_v1_get("rqr_mcmc_continue")(fit, n_mcmc = 1L),
    "not continuable"
  )
})

test_that("embedding contracts are portable data-only objects", {
  fixture <- fd_v1_fixture(missing = FALSE)
  prior <- fd_v1_get("rqr_beta_prior")(
    "ridge", ridge = list(tau2 = 3)
  )
  fit_once <- function(contract) {
    fd_v1_get("rqr_mcmc_fit")(
      y = fixture$y,
      X = fixture$X,
      coverage_level = 0.8,
      beta_prior_obj = prior,
      embedding_contract = contract,
      mcmc_control = list(n_burn = 0L, n_mcmc = 1L, seed = 773L)
    )
  }

  accepted <- list(
    schema_version = "fixture/1.0.0",
    digest = paste(rep("a", 64L), collapse = ""),
    matrix = matrix(1:4, 2L, 2L),
    flags = c(TRUE, FALSE)
  )
  expect_s3_class(fit_once(accepted), "rqr_mcmc")
  expect_error(
    fit_once(list(callback = function(x) x)),
    "data only|not permitted"
  )
  expect_error(
    fit_once(list(environment = new.env(parent = emptyenv()))),
    "data only|not permitted"
  )
  expect_error(
    fit_once(list(expression = quote(x + 1))),
    "data only|not permitted"
  )
})

test_that("fresh fits reject forged continuation bookkeeping", {
  fixture <- fd_v1_fixture(missing = FALSE)
  prior <- fd_v1_get("rqr_beta_prior")(
    "ridge", ridge = list(tau2 = 3)
  )
  expect_error(
    fd_v1_get("rqr_mcmc_fit")(
      y = fixture$y,
      X = fixture$X,
      coverage_level = 0.8,
      beta_prior_obj = prior,
      mcmc_control = list(
        n_burn = 0L, n_mcmc = 1L, seed = 774L
      ),
      init = list(completed_iterations = 20L)
    ),
    "Continuation-only init fields"
  )
  expect_error(
    fd_v1_get("rqr_mcmc_fit")(
      y = fixture$y,
      X = fixture$X,
      coverage_level = 0.8,
      beta_prior_obj = prior,
      mcmc_control = list(
        n_burn = 0L, n_mcmc = 1L, seed = 774L
      ),
      init = list(
        continued_from_checkpoint = TRUE,
        completed_iterations = 20L,
        parent_cumulative_numerical_repair_count = 0L,
        parent_chain_history_numerically_exact = TRUE
      )
    ),
    "Continuation-only init fields"
  )

  fit <- fd_v1_fit(
    fixture, prior, mode = "fixed_rate",
    n_mcmc = 2L, seed = 775L
  )
  expect_error(
    fd_v1_get("rqr_mcmc_continue")(
      fit, n_mcmc = 1L, store_latent_draws = "not-logical"
    ),
    "must be TRUE or FALSE"
  )
  expect_error(
    fd_v1_get("rqr_mcmc_continue")(
      fit, n_mcmc = 1L, store_prior_state_draws = c(TRUE, FALSE)
    ),
    "must be TRUE or FALSE"
  )

  altered_alias <- fit
  altered_alias$last$lambda <- altered_alias$last$lambda + 1
  expect_error(
    fd_v1_get("rqr_mcmc_continue")(
      altered_alias, n_mcmc = 1L
    ),
    "last-state alias"
  )
})

test_that("static continuation normalizes absent stored provenance paths", {
  fixture <- fd_v1_fixture(missing = TRUE)
  prior <- fd_v1_get("rqr_beta_prior")(
    "ridge", ridge = list(tau2 = 3)
  )
  fit <- fd_v1_fit(
    fixture, prior, mode = "fixed_rate",
    n_mcmc = 2L, seed = 776L
  )
  fit$provenance$repo_root <- NA_character_
  fit$provenance$expected_git_commit <- NA_character_
  fit$provenance$primary_runtime_attestation <- NA_character_

  continued <- fd_v1_get("rqr_mcmc_continue")(
    fit, n_mcmc = 1L
  )
  expect_s3_class(continued, "rqr_mcmc")
  expect_identical(
    continued$checkpoint_state$completed_iterations, 3L
  )
  expect_length(
    continued$continuation_contract$environment_mismatches, 0L
  )
  expect_false(
    continued$continuation_contract$environment_override_used
  )
})

test_that("fixed-design MCMC controls fail closed on unknown fields", {
  X <- cbind(intercept = 1, slope = seq(-1, 1, length.out = 6L))
  y <- seq_len(6L) / 10
  expect_error(
    fd_v1_get("rqr_mcmc_fit")(
      y = y, X = X, coverage_level = 0.8,
      mcmc_control = list(n_mcm = 2L)
    ),
    "unsupported fields: n_mcm"
  )
  expect_error(
    fd_v1_get("rqr_mcmc_fit")(
      y = y, X = X, coverage_level = 0.8,
      mcmc_control = list(2L)
    ),
    "fully named"
  )
  expect_error(
    fd_v1_get("rqr_mcmc_fit")(
      y = y, X = X, coverage_level = 0.8,
      mcmc_control = list(
        n_mcmc = 2L, store_latent_draws = "yes"
      )
    ),
    "must be TRUE or FALSE"
  )
  set.seed(1311L)
  rng_before_late_control <- .Random.seed
  expect_error(
    fd_v1_get("rqr_mcmc_fit")(
      y = y, X = X, coverage_level = 0.8,
      mcmc_control = list(
        n_burn = 0L, n_mcmc = 1L, seed = 99L,
        precision_beta = list(trace = 1)
      )
    ),
    "must be TRUE or FALSE"
  )
  expect_identical(.Random.seed, rng_before_late_control)
  expect_error(
    fd_v1_get("rqr_mcmc_fit")(
      y = y, X = X, coverage_level = 0.8,
      mcmc_control = list(
        n_mcmc = 2L, seed = 1L, rng_seed = 1L
      )
    ),
    "both seed and rng_seed"
  )
  set.seed(1310L)
  rng_before_seed_state_conflict <- .Random.seed
  expect_error(
    fd_v1_get("rqr_mcmc_fit")(
      y = y, X = X, coverage_level = 0.8,
      mcmc_control = list(
        n_burn = 0L, n_mcmc = 2L, seed = 1L
      ),
      init = list(rng_state = rng_before_seed_state_conflict)
    ),
    "seed or init\\$rng_state, not both"
  )
  expect_identical(.Random.seed, rng_before_seed_state_conflict)
  expect_error(
    fd_v1_get("rqr_mcmc_fit")(
      y = y, X = X, coverage_level = 0.8,
      mcmc_control = list(
        n_burn = 0L, n_mcmc = 2L, rng_seed = 1L
      ),
      init = list(rng_state = rng_before_seed_state_conflict)
    ),
    "seed or init\\$rng_state, not both"
  )
  expect_identical(.Random.seed, rng_before_seed_state_conflict)
  expect_error(
    fd_v1_get("rqr_mcmc_fit")(
      y = y, X = X, coverage_level = 0.8,
      mcmc_control = list(n_mcmc = 2L),
      init = list(beta1 = c(0, 0), beta_root1 = c(0, 0))
    ),
    "canonical fields and their legacy aliases"
  )
  expect_error(
    fd_v1_get("rqr_mcmc_fit")(
      y = y, X = X, coverage_level = 0.8,
      root_swap_probability = c(0.25, 0.75),
      mcmc_control = list(n_burn = 0L, n_mcmc = 1L)
    ),
    "numeric scalar"
  )
  expect_error(
    fd_v1_get("rqr_mcmc_fit")(
      y = y, X = X, coverage_level = c(0.8, 0.9),
      mcmc_control = list(n_burn = 0L, n_mcmc = 1L)
    ),
    "numeric scalar"
  )
  expect_error(
    fd_v1_get("rqr_mcmc_fit")(
      y = y, X = X, coverage_level = 0.8,
      lambda_prior = list(shpae = 99),
      learning_rate_mode = "learned_pseudoresidual_normalized",
      mcmc_control = list(n_burn = 0L, n_mcmc = 1L)
    ),
    "unsupported fields"
  )
  expect_error(
    fd_v1_get("rqr_mcmc_fit")(
      y = y, X = X, coverage_level = 0.8,
      provenance_control = list(repo_rooot = tempdir()),
      mcmc_control = list(n_burn = 0L, n_mcmc = 1L)
    ),
    "unsupported fields: repo_rooot"
  )
  expect_error(
    fd_v1_get("rqr_mcmc_fit")(
      y = y, X = X, coverage_level = 0.8,
      provenance_control = list(
        external_repositories = list(
          reference = list(source_subdir = c(".", "application"))
        )
      ),
      mcmc_control = list(n_burn = 0L, n_mcmc = 1L)
    ),
    "source_subdir"
  )
  expect_error(
    fd_v1_get("rqr_mcmc_fit")(
      y = y, X = X, coverage_level = 0.8,
      provenance_control = list(
        external_repositories = list(),
        required_external_repositories = "missing"
      ),
      mcmc_control = list(n_burn = 0L, n_mcmc = 1L)
    ),
    "needs a specification"
  )
  expect_error(
    fd_v1_get("rqr_mcmc_fit")(
      y = y, X = X, coverage_level = 0.8,
      mcmc_control = list(
        n_burn = 0L, n_mcmc = 1L,
        precision_beta = list(jiter_ladder = 0)
      )
    ),
    "unsupported fields: jiter_ladder"
  )
  expect_error(
    fd_v1_get("rqr_mcmc_fit")(
      y = y, X = X, coverage_level = 0.8,
      mcmc_control = list(
        n_burn = 0L, n_mcmc = 1L,
        precision_beta = list(trace = 1)
      )
    ),
    "must be TRUE or FALSE"
  )
  expect_error(
    fd_v1_get("rqr_mcmc_fit")(
      y = y, X = X, coverage_level = 0.8,
      mcmc_control = list(
        n_burn = 0L, n_mcmc = 1L,
        precision_beta = list(enabled = FALSE)
      )
    ),
    "requires enabled=TRUE"
  )
  expect_error(
    fd_v1_get("rqr_mcmc_fit")(
      y = y, X = X, coverage_level = 0.8,
      beta_prior_obj = fd_v1_get("rqr_beta_prior")(
        "ridge", ridge = list(tau2 = 2)
      ),
      mcmc_control = list(
        n_burn = 0L, n_mcmc = 1L,
        intercept_name = "intercept"
      )
    ),
    "only when it exactly matches"
  )
  expect_error(
    fd_v1_get(".rqr_require_external_repository")(
      list(), c("exdqlm", "other"), strrep("a", 40L),
      runtime_package = "exdqlm"
    ),
    "one nonempty string"
  )
  expect_error(
    fd_v1_get(".rqr_require_external_repository")(
      list(), "exdqlm", c(strrep("a", 40L), strrep("b", 40L)),
      runtime_package = "exdqlm"
    ),
    "one complete Git SHA"
  )
})
