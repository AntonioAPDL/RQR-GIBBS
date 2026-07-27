static_output_fixture <- function() {
  X <- cbind(
    "(Intercept)" = 1,
    x = seq(-1, 1, length.out = 8)
  )
  y <- c(-1.2, -0.8, -0.3, 0.1, 0.45, 0.9, 1.3, 1.8)
  list(y = y, X = X)
}

static_output_fit <- function(seed = 9201L) {
  fixture <- static_output_fixture()
  rqr_mcmc_fit(
    y = fixture$y,
    X = fixture$X,
    coverage_level = 0.8,
    beta_prior_obj = rqr_beta_prior(
      "ridge", ridge = list(tau2 = 3)
    ),
    mcmc_control = list(
      n_burn = 1L, n_mcmc = 4L, thin = 1L, seed = seed
    )
  )
}

rehash_static_draws <- function(draws) {
  draws$semantic_digest <- rqrgibbs:::.rqr_digest(
    draws[setdiff(names(draws), "semantic_digest")]
  )
  draws
}

rehash_static_prediction <- function(prediction) {
  prediction$semantic_digest <- rqrgibbs:::.rqr_digest(
    prediction[setdiff(names(prediction), "semantic_digest")]
  )
  prediction
}

test_that("native static draws bind source and exact selection semantics", {
  fit <- static_output_fit()
  validate <- rqrgibbs:::.rqr_validate_static_draws

  set.seed(9202L)
  rng_before_all <- .Random.seed
  all_draws <- rqr_posterior_draws(fit)
  expect_identical(.Random.seed, rng_before_all)
  expect_identical(
    class(all_draws), c("rqr_static_draws", "list")
  )
  expect_identical(
    names(attributes(all_draws)), c("names", "class")
  )
  expect_identical(
    all_draws$schema_version, "rqrgibbs_static_draws/1.0.0"
  )
  expect_identical(
    all_draws$source$source_type, "native_retained_draws"
  )
  expect_identical(
    all_draws$source$fit_checkpoint_digest,
    fit$checkpoint_digest
  )
  expect_identical(
    all_draws$source$retained_draws_digest,
    fit$retained_draws_digest
  )
  expect_identical(
    all_draws$source$target_digest,
    fit$checkpoint_state$target_digest
  )
  expect_identical(
    all_draws$source$data_digest,
    fit$data_contract$data_digest
  )
  expect_identical(
    all_draws$source$design_digest,
    fit$data_contract$design_digest
  )
  expect_identical(
    all_draws$selection$method, "all_retained"
  )
  expect_true(all_draws$source_bound)
  expect_false(all_draws$response_predictive_draws)
  expect_identical(validate(fit, all_draws), all_draws)

  sampled <- rqr_posterior_draws(fit, nd = 2L, seed = 9203L)
  expect_identical(
    sampled$selection$method, "random_subsample"
  )
  expect_true(sampled$selection$seed_supplied)
  expect_identical(sampled$selection$seed, 9203L)
  expect_true(sampled$selection$reproducibility_bound)
  expect_true(is.integer(sampled$selection$rng_state_before))
  expect_true(is.integer(sampled$selection$rng_state_after))

  set.seed(9204L)
  rng_before_validation <- .Random.seed
  expect_identical(validate(fit, sampled), sampled)
  expect_identical(.Random.seed, rng_before_validation)

  sampled_again <- rqr_posterior_draws(
    fit, nd = 2L, seed = 9203L
  )
  expect_identical(sampled_again, sampled)

  local({
    old_exists <- exists(
      ".Random.seed", envir = .GlobalEnv, inherits = FALSE
    )
    old_state <- if (old_exists) .Random.seed else NULL
    on.exit({
      if (old_exists) {
        assign(".Random.seed", old_state, envir = .GlobalEnv)
      } else if (exists(
        ".Random.seed", envir = .GlobalEnv, inherits = FALSE
      )) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    }, add = TRUE)
    if (old_exists) rm(".Random.seed", envir = .GlobalEnv)
    ambient_uninitialized <- rqr_posterior_draws(
      fit, nd = 2L
    )
    expect_null(
      ambient_uninitialized$selection$rng_state_before
    )
    expect_true(is.integer(
      ambient_uninitialized$selection$rng_state_after
    ))
    expect_false(
      ambient_uninitialized$selection$reproducibility_bound
    )
    expect_false(
      ambient_uninitialized$reproducibility_eligible
    )
    expect_false(ambient_uninitialized$promotion_eligible)
    expect_identical(
      validate(fit, ambient_uninitialized),
      ambient_uninitialized
    )
  })
})

test_that("plain static coefficients stay explicitly unbound", {
  fit <- static_output_fit(9211L)
  native <- rqr_posterior_draws(fit)
  foreign <- list(
    beta_root1 = native$beta_root1,
    beta_root2 = native$beta_root2,
    lambda = native$lambda,
    draw_index = native$draw_index,
    nd = native$nd
  )
  normalized <- rqrgibbs:::.rqr_validate_static_draws(
    fit, foreign
  )
  expect_identical(
    class(normalized), c("rqr_static_draws", "list")
  )
  expect_identical(
    normalized$source$source_type, "explicit_unbound"
  )
  expect_false(normalized$source_bound)
  expect_false(normalized$reproducibility_eligible)
  expect_false(normalized$promotion_eligible)
  expect_identical(
    normalized$source$fit_checkpoint_digest, NA_character_
  )
  expect_identical(
    normalized$source$retained_draws_digest, NA_character_
  )

  X_new <- static_output_fixture()$X[1:3, , drop = FALSE]
  prediction <- predict_interval(
    fit, X_new = X_new, draws = foreign
  )
  expect_identical(
    prediction$draws$source$source_type, "explicit_unbound"
  )
  expect_false(prediction$source_bound)
  expect_false(prediction$reproducibility_eligible)
  expect_false(prediction$promotion_eligible)
  expect_identical(
    prediction$promotion_status,
    "explicit_coefficients_unbound_nonpromotable"
  )
  expect_match(prediction$interpretation, "explicit unbound")
  expect_false(prediction$response_predictive_draws)
})

test_that("static draw envelopes reject coherent-rehash source attacks", {
  fit <- static_output_fit(9221L)
  validate <- rqrgibbs:::.rqr_validate_static_draws
  draws <- rqr_posterior_draws(fit, nd = 2L, seed = 9222L)

  bad_source <- draws
  bad_source$source$fit_checkpoint_digest <- paste0(
    rep("0", 64L), collapse = ""
  )
  bad_source <- rehash_static_draws(bad_source)
  expect_error(
    validate(fit, bad_source), "declared source"
  )

  bad_value <- draws
  bad_value$beta_root1[1L, 1L] <-
    bad_value$beta_root1[1L, 1L] + 0.25
  bad_value$payload_digest <- rqrgibbs:::.rqr_digest(
    bad_value[c(
      "beta_root1", "beta_root2", "lambda",
      "draw_index", "nd"
    )]
  )
  bad_value <- rehash_static_draws(bad_value)
  expect_error(
    validate(fit, bad_value), "do not identify"
  )

  bad_selection <- draws
  reversed <- rev(draws$draw_index)
  bad_selection$draw_index <- reversed
  bad_selection$beta_root1 <- fit$samp.beta_root1[
    reversed, , drop = FALSE
  ]
  bad_selection$beta_root2 <- fit$samp.beta_root2[
    reversed, , drop = FALSE
  ]
  bad_selection$lambda <- as.numeric(fit$samp.lambda[reversed])
  bad_selection$payload_digest <- rqrgibbs:::.rqr_digest(
    bad_selection[c(
      "beta_root1", "beta_root2", "lambda",
      "draw_index", "nd"
    )]
  )
  bad_selection <- rehash_static_draws(bad_selection)
  expect_error(
    validate(fit, bad_selection), "RNG semantics"
  )

  bad_seed <- draws
  bad_seed$selection$seed <- 9223L
  bad_seed <- rehash_static_draws(bad_seed)
  expect_error(
    validate(fit, bad_seed), "RNG semantics"
  )

  bad_class <- draws
  class(bad_class) <- c(
    "rqr_static_draws", "unexpected", "list"
  )
  expect_error(validate(fit, bad_class), "exact canonical class")

  bad_attribute <- draws
  attr(bad_attribute, "fabricated") <- TRUE
  expect_error(
    validate(fit, bad_attribute), "exact canonical class"
  )
})

test_that("static predictions are typed, design-bound, and content-digested", {
  fit <- static_output_fit(9231L)
  X_new <- static_output_fixture()$X[
    c(1L, 4L, 8L), , drop = FALSE
  ]
  prediction <- predict_interval(
    fit, X_new = X_new, nd = 3L, seed = 9232L
  )
  validate <- rqrgibbs:::.rqr_validate_static_prediction

  expect_identical(
    class(prediction), c("rqr_static_prediction", "list")
  )
  expect_identical(
    names(attributes(prediction)), c("names", "class")
  )
  expect_identical(
    prediction$schema_version,
    "rqrgibbs_interval_prediction/2.0.0"
  )
  expect_identical(prediction$X_new, X_new)
  expect_identical(
    prediction$source$new_design_digest,
    prediction$new_design_digest
  )
  expect_identical(
    prediction$source$draw_semantic_digest,
    prediction$draws$semantic_digest
  )
  expect_true(prediction$source_bound)
  expect_false(prediction$response_predictive_draws)
  expect_true(all(prediction$width_draws >= 0))
  expect_silent(validate(fit, prediction))

  bad_content <- prediction
  bad_content$lower_draws[1L, 1L] <-
    bad_content$lower_draws[1L, 1L] - 0.5
  bad_content$content_digest <- rqrgibbs:::.rqr_digest(list(
    X_new = bad_content$X_new,
    lower_draws = bad_content$lower_draws,
    upper_draws = bad_content$upper_draws,
    midpoint_draws = bad_content$midpoint_draws,
    width_draws = bad_content$width_draws,
    lower_mean = bad_content$lower_mean,
    upper_mean = bad_content$upper_mean,
    midpoint_mean = bad_content$midpoint_mean,
    width_mean = bad_content$width_mean
  ))
  bad_content <- rehash_static_prediction(bad_content)
  expect_error(validate(fit, bad_content), "roots, input design")

  bad_design <- prediction
  bad_design$X_new[1L, 2L] <- bad_design$X_new[1L, 2L] + 1
  bad_design <- rehash_static_prediction(bad_design)
  expect_error(validate(fit, bad_design), "roots, input design")

  bad_response_flag <- prediction
  bad_response_flag$response_predictive_draws <- TRUE
  bad_response_flag <- rehash_static_prediction(
    bad_response_flag
  )
  expect_error(
    validate(fit, bad_response_flag), "no-response semantics"
  )
})
