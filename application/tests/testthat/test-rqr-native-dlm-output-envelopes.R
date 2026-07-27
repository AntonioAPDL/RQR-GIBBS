.rqr_dlm_output_test_fit <- function(
    seed = 31801L, evolution = c("fixed", "component")) {
  evolution <- match.arg(evolution)
  common <- list(
    y = c(-0.65, -0.1, NA_real_, 0.45, 0.9),
    model = rqr_polytrend(
      1L, m0 = 0, C0 = matrix(2, 1L, 1L)
    ),
    coverage_level = 0.8,
    learning_rate_mode = "fixed_rate",
    numerical_policy = "fail",
    mcmc_control = list(
      n_burn = 0L, n_mcmc = 3L, thin = 1L,
      seed = seed, backend = "R",
      store_state_draws = TRUE
    )
  )
  if (identical(evolution, "fixed")) {
    do.call(rqr_dlm_fit, c(
      common,
      list(evolution_spec = rqr_evolution_fixed(
        matrix(0.04, 1L, 1L)
      ))
    ))
  } else {
    do.call(rqr_dlm_fit, c(
      common,
      list(
        evolution_mode = "component_scale",
        component_templates =
          list(matrix(1, 1L, 1L)),
        evolution_scale_prior =
          list(shape = 3, rate = 2),
        evolution_scale_initial = 1
      )
    ))
  }
}

.rqr_dlm_output_clone <- function(x) {
  unserialize(serialize(x, NULL, version = 3L))
}

.rqr_dlm_output_rehash <- function(x) {
  payload <- unclass(x)
  payload$semantic_digest <- NULL
  x$semantic_digest <- rqrgibbs:::.rqr_digest(payload)
  x
}

test_that("DLM draw extraction has an exact source and RNG envelope", {
  fit <- .rqr_dlm_output_test_fit()

  set.seed(31802L)
  rng_before <- .Random.seed
  all_draws <- rqr_posterior_draws(fit)
  expect_identical(.Random.seed, rng_before)
  expect_identical(
    class(all_draws), c("rqr_dlm_draws", "list")
  )
  expect_identical(
    names(attributes(all_draws)), c("names", "class")
  )
  expect_identical(
    all_draws$schema_version, "rqrgibbs_dlm_draws/1.0.0"
  )
  expect_true(all_draws$source_bound)
  expect_identical(
    all_draws$source$fit_checkpoint_digest,
    fit$checkpoint_digest
  )
  expect_identical(
    all_draws$source$retained_draws_digest,
    fit$retained_draws_digest
  )
  expect_identical(all_draws$selection$mode, "none")
  expect_identical(all_draws$selection$selection_mode, "all")
  expect_false(all_draws$response_predictive_draws)
  expect_silent(
    rqrgibbs:::.rqr_validate_dlm_draws(fit, all_draws)
  )

  selected_a <- rqr_posterior_draws(
    fit, nd = 5L, seed = 31803L
  )
  selected_b <- rqr_posterior_draws(
    fit, nd = 5L, seed = 31803L
  )
  expect_identical(selected_a, selected_b)
  expect_identical(.Random.seed, rng_before)
  expect_identical(
    selected_a$selection$mode, "explicit_seed"
  )
  expect_identical(
    selected_a$selection$requested_draw_count, 5L
  )
  expect_true(
    selected_a$selection$sampling_with_replacement
  )

  ambient <- rqr_posterior_draws(fit, nd = 2L)
  expect_identical(ambient$selection$mode, "ambient_rng")
  expect_false(ambient$selection$reproducibility_bound)
  expect_false(ambient$reproducibility_eligible)
  expect_false(ambient$promotion_eligible)
})

test_that("foreign fitted-time DLM matrices never masquerade as native", {
  fit <- .rqr_dlm_output_test_fit(seed = 31804L)
  native <- rqr_posterior_draws(fit)
  explicit <- list(
    eta_root1 = native$eta_root1,
    eta_root2 = native$eta_root2,
    lambda = native$lambda,
    index = native$index,
    nd = native$nd
  )
  prediction <- predict_interval(fit, draws = explicit)

  expect_identical(
    class(prediction), c("rqr_dlm_prediction", "list")
  )
  expect_identical(
    names(attributes(prediction)), c("names", "class")
  )
  expect_false(prediction$source_bound)
  expect_false(prediction$reproducibility_eligible)
  expect_false(prediction$promotion_eligible)
  expect_identical(
    prediction$draws$source$binding_status,
    "external_unbound"
  )
  expect_identical(
    prediction$draws$selection$mode,
    "external_unbound"
  )
  expect_identical(
    prediction$promotion_status,
    "external_draws_unbound_nonpromotable"
  )
  expect_false(prediction$response_predictive_draws)
  expect_silent(
    rqrgibbs:::.rqr_validate_dlm_prediction(
      fit, prediction
    )
  )

  direct <- predict_interval(fit, nd = 2L, seed = 31805L)
  expect_true(direct$source_bound)
  expect_identical(
    direct$fit_checkpoint_digest, fit$checkpoint_digest
  )
  expect_identical(
    direct$retained_draws_digest,
    fit$retained_draws_digest
  )
})

test_that("DLM draw and prediction mutations fail after coherent rehashing", {
  fit <- .rqr_dlm_output_test_fit(seed = 31806L)
  draws <- rqr_posterior_draws(
    fit, nd = 2L, seed = 31807L
  )

  bad <- .rqr_dlm_output_clone(draws)
  bad$source$fit_checkpoint_digest <- paste0(
    strrep("0", 63L), "1"
  )
  bad <- .rqr_dlm_output_rehash(bad)
  expect_error(
    rqrgibbs:::.rqr_validate_dlm_draws(fit, bad),
    "not bound to the exact"
  )

  bad <- .rqr_dlm_output_clone(draws)
  bad$index <- rev(bad$index)
  bad$eta_root1 <- fit$samp.eta_root1[
    , bad$index, drop = FALSE
  ]
  bad$eta_root2 <- fit$samp.eta_root2[
    , bad$index, drop = FALSE
  ]
  bad$lambda <- fit$samp.lambda[bad$index]
  bad <- .rqr_dlm_output_rehash(bad)
  expect_error(
    rqrgibbs:::.rqr_validate_dlm_draws(fit, bad),
    "RNG binding"
  )

  bad <- .rqr_dlm_output_clone(draws)
  bad$promotion_eligible <- !bad$promotion_eligible
  bad <- .rqr_dlm_output_rehash(bad)
  expect_error(
    rqrgibbs:::.rqr_validate_dlm_draws(fit, bad),
    "eligibility"
  )

  prediction <- predict_interval(
    fit, nd = 2L, seed = 31808L
  )
  bad_prediction <- .rqr_dlm_output_clone(prediction)
  bad_prediction$lower_draws[1L, 1L] <-
    bad_prediction$lower_draws[1L, 1L] - 0.25
  bad_prediction <- .rqr_dlm_output_rehash(
    bad_prediction
  )
  expect_error(
    rqrgibbs:::.rqr_validate_dlm_prediction(
      fit, bad_prediction
    ),
    "roots, summaries"
  )
})

test_that("future DLM roots bind future inputs and replay RNG exactly", {
  fit <- .rqr_dlm_output_test_fit(seed = 31809L)
  args <- list(
    object = fit,
    FF_future = matrix(1, 1L, 2L),
    GG_future = matrix(1, 1L, 1L),
    W_future = matrix(0.04, 1L, 1L),
    nd = 4L,
    seed = 31810L
  )
  set.seed(31811L)
  rng_before <- .Random.seed
  forecast_a <- do.call(rqr_forecast_roots, args)
  forecast_b <- do.call(rqr_forecast_roots, args)
  expect_identical(forecast_a, forecast_b)
  expect_identical(.Random.seed, rng_before)
  expect_identical(
    class(forecast_a), c("rqr_dlm_forecast", "list")
  )
  expect_identical(
    names(attributes(forecast_a)), c("names", "class")
  )
  expect_identical(
    forecast_a$schema_version,
    "rqrgibbs_dlm_forecast/1.0.0"
  )
  expect_true(forecast_a$source_bound)
  expect_identical(
    forecast_a$source$fit_checkpoint_digest,
    fit$checkpoint_digest
  )
  expect_identical(
    forecast_a$future_contract$future_evolution_mode,
    "fixed_W"
  )
  expect_identical(
    forecast_a$rng_binding$mode, "explicit_seed"
  )
  expect_false(forecast_a$response_predictive_draws)
  expect_silent(
    rqrgibbs:::.rqr_validate_dlm_forecast(
      fit, forecast_a
    )
  )

  bad <- .rqr_dlm_output_clone(forecast_a)
  bad$eta_root1[1L, 1L] <- bad$eta_root1[1L, 1L] + 0.1
  bad$lower_draws <- pmin(bad$eta_root1, bad$eta_root2)
  bad$upper_draws <- pmax(bad$eta_root1, bad$eta_root2)
  bad$midpoint_draws <- 0.5 * (
    bad$lower_draws + bad$upper_draws
  )
  bad$width_draws <- bad$upper_draws - bad$lower_draws
  bad$lower_mean <- rowMeans(bad$lower_draws)
  bad$upper_mean <- rowMeans(bad$upper_draws)
  bad$midpoint_mean <- rowMeans(bad$midpoint_draws)
  bad$width_mean <- rowMeans(bad$width_draws)
  bad <- .rqr_dlm_output_rehash(bad)
  expect_error(
    rqrgibbs:::.rqr_validate_dlm_forecast(fit, bad),
    "not the recorded RNG transition"
  )

  bad <- .rqr_dlm_output_clone(forecast_a)
  bad$rng_binding$seed <- 31812L
  bad <- .rqr_dlm_output_rehash(bad)
  expect_error(
    rqrgibbs:::.rqr_validate_dlm_forecast(fit, bad),
    "RNG start state"
  )
})

test_that("ambient and external-state forecasts are explicitly ineligible", {
  fit <- .rqr_dlm_output_test_fit(seed = 31813L)
  ambient <- rqr_forecast_roots(
    fit, FF_future = matrix(1, 1L, 1L),
    GG_future = 1, W_future = 0.04
  )
  expect_identical(ambient$rng_binding$mode, "ambient_rng")
  expect_false(ambient$reproducibility_eligible)
  expect_false(ambient$promotion_eligible)
  expect_identical(
    ambient$promotion_status, "ambient_rng_nonpromotable"
  )

  state_only <- structure(
    list(
      samp.theta_terminal_root1 =
        matrix(c(-0.4, -0.2), 1L, 2L),
      samp.theta_terminal_root2 =
        matrix(c(0.5, 0.7), 1L, 2L),
      expanded_model = list(p = 1L),
      model_spec = list(
        evolution_mode = "fixed_W",
        numerical_policy = "fail"
      ),
      misc = list(jitter_ladder = 0)
    ),
    class = c("rqr_dlm_mcmc", "rqr_fit")
  )
  forecast <- rqr_forecast_roots(
    state_only, FF_future = matrix(1, 1L, 1L),
    GG_future = 1, W_future = 0.01,
    seed = 31814L
  )
  expect_false(forecast$source_bound)
  expect_false(forecast$reproducibility_eligible)
  expect_false(forecast$promotion_eligible)
  expect_identical(
    forecast$source$binding_status,
    "unbound_external_state_fixture"
  )
  expect_identical(
    forecast$promotion_status,
    "external_state_fixture_unbound_nonpromotable"
  )
})

test_that("component-scale forecasts bind saved scale rows", {
  fit <- .rqr_dlm_output_test_fit(
    seed = 31815L, evolution = "component"
  )
  forecast <- rqr_forecast_roots(
    fit, FF_future = matrix(1, 1L, 2L),
    GG_future = 1,
    component_templates_future =
      list(matrix(1, 1L, 1L)),
    nd = 4L, seed = 31816L
  )
  expect_identical(
    forecast$future_contract$future_evolution_mode,
    "component_scale"
  )
  expect_identical(
    forecast$diagnostics$component_scale_draws,
    fit$samp.evolution_scale[
      forecast$draw_index, , drop = FALSE
    ]
  )
  expect_silent(
    rqrgibbs:::.rqr_validate_dlm_forecast(fit, forecast)
  )
})
