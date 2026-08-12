test_that("weighted shortest interval matches brute-force search", {
  x <- c(0, 1, 1, 3, 4)
  w <- c(0.1, 0.2, 0.2, 0.4, 0.1)

  fast <- rqr_weighted_shortest_interval(x, w, target_content = 0.55)
  brute <- rqrgibbs:::.rqr_weighted_shortest_bruteforce(
    x, w, target_content = 0.55
  )

  expect_equal(fast$lower_index, brute$best$left)
  expect_equal(fast$upper_index, brute$best$right)
  expect_equal(fast$width, brute$best$width)
  expect_equal(fast$retained_mass, brute$best$retained)
  expect_identical(fast$tie_rule, "deterministic_first_minimum_by_lower_index")
})

test_that("direct DP content probability uses exact Beta fixed-interval law", {
  y <- c(-2, -1, 0, 1, 2)
  base <- rqr_dp_base_normal(mean = 0, sd = 2)
  fit <- rqr_dp_fit(y, concentration = 2, base_measure = base)

  prob <- rqr_dp_content_probability(fit, lower = -1, upper = 1, content = 0.5)
  h_mass <- stats::pnorm(1, 0, 2) - stats::pnorm(-1, 0, 2)
  expect_equal(prob$observed_count, 3L)
  expect_equal(prob$beta_shape1, 2 * h_mass + 3)
  expect_equal(prob$beta_shape2, 2 * (1 - h_mass) + 2)
  expect_equal(
    prob$posterior_content_probability,
    stats::pbeta(0.5, prob$beta_shape1, prob$beta_shape2, lower.tail = FALSE)
  )
  expect_true(fit$response_likelihood)
  expect_false(fit$generalized_bayes)
})

test_that("strict direct DP rejects empirical-Bayes base measures", {
  y <- rnorm(10)

  expect_warning(base <- rqr_dp_base_empirical_normal(y), "data dependent")
  expect_error(
    rqr_dp_fit(y, concentration = 1, base_measure = base),
    "data-dependent"
  )
  expect_s3_class(
    rqr_dp_fit(y, concentration = 1, base_measure = base,
               strict_bayes = FALSE),
    "rqr_dp_fit"
  )
})

test_that("direct DP draws and shortest functionals are reproducible", {
  y <- seq(-2, 2, length.out = 20)
  fit <- rqr_dp_fit(y, 1, rqr_dp_base_normal(0, 2))

  draws_a <- rqr_dp_draws(
    fit, n_draws = 3, residual_mass_tol = 0.05, seed = 42
  )
  draws_b <- rqr_dp_draws(
    fit, n_draws = 3, residual_mass_tol = 0.05, seed = 42
  )
  expect_equal(draws_a$draws_digest, draws_b$draws_digest)

  shortest <- rqr_dp_shortest_draws(draws_a, target_content = 0.5)
  expect_equal(nrow(shortest), 3L)
  expect_true(all(shortest$retained_mass >= 0.5 - 1e-12))
  expect_true(all(is.finite(shortest$width)))
})

test_that("direct DP Bayesian action and hybrid scan action expose constraints", {
  y <- seq(-2, 2, length.out = 50)
  base <- rqr_dp_base_normal(0, 2)
  fit <- rqr_dp_fit(y, concentration = 1, base_measure = base)

  action <- rqr_dp_bayes_tolerance_action(
    fit, content = 0.35, posterior_confidence = 0.5
  )
  expect_s3_class(action, "rqr_dp_bayes_tolerance_action")
  expect_gt(action$candidates_evaluated, 0)
  expect_true(action$posterior_constraint_status %in%
                c("satisfied", "infeasible_within_candidate_class"))

  hybrid <- rqr_tcsp_hybrid_bayes_fit(
    y,
    guaranteed_content = 0.35,
    tolerance_confidence = 0.55,
    posterior_confidence = 0.5,
    distribution_engine = "direct_dp",
    scan_method = "dkw_conservative",
    distribution_args = list(concentration = 1, base_measure = base),
    action_control = list(
      n_shortest_draws = 2,
      dp_draw_args = list(residual_mass_tol = 0.05, seed = 11)
    )
  )
  expect_s3_class(hybrid, "rqr_hybrid_bayes_tcsp")
  expect_identical(hybrid$formal_action_class,
                   "closed_order_statistic_intervals")
  expect_true(hybrid$response_likelihood)
  expect_false(hybrid$generalized_bayes)
  expect_true(hybrid$scan_contract$scan_count_fixed_not_resampled)
  expect_equal(nrow(hybrid$posterior_shortest_target_draws), 2L)
})
