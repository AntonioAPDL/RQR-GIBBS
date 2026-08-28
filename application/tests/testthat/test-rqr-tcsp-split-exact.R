test_that("exact-spacing gap uses Beta d, N+1-d indexing", {
  gap <- rqr_tcsp_exact_spacing_gap(
    main_n = 40,
    guaranteed_content = 0.50,
    tolerance_confidence = 0.80
  )
  d <- gap$spacing_gap

  expect_equal(gap$closed_position_count, d + 1L)
  expect_equal(gap$beta_shape1, d)
  expect_equal(gap$beta_shape2, 41L - d)
  expect_gte(gap$exact_beta_survival_probability, 0.80)
  if (d > 1L) {
    previous <- stats::pbeta(
      0.50, shape1 = d - 1L, shape2 = 41L - (d - 1L),
      lower.tail = FALSE
    )
    expect_lt(previous, 0.80)
  }
})

test_that("split exact TCSP keeps pilot and main indices disjoint and reproducible", {
  y <- seq(-2, 2, length.out = 80)^3 / 6 + seq(-2, 2, length.out = 80)
  fit1 <- rqr_tcsp_split_exact_fit(
    y,
    guaranteed_content = 0.45,
    tolerance_confidence = 0.75,
    pilot_fraction = 0.30,
    pilot_method = "empirical_shortest",
    split_seed = 8123
  )
  fit2 <- rqr_tcsp_split_exact_fit(
    y,
    guaranteed_content = 0.45,
    tolerance_confidence = 0.75,
    pilot_fraction = 0.30,
    pilot_method = "empirical_shortest",
    split_seed = 8123
  )

  expect_length(intersect(fit1$pilot_indices, fit1$main_indices), 0)
  expect_setequal(c(fit1$pilot_indices, fit1$main_indices), seq_along(y))
  expect_identical(fit1$pilot_indices, fit2$pilot_indices)
  expect_equal(fit1$contract$lower_endpoint, fit2$contract$lower_endpoint)
  expect_equal(fit1$contract$upper_endpoint, fit2$contract$upper_endpoint)
  expect_true(fit1$contract$main_sample_not_used_for_pilot_placement)
  expect_identical(fit1$contract$formal_action_source,
                   "independent_target_selection_fixed_spacing")
})

test_that("split exact ECM pilot reserves target arguments and can fit fixed tilt", {
  y <- sort(rnorm(70))
  expect_error(
    rqr_tcsp_split_exact_fit(
      y, 0.40, 0.70, 0.35,
      pilot_method = "ecm_fixed_tilt",
      split_seed = 8124,
      ecm_args = list(coverage_level = 0.9)
    ),
    "reserves"
  )

  fit <- rqr_tcsp_split_exact_fit(
    y, 0.40, 0.70, 0.35,
    pilot_method = "ecm_fixed_tilt",
    split_seed = 8124,
    ecm_args = list(ecm_control = list(
      max_iter = 15,
      stable_iterations = 1,
      tol_stationarity = 1e6
    ))
  )

  expect_s3_class(fit$ecm_fit_optional, "rqr_ecm")
  expect_true(is.finite(fit$contract$pilot_lower_tail_coordinate))
  expect_gte(fit$contract$pilot_lower_tail_coordinate, 0)
  expect_lte(
    fit$contract$pilot_lower_tail_coordinate,
    1 - fit$contract$effective_pilot_content
  )
})
