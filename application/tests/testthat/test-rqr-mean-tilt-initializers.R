test_that("Cornish-Fisher mean-tilt pilots satisfy algebraic invariances", {
  expect_equal(rqr_mt_cf_constant(0.90), 0.1884922579, tolerance = 5e-10)

  symmetric_y <- c(-4, -3, -2, -1, 0, 1, 2, 3, 4)
  cf_sym_sh <- rqr_mt_tilt_cf(symmetric_y, 0.90, "shortest")
  cf_sym_et <- rqr_mt_tilt_cf(symmetric_y, 0.90, "equal_tailed")
  expect_s3_class(cf_sym_sh, "rqr_mt_tilt_pilot")
  expect_equal(cf_sym_sh$adjusted_skewness, 0, tolerance = 1e-14)
  expect_equal(cf_sym_sh$delta_standardized, 0, tolerance = 1e-14)
  expect_equal(cf_sym_et$delta_standardized, 0, tolerance = 1e-14)
  expect_false(cf_sym_sh$sampled_tilt)
  expect_true(cf_sym_sh$fixed_tilt)

  y <- c(-2.1, -1.3, -0.4, 0.1, 0.4, 0.9, 1.2, 2.8, 5.3)
  cf_y <- rqr_mt_tilt_cf(y, 0.90, "shortest")
  cf_neg <- rqr_mt_tilt_cf(-y, 0.90, "shortest")
  expect_equal(
    cf_neg$adjusted_skewness, -cf_y$adjusted_skewness,
    tolerance = 1e-12
  )
  expect_equal(
    cf_neg$delta_standardized, -cf_y$delta_standardized,
    tolerance = 1e-12
  )
  expect_equal(
    cf_neg$excess_kurtosis, cf_y$excess_kurtosis,
    tolerance = 1e-12
  )
  expect_equal(
    cf_neg$standardized_sixth_moment,
    cf_y$standardized_sixth_moment,
    tolerance = 1e-12
  )
  expect_equal(cf_neg$delta_raw, -cf_y$delta_raw, tolerance = 1e-12)
  expect_true(all(c("u_lower", "u_upper", "clipped") %in%
                    names(cf_y$probability_window)))
  expect_true(cf_y$u_lower_cf >= 0)
  expect_true(cf_y$u_upper_cf <= 1)
  expect_true(cf_y$reliability_status %in% c("nominal", "diagnostic_caution"))

  shifted_scaled <- rqr_mt_tilt_cf(7.5 + 3.2 * y, 0.90, "shortest")
  expect_equal(
    shifted_scaled$delta_standardized, cf_y$delta_standardized,
    tolerance = 1e-12
  )
  expect_equal(
    shifted_scaled$delta_raw, 3.2 * cf_y$delta_raw,
    tolerance = 1e-11
  )

  cf_y_et <- rqr_mt_tilt_cf(y, 0.90, "equal_tailed")
  expect_equal(
    cf_y_et$delta_standardized, cf_y$delta_standardized / 3,
    tolerance = 1e-15
  )
  expect_equal(cf_y_et$delta_raw, cf_y$delta_raw / 3, tolerance = 1e-15)
})

test_that("empirical mean-tilt pilots account for windows and boundaries", {
  emp_y <- c(0, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 1.8, 2.0, 5.0)
  emp_sh <- rqr_mt_tilt_empirical_shortest(emp_y, 0.80)
  emp_et <- rqr_mt_tilt_empirical_equal_tailed(emp_y, 0.80)
  expect_s3_class(emp_sh, "rqr_mt_tilt_pilot")
  expect_identical(emp_sh$m, 8L)
  expect_equal(emp_sh$realized_content, 0.8, tolerance = 1e-15)
  expect_identical(emp_sh$boundary, "lower")
  expect_equal(
    emp_sh$feasible_delta_standardized_range,
    sort(emp_sh$feasible_delta_standardized_range)
  )
  expect_identical(emp_et$lower_omitted + emp_et$upper_omitted, 2L)
  expect_equal(emp_et$realized_content, 0.8, tolerance = 1e-15)
})

test_that("mean-tilt pilots fail loudly for invalid inputs", {
  expect_error(rqr_mt_tilt_cf(c(1, 1, 1), 0.90), "standard deviation")
  expect_error(rqr_mt_tilt_cf(c(1, 2), 0.90), "At least three")
  expect_error(rqr_mt_tilt_cf(c(1, 2, NA), 0.90), "Nonfinite")
  expect_error(rqr_mt_tilt_cf(c(1, 2, 3), 1.00), "coverage_level")

  with_na <- rqr_mt_tilt_cf(c(1, 2, 3, NA), 0.80, na_rm = TRUE)
  expect_identical(with_na$n_removed, 1L)
  expect_true(is.finite(with_na$delta_standardized))
})

test_that("mean-tilt screen grid and validation selection are fail-closed", {
  y <- c(-2.1, -1.3, -0.4, 0.1, 0.4, 0.9, 1.2, 2.8, 5.3)
  screen <- rqr_mt_tilt_screen(y = y, coverage_level = 0.90)
  expect_s3_class(screen, "rqr_mt_tilt_screen")
  expect_true(0 %in% screen$candidates)
  expect_true(any(screen$anchors$anchor == "cornish_fisher_shortest"))
  expect_true(any(screen$anchors$anchor == "empirical_shortest_window"))
  expect_false(screen$sampled_tilt)
  expect_true(all(
    screen$candidates >= screen$empirical_feasible_range[[1L]] - 1e-14 &
      screen$candidates <= screen$empirical_feasible_range[[2L]] + 1e-14
  ))

  failed <- rqr_mt_select_tilt_candidate(
    candidates = c(-0.2, 0, 0.2),
    mean_width = c(1.1, 1.0, 0.8),
    empirical_coverage = c(0.80, 0.82, 0.83),
    coverage_level = 0.90,
    tolerance = 0.02
  )
  expect_identical(failed$status, "failed_no_coverage_candidate")
  expect_true(is.na(failed$selected_delta_standardized))

  selected <- rqr_mt_select_tilt_candidate(
    candidates = c(-0.2, 0, 0.2),
    mean_width = c(1.1, 1.0, 0.8),
    empirical_coverage = c(0.90, 0.91, 0.87),
    coverage_level = 0.90,
    tolerance = 0
  )
  expect_identical(selected$status, "selected")
  expect_equal(selected$selected_delta_standardized, 0)

  guarded <- rqr_mt_select_tilt_candidate(
    candidates = c(-0.2, 0, 0.2),
    mean_width = c(1.1, 1.0, 0.8),
    empirical_coverage = c(0.94, 0.93, 0.89),
    coverage_level = 0.90,
    tolerance = 0.02,
    coverage_guard = "simultaneous_binomial",
    validation_n = 500L,
    confidence_level = 0.90
  )
  expect_identical(guarded$coverage_guard, "simultaneous_binomial")
  expect_true(all(guarded$coverage_lower_bound <= guarded$empirical_coverage))
  expect_true(guarded$status %in% c("selected", "failed_no_coverage_candidate"))
  expect_error(
    rqr_mt_select_tilt_candidate(
      candidates = c(0, 0.1),
      mean_width = c(1, 1),
      empirical_coverage = c(0.91, 0.92),
      coverage_level = 0.9,
      coverage_guard = "simultaneous_binomial"
    ),
    "validation_n is required"
  )
})

test_that("bootstrap diagnostic is reproducible and does not redefine target", {
  y <- c(-2.1, -1.3, -0.4, 0.1, 0.4, 0.9, 1.2, 2.8, 5.3)
  a <- rqr_mt_tilt_cf(y, 0.90, "shortest", bootstrap = 20L, seed = 912)
  b <- rqr_mt_tilt_cf(y, 0.90, "shortest", bootstrap = 20L, seed = 912)
  expect_identical(a$bootstrap$draws_standardized,
                   b$bootstrap$draws_standardized)
  expect_identical(a$bootstrap$requested, 20L)
  expect_false(a$sampled_tilt)
})
