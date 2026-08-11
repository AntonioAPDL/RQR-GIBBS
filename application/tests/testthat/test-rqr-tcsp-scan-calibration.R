test_that("TCSP scan probability is monotone in retained count for fixed draws", {
  set.seed(9101)
  draws <- replicate(200, sort(stats::runif(12)), simplify = FALSE)
  max_counts <- vapply(
    draws,
    rqrgibbs:::.rqr_tcsp_scan_max_count,
    integer(1L),
    content = 0.35
  )
  probabilities <- vapply(1:13, function(k) mean(max_counts < k), numeric(1L))

  expect_true(all(diff(probabilities) >= 0))
  expect_equal(probabilities[[1L]], 0)
  expect_equal(probabilities[[13L]], 1)
})

test_that("TCSP DKW calibration returns a conservative retained count", {
  cal <- rqr_tcsp_calibrate_count(
    n = 500,
    guaranteed_content = 0.50,
    tolerance_confidence = 0.80,
    method = "dkw_conservative"
  )

  expect_identical(cal$scan_critical_method, "dkw_conservative")
  expect_gt(cal$retained_count, ceiling(0.50 * 500))
  expect_lte(cal$retained_count, 500)
  expect_equal(cal$target_content, cal$retained_count / 500)
  expect_equal(cal$content_buffer, cal$target_content - 0.50)
  eps <- sqrt(log(2 / (1 - cal$tolerance_confidence)) / (2 * cal$n))
  expect_gt(cal$retained_count / cal$n - 2 * eps,
            cal$guaranteed_content)
  expect_lte((cal$retained_count - 1L) / cal$n - 2 * eps,
             cal$guaranteed_content)
  expect_false(cal$finite_sample_claim_available)
})

test_that("TCSP Monte Carlo calibration is labeled as numerical, not exact", {
  prob <- rqr_tcsp_scan_probability(
    n = 8,
    guaranteed_content = 0.30,
    retained_count = 6,
    method = "monte_carlo_conservative",
    n_sim = 80,
    numerical_confidence = 0.90,
    seed = 9102
  )

  expect_identical(prob$method, "monte_carlo_conservative")
  expect_match(prob$numerical_error_control, "Clopper-Pearson")
  expect_true(is.finite(prob$probability_estimate))
  expect_true(is.finite(prob$certified_lower_probability))
  expect_lte(prob$certified_lower_probability, prob$probability_estimate)
})
