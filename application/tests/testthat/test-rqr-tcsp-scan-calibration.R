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

test_that("TCSP scan distribution and CDF band are deterministic and simultaneous", {
  dist1 <- rqr_tcsp_scan_distribution(
    n = 12,
    guaranteed_content = 0.30,
    n_sim = 120,
    seed = 9102
  )
  dist2 <- rqr_tcsp_scan_distribution(
    n = 12,
    guaranteed_content = 0.30,
    n_sim = 120,
    seed = 9102
  )
  band <- rqr_tcsp_scan_cdf_band(dist1, numerical_confidence = 0.90)

  expect_s3_class(dist1, "rqr_tcsp_scan_distribution")
  expect_s3_class(band, "rqr_tcsp_scan_cdf_band")
  expect_identical(dist1$max_count_histogram, dist2$max_count_histogram)
  expect_equal(sum(dist1$max_count_histogram), 120)
  expect_identical(band$band_method,
                   "massart_dkw_empirical_cdf_lower_band")
  expect_true(all(band$cdf$certified_lower_cdf <= band$cdf$empirical_cdf))
  expect_equal(band$cdf$certified_lower_cdf[[nrow(band$cdf)]], 1)
  expect_true(is.character(band$cdf_band_digest))
})

test_that("TCSP Monte Carlo calibration is simultaneous numerical, not exact", {
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
  expect_match(prob$numerical_error_control, "Simultaneous Massart-DKW")
  expect_true(prob$simultaneous_numerical_calibration)
  expect_identical(prob$scan_cdf_band_method,
                   "massart_dkw_empirical_cdf_lower_band")
  expect_true(is.finite(prob$probability_estimate))
  expect_true(is.finite(prob$certified_lower_probability))
  expect_lte(prob$certified_lower_probability, prob$probability_estimate)
})

test_that("TCSP Monte Carlo calibration uses exact terminal range rescue", {
  prob <- rqr_tcsp_scan_probability(
    n = 500,
    guaranteed_content = 0.99,
    retained_count = 500,
    method = "monte_carlo_conservative",
    n_sim = 80,
    numerical_confidence = 0.995,
    seed = 9104
  )
  expected <- 1 - 0.99^499 * (500 - 499 * 0.99)

  expect_identical(prob$method, "exact_terminal_range")
  expect_true(prob$exact_terminal_range_calibration)
  expect_equal(prob$certified_lower_probability, expected,
               tolerance = 1e-12)
  expect_true(prob$certified_lower_probability >= 0.95)

  cal <- rqr_tcsp_calibrate_count(
    n = 500,
    guaranteed_content = 0.99,
    tolerance_confidence = 0.95,
    method = "monte_carlo_conservative",
    n_sim = 5000,
    numerical_confidence = 0.995,
    seed = 9104
  )

  expect_equal(cal$retained_count, 500L)
  expect_equal(cal$target_content, 1)
  expect_true(cal$exact_terminal_range_calibration)
  expect_true(cal$finite_sample_claim_available)
  expect_identical(cal$scan_probability$method, "exact_terminal_range")
})

test_that("TCSP Monte Carlo calibration uses one simultaneous band for selection", {
  cal <- rqr_tcsp_calibrate_count(
    n = 10,
    guaranteed_content = 0.35,
    tolerance_confidence = 0.75,
    method = "monte_carlo_conservative",
    n_sim = 250,
    numerical_confidence = 0.80,
    seed = 9104
  )

  expect_true(cal$simultaneous_numerical_calibration)
  expect_identical(cal$scan_probability$cdf_band_digest,
                   cal$cdf_band_digest)
  expect_true(cal$scan_probability$certified_lower_probability >= 0.75)
  if (cal$retained_count > 1L) {
    prev <- rqrgibbs:::.rqr_tcsp_probability_from_band(
      cal$scan_cdf_band, cal$retained_count - 1L
    )
    expect_lt(prev$certified_lower_probability, 0.75)
  }
})
