test_that("TCSP shortest window matches brute-force closed-window scanning", {
  y <- c(3.2, -1.1, 0.4, 0.7, 0.9, 1.5, 2.8)
  k <- 4L
  got <- rqr_tcsp_shortest_window(y, retained_count = k)
  ys <- sort(y)
  starts <- seq_len(length(y) - k + 1L)
  ends <- starts + k - 1L
  widths <- ys[ends] - ys[starts]
  expected_start <- which.min(widths)

  expect_s3_class(got, "rqr_tcsp_window")
  expect_identical(got$interval_endpoint_convention,
                   "closed_order_statistic_window")
  expect_identical(got$shortest_window_start, as.integer(expected_start))
  expect_identical(got$shortest_window_end,
                   as.integer(expected_start + k - 1L))
  expect_equal(got$width, min(widths))
  expect_true(got$global_shortest_verified)
})

test_that("TCSP tilt is retained mean minus full mean", {
  y <- c(-2, -0.5, 0, 0.2, 0.4, 4)
  got <- rqr_tcsp_shortest_window(y, retained_count = 4L)
  tilt <- rqr_tcsp_tilt_from_window(got)
  retained <- sort(y)[got$shortest_window_start:got$shortest_window_end]

  expect_equal(got$retained_mean, mean(retained))
  expect_equal(tilt$delta_raw, mean(retained) - mean(y))
  expect_equal(tilt$delta_standardized, tilt$delta_raw / stats::sd(y))
})

test_that("TCSP shortest window handles deterministic first ties", {
  y <- c(0, 1, 2, 10, 11, 12)
  got <- rqr_tcsp_shortest_window(y, retained_count = 3L)

  expect_identical(got$tie_count, 2L)
  expect_identical(got$tie_rule, "first")
  expect_identical(got$shortest_window_start, 1L)
  expect_equal(got$lower_endpoint, 0)
  expect_equal(got$upper_endpoint, 2)
})
