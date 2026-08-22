test_that("MTI boundary target preserves interior scan targets", {
  target <- rqr_tcsp_mti_boundary_target(
    n = 100, retained_count = 98, guaranteed_content = 0.90
  )

  expect_s3_class(target, "rqr_tcsp_mti_boundary_target")
  expect_false(target$boundary_continuation)
  expect_equal(target$scan_target_content, 0.98)
  expect_equal(target$ecm_target_content, 0.98)
  expect_true(is.na(target$epsilon))
})

test_that("MTI boundary target gives predeclared interior continuations", {
  cases <- data.frame(
    n = c(50L, 100L, 500L),
    retained_count = c(50L, 100L, 500L),
    guaranteed_content = c(0.90, 0.95, 0.99),
    expected_q = c(0.990, 0.995, 0.999)
  )
  for (ii in seq_len(nrow(cases))) {
    target <- rqr_tcsp_mti_boundary_target(
      n = cases$n[[ii]],
      retained_count = cases$retained_count[[ii]],
      guaranteed_content = cases$guaranteed_content[[ii]]
    )
    expect_true(target$boundary_continuation)
    expect_equal(target$scan_target_content, 1)
    expect_equal(target$ecm_target_content, cases$expected_q[[ii]])
    expect_gt(target$ecm_target_content, cases$guaranteed_content[[ii]])
    expect_lt(target$ecm_target_content, 1)
  }
})

test_that("fractional tilt interpolates between adjacent shortest windows", {
  y <- c(-4, -2, -1, 0, 1, 2, 8, 9)
  q <- 1 - 1 / (2 * length(y))
  tilt <- rqr_tcsp_fractional_tilt(y, q)
  lower_window <- rqr_tcsp_shortest_window(y, length(y) - 1L)
  upper_window <- rqr_tcsp_shortest_window(y, length(y))

  expect_s3_class(tilt, "rqr_tcsp_fractional_tilt")
  expect_equal(tilt$lower_count, length(y) - 1L)
  expect_equal(tilt$upper_count, length(y))
  expect_equal(tilt$interpolation_weight_lower, 0.5)
  expect_equal(tilt$interpolation_weight_upper, 0.5)
  expect_equal(tilt$delta_raw,
               0.5 * lower_window$delta_raw + 0.5 * upper_window$delta_raw)
  expect_equal(upper_window$delta_raw, 0)
})

test_that("integer fractional tilt reproduces shortest-window tilt", {
  y <- sort(c(stats::rnorm(30), 5))
  window <- rqr_tcsp_shortest_window(y, retained_count = 20L)
  tilt <- rqr_tcsp_fractional_tilt(y, target_content = 20 / length(y))

  expect_equal(tilt$rule, "integer_shortest_window")
  expect_equal(tilt$lower_count, 20L)
  expect_equal(tilt$upper_count, 20L)
  expect_equal(tilt$delta_raw, window$delta_raw)
})
