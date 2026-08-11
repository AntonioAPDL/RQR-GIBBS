test_that("TCSP path records local continuation diagnostics but uses global actions", {
  y <- sort(c(stats::qnorm(seq(0.01, 0.99, length.out = 300)), 5, 5.5))
  path <- rqr_tcsp_path(
    y,
    guaranteed_contents = c(0.35, 0.45, 0.55),
    tolerance_confidence = 0.80,
    scan_method = "dkw_conservative"
  )

  expect_s3_class(path, "rqr_tcsp_path")
  expect_equal(nrow(path$path), 3)
  expect_true(all(path$path$global_shortest_verified))
  expect_true(all(diff(path$path$retained_count) >= 0))
  expect_true(all(is.na(path$path$local_regret[[1L]]) |
                    path$path$local_regret[[1L]] == 0))
  expect_true(all(path$path$local_regret[-1L] >= -1e-12))
})

test_that("TCSP local correction expands trust region and verifies global regret", {
  y <- c(0, 1, 2, 3, 100, 101, 102, 103)
  global <- rqr_tcsp_shortest_window(y, retained_count = 4L)
  local <- rqr_tcsp_local_correct(
    y,
    retained_count = 4L,
    predicted_start = 5L,
    trust_radius = 0L,
    global_window = global
  )

  expect_true(local$globally_verified)
  expect_equal(local$local_regret, 0)
  expect_true(local$local_start %in% c(1L, 5L))
})
