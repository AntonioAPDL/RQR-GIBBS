test_that("TCSP fails closed when conservative calibration is infeasible", {
  expect_error(
    rqr_tcsp_calibrate_count(
      n = 20,
      guaranteed_content = 0.95,
      tolerance_confidence = 0.99,
      method = "dkw_conservative"
    ),
    "infeasible"
  )
})

test_that("TCSP rejects impossible retained windows and degenerate samples", {
  expect_error(rqr_tcsp_shortest_window(1:5, retained_count = 6L),
               "cannot exceed")
  expect_error(rqr_tcsp_fit_univariate(rep(1, 10), 0.5, 0.8,
                                       scan_method = "dkw_conservative"),
               "standard deviation")
})
