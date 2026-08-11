test_that("TCSP univariate fit stores the formal action separately from posterior summaries", {
  y <- seq(-3, 3, length.out = 500)^3 / 9 + seq(-3, 3, length.out = 500)
  fit <- rqr_tcsp_fit_univariate(
    y,
    guaranteed_content = 0.50,
    tolerance_confidence = 0.80,
    scan_method = "dkw_conservative",
    fit_mcmc = FALSE
  )
  contract <- fit$contract
  validation <- rqr_tcsp_validate_action(fit)

  expect_s3_class(fit, "rqr_tcsp_fit")
  expect_true(validation$valid)
  expect_identical(contract$posterior_summary_action,
                   "not_formal_tolerance_action")
  expect_identical(contract$formal_tolerance_action,
                   "[Y_(j), Y_(j+k-1)] with first global minimum-width tie rule")
  expect_equal(contract$target_content, contract$retained_count / length(y))
  expect_equal(contract$delta_raw,
               contract$retained_mean - mean(y))
})

test_that("TCSP action validation rejects posterior-summary promotion", {
  y <- seq(-2, 2, length.out = 200)
  fit <- rqr_tcsp_fit_univariate(
    y, 0.40, 0.80, scan_method = "dkw_conservative"
  )
  bad <- fit$contract
  bad$posterior_summary_action <- "formal_tolerance_action"

  expect_false(rqr_tcsp_validate_action(bad)$valid)
  expect_match(
    paste(rqr_tcsp_validate_action(bad)$problems, collapse = " "),
    "posterior summary"
  )
})
