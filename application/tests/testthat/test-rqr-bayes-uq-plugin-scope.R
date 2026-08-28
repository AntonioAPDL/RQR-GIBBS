test_that("TCSP-MTI plug-in path declares fixed-target uncertainty-summary setting", {
  y <- seq(-2, 2, length.out = 50)
  fit <- rqr_tcsp_plugin_fit_univariate(
    y,
    guaranteed_content = 0.35,
    tolerance_confidence = 0.55,
    scan_method = "dkw_conservative"
  )

  expect_s3_class(fit, "rqr_tcsp_fit")
  expect_identical(fit$contract$uq_scope, "fixed_target_plugin")
  expect_identical(
    fit$contract$lifecycle_status,
    "superseded_for_unconditional_shortest_interval_uq"
  )
  expect_identical(
    fit$contract$authoritative_full_distribution_uq_function,
    "tcsp_hybrid_bayes_fit"
  )
  expect_false(fit$contract$response_likelihood)
  expect_true(fit$contract$generalized_bayes)
})

test_that("TCSP plug-in warning is optional", {
  y <- seq(-2, 2, length.out = 80)
  old <- getOption("rqrgibbs.warn_tcsp_plugin")
  on.exit(options(rqrgibbs.warn_tcsp_plugin = old), add = TRUE)
  options(rqrgibbs.warn_tcsp_plugin = TRUE)

  expect_warning(
    rqr_tcsp_fit_univariate(
      y,
      guaranteed_content = 0.35,
      tolerance_confidence = 0.55,
      scan_method = "dkw_conservative",
      fit_ecm = TRUE,
      ecm_args = list(ecm_control = list(max_iter = 2))
    ),
    "fixed-target plug-in uncertainty summaries"
  )
})
