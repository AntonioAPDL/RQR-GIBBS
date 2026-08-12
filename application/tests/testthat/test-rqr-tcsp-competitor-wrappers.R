tcsp_competitor_environment <- function() {
  env <- new.env(parent = globalenv())
  sys.source(
    testthat::test_path(
      "..", "..", "scripts", "lib", "tcsp_validation_study.R"
    ),
    envir = env
  )
  env
}

tcsp_competitor_config <- function(env) {
  env$tcspv_read_config(testthat::test_path(
    "..", "..", "config", "tcsp_validation_v1.json"
  ))
}

tcsp_enable_methods <- function(config, method_ids) {
  for (ii in seq_along(config$methods)) {
    if (config$methods[[ii]]$method_id %in% method_ids) {
      config$methods[[ii]]$enabled <- TRUE
      config$methods[[ii]]$disabled_reason <- ""
    }
  }
  config
}

test_that("TCSP optional tolerance-package wrappers return normalized intervals", {
  testthat::skip_if_not_installed("tolerance")
  env <- tcsp_competitor_environment()
  config <- tcsp_competitor_config(env)
  criticals <- env$tcspv_critical_counts(config, "tiny")
  y <- seq(-2, 2, length.out = 50)^3 + seq(-2, 2, length.out = 50)
  methods <- c(
    "young_mathew", "wald_order", "hahn_meeker", "normal_exact_tolerance"
  )

  for (method_id in methods) {
    interval <- env$tcspv_interval_for_method(
      config, method_id, y, "normal", 0.80, 0.80, criticals
    )
    expect_false(interval$failed, info = method_id)
    expect_true(is.finite(interval$lower), info = method_id)
    expect_true(is.finite(interval$upper), info = method_id)
    expect_lte(interval$lower, interval$upper)
    expect_identical(interval$source_package, "tolerance")
    expect_true(nzchar(interval$source_version))
    expect_true(nzchar(interval$certificate_status))
    expect_identical(interval$row_selection, "minimum_width_first_tie")
  }
})

test_that("TCSP validation can run a tiny optional-competitor smoke grid", {
  testthat::skip_if_not_installed("tolerance")
  env <- tcsp_competitor_environment()
  config <- tcsp_competitor_config(env)
  optional <- c(
    "young_mathew", "wald_order", "hahn_meeker", "normal_exact_tolerance"
  )
  config <- tcsp_enable_methods(config, optional)
  config$modes$tiny$replications <- 2L
  config$modes$tiny$sample_sizes <- list(50L)
  config$modes$tiny$dgp_ids <- list("normal")
  config$modes$tiny$guaranteed_contents <- list(0.80)
  config$modes$tiny$tolerance_confidences <- list(0.80)
  config$modes$tiny$method_ids <- as.list(c(
    "tcsp_mc", "wilks_symmetric", optional
  ))
  config$scan_calibration$tiny_n_sim <- 100L

  expect_invisible(env$tcspv_validate_config(config))
  results <- env$tcspv_run_repeated_sample(config, "tiny")
  expect_equal(nrow(results), 2L * 6L)
  expect_true(all(optional %in% results$method_id))
  optional_rows <- results[results$method_id %in% optional, , drop = FALSE]
  expect_true(all(optional_rows$source_package == "tolerance"))
  expect_true(all(nzchar(optional_rows$source_version)))
  expect_true(all(!optional_rows$failed))
})
