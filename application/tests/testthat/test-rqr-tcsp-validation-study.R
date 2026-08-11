tcsp_validation_environment <- function() {
  env <- new.env(parent = globalenv())
  sys.source(
    testthat::test_path(
      "..", "..", "scripts", "lib", "tcsp_validation_study.R"
    ),
    envir = env
  )
  env
}

tcsp_validation_config <- function(env) {
  env$tcspv_read_config(testthat::test_path(
    "..", "..", "config", "tcsp_validation_v1.json"
  ))
}

test_that("TCSP validation config is fail-closed and complete", {
  env <- tcsp_validation_environment()
  config <- tcsp_validation_config(env)
  expect_invisible(env$tcspv_validate_config(config))
  expect_true(config$execution$full_pilot_authorized)
  expect_false(config$execution$confirmatory_authorized)
  expect_true("full_pilot" %in% names(config$modes))
  expect_true(1200L %in% as.integer(unlist(config$modes$full_pilot$sample_sizes)))
  methods <- env$tcspv_methods(config)
  expect_true(all(c(
    "tcsp_dkw", "tcsp_mc", "wilks_symmetric", "normal_howe",
    "oracle_shortest"
  ) %in% methods$method_id))
  disabled <- methods[!methods$enabled, , drop = FALSE]
  expect_true(all(nzchar(disabled$disabled_reason)))
})

test_that("TCSP full-pilot mode uses its own scan-calibration budget", {
  env <- tcsp_validation_environment()
  config <- tcsp_validation_config(env)
  config$scan_calibration$full_pilot_n_sim <- 80L
  mc <- env$tcspv_mc_count(config, 20L, 0.50, 0.80, "full_pilot")
  expect_equal(mc$n_sim, 80L)
  expect_true(isTRUE(mc$certified_lower_probability >= 0.80))
  expect_true(is.finite(mc$seed))
})

test_that("TCSP validation DGP CDFs and quantiles are coherent", {
  env <- tcsp_validation_environment()
  config <- tcsp_validation_config(env)
  for (dgp_id in env$tcspv_dgps(config)$dgp_id) {
    probs <- c(0.10, 0.50, 0.90)
    qs <- env$tcspv_quantile(config, dgp_id, probs)
    expect_true(all(is.finite(qs)))
    expect_true(all(diff(qs) > 0))
    expect_equal(env$tcspv_cdf(config, dgp_id, qs), probs,
                 tolerance = 1e-7)
  }
})

test_that("TCSP validation critical counts satisfy DKW and Wilks contracts", {
  env <- tcsp_validation_environment()
  config <- tcsp_validation_config(env)
  criticals <- env$tcspv_critical_counts(config, "preflight")
  dkw <- criticals[criticals$method_id == "tcsp_dkw", , drop = FALSE]
  expect_true(nrow(dkw) > 0)
  expect_true(any(dkw$infeasible))
  expect_true(any(!dkw$infeasible))
  for (ii in seq_len(nrow(dkw))) {
    eps <- sqrt(log(2 / (1 - dkw$tolerance_confidence[[ii]])) /
                  (2 * dkw$n[[ii]]))
    if (!dkw$infeasible[[ii]]) {
      expect_gt(dkw$retained_count[[ii]] / dkw$n[[ii]] - 2 * eps,
                dkw$guaranteed_content[[ii]])
      expect_lte((dkw$retained_count[[ii]] - 1L) / dkw$n[[ii]] - 2 * eps,
                 dkw$guaranteed_content[[ii]])
    }
  }
  wilks <- criticals[criticals$method_id == "wilks_symmetric", , drop = FALSE]
  expect_true(all(wilks$certificate >= 0 & wilks$certificate <= 1))
})

test_that("TCSP validation methods return evaluable intervals on tiny data", {
  env <- tcsp_validation_environment()
  config <- tcsp_validation_config(env)
  criticals <- env$tcspv_critical_counts(config, "tiny")
  y <- env$tcspv_sample(config, "normal", 40L,
                        env$tcspv_seed(config, "data", "unit", "normal"))
  for (method_id in c("tcsp_dkw", "tcsp_mc", "wilks_symmetric",
                      "equal_tailed_tcsp_content", "normal_howe",
                      "oracle_shortest")) {
    interval <- env$tcspv_interval_for_method(
      config, method_id, y, "normal", 0.80, 0.80, criticals
    )
    expect_true(is.logical(interval$failed))
    if (!interval$failed) {
      eval <- env$tcspv_evaluate_interval(
        config, "normal", interval$lower, interval$upper, 0.80
      )
      expect_true(is.finite(eval$true_content))
      expect_true(is.finite(eval$width))
    }
  }
})

test_that("TCSP tiny repeated-sample runner returns summaries", {
  env <- tcsp_validation_environment()
  config <- tcsp_validation_config(env)
  config$modes$tiny$replications <- 2L
  config$modes$tiny$sample_sizes <- list(40L)
  config$modes$tiny$dgp_ids <- list("normal")
  config$modes$tiny$guaranteed_contents <- list(0.80)
  config$modes$tiny$tolerance_confidences <- list(0.80)
  config$modes$tiny$method_ids <- as.list(c(
    "tcsp_dkw", "tcsp_mc", "wilks_symmetric", "normal_howe",
    "oracle_shortest"
  ))
  config$scan_calibration$tiny_n_sim <- 80L
  results <- env$tcspv_run_repeated_sample(config, "tiny")
  summary <- env$tcspv_summary(results, config$design$summary_confidence)
  expect_equal(nrow(results), 2L * 5L)
  expect_equal(nrow(summary), 5L)
  expect_true(all(c("tolerance_success_rate", "mean_width") %in%
                    names(summary)))
})
