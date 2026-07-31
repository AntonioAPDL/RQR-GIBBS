testthat::local_edition(3)

repo_root <- normalizePath(file.path(testthat::test_path(), "..", "..", ".."))
source(file.path(
  repo_root, "application", "scripts", "32_oracle_tilt_illustration_utils.R"
))
source(file.path(
  repo_root, "application", "scripts", "33_oracle_tilt_forensic_utils.R"
))
source(file.path(
  repo_root, "application", "scripts", "34_oracle_tilt_publication_utils.R"
))

config_path <- file.path(
  repo_root, "application", "config",
  "oracle_tilt_c095_publication_20260731.json"
)

testthat::test_that("publication config freezes the balanced 27-chain grid", {
  config <- oti_read_json(config_path)
  testthat::expect_invisible(otp_validate_config(config))
  plan <- otp_plan(config)
  testthat::expect_equal(nrow(plan), 27L)
  testthat::expect_equal(
    unname(as.integer(table(plan$family))), c(15L, 12L)
  )
  testthat::expect_equal(
    unique(plan$n_mcmc[plan$family == "fixed_design"]), 6000L
  )
  testthat::expect_equal(unique(plan$n_mcmc[plan$family == "dlm"]), 6000L)
  testthat::expect_false(any(plan$cornish_fisher_used))
  seeds <- mapply(
    function(family, target, chain) {
      otp_seed(config, family, target, chain)
    },
    plan$family, plan$target, plan$chain
  )
  testthat::expect_equal(length(unique(seeds)), nrow(plan))
})

testthat::test_that("slope stress is finite and target independent in scale", {
  config <- oti_read_json(config_path)
  law <- oti_law_from_config(config)
  oracle <- oti_oracle_targets(law, config$coverage_level, config$targets)
  dgp <- oti_dlm_dgp(config, law)
  targets <- oti_targets_by_index(
    dgp$mean_truth, dgp$scale_truth, oracle, dgp$observed
  )
  offset <- 2 * sqrt(dgp$initial_slope_variance)
  for (target in c("RQR", "ET", "SH")) {
    truth <- oti_target_row(targets, target)
    baseline <- otf_initial_state_paths("oracle_centered", dgp, truth)
    stress <- otf_initial_state_paths("slope_stress", dgp, truth)
    testthat::expect_equal(
      stress$theta0_root2[2L] - baseline$theta0_root2[2L], offset
    )
    testthat::expect_equal(
      stress$state_root2[2L, ] - baseline$state_root2[2L, ],
      rep(offset, nrow(dgp$state_truth))
    )
    testthat::expect_true(all(is.finite(unlist(stress))))
  }
})

testthat::test_that("ESS-only failures receive the bounded warning disposition", {
  config <- oti_read_json(config_path)
  chains <- data.frame(
    numerical_repair_count = rep(0L, 4L),
    exact_joint_target = TRUE,
    target_numerical_eligible = TRUE,
    reproducibility_eligible = TRUE,
    promotion_eligible = TRUE,
    primary_runtime_source_match = TRUE
  )
  diagnostics <- data.frame(
    rhat = c(1.001, 1.004),
    ess_bulk = c(1200, 350),
    ess_tail = c(1100, 190),
    mcse_over_sd = c(0.02, 0.06),
    pass = c(TRUE, FALSE)
  )
  warning <- otp_cell_disposition(
    "fixed_design", "SH", diagnostics, chains, config
  )
  testthat::expect_true(warning$hard_pass)
  testthat::expect_identical(
    warning$disposition, "illustration_warning_ess_only"
  )
  diagnostics$rhat[2L] <- 1.08
  failed <- otp_cell_disposition(
    "fixed_design", "SH", diagnostics, chains, config
  )
  testthat::expect_false(failed$hard_pass)
  testthat::expect_identical(failed$disposition, "fail")
})

testthat::test_that("publication plotters write deterministic PDF and PNG types", {
  x <- seq(-1, 1, length.out = 20)
  curves <- do.call(rbind, lapply(c("RQR", "ET", "SH"), function(target) {
    data.frame(
      family = "fixed_design", target = target, index = seq_along(x),
      x = x, y = sin(x), mean_truth = sin(x),
      oracle_lower = sin(x) - 1, oracle_upper = sin(x) + 1,
      fit_lower = sin(x) - 0.9, fit_upper = sin(x) + 0.9,
      fit_midpoint = sin(x), fit_width = 1.8,
      fit_lower_q025 = sin(x) - 1.1,
      fit_lower_q975 = sin(x) - 0.7,
      fit_upper_q025 = sin(x) + 0.7,
      fit_upper_q975 = sin(x) + 1.1,
      fit_midpoint_q025 = sin(x) - 0.1,
      fit_midpoint_q975 = sin(x) + 0.1,
      fit_width_q025 = 1.6, fit_width_q975 = 2,
      observed = TRUE
    )
  }))
  errors <- do.call(rbind, lapply(c("RQR", "ET", "SH"), function(target) {
    do.call(rbind, lapply(c("lower", "upper"), function(endpoint) {
      e <- seq(-1, 1, length.out = 100)
      data.frame(
        family = "fixed_design", target = target, endpoint = endpoint,
        error = e, density = stats::dnorm(e), q025 = -0.7,
        median = 0, q975 = 0.7, mean_error = 0, rmse = 0.2
      )
    }))
  }))
  directory <- withr::local_tempdir()
  pdf <- file.path(directory, "fit.pdf")
  png <- file.path(directory, "errors.png")
  testthat::expect_invisible(
    oti_plot_curve_panels(curves, pdf, "Fixture")
  )
  testthat::expect_invisible(
    oti_plot_endpoint_error_panels(errors, png, "Fixture")
  )
  testthat::expect_gt(file.info(pdf)$size, 0)
  testthat::expect_gt(file.info(png)$size, 0)
})
