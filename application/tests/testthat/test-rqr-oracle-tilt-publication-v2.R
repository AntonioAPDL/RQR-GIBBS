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
source(file.path(
  repo_root, "application", "scripts", "40_oracle_tilt_publication_v2_utils.R"
))

config_path <- file.path(
  repo_root, "application", "config",
  "oracle_tilt_c095_publication_v2_20260731.json"
)
read_config <- function() oti_read_json(config_path)

testthat::test_that("v2 freezes the 0.80/95-percent exact-oracle contract", {
  config <- read_config()
  testthat::expect_invisible(otv2_validate_config(config))
  plan <- otv2_plan(config)
  testthat::expect_equal(nrow(plan), 27L)
  testthat::expect_equal(
    unname(as.integer(table(plan$family))), c(15L, 12L)
  )
  testthat::expect_equal(length(unique(plan$seed)), 27L)
  testthat::expect_false(any(plan$cornish_fisher_used))
  testthat::expect_identical(
    config$fixed_design$mcmc_control$kernel_repetitions, 2L
  )
  testthat::expect_type(config$execution_authorized, "logical")
  testthat::expect_length(config$execution_authorized, 1L)

  law <- otv2_law(config)
  testthat::expect_equal(law$raw_mean, -3.75, tolerance = 1e-12)
  testthat::expect_equal(
    law$raw_sd, 5.153882032022076, tolerance = 1e-12
  )
  testthat::expect_equal(
    otv2_al_skewness(0.8), -1.7976169855634085, tolerance = 1e-12
  )
  oracle <- oti_oracle_targets(law, 0.95, c("RQR", "ET", "SH"))
  reproduced_content <- law$p(oracle$upper_innovation) -
    law$p(oracle$lower_innovation)
  testthat::expect_equal(reproduced_content, rep(0.95, 3L), tolerance = 1e-10)
  testthat::expect_equal(
    oracle$retained_mean_innovation, oracle$delta_innovation,
    tolerance = 1e-10
  )
  testthat::expect_lt(abs(oracle$delta_innovation[oracle$target == "RQR"]),
                      1e-10)
  testthat::expect_lt(abs(
    law$d(oracle$lower_innovation[oracle$target == "SH"]) -
      law$d(oracle$upper_innovation[oracle$target == "SH"])
  ), 1e-8)
})

testthat::test_that("config validation rejects silent design drift", {
  config <- read_config()
  bad <- config
  bad$innovation$tau <- 0.99
  testthat::expect_error(otv2_validate_config(bad), "AL_0.80")
  bad <- config
  bad$fixed_design$mcmc_control$n_mcmc <- 6000.5
  testthat::expect_error(otv2_validate_config(bad), "finite integer")
  bad <- config
  bad$fixed_design$mcmc_control$kernel_repetitions <- 1L
  testthat::expect_error(otv2_validate_config(bad), "MCMC contract")
  bad <- config
  bad$dlm$mcmc_control$store_state_draws <- FALSE
  testthat::expect_error(otv2_validate_config(bad), "storage contract")
  bad <- config
  bad$diagnostics$rhat_max <- 1.05
  testthat::expect_error(otv2_validate_config(bad), "diagnostic contract")
})

testthat::test_that("empirical basis is orthogonal and truth-exact", {
  preflight <- otv2_design_preflight(read_config())
  basis <- preflight$fixed_dgp$basis
  testthat::expect_equal(basis$rank, 3L)
  testthat::expect_equal(
    unname(basis$gram), diag(3L), tolerance = 1e-12
  )
  testthat::expect_lt(basis$maximum_reconstruction_error, 1e-12)
  testthat::expect_true(all(
    preflight$projection_audit$max_absolute_residual < 1e-12
  ))
  selected <- preflight$static_prior_audit$selected
  testthat::expect_equal(sum(selected), 1L)
  testthat::expect_equal(
    preflight$static_prior_audit$tau2[selected], 1,
    tolerance = 1e-12
  )
  testthat::expect_gte(
    preflight$static_prior_audit$center_half_width[selected], 2.5
  )
  testthat::expect_lte(
    preflight$static_prior_audit$maximum_half_width[selected], 6
  )
})

testthat::test_that("normalized-time DLM preserves the physical prior", {
  config <- read_config()
  preflight <- otv2_design_preflight(config)
  dgp <- preflight$dlm_dgp
  testthat::expect_equal(length(dgp$time), 1200L)
  testthat::expect_equal(range(dgp$time), c(1 / 1200, 1))
  testthat::expect_equal(sum(!dgp$observed), 22L)
  testthat::expect_equal(sum(dgp$observed), 1178L)
  for (target in c("RQR", "ET", "SH")) {
    truth <- oti_target_row(preflight$dlm_targets, target)
    testthat::expect_identical(is.na(truth$mean_tilt), is.na(dgp$y))
  }
  testthat::expect_equal(
    max(preflight$fixed_horizon_audit$max_absolute_error), 0,
    tolerance = 1e-10
  )
  selected <- preflight$dlm_prior_audit$selected
  testthat::expect_equal(sum(selected), 1L)
  testthat::expect_equal(
    preflight$dlm_prior_audit[selected, c(
      "initial_level_variance", "initial_slope_variance",
      "q_level", "q_slope"
    )],
    data.frame(
      initial_level_variance = 1, initial_slope_variance = 0.25,
      q_level = 0.04, q_slope = 0.09
    ),
    ignore_attr = TRUE
  )
  matrices <- otv2_local_linear_matrices(1 / 1200, 0.04, 0.09)
  testthat::expect_equal(dgp$model$GG[, , 1L], matrices$G)
  testthat::expect_equal(dgp$W[, , 1200L], matrices$W)
  testthat::expect_equal(matrices$W, t(matrices$W), tolerance = 1e-15)
  testthat::expect_true(min(eigen(matrices$W, symmetric = TRUE)$values) > 0)
})

testthat::test_that("all exact-oracle cells have adequate rare-tail counts", {
  preflight <- otv2_design_preflight(read_config())
  tail <- preflight$tail_information
  testthat::expect_equal(tail$target, c("RQR", "ET", "SH"))
  testthat::expect_true(all(tail$fixed_design_expected_rare_count >= 10))
  testthat::expect_true(all(tail$dlm_expected_rare_count >= 10))
  testthat::expect_equal(
    min(tail$dlm_expected_rare_count), 11.7800010264016,
    tolerance = 1e-10
  )
})

testthat::test_that("DGPs and overdispersed paths are deterministic", {
  config <- read_config()
  first <- otv2_design_preflight(config)
  second <- otv2_design_preflight(config)
  testthat::expect_identical(first$fixed_dgp$y, second$fixed_dgp$y)
  testthat::expect_identical(first$dlm_dgp$y, second$dlm_dgp$y)
  truth <- oti_target_row(first$dlm_targets, "SH")
  centered <- otv2_initial_state_paths(
    "oracle_centered", first$dlm_dgp, truth
  )
  stressed <- otv2_initial_state_paths("slope_stress", first$dlm_dgp, truth)
  offset <- 2 * sqrt(first$dlm_dgp$initial_slope_variance)
  testthat::expect_equal(
    stressed$state_root2[2L, ] - centered$state_root2[2L, ],
    rep(offset, 1200L)
  )
  testthat::expect_equal(
    stressed$state_root2[1L, ] - centered$state_root2[1L, ],
    offset * first$dlm_dgp$time
  )
})

testthat::test_that("preflight and independent conditional references pass", {
  config <- read_config()
  preflight <- otv2_design_preflight(config)
  testthat::expect_equal(nrow(preflight$gates), 8L)
  testthat::expect_true(all(preflight$gates$pass))
  references <- otv2_reference_suite(config)
  testthat::expect_equal(nrow(references), 12L)
  testthat::expect_true(all(references$pass))
  testthat::expect_equal(
    references$value[references$gate == "dlm_R_repair_count"], 0
  )
  testthat::expect_equal(
    references$value[references$gate == "dlm_cpp_repair_count"], 0
  )
})

testthat::test_that("recovery gates distinguish adequate and poor fits", {
  config <- read_config()
  x <- seq(-1, 1, length.out = 101L)
  curves <- data.frame(
    x = x, oracle_lower = -1 + 0.2 * x, oracle_upper = 1 + 0.2 * x,
    fit_lower = -1 + 0.2 * x, fit_upper = 1 + 0.2 * x,
    fit_lower_q025 = -1.1 + 0.2 * x,
    fit_lower_q975 = -0.9 + 0.2 * x,
    fit_upper_q025 = 0.9 + 0.2 * x,
    fit_upper_q975 = 1.1 + 0.2 * x
  )
  metrics <- data.frame(oracle_mean_width = 2, mean_width = 2)
  good <- otv2_recovery_summary("fixed_design", curves, metrics, config)
  testthat::expect_equal(good$endpoint_rmse_over_oracle_width, 0)
  testthat::expect_equal(good$mean_width_ratio, 1)
  curves$fit_lower <- curves$fit_lower - 1
  bad <- otv2_recovery_summary("fixed_design", curves, metrics, config)
  testthat::expect_gt(
    bad$endpoint_rmse_over_oracle_width,
    config$recovery_gates$endpoint_rmse_over_oracle_width_max
  )
})

testthat::test_that("benchmark assessment rejects gross recovery failure", {
  config <- read_config()
  preflight <- otv2_design_preflight(config)
  truth <- oti_target_row(preflight$fixed_targets, "SH")
  n_draw <- 20L
  exact_lower <- matrix(truth$oracle_lower, nrow = length(truth$oracle_lower),
                        ncol = n_draw)
  exact_upper <- matrix(truth$oracle_upper, nrow = length(truth$oracle_upper),
                        ncol = n_draw)
  exact_pred <- list(
    lower_draws = exact_lower,
    upper_draws = exact_upper,
    midpoint_draws = 0.5 * (exact_lower + exact_upper),
    width_draws = exact_upper - exact_lower,
    lower_mean = rowMeans(exact_lower),
    upper_mean = rowMeans(exact_upper),
    midpoint_mean = rowMeans(0.5 * (exact_lower + exact_upper)),
    width_mean = rowMeans(exact_upper - exact_lower)
  )
  good <- otv2_benchmark_assessment(
    "fixed_design", "SH", list(pred = exact_pred), preflight$fixed_dgp,
    preflight$fixed_targets, config
  )
  testthat::expect_true(good$gross_recovery_pass)
  testthat::expect_true(good$pathology_pass)

  shifted <- exact_pred
  shifted$lower_draws <- shifted$lower_draws - 2 *
    mean(truth$oracle_width)
  shifted$lower_mean <- rowMeans(shifted$lower_draws)
  shifted$midpoint_draws <- 0.5 *
    (shifted$lower_draws + shifted$upper_draws)
  shifted$midpoint_mean <- rowMeans(shifted$midpoint_draws)
  shifted$width_draws <- shifted$upper_draws - shifted$lower_draws
  shifted$width_mean <- rowMeans(shifted$width_draws)
  bad <- otv2_benchmark_assessment(
    "fixed_design", "SH", list(pred = shifted), preflight$fixed_dgp,
    preflight$fixed_targets, config
  )
  testthat::expect_false(bad$gross_recovery_pass)
})

testthat::test_that("two complete static transitions preserve a tilted target", {
  x <- seq(-1, 1, length.out = 18L)
  X <- cbind(intercept = 1, x = x)
  y <- 0.2 + 0.4 * x + seq(-0.15, 0.15, length.out = length(x))
  fit <- rqrgibbs::rqr_mcmc_fit(
    y = y, X = X, coverage_level = 0.8, learning_rate = 1,
    learning_rate_mode = "fixed_rate", mean_tilt = rep(0.15, length(y)),
    beta_prior_obj = rqrgibbs::beta_prior(
      "ridge", ridge = list(tau2 = 2)
    ),
    numerical_policy = "fail",
    mcmc_control = list(
      n_burn = 3L, n_mcmc = 8L, thin = 1L, seed = 202607319L,
      kernel_repetitions = 2L, store_latent_draws = FALSE
    )
  )
  testthat::expect_identical(fit$model_spec$kernel_repetitions, 2L)
  testthat::expect_identical(
    fit$model_spec$mean_tilt_summary$nonzero_count, length(y)
  )
  testthat::expect_true(fit$model_spec$exact_joint_target)
  testthat::expect_equal(fit$model_spec$numerical_repair_count, 0L)
  testthat::expect_true(all(
    fit$diagnostics$root_swap_count_trace %in% 0:2
  ))
})
