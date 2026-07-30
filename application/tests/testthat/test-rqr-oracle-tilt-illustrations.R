testthat::test_that("oracle-tilt targets have the intended population roles", {
  source(testthat::test_path(
    "..", "..", "scripts", "32_oracle_tilt_illustration_utils.R"
  ))
  law <- oti_al_law(tau = 0.65, standardized = TRUE)
  targets <- oti_oracle_targets(law, coverage_level = 0.8)

  testthat::expect_equal(targets$target, c("RQR", "ET", "SH"))
  testthat::expect_true(all(abs(targets$content - 0.8) < 1e-8))
  testthat::expect_lt(abs(targets$delta_innovation[targets$target == "RQR"]), 1e-7)
  testthat::expect_true(
    targets$width_innovation[targets$target == "SH"] <=
      min(targets$width_innovation[targets$target != "SH"]) + 1e-8
  )
  testthat::expect_true(
    abs(targets$u[targets$target == "ET"] - 0.1) < 1e-12
  )
})

testthat::test_that("fixed-design oracle tilts are exactly representable by the design", {
  source(testthat::test_path(
    "..", "..", "scripts", "32_oracle_tilt_illustration_utils.R"
  ))
  config <- list(
    coverage_level = 0.8,
    fixed_design = list(n = 50L, seed = 11L)
  )
  law <- oti_al_law(tau = 0.65, standardized = TRUE)
  dgp <- oti_fixed_design_dgp(config, law)
  oracle <- oti_oracle_targets(law, 0.8)
  targets <- oti_targets_by_index(
    dgp$mean_truth, dgp$scale_truth, oracle, dgp$observed
  )
  for (target in c("RQR", "ET", "SH")) {
    z <- oti_target_row(targets, target)
    lower_fit <- stats::lm(z$oracle_lower ~ dgp$X - 1)
    upper_fit <- stats::lm(z$oracle_upper ~ dgp$X - 1)
    testthat::expect_lt(max(abs(stats::resid(lower_fit))), 1e-10)
    testthat::expect_lt(max(abs(stats::resid(upper_fit))), 1e-10)
    testthat::expect_true(all(is.finite(z$mean_tilt)))
  }
})

testthat::test_that("publication illustration fixes size and exact tau-0.99 oracles", {
  testthat::skip_if_not_installed("jsonlite")
  config <- jsonlite::read_json(
    testthat::test_path(
      "..", "..", "config", "oracle_tilt_illustrations_20260728.json"
    ),
    simplifyVector = TRUE
  )
  testthat::expect_identical(as.integer(config$fixed_design$n), 540L)
  testthat::expect_equal(as.numeric(config$innovation$tau), 0.99)
  testthat::expect_true(isTRUE(config$innovation$standardized))

  source(testthat::test_path(
    "..", "..", "scripts", "32_oracle_tilt_illustration_utils.R"
  ))
  law <- oti_law_from_config(config)
  targets <- oti_oracle_targets(law, config$coverage_level)
  testthat::expect_true(all(
    targets$oracle_construction ==
      "population_quantile_truncated_moment"
  ))
  testthat::expect_true(all(!targets$uses_cornish_fisher))
  testthat::expect_true(all(
    abs(targets$content - config$coverage_level) < 1e-10
  ))
  testthat::expect_lt(
    abs(targets$retained_mean_innovation[targets$target == "RQR"]),
    1e-10
  )
  testthat::expect_equal(
    targets$u[targets$target == "ET"],
    (1 - config$coverage_level) / 2,
    tolerance = 1e-12
  )
  testthat::expect_true(
    targets$width_innovation[targets$target == "SH"] <=
      min(targets$width_innovation[targets$target != "SH"]) + 1e-8
  )
  oracle_width <- targets$width_innovation[targets$target == "SH"]
  u_grid <- seq(0, 1 - config$coverage_level, length.out = 5001L)
  grid_width <- vapply(
    u_grid,
    function(u) law$q(u + config$coverage_level) - law$q(u),
    numeric(1L)
  )
  testthat::expect_lte(oracle_width, min(grid_width) + 1e-7)
})

testthat::test_that("DLM oracle tilts preserve NA masks at missing observations", {
  testthat::skip_if_not_installed("rqrgibbs")
  source(testthat::test_path(
    "..", "..", "scripts", "32_oracle_tilt_illustration_utils.R"
  ))
  config <- list(
    coverage_level = 0.8,
    dlm = list(T = 40L, seed = 12L, missing_times = c(7L, 19L))
  )
  law <- oti_al_law(tau = 0.65, standardized = TRUE)
  dgp <- oti_dlm_dgp(config, law)
  oracle <- oti_oracle_targets(law, 0.8)
  targets <- oti_targets_by_index(
    dgp$mean_truth, dgp$scale_truth, oracle, dgp$observed
  )
  for (target in c("RQR", "ET", "SH")) {
    z <- oti_target_row(targets, target)
    testthat::expect_true(all(is.na(z$mean_tilt[!dgp$observed])))
    testthat::expect_true(all(is.finite(z$mean_tilt[dgp$observed])))
  }
})

testthat::test_that("endpoint error summaries are centered on oracle differences", {
  source(testthat::test_path(
    "..", "..", "scripts", "32_oracle_tilt_illustration_utils.R"
  ))
  truth <- data.frame(
    oracle_lower = c(-1, -0.5, 0),
    oracle_upper = c(1, 1.5, 2),
    stringsAsFactors = FALSE
  )
  pred <- list(
    lower_draws = cbind(
      truth$oracle_lower + c(-0.1, 0.0, 0.1),
      truth$oracle_lower + c(0.0, 0.1, 0.2),
      truth$oracle_lower + c(-0.2, -0.1, 0.0)
    ),
    upper_draws = cbind(
      truth$oracle_upper + c(0.1, 0.0, -0.1),
      truth$oracle_upper + c(0.2, 0.1, 0.0),
      truth$oracle_upper + c(0.0, -0.1, -0.2)
    )
  )
  dens <- oti_endpoint_error_density_frame("fixed_design", "RQR", pred, truth)
  summ <- oti_endpoint_error_summary_frame("fixed_design", "RQR", pred, truth)
  testthat::expect_setequal(dens$endpoint, c("lower", "upper"))
  testthat::expect_setequal(summ$endpoint, c("lower", "upper"))
  testthat::expect_true(all(is.finite(dens$error)))
  testthat::expect_true(all(is.finite(dens$density)))
  testthat::expect_true(all(summ$q025_error <= summ$median_error))
  testthat::expect_true(all(summ$median_error <= summ$q975_error))
  testthat::expect_true(all(summ$rmse >= 0))
})

testthat::test_that("paper run control and chain seeds are explicit", {
  source(testthat::test_path(
    "..", "..", "scripts", "32_oracle_tilt_illustration_utils.R"
  ))
  config <- list(
    paper_mcmc_control = list(
      enabled = TRUE,
      n_chains = 4L,
      diagnostics = list(enabled = TRUE)
    ),
    fixed_design = list(seed = 1000L),
    dlm = list(seed = 2000L)
  )
  paper <- oti_run_control(config, paper_figures = TRUE)
  quick <- oti_run_control(config, quick = TRUE, paper_figures = TRUE)
  one_chain <- oti_run_control(config, paper_figures = TRUE, one_chain = TRUE)

  testthat::expect_true(paper$paper_mode)
  testthat::expect_equal(paper$n_chains, 4L)
  testthat::expect_true(paper$diagnostics$enabled)
  testthat::expect_false(quick$paper_mode)
  testthat::expect_equal(quick$n_chains, 1L)
  testthat::expect_false(one_chain$paper_mode)
  testthat::expect_equal(one_chain$n_chains, 1L)

  seeds <- vapply(seq_len(4L), function(chain) {
    oti_chain_seed(config, "fixed_design", "RQR", chain)
  }, integer(1L))
  testthat::expect_equal(length(unique(seeds)), 4L)
  testthat::expect_equal(seeds[1L], 1100L)
})

testthat::test_that("family paper controls override only the selected family", {
  source(testthat::test_path(
    "..", "..", "scripts", "32_oracle_tilt_illustration_utils.R"
  ))
  config <- list(
    mcmc_control = list(n_burn = 100L, n_mcmc = 200L, thin = 1L),
    paper_mcmc_control = list(
      enabled = TRUE,
      n_chains = 4L,
      n_burn = 1000L,
      n_mcmc = 2000L,
      diagnostics = list(enabled = TRUE)
    ),
    fixed_design = list(mcmc_control = list(store_latent_draws = FALSE)),
    dlm = list(
      mcmc_control = list(backend = "cpp"),
      paper_mcmc_control = list(n_mcmc = 8000L)
    )
  )

  fixed_paper <- oti_mcmc_control(
    config, "fixed_design", paper_mode = TRUE, seed = 11L
  )
  dlm_paper <- oti_mcmc_control(
    config, "dlm", paper_mode = TRUE, seed = 12L
  )
  dlm_regular <- oti_mcmc_control(
    config, "dlm", paper_mode = FALSE, seed = 13L
  )

  testthat::expect_equal(fixed_paper$n_burn, 1000L)
  testthat::expect_equal(fixed_paper$n_mcmc, 2000L)
  testthat::expect_equal(dlm_paper$n_burn, 1000L)
  testthat::expect_equal(dlm_paper$n_mcmc, 8000L)
  testthat::expect_equal(dlm_paper$backend, "cpp")
  testthat::expect_equal(dlm_regular$n_burn, 100L)
  testthat::expect_equal(dlm_regular$n_mcmc, 200L)
})

testthat::test_that("prediction combination and diagnostics have stable schemas", {
  testthat::skip_if_not_installed("posterior")
  source(testthat::test_path(
    "..", "..", "scripts", "32_oracle_tilt_illustration_utils.R"
  ))
  pred1 <- list(
    lower_draws = matrix(1:6, 3, 2),
    upper_draws = matrix(7:12, 3, 2),
    midpoint_draws = matrix(13:18, 3, 2),
    width_draws = matrix(19:24, 3, 2)
  )
  pred2 <- lapply(pred1, function(x) x + 100)
  combined <- oti_combine_predictions(list(pred1, pred2))
  testthat::expect_equal(dim(combined$lower_draws), c(3L, 4L))
  testthat::expect_equal(combined$lower_mean, rowMeans(combined$lower_draws))

  set.seed(1)
  scalar_chains <- list(
    cbind(a = stats::rnorm(80), b = stats::rnorm(80)),
    cbind(a = stats::rnorm(80), b = stats::rnorm(80))
  )
  run_control <- list(
    diagnostics = list(
      enabled = TRUE,
      rhat_max = Inf,
      bulk_ess_min = 1,
      tail_ess_min = 1,
      mcse_over_sd_max = Inf
    )
  )
  diag <- oti_mcmc_diagnostics("fixed_design", "RQR", scalar_chains, run_control)
  testthat::expect_setequal(diag$estimand, c("a", "b"))
  testthat::expect_true(all(is.finite(diag$rhat)))
  testthat::expect_true(all(diag$pass))
})

testthat::test_that("runner dry-run writes the declared compact contract", {
  testthat::skip_if_not_installed("rqrgibbs")
  testthat::skip_if_not_installed("jsonlite")
  source(testthat::test_path(
    "..", "..", "scripts", "32_oracle_tilt_illustration_utils.R"
  ))
  tmp <- tempfile("oti_dry_run_")
  config <- normalizePath(testthat::test_path(
    "..", "..", "config", "oracle_tilt_illustrations_20260728.json"
  ), mustWork = TRUE)
  script <- normalizePath(testthat::test_path(
    "..", "..", "scripts", "32_run_oracle_tilt_illustrations.R"
  ), mustWork = TRUE)
  status <- system2(
    file.path(R.home("bin"), "Rscript"),
    c(
      script,
      paste0("--config=", config),
      paste0("--output-dir=", tmp),
      "--families=fixed_design,dlm",
      "--dry-run"
    )
  )
  testthat::expect_equal(status, 0)
  testthat::expect_true(file.exists(file.path(tmp, "oracle_targets.csv")))
  testthat::expect_true(file.exists(file.path(tmp, "fit_plan.csv")))
  testthat::expect_true(file.exists(file.path(tmp, "runtime_state.json")))
  testthat::expect_true(file.exists(file.path(tmp, "artifact_manifest.csv")))
  plan <- read.csv(file.path(tmp, "fit_plan.csv"))
  testthat::expect_equal(nrow(plan), 6L)
  testthat::expect_setequal(plan$family, c("fixed_design", "dlm"))
  testthat::expect_setequal(plan$target, c("RQR", "ET", "SH"))
  testthat::expect_identical(unique(plan$mean_tilt_source), "population_oracle")
  testthat::expect_true(all(plan$n_chains == 1L))
  testthat::expect_true(all(!plan$paper_mode))
})
