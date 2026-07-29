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
  testthat::expect_true(file.exists(file.path(tmp, "artifact_manifest.csv")))
  plan <- read.csv(file.path(tmp, "fit_plan.csv"))
  testthat::expect_equal(nrow(plan), 6L)
  testthat::expect_setequal(plan$family, c("fixed_design", "dlm"))
  testthat::expect_setequal(plan$target, c("RQR", "ET", "SH"))
})
