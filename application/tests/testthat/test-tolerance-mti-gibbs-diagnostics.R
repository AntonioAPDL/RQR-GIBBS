source_gibbs_diagnostic_env <- function() {
  old <- Sys.getenv("RQRGIBBS_GIBBS_DIAGNOSTIC_SOURCE_ONLY", unset = NA)
  old_path <- Sys.getenv("RQRGIBBS_GIBBS_DIAGNOSTIC_SCRIPT_PATH", unset = NA)
  script <- normalizePath(
    testthat::test_path(
      "..", "..", "scripts", "77_run_tolerance_mti_gibbs_diagnostics.R"
    ),
    winslash = "/",
    mustWork = TRUE
  )
  Sys.setenv(RQRGIBBS_GIBBS_DIAGNOSTIC_SOURCE_ONLY = "true")
  Sys.setenv(RQRGIBBS_GIBBS_DIAGNOSTIC_SCRIPT_PATH = script)
  on.exit({
    if (is.na(old)) {
      Sys.unsetenv("RQRGIBBS_GIBBS_DIAGNOSTIC_SOURCE_ONLY")
    } else {
      Sys.setenv(RQRGIBBS_GIBBS_DIAGNOSTIC_SOURCE_ONLY = old)
    }
    if (is.na(old_path)) {
      Sys.unsetenv("RQRGIBBS_GIBBS_DIAGNOSTIC_SCRIPT_PATH")
    } else {
      Sys.setenv(RQRGIBBS_GIBBS_DIAGNOSTIC_SCRIPT_PATH = old_path)
    }
  }, add = TRUE)
  env <- new.env(parent = globalenv())
  sys.source(script, envir = env)
  env
}

minimal_gibbs_diagnostic_config <- function() {
  list(
    base_seed = 910100L,
    scan_calibration = list(seed = 910200L),
    dgps = list(list(dgp_id = "normal", family = "normal"))
  )
}

test_that("targeted Gibbs diagnostic plan encodes only adjudication cells", {
  env <- source_gibbs_diagnostic_env()
  plan <- env$gdiag_default_plan()

  expect_equal(nrow(plan), 15L)
  expect_setequal(
    plan$cell_role,
    c("hard_feasible_large", "small_feasible", "expected_fail_closed")
  )
  expect_true(all(plan$run_gibbs[plan$cell_role != "expected_fail_closed"]))
  expect_false(any(plan$run_gibbs[plan$cell_role == "expected_fail_closed"]))
  expect_equal(
    unique(plan$n[plan$cell_role == "hard_feasible_large"]),
    1000L
  )
  expect_equal(
    unique(plan$n[plan$cell_role == "small_feasible"]),
    100L
  )
  expect_equal(
    env$gdiag_split_arg("normal, lognormal_hard,student_t3"),
    c("normal", "lognormal_hard", "student_t3")
  )
  expect_null(env$gdiag_split_arg(""))
})

test_that("task scalarization preserves config lists and row-wise controls", {
  env <- source_gibbs_diagnostic_env()
  row <- env$gdiag_default_plan()[1L, , drop = FALSE]
  row$replication <- 2L
  row$chain <- 3L
  row$task_id <- 7L
  config <- minimal_gibbs_diagnostic_config()

  task <- env$gdiag_scalarize_task_row(
    row = row,
    config = config,
    n_burn = 11L,
    n_mcmc = 13L,
    thin = 2L,
    n_sim = 17L,
    numerical_confidence = 0.9,
    scan_seed_base = 19L,
    chain_seed_base = 23L,
    learning_rate = 1.25,
    beta_ridge_tau2 = 100
  )

  expect_identical(task$config, config)
  expect_identical(task$run_gibbs, TRUE)
  expect_equal(task$replication, 2L)
  expect_equal(task$chain, 3L)
  expect_equal(task$n_burn, 11L)
  expect_equal(task$n_mcmc, 13L)
  expect_equal(task$thin, 2L)
  expect_equal(task$n_sim, 17L)
  expect_equal(task$numerical_confidence, 0.9)
  expect_equal(task$beta_ridge_tau2, 100)
})

test_that("trace slicing and estimator diagnostics are chain-aware", {
  env <- source_gibbs_diagnostic_env()
  expect_equal(env$gdiag_trace_indices(2L, 3L, 1L), 3:5)
  expect_equal(env$gdiag_trace_indices(2L, 3L, 2L), c(4L, 6L, 8L))

  draws <- data.frame(
    mode_source = "unit",
    cell_role = "small_feasible",
    dgp_id = "normal",
    n = 100L,
    guaranteed_content = 0.90,
    tolerance_confidence = 0.95,
    replication = 1L,
    chain = rep(1:2, each = 5L),
    lower = c(seq(-1.1, -0.9, length.out = 5L),
              seq(-1.08, -0.88, length.out = 5L)),
    upper = c(seq(0.9, 1.1, length.out = 5L),
              seq(0.92, 1.12, length.out = 5L)),
    width = 2,
    midpoint = 0,
    loss = seq(10, 1, length.out = 10L),
    target_loss = seq(9, 1, length.out = 10L)
  )
  diagnostics <- env$gdiag_draw_diagnostics(draws)

  expect_setequal(
    diagnostics$estimand,
    c("lower", "upper", "width", "midpoint", "loss", "target_loss")
  )
  expect_true(all(diagnostics$chains == 2L))
  expect_true(all(diagnostics$draws_per_chain == 5L))
  expect_true(is.finite(diagnostics$rhat[diagnostics$estimand == "lower"]))
})

test_that("Gibbs diagnostics fail closed when scan calibration is infeasible", {
  env <- source_gibbs_diagnostic_env()
  task <- list(
    config = minimal_gibbs_diagnostic_config(),
    mode_source = "unit",
    cell_role = "expected_fail_closed",
    dgp_id = "normal",
    n = 50L,
    guaranteed_content = 0.99,
    tolerance_confidence = 0.95,
    replication = 1L,
    chain = 1L,
    run_gibbs = TRUE,
    n_burn = 2L,
    n_mcmc = 4L,
    thin = 1L,
    n_sim = 50L,
    numerical_confidence = 0.9,
    scan_seed_base = 910200L,
    chain_seed_base = 910300L,
    learning_rate = 1,
    beta_ridge_tau2 = 100
  )
  result <- env$gdiag_run_task(task)

  expect_equal(nrow(result$draws), 0L)
  expect_equal(nrow(result$chain_summary), 0L)
  expect_equal(nrow(result$failclosed), 1L)
  expect_match(result$failclosed$reason, "infeasible|No retained count")
})
