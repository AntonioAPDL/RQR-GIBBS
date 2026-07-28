test_that("mean-tilt DGP oracle script reproduces deterministic fixtures", {
  repo_root <- testthat::test_path("..", "..", "..")
  script <- file.path(
    repo_root, "application", "scripts", "validate_mt_rqr_cf_dgps.R"
  )
  env <- new.env(parent = globalenv())
  sys.source(script, envir = env)
  init <- env$load_mean_tilt_initializers(repo_root)
  oracle <- env$make_oracle_table(0.90, init)
  checks <- env$make_oracle_check_table(oracle, 0.90)

  expect_true(all(checks$coverage_pass))
  expect_true(all(checks$rqr_mean_pass))
  expect_true(all(checks$shortest_grid_pass))
  expect_true(all(checks$finite_oracle_pass))

  normal <- oracle[oracle$dgp_id == "normal", ]
  expect_equal(normal$u_shortest, 0.05, tolerance = 2e-7)
  expect_equal(normal$d_shortest, 0, tolerance = 1e-8)
  expect_equal(normal$d_equal_tailed, 0, tolerance = 1e-8)
  expect_equal(normal$d_rqr, 0, tolerance = 1e-8)
  expect_equal(normal$width_shortest, normal$width_equal_tailed,
               tolerance = 1e-7)
  expect_equal(normal$width_shortest, normal$width_rqr,
               tolerance = 1e-7)

  exponential <- oracle[oracle$dgp_id == "exponential", ]
  expect_equal(exponential$u_shortest, 0, tolerance = 1e-10)
  expect_identical(exponential$shortest_boundary, "lower")
  expect_equal(
    exponential$delta_shortest,
    (1 - 0.90) / 0.90 * log(1 - 0.90),
    tolerance = 2e-8
  )

  beta_right <- oracle[oracle$dgp_id == "beta_right", ]
  beta_left <- oracle[oracle$dgp_id == "beta_left", ]
  expect_equal(beta_left$d_shortest, -beta_right$d_shortest,
               tolerance = 5e-8)
  expect_equal(beta_left$d_equal_tailed, -beta_right$d_equal_tailed,
               tolerance = 5e-8)
})

test_that("mean-tilt DGP oracle agrees with independent Python reference", {
  repo_root <- testthat::test_path("..", "..", "..")
  script <- file.path(
    repo_root, "application", "scripts", "validate_mt_rqr_cf_dgps.R"
  )
  env <- new.env(parent = globalenv())
  sys.source(script, envir = env)
  init <- env$load_mean_tilt_initializers(repo_root)
  oracle <- env$make_oracle_table(0.90, init)
  reference <- utils::read.csv(
    file.path(
      repo_root, "application", "inst", "extdata",
      "mean_tilt_cf_oracle_reference_20260727.csv"
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  comparable <- c(
    "coverage", "mean", "sd", "skewness", "u_shortest",
    "L_shortest", "U_shortest", "width_shortest",
    "delta_shortest", "d_shortest", "u_equal_tailed",
    "delta_equal_tailed", "d_equal_tailed",
    "d_cf_shortest", "d_cf_equal_tailed"
  )
  oracle <- oracle[match(reference$dgp, oracle$dgp), ]
  expect_identical(oracle$dgp, reference$dgp)
  expect_identical(oracle$shortest_boundary, reference$shortest_boundary)
  for (field in comparable) {
    expect_equal(
      oracle[[field]], reference[[field]],
      tolerance = 2e-6,
      info = field
    )
  }
})

test_that("mean-tilt validation script writes compact smoke artifacts", {
  repo_root <- testthat::test_path("..", "..", "..")
  script <- file.path(
    repo_root, "application", "scripts", "validate_mt_rqr_cf_dgps.R"
  )
  out_dir <- tempfile("mt_rqr_cf_smoke_")
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
  env <- new.env(parent = globalenv())
  sys.source(script, envir = env)
  result <- env$run_validation(
    out_dir = out_dir, coverage = 0.90, n_values = 40L,
    reps = 3L, seed = 777L, save_replicates = TRUE,
    repo_root = repo_root
  )
  expect_true(all(result$checks$coverage_pass))
  expected <- c(
    "cf_dgp_oracle_targets.csv",
    "cf_dgp_oracle_checks.csv",
    "cf_dgp_monte_carlo_summary.csv",
    "cf_dgp_monte_carlo_replicates.csv",
    "cf_dgp_run_config.csv",
    "cf_dgp_session_info.txt",
    "cf_dgp_oracle_comparison.pdf",
    "cf_dgp_oracle_comparison.png",
    "cf_dgp_rmse_comparison.pdf",
    "cf_dgp_rmse_comparison.png"
  )
  expect_true(all(file.exists(file.path(out_dir, expected))))
})
