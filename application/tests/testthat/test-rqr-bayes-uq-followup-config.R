`%||%` <- function(a, b) if (is.null(a)) b else a

followup_config_path <- function() {
  testthat::test_path(
    "..", "..", "config", "rqr_bayes_uq_followup_20260816.json"
  )
}

full_range_confidence <- function(n, content) {
  1 - n * (1 - content) * content^(n - 1) - content^n
}

first_full_range_n <- function(content, confidence) {
  n <- 2L
  repeat {
    if (full_range_confidence(n, content) >= confidence) return(n)
    n <- n + 1L
  }
}

mode_cells <- function(mode_cfg) {
  cells <- mode_cfg$design_cells
  do.call(rbind, lapply(cells, function(cell) {
    data.frame(
      cell_id = cell$cell_id,
      n = as.integer(cell$n),
      guaranteed_content = as.numeric(cell$guaranteed_content),
      tolerance_confidence = as.numeric(cell$tolerance_confidence),
      stringsAsFactors = FALSE
    )
  }))
}

test_that("follow-up config separates 90% and 95% feasibility thresholds", {
  config <- jsonlite::read_json(followup_config_path(), simplifyVector = FALSE)

  expect_true(config$execution$ecm200_audit_authorized)
  expect_true(config$execution$paper_matched_90_authorized)
  expect_true(config$execution$small_sample_95_authorized)
  expect_true(config$claim_scope$iid_univariate_continuous_only)
  expect_false(config$claim_scope$regression_tolerance)
  expect_false(config$claim_scope$dynamic_tolerance)

  expect_equal(
    vapply(c(0.90, 0.95, 0.99), first_full_range_n, integer(1L),
           confidence = 0.90),
    c(38L, 77L, 388L)
  )
  expect_equal(
    vapply(c(0.90, 0.95, 0.99), first_full_range_n, integer(1L),
           confidence = 0.95),
    c(46L, 93L, 473L)
  )

  paper <- mode_cells(config$modes$paper_matched_90)
  expect_equal(paper$n, c(38L, 77L, 388L))
  expect_equal(paper$guaranteed_content, c(0.90, 0.95, 0.99))
  expect_true(all(paper$tolerance_confidence == 0.90))

  small <- mode_cells(config$modes$small_sample_95)
  expect_true(all(small$tolerance_confidence == 0.95))
  expect_true(all(c(46L, 93L, 473L) %in% small$n))
  expect_false(any(
    small$n == 38L & small$guaranteed_content == 0.90 |
      small$n == 77L & small$guaranteed_content == 0.95 |
      small$n == 388L & small$guaranteed_content == 0.99
  ))

  methods <- vapply(config$methods, `[[`, character(1L), "method_id")
  expect_true(all(c("young_mathew", "tcsp_mti_gibbs_median_mc",
                    "tcsp_mti_ecm_map_mc") %in% methods))
  expect_equal(
    config$engine_defaults$mti_ecm$ecm200_audit_ecm_control$max_iter,
    200
  )
  expect_true(config$engine_defaults$mti_ecm$ecm200_audit_ecm_control$
                store_iteration_trace)
})

test_that("follow-up smoke worker emits Young-Mathew and ECM diagnostics", {
  output_dir <- tempfile("rqr-bayes-uq-followup-smoke-")
  on.exit(unlink(output_dir, recursive = TRUE, force = TRUE), add = TRUE)

  script <- normalizePath(
    testthat::test_path("..", "..", "scripts", "69_validate_rqr_bayes_uq.R"),
    winslash = "/",
    mustWork = TRUE
  )
  config <- normalizePath(followup_config_path(), winslash = "/", mustWork = TRUE)
  status <- system2(
    "Rscript",
    c(script, "--mode=smoke", paste0("--config=", config),
      paste0("--output-dir=", output_dir)),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_equal(attr(status, "status") %||% 0L, 0L)

  results <- read.csv(file.path(output_dir, "bayes_uq_validation_results.csv"))
  summary <- read.csv(file.path(output_dir, "bayes_uq_validation_summary.csv"))

  expect_equal(nrow(results), 42L)
  expect_true(any(results$dgp_id == "beta_left"))
  expect_true(any(results$method_id == "young_mathew"))
  expect_true(any(results$method_id == "tcsp_mti_ecm_map_mc"))
  expect_true(all(c(
    "ecm_trace_length", "ecm_initial_objective", "ecm_final_objective",
    "ecm_relative_objective_drop", "ecm_final_stationarity"
  ) %in% names(results)))
  expect_true(all(c(
    "mean_ecm_trace_length", "median_ecm_relative_objective_drop",
    "median_ecm_final_stationarity"
  ) %in% names(summary)))

  ecm_rows <- results[
    results$method_id == "tcsp_mti_ecm_map_mc" & !results$infeasible,
    ,
    drop = FALSE
  ]
  expect_gt(nrow(ecm_rows), 0L)
  expect_true(any(is.finite(ecm_rows$ecm_relative_objective_drop)))
})

test_that("follow-up smoke wave preflight records paired cells", {
  run_root <- tempfile("rqr-bayes-uq-followup-wave-root-")
  on.exit(unlink(run_root, recursive = TRUE, force = TRUE), add = TRUE)

  script <- normalizePath(
    testthat::test_path(
      "..", "..", "scripts", "71_manage_rqr_bayes_uq_main_waves.R"
    ),
    winslash = "/",
    mustWork = TRUE
  )
  config <- normalizePath(followup_config_path(), winslash = "/", mustWork = TRUE)
  status <- system2(
    "Rscript",
    c(script, "--action=prepare", "--mode=smoke",
      paste0("--config=", config), paste0("--run-root=", run_root),
      "--run-id=test_followup_smoke", "--require-clean=false"),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_equal(attr(status, "status") %||% 0L, 0L)

  run_dir <- file.path(run_root, "test_followup_smoke")
  wave_plan <- read.csv(file.path(run_dir, "wave_plan.csv"),
                        stringsAsFactors = FALSE)
  health <- jsonlite::read_json(file.path(run_dir, "health.json"),
                                simplifyVector = TRUE)

  expect_equal(nrow(wave_plan), 6L)
  expect_true("cell_id" %in% names(wave_plan))
  expect_equal(sum(wave_plan$expected_result_rows), 42L)
  expect_equal(health$rows_expected, 42L)
})
