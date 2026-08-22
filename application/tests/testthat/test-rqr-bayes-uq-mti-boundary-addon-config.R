`%||%` <- function(a, b) if (is.null(a)) b else a

boundary_addon_config_path <- function() {
  testthat::test_path(
    "..", "..", "config",
    "rqr_bayes_uq_validation_mti_ecm_boundary_addon_20260821.json"
  )
}

test_that("MTI-ECM boundary add-on config freezes the intended scope", {
  config <- jsonlite::read_json(
    boundary_addon_config_path(), simplifyVector = FALSE
  )
  expect_true(config$execution$smoke_authorized)
  expect_true(config$execution$confirmatory_authorized)
  expect_true(config$claim_scope$iid_univariate_continuous_only)
  expect_true(config$claim_scope$mti_ecm_boundary_continuation_empirical_validation)
  expect_false(config$claim_scope$posterior_endpoint_coverage_claim_available)

  smoke <- config$modes$smoke
  expect_equal(as.character(smoke$method_ids), "tcsp_mti_ecm_boundary_mc")
  expect_equal(as.numeric(smoke$posterior_confidences), 0.95)

  main <- config$modes$confirmatory
  expect_equal(as.character(main$method_ids), "tcsp_mti_ecm_boundary_mc")
  expect_equal(as.integer(main$replications), 1000L)
  expect_equal(length(main$design_cells), 9L)
  expect_equal(length(main$dgp_ids), 8L)

  methods <- vapply(config$methods, `[[`, character(1), "method_id")
  method <- config$methods[[which(methods == "tcsp_mti_ecm_boundary_mc")]]
  expect_false(method$formal_tolerance_action)
  expect_true(method$generalized_bayes)
  expect_false(method$response_likelihood)
  expect_equal(method$mti_boundary_rule, "half_step_continuation")
  expect_equal(config$engine_defaults$mti_ecm$confirmatory_ecm_control$
                 ecm_backend, "cpp")
  expect_equal(config$engine_defaults$mti_ecm$confirmatory_ecm_control$
                 tol_stationarity, 1e-3)
})

test_that("MTI-ECM boundary add-on smoke worker fits boundary rows", {
  output_dir <- tempfile("rqr-bayes-uq-mti-boundary-smoke-")
  on.exit(unlink(output_dir, recursive = TRUE, force = TRUE), add = TRUE)

  script <- normalizePath(
    testthat::test_path("..", "..", "scripts", "69_validate_rqr_bayes_uq.R"),
    winslash = "/",
    mustWork = TRUE
  )
  config <- normalizePath(
    boundary_addon_config_path(), winslash = "/", mustWork = TRUE
  )
  status <- system2(
    "Rscript",
    c(script, "--mode=smoke", paste0("--config=", config),
      paste0("--output-dir=", output_dir), "--wave-id=boundary_addon_smoke"),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_equal(attr(status, "status") %||% 0L, 0L)

  results <- read.csv(file.path(output_dir, "bayes_uq_validation_results.csv"))
  summary <- read.csv(file.path(output_dir, "bayes_uq_validation_summary.csv"))
  expect_equal(unique(results$method_id), "tcsp_mti_ecm_boundary_mc")
  expect_true(all(!results$infeasible))
  expect_true(all(results$response_likelihood == FALSE))
  expect_true(all(results$generalized_bayes == TRUE))
  expect_true(all(results$ecm_backend == "cpp"))
  expect_true(any(results$mti_boundary_continuation))
  boundary_rows <- results[results$mti_boundary_continuation, , drop = FALSE]
  expect_true(all(boundary_rows$scan_target_content == 1))
  expect_true(all(boundary_rows$mti_ecm_target_content <
                    boundary_rows$scan_target_content))
  expect_true(all(boundary_rows$mti_ecm_target_content >
                    boundary_rows$guaranteed_content))
  expect_true(all(is.finite(boundary_rows$mti_boundary_epsilon)))
  expect_true(all(is.finite(results$ecm_final_stationarity)))
  expect_true(all(c(
    "mean_scan_target_content", "mean_mti_ecm_target_content",
    "mti_boundary_continuation_rate", "mean_mti_boundary_epsilon",
    "ecm_cpp_rate"
  ) %in% names(summary)))
})
