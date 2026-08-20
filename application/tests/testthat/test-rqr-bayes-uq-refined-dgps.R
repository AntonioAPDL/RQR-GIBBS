`%||%` <- function(a, b) if (is.null(a)) b else a

refined_config_path <- function() {
  testthat::test_path(
    "..", "..", "config",
    "rqr_bayes_uq_validation_main_3method_refined_dgps_20260820.json"
  )
}

read_refined_config <- function() {
  jsonlite::read_json(refined_config_path(), simplifyVector = FALSE)
}

write_temp_config <- function(config) {
  path <- tempfile("rqr-bayes-uq-refined-", fileext = ".json")
  jsonlite::write_json(config, path, pretty = TRUE, auto_unbox = TRUE)
  path
}

test_that("refined three-method validation config has the intended scope", {
  config <- read_refined_config()

  expect_identical(
    config$study_id,
    "rqr_bayes_uq_validation_main_3method_refined_dgps_20260820"
  )
  expect_true(config$execution$confirmatory_authorized)
  expect_true(config$claim_scope$iid_univariate_continuous_only)
  expect_false(config$claim_scope$regression_tolerance)
  expect_false(config$claim_scope$dynamic_tolerance)
  expect_true(config$claim_scope$coverage_levels_are_guaranteed_contents)
  expect_true(config$claim_scope$structurally_infeasible_cells_excluded)
  expect_identical(config$scan_calibration$method, "monte_carlo_cp_adaptive")

  confirmatory <- config$modes$confirmatory
  dgp_ids <- as.character(unlist(confirmatory$dgp_ids, use.names = FALSE))
  method_ids <- as.character(unlist(confirmatory$method_ids, use.names = FALSE))

  expect_equal(length(dgp_ids), 8L)
  expect_setequal(
    dgp_ids,
    c("normal", "laplace", "student_t3", "contaminated_normal",
      "gamma2", "exponential", "lognormal_hard", "beta52")
  )
  expect_false(any(dgp_ids %in% c("mixture", "sharp_mixture")))
  expect_identical(method_ids, c("tcsp_mc", "young_mathew", "wilks_minmax"))
  expect_equal(length(confirmatory$design_cells), 9L)
  expect_equal(as.integer(confirmatory$replications), 1000L)

  methods <- setNames(config$methods, vapply(config$methods, `[[`,
                                            character(1L), "method_id"))
  expect_identical(methods$tcsp_mc$scan_method, "monte_carlo_cp_adaptive")
  expect_true(methods$tcsp_mc$formal_tolerance_action)
  expect_false(methods$young_mathew$response_likelihood)
  expect_false(methods$wilks_minmax$response_likelihood)

  expected_rows <- length(dgp_ids) * length(confirmatory$design_cells) *
    length(method_ids) * as.integer(confirmatory$replications)
  expect_equal(expected_rows, 216000L)
})

test_that("refined DGP oracle mappings are available during wave preflight", {
  config <- read_refined_config()
  config$modes$smoke$replications <- 1L
  config$modes$smoke$design_cells <- list(
    list(
      cell_id = "n0050_c090_t095",
      n = 50,
      guaranteed_content = 0.90,
      tolerance_confidence = 0.95
    )
  )
  config$modes$smoke$sample_sizes <- list(50L)
  config$modes$smoke$guaranteed_contents <- list(0.90)
  config$modes$smoke$method_ids <- list("tcsp_mc")
  config$modes$smoke$scan_n_sim <- 80L
  config$modes$smoke$scan_numerical_confidence <- 0.90
  config$modes$smoke$scan_adaptive_control <- list(
    initial_n_sim = 80L,
    batch_n_sim = 80L,
    max_n_sim = 160L,
    max_looks = 2L,
    stable_looks = 1L
  )
  config_path <- write_temp_config(config)
  run_root <- tempfile("rqr-bayes-uq-refined-preflight-")
  on.exit(unlink(c(config_path, run_root), recursive = TRUE, force = TRUE),
          add = TRUE)

  script <- normalizePath(
    testthat::test_path(
      "..", "..", "scripts", "71_manage_rqr_bayes_uq_main_waves.R"
    ),
    winslash = "/",
    mustWork = TRUE
  )
  status <- system2(
    "Rscript",
    c(script, "--action=prepare", "--mode=smoke",
      paste0("--config=", config_path),
      paste0("--run-root=", run_root),
      "--run-id=preflight_refined_dgps_test",
      "--require-clean=false"),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_equal(attr(status, "status") %||% 0L, 0L, info = paste(status, collapse = "\n"))

  run_dir <- file.path(run_root, "preflight_refined_dgps_test")
  manifest <- jsonlite::read_json(
    file.path(run_dir, "preflight_manifest.json"), simplifyVector = TRUE
  )
  oracle <- read.csv(file.path(run_dir, "oracle_reference.csv"))
  wave_plan <- read.csv(file.path(run_dir, "wave_plan.csv"))

  expect_equal(manifest$n_waves, 8L)
  expect_equal(manifest$expected_result_rows, 8L)
  expect_equal(manifest$oracle_certificates, 8L)
  expect_setequal(
    oracle$dgp_id,
    c("normal", "laplace", "student_t3", "contaminated_normal",
      "gamma2", "exponential", "lognormal_hard", "beta52")
  )
  expect_true(all(is.finite(oracle$lower)))
  expect_true(all(is.finite(oracle$upper)))
  expect_true(all(oracle$lower < oracle$upper))
  expect_true(all(oracle$content_residual >= -1e-8))
  expect_setequal(wave_plan$method_ids, "tcsp_mc")
})

test_that("worker executes the newly added DGP families", {
  config <- read_refined_config()
  new_dgps <- c("laplace", "gamma2", "exponential", "beta52")
  config$modes$smoke$replications <- 1L
  config$modes$smoke$design_cells <- list(
    list(
      cell_id = "n0050_c090_t095",
      n = 50,
      guaranteed_content = 0.90,
      tolerance_confidence = 0.95
    )
  )
  config$modes$smoke$sample_sizes <- list(50L)
  config$modes$smoke$guaranteed_contents <- list(0.90)
  config$modes$smoke$dgp_ids <- as.list(new_dgps)
  config$modes$smoke$method_ids <- list(
    "tcsp_mc", "young_mathew", "wilks_minmax"
  )
  config$modes$smoke$scan_n_sim <- 80L
  config$modes$smoke$scan_numerical_confidence <- 0.90
  config$modes$smoke$scan_adaptive_control <- list(
    initial_n_sim = 80L,
    batch_n_sim = 80L,
    max_n_sim = 160L,
    max_looks = 2L,
    stable_looks = 1L
  )

  config_path <- write_temp_config(config)
  output_dir <- tempfile("rqr-bayes-uq-refined-worker-")
  on.exit(unlink(c(config_path, output_dir), recursive = TRUE, force = TRUE),
          add = TRUE)

  script <- normalizePath(
    testthat::test_path(
      "..", "..", "scripts", "69_validate_rqr_bayes_uq.R"
    ),
    winslash = "/",
    mustWork = TRUE
  )
  status <- system2(
    "Rscript",
    c(script, "--mode=smoke", paste0("--config=", config_path),
      paste0("--output-dir=", output_dir), "--wave-id=refined_new_dgps",
      paste0("--wave-dgp=", paste(new_dgps, collapse = ",")),
      "--wave-n=50", "--wave-content=0.90"),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_equal(attr(status, "status") %||% 0L, 0L, info = paste(status, collapse = "\n"))

  results <- read.csv(file.path(output_dir, "bayes_uq_validation_results.csv"))
  expect_equal(nrow(results), length(new_dgps) * 3L)
  expect_setequal(results$dgp_id, new_dgps)
  expect_setequal(
    results$method_id,
    c("tcsp_mc", "young_mathew", "wilks_minmax")
  )
  expect_true(all(is.finite(results$formal_action_content)))
  expect_true(all(is.finite(results$width[!results$infeasible])))
  tcsp_rows <- results[results$method_id == "tcsp_mc", , drop = FALSE]
  expect_true(all(tcsp_rows$scan_critical_method == "monte_carlo_cp_adaptive"))
})
