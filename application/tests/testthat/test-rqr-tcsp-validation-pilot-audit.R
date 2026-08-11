tcsp_validation_audit_environment <- function() {
  env <- new.env(parent = globalenv())
  sys.source(
    testthat::test_path(
      "..", "..", "scripts", "lib", "tcsp_validation_study.R"
    ),
    envir = env
  )
  env
}

tcsp_validation_audit_config <- function(env) {
  env$tcspv_read_config(testthat::test_path(
    "..", "..", "config", "tcsp_validation_v1.json"
  ))
}

test_that("TCSP summaries keep all-failed cells explicit", {
  env <- tcsp_validation_audit_environment()
  results <- data.frame(
    mode = "pilot",
    dgp_id = "normal",
    n = 80L,
    guaranteed_content = 0.80,
    tolerance_confidence = 0.80,
    method_id = "tcsp_dkw",
    tolerance_success = c(FALSE, FALSE),
    failed = c(TRUE, TRUE),
    width = c(NA_real_, NA_real_),
    true_content = c(NA_real_, NA_real_),
    content_deficit = c(NA_real_, NA_real_),
    lower_omitted = c(NA_real_, NA_real_),
    upper_omitted = c(NA_real_, NA_real_),
    runtime_sec = c(0.001, 0.001),
    stringsAsFactors = FALSE
  )
  summary <- env$tcspv_summary(results)
  expect_equal(summary$failures, 2L)
  expect_equal(summary$failure_rate, 1)
  expect_equal(summary$tolerance_success_rate, 0)
  expect_true(is.na(summary$mean_width))
  expect_false(is.nan(summary$mean_width))
})

test_that("TCSP pilot audit publishes compact reproducible evidence", {
  env <- tcsp_validation_audit_environment()
  config <- tcsp_validation_audit_config(env)
  config$modes$pilot$replications <- 2L
  config$modes$pilot$sample_sizes <- as.list(c(80L, 250L))
  config$modes$pilot$guaranteed_contents <- list(0.80)
  config$modes$pilot$tolerance_confidences <- list(0.80)
  config$modes$pilot$dgp_ids <- list("normal")
  config$modes$pilot$method_ids <- as.list(c(
    "tcsp_dkw", "tcsp_mc", "wilks_symmetric", "normal_howe"
  ))
  config$scan_calibration$pilot_n_sim <- 80L

  run_dir <- file.path(tempdir(), paste0("tcsp-run-", Sys.getpid()))
  output_dir <- file.path(tempdir(), paste0("tcsp-audit-", Sys.getpid()))
  unlink(c(run_dir, output_dir), recursive = TRUE, force = TRUE)
  repo_root <- normalizePath(testthat::test_path("..", "..", ".."),
                             winslash = "/", mustWork = TRUE)
  env$tcspv_write_run(config, "pilot", run_dir, repo_root)
  expect_silent(env$tcspv_write_pilot_audit(run_dir, output_dir))

  expected <- c(
    "README.md", "audit_summary.json", "audit_gates.csv",
    "method_summary.csv", "cell_summary_compact.csv",
    "critical_count_summary.csv", "dkw_feasibility.csv",
    "mc_calibration_health.csv", "normal_howe_sensitivity.csv",
    "next_stage_plan.csv", "source_run_manifest.csv", "artifact_hashes.csv"
  )
  expect_true(all(file.exists(file.path(output_dir, expected))))
  audit_summary <- jsonlite::read_json(
    file.path(output_dir, "audit_summary.json"), simplifyVector = TRUE
  )
  expect_identical(audit_summary$status, "audited_rehearsal_not_promoted")
  expect_false(audit_summary$confirmatory_ready)
  expect_false(audit_summary$response_likelihood)

  method_summary <- utils::read.csv(
    file.path(output_dir, "method_summary.csv"), stringsAsFactors = FALSE
  )
  dkw <- method_summary[method_summary$method_id == "tcsp_dkw", , drop = FALSE]
  expect_equal(nrow(dkw), 1L)
  expect_gt(dkw$failure_rate_all_rows, 0)
  expect_lt(dkw$failure_rate_all_rows, 1)

  manifest <- utils::read.csv(
    file.path(output_dir, "artifact_hashes.csv"), stringsAsFactors = FALSE
  )
  expect_true(all(file.exists(file.path(output_dir, manifest$path))))
  expect_true(any(manifest$path == "README.md"))
})
