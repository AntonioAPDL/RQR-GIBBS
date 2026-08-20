test_that("targeted adaptive TCSP config is scoped to changed cells", {
  config_path <- test_path(
    "..", "..", "config",
    "rqr_bayes_uq_validation_tcsp_adaptive_targeted_20260820.json"
  )
  config <- jsonlite::read_json(config_path, simplifyVector = FALSE)
  expect_identical(config$study_id,
                   "rqr_bayes_uq_validation_tcsp_adaptive_targeted_20260820")
  expect_identical(config$scan_calibration$method,
                   "monte_carlo_cp_adaptive")
  expect_length(config$methods, 1L)
  expect_identical(config$methods[[1L]]$method_id, "tcsp_mc")
  expect_identical(config$methods[[1L]]$scan_method,
                   "monte_carlo_cp_adaptive")

  cells <- config$modes$confirmatory$design_cells
  expect_length(cells, 4L)
  cell_ids <- vapply(cells, `[[`, character(1L), "cell_id")
  expect_setequal(
    cell_ids,
    c("n0500_c090_t095", "n0500_c095_t095",
      "n1000_c090_t095", "n1000_c095_t095")
  )
  expected_rows <- length(config$modes$confirmatory$dgp_ids) *
    length(cells) *
    as.integer(config$modes$confirmatory$replications) *
    length(config$modes$confirmatory$method_ids)
  expect_equal(expected_rows, 28000)
  expect_true(isTRUE(config$modes$confirmatory$paired_thresholds))
  expect_equal(config$base_seed, 963300)
})

test_that("adaptive TCSP adjudication gate compares paired rows", {
  script <- test_path(
    "..", "..", "scripts",
    "80_adjudicate_tcsp_adaptive_targeted_validation.R"
  )
  Sys.setenv(
    RQRGIBBS_TCSP_ADAPTIVE_ADJUDICATION_SOURCE_ONLY = "true",
    RQRGIBBS_TCSP_ADAPTIVE_ADJUDICATION_SCRIPT_PATH = script
  )
  source(script, local = TRUE)
  Sys.unsetenv(c(
    "RQRGIBBS_TCSP_ADAPTIVE_ADJUDICATION_SOURCE_ONLY",
    "RQRGIBBS_TCSP_ADAPTIVE_ADJUDICATION_SCRIPT_PATH"
  ))

  old_dir <- tempfile("old-tcsp-")
  new_dir <- tempfile("new-tcsp-")
  out_dir <- tempfile("tcsp-adjudication-")
  dir.create(old_dir)
  dir.create(new_dir)

  keys <- data.frame(
    mode = "confirmatory",
    dgp_id = rep(c("normal", "lognormal"), each = 2L),
    n = 500L,
    guaranteed_content = 0.9,
    tolerance_confidence = 0.95,
    posterior_confidence = 0.95,
    replication = rep(1:2, 2L),
    seed = c(11L, 12L, 21L, 22L),
    method_id = "tcsp_mc",
    success = TRUE,
    infeasible = FALSE,
    retained_count = 468L,
    stringsAsFactors = FALSE
  )
  old <- keys
  old$scan_critical_method <- "monte_carlo_conservative"
  old$width <- c(10, 12, 9, 11)

  adaptive <- keys
  adaptive$scan_critical_method <- "monte_carlo_cp_adaptive"
  adaptive$retained_count <- 467L
  adaptive$width <- c(9.5, 11.5, 8.5, 10.5)

  write.csv(old, file.path(old_dir, "bayes_uq_validation_results.csv"),
            row.names = FALSE)
  write.csv(adaptive, file.path(new_dir, "bayes_uq_validation_results.csv"),
            row.names = FALSE)
  health <- list(final_artifacts_present = TRUE, rows_remaining = 0L,
                 waves_failed = 0L)
  jsonlite::write_json(health, file.path(old_dir, "health.json"),
                       auto_unbox = TRUE)
  jsonlite::write_json(health, file.path(new_dir, "health.json"),
                       auto_unbox = TRUE)
  jsonlite::write_json(list(), file.path(old_dir, "manifest.json"))
  jsonlite::write_json(list(), file.path(new_dir, "manifest.json"))
  write.csv(data.frame(), file.path(old_dir, "bayes_uq_validation_summary.csv"),
            row.names = FALSE)
  write.csv(data.frame(), file.path(new_dir, "bayes_uq_validation_summary.csv"),
            row.names = FALSE)

  manifest <- tcsp_adaptive_adjudicate(
    old_run_dir = old_dir,
    new_run_dir = new_dir,
    output_dir = out_dir,
    expected_rows = 4L,
    min_delivery = 0.94,
    max_delivery_drop = 0.01,
    min_width_nonincrease_fraction = 0.75
  )

  expect_identical(manifest$gate_status, "promote_adaptive_tcsp")
  expect_equal(manifest$paired_rows, 4L)
  expect_true(file.exists(file.path(out_dir, "adaptive_tcsp_dgp_summary.csv")))
  gates <- utils::read.csv(file.path(out_dir, "adaptive_tcsp_gate_table.csv"),
                           stringsAsFactors = FALSE)
  expect_true(all(gates$pass))
})
