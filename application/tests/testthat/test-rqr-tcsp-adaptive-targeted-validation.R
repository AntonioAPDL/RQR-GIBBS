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

test_that("adaptive TCSP promotion gate treats old-overdelivery loss as diagnostic", {
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

  dgp_summary <- data.frame(
    tolerance_confidence = c(0.95, 0.95),
    adaptive_return_rate = c(1, 1),
    adaptive_delivery_rate = c(0.962, 0.980),
    delivery_rate_delta = c(-0.017, -0.005),
    mean_width_delta = c(-0.05, -0.02),
    stringsAsFactors = FALSE
  )
  paired <- data.frame(pair_status = rep("paired", 2000),
                       stringsAsFactors = FALSE)
  gate <- tcsp_adaptive_gate(
    dgp_summary = dgp_summary,
    paired = paired,
    expected_rows = 2000L,
    min_delivery = NA_real_,
    max_delivery_drop = 0.01,
    min_width_nonincrease_fraction = 0.75
  )

  expect_identical(gate$gate_status, "promote_adaptive_tcsp")
  expect_true(all(gate$gate_table$pass[gate$gate_table$role ==
                                         "promotion_gate"]))
  diagnostic <- gate$gate_table[
    gate$gate_table$gate == "delivery_drop_observed",
    ,
    drop = FALSE
  ]
  expect_equal(as.numeric(diagnostic$value), -0.017)
  expect_identical(diagnostic$role, "diagnostic")
})

test_that("adaptive TCSP article composer replaces only targeted TCSP rows", {
  script <- test_path(
    "..", "..", "scripts",
    "81_compose_adaptive_tcsp_article_inputs.R"
  )
  Sys.setenv(
    RQRGIBBS_TCSP_ARTICLE_COMPOSER_SOURCE_ONLY = "true",
    RQRGIBBS_TCSP_ARTICLE_COMPOSER_SCRIPT_PATH = script
  )
  source(script, local = TRUE)
  Sys.unsetenv(c(
    "RQRGIBBS_TCSP_ARTICLE_COMPOSER_SOURCE_ONLY",
    "RQRGIBBS_TCSP_ARTICLE_COMPOSER_SCRIPT_PATH"
  ))

  baseline_dir <- tempfile("baseline-run-")
  adaptive_dir <- tempfile("adaptive-run-")
  output_dir <- tempfile("article-composite-")
  dir.create(baseline_dir)
  dir.create(adaptive_dir)

  health <- list(final_artifacts_present = TRUE, rows_remaining = 0L,
                 waves_failed = 0L)
  jsonlite::write_json(health, file.path(baseline_dir, "health.json"),
                       auto_unbox = TRUE)
  jsonlite::write_json(health, file.path(adaptive_dir, "health.json"),
                       auto_unbox = TRUE)

  base_rows <- expand.grid(
    dgp_id = c("normal", "lognormal"),
    n = c(500L, 1000L),
    guaranteed_content = c(0.90, 0.99),
    tolerance_confidence = 0.95,
    posterior_confidence = 0.95,
    replication = 1:2,
    method_id = c("tcsp_mc", "young_mathew"),
    stringsAsFactors = FALSE
  )
  base_rows$seed <- seq_len(nrow(base_rows))
  base_rows$scan_critical_method <- ifelse(
    base_rows$method_id == "tcsp_mc",
    "monte_carlo_conservative",
    NA_character_
  )
  base_rows$width <- 10
  base_rows$success <- TRUE
  base_rows$infeasible <- FALSE

  adaptive_rows <- base_rows[
    base_rows$method_id == "tcsp_mc" &
      base_rows$n == 500L &
      abs(base_rows$guaranteed_content - 0.90) < 1e-12,
    ,
    drop = FALSE
  ]
  adaptive_rows$scan_critical_method <- "monte_carlo_cp_adaptive"
  adaptive_rows$width <- 9

  base_scan <- data.frame(
    method = "monte_carlo_conservative",
    n = c(500L, 1000L),
    guaranteed_content = c(0.90, 0.99),
    tolerance_confidence = 0.95,
    retained_count = c(468L, 998L),
    content_buffer = c(0.036, 0.008),
    certified_lower_probability = c(0.951, 0.970),
    stringsAsFactors = FALSE
  )
  adaptive_scan <- base_scan[1, , drop = FALSE]
  adaptive_scan$method <- "monte_carlo_cp_adaptive"
  adaptive_scan$retained_count <- 467L
  adaptive_scan$content_buffer <- 0.034

  write.csv(base_rows,
            file.path(baseline_dir, "bayes_uq_validation_results.csv"),
            row.names = FALSE)
  write.csv(adaptive_rows,
            file.path(adaptive_dir, "bayes_uq_validation_results.csv"),
            row.names = FALSE)
  write.csv(base_scan, file.path(baseline_dir, "scan_calibration_summary.csv"),
            row.names = FALSE)
  write.csv(adaptive_scan,
            file.path(adaptive_dir, "scan_calibration_summary.csv"),
            row.names = FALSE)

  manifest <- compose_adaptive_tcsp_article_inputs(
    baseline_run_dir = baseline_dir,
    adaptive_run_dir = adaptive_dir,
    output_dir = output_dir
  )
  out <- read.csv(file.path(output_dir, "bayes_uq_validation_results.csv"),
                  stringsAsFactors = FALSE, check.names = FALSE)
  scan <- read.csv(file.path(output_dir, "scan_calibration_summary.csv"),
                   stringsAsFactors = FALSE, check.names = FALSE)

  expect_equal(nrow(out), nrow(base_rows))
  expect_equal(manifest$tcsp_rows_replaced, nrow(adaptive_rows))
  expect_true(all(out$width[out$method_id == "young_mathew"] == 10))
  replaced <- out[out$method_id == "tcsp_mc" & out$n == 500L &
                    abs(out$guaranteed_content - 0.90) < 1e-12, ,
                  drop = FALSE]
  expect_true(all(replaced$scan_critical_method ==
                    "monte_carlo_cp_adaptive"))
  expect_true(all(replaced$width == 9))
  unchanged <- out[out$method_id == "tcsp_mc" & out$n == 1000L, ,
                   drop = FALSE]
  expect_true(all(unchanged$scan_critical_method ==
                    "monte_carlo_conservative"))
  expect_equal(scan$retained_count[scan$n == 500L], 467L)
  expect_equal(scan$retained_count[scan$n == 1000L], 998L)
})
