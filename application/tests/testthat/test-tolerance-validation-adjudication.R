source_adjudication_env <- function() {
  old <- Sys.getenv("RQRGIBBS_ADJUDICATION_SOURCE_ONLY", unset = NA)
  old_path <- Sys.getenv("RQRGIBBS_ADJUDICATION_SCRIPT_PATH", unset = NA)
  script <- normalizePath(
    testthat::test_path(
      "..", "..", "scripts", "76_adjudicate_tolerance_validation_results.R"
    ),
    winslash = "/",
    mustWork = TRUE
  )
  Sys.setenv(RQRGIBBS_ADJUDICATION_SOURCE_ONLY = "true")
  Sys.setenv(RQRGIBBS_ADJUDICATION_SCRIPT_PATH = script)
  on.exit({
    if (is.na(old)) {
      Sys.unsetenv("RQRGIBBS_ADJUDICATION_SOURCE_ONLY")
    } else {
      Sys.setenv(RQRGIBBS_ADJUDICATION_SOURCE_ONLY = old)
    }
    if (is.na(old_path)) {
      Sys.unsetenv("RQRGIBBS_ADJUDICATION_SCRIPT_PATH")
    } else {
      Sys.setenv(RQRGIBBS_ADJUDICATION_SCRIPT_PATH = old_path)
    }
  }, add = TRUE)
  env <- new.env(parent = globalenv())
  sys.source(script, envir = env)
  env
}

minimal_adjudication_rows <- function(run_label = "synthetic") {
  data.frame(
    run_label = run_label,
    run_component = "primary",
    mode = "unit",
    dgp_id = "normal",
    n = c(50L, 500L, 500L, 500L, 500L, 1000L),
    guaranteed_content = c(0.99, 0.99, 0.99, 0.99, 0.90, 0.99),
    tolerance_confidence = 0.95,
    posterior_confidence = 0.95,
    replication = 1L,
    seed = seq_len(6L),
    method_id = c(
      "wilks_minmax", "wilks_minmax", "tcsp_dkw",
      "tcsp_mti_ecm_map_mc", "tcsp_mti_gibbs_median_mc", "young_mathew"
    ),
    infeasible = c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE),
    success = c(TRUE, TRUE, NA, NA, TRUE, TRUE),
    lower = -1,
    upper = 1,
    width = c(2, 2, NA, NA, 2.2, 1.9),
    scan_certified_lower_probability = c(NA, NA, NA, NA, 0.96, NA),
    fit_class = c(
      "wilks_minmax_comparator", "wilks_minmax_comparator",
      "rqr_tcsp_dkw_calibration_infeasible",
      "rqr_tcsp_mti_calibration_infeasible",
      "mti_mcmc|rqr_mcmc", "tolerance_nptol_int|young_mathew"
    ),
    message = "",
    posterior_draws = c(NA, NA, NA, NA, 50L, NA),
    mcmc_n_burn = c(NA, NA, NA, NA, 25L, NA),
    mcmc_n_mcmc = c(NA, NA, NA, NA, 50L, NA),
    mcmc_thin = c(NA, NA, NA, NA, 1L, NA),
    ecm_converged = c(NA, NA, NA, TRUE, NA, NA),
    ecm_iterations = c(NA, NA, NA, 200L, NA, NA),
    ecm_relative_objective_drop = c(NA, NA, NA, 0.2, NA, NA),
    ecm_final_stationarity = c(NA, NA, NA, 5e-4, NA, NA),
    target_audit_digest = c("", "", "", "ecm-digest", "gibbs-digest",
                            "ym-digest"),
    elapsed_sec = c(0.001, 0.001, 0.001, 0.5, 0.8, 0.03),
    stringsAsFactors = FALSE
  )
}

write_synthetic_run <- function(path, rows) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(
    list(
      schema_version = "unit/wave_health",
      final_artifacts_present = TRUE,
      rows_remaining = 0,
      waves_failed = 0,
      rows_completed = nrow(rows),
      rows_expected = nrow(rows)
    ),
    file.path(path, "health.json"),
    pretty = TRUE,
    auto_unbox = TRUE
  )
  jsonlite::write_json(
    list(schema_version = "unit/manifest", n_result_rows = nrow(rows)),
    file.path(path, "manifest.json"),
    pretty = TRUE,
    auto_unbox = TRUE
  )
  jsonlite::write_json(
    list(schema_version = "unit/config"),
    file.path(path, "config_frozen.json"),
    pretty = TRUE,
    auto_unbox = TRUE
  )
  utils::write.csv(rows, file.path(path, "bayes_uq_validation_results.csv"),
                   row.names = FALSE)
  utils::write.csv(rows[0, ], file.path(path, "bayes_uq_validation_summary.csv"),
                   row.names = FALSE)
  utils::write.csv(data.frame(wave_id = "unit"),
                   file.path(path, "wave_plan.csv"), row.names = FALSE)
  utils::write.csv(data.frame(wave_id = "unit", status = "complete"),
                   file.path(path, "wave_status.csv"), row.names = FALSE)
  utils::write.csv(data.frame(method_id = "tcsp_mc"),
                   file.path(path, "scan_calibration_summary.csv"),
                   row.names = FALSE)
}

test_that("Wilks full-range certificate matches threshold cells", {
  env <- source_adjudication_env()
  expect_true(env$tva_full_range_certificate(46, 0.90) >= 0.95)
  expect_true(env$tva_full_range_certificate(50, 0.99) < 0.95)
  expect_true(env$tva_full_range_certificate(473, 0.99) >= 0.95)

  audit <- env$tva_wilks_certificate_frame(
    n = c(46L, 50L, 473L),
    content = c(0.90, 0.99, 0.99),
    confidence = c(0.95, 0.95, 0.95)
  )
  expect_equal(
    audit$certifies_requested_statement,
    c(TRUE, FALSE, TRUE)
  )
  expect_true(all(audit$certificate_status == "exact_beta_full_range"))
})

test_that("adjudication taxonomy separates certification and computation", {
  env <- source_adjudication_env()
  rows <- env$tva_classify_rows(minimal_adjudication_rows())

  wilks_bad <- rows[rows$method_id == "wilks_minmax" &
                      rows$n == 50L, , drop = FALSE]
  wilks_good <- rows[rows$method_id == "wilks_minmax" &
                       rows$n == 500L, , drop = FALSE]
  expect_false(wilks_bad$certifies_requested_statement)
  expect_equal(wilks_bad$failure_taxonomy, "returned_uncertified")
  expect_true(wilks_good$certifies_requested_statement)

  expect_equal(
    rows$failure_taxonomy[rows$method_id == "tcsp_dkw"],
    "conservative_bound_infeasible"
  )
  expect_equal(
    rows$failure_taxonomy[rows$method_id == "tcsp_mti_ecm_map_mc"],
    "fixed_target_mti_infeasible"
  )
  expect_true(rows$mti_gibbs_screening_budget[
    rows$method_id == "tcsp_mti_gibbs_median_mc"
  ])
  expect_true(rows$ecm_stationarity_pass_1e3[
    rows$method_id == "tcsp_mti_ecm_map_mc"
  ])
})

test_that("Young-Mathew contract audit records package-call semantics", {
  env <- source_adjudication_env()
  rows <- env$tva_classify_rows(minimal_adjudication_rows())
  audit <- env$tva_young_mathew_contract_audit(rows)

  expect_equal(nrow(audit), 1L)
  expect_equal(audit$function_name, "nptol.int")
  expect_equal(audit$method, "YM")
  expect_equal(audit$side, 2L)
  expect_equal(audit$alpha, 0.05)
  expect_equal(audit$P, 0.99)
  expect_equal(audit$endpoint_order_pass_rate, 1)
  expect_equal(audit$target_audit_digest_present_rate, 1)
})

test_that("MTI diagnostic audits detect ECM stability and Gibbs screening budget", {
  env <- source_adjudication_env()
  rows <- env$tva_classify_rows(minimal_adjudication_rows())
  ecm <- env$tva_mti_ecm_stationarity_audit(rows)
  gibbs <- env$tva_mti_gibbs_budget_audit(rows)

  expect_equal(ecm$stationarity_pass_rate_1e3, 1)
  expect_false(ecm$hard_cell_needs_sensitivity)
  expect_equal(gibbs$mean_posterior_draws, 50)
  expect_true(gibbs$needs_targeted_gibbs_diagnostic)
})

test_that("adjudication runner writes reproducible synthetic bundle", {
  env <- source_adjudication_env()
  root <- tempfile("tva-runs-")
  output <- tempfile("tva-output-")
  on.exit(unlink(c(root, output), recursive = TRUE, force = TRUE), add = TRUE)

  rows <- minimal_adjudication_rows()
  write_synthetic_run(file.path(root, "main"), rows)
  write_synthetic_run(file.path(root, "ecm"), rows)
  write_synthetic_run(file.path(root, "small95"), rows)
  write_synthetic_run(file.path(root, "paper90"), rows)

  out <- env$tva_run_adjudication(
    main_run_dir = file.path(root, "main"),
    ecm_run_dir = file.path(root, "ecm"),
    small95_run_dir = file.path(root, "small95"),
    paper90_run_dir = file.path(root, "paper90"),
    output_dir = output,
    require_clean = FALSE,
    source_commit = "unit-test"
  )
  expect_true(dir.exists(out))
  expect_true(file.exists(file.path(out, "adjudication_manifest.json")))
  expect_true(file.exists(file.path(out, "wilks_certificate_audit.csv")))
  expect_true(file.exists(file.path(out, "method_failure_taxonomy.csv")))
  manifest <- jsonlite::read_json(
    file.path(out, "adjudication_manifest.json"), simplifyVector = TRUE
  )
  expect_true(manifest$gates$wilks_certificates_present)
  expect_true(manifest$gates$mti_gibbs_screening_budget_detected)
})
