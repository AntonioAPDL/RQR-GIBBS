test_that("V5 corrects only the oracle normalization in the frozen V3 design", {
  scripts <- testthat::test_path("..", "..", "scripts")
  environment <- new.env(parent = globalenv())
  for (file in c(
    "32_oracle_tilt_illustration_utils.R",
    "33_oracle_tilt_forensic_utils.R",
    "34_oracle_tilt_publication_utils.R",
    "42_oracle_tilt_publication_v3_utils.R",
    "58_oracle_tilt_v5_utils.R"
  )) {
    sys.source(file.path(scripts, file), envir = environment)
  }
  config <- environment$oti_read_json(testthat::test_path(
    "..", "..", "config",
    "oracle_tilt_c095_v5_exact_delta_20260810.json"
  ))
  expect_invisible(environment$otv5_validate_config(config))
  preflight <- environment$otv5_design_preflight(config)
  expect_true(preflight$pass)
  expect_identical(preflight$oracle_schema, environment$otv5_oracle_schema())
  expect_identical(
    preflight$tilt_definition, environment$otv5_tilt_definition()
  )

  historical <- utils::read.csv(testthat::test_path(
    "..", "..", "..", "figures", "data", "oracle_tilt_c095_v3",
    "design_contract.csv"
  ), stringsAsFactors = FALSE)
  got_digests <- c(
    fixed_design = environment$otf_object_sha256(preflight$fixed_dgp),
    dlm = environment$otf_object_sha256(preflight$dlm_dgp)
  )
  historical_digests <- stats::setNames(
    historical$dgp_digest, historical$family
  )
  expect_identical(got_digests, historical_digests[names(got_digests)])

  oracle <- preflight$oracle
  expect_identical(oracle$target, c("RQR", "ET", "SH"))
  expect_true(all(oracle$oracle_schema == environment$otv5_oracle_schema()))
  expect_true(all(
    oracle$tilt_definition == environment$otv5_tilt_definition()
  ))
  expect_equal(
    oracle$delta_innovation,
    oracle$conditional_retained_mean_innovation -
      oracle$population_mean_innovation,
    tolerance = 0
  )
  expect_identical(oracle$delta_innovation[oracle$target == "RQR"], 0)

  legacy <- utils::read.csv(testthat::test_path(
    "..", "..", "..", "figures", "data", "oracle_tilt_c095_v3",
    "oracle_targets.csv"
  ), stringsAsFactors = FALSE)
  expect_error(
    environment$otv5_reject_legacy_oracle(legacy), "Legacy oracle evidence"
  )
  expect_invisible(environment$otv5_reject_legacy_oracle(oracle))
})

test_that("V5 ET and SH tilts use conditional rather than truncated means", {
  scripts <- testthat::test_path("..", "..", "scripts")
  environment <- new.env(parent = globalenv())
  sys.source(
    file.path(scripts, "32_oracle_tilt_illustration_utils.R"),
    envir = environment
  )
  sys.source(
    file.path(scripts, "42_oracle_tilt_publication_v3_utils.R"),
    envir = environment
  )
  sys.source(
    file.path(scripts, "58_oracle_tilt_v5_utils.R"),
    envir = environment
  )
  law <- environment$oti_al_law(0.8, standardized = TRUE)
  oracle <- environment$otv5_oracle_targets(
    law, 0.95, c("RQR", "ET", "SH")
  )
  nonzero <- oracle$target %in% c("ET", "SH")
  expect_equal(
    oracle$delta_innovation[nonzero],
    oracle$truncated_first_moment_innovation[nonzero] / 0.95,
    tolerance = 1e-14
  )
  expect_equal(
    oracle$delta_innovation[oracle$target == "ET"],
    0.0560608464325982,
    tolerance = 1e-13
  )
  expect_equal(
    oracle$delta_innovation[oracle$target == "SH"],
    0.11472186306441,
    tolerance = 1e-13
  )
})

test_that("V5 completion retains diagnostic warnings without relabeling them", {
  scripts <- testthat::test_path("..", "..", "scripts")
  environment <- new.env(parent = globalenv())
  for (file in c(
    "32_oracle_tilt_illustration_utils.R",
    "33_oracle_tilt_forensic_utils.R",
    "34_oracle_tilt_publication_utils.R",
    "42_oracle_tilt_publication_v3_utils.R",
    "58_oracle_tilt_v5_utils.R"
  )) {
    sys.source(file.path(scripts, file), envir = environment)
  }
  config <- environment$oti_read_json(testthat::test_path(
    "..", "..", "config",
    "oracle_tilt_c095_v5_exact_delta_20260810.json"
  ))
  expect_identical(config$diagnostics$rhat_max, 1.01)
  expect_identical(config$diagnostics$bulk_ess_min, 1000L)
  expect_identical(config$diagnostics$tail_ess_min, 1000L)
  expect_identical(config$diagnostics$mcse_over_sd_max, 0.05)

  summary <- data.frame(
    provenance_pass = TRUE,
    conditional_parity_pass = TRUE,
    pathology_pass = TRUE,
    strict_diagnostics_pass = FALSE,
    recovery_pass = FALSE,
    heterogeneity_pass = FALSE,
    computational_pass = FALSE,
    numerical_repair_count = 0L,
    endpoint_rmse_over_oracle_width = 0.22,
    mean_width_ratio = 0.96,
    lower_bias_over_oracle_width = -0.08,
    upper_bias_over_oracle_width = 0.07,
    low_scale_endpoint_rmse_over_local_width = 0.28,
    high_scale_endpoint_rmse_over_local_width = 0.31,
    width_contrast_relative_error = 0.24,
    seasonal_width_amplitude_ratio = NA_real_,
    seasonal_width_phase_error = NA_real_,
    stringsAsFactors = FALSE
  )
  cell <- list(
    fit_summary = summary,
    mcmc_diagnostics = data.frame(pass = c(TRUE, FALSE))
  )
  assessed <- environment$otv5_apply_completion_policy(
    cell, "fixed_design", config
  )$fit_summary
  expect_true(assessed$hard_computational_pass)
  expect_true(assessed$completion_eligible)
  expect_true(assessed$broad_recovery_pass)
  expect_true(assessed$broad_heterogeneity_pass)
  expect_true(assessed$manuscript_illustration_evidence_eligible)
  expect_false(assessed$strict_pass)
  expect_identical(assessed$disposition, "diagnostic_aware_pass")
  expect_match(assessed$warning_codes, "mcmc_diagnostic_warning")
  expect_identical(assessed$diagnostic_warning_count, 1L)

  recovery_review <- cell
  recovery_review$fit_summary$endpoint_rmse_over_oracle_width <- 0.41
  assessed_review <- environment$otv5_apply_completion_policy(
    recovery_review, "fixed_design", config
  )$fit_summary
  expect_true(assessed_review$completion_eligible)
  expect_false(assessed_review$manuscript_illustration_evidence_eligible)
  expect_identical(
    assessed_review$disposition, "completed_requires_recovery_review"
  )

  hard_failure <- cell
  hard_failure$fit_summary$provenance_pass <- FALSE
  assessed_failure <- environment$otv5_apply_completion_policy(
    hard_failure, "fixed_design", config
  )$fit_summary
  expect_false(assessed_failure$completion_eligible)
  expect_false(assessed_failure$manuscript_illustration_evidence_eligible)
  expect_identical(assessed_failure$disposition, "hard_failure")
})

test_that("V5 completion policy is frozen and rejects threshold relabeling", {
  scripts <- testthat::test_path("..", "..", "scripts")
  environment <- new.env(parent = globalenv())
  for (file in c(
    "32_oracle_tilt_illustration_utils.R",
    "33_oracle_tilt_forensic_utils.R",
    "34_oracle_tilt_publication_utils.R",
    "42_oracle_tilt_publication_v3_utils.R",
    "58_oracle_tilt_v5_utils.R"
  )) {
    sys.source(file.path(scripts, file), envir = environment)
  }
  config <- environment$oti_read_json(testthat::test_path(
    "..", "..", "config",
    "oracle_tilt_c095_v5_exact_delta_20260810.json"
  ))
  expect_invisible(environment$otv5_validate_config(config))

  altered <- config
  altered$completion_policy$threshold_relabeling_prohibited <- FALSE
  expect_error(
    environment$otv5_validate_config(altered),
    "oracle or interpretation contract changed"
  )
  altered <- config
  altered$completion_policy$broad_illustration_suitability$
    endpoint_rmse_over_oracle_width_max <- 0.41
  expect_error(
    environment$otv5_validate_config(altered),
    "broad illustration-suitability contract changed"
  )
})
