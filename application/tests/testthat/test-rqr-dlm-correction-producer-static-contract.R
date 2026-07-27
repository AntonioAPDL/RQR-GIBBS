producer_script <- function(name) {
  testthat::test_path("..", "..", "scripts", name)
}

producer_lines <- function(name) {
  readLines(producer_script(name), warn = FALSE)
}

test_that("the M01 producer freezes complete role-specific transition contracts", {
  path <- producer_script("22_validate_rqr_dlm_wave1_corrections.R")
  expect_silent(parse(file = path))
  script <- paste(producer_lines(basename(path)), collapse = "\n")

  for (token in c(
      'wave1 = "static_gaussian_T200__target0200__sentinel"',
      'wave2 = "local_level_gaussian_T200__target0200__sentinel"',
      'wave_tag, wave1 = "regression", wave2 = "level"',
      "expected_transition_kernel <- list(",
      "rqrgibbs_dlm_transition_kernel/1.0.0",
      "collapsed_log_q_coordinate_order = expected_component_name",
      "rqrgibbs_dlm_transition_kernel_invariant/1.0.0",
      "transition_kernel_contract_match",
      "transition_kernel_contract_digests",
      "transition_kernel_contract_matches",
      "all_fit_transition_contracts_complete",
      "all_fit_transition_contracts_match_expected",
      "rqrgibbs_dlm_wave_correction_validation/2.2.0"
    )) {
    expect_match(script, token, fixed = TRUE)
  }
  expect_match(
    script,
    "setdiff(",
    fixed = TRUE
  )
})

test_that("the M02 correction producer has a closed wave contract", {
  path <- producer_script(
    "23_validate_rqr_dlm_wave1_comparator_projection.R"
  )
  expect_silent(parse(file = path))
  script <- paste(producer_lines(basename(path)), collapse = "\n")

  expect_match(
    script,
    'wave1 = "static_gaussian_T200__target0200__sentinel"',
    fixed = TRUE
  )
  expect_match(
    script,
    'wave2 = "local_level_gaussian_T200__target0200__sentinel"',
    fixed = TRUE
  )
  expect_match(script, "!wave_id %in% unname(allowed_waves)", fixed = TRUE)
  expect_match(
    script,
    "Each frozen M02 wave must contain standard and sentinel schedule roles.",
    fixed = TRUE
  )
  expect_match(
    script,
    "rqrgibbs_dlm_wave_comparator_projection_validation/2.1.0",
    fixed = TRUE
  )
  expect_false(grepl(
    ')) "wave1" else "wave2"',
    script,
    fixed = TRUE
  ))
})

test_that("the M02 producer binds both frozen schedules to realized draws", {
  script <- paste(producer_lines(
    "23_validate_rqr_dlm_wave1_comparator_projection.R"
  ), collapse = "\n")

  for (token in c(
      "dynamic_quantile_endpoint_standard",
      "dynamic_quantile_endpoint_sentinel",
      "applied_schedule = value$diagnostics$schedule",
      "schedule_applied = value$diagnostics$schedule_applied",
      "state_draw_dimensions",
      "scale_draw_lengths",
      "training_horizon = as.integer(generated$T)",
      "applied_schedule_evidence = schedule_evidence",
      "all_applied_schedules_verified = schedule_contract_pass",
      "configured_burn = as.integer(configured_schedule$burn)",
      "configured_retain = as.integer(configured_schedule$retain)",
      "configured_thin = as.integer(configured_schedule$thin)",
      "applied_burn = as.integer(applied_schedule$burn)",
      "applied_retain = as.integer(applied_schedule$retain)",
      "applied_thin = as.integer(applied_schedule$thin)",
      "realized_state_draw_dimensions",
      "realized_scale_draw_lengths",
      "schedule_contract_pass = all(vapply("
    )) {
    expect_match(script, token, fixed = TRUE)
  }
  expect_match(
    script,
    "dimensions[[2L]], result$training_horizon",
    fixed = TRUE
  )
  expect_match(
    script,
    "dimensions[[3L]], as.integer(expected$retain)",
    fixed = TRUE
  )
  expect_match(
    script,
    "result$state_draw_dimensions[[1L]],",
    fixed = TRUE
  )
  expect_match(
    script,
    "result$state_draw_dimensions[[2L]]",
    fixed = TRUE
  )
})

test_that("the resource producer binds source runtime and frozen inputs", {
  path <- producer_script("25_validate_rqr_dlm_resource_envelope.R")
  expect_silent(parse(file = path))
  script <- paste(producer_lines(basename(path)), collapse = "\n")

  for (token in c(
      "rqrgibbs_dlm_resource_envelope_validation/2.0.0",
      "RQR_EXPECTED_PRIMARY_COMMIT",
      "RQR_PRIMARY_RUNTIME_ATTESTATION",
      "exact_commit_attestation_pair_verified = exact_runtime_pair",
      "primary_runtime_source_match",
      "primary_reproducibility_eligible",
      "promotion_evidence_eligible = promotion_evidence_eligible",
      "development_execution =",
      'rqr_confirm_seed_ledger(contract, planning = "maximum")',
      "config_digest = config_digest",
      "incidence_digest = incidence_digest",
      "maximum_seed_ledger_digest = seed_ledger_digest",
      "toolchain_manifest_digest = toolchain_digest",
      '"R_version", "platform", "R_compiler", "BLAS", "LAPACK"',
      "!anyDuplicated(toolchain$key)"
    )) {
    expect_match(script, token, fixed = TRUE)
  }
  expect_match(
    script,
    "!nzchar(expected_commit)",
    fixed = TRUE
  )
})

test_that("the resource producer uses package-true retained-draw orientations", {
  script <- paste(producer_lines(
    "25_validate_rqr_dlm_resource_envelope.R"
  ), collapse = "\n")

  for (token in c(
      '"p x T x retained"',
      '"T x retained"',
      '"p x retained"',
      '"retained x components"',
      "array(0, c(p, T, draws))",
      "matrix(0, T, draws)",
      "matrix(0, p, draws)",
      "matrix(1, draws, components)",
      "samp.theta_terminal_root1",
      "samp.theta0_root1",
      "samp.evolution_scale_shape",
      "samp.evolution_scale_rate"
    )) {
    expect_match(script, token, fixed = TRUE)
  }
  expect_false(grepl(
    "array(0, c(draws, p, T))",
    script,
    fixed = TRUE
  ))
  expect_false(grepl(
    "matrix(0, draws, T)",
    script,
    fixed = TRUE
  ))
})

test_that("the embedded resource writer and reader script parses", {
  expressions <- parse(
    file = producer_script("25_validate_rqr_dlm_resource_envelope.R")
  )
  write_index <- which(vapply(
    expressions,
    function(expression) {
      is.call(expression) &&
        identical(expression[[1L]], as.name("writeLines"))
    },
    logical(1L)
  ))
  expect_length(write_index, 1L)
  payload <- eval(expressions[[write_index]][[2L]], envir = baseenv())
  expect_type(payload, "character")
  expect_silent(parse(text = paste(payload, collapse = "\n")))
})

test_that("the resource producer measures fresh writer and reader processes", {
  script <- paste(producer_lines(
    "25_validate_rqr_dlm_resource_envelope.R"
  ), collapse = "\n")

  for (token in c(
      'tmpdir = tempdir(), fileext = ".R"',
      'run_child(\n    "write"',
      'run_child(\n    "read"',
      "writer_pid",
      "clean_reader_pid",
      "clean_process_pair_valid",
      "distinct_clean_processes_verified",
      "writer_peak_RSS_KiB",
      "clean_reader_peak_RSS_KiB",
      "telemetry_complete",
      "all(is.finite(telemetry))",
      "all(telemetry > 0)",
      "maximum_writer_or_clean_reader_peak_RSS_KiB"
    )) {
    expect_match(script, token, fixed = TRUE)
  }
  expect_false(grepl("max(telemetry, na.rm = TRUE)", script, fixed = TRUE))
  expect_match(
    script,
    "sha256_after_reader <- rqr_confirm_sha256(path)",
    fixed = TRUE
  )
})

test_that("the resource producer closes over six compact non-RDS files", {
  script <- paste(producer_lines(
    "25_validate_rqr_dlm_resource_envelope.R"
  ), collapse = "\n")
  expected <- c(
    "fit_shape_contract.csv",
    "resource_closeout.csv",
    "resource_envelope.csv",
    "toolchain_manifest.csv",
    "validation_manifest.json",
    "artifact_hashes.csv"
  )
  for (filename in expected) {
    expect_match(script, filename, fixed = TRUE)
  }
  expect_match(
    script,
    "The resource gate did not publish exactly six compact files.",
    fixed = TRUE
  )
  expect_match(
    script,
    'any(grepl("\\\\.rds$", observed_pre_manifest_files',
    fixed = TRUE
  )
  expect_match(
    script,
    "unlink(path, force = TRUE)",
    fixed = TRUE
  )
  expect_match(
    script,
    "rqrgibbs_dlm_resource_envelope_closeout/1.0.0",
    fixed = TRUE
  )
  closeout_start <- grep(
    "^resource_closeout <- data.frame\\(",
    producer_lines("25_validate_rqr_dlm_resource_envelope.R")
  )
  closeout_end <- grep(
    "^rqr_confirm_atomic_write_csv\\($",
    producer_lines("25_validate_rqr_dlm_resource_envelope.R")
  )
  closeout_end <- closeout_end[closeout_end > closeout_start][[1L]]
  closeout <- paste(
    producer_lines("25_validate_rqr_dlm_resource_envelope.R")[
      closeout_start:(closeout_end - 1L)
    ],
    collapse = "\n"
  )
  expected_columns <- c(
    "schema_version", "source_commit", "package_version",
    "primary_runtime_tree_digest",
    "primary_runtime_attestation_sha256", "config_digest",
    "incidence_digest", "maximum_seed_ledger_digest",
    "toolchain_manifest_digest",
    "exact_commit_attestation_pair_verified",
    "promotion_evidence_eligible", "telemetry_complete",
    "resource_margin_pass", "all_writer_shapes_valid",
    "all_clean_deserializations_valid",
    "maximum_writer_or_clean_reader_peak_RSS_KiB", "status"
  )
  positions <- vapply(
    expected_columns,
    function(column) {
      match <- regexpr(
        paste0("(^|\\n)[[:space:]]*", column, "[[:space:]]*="),
        closeout, perl = TRUE
      )
      as.integer(match[[1L]])
    },
    integer(1L)
  )
  expect_true(all(positions > 0L))
  expect_true(all(diff(positions) > 0L))
  assignment_lines <- grep(
    "^[[:space:]]{2}[a-zA-Z][a-zA-Z0-9_.]*[[:space:]]*=",
    strsplit(closeout, "\n", fixed = TRUE)[[1L]],
    value = TRUE
  )
  observed_columns <- sub(
    "^[[:space:]]*([a-zA-Z][a-zA-Z0-9_.]*)[[:space:]]*=.*$",
    "\\1", assignment_lines
  )
  observed_columns <- setdiff(observed_columns, "stringsAsFactors")
  expect_identical(observed_columns, expected_columns)
})
