test_that("diagnostic-aware completion is explicit and separate", {
  repo_root <- testthat::test_path("..", "..", "..")
  environment <- new.env(parent = globalenv())
  sys.source(
    file.path(
      repo_root, "application", "scripts", "lib",
      "rqr_dlm_confirmatory_simulation.R"
    ),
    envir = environment
  )
  sys.source(
    file.path(
      repo_root, "application", "scripts", "lib",
      "rqr_dlm_diagnostic_aware_completion.R"
    ),
    envir = environment
  )
  contract <- environment$rqr_confirm_read_contract(repo_root)
  expect_silent(environment$rqr_confirm_validate_contract(contract))
  expect_false(contract$config$confirmatory_execution_authorized)
  record <- environment$rqr_completion_read_policy(repo_root)
  expect_true(record$policy$execution_authorized)
  expect_identical(
    record$policy$interpretation,
    "diagnostic_aware_not_convergence_validated"
  )
  expect_false(record$policy$diagnostic_thresholds_changed)
  expect_true(record$policy$complete_maximum_design)
  expect_true(record$policy$precision_stopping_disabled)
  expect_false(record$policy$result_contract$scientific_promotion)
  applied <- environment$rqr_completion_apply_policy(
    contract, record$policy
  )
  expect_identical(
    applied$mcmc_control_overrides$M11,
    list(
      component_scale_directional_interweave = TRUE,
      component_scale_directional_sweeps = 1L
    )
  )
})

test_that("diagnostic-aware decisions always reach the frozen maximum", {
  repo_root <- testthat::test_path("..", "..", "..")
  environment <- new.env(parent = globalenv())
  sys.source(
    file.path(
      repo_root, "application", "scripts", "lib",
      "rqr_dlm_confirmatory_simulation.R"
    ),
    envir = environment
  )
  sys.source(
    file.path(
      repo_root, "application", "scripts", "lib",
      "rqr_dlm_diagnostic_aware_completion.R"
    ),
    envir = environment
  )
  contract <- environment$rqr_confirm_read_contract(repo_root)
  decisions <- data.frame(
    batch_group = c("core", "sensitivity", "core-max"),
    DGP = c("S01", "S15", "S02"),
    replication_rule = c("C", "S", "C"),
    replications = c(200L, 100L, 600L),
    precision_pass = TRUE,
    cell_mean_precision_pass = TRUE,
    paired_contrast_precision_pass = TRUE,
    no_fit_failures = TRUE,
    paired_batch_complete = TRUE,
    performance_sign_used = FALSE,
    TOST_used = FALSE,
    next_action = "precision_pass_stop",
    next_replications = c(200L, 100L, 600L),
    stringsAsFactors = FALSE
  )
  observed <- environment$rqr_completion_force_maximum_decisions(
    decisions, contract
  )
  expect_identical(
    observed$next_action,
    c(
      "add_complete_paired_DGP_batch",
      "add_complete_paired_DGP_batch",
      "precision_pass_stop"
    )
  )
  expect_identical(observed$next_replications, c(300L, 150L, 600L))
  expect_true(all(observed$precision_pass))
})

test_that("runner records diagnostics without hiding their failures", {
  repo_root <- testthat::test_path("..", "..", "..")
  runner <- paste(readLines(file.path(
    repo_root, "application", "scripts",
    "15_run_rqr_dlm_confirmatory_simulation.R"
  ), warn = FALSE), collapse = "\n")
  launcher <- paste(readLines(file.path(
    repo_root, "application", "scripts",
    "17_launch_rqr_dlm_confirmatory_wave.R"
  ), warn = FALSE), collapse = "\n")
  expect_match(runner, "completed_with_diagnostic_warning", fixed = TRUE)
  expect_match(runner, "diagnostic_warning_summary.csv", fixed = TRUE)
  expect_match(runner, "diagnostic_warning_sensitivity.csv", fixed = TRUE)
  expect_match(runner, "rqr_completion_force_maximum_decisions", fixed = TRUE)
  expect_match(launcher, "diagnostic_failures_nonblocking", fixed = TRUE)
  expect_match(runner, "mcmc_diagnostic_construction_failure", fixed = TRUE)
  expect_match(runner, "failed_global_stop", fixed = TRUE)
})

test_that("wave state cryptographically binds the completion policy", {
  repo_root <- testthat::test_path("..", "..", "..")
  environment <- new.env(parent = globalenv())
  sys.source(
    file.path(
      repo_root, "application", "scripts", "lib",
      "rqr_dlm_confirmatory_simulation.R"
    ),
    envir = environment
  )
  digest64 <- paste(rep("a", 64L), collapse = "")
  commit40 <- paste(rep("b", 40L), collapse = "")
  output_root <- tempfile("diagnostic-aware-wave-root-")
  dir.create(output_root)
  on.exit(unlink(output_root, recursive = TRUE, force = TRUE), add = TRUE)
  authorization <- list(
    authorization_commit = commit40,
    reviewed_implementation_commit = commit40,
    primary_runtime_tree_digest = digest64,
    task_plan_sha256 = digest64,
    seed_ledger_sha256 = digest64,
    execution_policy_sha256 = digest64
  )
  binding <- environment$rqr_confirm_wave_binding(
    run_id = "diagnostic-aware-test",
    expected_commit = commit40,
    authorization = authorization,
    config_sha256 = digest64,
    incidence_sha256 = digest64,
    seed_ledger_sha256 = digest64,
    task_plan_sha256 = digest64,
    wave_plan_sha256 = digest64,
    wave_output_base = output_root
  )
  expect_identical(
    binding$schema_version, "rqrgibbs_dlm_wave_run/1.2.0"
  )
  expect_identical(binding$execution_policy_sha256, digest64)
  expect_match(binding$binding_digest, "^[0-9a-f]{64}$")
  expect_silent(environment$rqr_confirm_wave_output_root(
    binding,
    data.frame(
      canonical_wave_index = 1L, wave_id = "wave-one",
      stringsAsFactors = FALSE
    )
  ))
})
