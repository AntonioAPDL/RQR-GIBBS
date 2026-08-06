test_that("multicomponent recovery remains fixed and fail closed", {
  application_root <- normalizePath(file.path(test_path(), "..", ".."))
  repo_root <- normalizePath(file.path(application_root, ".."))
  runner_path <- file.path(
    application_root, "scripts",
    "55_compare_rqr_dlm_multicomponent_scale_candidates.R"
  )
  coordinator_path <- file.path(
    application_root, "scripts",
    "56_orchestrate_rqr_dlm_multicomponent_recovery.sh"
  )
  health_path <- file.path(
    application_root, "scripts",
    "57_healthcheck_rqr_dlm_multicomponent_recovery.R"
  )
  plan_path <- file.path(
    repo_root, "docs", "implementation_notes",
    "rqr_dlm_m11_multicomponent_recovery_plan_20260806.md"
  )
  expect_true(all(file.exists(c(
    runner_path, coordinator_path, health_path, plan_path
  ))))
  runner <- paste(readLines(runner_path, warn = FALSE), collapse = "\n")
  coordinator <- paste(
    readLines(coordinator_path, warn = FALSE), collapse = "\n"
  )
  health <- paste(readLines(health_path, warn = FALSE), collapse = "\n")
  expect_match(runner, "baseline_joint1_coordinate", fixed = TRUE)
  expect_match(runner, "directional1_joint1", fixed = TRUE)
  expect_match(runner, "joint2_coordinate", fixed = TRUE)
  expect_match(runner, "directional1_joint2", fixed = TRUE)
  expect_match(runner, "c(166L, 167L, 77L)", fixed = TRUE)
  expect_match(runner, "nrow(jobs) != 48L", fixed = TRUE)
  expect_match(
    runner,
    "first_all_case_eligible_candidate_by_predeclared_order",
    fixed = TRUE
  )
  expect_match(runner, "confirmatory_launch_authorized = FALSE", fixed = TRUE)
  expect_match(runner, "scientific_metrics_used = FALSE", fixed = TRUE)
  expect_match(coordinator, "trap record_terminal_status EXIT", fixed = TRUE)
  expect_match(coordinator, "trap 'exit 143' TERM", fixed = TRUE)
  expect_match(coordinator, "mktemp", fixed = TRUE)
  expect_false(grepl("pgrep", health, fixed = TRUE))
  expect_match(health, "ps -eo pid=,pgid=,stat=", fixed = TRUE)
  expect_match(health, "main_launch_authorized", fixed = TRUE)
})

test_that("failed S10 guard is hash-bound in the frozen contract", {
  environment <- new.env(parent = asNamespace("rqrgibbs"))
  source(
    file.path(
      normalizePath(file.path(test_path(), "..", "..")),
      "scripts", "lib", "rqr_dlm_confirmatory_simulation.R"
    ),
    local = environment
  )
  contract <- environment$rqr_confirm_read_contract(
    normalizePath(file.path(test_path(), "..", "..", ".."))
  )
  expect_invisible(environment$rqr_confirm_validate_contract(
    contract, require_closed = TRUE
  ))
  correction <- contract$config$implementation_correction
  expect_identical(correction$higher_dimensional_guard_jobs, 8L)
  expect_identical(correction$higher_dimensional_guard_diagnostics, 95L)
  expect_identical(
    correction$higher_dimensional_guard_failed_diagnostics, 16L
  )
  expect_false(correction$higher_dimensional_guard_outputs_reused)
  expect_false(
    correction$higher_dimensional_guard_scientific_metrics_used
  )
})

test_that("workflow transition telemetry de-duplicates directional cycles", {
  environment <- new.env(parent = asNamespace("rqrgibbs"))
  source(
    file.path(
      normalizePath(file.path(test_path(), "..", "..")),
      "scripts", "lib", "rqr_dlm_confirmatory_simulation.R"
    ),
    local = environment
  )
  fit <- list(
    diagnostics = list(
      component_scale_interweave = data.frame(
        iteration = rep(1:2, each = 2L), cycle = 1L,
        component = rep(c("trend", "regression"), 2L),
        evaluations = c(4L, 5L, 6L, 7L),
        shrink_steps = c(0L, 1L, 1L, 0L),
        directional_sweeps = 1L,
        directional_evaluations = rep(c(8L, 9L), each = 2L),
        directional_shrink_steps = rep(c(1L, 2L), each = 2L),
        directional_max_distance = rep(c(0.3, 0.7), each = 2L),
        exact_random_direction_slice = TRUE
      ),
      component_scale_joint_elliptical = data.frame()
    ),
    provenance = list(object_digests = list(
      target = strrep("a", 64L), model = strrep("b", 64L),
      evolution = strrep("c", 64L)
    )),
    model_spec = list(component_scale_transition_kernel = list(
      directional = TRUE, sweeps = 1L, target_change = FALSE
    ))
  )
  class(fit) <- c("rqr_dlm_mcmc", "rqrgibbs_fit")
  telemetry <- environment$rqr_confirm_transition_telemetry(fit)
  expect_identical(
    telemetry$schema_version,
    "rqrgibbs_dlm_transition_telemetry/1.0.0"
  )
  expect_identical(telemetry$coordinate_evaluations, 22L)
  expect_identical(telemetry$directional_updates, 2L)
  expect_identical(telemetry$directional_evaluations, 17L)
  expect_equal(telemetry$directional_max_distance, 0.7)
  expect_true(telemetry$all_directional_updates_exact)
  expect_identical(telemetry$target_digest, strrep("a", 64L))
})
