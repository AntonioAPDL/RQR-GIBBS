validation_environment <- function() {
  environment <- new.env(parent = globalenv())
  sys.source(
    testthat::test_path(
      "..", "..", "scripts", "60_oracle_mean_tilt_validation_utils.R"
    ),
    envir = environment
  )
  environment
}

validation_config <- function(environment) {
  environment$omtv_read_config(testthat::test_path(
    "..", "..", "config", "oracle_mean_tilt_validation_v1.json"
  ))
}

test_that("validation preflight freezes a complete target-aligned design", {
  environment <- validation_environment()
  config <- validation_config(environment)
  expect_invisible(environment$omtv_validate_config(config))
  preflight <- environment$omtv_preflight(config)
  expect_true(preflight$pass)
  expect_equal(nrow(preflight$oracle), 18L)
  expect_equal(nrow(preflight$incidence), 36L)
  expect_equal(nrow(preflight$projection_audit), 72L)
  expect_true(all(preflight$incidence$included))
  expect_true(all(preflight$oracle$uses_cornish_fisher == FALSE))
  expect_true(all(
    preflight$oracle$tilt_definition ==
      "conditional_retained_mean_minus_population_mean"
  ))
  expect_lt(max(preflight$projection_audit$maximum_absolute_residual), 1e-10)
  expect_gte(min(preflight$tail_information$expected_rare_tail_count), 10)
  expect_gte(
    min(preflight$tail_information$expected_rare_tail_count_per_stratum), 2
  )
})

test_that("symmetric and reflected oracle cells satisfy exact identities", {
  environment <- validation_environment()
  preflight <- environment$omtv_preflight(
    validation_config(environment)
  )
  symmetric <- preflight$oracle[
    preflight$oracle$scenario_id == "S01_symmetric_control", , drop = FALSE
  ]
  expect_identical(symmetric$mean_tilt, rep(0, 3L))
  expect_equal(diff(range(symmetric$lower_root)), 0, tolerance = 1e-7)
  expect_equal(diff(range(symmetric$upper_root)), 0, tolerance = 1e-7)
  for (target in c("RQR", "ET", "SH")) {
    left <- preflight$oracle[
      preflight$oracle$scenario_id == "S02_reflected_skew" &
        preflight$oracle$target == target, , drop = FALSE
    ]
    right <- preflight$oracle[
      preflight$oracle$scenario_id == "S03_primary_skew" &
        preflight$oracle$target == target, , drop = FALSE
    ]
    expect_equal(left$lower_root, -right$upper_root, tolerance = 1e-7)
    expect_equal(left$upper_root, -right$lower_root, tolerance = 1e-7)
    expect_equal(left$mean_tilt, -right$mean_tilt, tolerance = 1e-7)
  }
})

test_that("DGP streams are deterministic and target triplets share responses", {
  environment <- validation_environment()
  config <- validation_config(environment)
  preflight <- environment$omtv_preflight(config)
  streams <- environment$omtv_assign_streams(config, 3L)
  for (family in c("fixed_design", "dlm")) {
    blueprint <- preflight$blueprints[[paste(
      "S03_primary_skew", family, sep = "/"
    )]]
    first <- environment$omtv_generate_replication(
      config, blueprint, 0.80, streams$seed_serialized[[1L]]
    )
    second <- environment$omtv_generate_replication(
      config, blueprint, 0.80, streams$seed_serialized[[1L]]
    )
    expect_identical(first, second)
    expect_identical(first$y, second$y)
    if (identical(family, "dlm")) {
      expect_equal(sum(is.na(first$y)), length(blueprint$missing_times))
      expect_true(all(is.finite(first$y_full)))
      expect_gt(diff(range(
        blueprint$deterministic_seasonal_state[1L, ]
      )), 1)
    }
  }
})

test_that("replication truth uses the generated dynamic location path", {
  environment <- validation_environment()
  config <- validation_config(environment)
  preflight <- environment$omtv_preflight(config)
  stream <- environment$omtv_assign_streams(config, 1L)$seed_serialized[[1L]]
  blueprint <- preflight$blueprints[["S05_article_stress/dlm"]]
  generated <- environment$omtv_generate_replication(
    config, blueprint, 0.80, stream
  )
  oracle <- preflight$oracle[
    preflight$oracle$scenario_id == "S05_article_stress" &
      preflight$oracle$target == "SH", , drop = FALSE
  ]
  truth <- environment$omtv_replication_truth(
    blueprint, generated, oracle
  )
  expect_identical(truth$mean_truth, generated$latent_location)
  expect_equal(
    truth$oracle_lower,
    generated$latent_location + blueprint$scale_truth * oracle$lower_root,
    tolerance = 0
  )
  expect_equal(
    rqrgibbs::rqr_oracle_conditional_content(
      truth$oracle_lower, truth$oracle_upper,
      truth$mean_truth, truth$scale_truth,
      family = "asymmetric_laplace",
      params = list(tau = 0.80, scale = 1, variance_standardized = TRUE)
    ),
    rep(0.95, nrow(truth)), tolerance = 1e-12
  )
})

test_that("prospective schedules are explicit and execution stays fail closed", {
  environment <- validation_environment()
  config <- validation_config(environment)
  expect_error(environment$omtv_replication_schedule(config), "not frozen")
  expect_error(environment$omtv_task_plan(config), "not frozen")

  frozen <- config
  frozen$replication_schedule_frozen <- TRUE
  frozen$execution_authorized <- TRUE
  frozen$precision_planning$status <-
    "frozen_after_production_shape_benchmarks"
  frozen$precision_planning$excess_risk_practical_margin_unfrozen <- FALSE
  frozen$precision_planning$excess_risk_practical_margin <- 0.01
  frozen$resources$maximum_worker_seconds_unfrozen <- FALSE
  frozen$resources$maximum_wave_seconds_unfrozen <- FALSE
  frozen$resources$maximum_process_tree_rss_kib_unfrozen <- FALSE
  frozen$resources$maximum_run_bytes_unfrozen <- FALSE
  frozen$resources$maximum_worker_seconds <- 86400
  frozen$resources$maximum_wave_seconds <- 86400
  frozen$resources$maximum_process_tree_rss_kib <- 25165824
  frozen$resources$maximum_run_bytes <- 1e10
  frozen$replication_schedule <- list(
    replications_by_scenario = stats::setNames(
      as.list(rep(250L, 6L)),
      environment$omtv_scenario_frame(frozen)$scenario_id
    ),
    wave_size = 25L
  )
  expect_invisible(environment$omtv_validate_config(frozen))
  plan <- environment$omtv_task_plan(frozen)
  expect_equal(nrow(plan), 6L * 2L * 3L * 250L)
  expect_identical(anyDuplicated(plan$task_key), 0L)
  expect_equal(max(plan$wave), 10L)

  invalid <- frozen
  invalid$replication_schedule$replications_by_scenario[[1L]] <- 251L
  expect_error(environment$omtv_task_plan(invalid), "declared checkpoint")
})

test_that("precision decisions use dataset replications as the unit", {
  environment <- validation_environment()
  config <- validation_config(environment)
  n <- 250L
  rows <- data.frame(
    scenario_id = "S01_symmetric_control", model_family = "fixed_design",
    target = "RQR", replication = seq_len(n),
    conditional_content_error = rep(c(-0.001, 0.001), length.out = n),
    lower_bias = rep(c(-0.002, 0.002), length.out = n),
    upper_bias = rep(c(-0.002, 0.002), length.out = n),
    width_bias = rep(c(-0.002, 0.002), length.out = n),
    mean_excess_target_risk = rep(c(0, 0.001), length.out = n),
    response_sd = 1, oracle_mean_width = 3,
    stringsAsFactors = FALSE
  )
  decision <- environment$omtv_precision_decision(rows, config, 250L)
  expect_equal(nrow(decision), 1L)
  expect_true(decision$endpoint_content_precision_pass)
  expect_false(decision$excess_risk_precision_pass)
  expect_false(decision$primary_precision_pass)
  expect_true(is.na(decision$excess_risk_precision_threshold))
  expect_lt(decision$content_error_mcse, 0.005)
})

test_that("tiny exact-tilt fits exercise both production model families", {
  skip_if_not_installed("posterior", minimum_version = "1.7.0")
  environment <- validation_environment()
  config <- validation_config(environment)
  preflight <- environment$omtv_preflight(config)
  streams <- environment$omtv_assign_streams(config, 4L)

  for (index in seq_along(c("fixed_design", "dlm"))) {
    family <- c("fixed_design", "dlm")[[index]]
    blueprint <- preflight$blueprints[[paste(
      "S01_symmetric_control", family, sep = "/"
    )]]
    generated <- environment$omtv_generate_replication(
      config, blueprint, 0.50,
      streams$seed_serialized[[2L * index - 1L]]
    )
    oracle <- preflight$oracle[
      preflight$oracle$scenario_id == "S01_symmetric_control" &
        preflight$oracle$target == "ET", , drop = FALSE
    ]
    result <- environment$omtv_fit_replication(
      config, blueprint, generated, oracle,
      streams$seed_serialized[[2L * index]],
      mcmc_override = list(
        n_burn = 10L, n_mcmc = 30L, thin = 1L,
        kernel_repetitions = 1L
      ),
      retain_diagnostic_draws = TRUE
    )
    expect_identical(
      result$schema_version, "rqrgibbs_oracle_mean_tilt_fit/1.0.0"
    )
    expect_identical(result$family, family)
    expect_identical(result$numerical_repair_count, 0L)
    expect_true(result$exact_joint_target)
    expect_true(result$target_numerical_eligible)
    expect_lte(result$loss_identity_maximum_absolute_error, 1e-12)
    expect_true(all(is.finite(result$endpoint_summary$lower_mean)))
    expect_true(all(is.finite(result$endpoint_summary$upper_mean)))
    expect_true(all(
      result$endpoint_summary$lower_mean <=
        result$endpoint_summary$upper_mean
    ))
  }
})
