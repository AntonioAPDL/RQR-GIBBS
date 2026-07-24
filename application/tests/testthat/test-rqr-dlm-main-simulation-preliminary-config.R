load_main_simulation_contract <- function() {
  config_root <- testthat::test_path(
    "..", "..", "config", "rqr_dlm"
  )
  env <- new.env(parent = baseenv())
  sys.source(
    file.path(
      config_root, "rqr_dlm_main_simulation_preliminary_20260724.R"
    ),
    envir = env
  )
  list(
    config = env$rqr_dlm_main_simulation_preliminary,
    scenarios = utils::read.csv(
      file.path(
        config_root,
        "rqr_dlm_main_simulation_preliminary_scenarios_20260724.csv"
      ),
      stringsAsFactors = FALSE, check.names = FALSE
    ),
    methods = utils::read.csv(
      file.path(
        config_root,
        "rqr_dlm_main_simulation_preliminary_methods_20260724.csv"
      ),
      stringsAsFactors = FALSE, check.names = FALSE
    )
  )
}

test_that("main RQR-DLM design revision is versioned and fail closed", {
  contract <- load_main_simulation_contract()
  config <- contract$config
  expect_identical(
    config$schema_version,
    "rqrgibbs_dlm_main_simulation_preliminary/0.2.0"
  )
  expect_identical(
    config$runner_modes_implemented,
    c(
      "preflight", "oracle-reference", "tiny-end-to-end",
      "diagnostic-pilot-preflight"
    )
  )
  expect_false(config$diagnostic_pilot_execution_authorized)
  expect_false(config$confirmatory_execution_authorized)
  expect_true(config$generalized_bayes)
  expect_false(config$response_likelihood)
  expect_false(config$response_prediction_contract)
  expect_true("RQR-DESN" %in% config$scope$excludes)
  expect_true("CAVI or ELBO" %in% config$scope$excludes)
  expect_true(all(
    c("diagnostic-pilot", "execute-confirmatory") %in%
      config$runner_modes_forbidden
  ))
})

test_that("oracle and target-aligned estimand contracts are explicit", {
  config <- load_main_simulation_contract()$config
  expect_identical(
    config$oracle$independent_certificate,
    "coverage_profile_all_detected_basins"
  )
  expect_true(config$oracle$location_scale_equivariance_required)
  expect_false(config$oracle$call_numerical_error_a_rigorous_bound)
  expect_true(config$oracle$endpoint_rmse_requires_unique_minimizer)
  expect_identical(
    config$estimands$rqr_method_endpoint_target,
    "population_RQR_roots"
  )
  expect_identical(
    config$estimands$quantile_method_endpoint_target,
    "population_equal_tailed_quantile_endpoints"
  )
  expect_identical(
    config$estimands$cross_target_measure_label,
    "cross_target_distance_not_bias"
  )
  expect_identical(
    config$estimands$rqr_loss_role, "RQR_home_target_measure"
  )
})

test_that("coverage-width and Monte Carlo rules cannot reward noise", {
  config <- load_main_simulation_contract()$config
  expect_equal(config$coverage_width$practical_equivalence_margin, 0.02)
  expect_equal(config$coverage_width$confidence_level, 0.90)
  expect_identical(
    config$coverage_width$qualification,
    "TOST_interval_wholly_inside_margin"
  )
  expect_true(
    config$coverage_width$narrower_width_requires_both_coverage_qualified
  )
  expect_true(config$coverage_width$coverage_width_frontier_required)
  expect_true(
    config$coverage_width$post_hoc_test_width_calibration_forbidden
  )
  expect_identical(
    unname(config$coverage_width$nominal_replications_for_margin),
    c(1083L, 609L)
  )
  expect_identical(
    config$monte_carlo$endpoint_midpoint_mcse_denominator,
    "training_response_sd"
  )
  expect_identical(
    config$monte_carlo$width_mcse_denominator,
    "mean_oracle_RQR_width"
  )
  expect_identical(
    config$monte_carlo$near_zero_oracle_width_policy, "fail_DGP"
  )
  expect_true(config$monte_carlo$stopping_uses_monte_carlo_precision_only)
  expect_false(config$monte_carlo$stopping_uses_performance_rank)
  expect_false(config$monte_carlo$stopping_uses_sign_or_significance)
})

test_that("scenario table freezes DGPs and required contrasts", {
  contract <- load_main_simulation_contract()
  config <- contract$config
  scenarios <- contract$scenarios
  expect_identical(anyDuplicated(scenarios$dgp_id), 0L)
  expect_setequal(
    scenarios$dgp_id,
    c(config$design$primary_dgp_ids, config$design$sensitivity_dgp_ids)
  )
  expect_true(all(
    scenarios$schema_version ==
      "rqrgibbs_dlm_main_simulation_scenario/1.0.0"
  ))
  required_nonmissing <- c(
    "state_structure", "error_family", "error_parameters",
    "initial_state_law", "predictor_law", "innovation_covariance",
    "scale_formula", "scale_floor", "training_transition",
    "future_transition", "minimum_root_separation",
    "reference_coverage", "shared_response_law_across_coverages",
    "claim_scope"
  )
  expect_true(all(vapply(
    scenarios[required_nonmissing],
    function(value) all(!is.na(value) & nzchar(as.character(value))),
    logical(1L)
  )))
  expect_true(all(scenarios$scale_floor > 0))
  expect_true(all(scenarios$minimum_root_separation > 0))
  expect_true(all(scenarios$shared_response_law_across_coverages))

  seasonal <- scenarios[
    scenarios$matched_pair_id == "trend_seasonal_error_pair", ,
    drop = FALSE
  ]
  expect_equal(nrow(seasonal), 2L)
  matched_fields <- c(
    "state_structure", "initial_state_law", "predictor_law",
    "innovation_covariance", "seasonal_period", "seasonal_amplitude",
    "seasonal_phase", "scale_formula", "scale_floor", "core_T", "core_H"
  )
  for (field in matched_fields) {
    expect_identical(seasonal[[field]][[1L]], seasonal[[field]][[2L]])
  }
  expect_setequal(
    seasonal$error_family,
    c("standard_normal", "centered_standardized_lognormal")
  )
  expect_true(
    "rqr_dlm_common_evolution_ablation" %in% contract$methods$method_id
  )
  independent <- scenarios[
    scenarios$dgp_id == "independent_root_prior_alignment", ,
    drop = FALSE
  ]
  expect_match(independent$scale_formula, "U_t-L_t", fixed = TRUE)
  expect_match(independent$training_transition, "recover mu_t and s_t")
  expect_true(independent$shared_response_law_across_coverages)
})

test_that("comparator and tuning contracts are pinned", {
  contract <- load_main_simulation_contract()
  config <- contract$config
  methods <- contract$methods
  expect_identical(anyDuplicated(methods$method_id), 0L)
  expect_true(all(
    methods$schema_version ==
      "rqrgibbs_dlm_main_simulation_method/1.0.0"
  ))
  expect_identical(
    config$methods$dynamic_quantile_engine,
    "CRAN_exdqlm_1.1.0_reduced_AL_DQLM_MCMC"
  )
  expect_identical(config$methods$static_quantile_engine, "quantreg_rq")
  expect_identical(config$methods$external_source$version, "1.1.0")
  expect_match(
    config$methods$external_source$sha256, "^[0-9a-f]{64}$"
  )
  expect_false(config$methods$external_source$protected_checkout_used)
  expect_true(config$methods$external_source$isolated_runtime_required)
  expect_identical(
    config$methods$static_external_source$package, "quantreg"
  )
  expect_identical(
    config$methods$static_external_source$version, "6.1"
  )
  expect_match(
    config$methods$static_external_source$sha256, "^[0-9a-f]{64}$"
  )
  expect_true(
    config$methods$static_external_source$isolated_runtime_required
  )
  dynamic <- methods[
    methods$method_id == "dynamic_equal_tailed_quantile_interval", ,
    drop = FALSE
  ]
  expect_match(dynamic$engine, "reduced_AL_DQLM_MCMC")
  expect_match(dynamic$crossing_rule, "store_raw_then_order")
  static <- methods[
    methods$method_id == "static_equal_tailed_quantile_regression", ,
    drop = FALSE
  ]
  expect_identical(static$engine, "quantreg_rq")
  expect_true(config$tuning$uses_training_data_only)
  expect_true(config$tuning$equal_search_budget_required)
  expect_true(config$tuning$test_coverage_tuning_forbidden)
  expect_lte(
    config$tuning$maximum_discount_combinations,
    length(config$tuning$block_specific_discount_grid)^2
  )
})

test_that("forecast and MCMC evidence contracts are distinct", {
  config <- load_main_simulation_contract()$config
  expect_false(identical(
    config$forecast$realized_root_path,
    config$forecast$oracle_conditional_mean_root
  ))
  expect_identical(
    config$forecast$endpoint_bias_rmse_target,
    "oracle_conditional_mean_root"
  )
  expect_identical(
    config$forecast$realized_forecast_error_target,
    "realized_root_path"
  )
  expect_false(config$forecast$posterior_predictive_coverage_claim)
  expect_false(config$forecast$true_W_method_competitive)
  expect_gte(
    config$monte_carlo$
      diagnostic_pilot_replications_per_mechanism_coverage_method,
    2L
  )
  expect_identical(config$mcmc$diagnostic_pilot_chains, 4L)
  expect_true(config$mcmc$per_fit_within_chain_ess_required)
  expect_true(config$mcmc$sentinel_selected_before_data_generation)
  expect_identical(
    config$mcmc$sentinel_failure_action, "stop_declared_cell"
  )
  expect_true(config$mcmc$zero_repairs_required)
  expect_true(config$mcmc$exact_provenance_required)
})
