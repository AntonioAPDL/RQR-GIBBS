test_that("bounded dynamic fixture configuration is exact and non-production", {
  config_path <- testthat::test_path(
    "..", "..", "config", "rqr_dlm",
    "rqr_dlm_bounded_dynamic_fixtures_20260723.R"
  )
  environment <- new.env(parent = baseenv())
  sys.source(config_path, envir = environment)
  config <- environment$rqr_dlm_bounded_dynamic_fixtures

  expect_identical(
    config$schema_version, "rqrgibbs_dlm_bounded_fixtures/5.0.0"
  )
  expect_true(config$generalized_bayes)
  expect_false(config$response_likelihood)
  expect_false(config$response_prediction_contract)
  expect_false(config$production_simulation_authorized)
  expect_true(config$bounded_dynamic_execution_authorized)
  expect_identical(
    config$runner_modes,
    c(
      "preflight", "reference-only", "benchmark-one-cell",
      "execute-bounded"
    )
  )
  expect_true(config$benchmark_one_cell_authorized)
  expect_identical(
    config$benchmark$fixture_id,
    "shared_component_scale_trend_regression"
  )
  expect_identical(config$benchmark$chains, 4L)
  expect_identical(config$mcmc$chains, 4L)
  expect_identical(config$mcmc$burn_in, 2000L)
  expect_identical(config$mcmc$retained_per_chain, 6000L)
  expect_length(unique(config$mcmc$seeds), 4L)
  expect_true(config$mcmc$store_state_draws)
  expect_false(config$mcmc$store_latent_draws)
  expect_length(config$mcmc$initialization_profiles, 4L)
  expect_identical(config$continuation$history_segments, 3L)
  expect_identical(config$continuation$generation_indices, 0:2)
  expect_identical(
    config$continuation$retained_by_segment, c(2L, 2L, 2L)
  )
  expect_identical(config$continuation$uninterrupted_retained, 6L)
  expect_true(config$resources$sequential_execution)
  expect_identical(config$resources$hard_timeout_minutes, 240L)
  expect_true(config$resources$require_active_process_tree_monitor)
  expect_identical(
    config$resources$monitor_kind, "pgid_sampled_fallback"
  )
  expect_false(config$resources$kernel_hard_memory_ceiling)
  expect_identical(config$gates$mcse_provider, "posterior_mcse_mean")
  expect_setequal(
    vapply(
      config$fixtures, function(fixture) fixture$evolution_mode,
      character(1L)
    ),
    c("fixed_W", "discount_template", "component_scale")
  )
  expect_false(any(vapply(
    config$fixtures,
    function(fixture) identical(
      fixture$evolution_mode, "adaptive_discount"
    ),
    logical(1L)
  )))
  expect_true(all(vapply(
    config$fixtures,
    function(fixture) length(fixture$y) == fixture$n_time,
    logical(1L)
  )))
  expect_identical(
    config$fixtures$frozen_trend_seasonal_discount$dim_df,
    c(2L, 3L)
  )
  expect_identical(
    length(
      config$fixtures$
        shared_component_scale_trend_regression$component_templates
    ),
    2L
  )
  expect_identical(
    dim(
      config$fixtures$frozen_trend_seasonal_discount$
        state_components[[1L]]$C0
    ),
    c(2L, 2L)
  )
  expect_identical(
    config$gates$root_swap_activity_role, "sidecar_only"
  )
  helper_path <- testthat::test_path(
    "..", "..", "scripts", "lib", "rqr_dlm_bounded_fixtures.R"
  )
  helper_environment <- new.env(parent = globalenv())
  sys.source(helper_path, envir = helper_environment)
  diagnostics_helper_path <- testthat::test_path(
    "..", "..", "scripts", "lib",
    "rqr_dlm_bounded_diagnostics.R"
  )
  sys.source(diagnostics_helper_path, envir = helper_environment)
  expect_invisible(
    helper_environment$rqr_validate_bounded_dlm_config(config)
  )
  constructed <- helper_environment$rqr_build_all_bounded_dlm_fixtures(
    config
  )
  expect_length(constructed, 3L)
  expect_true(all(vapply(
    constructed,
    function(item) isTRUE(item$evolution$exact_joint_target),
    logical(1L)
  )))
  expect_true(
    constructed$frozen_trend_seasonal_discount$
      construction_audit$extension_reproduces_training
  )
  expect_identical(
    dim(
      constructed$shared_component_scale_trend_regression$
        future$component_templates[[1L]]
    ),
    c(2L, 2L, 3L)
  )
  expected_schema_counts <- c(
    fixed_W_local_level = 117L,
    frozen_trend_seasonal_discount = 181L,
    shared_component_scale_trend_regression = 149L
  )
  for (fixture_id in names(constructed)) {
    fixed_schema <-
      helper_environment$rqr_bounded_expected_estimand_names(
        constructed[[fixture_id]], "fixed_rate"
      )
    learned_schema <-
      helper_environment$rqr_bounded_expected_estimand_names(
        constructed[[fixture_id]],
        "learned_pseudoresidual_normalized"
    )
    expect_length(
      fixed_schema, expected_schema_counts[[fixture_id]]
    )
    expect_length(
      learned_schema, expected_schema_counts[[fixture_id]] + 1L
    )
    expect_identical(
      learned_schema,
      append(fixed_schema, "log_lambda", after = {
        if (identical(
              constructed[[fixture_id]]$evolution$mode,
              "component_scale"
            )) {
          length(fixed_schema) -
            2L * length(
              constructed[[fixture_id]]$evolution$component_names
            )
        } else {
          length(fixed_schema)
        }
      })
    )
  }
  fixed_schema <-
    helper_environment$rqr_bounded_expected_estimand_names(
      constructed$fixed_W_local_level, "fixed_rate"
    )
  complete_chains <- replicate(
    4L,
    matrix(
      1, nrow = 3L, ncol = length(fixed_schema),
      dimnames = list(NULL, fixed_schema)
    ),
    simplify = FALSE
  )
  expect_identical(
    helper_environment$rqr_bounded_validate_estimand_schemas(
      complete_chains, constructed$fixed_W_local_level, "fixed_rate"
    ),
    fixed_schema
  )
  omitted_training <- lapply(
    complete_chains,
    function(values) values[, colnames(values) != "train_lower_t001"]
  )
  omitted_time0 <- lapply(
    complete_chains,
    function(values) {
      values[, colnames(values) != "time0_state_midpoint_1"]
    }
  )
  omitted_future <- lapply(
    complete_chains,
    function(values) {
      values[
        ,
        colnames(values) !=
          "future_conditional_mean_width_t004"
      ]
    }
  )
  expect_error(
    helper_environment$rqr_bounded_validate_estimand_schemas(
      omitted_training, constructed$fixed_W_local_level, "fixed_rate"
    ),
    "schema mismatch"
  )
  expect_error(
    helper_environment$rqr_bounded_validate_estimand_schemas(
      omitted_time0, constructed$fixed_W_local_level, "fixed_rate"
    ),
    "schema mismatch"
  )
  expect_error(
    helper_environment$rqr_bounded_validate_estimand_schemas(
      omitted_future, constructed$fixed_W_local_level, "fixed_rate"
    ),
    "schema mismatch"
  )
  learned_schema <-
    helper_environment$rqr_bounded_expected_estimand_names(
      constructed$fixed_W_local_level,
      "learned_pseudoresidual_normalized"
    )
  learned_chains <- replicate(
    4L,
    matrix(
      1, nrow = 3L, ncol = length(learned_schema),
      dimnames = list(NULL, learned_schema)
    ),
    simplify = FALSE
  )
  omitted_lambda <- lapply(
    learned_chains,
    function(values) values[, colnames(values) != "log_lambda"]
  )
  expect_error(
    helper_environment$rqr_bounded_validate_estimand_schemas(
      omitted_lambda, constructed$fixed_W_local_level,
      "learned_pseudoresidual_normalized"
    ),
    "schema mismatch"
  )
  component_schema <-
    helper_environment$rqr_bounded_expected_estimand_names(
      constructed$shared_component_scale_trend_regression,
      "fixed_rate"
    )
  component_chains <- replicate(
    4L,
    matrix(
      1, nrow = 3L, ncol = length(component_schema),
      dimnames = list(NULL, component_schema)
    ),
    simplify = FALSE
  )
  omitted_component <- lapply(
    component_chains,
    function(values) {
      values[
        ,
        colnames(values) != "log_component_scale_regression"
      ]
    }
  )
  expect_error(
    helper_environment$rqr_bounded_validate_estimand_schemas(
      omitted_component,
      constructed$shared_component_scale_trend_regression,
      "fixed_rate"
    ),
    "schema mismatch"
  )
  initializations <- lapply(
    config$mcmc$initialization_profiles,
    function(profile) {
      helper_environment$rqr_bounded_initialization(
        constructed$fixed_W_local_level,
        profile,
        config$coverage_level
      )
    }
  )
  expect_length(unique(vapply(
    initializations,
    function(initial) digest::digest(
      initial, algo = "sha256", serialize = TRUE
    ),
    character(1L)
  )), 4L)
  expect_true(all(vapply(
    initializations,
    function(initial) {
      all(is.finite(initial$state_root1)) &&
        all(is.finite(initial$state_root2)) &&
        initial$lambda > 0
    },
    logical(1L)
  )))
})

test_that("bounded future means preserve draw identity and RDS publication validates readback", {
  helper_path <- testthat::test_path(
    "..", "..", "scripts", "lib",
    "rqr_dlm_bounded_diagnostics.R"
  )
  helper_environment <- new.env(parent = globalenv())
  sys.source(helper_path, envir = helper_environment)

  marker_fit <- structure(list(
    samp.theta_terminal_root1 = matrix(c(11, 22, 33), 1L, 3L),
    samp.theta_terminal_root2 = matrix(c(-11, -22, -33), 1L, 3L)
  ), class = c("rqr_dlm_mcmc", "rqr_fit"))
  future <- list(
    H = 2L,
    FF = matrix(1, 1L, 2L),
    GG = array(1, dim = c(1L, 1L, 2L))
  )
  conditional <-
    helper_environment$rqr_bounded_future_conditional_mean_roots(
      marker_fit, future
    )
  expect_identical(conditional$draw_index, 1:3)
  expect_identical(
    unname(conditional$eta_root1),
    matrix(rep(c(11, 22, 33), each = 2L), 2L, 3L)
  )
  expect_identical(
    unname(conditional$eta_root2),
    matrix(rep(c(-11, -22, -33), each = 2L), 2L, 3L)
  )

  fit <- rqr_dlm_fit(
    y = c(-1, 0, 1),
    model = rqr_polytrend(1L, C0 = 2),
    coverage_level = 0.8,
    evolution_mode = "fixed_W",
    W = 0.05,
    numerical_policy = "fail",
    mcmc_control = list(
      n_burn = 0, n_mcmc = 1, seed = 87601,
      backend = "cpp", store_state_draws = TRUE
    )
  )
  expect_identical(dim(fit$samp.theta0_root1), c(1L, 1L))
  expect_identical(dim(fit$samp.theta0_root2), c(1L, 1L))
  fit_future <-
    helper_environment$rqr_bounded_future_conditional_mean_roots(
      fit, future
    )
  fit_estimands <- helper_environment$rqr_bounded_chain_estimands(
    fit, fit_future
  )
  fit_fixture <- list(
    expanded_model = fit$expanded_model,
    future = future,
    evolution = fit$evolution
  )
  expect_identical(
    helper_environment$rqr_bounded_validate_estimand_schemas(
      list(fit_estimands), fit_fixture, "fixed_rate"
    ),
    colnames(fit_estimands)
  )
  output_dir <- tempfile("bounded-chain-publication-")
  dir.create(output_dir)
  path <- file.path(output_dir, "valid.rds")
  evidence <- helper_environment$rqr_bounded_publish_fit_rds(
    fit, path
  )
  expect_true(file.exists(path))
  expect_identical(
    evidence$sha256,
    digest::digest(
      file = path, algo = "sha256", serialize = FALSE
    )
  )
  expect_identical(
    readRDS(path)$checkpoint_digest, fit$checkpoint_digest
  )

  invalid <- fit
  invalid$checkpoint_state$completed_iterations <- 99L
  invalid_path <- file.path(output_dir, "invalid.rds")
  expect_error(
    helper_environment$rqr_bounded_publish_fit_rds(
      invalid, invalid_path
    ),
    "checkpoint-digest"
  )
  expect_false(file.exists(invalid_path))

  rollback_dir <- file.path(output_dir, "rollback")
  dir.create(rollback_dir)
  rollback_path <- file.path(rollback_dir, "post-rename-failure.rds")
  expect_error(
    helper_environment$rqr_bounded_publish_fit_rds(
      fit,
      rollback_path,
      post_rename_hook = function(path) {
        expect_true(file.exists(path))
        stop("injected post-rename integrity failure", call. = FALSE)
      }
    ),
    "injected post-rename integrity failure"
  )
  expect_false(file.exists(rollback_path))
  expect_length(
    list.files(rollback_dir, all.files = TRUE, no.. = TRUE),
    0L
  )
})

test_that("intercept CDF references are versioned and generator-bound", {
  reference_path <- testthat::test_path(
    "..", "..", "inst", "extdata",
    "output7_corrected_cdf_references.csv"
  )
  generator_path <- testthat::test_path(
    "..", "..", "scripts",
    "07_generate_intercept_cdf_references.R"
  )
  reference <- utils::read.csv(
    reference_path, stringsAsFactors = FALSE
  )
  expect_identical(nrow(reference), 5L)
  expect_true(all(
    reference$reference_schema ==
      "rqrgibbs_intercept_cdf_reference/2.0.0"
  ))
  expect_setequal(
    reference$estimand,
    c("lambda", "lower_root", "upper_root", "width", "midpoint")
  )
  expect_true(all(reference$order_convergence_difference <= 5e-10))
  expect_identical(
    unique(reference$generator_sha256),
    digest::digest(
      file = generator_path, algo = "sha256", serialize = FALSE
    )
  )
  expect_equal(
    reference$reference_value[
      match(
        c("lambda", "lower_root", "upper_root", "width", "midpoint"),
        reference$estimand
      )
    ],
    c(
      0.347247584303805, 0.408193003274045,
      0.562140568140968, 0.573003849468578,
      0.489059519337561
    ),
    tolerance = 1e-15
  )
})
