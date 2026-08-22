test_that("MTI-ECM DP profile selects only candidates passing DP screen", {
  y <- sort(c(-2.2, -1.5, -0.9, -0.4, 0.1, 0.5, 0.9, 1.4, 2.1, 3.2))
  action <- rqr_mti_ecm_dp_profile_action(
    y = y,
    content = 0.55,
    posterior_confidence = 0.50,
    dp_concentration = 1,
    dp_base_measure = rqr_dp_base_normal(0, 4),
    scan_target_content = 0.80,
    q_grid = c(0.60, 0.75),
    tilt_grid_control = list(
      tilt_offsets_sd = 0,
      include_zero_tilt = TRUE,
      max_abs_tilt_sd = 2
    ),
    ecm_control = list(
      max_iter = 40,
      stable_iterations = 1,
      tol_stationarity = 1e6,
      residual_product_floor = 1e-8,
      multistart = FALSE,
      ecm_backend = "cpp"
    ),
    expand_if_empty = FALSE
  )

  expect_s3_class(action, "rqr_mti_ecm_dp_profile_action")
  expect_true(action$generalized_bayes)
  expect_true(action$response_likelihood)
  expect_true(action$no_fallback_used)
  expect_gt(action$candidates_evaluated, 0)
  expect_gt(nrow(action$selected), 0)
  expect_gte(action$selected$posterior_content_probability[[1L]], 0.50)
  expect_equal(action$selected$width[[1L]], min(
    action$candidates$width[
      action$candidates$posterior_constraint_satisfied
    ],
    na.rm = TRUE
  ))
})

test_that("MTI-ECM DP profile reports infeasibility without fallback", {
  y <- sort(seq(-1, 1, length.out = 12))
  action <- rqr_mti_ecm_dp_profile_action(
    y = y,
    content = 0.95,
    posterior_confidence = 0.999,
    dp_concentration = 1,
    dp_base_measure = rqr_dp_base_normal(0, 4),
    q_grid = 0.951,
    tilt_grid_control = list(
      tilt_offsets_sd = 0,
      include_zero_tilt = FALSE,
      max_abs_tilt_sd = 2
    ),
    ecm_control = list(
      max_iter = 20,
      stable_iterations = 1,
      tol_stationarity = 1e6,
      residual_product_floor = 1e-8,
      multistart = FALSE,
      ecm_backend = "cpp"
    ),
    expand_if_empty = FALSE
  )

  expect_equal(nrow(action$selected), 0)
  expect_equal(action$posterior_constraint_status,
               "infeasible_within_profile_grid")
  expect_true(action$no_fallback_used)
})

test_that("MTI-ECM DP profile add-on config freezes intended scope", {
  path <- test_path(
    "..", "..", "config",
    "rqr_bayes_uq_validation_mti_ecm_dp_profile_addon_20260822.json"
  )
  skip_if_not(file.exists(path), "validation config is outside package build")
  config <- jsonlite::read_json(path, simplifyVector = FALSE)
  methods <- vapply(config$methods, `[[`, character(1L), "method_id")
  method <- config$methods[[match("mti_ecm_dp_profile", methods)]]

  expect_equal(config$study_id,
               "rqr_bayes_uq_validation_mti_ecm_dp_profile_addon_20260822")
  expect_true(config$method_family_scope$direct_dp_content_screen)
  expect_true(config$method_family_scope$no_fallback_to_tcsp)
  expect_equal(as.character(config$modes$confirmatory$method_ids),
               "mti_ecm_dp_profile")
  expect_true(method$generalized_bayes)
  expect_true(method$response_likelihood)
  expect_false(method$formal_tolerance_action)
  expect_equal(method$scan_method, "monte_carlo_cp_adaptive")
  expect_equal(config$engine_defaults$mti_ecm_dp_profile$
                 confirmatory_ecm_control$ecm_backend, "cpp")
})

test_that("MTI-ECM DP profile tuning config freezes paired variant scope", {
  path <- test_path(
    "..", "..", "config",
    "rqr_bayes_uq_validation_mti_ecm_dp_profile_tuning_20260822.json"
  )
  skip_if_not(file.exists(path), "validation config is outside package build")
  config <- jsonlite::read_json(path, simplifyVector = FALSE)
  methods <- vapply(config$methods, `[[`, character(1L), "method_id")
  method_by_id <- stats::setNames(config$methods, methods)

  expect_equal(config$study_id,
               "rqr_bayes_uq_validation_mti_ecm_dp_profile_tuning_20260822")
  expect_equal(length(methods), 8L)
  expect_true(all(grepl("^mti_ecm_dp_profile_tune_", methods)))
  expect_equal(
    as.numeric(unlist(config$modes$confirmatory$posterior_confidences)),
    0.95
  )
  expect_true(all(vapply(
    config$methods,
    function(x) isTRUE(x$mti_ecm_dp_profile_method),
    logical(1L)
  )))
  expect_equal(
    method_by_id$mti_ecm_dp_profile_tune_p990_deepq$profile_config$
      posterior_confidence,
    0.990
  )
  expect_equal(
    method_by_id$mti_ecm_dp_profile_tune_p995_deepq_widetilt$
      profile_config$q_grid_control$q_max,
    0.9995
  )
  expect_equal(
    method_by_id$mti_ecm_dp_profile_tune_p990_deepq_ecm200$
      profile_config$ecm_control$max_iter,
    200
  )
})

test_that("validation worker runs a reduced MTI-ECM DP profile smoke", {
  base_path <- test_path(
    "..", "..", "config",
    "rqr_bayes_uq_validation_mti_ecm_dp_profile_addon_20260822.json"
  )
  script <- test_path("..", "..", "scripts", "69_validate_rqr_bayes_uq.R")
  skip_if_not(file.exists(base_path) && file.exists(script),
              "validation launcher files are outside package build")
  config <- jsonlite::read_json(base_path, simplifyVector = FALSE)
  config$modes$smoke$replications <- 1
  config$modes$smoke$dgp_ids <- list("normal")
  config$modes$smoke$design_cells <- list(list(
    cell_id = "n0020_c050_t080",
    n = 20,
    guaranteed_content = 0.50,
    tolerance_confidence = 0.80
  ))
  config$modes$smoke$sample_sizes <- list(20)
  config$modes$smoke$guaranteed_contents <- list(0.50)
  config$modes$smoke$tolerance_confidences <- list(0.80)
  config$modes$smoke$posterior_confidences <- list(0.50)
  config$modes$smoke$scan_n_sim <- 100
  config$modes$smoke$scan_numerical_confidence <- 0.80
  config$modes$smoke$scan_adaptive_control <- list(
    initial_n_sim = 100,
    batch_n_sim = 100,
    max_n_sim = 200,
    max_looks = 2,
    stable_looks = 1
  )
  config$engine_defaults$mti_ecm_dp_profile$smoke_q_grid_control <-
    list(n_points = 2, q_max = 0.90, include_scan_target = TRUE)
  config$engine_defaults$mti_ecm_dp_profile$smoke_tilt_grid_control <-
    list(
      tilt_offsets_sd = list(0),
      include_zero_tilt = TRUE,
      max_abs_tilt_sd = 2
    )
  config$engine_defaults$mti_ecm_dp_profile$smoke_ecm_control <-
    list(
      max_iter = 25,
      tol_stationarity = 1e6,
      stable_iterations = 1,
      residual_product_floor = 1e-8,
      multistart = FALSE,
      store_iteration_trace = FALSE,
      ecm_backend = "cpp"
    )

  config_path <- tempfile("mti-ecm-dp-profile-config-", fileext = ".json")
  output_dir <- tempfile("mti-ecm-dp-profile-smoke-")
  jsonlite::write_json(config, config_path, auto_unbox = TRUE, pretty = TRUE)
  status <- system2(
    "Rscript",
    c(
      script,
      "--mode=smoke",
      paste0("--config=", config_path),
      paste0("--output-dir=", output_dir),
      "--wave-id=mti_ecm_dp_profile_smoke"
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  exit_status <- attr(status, "status")
  if (is.null(exit_status)) exit_status <- 0L
  expect_equal(exit_status, 0L, info = paste(status, collapse = "\n"))

  results <- read.csv(file.path(output_dir, "bayes_uq_validation_results.csv"))
  expect_equal(nrow(results), 1L)
  expect_equal(results$method_id, "mti_ecm_dp_profile")
  expect_true(results$response_likelihood)
  expect_true(results$generalized_bayes)
  expect_true(is.finite(results$candidates_evaluated))
  expect_gt(results$scan_target_content, results$guaranteed_content)
  expect_gte(results$posterior_probability, 0.50)
  expect_equal(results$mti_certificate_scope,
               "direct_dp_content_screen_repeated_sampling_validation")
})

test_that("validation worker records method-specific MTI-ECM tuning screens", {
  base_path <- test_path(
    "..", "..", "config",
    "rqr_bayes_uq_validation_mti_ecm_dp_profile_tuning_20260822.json"
  )
  script <- test_path("..", "..", "scripts", "69_validate_rqr_bayes_uq.R")
  skip_if_not(file.exists(base_path) && file.exists(script),
              "validation launcher files are outside package build")
  config <- jsonlite::read_json(base_path, simplifyVector = FALSE)
  keep <- c("mti_ecm_dp_profile_tune_p095",
            "mti_ecm_dp_profile_tune_p990_deepq")
  config$methods <- config$methods[
    vapply(config$methods, function(x) x$method_id %in% keep, logical(1L))
  ]
  config$modes$smoke$method_ids <- as.list(keep)
  config$modes$smoke$replications <- 1
  config$modes$smoke$dgp_ids <- list("normal")
  config$modes$smoke$design_cells <- list(list(
    cell_id = "n0020_c050_t080",
    n = 20,
    guaranteed_content = 0.50,
    tolerance_confidence = 0.80
  ))
  config$modes$smoke$sample_sizes <- list(20)
  config$modes$smoke$guaranteed_contents <- list(0.50)
  config$modes$smoke$tolerance_confidences <- list(0.80)
  config$modes$smoke$posterior_confidences <- list(0.95)
  config$modes$smoke$scan_n_sim <- 100
  config$modes$smoke$scan_numerical_confidence <- 0.80
  config$modes$smoke$scan_adaptive_control <- list(
    initial_n_sim = 100,
    batch_n_sim = 100,
    max_n_sim = 200,
    max_looks = 2,
    stable_looks = 1
  )
  for (ii in seq_along(config$methods)) {
    config$methods[[ii]]$profile_config$ecm_control <- list(
      max_iter = 25,
      tol_stationarity = 1e6,
      stable_iterations = 1,
      residual_product_floor = 1e-8,
      multistart = FALSE,
      store_iteration_trace = FALSE,
      ecm_backend = "cpp"
    )
    config$methods[[ii]]$profile_config$tilt_grid_control <- list(
      tilt_offsets_sd = list(0),
      include_zero_tilt = TRUE,
      max_abs_tilt_sd = 2
    )
    config$methods[[ii]]$profile_config$q_grid_control <- list(
      n_points = 2,
      q_max = 0.90,
      include_scan_target = TRUE
    )
  }

  config_path <- tempfile("mti-ecm-dp-profile-tuning-config-",
                          fileext = ".json")
  output_dir <- tempfile("mti-ecm-dp-profile-tuning-smoke-")
  jsonlite::write_json(config, config_path, auto_unbox = TRUE, pretty = TRUE)
  status <- system2(
    "Rscript",
    c(
      script,
      "--mode=smoke",
      paste0("--config=", config_path),
      paste0("--output-dir=", output_dir),
      "--wave-id=mti_ecm_dp_profile_tuning_smoke"
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  exit_status <- attr(status, "status")
  if (is.null(exit_status)) exit_status <- 0L
  expect_equal(exit_status, 0L, info = paste(status, collapse = "\n"))

  results <- read.csv(file.path(output_dir, "bayes_uq_validation_results.csv"))
  expect_equal(sort(results$method_id), sort(keep))
  screen_by_method <- stats::setNames(
    results$effective_posterior_confidence,
    results$method_id
  )
  expect_equal(screen_by_method[["mti_ecm_dp_profile_tune_p095"]], 0.95)
  expect_equal(screen_by_method[["mti_ecm_dp_profile_tune_p990_deepq"]], 0.99)
  expect_true(all(results$posterior_confidence == 0.95))
})
