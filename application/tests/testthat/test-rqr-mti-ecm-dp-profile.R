test_that("MTI-ECM DP profile selects only candidates passing DP content checks", {
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

test_that("MTI-ECM DP profile stage-2 tuning config freezes broad scope", {
  path <- test_path(
    "..", "..", "config",
    "rqr_bayes_uq_validation_mti_ecm_dp_profile_stage2_tuning_20260823.json"
  )
  skip_if_not(file.exists(path), "validation config is outside package build")
  config <- jsonlite::read_json(path, simplifyVector = FALSE)
  methods <- vapply(config$methods, `[[`, character(1L), "method_id")
  method_by_id <- stats::setNames(config$methods, methods)

  expect_equal(
    config$study_id,
    "rqr_bayes_uq_validation_mti_ecm_dp_profile_stage2_tuning_20260823"
  )
  expect_equal(length(methods), 14L)
  expect_true(all(grepl("^mti_ecm_dp_profile_tune_", methods)))
  expect_equal(config$base_seed, 963300)
  expect_equal(config$modes$confirmatory$replications, 1000)
  expect_equal(length(config$modes$confirmatory$dgp_ids), 8L)
  expect_equal(length(config$modes$confirmatory$design_cells), 9L)
  expect_equal(
    as.numeric(unlist(config$modes$confirmatory$posterior_confidences)),
    0.95
  )
  expect_true(all(vapply(
    config$methods,
    function(x) isTRUE(x$mti_ecm_dp_profile_method),
    logical(1L)
  )))

  screens <- sort(vapply(
    config$methods,
    function(x) as.numeric(x$profile_config$posterior_confidence),
    numeric(1L)
  ))
  expect_true(all(c(0.9800, 0.9825, 0.9850, 0.9875, 0.9890) %in%
                    unique(screens)))
  expect_equal(
    method_by_id$mti_ecm_dp_profile_tune_p985_deepq_q9995$
      profile_config$q_grid_control$q_max,
    0.9995
  )
  expect_equal(
    method_by_id$mti_ecm_dp_profile_tune_p9875_deepq_q9995_widetilt$
      profile_config$tilt_grid_control$max_abs_tilt_sd,
    3
  )
  expect_equal(
    method_by_id$mti_ecm_dp_profile_tune_p985_deepq_q9995_alpha05$
      profile_config$dp_concentration,
    0.5
  )
  expect_equal(
    method_by_id$mti_ecm_dp_profile_tune_p985_deepq_q9995_alpha2$
      profile_config$dp_concentration,
    2
  )
  expect_equal(
    method_by_id$mti_ecm_dp_profile_tune_p985_deepq_q9995_ecm200$
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

test_that("validation worker records stage-2 screens and DP concentration", {
  base_path <- test_path(
    "..", "..", "config",
    "rqr_bayes_uq_validation_mti_ecm_dp_profile_stage2_tuning_20260823.json"
  )
  script <- test_path("..", "..", "scripts", "69_validate_rqr_bayes_uq.R")
  skip_if_not(file.exists(base_path) && file.exists(script),
              "validation launcher files are outside package build")
  config <- jsonlite::read_json(base_path, simplifyVector = FALSE)
  keep <- c("mti_ecm_dp_profile_tune_p9825_deepq",
            "mti_ecm_dp_profile_tune_p985_deepq_q9995_alpha2")
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

  config_path <- tempfile("mti-ecm-dp-profile-stage2-config-",
                          fileext = ".json")
  output_dir <- tempfile("mti-ecm-dp-profile-stage2-smoke-")
  jsonlite::write_json(config, config_path, auto_unbox = TRUE, pretty = TRUE)
  status <- system2(
    "Rscript",
    c(
      script,
      "--mode=smoke",
      paste0("--config=", config_path),
      paste0("--output-dir=", output_dir),
      "--wave-id=mti_ecm_dp_profile_stage2_smoke"
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
  concentration_by_method <- stats::setNames(
    results$direct_dp_concentration,
    results$method_id
  )
  expect_equal(screen_by_method[["mti_ecm_dp_profile_tune_p9825_deepq"]],
               0.9825)
  expect_equal(
    screen_by_method[["mti_ecm_dp_profile_tune_p985_deepq_q9995_alpha2"]],
    0.985
  )
  expect_equal(
    concentration_by_method[["mti_ecm_dp_profile_tune_p9825_deepq"]],
    1
  )
  expect_equal(
    concentration_by_method[["mti_ecm_dp_profile_tune_p985_deepq_q9995_alpha2"]],
    2
  )
})

test_that("adaptive MTI-ECM calibration config freezes candidate scope", {
  path <- test_path(
    "..", "..", "config",
    "rqr_bayes_uq_validation_mti_ecm_adaptive_calibration_20260824.json"
  )
  skip_if_not(file.exists(path), "validation config is outside package build")
  config <- jsonlite::read_json(path, simplifyVector = FALSE)
  methods <- vapply(config$methods, `[[`, character(1L), "method_id")

  expect_equal(
    config$study_id,
    "rqr_bayes_uq_validation_mti_ecm_adaptive_calibration_20260824"
  )
  expect_equal(length(methods), 7L)
  expect_true(all(grepl("^mti_ecm_adaptive_screen_p", methods)))
  expect_true(all(vapply(
    config$methods,
    function(x) isTRUE(x$adaptive_mti_ecm_profile),
    logical(1L)
  )))
  expect_true(config$method_family_scope$candidate_methods_not_final_policy)
  expect_equal(
    config$engine_defaults$mti_ecm_dp_profile$adaptive_policy_id,
    "mti_ecm_adaptive_cell_calibration"
  )
  expect_true(length(config$engine_defaults$mti_ecm_dp_profile$q_offsets) >= 5)
  expect_equal(config$modes$confirmatory$replications, 1000)
})

test_that("strict adaptive MTI-ECM calibration config freezes retuning scope", {
  path <- test_path(
    "..", "..", "config",
    "rqr_bayes_uq_validation_mti_ecm_adaptive_strict_calibration_20260825.json"
  )
  skip_if_not(file.exists(path), "validation config is outside package build")
  config <- jsonlite::read_json(path, simplifyVector = FALSE)
  methods <- vapply(config$methods, `[[`, character(1L), "method_id")
  screens <- vapply(config$methods, function(x) {
    as.numeric(x$profile_config$posterior_confidence)
  }, numeric(1L))

  expect_equal(
    config$study_id,
    "rqr_bayes_uq_validation_mti_ecm_adaptive_strict_calibration_20260825"
  )
  expect_equal(length(methods), 7L)
  expect_true(all(grepl("^mti_ecm_adaptive_strict_screen_p", methods)))
  expect_false(any(methods %in% c("tcsp_mc", "young_mathew", "wilks_exact")))
  expect_equal(
    screens,
    c(0.980, 0.985, 0.990, 0.9925, 0.995, 0.9975, 0.9995)
  )
  expect_true(config$claim_scope$strict_policy_selection_required_before_promotion)
  expect_equal(config$modes$confirmatory$replications, 1000)
  expect_equal(length(config$modes$confirmatory$design_cells), 9L)
  expect_equal(length(config$modes$confirmatory$dgp_ids), 8L)
  expect_equal(config$modes$confirmatory$method_ids, as.list(methods))
  q_offsets <- as.numeric(config$engine_defaults$mti_ecm_dp_profile$q_offsets)
  expect_true(any(q_offsets < 0))
  expect_true(any(q_offsets > 0))
  expect_gte(config$engine_defaults$mti_ecm_dp_profile$q_max, 0.9995)
  expect_equal(
    config$engine_defaults$mti_ecm_dp_profile$adaptive_policy_id,
    "mti_ecm_adaptive_cell_strict_calibration"
  )
})

test_that("targeted MTI-ECM refinement config freezes the dense screen menu", {
  path <- test_path(
    "..", "..", "config",
    "rqr_bayes_uq_validation_mti_ecm_adaptive_targeted_refinement_20260826.json"
  )
  skip_if_not(file.exists(path), "validation config is outside package build")
  config <- jsonlite::read_json(path, simplifyVector = FALSE)
  methods <- vapply(config$methods, `[[`, character(1L), "method_id")
  screens <- vapply(config$methods, function(x) {
    as.numeric(x$profile_config$posterior_confidence)
  }, numeric(1L))

  expect_equal(
    config$study_id,
    "rqr_bayes_uq_validation_mti_ecm_adaptive_targeted_refinement_20260826"
  )
  expect_equal(length(methods), 10L)
  expect_true(all(grepl("^mti_ecm_adaptive_refine_screen_p", methods)))
  expect_false(any(grepl("p9995$", methods)))
  expect_equal(
    screens,
    c(0.995, 0.996, 0.9965, 0.997, 0.99725,
      0.9975, 0.998, 0.9985, 0.999, 0.99925)
  )
  expect_equal(config$diagnostics$policy_builder_selection_rule, "cell")
  expect_equal(config$diagnostics$policy_builder_width_objective,
               "median-q975")
  expect_true(config$claim_scope$strict_policy_selection_required_before_promotion)
  expect_equal(config$modes$confirmatory$replications, 1000)
  expect_equal(length(config$modes$confirmatory$design_cells), 9L)
  expect_equal(length(config$modes$confirmatory$dgp_ids), 8L)
  expect_equal(config$modes$confirmatory$method_ids, as.list(methods))
  expect_true(all(config$modes$smoke$method_ids %in% methods))
  boundary_q <- as.numeric(
    config$engine_defaults$mti_ecm_dp_profile$boundary_q_values
  )
  expect_true(all(screens %in% boundary_q))
  expect_gte(config$engine_defaults$mti_ecm_dp_profile$q_max, 0.9995)
  expect_equal(
    config$engine_defaults$mti_ecm_dp_profile$adaptive_policy_id,
    "mti_ecm_adaptive_cell_targeted_refinement"
  )
})

test_that("adaptive MTI-ECM selected config freezes deployed cell policy", {
  path <- test_path(
    "..", "..", "config",
    "rqr_bayes_uq_validation_mti_ecm_adaptive_selected_20260825.json"
  )
  skip_if_not(file.exists(path), "validation config is outside package build")
  config <- jsonlite::read_json(path, simplifyVector = FALSE)
  methods <- vapply(config$methods, `[[`, character(1L), "method_id")

  expect_equal(
    config$study_id,
    "rqr_bayes_uq_validation_mti_ecm_adaptive_selected_20260825"
  )
  expect_equal(methods, "mti_ecm_adaptive_cell")
  expect_equal(config$adaptive_mti_ecm$policy_id,
               "mti_ecm_adaptive_cell_pooled_20260825")
  repo_root <- normalizePath(test_path("..", "..", ".."),
                             winslash = "/", mustWork = TRUE)
  expect_true(file.exists(file.path(
    repo_root, config$adaptive_mti_ecm$frozen_policy_path
  )))
  expect_true(file.exists(file.path(
    repo_root, config$adaptive_mti_ecm$diagnostics_path
  )))
  expect_true(isTRUE(config$methods[[1L]]$deployable))
  expect_true(isTRUE(config$methods[[1L]]$adaptive_mti_ecm_profile))
  expect_equal(
    config$methods[[1L]]$profile_config$frozen_policy_path,
    config$adaptive_mti_ecm$frozen_policy_path
  )
  expect_equal(config$modes$confirmatory$replications, 1000)
})

test_that("validation worker records adaptive MTI-ECM menu diagnostics", {
  base_path <- test_path(
    "..", "..", "config",
    "rqr_bayes_uq_validation_mti_ecm_adaptive_calibration_20260824.json"
  )
  script <- test_path("..", "..", "scripts", "69_validate_rqr_bayes_uq.R")
  skip_if_not(file.exists(base_path) && file.exists(script),
              "validation launcher files are outside package build")
  config <- jsonlite::read_json(base_path, simplifyVector = FALSE)
  keep <- "mti_ecm_adaptive_screen_p985"
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
  config$engine_defaults$mti_ecm_dp_profile$tilt_grid_control <- list(
    tilt_offsets_sd = list(0),
    include_zero_tilt = TRUE,
    max_abs_tilt_sd = 2
  )
  config$engine_defaults$mti_ecm_dp_profile$ecm_control <- list(
    max_iter = 25,
    tol_stationarity = 1e6,
    stable_iterations = 1,
    residual_product_floor = 1e-8,
    multistart = FALSE,
    store_iteration_trace = FALSE,
    ecm_backend = "cpp"
  )

  config_path <- tempfile("mti-ecm-adaptive-config-", fileext = ".json")
  output_dir <- tempfile("mti-ecm-adaptive-smoke-")
  jsonlite::write_json(config, config_path, auto_unbox = TRUE, pretty = TRUE)
  status <- system2(
    "Rscript",
    c(
      script,
      "--mode=smoke",
      paste0("--config=", config_path),
      paste0("--output-dir=", output_dir),
      "--wave-id=mti_ecm_adaptive_smoke"
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  exit_status <- attr(status, "status")
  if (is.null(exit_status)) exit_status <- 0L
  expect_equal(exit_status, 0L, info = paste(status, collapse = "\n"))

  results <- read.csv(file.path(output_dir, "bayes_uq_validation_results.csv"))
  expect_equal(nrow(results), 1L)
  expect_equal(results$method_id, keep)
  expect_equal(results$adaptive_policy_id,
               "mti_ecm_adaptive_cell_calibration")
  expect_true(nzchar(results$adaptive_menu_digest))
  expect_true(nzchar(results$adaptive_q_grid_digest))
  expect_true(is.finite(results$adaptive_q_anchor))
  expect_true(results$adaptive_q_grid_size >= 1)
  expect_true(is.finite(results$sample_bowley_skewness))
  expect_true(is.finite(results$sample_tail_ratio))
  expect_equal(results$effective_posterior_confidence, 0.985)
})

test_that("validation worker consumes frozen adaptive MTI-ECM policy", {
  base_path <- test_path(
    "..", "..", "config",
    "rqr_bayes_uq_validation_mti_ecm_adaptive_selected_20260825.json"
  )
  script <- test_path("..", "..", "scripts", "69_validate_rqr_bayes_uq.R")
  skip_if_not(file.exists(base_path) && file.exists(script),
              "validation launcher files are outside package build")
  config <- jsonlite::read_json(base_path, simplifyVector = FALSE)
  config$modes$smoke$replications <- 1
  config$modes$smoke$dgp_ids <- list("normal")
  config$modes$smoke$design_cells <- list(list(
    cell_id = "n0050_c090_t095",
    n = 50,
    guaranteed_content = 0.90,
    tolerance_confidence = 0.95
  ))
  config$modes$smoke$sample_sizes <- list(50)
  config$modes$smoke$guaranteed_contents <- list(0.90)
  config$modes$smoke$tolerance_confidences <- list(0.95)
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
  config$engine_defaults$mti_ecm_dp_profile$tilt_grid_control <- list(
    tilt_offsets_sd = list(0),
    include_zero_tilt = TRUE,
    max_abs_tilt_sd = 2
  )
  config$engine_defaults$mti_ecm_dp_profile$ecm_control <- list(
    max_iter = 25,
    tol_stationarity = 1e6,
    stable_iterations = 1,
    residual_product_floor = 1e-8,
    multistart = FALSE,
    store_iteration_trace = FALSE,
    ecm_backend = "cpp"
  )

  config_path <- tempfile("mti-ecm-adaptive-selected-config-", fileext = ".json")
  output_dir <- tempfile("mti-ecm-adaptive-selected-smoke-")
  jsonlite::write_json(config, config_path, auto_unbox = TRUE, pretty = TRUE)
  status <- system2(
    "Rscript",
    c(
      script,
      "--mode=smoke",
      paste0("--config=", config_path),
      paste0("--output-dir=", output_dir),
      "--wave-id=mti_ecm_adaptive_selected_smoke"
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  exit_status <- attr(status, "status")
  if (is.null(exit_status)) exit_status <- 0L
  expect_equal(exit_status, 0L, info = paste(status, collapse = "\n"))

  results <- read.csv(file.path(output_dir, "bayes_uq_validation_results.csv"))
  expect_equal(nrow(results), 1L)
  expect_equal(results$method_id, "mti_ecm_adaptive_cell")
  expect_equal(results$adaptive_policy_id,
               "mti_ecm_adaptive_cell_pooled_20260825")
  expect_equal(results$adaptive_source_method_id,
               "mti_ecm_adaptive_screen_p980")
  expect_equal(results$effective_posterior_confidence, 0.980)
  expect_true(nzchar(results$adaptive_calibration_digest))
  expect_true(nzchar(results$adaptive_menu_digest))
  expect_true(results$adaptive_q_grid_size >= 1)
})

test_that("independent guarded MTI-ECM validation freezes selected policy", {
  config_path <- test_path(
    "..", "..", "config",
    "rqr_bayes_uq_validation_mti_ecm_independent_guarded_20260827.json"
  )
  policy_path <- test_path(
    "..", "..", "config",
    "mti_ecm_adaptive_cell_guarded_p995_policy_20260827.csv"
  )
  diagnostics_path <- test_path(
    "..", "..", "config",
    "mti_ecm_adaptive_cell_guarded_p995_policy_20260827_diagnostics.csv"
  )
  skip_if_not(
    file.exists(config_path) &&
      file.exists(policy_path) &&
      file.exists(diagnostics_path),
    "independent validation config and policy are outside package build"
  )

  config <- jsonlite::read_json(config_path, simplifyVector = FALSE)
  policy <- read.csv(policy_path, stringsAsFactors = FALSE)
  method_ids <- vapply(config$methods, `[[`, character(1L), "method_id")
  confirmatory_ids <- as.character(unlist(
    config$modes$confirmatory$method_ids, use.names = FALSE
  ))

  expect_true(isTRUE(config$execution$confirmatory_authorized))
  expect_equal(as.integer(config$execution$confirmatory_max_concurrent), 50L)
  expect_identical(confirmatory_ids, "mti_ecm_adaptive_cell")
  expect_false(any(grepl("^mti_ecm_adaptive_refine_screen_", confirmatory_ids)))
  expect_identical(
    config$adaptive_mti_ecm$frozen_policy_path,
    "application/config/mti_ecm_adaptive_cell_guarded_p995_policy_20260827.csv"
  )
  expect_identical(
    config$adaptive_mti_ecm$policy_id,
    "mti_ecm_adaptive_cell_guarded_p995_20260827"
  )
  expect_true(all(policy$method_id == "mti_ecm_adaptive_cell"))
  expect_true(all(policy$source_method_id %in% method_ids))
  expect_equal(as.integer(config$modes$confirmatory$replications), 1000L)
  expect_equal(as.integer(config$base_seed), 1982700L)
  expect_false(isTRUE(config$posterior_predictive_response_draws))
})
