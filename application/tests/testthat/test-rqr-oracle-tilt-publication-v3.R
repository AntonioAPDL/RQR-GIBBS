testthat::local_edition(3)

repo_root <- normalizePath(file.path(testthat::test_path(), "..", "..", ".."))
source(file.path(
  repo_root, "application", "scripts", "32_oracle_tilt_illustration_utils.R"
))
source(file.path(
  repo_root, "application", "scripts", "33_oracle_tilt_forensic_utils.R"
))
source(file.path(
  repo_root, "application", "scripts", "34_oracle_tilt_publication_utils.R"
))
source(file.path(
  repo_root, "application", "scripts", "42_oracle_tilt_publication_v3_utils.R"
))

config_path <- file.path(
  repo_root, "application", "config",
  "oracle_tilt_c095_publication_v3_20260801.json"
)
read_config <- function() oti_read_json(config_path)

valid_provenance_state <- function() {
  state <- as.list(setNames(
    rep(TRUE, length(otv3_provenance_gate_names())),
    otv3_provenance_gate_names()
  ))
  state$git_dirty <- FALSE
  state$git_commit <- strrep("a", 40L)
  state$expected_git_commit <- state$git_commit
  state$runtime_package_path <- "/isolated/library/rqrgibbs"
  state$runtime_package_version <- "0.1.0.9030"
  state$runtime_attestation <- "/isolated/attestation.rds"
  state$runtime_attestation_schema <- "rqrgibbs_runtime_attestation/5.0.0"
  state$source_tree_digest <- strrep("b", 40L)
  state$runtime_package_tree_digest <- strrep("c", 64L)
  state
}

valid_provenance_audit <- function(family = "fixed_design", target = "RQR",
                                   chain = 1L) {
  state <- valid_provenance_state()
  do.call(rbind, lapply(
    c("worker_entry", "fit_recorded", "worker_exit"),
    function(phase) otv3_provenance_snapshot(
      state, phase, family, target, chain
    )
  ))
}

testthat::test_that("v3 freezes the 0.80/95-percent exact-oracle contract", {
  config <- read_config()
  testthat::expect_invisible(otv3_validate_config(config))
  plan <- otv3_plan(config)
  testthat::expect_equal(nrow(plan), 27L)
  testthat::expect_equal(
    unname(as.integer(table(plan$family))), c(15L, 12L)
  )
  testthat::expect_equal(length(unique(plan$seed)), 27L)
  testthat::expect_false(any(plan$cornish_fisher_used))
  testthat::expect_identical(
    config$fixed_design$mcmc_control$kernel_repetitions, 2L
  )
  testthat::expect_identical(
    as.character(unlist(config$fixed_design$initial_profiles)),
    c("moment_centered", "moment_narrow", "moment_wide",
      "moment_shape_stress")
  )
  testthat::expect_type(config$execution_authorized, "logical")
  testthat::expect_length(config$execution_authorized, 1L)

  law <- otv3_law(config)
  testthat::expect_equal(law$raw_mean, -3.75, tolerance = 1e-12)
  testthat::expect_equal(
    law$raw_sd, 5.153882032022076, tolerance = 1e-12
  )
  testthat::expect_equal(
    otv3_al_skewness(0.8), -1.7976169855634085, tolerance = 1e-12
  )
  oracle <- oti_oracle_targets(law, 0.95, c("RQR", "ET", "SH"))
  reproduced_content <- law$p(oracle$upper_innovation) -
    law$p(oracle$lower_innovation)
  testthat::expect_equal(reproduced_content, rep(0.95, 3L), tolerance = 1e-10)
  testthat::expect_equal(
    oracle$retained_mean_innovation, oracle$delta_innovation,
    tolerance = 1e-10
  )
  testthat::expect_lt(abs(oracle$delta_innovation[oracle$target == "RQR"]),
                      1e-10)
  testthat::expect_lt(abs(
    law$d(oracle$lower_innovation[oracle$target == "SH"]) -
      law$d(oracle$upper_innovation[oracle$target == "SH"])
  ), 1e-8)
})

testthat::test_that("config validation rejects silent design drift", {
  config <- read_config()
  bad <- config
  bad$innovation$tau <- 0.99
  testthat::expect_error(otv3_validate_config(bad), "AL_0.80")
  bad <- config
  bad$fixed_design$mcmc_control$n_mcmc <- 6000.5
  testthat::expect_error(otv3_validate_config(bad), "finite integer")
  bad <- config
  bad$fixed_design$mcmc_control$kernel_repetitions <- 1L
  testthat::expect_error(otv3_validate_config(bad), "MCMC contract")
  bad <- config
  bad$fixed_design$initialization_contract$width_multipliers[[2L]] <- 0.6
  testthat::expect_error(
    otv3_validate_config(bad), "initialization contract"
  )
  bad <- config
  bad$dlm$mcmc_control$store_state_draws <- TRUE
  testthat::expect_error(otv3_validate_config(bad), "storage contract")
  bad <- config
  bad$fixed_design$basis_contract$scale_control_coefficients[[1L]] <- -0.1
  testthat::expect_error(otv3_validate_config(bad), "scale coefficients")
  bad <- config
  bad$dlm$seasonal_contract$period <- 240L
  testthat::expect_error(otv3_validate_config(bad), "seasonal")
  bad <- config
  bad$diagnostics$rhat_max <- 1.05
  testthat::expect_error(otv3_validate_config(bad), "diagnostic contract")
  bad <- config
  bad$recovery_gates$endpoint_summary_joint_inclusion_role <- "strict_gate"
  testthat::expect_error(
    otv3_validate_config(bad), "descriptive-only"
  )
  bad <- config
  bad$resources$maximum_threads <- 4L
  testthat::expect_error(otv3_validate_config(bad), "resource contract")
})

testthat::test_that("cubic spline basis is orthogonal and truth-exact", {
  preflight <- otv3_design_preflight(read_config())
  basis <- preflight$fixed_dgp$basis
  testthat::expect_equal(length(preflight$fixed_dgp$y), 2400L)
  testthat::expect_equal(basis$rank, 8L)
  testthat::expect_equal(
    unname(basis$gram), diag(8L), tolerance = 1e-12
  )
  testthat::expect_lt(basis$maximum_reconstruction_error, 1e-12)
  testthat::expect_true(all(
    preflight$projection_audit$max_absolute_residual < 1e-12
  ))
  selected <- preflight$static_prior_audit$selected
  testthat::expect_equal(sum(selected), 1L)
  testthat::expect_equal(
    preflight$static_prior_audit$tau2[selected], 0.25,
    tolerance = 1e-12
  )
  testthat::expect_gte(
    preflight$static_prior_audit$center_half_width[selected], 2.0
  )
  testthat::expect_lte(
    preflight$static_prior_audit$maximum_half_width[selected], 7.5
  )
  testthat::expect_gte(min(preflight$fixed_dgp$scale_truth), 0.35)
  testthat::expect_true(
    preflight$fixed_dgp$scale_ratio >= 2 &&
      preflight$fixed_dgp$scale_ratio <= 2.5
  )
})

testthat::test_that("static moment starts are data-derived, distinct, and admissible", {
  preflight <- otv3_design_preflight(read_config())
  testthat::expect_equal(
    preflight$oracle$delta_innovation[
      preflight$oracle$target == "RQR"
    ],
    0
  )
  testthat::expect_equal(
    unique(oti_target_row(preflight$fixed_targets, "RQR")$mean_tilt),
    0
  )
  audit <- preflight$fixed_initialization_audit
  testthat::expect_equal(nrow(audit), 12L)
  testthat::expect_equal(length(unique(audit$initialization_digest)), 12L)
  testthat::expect_true(all(audit$minimum_width >= 0.05))
  testthat::expect_false(any(audit$target_changed))
  testthat::expect_equal(
    unique(audit$absolute_moment), 0.7332205895378953,
    tolerance = 1e-10
  )
  for (target in c("RQR", "ET", "SH")) {
    profiles <- preflight$fixed_initializations[[target]]$profiles
    testthat::expect_identical(
      names(profiles),
      c("moment_centered", "moment_narrow", "moment_wide",
        "moment_shape_stress")
    )
    testthat::expect_true(all(vapply(
      profiles,
      function(initialization) {
        eta1 <- drop(preflight$fixed_dgp$X %*%
          initialization$beta_root1)
        eta2 <- drop(preflight$fixed_dgp$X %*%
          initialization$beta_root2)
        min(abs(eta2 - eta1)) >= 0.05
      },
      logical(1L)
    )))
  }
})

testthat::test_that("seasonal DLM preserves the physical and harmonic contracts", {
  config <- read_config()
  preflight <- otv3_design_preflight(config)
  dgp <- preflight$dlm_dgp
  testthat::expect_equal(length(dgp$time), 1200L)
  testthat::expect_equal(range(dgp$time), c(1 / 1200, 1))
  testthat::expect_equal(sum(!dgp$observed), 22L)
  testthat::expect_equal(sum(dgp$observed), 1178L)
  for (target in c("RQR", "ET", "SH")) {
    truth <- oti_target_row(preflight$dlm_targets, target)
    testthat::expect_identical(is.na(truth$mean_tilt), is.na(dgp$y))
  }
  testthat::expect_equal(length(dgp$model$m0), 4L)
  testthat::expect_equal(dgp$model$component_dims, c(2L, 2L))
  testthat::expect_equal(
    dgp$model$component_names, c("local_linear", "seasonal_harmonic")
  )
  testthat::expect_equal(
    max(preflight$fixed_horizon_audit$max_absolute_error), 0,
    tolerance = 1e-10
  )
  selected <- preflight$dlm_prior_audit$selected
  testthat::expect_equal(sum(selected), 1L)
  testthat::expect_equal(
    preflight$dlm_prior_audit[selected, c(
      "initial_level_variance", "initial_slope_variance",
      "q_level", "q_slope"
    )],
    data.frame(
      initial_level_variance = 1, initial_slope_variance = 0.25,
      q_level = 0.04, q_slope = 0.09
    ),
    ignore_attr = TRUE
  )
  matrices <- otv3_local_linear_matrices(1 / 1200, 0.04, 0.09)
  testthat::expect_equal(dgp$model$GG[1:2, 1:2, 1L], matrices$G)
  testthat::expect_equal(dgp$W[1:2, 1:2, 1200L], matrices$W)
  expected_seasonal_W <- array(
    rep(diag(1e-6, 2L), 1200L), c(2L, 2L, 1200L)
  )
  testthat::expect_equal(dgp$W[3:4, 3:4, ], expected_seasonal_W)
  testthat::expect_equal(matrices$W, t(matrices$W), tolerance = 1e-15)
  testthat::expect_true(min(eigen(matrices$W, symmetric = TRUE)$values) > 0)
  testthat::expect_equal(preflight$observability_audit$rank, 4L)
  testthat::expect_lt(
    preflight$seasonal_covariance_audit$
      maximum_covariance_recursion_error, 1e-10
  )
  testthat::expect_true(all(
    preflight$dynamic_projection_audit$max_absolute_residual < 1e-10
  ))
  testthat::expect_equal(dgp$seasonal_period, 300L)
  testthat::expect_equal(1200 / dgp$seasonal_period, 4)
  testthat::expect_true(
    dgp$scale_ratio >= 2 && dgp$scale_ratio <= 2.6
  )
})

testthat::test_that("all exact-oracle cells have adequate rare-tail counts", {
  preflight <- otv3_design_preflight(read_config())
  tail <- preflight$tail_information
  testthat::expect_equal(tail$target, c("RQR", "ET", "SH"))
  testthat::expect_true(all(tail$fixed_design_expected_rare_count >= 10))
  testthat::expect_true(all(tail$dlm_expected_rare_count >= 10))
  testthat::expect_equal(
    min(tail$dlm_expected_rare_count), 11.7800010264016,
    tolerance = 1e-10
  )
  testthat::expect_gte(
    min(preflight$scale_information$expected_rare_tail_count), 2
  )
})

testthat::test_that("DGPs and overdispersed paths are deterministic", {
  config <- read_config()
  first <- otv3_design_preflight(config)
  second <- otv3_design_preflight(config)
  testthat::expect_identical(first$fixed_dgp$y, second$fixed_dgp$y)
  testthat::expect_identical(first$dlm_dgp$y, second$dlm_dgp$y)
  truth <- oti_target_row(first$dlm_targets, "SH")
  centered <- otv3_initial_state_paths(
    "oracle_centered", first$dlm_dgp, truth
  )
  stressed <- otv3_initial_state_paths(
    "trend_seasonal_stress", first$dlm_dgp, truth
  )
  testthat::expect_equal(dim(centered$state_root1), c(4L, 1200L))
  testthat::expect_equal(length(centered$theta0_root1), 4L)
  testthat::expect_false(identical(
    stressed$state_root2[2L, ], centered$state_root2[2L, ]
  ))
  testthat::expect_false(identical(
    stressed$state_root2[3L, ], centered$state_root2[3L, ]
  ))
  expanded <- otf_expanded_dlm(first$dlm_dgp)
  testthat::expect_equal(
    colSums(expanded$FF * centered$state_root1), truth$oracle_lower,
    tolerance = 1e-10
  )
  testthat::expect_equal(
    colSums(expanded$FF * centered$state_root2), truth$oracle_upper,
    tolerance = 1e-10
  )
})

testthat::test_that("chain batches enforce the worker ceiling deterministically", {
  testthat::expect_identical(
    unname(otv3_chain_batches(1:4, 2L)), list(1:2, 3:4)
  )
  testthat::expect_identical(
    unname(otv3_chain_batches(1:5, 2L)), list(1:2, 3:4, 5L)
  )
  testthat::expect_identical(
    unname(otv3_chain_batches(1:4, 5L)), list(1:4)
  )
  testthat::expect_error(otv3_chain_batches(c(1L, 1L), 2L), "unique")
})

testthat::test_that("the fit plan is the single authoritative cell order", {
  plan <- otv3_plan(read_config())
  cells <- otv3_cell_plan(plan)
  expected <- plan[!duplicated(paste(plan$family, plan$target)),
                   c("family", "target")]
  rownames(expected) <- NULL
  testthat::expect_identical(cells[, c("family", "target")], expected)
  testthat::expect_identical(cells$order, 1:6)
  testthat::expect_true(all(cells$process_isolation))
  testthat::expect_true(all(cells$maximum_chain_workers == 2L))
  bad <- plan[plan$target != "SH", , drop = FALSE]
  testthat::expect_error(otv3_cell_plan(bad), "six unique cells")
})

testthat::test_that("failure ledgers handle first and heterogeneous rows", {
  root <- tempfile("otv3-failure-ledger-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  path <- file.path(root, "failure_log.csv")
  first <- otv3_append_failure(
    path, "fixed_design", "RQR", NA_integer_, "cell_gate",
    "first, quoted failure"
  )
  testthat::expect_equal(nrow(first), 1L)
  testthat::expect_identical(first$sequence, 1L)
  testthat::expect_identical(first$schema_version, otv3_failure_schema())
  second <- otv3_append_failure(
    path, "dlm", "ET", 3L, "fit", "second failure"
  )
  testthat::expect_equal(nrow(second), 2L)
  testthat::expect_identical(second$sequence, 1:2)
  testthat::expect_identical(second$chain, c(NA_integer_, 3L))
  reread <- utils::read.csv(path, stringsAsFactors = FALSE)
  testthat::expect_identical(reread$message, second$message)
  bad <- reread
  bad$sequence[2L] <- NA
  otf_atomic_write_csv(bad, path)
  testthat::expect_error(
    otv3_append_failure(path, "dlm", "SH", 1L, "fit", "third"),
    "invalid sequence"
  )
})

testthat::test_that("provenance audits expose phase-specific failures", {
  audit <- valid_provenance_audit()
  testthat::expect_invisible(
    otv3_validate_provenance_audit(audit, "fixed_design", "RQR", 1L)
  )
  testthat::expect_true(all(audit$snapshot_pass))
  failed_state <- valid_provenance_state()
  failed_state$runtime_attestation_match <- FALSE
  failed <- otv3_provenance_snapshot(
    failed_state, "worker_exit", "fixed_design", "RQR", 1L
  )
  testthat::expect_false(failed$snapshot_pass)
  testthat::expect_match(
    failed$failed_gates, "runtime_attestation_match", fixed = TRUE
  )
  malformed <- audit[c(1L, 3L, 2L), ]
  testthat::expect_error(
    otv3_validate_provenance_audit(malformed), "invalid"
  )
  mismatched <- audit
  mismatched$runtime_package_tree_digest[2L] <- strrep("d", 64L)
  testthat::expect_error(
    otv3_validate_provenance_audit(mismatched), "invalid"
  )
  testthat::expect_true("provenance_audit.csv" %in% otv3_compact_files())
})

testthat::test_that("preflight and independent conditional references pass", {
  config <- read_config()
  preflight <- otv3_design_preflight(config)
  testthat::expect_equal(nrow(preflight$gates), 17L)
  testthat::expect_true(all(preflight$gates$pass))
  references <- otv3_reference_suite(config)
  testthat::expect_equal(nrow(references), 24L)
  testthat::expect_true(all(references$pass))
  testthat::expect_equal(
    references$value[references$gate == "dlm_R_repair_count"], 0
  )
  testthat::expect_equal(
    references$value[references$gate == "dlm_cpp_repair_count"], 0
  )
  testthat::expect_equal(
    references$value[references$gate == "seasonal_cpp_repair_count"], 0
  )
  testthat::expect_equal(
    references$value[
      references$gate == "seasonal_cpp_sample_finite_zero_repair"
    ], 1
  )
})

testthat::test_that("recovery gates distinguish adequate and poor fits", {
  config <- read_config()
  x <- seq(-1, 1, length.out = 101L)
  curves <- data.frame(
    x = x, oracle_lower = -1 + 0.2 * x, oracle_upper = 1 + 0.2 * x,
    fit_lower = -1 + 0.2 * x, fit_upper = 1 + 0.2 * x,
    fit_lower_q025 = -1.1 + 0.2 * x,
    fit_lower_q975 = -0.9 + 0.2 * x,
    fit_upper_q025 = 0.9 + 0.2 * x,
    fit_upper_q975 = 1.1 + 0.2 * x
  )
  metrics <- data.frame(oracle_mean_width = 2, mean_width = 2)
  good <- otv3_recovery_summary("fixed_design", curves, metrics, config)
  testthat::expect_equal(good$endpoint_rmse_over_oracle_width, 0)
  testthat::expect_equal(good$mean_width_ratio, 1)
  low_inclusion <- good
  low_inclusion$endpoint_summary_joint_inclusion <- 0
  low_inclusion$static_edge_center_rmse_ratio <- 1
  testthat::expect_true(otv3_recovery_gate_pass(
    "fixed_design", low_inclusion, config$recovery_gates
  ))
  curves$fit_lower <- curves$fit_lower - 1
  bad <- otv3_recovery_summary("fixed_design", curves, metrics, config)
  testthat::expect_gt(
    bad$endpoint_rmse_over_oracle_width,
    config$recovery_gates$endpoint_rmse_over_oracle_width_max
  )
  testthat::expect_false(otv3_recovery_gate_pass(
    "fixed_design", bad, config$recovery_gates
  ))
})

testthat::test_that("heterogeneity diagnostics recover scale contrast and phase", {
  n <- 120L
  index <- seq_len(n)
  period <- 30L
  scale <- 0.55 + 0.22 * cos(2 * pi * index / period - 0.50)
  oracle_width <- 3 * scale
  oracle_lower <- -0.5 * oracle_width
  oracle_upper <- 0.5 * oracle_width
  curves <- data.frame(
    index = index, oracle_lower = oracle_lower,
    oracle_upper = oracle_upper, fit_lower = oracle_lower,
    fit_upper = oracle_upper, fit_width = oracle_width
  )
  truth <- data.frame(oracle_width = oracle_width)
  dgp <- list(
    family = "dlm", scale_truth = scale,
    seasonal_harmonic = 1L, seasonal_period = period
  )
  exact <- otv3_heterogeneity_summary("dlm", curves, truth, dgp)
  testthat::expect_equal(exact$width_contrast_relative_error, 0,
                         tolerance = 1e-12)
  testthat::expect_equal(exact$seasonal_width_amplitude_ratio, 1,
                         tolerance = 1e-12)
  testthat::expect_equal(exact$seasonal_width_phase_error, 0,
                         tolerance = 1e-12)

  flat <- curves
  flat$fit_lower <- -mean(oracle_width) / 2
  flat$fit_upper <- mean(oracle_width) / 2
  flat$fit_width <- mean(oracle_width)
  poor <- otv3_heterogeneity_summary("dlm", flat, truth, dgp)
  testthat::expect_gt(poor$width_contrast_relative_error, 0.5)
  testthat::expect_lt(poor$seasonal_width_amplitude_ratio, 0.1)
})

testthat::test_that("benchmark assessment rejects gross recovery failure", {
  config <- read_config()
  preflight <- otv3_design_preflight(config)
  truth <- oti_target_row(preflight$fixed_targets, "SH")
  n_draw <- 20L
  exact_lower <- matrix(truth$oracle_lower, nrow = length(truth$oracle_lower),
                        ncol = n_draw)
  exact_upper <- matrix(truth$oracle_upper, nrow = length(truth$oracle_upper),
                        ncol = n_draw)
  exact_pred <- list(
    lower_draws = exact_lower,
    upper_draws = exact_upper,
    midpoint_draws = 0.5 * (exact_lower + exact_upper),
    width_draws = exact_upper - exact_lower,
    lower_mean = rowMeans(exact_lower),
    upper_mean = rowMeans(exact_upper),
    midpoint_mean = rowMeans(0.5 * (exact_lower + exact_upper)),
    width_mean = rowMeans(exact_upper - exact_lower)
  )
  good <- otv3_benchmark_assessment(
    "fixed_design", "SH", list(pred = exact_pred), preflight$fixed_dgp,
    preflight$fixed_targets, config
  )
  testthat::expect_true(good$gross_recovery_pass)
  testthat::expect_true(good$pathology_pass)

  shifted <- exact_pred
  shifted$lower_draws <- shifted$lower_draws - 2 *
    mean(truth$oracle_width)
  shifted$lower_mean <- rowMeans(shifted$lower_draws)
  shifted$midpoint_draws <- 0.5 *
    (shifted$lower_draws + shifted$upper_draws)
  shifted$midpoint_mean <- rowMeans(shifted$midpoint_draws)
  shifted$width_draws <- shifted$upper_draws - shifted$lower_draws
  shifted$width_mean <- rowMeans(shifted$width_draws)
  bad <- otv3_benchmark_assessment(
    "fixed_design", "SH", list(pred = shifted), preflight$fixed_dgp,
    preflight$fixed_targets, config
  )
  testthat::expect_false(bad$gross_recovery_pass)
})

testthat::test_that("two complete static transitions preserve a tilted target", {
  x <- seq(-1, 1, length.out = 18L)
  X <- cbind(intercept = 1, x = x)
  y <- 0.2 + 0.4 * x + seq(-0.15, 0.15, length.out = length(x))
  fit <- rqrgibbs::rqr_mcmc_fit(
    y = y, X = X, coverage_level = 0.8, learning_rate = 1,
    learning_rate_mode = "fixed_rate", mean_tilt = rep(0.15, length(y)),
    beta_prior_obj = rqrgibbs::beta_prior(
      "ridge", ridge = list(tau2 = 2)
    ),
    numerical_policy = "fail",
    mcmc_control = list(
      n_burn = 3L, n_mcmc = 8L, thin = 1L, seed = 202608019L,
      kernel_repetitions = 2L, store_latent_draws = FALSE
    )
  )
  testthat::expect_identical(fit$model_spec$kernel_repetitions, 2L)
  testthat::expect_identical(
    fit$model_spec$mean_tilt_summary$nonzero_count, length(y)
  )
  testthat::expect_true(fit$model_spec$exact_joint_target)
  testthat::expect_equal(fit$model_spec$numerical_repair_count, 0L)
  testthat::expect_true(all(
    fit$diagnostics$root_swap_count_trace %in% 0:2
  ))
})

testthat::test_that("endpoint-only storage reconstructs redundant predictions exactly", {
  lower <- matrix(seq(-2, -0.1, length.out = 60L), 10L, 6L)
  upper <- lower + matrix(seq(0.5, 1.5, length.out = 60L), 10L, 6L)
  full <- list(
    lower_draws = lower, upper_draws = upper,
    midpoint_draws = 0.5 * (lower + upper),
    width_draws = upper - lower
  )
  compact <- otv3_endpoint_only_prediction(full)
  testthat::expect_true(otv3_prediction_storage_contract(compact))
  testthat::expect_false(any(
    c("midpoint_draws", "width_draws") %in% names(compact)
  ))
  restored <- otv3_materialize_prediction(compact)
  testthat::expect_identical(restored$lower_draws, lower)
  testthat::expect_identical(restored$upper_draws, upper)
  testthat::expect_identical(restored$midpoint_draws, full$midpoint_draws)
  testthat::expect_identical(restored$width_draws, full$width_draws)
  testthat::expect_identical(
    restored$midpoint_mean, rowMeans(full$midpoint_draws)
  )
  testthat::expect_identical(
    restored$width_mean, rowMeans(full$width_draws)
  )
  combined <- otv3_combine_endpoint_predictions(list(compact, compact))
  testthat::expect_identical(
    combined$lower_draws, cbind(lower, lower)
  )
  testthat::expect_identical(
    combined$upper_draws, cbind(upper, upper)
  )
  testthat::expect_identical(
    combined$midpoint_draws, cbind(full$midpoint_draws, full$midpoint_draws)
  )
})

testthat::test_that("endpoint-only workers reject contract and payload drift", {
  prediction <- otv3_endpoint_only_prediction(list(
    lower_draws = matrix(-1, 3L, 4L),
    upper_draws = matrix(1, 3L, 4L)
  ))
  contract <- list(
    schema_version = otv3_worker_schema(), source_commit = strrep("a", 40L),
    family = "fixed_design", target = "RQR", chain = 1L
  )
  envelope <- list(
    schema_version = otv3_worker_schema(), contract = contract,
    contract_digest = otf_object_sha256(contract),
    result = list(
      pred = prediction,
      provenance_audit = valid_provenance_audit(),
      chain_summary = data.frame(
        worker_entry_provenance_match = TRUE,
        fit_recorded_provenance_match = TRUE,
        worker_exit_provenance_match = TRUE
      )
    )
  )
  testthat::expect_invisible(
    otv3_validate_worker_artifact(envelope, contract)
  )
  bad <- envelope
  bad$contract$chain <- 2L
  testthat::expect_error(
    otv3_validate_worker_artifact(bad, contract), "invalid"
  )
  bad <- envelope
  bad$result$pred$midpoint_draws <- matrix(0, 3L, 4L)
  testthat::expect_error(
    otv3_validate_worker_artifact(bad, contract), "invalid"
  )
  bad <- envelope
  bad$result$pred$upper_draws[1L, 1L] <- -2
  testthat::expect_error(
    otv3_validate_worker_artifact(bad, contract), "invalid"
  )
  bad <- envelope
  bad$result$provenance_audit$phase[2L] <- "worker_exit"
  testthat::expect_error(
    otv3_validate_worker_artifact(bad, contract), "invalid"
  )
})

testthat::test_that("cell contracts bind every ordered worker artifact", {
  manifest <- data.frame(
    chain = 1:4,
    sha256 = vapply(1:4, function(index) strrep(index, 64L), character(1L)),
    contract_digest = vapply(
      5:8, function(index) strrep(index, 64L), character(1L)
    ),
    stringsAsFactors = FALSE
  )
  contract <- otv3_build_cell_contract(
    strrep("a", 40L), strrep("b", 64L), strrep("c", 64L),
    "fixed_design", "RQR", manifest
  )
  testthat::expect_identical(contract$schema_version, otv3_cell_schema())
  testthat::expect_identical(contract$chains, 1:4)
  testthat::expect_identical(
    contract$prediction_storage_contract, "ordered_endpoints_only"
  )
  changed <- manifest
  changed$sha256[1L] <- strrep("9", 64L)
  testthat::expect_false(identical(
    otf_object_sha256(contract),
    otf_object_sha256(otv3_build_cell_contract(
      strrep("a", 40L), strrep("b", 64L), strrep("c", 64L),
      "fixed_design", "RQR", changed
    ))
  ))
  duplicate <- manifest
  duplicate$chain[2L] <- 1L
  testthat::expect_error(
    otv3_build_cell_contract(
      strrep("a", 40L), strrep("b", 64L), strrep("c", 64L),
      "fixed_design", "RQR", duplicate
    ),
    "cannot define"
  )
})

testthat::test_that("execute is wired through a fresh-process cell orchestrator", {
  wrapper <- readLines(file.path(
    repo_root, "application", "scripts",
    "43_run_oracle_tilt_publication_v3.sh"
  ))
  orchestrator <- readLines(file.path(
    repo_root, "application", "scripts",
    "44_orchestrate_oracle_tilt_v3_execute.sh"
  ))
  testthat::expect_true(any(grepl(
    "44_orchestrate_oracle_tilt_v3_execute.sh", wrapper, fixed = TRUE
  )))
  testthat::expect_equal(sum(grepl("run_stage cell", orchestrator, fixed = TRUE)), 1L)
  testthat::expect_true(any(grepl(
    "cell_plan.csv", orchestrator, fixed = TRUE
  )))
  testthat::expect_true(any(grepl(
    "planned_cells", orchestrator, fixed = TRUE
  )))
  testthat::expect_false(any(grepl(
    "for family in fixed_design dlm", orchestrator, fixed = TRUE
  )))
  testthat::expect_true(any(grepl(
    "post_stage_r_processes", orchestrator, fixed = TRUE
  )))
  testthat::expect_true(any(grepl(
    "process-isolated stage failed", orchestrator, fixed = TRUE
  )))
  acceptance <- readLines(file.path(
    repo_root, "application", "scripts",
    "45_launch_oracle_tilt_v3_acceptance.sh"
  ))
  testthat::expect_true(any(grepl(
    "RQR_ORACLE_TILT_V3_ACCEPTANCE_CONFIRM", acceptance, fixed = TRUE
  )))
  testthat::expect_true(any(grepl(
    "MemoryMax=14G", acceptance, fixed = TRUE
  )))
})

testthat::test_that("resource rehearsal uses two clean sequential R processes", {
  root <- tempfile("otv3-rehearsal-")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  script <- file.path(
    repo_root, "application", "scripts",
    "44_run_oracle_tilt_v3_resource_rehearsal.sh"
  )
  output <- system2(
    "setsid", c("bash", script, paste0("--output-dir=", root)),
    stdout = TRUE, stderr = TRUE,
    env = c(
      "RQR_ORACLE_TILT_V3_REHEARSAL_N_INDEX=20",
      "RQR_ORACLE_TILT_V3_REHEARSAL_N_DRAW=30",
      "RQR_ORACLE_TILT_V3_REHEARSAL_N_CHAIN=2"
    )
  )
  testthat::expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
  summary <- utils::read.csv(
    file.path(root, "rehearsal_summary.csv"), stringsAsFactors = FALSE
  )
  testthat::expect_equal(nrow(summary), 2L)
  testthat::expect_equal(length(unique(summary$pid)), 2L)
  testthat::expect_true(all(summary$post_stage_r_processes == 0L))
  testthat::expect_true(all(summary$pass))
  otp_verify_manifest(root)
})

testthat::test_that("resource rehearsal separates immutable and live wrapper evidence", {
  rehearsal_script <- readLines(
    file.path(
      repo_root, "application", "scripts",
      "44_run_oracle_tilt_v3_resource_rehearsal.sh"
    ),
    warn = FALSE
  )
  joined <- paste(rehearsal_script, collapse = "\n")
  testthat::expect_match(joined, "wrapper_owned <- c", fixed = TRUE)
  for (name in c(
    "process_group_monitor.csv", "runner.stdout.log", "runner.stderr.log",
    "resource_summary.csv", "wrapper_closeout.csv",
    "wrapper_artifact_manifest.csv", "wrapper_failure_log.csv"
  )) {
    testthat::expect_match(joined, name, fixed = TRUE)
  }
  testthat::expect_match(
    joined, "!relative %in% wrapper_owned", fixed = TRUE
  )
})
