testthat::local_edition(3)

.source_forensic_helpers <- function() {
  source(testthat::test_path(
    "..", "..", "scripts", "32_oracle_tilt_illustration_utils.R"
  ))
  source(testthat::test_path(
    "..", "..", "scripts", "33_oracle_tilt_forensic_utils.R"
  ))
}

testthat::test_that("high-content population geometry identifies the SH boundary margin", {
  .source_forensic_helpers()
  law <- oti_al_law(tau = 0.99, standardized = TRUE)
  oracle <- oti_oracle_targets(law, coverage_level = 0.95)
  geometry <- otf_tilt_geometry(
    law, 0.95, oracle, caution_fraction = 0.02
  )

  testthat::expect_setequal(geometry$target, c("RQR", "ET", "SH"))
  testthat::expect_true(
    geometry$near_population_boundary[geometry$target == "SH"]
  )
  testthat::expect_false(
    geometry$near_population_boundary[geometry$target == "RQR"]
  )
  testthat::expect_lt(
    geometry$upper_margin_fraction[geometry$target == "SH"],
    0.01
  )
  testthat::expect_true(all(
    geometry$delta >= geometry$delta_minus - 1e-10 &
      geometry$delta <= geometry$delta_plus + 1e-10
  ))
})

testthat::test_that("dynamic prior canonical shift matches a dense Gaussian calculation", {
  .source_forensic_helpers()
  config <- list(
    coverage_level = 0.95,
    dlm = list(
      T = 20L, seed = 31L, missing_times = c(5L, 12L),
      scale = 0.55, state_sd_level = 0.035, state_sd_slope = 0.006
    )
  )
  law <- oti_al_law(tau = 0.99, standardized = TRUE)
  dgp <- oti_dlm_dgp(config, law)
  oracle <- oti_oracle_targets(law, 0.95)
  targets <- oti_targets_by_index(
    dgp$mean_truth, dgp$scale_truth, oracle, dgp$observed
  )
  truth <- oti_target_row(targets, "ET")
  got <- otf_prior_canonical_shift(dgp, truth$mean_tilt, 0.95)
  expanded <- got$expanded
  dense <- otf_dense_gaussian_reference(
    z = rep(NA_real_, expanded$n_time),
    H = matrix(0, expanded$p, expanded$n_time),
    V = rep(1, expanded$n_time),
    expanded = expanded,
    canonical_shift = got$canonical_shift
  )
  dense_ordinate <- colSums(expanded$FF * dense$mean)

  testthat::expect_equal(
    got$state_shift, dense$mean, tolerance = 1e-8
  )
  testthat::expect_equal(
    got$ordinate_shift, dense_ordinate, tolerance = 1e-8
  )
  testthat::expect_true(all(
    got$canonical_shift[, !dgp$observed, drop = FALSE] == 0
  ))
})

testthat::test_that("forensic initial profiles are finite and genuinely dispersed", {
  .source_forensic_helpers()
  config <- list(
    coverage_level = 0.95,
    dlm = list(T = 20L, seed = 32L, missing_times = 7L)
  )
  law <- oti_al_law(tau = 0.99, standardized = TRUE)
  dgp <- oti_dlm_dgp(config, law)
  oracle <- oti_oracle_targets(law, 0.95)
  targets <- oti_targets_by_index(
    dgp$mean_truth, dgp$scale_truth, oracle, dgp$observed
  )
  truth <- oti_target_row(targets, "SH")
  profiles <- lapply(
    c("oracle", "narrow", "wide"),
    otf_initial_state_paths,
    dgp = dgp,
    truth = truth
  )
  widths <- vapply(profiles, function(x) {
    mean(x$state_root2[1L, ] - x$state_root1[1L, ])
  }, numeric(1L))

  testthat::expect_true(all(vapply(
    profiles,
    function(x) all(is.finite(c(
      x$state_root1, x$state_root2,
      x$theta0_root1, x$theta0_root2
    ))),
    logical(1L)
  )))
  testthat::expect_true(widths[2L] < widths[1L])
  testthat::expect_true(widths[1L] < widths[3L])
  testthat::expect_identical(
    otf_initial_state_paths("default", dgp, truth), list()
  )
  stress <- otf_initial_state_paths(
    "prior_shift_stress", dgp, truth, coverage_level = 0.95
  )
  testthat::expect_gt(
    max(abs(stress$state_root2)),
    max(abs(profiles[[1L]]$state_root2))
  )
  testthat::expect_error(
    otf_initial_state_paths("prior_shift_stress", dgp, truth),
    "coverage_level is required",
    fixed = TRUE
  )
})

testthat::test_that("the illustrative DLM exposes a configurable proper initial prior", {
  .source_forensic_helpers()
  config <- list(
    dlm = list(
      T = 20L,
      seed = 34L,
      initial_level_variance = 2,
      initial_slope_variance = 0.01
    )
  )
  law <- oti_al_law(tau = 0.99, standardized = TRUE)
  dgp <- oti_dlm_dgp(config, law)
  expanded <- otf_expanded_dlm(dgp)
  changed <- otf_dlm_with_initial_prior(dgp, 3, 0.1)

  testthat::expect_equal(diag(expanded$C0), c(2, 0.01))
  testthat::expect_equal(
    diag(otf_expanded_dlm(changed)$C0), c(3, 0.1)
  )
  testthat::expect_equal(changed$W, dgp$W)
  testthat::expect_equal(changed$y, dgp$y)
})

testthat::test_that("vectorized state-prior quadratics equal the scalar definition", {
  .source_forensic_helpers()
  config <- list(dlm = list(T = 20L, seed = 35L))
  law <- oti_al_law(tau = 0.99, standardized = TRUE)
  expanded <- otf_expanded_dlm(oti_dlm_dgp(config, law))
  set.seed(351L)
  theta0 <- matrix(stats::rnorm(expanded$p * 3L), expanded$p, 3L)
  theta <- array(
    stats::rnorm(expanded$p * expanded$n_time * 3L),
    c(expanded$p, expanded$n_time, 3L)
  )
  got <- otf_root_prior_quadratic(theta, theta0, expanded)
  expected <- vapply(seq_len(3L), function(jj) {
    d0 <- theta0[, jj] - expanded$m0
    value <- drop(crossprod(d0, solve(expanded$C0, d0)))
    previous <- theta0[, jj]
    for (tt in seq_len(expanded$n_time)) {
      innovation <-
        theta[, tt, jj] - expanded$GG[, , tt] %*% previous
      value <- value + drop(crossprod(
        innovation,
        solve(expanded$W[, , tt], innovation)
      ))
      previous <- theta[, tt, jj]
    }
    value
  }, numeric(1L))

  testthat::expect_equal(got, expected, tolerance = 1e-10)
})

testthat::test_that("worker checkpoints are atomic, hashed, and overwrite safely", {
  .source_forensic_helpers()
  root <- tempfile("rqr_forensic_atomic_")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  rds_path <- file.path(root, "worker.rds")
  csv_path <- file.path(root, "trace.csv")
  first <- list(contract = "first", value = 1:3)
  second <- list(contract = "second", value = 4:6)
  trace <- data.frame(iteration = 1:3, width = c(1.2, 1.3, 1.4))

  testthat::expect_identical(
    otf_atomic_save_rds(first, rds_path), rds_path
  )
  first_digest <- oti_file_sha256(rds_path)
  testthat::expect_identical(readRDS(rds_path), first)

  testthat::expect_identical(
    otf_atomic_save_rds(second, rds_path), rds_path
  )
  testthat::expect_identical(readRDS(rds_path), second)
  testthat::expect_false(identical(
    oti_file_sha256(rds_path), first_digest
  ))

  testthat::expect_identical(
    otf_atomic_write_csv(trace, csv_path), csv_path
  )
  testthat::expect_equal(
    utils::read.csv(csv_path), trace, tolerance = 1e-12
  )
  remaining <- setdiff(
    list.files(root, all.files = TRUE), c(".", "..")
  )
  testthat::expect_false(any(startsWith(remaining, ".")))
})

testthat::test_that("trace decomposition and conditional references match the declared target", {
  .source_forensic_helpers()
  config <- list(
    coverage_level = 0.95,
    dlm = list(T = 20L, seed = 33L, missing_times = c(6L, 13L))
  )
  law <- oti_al_law(tau = 0.99, standardized = TRUE)
  dgp <- oti_dlm_dgp(config, law)
  oracle <- oti_oracle_targets(law, 0.95)
  targets <- oti_targets_by_index(
    dgp$mean_truth, dgp$scale_truth, oracle, dgp$observed
  )
  truth <- oti_target_row(targets, "ET")
  fit <- rqrgibbs::rqr_dlm_fit(
    y = dgp$y,
    model = dgp$model,
    coverage_level = 0.95,
    evolution_mode = "fixed_W",
    W = dgp$W,
    learning_rate = 1,
    learning_rate_mode = "fixed_rate",
    mean_tilt = truth$mean_tilt,
    numerical_policy = "fail",
    mcmc_control = list(
      n_burn = 3L, n_mcmc = 5L, thin = 1L, seed = 331L,
      backend = "R", store_state_draws = TRUE, store_latent_draws = TRUE
    ),
    init = otf_initial_state_paths("oracle", dgp, truth)
  )
  trace <- otf_dlm_trace_frame(
    fit, dgp, truth, "ET", 1L, "oracle", 0.95
  )
  reference <- otf_conditional_reference(
    fit, dgp, truth, 0.95, draw = 3L
  )
  direct <- vapply(seq_len(ncol(fit$samp.eta_root1)), function(jj) {
    sum(rqrgibbs::rqr_mean_tilt_loss(
      dgp$y,
      fit$samp.eta_root1[, jj],
      fit$samp.eta_root2[, jj],
      coverage_level = 0.95,
      mean_tilt = truth$mean_tilt
    ))
  }, numeric(1L))

  testthat::expect_equal(trace$target_loss, direct, tolerance = 1e-10)
  testthat::expect_true(all(is.finite(trace$negative_log_target)))
  testthat::expect_true(all(trace$prior_quadratic >= 0))
  testthat::expect_equal(nrow(trace), 5L)
  diagnostic_trace <- otf_trace_diagnostic_matrix(trace)
  testthat::expect_true(all(c(
    "target_loss", "prior_quadratic", "negative_log_target",
    "maximum_width"
  ) %in% colnames(diagnostic_trace)))
  testthat::expect_false(any(grepl(
    "^root[12]_", colnames(diagnostic_trace)
  )))
  testthat::expect_true(
    otf_conditional_reference_pass(
      reference,
      relative_tolerance = 1e-7,
      R_cpp_absolute_tolerance = 1e-10
    )
  )
  testthat::expect_true(all(
    reference$dense_precision_reciprocal_condition > 0
  ))
  testthat::expect_true(all(reference$R_repair_count == 0L))
  testthat::expect_true(all(reference$cpp_repair_count == 0L))
})

testthat::test_that("tracked forensic configuration is fail-closed", {
  testthat::skip_if_not_installed("jsonlite")
  config <- jsonlite::read_json(
    testthat::test_path(
      "..", "..", "config", "oracle_tilt_forensics_20260730.json"
    ),
    simplifyVector = TRUE
  )
  testthat::expect_false(isTRUE(config$execution_authorized))
  testthat::expect_equal(config$coverage_level, 0.95)
  testthat::expect_equal(config$innovation$tau, 0.99)
  testthat::expect_equal(
    config$schema_version,
    "rqrgibbs_oracle_tilt_forensics_config/1.1.0"
  )
  testthat::expect_true(config$dlm$mcmc_control$store_state_draws)
  testthat::expect_true(config$dlm$mcmc_control$store_latent_draws)
  testthat::expect_setequal(
    config$dlm$initial_profiles,
    c("default", "oracle", "narrow", "wide")
  )
  testthat::expect_setequal(
    config$prior_sensitivity$initial_profiles,
    c("oracle", "prior_shift_stress")
  )
  testthat::expect_equal(
    config$prior_sensitivity$initial_slope_variances,
    c(1, 0.1, 0.01, 0.001)
  )

  acceptance <- jsonlite::read_json(
    testthat::test_path(
      "..", "..", "config",
      "oracle_tilt_dlm_sh_acceptance_20260730.json"
    ),
    simplifyVector = TRUE
  )
  testthat::expect_false(isTRUE(acceptance$execution_authorized))
  testthat::expect_false(isTRUE(acceptance$fixed_design$enabled))
  testthat::expect_identical(acceptance$dlm$targets, "SH")
  testthat::expect_equal(
    acceptance$dlm$initial_slope_variance, 0.001
  )
  testthat::expect_equal(acceptance$dlm$workers, 1)
  testthat::expect_equal(
    acceptance$dlm$mcmc_control$n_mcmc, 24000
  )
  testthat::expect_setequal(
    acceptance$dlm$initial_profiles,
    c("default", "oracle", "narrow", "wide", "prior_shift_stress")
  )
})

testthat::test_that("forensic runner preflight writes a compact hashed contract", {
  testthat::skip_if_not_installed("jsonlite")
  tmp <- tempfile("rqr_forensic_preflight_")
  config <- normalizePath(testthat::test_path(
    "..", "..", "config", "oracle_tilt_forensics_20260730.json"
  ), mustWork = TRUE)
  script <- normalizePath(testthat::test_path(
    "..", "..", "scripts", "33_run_oracle_tilt_forensics.R"
  ), mustWork = TRUE)
  status <- system2(
    file.path(R.home("bin"), "Rscript"),
    c(
      script,
      "--mode=preflight",
      paste0("--config=", config),
      paste0("--output-dir=", tmp)
    )
  )
  testthat::expect_equal(status, 0)
  required <- c(
    "population_tilt_geometry.csv",
    "dlm_prior_shift_summary.csv",
    "dlm_escaping_direction_profile.csv",
    "dlm_escaping_direction_optimum.csv",
    "fit_plan.csv",
    "closeout.json",
    "artifact_manifest.csv"
  )
  testthat::expect_true(all(file.exists(file.path(tmp, required))))
  closeout <- jsonlite::read_json(
    file.path(tmp, "closeout.json"), simplifyVector = TRUE
  )
  testthat::expect_true(closeout$preflight_pass)
  testthat::expect_false(closeout$execution_completed)
  testthat::expect_false(closeout$manuscript_promotion_authorized)
})

testthat::test_that("DLM-SH acceptance preflight contains only the frozen five-start plan", {
  testthat::skip_if_not_installed("jsonlite")
  tmp <- tempfile("rqr_forensic_sh_acceptance_preflight_")
  config <- normalizePath(testthat::test_path(
    "..", "..", "config",
    "oracle_tilt_dlm_sh_acceptance_20260730.json"
  ), mustWork = TRUE)
  script <- normalizePath(testthat::test_path(
    "..", "..", "scripts", "33_run_oracle_tilt_forensics.R"
  ), mustWork = TRUE)
  status <- system2(
    file.path(R.home("bin"), "Rscript"),
    c(
      script,
      "--mode=preflight",
      paste0("--config=", config),
      paste0("--output-dir=", tmp)
    )
  )
  testthat::expect_equal(status, 0)
  plan <- utils::read.csv(file.path(tmp, "fit_plan.csv"))
  testthat::expect_equal(nrow(plan), 5L)
  testthat::expect_true(all(plan$stage == "dlm_trace_forensics"))
  testthat::expect_true(all(plan$family == "dlm"))
  testthat::expect_true(all(plan$target == "SH"))
  testthat::expect_setequal(
    plan$chain_or_profile,
    c("default", "oracle", "narrow", "wide", "prior_shift_stress")
  )
})
