root <- normalizePath(if (file.exists("application/scripts/lib/mti_fixed_prior.R")) "." else "../../..")
for (name in c("mti_gibbs_practical", "mti_gibbs_full_report_review", "mti_gibbs_full_publication",
  "mti_gibbs_width_diagnosis", "mti_prior_screen", "pharma_tolerance_application", "mti_fixed_prior"))
  source(file.path(root, "application/scripts/lib", paste0(name, ".R")))
reference <- list(mode = "fixed", center = 0, scale = 1,
  source = "declared_test_coordinates", estimated_from_analysis_sample = FALSE)

testthat::test_that("the fixed prior does not use sample location, scale, or size", {
  a <- mfp_spec(c(-1, 1), 2, reference)
  for (y in list(c(900, 901, 1000), rep(4, 1000), c(-1e5, 1e5))) {
    b <- mfp_spec(y, 2, reference)
    testthat::expect_identical(b$prior_sha256, a$prior_sha256)
    testthat::expect_identical(b$prior_sd, 2)
    testthat::expect_identical(b$prior_variance, 4)
    testthat::expect_identical(b$learning_rate, 1)
    testthat::expect_false(b$data_adaptive_prior)
  }
  bad <- reference; bad$estimated_from_analysis_sample <- TRUE
  testthat::expect_error(mfp_spec(1:3, 2, bad), "fixed independent")
  bad <- reference; bad$scale <- 0
  testthat::expect_error(mfp_spec(1:3, 2, bad), "Invalid prior")
})

testthat::test_that("prior and loss units change together", {
  y <- c(-3, -.5, 1, 3)
  ref <- reference; ref$center <- 20; ref$scale <- 7
  a <- mfp_spec(y, 2, reference); b <- mfp_spec(20 + 7 * y, 2, ref)
  testthat::expect_equal(a$z, b$z)
  testthat::expect_equal(b$prior_sd, 7 * a$prior_sd)
  testthat::expect_equal(b$learning_rate, a$learning_rate / 49)
  testthat::expect_equal(b$prior_variance, 49 * a$prior_variance)
  testthat::expect_equal(mgp_objective(y, -2, 2, .9, -.1, precision = .25),
    sum(((y + 2) * (y - 2)) * (.9 - ((y + 2) * (y - 2) < 0)) - .9 * -.1 * (-2 + 2 - 2 * y)) +
      ((-2)^2 + 2^2) / 8)
})

testthat::test_that("stable one-root increments agree with direct objectives", {
  y <- c(-3, -1, -.1, .5, 2)
  roots <- c(-1.4, 1.2)
  for (j in 1:2) for (proposed in c(-2, -.4, .5, 2.3)) {
    new <- roots; new[j] <- proposed
    delta <- mfp_increment(y, roots, j, proposed, .93, -.2, 4)
    direct <- mgp_objective(y, new[1], new[2], .93, -.2, precision = .25) -
      mgp_objective(y, roots[1], roots[2], .93, -.2, precision = .25)
    testthat::expect_equal(delta$change, direct, tolerance = 1e-12)
    testthat::expect_gt(delta$error_bound, 0)
  }
  increase <- mfp_increment(y, roots, 1, -100, .93, 0, 4)
  testthat::expect_gt(increase$change, increase$error_bound)
})

testthat::test_that("analytical knot restoration preserves closed counts", {
  y <- c(-2, -.5, .2, 1, 2)
  a <- mfp_restore(y, c(-2 + 1e-10, 2 - 1e-10), .9999, 0, 1)
  testthat::expect_identical(a$roots, c(-2, 2))
  testthat::expect_equal(sum(y >= a$roots[1] & y <= a$roots[2]), 5)
  testthat::expect_true(all(a$adjustments$objective_change <= a$adjustments$error_bound))
  far <- mfp_restore(y, c(-300, 1.5), .9, -.2, 10000)
  testthat::expect_identical(far$roots, c(-300, 1.5))
})

testthat::test_that("bounded conditional-mode refinement decreases the same root objective", {
  cfg <- jsonlite::read_json(file.path(root, "application/config/mti_fixed_prior_20261114.json"), simplifyVector = TRUE)
  y <- c(-3, -1, -.1, .5, 2); roots <- c(-1.4, 1.2)
  start <- mgp_objective(y, roots[1], roots[2], .93, -.2, precision = .25)
  p <- mfp_polish(y, roots, .93, -.2, 4, cfg$numerical)
  testthat::expect_true(p$accuracy$pass)
  testthat::expect_lte(tail(p$trace$objective, 1), start)
  testthat::expect_true(all(p$trace$stable_objective_change <= p$trace$objective_change_error_bound))
  testthat::expect_true(all(p$trace$numerical_stage == "conditional_mode_refinement"))
  testthat::expect_lte(p$cycles, cfg$numerical$conditional_mode_cycles)
})

testthat::test_that("truth evaluation is separate from fitting inputs", {
  testthat::expect_false(any(c("cdf", "heldout", "family", "oracle", "population_content") %in%
    names(formals(mfp_fit_data))))
  before <- list(returned = TRUE, rows = data.frame(lower = -1, upper = 1))
  a <- mfp_evaluate(before, pnorm, .9); b <- mfp_evaluate(before, function(x) pnorm(x, 20), .9)
  testthat::expect_identical(a$rows$lower, b$rows$lower)
  testthat::expect_identical(a$rows$upper, b$rows$upper)
  testthat::expect_false(identical(a$rows$population_content, b$rows$population_content))
})

testthat::test_that("the Gamma closed-count boundary is resolved after conditional refinement", {
  cfg <- jsonlite::read_json(file.path(root, "application/config/mti_fixed_prior_20261114.json"), simplifyVector = TRUE)
  set.seed(1840390352L); y <- (rgamma(500, .5) - .5) / sqrt(.5)
  controls <- list(max_iter = 200L, tol_stationarity = .001, stable_iterations = 1L,
    residual_product_floor = 1e-8, multistart = FALSE, ecm_backend = "cpp", store_iteration_trace = TRUE)
  # Reproduce the original grid's binary value rather than its rounded decimal label.
  q <- .9765 - .Machine$double.eps / 2
  action <- rqrgibbs::rqr_mti_ecm_dp_profile_action(y, .95, .995, q_grid = q,
    tilt_grid_control = list(tilt_offsets_sd = 0, include_zero_tilt = TRUE),
    beta_prior_obj = rqrgibbs::beta_prior("ridge", ridge = list(tau2 = 4)),
    ecm_control = controls, expand_if_empty = FALSE, return_candidate_traces = TRUE)
  refined <- mfp_refine(action, y, 2, cfg$numerical, controls)
  target <- refined$candidates[refined$candidates$mean_tilt == 0, ]
  testthat::expect_equal(nrow(target), 1L)
  testthat::expect_true(target$numerically_accurate && target$count_stable)
  testthat::expect_equal(target$observed_count, 489L)
  testthat::expect_equal(refined$unresolved_candidates, 0L)
  trace <- refined$traces[[match(target$candidate_index, refined$candidates$candidate_index)]]
  testthat::expect_true(any(trace$numerical_stage == "final_conditional_knot_representation"))
})

testthat::test_that("the historical cancellation case has a stable decreasing increment", {
  set.seed(1820100008L)
  raw <- stats::rt(100, 3) / sqrt(3)
  y <- (raw - mean(raw)) / sd(raw)
  # These are fixed regression-case coordinates, not a newly estimated prior.
  roots <- c(-7.63998554726155, y[64] - 2 * .Machine$double.eps)
  difference <- mfp_increment(y, roots, 2L, y[64], .975, -.148530314198784, 1)
  testthat::expect_lt(difference$change, 0)
  restored <- mfp_restore(y, roots, .975, -.148530314198784, 1)
  testthat::expect_true(any(restored$roots == y[64]))
})

testthat::test_that("all four missing-maximum examples are completed without a scan fallback", {
  cfg <- jsonlite::read_json(file.path(root, "application/config/mti_fixed_prior_20261114.json"), simplifyVector = TRUE)
  controls <- list(max_iter = 200L, multistart = FALSE, ecm_backend = "cpp", store_iteration_trace = TRUE)
  for (seed in c(1820480001L, 1820480005L, 1820480010L, 1820480011L)) {
    set.seed(seed); l <- 1.25
    y <- (rlnorm(500, 0, l) - exp(l^2 / 2)) / sqrt((exp(l^2) - 1) * exp(l^2))
    action <- rqrgibbs::rqr_mti_ecm_dp_profile_action(y, .99, .995,
      q_grid = .9999, tilt_grid_control = list(tilt_offsets_sd = 0, include_zero_tilt = TRUE),
      beta_prior_obj = rqrgibbs::beta_prior("ridge", ridge = list(tau2 = 1)),
      ecm_control = controls, expand_if_empty = FALSE, return_candidate_traces = TRUE)
    refined <- mfp_refine(action, y, 1, cfg$numerical, controls)
    testthat::expect_equal(nrow(refined$selected), 1L)
    testthat::expect_true(refined$selected$numerically_accurate)
    testthat::expect_equal(refined$selected$observed_count, 500L)
    testthat::expect_gte(refined$selected$posterior_content_probability, .995)
  }
})

testthat::test_that("held-out outcomes cannot change a fitted fixed-prior profile", {
  cfg <- jsonlite::read_json(file.path(root, "application/config/mti_fixed_prior_20261114.json"), simplifyVector = TRUE)
  set.seed(901L); training <- rnorm(50)
  fit <- function(unused_heldout) mfp_fit_data(training, .9, .95, 1, 2, reference,
    list(ecm_control = list(ecm_backend = "cpp", multistart = FALSE)),
    list(base = list(mean = 0, sd = 4)), list(screen = .995, q_grid = c(.99, .9999)),
    cfg$numerical, cfg$sampling, 1001L, list(), sample_gibbs = FALSE)
  a <- fit(rep(-1e8, 100)); b <- fit(rep(1e8, 100))
  testthat::expect_true(a$returned && b$returned)
  testthat::expect_identical(a$prior, b$prior)
  testthat::expect_identical(a$selected, b$selected)
  testthat::expect_identical(a$candidates, b$candidates)
})

testthat::test_that("benchmarks use the same observations and retain structural limitations", {
  y <- seq(-2, 3, length.out = 50)
  b <- mfp_comparators(y, .9, .95)
  testthat::expect_equal(b$lower[b$method == "wilks"], min(y))
  testthat::expect_equal(b$upper[b$method == "wilks"], max(y))
  testthat::expect_true(all(b$returned))
  infeasible <- mfp_comparators(y, .99, .95)
  testthat::expect_false(infeasible$returned[infeasible$method == "wilks"])
})

testthat::test_that("development, calibration, and final-validation observations are separated", {
  cfg <- jsonlite::read_json(file.path(root, "application/config/mti_fixed_prior_20261114.json"), simplifyVector = TRUE)
  context <- list(laws = setNames(vector("list", 8), letters[1:8]),
    scan = data.frame(n = c(50,100,100,500,500,500,1000,1000,1000),
      content = c(.9,.9,.95,.9,.95,.99,.9,.95,.99), tolerance_confidence = .95, retained_fraction = 1))
  a <- mfp_design("development", cfg, context); b <- mfp_design("calibration", cfg, context)
  c <- mfp_design("validation", cfg, context)
  testthat::expect_equal(nrow(a), 1728)
  testthat::expect_equal(nrow(b), 14400)
  testthat::expect_equal(nrow(c), 72000)
  testthat::expect_false(any(a$data_seed %in% b$data_seed))
  testthat::expect_false(any(c$data_seed %in% c(a$data_seed, b$data_seed)))
  testthat::expect_false(cfg$automatic_prior_tuning)
  testthat::expect_false(cfg$automatic_article_replacement)
})

testthat::test_that("task and chain seeds are checked before expensive sampling", {
  cfg <- jsonlite::read_json(file.path(root, "application/config/mti_fixed_prior_20261114.json"), simplifyVector = TRUE)
  context <- list(laws = setNames(vector("list", 8), letters[1:8]),
    training_ids = list(example = 1:100),
    scan = data.frame(n = c(50,100,100,500,500,500,1000,1000,1000),
      content = c(.9,.9,.95,.9,.95,.99,.9,.95,.99), tolerance_confidence = .95, retained_fraction = 1))
  testthat::expect_silent(mfp_check_designs(cfg, context))
  bad <- cfg; bad$stages$validation$chain_seed_base <- cfg$stages$calibration$chain_seed_base
  testthat::expect_error(mfp_check_designs(bad, context), "chain seeds overlap")
  bad <- cfg; bad$stages$validation$data_seed_base <- cfg$stages$development$data_seed_base
  testthat::expect_error(mfp_check_designs(bad, context), "seeds overlap")
})

testthat::test_that("storage estimates handle one file and reject missing evidence", {
  testthat::expect_equal(mfp_storage_estimate(100, 10), 1250)
  testthat::expect_gt(mfp_storage_estimate(c(100, 200), 10), 1875)
  testthat::expect_equal(mfp_storage_estimate(100, 0), 0)
  testthat::expect_error(mfp_storage_estimate(numeric(), 10), "development fits")
})

testthat::test_that("collection retains failed fits and rejects corrupted results", {
  directory <- tempfile(); dir.create(directory); dir.create(file.path(directory, "fits"))
  on.exit(unlink(directory, recursive = TRUE))
  cfg <- list(primary_kappa = 2)
  tasks <- data.frame(job_id = 1:100, response_id = "test_law", n = 50,
    content = .9, tolerance_confidence = .95, kappa = 2)
  save_fit <- function(j, returned = TRUE, attained = TRUE) {
    task <- tasks[j, , drop = FALSE]
    r <- list(task = task, returned = returned, reason = "deliberate_test_failure")
    if (returned) r$rows <- cbind(task[rep(1, 2), ], method = c("ecm", "gibbs"),
      returned = TRUE, attained = attained, width = 2, ecm_coordinate_ok = TRUE, gibbs_adequate = TRUE)
    path <- file.path(directory, "fits", sprintf("fit_%06d.rds", j))
    saveRDS(r, path)
    mgfp_json_write(list(identity = "test_identity", sha256 = mwd_file_hash(path)), paste0(path, ".json"))
  }
  for (j in tasks$job_id) save_fit(j)
  status <- mfp_collect(tasks, directory, "test_identity", cfg)
  testthat::expect_true(status$numerical_pass && status$ready_for_final_validation)
  testthat::expect_false(status$simultaneous_tolerance_guarantee)
  testthat::expect_false(status$article_replaced)
  for (j in 1:50) save_fit(j, attained = FALSE)
  status <- mfp_collect(tasks, directory, "test_identity", cfg)
  testthat::expect_true(status$calibration_clearly_underattains)
  testthat::expect_false(status$ready_for_final_validation)
  save_fit(1L, returned = FALSE)
  status <- mfp_collect(tasks, directory, "test_identity", cfg)
  testthat::expect_identical(status$failed_pairs, 1L)
  testthat::expect_false(status$numerical_pass)
  cells <- read.csv(file.path(directory, "cell_summary.csv"))
  testthat::expect_true(all(cells$attempts == 100 & cells$returned == 99))
  mfp_review(directory, cfg)
  testthat::expect_gt(file.info(file.path(directory, "study_review.pdf"))$size, 1000)
  path <- file.path(directory, "fits", "fit_000001.rds")
  saveRDS(list(tampered = TRUE), path)
  testthat::expect_error(mfp_collect(tasks, directory, "test_identity", cfg), "identity changed")
})
