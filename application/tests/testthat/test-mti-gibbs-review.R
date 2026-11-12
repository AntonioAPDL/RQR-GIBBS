root <- normalizePath(if (file.exists("application/scripts/lib/mti_gibbs_review.R")) "." else "../../..")
for (file in c("pharma_tolerance_application.R", "pharma_mti_application.R",
    "mti_profile_revision.R", "mti_profile_review.R", "mti_final_reporting.R",
    "mti_ecm_recovery.R", "mti_gibbs_comparison.R", "mti_gibbs_practical.R",
    "mti_gibbs_review.R")) source(file.path(root, "application/scripts/lib", file))

testthat::test_that("empirical tilt limits have the stated exact outward slopes", {
  y <- c(-2, -.2, .1, .1, 1, 3)
  for (q in c(.5, .9, .999)) {
    zero <- mgr_geometry(y, q, 0)
    testthat::expect_equal(zero$target_class, "interior")
    for (delta in c(zero$tilt_lower_limit, zero$tilt_upper_limit)) {
      boundary <- mgr_geometry(y, q, delta)
      testthat::expect_equal(boundary$target_class, "boundary")
      testthat::expect_equal(min(boundary$lower_ray_slope, boundary$upper_ray_slope),
        0, tolerance = 1e-12)
    }
    for (delta in c(zero$tilt_lower_limit - .1, zero$tilt_upper_limit + .1)) {
      outside <- mgr_geometry(y, q, delta)
      testthat::expect_equal(outside$target_class, "outside")
      testthat::expect_lt(min(outside$lower_ray_slope, outside$upper_ray_slope), 0)
    }
  }
})

testthat::test_that("prior reweighting preserves the baseline and excludes wider priors", {
  testthat::skip_if_not_installed("posterior")
  set.seed(731)
  fits <- lapply(1:4, function(i) list(draws = cbind(rnorm(1000, -1, .2), rnorm(1000, 1, .2))))
  width <- mgp_array(fits)[, , "width"]
  d <- mgr_reweight(fits, list(prior_variance = 1), c(1, .5, .1), 400)
  testthat::expect_equal(d$width_mean[1], mean(width), tolerance = 1e-12)
  testthat::expect_equal(d$width_mean_mcse[1], posterior::mcse_mean(width), tolerance = 1e-12)
  testthat::expect_equal(d$kish_weight_ess[1], 4000)
  testthat::expect_true(all(is.finite(d$width_mean)))
  testthat::expect_lt(d$width_mean[3], d$width_mean[1])
  testthat::expect_error(mgr_reweight(fits, list(prior_variance = 1), 2, 400))
  testthat::expect_error(mgr_reweight(fits, list(prior_variance = 1), 0, 400))
  bad <- mgr_reweight(fits, list(prior_variance = 1), .01, 1e6)
  testthat::expect_false(bad$sensitivity_supported)
})

testthat::test_that("sampler agreement uses combined Monte Carlo error and fixed variables", {
  ref <- data.frame(case_id = "fixture", dataset_id = 1L, variable = c("lower", "upper"),
    mean = c(-1, 1), mcse_mean = c(.03, .04))
  newer <- ref; newer$mean <- newer$mean + .1
  d <- mgr_agreement(ref, newer, "original", FALSE)
  testthat::expect_equal(d$combined_mcse, sqrt(2) * ref$mcse_mean)
  testthat::expect_equal(d$standardized_difference, .1 / d$combined_mcse)
  testthat::expect_false(any(d$reference_reliable))
  testthat::expect_error(mgr_agreement(ref, newer[2:1, ], "original", TRUE), "variables differ")
})

testthat::test_that("saved summaries are checked numerically and aligned by quantity", {
  columns <- c("mean", "median", "sd", "rhat", "ess_bulk", "ess_tail", "mcse_mean",
    "lower_025", "upper_975")
  d <- data.frame(variable = c("lower", "upper"))
  for (column in columns) d[[column]] <- c(1, 2)
  testthat::expect_silent(mgr_compare_saved_summary(d, d[2:1, ]))
  bad <- d; bad$mean[1] <- 2
  testthat::expect_error(mgr_compare_saved_summary(d, bad), "Saved summary differs")
})

testthat::test_that("cell summaries never pool widths across distributions or designs", {
  d <- data.frame(source = "simulation", diagnostic_role = "balanced",
    response_id = c("normal", "normal", "lognormal", "normal"),
    n = c(50, 50, 50, 100), content = .9, target_class = "interior",
    maximum_rhat = 1, minimum_bulk_ess = 1000, ecm_relative_objective_gap = 0,
    ecm_width = c(2, 4, 100, 10), minimum_width = c(2, 4, 100, 10),
    posterior_width_mean = c(3, 5, 120, 11))
  cells <- mgr_cells(d)
  testthat::expect_equal(nrow(cells), 3)
  z <- cells[cells$distribution == "normal" & cells$n == 50, ]
  testthat::expect_equal(z$datasets, 2)
  testthat::expect_equal(z$ecm_width_mean, 3)
  testthat::expect_equal(c(z$posterior_width_mean_min, z$posterior_width_mean_max), c(3, 5))
})

testthat::test_that("review reads reject changed identity, observations, and objective traces", {
  testthat::skip_if_not_installed("digest")
  testthat::skip_if_not_installed("jsonlite")
  directory <- tempfile("posterior-review-")
  dir.create(file.path(directory, "chains"), recursive = TRUE)
  on.exit(unlink(directory, recursive = TRUE))
  y <- c(-1, .1, 1)
  task <- data.frame(job_id = 1L, dataset_id = 1L, case_id = "fixture", chain = 1L,
    q = .9, delta = 0, dataset_sha256 = mgc_hash(y))
  draws <- cbind(lower = c(-2, -1.8, -1.7), upper = c(2, 1.9, 1.8))
  fit <- list(task = task, draws = draws, passed = TRUE, repair_count = 0L,
    sample_sha256 = task$dataset_sha256, elapsed_seconds = 0,
    retained_objective = mgp_objective(y, draws[, 1], draws[, 2], .9, 0, 1, .1))
  fit$objective_summary <- range(fit$retained_objective)
  identity <- list(commit = "fixture")
  path <- mgc_chain_path(directory, 1L)
  write_record <- function(value) {
    saveRDS(value, path)
    pta_atomic_write_json(list(passed = TRUE, job_id = 1L, task_sha256 = mgc_hash(task),
      sample_sha256 = task$dataset_sha256, n_mcmc = 3L, sha256 = mgc_file_hash(path),
      analysis_identity = mgc_hash(identity)), paste0(path, ".json"))
  }
  base <- list(learning_rate = 1, prior_variance = 10)
  write_record(fit)
  testthat::expect_silent(mgr_read(directory, task, 3L, y, base, 3L, identity))
  testthat::expect_error(mgr_read(directory, task, 3L, y, base, 3L, list(commit = "changed")),
    "analysis identity differs")
  testthat::expect_error(mgr_read(directory, task, 3L, y + 1, base, 3L, identity),
    "observations differ")
  bad <- fit; bad$retained_objective <- bad$retained_objective + 1
  write_record(bad)
  testthat::expect_error(mgr_read(directory, task, 3L, y, base, 3L, identity),
    "objective check failed")
  saveRDS(fit, path)
  testthat::expect_error(mgr_read(directory, task, 3L, y, base, 3L, identity), "Corrupted")
})
