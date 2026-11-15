root <- normalizePath(if (file.exists("application/scripts/lib/mti_fixed_prior_closure.R")) "." else "../../..")
for (name in c("mti_gibbs_practical", "mti_gibbs_full_report_review", "mti_gibbs_full_publication",
  "mti_gibbs_width_diagnosis", "mti_prior_screen", "pharma_tolerance_application", "mti_fixed_prior", "mti_fixed_prior_closure"))
  source(file.path(root, "application/scripts/lib", paste0(name, ".R")))
prior <- mwd_json(file.path(root, "application/config/mti_fixed_prior_20261114.json"))

testthat::test_that("continuation preserves the fixed Bayesian specification", {
  manifest <- list(identity = list(configuration = prior))
  testthat::expect_silent(mfc_settings_check(prior, manifest))
  bad <- prior; bad$primary_kappa <- 1
  testthat::expect_error(mfc_settings_check(bad, manifest), "settings differ")
  bad <- prior; bad$simulation_reference$estimated_from_analysis_sample <- TRUE
  testthat::expect_error(mfc_settings_check(bad, list(identity = list(configuration = bad))), "fixed independent")
  testthat::expect_false(prior$automatic_prior_tuning)
  controls <- mwd_json(file.path(root, "application/config/mti_fixed_prior_closure_20261115.json"))
  testthat::expect_false(controls$automatic_prior_tuning)
  testthat::expect_false(controls$automatic_article_replacement)
})

testthat::test_that("numerical repair rejects changed samples, targets, priors, and accepted candidates", {
  task <- data.frame(job_id = 1L, stage = "validation", kappa = 2)
  specification <- mfp_spec(c(0, 1), 2, prior$simulation_reference)
  candidates <- data.frame(candidate_index = 1:2, target_content = c(.97, .98), mean_tilt = c(0, .1),
    central_tilt = 0, lower = c(-1, -2), upper = c(1, 2), ecm_objective = c(1, 2), observed_count = c(48L, 49L),
    numerically_accurate = c(TRUE, FALSE))
  original <- list(task = task, returned = FALSE, reason = "unresolved_profile_candidates",
    prior = specification, dp_prior = list(estimated_from_analysis_sample = FALSE),
    sample_sha256 = "same_observations", candidates = candidates)
  repaired <- original; repaired$returned <- TRUE; repaired$candidates$numerically_accurate <- TRUE
  repaired$rows <- data.frame(job_id = 1L, method = c("ecm", "gibbs"), returned = TRUE,
    ecm_coordinate_ok = TRUE, gibbs_adequate = TRUE)
  testthat::expect_silent(mfc_repair_check(original, repaired, task, prior))
  bad <- repaired; bad$sample_sha256 <- "different_observations"
  testthat::expect_error(mfc_repair_check(original, bad, task, prior), "sample or prior changed")
  bad <- repaired; bad$candidates$target_content[2] <- .99
  testthat::expect_error(mfc_repair_check(original, bad, task, prior), "target menu differs")
  bad <- repaired; bad$candidates$lower[1] <- -.9
  testthat::expect_error(mfc_repair_check(original, bad, task, prior), "accurate candidate changed")
  bad <- repaired; bad$prior$prior_sha256 <- mfp_spec(c(0, 1), 1, prior$simulation_reference)$prior_sha256
  testthat::expect_error(mfc_repair_check(original, bad, task, prior), "independent prior")
  bad <- repaired; bad$prior$center <- 9
  testthat::expect_error(mfc_repair_check(original, bad, task, prior), "independent prior")
  accepted <- original; accepted$returned <- TRUE
  testthat::expect_error(mfc_repair_check(accepted, repaired, task, prior), "accepted result")
  rows <- data.frame(job_id = rep(1:2, each = 2), method = rep(c("ecm", "gibbs"), 2),
    returned = c(FALSE, FALSE, TRUE, TRUE))
  merged <- mfc_merge(rows, list(repaired))
  testthat::expect_equal(nrow(merged), 4L)
  testthat::expect_true(all(merged$returned))
  testthat::expect_identical(merged[job_id == 2, returned], c(TRUE, TRUE))
  testthat::expect_error(mfc_merge(rows, list(repaired, repaired)), "Duplicate")
})

testthat::test_that("atomic saved fits are reusable only with compatible receipts", {
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(c(path, paste0(path, ".json"))))
  task <- data.frame(job_id = 1L)
  result <- list(task = task, returned = TRUE, value = 7)
  mfc_save(result, path, "identity")
  testthat::expect_identical(mfc_read(path, "identity", task), result)
  testthat::expect_error(mfc_read(path, "changed_identity"), "receipt differs")
  testthat::expect_error(mfc_read(path, "identity", data.frame(job_id = 2L)), "task differs")
  saveRDS(list(tampered = TRUE), path)
  testthat::expect_error(mfc_read(path, "identity"), "receipt differs")
})

testthat::test_that("parent completion cannot be inferred from partial progress", {
  directory <- tempfile(); dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE))
  testthat::expect_false(mfc_parent_ready(directory, 10L))
  mgfp_json_write(list(exit_code = 1), file.path(directory, "controller_exit.json"))
  testthat::expect_error(mfc_parent_ready(directory, 10L), "unsuccessfully")
  mgfp_json_write(list(exit_code = 0), file.path(directory, "controller_exit.json"))
  testthat::expect_error(mfc_parent_ready(directory, 10L), "before independent validation")
  mgfp_json_write(list(completed = 9L, expected = 10L), file.path(directory, "validation/status.json"))
  testthat::expect_error(mfc_parent_ready(directory, 10L), "incomplete")
  mgfp_json_write(list(completed = 10L, expected = 10L), file.path(directory, "validation/status.json"))
  testthat::expect_true(mfc_parent_ready(directory, 10L))
})

testthat::test_that("restart reuses verified jobs and reruns only an unreceipted interrupted save", {
  directory <- tempfile(); dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE))
  tasks <- data.frame(job_id = 1:2)
  calls <- 0L
  fit <- function(task) { calls <<- calls + 1L; list(task = task, returned = TRUE) }
  mfc_run_tasks(tasks, directory, "restart_identity", 1L, 0, fit)
  testthat::expect_equal(calls, 2L)
  mfc_run_tasks(tasks, directory, "restart_identity", 1L, 0, function(task) stop("unexpected refit"))
  testthat::expect_equal(calls, 2L)
  unlink(file.path(directory, "fits/fit_000002.rds.json"))
  mfc_run_tasks(tasks, directory, "restart_identity", 1L, 0, fit)
  testthat::expect_equal(calls, 3L)
  testthat::expect_true(file.exists(file.path(directory, "fits/fit_000002.rds.json")))
})

testthat::test_that("diagnostic selection is bounded and reproducible", {
  controls <- mwd_json(file.path(root, "application/config/mti_fixed_prior_closure_20261115.json"))$diagnostics
  rows <- expand.grid(job_id = 1:40, method = c("ecm", "gibbs"), stringsAsFactors = FALSE)
  rows$data_seed <- 100L + rows$job_id; rows$response_id <- controls$response_id
  rows$n <- controls$n; rows$content <- controls$content; rows$returned <- TRUE
  rows$attained <- ifelse(rows$job_id %% 3 == 0, TRUE, ifelse(rows$job_id %% 3 == 1, rows$method == "ecm", FALSE))
  a <- mfc_diagnostic_design(rows, controls); b <- mfc_diagnostic_design(rows[nrow(rows):1, ], controls)
  testthat::expect_equal(a, b)
  testthat::expect_equal(nrow(a), 12L)
  testthat::expect_true(all(table(a$pattern) == 4L))
  testthat::expect_false(anyDuplicated(a$seed) > 0)
})

testthat::test_that("population truth affects diagnostic evaluation but never posterior draws", {
  set.seed(501L); y <- rnorm(50)
  path <- tempfile(fileext = ".rds"); pdf <- tempfile(fileext = ".pdf")
  on.exit(unlink(c(path, pdf)))
  specification <- mfp_spec(y, 2, prior$simulation_reference)
  source <- list(task = data.frame(job_id = 1L, dataset_id = 1L, response_id = "normal", data_seed = 501L,
      n = 50L, content = .9, kappa = 2, stage = "validation"), returned = TRUE,
    sample_sha256 = mwd_hash(y), prior = specification, q = .95, delta_reference = 0,
    dp_prior = list(concentration = 1, raw_base = list(mean = 0, sd = 4), estimated_from_analysis_sample = FALSE),
    rows = data.frame(method = c("ecm", "gibbs"), lower = -2, upper = 2, population_content = .95,
      dp_content_probability = .99),
    parameters_reference_units = data.frame(variable = c("lower", "upper", "width", "midpoint"),
      mean = c(-2, 2, 4, 0), mcse_mean = .1))
  saveRDS(source, path)
  task <- data.frame(job_id = 1L, seed = 1000000L, pattern = "both")
  controls <- list(n_burn = 50L, n_mcmc = 256L, chains = 4L)
  policy <- mwd_json(file.path(root, "application/config/mti_gibbs_practical_20261112.json"))$policy
  context <- list(laws = list(normal = list(r = rnorm, p = pnorm)))
  a <- mfc_diagnostic_job(task, path, prior, context, controls, policy)
  context$laws$normal$p <- function(x) pnorm(x, 20)
  b <- mfc_diagnostic_job(task, path, prior, context, controls, policy)
  testthat::expect_identical(a$fits, b$fits)
  testthat::expect_identical(a$parameters, b$parameters)
  testthat::expect_identical(a$dp_probability, b$dp_probability)
  testthat::expect_false(identical(a$population_content, b$population_content))
  testthat::expect_identical(a$prior$prior_sha256, specification$prior_sha256)
  mfc_diagnostic_pdf(list(a), pdf)
  testthat::expect_gt(file.info(pdf)$size, 1000)
})

testthat::test_that("the review reports cell deficits without promoting a tolerance guarantee", {
  directory <- tempfile(); dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE))
  d <- expand.grid(job_id = 1:200, method = c("ecm", "gibbs"), stringsAsFactors = FALSE)
  d$response_id <- "normal"; d$n <- 50L; d$content <- .9; d$tolerance_confidence <- .95
  d$kappa <- 2; d$returned <- TRUE; d$ecm_coordinate_ok <- TRUE; d$gibbs_adequate <- TRUE
  d$width <- 2; d$attained <- !(d$method == "ecm" & d$job_id <= 30L)
  b <- d; b$method <- ifelse(b$method == "ecm", "young_mathew", "wilks")
  mwd_write(d, file.path(directory, "interval_results.csv"))
  mwd_write(b, file.path(directory, "comparator_results.csv"))
  status <- mfc_summary(directory, prior)
  testthat::expect_true(status$numerical_pass)
  testthat::expect_equal(nrow(status$pointwise_underattainment_warnings), 1L)
  testthat::expect_false(status$simultaneous_tolerance_guarantee)
  testthat::expect_false(status$prior_uses_analysis_data)
  testthat::expect_false(status$article_replaced)
  testthat::expect_gt(file.info(file.path(directory, "study_review.pdf"))$size, 1000)
})

testthat::test_that("application held-out observations cannot specify the prior or change posterior draws", {
  set.seed(18L); y <- rnorm(50)
  task <- data.frame(training_key = "test:1", response_id = "fct_tensile", n = 50L,
    content = .9, tolerance_confidence = .95, kappa = 2, job_id = 1L, stage = "application")
  context <- list(training_ids = list("test:1" = 1:50),
    observed = data.frame(.analysis_row = 1:70, fct_tensile = c(y, rep(0, 20))),
    policy = data.frame(n = 50L, content = .9, screen = .995, q_grid = ".99,.9999"),
    scan = data.frame(n = 50L, content = .9, retained_fraction = 1),
    profile = list(ecm_control = list(ecm_backend = "cpp", multistart = FALSE)),
    dp = list(base = list(mean = 0, sd = 4)))
  specification <- prior
  specification$diagnostic_config <- file.path(root, prior$diagnostic_config)
  # The application helper reads this policy through a repository-relative path.
  old <- setwd(root); on.exit(setwd(old), add = TRUE)
  specification$sampling$n_burn <- 50L; specification$sampling$n_mcmc <- 256L
  specification$sampling$maximum_retained <- 256L
  a <- mfp_application_job(task, specification, context)
  context$observed$fct_tensile[51:70] <- -1e8
  b <- mfp_application_job(task, specification, context)
  testthat::expect_true(a$returned && b$returned)
  testthat::expect_identical(a$prior, b$prior)
  testthat::expect_identical(a$fits, b$fits)
  testthat::expect_identical(a$selected, b$selected)
  testthat::expect_false(identical(a$rows$heldout_empirical_content, b$rows$heldout_empirical_content))
})

testthat::test_that("application summaries retain failed attempts and separate partition types", {
  directory <- tempfile(); dir.create(directory); dir.create(file.path(directory, "fits"))
  on.exit(unlink(directory, recursive = TRUE))
  tasks <- data.frame(job_id = 1:3, training_key = c("grouped:1", "grouped:2", "random:1"),
    response_id = "response", kappa = 2)
  for (j in 1:3) {
    task <- tasks[j, ]; r <- list(task = task, returned = j != 2L, reason = "deliberate_test_failure")
    if (r$returned) r$rows <- cbind(task[rep(1, 2), ], method = c("ecm", "gibbs"),
      returned = TRUE, width = 2, heldout_empirical_content = .9,
      ecm_coordinate_ok = TRUE, gibbs_adequate = TRUE)
    r$comparators <- cbind(task[rep(1, 2), ], method = c("young_mathew", "wilks"),
      returned = TRUE, width = 3, heldout_empirical_content = .95)
    mfc_save(r, file.path(directory, "fits", sprintf("fit_%06d.rds", j)), "application_test")
  }
  status <- mfp_collect_application(tasks, directory, "application_test")
  testthat::expect_false(status$numerical_pass)
  testthat::expect_identical(status$failed_pairs, 1L)
  d <- data.table::fread(file.path(directory, "interval_results.csv"))
  testthat::expect_equal(nrow(d), 6L)
  cells <- mfc_application_review(directory)
  testthat::expect_equal(cells[partition == "grouped" & method == "ecm", attempts], 2L)
  testthat::expect_equal(cells[partition == "grouped" & method == "ecm", returned], 1L)
  testthat::expect_equal(nrow(cells), 8L)
  testthat::expect_gt(file.info(file.path(directory, "application_review.pdf"))$size, 1000)
})
