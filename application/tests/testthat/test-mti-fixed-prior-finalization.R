root <- normalizePath(if (file.exists("application/scripts/lib/mti_fixed_prior_finalization.R")) "." else "../../..")
for (name in c("mti_gibbs_practical", "mti_gibbs_full_report_review", "mti_gibbs_full_publication",
  "mti_gibbs_width_diagnosis", "mti_prior_screen", "pharma_tolerance_application", "mti_fixed_prior",
  "mti_fixed_prior_closure", "mti_fixed_prior_finalization", "mti_fixed_prior_publication"))
  source(file.path(root, "application/scripts/lib", paste0(name, ".R")))

testthat::test_that("precision selection cannot replace adequate fits or mixing failures", {
  r <- list(returned = TRUE, rows = data.frame(ecm_coordinate_ok = TRUE, gibbs_preferred = TRUE,
    gibbs_mean_precision = FALSE))
  testthat::expect_true(mff_precision_only(r))
  good <- r; good$rows$gibbs_mean_precision <- TRUE
  testthat::expect_false(mff_precision_only(good))
  bad <- r; bad$rows$gibbs_preferred <- FALSE
  testthat::expect_false(mff_precision_only(bad))
  bad <- r; bad$rows$ecm_coordinate_ok <- FALSE
  testthat::expect_false(mff_precision_only(bad))
  bad <- r; bad$returned <- FALSE
  testthat::expect_false(mff_precision_only(bad))
})

testthat::test_that("lengthening a fixed-target chain preserves every saved draw", {
  set.seed(41); y <- 1.3 + .1 * rnorm(50)
  prior <- mwd_json(file.path(root, "application/config/mti_fixed_prior_20261114.json"))
  prior$sampling$n_burn <- 20L; prior$sampling$n_mcmc <- 128L; prior$sampling$maximum_retained <- 128L
  policy <- list(preferred_rhat = 2, preferred_ess = 1, preferred_mcse_ratio = 1,
    exploratory_rhat = 2, exploratory_ess = 1, exploratory_mcse_ratio = 1, relative_width_resolution = .00001)
  r <- mfp_fit_data(y, .9, .95, 1, 2, prior$application_reference,
    list(ecm_control = list(ecm_backend = "cpp", multistart = FALSE)),
    list(base = list(mean = 0, sd = 4)), list(screen = .995, q_grid = c(.99, .9999)),
    prior$numerical, prior$sampling, 1670000100L, policy)
  testthat::expect_true(mff_precision_only(r))
  task <- data.frame(training_key = "test:1", response_id = "fct_tensile", n = 50L,
    content = .9, tolerance_confidence = .95, kappa = 2, job_id = 1L, stage = "application")
  r$task <- task; r$rows <- cbind(task[rep(1, 2), ], r$rows)
  context <- list(training_ids = list("test:1" = 1:50),
    observed = data.frame(.analysis_row = 1:60, fct_tensile = c(y, rep(1.3, 10))))
  r$rows$heldout_empirical_content <- 1
  a <- mff_extend(r, prior, context, 256L, policy)
  testthat::expect_silent(mff_prefix_check(r$fits, a$fits))
  testthat::expect_identical(a$prior, r$prior)
  testthat::expect_identical(a$selected, r$selected)
  testthat::expect_identical(a$candidates, r$candidates)
  testthat::expect_identical(a$q, r$q)
  testthat::expect_identical(a$delta_reference, r$delta_reference)
  context$observed$fct_tensile[51:60] <- -1e8
  b <- mff_extend(r, prior, context, 256L, policy)
  testthat::expect_identical(a$fits, b$fits)
  testthat::expect_identical(a$parameters_reference_units, b$parameters_reference_units)
  testthat::expect_false(identical(a$rows$heldout_empirical_content, b$rows$heldout_empirical_content))
  altered <- a$fits; altered[[1]]$draws[1, 1] <- 99
  testthat::expect_error(mff_prefix_check(r$fits, altered), "prefix changed")
  context$observed$fct_tensile[1] <- 99
  testthat::expect_error(mff_extend(r, prior, context, 256L, policy), "observations changed")
  merged <- mff_merge(data.table::as.data.table(r$rows), list(a))
  testthat::expect_equal(nrow(merged), 2L)
  path <- tempfile(fileext = ".csv"); data.table::fwrite(r$rows, path)
  testthat::expect_silent(mff_merge(data.table::fread(path), list(a)))
  changed <- a; changed$rows$width[changed$rows$method == "ecm"] <- changed$rows$width[1] + 1e-8
  testthat::expect_error(mff_merge(data.table::fread(path), list(changed)), "ECM interval changed")
  unlink(path)
  testthat::expect_error(mff_merge(r$rows, list(a, a)), "Duplicate")
  good <- r$rows; good$gibbs_mean_precision <- TRUE
  testthat::expect_error(mff_merge(good, list(a)), "adequate result")
})

testthat::test_that("reporting retains separate raw width quantiles and rejects missing design rows", {
  d <- expand.grid(job_id = 1:20, method = names(mff_methods), stringsAsFactors = FALSE)
  d$response_id <- ifelse(d$job_id <= 10, "normal", "exponential")
  d$n <- 50L; d$content <- .9; d$tolerance_confidence <- .95
  d$returned <- TRUE; d$attained <- TRUE
  d$width <- ifelse(d$job_id <= 10, d$job_id, 1000 + d$job_id)
  cells <- mff_simulation_cells(d, expected = 10L, settings = 2L)
  testthat::expect_equal(nrow(cells), 8L)
  testthat::expect_equal(cells[response_id == "normal" & method == "ecm", width_q025], unname(quantile(1:10, .025)))
  testthat::expect_true(all(cells[response_id == "exponential", width_q025] > 1000))
  main <- mff_primary_cells(cells)
  testthat::expect_equal(nrow(main), 6L)
  testthat::expect_false("mti_gibbs_mean" %in% main$method_id)
  testthat::expect_error(mff_simulation_cells(rbind(d, d[1, ]), 10L, 2L), "Duplicate")
  testthat::expect_error(mff_simulation_cells(d[-1, ], 10L, 2L), "Incomplete")
})

testthat::test_that("finalization waits for a successful complete continuation", {
  directory <- tempfile(); dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE))
  testthat::expect_false(mff_ready(directory))
  mgfp_json_write(list(exit_code = 1), file.path(directory, "controller_exit.json"))
  testthat::expect_error(mff_ready(directory), "unsuccessfully")
  mgfp_json_write(list(exit_code = 0), file.path(directory, "controller_exit.json"))
  mgfp_json_write(list(phase = "application"), file.path(directory, "status.json"))
  testthat::expect_error(mff_ready(directory), "planned attempts")
  mgfp_json_write(list(phase = "completed_attempts_require_numerical_review"), file.path(directory, "status.json"))
  for (stage in c("application", "diagnostics")) {
    dir.create(file.path(directory, stage))
    mgfp_json_write(list(completed = 1L, expected = 1L), file.path(directory, stage, "status.json"))
  }
  testthat::expect_true(mff_ready(directory))
  mgfp_json_write(list(completed = 0L, expected = 1L), file.path(directory, "diagnostics/status.json"))
  testthat::expect_error(mff_ready(directory), "Incomplete diagnostics")
})

testthat::test_that("portable reporting evidence rejects changed files and prior interpretation", {
  directory <- tempfile(); dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE))
  file <- file.path(directory, "values.csv"); mwd_write(data.frame(x = 1), file)
  p <- list(prior = mwd_json(file.path(root, "application/config/mti_fixed_prior_20261114.json")),
    new_distribution_free_guarantee = FALSE, automatic_article_replacement = FALSE,
    data_files = list("values.csv" = mwd_file_hash(file)))
  path <- file.path(directory, "provenance.json"); mgfp_json_write(p, path)
  testthat::expect_silent(mff_verify_evidence(directory))
  bad <- p; bad$prior$application_reference$estimated_from_analysis_sample <- TRUE
  mgfp_json_write(bad, path)
  testthat::expect_error(mff_verify_evidence(directory), "interpretation or prior")
  mgfp_json_write(p, path); mwd_write(data.frame(x = 2), file)
  testthat::expect_error(mff_verify_evidence(directory), "evidence changed")
})
