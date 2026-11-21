root <- normalizePath(if (file.exists("application/scripts/lib/mti_ecm_adaptive_calibration.R")) "." else "../../..")
for (name in c("mti_gibbs_practical", "mti_gibbs_full_report_review", "mti_gibbs_full_publication",
  "mti_gibbs_width_diagnosis", "mti_fixed_prior", "mti_gibbs_boundary_calibration",
  "mti_gibbs_decision_calibration", "mti_gibbs_joint", "mti_ecm_nominal",
  "mti_ecm_adaptive_calibration", "mti_ecm_adaptive_study"))
  source(file.path(root, "application/scripts/lib", paste0(name, ".R")))
adaptive_config <- function() mwd_json(file.path(root, "application/config/mti_ecm_adaptive_calibration_20261120.json"))
adaptive_candidates <- function() data.frame(candidate_index = 1:3, target_content = c(.92, .96, .99),
  mean_tilt = 0, central_tilt = 0, lower = -c(1, 2, 3), upper = c(1, 2, 3), width = c(2, 4, 6),
  finite_interval = TRUE, numerically_accurate = TRUE, posterior_content_probability = c(.7, .9, .999))
adaptive_paths <- function(n = 100) lapply(seq_len(n), function(i) {
  p <- adaptive_candidates(); p$population_content <- c(if (i %% 2) .95 else .8, .96, .99)
  p$attained <- p$population_content >= .9; p
})

testthat::test_that("adaptive calibration preserves the independently specified model", {
  cfg <- adaptive_config(); testthat::expect_true(mac_validate(cfg))
  for (field in c("prior_sd", "learning_rate", "dp_concentration", "dp_base_sd", "radius_multiplier")) {
    bad <- cfg; bad[[field]] <- bad[[field]] + 1
    testthat::expect_error(mac_validate(bad), "specification")
  }
  for (field in c("sample_gibbs", "automatic_article_replacement", "application_transfer", "fresh_selection_assessment")) {
    bad <- cfg; bad[[field]] <- TRUE
    testthat::expect_error(mac_validate(bad), "specification")
  }
  bad <- cfg; bad$coordinates$estimated_from_analysis_sample <- TRUE
  testthat::expect_error(mac_validate(bad), "specification")
})

testthat::test_that("sample features use type-eight quantiles and handle degeneracy", {
  y <- c(-10, -2, 0, 1, 2, 4, 20)
  q <- unname(quantile(y, c(.1, .25, .5, .75, .9), type = 8))
  testthat::expect_equal(mac_features(y)$absolute_bowley, abs((q[4] + q[2] - 2*q[3])/(q[4] - q[2])))
  testthat::expect_equal(mac_features(y), mac_features(4*y + 30), tolerance = 1e-12)
  testthat::expect_equal(mac_features(y), mac_features(-y), tolerance = 1e-12)
  testthat::expect_true(mac_features(rep(2, 10))$degenerate)
  testthat::expect_true(is.na(mac_features(rep(2, 10))$absolute_bowley))
  testthat::expect_error(mac_features(c(1, NA)), "Finite")
})

testthat::test_that("the deployed rule rejects hidden distribution information", {
  rule <- list(feature = "absolute_bowley", split = .3, gamma_low = .8, gamma_high = .9, gamma_invalid = .995)
  features <- data.frame(absolute_bowley = c(.1, .3, .7, NA), log_tail_weight = 1)
  testthat::expect_equal(mac_predict(rule, features), c(.8, .8, .9, .995))
  bad <- rule; bad$response_id <- "normal"
  testthat::expect_error(mac_predict(bad, features), "frozen")
  bad <- rule; bad$true_cdf <- pnorm
  testthat::expect_error(mac_predict(bad, features), "frozen")
  bad <- rule; bad$gamma_low <- 1
  testthat::expect_error(mac_predict(bad, features), "frozen")
})

testthat::test_that("event sums reproduce selection and nonmonotone attainment", {
  paths <- adaptive_paths(6); paths[[1]]$attained <- c(TRUE, FALSE, TRUE)
  laws <- rep(c("a", "b"), each = 3); gamma <- c(.7, .7 + 1e-12, .9, .995)
  out <- mac_moments(paths, laws, rep(TRUE, 6), gamma, c("a", "b"))
  for (i in seq_along(gamma)) for (j in 1:2) {
    selected <- lapply(paths[laws == c("a", "b")[j]], mdc_select, gamma = gamma[i])
    testthat::expect_equal(out$width[i, j], sum(vapply(selected, function(p) p$width, 0)))
    testthat::expect_equal(out$success[i, j], sum(vapply(selected, function(p) as.numeric(p$attained), 0)))
  }
  empty <- mac_moments(paths, laws, rep(FALSE, 6), gamma, c("a", "b"))
  testthat::expect_equal(sum(empty$width), 0); testthat::expect_equal(sum(empty$counts), 0)
})

testthat::test_that("feature calibration improves a known example without changing roots", {
  cfg <- adaptive_config(); cfg$maximum_cutoffs <- 6
  paths <- adaptive_paths(); features <- data.frame(absolute_bowley = rep(c(0, .8), 50), log_tail_weight = 0)
  laws <- rep(c("a", "b"), each = 50); before <- mwd_hash(paths)
  fit <- mac_train(paths, features, laws, .995, .9, .95, cfg)
  testthat::expect_true(fit$available); testthat::expect_equal(fit$rule$feature, "absolute_bowley")
  rows <- mac_evaluate(paths, features, laws, fit$rule, .995)
  testthat::expect_true(all(rows$attained)); testthat::expect_equal(mean(rows$width), 3)
  testthat::expect_true(all(rows$width <= rows$reference_width)); testthat::expect_identical(mwd_hash(paths), before)
  cv <- mac_develop(paths, features, laws, rep(1:50, 2), .995, .9, .95, cfg)
  testthat::expect_true(cv$advance); testthat::expect_equal(cv$rule$feature, "absolute_bowley")
  testthat::expect_equal(sort(unique(cv$fold_rows$fold)), 1:5)
  constant <- mac_train(paths, features, laws, .995, .9, .95, cfg, allow_feature = FALSE)
  testthat::expect_equal(constant$rule$gamma_invalid, .995)
  testthat::expect_lte(constant$rule$gamma_low, .995)
})

testthat::test_that("held-out folds never choose their own splits or cutoff menus", {
  develop <- mac_develop; environment(develop) <- new.env(parent = environment(mac_develop)); seen <- list()
  environment(develop)$mac_train <- function(paths, features, laws, gamma, content, confidence, cfg,
    allow_feature = TRUE, references = NULL) {
    seen[[length(seen) + 1L]] <<- features$marker
    list(rule = mac_constant(gamma), available = FALSE)
  }
  features <- data.frame(absolute_bowley = 0, log_tail_weight = 0, marker = 1:20)
  develop(adaptive_paths(20), features, rep("a", 20), 1:20, .995, .9, .95, adaptive_config())
  for (k in 1:5) for (j in 1:2) {
    heldout <- which((1:20 - 1) %% 5 + 1 == k)
    testthat::expect_length(intersect(seen[[2*(k - 1) + j]], heldout), 0)
  }
})

testthat::test_that("unavailable proposals retain the original rule without relabeling", {
  paths <- lapply(adaptive_paths(20), function(p) {p$attained <- rep(FALSE, nrow(p)); p$population_content <- .8; p})
  features <- data.frame(absolute_bowley = rep(c(0, .8), 10), log_tail_weight = 0)
  cfg <- adaptive_config(); cfg$maximum_cutoffs <- 4
  fit <- mac_train(paths, features, rep("a", 20), .995, .9, .95, cfg)
  testthat::expect_false(fit$available); testthat::expect_equal(fit$rule$gamma_low, .995)
  cv <- mac_develop(paths, features, rep("a", 20), 1:20, .995, .9, .95, cfg)
  testthat::expect_false(cv$advance); testthat::expect_equal(cv$kind, "reference")
})

testthat::test_that("grid gains use the original rather than enriched reference width", {
  paths <- adaptive_paths(10); references <- mac_references(paths, .995); references$reference_width <- 10
  rows <- mac_evaluate(paths, data.frame(absolute_bowley = rep(0, 10)), rep("a", 10),
    mac_constant(.995), .995, references)
  testthat::expect_equal(mean(rows$width/rows$reference_width), .6)
  testthat::expect_error(mac_evaluate(paths, data.frame(absolute_bowley = rep(0, 10)), rep("a", 10),
    mac_constant(.9999), .995), "exceeds")
})

testthat::test_that("closed-interval counts include observations at either endpoint", {
  y <- c(-1, -1, 0, 1, 2)
  d <- data.frame(lower = c(-1, 0), upper = c(1, 2), observed_count = c(4L, 3L))
  d$posterior_content_probability <- mjc_probabilities(y, d$lower, d$upper, .9, 1, 4)
  testthat::expect_true(mac_dp_check(y, d, .9, adaptive_config()))
  d$observed_count[1] <- 3
  testthat::expect_error(mac_dp_check(y, d, .9, adaptive_config()), "closed-interval")
})

testthat::test_that("freezing protects priors, rules and sample assignments", {
  directory <- tempfile(); dir.create(directory); on.exit(unlink(directory, recursive = TRUE))
  cfg <- adaptive_config(); cfg$output_root <- directory
  d <- list(n = 100L, content = .9, tolerance_confidence = .95, gamma_reference = .995,
    cutoff = mac_constant(.995), refine = FALSE, choice = "unchanged_reference")
  p <- list(designs = list(d), advancing_designs = 0L)
  tasks <- data.frame(job_id = 1:2, data_seed = c(10L, 11L)); identity <- list(source_commit = "fixture")
  result <- mac_freeze(p, tasks, cfg, identity)
  testthat::expect_identical(mac_freeze(p, tasks, cfg, identity), result)
  bad <- p; bad$designs[[1]]$true_family <- "normal"
  testthat::expect_error(mac_policy_check(bad), "development information")
  bad <- p; bad$designs[[1]]$cutoff$gamma_low <- .999
  testthat::expect_error(mac_policy_check(bad), "more restrictive")
  bad <- p; bad$designs <- c(bad$designs, bad$designs)
  testthat::expect_error(mac_policy_check(bad), "duplicated")
  tasks$data_seed[1] <- 12
  testthat::expect_error(mac_freeze(p, tasks, cfg, identity), "tasks changed")
})

testthat::test_that("new random seeds reject historical and within-study overlap", {
  tasks <- data.frame(data_seed = c(1L, 2L), ecm_seed = c(11L, 12L))
  testthat::expect_true(mac_seed_check(tasks, c(100, 200)))
  testthat::expect_error(mac_seed_check(tasks, c(1, 200)), "overlap")
  tasks$ecm_seed[1] <- 2L; testthat::expect_error(mac_seed_check(tasks, integer()), "overlap")
})

testthat::test_that("fitters receive no population CDF, family name or evaluation result", {
  task <- data.frame(n = 100, content = .9, tolerance_confidence = .95, retained_fraction = .98,
    ecm_seed = 10L, response_id = "normal", attained = TRUE)
  context <- list(laws = list(normal = list(p = pnorm)), dgps = list(), policy = data.frame(),
    prior = list(simulation_reference = list(), numerical = list(), stages = list()),
    profile = list(), dp = list(), policy_diagnostics = list(), population_content = .99)
  inputs <- mac_fitting_inputs(task, context)
  testthat::expect_false(any(c("response_id", "attained") %in% names(inputs$task)))
  testthat::expect_false(any(c("laws", "dgps", "population_content") %in% names(inputs$context)))
  testthat::expect_false("stages" %in% names(inputs$context$prior))
})

testthat::test_that("the native and R root fits agree after the same conditional checks", {
  private <- file.path(root, "application/cache/gamma_scale_library")
  if (dir.exists(private)) withr::local_libpaths(c(private, .libPaths()))
  testthat::skip_if_not_installed("rqrgibbs")
  controls <- mwd_json(file.path(root, "application/config/mti_fixed_prior_20261114.json"))$numerical
  y <- c(-2, -1, -.5, -.1, .1, .5, 1, 2, 4, 8)
  for (q in c(.95, .999)) for (delta in c(0, .02)) {
    fits <- lapply(c("R", "cpp"), function(backend) {
      fit <- rqrgibbs::rqr_ecm_fit(y, matrix(1, length(y), 1), q, mean_tilt = delta,
        beta_prior_obj = rqrgibbs::beta_prior("ridge", ridge = list(tau2 = 4)),
        ecm_control = list(ecm_backend = backend, max_iter = 100, multistart = FALSE))
      mfp_polish(y, sort(c(fit$beta_root1, fit$beta_root2)), q, delta, 4, controls)
    })
    testthat::expect_true(all(vapply(fits, function(f) f$accuracy$pass, FALSE)))
    testthat::expect_equal(fits[[1]]$roots, fits[[2]]$roots, tolerance = 1e-7)
  }
})

testthat::test_that("added pairs are bounded, interior and absent from the original grid", {
  private <- file.path(root, "application/cache/gamma_scale_library")
  if (dir.exists(private)) withr::local_libpaths(c(private, .libPaths()))
  testthat::skip_if_not_installed("rqrgibbs")
  cfg <- adaptive_config(); d <- adaptive_candidates(); before <- mwd_hash(d)
  pairs <- mac_propose_pairs(seq_len(20), d, .9, .995, list(profile = list(tilt_grid_control = list())), cfg)
  testthat::expect_gt(nrow(pairs), 0); testthat::expect_lte(nrow(pairs), cfg$refinement$maximum_new_pairs)
  testthat::expect_true(all(pairs$target_content > .9 & pairs$target_content < 1))
  key <- function(z) paste(z$target_content, z$mean_tilt)
  testthat::expect_length(intersect(key(pairs), key(d)), 0)
  testthat::expect_identical(mwd_hash(d), before)
})

testthat::test_that("a no-gain conclusion is independently verified and corruption is detected", {
  directory <- tempfile(); dir.create(directory); on.exit(unlink(directory, recursive = TRUE))
  cfg <- adaptive_config(); cfg$output_root <- directory; cfg$expected_cells <- 1; cfg$development_replications <- 2
  source <- file.path(directory, "original"); dir.create(file.path(source, "validation/fits"), recursive = TRUE)
  tasks <- data.frame(n = 50L, content = .9, tolerance_confidence = .95, response_id = "normal",
    replication = 1:2, data_seed = c(71L, 72L), job_id = 1:2, ecm_seed = c(91L, 92L), retained_fraction = 1, cell_id = 1L)
  for (i in 1:2) {
    task <- tasks[i, , drop = FALSE]; set.seed(task$data_seed); y <- rnorm(task$n)
    d <- adaptive_candidates()[3, ]; rownames(d) <- NULL
    d$lower <- min(y); d$upper <- max(y); d$width <- diff(range(y)); d$observed_count <- length(y)
    d$posterior_content_probability <- mjc_probabilities(y, d$lower, d$upper, task$content, 1, 4)
    mbc_atomic_fit(list(ok = TRUE, task = task, unresolved_candidates = 0L, sample_sha256 = mwd_hash(y),
      prior = list(prior_sd = 2, center = 0, scale = 1, learning_rate = 1, data_adaptive_prior = FALSE), candidates = d),
      file.path(source, "validation/fits", sprintf("fit_%06d.rds", task$job_id)), "historical", task)
  }
  context <- list(source = list(directory = source), source_stage = "historical", source_tasks = tasks,
    laws = list(normal = list(r = rnorm, p = pnorm)), policy = data.frame(n = 50, content = .9, posterior_confidence = .5))
  program <- file.path(root, "application/config/mti_ecm_adaptive_calibration_20261120.json")
  identity <- list(source_commit = "fixture", programs = mgfp_hashes(program), article_files = mgfp_hashes(program),
    package = list(installed_files = mgfp_hashes(program)))
  mgfp_json_write(list(identity = identity, identity_hash = mwd_hash(identity)), file.path(directory, "manifest.json"))
  cases <- mac_cases(context); saveRDS(cases, file.path(directory, "audit_tasks.rds"))
  id <- mwd_hash(list(identity, "audit", cases)); dir.create(file.path(directory, "audit/fits"), recursive = TRUE)
  mgfp_json_write(list(analysis_id = id), file.path(directory, "audit/manifest.json"))
  case <- cases[1, , drop = FALSE]
  mbc_atomic_fit(mac_audit_case(case, cfg, context), file.path(directory, "audit/fits/fit_000001.rds"), id, case)
  saveRDS(list(designs = list(), advancing_designs = 0L), file.path(directory, "selected_development_rule.rds"))
  mgfp_json_write(list(status = "complete_no_development_gain", advancing_designs = 0L), file.path(directory, "study_summary.json"))
  testthat::expect_true(mac_verify(cfg, context, 1L)$passed)
  # Test the independent-assessment data path with controlled numerical/comparator fixtures.
  verify <- mac_verify; environment(verify) <- new.env(parent = environment(mac_verify))
  environment(verify)$mfp_accuracy <- function(...) list(pass = TRUE)
  benchmark <- function(y, content, confidence) data.frame(method = c("young_mathew", "wilks"),
    returned = TRUE, lower = min(y), upper = max(y), width = diff(range(y)), failure_reason = "")
  environment(verify)$mfp_comparators <- benchmark
  cfg$validation_replications <- 2
  fresh <- tasks; fresh$data_seed <- c(171L, 172L); fresh$ecm_seed <- c(191L, 192L)
  p <- list(designs = list(list(n = 50L, content = .9, tolerance_confidence = .95,
    gamma_reference = .5, cutoff = mac_constant(.5), refine = FALSE, choice = "fixture")), advancing_designs = 1L)
  saveRDS(p, file.path(directory, "selected_development_rule.rds"))
  frozen <- mac_freeze(p, fresh, cfg, identity)
  validation_id <- mwd_hash(list(c(identity, list(frozen_rule = mwd_hash(frozen))), "validation", fresh))
  dir.create(file.path(directory, "validation/fits"), recursive = TRUE)
  mgfp_json_write(list(analysis_id = validation_id), file.path(directory, "validation/manifest.json"))
  rows <- lapply(1:2, function(i) {
    task <- fresh[i, , drop = FALSE]; set.seed(task$data_seed); y <- rnorm(task$n)
    d <- adaptive_candidates()[3, ]; rownames(d) <- NULL
    d$lower <- min(y); d$upper <- max(y); d$width <- diff(range(y)); d$observed_count <- length(y)
    d$posterior_content_probability <- mjc_probabilities(y, d$lower, d$upper, task$content, 1, 4)
    comparison <- benchmark(y, task$content, task$tolerance_confidence); comparison$numerically_accurate <- TRUE
    value <- data.table::rbindlist(list(mac_interval(d, .5, "ecm_primary"),
      mac_interval(d, .5, "ecm_proposed"), comparison), fill = TRUE)
    value[, population_content := pnorm(upper) - pnorm(lower)]
    value[, attained := returned & population_content >= task$content]
    for (name in c("job_id", "n", "content", "tolerance_confidence", "response_id", "data_seed"))
      data.table::set(value, j = name, value = task[[name]])
    mbc_atomic_fit(list(ok = TRUE, unresolved_candidates = 0L, prior = list(prior_sd = 2, data_adaptive_prior = FALSE),
      task = task, sample_sha256 = mwd_hash(y), rule_sha256 = mwd_hash(frozen$designs[[1]]),
      candidates = d, original_candidates = d, rows = value),
      file.path(directory, "validation/fits", sprintf("fit_%06d.rds", task$job_id)), validation_id, task)
    value
  })
  all_rows <- data.table::rbindlist(rows); mac_report(all_rows, cfg)
  mgfp_json_write(list(status = "complete_requires_scientific_review", advancing_designs = 1L), file.path(directory, "study_summary.json"))
  testthat::expect_true(verify(cfg, context, 1L)$passed)
  pair_path <- file.path(directory, "validation/paired_comparison.csv")
  damaged <- data.table::fread(pair_path); damaged$mean_width_change[1] <- 1; mec_write_intervals(damaged, pair_path)
  testthat::expect_error(verify(cfg, context, 1L), "paired comparison")
  mac_report(all_rows, cfg)
  file <- file.path(source, "validation/fits/fit_000001.rds")
  saveRDS(list(changed = TRUE), file)
  testthat::expect_error(verify(cfg, context, 1L), "checksum")
})

testthat::test_that("width percentiles and Monte Carlo confidence intervals remain distinct", {
  d <- data.table::data.table(n = 100, content = .9, tolerance_confidence = .95, response_id = "a",
    method = "ecm_proposed", returned = TRUE, attained = c(rep(TRUE, 950), rep(FALSE, 50)),
    width = seq_len(1000), population_content = seq(.8, .99, length.out = 1000))
  s <- mac_summary(d, adaptive_config())
  testthat::expect_equal(s$attainment, .95); testthat::expect_lt(s$lower, .95); testthat::expect_gt(s$upper, .95)
  testthat::expect_equal(s$width_q025, unname(quantile(d$width, .025)))
  testthat::expect_equal(s$content_q975, unname(quantile(d$population_content, .975)))
  testthat::expect_equal(s$width_samples, 1000)
})

testthat::test_that("reports retain raw width ranges, means and paired changes", {
  directory <- tempfile(); dir.create(directory); on.exit(unlink(directory, recursive = TRUE))
  cfg <- adaptive_config(); cfg$output_root <- directory
  rows <- data.table::rbindlist(lapply(c("ecm_primary", "ecm_proposed", "young_mathew", "wilks"), function(m)
    data.frame(job_id = 1:5, n = 100, content = .9, tolerance_confidence = .95, response_id = "normal",
      method = m, lower = -1, upper = 1:5, width = 2:6, returned = TRUE, attained = TRUE, population_content = .95)))
  report <- mac_report(rows, cfg)
  testthat::expect_equal(nrow(report$summary), 4); testthat::expect_equal(nrow(report$paired), 3)
  testthat::expect_equal(report$paired$mean_width_change, c(0, 0, 0))
  testthat::expect_gt(file.info(file.path(directory, "validation/ecm_calibration_diagnostics.pdf"))$size, 1000)
  exported <- data.table::fread(file.path(directory, "validation/interval_results.csv"))
  testthat::expect_equal(exported$width, rows$width)
  missing <- data.frame(reason = c("", ""), interval = c(NA_real_, NA_real_), valid = c(TRUE, FALSE))
  path <- file.path(directory, "empty.csv"); mec_write_intervals(missing, path)
  testthat::expect_identical(as.data.frame(mac_read_table(path, missing)), missing)
})
