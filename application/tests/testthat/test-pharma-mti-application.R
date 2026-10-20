pma_test_env <- function() {
  e <- new.env(parent = globalenv())
  for (name in c("pharma_tolerance_application.R", "pharma_mti_application.R")) {
    sys.source(testthat::test_path("..", "..", "scripts", "lib", name), envir = e)
  }
  e
}
pma_test_config <- function(e) e$pma_read_config(testthat::test_path(
  "..", "..", "config", "pharma_tolerance_application_mti_only_20261020.json"))
pma_test_data <- function() data.frame(
  batch = seq_len(187), .source_row = seq_len(187), .analysis_row = seq_len(187),
  start = rep(letters[1:19], c(rep(10L, 18), 7L)),
  api_batch = rep(seq_len(53), c(rep(3L, 52), 31L)),
  fct_tensile = seq_len(187) / 100, tbl_rsd_weight = sqrt(seq_len(187)))
pma_test_results <- function(e) {
  cfg <- pma_test_config(e)
  cfg$analysis$smoke_splits <- 8L
  data <- pma_test_data()
  splits <- e$pma_make_splits(data, cfg, "smoke")
  context <- list(config = cfg, data = data, scan = list(retained_count = 98L,
    retained_fraction = .98))
  fake <- function(method, y, context, seed) {
    offset <- match(method, e$pma_methods) / 100
    list(returned = TRUE, lower = min(y) + offset,
      upper = max(y) + offset + (seed %% 5) / 100,
      selected_q = .98, selected_tilt = .01,
      posterior_content_probability = .999, posterior_confidence = .995,
      candidates_evaluated = 7L, feasible_count = 3L, ecm_converged = seed %% 2 == 0,
      ecm_iterations = 5L, ecm_final_stationarity = 1e-4, failure_reason = "")
  }
  rows <- lapply(splits, function(s) do.call(rbind, lapply(
    c("fct_tensile", "tbl_rsd_weight"), function(response) {
      e$pma_run_task(list(split = s, response = response,
        role = if (response == "fct_tensile") "primary" else "supplement"),
        context, fit_function = fake)
    })))
  list(results = do.call(rbind, rows), splits = splits, context = context, fake = fake)
}

test_that("revised application preserves its method and frozen calibration choices", {
  e <- pma_test_env()
  cfg <- pma_test_config(e)
  expect_identical(unlist(cfg$methods, use.names = FALSE), e$pma_methods)
  expect_false("tcsp_mc" %in% unlist(cfg$methods))
  expect_identical(cfg$method_roles$mti_ecm_adaptive_cell, "proposed")
  expect_identical(cfg$method_roles$young_mathew, "benchmark")
  expect_equal(cfg$scan_reference$expected_retained_count, 98)
  for (kind in c("policy", "validation_config")) {
    path <- if (kind == "policy") cfg$mti_ecm$policy_csv else cfg$mti_ecm$validation_config
    expected <- cfg$mti_ecm[[paste0(kind, "_sha256")]]
    path <- testthat::test_path("..", "..", sub("^application/", "", path))
    expect_invisible(e$pma_verify_hash(path, expected))
  }
  bad <- cfg
  bad$methods <- c(cfg$methods, "tcsp_mc")
  expect_error(e$pma_validate_config(bad), "three approved methods")
  expect_error(e$pma_fit("tcsp_mc", 1:100, NULL, 1), "Unapproved")
  temp <- tempfile()
  writeLines("changed", temp)
  expect_error(e$pma_verify_hash(temp, cfg$mti_ecm$policy_sha256), "hash mismatch")
  unlink(temp)
})

test_that("all split schemes are reproducible, response-independent, and correctly sized", {
  e <- pma_test_env()
  cfg <- pma_test_config(e)
  data <- pma_test_data()
  a <- e$pma_make_splits(data, cfg, "smoke")
  set.seed(888)
  data$fct_tensile <- rev(data$fct_tensile)
  b <- e$pma_make_splits(data, cfg, "smoke")
  expect_identical(a, b)
  for (s in a) {
    expect_length(s$train, 100L)
    expect_length(s$heldout, 87L)
    expect_length(intersect(s$train, s$heldout), 0L)
    expect_setequal(c(s$train, s$heldout), seq_len(187))
    if (s$scheme %in% c("month_grouped", "api_grouped")) {
      v <- cfg$sensitivity$grouped_variables[[s$scheme]]
      expect_length(intersect(data[[v]][s$train], data[[v]][s$heldout]), 0L)
    }
  }
  random <- Filter(function(s) s$scheme == "random", a)
  for (s in random) {
    set.seed(s$seed)
    expect_identical(s$train, sort(sample.int(187, 100)))
  }
  forward <- Filter(function(s) s$scheme == "chronological_forward", a)[[1]]
  reverse <- Filter(function(s) s$scheme == "chronological_reverse", a)[[1]]
  expect_identical(forward$train, 1:100)
  expect_identical(reverse$train, 88:187)
  groups <- e$pma_group_partitions(rep(1:4, each = 2), 4, 250, 123)
  expect_length(groups, 6L)
  expect_equal(attr(groups, "available_partitions"), 6)
  expect_length(unique(vapply(groups, function(s) paste(s$train, collapse = ","), "")), 6L)
})

test_that("canonical results retain failures and evaluate closed held-out intervals", {
  e <- pma_test_env()
  f <- pma_test_results(e)
  expect_invisible(e$pma_validate_results(f$results, f$splits))
  expect_setequal(unique(f$results$method_id), e$pma_methods)
  expect_equal(f$results$width, f$results$upper - f$results$lower)
  expect_true(all(f$results$scan_reference_retained_count == 98L))
  broken <- f$fake
  fail <- function(method, y, context, seed) {
    if (method == "young_mathew") stop("injected numerical failure")
    broken(method, y, context, seed)
  }
  task <- list(split = f$splits[[1]], response = "fct_tensile", role = "primary")
  z <- e$pma_run_task(task, f$context, fit_function = fail)
  expect_equal(nrow(z), 3L)
  ym <- z[z$method_id == "young_mathew", ]
  expect_false(ym$interval_returned)
  expect_false(ym$heldout_attains_content)
  expect_match(ym$failure_reason, "injected numerical failure")
  expect_true(is.na(ym$width))
  summary <- e$pma_summary(z)
  expect_equal(summary$heldout_attainment_rate[summary$method_id == "young_mathew"], 0)
  expect_equal(summary$interval_return_rate[summary$method_id == "young_mathew"], 0)
  expect_error(e$pma_validate_results(rbind(z, z)), "Duplicate")
  interval <- rqrgibbs::rqr_interval_empirical_content(c(1, 2, 3), 1, 3)
  expect_equal(interval$empirical_content, 1)
  expect_equal(interval$lower_omitted + interval$upper_omitted, 0)
})

test_that("paired summaries and convergence sensitivities use matched splits", {
  e <- pma_test_env()
  f <- pma_test_results(e)
  z <- f$results
  summary <- e$pma_summary(z)
  for (i in seq_len(nrow(summary))) {
    s <- summary[i, ]
    x <- z[z$response_id == s$response_id & z$split_scheme == s$split_scheme &
             z$method_id == s$method_id, ]
    expect_equal(s$width_median, median(x$width))
    expect_equal(s$heldout_attainment_rate, mean(x$heldout_attains_content))
  }
  paired <- e$pma_paired_values(z)
  expect_equal(paired$width_difference, paired$width_mti - paired$width_benchmark)
  expect_equal(paired$width_ratio, paired$width_mti / paired$width_benchmark)
  expect_equal(paired$heldout_upper_omitted_difference,
               paired$heldout_upper_omitted_mti - paired$heldout_upper_omitted_benchmark)
  expect_equal(nrow(paired), nrow(z) / 3L * 2L)
  p <- e$pma_paired_summary(z)
  expect_equal(p$iqr, p$q75 - p$q25)
  sensitivity <- e$pma_convergence_sensitivity(z)
  expect_true(all(sensitivity$splits <= sensitivity$original_splits))
  random <- sensitivity[sensitivity$split_scheme == "random", ]
  expect_true(all(random$splits == 4L))
})

test_that("representative selection minimizes the specified median-distance score", {
  e <- pma_test_env()
  z <- pma_test_results(e)$results
  a <- e$pma_representative(z)
  expect_equal(a, e$pma_representative(z[nrow(z):1L, ]))
  for (i in seq_len(nrow(a))) {
    rows <- z[z$response_id == a$response_id[i] & z$split_scheme == "random", ]
    ids <- sort(unique(rows$split_id))
    vars <- c("lower", "upper", "width", "heldout_empirical_content",
              "heldout_lower_omitted", "heldout_upper_omitted")
    x <- t(vapply(ids, function(id) {
      r <- rows[rows$split_id == id, ]
      as.vector(t(as.matrix(r[match(e$pma_methods, r$method_id), vars])))
    }, numeric(18)))
    med <- apply(x, 2, median)
    scales <- apply(x, 2, mad)
    for (j in seq_along(scales)) {
      if (scales[j] == 0) scales[j] <- diff(quantile(x[, j], c(.25, .75), type = 8))
      if (scales[j] == 0) scales[j] <- 1
    }
    scores <- rowSums(sweep(sweep(x, 2, med), 2, scales, "/")^2)
    expect_equal(a$score[i], min(scores))
    expect_equal(a$split_id[i], min(ids[scores == min(scores)]))
    expect_true(all(rows$interval_returned[rows$split_id == a$split_id[i]]))
  }
})

test_that("cached public data pass the full provenance check when available", {
  e <- pma_test_env()
  cfg <- pma_test_config(e)
  root <- normalizePath(testthat::test_path("..", "..", ".."))
  skip_if_not(file.exists(file.path(root, cfg$paths$raw_file)), "Public data are verified by application preflight")
  context <- e$pma_context(cfg, root, prepare = FALSE)
  expect_equal(nrow(context$data), 187L)
  expect_equal(context$scan$retained_count, 98L)
  expect_false(anyNA(context$data[, c("fct_tensile", "tbl_rsd_weight")]))
})

test_that("application language preserves the distinct statistical quantities", {
  e <- new.env(parent = globalenv())
  sys.source(testthat::test_path("..", "..", "scripts", "lib", "pharma_manuscript_language.R"), envir = e)
  bad <- c("Held-out empirical content is tolerance confidence.",
    "Posterior content probability guarantees nominal tolerance confidence.",
    "Generalized-posterior endpoint summaries are posterior predictive intervals.",
    "The fitted intervals are specification limits.",
    "The proposed Young--Mathew method is used.",
    "TCSP is also fitted.", "Overlapping splits are independent replications.")
  expect_true(all(vapply(bad, function(x) length(e$pma_language_findings(x)) > 0, logical(1))))
  expect_length(e$pma_language_findings(paste(
    "Held-out empirical content describes the finite dataset.",
    "Nominal tolerance confidence concerns the procedure under its assumptions.",
    "These intervals are not specification limits.")), 0L)
})

test_that("task seeds make evaluation independent of worker count and ordering", {
  e <- pma_test_env()
  f <- pma_test_results(e)
  tasks <- lapply(1:2, function(i) list(split=f$splits[[i]], response="fct_tensile", role="primary"))
  run <- function(task) e$pma_run_task(task, f$context, fit_function=f$fake)
  serial <- do.call(rbind, lapply(tasks, run))
  reverse <- do.call(rbind, lapply(rev(tasks), run))
  canonical <- function(z) {
    z <- z[order(z$split_id,z$method_id), setdiff(names(z),"elapsed_sec")]
    rownames(z) <- NULL
    z
  }
  expect_equal(canonical(serial), canonical(reverse))
  if (.Platform$OS.type != "windows") {
    parallel <- do.call(rbind, parallel::mclapply(tasks, run, mc.cores=2, mc.set.seed=FALSE))
    expect_equal(canonical(serial), canonical(parallel))
  }
})
test_that("portable endpoints preserve ties and double precision", {
  e <- pma_test_env()
  endpoint <- 1.503652687 - .Machine$double.eps
  x <- data.frame(lower = c(1, NA_real_), upper = c(endpoint, 2),
                  label = c("interval", "failed"))
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  e$pma_write_csv(x, path)
  y <- e$pta_read_csv(path)
  expect_identical(y$upper, x$upper)
  expect_true(is.na(y$lower[2]))
  expect_true(y$upper[1] < 1.503652687)
})
