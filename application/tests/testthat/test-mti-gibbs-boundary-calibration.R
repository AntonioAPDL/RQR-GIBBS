root <- normalizePath(if (file.exists("application/scripts/lib/mti_gibbs_boundary_calibration.R")) "." else "../../..")
for (name in c("mti_gibbs_full_report_review", "mti_gibbs_full_publication", "mti_gibbs_width_diagnosis",
  "mti_gibbs_practical", "mti_prior_screen", "mti_gibbs_boundary_calibration", "mti_gibbs_boundary_study"))
  source(file.path(root, "application/scripts/lib", paste0(name, ".R")))

testthat::test_that("interior projections remove flat outward directions without changing q or priors", {
  y <- seq(-1, 4, length.out = 100); q <- .93; limits <- mwd_limits(y, q)
  for (variant in c("log_rank", "sqrt_rank")) {
    r <- mbc_project_tilt(y, q, limits[1], variant)
    testthat::expect_gt(r$delta, limits[1])
    testthat::expect_lt(r$delta, limits[2])
    testthat::expect_gt(r$outward_slope, 0)
    testthat::expect_equal(r$target_class, "boundary")
    other <- unname(quantile(y, q, type = 1))
    change <- mwd_loss(y, min(y)-2, other, q, r$delta) -
      mwd_loss(y, min(y)-1, other, q, r$delta)
    testthat::expect_equal(change, r$outward_slope, tolerance = 1e-10)
    translated <- mbc_project_tilt(7 + 3*y, q, 3*limits[1], variant)
    testthat::expect_equal(translated$delta, 3*r$delta, tolerance = 1e-12)
    middle <- mean(limits)
    testthat::expect_equal(mbc_project_tilt(y, q, middle, variant)$delta, middle)
  }
  testthat::expect_equal(mbc_project_tilt(y, q, limits[1]-1, "original")$delta, unname(limits[1]-1))
  testthat::expect_equal(mbc_project_tilt(y, q, limits[1]-1, "log_rank")$target_class, "outside")
  testthat::expect_error(mbc_project_tilt(rep(1, 100), q, 0, "log_rank"), "nonconstant")
  testthat::expect_error(mbc_project_tilt(y, q, 0, "unknown"), "Unknown")
})

testthat::test_that("calibration rank has the exact simultaneous binomial assurance", {
  r <- mbc_rank(200L, .95, .05, 72L)
  testthat::expect_equal(r$rank, 199L)
  testthat::expect_gte(r$assurance, 1-.05/72)
  testthat::expect_lt(pbinom(r$rank-2L, 200L, .95), 1-.05/72)
  testthat::expect_equal(r$assurance, pbeta(.95, r$rank, 201-r$rank, lower.tail = FALSE))
  for (m in c(20L, 50L, 100L)) {
    rank <- mbc_rank(m, .8, .1, 1)
    testthat::expect_equal(rank$assurance, pbeta(.8, rank$rank, m+1-rank$rank, lower.tail = FALSE))
  }
  testthat::expect_error(mbc_rank(10, .95, .05, 72), "cannot support")
  testthat::expect_error(mbc_rank(0, .95, .05, 72), "Invalid")
})

testthat::test_that("radius inversion returns a feasible upper bracket and the decision is nested", {
  r <- mbc_required_scale(pnorm, 0, 1, .95)
  testthat::expect_gte(pnorm(r)-pnorm(-r), .95)
  testthat::expect_equal(r, qnorm(.975), tolerance = 1e-7)
  exp_cdf <- function(x) pexp(x+1)
  for (center in c(-2, 0, 3)) {
    s <- mbc_required_scale(exp_cdf, center, .5, .9)
    interval <- mbc_interval(center, .5, s)
    testthat::expect_gte(exp_cdf(interval[2])-exp_cdf(interval[1]), .9)
    outer <- mbc_interval(center, .5, 2*s)
    testthat::expect_lte(outer[1], interval[1])
    testthat::expect_gte(outer[2], interval[2])
  }
  testthat::expect_error(mbc_interval(0, 0, 1), "positive radii")
  testthat::expect_error(mbc_required_scale(function(x) NA_real_, 0, 1, .9), "invalid content")
})

testthat::test_that("development selection is by design and calibration is complete and worst-law", {
  cfg <- jsonlite::read_json(file.path(root, "application/config/mti_gibbs_boundary_calibration_20261115.json"),
    simplifyVector = TRUE)
  scan <- data.table::fread(file.path(root, "tables/tolerance_validation_article_scan_calibration.csv"))
  context <- list(scan = as.data.frame(scan), laws = setNames(rep(list(NULL), 8), paste0("law", 1:8)))
  a <- cfg$stages$calibration; b <- cfg$stages$pilot
  make_design <- function(s) mps_design(list(expected_cells = 72L, replications_per_cell = s$replications,
    data_seed_base = s$data_seed_base, kappa_grid = 2), context)
  calibration <- make_design(a); pilot <- make_design(b)
  testthat::expect_equal(nrow(calibration), 14400L)
  testthat::expect_equal(nrow(pilot), 7200L)
  testthat::expect_length(intersect(calibration$data_seed, pilot$data_seed), 0L)
  rows <- data.table::as.data.table(calibration)
  rows[, `:=`(required_scale = 1+replication/1000+cell_id/10000, variant = "sqrt_rank", adequate = TRUE)]
  result <- mbc_calibrate(rows, cfg)
  testthat::expect_equal(nrow(result$policy), 9L)
  testthat::expect_true(all(result$policy$rank == 199L & result$policy$multiplier > 1))
  for (i in seq_len(nrow(result$policy))) {
    z <- result$policy[i]
    testthat::expect_equal(z$multiplier, max(result$cells[n == z$n & content == z$content, scale]))
  }
  testthat::expect_error(mbc_calibrate(rows[-1], cfg), "every planned sample")
  rows$adequate[1] <- FALSE
  testthat::expect_error(mbc_calibrate(rows, cfg), "unresolved")
  dev <- data.table::CJ(response_id = c("a", "b"), n = 100L, content = .9,
    variant = c("original", "log_rank", "sqrt_rank"), replication = 1:12)
  dev[, `:=`(tolerance_confidence = .95, adequate = TRUE, required_scale = 1,
    width = c(original = 2, log_rank = 1, sqrt_rank = 1)[variant], ecm_lower = 0, ecm_upper = 2)]
  choice <- mbc_development_choice(dev, cfg$variants)
  testthat::expect_equal(choice$policy$variant, "log_rank")
  testthat::expect_false("response_id" %in% names(choice$policy))
})

testthat::test_that("checkpoint records reject changed tasks, identities, and file contents", {
  directory <- tempfile(); dir.create(directory); on.exit(unlink(directory, recursive = TRUE))
  path <- file.path(directory, "fit.rds"); task <- data.frame(job_id = 1L)
  mbc_atomic_fit(list(ok = TRUE), path, "source", task)
  testthat::expect_true(mbc_read_fit(path, "source", task)$ok)
  testthat::expect_error(mbc_read_fit(path, "different", task), "identity or checksum")
  testthat::expect_error(mbc_read_fit(path, "source", data.frame(job_id = 2L)), "identity or checksum")
  saveRDS(list(ok = FALSE), path)
  testthat::expect_error(mbc_read_fit(path, "source", task), "identity or checksum")
})

testthat::test_that("resuming preserves complete fits and refuses changed or unresolved results", {
  directory <- tempfile(); dir.create(directory); on.exit(unlink(directory, recursive = TRUE))
  calls <- 0L
  queue <- mbc_queue
  environment(queue) <- new.env(parent = environment(mbc_queue))
  environment(queue)$mbc_job <- function(task, ...) {
    calls <<- calls + 1L
    list(ok = TRUE, rows = data.frame(job_id = task$job_id, adequate = TRUE))
  }
  tasks <- data.frame(job_id = 1:2, n = 100L, content = .9, tolerance_confidence = .95)
  cfg <- list(output_root = directory, variants = "original", disk_reserve_gib = 0)
  first <- queue(tasks, "development", cfg, list(), "source", 1L)
  testthat::expect_equal(calls, 2L)
  again <- queue(tasks, "development", cfg, list(), "source", 1L)
  testthat::expect_identical(first$rows, again$rows)
  testthat::expect_equal(calls, 2L)
  testthat::expect_error(queue(tasks, "development", cfg, list(), "changed", 1L), "identity changed")
  altered <- tasks; altered$n[1] <- 50L
  testthat::expect_error(queue(altered, "development", cfg, list(), "source", 1L), "identity or checksum")
  testthat::expect_error(queue(tasks[c(1, 1), ], "other", cfg, list(), "source", 1L), "unique")
  path <- file.path(directory, "development/fits/fit_000001.rds")
  unlink(paste0(path, ".json"))
  testthat::expect_error(queue(tasks, "development", cfg, list(), "source", 1L), "lacks a receipt")
  failed <- file.path(directory, "failed/fits"); dir.create(failed, recursive = TRUE)
  identity <- mwd_hash(list("source", "failed", NULL))
  mbc_atomic_fit(list(ok = FALSE, reason = "numerical review"), file.path(failed, "fit_000001.rds"),
    identity, tasks[1, ])
  testthat::expect_error(queue(tasks[1, ], "failed", cfg, list(), "source", 1L), "will not be replaced")
})

testthat::test_that("true CDF evaluation cannot change the posterior endpoint summary", {
  y <- seq(-1, 1, length.out = 100)
  task <- data.frame(job_id = 1L, response_id = "test", n = 100L, content = .9,
    tolerance_confidence = .95, replication = 1L, data_seed = 1L)
  parameters <- data.frame(variable = c("midpoint", "width"), mean = c(0, 2),
    preferred_pass = TRUE, mean_precision = TRUE, rhat = 1, ess_bulk = 1500, ess_tail = 1500)
  fits <- list(list(draws = cbind(-1 + y/100, 1 + y/100)))
  anchor <- list(q = .95, delta_reference = 0, selected = list(lower = -1, upper = 1))
  geometry <- mbc_project_tilt(y, .95, 0, "original")
  a <- mbc_rows(task, "original", geometry, fits, parameters, anchor, y, pnorm)
  b <- mbc_rows(task, "original", geometry, fits, parameters, anchor, y, function(x) pnorm(x/2))
  fixed <- c("center", "halfwidth", "lower", "upper", "width", "delta", "q")
  testthat::expect_identical(a[fixed], b[fixed])
  testthat::expect_gt(b$required_scale, a$required_scale)
})

testthat::test_that("pilot reporting preserves paired methods and raw width quantiles", {
  directory <- tempfile(); dir.create(directory); on.exit(unlink(directory, recursive = TRUE))
  dir.create(file.path(directory, "pilot/fits"), recursive = TRUE)
  tasks <- data.frame(job_id = 1:3, response_id = "test", n = 100L, content = .9,
    tolerance_confidence = .95)
  for (i in seq_len(nrow(tasks))) {
    half <- 1 + i/10
    rows <- data.table::data.table(variant = "original", center = 0, halfwidth = half,
      lower = -half, upper = half, ecm_lower = -2, ecm_upper = 2)
    comparators <- data.table::data.table(method = c("young_mathew", "wilks"),
      lower = c(-1.9, -2.1), upper = c(1.9, 2.1), returned = TRUE)
    mbc_atomic_fit(list(ok = TRUE, rows = rows, comparators = comparators),
      file.path(directory, "pilot/fits", sprintf("fit_%06d.rds", i)), "test", tasks[i, ])
  }
  cfg <- list(output_root = directory)
  context <- list(laws = list(test = list(p = pnorm)))
  policy <- data.table::data.table(n = 100L, content = .9, tolerance_confidence = .95,
    variant = "original", multiplier = 2)
  cells <- mbc_pilot_report(cfg, context, tasks, "test", policy)
  testthat::expect_equal(nrow(cells), 5L)
  testthat::expect_true(all(cells$attempts == 3L & cells$returned == 3L))
  testthat::expect_equal(cells[method == "gibbs_calibrated", mean_width], 4.8)
  testthat::expect_equal(cells[method == "gibbs_calibrated", width_q025], unname(quantile(4*c(1.1,1.2,1.3), .025)))
  testthat::expect_equal(cells[method == "gibbs_calibrated", attainment], 1)
  testthat::expect_true(file.info(file.path(directory, "pilot/pilot_review.pdf"))$size > 0)
  summary <- mwd_json(file.path(directory, "study_summary.json"))
  testthat::expect_false(summary$article_replaced)
  testthat::expect_false(summary$full_validation_launched)
})
