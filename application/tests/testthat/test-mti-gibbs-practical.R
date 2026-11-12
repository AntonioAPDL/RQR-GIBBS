root <- normalizePath(if (file.exists("application/scripts/lib/mti_gibbs_practical.R")) "." else "../../..")
source(file.path(root, "application/scripts/lib/mti_gibbs_practical.R"))

testthat::test_that("marginal root energy equals the direct product-loss posterior", {
  y <- c(-2, -.3, -.3, .1, 1.7, 4)
  for (q in c(.5, .9, .9999)) for (delta in c(-.3, 0, .3)) for (b in c(-2, 1, 20)) {
    z <- mgp_conditional(y, b, q, delta, omega = 1.3, precision = .0001)
    x <- c(seq(-50, 50, length.out = 91), y, z$mode)
    expected <- mgp_objective(y, x, rep(b, length(x)), q, delta, 1.3, .0001) -
      mgp_objective(y, z$mode, b, q, delta, 1.3, .0001)
    testthat::expect_equal(z$energy(x), expected, tolerance = 1e-8)
    testthat::expect_gte(min(z$energy(x)), -1e-8)
    for (height in c(.001, .2, 3, 100)) {
      bounds <- mgp_slice_interval(z, height)
      testthat::expect_equal(z$energy(bounds), rep(height, 2), tolerance = 1e-7)
      testthat::expect_lte(max(z$energy(seq(bounds[1], bounds[2], length.out = 101))),
        height + 1e-7)
    }
  }
})

testthat::test_that("conditional slice draws agree with independent direct-loss quadrature", {
  testthat::skip_if_not_installed("posterior")
  y <- c(-1, -.2, .1, .4, 1.5)
  cfg <- list(learning_rate = 1, prior_variance = 2)
  for (delta in c(-.3, 0, .3)) {
    z <- mgp_conditional(y, 1, .9, delta, precision = .5)
    ref <- mgp_conditional_reference(y, 1, .9, delta, cfg)
    set.seed(123)
    current <- z$mode; draws <- numeric(6000)
    for (i in seq_along(draws)) draws[i] <- current <- mgp_slice_update(current, z)
    for (p in c(.025, .5, .975)) {
      observed <- unname(quantile(tail(draws, 5000), p))
      testthat::expect_equal(ref$cdf(observed), p, tolerance = .035)
    }
  }
})

testthat::test_that("intercept slice-within-Gibbs agrees with a two-root density grid", {
  y <- c(-1, -.2, .5, 1.5)
  cfg <- list(n_burn = 1000L, n_mcmc = 5000L, learning_rate = 1, prior_variance = .7,
    initial_midpoint_offsets_sd = c(-.3, 0, .3, 0), initial_halfwidth_multipliers = c(.8, 1, 1.2, 1.5))
  task <- data.frame(seed = 22L, chain = 1L, ecm_lower = -1, ecm_upper = 1, q = .8, delta = -.2)
  fit <- mgp_slice_fit(y, task, cfg)
  x <- seq(-6, 6, length.out = 401)
  grid <- expand.grid(a = x, b = x)
  energy <- mgp_objective(y, grid$a, grid$b, task$q, task$delta, precision = 1 / .7)
  probability <- exp(-energy + min(energy)); probability <- probability / sum(probability)
  expected <- c(sum(pmin(grid$a, grid$b) * probability),
    sum(pmax(grid$a, grid$b) * probability), sum(abs(grid$a - grid$b) * probability))
  actual <- c(colMeans(fit$draws), mean(fit$draws[, 2] - fit$draws[, 1]))
  testthat::expect_equal(unname(actual), expected, tolerance = .06)
  testthat::expect_true(all(is.finite(fit$retained_objective)))
})

testthat::test_that("practical summaries keep unresolved cases and distinguish tail precision", {
  testthat::skip_if_not_installed("posterior")
  cfg <- jsonlite::read_json(file.path(root, "application/config/mti_gibbs_practical_20261112.json"),
    simplifyVector = TRUE)
  task <- data.frame(case_id = "fixture", dataset_id = 1L, source = "simulation",
    response_id = "fixture", n = 50, content = .9, q = .98, delta = 0, ecm_lower = -2, ecm_upper = 2)
  set.seed(11)
  fits <- lapply(1:4, function(i) list(draws = cbind(lower = rnorm(2000, -2, .1),
    upper = rnorm(2000, 2, .1))))
  d <- mgp_summaries(fits, task, cfg$policy)
  testthat::expect_true(all(d$screen_pass))
  testthat::expect_true(all(is.finite(d$mcse_q025)))
  testthat::expect_equal(nrow(d), 4)
  fits[[1]]$draws[, 1] <- fits[[1]]$draws[, 1] - 2
  bad <- mgp_summaries(fits, task, cfg$policy)
  testthat::expect_true(any(bad$status == "unresolved"))
  testthat::expect_equal(nrow(bad), 4)
})

testthat::test_that("the balanced design deduplicates diagnostic overlap and separates seeds", {
  tasks <- data.frame(dataset_id = 1:9, source = c(rep("simulation", 7), "application", "application"),
    replication = c(1:7, 1, 2), data_seed = 1:9)
  full <- tasks[rep(1:9, each = 4), ]; full$seed <- 1000 + seq_len(nrow(full))
  parent <- full[full$dataset_id %in% c(1, 3, 7, 8), ]; parent$seed <- parent$seed + 100
  design <- mgp_design(full, parent, list(replications_per_cell = 5, seed_offset = 10000))
  testthat::expect_equal(length(unique(design$dataset_id)), 7)
  testthat::expect_equal(length(unique(design$dataset_id[design$diagnostic_role == "balanced"])), 6)
  testthat::expect_equal(length(unique(design$dataset_id[design$diagnostic_role == "stress"])), 1)
  testthat::expect_false(anyDuplicated(design$seed) > 0)
  testthat::expect_error(mgp_design(full, parent, list(replications_per_cell = 5, seed_offset = 0)))
})

testthat::test_that("slice-chain seeds are independent of dispatch order", {
  cfg <- list(n_burn = 10L, n_mcmc = 20L, learning_rate = 1, prior_variance = 10,
    initial_midpoint_offsets_sd = c(-.3, 0, .3, 0), initial_halfwidth_multipliers = c(.8, 1, 1.2, 1.5))
  y <- c(-1, -.2, .3, 1.5)
  tasks <- data.frame(seed = c(1001L, 1002L), chain = 1:2,
    ecm_lower = -1, ecm_upper = 1, q = .95, delta = -.01)
  serial <- lapply(1:2, function(i) mgp_slice_fit(y, tasks[i, ], cfg))
  reversed <- parallel::mclapply(2:1, function(i) mgp_slice_fit(y, tasks[i, ], cfg),
    mc.cores = 2L, mc.set.seed = FALSE)
  testthat::expect_identical(serial[[1]]$draws, reversed[[2]]$draws)
  testthat::expect_identical(serial[[2]]$last$rng_state, reversed[[1]]$last$rng_state)
})
