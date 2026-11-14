root <- normalizePath(if (file.exists("application/scripts/lib/mti_prior_screen.R")) "." else "../../..")
source(file.path(root, "application/scripts/lib/mti_gibbs_width_diagnosis.R"))
source(file.path(root, "application/scripts/lib/mti_prior_screen.R"))

testthat::test_that("the root prior and learning rate change together under response units", {
  y <- c(-2, -.3, .1, .7, 2)
  p <- mps_spec(y, 3); z <- mps_spec(7 * y + 20, 3)
  testthat::expect_equal(z$z, p$z)
  testthat::expect_equal(z$prior_sd, 7 * p$prior_sd)
  testthat::expect_equal(z$learning_rate, p$learning_rate / 49)
  testthat::expect_true(p$data_adaptive_prior)
  testthat::expect_error(mps_spec(rep(1, 5), 3))
  testthat::expect_error(mps_spec(y, 0))
})

testthat::test_that("the prior study uses paired fresh samples and cannot launch validation", {
  cfg <- jsonlite::read_json(file.path(root, "application/config/mti_prior_screen_20261113.json"), simplifyVector = TRUE)
  scan <- data.frame(n = rep(c(50, 100, 500), each = 3), content = rep(c(.9, .95, .99), 3),
    tolerance_confidence = .95, retained_fraction = .99)
  context <- list(scan = scan, laws = setNames(vector("list", 8), letters[1:8]))
  tasks <- mps_design(cfg, context)
  testthat::expect_equal(nrow(tasks), 72 * 12 * 5)
  testthat::expect_equal(length(unique(tasks$data_seed)), 72 * 12)
  testthat::expect_equal(length(unique(head(tasks$cell_id, 72))), 72)
  testthat::expect_false(anyDuplicated(tasks$job_id) > 0)
  testthat::expect_false(cfg$automatic_prior_selection)
  testthat::expect_false(cfg$automatic_full_validation_launch)
  testthat::expect_false(cfg$automatic_article_replacement)
})

testthat::test_that("transforming the DP base preserves its original response prior", {
  y <- c(1.1, 1.3, 1.5, 1.8); p <- mps_spec(y, 3)
  low <- 1.2; high <- 1.7
  raw <- pnorm(high, 0, 4) - pnorm(low, 0, 4)
  transformed <- pnorm((high - p$center) / p$scale, -p$center / p$scale, 4 / p$scale) -
    pnorm((low - p$center) / p$scale, -p$center / p$scale, 4 / p$scale)
  testthat::expect_equal(raw, transformed)
})
