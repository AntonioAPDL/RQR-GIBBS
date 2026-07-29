test_that("static root canonicalization is a complete-block relabeling", {
  X_audit <- cbind(1, seq(-1, 1, length.out = 7))
  beta_lower <- rbind(
    c(-1.0, 0.10),
    c(-0.9, 0.12),
    c(-1.1, 0.08),
    c(-1.0, 0.11)
  )
  beta_upper <- rbind(
    c(1.6, 0.20),
    c(1.5, 0.19),
    c(1.7, 0.21),
    c(1.55, 0.18)
  )
  raw1 <- beta_lower
  raw2 <- beta_upper
  raw1[c(2, 4), ] <- beta_upper[c(2, 4), ]
  raw2[c(2, 4), ] <- beta_lower[c(2, 4), ]

  diag <- rqr_canonicalize_root_draws(
    raw1, raw2, X_audit,
    reference_beta_lower = colMeans(beta_lower),
    reference_beta_upper = colMeans(beta_upper)
  )
  expect_s3_class(diag, "rqr_root_label_diagnostics")
  expect_identical(diag$status, "ok")
  expect_equal(diag$canonical_beta_lower, beta_lower, tolerance = 1e-12)
  expect_equal(diag$canonical_beta_upper, beta_upper, tolerance = 1e-12)
  expect_identical(diag$assignment, c("keep", "swap", "keep", "swap"))

  diag_swapped <- rqr_canonicalize_root_draws(
    raw2, raw1, X_audit,
    reference_beta_lower = colMeans(beta_lower),
    reference_beta_upper = colMeans(beta_upper)
  )
  expect_identical(diag_swapped$status, "ok")
  expect_equal(diag_swapped$canonical_beta_lower,
               diag$canonical_beta_lower, tolerance = 1e-12)
  expect_equal(diag_swapped$canonical_beta_upper,
               diag$canonical_beta_upper, tolerance = 1e-12)
})

test_that("static root canonicalization fails closed for crossings", {
  X_audit <- cbind(1, seq(-1, 1, length.out = 9))
  raw1 <- matrix(c(0, -2), 1)
  raw2 <- matrix(c(0, 2), 1)
  diag <- rqr_canonicalize_root_draws(
    raw1, raw2, X_audit,
    reference_beta_lower = c(-2, 0),
    reference_beta_upper = c(2, 0),
    ambiguity_tolerance = 0
  )
  expect_identical(diag$status, "failed_draw_crossing_on_audit_domain")
  expect_null(diag$canonical_beta_lower)
  expect_error(
    rqr_canonicalize_root_draws(
      raw1, raw2, X_audit,
      reference_beta_lower = c(-2, 0),
      reference_beta_upper = c(2, 0),
      fail_on_ambiguous = TRUE
    ),
    "canonicalization failed"
  )
})

test_that("DLM path canonicalization uses one whole-path assignment per draw", {
  lower <- cbind(c(-2, -1, 0), c(-1.8, -0.8, 0.2))
  upper <- cbind(c(1, 2, 3), c(1.2, 2.1, 3.1))
  raw1 <- lower
  raw2 <- upper
  raw1[, 2] <- upper[, 2]
  raw2[, 2] <- lower[, 2]
  diag <- rqr_canonicalize_root_paths(
    raw1, raw2,
    reference_eta_lower = rowMeans(lower),
    reference_eta_upper = rowMeans(upper)
  )
  expect_s3_class(diag, "rqr_root_path_label_diagnostics")
  expect_identical(diag$status, "ok")
  expect_identical(diag$assignment, c("keep", "swap"))
  expect_equal(diag$canonical_eta_lower, lower, tolerance = 1e-12)
  expect_equal(diag$canonical_eta_upper, upper, tolerance = 1e-12)
})

test_that("static MCMC complete-root swap probability is configurable", {
  X <- cbind(1, seq(-1, 1, length.out = 10))
  y <- 0.1 + 0.2 * X[, 2] + seq(-0.05, 0.05, length.out = 10)
  no_swap <- rqr_mcmc_fit(
    y, X, coverage_level = 0.8,
    mcmc_control = list(
      n_burn = 2, n_mcmc = 3, seed = 771,
      root_label_control = list(
        swap_probability = 0, canonicalize_draws = FALSE
      )
    )
  )
  all_swap <- rqr_mcmc_fit(
    y, X, coverage_level = 0.8,
    mcmc_control = list(
      n_burn = 2, n_mcmc = 3, seed = 771,
      root_label_control = list(
        swap_probability = 1, canonicalize_draws = FALSE
      )
    )
  )
  expect_false(any(no_swap$diagnostics$root_swap_trace))
  expect_true(all(all_swap$diagnostics$root_swap_trace))
  expect_identical(no_swap$model_spec$root_estimand, "unordered_root_pair")
  expect_false(no_swap$model_spec$raw_root_labels_identified)
  expect_equal(all_swap$model_spec$root_swap_probability, 1)
  expect_error(
    rqr_posterior_draws(no_swap, root_representation = "canonical"),
    "Canonical coefficient draws are unavailable"
  )
})
