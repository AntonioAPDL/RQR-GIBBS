test_that("oracle certificate cross-checks profile and unrestricted minima", {
  certificate <- rqr_oracle_certificate(
    "gaussian", 0.80, grid_size = 201L
  )
  expect_identical(
    certificate$schema_version,
    "rqrgibbs_rqr_oracle_reference/1.0.0"
  )
  expect_lt(abs(certificate$coverage_residual), 1e-10)
  expect_lt(abs(certificate$moment_residual), 1e-7)
  expect_gt(certificate$endpoint_separation, 0)
  expect_lt(certificate$global_objective_gap, 1e-7)
  expect_gt(certificate$local_profile_curvature, 0)
  expect_true(certificate$unique_minimizer)
  expect_false(certificate$quadrature_error_is_rigorous_bound)
  expect_match(certificate$distribution_digest, "^[0-9a-f]{64}$")
  expect_match(certificate$solver_digest, "^[0-9a-f]{64}$")
  expect_equal(
    certificate$lower_root, -certificate$upper_root,
    tolerance = 1e-6
  )
})

test_that("skewed oracle certificate is deterministic and finite", {
  first <- rqr_oracle_certificate(
    "centered_standardized_lognormal", 0.80,
    params = list(logmean = 0, logsd = 0.75),
    grid_size = 201L
  )
  second <- rqr_oracle_certificate(
    "centered_standardized_lognormal", 0.80,
    params = list(logmean = 0, logsd = 0.75),
    grid_size = 201L
  )
  expect_identical(first, second)
  expect_true(all(is.finite(c(
    first$lower_root, first$upper_root, first$profile_objective,
    first$unrestricted_objective, first$estimated_quadrature_error
  ))))
  expect_lt(abs(first$coverage_residual), 1e-9)
  expect_lt(abs(first$moment_residual), 1e-6)
  expect_lt(first$global_objective_gap, 1e-6)
  expect_true(first$unique_minimizer)
})

test_that("RQR oracle risk and roots are location-scale equivariant", {
  coverage <- 0.90
  roots <- rqr_oracle_roots("gaussian", coverage)
  base_risk <- rqr_oracle_risk(
    roots$lower_root, roots$upper_root, "gaussian", coverage
  )$value
  mu <- c(-2, 0.5, 4)
  scale <- c(0.25, 2, 5)
  endpoints <- rqr_oracle_endpoints(
    mu, scale, "gaussian", coverage
  )
  expect_equal(
    endpoints$lower,
    mu + scale * roots$lower_root,
    tolerance = 0
  )
  expect_equal(
    endpoints$upper,
    mu + scale * roots$upper_root,
    tolerance = 0
  )
  z <- seq(-5, 5, length.out = 101L)
  for (index in seq_along(mu)) {
    y <- mu[[index]] + scale[[index]] * z
    transformed <- rqr_check_loss(
      (y - endpoints$lower[[index]]) *
        (y - endpoints$upper[[index]]),
      coverage
    )
    standard <- rqr_check_loss(
      (z - roots$lower_root) * (z - roots$upper_root),
      coverage
    )
    expect_equal(
      transformed, scale[[index]]^2 * standard,
      tolerance = 1e-12
    )
    expect_gt(scale[[index]]^2 * base_risk, 0)
  }
  expect_error(
    rqr_oracle_endpoints(0, 0, "gaussian", coverage),
    "positive"
  )
})

test_that("heavy-tail oracle refinements have stable risk and residuals", {
  families <- list(
    list(family = "student_t", params = list(df = 5, scale = sqrt(3 / 5))),
    list(
      family = "normal_t_mixture",
      params = list(
        normal_weight = 0.90, t_weight = 0.10,
        t_df = 3, t_shift = 2, t_scale = 1,
        variance_standardized = TRUE
      )
    )
  )
  for (spec in families) {
    for (coverage in c(0.80, 0.90)) {
      primary <- rqr_oracle_certificate(
        spec$family, coverage, params = spec$params,
        tol = 1e-10, grid_size = 1601L
      )
      refined <- rqr_oracle_certificate(
        spec$family, coverage, params = spec$params,
        tol = 1e-11, grid_size = 3201L
      )
      expect_lt(max(abs(c(
        primary$lower_root - refined$lower_root,
        primary$upper_root - refined$upper_root
      ))), 1e-6)
      expect_lte(
        abs(primary$profile_objective - refined$profile_objective),
        1e-8
      )
      expect_lte(abs(primary$coverage_residual), 1e-8)
      expect_lte(abs(primary$moment_residual), 1e-7)
    }
  }
})
