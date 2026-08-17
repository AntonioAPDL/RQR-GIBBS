test_that("AL interval-oracle certificates satisfy the complete target matrix", {
  for (tau in c(0.20, 0.50, 0.80)) {
    params <- list(
      tau = tau, scale = 1, variance_standardized = TRUE
    )
    for (coverage in c(0.80, 0.90, 0.95)) {
      certificates <- lapply(c("RQR", "ET", "SH"), function(target) {
        rqr_interval_oracle(
          "asymmetric_laplace", coverage, target,
          params = params, tol = 1e-10, grid_size = 201L
        )
      })
      names(certificates) <- c("RQR", "ET", "SH")
      for (certificate in certificates) {
        expect_s3_class(certificate, "rqr_interval_oracle")
        expect_identical(
          certificate$schema_version,
          "rqrgibbs_interval_oracle/2.0.0"
        )
        expect_identical(
          certificate$tilt_definition,
          "conditional_retained_mean_minus_population_mean"
        )
        expect_false(certificate$uses_cornish_fisher)
        expect_lt(abs(certificate$content_residual), 1e-10)
        expect_lt(abs(certificate$retained_mean_residual), 1e-10)
        expect_lt(abs(certificate$numerical_moment_gap), 1e-9)
        expect_false(certificate$quadrature_error_is_rigorous_bound)
        expect_true(certificate$unique_minimizer)
        expect_s3_class(certificate$minimizer_set, "data.frame")
        expect_gte(nrow(certificate$minimizer_set), 1L)
        expect_match(certificate$distribution_digest, "^[0-9a-f]{64}$")
        expect_match(certificate$solver_digest, "^[0-9a-f]{64}$")
        expect_match(certificate$certificate_digest, "^[0-9a-f]{64}$")
        spec <- rqrgibbs:::.rqr_oracle_family_spec(
          "asymmetric_laplace", params
        )
        expect_equal(
          stats::uniroot(
            function(z) spec$F(z) - certificate$lower_probability,
            c(certificate$lower_root - 2, certificate$lower_root + 2),
            tol = .Machine$double.eps^0.75,
            extendInt = "yes"
          )$root,
          certificate$lower_root,
          tolerance = 1e-10
        )
        expect_equal(
          stats::uniroot(
            function(z) spec$F(z) - certificate$upper_probability,
            c(certificate$upper_root - 2, certificate$upper_root + 2),
            tol = .Machine$double.eps^0.75,
            extendInt = "yes"
          )$root,
          certificate$upper_root,
          tolerance = 1e-10
        )
      }
      expect_equal(
        certificates$ET$lower_probability,
        (1 - coverage) / 2,
        tolerance = 0
      )
      expect_equal(
        certificates$SH$lower_probability,
        tau * (1 - coverage),
        tolerance = 0
      )
      expect_lt(abs(certificates$SH$density_residual), 1e-10)
      expect_lt(abs(certificates$SH$shortest_optimizer_gap), 2e-8)
      expect_identical(certificates$RQR$mean_tilt, 0)
      if (identical(tau, 0.50)) {
        expect_identical(certificates$ET$mean_tilt, 0)
        expect_identical(certificates$SH$mean_tilt, 0)
        expect_equal(
          unname(vapply(certificates, `[[`, numeric(1L), "lower_root")),
          rep(certificates$RQR$lower_root, 3L),
          tolerance = 1e-7
        )
        expect_equal(
          unname(vapply(certificates, `[[`, numeric(1L), "upper_root")),
          rep(certificates$RQR$upper_root, 3L),
          tolerance = 1e-7
        )
      }
    }
  }
})

test_that("AL analytic partial moments agree with independent quadrature", {
  for (tau in c(0.20, 0.50, 0.80)) {
    for (standardized in c(FALSE, TRUE)) {
      spec <- rqrgibbs:::.rqr_oracle_family_spec(
        "asymmetric_laplace",
        list(
          tau = tau, scale = 1.3,
          variance_standardized = standardized
        )
      )
      for (x in c(-4, -1, 0, 1, 4)) {
        cuts <- sort(unique(c(-Inf, spec$kinks[spec$kinks < x], x)))
        numerical_first <- sum(vapply(
          seq_len(length(cuts) - 1L),
          function(ii) stats::integrate(
            function(z) z * spec$d(z), cuts[[ii]], cuts[[ii + 1L]],
            rel.tol = 1e-11, subdivisions = 2000L
          )$value,
          numeric(1L)
        ))
        numerical_second <- sum(vapply(
          seq_len(length(cuts) - 1L),
          function(ii) stats::integrate(
            function(z) z^2 * spec$d(z), cuts[[ii]], cuts[[ii + 1L]],
            rel.tol = 1e-11, subdivisions = 2000L
          )$value,
          numeric(1L)
        ))
        expect_equal(spec$M(x), numerical_first, tolerance = 1e-8)
        expect_equal(spec$M2(x), numerical_second, tolerance = 1e-8)
      }
      expect_identical(spec$M(-Inf), 0)
      expect_identical(spec$M2(-Inf), 0)
      expect_equal(spec$M(Inf), spec$mean, tolerance = 1e-12)
      expect_equal(
        spec$M2(Inf), spec$second_moment, tolerance = 1e-12
      )
      if (standardized) {
        expect_equal(spec$second_moment, 1, tolerance = 1e-12)
      }
    }
  }
})

test_that("AL target certificates obey reflection identities", {
  for (coverage in c(0.80, 0.90, 0.95)) {
    for (target in c("RQR", "ET", "SH")) {
      left <- rqr_interval_oracle(
        "asymmetric_laplace", coverage, target,
        params = list(
          tau = 0.80, scale = 1, variance_standardized = TRUE
        ),
        grid_size = 201L
      )
      right <- rqr_interval_oracle(
        "asymmetric_laplace", coverage, target,
        params = list(
          tau = 0.20, scale = 1, variance_standardized = TRUE
        ),
        grid_size = 201L
      )
      expect_equal(left$lower_root, -right$upper_root, tolerance = 1e-7)
      expect_equal(left$upper_root, -right$lower_root, tolerance = 1e-7)
      expect_equal(left$width, right$width, tolerance = 1e-7)
      expect_equal(left$mean_tilt, -right$mean_tilt, tolerance = 1e-7)
    }
  }
})

test_that("recovery tilt subtracts a nonzero population mean", {
  coverage <- 0.80
  oracle <- rqr_interval_oracle(
    "gaussian", coverage, "ET",
    params = list(mean = 2.5, sd = 1.4), grid_size = 201L
  )
  expect_equal(oracle$population_mean, 2.5, tolerance = 0)
  expect_equal(
    oracle$conditional_retained_mean - oracle$population_mean,
    0,
    tolerance = 1e-9
  )
  expect_equal(
    oracle$conditional_retained_mean - oracle$population_mean,
    oracle$mean_tilt,
    tolerance = 1e-9
  )
  expect_identical(oracle$mean_tilt, 0)

  asymmetric <- rqr_interval_oracle(
    "gaussian_mixture", coverage, "ET",
    params = list(
      weights = c(0.7, 0.3), means = c(-1, 3),
      sds = c(0.6, 1.1), center = FALSE
    ),
    grid_size = 201L
  )
  expect_equal(asymmetric$population_mean, 0.2, tolerance = 1e-10)
  expect_gt(abs(asymmetric$conditional_retained_mean), 1e-3)
  expect_gt(abs(asymmetric$mean_tilt), 1e-3)
  expect_equal(
    asymmetric$mean_tilt,
    asymmetric$conditional_retained_mean - asymmetric$population_mean,
    tolerance = 1e-10
  )
})

test_that("location-scale target transformation is exact", {
  oracle <- rqr_interval_oracle(
    "asymmetric_laplace", 0.95, "SH",
    params = list(
      tau = 0.80, scale = 1, variance_standardized = TRUE
    ),
    grid_size = 201L
  )
  location <- c(-2, 0.5, 4)
  scale <- c(0.25, 2, 5)
  endpoints <- rqr_interval_oracle_endpoints(location, scale, oracle)
  expect_equal(
    endpoints$lower, location + scale * oracle$lower_root,
    tolerance = 0
  )
  expect_equal(
    endpoints$upper, location + scale * oracle$upper_root,
    tolerance = 0
  )
  expect_equal(endpoints$mean_tilt, scale * oracle$mean_tilt, tolerance = 0)
  expect_equal(
    rqr_oracle_conditional_content(
      endpoints$lower, endpoints$upper, location, scale,
      family = "asymmetric_laplace",
      params = list(
        tau = 0.80, scale = 1, variance_standardized = TRUE
      )
    ),
    rep(0.95, length(location)),
    tolerance = 1e-12
  )
})

test_that("standardized beta oracle supports follow-up validation DGPs", {
  spec <- rqrgibbs:::.rqr_oracle_family_spec(
    "standardized_beta", list(shape1 = 5, shape2 = 2)
  )
  expect_equal(spec$mean, 0, tolerance = 1e-10)
  expect_equal(spec$second_moment, 1, tolerance = 1e-9)
  expect_true(all(is.finite(spec$support)))
  expect_equal(spec$F(spec$support[[1L]]), 0, tolerance = 1e-12)
  expect_equal(spec$F(spec$support[[2L]]), 1, tolerance = 1e-12)

  oracle <- rqr_interval_oracle(
    "standardized_beta", 0.95, "SH",
    params = list(shape1 = 5, shape2 = 2), grid_size = 201L
  )
  expect_s3_class(oracle, "rqr_interval_oracle")
  expect_lt(abs(oracle$content_residual), 1e-8)
  expect_lt(abs(oracle$density_residual), 1e-6)
  expect_true(is.finite(oracle$mean_tilt))
  expect_gt(oracle$width, 0)
})

test_that("exact tilted risk agrees with split numerical integration", {
  params <- list(tau = 0.80, scale = 1, variance_standardized = TRUE)
  oracle <- rqr_interval_oracle(
    "asymmetric_laplace", 0.90, "SH", params,
    grid_size = 201L
  )
  location <- 1.2
  scale <- 0.7
  endpoints <- rqr_interval_oracle_endpoints(location, scale, oracle)
  risk <- rqr_oracle_tilted_risk(
    endpoints$lower, endpoints$upper, 0.90,
    mean_tilt = endpoints$mean_tilt,
    location = location, scale = scale,
    family = "asymmetric_laplace", params = params
  )
  spec <- rqrgibbs:::.rqr_oracle_family_spec(
    "asymmetric_laplace", params
  )
  integrand <- function(z) {
    y <- location + scale * z
    rqr_mean_tilt_loss(
      y,
      rep(endpoints$lower, length(y)),
      rep(endpoints$upper, length(y)),
      0.90,
      mean_tilt = rep(endpoints$mean_tilt, length(y))
    ) * spec$d(z)
  }
  kink <- spec$kinks
  numerical <- stats::integrate(
    integrand, -Inf, kink, rel.tol = 1e-10
  )$value + stats::integrate(
    integrand, kink, Inf, rel.tol = 1e-10
  )$value
  expect_equal(risk$mean_tilted_risk, numerical, tolerance = 1e-8)
  expect_equal(risk$conditional_content, 0.90, tolerance = 1e-12)
})

test_that("current oracle APIs reject legacy and invalid contracts", {
  legacy_path <- testthat::test_path(
    "..", "..", "..", "figures", "data", "oracle_tilt_c095_v3",
    "oracle_targets.csv"
  )
  legacy <- utils::read.csv(legacy_path, stringsAsFactors = FALSE)
  expect_false("conditional_retained_mean" %in% names(legacy))
  expect_false("tilt_definition" %in% names(legacy))
  expect_error(
    rqr_interval_oracle_endpoints(
      0, 1, as.list(legacy[legacy$target == "ET", , drop = FALSE])
    ),
    "current rqr_interval_oracle"
  )
  expect_error(
    rqr_interval_oracle("asymmetric_laplace", 1, "ET"),
    "coverage_level"
  )
  expect_error(
    rqr_interval_oracle(
      "asymmetric_laplace", 0.8, "SH",
      params = list(tau = 0.8, scale = -1)
    ),
    "scale must be positive"
  )
  oracle <- rqr_interval_oracle("gaussian", 0.8, "RQR", grid_size = 201L)
  expect_error(
    rqr_interval_oracle_endpoints(0, 0, oracle),
    "scale must be positive"
  )
})

test_that("interval-oracle certificates are deterministic", {
  args <- list(
    family = "asymmetric_laplace", coverage_level = 0.95,
    target = "SH",
    params = list(tau = 0.80, scale = 1, variance_standardized = TRUE),
    tol = 1e-10, grid_size = 201L
  )
  first <- do.call(rqr_interval_oracle, args)
  second <- do.call(rqr_interval_oracle, args)
  expect_identical(first, second)
})

test_that("interval-oracle targets are stable under stricter solver controls", {
  for (target in c("RQR", "ET", "SH")) {
    base <- rqr_interval_oracle(
      "asymmetric_laplace", 0.95, target,
      params = list(
        tau = 0.80, scale = 1, variance_standardized = TRUE
      ),
      tol = 1e-9, grid_size = 401L
    )
    strict <- rqr_interval_oracle(
      "asymmetric_laplace", 0.95, target,
      params = list(
        tau = 0.80, scale = 1, variance_standardized = TRUE
      ),
      tol = 1e-11, grid_size = 1601L
    )
    expect_equal(base$lower_root, strict$lower_root, tolerance = 2e-8)
    expect_equal(base$upper_root, strict$upper_root, tolerance = 2e-8)
    expect_equal(base$mean_tilt, strict$mean_tilt, tolerance = 2e-8)
  }
})
