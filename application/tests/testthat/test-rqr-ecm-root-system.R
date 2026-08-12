test_that("inverse-moment root system matches the ECM CM equations", {
  y <- c(-1.5, -0.4, 0.2, 1.3, 2.0)
  X <- cbind("(Intercept)" = 1, x = seq(-1, 1, length.out = length(y)))
  beta_other <- c(0.2, -0.1)
  constants <- rqr_constants(0.7, learning_rate = 1.3)
  prior_prec <- c(0.01, 0.03)
  tau <- c(2, 3, 4, 5, 6)
  delta <- seq(-0.2, 0.2, length.out = length(y))

  got <- rqrgibbs:::.rqr_root_gaussian_system(
    y, X, beta_other,
    constants = constants,
    prior_prec = prior_prec,
    mean_tilt_observed = delta,
    latent_mode = "inverse_moment",
    inverse_V_mean = tau
  )
  eta_other <- drop(X %*% beta_other)
  A <- X * as.numeric(y - eta_other)
  c_component <- y^2 - y * eta_other
  expected_precision <- crossprod(A * sqrt(tau / (constants$phi * constants$sigma))) +
    diag(prior_prec, ncol(X))
  expected_rhs <- as.numeric(crossprod(
    A,
    (tau * c_component - constants$xi) /
      (constants$phi * constants$sigma)
  )) +
    constants$omega * constants$alpha * as.numeric(crossprod(X, delta))

  expect_equal(got$precision, expected_precision)
  expect_equal(got$rhs, expected_rhs)
  expect_equal(got$latent_mode, "inverse_moment")
})

test_that("draw-mode root system preserves Gibbs sufficient statistics", {
  y <- c(-2, -0.2, 0.5, 1.4)
  X <- cbind("(Intercept)" = 1, x = c(-1, 0, 0.5, 1))
  beta_other <- c(0.4, 0.3)
  constants <- rqr_constants(0.6, learning_rate = 0.9)
  prior_prec <- c(0.2, 0.4)
  V <- c(0.3, 0.6, 1.2, 2.1)

  got <- rqrgibbs:::.rqr_root_gaussian_system(
    y, X, beta_other,
    constants = constants,
    prior_prec = prior_prec,
    latent_mode = "draw",
    V = V
  )
  eta_other <- drop(X %*% beta_other)
  A <- X * as.numeric(y - eta_other)
  c_component <- y^2 - y * eta_other
  W <- 1 / (constants$phi * constants$sigma * V)
  z <- c_component - constants$xi * V

  expect_equal(got$precision, crossprod(A * sqrt(W)) + diag(prior_prec, ncol(X)))
  expect_equal(got$rhs, as.numeric(crossprod(A, W * z)))
})

test_that("fixed tilt shifts only the root information vector", {
  y <- seq(-1, 1, length.out = 5)
  X <- matrix(1, length(y), 1L)
  beta_other <- 0.25
  constants <- rqr_constants(0.8, learning_rate = 2)
  tau <- rep(3, length(y))
  no_tilt <- rqrgibbs:::.rqr_root_gaussian_system(
    y, X, beta_other, constants, prior_prec = 0.1,
    latent_mode = "inverse_moment", inverse_V_mean = tau,
    mean_tilt_observed = rep(0, length(y))
  )
  delta <- rep(0.4, length(y))
  with_tilt <- rqrgibbs:::.rqr_root_gaussian_system(
    y, X, beta_other, constants, prior_prec = 0.1,
    latent_mode = "inverse_moment", inverse_V_mean = tau,
    mean_tilt_observed = delta
  )

  expect_equal(with_tilt$precision, no_tilt$precision)
  expect_equal(
    with_tilt$rhs - no_tilt$rhs,
    constants$omega * constants$alpha * as.numeric(crossprod(X, delta))
  )
})
