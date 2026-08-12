test_that("ECM objective equals mean-tilted loss plus ridge penalties", {
  y <- c(-1.2, -0.3, 0.7, 1.6)
  X <- cbind("(Intercept)" = 1, x = c(-1, 0, 0.5, 1))
  beta1 <- c(-0.7, 0.2)
  beta2 <- c(0.9, -0.1)
  q <- 0.75
  omega <- 1.4
  constants <- rqr_constants(q, omega)
  delta <- c(0.2, -0.1, 0.0, 0.3)
  prior_prec <- c(0.05, 0.08)
  got <- rqrgibbs:::.rqr_ecm_objective(
    y, X, beta1, beta2, constants, delta, prior_prec, prior_prec
  )
  eta1 <- drop(X %*% beta1)
  eta2 <- drop(X %*% beta2)
  loss <- rqr_mean_tilt_loss(y, eta1, eta2, q, mean_tilt = delta, details = TRUE)
  expected <- omega * sum(loss$total) +
    0.5 * sum(prior_prec * beta1^2) +
    0.5 * sum(prior_prec * beta2^2)

  expect_equal(got$total, expected)
  expect_equal(got$product_loss, omega * sum(loss$product_loss))
  expect_equal(got$mean_tilt, -omega * sum(loss$linear_tilt))
})

test_that("ECM objective is invariant to complete-root swaps", {
  y <- rnorm(20)
  X <- cbind("(Intercept)" = 1, x = seq(-1, 1, length.out = 20))
  constants <- rqr_constants(0.8, 1.2)
  beta1 <- c(-0.4, 0.2)
  beta2 <- c(0.5, -0.1)
  prior_prec <- c(0.1, 0.1)
  delta <- rep(0.15, length(y))

  obj12 <- rqrgibbs:::.rqr_ecm_objective(
    y, X, beta1, beta2, constants, delta, prior_prec, prior_prec
  )
  obj21 <- rqrgibbs:::.rqr_ecm_objective(
    y, X, beta2, beta1, constants, delta, prior_prec, prior_prec
  )

  expect_equal(obj12$total, obj21$total)
  expect_equal(obj12$residual_products, obj21$residual_products)
})
