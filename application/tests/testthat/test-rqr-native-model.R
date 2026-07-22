test_that("native model composition preserves the exdqlm matrix contract", {
  T <- 12L
  trend <- rqr_polytrend(2L, m0 = c(1, 0), C0 = diag(c(2, 3)))
  reg <- rqr_regression(cbind(x1 = seq_len(T), x2 = seq_len(T)^2), C0 = diag(2))
  model <- trend + reg

  expect_s3_class(model, "rqr_dlm_model")
  expect_equal(dim(model$FF), c(4L, T))
  expect_equal(dim(model$GG), c(4L, 4L))
  expect_equal(model$component_dims, c(2L, 2L))
  expect_equal(model$GG[1:2, 1:2], matrix(c(1, 1, 0, 1), 2, 2, byrow = TRUE))
  expect_equal(model$GG[3:4, 3:4], diag(2))
  expect_equal(model$GG[1:2, 3:4], matrix(0, 2, 2))
})

test_that("component discounts match exdqlm 1.1.0 block construction", {
  D <- rqr_discount_matrix(df = c(0.9, 0.8, 1), dim.df = c(2, 1, 2))
  expect_equal(D[1:2, 1:2], matrix((1 - 0.9) / 0.9, 2, 2))
  expect_equal(D[3, 3], (1 - 0.8) / 0.8)
  expect_equal(D[4:5, 4:5], matrix(0, 2, 2))
  expect_equal(D[1:2, 3:5], matrix(0, 2, 3))
  expect_error(rqr_discount_matrix(c(0.9, 1.01), c(1, 1)), "df must be")
})

test_that("discount templates are deterministic and frozen", {
  model <- rqr_polytrend(1L, C0 = 2)
  a <- rqr_freeze_discount_template(model, 8L, df = 0.9, dim.df = 1L,
                                    reference_variance = rep(0.5, 8L))
  b <- rqr_freeze_discount_template(model, 8L, df = 0.9, dim.df = 1L,
                                    reference_variance = rep(0.5, 8L))
  expect_identical(a$W, b$W)
  expect_true(a$exact_joint_target)
  expect_true(a$frozen_before_mcmc)
  expect_true(all(a$W >= 0))
})

test_that("native RQR algebra and ordered endpoints are root-label invariant", {
  set.seed(990)
  y <- rnorm(15)
  eta1 <- rnorm(15)
  eta2 <- rnorm(15)
  expect_equal(
    rqr_pseudo_residual(y, eta1, eta2),
    rqr_residual_product(y, eta1, eta2),
    tolerance = 1e-12
  )
  first <- rqr_order_endpoints(eta1, eta2)
  swapped <- rqr_order_endpoints(eta2, eta1)
  expect_identical(first, swapped)
})
