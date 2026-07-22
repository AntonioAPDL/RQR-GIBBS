test_that("C++ FFBS moments match the pure-R reference", {
  set.seed(991)
  T <- 18L
  x <- matrix(rnorm(T), T, 1L)
  model <- rqr_polytrend(2L, C0 = diag(c(2, 0.5))) +
    rqr_regression(x, C0 = 1)
  z <- rnorm(T)
  V <- runif(T, 0.4, 1.4)
  D <- rqr_discount_matrix(c(0.96, 0.88), c(2, 1))
  evolution <- list(mode = "adaptive_discount", D = D)

  ref <- rqr_ffbs_smooth(z, model$FF, V, model$GG, model$m0, model$C0,
                         evolution, backend = "R")
  got <- rqr_ffbs_smooth(z, model$FF, V, model$GG, model$m0, model$C0,
                         evolution, backend = "cpp")
  expect_equal(got$filter_mean, ref$filter_mean, tolerance = 1e-12)
  expect_equal(got$filter_cov, ref$filter_cov, tolerance = 1e-12)
  expect_equal(got$smooth_mean, ref$smooth_mean, tolerance = 1e-12)
  expect_equal(got$smooth_cov, ref$smooth_cov, tolerance = 1e-12)
  expect_equal(got$forecast_variance, ref$forecast_variance, tolerance = 1e-12)
})

test_that("fixed-W FFBS supports missing pseudo-observations", {
  T <- 10L
  model <- rqr_polytrend(1L, C0 = 2)
  z <- seq(-1, 1, length.out = T)
  z[c(3, 8)] <- NA_real_
  evolution <- list(mode = "fixed_W", W = 0.05)
  out <- rqr_ffbs_sample(z, model$FF, rep(0.7, T), model$GG,
                         model$m0, model$C0, evolution, backend = "cpp")
  expect_equal(dim(out$path), c(1L, T))
  expect_true(all(is.finite(out$path)))
  expect_true(is.na(out$residual[3]))
  expect_true(is.na(out$forecast_variance[8]))
})

test_that("zero evolution covariance gives a numerically static sampled path", {
  T <- 9L
  model <- rqr_polytrend(1L, C0 = 2)
  out <- rqr_ffbs_sample(
    z = rnorm(T), H = model$FF, V = rep(0.6, T), GG = model$GG,
    m0 = model$m0, C0 = model$C0,
    evolution = list(mode = "fixed_W", W = 0),
    backend = "cpp"
  )
  expect_equal(as.numeric(out$path), rep(out$path[1, 1], T), tolerance = 1e-7)
  expect_gt(out$diagnostics$psd_draw_count, 0)
  expect_equal(out$diagnostics$jitter_count, 0)
})
