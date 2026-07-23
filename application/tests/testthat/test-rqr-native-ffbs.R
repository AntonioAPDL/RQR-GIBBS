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

test_that("fixed-W FFBS moments match an independent dense Gaussian posterior", {
  set.seed(1201)
  p <- 2L
  T <- 4L
  GG <- array(0, c(p, p, T))
  for (tt in seq_len(T)) GG[, , tt] <- matrix(c(1, 0.2, 0, 0.85), 2, 2, byrow = TRUE)
  W <- array(rep(diag(c(0.08, 0.03)), T), c(p, p, T))
  m0 <- c(0.2, -0.1)
  C0 <- matrix(c(1, -0.2, -0.2, 0.7), 2, 2)
  H <- matrix(rnorm(p * T), p, T)
  V <- runif(T, 0.5, 1.1)
  z <- rnorm(T)

  prior_mean <- matrix(0, p, T)
  prior_cov <- matrix(0, p * T, p * T)
  previous_mean <- m0
  previous_cov <- C0
  for (tt in seq_len(T)) {
    rows <- ((tt - 1L) * p + 1L):(tt * p)
    prior_mean[, tt] <- drop(GG[, , tt] %*% previous_mean)
    prior_cov[rows, rows] <- GG[, , tt] %*% previous_cov %*% t(GG[, , tt]) + W[, , tt]
    if (tt > 1L) {
      prior_rows <- seq_len((tt - 1L) * p)
      previous_block <- prior_cov[prior_rows, ((tt - 2L) * p + 1L):((tt - 1L) * p), drop = FALSE]
      prior_cov[prior_rows, rows] <- previous_block %*% t(GG[, , tt])
      prior_cov[rows, prior_rows] <- t(prior_cov[prior_rows, rows])
    }
    previous_mean <- prior_mean[, tt]
    previous_cov <- prior_cov[rows, rows]
  }
  observation <- matrix(0, T, p * T)
  for (tt in seq_len(T)) {
    observation[tt, ((tt - 1L) * p + 1L):(tt * p)] <- H[, tt]
  }
  prior_precision <- solve(prior_cov)
  post_cov <- solve(prior_precision + crossprod(observation / sqrt(V)))
  post_mean <- drop(post_cov %*% (
    prior_precision %*% as.vector(prior_mean) + crossprod(observation, z / V)
  ))

  fit_r <- rqr_ffbs_smooth(
    z, H, V, GG, m0, C0, list(mode = "fixed_W", W = W),
    backend = "R", numerical_policy = "fail"
  )
  fit_cpp <- rqr_ffbs_smooth(
    z, H, V, GG, m0, C0, list(mode = "fixed_W", W = W),
    backend = "cpp", numerical_policy = "fail"
  )
  expect_equal(as.vector(fit_r$smooth_mean), post_mean, tolerance = 1e-10)
  for (tt in seq_len(T)) {
    rows <- ((tt - 1L) * p + 1L):(tt * p)
    expect_equal(fit_r$smooth_cov[, , tt], post_cov[rows, rows], tolerance = 1e-10)
  }
  expect_equal(fit_cpp$smooth_mean, fit_r$smooth_mean, tolerance = 1e-12)
  expect_equal(fit_cpp$smooth_cov, fit_r$smooth_cov, tolerance = 1e-12)
  expect_equal(fit_r$diagnostics$repair_count, 0L)
  expect_equal(fit_cpp$diagnostics$repair_count, 0L)

  set.seed(1202)
  n_path <- 5000L
  sampled_paths <- replicate(
    n_path,
    as.vector(rqr_ffbs_sample(
      z, H, V, GG, m0, C0, list(mode = "fixed_W", W = W),
      backend = "cpp", numerical_policy = "fail"
    )$path)
  )
  sampled_mean <- rowMeans(sampled_paths)
  mean_mcse <- sqrt(diag(post_cov) / n_path)
  expect_lte(max(abs(sampled_mean - post_mean) / mean_mcse), 5)

  sampled_cov <- stats::cov(t(sampled_paths))
  cross_time_pairs <- rbind(c(1L, 7L), c(2L, 8L), c(1L, 4L))
  covariance_z <- apply(cross_time_pairs, 1L, function(pair) {
    ii <- pair[1L]
    jj <- pair[2L]
    covariance_mcse <- sqrt(
      (post_cov[ii, ii] * post_cov[jj, jj] + post_cov[ii, jj]^2) /
        (n_path - 1L)
    )
    abs(sampled_cov[ii, jj] - post_cov[ii, jj]) / covariance_mcse
  })
  expect_lte(max(covariance_z), 5)
})

test_that("backward covariance repair is consistent and fully recorded", {
  z <- c(0.2, -0.1)
  H <- matrix(c(1, 0, 1, 0), 2, 2)
  GG <- array(rep(diag(c(1, 0)), 2), c(2, 2, 2))
  evolution <- list(mode = "fixed_W", W = matrix(0, 2, 2))
  out_r <- rqr_ffbs_smooth(
    z, H, c(1, 1), GG, c(0, 0), diag(2), evolution,
    backend = "R", numerical_policy = "record_repair"
  )
  out_cpp <- rqr_ffbs_smooth(
    z, H, c(1, 1), GG, c(0, 0), diag(2), evolution,
    backend = "cpp", numerical_policy = "record_repair"
  )
  record <- subset(
    out_r$diagnostics$repair_records,
    stage == "backward_smoothing_prior_covariance" & time == 2L
  )
  expect_equal(nrow(record), 1L)
  Rstar <- out_r$prior_cov[, , 2L] + diag(record$jitter, 2)
  B <- out_r$filter_cov[, , 1L] %*% t(GG[, , 2L]) %*% solve(Rstar)
  expected <- out_r$filter_cov[, , 1L] +
    B %*% (out_r$smooth_cov[, , 2L] - Rstar) %*% t(B)
  expect_equal(out_r$smooth_cov[, , 1L], expected, tolerance = 1e-12)
  expect_equal(out_cpp$smooth_cov, out_r$smooth_cov, tolerance = 1e-12)
  expect_equal(out_cpp$diagnostics$repair_records, out_r$diagnostics$repair_records)
  expect_error(
    rqr_ffbs_smooth(
      z, H, c(1, 1), GG, c(0, 0), diag(2), evolution,
      backend = "cpp", numerical_policy = "fail"
    ),
    "Cholesky"
  )
})

test_that("only NA denotes a missing pseudo-observation", {
  model <- rqr_polytrend(1L, C0 = 1)
  args <- list(
    H = model$FF, V = c(1, 1), GG = model$GG, m0 = model$m0,
    C0 = model$C0, evolution = list(mode = "fixed_W", W = 0.1)
  )
  for (backend in c("R", "cpp")) {
    expect_error(
      do.call(rqr_ffbs_smooth, c(list(z = c(0, Inf), backend = backend), args)),
      "Inf"
    )
    expect_error(
      do.call(rqr_ffbs_smooth, c(list(z = c(0, NaN), backend = backend), args)),
      "NaN"
    )
  }
})

test_that("fail policy rejects eigenvalue projection in R and C++", {
  near_indefinite <- diag(c(1, -1e-12))
  exact_psd <- diag(c(1, 0))

  expect_error(
    rqrgibbs:::.rqr_sample_mvnorm_covariance(
      c(0, 0), near_indefinite, jitter_ladder = 0,
      numerical_policy = "fail"
    ),
    "projection is disabled"
  )
  expect_error(
    rqrgibbs:::rqr_mvn_draw_cpp(
      c(0, 0), near_indefinite, jitter_ladder = 0,
      allow_repair = FALSE
    ),
    "projection is disabled"
  )

  repaired_r <- rqrgibbs:::.rqr_sample_mvnorm_covariance(
    c(0, 0), near_indefinite, jitter_ladder = 0,
    numerical_policy = "record_repair"
  )
  repaired_cpp <- rqrgibbs:::rqr_mvn_draw_cpp(
    c(0, 0), near_indefinite, jitter_ladder = 0,
    allow_repair = TRUE
  )
  expect_identical(repaired_r$info$strategy, "psd_eigen")
  expect_identical(repaired_r$info$clamped_eigenvalues, 1L)
  expect_identical(repaired_cpp$info$strategy, "psd_eigen")
  expect_identical(repaired_cpp$info$clamped_eigenvalues, 1L)

  psd_r <- rqrgibbs:::.rqr_sample_mvnorm_covariance(
    c(0, 0), exact_psd, jitter_ladder = 0, numerical_policy = "fail"
  )
  psd_cpp <- rqrgibbs:::rqr_mvn_draw_cpp(
    c(0, 0), exact_psd, jitter_ladder = 0, allow_repair = FALSE
  )
  expect_identical(psd_r$info$clamped_eigenvalues, 0L)
  expect_identical(psd_cpp$info$clamped_eigenvalues, 0L)
})
