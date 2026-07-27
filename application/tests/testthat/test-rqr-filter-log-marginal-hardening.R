filter_hardening_fixture <- function() {
  p <- 2L
  n_time <- 5L
  GG <- array(0, c(p, p, n_time))
  W <- array(0, c(p, p, n_time))
  for (tt in seq_len(n_time)) {
    GG[, , tt] <- matrix(
      c(1, 0.01 * tt, -0.015, 0.96),
      p, p, byrow = TRUE
    )
    W[, , tt] <- diag(c(1e-10 * (1 + tt), 100 + 10 * tt))
  }
  list(
    z = c(0.2, NA_real_, -0.1, 0.3, NA_real_),
    H = rbind(
      c(1e3, 7e2, -8e2, 5e2, 9e2),
      c(1e-2, -2e-2, 1.5e-2, 8e-3, -1e-2)
    ),
    V = c(0.4, 0.7, 0.55, 0.9, 0.6),
    GG = GG,
    m0 = c(0.05, -0.2),
    C0 = diag(c(1e-8, 2e4)),
    W = W,
    evolution = rqr_evolution_fixed(W)
  )
}

dense_filter_log_marginal <- function(fixture) {
  p <- length(fixture$m0)
  n_time <- length(fixture$z)
  joint_state_covariance <- matrix(0, p * n_time, p * n_time)
  state_mean <- matrix(0, p, n_time)
  previous_mean <- fixture$m0
  previous_covariance <- fixture$C0
  block <- function(tt) {
    ((tt - 1L) * p + 1L):(tt * p)
  }
  for (tt in seq_len(n_time)) {
    gt <- fixture$GG[, , tt]
    state_mean[, tt] <- drop(gt %*% previous_mean)
    if (tt > 1L) {
      for (ss in seq_len(tt - 1L)) {
        cross_covariance <-
          gt %*% joint_state_covariance[
            block(tt - 1L), block(ss), drop = FALSE
          ]
        joint_state_covariance[block(tt), block(ss)] <- cross_covariance
        joint_state_covariance[block(ss), block(tt)] <-
          t(cross_covariance)
      }
    }
    current_covariance <-
      gt %*% previous_covariance %*% t(gt) + fixture$W[, , tt]
    joint_state_covariance[block(tt), block(tt)] <- current_covariance
    previous_mean <- state_mean[, tt]
    previous_covariance <- current_covariance
  }

  observation_mean <- vapply(
    seq_len(n_time),
    function(tt) {
      drop(crossprod(fixture$H[, tt], state_mean[, tt]))
    },
    numeric(1)
  )
  observation_covariance <- matrix(0, n_time, n_time)
  for (tt in seq_len(n_time)) {
    for (ss in seq_len(n_time)) {
      observation_covariance[tt, ss] <- drop(
        crossprod(
          fixture$H[, tt],
          joint_state_covariance[block(tt), block(ss), drop = FALSE] %*%
            fixture$H[, ss]
        )
      )
    }
  }
  diag(observation_covariance) <-
    diag(observation_covariance) + fixture$V
  observed <- which(!is.na(fixture$z))
  residual <- fixture$z[observed] - observation_mean[observed]
  covariance <- observation_covariance[
    observed, observed, drop = FALSE
  ]
  factor <- chol(covariance)
  -0.5 * (
    length(observed) * log(2 * pi) +
      2 * sum(log(diag(factor))) +
      sum(forwardsolve(t(factor), residual)^2)
  )
}

test_that("filter-log-marginal validates H exactly after expansion", {
  fixture <- filter_hardening_fixture()
  common <- fixture[c("z", "V", "GG", "m0", "C0", "evolution")]
  evaluate <- function(H, backend) {
    do.call(
      rqrgibbs:::.rqr_filter_log_marginal,
      c(common, list(H = H, backend = backend))
    )
  }

  for (backend in c("R", "cpp", "auto")) {
    expect_error(
      evaluate(matrix(1, 3L, length(fixture$z)), backend),
      "finite p x length\\(z\\)"
    )
    expect_error(
      evaluate(matrix(1, 2L, 2L), backend),
      "one column or n_time columns"
    )
    malformed <- fixture$H
    malformed[1L, 2L] <- Inf
    expect_error(evaluate(malformed, backend), "finite p x length\\(z\\)")
  }

  expect_equal(
    rqrgibbs:::.rqr_filter_log_marginal(
      fixture$z, matrix(c(1, 0.01), 2L, 1L), fixture$V,
      fixture$GG, fixture$m0, fixture$C0, fixture$evolution,
      backend = "R"
    ),
    rqrgibbs:::.rqr_filter_log_marginal(
      fixture$z,
      matrix(rep(c(1, 0.01), length(fixture$z)), 2L),
      fixture$V, fixture$GG, fixture$m0, fixture$C0,
      fixture$evolution, backend = "R"
    ),
    tolerance = 0
  )
})

test_that("direct C++ filter rejects asymmetric and indefinite covariances", {
  fixture <- filter_hardening_fixture()
  call_cpp <- function(C0 = fixture$C0, W = fixture$W,
                       H = fixture$H) {
    rqrgibbs:::rqr_filter_log_marginal_cpp(
      fixture$z, H, fixture$V, fixture$GG,
      fixture$m0, C0, W
    )
  }

  asymmetric_C0 <- fixture$C0
  asymmetric_C0[1L, 2L] <- 0.2
  expect_error(call_cpp(C0 = asymmetric_C0), "C0 is not symmetric")
  expect_error(
    call_cpp(C0 = diag(c(1, -0.1))),
    "C0 is materially indefinite"
  )

  asymmetric_W <- fixture$W
  asymmetric_W[1L, 2L, 3L] <- 0.2
  expect_error(call_cpp(W = asymmetric_W), "W slice 3 is not symmetric")
  indefinite_W <- fixture$W
  indefinite_W[, , 4L] <- diag(c(1, -0.1))
  expect_error(
    call_cpp(W = indefinite_W),
    "W slice 4 is materially indefinite"
  )

  expect_error(
    call_cpp(H = matrix(1, 3L, length(fixture$z))),
    "Incompatible filter-log-marginal dimensions"
  )
  expect_true(is.finite(call_cpp(C0 = diag(c(0, 2e4)))))
})

test_that("Joseph filter matches dense Gaussian marginal and R/C++ parity", {
  fixture <- filter_hardening_fixture()
  expected <- dense_filter_log_marginal(fixture)
  value_r <- rqrgibbs:::.rqr_filter_log_marginal(
    fixture$z, fixture$H, fixture$V, fixture$GG,
    fixture$m0, fixture$C0, fixture$evolution,
    backend = "R"
  )
  value_cpp <- rqrgibbs:::.rqr_filter_log_marginal(
    fixture$z, fixture$H, fixture$V, fixture$GG,
    fixture$m0, fixture$C0, fixture$evolution,
    backend = "cpp"
  )
  value_auto <- rqrgibbs:::.rqr_filter_log_marginal(
    fixture$z, fixture$H, fixture$V, fixture$GG,
    fixture$m0, fixture$C0, fixture$evolution,
    backend = "auto"
  )

  expect_equal(value_r, expected, tolerance = 5e-10)
  expect_equal(value_cpp, expected, tolerance = 5e-10)
  expect_equal(value_cpp, value_r, tolerance = 5e-12)
  expect_identical(value_auto, value_cpp)
  expect_identical(
    rqrgibbs:::.rqr_resolve_filter_backend("auto"),
    "cpp"
  )
})

test_that("filter log marginal handles missingness without consuming RNG", {
  fixture <- filter_hardening_fixture()
  set.seed(20260727)
  seed_before <- .Random.seed
  for (backend in c("R", "cpp", "auto")) {
    expect_true(is.finite(rqrgibbs:::.rqr_filter_log_marginal(
      fixture$z, fixture$H, fixture$V, fixture$GG,
      fixture$m0, fixture$C0, fixture$evolution,
      backend = backend
    )))
    expect_identical(.Random.seed, seed_before)
  }

  expect_identical(
    rqrgibbs:::.rqr_filter_log_marginal(
      rep(NA_real_, length(fixture$z)),
      fixture$H, fixture$V, fixture$GG,
      fixture$m0, fixture$C0, fixture$evolution,
      backend = "R"
    ),
    0
  )
  expect_identical(
    rqrgibbs:::.rqr_filter_log_marginal(
      rep(NA_real_, length(fixture$z)),
      fixture$H, fixture$V, fixture$GG,
      fixture$m0, fixture$C0, fixture$evolution,
      backend = "cpp"
    ),
    0
  )
  expect_identical(.Random.seed, seed_before)
})
