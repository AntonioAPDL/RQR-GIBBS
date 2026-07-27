test_that("public FFBS Joseph recursion is stable with missing measurements", {
  p <- 2L
  n_time <- 3L
  GG <- array(rep(diag(p), n_time), c(p, p, n_time))
  H <- matrix(rep(c(1, 1), n_time), p, n_time)
  C0 <- diag(c(1e8, 1e-8))
  W <- array(
    rep(diag(c(1e-6, 1e-12)), n_time),
    c(p, p, n_time)
  )
  z <- c(0.2, NA_real_, -0.1)
  V <- c(1e-8, 2e-8, 1e-8)
  arguments <- list(
    z = z, H = H, V = V, GG = GG, m0 = c(0, 0), C0 = C0,
    evolution = list(mode = "fixed_W", W = W),
    numerical_policy = "fail"
  )

  out_r <- do.call(
    rqr_ffbs_smooth, c(arguments, list(backend = "R"))
  )
  out_cpp <- do.call(
    rqr_ffbs_smooth, c(arguments, list(backend = "cpp"))
  )

  expect_equal(out_cpp$filter_mean, out_r$filter_mean, tolerance = 1e-9)
  expect_equal(out_cpp$filter_cov, out_r$filter_cov, tolerance = 1e-9)
  expect_equal(out_cpp$smooth_mean, out_r$smooth_mean, tolerance = 1e-9)
  expect_equal(out_cpp$smooth_cov, out_r$smooth_cov, tolerance = 1e-9)
  expect_true(is.na(out_r$forecast_variance[[2L]]))
  expect_true(is.na(out_cpp$forecast_variance[[2L]]))
  expect_identical(out_r$diagnostics$repair_count, 0L)
  expect_identical(out_cpp$diagnostics$repair_count, 0L)

  for (cube in list(
      out_r$prior_cov, out_r$filter_cov, out_r$smooth_cov,
      out_cpp$prior_cov, out_cpp$filter_cov, out_cpp$smooth_cov
    )) {
    for (tt in seq_len(dim(cube)[[3L]])) {
      values <- eigen(
        cube[, , tt], symmetric = TRUE, only.values = TRUE
      )$values
      scale <- max(abs(values))
      expect_gte(
        min(values),
        -100 * .Machine$double.eps * max(scale, .Machine$double.xmin)
      )
    }
  }
})

test_that("public FFBS preserves exact singular backward support without repair", {
  n_time <- 4L
  arguments <- list(
    z = c(-0.4, 0.1, NA_real_, 0.5),
    H = matrix(1, 1, n_time),
    V = rep(0.7, n_time),
    GG = array(1, c(1, 1, n_time)),
    m0 = 0,
    C0 = matrix(1, 1, 1),
    evolution = list(mode = "fixed_W", W = matrix(0, 1, 1)),
    numerical_policy = "fail"
  )

  set.seed(2201)
  out_r <- do.call(
    rqr_ffbs_sample, c(arguments, list(backend = "R"))
  )
  set.seed(2202)
  out_cpp <- do.call(
    rqr_ffbs_sample, c(arguments, list(backend = "cpp"))
  )

  expect_lte(
    max(abs(as.numeric(out_r$path) - out_r$path[[1L]])),
    1e-7
  )
  expect_lte(
    max(abs(as.numeric(out_cpp$path) - out_cpp$path[[1L]])),
    1e-7
  )
  expect_identical(out_r$diagnostics$repair_count, 0L)
  expect_identical(out_cpp$diagnostics$repair_count, 0L)
  expect_gt(out_r$diagnostics$psd_draw_count, 0L)
  expect_gt(out_cpp$diagnostics$psd_draw_count, 0L)
})

test_that("R and C++ FFBS expose the same declared covariance-repair ledger", {
  arguments <- list(
    z = c(0.2, -0.1),
    H = matrix(c(1, 0, 1, 0), 2, 2),
    V = c(1, 1),
    GG = array(rep(diag(c(1, 0)), 2), c(2, 2, 2)),
    m0 = c(0, 0),
    C0 = diag(2),
    evolution = list(mode = "fixed_W", W = matrix(0, 2, 2)),
    numerical_policy = "record_repair"
  )
  out_r <- do.call(
    rqr_ffbs_smooth, c(arguments, list(backend = "R"))
  )
  out_cpp <- do.call(
    rqr_ffbs_smooth, c(arguments, list(backend = "cpp"))
  )

  expect_equal(out_cpp$smooth_cov, out_r$smooth_cov, tolerance = 1e-12)
  expect_equal(
    out_cpp$diagnostics$repair_records,
    out_r$diagnostics$repair_records,
    tolerance = 1e-15
  )
  expect_identical(
    out_cpp$diagnostics$repair_count,
    out_r$diagnostics$repair_count
  )
  expect_true(any(
    out_r$diagnostics$repair_records$stage ==
      "backward_smoothing_prior_covariance"
  ))
})

test_that("direct native FFBS entry enforces the public covariance contract", {
  p <- 2L
  n_time <- 2L
  base <- list(
    z = c(0.1, -0.2),
    H = matrix(c(1, 0, 1, 0), p, n_time),
    V = c(1, 1),
    GG = array(rep(diag(p), n_time), c(p, p, n_time)),
    m0 = c(0, 0),
    C0 = diag(p),
    evolution_mode = 0L,
    W = array(rep(diag(p) * 0.1, n_time), c(p, p, n_time)),
    D = matrix(0, p, p),
    sample_path = FALSE,
    jitter_ladder = 0,
    evolution_label = "fixed_W",
    allow_covariance_repair = FALSE
  )

  expect_error(
    do.call(
      rqrgibbs:::rqr_ffbs_cpp,
      utils::modifyList(base, list(m0 = c(0, Inf)))
    ),
    "must be finite"
  )
  expect_error(
    do.call(
      rqrgibbs:::rqr_ffbs_cpp,
      utils::modifyList(base, list(C0 = diag(c(1, 0))))
    ),
    "positive definite"
  )
  bad_W <- base$W
  bad_W[, , 1L] <- matrix(c(1, 0.2, 0, 1), p, p)
  expect_error(
    do.call(
      rqrgibbs:::rqr_ffbs_cpp,
      utils::modifyList(base, list(W = bad_W))
    ),
    "not symmetric"
  )
  expect_error(
    do.call(
      rqrgibbs:::rqr_ffbs_cpp,
      utils::modifyList(base, list(jitter_ladder = c(0, 1e-8)))
    ),
    "repair is disabled"
  )
  expect_error(
    do.call(
      rqrgibbs:::rqr_ffbs_cpp,
      utils::modifyList(base, list(evolution_label = "adaptive_discount"))
    ),
    "does not match"
  )
})

test_that("fixed and frozen DLM execution follows its digested root scan", {
  original_ffbs <- getFromNamespace("rqr_ffbs_sample", "rqrgibbs")
  original_time0 <- getFromNamespace(
    ".rqr_draw_initial_state", "rqrgibbs"
  )
  events <- character()
  testthat::local_mocked_bindings(
    rqr_ffbs_sample = function(...) {
      events <<- c(events, "ffbs")
      original_ffbs(...)
    },
    .rqr_draw_initial_state = function(...) {
      events <<- c(events, "time0")
      original_time0(...)
    },
    .package = "rqrgibbs"
  )

  specifications <- list(
    fixed_W = list(evolution_mode = "fixed_W", W = 0.05),
    discount_template = list(
      evolution_mode = "discount_template",
      df = 0.95, dim.df = 1L, reference_variance = 1
    )
  )
  for (mode in names(specifications)) {
    events <- character()
    arguments <- c(
      list(
        y = c(-0.8, -0.2, 0.15, 0.7),
        model = rqr_polytrend(1L, C0 = 2),
        coverage_level = 0.8,
        learning_rate_mode = "fixed_rate",
        learning_rate = 1,
        mcmc_control = list(
          n_burn = 0L, n_mcmc = 1L, seed = 2203L, backend = "R"
        )
      ),
      specifications[[mode]]
    )
    fit <- do.call(rqr_dlm_fit, arguments)

    expect_identical(
      events, c("ffbs", "time0", "ffbs", "time0"),
      info = mode
    )
    expected <- c(
      "lambda_fixed", "latent_v_refresh",
      "root1_ffbs", "root1_time0", "root2_ffbs", "root2_time0",
      "global_root_swap"
    )
    expect_identical(
      fit$model_spec$transition_kernel$scan_order, expected,
      info = mode
    )
    expect_identical(
      fit$diagnostics$partial_collapse_order, expected,
      info = mode
    )
  }
})
