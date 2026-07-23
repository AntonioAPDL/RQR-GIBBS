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
  expect_equal(a$construction_audit$repair_count, 0L)
  expect_identical(
    names(a$construction_audit$repair_records),
    c(
      "stage", "time", "strategy", "jitter", "relative_jitter",
      "min_eigenvalue", "matrix_scale", "clamped_eigenvalues"
    )
  )
  for (tt in seq_len(dim(a$W)[3L])) {
    Wt <- matrix(a$W[, , tt], nrow = dim(a$W)[1L], ncol = dim(a$W)[2L])
    expect_equal(Wt, t(Wt), tolerance = 1e-14)
    expect_gte(min(eigen(Wt, symmetric = TRUE, only.values = TRUE)$values), -1e-12)
  }
})

test_that("discount-template repairs use the complete FFBS ledger schema", {
  model <- rqr_as_dlm_model(list(
    FF = matrix(c(1, 0), 2, 1),
    GG = diag(c(1, 0)),
    m0 = c(0, 0),
    C0 = diag(2),
    component_dims = c(1, 1),
    component_names = c("level", "degenerate")
  ))
  template <- rqr_freeze_discount_template(
    model, n_time = 2, df = c(0.9, 1), dim.df = c(1, 1),
    reference_variance = 1, numerical_policy = "record_repair"
  )
  records <- template$construction_audit$repair_records
  expect_gt(nrow(records), 0L)
  expect_equal(template$construction_audit$repair_count, nrow(records))
  expect_true(all(records$stage == "discount_template_filter_covariance"))
  expect_true(all(is.finite(records$jitter) & records$jitter > 0))
  expect_true(all(is.finite(records$relative_jitter)))
  expect_true(all(is.finite(records$min_eigenvalue)))
  expect_true(all(is.finite(records$matrix_scale)))
  expect_true(all(records$clamped_eigenvalues == 0L))
  expect_error(
    rqr_freeze_discount_template(
      model, n_time = 2, df = c(0.9, 1), dim.df = c(1, 1),
      reference_variance = 1, numerical_policy = "fail"
    ),
    "Cholesky"
  )
})

test_that("fixed evolution covariances are symmetric positive semidefinite", {
  fixed <- rqr_evolution_fixed(diag(2))
  expect_s3_class(fixed, "rqr_evolution")
  expect_true(fixed$exact_joint_target)
  working <- rqr_evolution_adaptive_working(c(0.9, 0.8), c(1, 1))
  expect_false(working$exact_joint_target)
  expect_true(working$working_sequential)
  expect_error(
    rqr_ffbs_smooth(
      z = 0, H = matrix(c(1, 0), 2, 1), V = 1, GG = diag(2),
      m0 = c(0, 0), C0 = diag(2),
      evolution = list(mode = "fixed_W", W = matrix(c(1, 0, 0, -1e-12), 2, 2)),
      backend = "R"
    ),
    "materially indefinite"
  )
  expect_error(
    rqr_ffbs_smooth(
      z = 0, H = matrix(c(1, 0), 2, 1), V = 1, GG = diag(2),
      m0 = c(0, 0), C0 = diag(2),
      evolution = list(mode = "fixed_W", W = matrix(c(1, 0.1, 0, 1), 2, 2)),
      backend = "R"
    ),
    "not symmetric"
  )
})

test_that("discount template inputs and component metadata fail explicitly", {
  model <- rqr_polytrend(1L, C0 = 2)
  expect_error(
    rqr_freeze_discount_template(
      model, 5L, df = 0.9, dim.df = 1L, reference_variance = c(1, 2)
    ),
    "length 1 or n_time"
  )
  bad <- list(
    FF = matrix(1, 3, 1), GG = diag(3), m0 = rep(0, 3), C0 = diag(3),
    component_dims = c(2, 2), component_names = c("a", "b")
  )
  expect_error(rqr_as_dlm_model(bad), "summing")
  bad$component_dims <- c(1, 2)
  bad$component_names <- "a"
  expect_error(rqr_as_dlm_model(bad), "matching")
  bad$component_dims <- c(1.5, 1.5)
  bad$component_names <- c("a", "b")
  expect_error(rqr_as_dlm_model(bad), "positive integers")
  expect_error(rqr_discount_matrix(0.9, 1.5), "positive integers")
})

test_that("component-scale evolution validates SPD templates and model blocks", {
  evo <- rqr_evolution_component_scale(
    templates = list(diag(2), matrix(0.5, 1, 1)),
    component_dims = c(2, 1),
    prior = list(shape = c(3, 4), rate = c(2, 5)),
    initial = c(1, 0.5),
    component_names = c("trend", "regression")
  )
  expect_s3_class(evo, "rqr_evolution")
  expect_identical(evo$mode, "component_scale")
  expect_true(evo$exact_joint_target)
  materialized <- rqrgibbs:::.rqr_materialize_component_evolution(
    evo, q = c(0.5, 2), n_time = 2, p = 3
  )
  expect_equal(materialized$W[, , 1], diag(c(0.5, 0.5, 1)))
  expect_equal(materialized$W[, , 2], materialized$W[, , 1])
  expect_error(
    rqr_evolution_component_scale(
      templates = list(matrix(c(1, 2, 2, 1), 2, 2)), component_dims = 2
    ),
    "Cholesky"
  )
  expect_error(
    rqr_evolution_component_scale(
      templates = list(matrix(1, 1, 1), matrix(1, 1, 1)), component_dims = c(1, 1),
      prior = list(shape = c(1, 2, 3), rate = 1)
    ),
    "scalar or length J"
  )
  expect_error(
    rqr_evolution_component_scale(
      templates = list(matrix(1, 1, 1), matrix(1, 1, 1)),
      component_dims = c(1, 1), initial = c(1, 2, 3)
    ),
    "scalar or length J"
  )
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
