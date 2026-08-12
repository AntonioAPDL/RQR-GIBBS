test_that("ECM latent inverse moment matches analytic GIG identity", {
  e <- c(-3, -0.7, 0.4, 2.2)
  q <- 0.8
  tau <- rqrgibbs:::.rqr_ecm_latent_inverse_mean(
    e, coverage_level = q, residual_product_floor = 0
  )

  expect_equal(tau$inverse_mean, 1 / (q * (1 - q) * abs(e)))
  expect_true(tau$exact_moment_used)
  expect_equal(tau$zero_residual_count, 0)
})

test_that("ECM inverse moment is distinct from VB latent scale", {
  e <- c(-2, 0.5, 1.5)
  q <- 0.75
  omega <- 1.7
  tau <- rqrgibbs:::.rqr_ecm_latent_inverse_mean(
    e, coverage_level = q, residual_product_floor = 0
  )$inverse_mean
  vb_mean <- rqrgibbs:::.rqr_vb_latent_mean(
    e, coverage_level = q, learning_rate = omega
  )
  constants <- rqr_constants(q, omega)
  ecm_weights <- tau / (constants$phi * constants$sigma)
  vb_plugin_weights <- 1 / (constants$phi * constants$sigma * vb_mean)

  expect_false(isTRUE(all.equal(ecm_weights, vb_plugin_weights)))
  expect_equal(
    rqrgibbs:::.rqr_ecm_latent_mean(e, q, omega),
    vb_mean
  )
})

test_that("ECM zero-residual safeguard is explicit", {
  expect_error(
    rqrgibbs:::.rqr_ecm_latent_inverse_mean(
      c(0, 1), coverage_level = 0.8, residual_product_floor = 0
    ),
    "zero residual"
  )
  safeguarded <- rqrgibbs:::.rqr_ecm_latent_inverse_mean(
    c(0, 1),
    coverage_level = 0.8,
    residual_product_floor = 1e-4,
    floor_type = "hard"
  )
  expect_true(all(is.finite(safeguarded$inverse_mean)))
  expect_equal(safeguarded$zero_residual_count, 1)
  expect_false(safeguarded$exact_moment_used)
})
