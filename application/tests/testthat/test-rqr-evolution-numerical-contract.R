test_that("collapsed density rejects only nonrepresentable scale coordinates", {
  evolution <- rqr_evolution_component_scale(
    templates = list(matrix(1, 1L, 1L)),
    component_dims = 1L,
    prior = list(shape = 2, rate = 1),
    initial = 1
  )
  kernel <- list(
    log_scale_power = 1,
    rate_increment = 0.25
  )
  common <- list(
    conditioned_kernel = kernel,
    z = 0,
    H = matrix(1, 1L, 1L),
    obs_variance = 1,
    GG = array(1, c(1L, 1L, 1L)),
    m0 = 0,
    C0 = matrix(1, 1L, 1L),
    evolution = evolution,
    backend = "R"
  )

  expect_identical(
    do.call(
      rqrgibbs:::.rqr_collapsed_component_scale_log_density,
      c(list(log_q = 1000), common)
    ),
    -Inf
  )
  expect_identical(
    do.call(
      rqrgibbs:::.rqr_collapsed_component_scale_log_density,
      c(list(log_q = -1000), common)
    ),
    -Inf
  )

  malformed <- common
  malformed$H <- matrix(c(1, 1), 2L, 1L)
  expect_error(
    do.call(
      rqrgibbs:::.rqr_collapsed_component_scale_log_density,
      c(list(log_q = 0), malformed)
    ),
    "H must be"
  )
})

test_that("time-zero exact singular conditionals need no numerical repair", {
  set.seed(1701)
  out <- rqrgibbs:::.rqr_draw_initial_state(
    theta1 = c(1, 0),
    G1 = diag(c(1, 0)),
    m0 = c(0, 0),
    C0 = diag(2),
    W1 = matrix(0, 2, 2),
    numerical_policy = "fail",
    jitter_ladder = 0
  )

  expect_named(out, c("draw", "diagnostics"))
  expect_equal(out$draw[1L], 1, tolerance = 1e-12)
  expect_true(all(is.finite(out$draw)))
  expect_identical(out$diagnostics$forecast_strategy, "psd_eigen")
  expect_identical(out$diagnostics$repair_count, 0L)
  expect_true(out$diagnostics$mathematically_exact_conditional)
  expect_true(out$diagnostics$numerically_exact)
  expect_true(out$diagnostics$exact_transition)
  expect_identical(
    out$diagnostics$repair_records,
    rqrgibbs:::.rqr_empty_repair_records()
  )
})

test_that("singular-support roundoff is bounded and policy controlled", {
  common <- list(
    theta1 = c(1, 5e-7),
    G1 = diag(c(1, 0)),
    m0 = c(0, 0),
    C0 = diag(2),
    W1 = matrix(0, 2, 2)
  )
  expect_error(
    do.call(
      rqrgibbs:::.rqr_draw_initial_state,
      c(
        common,
        list(numerical_policy = "fail", jitter_ladder = 0)
      )
    ),
    "outside the singular forecast support"
  )
  repaired <- do.call(
    rqrgibbs:::.rqr_draw_initial_state,
    c(
      common,
      list(
        numerical_policy = "record_repair",
        jitter_ladder = c(0, 1e-6)
      )
    )
  )
  expect_true(all(is.finite(repaired$draw)))
  expect_identical(repaired$diagnostics$repair_count, 1L)
  expect_identical(
    repaired$diagnostics$repair_records$stage,
    "time_zero_forecast_support"
  )
  expect_identical(
    repaired$diagnostics$repair_records$strategy,
    "support_projection"
  )

  severe <- common
  severe$theta1[[2L]] <- 1e-3
  expect_error(
    do.call(
      rqrgibbs:::.rqr_draw_initial_state,
      c(
        severe,
        list(
          numerical_policy = "record_repair",
          jitter_ladder = c(0, 1e-6)
        )
      )
    ),
    "outside the singular forecast support"
  )
})

test_that("time-zero Joseph covariance avoids subtractive cancellation", {
  arguments <- list(
    theta1 = c(0, 0),
    G1 = matrix(c(
      -5.61321041185283e25, 3.86061108276608e-58,
      2.98172926644094e-75, -2.57262812805289e-5
    ), 2L, 2L),
    m0 = c(0, 0),
    C0 = matrix(c(
      1.23850824888737e-92, -1.19769292577649e67,
      -1.19769292577649e67, 1.33808396919024e234
    ), 2L, 2L),
    W1 = matrix(c(
      1.78942143881118e225, -4.66366724545811e201,
      -4.66366724545811e201, 1.21546504947421e178
    ), 2L, 2L)
  )

  set.seed(1702)
  exact <- do.call(
    rqrgibbs:::.rqr_draw_initial_state,
    c(
      arguments,
      list(numerical_policy = "fail", jitter_ladder = 0)
    )
  )
  expect_true(all(is.finite(exact$draw)))
  expect_identical(exact$diagnostics$repair_count, 0L)
  expect_true(exact$diagnostics$numerically_exact)
  expect_true(exact$diagnostics$exact_transition)
  set.seed(1702)
  recorded <- do.call(
    rqrgibbs:::.rqr_draw_initial_state,
    c(
      arguments,
      list(
        numerical_policy = "record_repair",
        jitter_ladder = c(0, 1e-12)
      )
    )
  )
  expect_equal(recorded$draw, exact$draw, tolerance = 1e-12)
  expect_identical(recorded$diagnostics$repair_count, 0L)
  expect_true(recorded$diagnostics$numerically_exact)
  expect_true(recorded$diagnostics$exact_transition)
  expect_true(recorded$diagnostics$mathematically_exact_conditional)
  expect_identical(
    recorded$diagnostics$repair_records,
    rqrgibbs:::.rqr_empty_repair_records()
  )
})

test_that("time-zero covariance inputs are scale-relative PSD validated", {
  common <- list(
    theta1 = c(0, 0),
    G1 = diag(2),
    m0 = c(0, 0),
    C0 = diag(2),
    W1 = diag(2),
    numerical_policy = "fail",
    jitter_ladder = 0
  )
  expect_error(
    do.call(
      rqrgibbs:::.rqr_draw_initial_state,
      utils::modifyList(common, list(C0 = diag(c(1, -0.1))))
    ),
    "C0 is materially indefinite"
  )
  expect_error(
    do.call(
      rqrgibbs:::.rqr_draw_initial_state,
      utils::modifyList(common, list(W1 = diag(c(1, -0.1))))
    ),
    "W1 is materially indefinite"
  )
})

test_that("collapsed-update exactness separates target and numerics", {
  evolution <- rqr_evolution_component_scale(
    templates = list(matrix(1, 1L, 1L)),
    component_dims = 1L,
    prior = list(shape = 2, rate = 1),
    initial = 0.3
  )
  set.seed(1703)
  out <- rqrgibbs:::.rqr_collapsed_component_scale_update(
    conditioned_theta = matrix(c(0.1, -0.1), 1L, 2L),
    conditioned_theta0 = 0,
    z = c(0.2, -0.2),
    H = matrix(1, 1L, 2L),
    obs_variance = c(0.5, 0.5),
    GG = array(1, c(1L, 1L, 2L)),
    m0 = 0,
    C0 = matrix(1, 1L, 1L),
    evolution = evolution,
    q = 0.3,
    backend = "R",
    sweeps = 1L
  )

  expect_true(
    out$diagnostics$mathematically_exact_partially_collapsed
  )
  expect_true(
    out$diagnostics$numerically_exact_partially_collapsed
  )
  expect_identical(
    out$diagnostics$numerical_repair_count, 0L
  )
  expect_true(out$diagnostics$exact_partially_collapsed)

  working_evolution <- evolution
  working_evolution$exact_joint_target <- FALSE
  set.seed(1703)
  working <- rqrgibbs:::.rqr_collapsed_component_scale_update(
    conditioned_theta = matrix(c(0.1, -0.1), 1L, 2L),
    conditioned_theta0 = 0,
    z = c(0.2, -0.2),
    H = matrix(1, 1L, 2L),
    obs_variance = c(0.5, 0.5),
    GG = array(1, c(1L, 1L, 2L)),
    m0 = 0,
    C0 = matrix(1, 1L, 1L),
    evolution = working_evolution,
    q = 0.3,
    backend = "R",
    sweeps = 1L
  )
  expect_false(
    working$diagnostics$mathematically_exact_partially_collapsed
  )
  expect_true(
    working$diagnostics$numerically_exact_partially_collapsed
  )
  expect_false(working$diagnostics$exact_partially_collapsed)
})
