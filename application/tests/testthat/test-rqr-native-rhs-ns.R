rqr_native_rhs_function <- function(name) {
  get(name, envir = asNamespace("rqrgibbs"), inherits = FALSE)
}

make_bound_rhs_fixture <- function(fixed_shoulder = FALSE) {
  make_prior <- rqr_native_rhs_function(".rqr_beta_prior_spec")
  validate_prior <- rqr_native_rhs_function(".rqr_prior_validate")
  X <- cbind(
    bias = 1,
    x1 = c(-1, 0, 1, 2),
    x2 = c(2, -1, 0, 1)
  )
  prior <- make_prior(
    "rhs_ns",
    rhs_ns = list(
      intercept_name = "bias",
      intercept_mean = 0.25,
      intercept_precision = 0.04,
      tau0 = 0.7,
      a_zeta = 2.5,
      b_zeta = 1.3,
      zeta2_fixed = if (fixed_shoulder) 3 else NULL
    )
  )
  list(X = X, prior = validate_prior(prior, X))
}

test_that("RHS-NS requires an explicitly named constant-one intercept", {
  make_prior <- rqr_native_rhs_function(".rqr_beta_prior_spec")
  validate_prior <- rqr_native_rhs_function(".rqr_prior_validate")

  expect_error(
    make_prior("rhs_ns", rhs_ns = list()),
    "intercept_name"
  )
  expect_error(
    make_prior(
      "rhs_ns",
      rhs_ns = list(intercept_name = "bias", shrink_intercept = TRUE)
    ),
    "shrink_intercept must be FALSE"
  )
  prior <- make_prior(
    "rhs_ns", rhs_ns = list(intercept_name = "bias")
  )
  expect_error(
    validate_prior(prior, cbind(1, c(-1, 0, 1))),
    "column names"
  )
  expect_error(
    validate_prior(
      prior,
      cbind(bias = c(1, 1, 0.99), x = c(-1, 0, 1))
    ),
    "constant and equal to one"
  )
  duplicate <- matrix(c(1, 1, 1, -1, 0, 1), 3, 2)
  colnames(duplicate) <- c("bias", "bias")
  expect_error(
    validate_prior(prior, duplicate),
    "unique nonempty"
  )
  duplicate_active <- cbind(
    bias = 1,
    x = c(-1, 0, 1),
    x = c(1, 0, -1)
  )
  expect_error(
    validate_prior(prior, duplicate_active),
    "unique nonempty"
  )
})

test_that("RHS-NS initialization and stored state are canonical", {
  initialize_prior <- rqr_native_rhs_function(".rqr_prior_initialize")
  validate_state <- rqr_native_rhs_function(
    ".rqr_rhs_ns_state_validate"
  )
  prior <- make_bound_rhs_fixture()$prior

  expect_error(
    initialize_prior(
      prior,
      init = list(lambda2 = 1, init_lambda2 = 1)
    ),
    "cannot supply both lambda2 and init_lambda2"
  )
  expect_error(
    initialize_prior(prior, init = list(lamda2 = 1)),
    "unsupported fields: lamda2"
  )

  state <- initialize_prior(prior)
  changed_names <- state
  changed_names$active_names <- rev(changed_names$active_names)
  expect_error(
    validate_state(prior, changed_names),
    "active_names"
  )
  changed_flag <- state
  changed_flag$zeta2_is_fixed <- 0
  expect_error(
    validate_state(prior, changed_flag),
    "must be TRUE or FALSE"
  )
  extra <- state
  extra$unbound_field <- TRUE
  expect_error(
    validate_state(prior, extra),
    "canonical form"
  )
})

test_that("RHS-NS canonical precision implements the fictitious-normal shoulder", {
  initialize_prior <- rqr_native_rhs_function(".rqr_prior_initialize")
  canonical_prior <- rqr_native_rhs_function(".rqr_prior_canonical")
  target_contract <- rqr_native_rhs_function(".rqr_rhs_ns_target_contract")
  has_function <- rqr_native_rhs_function(".rqr_prior_has_function")
  fixture <- make_bound_rhs_fixture()
  prior <- fixture$prior
  state <- initialize_prior(
    prior,
    init = list(
      lambda2 = c(0.5, 2),
      nu = c(1.5, 0.8),
      tau2 = 0.25,
      xi = 1.2,
      zeta2 = 4
    )
  )
  canonical <- canonical_prior(prior, state)
  expected <- c(
    0.04,
    1 / (0.25 * 0.5) + 1 / 4,
    1 / (0.25 * 2) + 1 / 4
  )
  expect_equal(diag(canonical$precision), expected, tolerance = 1e-15)
  expect_equal(canonical$information, c(0.04 * 0.25, 0, 0))
  expect_identical(state$active_names, c("x1", "x2"))
  expect_false(has_function(state))
  expect_false(has_function(target_contract(prior)))
  expect_true(target_contract(prior)$exact_gibbs_conditionals)
  expect_null(target_contract(prior)$stochastic_floor)
})

test_that("every RHS-NS inverse-Gamma conditional matches the joint kernel", {
  initialize_prior <- rqr_native_rhs_function(".rqr_prior_initialize")
  log_kernel <- rqr_native_rhs_function(".rqr_rhs_ns_log_kernel")
  log_ig <- rqr_native_rhs_function(".rqr_rhs_ns_log_invgamma")
  lambda_conditional <- rqr_native_rhs_function(
    ".rqr_rhs_ns_conditional_lambda"
  )
  nu_conditional <- rqr_native_rhs_function(
    ".rqr_rhs_ns_conditional_nu"
  )
  tau_conditional <- rqr_native_rhs_function(
    ".rqr_rhs_ns_conditional_tau"
  )
  xi_conditional <- rqr_native_rhs_function(
    ".rqr_rhs_ns_conditional_xi"
  )
  zeta_conditional <- rqr_native_rhs_function(
    ".rqr_rhs_ns_conditional_zeta"
  )
  fixture <- make_bound_rhs_fixture()
  prior <- fixture$prior
  state <- initialize_prior(
    prior,
    init = list(
      lambda2 = c(0.7, 1.4), nu = c(1.2, 0.9),
      tau2 = 0.6, xi = 1.1, zeta2 = 2.2
    )
  )
  beta <- c(0.3, -0.8, 0.45)
  compare_ratio <- function(make_state, values, conditional) {
    states <- lapply(values, make_state)
    joint_difference <-
      log_kernel(prior, states[[2L]], beta) -
      log_kernel(prior, states[[1L]], beta)
    conditional_difference <-
      log_ig(values[2L], conditional$shape, conditional$rate) -
      log_ig(values[1L], conditional$shape, conditional$rate)
    expect_equal(joint_difference, conditional_difference, tolerance = 1e-12)
  }

  conditional <- lambda_conditional(
    beta[state$active_index[1L]], state$tau2, state$nu[1L]
  )
  compare_ratio(function(value) {
    candidate <- state
    candidate$lambda2[1L] <- value
    candidate
  }, c(0.5, 1.6), conditional)

  conditional <- nu_conditional(state$lambda2[1L])
  compare_ratio(function(value) {
    candidate <- state
    candidate$nu[1L] <- value
    candidate
  }, c(0.6, 2.1), conditional)

  conditional <- tau_conditional(
    beta[state$active_index], state$lambda2, state$xi
  )
  compare_ratio(function(value) {
    candidate <- state
    candidate$tau2 <- value
    candidate
  }, c(0.35, 1.3), conditional)

  conditional <- xi_conditional(state$tau2, prior$hypers$tau0)
  compare_ratio(function(value) {
    candidate <- state
    candidate$xi <- value
    candidate
  }, c(0.55, 1.8), conditional)

  conditional <- zeta_conditional(
    beta[state$active_index],
    prior$hypers$a_zeta,
    prior$hypers$b_zeta
  )
  compare_ratio(function(value) {
    candidate <- state
    candidate$zeta2 <- value
    candidate
  }, c(0.75, 3.2), conditional)
})

test_that("RHS-NS coefficient canonical form matches its joint kernel", {
  initialize_prior <- rqr_native_rhs_function(".rqr_prior_initialize")
  canonical_prior <- rqr_native_rhs_function(".rqr_prior_canonical")
  log_kernel <- rqr_native_rhs_function(".rqr_rhs_ns_log_kernel")
  fixture <- make_bound_rhs_fixture()
  prior <- fixture$prior
  state <- initialize_prior(
    prior,
    init = list(
      lambda2 = c(0.8, 1.6), nu = c(1, 1),
      tau2 = 0.7, xi = 1, zeta2 = 2.4
    )
  )
  canonical <- canonical_prior(prior, state)
  baseline <- c(0.1, -0.2, 0.4)

  for (index in seq_along(baseline)) {
    values <- c(-0.6, 0.9)
    candidates <- lapply(values, function(value) {
      beta <- baseline
      beta[index] <- value
      beta
    })
    joint_difference <-
      log_kernel(prior, state, candidates[[2L]]) -
      log_kernel(prior, state, candidates[[1L]])
    mean <- canonical$information[index] / canonical$precision[index, index]
    sd <- sqrt(1 / canonical$precision[index, index])
    normal_difference <-
      stats::dnorm(values[2L], mean, sd, log = TRUE) -
      stats::dnorm(values[1L], mean, sd, log = TRUE)
    expect_equal(joint_difference, normal_difference, tolerance = 1e-12)
  }
})

test_that("native RHS-NS update follows one independently reproduced Gibbs sweep", {
  initialize_prior <- rqr_native_rhs_function(".rqr_prior_initialize")
  update_prior <- rqr_native_rhs_function(".rqr_prior_update")
  fixture <- make_bound_rhs_fixture()
  prior <- fixture$prior
  state <- initialize_prior(
    prior,
    init = list(
      lambda2 = c(0.6, 1.7), nu = c(1.3, 0.75),
      tau2 = 0.55, xi = 1.4, zeta2 = 2.6
    )
  )
  beta <- c(0.2, -0.9, 0.35)

  independent_sweep <- function(state, beta, prior) {
    inverse_gamma <- function(shape, rate) {
      1 / stats::rgamma(1L, shape = shape, rate = rate)
    }
    active_beta <- beta[state$active_index]
    for (j in seq_along(active_beta)) {
      rate_lambda <-
        0.5 * active_beta[j]^2 / state$tau2 + 1 / state$nu[j]
      state$lambda2[j] <- inverse_gamma(1, rate_lambda)
      state$nu[j] <- inverse_gamma(1, 1 + 1 / state$lambda2[j])
    }
    state$tau2 <- inverse_gamma(
      (length(active_beta) + 1) / 2,
      0.5 * sum(active_beta^2 / state$lambda2) + 1 / state$xi
    )
    state$xi <- inverse_gamma(
      1, 1 / prior$hypers$tau0^2 + 1 / state$tau2
    )
    if (!state$zeta2_is_fixed) {
      state$zeta2 <- inverse_gamma(
        prior$hypers$a_zeta + length(active_beta) / 2,
        prior$hypers$b_zeta + 0.5 * sum(active_beta^2)
      )
    }
    state$update_count <- state$update_count + 1L
    state$numerical_repair_count <- 0L
    state
  }

  set.seed(7602601)
  actual <- update_prior(prior, state, beta)$state
  set.seed(7602601)
  expected <- independent_sweep(state, beta, prior)
  for (field in c(
      "lambda2", "nu", "tau2", "xi", "zeta2",
      "update_count", "numerical_repair_count")) {
    expect_identical(actual[[field]], expected[[field]])
  }
})

test_that("fixed shoulders and intercept-only RHS-NS remain exact", {
  initialize_prior <- rqr_native_rhs_function(".rqr_prior_initialize")
  update_prior <- rqr_native_rhs_function(".rqr_prior_update")
  canonical_prior <- rqr_native_rhs_function(".rqr_prior_canonical")
  make_prior <- rqr_native_rhs_function(".rqr_beta_prior_spec")
  validate_prior <- rqr_native_rhs_function(".rqr_prior_validate")

  fixture <- make_bound_rhs_fixture(fixed_shoulder = TRUE)
  state <- initialize_prior(fixture$prior)
  set.seed(7602602)
  update <- update_prior(fixture$prior, state, c(0.1, -0.2, 0.3))
  expect_identical(update$state$zeta2, 3)
  expect_true(update$state$zeta2_is_fixed)

  X <- matrix(1, 4, 1, dimnames = list(NULL, "bias"))
  prior <- validate_prior(
    make_prior(
      "rhs_ns",
      rhs_ns = list(
        intercept_name = "bias",
        intercept_mean = 1,
        intercept_precision = 0.5
      )
    ),
    X
  )
  state <- initialize_prior(prior)
  expect_length(state$lambda2, 0L)
  expect_equal(canonical_prior(prior, state)$precision, matrix(0.5, 1, 1))
  expect_equal(canonical_prior(prior, state)$information, 0.5)
  set.seed(7602603)
  update <- update_prior(prior, state, beta = 0.8)
  expect_identical(update$state$update_count, 1L)
  expect_true(all(is.finite(c(
    update$state$tau2, update$state$xi, update$state$zeta2
  ))))
})

test_that("RHS-NS never silently clips invalid stochastic states", {
  initialize_prior <- rqr_native_rhs_function(".rqr_prior_initialize")
  canonical_prior <- rqr_native_rhs_function(".rqr_prior_canonical")
  update_prior <- rqr_native_rhs_function(".rqr_prior_update")
  fixture <- make_bound_rhs_fixture()
  prior <- fixture$prior

  expect_error(
    initialize_prior(prior, init = list(lambda2 = c(0, 1))),
    "finite and positive"
  )
  state <- initialize_prior(prior)
  state$tau2 <- .Machine$double.xmax
  state$lambda2[] <- .Machine$double.xmax
  expect_error(
    canonical_prior(prior, state),
    "outside the finite positive floating-point range"
  )

  state <- initialize_prior(prior)
  huge <- sqrt(.Machine$double.xmax)
  expect_error(
    update_prior(prior, state, beta = c(0, huge, huge)),
    "conditional rate"
  )

  fractional <- initialize_prior(prior)
  fractional$dimension <- 3.5
  expect_error(
    canonical_prior(prior, fractional),
    "does not match"
  )
})
