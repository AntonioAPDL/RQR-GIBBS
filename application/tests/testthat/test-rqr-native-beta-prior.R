rqr_native_prior_function <- function(name) {
  get(name, envir = asNamespace("rqrgibbs"), inherits = FALSE)
}

test_that("native ridge and Gaussian priors share one canonical contract", {
  make_prior <- rqr_native_prior_function(".rqr_beta_prior_spec")
  validate_prior <- rqr_native_prior_function(".rqr_prior_validate")
  initialize_prior <- rqr_native_prior_function(".rqr_prior_initialize")
  canonical_prior <- rqr_native_prior_function(".rqr_prior_canonical")
  diagnose_prior <- rqr_native_prior_function(".rqr_prior_diagnostics")
  target_contract <- rqr_native_prior_function(".rqr_prior_target_contract")
  has_function <- rqr_native_prior_function(".rqr_prior_has_function")

  X <- cbind("(Intercept)" = 1, x = c(-1, 0, 1), z = c(2, -1, 0))
  ridge <- validate_prior(
    make_prior("ridge", ridge = list(tau2 = 4)), X
  )
  ridge_state <- initialize_prior(ridge)
  ridge_canonical <- canonical_prior(ridge, ridge_state)

  expect_identical(ridge$schema_version, "rqrgibbs_beta_prior/1.0.0")
  expect_identical(ridge$root_prior_contract, "shared_exchangeable")
  expect_true(ridge$root_priors_exchangeable)
  expect_false(has_function(ridge))
  expect_equal(ridge_canonical$precision, diag(0.25, 3))
  expect_equal(ridge_canonical$information, rep(0, 3))
  expect_false(diagnose_prior(ridge, ridge_state)$stateful)
  expect_false(has_function(target_contract(ridge)))

  ridge_as_gaussian <- validate_prior(
    make_prior(
      "gaussian",
      gaussian = list(mean = rep(0, 3), precision = diag(0.25, 3))
    ),
    X
  )
  expect_equal(
    canonical_prior(ridge_as_gaussian)$precision,
    ridge_canonical$precision
  )
  expect_equal(
    canonical_prior(ridge_as_gaussian)$information,
    ridge_canonical$information
  )
})

test_that("full Gaussian priors canonicalize covariance and nonzero means", {
  make_prior <- rqr_native_prior_function(".rqr_beta_prior_spec")
  validate_prior <- rqr_native_prior_function(".rqr_prior_validate")
  canonical_prior <- rqr_native_prior_function(".rqr_prior_canonical")

  Q <- matrix(c(
    2.0, 0.3, -0.1,
    0.3, 1.5, 0.2,
    -0.1, 0.2, 1.2
  ), 3, 3, byrow = TRUE)
  mu <- c(0.5, -1.0, 0.25)
  Sigma <- solve(Q)
  X <- cbind(a = 1, b = c(-1, 0, 1), c = c(1, 1, -1))

  from_precision <- validate_prior(
    make_prior(
      "gaussian", gaussian = list(mean = mu, precision = Q)
    ),
    X
  )
  from_covariance <- validate_prior(
    make_prior(
      "gaussian", gaussian = list(mean = mu, covariance = Sigma)
    ),
    X
  )
  canonical_q <- canonical_prior(from_precision)
  canonical_sigma <- canonical_prior(from_covariance)

  expect_equal(canonical_q$precision, Q, tolerance = 1e-14)
  expect_equal(canonical_sigma$precision, Q, tolerance = 1e-13)
  expect_equal(canonical_q$information, drop(Q %*% mu), tolerance = 1e-14)
  expect_equal(
    solve(canonical_q$precision, canonical_q$information),
    mu,
    tolerance = 1e-14
  )
})

test_that("Gaussian coefficient names bind exactly or explicitly by position", {
  make_prior <- rqr_native_prior_function(".rqr_beta_prior_spec")
  validate_prior <- rqr_native_prior_function(".rqr_prior_validate")
  target_contract <- rqr_native_prior_function(".rqr_prior_target_contract")

  coefficient_names <- c("intercept", "slope", "season")
  X <- cbind(
    intercept = 1,
    slope = c(-1, 0, 1),
    season = c(0.5, -0.5, 0.25)
  )
  Q <- diag(c(2, 3, 4))
  dimnames(Q) <- list(coefficient_names, coefficient_names)
  mu <- setNames(c(0.5, -0.25, 0.1), coefficient_names)

  named <- validate_prior(
    make_prior(
      "gaussian",
      gaussian = list(mean = mu, precision = Q)
    ),
    X
  )
  expect_identical(named$coefficient_binding, "exact_names")
  expect_identical(named$coefficient_names, coefficient_names)
  expect_identical(
    named$design_contract$coefficient_binding, "exact_names"
  )
  expect_identical(
    named$design_contract$coefficient_names, coefficient_names
  )
  expect_identical(
    target_contract(named)$coefficient_names, coefficient_names
  )

  Sigma <- diag(c(4, 5, 6))
  dimnames(Sigma) <- list(coefficient_names, coefficient_names)
  default_mean <- validate_prior(
    make_prior(
      "gaussian", gaussian = list(covariance = Sigma)
    ),
    X
  )
  expect_identical(
    default_mean$coefficient_binding, "exact_names"
  )
  expect_identical(
    dimnames(default_mean$canonical$precision),
    list(coefficient_names, coefficient_names)
  )

  positional <- validate_prior(
    make_prior(
      "gaussian",
      gaussian = list(
        mean = c(0.5, -0.25, 0.1),
        precision = diag(c(2, 3, 4))
      )
    ),
    X[, c("season", "intercept", "slope"), drop = FALSE]
  )
  expect_identical(positional$coefficient_binding, "position")
  expect_null(positional$coefficient_names)
  expect_identical(
    positional$design_contract$coefficient_binding, "position"
  )
  expect_null(positional$design_contract$coefficient_names)
})

test_that("partial, duplicate, reordered, and inconsistent Gaussian names fail closed", {
  make_prior <- rqr_native_prior_function(".rqr_beta_prior_spec")
  validate_prior <- rqr_native_prior_function(".rqr_prior_validate")

  coefficient_names <- c("intercept", "slope", "season")
  X <- cbind(
    intercept = 1,
    slope = c(-1, 0, 1),
    season = c(0.5, -0.5, 0.25)
  )
  Q <- diag(3)
  dimnames(Q) <- list(coefficient_names, coefficient_names)
  mu <- setNames(c(0, 0, 0), coefficient_names)
  prior <- make_prior(
    "gaussian", gaussian = list(mean = mu, precision = Q)
  )

  expect_error(
    validate_prior(
      prior, X[, c("slope", "intercept", "season"), drop = FALSE]
    ),
    "exactly match colnames\\(X\\)"
  )
  missing_design_name <- X
  colnames(missing_design_name)[2L] <- ""
  expect_error(
    validate_prior(prior, missing_design_name),
    "unique nonempty coefficient names"
  )
  duplicate_design_name <- X
  colnames(duplicate_design_name)[2L] <- "intercept"
  expect_error(
    validate_prior(prior, duplicate_design_name),
    "unique nonempty coefficient names"
  )
  expect_error(
    validate_prior(prior, unname(X)),
    "requires complete design-column names"
  )

  row_only <- diag(3)
  dimnames(row_only) <- list(coefficient_names, NULL)
  expect_error(
    make_prior(
      "gaussian",
      gaussian = list(mean = mu, precision = row_only)
    ),
    "complete row and column"
  )
  inconsistent <- Q
  colnames(inconsistent) <- c("slope", "intercept", "season")
  expect_error(
    make_prior(
      "gaussian",
      gaussian = list(mean = mu, precision = inconsistent)
    ),
    "row and column coefficient names.*identical"
  )
  duplicate_matrix_names <- Q
  dimnames(duplicate_matrix_names) <- list(
    c("intercept", "intercept", "season"),
    c("intercept", "intercept", "season")
  )
  expect_error(
    make_prior(
      "gaussian",
      gaussian = list(mean = mu, precision = duplicate_matrix_names)
    ),
    "unique nonempty coefficient names"
  )
  expect_error(
    make_prior(
      "gaussian",
      gaussian = list(mean = unname(mu), precision = Q)
    ),
    "mean must have complete coefficient names"
  )
  expect_error(
    make_prior(
      "gaussian",
      gaussian = list(mean = mu, precision = diag(3))
    ),
    "complete row and column"
  )
  reordered_mean <- mu[c("slope", "intercept", "season")]
  expect_error(
    make_prior(
      "gaussian",
      gaussian = list(mean = reordered_mean, precision = Q)
    ),
    "identical coefficient names in the same order"
  )
  incomplete_mean <- mu
  names(incomplete_mean)[2L] <- ""
  expect_error(
    make_prior(
      "gaussian",
      gaussian = list(mean = incomplete_mean, precision = Q)
    ),
    "unique nonempty coefficient names"
  )

  malformed <- prior
  malformed$canonical$precision <- unname(
    malformed$canonical$precision
  )
  expect_error(
    validate_prior(malformed, X),
    "canonical Gaussian precision must retain"
  )
})

test_that("Gaussian and design boundaries fail closed", {
  make_prior <- rqr_native_prior_function(".rqr_beta_prior_spec")
  validate_prior <- rqr_native_prior_function(".rqr_prior_validate")

  expect_error(
    make_prior(
      "gaussian",
      gaussian = list(precision = diag(2), covariance = diag(2))
    ),
    "exactly one"
  )
  expect_error(
    make_prior(
      "gaussian",
      gaussian = list(precision = matrix(c(1, 0.1, 0.2, 1), 2, 2))
    ),
    "symmetric"
  )
  expect_error(
    make_prior(
      "gaussian",
      gaussian = list(precision = matrix(c(1, 2, 2, 1), 2, 2))
    ),
    "positive definite"
  )
  prior <- make_prior("gaussian", gaussian = list(precision = diag(2)))
  expect_error(
    validate_prior(prior, matrix(1, 3, 3)),
    "dimension"
  )
  expect_error(
    validate_prior(prior, matrix(c(1, Inf, 1, 2), 2, 2)),
    "finite numeric"
  )
  expect_error(
    make_prior("ridge", ridge = list(tau = 2)),
    "unsupported fields"
  )
  expect_error(
    make_prior(
      c("ridge", "gaussian"),
      ridge = list(tau2 = 2)
    ),
    "exactly one prior name"
  )
  expect_error(
    make_prior(
      "ridge", ridge = list(tau2 = 2),
      gaussian = list(precision = matrix(1, 1, 1))
    ),
    "inactive prior types"
  )
  expect_error(
    make_prior(
      "gaussian",
      gaussian = list(precision = 1)
    ),
    "numeric square matrix"
  )
  expect_error(
    make_prior(
      "rhs_ns",
      rhs_ns = list(
        intercept_name = "intercept",
        intercept = "intercept"
      )
    ),
    "cannot supply both"
  )
})

test_that("legacy coercion extracts data and never retains closures", {
  coerce_prior <- rqr_native_prior_function(".rqr_beta_prior_coerce")
  has_function <- rqr_native_prior_function(".rqr_prior_has_function")

  legacy_ridge <- list(
    type = "ridge",
    hypers = list(tau2 = 7),
    init = function(...) stop("must not execute"),
    update = function(...) stop("must not retain")
  )
  ridge <- coerce_prior(legacy_ridge)
  expect_identical(ridge$type, "ridge")
  expect_equal(ridge$hypers$tau2, 7)
  expect_false(has_function(ridge))
  expect_silent(serialize(ridge, NULL))

  X <- cbind(bias = 1, x = c(-1, 0, 1))
  legacy_rhs <- list(
    type = "rhs_ns",
    hypers = list(
      tau0 = 0.4, a_zeta = 3, b_zeta = 2,
      zeta2_fixed = 5, shrink_intercept = FALSE,
      intercept_prec = 0.02
    ),
    init = function(...) stop("must not execute")
  )
  rhs <- coerce_prior(legacy_rhs, X = X, intercept_name = "bias")
  expect_identical(rhs$type, "rhs_ns")
  expect_equal(rhs$hypers$tau0, 0.4)
  expect_equal(rhs$hypers$zeta2_fixed, 5)
  expect_identical(rhs$design_contract$intercept_index, 1L)
  expect_false(has_function(rhs))

  legacy_rhs$hypers$shrink_intercept <- TRUE
  expect_error(
    coerce_prior(legacy_rhs, X = X, intercept_name = "bias"),
    "shrink_intercept=FALSE"
  )
  expect_error(
    coerce_prior(list(type = "rhs", hypers = list())),
    "only ridge, gaussian, and rhs_ns"
  )
})

test_that("stateless native prior updates preserve their state", {
  make_prior <- rqr_native_prior_function(".rqr_beta_prior_spec")
  validate_prior <- rqr_native_prior_function(".rqr_prior_validate")
  initialize_prior <- rqr_native_prior_function(".rqr_prior_initialize")
  update_prior <- rqr_native_prior_function(".rqr_prior_update")

  X <- cbind(a = 1, b = c(-1, 0, 1))
  prior <- validate_prior(
    make_prior(
      "gaussian",
      gaussian = list(mean = c(1, -1), precision = diag(c(2, 3)))
    ),
    X
  )
  state <- initialize_prior(prior)
  update <- update_prior(prior, state, beta = c(0.2, -0.4))
  expect_identical(update$state, state)
  expect_identical(update$numerical_repair_count, 0L)
})
