# Native ordinary-RQR Nishimura--Suchard shrunken-shoulder prior.
#
# For each non-intercept coefficient this module uses the exact augmented
# product kernel
#
#   N(beta_j | 0, tau^2 lambda_j^2) N(beta_j | 0, zeta^2)
#
# together with Makalic--Schmidt inverse-Gamma auxiliaries for the local and
# global horseshoe scales.  The second, fictitious Normal factor supplies the
# Gaussian shoulder.  Its normalization is part of the declared augmented
# joint kernel; this is not the conditionally normalized harmonic-variance
# hierarchy sometimes also called a regularized horseshoe.

.rqr_rhs_ns_state_schema <- function() {
  "rqrgibbs_rhs_ns_state/1.0.0"
}

.rqr_rhs_ns_prior_spec <- function(rhs_ns = list()) {
  if (!is.list(rhs_ns)) {
    stop("rhs_ns controls must be a list.", call. = FALSE)
  }
  .rqr_validate_named_list_fields(
    rhs_ns, "rhs_ns",
    c(
      "tau0", "a_zeta", "b_zeta", "zeta2_fixed", "c2_fixed",
      "intercept_name", "intercept", "intercept_mean",
      "intercept_precision", "intercept_prec", "shrink_intercept"
    )
  )
  alias_pairs <- list(
    c("zeta2_fixed", "c2_fixed"),
    c("intercept_name", "intercept"),
    c("intercept_precision", "intercept_prec")
  )
  for (pair in alias_pairs) {
    if (all(pair %in% names(rhs_ns))) {
      stop(
        sprintf(
          "rhs_ns cannot supply both %s and %s.",
          pair[[1L]], pair[[2L]]
        ),
        call. = FALSE
      )
    }
  }
  if (!is.null(rhs_ns$shrink_intercept) &&
      !identical(
        .rqr_scalar_logical(
          rhs_ns$shrink_intercept, "rhs_ns$shrink_intercept"
        ),
        FALSE
      )) {
    stop(
      "Native RHS-NS requires one explicit unshrunk intercept; shrink_intercept must be FALSE.",
      call. = FALSE
    )
  }
  intercept_name <- .rqr_prior_scalar_text(
    rhs_ns$intercept_name %||% rhs_ns$intercept,
    "rhs_ns$intercept_name"
  )
  tau0 <- .rqr_prior_scalar_positive(
    rhs_ns$tau0 %||% 1, "rhs_ns$tau0"
  )
  a_zeta <- .rqr_prior_scalar_positive(
    rhs_ns$a_zeta %||% 2, "rhs_ns$a_zeta"
  )
  b_zeta <- .rqr_prior_scalar_positive(
    rhs_ns$b_zeta %||% 1, "rhs_ns$b_zeta"
  )
  intercept_mean <- .rqr_prior_scalar_finite(
    rhs_ns$intercept_mean %||% 0, "rhs_ns$intercept_mean"
  )
  intercept_precision <- .rqr_prior_scalar_positive(
    rhs_ns$intercept_precision %||%
      rhs_ns$intercept_prec %||% 1e-16,
    "rhs_ns$intercept_precision"
  )
  zeta2_fixed <- rhs_ns$zeta2_fixed %||% rhs_ns$c2_fixed
  if (!is.null(zeta2_fixed)) {
    if (length(zeta2_fixed) == 1L && is.na(zeta2_fixed)) {
      zeta2_fixed <- NULL
    } else {
      zeta2_fixed <- .rqr_prior_scalar_positive(
        zeta2_fixed, "rhs_ns$zeta2_fixed"
      )
    }
  }

  prior <- list(
    schema_version = .rqr_beta_prior_schema(),
    type = "rhs_ns",
    root_prior_contract = "shared_exchangeable",
    root_priors_exchangeable = TRUE,
    stateful = TRUE,
    dimension = NULL,
    hypers = list(
      tau0 = tau0,
      a_zeta = a_zeta,
      b_zeta = b_zeta,
      zeta2_fixed = zeta2_fixed,
      intercept_name = intercept_name,
      intercept_mean = intercept_mean,
      intercept_precision = intercept_precision
    ),
    canonical = NULL,
    design_contract = NULL,
    implementation = "native",
    hierarchy = paste(
      "nishimura_suchard_fictitious_normal_shoulder",
      "with_makalic_schmidt_inverse_gamma_auxiliaries",
      sep = "_"
    ),
    stochastic_floor = NULL
  )
  .rqr_prior_assert_closure_free(prior)
  structure(prior, class = c("rqr_beta_prior", "list"))
}

.rqr_rhs_ns_expand_positive <- function(x, n, name, element_names = NULL) {
  if (n == 0L) {
    return(stats::setNames(numeric(0), character(0)))
  }
  x <- as.numeric(x)
  if (length(x) == 1L) x <- rep(x, n)
  if (length(x) != n || anyNA(x) || any(!is.finite(x)) || any(x <= 0)) {
    stop(
      sprintf("%s must be finite and positive, with length one or %d.", name, n),
      call. = FALSE
    )
  }
  if (!is.null(element_names)) names(x) <- element_names
  x
}

.rqr_rhs_ns_state_validate <- function(prior, state) {
  if (!identical(prior$type, "rhs_ns") ||
      is.null(prior$design_contract) ||
      is.na(prior$design_contract$intercept_index)) {
    stop("RHS-NS requires a design-bound native prior.", call. = FALSE)
  }
  if (!is.list(state) ||
      !identical(state$schema_version, .rqr_rhs_ns_state_schema()) ||
      !identical(state$type, "rhs_ns")) {
    stop("Invalid native RHS-NS state schema.", call. = FALSE)
  }
  expected_fields <- c(
    "schema_version", "type", "dimension", "intercept_index",
    "active_index", "active_names", "lambda2", "nu", "tau2", "xi",
    "zeta2", "zeta2_is_fixed", "update_count",
    "numerical_repair_count"
  )
  if (!identical(names(state), expected_fields)) {
    stop("RHS-NS state fields are not in canonical form.",
         call. = FALSE)
  }
  .rqr_prior_assert_closure_free(state, "RHS-NS state")
  p <- .rqr_prior_dimension(prior)
  active_index <- as.integer(prior$design_contract$active_index)
  valid_dimension <- is.numeric(state$dimension) &&
    length(state$dimension) == 1L &&
    !is.na(state$dimension) && is.finite(state$dimension) &&
    state$dimension == floor(state$dimension) && state$dimension >= 1L
  valid_intercept <- is.numeric(state$intercept_index) &&
    length(state$intercept_index) == 1L &&
    !is.na(state$intercept_index) && is.finite(state$intercept_index) &&
    state$intercept_index == floor(state$intercept_index) &&
    state$intercept_index >= 1L && state$intercept_index <= p
  valid_active <- is.numeric(state$active_index) &&
    all(!is.na(state$active_index)) &&
    all(is.finite(state$active_index)) &&
    all(state$active_index == floor(state$active_index)) &&
    all(state$active_index >= 1L) && all(state$active_index <= p) &&
    !anyDuplicated(state$active_index)
  if (!valid_dimension || !valid_intercept || !valid_active ||
      !identical(as.integer(state$dimension), p) ||
      !identical(as.integer(state$intercept_index),
                 as.integer(prior$design_contract$intercept_index)) ||
      !identical(as.integer(state$active_index), active_index)) {
    stop("RHS-NS state does not match the design-bound prior.", call. = FALSE)
  }
  expected_active_names <-
    prior$design_contract$column_names[active_index]
  if (!is.character(state$active_names) ||
      !identical(state$active_names, expected_active_names)) {
    stop(
      "RHS-NS active_names do not match the design-bound prior.",
      call. = FALSE
    )
  }
  m <- length(active_index)
  if (length(state$lambda2) != m || length(state$nu) != m ||
      anyNA(state$lambda2) || anyNA(state$nu) ||
      any(!is.finite(state$lambda2)) || any(!is.finite(state$nu)) ||
      any(state$lambda2 <= 0) || any(state$nu <= 0)) {
    stop("RHS-NS local-scale state is invalid.", call. = FALSE)
  }
  for (field in c("tau2", "xi", "zeta2")) {
    .rqr_prior_scalar_positive(
      state[[field]], sprintf("RHS-NS state$%s", field)
    )
  }
  fixed <- prior$hypers$zeta2_fixed
  if (!is.logical(state$zeta2_is_fixed) ||
      length(state$zeta2_is_fixed) != 1L ||
      is.na(state$zeta2_is_fixed)) {
    stop("RHS-NS zeta2_is_fixed must be TRUE or FALSE.",
         call. = FALSE)
  }
  if (is.null(fixed)) {
    if (isTRUE(state$zeta2_is_fixed)) {
      stop("RHS-NS state incorrectly marks the shoulder as fixed.",
           call. = FALSE)
    }
  } else {
    if (!isTRUE(state$zeta2_is_fixed) ||
        !identical(as.numeric(state$zeta2), as.numeric(fixed))) {
      stop("RHS-NS state does not preserve the declared fixed shoulder.",
           call. = FALSE)
    }
  }
  if (!is.numeric(state$update_count) || length(state$update_count) != 1L ||
      is.na(state$update_count) || !is.finite(state$update_count) ||
      state$update_count != floor(state$update_count) ||
      state$update_count < 0 ||
      state$update_count > .Machine$integer.max) {
    stop("RHS-NS update_count must be a nonnegative integer.", call. = FALSE)
  }
  if (!is.numeric(state$numerical_repair_count) ||
      length(state$numerical_repair_count) != 1L ||
      is.na(state$numerical_repair_count) ||
      !is.finite(state$numerical_repair_count) ||
      state$numerical_repair_count !=
        floor(state$numerical_repair_count) ||
      state$numerical_repair_count < 0 ||
      state$numerical_repair_count > .Machine$integer.max) {
    stop(
      "RHS-NS numerical_repair_count must be a nonnegative integer.",
      call. = FALSE
    )
  }
  if (!identical(
      as.integer(state$numerical_repair_count), 0L
    )) {
    stop(
      paste(
        "RHS-NS v1 has no repair path;",
        "numerical_repair_count must be zero."
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.rqr_rhs_ns_state_init <- function(prior, init = NULL) {
  if (is.null(init)) init <- list()
  if (!is.list(init)) stop("RHS-NS init must be a list.", call. = FALSE)
  .rqr_validate_named_list_fields(
    init, "RHS-NS init",
    c(
      "lambda2", "init_lambda2", "nu", "init_nu",
      "tau2", "init_tau2", "xi", "init_xi",
      "zeta2", "init_zeta2", "update_count",
      "numerical_repair_count"
    )
  )
  alias_pairs <- list(
    c("lambda2", "init_lambda2"),
    c("nu", "init_nu"),
    c("tau2", "init_tau2"),
    c("xi", "init_xi"),
    c("zeta2", "init_zeta2")
  )
  for (pair in alias_pairs) {
    if (all(pair %in% names(init))) {
      stop(
        sprintf(
          "RHS-NS init cannot supply both %s and %s.",
          pair[[1L]], pair[[2L]]
        ),
        call. = FALSE
      )
    }
  }
  .rqr_prior_assert_closure_free(init, "RHS-NS init")
  if (is.null(prior$design_contract) ||
      is.na(prior$design_contract$intercept_index)) {
    stop("Validate the RHS-NS prior against X before initialization.",
         call. = FALSE)
  }
  p <- .rqr_prior_dimension(prior)
  active_index <- as.integer(prior$design_contract$active_index)
  active_names <- prior$design_contract$column_names[active_index]
  m <- length(active_index)
  lambda2 <- .rqr_rhs_ns_expand_positive(
    init$lambda2 %||% init$init_lambda2 %||% 1,
    m, "RHS-NS init$lambda2", active_names
  )
  nu <- .rqr_rhs_ns_expand_positive(
    init$nu %||% init$init_nu %||% 1,
    m, "RHS-NS init$nu", active_names
  )
  tau2 <- .rqr_prior_scalar_positive(
    init$tau2 %||% init$init_tau2 %||% prior$hypers$tau0^2,
    "RHS-NS init$tau2"
  )
  xi <- .rqr_prior_scalar_positive(
    init$xi %||% init$init_xi %||% 1,
    "RHS-NS init$xi"
  )
  fixed <- prior$hypers$zeta2_fixed
  supplied_zeta <- init$zeta2 %||% init$init_zeta2
  if (!is.null(fixed) && !is.null(supplied_zeta) &&
      !identical(as.numeric(supplied_zeta), as.numeric(fixed))) {
    stop(
      "RHS-NS init$zeta2 cannot differ from the fixed shoulder.",
      call. = FALSE
    )
  }
  zeta2 <- if (!is.null(fixed)) {
    fixed
  } else {
    .rqr_prior_scalar_positive(
      supplied_zeta %||% 1, "RHS-NS init$zeta2"
    )
  }
  update_count <- init$update_count %||% 0L
  if (!is.numeric(update_count) || length(update_count) != 1L ||
      is.na(update_count) || !is.finite(update_count) ||
      update_count != floor(update_count) || update_count < 0 ||
      update_count > .Machine$integer.max) {
    stop(
      "RHS-NS init$update_count must be a nonnegative integer.",
      call. = FALSE
    )
  }
  numerical_repair_count <-
    init$numerical_repair_count %||% 0L
  if (!is.numeric(numerical_repair_count) ||
      length(numerical_repair_count) != 1L ||
      is.na(numerical_repair_count) ||
      !is.finite(numerical_repair_count) ||
      numerical_repair_count != floor(numerical_repair_count) ||
      numerical_repair_count < 0 ||
      numerical_repair_count > .Machine$integer.max) {
    stop(
      paste(
        "RHS-NS init$numerical_repair_count must be a",
        "nonnegative integer."
      ),
      call. = FALSE
    )
  }
  if (!identical(as.integer(numerical_repair_count), 0L)) {
    stop(
      paste(
        "RHS-NS v1 has no repair path;",
        "init$numerical_repair_count must be zero."
      ),
      call. = FALSE
    )
  }
  state <- list(
    schema_version = .rqr_rhs_ns_state_schema(),
    type = "rhs_ns",
    dimension = p,
    intercept_index = as.integer(
      prior$design_contract$intercept_index
    ),
    active_index = active_index,
    active_names = active_names,
    lambda2 = lambda2,
    nu = nu,
    tau2 = tau2,
    xi = xi,
    zeta2 = zeta2,
    zeta2_is_fixed = !is.null(fixed),
    update_count = as.integer(update_count),
    numerical_repair_count =
      as.integer(numerical_repair_count)
  )
  .rqr_rhs_ns_state_validate(prior, state)
  structure(state, class = c("rqr_rhs_ns_state", "rqr_beta_prior_state", "list"))
}

.rqr_rhs_ns_positive_precision_component <- function(log_value, name) {
  value <- exp(log_value)
  if (!is.finite(value) || value <= 0) {
    stop(
      sprintf(
        "%s is outside the finite positive floating-point range; no stochastic clipping was applied.",
        name
      ),
      call. = FALSE
    )
  }
  value
}

.rqr_rhs_ns_precision <- function(prior, state) {
  .rqr_rhs_ns_state_validate(prior, state)
  p <- .rqr_prior_dimension(prior)
  precision <- rep(NA_real_, p)
  intercept_index <- prior$design_contract$intercept_index
  precision[intercept_index] <- prior$hypers$intercept_precision
  if (length(state$active_index)) {
    horseshoe_precision <- vapply(
      seq_along(state$active_index),
      function(j) {
        .rqr_rhs_ns_positive_precision_component(
          -log(state$tau2) - log(state$lambda2[j]),
          sprintf("RHS-NS horseshoe precision for active coefficient %d", j)
        )
      },
      numeric(1L)
    )
    shoulder_precision <- .rqr_rhs_ns_positive_precision_component(
      -log(state$zeta2), "RHS-NS shoulder precision"
    )
    active_precision <- horseshoe_precision + shoulder_precision
    if (any(!is.finite(active_precision)) || any(active_precision <= 0)) {
      stop(
        paste(
          "RHS-NS combined precision is outside the finite positive",
          "floating-point range; no stochastic clipping was applied."
        ),
        call. = FALSE
      )
    }
    precision[state$active_index] <- active_precision
  }
  if (any(!is.finite(precision)) || any(precision <= 0)) {
    stop("RHS-NS produced an invalid coefficient precision.", call. = FALSE)
  }
  precision
}

.rqr_rhs_ns_conditional_lambda <- function(beta, tau2, nu) {
  beta <- .rqr_prior_scalar_finite(beta, "beta")
  tau2 <- .rqr_prior_scalar_positive(tau2, "tau2")
  nu <- .rqr_prior_scalar_positive(nu, "nu")
  rate <- 0.5 * beta^2 / tau2 + 1 / nu
  list(
    shape = 1,
    rate = .rqr_prior_scalar_positive(rate, "lambda2 conditional rate")
  )
}

.rqr_rhs_ns_conditional_nu <- function(lambda2) {
  lambda2 <- .rqr_prior_scalar_positive(lambda2, "lambda2")
  list(
    shape = 1,
    rate = .rqr_prior_scalar_positive(
      1 + 1 / lambda2, "nu conditional rate"
    )
  )
}

.rqr_rhs_ns_conditional_tau <- function(beta, lambda2, xi) {
  beta <- as.numeric(beta)
  lambda2 <- as.numeric(lambda2)
  if (length(beta) != length(lambda2) || any(!is.finite(beta)) ||
      any(!is.finite(lambda2)) || any(lambda2 <= 0)) {
    stop("tau2 conditional inputs are invalid.", call. = FALSE)
  }
  xi <- .rqr_prior_scalar_positive(xi, "xi")
  rate <- 0.5 * sum(beta^2 / lambda2) + 1 / xi
  list(
    shape = (length(beta) + 1) / 2,
    rate = .rqr_prior_scalar_positive(rate, "tau2 conditional rate")
  )
}

.rqr_rhs_ns_conditional_xi <- function(tau2, tau0) {
  tau2 <- .rqr_prior_scalar_positive(tau2, "tau2")
  tau0 <- .rqr_prior_scalar_positive(tau0, "tau0")
  list(
    shape = 1,
    rate = .rqr_prior_scalar_positive(
      1 / tau0^2 + 1 / tau2, "xi conditional rate"
    )
  )
}

.rqr_rhs_ns_conditional_zeta <- function(beta, a_zeta, b_zeta) {
  beta <- as.numeric(beta)
  if (any(!is.finite(beta))) {
    stop("zeta2 conditional beta must be finite.", call. = FALSE)
  }
  a_zeta <- .rqr_prior_scalar_positive(a_zeta, "a_zeta")
  b_zeta <- .rqr_prior_scalar_positive(b_zeta, "b_zeta")
  rate <- b_zeta + 0.5 * sum(beta^2)
  list(
    shape = a_zeta + length(beta) / 2,
    rate = .rqr_prior_scalar_positive(rate, "zeta2 conditional rate")
  )
}

.rqr_rhs_ns_conditional_parameters <- function(prior, state, beta) {
  .rqr_rhs_ns_state_validate(prior, state)
  beta <- as.numeric(beta)
  p <- .rqr_prior_dimension(prior)
  if (length(beta) != p || any(!is.finite(beta))) {
    stop("beta must be finite and match the RHS-NS dimension.", call. = FALSE)
  }
  active_beta <- beta[state$active_index]
  lambda <- lapply(seq_along(active_beta), function(j) {
    .rqr_rhs_ns_conditional_lambda(
      active_beta[j], state$tau2, state$nu[j]
    )
  })
  nu <- lapply(state$lambda2, .rqr_rhs_ns_conditional_nu)
  list(
    lambda2 = lambda,
    nu = nu,
    tau2 = .rqr_rhs_ns_conditional_tau(
      active_beta, state$lambda2, state$xi
    ),
    xi = .rqr_rhs_ns_conditional_xi(
      state$tau2, prior$hypers$tau0
    ),
    zeta2 = if (isTRUE(state$zeta2_is_fixed)) NULL else
      .rqr_rhs_ns_conditional_zeta(
        active_beta, prior$hypers$a_zeta, prior$hypers$b_zeta
      )
  )
}

.rqr_rhs_ns_rinvgamma <- function(shape, rate, name) {
  shape <- .rqr_prior_scalar_positive(shape, sprintf("%s shape", name))
  rate <- .rqr_prior_scalar_positive(rate, sprintf("%s rate", name))
  gamma_draw <- stats::rgamma(1L, shape = shape, rate = rate)
  if (!is.finite(gamma_draw) || gamma_draw <= 0) {
    stop(
      sprintf(
        "%s Gamma reciprocal draw is not finite and positive; no stochastic clipping was applied.",
        name
      ),
      call. = FALSE
    )
  }
  draw <- 1 / gamma_draw
  if (!is.finite(draw) || draw <= 0) {
    stop(
      sprintf(
        "%s inverse-Gamma draw is outside the finite positive floating-point range; no stochastic clipping was applied.",
        name
      ),
      call. = FALSE
    )
  }
  draw
}

.rqr_rhs_ns_state_update <- function(
    prior, state, beta,
    numerical_policy = c("fail", "record_repair")) {
  numerical_policy <- .rqr_numerical_policy(numerical_policy)
  .rqr_rhs_ns_state_validate(prior, state)
  beta <- as.numeric(beta)
  p <- .rqr_prior_dimension(prior)
  if (length(beta) != p || any(!is.finite(beta))) {
    stop("beta must be finite and match the RHS-NS dimension.", call. = FALSE)
  }
  active_beta <- beta[state$active_index]
  next_state <- state

  if (length(active_beta)) {
    for (j in seq_along(active_beta)) {
      lambda_cond <- .rqr_rhs_ns_conditional_lambda(
        active_beta[j], next_state$tau2, next_state$nu[j]
      )
      next_state$lambda2[j] <- .rqr_rhs_ns_rinvgamma(
        lambda_cond$shape, lambda_cond$rate,
        sprintf("RHS-NS lambda2[%d]", j)
      )
      nu_cond <- .rqr_rhs_ns_conditional_nu(next_state$lambda2[j])
      next_state$nu[j] <- .rqr_rhs_ns_rinvgamma(
        nu_cond$shape, nu_cond$rate,
        sprintf("RHS-NS nu[%d]", j)
      )
    }
  }

  tau_cond <- .rqr_rhs_ns_conditional_tau(
    active_beta, next_state$lambda2, next_state$xi
  )
  next_state$tau2 <- .rqr_rhs_ns_rinvgamma(
    tau_cond$shape, tau_cond$rate, "RHS-NS tau2"
  )
  xi_cond <- .rqr_rhs_ns_conditional_xi(
    next_state$tau2, prior$hypers$tau0
  )
  next_state$xi <- .rqr_rhs_ns_rinvgamma(
    xi_cond$shape, xi_cond$rate, "RHS-NS xi"
  )
  if (!isTRUE(next_state$zeta2_is_fixed)) {
    zeta_cond <- .rqr_rhs_ns_conditional_zeta(
      active_beta, prior$hypers$a_zeta, prior$hypers$b_zeta
    )
    next_state$zeta2 <- .rqr_rhs_ns_rinvgamma(
      zeta_cond$shape, zeta_cond$rate, "RHS-NS zeta2"
    )
  }
  if (next_state$update_count >= .Machine$integer.max) {
    stop("RHS-NS update_count exceeds the supported integer range.",
         call. = FALSE)
  }
  next_state$update_count <- as.integer(next_state$update_count + 1L)
  next_state$numerical_repair_count <-
    as.integer(state$numerical_repair_count)
  .rqr_rhs_ns_state_validate(prior, next_state)
  next_state <- structure(
    next_state,
    class = c("rqr_rhs_ns_state", "rqr_beta_prior_state", "list")
  )
  list(
    state = next_state,
    stats = .rqr_rhs_ns_diagnostics(prior, next_state),
    numerical_policy = numerical_policy,
    numerical_repair_count = 0L
  )
}

.rqr_rhs_ns_diagnostics <- function(prior, state) {
  .rqr_rhs_ns_state_validate(prior, state)
  active_count <- length(state$active_index)
  list(
    schema_version = .rqr_rhs_ns_state_schema(),
    type = "rhs_ns",
    hierarchy = prior$hierarchy,
    dimension = state$dimension,
    intercept_name = prior$hypers$intercept_name,
    intercept_index = state$intercept_index,
    active_count = active_count,
    active_names = state$active_names,
    tau = sqrt(state$tau2),
    tau2 = state$tau2,
    zeta2 = state$zeta2,
    zeta2_is_fixed = state$zeta2_is_fixed,
    lambda_min = if (active_count) min(sqrt(state$lambda2)) else NA_real_,
    lambda_mean = if (active_count) mean(sqrt(state$lambda2)) else NA_real_,
    lambda_max = if (active_count) max(sqrt(state$lambda2)) else NA_real_,
    update_count = as.integer(state$update_count),
    root_priors_exchangeable = TRUE,
    stochastic_floor = NULL,
    numerical_repair_count = 0L
  )
}

.rqr_rhs_ns_log_invgamma <- function(x, shape, rate) {
  x <- .rqr_prior_scalar_positive(x, "inverse-Gamma variate")
  shape <- .rqr_prior_scalar_positive(shape, "inverse-Gamma shape")
  rate <- .rqr_prior_scalar_positive(rate, "inverse-Gamma rate")
  shape * log(rate) - lgamma(shape) -
    (shape + 1) * log(x) - rate / x
}

.rqr_rhs_ns_log_normal <- function(x, mean = 0, log_variance) {
  x <- .rqr_prior_scalar_finite(x, "Normal variate")
  mean <- .rqr_prior_scalar_finite(mean, "Normal mean")
  log_variance <- .rqr_prior_scalar_finite(
    log_variance, "Normal log variance"
  )
  standardized_square <- (x - mean)^2 * exp(-log_variance)
  if (!is.finite(standardized_square)) return(-Inf)
  -0.5 * (log(2 * pi) + log_variance + standardized_square)
}

.rqr_rhs_ns_log_kernel <- function(prior, state, beta) {
  .rqr_rhs_ns_state_validate(prior, state)
  beta <- as.numeric(beta)
  p <- .rqr_prior_dimension(prior)
  if (length(beta) != p || any(!is.finite(beta))) {
    stop("beta must be finite and match the RHS-NS dimension.", call. = FALSE)
  }
  intercept_index <- state$intercept_index
  value <- .rqr_rhs_ns_log_normal(
    beta[intercept_index],
    mean = prior$hypers$intercept_mean,
    log_variance = -log(prior$hypers$intercept_precision)
  )
  if (length(state$active_index)) {
    for (j in seq_along(state$active_index)) {
      coefficient <- beta[state$active_index[j]]
      value <- value +
        .rqr_rhs_ns_log_normal(
          coefficient,
          log_variance = log(state$tau2) + log(state$lambda2[j])
        ) +
        .rqr_rhs_ns_log_normal(
          coefficient, log_variance = log(state$zeta2)
        ) +
        .rqr_rhs_ns_log_invgamma(
          state$lambda2[j], 0.5, 1 / state$nu[j]
        ) +
        .rqr_rhs_ns_log_invgamma(state$nu[j], 0.5, 1)
    }
  }
  value <- value +
    .rqr_rhs_ns_log_invgamma(state$tau2, 0.5, 1 / state$xi) +
    .rqr_rhs_ns_log_invgamma(
      state$xi, 0.5, 1 / prior$hypers$tau0^2
    )
  if (!isTRUE(state$zeta2_is_fixed)) {
    value <- value + .rqr_rhs_ns_log_invgamma(
      state$zeta2, prior$hypers$a_zeta, prior$hypers$b_zeta
    )
  }
  as.numeric(value)
}

.rqr_rhs_ns_target_contract <- function(prior) {
  if (!identical(prior$type, "rhs_ns")) {
    stop("Expected a native RHS-NS prior.", call. = FALSE)
  }
  .rqr_prior_assert_closure_free(prior)
  list(
    schema_version = prior$schema_version,
    hierarchy = prior$hierarchy,
    root_prior_contract = prior$root_prior_contract,
    root_priors_exchangeable = prior$root_priors_exchangeable,
    dimension = prior$dimension,
    hypers = prior$hypers,
    design_contract = prior$design_contract,
    stochastic_floor = NULL,
    exact_gibbs_conditionals = TRUE
  )
}
