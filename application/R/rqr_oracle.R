#' RQR oracle roots for centered innovation laws
#'
#' The RQR interval roots `(a_c, b_c)` are defined by the coverage and
#' first-moment balance equations
#' `F(b_c) - F(a_c) = c` and
#' `M(b_c) - M(a_c) = c * E(E)`, where
#' `M(z) = E[E 1(E <= z)]`.  The helper is intended for simulation design and
#' endpoint-recovery audits; it is not used as a fitted model.
#'
#' @param family Innovation family. Supported canonical values are
#'   `"gaussian"`, `"laplace"`, `"student_t"`, `"centered_gamma"`,
#'   `"centered_exponential"`, `"asymmetric_laplace"`,
#'   `"gaussian_mixture"`, `"centered_lognormal"`,
#'   `"centered_standardized_lognormal"`, `"standardized_beta"`,
#'   `"normal_t_mixture"`, and `"standardized_skewed_normal_t_mixture"`.
#' @param coverage_level Target interval coverage in `(0, 1)`.
#' @param params Optional family parameters.
#' @param tol Numerical tolerance for the scalar root search.
#' @return A list with roots, coverage/moment residuals, and family metadata.
#' @export
rqr_oracle_roots <- function(family, coverage_level, params = list(), tol = 1e-8) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
  family <- tolower(gsub("-", "_", as.character(family)[1L]))
  c0 <- as.numeric(coverage_level)[1L]
  if (!is.finite(c0) || c0 <= 0 || c0 >= 1) {
    stop("coverage_level must be a finite scalar in (0, 1).", call. = FALSE)
  }
  spec <- .rqr_oracle_family_spec(family, params %||% list())
  eps <- max(1e-7, tol)
  p_grid <- seq(eps, 1 - c0 - eps, length.out = 401L)
  balance_at <- function(pa) {
    a <- spec$q(pa)
    b <- spec$q(pa + c0)
    spec$M(b) - spec$M(a) - c0 * spec$mean
  }
  vals <- vapply(p_grid, balance_at, numeric(1))
  finite <- is.finite(vals)
  if (!any(finite)) {
    stop(sprintf("RQR oracle balance could not be evaluated for family '%s'.", family), call. = FALSE)
  }
  p_grid <- p_grid[finite]
  vals <- vals[finite]
  sign_change <- which(vals[-length(vals)] * vals[-1L] <= 0)
  if (length(sign_change)) {
    ii <- sign_change[[1L]]
    pa <- stats::uniroot(balance_at, c(p_grid[ii], p_grid[ii + 1L]), tol = tol)$root
    method <- "root"
  } else {
    opt <- stats::optimize(function(pa) abs(balance_at(pa)), c(eps, 1 - c0 - eps))
    pa <- opt$minimum
    method <- "minimum_abs_balance"
  }
  a <- spec$q(pa)
  b <- spec$q(pa + c0)
  cov_resid <- spec$F(b) - spec$F(a) - c0
  mom_resid <- spec$M(b) - spec$M(a) - c0 * spec$mean
  list(
    family = family,
    coverage_level = c0,
    lower_root = as.numeric(a),
    upper_root = as.numeric(b),
    lower_probability = as.numeric(pa),
    upper_probability = as.numeric(pa + c0),
    mean = as.numeric(spec$mean),
    coverage_residual = as.numeric(cov_resid),
    moment_residual = as.numeric(mom_resid),
    method = method,
    params = spec$params
  )
}

#' Location-scale RQR oracle endpoints
#'
#' @param mu,sigma Location and positive scale vectors.
#' @inheritParams rqr_oracle_roots
#' @return A data frame with lower, upper, midpoint, and width.
#' @export
rqr_oracle_endpoints <- function(mu, sigma = 1, family, coverage_level, params = list(), tol = 1e-8) {
  roots <- rqr_oracle_roots(family, coverage_level, params = params, tol = tol)
  mu <- as.numeric(mu)
  sigma <- as.numeric(sigma)
  if (length(sigma) == 1L) sigma <- rep(sigma, length(mu))
  if (length(mu) != length(sigma)) stop("mu and sigma must have compatible lengths.", call. = FALSE)
  if (any(!is.finite(mu)) || any(!is.finite(sigma)) || any(sigma <= 0)) {
    stop("mu must be finite and sigma must be finite and positive.", call. = FALSE)
  }
  lower <- mu + sigma * roots$lower_root
  upper <- mu + sigma * roots$upper_root
  data.frame(
    lower = lower,
    upper = upper,
    midpoint = 0.5 * (lower + upper),
    width = upper - lower,
    lower_root = roots$lower_root,
    upper_root = roots$upper_root
  )
}

#' Certified fixed-content interval oracle
#'
#' Constructs a population certificate for the ordinary mean-preserving RQR
#' interval, the equal-tailed interval, or the shortest contiguous interval.
#' The recovery tilt is the conditional retained mean minus the population
#' mean. It is fixed target metadata for a generalized-Bayes loss update, not a
#' response-likelihood parameter.
#'
#' @param target One of `"RQR"`, `"ET"`, or `"SH"`.
#' @param grid_size Odd deterministic profile-grid size used for independent
#'   optimization checks.
#' @inheritParams rqr_oracle_roots
#' @return A versioned `rqr_interval_oracle` certificate, including the
#'   target-defining probabilities, exact recovery tilt, numerical checks,
#'   uniqueness flag, and detected minimizer set.
#' @export
rqr_interval_oracle <- function(
    family, coverage_level, target = c("RQR", "ET", "SH"),
    params = list(), tol = 1e-10, grid_size = 1601L) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
  family <- tolower(gsub("-", "_", as.character(family)[1L]))
  target <- match.arg(toupper(as.character(target)[1L]), c("RQR", "ET", "SH"))
  c0 <- as.numeric(coverage_level)[1L]
  if (!is.finite(c0) || c0 <= 0 || c0 >= 1) {
    stop("coverage_level must be a finite scalar in (0, 1).", call. = FALSE)
  }
  tol <- as.numeric(tol)[1L]
  if (!is.finite(tol) || tol <= 0 || tol >= 0.01) {
    stop("tol must be finite and in (0, 0.01).", call. = FALSE)
  }
  if (!is.numeric(grid_size) || length(grid_size) != 1L ||
      is.na(grid_size) || !is.finite(grid_size) ||
      grid_size != floor(grid_size) || grid_size < 101L ||
      grid_size %% 2L != 1L || grid_size > .Machine$integer.max) {
    stop("grid_size must be an odd finite integer of at least 101.", call. = FALSE)
  }
  grid_size <- as.integer(grid_size)
  spec <- .rqr_oracle_family_spec(family, params %||% list())
  upper_u <- 1 - c0
  eps <- min(upper_u / 1000, max(1e-12, tol))
  width_at <- function(u) as.numeric(spec$q(u + c0) - spec$q(u))

  numerical_sh_u <- NA_real_
  analytic_sh_u <- NA_real_
  target_reference <- NULL
  minimizer_set <- NULL
  if (identical(target, "RQR")) {
    target_reference <- rqr_oracle_certificate(
      family, c0, params = params, tol = tol, grid_size = grid_size
    )
    retained_mean_balance <- function(u) {
      lower <- spec$q(u)
      upper <- spec$q(u + c0)
      as.numeric(spec$M(upper) - spec$M(lower) - c0 * spec$mean)
    }
    root_grid <- seq(eps, upper_u - eps, length.out = grid_size)
    balance_grid <- vapply(root_grid, retained_mean_balance, numeric(1L))
    exact_index <- which(balance_grid == 0)
    if (length(exact_index)) {
      u <- root_grid[[exact_index[[which.min(abs(
        root_grid[exact_index] - target_reference$lower_probability
      ))]]]]
    } else {
      sign_change <- which(
        balance_grid[-length(balance_grid)] * balance_grid[-1L] < 0
      )
      if (!length(sign_change)) {
        stop(
          "Could not bracket the retained-mean balance for the RQR oracle.",
          call. = FALSE
        )
      }
      nearest <- sign_change[[which.min(abs(
        0.5 * (root_grid[sign_change] + root_grid[sign_change + 1L]) -
          target_reference$lower_probability
      ))]]
      u <- stats::uniroot(
        retained_mean_balance,
        interval = root_grid[c(nearest, nearest + 1L)],
        tol = .Machine$double.eps^0.75,
        maxiter = 1000L
      )$root
    }
    target_method <- "direct_retained_mean_balance_root"
    unique_minimizer <- isTRUE(target_reference$unique_minimizer)
    minimizer_set <- target_reference$minimizer_set
  } else if (identical(target, "ET")) {
    u <- upper_u / 2
    target_method <- "equal_tail_probabilities"
    unique_minimizer <- TRUE
    minimizer_set <- data.frame(
      lower_probability = u, objective = width_at(u),
      stringsAsFactors = FALSE
    )
  } else {
    grid <- seq(eps, upper_u - eps, length.out = grid_size)
    widths <- vapply(grid, width_at, numeric(1L))
    best <- which.min(widths)
    lo <- grid[[max(1L, best - 1L)]]
    hi <- grid[[min(grid_size, best + 1L)]]
    numerical_sh_u <- if (lo < hi) {
      stats::optimize(width_at, c(lo, hi), tol = tol)$minimum
    } else {
      grid[[best]]
    }
    if (identical(spec$family, "asymmetric_laplace")) {
      analytic_sh_u <- spec$params$tau * upper_u
      u <- analytic_sh_u
      target_method <- "analytic_asymmetric_laplace_shortest_window"
    } else {
      u <- numerical_sh_u
      target_method <- "deterministic_profile_shortest_window"
    }
    objective_tolerance <- max(100 * tol, 1e-10) *
      max(1, abs(width_at(u)))
    local <- c(
      TRUE,
      widths[2L:(grid_size - 1L)] <= widths[1L:(grid_size - 2L)] &
        widths[2L:(grid_size - 1L)] <= widths[3L:grid_size],
      TRUE
    )
    competing <- which(local & widths <= width_at(u) + objective_tolerance)
    if (identical(spec$family, "asymmetric_laplace")) {
      unique_minimizer <- TRUE
      minimizer_set <- data.frame(
        lower_probability = u, objective = width_at(u),
        stringsAsFactors = FALSE
      )
    } else {
      unique_minimizer <- length(competing) == 1L
      minimizer_set <- data.frame(
        lower_probability = grid[competing], objective = widths[competing],
        stringsAsFactors = FALSE
      )
    }
  }

  lower <- as.numeric(spec$q(u))
  upper <- as.numeric(spec$q(u + c0))
  if (!is.finite(lower) || !is.finite(upper) || lower >= upper) {
    stop("The selected oracle does not have finite ordered roots.", call. = FALSE)
  }
  content <- as.numeric(spec$F(upper) - spec$F(lower))
  truncated_first_moment <- as.numeric(spec$M(upper) - spec$M(lower))
  conditional_retained_mean <- truncated_first_moment / c0
  mean_tilt <- conditional_retained_mean - spec$mean
  canonical_zero_tolerance <- max(100 * tol, 1e-12) *
    max(1, sqrt(spec$second_moment))
  if (abs(mean_tilt) <= canonical_zero_tolerance) mean_tilt <- 0

  integration_cuts <- sort(unique(c(
    lower,
    (spec$kinks %||% numeric(0))[
      (spec$kinks %||% numeric(0)) > lower &
        (spec$kinks %||% numeric(0)) < upper
    ],
    upper
  )))
  numerical_pieces <- lapply(seq_len(length(integration_cuts) - 1L), function(ii) {
    stats::integrate(
      function(z) z * spec$d(z),
      lower = integration_cuts[[ii]],
      upper = integration_cuts[[ii + 1L]],
      rel.tol = max(tol, .Machine$double.eps^0.75),
      subdivisions = 1000L,
      stop.on.error = TRUE
    )
  })
  numerical_truncated_first_moment <- sum(vapply(
    numerical_pieces, `[[`, numeric(1L), "value"
  ))
  estimated_quadrature_error <- sum(vapply(
    numerical_pieces, `[[`, numeric(1L), "abs.error"
  ))
  numerical_moment_gap <-
    numerical_truncated_first_moment - truncated_first_moment

  interior <- u > eps && u < upper_u - eps
  density_residual <- if (identical(target, "SH") && interior) {
    as.numeric(spec$d(lower) - spec$d(upper))
  } else {
    NA_real_
  }
  sh_optimizer_gap <- if (identical(target, "SH") &&
      is.finite(numerical_sh_u)) {
    as.numeric(u - numerical_sh_u)
  } else {
    NA_real_
  }
  distribution_contract <- list(
    family = spec$family,
    params = spec$params,
    mean = spec$mean,
    second_moment = spec$second_moment,
    support = spec$support
  )
  solver_contract <- list(
    schema_version = "rqrgibbs_interval_oracle/2.0.0",
    tolerance = tol,
    grid_size = grid_size,
    target_method = target_method,
    numerical_moment_reference = "split_adaptive_quadrature",
    numerical_error_role = "estimated_not_rigorous_bound",
    tilt_definition =
      "conditional_retained_mean_minus_population_mean"
  )
  certificate_contract <- list(
    distribution = distribution_contract,
    coverage_level = c0,
    target = target,
    lower_probability = u,
    upper_probability = u + c0,
    lower_root = lower,
    upper_root = upper,
    truncated_first_moment = truncated_first_moment,
    conditional_retained_mean = conditional_retained_mean,
    population_mean = spec$mean,
    mean_tilt = mean_tilt,
    unique_minimizer = unique_minimizer,
    minimizer_lower_probabilities = minimizer_set$lower_probability,
    target_method = target_method,
    solver = solver_contract
  )
  out <- c(certificate_contract, list(
    schema_version = solver_contract$schema_version,
    midpoint = 0.5 * (lower + upper),
    width = upper - lower,
    content = content,
    content_residual = content - c0,
    retained_mean_residual =
      truncated_first_moment - c0 * (spec$mean + mean_tilt),
    density_residual = density_residual,
    analytic_shortest_lower_probability = analytic_sh_u,
    numerical_shortest_lower_probability = numerical_sh_u,
    shortest_optimizer_gap = sh_optimizer_gap,
    interior_target = interior,
    unique_minimizer = unique_minimizer,
    minimizer_set = minimizer_set,
    numerical_truncated_first_moment =
      numerical_truncated_first_moment,
    numerical_moment_gap = numerical_moment_gap,
    estimated_quadrature_error = estimated_quadrature_error,
    quadrature_error_is_rigorous_bound = FALSE,
    tilt_definition = solver_contract$tilt_definition,
    uses_cornish_fisher = FALSE,
    distribution_digest = digest::digest(
      distribution_contract, algo = "sha256", serialize = TRUE
    ),
    solver_digest = digest::digest(
      solver_contract, algo = "sha256", serialize = TRUE
    ),
    certificate_digest = digest::digest(
      certificate_contract, algo = "sha256", serialize = TRUE
    ),
    rqr_reference = target_reference
  ))
  class(out) <- c("rqr_interval_oracle", "list")
  out
}

#' Transform a fixed-content oracle to location-scale endpoints
#'
#' @param location Location vector for the innovation law.
#' @param scale Positive response-scale vector.
#' @param oracle A certificate returned by [rqr_interval_oracle()].
#' @return A data frame of target endpoints, retained means, and fixed tilts.
#' @export
rqr_interval_oracle_endpoints <- function(location, scale = 1, oracle) {
  if (!inherits(oracle, "rqr_interval_oracle") ||
      !identical(
        oracle$tilt_definition,
        "conditional_retained_mean_minus_population_mean"
      )) {
    stop("oracle must be a current rqr_interval_oracle certificate.", call. = FALSE)
  }
  location <- as.numeric(location)
  scale <- as.numeric(scale)
  if (length(scale) == 1L) scale <- rep(scale, length(location))
  if (!length(location) || length(location) != length(scale) ||
      any(!is.finite(location)) || any(!is.finite(scale)) ||
      any(scale <= 0)) {
    stop("location and scale must be compatible; scale must be positive.", call. = FALSE)
  }
  lower <- location + scale * oracle$lower_root
  upper <- location + scale * oracle$upper_root
  population_mean <- location + scale * oracle$population_mean
  conditional_retained_mean <-
    location + scale * oracle$conditional_retained_mean
  data.frame(
    lower = lower,
    upper = upper,
    midpoint = 0.5 * (lower + upper),
    width = upper - lower,
    population_mean = population_mean,
    conditional_retained_mean = conditional_retained_mean,
    mean_tilt = scale * oracle$mean_tilt,
    target = oracle$target,
    coverage_level = oracle$coverage_level,
    oracle_digest = oracle$certificate_digest,
    stringsAsFactors = FALSE
  )
}

#' Exact DGP content of candidate interval endpoints
#'
#' This evaluates content under a declared location-scale data-generating law.
#' It does not construct response-predictive draws from an RQR fit.
#'
#' @param lower,upper Candidate ordered endpoint vectors.
#' @param location,scale DGP location and positive scale vectors.
#' @inheritParams rqr_oracle_roots
#' @return Numeric conditional-content values.
#' @export
rqr_oracle_conditional_content <- function(
    lower, upper, location = 0, scale = 1,
    family, params = list()) {
  values <- list(
    lower = as.numeric(lower), upper = as.numeric(upper),
    location = as.numeric(location), scale = as.numeric(scale)
  )
  n <- max(vapply(values, length, integer(1L)))
  if (!n || any(!vapply(values, function(x) length(x) %in% c(1L, n), logical(1L)))) {
    stop("lower, upper, location, and scale must have compatible lengths.", call. = FALSE)
  }
  values <- lapply(values, rep_len, length.out = n)
  if (any(!is.finite(unlist(values, use.names = FALSE))) ||
      any(values$lower >= values$upper) || any(values$scale <= 0)) {
    stop("Candidate endpoints must be finite and ordered; scale must be positive.", call. = FALSE)
  }
  spec <- .rqr_oracle_family_spec(
    tolower(gsub("-", "_", as.character(family)[1L])), params
  )
  z_lower <- (values$lower - values$location) / values$scale
  z_upper <- (values$upper - values$location) / values$scale
  as.numeric(spec$F(z_upper) - spec$F(z_lower))
}

#' Exact DGP mean-tilted RQR risk
#'
#' Evaluates the expected fixed-rate mean-tilted RQR loss under a declared
#' location-scale data-generating law. This is a population loss calculation,
#' not a response log score or likelihood evaluation.
#'
#' @param mean_tilt Fixed response-scale target tilt.
#' @inheritParams rqr_oracle_conditional_content
#' @inheritParams rqr_oracle_roots
#' @return A data frame with ordinary, linear-tilt, and total expected loss.
#' @export
rqr_oracle_tilted_risk <- function(
    lower, upper, coverage_level, mean_tilt = 0,
    location = 0, scale = 1, family, params = list()) {
  values <- list(
    lower = as.numeric(lower), upper = as.numeric(upper),
    mean_tilt = as.numeric(mean_tilt), location = as.numeric(location),
    scale = as.numeric(scale)
  )
  n <- max(vapply(values, length, integer(1L)))
  if (!n || any(!vapply(values, function(x) length(x) %in% c(1L, n), logical(1L)))) {
    stop("Risk arguments must have compatible lengths.", call. = FALSE)
  }
  values <- lapply(values, rep_len, length.out = n)
  if (any(!is.finite(unlist(values, use.names = FALSE))) ||
      any(values$lower >= values$upper) || any(values$scale <= 0)) {
    stop("Risk endpoints must be finite and ordered; scale must be positive.", call. = FALSE)
  }
  c0 <- rqr_constants(coverage_level)$alpha
  spec <- .rqr_oracle_family_spec(
    tolower(gsub("-", "_", as.character(family)[1L])), params
  )
  z_lower <- (values$lower - values$location) / values$scale
  z_upper <- (values$upper - values$location) / values$scale
  ordinary <- vapply(seq_len(n), function(ii) {
    values$scale[[ii]]^2 * .rqr_oracle_moment_risk(
      spec, z_lower[[ii]], z_upper[[ii]], c0
    )
  }, numeric(1L))
  population_mean <- values$location + values$scale * spec$mean
  linear_tilt <- c0 * values$mean_tilt *
    (values$lower + values$upper - 2 * population_mean)
  data.frame(
    ordinary_product_check_risk = ordinary,
    linear_tilt_expectation = linear_tilt,
    mean_tilted_risk = ordinary - linear_tilt,
    conditional_content = rqr_oracle_conditional_content(
      values$lower, values$upper, values$location, values$scale,
      family = family, params = params
    ),
    stringsAsFactors = FALSE
  )
}

#' Population RQR risk
#'
#' Evaluates the expected check loss of the residual product
#' `(Z - lower) * (Z - upper)` under a supported standardized innovation law.
#' Numerical integration is split at both roots.
#'
#' @param lower,upper Ordered finite interval roots.
#' @inheritParams rqr_oracle_roots
#' @return A list containing the population risk and the estimated absolute
#'   quadrature error. The error is a numerical estimate, not a rigorous bound.
#' @export
rqr_oracle_risk <- function(
    lower, upper, family, coverage_level, params = list(),
    tol = 1e-9) {
  lower <- as.numeric(lower)[1L]
  upper <- as.numeric(upper)[1L]
  if (!is.finite(lower) || !is.finite(upper) || lower >= upper) {
    stop("lower and upper must be finite ordered scalars.", call. = FALSE)
  }
  c0 <- rqr_constants(coverage_level)$alpha
  spec <- .rqr_oracle_family_spec(
    tolower(gsub("-", "_", as.character(family)[1L])), params
  )
  integrand <- function(z) {
    rqr_check_loss((z - lower) * (z - upper), c0) * spec$d(z)
  }
  inside_support <- c(lower, upper)
  inside_support <- inside_support[
    inside_support > spec$support[[1L]] &
      inside_support < spec$support[[2L]]
  ]
  cuts <- sort(unique(c(
    spec$support[[1L]], inside_support, spec$support[[2L]]
  )))
  pieces <- lapply(seq_len(length(cuts) - 1L), function(index) {
    stats::integrate(
      integrand, lower = cuts[[index]], upper = cuts[[index + 1L]],
      rel.tol = tol, abs.tol = 0, subdivisions = 1000L,
      stop.on.error = TRUE
    )
  })
  list(
    value = sum(vapply(pieces, `[[`, numeric(1L), "value")),
    estimated_absolute_error = sum(vapply(
      pieces, `[[`, numeric(1L), "abs.error"
    )),
    coverage_level = c0,
    lower_root = lower,
    upper_root = upper,
    family = spec$family,
    params = spec$params
  )
}

.rqr_oracle_moment_risk <- function(spec, lower, upper, coverage_level) {
  total_product <-
    spec$second_moment -
    (lower + upper) * spec$mean +
    lower * upper
  inside_probability <- spec$F(upper) - spec$F(lower)
  inside_first <- spec$M(upper) - spec$M(lower)
  inside_second <- spec$M2(upper) - spec$M2(lower)
  inside_product <-
    inside_second -
    (lower + upper) * inside_first +
    lower * upper * inside_probability
  as.numeric(coverage_level * total_product - inside_product)
}

#' Certified population RQR oracle
#'
#' Cross-checks an unrestricted two-dimensional optimization against the
#' independent coverage-profile parameterization
#' `a(u) = F^{-1}(u)`, `b(u) = F^{-1}(u + c)`. Every minimum basin detected on
#' the deterministic profile grid is refined separately.
#'
#' @inheritParams rqr_oracle_roots
#' @param grid_size Odd number of deterministic profile-grid points.
#' @return A versioned oracle certificate containing both optimizers,
#'   stationary residuals, objectives, curvature, numerical-error estimate,
#'   uniqueness status, and content digests.
#' @export
rqr_oracle_certificate <- function(
    family, coverage_level, params = list(), tol = 1e-8,
    grid_size = 1601L) {
  family <- tolower(gsub("-", "_", as.character(family)[1L]))
  c0 <- rqr_constants(coverage_level)$alpha
  if (!is.numeric(grid_size) || length(grid_size) != 1L ||
      is.na(grid_size) || !is.finite(grid_size) ||
      grid_size != floor(grid_size) ||
      grid_size > .Machine$integer.max) {
    stop("grid_size must be one finite whole number.", call. = FALSE)
  }
  grid_size <- as.integer(grid_size)
  if (grid_size < 101L ||
      grid_size %% 2L != 1L) {
    stop("grid_size must be an odd integer of at least 101.", call. = FALSE)
  }
  spec <- .rqr_oracle_family_spec(family, params)
  eps <- max(1e-8, tol)
  upper_u <- 1 - c0 - eps
  profile_roots <- function(u) c(spec$q(u), spec$q(u + c0))
  profile_value <- function(u) {
    roots <- profile_roots(u)
    .rqr_oracle_moment_risk(
      spec, roots[[1L]], roots[[2L]], c0
    )
  }
  u_grid <- seq(eps, upper_u, length.out = grid_size)
  profile_grid <- vapply(u_grid, profile_value, numeric(1L))
  local_minimum <- rep(FALSE, grid_size)
  local_minimum[c(1L, grid_size)] <- TRUE
  local_minimum[2L:(grid_size - 1L)] <-
    profile_grid[2L:(grid_size - 1L)] <=
      profile_grid[1L:(grid_size - 2L)] &
    profile_grid[2L:(grid_size - 1L)] <=
      profile_grid[3L:grid_size]
  basin_index <- which(local_minimum)
  basin_index <- unique(c(
    basin_index, which.min(profile_grid), 1L, grid_size
  ))
  basin_fits <- lapply(basin_index, function(index) {
    lo <- u_grid[[max(1L, index - 1L)]]
    hi <- u_grid[[min(grid_size, index + 1L)]]
    if (identical(lo, hi)) {
      list(minimum = lo, objective = profile_value(lo))
    } else {
      fit <- stats::optimize(profile_value, c(lo, hi), tol = tol)
      list(minimum = fit$minimum, objective = fit$objective)
    }
  })
  candidates <- data.frame(
    lower_probability = vapply(
      basin_fits, `[[`, numeric(1L), "minimum"
    ),
    objective = vapply(basin_fits, `[[`, numeric(1L), "objective"),
    stringsAsFactors = FALSE
  )
  candidates <- candidates[
    order(candidates$objective, candidates$lower_probability),
    ,
    drop = FALSE
  ]
  candidates <- candidates[
    !duplicated(round(candidates$lower_probability / tol)),
    ,
    drop = FALSE
  ]
  profile_u <- candidates$lower_probability[[1L]]
  profile_pair <- profile_roots(profile_u)

  start_u <- unique(c(
    profile_u,
    seq(max(eps, 0.05 * (1 - c0)),
        min(upper_u, 0.95 * (1 - c0)), length.out = 7L)
  ))
  starts <- lapply(start_u, function(u) {
    pair <- profile_roots(u)
    c(midpoint = mean(pair), log_width = log(diff(pair)))
  })
  objective_unrestricted <- function(value) {
    width <- exp(value[[2L]])
    .rqr_oracle_moment_risk(
      spec,
      value[[1L]] - width / 2,
      value[[1L]] + width / 2,
      c0
    )
  }
  lower_bound <- spec$q(eps)
  upper_bound <- spec$q(1 - eps)
  fits_2d <- lapply(starts, function(start) {
    stats::optim(
      start, objective_unrestricted, method = "L-BFGS-B",
      lower = c(lower_bound, log(.Machine$double.eps^0.25)),
      upper = c(upper_bound, log(max(1, upper_bound - lower_bound) * 4)),
      control = list(factr = max(1, tol / .Machine$double.eps))
    )
  })
  best_2d <- fits_2d[[which.min(vapply(
    fits_2d, `[[`, numeric(1L), "value"
  ))]]
  unrestricted_pair <- c(
    best_2d$par[[1L]] - exp(best_2d$par[[2L]]) / 2,
    best_2d$par[[1L]] + exp(best_2d$par[[2L]]) / 2
  )
  risk <- rqr_oracle_risk(
    profile_pair[[1L]], profile_pair[[2L]],
    family, c0, params, tol = tol
  )
  h <- max(1e-5, sqrt(tol)) * max(1, 1 - c0)
  u_minus <- max(eps, profile_u - h)
  u_plus <- min(upper_u, profile_u + h)
  curvature <- (
    profile_value(u_plus) - 2 * risk$value + profile_value(u_minus)
  ) / ((0.5 * (u_plus - u_minus))^2)
  coverage_residual <-
    spec$F(profile_pair[[2L]]) - spec$F(profile_pair[[1L]]) - c0
  moment_residual <-
    spec$M(profile_pair[[2L]]) - spec$M(profile_pair[[1L]]) -
      c0 * spec$mean
  objective_tolerance <- max(1e-8, 50 * tol) * max(1, abs(risk$value))
  minimizing <- candidates[
    candidates$objective <= risk$value + objective_tolerance, ,
    drop = FALSE
  ]
  distribution_contract <- list(
    family = family, params = spec$params, mean = spec$mean,
    second_moment = spec$second_moment, support = spec$support
  )
  solver_contract <- list(
    schema_version = "rqrgibbs_rqr_oracle_reference/1.0.0",
    grid_size = grid_size, tolerance = tol,
    profile = "coverage_profile_all_detected_basins",
    unrestricted = "midpoint_log_width_multi_start",
    numerical_error_role = "estimated_not_rigorous_bound"
  )
  list(
    schema_version = solver_contract$schema_version,
    family = family,
    coverage_level = c0,
    lower_root = profile_pair[[1L]],
    upper_root = profile_pair[[2L]],
    lower_probability = profile_u,
    upper_probability = profile_u + c0,
    profile_objective = risk$value,
    unrestricted_lower_root = unrestricted_pair[[1L]],
    unrestricted_upper_root = unrestricted_pair[[2L]],
    unrestricted_objective = best_2d$value,
    global_objective_gap = abs(best_2d$value - risk$value),
    coverage_residual = coverage_residual,
    moment_residual = moment_residual,
    local_profile_curvature = curvature,
    estimated_quadrature_error = risk$estimated_absolute_error,
    quadrature_error_is_rigorous_bound = FALSE,
    endpoint_separation = diff(profile_pair),
    unique_minimizer = nrow(minimizing) == 1L && curvature > 0,
    minimizer_set = minimizing,
    profile_candidates = candidates,
    distribution_digest = digest::digest(
      distribution_contract, algo = "sha256", serialize = TRUE
    ),
    solver_digest = digest::digest(
      solver_contract, algo = "sha256", serialize = TRUE
    ),
    params = spec$params
  )
}

.rqr_oracle_family_spec <- function(family, params) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
  truncated_t_moments <- function(z, df) {
    z <- as.numeric(z)
    probability <- stats::pt(z, df = df)
    first <- numeric(length(z))
    second <- numeric(length(z))
    finite <- is.finite(z)
    if (any(finite)) {
      density <- stats::dt(z[finite], df = df)
      first[finite] <-
        -(df + z[finite]^2) * density / (df - 1)
      second[finite] <-
        df / (df - 2) * probability[finite] -
        z[finite] * (df + z[finite]^2) * density / (df - 2)
    }
    first[is.infinite(z) & z > 0] <- 0
    second[is.infinite(z) & z > 0] <- df / (df - 2)
    list(first = first, second = second)
  }
  numeric_moment <- function(d, lower, upper, power) {
    force(d)
    force(lower)
    function(z) {
      z <- as.numeric(z)
      vapply(z, function(zz) {
        if (!is.finite(zz) && zz < 0) return(0)
        if (zz <= lower) return(0)
        hi <- min(zz, upper)
        stats::integrate(
          function(x) x^power * d(x),
          lower = lower,
          upper = hi,
          rel.tol = 1e-9,
          subdivisions = 300L
        )$value
      }, numeric(1))
    }
  }
  if (family %in% c("gaussian", "normal")) {
    mu <- as.numeric(params$mean %||% 0)[1L]
    sd <- as.numeric(params$sd %||% 1)[1L]
    if (!is.finite(sd) || sd <= 0) stop("gaussian sd must be positive.", call. = FALSE)
    d <- function(x) stats::dnorm(x, mean = mu, sd = sd)
    return(list(
      family = "gaussian",
      d = d,
      F = function(z) stats::pnorm(z, mean = mu, sd = sd),
      q = function(p) stats::qnorm(p, mean = mu, sd = sd),
      M = function(z) mu * stats::pnorm(z, mu, sd) - sd * stats::dnorm(z, mu, sd),
      M2 = function(z) {
        k <- (z - mu) / sd
        (mu^2 + sd^2) * stats::pnorm(k) -
          sd * (2 * mu + sd * k) * stats::dnorm(k)
      },
      mean = mu,
      second_moment = mu^2 + sd^2,
      support = c(-Inf, Inf),
      params = list(mean = mu, sd = sd)
    ))
  }
  if (family == "student_t") {
    df <- as.numeric(params$df %||% 5)[1L]
    scale <- as.numeric(params$scale %||% 1)[1L]
    if (!is.finite(df) || df <= 2) {
      stop("student_t df must exceed 2 for finite RQR risk.", call. = FALSE)
    }
    if (!is.finite(scale) || scale <= 0) stop("student_t scale must be positive.", call. = FALSE)
    d <- function(x) stats::dt(x / scale, df = df) / scale
    moments <- function(z) truncated_t_moments(z / scale, df)
    return(list(
      family = "student_t", d = d,
      F = function(z) stats::pt(z / scale, df = df),
      q = function(p) scale * stats::qt(p, df = df),
      M = function(z) scale * moments(z)$first,
      M2 = function(z) scale^2 * moments(z)$second,
      mean = 0,
      second_moment = scale^2 * df / (df - 2),
      support = c(-Inf, Inf),
      params = list(df = df, scale = scale)
    ))
  }
  if (family == "laplace") {
    loc <- as.numeric(params$location %||% 0)[1L]
    scale <- as.numeric(params$scale %||% 1)[1L]
    if (!is.finite(scale) || scale <= 0) stop("laplace scale must be positive.", call. = FALSE)
    F <- function(x) ifelse(x < loc, 0.5 * exp((x - loc) / scale), 1 - 0.5 * exp(-(x - loc) / scale))
    q <- function(p) ifelse(p < 0.5, loc + scale * log(2 * p), loc - scale * log(2 * (1 - p)))
    d <- function(x) 0.5 / scale * exp(-abs(x - loc) / scale)
    return(list(
      family = "laplace", d = d, F = F, q = q,
      M = numeric_moment(d, -Inf, Inf, 1),
      M2 = numeric_moment(d, -Inf, Inf, 2),
      mean = loc, second_moment = loc^2 + 2 * scale^2,
      support = c(-Inf, Inf),
      params = list(location = loc, scale = scale)
    ))
  }
  if (family %in% c("centered_gamma", "centered_exponential")) {
    shape <- as.numeric(params$shape %||% if (family == "centered_exponential") 1 else 2)[1L]
    scale <- as.numeric(params$scale %||% 1)[1L]
    center <- shape * scale
    if (!is.finite(shape) || shape <= 0 || !is.finite(scale) || scale <= 0) {
      stop("centered_gamma shape and scale must be positive.", call. = FALSE)
    }
    F <- function(x) stats::pgamma(x + center, shape = shape, scale = scale)
    q <- function(p) stats::qgamma(p, shape = shape, scale = scale) - center
    d <- function(x) stats::dgamma(x + center, shape = shape, scale = scale)
    return(list(
      family = "centered_gamma", d = d, F = F, q = q,
      M = numeric_moment(d, -center, Inf, 1),
      M2 = numeric_moment(d, -center, Inf, 2),
      mean = 0, second_moment = shape * scale^2,
      support = c(-center, Inf),
      params = list(shape = shape, scale = scale, center = center)
    ))
  }
  if (family %in% c("beta", "standardized_beta", "centered_standardized_beta")) {
    shape1 <- as.numeric(params$shape1 %||% params$a %||% 2)[1L]
    shape2 <- as.numeric(params$shape2 %||% params$b %||% 5)[1L]
    if (!is.finite(shape1) || shape1 <= 0 ||
        !is.finite(shape2) || shape2 <= 0) {
      stop("beta shape parameters must be positive.", call. = FALSE)
    }
    raw_mean <- shape1 / (shape1 + shape2)
    raw_var <- shape1 * shape2 /
      ((shape1 + shape2)^2 * (shape1 + shape2 + 1))
    standardize <- family %in% c(
      "standardized_beta", "centered_standardized_beta"
    ) || isTRUE(params$variance_standardized %||% FALSE)
    center <- if (isTRUE(params$center %||% standardize)) raw_mean else 0
    scale <- if (standardize) sqrt(raw_var) else 1
    if (!is.finite(scale) || scale <= 0) {
      stop("standardized beta scale must be positive.", call. = FALSE)
    }
    support <- c((0 - center) / scale, (1 - center) / scale)
    d <- function(x) {
      raw <- scale * as.numeric(x) + center
      scale * stats::dbeta(raw, shape1 = shape1, shape2 = shape2)
    }
    F <- function(x) {
      raw <- scale * as.numeric(x) + center
      stats::pbeta(raw, shape1 = shape1, shape2 = shape2)
    }
    q <- function(p) {
      (stats::qbeta(p, shape1 = shape1, shape2 = shape2) - center) /
        scale
    }
    return(list(
      family = if (standardize) "standardized_beta" else "beta",
      d = d, F = F, q = q,
      M = numeric_moment(d, support[[1L]], support[[2L]], 1),
      M2 = numeric_moment(d, support[[1L]], support[[2L]], 2),
      mean = (raw_mean - center) / scale,
      second_moment = (raw_var + (raw_mean - center)^2) / scale^2,
      support = support,
      params = list(
        shape1 = shape1, shape2 = shape2, center = center, scale = scale,
        variance_standardized = standardize
      )
    ))
  }
  if (family == "asymmetric_laplace") {
    tau <- as.numeric(params$tau %||% params$p %||% 0.25)[1L]
    raw_scale <- as.numeric(params$scale %||% 1)[1L]
    variance_standardized <- isTRUE(
      params$variance_standardized %||% FALSE
    )
    if (!is.finite(tau) || tau <= 0 || tau >= 1 ||
        !is.finite(raw_scale) || raw_scale <= 0) {
      stop("asymmetric_laplace tau must be in (0,1) and scale must be positive.", call. = FALSE)
    }
    raw_mean <- raw_scale * (1 - 2 * tau) / (tau * (1 - tau))
    raw_variance <- raw_scale^2 *
      (1 - 2 * tau + 2 * tau^2) / (tau^2 * (1 - tau)^2)
    raw_second_moment <- raw_variance + raw_mean^2
    normalization_scale <- if (variance_standardized) {
      sqrt(raw_variance)
    } else {
      1
    }
    rho <- function(z) z * (tau - as.numeric(z < 0))
    d_raw <- function(z) {
      tau * (1 - tau) / raw_scale * exp(-rho(z / raw_scale))
    }
    F_raw <- function(z) {
      ifelse(
        z < 0,
        tau * exp((1 - tau) * z / raw_scale),
        1 - (1 - tau) * exp(-tau * z / raw_scale)
      )
    }
    q_raw <- function(p) {
      ifelse(
        p < tau,
        raw_scale * log(p / tau) / (1 - tau),
        -raw_scale * log((1 - p) / (1 - tau)) / tau
      )
    }
    M_raw <- function(z) {
      z <- as.numeric(z)
      probability <- F_raw(z)
      out <- ifelse(
        z < 0,
        probability * (z - raw_scale / (1 - tau)),
        raw_mean - (1 - tau) * exp(-tau * z / raw_scale) *
          (z + raw_scale / tau)
      )
      out[is.infinite(z) & z < 0] <- 0
      out[is.infinite(z) & z > 0] <- raw_mean
      out
    }
    M2_raw <- function(z) {
      z <- as.numeric(z)
      probability <- F_raw(z)
      out <- ifelse(
        z < 0,
        probability * (
          z^2 - 2 * raw_scale * z / (1 - tau) +
            2 * raw_scale^2 / (1 - tau)^2
        ),
        raw_second_moment -
          (1 - tau) * exp(-tau * z / raw_scale) * (
            z^2 + 2 * raw_scale * z / tau +
              2 * raw_scale^2 / tau^2
          )
      )
      out[is.infinite(z) & z < 0] <- 0
      out[is.infinite(z) & z > 0] <- raw_second_moment
      out
    }
    to_raw <- function(x) raw_mean + normalization_scale * as.numeric(x)
    F <- function(x) F_raw(to_raw(x))
    d <- function(x) normalization_scale * d_raw(to_raw(x))
    q <- function(p) (q_raw(p) - raw_mean) / normalization_scale
    M <- function(x) {
      raw_x <- to_raw(x)
      (M_raw(raw_x) - raw_mean * F_raw(raw_x)) /
        normalization_scale
    }
    M2 <- function(x) {
      raw_x <- to_raw(x)
      (
        M2_raw(raw_x) - 2 * raw_mean * M_raw(raw_x) +
          raw_mean^2 * F_raw(raw_x)
      ) / normalization_scale^2
    }
    return(list(
      family = "asymmetric_laplace", d = d,
      F = F, q = q, M = M, M2 = M2,
      mean = 0,
      second_moment = raw_variance / normalization_scale^2,
      support = c(-Inf, Inf),
      kinks = -raw_mean / normalization_scale,
      params = list(
        tau = tau,
        scale = raw_scale,
        center = raw_mean,
        raw_mean = raw_mean,
        raw_sd = sqrt(raw_variance),
        normalization_scale = normalization_scale,
        variance_standardized = variance_standardized
      )
    ))
  }
  if (family == "gaussian_mixture") {
    weights <- as.numeric(params$weights %||% c(0.1, 0.9))
    means <- as.numeric(params$means %||% c(0, 1))
    sds <- as.numeric(params$sds %||% c(0.5, 1.5))
    if (length(weights) != length(means) || length(weights) != length(sds)) {
      stop("gaussian_mixture weights, means, and sds must have equal lengths.", call. = FALSE)
    }
    weights <- weights / sum(weights)
    if (any(!is.finite(weights)) || any(weights < 0) || any(!is.finite(sds)) || any(sds <= 0)) {
      stop("gaussian_mixture has invalid weights or standard deviations.", call. = FALSE)
    }
    raw_mean <- sum(weights * means)
    center <- if (isTRUE(params$center %||% TRUE)) raw_mean else 0
    F <- function(x) {
      x <- as.numeric(x)
      mat <- sapply(seq_along(weights), function(ii) weights[ii] * stats::pnorm(x + center, means[ii], sds[ii]))
      if (is.null(dim(mat))) mat <- matrix(mat, nrow = length(x))
      rowSums(mat)
    }
    d <- function(x) {
      x <- as.numeric(x)
      mat <- sapply(seq_along(weights), function(ii) weights[ii] * stats::dnorm(x + center, means[ii], sds[ii]))
      if (is.null(dim(mat))) mat <- matrix(mat, nrow = length(x))
      rowSums(mat)
    }
    q <- function(p) vapply(p, function(pp) {
      stats::uniroot(
        function(z) F(z) - pp,
        c(
          min(means - center - 12 * sds),
          max(means - center + 12 * sds)
        ),
        tol = .Machine$double.eps^0.75
      )$root
    }, numeric(1))
    second_moment <- sum(
      weights * (sds^2 + (means - center)^2)
    )
    return(list(
      family = "gaussian_mixture", d = d,
      F = F,
      q = q,
      M = numeric_moment(d, -Inf, Inf, 1),
      M2 = numeric_moment(d, -Inf, Inf, 2),
      mean = raw_mean - center,
      second_moment = second_moment,
      support = c(-Inf, Inf),
      params = list(weights = weights, means = means, sds = sds, center = center)
    ))
  }
  if (family %in% c(
        "centered_lognormal", "centered_standardized_lognormal"
      )) {
    logmean <- as.numeric(params$logmean %||% 0)[1L]
    logsd <- as.numeric(params$logsd %||% 0.75)[1L]
    if (!is.finite(logmean) || !is.finite(logsd) || logsd <= 0) {
      stop("log-normal parameters must be finite with positive logsd.",
           call. = FALSE)
    }
    raw_mean <- exp(logmean + 0.5 * logsd^2)
    raw_var <- (exp(logsd^2) - 1) *
      exp(2 * logmean + logsd^2)
    standardize <- identical(family, "centered_standardized_lognormal") ||
      isTRUE(params$variance_standardized %||% FALSE)
    scale <- if (standardize) sqrt(raw_var) else 1
    support <- c(-raw_mean / scale, Inf)
    d <- function(x) {
      stats::dlnorm(scale * x + raw_mean, logmean, logsd) * scale
    }
    F <- function(x) {
      stats::plnorm(scale * x + raw_mean, logmean, logsd)
    }
    q <- function(p) {
      (stats::qlnorm(p, logmean, logsd) - raw_mean) / scale
    }
    raw_truncated <- function(x, power) {
      y <- scale * x + raw_mean
      ifelse(
        y <= 0,
        0,
        exp(power * logmean + 0.5 * power^2 * logsd^2) *
          stats::pnorm((log(y) - logmean - power * logsd^2) / logsd)
      )
    }
    M <- function(x) {
      (raw_truncated(x, 1) - raw_mean * F(x)) / scale
    }
    M2 <- function(x) {
      (
        raw_truncated(x, 2) -
          2 * raw_mean * raw_truncated(x, 1) +
          raw_mean^2 * F(x)
      ) / scale^2
    }
    return(list(
      family = "centered_standardized_lognormal",
      d = d, F = F, q = q,
      M = M, M2 = M2,
      mean = 0,
      second_moment = raw_var / scale^2,
      support = support,
      params = list(
        logmean = logmean, logsd = logsd, center = raw_mean,
        scale = scale, variance_standardized = standardize
      )
    ))
  }
  if (family %in% c(
        "normal_t_mixture", "standardized_skewed_normal_t_mixture"
      )) {
    normal_weight <- as.numeric(params$normal_weight %||% 0.90)[1L]
    t_weight <- as.numeric(params$t_weight %||% (1 - normal_weight))[1L]
    t_df <- as.numeric(params$t_df %||% 3)[1L]
    t_shift <- as.numeric(params$t_shift %||% 2)[1L]
    t_scale <- as.numeric(params$t_scale %||% 1)[1L]
    weights <- c(normal_weight, t_weight)
    if (any(!is.finite(weights)) || any(weights < 0) ||
        abs(sum(weights) - 1) > 1e-12 ||
        !is.finite(t_df) || t_df <= 2 ||
        !is.finite(t_shift) || !is.finite(t_scale) || t_scale <= 0) {
      stop("normal_t_mixture parameters are invalid.", call. = FALSE)
    }
    raw_mean <- t_weight * t_shift
    raw_second <- normal_weight +
      t_weight * (t_shift^2 + t_scale^2 * t_df / (t_df - 2))
    raw_sd <- sqrt(raw_second - raw_mean^2)
    standardize <- isTRUE(params$variance_standardized %||% TRUE)
    center <- raw_mean
    scale <- if (standardize) raw_sd else 1
    F_raw <- function(z) {
      normal_weight * stats::pnorm(z) +
        t_weight * stats::pt((z - t_shift) / t_scale, df = t_df)
    }
    d_raw <- function(z) {
      normal_weight * stats::dnorm(z) +
        t_weight * stats::dt((z - t_shift) / t_scale, df = t_df) /
          t_scale
    }
    M_raw <- function(z) {
      standardized <- (z - t_shift) / t_scale
      t_moments <- truncated_t_moments(standardized, t_df)
      normal_weight * -stats::dnorm(z) +
        t_weight * (
          t_shift * stats::pt(standardized, df = t_df) +
          t_scale * t_moments$first
        )
    }
    M2_raw <- function(z) {
      standardized <- (z - t_shift) / t_scale
      t_probability <- stats::pt(standardized, df = t_df)
      t_moments <- truncated_t_moments(standardized, t_df)
      normal_second <- stats::pnorm(z)
      finite <- is.finite(z)
      normal_second[finite] <-
        normal_second[finite] -
        z[finite] * stats::dnorm(z[finite])
      normal_weight * normal_second +
        t_weight * (
          t_shift^2 * t_probability +
          2 * t_shift * t_scale * t_moments$first +
          t_scale^2 * t_moments$second
        )
    }
    F <- function(x) F_raw(scale * x + center)
    d <- function(x) d_raw(scale * x + center) * scale
    q <- function(p) vapply(p, function(probability) {
      lo <- (-12 - center) / scale
      hi <- (t_shift + 12 * t_scale - center) / scale
      while (F(lo) > probability) lo <- lo * 2
      while (F(hi) < probability) hi <- hi * 2
      stats::uniroot(
        function(z) F(z) - probability,
        c(lo, hi),
        tol = .Machine$double.eps^0.75
      )$root
    }, numeric(1L))
    return(list(
      family = "normal_t_mixture",
      d = d, F = F, q = q,
      M = function(x) {
        raw_x <- scale * x + center
        (M_raw(raw_x) - center * F_raw(raw_x)) / scale
      },
      M2 = function(x) {
        raw_x <- scale * x + center
        (
          M2_raw(raw_x) -
            2 * center * M_raw(raw_x) +
            center^2 * F_raw(raw_x)
        ) / scale^2
      },
      mean = 0,
      second_moment = (raw_second - raw_mean^2) / scale^2,
      support = c(-Inf, Inf),
      params = list(
        normal_weight = normal_weight, t_weight = t_weight,
        t_df = t_df, t_shift = t_shift, t_scale = t_scale,
        center = center, scale = scale,
        variance_standardized = standardize
      )
    ))
  }
  stop(sprintf("Unsupported RQR oracle family: %s", family), call. = FALSE)
}
