#' RQR oracle roots for centered innovation laws
#'
#' The RQR interval roots `(a_c, b_c)` are defined by the coverage and
#' first-moment balance equations
#' `F(b_c) - F(a_c) = c` and
#' `M(b_c) - M(a_c) = c * E(E)`, where
#' `M(z) = E[E 1(E <= z)]`.  The helper is intended for simulation design and
#' endpoint-recovery audits; it is not used as a fitted model.
#'
#' @param family Innovation family. Supported values are `"gaussian"`,
#'   `"laplace"`, `"student_t"`, `"centered_gamma"`,
#'   `"asymmetric_laplace"`, and `"gaussian_mixture"`.
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
  if (family == "asymmetric_laplace") {
    tau <- as.numeric(params$tau %||% params$p %||% 0.25)[1L]
    scale <- as.numeric(params$scale %||% 1)[1L]
    if (!is.finite(tau) || tau <= 0 || tau >= 1 || !is.finite(scale) || scale <= 0) {
      stop("asymmetric_laplace tau must be in (0,1) and scale must be positive.", call. = FALSE)
    }
    raw_mean <- scale * (1 - 2 * tau) / (tau * (1 - tau))
    rho <- function(z) z * (tau - as.numeric(z < 0))
    d_raw <- function(z) tau * (1 - tau) / scale * exp(-rho(z / scale))
    F_raw <- function(z) ifelse(z < 0, tau * exp((1 - tau) * z / scale), 1 - (1 - tau) * exp(-tau * z / scale))
    q_raw <- function(p) ifelse(p < tau, scale * log(p / tau) / (1 - tau), -scale * log((1 - p) / (1 - tau)) / tau)
    d <- function(x) d_raw(x + raw_mean)
    return(list(
      family = "asymmetric_laplace", d = d,
      F = function(z) F_raw(z + raw_mean),
      q = function(p) q_raw(p) - raw_mean,
      M = numeric_moment(d, -Inf, Inf, 1),
      M2 = numeric_moment(d, -Inf, Inf, 2),
      mean = 0,
      second_moment = scale^2 *
        (1 - 2 * tau + 2 * tau^2) / (tau^2 * (1 - tau)^2),
      support = c(-Inf, Inf),
      params = list(tau = tau, scale = scale, center = raw_mean)
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
