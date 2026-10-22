.rqr_mti_scalar <- function(x, name, positive = FALSE) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) ||
      (positive && x <= 0)) stop(name, " must be one finite ",
        if (positive) "positive " else "", "number.", call. = FALSE)
  as.numeric(x)
}

.rqr_mti_mass_sum <- function(y, mass) {
  k <- floor(mass)
  r <- mass - k
  value <- if (k > 0) sum(utils::head(y, k)) else 0
  if (r > 0) value <- value + r * y[k + 1L]
  value
}

#' Fractional empirical retained-mean bounds
#' @param y Finite univariate observations.
#' @param target_content Fitted content in (0,1).
#' @return A list of sharp fractional retained-mean and tilt bounds.
#' @export
rqr_mti_empirical_tilt_bounds <- function(y, target_content) {
  y <- .rqr_bayes_clean_y(y, na_rm = FALSE)
  q <- .rqr_bayes_assert_probability(.rqr_mti_scalar(target_content,
    "target_content"), "target_content")
  ys <- sort(y)
  mass <- length(y) * q
  lower <- .rqr_mti_mass_sum(ys, mass) / mass
  upper <- .rqr_mti_mass_sum(rev(ys), mass) / mass
  list(sample_size = length(y), target_content = q, fractional_mass = mass,
    training_mean = mean(y), retained_mean_lower = lower,
    retained_mean_upper = upper, delta_lower = lower - mean(y),
    delta_upper = upper - mean(y))
}

#' Mean tilt from one fractional shortest empirical support
#' @param y Finite univariate observations.
#' @param target_content Fitted content in (0,1).
#' @param allocation How fractional endpoint mass is allocated.
#' @return Support endpoints, exact fractional weights, retained mean and tilt.
#' @export
rqr_mti_fractional_window <- function(
    y, target_content, allocation = c("balanced", "left", "right")) {
  y <- .rqr_bayes_clean_y(y, na_rm = FALSE)
  q <- .rqr_bayes_assert_probability(.rqr_mti_scalar(target_content,
    "target_content"), "target_content")
  allocation <- match.arg(allocation)
  ys <- sort(y)
  n <- length(y)
  mass <- n * q
  nearest <- round(mass)
  if (nearest >= 1 && abs(mass - nearest) <=
      100 * .Machine$double.eps * max(1, mass)) {
    mass <- nearest
  }
  k <- floor(mass)
  r <- mass - k
  count <- as.integer(k + (r > 0))
  count <- max(1L, count)
  starts <- seq_len(n - count + 1L)
  widths <- ys[starts + count - 1L] - ys[starts]
  minimum <- min(widths)
  tie_tol <- 100 * .Machine$double.eps * max(1, abs(minimum))
  j <- which(abs(widths - minimum) <= tie_tol)[1L]
  idx <- seq.int(j, j + count - 1L)
  weights <- numeric(n)
  weights[idx] <- 1
  if (count == 1L) weights[idx] <- mass
  else if (r > 0) {
    weights[idx[c(1L, count)]] <- switch(allocation,
      balanced = rep((1 + r) / 2, 2L), left = c(1, r), right = c(r, 1))
  }
  retained <- sum(weights * ys) / mass
  bounds <- rqr_mti_empirical_tilt_bounds(y, q)
  list(schema_version = "rqrgibbs_fractional_support/1.0.0",
    rule = paste0("shortest_support_", allocation, "_endpoint_mass"),
    sample_size = n, target_content = q, fractional_mass = mass,
    lower = ys[idx[1L]], upper = ys[utils::tail(idx, 1L)],
    width = widths[j], start = j, end = utils::tail(idx, 1L),
    y_sorted = ys, membership_weights = weights,
    retained_mean = retained, training_mean = mean(y),
    delta_raw = retained - mean(y), delta_lower = bounds$delta_lower,
    delta_upper = bounds$delta_upper,
    closed_count = sum(ys >= ys[idx[1L]] & ys <= ys[utils::tail(idx, 1L)]))
}

.rqr_mti_profile_slice <- function(y, m, mass, q, delta, omega, precision, center) {
  distance <- abs(y - m)
  ordered <- order(distance, y)
  k <- floor(mass)
  fraction <- mass - k
  rank <- if (fraction > 0) k + 1L else max(1L, k)
  radius <- distance[ordered[rank]]
  error <- 32 * .Machine$double.eps * max(1, abs(m), abs(y))
  difference <- distance - radius
  inside <- difference < -error
  boundary <- which(abs(difference) <= error)
  remainder <- mass - sum(inside)
  fill <- function(indices) {
    w <- as.numeric(inside)
    if (length(indices)) {
      take <- pmin(1, pmax(0, remainder - seq_along(indices) + 1))
      w[indices] <- take
    }
    w
  }
  low <- fill(boundary[order(y[boundary])])
  high <- fill(boundary[order(y[boundary], decreasing = TRUE)])
  offset <- 4 * precision * m - 2 * precision * center -
    2 * omega * (q * sum(y) + length(y) * q * delta)
  glo <- offset + 2 * omega * sum(low * y)
  ghi <- offset + 2 * omega * sum(high * y)
  if (ghi < glo) { tmp <- glo; glo <- ghi; ghi <- tmp }
  mix <- if (ghi > glo) min(1, max(0, -glo / (ghi - glo))) else 0
  weights <- (1 - mix) * low + mix * high
  list(radius = radius, boundary = boundary, weights = weights,
       gradient_lower = glo, gradient_upper = ghi)
}

.rqr_mti_profile_core_r <- function(y, q, delta, omega, precision, center,
                                    max_iter, parameter_tolerance, store_trace) {
  mass <- length(y) * q - precision / omega
  if (mass <= 0) {
    m <- (omega * q * (sum(y) + length(y) * delta) + precision * center) /
      (omega * length(y) * q + precision)
    return(list(midpoint = m, iterations = 0L, trace = data.frame()))
  }
  ys <- sort(y)
  offset <- omega * (q * sum(y) + length(y) * q * delta) + precision * center
  lo <- (offset - omega * .rqr_mti_mass_sum(rev(ys), mass)) / (2 * precision)
  hi <- (offset - omega * .rqr_mti_mass_sum(ys, mass)) / (2 * precision)
  if (!is.finite(lo) || !is.finite(hi)) {
    stop("The fixed-target midpoint bracket is not finite.", call. = FALSE)
  }
  records <- list()
  m <- lo + (hi - lo) / 2
  for (iteration in seq_len(max_iter)) {
    m <- lo + (hi - lo) / 2
    z <- .rqr_mti_profile_slice(y, m, mass, q, delta, omega, precision, center)
    if (store_trace) records[[iteration]] <- data.frame(iteration = iteration,
      midpoint = m, bracket_lower = lo, bracket_upper = hi,
      gradient_lower = z$gradient_lower, gradient_upper = z$gradient_upper)
    if (z$gradient_lower <= 0 && z$gradient_upper >= 0) break
    if (hi - lo <= parameter_tolerance * max(1, abs(m), abs(y))) break
    if (z$gradient_upper < 0) lo <- m else hi <- m
  }
  list(midpoint = m, iterations = iteration,
    trace = if (length(records)) do.call(rbind, records) else data.frame())
}

.rqr_mti_intercept_objective <- function(y, lower, upper, q, delta, omega,
                                       precision, center) {
  omega * sum(rqr_mean_tilt_loss(y, rep(lower, length(y)),
    rep(upper, length(y)), q, delta)) +
    precision * ((lower - center)^2 + (upper - center)^2) / 2
}

#' Profiled intercept-only MTI endpoint estimates
#'
#' Minimizes the fixed-target Gaussian-penalized MTI criterion through its
#' convex midpoint profile. This is a loss-based generalized-Bayes endpoint
#' calculation; it supplies no response prediction or tolerance guarantee.
#' @param y Finite univariate observations.
#' @param target_content Fixed fitted content in (0,1).
#' @param mean_tilt Fixed scalar response-scale tilt.
#' @param learning_rate Positive fixed loss learning rate.
#' @param prior_precision Common positive Gaussian root precision.
#' @param prior_mean Common fixed Gaussian root mean.
#' @param backend Numerical implementation.
#' @param control List with max_iter, parameter_tolerance, objective_tolerance,
#'   store_trace, and fail_on_nonconvergence.
#' @return Endpoint estimates, objective and global lower bound, numerical
#'   error assessment, and optional midpoint traces.
#' @export
rqr_mti_intercept_fit <- function(y, target_content, mean_tilt = 0,
    learning_rate = 1, prior_precision = 1e-4, prior_mean = 0,
    backend = c("cpp", "R"), control = list()) {
  y <- .rqr_bayes_clean_y(y, na_rm = FALSE)
  if (length(y) < 2L) stop("At least two observations are required.", call. = FALSE)
  q <- .rqr_bayes_assert_probability(.rqr_mti_scalar(target_content,
    "target_content"), "target_content")
  delta <- .rqr_mti_scalar(mean_tilt, "mean_tilt")
  omega <- .rqr_mti_scalar(learning_rate, "learning_rate", TRUE)
  precision <- .rqr_mti_scalar(prior_precision, "prior_precision", TRUE)
  center <- .rqr_mti_scalar(prior_mean, "prior_mean")
  backend <- match.arg(backend)
  if (!is.list(control)) stop("control must be a list.", call. = FALSE)
  max_iter <- .rqr_scalar_integer(control$max_iter %||% 200L, "max_iter", 1L)
  xtol <- .rqr_mti_scalar(control$parameter_tolerance %||% 1e-15,
    "parameter_tolerance", TRUE)
  ftol <- .rqr_mti_scalar(control$objective_tolerance %||% 1e-8,
    "objective_tolerance", TRUE)
  store <- isTRUE(control$store_trace)
  core <- if (backend == "cpp") .rqr_mti_profile_intercept_cpp(
    y, q, delta, omega, precision, center, max_iter, xtol, store) else
    .rqr_mti_profile_core_r(y, q, delta, omega, precision, center, max_iter, xtol, store)
  mass <- length(y) * q - precision / omega
  m <- core$midpoint
  if (mass <= 0) {
    lower <- upper <- m
    weights <- rep(0, length(y))
    lower_bound <- objective <- .rqr_mti_intercept_objective(
      y, lower, upper, q, delta, omega, precision, center)
  } else {
    z <- .rqr_mti_profile_slice(y, m, mass, q, delta, omega, precision, center)
    boundary <- y[z$boundary]
    left <- boundary[boundary <= m]
    right <- boundary[boundary >= m]
    if (length(left) && length(right)) {
      lower <- max(left)
      upper <- min(right)
      m <- (lower + upper) / 2
    } else {
      knot <- boundary[1L]
      if (knot >= m) { upper <- knot; lower <- 2 * m - knot }
      else { lower <- knot; upper <- 2 * m - knot }
    }
    z <- .rqr_mti_profile_slice(y, m, mass, q, delta, omega, precision, center)
    weights <- z$weights
    objective <- .rqr_mti_intercept_objective(y, lower, upper, q, delta,
      omega, precision, center)
    centered <- y - center
    constant <- omega * (q * sum(centered^2) - sum(weights * centered^2) +
      2 * q * delta * sum(centered))
    linear <- 2 * omega * (sum(weights * centered) - q * sum(centered) -
      length(y) * q * delta)
    lower_bound <- constant - linear^2 / (8 * precision)
  }
  error <- objective - lower_bound
  converged <- is.finite(error) && abs(error) <= ftol * (1 + abs(objective))
  if (!converged && isTRUE(control$fail_on_nonconvergence %||% TRUE)) {
    stop("MTI profile objective accuracy was not attained: gap=", format(error),
      ". No fallback interval was substituted.", call. = FALSE)
  }
  if (store && nrow(core$trace)) {
    quantities <- t(vapply(core$trace$midpoint, function(mid) {
      radius <- if (mass <= 0) 0 else .rqr_mti_profile_slice(y, mid, mass,
        q, delta, omega, precision, center)$radius
      lo <- mid - radius
      hi <- mid + radius
      c(lower = lo, upper = hi, width = hi - lo,
        objective = .rqr_mti_intercept_objective(y, lo, hi, q, delta,
          omega, precision, center))
    }, numeric(4L)))
    core$trace <- cbind(core$trace, as.data.frame(quantities))
  }
  out <- list(schema_version = "rqrgibbs_mti_intercept_profile/1.0.0",
    lower = lower, upper = upper, width = upper - lower, midpoint = m,
    target_content = q, mean_tilt = delta, learning_rate = omega,
    prior_precision = precision, prior_mean = center, effective_mass = mass,
    observed_count = sum(y >= lower & y <= upper), objective = objective,
    objective_lower_bound = lower_bound, objective_gap = error,
    relative_objective_gap = error / (1 + abs(objective)), converged = converged,
    iterations = core$iterations, backend = backend, fractional_weights = weights,
    trace = core$trace, generalized_bayes = TRUE, response_likelihood = FALSE,
    tolerance_claim_available = FALSE)
  class(out) <- c("rqr_mti_intercept_fit", "list")
  out
}
