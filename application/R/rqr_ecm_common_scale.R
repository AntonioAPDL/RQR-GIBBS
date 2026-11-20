.rqr_cs_spec <- function(x) {
  if (!is.list(x) || !((x$prior %||% "log_uniform") %in% c("log_uniform", "gamma_rate"))) {
    stop("Common-scale ECM requires a proper bounded log_uniform or gamma_rate prior.", call. = FALSE)
  }
  prior <- x$prior %||% "log_uniform"
  allowed <- c("prior", "shape", "rate", "log_rate_bounds", "update", "polish_cycles", "multistart_rates")
  if (length(setdiff(names(x), allowed))) stop("Unknown scale_control argument.", call. = FALSE)
  shape <- rate <- 0
  if (prior == "gamma_rate") {
    shape <- x$shape; rate <- x$rate
    if (length(shape) != 1L || length(rate) != 1L || !is.numeric(shape) || !is.numeric(rate) ||
        !is.finite(shape) || !is.finite(rate) || shape <= 0 || rate <= 0) {
      stop("gamma_rate requires finite positive shape and rate.", call. = FALSE)
    }
  } else if (!is.null(x$shape) || !is.null(x$rate)) {
    stop("shape and rate apply only to gamma_rate.", call. = FALSE)
  }
  bounds <- as.numeric(x$log_rate_bounds)
  valid <- length(bounds) == 2L && !anyNA(bounds) && bounds[1] < 0 && bounds[2] > 0
  if (valid) {
    valid <- (is.infinite(bounds[1]) || exp(bounds[1]) > 0) &&
      (is.infinite(bounds[2]) || is.finite(exp(bounds[2])))
  }
  if (!valid || (prior == "log_uniform" && any(!is.finite(bounds)))) {
    stop("log_rate_bounds must enclose zero; log_uniform requires finite bounds.", call. = FALSE)
  }
  list(prior = prior, shape = shape, rate = rate, log_rate_bounds = bounds,
    update = match.arg(x$update %||% "ecm", c("ecm", "ecme")),
    polish_cycles = .rqr_scalar_integer(x$polish_cycles %||% 100L, "polish_cycles", 1L),
    multistart_rates = isTRUE(x$multistart_rates %||% TRUE),
    mode_parameterization = "roots_and_log_inverse_loss_scale",
    marginal_scale_power = "n", augmented_scale_power = "3n/2")
}

.rqr_cs_loss <- function(y, roots, q, delta) {
  e <- (y - roots[1]) * (y - roots[2])
  sum(e * (q - (e < 0)) - q * delta * (sum(roots) - 2 * y))
}

.rqr_cs_objective <- function(y, roots, q, delta, eta, precision, spec = list()) {
  exp(eta) * (.rqr_cs_loss(y, roots, q, delta) + (spec$rate %||% 0)) +
    precision * sum(roots^2) / 2 - (length(y) + (spec$shape %||% 0)) * eta
}

.rqr_cs_scale_mode <- function(coefficient, power, bounds) {
  if (coefficient <= 0) {
    if (!is.finite(bounds[2])) stop("Nonpositive unrestricted Gamma rate.", call. = FALSE)
    return(bounds[2])
  }
  max(bounds[1], min(bounds[2], log(power) - log(coefficient)))
}

.rqr_cs_tilt_domain <- function(y, q, delta) {
  n <- length(y); ys <- sort(y); mass <- n*q
  weights <- pmin(1, pmax(0, mass - seq_along(ys) + 1))
  average <- mean(y)
  positive <- n*((1-q)*(average-min(y))-q*delta)
  negative <- n*((1-q)*(max(y)-average)+q*delta)
  list(positive_ray_slope = positive, negative_ray_slope = negative,
    unbounded_loss_ray = positive < 0 || negative < 0,
    retained_tilt_lower = sum(weights*ys)/mass-average,
    retained_tilt_upper = sum(weights*rev(ys))/mass-average,
    unrestricted_gamma_proved = delta == 0)
}

.rqr_cs_root_mode <- function(y, other, q, delta, omega, precision) {
  ys <- sort(y)
  difference <- ys - other
  mass <- c(0, cumsum(abs(difference)))
  slope <- -q * sum(difference) + sum(pmin(difference, 0)) - length(y) * q * delta
  after <- precision * ys + omega * (slope + mass[-1L])
  crossing <- which(after >= 0)[1L]
  if (is.na(crossing)) return(-omega * (slope + utils::tail(mass, 1L)) / precision)
  before <- precision * ys[crossing] + omega * (slope + mass[crossing])
  if (before <= 0) ys[crossing] else -omega * (slope + mass[crossing]) / precision
}

.rqr_cs_root_increment <- function(y, roots, coordinate, proposed, q, delta, omega, precision) {
  old <- roots[coordinate]; other <- roots[3L - coordinate]; step <- proposed - old
  e0 <- (y - old) * (y - other); e1 <- (y - proposed) * (y - other)
  same <- (e0 < 0) == (e1 < 0)
  change <- e1 * (q - (e1 < 0)) - e0 * (q - (e0 < 0))
  change[same] <- -step * (y[same] - other) * (q - (e0[same] < 0))
  omega * (sum(change) - length(y) * q * delta * step) +
    precision * step * (old + proposed) / 2
}

.rqr_cs_accuracy <- function(y, roots, q, delta, eta, precision, spec) {
  omega <- exp(eta)
  modes <- vapply(1:2, function(j) .rqr_cs_root_mode(y, roots[3L-j], q, delta, omega, precision), 0)
  improvements <- vapply(1:2, function(j) max(0, -.rqr_cs_root_increment(
    y, roots, j, modes[j], q, delta, omega, precision)), 0)
  loss <- .rqr_cs_loss(y, roots, q, delta)
  coefficient <- loss + (spec$rate %||% 0)
  power <- length(y) + (spec$shape %||% 0)
  best_eta <- .rqr_cs_scale_mode(coefficient, power, spec$log_rate_bounds)
  scale_change <- omega * expm1(best_eta - eta) * coefficient - power * (best_eta - eta)
  list(coordinate_gap = max(improvements), endpoint_gap = max(abs(modes - roots)),
    scale_gap = max(0, -scale_change), scale_displacement = abs(best_eta - eta),
    at_lower_bound = eta == spec$log_rate_bounds[1], at_upper_bound = eta == spec$log_rate_bounds[2],
    loss = loss, root_modes = modes, scale_mode = best_eta)
}

.rqr_cs_run_r <- function(y, q, delta, precision, roots, eta, control, spec, label) {
  X <- matrix(1, length(y), 1L)
  bounds <- spec$log_rate_bounds
  floor <- control$residual_product_floor * .rqr_ecm_response_product_scale(y)
  objective <- .rqr_cs_objective(y, roots, q, delta, eta, precision, spec)
  trace <- list(); backtracking <- 0L
  record <- function(phase, iteration) {
    data.frame(start_label = label, phase = phase, iteration = iteration,
      root1 = roots[1], root2 = roots[2], width = abs(diff(roots)),
      log_learning_rate = eta, learning_rate = exp(eta), loss_scale = exp(-eta),
      objective = objective, tilted_loss = .rqr_cs_loss(y, roots, q, delta),
      at_lower_bound = eta == bounds[1], at_upper_bound = eta == bounds[2])
  }
  trace[[1L]] <- record("initial", 0L)
  for (iteration in seq_len(control$max_iter)) {
    old <- roots; old_eta <- eta; old_objective <- objective
    constants <- rqr_constants(q, exp(eta))
    e <- (y-roots[1]) * (y-roots[2])
    inv <- .rqr_ecm_latent_inverse_mean(e, q, floor, control$floor_type)$inverse_mean
    ev <- .rqr_ecm_latent_mean(e, q, exp(eta))
    proposal <- roots
    for (j in 1:2) {
      system <- .rqr_root_gaussian_system(y, X, proposal[3L-j], constants,
        precision, rep(delta, length(y)), latent_mode = "inverse_moment", inverse_V_mean = inv)
      proposal[j] <- .rqr_precision_mean(system$precision, system$rhs)[1]
    }
    if (control$canonicalize_complete_roots) proposal <- sort(proposal)
    enew <- (y-proposal[1]) * (y-proposal[2])
    h <- sum(enew^2 * inv/(2*constants$phi) - constants$xi*enew/constants$phi +
      (constants$xi^2/(2*constants$phi)+1)*ev - q*delta*(sum(proposal)-2*y))
    proposed_eta <- if (spec$update == "ecm") .rqr_cs_scale_mode(h+spec$rate, 1.5*length(y)+spec$shape, bounds) else
      .rqr_cs_scale_mode(.rqr_cs_loss(y, proposal, q, delta)+spec$rate, length(y)+spec$shape, bounds)
    accepted <- FALSE
    for (bt in 0:control$backtracking_max_steps) {
      step <- 2^(-bt)
      next_roots <- old + step*(proposal-old); next_eta <- old_eta + step*(proposed_eta-old_eta)
      next_objective <- .rqr_cs_objective(y, next_roots, q, delta, next_eta, precision, spec)
      if (is.finite(next_objective) && next_objective <= old_objective + control$monotone_tolerance) {
        roots <- next_roots; eta <- next_eta; objective <- next_objective
        backtracking <- backtracking + bt; accepted <- TRUE; break
      }
    }
    if (!accepted) break
    trace[[length(trace)+1L]] <- record(spec$update, iteration)
    if (max(abs(roots-old), abs(eta-old_eta)) < control$tol_parameters &&
        abs(objective-old_objective) <= control$tol_objective*max(1, abs(old_objective))) break
  }
  # Exact observed-coordinate checks handle the nonsmooth residual knots.
  for (cycle in seq_len(spec$polish_cycles)) {
    for (j in 1:2) {
      proposed <- .rqr_cs_root_mode(y, roots[3L-j], q, delta, exp(eta), precision)
      change <- .rqr_cs_root_increment(y, roots, j, proposed, q, delta, exp(eta), precision)
      if (change > 1e-7 * max(1, abs(objective))) stop("Root polishing increased the objective.")
      roots[j] <- proposed
    }
    if (control$canonicalize_complete_roots) roots <- sort(roots)
    eta <- .rqr_cs_scale_mode(.rqr_cs_loss(y, roots, q, delta)+spec$rate, length(y)+spec$shape, bounds)
    objective <- .rqr_cs_objective(y, roots, q, delta, eta, precision, spec)
    trace[[length(trace)+1L]] <- record("observed_coordinate_polish", cycle)
    accuracy <- .rqr_cs_accuracy(y, roots, q, delta, eta, precision, spec)
    if (accuracy$coordinate_gap <= 1e-6 && accuracy$endpoint_gap <= 1e-5*max(1, abs(diff(roots))) &&
        accuracy$scale_gap <= 1e-6 && accuracy$scale_displacement <= 1e-5) break
  }
  list(roots = roots, eta = eta, objective = objective,
    trace = do.call(rbind, trace), backtracking_count = backtracking)
}

.rqr_ecm_common_scale_fit <- function(y, X, coverage_level, learning_rate, mean_tilt,
                                     beta_prior_obj, ecm_control, init, scale_control) {
  dat <- .rqr_assert_xy(y, X); y <- dat$y; X <- dat$X
  if (!.rqr_ecm_intercept_only(X)) stop("Unknown common-scale ECM currently requires an intercept-only design.", call. = FALSE)
  q <- .rqr_mt_assert_coverage(coverage_level)
  mt <- .rqr_normalize_mean_tilt(mean_tilt, length(y), rep(TRUE, length(y)))
  if (length(unique(mt$observed)) != 1L) stop("Unknown common-scale ECM requires a scalar fixed tilt.", call. = FALSE)
  delta <- mt$observed[1]
  spec <- .rqr_cs_spec(scale_control)
  if (!is.finite(spec$log_rate_bounds[2]) && delta != 0) {
    stop("Unrestricted Gamma scale learning requires zero tilt; nonzero-tilt propriety is not established.", call. = FALSE)
  }
  control <- .rqr_ecm_assert_control(ecm_control)
  if (!identical(control$floor_schedule, 1) || !control$monotone_backtracking) {
    stop("Common-scale ECM currently requires one residual floor and monotone backtracking.", call. = FALSE)
  }
  if (length(learning_rate) != 1L || !is.finite(learning_rate) || learning_rate <= 0 ||
      log(learning_rate) < spec$log_rate_bounds[1] || log(learning_rate) > spec$log_rate_bounds[2]) {
    stop("Initial learning_rate must lie inside the declared prior range.", call. = FALSE)
  }
  if (is.null(beta_prior_obj)) beta_prior_obj <- beta_prior("ridge", ridge = list(tau2 = 4))
  if (!identical(beta_prior_obj$type, "ridge")) stop("Unknown common-scale ECM requires proper ridge roots.", call. = FALSE)
  precision <- as.numeric(.rqr_prior_precision(beta_prior_obj, list(), p = 1L))
  if (length(precision) != 1L || !is.finite(precision) || precision <= 0) stop("Invalid Gaussian root precision.", call. = FALSE)
  starts <- if (length(y) >= 3L) .rqr_ecm_start_list(y, X, q, init, control) else {
    small <- .rqr_init_roots(y, X, q, init)
    list(list(label = "small_sample", beta1 = small$beta1, beta2 = small$beta2))
  }
  rates <- if (spec$multistart_rates) unique(c(log(learning_rate),
    spec$log_rate_bounds[is.finite(spec$log_rate_bounds)])) else log(learning_rate)
  results <- list(); summaries <- list()
  for (start in starts) for (eta in rates) {
    label <- paste0(start$label, "_eta_", format(eta, digits = 8))
    roots <- c(start$beta1, start$beta2)
    result <- if (control$ecm_backend == "cpp") {
      rqr_ecm_common_scale_run_cpp(y, q, delta, precision, roots, eta,
        spec$log_rate_bounds, control$max_iter, spec$polish_cycles,
        control$residual_product_floor, control$floor_type, control$tol_parameters,
        control$tol_objective, control$monotone_tolerance, control$backtracking_max_steps,
        spec$update, control$canonicalize_complete_roots, label, spec$shape, spec$rate)
    } else .rqr_cs_run_r(y, q, delta, precision, roots, eta, control, spec, label)
    if (!is.finite(result$objective) || any(!is.finite(result$roots))) stop("Nonfinite common-scale fit.")
    results[[length(results)+1L]] <- result
    summaries[[length(summaries)+1L]] <- data.frame(start_label = label,
      objective = result$objective, log_learning_rate = result$eta)
  }
  scores <- do.call(rbind, summaries); selected <- which.min(scores$objective); best <- results[[selected]]
  roots <- best$roots; eta <- best$eta; omega <- exp(eta)
  accuracy <- .rqr_cs_accuracy(y, roots, q, delta, eta, precision, spec)
  accurate <- accuracy$coordinate_gap <= 1e-6 && accuracy$endpoint_gap <= 1e-5*max(1, abs(diff(roots))) &&
    accuracy$scale_gap <= 1e-6 && accuracy$scale_displacement <= 1e-5
  if (!accurate && control$fail_on_nonconvergence) stop("Common-scale ECM requires additional numerical review.", call. = FALSE)
  constants <- rqr_constants(q, omega)
  comp <- .rqr_ecm_objective(y, X, roots[1], roots[2], constants, mt$observed, precision, precision)
  comp$scale_power <- -length(y)*eta; comp$total <- comp$total + comp$scale_power
  comp$scale_prior <- spec$rate*omega-spec$shape*eta
  comp$total <- comp$total + comp$scale_prior
  endpoints <- rqr_order_endpoints(rep(roots[1], length(y)), rep(roots[2], length(y)))
  endpoints$lower_mean <- mean(endpoints$lower); endpoints$upper_mean <- mean(endpoints$upper)
  endpoints$midpoint_mean <- mean(endpoints$midpoint); endpoints$width_mean <- mean(endpoints$width)
  endpoints$empirical_coverage <- mean(y >= endpoints$lower & y <= endpoints$upper)
  stationarity <- .rqr_ecm_stationarity(y, X, roots[1], roots[2], constants, mt$observed, precision, precision)
  stationarity$common_scale_accuracy <- accuracy
  out <- list(schema_version = "rqrgibbs_common_scale_ecm/1.0.0", method = "mti_common_scale_ecm",
    algorithm_variant = paste0(spec$update, "_common_scale_observed_coordinate_polish_", control$ecm_backend),
    target_formula = "exp(eta)*(L_delta(a,b)+rate)+Gaussian root penalties-(n+shape)*eta; density in (a,b,eta)",
    coverage_level = q, learning_rate = omega, mean_tilt = mt$observed,
    common_scale = c(spec, list(log_learning_rate = eta, loss_scale = exp(-eta),
      at_lower_bound = accuracy$at_lower_bound, at_upper_bound = accuracy$at_upper_bound,
      tilt_domain = .rqr_cs_tilt_domain(y,q,delta))),
    beta_prior_type = "ridge", beta_prior = list(type = "ridge", hypers = beta_prior_obj$hypers),
    beta_root1 = roots[1], beta_root2 = roots[2], ordered_endpoint_summary = endpoints,
    objective = comp$total, objective_components = comp[c("total", "product_loss", "mean_tilt", "prior_root1", "prior_root2", "scale_power", "scale_prior")],
    objective_trace = best$trace, multistart_summary = scores,
    selected_start = list(index = selected, label = scores$start_label[selected]),
    ecm_backend = control$ecm_backend, iterations = nrow(best$trace)-1L,
    converged = accurate, numerically_accurate = accurate,
    convergence_code = if (accurate) "observed_coordinate_accuracy_pass" else "additional_accuracy_review_required",
    stationarity_diagnostic = stationarity, residual_products = comp$residual_products,
    residual_floor = list(relative = control$residual_product_floor,
      absolute = control$residual_product_floor*.rqr_ecm_response_product_scale(y), floor_type = control$floor_type),
    safeguard_used = control$residual_product_floor > 0,
    exact_ecm_eligible = control$residual_product_floor == 0 && all(comp$residual_products != 0),
    latent_inverse_mean = .rqr_ecm_latent_inverse_mean(comp$residual_products, q,
      control$residual_product_floor*.rqr_ecm_response_product_scale(y), control$floor_type)$inverse_mean,
    latent_mean_optional = .rqr_ecm_latent_mean(comp$residual_products, q, omega),
    backtracking_count = best$backtracking_count, precision_repairs = 0L,
    response_likelihood = FALSE, formal_tolerance_action = FALSE, conditional_on_fixed_target = TRUE,
    model_spec = list(family = "rqr_fixed_design", inference = spec$update, generalized_bayes = TRUE,
      response_likelihood = FALSE, calibrated_uncertainty = FALSE, coverage_level = q,
      learning_rate = omega, learning_rate_mode = "bounded_common_scale", common_scale = spec,
      mean_tilt = mt$observed, beta_prior_type = "ridge", ecm_backend = control$ecm_backend), y = y, X = X,
    provenance = list(response_digest = .rqr_digest(y), design_digest = .rqr_digest(X),
      scale_prior_digest = .rqr_digest(spec), prior_digest = .rqr_digest(beta_prior_obj),
      objective_trace_digest = .rqr_digest(best$trace)))
  class(out) <- c("mti_ecm", "rqr_ecm", "mti_fit", "rqr_fit")
  out
}
