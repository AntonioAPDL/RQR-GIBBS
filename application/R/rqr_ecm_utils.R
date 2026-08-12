.rqr_ecm_schema <- function() {
  "rqrgibbs_fixed_target_ecm/1.0.0"
}

.rqr_ecm_path_schema <- function() {
  "rqrgibbs_fixed_target_ecm_path/1.0.0"
}

.rqr_ecm_assert_control <- function(ecm_control) {
  if (!is.list(ecm_control)) {
    stop("ecm_control must be a list.", call. = FALSE)
  }
  max_iter <- .rqr_scalar_integer(
    ecm_control$max_iter %||% 200L, "ecm_control$max_iter", 1L
  )
  tol_objective <- as.numeric(ecm_control$tol_objective %||% 1e-8)[1L]
  tol_parameters <- as.numeric(ecm_control$tol_parameters %||% 1e-7)[1L]
  tol_stationarity <- as.numeric(ecm_control$tol_stationarity %||% 1e-5)[1L]
  residual_product_floor <- as.numeric(
    ecm_control$residual_product_floor %||% 1e-8
  )[1L]
  monotone_tolerance <- as.numeric(
    ecm_control$monotone_tolerance %||% 1e-10
  )[1L]
  for (item in list(
    tol_objective = tol_objective,
    tol_parameters = tol_parameters,
    tol_stationarity = tol_stationarity,
    monotone_tolerance = monotone_tolerance
  )) {
    if (!is.finite(item) || item < 0) {
      stop("ECM tolerances must be finite and nonnegative.", call. = FALSE)
    }
  }
  if (!is.finite(residual_product_floor) || residual_product_floor < 0) {
    stop(
      "ecm_control$residual_product_floor must be finite and nonnegative.",
      call. = FALSE
    )
  }
  floor_type <- match.arg(
    as.character(ecm_control$floor_type %||% "hard")[1L],
    c("hard", "smooth")
  )
  floor_schedule <- as.numeric(ecm_control$floor_schedule %||% 1)
  if (!length(floor_schedule) || any(!is.finite(floor_schedule)) ||
      any(floor_schedule <= 0)) {
    stop("ecm_control$floor_schedule must contain positive finite values.",
         call. = FALSE)
  }
  floor_schedule <- sort(unique(floor_schedule), decreasing = TRUE)
  stable_iterations <- .rqr_scalar_integer(
    ecm_control$stable_iterations %||% 2L,
    "ecm_control$stable_iterations", 1L
  )
  backtracking_max_steps <- .rqr_scalar_integer(
    ecm_control$backtracking_max_steps %||% 20L,
    "ecm_control$backtracking_max_steps", 0L
  )
  jitter_starts <- .rqr_scalar_integer(
    ecm_control$jitter_starts %||% 0L, "ecm_control$jitter_starts", 0L
  )
  list(
    max_iter = max_iter,
    tol_objective = tol_objective,
    tol_parameters = tol_parameters,
    tol_stationarity = tol_stationarity,
    stable_iterations = stable_iterations,
    verbose = isTRUE(ecm_control$verbose %||% FALSE),
    residual_product_floor = residual_product_floor,
    floor_type = floor_type,
    floor_schedule = floor_schedule,
    monotone_backtracking = isTRUE(ecm_control$monotone_backtracking %||% TRUE),
    backtracking_max_steps = backtracking_max_steps,
    monotone_tolerance = monotone_tolerance,
    multistart = isTRUE(ecm_control$multistart %||% TRUE),
    jitter_starts = jitter_starts,
    seed = ecm_control$seed %||% ecm_control$rng_seed %||% NULL,
    precision_jitter = as.numeric(ecm_control$precision_jitter %||% 1e-10)[1L],
    canonicalize_complete_roots =
      isTRUE(ecm_control$canonicalize_complete_roots %||% TRUE),
    store_iteration_trace =
      isTRUE(ecm_control$store_iteration_trace %||% TRUE),
    fail_on_nonconvergence =
      isTRUE(ecm_control$fail_on_nonconvergence %||% FALSE)
  )
}

.rqr_ecm_response_product_scale <- function(y) {
  y <- as.numeric(y)
  scale <- stats::sd(y)^2
  if (!is.finite(scale) || scale <= 0) scale <- mean((y - mean(y))^2)
  if (!is.finite(scale) || scale <= 0) scale <- 1
  max(scale, 1)
}

.rqr_ecm_latent_inverse_mean <- function(e, coverage_level,
                                         residual_product_floor = 0,
                                         floor_type = c("hard", "smooth")) {
  q <- .rqr_mt_assert_coverage(coverage_level)
  e <- as.numeric(e)
  if (!length(e) || any(!is.finite(e))) {
    stop("e must be a nonempty finite residual-product vector.",
         call. = FALSE)
  }
  residual_product_floor <- as.numeric(residual_product_floor)[1L]
  if (!is.finite(residual_product_floor) || residual_product_floor < 0) {
    stop("residual_product_floor must be finite and nonnegative.",
         call. = FALSE)
  }
  floor_type <- match.arg(floor_type)
  abs_e <- abs(e)
  zero_residual_count <- sum(abs_e == 0)
  if (residual_product_floor > 0) {
    scale <- if (identical(floor_type, "hard")) {
      pmax(abs_e, residual_product_floor)
    } else {
      sqrt(e^2 + residual_product_floor^2)
    }
  } else {
    if (any(abs_e == 0)) {
      stop(
        paste(
          "Exact ECM inverse latent moment is infinite at zero residual",
          "products; set a positive residual_product_floor."
        ),
        call. = FALSE
      )
    }
    scale <- abs_e
  }
  tau <- 1 / (q * (1 - q) * scale)
  list(
    inverse_mean = tau,
    residual_product_scale = scale,
    minimum_absolute_residual_product = min(abs_e),
    zero_residual_count = as.integer(zero_residual_count),
    floor = residual_product_floor,
    floor_type = floor_type,
    exact_moment_used = residual_product_floor == 0 && zero_residual_count == 0
  )
}

.rqr_ecm_latent_mean <- function(e, coverage_level, learning_rate = 1) {
  constants <- rqr_constants(coverage_level, learning_rate)
  constants$alpha * (1 - constants$alpha) * (abs(as.numeric(e)) + 2 * constants$sigma)
}

.rqr_ecm_objective <- function(y, X, beta1, beta2, constants,
                               mean_tilt_observed, prior_prec1,
                               prior_prec2) {
  eta1 <- drop(X %*% beta1)
  eta2 <- drop(X %*% beta2)
  loss <- rqr_mean_tilt_loss(
    y, eta1, eta2,
    coverage_level = constants$alpha,
    mean_tilt = mean_tilt_observed,
    details = TRUE
  )
  product_component <- constants$omega * sum(loss$product_loss)
  mean_tilt_component <- -constants$omega * sum(loss$linear_tilt)
  prior_root1_component <- 0.5 * sum(as.numeric(prior_prec1) * beta1^2)
  prior_root2_component <- 0.5 * sum(as.numeric(prior_prec2) * beta2^2)
  total <- product_component + mean_tilt_component +
    prior_root1_component + prior_root2_component
  list(
    total = as.numeric(total),
    product_loss = as.numeric(product_component),
    mean_tilt = as.numeric(mean_tilt_component),
    prior_root1 = as.numeric(prior_root1_component),
    prior_root2 = as.numeric(prior_root2_component),
    residual_products = rqr_residual_product(y, eta1, eta2),
    eta1 = eta1,
    eta2 = eta2
  )
}

.rqr_ecm_stationarity <- function(y, X, beta1, beta2, constants,
                                  mean_tilt_observed, prior_prec1,
                                  prior_prec2) {
  eta1 <- drop(X %*% beta1)
  eta2 <- drop(X %*% beta2)
  e <- rqr_residual_product(y, eta1, eta2)
  psi_midpoint <- constants$alpha - as.numeric(e < 0)
  psi_midpoint[e == 0] <- constants$alpha - 0.5
  A2 <- X * as.numeric(y - eta2)
  A1 <- X * as.numeric(y - eta1)
  tilt_shift <- constants$omega * constants$alpha *
    as.numeric(crossprod(X, mean_tilt_observed))
  g1 <- -constants$omega * as.numeric(crossprod(A2, psi_midpoint)) -
    tilt_shift + as.numeric(prior_prec1) * beta1
  g2 <- -constants$omega * as.numeric(crossprod(A1, psi_midpoint)) -
    tilt_shift + as.numeric(prior_prec2) * beta2
  list(
    max_abs_midpoint_gradient = max(abs(c(g1, g2))),
    root1_midpoint_gradient = g1,
    root2_midpoint_gradient = g2,
    zero_residual_count = as.integer(sum(e == 0)),
    minimum_absolute_residual_product = min(abs(e)),
    diagnostic = if (any(e == 0)) {
      "midpoint_subgradient_reported_at_zero_residual_products"
    } else {
      "ordinary_gradient"
    }
  )
}

.rqr_ecm_precision_mean <- function(precision, rhs, jitter = 1e-10) {
  repair_count <- 0L
  condition_number <- suppressWarnings(kappa(precision, exact = FALSE))
  attempt <- tryCatch(
    .rqr_precision_mean(precision, rhs),
    error = function(e) e
  )
  if (!inherits(attempt, "error")) {
    return(list(mean = attempt, precision = precision,
                repair_count = repair_count,
                condition_number = condition_number))
  }
  scale <- max(abs(diag(precision)), 1)
  for (ii in seq_len(8L)) {
    repair_count <- ii
    repaired <- precision + diag(jitter * 10^(ii - 1L) * scale, ncol(precision))
    attempt <- tryCatch(
      .rqr_precision_mean(repaired, rhs),
      error = function(e) e
    )
    if (!inherits(attempt, "error")) {
      return(list(mean = attempt, precision = repaired,
                  repair_count = repair_count,
                  condition_number = suppressWarnings(kappa(repaired, exact = FALSE))))
    }
  }
  stop("ECM root precision solve failed after diagonal jitter repairs.",
       call. = FALSE)
}

.rqr_ecm_canonicalize_roots <- function(beta1, beta2, X) {
  eta1 <- drop(X %*% beta1)
  eta2 <- drop(X %*% beta2)
  if (mean(eta1 - eta2) <= 0) {
    return(list(beta1 = beta1, beta2 = beta2, swapped = FALSE))
  }
  list(beta1 = beta2, beta2 = beta1, swapped = TRUE)
}

.rqr_ecm_intercept_only <- function(X, tol = 1e-12) {
  X <- as.matrix(X)
  ncol(X) == 1L && max(abs(X[, 1L] - 1)) <= tol
}

.rqr_ecm_start_list <- function(y, X, coverage_level, init, control) {
  p <- ncol(X)
  starts <- list()
  add_start <- function(label, beta1, beta2) {
    beta1 <- as.numeric(beta1)
    beta2 <- as.numeric(beta2)
    if (length(beta1) != p || length(beta2) != p ||
        any(!is.finite(beta1)) || any(!is.finite(beta2))) {
      return(invisible(FALSE))
    }
    starts[[length(starts) + 1L]] <<- list(
      label = label,
      beta1 = beta1,
      beta2 = beta2
    )
    invisible(TRUE)
  }
  if (!is.null(init$previous_fit) && inherits(init$previous_fit, "rqr_ecm")) {
    add_start("previous_path_solution",
              init$previous_fit$beta_root1,
              init$previous_fit$beta_root2)
  }
  if (!is.null(init$beta1) || !is.null(init$beta2) ||
      !is.null(init$beta_root1) || !is.null(init$beta_root2)) {
    explicit <- .rqr_init_roots(y, X, coverage_level, init = init)
    add_start("user_supplied", explicit$beta1, explicit$beta2)
  }
  base <- .rqr_init_roots(y, X, coverage_level, init = list())
  add_start("centered_quantile", base$beta1, base$beta2)
  add_start("centered_quantile_swap", base$beta2, base$beta1)
  if (.rqr_ecm_intercept_only(X)) {
    k <- min(length(y), max(2L, as.integer(ceiling(coverage_level * length(y)))))
    win <- rqr_tcsp_shortest_window(y, retained_count = k)
    add_start("intercept_empirical_shortest",
              rep(win$lower_endpoint, p), rep(win$upper_endpoint, p))
    gamma <- tryCatch(.rqr_mt_adjusted_fisher_pearson_skewness(y),
                      error = function(e) NA_real_)
    if (is.finite(gamma)) {
      prob <- .rqr_mt_cf_probability_window(
        coverage_level, gamma1 = gamma, target = "shortest"
      )
      qs <- stats::quantile(
        y, probs = c(prob$u_lower, prob$u_upper),
        names = FALSE, type = 8
      )
      add_start("cornish_fisher_shortest",
                rep(qs[[1L]], p), rep(qs[[2L]], p))
    }
  }
  if (isTRUE(control$multistart) && control$jitter_starts > 0L) {
    restore_rng <- .rqr_mt_seed_scope(control$seed %||% 1L)
    on.exit(restore_rng(), add = TRUE)
    jitter_scale <- 0.02 * max(stats::sd(y), 1)
    for (ii in seq_len(control$jitter_starts)) {
      add_start(
        sprintf("seeded_complete_root_jitter_%02d", ii),
        base$beta1 + stats::rnorm(p, sd = jitter_scale),
        base$beta2 + stats::rnorm(p, sd = jitter_scale)
      )
    }
  }
  if (!isTRUE(control$multistart) && length(starts) > 1L) {
    starts <- starts[1L]
  }
  starts
}

.rqr_ecm_provenance <- function(y, X, coverage_level, learning_rate,
                                mean_tilt_info, beta_prior_obj, control,
                                objective_trace) {
  git_commit <- tryCatch(
    system2("git", c("rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE),
    error = function(e) NA_character_
  )
  list(
    git_commit = if (length(git_commit)) git_commit[[1L]] else NA_character_,
    package_version = utils::packageDescription("rqrgibbs")$Version %||% NA_character_,
    target_content = coverage_level,
    learning_rate = learning_rate,
    mean_tilt_digest = mean_tilt_info$digest,
    prior_digest = .rqr_digest(beta_prior_obj),
    design_digest = .rqr_digest(X),
    response_digest = .rqr_digest(y),
    algorithm_schema = .rqr_ecm_schema(),
    safeguard_policy = list(
      residual_product_floor = control$residual_product_floor,
      floor_type = control$floor_type,
      floor_schedule = control$floor_schedule
    ),
    objective_trace_digest = .rqr_digest(objective_trace)
  )
}
