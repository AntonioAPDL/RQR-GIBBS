# Exact component-scale evolution specifications for RQR-DLM.

#' Construct a fixed-covariance RQR evolution specification
#'
#' @param W Evolution covariance matrix or time-varying cube. Dimensions and
#'   positive-semidefinite validity are checked against the model at fit time.
#' @return An exact fixed-prior `rqr_evolution` specification.
#' @export
rqr_evolution_fixed <- function(W) {
  if (is.null(W) || !is.numeric(W) || !length(W) || any(!is.finite(W))) {
    stop("W must be a finite numeric covariance matrix or cube.", call. = FALSE)
  }
  structure(list(
    mode = "fixed_W", W = W, exact_joint_target = TRUE,
    frozen_before_mcmc = TRUE
  ), class = "rqr_evolution")
}

#' Construct an adaptive working-discount evolution specification
#'
#' This constructor preserves the exdqlm component-discount matrix interface
#' while making the non-joint-target status explicit in its name and metadata.
#'
#' @param df Component discounts in `(0,1]`.
#' @param component_dims Positive state-block dimensions.
#' @return An experimental working/sequential `rqr_evolution` specification.
#' @export
rqr_evolution_adaptive_working <- function(df, component_dims) {
  component_dims <- .rqr_positive_integer_vector(component_dims, "component_dims")
  D <- rqr_discount_matrix(df, component_dims, sum(component_dims))
  structure(list(
    mode = "adaptive_discount",
    df = as.numeric(df),
    dim.df = component_dims,
    D = D,
    exact_joint_target = FALSE,
    frozen_before_mcmc = FALSE,
    working_sequential = TRUE
  ), class = "rqr_evolution")
}

.rqr_component_indices <- function(component_dims) {
  ends <- cumsum(component_dims)
  starts <- c(1L, utils::head(ends, -1L) + 1L)
  Map(seq.int, starts, ends)
}

.rqr_validate_spd_template <- function(x, d, name) {
  dx <- dim(x)
  if (length(dx) == 2L) x <- array(as.matrix(x), c(d, d, 1L))
  dx <- dim(x)
  if (length(dx) != 3L || !all(dx[1:2] == c(d, d)) || dx[3L] < 1L ||
      any(!is.finite(x))) {
    stop(sprintf("%s must be a finite %d x %d matrix or cube.", name, d, d), call. = FALSE)
  }
  for (tt in seq_len(dx[3L])) {
    x[, , tt] <- .rqr_validate_symmetric_matrix(
      x[, , tt], sprintf("%s slice %d", name, tt)
    )
    .rqr_chol_with_jitter(x[, , tt], jitter_ladder = 0)
  }
  x
}

#' Construct an exact component-scale evolution prior
#'
#' Defines `W_t = blockdiag(q_1 Q_1t, ..., q_J Q_Jt)` with fixed positive-
#' definite templates and shared inverse-Gamma component multipliers across the
#' two exchangeable roots. This is distinct from adaptive discount recursion.
#'
#' @param templates List of component covariance matrices or time-varying cubes.
#' @param component_dims Positive component dimensions summing to the state size.
#' @param prior Inverse-Gamma shape and rate/scale lists or vectors.
#' @param initial Positive initial component multipliers.
#' @param component_names Optional component names.
#' @return An `rqr_evolution` specification.
#' @export
rqr_evolution_component_scale <- function(
    templates, component_dims, prior = list(shape = 2, rate = 1),
    initial = 1, component_names = NULL) {
  component_dims <- .rqr_positive_integer_vector(component_dims, "component_dims")
  J <- length(component_dims)
  if (!is.list(templates) || length(templates) != J) {
    stop("templates must be a list with one matrix or cube per component.", call. = FALSE)
  }
  templates <- lapply(seq_len(J), function(j) {
    .rqr_validate_spd_template(templates[[j]], component_dims[j], sprintf("templates[[%d]]", j))
  })
  template_times <- vapply(templates, function(x) dim(x)[3L], integer(1L))
  nonconstant <- unique(template_times[template_times > 1L])
  if (length(nonconstant) > 1L) {
    stop("Time-varying component templates must have a common number of slices.", call. = FALSE)
  }
  if (!is.list(prior)) stop("prior must be a list with shape and rate.", call. = FALSE)
  shape <- as.numeric(prior$shape %||% prior$a %||% 2)
  rate <- as.numeric(prior$rate %||% prior$scale %||% prior$b %||% 1)
  if (!length(shape) %in% c(1L, J) || !length(rate) %in% c(1L, J)) {
    stop("Component-scale inverse-Gamma shape and rate must be scalar or length J.", call. = FALSE)
  }
  shape <- rep_len(shape, J)
  rate <- rep_len(rate, J)
  if (any(!is.finite(shape)) ||
      any(!is.finite(rate)) || any(shape <= 0) || any(rate <= 0)) {
    stop("Component-scale inverse-Gamma shape and rate must be positive.", call. = FALSE)
  }
  initial <- as.numeric(initial)
  if (!length(initial) %in% c(1L, J)) {
    stop("initial must be scalar or length J.", call. = FALSE)
  }
  initial <- rep_len(initial, J)
  if (any(!is.finite(initial)) || any(initial <= 0)) {
    stop("initial must contain positive component multipliers.", call. = FALSE)
  }
  if (is.null(component_names)) component_names <- paste0("component", seq_len(J))
  component_names <- as.character(component_names)
  if (length(component_names) != J || anyNA(component_names) || any(!nzchar(component_names)) ||
      anyDuplicated(component_names)) {
    stop("component_names must be unique nonempty names matching component_dims.", call. = FALSE)
  }
  structure(list(
    mode = "component_scale",
    templates = templates,
    component_dims = component_dims,
    component_names = component_names,
    prior = list(shape = shape, rate = rate),
    initial = initial,
    exact_joint_target = TRUE,
    frozen_before_mcmc = TRUE,
    shared_across_roots = TRUE
  ), class = "rqr_evolution")
}

.rqr_expand_component_templates <- function(evolution, n_time, p) {
  if (!inherits(evolution, "rqr_evolution") || !identical(evolution$mode, "component_scale")) {
    stop("Expected a component_scale rqr_evolution object.", call. = FALSE)
  }
  dims <- as.integer(evolution$component_dims)
  if (sum(dims) != p) stop("Component template dimensions do not match the state dimension.", call. = FALSE)
  lapply(seq_along(dims), function(j) {
    template <- evolution$templates[[j]]
    nt <- dim(template)[3L]
    if (nt == n_time) return(template)
    if (nt == 1L) return(array(rep(template[, , 1L], n_time), c(dims[j], dims[j], n_time)))
    stop(sprintf("Component template %d must have one or n_time slices.", j), call. = FALSE)
  })
}

.rqr_materialize_component_evolution <- function(evolution, q, n_time, p) {
  q <- as.numeric(q)
  dims <- as.integer(evolution$component_dims)
  if (length(q) != length(dims) || any(!is.finite(q)) || any(q <= 0)) {
    stop("Component evolution scales must be finite and positive.", call. = FALSE)
  }
  templates <- .rqr_expand_component_templates(evolution, n_time, p)
  indices <- .rqr_component_indices(dims)
  W <- array(0, c(p, p, n_time))
  for (tt in seq_len(n_time)) {
    for (j in seq_along(dims)) {
      W[indices[[j]], indices[[j]], tt] <- q[j] * templates[[j]][, , tt]
    }
  }
  structure(list(
    mode = "component_scale", W = W, exact_joint_target = TRUE,
    frozen_before_mcmc = FALSE, component_scales = q
  ), class = "rqr_evolution")
}

.rqr_component_W_from_expanded_templates <- function(
    templates, component_dims, q, n_time, p) {
  component_dims <- as.integer(component_dims)
  q <- as.numeric(q)
  if (length(templates) != length(component_dims) ||
      length(q) != length(component_dims) ||
      sum(component_dims) != p ||
      any(!is.finite(q)) || any(q <= 0)) {
    stop(
      "The expanded component-covariance inputs are invalid.",
      call. = FALSE
    )
  }
  indices <- .rqr_component_indices(component_dims)
  W <- array(0, c(p, p, n_time))
  for (j in seq_along(component_dims)) {
    W[indices[[j]], indices[[j]], ] <-
      q[[j]] * templates[[j]]
  }
  W
}

.rqr_draw_initial_state <- function(theta1, G1, m0, C0, W1) {
  theta1 <- as.numeric(theta1)
  m0 <- as.numeric(m0)
  p <- length(m0)
  G1 <- as.matrix(G1)
  C0 <- .rqr_validate_symmetric_matrix(C0, "C0")
  W1 <- .rqr_validate_symmetric_matrix(W1, "W1")
  if (length(theta1) != p ||
      !identical(dim(G1), c(p, p)) ||
      !identical(dim(C0), c(p, p)) ||
      !identical(dim(W1), c(p, p))) {
    stop(
      "The time-zero conditional inputs have incompatible dimensions.",
      call. = FALSE
    )
  }
  forecast_covariance <- .rqr_symmetrize(
    G1 %*% C0 %*% t(G1) + W1
  )
  forecast_factor <- tryCatch(
    chol(forecast_covariance), error = function(error) NULL
  )
  if (is.null(forecast_factor)) {
    forecast_eigen <- eigen(forecast_covariance, symmetric = TRUE)
    forecast_scale <- max(abs(forecast_eigen$values))
    if (forecast_scale > 0 &&
        min(forecast_eigen$values) / forecast_scale < -1e-10) {
      stop(
        "The time-zero forecast covariance is materially indefinite.",
        call. = FALSE
      )
    }
    rank_tolerance <- 100 * .Machine$double.eps *
      max(1, p) * forecast_scale
    positive <- forecast_eigen$values > rank_tolerance
    forecast_inverse <- if (any(positive)) {
      forecast_eigen$vectors[, positive, drop = FALSE] %*%
        (t(forecast_eigen$vectors[, positive, drop = FALSE]) /
          forecast_eigen$values[positive])
    } else {
      matrix(0, p, p)
    }
    solve_forecast <- function(value) forecast_inverse %*% value
  } else {
    solve_forecast <- function(value) {
      backsolve(
        forecast_factor,
        forwardsolve(t(forecast_factor), value)
      )
    }
  }
  gain <- C0 %*% t(G1)
  innovation <- theta1 - drop(G1 %*% m0)
  if (is.null(forecast_factor)) {
    range_residual <- innovation -
      drop(forecast_covariance %*% solve_forecast(innovation))
    residual_scale <- max(
      abs(innovation), sqrt(forecast_scale), .Machine$double.xmin
    )
    if (max(abs(range_residual)) / residual_scale > 1e-8) {
      stop(
        "The time-one state is outside the singular forecast support.",
        call. = FALSE
      )
    }
  }
  conditional_mean <- m0 + drop(
    gain %*% solve_forecast(innovation)
  )
  conditional_covariance <- .rqr_symmetrize(
    C0 - gain %*% solve_forecast(G1 %*% C0)
  )
  conditional_factor <- tryCatch(
    chol(conditional_covariance), error = function(error) NULL
  )
  if (!is.null(conditional_factor)) {
    return(as.numeric(
      conditional_mean +
        t(conditional_factor) %*% stats::rnorm(p)
    ))
  }
  conditional_eigen <- eigen(
    conditional_covariance, symmetric = TRUE
  )
  conditional_scale <- max(abs(conditional_eigen$values))
  if (conditional_scale > 0 &&
      min(conditional_eigen$values) / conditional_scale < -1e-10) {
    stop(
      "The time-zero conditional covariance is materially indefinite.",
      call. = FALSE
    )
  }
  as.numeric(
    conditional_mean +
      conditional_eigen$vectors %*%
        (sqrt(pmax(conditional_eigen$values, 0)) *
          stats::rnorm(p))
  )
}

.rqr_component_scale_posterior <- function(
    theta1, theta2, theta01, theta02, GG, evolution) {
  theta1 <- as.matrix(theta1)
  theta2 <- as.matrix(theta2)
  p <- nrow(theta1)
  T <- ncol(theta1)
  if (!all(dim(theta2) == c(p, T))) stop("Root paths have incompatible dimensions.", call. = FALSE)
  GG <- .rqr_expand_cube(GG, T, p, "GG")
  templates <- .rqr_expand_component_templates(evolution, T, p)
  dims <- as.integer(evolution$component_dims)
  indices <- .rqr_component_indices(dims)
  shape <- evolution$prior$shape + T * dims
  rate <- as.numeric(evolution$prior$rate)
  theta0 <- list(as.numeric(theta01), as.numeric(theta02))
  paths <- list(theta1, theta2)
  for (k in 1:2) {
    previous <- theta0[[k]]
    for (tt in seq_len(T)) {
      innovation <- paths[[k]][, tt] - drop(GG[, , tt] %*% previous)
      for (j in seq_along(dims)) {
        d <- innovation[indices[[j]]]
        U <- chol(templates[[j]][, , tt])
        whitened <- forwardsolve(t(U), d)
        rate[j] <- rate[j] + 0.5 * sum(whitened^2)
      }
      previous <- paths[[k]][, tt]
    }
  }
  list(shape = shape, rate = rate)
}

.rqr_sample_component_scales <- function(theta1, theta2, theta01, theta02, GG, evolution) {
  posterior <- .rqr_component_scale_posterior(
    theta1, theta2, theta01, theta02, GG, evolution
  )
  list(
    draw = 1 / stats::rgamma(length(posterior$shape), posterior$shape, rate = posterior$rate),
    posterior = posterior
  )
}

.rqr_conditioned_component_scale_kernel <- function(
    theta, theta0, GG, evolution) {
  theta <- as.matrix(theta)
  p <- nrow(theta)
  T <- ncol(theta)
  theta0 <- as.numeric(theta0)
  if (length(theta0) != p ||
      any(!is.finite(c(theta, theta0)))) {
    stop(
      "The conditioned component-scale path inputs are invalid.",
      call. = FALSE
    )
  }
  GG <- .rqr_expand_cube(GG, T, p, "GG")
  templates <- .rqr_expand_component_templates(evolution, T, p)
  dims <- as.integer(evolution$component_dims)
  indices <- .rqr_component_indices(dims)
  rate_increment <- numeric(length(dims))
  previous <- theta0
  for (tt in seq_len(T)) {
    innovation <- theta[, tt] - drop(GG[, , tt] %*% previous)
    for (j in seq_along(dims)) {
      U <- chol(templates[[j]][, , tt])
      whitened <- forwardsolve(
        t(U), innovation[indices[[j]]]
      )
      rate_increment[[j]] <- rate_increment[[j]] +
        0.5 * sum(whitened^2)
    }
    previous <- theta[, tt]
  }
  list(
    log_scale_power = 0.5 * T * dims,
    rate_increment = rate_increment
  )
}

.rqr_collapsed_component_scale_log_density <- function(
    log_q, conditioned_kernel, z, H, obs_variance, GG, m0, C0,
    evolution, backend = c("cpp", "R", "auto"),
    expanded_templates = NULL) {
  log_q <- as.numeric(log_q)
  candidate_q <- exp(log_q)
  if (length(candidate_q) != length(evolution$component_dims) ||
      any(!is.finite(candidate_q)) || any(candidate_q <= 0)) {
    return(-Inf)
  }
  backend <- match.arg(backend)
  candidate_W <- if (is.null(expanded_templates)) {
    .rqr_materialize_component_evolution(
      evolution, candidate_q, length(z), length(m0)
    )$W
  } else {
    .rqr_component_W_from_expanded_templates(
      expanded_templates, evolution$component_dims, candidate_q,
      length(z), length(m0)
    )
  }
  log_marginal <- tryCatch(
    if (identical(backend, "cpp") &&
        !is.null(expanded_templates)) {
      as.numeric(rqr_filter_log_marginal_cpp(
        z, H, obs_variance, GG, m0, C0, candidate_W
      ))
    } else {
      .rqr_filter_log_marginal(
        z = z, H = H, V = obs_variance, GG = GG,
        m0 = m0, C0 = C0,
        evolution = structure(
          list(mode = "component_scale", W = candidate_W),
          class = "rqr_evolution"
        ),
        backend = backend
      )
    },
    error = function(error) -Inf
  )
  if (!is.finite(log_marginal)) return(-Inf)
  sum(
    -(as.numeric(evolution$prior$shape) +
        conditioned_kernel$log_scale_power) * log_q -
      (as.numeric(evolution$prior$rate) +
        conditioned_kernel$rate_increment) / candidate_q
  ) + log_marginal
}

.rqr_collapsed_component_scale_update <- function(
    conditioned_theta, conditioned_theta0, z, H, obs_variance,
    GG, m0, C0, evolution, q, backend = c("cpp", "R", "auto"),
    width = 1, sweeps = 1L, max_steps = 100L,
    max_shrink = 1000L) {
  q <- as.numeric(q)
  log_q <- log(q)
  if (length(q) != length(evolution$component_dims) ||
      any(!is.finite(q)) || any(q <= 0)) {
    stop(
      "The collapsed component-scale values are invalid.",
      call. = FALSE
    )
  }
  backend <- match.arg(backend)
  conditioned_theta <- as.matrix(conditioned_theta)
  n_time <- ncol(conditioned_theta)
  p <- nrow(conditioned_theta)
  z <- as.numeric(z)
  H <- as.matrix(H)
  obs_variance <- as.numeric(obs_variance)
  GG <- .rqr_expand_cube(GG, n_time, p, "GG")
  m0 <- as.numeric(m0)
  C0 <- as.matrix(C0)
  if (length(z) != n_time ||
      !identical(dim(H), c(p, n_time)) ||
      length(obs_variance) != n_time ||
      length(m0) != p ||
      !identical(dim(C0), c(p, p)) ||
      any(is.nan(z)) || any(is.infinite(z)) ||
      any(!is.finite(c(H, obs_variance, GG, m0, C0))) ||
      any(obs_variance <= 0)) {
    stop(
      "The collapsed component-scale filter inputs are invalid.",
      call. = FALSE
    )
  }
  expanded_templates <- .rqr_expand_component_templates(
    evolution, n_time, p
  )
  conditioned_kernel <- .rqr_conditioned_component_scale_kernel(
    conditioned_theta, conditioned_theta0, GG, evolution
  )
  evaluate <- function(candidate_log_q) {
    .rqr_collapsed_component_scale_log_density(
      candidate_log_q, conditioned_kernel, z, H, obs_variance,
      GG, m0, C0, evolution, backend, expanded_templates
    )
  }
  sweeps <- .rqr_scalar_integer(
    sweeps, "collapsed component-scale slice sweeps", 1L
  )
  evaluation_count <- shrink_count <- integer(length(q))
  for (sweep in seq_len(sweeps)) {
    for (j in seq_along(q)) {
      coordinate_density <- function(value) {
        candidate <- log_q
        candidate[[j]] <- value
        evaluate(candidate)
      }
      update <- .rqr_slice_log_coordinate(
        log_q[[j]], coordinate_density, width = width,
        max_steps = max_steps, max_shrink = max_shrink
      )
      log_q[[j]] <- update$value
      evaluation_count[[j]] <-
        evaluation_count[[j]] + update$evaluations
      shrink_count[[j]] <-
        shrink_count[[j]] + update$shrink_steps
    }
  }
  list(
    q = exp(log_q),
    diagnostics = list(
      evaluations = evaluation_count,
      shrink_steps = shrink_count,
      sweeps = sweeps,
      integrated_root_path = TRUE,
      conditioned_root_path = TRUE,
      exact_partially_collapsed = TRUE
    ),
    conditioned_kernel = conditioned_kernel
  )
}

.rqr_zero_mean_state_prior_draw <- function(
    GG, C0, W, jitter_ladder, numerical_policy) {
  C0 <- as.matrix(C0)
  p <- nrow(C0)
  if (ncol(C0) != p || length(dim(W)) != 3L ||
      !all(dim(W)[1:2] == c(p, p))) {
    stop("The joint state-prior draw dimensions are invalid.", call. = FALSE)
  }
  T <- dim(W)[3L]
  GG <- .rqr_expand_cube(GG, T, p, "GG")
  theta <- matrix(NA_real_, p, T)
  repairs <- .rqr_empty_repair_records()
  initial <- .rqr_sample_mvnorm_covariance(
    rep(0, p), C0, jitter_ladder, numerical_policy
  )
  repairs <- .rqr_add_repair_record(
    repairs, "joint_elliptical_prior_initial", 0L, initial$info
  )
  previous <- initial$draw
  for (tt in seq_len(T)) {
    innovation <- .rqr_sample_mvnorm_covariance(
      rep(0, p), W[, , tt], jitter_ladder, numerical_policy
    )
    repairs <- .rqr_add_repair_record(
      repairs, "joint_elliptical_prior_innovation", tt,
      innovation$info
    )
    theta[, tt] <- drop(GG[, , tt] %*% previous) + innovation$draw
    previous <- theta[, tt]
  }
  list(theta0 = initial$draw, theta = theta, repair_records = repairs)
}

.rqr_state_prior_mean <- function(GG, m0, n_time) {
  m0 <- as.numeric(m0)
  p <- length(m0)
  GG <- .rqr_expand_cube(GG, n_time, p, "GG")
  value <- matrix(NA_real_, p, n_time)
  previous <- m0
  for (tt in seq_len(n_time)) {
    value[, tt] <- drop(GG[, , tt] %*% previous)
    previous <- value[, tt]
  }
  value
}

.rqr_joint_state_augmented_log_likelihood <- function(
    theta1, theta2, FF, y, observed, v, xi, obs_variance) {
  theta1 <- as.matrix(theta1)
  theta2 <- as.matrix(theta2)
  FF <- as.matrix(FF)
  y <- as.numeric(y)
  observed <- as.logical(observed)
  v <- as.numeric(v)
  obs_variance <- as.numeric(obs_variance)
  T <- length(y)
  if (!identical(dim(theta1), dim(theta2)) || ncol(theta1) != T ||
      !identical(dim(FF), c(nrow(theta1), T)) || length(observed) != T ||
      length(v) != T || length(obs_variance) != T || anyNA(observed) ||
      any(!is.finite(c(theta1, theta2, FF, y[observed], v,
                       obs_variance, xi))) || any(v <= 0) ||
      any(obs_variance <= 0)) {
    stop("The joint elliptical likelihood inputs are invalid.",
         call. = FALSE)
  }
  eta1 <- .rqr_state_ordinates(FF, theta1)
  eta2 <- .rqr_state_ordinates(FF, theta2)
  residual <- rqr_residual_product(
    y[observed], eta1[observed], eta2[observed]
  ) - xi * v[observed]
  value <- -0.5 * sum(residual^2 / obs_variance[observed])
  if (!is.finite(value)) -Inf else value
}

# Exact joint path move for the two roots conditional on component scales and
# pseudo-AL latent variables. The stacked state prior is Gaussian even though
# the augmented observation kernel is quartic jointly in the roots. Elliptical
# slice sampling therefore supplies a joint, rejection-free-in-stationarity
# transition without pretending that a simultaneous Gaussian FFBS draw exists.
.rqr_joint_state_elliptical_slice <- function(
    theta1, theta2, theta01, theta02, GG, FF, m0, C0, W,
    y, observed, v, xi, obs_variance,
    jitter_ladder = 0, numerical_policy = c("fail", "record_repair"),
    max_shrink = 1000L) {
  theta1 <- as.matrix(theta1)
  theta2 <- as.matrix(theta2)
  theta01 <- as.numeric(theta01)
  theta02 <- as.numeric(theta02)
  m0 <- as.numeric(m0)
  C0 <- as.matrix(C0)
  p <- length(m0)
  T <- length(y)
  if (!identical(dim(theta1), c(p, T)) ||
      !identical(dim(theta2), c(p, T)) || length(theta01) != p ||
      length(theta02) != p || !identical(dim(C0), c(p, p)) ||
      length(dim(W)) != 3L || !all(dim(W) == c(p, p, T)) ||
      any(!is.finite(c(theta1, theta2, theta01, theta02, m0, C0, W)))) {
    stop("The joint elliptical state dimensions are invalid.",
         call. = FALSE)
  }
  numerical_policy <- .rqr_numerical_policy(numerical_policy)
  jitter_ladder <- .rqr_jitter_ladder(numerical_policy, jitter_ladder)
  max_shrink <- .rqr_scalar_integer(
    max_shrink, "joint state elliptical max_shrink", 1L
  )
  GG <- .rqr_expand_cube(GG, T, p, "GG")
  prior_mean <- .rqr_state_prior_mean(GG, m0, T)
  direction1 <- .rqr_zero_mean_state_prior_draw(
    GG, C0, W, jitter_ladder, numerical_policy
  )
  direction2 <- .rqr_zero_mean_state_prior_draw(
    GG, C0, W, jitter_ladder, numerical_policy
  )
  current_log_likelihood <- .rqr_joint_state_augmented_log_likelihood(
    theta1, theta2, FF, y, observed, v, xi, obs_variance
  )
  if (!is.finite(current_log_likelihood)) {
    stop("The current joint-state augmented likelihood is nonfinite.",
         call. = FALSE)
  }
  log_height <- current_log_likelihood + log(stats::runif(1L))
  angle <- stats::runif(1L, 0, 2 * pi)
  lower <- angle - 2 * pi
  upper <- angle
  # The path must be centered at its time-specific prior mean, not at m0.
  centered1 <- theta1 - prior_mean
  centered2 <- theta2 - prior_mean
  centered01 <- theta01 - m0
  centered02 <- theta02 - m0
  for (attempt in seq_len(max_shrink)) {
    cosine <- cos(angle)
    sine <- sin(angle)
    proposal1 <- prior_mean + centered1 * cosine +
      direction1$theta * sine
    proposal2 <- prior_mean + centered2 * cosine +
      direction2$theta * sine
    proposal01 <- m0 + centered01 * cosine + direction1$theta0 * sine
    proposal02 <- m0 + centered02 * cosine + direction2$theta0 * sine
    proposal_log_likelihood <- .rqr_joint_state_augmented_log_likelihood(
      proposal1, proposal2, FF, y, observed, v, xi, obs_variance
    )
    if (proposal_log_likelihood >= log_height) {
      repairs <- rbind(
        direction1$repair_records, direction2$repair_records
      )
      return(list(
        theta1 = proposal1, theta2 = proposal2,
        theta01 = proposal01, theta02 = proposal02,
        diagnostics = list(
          evaluations = attempt,
          shrink_steps = attempt - 1L,
          current_log_likelihood = current_log_likelihood,
          accepted_log_likelihood = proposal_log_likelihood,
          exact_joint_elliptical_slice = TRUE,
          repair_records = repairs
        )
      ))
    }
    if (angle < 0) lower <- angle else upper <- angle
    angle <- stats::runif(1L, lower, upper)
  }
  stop("The joint state elliptical slice shrink limit was reached.",
       call. = FALSE)
}

.rqr_component_noncentered_innovations <- function(
    theta, theta0, GG, evolution, q) {
  theta <- as.matrix(theta)
  p <- nrow(theta)
  T <- ncol(theta)
  theta0 <- as.numeric(theta0)
  q <- as.numeric(q)
  dims <- as.integer(evolution$component_dims)
  if (length(theta0) != p || sum(dims) != p ||
      length(q) != length(dims) ||
      any(!is.finite(c(theta, theta0, q))) || any(q <= 0)) {
    stop("The noncentered component-scale inputs are invalid.",
         call. = FALSE)
  }
  GG <- .rqr_expand_cube(GG, T, p, "GG")
  indices <- .rqr_component_indices(dims)
  standardized <- matrix(NA_real_, p, T)
  previous <- theta0
  for (tt in seq_len(T)) {
    innovation <- theta[, tt] - drop(GG[, , tt] %*% previous)
    for (j in seq_along(dims)) {
      standardized[indices[[j]], tt] <-
        innovation[indices[[j]]] / sqrt(q[[j]])
    }
    previous <- theta[, tt]
  }
  if (any(!is.finite(standardized))) {
    stop("The standardized component innovations are nonfinite.",
         call. = FALSE)
  }
  standardized
}

.rqr_reconstruct_component_path <- function(
    standardized, theta0, GG, evolution, q) {
  standardized <- as.matrix(standardized)
  p <- nrow(standardized)
  T <- ncol(standardized)
  theta0 <- as.numeric(theta0)
  q <- as.numeric(q)
  dims <- as.integer(evolution$component_dims)
  if (length(theta0) != p || sum(dims) != p ||
      length(q) != length(dims) ||
      any(!is.finite(c(standardized, theta0, q))) || any(q <= 0)) {
    stop("The component-path reconstruction inputs are invalid.",
         call. = FALSE)
  }
  GG <- .rqr_expand_cube(GG, T, p, "GG")
  indices <- .rqr_component_indices(dims)
  theta <- matrix(NA_real_, p, T)
  previous <- theta0
  for (tt in seq_len(T)) {
    innovation <- numeric(p)
    for (j in seq_along(dims)) {
      innovation[indices[[j]]] <-
        sqrt(q[[j]]) * standardized[indices[[j]], tt]
    }
    theta[, tt] <- drop(GG[, , tt] %*% previous) + innovation
    previous <- theta[, tt]
  }
  if (any(!is.finite(theta))) {
    stop("The reconstructed component path is nonfinite.",
         call. = FALSE)
  }
  theta
}

.rqr_component_path_basis <- function(
    standardized, theta0, GG, evolution) {
  standardized <- as.matrix(standardized)
  p <- nrow(standardized)
  T <- ncol(standardized)
  theta0 <- as.numeric(theta0)
  dims <- as.integer(evolution$component_dims)
  if (length(theta0) != p || sum(dims) != p ||
      any(!is.finite(c(standardized, theta0)))) {
    stop("The component path-basis inputs are invalid.", call. = FALSE)
  }
  GG <- .rqr_expand_cube(GG, T, p, "GG")
  indices <- .rqr_component_indices(dims)
  baseline <- matrix(NA_real_, p, T)
  basis <- array(0, c(p, T, length(dims)))
  previous_baseline <- theta0
  previous_basis <- matrix(0, p, length(dims))
  for (tt in seq_len(T)) {
    baseline[, tt] <- drop(GG[, , tt] %*% previous_baseline)
    for (j in seq_along(dims)) {
      current <- drop(GG[, , tt] %*% previous_basis[, j])
      current[indices[[j]]] <-
        current[indices[[j]]] + standardized[indices[[j]], tt]
      basis[, tt, j] <- current
      previous_basis[, j] <- current
    }
    previous_baseline <- baseline[, tt]
  }
  if (any(!is.finite(c(baseline, basis)))) {
    stop("The component path basis is nonfinite.", call. = FALSE)
  }
  list(baseline = baseline, basis = basis)
}

.rqr_component_path_from_basis <- function(path_basis, q) {
  q <- as.numeric(q)
  dimensions <- dim(path_basis$basis)
  if (!is.matrix(path_basis$baseline) ||
      length(dimensions) != 3L ||
      !identical(
        dim(path_basis$baseline),
        dimensions[1:2]
      ) ||
      length(q) != dimensions[[3L]] ||
      any(!is.finite(c(path_basis$baseline, path_basis$basis, q))) ||
      any(q <= 0)) {
    stop("The component path-basis reconstruction is invalid.",
         call. = FALSE)
  }
  output <- path_basis$baseline
  for (j in seq_along(q)) {
    output <- output + sqrt(q[[j]]) * path_basis$basis[, , j]
  }
  output
}

.rqr_component_ordinate_basis <- function(FF, path_basis) {
  FF <- as.matrix(FF)
  dimensions <- dim(path_basis$basis)
  if (!is.matrix(path_basis$baseline) ||
      length(dimensions) != 3L ||
      !identical(dim(FF), dim(path_basis$baseline)) ||
      !identical(dim(path_basis$baseline), dimensions[1:2]) ||
      any(!is.finite(c(FF, path_basis$baseline, path_basis$basis)))) {
    stop("The component ordinate-basis inputs are invalid.",
         call. = FALSE)
  }
  component_basis <- vapply(
    seq_len(dimensions[[3L]]),
    function(j) colSums(FF * path_basis$basis[, , j]),
    numeric(ncol(FF))
  )
  if (dimensions[[3L]] == 1L) {
    component_basis <- matrix(component_basis, ncol = 1L)
  }
  list(
    baseline = colSums(FF * path_basis$baseline),
    basis = component_basis
  )
}

.rqr_slice_log_coordinate <- function(
    current, log_density, width = 1, max_steps = 100L,
    max_shrink = 1000L) {
  current <- as.numeric(current)[1L]
  width <- as.numeric(width)[1L]
  max_steps <- .rqr_scalar_integer(
    max_steps, "component-scale slice max_steps", 1L
  )
  max_shrink <- .rqr_scalar_integer(
    max_shrink, "component-scale slice max_shrink", 1L
  )
  if (!is.finite(current) || !is.finite(width) || width <= 0) {
    stop("The component-scale slice inputs are invalid.", call. = FALSE)
  }
  evaluations <- 0L
  evaluate <- function(value) {
    evaluations <<- evaluations + 1L
    result <- as.numeric(log_density(value))[1L]
    if (is.na(result) || result == Inf) {
      stop("The component-scale slice log density is invalid.",
           call. = FALSE)
    }
    result
  }
  current_density <- evaluate(current)
  if (!is.finite(current_density)) {
    stop("The current component-scale slice density is nonfinite.",
         call. = FALSE)
  }
  slice_height <- current_density - stats::rexp(1L)
  offset <- stats::runif(1L)
  left <- current - width * offset
  right <- left + width
  remaining_left <- floor(max_steps * stats::runif(1L))
  remaining_right <- max_steps - 1L - remaining_left
  left_density <- evaluate(left)
  while (remaining_left > 0L && left_density > slice_height) {
    left <- left - width
    remaining_left <- remaining_left - 1L
    left_density <- evaluate(left)
  }
  right_density <- evaluate(right)
  while (remaining_right > 0L && right_density > slice_height) {
    right <- right + width
    remaining_right <- remaining_right - 1L
    right_density <- evaluate(right)
  }
  for (attempt in seq_len(max_shrink)) {
    proposal <- stats::runif(1L, left, right)
    proposal_density <- evaluate(proposal)
    if (proposal_density >= slice_height) {
      return(list(
        value = proposal,
        evaluations = evaluations,
        shrink_steps = attempt - 1L
      ))
    }
    if (proposal < current) {
      left <- proposal
    } else {
      right <- proposal
    }
  }
  stop("The component-scale slice shrink limit was reached.",
       call. = FALSE)
}

.rqr_component_noncentered_log_density <- function(
    log_q, ordinate_basis1, ordinate_basis2, y, observed, v, xi,
    obs_variance, evolution) {
  log_q <- as.numeric(log_q)
  candidate_q <- exp(log_q)
  if (length(log_q) != length(evolution$component_dims) ||
      any(!is.finite(candidate_q)) || any(candidate_q <= 0)) {
    return(-Inf)
  }
  eta1 <- as.numeric(
    ordinate_basis1$baseline +
      ordinate_basis1$basis %*% sqrt(candidate_q)
  )
  eta2 <- as.numeric(
    ordinate_basis2$baseline +
      ordinate_basis2$basis %*% sqrt(candidate_q)
  )
  augmented_residual <- rqr_residual_product(
    y[observed], eta1[observed], eta2[observed]
  ) - xi * v[observed]
  scaled_square <- augmented_residual^2 / obs_variance[observed]
  if (any(!is.finite(scaled_square))) {
    return(-Inf)
  }
  sum(
    -evolution$prior$shape * log_q -
      evolution$prior$rate / candidate_q
  ) - 0.5 * sum(scaled_square)
}

.rqr_interweave_component_scales <- function(
    theta1, theta2, theta01, theta02, GG, FF, y, observed, v,
    xi, obs_variance, evolution, q, width = 1, sweeps = 1L,
    max_steps = 100L, max_shrink = 1000L) {
  q <- as.numeric(q)
  log_q <- log(q)
  path_basis1 <- rqr_noncentered_basis_cpp(
    as.matrix(theta1), as.numeric(theta01),
    .rqr_expand_cube(GG, ncol(theta1), nrow(theta1), "GG"),
    as.integer(evolution$component_dims), q
  )
  path_basis2 <- rqr_noncentered_basis_cpp(
    as.matrix(theta2), as.numeric(theta02),
    .rqr_expand_cube(GG, ncol(theta2), nrow(theta2), "GG"),
    as.integer(evolution$component_dims), q
  )
  ordinate_basis1 <- .rqr_component_ordinate_basis(FF, path_basis1)
  ordinate_basis2 <- .rqr_component_ordinate_basis(FF, path_basis2)
  standardized1 <- path_basis1$standardized
  standardized2 <- path_basis2$standardized
  y <- as.numeric(y)
  observed <- as.logical(observed)
  v <- as.numeric(v)
  obs_variance <- as.numeric(obs_variance)
  if (length(y) != ncol(theta1) || length(observed) != length(y) ||
      length(v) != length(y) || length(obs_variance) != length(y) ||
      anyNA(observed) || any(!is.finite(y[observed])) ||
      any(!is.finite(c(v, obs_variance))) ||
      any(v <= 0) || any(obs_variance <= 0) ||
      !is.finite(xi)) {
    stop("The component-scale interweaving inputs are invalid.",
         call. = FALSE)
  }
  evaluate <- function(candidate_log_q) {
    .rqr_component_noncentered_log_density(
      candidate_log_q, ordinate_basis1, ordinate_basis2, y, observed,
      v, xi, obs_variance, evolution
    )
  }
  sweeps <- .rqr_scalar_integer(
    sweeps, "component-scale slice sweeps", 1L
  )
  evaluation_count <- shrink_count <- integer(length(q))
  for (sweep in seq_len(sweeps)) {
    for (j in seq_along(q)) {
      coordinate_density <- function(value) {
        candidate <- log_q
        candidate[[j]] <- value
        evaluate(candidate)
      }
      update <- .rqr_slice_log_coordinate(
        log_q[[j]], coordinate_density, width = width,
        max_steps = max_steps, max_shrink = max_shrink
      )
      log_q[[j]] <- update$value
      evaluation_count[[j]] <-
        evaluation_count[[j]] + update$evaluations
      shrink_count[[j]] <-
        shrink_count[[j]] + update$shrink_steps
    }
  }
  q <- exp(log_q)
  list(
    q = q,
    theta1 = .rqr_component_path_from_basis(path_basis1, q),
    theta2 = .rqr_component_path_from_basis(path_basis2, q),
    diagnostics = list(
      evaluations = evaluation_count,
      shrink_steps = shrink_count,
      sweeps = sweeps,
      exact_noncentered_slice = TRUE
    )
  )
}
