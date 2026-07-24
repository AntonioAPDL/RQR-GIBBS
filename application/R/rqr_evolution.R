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
