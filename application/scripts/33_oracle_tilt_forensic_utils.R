otf_schema <- function() "rqrgibbs_oracle_tilt_forensics/1.1.0"

otf_rbind_fill <- function(x) {
  x <- Filter(function(z) !is.null(z) && NROW(z) > 0L, x)
  if (!length(x)) return(data.frame())
  oti_rbind_fill(x)
}

otf_assert_flag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    oti_stop(name, " must be one nonmissing logical value.")
  }
  x
}

otf_assert_probability <- function(x, name) {
  oti_scalar(x, name, lower = 0, upper = 1)
}

otf_object_sha256 <- function(x) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    oti_stop("digest is required for forensic object contracts.")
  }
  digest::digest(x, algo = "sha256", serialize = TRUE)
}

otf_atomic_save_rds <- function(value, path, compress = FALSE) {
  oti_ensure_dir(dirname(path))
  temporary <- tempfile(
    paste0(".", basename(path), "-"),
    tmpdir = dirname(path)
  )
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  saveRDS(value, temporary, version = 3L, compress = compress)
  if (!file.rename(temporary, path)) {
    oti_stop("Could not atomically publish ", basename(path), ".")
  }
  invisible(path)
}

otf_atomic_write_csv <- function(value, path) {
  oti_ensure_dir(dirname(path))
  temporary <- tempfile(
    paste0(".", basename(path), "-"),
    tmpdir = dirname(path)
  )
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  write.csv(value, temporary, row.names = FALSE, na = "")
  if (!file.rename(temporary, path)) {
    oti_stop("Could not atomically publish ", basename(path), ".")
  }
  invisible(path)
}

otf_evolution_cube <- function(W, p, n_time) {
  W <- as.array(W)
  if (length(dim(W)) == 2L) {
    if (!all(dim(W) == c(p, p))) {
      oti_stop("The fixed evolution covariance must be p x p.")
    }
    out <- array(NA_real_, c(p, p, n_time))
    for (tt in seq_len(n_time)) out[, , tt] <- W
    return(out)
  }
  if (length(dim(W)) != 3L || !all(dim(W) == c(p, p, n_time))) {
    oti_stop("The fixed evolution covariance must be p x p or p x p x T.")
  }
  W
}

otf_expanded_dlm <- function(dgp) {
  expanded <- rqrgibbs:::.rqr_expand_model(
    rqrgibbs::rqr_as_dlm_model(dgp$model), length(dgp$y)
  )
  list(
    FF = expanded$FF,
    GG = expanded$GG,
    m0 = expanded$m0,
    C0 = expanded$C0,
    W = otf_evolution_cube(dgp$W, expanded$p, expanded$n_time),
    p = expanded$p,
    n_time = expanded$n_time
  )
}

otf_state_path_covariance <- function(GG, C0, W) {
  GG <- as.array(GG)
  C0 <- as.matrix(C0)
  W <- as.array(W)
  p <- nrow(C0)
  n_time <- dim(GG)[3L]
  if (!all(dim(GG) == c(p, p, n_time)) ||
      !all(dim(W) == c(p, p, n_time))) {
    oti_stop("GG, C0, and W have incompatible dimensions.")
  }
  out <- array(0, c(p, p, n_time, n_time))
  marginal <- vector("list", n_time)
  for (tt in seq_len(n_time)) {
    previous <- if (tt == 1L) C0 else marginal[[tt - 1L]]
    marginal[[tt]] <- GG[, , tt] %*% previous %*% t(GG[, , tt]) +
      W[, , tt]
    out[, , tt, tt] <- marginal[[tt]]
    if (tt > 1L) {
      for (ss in seq_len(tt - 1L)) {
        out[, , tt, ss] <- GG[, , tt] %*% out[, , tt - 1L, ss]
        out[, , ss, tt] <- t(out[, , tt, ss])
      }
    }
  }
  out
}

otf_prior_canonical_shift <- function(dgp, mean_tilt, coverage_level,
                                      learning_rate = 1) {
  expanded <- otf_expanded_dlm(dgp)
  observed <- is.finite(dgp$y)
  delta <- as.numeric(mean_tilt)
  if (length(delta) == 1L) delta <- rep(delta, expanded$n_time)
  if (length(delta) != expanded$n_time ||
      any(!is.finite(delta[observed])) ||
      any(!is.na(delta[!observed]))) {
    oti_stop("mean_tilt must match the DLM horizon and use NA at missing times.")
  }
  canonical <- matrix(0, expanded$p, expanded$n_time)
  canonical[, observed] <- sweep(
    expanded$FF[, observed, drop = FALSE],
    2L,
    learning_rate * coverage_level * delta[observed],
    `*`
  )
  covariance <- otf_state_path_covariance(
    expanded$GG, expanded$C0, expanded$W
  )
  state_shift <- matrix(0, expanded$p, expanded$n_time)
  for (tt in seq_len(expanded$n_time)) {
    for (ss in seq_len(expanded$n_time)) {
      state_shift[, tt] <- state_shift[, tt] +
        covariance[, , tt, ss] %*% canonical[, ss]
    }
  }
  transition_from_time0 <- diag(expanded$p)
  time0_state_shift <- numeric(expanded$p)
  for (ss in seq_len(expanded$n_time)) {
    transition_from_time0 <-
      expanded$GG[, , ss] %*% transition_from_time0
    time0_state_shift <- time0_state_shift +
      expanded$C0 %*% t(transition_from_time0) %*% canonical[, ss]
  }
  ordinate_shift <- colSums(expanded$FF * state_shift)
  list(
    canonical_shift = canonical,
    state_shift = state_shift,
    time0_state_shift = drop(time0_state_shift),
    ordinate_shift = ordinate_shift,
    covariance = covariance,
    expanded = expanded
  )
}

otf_dlm_with_initial_prior <- function(dgp, level_variance, slope_variance) {
  level_variance <- oti_scalar(
    level_variance, "level_variance", .Machine$double.eps
  )
  slope_variance <- oti_scalar(
    slope_variance, "slope_variance", .Machine$double.eps
  )
  out <- dgp
  out$model <- rqrgibbs::rqr_polytrend(
    order = 2L,
    m0 = c(0, 0),
    C0 = diag(c(level_variance, slope_variance)),
    name = "local_linear"
  )
  out$initial_level_variance <- level_variance
  out$initial_slope_variance <- slope_variance
  out
}

otf_tilt_geometry <- function(law, coverage_level, oracle_targets,
                              caution_fraction = 0.02) {
  caution_fraction <- otf_assert_probability(
    caution_fraction, "caution_fraction"
  )
  lower_boundary <- oti_interval_from_u(law, 0, coverage_level)
  upper_boundary <- oti_interval_from_u(
    law, 1 - coverage_level, coverage_level
  )
  delta_minus <- lower_boundary$delta_innovation
  delta_plus <- upper_boundary$delta_innovation
  span <- delta_plus - delta_minus
  if (!is.finite(span) || span <= 0) {
    oti_stop("The population tilt range is not finite and increasing.")
  }
  rows <- lapply(seq_len(nrow(oracle_targets)), function(ii) {
    delta <- oracle_targets$delta_innovation[ii]
    lower_margin <- delta - delta_minus
    upper_margin <- delta_plus - delta
    data.frame(
      target = oracle_targets$target[ii],
      coverage_level = coverage_level,
      u = oracle_targets$u[ii],
      delta = delta,
      delta_minus = delta_minus,
      delta_plus = delta_plus,
      lower_margin = lower_margin,
      upper_margin = upper_margin,
      lower_margin_fraction = lower_margin / span,
      upper_margin_fraction = upper_margin / span,
      minimum_margin_fraction = min(lower_margin, upper_margin) / span,
      caution_fraction = caution_fraction,
      near_population_boundary =
        min(lower_margin, upper_margin) / span < caution_fraction,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

otf_prior_shift_summary <- function(dgp, targets_by_index, oracle_targets,
                                    coverage_level, learning_rate = 1) {
  rows <- lapply(oracle_targets$target, function(target) {
    truth <- oti_target_row(targets_by_index, target)
    shift <- otf_prior_canonical_shift(
      dgp, truth$mean_tilt, coverage_level, learning_rate
    )
    oracle_width <- mean(truth$oracle_width[truth$observed])
    response_scale <- stats::sd(dgp$y[is.finite(dgp$y)])
    data.frame(
      target = target,
      coverage_level = coverage_level,
      delta_innovation =
        oracle_targets$delta_innovation[oracle_targets$target == target],
      delta_response = unique(truth$mean_tilt[truth$observed])[1L],
      terminal_ordinate_shift =
        shift$ordinate_shift[length(shift$ordinate_shift)],
      maximum_absolute_ordinate_shift = max(abs(shift$ordinate_shift)),
      ordinate_shift_l2 = sqrt(sum(shift$ordinate_shift^2)),
      oracle_mean_width = oracle_width,
      response_sd = response_scale,
      terminal_shift_over_oracle_width =
        shift$ordinate_shift[length(shift$ordinate_shift)] / oracle_width,
      max_shift_over_response_sd =
        max(abs(shift$ordinate_shift)) / response_scale,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

otf_interval_from_delta <- function(law, coverage_level, delta) {
  upper_u <- 1 - coverage_level
  balance <- function(u) {
    oti_interval_from_u(law, u, coverage_level)$delta_innovation - delta
  }
  f0 <- balance(0)
  f1 <- balance(upper_u)
  tolerance <- 1e-10
  if (delta < f0 + delta - tolerance || delta > f1 + delta + tolerance) {
    oti_stop("delta is outside the finite population tilt path.")
  }
  if (abs(f0) <= tolerance) {
    u <- 0
  } else if (abs(f1) <= tolerance) {
    u <- upper_u
  } else {
    u <- stats::uniroot(balance, c(0, upper_u), tol = 1e-12)$root
  }
  oti_interval_from_u(law, u, coverage_level)
}

otf_direction_profile <- function(dgp, truth, coverage_level,
                                  learning_rate = 1,
                                  slope_grid = NULL) {
  expanded <- otf_expanded_dlm(dgp)
  if (expanded$p != 2L) {
    oti_stop("The escaping-direction profile currently requires p=2.")
  }
  n_time <- expanded$n_time
  observed <- is.finite(dgp$y)
  lower <- truth$oracle_lower
  upper0 <- truth$oracle_upper
  state_slope <- dgp$state_truth[, 2L]
  theta_lower <- rbind(lower, state_slope)
  theta0_lower <- matrix(
    c(lower[1L] - state_slope[1L], state_slope[1L]),
    ncol = 1L
  )
  prior_for_slope <- function(slope) {
    theta_upper <- array(
      rbind(upper0 + slope * seq_len(n_time), state_slope + slope),
      dim = c(expanded$p, n_time, 1L)
    )
    theta0_upper <- matrix(
      c(
        upper0[1L] - state_slope[1L],
        state_slope[1L] + slope
      ),
      ncol = 1L
    )
    lower_value <- otf_root_prior_quadratic(
      array(theta_lower, dim = c(expanded$p, n_time, 1L)),
      theta0_lower, expanded
    )
    upper_value <- otf_root_prior_quadratic(
      theta_upper, theta0_upper, expanded
    )
    lower_value + upper_value
  }
  if (is.null(slope_grid)) {
    slope_grid <- unique(c(
      seq(-0.25, 1, length.out = 51L),
      seq(2, 50, length.out = 49L),
      seq(60, 500, length.out = 45L)
    ))
  }
  slope_grid <- sort(as.numeric(slope_grid))
  slope_grid <- slope_grid[is.finite(slope_grid)]
  direction <- seq_len(n_time)
  rows <- lapply(slope_grid, function(slope) {
    upper <- upper0 + slope * direction
    ordinary <- sum(rqrgibbs::rqr_check_loss(
      rqrgibbs::rqr_residual_product(
        dgp$y[observed], lower[observed], upper[observed]
      ),
      coverage_level
    ))
    tilt <- -coverage_level * sum(
      truth$mean_tilt[observed] *
        (lower[observed] + upper[observed] - 2 * dgp$y[observed])
    )
    prior_quadratic <- prior_for_slope(slope)
    data.frame(
      slope = slope,
      terminal_upper = upper[n_time],
      ordinary_loss = ordinary,
      tilt_contribution = tilt,
      target_loss = ordinary + tilt,
      prior_quadratic = prior_quadratic,
      negative_log_target =
        learning_rate * (ordinary + tilt) + 0.5 * prior_quadratic,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  optimum <- stats::optimize(
    function(slope) {
      upper <- upper0 + slope * direction
      ordinary <- sum(rqrgibbs::rqr_check_loss(
        rqrgibbs::rqr_residual_product(
          dgp$y[observed], lower[observed], upper[observed]
        ),
        coverage_level
      ))
      tilt <- -coverage_level * sum(
        truth$mean_tilt[observed] *
          (lower[observed] + upper[observed] - 2 * dgp$y[observed])
      )
      learning_rate * (ordinary + tilt) + 0.5 * prior_for_slope(slope)
    },
    interval = range(slope_grid)
  )
  attr(out, "optimum") <- data.frame(
    optimum_slope = optimum$minimum,
    optimum_terminal_upper =
      upper0[n_time] + optimum$minimum * n_time,
    optimum_negative_log_target = optimum$objective,
    stringsAsFactors = FALSE
  )
  out
}

otf_initial_state_paths <- function(profile, dgp, truth,
                                    coverage_level = NULL,
                                    learning_rate = 1) {
  profile <- match.arg(
    profile,
    c(
      "default", "oracle", "oracle_centered", "narrow", "wide",
      "slope_stress", "prior_shift_stress"
    )
  )
  if (identical(profile, "oracle_centered")) profile <- "oracle"
  if (identical(profile, "default")) return(list())
  p <- ncol(dgp$state_truth)
  n_time <- nrow(dgp$state_truth)
  if (p != 2L) oti_stop("Forensic initial profiles currently require p=2.")
  midpoint <- 0.5 * (truth$oracle_lower + truth$oracle_upper)
  half_width <- 0.5 * truth$oracle_width
  multiplier <- switch(
    profile,
    oracle = 1,
    narrow = 0.25,
    wide = 2.5,
    slope_stress = 1,
    prior_shift_stress = 1
  )
  lower <- midpoint - multiplier * half_width
  upper <- midpoint + multiplier * half_width
  slope <- dgp$state_truth[, 2L]
  theta1 <- rbind(lower, slope)
  theta2 <- rbind(upper, slope)
  stopifnot(all(dim(theta1) == c(p, n_time)))
  out <- list(
    state_root1 = theta1,
    state_root2 = theta2,
    theta0_root1 = c(lower[1L] - slope[1L], slope[1L]),
    theta0_root2 = c(upper[1L] - slope[1L], slope[1L])
  )
  if (identical(profile, "slope_stress")) {
    slope_offset <- 2 * sqrt(dgp$initial_slope_variance)
    time_index <- seq_len(n_time)
    out$theta0_root2[2L] <- out$theta0_root2[2L] + slope_offset
    out$state_root2[2L, ] <- out$state_root2[2L, ] + slope_offset
    out$state_root2[1L, ] <-
      out$state_root2[1L, ] + slope_offset * time_index
  }
  if (identical(profile, "prior_shift_stress")) {
    if (is.null(coverage_level)) {
      oti_stop(
        "coverage_level is required for the prior_shift_stress profile."
      )
    }
    shift <- otf_prior_canonical_shift(
      dgp, truth$mean_tilt, coverage_level, learning_rate
    )
    out$state_root2 <- out$state_root2 + shift$state_shift
    out$theta0_root2 <- out$theta0_root2 + shift$time0_state_shift
  }
  out
}

otf_root_prior_quadratic <- function(theta, theta0, expanded) {
  theta <- as.array(theta)
  theta0 <- as.matrix(theta0)
  p <- expanded$p
  n_time <- expanded$n_time
  n_draw <- dim(theta)[3L]
  if (!all(dim(theta) == c(p, n_time, n_draw)) ||
      !all(dim(theta0) == c(p, n_draw))) {
    oti_stop("State and time-zero draw dimensions are incompatible.")
  }
  C0_inv <- solve(expanded$C0)
  W_inv <- lapply(seq_len(n_time), function(tt) solve(expanded$W[, , tt]))
  d0 <- sweep(theta0, 1L, expanded$m0, `-`)
  out <- colSums(d0 * (C0_inv %*% d0))
  previous <- theta0
  for (tt in seq_len(n_time)) {
    current <- matrix(theta[, tt, ], nrow = p, ncol = n_draw)
    innovation <-
      current - expanded$GG[, , tt] %*% previous
    out <- out + colSums(
      innovation * (W_inv[[tt]] %*% innovation)
    )
    previous <- current
  }
  out
}

otf_trace_indices <- function(n_time, observed) {
  requested <- c(1L, 34L, 35L, 36L, 51L, 70L, 71L, n_time)
  unique(requested[requested >= 1L & requested <= n_time])
}

otf_dlm_trace_frame <- function(fit, dgp, truth, target, chain, profile,
                                coverage_level, learning_rate = 1) {
  if (is.null(fit$samp.theta_root1) || is.null(fit$samp.theta_root2) ||
      is.null(fit$samp.theta0_root1) || is.null(fit$samp.theta0_root2)) {
    oti_stop("Forensic DLM traces require stored state and time-zero draws.")
  }
  if (is.null(fit$samp.latent_v)) {
    oti_stop("Forensic DLM traces require stored latent-scale draws.")
  }
  expanded <- otf_expanded_dlm(dgp)
  eta1 <- as.matrix(fit$samp.eta_root1)
  eta2 <- as.matrix(fit$samp.eta_root2)
  n_draw <- ncol(eta1)
  observed <- is.finite(dgp$y)
  lower <- pmin(eta1, eta2)
  upper <- pmax(eta1, eta2)
  ordinary <- vapply(seq_len(n_draw), function(jj) {
    sum(rqrgibbs::rqr_check_loss(
      rqrgibbs::rqr_residual_product(
        dgp$y[observed], eta1[observed, jj], eta2[observed, jj]
      ),
      coverage_level
    ))
  }, numeric(1L))
  tilt <- vapply(seq_len(n_draw), function(jj) {
    -coverage_level * sum(
      truth$mean_tilt[observed] *
        (
          eta1[observed, jj] + eta2[observed, jj] -
            2 * dgp$y[observed]
        )
    )
  }, numeric(1L))
  prior1 <- otf_root_prior_quadratic(
    fit$samp.theta_root1, fit$samp.theta0_root1, expanded
  )
  prior2 <- otf_root_prior_quadratic(
    fit$samp.theta_root2, fit$samp.theta0_root2, expanded
  )
  root_swap <- tail(fit$diagnostics$root_swap_trace, n_draw)
  out <- data.frame(
    target = target,
    chain = chain,
    profile = profile,
    draw = seq_len(n_draw),
    ordinary_loss = ordinary,
    tilt_contribution = tilt,
    target_loss = ordinary + tilt,
    root1_prior_quadratic = prior1,
    root2_prior_quadratic = prior2,
    prior_quadratic = prior1 + prior2,
    negative_log_target =
      learning_rate * (ordinary + tilt) + 0.5 * (prior1 + prior2),
    maximum_absolute_root_ordinate =
      pmax(apply(abs(eta1), 2L, max), apply(abs(eta2), 2L, max)),
    maximum_width = apply(upper - lower, 2L, max),
    root_swap = as.logical(root_swap),
    stringsAsFactors = FALSE
  )
  selected <- otf_trace_indices(expanded$n_time, observed)
  for (tt in selected) {
    suffix <- sprintf("t%03d", tt)
    out[[paste0("lower_", suffix)]] <- lower[tt, ]
    out[[paste0("upper_", suffix)]] <- upper[tt, ]
    out[[paste0("midpoint_", suffix)]] <- 0.5 * (lower[tt, ] + upper[tt, ])
    out[[paste0("width_", suffix)]] <- upper[tt, ] - lower[tt, ]
    out[[paste0("root1_level_", suffix)]] <-
      fit$samp.theta_root1[1L, tt, ]
    out[[paste0("root2_level_", suffix)]] <-
      fit$samp.theta_root2[1L, tt, ]
    out[[paste0("root1_slope_", suffix)]] <-
      fit$samp.theta_root1[2L, tt, ]
    out[[paste0("root2_slope_", suffix)]] <-
      fit$samp.theta_root2[2L, tt, ]
    out[[paste0("latent_v_", suffix)]] <- fit$samp.latent_v[tt, ]
  }
  for (kk in seq_len(expanded$p)) {
    out[[paste0("root1_time0_state", kk)]] <- fit$samp.theta0_root1[kk, ]
    out[[paste0("root2_time0_state", kk)]] <- fit$samp.theta0_root2[kk, ]
    out[[paste0("root1_terminal_state", kk)]] <-
      fit$samp.theta_terminal_root1[kk, ]
    out[[paste0("root2_terminal_state", kk)]] <-
      fit$samp.theta_terminal_root2[kk, ]
  }
  out
}

otf_dense_gaussian_reference <- function(z, H, V, expanded,
                                         canonical_shift) {
  p <- expanded$p
  n_time <- expanded$n_time
  index <- function(tt) ((tt - 1L) * p + 1L):(tt * p)
  precision <- matrix(0, p * n_time, p * n_time)
  information <- numeric(p * n_time)
  for (tt in seq_len(n_time)) {
    ii <- index(tt)
    Gt <- expanded$GG[, , tt]
    if (tt == 1L) {
      Rt <- Gt %*% expanded$C0 %*% t(Gt) + expanded$W[, , tt]
      Rt_inv <- solve(Rt)
      prior_mean <- drop(Gt %*% expanded$m0)
      precision[ii, ii] <- precision[ii, ii] + Rt_inv
      information[ii] <- information[ii] + drop(Rt_inv %*% prior_mean)
    } else {
      jj <- index(tt - 1L)
      W_inv <- solve(expanded$W[, , tt])
      precision[jj, jj] <- precision[jj, jj] + t(Gt) %*% W_inv %*% Gt
      precision[ii, ii] <- precision[ii, ii] + W_inv
      precision[jj, ii] <- precision[jj, ii] - t(Gt) %*% W_inv
      precision[ii, jj] <- precision[ii, jj] - W_inv %*% Gt
    }
    if (!is.na(z[tt])) {
      ht <- H[, tt]
      precision[ii, ii] <- precision[ii, ii] + tcrossprod(ht) / V[tt]
      information[ii] <- information[ii] + ht * z[tt] / V[tt]
    }
    information[ii] <- information[ii] + canonical_shift[, tt]
  }
  precision <- 0.5 * precision + 0.5 * t(precision)
  precision_chol <- chol(precision)
  mean <- drop(backsolve(
    precision_chol,
    forwardsolve(t(precision_chol), information)
  ))
  covariance <- chol2inv(precision_chol)
  residual <- drop(precision %*% mean - information)
  list(
    mean = matrix(mean, p, n_time),
    covariance = covariance,
    precision = precision,
    information = information,
    reciprocal_condition = rcond(precision),
    maximum_absolute_normalized_residual =
      max(abs(residual)) / max(1, max(abs(information)))
  )
}

otf_conditional_reference <- function(fit, dgp, truth, coverage_level,
                                      draw = NULL) {
  expanded <- otf_expanded_dlm(dgp)
  observed <- is.finite(dgp$y)
  n_draw <- ncol(fit$samp.eta_root1)
  draw <- as.integer(draw %||% ceiling(n_draw / 2))
  if (length(draw) != 1L || is.na(draw) || draw < 1L || draw > n_draw) {
    oti_stop("draw is outside the retained range.")
  }
  constants <- rqrgibbs::rqr_constants(coverage_level, 1)
  latent_v <- fit$samp.latent_v[, draw]
  V <- constants$phi * latent_v
  canonical <- matrix(0, expanded$p, expanded$n_time)
  canonical[, observed] <- sweep(
    expanded$FF[, observed, drop = FALSE],
    2L,
    coverage_level * truth$mean_tilt[observed],
    `*`
  )
  evolution <- rqrgibbs::rqr_evolution_fixed(expanded$W)
  rows <- list()
  for (root in c("root1", "root2")) {
    other <- if (root == "root1") {
      fit$samp.eta_root2[, draw]
    } else {
      fit$samp.eta_root1[, draw]
    }
    H <- sweep(expanded$FF, 2L, dgp$y - other, `*`)
    H[, !observed] <- 0
    z <- dgp$y * (dgp$y - other) - constants$xi * latent_v
    z[!observed] <- NA_real_
    dense <- otf_dense_gaussian_reference(z, H, V, expanded, canonical)
    smooth_R <- rqrgibbs::rqr_ffbs_smooth(
      z, H, V, expanded$GG, expanded$m0, expanded$C0, evolution,
      backend = "R", numerical_policy = "fail",
      canonical_shift = canonical
    )
    smooth_cpp <- rqrgibbs::rqr_ffbs_smooth(
      z, H, V, expanded$GG, expanded$m0, expanded$C0, evolution,
      backend = "cpp", numerical_policy = "fail",
      canonical_shift = canonical
    )
    dense_blocks <- array(NA_real_, c(expanded$p, expanded$p, expanded$n_time))
    for (tt in seq_len(expanded$n_time)) {
      ii <- ((tt - 1L) * expanded$p + 1L):(tt * expanded$p)
      dense_blocks[, , tt] <- dense$covariance[ii, ii]
    }
    mean_scale <- max(1, max(abs(dense$mean)))
    covariance_scale <- max(1, max(abs(dense_blocks)))
    R_dense_mean_error <- max(abs(smooth_R$smooth_mean - dense$mean))
    cpp_dense_mean_error <- max(abs(smooth_cpp$smooth_mean - dense$mean))
    R_cpp_mean_error <-
      max(abs(smooth_R$smooth_mean - smooth_cpp$smooth_mean))
    R_dense_covariance_error <-
      max(abs(smooth_R$smooth_cov - dense_blocks))
    cpp_dense_covariance_error <-
      max(abs(smooth_cpp$smooth_cov - dense_blocks))
    rows[[root]] <- data.frame(
      root = root,
      draw = draw,
      dense_mean_scale = mean_scale,
      dense_marginal_covariance_scale = covariance_scale,
      dense_precision_reciprocal_condition = dense$reciprocal_condition,
      dense_maximum_absolute_normalized_residual =
        dense$maximum_absolute_normalized_residual,
      max_R_dense_mean_absolute_error = R_dense_mean_error,
      max_cpp_dense_mean_absolute_error = cpp_dense_mean_error,
      max_R_cpp_mean_absolute_error = R_cpp_mean_error,
      max_R_dense_mean_relative_error =
        R_dense_mean_error / mean_scale,
      max_cpp_dense_mean_relative_error =
        cpp_dense_mean_error / mean_scale,
      max_R_dense_marginal_covariance_absolute_error =
        R_dense_covariance_error,
      max_cpp_dense_marginal_covariance_absolute_error =
        cpp_dense_covariance_error,
      max_R_dense_marginal_covariance_relative_error =
        R_dense_covariance_error / covariance_scale,
      max_cpp_dense_marginal_covariance_relative_error =
        cpp_dense_covariance_error / covariance_scale,
      R_repair_count = smooth_R$diagnostics$repair_count,
      cpp_repair_count = smooth_cpp$diagnostics$repair_count,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

otf_conditional_reference_pass <- function(reference,
                                           relative_tolerance = 1e-7,
                                           R_cpp_absolute_tolerance = 1e-10) {
  relative_tolerance <- oti_scalar(
    relative_tolerance, "relative_tolerance", 0
  )
  R_cpp_absolute_tolerance <- oti_scalar(
    R_cpp_absolute_tolerance, "R_cpp_absolute_tolerance", 0
  )
  relative_columns <- grep(
    "_relative_error$", names(reference), value = TRUE
  )
  length(relative_columns) > 0L &&
    all(is.finite(as.matrix(
      reference[, relative_columns, drop = FALSE]
    ))) &&
    max(as.matrix(reference[, relative_columns, drop = FALSE])) <=
      relative_tolerance &&
    all(reference$max_R_cpp_mean_absolute_error <=
          R_cpp_absolute_tolerance) &&
    all(reference$R_repair_count == 0L) &&
    all(reference$cpp_repair_count == 0L)
}

otf_scale_pathology_summary <- function(pred, dgp, truth, trace,
                                        width_ratio_threshold = 20,
                                        endpoint_sd_threshold = 20) {
  width_ratio_threshold <- oti_scalar(
    width_ratio_threshold, "width_ratio_threshold", 1
  )
  endpoint_sd_threshold <- oti_scalar(
    endpoint_sd_threshold, "endpoint_sd_threshold", 1
  )
  response_sd <- stats::sd(dgp$y[is.finite(dgp$y)])
  oracle_width <- mean(truth$oracle_width[truth$observed])
  endpoint_scale <- pmax(
    apply(abs(pred$lower_draws), 2L, max),
    apply(abs(pred$upper_draws), 2L, max)
  ) / response_sd
  width_scale <- apply(pred$width_draws, 2L, max) / oracle_width
  remote <- endpoint_scale > endpoint_sd_threshold |
    width_scale > width_ratio_threshold
  data.frame(
    response_sd = response_sd,
    pathology_oracle_mean_width = oracle_width,
    maximum_endpoint_over_response_sd = max(endpoint_scale),
    median_maximum_width_over_oracle_width = stats::median(width_scale),
    maximum_width_over_oracle_width = max(width_scale),
    remote_draw_fraction = mean(remote),
    width_ratio_threshold = width_ratio_threshold,
    endpoint_sd_threshold = endpoint_sd_threshold,
    minimum_negative_log_target = min(trace$negative_log_target),
    median_negative_log_target = stats::median(trace$negative_log_target),
    stringsAsFactors = FALSE
  )
}

otf_trace_summary <- function(trace) {
  data.frame(
    target = trace$target[1L],
    chain = trace$chain[1L],
    profile = trace$profile[1L],
    n_draws = nrow(trace),
    median_target_loss = stats::median(trace$target_loss),
    median_prior_quadratic = stats::median(trace$prior_quadratic),
    median_negative_log_target = stats::median(trace$negative_log_target),
    median_maximum_width = stats::median(trace$maximum_width),
    q975_maximum_width = as.numeric(stats::quantile(
      trace$maximum_width, 0.975, names = FALSE, type = 8
    )),
    maximum_absolute_root_ordinate =
      max(trace$maximum_absolute_root_ordinate),
    root_swap_fraction = mean(trace$root_swap),
    stringsAsFactors = FALSE
  )
}

otf_trace_diagnostic_matrix <- function(trace) {
  required <- c(
    "target_loss",
    "prior_quadratic",
    "negative_log_target",
    "maximum_width"
  )
  selected <- c(
    required,
    grep(
      "^(lower|upper|midpoint|width)_t[0-9]+$",
      names(trace),
      value = TRUE
    )
  )
  missing <- setdiff(required, names(trace))
  if (length(missing)) {
    oti_stop(
      "Forensic trace is missing diagnostic fields: ",
      paste(missing, collapse = ", "),
      "."
    )
  }
  out <- as.matrix(trace[, selected, drop = FALSE])
  storage.mode(out) <- "double"
  out
}
