.rqr_state_ordinates <- function(FF, path) colSums(FF * path)

.rqr_init_state_paths <- function(y, FF, m0, coverage_level, init = list()) {
  p <- nrow(FF)
  T <- ncol(FF)
  supplied1 <- init$state_root1 %||% init$theta1 %||% NULL
  supplied2 <- init$state_root2 %||% init$theta2 %||% NULL
  if (!is.null(supplied1) || !is.null(supplied2)) {
    if (is.null(supplied1) || is.null(supplied2)) stop("init must provide both state paths.", call. = FALSE)
    supplied1 <- as.matrix(supplied1)
    supplied2 <- as.matrix(supplied2)
    if (!all(dim(supplied1) == c(p, T)) || !all(dim(supplied2) == c(p, T)) ||
        any(!is.finite(supplied1)) || any(!is.finite(supplied2))) {
      stop("Initial state paths must be finite p x T matrices.", call. = FALSE)
    }
    return(list(theta1 = supplied1, theta2 = supplied2))
  }
  observed <- !is.na(y)
  alpha <- rqr_constants(coverage_level)$alpha
  probs <- c((1 - alpha) / 2, 1 - (1 - alpha) / 2)
  qs <- as.numeric(stats::quantile(y[observed], probs = probs, names = FALSE, type = 8))
  Fobs <- FF[, observed, drop = FALSE]
  base <- drop(crossprod(Fobs, m0))
  precision <- tcrossprod(Fobs) + diag(1e-6, p)
  correction <- function(target) {
    rhs <- Fobs %*% rep(target, ncol(Fobs)) - Fobs %*% base
    m0 + solve(precision, rhs)
  }
  b1 <- correction(qs[1L])
  b2 <- correction(qs[2L])
  list(
    theta1 = matrix(rep(b1, T), p, T),
    theta2 = matrix(rep(b2, T), p, T)
  )
}

.rqr_dlm_evolution <- function(
    mode, model, expanded, y, W, df, dim.df, reference_variance,
    reference_design, component_templates, evolution_scale_prior,
    evolution_scale_initial, evolution_spec, numerical_policy, jitter_ladder) {
  if (!is.null(evolution_spec)) {
    if (!inherits(evolution_spec, "rqr_evolution") || is.null(evolution_spec$mode)) {
      stop("evolution_spec must be an rqr_evolution object.", call. = FALSE)
    }
    mode <- as.character(evolution_spec$mode)[1L]
    if (!mode %in% c("fixed_W", "discount_template", "component_scale", "adaptive_discount")) {
      stop("evolution_spec has an unsupported mode.", call. = FALSE)
    }
    if (identical(mode, "component_scale") &&
        !identical(as.integer(evolution_spec$component_dims), as.integer(expanded$component_dims))) {
      stop("component_scale dimensions must match model$component_dims.", call. = FALSE)
    }
    return(evolution_spec)
  }
  mode <- match.arg(
    mode, c("fixed_W", "discount_template", "component_scale", "adaptive_discount")
  )
  p <- expanded$p
  T <- expanded$n_time
  if (mode == "fixed_W") {
    if (is.null(W)) stop("evolution_mode='fixed_W' requires W.", call. = FALSE)
    return(rqr_evolution_fixed(.rqr_expand_cube(W, T, p, "W")))
  }
  if (mode == "component_scale") {
    if (is.null(component_templates)) {
      stop("evolution_mode='component_scale' requires component_templates.", call. = FALSE)
    }
    return(rqr_evolution_component_scale(
      templates = component_templates,
      component_dims = expanded$component_dims,
      component_names = expanded$component_names,
      prior = evolution_scale_prior,
      initial = evolution_scale_initial
    ))
  }
  if (is.null(dim.df)) dim.df <- model$component_dims %||% p
  if (is.null(df)) stop("Discount evolution requires df.", call. = FALSE)
  if (mode == "adaptive_discount") {
    return(rqr_evolution_adaptive_working(df, dim.df))
  }
  reference_source <- if (is.null(reference_variance)) {
    "training_response_variance"
  } else {
    "user_supplied"
  }
  if (is.null(reference_variance)) {
    yy <- y[!is.na(y)]
    empirical_variance <- stats::var(yy)
    if (!is.finite(empirical_variance) || empirical_variance <= 0) empirical_variance <- 1
    reference_variance <- max(empirical_variance, sqrt(.Machine$double.eps))
  }
  template <- rqr_freeze_discount_template(
    model = model, n_time = T, df = df, dim.df = dim.df,
    reference_variance = reference_variance,
    reference_design = reference_design,
    numerical_policy = numerical_policy,
    jitter_ladder = jitter_ladder
  )
  template$reference_source <- reference_source
  template$empirical_bayes <- identical(reference_source, "training_response_variance")
  template
}

.rqr_bind_ffbs_repairs <- function(records, diagnostics, iteration, root) {
  current <- diagnostics$repair_records
  if (is.null(current) || !nrow(current)) return(records)
  current$iteration <- as.integer(iteration)
  current$root <- as.character(root)
  current <- current[, c(
    "iteration", "root", "stage", "time", "strategy", "jitter",
    "relative_jitter", "min_eigenvalue", "matrix_scale", "jitter_scale",
    "absolute_jitter_fallback", "clamped_eigenvalues"
  )]
  if (is.null(records)) current else rbind(records, current)
}

.rqr_dlm_provenance_matrices <- function(expanded, evolution) {
  list(
    FF = expanded$FF,
    GG = expanded$GG,
    C0 = expanded$C0,
    evolution_W = evolution$W %||% NULL,
    evolution_templates = evolution$templates %||% NULL,
    evolution_discount = evolution$D %||% NULL
  )
}

.rqr_dlm_model_contract <- function(expanded) {
  list(
    FF = expanded$FF,
    GG = expanded$GG,
    m0 = expanded$m0,
    C0 = expanded$C0,
    component_dims = expanded$component_dims,
    component_names = expanded$component_names,
    state_dimension = expanded$p,
    n_time = expanded$n_time
  )
}

.rqr_dlm_target_contract <- function(
    coverage_level, learning_rate_mode, fixed_learning_rate,
    loss_reference_scale, lambda_prior, numerical_policy, jitter_ladder) {
  list(
    loss_name = "rqr_residual_product_check_loss",
    coverage_level = coverage_level,
    learning_rate_mode = learning_rate_mode,
    fixed_learning_rate = fixed_learning_rate,
    loss_reference_scale = loss_reference_scale,
    lambda_prior = lambda_prior,
    numerical_policy = numerical_policy,
    jitter_ladder = as.numeric(jitter_ladder),
    root_priors_exchangeable = TRUE,
    root_swap_move = TRUE
  )
}

.rqr_dlm_evolution_contract <- function(evolution) {
  unclass(evolution)
}

.rqr_dlm_provenance_objects <- function(expanded, evolution, target_contract) {
  list(
    model = .rqr_dlm_model_contract(expanded),
    target = target_contract,
    evolution = .rqr_dlm_evolution_contract(evolution)
  )
}

.rqr_dlm_coverage_summary <- function(y, observed, lower, upper) {
  lower_mean <- rowMeans(lower)
  upper_mean <- rowMeans(upper)
  lower_observed <- lower[observed, , drop = FALSE]
  upper_observed <- upper[observed, , drop = FALSE]
  y_observed <- y[observed]
  covered <- sweep(lower_observed, 1L, y_observed, `<=`) &
    sweep(upper_observed, 1L, y_observed, `>=`)
  coverage_by_draw <- colMeans(covered)
  list(
    lower_mean = lower_mean,
    upper_mean = upper_mean,
    midpoint_mean = rowMeans(0.5 * (lower + upper)),
    width_mean = rowMeans(upper - lower),
    coverage_posterior_mean_endpoints = mean(
      y_observed >= lower_mean[observed] & y_observed <= upper_mean[observed]
    ),
    coverage_draw_mean = mean(coverage_by_draw),
    coverage_draw_quantiles = stats::quantile(
      coverage_by_draw, c(0.05, 0.5, 0.95), names = TRUE, type = 8
    ),
    width_mean_scalar = mean(rowMeans(upper - lower))
  )
}

#' Fit a dynamic RQR interval-root model with alternating FFBS
#'
#' This sampler targets root trajectories under an exponentiated RQR loss and
#' Gaussian state priors. It does not define a response likelihood. Fixed `W`,
#' frozen discount templates, and component-scale evolution priors define exact
#' generalized-posterior samplers when no numerical repair is used. Adaptive
#' discount recursion is an explicitly experimental working update.
#'
#' @param y Response vector. `NA` values are treated as missing observations.
#' @param model An [rqr_as_dlm_model()] or exdqlm-compatible model.
#' @param coverage_level Interval coverage target in `(0,1)`.
#' @param evolution_mode One of `"fixed_W"`, `"discount_template"`,
#'   `"component_scale"`, or `"adaptive_discount"`.
#' @param evolution_spec Optional previously constructed evolution object. Used
#'   for exact continuation and overrides the other evolution arguments.
#' @param W Fixed evolution covariance matrix or cube.
#' @param df,dim.df Component discounts and their state dimensions.
#' @param reference_variance,reference_design Inputs used to freeze a discount
#'   template before MCMC.
#' @param component_templates Fixed SPD templates for exact component scales.
#' @param evolution_scale_prior Inverse-Gamma shape and rate for component
#'   evolution multipliers.
#' @param evolution_scale_initial Initial positive component multipliers.
#' @param learning_rate Fixed generalized-Bayes learning rate `omega_R`.
#' @param lambda_initial Initial inverse loss scale for learned modes.
#' @param loss_reference_scale Positive reference scale `s_L`. It does not
#'   alter fixed `learning_rate`; learned modes use `lambda/s_L`.
#' @param learning_rate_mode One of `"fixed_rate"`,
#'   `"learned_pseudoresidual_normalized"`, or `"learned_pure"`. Legacy mode
#'   spellings are accepted and normalized to these names.
#' @param lambda_prior Gamma shape--rate prior without a custom power field.
#' @param numerical_policy Either `"fail"` or `"record_repair"`.
#' @param provenance_control Optional primary-repository provenance plus named
#'   `external_repositories`. Repository specifications contain `repo_root`, a
#'   complete 40-character `expected_git_commit`, and optional runtime package
#'   and attestation fields.
#' @param mcmc_control Iteration, seed, storage, backend, progress, and jitter
#'   controls.
#' @param init Optional initial states, latent scales, lambda, evolution scales,
#'   time-zero states, and RNG state.
#' @return An `rqr_dlm_mcmc` object.
#' @export
rqr_dlm_fit <- function(
    y, model, coverage_level,
    evolution_mode = c(
      "fixed_W", "discount_template", "component_scale", "adaptive_discount"
    ),
    evolution_spec = NULL,
    W = NULL, df = NULL, dim.df = NULL,
    reference_variance = NULL, reference_design = NULL,
    component_templates = NULL,
    evolution_scale_prior = list(shape = 2, rate = 1),
    evolution_scale_initial = 1,
    learning_rate = 1, lambda_initial = 1, loss_reference_scale = 1,
    learning_rate_mode = c(
      "fixed_rate", "learned_pseudoresidual_normalized", "learned_pure"
    ),
    lambda_prior = list(shape = 4, rate = 4),
    numerical_policy = c("fail", "record_repair"),
    provenance_control = list(),
    mcmc_control = list(), init = list()) {
  y <- as.numeric(y)
  if (!length(y) || !any(!is.na(y)) || any(is.nan(y)) || any(is.infinite(y))) {
    stop("y must contain at least one finite observation; missing values must be NA.", call. = FALSE)
  }
  model <- rqr_as_dlm_model(model)
  expanded <- .rqr_expand_model(model, length(y))
  T <- length(y)
  p <- expanded$p
  observed <- !is.na(y)
  n_obs <- sum(observed)
  numerical_policy <- .rqr_numerical_policy(numerical_policy)
  provenance_control <- .rqr_provenance_control(provenance_control)
  if (!is.list(mcmc_control)) stop("mcmc_control must be a list.", call. = FALSE)
  jitter_ladder <- as.numeric(
    mcmc_control$jitter_ladder %||% c(0, 1e-12, 1e-10, 1e-8, 1e-6)
  )
  .rqr_jitter_ladder(numerical_policy, jitter_ladder)

  learning_rate_mode <- .rqr_learning_rate_mode(learning_rate_mode)
  lambda_prior <- .rqr_lambda_prior(lambda_prior, learning_rate_mode)
  loss_reference_scale <- as.numeric(loss_reference_scale)[1L]
  learning_rate <- as.numeric(learning_rate)[1L]
  lambda_initial <- as.numeric(init$lambda %||% lambda_initial)[1L]
  if (!is.finite(loss_reference_scale) || loss_reference_scale <= 0) {
    stop("loss_reference_scale must be finite and positive.", call. = FALSE)
  }
  if (!is.finite(learning_rate) || learning_rate <= 0) {
    stop("learning_rate must be finite and positive.", call. = FALSE)
  }
  if (!is.finite(lambda_initial) || lambda_initial <= 0) {
    stop("lambda_initial must be finite and positive.", call. = FALSE)
  }
  learn_lambda <- learning_rate_mode != "fixed_rate"
  lambda <- if (learn_lambda) lambda_initial else learning_rate * loss_reference_scale
  constants <- rqr_constants(coverage_level, lambda / loss_reference_scale)

  requested_mode <- if (is.null(evolution_spec)) match.arg(evolution_mode) else evolution_spec$mode
  evolution <- .rqr_dlm_evolution(
    requested_mode, model, expanded, y, W, df, dim.df,
    reference_variance, reference_design, component_templates,
    evolution_scale_prior, evolution_scale_initial, evolution_spec,
    numerical_policy, jitter_ladder
  )
  evolution_mode <- evolution$mode
  if (!isTRUE(evolution$exact_joint_target)) {
    warning(
      paste(
        "adaptive_discount is an experimental working/sequential recursion,",
        "not an exact Gibbs sampler for a declared fixed joint target."
      ),
      call. = FALSE
    )
  }

  n_burn <- .rqr_scalar_integer(mcmc_control$n_burn %||% 500L, "mcmc_control$n_burn", 0L)
  n_keep <- .rqr_scalar_integer(mcmc_control$n_mcmc %||% 1000L, "mcmc_control$n_mcmc", 1L)
  thin <- .rqr_scalar_integer(mcmc_control$thin %||% 1L, "mcmc_control$thin", 1L)
  seed <- mcmc_control$seed %||% NULL
  if (!is.null(seed)) {
    seed <- .rqr_scalar_integer(seed, "mcmc_control$seed", 0L)
    set.seed(seed)
  } else {
    .rqr_restore_rng(init$rng_state %||% NULL)
  }
  backend_requested <- match.arg(
    mcmc_control$backend %||% "cpp", c("cpp", "R", "auto")
  )
  backend_resolved <- .rqr_resolve_ffbs_backend(backend_requested)
  store_state_draws <- isTRUE(mcmc_control$store_state_draws %||% FALSE)
  store_latent_draws <- isTRUE(mcmc_control$store_latent_draws %||% FALSE)
  verbose <- isTRUE(mcmc_control$verbose %||% FALSE)
  progress_every <- .rqr_scalar_integer(
    mcmc_control$progress_every %||% 100L, "mcmc_control$progress_every", 1L
  )

  paths <- .rqr_init_state_paths(y, expanded$FF, expanded$m0, coverage_level, init)
  theta1 <- paths$theta1
  theta2 <- paths$theta2
  v_initial <- as.numeric(init$latent_v %||% loss_reference_scale / lambda)
  if (!length(v_initial) %in% c(1L, T) || any(!is.finite(v_initial)) || any(v_initial <= 0)) {
    stop("init$latent_v must be finite, positive, and scalar or length(y).", call. = FALSE)
  }
  v <- rep_len(v_initial, T)
  component_mode <- identical(evolution_mode, "component_scale")
  time0_completion_mode <- component_mode ||
    (
      store_state_draws &&
        evolution_mode %in% c("fixed_W", "discount_template")
    )
  q_evolution <- if (component_mode) {
    as.numeric(init$evolution_scale %||% evolution$initial)
  } else {
    numeric(0)
  }
  if (component_mode && (length(q_evolution) != length(evolution$component_dims) ||
      any(!is.finite(q_evolution)) || any(q_evolution <= 0))) {
    stop("Initial component evolution scales are invalid.", call. = FALSE)
  }
  theta01 <- as.numeric(init$theta0_root1 %||% expanded$m0)
  theta02 <- as.numeric(init$theta0_root2 %||% expanded$m0)
  if (length(theta01) != p || length(theta02) != p ||
      any(!is.finite(theta01)) || any(!is.finite(theta02))) {
    stop("Initial time-zero states must be finite vectors of state dimension p.", call. = FALSE)
  }

  eta1_draws <- matrix(NA_real_, T, n_keep)
  eta2_draws <- matrix(NA_real_, T, n_keep)
  theta1_draws <- if (store_state_draws) array(NA_real_, c(p, T, n_keep)) else NULL
  theta2_draws <- if (store_state_draws) array(NA_real_, c(p, T, n_keep)) else NULL
  terminal1_draws <- terminal2_draws <- matrix(NA_real_, p, n_keep)
  v_draws <- if (store_latent_draws) matrix(NA_real_, T, n_keep) else NULL
  lambda_draws <- numeric(n_keep)
  q_draws <- if (component_mode) {
    matrix(NA_real_, n_keep, length(q_evolution), dimnames = list(NULL, evolution$component_names))
  } else NULL
  q_shape_draws <- if (component_mode) {
    matrix(
      NA_real_, n_keep, length(q_evolution),
      dimnames = list(NULL, evolution$component_names)
    )
  } else NULL
  q_rate_draws <- if (component_mode) {
    matrix(
      NA_real_, n_keep, length(q_evolution),
      dimnames = list(NULL, evolution$component_names)
    )
  } else NULL
  theta01_draws <- theta02_draws <- if (
      time0_completion_mode
    ) {
    matrix(NA_real_, p, n_keep)
  } else {
    NULL
  }

  total_iter <- n_burn + n_keep * thin
  loss_trace <- lambda_trace <- effective_rate_trace <- numeric(total_iter)
  lambda_shape_trace <- lambda_rate_trace <- rep(NA_real_, total_iter)
  root_swap_trace <- logical(total_iter)
  ffbs_iteration <- vector("list", 2L * total_iter)
  repair_records <- NULL
  save_idx <- 0L

  for (iter in seq_len(total_iter)) {
    eta1 <- .rqr_state_ordinates(expanded$FF, theta1)
    eta2 <- .rqr_state_ordinates(expanded$FF, theta2)
    e <- rqr_residual_product(y[observed], eta1[observed], eta2[observed])
    loss_current <- sum(rqr_check_loss(e, constants$alpha))
    if (learn_lambda) {
      lp <- .rqr_lambda_posterior_params(
        loss_sum = loss_current / loss_reference_scale,
        n = n_obs, lambda_prior = lambda_prior,
        learning_rate_mode = learning_rate_mode
      )
      lambda_shape_trace[iter] <- lp$shape
      lambda_rate_trace[iter] <- lp$rate
      lambda <- stats::rgamma(1L, shape = lp$shape, rate = lp$rate)
      constants <- rqr_constants(coverage_level, lambda / loss_reference_scale)
    }

    # Full latent-scale refresh immediately after the collapsed lambda draw.
    gp <- rqr_gig_params(e, coverage_level, constants$omega)
    v[observed] <- rqr_sample_gig_half(gp$b, gp$a)
    v[!observed] <- loss_reference_scale / lambda
    obs_variance <- constants$phi * loss_reference_scale * v / lambda
    evolution_iter <- if (component_mode) {
      .rqr_materialize_component_evolution(evolution, q_evolution, T, p)
    } else evolution

    H1 <- sweep(expanded$FF, 2L, y - eta2, `*`)
    H1[, !observed] <- 0
    z1 <- y * (y - eta2) - constants$xi * v
    z1[!observed] <- NA_real_
    draw1 <- rqr_ffbs_sample(
      z1, H1, obs_variance, expanded$GG, expanded$m0, expanded$C0,
      evolution_iter, backend = backend_resolved, jitter_ladder = jitter_ladder,
      numerical_policy = numerical_policy
    )
    theta1 <- draw1$path
    if (component_mode) {
      theta01 <- .rqr_draw_initial_state(
        theta1[, 1L], expanded$GG[, , 1L], expanded$m0, expanded$C0,
        evolution_iter$W[, , 1L]
      )
    }
    eta1 <- .rqr_state_ordinates(expanded$FF, theta1)

    H2 <- sweep(expanded$FF, 2L, y - eta1, `*`)
    H2[, !observed] <- 0
    z2 <- y * (y - eta1) - constants$xi * v
    z2[!observed] <- NA_real_
    draw2 <- rqr_ffbs_sample(
      z2, H2, obs_variance, expanded$GG, expanded$m0, expanded$C0,
      evolution_iter, backend = backend_resolved, jitter_ladder = jitter_ladder,
      numerical_policy = numerical_policy
    )
    theta2 <- draw2$path
    if (component_mode) {
      theta02 <- .rqr_draw_initial_state(
        theta2[, 1L], expanded$GG[, , 1L], expanded$m0, expanded$C0,
        evolution_iter$W[, , 1L]
      )
      q_update <- .rqr_sample_component_scales(
        theta1, theta2, theta01, theta02, expanded$GG, evolution
      )
      q_evolution <- q_update$draw
    } else if (time0_completion_mode) {
      # Fixed-W and frozen-template FFBS integrate theta_0 out through
      # (m0, C0). Complete each root path with an exact draw from
      # p(theta_0 | theta_1) so stored full-state summaries have the same
      # time-zero contract as component-scale fits.
      theta01 <- .rqr_draw_initial_state(
        theta1[, 1L], expanded$GG[, , 1L], expanded$m0, expanded$C0,
        evolution_iter$W[, , 1L]
      )
      theta02 <- .rqr_draw_initial_state(
        theta2[, 1L], expanded$GG[, , 1L], expanded$m0, expanded$C0,
        evolution_iter$W[, , 1L]
      )
    }

    if (stats::runif(1L) < 0.5) {
      tmp <- theta1; theta1 <- theta2; theta2 <- tmp
      tmp <- theta01; theta01 <- theta02; theta02 <- tmp
      root_swap_trace[iter] <- TRUE
    }
    eta1 <- .rqr_state_ordinates(expanded$FF, theta1)
    eta2 <- .rqr_state_ordinates(expanded$FF, theta2)

    loss_trace[iter] <- sum(rqr_check_loss(
      rqr_residual_product(y[observed], eta1[observed], eta2[observed]), constants$alpha
    ))
    lambda_trace[iter] <- lambda
    effective_rate_trace[iter] <- lambda / loss_reference_scale
    repair_records <- .rqr_bind_ffbs_repairs(repair_records, draw1$diagnostics, iter, "root1")
    repair_records <- .rqr_bind_ffbs_repairs(repair_records, draw2$diagnostics, iter, "root2")
    ffbs_iteration[[2L * iter - 1L]] <- data.frame(
      iteration = iter, root = "root1",
      jitter_count = draw1$diagnostics$jitter_count,
      repair_count = draw1$diagnostics$repair_count,
      psd_draw_count = draw1$diagnostics$psd_draw_count,
      min_forecast_variance = draw1$diagnostics$min_forecast_variance
    )
    ffbs_iteration[[2L * iter]] <- data.frame(
      iteration = iter, root = "root2",
      jitter_count = draw2$diagnostics$jitter_count,
      repair_count = draw2$diagnostics$repair_count,
      psd_draw_count = draw2$diagnostics$psd_draw_count,
      min_forecast_variance = draw2$diagnostics$min_forecast_variance
    )

    if (iter > n_burn && (iter - n_burn) %% thin == 0L) {
      save_idx <- save_idx + 1L
      eta1_draws[, save_idx] <- eta1
      eta2_draws[, save_idx] <- eta2
      terminal1_draws[, save_idx] <- theta1[, T]
      terminal2_draws[, save_idx] <- theta2[, T]
      lambda_draws[save_idx] <- lambda
      if (store_state_draws) {
        theta1_draws[, , save_idx] <- theta1
        theta2_draws[, , save_idx] <- theta2
      }
      if (!is.null(theta01_draws)) {
        theta01_draws[, save_idx] <- theta01
        theta02_draws[, save_idx] <- theta02
      }
      if (store_latent_draws) v_draws[, save_idx] <- v
      if (component_mode) {
        q_draws[save_idx, ] <- q_evolution
        q_shape_draws[save_idx, ] <- q_update$posterior$shape
        q_rate_draws[save_idx, ] <- q_update$posterior$rate
      }
    }
    if (verbose && (iter %% progress_every == 0L || iter == total_iter)) {
      message(sprintf("[rqr_dlm_fit] iter %d/%d loss=%.6g", iter, total_iter, loss_trace[iter]))
    }
  }

  lower <- pmin(eta1_draws, eta2_draws)
  upper <- pmax(eta1_draws, eta2_draws)
  lambda_summary <- .rqr_lambda_summary(lambda_draws)
  continued_from_checkpoint <- isTRUE(init$continued_from_checkpoint %||% FALSE)
  template_repairs <- if (continued_from_checkpoint) {
    0L
  } else {
    .rqr_history_count(
      evolution$construction_audit$repair_count %||% 0L,
      "evolution construction repair count"
    )
  }
  mcmc_repairs <- .rqr_history_count(
    if (is.null(repair_records)) 0L else nrow(repair_records),
    "MCMC repair count"
  )
  segment_repairs <- .rqr_history_count(
    as.double(template_repairs) + as.double(mcmc_repairs),
    "segment repair count"
  )
  numerical_exact <- segment_repairs == 0L
  parent_cumulative_repairs <- .rqr_history_count(
    init$parent_cumulative_numerical_repair_count %||% 0L,
    "parent cumulative repair count"
  )
  cumulative_repairs <- .rqr_history_count(
    as.double(parent_cumulative_repairs) + as.double(segment_repairs),
    "cumulative repair count"
  )
  parent_chain_numerically_exact <- isTRUE(
    init$parent_chain_history_numerically_exact %||% TRUE
  )
  chain_history_numerically_exact <-
    parent_chain_numerically_exact && numerical_exact
  parent_promotion_eligible <- if (continued_from_checkpoint) {
    isTRUE(init$parent_promotion_eligible)
  } else {
    TRUE
  }
  mathematical_exact <- isTRUE(evolution$exact_joint_target)
  rng_state <- .rqr_rng_state()
  completed_offset <- .rqr_history_count(
    init$completed_iterations %||% 0L,
    "completed_iterations"
  )
  completed_iterations <- .rqr_history_count(
    as.double(completed_offset) + as.double(total_iter),
    "cumulative completed_iterations"
  )
  target_contract <- .rqr_dlm_target_contract(
    coverage_level = constants$alpha,
    learning_rate_mode = learning_rate_mode,
    fixed_learning_rate = if (learn_lambda) NA_real_ else learning_rate,
    loss_reference_scale = loss_reference_scale,
    lambda_prior = lambda_prior,
    numerical_policy = numerical_policy,
    jitter_ladder = jitter_ladder
  )
  provenance <- .rqr_provenance(
    data = list(y = y),
    matrices = .rqr_dlm_provenance_matrices(expanded, evolution),
    numerical_policy = numerical_policy,
    initial_seed = seed,
    repo_root = provenance_control$repo_root,
    expected_git_commit = provenance_control$expected_git_commit,
    backend = backend_resolved,
    backend_requested = backend_requested,
    backend_resolved = backend_resolved,
    objects = .rqr_dlm_provenance_objects(
      expanded, evolution, target_contract
    ),
    external_repositories = provenance_control$external_repositories,
    required_external_repositories =
      provenance_control$required_external_repositories,
    primary_runtime_attestation =
      provenance_control$primary_runtime_attestation
  )
  segment_target_numerical_eligible <- mathematical_exact && numerical_exact
  target_numerical_eligible <- mathematical_exact &&
    chain_history_numerically_exact
  checkpoint <- list(
    schema_version = provenance$schema_version,
    completed_iterations = completed_iterations,
    theta_root1 = theta1,
    theta_root2 = theta2,
    theta0_root1 = theta01,
    theta0_root2 = theta02,
    latent_v = v,
    lambda = lambda,
    evolution_scale = q_evolution,
    rng_state = rng_state
  )
  checkpoint_digest <- .rqr_digest(checkpoint)
  out <- list(
    method = "mcmc_ffbs",
    family = "rqr_dlm",
    model = model,
    expanded_model = expanded,
    evolution = evolution,
    y = y,
    model_spec = list(
      family = "rqr_dlm",
      parameterization = "exchangeable_dynamic_roots",
      loss_name = "rqr_residual_product_check_loss",
      state_model = "linear_gaussian_interval_root_evolution",
      coverage_level = constants$alpha,
      learning_rate_mode = learning_rate_mode,
      fixed_learning_rate = if (learn_lambda) NA_real_ else learning_rate,
      learning_rate = if (learn_lambda) mean(lambda_draws / loss_reference_scale) else learning_rate,
      lambda_initial = lambda_initial,
      loss_reference_scale = loss_reference_scale,
      effective_learning_rate = mean(lambda_draws / loss_reference_scale),
      lambda_prior = lambda_prior,
      lambda_summary = lambda_summary,
      inferential_target = .rqr_target_formula(learning_rate_mode),
      generalized_bayes = TRUE,
      response_likelihood = FALSE,
      evolution_mode = evolution_mode,
      target_contract = if (mathematical_exact) "fixed_joint_exact" else "working_sequential",
      exact_joint_target = mathematical_exact,
      numerical_policy = numerical_policy,
      numerical_repair_count = segment_repairs,
      cumulative_numerical_repair_count = cumulative_repairs,
      numerically_exact_transition = numerical_exact,
      chain_history_numerically_exact = chain_history_numerically_exact,
      parent_chain_history_numerically_exact =
        parent_chain_numerically_exact,
      segment_target_numerical_eligible =
        segment_target_numerical_eligible,
      target_numerical_eligible = target_numerical_eligible,
      reproducibility_eligible = provenance$reproducibility_eligible,
      parent_promotion_eligible = parent_promotion_eligible,
      promotion_eligible = target_numerical_eligible &&
        parent_promotion_eligible && provenance$reproducibility_eligible,
      root_priors_exchangeable = TRUE,
      root_swap_move = TRUE
    ),
    samp.eta_root1 = eta1_draws,
    samp.eta_root2 = eta2_draws,
    samp.theta_root1 = theta1_draws,
    samp.theta_root2 = theta2_draws,
    samp.theta_terminal_root1 = terminal1_draws,
    samp.theta_terminal_root2 = terminal2_draws,
    samp.theta0_root1 = theta01_draws,
    samp.theta0_root2 = theta02_draws,
    samp.lambda = lambda_draws,
    samp.latent_v = v_draws,
    samp.evolution_scale = q_draws,
    samp.evolution_scale_shape = q_shape_draws,
    samp.evolution_scale_rate = q_rate_draws,
    summary = .rqr_dlm_coverage_summary(y, observed, lower, upper),
    diagnostics = list(
      loss_trace = loss_trace,
      scaled_loss_trace = loss_trace / loss_reference_scale,
      lambda_trace = lambda_trace,
      effective_learning_rate_trace = effective_rate_trace,
      lambda_post_shape_trace = lambda_shape_trace,
      lambda_post_rate_trace = lambda_rate_trace,
      root_swap_trace = root_swap_trace,
      ffbs_iteration = do.call(rbind, ffbs_iteration),
      numerical_repairs = repair_records %||% data.frame(),
      template_construction_audit = evolution$construction_audit %||% NULL,
      partial_collapse_order = c(
        "lambda_collapsed", "latent_v_refresh", "root1_ffbs",
        if (component_mode) "root1_time0" else NULL,
        "root2_ffbs",
        if (component_mode) {
          "root2_time0"
        } else if (time0_completion_mode) {
          "fixed_evolution_time0_completion"
        } else NULL,
        if (component_mode) "component_scale_update" else NULL,
        "global_root_swap"
      )
    ),
    provenance = provenance,
    checkpoint_state = checkpoint,
    checkpoint_digest = checkpoint_digest,
    last = checkpoint,
    misc = list(
      n_burn = n_burn, n_mcmc = n_keep, thin = thin, seed = seed,
      backend = backend_resolved,
      backend_requested = backend_requested,
      backend_resolved = backend_resolved,
      observed = observed, store_state_draws = store_state_draws,
      store_latent_draws = store_latent_draws, jitter_ladder = jitter_ladder,
      note = paste(
        "Root trajectory draws arise from a generalized-Bayes loss update;",
        "they are not response draws."
      )
    )
  )
  out$continuation_history_contract <- .rqr_make_continuation_history(
    checkpoint_digest = checkpoint_digest,
    segment_numerical_repair_count =
      out$model_spec$numerical_repair_count,
    segment_exact_joint_target =
      out$model_spec$exact_joint_target,
    segment_environment_base_eligible =
      out$provenance$reproducibility_eligible,
    segment_target_contract_digest =
      out$provenance$object_digests$target,
    backend_requested = out$provenance$backend_requested,
    backend_resolved = out$provenance$backend_resolved
  )
  out$continuation_history_digest <- .rqr_digest(
    out$continuation_history_contract
  )
  class(out) <- c("rqr_dlm_mcmc", "rqr_fit")
  out
}

.rqr_validate_dlm_continuation <- function(object, allow_environment_mismatch = FALSE) {
  if (length(allow_environment_mismatch) != 1L ||
      !is.logical(allow_environment_mismatch) || is.na(allow_environment_mismatch)) {
    stop("allow_environment_mismatch must be TRUE or FALSE.", call. = FALSE)
  }
  supported_schema <- .rqr_schema_version()
  object_schema <- object$provenance$schema_version %||% NA_character_
  checkpoint_schema <- object$checkpoint_state$schema_version %||% NA_character_
  if (!identical(object_schema, supported_schema) ||
      !identical(checkpoint_schema, supported_schema)) {
    stop(
      sprintf(
        "Continuation requires schema %s in both the fit and checkpoint.",
        supported_schema
      ),
      call. = FALSE
    )
  }
  .rqr_history_count(
    object$checkpoint_state$completed_iterations,
    "checkpoint_state$completed_iterations"
  )
  continuation_history <- .rqr_validate_continuation_history(object)
  stored_checkpoint_digest <- object$checkpoint_digest %||% NA_character_
  if (!.rqr_nonmissing_text(stored_checkpoint_digest) ||
      !identical(.rqr_digest(object$checkpoint_state), stored_checkpoint_digest)) {
    stop(
      "Continuation checkpoint digest does not match the fitted object.",
      call. = FALSE
    )
  }

  expanded <- .rqr_expand_model(rqr_as_dlm_model(object$model), length(object$y))
  current_data_digest <- .rqr_digest(list(y = as.numeric(object$y)))
  current_matrix_digests <- lapply(
    .rqr_dlm_provenance_matrices(expanded, object$evolution), .rqr_digest
  )
  if (!identical(current_data_digest, object$provenance$data_digest)) {
    stop("Continuation data digest does not match the fitted object.", call. = FALSE)
  }
  if (!identical(current_matrix_digests, object$provenance$matrix_digests)) {
    stop("Continuation model/evolution matrix digests do not match the fitted object.", call. = FALSE)
  }
  fixed_rate <- object$model_spec$fixed_learning_rate
  if (is.null(fixed_rate) || !is.finite(fixed_rate)) fixed_rate <- NA_real_
  current_target_contract <- .rqr_dlm_target_contract(
    coverage_level = object$model_spec$coverage_level,
    learning_rate_mode = object$model_spec$learning_rate_mode,
    fixed_learning_rate = fixed_rate,
    loss_reference_scale = object$model_spec$loss_reference_scale,
    lambda_prior = object$model_spec$lambda_prior,
    numerical_policy = object$model_spec$numerical_policy,
    jitter_ladder = object$misc$jitter_ladder
  )
  current_object_digests <- lapply(
    .rqr_dlm_provenance_objects(
      expanded, object$evolution, current_target_contract
    ),
    .rqr_digest
  )
  if (!identical(current_object_digests, object$provenance$object_digests)) {
    stop(
      "Continuation model, target, or evolution digest does not match the fitted object.",
      call. = FALSE
    )
  }

  stored_expected <- object$provenance$expected_git_commit %||% NA_character_
  backend_requested <- object$misc$backend_requested %||% object$misc$backend
  backend_resolved <- .rqr_resolve_ffbs_backend(backend_requested)
  current <- .rqr_provenance(
    data = list(y = as.numeric(object$y)),
    matrices = .rqr_dlm_provenance_matrices(expanded, object$evolution),
    numerical_policy = object$model_spec$numerical_policy,
    repo_root = object$provenance$repo_root %||% NA_character_,
    expected_git_commit = if (is.na(stored_expected)) NULL else stored_expected,
    backend = backend_resolved,
    backend_requested = backend_requested,
    backend_resolved = backend_resolved,
    objects = .rqr_dlm_provenance_objects(
      expanded, object$evolution, current_target_contract
    ),
    external_repositories = object$provenance$external_repositories,
    required_external_repositories =
      object$provenance$required_external_repositories,
    primary_runtime_attestation = {
      value <- object$provenance$primary_runtime_attestation %||% NA_character_
      if (is.na(value)) NULL else value
    }
  )
  compare_fields <- c(
    "package_version", "R_version", "platform", "compiler", "BLAS", "LAPACK",
    "git_commit", "git_commit_available", "git_status_available", "git_dirty",
    "expected_git_commit", "expected_git_commit_match",
    "basic_provenance_complete", "provenance_complete",
    "primary_runtime_source_match", "primary_runtime_package_path",
    "primary_source_commit", "primary_source_tree_digest",
    "primary_runtime_tree_digest",
    "backend_requested", "backend_resolved", "RNGkind"
  )
  mismatches <- compare_fields[!vapply(compare_fields, function(field) {
    identical(object$provenance[[field]], current[[field]])
  }, logical(1L))]
  if (!identical(object$provenance$dependency_versions, current$dependency_versions)) {
    mismatches <- c(mismatches, "dependency_versions")
  }
  if (!identical(
        object$provenance$external_repositories,
        current$external_repositories
      )) {
    mismatches <- c(mismatches, "external_repositories")
  }
  if (length(mismatches)) {
    message <- sprintf(
      "Continuation environment differs in: %s.", paste(unique(mismatches), collapse = ", ")
    )
    if (!allow_environment_mismatch) {
      stop(
        paste(message, "Set allow_environment_mismatch=TRUE only for a non-bitwise portability run."),
        call. = FALSE
      )
    }
    warning(
      paste(message, "Exact bitwise continuation is not claimed for this segment."),
      call. = FALSE
    )
  }
  invisible(list(
    current_provenance = current,
    environment_mismatches = unique(mismatches),
    checkpoint_digest = stored_checkpoint_digest,
    object_digests = current_object_digests,
    continuation_history = continuation_history
  ))
}

#' Continue an RQR-DLM chain from its exact checkpoint
#'
#' Continuation restores the full RNG state and every Markov state required by
#' the native sampler. Numerical repair and promotion eligibility are inherited
#' cumulatively, and the requested and resolved FFBS backends are checked
#' separately. The function returns only the newly requested draws.
#'
#' @param object An `rqr_dlm_mcmc` fit.
#' @param n_mcmc Positive number of additional retained draws.
#' @param thin Positive thinning interval; defaults to the original fit.
#' @param store_state_draws,store_latent_draws Storage choices for new draws.
#' @param allow_environment_mismatch If `TRUE`, continue after an explicit
#'   warning when package, R, platform, BLAS/LAPACK, dependency, or source-commit
#'   metadata differ. Schema, checkpoint, data, model, target, and evolution
#'   digest mismatches always stop. The override is persisted and removes
#'   reproducibility and promotion eligibility from the returned segment.
#' @return A new `rqr_dlm_mcmc` segment beginning at the checkpoint.
#' @export
rqr_dlm_continue <- function(object, n_mcmc, thin = object$misc$thin,
                             store_state_draws = object$misc$store_state_draws,
                             store_latent_draws = object$misc$store_latent_draws,
                             allow_environment_mismatch = FALSE) {
  if (!inherits(object, "rqr_dlm_mcmc")) stop("Expected an rqr_dlm_mcmc object.", call. = FALSE)
  n_mcmc <- .rqr_scalar_integer(n_mcmc, "n_mcmc", 1L)
  thin <- .rqr_scalar_integer(thin, "thin", 1L)
  validation <- .rqr_validate_dlm_continuation(
    object, allow_environment_mismatch
  )
  checkpoint <- object$checkpoint_state
  if (is.null(checkpoint$rng_state)) stop("The fit does not contain a complete RNG checkpoint.", call. = FALSE)
  fixed_rate <- object$model_spec$fixed_learning_rate
  if (is.null(fixed_rate) || !is.finite(fixed_rate)) fixed_rate <- 1
  segment <- rqr_dlm_fit(
    y = object$y,
    model = object$model,
    coverage_level = object$model_spec$coverage_level,
    evolution_spec = object$evolution,
    learning_rate = fixed_rate,
    lambda_initial = checkpoint$lambda,
    loss_reference_scale = object$model_spec$loss_reference_scale,
    learning_rate_mode = object$model_spec$learning_rate_mode,
    lambda_prior = object$model_spec$lambda_prior,
    numerical_policy = object$model_spec$numerical_policy,
    provenance_control = list(
      repo_root = if (is.na(object$provenance$repo_root)) NULL else object$provenance$repo_root,
      expected_git_commit = if (is.na(object$provenance$expected_git_commit)) {
        NULL
      } else {
        object$provenance$expected_git_commit
      },
      primary_runtime_attestation = {
        value <- object$provenance$primary_runtime_attestation %||%
          NA_character_
        if (is.na(value)) NULL else value
      },
      external_repositories = lapply(
        object$provenance$external_repositories %||% list(),
        function(x) list(
          repo_root = if (is.na(x$repo_root)) NULL else x$repo_root,
          expected_git_commit = if (is.na(x$expected_git_commit)) {
            NULL
          } else {
            x$expected_git_commit
          },
          runtime_package = if (is.na(x$runtime_package)) {
            NULL
          } else {
            x$runtime_package
          },
          runtime_attestation = if (is.na(x$runtime_attestation)) {
            NULL
          } else {
            x$runtime_attestation
          },
          require_isolated_runtime = isTRUE(x$require_isolated_runtime)
          ,
          source_subdir = x$source_subdir %||% "."
        )
      ),
      required_external_repositories =
        object$provenance$required_external_repositories %||% character(0)
    ),
    mcmc_control = list(
      n_burn = 0L, n_mcmc = n_mcmc, thin = thin, seed = NULL,
      backend = object$misc$backend_requested %||% object$misc$backend,
      store_state_draws = isTRUE(store_state_draws),
      store_latent_draws = isTRUE(store_latent_draws),
      jitter_ladder = object$misc$jitter_ladder
    ),
    init = list(
      state_root1 = checkpoint$theta_root1,
      state_root2 = checkpoint$theta_root2,
      theta0_root1 = checkpoint$theta0_root1,
      theta0_root2 = checkpoint$theta0_root2,
      latent_v = checkpoint$latent_v,
      lambda = checkpoint$lambda,
      evolution_scale = checkpoint$evolution_scale,
      rng_state = checkpoint$rng_state,
      completed_iterations = checkpoint$completed_iterations,
      continued_from_checkpoint = TRUE,
      parent_cumulative_numerical_repair_count =
        validation$continuation_history$
          cumulative_numerical_repair_count,
      parent_chain_history_numerically_exact =
        validation$continuation_history$
          chain_history_numerically_exact,
      parent_promotion_eligible =
        validation$continuation_history$promotion_eligible
    )
  )
  environment_override_used <- length(validation$environment_mismatches) > 0L
  parent_reproducibility_eligible <- isTRUE(
    validation$continuation_history$reproducibility_eligible
  )
  current_environment_eligible <- isTRUE(
    segment$provenance$reproducibility_eligible
  )
  inherited_reproducibility_eligible <- current_environment_eligible &&
    parent_reproducibility_eligible && !environment_override_used
  parent_chain_history_numerically_exact <- isTRUE(
    validation$continuation_history$chain_history_numerically_exact
  )
  parent_promotion_eligible <- isTRUE(
    validation$continuation_history$promotion_eligible
  )
  parent_cumulative_repairs <- .rqr_history_count(
    validation$continuation_history$cumulative_numerical_repair_count,
    "parent cumulative repair count"
  )
  same_resolved_backend <- identical(
    object$provenance$backend_resolved %||% object$provenance$backend,
    validation$current_provenance$backend_resolved %||%
      validation$current_provenance$backend
  )
  bitwise_continuation_claim <- !environment_override_used &&
    !length(validation$environment_mismatches) &&
    parent_reproducibility_eligible &&
    current_environment_eligible &&
    same_resolved_backend
  segment$continuation_contract <- list(
    continued_from_checkpoint = TRUE,
    parent_checkpoint_digest = validation$checkpoint_digest,
    parent_completed_iterations = checkpoint$completed_iterations,
    model_target_evolution_digests = validation$object_digests,
    environment_mismatches = validation$environment_mismatches,
    environment_override_used = environment_override_used,
    bitwise_continuation_claim = bitwise_continuation_claim,
    parent_reproducibility_eligible = parent_reproducibility_eligible,
    parent_target_numerical_eligible =
      isTRUE(object$model_spec$target_numerical_eligible),
    parent_promotion_eligible = parent_promotion_eligible,
    parent_chain_history_numerically_exact =
      parent_chain_history_numerically_exact,
    parent_cumulative_numerical_repair_count =
      parent_cumulative_repairs,
    chain_history_numerically_exact =
      isTRUE(segment$model_spec$chain_history_numerically_exact),
    cumulative_numerical_repair_count =
      segment$model_spec$cumulative_numerical_repair_count,
    backend_requested =
      segment$provenance$backend_requested,
    parent_backend_resolved =
      object$provenance$backend_resolved %||% object$provenance$backend,
    current_backend_resolved =
      segment$provenance$backend_resolved %||% segment$provenance$backend,
    current_environment_reproducibility_eligible =
      current_environment_eligible
  )
  segment$provenance$continued_from_checkpoint <- TRUE
  segment$provenance$parent_checkpoint_digest <- validation$checkpoint_digest
  segment$provenance$environment_override_used <- environment_override_used
  segment$provenance$reproducibility_eligible <-
    inherited_reproducibility_eligible
  segment$model_spec$reproducibility_eligible <-
    inherited_reproducibility_eligible
  segment$model_spec$promotion_eligible <-
    isTRUE(segment$model_spec$target_numerical_eligible) &&
    parent_promotion_eligible &&
    inherited_reproducibility_eligible
  segment$continuation_history_contract <- .rqr_make_continuation_history(
    checkpoint_digest = segment$checkpoint_digest,
    segment_numerical_repair_count =
      segment$model_spec$numerical_repair_count,
    segment_exact_joint_target =
      segment$model_spec$exact_joint_target,
    segment_environment_base_eligible =
      current_environment_eligible,
    segment_target_contract_digest =
      segment$provenance$object_digests$target,
    backend_requested = segment$provenance$backend_requested,
    backend_resolved = segment$provenance$backend_resolved,
    parent = validation$continuation_history,
    parent_checkpoint_digest = validation$checkpoint_digest,
    environment_mismatches = validation$environment_mismatches,
    environment_override_used = environment_override_used
  )
  segment$continuation_history_digest <- .rqr_digest(
    segment$continuation_history_contract
  )
  segment
}

#' @export
rqr_posterior_draws.rqr_dlm_mcmc <- function(object, nd = NULL, seed = NULL, ...) {
  if (!inherits(object, "rqr_dlm_mcmc")) stop("Expected an rqr_dlm_mcmc object.", call. = FALSE)
  if (!is.null(seed)) set.seed(.rqr_scalar_integer(seed, "seed", 0L))
  n_save <- ncol(object$samp.eta_root1)
  idx <- if (is.null(nd)) seq_len(n_save) else {
    nd <- .rqr_scalar_integer(nd, "nd", 1L)
    sample.int(n_save, nd, replace = nd > n_save)
  }
  list(
    eta_root1 = object$samp.eta_root1[, idx, drop = FALSE],
    eta_root2 = object$samp.eta_root2[, idx, drop = FALSE],
    lambda = object$samp.lambda[idx], index = idx, nd = length(idx)
  )
}

#' @export
predict_interval.rqr_dlm_mcmc <- function(object, nd = NULL, draws = NULL, seed = NULL, ...) {
  if (is.null(draws)) draws <- rqr_posterior_draws(object, nd = nd, seed = seed)
  lower <- pmin(draws$eta_root1, draws$eta_root2)
  upper <- pmax(draws$eta_root1, draws$eta_root2)
  list(
    lower_draws = lower, upper_draws = upper,
    midpoint_draws = 0.5 * (lower + upper), width_draws = upper - lower,
    lower_mean = rowMeans(lower), upper_mean = rowMeans(upper),
    midpoint_mean = rowMeans(0.5 * (lower + upper)), width_mean = rowMeans(upper - lower),
    model_spec = object$model_spec
  )
}

#' Draw future root trajectories from an RQR-DLM fit
#'
#' These are state-evolution draws for interval roots, not predictive response
#' draws. Future evolution covariances are explicit and validated.
#'
#' @param object An `rqr_dlm_mcmc` fit.
#' @param FF_future State-by-horizon observation design.
#' @param GG_future Evolution matrix or cube.
#' @param W_future Explicit evolution covariance matrix or cube. Supply either
#'   this argument or `component_templates_future`.
#' @param component_templates_future Optional fixed future component templates.
#'   For a component-scale fit, these are combined with the saved draw-specific
#'   evolution multipliers.
#' @param nd Number of saved MCMC draws to use.
#' @param seed Optional seed.
#' @param numerical_policy Either `"fail"` or `"record_repair"`.
#' @param jitter_ladder Matrix-relative jitter ladder for record-repair mode.
#'   An exactly zero matrix uses a separately recorded absolute fallback.
#' @return Future root and ordered endpoint draws with repair diagnostics.
#' @export
rqr_forecast_roots <- function(
    object, FF_future, GG_future, W_future = NULL,
    component_templates_future = NULL, nd = NULL, seed = NULL,
    numerical_policy = object$model_spec$numerical_policy %||% "fail",
    jitter_ladder = object$misc$jitter_ladder %||% c(0, 1e-12, 1e-10, 1e-8, 1e-6)) {
  if (!inherits(object, "rqr_dlm_mcmc")) stop("Expected an rqr_dlm_mcmc object.", call. = FALSE)
  terminal1 <- object$samp.theta_terminal_root1
  terminal2 <- object$samp.theta_terminal_root2
  if (is.null(terminal1) || is.null(terminal2)) {
    if (is.null(object$samp.theta_root1) || is.null(object$samp.theta_root2)) {
      stop("The fit contains neither terminal nor full state draws.", call. = FALSE)
    }
    terminal1 <- object$samp.theta_root1[, dim(object$samp.theta_root1)[2L], ]
    terminal2 <- object$samp.theta_root2[, dim(object$samp.theta_root2)[2L], ]
  }
  if (!is.null(seed)) set.seed(.rqr_scalar_integer(seed, "seed", 0L))
  FF_future <- as.matrix(FF_future)
  p <- nrow(terminal1)
  H <- ncol(FF_future)
  if (nrow(FF_future) != p || H < 1L || any(!is.finite(FF_future))) {
    stop("FF_future must be finite p x H.", call. = FALSE)
  }
  GG <- .rqr_expand_cube(GG_future, H, p, "GG_future")
  if (!is.null(W_future) && !is.null(component_templates_future)) {
    stop("Supply W_future or component_templates_future, not both.", call. = FALSE)
  }
  if (is.null(W_future) && is.null(component_templates_future)) {
    stop("Supply W_future or component_templates_future.", call. = FALSE)
  }
  component_future <- !is.null(component_templates_future)
  W_fixed <- NULL
  future_component_evolution <- NULL
  if (component_future) {
    if (!identical(object$model_spec$evolution_mode, "component_scale") ||
        is.null(object$samp.evolution_scale)) {
      stop(
        "component_templates_future requires a component_scale fit with saved scale draws.",
        call. = FALSE
      )
    }
    future_component_evolution <- rqr_evolution_component_scale(
      templates = component_templates_future,
      component_dims = object$evolution$component_dims,
      prior = object$evolution$prior,
      initial = 1,
      component_names = object$evolution$component_names
    )
    .rqr_expand_component_templates(future_component_evolution, H, p)
  } else {
    W_fixed <- .rqr_prepare_evolution(
      list(mode = "fixed_W", W = W_future), p, H
    )$W
  }
  numerical_policy <- .rqr_numerical_policy(numerical_policy)
  ladder <- .rqr_jitter_ladder(numerical_policy, jitter_ladder)
  n_save <- ncol(terminal1)
  idx <- if (is.null(nd)) seq_len(n_save) else {
    nd <- .rqr_scalar_integer(nd, "nd", 1L)
    sample.int(n_save, nd, replace = nd > n_save)
  }
  root1 <- root2 <- matrix(NA_real_, H, length(idx))
  repairs <- NULL
  for (j in seq_along(idx)) {
    s1 <- terminal1[, idx[j]]
    s2 <- terminal2[, idx[j]]
    W_draw <- if (component_future) {
      .rqr_materialize_component_evolution(
        future_component_evolution,
        q = object$samp.evolution_scale[idx[j], ],
        n_time = H,
        p = p
      )$W
    } else {
      W_fixed
    }
    for (hh in seq_len(H)) {
      mu1 <- drop(GG[, , hh] %*% s1)
      mu2 <- drop(GG[, , hh] %*% s2)
      d1 <- .rqr_sample_mvnorm_covariance(
        mu1, W_draw[, , hh], ladder, numerical_policy
      )
      d2 <- .rqr_sample_mvnorm_covariance(
        mu2, W_draw[, , hh], ladder, numerical_policy
      )
      s1 <- d1$draw
      s2 <- d2$draw
      new_repair <- .rqr_add_repair_record(
        .rqr_empty_repair_records(), "future_state_covariance_root1", hh, d1$info
      )
      if (nrow(new_repair)) {
        new_repair$draw <- j; new_repair$root <- 1L
        repairs <- if (is.null(repairs)) new_repair else rbind(repairs, new_repair)
      }
      new_repair <- .rqr_add_repair_record(
        .rqr_empty_repair_records(), "future_state_covariance_root2", hh, d2$info
      )
      if (nrow(new_repair)) {
        new_repair$draw <- j; new_repair$root <- 2L
        repairs <- if (is.null(repairs)) new_repair else rbind(repairs, new_repair)
      }
      root1[hh, j] <- drop(crossprod(FF_future[, hh], s1))
      root2[hh, j] <- drop(crossprod(FF_future[, hh], s2))
    }
  }
  lower <- pmin(root1, root2)
  upper <- pmax(root1, root2)
  list(
    eta_root1 = root1, eta_root2 = root2,
    lower_draws = lower, upper_draws = upper,
    midpoint_draws = 0.5 * (lower + upper), width_draws = upper - lower,
    lower_mean = rowMeans(lower), upper_mean = rowMeans(upper),
    draw_index = idx,
    diagnostics = list(
      numerical_policy = numerical_policy,
      repair_count = if (is.null(repairs)) 0L else nrow(repairs),
      repair_records = repairs %||% data.frame(),
      future_evolution_mode = if (component_future) "component_scale" else "fixed_W",
      component_scale_draws = if (component_future) {
        object$samp.evolution_scale[idx, , drop = FALSE]
      } else {
        NULL
      }
    ),
    interpretation = "Future interval-root state draws; no response simulation contract is implied."
  )
}

#' @export
print.rqr_dlm_mcmc <- function(x, ...) {
  cat("RQR dynamic MCMC fit\n")
  cat(sprintf("  coverage_level: %.4f\n", x$model_spec$coverage_level))
  cat(sprintf("  evolution_mode: %s\n", x$model_spec$evolution_mode))
  cat(sprintf("  target contract: %s\n", x$model_spec$target_contract))
  cat(sprintf("  numerical repairs: %d\n", x$model_spec$numerical_repair_count))
  cat(sprintf("  promotion eligible: %s\n", if (x$model_spec$promotion_eligible) "yes" else "no"))
  cat(sprintf("  draws:          %d\n", ncol(x$samp.eta_root1)))
  cat("  interpretation: generalized-Bayes root paths, not response draws\n")
  invisible(x)
}
