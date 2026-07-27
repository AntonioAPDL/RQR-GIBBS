# One exact ordinary-RQR fixed-design Gibbs transition.
#
# The transition is deliberately kept separate from storage, continuation, and
# reporting.  Both a fresh fit and an exact continuation call this function.
# Missing responses contribute neither a loss term nor a latent-scale draw.
# Their positions remain in the full-length latent vector using the
# deterministic current-scale placeholder so row alignment is never lost.

.rqr_fixed_design_beta_update <- function(
    data, beta_other, latent_v, constants, prior, prior_state,
    precision_beta_cfg, iteration, root) {
  observed <- data$observed
  y <- data$y[observed]
  X <- data$X[observed, , drop = FALSE]
  eta_other <- drop(X %*% beta_other)
  v <- latent_v[observed]

  # Conditional pseudo-observation:
  # y(y - X beta_other) - xi V
  #   = diag(y - X beta_other) X beta + epsilon,
  # epsilon ~ N(0, phi sigma V).
  design <- X * as.numeric(y - eta_other)
  # Preserve the established complete-data arithmetic/RNG oracle exactly.
  pseudo_response <- y^2 - y * eta_other - constants$xi * v
  weight <- 1 / (constants$phi * constants$sigma * v)
  canonical <- .rqr_prior_canonical(
    prior, prior_state, p = data$p
  )
  precision <- crossprod(design * sqrt(weight)) +
    canonical$precision
  information <- as.numeric(
    crossprod(design, weight * pseudo_response)
  ) + canonical$information
  draw <- .exal_mcmc_sample_mvnorm_prec(
    rhs = information,
    Prec = precision,
    precision_beta_cfg = precision_beta_cfg,
    context = list(
      iteration = iteration,
      update_family = "rqr_generalized_bayes_loss",
      beta_prior_type = prior$type,
      root = root
    )
  )
  draw$prior_canonical <- canonical
  draw
}

.rqr_transition_repair_rows <- function(info, iteration, root) {
  rows <- .rqr_add_repair_record(
    .rqr_empty_repair_records(),
    stage = "beta_precision",
    time = NA_integer_,
    info = info %||% list()
  )
  if (nrow(rows)) {
    rows$iteration <- as.integer(iteration)
    rows$root <- as.character(root)
  }
  rows
}

.rqr_fixed_design_transition <- function(
    state, data, target, prior, precision_beta_cfg, iteration) {
  if (!is.list(state) ||
      !all(c(
        "beta_root1", "beta_root2", "lambda", "latent_v",
        "beta_prior_state1", "beta_prior_state2"
      ) %in% names(state))) {
    stop("The fixed-design transition state is incomplete.", call. = FALSE)
  }
  iteration <- .rqr_scalar_integer(
    iteration, "iteration", minimum = 1L
  )
  observed <- data$observed
  y_obs <- data$y[observed]
  X_obs <- data$X[observed, , drop = FALSE]
  beta1 <- as.numeric(state$beta_root1)
  beta2 <- as.numeric(state$beta_root2)
  if (length(beta1) != data$p || length(beta2) != data$p ||
      any(!is.finite(beta1)) || any(!is.finite(beta2))) {
    stop("The root-coefficient state is invalid.", call. = FALSE)
  }

  mode <- target$learning_rate_mode
  learn_lambda <- !identical(mode, "fixed_rate")
  lambda <- if (learn_lambda) {
    as.numeric(state$lambda)[1L]
  } else {
    target$fixed_learning_rate * target$loss_reference_scale
  }
  if (!is.finite(lambda) || lambda <= 0) {
    stop("The transition lambda state must be finite and positive.",
         call. = FALSE)
  }
  constants <- rqr_constants(
    target$coverage_level, lambda / target$loss_reference_scale
  )

  eta1_obs <- drop(X_obs %*% beta1)
  eta2_obs <- drop(X_obs %*% beta2)
  residual_product <- rqr_residual_product(
    y_obs, eta1_obs, eta2_obs
  )
  loss <- sum(rqr_check_loss(
    residual_product, target$coverage_level
  ))
  lambda_posterior <- list(
    shape = NA_real_, rate = NA_real_, power_count = 0
  )
  if (learn_lambda) {
    lambda_posterior <- .rqr_lambda_posterior_params(
      loss_sum = loss / target$loss_reference_scale,
      n = data$n_observed,
      lambda_prior = target$lambda_prior,
      learning_rate_mode = mode
    )
    lambda <- stats::rgamma(
      1L, shape = lambda_posterior$shape,
      rate = lambda_posterior$rate
    )
    if (!is.finite(lambda) || lambda <= 0) {
      stop("The learned inverse-loss-scale draw was invalid.",
           call. = FALSE)
    }
    constants <- rqr_constants(
      target$coverage_level, lambda / target$loss_reference_scale
    )
  }

  # Mandatory full observed-site latent refresh after a collapsed lambda draw.
  gig <- rqr_gig_params(
    residual_product,
    coverage_level = target$coverage_level,
    learning_rate = constants$omega
  )
  v_observed <- as.numeric(.sample_gig_devroye_required(
    1L, p = gig$p, a = gig$a, b_vec = gig$b,
    context = "rqr_fixed_design_transition::latent_v"
  )[1L, ])
  latent_v <- rep(constants$sigma, data$n_total)
  latent_v[observed] <- v_observed

  update1 <- .rqr_fixed_design_beta_update(
    data = data, beta_other = beta2, latent_v = latent_v,
    constants = constants, prior = prior,
    prior_state = state$beta_prior_state1,
    precision_beta_cfg = precision_beta_cfg,
    iteration = iteration, root = "root1"
  )
  beta1 <- update1$draw
  prior_update1 <- .rqr_prior_update(
    prior, state$beta_prior_state1, beta1,
    numerical_policy = target$numerical_policy
  )

  update2 <- .rqr_fixed_design_beta_update(
    data = data, beta_other = beta1, latent_v = latent_v,
    constants = constants, prior = prior,
    prior_state = state$beta_prior_state2,
    precision_beta_cfg = precision_beta_cfg,
    iteration = iteration, root = "root2"
  )
  beta2 <- update2$draw
  prior_update2 <- .rqr_prior_update(
    prior, state$beta_prior_state2, beta2,
    numerical_policy = target$numerical_policy
  )

  prior_state1 <- prior_update1$state
  prior_state2 <- prior_update2$state
  prior_stats1 <- prior_update1$stats %||% list()
  prior_stats2 <- prior_update2$stats %||% list()
  precision_info1 <- update1$info %||% list()
  precision_info2 <- update2$info %||% list()
  # Repair provenance refers to the conditional block that was actually
  # factorized. Record it before any label permutation.
  precision_repairs <- rbind(
    .rqr_transition_repair_rows(
      precision_info1, iteration, "root1"
    ),
    .rqr_transition_repair_rows(
      precision_info2, iteration, "root2"
    )
  )

  # Always consume exactly one uniform variate.  This makes p_swap part of the
  # transition contract and preserves the historical complete-data RNG stream
  # when p_swap = 0.5.
  root_swapped <- stats::runif(1L) < target$root_swap_probability
  if (root_swapped) {
    temporary <- beta1
    beta1 <- beta2
    beta2 <- temporary
    temporary <- prior_state1
    prior_state1 <- prior_state2
    prior_state2 <- temporary
    temporary <- prior_stats1
    prior_stats1 <- prior_stats2
    prior_stats2 <- temporary
    temporary <- precision_info1
    precision_info1 <- precision_info2
    precision_info2 <- temporary
  }

  eta1_obs <- drop(X_obs %*% beta1)
  eta2_obs <- drop(X_obs %*% beta2)
  loss <- sum(rqr_check_loss(
    rqr_residual_product(y_obs, eta1_obs, eta2_obs),
    target$coverage_level
  ))
  prior_repair_count <- .rqr_history_count(
    prior_update1$numerical_repair_count %||% 0L,
    "root1 prior numerical-repair count"
  ) + .rqr_history_count(
    prior_update2$numerical_repair_count %||% 0L,
    "root2 prior numerical-repair count"
  )

  list(
    state = list(
      beta_root1 = beta1,
      beta_root2 = beta2,
      lambda = lambda,
      latent_v = latent_v,
      beta_prior_state1 = prior_state1,
      beta_prior_state2 = prior_state2
    ),
    constants = constants,
    loss = loss,
    lambda_posterior = lambda_posterior,
    root_swapped = root_swapped,
    precision_info_root1 = precision_info1,
    precision_info_root2 = precision_info2,
    prior_stats_root1 = prior_stats1,
    prior_stats_root2 = prior_stats2,
    precision_repair_records = precision_repairs,
    prior_numerical_repair_count = prior_repair_count,
    numerical_repair_count =
      as.integer(nrow(precision_repairs) + prior_repair_count)
  )
}
