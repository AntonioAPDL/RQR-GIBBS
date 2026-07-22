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
  observed <- is.finite(y)
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

.rqr_dlm_evolution <- function(mode, model, expanded, y, W, df, dim.df,
                               reference_variance, reference_design) {
  mode <- match.arg(mode, c("fixed_W", "discount_template", "adaptive_discount"))
  p <- expanded$p
  T <- expanded$n_time
  if (mode == "fixed_W") {
    if (is.null(W)) stop("evolution_mode='fixed_W' requires W.", call. = FALSE)
    return(structure(list(
      mode = mode,
      W = .rqr_expand_cube(W, T, p, "W"),
      exact_joint_target = TRUE,
      frozen_before_mcmc = TRUE
    ), class = "rqr_evolution"))
  }
  if (is.null(dim.df)) dim.df <- model$component_dims %||% p
  if (is.null(df)) stop("Discount evolution requires df.", call. = FALSE)
  D <- rqr_discount_matrix(df, dim.df, p)
  if (mode == "adaptive_discount") {
    return(structure(list(
      mode = mode, df = as.numeric(df), dim.df = as.integer(dim.df), D = D,
      exact_joint_target = FALSE, frozen_before_mcmc = FALSE
    ), class = "rqr_evolution"))
  }
  reference_source <- if (is.null(reference_variance)) {
    "training_response_variance"
  } else {
    "user_supplied"
  }
  if (is.null(reference_variance)) {
    yy <- y[is.finite(y)]
    empirical_variance <- stats::var(yy)
    if (!is.finite(empirical_variance) || empirical_variance <= 0) empirical_variance <- 1
    reference_variance <- max(empirical_variance, sqrt(.Machine$double.eps))
  }
  template <- rqr_freeze_discount_template(
    model = model, n_time = T, df = df, dim.df = dim.df,
    reference_variance = reference_variance,
    reference_design = reference_design
  )
  template$reference_source <- reference_source
  template$empirical_bayes <- identical(reference_source, "training_response_variance")
  template
}

#' Fit a dynamic RQR interval model with alternating FFBS
#'
#' This sampler targets root trajectories under an exponentiated RQR loss and
#' Gaussian state priors. It does not define a response likelihood. Fixed `W`
#' and frozen discount templates define exact generalized-posterior samplers;
#' adaptive discount recursion is an explicitly experimental working update.
#'
#' @param y Response vector. `NA` values are treated as missing observations.
#' @param model An [rqr_as_dlm_model()] or exdqlm-compatible model.
#' @param coverage_level Interval coverage target in `(0,1)`.
#' @param evolution_mode One of `"fixed_W"`, `"discount_template"`, or
#'   `"adaptive_discount"`.
#' @param W Fixed evolution covariance matrix or cube.
#' @param df,dim.df Component discounts and their state dimensions.
#' @param reference_variance,reference_design Inputs used to freeze a discount
#'   template before MCMC.
#' @param learning_rate Fixed learning rate or initial learned inverse scale.
#' @param loss_reference_scale Positive scale `s_L` dividing the loss.
#' @param learning_rate_mode `"fixed"`, normalized `"learned_scale"`, or
#'   diagnostic `"learned_pure"`.
#' @param lambda_prior Gamma shape--rate prior.
#' @param mcmc_control Iteration, seed, storage, backend, progress, and jitter
#'   controls.
#' @param init Optional initial states, latent scales, and lambda.
#' @return An `rqr_dlm_mcmc` object.
#' @export
rqr_dlm_fit <- function(y, model, coverage_level,
                        evolution_mode = c("fixed_W", "discount_template", "adaptive_discount"),
                        W = NULL, df = NULL, dim.df = NULL,
                        reference_variance = NULL, reference_design = NULL,
                        learning_rate = 1, loss_reference_scale = 1,
                        learning_rate_mode = c("fixed", "learned_scale", "learned_pure"),
                        lambda_prior = list(shape = 4, rate = 4),
                        mcmc_control = list(), init = list()) {
  y <- as.numeric(y)
  if (!length(y) || !any(is.finite(y)) || any(!is.finite(y) & !is.na(y))) {
    stop("y must contain at least one observed finite value; missing values must be NA.", call. = FALSE)
  }
  model <- rqr_as_dlm_model(model)
  expanded <- .rqr_expand_model(model, length(y))
  T <- length(y)
  p <- expanded$p
  observed <- is.finite(y)
  n_obs <- sum(observed)
  evolution_mode <- match.arg(evolution_mode)
  learning_rate_mode <- .rqr_learning_rate_mode(learning_rate_mode)
  lambda_prior <- .rqr_lambda_prior(lambda_prior, learning_rate_mode)
  loss_reference_scale <- as.numeric(loss_reference_scale)[1L]
  if (!is.finite(loss_reference_scale) || loss_reference_scale <= 0) {
    stop("loss_reference_scale must be finite and positive.", call. = FALSE)
  }
  lambda <- as.numeric(init$lambda %||% learning_rate)[1L]
  if (!is.finite(lambda) || lambda <= 0) stop("Initial learning_rate/lambda must be positive.", call. = FALSE)
  learn_lambda <- learning_rate_mode != "fixed"
  constants <- rqr_constants(coverage_level, lambda / loss_reference_scale)
  evolution <- .rqr_dlm_evolution(
    evolution_mode, model, expanded, y, W, df, dim.df,
    reference_variance, reference_design
  )
  if (!isTRUE(evolution$exact_joint_target)) {
    warning(
      "adaptive_discount is an experimental working/sequential recursion, not an exact Gibbs sampler for a declared fixed joint target.",
      call. = FALSE
    )
  }

  if (!is.list(mcmc_control)) stop("mcmc_control must be a list.", call. = FALSE)
  n_burn <- max(0L, as.integer(mcmc_control$n_burn %||% 500L))
  n_keep <- max(1L, as.integer(mcmc_control$n_mcmc %||% 1000L))
  thin <- max(1L, as.integer(mcmc_control$thin %||% 1L))
  seed <- mcmc_control$seed %||% NULL
  if (!is.null(seed)) set.seed(as.integer(seed)[1L])
  backend <- match.arg(mcmc_control$backend %||% "cpp", c("cpp", "R", "auto"))
  store_state_draws <- isTRUE(mcmc_control$store_state_draws %||% TRUE)
  store_latent_draws <- isTRUE(mcmc_control$store_latent_draws %||% FALSE)
  verbose <- isTRUE(mcmc_control$verbose %||% FALSE)
  progress_every <- max(1L, as.integer(mcmc_control$progress_every %||% 100L))
  jitter_ladder <- as.numeric(mcmc_control$jitter_ladder %||% c(0, 1e-12, 1e-10, 1e-8, 1e-6))

  paths <- .rqr_init_state_paths(y, expanded$FF, expanded$m0, coverage_level, init)
  theta1 <- paths$theta1
  theta2 <- paths$theta2
  v <- rep_len(as.numeric(init$latent_v %||% loss_reference_scale / lambda), T)
  if (length(v) != T || any(!is.finite(v)) || any(v <= 0)) v <- rep(loss_reference_scale / lambda, T)

  eta1_draws <- matrix(NA_real_, T, n_keep)
  eta2_draws <- matrix(NA_real_, T, n_keep)
  theta1_draws <- if (store_state_draws) array(NA_real_, c(p, T, n_keep)) else NULL
  theta2_draws <- if (store_state_draws) array(NA_real_, c(p, T, n_keep)) else NULL
  v_draws <- if (store_latent_draws) matrix(NA_real_, T, n_keep) else NULL
  lambda_draws <- numeric(n_keep)
  total_iter <- n_burn + n_keep * thin
  loss_trace <- lambda_trace <- effective_rate_trace <- numeric(total_iter)
  lambda_shape_trace <- lambda_rate_trace <- rep(NA_real_, total_iter)
  ffbs_root1 <- ffbs_root2 <- vector("list", total_iter)
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

    H1 <- sweep(expanded$FF, 2L, y - eta2, `*`)
    H1[, !observed] <- 0
    z1 <- y * (y - eta2) - constants$xi * v
    z1[!observed] <- NA_real_
    draw1 <- rqr_ffbs_sample(
      z1, H1, obs_variance, expanded$GG, expanded$m0, expanded$C0,
      evolution, backend = backend, jitter_ladder = jitter_ladder
    )
    theta1 <- draw1$path
    eta1 <- .rqr_state_ordinates(expanded$FF, theta1)

    H2 <- sweep(expanded$FF, 2L, y - eta1, `*`)
    H2[, !observed] <- 0
    z2 <- y * (y - eta1) - constants$xi * v
    z2[!observed] <- NA_real_
    draw2 <- rqr_ffbs_sample(
      z2, H2, obs_variance, expanded$GG, expanded$m0, expanded$C0,
      evolution, backend = backend, jitter_ladder = jitter_ladder
    )
    theta2 <- draw2$path
    eta2 <- .rqr_state_ordinates(expanded$FF, theta2)

    loss_trace[iter] <- sum(rqr_check_loss(
      rqr_residual_product(y[observed], eta1[observed], eta2[observed]), constants$alpha
    ))
    lambda_trace[iter] <- lambda
    effective_rate_trace[iter] <- lambda / loss_reference_scale
    ffbs_root1[[iter]] <- draw1$diagnostics
    ffbs_root2[[iter]] <- draw2$diagnostics

    if (iter > n_burn && (iter - n_burn) %% thin == 0L) {
      save_idx <- save_idx + 1L
      eta1_draws[, save_idx] <- eta1
      eta2_draws[, save_idx] <- eta2
      lambda_draws[save_idx] <- lambda
      if (store_state_draws) {
        theta1_draws[, , save_idx] <- theta1
        theta2_draws[, , save_idx] <- theta2
      }
      if (store_latent_draws) v_draws[, save_idx] <- v
    }
    if (verbose && (iter %% progress_every == 0L || iter == total_iter)) {
      message(sprintf("[rqr_dlm_fit] iter %d/%d loss=%.6g", iter, total_iter, loss_trace[iter]))
    }
  }

  lower <- pmin(eta1_draws, eta2_draws)
  upper <- pmax(eta1_draws, eta2_draws)
  lambda_summary <- .rqr_lambda_summary(lambda_draws)
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
      coverage_level = constants$alpha,
      learning_rate_mode = learning_rate_mode,
      learning_rate = if (learn_lambda) lambda_summary$mean else lambda,
      loss_reference_scale = loss_reference_scale,
      effective_learning_rate = if (learn_lambda) mean(lambda_draws / loss_reference_scale) else lambda / loss_reference_scale,
      lambda_prior = lambda_prior,
      lambda_summary = lambda_summary,
      generalized_bayes = TRUE,
      response_likelihood = FALSE,
      evolution_mode = evolution_mode,
      exact_joint_target = isTRUE(evolution$exact_joint_target),
      root_priors_exchangeable = TRUE
    ),
    samp.eta_root1 = eta1_draws,
    samp.eta_root2 = eta2_draws,
    samp.theta_root1 = theta1_draws,
    samp.theta_root2 = theta2_draws,
    samp.lambda = lambda_draws,
    samp.latent_v = v_draws,
    summary = list(
      lower_mean = rowMeans(lower), upper_mean = rowMeans(upper),
      midpoint_mean = rowMeans(0.5 * (lower + upper)),
      width_mean = rowMeans(upper - lower),
      coverage_in_sample = mean(y[observed] >= rowMeans(lower)[observed] & y[observed] <= rowMeans(upper)[observed]),
      width_mean_scalar = mean(rowMeans(upper - lower))
    ),
    diagnostics = list(
      loss_trace = loss_trace,
      scaled_loss_trace = loss_trace / loss_reference_scale,
      lambda_trace = lambda_trace,
      effective_learning_rate_trace = effective_rate_trace,
      lambda_post_shape_trace = lambda_shape_trace,
      lambda_post_rate_trace = lambda_rate_trace,
      ffbs_root1 = ffbs_root1,
      ffbs_root2 = ffbs_root2,
      partial_collapse_order = c("lambda_collapsed", "latent_v_refresh", "root1_ffbs", "root2_ffbs")
    ),
    last = list(theta_root1 = theta1, theta_root2 = theta2, latent_v = v, lambda = lambda),
    misc = list(
      n_burn = n_burn, n_mcmc = n_keep, thin = thin, seed = seed,
      backend = backend, observed = observed,
      note = "Root trajectory draws arise from a generalized-Bayes loss update; they are not response draws."
    )
  )
  class(out) <- c("rqr_dlm_mcmc", "rqr_fit")
  out
}

#' @export
rqr_posterior_draws.rqr_dlm_mcmc <- function(object, nd = NULL, seed = NULL, ...) {
  if (!inherits(object, "rqr_dlm_mcmc")) stop("Expected an rqr_dlm_mcmc object.", call. = FALSE)
  if (!is.null(seed)) set.seed(as.integer(seed)[1L])
  n_save <- ncol(object$samp.eta_root1)
  idx <- if (is.null(nd) || is.na(nd)) seq_len(n_save) else {
    nd <- max(1L, as.integer(nd)[1L])
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
#' draws. Future evolution covariances are explicit so the forecasting contract
#' cannot silently reuse an in-sample adaptive recursion.
#'
#' @param object An `rqr_dlm_mcmc` fit with stored state draws.
#' @param FF_future State-by-horizon observation design.
#' @param GG_future Evolution matrix or cube.
#' @param W_future Evolution covariance matrix or cube.
#' @param nd Number of saved MCMC draws to use.
#' @param seed Optional seed.
#' @return Future root and ordered endpoint draws.
#' @export
rqr_forecast_roots <- function(object, FF_future, GG_future, W_future, nd = NULL, seed = NULL) {
  if (!inherits(object, "rqr_dlm_mcmc")) stop("Expected an rqr_dlm_mcmc object.", call. = FALSE)
  if (is.null(object$samp.theta_root1) || is.null(object$samp.theta_root2)) {
    stop("Forecasting requires mcmc_control$store_state_draws=TRUE.", call. = FALSE)
  }
  if (!is.null(seed)) set.seed(as.integer(seed)[1L])
  FF_future <- as.matrix(FF_future)
  p <- dim(object$samp.theta_root1)[1L]
  H <- ncol(FF_future)
  if (nrow(FF_future) != p || H < 1L || any(!is.finite(FF_future))) stop("FF_future must be finite p x H.", call. = FALSE)
  GG <- .rqr_expand_cube(GG_future, H, p, "GG_future")
  W <- .rqr_expand_cube(W_future, H, p, "W_future")
  n_save <- dim(object$samp.theta_root1)[3L]
  idx <- if (is.null(nd) || is.na(nd)) seq_len(n_save) else {
    nd <- max(1L, as.integer(nd)[1L])
    sample.int(n_save, nd, replace = nd > n_save)
  }
  root1 <- root2 <- matrix(NA_real_, H, length(idx))
  for (j in seq_along(idx)) {
    s1 <- object$samp.theta_root1[, dim(object$samp.theta_root1)[2L], idx[j]]
    s2 <- object$samp.theta_root2[, dim(object$samp.theta_root2)[2L], idx[j]]
    for (hh in seq_len(H)) {
      mu1 <- drop(GG[, , hh] %*% s1)
      mu2 <- drop(GG[, , hh] %*% s2)
      s1 <- .rqr_sample_mvnorm_covariance(mu1, W[, , hh])$draw
      s2 <- .rqr_sample_mvnorm_covariance(mu2, W[, , hh])$draw
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
    interpretation = "Future interval-root state draws; no response simulation contract is implied."
  )
}

#' @export
print.rqr_dlm_mcmc <- function(x, ...) {
  cat("RQR dynamic MCMC fit\n")
  cat(sprintf("  coverage_level: %.4f\n", x$model_spec$coverage_level))
  cat(sprintf("  evolution_mode: %s\n", x$model_spec$evolution_mode))
  cat(sprintf("  exact target:   %s\n", if (x$model_spec$exact_joint_target) "yes" else "no (experimental working recursion)"))
  cat(sprintf("  draws:          %d\n", ncol(x$samp.eta_root1)))
  cat("  interpretation: generalized-Bayes root paths, not response draws\n")
  invisible(x)
}
