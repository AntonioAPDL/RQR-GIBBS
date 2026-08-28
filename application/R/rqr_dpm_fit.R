.rqr_dpm_logsumexp <- function(x) {
  m <- max(x)
  if (!is.finite(m)) return(m)
  m + log(sum(exp(x - m)))
}

.rqr_dpm_row_log_normalize <- function(log_prob) {
  n <- nrow(log_prob)
  out <- matrix(0, nrow = n, ncol = ncol(log_prob))
  for (i in seq_len(n)) {
    z <- .rqr_dpm_logsumexp(log_prob[i, ])
    out[i, ] <- exp(log_prob[i, ] - z)
  }
  out
}

.rqr_dpm_stick_to_weights <- function(v) {
  v <- as.numeric(v)
  if (!length(v) || any(!is.finite(v)) || any(v < 0) || any(v > 1)) {
    stop("Stick-breaking weights must be finite values in [0, 1].",
         call. = FALSE)
  }
  k <- length(v)
  if (k == 1L) return(1)
  v[[k]] <- 1
  remaining <- cumprod(c(1, 1 - v[-k]))
  weights <- v * remaining
  weights / sum(weights)
}

.rqr_dpm_weights_to_stick <- function(weights, eps = 1e-8) {
  weights <- as.numeric(weights)
  weights <- pmax(weights, eps)
  weights <- weights / sum(weights)
  k <- length(weights)
  if (k == 1L) return(1)
  v <- numeric(k)
  residual <- 1
  for (j in seq_len(k - 1L)) {
    v[[j]] <- min(1 - eps, max(eps, weights[[j]] / residual))
    residual <- residual * (1 - v[[j]])
  }
  v[[k]] <- 1
  v
}

.rqr_dpm_default_hyper <- function(y, hyper = list()) {
  y <- as.numeric(y)
  s2 <- stats::var(y)
  if (!is.finite(s2) || s2 <= 0) s2 <- 1
  m0 <- as.numeric(hyper$m0 %||% mean(y))[1L]
  kappa0 <- .rqr_bayes_assert_positive(
    hyper$kappa0 %||% 0.1, "hyper$kappa0"
  )
  a0 <- .rqr_bayes_assert_positive(hyper$a0 %||% 2, "hyper$a0")
  b0 <- .rqr_bayes_assert_positive(hyper$b0 %||% s2, "hyper$b0")
  a_alpha <- .rqr_bayes_assert_positive(
    hyper$a_alpha %||% 2, "hyper$a_alpha"
  )
  b_alpha <- .rqr_bayes_assert_positive(
    hyper$b_alpha %||% 2, "hyper$b_alpha"
  )
  if (!is.finite(m0)) stop("hyper$m0 must be finite.", call. = FALSE)
  list(
    m0 = m0,
    kappa0 = kappa0,
    a0 = a0,
    b0 = b0,
    a_alpha = a_alpha,
    b_alpha = b_alpha
  )
}

.rqr_dpm_validate_state <- function(weights, mu, lambda) {
  weights <- as.numeric(weights)
  mu <- as.numeric(mu)
  lambda <- as.numeric(lambda)
  k <- length(weights)
  if (!k || length(mu) != k || length(lambda) != k ||
      any(!is.finite(weights)) || any(!is.finite(mu)) ||
      any(!is.finite(lambda)) || any(weights < 0) || sum(weights) <= 0 ||
      any(lambda <= 0)) {
    stop("Invalid DPM state.", call. = FALSE)
  }
  list(weights = weights / sum(weights), mu = mu, lambda = lambda)
}

.rqr_dpm_init_state <- function(y, truncation_level, concentration, hyper,
                                init = list()) {
  k <- .rqr_mt_assert_count(truncation_level, "truncation_level", 1L)
  alpha <- .rqr_bayes_assert_positive(concentration, "concentration")
  s <- stats::sd(y)
  if (!is.finite(s) || s <= 0) s <- 1
  probs <- seq(0.1, 0.9, length.out = k)
  default_mu <- as.numeric(stats::quantile(y, probs = probs, names = FALSE))
  if (length(unique(default_mu)) < k) {
    default_mu <- seq(mean(y) - s, mean(y) + s, length.out = k)
  }
  mu <- as.numeric(init$mu %||% default_mu)
  lambda <- as.numeric(init$lambda %||% rep(1 / s^2, k))
  weights <- as.numeric(init$weights %||% rep(1 / k, k))
  if (length(mu) != k || length(lambda) != k || length(weights) != k) {
    stop("init$weights, init$mu, and init$lambda must match truncation_level.",
         call. = FALSE)
  }
  state <- .rqr_dpm_validate_state(weights, mu, lambda)
  state$sticks <- .rqr_dpm_weights_to_stick(state$weights)
  state$alpha <- alpha
  state$hyper <- hyper
  state
}

.rqr_dpm_log_component_density <- function(y, weights, mu, lambda) {
  state <- .rqr_dpm_validate_state(weights, mu, lambda)
  n <- length(y)
  k <- length(state$weights)
  out <- matrix(NA_real_, nrow = n, ncol = k)
  for (j in seq_len(k)) {
    out[, j] <- log(state$weights[[j]]) +
      stats::dnorm(y, mean = state$mu[[j]],
                   sd = 1 / sqrt(state$lambda[[j]]), log = TRUE)
  }
  out
}

.rqr_dpm_loglik <- function(y, weights, mu, lambda) {
  log_prob <- .rqr_dpm_log_component_density(y, weights, mu, lambda)
  sum(apply(log_prob, 1L, .rqr_dpm_logsumexp))
}

.rqr_dpm_allocation_entropy <- function(resp) {
  -sum(resp * log(pmax(resp, .Machine$double.xmin))) / nrow(resp)
}

.rqr_dpm_component_posterior <- function(y, members, hyper) {
  if (length(members)) {
    yk <- y[members]
    nk <- length(yk)
    ybar <- mean(yk)
    ss <- sum((yk - ybar)^2)
  } else {
    nk <- 0L
    ybar <- 0
    ss <- 0
  }
  kappa_n <- hyper$kappa0 + nk
  m_n <- if (nk) {
    (hyper$kappa0 * hyper$m0 + nk * ybar) / kappa_n
  } else {
    hyper$m0
  }
  a_n <- hyper$a0 + nk / 2
  b_n <- hyper$b0 + 0.5 * ss
  if (nk) {
    b_n <- b_n + 0.5 * hyper$kappa0 * nk / kappa_n *
      (ybar - hyper$m0)^2
  }
  list(kappa_n = kappa_n, m_n = m_n, a_n = a_n, b_n = b_n)
}

.rqr_dpm_fractional_component_posterior <- function(y, r, hyper) {
  nk <- sum(r)
  if (nk > 0) {
    ybar <- sum(r * y) / nk
    ss <- sum(r * (y - ybar)^2)
  } else {
    ybar <- 0
    ss <- 0
  }
  kappa_n <- hyper$kappa0 + nk
  m_n <- if (nk > 0) {
    (hyper$kappa0 * hyper$m0 + nk * ybar) / kappa_n
  } else {
    hyper$m0
  }
  a_n <- hyper$a0 + nk / 2
  b_n <- hyper$b0 + 0.5 * ss
  if (nk > 0) {
    b_n <- b_n + 0.5 * hyper$kappa0 * nk / kappa_n *
      (ybar - hyper$m0)^2
  }
  list(kappa_n = kappa_n, m_n = m_n, a_n = a_n, b_n = b_n, nk = nk)
}

.rqr_dpm_draw_allocations <- function(resp) {
  z <- integer(nrow(resp))
  for (i in seq_len(nrow(resp))) {
    z[[i]] <- sample.int(ncol(resp), size = 1L, prob = resp[i, ])
  }
  z
}

.rqr_dpm_draw_state <- function(y, z, sticks, alpha, hyper,
                                sample_concentration = FALSE) {
  k <- length(sticks)
  counts <- tabulate(z, nbins = k)
  mu <- numeric(k)
  lambda <- numeric(k)
  for (j in seq_len(k)) {
    post <- .rqr_dpm_component_posterior(y, which(z == j), hyper)
    lambda[[j]] <- stats::rgamma(1L, shape = post$a_n, rate = post$b_n)
    mu[[j]] <- stats::rnorm(
      1L, mean = post$m_n, sd = 1 / sqrt(post$kappa_n * lambda[[j]])
    )
  }
  if (k > 1L) {
    for (j in seq_len(k - 1L)) {
      sticks[[j]] <- stats::rbeta(
        1L, shape1 = 1 + counts[[j]],
        shape2 = alpha + sum(counts[(j + 1L):k])
      )
    }
  }
  sticks[[k]] <- 1
  if (isTRUE(sample_concentration) && k > 1L) {
    log_residual <- sum(log(pmax(1 - sticks[-k], .Machine$double.xmin)))
    rate <- hyper$b_alpha - log_residual
    alpha <- stats::rgamma(
      1L, shape = hyper$a_alpha + k - 1L, rate = rate
    )
  }
  weights <- .rqr_dpm_stick_to_weights(sticks)
  list(
    weights = weights,
    mu = mu,
    lambda = lambda,
    sticks = sticks,
    alpha = alpha,
    counts = counts
  )
}

.rqr_dpm_state_digest <- function(parameters) {
  if (!length(parameters)) return(NA_character_)
  .rqr_bayes_digest(list(
    first = parameters[[1L]],
    last = parameters[[length(parameters)]],
    n = length(parameters)
  ))
}

#' Fit a truncated Gaussian Dirichlet-process mixture posterior
#'
#' Fits a smooth response-distribution posterior using a finite
#' stick-breaking Gaussian DPM approximation.  This is a response likelihood
#' model and is separate from the RQR generalized-Bayes loss updates.
#'
#' @param y Numeric univariate responses.
#' @param truncation_level Number of stick-breaking atoms.
#' @param concentration Positive DP concentration parameter.
#' @param sample_concentration Sample `concentration` from its Gamma posterior
#'   conditional on the sticks.
#' @param hyper Normal-Gamma and concentration hyperparameters.
#' @param mcmc_control List with `n_iter`, `burn_in`, `thin`, `seed`, and
#'   `store_allocations`.
#' @param init Optional initial `weights`, `mu`, and `lambda`.
#' @param na_rm Remove missing responses.
#' @return An `rqr_dpm_mcmc` object.
#' @export
rqr_dpm_fit <- function(y, truncation_level = 10L, concentration = 1,
                        sample_concentration = FALSE, hyper = list(),
                        mcmc_control = list(), init = list(),
                        na_rm = FALSE) {
  y <- .rqr_bayes_clean_y(y, na_rm = na_rm)
  k <- .rqr_mt_assert_count(truncation_level, "truncation_level", 1L)
  hyper <- .rqr_dpm_default_hyper(y, hyper)
  n_iter <- .rqr_mt_assert_count(
    mcmc_control$n_iter %||% 400L, "mcmc_control$n_iter", 1L
  )
  burn_in <- .rqr_mt_assert_count(
    mcmc_control$burn_in %||% floor(n_iter / 2), "mcmc_control$burn_in", 0L
  )
  thin <- .rqr_mt_assert_count(mcmc_control$thin %||% 5L, "mcmc_control$thin", 1L)
  if (burn_in >= n_iter) {
    stop("mcmc_control$burn_in must be smaller than n_iter.", call. = FALSE)
  }
  seed <- mcmc_control$seed %||% NULL
  restore_rng <- .rqr_mt_seed_scope(seed)
  on.exit(restore_rng(), add = TRUE)
  state <- .rqr_dpm_init_state(y, k, concentration, hyper, init = init)
  resp <- .rqr_dpm_row_log_normalize(
    .rqr_dpm_log_component_density(
      y, state$weights, state$mu, state$lambda
    )
  )
  z <- .rqr_dpm_draw_allocations(resp)
  if (burn_in + thin > n_iter) {
    stop("mcmc_control leaves no retained DPM posterior draws.",
         call. = FALSE)
  }
  keep_iters <- seq.int(burn_in + thin, n_iter, by = thin)
  n_keep <- length(keep_iters)
  parameters <- vector("list", n_keep)
  allocation_draws <- if (isTRUE(mcmc_control$store_allocations %||% FALSE)) {
    vector("list", n_keep)
  } else {
    NULL
  }
  trace <- data.frame(
    draw = integer(0),
    iter = integer(0),
    alpha = numeric(0),
    log_likelihood = numeric(0),
    occupied_components = integer(0),
    final_stick_weight = numeric(0),
    allocation_entropy = numeric(0)
  )
  save_idx <- 0L
  for (iter in seq_len(n_iter)) {
    resp <- .rqr_dpm_row_log_normalize(
      .rqr_dpm_log_component_density(
        y, state$weights, state$mu, state$lambda
      )
    )
    z <- .rqr_dpm_draw_allocations(resp)
    state <- .rqr_dpm_draw_state(
      y, z, state$sticks, state$alpha, hyper,
      sample_concentration = sample_concentration
    )
    if (iter %in% keep_iters) {
      save_idx <- save_idx + 1L
      parameters[[save_idx]] <- list(
        weights = state$weights,
        mu = state$mu,
        lambda = state$lambda,
        alpha = state$alpha
      )
      if (!is.null(allocation_draws)) allocation_draws[[save_idx]] <- z
      trace[save_idx, ] <- list(
        draw = save_idx,
        iter = iter,
        alpha = state$alpha,
        log_likelihood = .rqr_dpm_loglik(
          y, state$weights, state$mu, state$lambda
        ),
        occupied_components = sum(tabulate(z, nbins = k) > 0L),
        final_stick_weight = state$weights[[k]],
        allocation_entropy = .rqr_dpm_allocation_entropy(resp)
      )
    }
  }
  out <- list(
    schema_version = "rqrgibbs_gaussian_dpm_mcmc/1.0.0",
    model = "truncated_gaussian_dirichlet_process_mixture_response_distribution",
    y = y,
    sample_size = length(y),
    truncation_level = as.integer(k),
    concentration = concentration,
    sample_concentration = isTRUE(sample_concentration),
    hyper = hyper,
    mcmc_control = list(
      n_iter = as.integer(n_iter),
      burn_in = as.integer(burn_in),
      thin = as.integer(thin),
      seed = if (is.null(seed)) NA_integer_ else as.integer(seed),
      store_allocations = !is.null(allocation_draws)
    ),
    parameters = parameters,
    trace = trace,
    allocations = allocation_draws,
    response_likelihood = TRUE,
    generalized_bayes = FALSE,
    posterior_predictive_distribution_available = TRUE,
    uq_role = "smooth_response_distribution_posterior",
    provenance = .rqr_bayes_provenance(seed = seed, extra = list(
      hyper_digest = .rqr_bayes_digest(hyper)
    ))
  )
  out$provenance_digest <- .rqr_bayes_digest(list(
    y_digest = .rqr_bayes_digest(y),
    truncation_level = k,
    concentration = concentration,
    sample_concentration = isTRUE(sample_concentration),
    hyper = hyper,
    mcmc_control = out$mcmc_control,
    parameter_digest = .rqr_dpm_state_digest(parameters)
  ))
  class(out) <- c("dpm_mcmc", "rqr_dpm_mcmc", "distribution_fit",
                  "rqr_distribution_fit", "list")
  out
}

.rqr_dpm_mixture_density <- function(x, weights, mu, lambda) {
  state <- .rqr_dpm_validate_state(weights, mu, lambda)
  x <- as.numeric(x)
  dens <- numeric(length(x))
  for (j in seq_along(state$weights)) {
    dens <- dens + state$weights[[j]] *
      stats::dnorm(x, mean = state$mu[[j]],
                   sd = 1 / sqrt(state$lambda[[j]]))
  }
  dens
}

.rqr_dpm_mixture_cdf <- function(x, weights, mu, lambda) {
  state <- .rqr_dpm_validate_state(weights, mu, lambda)
  x <- as.numeric(x)
  cdf <- numeric(length(x))
  for (j in seq_along(state$weights)) {
    cdf <- cdf + state$weights[[j]] *
      stats::pnorm(x, mean = state$mu[[j]],
                   sd = 1 / sqrt(state$lambda[[j]]))
  }
  pmax(0, pmin(1, cdf))
}

.rqr_dpm_interval_mass_one <- function(lower, upper, weights, mu, lambda) {
  .rqr_dpm_mixture_cdf(upper, weights, mu, lambda) -
    .rqr_dpm_mixture_cdf(lower, weights, mu, lambda)
}

.rqr_dpm_interval_raw_moment_one <- function(lower, upper, weights, mu, lambda) {
  state <- .rqr_dpm_validate_state(weights, mu, lambda)
  total <- 0
  for (j in seq_along(state$weights)) {
    sd <- 1 / sqrt(state$lambda[[j]])
    a <- (lower - state$mu[[j]]) / sd
    b <- (upper - state$mu[[j]]) / sd
    mass <- stats::pnorm(b) - stats::pnorm(a)
    moment <- state$mu[[j]] * mass + sd *
      (stats::dnorm(a) - stats::dnorm(b))
    total <- total + state$weights[[j]] * moment
  }
  total
}

.rqr_dpm_mixture_quantile <- function(p, weights, mu, lambda,
                                      tolerance = 1e-8) {
  p <- as.numeric(p)[1L]
  if (!is.finite(p) || p < 0 || p > 1) {
    stop("p must be one finite probability in [0, 1].", call. = FALSE)
  }
  p <- min(1 - tolerance, max(tolerance, p))
  state <- .rqr_dpm_validate_state(weights, mu, lambda)
  sd <- 1 / sqrt(state$lambda)
  lo <- min(state$mu - 12 * sd)
  hi <- max(state$mu + 12 * sd)
  expand <- 0L
  while (.rqr_dpm_mixture_cdf(lo, state$weights, state$mu, state$lambda) > p &&
         expand < 40L) {
    span <- hi - lo
    lo <- lo - span
    expand <- expand + 1L
  }
  expand <- 0L
  while (.rqr_dpm_mixture_cdf(hi, state$weights, state$mu, state$lambda) < p &&
         expand < 40L) {
    span <- hi - lo
    hi <- hi + span
    expand <- expand + 1L
  }
  stats::uniroot(
    function(x) .rqr_dpm_mixture_cdf(
      x, state$weights, state$mu, state$lambda
    ) - p,
    lower = lo, upper = hi, tol = tolerance
  )$root
}

#' Gaussian DPM posterior density
#'
#' @param fit A Gaussian DPM fit.
#' @param x Evaluation points.
#' @param draw Optional retained draw index.  If omitted, returns the posterior
#'   mean density across retained draws.
#' @return A data frame with density estimates.
#' @export
rqr_dpm_density <- function(fit, x, draw = NULL) {
  if (!inherits(fit, "rqr_dpm_mcmc") && !inherits(fit, "rqr_dpm_ecm")) {
    stop("fit must be from rqr_dpm_fit() or rqr_dpm_ecm_fit().",
         call. = FALSE)
  }
  x <- as.numeric(x)
  if (any(!is.finite(x))) stop("x must be finite.", call. = FALSE)
  params <- if (inherits(fit, "rqr_dpm_ecm")) {
    list(fit$parameters)
  } else {
    fit$parameters
  }
  if (!is.null(draw)) params <- params[[as.integer(draw)[1L]]]
  if (is.list(params) && !is.null(params$weights)) {
    dens <- .rqr_dpm_mixture_density(x, params$weights, params$mu, params$lambda)
  } else {
    mat <- vapply(params, function(p) {
      .rqr_dpm_mixture_density(x, p$weights, p$mu, p$lambda)
    }, numeric(length(x)))
    dens <- rowMeans(mat)
  }
  data.frame(x = x, density = dens)
}

#' Gaussian DPM posterior CDF
#'
#' @inheritParams rqr_dpm_density
#' @return A data frame with CDF estimates.
#' @export
rqr_dpm_cdf <- function(fit, x, draw = NULL) {
  if (!inherits(fit, "rqr_dpm_mcmc") && !inherits(fit, "rqr_dpm_ecm")) {
    stop("fit must be from rqr_dpm_fit() or rqr_dpm_ecm_fit().",
         call. = FALSE)
  }
  x <- as.numeric(x)
  if (any(!is.finite(x))) stop("x must be finite.", call. = FALSE)
  params <- if (inherits(fit, "rqr_dpm_ecm")) {
    list(fit$parameters)
  } else {
    fit$parameters
  }
  if (!is.null(draw)) params <- params[[as.integer(draw)[1L]]]
  if (is.list(params) && !is.null(params$weights)) {
    cdf <- .rqr_dpm_mixture_cdf(x, params$weights, params$mu, params$lambda)
  } else {
    mat <- vapply(params, function(p) {
      .rqr_dpm_mixture_cdf(x, p$weights, p$mu, p$lambda)
    }, numeric(length(x)))
    cdf <- rowMeans(mat)
  }
  data.frame(x = x, cdf = cdf)
}

#' Gaussian DPM mixture quantile
#'
#' @param fit A Gaussian DPM fit.
#' @param p Probabilities in (0, 1).
#' @param draw Retained draw index; required for MCMC fits.
#' @param tolerance Root-finding tolerance.
#' @return A data frame with quantiles.
#' @export
rqr_dpm_quantile <- function(fit, p, draw = 1L, tolerance = 1e-8) {
  if (!inherits(fit, "rqr_dpm_mcmc") && !inherits(fit, "rqr_dpm_ecm")) {
    stop("fit must be from rqr_dpm_fit() or rqr_dpm_ecm_fit().",
         call. = FALSE)
  }
  p <- as.numeric(p)
  if (!length(p) || any(!is.finite(p)) || any(p <= 0) || any(p >= 1)) {
    stop("p must contain finite probabilities in (0, 1).", call. = FALSE)
  }
  params <- if (inherits(fit, "rqr_dpm_ecm")) {
    fit$parameters
  } else {
    fit$parameters[[as.integer(draw)[1L]]]
  }
  q <- vapply(p, .rqr_dpm_mixture_quantile, numeric(1L),
              weights = params$weights, mu = params$mu,
              lambda = params$lambda, tolerance = tolerance)
  data.frame(p = p, quantile = q)
}

#' Gaussian DPM posterior content probability
#'
#' Estimates `Pr{F([lower, upper]) >= content | y}` across retained mixture
#' draws.
#'
#' @param fit A Gaussian DPM MCMC fit.
#' @param lower,upper Interval endpoints.
#' @param content Required population content.
#' @return A data frame with Monte Carlo posterior content summaries.
#' @export
rqr_dpm_content_probability <- function(fit, lower, upper, content) {
  if (!inherits(fit, "rqr_dpm_mcmc")) {
    stop("fit must come from rqr_dpm_fit().", call. = FALSE)
  }
  c_target <- .rqr_bayes_assert_probability(content, "content")
  lower <- as.numeric(lower)
  upper <- as.numeric(upper)
  if (length(lower) != length(upper) || any(!is.finite(lower)) ||
      any(!is.finite(upper)) || any(lower > upper)) {
    stop("lower and upper must be finite vectors with lower <= upper.",
         call. = FALSE)
  }
  rows <- lapply(seq_along(lower), function(ii) {
    masses <- vapply(fit$parameters, function(p) {
      .rqr_dpm_interval_mass_one(
        lower[[ii]], upper[[ii]], p$weights, p$mu, p$lambda
      )
    }, numeric(1L))
    data.frame(
      lower = lower[[ii]],
      upper = upper[[ii]],
      content = c_target,
      posterior_mean_content = mean(masses),
      posterior_sd_content = stats::sd(masses),
      posterior_q05_content = as.numeric(stats::quantile(
        masses, 0.05, names = FALSE
      )),
      posterior_q50_content = as.numeric(stats::quantile(
        masses, 0.50, names = FALSE
      )),
      posterior_q95_content = as.numeric(stats::quantile(
        masses, 0.95, names = FALSE
      )),
      posterior_content_probability = mean(masses >= c_target),
      n_posterior_draws = length(masses)
    )
  })
  do.call(rbind, rows)
}

.rqr_dpm_shortest_one <- function(params, target_content, grid_size,
                                  optimize, tolerance) {
  q <- .rqr_bayes_assert_probability(target_content, "target_content")
  grid_size <- .rqr_mt_assert_count(grid_size, "grid_size", 5L)
  max_lower_tail <- 1 - q
  u_grid <- seq(0, max_lower_tail, length.out = grid_size)
  q_lower <- vapply(u_grid, .rqr_dpm_mixture_quantile, numeric(1L),
                    weights = params$weights, mu = params$mu,
                    lambda = params$lambda, tolerance = tolerance)
  q_upper <- vapply(u_grid + q, .rqr_dpm_mixture_quantile, numeric(1L),
                    weights = params$weights, mu = params$mu,
                    lambda = params$lambda, tolerance = tolerance)
  widths <- q_upper - q_lower
  min_width <- min(widths)
  idx <- which(widths <= min_width + tolerance)[[1L]]
  u_star <- u_grid[[idx]]
  width_star <- widths[[idx]]
  if (isTRUE(optimize) && length(u_grid) > 2L) {
    step <- u_grid[[2L]] - u_grid[[1L]]
    lo <- max(0, u_star - step)
    hi <- min(max_lower_tail, u_star + step)
    opt <- stats::optimize(function(u) {
      .rqr_dpm_mixture_quantile(
        u + q, params$weights, params$mu, params$lambda,
        tolerance = tolerance
      ) -
        .rqr_dpm_mixture_quantile(
          u, params$weights, params$mu, params$lambda,
          tolerance = tolerance
        )
    }, interval = c(lo, hi))
    if (is.finite(opt$objective) && opt$objective < width_star + tolerance) {
      u_star <- opt$minimum
      width_star <- opt$objective
    }
  }
  lower <- .rqr_dpm_mixture_quantile(
    u_star, params$weights, params$mu, params$lambda,
    tolerance = tolerance
  )
  upper <- .rqr_dpm_mixture_quantile(
    u_star + q, params$weights, params$mu, params$lambda,
    tolerance = tolerance
  )
  mass <- .rqr_dpm_interval_mass_one(
    lower, upper, params$weights, params$mu, params$lambda
  )
  raw_moment <- .rqr_dpm_interval_raw_moment_one(
    lower, upper, params$weights, params$mu, params$lambda
  )
  retained_mean <- raw_moment / mass
  full_mean <- sum(params$weights * params$mu)
  list(
    lower = lower,
    upper = upper,
    width = upper - lower,
    retained_mass = mass,
    retained_mean = retained_mean,
    full_mean = full_mean,
    lower_tail_mass = u_star,
    tilt = retained_mean - full_mean,
    tie_count = sum(abs(widths - min_width) <= tolerance),
    tie_rule = "deterministic_first_grid_minimum_with_local_refinement",
    boundary_status = if (u_star <= tolerance &&
                          u_star + q >= 1 - tolerance) {
      "both"
    } else if (u_star <= tolerance) {
      "lower"
    } else if (u_star + q >= 1 - tolerance) {
      "upper"
    } else {
      "interior"
    }
  )
}

#' Shortest-interval functionals from Gaussian DPM posterior draws
#'
#' @param fit A Gaussian DPM MCMC fit.
#' @param target_content Shortest-interval content.
#' @param grid_size Number of lower-tail grid points for each draw.
#' @param optimize Locally refine the best grid interval.
#' @param tolerance Numerical tolerance.
#' @return A data frame of posterior shortest-function draws.
#' @export
rqr_dpm_shortest_draws <- function(fit, target_content, grid_size = 101L,
                                   optimize = TRUE, tolerance = 1e-8) {
  if (!inherits(fit, "rqr_dpm_mcmc")) {
    stop("fit must come from rqr_dpm_fit().", call. = FALSE)
  }
  q <- .rqr_bayes_assert_probability(target_content, "target_content")
  rows <- lapply(seq_along(fit$parameters), function(s) {
    sh <- .rqr_dpm_shortest_one(
      fit$parameters[[s]], target_content = q, grid_size = grid_size,
      optimize = optimize, tolerance = tolerance
    )
    data.frame(
      draw = as.integer(s),
      engine = "gaussian_dpm",
      lower = sh$lower,
      upper = sh$upper,
      width = sh$width,
      retained_mass = sh$retained_mass,
      retained_mean = sh$retained_mean,
      full_mean = sh$full_mean,
      lower_tail_mass = sh$lower_tail_mass,
      tilt = sh$tilt,
      tie_count = as.integer(sh$tie_count),
      tie_rule = sh$tie_rule,
      boundary_status = sh$boundary_status,
      truncation_level = fit$truncation_level
    )
  })
  out <- do.call(rbind, rows)
  attr(out, "schema_version") <- "rqrgibbs_dpm_shortest_draws/1.0.0"
  attr(out, "target_content") <- q
  attr(out, "functional_method") <- "quantile_width_grid_with_local_refinement"
  out
}

#' Gaussian DPM Bayesian tolerance action
#'
#' Searches closed order-statistic intervals using Monte Carlo posterior
#' content probabilities from retained DPM draws.
#'
#' @param fit A Gaussian DPM MCMC fit.
#' @param content Required population content.
#' @param posterior_confidence Required posterior probability.
#' @param action_class Candidate class.
#' @return A Bayesian action object.
#' @export
rqr_dpm_bayes_tolerance_action <- function(
    fit, content, posterior_confidence,
    action_class = "closed_order_statistic_intervals") {
  if (!inherits(fit, "rqr_dpm_mcmc")) {
    stop("fit must come from rqr_dpm_fit().", call. = FALSE)
  }
  c_target <- .rqr_bayes_assert_probability(content, "content")
  post_conf <- .rqr_bayes_assert_probability(
    posterior_confidence, "posterior_confidence"
  )
  action_class <- match.arg(
    as.character(action_class)[1L], "closed_order_statistic_intervals"
  )
  candidates <- .rqr_order_interval_candidates(fit$y)
  probs <- rqr_dpm_content_probability(
    fit, candidates$lower, candidates$upper, c_target
  )
  candidates$posterior_content_probability <-
    probs$posterior_content_probability
  feasible <- candidates[
    candidates$posterior_content_probability >= post_conf, , drop = FALSE
  ]
  selected <- feasible[order(feasible$width, feasible$lower_index,
                             feasible$upper_index), , drop = FALSE]
  selected <- if (nrow(selected)) selected[1L, , drop = FALSE] else selected
  out <- list(
    schema_version = "rqrgibbs_dpm_bayes_tolerance_action/1.0.0",
    action_name = "DPM-B",
    action_class = action_class,
    content = c_target,
    posterior_confidence = post_conf,
    posterior_threshold = post_conf,
    selected = selected,
    candidates_evaluated = nrow(candidates),
    feasible_count = nrow(feasible),
    posterior_constraint_status =
      if (nrow(feasible)) "satisfied" else "infeasible_within_candidate_class",
    candidates = candidates
  )
  class(out) <- c("dpm_bayes_tolerance_action",
                  "rqr_dpm_bayes_tolerance_action", "list")
  out
}

.rqr_dpm_ecm_responsibilities <- function(y, weights, mu, lambda) {
  .rqr_dpm_row_log_normalize(
    .rqr_dpm_log_component_density(y, weights, mu, lambda)
  )
}

.rqr_dpm_ecm_mstep <- function(y, resp, hyper, eps = 1e-8) {
  k <- ncol(resp)
  nk <- colSums(resp)
  weights <- pmax(nk, eps)
  weights <- weights / sum(weights)
  mu <- numeric(k)
  lambda <- numeric(k)
  for (j in seq_len(k)) {
    post <- .rqr_dpm_fractional_component_posterior(y, resp[, j], hyper)
    mu[[j]] <- post$m_n
    shape <- post$a_n
    lambda[[j]] <- if (shape > 1) (shape - 1) / post$b_n else shape / post$b_n
    lambda[[j]] <- max(lambda[[j]], eps)
  }
  list(weights = weights, mu = mu, lambda = lambda)
}

.rqr_dpm_ecm_objective <- function(y, weights, mu, lambda, hyper) {
  ll <- .rqr_dpm_loglik(y, weights, mu, lambda)
  comp <- 0
  for (j in seq_along(weights)) {
    comp <- comp +
      stats::dgamma(lambda[[j]], shape = hyper$a0, rate = hyper$b0,
                    log = TRUE) +
      stats::dnorm(mu[[j]], mean = hyper$m0,
                   sd = 1 / sqrt(hyper$kappa0 * lambda[[j]]), log = TRUE)
  }
  ll + comp
}

#' Deterministic ECM fit for a truncated Gaussian DPM
#'
#' Fits a smooth Gaussian-mixture MAP surrogate.  This path is useful for
#' initialization and diagnostics; it is not a substitute for posterior
#' uncertainty summaries.
#'
#' @inheritParams rqr_dpm_fit
#' @param optimize_concentration Reserved deterministic concentration update.
#' @param ecm_control List with `max_iter`, `tol_objective`,
#'   `monotone_tolerance`, `seed`, and `backtracking_max_steps`.
#' @return An `rqr_dpm_ecm` object.
#' @export
rqr_dpm_ecm_fit <- function(y, truncation_level = 10L, concentration = 1,
                            optimize_concentration = FALSE, hyper = list(),
                            ecm_control = list(), init = list(),
                            na_rm = FALSE) {
  y <- .rqr_bayes_clean_y(y, na_rm = na_rm)
  k <- .rqr_mt_assert_count(truncation_level, "truncation_level", 1L)
  alpha <- .rqr_bayes_assert_positive(concentration, "concentration")
  hyper <- .rqr_dpm_default_hyper(y, hyper)
  max_iter <- .rqr_mt_assert_count(
    ecm_control$max_iter %||% 100L, "ecm_control$max_iter", 1L
  )
  tol_objective <- .rqr_bayes_assert_nonnegative(
    ecm_control$tol_objective %||% 1e-7, "ecm_control$tol_objective"
  )
  monotone_tolerance <- .rqr_bayes_assert_nonnegative(
    ecm_control$monotone_tolerance %||% 1e-10,
    "ecm_control$monotone_tolerance"
  )
  backtracking_max_steps <- .rqr_mt_assert_count(
    ecm_control$backtracking_max_steps %||% 20L,
    "ecm_control$backtracking_max_steps", 0L
  )
  seed <- ecm_control$seed %||% NULL
  restore_rng <- .rqr_mt_seed_scope(seed)
  on.exit(restore_rng(), add = TRUE)
  state <- .rqr_dpm_init_state(y, k, alpha, hyper, init = init)
  objective <- .rqr_dpm_ecm_objective(
    y, state$weights, state$mu, state$lambda, hyper
  )
  trace <- data.frame(
    iter = 0L,
    objective = objective,
    observed_log_likelihood = .rqr_dpm_loglik(
      y, state$weights, state$mu, state$lambda
    ),
    max_parameter_change = NA_real_,
    backtracking_steps = 0L,
    alpha = alpha
  )
  converged <- FALSE
  for (iter in seq_len(max_iter)) {
    resp <- .rqr_dpm_ecm_responsibilities(
      y, state$weights, state$mu, state$lambda
    )
    proposal <- .rqr_dpm_ecm_mstep(y, resp, hyper)
    if (isTRUE(optimize_concentration)) {
      proposal$alpha <- alpha
    }
    proposal_obj <- .rqr_dpm_ecm_objective(
      y, proposal$weights, proposal$mu, proposal$lambda, hyper
    )
    step <- 1
    bt <- 0L
    while (proposal_obj + monotone_tolerance < objective &&
           bt < backtracking_max_steps) {
      step <- step / 2
      proposal$weights <- step * proposal$weights + (1 - step) * state$weights
      proposal$weights <- proposal$weights / sum(proposal$weights)
      proposal$mu <- step * proposal$mu + (1 - step) * state$mu
      proposal$lambda <- step * proposal$lambda + (1 - step) * state$lambda
      proposal_obj <- .rqr_dpm_ecm_objective(
        y, proposal$weights, proposal$mu, proposal$lambda, hyper
      )
      bt <- bt + 1L
    }
    delta <- max(
      abs(proposal$weights - state$weights),
      abs(proposal$mu - state$mu),
      abs(proposal$lambda - state$lambda)
    )
    state$weights <- proposal$weights
    state$mu <- proposal$mu
    state$lambda <- proposal$lambda
    state$sticks <- .rqr_dpm_weights_to_stick(state$weights)
    objective_delta <- proposal_obj - objective
    objective <- proposal_obj
    trace[nrow(trace) + 1L, ] <- list(
      iter = iter,
      objective = objective,
      observed_log_likelihood = .rqr_dpm_loglik(
        y, state$weights, state$mu, state$lambda
      ),
      max_parameter_change = delta,
      backtracking_steps = bt,
      alpha = alpha
    )
    if (abs(objective_delta) <= tol_objective || delta <= tol_objective) {
      converged <- TRUE
      break
    }
  }
  out <- list(
    schema_version = "rqrgibbs_gaussian_dpm_ecm/1.0.0",
    model = "truncated_gaussian_dpm_map_response_distribution_surrogate",
    y = y,
    sample_size = length(y),
    truncation_level = as.integer(k),
    concentration = alpha,
    optimize_concentration = isTRUE(optimize_concentration),
    hyper = hyper,
    parameters = list(
      weights = state$weights,
      mu = state$mu,
      lambda = state$lambda,
      alpha = alpha
    ),
    trace = trace,
    converged = converged,
    response_likelihood = TRUE,
    generalized_bayes = FALSE,
    posterior_predictive_distribution_available = FALSE,
    uq_role = "deterministic_map_diagnostic_not_posterior_uq",
    provenance = .rqr_bayes_provenance(seed = seed)
  )
  out$provenance_digest <- .rqr_bayes_digest(list(
    y_digest = .rqr_bayes_digest(y),
    truncation_level = k,
    concentration = alpha,
    hyper = hyper,
    trace_tail = utils::tail(trace, 1L)
  ))
  class(out) <- c("dpm_ecm", "rqr_dpm_ecm", "distribution_map",
                  "rqr_distribution_map", "list")
  out
}

print.rqr_dpm_mcmc <- function(x, ...) {
  cat(
    sprintf(
      "Gaussian DPM shortest-interval fit: n=%d, K=%d, retained_draws=%d\n",
      x$sample_size, x$truncation_level, length(x$parameters)
    )
  )
  invisible(x)
}

print.rqr_dpm_ecm <- function(x, ...) {
  cat(
    sprintf(
      "Gaussian DPM ECM diagnostic fit: n=%d, K=%d, converged=%s\n",
      x$sample_size, x$truncation_level, x$converged
    )
  )
  invisible(x)
}
