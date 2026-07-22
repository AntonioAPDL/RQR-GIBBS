# RQR utilities deliberately keep their own fallback for the package's common
# null-coalescing idiom so source load order cannot affect the new backend.
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

#' RQR loss constants
#'
#' @param coverage_level Interval coverage level in `(0, 1)`.
#' @param learning_rate Positive generalized-Bayes learning rate.
#' @return A named list with `alpha`, `omega`, `sigma`, `xi`, and `phi`.
#' @export
rqr_constants <- function(coverage_level, learning_rate = 1) {
  alpha <- as.numeric(coverage_level)[1L]
  omega <- as.numeric(learning_rate)[1L]
  if (!is.finite(alpha) || alpha <= 0 || alpha >= 1) {
    stop("coverage_level must be a finite scalar in (0, 1).", call. = FALSE)
  }
  if (!is.finite(omega) || omega <= 0) {
    stop("learning_rate must be a finite positive scalar.", call. = FALSE)
  }
  list(
    alpha = alpha,
    omega = omega,
    sigma = 1 / omega,
    xi = (1 - 2 * alpha) / (alpha * (1 - alpha)),
    phi = 2 / (alpha * (1 - alpha))
  )
}

#' RQR check loss
#'
#' @param u Numeric residual product.
#' @param coverage_level Interval coverage level.
#' @return Numeric vector of RQR check losses.
#' @export
rqr_check_loss <- function(u, coverage_level) {
  alpha <- rqr_constants(coverage_level)$alpha
  u <- as.numeric(u)
  u * (alpha - as.numeric(u < 0))
}

#' RQR root residual product
#'
#' @param y Response vector.
#' @param eta1,eta2 Numeric root ordinates.
#' @return Numeric vector `(y - eta1) * (y - eta2)`.
#' @export
rqr_residual_product <- function(y, eta1, eta2) {
  y <- as.numeric(y)
  eta1 <- as.numeric(eta1)
  eta2 <- as.numeric(eta2)
  if (length(y) != length(eta1) || length(y) != length(eta2)) {
    stop("y, eta1, and eta2 must have the same length.", call. = FALSE)
  }
  (y - eta1) * (y - eta2)
}

#' RQR pseudo residual from the transformed AL representation
#'
#' @param y Response vector.
#' @param eta1,eta2 Numeric root ordinates.
#' @return Numeric vector `y^2 - y * (eta1 + eta2) + eta1 * eta2`.
#' @export
rqr_pseudo_residual <- function(y, eta1, eta2) {
  y <- as.numeric(y)
  eta1 <- as.numeric(eta1)
  eta2 <- as.numeric(eta2)
  if (length(y) != length(eta1) || length(y) != length(eta2)) {
    stop("y, eta1, and eta2 must have the same length.", call. = FALSE)
  }
  y^2 - y * (eta1 + eta2) + eta1 * eta2
}

#' RQR ordered endpoints
#'
#' @param eta1,eta2 Numeric root ordinates.
#' @return A list with `lower`, `upper`, `midpoint`, and `width`.
#' @export
rqr_order_endpoints <- function(eta1, eta2) {
  eta1 <- as.numeric(eta1)
  eta2 <- as.numeric(eta2)
  if (length(eta1) != length(eta2)) {
    stop("eta1 and eta2 must have the same length.", call. = FALSE)
  }
  lower <- pmin(eta1, eta2)
  upper <- pmax(eta1, eta2)
  list(
    lower = lower,
    upper = upper,
    midpoint = 0.5 * (lower + upper),
    width = upper - lower
  )
}

#' RQR GIG parameters for latent pseudo-AL scales
#'
#' The returned `a` and `b` match the package GIG convention proportional to
#' `x^(p - 1) exp(-(a * x + b / x) / 2)`.
#'
#' @param e Residual product.
#' @param coverage_level Interval coverage level.
#' @param learning_rate Positive generalized-Bayes learning rate.
#' @return A list with `p`, `a`, and `b`.
#' @export
rqr_gig_params <- function(e, coverage_level, learning_rate = 1) {
  cc <- rqr_constants(coverage_level, learning_rate)
  e <- as.numeric(e)
  list(
    p = 0.5,
    a = 1 / (2 * cc$sigma * cc$alpha * (1 - cc$alpha)),
    b = cc$alpha * (1 - cc$alpha) * e^2 / (2 * cc$sigma)
  )
}

.rqr_learning_rate_mode <- function(learning_rate_mode) {
  mode <- tolower(as.character(learning_rate_mode %||% "fixed_rate")[1L])
  mode <- switch(mode,
    fixed = "fixed_rate",
    learned = "learned_pseudoresidual_normalized",
    scale = "learned_pseudoresidual_normalized",
    learned_scale = "learned_pseudoresidual_normalized",
    learned_loss_scale = "learned_pseudoresidual_normalized",
    pure = "learned_pure",
    mode
  )
  choices <- c("fixed_rate", "learned_pseudoresidual_normalized", "learned_pure")
  if (!mode %in% choices) {
    stop(
      sprintf(
        "learning_rate_mode must be one of {'%s'}.",
        paste(choices, collapse = "','")
      ),
      call. = FALSE
    )
  }
  mode
}

.rqr_lambda_prior <- function(lambda_prior = list(), learning_rate_mode = "fixed_rate") {
  mode <- .rqr_learning_rate_mode(learning_rate_mode)
  if (inherits(lambda_prior, "rqr_lambda_prior")) {
    if (!identical(lambda_prior$mode, mode)) {
      stop("Internal lambda prior mode does not match learning_rate_mode.", call. = FALSE)
    }
    return(lambda_prior)
  }
  if (is.null(lambda_prior)) lambda_prior <- list()
  if (!is.list(lambda_prior)) {
    stop("lambda_prior must be a list with positive shape and rate.", call. = FALSE)
  }
  shape <- as.numeric(lambda_prior$shape %||% lambda_prior$a %||% 4)[1L]
  rate <- as.numeric(lambda_prior$rate %||% lambda_prior$b %||% 4)[1L]
  if (!is.null(lambda_prior$power) || !is.null(lambda_prior$nu)) {
    stop(
      "lambda_prior$power and lambda_prior$nu are not accepted: the normalized and pure targets have fixed powers.",
      call. = FALSE
    )
  }
  power <- if (identical(mode, "learned_pseudoresidual_normalized")) 1 else 0
  if (identical(mode, "fixed_rate")) {
    if (!is.finite(shape)) shape <- 4
    if (!is.finite(rate)) rate <- 4
  }
  if (!is.finite(shape) || shape <= 0) stop("lambda_prior$shape must be positive.", call. = FALSE)
  if (!is.finite(rate) || rate <= 0) stop("lambda_prior$rate must be positive.", call. = FALSE)
  structure(
    list(shape = shape, rate = rate, power = power, mode = mode),
    class = c("rqr_lambda_prior", "list")
  )
}

.rqr_lambda_posterior_params <- function(loss_sum, n, lambda_prior, learning_rate_mode) {
  mode <- .rqr_learning_rate_mode(learning_rate_mode)
  prior <- .rqr_lambda_prior(lambda_prior, mode)
  loss_sum <- as.numeric(loss_sum)[1L]
  n <- as.integer(n)[1L]
  if (!is.finite(loss_sum) || loss_sum < 0) stop("loss_sum must be finite and nonnegative.", call. = FALSE)
  if (!is.finite(n) || n <= 0L) stop("n must be a positive integer.", call. = FALSE)
  if (identical(mode, "fixed_rate")) {
    return(list(shape = NA_real_, rate = NA_real_, power_count = 0))
  }
  power_count <- prior$power * n
  list(
    shape = prior$shape + power_count,
    rate = prior$rate + loss_sum,
    power_count = power_count
  )
}

.rqr_target_formula <- function(learning_rate_mode) {
  mode <- .rqr_learning_rate_mode(learning_rate_mode)
  switch(mode,
    fixed_rate = "pi(theta|y) proportional to pi(theta) exp{-omega_R L(theta)}",
    learned_pseudoresidual_normalized = paste0(
      "pi(theta,lambda|y) proportional to pi(theta) pi(lambda) ",
      "lambda^n_obs exp{-lambda L(theta)/s_L}"
    ),
    learned_pure = paste0(
      "pi(theta,lambda|y) proportional to pi(theta) pi(lambda) ",
      "exp{-lambda L(theta)/s_L}"
    )
  )
}

.rqr_scalar_integer <- function(x, name, minimum = 0L, maximum = .Machine$integer.max) {
  if (length(x) != 1L || is.na(x) || !is.finite(x) || x != floor(x) ||
      x < minimum || x > maximum) {
    stop(
      sprintf("%s must be one finite integer in [%d, %d].", name, minimum, maximum),
      call. = FALSE
    )
  }
  as.integer(x)
}

.rqr_positive_integer_vector <- function(x, name) {
  if (!is.numeric(x) || !length(x) || anyNA(x) || any(!is.finite(x)) ||
      any(x != floor(x)) || any(x < 1) || any(x > .Machine$integer.max)) {
    stop(sprintf("%s must contain positive integers.", name), call. = FALSE)
  }
  as.integer(x)
}

.rqr_rng_state <- function() {
  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) return(NULL)
  get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
}

.rqr_restore_rng <- function(state) {
  if (is.null(state)) return(invisible(FALSE))
  state <- as.integer(state)
  if (length(state) < 2L || anyNA(state)) {
    stop("init$rng_state must be a complete integer .Random.seed vector.", call. = FALSE)
  }
  assign(".Random.seed", state, envir = .GlobalEnv)
  invisible(TRUE)
}

.rqr_digest <- function(object) {
  digest::digest(object, algo = "sha256", serialize = TRUE)
}

.rqr_find_repo_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = FALSE)
  repeat {
    if (file.exists(file.path(current, ".git")) &&
        file.exists(file.path(current, "application", "DESCRIPTION"))) return(current)
    parent <- dirname(current)
    if (identical(parent, current)) return(NA_character_)
    current <- parent
  }
}

.rqr_git_value <- function(repo_root, args) {
  if (is.na(repo_root) || !nzchar(repo_root) || !nzchar(Sys.which("git"))) return(NA_character_)
  out <- suppressWarnings(tryCatch(
    system2("git", c("-C", repo_root, args), stdout = TRUE, stderr = FALSE),
    error = function(e) character(0)
  ))
  if (!length(out)) NA_character_ else paste(out, collapse = "\n")
}

.rqr_provenance <- function(data, matrices = list(), numerical_policy = NA_character_,
                            initial_seed = NULL, repo_root = NULL) {
  if (is.null(repo_root)) repo_root <- .rqr_find_repo_root()
  pkg_version <- tryCatch(as.character(utils::packageVersion("rqrgibbs")), error = function(e) NA_character_)
  dependency_names <- c("Rcpp", "RcppArmadillo", "digest")
  dependency_versions <- vapply(dependency_names, function(pkg) {
    tryCatch(as.character(utils::packageVersion(pkg)), error = function(e) NA_character_)
  }, character(1L))
  git_commit <- .rqr_git_value(repo_root, c("rev-parse", "HEAD"))
  dirty <- .rqr_git_value(repo_root, c("status", "--porcelain"))
  ext <- extSoftVersion()
  list(
    schema_version = "rqrgibbs_fit/1.0.0",
    package_version = pkg_version,
    git_commit = git_commit,
    git_dirty = !is.na(dirty) && nzchar(dirty),
    repo_root = repo_root,
    R_version = R.version.string,
    platform = R.version$platform,
    compiler = R.version$compiler %||% NA_character_,
    BLAS = unname(ext["BLAS"] %||% NA_character_),
    LAPACK = unname(ext["LAPACK"] %||% NA_character_),
    dependency_versions = dependency_versions,
    RNGkind = RNGkind(),
    initial_seed = initial_seed,
    numerical_policy = numerical_policy,
    data_digest = .rqr_digest(data),
    matrix_digests = lapply(matrices, .rqr_digest),
    recorded_at = format(Sys.time(), tz = "UTC", usetz = TRUE)
  )
}

.rqr_sample_lambda_collapsed <- function(loss_sum, n, lambda_prior, learning_rate_mode) {
  pp <- .rqr_lambda_posterior_params(loss_sum, n, lambda_prior, learning_rate_mode)
  stats::rgamma(1L, shape = pp$shape, rate = pp$rate)
}

.rqr_loss_sum <- function(y, X, beta1, beta2, coverage_level) {
  eta1 <- drop(X %*% beta1)
  eta2 <- drop(X %*% beta2)
  sum(rqr_check_loss(rqr_residual_product(y, eta1, eta2), coverage_level))
}

.rqr_lambda_summary <- function(lambda_draws) {
  lambda_draws <- as.numeric(lambda_draws)
  lambda_draws <- lambda_draws[is.finite(lambda_draws) & lambda_draws > 0]
  if (!length(lambda_draws)) {
    return(list(
      mean = NA_real_, median = NA_real_, sd = NA_real_,
      q05 = NA_real_, q25 = NA_real_, q75 = NA_real_, q95 = NA_real_,
      implied_sigma_mean = NA_real_
    ))
  }
  qs <- as.numeric(stats::quantile(lambda_draws, probs = c(0.05, 0.25, 0.75, 0.95), names = FALSE, type = 8))
  list(
    mean = mean(lambda_draws),
    median = stats::median(lambda_draws),
    sd = stats::sd(lambda_draws),
    q05 = qs[1L],
    q25 = qs[2L],
    q75 = qs[3L],
    q95 = qs[4L],
    implied_sigma_mean = mean(1 / lambda_draws)
  )
}

.rqr_assert_xy <- function(y, X) {
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  y <- as.numeric(y)
  if (!length(y) || !nrow(X) || length(y) != nrow(X)) {
    stop("y must be numeric with length equal to nrow(X).", call. = FALSE)
  }
  if (!ncol(X)) stop("X must have at least one column.", call. = FALSE)
  if (any(!is.finite(y)) || any(!is.finite(X))) {
    stop("y and X must contain only finite values.", call. = FALSE)
  }
  list(y = y, X = X)
}

.rqr_intercept_index <- function(X, tol = 1e-12) {
  X <- as.matrix(X)
  for (j in seq_len(ncol(X))) {
    xj <- X[, j]
    if (all(is.finite(xj)) && max(abs(xj - xj[1L])) <= tol && abs(xj[1L] - 1) <= tol) {
      return(j)
    }
  }
  NA_integer_
}

.rqr_init_roots <- function(y, X, coverage_level, init = list()) {
  p <- ncol(X)
  b1 <- init$beta1 %||% init$beta_root1 %||% NULL
  b2 <- init$beta2 %||% init$beta_root2 %||% NULL
  if (!is.null(b1) || !is.null(b2)) {
    if (is.null(b1) || is.null(b2)) stop("init must provide both beta1 and beta2.", call. = FALSE)
    b1 <- as.numeric(b1)
    b2 <- as.numeric(b2)
    if (length(b1) != p || length(b2) != p) stop("Initial beta vectors must have ncol(X) entries.", call. = FALSE)
    return(list(beta1 = b1, beta2 = b2))
  }
  alpha <- rqr_constants(coverage_level)$alpha
  probs <- c((1 - alpha) / 2, 1 - (1 - alpha) / 2)
  qs <- as.numeric(stats::quantile(y, probs = probs, names = FALSE, type = 8))
  beta1 <- rep(0, p)
  beta2 <- rep(0, p)
  jj <- .rqr_intercept_index(X)
  if (is.na(jj)) jj <- 1L
  beta1[jj] <- qs[1L]
  beta2[jj] <- qs[2L]
  list(beta1 = beta1, beta2 = beta2)
}

.rqr_prior_precision <- function(beta_prior_obj, state, p) {
  if (is.null(beta_prior_obj)) {
    return(rep(1 / 1e4, p))
  }
  type <- as.character(beta_prior_obj$type %||% "ridge")[1L]
  if (identical(type, "ridge")) {
    tau2 <- as.numeric(beta_prior_obj$hypers$tau2 %||% 1e4)[1L]
    if (!is.finite(tau2) || tau2 <= 0) stop("ridge tau2 must be positive.", call. = FALSE)
    return(rep(1 / tau2, p))
  }
  if (identical(type, "rhs_ns")) {
    return(.exal_mcmc_rhs_ns_precisions(state, p = p))
  }
  stop("RQR currently supports beta_prior_obj$type in {'ridge','rhs_ns'}.", call. = FALSE)
}

.rqr_prior_state_init <- function(beta_prior_obj, p, init_state = NULL) {
  type <- as.character(beta_prior_obj$type %||% "ridge")[1L]
  if (identical(type, "ridge")) return(list())
  if (identical(type, "rhs_ns")) {
    st <- .exal_mcmc_rhs_ns_prepare_state(beta_prior_obj, p = p, init = list(beta_prior_state = init_state))
    return(st)
  }
  stop("RQR currently supports beta_prior_obj$type in {'ridge','rhs_ns'}.", call. = FALSE)
}

.rqr_prior_state_update <- function(beta_prior_obj, state, beta, freeze_tau = FALSE) {
  type <- as.character(beta_prior_obj$type %||% "ridge")[1L]
  if (identical(type, "ridge")) {
    return(list(state = state, stats = list()))
  }
  if (identical(type, "rhs_ns")) {
    return(.exal_mcmc_rhs_ns_gibbs_update(state, beta, beta_prior_obj, freeze_tau = freeze_tau))
  }
  stop("RQR currently supports beta_prior_obj$type in {'ridge','rhs_ns'}.", call. = FALSE)
}

.rqr_beta_update <- function(y, X, beta_other, V, constants, prior_prec,
                             precision_beta_cfg = list(), context = list()) {
  eta_other <- drop(X %*% beta_other)
  A <- X * as.numeric(y - eta_other)
  z <- y^2 - y * eta_other - constants$xi * V
  W <- 1 / (constants$phi * constants$sigma * V)
  Prec <- crossprod(A * sqrt(W)) + diag(as.numeric(prior_prec), ncol(X))
  rhs <- crossprod(A, W * z)
  .exal_mcmc_sample_mvnorm_prec(
    rhs = as.numeric(rhs),
    Prec = Prec,
    precision_beta_cfg = precision_beta_cfg,
    context = context
  )
}

.rqr_precision_mean <- function(Prec, rhs) {
  Uc <- chol((Prec + t(Prec)) / 2)
  as.numeric(backsolve(Uc, forwardsolve(t(Uc), rhs)))
}

.rqr_fit_summary <- function(y, X, beta1_draws, beta2_draws) {
  eta1 <- X %*% t(beta1_draws)
  eta2 <- X %*% t(beta2_draws)
  lower <- pmin(eta1, eta2)
  upper <- pmax(eta1, eta2)
  lower_mean <- rowMeans(lower)
  upper_mean <- rowMeans(upper)
  covered_by_draw <- sweep(lower, 1L, y, `<=`) & sweep(upper, 1L, y, `>=`)
  coverage_by_draw <- colMeans(covered_by_draw)
  list(
    beta_root1_mean = colMeans(beta1_draws),
    beta_root2_mean = colMeans(beta2_draws),
    lower_mean = lower_mean,
    upper_mean = upper_mean,
    midpoint_mean = rowMeans(0.5 * (lower + upper)),
    width_mean = rowMeans(upper - lower),
    coverage_posterior_mean_endpoints = mean(y >= lower_mean & y <= upper_mean),
    coverage_draw_mean = mean(coverage_by_draw),
    coverage_draw_quantiles = stats::quantile(
      coverage_by_draw, c(0.05, 0.5, 0.95), names = TRUE, type = 8
    ),
    width_mean_scalar = mean(rowMeans(upper - lower))
  )
}

#' Draw posterior beta samples from an RQR fit
#'
#' @param object An RQR fit object.
#' @param nd Number of draws. `NULL` keeps all available MCMC draws.
#' @param seed Optional RNG seed.
#' @param ... Reserved.
#' @return A list with `beta_root1`, `beta_root2`, and `nd`.
#' @export
rqr_posterior_draws <- function(object, nd = NULL, seed = NULL, ...) {
  UseMethod("rqr_posterior_draws")
}

#' Predict RQR intervals
#'
#' @param object An RQR fit object.
#' @param ... Method-specific arguments.
#' @return A list of interval draws and summaries.
#' @export
predict_interval <- function(object, ...) {
  UseMethod("predict_interval")
}
