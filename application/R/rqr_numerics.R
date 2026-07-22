# Internal numerical utilities for the standalone package.

.rqr_symmetrize <- function(x) {
  x <- as.matrix(x)
  0.5 * (x + t(x))
}

.rqr_chol_with_jitter <- function(x, jitter_ladder = c(0, 1e-12, 1e-10, 1e-8, 1e-6)) {
  x <- .rqr_symmetrize(x)
  if (nrow(x) != ncol(x) || any(!is.finite(x))) {
    stop("Matrix must be finite and square.", call. = FALSE)
  }
  scale <- max(1, max(abs(diag(x))))
  ladder <- unique(as.numeric(jitter_ladder))
  ladder <- ladder[is.finite(ladder) & ladder >= 0]
  for (jj in ladder) {
    candidate <- if (jj == 0) x else x + diag(jj * scale, nrow(x))
    ans <- tryCatch(chol(candidate), error = function(e) NULL)
    if (!is.null(ans)) {
      return(list(chol = ans, matrix = candidate, jitter = jj * scale))
    }
  }
  stop("Positive-definite Cholesky factorization failed after the declared jitter ladder.", call. = FALSE)
}

.rqr_sample_mvnorm_precision <- function(rhs, precision,
                                          jitter_ladder = c(0, 1e-12, 1e-10, 1e-8, 1e-6)) {
  rhs <- as.numeric(rhs)
  precision <- as.matrix(precision)
  if (nrow(precision) != length(rhs) || ncol(precision) != length(rhs)) {
    stop("precision dimension must match rhs.", call. = FALSE)
  }
  fac <- .rqr_chol_with_jitter(precision, jitter_ladder)
  mean <- backsolve(fac$chol, forwardsolve(t(fac$chol), rhs))
  draw <- as.numeric(mean + backsolve(fac$chol, stats::rnorm(length(rhs))))
  list(draw = draw, mean = as.numeric(mean), info = list(
    strategy = if (fac$jitter == 0) "cholesky" else "cholesky_jitter",
    jitter = fac$jitter
  ))
}

.rqr_sample_mvnorm_covariance <- function(mean, covariance,
                                           jitter_ladder = c(0, 1e-12, 1e-10, 1e-8, 1e-6)) {
  mean <- as.numeric(mean)
  covariance <- .rqr_symmetrize(covariance)
  if (!all(dim(covariance) == c(length(mean), length(mean))) ||
      any(!is.finite(covariance))) {
    stop("covariance dimension must match mean and contain finite values.", call. = FALSE)
  }
  direct <- tryCatch(chol(covariance), error = function(e) NULL)
  if (!is.null(direct)) {
    return(list(
      draw = as.numeric(mean + t(direct) %*% stats::rnorm(length(mean))),
      info = list(strategy = "cholesky", jitter = 0)
    ))
  }
  ee <- eigen(covariance, symmetric = TRUE)
  scale <- max(1, max(abs(ee$values)))
  if (min(ee$values) >= -1e-10 * scale) {
    values <- pmax(ee$values, 0)
    return(list(
      draw = as.numeric(mean + ee$vectors %*% (sqrt(values) * stats::rnorm(length(mean)))),
      info = list(
        strategy = "psd_eigen",
        jitter = 0,
        min_eigenvalue = min(ee$values),
        clamped_eigenvalues = sum(ee$values < 0)
      )
    ))
  }
  fac <- .rqr_chol_with_jitter(covariance, jitter_ladder)
  list(
    draw = as.numeric(mean + t(fac$chol) %*% stats::rnorm(length(mean))),
    info = list(strategy = "cholesky_jitter", jitter = fac$jitter)
  )
}

# Compatibility name used by the implementation seed files. The standalone
# implementation intentionally performs only declared Cholesky+jitter repair;
# it does not silently replace an indefinite precision by an SVD absolute value.
.exal_mcmc_sample_mvnorm_prec <- function(rhs, Prec, precision_beta_cfg = list(), context = list()) {
  ladder <- precision_beta_cfg$jitter_ladder %||% c(0, 1e-12, 1e-10, 1e-8, 1e-6)
  .rqr_sample_mvnorm_precision(rhs, Prec, jitter_ladder = ladder)
}

.exal_normalize_mcmc_precision_beta_cfg <- function(precision_cfg = NULL) {
  precision_cfg <- precision_cfg %||% list()
  if (!is.list(precision_cfg)) stop("precision configuration must be a list.", call. = FALSE)
  list(
    enabled = TRUE,
    symmetrize = TRUE,
    jitter_ladder = precision_cfg$jitter_ladder %||% c(0, 1e-12, 1e-10, 1e-8, 1e-6),
    eigen_fallback = FALSE,
    trace = isTRUE(precision_cfg$trace %||% TRUE)
  )
}

.exal_gig_floor <- function() sqrt(.Machine$double.eps)

.rqr_rinvgauss <- function(n, mean, shape) {
  n <- as.integer(n)[1L]
  mean <- rep_len(as.numeric(mean), n)
  shape <- rep_len(as.numeric(shape), n)
  if (n < 1L || any(!is.finite(mean)) || any(mean <= 0) ||
      any(!is.finite(shape)) || any(shape <= 0)) {
    stop("Inverse-Gaussian mean and shape must be finite and positive.", call. = FALSE)
  }
  z2 <- stats::rnorm(n)^2
  w <- mean * z2 / shape
  # Stable forms of mu * {1 + w/2 - sqrt(w^2 + 4w)/2}.
  x <- mean
  small <- w > 0 & w < 1
  x[small] <- mean[small] /
    (1 + 0.5 * w[small] + 0.5 * sqrt(w[small]) * sqrt(w[small] + 4))
  large <- w >= 1
  x[large] <- (shape[large] / z2[large]) /
    (1 / w[large] + 0.5 + 0.5 * sqrt(1 + 4 / w[large]))
  choose_x <- stats::runif(n) <= mean / (mean + x)
  ifelse(choose_x, x, mean^2 / x)
}

#' Sample the GIG distribution used by RQR
#'
#' Samples `GIG(1/2,a,b)` under density convention proportional to
#' `v^(-1/2) exp(-(a*v+b/v)/2)`. The exact `b=0` limit is handled separately.
#'
#' @param b Nonnegative vector.
#' @param a Positive scalar or vector.
#' @return A positive numeric vector.
#' @export
rqr_sample_gig_half <- function(b, a) {
  b <- as.numeric(b)
  a <- rep_len(as.numeric(a), length(b))
  if (!length(b) || any(!is.finite(a)) || any(a <= 0) ||
      any(!is.finite(b)) || any(b < 0)) {
    stop("a must be positive and b must be nonnegative.", call. = FALSE)
  }
  out <- numeric(length(b))
  zero <- b == 0
  if (any(zero)) out[zero] <- stats::rgamma(sum(zero), shape = 0.5, rate = a[zero] / 2)
  if (any(!zero)) {
    # If X ~ GIG(1/2,a,b), then 1/X ~ IG(sqrt(a/b), shape=a).
    inv <- .rqr_rinvgauss(
      sum(!zero),
      mean = sqrt(a[!zero]) / sqrt(b[!zero]),
      shape = a[!zero]
    )
    out[!zero] <- 1 / inv
  }
  out
}

.sample_gig_devroye_required <- function(n_draws, p, a, b_vec, context = "RQR") {
  if (as.integer(n_draws)[1L] < 1L || abs(as.numeric(p)[1L] - 0.5) > 1e-12) {
    stop("The native RQR sampler currently implements only GIG p=1/2.", call. = FALSE)
  }
  ans <- replicate(as.integer(n_draws)[1L], rqr_sample_gig_half(b_vec, a))
  t(as.matrix(ans))
}

#' Construct a standalone beta-prior specification
#'
#' Ridge is native. RHS-family objects may be constructed by an installed
#' `exdqlm` reference package, but their MCMC state remains an explicit adapter
#' rather than part of the native DLM core.
#'
#' @param type Prior type.
#' @param ridge Ridge controls, including `tau2`.
#' @param rhs RHS controls forwarded to `exdqlm::beta_prior()`.
#' @return A prior specification.
#' @export
beta_prior <- function(type = c("ridge", "rhs", "rhs_ns"), ridge = list(), rhs = list()) {
  type <- match.arg(type)
  if (identical(type, "ridge")) {
    tau2 <- as.numeric(ridge$tau2 %||% 1e4)[1L]
    if (!is.finite(tau2) || tau2 <= 0) stop("ridge$tau2 must be positive.", call. = FALSE)
    return(list(type = "ridge", hypers = list(tau2 = tau2)))
  }
  if (!requireNamespace("exdqlm", quietly = TRUE)) {
    stop("RHS-family construction requires the pinned exdqlm reference package.", call. = FALSE)
  }
  getExportedValue("exdqlm", "beta_prior")(type = type, ridge = ridge, rhs = rhs)
}

.rqr_exdqlm_internal <- function(name) {
  if (!requireNamespace("exdqlm", quietly = TRUE)) {
    stop(sprintf("The '%s' compatibility adapter requires exdqlm.", name), call. = FALSE)
  }
  utils::getFromNamespace(name, "exdqlm")
}

.qdesn_assert_rhs_prior_obj_intercept_policy <- function(...) {
  .rqr_exdqlm_internal(".qdesn_assert_rhs_prior_obj_intercept_policy")(...)
}
.exal_mcmc_rhs_ns_prepare_state <- function(...) {
  .rqr_exdqlm_internal(".exal_mcmc_rhs_ns_prepare_state")(...)
}
.exal_mcmc_rhs_ns_precisions <- function(...) {
  .rqr_exdqlm_internal(".exal_mcmc_rhs_ns_precisions")(...)
}
.exal_mcmc_rhs_ns_gibbs_update <- function(...) {
  .rqr_exdqlm_internal(".exal_mcmc_rhs_ns_gibbs_update")(...)
}

qdesn_fit_vb <- function(...) {
  if (!requireNamespace("exdqlm", quietly = TRUE)) {
    stop("RQR-DESN design construction requires exdqlm.", call. = FALSE)
  }
  getExportedValue("exdqlm", "qdesn_fit_vb")(...)
}
