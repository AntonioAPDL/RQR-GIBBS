# Internal numerical utilities for the standalone package.

.rqr_symmetrize <- function(x) {
  x <- as.matrix(x)
  0.5 * (x + t(x))
}

.rqr_numerical_policy <- function(policy = c("fail", "record_repair")) {
  match.arg(as.character(policy)[1L], c("fail", "record_repair"))
}

.rqr_jitter_ladder <- function(policy, jitter_ladder) {
  policy <- .rqr_numerical_policy(policy)
  if (identical(policy, "fail")) return(0)
  ladder <- unique(as.numeric(jitter_ladder))
  if (!length(ladder) || any(!is.finite(ladder)) || any(ladder < 0)) {
    stop("jitter_ladder must contain finite nonnegative values.", call. = FALSE)
  }
  sort(unique(c(0, ladder)))
}

.rqr_empty_repair_records <- function() {
  data.frame(
    stage = character(0), time = integer(0), strategy = character(0),
    jitter = numeric(0), relative_jitter = numeric(0),
    min_eigenvalue = numeric(0), matrix_scale = numeric(0),
    jitter_scale = numeric(0), absolute_jitter_fallback = logical(0),
    clamped_eigenvalues = integer(0),
    stringsAsFactors = FALSE
  )
}

.rqr_add_repair_record <- function(records, stage, time, info) {
  clamped <- as.integer(info$clamped_eigenvalues %||% 0L)
  jitter <- as.numeric(info$jitter %||% 0)
  if (jitter <= 0 && clamped <= 0L) return(records)
  rbind(records, data.frame(
    stage = as.character(stage), time = as.integer(time),
    strategy = as.character(
      info$strategy %||% if (jitter > 0) "cholesky_jitter" else "eigen_clamp"
    ),
    jitter = jitter,
    relative_jitter = as.numeric(info$relative_jitter %||% NA_real_),
    min_eigenvalue = as.numeric(info$min_eigenvalue %||% NA_real_),
    matrix_scale = as.numeric(info$matrix_scale %||% NA_real_),
    jitter_scale = as.numeric(info$jitter_scale %||% info$matrix_scale %||% NA_real_),
    absolute_jitter_fallback = isTRUE(info$absolute_jitter_fallback %||% FALSE),
    clamped_eigenvalues = clamped,
    stringsAsFactors = FALSE
  ))
}

.rqr_validate_symmetric_matrix <- function(
    x, name = "matrix", tolerance = 100 * .Machine$double.eps) {
  x <- as.matrix(x)
  if (nrow(x) != ncol(x) || any(!is.finite(x))) {
    stop(sprintf("%s must be a finite square matrix.", name), call. = FALSE)
  }
  scale <- max(abs(x))
  asymmetry <- max(abs(x - t(x)))
  if (scale > 0 && asymmetry / scale > tolerance) {
    stop(sprintf("%s is not symmetric.", name), call. = FALSE)
  }
  if (scale == 0 && asymmetry > 0) {
    stop(sprintf("%s is not symmetric.", name), call. = FALSE)
  }
  .rqr_symmetrize(x)
}

.rqr_validate_covariance_cube <- function(x, name = "covariance", tolerance = 100 * .Machine$double.eps) {
  if (length(dim(x)) != 3L || dim(x)[1L] != dim(x)[2L] || any(!is.finite(x))) {
    stop(sprintf("%s must be a finite square covariance cube.", name), call. = FALSE)
  }
  for (tt in seq_len(dim(x)[3L])) {
    current <- .rqr_validate_symmetric_matrix(
      matrix(x[, , tt], nrow = dim(x)[1L], ncol = dim(x)[2L]),
      sprintf("%s slice %d", name, tt),
      tolerance
    )
    eigenvalues <- eigen(current, symmetric = TRUE, only.values = TRUE)$values
    eigen_scale <- max(abs(eigenvalues))
    minimum <- min(eigenvalues)
    if (eigen_scale > 0 && minimum / eigen_scale < -tolerance) {
      stop(sprintf("%s slice %d is materially indefinite.", name, tt), call. = FALSE)
    }
    x[, , tt] <- current
  }
  x
}

.rqr_chol_with_jitter <- function(x, jitter_ladder = c(0, 1e-12, 1e-10, 1e-8, 1e-6)) {
  x <- .rqr_symmetrize(x)
  if (nrow(x) != ncol(x) || any(!is.finite(x))) {
    stop("Matrix must be finite and square.", call. = FALSE)
  }
  matrix_scale <- max(abs(x))
  absolute_fallback <- matrix_scale == 0
  jitter_scale <- if (absolute_fallback) 1 else matrix_scale
  ladder <- unique(as.numeric(jitter_ladder))
  ladder <- ladder[is.finite(ladder) & ladder >= 0]
  min_eigenvalue <- NA_real_
  for (jj in ladder) {
    candidate <- if (jj == 0) x else x + diag(jj * jitter_scale, nrow(x))
    ans <- tryCatch(chol(candidate), error = function(e) NULL)
    if (!is.null(ans)) {
      if (jj > 0 && is.na(min_eigenvalue)) {
        min_eigenvalue <- min(eigen(x, symmetric = TRUE, only.values = TRUE)$values)
      }
      return(list(
        chol = ans, matrix = candidate, jitter = jj * jitter_scale,
        relative_jitter = if (absolute_fallback && jj > 0) NA_real_ else jj,
        min_eigenvalue = min_eigenvalue,
        matrix_scale = matrix_scale,
        jitter_scale = jitter_scale,
        absolute_jitter_fallback = absolute_fallback && jj > 0
      ))
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
    jitter = fac$jitter,
    relative_jitter = fac$relative_jitter,
    min_eigenvalue = fac$min_eigenvalue,
    clamped_eigenvalues = 0L,
    matrix_scale = fac$matrix_scale,
    jitter_scale = fac$jitter_scale,
    absolute_jitter_fallback = fac$absolute_jitter_fallback
  ))
}

.rqr_sample_mvnorm_covariance <- function(mean, covariance,
                                           jitter_ladder = c(0, 1e-12, 1e-10, 1e-8, 1e-6),
                                           numerical_policy = c("fail", "record_repair")) {
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
      info = list(
        strategy = "cholesky", jitter = 0, relative_jitter = 0,
        min_eigenvalue = NA_real_, clamped_eigenvalues = 0L,
        matrix_scale = max(abs(covariance)),
        jitter_scale = max(abs(covariance)),
        absolute_jitter_fallback = FALSE
      )
    ))
  }
  numerical_policy <- .rqr_numerical_policy(numerical_policy)
  ee <- eigen(covariance, symmetric = TRUE)
  scale <- max(abs(ee$values))
  near_psd <- scale == 0 || min(ee$values) / scale >= -1e-10
  if (near_psd) {
    if (identical(numerical_policy, "fail") && any(ee$values < 0)) {
      stop(
        paste(
          "Gaussian covariance has a negative eigenvalue, and projection is",
          "disabled under numerical_policy='fail'."
        ),
        call. = FALSE
      )
    }
    values <- pmax(ee$values, 0)
    return(list(
      draw = as.numeric(mean + ee$vectors %*% (sqrt(values) * stats::rnorm(length(mean)))),
      info = list(
        strategy = "psd_eigen",
        jitter = 0, relative_jitter = 0,
        min_eigenvalue = min(ee$values),
        clamped_eigenvalues = sum(ee$values < 0),
        matrix_scale = max(abs(covariance)),
        jitter_scale = max(abs(covariance)),
        absolute_jitter_fallback = FALSE
      )
    ))
  }
  if (identical(numerical_policy, "fail")) {
    stop(
      paste(
        "Gaussian covariance is indefinite, and repair is disabled under",
        "numerical_policy='fail'."
      ),
      call. = FALSE
    )
  }
  fac <- .rqr_chol_with_jitter(covariance, jitter_ladder)
  list(
    draw = as.numeric(mean + t(fac$chol) %*% stats::rnorm(length(mean))),
    info = list(
      strategy = "cholesky_jitter", jitter = fac$jitter,
      relative_jitter = fac$relative_jitter,
      min_eigenvalue = fac$min_eigenvalue,
      clamped_eigenvalues = 0L,
      matrix_scale = fac$matrix_scale,
      jitter_scale = fac$jitter_scale,
      absolute_jitter_fallback = fac$absolute_jitter_fallback
    )
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

.rqr_rinvgauss_log <- function(n, log_mean, log_shape) {
  n <- as.integer(n)[1L]
  log_mean <- rep_len(as.numeric(log_mean), n)
  log_shape <- rep_len(as.numeric(log_shape), n)
  if (n < 1L || any(!is.finite(log_mean)) || any(!is.finite(log_shape))) {
    stop("Log inverse-Gaussian mean and shape must be finite.", call. = FALSE)
  }
  z2 <- stats::rnorm(n)^2
  log_w <- log_mean + log(z2) - log_shape
  log_denominator <- numeric(n)
  moderate <- is.finite(log_w) & log_w <= 350
  if (any(moderate)) {
    w <- exp(log_w[moderate])
    log_denominator[moderate] <- log1p(
      0.5 * w + 0.5 * sqrt(w) * sqrt(w + 4)
    )
  }
  large <- is.finite(log_w) & log_w > 350
  if (any(large)) {
    # d = 1 + w/2 + sqrt(w^2 + 4w)/2 = w{1/w + 1/2 + sqrt(1+4/w)/2}.
    inv_w <- exp(-log_w[large])
    log_denominator[large] <- log_w[large] + log(
      inv_w + 0.5 + 0.5 * sqrt(1 + 4 * inv_w)
    )
  }
  log_denominator[is.infinite(log_w) & log_w < 0] <- 0
  if (any(!is.finite(log_denominator))) {
    stop("Inverse-Gaussian transform produced a nonfinite log denominator.", call. = FALSE)
  }
  log_x <- log_mean - log_denominator
  # P(X=x)=mu/(mu+x)=1/{1+exp(-log(d))}; this form cannot overflow.
  choose_x <- stats::runif(n) <= stats::plogis(log_denominator)
  ifelse(choose_x, log_x, log_mean + log_denominator)
}

.rqr_rinvgauss <- function(n, mean, shape) {
  mean <- as.numeric(mean)
  shape <- as.numeric(shape)
  if (any(!is.finite(mean)) || any(mean <= 0) ||
      any(!is.finite(shape)) || any(shape <= 0)) {
    stop("Inverse-Gaussian mean and shape must be finite and positive.", call. = FALSE)
  }
  log_draw <- .rqr_rinvgauss_log(n, log(mean), log(shape))
  draw <- exp(log_draw)
  if (any(!is.finite(draw)) || any(draw <= 0)) {
    stop("Inverse-Gaussian draw is outside the finite positive floating-point range.", call. = FALSE)
  }
  draw
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
    log_inv <- .rqr_rinvgauss_log(
      sum(!zero),
      log_mean = 0.5 * (log(a[!zero]) - log(b[!zero])),
      log_shape = log(a[!zero])
    )
    out[!zero] <- exp(-log_inv)
    if (any(!is.finite(out[!zero])) || any(out[!zero] <= 0)) {
      stop("GIG draw is outside the finite positive floating-point range.", call. = FALSE)
    }
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
  .rqr_installed_namespace("exdqlm", "RHS-family construction")
  getExportedValue("exdqlm", "beta_prior")(type = type, ridge = ridge, rhs = rhs)
}

.rqr_installed_namespace <- function(package, context) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop(
      sprintf("%s requires the isolated %s runtime.", context, package),
      call. = FALSE
    )
  }
  namespace <- asNamespace(package)
  runtime_path <- normalizePath(
    getNamespaceInfo(namespace, "path"), winslash = "/", mustWork = TRUE
  )
  if (file.exists(file.path(runtime_path, ".git"))) {
    stop(
      sprintf(
        paste0(
          "%s refuses a namespace loaded from the %s source checkout. ",
          "Use the isolated archive-attested runtime."
        ),
        context, package
      ),
      call. = FALSE
    )
  }
  namespace
}

.rqr_exdqlm_internal <- function(name) {
  .rqr_installed_namespace(
    "exdqlm", sprintf("The '%s' compatibility adapter", name)
  )
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
  .rqr_installed_namespace("exdqlm", "RQR-DESN design construction")
  getExportedValue("exdqlm", "qdesn_fit_vb")(...)
}
