.rqr_prepare_evolution <- function(evolution, p, n_time) {
  if (!is.list(evolution) || is.null(evolution$mode)) {
    stop("evolution must be an RQR evolution specification.", call. = FALSE)
  }
  mode <- match.arg(
    as.character(evolution$mode)[1L],
    c("fixed_W", "discount_template", "component_scale", "adaptive_discount")
  )
  if (mode %in% c("fixed_W", "discount_template", "component_scale")) {
    if (is.null(evolution$W)) stop("fixed evolution requires W.", call. = FALSE)
    W <- .rqr_expand_cube(evolution$W, n_time, p, "evolution$W")
    W <- .rqr_validate_covariance_cube(W, "evolution$W")
    return(list(mode = mode, mode_code = 0L, W = W, D = matrix(0, p, p)))
  }
  D <- as.matrix(evolution$D)
  if (!all(dim(D) == c(p, p)) || any(!is.finite(D))) stop("adaptive evolution requires finite p x p D.", call. = FALSE)
  D <- .rqr_validate_symmetric_matrix(D, "evolution$D")
  dev <- eigen(D, symmetric = TRUE, only.values = TRUE)$values
  dev_scale <- max(abs(dev))
  if (dev_scale > 0 && min(dev) / dev_scale < -100 * .Machine$double.eps) {
    stop("D must be positive semidefinite.", call. = FALSE)
  }
  list(mode = mode, mode_code = 1L, W = array(0, c(p, p, n_time)), D = D)
}

.rqr_resolve_ffbs_backend <- function(backend = c("cpp", "R", "auto")) {
  backend <- match.arg(backend)
  cpp_available <- exists("rqr_ffbs_cpp", mode = "function")
  if (identical(backend, "cpp") && !cpp_available) {
    stop("Compiled FFBS backend is unavailable.", call. = FALSE)
  }
  if (identical(backend, "auto")) {
    if (cpp_available) "cpp" else "R"
  } else {
    backend
  }
}

.rqr_resolve_filter_backend <- function(backend = c("cpp", "R", "auto")) {
  backend <- match.arg(backend)
  cpp_available <- exists("rqr_filter_log_marginal_cpp", mode = "function")
  if (identical(backend, "cpp") && !cpp_available) {
    stop("Compiled filter-log-marginal backend is unavailable.", call. = FALSE)
  }
  if (identical(backend, "auto")) {
    if (cpp_available) "cpp" else "R"
  } else {
    backend
  }
}

.rqr_validate_filter_covariance <- function(
    x, name, tolerance = 100 * .Machine$double.eps) {
  x <- .rqr_validate_symmetric_matrix(x, name, tolerance)
  eigenvalues <- tryCatch(
    eigen(x, symmetric = TRUE, only.values = TRUE)$values,
    error = function(e) {
      stop(
        sprintf("Symmetric eigendecomposition failed for %s.", name),
        call. = FALSE
      )
    }
  )
  eigen_scale <- max(abs(eigenvalues))
  if (eigen_scale > 0 &&
      min(eigenvalues) / eigen_scale < -tolerance) {
    stop(sprintf("%s is materially indefinite.", name), call. = FALSE)
  }
  x
}

.rqr_ffbs_validate_covariance <- function(
    x, name, numerical_policy, jitter_ladder,
    reference_scale = NULL,
    tolerance = 100 * .Machine$double.eps) {
  x <- .rqr_validate_symmetric_matrix(x, name, tolerance)
  eig <- tryCatch(
    eigen(x, symmetric = TRUE),
    error = function(e) {
      stop(
        sprintf("Symmetric eigendecomposition failed for %s.", name),
        call. = FALSE
      )
    }
  )
  values <- eig$values
  eigen_scale <- max(abs(values))
  if (is.null(reference_scale)) {
    reference_scale <- eigen_scale
  } else {
    reference_scale <- as.numeric(reference_scale)
    if (length(reference_scale) != 1L || !is.finite(reference_scale) ||
        reference_scale < 0) {
      stop("Covariance reference_scale must be finite and nonnegative.",
           call. = FALSE)
    }
    reference_scale <- max(reference_scale, eigen_scale)
  }
  minimum <- min(values)
  base_info <- list(
    strategy = "validated_psd",
    jitter = 0,
    relative_jitter = 0,
    min_eigenvalue = minimum,
    clamped_eigenvalues = 0L,
    roundoff_clamped_eigenvalues = 0L,
    matrix_scale = max(abs(x)),
    jitter_scale = max(abs(x)),
    absolute_jitter_fallback = FALSE
  )
  if (minimum >= 0) {
    return(list(matrix = x, info = base_info))
  }
  near_psd <- reference_scale == 0 ||
    minimum / reference_scale >= -tolerance
  if (near_psd) {
    clamped <- pmax(values, 0)
    normalized <- .rqr_symmetrize(
      eig$vectors %*% (clamped * t(eig$vectors))
    )
    base_info$strategy <- "psd_roundoff"
    base_info$roundoff_clamped_eigenvalues <- sum(values < 0)
    return(list(matrix = normalized, info = base_info))
  }
  if (identical(numerical_policy, "fail")) {
    stop(
      sprintf(
        paste0(
          "%s has a negative eigenvalue, and covariance repair is ",
          "disabled under numerical_policy='fail'."
        ),
        name
      ),
      call. = FALSE
    )
  }
  factor <- .rqr_chol_with_jitter(x, jitter_ladder)
  list(
    matrix = factor$matrix,
    info = list(
      strategy = "cholesky_jitter",
      jitter = factor$jitter,
      relative_jitter = factor$relative_jitter,
      min_eigenvalue = factor$min_eigenvalue,
      clamped_eigenvalues = 0L,
      roundoff_clamped_eigenvalues = 0L,
      matrix_scale = factor$matrix_scale,
      jitter_scale = factor$jitter_scale,
      absolute_jitter_fallback = factor$absolute_jitter_fallback
    )
  )
}

.rqr_prepare_filter_log_marginal <- function(
    z, H, V, GG, m0, C0, evolution) {
  z <- as.numeric(z)
  if (!length(z) || any(is.nan(z)) || any(is.infinite(z))) {
    stop(
      "z must be nonempty and may contain finite values or NA only.",
      call. = FALSE
    )
  }
  m0 <- as.numeric(m0)
  if (!length(m0) || any(!is.finite(m0))) {
    stop("m0 must be a nonempty finite vector.", call. = FALSE)
  }
  p <- length(m0)
  n_time <- length(z)
  H <- .rqr_expand_columns(H, n_time, "H")
  if (!identical(dim(H), c(p, n_time)) || any(!is.finite(H))) {
    stop(
      "H must be a finite p x length(z) matrix after expansion.",
      call. = FALSE
    )
  }
  V <- as.numeric(V)
  if (length(V) != n_time || any(!is.finite(V)) || any(V <= 0)) {
    stop("V must be finite, positive, and length(z).", call. = FALSE)
  }
  GG <- .rqr_expand_cube(GG, n_time, p, "GG")
  if (any(!is.finite(GG))) {
    stop("GG must contain only finite values.", call. = FALSE)
  }
  C0 <- as.matrix(C0)
  if (!identical(dim(C0), c(p, p))) {
    stop("C0 dimensions must match length(m0).", call. = FALSE)
  }
  C0 <- .rqr_validate_filter_covariance(C0, "C0")
  evo <- .rqr_prepare_evolution(evolution, p, n_time)
  if (!identical(evo$mode_code, 0L)) {
    stop(
      "The filter log marginal requires a fixed covariance cube.",
      call. = FALSE
    )
  }
  list(
    z = z,
    H = H,
    V = V,
    GG = GG,
    m0 = m0,
    C0 = C0,
    W = evo$W
  )
}

.rqr_filter_log_marginal_r_prepared <- function(prepared) {
  z <- prepared$z
  H <- prepared$H
  V <- prepared$V
  GG <- prepared$GG
  m0 <- prepared$m0
  C0 <- prepared$C0
  W <- prepared$W
  p <- length(m0)
  n_time <- length(z)
  identity <- diag(1, p)
  m_previous <- m0
  C_previous <- C0
  log_marginal <- 0
  for (tt in seq_len(n_time)) {
    a <- drop(GG[, , tt] %*% m_previous)
    R <- .rqr_validate_filter_covariance(
      .rqr_symmetrize(
        GG[, , tt] %*% C_previous %*% t(GG[, , tt]) +
          W[, , tt]
      ),
      sprintf("forecast covariance at time %d", tt)
    )
    m <- a
    C <- R
    if (!is.na(z[[tt]])) {
      h <- H[, tt]
      rh <- drop(R %*% h)
      q <- drop(crossprod(h, rh)) + V[[tt]]
      if (!is.finite(q) || q <= 0) {
        stop(
          "Nonpositive forecast variance in filter log marginal.",
          call. = FALSE
        )
      }
      residual <- z[[tt]] - drop(crossprod(h, a))
      log_marginal <- log_marginal - 0.5 * (
        log(2 * pi) + log(q) + residual^2 / q
      )
      m <- a + rh * residual / q
      gain <- rh / q
      update <- identity - tcrossprod(gain, h)
      C <- .rqr_validate_filter_covariance(
        .rqr_symmetrize(
          update %*% R %*% t(update) +
            V[[tt]] * tcrossprod(gain)
        ),
        sprintf("filtered covariance at time %d", tt)
      )
    }
    if (any(!is.finite(c(m, C)))) {
      stop("Nonfinite filter-log-marginal recursion.", call. = FALSE)
    }
    m_previous <- m
    C_previous <- C
  }
  if (!is.finite(log_marginal)) {
    stop("The Gaussian filter log marginal is nonfinite.", call. = FALSE)
  }
  as.numeric(log_marginal)
}

.rqr_filter_log_marginal_r <- function(
    z, H, V, GG, m0, C0, evolution) {
  prepared <- .rqr_prepare_filter_log_marginal(
    z, H, V, GG, m0, C0, evolution
  )
  .rqr_filter_log_marginal_r_prepared(prepared)
}

.rqr_filter_log_marginal <- function(
    z, H, V, GG, m0, C0, evolution,
    backend = c("cpp", "R", "auto")) {
  backend <- .rqr_resolve_filter_backend(match.arg(backend))
  prepared <- .rqr_prepare_filter_log_marginal(
    z, H, V, GG, m0, C0, evolution
  )
  if (identical(backend, "cpp")) {
    return(as.numeric(rqr_filter_log_marginal_cpp(
      prepared$z, prepared$H, prepared$V, prepared$GG,
      prepared$m0, prepared$C0, prepared$W
    )))
  }
  .rqr_filter_log_marginal_r_prepared(prepared)
}

.rqr_ffbs_r <- function(z, H, V, GG, m0, C0, evolution, sample = FALSE,
                        jitter_ladder = c(0, 1e-12, 1e-10, 1e-8, 1e-6),
                        numerical_policy = c("fail", "record_repair")) {
  z <- as.numeric(z)
  if (any(is.nan(z)) || any(is.infinite(z))) {
    stop("z may contain finite values or NA only; NaN and Inf are invalid.", call. = FALSE)
  }
  H <- as.matrix(H)
  V <- as.numeric(V)
  p <- length(m0)
  n_time <- length(z)
  if (!all(dim(H) == c(p, n_time)) || length(V) != n_time ||
      any(!is.finite(H)) || any(!is.finite(V)) || any(V <= 0)) {
    stop("H must be p x T and V must be finite, positive, and length T.", call. = FALSE)
  }
  GG <- .rqr_expand_cube(GG, n_time, p, "GG")
  evo <- .rqr_prepare_evolution(evolution, p, n_time)
  a <- m <- matrix(NA_real_, p, n_time)
  R <- C <- array(NA_real_, c(p, p, n_time))
  q <- residual <- rep(NA_real_, n_time)
  numerical_policy <- .rqr_numerical_policy(numerical_policy)
  jitter_ladder <- .rqr_jitter_ladder(numerical_policy, jitter_ladder)
  repair_records <- .rqr_empty_repair_records()
  jitter_used <- numeric(0)
  roundoff_psd_count <- 0L
  identity <- diag(1, p)
  m_prev <- as.numeric(m0)
  C_prev <- .rqr_symmetrize(C0)
  for (tt in seq_len(n_time)) {
    gt <- GG[, , tt]
    a[, tt] <- drop(gt %*% m_prev)
    P <- .rqr_symmetrize(gt %*% C_prev %*% t(gt))
    Wt <- if (evo$mode_code == 0L) evo$W[, , tt] else .rqr_symmetrize(evo$D * P)
    forecast <- .rqr_ffbs_validate_covariance(
      .rqr_symmetrize(P + Wt),
      sprintf("forecast covariance at time %d", tt),
      numerical_policy, jitter_ladder
    )
    R[, , tt] <- forecast$matrix
    jitter_used <- c(jitter_used, forecast$info$jitter)
    roundoff_psd_count <- roundoff_psd_count +
      forecast$info$roundoff_clamped_eigenvalues
    repair_records <- .rqr_add_repair_record(
      repair_records, "forecast_covariance", tt, forecast$info
    )
    if (!is.na(z[tt])) {
      h <- H[, tt]
      rh <- drop(R[, , tt] %*% h)
      q[tt] <- drop(crossprod(h, rh)) + V[tt]
      if (!is.finite(q[tt]) || q[tt] <= 0) stop(sprintf("Nonpositive forecast variance at time %d.", tt), call. = FALSE)
      residual[tt] <- z[tt] - drop(crossprod(h, a[, tt]))
      gain <- rh / q[tt]
      m[, tt] <- a[, tt] + gain * residual[tt]
      update <- identity - tcrossprod(gain, h)
      filtered <- .rqr_ffbs_validate_covariance(
        .rqr_symmetrize(
          update %*% R[, , tt] %*% t(update) +
            V[tt] * tcrossprod(gain)
        ),
        sprintf("filtered covariance at time %d", tt),
        numerical_policy, jitter_ladder
      )
      C[, , tt] <- filtered$matrix
      jitter_used <- c(jitter_used, filtered$info$jitter)
      roundoff_psd_count <- roundoff_psd_count +
        filtered$info$roundoff_clamped_eigenvalues
      repair_records <- .rqr_add_repair_record(
        repair_records, "filter_covariance", tt, filtered$info
      )
    } else {
      m[, tt] <- a[, tt]
      C[, , tt] <- R[, , tt]
    }
    m_prev <- m[, tt]
    C_prev <- C[, , tt]
  }
  sm <- m
  sC <- C
  if (n_time > 1L) {
    for (tt in (n_time - 1L):1L) {
      facR <- .rqr_chol_with_jitter(R[, , tt + 1L], jitter_ladder)
      invR <- chol2inv(facR$chol)
      Rstar <- facR$matrix
      jitter_used <- c(jitter_used, facR$jitter)
      repair_records <- .rqr_add_repair_record(
        repair_records, "backward_smoothing_prior_covariance", tt + 1L,
        list(
          strategy = "cholesky_jitter", jitter = facR$jitter,
          relative_jitter = facR$relative_jitter,
          min_eigenvalue = facR$min_eigenvalue, matrix_scale = facR$matrix_scale,
          jitter_scale = facR$jitter_scale,
          absolute_jitter_fallback = facR$absolute_jitter_fallback,
          clamped_eigenvalues = 0L
        )
      )
      B <- C[, , tt] %*% t(GG[, , tt + 1L]) %*% invR
      sm[, tt] <- m[, tt] + B %*% (sm[, tt + 1L] - a[, tt + 1L])
      smoother_increment <-
        B %*% (sC[, , tt + 1L] - Rstar) %*% t(B)
      smoother <- .rqr_ffbs_validate_covariance(
        .rqr_symmetrize(C[, , tt] + smoother_increment),
        sprintf("smoother covariance at time %d", tt),
        numerical_policy, jitter_ladder,
        reference_scale = max(
          abs(C[, , tt]), abs(smoother_increment)
        )
      )
      sC[, , tt] <- smoother$matrix
      jitter_used <- c(jitter_used, smoother$info$jitter)
      roundoff_psd_count <- roundoff_psd_count +
        smoother$info$roundoff_clamped_eigenvalues
      repair_records <- .rqr_add_repair_record(
        repair_records, "smoother_covariance", tt, smoother$info
      )
    }
  }
  path <- NULL
  psd_draw_count <- 0L
  if (isTRUE(sample)) {
    path <- matrix(NA_real_, p, n_time)
    last_draw <- .rqr_sample_mvnorm_covariance(
      m[, n_time], C[, , n_time], jitter_ladder, numerical_policy
    )
    path[, n_time] <- last_draw$draw
    jitter_used <- c(jitter_used, last_draw$info$jitter)
    roundoff_psd_count <- roundoff_psd_count +
      last_draw$info$roundoff_clamped_eigenvalues
    repair_records <- .rqr_add_repair_record(
      repair_records, "terminal_draw_covariance", n_time, last_draw$info
    )
    psd_draw_count <- psd_draw_count + as.integer(last_draw$info$strategy == "psd_eigen")
    if (n_time > 1L) {
      for (tt in (n_time - 1L):1L) {
        facR <- .rqr_chol_with_jitter(R[, , tt + 1L], jitter_ladder)
        Rstar <- facR$matrix
        jitter_used <- c(jitter_used, facR$jitter)
        repair_records <- .rqr_add_repair_record(
          repair_records, "backward_sampling_prior_covariance", tt + 1L,
          list(
            strategy = "cholesky_jitter", jitter = facR$jitter,
            relative_jitter = facR$relative_jitter,
            min_eigenvalue = facR$min_eigenvalue, matrix_scale = facR$matrix_scale,
            jitter_scale = facR$jitter_scale,
            absolute_jitter_fallback = facR$absolute_jitter_fallback,
            clamped_eigenvalues = 0L
          )
        )
        B <- C[, , tt] %*% t(GG[, , tt + 1L]) %*% chol2inv(facR$chol)
        h <- m[, tt] + B %*% (path[, tt + 1L] - a[, tt + 1L])
        backward_reduction <- B %*% Rstar %*% t(B)
        backward <- .rqr_ffbs_validate_covariance(
          .rqr_symmetrize(C[, , tt] - backward_reduction),
          sprintf("backward conditional covariance at time %d", tt),
          numerical_policy, jitter_ladder,
          reference_scale = max(
            abs(C[, , tt]), abs(backward_reduction)
          )
        )
        HC <- backward$matrix
        jitter_used <- c(jitter_used, backward$info$jitter)
        roundoff_psd_count <- roundoff_psd_count +
          backward$info$roundoff_clamped_eigenvalues
        repair_records <- .rqr_add_repair_record(
          repair_records, "backward_draw_covariance", tt, backward$info
        )
        state_draw <- .rqr_sample_mvnorm_covariance(
          h, HC, jitter_ladder, numerical_policy
        )
        path[, tt] <- state_draw$draw
        jitter_used <- c(jitter_used, state_draw$info$jitter)
        roundoff_psd_count <- roundoff_psd_count +
          state_draw$info$roundoff_clamped_eigenvalues
        repair_records <- .rqr_add_repair_record(
          repair_records, "backward_draw_covariance", tt, state_draw$info
        )
        psd_draw_count <- psd_draw_count + as.integer(state_draw$info$strategy == "psd_eigen")
      }
    }
  }
  list(
    filter_mean = m, filter_cov = C,
    prior_mean = a, prior_cov = R,
    smooth_mean = sm, smooth_cov = sC,
    path = path, forecast_variance = q, residual = residual,
    diagnostics = list(
      backend = "R", evolution_mode = evo$mode,
      max_jitter = max(jitter_used, 0), jitter_count = sum(jitter_used > 0),
      psd_draw_count = psd_draw_count,
      roundoff_psd_count = roundoff_psd_count,
      numerical_policy = numerical_policy,
      repair_count = nrow(repair_records),
      repair_records = repair_records,
      min_forecast_variance = if (all(is.na(q))) NA_real_ else min(q, na.rm = TRUE)
    )
  )
}

.rqr_ffbs_dispatch <- function(z, H, V, GG, m0, C0, evolution,
                               sample, backend, jitter_ladder,
                               numerical_policy = c("fail", "record_repair")) {
  backend <- match.arg(backend, c("cpp", "R", "auto"))
  resolved_backend <- .rqr_resolve_ffbs_backend(backend)
  z <- as.numeric(z)
  if (!length(z)) {
    stop("z must be nonempty.", call. = FALSE)
  }
  if (any(is.nan(z))) {
    stop("z may contain finite values or NA only; NaN is invalid.", call. = FALSE)
  }
  if (any(is.infinite(z))) {
    stop("z may contain finite values or NA only; Inf is invalid.", call. = FALSE)
  }
  m0 <- as.numeric(m0)
  if (!length(m0) || any(!is.finite(m0))) {
    stop("m0 must be a nonempty finite state vector.", call. = FALSE)
  }
  p <- length(m0)
  n_time <- length(z)
  H <- .rqr_expand_columns(H, n_time, "H")
  V <- as.numeric(V)
  if (length(V) != n_time || any(!is.finite(V)) || any(V <= 0)) {
    stop("V must be finite, positive, and length(z).", call. = FALSE)
  }
  GG <- .rqr_expand_cube(GG, n_time, p, "GG")
  if (any(!is.finite(GG))) {
    stop("GG must contain only finite values.", call. = FALSE)
  }
  C0 <- .rqr_validate_symmetric_matrix(C0, "C0")
  if (!all(dim(C0) == c(p, p))) {
    stop("C0 dimensions must match length(m0).", call. = FALSE)
  }
  # C0 is a prior covariance, not a repairable working matrix. Requiring a
  # direct factorization at the exported boundary prevents silent target
  # changes in either backend.
  .rqr_chol_with_jitter(C0, jitter_ladder = 0)
  evo <- .rqr_prepare_evolution(evolution, p, n_time)
  numerical_policy <- .rqr_numerical_policy(numerical_policy)
  jitter_ladder <- .rqr_jitter_ladder(numerical_policy, jitter_ladder)
  use_cpp <- identical(resolved_backend, "cpp")
  if (use_cpp) {
    out <- rqr_ffbs_cpp(
      z = z, H = H, V = V,
      GG = GG,
      m0 = m0, C0 = C0,
      evolution_mode = evo$mode_code, W = evo$W, D = evo$D,
      sample_path = isTRUE(sample), jitter_ladder = as.numeric(jitter_ladder),
      evolution_label = evo$mode,
      allow_covariance_repair = identical(numerical_policy, "record_repair")
    )
    out$forecast_variance <- as.numeric(out$forecast_variance)
    out$residual <- as.numeric(out$residual)
    out$diagnostics$numerical_policy <- numerical_policy
    return(out)
  }
  .rqr_ffbs_r(
    z, H, V, GG, m0, C0, evolution, sample, jitter_ladder,
    numerical_policy
  )
}

#' Filter and smooth a scalar-observation Gaussian state-space model
#'
#' For a length-`T` pseudo-observation vector and state dimension `p`, `H` must
#' be finite `p x 1` or `p x T`; `V` must contain `T` finite positive values;
#' `GG` must be finite `p x p` or `p x p x T`; `m0` must be a finite
#' length-`p` vector; and `C0` must be a symmetric positive-definite `p x p`
#' covariance. Fixed evolution covariance slices must be symmetric
#' positive-semidefinite. Only `NA` in `z` denotes a missing measurement;
#' `NaN` and infinite values are rejected.
#'
#' The public boundary never repairs `C0` or a declared evolution covariance.
#' With `numerical_policy = "fail"`, any later covariance factorization failure
#' stops. `"record_repair"` permits only the declared matrix-relative jitter
#' ladder for eligible working or sampled covariances and returns every repair
#' in `diagnostics$repair_records`.
#'
#' @param z Length-`T` pseudo-observation vector; `NA` alone denotes a missing
#'   observation.
#' @param H Finite `p x 1` or `p x T` state-by-time observation design.
#' @param V Length-`T` finite positive observation variances.
#' @param GG Finite `p x p` evolution matrix or `p x p x T` cube.
#' @param m0 Finite length-`p` initial state mean.
#' @param C0 Symmetric positive-definite `p x p` initial state covariance.
#' @param evolution Valid RQR evolution specification with dimensions
#'   compatible with `p` and `T`.
#' @param backend One of `"cpp"`, `"R"`, or `"auto"`; `"auto"` uses C++ when
#'   the registered native kernel is available.
#' @param jitter_ladder Declared matrix-relative Cholesky jitter ladder. An
#'   exactly zero matrix uses a separately recorded absolute fallback.
#' @param numerical_policy Either `"fail"` or `"record_repair"`.
#' @return Filtering, prior, and smoothing means and covariance arrays,
#'   forecast variances and residuals, a `NULL` `path`, and numerical
#'   diagnostics including backend, evolution mode, repair count, and repair
#'   records.
#' @examples
#' moments <- rqr_ffbs_smooth(
#'   z = c(0, NA, 0.5),
#'   H = matrix(1, 1, 1),
#'   V = rep(1, 3),
#'   GG = matrix(1, 1, 1),
#'   m0 = 0,
#'   C0 = matrix(2, 1, 1),
#'   evolution = rqr_evolution_fixed(matrix(0.1, 1, 1)),
#'   backend = "R",
#'   numerical_policy = "fail"
#' )
#' stopifnot(is.na(moments$forecast_variance[2]))
#' @family RQR-DLM
#' @export
rqr_ffbs_smooth <- function(z, H, V, GG, m0, C0, evolution,
                            backend = c("cpp", "R", "auto"),
                            jitter_ladder = c(0, 1e-12, 1e-10, 1e-8, 1e-6),
                            numerical_policy = c("fail", "record_repair")) {
  .rqr_ffbs_dispatch(
    z, H, V, GG, m0, C0, evolution, FALSE, backend, jitter_ladder,
    numerical_policy
  )
}

#' Draw a Gaussian state path by FFBS
#'
#' @inheritParams rqr_ffbs_smooth
#' @return The filtering and smoothing output from [rqr_ffbs_smooth()] plus one
#'   sampled `p x T` state path. Numerical repairs, if permitted, are recorded
#'   in `diagnostics$repair_records`.
#' @examples
#' set.seed(1)
#' draw <- rqr_ffbs_sample(
#'   z = c(0, NA, 0.5),
#'   H = matrix(1, 1, 1),
#'   V = rep(1, 3),
#'   GG = matrix(1, 1, 1),
#'   m0 = 0,
#'   C0 = matrix(2, 1, 1),
#'   evolution = rqr_evolution_fixed(matrix(0.1, 1, 1)),
#'   backend = "R",
#'   numerical_policy = "fail"
#' )
#' stopifnot(identical(dim(draw$path), c(1L, 3L)))
#' @family RQR-DLM
#' @export
rqr_ffbs_sample <- function(z, H, V, GG, m0, C0, evolution,
                            backend = c("cpp", "R", "auto"),
                            jitter_ladder = c(0, 1e-12, 1e-10, 1e-8, 1e-6),
                            numerical_policy = c("fail", "record_repair")) {
  .rqr_ffbs_dispatch(
    z, H, V, GG, m0, C0, evolution, TRUE, backend, jitter_ladder,
    numerical_policy
  )
}
