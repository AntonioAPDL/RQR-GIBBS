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
    for (tt in seq_len(n_time)) {
      W[, , tt] <- .rqr_symmetrize(W[, , tt])
      ev <- eigen(W[, , tt], symmetric = TRUE, only.values = TRUE)$values
      if (min(ev) < -1e-10 * max(1, max(abs(ev)))) {
        stop(sprintf("W is materially indefinite at time %d.", tt), call. = FALSE)
      }
    }
    return(list(mode = mode, mode_code = 0L, W = W, D = matrix(0, p, p)))
  }
  D <- as.matrix(evolution$D)
  if (!all(dim(D) == c(p, p)) || any(!is.finite(D))) stop("adaptive evolution requires finite p x p D.", call. = FALSE)
  D <- .rqr_symmetrize(D)
  dev <- eigen(D, symmetric = TRUE, only.values = TRUE)$values
  if (min(dev) < -1e-10 * max(1, max(abs(dev)))) stop("D must be positive semidefinite.", call. = FALSE)
  list(mode = mode, mode_code = 1L, W = array(0, c(p, p, n_time)), D = D)
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
  m_prev <- as.numeric(m0)
  C_prev <- .rqr_symmetrize(C0)
  for (tt in seq_len(n_time)) {
    gt <- GG[, , tt]
    a[, tt] <- drop(gt %*% m_prev)
    P <- .rqr_symmetrize(gt %*% C_prev %*% t(gt))
    Wt <- if (evo$mode_code == 0L) evo$W[, , tt] else .rqr_symmetrize(evo$D * P)
    R[, , tt] <- .rqr_symmetrize(P + Wt)
    if (!is.na(z[tt])) {
      h <- H[, tt]
      rh <- drop(R[, , tt] %*% h)
      q[tt] <- drop(crossprod(h, rh)) + V[tt]
      if (!is.finite(q[tt]) || q[tt] <= 0) stop(sprintf("Nonpositive forecast variance at time %d.", tt), call. = FALSE)
      residual[tt] <- z[tt] - drop(crossprod(h, a[, tt]))
      m[, tt] <- a[, tt] + rh * residual[tt] / q[tt]
      C[, , tt] <- .rqr_symmetrize(R[, , tt] - tcrossprod(rh) / q[tt])
    } else {
      m[, tt] <- a[, tt]
      C[, , tt] <- R[, , tt]
    }
    # Fail early on a materially indefinite covariance.
    fac <- .rqr_chol_with_jitter(C[, , tt], jitter_ladder)
    jitter_used <- c(jitter_used, fac$jitter)
    repair_records <- .rqr_add_repair_record(
      repair_records, "filter_covariance", tt,
      list(
        strategy = "cholesky_jitter", jitter = fac$jitter,
        relative_jitter = fac$relative_jitter,
        min_eigenvalue = fac$min_eigenvalue, matrix_scale = fac$matrix_scale,
        clamped_eigenvalues = 0L
      )
    )
    C[, , tt] <- fac$matrix
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
          clamped_eigenvalues = 0L
        )
      )
      B <- C[, , tt] %*% t(GG[, , tt + 1L]) %*% invR
      sm[, tt] <- m[, tt] + B %*% (sm[, tt + 1L] - a[, tt + 1L])
      sC[, , tt] <- .rqr_symmetrize(C[, , tt] + B %*% (sC[, , tt + 1L] - Rstar) %*% t(B))
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
            clamped_eigenvalues = 0L
          )
        )
        B <- C[, , tt] %*% t(GG[, , tt + 1L]) %*% chol2inv(facR$chol)
        h <- m[, tt] + B %*% (path[, tt + 1L] - a[, tt + 1L])
        HC <- .rqr_symmetrize(C[, , tt] - B %*% Rstar %*% t(B))
        state_draw <- .rqr_sample_mvnorm_covariance(
          h, HC, jitter_ladder, numerical_policy
        )
        path[, tt] <- state_draw$draw
        jitter_used <- c(jitter_used, state_draw$info$jitter)
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
  p <- length(m0)
  n_time <- length(z)
  H <- .rqr_expand_columns(H, n_time, "H")
  evo <- .rqr_prepare_evolution(evolution, p, n_time)
  numerical_policy <- .rqr_numerical_policy(numerical_policy)
  jitter_ladder <- .rqr_jitter_ladder(numerical_policy, jitter_ladder)
  use_cpp <- backend != "R" && exists("rqr_ffbs_cpp", mode = "function")
  if (backend == "cpp" && !use_cpp) stop("Compiled FFBS backend is unavailable.", call. = FALSE)
  if (use_cpp) {
    out <- rqr_ffbs_cpp(
      z = as.numeric(z), H = H, V = as.numeric(V),
      GG = .rqr_expand_cube(GG, n_time, p, "GG"),
      m0 = as.numeric(m0), C0 = as.matrix(C0),
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
#' @param z Pseudo-observation vector; `NA` denotes a missing observation.
#' @param H State-by-time observation design.
#' @param V Positive observation variances.
#' @param GG Evolution matrix or cube.
#' @param m0,C0 Initial state prior.
#' @param evolution Evolution specification.
#' @param backend One of `"cpp"`, `"R"`, or `"auto"`.
#' @param jitter_ladder Declared relative Cholesky jitter ladder.
#' @param numerical_policy Either `"fail"` or `"record_repair"`.
#' @return Filtering and smoothing moments with numerical diagnostics.
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
#' @return Filtering/smoothing moments and one sampled state path.
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
