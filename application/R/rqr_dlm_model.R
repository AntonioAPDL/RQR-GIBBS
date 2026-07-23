.rqr_block_diag <- function(a, b) {
  a <- as.matrix(a)
  b <- as.matrix(b)
  out <- matrix(0, nrow(a) + nrow(b), ncol(a) + ncol(b))
  out[seq_len(nrow(a)), seq_len(ncol(a))] <- a
  out[nrow(a) + seq_len(nrow(b)), ncol(a) + seq_len(ncol(b))] <- b
  out
}

.rqr_expand_columns <- function(x, n_time, name) {
  x <- as.matrix(x)
  if (ncol(x) == n_time) return(x)
  if (ncol(x) == 1L) return(x[, rep(1L, n_time), drop = FALSE])
  stop(sprintf("%s must have one column or n_time columns.", name), call. = FALSE)
}

.rqr_expand_cube <- function(x, n_time, p, name) {
  dx <- dim(x)
  if (is.null(dx) && length(x) == p * p) {
    x <- matrix(as.numeric(x), p, p)
    dx <- dim(x)
  }
  if (length(dx) == 2L) {
    x <- as.matrix(x)
    if (!all(dim(x) == c(p, p))) stop(sprintf("%s has incompatible dimensions.", name), call. = FALSE)
    return(array(rep(x, n_time), dim = c(p, p, n_time)))
  }
  if (length(dx) == 3L && all(dx[1:2] == c(p, p))) {
    if (dx[3L] == n_time) return(x)
    if (dx[3L] == 1L) return(array(rep(x[, , 1L], n_time), dim = c(p, p, n_time)))
  }
  stop(sprintf("%s must be p x p or p x p x n_time.", name), call. = FALSE)
}

#' Validate or convert an RQR-DLM model
#'
#' The list contract (`FF`, `GG`, `m0`, and `C0`) is compatible with exdqlm
#' 1.1.0 model objects.
#'
#' @param model A model list or exdqlm-compatible object.
#' @return An `rqr_dlm_model`.
#' @export
rqr_as_dlm_model <- function(model) {
  if (!is.list(model) || !all(c("FF", "GG", "m0", "C0") %in% names(model))) {
    stop("model must contain FF, GG, m0, and C0.", call. = FALSE)
  }
  m0 <- as.numeric(model$m0)
  p <- length(m0)
  FF <- as.matrix(model$FF)
  GG <- model$GG
  C0 <- as.matrix(model$C0)
  storage.mode(FF) <- storage.mode(C0) <- "double"
  if (p < 1L || ncol(FF) < 1L || nrow(FF) != p || !all(dim(C0) == c(p, p))) {
    stop("FF, m0, and C0 have incompatible dimensions.", call. = FALSE)
  }
  dg <- dim(GG)
  if (!(length(dg) %in% c(2L, 3L)) || !all(dg[1:2] == c(p, p))) {
    stop("GG must be p x p or p x p x T.", call. = FALSE)
  }
  if (any(!is.finite(FF)) || any(!is.finite(GG)) || any(!is.finite(m0)) || any(!is.finite(C0))) {
    stop("model matrices must be finite.", call. = FALSE)
  }
  C0 <- .rqr_validate_symmetric_matrix(C0, "C0")
  .rqr_chol_with_jitter(C0, jitter_ladder = 0)
  component_dims <- .rqr_positive_integer_vector(
    model$component_dims %||% p, "component_dims"
  )
  component_names <- as.character(model$component_names %||% "component1")
  if (!length(component_dims) || anyNA(component_dims) || any(component_dims < 1L) ||
      sum(component_dims) != p) {
    stop("component_dims must be positive integers summing to length(m0).", call. = FALSE)
  }
  if (length(component_names) != length(component_dims) || anyNA(component_names) ||
      any(!nzchar(component_names)) || anyDuplicated(component_names)) {
    stop("component_names must be unique nonempty names matching component_dims.", call. = FALSE)
  }
  out <- list(
    FF = FF,
    GG = GG,
    m0 = matrix(m0, p, 1L),
    C0 = C0,
    component_dims = component_dims,
    component_names = component_names
  )
  class(out) <- "rqr_dlm_model"
  out
}

#' Polynomial-trend RQR-DLM component
#'
#' @param order Positive polynomial order.
#' @param m0 Prior state mean.
#' @param C0 Prior state covariance.
#' @param name Component name.
#' @return An `rqr_dlm_model`.
#' @export
rqr_polytrend <- function(order = 1L, m0 = NULL, C0 = NULL, name = "trend") {
  order <- .rqr_scalar_integer(order, "order", 1L)
  GG <- diag(order)
  if (order > 1L) GG[cbind(seq_len(order - 1L), 2:order)] <- 1
  FF <- matrix(c(1, rep(0, order - 1L)), order, 1L)
  if (is.null(m0)) m0 <- numeric(order)
  if (is.null(C0)) C0 <- diag(1e3, order)
  rqr_as_dlm_model(list(
    FF = FF, GG = GG, m0 = m0, C0 = C0,
    component_dims = order, component_names = name
  ))
}

#' Fourier seasonal RQR-DLM component
#'
#' @param period Positive seasonal period.
#' @param harmonics Integer harmonics.
#' @param m0 Prior state mean.
#' @param C0 Prior state covariance.
#' @param name Component name.
#' @return An `rqr_dlm_model`.
#' @export
rqr_seasonal <- function(period, harmonics = 1L, m0 = NULL, C0 = NULL, name = "seasonal") {
  period <- as.numeric(period)[1L]
  if (!is.numeric(harmonics) || !length(harmonics) || anyNA(harmonics) ||
      any(!is.finite(harmonics)) || any(harmonics != floor(harmonics))) {
    stop("period and harmonics define an invalid Fourier component.", call. = FALSE)
  }
  harmonics <- as.integer(harmonics)
  if (!is.finite(period) || period <= 1 || !length(harmonics) ||
      any(!is.finite(harmonics)) || any(harmonics < 1L) ||
      any(2 * harmonics > period)) {
    stop("period and harmonics define an invalid Fourier component.", call. = FALSE)
  }
  blocks <- vector("list", length(harmonics))
  fblocks <- vector("list", length(harmonics))
  for (j in seq_along(harmonics)) {
    w <- 2 * pi * harmonics[j] / period
    if (abs(w - pi) <= 1e-12) {
      blocks[[j]] <- matrix(-1, 1L, 1L)
      fblocks[[j]] <- 1
    } else {
      blocks[[j]] <- matrix(c(cos(w), sin(w), -sin(w), cos(w)), 2L, 2L, byrow = TRUE)
      fblocks[[j]] <- c(1, 0)
    }
  }
  GG <- blocks[[1L]]
  if (length(blocks) > 1L) for (j in 2:length(blocks)) GG <- .rqr_block_diag(GG, blocks[[j]])
  FF <- matrix(unlist(fblocks), ncol = 1L)
  p <- nrow(GG)
  if (is.null(m0)) m0 <- numeric(p)
  if (is.null(C0)) C0 <- diag(1e3, p)
  rqr_as_dlm_model(list(
    FF = FF, GG = GG, m0 = m0, C0 = C0,
    component_dims = p, component_names = name
  ))
}

#' Regression-state RQR-DLM component
#'
#' @param X Time-by-regressor design matrix.
#' @param m0 Prior state mean.
#' @param C0 Prior state covariance.
#' @param name Component name.
#' @return An `rqr_dlm_model`.
#' @export
rqr_regression <- function(X, m0 = NULL, C0 = NULL, name = "regression") {
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  if (!nrow(X) || !ncol(X) || any(!is.finite(X))) stop("X must be a finite T x p matrix.", call. = FALSE)
  p <- ncol(X)
  if (is.null(m0)) m0 <- numeric(p)
  if (is.null(C0)) C0 <- diag(1e3, p)
  rqr_as_dlm_model(list(
    FF = t(X), GG = diag(p), m0 = m0, C0 = C0,
    component_dims = p, component_names = name
  ))
}

#' @export
`+.rqr_dlm_model` <- function(e1, e2) {
  e1 <- rqr_as_dlm_model(e1)
  e2 <- rqr_as_dlm_model(e2)
  t1 <- ncol(e1$FF)
  t2 <- ncol(e2$FF)
  nt <- max(t1, t2)
  if (t1 != t2 && t1 != 1L && t2 != 1L) stop("Component FF time dimensions are incompatible.", call. = FALSE)
  FF <- rbind(.rqr_expand_columns(e1$FF, nt, "e1$FF"), .rqr_expand_columns(e2$FF, nt, "e2$FF"))
  p1 <- length(e1$m0)
  p2 <- length(e2$m0)
  d1 <- dim(e1$GG)
  d2 <- dim(e2$GG)
  gt <- max(if (length(d1) == 3L) d1[3L] else 1L, if (length(d2) == 3L) d2[3L] else 1L)
  if (gt > 1L && nt > 1L && gt != nt) stop("Component GG and FF time dimensions are incompatible.", call. = FALSE)
  g1 <- .rqr_expand_cube(e1$GG, gt, p1, "e1$GG")
  g2 <- .rqr_expand_cube(e2$GG, gt, p2, "e2$GG")
  GG <- array(0, c(p1 + p2, p1 + p2, gt))
  for (tt in seq_len(gt)) GG[, , tt] <- .rqr_block_diag(g1[, , tt], g2[, , tt])
  if (gt == 1L) GG <- GG[, , 1L]
  rqr_as_dlm_model(list(
    FF = FF,
    GG = GG,
    m0 = c(e1$m0, e2$m0),
    C0 = .rqr_block_diag(e1$C0, e2$C0),
    component_dims = c(e1$component_dims, e2$component_dims),
    component_names = c(e1$component_names, e2$component_names)
  ))
}

#' Build an exdqlm-compatible component discount matrix
#'
#' @param df Component-specific discounts in `(0,1]`.
#' @param dim.df State dimensions for those components.
#' @param p Total state dimension; defaults to `sum(dim.df)`.
#' @return A `p x p` block matrix.
#' @export
rqr_discount_matrix <- function(df, dim.df, p = sum(dim.df)) {
  df <- as.numeric(df)
  dim.df <- .rqr_positive_integer_vector(dim.df, "dim.df")
  p <- .rqr_scalar_integer(p, "p", 1L)
  if (!length(df) || length(df) != length(dim.df) || any(!is.finite(df)) ||
      any(df <= 0) || any(df > 1) || any(!is.finite(dim.df)) ||
      any(dim.df < 1L) || sum(dim.df) != p) {
    stop("df must be in (0,1], length(df)=length(dim.df), and sum(dim.df)=p.", call. = FALSE)
  }
  D <- matrix(0, p, p)
  ends <- cumsum(dim.df)
  starts <- c(1L, utils::head(ends, -1L) + 1L)
  for (j in seq_along(df)) D[starts[j]:ends[j], starts[j]:ends[j]] <- (1 - df[j]) / df[j]
  D
}

.rqr_expand_model <- function(model, n_time) {
  model <- rqr_as_dlm_model(model)
  p <- length(model$m0)
  list(
    FF = .rqr_expand_columns(model$FF, n_time, "model$FF"),
    GG = .rqr_expand_cube(model$GG, n_time, p, "model$GG"),
    m0 = as.numeric(model$m0),
    C0 = model$C0,
    component_dims = model$component_dims,
    component_names = model$component_names,
    p = p,
    n_time = n_time
  )
}

#' Freeze a component-discount covariance template
#'
#' @param model RQR-DLM model.
#' @param n_time Number of time points.
#' @param df,dim.df exdqlm-compatible component discounts and dimensions.
#' @param reference_variance Positive scalar or length-`n_time` vector.
#' @param reference_design Optional `p x n_time` pseudo-design; defaults to FF.
#' @param numerical_policy Either `"fail"` or `"record_repair"`.
#' @param jitter_ladder Matrix-relative jitter ladder used only under
#'   record-repair. An exactly zero matrix uses a separately recorded absolute
#'   fallback.
#' @return An evolution specification containing a fixed `W` cube and a
#'   construction audit.
#' @export
rqr_freeze_discount_template <- function(model, n_time, df, dim.df,
                                          reference_variance,
                                          reference_design = NULL,
                                          numerical_policy = c("fail", "record_repair"),
                                          jitter_ladder = c(0, 1e-12, 1e-10, 1e-8, 1e-6)) {
  ex <- .rqr_expand_model(model, n_time)
  D <- rqr_discount_matrix(df, dim.df, ex$p)
  reference_variance <- as.numeric(reference_variance)
  if (!length(reference_variance) %in% c(1L, n_time) ||
      any(!is.finite(reference_variance)) || any(reference_variance <= 0)) {
    stop("reference_variance must be positive and have length 1 or n_time.", call. = FALSE)
  }
  V <- rep_len(reference_variance, n_time)
  H <- if (is.null(reference_design)) ex$FF else as.matrix(reference_design)
  if (!all(dim(H) == c(ex$p, n_time)) || any(!is.finite(H))) {
    stop("reference_design must be finite p x n_time.", call. = FALSE)
  }
  C <- ex$C0
  W <- array(0, c(ex$p, ex$p, n_time))
  numerical_policy <- .rqr_numerical_policy(numerical_policy)
  ladder <- .rqr_jitter_ladder(numerical_policy, jitter_ladder)
  repair_records <- .rqr_empty_repair_records()
  minimum_eigenvalue <- numeric(n_time)
  for (tt in seq_len(n_time)) {
    P <- .rqr_symmetrize(ex$GG[, , tt] %*% C %*% t(ex$GG[, , tt]))
    W[, , tt] <- .rqr_symmetrize(D * P)
    R <- .rqr_symmetrize(P + W[, , tt])
    h <- H[, tt]
    q <- drop(crossprod(h, R %*% h)) + V[tt]
    if (!is.finite(q) || q <= 0) stop("Reference discount recursion produced a nonpositive variance.", call. = FALSE)
    gain <- drop(R %*% h) / q
    identity <- diag(ex$p)
    # Joseph form avoids subtractive loss of positive semidefiniteness.
    C <- .rqr_symmetrize(
      (identity - tcrossprod(gain, h)) %*% R %*% t(identity - tcrossprod(gain, h)) +
        tcrossprod(gain) * V[tt]
    )
    minimum_eigenvalue[tt] <- min(eigen(C, symmetric = TRUE, only.values = TRUE)$values)
    fac <- .rqr_chol_with_jitter(C, ladder)
    repair_records <- .rqr_add_repair_record(
      repair_records,
      stage = "discount_template_filter_covariance",
      time = tt,
      info = list(
        strategy = "cholesky_jitter",
        jitter = fac$jitter,
        relative_jitter = fac$relative_jitter,
        min_eigenvalue = fac$min_eigenvalue,
        matrix_scale = fac$matrix_scale,
        jitter_scale = fac$jitter_scale,
        absolute_jitter_fallback = fac$absolute_jitter_fallback,
        clamped_eigenvalues = 0L
      )
    )
    C <- fac$matrix
  }
  structure(list(
    mode = "discount_template",
    W = W,
    df = as.numeric(df),
    dim.df = as.integer(dim.df),
    D = D,
    reference_variance = V,
    reference_design = H,
    exact_joint_target = TRUE,
    frozen_before_mcmc = TRUE,
    construction_audit = list(
      numerical_policy = numerical_policy,
      repair_count = nrow(repair_records),
      repair_records = repair_records,
      repair_time = repair_records$time,
      repair_jitter = repair_records$jitter,
      minimum_eigenvalue = minimum_eigenvalue
    )
  ), class = "rqr_evolution")
}
