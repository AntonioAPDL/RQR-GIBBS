.rqr_root_label_schema <- function() {
  "rqrgibbs_root_label_contract/1.0.0"
}

.rqr_root_label_control_schema <- function() {
  "rqrgibbs_root_label_control/1.0.0"
}

.rqr_assert_draw_matrix <- function(x, name) {
  x <- as.matrix(x)
  if (!length(x) || nrow(x) < 1L || ncol(x) < 1L ||
      any(!is.finite(x))) {
    stop(sprintf("%s must be a nonempty finite numeric matrix.", name),
         call. = FALSE)
  }
  storage.mode(x) <- "double"
  x
}

.rqr_assert_audit_matrix <- function(X_audit, p, name = "X_audit") {
  X_audit <- as.matrix(X_audit)
  if (!length(X_audit) || nrow(X_audit) < 1L || ncol(X_audit) != p ||
      any(!is.finite(X_audit))) {
    stop(
      sprintf("%s must be a finite matrix with %d columns.", name, p),
      call. = FALSE
    )
  }
  storage.mode(X_audit) <- "double"
  X_audit
}

.rqr_normalize_audit_weights <- function(w, n, name = "audit_weights") {
  if (is.null(w)) return(rep(1 / n, n))
  w <- as.numeric(w)
  if (length(w) != n || any(!is.finite(w)) || any(w < 0) ||
      sum(w) <= 0) {
    stop(sprintf("%s must be a nonnegative finite vector with positive sum.",
                 name), call. = FALSE)
  }
  w / sum(w)
}

.rqr_root_label_reference <- function(
    beta_root1, beta_root2, X_audit, weights,
    reference_beta_lower = NULL, reference_beta_upper = NULL,
    reference_method = c("provided", "iterative_consensus"),
    gap_tolerance = 1e-8, max_iter = 20L) {
  p <- ncol(beta_root1)
  reference_method <- match.arg(reference_method)
  max_iter <- .rqr_scalar_integer(max_iter, "max_iter", 1L)

  if (identical(reference_method, "provided")) {
    lower <- as.numeric(reference_beta_lower)
    upper <- as.numeric(reference_beta_upper)
    if (length(lower) != p || length(upper) != p ||
        any(!is.finite(lower)) || any(!is.finite(upper))) {
      stop(
        "reference_beta_lower and reference_beta_upper must be finite coefficient vectors matching the draw dimension.",
        call. = FALSE
      )
    }
    return(list(
      lower = lower, upper = upper, method = "provided",
      converged = TRUE, iterations = 0L
    ))
  }

  b1_eta <- X_audit %*% t(beta_root1)
  b2_eta <- X_audit %*% t(beta_root2)
  gaps <- b2_eta - b1_eta
  separation <- apply(abs(gaps), 2L, min)
  anchor <- which.max(separation)
  if (!length(anchor) || !is.finite(separation[[anchor]]) ||
      separation[[anchor]] <= gap_tolerance) {
    return(list(
      lower = rep(NA_real_, p), upper = rep(NA_real_, p),
      method = "iterative_consensus", converged = FALSE,
      iterations = 0L, status = "failed_no_separated_anchor"
    ))
  }
  if (sum(weights * gaps[, anchor]) >= 0) {
    ref_lower <- beta_root1[anchor, ]
    ref_upper <- beta_root2[anchor, ]
  } else {
    ref_lower <- beta_root2[anchor, ]
    ref_upper <- beta_root1[anchor, ]
  }
  assignment <- rep(NA_character_, nrow(beta_root1))
  converged <- FALSE
  iterations <- 0L
  for (iter in seq_len(max_iter)) {
    distances <- .rqr_root_label_distances(
      beta_root1, beta_root2, X_audit, weights, ref_lower, ref_upper
    )
    new_assignment <- ifelse(
      distances$D_keep <= distances$D_swap, "keep", "swap"
    )
    lower_draws <- beta_root1
    upper_draws <- beta_root2
    swap <- new_assignment == "swap"
    if (any(swap)) {
      lower_draws[swap, ] <- beta_root2[swap, , drop = FALSE]
      upper_draws[swap, ] <- beta_root1[swap, , drop = FALSE]
    }
    ref_lower <- colMeans(lower_draws)
    ref_upper <- colMeans(upper_draws)
    iterations <- iter
    if (identical(new_assignment, assignment)) {
      converged <- TRUE
      break
    }
    assignment <- new_assignment
  }
  list(
    lower = as.numeric(ref_lower),
    upper = as.numeric(ref_upper),
    method = "iterative_consensus",
    converged = converged,
    iterations = as.integer(iterations),
    anchor_draw = as.integer(anchor)
  )
}

.rqr_root_label_distances <- function(
    beta_root1, beta_root2, X_audit, weights,
    reference_beta_lower, reference_beta_upper) {
  eta1 <- X_audit %*% t(beta_root1)
  eta2 <- X_audit %*% t(beta_root2)
  ref_lower <- drop(X_audit %*% as.numeric(reference_beta_lower))
  ref_upper <- drop(X_audit %*% as.numeric(reference_beta_upper))
  D_keep <- colSums(weights * sweep(eta1, 1L, ref_lower, "-")^2) +
    colSums(weights * sweep(eta2, 1L, ref_upper, "-")^2)
  D_swap <- colSums(weights * sweep(eta2, 1L, ref_lower, "-")^2) +
    colSums(weights * sweep(eta1, 1L, ref_upper, "-")^2)
  list(D_keep = as.numeric(D_keep), D_swap = as.numeric(D_swap))
}

.rqr_root_label_finalize <- function(
    beta_root1, beta_root2, X_audit, weights, reference,
    gap_tolerance, ambiguity_tolerance, fail_on_ambiguous,
    object_label = "coefficient") {
  distances <- .rqr_root_label_distances(
    beta_root1, beta_root2, X_audit, weights,
    reference$lower, reference$upper
  )
  assignment <- ifelse(distances$D_keep <= distances$D_swap, "keep", "swap")
  beta_lower <- beta_root1
  beta_upper <- beta_root2
  swap <- assignment == "swap"
  if (any(swap)) {
    beta_lower[swap, ] <- beta_root2[swap, , drop = FALSE]
    beta_upper[swap, ] <- beta_root1[swap, , drop = FALSE]
  }

  ref_gap <- drop(X_audit %*% (as.numeric(reference$upper) -
                                as.numeric(reference$lower)))
  gap <- X_audit %*% t(beta_upper - beta_lower)
  min_gap <- apply(gap, 2L, min)
  max_gap <- apply(gap, 2L, max)
  crossing <- min_gap < -gap_tolerance
  touching <- !crossing & min_gap <= gap_tolerance
  margin <- abs(distances$D_keep - distances$D_swap) /
    (1 + distances$D_keep + distances$D_swap)
  ambiguous <- margin <= ambiguity_tolerance

  reference_separated <- all(is.finite(ref_gap)) &&
    min(ref_gap) > gap_tolerance
  status <- "ok"
  if (!isTRUE(reference$converged)) {
    status <- reference$status %||% "failed_reference_not_converged"
  } else if (!reference_separated) {
    status <- "failed_reference_not_globally_separated"
  } else if (any(crossing)) {
    status <- "failed_draw_crossing_on_audit_domain"
  } else if (any(ambiguous)) {
    status <- "failed_ambiguous_assignment"
  }
  if (!identical(status, "ok") && isTRUE(fail_on_ambiguous)) {
    stop(
      sprintf(
        "Root %s canonicalization failed with status '%s'.",
        object_label, status
      ),
      call. = FALSE
    )
  }
  list(
    status = status,
    beta_lower = if (identical(status, "ok")) beta_lower else NULL,
    beta_upper = if (identical(status, "ok")) beta_upper else NULL,
    assignment = assignment,
    D_keep = distances$D_keep,
    D_swap = distances$D_swap,
    assignment_margin = margin,
    min_signed_gap = min_gap,
    max_signed_gap = max_gap,
    crossing = crossing,
    touching = touching,
    ambiguous = ambiguous,
    reference_gap_min = if (length(ref_gap)) min(ref_gap) else NA_real_,
    reference_gap_max = if (length(ref_gap)) max(ref_gap) else NA_real_,
    reference_separated = reference_separated
  )
}

#' Canonicalize exchangeable static RQR root draws
#'
#' RQR root coefficients are exchangeable under the symmetric two-root target.
#' This post-processing helper tries to assign each complete root block to a
#' single lower/upper coefficient chart on a declared audit design. It never
#' alters the Markov chain state, never relabels coefficients componentwise, and
#' fails closed when the fitted linear roots cross or are assignment-ambiguous
#' on the audit domain.
#'
#' @param beta_root1,beta_root2 Retained raw root coefficient draws, as
#'   draw-by-coefficient matrices.
#' @param X_audit Design matrix defining the audit domain on which a global
#'   lower/upper coefficient labeling is requested.
#' @param reference_beta_lower,reference_beta_upper Optional reference
#'   lower/upper coefficient vectors. If omitted, `reference_method` must be
#'   `"iterative_consensus"`.
#' @param audit_weights Optional nonnegative row weights for `X_audit`.
#' @param gap_tolerance Nonnegative tolerance for declaring root crossings or
#'   nonseparated references.
#' @param ambiguity_tolerance Nonnegative tolerance for declaring keep/swap
#'   assignment ties.
#' @param reference_method Either `"provided"` or `"iterative_consensus"`.
#' @param max_iter Maximum consensus iterations for `"iterative_consensus"`.
#' @param fail_on_ambiguous If `TRUE`, stop when the global chart is not
#'   supported. If `FALSE`, return a diagnostic object with `status != "ok"`.
#' @return A `rqr_root_label_diagnostics` object. Canonical coefficient draws
#'   are populated only when `status == "ok"`.
#' @export
rqr_canonicalize_root_draws <- function(
    beta_root1, beta_root2, X_audit,
    reference_beta_lower = NULL, reference_beta_upper = NULL,
    audit_weights = NULL, gap_tolerance = 1e-8,
    ambiguity_tolerance = 1e-8,
    reference_method = if (is.null(reference_beta_lower) &&
                           is.null(reference_beta_upper)) {
      "iterative_consensus"
    } else {
      "provided"
    },
    max_iter = 20L, fail_on_ambiguous = FALSE) {
  beta_root1 <- .rqr_assert_draw_matrix(beta_root1, "beta_root1")
  beta_root2 <- .rqr_assert_draw_matrix(beta_root2, "beta_root2")
  if (!identical(dim(beta_root1), dim(beta_root2))) {
    stop("beta_root1 and beta_root2 must have identical dimensions.",
         call. = FALSE)
  }
  X_audit <- .rqr_assert_audit_matrix(X_audit, ncol(beta_root1))
  weights <- .rqr_normalize_audit_weights(
    audit_weights, nrow(X_audit), "audit_weights"
  )
  gap_tolerance <- as.numeric(gap_tolerance)[1L]
  ambiguity_tolerance <- as.numeric(ambiguity_tolerance)[1L]
  if (!is.finite(gap_tolerance) || gap_tolerance < 0 ||
      !is.finite(ambiguity_tolerance) || ambiguity_tolerance < 0) {
    stop("gap_tolerance and ambiguity_tolerance must be finite and nonnegative.",
         call. = FALSE)
  }
  reference_method <- match.arg(reference_method,
                                c("provided", "iterative_consensus"))
  reference <- .rqr_root_label_reference(
    beta_root1, beta_root2, X_audit, weights,
    reference_beta_lower = reference_beta_lower,
    reference_beta_upper = reference_beta_upper,
    reference_method = reference_method,
    gap_tolerance = gap_tolerance,
    max_iter = max_iter
  )
  final <- .rqr_root_label_finalize(
    beta_root1, beta_root2, X_audit, weights, reference,
    gap_tolerance, ambiguity_tolerance, fail_on_ambiguous,
    object_label = "coefficient"
  )
  out <- list(
    schema_version = .rqr_root_label_schema(),
    object = "static_root_coefficients",
    root_estimand = "unordered_root_pair",
    raw_root_labels_identified = FALSE,
    canonicalization_changes_chain = FALSE,
    audit_domain = list(
      n = nrow(X_audit),
      p = ncol(X_audit),
      weights_digest = .rqr_digest(weights),
      X_digest = .rqr_digest(X_audit)
    ),
    tolerances = list(
      gap_tolerance = gap_tolerance,
      ambiguity_tolerance = ambiguity_tolerance
    ),
    reference = reference,
    status = final$status,
    canonical_beta_lower = final$beta_lower,
    canonical_beta_upper = final$beta_upper,
    assignment = final$assignment,
    diagnostics = final[setdiff(names(final), c("beta_lower", "beta_upper"))],
    summary = list(
      n_draws = nrow(beta_root1),
      swap_fraction = mean(final$assignment == "swap"),
      crossing_fraction = mean(final$crossing),
      touching_fraction = mean(final$touching),
      ambiguity_fraction = mean(final$ambiguous),
      min_reference_gap = final$reference_gap_min,
      min_draw_gap = min(final$min_signed_gap),
      min_assignment_margin = min(final$assignment_margin)
    ),
    interpretation = paste(
      "Canonical root coefficients are post-processing summaries for a",
      "declared audit design. Raw labels remain exchangeable MCMC labels;",
      "ordered interval endpoints are always available by pointwise sorting."
    )
  )
  class(out) <- c("rqr_root_label_diagnostics", "list")
  out
}

#' Canonicalize exchangeable RQR-DLM root paths
#'
#' This helper applies the same complete-root-block principle to retained
#' dynamic root ordinate paths. The keep/swap decision is one binary assignment
#' per retained path, based on a weighted path-space distance to a reference
#' lower/upper path. It does not relabel individual times separately.
#'
#' @param eta_root1,eta_root2 Time-by-draw raw root ordinate matrices.
#' @param reference_eta_lower,reference_eta_upper Optional reference lower/upper
#'   paths. If omitted, an iterative consensus reference is used.
#' @param time_weights Optional nonnegative weights over time points.
#' @inheritParams rqr_canonicalize_root_draws
#' @return A `rqr_root_path_label_diagnostics` object.
#' @export
rqr_canonicalize_root_paths <- function(
    eta_root1, eta_root2,
    reference_eta_lower = NULL, reference_eta_upper = NULL,
    time_weights = NULL, gap_tolerance = 1e-8,
    ambiguity_tolerance = 1e-8,
    reference_method = if (is.null(reference_eta_lower) &&
                           is.null(reference_eta_upper)) {
      "iterative_consensus"
    } else {
      "provided"
    },
    max_iter = 20L, fail_on_ambiguous = FALSE) {
  eta_root1 <- .rqr_assert_draw_matrix(t(as.matrix(eta_root1)), "eta_root1")
  eta_root2 <- .rqr_assert_draw_matrix(t(as.matrix(eta_root2)), "eta_root2")
  if (!identical(dim(eta_root1), dim(eta_root2))) {
    stop("eta_root1 and eta_root2 must have identical dimensions.",
         call. = FALSE)
  }
  X_identity <- diag(ncol(eta_root1))
  ref_lower <- if (is.null(reference_eta_lower)) NULL else
    as.numeric(reference_eta_lower)
  ref_upper <- if (is.null(reference_eta_upper)) NULL else
    as.numeric(reference_eta_upper)
  weights <- .rqr_normalize_audit_weights(
    time_weights, ncol(eta_root1), "time_weights"
  )
  out <- rqr_canonicalize_root_draws(
    beta_root1 = eta_root1,
    beta_root2 = eta_root2,
    X_audit = X_identity,
    reference_beta_lower = ref_lower,
    reference_beta_upper = ref_upper,
    audit_weights = weights,
    gap_tolerance = gap_tolerance,
    ambiguity_tolerance = ambiguity_tolerance,
    reference_method = reference_method,
    max_iter = max_iter,
    fail_on_ambiguous = fail_on_ambiguous
  )
  out$schema_version <- .rqr_root_label_schema()
  out$object <- "dynamic_root_ordinate_paths"
  out$canonical_eta_lower <- if (identical(out$status, "ok")) {
    t(out$canonical_beta_lower)
  } else {
    NULL
  }
  out$canonical_eta_upper <- if (identical(out$status, "ok")) {
    t(out$canonical_beta_upper)
  } else {
    NULL
  }
  out$canonical_beta_lower <- NULL
  out$canonical_beta_upper <- NULL
  out$interpretation <- paste(
    "Canonical path labels are complete-path post-processing summaries.",
    "They do not modify the DLM chain and do not define response draws."
  )
  class(out) <- c("rqr_root_path_label_diagnostics",
                  "rqr_root_label_diagnostics", "list")
  out
}

.rqr_normalize_root_label_control <- function(control, X) {
  if (is.null(control)) control <- list()
  if (is.logical(control) && length(control) == 1L && !is.na(control)) {
    control <- list(canonicalize_draws = control)
  }
  if (!is.list(control)) {
    stop("mcmc_control$root_label_control must be a list or TRUE/FALSE.",
         call. = FALSE)
  }
  swap_probability <- as.numeric(control$swap_probability %||% 0.5)[1L]
  if (!is.finite(swap_probability) ||
      swap_probability < 0 || swap_probability > 1) {
    stop("root_label_control$swap_probability must be in [0, 1].",
         call. = FALSE)
  }
  canonicalize_draws <- isTRUE(control$canonicalize_draws %||% TRUE)
  audit_X <- control$X_audit %||% control$audit_X %||% X
  audit_X <- .rqr_assert_audit_matrix(audit_X, ncol(X),
                                      "root_label_control$audit_X")
  gap_tolerance <- as.numeric(control$gap_tolerance %||% 1e-8)[1L]
  ambiguity_tolerance <- as.numeric(control$ambiguity_tolerance %||%
                                      1e-8)[1L]
  if (!is.finite(gap_tolerance) || gap_tolerance < 0 ||
      !is.finite(ambiguity_tolerance) || ambiguity_tolerance < 0) {
    stop(
      "root_label_control gap and ambiguity tolerances must be finite and nonnegative.",
      call. = FALSE
    )
  }
  fail_on_ambiguous <- isTRUE(control$fail_on_ambiguous %||% FALSE)
  reference_method <- as.character(control$reference_method %||%
                                     "iterative_consensus")[1L]
  if (!reference_method %in% c("provided", "iterative_consensus")) {
    stop(
      "root_label_control$reference_method must be 'provided' or 'iterative_consensus'.",
      call. = FALSE
    )
  }
  list(
    schema_version = .rqr_root_label_control_schema(),
    swap_probability = swap_probability,
    canonicalize_draws = canonicalize_draws,
    audit_X = audit_X,
    audit_weights = control$audit_weights %||% NULL,
    gap_tolerance = gap_tolerance,
    ambiguity_tolerance = ambiguity_tolerance,
    reference_method = reference_method,
    reference_beta_lower = control$reference_beta_lower %||% NULL,
    reference_beta_upper = control$reference_beta_upper %||% NULL,
    max_iter = .rqr_scalar_integer(control$max_iter %||% 20L,
                                   "root_label_control$max_iter", 1L),
    fail_on_ambiguous = fail_on_ambiguous,
    contract = list(
      schema_version = .rqr_root_label_control_schema(),
      root_estimand = "unordered_root_pair",
      raw_root_labels_identified = FALSE,
      complete_root_swap = TRUE,
      swap_probability = swap_probability,
      canonicalization_is_postprocessing = TRUE,
      canonicalize_draws = canonicalize_draws,
      audit_X_digest = .rqr_digest(audit_X),
      reference_method = reference_method,
      gap_tolerance = gap_tolerance,
      ambiguity_tolerance = ambiguity_tolerance
    )
  )
}
