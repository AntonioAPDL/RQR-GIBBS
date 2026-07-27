#' Evaluate future interval-root paths
#'
#' @param object A fitted RQR object.
#' @param ... Method arguments.
#' @return A model-specific future interval-root evaluation.
#' @export
forecast_paths <- function(object, ...) {
  UseMethod("forecast_paths")
}

#' Evaluate future interval-root paths from an RQR-DESN fit
#'
#' Future features must be fixed explicitly. A validated
#' [rqr_desn_future_design()] records whether they are precomputed,
#' teacher-forced for rolling one-step evaluation, or conditional on an
#' external driver path. The function never generates or samples a response.
#'
#' @param object An `rqr_desn_fit`.
#' @param H Optional horizon; checked against the supplied future design.
#' @param future_design Preferred validated future-design contract.
#' @param X_future Backward-compatible explicit feature matrix with the exact
#'   parent feature names and order. This legacy path is always labeled
#'   non-promotable; use `future_design` for verified semantics.
#' @param nd Number of coefficient draws.
#' @param seed Optional draw-selection seed.
#' @param ... Reserved.
#' @return Interval-root draws and summaries conditional on the future design.
#' @export
forecast_paths.rqr_desn_fit <- function(
    object, H = NULL, future_design = NULL,
    X_future = NULL, nd = NULL, seed = NULL, ...) {
  .rqr_reject_dots(list(...), "forecast_paths.rqr_desn_fit")
  if (!is.null(H)) {
    H <- .rqr_scalar_integer(H, "H", 1L)
  }
  .rqr_validate_desn_fit_envelope(object)
  if (!is.null(future_design) && !is.null(X_future)) {
    stop("Supply future_design or X_future, not both.", call. = FALSE)
  }
  legacy_matrix <- is.null(future_design)
  if (is.null(future_design)) {
    if (is.null(X_future)) {
      stop(
        paste(
          "Supply a frozen future_design or explicit X_future.",
          "Ordinary RQR-DESN has no recursive response simulator."
        ),
        call. = FALSE
      )
    }
    X_future <- .rqr_desn_explicit_future_matrix(
      X_future, object$design, "X_future"
    )
  } else {
    rqr_validate_desn_future_design(
      future_design, parent_design = object$design
    )
  }
  horizon <- if (legacy_matrix) {
    nrow(X_future)
  } else {
    nrow(future_design$X)
  }
  if (!is.null(H)) {
    if (!identical(H, as.integer(horizon))) {
      stop("H must equal the number of future-design rows.",
           call. = FALSE)
    }
  }
  out <- if (legacy_matrix) {
    predict_interval(
      object, X_new = X_future, nd = nd, seed = seed
    )
  } else {
    predict_interval(
      object, future_design = future_design,
      nd = nd, seed = seed
    )
  }
  out$H <- horizon
  out$evaluation_semantics <- if (legacy_matrix) {
    "legacy_explicit_matrix"
  } else {
    future_design$semantics
  }
  out$origin_fixed <- if (legacy_matrix) {
    NA
  } else {
    isTRUE(future_design$driver$origin_fixed)
  }
  out$response_predictive_draws <- FALSE
  out$interpretation <- if (legacy_matrix) {
    paste(
      "Future interval-root functions conditional on a legacy explicit",
      "feature matrix; this path is non-promotable and defines no future",
      "response distribution."
    )
  } else {
    paste(
      "Future interval-root functions conditional on a verified frozen",
      "future-design contract; no future response distribution is defined."
    )
  }
  out
}
