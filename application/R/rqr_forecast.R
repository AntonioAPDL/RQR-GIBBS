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
#' @return A typed and digested `rqr_desn_prediction` containing interval-root
#'   draws and summaries conditional on the future design.
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
  horizon <- if (is.null(future_design)) {
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
  out <- .rqr_desn_predict_impl(
    object = object,
    X_new = X_future,
    future_design = future_design,
    nd = nd,
    seed = seed,
    evaluation_api = "forecast_paths"
  )
  .rqr_validate_desn_prediction(object, out)
  out
}
