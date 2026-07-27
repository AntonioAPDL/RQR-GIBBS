# Fixed-design data and target contracts for ordinary RQR.

.rqr_static_fit_schema <- function() "rqrgibbs_static_fit/1.2.0"

.rqr_static_checkpoint_schema <- function() {
  "rqrgibbs_static_checkpoint/1.0.0"
}

.rqr_static_schedule_schema <- function() {
  "rqrgibbs_static_segment_schedule/2.0.0"
}

.rqr_static_data_schema <- function() {
  "rqrgibbs_fixed_design_data/1.0.0"
}

.rqr_static_target_schema <- function() {
  "rqrgibbs_ordinary_target/1.0.0"
}

.rqr_static_transition_version <- function() {
  "rqrgibbs_fixed_design_transition/1.0.0"
}

.rqr_static_draws_schema <- function() {
  "rqrgibbs_static_draws/1.0.0"
}

.rqr_static_draw_source_schema <- function() {
  "rqrgibbs_static_draw_source/1.0.0"
}

.rqr_static_draw_selection_schema <- function() {
  "rqrgibbs_static_draw_selection/1.0.0"
}

.rqr_static_prediction_schema <- function() {
  "rqrgibbs_interval_prediction/2.0.0"
}

.rqr_static_prediction_source_schema <- function() {
  "rqrgibbs_static_prediction_source/1.0.0"
}

.rqr_fixed_design_data <- function(y, X) {
  if (!is.numeric(y) || !is.null(dim(y)) ||
      is.object(y) && !is.atomic(y)) {
    stop("y must be a numeric vector.", call. = FALSE)
  }
  y <- as.numeric(y)
  if (!length(y)) {
    stop("y must contain at least one entry.", call. = FALSE)
  }
  if (any(is.nan(y)) || any(is.infinite(y))) {
    stop("y may contain NA, but not NaN or infinite values.", call. = FALSE)
  }

  if (!is.matrix(X) || !is.numeric(X)) {
    stop("X must be a numeric matrix.", call. = FALSE)
  }
  X_names <- colnames(X)
  X_rows <- rownames(X)
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  if (!nrow(X) || length(y) != nrow(X)) {
    stop("length(y) must equal nrow(X), with at least one row.", call. = FALSE)
  }
  if (!ncol(X)) {
    stop("X must have at least one column.", call. = FALSE)
  }
  if (any(!is.finite(X))) {
    stop("X must contain only finite values.", call. = FALSE)
  }
  if (!is.null(X_names)) {
    if (length(X_names) != ncol(X) ||
        anyNA(X_names) || any(!nzchar(X_names)) ||
        anyDuplicated(X_names)) {
      stop(
        paste(
          "If supplied, colnames(X) must be complete, nonempty,",
          "and unique."
        ),
        call. = FALSE
      )
    }
    colnames(X) <- X_names
  }
  if (!is.null(X_rows)) rownames(X) <- X_rows

  observed <- !is.na(y)
  if (!any(observed)) {
    stop("At least one response must be observed.", call. = FALSE)
  }
  observed_index <- which(observed)
  row_ids <- rownames(X)
  if (is.null(row_ids) || length(row_ids) != nrow(X) ||
      anyNA(row_ids) || any(!nzchar(row_ids)) || anyDuplicated(row_ids)) {
    row_ids <- as.character(seq_len(nrow(X)))
  }
  column_names <- colnames(X)
  named_columns <- !is.null(column_names)

  contract <- list(
    schema_version = .rqr_static_data_schema(),
    y = y,
    X = X,
    observed = observed,
    observed_index = observed_index,
    n_total = nrow(X),
    n_observed = length(observed_index),
    p = ncol(X),
    row_ids = row_ids,
    column_names = if (named_columns) column_names else NULL,
    named_columns = named_columns
  )
  contract$data_digest <- .rqr_digest(list(
    schema_version = contract$schema_version,
    y = contract$y,
    observed = contract$observed,
    row_ids = contract$row_ids
  ))
  contract$design_digest <- .rqr_digest(list(
    X = contract$X,
    column_names = contract$column_names,
    row_ids = contract$row_ids
  ))
  class(contract) <- c("rqr_fixed_design_data", "list")
  contract
}

.rqr_static_provenance_data <- function(data) {
  list(
    y = as.numeric(data$y),
    observed = as.logical(data$observed),
    row_ids = as.character(data$row_ids)
  )
}

.rqr_validate_root_swap_probability <- function(value) {
  .rqr_scalar_numeric(
    value, "root_swap_probability", lower = 0, upper = 1
  )
}

.rqr_assert_data_only_contract <- function(
    x, name = "contract", depth = 0L) {
  if (depth > 100L) {
    stop(
      sprintf("%s exceeds the supported nesting depth.", name),
      call. = FALSE
    )
  }
  if (isS4(x)) {
    stop(
      sprintf("%s must not contain S4 objects.", name),
      call. = FALSE
    )
  }
  kind <- typeof(x)
  allowed_atomic <- c(
    "NULL", "logical", "integer", "double", "complex",
    "character", "raw"
  )
  if (kind %in% allowed_atomic) {
    attributes_x <- attributes(x)
    if (!is.null(attributes_x)) {
      for (attribute_name in names(attributes_x)) {
        .rqr_assert_data_only_contract(
          attributes_x[[attribute_name]],
          sprintf("attr(%s, %s)", name, attribute_name),
          depth = depth + 1L
        )
      }
    }
    return(invisible(TRUE))
  }
  if (!identical(kind, "list")) {
    stop(
      sprintf(
        paste0(
          "%s must contain data only; objects of type '%s' ",
          "are not permitted."
        ),
        name, kind
      ),
      call. = FALSE
    )
  }
  for (index in seq_along(x)) {
    element_name <- names(x)[index]
    if (is.null(element_name) || is.na(element_name) ||
        !nzchar(element_name)) {
      element_name <- as.character(index)
    }
    .rqr_assert_data_only_contract(
      x[[index]],
      sprintf("%s[[%s]]", name, element_name),
      depth = depth + 1L
    )
  }
  attributes_x <- attributes(x)
  if (!is.null(attributes_x)) {
    for (attribute_name in names(attributes_x)) {
      .rqr_assert_data_only_contract(
        attributes_x[[attribute_name]],
        sprintf("attr(%s, %s)", name, attribute_name),
        depth = depth + 1L
      )
    }
  }
  invisible(TRUE)
}

.rqr_static_target_contract <- function(
    coverage_level, learning_rate_mode, fixed_learning_rate,
    loss_reference_scale, lambda_prior, beta_prior,
    numerical_policy, precision_beta, root_swap_probability,
    embedding_contract = NULL) {
  mode <- .rqr_learning_rate_mode(learning_rate_mode)
  fixed_learning_rate <- if (identical(mode, "fixed_rate")) {
    .rqr_scalar_numeric(
      fixed_learning_rate, "fixed_learning_rate",
      lower = 0, lower_open = TRUE
    )
  } else {
    NA_real_
  }
  loss_reference_scale <- .rqr_scalar_numeric(
    loss_reference_scale, "loss_reference_scale",
    lower = 0, lower_open = TRUE
  )
  if (!is.null(embedding_contract) && !is.list(embedding_contract)) {
    stop("embedding_contract must be NULL or a plain list.", call. = FALSE)
  }
  if (!is.null(embedding_contract)) {
    .rqr_assert_data_only_contract(
      embedding_contract, "embedding_contract"
    )
  }
  list(
    schema_version = .rqr_static_target_schema(),
    tilt = 0,
    loss_name = "rqr_residual_product_check_loss",
    coverage_level = rqr_constants(coverage_level)$alpha,
    learning_rate_mode = mode,
    fixed_learning_rate = fixed_learning_rate,
    loss_reference_scale = loss_reference_scale,
    lambda_prior = .rqr_lambda_prior(lambda_prior, mode),
    beta_prior = .rqr_prior_target_contract(beta_prior),
    roots_exchangeable = TRUE,
    root_swap_probability =
      .rqr_validate_root_swap_probability(root_swap_probability),
    numerical_policy = .rqr_numerical_policy(numerical_policy),
    precision_beta = precision_beta,
    transition_version = .rqr_static_transition_version(),
    embedding_contract = embedding_contract,
    generalized_bayes = TRUE,
    response_likelihood = FALSE,
    response_prediction_contract = FALSE
  )
}

.rqr_static_full_latent <- function(value, data, placeholder) {
  placeholder <- .rqr_scalar_numeric(
    placeholder, "latent placeholder", lower = 0, lower_open = TRUE
  )
  if (is.null(value)) {
    out <- rep(placeholder, data$n_total)
    return(out)
  }
  value <- as.numeric(value)
  if (length(value) == data$n_observed) {
    out <- rep(placeholder, data$n_total)
    out[data$observed] <- value
  } else if (length(value) == data$n_total) {
    out <- value
    out[!data$observed] <- placeholder
  } else {
    stop(
      "Initial latent_v must have length nrow(X) or n_observed.",
      call. = FALSE
    )
  }
  if (any(!is.finite(out[data$observed])) ||
      any(out[data$observed] <= 0)) {
    stop("Observed-site latent values must be finite and positive.",
         call. = FALSE)
  }
  out
}
