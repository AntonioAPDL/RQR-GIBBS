# Native, serializable coefficient-prior contracts for ordinary RQR.
#
# These objects deliberately contain data only.  In particular, they do not
# retain constructor, update, or diagnostic closures from a reference package.
# The same specification is used for both raw roots, which is the exchangeable
# prior contract required by the global root-label swap.

.rqr_beta_prior_schema <- function() {
  "rqrgibbs_beta_prior/1.0.0"
}

.rqr_prior_state_schema <- function() {
  "rqrgibbs_beta_prior_state/1.0.0"
}

.rqr_prior_has_function <- function(x) {
  if (is.function(x)) return(TRUE)
  if (!is.list(x)) return(FALSE)
  any(vapply(x, .rqr_prior_has_function, logical(1L)))
}

.rqr_prior_assert_closure_free <- function(x, name = "prior") {
  if (exists(
      ".rqr_assert_data_only_contract",
      mode = "function", inherits = TRUE
    )) {
    .rqr_assert_data_only_contract(x, name)
    return(invisible(TRUE))
  }
  if (.rqr_prior_has_function(x)) {
    stop(sprintf("%s must be closure-free and serializable.", name), call. = FALSE)
  }
  invisible(TRUE)
}

.rqr_prior_scalar_positive <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) ||
      !is.finite(x) || x <= 0) {
    stop(sprintf("%s must be a finite positive scalar.", name), call. = FALSE)
  }
  as.numeric(x)
}

.rqr_prior_scalar_finite <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x)) {
    stop(sprintf("%s must be a finite scalar.", name), call. = FALSE)
  }
  as.numeric(x)
}

.rqr_prior_scalar_text <- function(x, name) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop(sprintf("%s must be one nonempty character string.", name), call. = FALSE)
  }
  x
}

.rqr_prior_complete_names <- function(x, expected_length, name) {
  if (!is.character(x) || length(x) != expected_length ||
      anyNA(x) || any(!nzchar(x)) || anyDuplicated(x)) {
    stop(
      sprintf(
        "%s must contain %d unique nonempty coefficient names.",
        name, expected_length
      ),
      call. = FALSE
    )
  }
  as.character(x)
}

.rqr_gaussian_coefficient_names <- function(
    matrix_value, mean_value, mean_supplied, matrix_name) {
  p <- nrow(matrix_value)
  matrix_dimnames <- dimnames(matrix_value)
  matrix_naming_present <- !is.null(matrix_dimnames) &&
    any(vapply(matrix_dimnames, Negate(is.null), logical(1L)))
  mean_naming_present <- mean_supplied && !is.null(names(mean_value))
  named_contract <- matrix_naming_present || mean_naming_present
  if (!named_contract) return(NULL)

  if (is.null(matrix_dimnames) ||
      length(matrix_dimnames) != 2L ||
      is.null(matrix_dimnames[[1L]]) ||
      is.null(matrix_dimnames[[2L]])) {
    stop(
      paste(
        matrix_name,
        "must have complete row and column coefficient names whenever",
        "any Gaussian-prior names are supplied."
      ),
      call. = FALSE
    )
  }
  row_names <- .rqr_prior_complete_names(
    matrix_dimnames[[1L]], p, sprintf("rownames(%s)", matrix_name)
  )
  column_names <- .rqr_prior_complete_names(
    matrix_dimnames[[2L]], p, sprintf("colnames(%s)", matrix_name)
  )
  if (!identical(row_names, column_names)) {
    stop(
      sprintf(
        "The row and column coefficient names of %s must be identical and in the same order.",
        matrix_name
      ),
      call. = FALSE
    )
  }
  if (mean_supplied) {
    if (is.null(names(mean_value))) {
      stop(
        paste(
          "gaussian$mean must have complete coefficient names whenever",
          "the Gaussian matrix is named."
        ),
        call. = FALSE
      )
    }
    mean_names <- .rqr_prior_complete_names(
      names(mean_value), p, "names(gaussian$mean)"
    )
    if (!identical(mean_names, row_names)) {
      stop(
        paste(
          "gaussian$mean and the Gaussian matrix must use identical",
          "coefficient names in the same order."
        ),
        call. = FALSE
      )
    }
  }
  row_names
}

.rqr_prior_spd_matrix <- function(
    x, name, tolerance = 100 * .Machine$double.eps) {
  if (!is.matrix(x) || !is.numeric(x)) {
    stop(sprintf("%s must be a finite numeric square matrix.", name),
         call. = FALSE)
  }
  x <- as.matrix(x)
  if (nrow(x) < 1L || nrow(x) != ncol(x) ||
      any(!is.finite(x))) {
    stop(sprintf("%s must be a finite numeric square matrix.", name), call. = FALSE)
  }
  scale <- max(abs(x))
  asymmetry <- max(abs(x - t(x)))
  materially_asymmetric <- if (scale > 0) {
    asymmetry / scale > tolerance
  } else {
    asymmetry > 0
  }
  if (materially_asymmetric) {
    stop(sprintf("%s must be symmetric.", name), call. = FALSE)
  }
  x <- 0.5 * x + 0.5 * t(x)
  factor <- tryCatch(chol(x), error = function(error) NULL)
  if (is.null(factor)) {
    stop(sprintf("%s must be positive definite.", name), call. = FALSE)
  }
  list(matrix = x, chol = factor)
}

#' Native coefficient priors for ordinary RQR
#'
#' Constructs a closure-free prior shared by the two exchangeable interval
#' roots. Ridge and full Gaussian priors are static. `"rhs_ns"` is the native
#' Nishimura--Suchard shrunken-shoulder hierarchy with Makalic--Schmidt
#' inverse-Gamma auxiliaries and requires an explicitly named intercept.
#'
#' @param type One of `"ridge"`, `"gaussian"`, or `"rhs_ns"`.
#' @param ridge Ridge controls, including positive `tau2`.
#' @param gaussian Full-Gaussian controls: `mean` and exactly one of
#'   `precision` or `covariance`. If either input is named, the matrix must
#'   have complete identical row and column names, a supplied mean must have
#'   the same names, and those names must exactly match the design columns.
#'   When every Gaussian input is unnamed, coefficients bind by position.
#' @param rhs_ns RHS-NS controls: `tau0`, `a_zeta`, `b_zeta`, optional
#'   `zeta2_fixed`, `intercept_name`, `intercept_mean`, and
#'   `intercept_precision`.
#' @return A closure-free `rqr_beta_prior` specification. The design dimension
#'   and RHS intercept position are bound when a fit validates the prior.
#' @export
rqr_beta_prior <- function(
    type = c("ridge", "gaussian", "rhs_ns"),
    ridge = list(), gaussian = list(), rhs_ns = list()) {
  .rqr_beta_prior_spec(
    type = type, ridge = ridge, gaussian = gaussian,
    rhs_ns = rhs_ns
  )
}

.rqr_beta_prior_spec <- function(
    type = c("ridge", "gaussian", "rhs_ns"),
    ridge = list(), gaussian = list(), rhs_ns = list()) {
  choices <- c("ridge", "gaussian", "rhs_ns")
  if (identical(type, choices)) type <- type[[1L]]
  if (!is.character(type) || length(type) != 1L ||
      is.na(type) || !nzchar(type)) {
    stop("type must be exactly one prior name.", call. = FALSE)
  }
  type <- match.arg(tolower(type), choices)
  if (!is.list(ridge) || !is.list(gaussian) || !is.list(rhs_ns)) {
    stop("ridge, gaussian, and rhs_ns controls must be lists.", call. = FALSE)
  }
  .rqr_validate_named_list_fields(ridge, "ridge", "tau2")
  .rqr_validate_named_list_fields(
    gaussian, "gaussian", c("mean", "precision", "covariance")
  )
  .rqr_validate_named_list_fields(
    rhs_ns, "rhs_ns",
    c(
      "tau0", "a_zeta", "b_zeta", "zeta2_fixed", "c2_fixed",
      "intercept_name", "intercept", "intercept_mean",
      "intercept_precision", "intercept_prec", "shrink_intercept"
    )
  )
  inactive <- switch(
    type,
    ridge = c(gaussian = length(gaussian), rhs_ns = length(rhs_ns)),
    gaussian = c(ridge = length(ridge), rhs_ns = length(rhs_ns)),
    rhs_ns = c(ridge = length(ridge), gaussian = length(gaussian))
  )
  if (any(inactive > 0L)) {
    stop(
      sprintf(
        "Controls for inactive prior types must be empty: %s.",
        paste(names(inactive)[inactive > 0L], collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (identical(type, "ridge")) {
    tau2 <- .rqr_prior_scalar_positive(
      ridge$tau2 %||% 1e4, "ridge$tau2"
    )
    prior <- list(
      schema_version = .rqr_beta_prior_schema(),
      type = "ridge",
      root_prior_contract = "shared_exchangeable",
      root_priors_exchangeable = TRUE,
      stateful = FALSE,
      dimension = NULL,
      hypers = list(tau2 = tau2),
      canonical = NULL,
      design_contract = NULL,
      implementation = "native"
    )
  } else if (identical(type, "gaussian")) {
    supplied <- c(
      precision = !is.null(gaussian$precision),
      covariance = !is.null(gaussian$covariance)
    )
    if (sum(supplied) != 1L) {
      stop(
        "gaussian must supply exactly one of precision or covariance.",
        call. = FALSE
      )
    }
    parameterization <- names(supplied)[supplied]
    raw_matrix <- gaussian[[parameterization]]
    checked <- .rqr_prior_spd_matrix(
      raw_matrix, sprintf("gaussian$%s", parameterization)
    )
    p <- nrow(checked$matrix)
    mean_supplied <- !is.null(gaussian$mean)
    mean <- gaussian$mean %||% rep(0, p)
    if (!is.numeric(mean) || length(mean) != p || any(!is.finite(mean))) {
      stop(
        "gaussian$mean must be a finite numeric vector matching the matrix dimension.",
        call. = FALSE
      )
    }
    coefficient_names <- .rqr_gaussian_coefficient_names(
      raw_matrix, mean, mean_supplied,
      sprintf("gaussian$%s", parameterization)
    )
    mean <- as.numeric(mean)
    precision <- if (identical(parameterization, "precision")) {
      checked$matrix
    } else {
      inverse <- chol2inv(checked$chol)
      0.5 * inverse + 0.5 * t(inverse)
    }
    precision_check <- .rqr_prior_spd_matrix(
      precision, "canonical Gaussian precision"
    )
    precision <- precision_check$matrix
    if (!is.null(coefficient_names)) {
      dimnames(precision) <- list(
        coefficient_names, coefficient_names
      )
    }
    information <- as.numeric(precision %*% mean)
    prior <- list(
      schema_version = .rqr_beta_prior_schema(),
      type = "gaussian",
      root_prior_contract = "shared_exchangeable",
      root_priors_exchangeable = TRUE,
      stateful = FALSE,
      dimension = p,
      coefficient_binding = if (is.null(coefficient_names)) {
        "position"
      } else {
        "exact_names"
      },
      coefficient_names = coefficient_names,
      hypers = list(
        mean = mean,
        input_parameterization = parameterization
      ),
      canonical = list(
        precision = precision,
        information = information
      ),
      design_contract = NULL,
      implementation = "native"
    )
  } else {
    prior <- .rqr_rhs_ns_prior_spec(rhs_ns)
  }

  .rqr_prior_assert_closure_free(prior)
  structure(prior, class = c("rqr_beta_prior", "list"))
}

.rqr_prior_validate <- function(
    prior, X, intercept_tolerance = 100 * .Machine$double.eps) {
  if (!is.list(prior) ||
      !identical(prior$schema_version, .rqr_beta_prior_schema()) ||
      !identical(prior$root_prior_contract, "shared_exchangeable") ||
      !isTRUE(prior$root_priors_exchangeable)) {
    stop(
      sprintf(
        "prior must use the shared exchangeable %s contract.",
        .rqr_beta_prior_schema()
      ),
      call. = FALSE
    )
  }
  .rqr_prior_assert_closure_free(prior)
  X <- as.matrix(X)
  if (!is.numeric(X) || nrow(X) < 1L || ncol(X) < 1L ||
      any(!is.finite(X))) {
    stop("X must be a nonempty finite numeric matrix.", call. = FALSE)
  }
  storage.mode(X) <- "double"
  p <- ncol(X)
  if (!is.character(prior$type) || length(prior$type) != 1L ||
      is.na(prior$type) || !nzchar(prior$type)) {
    stop("The native coefficient-prior type is malformed.",
         call. = FALSE)
  }
  type <- prior$type
  if (!type %in% c("ridge", "gaussian", "rhs_ns")) {
    stop("Unsupported native coefficient-prior type.", call. = FALSE)
  }
  expected_fields <- switch(
    type,
    ridge = c(
      "schema_version", "type", "root_prior_contract",
      "root_priors_exchangeable", "stateful", "dimension",
      "hypers", "canonical", "design_contract", "implementation"
    ),
    gaussian = c(
      "schema_version", "type", "root_prior_contract",
      "root_priors_exchangeable", "stateful", "dimension",
      "coefficient_binding", "coefficient_names", "hypers",
      "canonical", "design_contract", "implementation"
    ),
    rhs_ns = c(
      "schema_version", "type", "root_prior_contract",
      "root_priors_exchangeable", "stateful", "dimension",
      "hypers", "canonical", "design_contract", "implementation",
      "hierarchy", "stochastic_floor"
    )
  )
  expected_hypers <- switch(
    type,
    ridge = "tau2",
    gaussian = c("mean", "input_parameterization"),
    rhs_ns = c(
      "tau0", "a_zeta", "b_zeta", "zeta2_fixed",
      "intercept_name", "intercept_mean", "intercept_precision"
    )
  )
  expected_stateful <- identical(type, "rhs_ns")
  if (!identical(class(prior), c("rqr_beta_prior", "list")) ||
      !identical(
        names(attributes(prior)), c("names", "class")
      ) ||
      !identical(names(prior), expected_fields) ||
      anyDuplicated(names(prior)) ||
      !is.list(prior$hypers) || is.object(prior$hypers) ||
      !identical(names(prior$hypers), expected_hypers) ||
      anyDuplicated(names(prior$hypers)) ||
      !is.logical(prior$root_priors_exchangeable) ||
      length(prior$root_priors_exchangeable) != 1L ||
      !identical(prior$root_priors_exchangeable, TRUE) ||
      !is.logical(prior$stateful) ||
      length(prior$stateful) != 1L ||
      !identical(prior$stateful, expected_stateful) ||
      !identical(prior$implementation, "native")) {
    stop(
      paste(
        "The native coefficient prior does not have its exact",
        "type-specific field and semantic contract."
      ),
      call. = FALSE
    )
  }
  if (is.null(prior$dimension)) {
    if (identical(type, "gaussian")) {
      stop("A Gaussian prior must declare its dimension.",
           call. = FALSE)
    }
  } else if (!is.numeric(prior$dimension) ||
      length(prior$dimension) != 1L ||
      is.na(prior$dimension) || !is.finite(prior$dimension) ||
      prior$dimension != floor(prior$dimension) ||
      prior$dimension < 1L) {
    stop("The native coefficient-prior dimension is invalid.",
         call. = FALSE)
  }
  if (identical(type, "gaussian") &&
      !identical(as.integer(prior$dimension), as.integer(p))) {
    stop("Gaussian prior dimension does not match ncol(X).", call. = FALSE)
  }
  if (identical(type, "ridge")) {
    .rqr_prior_scalar_positive(
      prior$hypers$tau2, "prior$hypers$tau2"
    )
    if (!is.null(prior$canonical)) {
      stop("A ridge prior cannot carry a canonical matrix block.",
           call. = FALSE)
    }
  } else if (identical(type, "gaussian")) {
    if (!is.numeric(prior$hypers$mean) ||
        length(prior$hypers$mean) != p ||
        any(!is.finite(prior$hypers$mean)) ||
        !identical(
          prior$hypers$input_parameterization,
          match.arg(
            prior$hypers$input_parameterization,
            c("precision", "covariance")
          )
        ) ||
        !is.list(prior$canonical) ||
        !identical(
          names(prior$canonical),
          c("precision", "information")
        ) ||
        !is.numeric(prior$canonical$information) ||
        length(prior$canonical$information) != p ||
        any(!is.finite(prior$canonical$information))) {
      stop("The canonical Gaussian-prior block is malformed.",
           call. = FALSE)
    }
    precision <- .rqr_prior_spd_matrix(
      prior$canonical$precision,
      "prior$canonical$precision"
    )$matrix
    expected_information <- as.numeric(
      precision %*% prior$hypers$mean
    )
    if (!identical(
          as.numeric(prior$canonical$information),
          expected_information
        )) {
      stop(
        "The Gaussian prior information vector is not reconstructible.",
        call. = FALSE
      )
    }
  } else {
    if (!is.null(prior$canonical) ||
        !identical(
          prior$hierarchy,
          paste(
            "nishimura_suchard_fictitious_normal_shoulder",
            "with_makalic_schmidt_inverse_gamma_auxiliaries",
            sep = "_"
          )
        ) ||
        !is.null(prior$stochastic_floor)) {
      stop("The RHS-NS hierarchy contract is malformed.",
           call. = FALSE)
    }
  }
  if (identical(type, "gaussian")) {
    coefficient_names <- prior$coefficient_names
    expected_binding <- if (is.null(coefficient_names)) {
      "position"
    } else {
      "exact_names"
    }
    if (!identical(prior$coefficient_binding, expected_binding)) {
      stop("The Gaussian coefficient-binding contract is malformed.",
           call. = FALSE)
    }
    if (!is.null(coefficient_names)) {
      coefficient_names <- .rqr_prior_complete_names(
        coefficient_names, p, "Gaussian prior coefficient_names"
      )
      design_names <- colnames(X)
      if (is.null(design_names)) {
        stop(
          paste(
            "A name-bound Gaussian prior requires complete design-column",
            "names in the exact prior order."
          ),
          call. = FALSE
        )
      }
      design_names <- .rqr_prior_complete_names(
        design_names, p, "colnames(X)"
      )
      if (!identical(design_names, coefficient_names)) {
        stop(
          paste(
            "Name-bound Gaussian prior coefficients must exactly match",
            "colnames(X) in the same order."
          ),
          call. = FALSE
        )
      }
      canonical_names <- dimnames(prior$canonical$precision)
      if (is.null(canonical_names) ||
          length(canonical_names) != 2L ||
          !identical(canonical_names[[1L]], coefficient_names) ||
          !identical(canonical_names[[2L]], coefficient_names)) {
        stop(
          paste(
            "The canonical Gaussian precision must retain the exact",
            "coefficient-name contract."
          ),
          call. = FALSE
        )
      }
    }
  }

  bound <- prior
  bound$dimension <- p
  bound$design_contract <- list(
    dimension = p,
    column_names = colnames(X),
    intercept_name = NA_character_,
    intercept_index = NA_integer_,
    active_index = seq_len(p),
    coefficient_binding = if (identical(type, "gaussian")) {
      prior$coefficient_binding
    } else {
      "position"
    },
    coefficient_names = if (identical(type, "gaussian")) {
      prior$coefficient_names
    } else {
      NULL
    }
  )
  if (identical(type, "rhs_ns")) {
    intercept_name <- .rqr_prior_scalar_text(
      prior$hypers$intercept_name, "rhs_ns$intercept_name"
    )
    column_names <- colnames(X)
    if (is.null(column_names) || anyNA(column_names) ||
        any(!nzchar(column_names)) || anyDuplicated(column_names)) {
      stop(
        paste(
          "RHS-NS requires unique nonempty design-column names",
          "and an explicit intercept name."
        ),
        call. = FALSE
      )
    }
    intercept_index <- which(column_names == intercept_name)
    if (length(intercept_index) != 1L) {
      stop(
        "RHS-NS intercept_name must identify exactly one design column.",
        call. = FALSE
      )
    }
    deviation <- max(abs(X[, intercept_index] - 1))
    if (!is.finite(deviation) || deviation > intercept_tolerance) {
      stop(
        "The declared RHS-NS intercept column must be constant and equal to one.",
        call. = FALSE
      )
    }
    bound$design_contract$intercept_name <- intercept_name
    bound$design_contract$intercept_index <- as.integer(intercept_index)
    bound$design_contract$active_index <-
      setdiff(seq_len(p), intercept_index)
  }
  .rqr_prior_assert_closure_free(bound)
  structure(bound, class = c("rqr_beta_prior", "list"))
}

.rqr_prior_dimension <- function(prior, p = NULL) {
  dimension <- prior$dimension %||% p
  if (!is.numeric(dimension) || length(dimension) != 1L ||
      is.na(dimension) || !is.finite(dimension) ||
      dimension != floor(dimension) || dimension < 1L) {
    stop("The prior dimension is unresolved or invalid.", call. = FALSE)
  }
  dimension <- as.integer(dimension)
  if (!is.null(p) && !identical(dimension, as.integer(p))) {
    stop("Requested dimension does not match the prior contract.", call. = FALSE)
  }
  dimension
}

.rqr_prior_initialize <- function(prior, p = NULL, init = NULL) {
  .rqr_prior_assert_closure_free(prior)
  p <- .rqr_prior_dimension(prior, p)
  if (identical(prior$type, "rhs_ns")) {
    return(.rqr_rhs_ns_state_init(prior, init = init))
  }
  state <- list(
    schema_version = .rqr_prior_state_schema(),
    type = prior$type,
    dimension = p,
    stateful = FALSE
  )
  structure(state, class = c("rqr_beta_prior_state", "list"))
}

.rqr_prior_state_validate <- function(prior, state, p = NULL) {
  p <- .rqr_prior_dimension(prior, p)
  if (identical(prior$type, "rhs_ns")) {
    .rqr_rhs_ns_state_validate(prior, state)
    return(invisible(TRUE))
  }
  expected <- structure(
    list(
      schema_version = .rqr_prior_state_schema(),
      type = prior$type,
      dimension = p,
      stateful = FALSE
    ),
    class = c("rqr_beta_prior_state", "list")
  )
  if (!identical(state, expected)) {
    stop(
      paste(
        "The non-stateful coefficient-prior state is not in",
        "canonical form."
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.rqr_prior_canonical <- function(prior, state = NULL, p = NULL) {
  .rqr_prior_assert_closure_free(prior)
  p <- .rqr_prior_dimension(prior, p)
  if (is.null(state) && !identical(prior$type, "rhs_ns")) {
    state <- .rqr_prior_initialize(prior, p = p)
  }
  .rqr_prior_state_validate(prior, state, p = p)
  if (identical(prior$type, "ridge")) {
    precision <- diag(1 / prior$hypers$tau2, nrow = p, ncol = p)
    information <- rep(0, p)
  } else if (identical(prior$type, "gaussian")) {
    precision <- prior$canonical$precision
    information <- prior$canonical$information
  } else if (identical(prior$type, "rhs_ns")) {
    precision <- diag(
      .rqr_rhs_ns_precision(prior, state),
      nrow = p, ncol = p
    )
    information <- rep(0, p)
    intercept_index <- prior$design_contract$intercept_index
    information[intercept_index] <-
      prior$hypers$intercept_precision *
      prior$hypers$intercept_mean
  } else {
    stop("Unsupported native coefficient-prior type.", call. = FALSE)
  }
  if (!all(dim(precision) == c(p, p)) ||
      length(information) != p ||
      any(!is.finite(precision)) || any(!is.finite(information))) {
    stop("Native prior produced invalid Gaussian canonical parameters.",
         call. = FALSE)
  }
  .rqr_prior_spd_matrix(precision, "prior canonical precision")
  list(
    precision = precision,
    information = as.numeric(information),
    dimension = p,
    type = prior$type
  )
}

.rqr_prior_update <- function(
    prior, state, beta, numerical_policy = c("fail", "record_repair")) {
  numerical_policy <- .rqr_numerical_policy(numerical_policy)
  p <- .rqr_prior_dimension(prior)
  beta <- as.numeric(beta)
  if (length(beta) != p || any(!is.finite(beta))) {
    stop("beta must be finite and match the prior dimension.", call. = FALSE)
  }
  .rqr_prior_state_validate(prior, state, p = p)
  if (identical(prior$type, "rhs_ns")) {
    return(.rqr_rhs_ns_state_update(
      prior, state, beta, numerical_policy = numerical_policy
    ))
  }
  list(
    state = state,
    stats = .rqr_prior_diagnostics(prior, state),
    numerical_repair_count = 0L
  )
}

.rqr_prior_diagnostics <- function(prior, state = NULL) {
  p <- .rqr_prior_dimension(prior)
  .rqr_prior_state_validate(prior, state, p = p)
  if (identical(prior$type, "rhs_ns")) {
    return(.rqr_rhs_ns_diagnostics(prior, state))
  }
  list(
    schema_version = .rqr_prior_state_schema(),
    type = prior$type,
    dimension = p,
    stateful = FALSE,
    root_priors_exchangeable = TRUE,
    numerical_repair_count = 0L
  )
}

.rqr_prior_target_contract <- function(prior) {
  .rqr_prior_assert_closure_free(prior)
  p <- .rqr_prior_dimension(prior)
  if (identical(prior$type, "rhs_ns")) {
    return(.rqr_rhs_ns_target_contract(prior))
  }
  canonical <- .rqr_prior_canonical(prior, p = p)
  list(
    schema_version = prior$schema_version,
    type = prior$type,
    root_prior_contract = prior$root_prior_contract,
    root_priors_exchangeable = prior$root_priors_exchangeable,
    dimension = p,
    coefficient_binding = prior$coefficient_binding %||% "position",
    coefficient_names = prior$coefficient_names %||% NULL,
    canonical = list(
      precision = canonical$precision,
      information = canonical$information
    ),
    stateful = FALSE
  )
}

.rqr_beta_prior_coerce <- function(
    object, X = NULL, intercept_name = NULL) {
  if (!is.list(object)) {
    stop("A legacy coefficient prior must be a list.", call. = FALSE)
  }
  if (identical(object$schema_version, .rqr_beta_prior_schema())) {
    .rqr_prior_assert_closure_free(object)
    prior <- structure(object, class = c("rqr_beta_prior", "list"))
    if (!is.null(intercept_name)) {
      intercept_name <- .rqr_prior_scalar_text(
        intercept_name, "intercept_name"
      )
      if (!identical(prior$type, "rhs_ns") ||
          !identical(
            intercept_name, prior$hypers$intercept_name
          )) {
        stop(
          paste(
            "intercept_name may accompany a native prior only when",
            "it exactly matches that RHS-NS prior's intercept_name."
          ),
          call. = FALSE
        )
      }
    }
    if (!is.null(X)) prior <- .rqr_prior_validate(prior, X)
    return(prior)
  }

  raw_type <- object$type %||% ""
  if (!is.character(raw_type) || length(raw_type) != 1L ||
      is.na(raw_type) || !nzchar(raw_type)) {
    stop("Legacy prior type must be one nonempty string.",
         call. = FALSE)
  }
  type <- tolower(raw_type)
  hypers <- object$hypers
  if (!is.list(hypers)) hypers <- list()
  if (identical(type, "ridge")) {
    prior <- .rqr_beta_prior_spec(
      "ridge", ridge = list(tau2 = hypers$tau2 %||% object$tau2)
    )
  } else if (identical(type, "gaussian")) {
    prior <- .rqr_beta_prior_spec(
      "gaussian",
      gaussian = list(
        mean = hypers$mean %||% object$mean,
        precision = hypers$precision %||% object$precision,
        covariance = hypers$covariance %||% object$covariance
      )
    )
  } else if (identical(type, "rhs_ns")) {
    shrink_intercept <- hypers$shrink_intercept %||%
      object$shrink_intercept %||% FALSE
    if (!identical(shrink_intercept, FALSE)) {
      stop(
        "Legacy RHS-NS coercion requires shrink_intercept=FALSE.",
        call. = FALSE
      )
    }
    declared_intercept <- intercept_name %||%
      hypers$intercept_name %||% object$intercept_name
    zeta2_fixed <- hypers$zeta2_fixed %||%
      hypers$c2_fixed %||% object$zeta2_fixed
    if (length(zeta2_fixed) == 1L && is.na(zeta2_fixed)) {
      zeta2_fixed <- NULL
    }
    prior <- .rqr_beta_prior_spec(
      "rhs_ns",
      rhs_ns = list(
        tau0 = hypers$tau0 %||% object$tau0,
        a_zeta = hypers$a_zeta %||% object$a_zeta,
        b_zeta = hypers$b_zeta %||% object$b_zeta,
        zeta2_fixed = zeta2_fixed,
        intercept_name = declared_intercept,
        intercept_mean = hypers$intercept_mean %||%
          object$intercept_mean %||% 0,
        intercept_precision = hypers$intercept_precision %||%
          hypers$intercept_prec %||% object$intercept_precision %||%
          object$intercept_prec
      )
    )
  } else {
    stop(
      "Legacy prior coercion supports only ridge, gaussian, and rhs_ns.",
      call. = FALSE
    )
  }
  .rqr_prior_assert_closure_free(prior)
  if (!is.null(X)) prior <- .rqr_prior_validate(prior, X)
  prior
}
