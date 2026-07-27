# Exact component-scale evolution specifications for RQR-DLM.

.rqr_dlm_evolution_modes <- function() {
  c(
    "fixed_W", "discount_template", "component_scale",
    "adaptive_discount"
  )
}

.rqr_dlm_evolution_schema <- function() {
  "rqrgibbs_dlm_evolution/1.0.0"
}

.rqr_dlm_evolution_properties <- function(mode) {
  if (!is.character(mode) || length(mode) != 1L ||
      is.na(mode) || !mode %in% .rqr_dlm_evolution_modes()) {
    stop("The RQR-DLM evolution mode is missing or unsupported.",
         call. = FALSE)
  }
  fixed_joint <- mode %in% c(
    "fixed_W", "discount_template", "component_scale"
  )
  list(
    exact_joint_target = fixed_joint,
    ordinary_v1_evolution = fixed_joint,
    continuation_supported = fixed_joint,
    time0_state_completion = fixed_joint,
    frozen_before_mcmc = !identical(mode, "adaptive_discount"),
    working_sequential = identical(mode, "adaptive_discount")
  )
}

.rqr_dlm_evolution_field_schema <- function(mode) {
  switch(
    mode,
    fixed_W = c(
      "schema_version", "mode", "W", "exact_joint_target",
      "frozen_before_mcmc"
    ),
    discount_template = c(
      "schema_version", "mode", "W", "df", "dim.df", "D",
      "reference_variance", "reference_design",
      "reference_variance_source", "reference_design_source",
      "empirical_bayes", "exact_joint_target",
      "frozen_before_mcmc", "construction_contract",
      "construction_audit"
    ),
    component_scale = c(
      "schema_version", "mode", "templates", "component_dims",
      "component_names", "prior", "initial", "exact_joint_target",
      "frozen_before_mcmc", "shared_across_roots"
    ),
    adaptive_discount = c(
      "schema_version", "mode", "df", "dim.df", "D",
      "exact_joint_target", "frozen_before_mcmc",
      "working_sequential"
    ),
    stop("The RQR-DLM evolution mode is missing or unsupported.",
         call. = FALSE)
  )
}

.rqr_validate_discount_construction_audit <- function(
    audit, n_time) {
  expected_fields <- c(
    "numerical_policy", "repair_count", "repair_records",
    "repair_time", "repair_jitter", "minimum_eigenvalue"
  )
  if (!is.list(audit) || is.object(audit) ||
      !identical(names(audit), expected_fields) ||
      anyDuplicated(names(audit))) {
    stop(
      paste(
        "The discount-template construction audit does not have its",
        "exact field schema."
      ),
      call. = FALSE
    )
  }
  policy <- .rqr_numerical_policy(audit$numerical_policy)
  records <- audit$repair_records
  empty_schema <- .rqr_empty_repair_records()
  if (!is.data.frame(records) ||
      !identical(names(records), names(empty_schema)) ||
      anyDuplicated(names(records)) ||
      !identical(vapply(records, typeof, character(1L)),
                 vapply(empty_schema, typeof, character(1L)))) {
    stop(
      "The discount-template repair ledger has an invalid schema.",
      call. = FALSE
    )
  }
  repair_count <- .rqr_history_count(
    audit$repair_count, "construction_audit$repair_count"
  )
  if (repair_count != nrow(records) ||
      !identical(audit$repair_time, records$time) ||
      !identical(audit$repair_jitter, records$jitter) ||
      anyNA(records$stage) || any(!nzchar(records$stage)) ||
      anyNA(records$time) || any(records$time < 1L) ||
      any(records$time > n_time) || anyDuplicated(records$time) ||
      anyNA(records$strategy) || any(!nzchar(records$strategy)) ||
      any(!is.finite(records$jitter)) || any(records$jitter <= 0) ||
      anyNA(records$absolute_jitter_fallback) ||
      anyNA(records$clamped_eigenvalues) ||
      any(records$clamped_eigenvalues != 0L)) {
    stop(
      "The discount-template repair ledger and summaries disagree.",
      call. = FALSE
    )
  }
  if (!is.numeric(audit$minimum_eigenvalue) ||
      is.object(audit$minimum_eigenvalue) ||
      !is.null(dim(audit$minimum_eigenvalue)) ||
      length(audit$minimum_eigenvalue) != n_time ||
      any(!is.finite(audit$minimum_eigenvalue))) {
    stop(
      paste(
        "construction_audit$minimum_eigenvalue must contain one",
        "finite value per time point."
      ),
      call. = FALSE
    )
  }
  if (identical(policy, "fail") && repair_count != 0L) {
    stop(
      "A fail-policy discount template cannot report numerical repairs.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.rqr_validate_discount_template_reconstruction <- function(
    evolution, expanded) {
  p <- expanded$p
  n_time <- expanded$n_time
  identity <- diag(p)
  C <- expanded$C0
  records <- evolution$construction_audit$repair_records
  construction <- evolution$construction_contract
  expected_contract_fields <- c(
    "schema_version", "algorithm", "numerical_policy",
    "jitter_ladder", "reference_variance_source",
    "reference_design_source"
  )
  if (!is.list(construction) || is.object(construction) ||
      !identical(names(construction), expected_contract_fields) ||
      anyDuplicated(names(construction)) ||
      !identical(
        construction$schema_version,
        "rqrgibbs_discount_template_construction/1.0.0"
      ) ||
      !identical(
        construction$algorithm,
        "reference_kalman_joseph_covariance_recursion"
      ) ||
      !identical(
        construction$reference_variance_source,
        evolution$reference_variance_source
      ) ||
      !identical(
        construction$reference_design_source,
        evolution$reference_design_source
      )) {
    stop(
      "The discount-template construction contract is invalid.",
      call. = FALSE
    )
  }
  policy <- .rqr_numerical_policy(construction$numerical_policy)
  ladder <- .rqr_jitter_ladder(
    policy, construction$jitter_ladder
  )
  if (!identical(
        evolution$construction_audit$numerical_policy,
        policy
      )) {
    stop(
      "The discount-template construction policy metadata disagrees.",
      call. = FALSE
    )
  }
  reconstructed_records <- .rqr_empty_repair_records()
  expected_minimum <- numeric(n_time)
  tolerance <- 1000 * .Machine$double.eps
  for (tt in seq_len(n_time)) {
    P <- .rqr_symmetrize(
      expanded$GG[, , tt] %*% C %*% t(expanded$GG[, , tt])
    )
    expected_W <- .rqr_symmetrize(evolution$D * P)
    supplied_W <- matrix(
      evolution$W[, , tt], nrow = p, ncol = p
    )
    scale <- max(1, abs(expected_W), abs(supplied_W))
    if (max(abs(expected_W - supplied_W)) >
        tolerance * scale) {
      stop(
        paste(
          "The discount-template W cube is not generated by its",
          "declared frozen reference recursion."
        ),
        call. = FALSE
      )
    }
    R <- .rqr_symmetrize(P + expected_W)
    h <- evolution$reference_design[, tt]
    V <- evolution$reference_variance[[tt]]
    q <- drop(crossprod(h, R %*% h)) + V
    if (!is.finite(q) || q <= 0) {
      stop(
        "The declared discount-template recursion has nonpositive variance.",
        call. = FALSE
      )
    }
    gain <- drop(R %*% h) / q
    C <- .rqr_symmetrize(
      (identity - tcrossprod(gain, h)) %*% R %*%
        t(identity - tcrossprod(gain, h)) +
        tcrossprod(gain) * V
    )
    expected_minimum[[tt]] <- min(
      eigen(C, symmetric = TRUE, only.values = TRUE)$values
    )
    factorization <- .rqr_chol_with_jitter(C, ladder)
    reconstructed_records <- .rqr_add_repair_record(
      reconstructed_records,
      stage = "discount_template_filter_covariance",
      time = tt,
      info = list(
        strategy = "cholesky_jitter",
        jitter = factorization$jitter,
        relative_jitter = factorization$relative_jitter,
        min_eigenvalue = factorization$min_eigenvalue,
        matrix_scale = factorization$matrix_scale,
        jitter_scale = factorization$jitter_scale,
        absolute_jitter_fallback =
          factorization$absolute_jitter_fallback,
        clamped_eigenvalues = 0L
      )
    )
    C <- factorization$matrix
  }
  eigen_scale <- pmax(
    1, abs(expected_minimum),
    abs(evolution$construction_audit$minimum_eigenvalue)
  )
  if (any(abs(
      expected_minimum -
        evolution$construction_audit$minimum_eigenvalue
    ) > tolerance * eigen_scale)) {
    stop(
      paste(
        "The discount-template minimum-eigenvalue audit is not",
        "reconstructible from the declared recursion."
      ),
      call. = FALSE
    )
  }
  reconstructed_audit <- list(
    numerical_policy = policy,
    repair_count = nrow(reconstructed_records),
    repair_records = reconstructed_records,
    repair_time = reconstructed_records$time,
    repair_jitter = reconstructed_records$jitter,
    minimum_eigenvalue = expected_minimum
  )
  if (!identical(
        evolution$construction_audit,
        reconstructed_audit
      )) {
    stop(
      paste(
        "The discount-template construction audit is not the exact",
        "reconstruction of its declared algorithm and inputs."
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.rqr_validate_dlm_evolution_spec <- function(
    evolution, expanded, y = NULL) {
  if (!is.list(evolution) ||
      !identical(class(evolution), "rqr_evolution") ||
      !identical(
        names(attributes(evolution)), c("names", "class")
      )) {
    stop(
      "evolution_spec must have exactly class 'rqr_evolution'.",
      call. = FALSE
    )
  }
  mode <- evolution$mode
  properties <- .rqr_dlm_evolution_properties(mode)
  expected_fields <- .rqr_dlm_evolution_field_schema(mode)
  if (!identical(names(evolution), expected_fields) ||
      anyDuplicated(names(evolution)) ||
      !identical(
        evolution$schema_version, .rqr_dlm_evolution_schema()
      )) {
    stop(
      sprintf(
        "The %s evolution specification does not have its exact field schema.",
        mode
      ),
      call. = FALSE
    )
  }
  valid_flag <- function(value) {
    is.logical(value) && length(value) == 1L && !is.na(value)
  }
  if (!valid_flag(evolution$exact_joint_target) ||
      !identical(
        evolution$exact_joint_target,
        properties$exact_joint_target
      ) ||
      !valid_flag(evolution$frozen_before_mcmc) ||
      !identical(
        evolution$frozen_before_mcmc,
        properties$frozen_before_mcmc
      )) {
    stop(
      paste(
        "Evolution exactness and frozen-status metadata must be",
        "reconstructed from the canonical mode contract."
      ),
      call. = FALSE
    )
  }

  p <- expanded$p
  n_time <- expanded$n_time
  if (identical(mode, "fixed_W")) {
    W <- .rqr_expand_cube(evolution$W, n_time, p, "evolution_spec$W")
    .rqr_validate_covariance_cube(W, "evolution_spec$W")
  } else if (identical(mode, "discount_template")) {
    if (!is.array(evolution$W) ||
        !identical(dim(evolution$W), c(p, p, n_time))) {
      stop(
        "A discount-template W must be a p x p x n_time cube.",
        call. = FALSE
      )
    }
    .rqr_validate_covariance_cube(
      evolution$W, "evolution_spec$W"
    )
    dims <- .rqr_positive_integer_vector(
      evolution$dim.df, "evolution_spec$dim.df"
    )
    expected_D <- rqr_discount_matrix(
      evolution$df, dims, p
    )
    if (!identical(dims, expanded$component_dims) ||
        !is.matrix(evolution$D) ||
        !identical(dim(evolution$D), c(p, p)) ||
        !identical(as.numeric(evolution$D), as.numeric(expected_D))) {
      stop(
        paste(
          "The discount-template block dimensions or discount matrix",
          "do not match the expanded model."
        ),
        call. = FALSE
      )
    }
    if (!is.numeric(evolution$reference_variance) ||
        is.object(evolution$reference_variance) ||
        !is.null(dim(evolution$reference_variance)) ||
        length(evolution$reference_variance) != n_time ||
        any(!is.finite(evolution$reference_variance)) ||
        any(evolution$reference_variance <= 0) ||
        !is.matrix(evolution$reference_design) ||
        !is.numeric(evolution$reference_design) ||
        is.object(evolution$reference_design) ||
        !identical(
          dim(evolution$reference_design), c(p, n_time)
        ) ||
        any(!is.finite(evolution$reference_design))) {
      stop(
        "The discount-template reference recursion inputs are invalid.",
        call. = FALSE
      )
    }
    allowed_variance_sources <- c(
      "user_supplied", "training_response_variance"
    )
    allowed_design_sources <- c(
      "model_design", "user_supplied"
    )
    if (!is.character(evolution$reference_variance_source) ||
        length(evolution$reference_variance_source) != 1L ||
        is.na(evolution$reference_variance_source) ||
        !evolution$reference_variance_source %in%
          allowed_variance_sources ||
        !is.character(evolution$reference_design_source) ||
        length(evolution$reference_design_source) != 1L ||
        is.na(evolution$reference_design_source) ||
        !evolution$reference_design_source %in%
          allowed_design_sources ||
        !valid_flag(evolution$empirical_bayes) ||
        !identical(
          evolution$empirical_bayes,
          identical(
            evolution$reference_variance_source,
            "training_response_variance"
          )
        )) {
      stop(
        "The discount-template reference-source metadata is invalid.",
        call. = FALSE
      )
    }
    if (identical(
          evolution$reference_design_source, "model_design"
        ) &&
        !identical(
          evolution$reference_design, expanded$FF
        )) {
      stop(
        paste(
          "A model-design discount template must use the expanded",
          "model observation design exactly."
        ),
        call. = FALSE
      )
    }
    if (identical(
          evolution$reference_variance_source,
          "training_response_variance"
        )) {
      if (!is.numeric(y) || is.object(y) || !is.null(dim(y)) ||
          length(y) != n_time || !any(!is.na(y)) ||
          any(is.nan(y)) || any(is.infinite(y))) {
        stop(
          paste(
            "A training-response discount template requires the",
            "complete fitted response contract."
          ),
          call. = FALSE
        )
      }
      observed_y <- y[!is.na(y)]
      expected_variance <- stats::var(observed_y)
      if (!is.finite(expected_variance) ||
          expected_variance <= 0) {
        expected_variance <- 1
      }
      expected_variance <- max(
        expected_variance, sqrt(.Machine$double.eps)
      )
      if (!identical(
            evolution$reference_variance,
            rep(expected_variance, n_time)
          )) {
        stop(
          paste(
            "The training-response reference variance is not",
            "reconstructible from the fitted response."
          ),
          call. = FALSE
        )
      }
    }
    .rqr_validate_discount_construction_audit(
      evolution$construction_audit, n_time
    )
    .rqr_validate_discount_template_reconstruction(
      evolution, expanded
    )
  } else if (identical(mode, "component_scale")) {
    rebuilt <- rqr_evolution_component_scale(
      templates = evolution$templates,
      component_dims = evolution$component_dims,
      component_names = evolution$component_names,
      prior = evolution$prior,
      initial = evolution$initial
    )
    if (!identical(evolution, rebuilt) ||
        !identical(
          as.integer(evolution$component_dims),
          as.integer(expanded$component_dims)
        ) ||
        !identical(
          evolution$component_names,
          expanded$component_names
        )) {
      stop(
        paste(
          "The component-scale evolution specification is not its",
          "canonical reconstruction for the expanded model."
        ),
        call. = FALSE
      )
    }
    .rqr_expand_component_templates(evolution, n_time, p)
  } else {
    rebuilt <- rqr_evolution_adaptive_working(
      evolution$df, evolution$dim.df
    )
    if (!identical(evolution, rebuilt) ||
        !identical(
          as.integer(evolution$dim.df),
          as.integer(expanded$component_dims)
        )) {
      stop(
        paste(
          "The adaptive working evolution specification is not its",
          "canonical reconstruction for the expanded model."
        ),
        call. = FALSE
      )
    }
  }
  invisible(properties)
}

#' Construct a fixed-covariance RQR evolution specification
#'
#' `W` is fixed before MCMC. For a state of dimension `p`, it must be a finite
#' symmetric positive-semidefinite `p x p` matrix or a `p x p x T` cube. A
#' matrix is reused at every time; a cube must have one or exactly `T` slices.
#' Dimensions, symmetry, and positive-semidefinite validity are checked against
#' the model at fit time.
#'
#' @param W Evolution covariance matrix or time-varying covariance cube.
#' @return An exact fixed-prior `rqr_evolution` specification.
#' @examples
#' evolution <- rqr_evolution_fixed(diag(c(0.04, 0.01)))
#' @family RQR-DLM
#' @export
rqr_evolution_fixed <- function(W) {
  dW <- dim(W)
  if (!is.numeric(W) || is.object(W) ||
      !length(dW) %in% c(2L, 3L) ||
      dW[1L] != dW[2L] || dW[1L] < 1L ||
      (length(dW) == 3L && dW[3L] < 1L) ||
      any(!is.finite(W))) {
    stop(
      "W must be a plain finite numeric square covariance matrix or cube.",
      call. = FALSE
    )
  }
  W <- if (length(dW) == 2L) {
    .rqr_validate_covariance_cube(
      array(W, c(dW, 1L)), "W"
    )[, , 1L, drop = FALSE]
  } else {
    .rqr_validate_covariance_cube(W, "W")
  }
  if (length(dW) == 2L) W <- matrix(W[, , 1L], dW[1L], dW[2L])
  structure(list(
    schema_version = .rqr_dlm_evolution_schema(),
    mode = "fixed_W", W = W, exact_joint_target = TRUE,
    frozen_before_mcmc = TRUE
  ), class = "rqr_evolution")
}

#' Construct an adaptive working-discount evolution specification
#'
#' This constructor preserves the exdqlm component-discount matrix interface
#' while making the non-joint-target status explicit in its name and metadata.
#' The covariance recursion adapts within the scan; it is an experimental
#' working/sequential method, not an exact fixed-joint ordinary-v1 target and
#' not eligible for ordinary-v1 promotion.
#'
#' @param df Component discounts in `(0,1]`.
#' @param component_dims Positive state-block dimensions.
#' @return An experimental working/sequential `rqr_evolution` specification.
#' @examples
#' working <- rqr_evolution_adaptive_working(
#'   df = c(0.95, 0.90),
#'   component_dims = c(2L, 1L)
#' )
#' stopifnot(!working$exact_joint_target)
#' @family RQR-DLM
#' @export
rqr_evolution_adaptive_working <- function(df, component_dims) {
  component_dims <- .rqr_positive_integer_vector(component_dims, "component_dims")
  D <- rqr_discount_matrix(df, component_dims, sum(component_dims))
  structure(list(
    schema_version = .rqr_dlm_evolution_schema(),
    mode = "adaptive_discount",
    df = as.numeric(df),
    dim.df = component_dims,
    D = D,
    exact_joint_target = FALSE,
    frozen_before_mcmc = FALSE,
    working_sequential = TRUE
  ), class = "rqr_evolution")
}

.rqr_component_indices <- function(component_dims) {
  ends <- cumsum(component_dims)
  starts <- c(1L, utils::head(ends, -1L) + 1L)
  Map(seq.int, starts, ends)
}

.rqr_validate_spd_template <- function(x, d, name) {
  dx <- dim(x)
  if (!is.numeric(x) || is.object(x) || !length(dx) %in% c(2L, 3L)) {
    stop(
      sprintf("%s must be a plain numeric matrix or cube.", name),
      call. = FALSE
    )
  }
  if (length(dx) == 2L) x <- array(x, c(d, d, 1L))
  dx <- dim(x)
  if (length(dx) != 3L || !all(dx[1:2] == c(d, d)) || dx[3L] < 1L ||
      any(!is.finite(x))) {
    stop(sprintf("%s must be a finite %d x %d matrix or cube.", name, d, d), call. = FALSE)
  }
  for (tt in seq_len(dx[3L])) {
    x[, , tt] <- .rqr_validate_symmetric_matrix(
      x[, , tt], sprintf("%s slice %d", name, tt)
    )
    .rqr_chol_with_jitter(x[, , tt], jitter_ladder = 0)
  }
  x
}

#' Construct an exact component-scale evolution prior
#'
#' Defines `W_t = blockdiag(q_1 Q_1t, ..., q_J Q_Jt)` with fixed positive-
#' definite templates and shared inverse-Gamma component multipliers across the
#' two exchangeable roots. This is distinct from adaptive discount recursion.
#' Each component template is a symmetric positive-definite `d_j x d_j`
#' matrix or `d_j x d_j x T` cube. All nonconstant cubes must have the same
#' number of slices; one-slice templates are reused over time. Component order
#' is part of the transition contract.
#'
#' @param templates Plain list of plain numeric component covariance matrices
#'   or time-varying cubes.
#' @param component_dims Plain positive-integer component dimensions summing to
#'   the state size.
#' @param prior Fully named list with inverse-Gamma `shape` and `rate` entries,
#'   each a plain positive numeric scalar or one value per component. Legacy
#'   aliases `a` for `shape` and either `scale` or `b` for `rate` remain
#'   accepted, but ambiguous or unknown fields are rejected.
#' @param initial Plain positive numeric component multipliers, scalar or one
#'   per component.
#' @param component_names Optional plain character vector of unique nonempty
#'   component names.
#' @return An exact fixed-template `rqr_evolution` specification. The component
#'   multipliers are subsequently learned by [rqr_dlm_fit()].
#' @examples
#' evolution <- rqr_evolution_component_scale(
#'   templates = list(diag(2), matrix(1, 1, 1)),
#'   component_dims = c(2L, 1L),
#'   prior = list(shape = c(2, 3), rate = c(1, 1)),
#'   component_names = c("trend", "regression")
#' )
#' @family RQR-DLM
#' @export
rqr_evolution_component_scale <- function(
    templates, component_dims, prior = list(shape = 2, rate = 1),
    initial = 1, component_names = NULL) {
  if (!is.numeric(component_dims) || is.object(component_dims) ||
      !is.null(dim(component_dims))) {
    stop("component_dims must be a plain positive-integer vector.", call. = FALSE)
  }
  component_dims <- .rqr_positive_integer_vector(component_dims, "component_dims")
  J <- length(component_dims)
  if (!is.list(templates) || is.object(templates) ||
      length(templates) != J) {
    stop(
      "templates must be a plain list with one matrix or cube per component.",
      call. = FALSE
    )
  }
  templates <- lapply(seq_len(J), function(j) {
    .rqr_validate_spd_template(templates[[j]], component_dims[j], sprintf("templates[[%d]]", j))
  })
  template_times <- vapply(templates, function(x) dim(x)[3L], integer(1L))
  nonconstant <- unique(template_times[template_times > 1L])
  if (length(nonconstant) > 1L) {
    stop("Time-varying component templates must have a common number of slices.", call. = FALSE)
  }
  .rqr_validate_named_list_fields(
    prior, "prior", c("shape", "a", "rate", "scale", "b")
  )
  shape_fields <- intersect(names(prior), c("shape", "a"))
  rate_fields <- intersect(names(prior), c("rate", "scale", "b"))
  if (length(shape_fields) > 1L || length(rate_fields) > 1L) {
    stop(
      paste(
        "prior must supply at most one shape field ('shape' or 'a') and",
        "at most one rate field ('rate', 'scale', or 'b')."
      ),
      call. = FALSE
    )
  }
  shape <- if (length(shape_fields)) prior[[shape_fields]] else 2
  rate <- if (length(rate_fields)) prior[[rate_fields]] else 1
  if (!is.numeric(shape) || is.object(shape) || !is.null(dim(shape)) ||
      !is.numeric(rate) || is.object(rate) || !is.null(dim(rate))) {
    stop(
      paste(
        "Component-scale inverse-Gamma shape and rate must be plain",
        "numeric vectors."
      ),
      call. = FALSE
    )
  }
  shape <- as.numeric(shape)
  rate <- as.numeric(rate)
  if (!length(shape) %in% c(1L, J) || !length(rate) %in% c(1L, J)) {
    stop("Component-scale inverse-Gamma shape and rate must be scalar or length J.", call. = FALSE)
  }
  shape <- rep_len(shape, J)
  rate <- rep_len(rate, J)
  if (any(!is.finite(shape)) ||
      any(!is.finite(rate)) || any(shape <= 0) || any(rate <= 0)) {
    stop("Component-scale inverse-Gamma shape and rate must be positive.", call. = FALSE)
  }
  if (!is.numeric(initial) || is.object(initial) || !is.null(dim(initial))) {
    stop("initial must be a plain numeric vector.", call. = FALSE)
  }
  initial <- as.numeric(initial)
  if (!length(initial) %in% c(1L, J)) {
    stop("initial must be scalar or length J.", call. = FALSE)
  }
  initial <- rep_len(initial, J)
  if (any(!is.finite(initial)) || any(initial <= 0)) {
    stop("initial must contain positive component multipliers.", call. = FALSE)
  }
  if (is.null(component_names)) component_names <- paste0("component", seq_len(J))
  if (!is.character(component_names) || is.object(component_names) ||
      length(component_names) != J || anyNA(component_names) ||
      any(!nzchar(component_names)) ||
      anyDuplicated(component_names)) {
    stop(
      paste(
        "component_names must be a plain character vector of unique",
        "nonempty names matching component_dims."
      ),
      call. = FALSE
    )
  }
  structure(list(
    schema_version = .rqr_dlm_evolution_schema(),
    mode = "component_scale",
    templates = templates,
    component_dims = component_dims,
    component_names = component_names,
    prior = list(shape = shape, rate = rate),
    initial = initial,
    exact_joint_target = TRUE,
    frozen_before_mcmc = TRUE,
    shared_across_roots = TRUE
  ), class = "rqr_evolution")
}

.rqr_expand_component_templates <- function(evolution, n_time, p) {
  if (!inherits(evolution, "rqr_evolution") || !identical(evolution$mode, "component_scale")) {
    stop("Expected a component_scale rqr_evolution object.", call. = FALSE)
  }
  dims <- as.integer(evolution$component_dims)
  if (sum(dims) != p) stop("Component template dimensions do not match the state dimension.", call. = FALSE)
  lapply(seq_along(dims), function(j) {
    template <- evolution$templates[[j]]
    nt <- dim(template)[3L]
    if (nt == n_time) return(template)
    if (nt == 1L) return(array(rep(template[, , 1L], n_time), c(dims[j], dims[j], n_time)))
    stop(sprintf("Component template %d must have one or n_time slices.", j), call. = FALSE)
  })
}

.rqr_materialize_component_evolution <- function(evolution, q, n_time, p) {
  q <- as.numeric(q)
  dims <- as.integer(evolution$component_dims)
  if (length(q) != length(dims) || any(!is.finite(q)) || any(q <= 0)) {
    stop("Component evolution scales must be finite and positive.", call. = FALSE)
  }
  templates <- .rqr_expand_component_templates(evolution, n_time, p)
  indices <- .rqr_component_indices(dims)
  W <- array(0, c(p, p, n_time))
  for (tt in seq_len(n_time)) {
    for (j in seq_along(dims)) {
      W[indices[[j]], indices[[j]], tt] <- q[j] * templates[[j]][, , tt]
    }
  }
  structure(list(
    mode = "component_scale", W = W, exact_joint_target = TRUE,
    frozen_before_mcmc = FALSE, component_scales = q
  ), class = "rqr_evolution")
}

.rqr_component_W_from_expanded_templates <- function(
    templates, component_dims, q, n_time, p) {
  component_dims <- as.integer(component_dims)
  q <- as.numeric(q)
  if (length(templates) != length(component_dims) ||
      length(q) != length(component_dims) ||
      sum(component_dims) != p ||
      any(!is.finite(q)) || any(q <= 0)) {
    stop(
      "The expanded component-covariance inputs are invalid.",
      call. = FALSE
    )
  }
  indices <- .rqr_component_indices(component_dims)
  W <- array(0, c(p, p, n_time))
  for (j in seq_along(component_dims)) {
    W[indices[[j]], indices[[j]], ] <-
      q[[j]] * templates[[j]]
  }
  W
}

.rqr_draw_initial_state <- function(
    theta1, G1, m0, C0, W1,
    numerical_policy = c("fail", "record_repair"),
    jitter_ladder = c(0, 1e-12, 1e-10, 1e-8, 1e-6)) {
  theta1 <- as.numeric(theta1)
  m0 <- as.numeric(m0)
  p <- length(m0)
  G1 <- as.matrix(G1)
  C0 <- .rqr_validate_filter_covariance(C0, "C0")
  W1 <- .rqr_validate_filter_covariance(W1, "W1")
  numerical_policy <- .rqr_numerical_policy(numerical_policy)
  jitter_ladder <- .rqr_jitter_ladder(
    numerical_policy, jitter_ladder
  )
  repair_records <- .rqr_empty_repair_records()
  roundoff_psd_count <- 0L
  if (length(theta1) != p ||
      !identical(dim(G1), c(p, p)) ||
      !identical(dim(C0), c(p, p)) ||
      !identical(dim(W1), c(p, p)) ||
      any(!is.finite(c(theta1, G1, m0, C0, W1)))) {
    stop(
      "The time-zero conditional inputs have incompatible dimensions.",
      call. = FALSE
    )
  }
  forecast_covariance <- .rqr_symmetrize(
    G1 %*% C0 %*% t(G1) + W1
  )
  forecast_factor <- tryCatch(
    chol(forecast_covariance), error = function(error) NULL
  )
  forecast_info <- list(
    strategy = "cholesky", jitter = 0, relative_jitter = 0,
    min_eigenvalue = NA_real_, clamped_eigenvalues = 0L,
    matrix_scale = max(abs(forecast_covariance)),
    jitter_scale = max(abs(forecast_covariance)),
    absolute_jitter_fallback = FALSE
  )
  forecast_singular <- FALSE
  if (is.null(forecast_factor)) {
    forecast_eigen <- eigen(forecast_covariance, symmetric = TRUE)
    forecast_scale <- max(abs(forecast_eigen$values))
    negative <- forecast_eigen$values < 0
    near_psd <- forecast_scale == 0 ||
      min(forecast_eigen$values) / forecast_scale >=
        -100 * .Machine$double.eps
    if (any(negative) &&
        identical(numerical_policy, "fail") &&
        !near_psd) {
      stop(
        paste(
          "The time-zero forecast covariance has a negative eigenvalue,",
          "and projection is disabled under numerical_policy='fail'."
        ),
        call. = FALSE
      )
    }
    if (any(negative) && !near_psd) {
      repaired <- .rqr_chol_with_jitter(
        forecast_covariance, jitter_ladder
      )
      forecast_factor <- repaired$chol
      forecast_covariance <- repaired$matrix
      forecast_info <- list(
        strategy = "cholesky_jitter",
        jitter = repaired$jitter,
        relative_jitter = repaired$relative_jitter,
        min_eigenvalue = repaired$min_eigenvalue,
        clamped_eigenvalues = 0L,
        matrix_scale = repaired$matrix_scale,
        jitter_scale = repaired$jitter_scale,
        absolute_jitter_fallback =
          repaired$absolute_jitter_fallback
      )
      repair_records <- .rqr_add_repair_record(
        repair_records, "time_zero_forecast_covariance", 0L,
        forecast_info
      )
      solve_forecast <- function(value) {
        backsolve(
          forecast_factor,
          forwardsolve(t(forecast_factor), value)
        )
      }
    } else {
      values <- if (any(negative)) {
        pmax(forecast_eigen$values, 0)
      } else {
        forecast_eigen$values
      }
      forecast_singular <- any(values == 0)
      positive <- values > 0
      forecast_inverse <- if (any(positive)) {
        forecast_eigen$vectors[, positive, drop = FALSE] %*%
          (t(forecast_eigen$vectors[, positive, drop = FALSE]) /
            values[positive])
      } else {
        matrix(0, p, p)
      }
      if (any(negative)) {
        forecast_covariance <- .rqr_symmetrize(
          forecast_eigen$vectors %*%
            (values * t(forecast_eigen$vectors))
        )
      }
      forecast_info <- list(
        strategy = "psd_eigen",
        jitter = 0, relative_jitter = 0,
        min_eigenvalue = min(forecast_eigen$values),
        clamped_eigenvalues = 0L,
        matrix_scale = max(abs(forecast_covariance)),
        jitter_scale = max(abs(forecast_covariance)),
        absolute_jitter_fallback = FALSE
      )
      roundoff_psd_count <- roundoff_psd_count + sum(negative)
      repair_records <- .rqr_add_repair_record(
        repair_records, "time_zero_forecast_covariance", 0L,
        forecast_info
      )
      solve_forecast <- function(value) forecast_inverse %*% value
    }
  } else {
    solve_forecast <- function(value) {
      backsolve(
        forecast_factor,
        forwardsolve(t(forecast_factor), value)
      )
    }
  }
  cross_covariance <- C0 %*% t(G1)
  innovation <- theta1 - drop(G1 %*% m0)
  if (forecast_singular) {
    projected_innovation <- drop(
      forecast_covariance %*% solve_forecast(innovation)
    )
    range_residual <- innovation - projected_innovation
    residual_scale <- max(
      abs(innovation), sqrt(max(abs(forecast_covariance))),
      .Machine$double.xmin
    )
    support_residual_ratio <-
      max(abs(range_residual)) / residual_scale
    if (support_residual_ratio > 1e-8) {
      support_repair_limit <- max(
        1e-8, jitter_ladder
      )
      if (identical(numerical_policy, "fail") ||
          support_residual_ratio > support_repair_limit) {
        stop(
          "The time-one state is outside the singular forecast support.",
          call. = FALSE
        )
      }
      innovation <- projected_innovation
      support_info <- list(
        strategy = "support_projection",
        jitter = 0,
        relative_jitter = support_residual_ratio,
        min_eigenvalue = forecast_info$min_eigenvalue,
        clamped_eigenvalues = sum(
          abs(range_residual) >
            1e-8 * residual_scale
        ),
        matrix_scale = max(abs(forecast_covariance)),
        jitter_scale = residual_scale,
        absolute_jitter_fallback = FALSE
      )
      repair_records <- .rqr_add_repair_record(
        repair_records, "time_zero_forecast_support", 0L,
        support_info
      )
    }
  }
  conditional_gain <- cross_covariance %*%
    solve_forecast(diag(1, p))
  conditional_mean <- m0 + drop(conditional_gain %*% innovation)
  conditional_update <- diag(1, p) - conditional_gain %*% G1
  conditional_covariance <- .rqr_symmetrize(
    conditional_update %*% C0 %*% t(conditional_update) +
      conditional_gain %*% W1 %*% t(conditional_gain)
  )
  conditional_factor <- tryCatch(
    chol(conditional_covariance),
    error = function(error) NULL
  )
  conditional_info <- list(
    strategy = "cholesky", jitter = 0, relative_jitter = 0,
    min_eigenvalue = NA_real_, clamped_eigenvalues = 0L,
    matrix_scale = max(abs(conditional_covariance)),
    jitter_scale = max(abs(conditional_covariance)),
    absolute_jitter_fallback = FALSE
  )
  if (!is.null(conditional_factor)) {
    conditional_draw <- as.numeric(
      conditional_mean +
        t(conditional_factor) %*% stats::rnorm(p)
    )
  } else {
    conditional_eigen <- eigen(
      conditional_covariance, symmetric = TRUE
    )
    conditional_reference_scale <- max(
      abs(C0),
      abs(conditional_update %*% C0 %*% t(conditional_update)),
      abs(conditional_gain %*% W1 %*% t(conditional_gain)),
      abs(conditional_covariance)
    )
    conditional_negative <- conditional_eigen$values < 0
    conditional_near_psd <- conditional_reference_scale == 0 ||
      min(conditional_eigen$values) /
        conditional_reference_scale >=
          -100 * .Machine$double.eps
    if (any(conditional_negative) &&
        identical(numerical_policy, "fail") &&
        !conditional_near_psd) {
      stop(
        paste(
          "The time-zero conditional covariance has a negative",
          "eigenvalue, and projection is disabled under",
          "numerical_policy='fail'."
        ),
        call. = FALSE
      )
    }
    if (!any(conditional_negative) ||
        conditional_near_psd) {
      conditional_values <- if (any(conditional_negative)) {
        pmax(conditional_eigen$values, 0)
      } else {
        conditional_eigen$values
      }
      conditional_draw <- as.numeric(
        conditional_mean +
          conditional_eigen$vectors %*%
            (sqrt(conditional_values) * stats::rnorm(p))
      )
      conditional_info <- list(
        strategy = "psd_eigen",
        jitter = 0, relative_jitter = 0,
        min_eigenvalue = min(conditional_eigen$values),
        clamped_eigenvalues = 0L,
        matrix_scale = max(abs(conditional_covariance)),
        jitter_scale = conditional_reference_scale,
        absolute_jitter_fallback = FALSE
      )
      roundoff_psd_count <- roundoff_psd_count +
        sum(conditional_negative)
    } else {
      repaired_factor <- NULL
      applied_relative_jitter <- NA_real_
      applied_jitter <- NA_real_
      for (relative_jitter in jitter_ladder) {
        jitter <- relative_jitter *
          conditional_reference_scale
        if (relative_jitter > 0 && jitter == 0) {
          stop(
            paste(
              "Relative time-zero conditional-covariance jitter",
              "underflowed to zero at the reference scale."
            ),
            call. = FALSE
          )
        }
        candidate <- conditional_covariance +
          diag(jitter, p)
        candidate_factor <- tryCatch(
          chol(candidate), error = function(error) NULL
        )
        if (!is.null(candidate_factor)) {
          repaired_factor <- candidate_factor
          applied_relative_jitter <- relative_jitter
          applied_jitter <- jitter
          break
        }
      }
      if (is.null(repaired_factor)) {
        stop(
          paste(
            "The time-zero conditional covariance could not be",
            "repaired by the declared jitter ladder."
          ),
          call. = FALSE
        )
      }
      conditional_draw <- as.numeric(
        conditional_mean +
          t(repaired_factor) %*% stats::rnorm(p)
      )
      conditional_info <- list(
        strategy = "cholesky_jitter",
        jitter = applied_jitter,
        relative_jitter = applied_relative_jitter,
        min_eigenvalue = min(conditional_eigen$values),
        clamped_eigenvalues = 0L,
        matrix_scale = max(abs(conditional_covariance)),
        jitter_scale = conditional_reference_scale,
        absolute_jitter_fallback =
          conditional_reference_scale == 0 &&
            applied_jitter > 0
      )
    }
  }
  repair_records <- .rqr_add_repair_record(
    repair_records, "time_zero_conditional_covariance", 0L,
    conditional_info
  )
  numerical_exact <- nrow(repair_records) == 0L
  list(
    draw = as.numeric(conditional_draw),
    diagnostics = list(
      numerical_policy = numerical_policy,
      mathematically_exact_conditional = TRUE,
      numerically_exact = numerical_exact,
      exact_transition =
        isTRUE(numerical_exact),
      repair_count = nrow(repair_records),
      repair_records = repair_records,
      roundoff_psd_count = roundoff_psd_count,
      forecast_strategy = forecast_info$strategy,
      conditional_strategy = conditional_info$strategy
    )
  )
}

.rqr_component_scale_posterior <- function(
    theta1, theta2, theta01, theta02, GG, evolution) {
  theta1 <- as.matrix(theta1)
  theta2 <- as.matrix(theta2)
  p <- nrow(theta1)
  T <- ncol(theta1)
  if (!all(dim(theta2) == c(p, T))) stop("Root paths have incompatible dimensions.", call. = FALSE)
  GG <- .rqr_expand_cube(GG, T, p, "GG")
  templates <- .rqr_expand_component_templates(evolution, T, p)
  dims <- as.integer(evolution$component_dims)
  indices <- .rqr_component_indices(dims)
  shape <- evolution$prior$shape + T * dims
  rate <- as.numeric(evolution$prior$rate)
  theta0 <- list(as.numeric(theta01), as.numeric(theta02))
  paths <- list(theta1, theta2)
  for (k in 1:2) {
    previous <- theta0[[k]]
    for (tt in seq_len(T)) {
      innovation <- paths[[k]][, tt] - drop(GG[, , tt] %*% previous)
      for (j in seq_along(dims)) {
        d <- innovation[indices[[j]]]
        U <- chol(templates[[j]][, , tt])
        whitened <- forwardsolve(t(U), d)
        rate[j] <- rate[j] + 0.5 * sum(whitened^2)
      }
      previous <- paths[[k]][, tt]
    }
  }
  list(shape = shape, rate = rate)
}

.rqr_sample_component_scales <- function(theta1, theta2, theta01, theta02, GG, evolution) {
  posterior <- .rqr_component_scale_posterior(
    theta1, theta2, theta01, theta02, GG, evolution
  )
  list(
    draw = 1 / stats::rgamma(length(posterior$shape), posterior$shape, rate = posterior$rate),
    posterior = posterior
  )
}

.rqr_conditioned_component_scale_kernel <- function(
    theta, theta0, GG, evolution) {
  theta <- as.matrix(theta)
  p <- nrow(theta)
  T <- ncol(theta)
  theta0 <- as.numeric(theta0)
  if (length(theta0) != p ||
      any(!is.finite(c(theta, theta0)))) {
    stop(
      "The conditioned component-scale path inputs are invalid.",
      call. = FALSE
    )
  }
  GG <- .rqr_expand_cube(GG, T, p, "GG")
  templates <- .rqr_expand_component_templates(evolution, T, p)
  dims <- as.integer(evolution$component_dims)
  indices <- .rqr_component_indices(dims)
  rate_increment <- numeric(length(dims))
  previous <- theta0
  for (tt in seq_len(T)) {
    innovation <- theta[, tt] - drop(GG[, , tt] %*% previous)
    for (j in seq_along(dims)) {
      U <- chol(templates[[j]][, , tt])
      whitened <- forwardsolve(
        t(U), innovation[indices[[j]]]
      )
      rate_increment[[j]] <- rate_increment[[j]] +
        0.5 * sum(whitened^2)
    }
    previous <- theta[, tt]
  }
  list(
    log_scale_power = 0.5 * T * dims,
    rate_increment = rate_increment
  )
}

.rqr_collapsed_component_scale_log_density <- function(
    log_q, conditioned_kernel, z, H, obs_variance, GG, m0, C0,
    evolution, backend = c("cpp", "R", "auto"),
    expanded_templates = NULL) {
  log_q <- as.numeric(log_q)
  candidate_q <- exp(log_q)
  if (length(candidate_q) != length(evolution$component_dims) ||
      any(!is.finite(candidate_q)) || any(candidate_q <= 0)) {
    return(-Inf)
  }
  backend <- match.arg(backend)
  candidate_W <- if (is.null(expanded_templates)) {
    .rqr_materialize_component_evolution(
      evolution, candidate_q, length(z), length(m0)
    )$W
  } else {
    .rqr_component_W_from_expanded_templates(
      expanded_templates, evolution$component_dims, candidate_q,
      length(z), length(m0)
    )
  }
  log_marginal <- if (identical(backend, "cpp") &&
      !is.null(expanded_templates)) {
    as.numeric(rqr_filter_log_marginal_cpp(
      z, H, obs_variance, GG, m0, C0, candidate_W
    ))
  } else {
    .rqr_filter_log_marginal(
      z = z, H = H, V = obs_variance, GG = GG,
      m0 = m0, C0 = C0,
      evolution = structure(
        list(mode = "component_scale", W = candidate_W),
        class = "rqr_evolution"
      ),
      backend = backend
    )
  }
  if (length(log_marginal) != 1L ||
      !is.finite(log_marginal)) {
    stop(
      paste(
        "The collapsed component-scale filter returned a nonfinite",
        "log marginal at a representable scale."
      ),
      call. = FALSE
    )
  }
  sum(
    -(as.numeric(evolution$prior$shape) +
        conditioned_kernel$log_scale_power) * log_q -
      (as.numeric(evolution$prior$rate) +
        conditioned_kernel$rate_increment) / candidate_q
  ) + log_marginal
}

.rqr_collapsed_component_scale_update <- function(
    conditioned_theta, conditioned_theta0, z, H, obs_variance,
    GG, m0, C0, evolution, q, backend = c("cpp", "R", "auto"),
    width = 1, sweeps = 1L, max_steps = 100L,
    max_shrink = 1000L) {
  q <- as.numeric(q)
  log_q <- log(q)
  if (length(q) != length(evolution$component_dims) ||
      any(!is.finite(q)) || any(q <= 0)) {
    stop(
      "The collapsed component-scale values are invalid.",
      call. = FALSE
    )
  }
  backend <- match.arg(backend)
  conditioned_theta <- as.matrix(conditioned_theta)
  n_time <- ncol(conditioned_theta)
  p <- nrow(conditioned_theta)
  z <- as.numeric(z)
  H <- as.matrix(H)
  obs_variance <- as.numeric(obs_variance)
  GG <- .rqr_expand_cube(GG, n_time, p, "GG")
  m0 <- as.numeric(m0)
  C0 <- as.matrix(C0)
  if (length(z) != n_time ||
      !identical(dim(H), c(p, n_time)) ||
      length(obs_variance) != n_time ||
      length(m0) != p ||
      !identical(dim(C0), c(p, p)) ||
      any(is.nan(z)) || any(is.infinite(z)) ||
      any(!is.finite(c(H, obs_variance, GG, m0, C0))) ||
      any(obs_variance <= 0)) {
    stop(
      "The collapsed component-scale filter inputs are invalid.",
      call. = FALSE
    )
  }
  expanded_templates <- .rqr_expand_component_templates(
    evolution, n_time, p
  )
  conditioned_kernel <- .rqr_conditioned_component_scale_kernel(
    conditioned_theta, conditioned_theta0, GG, evolution
  )
  evaluate <- function(candidate_log_q) {
    .rqr_collapsed_component_scale_log_density(
      candidate_log_q, conditioned_kernel, z, H, obs_variance,
      GG, m0, C0, evolution, backend, expanded_templates
    )
  }
  sweeps <- .rqr_scalar_integer(
    sweeps, "collapsed component-scale slice sweeps", 1L
  )
  evaluation_count <- shrink_count <- integer(length(q))
  for (sweep in seq_len(sweeps)) {
    for (j in seq_along(q)) {
      coordinate_density <- function(value) {
        candidate <- log_q
        candidate[[j]] <- value
        evaluate(candidate)
      }
      update <- .rqr_slice_log_coordinate(
        log_q[[j]], coordinate_density, width = width,
        max_steps = max_steps, max_shrink = max_shrink
      )
      log_q[[j]] <- update$value
      evaluation_count[[j]] <-
        evaluation_count[[j]] + update$evaluations
      shrink_count[[j]] <-
        shrink_count[[j]] + update$shrink_steps
    }
  }
  numerical_repair_records <- .rqr_empty_repair_records()
  mathematically_exact <- isTRUE(evolution$exact_joint_target)
  numerically_exact <- nrow(numerical_repair_records) == 0L
  list(
    q = exp(log_q),
    diagnostics = list(
      evaluations = evaluation_count,
      shrink_steps = shrink_count,
      sweeps = sweeps,
      integrated_root_path = TRUE,
      conditioned_root_path = TRUE,
      mathematically_exact_partially_collapsed =
        mathematically_exact,
      numerically_exact_partially_collapsed =
        numerically_exact,
      numerical_repair_count =
        nrow(numerical_repair_records),
      numerical_repair_records =
        numerical_repair_records,
      exact_partially_collapsed =
        mathematically_exact && numerically_exact
    ),
    conditioned_kernel = conditioned_kernel
  )
}

.rqr_component_noncentered_innovations <- function(
    theta, theta0, GG, evolution, q) {
  theta <- as.matrix(theta)
  p <- nrow(theta)
  T <- ncol(theta)
  theta0 <- as.numeric(theta0)
  q <- as.numeric(q)
  dims <- as.integer(evolution$component_dims)
  if (length(theta0) != p || sum(dims) != p ||
      length(q) != length(dims) ||
      any(!is.finite(c(theta, theta0, q))) || any(q <= 0)) {
    stop("The noncentered component-scale inputs are invalid.",
         call. = FALSE)
  }
  GG <- .rqr_expand_cube(GG, T, p, "GG")
  indices <- .rqr_component_indices(dims)
  standardized <- matrix(NA_real_, p, T)
  previous <- theta0
  for (tt in seq_len(T)) {
    innovation <- theta[, tt] - drop(GG[, , tt] %*% previous)
    for (j in seq_along(dims)) {
      standardized[indices[[j]], tt] <-
        innovation[indices[[j]]] / sqrt(q[[j]])
    }
    previous <- theta[, tt]
  }
  if (any(!is.finite(standardized))) {
    stop("The standardized component innovations are nonfinite.",
         call. = FALSE)
  }
  standardized
}

.rqr_reconstruct_component_path <- function(
    standardized, theta0, GG, evolution, q) {
  standardized <- as.matrix(standardized)
  p <- nrow(standardized)
  T <- ncol(standardized)
  theta0 <- as.numeric(theta0)
  q <- as.numeric(q)
  dims <- as.integer(evolution$component_dims)
  if (length(theta0) != p || sum(dims) != p ||
      length(q) != length(dims) ||
      any(!is.finite(c(standardized, theta0, q))) || any(q <= 0)) {
    stop("The component-path reconstruction inputs are invalid.",
         call. = FALSE)
  }
  GG <- .rqr_expand_cube(GG, T, p, "GG")
  indices <- .rqr_component_indices(dims)
  theta <- matrix(NA_real_, p, T)
  previous <- theta0
  for (tt in seq_len(T)) {
    innovation <- numeric(p)
    for (j in seq_along(dims)) {
      innovation[indices[[j]]] <-
        sqrt(q[[j]]) * standardized[indices[[j]], tt]
    }
    theta[, tt] <- drop(GG[, , tt] %*% previous) + innovation
    previous <- theta[, tt]
  }
  if (any(!is.finite(theta))) {
    stop("The reconstructed component path is nonfinite.",
         call. = FALSE)
  }
  theta
}

.rqr_component_path_basis <- function(
    standardized, theta0, GG, evolution) {
  standardized <- as.matrix(standardized)
  p <- nrow(standardized)
  T <- ncol(standardized)
  theta0 <- as.numeric(theta0)
  dims <- as.integer(evolution$component_dims)
  if (length(theta0) != p || sum(dims) != p ||
      any(!is.finite(c(standardized, theta0)))) {
    stop("The component path-basis inputs are invalid.", call. = FALSE)
  }
  GG <- .rqr_expand_cube(GG, T, p, "GG")
  indices <- .rqr_component_indices(dims)
  baseline <- matrix(NA_real_, p, T)
  basis <- array(0, c(p, T, length(dims)))
  previous_baseline <- theta0
  previous_basis <- matrix(0, p, length(dims))
  for (tt in seq_len(T)) {
    baseline[, tt] <- drop(GG[, , tt] %*% previous_baseline)
    for (j in seq_along(dims)) {
      current <- drop(GG[, , tt] %*% previous_basis[, j])
      current[indices[[j]]] <-
        current[indices[[j]]] + standardized[indices[[j]], tt]
      basis[, tt, j] <- current
      previous_basis[, j] <- current
    }
    previous_baseline <- baseline[, tt]
  }
  if (any(!is.finite(c(baseline, basis)))) {
    stop("The component path basis is nonfinite.", call. = FALSE)
  }
  list(baseline = baseline, basis = basis)
}

.rqr_component_path_from_basis <- function(path_basis, q) {
  q <- as.numeric(q)
  dimensions <- dim(path_basis$basis)
  if (!is.matrix(path_basis$baseline) ||
      length(dimensions) != 3L ||
      !identical(
        dim(path_basis$baseline),
        dimensions[1:2]
      ) ||
      length(q) != dimensions[[3L]] ||
      any(!is.finite(c(path_basis$baseline, path_basis$basis, q))) ||
      any(q <= 0)) {
    stop("The component path-basis reconstruction is invalid.",
         call. = FALSE)
  }
  output <- path_basis$baseline
  for (j in seq_along(q)) {
    output <- output + sqrt(q[[j]]) * path_basis$basis[, , j]
  }
  output
}

.rqr_component_ordinate_basis <- function(FF, path_basis) {
  FF <- as.matrix(FF)
  dimensions <- dim(path_basis$basis)
  if (!is.matrix(path_basis$baseline) ||
      length(dimensions) != 3L ||
      !identical(dim(FF), dim(path_basis$baseline)) ||
      !identical(dim(path_basis$baseline), dimensions[1:2]) ||
      any(!is.finite(c(FF, path_basis$baseline, path_basis$basis)))) {
    stop("The component ordinate-basis inputs are invalid.",
         call. = FALSE)
  }
  component_basis <- vapply(
    seq_len(dimensions[[3L]]),
    function(j) colSums(FF * path_basis$basis[, , j]),
    numeric(ncol(FF))
  )
  if (dimensions[[3L]] == 1L) {
    component_basis <- matrix(component_basis, ncol = 1L)
  }
  list(
    baseline = colSums(FF * path_basis$baseline),
    basis = component_basis
  )
}

.rqr_slice_log_coordinate <- function(
    current, log_density, width = 1, max_steps = 100L,
    max_shrink = 1000L) {
  current <- as.numeric(current)[1L]
  width <- as.numeric(width)[1L]
  max_steps <- .rqr_scalar_integer(
    max_steps, "component-scale slice max_steps", 1L
  )
  max_shrink <- .rqr_scalar_integer(
    max_shrink, "component-scale slice max_shrink", 1L
  )
  if (!is.finite(current) || !is.finite(width) || width <= 0) {
    stop("The component-scale slice inputs are invalid.", call. = FALSE)
  }
  evaluations <- 0L
  evaluate <- function(value) {
    evaluations <<- evaluations + 1L
    result <- as.numeric(log_density(value))[1L]
    if (is.na(result) || result == Inf) {
      stop("The component-scale slice log density is invalid.",
           call. = FALSE)
    }
    result
  }
  current_density <- evaluate(current)
  if (!is.finite(current_density)) {
    stop("The current component-scale slice density is nonfinite.",
         call. = FALSE)
  }
  slice_height <- current_density - stats::rexp(1L)
  offset <- stats::runif(1L)
  left <- current - width * offset
  right <- left + width
  remaining_left <- floor(max_steps * stats::runif(1L))
  remaining_right <- max_steps - 1L - remaining_left
  left_density <- evaluate(left)
  while (remaining_left > 0L && left_density > slice_height) {
    left <- left - width
    remaining_left <- remaining_left - 1L
    left_density <- evaluate(left)
  }
  right_density <- evaluate(right)
  while (remaining_right > 0L && right_density > slice_height) {
    right <- right + width
    remaining_right <- remaining_right - 1L
    right_density <- evaluate(right)
  }
  for (attempt in seq_len(max_shrink)) {
    proposal <- stats::runif(1L, left, right)
    proposal_density <- evaluate(proposal)
    if (proposal_density >= slice_height) {
      return(list(
        value = proposal,
        evaluations = evaluations,
        shrink_steps = attempt - 1L
      ))
    }
    if (proposal < current) {
      left <- proposal
    } else {
      right <- proposal
    }
  }
  stop("The component-scale slice shrink limit was reached.",
       call. = FALSE)
}

.rqr_component_noncentered_log_density <- function(
    log_q, ordinate_basis1, ordinate_basis2, y, observed, v, xi,
    obs_variance, evolution) {
  log_q <- as.numeric(log_q)
  candidate_q <- exp(log_q)
  if (length(log_q) != length(evolution$component_dims) ||
      any(!is.finite(candidate_q)) || any(candidate_q <= 0)) {
    return(-Inf)
  }
  eta1 <- as.numeric(
    ordinate_basis1$baseline +
      ordinate_basis1$basis %*% sqrt(candidate_q)
  )
  eta2 <- as.numeric(
    ordinate_basis2$baseline +
      ordinate_basis2$basis %*% sqrt(candidate_q)
  )
  augmented_residual <- rqr_residual_product(
    y[observed], eta1[observed], eta2[observed]
  ) - xi * v[observed]
  scaled_square <- augmented_residual^2 / obs_variance[observed]
  if (any(!is.finite(scaled_square))) {
    return(-Inf)
  }
  sum(
    -evolution$prior$shape * log_q -
      evolution$prior$rate / candidate_q
  ) - 0.5 * sum(scaled_square)
}

.rqr_interweave_component_scales <- function(
    theta1, theta2, theta01, theta02, GG, FF, y, observed, v,
    xi, obs_variance, evolution, q, width = 1, sweeps = 1L,
    max_steps = 100L, max_shrink = 1000L) {
  q <- as.numeric(q)
  log_q <- log(q)
  path_basis1 <- rqr_noncentered_basis_cpp(
    as.matrix(theta1), as.numeric(theta01),
    .rqr_expand_cube(GG, ncol(theta1), nrow(theta1), "GG"),
    as.integer(evolution$component_dims), q
  )
  path_basis2 <- rqr_noncentered_basis_cpp(
    as.matrix(theta2), as.numeric(theta02),
    .rqr_expand_cube(GG, ncol(theta2), nrow(theta2), "GG"),
    as.integer(evolution$component_dims), q
  )
  ordinate_basis1 <- .rqr_component_ordinate_basis(FF, path_basis1)
  ordinate_basis2 <- .rqr_component_ordinate_basis(FF, path_basis2)
  standardized1 <- path_basis1$standardized
  standardized2 <- path_basis2$standardized
  y <- as.numeric(y)
  observed <- as.logical(observed)
  v <- as.numeric(v)
  obs_variance <- as.numeric(obs_variance)
  if (length(y) != ncol(theta1) || length(observed) != length(y) ||
      length(v) != length(y) || length(obs_variance) != length(y) ||
      anyNA(observed) || any(!is.finite(y[observed])) ||
      any(!is.finite(c(v, obs_variance))) ||
      any(v <= 0) || any(obs_variance <= 0) ||
      !is.finite(xi)) {
    stop("The component-scale interweaving inputs are invalid.",
         call. = FALSE)
  }
  evaluate <- function(candidate_log_q) {
    .rqr_component_noncentered_log_density(
      candidate_log_q, ordinate_basis1, ordinate_basis2, y, observed,
      v, xi, obs_variance, evolution
    )
  }
  sweeps <- .rqr_scalar_integer(
    sweeps, "component-scale slice sweeps", 1L
  )
  evaluation_count <- shrink_count <- integer(length(q))
  for (sweep in seq_len(sweeps)) {
    for (j in seq_along(q)) {
      coordinate_density <- function(value) {
        candidate <- log_q
        candidate[[j]] <- value
        evaluate(candidate)
      }
      update <- .rqr_slice_log_coordinate(
        log_q[[j]], coordinate_density, width = width,
        max_steps = max_steps, max_shrink = max_shrink
      )
      log_q[[j]] <- update$value
      evaluation_count[[j]] <-
        evaluation_count[[j]] + update$evaluations
      shrink_count[[j]] <-
        shrink_count[[j]] + update$shrink_steps
    }
  }
  q <- exp(log_q)
  list(
    q = q,
    theta1 = .rqr_component_path_from_basis(path_basis1, q),
    theta2 = .rqr_component_path_from_basis(path_basis2, q),
    diagnostics = list(
      evaluations = evaluation_count,
      shrink_steps = shrink_count,
      sweeps = sweeps,
      exact_noncentered_slice = TRUE
    )
  )
}
