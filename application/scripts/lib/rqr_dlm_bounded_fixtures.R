# Canonical construction for the bounded dynamic RQR-DLM fixtures.
#
# Preflight and the eventual bounded runner must both call these functions.
# This prevents a shallow configuration check from approving objects that the
# public model, evolution, missingness, or forecast interfaces would reject.

`%||%` <- function(x, y) if (is.null(x)) y else x

rqr_bounded_component <- function(spec, X_override = NULL) {
  if (!is.list(spec) || is.null(spec$type)) {
    stop("Each state component must be a typed list.", call. = FALSE)
  }
  type <- as.character(spec$type)[1L]
  if (identical(type, "polytrend")) {
    return(rqrgibbs::rqr_polytrend(
      order = spec$order,
      C0 = spec$C0,
      name = spec$name
    ))
  }
  if (identical(type, "seasonal")) {
    return(rqrgibbs::rqr_seasonal(
      period = spec$period,
      harmonics = spec$harmonics,
      C0 = spec$C0,
      name = spec$name
    ))
  }
  if (identical(type, "regression")) {
    X <- X_override %||% spec$X
    return(rqrgibbs::rqr_regression(
      X = X,
      C0 = spec$C0,
      name = spec$name
    ))
  }
  stop("Unsupported bounded-fixture component type: ", type, call. = FALSE)
}

rqr_bounded_model <- function(component_specs, future_X = NULL) {
  components <- lapply(component_specs, function(spec) {
    override <- if (identical(spec$type, "regression")) future_X else NULL
    rqr_bounded_component(spec, X_override = override)
  })
  Reduce(`+`, components)
}

rqr_build_bounded_dlm_fixture <- function(fixture, fixture_id) {
  if (!is.list(fixture) || !length(fixture$state_components)) {
    stop("Fixture ", fixture_id, " has no state components.", call. = FALSE)
  }
  T <- as.integer(fixture$n_time)
  H <- as.integer(fixture$future_horizon)
  if (length(T) != 1L || is.na(T) || T < 2L ||
      length(H) != 1L || is.na(H) || H < 1L) {
    stop("Fixture ", fixture_id, " has invalid time dimensions.", call. = FALSE)
  }
  model <- rqr_bounded_model(fixture$state_components)
  model <- rqrgibbs::rqr_as_dlm_model(model)
  expanded <- rqrgibbs:::.rqr_expand_model(model, T)
  y <- as.numeric(fixture$y)
  if (length(y) != T || any(!is.finite(y))) {
    stop("Fixture ", fixture_id, " has an invalid complete response.", call. = FALSE)
  }
  missing_indices <- as.integer(fixture$missing_indices %||% integer(0))
  if (anyNA(missing_indices) || any(missing_indices < 1L) ||
      any(missing_indices > T) || anyDuplicated(missing_indices)) {
    stop("Fixture ", fixture_id, " has invalid missing indices.", call. = FALSE)
  }
  y_with_missing <- y
  y_with_missing[missing_indices] <- NA_real_
  if (!any(!is.na(y_with_missing))) {
    stop("Fixture ", fixture_id, " has no observed response.", call. = FALSE)
  }

  future_specs <- fixture$state_components
  future_X <- fixture$future_X %||% NULL
  if (any(vapply(
        future_specs,
        function(spec) identical(spec$type, "regression"),
        logical(1L)
      )) && is.null(future_X)) {
    stop(
      "Fixture ", fixture_id,
      " must declare future_X for its regression component.",
      call. = FALSE
    )
  }
  future_model <- rqr_bounded_model(future_specs, future_X = future_X)
  future_expanded <- rqrgibbs:::.rqr_expand_model(future_model, H)
  if (!identical(expanded$p, future_expanded$p) ||
      !identical(expanded$component_dims, future_expanded$component_dims) ||
      !identical(expanded$component_names, future_expanded$component_names)) {
    stop(
      "Fixture ", fixture_id,
      " changes the state contract at the forecast boundary.",
      call. = FALSE
    )
  }

  mode <- as.character(fixture$evolution_mode)[1L]
  future_W <- NULL
  component_templates_future <- NULL
  extension_reproduces_training <- NA
  if (identical(mode, "fixed_W")) {
    evolution <- rqrgibbs::rqr_evolution_fixed(
      rqrgibbs:::.rqr_expand_cube(
        fixture$W, T, expanded$p, "fixture$W"
      )
    )
    future_W <- rqrgibbs:::.rqr_expand_cube(
      fixture$future_W, H, expanded$p, "fixture$future_W"
    )
  } else if (identical(mode, "discount_template")) {
    if (!identical(
          fixture$future_template_rule,
          "extend_reference_recursion_T_plus_H"
        )) {
      stop(
        "Fixture ", fixture_id,
        " must declare the T+H discount-template extension rule.",
        call. = FALSE
      )
    }
    training_template <- rqrgibbs::rqr_freeze_discount_template(
      model = model,
      n_time = T,
      df = fixture$df,
      dim.df = fixture$dim_df,
      reference_variance = fixture$reference_variance,
      numerical_policy = "fail"
    )
    extended_template <- rqrgibbs::rqr_freeze_discount_template(
      model = model,
      n_time = T + H,
      df = fixture$df,
      dim.df = fixture$dim_df,
      reference_variance = fixture$reference_variance,
      numerical_policy = "fail"
    )
    extension_reproduces_training <- identical(
      training_template$W,
      extended_template$W[, , seq_len(T), drop = FALSE]
    )
    if (!extension_reproduces_training) {
      stop(
        "Fixture ", fixture_id,
        " does not reproduce its frozen training recursion at T+H.",
        call. = FALSE
      )
    }
    evolution <- training_template
    future_W <- extended_template$W[
      , , T + seq_len(H), drop = FALSE
    ]
  } else if (identical(mode, "component_scale")) {
    evolution <- rqrgibbs::rqr_evolution_component_scale(
      templates = fixture$component_templates,
      component_dims = expanded$component_dims,
      prior = fixture$component_scale_prior,
      initial = fixture$component_scale_initial,
      component_names = expanded$component_names
    )
    component_templates_future <- fixture$component_templates_future
    if (is.null(component_templates_future)) {
      stop(
        "Fixture ", fixture_id,
        " must declare component_templates_future.",
        call. = FALSE
      )
    }
    rqrgibbs::rqr_evolution_component_scale(
      templates = component_templates_future,
      component_dims = expanded$component_dims,
      prior = fixture$component_scale_prior,
      initial = fixture$component_scale_initial,
      component_names = expanded$component_names
    )
  } else {
    stop("Fixture ", fixture_id, " has unsupported evolution mode.", call. = FALSE)
  }

  if (!inherits(evolution, "rqr_evolution") ||
      !isTRUE(evolution$exact_joint_target) ||
      identical(evolution$mode, "adaptive_discount")) {
    stop("Fixture ", fixture_id, " is not a fixed-joint target.", call. = FALSE)
  }
  if (is.null(component_templates_future)) {
    if (!all(dim(future_W) == c(expanded$p, expanded$p, H)) ||
        any(!is.finite(future_W))) {
      stop("Fixture ", fixture_id, " has invalid future W.", call. = FALSE)
    }
  }

  list(
    fixture_id = fixture_id,
    model = model,
    expanded_model = expanded,
    y_complete = y,
    y = y_with_missing,
    observed = !is.na(y_with_missing),
    evolution = evolution,
    future = list(
      H = H,
      FF = future_expanded$FF,
      GG = future_expanded$GG,
      W = future_W,
      component_templates = component_templates_future
    ),
    construction_audit = list(
      state_dimension = expanded$p,
      component_dims = expanded$component_dims,
      component_names = expanded$component_names,
      observed_count = sum(!is.na(y_with_missing)),
      missing_count = sum(is.na(y_with_missing)),
      training_horizon = T,
      future_horizon = H,
      evolution_mode = evolution$mode,
      exact_joint_target = evolution$exact_joint_target,
      training_evolution_slices = if (!is.null(evolution$W)) {
        dim(evolution$W)[3L]
      } else {
        max(vapply(
          evolution$templates,
          function(template) dim(template)[3L],
          integer(1L)
        ))
      },
      future_evolution_slices = if (!is.null(future_W)) {
        dim(future_W)[3L]
      } else {
        unique(vapply(
          component_templates_future,
          function(template) dim(template)[3L],
          integer(1L)
        ))
      },
      extension_reproduces_training = extension_reproduces_training,
      model_digest = digest::digest(
        unclass(model), algo = "sha256", serialize = TRUE
      ),
      evolution_digest = digest::digest(
        unclass(evolution), algo = "sha256", serialize = TRUE
      ),
      missing_response_digest = digest::digest(
        y_with_missing, algo = "sha256", serialize = TRUE
      ),
      future_digest = digest::digest(
        list(
          FF = future_expanded$FF,
          GG = future_expanded$GG,
          W = future_W,
          component_templates = component_templates_future
        ),
        algo = "sha256", serialize = TRUE
      )
    )
  )
}

rqr_build_all_bounded_dlm_fixtures <- function(config) {
  fixture_ids <- names(config$fixtures)
  out <- lapply(fixture_ids, function(fixture_id) {
    rqr_build_bounded_dlm_fixture(
      config$fixtures[[fixture_id]], fixture_id
    )
  })
  names(out) <- fixture_ids
  out
}
