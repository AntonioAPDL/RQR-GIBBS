.rqr_state_ordinates <- function(FF, path) colSums(FF * path)

.rqr_dlm_draws_schema <- function() {
  "rqrgibbs_dlm_draws/1.0.0"
}

.rqr_dlm_draw_source_schema <- function() {
  "rqrgibbs_dlm_draw_source/1.0.0"
}

.rqr_dlm_rng_binding_schema <- function() {
  "rqrgibbs_dlm_rng_binding/1.0.0"
}

.rqr_dlm_prediction_schema <- function() {
  "rqrgibbs_dlm_prediction/1.0.0"
}

.rqr_dlm_forecast_schema <- function() {
  "rqrgibbs_dlm_forecast/1.0.0"
}

.rqr_dlm_forecast_source_schema <- function() {
  "rqrgibbs_dlm_forecast_source/1.0.0"
}

.rqr_dlm_future_contract_schema <- function() {
  "rqrgibbs_dlm_future_contract/1.0.0"
}

.rqr_dlm_assert_exact_list_object <- function(
    object, expected_class, name) {
  if (!is.list(object) ||
      !identical(class(object), expected_class) ||
      !identical(names(attributes(object)), c("names", "class"))) {
    stop(
      sprintf(
        "%s must have the exact canonical class and attributes.",
        name
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.rqr_dlm_is_sha256 <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) &&
    grepl("^[0-9a-f]{64}$", tolower(x))
}

.rqr_dlm_preserve_rng <- function(callback) {
  if (!is.function(callback)) {
    stop("callback must be a function.", call. = FALSE)
  }
  existed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  previous <- if (existed) {
    get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  } else {
    NULL
  }
  on.exit({
    if (existed) {
      assign(".Random.seed", previous, envir = .GlobalEnv)
    } else if (exists(
      ".Random.seed", envir = .GlobalEnv, inherits = FALSE
    )) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  callback()
}

.rqr_dlm_rng_binding <- function(
    operation, mode, seed = NA_integer_,
    rng_state_before = NULL, rng_state_after = NULL,
    selection_mode = if (identical(mode, "external_unbound")) {
      "external"
    } else if (identical(mode, "none")) {
      "all"
    } else {
      "subsample"
    },
    requested_draw_count = NA_integer_,
    sampling_with_replacement = NA) {
  if (!is.character(operation) || length(operation) != 1L ||
      is.na(operation) || !nzchar(operation)) {
    stop("RNG-binding operation must be one nonempty string.",
         call. = FALSE)
  }
  if (!is.character(mode) || length(mode) != 1L ||
      is.na(mode) ||
      !mode %in% c(
        "none", "explicit_seed", "ambient_rng",
        "external_unbound"
      )) {
    stop("RNG-binding mode is invalid.", call. = FALSE)
  }
  seed <- if (identical(mode, "explicit_seed")) {
    .rqr_scalar_integer(seed, "seed", 0L)
  } else {
    NA_integer_
  }
  rng_state_before <- .rqr_canonical_rng_state(rng_state_before)
  rng_state_after <- .rqr_canonical_rng_state(rng_state_after)
  if (!is.character(selection_mode) ||
      length(selection_mode) != 1L ||
      is.na(selection_mode) ||
      !selection_mode %in% c("all", "subsample", "external")) {
    stop("RNG-binding draw-selection mode is invalid.",
         call. = FALSE)
  }
  requested_draw_count <- if (identical(
    selection_mode, "subsample"
  )) {
    .rqr_scalar_integer(
      requested_draw_count, "requested_draw_count", 1L
    )
  } else {
    NA_integer_
  }
  sampling_with_replacement <- if (identical(
    selection_mode, "subsample"
  )) {
    .rqr_scalar_logical(
      sampling_with_replacement,
      "sampling_with_replacement"
    )
  } else {
    NA
  }
  contract <- list(
    schema_version = .rqr_dlm_rng_binding_schema(),
    operation = operation,
    mode = mode,
    seed = seed,
    selection_mode = selection_mode,
    requested_draw_count = requested_draw_count,
    sampling_with_replacement =
      sampling_with_replacement,
    rng_state_before = rng_state_before,
    rng_state_after = rng_state_after,
    reproducibility_bound =
      mode %in% c("none", "explicit_seed")
  )
  .rqr_validate_dlm_rng_binding(contract, operation = operation)
  contract
}

.rqr_validate_dlm_rng_binding <- function(
    contract, operation = NULL) {
  expected <- c(
    "schema_version", "operation", "mode", "seed",
    "selection_mode", "requested_draw_count",
    "sampling_with_replacement",
    "rng_state_before", "rng_state_after",
    "reproducibility_bound"
  )
  if (!is.list(contract) || is.object(contract) ||
      !identical(names(attributes(contract)), "names") ||
      !identical(names(contract), expected) ||
      !identical(
        contract$schema_version, .rqr_dlm_rng_binding_schema()
      ) ||
      !is.character(contract$operation) ||
      length(contract$operation) != 1L ||
      is.na(contract$operation) || !nzchar(contract$operation) ||
      (!is.null(operation) &&
       !identical(contract$operation, operation)) ||
      !is.character(contract$mode) ||
      length(contract$mode) != 1L || is.na(contract$mode) ||
      !contract$mode %in% c(
        "none", "explicit_seed", "ambient_rng",
        "external_unbound"
      ) ||
      !is.numeric(contract$seed) || length(contract$seed) != 1L ||
      !is.character(contract$selection_mode) ||
      length(contract$selection_mode) != 1L ||
      is.na(contract$selection_mode) ||
      !contract$selection_mode %in%
        c("all", "subsample", "external") ||
      !is.numeric(contract$requested_draw_count) ||
      length(contract$requested_draw_count) != 1L ||
      !is.logical(contract$sampling_with_replacement) ||
      length(contract$sampling_with_replacement) != 1L ||
      !is.logical(contract$reproducibility_bound) ||
      length(contract$reproducibility_bound) != 1L ||
      is.na(contract$reproducibility_bound)) {
    stop("The DLM RNG-binding contract has an invalid schema.",
         call. = FALSE)
  }
  before <- .rqr_canonical_rng_state(contract$rng_state_before)
  after <- .rqr_canonical_rng_state(contract$rng_state_after)
  seeded <- identical(contract$mode, "explicit_seed")
  expected_bound <- contract$mode %in% c("none", "explicit_seed")
  selection_semantics <- switch(
    contract$selection_mode,
    all = is.na(contract$requested_draw_count) &&
      is.na(contract$sampling_with_replacement),
    subsample =
      is.finite(contract$requested_draw_count) &&
      contract$requested_draw_count ==
        floor(contract$requested_draw_count) &&
      contract$requested_draw_count >= 1 &&
      contract$requested_draw_count <=
        .Machine$integer.max &&
      !is.na(contract$sampling_with_replacement),
    external = is.na(contract$requested_draw_count) &&
      is.na(contract$sampling_with_replacement),
    FALSE
  )
  mode_selection_semantics <- switch(
    contract$mode,
    none = identical(contract$selection_mode, "all"),
    external_unbound =
      identical(contract$selection_mode, "external"),
    explicit_seed = contract$selection_mode %in%
      c("all", "subsample"),
    ambient_rng = contract$selection_mode %in%
      c("all", "subsample"),
    FALSE
  )
  state_semantics <- switch(
    contract$mode,
    none = is.null(before) && is.null(after) &&
      is.na(contract$seed),
    explicit_seed = !is.null(before) && !is.null(after) &&
      is.finite(contract$seed) &&
      contract$seed == floor(contract$seed) &&
      contract$seed >= 0 &&
      contract$seed <= .Machine$integer.max,
    ambient_rng = !is.null(after) && is.na(contract$seed),
    external_unbound = is.null(before) && is.null(after) &&
      is.na(contract$seed),
    FALSE
  )
  if (!state_semantics || !selection_semantics ||
      !mode_selection_semantics ||
      !identical(contract$reproducibility_bound, expected_bound) ||
      (seeded &&
       !identical(as.integer(contract$seed), contract$seed))) {
    stop(
      "The DLM RNG-binding contract violates its mode semantics.",
      call. = FALSE
    )
  }
  invisible(contract)
}

.rqr_init_state_paths <- function(y, FF, m0, coverage_level, init = list()) {
  p <- nrow(FF)
  T <- ncol(FF)
  supplied1 <- init$state_root1 %||% init$theta1 %||% NULL
  supplied2 <- init$state_root2 %||% init$theta2 %||% NULL
  if (!is.null(supplied1) || !is.null(supplied2)) {
    if (is.null(supplied1) || is.null(supplied2)) stop("init must provide both state paths.", call. = FALSE)
    if (!is.matrix(supplied1) || !is.numeric(supplied1) ||
        is.object(supplied1) ||
        !is.matrix(supplied2) || !is.numeric(supplied2) ||
        is.object(supplied2) ||
        !identical(dim(supplied1), c(p, T)) ||
        !identical(dim(supplied2), c(p, T)) ||
        any(!is.finite(supplied1)) || any(!is.finite(supplied2))) {
      stop(
        "Initial state paths must be finite plain numeric p x T matrices.",
        call. = FALSE
      )
    }
    storage.mode(supplied1) <- "double"
    storage.mode(supplied2) <- "double"
    return(list(theta1 = supplied1, theta2 = supplied2))
  }
  observed <- !is.na(y)
  alpha <- rqr_constants(coverage_level)$alpha
  probs <- c((1 - alpha) / 2, 1 - (1 - alpha) / 2)
  qs <- as.numeric(stats::quantile(y[observed], probs = probs, names = FALSE, type = 8))
  Fobs <- FF[, observed, drop = FALSE]
  base <- drop(crossprod(Fobs, m0))
  precision <- tcrossprod(Fobs) + diag(1e-6, p)
  correction <- function(target) {
    rhs <- Fobs %*% rep(target, ncol(Fobs)) - Fobs %*% base
    m0 + solve(precision, rhs)
  }
  b1 <- correction(qs[1L])
  b2 <- correction(qs[2L])
  list(
    theta1 = matrix(rep(b1, T), p, T),
    theta2 = matrix(rep(b2, T), p, T)
  )
}

.rqr_dlm_evolution <- function(
    mode, model, expanded, y, W, df, dim.df, reference_variance,
    reference_design, component_templates, evolution_scale_prior,
    evolution_scale_initial, evolution_spec, numerical_policy, jitter_ladder) {
  if (!is.null(evolution_spec)) {
    .rqr_validate_dlm_evolution_spec(
      evolution_spec, expanded, y = y
    )
    if (identical(evolution_spec$mode, "fixed_W")) {
      evolution_spec <- rqr_evolution_fixed(
        .rqr_expand_cube(
          evolution_spec$W, expanded$n_time, expanded$p,
          "evolution_spec$W"
        )
      )
    }
    .rqr_validate_dlm_evolution_spec(
      evolution_spec, expanded, y = y
    )
    return(evolution_spec)
  }
  mode <- match.arg(
    mode, c("fixed_W", "discount_template", "component_scale", "adaptive_discount")
  )
  p <- expanded$p
  T <- expanded$n_time
  if (mode == "fixed_W") {
    if (is.null(W)) stop("evolution_mode='fixed_W' requires W.", call. = FALSE)
    return(rqr_evolution_fixed(.rqr_expand_cube(W, T, p, "W")))
  }
  if (mode == "component_scale") {
    if (is.null(component_templates)) {
      stop("evolution_mode='component_scale' requires component_templates.", call. = FALSE)
    }
    return(rqr_evolution_component_scale(
      templates = component_templates,
      component_dims = expanded$component_dims,
      component_names = expanded$component_names,
      prior = evolution_scale_prior,
      initial = evolution_scale_initial
    ))
  }
  if (is.null(dim.df)) dim.df <- model$component_dims %||% p
  if (is.null(df)) stop("Discount evolution requires df.", call. = FALSE)
  if (mode == "adaptive_discount") {
    return(rqr_evolution_adaptive_working(df, dim.df))
  }
  reference_variance_source <- if (is.null(reference_variance)) {
    "training_response_variance"
  } else {
    "user_supplied"
  }
  if (is.null(reference_variance)) {
    yy <- y[!is.na(y)]
    empirical_variance <- stats::var(yy)
    if (!is.finite(empirical_variance) || empirical_variance <= 0) empirical_variance <- 1
    reference_variance <- max(empirical_variance, sqrt(.Machine$double.eps))
  }
  template <- rqr_freeze_discount_template(
    model = model, n_time = T, df = df, dim.df = dim.df,
    reference_variance = reference_variance,
    reference_design = reference_design,
    numerical_policy = numerical_policy,
    jitter_ladder = jitter_ladder
  )
  template$reference_variance_source <- reference_variance_source
  template$empirical_bayes <- identical(
    reference_variance_source, "training_response_variance"
  )
  template$construction_contract$reference_variance_source <-
    reference_variance_source
  .rqr_validate_dlm_evolution_spec(
    template, expanded, y = y
  )
  template
}

.rqr_bind_ffbs_repairs <- function(records, diagnostics, iteration, root) {
  current <- diagnostics$repair_records
  if (is.null(current) || !nrow(current)) return(records)
  current$iteration <- as.integer(iteration)
  current$root <- as.character(root)
  current <- current[, c(
    "iteration", "root", "stage", "time", "strategy", "jitter",
    "relative_jitter", "min_eigenvalue", "matrix_scale", "jitter_scale",
    "absolute_jitter_fallback", "clamped_eigenvalues"
  )]
  if (is.null(records)) current else rbind(records, current)
}

.rqr_dlm_provenance_matrices <- function(expanded, evolution) {
  list(
    FF = expanded$FF,
    GG = expanded$GG,
    C0 = expanded$C0,
    evolution_W = evolution$W %||% NULL,
    evolution_templates = evolution$templates %||% NULL,
    evolution_discount = evolution$D %||% NULL
  )
}

.rqr_dlm_model_contract <- function(expanded) {
  list(
    FF = expanded$FF,
    GG = expanded$GG,
    m0 = expanded$m0,
    C0 = expanded$C0,
    component_dims = expanded$component_dims,
    component_names = expanded$component_names,
    state_dimension = expanded$p,
    n_time = expanded$n_time
  )
}

.rqr_dlm_target_schema <- function() {
  "rqrgibbs_dlm_ordinary_target/1.0.0"
}

.rqr_dlm_target_contract <- function(
    coverage_level, learning_rate_mode, fixed_learning_rate,
    loss_reference_scale, lambda_prior, numerical_policy, jitter_ladder) {
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
  numerical_policy <- .rqr_numerical_policy(numerical_policy)
  list(
    schema_version = .rqr_dlm_target_schema(),
    tilt = 0,
    loss_name = "rqr_residual_product_check_loss",
    coverage_level = rqr_constants(coverage_level)$alpha,
    learning_rate_mode = mode,
    fixed_learning_rate = fixed_learning_rate,
    loss_reference_scale = loss_reference_scale,
    lambda_prior = .rqr_lambda_prior(lambda_prior, mode),
    numerical_policy = numerical_policy,
    jitter_ladder = .rqr_jitter_ladder(
      numerical_policy, jitter_ladder
    ),
    root_priors_exchangeable = TRUE,
    root_swap_probability = 0.5,
    generalized_bayes = TRUE,
    response_likelihood = FALSE,
    response_prediction_contract = FALSE
  )
}

.rqr_dlm_transition_kernel_schema <- function() {
  "rqrgibbs_dlm_transition_kernel/1.0.0"
}

.rqr_dlm_transition_kernel_contract <- function(
    evolution_mode, learning_rate_mode,
    complete_time0_state = FALSE,
    component_names = character(),
    component_scale_collapsed_update = FALSE,
    component_scale_interweave = FALSE,
    component_scale_interweave_cycles = 1L,
    component_scale_slice_width = 1,
    component_scale_slice_sweeps = 1L,
    component_scale_slice_max_steps = 100L,
    component_scale_slice_max_shrink = 1000L) {
  evolution_mode <- as.character(evolution_mode)[1L]
  component_mode <- identical(evolution_mode, "component_scale")
  partially_collapsed <- component_mode &&
    isTRUE(component_scale_collapsed_update)
  interwoven <- component_mode && isTRUE(component_scale_interweave)
  learn_lambda <- !identical(learning_rate_mode, "fixed_rate")
  complete_time0_state <- isTRUE(complete_time0_state)
  component_names <- as.character(component_names)
  list(
    schema_version = .rqr_dlm_transition_kernel_schema(),
    evolution_mode = evolution_mode,
    learning_rate_mode = as.character(learning_rate_mode),
    time0_state_completion = complete_time0_state,
    one_root_partially_collapsed = partially_collapsed,
    collapsed_integrated_root = if (partially_collapsed) "root1" else "none",
    collapsed_conditioned_root = if (partially_collapsed) "root2" else "none",
    collapsed_log_q_coordinate_order = if (partially_collapsed) {
      component_names
    } else {
      character()
    },
    scan_order = c(
      if (learn_lambda) "lambda_collapsed" else "lambda_fixed",
      "latent_v_refresh",
      if (partially_collapsed) "component_scale_root1_collapsed",
      "root1_ffbs",
      if (complete_time0_state) "root1_time0",
      "root2_ffbs",
      if (complete_time0_state) "root2_time0",
      if (component_mode && !interwoven) {
        "component_scale_centered_inverse_gamma"
      },
      if (interwoven) {
        paste0(
          "component_scale_centered_noncentered_cycles_",
          as.integer(component_scale_interweave_cycles)
        )
      },
      "global_root_swap"
    ),
    collapsed_slice_width = if (partially_collapsed) {
      as.numeric(component_scale_slice_width)
    } else {
      NA_real_
    },
    collapsed_slice_sweeps = if (partially_collapsed) {
      as.integer(component_scale_slice_sweeps)
    } else {
      0L
    },
    collapsed_slice_max_steps = if (partially_collapsed) {
      as.integer(component_scale_slice_max_steps)
    } else {
      0L
    },
    collapsed_slice_max_shrink = if (partially_collapsed) {
      as.integer(component_scale_slice_max_shrink)
    } else {
      0L
    },
    centered_inverse_gamma = component_mode,
    noncentered_slice_interweave = interwoven,
    interweave_cycles = if (interwoven) {
      as.integer(component_scale_interweave_cycles)
    } else {
      0L
    },
    interweave_slice_width = if (interwoven) {
      as.numeric(component_scale_slice_width)
    } else {
      NA_real_
    },
    interweave_slice_sweeps_per_cycle = if (interwoven) {
      as.integer(component_scale_slice_sweeps)
    } else {
      0L
    },
    interweave_slice_max_steps = if (interwoven) {
      as.integer(component_scale_slice_max_steps)
    } else {
      0L
    },
    interweave_slice_max_shrink = if (interwoven) {
      as.integer(component_scale_slice_max_shrink)
    } else {
      0L
    },
    global_root_swap_probability = 0.5,
    target_change = FALSE
  )
}

.rqr_dlm_evolution_contract <- function(evolution) {
  unclass(evolution)
}

.rqr_dlm_model_spec_fields <- function() {
  c(
    "family", "parameterization", "target_schema_version", "tilt",
    "loss_name", "state_model",
    "coverage_level", "learning_rate_mode", "fixed_learning_rate",
    "learning_rate", "lambda_initial", "loss_reference_scale",
    "effective_learning_rate", "lambda_prior", "lambda_summary",
    "inferential_target", "generalized_bayes", "response_likelihood",
    "response_prediction_contract", "evolution_mode",
    "transition_kernel", "transition_kernel_digest",
    "target_contract", "exact_joint_target",
    "ordinary_v1_scope_eligible", "continuation_supported",
    "numerical_policy", "numerical_repair_count",
    "cumulative_numerical_repair_count",
    "numerically_exact_transition",
    "chain_history_numerically_exact",
    "parent_chain_history_numerically_exact",
    "segment_target_numerical_eligible",
    "target_numerical_eligible", "reproducibility_eligible",
    "parent_promotion_eligible", "promotion_eligible",
    "root_priors_exchangeable", "root_swap_move"
  )
}

.rqr_validate_dlm_model_spec_semantics <- function(
    object, evolution_properties) {
  spec <- object$model_spec
  if (!is.list(spec) || is.object(spec) ||
      !identical(names(attributes(spec)), "names") ||
      !identical(names(spec), .rqr_dlm_model_spec_fields()) ||
      anyDuplicated(names(spec))) {
    stop(
      "The DLM model specification does not have its exact field schema.",
      call. = FALSE
    )
  }
  mode <- .rqr_learning_rate_mode(spec$learning_rate_mode)
  canonical_target <- .rqr_dlm_target_contract(
    coverage_level = spec$coverage_level,
    learning_rate_mode = mode,
    fixed_learning_rate = spec$fixed_learning_rate,
    loss_reference_scale = spec$loss_reference_scale,
    lambda_prior = spec$lambda_prior,
    numerical_policy = spec$numerical_policy,
    jitter_ladder = object$misc$jitter_ladder
  )
  ordinary_scope <-
    evolution_properties$ordinary_v1_evolution &&
    !identical(mode, "learned_pure")
  segment_repairs <- .rqr_history_count(
    spec$numerical_repair_count,
    "model_spec$numerical_repair_count"
  )
  cumulative_repairs <- .rqr_history_count(
    spec$cumulative_numerical_repair_count,
    "model_spec$cumulative_numerical_repair_count"
  )
  valid_flag <- function(value) {
    is.logical(value) && length(value) == 1L && !is.na(value)
  }
  flag_fields <- c(
    "generalized_bayes", "response_likelihood",
    "response_prediction_contract", "exact_joint_target",
    "ordinary_v1_scope_eligible", "continuation_supported",
    "numerically_exact_transition",
    "chain_history_numerically_exact",
    "parent_chain_history_numerically_exact",
    "segment_target_numerical_eligible",
    "target_numerical_eligible", "reproducibility_eligible",
    "parent_promotion_eligible", "promotion_eligible",
    "root_priors_exchangeable", "root_swap_move"
  )
  if (!all(vapply(spec[flag_fields], valid_flag, logical(1L)))) {
    stop(
      "The DLM model specification contains invalid logical metadata.",
      call. = FALSE
    )
  }
  expected_segment_exact <- segment_repairs == 0L
  expected_chain_exact <- cumulative_repairs == 0L
  expected_segment_target <-
    evolution_properties$exact_joint_target &&
    expected_segment_exact
  expected_target <-
    evolution_properties$exact_joint_target &&
    expected_chain_exact
  expected_promotion <- ordinary_scope &&
    expected_target &&
    spec$reproducibility_eligible
  semantic_checks <- c(
    identical(spec$family, "rqr_dlm"),
    identical(
      spec$parameterization, "exchangeable_dynamic_roots"
    ),
    identical(
      spec$target_schema_version, .rqr_dlm_target_schema()
    ),
    is.numeric(spec$tilt) && length(spec$tilt) == 1L &&
      !is.na(spec$tilt) && identical(as.numeric(spec$tilt), 0),
    identical(
      spec$loss_name, "rqr_residual_product_check_loss"
    ),
    identical(
      spec$coverage_level, canonical_target$coverage_level
    ),
    identical(
      spec$learning_rate_mode,
      canonical_target$learning_rate_mode
    ),
    identical(
      spec$fixed_learning_rate,
      canonical_target$fixed_learning_rate
    ),
    identical(
      spec$loss_reference_scale,
      canonical_target$loss_reference_scale
    ),
    identical(spec$lambda_prior, canonical_target$lambda_prior),
    identical(
      spec$numerical_policy, canonical_target$numerical_policy
    ),
    identical(
      spec$state_model,
      "linear_gaussian_interval_root_evolution"
    ),
    identical(
      spec$inferential_target, .rqr_target_formula(mode)
    ),
    identical(
      spec$generalized_bayes, canonical_target$generalized_bayes
    ),
    identical(
      spec$response_likelihood,
      canonical_target$response_likelihood
    ),
    identical(
      spec$response_prediction_contract,
      canonical_target$response_prediction_contract
    ),
    identical(spec$evolution_mode, object$evolution$mode),
    identical(
      spec$target_contract,
      if (evolution_properties$exact_joint_target) {
        "fixed_joint_exact"
      } else {
        "working_sequential"
      }
    ),
    identical(
      spec$exact_joint_target,
      evolution_properties$exact_joint_target
    ),
    identical(spec$ordinary_v1_scope_eligible, ordinary_scope),
    identical(spec$continuation_supported, ordinary_scope),
    identical(
      spec$numerically_exact_transition,
      expected_segment_exact
    ),
    cumulative_repairs >= segment_repairs,
    identical(
      spec$chain_history_numerically_exact,
      expected_chain_exact
    ),
    identical(
      spec$segment_target_numerical_eligible,
      expected_segment_target
    ),
    identical(spec$target_numerical_eligible, expected_target),
    identical(
      spec$reproducibility_eligible,
      isTRUE(object$provenance$reproducibility_eligible)
    ),
    identical(spec$promotion_eligible, expected_promotion),
    identical(
      spec$root_priors_exchangeable,
      canonical_target$root_priors_exchangeable
    ),
    identical(
      spec$root_swap_move,
      canonical_target$root_swap_probability > 0
    )
  )
  if (!all(semantic_checks)) {
    stop(
      paste(
        "The DLM model specification conflicts with canonical",
        "target, evolution, numerical, or interpretation semantics."
      ),
      call. = FALSE
    )
  }
  invisible(list(
    learning_rate_mode = mode,
    ordinary_v1_scope_eligible = ordinary_scope,
    continuation_supported = ordinary_scope
  ))
}

.rqr_dlm_provenance_objects <- function(expanded, evolution, target_contract) {
  list(
    model = .rqr_dlm_model_contract(expanded),
    target = target_contract,
    evolution = .rqr_dlm_evolution_contract(evolution)
  )
}

.rqr_dlm_coverage_summary <- function(y, observed, lower, upper) {
  lower_mean <- rowMeans(lower)
  upper_mean <- rowMeans(upper)
  lower_observed <- lower[observed, , drop = FALSE]
  upper_observed <- upper[observed, , drop = FALSE]
  y_observed <- y[observed]
  covered <- sweep(lower_observed, 1L, y_observed, `<=`) &
    sweep(upper_observed, 1L, y_observed, `>=`)
  coverage_by_draw <- colMeans(covered)
  list(
    lower_mean = lower_mean,
    upper_mean = upper_mean,
    midpoint_mean = rowMeans(0.5 * (lower + upper)),
    width_mean = rowMeans(upper - lower),
    coverage_posterior_mean_endpoints = mean(
      y_observed >= lower_mean[observed] & y_observed <= upper_mean[observed]
    ),
    coverage_draw_mean = mean(coverage_by_draw),
    coverage_draw_quantiles = stats::quantile(
      coverage_by_draw, c(0.05, 0.5, 0.95), names = TRUE, type = 8
    ),
    width_mean_scalar = mean(rowMeans(upper - lower))
  )
}

.rqr_dlm_continuation_token <- new.env(parent = emptyenv())

.rqr_dlm_continuation_init_fields <- function() {
  c(
    "completed_iterations", "continued_from_checkpoint",
    "parent_cumulative_numerical_repair_count",
    "parent_chain_history_numerically_exact",
    "parent_promotion_eligible", "continuation_control"
  )
}

.rqr_dlm_schedule_schema <- function() {
  "rqrgibbs_dlm_segment_schedule/2.0.0"
}

.rqr_dlm_schedule_prefix <- function(segments) {
  list(
    schema_version = .rqr_dlm_schedule_schema(),
    fit_schema_version = .rqr_schema_version(),
    generation = as.integer(length(segments) - 1L),
    segments = segments
  )
}

.rqr_validate_dlm_schedule_value <- function(
    contract, stored_digest = NULL) {
  contract_fields <- c(
    "schema_version", "fit_schema_version", "generation", "segments"
  )
  segment_fields <- c(
    "generation", "start_completed_iterations", "n_burn",
    "n_retained_draws", "thin", "raw_iterations",
    "end_completed_iterations", "ends_on_retained_draw",
    "parent_checkpoint_digest", "parent_schedule_digest",
    "initialization_contract", "initialization_digest",
    "checkpoint_digest", "checkpoint_state"
  )
  if (!is.list(contract) || is.object(contract) ||
      !identical(names(attributes(contract)), "names") ||
      !identical(names(contract), contract_fields) ||
      !identical(contract$schema_version, .rqr_dlm_schedule_schema()) ||
      !identical(contract$fit_schema_version, .rqr_schema_version()) ||
      !is.list(contract$segments) ||
      is.object(contract$segments) ||
      !is.null(attributes(contract$segments)) ||
      !length(contract$segments)) {
    stop("The DLM segment-schedule contract is unsupported.",
         call. = FALSE)
  }
  if (!is.null(stored_digest) &&
      (!is.character(stored_digest) ||
       length(stored_digest) != 1L || is.na(stored_digest) ||
       !grepl("^[0-9a-f]{64}$", stored_digest) ||
       !identical(.rqr_digest(contract), stored_digest))) {
    stop("The DLM segment-schedule digest is invalid.", call. = FALSE)
  }
  generation <- tryCatch(
    .rqr_history_count(
      contract$generation, "segment_schedule_contract$generation"
    ),
    error = function(error) NA_integer_
  )
  if (is.na(generation) ||
      !identical(generation, as.integer(length(contract$segments) - 1L))) {
    stop("The DLM segment-schedule generation is invalid.", call. = FALSE)
  }

  previous_end <- 0L
  previous_checkpoint <- NA_character_
  for (index in seq_along(contract$segments)) {
    segment <- contract$segments[[index]]
    if (!is.list(segment) || is.object(segment) ||
        !identical(names(attributes(segment)), "names") ||
        !identical(names(segment), segment_fields)) {
      stop(
        sprintf(
          "DLM segment schedule %d is incomplete.",
          as.integer(index - 1L)
        ),
        call. = FALSE
      )
    }
    count <- function(field, minimum = 0L) {
      tryCatch(
        .rqr_scalar_integer(
          segment[[field]], sprintf("segment_schedule$%s", field),
          minimum = minimum
        ),
        error = function(error) NA_integer_
      )
    }
    segment_generation <- count("generation")
    start <- count("start_completed_iterations")
    burn <- count("n_burn")
    retained <- count("n_retained_draws", 1L)
    thin <- count("thin", 1L)
    raw <- count("raw_iterations", 1L)
    end <- count("end_completed_iterations", 1L)
    expected_raw <- as.double(burn) +
      as.double(retained) * as.double(thin)
    expected_end <- as.double(start) + expected_raw
    arithmetic_valid <- is.finite(expected_raw) &&
      is.finite(expected_end) &&
      expected_raw <= .Machine$integer.max &&
      expected_end <= .Machine$integer.max &&
      identical(raw, as.integer(expected_raw)) &&
      identical(end, as.integer(expected_end))
    checkpoint_valid <-
      is.character(segment$checkpoint_digest) &&
      length(segment$checkpoint_digest) == 1L &&
      !is.na(segment$checkpoint_digest) &&
      grepl("^[0-9a-f]{64}$", segment$checkpoint_digest) &&
      is.list(segment$checkpoint_state) &&
      !is.object(segment$checkpoint_state) &&
      identical(
        names(attributes(segment$checkpoint_state)), "names"
      ) &&
      identical(
        .rqr_digest(segment$checkpoint_state),
        segment$checkpoint_digest
      ) &&
      identical(
        segment$checkpoint_state$completed_iterations,
        end
      )
    initialization_valid <- tryCatch({
      .rqr_validate_initialization_contract(
        segment$initialization_contract,
        family = "rqr_dlm",
        stored_digest = segment$initialization_digest
      )
      identical(
        segment$initialization_contract$segment_type,
        if (index == 1L) "fresh" else "continuation"
      ) &&
        identical(
          segment$initialization_contract$
            parent_checkpoint_digest,
          if (index == 1L) NA_character_ else previous_checkpoint
        )
    }, error = function(error) FALSE)
    links_valid <- if (index == 1L) {
      is.character(segment$parent_checkpoint_digest) &&
        length(segment$parent_checkpoint_digest) == 1L &&
        is.na(segment$parent_checkpoint_digest) &&
        is.character(segment$parent_schedule_digest) &&
        length(segment$parent_schedule_digest) == 1L &&
        is.na(segment$parent_schedule_digest)
    } else {
      identical(
        segment$parent_checkpoint_digest, previous_checkpoint
      ) &&
        identical(
          segment$parent_schedule_digest,
          .rqr_digest(.rqr_dlm_schedule_prefix(
            contract$segments[seq_len(index - 1L)]
          ))
        )
    }
    endpoint_valid <-
      is.logical(segment$ends_on_retained_draw) &&
      length(segment$ends_on_retained_draw) == 1L &&
      !is.na(segment$ends_on_retained_draw) &&
      isTRUE(segment$ends_on_retained_draw)
    if (anyNA(c(
        segment_generation, start, burn, retained, thin, raw, end
      )) ||
        !identical(segment_generation, as.integer(index - 1L)) ||
        !identical(start, previous_end) ||
        (index > 1L && burn != 0L) ||
        !arithmetic_valid || !checkpoint_valid ||
        !initialization_valid ||
        !links_valid || !endpoint_valid) {
      stop(
        sprintf(
          "DLM segment schedule %d is structurally invalid.",
          as.integer(index - 1L)
        ),
        call. = FALSE
      )
    }
    previous_end <- end
    previous_checkpoint <- segment$checkpoint_digest
  }
  invisible(contract)
}

.rqr_make_dlm_schedule_contract <- function(
    start_completed_iterations, n_burn, n_retained_draws, thin,
    initialization_contract, initialization_digest,
    checkpoint_digest, checkpoint_state,
    parent = NULL, parent_digest = NULL) {
  start_completed_iterations <- .rqr_history_count(
    start_completed_iterations, "start_completed_iterations"
  )
  n_burn <- .rqr_history_count(n_burn, "n_burn")
  n_retained_draws <- .rqr_scalar_integer(
    n_retained_draws, "n_retained_draws", 1L
  )
  thin <- .rqr_scalar_integer(thin, "thin", 1L)
  if (!is.character(checkpoint_digest) ||
      length(checkpoint_digest) != 1L ||
      is.na(checkpoint_digest) ||
      !grepl("^[0-9a-f]{64}$", checkpoint_digest)) {
    stop("checkpoint_digest must be a complete SHA-256 digest.",
         call. = FALSE)
  }
  if (!is.list(checkpoint_state) ||
      !identical(.rqr_digest(checkpoint_state), checkpoint_digest)) {
    stop(
      "checkpoint_state must exactly realize checkpoint_digest.",
      call. = FALSE
    )
  }
  .rqr_validate_initialization_contract(
    initialization_contract,
    family = "rqr_dlm",
    stored_digest = initialization_digest
  )
  raw_iterations <- .rqr_history_count(
    as.double(n_burn) +
      as.double(n_retained_draws) * as.double(thin),
    "raw_iterations"
  )
  end_completed_iterations <- .rqr_history_count(
    as.double(start_completed_iterations) +
      as.double(raw_iterations),
    "end_completed_iterations"
  )
  if (is.null(parent)) {
    if (!is.null(parent_digest) || start_completed_iterations != 0L) {
      stop(
        "An initial DLM schedule must start at zero without a parent.",
        call. = FALSE
      )
    }
    generation <- 0L
    segments <- list()
    parent_checkpoint_digest <- NA_character_
    stored_parent_digest <- NA_character_
  } else {
    .rqr_validate_dlm_schedule_value(parent, parent_digest)
    if (n_burn != 0L) {
      stop("A continued DLM schedule cannot add burn-in.",
           call. = FALSE)
    }
    generation <- .rqr_history_count(
      as.double(parent$generation) + 1,
      "continued DLM schedule generation"
    )
    segments <- parent$segments
    prior <- utils::tail(segments, 1L)[[1L]]
    if (!identical(
        start_completed_iterations, prior$end_completed_iterations
      )) {
      stop(
        "A continued DLM schedule must start at the parent end count.",
        call. = FALSE
      )
    }
    parent_checkpoint_digest <- prior$checkpoint_digest
    stored_parent_digest <- parent_digest
  }
  segment <- list(
    generation = generation,
    start_completed_iterations = start_completed_iterations,
    n_burn = n_burn,
    n_retained_draws = n_retained_draws,
    thin = thin,
    raw_iterations = raw_iterations,
    end_completed_iterations = end_completed_iterations,
    ends_on_retained_draw = TRUE,
    parent_checkpoint_digest = parent_checkpoint_digest,
    parent_schedule_digest = stored_parent_digest,
    initialization_contract = initialization_contract,
    initialization_digest = initialization_digest,
    checkpoint_digest = checkpoint_digest,
    checkpoint_state = checkpoint_state
  )
  contract <- .rqr_dlm_schedule_prefix(c(segments, list(segment)))
  .rqr_validate_dlm_schedule_value(contract)
  contract
}

#' Fit a dynamic RQR interval-root model with alternating FFBS
#'
#' This sampler targets root trajectories under an exponentiated RQR loss and
#' Gaussian state priors. It does not define a response likelihood. Fixed `W`,
#' frozen discount templates, and component-scale evolution priors define exact
#' generalized-posterior samplers when no numerical repair is used. Adaptive
#' discount recursion is an explicitly experimental working update.
#'
#' @details
#' `mcmc_control` accepts the usual iteration, seed, storage, backend, progress,
#' and jitter fields plus seven controls that change the component-scale
#' transition kernel:
#'
#' * `component_scale_collapsed_update` (`FALSE`) enables the exact one-root
#'   partially collapsed log-scale slice update;
#' * `component_scale_interweave` (`FALSE`) enables exact centered/noncentered
#'   scale interweaving;
#' * `component_scale_slice_width` (`1`) is the positive log-scale slice width;
#' * `component_scale_slice_max_steps` (`100`) bounds slice stepping out;
#' * `component_scale_slice_max_shrink` (`1000`) bounds slice shrinkage;
#' * `component_scale_slice_sweeps` (`1`) gives the coordinate sweeps per slice
#'   update; and
#' * `component_scale_interweave_cycles` (`1`) gives the number of
#'   centered/noncentered cycles.
#'
#' These seven fields are valid only with `evolution_mode = "component_scale"`
#' when they enable a component-scale operation. Slice tuning fields are used
#' by whichever collapsed or interwoven slice update is enabled. The default
#' component-scale scan uses the centered inverse-Gamma update.
#'
#' The versioned transition contract records the complete scan. It updates a
#' learned loss rate when requested, refreshes the pseudo-AL scales, optionally
#' integrates `root1` while conditioning on `root2` in the partially collapsed
#' scale step, draws `root1` and then `root2` by conditional FFBS, completes
#' their time-zero states, applies the declared component-scale update, and
#' finishes with a global root-label swap of probability `0.5`. Time-zero
#' completion is part of every accepted exact DLM transition (fixed covariance,
#' frozen discount template, and component scale), independently of whether
#' state paths are stored. The experimental adaptive working transition omits
#' time-zero completion and records that omission in its kernel contract.
#' Component order, root orientation, all seven controls, and this scan order
#' are checkpoint- and continuation-bound; changing any of them requires a new
#' fit rather than continuation.
#'
#' @param y Response vector. `NA` values are treated as missing observations.
#' @param model An [rqr_as_dlm_model()] or exdqlm-compatible model.
#' @param coverage_level Interval coverage target in `(0,1)`.
#' @param evolution_mode One of `"fixed_W"`, `"discount_template"`,
#'   `"component_scale"`, or `"adaptive_discount"`.
#' @param evolution_spec Optional previously constructed evolution object. Used
#'   for exact continuation and overrides the other evolution arguments.
#' @param W Fixed evolution covariance matrix or cube.
#' @param df,dim.df Component discounts and their state dimensions.
#' @param reference_variance,reference_design Inputs used to freeze a discount
#'   template before MCMC.
#' @param component_templates Fixed SPD templates for exact component scales.
#' @param evolution_scale_prior Inverse-Gamma shape and rate for component
#'   evolution multipliers.
#' @param evolution_scale_initial Initial positive component multipliers.
#' @param learning_rate Fixed generalized-Bayes learning rate `omega_R`.
#' @param lambda_initial Initial inverse loss scale for learned modes.
#' @param loss_reference_scale Positive reference scale `s_L`. It does not
#'   alter fixed `learning_rate`; learned modes use `lambda/s_L`.
#' @param learning_rate_mode One of `"fixed_rate"`,
#'   `"learned_pseudoresidual_normalized"`, or `"learned_pure"`. Legacy mode
#'   spellings are accepted and normalized to these names. `"learned_pure"`
#'   remains executable only for diagnostic compatibility; it is outside the
#'   ordinary-v1 scope, nonpromotable, and noncontinuable.
#' @param lambda_prior Gamma shape--rate prior without a custom power field.
#' @param numerical_policy Either `"fail"` or `"record_repair"`.
#' @param provenance_control Optional primary-repository provenance plus named
#'   `external_repositories`. Repository specifications contain `repo_root`, a
#'   complete 40-character `expected_git_commit`, and optional runtime package
#'   and attestation fields.
#' @param mcmc_control A fully named list containing supported iteration, seed,
#'   storage, backend, progress, jitter, and optional exact partially
#'   collapsed/interwoven component-scale controls. Unknown fields and
#'   coercive scalar values are rejected.
#' @param init A fully named list of optional initial state paths, latent
#'   scales, lambda, evolution scales, time-zero states, and RNG state.
#'   Latent scales may be scalar, observed-site length, or response length;
#'   missing-site entries are ignored and replaced by the canonical inverse
#'   loss-rate placeholder.
#'   `theta1` and `theta2` remain supported as legacy aliases for
#'   `state_root1` and `state_root2`; supplying both forms is an error.
#'   Continuation-history fields are private to [rqr_dlm_continue()].
#' @return An `rqr_dlm_mcmc` object.
#' @family RQR-DLM
#' @export
rqr_dlm_fit <- function(
    y, model, coverage_level,
    evolution_mode = c(
      "fixed_W", "discount_template", "component_scale", "adaptive_discount"
    ),
    evolution_spec = NULL,
    W = NULL, df = NULL, dim.df = NULL,
    reference_variance = NULL, reference_design = NULL,
    component_templates = NULL,
    evolution_scale_prior = list(shape = 2, rate = 1),
    evolution_scale_initial = 1,
    learning_rate = 1, lambda_initial = 1, loss_reference_scale = 1,
    learning_rate_mode = c(
      "fixed_rate", "learned_pseudoresidual_normalized", "learned_pure"
    ),
    lambda_prior = list(shape = 4, rate = 4),
    numerical_policy = c("fail", "record_repair"),
    provenance_control = list(),
    mcmc_control = list(), init = list()) {
  if (is.list(init) &&
      any(.rqr_dlm_continuation_init_fields() %in% names(init))) {
    stop(
      paste(
        "Continuation-only init fields are private to",
        "rqr_dlm_continue()."
      ),
      call. = FALSE
    )
  }
  .rqr_dlm_fit_impl(
    y = y, model = model, coverage_level = coverage_level,
    evolution_mode = evolution_mode,
    evolution_spec = evolution_spec,
    W = W, df = df, dim.df = dim.df,
    reference_variance = reference_variance,
    reference_design = reference_design,
    component_templates = component_templates,
    evolution_scale_prior = evolution_scale_prior,
    evolution_scale_initial = evolution_scale_initial,
    learning_rate = learning_rate,
    lambda_initial = lambda_initial,
    loss_reference_scale = loss_reference_scale,
    learning_rate_mode = learning_rate_mode,
    lambda_prior = lambda_prior,
    numerical_policy = numerical_policy,
    provenance_control = provenance_control,
    mcmc_control = mcmc_control,
    init = init,
    .continuation_token = NULL
  )
}

.rqr_dlm_fit_impl <- function(
    y, model, coverage_level,
    evolution_mode = c(
      "fixed_W", "discount_template", "component_scale", "adaptive_discount"
    ),
    evolution_spec = NULL,
    W = NULL, df = NULL, dim.df = NULL,
    reference_variance = NULL, reference_design = NULL,
    component_templates = NULL,
    evolution_scale_prior = list(shape = 2, rate = 1),
    evolution_scale_initial = 1,
    learning_rate = 1, lambda_initial = 1, loss_reference_scale = 1,
    learning_rate_mode = c(
      "fixed_rate", "learned_pseudoresidual_normalized", "learned_pure"
    ),
    lambda_prior = list(shape = 4, rate = 4),
    numerical_policy = c("fail", "record_repair"),
    provenance_control = list(),
    mcmc_control = list(), init = list(),
    .continuation_token = NULL) {
  if (!is.numeric(y) || is.object(y) || !is.null(dim(y)) ||
      !length(y) || !any(!is.na(y)) || any(is.nan(y)) ||
      any(is.infinite(y))) {
    stop(
      paste(
        "y must be a plain numeric vector containing at least one finite",
        "observation; missing values must be NA."
      ),
      call. = FALSE
    )
  }
  y <- as.numeric(y)
  names(y) <- NULL
  if (!is.list(init)) stop("init must be a list.", call. = FALSE)
  .rqr_validate_named_list_fields(
    init, "init",
    c(
      "state_root1", "state_root2", "theta1", "theta2",
      "latent_v", "lambda", "evolution_scale",
      "theta0_root1", "theta0_root2", "rng_state",
      "completed_iterations", "continued_from_checkpoint",
      "parent_cumulative_numerical_repair_count",
      "parent_chain_history_numerically_exact",
      "parent_promotion_eligible", "continuation_control"
    )
  )
  if (all(c("state_root1", "theta1") %in% names(init)) ||
      all(c("state_root2", "theta2") %in% names(init))) {
    stop(
      "init cannot supply both canonical state paths and their legacy aliases.",
      call. = FALSE
    )
  }
  continued_from_checkpoint <- identical(
    .continuation_token, .rqr_dlm_continuation_token
  )
  declared_continuation <- .rqr_scalar_logical(
    init$continued_from_checkpoint %||% FALSE,
    "init$continued_from_checkpoint"
  )
  if (!identical(
      declared_continuation, continued_from_checkpoint
    ) ||
      (!continued_from_checkpoint &&
       any(.rqr_dlm_continuation_init_fields() %in% names(init)))) {
    stop(
      paste(
        "Continuation-only init fields require the private validated",
        "rqr_dlm_continue() worker boundary."
      ),
      call. = FALSE
    )
  }
  continuation_control <- init$continuation_control %||% list()
  if (!is.list(continuation_control)) {
    stop("The private DLM continuation control must be a list.",
         call. = FALSE)
  }
  .rqr_validate_named_list_fields(
    continuation_control, "init$continuation_control",
    c(
      "parent_schedule_contract", "parent_schedule_digest"
    )
  )
  parent_chain_numerically_exact <- .rqr_scalar_logical(
    init$parent_chain_history_numerically_exact %||% TRUE,
    "init$parent_chain_history_numerically_exact"
  )
  parent_promotion_eligible_input <- .rqr_scalar_logical(
    init$parent_promotion_eligible %||%
      if (continued_from_checkpoint) FALSE else TRUE,
    "init$parent_promotion_eligible"
  )
  completed_offset_input <- .rqr_history_count(
    init$completed_iterations %||% 0L,
    "init$completed_iterations"
  )
  parent_cumulative_repairs_input <- .rqr_history_count(
    init$parent_cumulative_numerical_repair_count %||% 0L,
    "init$parent_cumulative_numerical_repair_count"
  )
  if (!continued_from_checkpoint &&
      any(c(
        "completed_iterations",
        "parent_cumulative_numerical_repair_count",
        "parent_chain_history_numerically_exact",
        "parent_promotion_eligible"
      ) %in% names(init))) {
    stop(
      paste(
        "Continuation-history init fields require",
        "init$continued_from_checkpoint=TRUE."
      ),
      call. = FALSE
    )
  }
  if (!is.list(mcmc_control)) {
    stop("mcmc_control must be a list.", call. = FALSE)
  }
  .rqr_validate_named_list_fields(
    mcmc_control, "mcmc_control",
    c(
      "n_burn", "n_mcmc", "thin", "seed", "backend",
      "store_state_draws", "store_latent_draws", "verbose",
      "progress_every", "jitter_ladder",
      "component_scale_collapsed_update",
      "component_scale_interweave",
      "component_scale_slice_width",
      "component_scale_slice_max_steps",
      "component_scale_slice_max_shrink",
      "component_scale_slice_sweeps",
      "component_scale_interweave_cycles"
    )
  )
  model <- rqr_as_dlm_model(model)
  expanded <- .rqr_expand_model(model, length(y))
  T <- length(y)
  p <- expanded$p
  observed <- !is.na(y)
  n_obs <- sum(observed)
  numerical_policy <- .rqr_numerical_policy(numerical_policy)
  provenance_control <- .rqr_provenance_control(provenance_control)
  jitter_ladder <- mcmc_control$jitter_ladder %||%
    c(0, 1e-12, 1e-10, 1e-8, 1e-6)
  if (!is.numeric(jitter_ladder) || is.object(jitter_ladder) ||
      !is.null(dim(jitter_ladder)) || !length(jitter_ladder) ||
      any(!is.finite(jitter_ladder)) || any(jitter_ladder < 0)) {
    stop(
      paste(
        "mcmc_control$jitter_ladder must be a nonempty plain numeric",
        "vector of finite nonnegative values."
      ),
      call. = FALSE
    )
  }
  jitter_ladder <- as.numeric(jitter_ladder)
  jitter_ladder <- .rqr_jitter_ladder(
    numerical_policy, jitter_ladder
  )

  learning_rate_mode <- .rqr_learning_rate_mode(learning_rate_mode)
  lambda_prior <- .rqr_lambda_prior(lambda_prior, learning_rate_mode)
  loss_reference_scale <- .rqr_scalar_numeric(
    loss_reference_scale, "loss_reference_scale",
    lower = 0, lower_open = TRUE
  )
  learning_rate <- .rqr_scalar_numeric(
    learning_rate, "learning_rate",
    lower = 0, lower_open = TRUE
  )
  lambda_initial <- .rqr_scalar_numeric(
    init$lambda %||% lambda_initial, "lambda_initial",
    lower = 0, lower_open = TRUE
  )
  learn_lambda <- learning_rate_mode != "fixed_rate"
  lambda <- if (learn_lambda) lambda_initial else learning_rate * loss_reference_scale
  constants <- rqr_constants(coverage_level, lambda / loss_reference_scale)

  requested_mode <- if (is.null(evolution_spec)) match.arg(evolution_mode) else evolution_spec$mode
  evolution <- .rqr_dlm_evolution(
    requested_mode, model, expanded, y, W, df, dim.df,
    reference_variance, reference_design, component_templates,
    evolution_scale_prior, evolution_scale_initial, evolution_spec,
    numerical_policy, jitter_ladder
  )
  evolution_mode <- evolution$mode
  evolution_properties <- .rqr_validate_dlm_evolution_spec(
    evolution, expanded, y = y
  )
  if (!evolution_properties$exact_joint_target) {
    warning(
      paste(
        "adaptive_discount is an experimental working/sequential recursion,",
        "not an exact Gibbs sampler for a declared fixed joint target."
      ),
      call. = FALSE
    )
  }

  n_burn <- .rqr_scalar_integer(mcmc_control$n_burn %||% 500L, "mcmc_control$n_burn", 0L)
  n_keep <- .rqr_scalar_integer(mcmc_control$n_mcmc %||% 1000L, "mcmc_control$n_mcmc", 1L)
  thin <- .rqr_scalar_integer(mcmc_control$thin %||% 1L, "mcmc_control$thin", 1L)
  seed <- mcmc_control$seed %||% NULL
  if (!is.null(seed)) {
    seed <- .rqr_scalar_integer(seed, "mcmc_control$seed", 0L)
  }
  if (!is.null(seed) && !is.null(init$rng_state)) {
    stop(
      "Supply mcmc_control$seed or init$rng_state, not both.",
      call. = FALSE
    )
  }
  backend_requested <- mcmc_control$backend %||% "cpp"
  if (!is.character(backend_requested) ||
      length(backend_requested) != 1L ||
      is.na(backend_requested) ||
      !backend_requested %in% c("cpp", "R", "auto")) {
    stop(
      "mcmc_control$backend must be exactly one of 'cpp', 'R', or 'auto'.",
      call. = FALSE
    )
  }
  backend_resolved <- .rqr_resolve_ffbs_backend(backend_requested)
  store_state_draws <- .rqr_scalar_logical(
    mcmc_control$store_state_draws %||% FALSE,
    "mcmc_control$store_state_draws"
  )
  store_latent_draws <- .rqr_scalar_logical(
    mcmc_control$store_latent_draws %||% FALSE,
    "mcmc_control$store_latent_draws"
  )
  verbose <- .rqr_scalar_logical(
    mcmc_control$verbose %||% FALSE,
    "mcmc_control$verbose"
  )
  progress_every <- .rqr_scalar_integer(
    mcmc_control$progress_every %||% 100L, "mcmc_control$progress_every", 1L
  )

  paths <- .rqr_init_state_paths(y, expanded$FF, expanded$m0, coverage_level, init)
  # Publicly supplied state paths may carry descriptive dimnames. The Markov
  # state contract is positional and must be byte-canonical, so retain the
  # validated values and dimensions while dropping nonsemantic attributes.
  theta1 <- matrix(
    as.numeric(paths$theta1), nrow = p, ncol = T
  )
  theta2 <- matrix(
    as.numeric(paths$theta2), nrow = p, ncol = T
  )
  # Missing observations contribute no augmented measurement. Canonicalize
  # their latent placeholders to the exact pseudo-AL inverse rate so arbitrary
  # missing-site inputs cannot alter initialization or continuation. A prior
  # implementation also relied on an unparenthesized `%||%`/division
  # expression, which divided restored learned-scale checkpoints a second
  # time. Explicit branching prevents both classes of defect.
  v_initial <- init$latent_v
  if (is.null(v_initial)) {
    v <- rep(constants$sigma, T)
  } else {
    if (!is.numeric(v_initial) || is.object(v_initial) ||
        !is.null(dim(v_initial)) ||
        !length(v_initial) %in% unique(c(1L, n_obs, T))) {
      stop(
        paste(
          "init$latent_v must be a plain numeric vector that is scalar,",
          "length n_observed, or length(y)."
        ),
        call. = FALSE
      )
    }
    v_initial <- as.numeric(v_initial)
    if (length(v_initial) == n_obs && n_obs != T) {
      v <- rep(constants$sigma, T)
      v[observed] <- v_initial
    } else {
      v <- rep_len(v_initial, T)
      v[!observed] <- constants$sigma
    }
    if (any(!is.finite(v[observed])) ||
        any(v[observed] <= 0)) {
      stop(
        "Observed-site init$latent_v values must be finite and positive.",
        call. = FALSE
      )
    }
  }
  component_mode <- identical(evolution_mode, "component_scale")
  component_scale_interweave <- .rqr_scalar_logical(
    mcmc_control$component_scale_interweave %||% FALSE,
    "mcmc_control$component_scale_interweave"
  )
  if (isTRUE(component_scale_interweave) && !component_mode) {
    stop(
      "Component-scale interweaving requires evolution_mode='component_scale'.",
      call. = FALSE
    )
  }
  component_scale_collapsed_update <- .rqr_scalar_logical(
    mcmc_control$component_scale_collapsed_update %||% FALSE,
    "mcmc_control$component_scale_collapsed_update"
  )
  if (isTRUE(component_scale_collapsed_update) && !component_mode) {
    stop(
      paste(
        "The partially collapsed scale update requires",
        "evolution_mode='component_scale'."
      ),
      call. = FALSE
    )
  }
  component_scale_slice_width <- .rqr_scalar_numeric(
    mcmc_control$component_scale_slice_width %||% 1,
    "mcmc_control$component_scale_slice_width",
    lower = 0, lower_open = TRUE
  )
  component_scale_slice_max_steps <- .rqr_scalar_integer(
    mcmc_control$component_scale_slice_max_steps %||% 100L,
    "mcmc_control$component_scale_slice_max_steps", 1L
  )
  component_scale_slice_max_shrink <- .rqr_scalar_integer(
    mcmc_control$component_scale_slice_max_shrink %||% 1000L,
    "mcmc_control$component_scale_slice_max_shrink", 1L
  )
  component_scale_slice_sweeps <- .rqr_scalar_integer(
    mcmc_control$component_scale_slice_sweeps %||% 1L,
    "mcmc_control$component_scale_slice_sweeps", 1L
  )
  component_scale_interweave_cycles <- .rqr_scalar_integer(
    mcmc_control$component_scale_interweave_cycles %||% 1L,
    "mcmc_control$component_scale_interweave_cycles", 1L
  )
  transition_kernel_contract <- .rqr_dlm_transition_kernel_contract(
    evolution_mode = evolution_mode,
    learning_rate_mode = learning_rate_mode,
    complete_time0_state = component_mode ||
      evolution_mode %in% c("fixed_W", "discount_template"),
    component_names = evolution$component_names %||% character(),
    component_scale_collapsed_update = component_scale_collapsed_update,
    component_scale_interweave = component_scale_interweave,
    component_scale_interweave_cycles = component_scale_interweave_cycles,
    component_scale_slice_width = component_scale_slice_width,
    component_scale_slice_sweeps = component_scale_slice_sweeps,
    component_scale_slice_max_steps = component_scale_slice_max_steps,
    component_scale_slice_max_shrink = component_scale_slice_max_shrink
  )
  transition_kernel_digest <- .rqr_digest(
    transition_kernel_contract
  )
  time0_completion_mode <- component_mode ||
    evolution_mode %in% c("fixed_W", "discount_template")
  q_evolution <- if (component_mode) {
    init$evolution_scale %||% evolution$initial
  } else {
    numeric(0)
  }
  if (component_mode &&
      (!is.numeric(q_evolution) || is.object(q_evolution) ||
       !is.null(dim(q_evolution)) ||
       length(q_evolution) != length(evolution$component_dims) ||
       any(!is.finite(q_evolution)) || any(q_evolution <= 0))) {
    stop(
      paste(
        "Initial component evolution scales must be a plain numeric,",
        "finite, positive vector with one value per component."
      ),
      call. = FALSE
    )
  }
  q_evolution <- as.numeric(q_evolution)
  theta01 <- init$theta0_root1 %||% expanded$m0
  theta02 <- init$theta0_root2 %||% expanded$m0
  if (!is.numeric(theta01) || is.object(theta01) ||
      !is.null(dim(theta01)) ||
      !is.numeric(theta02) || is.object(theta02) ||
      !is.null(dim(theta02)) ||
      length(theta01) != p || length(theta02) != p ||
      any(!is.finite(theta01)) || any(!is.finite(theta02))) {
    stop(
      paste(
        "Initial time-zero states must be plain numeric finite vectors",
        "of state dimension p."
      ),
      call. = FALSE
    )
  }
  theta01 <- as.numeric(theta01)
  theta02 <- as.numeric(theta02)
  initialization_contract <- .rqr_make_initialization_contract(
    family = "rqr_dlm",
    initial_state = list(
      theta_root1 = theta1,
      theta_root2 = theta2,
      theta0_root1 = theta01,
      theta0_root2 = theta02,
      latent_v = v,
      lambda = lambda,
      evolution_scale = q_evolution
    ),
    seed = seed,
    rng_state = init$rng_state %||% NULL,
    continued = continued_from_checkpoint,
    parent_checkpoint_digest = if (
        continued_from_checkpoint) {
      utils::tail(
        continuation_control$parent_schedule_contract$segments,
        1L
      )[[1L]]$checkpoint_digest
    } else {
      NULL
    }
  )
  initialization_digest <- .rqr_digest(
    initialization_contract
  )
  if (!is.null(seed)) {
    set.seed(seed)
  } else {
    .rqr_restore_rng(init$rng_state %||% NULL)
  }

  eta1_draws <- matrix(NA_real_, T, n_keep)
  eta2_draws <- matrix(NA_real_, T, n_keep)
  theta1_draws <- if (store_state_draws) array(NA_real_, c(p, T, n_keep)) else NULL
  theta2_draws <- if (store_state_draws) array(NA_real_, c(p, T, n_keep)) else NULL
  terminal1_draws <- terminal2_draws <- matrix(NA_real_, p, n_keep)
  v_draws <- if (store_latent_draws) matrix(NA_real_, T, n_keep) else NULL
  lambda_draws <- numeric(n_keep)
  q_draws <- if (component_mode) {
    matrix(NA_real_, n_keep, length(q_evolution), dimnames = list(NULL, evolution$component_names))
  } else NULL
  q_shape_draws <- if (component_mode) {
    matrix(
      NA_real_, n_keep, length(q_evolution),
      dimnames = list(NULL, evolution$component_names)
    )
  } else NULL
  q_rate_draws <- if (component_mode) {
    matrix(
      NA_real_, n_keep, length(q_evolution),
      dimnames = list(NULL, evolution$component_names)
    )
  } else NULL
  theta01_draws <- theta02_draws <- if (
      time0_completion_mode
    ) {
    matrix(NA_real_, p, n_keep)
  } else {
    NULL
  }

  total_iter <- n_burn + n_keep * thin
  loss_trace <- lambda_trace <- effective_rate_trace <- numeric(total_iter)
  lambda_shape_trace <- lambda_rate_trace <- rep(NA_real_, total_iter)
  root_swap_trace <- logical(total_iter)
  ffbs_iteration <- vector("list", 2L * total_iter)
  component_scale_interweave_iteration <- if (
      component_mode && isTRUE(component_scale_interweave)
    ) {
    vector("list", total_iter)
  } else {
    NULL
  }
  component_scale_collapsed_iteration <- if (
      component_mode && isTRUE(component_scale_collapsed_update)
    ) {
    vector("list", total_iter)
  } else {
    NULL
  }
  repair_records <- NULL
  save_idx <- 0L

  for (iter in seq_len(total_iter)) {
    eta1 <- .rqr_state_ordinates(expanded$FF, theta1)
    eta2 <- .rqr_state_ordinates(expanded$FF, theta2)
    e <- rqr_residual_product(y[observed], eta1[observed], eta2[observed])
    loss_current <- sum(rqr_check_loss(e, constants$alpha))
    if (learn_lambda) {
      lp <- .rqr_lambda_posterior_params(
        loss_sum = loss_current / loss_reference_scale,
        n = n_obs, lambda_prior = lambda_prior,
        learning_rate_mode = learning_rate_mode
      )
      lambda_shape_trace[iter] <- lp$shape
      lambda_rate_trace[iter] <- lp$rate
      lambda <- stats::rgamma(1L, shape = lp$shape, rate = lp$rate)
      constants <- rqr_constants(coverage_level, lambda / loss_reference_scale)
    }

    # Full latent-scale refresh immediately after the collapsed lambda draw.
    gp <- rqr_gig_params(e, coverage_level, constants$omega)
    v[observed] <- rqr_sample_gig_half(gp$b, gp$a)
    v[!observed] <- loss_reference_scale / lambda
    obs_variance <- constants$phi * loss_reference_scale * v / lambda
    H1 <- sweep(expanded$FF, 2L, y - eta2, `*`)
    H1[, !observed] <- 0
    z1 <- y * (y - eta2) - constants$xi * v
    z1[!observed] <- NA_real_
    if (component_mode && isTRUE(component_scale_collapsed_update)) {
      collapsed_update <- .rqr_collapsed_component_scale_update(
        conditioned_theta = theta2,
        conditioned_theta0 = theta02,
        z = z1, H = H1, obs_variance = obs_variance,
        GG = expanded$GG, m0 = expanded$m0, C0 = expanded$C0,
        evolution = evolution, q = q_evolution,
        backend = backend_resolved,
        width = component_scale_slice_width,
        sweeps = component_scale_slice_sweeps,
        max_steps = component_scale_slice_max_steps,
        max_shrink = component_scale_slice_max_shrink
      )
      q_evolution <- collapsed_update$q
      component_scale_collapsed_iteration[[iter]] <- data.frame(
        iteration = iter,
        component = evolution$component_names,
        evaluations = collapsed_update$diagnostics$evaluations,
        shrink_steps = collapsed_update$diagnostics$shrink_steps,
        sweeps = collapsed_update$diagnostics$sweeps,
        integrated_root = "root1",
        conditioned_root = "root2",
        mathematically_exact_partially_collapsed =
          collapsed_update$diagnostics$
            mathematically_exact_partially_collapsed,
        numerically_exact_partially_collapsed =
          collapsed_update$diagnostics$
            numerically_exact_partially_collapsed,
        numerical_repair_count =
          collapsed_update$diagnostics$numerical_repair_count,
        exact_partially_collapsed =
          collapsed_update$diagnostics$exact_partially_collapsed,
        stringsAsFactors = FALSE
      )
      repair_records <- .rqr_bind_ffbs_repairs(
        repair_records,
        list(
          repair_records =
            collapsed_update$diagnostics$numerical_repair_records
        ),
        iter, "collapsed_q"
      )
    }
    evolution_iter <- if (component_mode) {
      .rqr_materialize_component_evolution(evolution, q_evolution, T, p)
    } else evolution
    draw1 <- rqr_ffbs_sample(
      z1, H1, obs_variance, expanded$GG, expanded$m0, expanded$C0,
      evolution_iter, backend = backend_resolved, jitter_ladder = jitter_ladder,
      numerical_policy = numerical_policy
    )
    theta1 <- draw1$path
    if (time0_completion_mode) {
      # Complete root 1 before drawing root 2. This ordering is part of the
      # digested transition/RNG contract for every exact fixed-joint mode.
      time0_root1 <- .rqr_draw_initial_state(
        theta1[, 1L], expanded$GG[, , 1L], expanded$m0, expanded$C0,
        evolution_iter$W[, , 1L],
        numerical_policy = numerical_policy,
        jitter_ladder = jitter_ladder
      )
      theta01 <- time0_root1$draw
      repair_records <- .rqr_bind_ffbs_repairs(
        repair_records, time0_root1$diagnostics, iter, "root1"
      )
    }
    eta1 <- .rqr_state_ordinates(expanded$FF, theta1)

    H2 <- sweep(expanded$FF, 2L, y - eta1, `*`)
    H2[, !observed] <- 0
    z2 <- y * (y - eta1) - constants$xi * v
    z2[!observed] <- NA_real_
    draw2 <- rqr_ffbs_sample(
      z2, H2, obs_variance, expanded$GG, expanded$m0, expanded$C0,
      evolution_iter, backend = backend_resolved, jitter_ladder = jitter_ladder,
      numerical_policy = numerical_policy
    )
    theta2 <- draw2$path
    if (component_mode) {
      time0_root2 <- .rqr_draw_initial_state(
        theta2[, 1L], expanded$GG[, , 1L], expanded$m0, expanded$C0,
        evolution_iter$W[, , 1L],
        numerical_policy = numerical_policy,
        jitter_ladder = jitter_ladder
      )
      theta02 <- time0_root2$draw
      repair_records <- .rqr_bind_ffbs_repairs(
        repair_records, time0_root2$diagnostics, iter, "root2"
      )
      if (isTRUE(component_scale_interweave)) {
        iteration_interweave <- vector(
          "list", component_scale_interweave_cycles
        )
        for (cycle in seq_len(component_scale_interweave_cycles)) {
          q_update <- .rqr_sample_component_scales(
            theta1, theta2, theta01, theta02, expanded$GG, evolution
          )
          q_evolution <- q_update$draw
          interweave <- .rqr_interweave_component_scales(
            theta1 = theta1, theta2 = theta2,
            theta01 = theta01, theta02 = theta02,
            GG = expanded$GG, FF = expanded$FF,
            y = y, observed = observed, v = v,
            xi = constants$xi, obs_variance = obs_variance,
            evolution = evolution, q = q_evolution,
            width = component_scale_slice_width,
            sweeps = component_scale_slice_sweeps,
            max_steps = component_scale_slice_max_steps,
            max_shrink = component_scale_slice_max_shrink
          )
          q_evolution <- interweave$q
          theta1 <- interweave$theta1
          theta2 <- interweave$theta2
          iteration_interweave[[cycle]] <- data.frame(
            iteration = iter,
            cycle = cycle,
            component = evolution$component_names,
            evaluations = interweave$diagnostics$evaluations,
            shrink_steps = interweave$diagnostics$shrink_steps,
            sweeps_per_cycle = interweave$diagnostics$sweeps,
            exact_noncentered_slice = TRUE,
            stringsAsFactors = FALSE
          )
        }
        q_update <- list(
          draw = q_evolution,
          posterior = .rqr_component_scale_posterior(
            theta1, theta2, theta01, theta02, expanded$GG, evolution
          )
        )
        component_scale_interweave_iteration[[iter]] <-
          do.call(rbind, iteration_interweave)
      } else {
        q_update <- .rqr_sample_component_scales(
          theta1, theta2, theta01, theta02, expanded$GG, evolution
        )
        q_evolution <- q_update$draw
      }
    } else if (time0_completion_mode) {
      # Fixed-W and frozen-template FFBS integrate theta_0 out through
      # (m0, C0). Root 1 was completed before the root-2 FFBS draw; complete
      # root 2 now so execution agrees with the declared blocked scan.
      time0_root2 <- .rqr_draw_initial_state(
        theta2[, 1L], expanded$GG[, , 1L], expanded$m0, expanded$C0,
        evolution_iter$W[, , 1L],
        numerical_policy = numerical_policy,
        jitter_ladder = jitter_ladder
      )
      theta02 <- time0_root2$draw
      repair_records <- .rqr_bind_ffbs_repairs(
        repair_records, time0_root2$diagnostics, iter, "root2"
      )
    }

    if (stats::runif(1L) < 0.5) {
      tmp <- theta1; theta1 <- theta2; theta2 <- tmp
      tmp <- theta01; theta01 <- theta02; theta02 <- tmp
      root_swap_trace[iter] <- TRUE
    }
    eta1 <- .rqr_state_ordinates(expanded$FF, theta1)
    eta2 <- .rqr_state_ordinates(expanded$FF, theta2)

    loss_trace[iter] <- sum(rqr_check_loss(
      rqr_residual_product(y[observed], eta1[observed], eta2[observed]), constants$alpha
    ))
    lambda_trace[iter] <- lambda
    effective_rate_trace[iter] <- lambda / loss_reference_scale
    repair_records <- .rqr_bind_ffbs_repairs(repair_records, draw1$diagnostics, iter, "root1")
    repair_records <- .rqr_bind_ffbs_repairs(repair_records, draw2$diagnostics, iter, "root2")
    ffbs_iteration[[2L * iter - 1L]] <- data.frame(
      iteration = iter, root = "root1",
      jitter_count = draw1$diagnostics$jitter_count,
      repair_count = draw1$diagnostics$repair_count,
      psd_draw_count = draw1$diagnostics$psd_draw_count,
      roundoff_psd_count = draw1$diagnostics$roundoff_psd_count,
      min_forecast_variance = draw1$diagnostics$min_forecast_variance
    )
    ffbs_iteration[[2L * iter]] <- data.frame(
      iteration = iter, root = "root2",
      jitter_count = draw2$diagnostics$jitter_count,
      repair_count = draw2$diagnostics$repair_count,
      psd_draw_count = draw2$diagnostics$psd_draw_count,
      roundoff_psd_count = draw2$diagnostics$roundoff_psd_count,
      min_forecast_variance = draw2$diagnostics$min_forecast_variance
    )

    if (iter > n_burn && (iter - n_burn) %% thin == 0L) {
      save_idx <- save_idx + 1L
      eta1_draws[, save_idx] <- eta1
      eta2_draws[, save_idx] <- eta2
      terminal1_draws[, save_idx] <- theta1[, T]
      terminal2_draws[, save_idx] <- theta2[, T]
      lambda_draws[save_idx] <- lambda
      if (store_state_draws) {
        theta1_draws[, , save_idx] <- theta1
        theta2_draws[, , save_idx] <- theta2
      }
      if (!is.null(theta01_draws)) {
        theta01_draws[, save_idx] <- theta01
        theta02_draws[, save_idx] <- theta02
      }
      if (store_latent_draws) v_draws[, save_idx] <- v
      if (component_mode) {
        q_draws[save_idx, ] <- q_evolution
        q_shape_draws[save_idx, ] <- q_update$posterior$shape
        q_rate_draws[save_idx, ] <- q_update$posterior$rate
      }
    }
    if (verbose && (iter %% progress_every == 0L || iter == total_iter)) {
      message(sprintf("[rqr_dlm_fit] iter %d/%d loss=%.6g", iter, total_iter, loss_trace[iter]))
    }
  }

  lower <- pmin(eta1_draws, eta2_draws)
  upper <- pmax(eta1_draws, eta2_draws)
  lambda_summary <- .rqr_lambda_summary(lambda_draws)
  template_repairs <- if (continued_from_checkpoint) {
    0L
  } else {
    .rqr_history_count(
      evolution$construction_audit$repair_count %||% 0L,
      "evolution construction repair count"
    )
  }
  mcmc_repairs <- .rqr_history_count(
    if (is.null(repair_records)) 0L else nrow(repair_records),
    "MCMC repair count"
  )
  segment_repairs <- .rqr_history_count(
    as.double(template_repairs) + as.double(mcmc_repairs),
    "segment repair count"
  )
  numerical_exact <- segment_repairs == 0L
  parent_cumulative_repairs <- .rqr_history_count(
    parent_cumulative_repairs_input,
    "parent cumulative repair count"
  )
  cumulative_repairs <- .rqr_history_count(
    as.double(parent_cumulative_repairs) + as.double(segment_repairs),
    "cumulative repair count"
  )
  chain_history_numerically_exact <-
    parent_chain_numerically_exact && numerical_exact
  parent_promotion_eligible <- if (continued_from_checkpoint) {
    parent_promotion_eligible_input
  } else {
    TRUE
  }
  mathematical_exact <- evolution_properties$exact_joint_target
  rng_state <- .rqr_rng_state()
  completed_offset <- .rqr_history_count(
    completed_offset_input,
    "completed_iterations"
  )
  completed_iterations <- .rqr_history_count(
    as.double(completed_offset) + as.double(total_iter),
    "cumulative completed_iterations"
  )
  target_contract <- .rqr_dlm_target_contract(
    coverage_level = constants$alpha,
    learning_rate_mode = learning_rate_mode,
    fixed_learning_rate = if (learn_lambda) NA_real_ else learning_rate,
    loss_reference_scale = loss_reference_scale,
    lambda_prior = lambda_prior,
    numerical_policy = numerical_policy,
    jitter_ladder = jitter_ladder
  )
  provenance <- .rqr_provenance(
    data = list(y = y),
    matrices = .rqr_dlm_provenance_matrices(expanded, evolution),
    numerical_policy = numerical_policy,
    initial_seed = seed,
    repo_root = provenance_control$repo_root,
    expected_git_commit = provenance_control$expected_git_commit,
    backend = backend_resolved,
    backend_requested = backend_requested,
    backend_resolved = backend_resolved,
    objects = .rqr_dlm_provenance_objects(
      expanded, evolution, target_contract
    ),
    external_repositories = provenance_control$external_repositories,
    required_external_repositories =
      provenance_control$required_external_repositories,
    primary_runtime_attestation =
      provenance_control$primary_runtime_attestation,
    initialization_contract = initialization_contract
  )
  segment_target_numerical_eligible <- mathematical_exact && numerical_exact
  target_numerical_eligible <- mathematical_exact &&
    chain_history_numerically_exact
  ordinary_v1_scope_eligible <-
    evolution_properties$ordinary_v1_evolution &&
    !identical(learning_rate_mode, "learned_pure")
  checkpoint <- list(
    schema_version = provenance$schema_version,
    completed_iterations = completed_iterations,
    theta_root1 = theta1,
    theta_root2 = theta2,
    theta0_root1 = theta01,
    theta0_root2 = theta02,
    latent_v = v,
    lambda = lambda,
    evolution_scale = q_evolution,
    transition_kernel_schema =
      transition_kernel_contract$schema_version,
    transition_kernel = transition_kernel_contract,
    transition_kernel_digest = transition_kernel_digest,
    rng_state = rng_state
  )
  checkpoint_digest <- .rqr_digest(checkpoint)
  segment_schedule_contract <- .rqr_make_dlm_schedule_contract(
    start_completed_iterations = completed_offset,
    n_burn = n_burn,
    n_retained_draws = n_keep,
    thin = thin,
    initialization_contract = initialization_contract,
    initialization_digest = initialization_digest,
    checkpoint_digest = checkpoint_digest,
    checkpoint_state = checkpoint,
    parent =
      continuation_control$parent_schedule_contract %||% NULL,
    parent_digest =
      continuation_control$parent_schedule_digest %||% NULL
  )
  segment_schedule_digest <- .rqr_digest(
    segment_schedule_contract
  )
  out <- list(
    method = "mcmc_ffbs",
    family = "rqr_dlm",
    model = model,
    expanded_model = expanded,
    evolution = evolution,
    y = y,
    model_spec = list(
      family = "rqr_dlm",
      parameterization = "exchangeable_dynamic_roots",
      target_schema_version = .rqr_dlm_target_schema(),
      tilt = 0,
      loss_name = "rqr_residual_product_check_loss",
      state_model = "linear_gaussian_interval_root_evolution",
      coverage_level = constants$alpha,
      learning_rate_mode = learning_rate_mode,
      fixed_learning_rate = if (learn_lambda) NA_real_ else learning_rate,
      learning_rate = if (learn_lambda) mean(lambda_draws / loss_reference_scale) else learning_rate,
      lambda_initial = initialization_contract$initial_state$lambda,
      loss_reference_scale = loss_reference_scale,
      effective_learning_rate = mean(lambda_draws / loss_reference_scale),
      lambda_prior = lambda_prior,
      lambda_summary = lambda_summary,
      inferential_target = .rqr_target_formula(learning_rate_mode),
      generalized_bayes = TRUE,
      response_likelihood = FALSE,
      response_prediction_contract = FALSE,
      evolution_mode = evolution_mode,
      transition_kernel = transition_kernel_contract,
      transition_kernel_digest = transition_kernel_digest,
      target_contract = if (mathematical_exact) "fixed_joint_exact" else "working_sequential",
      exact_joint_target = mathematical_exact,
      ordinary_v1_scope_eligible = ordinary_v1_scope_eligible,
      continuation_supported = ordinary_v1_scope_eligible,
      numerical_policy = numerical_policy,
      numerical_repair_count = segment_repairs,
      cumulative_numerical_repair_count = cumulative_repairs,
      numerically_exact_transition = numerical_exact,
      chain_history_numerically_exact = chain_history_numerically_exact,
      parent_chain_history_numerically_exact =
        parent_chain_numerically_exact,
      segment_target_numerical_eligible =
        segment_target_numerical_eligible,
      target_numerical_eligible = target_numerical_eligible,
      reproducibility_eligible = provenance$reproducibility_eligible,
      parent_promotion_eligible = parent_promotion_eligible,
      promotion_eligible = ordinary_v1_scope_eligible &&
        target_numerical_eligible &&
        parent_promotion_eligible && provenance$reproducibility_eligible,
      root_priors_exchangeable = TRUE,
      root_swap_move = TRUE
    ),
    samp.eta_root1 = eta1_draws,
    samp.eta_root2 = eta2_draws,
    samp.theta_root1 = theta1_draws,
    samp.theta_root2 = theta2_draws,
    samp.theta_terminal_root1 = terminal1_draws,
    samp.theta_terminal_root2 = terminal2_draws,
    samp.theta0_root1 = theta01_draws,
    samp.theta0_root2 = theta02_draws,
    samp.lambda = lambda_draws,
    samp.latent_v = v_draws,
    samp.evolution_scale = q_draws,
    samp.evolution_scale_shape = q_shape_draws,
    samp.evolution_scale_rate = q_rate_draws,
    summary = .rqr_dlm_coverage_summary(y, observed, lower, upper),
    diagnostics = list(
      loss_trace = loss_trace,
      scaled_loss_trace = loss_trace / loss_reference_scale,
      lambda_trace = lambda_trace,
      effective_learning_rate_trace = effective_rate_trace,
      lambda_post_shape_trace = lambda_shape_trace,
      lambda_post_rate_trace = lambda_rate_trace,
      root_swap_trace = root_swap_trace,
      ffbs_iteration = do.call(rbind, ffbs_iteration),
      numerical_repairs =
        repair_records %||% .rqr_empty_dlm_repair_records(),
      component_scale_interweave = if (
          is.null(component_scale_interweave_iteration)) {
        data.frame()
      } else {
        do.call(rbind, component_scale_interweave_iteration)
      },
      component_scale_collapsed = if (
          is.null(component_scale_collapsed_iteration)) {
        data.frame()
      } else {
        do.call(rbind, component_scale_collapsed_iteration)
      },
      template_construction_audit = evolution$construction_audit %||% NULL,
      partial_collapse_order = transition_kernel_contract$scan_order
    ),
    initialization_contract = initialization_contract,
    initialization_digest = initialization_digest,
    provenance = provenance,
    checkpoint_state = checkpoint,
    checkpoint_digest = checkpoint_digest,
    last = checkpoint,
    segment_schedule_contract = segment_schedule_contract,
    segment_schedule_digest = segment_schedule_digest,
    misc = list(
      n_burn = n_burn, n_mcmc = n_keep, thin = thin, seed = seed,
      backend = backend_resolved,
      backend_requested = backend_requested,
      backend_resolved = backend_resolved,
      observed = observed,
      store_state_draws = store_state_draws,
      store_latent_draws = store_latent_draws,
      jitter_ladder = jitter_ladder,
      component_scale_collapsed_update =
        component_scale_collapsed_update,
      component_scale_interweave = component_scale_interweave,
      component_scale_interweave_cycles =
        component_scale_interweave_cycles,
      component_scale_slice_width = component_scale_slice_width,
      component_scale_slice_sweeps = component_scale_slice_sweeps,
      component_scale_slice_max_steps =
        component_scale_slice_max_steps,
      component_scale_slice_max_shrink =
        component_scale_slice_max_shrink,
      note = paste(
        "Root trajectory draws arise from a generalized-Bayes loss update;",
        "they are not response draws."
      )
    )
  )
  out$retained_draws_contract <- .rqr_make_retained_draws_contract(
    family = "rqr_dlm",
    draws = .rqr_dlm_retained_draws(out)
  )
  out$retained_draws_digest <- .rqr_digest(
    out$retained_draws_contract
  )
  out$retained_evidence_contract <-
    .rqr_make_retained_evidence_contract(
      family = "rqr_dlm",
      evidence = .rqr_dlm_retained_evidence(out)
    )
  out$retained_evidence_digest <- .rqr_digest(
    out$retained_evidence_contract
  )
  if (ordinary_v1_scope_eligible) {
    out$continuation_history_contract <- .rqr_make_continuation_history(
      checkpoint_digest = checkpoint_digest,
      segment_numerical_repair_count =
        out$model_spec$numerical_repair_count,
      segment_exact_joint_target =
        out$model_spec$exact_joint_target,
      segment_environment_base_eligible =
        out$provenance$reproducibility_eligible,
      segment_target_contract_digest =
        out$provenance$object_digests$target,
      backend_requested = out$provenance$backend_requested,
      backend_resolved = out$provenance$backend_resolved,
      segment_transition_kernel_schema =
        transition_kernel_contract$schema_version,
      segment_transition_kernel_digest =
        transition_kernel_digest
    )
    out$continuation_history_digest <- .rqr_digest(
      out$continuation_history_contract
    )
  } else {
    out["continuation_history_contract"] <- list(NULL)
    out$continuation_history_digest <- NA_character_
    out$model_spec$promotion_eligible <- FALSE
  }
  class(out) <- c("rqr_dlm_mcmc", "rqr_fit")
  if (!continued_from_checkpoint) {
    .rqr_validate_dlm_fit_envelope(out)
  }
  out
}

.rqr_dlm_retained_draws <- function(object) {
  list(
    samp.eta_root1 = object$samp.eta_root1,
    samp.eta_root2 = object$samp.eta_root2,
    samp.theta_root1 = object$samp.theta_root1,
    samp.theta_root2 = object$samp.theta_root2,
    samp.theta_terminal_root1 =
      object$samp.theta_terminal_root1,
    samp.theta_terminal_root2 =
      object$samp.theta_terminal_root2,
    samp.theta0_root1 = object$samp.theta0_root1,
    samp.theta0_root2 = object$samp.theta0_root2,
    samp.lambda = object$samp.lambda,
    samp.latent_v = object$samp.latent_v,
    samp.evolution_scale = object$samp.evolution_scale,
    samp.evolution_scale_shape =
      object$samp.evolution_scale_shape,
    samp.evolution_scale_rate =
      object$samp.evolution_scale_rate
  )
}

.rqr_dlm_retained_evidence <- function(object) {
  list(
    summary = object$summary,
    diagnostics = object$diagnostics,
    lambda_summary = object$model_spec$lambda_summary
  )
}

.rqr_validate_dlm_initialization_semantics <- function(
    contract, expanded, y, evolution, target, schedule) {
  state <- contract$initial_state
  p <- expanded$p
  T <- length(y)
  expected_fields <- c(
    "theta_root1", "theta_root2", "theta0_root1",
    "theta0_root2", "latent_v", "lambda", "evolution_scale"
  )
  valid_path <- function(value) {
    is.matrix(value) && is.numeric(value) && !is.object(value) &&
      identical(names(attributes(value)), "dim") &&
      identical(dim(value), c(p, T)) && all(is.finite(value))
  }
  valid_state <- function(value) {
    is.numeric(value) && !is.object(value) &&
      is.null(attributes(value)) &&
      is.null(dim(value)) && length(value) == p &&
      all(is.finite(value))
  }
  if (!is.list(state) || is.object(state) ||
      !identical(names(state), expected_fields) ||
      !valid_path(state$theta_root1) ||
      !valid_path(state$theta_root2) ||
      !valid_state(state$theta0_root1) ||
      !valid_state(state$theta0_root2) ||
      !is.numeric(state$latent_v) ||
      is.object(state$latent_v) ||
      !is.null(attributes(state$latent_v)) ||
      !is.null(dim(state$latent_v)) ||
      length(state$latent_v) != T ||
      any(!is.finite(state$latent_v)) ||
      any(state$latent_v <= 0) ||
      !is.numeric(state$lambda) ||
      is.object(state$lambda) ||
      !is.null(attributes(state$lambda)) ||
      !is.null(dim(state$lambda)) ||
      length(state$lambda) != 1L ||
      !is.finite(state$lambda) || state$lambda <= 0 ||
      !is.numeric(state$evolution_scale) ||
      is.object(state$evolution_scale) ||
      !is.null(attributes(state$evolution_scale)) ||
      !is.null(dim(state$evolution_scale)) ||
      any(!is.finite(state$evolution_scale))) {
    stop(
      "The DLM initialization state violates its exact schema.",
      call. = FALSE
    )
  }
  component_mode <- identical(evolution$mode, "component_scale")
  expected_q_count <- if (component_mode) {
    length(evolution$component_names)
  } else {
    0L
  }
  if (length(state$evolution_scale) != expected_q_count ||
      (component_mode && any(state$evolution_scale <= 0))) {
    stop(
      "The DLM initialization evolution-scale state is invalid.",
      call. = FALSE
    )
  }
  if (identical(target$learning_rate_mode, "fixed_rate")) {
    expected_lambda <-
      target$fixed_learning_rate * target$loss_reference_scale
    if (!identical(
        as.numeric(state$lambda), as.numeric(expected_lambda)
      )) {
      stop(
        "The DLM fixed-rate initialization lambda is inconsistent.",
        call. = FALSE
      )
    }
  }
  observed <- !is.na(y)
  initial_sigma <- rqr_constants(
    target$coverage_level,
    state$lambda / target$loss_reference_scale
  )$sigma
  if (any(!observed) &&
      !identical(
        as.numeric(state$latent_v[!observed]),
        rep(initial_sigma, sum(!observed))
      )) {
    stop(
      paste(
        "Missing-site DLM initialization latent placeholders are",
        "inconsistent with the initial inverse loss scale."
      ),
      call. = FALSE
    )
  }
  final_segment <- utils::tail(schedule$segments, 1L)[[1L]]
  continued <- final_segment$generation > 0L
  expected_type <- if (continued) "continuation" else "fresh"
  expected_parent <- if (continued) {
    final_segment$parent_checkpoint_digest
  } else {
    NA_character_
  }
  if (!identical(contract$segment_type, expected_type) ||
      !identical(contract$parent_checkpoint_digest, expected_parent)) {
    stop(
      paste(
        "The DLM initialization contract does not match its",
        "segment-schedule parent link."
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.rqr_dlm_checkpoint_initial_state <- function(checkpoint) {
  list(
    theta_root1 = checkpoint$theta_root1,
    theta_root2 = checkpoint$theta_root2,
    theta0_root1 = checkpoint$theta0_root1,
    theta0_root2 = checkpoint$theta0_root2,
    latent_v = checkpoint$latent_v,
    lambda = checkpoint$lambda,
    evolution_scale = checkpoint$evolution_scale
  )
}

.rqr_validate_dlm_schedule_state_chain <- function(
    schedule, expanded, y, evolution, target,
    transition_kernel, transition_kernel_digest,
    current_checkpoint) {
  expected_checkpoint_fields <- c(
    "schema_version", "completed_iterations",
    "theta_root1", "theta_root2", "theta0_root1",
    "theta0_root2", "latent_v", "lambda", "evolution_scale",
    "transition_kernel_schema", "transition_kernel",
    "transition_kernel_digest", "rng_state"
  )
  previous_checkpoint <- NULL
  p <- expanded$p
  T <- length(y)
  valid_path <- function(value) {
    is.matrix(value) && is.numeric(value) && !is.object(value) &&
      identical(names(attributes(value)), "dim") &&
      identical(dim(value), c(p, T)) && all(is.finite(value))
  }
  valid_state <- function(value) {
    is.numeric(value) && !is.object(value) &&
      is.null(attributes(value)) &&
      is.null(dim(value)) && length(value) == p &&
      all(is.finite(value))
  }
  for (index in seq_along(schedule$segments)) {
    segment <- schedule$segments[[index]]
    checkpoint <- segment$checkpoint_state
    if (!is.list(checkpoint) || is.object(checkpoint) ||
        !identical(names(attributes(checkpoint)), "names") ||
        !identical(names(checkpoint), expected_checkpoint_fields) ||
        anyDuplicated(names(checkpoint)) ||
        !identical(
          checkpoint$schema_version, .rqr_schema_version()
        ) ||
        !identical(
          checkpoint$completed_iterations,
          segment$end_completed_iterations
        ) ||
        !valid_path(checkpoint$theta_root1) ||
        !valid_path(checkpoint$theta_root2) ||
        !valid_state(checkpoint$theta0_root1) ||
        !valid_state(checkpoint$theta0_root2) ||
        !is.numeric(checkpoint$latent_v) ||
        is.object(checkpoint$latent_v) ||
        !is.null(attributes(checkpoint$latent_v)) ||
        !is.null(dim(checkpoint$latent_v)) ||
        length(checkpoint$latent_v) != T ||
        any(!is.finite(checkpoint$latent_v)) ||
        any(checkpoint$latent_v <= 0) ||
        !is.numeric(checkpoint$lambda) ||
        is.object(checkpoint$lambda) ||
        !is.null(attributes(checkpoint$lambda)) ||
        !is.null(dim(checkpoint$lambda)) ||
        length(checkpoint$lambda) != 1L ||
        !is.finite(checkpoint$lambda) ||
        checkpoint$lambda <= 0 ||
        !is.numeric(checkpoint$evolution_scale) ||
        is.object(checkpoint$evolution_scale) ||
        !is.null(attributes(checkpoint$evolution_scale)) ||
        !is.null(dim(checkpoint$evolution_scale)) ||
        length(checkpoint$evolution_scale) != if (
          identical(evolution$mode, "component_scale")
        ) {
          length(evolution$component_names)
        } else {
          0L
        } ||
        any(!is.finite(checkpoint$evolution_scale)) ||
        (identical(evolution$mode, "component_scale") &&
         any(checkpoint$evolution_scale <= 0)) ||
        !identical(
          checkpoint$transition_kernel_schema,
          .rqr_dlm_transition_kernel_schema()
        ) ||
        !identical(
          checkpoint$transition_kernel, transition_kernel
        ) ||
        !identical(
          checkpoint$transition_kernel_digest,
          transition_kernel_digest
        )) {
      stop(
        "A retained DLM segment checkpoint is semantically invalid.",
        call. = FALSE
      )
    }
    .rqr_canonical_rng_state(checkpoint$rng_state)
    .rqr_validate_dlm_initialization_semantics(
      segment$initialization_contract,
      expanded = expanded,
      y = y,
      evolution = evolution,
      target = target,
      schedule = .rqr_dlm_schedule_prefix(
        schedule$segments[seq_len(index)]
      )
    )
    if (index > 1L &&
        (!identical(
          segment$initialization_contract$initial_state,
          .rqr_dlm_checkpoint_initial_state(previous_checkpoint)
        ) ||
         !identical(
           segment$initialization_contract$rng_state,
           previous_checkpoint$rng_state
         ))) {
      stop(
        paste(
          "A DLM continuation initialization is not the exact",
          "parent checkpoint Markov state."
        ),
        call. = FALSE
      )
    }
    previous_checkpoint <- checkpoint
  }
  if (!identical(previous_checkpoint, current_checkpoint)) {
    stop(
      "The current DLM checkpoint is not the schedule terminal state.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.rqr_dlm_fit_fields <- function(include_continuation = FALSE) {
  c(
    "method", "family", "model", "expanded_model", "evolution",
    "y", "model_spec", "samp.eta_root1", "samp.eta_root2",
    "samp.theta_root1", "samp.theta_root2",
    "samp.theta_terminal_root1", "samp.theta_terminal_root2",
    "samp.theta0_root1", "samp.theta0_root2", "samp.lambda",
    "samp.latent_v", "samp.evolution_scale",
    "samp.evolution_scale_shape", "samp.evolution_scale_rate",
    "summary", "diagnostics", "initialization_contract",
    "initialization_digest", "provenance", "checkpoint_state",
    "checkpoint_digest", "last", "segment_schedule_contract",
    "segment_schedule_digest", "misc",
    "retained_draws_contract", "retained_draws_digest",
    "retained_evidence_contract", "retained_evidence_digest",
    "continuation_history_contract", "continuation_history_digest",
    if (include_continuation) "continuation_contract"
  )
}

.rqr_validate_dlm_continuation_contract <- function(
    object, schedule, history, object_digests) {
  generation <- schedule$generation
  if (generation == 0L) {
    if ("continuation_contract" %in% names(object)) {
      stop(
        "A fresh DLM fit cannot carry a continuation contract.",
        call. = FALSE
      )
    }
    return(invisible(TRUE))
  }
  expected_fields <- c(
    "continued_from_checkpoint", "parent_checkpoint_digest",
    "parent_completed_iterations",
    "model_target_evolution_digests", "environment_mismatches",
    "environment_override_used", "bitwise_continuation_claim",
    "parent_reproducibility_eligible",
    "parent_target_numerical_eligible",
    "parent_promotion_eligible",
    "parent_chain_history_numerically_exact",
    "parent_cumulative_numerical_repair_count",
    "chain_history_numerically_exact",
    "cumulative_numerical_repair_count", "backend_requested",
    "parent_backend_resolved", "current_backend_resolved",
    "current_environment_reproducibility_eligible"
  )
  contract <- object$continuation_contract
  final_segment <- utils::tail(schedule$segments, 1L)[[1L]]
  parent_segment <- schedule$segments[[length(schedule$segments) - 1L]]
  final_history <- utils::tail(history$segments, 1L)[[1L]]
  parent_history <- history$segments[[length(history$segments) - 1L]]
  logical_fields <- c(
    "environment_override_used", "bitwise_continuation_claim",
    "parent_reproducibility_eligible",
    "parent_target_numerical_eligible",
    "parent_promotion_eligible",
    "parent_chain_history_numerically_exact",
    "chain_history_numerically_exact",
    "current_environment_reproducibility_eligible"
  )
  if (!is.list(contract) || is.object(contract) ||
      !identical(names(attributes(contract)), "names") ||
      !identical(names(contract), expected_fields) ||
      anyDuplicated(names(contract)) ||
      !identical(contract$continued_from_checkpoint, TRUE) ||
      !identical(
        contract$parent_checkpoint_digest,
        final_segment$parent_checkpoint_digest
      ) ||
      !identical(
        contract$parent_completed_iterations,
        parent_segment$end_completed_iterations
      ) ||
      !identical(
        contract$model_target_evolution_digests, object_digests
      ) ||
      !is.character(contract$environment_mismatches) ||
      anyNA(contract$environment_mismatches) ||
      any(!nzchar(contract$environment_mismatches)) ||
      !identical(
        sort(unique(contract$environment_mismatches)),
        final_history$environment_mismatches
      ) ||
      !all(vapply(
        contract[logical_fields],
        function(value) {
          is.logical(value) && length(value) == 1L && !is.na(value)
        },
        logical(1L)
      )) ||
      !identical(
        contract$environment_override_used,
        final_history$environment_override_used
      ) ||
      !identical(
        contract$parent_reproducibility_eligible,
        parent_history$reproducibility_eligible
      ) ||
      !identical(
        contract$parent_target_numerical_eligible,
        parent_history$target_numerical_eligible
      ) ||
      !identical(
        contract$parent_promotion_eligible,
        parent_history$promotion_eligible
      ) ||
      !identical(
        contract$parent_chain_history_numerically_exact,
        parent_history$chain_history_numerically_exact
      ) ||
      !identical(
        contract$parent_cumulative_numerical_repair_count,
        parent_history$cumulative_numerical_repair_count
      ) ||
      !identical(
        contract$chain_history_numerically_exact,
        final_history$chain_history_numerically_exact
      ) ||
      !identical(
        contract$cumulative_numerical_repair_count,
        final_history$cumulative_numerical_repair_count
      ) ||
      !identical(
        contract$backend_requested,
        final_history$backend_requested
      ) ||
      !identical(
        contract$current_backend_resolved,
        final_history$backend_resolved
      ) ||
      (contract$bitwise_continuation_claim &&
       (length(contract$environment_mismatches) ||
        contract$environment_override_used))) {
    stop(
      "The DLM continuation contract is semantically invalid.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.rqr_validate_dlm_fit_envelope <- function(object) {
  has_continuation <- "continuation_contract" %in% names(object)
  if (!identical(class(object), c("rqr_dlm_mcmc", "rqr_fit")) ||
      !identical(names(attributes(object)), c("names", "class")) ||
      !identical(
        names(object),
        .rqr_dlm_fit_fields(has_continuation)
      ) ||
      anyDuplicated(names(object))) {
    stop("Expected an rqr_dlm_mcmc object.", call. = FALSE)
  }
  if (!identical(object$method, "mcmc_ffbs") ||
      !identical(object$family, "rqr_dlm") ||
      !is.list(object$provenance) ||
      !identical(
        object$provenance$schema_version, .rqr_schema_version()
      ) ||
      !is.list(object$checkpoint_state) ||
      !identical(
        object$checkpoint_state$schema_version,
        .rqr_schema_version()
      )) {
    stop(
      sprintf(
        "The DLM fitted object requires schema %s.",
        .rqr_schema_version()
      ),
      call. = FALSE
    )
  }
  checkpoint <- object$checkpoint_state
  expected_checkpoint_fields <- c(
    "schema_version", "completed_iterations",
    "theta_root1", "theta_root2",
    "theta0_root1", "theta0_root2",
    "latent_v", "lambda", "evolution_scale",
    "transition_kernel_schema", "transition_kernel",
    "transition_kernel_digest", "rng_state"
  )
  if (!is.list(checkpoint) || is.object(checkpoint) ||
      !identical(names(attributes(checkpoint)), "names") ||
      !identical(names(checkpoint), expected_checkpoint_fields) ||
      anyDuplicated(names(checkpoint))) {
    stop(
      "The DLM checkpoint does not have its exact versioned field schema.",
      call. = FALSE
    )
  }
  completed_iterations <- .rqr_history_count(
    checkpoint$completed_iterations,
    "checkpoint_state$completed_iterations"
  )
  checkpoint_digest <- object$checkpoint_digest %||% NA_character_
  if (!.rqr_nonmissing_text(checkpoint_digest) ||
      !grepl("^[0-9a-f]{64}$", checkpoint_digest) ||
      !identical(.rqr_digest(checkpoint), checkpoint_digest)) {
    stop("The DLM checkpoint digest is invalid.", call. = FALSE)
  }
  if (!is.list(object$last) ||
      !identical(object$last, checkpoint)) {
    stop(
      paste(
        "The DLM compatibility last-state alias does not match the",
        "authoritative checkpoint."
      ),
      call. = FALSE
    )
  }

  if (!is.numeric(object$y) || is.object(object$y) ||
      !is.null(dim(object$y)) || !length(object$y) ||
      !any(!is.na(object$y)) || any(is.nan(object$y)) ||
      any(is.infinite(object$y))) {
    stop("The fitted DLM response is invalid.", call. = FALSE)
  }
  initialization <- .rqr_validate_initialization_contract(
    object$initialization_contract,
    family = "rqr_dlm",
    stored_digest = object$initialization_digest
  )
  if (!identical(
        object$provenance$initialization_required, TRUE
      ) ||
      !identical(
        object$provenance$initialization_contract_digest,
        initialization$digest
      ) ||
      !identical(
        object$provenance$rng_initialization_bound,
        initialization$reproducibility_bound
      ) ||
      !identical(
        object$provenance$initial_seed,
        initialization$seed
      ) ||
      !identical(object$misc$seed, initialization$seed) ||
      !identical(
        as.numeric(object$model_spec$lambda_initial),
        as.numeric(
          object$initialization_contract$initial_state$lambda
        )
      )) {
    stop(
      "The DLM initialization contract is not bound to provenance.",
      call. = FALSE
    )
  }
  .rqr_validate_provenance_semantics(object$provenance)
  .rqr_validate_retained_draws_contract(
    object$retained_draws_contract,
    family = "rqr_dlm",
    draws = .rqr_dlm_retained_draws(object),
    stored_digest = object$retained_draws_digest
  )
  .rqr_validate_retained_evidence_contract(
    object$retained_evidence_contract,
    family = "rqr_dlm",
    evidence = .rqr_dlm_retained_evidence(object),
    stored_digest = object$retained_evidence_digest
  )
  expanded <- .rqr_expand_model(
    rqr_as_dlm_model(object$model), length(object$y)
  )
  if (!identical(object$expanded_model, expanded)) {
    stop(
      "The stored expanded DLM model is not its canonical reconstruction.",
      call. = FALSE
    )
  }
  p <- expanded$p
  T <- length(object$y)
  evolution_properties <- .rqr_validate_dlm_evolution_spec(
    object$evolution, expanded, y = object$y
  )
  spec_semantics <- .rqr_validate_dlm_model_spec_semantics(
    object, evolution_properties
  )
  continuation_supported <- spec_semantics$continuation_supported
  history <- if (continuation_supported) {
    .rqr_validate_continuation_history(object)
  } else {
    history_digest <- object$continuation_history_digest %||%
      NA_character_
    if (!is.null(object$continuation_history_contract) ||
        !is.character(history_digest) ||
        length(history_digest) != 1L ||
        !is.na(history_digest)) {
      stop(
        "A noncontinuable DLM fit cannot carry continuation history.",
        call. = FALSE
      )
    }
    NULL
  }
  expected_parent_chain_exact <- if (
      continuation_supported && history$generation > 0L) {
    history$segments[[length(history$segments) - 1L]]$
      chain_history_numerically_exact
  } else {
    TRUE
  }
  expected_parent_promotion <- if (
      continuation_supported && history$generation > 0L) {
    history$segments[[length(history$segments) - 1L]]$
      promotion_eligible
  } else {
    TRUE
  }
  if (!identical(
        object$model_spec$parent_chain_history_numerically_exact,
        expected_parent_chain_exact
      ) ||
      !identical(
        object$model_spec$parent_promotion_eligible,
        expected_parent_promotion
      )) {
    stop(
      paste(
        "The DLM parent-chain status is not reconstructible from",
        "continuation history."
      ),
      call. = FALSE
    )
  }
  schedule <- object$segment_schedule_contract
  schedule_digest <- object$segment_schedule_digest %||% NA_character_
  .rqr_validate_dlm_schedule_value(schedule, schedule_digest)
  final_schedule <- utils::tail(schedule$segments, 1L)[[1L]]
  if (!identical(
      final_schedule$checkpoint_digest, checkpoint_digest
    ) ||
      !identical(
        final_schedule$end_completed_iterations,
        completed_iterations
      ) ||
      !identical(
        final_schedule$initialization_contract,
        object$initialization_contract
      ) ||
      !identical(
        final_schedule$initialization_digest,
        object$initialization_digest
      ) ||
      !identical(
        final_schedule$checkpoint_state, checkpoint
      )) {
    stop(
      paste(
        "The final DLM segment schedule does not bind the checkpoint",
        "and completed-iteration count."
      ),
      call. = FALSE
    )
  }
  if (continuation_supported) {
    if (!identical(schedule$generation, history$generation) ||
        length(schedule$segments) != length(history$segments) ||
        !all(vapply(
          seq_along(schedule$segments),
          function(index) identical(
            schedule$segments[[index]]$checkpoint_digest,
            history$segments[[index]]$checkpoint_digest
          ),
          logical(1L)
        ))) {
      stop(
        paste(
          "The DLM segment schedule does not match the",
          "continuation-history checkpoint chain."
        ),
        call. = FALSE
      )
    }
  } else if (length(schedule$segments) != 1L ||
      schedule$generation != 0L) {
    stop(
      "A noncontinuable DLM fit must have one initial schedule segment.",
      call. = FALSE
    )
  }

  n_burn <- .rqr_history_count(object$misc$n_burn, "misc$n_burn")
  n_mcmc <- .rqr_scalar_integer(
    object$misc$n_mcmc, "misc$n_mcmc", 1L
  )
  thin <- .rqr_scalar_integer(object$misc$thin, "misc$thin", 1L)
  store_state_draws <- .rqr_scalar_logical(
    object$misc$store_state_draws, "misc$store_state_draws"
  )
  store_latent_draws <- .rqr_scalar_logical(
    object$misc$store_latent_draws, "misc$store_latent_draws"
  )
  if (!identical(final_schedule$n_burn, n_burn) ||
      !identical(final_schedule$n_retained_draws, n_mcmc) ||
      !identical(final_schedule$thin, thin)) {
    stop(
      "The final DLM segment schedule conflicts with MCMC controls.",
      call. = FALSE
    )
  }
  repair_ledger <- object$diagnostics$numerical_repairs
  .rqr_validate_repair_records(
    repair_ledger,
    "DLM numerical repair ledger",
    dlm = TRUE,
    max_iteration = final_schedule$raw_iterations
  )
  construction_repairs <- if (final_schedule$generation == 0L) {
    .rqr_history_count(
      object$evolution$construction_audit$repair_count %||% 0L,
      "DLM evolution construction repair count"
    )
  } else {
    0L
  }
  reconstructed_segment_repairs <- .rqr_history_count(
    as.double(nrow(repair_ledger)) +
      as.double(construction_repairs),
    "reconstructed DLM segment numerical-repair count"
  )
  if (!identical(
        object$model_spec$numerical_repair_count,
        reconstructed_segment_repairs
      )) {
    stop(
      paste(
        "DLM numerical exactness is not reconstructible from the",
        "MCMC and evolution-construction repair ledgers."
      ),
      call. = FALSE
    )
  }

  if (!identical(
      .rqr_digest(list(y = as.numeric(object$y))),
      object$provenance$data_digest
    ) ||
      !identical(
        lapply(
          .rqr_dlm_provenance_matrices(expanded, object$evolution),
          .rqr_digest
        ),
        object$provenance$matrix_digests
      )) {
    stop("The DLM data digest or model/evolution matrix digest changed.",
         call. = FALSE)
  }
  fixed_rate <- object$model_spec$fixed_learning_rate
  if (is.null(fixed_rate) || !is.finite(fixed_rate)) {
    fixed_rate <- NA_real_
  }
  target <- .rqr_dlm_target_contract(
    coverage_level = object$model_spec$coverage_level,
    learning_rate_mode = object$model_spec$learning_rate_mode,
    fixed_learning_rate = fixed_rate,
    loss_reference_scale = object$model_spec$loss_reference_scale,
    lambda_prior = object$model_spec$lambda_prior,
    numerical_policy = object$model_spec$numerical_policy,
    jitter_ladder = object$misc$jitter_ladder
  )
  if (!identical(object$misc$jitter_ladder, target$jitter_ladder)) {
    stop(
      paste(
        "The stored DLM jitter ladder is not the canonical ladder for",
        "its numerical policy."
      ),
      call. = FALSE
    )
  }
  .rqr_validate_dlm_initialization_semantics(
    object$initialization_contract,
    expanded = expanded,
    y = object$y,
    evolution = object$evolution,
    target = target,
    schedule = schedule
  )
  object_digests <- lapply(
    .rqr_dlm_provenance_objects(expanded, object$evolution, target),
    .rqr_digest
  )
  if (!identical(
      object_digests, object$provenance$object_digests
    )) {
    stop("The DLM model, target, or evolution digest changed.",
         call. = FALSE)
  }
  .rqr_validate_dlm_continuation_contract(
    object, schedule, history, object_digests
  )
  transition_kernel <- .rqr_dlm_transition_kernel_contract(
    evolution_mode = object$model_spec$evolution_mode,
    learning_rate_mode = object$model_spec$learning_rate_mode,
    complete_time0_state =
      object$model_spec$evolution_mode %in%
        c("component_scale", "fixed_W", "discount_template"),
    component_names = object$evolution$component_names %||% character(),
    component_scale_collapsed_update =
      object$misc$component_scale_collapsed_update %||% FALSE,
    component_scale_interweave =
      object$misc$component_scale_interweave %||% FALSE,
    component_scale_interweave_cycles =
      object$misc$component_scale_interweave_cycles %||% 1L,
    component_scale_slice_width =
      object$misc$component_scale_slice_width %||% 1,
    component_scale_slice_sweeps =
      object$misc$component_scale_slice_sweeps %||% 1L,
    component_scale_slice_max_steps =
      object$misc$component_scale_slice_max_steps %||% 100L,
    component_scale_slice_max_shrink =
      object$misc$component_scale_slice_max_shrink %||% 1000L
  )
  transition_kernel_digest <- .rqr_digest(transition_kernel)
  if (!identical(
        checkpoint$transition_kernel_schema,
        .rqr_dlm_transition_kernel_schema()
      ) ||
      !identical(checkpoint$transition_kernel, transition_kernel) ||
      !identical(
        checkpoint$transition_kernel_digest, transition_kernel_digest
      ) ||
      !identical(
        object$model_spec$transition_kernel,
        transition_kernel
      ) ||
      !identical(
        object$model_spec$transition_kernel_digest,
        transition_kernel_digest
      )) {
    stop("The DLM transition-kernel contract changed.", call. = FALSE)
  }
  .rqr_validate_dlm_schedule_state_chain(
    schedule,
    expanded = expanded,
    y = object$y,
    evolution = object$evolution,
    target = target,
    transition_kernel = transition_kernel,
    transition_kernel_digest = transition_kernel_digest,
    current_checkpoint = checkpoint
  )
  valid_path <- function(value) {
    is.matrix(value) && is.numeric(value) && !is.object(value) &&
      identical(dim(value), c(p, T)) && all(is.finite(value))
  }
  valid_state <- function(value) {
    is.numeric(value) && !is.object(value) && is.null(dim(value)) &&
      length(value) == p && all(is.finite(value))
  }
  if (!valid_path(checkpoint$theta_root1) ||
      !valid_path(checkpoint$theta_root2) ||
      !valid_state(checkpoint$theta0_root1) ||
      !valid_state(checkpoint$theta0_root2) ||
      !is.numeric(checkpoint$latent_v) ||
      is.object(checkpoint$latent_v) ||
      !is.null(dim(checkpoint$latent_v)) ||
      length(checkpoint$latent_v) != T ||
      any(!is.finite(checkpoint$latent_v)) ||
      any(checkpoint$latent_v <= 0) ||
      !is.numeric(checkpoint$lambda) ||
      length(checkpoint$lambda) != 1L ||
      !is.finite(checkpoint$lambda) ||
      checkpoint$lambda <= 0 ||
      is.null(checkpoint$rng_state)) {
    stop("The DLM checkpoint state is incomplete or invalid.",
         call. = FALSE)
  }

  stored <- .rqr_validate_dlm_stored_draws(object)
  if (!identical(dim(stored$eta_root1), c(T, n_mcmc))) {
    stop("The retained DLM root draws conflict with MCMC controls.",
         call. = FALSE)
  }
  expected_lower <- pmin(stored$eta_root1, stored$eta_root2)
  expected_upper <- pmax(stored$eta_root1, stored$eta_root2)
  expected_summary <- .rqr_dlm_coverage_summary(
    object$y, !is.na(object$y), expected_lower, expected_upper
  )
  expected_lambda_summary <- .rqr_lambda_summary(stored$lambda)
  expected_effective_rate <- mean(
    stored$lambda / target$loss_reference_scale
  )
  if (!identical(object$summary, expected_summary) ||
      !identical(
        object$model_spec$lambda_summary,
        expected_lambda_summary
      ) ||
      !identical(
        as.numeric(object$model_spec$effective_learning_rate),
        as.numeric(expected_effective_rate)
      ) ||
      !identical(
        as.numeric(object$model_spec$learning_rate),
        as.numeric(expected_effective_rate)
      )) {
    stop(
      paste(
        "DLM draw-derived summaries are inconsistent with the",
        "retained generalized-posterior draws."
      ),
      call. = FALSE
    )
  }
  forecast_state <- .rqr_validate_dlm_forecast_state(
    object, full_fit = TRUE
  )
  if (!identical(forecast_state$n_save, n_mcmc) ||
      !identical(
        as.numeric(forecast_state$terminal_root1[, n_mcmc]),
        as.numeric(checkpoint$theta_root1[, T])
      ) ||
      !identical(
        as.numeric(forecast_state$terminal_root2[, n_mcmc]),
        as.numeric(checkpoint$theta_root2[, T])
      ) ||
      !identical(
        as.numeric(stored$lambda[n_mcmc]),
        as.numeric(checkpoint$lambda)
      )) {
    stop(
      paste(
        "The final retained DLM state or lambda does not match the",
        "authoritative checkpoint."
      ),
      call. = FALSE
    )
  }
  expected_eta1 <- .rqr_state_ordinates(
    expanded$FF, checkpoint$theta_root1
  )
  expected_eta2 <- .rqr_state_ordinates(
    expanded$FF, checkpoint$theta_root2
  )
  if (!identical(
      as.numeric(stored$eta_root1[, n_mcmc]),
      as.numeric(expected_eta1)
    ) ||
      !identical(
        as.numeric(stored$eta_root2[, n_mcmc]),
        as.numeric(expected_eta2)
      )) {
    stop(
      "The final retained DLM root ordinates do not match the checkpoint.",
      call. = FALSE
    )
  }
  if (store_state_draws) {
    if (!identical(
        as.numeric(object$samp.theta_root1[, , n_mcmc]),
        as.numeric(checkpoint$theta_root1)
      ) ||
        !identical(
          as.numeric(object$samp.theta_root2[, , n_mcmc]),
          as.numeric(checkpoint$theta_root2)
        )) {
      stop(
        "The final retained DLM state paths do not match the checkpoint.",
        call. = FALSE
      )
    }
  } else if (!is.null(object$samp.theta_root1) ||
      !is.null(object$samp.theta_root2)) {
    stop(
      "Full state paths are present although their storage is disabled.",
      call. = FALSE
    )
  }
  time0_root1_present <- !is.null(object$samp.theta0_root1)
  time0_root2_present <- !is.null(object$samp.theta0_root2)
  time0_required <- isTRUE(
    transition_kernel$time0_state_completion
  )
  if (time0_required &&
      (!isTRUE(time0_root1_present) ||
        !isTRUE(time0_root2_present))) {
    stop(
      paste(
        "Stored DLM time-zero draws are required for both roots by",
        "the transition-kernel contract."
      ),
      call. = FALSE
    )
  }
  if (!time0_required &&
      (time0_root1_present || time0_root2_present)) {
    stop(
      paste(
        "Stored DLM time-zero draws are present although the",
        "transition-kernel contract does not complete them."
      ),
      call. = FALSE
    )
  }
  if (time0_required) {
    if (!is.matrix(object$samp.theta0_root1) ||
        !is.matrix(object$samp.theta0_root2) ||
        !identical(dim(object$samp.theta0_root1), c(p, n_mcmc)) ||
        !identical(dim(object$samp.theta0_root2), c(p, n_mcmc)) ||
        !identical(
          as.numeric(object$samp.theta0_root1[, n_mcmc]),
          as.numeric(checkpoint$theta0_root1)
        ) ||
        !identical(
          as.numeric(object$samp.theta0_root2[, n_mcmc]),
          as.numeric(checkpoint$theta0_root2)
        )) {
      stop(
        "The final retained DLM time-zero states do not match the checkpoint.",
        call. = FALSE
      )
    }
  }
  if (store_latent_draws) {
    if (!is.matrix(object$samp.latent_v) ||
        !is.numeric(object$samp.latent_v) ||
        !identical(dim(object$samp.latent_v), c(T, n_mcmc)) ||
        !identical(
          as.numeric(object$samp.latent_v[, n_mcmc]),
          as.numeric(checkpoint$latent_v)
        )) {
      stop(
        "The final retained DLM latent state does not match the checkpoint.",
        call. = FALSE
      )
    }
  } else if (!is.null(object$samp.latent_v)) {
    stop(
      "Latent states are present although their storage is disabled.",
      call. = FALSE
    )
  }
  component_mode <- identical(
    object$model_spec$evolution_mode, "component_scale"
  )
  if (component_mode) {
    q_draws <- forecast_state$evolution_scale
    q_count <- ncol(q_draws)
    component_names <- object$evolution$component_names
    q_shape <- object$samp.evolution_scale_shape
    q_rate <- object$samp.evolution_scale_rate
    if (!is.numeric(checkpoint$evolution_scale) ||
        is.object(checkpoint$evolution_scale) ||
        !is.null(dim(checkpoint$evolution_scale)) ||
        length(checkpoint$evolution_scale) != q_count ||
        !identical(
          as.numeric(q_draws[n_mcmc, ]),
          as.numeric(checkpoint$evolution_scale)
        ) ||
        !is.matrix(q_shape) || !is.numeric(q_shape) ||
        is.object(q_shape) ||
        !identical(dim(q_shape), c(n_mcmc, q_count)) ||
        any(!is.finite(q_shape)) || any(q_shape <= 0) ||
        !identical(colnames(q_shape), component_names) ||
        !is.matrix(q_rate) || !is.numeric(q_rate) ||
        is.object(q_rate) ||
        !identical(dim(q_rate), c(n_mcmc, q_count)) ||
        any(!is.finite(q_rate)) || any(q_rate <= 0) ||
        !identical(colnames(q_rate), component_names)) {
      stop(
        paste(
          "The final retained DLM evolution state or conditional",
          "parameters do not match the checkpoint contract."
        ),
        call. = FALSE
      )
    }
  } else if (length(checkpoint$evolution_scale) != 0L ||
      !is.null(object$samp.evolution_scale) ||
      !is.null(object$samp.evolution_scale_shape) ||
      !is.null(object$samp.evolution_scale_rate)) {
    stop(
      "Non-component DLM fits must not contain evolution-scale draws.",
      call. = FALSE
    )
  }
  if (identical(
      object$model_spec$learning_rate_mode, "fixed_rate"
    )) {
    expected_lambda <- object$model_spec$fixed_learning_rate *
      object$model_spec$loss_reference_scale
    if (!identical(
        as.numeric(checkpoint$lambda), as.numeric(expected_lambda)
      ) ||
        !identical(
          as.numeric(stored$lambda),
          rep(as.numeric(expected_lambda), n_mcmc)
        )) {
      stop("Fixed-rate DLM lambda values are not exact constants.",
           call. = FALSE)
    }
  }
  invisible(list(
    checkpoint = checkpoint,
    checkpoint_digest = checkpoint_digest,
    history = history,
    schedule = schedule,
    expanded = expanded,
    target = target,
    object_digests = object_digests
  ))
}

.rqr_validate_dlm_fit_if_present <- function(
    object, allow_state_only = FALSE) {
  if (!inherits(object, "rqr_dlm_mcmc")) {
    stop("Expected an rqr_dlm_mcmc object.", call. = FALSE)
  }
  allow_state_only <- .rqr_scalar_logical(
    allow_state_only, "allow_state_only"
  )
  full_markers <- c(
    "provenance", "checkpoint_state", "checkpoint_digest", "last",
    "segment_schedule_contract", "segment_schedule_digest"
  )
  claimed_full_fit <-
    identical(object$method %||% NA_character_, "mcmc_ffbs") ||
    identical(object$family %||% NA_character_, "rqr_dlm") ||
    any(full_markers %in% names(object))
  fitted_draw_fields <- c(
    "samp.eta_root1", "samp.eta_root2", "samp.lambda", "y"
  )
  fitted_draws_present <- vapply(
    fitted_draw_fields,
    function(field) !is.null(object[[field]]),
    logical(1L)
  )
  if (claimed_full_fit) {
    .rqr_validate_dlm_fit_envelope(object)
    return(invisible(TRUE))
  }
  if (!allow_state_only) {
    stop(
      paste(
        "This fitted-time operation requires a complete validated",
        "rqr_dlm_mcmc fit."
      ),
      call. = FALSE
    )
  }
  if (any(fitted_draws_present)) {
    stop(
      paste(
        "An external forecast-state fixture cannot claim fitted root,",
        "lambda, or response bindings without a complete fit envelope."
      ),
      call. = FALSE
    )
  }
  invisible(FALSE)
}

.rqr_validate_dlm_continuation <- function(object, allow_environment_mismatch = FALSE) {
  if (length(allow_environment_mismatch) != 1L ||
      !is.logical(allow_environment_mismatch) || is.na(allow_environment_mismatch)) {
    stop("allow_environment_mismatch must be TRUE or FALSE.", call. = FALSE)
  }
  envelope <- .rqr_validate_dlm_fit_envelope(object)
  continuation_history <- envelope$history
  stored_checkpoint_digest <- envelope$checkpoint_digest

  expanded <- .rqr_expand_model(rqr_as_dlm_model(object$model), length(object$y))
  current_data_digest <- .rqr_digest(list(y = as.numeric(object$y)))
  current_matrix_digests <- lapply(
    .rqr_dlm_provenance_matrices(expanded, object$evolution), .rqr_digest
  )
  if (!identical(current_data_digest, object$provenance$data_digest)) {
    stop("Continuation data digest does not match the fitted object.", call. = FALSE)
  }
  if (!identical(current_matrix_digests, object$provenance$matrix_digests)) {
    stop("Continuation model/evolution matrix digests do not match the fitted object.", call. = FALSE)
  }
  fixed_rate <- object$model_spec$fixed_learning_rate
  if (is.null(fixed_rate) || !is.finite(fixed_rate)) fixed_rate <- NA_real_
  current_target_contract <- .rqr_dlm_target_contract(
    coverage_level = object$model_spec$coverage_level,
    learning_rate_mode = object$model_spec$learning_rate_mode,
    fixed_learning_rate = fixed_rate,
    loss_reference_scale = object$model_spec$loss_reference_scale,
    lambda_prior = object$model_spec$lambda_prior,
    numerical_policy = object$model_spec$numerical_policy,
    jitter_ladder = object$misc$jitter_ladder
  )
  current_transition_kernel <- .rqr_dlm_transition_kernel_contract(
    evolution_mode = object$model_spec$evolution_mode,
    learning_rate_mode = object$model_spec$learning_rate_mode,
    complete_time0_state =
      object$model_spec$evolution_mode %in%
        c("component_scale", "fixed_W", "discount_template"),
    component_names = object$evolution$component_names %||% character(),
    component_scale_collapsed_update =
      object$misc$component_scale_collapsed_update %||% FALSE,
    component_scale_interweave =
      object$misc$component_scale_interweave %||% FALSE,
    component_scale_interweave_cycles =
      object$misc$component_scale_interweave_cycles %||% 1L,
    component_scale_slice_width =
      object$misc$component_scale_slice_width %||% 1,
    component_scale_slice_sweeps =
      object$misc$component_scale_slice_sweeps %||% 1L,
    component_scale_slice_max_steps =
      object$misc$component_scale_slice_max_steps %||% 100L,
    component_scale_slice_max_shrink =
      object$misc$component_scale_slice_max_shrink %||% 1000L
  )
  current_transition_kernel_digest <- .rqr_digest(
    current_transition_kernel
  )
  if (!identical(
      object$checkpoint_state$transition_kernel_schema,
      .rqr_dlm_transition_kernel_schema()
    ) ||
      !identical(
      object$checkpoint_state$transition_kernel,
      current_transition_kernel
    ) ||
      !identical(
        object$checkpoint_state$transition_kernel_digest,
        current_transition_kernel_digest
      ) ||
      !identical(
        object$model_spec$transition_kernel,
        current_transition_kernel
      ) ||
      !identical(
        object$model_spec$transition_kernel_digest,
        current_transition_kernel_digest
      )) {
    stop(
      "Continuation transition-kernel contract does not match the fitted object.",
      call. = FALSE
    )
  }
  current_object_digests <- lapply(
    .rqr_dlm_provenance_objects(
      expanded, object$evolution, current_target_contract
    ),
    .rqr_digest
  )
  if (!identical(current_object_digests, object$provenance$object_digests)) {
    stop(
      "Continuation model, target, or evolution digest does not match the fitted object.",
      call. = FALSE
    )
  }

  stored_repo_root <- .rqr_stored_optional_provenance_text(
    object$provenance$repo_root, "repo_root"
  )
  stored_expected <- .rqr_stored_optional_provenance_text(
    object$provenance$expected_git_commit, "expected_git_commit"
  )
  primary_attestation <- .rqr_stored_optional_provenance_text(
    object$provenance$primary_runtime_attestation,
    "primary_runtime_attestation"
  )
  backend_requested <- object$misc$backend_requested %||% object$misc$backend
  backend_resolved <- .rqr_resolve_ffbs_backend(backend_requested)
  current <- .rqr_provenance(
    data = list(y = as.numeric(object$y)),
    matrices = .rqr_dlm_provenance_matrices(expanded, object$evolution),
    numerical_policy = object$model_spec$numerical_policy,
    initial_seed = object$provenance$initial_seed,
    repo_root = stored_repo_root,
    expected_git_commit = stored_expected,
    backend = backend_resolved,
    backend_requested = backend_requested,
    backend_resolved = backend_resolved,
    objects = .rqr_dlm_provenance_objects(
      expanded, object$evolution, current_target_contract
    ),
    external_repositories = object$provenance$external_repositories,
    required_external_repositories =
      object$provenance$required_external_repositories,
    primary_runtime_attestation = primary_attestation,
    initialization_contract = object$initialization_contract
  )
  compare_fields <- c(
    "package_version", "R_version", "platform", "compiler", "BLAS", "LAPACK",
    "git_commit", "git_commit_available", "git_status_available", "git_dirty",
    "expected_git_commit", "expected_git_commit_match",
    "basic_provenance_complete", "provenance_complete",
    "primary_runtime_source_match", "primary_runtime_package_path",
    "primary_source_commit", "primary_source_tree_digest",
    "primary_runtime_tree_digest",
    "backend_requested", "backend_resolved", "RNGkind",
    "initialization_required", "initialization_contract_digest",
    "rng_initialization_bound"
  )
  mismatches <- compare_fields[!vapply(compare_fields, function(field) {
    identical(object$provenance[[field]], current[[field]])
  }, logical(1L))]
  if (!identical(object$provenance$dependency_versions, current$dependency_versions)) {
    mismatches <- c(mismatches, "dependency_versions")
  }
  if (!identical(
        object$provenance$external_repositories,
        current$external_repositories
      )) {
    mismatches <- c(mismatches, "external_repositories")
  }
  if (length(mismatches)) {
    message <- sprintf(
      "Continuation environment differs in: %s.", paste(unique(mismatches), collapse = ", ")
    )
    if (!allow_environment_mismatch) {
      stop(
        paste(message, "Set allow_environment_mismatch=TRUE only for a non-bitwise portability run."),
        call. = FALSE
      )
    }
    warning(
      paste(message, "Exact bitwise continuation is not claimed for this segment."),
      call. = FALSE
    )
  }
  invisible(list(
    current_provenance = current,
    environment_mismatches = unique(mismatches),
    checkpoint_digest = stored_checkpoint_digest,
    object_digests = current_object_digests,
    continuation_history = continuation_history,
    schedule = envelope$schedule
  ))
}

#' Continue an RQR-DLM chain from its exact checkpoint
#'
#' Continuation restores the full RNG state and every Markov state required by
#' the native sampler. Numerical repair and promotion eligibility are inherited
#' cumulatively, and the requested and resolved FFBS backends are checked
#' separately. The function returns only the newly requested draws. Diagnostic
#' `"learned_pure"` fits are deliberately not continuable.
#'
#' @param object An `rqr_dlm_mcmc` fit.
#' @param n_mcmc Positive number of additional retained draws.
#' @param thin Positive thinning interval; defaults to the original fit.
#' @param store_state_draws,store_latent_draws Storage choices for new draws.
#' @param allow_environment_mismatch If `TRUE`, continue after an explicit
#'   warning when package, R, platform, BLAS/LAPACK, dependency, or source-commit
#'   metadata differ. Schema, checkpoint, data, model, target, and evolution
#'   digest mismatches always stop. The override is persisted and removes
#'   reproducibility and promotion eligibility from the returned segment.
#' @return A new `rqr_dlm_mcmc` segment beginning at the checkpoint.
#' @family RQR-DLM
#' @export
rqr_dlm_continue <- function(object, n_mcmc, thin = object$misc$thin,
                             store_state_draws = object$misc$store_state_draws,
                             store_latent_draws = object$misc$store_latent_draws,
                             allow_environment_mismatch = FALSE) {
  if (!inherits(object, "rqr_dlm_mcmc")) {
    stop("Expected an rqr_dlm_mcmc object.", call. = FALSE)
  }
  if (identical(
      object$model_spec$learning_rate_mode, "learned_pure"
    ) ||
      identical(
        object$model_spec$continuation_supported, FALSE
      )) {
    stop(
      "learned_pure is a diagnostic legacy target and is not continuable.",
      call. = FALSE
    )
  }
  n_mcmc <- .rqr_scalar_integer(n_mcmc, "n_mcmc", 1L)
  thin <- .rqr_scalar_integer(thin, "thin", 1L)
  store_state_draws <- .rqr_scalar_logical(
    store_state_draws, "store_state_draws"
  )
  store_latent_draws <- .rqr_scalar_logical(
    store_latent_draws, "store_latent_draws"
  )
  allow_environment_mismatch <- .rqr_scalar_logical(
    allow_environment_mismatch, "allow_environment_mismatch"
  )
  validation <- .rqr_validate_dlm_continuation(
    object, allow_environment_mismatch
  )
  checkpoint <- object$checkpoint_state
  if (is.null(checkpoint$rng_state)) stop("The fit does not contain a complete RNG checkpoint.", call. = FALSE)
  fixed_rate <- object$model_spec$fixed_learning_rate
  if (is.null(fixed_rate) || !is.finite(fixed_rate)) fixed_rate <- 1
  stored_repo_root <- .rqr_stored_optional_provenance_text(
    object$provenance$repo_root, "repo_root"
  )
  stored_expected <- .rqr_stored_optional_provenance_text(
    object$provenance$expected_git_commit, "expected_git_commit"
  )
  primary_attestation <- .rqr_stored_optional_provenance_text(
    object$provenance$primary_runtime_attestation,
    "primary_runtime_attestation"
  )
  segment <- .rqr_dlm_fit_impl(
    y = object$y,
    model = object$model,
    coverage_level = object$model_spec$coverage_level,
    evolution_spec = object$evolution,
    learning_rate = fixed_rate,
    lambda_initial = checkpoint$lambda,
    loss_reference_scale = object$model_spec$loss_reference_scale,
    learning_rate_mode = object$model_spec$learning_rate_mode,
    lambda_prior = object$model_spec$lambda_prior,
    numerical_policy = object$model_spec$numerical_policy,
    provenance_control = list(
      repo_root = stored_repo_root,
      expected_git_commit = stored_expected,
      primary_runtime_attestation = primary_attestation,
      external_repositories = lapply(
        object$provenance$external_repositories %||% list(),
        function(x) list(
          repo_root = .rqr_stored_optional_provenance_text(
            x$repo_root, "external_repositories$repo_root"
          ),
          expected_git_commit = .rqr_stored_optional_provenance_text(
            x$expected_git_commit,
            "external_repositories$expected_git_commit"
          ),
          runtime_package = .rqr_stored_optional_provenance_text(
            x$runtime_package, "external_repositories$runtime_package"
          ),
          runtime_attestation = .rqr_stored_optional_provenance_text(
            x$runtime_attestation,
            "external_repositories$runtime_attestation"
          ),
          require_isolated_runtime = isTRUE(x$require_isolated_runtime)
          ,
          source_subdir = x$source_subdir %||% "."
        )
      ),
      required_external_repositories =
        object$provenance$required_external_repositories %||% character(0)
    ),
    mcmc_control = list(
      n_burn = 0L, n_mcmc = n_mcmc, thin = thin, seed = NULL,
      backend = object$misc$backend_requested %||% object$misc$backend,
      store_state_draws = store_state_draws,
      store_latent_draws = store_latent_draws,
      jitter_ladder = object$misc$jitter_ladder,
      component_scale_collapsed_update =
        isTRUE(object$misc$component_scale_collapsed_update),
      component_scale_interweave =
        isTRUE(object$misc$component_scale_interweave),
      component_scale_interweave_cycles =
        object$misc$component_scale_interweave_cycles %||% 1L,
      component_scale_slice_width =
        object$misc$component_scale_slice_width %||% 1,
      component_scale_slice_sweeps =
        object$misc$component_scale_slice_sweeps %||% 1L,
      component_scale_slice_max_steps =
        object$misc$component_scale_slice_max_steps %||% 100L,
      component_scale_slice_max_shrink =
        object$misc$component_scale_slice_max_shrink %||% 1000L
    ),
    init = list(
      state_root1 = checkpoint$theta_root1,
      state_root2 = checkpoint$theta_root2,
      theta0_root1 = checkpoint$theta0_root1,
      theta0_root2 = checkpoint$theta0_root2,
      latent_v = checkpoint$latent_v,
      lambda = checkpoint$lambda,
      evolution_scale = checkpoint$evolution_scale,
      rng_state = checkpoint$rng_state,
      completed_iterations = checkpoint$completed_iterations,
      continued_from_checkpoint = TRUE,
      parent_cumulative_numerical_repair_count =
        validation$continuation_history$
          cumulative_numerical_repair_count,
      parent_chain_history_numerically_exact =
        validation$continuation_history$
          chain_history_numerically_exact,
      parent_promotion_eligible =
        validation$continuation_history$promotion_eligible,
      continuation_control = list(
        parent_schedule_contract = validation$schedule,
        parent_schedule_digest = object$segment_schedule_digest
      )
    ),
    .continuation_token = .rqr_dlm_continuation_token
  )
  environment_override_used <- length(validation$environment_mismatches) > 0L
  parent_reproducibility_eligible <- isTRUE(
    validation$continuation_history$reproducibility_eligible
  )
  current_environment_eligible <- isTRUE(
    segment$provenance$reproducibility_eligible
  )
  inherited_reproducibility_eligible <- current_environment_eligible &&
    parent_reproducibility_eligible && !environment_override_used
  parent_chain_history_numerically_exact <- isTRUE(
    validation$continuation_history$chain_history_numerically_exact
  )
  parent_promotion_eligible <- isTRUE(
    validation$continuation_history$promotion_eligible
  )
  parent_cumulative_repairs <- .rqr_history_count(
    validation$continuation_history$cumulative_numerical_repair_count,
    "parent cumulative repair count"
  )
  same_resolved_backend <- identical(
    object$provenance$backend_resolved %||% object$provenance$backend,
    validation$current_provenance$backend_resolved %||%
      validation$current_provenance$backend
  )
  bitwise_continuation_claim <- !environment_override_used &&
    !length(validation$environment_mismatches) &&
    parent_reproducibility_eligible &&
    current_environment_eligible &&
    same_resolved_backend
  segment$continuation_contract <- list(
    continued_from_checkpoint = TRUE,
    parent_checkpoint_digest = validation$checkpoint_digest,
    parent_completed_iterations = checkpoint$completed_iterations,
    model_target_evolution_digests = validation$object_digests,
    environment_mismatches = validation$environment_mismatches,
    environment_override_used = environment_override_used,
    bitwise_continuation_claim = bitwise_continuation_claim,
    parent_reproducibility_eligible = parent_reproducibility_eligible,
    parent_target_numerical_eligible =
      isTRUE(object$model_spec$target_numerical_eligible),
    parent_promotion_eligible = parent_promotion_eligible,
    parent_chain_history_numerically_exact =
      parent_chain_history_numerically_exact,
    parent_cumulative_numerical_repair_count =
      parent_cumulative_repairs,
    chain_history_numerically_exact =
      isTRUE(segment$model_spec$chain_history_numerically_exact),
    cumulative_numerical_repair_count =
      segment$model_spec$cumulative_numerical_repair_count,
    backend_requested =
      segment$provenance$backend_requested,
    parent_backend_resolved =
      object$provenance$backend_resolved %||% object$provenance$backend,
    current_backend_resolved =
      segment$provenance$backend_resolved %||% segment$provenance$backend,
    current_environment_reproducibility_eligible =
      current_environment_eligible
  )
  segment$provenance$continued_from_checkpoint <- TRUE
  segment$provenance$parent_checkpoint_digest <- validation$checkpoint_digest
  segment$provenance$parent_reproducibility_eligible <-
    parent_reproducibility_eligible
  segment$provenance$environment_override_used <- environment_override_used
  segment$provenance$reproducibility_eligible <-
    inherited_reproducibility_eligible
  segment$model_spec$reproducibility_eligible <-
    inherited_reproducibility_eligible
  segment$model_spec$promotion_eligible <-
    isTRUE(segment$model_spec$ordinary_v1_scope_eligible) &&
    isTRUE(segment$model_spec$target_numerical_eligible) &&
    parent_promotion_eligible &&
    inherited_reproducibility_eligible
  segment$continuation_history_contract <- .rqr_make_continuation_history(
    checkpoint_digest = segment$checkpoint_digest,
    segment_numerical_repair_count =
      segment$model_spec$numerical_repair_count,
    segment_exact_joint_target =
      segment$model_spec$exact_joint_target,
    segment_environment_base_eligible =
      current_environment_eligible,
    segment_target_contract_digest =
      segment$provenance$object_digests$target,
    backend_requested = segment$provenance$backend_requested,
    backend_resolved = segment$provenance$backend_resolved,
    segment_transition_kernel_schema =
      segment$checkpoint_state$transition_kernel_schema,
    segment_transition_kernel_digest =
      segment$checkpoint_state$transition_kernel_digest,
    parent = validation$continuation_history,
    parent_checkpoint_digest = validation$checkpoint_digest,
    environment_mismatches = validation$environment_mismatches,
    environment_override_used = environment_override_used
  )
  segment$continuation_history_digest <- .rqr_digest(
    segment$continuation_history_contract
  )
  .rqr_validate_dlm_fit_envelope(segment)
  segment
}

#' Extract fitted root-ordinate draws from RQR-DLM MCMC
#'
#' The returned matrices contain root ordinates over the fitted time range,
#' rather than full state paths. These are generalized-posterior root
#' functionals, not response draws. Native extraction returns a typed,
#' content-digested envelope bound to the exact fit checkpoint, retained-draw
#' contract, target, model, evolution, and draw-selection operation.
#'
#' @param object An `rqr_dlm_mcmc` fit.
#' @param nd Number of retained draws to return. `NULL` keeps all draws.
#' @param seed Optional seed used only when draws are subsampled.
#' @param ... Reserved; supplying an argument is an error.
#' @return A typed `rqr_dlm_draws` object containing time-by-draw matrices
#'   `eta_root1` and `eta_root2`, corresponding loss-rate draws `lambda`,
#'   selected indices `index`, `nd`, and exact source and RNG bindings.
#' @family RQR-DLM
#' @export
rqr_posterior_draws.rqr_dlm_mcmc <- function(
    object, nd = NULL, seed = NULL, ...) {
  .rqr_reject_dots(
    list(...), "rqr_posterior_draws.rqr_dlm_mcmc"
  )
  .rqr_validate_dlm_fit_if_present(object)
  stored <- .rqr_validate_dlm_stored_draws(object)
  n_save <- ncol(stored$eta_root1)
  selection <- .rqr_dlm_select_draw_indices(
    n_save = n_save, nd = nd, seed = seed,
    operation = "fitted_draw_selection"
  )
  idx <- selection$index
  out <- list(
    schema_version = .rqr_dlm_draws_schema(),
    eta_root1 = stored$eta_root1[, idx, drop = FALSE],
    eta_root2 = stored$eta_root2[, idx, drop = FALSE],
    lambda = as.numeric(stored$lambda[idx]),
    index = as.integer(idx),
    nd = as.integer(length(idx)),
    source = .rqr_dlm_native_draw_source(object),
    selection = selection$rng_binding,
    source_bound = TRUE,
    reproducibility_eligible =
      isTRUE(object$model_spec$reproducibility_eligible) &&
      isTRUE(selection$rng_binding$reproducibility_bound),
    promotion_eligible =
      isTRUE(object$model_spec$promotion_eligible) &&
      isTRUE(selection$rng_binding$reproducibility_bound),
    response_predictive_draws = FALSE
  )
  out$semantic_digest <- .rqr_digest(out)
  class(out) <- c("rqr_dlm_draws", "list")
  .rqr_validate_dlm_draws(object, out)
  out
}

.rqr_dlm_native_draw_source <- function(object) {
  digests <- object$provenance$object_digests
  list(
    schema_version = .rqr_dlm_draw_source_schema(),
    binding_status = "fit_retained_draws",
    fit_schema_version = object$provenance$schema_version,
    fit_checkpoint_digest = object$checkpoint_digest,
    retained_draws_digest = object$retained_draws_digest,
    target_digest = digests$target,
    model_digest = digests$model,
    evolution_digest = digests$evolution,
    data_digest = object$provenance$data_digest
  )
}

.rqr_dlm_external_draw_source <- function(payload_digest) {
  list(
    schema_version = .rqr_dlm_draw_source_schema(),
    binding_status = "external_unbound",
    fit_schema_version = NA_character_,
    fit_checkpoint_digest = NA_character_,
    retained_draws_digest = NA_character_,
    target_digest = NA_character_,
    model_digest = NA_character_,
    evolution_digest = NA_character_,
    data_digest = payload_digest
  )
}

.rqr_dlm_select_draw_indices <- function(
    n_save, nd = NULL, seed = NULL,
    operation = "fitted_draw_selection") {
  n_save <- .rqr_scalar_integer(n_save, "n_save", 1L)
  if (is.null(nd)) {
    if (!is.null(seed)) {
      stop(
        "seed must be NULL when nd is NULL because no subsampling occurs.",
        call. = FALSE
      )
    }
    return(list(
      index = seq_len(n_save),
      rng_binding = .rqr_dlm_rng_binding(
        operation = operation, mode = "none"
      )
    ))
  }
  nd <- .rqr_scalar_integer(nd, "nd", 1L)
  if (!is.null(seed)) {
    seed <- .rqr_scalar_integer(seed, "seed", 0L)
    generated <- .rqr_dlm_preserve_rng(function() {
      set.seed(seed)
      before <- .rqr_rng_state()
      index <- sample.int(
        n_save, nd, replace = nd > n_save
      )
      list(
        index = index,
        before = before,
        after = .rqr_rng_state()
      )
    })
    mode <- "explicit_seed"
  } else {
    before <- .rqr_rng_state()
    index <- sample.int(n_save, nd, replace = nd > n_save)
    generated <- list(
      index = index, before = before,
      after = .rqr_rng_state()
    )
    mode <- "ambient_rng"
  }
  list(
    index = as.integer(generated$index),
    rng_binding = .rqr_dlm_rng_binding(
      operation = operation, mode = mode,
      seed = seed %||% NA_integer_,
      rng_state_before = generated$before,
      rng_state_after = generated$after,
      selection_mode = "subsample",
      requested_draw_count = nd,
      sampling_with_replacement = nd > n_save
    )
  )
}

.rqr_dlm_replay_selection <- function(
    n_save, nd, rng_binding) {
  .rqr_validate_dlm_rng_binding(rng_binding)
  n_save <- .rqr_scalar_integer(n_save, "n_save", 1L)
  nd <- .rqr_scalar_integer(nd, "nd", 1L)
  if (identical(rng_binding$mode, "none")) {
    if (!identical(nd, n_save) ||
        !identical(rng_binding$selection_mode, "all")) {
      stop(
        "A no-RNG draw selection must retain every fitted draw.",
        call. = FALSE
      )
    }
    return(seq_len(n_save))
  }
  if (identical(rng_binding$mode, "external_unbound")) {
    return(NULL)
  }
  if (!identical(
        rng_binding$selection_mode, "subsample"
      ) ||
      !identical(
        rng_binding$requested_draw_count, nd
      ) ||
      !identical(
        rng_binding$sampling_with_replacement,
        nd > n_save
      )) {
    stop(
      "The DLM draw-selection count or replacement contract is invalid.",
      call. = FALSE
    )
  }
  if (identical(rng_binding$mode, "ambient_rng") &&
      is.null(rng_binding$rng_state_before)) {
    return(NULL)
  }
  replay <- .rqr_dlm_preserve_rng(function() {
    if (identical(rng_binding$mode, "explicit_seed")) {
      set.seed(rng_binding$seed)
      if (!identical(
        .rqr_rng_state(), rng_binding$rng_state_before
      )) {
        stop(
          "The explicit draw-selection seed does not reproduce its RNG state.",
          call. = FALSE
        )
      }
    } else {
      .rqr_restore_rng(rng_binding$rng_state_before)
    }
    index <- sample.int(
      n_save, nd, replace = nd > n_save
    )
    after <- .rqr_rng_state()
    list(index = as.integer(index), after = after)
  })
  if (!identical(replay$after, rng_binding$rng_state_after)) {
    stop(
      "The recorded DLM draw-selection RNG transition is invalid.",
      call. = FALSE
    )
  }
  replay$index
}

.rqr_validate_dlm_stored_draws <- function(object) {
  if (!inherits(object, "rqr_dlm_mcmc")) {
    stop("Expected an rqr_dlm_mcmc object.", call. = FALSE)
  }
  eta1 <- object$samp.eta_root1
  eta2 <- object$samp.eta_root2
  if (!is.matrix(eta1) || !is.numeric(eta1) || is.object(eta1) ||
      !is.matrix(eta2) || !is.numeric(eta2) || is.object(eta2) ||
      !identical(dim(eta1), dim(eta2)) ||
      nrow(eta1) < 1L || ncol(eta1) < 1L ||
      any(!is.finite(eta1)) || any(!is.finite(eta2))) {
    stop(
      paste(
        "Stored DLM root-ordinate draws must be matching nonempty finite",
        "plain numeric matrices."
      ),
      call. = FALSE
    )
  }
  if (!is.numeric(object$y) || is.object(object$y) ||
      !is.null(dim(object$y)) || length(object$y) != nrow(eta1) ||
      !any(!is.na(object$y)) || any(is.nan(object$y)) ||
      any(is.infinite(object$y))) {
    stop(
      "Stored DLM root-ordinate rows must match the fitted response length.",
      call. = FALSE
    )
  }
  lambda <- object$samp.lambda
  if (!is.numeric(lambda) || is.object(lambda) || !is.null(dim(lambda)) ||
      length(lambda) != ncol(eta1) ||
      any(!is.finite(lambda)) || any(lambda <= 0)) {
    stop(
      "Stored DLM lambda draws must be finite, positive, and aligned with the root draws.",
      call. = FALSE
    )
  }
  storage.mode(eta1) <- "double"
  storage.mode(eta2) <- "double"
  lambda <- as.numeric(lambda)
  names(lambda) <- NULL
  list(
    eta_root1 = eta1,
    eta_root2 = eta2,
    lambda = lambda
  )
}

.rqr_validate_dlm_forecast_state <- function(
    object, full_fit = FALSE) {
  if (!inherits(object, "rqr_dlm_mcmc")) {
    stop("Expected an rqr_dlm_mcmc object.", call. = FALSE)
  }
  full_fit <- .rqr_scalar_logical(full_fit, "full_fit")
  validate_terminal <- function(value, name) {
    if (!is.matrix(value) || !is.numeric(value) || is.object(value) ||
        nrow(value) < 1L || ncol(value) < 1L ||
        any(!is.finite(value))) {
      stop(
        sprintf(
          "%s must be a nonempty finite plain numeric state-by-draw matrix.",
          name
        ),
        call. = FALSE
      )
    }
    storage.mode(value) <- "double"
    value
  }
  validate_paths <- function(value, name) {
    dimensions <- dim(value)
    if (!is.array(value) || !is.numeric(value) || is.object(value) ||
        length(dimensions) != 3L ||
        any(dimensions < 1L) || any(!is.finite(value))) {
      stop(
        sprintf(
          "%s must be a nonempty finite plain numeric p-by-T-by-draw array.",
          name
        ),
        call. = FALSE
      )
    }
    storage.mode(value) <- "double"
    value
  }

  terminal1_present <- !is.null(object$samp.theta_terminal_root1)
  terminal2_present <- !is.null(object$samp.theta_terminal_root2)
  if (!identical(terminal1_present, terminal2_present)) {
    stop(
      "Stored terminal state draws must be present for both roots or neither.",
      call. = FALSE
    )
  }
  paths1_present <- !is.null(object$samp.theta_root1)
  paths2_present <- !is.null(object$samp.theta_root2)
  if (!identical(paths1_present, paths2_present)) {
    stop(
      "Stored full state draws must be present for both roots or neither.",
      call. = FALSE
    )
  }

  paths1 <- paths2 <- NULL
  if (paths1_present) {
    paths1 <- validate_paths(
      object$samp.theta_root1, "samp.theta_root1"
    )
    paths2 <- validate_paths(
      object$samp.theta_root2, "samp.theta_root2"
    )
    if (!identical(dim(paths1), dim(paths2))) {
      stop(
        "Stored full state draws must have matching root dimensions.",
        call. = FALSE
      )
    }
  }
  if (terminal1_present) {
    terminal1 <- validate_terminal(
      object$samp.theta_terminal_root1,
      "samp.theta_terminal_root1"
    )
    terminal2 <- validate_terminal(
      object$samp.theta_terminal_root2,
      "samp.theta_terminal_root2"
    )
    if (!identical(dim(terminal1), dim(terminal2))) {
      stop(
        "Stored terminal state draws must have matching root dimensions.",
        call. = FALSE
      )
    }
  } else if (paths1_present) {
    path_dim <- dim(paths1)
    terminal1 <- matrix(
      paths1[, path_dim[2L], , drop = FALSE],
      nrow = path_dim[1L], ncol = path_dim[3L]
    )
    terminal2 <- matrix(
      paths2[, path_dim[2L], , drop = FALSE],
      nrow = path_dim[1L], ncol = path_dim[3L]
    )
  } else {
    stop(
      "The fit contains neither terminal nor full state draws.",
      call. = FALSE
    )
  }

  p <- nrow(terminal1)
  n_save <- ncol(terminal1)
  if (paths1_present) {
    path_dim <- dim(paths1)
    if (!identical(path_dim[c(1L, 3L)], c(p, n_save))) {
      stop(
        "Full and terminal state draws have incompatible retained-draw dimensions.",
        call. = FALSE
      )
    }
    terminal_from_paths1 <- matrix(
      paths1[, path_dim[2L], , drop = FALSE],
      nrow = p, ncol = n_save
    )
    terminal_from_paths2 <- matrix(
      paths2[, path_dim[2L], , drop = FALSE],
      nrow = p, ncol = n_save
    )
    if (terminal1_present &&
        (!identical(as.numeric(terminal1), as.numeric(terminal_from_paths1)) ||
         !identical(as.numeric(terminal2), as.numeric(terminal_from_paths2)))) {
      stop(
        "Stored terminal states do not equal the last retained full-state slices.",
        call. = FALSE
      )
    }
  }

  expanded_p <- object$expanded_model$p %||% NULL
  if (!is.null(expanded_p)) {
    expanded_p <- .rqr_scalar_integer(
      expanded_p, "expanded_model$p", 1L
    )
    if (!identical(expanded_p, p)) {
      stop(
        "Stored terminal-state dimension does not match expanded_model$p.",
        call. = FALSE
      )
    }
  }

  fitted_fields <- c(
    "samp.eta_root1", "samp.eta_root2", "samp.lambda", "y"
  )
  fitted_fields_present <- vapply(
    fitted_fields, function(field) !is.null(object[[field]]), logical(1L)
  )
  draw_binding_status <- "unbound_external_state_fixture"
  stored <- NULL
  if (full_fit) {
    if (!all(fitted_fields_present)) {
      stop(
        "A complete DLM fit is missing fitted-draw binding fields.",
        call. = FALSE
      )
    }
    stored <- .rqr_validate_dlm_stored_draws(object)
    if (ncol(stored$eta_root1) != n_save) {
      stop(
        "Stored terminal states are not aligned with the retained DLM draws.",
        call. = FALSE
      )
    }
    draw_binding_status <- "fit_retained_draws"
  } else if (any(fitted_fields_present)) {
    stop(
      paste(
        "An external forecast-state fixture cannot carry pseudo-fitted",
        "root, lambda, or response fields."
      ),
      call. = FALSE
    )
  }

  if (!is.null(stored) &&
      (is.null(expanded_p) || is.null(object$expanded_model$FF))) {
    stop(
      paste(
        "A fitted-draw forecast requires the stored expanded-model",
        "state dimension and observation design."
      ),
      call. = FALSE
    )
  }
  if (!is.null(stored)) {
    FF <- object$expanded_model$FF
    if (!is.matrix(FF) || !is.numeric(FF) || is.object(FF) ||
        !identical(dim(FF), c(p, nrow(stored$eta_root1))) ||
        any(!is.finite(FF))) {
      stop(
        "expanded_model$FF is incompatible with the stored forecast state.",
        call. = FALSE
      )
    }
    fitted_terminal1 <- colSums(
      terminal1 * as.numeric(FF[, ncol(FF)])
    )
    fitted_terminal2 <- colSums(
      terminal2 * as.numeric(FF[, ncol(FF)])
    )
    ordinate_scale <- pmax(
      1, abs(stored$eta_root1[nrow(stored$eta_root1), ]),
      abs(stored$eta_root2[nrow(stored$eta_root2), ])
    )
    tolerance <- 100 * .Machine$double.eps * ordinate_scale
    if (any(abs(
        fitted_terminal1 -
          stored$eta_root1[nrow(stored$eta_root1), ]
      ) > tolerance) ||
        any(abs(
          fitted_terminal2 -
            stored$eta_root2[nrow(stored$eta_root2), ]
        ) > tolerance)) {
      stop(
        paste(
          "Stored terminal states are not draw-wise aligned with the",
          "terminal fitted root ordinates."
        ),
        call. = FALSE
      )
    }
  }

  evolution_mode <- object$model_spec$evolution_mode %||% NULL
  if (!is.character(evolution_mode) ||
      length(evolution_mode) != 1L || is.na(evolution_mode) ||
      !evolution_mode %in% c(
        "fixed_W", "discount_template", "component_scale",
        "adaptive_discount"
      )) {
    stop(
      "model_spec$evolution_mode is missing or unsupported.",
      call. = FALSE
    )
  }
  component_scale <- object$samp.evolution_scale
  if (identical(evolution_mode, "component_scale")) {
    evolution <- object$evolution
    if (!inherits(evolution, "rqr_evolution") ||
        !identical(evolution$mode, "component_scale")) {
      stop(
        "A component-scale forecast requires its stored rqr_evolution contract.",
        call. = FALSE
      )
    }
    component_dims <- .rqr_positive_integer_vector(
      evolution$component_dims, "evolution$component_dims"
    )
    component_names <- evolution$component_names
    if (sum(component_dims) != p ||
        !is.character(component_names) ||
        length(component_names) != length(component_dims) ||
        anyNA(component_names) || any(!nzchar(component_names)) ||
        anyDuplicated(component_names)) {
      stop(
        "Stored component names and dimensions do not match the state dimension.",
        call. = FALSE
      )
    }
    if (!is.matrix(component_scale) ||
        !is.numeric(component_scale) || is.object(component_scale) ||
        !identical(
          dim(component_scale), c(n_save, length(component_dims))
        ) ||
        any(!is.finite(component_scale)) ||
        any(component_scale <= 0)) {
      stop(
        paste(
          "samp.evolution_scale must be a finite positive",
          "retained-draw-by-component numeric matrix."
        ),
        call. = FALSE
      )
    }
    if (!identical(colnames(component_scale), component_names)) {
      stop(
        paste(
          "samp.evolution_scale columns must exactly follow the stored",
          "component-name order."
        ),
        call. = FALSE
      )
    }
    storage.mode(component_scale) <- "double"
  } else if (!is.null(component_scale)) {
    stop(
      "Non-component-scale fits must not contain component-scale draws.",
      call. = FALSE
    )
  }

  list(
    terminal_root1 = terminal1,
    terminal_root2 = terminal2,
    evolution_scale = component_scale,
    state_dimension = p,
    n_save = n_save,
    draw_binding_status = draw_binding_status
  )
}

.rqr_validate_dlm_draws <- function(object, draws) {
  stored <- .rqr_validate_dlm_stored_draws(object)
  if (inherits(draws, "rqr_dlm_draws")) {
    return(.rqr_validate_typed_dlm_draws(
      object, draws, stored = stored
    ))
  }

  # Backward-compatible explicit matrices are accepted only as foreign,
  # unbound inputs. No combination of values or declared indices promotes a
  # hand-built list into a native retained-draw object.
  allowed_fields <- c(
    "eta_root1", "eta_root2", "lambda", "index", "nd"
  )
  if (!is.list(draws) || is.object(draws)) {
    stop("draws must be a plain named list.", call. = FALSE)
  }
  .rqr_validate_named_list_fields(
    draws, "draws", allowed = allowed_fields
  )
  if (!all(c("eta_root1", "eta_root2") %in% names(draws))) {
    stop(
      "draws must contain both root-ordinate matrices.",
      call. = FALSE
    )
  }
  eta1 <- draws$eta_root1
  eta2 <- draws$eta_root2
  if (!is.matrix(eta1) || !is.numeric(eta1) || is.object(eta1) ||
      !is.matrix(eta2) || !is.numeric(eta2) || is.object(eta2)) {
    stop(
      "The supplied DLM root-ordinate draws must be plain numeric matrices.",
      call. = FALSE
    )
  }
  if (!identical(dim(eta1), dim(eta2)) ||
      nrow(eta1) != nrow(stored$eta_root1) ||
      ncol(eta1) < 1L ||
      any(!is.finite(eta1)) || any(!is.finite(eta2))) {
    stop(
      paste(
        "Supplied DLM root-ordinate draws must be matching finite numeric",
        "matrices with one row per fitted time."
      ),
      call. = FALSE
    )
  }
  storage.mode(eta1) <- "double"
  storage.mode(eta2) <- "double"
  n_draw <- ncol(eta1)

  lambda <- NULL
  if ("lambda" %in% names(draws) && !is.null(draws$lambda)) {
    lambda <- draws$lambda
    if (!is.numeric(lambda) || is.object(lambda) ||
        !is.null(dim(lambda)) ||
        length(lambda) != n_draw ||
        any(!is.finite(lambda)) || any(lambda <= 0)) {
      stop(
        paste(
          "Supplied DLM lambda draws must be a finite positive numeric",
          "vector with one value per root draw."
        ),
        call. = FALSE
      )
    }
    storage.mode(lambda) <- "double"
    names(lambda) <- NULL
  }

  nd <- if ("nd" %in% names(draws)) {
    if (!is.numeric(draws$nd) || is.object(draws$nd) ||
        !is.null(dim(draws$nd)) || length(draws$nd) != 1L) {
      stop(
        "draws$nd must be one plain numeric scalar.",
        call. = FALSE
      )
    }
    .rqr_scalar_integer(draws$nd, "draws$nd", 1L)
  } else {
    n_draw
  }
  if (!identical(nd, n_draw)) {
    stop(
      "draws$nd must equal the number of root-draw columns.",
      call. = FALSE
    )
  }

  index <- if ("index" %in% names(draws)) {
    draws$index
  } else {
    rep(NA_integer_, n_draw)
  }
  if (!is.integer(index) || is.object(index) ||
      !is.null(dim(index)) || length(index) != n_draw) {
    stop(
      "draws$index must be an integer vector with one entry per draw.",
      call. = FALSE
    )
  }
  n_save <- ncol(stored$eta_root1)
  unknown_index <- all(is.na(index))
  if (!unknown_index) {
    if (anyNA(index) ||
        any(index < 1L) || any(index > n_save)) {
      stop(
        "Known draws$index values must lie within the fitted retained-draw range.",
        call. = FALSE
      )
    }
  }
  names(index) <- NULL

  static_payload <- list(
    eta_root1 = eta1,
    eta_root2 = eta2,
    lambda = lambda,
    index = index,
    nd = nd
  )
  out <- c(
    list(schema_version = .rqr_dlm_draws_schema()),
    static_payload,
    list(
      source = .rqr_dlm_external_draw_source(
        .rqr_digest(static_payload)
      ),
      selection = .rqr_dlm_rng_binding(
        operation = "fitted_draw_selection",
        mode = "external_unbound"
      ),
      source_bound = FALSE,
      reproducibility_eligible = FALSE,
      promotion_eligible = FALSE,
      response_predictive_draws = FALSE
    )
  )
  out$semantic_digest <- .rqr_digest(out)
  class(out) <- c("rqr_dlm_draws", "list")
  .rqr_validate_typed_dlm_draws(object, out, stored = stored)
}

.rqr_validate_typed_dlm_draws <- function(
    object, draws, stored = NULL) {
  if (is.null(stored)) {
    stored <- .rqr_validate_dlm_stored_draws(object)
  }
  .rqr_dlm_assert_exact_list_object(
    draws, c("rqr_dlm_draws", "list"),
    "RQR-DLM posterior draws"
  )
  expected_fields <- c(
    "schema_version", "eta_root1", "eta_root2", "lambda",
    "index", "nd", "source", "selection", "source_bound",
    "reproducibility_eligible", "promotion_eligible",
    "response_predictive_draws", "semantic_digest"
  )
  if (!identical(names(draws), expected_fields) ||
      !identical(
        draws$schema_version, .rqr_dlm_draws_schema()
      )) {
    stop(
      "The RQR-DLM posterior-draw envelope is noncanonical.",
      call. = FALSE
    )
  }
  eta1 <- draws$eta_root1
  eta2 <- draws$eta_root2
  if (!is.matrix(eta1) || !is.numeric(eta1) || is.object(eta1) ||
      !is.matrix(eta2) || !is.numeric(eta2) || is.object(eta2) ||
      !identical(dim(eta1), dim(eta2)) ||
      nrow(eta1) != nrow(stored$eta_root1) ||
      ncol(eta1) < 1L ||
      any(!is.finite(eta1)) || any(!is.finite(eta2))) {
    stop(
      paste(
        "Typed DLM root-ordinate draws must be matching finite plain",
        "numeric matrices with one row per fitted time."
      ),
      call. = FALSE
    )
  }
  n_draw <- ncol(eta1)
  if (!is.numeric(draws$lambda) &&
      !is.null(draws$lambda)) {
    stop("Typed DLM lambda draws are invalid.", call. = FALSE)
  }
  if (!is.null(draws$lambda) &&
      (is.object(draws$lambda) || !is.null(dim(draws$lambda)) ||
       length(draws$lambda) != n_draw ||
       any(!is.finite(draws$lambda)) ||
       any(draws$lambda <= 0))) {
    stop("Typed DLM lambda draws are invalid.", call. = FALSE)
  }
  if (!is.integer(draws$index) || is.object(draws$index) ||
      !is.null(dim(draws$index)) ||
      length(draws$index) != n_draw ||
      !is.integer(draws$nd) || length(draws$nd) != 1L ||
      is.object(draws$nd) || !is.null(dim(draws$nd)) ||
      !identical(draws$nd, as.integer(n_draw))) {
    stop(
      "Typed DLM draw indices and draw count are invalid.",
      call. = FALSE
    )
  }
  .rqr_validate_dlm_rng_binding(
    draws$selection, operation = "fitted_draw_selection"
  )
  if (!is.list(draws$source) || is.object(draws$source) ||
      !identical(names(attributes(draws$source)), "names") ||
      !identical(
        names(draws$source),
        names(.rqr_dlm_native_draw_source(object))
      ) ||
      !identical(
        draws$source$schema_version,
        .rqr_dlm_draw_source_schema()
      ) ||
      !is.logical(draws$source_bound) ||
      length(draws$source_bound) != 1L ||
      is.na(draws$source_bound) ||
      !is.logical(draws$reproducibility_eligible) ||
      length(draws$reproducibility_eligible) != 1L ||
      is.na(draws$reproducibility_eligible) ||
      !is.logical(draws$promotion_eligible) ||
      length(draws$promotion_eligible) != 1L ||
      is.na(draws$promotion_eligible) ||
      !identical(draws$response_predictive_draws, FALSE)) {
    stop(
      "The RQR-DLM posterior-draw source contract is invalid.",
      call. = FALSE
    )
  }
  static_payload <- list(
    eta_root1 = draws$eta_root1,
    eta_root2 = draws$eta_root2,
    lambda = draws$lambda,
    index = draws$index,
    nd = draws$nd
  )
  if (isTRUE(draws$source_bound)) {
    if (!identical(draws$source, .rqr_dlm_native_draw_source(object)) ||
        !identical(
          draws$selection$mode %in%
            c("none", "explicit_seed", "ambient_rng"),
          TRUE
        ) ||
        anyNA(draws$index) ||
        any(draws$index < 1L) ||
        any(draws$index > ncol(stored$eta_root1)) ||
        !identical(
          draws$eta_root1,
          stored$eta_root1[, draws$index, drop = FALSE]
        ) ||
        !identical(
          draws$eta_root2,
          stored$eta_root2[, draws$index, drop = FALSE]
        ) ||
        !identical(
          as.numeric(draws$lambda),
          as.numeric(stored$lambda[draws$index])
        )) {
      stop(
        paste(
          "RQR-DLM posterior draws are not bound to the exact",
          "source fit, target, model, evolution, and retained draws."
        ),
        call. = FALSE
      )
    }
    replay <- .rqr_dlm_replay_selection(
      ncol(stored$eta_root1), draws$nd, draws$selection
    )
    if (!is.null(replay) &&
        !identical(as.integer(replay), draws$index)) {
      stop(
        "The selected DLM draw indices do not match their RNG binding.",
        call. = FALSE
      )
    }
    expected_reproducibility <-
      isTRUE(object$model_spec$reproducibility_eligible) &&
      isTRUE(draws$selection$reproducibility_bound)
    expected_promotion <-
      isTRUE(object$model_spec$promotion_eligible) &&
      isTRUE(draws$selection$reproducibility_bound)
  } else {
    expected_source <- .rqr_dlm_external_draw_source(
      .rqr_digest(static_payload)
    )
    if (!identical(draws$source, expected_source) ||
        !identical(draws$selection$mode, "external_unbound") ||
        !identical(draws$selection$reproducibility_bound, FALSE)) {
      stop(
        paste(
          "Foreign fitted-time DLM matrices must remain explicitly",
          "unbound and non-promotable."
        ),
        call. = FALSE
      )
    }
    expected_reproducibility <- FALSE
    expected_promotion <- FALSE
  }
  payload <- unclass(draws)
  semantic_digest <- payload$semantic_digest
  payload$semantic_digest <- NULL
  if (!identical(
        draws$reproducibility_eligible,
        expected_reproducibility
      ) ||
      !identical(draws$promotion_eligible, expected_promotion) ||
      !.rqr_dlm_is_sha256(semantic_digest) ||
      !identical(semantic_digest, .rqr_digest(payload))) {
    stop(
      paste(
        "The DLM posterior-draw digest or derived eligibility",
        "status is inconsistent."
      ),
      call. = FALSE
    )
  }
  invisible(draws)
}

#' Evaluate fitted RQR-DLM interval-root paths
#'
#' The method orders the two fitted root ordinates at every training time. It
#' does not propagate future states and does not define response-predictive
#' draws; use [rqr_forecast_roots()] for explicit future root evolution.
#'
#' @param object An `rqr_dlm_mcmc` fit.
#' @param nd Number of retained draws to use when `draws` is `NULL`.
#' @param draws Optional output from [rqr_posterior_draws()] for this fit. A
#'   plain list containing only the two time-by-draw root-ordinate matrices is
#'   also accepted for fitted-time evaluation, but is normalized to an
#'   explicitly unbound, non-reproducible, and non-promotable draw envelope.
#' @param seed Optional seed used only when draws are subsampled.
#' @param ... Reserved; supplying an argument is an error.
#' @return A typed, content-digested `rqr_dlm_prediction` containing fitted-time
#'   lower, upper, midpoint, and width root functionals and canonical source
#'   metadata. It never contains response-predictive draws.
#' @family RQR-DLM
#' @export
predict_interval.rqr_dlm_mcmc <- function(
    object, nd = NULL, draws = NULL, seed = NULL, ...) {
  .rqr_reject_dots(
    list(...), "predict_interval.rqr_dlm_mcmc"
  )
  .rqr_validate_dlm_fit_if_present(object)
  .rqr_validate_dlm_stored_draws(object)
  if (is.null(draws)) {
    draws <- rqr_posterior_draws(object, nd = nd, seed = seed)
  } else {
    if (!is.null(nd) || !is.null(seed)) {
      stop(
        "nd and seed must be NULL when explicit draws are supplied.",
        call. = FALSE
      )
    }
    draws <- .rqr_validate_dlm_draws(object, draws)
  }
  out <- .rqr_build_dlm_prediction(object, draws)
  .rqr_validate_dlm_prediction(object, out)
  out
}

.rqr_build_dlm_prediction <- function(object, draws) {
  .rqr_validate_typed_dlm_draws(object, draws)
  lower <- pmin(draws$eta_root1, draws$eta_root2)
  upper <- pmax(draws$eta_root1, draws$eta_root2)
  midpoint <- 0.5 * (lower + upper)
  width <- upper - lower
  out <- list(
    schema_version = .rqr_dlm_prediction_schema(),
    lower_draws = lower,
    upper_draws = upper,
    midpoint_draws = midpoint,
    width_draws = width,
    lower_mean = rowMeans(lower),
    upper_mean = rowMeans(upper),
    midpoint_mean = rowMeans(midpoint),
    width_mean = rowMeans(width),
    draws = draws,
    draw_index = draws$index,
    model_spec = object$model_spec,
    fit_checkpoint_digest = if (isTRUE(draws$source_bound)) {
      object$checkpoint_digest
    } else {
      NA_character_
    },
    retained_draws_digest = if (isTRUE(draws$source_bound)) {
      object$retained_draws_digest
    } else {
      NA_character_
    },
    fitted_design_digest = .rqr_digest(list(
      FF = object$expanded_model$FF,
      observed = object$misc$observed,
      response_length = length(object$y)
    )),
    source_bound = draws$source_bound,
    reproducibility_eligible = draws$reproducibility_eligible,
    promotion_eligible = draws$promotion_eligible,
    promotion_status = if (isTRUE(draws$source_bound)) {
      if (isTRUE(draws$promotion_eligible)) {
        "native_fit_bound_eligible"
      } else {
        "native_fit_bound_not_promotion_eligible"
      }
    } else {
      "external_draws_unbound_nonpromotable"
    },
    response_predictive_draws = FALSE,
    interpretation = paste(
      "Fitted-time interval-root functionals;",
      "no response draw is defined."
    )
  )
  out$semantic_digest <- .rqr_digest(out)
  class(out) <- c("rqr_dlm_prediction", "list")
  out
}

.rqr_validate_dlm_prediction <- function(object, prediction) {
  .rqr_validate_dlm_fit_if_present(object)
  .rqr_dlm_assert_exact_list_object(
    prediction, c("rqr_dlm_prediction", "list"),
    "RQR-DLM fitted prediction"
  )
  expected_fields <- c(
    "schema_version", "lower_draws", "upper_draws",
    "midpoint_draws", "width_draws", "lower_mean",
    "upper_mean", "midpoint_mean", "width_mean", "draws",
    "draw_index", "model_spec", "fit_checkpoint_digest",
    "retained_draws_digest", "fitted_design_digest",
    "source_bound", "reproducibility_eligible",
    "promotion_eligible", "promotion_status",
    "response_predictive_draws", "interpretation",
    "semantic_digest"
  )
  if (!identical(names(prediction), expected_fields) ||
      !identical(
        prediction$schema_version, .rqr_dlm_prediction_schema()
      )) {
    stop(
      "The RQR-DLM fitted prediction envelope is noncanonical.",
      call. = FALSE
    )
  }
  .rqr_validate_typed_dlm_draws(object, prediction$draws)
  expected <- .rqr_build_dlm_prediction(
    object, prediction$draws
  )
  if (!identical(prediction, expected)) {
    stop(
      paste(
        "The RQR-DLM fitted prediction roots, summaries, source",
        "binding, digest, or no-response semantics are inconsistent."
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.rqr_dlm_forecast_source <- function(
    object, forecast_state, full_fit) {
  terminal_digest <- .rqr_digest(list(
    terminal_root1 = forecast_state$terminal_root1,
    terminal_root2 = forecast_state$terminal_root2
  ))
  scale_digest <- if (is.null(forecast_state$evolution_scale)) {
    NA_character_
  } else {
    .rqr_digest(forecast_state$evolution_scale)
  }
  if (isTRUE(full_fit)) {
    digests <- object$provenance$object_digests
    return(list(
      schema_version = .rqr_dlm_forecast_source_schema(),
      binding_status = "fit_retained_draws",
      fit_schema_version = object$provenance$schema_version,
      fit_checkpoint_digest = object$checkpoint_digest,
      retained_draws_digest = object$retained_draws_digest,
      target_digest = digests$target,
      model_digest = digests$model,
      evolution_digest = digests$evolution,
      terminal_state_digest = terminal_digest,
      evolution_scale_digest = scale_digest,
      external_state_digest = NA_character_
    ))
  }
  list(
    schema_version = .rqr_dlm_forecast_source_schema(),
    binding_status = "unbound_external_state_fixture",
    fit_schema_version = NA_character_,
    fit_checkpoint_digest = NA_character_,
    retained_draws_digest = NA_character_,
    target_digest = NA_character_,
    model_digest = NA_character_,
    evolution_digest = NA_character_,
    terminal_state_digest = terminal_digest,
    evolution_scale_digest = scale_digest,
    external_state_digest = .rqr_digest(list(
      terminal_root1 = forecast_state$terminal_root1,
      terminal_root2 = forecast_state$terminal_root2,
      evolution_scale = forecast_state$evolution_scale,
      state_dimension = forecast_state$state_dimension,
      n_save = forecast_state$n_save,
      model_spec = object$model_spec,
      evolution = object$evolution %||% NULL
    ))
  )
}

.rqr_dlm_future_contract <- function(
    object, forecast_state, full_fit, FF_future, GG_future,
    W_future = NULL, component_templates_future = NULL,
    numerical_policy, jitter_ladder) {
  p <- forecast_state$state_dimension
  if (!is.matrix(FF_future) || !is.numeric(FF_future) ||
      is.object(FF_future)) {
    stop("FF_future must be a plain numeric p x H matrix.",
         call. = FALSE)
  }
  H <- ncol(FF_future)
  if (nrow(FF_future) != p || H < 1L ||
      any(!is.finite(FF_future))) {
    stop("FF_future must be finite p x H.", call. = FALSE)
  }
  storage.mode(FF_future) <- "double"
  GG <- .rqr_expand_cube(GG_future, H, p, "GG_future")
  if (!is.null(W_future) &&
      !is.null(component_templates_future)) {
    stop(
      "Supply W_future or component_templates_future, not both.",
      call. = FALSE
    )
  }
  if (is.null(W_future) &&
      is.null(component_templates_future)) {
    stop(
      "Supply W_future or component_templates_future.",
      call. = FALSE
    )
  }
  component_future <- !is.null(component_templates_future)
  if (component_future) {
    if (!identical(object$model_spec$evolution_mode, "component_scale") ||
        is.null(forecast_state$evolution_scale) ||
        !inherits(object$evolution, "rqr_evolution") ||
        !identical(object$evolution$mode, "component_scale")) {
      stop(
        paste(
          "component_templates_future requires a component_scale fit",
          "with saved scale draws."
        ),
        call. = FALSE
      )
    }
    future_evolution <- rqr_evolution_component_scale(
      templates = component_templates_future,
      component_dims = object$evolution$component_dims,
      prior = object$evolution$prior,
      initial = 1,
      component_names = object$evolution$component_names
    )
    templates <- .rqr_expand_component_templates(
      future_evolution, H, p
    )
    W <- NULL
    component_dims <- as.integer(
      future_evolution$component_dims
    )
    component_names <- as.character(
      future_evolution$component_names
    )
  } else {
    W <- .rqr_prepare_evolution(
      list(mode = "fixed_W", W = W_future), p, H
    )$W
    templates <- list()
    component_dims <- integer()
    component_names <- character()
  }
  numerical_policy <- .rqr_numerical_policy(numerical_policy)
  if (!is.numeric(jitter_ladder) || is.object(jitter_ladder) ||
      !is.null(dim(jitter_ladder)) || !length(jitter_ladder) ||
      any(!is.finite(jitter_ladder)) ||
      any(jitter_ladder < 0)) {
    stop(
      paste(
        "jitter_ladder must be a nonempty plain numeric vector of finite",
        "nonnegative values."
      ),
      call. = FALSE
    )
  }
  ladder <- .rqr_jitter_ladder(
    numerical_policy, jitter_ladder
  )
  contract <- list(
    schema_version = .rqr_dlm_future_contract_schema(),
    state_dimension = as.integer(p),
    horizon = as.integer(H),
    FF_future = FF_future,
    GG_future = GG,
    future_evolution_mode = if (component_future) {
      "component_scale"
    } else {
      "fixed_W"
    },
    W_future = W,
    component_templates_future = templates,
    component_dims = component_dims,
    component_names = component_names,
    parent_evolution_digest = if (isTRUE(full_fit)) {
      object$provenance$object_digests$evolution
    } else {
      NA_character_
    },
    numerical_policy = numerical_policy,
    jitter_ladder = as.numeric(ladder)
  )
  contract$semantic_digest <- .rqr_digest(contract)
  contract
}

.rqr_validate_dlm_future_contract <- function(
    contract, source, full_fit) {
  expected_fields <- c(
    "schema_version", "state_dimension", "horizon",
    "FF_future", "GG_future", "future_evolution_mode",
    "W_future", "component_templates_future",
    "component_dims", "component_names",
    "parent_evolution_digest", "numerical_policy",
    "jitter_ladder", "semantic_digest"
  )
  if (!is.list(contract) || is.object(contract) ||
      !identical(names(attributes(contract)), "names") ||
      !identical(names(contract), expected_fields) ||
      !identical(
        contract$schema_version,
        .rqr_dlm_future_contract_schema()
      )) {
    stop(
      "The future DLM root contract has an invalid schema.",
      call. = FALSE
    )
  }
  p <- .rqr_scalar_integer(
    contract$state_dimension,
    "future_contract$state_dimension", 1L
  )
  H <- .rqr_scalar_integer(
    contract$horizon, "future_contract$horizon", 1L
  )
  if (!is.matrix(contract$FF_future) ||
      !is.numeric(contract$FF_future) ||
      is.object(contract$FF_future) ||
      !identical(dim(contract$FF_future), c(p, H)) ||
      any(!is.finite(contract$FF_future)) ||
      !is.array(contract$GG_future) ||
      !is.numeric(contract$GG_future) ||
      is.object(contract$GG_future) ||
      !identical(dim(contract$GG_future), c(p, p, H)) ||
      any(!is.finite(contract$GG_future)) ||
      !identical(
        contract$numerical_policy,
        .rqr_numerical_policy(contract$numerical_policy)
      ) ||
      !is.numeric(contract$jitter_ladder) ||
      is.object(contract$jitter_ladder) ||
      !is.null(dim(contract$jitter_ladder)) ||
      !length(contract$jitter_ladder) ||
      any(!is.finite(contract$jitter_ladder)) ||
      any(contract$jitter_ladder < 0) ||
      !identical(
        contract$jitter_ladder,
        as.numeric(.rqr_jitter_ladder(
          contract$numerical_policy,
          contract$jitter_ladder
        ))
      )) {
    stop(
      "The future DLM design, transition, or numerical contract is invalid.",
      call. = FALSE
    )
  }
  if (!identical(
        contract$future_evolution_mode,
        "fixed_W"
      ) &&
      !identical(
        contract$future_evolution_mode,
        "component_scale"
      )) {
    stop("The future DLM evolution mode is invalid.",
         call. = FALSE)
  }
  if (identical(contract$future_evolution_mode, "fixed_W")) {
    canonical_W <- .rqr_validate_covariance_cube(
      contract$W_future, "future_contract$W_future"
    )
    if (!identical(dim(contract$W_future), c(p, p, H)) ||
        !identical(contract$W_future, canonical_W) ||
        !identical(contract$component_templates_future, list()) ||
        !identical(contract$component_dims, integer()) ||
        !identical(contract$component_names, character())) {
      stop(
        "The fixed-W future DLM evolution contract is inconsistent.",
        call. = FALSE
      )
    }
  } else {
    dims <- .rqr_positive_integer_vector(
      contract$component_dims,
      "future_contract$component_dims"
    )
    if (!is.null(contract$W_future) ||
        !is.list(contract$component_templates_future) ||
        is.object(contract$component_templates_future) ||
        length(contract$component_templates_future) !=
          length(dims) ||
        sum(dims) != p ||
        !is.character(contract$component_names) ||
        length(contract$component_names) != length(dims) ||
        anyNA(contract$component_names) ||
        any(!nzchar(contract$component_names)) ||
        anyDuplicated(contract$component_names)) {
      stop(
        "The component-scale future DLM evolution contract is inconsistent.",
        call. = FALSE
      )
    }
    for (j in seq_along(dims)) {
      template <- contract$component_templates_future[[j]]
      if (!is.array(template) || !is.numeric(template) ||
          is.object(template) ||
          !identical(dim(template), c(dims[j], dims[j], H))) {
        stop(
          "A future component template has an invalid dimension.",
          call. = FALSE
        )
      }
      canonical_template <- .rqr_validate_covariance_cube(
        template,
        sprintf(
          "future_contract$component_templates_future[[%d]]", j
        )
      )
      if (!identical(template, canonical_template)) {
        stop(
          "A future component template is not in canonical symmetric form.",
          call. = FALSE
        )
      }
    }
  }
  expected_parent <- if (isTRUE(full_fit)) {
    source$evolution_digest
  } else {
    NA_character_
  }
  payload <- contract
  semantic_digest <- payload$semantic_digest
  payload$semantic_digest <- NULL
  if (!identical(
        contract$parent_evolution_digest,
        expected_parent
      ) ||
      !.rqr_dlm_is_sha256(semantic_digest) ||
      !identical(semantic_digest, .rqr_digest(payload))) {
    stop(
      "The future DLM contract digest or parent binding is invalid.",
      call. = FALSE
    )
  }
  invisible(contract)
}

.rqr_empty_dlm_forecast_repair_records <- function() {
  base <- .rqr_empty_repair_records()
  data.frame(
    draw = integer(0),
    root = integer(0),
    base,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

.rqr_dlm_forecast_kernel <- function(
    forecast_state, future_contract, index) {
  p <- future_contract$state_dimension
  H <- future_contract$horizon
  root1 <- root2 <- matrix(
    NA_real_, H, length(index)
  )
  repairs <- .rqr_empty_dlm_forecast_repair_records()
  for (j in seq_along(index)) {
    s1 <- forecast_state$terminal_root1[, index[j]]
    s2 <- forecast_state$terminal_root2[, index[j]]
    W_draw <- if (identical(
      future_contract$future_evolution_mode,
      "component_scale"
    )) {
      .rqr_component_W_from_expanded_templates(
        templates =
          future_contract$component_templates_future,
        component_dims = future_contract$component_dims,
        q = forecast_state$evolution_scale[index[j], ],
        n_time = H,
        p = p
      )
    } else {
      future_contract$W_future
    }
    for (hh in seq_len(H)) {
      mu1 <- drop(
        future_contract$GG_future[, , hh] %*% s1
      )
      mu2 <- drop(
        future_contract$GG_future[, , hh] %*% s2
      )
      d1 <- .rqr_sample_mvnorm_covariance(
        mu1, W_draw[, , hh],
        future_contract$jitter_ladder,
        future_contract$numerical_policy
      )
      d2 <- .rqr_sample_mvnorm_covariance(
        mu2, W_draw[, , hh],
        future_contract$jitter_ladder,
        future_contract$numerical_policy
      )
      s1 <- d1$draw
      s2 <- d2$draw
      for (root in 1:2) {
        info <- if (root == 1L) d1$info else d2$info
        row <- .rqr_add_repair_record(
          .rqr_empty_repair_records(),
          paste0("future_state_covariance_root", root),
          hh, info
        )
        if (nrow(row)) {
          row <- data.frame(
            draw = as.integer(j),
            root = as.integer(root),
            row,
            stringsAsFactors = FALSE,
            check.names = FALSE
          )
          repairs <- rbind(repairs, row)
        }
      }
      root1[hh, j] <- drop(crossprod(
        future_contract$FF_future[, hh], s1
      ))
      root2[hh, j] <- drop(crossprod(
        future_contract$FF_future[, hh], s2
      ))
    }
  }
  list(
    eta_root1 = root1,
    eta_root2 = root2,
    repair_records = repairs
  )
}

#' Draw future root trajectories from an RQR-DLM fit
#'
#' These are state-evolution draws for interval roots, not predictive response
#' draws. Future evolution covariances are explicit and validated.
#'
#' @param object An `rqr_dlm_mcmc` fit.
#' @param FF_future State-by-horizon observation design.
#' @param GG_future Evolution matrix or cube.
#' @param W_future Explicit evolution covariance matrix or cube. Supply either
#'   this argument or `component_templates_future`.
#' @param component_templates_future Optional fixed future component templates.
#'   For a component-scale fit, these are combined with the saved draw-specific
#'   evolution multipliers.
#' @param nd Number of saved MCMC draws to use.
#' @param seed Optional seed binding draw selection and all future state
#'   innovations. An omitted seed consumes ambient RNG and makes the forecast
#'   envelope non-reproducible and non-promotable even when the parent fit is
#'   eligible.
#' @param numerical_policy Either `"fail"` or `"record_repair"`.
#' @param jitter_ladder Matrix-relative jitter ladder for record-repair mode.
#'   An exactly zero matrix uses a separately recorded absolute fallback.
#' @return A typed, content-digested `rqr_dlm_forecast` containing future root
#'   and ordered endpoint draws, exact future-input and RNG contracts, repair
#'   diagnostics, and an explicit retained-draw binding status. Hand-built
#'   state-only fixtures remain unbound and non-promotable.
#' @family RQR-DLM
#' @export
rqr_forecast_roots <- function(
    object, FF_future, GG_future, W_future = NULL,
    component_templates_future = NULL, nd = NULL, seed = NULL,
    numerical_policy = object$model_spec$numerical_policy %||% "fail",
    jitter_ladder = object$misc$jitter_ladder %||% c(0, 1e-12, 1e-10, 1e-8, 1e-6)) {
  full_fit <- .rqr_validate_dlm_fit_if_present(
    object, allow_state_only = TRUE
  )
  forecast_state <- .rqr_validate_dlm_forecast_state(
    object, full_fit = full_fit
  )
  source <- .rqr_dlm_forecast_source(
    object, forecast_state, full_fit
  )
  future_contract <- .rqr_dlm_future_contract(
    object = object,
    forecast_state = forecast_state,
    full_fit = full_fit,
    FF_future = FF_future,
    GG_future = GG_future,
    W_future = W_future,
    component_templates_future =
      component_templates_future,
    numerical_policy = numerical_policy,
    jitter_ladder = jitter_ladder
  )
  n_save <- forecast_state$n_save
  if (!is.null(seed)) {
    seed <- .rqr_scalar_integer(seed, "seed", 0L)
  }
  if (!is.null(nd)) {
    nd <- .rqr_scalar_integer(nd, "nd", 1L)
  }
  execute <- function() {
    before <- .rqr_rng_state()
    idx <- if (is.null(nd)) {
      seq_len(n_save)
    } else {
      sample.int(n_save, nd, replace = nd > n_save)
    }
    kernel <- .rqr_dlm_forecast_kernel(
      forecast_state, future_contract, as.integer(idx)
    )
    list(
      index = as.integer(idx),
      kernel = kernel,
      rng_state_before = before,
      rng_state_after = .rqr_rng_state()
    )
  }
  if (!is.null(seed)) {
    generated <- .rqr_dlm_preserve_rng(function() {
      set.seed(seed)
      execute()
    })
    rng_mode <- "explicit_seed"
  } else {
    generated <- execute()
    rng_mode <- "ambient_rng"
  }
  rng_binding <- .rqr_dlm_rng_binding(
    operation = "future_root_simulation",
    mode = rng_mode,
    seed = seed %||% NA_integer_,
    rng_state_before = generated$rng_state_before,
    rng_state_after = generated$rng_state_after,
    selection_mode = if (is.null(nd)) "all" else "subsample",
    requested_draw_count = nd %||% NA_integer_,
    sampling_with_replacement = if (is.null(nd)) {
      NA
    } else {
      nd > n_save
    }
  )
  root1 <- generated$kernel$eta_root1
  root2 <- generated$kernel$eta_root2
  repairs <- generated$kernel$repair_records
  idx <- generated$index
  lower <- pmin(root1, root2)
  upper <- pmax(root1, root2)
  midpoint <- 0.5 * (lower + upper)
  width <- upper - lower
  source_bound <- isTRUE(full_fit)
  reproducibility_eligible <- source_bound &&
    isTRUE(object$model_spec$reproducibility_eligible) &&
    isTRUE(rng_binding$reproducibility_bound)
  promotion_eligible <- source_bound &&
    isTRUE(object$model_spec$promotion_eligible) &&
    isTRUE(rng_binding$reproducibility_bound) &&
    !nrow(repairs)
  out <- list(
    schema_version = .rqr_dlm_forecast_schema(),
    eta_root1 = root1,
    eta_root2 = root2,
    lower_draws = lower,
    upper_draws = upper,
    midpoint_draws = midpoint,
    width_draws = width,
    lower_mean = rowMeans(lower),
    upper_mean = rowMeans(upper),
    midpoint_mean = rowMeans(midpoint),
    width_mean = rowMeans(width),
    draw_index = idx,
    draw_binding_status = forecast_state$draw_binding_status,
    source = source,
    future_contract = future_contract,
    rng_binding = rng_binding,
    diagnostics = list(
      numerical_policy = future_contract$numerical_policy,
      repair_count = as.integer(nrow(repairs)),
      repair_records = repairs,
      future_evolution_mode =
        future_contract$future_evolution_mode,
      draw_binding_status = forecast_state$draw_binding_status,
      component_scale_draws = if (identical(
        future_contract$future_evolution_mode,
        "component_scale"
      )) {
        forecast_state$evolution_scale[idx, , drop = FALSE]
      } else {
        NULL
      }
    ),
    source_bound = source_bound,
    parent_fit_reproducibility_eligible =
      source_bound &&
      isTRUE(object$model_spec$reproducibility_eligible),
    parent_fit_promotion_eligible =
      source_bound &&
      isTRUE(object$model_spec$promotion_eligible),
    reproducibility_eligible = reproducibility_eligible,
    promotion_eligible = promotion_eligible,
    promotion_status = if (!source_bound) {
      "external_state_fixture_unbound_nonpromotable"
    } else if (!isTRUE(rng_binding$reproducibility_bound)) {
      "ambient_rng_nonpromotable"
    } else if (nrow(repairs)) {
      "future_numerical_repairs_nonpromotable"
    } else if (promotion_eligible) {
      "fit_and_future_rng_bound_eligible"
    } else {
      "parent_fit_not_promotion_eligible"
    },
    response_predictive_draws = FALSE,
    interpretation = "Future interval-root state draws; no response simulation contract is implied."
  )
  out$semantic_digest <- .rqr_digest(out)
  class(out) <- c("rqr_dlm_forecast", "list")
  .rqr_validate_dlm_forecast(object, out)
  out
}

.rqr_validate_dlm_forecast <- function(object, forecast) {
  full_fit <- .rqr_validate_dlm_fit_if_present(
    object, allow_state_only = TRUE
  )
  forecast_state <- .rqr_validate_dlm_forecast_state(
    object, full_fit = full_fit
  )
  .rqr_dlm_assert_exact_list_object(
    forecast, c("rqr_dlm_forecast", "list"),
    "RQR-DLM future-root forecast"
  )
  expected_fields <- c(
    "schema_version", "eta_root1", "eta_root2",
    "lower_draws", "upper_draws", "midpoint_draws",
    "width_draws", "lower_mean", "upper_mean",
    "midpoint_mean", "width_mean", "draw_index",
    "draw_binding_status", "source", "future_contract",
    "rng_binding", "diagnostics", "source_bound",
    "parent_fit_reproducibility_eligible",
    "parent_fit_promotion_eligible",
    "reproducibility_eligible", "promotion_eligible",
    "promotion_status", "response_predictive_draws",
    "interpretation", "semantic_digest"
  )
  if (!identical(names(forecast), expected_fields) ||
      !identical(
        forecast$schema_version, .rqr_dlm_forecast_schema()
      )) {
    stop(
      "The RQR-DLM future-root envelope is noncanonical.",
      call. = FALSE
    )
  }
  expected_source <- .rqr_dlm_forecast_source(
    object, forecast_state, full_fit
  )
  if (!is.list(forecast$source) ||
      is.object(forecast$source) ||
      !identical(names(attributes(forecast$source)), "names") ||
      !identical(forecast$source, expected_source)) {
    stop(
      "The future-root output is not bound to its exact source state.",
      call. = FALSE
    )
  }
  .rqr_validate_dlm_future_contract(
    forecast$future_contract,
    source = forecast$source,
    full_fit = full_fit
  )
  if (!identical(
        forecast$future_contract$state_dimension,
        forecast_state$state_dimension
      )) {
    stop(
      "The future-root state dimension differs from its source.",
      call. = FALSE
    )
  }
  .rqr_validate_dlm_rng_binding(
    forecast$rng_binding,
    operation = "future_root_simulation"
  )
  if (!forecast$rng_binding$mode %in%
      c("explicit_seed", "ambient_rng")) {
    stop(
      "Future root simulation requires an explicit or ambient RNG.",
      call. = FALSE
    )
  }
  H <- forecast$future_contract$horizon
  index <- forecast$draw_index
  if (!is.integer(index) || is.object(index) ||
      !is.null(dim(index)) || !length(index) ||
      anyNA(index) || any(index < 1L) ||
      any(index > forecast_state$n_save)) {
    stop("The future-root draw indices are invalid.",
         call. = FALSE)
  }
  n_draw <- length(index)
  selection_valid <- if (identical(
    forecast$rng_binding$selection_mode, "all"
  )) {
    identical(index, seq_len(forecast_state$n_save)) &&
      identical(n_draw, forecast_state$n_save) &&
      is.na(forecast$rng_binding$requested_draw_count) &&
      is.na(forecast$rng_binding$sampling_with_replacement)
  } else {
    identical(
      forecast$rng_binding$selection_mode, "subsample"
    ) &&
      identical(
        forecast$rng_binding$requested_draw_count,
        as.integer(n_draw)
      ) &&
      identical(
        forecast$rng_binding$sampling_with_replacement,
        n_draw > forecast_state$n_save
      )
  }
  if (!selection_valid) {
    stop(
      "The future-root draw-selection contract is invalid.",
      call. = FALSE
    )
  }
  numeric_matrices <- c(
    "eta_root1", "eta_root2", "lower_draws",
    "upper_draws", "midpoint_draws", "width_draws"
  )
  matrix_valid <- vapply(
    numeric_matrices,
    function(field) {
      value <- forecast[[field]]
      is.matrix(value) && is.numeric(value) &&
        !is.object(value) &&
        identical(dim(value), c(H, n_draw)) &&
        all(is.finite(value))
    },
    logical(1L)
  )
  summary_fields <- c(
    "lower_mean", "upper_mean",
    "midpoint_mean", "width_mean"
  )
  summary_valid <- vapply(
    summary_fields,
    function(field) {
      value <- forecast[[field]]
      is.numeric(value) && !is.object(value) &&
        is.null(dim(value)) && length(value) == H &&
        all(is.finite(value))
    },
    logical(1L)
  )
  lower <- pmin(forecast$eta_root1, forecast$eta_root2)
  upper <- pmax(forecast$eta_root1, forecast$eta_root2)
  midpoint <- 0.5 * (lower + upper)
  width <- upper - lower
  if (!all(matrix_valid) || !all(summary_valid) ||
      !identical(forecast$lower_draws, lower) ||
      !identical(forecast$upper_draws, upper) ||
      !identical(forecast$midpoint_draws, midpoint) ||
      !identical(forecast$width_draws, width) ||
      !identical(forecast$lower_mean, rowMeans(lower)) ||
      !identical(forecast$upper_mean, rowMeans(upper)) ||
      !identical(
        forecast$midpoint_mean, rowMeans(midpoint)
      ) ||
      !identical(forecast$width_mean, rowMeans(width))) {
    stop(
      "The future-root draws or ordered summaries are inconsistent.",
      call. = FALSE
    )
  }
  expected_diagnostics_fields <- c(
    "numerical_policy", "repair_count", "repair_records",
    "future_evolution_mode", "draw_binding_status",
    "component_scale_draws"
  )
  diagnostics <- forecast$diagnostics
  repairs <- diagnostics$repair_records
  empty_repairs <- .rqr_empty_dlm_forecast_repair_records()
  if (!is.list(diagnostics) || is.object(diagnostics) ||
      !identical(names(attributes(diagnostics)), "names") ||
      !identical(names(diagnostics), expected_diagnostics_fields) ||
      !is.data.frame(repairs) ||
      !identical(names(repairs), names(empty_repairs)) ||
      !identical(
        vapply(repairs, typeof, character(1L)),
        vapply(empty_repairs, typeof, character(1L))
      ) ||
      !is.integer(diagnostics$repair_count) ||
      length(diagnostics$repair_count) != 1L ||
      !identical(
        diagnostics$repair_count,
        as.integer(nrow(repairs))
      ) ||
      !identical(
        diagnostics$numerical_policy,
        forecast$future_contract$numerical_policy
      ) ||
      !identical(
        diagnostics$future_evolution_mode,
        forecast$future_contract$future_evolution_mode
      ) ||
      !identical(
        diagnostics$draw_binding_status,
        forecast_state$draw_binding_status
      )) {
    stop(
      "The future-root diagnostic or repair envelope is invalid.",
      call. = FALSE
    )
  }
  if (nrow(repairs)) {
    base <- repairs[
      setdiff(names(repairs), c("draw", "root")),
      drop = FALSE
    ]
    .rqr_validate_repair_records(
      base, "future-root numerical repair ledger"
    )
    if (anyNA(repairs$draw) || any(repairs$draw < 1L) ||
        any(repairs$draw > n_draw) ||
        anyNA(repairs$root) ||
        any(!repairs$root %in% 1:2)) {
      stop(
        "The future-root repair draw or root index is invalid.",
        call. = FALSE
      )
    }
  }
  component_future <- identical(
    forecast$future_contract$future_evolution_mode,
    "component_scale"
  )
  expected_scales <- if (component_future) {
    forecast_state$evolution_scale[index, , drop = FALSE]
  } else {
    NULL
  }
  if (!identical(
        diagnostics$component_scale_draws,
        expected_scales
      ) ||
      !identical(
        forecast$draw_binding_status,
        forecast_state$draw_binding_status
      )) {
    stop(
      "The future-root retained-draw or component-scale binding changed.",
      call. = FALSE
    )
  }

  # Replay the complete draw-selection and future-evolution transition when
  # an initial RNG state is available. Explicit seeds always supply one.
  can_replay <- !is.null(
    forecast$rng_binding$rng_state_before
  )
  if (can_replay) {
    replay <- .rqr_dlm_preserve_rng(function() {
      if (identical(
        forecast$rng_binding$mode, "explicit_seed"
      )) {
        set.seed(forecast$rng_binding$seed)
      } else {
        .rqr_restore_rng(
          forecast$rng_binding$rng_state_before
        )
      }
      if (!identical(
        .rqr_rng_state(),
        forecast$rng_binding$rng_state_before
      )) {
        stop(
          "The future-root RNG start state is not reproducible.",
          call. = FALSE
        )
      }
      replay_index <- if (identical(
        forecast$rng_binding$selection_mode, "all"
      )) {
        if (!identical(n_draw, forecast_state$n_save) ||
            !identical(
              index, seq_len(forecast_state$n_save)
            )) {
          stop(
            "The all-draw future selection is inconsistent.",
            call. = FALSE
          )
        }
        seq_len(forecast_state$n_save)
      } else {
        if (!identical(
              forecast$rng_binding$selection_mode,
              "subsample"
            ) ||
            !identical(
              forecast$rng_binding$requested_draw_count,
              as.integer(n_draw)
            ) ||
            !identical(
              forecast$rng_binding$sampling_with_replacement,
              n_draw > forecast_state$n_save
            )) {
          stop(
            "The future-root draw-selection contract is invalid.",
            call. = FALSE
          )
        }
        sample.int(
          forecast_state$n_save, n_draw,
          replace = n_draw > forecast_state$n_save
        )
      }
      kernel <- .rqr_dlm_forecast_kernel(
        forecast_state, forecast$future_contract,
        as.integer(replay_index)
      )
      list(
        index = as.integer(replay_index),
        kernel = kernel,
        after = .rqr_rng_state()
      )
    })
    if (!identical(
          replay$after,
          forecast$rng_binding$rng_state_after
        ) ||
        !identical(replay$index, index) ||
        !identical(
          replay$kernel$eta_root1, forecast$eta_root1
        ) ||
        !identical(
          replay$kernel$eta_root2, forecast$eta_root2
        ) ||
        !identical(
          replay$kernel$repair_records, repairs
        )) {
      stop(
        paste(
          "The future-root output is not the recorded RNG transition",
          "from its exact source state and future-input contract."
        ),
        call. = FALSE
      )
    }
  }
  source_bound <- isTRUE(full_fit)
  expected_parent_repro <- source_bound &&
    isTRUE(object$model_spec$reproducibility_eligible)
  expected_parent_promotion <- source_bound &&
    isTRUE(object$model_spec$promotion_eligible)
  expected_repro <- expected_parent_repro &&
    isTRUE(forecast$rng_binding$reproducibility_bound)
  expected_promotion <- expected_parent_promotion &&
    isTRUE(forecast$rng_binding$reproducibility_bound) &&
    !nrow(repairs)
  expected_status <- if (!source_bound) {
    "external_state_fixture_unbound_nonpromotable"
  } else if (!isTRUE(
    forecast$rng_binding$reproducibility_bound
  )) {
    "ambient_rng_nonpromotable"
  } else if (nrow(repairs)) {
    "future_numerical_repairs_nonpromotable"
  } else if (expected_promotion) {
    "fit_and_future_rng_bound_eligible"
  } else {
    "parent_fit_not_promotion_eligible"
  }
  payload <- unclass(forecast)
  semantic_digest <- payload$semantic_digest
  payload$semantic_digest <- NULL
  if (!identical(forecast$source_bound, source_bound) ||
      !identical(
        forecast$parent_fit_reproducibility_eligible,
        expected_parent_repro
      ) ||
      !identical(
        forecast$parent_fit_promotion_eligible,
        expected_parent_promotion
      ) ||
      !identical(
        forecast$reproducibility_eligible,
        expected_repro
      ) ||
      !identical(
        forecast$promotion_eligible,
        expected_promotion
      ) ||
      !identical(forecast$promotion_status, expected_status) ||
      !identical(forecast$response_predictive_draws, FALSE) ||
      !identical(
        forecast$interpretation,
        paste(
          "Future interval-root state draws;",
          "no response simulation contract is implied."
        )
      ) ||
      !.rqr_dlm_is_sha256(semantic_digest) ||
      !identical(semantic_digest, .rqr_digest(payload))) {
    stop(
      paste(
        "The future-root source eligibility, interpretation,",
        "or content digest is inconsistent."
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Print an RQR-DLM MCMC summary
#'
#' @param x An `rqr_dlm_mcmc` fit.
#' @param ... Reserved; supplying an argument is an error.
#' @return `x`, invisibly.
#' @export
print.rqr_dlm_mcmc <- function(x, ...) {
  .rqr_reject_dots(list(...), "print.rqr_dlm_mcmc")
  .rqr_validate_dlm_fit_if_present(x)
  stored <- .rqr_validate_dlm_stored_draws(x)
  cat("RQR dynamic MCMC fit\n")
  cat(sprintf("  coverage_level: %.4f\n", x$model_spec$coverage_level))
  cat(sprintf("  evolution_mode: %s\n", x$model_spec$evolution_mode))
  cat(sprintf("  target contract: %s\n", x$model_spec$target_contract))
  cat(sprintf("  numerical repairs: %d\n", x$model_spec$numerical_repair_count))
  cat(sprintf("  promotion eligible: %s\n", if (x$model_spec$promotion_eligible) "yes" else "no"))
  cat(sprintf("  draws:          %d\n", ncol(stored$eta_root1)))
  cat("  interpretation: generalized-Bayes root paths, not response draws\n")
  invisible(x)
}
