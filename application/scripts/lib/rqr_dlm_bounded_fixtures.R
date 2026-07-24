# Canonical construction for the bounded dynamic RQR-DLM fixtures.
#
# Preflight and the eventual bounded runner must both call these functions.
# This prevents a shallow configuration check from approving objects that the
# public model, evolution, missingness, or forecast interfaces would reject.

`%||%` <- function(x, y) if (is.null(x)) y else x

rqr_validate_bounded_dlm_config <- function(config) {
  required <- c(
    "schema_version", "config_id", "scope", "generalized_bayes",
    "response_likelihood", "response_prediction_contract",
    "production_simulation_authorized",
    "bounded_dynamic_execution_authorized",
    "benchmark_one_cell_authorized", "runner_modes",
    "coverage_level", "learning_rate_modes", "fixed_learning_rate",
    "loss_reference_scale", "lambda_prior", "mcmc", "seeds",
    "continuation", "resources", "benchmark", "gates", "fixtures"
  )
  if (!is.list(config) || !all(required %in% names(config)) ||
      !identical(
        config$schema_version,
        "rqrgibbs_dlm_bounded_fixtures/5.0.0"
      ) ||
      !isTRUE(config$generalized_bayes) ||
      isTRUE(config$response_likelihood) ||
      isTRUE(config$response_prediction_contract) ||
      isTRUE(config$production_simulation_authorized) ||
      !identical(
        config$runner_modes,
        c(
          "preflight", "reference-only", "benchmark-one-cell",
          "execute-bounded"
        )
      )) {
    stop("The bounded configuration interpretation is invalid.", call. = FALSE)
  }
  scalar_positive <- function(value) {
    is.numeric(value) && length(value) == 1L &&
      !is.na(value) && is.finite(value) && value > 0
  }
  scalar_integer <- function(value, minimum = 0L) {
    is.numeric(value) && length(value) == 1L &&
      !is.na(value) && is.finite(value) &&
      value == as.integer(value) && value >= minimum
  }
  if (!scalar_positive(config$coverage_level) ||
      config$coverage_level >= 1 ||
      !identical(
        config$learning_rate_modes,
        c("fixed_rate", "learned_pseudoresidual_normalized")
      ) ||
      !scalar_positive(config$fixed_learning_rate) ||
      !scalar_positive(config$loss_reference_scale) ||
      !is.list(config$lambda_prior) ||
      !scalar_positive(config$lambda_prior$shape) ||
      !scalar_positive(config$lambda_prior$rate)) {
    stop("The bounded target configuration is invalid.", call. = FALSE)
  }
  mcmc <- config$mcmc
  mcmc_required <- c(
    "chains", "burn_in", "retained_per_chain", "thin", "seeds",
    "backend", "numerical_policy", "store_state_draws",
    "store_latent_draws", "initialization_profiles"
  )
  if (!is.list(mcmc) || !all(mcmc_required %in% names(mcmc)) ||
      !identical(mcmc$chains, 4L) ||
      !scalar_integer(mcmc$burn_in) ||
      !scalar_integer(mcmc$retained_per_chain, 1L) ||
      !scalar_integer(mcmc$thin, 1L) ||
      length(mcmc$seeds) != mcmc$chains ||
      anyNA(mcmc$seeds) ||
      any(mcmc$seeds != as.integer(mcmc$seeds)) ||
      anyDuplicated(mcmc$seeds) ||
      !identical(mcmc$backend, "cpp") ||
      !identical(mcmc$numerical_policy, "fail") ||
      !identical(mcmc$burn_in, 2000L) ||
      !identical(mcmc$retained_per_chain, 6000L) ||
      !identical(mcmc$thin, 1L) ||
      !isTRUE(mcmc$store_state_draws) ||
      isTRUE(mcmc$store_latent_draws) ||
      length(mcmc$initialization_profiles) != mcmc$chains) {
    stop("The bounded MCMC configuration is invalid.", call. = FALSE)
  }
  profile_names <- c(
    "lower_root_shift", "upper_root_shift", "lambda_initial",
    "component_scale_multiplier"
  )
  profile_valid <- vapply(mcmc$initialization_profiles, function(profile) {
    is.list(profile) &&
      identical(sort(names(profile)), sort(profile_names)) &&
      all(is.finite(unlist(profile, use.names = FALSE))) &&
      profile$lower_root_shift < profile$upper_root_shift &&
      profile$lambda_initial > 0 &&
      profile$component_scale_multiplier > 0
  }, logical(1L))
  seed_values <- unlist(config$seeds, use.names = FALSE)
  if (!all(profile_valid) || !length(seed_values) ||
      anyNA(seed_values) || any(!is.finite(seed_values)) ||
      any(seed_values != as.integer(seed_values)) ||
      any(seed_values <= 0) || anyDuplicated(seed_values)) {
    stop("The bounded initialization or seed contract is invalid.", call. = FALSE)
  }
  continuation <- config$continuation
  if (!is.list(continuation) ||
      !identical(continuation$history_segments, 3L) ||
      !identical(continuation$generation_indices, 0:2) ||
      !identical(
        continuation$retained_by_segment, c(2L, 2L, 2L)
      ) ||
      !identical(continuation$uninterrupted_retained, 6L) ||
      !isTRUE(continuation$require_checkpoint_digest) ||
      !isTRUE(continuation$require_history_digest)) {
    stop("The bounded continuation contract is invalid.", call. = FALSE)
  }
  resources <- config$resources
  if (!is.list(resources) ||
      !isTRUE(resources$sequential_execution) ||
      !scalar_integer(resources$hard_timeout_minutes, 1L) ||
      !identical(resources$hard_timeout_minutes, 240L) ||
      !scalar_positive(
        resources$maximum_sampled_process_group_rss_gib
      ) ||
      !scalar_integer(resources$maximum_process_tree_threads, 1L) ||
      !scalar_integer(resources$maximum_process_tree_processes, 1L) ||
      !scalar_positive(resources$monitor_interval_seconds) ||
      !isTRUE(resources$require_active_process_tree_monitor) ||
      !identical(resources$monitor_kind, "pgid_sampled_fallback") ||
      !identical(resources$kernel_hard_memory_ceiling, FALSE)) {
    stop("The bounded resource contract is invalid.", call. = FALSE)
  }
  benchmark <- config$benchmark
  if (!is.list(benchmark) ||
      !isTRUE(config$benchmark_one_cell_authorized) ||
      !identical(
        benchmark$fixture_id,
        "shared_component_scale_trend_regression"
      ) ||
      !identical(
        benchmark$learning_rate_mode,
        "learned_pseudoresidual_normalized"
      ) ||
      !identical(benchmark$chains, 4L) ||
      !isTRUE(benchmark$use_full_mcmc_schedule) ||
      !identical(
        benchmark$purpose,
        "representative_full_cell_timing_and_storage_only"
      )) {
    stop("The bounded one-cell benchmark contract is invalid.", call. = FALSE)
  }
  gates <- config$gates
  if (!is.list(gates) ||
      !scalar_positive(gates$maximum_rank_normalized_rhat) ||
      gates$maximum_rank_normalized_rhat > 1.01 ||
      !scalar_integer(gates$minimum_bulk_ess, 1L) ||
      !scalar_integer(gates$minimum_tail_ess, 1L) ||
      !identical(gates$maximum_numerical_repairs, 0L) ||
      !isTRUE(gates$require_primary_runtime_source_match) ||
      !isTRUE(gates$require_exact_joint_target) ||
      !identical(gates$root_swap_activity_role, "sidecar_only") ||
      !identical(
        gates$primary_diagnostics,
        "posterior_rank_normalized_rhat_bulk_tail_ess"
      ) ||
      !identical(gates$mcse_provider, "posterior_mcse_mean") ||
      !identical(gates$fixed_rate_lambda_gate, "exact_identity") ||
      !isTRUE(gates$require_three_segment_continuation)) {
    stop("The bounded diagnostic gate contract is invalid.", call. = FALSE)
  }
  modes <- vapply(
    config$fixtures,
    function(fixture) as.character(fixture$evolution_mode)[1L],
    character(1L)
  )
  if (length(config$fixtures) != 3L ||
      !setequal(
        modes, c("fixed_W", "discount_template", "component_scale")
      ) ||
      any(modes == "adaptive_discount") ||
      any(!vapply(config$fixtures, function(fixture) {
        scalar_integer(fixture$n_time, 2L) &&
          scalar_integer(fixture$future_horizon, 1L) &&
          length(fixture$y) == fixture$n_time &&
          all(is.finite(fixture$y))
      }, logical(1L)))) {
    stop("The bounded fixture declarations are invalid.", call. = FALSE)
  }
  invisible(TRUE)
}

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

rqr_bounded_initialization <- function(
    constructed_fixture, profile, coverage_level) {
  if (!is.list(constructed_fixture) || !is.list(profile)) {
    stop("A constructed fixture and initialization profile are required.", call. = FALSE)
  }
  expanded <- constructed_fixture$expanded_model
  paths <- rqrgibbs:::.rqr_init_state_paths(
    constructed_fixture$y,
    expanded$FF,
    expanded$m0,
    coverage_level,
    init = list()
  )
  shift_path <- function(path, amount) {
    for (time in seq_len(ncol(path))) {
      direction <- expanded$FF[, time]
      norm2 <- sum(direction^2)
      if (!is.finite(norm2) || norm2 <= 0) {
        stop("A fixture has a zero observation direction.", call. = FALSE)
      }
      path[, time] <- path[, time] + direction * amount / norm2
    }
    path
  }
  initial <- list(
    state_root1 = shift_path(
      paths$theta1, as.numeric(profile$lower_root_shift)
    ),
    state_root2 = shift_path(
      paths$theta2, as.numeric(profile$upper_root_shift)
    ),
    lambda = as.numeric(profile$lambda_initial)
  )
  first_direction <- expanded$FF[, 1L]
  first_norm2 <- sum(first_direction^2)
  initial$theta0_root1 <- expanded$m0 +
    first_direction * profile$lower_root_shift / first_norm2
  initial$theta0_root2 <- expanded$m0 +
    first_direction * profile$upper_root_shift / first_norm2
  if (identical(constructed_fixture$evolution$mode, "component_scale")) {
    initial$evolution_scale <-
      constructed_fixture$evolution$initial *
      as.numeric(profile$component_scale_multiplier)
  }
  initial
}

rqr_bounded_fit_arguments <- function(
    constructed_fixture, config, learning_rate_mode, chain_index,
    provenance_control, n_burn = config$mcmc$burn_in,
    n_mcmc = config$mcmc$retained_per_chain) {
  chain_index <- as.integer(chain_index)
  if (length(chain_index) != 1L || is.na(chain_index) ||
      chain_index < 1L || chain_index > config$mcmc$chains) {
    stop("chain_index is outside the frozen chain contract.", call. = FALSE)
  }
  profile <- config$mcmc$initialization_profiles[[chain_index]]
  list(
    y = constructed_fixture$y,
    model = constructed_fixture$model,
    coverage_level = config$coverage_level,
    evolution_spec = constructed_fixture$evolution,
    learning_rate = config$fixed_learning_rate,
    lambda_initial = profile$lambda_initial,
    loss_reference_scale = config$loss_reference_scale,
    learning_rate_mode = learning_rate_mode,
    lambda_prior = config$lambda_prior,
    numerical_policy = config$mcmc$numerical_policy,
    provenance_control = provenance_control,
    mcmc_control = list(
      n_burn = as.integer(n_burn),
      n_mcmc = as.integer(n_mcmc),
      thin = config$mcmc$thin,
      seed = config$mcmc$seeds[chain_index],
      backend = config$mcmc$backend,
      store_state_draws = config$mcmc$store_state_draws,
      store_latent_draws = config$mcmc$store_latent_draws
    ),
    init = rqr_bounded_initialization(
      constructed_fixture, profile, config$coverage_level
    )
  )
}
