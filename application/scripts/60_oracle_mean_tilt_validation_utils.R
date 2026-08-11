# Utilities for repeated-DGP validation of exact, fixed mean tilts.
#
# The asymmetric-Laplace distribution is the declared data-generating law.
# Every fitted model remains a loss-based generalized-Bayes update based on
# the pseudo-AL augmentation of the RQR loss; no response-likelihood or
# posterior-predictive response interpretation is introduced here.

omtv_schema <- function() "rqrgibbs_oracle_mean_tilt_validation/1.0.0"
omtv_config_schema <- function() {
  "rqrgibbs_oracle_mean_tilt_validation_config/1.0.0"
}
omtv_oracle_schema <- function() "rqrgibbs_interval_oracle/2.0.0"
omtv_tilt_definition <- function() {
  "conditional_retained_mean_minus_population_mean"
}

omtv_stop <- function(...) stop(paste0(...), call. = FALSE)
omtv_digest <- function(x) digest::digest(x, algo = "sha256", serialize = TRUE)

omtv_scalar_integer <- function(x, name, lower = 0L) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) ||
      x != floor(x) || x < lower || x > .Machine$integer.max) {
    omtv_stop(name, " must be one finite integer of at least ", lower, ".")
  }
  as.integer(x)
}

omtv_scalar_number <- function(x, name, lower = -Inf, upper = Inf) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) ||
      x < lower || x > upper) {
    omtv_stop(name, " must be one finite number in the declared range.")
  }
  as.numeric(x)
}

omtv_read_config <- function(path) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    omtv_stop("jsonlite is required for the validation workflow.")
  }
  jsonlite::read_json(path, simplifyVector = FALSE)
}

omtv_scenario_frame <- function(config) {
  rows <- lapply(config$scenarios, function(scenario) {
    data.frame(
      scenario_id = as.character(scenario$scenario_id),
      role = as.character(scenario$role),
      tau = as.numeric(scenario$tau),
      coverage_level = as.numeric(scenario$coverage_level),
      n_index = as.integer(scenario$n_index),
      static_design = as.character(scenario$static_design),
      dlm_design = as.character(scenario$dlm_design),
      missing_windows = isTRUE(scenario$missing_windows),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

omtv_validate_config <- function(config) {
  if (!is.list(config) || !identical(config$schema_version, omtv_config_schema())) {
    omtv_stop("Unsupported oracle mean-tilt validation config schema.")
  }
  fixed <- c(
    identical(config$campaign_id, "oracle_mean_tilt_validation_v1"),
    identical(config$scientific_protocol_frozen, TRUE),
    identical(as.character(unlist(config$coverage_targets)),
              c("RQR", "ET", "SH")),
    identical(config$learning_rate_mode, "fixed_rate"),
    identical(config$tilt_source,
              "exact_population_conditional_mean_oracle"),
    identical(config$tilt_definition, omtv_tilt_definition()),
    identical(config$oracle_schema, omtv_oracle_schema()),
    identical(config$methods$fixed_design$prior, "ridge"),
    identical(config$methods$dlm$evolution_mode, "fixed_W"),
    identical(config$methods$dlm$component_scale_evolution, FALSE),
    identical(config$methods$dlm$adaptive_discount, FALSE)
  )
  if (!all(fixed)) omtv_stop("A fixed scientific validation contract changed.")
  if (!identical(as.numeric(config$learning_rate), 1) ||
      !identical(config$methods$fixed_design$fit_engine, "rqr_mcmc_fit") ||
      !identical(config$methods$dlm$fit_engine, "rqr_dlm_fit") ||
      !identical(config$methods$dlm$backend, "cpp") ||
      !identical(config$methods$dlm$future_root_role,
                 "secondary_not_hard_gate")) {
    omtv_stop("The fixed-rate model/engine contract changed.")
  }
  omtv_scalar_number(
    config$methods$fixed_design$ridge_tau2,
    "methods$fixed_design$ridge_tau2", lower = 0
  )
  if (config$methods$fixed_design$ridge_tau2 <= 0) {
    omtv_stop("The ridge prior variance must be positive.")
  }
  for (name in c("execution_authorized", "replication_schedule_frozen")) {
    if (!is.logical(config[[name]]) || length(config[[name]]) != 1L ||
        is.na(config[[name]])) {
      omtv_stop(name, " must be one nonmissing logical value.")
    }
  }
  if (isTRUE(config$execution_authorized) &&
      !isTRUE(config$replication_schedule_frozen)) {
    omtv_stop("Execution authorization requires a frozen replication schedule.")
  }
  mcmc_integer_bounds <- list(
    sentinel_chains = 4L, ordinary_replication_chains = 1L,
    n_burn = 1L, n_mcmc = 2L, thin = 1L, kernel_repetitions = 1L
  )
  for (name in names(mcmc_integer_bounds)) {
    value <- omtv_scalar_integer(
      config$mcmc[[name]], paste0("mcmc$", name),
      mcmc_integer_bounds[[name]]
    )
    if (name == "sentinel_chains" && value != 4L) {
      omtv_stop("The sentinel contract requires exactly four chains.")
    }
    if (name == "ordinary_replication_chains" && value != 1L) {
      omtv_stop("Ordinary replications require exactly one fixed-budget chain.")
    }
  }
  if (!identical(config$mcmc$store_latent_draws, FALSE) ||
      !identical(config$mcmc$store_state_draws, FALSE) ||
      !identical(config$mcmc$numerical_policy, "fail")) {
    omtv_stop("The compact exact-target MCMC contract changed.")
  }
  diagnostic_values <- c(
    rhat_max = config$diagnostics$rhat_max,
    bulk_ess_min = config$diagnostics$bulk_ess_min,
    tail_ess_min = config$diagnostics$tail_ess_min,
    mcse_over_sd_max = config$diagnostics$mcse_over_sd_max
  )
  if (any(!is.finite(diagnostic_values)) || any(diagnostic_values <= 0) ||
      !identical(as.numeric(config$diagnostics$numerical_repairs_max), 0) ||
      !isTRUE(config$diagnostics$exact_joint_target_required)) {
    omtv_stop("The convergence/numerical gate contract is invalid.")
  }
  interpretation_false <- c(
    "response_likelihood", "response_predictive_draws",
    "cornish_fisher_used", "tilt_estimated", "tilt_selected",
    "exal_used", "desn_included", "vb_included", "rhs_ns_included",
    "learned_loss_scale_included"
  )
  if (!isTRUE(config$interpretation$known_dgp_oracle_tilts) ||
      any(!vapply(
        interpretation_false,
        function(name) identical(config$interpretation[[name]], FALSE),
        logical(1L)
      ))) {
    omtv_stop("The validation interpretation contract is incomplete.")
  }
  scenarios <- omtv_scenario_frame(config)
  expected <- c(
    "S01_symmetric_control", "S02_reflected_skew", "S03_primary_skew",
    "S04_higher_content", "S05_article_stress",
    "S06_information_growth"
  )
  if (nrow(scenarios) != 6L || anyDuplicated(scenarios$scenario_id) ||
      !identical(scenarios$scenario_id, expected) ||
      any(!scenarios$tau %in% c(0.20, 0.50, 0.80)) ||
      any(!scenarios$coverage_level %in% c(0.80, 0.90, 0.95)) ||
      any(!scenarios$n_index %in% c(300L, 600L, 1200L))) {
    omtv_stop("The six-scenario scientific grid changed.")
  }
  checkpoints <- as.integer(unlist(
    config$precision_planning$candidate_checkpoints
  ))
  precision_status <- as.character(config$precision_planning$status)
  allowed_precision_status <- if (isTRUE(config$replication_schedule_frozen)) {
    "frozen_after_production_shape_benchmarks"
  } else {
    "awaiting_production_shape_benchmarks"
  }
  if (!identical(checkpoints, c(250L, 500L, 750L, 1000L, 1500L, 2000L)) ||
      !identical(precision_status, allowed_precision_status) ||
      !isTRUE(config$precision_planning$all_primary_metrics_must_pass) ||
      !isTRUE(config$precision_planning$selective_extension_prohibited)) {
    omtv_stop("The prospective precision-planning contract changed.")
  }
  for (name in c(
    "content_error_mcse_max", "endpoint_bias_mcse_over_response_sd_max",
    "width_bias_mcse_over_oracle_width_max",
    "excess_risk_contrast_mcse_fraction"
  )) {
    value <- omtv_scalar_number(
      config$precision_planning[[name]], paste0("precision_planning$", name),
      lower = 0, upper = 1
    )
    if (value <= 0) omtv_stop("Precision thresholds must be positive.")
  }
  margin_unfrozen <- config$precision_planning$
    excess_risk_practical_margin_unfrozen
  if (!is.logical(margin_unfrozen) || length(margin_unfrozen) != 1L ||
      is.na(margin_unfrozen) ||
      identical(margin_unfrozen, isTRUE(config$replication_schedule_frozen))) {
    omtv_stop(
      "The excess-risk practical margin must remain explicitly unfrozen until the replication schedule is frozen."
    )
  }
  if (!isTRUE(margin_unfrozen)) {
    omtv_scalar_number(
      config$precision_planning$excess_risk_practical_margin,
      "precision_planning$excess_risk_practical_margin", lower = 0
    )
    if (config$precision_planning$excess_risk_practical_margin <= 0) {
      omtv_stop("The frozen excess-risk practical margin must be positive.")
    }
  }
  if (!identical(config$rng$kind, "L'Ecuyer-CMRG") ||
      !identical(config$rng$normal_kind, "Inversion") ||
      !identical(config$rng$sample_kind, "Rejection") ||
      !isTRUE(config$rng$common_dgp_across_target_triplet) ||
      !isTRUE(config$rng$seed_selection_prohibited) ||
      !isTRUE(config$rng$replacement_of_failed_replication_prohibited)) {
    omtv_stop("The validation RNG contract changed.")
  }
  omtv_scalar_integer(config$rng$master_seed, "rng$master_seed", 1L)
  workers <- omtv_scalar_integer(
    config$resources$maximum_fit_workers,
    "resources$maximum_fit_workers", 1L
  )
  threads <- omtv_scalar_integer(
    config$resources$threads_per_worker,
    "resources$threads_per_worker", 1L
  )
  omtv_scalar_number(
    config$resources$minimum_free_bytes,
    "resources$minimum_free_bytes", lower = 1
  )
  if (workers > 32L || threads != 1L ||
      !isTRUE(config$resources$require_no_competing_rqrgibbs_heavy_run) ||
      !isTRUE(config$resources$bounded_resumable_waves_required)) {
    omtv_stop("The bounded resource/concurrency contract changed.")
  }
  resource_unfrozen <- c(
    config$resources$maximum_worker_seconds_unfrozen,
    config$resources$maximum_wave_seconds_unfrozen,
    config$resources$maximum_process_tree_rss_kib_unfrozen,
    config$resources$maximum_run_bytes_unfrozen
  )
  if (!is.logical(resource_unfrozen) || length(resource_unfrozen) != 4L ||
      anyNA(resource_unfrozen) ||
      any(resource_unfrozen == isTRUE(config$replication_schedule_frozen))) {
    omtv_stop("Resource ceilings must remain unfrozen until the replication schedule is frozen.")
  }
  if (isTRUE(config$replication_schedule_frozen)) {
    omtv_scalar_number(
      config$resources$maximum_worker_seconds,
      "resources$maximum_worker_seconds", lower = 1
    )
    omtv_scalar_number(
      config$resources$maximum_wave_seconds,
      "resources$maximum_wave_seconds", lower = 1
    )
    omtv_scalar_number(
      config$resources$maximum_process_tree_rss_kib,
      "resources$maximum_process_tree_rss_kib", lower = 1
    )
    omtv_scalar_number(
      config$resources$maximum_run_bytes,
      "resources$maximum_run_bytes", lower = 1
    )
  }
  invisible(config)
}

omtv_oracle_table <- function(config) {
  scenarios <- omtv_scenario_frame(config)
  targets <- as.character(unlist(config$coverage_targets))
  rows <- list()
  index <- 0L
  for (scenario_index in seq_len(nrow(scenarios))) {
    scenario <- scenarios[scenario_index, , drop = FALSE]
    params <- list(
      tau = scenario$tau, scale = 1, variance_standardized = TRUE
    )
    for (target in targets) {
      index <- index + 1L
      oracle <- rqrgibbs::rqr_interval_oracle(
        "asymmetric_laplace", scenario$coverage_level, target,
        params = params, tol = 1e-10, grid_size = 1601L
      )
      rows[[index]] <- data.frame(
        scenario_id = scenario$scenario_id,
        target = target,
        tau = scenario$tau,
        coverage_level = scenario$coverage_level,
        lower_probability = oracle$lower_probability,
        upper_probability = oracle$upper_probability,
        lower_root = oracle$lower_root,
        upper_root = oracle$upper_root,
        width = oracle$width,
        truncated_first_moment = oracle$truncated_first_moment,
        conditional_retained_mean = oracle$conditional_retained_mean,
        population_mean = oracle$population_mean,
        mean_tilt = oracle$mean_tilt,
        content_residual = oracle$content_residual,
        retained_mean_residual = oracle$retained_mean_residual,
        unique_minimizer = oracle$unique_minimizer,
        oracle_schema = oracle$schema_version,
        tilt_definition = oracle$tilt_definition,
        oracle_digest = oracle$certificate_digest,
        uses_cornish_fisher = oracle$uses_cornish_fisher,
        stringsAsFactors = FALSE
      )
    }
  }
  out <- do.call(rbind, rows)
  key <- paste(out$scenario_id, out$target, sep = "/")
  if (nrow(out) != 18L || anyDuplicated(key) ||
      any(out$oracle_schema != omtv_oracle_schema()) ||
      any(out$tilt_definition != omtv_tilt_definition()) ||
      any(out$uses_cornish_fisher) ||
      any(!out$unique_minimizer) ||
      any(abs(out$content_residual) > 1e-10) ||
      any(abs(out$retained_mean_residual) > 1e-10)) {
    omtv_stop("The 18-row oracle target table failed.")
  }
  out
}

omtv_incidence_matrix <- function(config, oracle) {
  scenarios <- omtv_scenario_frame(config)
  grid <- merge(
    expand.grid(
      model_family = c("fixed_design", "dlm"),
      target = as.character(unlist(config$coverage_targets)),
      stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE
    ),
    scenarios,
    by = NULL
  )
  grid <- grid[order(
    match(grid$scenario_id, scenarios$scenario_id),
    match(grid$model_family, c("fixed_design", "dlm")),
    match(grid$target, c("RQR", "ET", "SH"))
  ), , drop = FALSE]
  grid$included <- TRUE
  grid$omission_reason <- ""
  grid$method_id <- paste(grid$model_family, grid$target, sep = "/")
  oracle_key <- paste(oracle$scenario_id, oracle$target, sep = "/")
  row_key <- paste(grid$scenario_id, grid$target, sep = "/")
  match_index <- match(row_key, oracle_key)
  grid$oracle_digest <- oracle$oracle_digest[match_index]
  grid$replication_count <- NA_integer_
  grid$replication_status <- "awaiting_precision_and_resource_freeze"
  grid$fit_count <- NA_integer_
  grid$incidence_id <- sprintf("I%03d", seq_len(nrow(grid)))
  rownames(grid) <- NULL
  if (nrow(grid) != 36L || anyDuplicated(grid$incidence_id) ||
      anyNA(match_index) || anyNA(grid$oracle_digest)) {
    omtv_stop("The explicit 36-row incidence matrix failed.")
  }
  grid
}

omtv_static_basis <- function(x, config, design) {
  x <- as.numeric(x)
  contract <- config$static_contract
  if (identical(design, "linear_homoscedastic")) {
    return(cbind(intercept = 1, x = x))
  }
  if (!identical(design, "spline_heteroscedastic")) {
    omtv_stop("Unknown static design: ", design)
  }
  splines::bs(
    x, degree = as.integer(contract$spline_degree),
    knots = as.numeric(unlist(contract$internal_knots)),
    Boundary.knots = as.numeric(unlist(contract$boundary_knots)),
    intercept = isTRUE(contract$spline_intercept)
  )
}

omtv_static_blueprint <- function(config, scenario, oracle) {
  x <- seq(-1, 1, length.out = scenario$n_index)
  evaluation_x <- seq(
    config$static_contract$design_domain[[1L]],
    config$static_contract$design_domain[[2L]],
    length.out = as.integer(config$static_contract$evaluation_grid_size)
  )
  X <- omtv_static_basis(x, config, scenario$static_design)
  X_eval <- omtv_static_basis(evaluation_x, config, scenario$static_design)
  if (identical(scenario$static_design, "linear_homoscedastic")) {
    mean_coefficient <- as.numeric(unlist(config$static_contract$linear_mean))
    scale_coefficient <- c(config$static_contract$constant_scale, 0)
  } else {
    mean_coefficient <- as.numeric(unlist(
      config$static_contract$mean_coefficients
    ))
    scale_coefficient <- as.numeric(unlist(
      config$static_contract$scale_coefficients
    ))
  }
  mu <- drop(X %*% mean_coefficient)
  scale <- drop(X %*% scale_coefficient)
  mu_eval <- drop(X_eval %*% mean_coefficient)
  scale_eval <- drop(X_eval %*% scale_coefficient)
  if (any(!is.finite(scale)) || any(!is.finite(scale_eval)) ||
      min(c(scale, scale_eval)) < config$static_contract$minimum_scale) {
    omtv_stop("The static population scale violates its floor.")
  }
  targets <- lapply(seq_len(nrow(oracle)), function(ii) {
    row <- oracle[ii, , drop = FALSE]
    data.frame(
      target = row$target,
      index = seq_along(x), x = x,
      mean_truth = mu, scale_truth = scale,
      oracle_lower = mu + scale * row$lower_root,
      oracle_upper = mu + scale * row$upper_root,
      oracle_width = scale * row$width,
      mean_tilt = scale * row$mean_tilt,
      oracle_digest = row$oracle_digest,
      stringsAsFactors = FALSE
    )
  })
  targets <- do.call(rbind, targets)
  list(
    family = "fixed_design", scenario_id = scenario$scenario_id,
    x = x, X = X, evaluation_x = evaluation_x, X_eval = X_eval,
    mean_truth = mu, scale_truth = scale,
    mean_evaluation = mu_eval, scale_evaluation = scale_eval,
    mean_coefficient = mean_coefficient,
    scale_coefficient = scale_coefficient,
    targets = targets,
    design_digest = omtv_digest(list(x = x, X = X, X_eval = X_eval)),
    population_digest = omtv_digest(list(mu = mu, scale = scale))
  )
}

omtv_block_diag <- function(A, B) {
  A <- as.matrix(A); B <- as.matrix(B)
  out <- matrix(0, nrow(A) + nrow(B), ncol(A) + ncol(B))
  out[seq_len(nrow(A)), seq_len(ncol(A))] <- A
  out[nrow(A) + seq_len(nrow(B)), ncol(A) + seq_len(ncol(B))] <- B
  out
}

omtv_dlm_blueprint <- function(config, scenario, oracle) {
  T <- scenario$n_index
  time <- seq_len(T) / T
  dt <- 1 / T
  contract <- config$dlm_contract
  if (identical(scenario$dlm_design, "local_level_constant_scale")) {
    GG <- array(1, c(1L, 1L, T))
    W <- array(contract$local_level_evolution_variance_per_unit_time * dt,
               c(1L, 1L, T))
    model <- rqrgibbs::rqr_as_dlm_model(list(
      FF = matrix(1, 1L, 1L), GG = GG,
      m0 = contract$local_level_initial_mean,
      C0 = matrix(contract$local_level_initial_variance, 1L, 1L),
      component_dims = 1L, component_names = "local_level"
    ))
    location_state <- matrix(
      contract$local_level_initial_mean, 1L, T
    )
    mean_truth <- as.numeric(location_state)
    scale_truth <- rep(contract$constant_scale, T)
    root_state <- function(quantile) location_state +
      quantile * contract$constant_scale
    local_state_dimension <- 1L
    deterministic_seasonal_state <- NULL
    scale_state <- matrix(contract$constant_scale, 1L, T)
  } else if (identical(
    scenario$dlm_design, "local_linear_seasonal_varying_scale"
  )) {
    G_local <- matrix(c(1, 0, dt, 1), 2L, 2L)
    q_level <- contract$q_level_per_unit_time
    q_slope <- contract$q_slope_per_unit_time
    W_local <- matrix(c(
      q_level * dt + q_slope * dt^3 / 3, q_slope * dt^2 / 2,
      q_slope * dt^2 / 2, q_slope * dt
    ), 2L, 2L)
    omega <- 2 * pi * contract$seasonal_cycles / T
    G_seasonal <- matrix(c(cos(omega), -sin(omega),
                           sin(omega), cos(omega)), 2L, 2L)
    G_one <- omtv_block_diag(G_local, G_seasonal)
    W_one <- omtv_block_diag(
      W_local, diag(contract$seasonal_evolution_variance, 2L)
    )
    GG <- array(rep(G_one, T), c(4L, 4L, T))
    W <- array(rep(W_one, T), c(4L, 4L, T))
    C0 <- diag(c(
      as.numeric(unlist(contract$local_linear_initial_covariance)),
      rep(contract$seasonal_initial_variance, 2L)
    ))
    model <- rqrgibbs::rqr_as_dlm_model(list(
      FF = matrix(c(1, 0, 1, 0), 4L, 1L), GG = GG,
      m0 = c(as.numeric(unlist(contract$local_linear_initial_state)), 0, 0),
      C0 = C0, component_dims = c(2L, 2L),
      component_names = c("local_linear", "seasonal")
    ))
    initial <- as.numeric(unlist(contract$local_linear_initial_state))
    local <- rbind(initial[[1L]] + initial[[2L]] * time,
                   rep(initial[[2L]], T))
    seasonal <- rbind(
      contract$seasonal_amplitude *
        sin(omega * seq_len(T) + contract$seasonal_phase),
      contract$seasonal_amplitude *
        cos(omega * seq_len(T) + contract$seasonal_phase)
    )
    scale_seasonal <- rbind(
      contract$scale_amplitude *
        cos(omega * seq_len(T) + contract$scale_phase),
      -contract$scale_amplitude *
        sin(omega * seq_len(T) + contract$scale_phase)
    )
    mean_truth <- local[1L, ] + seasonal[1L, ]
    scale_truth <- contract$scale_baseline + scale_seasonal[1L, ]
    root_state <- function(quantile) {
      trend <- local
      trend[1L, ] <- trend[1L, ] + quantile * contract$scale_baseline
      rbind(trend, seasonal + quantile * scale_seasonal)
    }
    location_state <- rbind(local, seasonal)
    local_state_dimension <- 2L
    deterministic_seasonal_state <- seasonal
    scale_state <- rbind(
      rbind(rep(contract$scale_baseline, T), rep(0, T)),
      scale_seasonal
    )
  } else {
    omtv_stop("Unknown DLM design: ", scenario$dlm_design)
  }
  if (min(scale_truth) <= 0) omtv_stop("The DLM scale is not positive.")
  missing <- rep(FALSE, T)
  if (isTRUE(scenario$missing_windows)) {
    windows <- matrix(
      as.numeric(unlist(contract$missing_windows)), ncol = 2L, byrow = TRUE
    )
    for (ii in seq_len(nrow(windows))) {
      missing <- missing | (time >= windows[ii, 1L] & time <= windows[ii, 2L])
    }
  }
  targets <- lapply(seq_len(nrow(oracle)), function(ii) {
    row <- oracle[ii, , drop = FALSE]
    lower_state <- root_state(row$lower_root)
    upper_state <- root_state(row$upper_root)
    FF <- if (nrow(lower_state) == 1L) matrix(1, 1L, T) else
      matrix(rep(c(1, 0, 1, 0), T), 4L, T)
    lower <- colSums(FF * lower_state)
    upper <- colSums(FF * upper_state)
    data.frame(
      target = row$target, index = seq_len(T), time = time,
      observed = !missing,
      mean_truth = mean_truth, scale_truth = scale_truth,
      oracle_lower = lower, oracle_upper = upper,
      oracle_width = upper - lower,
      mean_tilt = ifelse(!missing, scale_truth * row$mean_tilt, NA_real_),
      oracle_digest = row$oracle_digest,
      stringsAsFactors = FALSE
    )
  })
  list(
    family = "dlm", scenario_id = scenario$scenario_id,
    T = T, time = time, model = model, GG = GG, W = W,
    mean_truth = mean_truth, scale_truth = scale_truth,
    location_state_truth = location_state,
    scale_state_truth = scale_state,
    local_state_dimension = local_state_dimension,
    deterministic_seasonal_state = deterministic_seasonal_state,
    observed = !missing, missing_times = which(missing),
    root_state = root_state, targets = do.call(rbind, targets),
    design_digest = omtv_digest(list(time = time, GG = GG, W = W)),
    population_digest = omtv_digest(list(
      mean = mean_truth, scale = scale_truth, missing = missing
    ))
  )
}

omtv_projection_audit <- function(blueprint) {
  targets <- split(blueprint$targets, blueprint$targets$target)
  rows <- list()
  index <- 0L
  if (identical(blueprint$family, "fixed_design")) {
    for (target in names(targets)) for (endpoint in c("lower", "upper")) {
      index <- index + 1L
      truth <- targets[[target]][[paste0("oracle_", endpoint)]]
      coefficient <- qr.solve(blueprint$X, truth)
      residual <- truth - drop(blueprint$X %*% coefficient)
      rows[[index]] <- data.frame(
        family = blueprint$family, scenario_id = blueprint$scenario_id,
        target = target, endpoint = endpoint,
        rank = qr(blueprint$X)$rank,
        dimension = ncol(blueprint$X),
        maximum_absolute_residual = max(abs(residual)),
        root_mean_square_residual = sqrt(mean(residual^2)),
        stringsAsFactors = FALSE
      )
    }
  } else {
    expanded <- rqrgibbs:::.rqr_expand_model(
      rqrgibbs::rqr_as_dlm_model(blueprint$model), blueprint$T
    )
    for (target in names(targets)) for (endpoint in c("lower", "upper")) {
      index <- index + 1L
      truth <- targets[[target]][[paste0("oracle_", endpoint)]]
      q <- (truth - blueprint$mean_truth) / blueprint$scale_truth
      state <- blueprint$root_state(mean(q))
      reconstructed <- colSums(expanded$FF * state)
      residual <- truth - reconstructed
      rows[[index]] <- data.frame(
        family = blueprint$family, scenario_id = blueprint$scenario_id,
        target = target, endpoint = endpoint,
        rank = qr(t(expanded$FF))$rank,
        dimension = expanded$p,
        maximum_absolute_residual = max(abs(residual)),
        root_mean_square_residual = sqrt(mean(residual^2)),
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

omtv_tail_information <- function(config, oracle) {
  scenarios <- omtv_scenario_frame(config)
  index <- match(oracle$scenario_id, scenarios$scenario_id)
  lower <- oracle$lower_probability
  upper <- 1 - oracle$upper_probability
  rare <- pmin(lower, upper)
  n <- scenarios$n_index[index]
  strata <- ifelse(
    scenarios$static_design[index] == "linear_homoscedastic", 1L, 5L
  )
  data.frame(
    scenario_id = oracle$scenario_id, target = oracle$target,
    lower_tail_probability = lower,
    upper_tail_probability = upper,
    rare_tail_probability = rare,
    n_index = n,
    expected_rare_tail_count = n * rare,
    declared_scale_strata = strata,
    expected_rare_tail_count_per_stratum = n * rare / strata,
    endpoint_density_lower = mapply(function(tau, root) {
      spec <- rqrgibbs:::.rqr_oracle_family_spec(
        "asymmetric_laplace",
        list(tau = tau, scale = 1, variance_standardized = TRUE)
      )
      spec$d(root)
    }, oracle$tau, oracle$lower_root),
    endpoint_density_upper = mapply(function(tau, root) {
      spec <- rqrgibbs:::.rqr_oracle_family_spec(
        "asymmetric_laplace",
        list(tau = tau, scale = 1, variance_standardized = TRUE)
      )
      spec$d(root)
    }, oracle$tau, oracle$upper_root),
    stringsAsFactors = FALSE
  )
}

omtv_preflight <- function(config) {
  omtv_validate_config(config)
  oracle <- omtv_oracle_table(config)
  incidence <- omtv_incidence_matrix(config, oracle)
  scenarios <- omtv_scenario_frame(config)
  blueprints <- list()
  projections <- list()
  for (ii in seq_len(nrow(scenarios))) {
    scenario <- scenarios[ii, , drop = FALSE]
    scenario_oracle <- oracle[oracle$scenario_id == scenario$scenario_id,
                              , drop = FALSE]
    for (family in c("fixed_design", "dlm")) {
      blueprint <- if (identical(family, "fixed_design")) {
        omtv_static_blueprint(config, scenario, scenario_oracle)
      } else {
        omtv_dlm_blueprint(config, scenario, scenario_oracle)
      }
      key <- paste(scenario$scenario_id, family, sep = "/")
      blueprints[[key]] <- blueprint
      projections[[key]] <- omtv_projection_audit(blueprint)
    }
  }
  projection <- do.call(rbind, projections)
  tail <- omtv_tail_information(config, oracle)
  gates <- data.frame(
    gate = c(
      "oracle_rows", "incidence_rows", "projection_max_absolute_residual",
      "static_minimum_scale", "dlm_minimum_scale",
      "minimum_expected_rare_tail_count",
      "minimum_expected_rare_tail_count_per_stratum",
      "replication_schedule_state_consistent",
      "execution_authorization_consistent"
    ),
    value = c(
      nrow(oracle), nrow(incidence), max(projection$maximum_absolute_residual),
      min(vapply(blueprints[grepl("/fixed_design$", names(blueprints))],
                 function(x) min(x$scale_truth), numeric(1L))),
      min(vapply(blueprints[grepl("/dlm$", names(blueprints))],
                 function(x) min(x$scale_truth), numeric(1L))),
      min(tail$expected_rare_tail_count),
      min(tail$expected_rare_tail_count_per_stratum),
      as.numeric(
        (!isTRUE(config$replication_schedule_frozen) &&
           identical(config$precision_planning$status,
                     "awaiting_production_shape_benchmarks")) ||
          (isTRUE(config$replication_schedule_frozen) &&
             identical(config$precision_planning$status,
                       "frozen_after_production_shape_benchmarks"))
      ),
      as.numeric(
        !isTRUE(config$execution_authorized) ||
          isTRUE(config$replication_schedule_frozen)
      )
    ),
    threshold = c(18, 36, config$static_contract$projection_tolerance,
                  config$static_contract$minimum_scale,
                  0.30, 10, 2, 1, 1),
    comparison = c("==", "==", "<=", ">=", ">=", ">=", ">=", "==", "=="),
    stringsAsFactors = FALSE
  )
  gates$pass <- mapply(function(value, threshold, comparison) {
    switch(comparison,
      "==" = abs(value - threshold) <= 1e-12,
      "<=" = value <= threshold,
      ">=" = value >= threshold,
      FALSE
    )
  }, gates$value, gates$threshold, gates$comparison)
  list(
    schema_version = omtv_schema(), oracle = oracle,
    incidence = incidence, scenarios = scenarios, tail_information = tail,
    projection_audit = projection, gates = gates,
    blueprints = blueprints, pass = all(gates$pass),
    protocol_digest = omtv_digest(list(
      scenarios = scenarios, incidence = incidence[, setdiff(
        names(incidence), c("replication_count", "fit_count")
      )], oracle = oracle
    ))
  )
}

omtv_assign_streams <- function(config, n_streams) {
  n_streams <- omtv_scalar_integer(n_streams, "n_streams", 1L)
  old_kind <- RNGkind()
  old_seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (old_seed_exists) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit({
    do.call(RNGkind, as.list(old_kind))
    if (old_seed_exists) assign(".Random.seed", old_seed, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
      rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)
  RNGkind(config$rng$kind, config$rng$normal_kind, config$rng$sample_kind)
  set.seed(as.integer(config$rng$master_seed))
  streams <- vector("list", n_streams)
  streams[[1L]] <- .Random.seed
  if (n_streams > 1L) for (ii in 2:n_streams) {
    streams[[ii]] <- parallel::nextRNGStream(streams[[ii - 1L]])
  }
  data.frame(
    stream_id = sprintf("stream_%08d", seq_len(n_streams)),
    seed_serialized = vapply(
      streams, function(seed) paste(seed, collapse = ";"), character(1L)
    ),
    seed_digest = vapply(streams, omtv_digest, character(1L)),
    stringsAsFactors = FALSE
  )
}

omtv_rng_ledger <- function(config, replications) {
  replications <- omtv_scalar_integer(replications, "replications", 1L)
  scenarios <- omtv_scenario_frame(config)
  targets <- as.character(unlist(config$coverage_targets))
  rows <- list()
  index <- 0L
  for (scenario_id in scenarios$scenario_id) {
    for (family in c("fixed_design", "dlm")) {
      for (replication in seq_len(replications)) {
        index <- index + 1L
        rows[[index]] <- data.frame(
          scenario_id = scenario_id, model_family = family,
          target = "DGP", replication = replication,
          stream_role = "shared_target_triplet_dgp",
          stringsAsFactors = FALSE
        )
        for (target in targets) {
          index <- index + 1L
          rows[[index]] <- data.frame(
            scenario_id = scenario_id, model_family = family,
            target = target, replication = replication,
            stream_role = "target_specific_mcmc",
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  streams <- omtv_assign_streams(config, nrow(do.call(rbind, rows)))
  ledger <- cbind(do.call(rbind, rows), streams)
  ledger$stream_contract_digest <- omtv_digest(list(
    schema_version = omtv_schema(), rng = config$rng,
    keys = ledger[, c(
      "scenario_id", "model_family", "target", "replication",
      "stream_role", "stream_id"
    )]
  ))
  rownames(ledger) <- NULL
  ledger
}

omtv_with_stream <- function(seed_serialized, expression) {
  seed <- as.integer(strsplit(seed_serialized, ";", fixed = TRUE)[[1L]])
  old_seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (old_seed_exists) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit({
    if (old_seed_exists) assign(".Random.seed", old_seed, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
      rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)
  assign(".Random.seed", seed, envir = .GlobalEnv)
  force(expression)
}

omtv_generate_replication <- function(
    config, blueprint, tau, dgp_stream, family = blueprint$family) {
  law <- rqrgibbs:::.rqr_oracle_family_spec(
    "asymmetric_laplace",
    list(tau = tau, scale = 1, variance_standardized = TRUE)
  )
  draw_innovation <- function(n) law$q(stats::runif(n))
  omtv_with_stream(dgp_stream, {
    if (identical(family, "fixed_design")) {
      y <- blueprint$mean_truth + blueprint$scale_truth *
        draw_innovation(length(blueprint$mean_truth))
      return(list(y = y, y_full = y, latent_location = NULL))
    }
    T <- blueprint$T
    expanded <- rqrgibbs:::.rqr_expand_model(
      rqrgibbs::rqr_as_dlm_model(blueprint$model), T
    )
    local_dimension <- blueprint$local_state_dimension
    state <- expanded$m0[seq_len(local_dimension)]
    path <- blueprint$location_state_truth
    for (tt in seq_len(T)) {
      Gt <- expanded$GG[
        seq_len(local_dimension), seq_len(local_dimension), tt,
        drop = FALSE
      ][, , 1L]
      Wt <- blueprint$W[
        seq_len(local_dimension), seq_len(local_dimension), tt,
        drop = FALSE
      ][, , 1L]
      Wt <- 0.5 * Wt + 0.5 * t(Wt)
      eigen <- eigen(Wt, symmetric = TRUE)
      innovation <- eigen$vectors %*%
        (sqrt(pmax(eigen$values, 0)) * stats::rnorm(local_dimension))
      state <- drop(Gt %*% state + innovation)
      path[seq_len(local_dimension), tt] <- state
    }
    location <- colSums(expanded$FF * path)
    y_full <- location + blueprint$scale_truth * draw_innovation(T)
    y <- y_full
    y[!blueprint$observed] <- NA_real_
    list(y = y, y_full = y_full, latent_location = location,
         latent_state = path)
  })
}

omtv_seed_vector <- function(seed_serialized) {
  seed <- suppressWarnings(as.integer(
    strsplit(as.character(seed_serialized), ";", fixed = TRUE)[[1L]]
  ))
  if (length(seed) != 7L || anyNA(seed)) {
    omtv_stop("A serialized L'Ecuyer-CMRG stream is malformed.")
  }
  seed
}

omtv_replication_truth <- function(blueprint, generated, scenario_oracle) {
  required <- c(
    "lower_root", "upper_root", "width", "mean_tilt",
    "coverage_level", "target", "oracle_digest"
  )
  if (!is.data.frame(scenario_oracle) || nrow(scenario_oracle) != 1L ||
      !all(required %in% names(scenario_oracle))) {
    omtv_stop("Exactly one complete target oracle is required.")
  }
  if (identical(blueprint$family, "fixed_design")) {
    location <- blueprint$mean_evaluation
    scale <- blueprint$scale_evaluation
    index <- seq_along(blueprint$evaluation_x)
    coordinate <- blueprint$evaluation_x
    observed <- rep(TRUE, length(index))
  } else {
    location <- generated$latent_location
    scale <- blueprint$scale_truth
    index <- seq_len(blueprint$T)
    coordinate <- blueprint$time
    observed <- blueprint$observed
  }
  lower <- location + scale * scenario_oracle$lower_root
  upper <- location + scale * scenario_oracle$upper_root
  data.frame(
    index = index, coordinate = coordinate, observed = observed,
    mean_truth = location, scale_truth = scale,
    oracle_lower = lower, oracle_upper = upper,
    oracle_midpoint = 0.5 * (lower + upper),
    oracle_width = upper - lower,
    mean_tilt = scale * scenario_oracle$mean_tilt,
    target = scenario_oracle$target,
    coverage_level = scenario_oracle$coverage_level,
    oracle_digest = scenario_oracle$oracle_digest,
    stringsAsFactors = FALSE
  )
}

omtv_fit_draw_diagnostics <- function(
    lower, upper, loss_draws = NULL, retain_draws = FALSE) {
  lower <- as.matrix(lower)
  upper <- as.matrix(upper)
  if (!all(dim(lower) == dim(upper)) || any(!is.finite(lower)) ||
      any(!is.finite(upper))) {
    omtv_stop("Endpoint draws are nonfinite or dimensionally incompatible.")
  }
  locations <- sort(unique(as.integer(round(seq(
    1, nrow(lower), length.out = min(9L, nrow(lower))
  )))))
  draws <- cbind(
    mean_lower = colMeans(lower),
    mean_upper = colMeans(upper),
    mean_midpoint = colMeans(0.5 * (lower + upper)),
    mean_width = colMeans(upper - lower)
  )
  for (index in locations) {
    suffix <- sprintf("%04d", index)
    draws <- cbind(
      draws,
      setNames(data.frame(
        lower[index, ], upper[index, ],
        0.5 * (lower[index, ] + upper[index, ]),
        upper[index, ] - lower[index, ]
      ), paste0(c("lower_", "upper_", "midpoint_", "width_"), suffix))
    )
  }
  if (!is.null(loss_draws)) {
    loss_draws <- as.numeric(loss_draws)
    if (length(loss_draws) == nrow(draws) && all(is.finite(loss_draws))) {
      draws <- cbind(draws, target_loss = loss_draws)
    }
  }
  draws <- as.matrix(draws)
  if (requireNamespace("posterior", quietly = TRUE)) {
    posterior_draws <- posterior::as_draws_matrix(draws)
    bulk <- posterior::ess_bulk(posterior_draws)
    tail <- posterior::ess_tail(posterior_draws)
    mcse <- posterior::mcse_mean(posterior_draws)
  } else {
    bulk <- tail <- rep(NA_real_, ncol(draws))
    mcse <- rep(NA_real_, ncol(draws))
  }
  sd_draw <- apply(draws, 2L, stats::sd)
  out <- data.frame(
    variable = colnames(draws),
    ess_bulk = as.numeric(bulk), ess_tail = as.numeric(tail),
    mcse_mean = as.numeric(mcse), posterior_sd = sd_draw,
    mcse_over_sd = ifelse(sd_draw > 0, as.numeric(mcse) / sd_draw, 0),
    stringsAsFactors = FALSE
  )
  if (isTRUE(retain_draws)) attr(out, "draw_matrix") <- draws
  out
}

omtv_fit_replication <- function(
    config, blueprint, generated, scenario_oracle, mcmc_stream,
    provenance_control = list(), mcmc_override = list(),
    retain_diagnostic_draws = FALSE) {
  truth <- omtv_replication_truth(blueprint, generated, scenario_oracle)
  control <- modifyList(list(
    n_burn = as.integer(config$mcmc$n_burn),
    n_mcmc = as.integer(config$mcmc$n_mcmc),
    thin = as.integer(config$mcmc$thin),
    kernel_repetitions = as.integer(config$mcmc$kernel_repetitions),
    store_latent_draws = isTRUE(config$mcmc$store_latent_draws),
    verbose = FALSE, progress_every = 500L
  ), mcmc_override)
  rng_state <- omtv_seed_vector(mcmc_stream)
  started <- proc.time()[["elapsed"]]
  if (identical(blueprint$family, "fixed_design")) {
    fit <- rqrgibbs::rqr_mcmc_fit(
      y = generated$y, X = blueprint$X,
      coverage_level = scenario_oracle$coverage_level,
      learning_rate = config$learning_rate,
      learning_rate_mode = "fixed_rate",
      mean_tilt = blueprint$scale_truth * scenario_oracle$mean_tilt,
      beta_prior_obj = rqrgibbs::beta_prior(
        "ridge", ridge = list(tau2 = config$methods$fixed_design$ridge_tau2)
      ),
      numerical_policy = config$mcmc$numerical_policy,
      provenance_control = provenance_control,
      mcmc_control = control,
      init = list(rng_state = rng_state)
    )
    prediction <- rqrgibbs::predict_interval(fit, blueprint$X_eval)
  } else {
    control$backend <- config$methods$dlm$backend
    control$store_state_draws <- isTRUE(config$mcmc$store_state_draws)
    fit <- rqrgibbs::rqr_dlm_fit(
      y = generated$y, model = blueprint$model,
      coverage_level = scenario_oracle$coverage_level,
      evolution_mode = "fixed_W", W = blueprint$W,
      learning_rate = config$learning_rate,
      learning_rate_mode = "fixed_rate",
      mean_tilt = blueprint$scale_truth * scenario_oracle$mean_tilt,
      numerical_policy = config$mcmc$numerical_policy,
      provenance_control = provenance_control,
      mcmc_control = control,
      init = list(rng_state = rng_state)
    )
    prediction <- rqrgibbs::predict_interval(fit)
  }
  elapsed <- proc.time()[["elapsed"]] - started
  lower <- as.matrix(prediction$lower_draws)
  upper <- as.matrix(prediction$upper_draws)
  point_lower <- rowMeans(lower)
  point_upper <- rowMeans(upper)
  params <- list(
    tau = scenario_oracle$tau, scale = 1, variance_standardized = TRUE
  )
  estimands <- omtv_exact_estimands(
    point_lower, point_upper, truth, scenario_oracle, params
  )
  credible_lower <- t(apply(lower, 1L, stats::quantile,
                            probs = c(0.025, 0.975), names = FALSE, type = 8))
  credible_upper <- t(apply(upper, 1L, stats::quantile,
                            probs = c(0.025, 0.975), names = FALSE, type = 8))
  estimands$lower_credible_inclusion <- mean(
    truth$oracle_lower >= credible_lower[, 1L] &
      truth$oracle_lower <= credible_lower[, 2L]
  )
  estimands$upper_credible_inclusion <- mean(
    truth$oracle_upper >= credible_upper[, 1L] &
      truth$oracle_upper <= credible_upper[, 2L]
  )
  estimands$response_sd <- stats::sd(generated$y_full)
  estimands$oracle_mean_width <- mean(truth$oracle_width)
  diagnostics <- omtv_fit_draw_diagnostics(
    lower, upper, fit$diagnostics$mean_tilted_target_loss_trace[
      seq.int(
        length(fit$diagnostics$mean_tilted_target_loss_trace) -
          ncol(lower) + 1L,
        length(fit$diagnostics$mean_tilted_target_loss_trace)
      )
    ], retain_draws = retain_diagnostic_draws
  )
  ordinary <- fit$diagnostics$ordinary_product_check_loss_trace
  linear <- fit$diagnostics$tilt_linear_trace
  total <- fit$diagnostics$mean_tilted_target_loss_trace
  identity_error <- max(abs(total - (ordinary - linear)))
  model_spec <- fit$model_spec
  repairs <- as.integer(if (is.null(model_spec$numerical_repair_count)) {
    0L
  } else {
    model_spec$numerical_repair_count
  })
  exact_target <- isTRUE(model_spec$exact_joint_target)
  numerical_eligible <- isTRUE(model_spec$target_numerical_eligible)
  diagnostic_pass <- all(is.finite(diagnostics$ess_bulk)) &&
    all(is.finite(diagnostics$ess_tail)) &&
    all(is.finite(diagnostics$mcse_over_sd)) &&
    min(diagnostics$ess_bulk) >= config$diagnostics$bulk_ess_min &&
    min(diagnostics$ess_tail) >= config$diagnostics$tail_ess_min &&
    max(diagnostics$mcse_over_sd) <= config$diagnostics$mcse_over_sd_max
  list(
    schema_version = "rqrgibbs_oracle_mean_tilt_fit/1.0.0",
    family = blueprint$family, scenario_id = blueprint$scenario_id,
    target = scenario_oracle$target,
    oracle_digest = scenario_oracle$oracle_digest,
    mean_tilt_digest = model_spec$mean_tilt_digest,
    elapsed_seconds = elapsed,
    numerical_repair_count = repairs,
    exact_joint_target = exact_target,
    target_numerical_eligible = numerical_eligible,
    reproducibility_eligible = isTRUE(model_spec$reproducibility_eligible),
    loss_identity_maximum_absolute_error = identity_error,
    diagnostic_pass = diagnostic_pass,
    estimands = estimands, diagnostics = diagnostics,
    endpoint_summary = data.frame(
      index = truth$index, coordinate = truth$coordinate,
      observed = truth$observed,
      lower_mean = point_lower, upper_mean = point_upper,
      lower_q025 = credible_lower[, 1L], lower_q975 = credible_lower[, 2L],
      upper_q025 = credible_upper[, 1L], upper_q975 = credible_upper[, 2L],
      oracle_lower = truth$oracle_lower, oracle_upper = truth$oracle_upper,
      stringsAsFactors = FALSE
    ),
    pass = repairs == 0L && exact_target && numerical_eligible &&
      diagnostic_pass &&
      is.finite(identity_error) && identity_error <= 1e-8 &&
      all(is.finite(point_lower)) && all(is.finite(point_upper)) &&
      all(point_lower <= point_upper)
  )
}

omtv_exact_estimands <- function(
    lower, upper, truth, scenario_oracle, family_params) {
  content <- rqrgibbs::rqr_oracle_conditional_content(
    lower, upper, truth$mean_truth, truth$scale_truth,
    family = "asymmetric_laplace", params = family_params
  )
  risk <- rqrgibbs::rqr_oracle_tilted_risk(
    lower, upper, scenario_oracle$coverage_level,
    mean_tilt = truth$scale_truth * scenario_oracle$mean_tilt,
    location = truth$mean_truth, scale = truth$scale_truth,
    family = "asymmetric_laplace", params = family_params
  )
  oracle_risk <- rqrgibbs::rqr_oracle_tilted_risk(
    truth$oracle_lower, truth$oracle_upper,
    scenario_oracle$coverage_level,
    mean_tilt = truth$scale_truth * scenario_oracle$mean_tilt,
    location = truth$mean_truth, scale = truth$scale_truth,
    family = "asymmetric_laplace", params = family_params
  )
  data.frame(
    lower_bias = mean(lower - truth$oracle_lower),
    upper_bias = mean(upper - truth$oracle_upper),
    lower_mae = mean(abs(lower - truth$oracle_lower)),
    upper_mae = mean(abs(upper - truth$oracle_upper)),
    endpoint_rmse = sqrt(mean(c(
      (lower - truth$oracle_lower)^2,
      (upper - truth$oracle_upper)^2
    ))),
    midpoint_bias = mean(0.5 * (lower + upper) -
                           0.5 * (truth$oracle_lower + truth$oracle_upper)),
    width_bias = mean((upper - lower) - truth$oracle_width),
    width_ratio = mean(upper - lower) / mean(truth$oracle_width),
    conditional_content = mean(content),
    conditional_content_error = mean(content) -
      scenario_oracle$coverage_level,
    mean_excess_target_risk = mean(
      risk$mean_tilted_risk - oracle_risk$mean_tilted_risk
    ),
    stringsAsFactors = FALSE
  )
}

omtv_precision_decision <- function(replication_rows, config, checkpoint) {
  checkpoint <- omtv_scalar_integer(checkpoint, "checkpoint", 1L)
  declared <- as.integer(unlist(
    config$precision_planning$candidate_checkpoints
  ))
  if (!checkpoint %in% declared) {
    omtv_stop("Precision may be checked only at a declared checkpoint.")
  }
  required <- c(
    "scenario_id", "model_family", "target", "replication",
    "conditional_content_error", "lower_bias", "upper_bias", "width_bias",
    "mean_excess_target_risk", "response_sd", "oracle_mean_width"
  )
  if (!is.data.frame(replication_rows) ||
      !all(required %in% names(replication_rows))) {
    omtv_stop("Replication estimands are incomplete for precision checking.")
  }
  key <- interaction(
    replication_rows$scenario_id, replication_rows$model_family,
    replication_rows$target, drop = TRUE
  )
  groups <- split(replication_rows, key)
  rows <- lapply(groups, function(data) {
    if (nrow(data) != checkpoint || anyDuplicated(data$replication)) {
      omtv_stop("A precision cell does not contain the declared checkpoint.")
    }
    mcse <- function(x) stats::sd(x) / sqrt(length(x))
    lower_mcse <- mcse(data$lower_bias)
    upper_mcse <- mcse(data$upper_bias)
    data.frame(
      scenario_id = data$scenario_id[[1L]],
      model_family = data$model_family[[1L]],
      target = data$target[[1L]], checkpoint = checkpoint,
      content_error_mcse = mcse(data$conditional_content_error),
      lower_bias_mcse_over_response_sd =
        lower_mcse / mean(data$response_sd),
      upper_bias_mcse_over_response_sd =
        upper_mcse / mean(data$response_sd),
      endpoint_bias_mcse_over_response_sd =
        max(lower_mcse, upper_mcse) / mean(data$response_sd),
      width_bias_mcse_over_oracle_width =
        mcse(data$width_bias) / mean(data$oracle_mean_width),
      excess_risk_mcse = mcse(data$mean_excess_target_risk),
      mean_response_variance = mean(data$response_sd^2),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out$endpoint_content_precision_pass <- with(out,
    content_error_mcse <=
      config$precision_planning$content_error_mcse_max &
    endpoint_bias_mcse_over_response_sd <=
      config$precision_planning$endpoint_bias_mcse_over_response_sd_max &
    width_bias_mcse_over_oracle_width <=
      config$precision_planning$width_bias_mcse_over_oracle_width_max
  )
  margin_unfrozen <- isTRUE(
    config$precision_planning$excess_risk_practical_margin_unfrozen
  )
  out$excess_risk_mcse_over_response_variance <-
    out$excess_risk_mcse / out$mean_response_variance
  if (margin_unfrozen) {
    out$excess_risk_precision_threshold <- NA_real_
    out$excess_risk_precision_pass <- FALSE
  } else {
    out$excess_risk_precision_threshold <-
      config$precision_planning$excess_risk_practical_margin *
      config$precision_planning$excess_risk_contrast_mcse_fraction
    out$excess_risk_precision_pass <-
      out$excess_risk_mcse_over_response_variance <=
      out$excess_risk_precision_threshold
  }
  out$primary_precision_pass <-
    out$endpoint_content_precision_pass & out$excess_risk_precision_pass
  out
}

omtv_replication_schedule <- function(config) {
  if (!isTRUE(config$replication_schedule_frozen) ||
      !is.list(config$replication_schedule)) {
    omtv_stop("The replication schedule is not frozen.")
  }
  scenarios <- omtv_scenario_frame(config)
  counts <- config$replication_schedule$replications_by_scenario
  if (is.null(names(counts))) {
    omtv_stop("replications_by_scenario must be a named object.")
  }
  values <- vapply(scenarios$scenario_id, function(scenario_id) {
    omtv_scalar_integer(counts[[scenario_id]], paste0(
      "replications_by_scenario$", scenario_id
    ), 1L)
  }, integer(1L))
  wave_size <- omtv_scalar_integer(
    config$replication_schedule$wave_size, "replication_schedule$wave_size", 1L
  )
  if (!all(values %in% as.integer(unlist(
    config$precision_planning$candidate_checkpoints
  )))) {
    omtv_stop("Every frozen replication count must be a declared checkpoint.")
  }
  data.frame(
    scenario_id = scenarios$scenario_id,
    replications = unname(values), wave_size = wave_size,
    stringsAsFactors = FALSE
  )
}

omtv_task_plan <- function(config) {
  schedule <- omtv_replication_schedule(config)
  targets <- as.character(unlist(config$coverage_targets))
  rows <- lapply(seq_len(nrow(schedule)), function(ii) {
    expand.grid(
      scenario_id = schedule$scenario_id[[ii]],
      model_family = c("fixed_design", "dlm"), target = targets,
      replication = seq_len(schedule$replications[[ii]]),
      stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out <- out[order(
    match(out$scenario_id, schedule$scenario_id), out$replication,
    match(out$model_family, c("fixed_design", "dlm")),
    match(out$target, targets)
  ), , drop = FALSE]
  out$task_id <- sprintf("T%08d", seq_len(nrow(out)))
  out$task_key <- paste(
    out$scenario_id, out$model_family, out$target,
    sprintf("rep%04d", out$replication), sep = "/"
  )
  out$wave <- ceiling(out$replication / schedule$wave_size[
    match(out$scenario_id, schedule$scenario_id)
  ])
  if (anyDuplicated(out$task_id) || anyDuplicated(out$task_key)) {
    omtv_stop("The frozen task plan contains duplicate keys.")
  }
  rownames(out) <- NULL
  out
}
