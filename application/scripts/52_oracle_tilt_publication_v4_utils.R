# Utilities for the prospective three-candidate publication-v4 illustration
# screen.  The validated V3 numerical, fitting, and summary functions are
# sourced before this file.  V4 adds candidate-specific random streams,
# target-shared data contracts, single-worker cells, and deterministic
# family-level selection without mutating the closed V3 implementation.

otv4_schema <- function() "rqrgibbs_oracle_tilt_publication/4.0.0"

otv4_config_schema <- function() {
  "rqrgibbs_oracle_tilt_publication_config/4.0.0"
}

otv4_worker_schema <- function() {
  "rqrgibbs_oracle_tilt_publication_worker/2.0.0"
}

otv4_cell_schema <- function() {
  "rqrgibbs_oracle_tilt_publication_cell/2.0.0"
}

otv4_selection_schema <- function() {
  "rqrgibbs_oracle_tilt_seed_selection/1.0.0"
}

otv4_candidate_ids <- function() {
  sprintf("candidate_%02d", seq_len(3L))
}

otv4_stream_names <- function() {
  c("fixed_response", "dlm_state", "dlm_response")
}

otv4_whole_scalar <- function(value, name, lower = 0) {
  value <- as.numeric(value)
  if (length(value) != 1L || is.na(value) || !is.finite(value) ||
      value != floor(value) || value < lower || value > 2^53) {
    oti_stop(name, " must be one exactly representable nonnegative integer.")
  }
  value
}

otv4_v3_template <- function(config) {
  template <- config
  template$schema_version <- otv3_config_schema()
  template$campaign_id <- NULL
  template$candidate_contract <- NULL
  template$selection <- NULL
  template$fixed_design$seed <- 202608011L
  template$fixed_design$workers <- 2L
  template$dlm$seed <- 202608012L
  template$dlm$workers <- 2L
  template$dlm$target_retained_draws <- NULL
  template$recovery_gates$scale_stratum_width_contrast_relative_error_max <-
    0.20
  template$resources <- list(
    minimum_free_bytes = 21474836480,
    maximum_processes = 7L,
    maximum_R_processes = 3L,
    maximum_threads = 8L,
    maximum_sampled_rss_kib = 12582912L,
    maximum_execute_seconds = 28800L,
    monitor_interval_seconds = 0.2
  )
  template$interpretation$prospective_seed_screen <- NULL
  template$interpretation$typical_performance_claim <- NULL
  template$interpretation$automatic_manuscript_promotion <- NULL
  template
}

otv4_candidates <- function(config) {
  values <- config$candidate_contract$candidates
  if (!is.list(values) || length(values) != 3L) {
    oti_stop("The V4 candidate contract must contain exactly three entries.")
  }
  rows <- do.call(rbind, lapply(values, function(value) {
    data.frame(
      candidate_id = as.character(value$candidate_id),
      master_seed = otv3_integer_scalar(
        value$master_seed, "candidate master seed", 1L
      ),
      fixed_chain_seed_base = otv3_integer_scalar(
        value$fixed_chain_seed_base, "fixed chain seed base", 1L
      ),
      dlm_chain_seed_base = otv3_integer_scalar(
        value$dlm_chain_seed_base, "DLM chain seed base", 1L
      ),
      stringsAsFactors = FALSE
    )
  }))
  rownames(rows) <- NULL
  rows
}

otv4_validate_config <- function(config) {
  if (!is.list(config) ||
      !identical(as.character(config$schema_version), otv4_config_schema()) ||
      !identical(as.character(config$campaign_id),
                 "publication_v4_seed_screen")) {
    oti_stop("Unsupported oracle-tilt publication V4 configuration.")
  }
  otv3_required_logical(config$execution_authorized, "execution_authorized")

  # Reuse the strict V3 validator for the unchanged scientific construction.
  # The adapter restores only the V3 worker/resource fields that are
  # intentionally different in V4.
  otv3_validate_config(otv4_v3_template(config))

  candidate_contract <- config$candidate_contract %||% list()
  if (otv3_integer_scalar(
        candidate_contract$candidate_count, "candidate_count", 1L
      ) != 3L ||
      !identical(as.character(candidate_contract$selection_unit),
                 "family_shared_across_targets") ||
      !identical(as.character(candidate_contract$rng_kind),
                 "L'Ecuyer-CMRG") ||
      !identical(as.character(candidate_contract$normal_kind), "Inversion") ||
      !identical(as.character(candidate_contract$sample_kind), "Rejection") ||
      !identical(
        as.character(unlist(candidate_contract$named_dgp_streams)),
        otv4_stream_names()
      )) {
    oti_stop("The V4 candidate/RNG contract changed.")
  }
  candidates <- otv4_candidates(config)
  if (!identical(candidates$candidate_id, otv4_candidate_ids()) ||
      anyDuplicated(candidates$master_seed) ||
      anyDuplicated(c(candidates$fixed_chain_seed_base,
                      candidates$dlm_chain_seed_base))) {
    oti_stop("Candidate identifiers and seed bases must be ordered and unique.")
  }
  auxiliary <- vapply(
    c("reference_seed", "rehearsal_seed", "selector_fixture_seed"),
    function(name) otv3_integer_scalar(
      candidate_contract[[name]], paste0(name), 1L
    ), integer(1L)
  )
  if (anyDuplicated(c(candidates$master_seed,
                      candidates$fixed_chain_seed_base,
                      candidates$dlm_chain_seed_base, auxiliary))) {
    oti_stop("DGP, chain, reference, rehearsal, and selector seeds overlap.")
  }

  if (otv3_integer_scalar(
        config$fixed_design$workers, "fixed_design$workers", 1L
      ) != 1L ||
      otv3_integer_scalar(config$dlm$workers, "dlm$workers", 1L) != 1L) {
    oti_stop("Every V4 fit cell must use exactly one chain worker.")
  }
  retained <- config$dlm$target_retained_draws %||% list()
  retained <- vapply(c("RQR", "ET", "SH"), function(target) {
    otv3_integer_scalar(
      retained[[target]], paste0("dlm retained draws for ", target), 1L
    )
  }, integer(1L))
  if (!identical(unname(retained), c(6000L, 6000L, 12000L))) {
    oti_stop("The V4 DLM target-specific retained-draw contract changed.")
  }
  if (!otv3_close(
    config$recovery_gates$scale_stratum_width_contrast_relative_error_max,
    0.21
  )) {
    oti_stop("The prospectively declared V4 width-contrast reference is 0.21.")
  }

  selection <- config$selection %||% list()
  required_ranking <- c(
    "minimum_worst_standardized_discrepancy",
    "minimum_mean_standardized_discrepancy",
    "minimum_mean_endpoint_rmse_over_oracle_width",
    "lowest_candidate_id"
  )
  if (!identical(as.character(selection$schema_version),
                 otv4_selection_schema()) ||
      !identical(as.character(selection$selection_unit),
                 "family_shared_across_targets") ||
      !identical(as.character(unlist(selection$required_targets)),
                 c("RQR", "ET", "SH")) ||
      !isTRUE(selection$require_all_cells_computationally_eligible) ||
      !isTRUE(selection$require_all_cells_gross_recovery_eligible) ||
      !identical(as.character(selection$strict_recovery_role),
                 "reported_not_automatic_rejection") ||
      !identical(selection$realized_content_in_score, FALSE) ||
      !identical(selection$aesthetic_judgment_in_score, FALSE) ||
      !identical(as.character(unlist(selection$ranking)),
                 required_ranking)) {
    oti_stop("The prospective family-level selection contract changed.")
  }

  resources <- config$resources %||% list()
  expected_resources <- c(
    minimum_free_bytes = 53687091200,
    minimum_available_memory_bytes = 107374182400,
    minimum_idle_logical_cpus = 24,
    maximum_fit_workers = 18,
    maximum_processes = 64,
    maximum_R_processes = 19,
    maximum_threads = 64,
    maximum_sampled_rss_kib = 100663296,
    maximum_execute_seconds = 43200
  )
  for (name in names(expected_resources)) {
    value <- otv4_whole_scalar(resources[[name]], name, 1)
    if (!identical(value, as.numeric(expected_resources[[name]]))) {
      oti_stop("The V4 resource contract changed: ", name, ".")
    }
  }
  if (!otv3_close(resources$monitor_interval_seconds, 0.2) ||
      !isTRUE(resources$require_no_other_rqrgibbs_heavy_processes)) {
    oti_stop("The V4 monitor/host-exclusion contract changed.")
  }

  interpretation <- config$interpretation %||% list()
  required_false <- c(
    "response_likelihood", "response_predictive_draws",
    "cornish_fisher_used", "simulation_study", "typical_performance_claim",
    "automatic_manuscript_promotion"
  )
  if (any(vapply(required_false, function(name) {
    !identical(interpretation[[name]], FALSE)
  }, logical(1L))) ||
      !isTRUE(interpretation$population_oracle_tilts) ||
      !isTRUE(interpretation$prospective_seed_screen)) {
    oti_stop("The V4 interpretation contract is incomplete.")
  }
  invisible(config)
}

otv4_rng_state <- function(master_seed, stream_index = 1L) {
  master_seed <- otv3_integer_scalar(master_seed, "master_seed", 1L)
  stream_index <- otv3_integer_scalar(stream_index, "stream_index", 1L)
  old_kind <- RNGkind()
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit({
    do.call(RNGkind, as.list(old_kind))
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  RNGkind("L'Ecuyer-CMRG", "Inversion", "Rejection")
  set.seed(master_seed)
  state <- get(".Random.seed", envir = .GlobalEnv)
  if (stream_index > 1L) {
    for (index in seq_len(stream_index - 1L)) {
      state <- parallel::nextRNGStream(state)
    }
  }
  as.integer(state)
}

otv4_with_rng_state <- function(state, expression) {
  state <- as.integer(state)
  if (length(state) != 7L || anyNA(state)) {
    oti_stop("A V4 L'Ecuyer-CMRG stream must contain seven integers.")
  }
  old_kind <- RNGkind()
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit({
    do.call(RNGkind, as.list(old_kind))
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  RNGkind("L'Ecuyer-CMRG", "Inversion", "Rejection")
  assign(".Random.seed", state, envir = .GlobalEnv)
  force(expression)
}

otv4_seed_manifest <- function(config) {
  otv4_validate_config(config)
  candidates <- otv4_candidates(config)
  rows <- list()
  for (candidate_index in seq_len(nrow(candidates))) {
    for (stream_index in seq_along(otv4_stream_names())) {
      state <- otv4_rng_state(
        candidates$master_seed[candidate_index], stream_index
      )
      rows[[length(rows) + 1L]] <- data.frame(
        candidate_id = candidates$candidate_id[candidate_index],
        master_seed = candidates$master_seed[candidate_index],
        stream = otv4_stream_names()[stream_index],
        stream_index = stream_index,
        rng_kind = "L'Ecuyer-CMRG", normal_kind = "Inversion",
        sample_kind = "Rejection",
        state = paste(state, collapse = ";"),
        state_digest = otf_object_sha256(state),
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

otv4_candidate_config <- function(config, candidate_id, target = NULL) {
  candidates <- otv4_candidates(config)
  selected <- candidates$candidate_id == as.character(candidate_id)
  if (sum(selected) != 1L) oti_stop("Unknown V4 candidate identifier.")
  value <- config
  value$fixed_design$seed <- candidates$fixed_chain_seed_base[selected]
  value$dlm$seed <- candidates$dlm_chain_seed_base[selected]
  if (!is.null(target)) {
    target <- toupper(as.character(target)[1L])
    if (!target %in% c("RQR", "ET", "SH")) oti_stop("Unknown V4 target.")
    value$dlm$mcmc_control$n_mcmc <- otv3_integer_scalar(
      value$dlm$target_retained_draws[[target]], "target retained draws", 1L
    )
  }
  value
}

otv4_chain_seed <- function(config, candidate_id, family, target, chain) {
  value <- otv4_candidate_config(config, candidate_id, target)
  otp_seed(value, family, target, chain)
}

otv4_candidate_stream_state <- function(config, candidate_id, stream) {
  candidates <- otv4_candidates(config)
  selected <- candidates$candidate_id == as.character(candidate_id)
  stream_index <- match(as.character(stream), otv4_stream_names())
  if (sum(selected) != 1L || is.na(stream_index)) {
    oti_stop("Unknown candidate or named DGP stream.")
  }
  otv4_rng_state(candidates$master_seed[selected], stream_index)
}

otv4_fixed_design_dgp <- function(config, law, candidate_id) {
  state <- otv4_candidate_stream_state(
    config, candidate_id, "fixed_response"
  )
  candidate_config <- otv4_candidate_config(config, candidate_id)
  n <- as.integer(candidate_config$fixed_design$n)
  x <- seq(-1, 1, length.out = n)
  contract <- candidate_config$fixed_design$basis_contract
  basis <- otv3_empirical_spline_basis(x, contract)
  prior <- otv3_static_prior_audit(candidate_config, basis)
  mean_coefficients <- otv3_numeric_vector(
    contract$mean_control_coefficients, "mean coefficients"
  )
  scale_coefficients <- otv3_numeric_vector(
    contract$scale_control_coefficients, "scale coefficients",
    lower = .Machine$double.eps
  )
  mu <- drop(basis$raw %*% mean_coefficients)
  scale <- drop(basis$raw %*% scale_coefficients)
  scale_ratio <- max(scale) / min(scale)
  if (min(scale) < contract$minimum_scale ||
      scale_ratio < contract$minimum_scale_ratio ||
      scale_ratio > contract$maximum_scale_ratio) {
    oti_stop("The static scale function violates its frozen range contract.")
  }
  innovations <- otv4_with_rng_state(state, law$r(n))
  y <- mu + scale * innovations
  list(
    family = "fixed_design", candidate_id = candidate_id,
    master_seed = otv4_candidates(config)$master_seed[
      otv4_candidates(config)$candidate_id == candidate_id
    ],
    dgp_stream = "fixed_response",
    dgp_stream_digest = otf_object_sha256(state),
    x = x, X = basis$X, y = y, innovation_truth = innovations,
    mean_truth = mu, scale_truth = scale,
    observed = rep(TRUE, n), basis = basis, prior_audit = prior,
    mean_control_coefficients = mean_coefficients,
    scale_control_coefficients = scale_coefficients,
    scale_ratio = scale_ratio, ridge_tau2 = prior$selected_tau2
  )
}

otv4_dlm_dgp <- function(config, law, candidate_id) {
  candidate_config <- otv4_candidate_config(config, candidate_id)
  state_stream <- otv4_candidate_stream_state(config, candidate_id, "dlm_state")
  response_stream <- otv4_candidate_stream_state(
    config, candidate_id, "dlm_response"
  )
  time_contract <- otv3_time_grid(candidate_config)
  prior <- otv3_dlm_prior_audit(candidate_config)
  seasonal_prior <- otv3_seasonal_prior_audit(candidate_config)
  selected <- prior$selected
  matrices <- otv3_local_linear_matrices(
    time_contract$delta, selected$q_level, selected$q_slope
  )
  T <- as.integer(candidate_config$dlm$T)
  trend_GG <- otv3_expand_cube(matrices$G, T)
  trend_C0 <- diag(c(
    selected$initial_level_variance,
    selected$initial_slope_variance
  ))
  trend_model <- rqrgibbs::rqr_as_dlm_model(list(
    FF = matrix(c(1, 0), 2L, 1L), GG = trend_GG,
    m0 = c(0, 0), C0 = trend_C0,
    component_dims = 2L, component_names = "local_linear"
  ))
  seasonal_contract <- candidate_config$dlm$seasonal_contract
  seasonal_model <- rqrgibbs::rqr_seasonal(
    period = seasonal_contract$period,
    harmonics = seasonal_contract$harmonic,
    m0 = c(0, 0), C0 = diag(seasonal_prior$selected_variance, 2L),
    name = "seasonal_harmonic"
  )
  model <- trend_model + seasonal_model
  W_one <- otv3_block_diag(
    matrices$W, diag(seasonal_contract$evolution_variance, 2L)
  )
  W <- otv3_expand_cube(W_one, T)

  local_theta <- otv4_with_rng_state(state_stream, {
    value <- matrix(NA_real_, 2L, T)
    previous <- as.numeric(unlist(candidate_config$dlm$initial_state))
    if (length(previous) != 2L || any(!is.finite(previous))) {
      oti_stop("dlm$initial_state must contain two finite values.")
    }
    factor <- t(chol(matrices$W))
    for (index in seq_len(T)) {
      previous <- drop(
        matrices$G %*% previous + factor %*% stats::rnorm(2L)
      )
      value[, index] <- previous
    }
    value
  })
  omega <- 2 * pi * seasonal_contract$harmonic /
    seasonal_contract$period
  indices <- seq_len(T)
  mean_seasonal <- seasonal_contract$mean_amplitude *
    sin(omega * indices + seasonal_contract$mean_phase)
  mean_seasonal_state <- rbind(
    mean_seasonal,
    seasonal_contract$mean_amplitude *
      cos(omega * indices + seasonal_contract$mean_phase)
  )
  scale_contract <- candidate_config$dlm$scale_contract
  scale_seasonal <- scale_contract$amplitude *
    cos(omega * indices + scale_contract$phase)
  scale_seasonal_state <- rbind(
    scale_seasonal,
    -scale_contract$amplitude *
      sin(omega * indices + scale_contract$phase)
  )
  scale <- scale_contract$baseline + scale_seasonal
  scale_ratio <- max(scale) / min(scale)
  if (min(scale) < scale_contract$minimum_scale ||
      scale_ratio < scale_contract$minimum_scale_ratio ||
      scale_ratio > scale_contract$maximum_scale_ratio) {
    oti_stop("The dynamic scale function violates its frozen range contract.")
  }
  mean_truth <- local_theta[1L, ] + mean_seasonal
  innovations <- otv4_with_rng_state(response_stream, law$r(T))
  y_full <- mean_truth + scale * innovations
  y <- y_full
  y[time_contract$missing] <- NA_real_
  list(
    family = "dlm", candidate_id = candidate_id,
    master_seed = otv4_candidates(config)$master_seed[
      otv4_candidates(config)$candidate_id == candidate_id
    ],
    dgp_state_stream_digest = otf_object_sha256(state_stream),
    dgp_response_stream_digest = otf_object_sha256(response_stream),
    time = time_contract$time, delta = time_contract$delta,
    y = y, y_full = y_full, innovation_truth = innovations,
    model = model, W = W, mean_truth = mean_truth,
    state_truth = t(rbind(local_theta, mean_seasonal_state)),
    local_state_truth = t(local_theta),
    mean_seasonal_state_truth = t(mean_seasonal_state),
    scale_seasonal_state_truth = t(scale_seasonal_state),
    scale_baseline = scale_contract$baseline,
    scale_truth = scale, scale_ratio = scale_ratio,
    initial_level_variance = selected$initial_level_variance,
    initial_slope_variance = selected$initial_slope_variance,
    q_level = selected$q_level, q_slope = selected$q_slope,
    observed = !time_contract$missing,
    missing_times = which(time_contract$missing),
    missing_windows = time_contract$windows,
    seasonal_period = seasonal_contract$period,
    seasonal_harmonic = seasonal_contract$harmonic,
    prior_audit = prior, seasonal_prior_audit = seasonal_prior,
    fixed_horizon_audit = otv3_fixed_horizon_audit(
      trend_C0, selected$q_level, selected$q_slope
    )
  )
}

otv4_plan <- function(config) {
  otv4_validate_config(config)
  rows <- list()
  candidates <- otv4_candidates(config)
  for (candidate_id in candidates$candidate_id) {
    for (target in c("RQR", "ET", "SH")) {
      fixed_profiles <- as.character(unlist(config$fixed_design$initial_profiles))
      fixed <- data.frame(
        candidate_id = candidate_id, family = "fixed_design", target = target,
        chain = seq_along(fixed_profiles), profile = fixed_profiles,
        n_burn = as.integer(config$fixed_design$mcmc_control$n_burn),
        n_mcmc = as.integer(config$fixed_design$mcmc_control$n_mcmc),
        stringsAsFactors = FALSE
      )
      dlm_profiles <- as.character(unlist(config$dlm$initial_profiles))
      dlm <- data.frame(
        candidate_id = candidate_id, family = "dlm", target = target,
        chain = seq_along(dlm_profiles), profile = dlm_profiles,
        n_burn = as.integer(config$dlm$mcmc_control$n_burn),
        n_mcmc = as.integer(config$dlm$target_retained_draws[[target]]),
        stringsAsFactors = FALSE
      )
      rows[[length(rows) + 1L]] <- fixed
      rows[[length(rows) + 1L]] <- dlm
    }
  }
  out <- do.call(rbind, rows)
  out$cell <- paste(out$candidate_id, out$family, out$target, sep = "/")
  out$learning_rate_mode <- "fixed_rate"
  out$tilt_source <- "exact_population_oracle"
  out$cornish_fisher_used <- FALSE
  out$seed <- mapply(
    function(candidate_id, family, target, chain) {
      otv4_chain_seed(config, candidate_id, family, target, chain)
    }, out$candidate_id, out$family, out$target, out$chain
  )
  if (nrow(out) != 81L || anyDuplicated(out$seed) ||
      any(out$cornish_fisher_used)) {
    oti_stop("The V4 81-chain plan is invalid.")
  }
  rownames(out) <- NULL
  out
}

otv4_cell_plan <- function(fit_plan) {
  required <- c("candidate_id", "family", "target")
  if (!is.data.frame(fit_plan) || !all(required %in% names(fit_plan))) {
    oti_stop("The V4 fit plan cannot define its cell plan.")
  }
  key <- do.call(paste, c(fit_plan[required], sep = "\r"))
  out <- fit_plan[!duplicated(key), required, drop = FALSE]
  rownames(out) <- NULL
  expected <- expand.grid(
    candidate_id = otv4_candidate_ids(),
    family = c("fixed_design", "dlm"),
    target = c("RQR", "ET", "SH"),
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  got_key <- do.call(paste, c(out[required], sep = "/"))
  expected_key <- do.call(paste, c(expected[required], sep = "/"))
  if (nrow(out) != 18L || anyDuplicated(got_key) ||
      !setequal(got_key, expected_key)) {
    oti_stop("The V4 cell plan must contain 18 unique candidate cells.")
  }
  out$order <- seq_len(nrow(out))
  out$cell_key <- paste(
    out$candidate_id, out$family, tolower(out$target), sep = "_"
  )
  out$process_isolation <- TRUE
  out$maximum_chain_workers <- 1L
  out
}

otv4_gate_pass <- function(value, threshold, comparison) {
  switch(
    comparison,
    ">=" = value >= threshold,
    "<=" = value <= threshold,
    "==" = abs(value - threshold) <= 1e-12,
    FALSE
  )
}

otv4_candidate_preflight <- function(config, candidate_id) {
  candidate_config <- otv4_candidate_config(config, candidate_id)
  law <- otv3_law(candidate_config)
  oracle <- otv3_exact_zero_rqr_oracle(oti_oracle_targets(
    law, candidate_config$coverage_level, candidate_config$targets
  ))
  fixed <- otv4_fixed_design_dgp(config, law, candidate_id)
  dlm <- otv4_dlm_dgp(config, law, candidate_id)
  fixed_targets <- oti_targets_by_index(
    fixed$mean_truth, fixed$scale_truth, oracle, fixed$observed
  )
  dlm_targets <- oti_targets_by_index(
    dlm$mean_truth, dlm$scale_truth, oracle, dlm$observed
  )
  fixed_initializations <- lapply(c("RQR", "ET", "SH"), function(target) {
    otv3_static_initialization_profiles(
      candidate_config, fixed, oti_target_row(fixed_targets, target), law
    )
  })
  names(fixed_initializations) <- c("RQR", "ET", "SH")
  initialization_audit <- do.call(rbind, lapply(
    fixed_initializations, `[[`, "audit"
  ))
  tail <- otv3_tail_information(
    candidate_config, oracle, sum(dlm$observed)
  )
  projection <- otv3_projection_audit(fixed, fixed_targets)
  dynamic_projection <- otv3_dynamic_projection_audit(dlm, dlm_targets)
  seasonal_covariance <- otv3_seasonal_covariance_audit(dlm)
  observability <- otv3_observability_audit(dlm)
  scale_information <- otv3_scale_information(
    candidate_config, oracle, fixed, dlm
  )
  gram_error <- max(abs(fixed$basis$gram - diag(ncol(fixed$basis$X))))
  horizon_error <- max(dlm$fixed_horizon_audit$max_absolute_error)
  candidate_plan <- otv4_plan(config)
  candidate_plan <- candidate_plan[
    candidate_plan$candidate_id == candidate_id, , drop = FALSE
  ]
  gates <- data.frame(
    candidate_id = candidate_id,
    gate = c(
      "expected_rare_tail_count", "static_design_rank",
      "static_basis_gram", "static_truth_projection",
      "dynamic_truth_projection", "fixed_horizon_covariance",
      "seasonal_covariance_recursion", "dynamic_observability_rank",
      "static_scale_floor", "dynamic_scale_floor",
      "scale_quintile_rare_tail_count", "missing_mask", "fit_plan_size",
      "static_initialization_count", "static_initialization_minimum_width",
      "static_initialization_unique_digests", "cornish_fisher_absent"
    ),
    value = c(
      min(c(tail$fixed_design_expected_rare_count,
            tail$dlm_expected_rare_count)),
      fixed$basis$rank, gram_error,
      max(projection$max_absolute_residual),
      max(dynamic_projection$max_absolute_residual), horizon_error,
      seasonal_covariance$maximum_covariance_recursion_error,
      observability$rank, min(fixed$scale_truth), min(dlm$scale_truth),
      min(scale_information$expected_rare_tail_count),
      sum(!dlm$observed), nrow(candidate_plan),
      nrow(initialization_audit), min(initialization_audit$minimum_width),
      length(unique(initialization_audit$initialization_digest)), 0
    ),
    threshold = c(
      config$preflight_gates$minimum_expected_rare_tail_count,
      config$preflight_gates$static_design_rank,
      config$preflight_gates$static_gram_max_abs_error,
      config$preflight_gates$static_projection_max_abs,
      config$preflight_gates$dynamic_projection_max_abs,
      config$preflight_gates$fixed_horizon_covariance_max_abs_error,
      config$preflight_gates$seasonal_covariance_recursion_max_abs_error,
      config$preflight_gates$dynamic_observability_rank,
      config$fixed_design$basis_contract$minimum_scale,
      config$dlm$scale_contract$minimum_scale,
      config$preflight_gates$minimum_expected_rare_tail_count_per_scale_quintile,
      config$dlm$expected_missing, 27, 12,
      config$fixed_design$initialization_contract$minimum_initial_width,
      12, 0
    ),
    comparison = c(
      ">=", "==", "<=", "<=", "<=", "<=", "<=", "==",
      ">=", ">=", ">=", "==", "==", "==", ">=", "==", "=="
    ),
    stringsAsFactors = FALSE
  )
  gates$pass <- mapply(
    otv4_gate_pass, gates$value, gates$threshold, gates$comparison
  )
  list(
    candidate_id = candidate_id, config = candidate_config,
    law = law, oracle = oracle, fixed_dgp = fixed, dlm_dgp = dlm,
    fixed_targets = fixed_targets, dlm_targets = dlm_targets,
    tail_information = tail, projection_audit = projection,
    dynamic_projection_audit = dynamic_projection,
    seasonal_covariance_audit = seasonal_covariance,
    observability_audit = observability,
    scale_information = scale_information,
    static_prior_audit = fixed$prior_audit$table,
    dlm_prior_audit = dlm$prior_audit$table,
    seasonal_prior_audit = dlm$seasonal_prior_audit$table,
    fixed_initializations = fixed_initializations,
    fixed_initialization_audit = initialization_audit,
    fixed_horizon_audit = dlm$fixed_horizon_audit,
    plan = candidate_plan, gates = gates, pass = all(gates$pass)
  )
}

otv4_design_preflight <- function(config) {
  otv4_validate_config(config)
  candidates <- lapply(
    otv4_candidate_ids(),
    function(candidate_id) otv4_candidate_preflight(config, candidate_id)
  )
  names(candidates) <- otv4_candidate_ids()
  plan <- otv4_plan(config)
  cell_plan <- otv4_cell_plan(plan)
  seed_manifest <- otv4_seed_manifest(config)

  dgp_rows <- do.call(rbind, lapply(candidates, function(value) {
    data.frame(
      candidate_id = value$candidate_id,
      fixed_dgp_digest = otf_object_sha256(value$fixed_dgp),
      fixed_response_digest = otf_object_sha256(value$fixed_dgp$y),
      fixed_design_digest = otf_object_sha256(list(
        x = value$fixed_dgp$x, X = value$fixed_dgp$X,
        mean_truth = value$fixed_dgp$mean_truth,
        scale_truth = value$fixed_dgp$scale_truth,
        observed = value$fixed_dgp$observed
      )),
      fixed_target_digest = otf_object_sha256(value$fixed_targets),
      dlm_dgp_digest = otf_object_sha256(value$dlm_dgp),
      dlm_response_digest = otf_object_sha256(value$dlm_dgp$y_full),
      dlm_state_digest = otf_object_sha256(value$dlm_dgp$local_state_truth),
      dlm_design_digest = otf_object_sha256(list(
        time = value$dlm_dgp$time, model = value$dlm_dgp$model,
        W = value$dlm_dgp$W, scale_truth = value$dlm_dgp$scale_truth,
        observed = value$dlm_dgp$observed,
        mean_seasonal_state_truth =
          value$dlm_dgp$mean_seasonal_state_truth
      )),
      dlm_target_digest = otf_object_sha256(value$dlm_targets),
      stringsAsFactors = FALSE
    )
  }))
  cross_gates <- data.frame(
    gate = c(
      "candidate_count", "cell_count", "chain_count",
      "unique_chain_seeds", "unique_dgp_streams",
      "fixed_responses_differ", "dlm_responses_differ",
      "dlm_states_differ", "fixed_design_shared",
      "dlm_design_shared", "candidate_preflights"
    ),
    value = c(
      length(candidates), nrow(cell_plan), nrow(plan),
      length(unique(plan$seed)), length(unique(seed_manifest$state_digest)),
      length(unique(dgp_rows$fixed_response_digest)),
      length(unique(dgp_rows$dlm_response_digest)),
      length(unique(dgp_rows$dlm_state_digest)),
      length(unique(dgp_rows$fixed_design_digest)),
      length(unique(dgp_rows$dlm_design_digest)),
      sum(vapply(candidates, `[[`, logical(1L), "pass"))
    ),
    threshold = c(3, 18, 81, 81, 9, 3, 3, 3, 1, 1, 3),
    comparison = rep("==", 11L), stringsAsFactors = FALSE
  )
  cross_gates$pass <- mapply(
    otv4_gate_pass, cross_gates$value, cross_gates$threshold,
    cross_gates$comparison
  )
  list(
    schema_version = otv4_schema(), candidates = candidates,
    plan = plan, cell_plan = cell_plan, seed_manifest = seed_manifest,
    dgp_manifest = dgp_rows,
    candidate_gates = do.call(rbind, lapply(candidates, `[[`, "gates")),
    cross_candidate_gates = cross_gates,
    pass = all(cross_gates$pass) &&
      all(vapply(candidates, `[[`, logical(1L), "pass"))
  )
}

otv4_gross_cell_eligibility <- function(summary, config) {
  if (!is.data.frame(summary) || nrow(summary) != 1L) {
    oti_stop("Gross cell eligibility requires one fit-summary row.")
  }
  gates <- config$recovery_gates
  required <- c(
    "computational_pass", "pathology_pass",
    "endpoint_rmse_over_oracle_width", "mean_width_ratio",
    "lower_bias_over_oracle_width", "upper_bias_over_oracle_width",
    "low_scale_endpoint_rmse_over_local_width",
    "high_scale_endpoint_rmse_over_local_width",
    "width_contrast_relative_error", "seasonal_width_amplitude_ratio",
    "seasonal_width_phase_error", "family"
  )
  if (!all(required %in% names(summary))) {
    oti_stop("The fit summary lacks V4 gross-eligibility fields.")
  }
  gross_recovery <- with(
    summary,
    is.finite(endpoint_rmse_over_oracle_width) &&
      endpoint_rmse_over_oracle_width <=
        2 * gates$endpoint_rmse_over_oracle_width_max &&
      is.finite(mean_width_ratio) &&
      mean_width_ratio >= max(0, gates$mean_width_ratio_min - 0.20) &&
      mean_width_ratio <= gates$mean_width_ratio_max + 0.20 &&
      abs(lower_bias_over_oracle_width) <=
        2 * gates$absolute_endpoint_bias_over_oracle_width_max &&
      abs(upper_bias_over_oracle_width) <=
        2 * gates$absolute_endpoint_bias_over_oracle_width_max
  )
  gross_heterogeneity <- with(
    summary,
    is.finite(low_scale_endpoint_rmse_over_local_width) &&
      low_scale_endpoint_rmse_over_local_width <=
        2 * gates$scale_stratum_endpoint_rmse_over_local_width_max &&
      is.finite(high_scale_endpoint_rmse_over_local_width) &&
      high_scale_endpoint_rmse_over_local_width <=
        2 * gates$scale_stratum_endpoint_rmse_over_local_width_max &&
      is.finite(width_contrast_relative_error) &&
      width_contrast_relative_error <=
        2 * gates$scale_stratum_width_contrast_relative_error_max
  )
  if (identical(as.character(summary$family), "dlm")) {
    gross_heterogeneity <- gross_heterogeneity && with(
      summary,
      is.finite(seasonal_width_amplitude_ratio) &&
        seasonal_width_amplitude_ratio >= 0.5 &&
        seasonal_width_amplitude_ratio <= 1.5 &&
        is.finite(seasonal_width_phase_error) &&
        seasonal_width_phase_error <= 0.70
    )
  }
  data.frame(
    computationally_eligible = isTRUE(summary$computational_pass) &&
      isTRUE(summary$pathology_pass),
    gross_recovery_eligible = isTRUE(gross_recovery) &&
      isTRUE(gross_heterogeneity),
    selection_eligible = isTRUE(summary$computational_pass) &&
      isTRUE(summary$pathology_pass) && isTRUE(gross_recovery) &&
      isTRUE(gross_heterogeneity),
    stringsAsFactors = FALSE
  )
}

otv4_score_components <- function(summary, config) {
  if (!is.data.frame(summary) || nrow(summary) != 1L) {
    oti_stop("V4 scoring requires one fit-summary row.")
  }
  family <- as.character(summary$family)
  if (!family %in% c("fixed_design", "dlm")) {
    oti_stop("Unknown family in V4 scoring.")
  }
  gates <- config$recovery_gates
  width_ratio <- as.numeric(summary$mean_width_ratio)
  width_reference <- if (width_ratio < 1) {
    1 - gates$mean_width_ratio_min
  } else {
    gates$mean_width_ratio_max - 1
  }
  rows <- data.frame(
    component = c(
      "endpoint_rmse", "mean_width_distance",
      "maximum_absolute_endpoint_bias", "maximum_scale_stratum_rmse",
      "width_contrast_error"
    ),
    raw_value = c(
      summary$endpoint_rmse_over_oracle_width,
      abs(width_ratio - 1),
      max(abs(c(summary$lower_bias_over_oracle_width,
                summary$upper_bias_over_oracle_width))),
      max(c(summary$low_scale_endpoint_rmse_over_local_width,
            summary$high_scale_endpoint_rmse_over_local_width)),
      summary$width_contrast_relative_error
    ),
    reference = c(
      gates$endpoint_rmse_over_oracle_width_max,
      width_reference,
      gates$absolute_endpoint_bias_over_oracle_width_max,
      gates$scale_stratum_endpoint_rmse_over_local_width_max,
      gates$scale_stratum_width_contrast_relative_error_max
    ),
    stringsAsFactors = FALSE
  )
  if (identical(family, "fixed_design")) {
    rows <- rbind(rows, data.frame(
      component = "static_edge_center_rmse_ratio",
      raw_value = summary$static_edge_center_rmse_ratio,
      reference = gates$static_edge_center_rmse_ratio_max,
      stringsAsFactors = FALSE
    ))
  } else {
    rows <- rbind(rows, data.frame(
      component = c(
        "seasonal_width_amplitude_distance", "seasonal_width_phase_error"
      ),
      raw_value = c(
        abs(summary$seasonal_width_amplitude_ratio - 1),
        summary$seasonal_width_phase_error
      ),
      reference = c(
        max(
          1 - gates$dlm_seasonal_width_amplitude_ratio_min,
          gates$dlm_seasonal_width_amplitude_ratio_max - 1
        ),
        gates$dlm_seasonal_width_phase_error_max
      ),
      stringsAsFactors = FALSE
    ))
  }
  if (any(!is.finite(rows$raw_value)) ||
      any(!is.finite(rows$reference)) || any(rows$reference <= 0)) {
    oti_stop("V4 score components must be finite with positive references.")
  }
  rows$standardized_discrepancy <- rows$raw_value / rows$reference
  rows
}

otv4_select_candidates <- function(fit_summary, config) {
  otv4_validate_config(config)
  required <- c(
    "candidate_id", "family", "target", "computational_pass",
    "pathology_pass", "recovery_pass", "heterogeneity_pass",
    "endpoint_rmse_over_oracle_width", "mean_width_ratio",
    "lower_bias_over_oracle_width", "upper_bias_over_oracle_width",
    "static_edge_center_rmse_ratio",
    "low_scale_endpoint_rmse_over_local_width",
    "high_scale_endpoint_rmse_over_local_width",
    "width_contrast_relative_error", "seasonal_width_amplitude_ratio",
    "seasonal_width_phase_error"
  )
  if (!is.data.frame(fit_summary) || nrow(fit_summary) != 18L ||
      !all(required %in% names(fit_summary))) {
    oti_stop("V4 selection requires one complete 18-cell fit summary.")
  }
  fit_summary$candidate_id <- as.character(fit_summary$candidate_id)
  fit_summary$family <- as.character(fit_summary$family)
  fit_summary$target <- toupper(as.character(fit_summary$target))
  keys <- paste(
    fit_summary$candidate_id, fit_summary$family, fit_summary$target, sep = "/"
  )
  expected <- expand.grid(
    candidate_id = otv4_candidate_ids(),
    family = c("fixed_design", "dlm"),
    target = c("RQR", "ET", "SH"),
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  expected_keys <- paste(
    expected$candidate_id, expected$family, expected$target, sep = "/"
  )
  if (anyDuplicated(keys) || !setequal(keys, expected_keys)) {
    oti_stop("V4 selection forbids missing, duplicate, or mixed target cells.")
  }
  fit_summary <- fit_summary[order(
    fit_summary$family, fit_summary$candidate_id,
    match(fit_summary$target, c("RQR", "ET", "SH"))
  ), , drop = FALSE]
  rownames(fit_summary) <- NULL

  eligibility <- do.call(rbind, lapply(seq_len(nrow(fit_summary)), function(i) {
    otv4_gross_cell_eligibility(fit_summary[i, , drop = FALSE], config)
  }))
  cell_audit <- cbind(
    fit_summary[, c(
      "candidate_id", "family", "target", "computational_pass",
      "pathology_pass", "recovery_pass", "heterogeneity_pass"
    ), drop = FALSE],
    eligibility
  )
  components <- do.call(rbind, lapply(seq_len(nrow(fit_summary)), function(i) {
    identity <- fit_summary[
      i, c("candidate_id", "family", "target"), drop = FALSE
    ]
    rownames(identity) <- NULL
    score <- otv4_score_components(fit_summary[i, , drop = FALSE], config)
    rownames(score) <- NULL
    cbind(identity[rep(1L, nrow(score)), , drop = FALSE], score)
  }))
  rownames(components) <- NULL

  ranking_rows <- list()
  selected_rows <- list()
  for (family in c("fixed_design", "dlm")) {
    for (candidate_id in otv4_candidate_ids()) {
      selected_cells <- cell_audit$family == family &
        cell_audit$candidate_id == candidate_id
      selected_components <- components$family == family &
        components$candidate_id == candidate_id
      endpoint_rows <- fit_summary$family == family &
        fit_summary$candidate_id == candidate_id
      eligible <- sum(selected_cells) == 3L &&
        all(cell_audit$selection_eligible[selected_cells])
      ranking_rows[[length(ranking_rows) + 1L]] <- data.frame(
        family = family, candidate_id = candidate_id,
        eligible = eligible,
        eligible_cells = sum(cell_audit$selection_eligible[selected_cells]),
        strict_pass_cells = sum(
          cell_audit$recovery_pass[selected_cells] &
            cell_audit$heterogeneity_pass[selected_cells]
        ),
        worst_standardized_discrepancy = if (eligible) {
          max(components$standardized_discrepancy[selected_components])
        } else Inf,
        mean_standardized_discrepancy = if (eligible) {
          mean(components$standardized_discrepancy[selected_components])
        } else Inf,
        mean_endpoint_rmse_over_oracle_width = if (eligible) {
          mean(fit_summary$endpoint_rmse_over_oracle_width[endpoint_rows])
        } else Inf,
        stringsAsFactors = FALSE
      )
    }
    family_ranking <- do.call(rbind, ranking_rows)
    family_ranking <- family_ranking[family_ranking$family == family, , drop = FALSE]
    eligible_ranking <- family_ranking[family_ranking$eligible, , drop = FALSE]
    if (nrow(eligible_ranking)) {
      eligible_ranking <- eligible_ranking[order(
        eligible_ranking$worst_standardized_discrepancy,
        eligible_ranking$mean_standardized_discrepancy,
        eligible_ranking$mean_endpoint_rmse_over_oracle_width,
        eligible_ranking$candidate_id
      ), , drop = FALSE]
      winner <- eligible_ranking[1L, , drop = FALSE]
      selected_rows[[length(selected_rows) + 1L]] <- data.frame(
        family = family, selected_candidate_id = winner$candidate_id,
        worst_standardized_discrepancy =
          winner$worst_standardized_discrepancy,
        mean_standardized_discrepancy =
          winner$mean_standardized_discrepancy,
        mean_endpoint_rmse_over_oracle_width =
          winner$mean_endpoint_rmse_over_oracle_width,
        near_boundary_review_required =
          winner$worst_standardized_discrepancy > 1,
        stringsAsFactors = FALSE
      )
    }
  }
  ranking <- do.call(rbind, ranking_rows)
  ranking$rank <- NA_integer_
  for (family in unique(ranking$family)) {
    indices <- which(ranking$family == family)
    ordered <- order(
      !ranking$eligible[indices],
      ranking$worst_standardized_discrepancy[indices],
      ranking$mean_standardized_discrepancy[indices],
      ranking$mean_endpoint_rmse_over_oracle_width[indices],
      ranking$candidate_id[indices]
    )
    ranking$rank[indices[ordered]] <- seq_along(indices)
  }
  ranking <- ranking[order(ranking$family, ranking$rank), , drop = FALSE]
  selected <- if (length(selected_rows)) do.call(rbind, selected_rows) else
    data.frame()
  list(
    schema_version = otv4_selection_schema(),
    cell_audit = cell_audit, score_components = components,
    family_ranking = ranking, selected = selected,
    complete = nrow(selected) == 2L,
    target_specific_selection_prohibited = TRUE,
    realized_content_used = FALSE, aesthetic_judgment_used = FALSE
  )
}

otv4_validate_worker_artifact <- function(envelope, expected_contract) {
  expected_digest <- otf_object_sha256(expected_contract)
  prediction_valid <- is.list(envelope) && is.list(envelope$result) &&
    tryCatch({
      otv3_endpoint_only_prediction(envelope$result$pred)
      TRUE
    }, error = function(error) FALSE)
  provenance_valid <- is.list(envelope) && is.list(envelope$result) &&
    tryCatch({
      otv3_validate_provenance_audit(
        envelope$result$provenance_audit,
        expected_contract$family, expected_contract$target,
        expected_contract$chain
      )
      TRUE
    }, error = function(error) FALSE)
  valid <- is.list(envelope) &&
    identical(envelope$schema_version, otv4_worker_schema()) &&
    identical(envelope$contract_digest, expected_digest) &&
    identical(envelope$contract, expected_contract) &&
    is.list(envelope$result) && prediction_valid && provenance_valid &&
    is.data.frame(envelope$result$chain_summary) &&
    nrow(envelope$result$chain_summary) == 1L &&
    otv3_prediction_storage_contract(envelope$result$pred)
  if (!valid) oti_stop("The V4 worker artifact is invalid.")
  invisible(TRUE)
}

otv4_dgp_envelope <- function(preflight, family, source_commit,
                              config_sha256) {
  if (!family %in% c("fixed_design", "dlm")) {
    oti_stop("Unknown V4 DGP family.")
  }
  dgp <- if (family == "fixed_design") preflight$fixed_dgp else
    preflight$dlm_dgp
  targets <- if (family == "fixed_design") preflight$fixed_targets else
    preflight$dlm_targets
  initializations <- if (family == "fixed_design") {
    lapply(preflight$fixed_initializations, `[[`, "profiles")
  } else NULL
  contract <- list(
    schema_version = "rqrgibbs_oracle_tilt_v4_dgp/1.0.0",
    source_commit = source_commit, config_sha256 = config_sha256,
    candidate_id = preflight$candidate_id, family = family,
    dgp_digest = otf_object_sha256(dgp),
    target_digest = otf_object_sha256(targets),
    initialization_digest = if (is.null(initializations)) NA_character_ else
      otf_object_sha256(initializations),
    target_shared_data = TRUE
  )
  list(
    schema_version = contract$schema_version, contract = contract,
    contract_digest = otf_object_sha256(contract),
    dgp = dgp, targets = targets, initializations = initializations
  )
}

otv4_validate_dgp_envelope <- function(envelope, expected_candidate,
                                       expected_family, source_commit,
                                       config_sha256) {
  valid <- is.list(envelope) && is.list(envelope$contract) &&
    identical(envelope$schema_version,
              "rqrgibbs_oracle_tilt_v4_dgp/1.0.0") &&
    identical(envelope$contract$schema_version, envelope$schema_version) &&
    identical(envelope$contract$candidate_id, expected_candidate) &&
    identical(envelope$contract$family, expected_family) &&
    identical(envelope$contract$source_commit, source_commit) &&
    identical(envelope$contract$config_sha256, config_sha256) &&
    identical(envelope$contract_digest,
              otf_object_sha256(envelope$contract)) &&
    identical(envelope$contract$dgp_digest,
              otf_object_sha256(envelope$dgp)) &&
    identical(envelope$contract$target_digest,
              otf_object_sha256(envelope$targets)) &&
    isTRUE(envelope$contract$target_shared_data)
  if (identical(expected_family, "fixed_design")) {
    valid <- valid && is.list(envelope$initializations) &&
      identical(envelope$contract$initialization_digest,
                otf_object_sha256(envelope$initializations))
  } else {
    valid <- valid && is.null(envelope$initializations)
  }
  if (!valid) oti_stop("The canonical V4 DGP envelope is invalid.")
  invisible(TRUE)
}

otv4_cell_key <- function(candidate_id, family, target) {
  paste(candidate_id, family, tolower(target), sep = "_")
}

otv4_compact_files <- function() {
  c(
    "config.json", "source_state.json", "runtime_binding.json",
    "candidate_seed_manifest.csv", "dgp_manifest.csv", "fit_plan.csv",
    "cell_plan.csv", "candidate_preflight_gates.csv",
    "cross_candidate_gates.csv", "design_contract.csv",
    "oracle_targets.csv", "tail_information.csv",
    "static_projection_audit.csv", "dynamic_projection_audit.csv",
    "static_initialization_audit.csv", "scale_information.csv",
    "reference_gates.csv", "input_bundle_binding.csv",
    "benchmark_summary.csv", "run_status.csv", "fit_summary.csv",
    "fit_curves.csv", "endpoint_error_density.csv",
    "endpoint_error_summary.csv", "endpoint_error_by_index.csv",
    "chain_summary.csv", "provenance_audit.csv", "mcmc_diagnostics.csv",
    "conditional_parity.csv", "pathology_summary.csv",
    "recovery_summary.csv", "heterogeneity_summary.csv",
    "cell_manifest.csv", "worker_manifest.csv", "cell_audit.csv",
    "selection_score_components.csv", "family_ranking.csv",
    "selected_candidates.csv", "failure_log.csv", "closeout.json"
  )
}

# Verify the monitored wrapper's complete file inventory independently of the
# runner-owned compact artifact manifest.  The wrapper manifest intentionally
# hashes every closed-bundle file except itself, including the runner manifest,
# resource telemetry, logs, and wrapper closeout.  Requiring an exact two-way
# file-set match prevents a stale manifest, an unrecorded extra file, or a
# post-closeout mutation from being accepted as promotion evidence.
otv4_verify_wrapper_manifest <- function(root) {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  manifest_path <- file.path(root, "wrapper_artifact_manifest.csv")
  if (!file.exists(manifest_path)) {
    oti_stop("The V4 bundle lacks wrapper_artifact_manifest.csv.")
  }
  manifest <- utils::read.csv(
    manifest_path, stringsAsFactors = FALSE, check.names = FALSE
  )
  required <- c("sha256", "bytes", "path")
  if (!identical(names(manifest), required) || !nrow(manifest) ||
      anyNA(manifest) || anyDuplicated(manifest$path) ||
      any(!grepl("^[0-9a-f]{64}$", manifest$sha256)) ||
      any(!is.finite(manifest$bytes)) || any(manifest$bytes < 0) ||
      any(manifest$bytes != floor(manifest$bytes)) ||
      any(!nzchar(manifest$path)) ||
      any(startsWith(manifest$path, "/")) ||
      any(grepl("(^|/)\\.\\.(/|$)", manifest$path))) {
    oti_stop("The V4 wrapper artifact manifest is malformed.")
  }
  files <- list.files(
    root, recursive = TRUE, full.names = TRUE, all.files = TRUE,
    no.. = TRUE, include.dirs = FALSE
  )
  relative <- substring(files, nchar(root) + 2L)
  keep <- relative != "wrapper_artifact_manifest.csv"
  files <- files[keep]
  relative <- relative[keep]
  if (!setequal(relative, manifest$path)) {
    oti_stop("The V4 wrapper manifest file inventory is incomplete.")
  }
  index <- match(manifest$path, relative)
  actual_bytes <- unname(file.info(files[index])$size)
  actual_sha256 <- unname(vapply(
    files[index], oti_file_sha256, character(1L)
  ))
  if (anyNA(actual_bytes) ||
      !identical(as.numeric(manifest$bytes), as.numeric(actual_bytes)) ||
      !identical(as.character(manifest$sha256), actual_sha256)) {
    oti_stop("The V4 wrapper artifact manifest failed content verification.")
  }
  invisible(TRUE)
}
