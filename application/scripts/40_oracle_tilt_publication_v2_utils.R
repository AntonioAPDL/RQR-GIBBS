otv2_schema <- function() "rqrgibbs_oracle_tilt_publication/2.0.0"

otv2_config_schema <- function() {
  "rqrgibbs_oracle_tilt_publication_config/2.0.0"
}

otv2_preflight_schema <- function() {
  "rqrgibbs_oracle_tilt_publication_preflight/1.0.0"
}

otv2_close <- function(x, y, tolerance = 1e-12) {
  length(x) == 1L && length(y) == 1L && is.finite(x) && is.finite(y) &&
    abs(as.numeric(x) - as.numeric(y)) <= tolerance
}

otv2_required_logical <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    oti_stop(name, " must be one nonmissing logical value.")
  }
  x
}

otv2_numeric_vector <- function(x, name, lower = -Inf, upper = Inf,
                                minimum_length = 1L) {
  value <- as.numeric(unlist(x))
  if (length(value) < minimum_length || any(!is.finite(value)) ||
      any(value < lower) || any(value > upper)) {
    oti_stop(name, " must contain finite values in the declared range.")
  }
  value
}

otv2_integer_scalar <- function(x, name, lower = 0L) {
  value <- as.numeric(x)
  if (length(value) != 1L || !is.finite(value) || value != floor(value) ||
      value < lower || value > .Machine$integer.max) {
    oti_stop(name, " must be one finite integer in the declared range.")
  }
  as.integer(value)
}

otv2_validate_config <- function(config) {
  if (!identical(as.character(config$schema_version), otv2_config_schema())) {
    oti_stop("Unsupported oracle-tilt publication v2 configuration schema.")
  }
  otv2_required_logical(config$execution_authorized, "execution_authorized")
  if (!otv2_close(config$coverage_level, 0.95) ||
      !otv2_close(config$learning_rate, 1)) {
    oti_stop("The v2 illustration requires content 0.95 and learning rate 1.")
  }
  targets <- oti_normalize_targets(config$targets)
  if (!identical(targets, c("RQR", "ET", "SH")) ||
      !identical(as.character(config$tilt_source),
                 "exact_population_oracle")) {
    oti_stop("The ordered exact-oracle RQR/ET/SH target contract changed.")
  }
  law <- config$innovation %||% list()
  if (!identical(as.character(law$family), "asymmetric_laplace") ||
      !otv2_close(law$tau, 0.8) || !otv2_close(law$location, 0) ||
      !otv2_close(law$scale, 1) || !isTRUE(law$standardized)) {
    oti_stop("The v2 source law must be standardized AL_0.80(0,1).")
  }
  fixed <- config$fixed_design %||% list()
  if (otv2_integer_scalar(fixed$n, "fixed_design$n", 20L) != 1200L ||
      !identical(as.character(fixed$basis),
                 "empirical_orthogonal_quadratic") ||
      otv2_integer_scalar(
        fixed$n_chains, "fixed_design$n_chains", 2L
      ) != 4L ||
      otv2_integer_scalar(
        fixed$workers, "fixed_design$workers", 1L
      ) != 2L) {
    oti_stop("The fixed-design v2 contract changed.")
  }
  fixed_control <- fixed$mcmc_control %||% list()
  if (otv2_integer_scalar(fixed_control$n_burn,
                          "fixed_design$mcmc_control$n_burn", 1L) != 1500L ||
      otv2_integer_scalar(fixed_control$n_mcmc,
                          "fixed_design$mcmc_control$n_mcmc", 1L) != 6000L ||
      otv2_integer_scalar(fixed_control$thin,
                          "fixed_design$mcmc_control$thin", 1L) != 1L ||
      !identical(fixed_control$store_latent_draws, FALSE)) {
    oti_stop("The fixed-design MCMC contract changed.")
  }
  ridge <- fixed$ridge_prior_selection %||% list()
  ridge_candidates <- otv2_numeric_vector(
    ridge$candidate_tau2, "fixed-design ridge candidates",
    lower = .Machine$double.eps
  )
  if (anyDuplicated(ridge_candidates) || is.unsorted(ridge_candidates) ||
      !identical(as.character(ridge$selection_rule), "largest_passing") ||
      !otv2_close(ridge$expected_tau2, 1)) {
    oti_stop("The fixed-design prior-selection contract changed.")
  }
  dlm <- config$dlm %||% list()
  if (otv2_integer_scalar(dlm$T, "dlm$T", 20L) != 1200L ||
      !identical(as.character(dlm$time_grid), "i_over_T") ||
      otv2_integer_scalar(dlm$n_chains, "dlm$n_chains", 2L) != 5L ||
      otv2_integer_scalar(dlm$workers, "dlm$workers", 1L) != 2L ||
      otv2_integer_scalar(
        dlm$expected_missing, "dlm$expected_missing", 0L
      ) != 22L ||
      otv2_integer_scalar(
        dlm$expected_observed, "dlm$expected_observed", 1L
      ) !=
        1178L) {
    oti_stop("The DLM v2 horizon or missingness contract changed.")
  }
  if (!identical(
    otv2_numeric_vector(dlm$time_horizon, "dlm$time_horizon", 0, 1, 2L),
    c(0, 1)
  )) {
    oti_stop("The DLM physical horizon must remain [0,1].")
  }
  dlm_control <- dlm$mcmc_control %||% list()
  if (otv2_integer_scalar(dlm_control$n_burn,
                          "dlm$mcmc_control$n_burn", 1L) != 2500L ||
      otv2_integer_scalar(dlm_control$n_mcmc,
                          "dlm$mcmc_control$n_mcmc", 1L) != 6000L ||
      otv2_integer_scalar(dlm_control$thin,
                          "dlm$mcmc_control$thin", 1L) != 1L ||
      !identical(as.character(dlm_control$backend), "cpp") ||
      !identical(dlm_control$store_state_draws, TRUE) ||
      !identical(dlm_control$store_latent_draws, TRUE)) {
    oti_stop("The DLM MCMC/storage contract changed.")
  }
  profiles <- as.character(unlist(dlm$initial_profiles))
  if (!identical(
    profiles,
    c("default", "oracle_centered", "narrow", "wide", "slope_stress")
  )) {
    oti_stop("The five DLM initialization profiles changed.")
  }
  plan <- otv2_plan(config)
  if (nrow(plan) != 27L || length(unique(plan$seed)) != 27L ||
      any(plan$cornish_fisher_used)) {
    oti_stop("The 27-chain seed and target plan changed.")
  }
  diagnostics <- config$diagnostics %||% list()
  if (!otv2_close(diagnostics$rhat_max, 1.01) ||
      !otv2_close(diagnostics$bulk_ess_min, 1000) ||
      !otv2_close(diagnostics$tail_ess_min, 1000) ||
      !otv2_close(diagnostics$mcse_over_sd_max, 0.05)) {
    oti_stop("The maintained MCMC diagnostic contract changed.")
  }
  interpretation <- config$interpretation %||% list()
  required_false <- c(
    "response_likelihood", "response_predictive_draws",
    "cornish_fisher_used", "simulation_study"
  )
  if (any(vapply(required_false, function(name) {
    !identical(interpretation[[name]], FALSE)
  }, logical(1L))) || !isTRUE(interpretation$population_oracle_tilts)) {
    oti_stop("The v2 interpretation contract is incomplete.")
  }
  invisible(config)
}

otv2_al_skewness <- function(tau) {
  tau <- oti_scalar(tau, "tau", .Machine$double.eps,
                    1 - .Machine$double.eps)
  m1 <- (1 - tau) / tau - tau / (1 - tau)
  m2 <- 2 * ((1 - tau) / tau^2 + tau / (1 - tau)^2)
  m3 <- 6 * ((1 - tau) / tau^3 - tau / (1 - tau)^3)
  variance <- m2 - m1^2
  (m3 - 3 * m1 * m2 + 2 * m1^3) / variance^(3 / 2)
}

otv2_law <- function(config) {
  law <- oti_al_law(config$innovation$tau, standardized = TRUE)
  checks <- c(
    raw_mean = abs(law$raw_mean - config$innovation$expected_raw_mean),
    raw_sd = abs(law$raw_sd - config$innovation$expected_raw_sd),
    skewness = abs(
      otv2_al_skewness(config$innovation$tau) -
        config$innovation$expected_standardized_skewness
    )
  )
  if (any(!is.finite(checks)) || max(checks) > 1e-12) {
    oti_stop("The AL_0.80 standardization receipt does not reproduce.")
  }
  law
}

otv2_empirical_quadratic_basis <- function(x) {
  x <- as.numeric(x)
  if (length(x) < 3L || any(!is.finite(x)) || length(unique(x)) < 3L) {
    oti_stop("The empirical quadratic basis requires finite distinct x values.")
  }
  raw <- cbind(`(Intercept)` = 1, x = x, x2 = x^2)
  qr_raw <- qr(raw, LAPACK = FALSE)
  if (qr_raw$rank != 3L) oti_stop("The raw quadratic design is rank deficient.")
  Q <- qr.Q(qr_raw, complete = FALSE)
  for (column in seq_len(3L)) {
    if (sum(Q[, column] * raw[, column]) < 0) Q[, column] <- -Q[, column]
  }
  X <- Q * sqrt(length(x))
  colnames(X) <- c("basis_0", "basis_1", "basis_2")
  transform <- solve(crossprod(raw), crossprod(raw, X))
  reconstructed <- raw %*% transform
  list(
    X = X,
    raw = raw,
    transform = transform,
    rank = qr(X)$rank,
    gram = crossprod(X) / nrow(X),
    maximum_reconstruction_error = max(abs(reconstructed - X)),
    row_norm = sqrt(rowSums(X^2))
  )
}

otv2_static_prior_audit <- function(config, basis) {
  selection <- config$fixed_design$ridge_prior_selection
  candidates <- as.numeric(unlist(selection$candidate_tau2))
  probability <- oti_scalar(selection$probability, "prior probability", .5, 1)
  critical <- stats::qnorm(0.5 + probability / 2)
  center_index <- which.min(abs(seq(-1, 1, length.out = nrow(basis$X))))
  center_norm <- basis$row_norm[center_index]
  maximum_norm <- max(basis$row_norm)
  out <- data.frame(
    tau2 = candidates,
    center_row_norm = center_norm,
    maximum_row_norm = maximum_norm,
    center_half_width = critical * sqrt(candidates) * center_norm,
    maximum_half_width = critical * sqrt(candidates) * maximum_norm,
    stringsAsFactors = FALSE
  )
  out$pass <- with(
    out,
    center_half_width >= selection$center_half_width_min &
      maximum_half_width <= selection$maximum_half_width_max
  )
  passing <- out[out$pass, , drop = FALSE]
  if (!nrow(passing)) oti_stop("No fixed-design ridge candidate passes.")
  selected <- max(passing$tau2)
  out$selected <- out$tau2 == selected
  if (sum(out$selected) != 1L ||
      !otv2_close(selected, selection$expected_tau2)) {
    oti_stop("The fixed-design ridge selection is not unique or expected.")
  }
  list(table = out, selected_tau2 = selected)
}

otv2_time_grid <- function(config) {
  T <- as.integer(config$dlm$T)
  time <- seq_len(T) / T
  delta <- 1 / T
  windows <- matrix(
    as.numeric(unlist(config$dlm$missing_windows)), ncol = 2L, byrow = TRUE
  )
  if (!all(dim(windows) == c(2L, 2L)) || any(!is.finite(windows)) ||
      any(windows[, 1L] >= windows[, 2L])) {
    oti_stop("dlm$missing_windows must contain two ordered finite windows.")
  }
  missing <- rep(FALSE, T)
  for (row in seq_len(nrow(windows))) {
    missing <- missing |
      (time >= windows[row, 1L] & time <= windows[row, 2L])
  }
  if (sum(missing) != as.integer(config$dlm$expected_missing) ||
      sum(!missing) != as.integer(config$dlm$expected_observed)) {
    oti_stop("The normalized-time missing mask changed.")
  }
  list(time = time, delta = delta, missing = missing, windows = windows)
}

otv2_local_linear_matrices <- function(delta, q_level, q_slope) {
  delta <- oti_scalar(delta, "delta", .Machine$double.eps)
  q_level <- oti_scalar(q_level, "q_level", .Machine$double.eps)
  q_slope <- oti_scalar(q_slope, "q_slope", .Machine$double.eps)
  G <- matrix(c(1, 0, delta, 1), 2L, 2L)
  W <- matrix(c(
    q_level * delta + q_slope * delta^3 / 3,
    q_slope * delta^2 / 2,
    q_slope * delta^2 / 2,
    q_slope * delta
  ), 2L, 2L)
  list(G = G, W = W)
}

otv2_dlm_prior_audit <- function(config) {
  selection <- config$dlm$evolution_prior_selection
  grid <- expand.grid(
    initial_level_variance = as.numeric(unlist(
      selection$candidate_initial_level_variance
    )),
    initial_slope_variance = as.numeric(unlist(
      selection$candidate_initial_slope_variance
    )),
    q_level = as.numeric(unlist(selection$candidate_q_level)),
    q_slope = as.numeric(unlist(selection$candidate_q_slope)),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  critical <- stats::qnorm(0.5 + selection$probability / 2)
  grid$initial_level_half_width <- critical *
    sqrt(grid$initial_level_variance)
  grid$terminal_level_variance <- with(
    grid,
    initial_level_variance + initial_slope_variance + q_level + q_slope / 3
  )
  grid$terminal_level_half_width <- critical *
    sqrt(grid$terminal_level_variance)
  grid$terminal_root_difference_sd <- sqrt(
    2 * grid$terminal_level_variance
  )
  grid$terminal_slope_half_width <- critical * sqrt(
    grid$initial_slope_variance + grid$q_slope
  )
  grid$pass <- with(
    grid,
    initial_level_half_width >= selection$initial_level_half_width_min &
      initial_level_half_width <= selection$initial_level_half_width_max &
      terminal_level_half_width <= selection$terminal_level_half_width_max &
      terminal_root_difference_sd <=
        selection$terminal_root_difference_sd_max &
      terminal_slope_half_width >= selection$terminal_slope_half_width_min &
      terminal_slope_half_width <= selection$terminal_slope_half_width_max
  )
  passing <- grid[grid$pass, , drop = FALSE]
  if (!nrow(passing)) oti_stop("No DLM prior/evolution candidate passes.")
  expected <- c(
    initial_level_variance = selection$expected_initial_level_variance,
    initial_slope_variance = selection$expected_initial_slope_variance,
    q_level = selection$expected_q_level,
    q_slope = selection$expected_q_slope
  )
  selected <- Reduce(`&`, Map(
    function(name, value) abs(grid[[name]] - value) <= 1e-12,
    names(expected), as.numeric(expected)
  ))
  grid$selected <- selected
  if (sum(selected) != 1L || !grid$pass[selected]) {
    oti_stop("The expected DLM prior/evolution candidate does not pass.")
  }
  for (name in names(expected)) {
    if (max(passing[[name]]) != expected[[name]]) {
      oti_stop("The DLM selection is not componentwise-largest passing.")
    }
  }
  list(table = grid, selected = as.list(expected))
}

otv2_expand_cube <- function(matrix, n_time) {
  array(rep(as.matrix(matrix), n_time), c(nrow(matrix), ncol(matrix), n_time))
}

otv2_fixed_horizon_audit <- function(C0, q_level, q_slope,
                                     grids = c(100L, 1200L)) {
  propagate <- function(T) {
    matrices <- otv2_local_linear_matrices(1 / T, q_level, q_slope)
    covariance <- as.matrix(C0)
    for (index in seq_len(T)) {
      covariance <- matrices$G %*% covariance %*% t(matrices$G) + matrices$W
    }
    covariance
  }
  reference <- matrix(c(
    C0[1, 1] + C0[2, 2] + q_level + q_slope / 3,
    C0[2, 2] + q_slope / 2,
    C0[2, 2] + q_slope / 2,
    C0[2, 2] + q_slope
  ), 2L, 2L)
  rows <- lapply(as.integer(grids), function(T) {
    got <- propagate(T)
    data.frame(
      T = T,
      max_absolute_error = max(abs(got - reference)),
      level_variance = got[1L, 1L],
      level_slope_covariance = got[1L, 2L],
      slope_variance = got[2L, 2L],
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

otv2_fixed_design_dgp <- function(config, law) {
  set.seed(as.integer(config$fixed_design$seed))
  n <- as.integer(config$fixed_design$n)
  x <- seq(-1, 1, length.out = n)
  basis <- otv2_empirical_quadratic_basis(x)
  prior <- otv2_static_prior_audit(config, basis)
  mu <- 0.35 + 0.85 * x - 0.30 * x^2
  scale <- 0.52 + 0.15 * (x + 1) / 2
  y <- mu + scale * law$r(n)
  list(
    family = "fixed_design", seed = as.integer(config$fixed_design$seed),
    x = x, X = basis$X, y = y, mean_truth = mu, scale_truth = scale,
    observed = rep(TRUE, n), basis = basis, prior_audit = prior,
    ridge_tau2 = prior$selected_tau2
  )
}

otv2_dlm_dgp <- function(config, law) {
  time_contract <- otv2_time_grid(config)
  prior <- otv2_dlm_prior_audit(config)
  selected <- prior$selected
  matrices <- otv2_local_linear_matrices(
    time_contract$delta, selected$q_level, selected$q_slope
  )
  T <- as.integer(config$dlm$T)
  GG <- otv2_expand_cube(matrices$G, T)
  W <- otv2_expand_cube(matrices$W, T)
  C0 <- diag(c(
    selected$initial_level_variance,
    selected$initial_slope_variance
  ))
  model <- rqrgibbs::rqr_as_dlm_model(list(
    FF = matrix(c(1, 0), 2L, 1L), GG = GG, m0 = c(0, 0), C0 = C0,
    component_dims = 2L, component_names = "local_linear"
  ))
  set.seed(as.integer(config$dlm$seed))
  theta <- matrix(NA_real_, 2L, T)
  previous <- as.numeric(unlist(config$dlm$initial_state))
  if (length(previous) != 2L || any(!is.finite(previous))) {
    oti_stop("dlm$initial_state must contain finite level and slope values.")
  }
  factor <- t(chol(matrices$W))
  for (index in seq_len(T)) {
    previous <- drop(matrices$G %*% previous + factor %*% stats::rnorm(2L))
    theta[, index] <- previous
  }
  scale <- rep(oti_scalar(config$dlm$scale, "dlm$scale", 1e-8), T)
  y_full <- theta[1L, ] + scale * law$r(T)
  y <- y_full
  y[time_contract$missing] <- NA_real_
  list(
    family = "dlm", seed = as.integer(config$dlm$seed),
    time = time_contract$time, delta = time_contract$delta,
    y = y, y_full = y_full, model = model, W = W,
    mean_truth = theta[1L, ], state_truth = t(theta), scale_truth = scale,
    initial_level_variance = selected$initial_level_variance,
    initial_slope_variance = selected$initial_slope_variance,
    q_level = selected$q_level, q_slope = selected$q_slope,
    observed = !time_contract$missing,
    missing_times = which(time_contract$missing),
    missing_windows = time_contract$windows,
    prior_audit = prior,
    fixed_horizon_audit = otv2_fixed_horizon_audit(
      C0, selected$q_level, selected$q_slope
    )
  )
}

otv2_tail_information <- function(config, oracle, n_observed_dlm) {
  lower <- oracle$u
  upper <- 1 - config$coverage_level - oracle$u
  rare <- pmin(lower, upper)
  data.frame(
    target = oracle$target,
    lower_tail_probability = lower,
    upper_tail_probability = upper,
    rare_tail_probability = rare,
    fixed_design_n = as.integer(config$fixed_design$n),
    fixed_design_expected_rare_count =
      as.integer(config$fixed_design$n) * rare,
    dlm_n_observed = as.integer(n_observed_dlm),
    dlm_expected_rare_count = as.integer(n_observed_dlm) * rare,
    stringsAsFactors = FALSE
  )
}

otv2_projection_audit <- function(dgp, targets) {
  rows <- lapply(c("RQR", "ET", "SH"), function(target) {
    truth <- oti_target_row(targets, target)
    do.call(rbind, lapply(c("lower", "upper"), function(endpoint) {
      value <- truth[[paste0("oracle_", endpoint)]]
      coefficient <- qr.solve(dgp$X, value)
      residual <- value - drop(dgp$X %*% coefficient)
      data.frame(
        target = target, endpoint = endpoint,
        max_absolute_residual = max(abs(residual)),
        root_mean_square_residual = sqrt(mean(residual^2)),
        stringsAsFactors = FALSE
      )
    }))
  })
  do.call(rbind, rows)
}

otv2_plan <- function(config) {
  rows <- list()
  for (target in c("RQR", "ET", "SH")) {
    rows[[length(rows) + 1L]] <- data.frame(
      family = "fixed_design", target = target,
      chain = seq_len(as.integer(config$fixed_design$n_chains)),
      profile = "seed_only",
      n_burn = as.integer(config$fixed_design$mcmc_control$n_burn),
      n_mcmc = as.integer(config$fixed_design$mcmc_control$n_mcmc),
      stringsAsFactors = FALSE
    )
    profiles <- as.character(unlist(config$dlm$initial_profiles))
    rows[[length(rows) + 1L]] <- data.frame(
      family = "dlm", target = target,
      chain = seq_along(profiles), profile = profiles,
      n_burn = as.integer(config$dlm$mcmc_control$n_burn),
      n_mcmc = as.integer(config$dlm$mcmc_control$n_mcmc),
      stringsAsFactors = FALSE
    )
  }
  out <- do.call(rbind, rows)
  out$cell <- paste(out$family, out$target, sep = "/")
  out$learning_rate_mode <- "fixed_rate"
  out$tilt_source <- "exact_population_oracle"
  out$cornish_fisher_used <- FALSE
  out$seed <- mapply(
    function(family, target, chain) otp_seed(config, family, target, chain),
    out$family, out$target, out$chain
  )
  rownames(out) <- NULL
  out
}

otv2_design_preflight <- function(config) {
  otv2_validate_config(config)
  law <- otv2_law(config)
  oracle <- oti_oracle_targets(law, config$coverage_level, config$targets)
  fixed <- otv2_fixed_design_dgp(config, law)
  dlm <- otv2_dlm_dgp(config, law)
  fixed_targets <- oti_targets_by_index(
    fixed$mean_truth, fixed$scale_truth, oracle, fixed$observed
  )
  dlm_targets <- oti_targets_by_index(
    dlm$mean_truth, dlm$scale_truth, oracle, dlm$observed
  )
  tail <- otv2_tail_information(config, oracle, sum(dlm$observed))
  projection <- otv2_projection_audit(fixed, fixed_targets)
  gram_error <- max(abs(fixed$basis$gram - diag(3L)))
  horizon_error <- max(dlm$fixed_horizon_audit$max_absolute_error)
  gates <- data.frame(
    gate = c(
      "expected_rare_tail_count", "static_design_rank",
      "static_basis_gram", "static_truth_projection",
      "fixed_horizon_covariance", "missing_mask", "fit_plan_size",
      "cornish_fisher_absent"
    ),
    value = c(
      min(c(tail$fixed_design_expected_rare_count,
            tail$dlm_expected_rare_count)),
      fixed$basis$rank, gram_error,
      max(projection$max_absolute_residual), horizon_error,
      sum(!dlm$observed), nrow(otv2_plan(config)), 0
    ),
    threshold = c(
      config$preflight_gates$minimum_expected_rare_tail_count,
      config$preflight_gates$static_design_rank,
      config$preflight_gates$static_gram_max_abs_error,
      config$preflight_gates$static_projection_max_abs,
      config$preflight_gates$fixed_horizon_covariance_max_abs_error,
      config$dlm$expected_missing, 27, 0
    ),
    comparison = c(">=", "==", "<=", "<=", "<=", "==", "==", "=="),
    stringsAsFactors = FALSE
  )
  gates$pass <- mapply(function(value, threshold, comparison) {
    switch(comparison,
      ">=" = value >= threshold,
      "<=" = value <= threshold,
      "==" = abs(value - threshold) <= 1e-12,
      FALSE
    )
  }, gates$value, gates$threshold, gates$comparison)
  list(
    law = law, oracle = oracle, fixed_dgp = fixed, dlm_dgp = dlm,
    fixed_targets = fixed_targets, dlm_targets = dlm_targets,
    tail_information = tail, projection_audit = projection,
    static_prior_audit = fixed$prior_audit$table,
    dlm_prior_audit = dlm$prior_audit$table,
    fixed_horizon_audit = dlm$fixed_horizon_audit,
    plan = otv2_plan(config), gates = gates,
    pass = all(gates$pass)
  )
}

otv2_reference_suite <- function(config) {
  set.seed(202607313L)
  p <- 3L
  n <- 18L
  X <- matrix(stats::rnorm(n * p), n, p)
  y <- stats::rnorm(n)
  beta_other <- c(-0.2, 0.3, 0.1)
  latent <- stats::runif(n, 0.4, 1.2)
  delta_tilt <- seq(-0.08, 0.08, length.out = n)
  constants <- rqrgibbs::rqr_constants(0.95, 1)
  prior_precision <- rep(1, p)
  eta_other <- drop(X %*% beta_other)
  A <- X * as.numeric(y - eta_other)
  z <- y^2 - y * eta_other - constants$xi * latent
  weight <- 1 / (constants$phi * constants$sigma * latent)
  precision <- crossprod(A * sqrt(weight)) + diag(prior_precision, p)
  rhs <- crossprod(A, weight * z) +
    constants$omega * constants$alpha * crossprod(X, delta_tilt)
  expected_mean <- solve(precision, rhs)
  expected_covariance <- solve(precision)
  draws <- replicate(5000L, rqrgibbs:::.rqr_beta_update(
    y = y, X = X, beta_other = beta_other, V = latent,
    constants = constants, prior_prec = prior_precision,
    mean_tilt_observed = delta_tilt
  )$draw)
  sampled_mean <- rowMeans(draws)
  sampled_covariance <- stats::cov(t(draws))
  mean_z <- max(abs(sampled_mean - expected_mean) /
    sqrt(diag(expected_covariance) / ncol(draws)))
  covariance_pairs <- which(upper.tri(expected_covariance, diag = TRUE),
                            arr.ind = TRUE)
  covariance_z <- apply(covariance_pairs, 1L, function(pair) {
    i <- pair[1L]
    j <- pair[2L]
    mcse <- sqrt((
      expected_covariance[i, i] * expected_covariance[j, j] +
        expected_covariance[i, j]^2
    ) / (ncol(draws) - 1L))
    abs(sampled_covariance[i, j] - expected_covariance[i, j]) / mcse
  })

  T <- 12L
  selected <- otv2_dlm_prior_audit(config)$selected
  matrices <- otv2_local_linear_matrices(
    1 / T, selected$q_level, selected$q_slope
  )
  GG <- otv2_expand_cube(matrices$G, T)
  W <- otv2_expand_cube(matrices$W, T)
  C0 <- diag(c(
    selected$initial_level_variance,
    selected$initial_slope_variance
  ))
  H <- rbind(rep(1, T), seq(-0.2, 0.2, length.out = T))
  response <- stats::rnorm(T)
  response[c(4L, 9L)] <- NA_real_
  variance <- stats::runif(T, 0.4, 1.1)
  shift <- rbind(
    seq(-0.03, 0.03, length.out = T),
    seq(0.01, -0.01, length.out = T)
  )
  shift[, is.na(response)] <- 0
  expanded <- list(
    p = 2L, n_time = T, GG = GG, W = W, C0 = C0,
    m0 = matrix(c(0, 0), 2L, 1L), FF = H
  )
  dense <- otf_dense_gaussian_reference(
    response, H, variance, expanded, shift
  )
  evolution <- rqrgibbs::rqr_evolution_fixed(W)
  smooth_R <- rqrgibbs::rqr_ffbs_smooth(
    response, H, variance, GG, expanded$m0, C0, evolution,
    backend = "R", numerical_policy = "fail", canonical_shift = shift
  )
  smooth_cpp <- rqrgibbs::rqr_ffbs_smooth(
    response, H, variance, GG, expanded$m0, C0, evolution,
    backend = "cpp", numerical_policy = "fail", canonical_shift = shift
  )
  dense_blocks <- array(NA_real_, c(2L, 2L, T))
  for (index in seq_len(T)) {
    rows <- ((index - 1L) * 2L + 1L):(index * 2L)
    dense_blocks[, , index] <- dense$covariance[rows, rows]
  }
  set.seed(202607314L)
  paths <- replicate(3000L, as.vector(rqrgibbs::rqr_ffbs_sample(
    response, H, variance, GG, expanded$m0, C0, evolution,
    backend = "cpp", numerical_policy = "fail", canonical_shift = shift
  )$path))
  sampled_path_covariance <- stats::cov(t(paths))
  pairs <- rbind(c(1L, 7L), c(2L, 18L), c(5L, 24L))
  path_covariance_z <- apply(pairs, 1L, function(pair) {
    i <- pair[1L]
    j <- pair[2L]
    mcse <- sqrt((
      dense$covariance[i, i] * dense$covariance[j, j] +
        dense$covariance[i, j]^2
    ) / (ncol(paths) - 1L))
    abs(sampled_path_covariance[i, j] - dense$covariance[i, j]) / mcse
  })
  rows <- data.frame(
    gate = c(
      "static_conditional_sampled_mean_z",
      "static_conditional_sampled_covariance_z",
      "dlm_R_dense_mean_absolute_error",
      "dlm_cpp_dense_mean_absolute_error",
      "dlm_R_cpp_mean_absolute_error",
      "dlm_R_dense_marginal_covariance_absolute_error",
      "dlm_cpp_dense_marginal_covariance_absolute_error",
      "dlm_R_cpp_marginal_covariance_absolute_error",
      "dlm_sampled_cross_time_covariance_z",
      "dlm_R_repair_count", "dlm_cpp_repair_count",
      "missing_measurement_omission"
    ),
    value = c(
      mean_z, max(covariance_z),
      max(abs(smooth_R$smooth_mean - dense$mean)),
      max(abs(smooth_cpp$smooth_mean - dense$mean)),
      max(abs(smooth_R$smooth_mean - smooth_cpp$smooth_mean)),
      max(abs(smooth_R$smooth_cov - dense_blocks)),
      max(abs(smooth_cpp$smooth_cov - dense_blocks)),
      max(abs(smooth_R$smooth_cov - smooth_cpp$smooth_cov)),
      max(path_covariance_z), smooth_R$diagnostics$repair_count,
      smooth_cpp$diagnostics$repair_count,
      as.numeric(all(is.na(smooth_R$residual[c(4L, 9L)])))
    ),
    threshold = c(5, 5, 1e-9, 1e-9, 1e-10, 1e-9, 1e-9, 1e-10,
                  5, 0, 0, 1),
    comparison = c(rep("<=", 11L), "=="),
    stringsAsFactors = FALSE
  )
  rows$pass <- mapply(function(value, threshold, comparison) {
    if (comparison == "<=") value <= threshold else
      abs(value - threshold) <= 1e-12
  }, rows$value, rows$threshold, rows$comparison)
  rows
}

otv2_initial_state_paths <- function(profile, dgp, truth) {
  profile <- match.arg(profile, c(
    "default", "oracle_centered", "narrow", "wide", "slope_stress"
  ))
  if (identical(profile, "default")) return(list())
  midpoint <- 0.5 * (truth$oracle_lower + truth$oracle_upper)
  half_width <- 0.5 * truth$oracle_width
  multiplier <- switch(
    profile, oracle_centered = 1, narrow = 0.25, wide = 2.5,
    slope_stress = 1
  )
  lower <- midpoint - multiplier * half_width
  upper <- midpoint + multiplier * half_width
  slope <- dgp$state_truth[, 2L]
  theta1 <- rbind(lower, slope)
  theta2 <- rbind(upper, slope)
  delta <- dgp$delta
  out <- list(
    state_root1 = theta1, state_root2 = theta2,
    theta0_root1 = c(lower[1L] - delta * slope[1L], slope[1L]),
    theta0_root2 = c(upper[1L] - delta * slope[1L], slope[1L])
  )
  if (identical(profile, "slope_stress")) {
    offset <- 2 * sqrt(dgp$initial_slope_variance)
    out$theta0_root2[2L] <- out$theta0_root2[2L] + offset
    out$state_root2[2L, ] <- out$state_root2[2L, ] + offset
    out$state_root2[1L, ] <- out$state_root2[1L, ] + offset * dgp$time
  }
  out
}

otv2_diagnostic_indices <- function(dgp) {
  n <- length(dgp$y)
  regular <- unique(as.integer(round(seq(1, n, length.out = 21L))))
  missing <- which(!dgp$observed)
  boundary <- integer(0)
  if (length(missing)) {
    starts <- missing[c(TRUE, diff(missing) > 1L)]
    ends <- missing[c(diff(missing) > 1L, TRUE)]
    boundary <- unique(c(starts - 1L, starts, ends, ends + 1L))
  }
  sort(unique(c(regular, boundary[boundary >= 1L & boundary <= n])))
}

otv2_scalar_draw_matrix <- function(family, pred, truth, y, coverage_level,
                                    dgp) {
  lower <- as.matrix(pred$lower_draws)
  upper <- as.matrix(pred$upper_draws)
  observed <- as.logical(truth$observed) & is.finite(y)
  values <- data.frame(
    mean_lower_observed = colMeans(lower[observed, , drop = FALSE]),
    mean_upper_observed = colMeans(upper[observed, , drop = FALSE]),
    mean_width_observed = colMeans((upper - lower)[observed, , drop = FALSE]),
    mean_midpoint_observed = colMeans(
      0.5 * (lower + upper)[observed, , drop = FALSE]
    ),
    stringsAsFactors = FALSE
  )
  values$observed_mean_tilted_loss <- vapply(seq_len(ncol(lower)), function(j) {
    sum(rqrgibbs::rqr_mean_tilt_loss(
      y, lower[, j], upper[, j], coverage_level,
      mean_tilt = truth$mean_tilt
    ))
  }, numeric(1L))
  indices <- if (identical(family, "fixed_design")) {
    unique(as.integer(round(seq(1, length(y), length.out = 15L))))
  } else {
    otv2_diagnostic_indices(dgp)
  }
  for (index in indices) {
    suffix <- sprintf("%04d", index)
    values[[paste0("lower_", suffix)]] <- lower[index, ]
    values[[paste0("upper_", suffix)]] <- upper[index, ]
    values[[paste0("width_", suffix)]] <- upper[index, ] - lower[index, ]
    values[[paste0("midpoint_", suffix)]] <-
      0.5 * (upper[index, ] + lower[index, ])
  }
  as.matrix(values)
}

otv2_fixed_chain <- function(config, dgp, targets, target, chain,
                             provenance_control, mcmc_override = NULL) {
  truth <- oti_target_row(targets, target)
  control <- config$fixed_design$mcmc_control
  if (!is.null(mcmc_override)) control <- modifyList(control, mcmc_override)
  seed <- otp_seed(config, "fixed_design", target, chain)
  control$seed <- seed
  elapsed <- system.time(fit <- rqrgibbs::rqr_mcmc_fit(
    y = dgp$y, X = dgp$X, coverage_level = config$coverage_level,
    learning_rate = config$learning_rate,
    learning_rate_mode = "fixed_rate", mean_tilt = truth$mean_tilt,
    beta_prior_obj = oti_ridge_prior(dgp$ridge_tau2),
    numerical_policy = "fail", provenance_control = provenance_control,
    mcmc_control = control
  ))
  pred <- rqrgibbs::predict_interval(fit, X_new = dgp$X)
  list(
    pred = pred,
    scalar_draws = otv2_scalar_draw_matrix(
      "fixed_design", pred, truth, dgp$y, config$coverage_level, dgp
    ),
    chain_summary = cbind(
      oti_chain_summary("fixed_design", target, chain, seed, fit, elapsed),
      profile = "seed_only", otp_provenance_summary(fit)
    )
  )
}

otv2_conditional_parity <- function(fit, dgp, truth, coverage_level,
                                    draw = NULL) {
  expanded <- otf_expanded_dlm(dgp)
  observed <- is.finite(dgp$y)
  n_draw <- ncol(fit$samp.eta_root1)
  draw <- as.integer(draw %||% ceiling(n_draw / 2))
  constants <- rqrgibbs::rqr_constants(coverage_level, 1)
  latent_v <- fit$samp.latent_v[, draw]
  variance <- constants$phi * latent_v
  canonical <- matrix(0, expanded$p, expanded$n_time)
  canonical[, observed] <- sweep(
    expanded$FF[, observed, drop = FALSE], 2L,
    coverage_level * truth$mean_tilt[observed], `*`
  )
  evolution <- rqrgibbs::rqr_evolution_fixed(expanded$W)
  rows <- lapply(c("root1", "root2"), function(root) {
    other <- if (root == "root1") fit$samp.eta_root2[, draw] else
      fit$samp.eta_root1[, draw]
    H <- sweep(expanded$FF, 2L, dgp$y - other, `*`)
    H[, !observed] <- 0
    z <- dgp$y * (dgp$y - other) - constants$xi * latent_v
    z[!observed] <- NA_real_
    R <- rqrgibbs::rqr_ffbs_smooth(
      z, H, variance, expanded$GG, expanded$m0, expanded$C0, evolution,
      backend = "R", numerical_policy = "fail", canonical_shift = canonical
    )
    cpp <- rqrgibbs::rqr_ffbs_smooth(
      z, H, variance, expanded$GG, expanded$m0, expanded$C0, evolution,
      backend = "cpp", numerical_policy = "fail", canonical_shift = canonical
    )
    data.frame(
      root = root, draw = draw,
      maximum_mean_absolute_error = max(abs(R$smooth_mean - cpp$smooth_mean)),
      maximum_covariance_absolute_error =
        max(abs(R$smooth_cov - cpp$smooth_cov)),
      R_repair_count = R$diagnostics$repair_count,
      cpp_repair_count = cpp$diagnostics$repair_count,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

otv2_pathology_summary <- function(pred, dgp, truth, config) {
  response_sd <- stats::sd(dgp$y[is.finite(dgp$y)])
  oracle_width <- mean(truth$oracle_width[truth$observed])
  endpoint_scale <- pmax(
    apply(abs(pred$lower_draws), 2L, max),
    apply(abs(pred$upper_draws), 2L, max)
  ) / response_sd
  width_scale <- apply(pred$width_draws, 2L, max) / oracle_width
  remote <- endpoint_scale > config$diagnostics$remote_endpoint_sd_threshold |
    width_scale > config$diagnostics$remote_width_ratio_threshold
  data.frame(
    response_sd = response_sd,
    oracle_mean_width = oracle_width,
    maximum_endpoint_over_response_sd = max(endpoint_scale),
    median_maximum_width_over_oracle_width = stats::median(width_scale),
    maximum_width_over_oracle_width = max(width_scale),
    remote_draw_fraction = mean(remote),
    stringsAsFactors = FALSE
  )
}

otv2_dlm_chain <- function(config, dgp, targets, target, chain,
                           provenance_control, mcmc_override = NULL) {
  truth <- oti_target_row(targets, target)
  profiles <- as.character(unlist(config$dlm$initial_profiles))
  profile <- profiles[chain]
  seed <- otp_seed(config, "dlm", target, chain)
  control <- config$dlm$mcmc_control
  if (!is.null(mcmc_override)) control <- modifyList(control, mcmc_override)
  control$seed <- seed
  init <- otv2_initial_state_paths(profile, dgp, truth)
  elapsed <- system.time(fit <- rqrgibbs::rqr_dlm_fit(
    y = dgp$y, model = dgp$model, coverage_level = config$coverage_level,
    evolution_mode = "fixed_W", W = dgp$W,
    learning_rate = config$learning_rate,
    learning_rate_mode = "fixed_rate", mean_tilt = truth$mean_tilt,
    numerical_policy = "fail", provenance_control = provenance_control,
    mcmc_control = control, init = init
  ))
  pred <- rqrgibbs::predict_interval(fit)
  list(
    pred = pred,
    scalar_draws = otv2_scalar_draw_matrix(
      "dlm", pred, truth, dgp$y, config$coverage_level, dgp
    ),
    chain_summary = cbind(
      oti_chain_summary("dlm", target, chain, seed, fit, elapsed),
      profile = profile, otp_provenance_summary(fit)
    ),
    conditional_parity = otv2_conditional_parity(
      fit, dgp, truth, config$coverage_level
    ),
    pathology = otv2_pathology_summary(pred, dgp, truth, config)
  )
}

otv2_recovery_summary <- function(family, curves, metrics, config) {
  width <- metrics$oracle_mean_width
  joint_inclusion <- mean(
    curves$oracle_lower >= curves$fit_lower_q025 &
      curves$oracle_lower <= curves$fit_lower_q975 &
      curves$oracle_upper >= curves$fit_upper_q025 &
      curves$oracle_upper <= curves$fit_upper_q975
  )
  lower_bias <- mean(curves$fit_lower - curves$oracle_lower)
  upper_bias <- mean(curves$fit_upper - curves$oracle_upper)
  all_rmse <- sqrt(mean(
    (curves$fit_lower - curves$oracle_lower)^2 +
      (curves$fit_upper - curves$oracle_upper)^2
  ) / 2)
  edge_center_ratio <- NA_real_
  if (identical(family, "fixed_design")) {
    fraction <- config$recovery_gates$static_edge_fraction
    edge <- abs(curves$x) >= 1 - fraction
    center <- abs(curves$x) <= fraction
    point_error <- sqrt(0.5 * (
      (curves$fit_lower - curves$oracle_lower)^2 +
        (curves$fit_upper - curves$oracle_upper)^2
    ))
    edge_center_ratio <- sqrt(mean(point_error[edge]^2)) /
      sqrt(mean(point_error[center]^2))
  }
  data.frame(
    endpoint_rmse_all = all_rmse,
    endpoint_rmse_over_oracle_width = all_rmse / width,
    mean_width_ratio = metrics$mean_width / width,
    endpoint_summary_joint_inclusion = joint_inclusion,
    lower_bias_over_oracle_width = lower_bias / width,
    upper_bias_over_oracle_width = upper_bias / width,
    static_edge_center_rmse_ratio = edge_center_ratio,
    stringsAsFactors = FALSE
  )
}

otv2_summarize_cell <- function(family, target, chain_results, dgp, targets,
                                config) {
  truth <- oti_target_row(targets, target)
  pred <- oti_combine_predictions(lapply(chain_results, `[[`, "pred"))
  chains <- do.call(rbind, lapply(chain_results, `[[`, "chain_summary"))
  diagnostics <- oti_mcmc_diagnostics(
    family, target, lapply(chain_results, `[[`, "scalar_draws"),
    otp_diagnostic_contract(config)
  )
  x <- if (identical(family, "fixed_design")) dgp$x else dgp$time
  curves <- oti_curve_frame(family, target, x, dgp$y, pred, truth)
  metrics <- oti_interval_metrics(pred, truth, dgp$y)
  recovery <- otv2_recovery_summary(family, curves, metrics, config)
  parity <- if (identical(family, "dlm")) {
    do.call(rbind, lapply(chain_results, `[[`, "conditional_parity"))
  } else data.frame()
  pathology <- if (identical(family, "dlm")) {
    do.call(rbind, lapply(chain_results, `[[`, "pathology"))
  } else data.frame()
  provenance_pass <- all(
    chains$numerical_repair_count == 0L & chains$exact_joint_target &
      chains$target_numerical_eligible & chains$reproducibility_eligible &
      chains$promotion_eligible & chains$primary_runtime_source_match
  )
  diagnostics_pass <- nrow(diagnostics) > 0L && all(diagnostics$pass)
  parity_pass <- if (identical(family, "dlm")) {
    nrow(parity) > 0L &&
      all(parity$maximum_mean_absolute_error <=
            config$diagnostics$R_cpp_absolute_tolerance) &&
      all(parity$maximum_covariance_absolute_error <=
            config$diagnostics$R_cpp_absolute_tolerance) &&
      all(parity$R_repair_count == 0L & parity$cpp_repair_count == 0L)
  } else TRUE
  pathology_pass <- if (identical(family, "dlm")) {
    nrow(pathology) > 0L && all(
      pathology$remote_draw_fraction <=
        config$diagnostics$maximum_remote_draw_fraction
    )
  } else TRUE
  gates <- config$recovery_gates
  recovery_pass <- with(
    recovery,
    endpoint_rmse_over_oracle_width <=
      gates$endpoint_rmse_over_oracle_width_max &
      mean_width_ratio >= gates$mean_width_ratio_min &
      mean_width_ratio <= gates$mean_width_ratio_max &
      endpoint_summary_joint_inclusion >=
        gates$endpoint_summary_joint_inclusion_min &
      abs(lower_bias_over_oracle_width) <=
        gates$absolute_endpoint_bias_over_oracle_width_max &
      abs(upper_bias_over_oracle_width) <=
        gates$absolute_endpoint_bias_over_oracle_width_max &
      (identical(family, "dlm") || static_edge_center_rmse_ratio <=
         gates$static_edge_center_rmse_ratio_max)
  )
  computational_pass <- provenance_pass && diagnostics_pass && parity_pass &&
    pathology_pass
  disposition <- if (computational_pass && recovery_pass) "strict_pass" else
    "fail"
  fit_summary <- cbind(
    data.frame(
      family = family, target = target,
      provenance_pass = provenance_pass,
      strict_diagnostics_pass = diagnostics_pass,
      conditional_parity_pass = parity_pass,
      pathology_pass = pathology_pass,
      computational_pass = computational_pass,
      recovery_pass = recovery_pass,
      disposition = disposition,
      manuscript_illustration_evidence_eligible =
        computational_pass && recovery_pass,
      n_chains = nrow(chains), retained_draws = ncol(pred$lower_draws),
      numerical_repair_count = sum(chains$numerical_repair_count),
      stringsAsFactors = FALSE
    ),
    metrics, recovery
  )
  list(
    fit_summary = fit_summary, fit_curves = curves,
    endpoint_error_density = oti_endpoint_error_density_frame(
      family, target, pred, truth
    ),
    endpoint_error_summary = oti_endpoint_error_summary_frame(
      family, target, pred, truth
    ),
    endpoint_error_by_index = oti_endpoint_error_by_index_frame(
      family, target, pred, truth
    ),
    chain_summary = chains, mcmc_diagnostics = diagnostics,
    conditional_parity = parity, pathology_summary = pathology,
    recovery_summary = recovery
  )
}

otv2_compact_files <- function() {
  c(
    "config.json", "source_state.json", "runtime_binding.json",
    "design_contract.csv", "oracle_targets.csv", "tail_information.csv",
    "static_basis_audit.csv", "static_basis_transform.csv",
    "static_projection_audit.csv", "static_prior_predictive.csv",
    "dlm_prior_predictive.csv", "dlm_time_contract.csv",
    "fixed_horizon_audit.csv", "fit_plan.csv", "preflight_gates.csv",
    "reference_gates.csv", "input_bundle_binding.csv",
    "benchmark_summary.csv", "fit_summary.csv",
    "fit_curves.csv", "endpoint_error_density.csv",
    "endpoint_error_summary.csv", "endpoint_error_by_index.csv",
    "chain_summary.csv", "mcmc_diagnostics.csv",
    "conditional_parity.csv", "pathology_summary.csv",
    "recovery_summary.csv", "cell_disposition.csv", "run_status.csv",
    "worker_manifest.csv", "failure_log.csv",
    "closeout.json"
  )
}
