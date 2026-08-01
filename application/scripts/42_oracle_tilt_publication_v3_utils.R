otv3_schema <- function() "rqrgibbs_oracle_tilt_publication/3.0.0"

otv3_config_schema <- function() {
  "rqrgibbs_oracle_tilt_publication_config/3.0.0"
}

otv3_preflight_schema <- function() {
  "rqrgibbs_oracle_tilt_publication_preflight/1.0.0"
}

otv3_close <- function(x, y, tolerance = 1e-12) {
  length(x) == 1L && length(y) == 1L && is.finite(x) && is.finite(y) &&
    abs(as.numeric(x) - as.numeric(y)) <= tolerance
}

otv3_required_logical <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    oti_stop(name, " must be one nonmissing logical value.")
  }
  x
}

otv3_numeric_vector <- function(x, name, lower = -Inf, upper = Inf,
                                minimum_length = 1L) {
  value <- as.numeric(unlist(x))
  if (length(value) < minimum_length || any(!is.finite(value)) ||
      any(value < lower) || any(value > upper)) {
    oti_stop(name, " must contain finite values in the declared range.")
  }
  value
}

otv3_integer_scalar <- function(x, name, lower = 0L) {
  value <- as.numeric(x)
  if (length(value) != 1L || !is.finite(value) || value != floor(value) ||
      value < lower || value > .Machine$integer.max) {
    oti_stop(name, " must be one finite integer in the declared range.")
  }
  as.integer(value)
}

otv3_validate_config <- function(config) {
  if (!identical(as.character(config$schema_version), otv3_config_schema())) {
    oti_stop("Unsupported oracle-tilt publication v3 configuration schema.")
  }
  otv3_required_logical(config$execution_authorized, "execution_authorized")
  if (!otv3_close(config$coverage_level, 0.95) ||
      !otv3_close(config$learning_rate, 1)) {
    oti_stop("The v3 illustration requires content 0.95 and learning rate 1.")
  }
  targets <- oti_normalize_targets(config$targets)
  if (!identical(targets, c("RQR", "ET", "SH")) ||
      !identical(as.character(config$tilt_source),
                 "exact_population_oracle")) {
    oti_stop("The ordered exact-oracle RQR/ET/SH target contract changed.")
  }
  law <- config$innovation %||% list()
  if (!identical(as.character(law$family), "asymmetric_laplace") ||
      !otv3_close(law$tau, 0.8) || !otv3_close(law$location, 0) ||
      !otv3_close(law$scale, 1) || !isTRUE(law$standardized)) {
    oti_stop("The v3 source law must be standardized AL_0.80(0,1).")
  }
  fixed <- config$fixed_design %||% list()
  if (otv3_integer_scalar(fixed$n, "fixed_design$n", 20L) != 1200L ||
      !identical(as.character(fixed$basis),
                 "empirical_orthogonal_cubic_bspline") ||
      otv3_integer_scalar(
        fixed$n_chains, "fixed_design$n_chains", 2L
      ) != 4L ||
      otv3_integer_scalar(
        fixed$workers, "fixed_design$workers", 1L
      ) != 2L) {
    oti_stop("The fixed-design v3 contract changed.")
  }
  basis <- fixed$basis_contract %||% list()
  if (otv3_integer_scalar(basis$degree, "basis degree", 1L) != 3L ||
      otv3_integer_scalar(
        basis$expected_dimension, "basis dimension", 1L
      ) != 8L ||
      !identical(basis$intercept, TRUE) ||
      !identical(
        otv3_numeric_vector(basis$internal_knots, "internal knots"),
        c(-0.60, -0.20, 0.20, 0.60)
      ) ||
      !identical(
        otv3_numeric_vector(basis$boundary_knots, "boundary knots"),
        c(-1, 1)
      ) ||
      length(otv3_numeric_vector(
        basis$mean_control_coefficients, "mean coefficients"
      )) != 8L ||
      length(otv3_numeric_vector(
        basis$scale_control_coefficients, "scale coefficients",
        lower = .Machine$double.eps
      )) != 8L ||
      !otv3_close(basis$minimum_scale, 0.35) ||
      !otv3_close(basis$minimum_scale_ratio, 2.0) ||
      !otv3_close(basis$maximum_scale_ratio, 2.5)) {
    oti_stop("The cubic B-spline population contract changed.")
  }
  fixed_control <- fixed$mcmc_control %||% list()
  if (otv3_integer_scalar(fixed_control$n_burn,
                          "fixed_design$mcmc_control$n_burn", 1L) != 1500L ||
      otv3_integer_scalar(fixed_control$n_mcmc,
                          "fixed_design$mcmc_control$n_mcmc", 1L) != 6000L ||
      otv3_integer_scalar(fixed_control$thin,
                          "fixed_design$mcmc_control$thin", 1L) != 1L ||
      otv3_integer_scalar(
        fixed_control$kernel_repetitions,
        "fixed_design$mcmc_control$kernel_repetitions", 1L
      ) != 2L ||
      !identical(fixed_control$store_latent_draws, FALSE)) {
    oti_stop("The fixed-design MCMC contract changed.")
  }
  ridge <- fixed$ridge_prior_selection %||% list()
  ridge_candidates <- otv3_numeric_vector(
    ridge$candidate_tau2, "fixed-design ridge candidates",
    lower = .Machine$double.eps
  )
  if (anyDuplicated(ridge_candidates) || is.unsorted(ridge_candidates) ||
      !identical(as.character(ridge$selection_rule), "largest_passing") ||
      !otv3_close(ridge$expected_tau2, 0.25)) {
    oti_stop("The fixed-design prior-selection contract changed.")
  }
  dlm <- config$dlm %||% list()
  if (otv3_integer_scalar(dlm$T, "dlm$T", 20L) != 1200L ||
      !identical(as.character(dlm$time_grid), "i_over_T") ||
      otv3_integer_scalar(dlm$n_chains, "dlm$n_chains", 2L) != 5L ||
      otv3_integer_scalar(dlm$workers, "dlm$workers", 1L) != 2L ||
      otv3_integer_scalar(
        dlm$expected_missing, "dlm$expected_missing", 0L
      ) != 22L ||
      otv3_integer_scalar(
        dlm$expected_observed, "dlm$expected_observed", 1L
      ) !=
        1178L) {
    oti_stop("The DLM v3 horizon or missingness contract changed.")
  }
  if (!identical(
    otv3_numeric_vector(dlm$time_horizon, "dlm$time_horizon", 0, 1, 2L),
    c(0, 1)
  )) {
    oti_stop("The DLM physical horizon must remain [0,1].")
  }
  dlm_control <- dlm$mcmc_control %||% list()
  if (otv3_integer_scalar(dlm_control$n_burn,
                          "dlm$mcmc_control$n_burn", 1L) != 2500L ||
      otv3_integer_scalar(dlm_control$n_mcmc,
                          "dlm$mcmc_control$n_mcmc", 1L) != 6000L ||
      otv3_integer_scalar(dlm_control$thin,
                          "dlm$mcmc_control$thin", 1L) != 1L ||
      !identical(as.character(dlm_control$backend), "cpp") ||
      !identical(dlm_control$store_state_draws, FALSE) ||
      !identical(dlm_control$store_latent_draws, FALSE)) {
    oti_stop("The DLM MCMC/storage contract changed.")
  }
  seasonal <- dlm$seasonal_contract %||% list()
  scale <- dlm$scale_contract %||% list()
  seasonal_prior <- dlm$seasonal_prior_selection %||% list()
  if (otv3_integer_scalar(seasonal$period, "seasonal period", 2L) != 300L ||
      otv3_integer_scalar(seasonal$harmonic, "seasonal harmonic", 1L) != 1L ||
      otv3_integer_scalar(
        seasonal$expected_cycles, "seasonal cycles", 1L
      ) != 4L ||
      !otv3_close(seasonal$mean_amplitude, 0.62) ||
      !otv3_close(seasonal$mean_phase, 0.25) ||
      !otv3_close(seasonal$evolution_variance, 0) ||
      !otv3_close(scale$baseline, 0.55) ||
      !otv3_close(scale$amplitude, 0.22) ||
      !otv3_close(scale$phase, -0.50) ||
      !otv3_close(scale$minimum_scale, 0.30) ||
      !otv3_close(scale$minimum_scale_ratio, 2.0) ||
      !otv3_close(scale$maximum_scale_ratio, 2.6) ||
      !identical(
        otv3_numeric_vector(
          seasonal_prior$candidate_initial_variance,
          "seasonal prior candidates", lower = .Machine$double.eps
        ), c(0.25, 0.50, 1, 2)
      ) ||
      !identical(as.character(seasonal_prior$selection_rule),
                 "largest_passing") ||
      !otv3_close(seasonal_prior$expected_initial_variance, 1)) {
    oti_stop("The dynamic seasonal location-scale contract changed.")
  }
  profiles <- as.character(unlist(dlm$initial_profiles))
  if (!identical(
    profiles,
    c("default", "oracle_centered", "narrow", "wide",
      "trend_seasonal_stress")
  )) {
    oti_stop("The five DLM initialization profiles changed.")
  }
  plan <- otv3_plan(config)
  if (nrow(plan) != 27L || length(unique(plan$seed)) != 27L ||
      any(plan$cornish_fisher_used)) {
    oti_stop("The 27-chain seed and target plan changed.")
  }
  diagnostics <- config$diagnostics %||% list()
  if (!otv3_close(diagnostics$rhat_max, 1.01) ||
      !otv3_close(diagnostics$bulk_ess_min, 1000) ||
      !otv3_close(diagnostics$tail_ess_min, 1000) ||
      !otv3_close(diagnostics$mcse_over_sd_max, 0.05)) {
    oti_stop("The maintained MCMC diagnostic contract changed.")
  }
  resources <- config$resources %||% list()
  if (!otv3_close(resources$minimum_free_bytes, 21474836480) ||
      otv3_integer_scalar(
        resources$maximum_processes, "maximum_processes", 1L
      ) != 7L ||
      otv3_integer_scalar(
        resources$maximum_R_processes, "maximum_R_processes", 1L
      ) != 3L ||
      otv3_integer_scalar(
        resources$maximum_threads, "maximum_threads", 1L
      ) != 8L ||
      otv3_integer_scalar(
        resources$maximum_sampled_rss_kib,
        "maximum_sampled_rss_kib", 1L
      ) != 12582912L ||
      otv3_integer_scalar(
        resources$maximum_execute_seconds,
        "maximum_execute_seconds", 1L
      ) != 28800L ||
      !otv3_close(resources$monitor_interval_seconds, 0.2)) {
    oti_stop("The monitored resource contract changed.")
  }
  interpretation <- config$interpretation %||% list()
  required_false <- c(
    "response_likelihood", "response_predictive_draws",
    "cornish_fisher_used", "simulation_study"
  )
  if (any(vapply(required_false, function(name) {
    !identical(interpretation[[name]], FALSE)
  }, logical(1L))) || !isTRUE(interpretation$population_oracle_tilts)) {
    oti_stop("The v3 interpretation contract is incomplete.")
  }
  invisible(config)
}

otv3_al_skewness <- function(tau) {
  tau <- oti_scalar(tau, "tau", .Machine$double.eps,
                    1 - .Machine$double.eps)
  m1 <- (1 - tau) / tau - tau / (1 - tau)
  m2 <- 2 * ((1 - tau) / tau^2 + tau / (1 - tau)^2)
  m3 <- 6 * ((1 - tau) / tau^3 - tau / (1 - tau)^3)
  variance <- m2 - m1^2
  (m3 - 3 * m1 * m2 + 2 * m1^3) / variance^(3 / 2)
}

otv3_law <- function(config) {
  law <- oti_al_law(config$innovation$tau, standardized = TRUE)
  checks <- c(
    raw_mean = abs(law$raw_mean - config$innovation$expected_raw_mean),
    raw_sd = abs(law$raw_sd - config$innovation$expected_raw_sd),
    skewness = abs(
      otv3_al_skewness(config$innovation$tau) -
        config$innovation$expected_standardized_skewness
    )
  )
  if (any(!is.finite(checks)) || max(checks) > 1e-12) {
    oti_stop("The AL_0.80 standardization receipt does not reproduce.")
  }
  law
}

otv3_empirical_spline_basis <- function(x, contract) {
  x <- as.numeric(x)
  if (length(x) < 8L || any(!is.finite(x)) || length(unique(x)) < 8L) {
    oti_stop("The empirical spline basis requires finite distinct x values.")
  }
  degree <- otv3_integer_scalar(contract$degree, "basis degree", 1L)
  knots <- otv3_numeric_vector(contract$internal_knots, "internal knots")
  boundary <- otv3_numeric_vector(
    contract$boundary_knots, "boundary knots", minimum_length = 2L
  )
  raw <- splines::bs(
    x, degree = degree, knots = knots, Boundary.knots = boundary,
    intercept = isTRUE(contract$intercept)
  )
  colnames(raw) <- sprintf("spline_%02d", seq_len(ncol(raw)))
  qr_raw <- qr(raw, LAPACK = FALSE)
  expected <- otv3_integer_scalar(
    contract$expected_dimension, "basis dimension", 1L
  )
  if (ncol(raw) != expected || qr_raw$rank != expected) {
    oti_stop("The raw cubic B-spline design is rank deficient or misdimensioned.")
  }
  Q <- qr.Q(qr_raw, complete = FALSE)
  for (column in seq_len(expected)) {
    if (sum(Q[, column] * raw[, column]) < 0) Q[, column] <- -Q[, column]
  }
  X <- Q * sqrt(length(x))
  colnames(X) <- sprintf("basis_%02d", seq_len(expected))
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

otv3_static_prior_audit <- function(config, basis) {
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
      !otv3_close(selected, selection$expected_tau2)) {
    oti_stop("The fixed-design ridge selection is not unique or expected.")
  }
  list(table = out, selected_tau2 = selected)
}

otv3_time_grid <- function(config) {
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

otv3_local_linear_matrices <- function(delta, q_level, q_slope) {
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

otv3_dlm_prior_audit <- function(config) {
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

otv3_seasonal_prior_audit <- function(config) {
  selection <- config$dlm$seasonal_prior_selection
  candidates <- otv3_numeric_vector(
    selection$candidate_initial_variance,
    "seasonal prior candidates", lower = .Machine$double.eps
  )
  critical <- stats::qnorm(0.5 + selection$probability / 2)
  out <- data.frame(
    initial_variance = candidates,
    coefficient_half_width = critical * sqrt(candidates),
    stringsAsFactors = FALSE
  )
  out$pass <- with(
    out,
    coefficient_half_width >= selection$coefficient_half_width_min &
      coefficient_half_width <= selection$coefficient_half_width_max
  )
  passing <- out[out$pass, , drop = FALSE]
  if (!nrow(passing)) oti_stop("No seasonal prior candidate passes.")
  selected <- max(passing$initial_variance)
  out$selected <- out$initial_variance == selected
  if (sum(out$selected) != 1L ||
      !otv3_close(selected, selection$expected_initial_variance)) {
    oti_stop("The seasonal prior selection is not unique or expected.")
  }
  list(table = out, selected_variance = selected)
}

otv3_block_diag <- function(A, B) {
  A <- as.matrix(A)
  B <- as.matrix(B)
  out <- matrix(0, nrow(A) + nrow(B), ncol(A) + ncol(B))
  out[seq_len(nrow(A)), seq_len(ncol(A))] <- A
  rows <- nrow(A) + seq_len(nrow(B))
  cols <- ncol(A) + seq_len(ncol(B))
  out[rows, cols] <- B
  out
}

otv3_expand_cube <- function(matrix, n_time) {
  array(rep(as.matrix(matrix), n_time), c(nrow(matrix), ncol(matrix), n_time))
}

otv3_fixed_horizon_audit <- function(C0, q_level, q_slope,
                                     grids = c(100L, 1200L)) {
  propagate <- function(T) {
    matrices <- otv3_local_linear_matrices(1 / T, q_level, q_slope)
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

otv3_fixed_design_dgp <- function(config, law) {
  set.seed(as.integer(config$fixed_design$seed))
  n <- as.integer(config$fixed_design$n)
  x <- seq(-1, 1, length.out = n)
  contract <- config$fixed_design$basis_contract
  basis <- otv3_empirical_spline_basis(x, contract)
  prior <- otv3_static_prior_audit(config, basis)
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
  y <- mu + scale * law$r(n)
  list(
    family = "fixed_design", seed = as.integer(config$fixed_design$seed),
    x = x, X = basis$X, y = y, mean_truth = mu, scale_truth = scale,
    observed = rep(TRUE, n), basis = basis, prior_audit = prior,
    mean_control_coefficients = mean_coefficients,
    scale_control_coefficients = scale_coefficients,
    scale_ratio = scale_ratio,
    ridge_tau2 = prior$selected_tau2
  )
}

otv3_dlm_dgp <- function(config, law) {
  time_contract <- otv3_time_grid(config)
  prior <- otv3_dlm_prior_audit(config)
  seasonal_prior <- otv3_seasonal_prior_audit(config)
  selected <- prior$selected
  matrices <- otv3_local_linear_matrices(
    time_contract$delta, selected$q_level, selected$q_slope
  )
  T <- as.integer(config$dlm$T)
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
  seasonal_contract <- config$dlm$seasonal_contract
  seasonal_model <- rqrgibbs::rqr_seasonal(
    period = seasonal_contract$period,
    harmonics = seasonal_contract$harmonic,
    m0 = c(0, 0),
    C0 = diag(seasonal_prior$selected_variance, 2L),
    name = "seasonal_harmonic"
  )
  model <- trend_model + seasonal_model
  W_one <- otv3_block_diag(
    matrices$W,
    diag(seasonal_contract$evolution_variance, 2L)
  )
  W <- otv3_expand_cube(W_one, T)
  set.seed(as.integer(config$dlm$seed))
  local_theta <- matrix(NA_real_, 2L, T)
  previous <- as.numeric(unlist(config$dlm$initial_state))
  if (length(previous) != 2L || any(!is.finite(previous))) {
    oti_stop("dlm$initial_state must contain finite level and slope values.")
  }
  factor <- t(chol(matrices$W))
  for (index in seq_len(T)) {
    previous <- drop(matrices$G %*% previous + factor %*% stats::rnorm(2L))
    local_theta[, index] <- previous
  }
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
  scale_contract <- config$dlm$scale_contract
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
  y_full <- mean_truth + scale * law$r(T)
  y <- y_full
  y[time_contract$missing] <- NA_real_
  list(
    family = "dlm", seed = as.integer(config$dlm$seed),
    time = time_contract$time, delta = time_contract$delta,
    y = y, y_full = y_full, model = model, W = W,
    mean_truth = mean_truth,
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

otv3_tail_information <- function(config, oracle, n_observed_dlm) {
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

otv3_projection_audit <- function(dgp, targets) {
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

otv3_dynamic_state_for_quantile <- function(dgp, innovation_quantile) {
  innovation_quantile <- oti_scalar(
    innovation_quantile, "innovation quantile", -Inf, Inf
  )
  trend <- t(dgp$local_state_truth)
  trend[1L, ] <- trend[1L, ] +
    innovation_quantile * dgp$scale_baseline
  seasonal <- t(dgp$mean_seasonal_state_truth) +
    innovation_quantile * t(dgp$scale_seasonal_state_truth)
  rbind(trend, seasonal)
}

otv3_dynamic_projection_audit <- function(dgp, targets) {
  expanded <- otf_expanded_dlm(dgp)
  rows <- lapply(c("RQR", "ET", "SH"), function(target) {
    truth <- oti_target_row(targets, target)
    do.call(rbind, lapply(c("lower", "upper"), function(endpoint) {
      oracle <- truth[[paste0("oracle_", endpoint)]]
      quantile <- mean((oracle - dgp$mean_truth) / dgp$scale_truth)
      state <- otv3_dynamic_state_for_quantile(dgp, quantile)
      reconstructed <- colSums(expanded$FF * state)
      residual <- oracle - reconstructed
      data.frame(
        target = target, endpoint = endpoint,
        innovation_quantile = quantile,
        max_absolute_residual = max(abs(residual)),
        root_mean_square_residual = sqrt(mean(residual^2)),
        stringsAsFactors = FALSE
      )
    }))
  })
  do.call(rbind, rows)
}

otv3_seasonal_covariance_audit <- function(dgp) {
  expanded <- otf_expanded_dlm(dgp)
  covariance <- expanded$C0[3:4, 3:4, drop = FALSE]
  initial <- covariance
  maximum_error <- 0
  for (index in seq_len(expanded$n_time)) {
    G <- expanded$GG[3:4, 3:4, index, drop = FALSE][, , 1L]
    W <- expanded$W[3:4, 3:4, index, drop = FALSE][, , 1L]
    covariance <- G %*% covariance %*% t(G) + W
    maximum_error <- max(maximum_error, max(abs(covariance - initial)))
  }
  data.frame(
    seasonal_dimension = 2L,
    evolution_variance = max(abs(expanded$W[3:4, 3:4, ])),
    maximum_covariance_invariance_error = maximum_error,
    stringsAsFactors = FALSE
  )
}

otv3_observability_audit <- function(dgp) {
  expanded <- otf_expanded_dlm(dgp)
  period <- as.integer(dgp$seasonal_period)
  transition <- diag(expanded$p)
  rows <- matrix(NA_real_, period, expanded$p)
  for (index in seq_len(period)) {
    transition <- expanded$GG[, , index] %*% transition
    rows[index, ] <- drop(t(expanded$FF[, index, drop = FALSE]) %*%
      transition)
  }
  data.frame(
    rows = nrow(rows), columns = ncol(rows), rank = qr(rows)$rank,
    reciprocal_condition_number = 1 / kappa(rows),
    stringsAsFactors = FALSE
  )
}

otv3_scale_information <- function(config, oracle, fixed_dgp, dlm_dgp) {
  one_family <- function(family, scale, observed) {
    scale <- scale[observed]
    group <- pmin(5L, ceiling(rank(scale, ties.method = "first") /
      (length(scale) / 5)))
    do.call(rbind, lapply(seq_len(5L), function(quintile) {
      n <- sum(group == quintile)
      data.frame(
        family = family, scale_quintile = quintile,
        n_observed = n, scale_min = min(scale[group == quintile]),
        scale_max = max(scale[group == quintile]),
        stringsAsFactors = FALSE
      )
    }))
  }
  base <- rbind(
    one_family("fixed_design", fixed_dgp$scale_truth, fixed_dgp$observed),
    one_family("dlm", dlm_dgp$scale_truth, dlm_dgp$observed)
  )
  rows <- merge(
    base,
    data.frame(
      target = oracle$target,
      rare_tail_probability = pmin(
        oracle$u, 1 - config$coverage_level - oracle$u
      ),
      stringsAsFactors = FALSE
    ), by = NULL
  )
  rows$expected_rare_tail_count <-
    rows$n_observed * rows$rare_tail_probability
  rows[order(rows$family, rows$target, rows$scale_quintile), ]
}

otv3_plan <- function(config) {
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

otv3_chain_batches <- function(chains, workers) {
  chains <- as.integer(chains)
  workers <- otv3_integer_scalar(workers, "workers", 1L)
  if (!length(chains) || anyNA(chains) || any(chains < 1L) ||
      anyDuplicated(chains)) {
    oti_stop("chains must contain unique positive integers.")
  }
  split(chains, ceiling(seq_along(chains) / workers))
}

otv3_design_preflight <- function(config) {
  otv3_validate_config(config)
  law <- otv3_law(config)
  oracle <- oti_oracle_targets(law, config$coverage_level, config$targets)
  fixed <- otv3_fixed_design_dgp(config, law)
  dlm <- otv3_dlm_dgp(config, law)
  fixed_targets <- oti_targets_by_index(
    fixed$mean_truth, fixed$scale_truth, oracle, fixed$observed
  )
  dlm_targets <- oti_targets_by_index(
    dlm$mean_truth, dlm$scale_truth, oracle, dlm$observed
  )
  tail <- otv3_tail_information(config, oracle, sum(dlm$observed))
  projection <- otv3_projection_audit(fixed, fixed_targets)
  dynamic_projection <- otv3_dynamic_projection_audit(dlm, dlm_targets)
  seasonal_covariance <- otv3_seasonal_covariance_audit(dlm)
  observability <- otv3_observability_audit(dlm)
  scale_information <- otv3_scale_information(config, oracle, fixed, dlm)
  gram_error <- max(abs(
    fixed$basis$gram - diag(ncol(fixed$basis$X))
  ))
  horizon_error <- max(dlm$fixed_horizon_audit$max_absolute_error)
  gates <- data.frame(
    gate = c(
      "expected_rare_tail_count", "static_design_rank",
      "static_basis_gram", "static_truth_projection",
      "dynamic_truth_projection", "fixed_horizon_covariance",
      "seasonal_covariance_invariance", "dynamic_observability_rank",
      "static_scale_floor", "dynamic_scale_floor",
      "scale_quintile_rare_tail_count", "missing_mask", "fit_plan_size",
      "cornish_fisher_absent"
    ),
    value = c(
      min(c(tail$fixed_design_expected_rare_count,
            tail$dlm_expected_rare_count)),
      fixed$basis$rank, gram_error,
      max(projection$max_absolute_residual),
      max(dynamic_projection$max_absolute_residual), horizon_error,
      seasonal_covariance$maximum_covariance_invariance_error,
      observability$rank, min(fixed$scale_truth), min(dlm$scale_truth),
      min(scale_information$expected_rare_tail_count),
      sum(!dlm$observed), nrow(otv3_plan(config)), 0
    ),
    threshold = c(
      config$preflight_gates$minimum_expected_rare_tail_count,
      config$preflight_gates$static_design_rank,
      config$preflight_gates$static_gram_max_abs_error,
      config$preflight_gates$static_projection_max_abs,
      config$preflight_gates$dynamic_projection_max_abs,
      config$preflight_gates$fixed_horizon_covariance_max_abs_error,
      config$preflight_gates$seasonal_covariance_invariance_max_abs_error,
      config$preflight_gates$dynamic_observability_rank,
      config$fixed_design$basis_contract$minimum_scale,
      config$dlm$scale_contract$minimum_scale,
      config$preflight_gates$
        minimum_expected_rare_tail_count_per_scale_quintile,
      config$dlm$expected_missing, 27, 0
    ),
    comparison = c(
      ">=", "==", "<=", "<=", "<=", "<=", "<=", "==",
      ">=", ">=", ">=", "==", "==", "=="
    ),
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
    dynamic_projection_audit = dynamic_projection,
    seasonal_covariance_audit = seasonal_covariance,
    observability_audit = observability,
    scale_information = scale_information,
    static_prior_audit = fixed$prior_audit$table,
    dlm_prior_audit = dlm$prior_audit$table,
    seasonal_prior_audit = dlm$seasonal_prior_audit$table,
    fixed_horizon_audit = dlm$fixed_horizon_audit,
    plan = otv3_plan(config), gates = gates,
    pass = all(gates$pass)
  )
}

otv3_psd_factor <- function(matrix, tolerance = 100 * .Machine$double.eps) {
  matrix <- 0.5 * matrix + 0.5 * t(matrix)
  eigen_result <- eigen(matrix, symmetric = TRUE)
  scale <- max(1, max(abs(eigen_result$values)))
  if (min(eigen_result$values) < -tolerance * scale) {
    oti_stop("A covariance in the PSD reference is materially indefinite.")
  }
  retained <- eigen_result$values > tolerance * scale
  if (!any(retained)) return(matrix(0, nrow(matrix), 0L))
  sweep(
    eigen_result$vectors[, retained, drop = FALSE],
    2L, sqrt(eigen_result$values[retained]), `*`
  )
}

otv3_dense_gaussian_reference_psd <- function(z, H, V, expanded,
                                               canonical_shift) {
  p <- expanded$p
  n_time <- expanded$n_time
  if (length(z) != n_time || !identical(dim(H), c(p, n_time)) ||
      length(V) != n_time ||
      !identical(dim(canonical_shift), c(p, n_time))) {
    oti_stop("PSD dense-reference inputs have incompatible dimensions.")
  }

  initial_factor <- otv3_psd_factor(expanded$C0)
  evolution_factors <- lapply(seq_len(n_time), function(tt) {
    otv3_psd_factor(expanded$W[, , tt])
  })
  innovation_dimensions <- vapply(evolution_factors, ncol, integer(1L))
  q <- ncol(initial_factor) + sum(innovation_dimensions)
  if (q < 1L) oti_stop("The PSD reference has no stochastic coordinates.")

  state_map <- matrix(0, p * n_time, q)
  state_base <- matrix(0, p, n_time)
  previous_map <- matrix(0, p, q)
  previous_map[, seq_len(ncol(initial_factor))] <- initial_factor
  previous_base <- drop(expanded$m0)
  offset <- ncol(initial_factor)
  state_index <- function(tt) ((tt - 1L) * p + 1L):(tt * p)
  for (tt in seq_len(n_time)) {
    current_map <- expanded$GG[, , tt] %*% previous_map
    current_base <- drop(expanded$GG[, , tt] %*% previous_base)
    rank_w <- innovation_dimensions[tt]
    if (rank_w > 0L) {
      columns <- offset + seq_len(rank_w)
      current_map[, columns] <- evolution_factors[[tt]]
      offset <- offset + rank_w
    }
    state_map[state_index(tt), ] <- current_map
    state_base[, tt] <- current_base
    previous_map <- current_map
    previous_base <- current_base
  }

  precision <- diag(q)
  information <- numeric(q)
  observed <- which(!is.na(z))
  for (tt in observed) {
    row_map <- drop(H[, tt] %*% state_map[state_index(tt), , drop = FALSE])
    base_ordinate <- sum(H[, tt] * state_base[, tt])
    precision <- precision + tcrossprod(row_map) / V[tt]
    information <- information + row_map * (z[tt] - base_ordinate) / V[tt]
  }
  for (tt in seq_len(n_time)) {
    information <- information +
      crossprod(state_map[state_index(tt), , drop = FALSE],
                canonical_shift[, tt])[, 1L]
  }
  precision <- 0.5 * precision + 0.5 * t(precision)
  precision_chol <- chol(precision)
  coordinate_mean <- drop(backsolve(
    precision_chol,
    forwardsolve(t(precision_chol), information)
  ))
  coordinate_covariance <- chol2inv(precision_chol)
  mean_vector <- as.vector(state_base) + drop(state_map %*% coordinate_mean)
  covariance <- state_map %*% coordinate_covariance %*% t(state_map)
  residual <- drop(precision %*% coordinate_mean - information)
  list(
    mean = matrix(mean_vector, p, n_time),
    covariance = covariance,
    precision = precision,
    information = information,
    reciprocal_condition = rcond(precision),
    maximum_absolute_normalized_residual =
      max(abs(residual)) / max(1, max(abs(information))),
    stochastic_dimension = q,
    evolution_ranks = innovation_dimensions
  )
}

otv3_reference_suite <- function(config) {
  set.seed(202608013L)
  p <- 8L
  n <- 24L
  X <- matrix(stats::rnorm(n * p), n, p)
  y <- stats::rnorm(n)
  beta_other <- seq(-0.25, 0.25, length.out = p)
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
  selected <- otv3_dlm_prior_audit(config)$selected
  matrices <- otv3_local_linear_matrices(
    1 / T, selected$q_level, selected$q_slope
  )
  GG <- otv3_expand_cube(matrices$G, T)
  W <- otv3_expand_cube(matrices$W, T)
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
  set.seed(202608014L)
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

  seasonal_T <- 16L
  seasonal_delta <- 1 / seasonal_T
  local <- otv3_local_linear_matrices(
    seasonal_delta, selected$q_level, selected$q_slope
  )
  seasonal_G <- rqrgibbs::rqr_seasonal(
    period = 8, harmonics = 1, C0 = diag(1, 2)
  )$GG
  seasonal_GG_one <- otv3_block_diag(local$G, seasonal_G)
  seasonal_W_one <- otv3_block_diag(local$W, matrix(0, 2, 2))
  seasonal_GG <- otv3_expand_cube(seasonal_GG_one, seasonal_T)
  seasonal_W <- otv3_expand_cube(seasonal_W_one, seasonal_T)
  seasonal_C0 <- otv3_block_diag(diag(c(1, 0.25)), diag(1, 2))
  seasonal_H <- rbind(
    rep(1, seasonal_T),
    seq(-0.15, 0.15, length.out = seasonal_T),
    cos(2 * pi * seq_len(seasonal_T) / 8),
    sin(2 * pi * seq_len(seasonal_T) / 8)
  )
  seasonal_response <- stats::rnorm(seasonal_T)
  seasonal_response[c(5L, 12L)] <- NA_real_
  seasonal_variance <- stats::runif(seasonal_T, 0.35, 1.15)
  seasonal_shift <- rbind(
    seq(-0.04, 0.04, length.out = seasonal_T),
    seq(0.02, -0.02, length.out = seasonal_T),
    0.03 * cos(2 * pi * seq_len(seasonal_T) / 8),
    0.03 * sin(2 * pi * seq_len(seasonal_T) / 8)
  )
  seasonal_shift[, is.na(seasonal_response)] <- 0
  seasonal_expanded <- list(
    p = 4L, n_time = seasonal_T, GG = seasonal_GG, W = seasonal_W,
    C0 = seasonal_C0, m0 = matrix(0, 4L, 1L), FF = seasonal_H
  )
  seasonal_dense <- otv3_dense_gaussian_reference_psd(
    seasonal_response, seasonal_H, seasonal_variance,
    seasonal_expanded, seasonal_shift
  )
  seasonal_evolution <- rqrgibbs::rqr_evolution_fixed(seasonal_W)
  seasonal_R <- rqrgibbs::rqr_ffbs_smooth(
    seasonal_response, seasonal_H, seasonal_variance, seasonal_GG,
    seasonal_expanded$m0, seasonal_C0, seasonal_evolution,
    backend = "R", numerical_policy = "fail",
    canonical_shift = seasonal_shift
  )
  seasonal_cpp <- rqrgibbs::rqr_ffbs_smooth(
    seasonal_response, seasonal_H, seasonal_variance, seasonal_GG,
    seasonal_expanded$m0, seasonal_C0, seasonal_evolution,
    backend = "cpp", numerical_policy = "fail",
    canonical_shift = seasonal_shift
  )
  seasonal_dense_blocks <- array(NA_real_, c(4L, 4L, seasonal_T))
  for (index in seq_len(seasonal_T)) {
    block <- ((index - 1L) * 4L + 1L):(index * 4L)
    seasonal_dense_blocks[, , index] <-
      seasonal_dense$covariance[block, block]
  }
  seasonal_rows <- data.frame(
    gate = c(
      "seasonal_R_dense_mean_absolute_error",
      "seasonal_cpp_dense_mean_absolute_error",
      "seasonal_R_cpp_mean_absolute_error",
      "seasonal_R_dense_marginal_covariance_absolute_error",
      "seasonal_cpp_dense_marginal_covariance_absolute_error",
      "seasonal_R_cpp_marginal_covariance_absolute_error",
      "seasonal_R_repair_count", "seasonal_cpp_repair_count",
      "seasonal_missing_measurement_omission",
      "seasonal_zero_evolution_block"
    ),
    value = c(
      max(abs(seasonal_R$smooth_mean - seasonal_dense$mean)),
      max(abs(seasonal_cpp$smooth_mean - seasonal_dense$mean)),
      max(abs(seasonal_R$smooth_mean - seasonal_cpp$smooth_mean)),
      max(abs(seasonal_R$smooth_cov - seasonal_dense_blocks)),
      max(abs(seasonal_cpp$smooth_cov - seasonal_dense_blocks)),
      max(abs(seasonal_R$smooth_cov - seasonal_cpp$smooth_cov)),
      seasonal_R$diagnostics$repair_count,
      seasonal_cpp$diagnostics$repair_count,
      as.numeric(all(is.na(
        seasonal_R$residual[c(5L, 12L)]
      ))),
      max(abs(seasonal_W[3:4, 3:4, ]))
    ),
    threshold = c(1e-9, 1e-9, 1e-10, 1e-9, 1e-9, 1e-10, 0, 0, 1, 0),
    comparison = c(rep("<=", 8L), "==", "=="),
    stringsAsFactors = FALSE
  )
  rows <- rbind(rows, seasonal_rows)
  rows$pass <- mapply(function(value, threshold, comparison) {
    if (comparison == "<=") value <= threshold else
      abs(value - threshold) <= 1e-12
  }, rows$value, rows$threshold, rows$comparison)
  rows
}

otv3_initial_state_paths <- function(profile, dgp, truth) {
  profile <- match.arg(profile, c(
    "default", "oracle_centered", "narrow", "wide",
    "trend_seasonal_stress"
  ))
  if (identical(profile, "default")) return(list())
  lower_quantile <- mean(
    (truth$oracle_lower - dgp$mean_truth) / dgp$scale_truth
  )
  upper_quantile <- mean(
    (truth$oracle_upper - dgp$mean_truth) / dgp$scale_truth
  )
  lower_state <- otv3_dynamic_state_for_quantile(dgp, lower_quantile)
  upper_state <- otv3_dynamic_state_for_quantile(dgp, upper_quantile)
  midpoint_state <- 0.5 * (lower_state + upper_state)
  half_difference_state <- 0.5 * (upper_state - lower_state)
  multiplier <- switch(
    profile, oracle_centered = 1, narrow = 0.25, wide = 2.5,
    trend_seasonal_stress = 1
  )
  theta1 <- midpoint_state - multiplier * half_difference_state
  theta2 <- midpoint_state + multiplier * half_difference_state
  expanded <- otf_expanded_dlm(dgp)
  G1 <- expanded$GG[, , 1L]
  out <- list(
    state_root1 = theta1, state_root2 = theta2,
    theta0_root1 = drop(solve(G1, theta1[, 1L])),
    theta0_root2 = drop(solve(G1, theta2[, 1L]))
  )
  if (identical(profile, "trend_seasonal_stress")) {
    perturbation0 <- c(
      0, 2 * sqrt(dgp$initial_slope_variance),
      2 * sqrt(dgp$seasonal_prior_audit$selected_variance), 0
    )
    perturbation <- matrix(NA_real_, expanded$p, expanded$n_time)
    previous <- perturbation0
    for (index in seq_len(expanded$n_time)) {
      previous <- drop(expanded$GG[, , index] %*% previous)
      perturbation[, index] <- previous
    }
    out$theta0_root2 <- out$theta0_root2 + perturbation0
    out$state_root2 <- out$state_root2 + perturbation
  }
  out
}

otv3_diagnostic_indices <- function(dgp) {
  n <- length(dgp$y)
  regular <- unique(as.integer(round(seq(1, n, length.out = 21L))))
  scale_extrema <- c(which.min(dgp$scale_truth), which.max(dgp$scale_truth))
  if (identical(dgp$family, "fixed_design")) {
    knots <- as.numeric(attr(dgp$basis$raw, "knots"))
    knot_indices <- vapply(
      knots, function(knot) which.min(abs(dgp$x - knot)), integer(1L)
    )
    return(sort(unique(c(regular, scale_extrema, knot_indices))))
  }
  missing <- which(!dgp$observed)
  boundary <- integer(0)
  if (length(missing)) {
    starts <- missing[c(TRUE, diff(missing) > 1L)]
    ends <- missing[c(diff(missing) > 1L, TRUE)]
    boundary <- unique(c(starts - 1L, starts, ends, ends + 1L))
  }
  seasonal_extrema <- c(
    which.min(dgp$mean_seasonal_state_truth[, 1L]),
    which.max(dgp$mean_seasonal_state_truth[, 1L])
  )
  sort(unique(c(
    regular, scale_extrema, seasonal_extrema,
    boundary[boundary >= 1L & boundary <= n]
  )))
}

otv3_scalar_draw_matrix <- function(family, pred, truth, y, coverage_level,
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
  indices <- otv3_diagnostic_indices(dgp)
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

otv3_fixed_chain <- function(config, dgp, targets, target, chain,
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
    scalar_draws = otv3_scalar_draw_matrix(
      "fixed_design", pred, truth, dgp$y, config$coverage_level, dgp
    ),
    chain_summary = cbind(
      oti_chain_summary("fixed_design", target, chain, seed, fit, elapsed),
      profile = "seed_only", otp_provenance_summary(fit)
    )
  )
}

otv3_conditional_parity <- function(fit, dgp, truth, coverage_level,
                                    draw = NULL) {
  expanded <- otf_expanded_dlm(dgp)
  observed <- is.finite(dgp$y)
  stored_latent <- !is.null(fit$samp.latent_v)
  n_draw <- ncol(fit$samp.eta_root1)
  draw <- as.integer(draw %||% if (stored_latent) ceiling(n_draw / 2) else 0L)
  constants <- rqrgibbs::rqr_constants(coverage_level, 1)
  latent_v <- if (stored_latent) {
    fit$samp.latent_v[, draw]
  } else {
    fit$checkpoint_state$latent_v
  }
  variance <- constants$phi * latent_v
  canonical <- matrix(0, expanded$p, expanded$n_time)
  canonical[, observed] <- sweep(
    expanded$FF[, observed, drop = FALSE], 2L,
    coverage_level * truth$mean_tilt[observed], `*`
  )
  evolution <- rqrgibbs::rqr_evolution_fixed(expanded$W)
  rows <- lapply(c("root1", "root2"), function(root) {
    other <- if (stored_latent) {
      if (root == "root1") fit$samp.eta_root2[, draw] else
        fit$samp.eta_root1[, draw]
    } else {
      theta_other <- if (root == "root1") {
        fit$checkpoint_state$theta_root2
      } else {
        fit$checkpoint_state$theta_root1
      }
      colSums(expanded$FF * theta_other)
    }
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

otv3_pathology_summary <- function(pred, dgp, truth, config) {
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

otv3_dlm_chain <- function(config, dgp, targets, target, chain,
                           provenance_control, mcmc_override = NULL) {
  truth <- oti_target_row(targets, target)
  profiles <- as.character(unlist(config$dlm$initial_profiles))
  profile <- profiles[chain]
  seed <- otp_seed(config, "dlm", target, chain)
  control <- config$dlm$mcmc_control
  if (!is.null(mcmc_override)) control <- modifyList(control, mcmc_override)
  control$seed <- seed
  init <- otv3_initial_state_paths(profile, dgp, truth)
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
    scalar_draws = otv3_scalar_draw_matrix(
      "dlm", pred, truth, dgp$y, config$coverage_level, dgp
    ),
    chain_summary = cbind(
      oti_chain_summary("dlm", target, chain, seed, fit, elapsed),
      profile = profile, otp_provenance_summary(fit)
    ),
    conditional_parity = otv3_conditional_parity(
      fit, dgp, truth, config$coverage_level
    ),
    pathology = otv3_pathology_summary(pred, dgp, truth, config)
  )
}

otv3_heterogeneity_summary <- function(family, curves, truth, dgp) {
  scale <- dgp$scale_truth
  group <- pmin(5L, ceiling(rank(scale, ties.method = "first") /
    (length(scale) / 5)))
  low <- group == 1L
  high <- group == 5L
  local_width <- truth$oracle_width
  normalized_error <- sqrt(0.5 * (
    ((curves$fit_lower - curves$oracle_lower) / local_width)^2 +
      ((curves$fit_upper - curves$oracle_upper) / local_width)^2
  ))
  oracle_contrast <- mean(local_width[high]) / mean(local_width[low])
  fitted_contrast <- mean(curves$fit_width[high]) /
    mean(curves$fit_width[low])
  amplitude_ratio <- phase_error <- NA_real_
  if (identical(family, "dlm")) {
    omega <- 2 * pi * dgp$seasonal_harmonic / dgp$seasonal_period
    design <- cbind(
      intercept = 1,
      cosine = cos(omega * curves$index),
      sine = sin(omega * curves$index)
    )
    oracle_coefficient <- qr.solve(design, local_width)
    fitted_coefficient <- qr.solve(design, curves$fit_width)
    oracle_amplitude <- sqrt(sum(oracle_coefficient[c("cosine", "sine")]^2))
    fitted_amplitude <- sqrt(sum(fitted_coefficient[c("cosine", "sine")]^2))
    oracle_phase <- atan2(-oracle_coefficient["sine"],
                          oracle_coefficient["cosine"])
    fitted_phase <- atan2(-fitted_coefficient["sine"],
                          fitted_coefficient["cosine"])
    amplitude_ratio <- fitted_amplitude / oracle_amplitude
    phase_error <- abs(atan2(
      sin(fitted_phase - oracle_phase), cos(fitted_phase - oracle_phase)
    ))
  }
  data.frame(
    low_scale_endpoint_rmse_over_local_width =
      sqrt(mean(normalized_error[low]^2)),
    high_scale_endpoint_rmse_over_local_width =
      sqrt(mean(normalized_error[high]^2)),
    oracle_high_low_width_contrast = oracle_contrast,
    fitted_high_low_width_contrast = fitted_contrast,
    width_contrast_relative_error = abs(fitted_contrast / oracle_contrast - 1),
    seasonal_width_amplitude_ratio = amplitude_ratio,
    seasonal_width_phase_error = phase_error,
    stringsAsFactors = FALSE
  )
}

otv3_recovery_summary <- function(family, curves, metrics, config) {
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

otv3_benchmark_assessment <- function(family, target, result, dgp, targets,
                                      config) {
  truth <- oti_target_row(targets, target)
  x <- if (identical(family, "fixed_design")) dgp$x else dgp$time
  curves <- oti_curve_frame(family, target, x, dgp$y, result$pred, truth)
  metrics <- oti_interval_metrics(result$pred, truth, dgp$y)
  recovery <- otv3_recovery_summary(family, curves, metrics, config)
  heterogeneity <- otv3_heterogeneity_summary(
    family, curves, truth, dgp
  )
  gates <- config$recovery_gates
  gross_recovery_pass <- with(
    recovery,
    is.finite(endpoint_rmse_over_oracle_width) &
      endpoint_rmse_over_oracle_width <=
        2 * gates$endpoint_rmse_over_oracle_width_max &
      is.finite(mean_width_ratio) &
      mean_width_ratio >= max(0, gates$mean_width_ratio_min - 0.20) &
      mean_width_ratio <= gates$mean_width_ratio_max + 0.20 &
      abs(lower_bias_over_oracle_width) <=
        2 * gates$absolute_endpoint_bias_over_oracle_width_max &
      abs(upper_bias_over_oracle_width) <=
        2 * gates$absolute_endpoint_bias_over_oracle_width_max
  )
  pathology_pass <- if (identical(family, "dlm")) {
    pathology <- result$pathology
    nrow(pathology) == 1L && is.finite(pathology$remote_draw_fraction) &&
      pathology$remote_draw_fraction <=
        config$diagnostics$maximum_remote_draw_fraction
  } else TRUE
  heterogeneity_pass <- with(
    heterogeneity,
    low_scale_endpoint_rmse_over_local_width <=
      2 * config$recovery_gates$
        scale_stratum_endpoint_rmse_over_local_width_max &
      high_scale_endpoint_rmse_over_local_width <=
        2 * config$recovery_gates$
          scale_stratum_endpoint_rmse_over_local_width_max &
      width_contrast_relative_error <=
        2 * config$recovery_gates$
          scale_stratum_width_contrast_relative_error_max &
      (identical(family, "fixed_design") ||
        (seasonal_width_amplitude_ratio >= 0.5 &&
         seasonal_width_amplitude_ratio <= 1.5 &&
         seasonal_width_phase_error <= 0.70))
  )
  cbind(data.frame(
    family = family, target = target,
    endpoint_rmse_over_oracle_width =
      recovery$endpoint_rmse_over_oracle_width,
    mean_width_ratio = recovery$mean_width_ratio,
    lower_bias_over_oracle_width = recovery$lower_bias_over_oracle_width,
    upper_bias_over_oracle_width = recovery$upper_bias_over_oracle_width,
    gross_recovery_pass = gross_recovery_pass,
    pathology_pass = pathology_pass,
    heterogeneity_pass = heterogeneity_pass,
    stringsAsFactors = FALSE
  ), heterogeneity)
}

otv3_summarize_cell <- function(family, target, chain_results, dgp, targets,
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
  recovery <- otv3_recovery_summary(family, curves, metrics, config)
  heterogeneity <- otv3_heterogeneity_summary(
    family, curves, truth, dgp
  )
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
  heterogeneity_pass <- with(
    heterogeneity,
    low_scale_endpoint_rmse_over_local_width <=
      gates$scale_stratum_endpoint_rmse_over_local_width_max &
      high_scale_endpoint_rmse_over_local_width <=
        gates$scale_stratum_endpoint_rmse_over_local_width_max &
      width_contrast_relative_error <=
        gates$scale_stratum_width_contrast_relative_error_max &
      (identical(family, "fixed_design") ||
        (seasonal_width_amplitude_ratio >=
           gates$dlm_seasonal_width_amplitude_ratio_min &
         seasonal_width_amplitude_ratio <=
           gates$dlm_seasonal_width_amplitude_ratio_max &
         seasonal_width_phase_error <=
           gates$dlm_seasonal_width_phase_error_max))
  )
  computational_pass <- provenance_pass && diagnostics_pass && parity_pass &&
    pathology_pass
  disposition <- if (computational_pass && recovery_pass &&
      heterogeneity_pass) "strict_pass" else
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
      heterogeneity_pass = heterogeneity_pass,
      disposition = disposition,
      manuscript_illustration_evidence_eligible =
        computational_pass && recovery_pass && heterogeneity_pass,
      n_chains = nrow(chains), retained_draws = ncol(pred$lower_draws),
      numerical_repair_count = sum(chains$numerical_repair_count),
      stringsAsFactors = FALSE
    ),
    metrics, recovery, heterogeneity
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
    recovery_summary = recovery,
    heterogeneity_summary = heterogeneity
  )
}

otv3_compact_files <- function() {
  c(
    "config.json", "source_state.json", "runtime_binding.json",
    "design_contract.csv", "oracle_targets.csv", "tail_information.csv",
    "static_basis_audit.csv", "static_basis_transform.csv",
    "static_projection_audit.csv", "static_prior_predictive.csv",
    "dynamic_projection_audit.csv", "dlm_prior_predictive.csv",
    "seasonal_prior_predictive.csv", "dlm_time_contract.csv",
    "fixed_horizon_audit.csv", "seasonal_covariance_audit.csv",
    "dynamic_observability_audit.csv", "scale_information.csv",
    "fit_plan.csv", "preflight_gates.csv",
    "reference_gates.csv", "input_bundle_binding.csv",
    "benchmark_summary.csv", "fit_summary.csv",
    "fit_curves.csv", "endpoint_error_density.csv",
    "endpoint_error_summary.csv", "endpoint_error_by_index.csv",
    "chain_summary.csv", "mcmc_diagnostics.csv",
    "conditional_parity.csv", "pathology_summary.csv",
    "recovery_summary.csv", "heterogeneity_summary.csv",
    "cell_disposition.csv", "run_status.csv",
    "worker_manifest.csv", "failure_log.csv",
    "closeout.json"
  )
}
