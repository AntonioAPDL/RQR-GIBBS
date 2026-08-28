.rqr_tcsp_schema <- function() {
  "rqrgibbs_tcsp_scan_action/1.0.0"
}

.rqr_tcsp_path_schema <- function() {
  "rqrgibbs_tcsp_scan_path/1.0.0"
}

.rqr_tcsp_assert_probability <- function(x, name) {
  x <- as.numeric(x)[1L]
  if (!is.finite(x) || x <= 0 || x >= 1) {
    stop(sprintf("%s must be one finite scalar in (0, 1).", name),
         call. = FALSE)
  }
  x
}

.rqr_tcsp_assert_method <- function(method) {
  match.arg(
    as.character(method)[1L],
    c("monte_carlo_conservative", "monte_carlo_cp_adaptive",
      "dkw_conservative")
  )
}

.rqr_tcsp_scan_max_count <- function(u, content, tolerance = 1e-14) {
  u <- sort(as.numeric(u))
  n <- length(u)
  best <- 0L
  right <- 1L
  for (left in seq_len(n)) {
    if (right < left) right <- left
    while (right + 1L <= n &&
           u[[right + 1L]] - u[[left]] <= content + tolerance) {
      right <- right + 1L
    }
    current <- right - left + 1L
    if (current > best) best <- current
  }
  as.integer(best)
}

.rqr_tcsp_digest <- function(x) {
  if (requireNamespace("digest", quietly = TRUE)) {
    digest::digest(x, algo = "sha256")
  } else {
    NA_character_
  }
}

.rqr_tcsp_count_histogram <- function(max_counts, n) {
  counts <- tabulate(as.integer(max_counts) + 1L, nbins = n + 1L)
  names(counts) <- as.character(0:n)
  counts
}

.rqr_tcsp_package_version <- function() {
  tryCatch(as.character(utils::packageVersion("rqrgibbs")),
           error = function(e) NA_character_)
}

.rqr_tcsp_git_commit <- function() {
  out <- tryCatch(
    suppressWarnings(system2("git", c("rev-parse", "HEAD"),
                             stdout = TRUE, stderr = FALSE)),
    error = function(e) character(0)
  )
  status <- attr(out, "status") %||% 0L
  if (!length(out) || is.na(status) || status != 0L) {
    return(NA_character_)
  }
  as.character(out[[1L]])
}

.rqr_tcsp_git_status_short <- function() {
  out <- tryCatch(
    suppressWarnings(system2("git", c("status", "--short"),
                             stdout = TRUE, stderr = FALSE)),
    error = function(e) character(0)
  )
  status <- attr(out, "status") %||% 0L
  if (is.na(status) || status != 0L) {
    return(NA_character_)
  }
  as.character(out)
}

.rqr_tcsp_finalize_calibration <- function(out) {
  out$package_version <- .rqr_tcsp_package_version()
  out$git_commit <- .rqr_tcsp_git_commit()
  out$git_status_short <- .rqr_tcsp_git_status_short()
  out$calibration_digest <- .rqr_tcsp_digest(out)
  out
}

.rqr_tcsp_scan_histogram_r <- function(n, guaranteed_content, n_sim) {
  counts <- integer(n_sim)
  for (ii in seq_len(n_sim)) {
    counts[[ii]] <- .rqr_tcsp_scan_max_count(
      stats::runif(n), guaranteed_content
    )
  }
  .rqr_tcsp_count_histogram(counts, n)
}

.rqr_tcsp_binom_lower <- function(successes, trials, alpha) {
  successes <- as.integer(successes)
  trials <- as.integer(trials)
  alpha <- as.numeric(alpha)
  lower <- numeric(length(successes))
  positive <- successes > 0L
  lower[positive] <- stats::qbeta(
    alpha,
    successes[positive],
    trials - successes[positive] + 1L
  )
  lower
}

.rqr_tcsp_adaptive_control <- function(n_sim, numerical_confidence,
                                       adaptive_control = list()) {
  if (is.null(adaptive_control)) adaptive_control <- list()
  if (!is.list(adaptive_control)) {
    stop("adaptive_control must be a list.", call. = FALSE)
  }
  n_sim <- .rqr_mt_assert_count(n_sim, "n_sim", 1L)
  initial_n_sim <- .rqr_mt_assert_count(
    adaptive_control$initial_n_sim %||% n_sim,
    "adaptive_control$initial_n_sim", 1L
  )
  batch_n_sim <- .rqr_mt_assert_count(
    adaptive_control$batch_n_sim %||% initial_n_sim,
    "adaptive_control$batch_n_sim", 1L
  )
  default_max <- if (initial_n_sim >= 10000L) {
    max(initial_n_sim, 200000L)
  } else {
    initial_n_sim
  }
  max_n_sim <- .rqr_mt_assert_count(
    adaptive_control$max_n_sim %||% default_max,
    "adaptive_control$max_n_sim", 1L
  )
  if (max_n_sim < initial_n_sim) {
    stop("adaptive_control$max_n_sim cannot be smaller than initial_n_sim.",
         call. = FALSE)
  }
  implied_looks <- 1L + ceiling(max(0L, max_n_sim - initial_n_sim) /
                                  batch_n_sim)
  max_looks <- .rqr_mt_assert_count(
    adaptive_control$max_looks %||% implied_looks,
    "adaptive_control$max_looks", 1L
  )
  stable_looks <- .rqr_mt_assert_count(
    adaptive_control$stable_looks %||% 2L,
    "adaptive_control$stable_looks", 1L
  )
  list(
    initial_n_sim = as.integer(initial_n_sim),
    batch_n_sim = as.integer(batch_n_sim),
    max_n_sim = as.integer(max_n_sim),
    max_looks = as.integer(max_looks),
    stable_looks = as.integer(stable_looks),
    numerical_confidence = numerical_confidence,
    alpha_allocation = "bonferroni_over_feasible_counts_and_adaptive_looks",
    alpha_num = 1 - numerical_confidence
  )
}

.rqr_tcsp_infeasible_probability <- function(n, guaranteed_content,
                                             requested_method) {
  list(
    method = "logical_infeasible_retained_count",
    requested_method = requested_method,
    n = n,
    guaranteed_content = guaranteed_content,
    retained_count = as.integer(n + 1L),
    scan_threshold_max_count = as.integer(n),
    probability_estimate = 1,
    certified_lower_probability = 1,
    numerical_confidence = 1,
    numerical_error_control =
      "Logical event M_n(c) < n + 1; retained_count=n+1 is infeasible as a data interval.",
    simultaneous_numerical_calibration = FALSE,
    exact_terminal_range_calibration = FALSE,
    n_sim = 0L,
    successes = NA_integer_,
    failures = NA_integer_,
    finite_sample_claim_available = FALSE
  )
}

.rqr_tcsp_cp_candidate_bounds <- function(histogram, n, guaranteed_content,
                                          numerical_confidence,
                                          max_looks = 1L, look = 1L) {
  histogram <- as.integer(histogram)
  n <- .rqr_mt_assert_count(n, "n", 1L)
  if (length(histogram) != n + 1L) {
    stop("histogram must have length n + 1.", call. = FALSE)
  }
  n_sim <- sum(histogram)
  if (!is.finite(n_sim) || n_sim < 1L) {
    stop("histogram must contain at least one scan replicate.",
         call. = FALSE)
  }
  numerical_confidence <- .rqr_tcsp_assert_probability(
    numerical_confidence, "numerical_confidence"
  )
  max_looks <- .rqr_mt_assert_count(max_looks, "max_looks", 1L)
  look <- .rqr_mt_assert_count(look, "look", 1L)
  feasible_candidates <- max(n, 1L)
  alpha_num <- 1 - numerical_confidence
  alpha_per_bound <- alpha_num / (feasible_candidates * max_looks)
  cumulative <- cumsum(histogram)
  retained_count <- seq_len(n + 1L)
  threshold <- retained_count - 1L
  successes <- ifelse(threshold <= n, cumulative[threshold + 1L], n_sim)
  empirical <- successes / n_sim
  lower <- .rqr_tcsp_binom_lower(successes, n_sim, alpha_per_bound)
  source <- rep("clopper_pearson_bonferroni_lower_bound",
                length(retained_count))

  terminal <- .rqr_tcsp_terminal_range_probability(
    n, guaranteed_content, "monte_carlo_cp_adaptive"
  )
  lower[[n]] <- terminal$certified_lower_probability
  empirical[[n]] <- terminal$probability_estimate
  successes[[n]] <- NA_integer_
  source[[n]] <- "exact_terminal_range"
  lower[[n + 1L]] <- 1
  empirical[[n + 1L]] <- 1
  successes[[n + 1L]] <- n_sim
  source[[n + 1L]] <- "logical_infeasible_retained_count"

  data.frame(
    retained_count = as.integer(retained_count),
    scan_threshold_max_count = as.integer(threshold),
    cumulative_count = as.integer(successes),
    empirical_cdf = as.numeric(empirical),
    certified_lower_cdf = as.numeric(lower),
    lower_bound_source = source,
    alpha_per_bound = alpha_per_bound,
    look = as.integer(look),
    n_sim = as.integer(n_sim),
    stringsAsFactors = FALSE
  )
}

.rqr_tcsp_probability_from_cp_bounds <- function(bounds, retained_count,
                                                 n, guaranteed_content,
                                                 numerical_confidence,
                                                 control, histogram_digest) {
  k <- .rqr_mt_assert_count(retained_count, "retained_count", 1L)
  row <- bounds[bounds$retained_count == k, , drop = FALSE]
  if (nrow(row) != 1L) {
    stop("retained_count is not represented in the adaptive bounds.",
         call. = FALSE)
  }
  list(
    method = "monte_carlo_cp_adaptive",
    scan_cdf_band_method =
      "clopper_pearson_bonferroni_adaptive_lower_bounds",
    n = n,
    guaranteed_content = guaranteed_content,
    retained_count = k,
    scan_threshold_max_count = row$scan_threshold_max_count[[1L]],
    probability_estimate = row$empirical_cdf[[1L]],
    certified_lower_probability = row$certified_lower_cdf[[1L]],
    numerical_confidence = numerical_confidence,
    numerical_error_control = paste(
      "One-sided Clopper-Pearson lower bounds with Bonferroni",
      "allocation over feasible retained counts and adaptive looks."
    ),
    simultaneous_numerical_calibration = TRUE,
    n_sim = row$n_sim[[1L]],
    successes = row$cumulative_count[[1L]],
    failures = if (is.na(row$cumulative_count[[1L]])) {
      NA_integer_
    } else {
      row$n_sim[[1L]] - row$cumulative_count[[1L]]
    },
    alpha_per_bound = row$alpha_per_bound[[1L]],
    lower_bound_source = row$lower_bound_source[[1L]],
    adaptive_control = control,
    scan_distribution_digest = histogram_digest,
    cdf_band_digest = .rqr_tcsp_digest(bounds),
    exact_terminal_range_calibration =
      identical(row$lower_bound_source[[1L]], "exact_terminal_range"),
    finite_sample_claim_available =
      identical(row$lower_bound_source[[1L]], "exact_terminal_range")
  )
}

.rqr_tcsp_adaptive_calibrate_count <- function(
    n, guaranteed_content, tolerance_confidence,
    n_sim = 20000L, numerical_confidence = 0.999, seed = NULL,
    adaptive_control = list()) {
  terminal <- .rqr_tcsp_terminal_range_probability(
    n, guaranteed_content, "monte_carlo_cp_adaptive"
  )
  control <- .rqr_tcsp_adaptive_control(
    n_sim, numerical_confidence, adaptive_control
  )
  if (terminal$certified_lower_probability < tolerance_confidence) {
    prob <- .rqr_tcsp_infeasible_probability(
      n, guaranteed_content, "monte_carlo_cp_adaptive"
    )
    out <- list(
      schema_version = .rqr_tcsp_schema(),
      calibration_schema_version = "rqrgibbs_tcsp_scan_calibration/1.1.0",
      method = "scan_calibrated_tcsp_mti",
      legacy_method = "scan_calibrated_tcsp_mt_rqr",
      scan_critical_method = "monte_carlo_cp_adaptive",
      n = n,
      guaranteed_content = guaranteed_content,
      tolerance_confidence = tolerance_confidence,
      retained_count = as.integer(n + 1L),
      target_content = NA_real_,
      content_buffer = NA_real_,
      scan_probability = prob,
      terminal_probability = terminal$certified_lower_probability,
      adaptive_control = control,
      finite_sample_claim_available = FALSE,
      asymptotic_claim_available = FALSE,
      infeasible = TRUE,
      structural_status = "terminal_not_certified",
      message = paste(
        "TCSP calibration is structurally infeasible: even the full sample",
        "range does not certify the requested content and confidence."
      )
    )
    return(.rqr_tcsp_finalize_calibration(out))
  }

  rng_kind <- RNGkind()
  restore_rng <- .rqr_mt_seed_scope(seed)
  on.exit(restore_rng(), add = TRUE)
  histogram <- integer(n + 1L)
  names(histogram) <- as.character(0:n)
  total_n_sim <- 0L
  previous_k <- NA_integer_
  stable_count <- 0L
  trace <- vector("list", control$max_looks)
  last_bounds <- NULL
  last_selected <- NA_integer_
  last_point <- NA_integer_

  for (look in seq_len(control$max_looks)) {
    remaining <- control$max_n_sim - total_n_sim
    if (remaining <= 0L) break
    batch <- if (look == 1L) {
      min(control$initial_n_sim, remaining)
    } else {
      min(control$batch_n_sim, remaining)
    }
    histogram <- histogram + .rqr_tcsp_scan_histogram_r(
      n, guaranteed_content, batch
    )
    total_n_sim <- total_n_sim + batch
    bounds <- .rqr_tcsp_cp_candidate_bounds(
      histogram, n, guaranteed_content, numerical_confidence,
      max_looks = control$max_looks, look = look
    )
    certified <- bounds$retained_count[
      is.finite(bounds$certified_lower_cdf) &
        bounds$certified_lower_cdf >= tolerance_confidence
    ]
    point <- bounds$retained_count[
      is.finite(bounds$empirical_cdf) &
        bounds$empirical_cdf >= tolerance_confidence
    ]
    selected_k <- if (length(certified)) certified[[1L]] else NA_integer_
    point_k <- if (length(point)) point[[1L]] else NA_integer_
    if (!is.na(selected_k) && identical(selected_k, previous_k)) {
      stable_count <- stable_count + 1L
    } else if (!is.na(selected_k)) {
      stable_count <- 1L
    } else {
      stable_count <- 0L
    }
    previous_k <- selected_k
    last_bounds <- bounds
    last_selected <- selected_k
    last_point <- point_k
    trace[[look]] <- data.frame(
      look = as.integer(look),
      batch_n_sim = as.integer(batch),
      n_sim_total = as.integer(total_n_sim),
      certified_crossing_k = as.integer(selected_k),
      point_estimate_crossing_k = as.integer(point_k),
      selected_stable_looks = as.integer(stable_count),
      stringsAsFactors = FALSE
    )
    if (!is.na(selected_k) &&
        stable_count >= control$stable_looks &&
        !is.na(point_k) && point_k == selected_k) {
      break
    }
  }

  batch_trace <- do.call(rbind, trace[!vapply(trace, is.null, logical(1L))])
  if (is.null(last_bounds) || is.na(last_selected)) {
    prob <- .rqr_tcsp_infeasible_probability(
      n, guaranteed_content, "monte_carlo_cp_adaptive"
    )
    out <- list(
      schema_version = .rqr_tcsp_schema(),
      calibration_schema_version = "rqrgibbs_tcsp_scan_calibration/1.1.0",
      method = "scan_calibrated_tcsp_mti",
      legacy_method = "scan_calibrated_tcsp_mt_rqr",
      scan_critical_method = "monte_carlo_cp_adaptive",
      n = n,
      guaranteed_content = guaranteed_content,
      tolerance_confidence = tolerance_confidence,
      retained_count = as.integer(n + 1L),
      target_content = NA_real_,
      content_buffer = NA_real_,
      scan_probability = prob,
      scan_candidate_bounds = last_bounds,
      adaptive_batch_trace = batch_trace,
      adaptive_control = control,
      terminal_probability = terminal$certified_lower_probability,
      finite_sample_claim_available = FALSE,
      asymptotic_claim_available = FALSE,
      infeasible = TRUE,
      structural_status = "numerical_budget_exhausted",
      message = paste(
        "TCSP adaptive calibration exhausted the numerical budget without",
        "a certified retained count."
      )
    )
    return(.rqr_tcsp_finalize_calibration(out))
  }

  histogram_digest <- .rqr_tcsp_digest(list(
    histogram = histogram,
    n = n,
    guaranteed_content = guaranteed_content,
    n_sim = total_n_sim,
    seed = seed,
    rng_kind = rng_kind
  ))
  prob <- .rqr_tcsp_probability_from_cp_bounds(
    last_bounds, last_selected, n, guaranteed_content,
    numerical_confidence, control, histogram_digest
  )
  previous_row <- last_bounds[
    last_bounds$retained_count == max(1L, last_selected - 1L),
    , drop = FALSE
  ]
  out <- list(
    schema_version = .rqr_tcsp_schema(),
    calibration_schema_version = "rqrgibbs_tcsp_scan_calibration/1.1.0",
    method = "scan_calibrated_tcsp_mti",
    legacy_method = "scan_calibrated_tcsp_mt_rqr",
    scan_critical_method = "monte_carlo_cp_adaptive",
    n = n,
    guaranteed_content = guaranteed_content,
    tolerance_confidence = tolerance_confidence,
    retained_count = as.integer(last_selected),
    target_content = if (last_selected <= n) last_selected / n else NA_real_,
    content_buffer = if (last_selected <= n) {
      last_selected / n - guaranteed_content
    } else {
      NA_real_
    },
    point_estimate_crossing_k = as.integer(last_point),
    certified_crossing_k = as.integer(last_selected),
    previous_candidate_lower_bound =
      previous_row$certified_lower_cdf[[1L]] %||% NA_real_,
    selected_candidate_lower_bound = prob$certified_lower_probability,
    scan_probability = prob,
    scan_candidate_bounds = last_bounds,
    adaptive_batch_trace = batch_trace,
    adaptive_control = control,
    scan_distribution_digest = histogram_digest,
    cdf_band_digest = .rqr_tcsp_digest(last_bounds),
    terminal_probability = terminal$certified_lower_probability,
    simultaneous_numerical_calibration = TRUE,
    exact_terminal_range_calibration =
      isTRUE(prob$exact_terminal_range_calibration),
    finite_sample_claim_available =
      isTRUE(prob$exact_terminal_range_calibration),
    asymptotic_claim_available = FALSE,
    infeasible = last_selected > n,
    structural_status = if (last_selected > n) {
      "terminal_not_certified"
    } else if (last_selected == n) {
      "terminal_certified"
    } else {
      "interior_certified"
    }
  )
  .rqr_tcsp_finalize_calibration(out)
}

.rqr_tcsp_probability_from_band <- function(band, retained_count) {
  k <- .rqr_mt_assert_count(retained_count, "retained_count", 1L)
  threshold <- min(max(k - 1L, 0L), band$n)
  row <- band$cdf[band$cdf$max_count <= threshold, , drop = FALSE]
  row <- row[nrow(row), , drop = FALSE]
  list(
    method = "monte_carlo_conservative",
    scan_cdf_band_method = band$band_method,
    n = band$n,
    guaranteed_content = band$guaranteed_content,
    retained_count = k,
    scan_threshold_max_count = as.integer(threshold),
    probability_estimate = row$empirical_cdf[[1L]],
    certified_lower_probability = row$certified_lower_cdf[[1L]],
    numerical_confidence = band$numerical_confidence,
    numerical_error_control = band$numerical_error_control,
    simultaneous_numerical_calibration = TRUE,
    n_sim = band$n_sim,
    successes = row$cumulative_count[[1L]],
    failures = band$n_sim - row$cumulative_count[[1L]],
    cdf_band_radius = band$cdf_band_radius,
    scan_distribution_digest = band$scan_distribution_digest,
    cdf_band_digest = band$cdf_band_digest
  )
}

.rqr_tcsp_terminal_range_probability <- function(n, guaranteed_content,
                                                 requested_method) {
  n <- .rqr_mt_assert_count(n, "n", 1L)
  c_target <- .rqr_tcsp_assert_probability(
    guaranteed_content, "guaranteed_content"
  )
  log_c <- log(c_target)
  range_cdf <- exp((n - 1L) * log_c) * (n - (n - 1L) * c_target)
  range_cdf <- min(max(range_cdf, 0), 1)
  probability <- min(max(1 - range_cdf, 0), 1)
  list(
    method = "exact_terminal_range",
    requested_method = requested_method,
    n = n,
    guaranteed_content = c_target,
    retained_count = as.integer(n),
    scan_threshold_max_count = as.integer(n - 1L),
    probability_estimate = probability,
    certified_lower_probability = probability,
    numerical_confidence = 1,
    numerical_error_control =
      "Exact Uniform sample-range law for the terminal retained_count=n scan event.",
    simultaneous_numerical_calibration = FALSE,
    exact_terminal_range_calibration = TRUE,
    n_sim = 0L,
    successes = NA_integer_,
    failures = NA_integer_,
    range_cdf_at_content = range_cdf,
    finite_sample_claim_available = TRUE
  )
}

.rqr_tcsp_posterior_beta_prior_type <- function(posterior_fit) {
  posterior_fit$model_spec$beta_prior_type %||%
    posterior_fit$beta_prior$type %||%
    NA_character_
}

#' Simulate the Uniform TCSP scan-statistic distribution
#'
#' Simulates \eqn{M_n(c)}, the maximum number of iid Uniform(0,1) points
#' falling in any interval of length at most `guaranteed_content`.  The returned
#' histogram is the common Monte Carlo object used for all candidate retained
#' counts in simultaneous scan calibration.
#'
#' @inheritParams rqr_tcsp_scan_probability
#' @return An `rqr_tcsp_scan_distribution` object.
#' @export
rqr_tcsp_scan_distribution <- function(
    n, guaranteed_content, n_sim = 20000L, seed = NULL) {
  n <- .rqr_mt_assert_count(n, "n", 1L)
  c_target <- .rqr_tcsp_assert_probability(
    guaranteed_content, "guaranteed_content"
  )
  n_sim <- .rqr_mt_assert_count(n_sim, "n_sim", 1L)
  rng_kind <- RNGkind()
  restore_rng <- .rqr_mt_seed_scope(seed)
  on.exit(restore_rng(), add = TRUE)
  max_counts <- integer(n_sim)
  for (ii in seq_len(n_sim)) {
    max_counts[[ii]] <- .rqr_tcsp_scan_max_count(stats::runif(n), c_target)
  }
  histogram <- .rqr_tcsp_count_histogram(max_counts, n)
  out <- list(
    schema_version = .rqr_tcsp_schema(),
    method = "uniform_scan_max_count_distribution",
    scan_algorithm_version = "two_pointer_sorted_uniform/1.0.0",
    n = n,
    guaranteed_content = c_target,
    n_sim = n_sim,
    seed = if (is.null(seed)) NA_integer_ else as.integer(seed),
    rng_kind = rng_kind,
    max_count_histogram = histogram,
    max_count_summary = stats::quantile(
      max_counts, c(0, 0.5, 0.9, 0.99, 1), names = TRUE, type = 1
    ),
    max_count_digest = .rqr_tcsp_digest(max_counts)
  )
  out$scan_distribution_digest <- .rqr_tcsp_digest(out)
  class(out) <- c("tcsp_scan_distribution", "rqr_tcsp_scan_distribution",
                  "list")
  out
}

#' Build a simultaneous CDF lower band for Uniform scan calibration
#'
#' Applies a Massart-DKW empirical-CDF band to the simulated distribution of
#' \eqn{M_n(c)}.  The band is simultaneous over all retained-count candidates;
#' it controls numerical calibration error, not sampling uncertainty in the
#' observed response data.
#'
#' @param distribution A scan distribution from
#'   [rqr_tcsp_scan_distribution()].
#' @param numerical_confidence Confidence level for the simultaneous Monte
#'   Carlo CDF band.
#' @return An `rqr_tcsp_scan_cdf_band` object.
#' @export
rqr_tcsp_scan_cdf_band <- function(distribution, numerical_confidence = 0.999) {
  if (!is.list(distribution) ||
      !identical(distribution$method, "uniform_scan_max_count_distribution")) {
    stop("distribution must come from tcsp_scan_distribution() or rqr_tcsp_scan_distribution().",
         call. = FALSE)
  }
  numerical_confidence <- .rqr_tcsp_assert_probability(
    numerical_confidence, "numerical_confidence"
  )
  n <- .rqr_mt_assert_count(distribution$n, "distribution$n", 1L)
  n_sim <- .rqr_mt_assert_count(distribution$n_sim, "distribution$n_sim", 1L)
  counts <- as.integer(distribution$max_count_histogram)
  if (length(counts) != n + 1L || sum(counts) != n_sim) {
    stop("distribution histogram is inconsistent with n and n_sim.",
         call. = FALSE)
  }
  cumulative <- cumsum(counts)
  empirical <- cumulative / n_sim
  radius <- sqrt(log(2 / (1 - numerical_confidence)) / (2 * n_sim))
  lower <- pmax(0, empirical - radius)
  lower[[n + 1L]] <- 1
  cdf <- data.frame(
    max_count = 0:n,
    cumulative_count = cumulative,
    empirical_cdf = empirical,
    certified_lower_cdf = lower
  )
  out <- list(
    schema_version = .rqr_tcsp_schema(),
    method = "uniform_scan_empirical_cdf_band",
    band_method = "massart_dkw_empirical_cdf_lower_band",
    n = n,
    guaranteed_content = distribution$guaranteed_content,
    n_sim = n_sim,
    numerical_confidence = numerical_confidence,
    cdf_band_radius = radius,
    cdf = cdf,
    max_count_histogram = distribution$max_count_histogram,
    scan_distribution_digest = distribution$scan_distribution_digest,
    numerical_error_control =
      "Simultaneous Massart-DKW lower band for the Monte Carlo distribution of the Uniform scan statistic."
  )
  out$cdf_band_digest <- .rqr_tcsp_digest(out)
  class(out) <- c("tcsp_scan_cdf_band", "rqr_tcsp_scan_cdf_band", "list")
  out
}

#' Uniform scan-statistic calibration probability
#'
#' Estimates or bounds \eqn{\Pr\{M_n(c) < k\}}, where \eqn{M_n(c)} is the
#' maximum number of iid Uniform(0,1) points falling in any interval of length
#' at most `guaranteed_content`.  Monte Carlo output is explicitly numerical
#' calibration evidence, not an exact recursion.
#'
#' @param n Sample size.
#' @param guaranteed_content Population content `c`.
#' @param retained_count Candidate retained count `k`.
#' @param method Critical-value method.
#' @param n_sim Number of uniform Monte Carlo replicates for
#'   `"monte_carlo_conservative"`.
#' @param numerical_confidence Confidence level for the simultaneous Monte
#'   Carlo CDF lower band.
#' @param seed Optional simulation seed.
#' @return A list with point and certified lower probabilities.
#' @export
rqr_tcsp_scan_probability <- function(
    n, guaranteed_content, retained_count,
    method = c("monte_carlo_conservative", "monte_carlo_cp_adaptive",
               "dkw_conservative"),
    n_sim = 20000L, numerical_confidence = 0.999, seed = NULL,
    adaptive_control = list()) {
  n <- .rqr_mt_assert_count(n, "n", 1L)
  c_target <- .rqr_tcsp_assert_probability(
    guaranteed_content, "guaranteed_content"
  )
  k <- .rqr_mt_assert_count(retained_count, "retained_count", 1L)
  if (k > n + 1L) stop("retained_count cannot exceed n + 1.", call. = FALSE)
  method <- .rqr_tcsp_assert_method(method)
  numerical_confidence <- .rqr_tcsp_assert_probability(
    numerical_confidence, "numerical_confidence"
  )

  if (method %in% c("monte_carlo_conservative", "monte_carlo_cp_adaptive") &&
      k == n) {
    return(.rqr_tcsp_terminal_range_probability(n, c_target, method))
  }
  if (identical(method, "monte_carlo_cp_adaptive") && k == n + 1L) {
    return(.rqr_tcsp_infeasible_probability(n, c_target, method))
  }

  if (identical(method, "dkw_conservative")) {
    eps <- sqrt(log(2 / (1 - numerical_confidence)) / (2 * n))
    certified <- as.numeric(k / n - 2 * eps > c_target)
    return(list(
      method = method,
      n = n,
      guaranteed_content = c_target,
      retained_count = k,
      probability_estimate = certified,
      certified_lower_probability = certified,
      numerical_confidence = numerical_confidence,
      numerical_error_control =
        "Massart-DKW uniform-band fallback; conservative and not a scan recursion.",
      n_sim = 0L,
      successes = NA_integer_,
      failures = NA_integer_,
      dkw_epsilon = eps
    ))
  }

  if (identical(method, "monte_carlo_cp_adaptive")) {
    distribution <- rqr_tcsp_scan_distribution(
      n, c_target, n_sim = n_sim, seed = seed
    )
    control <- .rqr_tcsp_adaptive_control(
      n_sim, numerical_confidence, adaptive_control
    )
    bounds <- .rqr_tcsp_cp_candidate_bounds(
      distribution$max_count_histogram, n, c_target, numerical_confidence,
      max_looks = control$max_looks, look = 1L
    )
    return(.rqr_tcsp_probability_from_cp_bounds(
      bounds, k, n, c_target, numerical_confidence,
      control, distribution$scan_distribution_digest
    ))
  }

  distribution <- rqr_tcsp_scan_distribution(
    n, c_target, n_sim = n_sim, seed = seed
  )
  band <- rqr_tcsp_scan_cdf_band(
    distribution, numerical_confidence = numerical_confidence
  )
  .rqr_tcsp_probability_from_band(band, k)
}

#' Calibrate a retained count for the TCSP action
#'
#' Returns the smallest retained count whose certified scan probability reaches
#' the requested tolerance confidence under the declared critical-value method.
#'
#' @inheritParams rqr_tcsp_scan_probability
#' @param tolerance_confidence Repeated-sample tolerance confidence `1-alpha`.
#' @return A calibration object.
#' @export
rqr_tcsp_calibrate_count <- function(
    n, guaranteed_content, tolerance_confidence,
    method = c("monte_carlo_conservative", "monte_carlo_cp_adaptive",
               "dkw_conservative"),
    n_sim = 20000L, numerical_confidence = 0.999, seed = NULL,
    adaptive_control = list()) {
  n <- .rqr_mt_assert_count(n, "n", 1L)
  c_target <- .rqr_tcsp_assert_probability(
    guaranteed_content, "guaranteed_content"
  )
  tolerance_confidence <- .rqr_tcsp_assert_probability(
    tolerance_confidence, "tolerance_confidence"
  )
  method <- .rqr_tcsp_assert_method(method)

  if (identical(method, "monte_carlo_cp_adaptive")) {
    return(.rqr_tcsp_adaptive_calibrate_count(
      n = n,
      guaranteed_content = c_target,
      tolerance_confidence = tolerance_confidence,
      n_sim = n_sim,
      numerical_confidence = numerical_confidence,
      seed = seed,
      adaptive_control = adaptive_control
    ))
  }

  if (identical(method, "dkw_conservative")) {
    eps <- sqrt(log(2 / (1 - tolerance_confidence)) / (2 * n))
    k <- as.integer(floor(n * (c_target + 2 * eps)) + 1L)
    if (k > n) {
      stop(
        paste(
          "TCSP calibration is infeasible: the conservative DKW buffer",
          "requires retaining more than n observations."
        ),
        call. = FALSE
      )
    }
    prob <- rqr_tcsp_scan_probability(
      n, c_target, k, method = method,
      numerical_confidence = tolerance_confidence
    )
    out <- list(
      schema_version = .rqr_tcsp_schema(),
      calibration_schema_version = "rqrgibbs_tcsp_scan_calibration/1.0.0",
      method = "scan_calibrated_tcsp_mti",
      legacy_method = "scan_calibrated_tcsp_mt_rqr",
      scan_critical_method = method,
      n = n,
      guaranteed_content = c_target,
      tolerance_confidence = tolerance_confidence,
      retained_count = k,
      target_content = k / n,
      content_buffer = k / n - c_target,
      scan_probability = prob,
      finite_sample_claim_available = FALSE,
      asymptotic_claim_available = FALSE,
      infeasible = FALSE
    )
    return(.rqr_tcsp_finalize_calibration(out))
  }

  distribution <- rqr_tcsp_scan_distribution(
    n, c_target, n_sim = n_sim, seed = seed
  )
  band <- rqr_tcsp_scan_cdf_band(
    distribution, numerical_confidence = numerical_confidence
  )
  for (k in seq_len(n + 1L)) {
    prob <- if (k == n) {
      .rqr_tcsp_terminal_range_probability(n, c_target, method)
    } else {
      .rqr_tcsp_probability_from_band(band, k)
    }
    if (is.finite(prob$certified_lower_probability) &&
        prob$certified_lower_probability >= tolerance_confidence) {
      out <- list(
        schema_version = .rqr_tcsp_schema(),
        calibration_schema_version = "rqrgibbs_tcsp_scan_calibration/1.0.0",
        method = "scan_calibrated_tcsp_mti",
        legacy_method = "scan_calibrated_tcsp_mt_rqr",
        scan_critical_method = method,
        n = n,
        guaranteed_content = c_target,
        tolerance_confidence = tolerance_confidence,
        retained_count = as.integer(k),
        target_content = k / n,
        content_buffer = k / n - c_target,
        scan_probability = prob,
        scan_cdf_band = band,
        scan_distribution_digest = distribution$scan_distribution_digest,
        cdf_band_digest = band$cdf_band_digest,
        simultaneous_numerical_calibration =
          isTRUE(prob$simultaneous_numerical_calibration),
        exact_terminal_range_calibration =
          isTRUE(prob$exact_terminal_range_calibration),
        finite_sample_claim_available =
          isTRUE(prob$finite_sample_claim_available),
        asymptotic_claim_available = FALSE,
        infeasible = k > n
      )
      return(.rqr_tcsp_finalize_calibration(out))
    }
  }
  stop("TCSP calibration failed closed: no retained count reached the certificate.",
       call. = FALSE)
}

#' @rdname rqr_tcsp_calibrate_count
#' @export
rqr_tcsp_scan_count <- rqr_tcsp_calibrate_count

#' Canonical shortest closed k-observation window
#'
#' Selects the first global minimum-width closed interval
#' \eqn{[Y_{(j)},Y_{(j+k-1)}]} containing exactly `retained_count` sorted
#' observations under the repository numerical tie rule.
#'
#' @param y Response sample.
#' @param retained_count Retained observation count.
#' @param na_rm Remove nonfinite observations before scanning.
#' @return An `rqr_tcsp_window` object.
#' @export
rqr_tcsp_shortest_window <- function(y, retained_count, na_rm = FALSE) {
  clean <- .rqr_mt_clean_sample(y, na_rm = na_rm)
  y <- clean$y
  n <- length(y)
  k <- .rqr_mt_assert_count(retained_count, "retained_count", 1L)
  if (k > n) {
    stop("retained_count cannot exceed the number of finite observations.",
         call. = FALSE)
  }
  q <- k / n
  y_sorted <- sort(y)
  starts <- seq_len(n - k + 1L)
  ends <- starts + k - 1L
  widths <- y_sorted[ends] - y_sorted[starts]
  min_width <- min(widths)
  tolerance <- 100 * .Machine$double.eps * max(1, abs(min_width))
  ties <- which(abs(widths - min_width) <= tolerance)
  start <- ties[[1L]]
  end <- start + k - 1L
  retained <- y_sorted[start:end]
  y_mean <- mean(y)
  y_sd <- stats::sd(y)
  out <- list(
    schema_version = .rqr_tcsp_schema(),
    method = "canonical_closed_shortest_k_observation_window",
    sample_size = n,
    retained_count = k,
    target_content = q,
    interval_endpoint_convention = "closed_order_statistic_window",
    formal_tolerance_action =
      "[Y_(j), Y_(j+k-1)] with first global minimum-width tie rule",
    shortest_window_start = as.integer(start),
    shortest_window_end = as.integer(end),
    tie_count = as.integer(length(ties)),
    tie_rule = "first",
    boundary_status = .rqr_mt_boundary_label(start, end, n),
    lower_endpoint = retained[[1L]],
    upper_endpoint = retained[[length(retained)]],
    width = min_width,
    retained_mean = mean(retained),
    training_mean = y_mean,
    training_sd = y_sd,
    delta_raw = mean(retained) - y_mean,
    delta_standardized = (mean(retained) - y_mean) / y_sd,
    n_removed = clean$n_removed,
    global_shortest_verified = TRUE
  )
  class(out) <- c("tcsp_window", "rqr_tcsp_window", "list")
  out
}

#' Extract TCSP mean tilt from a shortest window
#'
#' @param window A TCSP window object.
#' @return A list with raw and standardized tilt.
#' @export
rqr_tcsp_tilt_from_window <- function(window) {
  if (!is.list(window) || is.null(window$delta_raw) ||
      is.null(window$delta_standardized)) {
    stop("window must be an rqr_tcsp_window-like object.", call. = FALSE)
  }
  list(
    delta_raw = as.numeric(window$delta_raw)[1L],
    delta_standardized = as.numeric(window$delta_standardized)[1L],
    retained_mean = as.numeric(window$retained_mean)[1L],
    training_mean = as.numeric(window$training_mean)[1L],
    target_content = as.numeric(window$target_content)[1L],
    retained_count = as.integer(window$retained_count)
  )
}

#' Boundary-continuation target for fixed-target MTI-ECM
#'
#' The strict scan-aligned MTI-ECM target uses `q = retained_count / n`.
#' When the scan calibration selects the full sample range, this value is one
#' and the MTI/RQR fixed-target computation is singular.  This helper leaves
#' interior scan targets unchanged and provides a predeclared interior
#' continuation for the full-range case.
#'
#' @param n Sample size.
#' @param retained_count Scan-calibrated retained count.
#' @param guaranteed_content Requested population content.
#' @param epsilon_min Lower bound for the boundary offset from one.
#' @return A boundary-continuation target object.
#' @export
rqr_tcsp_mti_boundary_target <- function(
    n, retained_count, guaranteed_content, epsilon_min = 1e-4) {
  n <- .rqr_mt_assert_count(n, "n", 1L)
  k <- .rqr_mt_assert_count(retained_count, "retained_count", 1L)
  c_target <- .rqr_tcsp_assert_probability(
    guaranteed_content, "guaranteed_content"
  )
  epsilon_min <- as.numeric(epsilon_min)[1L]
  if (!is.finite(epsilon_min) || epsilon_min <= 0) {
    stop("epsilon_min must be finite and positive.", call. = FALSE)
  }
  if (k > n) {
    stop("retained_count cannot exceed n for MTI boundary continuation.",
         call. = FALSE)
  }
  scan_target <- k / n
  if (k < n) {
    out <- list(
      schema_version = "rqrgibbs_tcsp_mti_boundary_target/1.0.0",
      rule = "scan_count_exact",
      n = n,
      retained_count = k,
      guaranteed_content = c_target,
      scan_target_content = scan_target,
      ecm_target_content = scan_target,
      epsilon = NA_real_,
      boundary_continuation = FALSE,
      certificate_scope = "scan_action_only"
    )
  } else {
    epsilon <- min(max(1 / (2 * n), epsilon_min), (1 - c_target) / 2)
    ecm_target <- 1 - epsilon
    if (!is.finite(ecm_target) || ecm_target <= c_target ||
        ecm_target >= 1) {
      stop("Boundary continuation did not produce an interior target.",
           call. = FALSE)
    }
    out <- list(
      schema_version = "rqrgibbs_tcsp_mti_boundary_target/1.0.0",
      rule = "half_step_full_range_continuation",
      n = n,
      retained_count = k,
      guaranteed_content = c_target,
      scan_target_content = scan_target,
      ecm_target_content = ecm_target,
      epsilon = epsilon,
      boundary_continuation = TRUE,
      certificate_scope = "scan_action_only"
    )
  }
  out$target_digest <- .rqr_tcsp_digest(out)
  class(out) <- c("tcsp_mti_boundary_target",
                  "rqr_tcsp_mti_boundary_target", "list")
  out
}

#' Fractional empirical MTI tilt from adjacent shortest windows
#'
#' Computes the empirical mean tilt associated with an interior target content.
#' Integer retained-count targets reproduce the usual shortest-window tilt.
#' Fractional targets linearly interpolate the tilts from the adjacent
#' shortest-window counts.
#'
#' @param y Response sample.
#' @param target_content Interior fitted content.
#' @param na_rm Remove nonfinite observations before scanning.
#' @return A fractional empirical tilt object.
#' @export
rqr_tcsp_fractional_tilt <- function(y, target_content, na_rm = FALSE) {
  clean <- .rqr_mt_clean_sample(y, na_rm = na_rm)
  y <- clean$y
  n <- length(y)
  q <- .rqr_tcsp_assert_probability(target_content, "target_content")
  raw_count <- n * q
  nearest <- round(raw_count)
  integer_tol <- 100 * .Machine$double.eps * max(1, abs(raw_count))
  if (abs(raw_count - nearest) <= integer_tol &&
      nearest >= 1L && nearest <= n) {
    window <- rqr_tcsp_shortest_window(y, as.integer(nearest), na_rm = FALSE)
    tilt <- rqr_tcsp_tilt_from_window(window)
    out <- list(
      schema_version = "rqrgibbs_tcsp_fractional_tilt/1.0.0",
      rule = "integer_shortest_window",
      sample_size = n,
      target_content = q,
      lower_count = as.integer(nearest),
      upper_count = as.integer(nearest),
      interpolation_weight_lower = 1,
      interpolation_weight_upper = 0,
      delta_raw = tilt$delta_raw,
      delta_standardized = tilt$delta_standardized,
      retained_mean = tilt$retained_mean,
      training_mean = tilt$training_mean,
      n_removed = clean$n_removed,
      lower_window = window,
      upper_window = window
    )
    out$tilt_digest <- .rqr_tcsp_digest(out)
    class(out) <- c("tcsp_fractional_tilt", "rqr_tcsp_fractional_tilt",
                    "list")
    return(out)
  }

  lower_count <- max(1L, floor(raw_count))
  upper_count <- min(n, ceiling(raw_count))
  if (lower_count == upper_count) {
    stop("Fractional tilt target could not identify adjacent counts.",
         call. = FALSE)
  }
  lower_q <- lower_count / n
  upper_q <- upper_count / n
  weight_lower <- (upper_q - q) / (upper_q - lower_q)
  weight_upper <- 1 - weight_lower
  lower_window <- rqr_tcsp_shortest_window(y, lower_count, na_rm = FALSE)
  upper_window <- rqr_tcsp_shortest_window(y, upper_count, na_rm = FALSE)
  lower_tilt <- rqr_tcsp_tilt_from_window(lower_window)
  upper_tilt <- rqr_tcsp_tilt_from_window(upper_window)
  delta_raw <- weight_lower * lower_tilt$delta_raw +
    weight_upper * upper_tilt$delta_raw
  y_sd <- stats::sd(y)
  out <- list(
    schema_version = "rqrgibbs_tcsp_fractional_tilt/1.0.0",
    rule = "linear_interpolation_between_adjacent_shortest_windows",
    sample_size = n,
    target_content = q,
    lower_count = as.integer(lower_count),
    upper_count = as.integer(upper_count),
    lower_content = lower_q,
    upper_content = upper_q,
    interpolation_weight_lower = weight_lower,
    interpolation_weight_upper = weight_upper,
    delta_raw = delta_raw,
    delta_standardized = delta_raw / y_sd,
    retained_mean = mean(y) + delta_raw,
    training_mean = mean(y),
    n_removed = clean$n_removed,
    lower_window = lower_window,
    upper_window = upper_window
  )
  out$tilt_digest <- .rqr_tcsp_digest(out)
  class(out) <- c("tcsp_fractional_tilt", "rqr_tcsp_fractional_tilt", "list")
  out
}

.rqr_tcsp_validate_mcmc_request <- function(fit_mcmc, mcmc_args) {
  if (!isTRUE(fit_mcmc)) return(invisible(TRUE))
  if (!is.list(mcmc_args)) stop("mcmc_args must be a list.", call. = FALSE)
  reserved <- c(
    "y", "X", "coverage_level", "mean_tilt",
    "learning_rate", "learning_rate_mode", "beta_prior_obj",
    "response_likelihood"
  )
  supplied_reserved <- intersect(names(mcmc_args), reserved)
  if (length(supplied_reserved)) {
    stop(
      paste(
        "TCSP fixed-target MCMC reserves these mcmc_args fields:",
        paste(supplied_reserved, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  mode <- mcmc_args$learning_rate_mode %||% "fixed_rate"
  if (!identical(mode, "fixed_rate")) {
    stop("TCSP nonzero tilt MCMC requires learning_rate_mode='fixed_rate'.",
         call. = FALSE)
  }
  prior <- mcmc_args$beta_prior_obj %||%
    beta_prior("ridge", ridge = list(tau2 = 1e4))
  if (!is.list(prior) || !identical(prior$type, "ridge")) {
    stop("TCSP nonzero tilt MCMC currently requires a ridge beta prior.",
         call. = FALSE)
  }
  invisible(TRUE)
}

.rqr_tcsp_validate_ecm_request <- function(fit_ecm, ecm_args) {
  if (!isTRUE(fit_ecm)) return(invisible(TRUE))
  if (!is.list(ecm_args)) stop("ecm_args must be a list.", call. = FALSE)
  reserved <- c(
    "y", "X", "coverage_level", "mean_tilt",
    "learning_rate", "beta_prior_obj", "response_likelihood"
  )
  supplied_reserved <- intersect(names(ecm_args), reserved)
  if (length(supplied_reserved)) {
    stop(
      paste(
        "TCSP fixed-target ECM reserves these ecm_args fields:",
        paste(supplied_reserved, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Fit the univariate TCSP-MTI target
#'
#' Calibrates the retained count, selects the formal shortest closed-window
#' tolerance action, freezes `q` and `delta`, and optionally fits deterministic
#' ECM and/or intercept-only fixed-rate ridge generalized posterior summaries.
#' The scan action is the tolerance object; ECM and posterior summaries are
#' separate loss-based fixed-target plug-in summaries.  Use
#' [tcsp_hybrid_bayes_fit()] for a full-distribution Bayesian
#' content-probability action.
#'
#' @param y Response sample.
#' @param guaranteed_content Minimum population content `c`.
#' @param tolerance_confidence Repeated-sample tolerance confidence `1-alpha`.
#' @param scan_method Critical-value method.
#' @param fit_mcmc Fit the intercept-only generalized posterior.
#' @param fit_ecm Fit the deterministic intercept-only fixed-target ECM mode.
#' @param learning_rate Fixed generalized-Bayes learning rate `omega`.
#' @param mcmc_args Optional arguments passed to [rqr_mcmc_fit()].
#' @param ecm_args Optional arguments passed to [rqr_ecm_fit()].
#' @inheritParams rqr_tcsp_scan_probability
#' @return A `tcsp_mti_fit` object, with legacy `rqr_tcsp_fit` class retained.
#' @export
rqr_tcsp_fit_univariate <- function(
    y, guaranteed_content, tolerance_confidence,
    scan_method = c("monte_carlo_conservative", "monte_carlo_cp_adaptive",
                    "dkw_conservative"),
    n_sim = 20000L, numerical_confidence = 0.999, seed = NULL,
    adaptive_control = list(),
    fit_mcmc = FALSE, fit_ecm = FALSE, learning_rate = 1,
    mcmc_args = list(), ecm_args = list(),
    na_rm = FALSE) {
  clean <- .rqr_mt_clean_sample(y, na_rm = na_rm)
  y <- clean$y
  learning_rate <- as.numeric(learning_rate)[1L]
  if (!is.finite(learning_rate) || learning_rate <= 0) {
    stop("learning_rate must be a finite positive scalar.", call. = FALSE)
  }
  calibration <- rqr_tcsp_calibrate_count(
    n = length(y),
    guaranteed_content = guaranteed_content,
    tolerance_confidence = tolerance_confidence,
    method = scan_method,
    n_sim = n_sim,
    numerical_confidence = numerical_confidence,
    seed = seed,
    adaptive_control = adaptive_control
  )
  if (isTRUE(calibration$infeasible) || calibration$retained_count > length(y)) {
    stop("TCSP calibration is infeasible for this sample size and target.",
         call. = FALSE)
  }
  window <- rqr_tcsp_shortest_window(
    y, retained_count = calibration$retained_count, na_rm = FALSE
  )
  tilt <- rqr_tcsp_tilt_from_window(window)
  .rqr_tcsp_validate_mcmc_request(fit_mcmc, mcmc_args)
  .rqr_tcsp_validate_ecm_request(fit_ecm, ecm_args)
  if (isTRUE(getOption("rqrgibbs.warn_tcsp_plugin", FALSE)) &&
      (isTRUE(fit_mcmc) || isTRUE(fit_ecm))) {
    warning(
      paste(
        "TCSP-MTI MCMC/ECM summaries are fixed-target plug-in uncertainty summaries.",
        "They are not unconditional Bayesian uncertainty summaries for the population shortest interval;",
        "use tcsp_hybrid_bayes_fit() for that role."
      ),
      call. = FALSE
    )
  }

  contract <- list(
    schema_version = .rqr_tcsp_schema(),
    method = "scan_calibrated_tolerance_calibrated_shortest_path_mti",
    legacy_method = "scan_calibrated_tolerance_calibrated_shortest_path_mt_rqr",
    uq_scope = "fixed_target_plugin",
    lifecycle_status = "superseded_for_unconditional_shortest_interval_uq",
    authoritative_full_distribution_uq_function = "tcsp_hybrid_bayes_fit",
    posterior_endpoint_coverage_claim_available = FALSE,
    response_likelihood = FALSE,
    generalized_bayes = TRUE,
    sample_size = length(y),
    guaranteed_content = calibration$guaranteed_content,
    tolerance_confidence = calibration$tolerance_confidence,
    target_content = calibration$target_content,
    content_buffer = calibration$content_buffer,
    retained_count = calibration$retained_count,
    scan_critical_method = calibration$scan_critical_method,
    scan_critical_value = calibration$retained_count,
    scan_numerical_error_control =
      calibration$scan_probability$numerical_error_control,
    formal_tolerance_action = window$formal_tolerance_action,
    interval_endpoint_convention = window$interval_endpoint_convention,
    shortest_window_start = window$shortest_window_start,
    shortest_window_end = window$shortest_window_end,
    tie_count = window$tie_count,
    tie_rule = window$tie_rule,
    boundary_status = window$boundary_status,
    lower_endpoint = window$lower_endpoint,
    upper_endpoint = window$upper_endpoint,
    width = window$width,
    retained_mean = window$retained_mean,
    delta_raw = tilt$delta_raw,
    delta_standardized = tilt$delta_standardized,
    learning_rate = learning_rate,
    learning_rate_mode = "fixed_rate",
    prior_type = NA_character_,
    posterior_model_spec_digest = NA_character_,
    posterior_summary_action = "not_formal_tolerance_action",
    ecm_model_spec_digest = NA_character_,
    ecm_map_action = "not_formal_tolerance_action",
    ecm_conditional_on_selected_tcsp_target = isTRUE(fit_ecm),
    ecm_fit_available = FALSE,
    posterior_fit_available = FALSE,
    engine_unavailable_reasons = list(),
    global_shortest_verified = TRUE,
    assumptions_passed = c("iid_continuous_required_by_theory_not_tested_by_code"),
    assumptions_failed = character(0),
    finite_sample_claim_available = FALSE,
    asymptotic_claim_available = FALSE,
    response_scale_description = "original response scale",
    root_label_contract = "complete roots are exchangeable; endpoints by sorting",
    provenance_digest = .rqr_tcsp_digest(list(
      calibration = calibration,
      window = window,
      posterior_model_spec = NULL
    ))
  )
  make_out <- function(posterior_fit = NULL, ecm_fit = NULL) {
    out <- list(
      contract = contract,
      calibration = calibration,
      window = window,
      posterior_fit = posterior_fit,
      ecm_fit = ecm_fit
    )
    class(out) <- c("tcsp_mti_fit", "rqr_tcsp_fit", "list")
    out
  }

  posterior_fit <- NULL
  ecm_fit <- NULL
  if (calibration$target_content >= 1) {
    if (isTRUE(fit_ecm)) {
      contract$engine_unavailable_reasons$ecm <- paste(
        "TCSP empirical range action has target_content >= 1;",
        "rqr_ecm_fit() requires coverage_level in (0, 1)."
      )
    }
    if (isTRUE(fit_mcmc)) {
      contract$engine_unavailable_reasons$posterior <- paste(
        "TCSP empirical range action has target_content >= 1;",
        "rqr_mcmc_fit() requires coverage_level in (0, 1)."
      )
    }
    contract$provenance_digest <- .rqr_tcsp_digest(list(
      calibration = calibration,
      window = window,
      posterior_model_spec = NULL,
      ecm_model_spec = NULL,
      engine_unavailable_reasons = contract$engine_unavailable_reasons
    ))
    return(make_out(NULL, NULL))
  }

  if (isTRUE(fit_ecm)) {
    args <- utils::modifyList(
      list(
        y = y,
        X = matrix(1, length(y), 1L, dimnames = list(NULL, "(Intercept)")),
        coverage_level = calibration$target_content,
        learning_rate = learning_rate,
        mean_tilt = tilt$delta_raw,
        beta_prior_obj = beta_prior("ridge", ridge = list(tau2 = 1e4))
      ),
      ecm_args
    )
    ecm_fit <- do.call(rqr_ecm_fit, args)
    if (!isTRUE(all.equal(ecm_fit$model_spec$coverage_level,
                          calibration$target_content, tolerance = 0))) {
      stop("TCSP ECM target audit failed: coverage_level drifted.",
           call. = FALSE)
    }
    if (!isTRUE(all.equal(ecm_fit$model_spec$fixed_learning_rate,
                          learning_rate, tolerance = 0))) {
      stop("TCSP ECM target audit failed: fixed learning_rate drifted.",
           call. = FALSE)
    }
    if (isTRUE(ecm_fit$model_spec$response_likelihood)) {
      stop("TCSP ECM target audit failed: response_likelihood is TRUE.",
           call. = FALSE)
    }
    if (!identical(ecm_fit$model_spec$beta_prior_type, "ridge")) {
      stop("TCSP ECM target audit failed: beta prior is not ridge.",
           call. = FALSE)
    }
    if (!isTRUE(all.equal(unique(ecm_fit$model_spec$mean_tilt),
                          tilt$delta_raw, tolerance = 1e-12))) {
      stop("TCSP ECM target audit failed: mean_tilt drifted.",
           call. = FALSE)
    }
    if (!is.matrix(ecm_fit$X) || ncol(ecm_fit$X) != 1L ||
        !identical(colnames(ecm_fit$X), "(Intercept)")) {
      stop("TCSP ECM target audit failed: design is not intercept-only.",
           call. = FALSE)
    }
    contract$ecm_fit_available <- TRUE
    contract$ecm_model_spec_digest <- .rqr_tcsp_digest(ecm_fit$model_spec)
    contract$prior_type <- ecm_fit$model_spec$beta_prior_type
  }

  if (isTRUE(fit_mcmc)) {
    args <- utils::modifyList(
      list(
        y = y,
        X = matrix(1, length(y), 1L, dimnames = list(NULL, "(Intercept)")),
        coverage_level = calibration$target_content,
        learning_rate = learning_rate,
        mean_tilt = tilt$delta_raw,
        learning_rate_mode = "fixed_rate",
        beta_prior_obj = beta_prior("ridge", ridge = list(tau2 = 1e4))
      ),
      mcmc_args
    )
    posterior_fit <- do.call(rqr_mcmc_fit, args)
    if (!isTRUE(all.equal(posterior_fit$model_spec$coverage_level,
                          calibration$target_content, tolerance = 0))) {
      stop("TCSP MCMC target audit failed: coverage_level drifted.",
           call. = FALSE)
    }
    if (!isTRUE(all.equal(posterior_fit$model_spec$fixed_learning_rate,
                          learning_rate, tolerance = 0))) {
      stop("TCSP MCMC target audit failed: fixed learning_rate drifted.",
           call. = FALSE)
    }
    if (!identical(posterior_fit$model_spec$learning_rate_mode,
                   "fixed_rate")) {
      stop("TCSP MCMC target audit failed: learning_rate_mode drifted.",
           call. = FALSE)
    }
    if (isTRUE(posterior_fit$model_spec$response_likelihood)) {
      stop("TCSP MCMC target audit failed: response_likelihood is TRUE.",
           call. = FALSE)
    }
    if (!identical(.rqr_tcsp_posterior_beta_prior_type(posterior_fit),
                   "ridge")) {
      stop("TCSP MCMC target audit failed: beta prior is not ridge.",
           call. = FALSE)
    }
    if (!isTRUE(all.equal(unique(posterior_fit$model_spec$mean_tilt),
                          tilt$delta_raw, tolerance = 1e-12))) {
      stop("TCSP MCMC target audit failed: mean_tilt drifted.",
           call. = FALSE)
    }
    if (!is.matrix(posterior_fit$X) || ncol(posterior_fit$X) != 1L ||
        !identical(colnames(posterior_fit$X), "(Intercept)")) {
      stop("TCSP MCMC target audit failed: design is not intercept-only.",
           call. = FALSE)
    }
    contract$prior_type <- .rqr_tcsp_posterior_beta_prior_type(posterior_fit)
    contract$posterior_model_spec_digest <- .rqr_tcsp_digest(
      posterior_fit$model_spec
    )
    contract$posterior_fit_available <- TRUE
  }
  contract$provenance_digest <- .rqr_tcsp_digest(list(
    calibration = calibration,
    window = window,
    posterior_model_spec = if (is.null(posterior_fit)) NULL else
      posterior_fit$model_spec,
    ecm_model_spec = if (is.null(ecm_fit)) NULL else ecm_fit$model_spec
  ))
  make_out(posterior_fit, ecm_fit)
}

#' @rdname rqr_tcsp_fit_univariate
#' @export
rqr_tcsp_plugin_fit_univariate <- function(...) {
  rqr_tcsp_fit_univariate(...)
}

#' Predict a TCSP path start by nested expansion
#'
#' @param previous_window Previous TCSP window.
#' @param next_retained_count Next retained count.
#' @param n Sample size.
#' @return A start/end prediction.
#' @export
rqr_tcsp_predict_next_start <- function(previous_window, next_retained_count,
                                        n = previous_window$sample_size) {
  k_next <- .rqr_mt_assert_count(next_retained_count, "next_retained_count", 1L)
  n <- .rqr_mt_assert_count(n, "n", 1L)
  start <- as.integer(previous_window$shortest_window_start %||% previous_window$start_index)
  end <- as.integer(previous_window$shortest_window_end %||% previous_window$end_index)
  if (!is.finite(start) || !is.finite(end) || start < 1L || end > n) {
    stop("previous_window must contain valid start/end indices.", call. = FALSE)
  }
  while (end - start + 1L < k_next) {
    can_left <- start > 1L
    can_right <- end < n
    if (can_left) {
      start <- start - 1L
    } else if (can_right) {
      end <- end + 1L
    } else {
      break
    }
    if (end - start + 1L >= k_next) break
    if (can_right) end <- end + 1L
  }
  list(
    predicted_start = as.integer(start),
    predicted_end = as.integer(end),
    retained_count = as.integer(end - start + 1L),
    requested_retained_count = k_next,
    mode = "nested_left_first_expansion"
  )
}

#' Locally correct a TCSP path prediction
#'
#' Searches a trust region around a predicted start.  The returned object
#' records regret against the global action when `global_window` is supplied.
#'
#' @param y Response sample.
#' @param retained_count Retained count.
#' @param predicted_start Predicted start index.
#' @param trust_radius Initial trust-region radius.
#' @param global_window Optional globally verified TCSP window.
#' @return A local correction object.
#' @export
rqr_tcsp_local_correct <- function(y, retained_count, predicted_start,
                                   trust_radius = 2L, global_window = NULL) {
  clean <- .rqr_mt_clean_sample(y, na_rm = FALSE)
  y_sorted <- sort(clean$y)
  n <- length(y_sorted)
  k <- .rqr_mt_assert_count(retained_count, "retained_count", 1L)
  if (k > n) stop("retained_count cannot exceed n.", call. = FALSE)
  predicted_start <- .rqr_mt_assert_count(
    predicted_start, "predicted_start", 1L
  )
  max_start <- n - k + 1L
  if (predicted_start > max_start) predicted_start <- max_start
  trust_radius <- .rqr_mt_assert_count(trust_radius, "trust_radius", 0L)
  radius <- trust_radius
  repeat {
    starts <- seq.int(
      max(1L, predicted_start - radius),
      min(max_start, predicted_start + radius)
    )
    ends <- starts + k - 1L
    widths <- y_sorted[ends] - y_sorted[starts]
    best <- which.min(widths)
    start <- starts[[best]]
    boundary_hit <- start == min(starts) || start == max(starts)
    if (!boundary_hit || radius >= max_start) break
    radius <- min(max_start, max(1L, 2L * radius + 1L))
  }
  local_width <- y_sorted[[start + k - 1L]] - y_sorted[[start]]
  global_start <- if (!is.null(global_window)) {
    as.integer(global_window$shortest_window_start)
  } else {
    as.integer(rqr_tcsp_shortest_window(y_sorted, k)$shortest_window_start)
  }
  global_width <- y_sorted[[global_start + k - 1L]] - y_sorted[[global_start]]
  list(
    local_start = as.integer(start),
    local_end = as.integer(start + k - 1L),
    local_width = local_width,
    expanded_radius = as.integer(radius),
    boundary_hit = isTRUE(boundary_hit),
    global_start = global_start,
    global_width = global_width,
    local_regret = local_width - global_width,
    globally_verified = abs(local_width - global_width) <=
      100 * .Machine$double.eps * max(1, abs(global_width))
  )
}

#' Trace TCSP actions across content levels
#'
#' Each formal action is globally verified.  Local continuation diagnostics are
#' recorded but are not used as a substitute for the global action.
#'
#' @param y Response sample.
#' @param guaranteed_contents Vector of target contents.
#' @inheritParams rqr_tcsp_fit_univariate
#' @return An `rqr_tcsp_path` object.
#' @export
rqr_tcsp_path <- function(
    y, guaranteed_contents, tolerance_confidence,
    scan_method = c("monte_carlo_conservative", "monte_carlo_cp_adaptive",
                    "dkw_conservative"),
    n_sim = 20000L, numerical_confidence = 0.999, seed = NULL,
    adaptive_control = list(),
    na_rm = FALSE) {
  clean <- .rqr_mt_clean_sample(y, na_rm = na_rm)
  y <- clean$y
  contents <- as.numeric(guaranteed_contents)
  if (!length(contents) || any(!is.finite(contents)) ||
      any(contents <= 0 | contents >= 1)) {
    stop("guaranteed_contents must contain finite values in (0, 1).",
         call. = FALSE)
  }
  ord <- order(contents)
  contents <- contents[ord]
  rows <- vector("list", length(contents))
  diagnostics <- vector("list", length(contents))
  previous <- NULL
  for (ii in seq_along(contents)) {
    fit <- rqr_tcsp_fit_univariate(
      y = y,
      guaranteed_content = contents[[ii]],
      tolerance_confidence = tolerance_confidence,
      scan_method = scan_method,
      n_sim = n_sim,
      numerical_confidence = numerical_confidence,
      seed = if (is.null(seed)) NULL else as.integer(seed) + ii,
      adaptive_control = adaptive_control,
      fit_mcmc = FALSE
    )
    win <- fit$window
    pred <- if (is.null(previous)) {
      NULL
    } else {
      rqr_tcsp_predict_next_start(previous, fit$contract$retained_count,
                                  n = length(y))
    }
    local <- if (is.null(pred)) {
      NULL
    } else {
      rqr_tcsp_local_correct(
        y, fit$contract$retained_count, pred$predicted_start,
        global_window = win
      )
    }
    rows[[ii]] <- data.frame(
      guaranteed_content = fit$contract$guaranteed_content,
      target_content = fit$contract$target_content,
      retained_count = fit$contract$retained_count,
      content_buffer = fit$contract$content_buffer,
      start = win$shortest_window_start,
      end = win$shortest_window_end,
      width = win$width,
      delta_raw = win$delta_raw,
      delta_standardized = win$delta_standardized,
      local_regret = if (is.null(local)) NA_real_ else local$local_regret,
      global_shortest_verified = TRUE
    )
    diagnostics[[ii]] <- list(prediction = pred, local_correction = local)
    previous <- win
  }
  out <- list(
    schema_version = .rqr_tcsp_path_schema(),
    method = "globally_verified_tcsp_mti_content_path",
    legacy_method = "globally_verified_tcsp_content_path",
    tolerance_confidence = tolerance_confidence,
    scan_method = .rqr_tcsp_assert_method(scan_method),
    path = do.call(rbind, rows),
    diagnostics = diagnostics
  )
  class(out) <- c("tcsp_mti_path", "rqr_tcsp_path", "list")
  out
}

#' TCSP scan-calibration boundary map
#'
#' Computes scan-only retained-count diagnostics over a grid of sample sizes,
#' contents, and tolerance confidences.  This is a calibration audit: it does
#' not fit MTI, Gibbs, ECM, or Bayesian response-distribution summaries.
#'
#' @param sample_sizes Integer sample sizes.
#' @param guaranteed_contents Population contents.
#' @param tolerance_confidences Tolerance confidence levels.
#' @param method Scan calibration method.
#' @inheritParams rqr_tcsp_calibrate_count
#' @return A data frame with one row per calibration cell.
#' @export
rqr_tcsp_calibration_boundary_map <- function(
    sample_sizes, guaranteed_contents, tolerance_confidences,
    method = c("monte_carlo_cp_adaptive", "monte_carlo_conservative",
               "dkw_conservative"),
    n_sim = 20000L, numerical_confidence = 0.999, seed = NULL,
    adaptive_control = list()) {
  method <- .rqr_tcsp_assert_method(method)
  sample_sizes <- as.integer(sample_sizes)
  guaranteed_contents <- as.numeric(guaranteed_contents)
  tolerance_confidences <- as.numeric(tolerance_confidences)
  if (!length(sample_sizes) || !length(guaranteed_contents) ||
      !length(tolerance_confidences)) {
    stop("sample_sizes, guaranteed_contents, and tolerance_confidences cannot be empty.",
         call. = FALSE)
  }
  rows <- list()
  index <- 0L
  counter <- 0L
  for (n in sample_sizes) {
    n <- .rqr_mt_assert_count(n, "sample_sizes", 1L)
    for (c_target in guaranteed_contents) {
      c_target <- .rqr_tcsp_assert_probability(
        c_target, "guaranteed_contents"
      )
      terminal <- .rqr_tcsp_terminal_range_probability(
        n, c_target, method
      )
      for (gamma in tolerance_confidences) {
        gamma <- .rqr_tcsp_assert_probability(
          gamma, "tolerance_confidences"
        )
        counter <- counter + 1L
        cell_seed <- if (is.null(seed)) NULL else as.integer(seed) + counter
        cal <- tryCatch(
          rqr_tcsp_calibrate_count(
            n = n,
            guaranteed_content = c_target,
            tolerance_confidence = gamma,
            method = method,
            n_sim = n_sim,
            numerical_confidence = numerical_confidence,
            seed = cell_seed,
            adaptive_control = adaptive_control
          ),
          error = function(e) {
            list(
              scan_critical_method = method,
              n = n,
              guaranteed_content = c_target,
              tolerance_confidence = gamma,
              retained_count = as.integer(n + 1L),
              target_content = NA_real_,
              content_buffer = NA_real_,
              scan_probability = list(
                probability_estimate = NA_real_,
                certified_lower_probability = NA_real_,
                n_sim = as.integer(n_sim)
              ),
              infeasible = TRUE,
              structural_status = "calibration_error",
              message = conditionMessage(e),
              calibration_digest = NA_character_
            )
          }
        )
        retained <- as.integer(cal$retained_count %||% NA_integer_)
        infeasible <- isTRUE(cal$infeasible) || is.na(retained) ||
          retained > n
        status <- cal$structural_status %||%
          if (terminal$certified_lower_probability < gamma) {
            "terminal_not_certified"
          } else if (infeasible) {
            "numerical_budget_exhausted"
          } else if (retained == n) {
            "terminal_certified"
          } else {
            "interior_certified"
          }
        index <- index + 1L
        rows[[index]] <- data.frame(
          n = as.integer(n),
          guaranteed_content = c_target,
          tolerance_confidence = gamma,
          scan_critical_method = cal$scan_critical_method %||% method,
          retained_count = retained,
          target_content = if (!infeasible) {
            cal$target_content %||% (retained / n)
          } else {
            NA_real_
          },
          content_buffer = if (!infeasible) {
            cal$content_buffer %||% (retained / n - c_target)
          } else {
            NA_real_
          },
          certified_lower_probability =
            cal$scan_probability$certified_lower_probability %||% NA_real_,
          point_estimate_probability =
            cal$scan_probability$probability_estimate %||% NA_real_,
          n_sim_total = cal$scan_probability$n_sim %||% NA_integer_,
          terminal_exact_probability =
            terminal$certified_lower_probability,
          terminal_certifies =
            terminal$certified_lower_probability + 1e-12 >= gamma,
          interior_window_available = !infeasible && retained < n,
          infeasible = infeasible,
          structural_status = status,
          calibration_digest = cal$calibration_digest %||% NA_character_,
          message = cal$message %||% "",
          stringsAsFactors = FALSE
        )
      }
    }
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' TCSP scan-calibration seed-stability audit
#'
#' Repeats scan-only calibration over independent seeds and records the selected
#' retained count for each cell.  This diagnoses Monte Carlo calibration
#' stability without fitting interval models.
#'
#' @param seeds Integer seeds.
#' @inheritParams rqr_tcsp_calibration_boundary_map
#' @return A data frame with one row per cell and seed.
#' @export
rqr_tcsp_calibration_stability <- function(
    sample_sizes, guaranteed_contents, tolerance_confidences, seeds,
    method = c("monte_carlo_cp_adaptive", "monte_carlo_conservative",
               "dkw_conservative"),
    n_sim = 20000L, numerical_confidence = 0.999,
    adaptive_control = list()) {
  seeds <- as.integer(seeds)
  if (!length(seeds) || any(!is.finite(seeds))) {
    stop("seeds must contain at least one finite integer seed.",
         call. = FALSE)
  }
  rows <- vector("list", length(seeds))
  for (ii in seq_along(seeds)) {
    one <- rqr_tcsp_calibration_boundary_map(
      sample_sizes = sample_sizes,
      guaranteed_contents = guaranteed_contents,
      tolerance_confidences = tolerance_confidences,
      method = method,
      n_sim = n_sim,
      numerical_confidence = numerical_confidence,
      seed = seeds[[ii]],
      adaptive_control = adaptive_control
    )
    one$seed <- seeds[[ii]]
    rows[[ii]] <- one
  }
  out <- do.call(rbind, rows)
  out <- out[order(out$n, out$guaranteed_content,
                   out$tolerance_confidence, out$seed), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Validate a TCSP formal action contract
#'
#' @param fit A TCSP fit or action contract.
#' @return A validation list.
#' @export
rqr_tcsp_validate_action <- function(fit) {
  contract <- if (is.list(fit) && !is.null(fit$contract)) fit$contract else fit
  required <- c(
    "guaranteed_content", "tolerance_confidence", "target_content",
    "retained_count", "formal_tolerance_action",
    "interval_endpoint_convention", "delta_raw", "posterior_summary_action"
  )
  missing <- setdiff(required, names(contract))
  problems <- character(0)
  if (length(missing)) {
    problems <- c(problems, paste("missing", paste(missing, collapse = ",")))
  }
  if (!identical(contract$interval_endpoint_convention,
                 "closed_order_statistic_window")) {
    problems <- c(problems, "unsupported interval endpoint convention")
  }
  if (identical(contract$posterior_summary_action,
                "formal_tolerance_action")) {
    problems <- c(problems, "posterior summary is mislabeled as formal action")
  }
  list(
    valid = !length(problems),
    problems = problems,
    finite_sample_claim_available =
      isTRUE(contract$finite_sample_claim_available),
    global_shortest_verified = isTRUE(contract$global_shortest_verified)
  )
}
