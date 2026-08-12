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
    c("monte_carlo_conservative", "dkw_conservative")
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
  class(out) <- c("rqr_tcsp_scan_distribution", "list")
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
    stop("distribution must come from rqr_tcsp_scan_distribution().",
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
  class(out) <- c("rqr_tcsp_scan_cdf_band", "list")
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
    method = c("monte_carlo_conservative", "dkw_conservative"),
    n_sim = 20000L, numerical_confidence = 0.999, seed = NULL) {
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
    method = c("monte_carlo_conservative", "dkw_conservative"),
    n_sim = 20000L, numerical_confidence = 0.999, seed = NULL) {
  n <- .rqr_mt_assert_count(n, "n", 1L)
  c_target <- .rqr_tcsp_assert_probability(
    guaranteed_content, "guaranteed_content"
  )
  tolerance_confidence <- .rqr_tcsp_assert_probability(
    tolerance_confidence, "tolerance_confidence"
  )
  method <- .rqr_tcsp_assert_method(method)

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
    return(list(
      schema_version = .rqr_tcsp_schema(),
      method = "scan_calibrated_tcsp_mt_rqr",
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
    ))
  }

  distribution <- rqr_tcsp_scan_distribution(
    n, c_target, n_sim = n_sim, seed = seed
  )
  band <- rqr_tcsp_scan_cdf_band(
    distribution, numerical_confidence = numerical_confidence
  )
  for (k in seq_len(n + 1L)) {
    prob <- .rqr_tcsp_probability_from_band(band, k)
    if (is.finite(prob$certified_lower_probability) &&
        prob$certified_lower_probability >= tolerance_confidence) {
      return(list(
        schema_version = .rqr_tcsp_schema(),
        method = "scan_calibrated_tcsp_mt_rqr",
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
        simultaneous_numerical_calibration = TRUE,
        finite_sample_claim_available = FALSE,
        asymptotic_claim_available = FALSE,
        infeasible = k > n
      ))
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
  class(out) <- c("rqr_tcsp_window", "list")
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

#' Fit the univariate TCSP-MT-RQR target
#'
#' Calibrates the retained count, selects the formal shortest closed-window
#' tolerance action, freezes `q` and `delta`, and optionally fits the
#' intercept-only fixed-rate ridge generalized posterior.  The scan action is
#' the tolerance object; posterior summaries are separate loss-based endpoint
#' uncertainty summaries.
#'
#' @param y Response sample.
#' @param guaranteed_content Minimum population content `c`.
#' @param tolerance_confidence Repeated-sample tolerance confidence `1-alpha`.
#' @param scan_method Critical-value method.
#' @param fit_mcmc Fit the intercept-only generalized posterior.
#' @param learning_rate Fixed generalized-Bayes learning rate `omega`.
#' @param mcmc_args Optional arguments passed to [rqr_mcmc_fit()].
#' @inheritParams rqr_tcsp_scan_probability
#' @return An `rqr_tcsp_fit` object.
#' @export
rqr_tcsp_fit_univariate <- function(
    y, guaranteed_content, tolerance_confidence,
    scan_method = c("monte_carlo_conservative", "dkw_conservative"),
    n_sim = 20000L, numerical_confidence = 0.999, seed = NULL,
    fit_mcmc = FALSE, learning_rate = 1, mcmc_args = list(),
    na_rm = FALSE) {
  clean <- .rqr_mt_clean_sample(y, na_rm = na_rm)
  y <- clean$y
  calibration <- rqr_tcsp_calibrate_count(
    n = length(y),
    guaranteed_content = guaranteed_content,
    tolerance_confidence = tolerance_confidence,
    method = scan_method,
    n_sim = n_sim,
    numerical_confidence = numerical_confidence,
    seed = seed
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

  contract <- list(
    schema_version = .rqr_tcsp_schema(),
    method = "scan_calibrated_tolerance_calibrated_shortest_path_mt_rqr",
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
  make_out <- function(posterior_fit = NULL) {
    out <- list(
      contract = contract,
      calibration = calibration,
      window = window,
      posterior_fit = posterior_fit
    )
    class(out) <- c("rqr_tcsp_fit", "list")
    out
  }

  posterior_fit <- NULL
  if (isTRUE(fit_mcmc)) {
    if (calibration$target_content >= 1) {
      out <- make_out(NULL)
      msg <- paste(
        "TCSP empirical range action has target_content >= 1;",
        "rqr_mcmc_fit() requires coverage_level in (0, 1).",
        "The formal tolerance action is preserved in the error condition."
      )
      stop(
        structure(
          list(message = msg, call = NULL, tcsp_fit = out),
          class = c("rqr_tcsp_mcmc_unavailable_error", "error", "condition")
        )
      )
    }
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
    contract$provenance_digest <- .rqr_tcsp_digest(list(
      calibration = calibration,
      window = window,
      posterior_model_spec = posterior_fit$model_spec
    ))
  }
  make_out(posterior_fit)
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
    scan_method = c("monte_carlo_conservative", "dkw_conservative"),
    n_sim = 20000L, numerical_confidence = 0.999, seed = NULL,
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
    method = "globally_verified_tcsp_content_path",
    tolerance_confidence = tolerance_confidence,
    scan_method = .rqr_tcsp_assert_method(scan_method),
    path = do.call(rbind, rows),
    diagnostics = diagnostics
  )
  class(out) <- c("rqr_tcsp_path", "list")
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
