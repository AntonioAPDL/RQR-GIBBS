#' Hybrid Bayesian-scan shortest tolerance fit
#'
#' Fits a full-distribution Bayesian posterior for `F` and reports the
#' minimum-width closed order-statistic interval satisfying both the scan count
#' and a posterior content-probability constraint.  The direct-DP engine is the
#' authoritative initial engine; the Gaussian DPM engine is a smooth posterior
#' UQ option with Monte Carlo content probabilities.
#'
#' @param y Numeric univariate responses.
#' @param guaranteed_content Required population content `c`.
#' @param tolerance_confidence Scan tolerance confidence.
#' @param posterior_confidence Required posterior content probability.
#' @param distribution_engine `"direct_dp"` or `"gaussian_dpm"`.
#' @param scan_method TCSP scan calibration method.
#' @param action_class Candidate class.
#' @param distribution_args Engine-specific arguments.
#' @param scan_args Scan calibration arguments.
#' @param action_control Action controls including `n_shortest_draws`.
#' @param na_rm Remove missing responses.
#' @return An `rqr_hybrid_bayes_tcsp` object.
#' @export
rqr_tcsp_hybrid_bayes_fit <- function(
    y, guaranteed_content, tolerance_confidence, posterior_confidence,
    distribution_engine = c("direct_dp", "gaussian_dpm"),
    scan_method = c("monte_carlo_conservative", "dkw_conservative"),
    action_class = "closed_order_statistic_intervals",
    distribution_args = list(), scan_args = list(), action_control = list(),
    na_rm = FALSE) {
  y <- .rqr_bayes_clean_y(y, na_rm = na_rm)
  c_target <- .rqr_bayes_assert_probability(
    guaranteed_content, "guaranteed_content"
  )
  tol_conf <- .rqr_bayes_assert_probability(
    tolerance_confidence, "tolerance_confidence"
  )
  post_conf <- .rqr_bayes_assert_probability(
    posterior_confidence, "posterior_confidence"
  )
  distribution_engine <- match.arg(distribution_engine)
  scan_method <- .rqr_tcsp_assert_method(scan_method)
  action_class <- match.arg(
    as.character(action_class)[1L], "closed_order_statistic_intervals"
  )
  scan_reserved <- c("n", "guaranteed_content", "tolerance_confidence", "method")
  if (length(intersect(names(scan_args), scan_reserved))) {
    stop("scan_args may not override n, guaranteed_content, tolerance_confidence, or method.",
         call. = FALSE)
  }
  precomputed_calibration <- scan_args$calibration %||% NULL
  scan_args$calibration <- NULL
  if (is.null(precomputed_calibration)) {
    calibration <- do.call(rqr_tcsp_calibrate_count, utils::modifyList(
      list(
        n = length(y),
        guaranteed_content = c_target,
        tolerance_confidence = tol_conf,
        method = scan_method
      ),
      scan_args
    ))
  } else {
    calibration <- precomputed_calibration
    if (!is.list(calibration) ||
        !identical(as.integer(calibration$n), as.integer(length(y))) ||
        !isTRUE(all.equal(calibration$guaranteed_content, c_target)) ||
        !isTRUE(all.equal(calibration$tolerance_confidence, tol_conf)) ||
        !identical(calibration$scan_critical_method, scan_method) ||
        is.null(calibration$retained_count)) {
      stop("scan_args$calibration is not compatible with the requested hybrid fit.",
           call. = FALSE)
    }
  }
  if (isTRUE(calibration$infeasible) || calibration$retained_count > length(y)) {
    stop("Hybrid Bayesian-scan calibration is infeasible for this sample size.",
         call. = FALSE)
  }
  formal_scan_window <- rqr_tcsp_shortest_window(
    y, retained_count = calibration$retained_count
  )
  posterior_shortest <- NULL
  if (identical(distribution_engine, "direct_dp")) {
    reserved <- c("y")
    if (length(intersect(names(distribution_args), reserved))) {
      stop("distribution_args may not override y.", call. = FALSE)
    }
    if (is.null(distribution_args$concentration)) {
      stop("distribution_args$concentration is required for direct_dp.",
           call. = FALSE)
    }
    if (is.null(distribution_args$base_measure)) {
      stop("distribution_args$base_measure is required for direct_dp.",
           call. = FALSE)
    }
    fit <- do.call(rqr_dp_fit, utils::modifyList(
      list(y = y),
      distribution_args
    ))
    bayes_action <- rqr_dp_bayes_tolerance_action(
      fit, content = c_target, posterior_confidence = post_conf,
      action_class = action_class
    )
    hybrid_action <- .rqr_hybrid_search_from_probabilities(
      y = y,
      posterior_probability = function(lower, upper, content) {
        rqr_dp_content_probability(fit, lower, upper, content)$
          posterior_content_probability
      },
      content = c_target,
      posterior_confidence = post_conf,
      scan_count = calibration$retained_count,
      action_name = "HDP-S",
      method = "direct_dp_exact_beta_content"
    )
    n_shortest <- action_control$n_shortest_draws %||% 0L
    n_shortest <- .rqr_mt_assert_count(
      n_shortest, "action_control$n_shortest_draws", 0L
    )
    if (n_shortest > 0L) {
      draw_args <- action_control$dp_draw_args %||% list()
      draws <- do.call(rqr_dp_draws, utils::modifyList(
        list(fit = fit, n_draws = n_shortest),
        draw_args
      ))
      posterior_shortest <- rqr_dp_shortest_draws(draws, c_target)
    }
  } else {
    reserved <- c("y")
    if (length(intersect(names(distribution_args), reserved))) {
      stop("distribution_args may not override y.", call. = FALSE)
    }
    fit <- do.call(rqr_dpm_fit, utils::modifyList(
      list(y = y),
      distribution_args
    ))
    bayes_action <- rqr_dpm_bayes_tolerance_action(
      fit, content = c_target, posterior_confidence = post_conf,
      action_class = action_class
    )
    hybrid_action <- .rqr_hybrid_search_from_probabilities(
      y = y,
      posterior_probability = function(lower, upper, content) {
        rqr_dpm_content_probability(fit, lower, upper, content)$
          posterior_content_probability
      },
      content = c_target,
      posterior_confidence = post_conf,
      scan_count = calibration$retained_count,
      action_name = "HDPM-S",
      method = "gaussian_dpm_monte_carlo_content"
    )
    n_shortest <- action_control$n_shortest_draws %||% 0L
    n_shortest <- .rqr_mt_assert_count(
      n_shortest, "action_control$n_shortest_draws", 0L
    )
    if (n_shortest > 0L) {
      posterior_shortest <- utils::head(
        rqr_dpm_shortest_draws(fit, c_target),
        n_shortest
      )
    }
  }
  selected <- hybrid_action$selected
  formal_action <- if (nrow(selected)) {
    list(
      formal_tolerance_action = sprintf(
        "[Y_(%d),Y_(%d)]",
        selected$lower_index[[1L]], selected$upper_index[[1L]]
      ),
      lower_endpoint = selected$lower[[1L]],
      upper_endpoint = selected$upper[[1L]],
      width = selected$width[[1L]],
      retained_count = selected$observed_count[[1L]],
      posterior_content_probability =
        selected$posterior_content_probability[[1L]],
      action_source = hybrid_action$action_name
    )
  } else {
    list(
      formal_tolerance_action = NA_character_,
      lower_endpoint = NA_real_,
      upper_endpoint = NA_real_,
      width = NA_real_,
      retained_count = NA_integer_,
      posterior_content_probability = NA_real_,
      action_source = hybrid_action$action_name
    )
  }
  out <- list(
    schema_version = "rqrgibbs_hybrid_bayes_tcsp/1.0.0",
    method = "hybrid_bayesian_scan_shortest_tolerance_interval",
    distribution_engine = distribution_engine,
    formal_tolerance_action = formal_action,
    scan_contract = list(
      calibration = calibration,
      scan_only_shortest_window = formal_scan_window,
      scan_count_fixed_not_resampled = TRUE,
      tolerance_confidence = tol_conf,
      scan_confidence_distinct_from_posterior_confidence = TRUE
    ),
    posterior_distribution_fit = fit,
    posterior_shortest_target_draws = posterior_shortest,
    bayesian_tolerance_action = bayes_action,
    hybrid_bayesian_scan_action = hybrid_action,
    posterior_content_probability =
      formal_action$posterior_content_probability,
    posterior_constraint_status = hybrid_action$posterior_constraint_status,
    legacy_plugin_fit_optional = NULL,
    response_likelihood = TRUE,
    generalized_bayes = FALSE,
    formal_action_class = action_class,
    finite_sample_scan_guard_available = TRUE,
    posterior_endpoint_coverage_claim_available = FALSE,
    provenance = .rqr_bayes_provenance(extra = list(
      distribution_engine = distribution_engine,
      scan_calibration_digest = calibration$calibration_digest %||%
        .rqr_bayes_digest(calibration),
      posterior_fit_digest = fit$provenance_digest %||%
        .rqr_bayes_digest(fit)
    ))
  )
  out$provenance_digest <- .rqr_bayes_digest(list(
    formal_tolerance_action = formal_action,
    scan_contract = out$scan_contract,
    posterior_fit_digest = out$provenance$posterior_fit_digest,
    hybrid_action = hybrid_action$selected
  ))
  class(out) <- c("tcsp_hybrid_bayes_fit", "rqr_hybrid_bayes_tcsp",
                  "tcsp_tolerance_fit", "rqr_tolerance_fit", "list")
  out
}
