#' Candidate intervals from profiled MTI targets and a direct DP content check
#'
#' The fixed-target calculation minimizes the penalized MTI criterion. The
#' separate DP model evaluates posterior interval content. The complete rule
#' requires its own repeated-sampling calibration and independent validation.
#' No historical MTI-ECM calibration is transferred automatically.
#' @param y Finite univariate observations.
#' @param content Required population content.
#' @param posterior_confidence Required DP posterior content probability.
#' @param q_grid Fixed fitted-content menu, with values in (content,1).
#' @param tilt_rule Fractional admissible targets or historical diagnostic targets.
#' @param tilt_positions Prespecified relative positions in the empirical tilt range.
#' @param allocation Fractional endpoint allocation for the central target.
#' @param historical_tilt_control Controls for the historical diagnostic menu.
#' @param learning_rate Positive fixed loss learning rate.
#' @param prior_precision,prior_mean Common Gaussian root precision and mean.
#' @param dp_concentration,dp_base_measure Fixed direct DP model parameters.
#' @param backend Profile numerical implementation.
#' @param control Fixed-target numerical controls.
#' @param return_candidate_traces Retain each fixed-target midpoint trace.
#' @return Selected interval, all candidate results and explicit nonreturn status.
#' @export
rqr_mti_dp_profile <- function(y, content, posterior_confidence, q_grid = NULL,
    tilt_rule = c("fractional_admissible", "historical"),
    tilt_positions = seq(0, 1, length.out = 7L), allocation = "balanced",
    historical_tilt_control = list(), learning_rate = 1,
    prior_precision = 1e-4, prior_mean = 0, dp_concentration = 1,
    dp_base_measure = rqr_dp_base_normal(mean = 0, sd = 4),
    backend = c("cpp", "R"), control = list(), return_candidate_traces = FALSE) {
  y <- .rqr_bayes_clean_y(y, na_rm = FALSE)
  n <- length(y)
  if (n < 2L) stop("At least two observations are required.", call. = FALSE)
  c_target <- .rqr_bayes_assert_probability(.rqr_mti_scalar(content, "content"), "content")
  threshold <- .rqr_bayes_assert_probability(.rqr_mti_scalar(posterior_confidence,
    "posterior_confidence"), "posterior_confidence")
  tilt_rule <- match.arg(tilt_rule)
  backend <- match.arg(backend)
  if (is.null(q_grid)) q_grid <- seq(c_target + (1 - c_target) / (2 * n),
    1 - (1 - c_target) / (2 * n), length.out = 9L)
  if (!is.numeric(q_grid) || !length(q_grid) || any(!is.finite(q_grid)) ||
      any(q_grid <= c_target | q_grid >= 1)) {
    stop("q_grid must contain values strictly between content and one.", call. = FALSE)
  }
  if (!is.numeric(tilt_positions) || !length(tilt_positions) ||
      any(!is.finite(tilt_positions)) || any(tilt_positions < 0 | tilt_positions > 1)) {
    stop("tilt_positions must be finite values in [0,1].", call. = FALSE)
  }
  q_grid <- sort(unique(q_grid))
  control$store_trace <- isTRUE(return_candidate_traces)
  control$fail_on_nonconvergence <- TRUE
  dp <- rqr_dp_fit(y, dp_concentration, dp_base_measure, strict_bayes = TRUE)
  candidates <- list()
  traces <- list()
  index <- 0L
  for (q in q_grid) {
    bounds <- rqr_mti_empirical_tilt_bounds(y, q)
    if (tilt_rule == "historical") {
      old <- do.call(.rqr_mti_profile_tilt_values,
        c(list(y = y, target_content = q), historical_tilt_control))
      central <- old$central_tilt
      deltas <- old$values
    } else {
      central <- rqr_mti_fractional_window(y, q, allocation)$delta_raw
      deltas <- .rqr_mti_profile_unique_numeric(c(central, 0,
        bounds$delta_lower + tilt_positions * (bounds$delta_upper - bounds$delta_lower)))
    }
    for (delta in deltas) {
      index <- index + 1L
      fit <- tryCatch(rqr_mti_intercept_fit(y, q, delta, learning_rate,
        prior_precision, prior_mean, backend, control), error = function(e) e)
      failed <- inherits(fit, "error")
      candidates[[index]] <- data.frame(candidate_index = index,
        target_content = q, mean_tilt = delta, central_tilt = central,
        delta_lower = bounds$delta_lower, delta_upper = bounds$delta_upper,
        empirical_tilt_admissible = delta >= bounds$delta_lower - 1e-12 &
          delta <= bounds$delta_upper + 1e-12,
        lower = if (failed) NA_real_ else fit$lower,
        upper = if (failed) NA_real_ else fit$upper,
        width = if (failed) NA_real_ else fit$width,
        objective = if (failed) NA_real_ else fit$objective,
        objective_gap = if (failed) NA_real_ else fit$objective_gap,
        relative_objective_gap = if (failed) NA_real_ else fit$relative_objective_gap,
        profile_converged = !failed && isTRUE(fit$converged),
        iterations = if (failed) NA_integer_ else fit$iterations,
        failure_reason = if (failed) conditionMessage(fit) else NA_character_)
      if (!failed && return_candidate_traces && nrow(fit$trace)) {
        z <- fit$trace
        z$candidate_index <- index
        z$target_content <- q
        z$mean_tilt <- delta
        traces[[index]] <- z
      }
    }
  }
  candidates <- do.call(rbind, candidates)
  finite <- candidates$profile_converged & is.finite(candidates$lower) &
    is.finite(candidates$upper)
  candidates$posterior_content_probability <- NA_real_
  candidates$observed_count <- NA_integer_
  if (any(finite)) {
    p <- rqr_dp_content_probability(dp, candidates$lower[finite],
      candidates$upper[finite], c_target)
    candidates$posterior_content_probability[finite] <- p$posterior_content_probability
    candidates$observed_count[finite] <- p$observed_count
  }
  candidates$posterior_constraint_satisfied <- finite &
    !is.na(candidates$posterior_content_probability) &
    candidates$posterior_content_probability >= threshold
  eligible <- candidates[candidates$posterior_constraint_satisfied, ]
  if (nrow(eligible)) {
    eligible <- eligible[order(eligible$width, eligible$target_content,
      abs(eligible$mean_tilt - eligible$central_tilt), eligible$lower,
      eligible$candidate_index), ]
  }
  selected <- utils::head(eligible, 1L)
  failed <- any(!candidates$profile_converged)
  if (failed) selected <- eligible[0L, ]
  list(schema_version = "rqrgibbs_mti_dp_profile/1.0.0",
    method_id = paste0("mti_profile_", tilt_rule, "_v1"),
    selected = selected, candidates = candidates,
    candidate_traces = if (length(traces)) do.call(rbind, traces) else data.frame(),
    interval_returned = nrow(selected) == 1L,
    status = if (failed) "candidate_numerical_failure" else if (nrow(selected))
      "returned" else "no_eligible_interval",
    content = c_target, posterior_confidence = threshold,
    generalized_bayes = TRUE, dp_response_model = TRUE,
    frequentist_tolerance_theorem_available = FALSE,
    historical_policy_transferred = FALSE)
}
