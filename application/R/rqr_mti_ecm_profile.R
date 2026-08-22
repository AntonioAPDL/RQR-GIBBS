.rqr_mti_profile_schema <- function() {
  "rqrgibbs_mti_ecm_dp_profile/1.0.0"
}

.rqr_mti_profile_unique_numeric <- function(x, tolerance = 1e-12) {
  x <- sort(as.numeric(x[is.finite(x)]))
  if (!length(x)) return(numeric())
  out <- x[[1L]]
  for (value in x[-1L]) {
    if (abs(value - utils::tail(out, 1L)) > tolerance) {
      out <- c(out, value)
    }
  }
  out
}

.rqr_mti_profile_q_grid <- function(
    n, content, scan_target_content = NA_real_, n_points = 7L,
    q_min_buffer = NULL, q_max = 0.995, include_scan_target = TRUE) {
  n <- .rqr_mt_assert_count(n, "n", 2L)
  c_target <- .rqr_bayes_assert_probability(content, "content")
  n_points <- .rqr_mt_assert_count(n_points, "n_points", 1L)
  q_max <- as.numeric(q_max)[1L]
  if (!is.finite(q_max) || q_max <= c_target || q_max >= 1) {
    stop("q_max must be one finite value in (content, 1).", call. = FALSE)
  }
  buffer <- q_min_buffer %||% max(1e-4, 0.25 / n)
  buffer <- as.numeric(buffer)[1L]
  if (!is.finite(buffer) || buffer <= 0) {
    stop("q_min_buffer must be finite and positive.", call. = FALSE)
  }
  q_lower <- min(1 - 1e-8, c_target + buffer)
  scan_q <- as.numeric(scan_target_content)[1L]
  if (is.finite(scan_q) && scan_q > c_target && scan_q < 1) {
    q_upper <- min(q_max, scan_q)
  } else if (is.finite(scan_q) && scan_q >= 1) {
    q_upper <- min(q_max, 1 - max(1e-4, 0.25 / n))
  } else {
    q_upper <- min(q_max, c_target + max(2 * buffer, 2 / n, 0.02))
  }
  if (!is.finite(q_upper) || q_upper <= q_lower) {
    q_upper <- min(q_max, 1 - 1e-8)
  }
  if (q_upper <= q_lower) {
    stop("The profile q grid is empty for this content and q_max.",
         call. = FALSE)
  }
  if (n_points == 1L) {
    grid <- q_lower
  } else {
    t <- seq(0, 1, length.out = n_points)
    grid <- q_lower + (q_upper - q_lower) * t^1.7
  }
  if (isTRUE(include_scan_target) && is.finite(scan_q) &&
      scan_q > c_target && scan_q < 1 && scan_q <= q_max) {
    grid <- c(grid, scan_q)
  }
  .rqr_mti_profile_unique_numeric(
    pmin(q_max, pmax(c_target + .Machine$double.eps, grid))
  )
}

.rqr_mti_profile_fractional_endpoints <- function(tilt) {
  wl <- as.numeric(tilt$interpolation_weight_lower %||% 1)[1L]
  wu <- as.numeric(tilt$interpolation_weight_upper %||% (1 - wl))[1L]
  lower <- wl * tilt$lower_window$lower_endpoint +
    wu * tilt$upper_window$lower_endpoint
  upper <- wl * tilt$lower_window$upper_endpoint +
    wu * tilt$upper_window$upper_endpoint
  c(lower = lower, upper = upper)
}

.rqr_mti_profile_tilt_values <- function(
    y, target_content, tilt_offsets_sd = c(-0.25, 0, 0.25),
    include_zero_tilt = TRUE, max_abs_tilt_sd = 2) {
  tilt <- rqr_tcsp_fractional_tilt(y, target_content, na_rm = FALSE)
  s <- stats::sd(y)
  if (!is.finite(s) || s <= 0) s <- 1
  offsets <- as.numeric(tilt_offsets_sd)
  if (!length(offsets) || any(!is.finite(offsets))) {
    stop("tilt_offsets_sd must contain finite numeric offsets.",
         call. = FALSE)
  }
  max_abs_tilt_sd <- as.numeric(max_abs_tilt_sd)[1L]
  if (!is.finite(max_abs_tilt_sd) || max_abs_tilt_sd <= 0) {
    stop("max_abs_tilt_sd must be finite and positive.", call. = FALSE)
  }
  values <- tilt$delta_raw + offsets * s
  if (isTRUE(include_zero_tilt)) values <- c(values, 0)
  limit <- max_abs_tilt_sd * s
  values <- values[abs(values) <= limit + 1e-12]
  values <- .rqr_mti_profile_unique_numeric(values)
  if (!length(values)) values <- tilt$delta_raw
  list(
    target_content = as.numeric(target_content),
    central_tilt = as.numeric(tilt$delta_raw),
    central_tilt_standardized = as.numeric(tilt$delta_standardized),
    tilt_rule = as.character(tilt$rule),
    lower_count = as.integer(tilt$lower_count),
    upper_count = as.integer(tilt$upper_count),
    interpolation_weight_lower =
      as.numeric(tilt$interpolation_weight_lower %||% NA_real_),
    endpoints = .rqr_mti_profile_fractional_endpoints(tilt),
    values = values,
    tilt_object = tilt
  )
}

.rqr_mti_profile_ecm_row <- function(
    y, X, q, delta, central, learning_rate, beta_prior_obj, ecm_control,
    candidate_index) {
  start_shift <- delta - central$central_tilt
  init <- list(
    beta1 = c(central$endpoints[["lower"]] + start_shift),
    beta2 = c(central$endpoints[["upper"]] + start_shift)
  )
  fit <- tryCatch(
    rqr_ecm_fit(
      y = y,
      X = X,
      coverage_level = q,
      learning_rate = learning_rate,
      mean_tilt = delta,
      beta_prior_obj = beta_prior_obj,
      ecm_control = ecm_control,
      init = init
    ),
    error = function(e) e
  )
  if (inherits(fit, "error")) {
    return(data.frame(
      candidate_index = as.integer(candidate_index),
      target_content = q,
      mean_tilt = delta,
      central_tilt = central$central_tilt,
      lower = NA_real_,
      upper = NA_real_,
      width = NA_real_,
      observed_count = NA_integer_,
      retained_fraction = NA_real_,
      ecm_converged = FALSE,
      ecm_iterations = NA_integer_,
      ecm_objective = NA_real_,
      ecm_final_stationarity = NA_real_,
      ecm_selected_start = NA_character_,
      ecm_backend = ecm_control$ecm_backend %||% NA_character_,
      finite_interval = FALSE,
      failure_reason = conditionMessage(fit),
      stringsAsFactors = FALSE
    ))
  }
  pred <- predict_interval(fit, X_new = X[1L, , drop = FALSE])
  lower <- as.numeric(pred$lower)[1L]
  upper <- as.numeric(pred$upper)[1L]
  finite_interval <- is.finite(lower) && is.finite(upper) && upper >= lower
  stationarity <- fit$stationarity_diagnostic$max_abs_midpoint_gradient %||%
    NA_real_
  data.frame(
    candidate_index = as.integer(candidate_index),
    target_content = q,
    mean_tilt = delta,
    central_tilt = central$central_tilt,
    lower = lower,
    upper = upper,
    width = upper - lower,
    observed_count = if (finite_interval) {
      as.integer(sum(y >= lower & y <= upper))
    } else {
      NA_integer_
    },
    retained_fraction = if (finite_interval) {
      mean(y >= lower & y <= upper)
    } else {
      NA_real_
    },
    ecm_converged = isTRUE(fit$converged),
    ecm_iterations = as.integer(fit$iterations %||% NA_integer_),
    ecm_objective = as.numeric(fit$objective %||% NA_real_),
    ecm_final_stationarity = as.numeric(stationarity)[1L],
    ecm_selected_start = fit$selected_start$label %||% NA_character_,
    ecm_backend = fit$ecm_backend %||% fit$model_spec$ecm_backend %||%
      ecm_control$ecm_backend %||% NA_character_,
    finite_interval = finite_interval,
    failure_reason = "",
    stringsAsFactors = FALSE
  )
}

#' DP-calibrated MTI-ECM profile action
#'
#' Searches fixed-target MTI-ECM endpoint candidates over a grid of fitted
#' contents and mean tilts, then screens the resulting intervals with the exact
#' direct-DP posterior content probability for fixed intervals.  The MTI-ECM
#' layer is a loss-based generalized-Bayes endpoint construction.  The DP layer
#' is an ordinary response-distribution model used only to evaluate interval
#' content.
#'
#' @param y Numeric univariate response sample.
#' @param content Required population content.
#' @param posterior_confidence Required posterior probability that the selected
#'   interval has at least `content` population mass under the direct-DP
#'   response-distribution posterior.
#' @param dp_concentration Positive direct-DP concentration.
#' @param dp_base_measure Base measure from [rqr_dp_base_normal()] or a
#'   compatible object.
#' @param strict_bayes Reject data-dependent DP base measures.
#' @param scan_target_content Optional scan-calibrated fitted content used only
#'   as an upper reference for the MTI profile grid.
#' @param q_grid Optional fitted-content grid.  If `NULL`, a deterministic grid
#'   is built from `content`, `n`, and `scan_target_content`.
#' @param q_grid_control List with `n_points`, `q_min_buffer`, `q_max`, and
#'   `include_scan_target`.
#' @param tilt_grid_control List with `tilt_offsets_sd`, `include_zero_tilt`,
#'   and `max_abs_tilt_sd`.
#' @param learning_rate Fixed generalized-Bayes learning rate for MTI-ECM.
#' @param beta_prior_obj Ridge beta prior object.  Defaults to a weak ridge
#'   prior.
#' @param ecm_control ECM control list.  For iid validation, use
#'   `ecm_backend = "cpp"`.
#' @param expand_if_empty Expand the profile grid once if no candidate passes
#'   the DP screen.
#' @param na_rm Remove missing responses.
#' @return An `rqr_mti_ecm_dp_profile_action` object.
#' @export
rqr_mti_ecm_dp_profile_action <- function(
    y, content, posterior_confidence, dp_concentration = 1,
    dp_base_measure = rqr_dp_base_normal(mean = 0, sd = 4),
    strict_bayes = TRUE, scan_target_content = NA_real_, q_grid = NULL,
    q_grid_control = list(), tilt_grid_control = list(), learning_rate = 1,
    beta_prior_obj = NULL, ecm_control = list(), expand_if_empty = TRUE,
    na_rm = FALSE) {
  y <- .rqr_bayes_clean_y(y, na_rm = na_rm)
  if (length(y) < 2L) stop("At least two observations are required.",
                           call. = FALSE)
  c_target <- .rqr_bayes_assert_probability(content, "content")
  post_conf <- .rqr_bayes_assert_probability(
    posterior_confidence, "posterior_confidence"
  )
  learning_rate <- .rqr_bayes_assert_positive(learning_rate, "learning_rate")
  if (is.null(beta_prior_obj)) {
    beta_prior_obj <- beta_prior(
      "ridge", ridge = list(tau2 = ecm_control$beta_ridge_tau2 %||% 1e4)
    )
  }
  ecm_control <- utils::modifyList(
    list(
      max_iter = 100L,
      stable_iterations = 1L,
      tol_stationarity = 1e-3,
      residual_product_floor = 1e-8,
      multistart = FALSE,
      store_iteration_trace = FALSE,
      ecm_backend = "cpp"
    ),
    ecm_control
  )
  ecm_control <- .rqr_ecm_assert_control(ecm_control)

  if (is.null(q_grid)) {
    q_grid <- do.call(
      .rqr_mti_profile_q_grid,
      utils::modifyList(
        list(
          n = length(y),
          content = c_target,
          scan_target_content = scan_target_content
        ),
        q_grid_control
      )
    )
  } else {
    q_grid <- as.numeric(q_grid)
    if (!length(q_grid) || any(!is.finite(q_grid)) ||
        any(q_grid <= c_target | q_grid >= 1)) {
      stop("q_grid must contain finite values in (content, 1).",
           call. = FALSE)
    }
    q_grid <- .rqr_mti_profile_unique_numeric(q_grid)
  }

  fit_dp <- rqr_dp_fit(
    y = y,
    concentration = dp_concentration,
    base_measure = dp_base_measure,
    strict_bayes = strict_bayes
  )
  X <- matrix(1, length(y), 1L, dimnames = list(NULL, "(Intercept)"))
  build_candidates <- function(grid, expanded = FALSE) {
    rows <- list()
    index <- 0L
    for (q in grid) {
      central <- do.call(
        .rqr_mti_profile_tilt_values,
        utils::modifyList(
          list(y = y, target_content = q),
          tilt_grid_control
        )
      )
      for (delta in central$values) {
        index <- index + 1L
        row <- .rqr_mti_profile_ecm_row(
          y = y,
          X = X,
          q = q,
          delta = delta,
          central = central,
          learning_rate = learning_rate,
          beta_prior_obj = beta_prior_obj,
          ecm_control = ecm_control,
          candidate_index = index
        )
        row$tilt_rule <- central$tilt_rule
        row$mti_tilt_lower_count <- central$lower_count
        row$mti_tilt_upper_count <- central$upper_count
        row$mti_tilt_interpolation_weight <-
          central$interpolation_weight_lower
        row$grid_expanded <- isTRUE(expanded)
        rows[[index]] <- row
      }
    }
    do.call(rbind, rows)
  }
  candidates <- build_candidates(q_grid, expanded = FALSE)
  finite <- candidates$finite_interval
  candidates$posterior_content_probability <- NA_real_
  candidates$posterior_mean_content <- NA_real_
  candidates$posterior_var_content <- NA_real_
  candidates$base_mass <- NA_real_
  if (any(finite)) {
    probs <- rqr_dp_content_probability(
      fit_dp,
      lower = candidates$lower[finite],
      upper = candidates$upper[finite],
      content = c_target
    )
    candidates$posterior_content_probability[finite] <-
      probs$posterior_content_probability
    candidates$posterior_mean_content[finite] <- probs$posterior_mean_content
    candidates$posterior_var_content[finite] <- probs$posterior_var_content
    candidates$base_mass[finite] <- probs$base_mass
  }
  candidates$posterior_constraint_satisfied <-
    is.finite(candidates$posterior_content_probability) &
    candidates$posterior_content_probability >= post_conf

  expanded <- FALSE
  if (!any(candidates$posterior_constraint_satisfied) &&
      isTRUE(expand_if_empty)) {
    expanded <- TRUE
    expanded_q <- .rqr_mti_profile_q_grid(
      n = length(y),
      content = c_target,
      scan_target_content = scan_target_content,
      n_points = max(length(q_grid), 9L),
      q_min_buffer = min(max(1e-5, 0.1 / length(y)), (1 - c_target) / 4),
      q_max = max(q_grid_control$q_max %||% 0.995, max(q_grid)),
      include_scan_target = TRUE
    )
    expanded_q <- setdiff(expanded_q, q_grid)
    if (length(expanded_q)) {
      more <- build_candidates(expanded_q, expanded = TRUE)
      finite_more <- more$finite_interval
      more$posterior_content_probability <- NA_real_
      more$posterior_mean_content <- NA_real_
      more$posterior_var_content <- NA_real_
      more$base_mass <- NA_real_
      if (any(finite_more)) {
        probs <- rqr_dp_content_probability(
          fit_dp,
          lower = more$lower[finite_more],
          upper = more$upper[finite_more],
          content = c_target
        )
        more$posterior_content_probability[finite_more] <-
          probs$posterior_content_probability
        more$posterior_mean_content[finite_more] <-
          probs$posterior_mean_content
        more$posterior_var_content[finite_more] <- probs$posterior_var_content
        more$base_mass[finite_more] <- probs$base_mass
      }
      more$posterior_constraint_satisfied <-
        is.finite(more$posterior_content_probability) &
        more$posterior_content_probability >= post_conf
      candidates <- rbind(candidates, more)
      candidates$candidate_index <- seq_len(nrow(candidates))
    }
  }

  feasible <- candidates[
    candidates$finite_interval &
      candidates$posterior_constraint_satisfied,
    ,
    drop = FALSE
  ]
  if (nrow(feasible)) {
    feasible <- feasible[order(
      feasible$width,
      feasible$target_content,
      abs(feasible$mean_tilt - feasible$central_tilt),
      feasible$lower,
      feasible$candidate_index
    ), , drop = FALSE]
    selected <- feasible[1L, , drop = FALSE]
    status <- "satisfied"
  } else {
    selected <- candidates[0L, , drop = FALSE]
    status <- "infeasible_within_profile_grid"
  }

  out <- list(
    schema_version = .rqr_mti_profile_schema(),
    method = "profile_calibrated_mti_ecm_with_direct_dp_content_screen",
    content = c_target,
    posterior_confidence = post_conf,
    scan_target_content = as.numeric(scan_target_content)[1L],
    q_grid = q_grid,
    profile_grid_expanded = expanded,
    selected = selected,
    candidates = candidates,
    candidates_evaluated = nrow(candidates),
    feasible_count = nrow(feasible),
    posterior_constraint_status = status,
    posterior_distribution_fit = fit_dp,
    response_likelihood = TRUE,
    generalized_bayes = TRUE,
    formal_tolerance_action = FALSE,
    finite_sample_scan_guard_available = FALSE,
    posterior_endpoint_coverage_claim_available = FALSE,
    no_fallback_used = TRUE,
    provenance = .rqr_bayes_provenance(extra = list(
      profile_grid_digest = .rqr_bayes_digest(q_grid),
      candidate_digest = .rqr_bayes_digest(candidates),
      dp_fit_digest = fit_dp$provenance_digest,
      ecm_control = ecm_control
    ))
  )
  out$provenance_digest <- .rqr_bayes_digest(list(
    selected = selected,
    candidates = candidates,
    content = c_target,
    posterior_confidence = post_conf,
    dp_fit_digest = fit_dp$provenance_digest
  ))
  class(out) <- c(
    "mti_ecm_dp_profile_action",
    "rqr_mti_ecm_dp_profile_action",
    "list"
  )
  out
}

#' @export
print.rqr_mti_ecm_dp_profile_action <- function(x, ...) {
  cat("DP-calibrated MTI-ECM profile action\n")
  cat(sprintf("  content:               %.4f\n", x$content))
  cat(sprintf("  posterior confidence:  %.4f\n", x$posterior_confidence))
  cat(sprintf("  candidates evaluated:  %d\n", x$candidates_evaluated))
  cat(sprintf("  candidates passing:    %d\n", x$feasible_count))
  if (nrow(x$selected)) {
    cat(sprintf("  selected q:            %.4f\n",
                x$selected$target_content[[1L]]))
    cat(sprintf("  selected width:        %.6g\n", x$selected$width[[1L]]))
  } else {
    cat("  selected width:        unavailable\n")
  }
  invisible(x)
}
