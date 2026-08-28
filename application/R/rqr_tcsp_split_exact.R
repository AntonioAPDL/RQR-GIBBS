.rqr_tcsp_split_exact_schema <- function() {
  "rqrgibbs_tcsp_split_exact_spacing/1.0.0"
}

.rqr_tcsp_split_pilot_method <- function(pilot_method) {
  match.arg(
    as.character(pilot_method)[1L],
    c("empirical_shortest", "ecm_fixed_tilt", "ecm_profile", "cornish_fisher")
  )
}

.rqr_tcsp_plotting_position <- function(index, n, convention = "rank_over_n_plus_1") {
  convention <- match.arg(as.character(convention)[1L], "rank_over_n_plus_1")
  index / (n + 1)
}

#' Exact fixed-spacing TCSP gap
#'
#' For a main sample of size `main_n` and fixed indices `r < s`, the
#' probability spacing `F(Z_(s)) - F(Z_(r))` has a
#' `Beta(d, main_n + 1 - d)` law with `d = s - r`.  The closed interval
#' contains `d + 1` observed positions.
#'
#' @param main_n Main-sample size.
#' @param guaranteed_content Minimum population content `c`.
#' @param tolerance_confidence Repeated-sample tolerance confidence.
#' @return A calibration object.
#' @export
rqr_tcsp_exact_spacing_gap <- function(main_n, guaranteed_content,
                                       tolerance_confidence) {
  main_n <- .rqr_mt_assert_count(main_n, "main_n", 2L)
  c_target <- .rqr_tcsp_assert_probability(
    guaranteed_content, "guaranteed_content"
  )
  conf <- .rqr_tcsp_assert_probability(
    tolerance_confidence, "tolerance_confidence"
  )
  candidates <- seq_len(main_n - 1L)
  survival <- stats::pbeta(
    c_target,
    shape1 = candidates,
    shape2 = main_n + 1L - candidates,
    lower.tail = FALSE
  )
  ok <- which(survival >= conf)
  if (!length(ok)) {
    stop("Exact spacing calibration is infeasible for this main sample size.",
         call. = FALSE)
  }
  d <- candidates[[ok[[1L]]]]
  list(
    schema_version = .rqr_tcsp_split_exact_schema(),
    method = "exact_beta_spacing_gap",
    main_n = as.integer(main_n),
    guaranteed_content = c_target,
    tolerance_confidence = conf,
    spacing_gap = as.integer(d),
    closed_position_count = as.integer(d + 1L),
    beta_shape1 = as.integer(d),
    beta_shape2 = as.integer(main_n + 1L - d),
    exact_beta_survival_probability = survival[[ok[[1L]]]],
    effective_pilot_content = d / (main_n + 1L),
    index_convention =
      "d=s-r is the probability-spacing parameter; closed interval count is d+1",
    finite_sample_claim_available = TRUE,
    conditional_on_independent_pilot = TRUE
  )
}

.rqr_tcsp_split_exact_indices <- function(n, pilot_fraction, split_seed) {
  pilot_fraction <- as.numeric(pilot_fraction)[1L]
  if (!is.finite(pilot_fraction) || pilot_fraction <= 0 ||
      pilot_fraction >= 1) {
    stop("pilot_fraction must be one finite scalar in (0, 1).",
         call. = FALSE)
  }
  restore_rng <- .rqr_mt_seed_scope(split_seed)
  on.exit(restore_rng(), add = TRUE)
  pilot_n <- as.integer(floor(n * pilot_fraction))
  pilot_n <- min(max(pilot_n, 3L), n - 2L)
  if (pilot_n < 3L || n - pilot_n < 2L) {
    stop("The requested split leaves too few target-selection or main observations.",
         call. = FALSE)
  }
  pilot_indices <- sort(sample.int(n, size = pilot_n, replace = FALSE))
  main_indices <- setdiff(seq_len(n), pilot_indices)
  list(
    pilot_indices = as.integer(pilot_indices),
    main_indices = as.integer(main_indices),
    pilot_n = as.integer(length(pilot_indices)),
    main_n = as.integer(length(main_indices))
  )
}

.rqr_tcsp_split_pilot_lower_tail <- function(
    pilot_y, q_eff, pilot_method, pilot_args, ecm_args) {
  m <- length(pilot_y)
  pilot_method <- .rqr_tcsp_split_pilot_method(pilot_method)
  plotting <- pilot_args$plotting_position %||% "rank_over_n_plus_1"
  clamp <- function(u) min(max(as.numeric(u)[1L], 0), 1 - q_eff)
  if (identical(pilot_method, "empirical_shortest")) {
    k <- min(m, max(2L, as.integer(ceiling(q_eff * m))))
    window <- rqr_tcsp_shortest_window(pilot_y, retained_count = k)
    u <- .rqr_tcsp_plotting_position(
      window$shortest_window_start, m, convention = plotting
    )
    return(list(
      lower_tail_coordinate = clamp(u),
      pilot_fit = window,
      ecm_fit = NULL,
      pilot_width = window$width,
      selected_tilt = window$delta_raw,
      method_detail = "pilot_empirical_shortest_closed_window"
    ))
  }
  if (identical(pilot_method, "cornish_fisher")) {
    gamma <- .rqr_mt_adjusted_fisher_pearson_skewness(pilot_y)
    prob <- .rqr_mt_cf_probability_window(
      q_eff, gamma1 = gamma, target = "shortest"
    )
    return(list(
      lower_tail_coordinate = clamp(prob$u_lower),
      pilot_fit = prob,
      ecm_fit = NULL,
      pilot_width = NA_real_,
      selected_tilt = NA_real_,
      method_detail = "pilot_cornish_fisher_shortest_probability_window"
    ))
  }
  reserved <- c(
    "y", "X", "coverage_level", "mean_tilt", "learning_rate",
    "beta_prior_obj", "response_likelihood"
  )
  supplied_reserved <- intersect(names(ecm_args), reserved)
  if (length(supplied_reserved)) {
    stop(
      paste(
        "Split exact TCSP reserves these ecm_args fields:",
        paste(supplied_reserved, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  X_pilot <- matrix(1, m, 1L, dimnames = list(NULL, "(Intercept)"))
  empirical <- rqr_tcsp_shortest_window(
    pilot_y, retained_count = min(m, max(2L, as.integer(ceiling(q_eff * m))))
  )
  fit_one <- function(delta, label) {
    args <- utils::modifyList(
      list(
        y = pilot_y,
        X = X_pilot,
        coverage_level = q_eff,
        learning_rate = pilot_args$learning_rate %||% 1,
        mean_tilt = delta,
        beta_prior_obj = beta_prior(
          "ridge",
          ridge = list(tau2 = pilot_args$beta_ridge_tau2 %||% 1e4)
        )
      ),
      ecm_args
    )
    fit <- do.call(rqr_ecm_fit, args)
    pred <- predict_interval(fit, X_new = X_pilot[1, , drop = FALSE])
    list(
      label = label,
      fit = fit,
      width = as.numeric(pred$width)[1L],
      lower = as.numeric(pred$lower)[1L],
      upper = as.numeric(pred$upper)[1L],
      delta = delta
    )
  }
  if (identical(pilot_method, "ecm_fixed_tilt")) {
    delta <- pilot_args$mean_tilt %||% pilot_args$delta %||% empirical$delta_raw
    candidate <- fit_one(delta, "fixed_tilt")
    lower_rank <- sum(sort(pilot_y) < candidate$lower) + 1L
    u <- .rqr_tcsp_plotting_position(lower_rank, m, convention = plotting)
    return(list(
      lower_tail_coordinate = clamp(u),
      pilot_fit = empirical,
      ecm_fit = candidate$fit,
      pilot_width = candidate$width,
      selected_tilt = candidate$delta,
      method_detail = "pilot_ecm_fixed_tilt"
    ))
  }
  tilt_grid <- as.numeric(
    pilot_args$tilt_grid %||%
      c(empirical$delta_raw - stats::sd(pilot_y) / 4,
        empirical$delta_raw,
        empirical$delta_raw + stats::sd(pilot_y) / 4)
  )
  if (!length(tilt_grid) || any(!is.finite(tilt_grid))) {
    stop("pilot_args$tilt_grid must contain finite values.", call. = FALSE)
  }
  candidates <- lapply(seq_along(tilt_grid), function(ii) {
    fit_one(tilt_grid[[ii]], sprintf("tilt_grid_%02d", ii))
  })
  widths <- vapply(candidates, `[[`, numeric(1L), "width")
  selected <- candidates[[which.min(widths)]]
  lower_rank <- sum(sort(pilot_y) < selected$lower) + 1L
  u <- .rqr_tcsp_plotting_position(lower_rank, m, convention = plotting)
  list(
    lower_tail_coordinate = clamp(u),
    pilot_fit = list(
      empirical_shortest = empirical,
      tilt_grid = tilt_grid,
      grid_summary = data.frame(
        tilt = tilt_grid,
        width = widths,
        selected = seq_along(tilt_grid) == which.min(widths)
      )
    ),
    ecm_fit = selected$fit,
    pilot_width = selected$width,
    selected_tilt = selected$delta,
    method_detail = "pilot_ecm_profile_predeclared_tilt_grid"
  )
}

#' Split-sample exact-spacing TCSP fit
#'
#' Uses an independent target-selection sample to choose placement and an independent main
#' sample to form a fixed order-statistic spacing.  Conditional on the selected placement,
#' the main-sample spacing has an exact Beta law under iid continuity.
#'
#' @param y Response sample.
#' @param guaranteed_content Minimum population content `c`.
#' @param tolerance_confidence Repeated-sample tolerance confidence.
#' @param pilot_fraction Fraction of observations assigned to the target-selection sample.
#' @param pilot_method Target-selection placement method.
#' @param split_seed Integer seed fixing the split before fitting.
#' @param pilot_args Optional target-selection controls.
#' @param ecm_args Optional ECM controls for ECM target-selection methods.
#' @param na_rm Remove nonfinite observations before splitting.
#' @return An `rqr_tcsp_split_exact_fit` object.
#' @export
rqr_tcsp_split_exact_fit <- function(
    y, guaranteed_content, tolerance_confidence, pilot_fraction,
    pilot_method = c("empirical_shortest", "ecm_fixed_tilt", "ecm_profile",
                     "cornish_fisher"),
    split_seed, pilot_args = list(), ecm_args = list(), na_rm = FALSE) {
  clean <- .rqr_mt_clean_sample(y, na_rm = na_rm)
  y <- clean$y
  n <- length(y)
  if (missing(split_seed) || is.null(split_seed)) {
    stop("split_seed must be supplied to fix target-selection/main independence.",
         call. = FALSE)
  }
  if (!is.list(pilot_args)) stop("pilot_args must be a list.", call. = FALSE)
  if (!is.list(ecm_args)) stop("ecm_args must be a list.", call. = FALSE)
  pilot_method <- .rqr_tcsp_split_pilot_method(pilot_method)
  split <- .rqr_tcsp_split_exact_indices(n, pilot_fraction, split_seed)
  gap <- rqr_tcsp_exact_spacing_gap(
    main_n = split$main_n,
    guaranteed_content = guaranteed_content,
    tolerance_confidence = tolerance_confidence
  )
  pilot_y <- y[split$pilot_indices]
  main_y <- y[split$main_indices]
  pilot <- .rqr_tcsp_split_pilot_lower_tail(
    pilot_y = pilot_y,
    q_eff = gap$effective_pilot_content,
    pilot_method = pilot_method,
    pilot_args = pilot_args,
    ecm_args = ecm_args
  )
  main_sorted <- sort(main_y)
  max_lower <- split$main_n - gap$spacing_gap
  lower_candidates <- seq_len(max_lower)
  lower_positions <- lower_candidates / (split$main_n + 1)
  lower_index <- lower_candidates[which.min(
    abs(lower_positions - pilot$lower_tail_coordinate)
  )]
  upper_index <- lower_index + gap$spacing_gap
  lower_endpoint <- main_sorted[[lower_index]]
  upper_endpoint <- main_sorted[[upper_index]]
  width <- upper_endpoint - lower_endpoint
  contract <- list(
    schema_version = .rqr_tcsp_split_exact_schema(),
    method = "pilot_selected_exact_spacing_tcsp",
    sample_size = n,
    pilot_n = split$pilot_n,
    main_n = split$main_n,
    guaranteed_content = gap$guaranteed_content,
    tolerance_confidence = gap$tolerance_confidence,
    split_seed = as.integer(split_seed),
    pilot_method = pilot_method,
    spacing_gap = gap$spacing_gap,
    closed_position_count = gap$closed_position_count,
    effective_pilot_content = gap$effective_pilot_content,
    pilot_lower_tail_coordinate = pilot$lower_tail_coordinate,
    main_lower_index = as.integer(lower_index),
    main_upper_index = as.integer(upper_index),
    exact_beta_survival_probability =
      gap$exact_beta_survival_probability,
    conditional_tolerance_confidence = gap$tolerance_confidence,
    formal_tolerance_action =
      "[Z_(r_hat), Z_(r_hat+d)] on the independent main sample with fixed r_hat",
    formal_action_source = "independent_target_selection_fixed_spacing",
    posterior_summary_action = "not_formal_tolerance_action",
    interval_endpoint_convention = "closed_order_statistic_spacing",
    lower_endpoint = lower_endpoint,
    upper_endpoint = upper_endpoint,
    width = width,
    finite_sample_claim_available = TRUE,
    asymptotic_claim_available = FALSE,
    assumptions_passed = c("iid_continuous_required_by_theory_not_tested_by_code"),
    response_scale_description = "original response scale",
    guarantee_statement =
      "Conditional on the independently selected placement, the main-sample probability spacing has the recorded Beta survival probability.",
    index_convention = gap$index_convention,
    main_sample_not_used_for_pilot_placement = TRUE,
    n_removed = clean$n_removed
  )
  out <- list(
    schema_version = .rqr_tcsp_split_exact_schema(),
    contract = contract,
    calibration = gap,
    pilot_indices = split$pilot_indices,
    main_indices = split$main_indices,
    pilot_fit = pilot$pilot_fit,
    ecm_fit_optional = pilot$ecm_fit,
    pilot_diagnostics = pilot[c(
      "lower_tail_coordinate", "pilot_width", "selected_tilt",
      "method_detail"
    )],
    main_order_statistics = list(
      lower_index = as.integer(lower_index),
      upper_index = as.integer(upper_index),
      lower_endpoint = lower_endpoint,
      upper_endpoint = upper_endpoint,
      width = width
    ),
    provenance = list(
      split_seed = as.integer(split_seed),
      pilot_fit_digest = .rqr_tcsp_digest(pilot$pilot_fit),
      ecm_fit_digest = .rqr_tcsp_digest(pilot$ecm_fit),
      response_digest = .rqr_tcsp_digest(y),
      contract_digest = .rqr_tcsp_digest(contract)
    )
  )
  class(out) <- c("tcsp_split_exact_fit", "rqr_tcsp_split_exact_fit", "list")
  out
}
