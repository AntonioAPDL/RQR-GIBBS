.rqr_mti_ecm_adaptive_schema <- function() {
  "rqrgibbs_mti_ecm_adaptive/1.0.0"
}

.rqr_named_list <- function(x) {
  if (is.null(x)) list() else x
}

.rqr_probability_vector <- function(x, name) {
  x <- as.numeric(unlist(x, use.names = FALSE))
  if (!length(x) || any(!is.finite(x)) || any(x <= 0 | x >= 1)) {
    stop(sprintf("%s must contain finite probabilities in (0, 1).", name),
         call. = FALSE)
  }
  sort(unique(x))
}

.rqr_numeric_vector <- function(x, name, allow_empty = FALSE) {
  x <- as.numeric(unlist(x, use.names = FALSE))
  if (!length(x)) {
    if (isTRUE(allow_empty)) return(numeric())
    stop(sprintf("%s must contain at least one value.", name), call. = FALSE)
  }
  if (any(!is.finite(x))) {
    stop(sprintf("%s must contain finite numeric values.", name),
         call. = FALSE)
  }
  x
}

.rqr_delivery_bound_method <- function(method) {
  method <- tolower(as.character(method)[1L])
  choices <- c("clopper_pearson", "wilson", "jeffreys")
  if (!method %in% choices) {
    stop("method must be one of clopper_pearson, wilson, or jeffreys.",
         call. = FALSE)
  }
  method
}

#' Lower confidence bounds for repeated-sample delivery
#'
#' Computes a one-sided lower confidence bound for a binomial delivery
#' probability. The function is used to calibrate adaptive MTI-ECM policies
#' without hard-coding an observed-delivery cutoff.
#'
#' @param successes Number of successful replications.
#' @param replications Number of replications.
#' @param confidence One-sided confidence level for the lower bound.
#' @param method Bound construction. One of `"clopper_pearson"`, `"wilson"`,
#'   or `"jeffreys"`.
#' @return Numeric lower bound.
#' @export
rqr_delivery_lower_bound <- function(
    successes, replications, confidence = 0.95,
    method = c("clopper_pearson", "wilson", "jeffreys")) {
  method <- .rqr_delivery_bound_method(method[[1L]])
  successes <- as.numeric(successes)[1L]
  replications <- .rqr_mt_assert_count(replications, "replications", 1L)
  confidence <- .rqr_bayes_assert_probability(confidence, "confidence")
  if (!is.finite(successes) || successes != floor(successes) ||
      successes < 0 || successes > replications) {
    stop("successes must be an integer in [0, replications].",
         call. = FALSE)
  }
  successes <- as.integer(successes)
  alpha <- 1 - confidence
  if (successes == 0L) return(0)
  if (identical(method, "clopper_pearson")) {
    return(stats::qbeta(alpha, successes, replications - successes + 1))
  }
  if (identical(method, "jeffreys")) {
    return(stats::qbeta(alpha, successes + 0.5,
                        replications - successes + 0.5))
  }
  z <- stats::qnorm(confidence)
  n <- replications
  phat <- successes / n
  denom <- 1 + z^2 / n
  center <- phat + z^2 / (2 * n)
  radius <- z * sqrt(phat * (1 - phat) / n + z^2 / (4 * n^2))
  max(0, (center - radius) / denom)
}

#' Minimum successes required by a delivery calibration rule
#'
#' Finds the smallest success count whose lower confidence bound is at least
#' the requested target plus a configurable margin.
#'
#' @param replications Number of calibration or validation replications.
#' @param target Target repeated-sample delivery probability.
#' @param confidence One-sided confidence level for the lower bound.
#' @param method Bound construction.
#' @param margin Nonnegative margin added to `target`.
#' @return Integer minimum success count, or `NA_integer_` if the target is
#'   impossible under the requested margin.
#' @export
rqr_delivery_min_successes <- function(
    replications, target, confidence = 0.95,
    method = c("clopper_pearson", "wilson", "jeffreys"),
    margin = 0) {
  method <- .rqr_delivery_bound_method(method[[1L]])
  replications <- .rqr_mt_assert_count(replications, "replications", 1L)
  target <- .rqr_bayes_assert_probability(target, "target")
  confidence <- .rqr_bayes_assert_probability(confidence, "confidence")
  margin <- .rqr_bayes_assert_nonnegative(margin, "margin")
  threshold <- target + margin
  if (!is.finite(threshold) || threshold >= 1) return(NA_integer_)
  for (successes in 0:replications) {
    lower <- rqr_delivery_lower_bound(
      successes, replications, confidence = confidence, method = method
    )
    if (lower >= threshold) return(as.integer(successes))
  }
  NA_integer_
}

rqr_mti_ecm_sample_diagnostics <- function(y) {
  y <- .rqr_bayes_clean_y(y, na_rm = FALSE)
  med <- stats::median(y)
  q <- stats::quantile(y, c(0.10, 0.25, 0.75, 0.90),
                       names = FALSE, type = 8)
  iqr <- q[[3L]] - q[[2L]]
  if (!is.finite(iqr) || iqr <= 0) iqr <- stats::sd(y)
  if (!is.finite(iqr) || iqr <= 0) iqr <- 1
  left_tail <- med - q[[1L]]
  right_tail <- q[[4L]] - med
  bowley <- ((q[[3L]] + q[[2L]] - 2 * med) /
               max(q[[3L]] - q[[2L]], .Machine$double.eps))
  data.frame(
    n = length(y),
    median = med,
    robust_scale = iqr,
    bowley_skewness = bowley,
    robust_tail_ratio = right_tail / max(left_tail, .Machine$double.eps),
    stringsAsFactors = FALSE
  )
}

.rqr_mti_ecm_policy_cell_key <- function(n, content, tolerance_confidence) {
  sprintf(
    "n%04d_c%s_t%s",
    as.integer(n),
    gsub("\\.", "", sprintf("%.3f", as.numeric(content))),
    gsub("\\.", "", sprintf("%.3f", as.numeric(tolerance_confidence)))
  )
}

.rqr_mti_ecm_grid_from_config <- function(
    content, q_anchor, n, policy_config, diagnostics) {
  c_target <- .rqr_bayes_assert_probability(content, "content")
  n <- .rqr_mt_assert_count(n, "n", 2L)
  policy_config <- .rqr_named_list(policy_config)
  offsets <- .rqr_numeric_vector(
    policy_config$q_offsets %||%
      c(0, -0.001, -0.0025, -0.005, -0.01, -0.02),
    "policy_config$q_offsets"
  )
  q_buffer <- as.numeric(
    policy_config$q_min_buffer %||% max(1e-4, 0.25 / n)
  )[1L]
  if (!is.finite(q_buffer) || q_buffer <= 0) {
    stop("policy_config$q_min_buffer must be finite and positive.",
         call. = FALSE)
  }
  q_max <- as.numeric(policy_config$q_max %||% 0.9995)[1L]
  if (!is.finite(q_max) || q_max <= c_target || q_max >= 1) {
    stop("policy_config$q_max must be one finite value in (content, 1).",
         call. = FALSE)
  }
  q_anchor <- as.numeric(q_anchor)[1L]
  if (!is.finite(q_anchor) || q_anchor <= c_target || q_anchor >= 1) {
    q_anchor <- min(q_max, c_target + max(2 * q_buffer, 1 / n))
  }
  q_grid <- q_anchor + offsets
  boundary_cut <- as.numeric(policy_config$boundary_anchor_cutoff %||% 0.995)[1L]
  full_boundary <- isTRUE(diagnostics$tcsp_full_sample[[1L]] %||% FALSE)
  if ((is.finite(boundary_cut) && q_anchor >= boundary_cut) ||
      isTRUE(full_boundary)) {
    q_grid <- c(
      q_grid,
      .rqr_numeric_vector(
        policy_config$boundary_q_values %||% c(0.995, 0.9975, 0.999, 0.9995),
        "policy_config$boundary_q_values",
        allow_empty = TRUE
      )
    )
  }
  lower <- c_target + q_buffer
  q_grid <- q_grid[is.finite(q_grid) & q_grid > lower & q_grid < 1]
  q_grid <- pmin(q_grid, q_max)
  q_grid <- q_grid[q_grid > c_target & q_grid < 1]
  q_grid <- .rqr_mti_profile_unique_numeric(q_grid)
  if (!length(q_grid)) {
    q_grid <- .rqr_mti_profile_q_grid(
      n = n,
      content = c_target,
      scan_target_content = q_anchor,
      n_points = as.integer(policy_config$fallback_n_points %||% 5L),
      q_min_buffer = q_buffer,
      q_max = q_max,
      include_scan_target = TRUE
    )
  }
  q_grid
}

#' Build an adaptive MTI-ECM profile menu
#'
#' Constructs the fitted-content grid and direct-DP screen for an adaptive
#' MTI-ECM profile action. The function is deterministic for fixed inputs and
#' does not select a final interval.
#'
#' @param y Numeric response sample.
#' @param content Required population content.
#' @param tolerance_confidence Target tolerance confidence.
#' @param scan_target_content Optional TCSP retained fraction used as the
#'   fitted-content anchor.
#' @param policy Policy name. Currently `"global"` and `"cell"` use configured
#'   rules, while `"diagnostic"` records observable diagnostics for future
#'   calibrated rules.
#' @param policy_config List of menu parameters.
#' @param calibration_rule Optional one-row calibration rule with `screen`,
#'   `q_grid`, or `menu_config` fields.
#' @param na_rm Remove missing responses.
#' @return A list containing `q_grid`, `posterior_confidence`,
#'   `tilt_grid_control`, `dp_concentration`, diagnostics, and provenance.
#' @export
rqr_mti_ecm_adaptive_profile_menu <- function(
    y, content, tolerance_confidence, scan_target_content = NA_real_,
    policy = c("cell", "global", "diagnostic"), policy_config = list(),
    calibration_rule = NULL, na_rm = FALSE) {
  y <- .rqr_bayes_clean_y(y, na_rm = na_rm)
  if (length(y) < 2L) stop("At least two observations are required.",
                           call. = FALSE)
  c_target <- .rqr_bayes_assert_probability(content, "content")
  tol_conf <- .rqr_bayes_assert_probability(
    tolerance_confidence, "tolerance_confidence"
  )
  policy <- match.arg(policy)
  policy_config <- .rqr_named_list(policy_config)
  calibration_rule <- .rqr_named_list(calibration_rule)
  n <- length(y)
  scan_q <- as.numeric(scan_target_content)[1L]
  q_anchor <- scan_q
  diagnostics <- rqr_mti_ecm_sample_diagnostics(y)
  diagnostics$target_content <- c_target
  diagnostics$tolerance_confidence <- tol_conf
  diagnostics$q_anchor <- if (is.finite(q_anchor)) q_anchor else NA_real_
  diagnostics$tcsp_full_sample <- is.finite(q_anchor) && q_anchor >= 1 - 1e-12
  diagnostics$cell_key <- .rqr_mti_ecm_policy_cell_key(n, c_target, tol_conf)

  rule_screen <- calibration_rule$screen %||%
    calibration_rule$posterior_confidence %||%
    policy_config$posterior_confidence %||%
    policy_config$screen %||%
    tol_conf
  posterior_confidence <- .rqr_bayes_assert_probability(
    rule_screen, "adaptive posterior_confidence"
  )

  q_grid <- calibration_rule$q_grid %||% NULL
  if (is.character(q_grid) && length(q_grid) == 1L) {
    q_grid <- strsplit(q_grid, ",", fixed = TRUE)[[1L]]
  }
  if (!is.null(q_grid)) {
    q_grid <- .rqr_probability_vector(q_grid, "calibration_rule$q_grid")
    q_grid <- q_grid[q_grid > c_target]
    if (!length(q_grid)) {
      stop("calibration_rule$q_grid has no values above content.",
           call. = FALSE)
    }
  } else {
    q_grid <- .rqr_mti_ecm_grid_from_config(
      content = c_target,
      q_anchor = q_anchor,
      n = n,
      policy_config = policy_config,
      diagnostics = diagnostics
    )
  }

  tilt_grid_control <- policy_config$tilt_grid_control %||% list()
  if (!is.null(calibration_rule$tilt_grid_control) &&
      is.list(calibration_rule$tilt_grid_control)) {
    tilt_grid_control <- calibration_rule$tilt_grid_control
  }
  dp_concentration <- as.numeric(
    calibration_rule$dp_concentration %||%
      policy_config$dp_concentration %||% 1
  )[1L]
  if (!is.finite(dp_concentration) || dp_concentration <= 0) {
    stop("Adaptive MTI-ECM DP concentration must be positive.",
         call. = FALSE)
  }
  menu <- list(
    schema_version = .rqr_mti_ecm_adaptive_schema(),
    policy = policy,
    policy_id = as.character(policy_config$policy_id %||% policy),
    menu_id = as.character(
      calibration_rule$menu_id %||% policy_config$menu_id %||% policy
    ),
    cell_key = diagnostics$cell_key[[1L]],
    content = c_target,
    tolerance_confidence = tol_conf,
    posterior_confidence = posterior_confidence,
    q_anchor = q_anchor,
    q_grid = q_grid,
    tilt_grid_control = tilt_grid_control,
    dp_concentration = dp_concentration,
    diagnostics = diagnostics
  )
  menu$provenance_digest <- .rqr_bayes_digest(menu)
  class(menu) <- c("rqr_mti_ecm_adaptive_profile_menu", "list")
  menu
}
