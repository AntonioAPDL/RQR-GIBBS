#' Fit a conjugate direct Dirichlet-process posterior
#'
#' The model is `F ~ DP(a, H)` and `Y_i | F iid F`.  This is a response
#' distribution model, not an RQR generalized posterior.
#'
#' @param y Numeric univariate responses.
#' @param concentration Positive DP concentration `a`.
#' @param base_measure Base measure from `rqr_dp_base_normal()` or a compatible
#'   object.
#' @param strict_bayes If `TRUE`, reject data-dependent base measures.
#' @param na_rm Remove missing responses.
#' @return An `rqr_dp_fit` object.
#' @export
rqr_dp_fit <- function(y, concentration, base_measure,
                       strict_bayes = TRUE, na_rm = FALSE) {
  y <- .rqr_bayes_clean_y(y, na_rm = na_rm)
  a <- .rqr_bayes_assert_positive(concentration, "concentration")
  base_measure <- .rqr_dp_base_validate(base_measure)
  strict_bayes <- isTRUE(strict_bayes)
  if (strict_bayes && isTRUE(base_measure$is_data_dependent)) {
    stop("strict_bayes = TRUE rejects data-dependent DP base measures.",
         call. = FALSE)
  }
  y_sorted <- sort(y)
  out <- list(
    schema_version = "rqrgibbs_direct_dp_fit/1.0.0",
    model = "direct_dirichlet_process_response_distribution",
    y = y,
    y_sorted = y_sorted,
    sample_size = length(y),
    concentration = a,
    posterior_concentration = a + length(y),
    base_measure = base_measure,
    strict_bayes = strict_bayes,
    response_likelihood = TRUE,
    generalized_bayes = FALSE,
    posterior_predictive_distribution_available = TRUE,
    posterior_predictive_description =
      "Discrete Polya predictive: a/(a+n) H + empirical atoms/(a+n).",
    provenance = .rqr_bayes_provenance(extra = list(
      base_measure_digest = base_measure$provenance_digest
    ))
  )
  out$provenance_digest <- .rqr_bayes_digest(list(
    y_digest = .rqr_bayes_digest(y),
    concentration = a,
    base_measure_digest = base_measure$provenance_digest,
    strict_bayes = strict_bayes
  ))
  class(out) <- c("dp_fit", "rqr_dp_fit", "distribution_fit",
                  "rqr_distribution_fit", "list")
  out
}

#' Direct-DP posterior content probability
#'
#' Computes `Pr{F([lower, upper]) >= content | y}` exactly from the Beta
#' posterior law for fixed intervals.
#'
#' @param fit A direct-DP fit from `rqr_dp_fit()`.
#' @param lower,upper Interval endpoints.
#' @param content Required population content.
#' @return A data frame with Beta parameters and posterior probabilities.
#' @export
rqr_dp_content_probability <- function(fit, lower, upper, content) {
  if (!inherits(fit, "rqr_dp_fit")) {
    stop("fit must come from rqr_dp_fit().", call. = FALSE)
  }
  c_target <- .rqr_bayes_assert_probability(content, "content")
  lower <- as.numeric(lower)
  upper <- as.numeric(upper)
  if (length(lower) != length(upper)) {
    stop("lower and upper must have the same length.", call. = FALSE)
  }
  if (any(!is.finite(lower)) || any(!is.finite(upper)) ||
      any(lower > upper)) {
    stop("lower and upper must be finite with lower <= upper.",
         call. = FALSE)
  }
  n_in <- vapply(seq_along(lower), function(ii) {
    sum(fit$y >= lower[[ii]] & fit$y <= upper[[ii]])
  }, integer(1L))
  h_mass <- .rqr_dp_base_mass(fit$base_measure, lower, upper)
  shape1 <- fit$concentration * h_mass + n_in
  shape2 <- fit$concentration * (1 - h_mass) + fit$sample_size - n_in
  probability <- stats::pbeta(
    c_target, shape1 = shape1, shape2 = shape2, lower.tail = FALSE
  )
  data.frame(
    lower = lower,
    upper = upper,
    content = c_target,
    observed_count = as.integer(n_in),
    base_mass = h_mass,
    beta_shape1 = shape1,
    beta_shape2 = shape2,
    posterior_mean_content = shape1 / (shape1 + shape2),
    posterior_var_content = shape1 * shape2 /
      ((shape1 + shape2)^2 * (shape1 + shape2 + 1)),
    posterior_content_probability = probability
  )
}

print.rqr_dp_fit <- function(x, ...) {
  cat(
    sprintf(
      "Direct DP shortest-UQ fit: n=%d, concentration=%s, base=%s\n",
      x$sample_size,
      format(x$concentration, digits = 4),
      x$base_measure$name
    )
  )
  invisible(x)
}
