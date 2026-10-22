#' Refine an MTI fitted-content grid by effective observation mass
#'
#' Preserves the supplied targets and adds one midpoint for each unrepresented
#' positive effective-mass cell within their range. The mass is
#' `sample_size * q - prior_precision / learning_rate`. This is an opt-in
#' numerical search rule, not a continuous optimum or tolerance guarantee.
#' @param q_grid Existing fitted-content values strictly between content and one.
#' @param sample_size Number of observations, at least two.
#' @param content Requested population content in (0,1).
#' @param learning_rate Positive fixed loss learning rate.
#' @param prior_precision Common positive Gaussian root precision.
#' @param max_added_points Optional nonnegative integer computational budget.
#' @return Original and refined grids, added targets, effective-mass cell
#'   diagnostics, and explicit limitations. No fitting or calibration is done.
#' @export
rqr_mti_refine_content_grid <- function(q_grid, sample_size, content,
    learning_rate = 1, prior_precision = 1e-4, max_added_points = NULL) {
  n <- .rqr_scalar_integer(sample_size, "sample_size", 2L)
  c_target <- .rqr_bayes_assert_probability(.rqr_mti_scalar(content, "content"), "content")
  omega <- .rqr_mti_scalar(learning_rate, "learning_rate", TRUE)
  precision <- .rqr_mti_scalar(prior_precision, "prior_precision", TRUE)
  if (!is.numeric(q_grid) || !length(q_grid) || any(!is.finite(q_grid)) ||
      any(q_grid <= c_target | q_grid >= 1)) {
    stop("q_grid must contain finite values strictly between content and one.", call. = FALSE)
  }
  if (!is.null(max_added_points)) {
    max_added_points <- .rqr_scalar_integer(max_added_points, "max_added_points", 0L)
  }
  eta <- precision / omega
  if (!is.finite(eta)) stop("The prior-to-learning-rate ratio must be finite.", call. = FALSE)
  original <- sort(unique(q_grid))
  mass <- n * original - eta
  positive <- mass > 0
  cells <- data.frame(radius_rank = integer(), mass_lower = numeric(),
    mass_upper = numeric(), original_targets = integer(), added_q = numeric())
  added <- numeric()
  if (any(positive)) {
    # At integer mass k the lower minimizing radius has rank k.
    represented <- ceiling(mass[positive])
    lower_rank <- max(1, ceiling(min(mass)))
    upper_rank <- ceiling(max(mass))
    required <- upper_rank - lower_rank + 1 - length(unique(represented))
    if (!is.null(max_added_points) && required > max_added_points) {
      stop("Content-grid refinement exceeds max_added_points; no partial grid was returned.", call. = FALSE)
    }
    ranks <- seq.int(lower_rank, upper_rank)
    missing <- setdiff(ranks, represented)
    added <- (missing - 0.5 + eta) / n
    if (any(added <= min(original) | added >= max(original) |
            added <= c_target | added >= 1) ||
        any(ceiling(n * added - eta) != missing)) {
      stop("A missing effective-mass cell has no representable interior target.", call. = FALSE)
    }
    cells <- data.frame(radius_rank = ranks, mass_lower = ranks - 1,
      mass_upper = ranks, original_targets = tabulate(match(represented, ranks), length(ranks)),
      added_q = added[match(ranks, missing)])
  }
  list(schema_version = "rqrgibbs_mti_content_grid/1.0.0",
    rule = "one_target_per_unrepresented_positive_effective_mass_cell",
    original_q_grid = original, q_grid = sort(unique(c(original, added))),
    added_q = added, sample_size = n, content = c_target,
    learning_rate = omega, prior_precision = precision, prior_rate_ratio = eta,
    original_effective_mass = mass, cells = cells,
    collapsed_original_targets = sum(!positive),
    original_range_preserved = TRUE, continuous_optimum_claim = FALSE,
    tolerance_claim_available = FALSE)
}
