#' Empirical content of a fixed interval
#'
#' Evaluates the fraction of observed values falling inside a fixed closed
#' interval. This is useful for held-out application checks where the population
#' distribution is unknown.
#'
#' @param y Numeric values used for evaluation.
#' @param lower Lower interval endpoint.
#' @param upper Upper interval endpoint.
#' @param na_rm Remove missing values before evaluation.
#' @return A data frame with the finite evaluation size, endpoints, width,
#'   empirical content, and lower/upper omitted fractions.
#' @export
rqr_interval_empirical_content <- function(y, lower, upper, na_rm = FALSE) {
  y <- .rqr_bayes_clean_y(y, na_rm = na_rm)
  lower <- as.numeric(lower)[1L]
  upper <- as.numeric(upper)[1L]
  if (!is.finite(lower) || !is.finite(upper)) {
    stop("lower and upper must be finite scalar endpoints.", call. = FALSE)
  }
  if (upper < lower) {
    stop("upper cannot be smaller than lower.", call. = FALSE)
  }
  n <- length(y)
  inside <- y >= lower & y <= upper
  below <- y < lower
  above <- y > upper
  data.frame(
    n = as.integer(n),
    lower = lower,
    upper = upper,
    width = upper - lower,
    empirical_content = mean(inside),
    lower_omitted = mean(below),
    upper_omitted = mean(above),
    stringsAsFactors = FALSE
  )
}
