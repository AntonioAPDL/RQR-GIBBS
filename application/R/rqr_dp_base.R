.rqr_dp_base_validate <- function(base_measure) {
  required <- c(
    "cdf", "quantile", "random_draw", "support",
    "is_data_dependent", "provenance_digest"
  )
  missing <- setdiff(required, names(base_measure))
  if (length(missing)) {
    stop("base_measure is missing field(s): ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  if (!is.function(base_measure$cdf) ||
      !is.function(base_measure$quantile) ||
      !is.function(base_measure$random_draw)) {
    stop("base_measure cdf, quantile, and random_draw fields must be functions.",
         call. = FALSE)
  }
  if (!is.logical(base_measure$is_data_dependent) ||
      length(base_measure$is_data_dependent) != 1L ||
      is.na(base_measure$is_data_dependent)) {
    stop("base_measure$is_data_dependent must be TRUE or FALSE.",
         call. = FALSE)
  }
  base_measure
}

.rqr_dp_base_mass <- function(base_measure, lower, upper) {
  base_measure <- .rqr_dp_base_validate(base_measure)
  lower <- as.numeric(lower)
  upper <- as.numeric(upper)
  if (length(lower) != length(upper) || any(!is.finite(lower)) ||
      any(!is.finite(upper)) || any(lower > upper)) {
    stop("lower and upper must be finite vectors with lower <= upper.",
         call. = FALSE)
  }
  pmax(0, pmin(1, base_measure$cdf(upper) - base_measure$cdf(lower)))
}

.rqr_dp_base_object <- function(name, parameters, cdf, quantile, random_draw,
                                log_density = NULL, support = c(-Inf, Inf),
                                is_data_dependent = FALSE) {
  contract <- list(
    schema_version = "rqrgibbs_dp_base_measure/1.0.0",
    name = name,
    parameters = parameters,
    support = support,
    is_data_dependent = isTRUE(is_data_dependent)
  )
  out <- list(
    schema_version = contract$schema_version,
    name = name,
    parameters = parameters,
    cdf = cdf,
    quantile = quantile,
    random_draw = random_draw,
    log_density = log_density,
    support = support,
    is_data_dependent = isTRUE(is_data_dependent),
    provenance_digest = .rqr_bayes_digest(contract)
  )
  class(out) <- c("rqr_dp_base_measure", "list")
  out
}

#' Direct-DP Gaussian base measure
#'
#' @param mean,sd Mean and standard deviation of the Gaussian centering
#'   distribution.
#' @return An `rqr_dp_base_measure`.
#' @export
rqr_dp_base_normal <- function(mean = 0, sd = 1) {
  mean <- as.numeric(mean)[1L]
  sd <- .rqr_bayes_assert_positive(sd, "sd")
  if (!is.finite(mean)) stop("mean must be finite.", call. = FALSE)
  .rqr_dp_base_object(
    name = "normal",
    parameters = list(mean = mean, sd = sd),
    cdf = function(x) stats::pnorm(x, mean = mean, sd = sd),
    quantile = function(p) stats::qnorm(p, mean = mean, sd = sd),
    random_draw = function(n) stats::rnorm(n, mean = mean, sd = sd),
    log_density = function(x) stats::dnorm(x, mean = mean, sd = sd, log = TRUE),
    support = c(-Inf, Inf),
    is_data_dependent = FALSE
  )
}

#' Direct-DP Student-t base measure
#'
#' @param location,scale,df Location, scale, and degrees of freedom.
#' @return An `rqr_dp_base_measure`.
#' @export
rqr_dp_base_student_t <- function(location = 0, scale = 1, df = 5) {
  location <- as.numeric(location)[1L]
  scale <- .rqr_bayes_assert_positive(scale, "scale")
  df <- .rqr_bayes_assert_positive(df, "df")
  if (!is.finite(location)) stop("location must be finite.", call. = FALSE)
  .rqr_dp_base_object(
    name = "student_t",
    parameters = list(location = location, scale = scale, df = df),
    cdf = function(x) stats::pt((x - location) / scale, df = df),
    quantile = function(p) location + scale * stats::qt(p, df = df),
    random_draw = function(n) location + scale * stats::rt(n, df = df),
    log_density = function(x) stats::dt((x - location) / scale, df = df,
                                        log = TRUE) - log(scale),
    support = c(-Inf, Inf),
    is_data_dependent = FALSE
  )
}

#' Empirical-Bayes direct-DP Gaussian base measure
#'
#' Constructs a data-dependent Gaussian centering distribution.  It is useful
#' for sensitivity analysis but rejected by `rqr_dp_fit(strict_bayes = TRUE)`.
#'
#' @param y Response vector.
#' @param scale_multiplier Positive multiplier for the sample standard
#'   deviation.
#' @param na_rm Remove missing values.
#' @return An `rqr_dp_base_measure` marked data dependent.
#' @export
rqr_dp_base_empirical_normal <- function(
    y, scale_multiplier = 2, na_rm = FALSE) {
  y <- .rqr_bayes_clean_y(y, na_rm = na_rm)
  scale_multiplier <- .rqr_bayes_assert_positive(
    scale_multiplier, "scale_multiplier"
  )
  s <- stats::sd(y)
  if (!is.finite(s) || s <= 0) s <- 1
  mean_y <- mean(y)
  sd_y <- scale_multiplier * s
  warning(
    paste(
      "rqr_dp_base_empirical_normal() is data dependent.",
      "Use only for empirical-Bayes sensitivity analysis, not strict Bayesian claims."
    ),
    call. = FALSE
  )
  out <- rqr_dp_base_normal(mean = mean_y, sd = sd_y)
  out$name <- "empirical_normal"
  out$parameters <- list(
    mean = mean_y, sd = sd_y, scale_multiplier = scale_multiplier
  )
  out$is_data_dependent <- TRUE
  out$provenance_digest <- .rqr_bayes_digest(list(
    schema_version = out$schema_version,
    name = out$name,
    parameters = out$parameters,
    is_data_dependent = TRUE
  ))
  out
}

