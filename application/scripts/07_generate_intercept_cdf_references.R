#!/usr/bin/env Rscript

# Reproduce the event-boundary-aware CDF references for the deterministic
# intercept-only bounded pilot.  The integrations are over the independent
# N(0, 25) root priors transformed to probability coordinates.  Response-loss
# kinks and CDF-event boundaries are split explicitly.

if (!requireNamespace("pracma", quietly = TRUE) ||
    !requireNamespace("digest", quietly = TRUE)) {
  stop("Packages pracma and digest are required.", call. = FALSE)
}

args <- commandArgs(trailingOnly = TRUE)
output_path <- if (length(args)) args[[1L]] else {
  file.path(
    getwd(), "application", "inst", "extdata",
    "output7_corrected_cdf_references.csv"
  )
}
orders <- c(48L, 64L, 80L)
y <- c(
  -2.0, -1.3, -0.8, -0.4, -0.1, 0.1,
   0.35, 0.7, 1.1, 1.6, 2.2, 3.0
)
alpha <- 0.80
prior_sd <- 5
lambda_shape <- 4
lambda_rate <- 4
marginal_shape <- lambda_shape + length(y)
response_cuts <- sort(unique(c(0, stats::pnorm(y / prior_sd), 1)))
gauss_cache <- new.env(parent = emptyenv())

gauss_rule <- function(order, lower, upper) {
  key <- as.character(order)
  if (!exists(key, envir = gauss_cache, inherits = FALSE)) {
    assign(
      key, pracma::gaussLegendre(order, -1, 1),
      envir = gauss_cache
    )
  }
  standard <- get(key, envir = gauss_cache, inherits = FALSE)
  half_width <- 0.5 * (upper - lower)
  midpoint <- 0.5 * (upper + lower)
  list(
    x = midpoint + half_width * standard$x,
    w = half_width * standard$w
  )
}

root_weight <- function(probability1, probability2) {
  root1 <- stats::qnorm(probability1) * prior_sd
  root2 <- stats::qnorm(probability2) * prior_sd
  loss <- numeric(length(root1))
  for (response in y) {
    residual_product <- (response - root1) * (response - root2)
    loss <- loss + residual_product *
      (alpha - as.numeric(residual_product < 0))
  }
  rate <- lambda_rate + loss
  list(
    root1 = root1, root2 = root2, rate = rate,
    weight = rate^(-marginal_shape)
  )
}

integrate_rectangles <- function(
    order, lower1 = 0, upper1 = 1, lower2 = 0, upper2 = 1,
    functional = function(values) 1) {
  cuts1 <- sort(unique(c(
    lower1, response_cuts[
      response_cuts > lower1 & response_cuts < upper1
    ], upper1
  )))
  cuts2 <- sort(unique(c(
    lower2, response_cuts[
      response_cuts > lower2 & response_cuts < upper2
    ], upper2
  )))
  if (lower1 >= upper1 || lower2 >= upper2) return(0)
  value <- 0
  for (ii in seq_len(length(cuts1) - 1L)) {
    nodes1 <- gauss_rule(order, cuts1[ii], cuts1[ii + 1L])
    for (jj in seq_len(length(cuts2) - 1L)) {
      nodes2 <- gauss_rule(
        order, cuts2[jj], cuts2[jj + 1L]
      )
      grid <- expand.grid(
        probability1 = nodes1$x,
        probability2 = nodes2$x
      )
      values <- root_weight(grid$probability1, grid$probability2)
      weights <- as.vector(outer(nodes1$w, nodes2$w))
      value <- value + sum(
        weights * values$weight * functional(values)
      )
    }
  }
  value
}

integrate_nested <- function(
    order, inner_limits, extra_outer_cuts = numeric(0)) {
  outer_cuts <- sort(unique(c(
    response_cuts,
    extra_outer_cuts[
      extra_outer_cuts > 0 & extra_outer_cuts < 1
    ]
  )))
  total <- 0
  for (ii in seq_len(length(outer_cuts) - 1L)) {
    outer <- gauss_rule(
      order, outer_cuts[ii], outer_cuts[ii + 1L]
    )
    inner_values <- numeric(length(outer$x))
    for (kk in seq_along(outer$x)) {
      root1 <- stats::qnorm(outer$x[kk]) * prior_sd
      limits <- inner_limits(root1)
      lower <- max(0, limits[1L])
      upper <- min(1, limits[2L])
      if (lower >= upper) next
      inner_cuts <- sort(unique(c(
        lower,
        response_cuts[
          response_cuts > lower & response_cuts < upper
        ],
        upper
      )))
      for (jj in seq_len(length(inner_cuts) - 1L)) {
        inner <- gauss_rule(
          order, inner_cuts[jj], inner_cuts[jj + 1L]
        )
        values <- root_weight(
          rep(outer$x[kk], length(inner$x)), inner$x
        )
        inner_values[kk] <- inner_values[kk] +
          sum(inner$w * values$weight)
      }
    }
    total <- total + sum(outer$w * inner_values)
  }
  total
}

compute_references <- function(order) {
  denominator <- integrate_rectangles(order)
  lower_threshold <- -1.5
  lower_probability <- stats::pnorm(lower_threshold / prior_sd)
  both_above <- integrate_rectangles(
    order,
    lower1 = lower_probability, upper1 = 1,
    lower2 = lower_probability, upper2 = 1
  )
  upper_threshold <- 2.5
  upper_probability <- stats::pnorm(upper_threshold / prior_sd)
  both_below <- integrate_rectangles(
    order,
    lower1 = 0, upper1 = upper_probability,
    lower2 = 0, upper2 = upper_probability
  )
  width_threshold <- 4
  width_numerator <- integrate_nested(
    order,
    inner_limits = function(root1) {
      stats::pnorm(
        c(root1 - width_threshold, root1 + width_threshold) / prior_sd
      )
    },
    extra_outer_cuts = stats::pnorm(
      c(y - width_threshold, y + width_threshold) / prior_sd
    )
  )
  midpoint_threshold <- 0.5
  midpoint_numerator <- integrate_nested(
    order,
    inner_limits = function(root1) {
      c(0, stats::pnorm(
        (2 * midpoint_threshold - root1) / prior_sd
      ))
    },
    extra_outer_cuts = stats::pnorm(
      (2 * midpoint_threshold - y) / prior_sd
    )
  )
  lambda_numerator <- integrate_rectangles(
    order,
    functional = function(values) {
      stats::pgamma(
        1, shape = marginal_shape, rate = values$rate
      )
    }
  )
  c(
    lambda = lambda_numerator / denominator,
    lower_root = 1 - both_above / denominator,
    upper_root = both_below / denominator,
    width = width_numerator / denominator,
    midpoint = midpoint_numerator / denominator
  )
}

order_results <- vapply(orders, compute_references, numeric(5L))
reference <- order_results[, ncol(order_results)]
previous <- order_results[, ncol(order_results) - 1L]
convergence_difference <- abs(reference - previous)
if (any(!is.finite(reference)) ||
    any(convergence_difference > 5e-10)) {
  stop(
    "Event-boundary quadrature did not meet its order-convergence gate.",
    call. = FALSE
  )
}
thresholds <- c(
  lambda = 1, lower_root = -1.5, upper_root = 2.5,
  width = 4, midpoint = 0.5
)
methods <- c(
  lambda = paste(
    "conditional Gamma CDF integrated over response-kink-split",
    "root probability cells"
  ),
  lower_root = paste(
    "root probability axes split at responses and Phi(-1.5/5)"
  ),
  upper_root = paste(
    "root probability axes split at responses and Phi(2.5/5)"
  ),
  width = paste(
    "nested Gauss-Legendre with exact |root1-root2|<=4 limits",
    "and response-kink splits"
  ),
  midpoint = paste(
    "nested Gauss-Legendre with exact root1+root2<=1 limit",
    "and response-kink splits"
  )
)
generator_path <- normalizePath(
  sub("^--file=", "", grep(
    "^--file=", commandArgs(trailingOnly = FALSE), value = TRUE
  )[1L]),
  winslash = "/", mustWork = TRUE
)
result <- data.frame(
  reference_schema = "rqrgibbs_intercept_cdf_reference/2.0.0",
  comparison_type = "cdf",
  estimand = names(reference),
  threshold = unname(thresholds[names(reference)]),
  reference_value = unname(reference),
  reference_method = unname(methods[names(reference)]),
  quadrature_order = max(orders),
  previous_order = orders[length(orders) - 1L],
  order_convergence_difference = unname(convergence_difference),
  generator = "application/scripts/07_generate_intercept_cdf_references.R",
  generator_sha256 = digest::digest(
    file = generator_path, algo = "sha256", serialize = FALSE
  ),
  stringsAsFactors = FALSE
)
old_digits <- getOption("digits")
options(digits = 17)
utils::write.csv(result, output_path, row.names = FALSE)
options(digits = old_digits)
print(order_results, digits = 16)
cat("Wrote:", normalizePath(output_path, winslash = "/", mustWork = TRUE), "\n")
