.rqr_weighted_aggregate <- function(x, weights) {
  x <- as.numeric(x)
  weights <- as.numeric(weights)
  if (!length(x) || length(x) != length(weights) ||
      any(!is.finite(x)) || any(!is.finite(weights)) || any(weights < 0) ||
      sum(weights) <= 0) {
    stop("x and weights must be finite, nonnegative, same-length vectors with positive total weight.",
         call. = FALSE)
  }
  weights <- weights / sum(weights)
  ord <- order(x, seq_along(x))
  x <- x[ord]
  weights <- weights[ord]
  groups <- cumsum(c(TRUE, diff(x) != 0))
  values <- as.numeric(tapply(x, groups, `[`, 1L))
  mass <- as.numeric(tapply(weights, groups, sum))
  list(values = values, weights = mass)
}

#' Global shortest interval for a weighted discrete distribution
#'
#' Finds the first global minimum-width closed interval over sorted support
#' atoms whose retained mass is at least `target_content`.
#'
#' @param x Numeric support atoms.
#' @param weights Nonnegative atom weights. They are normalized internally.
#' @param target_content Required retained probability.
#' @param tolerance Numerical tolerance for content and tie comparison.
#' @return A shortest-interval summary.
#' @export
rqr_weighted_shortest_interval <- function(
    x, weights, target_content, tolerance = 1e-12) {
  q <- .rqr_bayes_assert_probability(target_content, "target_content")
  tolerance <- .rqr_bayes_assert_nonnegative(tolerance, "tolerance")
  agg <- .rqr_weighted_aggregate(x, weights)
  values <- agg$values
  weights <- agg$weights
  m <- length(values)
  prefix <- c(0, cumsum(weights))
  best <- NULL
  right <- 1L
  tie_count <- 0L
  for (left in seq_len(m)) {
    if (right < left) right <- left
    while (right <= m && prefix[[right + 1L]] - prefix[[left]] < q - tolerance) {
      right <- right + 1L
    }
    if (right > m) break
    retained <- prefix[[right + 1L]] - prefix[[left]]
    width <- values[[right]] - values[[left]]
    candidate <- list(
      lower_index = as.integer(left),
      upper_index = as.integer(right),
      lower = values[[left]],
      upper = values[[right]],
      width = width,
      retained_mass = retained
    )
    if (is.null(best) || width < best$width - tolerance) {
      best <- candidate
      tie_count <- 1L
    } else if (abs(width - best$width) <= tolerance) {
      tie_count <- tie_count + 1L
    }
  }
  if (is.null(best)) {
    stop("No weighted interval satisfies target_content.", call. = FALSE)
  }
  keep <- seq.int(best$lower_index, best$upper_index)
  retained_mean <- sum(values[keep] * weights[keep]) / best$retained_mass
  full_mean <- sum(values * weights)
  out <- c(best, list(
    retained_mean = retained_mean,
    full_mean = full_mean,
    lower_tail_mass = prefix[[best$lower_index]],
    tilt = retained_mean - full_mean,
    tie_count = as.integer(tie_count),
    tie_rule = "deterministic_first_minimum_by_lower_index",
    boundary_status = if (best$lower_index == 1L && best$upper_index == m) {
      "both"
    } else if (best$lower_index == 1L) {
      "lower"
    } else if (best$upper_index == m) {
      "upper"
    } else {
      "interior"
    },
    support_size = as.integer(m),
    target_content = q
  ))
  class(out) <- c("rqr_weighted_shortest_interval", "list")
  out
}

.rqr_weighted_shortest_bruteforce <- function(
    x, weights, target_content, tolerance = 1e-12) {
  q <- .rqr_bayes_assert_probability(target_content, "target_content")
  agg <- .rqr_weighted_aggregate(x, weights)
  values <- agg$values
  weights <- agg$weights
  m <- length(values)
  best <- NULL
  tie_count <- 0L
  for (left in seq_len(m)) {
    retained <- 0
    for (right in left:m) {
      retained <- retained + weights[[right]]
      if (retained >= q - tolerance) {
        width <- values[[right]] - values[[left]]
        if (is.null(best) || width < best$width - tolerance) {
          best <- list(left = left, right = right, width = width,
                       retained = retained)
          tie_count <- 1L
        } else if (abs(width - best$width) <= tolerance) {
          tie_count <- tie_count + 1L
        }
      }
    }
  }
  list(best = best, tie_count = tie_count)
}

