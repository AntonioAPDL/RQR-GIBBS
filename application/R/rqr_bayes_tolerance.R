.rqr_order_interval_candidates <- function(y) {
  y <- sort(as.numeric(y))
  n <- length(y)
  rows <- vector("list", n * (n + 1L) / 2L)
  pos <- 0L
  for (j in seq_len(n)) {
    for (l in j:n) {
      pos <- pos + 1L
      rows[[pos]] <- data.frame(
        lower_index = as.integer(j),
        upper_index = as.integer(l),
        lower = y[[j]],
        upper = y[[l]],
        observed_count = as.integer(l - j + 1L),
        width = y[[l]] - y[[j]]
      )
    }
  }
  do.call(rbind, rows)
}

.rqr_bayes_constraint_status <- function(
    candidates, selected, posterior_threshold, scan_count = NULL) {
  if (is.null(selected) || !nrow(selected)) return("infeasible_within_candidate_class")
  if (is.null(scan_count)) return("binding_or_satisfied")
  scan_only <- candidates[candidates$observed_count >= scan_count, , drop = FALSE]
  if (!nrow(scan_only)) return("infeasible_within_candidate_class")
  scan_best <- scan_only[order(scan_only$width, scan_only$lower_index,
                               scan_only$upper_index), , drop = FALSE][1L, ]
  if (isTRUE(scan_best$posterior_content_probability >= posterior_threshold)) {
    "redundant_given_scan"
  } else {
    "binding"
  }
}

#' Direct-DP Bayesian tolerance action
#'
#' Searches closed order-statistic intervals and returns the first
#' minimum-width interval satisfying the posterior content constraint.
#'
#' @param fit A direct-DP fit.
#' @param content Required population content.
#' @param posterior_confidence Required posterior probability.
#' @param action_class Candidate class.  Only closed order-statistic intervals
#'   are currently implemented.
#' @return A Bayesian action object.
#' @export
rqr_dp_bayes_tolerance_action <- function(
    fit, content, posterior_confidence,
    action_class = "closed_order_statistic_intervals") {
  if (!inherits(fit, "rqr_dp_fit")) {
    stop("fit must come from rqr_dp_fit().", call. = FALSE)
  }
  c_target <- .rqr_bayes_assert_probability(content, "content")
  post_conf <- .rqr_bayes_assert_probability(
    posterior_confidence, "posterior_confidence"
  )
  action_class <- match.arg(
    as.character(action_class)[1L], "closed_order_statistic_intervals"
  )
  candidates <- .rqr_order_interval_candidates(fit$y)
  probs <- rqr_dp_content_probability(
    fit, candidates$lower, candidates$upper, c_target
  )
  candidates$posterior_content_probability <-
    probs$posterior_content_probability
  feasible <- candidates[
    candidates$posterior_content_probability >= post_conf, , drop = FALSE
  ]
  selected <- feasible[order(feasible$width, feasible$lower_index,
                             feasible$upper_index), , drop = FALSE]
  selected <- if (nrow(selected)) selected[1L, , drop = FALSE] else selected
  out <- list(
    schema_version = "rqrgibbs_dp_bayes_tolerance_action/1.0.0",
    action_name = "DP-B",
    action_class = action_class,
    content = c_target,
    posterior_confidence = post_conf,
    posterior_threshold = post_conf,
    selected = selected,
    candidates_evaluated = nrow(candidates),
    feasible_count = nrow(feasible),
    posterior_constraint_status =
      if (nrow(feasible)) "satisfied" else "infeasible_within_candidate_class",
    candidates = candidates
  )
  class(out) <- c("rqr_dp_bayes_tolerance_action", "list")
  out
}

.rqr_hybrid_search_from_probabilities <- function(
    y, posterior_probability, content, posterior_confidence, scan_count,
    action_name, method) {
  c_target <- .rqr_bayes_assert_probability(content, "content")
  post_conf <- .rqr_bayes_assert_probability(
    posterior_confidence, "posterior_confidence"
  )
  candidates <- .rqr_order_interval_candidates(y)
  candidates$posterior_content_probability <- posterior_probability(
    candidates$lower, candidates$upper, c_target
  )
  candidates$scan_constraint_satisfied <- candidates$observed_count >= scan_count
  candidates$posterior_constraint_satisfied <-
    candidates$posterior_content_probability >= post_conf
  feasible <- candidates[
    candidates$scan_constraint_satisfied &
      candidates$posterior_constraint_satisfied, , drop = FALSE
  ]
  selected <- feasible[order(feasible$width, feasible$lower_index,
                             feasible$upper_index), , drop = FALSE]
  selected <- if (nrow(selected)) selected[1L, , drop = FALSE] else selected
  list(
    schema_version = "rqrgibbs_hybrid_bayes_scan_action/1.0.0",
    action_name = action_name,
    method = method,
    action_class = "closed_order_statistic_intervals",
    content = c_target,
    posterior_confidence = post_conf,
    scan_count = as.integer(scan_count),
    selected = selected,
    feasible_count = nrow(feasible),
    candidates_evaluated = nrow(candidates),
    posterior_constraint_status = .rqr_bayes_constraint_status(
      candidates, selected, post_conf, scan_count
    ),
    candidates = candidates
  )
}

