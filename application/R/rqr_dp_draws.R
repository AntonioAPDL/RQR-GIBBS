.rqr_dp_sample_atom <- function(fit, n) {
  prob_base <- fit$concentration / fit$posterior_concentration
  from_base <- stats::runif(n) < prob_base
  atoms <- numeric(n)
  if (any(from_base)) {
    atoms[from_base] <- fit$base_measure$random_draw(sum(from_base))
  }
  if (any(!from_base)) {
    atoms[!from_base] <- sample(fit$y, sum(!from_base), replace = TRUE)
  }
  atoms
}

#' Draw from a direct-DP posterior
#'
#' Uses an adaptive truncated Sethuraman representation of
#' `DP(a + n, H_n)`.  Draws are independent across posterior iterations.
#'
#' @param fit A direct-DP fit.
#' @param n_draws Number of posterior distribution draws.
#' @param residual_mass_tol Stop each stick-breaking draw when residual mass is
#'   below this tolerance.
#' @param seed Optional master seed.
#' @param max_atoms Safety cap on atoms per posterior draw.
#' @param parallel Reserved; direct serial reference path is used initially.
#' @return An `rqr_dp_draws` object.
#' @export
rqr_dp_draws <- function(fit, n_draws, residual_mass_tol = 1e-4,
                         seed = NULL, max_atoms = 20000L,
                         parallel = FALSE) {
  if (!inherits(fit, "rqr_dp_fit")) {
    stop("fit must come from rqr_dp_fit().", call. = FALSE)
  }
  n_draws <- .rqr_mt_assert_count(n_draws, "n_draws", 1L)
  residual_mass_tol <- .rqr_bayes_assert_positive(
    residual_mass_tol, "residual_mass_tol"
  )
  max_atoms <- .rqr_mt_assert_count(max_atoms, "max_atoms", 1L)
  if (isTRUE(parallel)) {
    stop("parallel DP draws are reserved; use the deterministic serial reference path.",
         call. = FALSE)
  }
  rng_kind <- RNGkind()
  restore_rng <- .rqr_mt_seed_scope(seed)
  on.exit(restore_rng(), add = TRUE)
  draws <- vector("list", n_draws)
  residuals <- numeric(n_draws)
  atom_counts <- integer(n_draws)
  for (s in seq_len(n_draws)) {
    residual <- 1
    weights <- numeric()
    while (residual > residual_mass_tol && length(weights) < max_atoms) {
      v <- stats::rbeta(1L, 1, fit$posterior_concentration)
      w <- residual * v
      weights <- c(weights, w)
      residual <- residual * (1 - v)
    }
    if (residual > residual_mass_tol) {
      stop("DP stick-breaking draw hit max_atoms before residual_mass_tol.",
           call. = FALSE)
    }
    atoms <- .rqr_dp_sample_atom(fit, length(weights))
    draws[[s]] <- list(
      atoms = atoms,
      weights = weights / sum(weights),
      residual_mass = residual,
      atom_count = length(weights)
    )
    residuals[[s]] <- residual
    atom_counts[[s]] <- length(weights)
  }
  out <- list(
    schema_version = "rqrgibbs_direct_dp_draws/1.0.0",
    engine = "direct_dp",
    fit_digest = fit$provenance_digest,
    n_draws = as.integer(n_draws),
    residual_mass_tol = residual_mass_tol,
    residual_mass = residuals,
    atom_count = atom_counts,
    seed = if (is.null(seed)) NA_integer_ else as.integer(seed),
    rng_kind = rng_kind,
    draws = draws,
    provenance = .rqr_bayes_provenance(seed = seed)
  )
  out$draws_digest <- .rqr_bayes_digest(list(
    residual_mass = residuals,
    atom_count = atom_counts,
    first_draw = draws[[1L]]
  ))
  class(out) <- c("rqr_dp_draws", "rqr_distribution_draws", "list")
  out
}

#' Draw Bayesian-bootstrap weighted empirical distributions
#'
#' @param y Numeric response vector.
#' @param n_draws Number of bootstrap posterior draws.
#' @param seed Optional seed.
#' @param na_rm Remove missing responses.
#' @return An `rqr_bayesian_bootstrap_draws` object.
#' @export
rqr_bayesian_bootstrap_draws <- function(y, n_draws, seed = NULL,
                                         na_rm = FALSE) {
  y <- .rqr_bayes_clean_y(y, na_rm = na_rm)
  n_draws <- .rqr_mt_assert_count(n_draws, "n_draws", 1L)
  rng_kind <- RNGkind()
  restore_rng <- .rqr_mt_seed_scope(seed)
  on.exit(restore_rng(), add = TRUE)
  draws <- vector("list", n_draws)
  for (s in seq_len(n_draws)) {
    draws[[s]] <- list(
      atoms = y,
      weights = .rqr_bayes_dirichlet(rep(1, length(y))),
      residual_mass = 0,
      atom_count = length(y)
    )
  }
  out <- list(
    schema_version = "rqrgibbs_bayesian_bootstrap_draws/1.0.0",
    engine = "bayesian_bootstrap",
    y = y,
    n_draws = as.integer(n_draws),
    seed = if (is.null(seed)) NA_integer_ else as.integer(seed),
    rng_kind = rng_kind,
    draws = draws,
    response_likelihood = FALSE,
    proper_direct_dp = FALSE,
    diagnostic_engine = TRUE,
    provenance = .rqr_bayes_provenance(seed = seed)
  )
  class(out) <- c("rqr_bayesian_bootstrap_draws", "rqr_distribution_draws", "list")
  out
}

#' Shortest-interval functionals from direct-DP or Bayesian-bootstrap draws
#'
#' @param draws Distribution draws from `rqr_dp_draws()` or
#'   `rqr_bayesian_bootstrap_draws()`.
#' @param target_content Shortest-interval content.
#' @param tolerance Numerical tolerance.
#' @return A data frame of posterior shortest-function draws.
#' @export
rqr_dp_shortest_draws <- function(draws, target_content, tolerance = 1e-12) {
  if (!inherits(draws, "rqr_distribution_draws")) {
    stop("draws must be rqr_distribution_draws.", call. = FALSE)
  }
  q <- .rqr_bayes_assert_probability(target_content, "target_content")
  rows <- lapply(seq_along(draws$draws), function(s) {
    d <- draws$draws[[s]]
    sh <- rqr_weighted_shortest_interval(
      d$atoms, d$weights, target_content = q, tolerance = tolerance
    )
    data.frame(
      draw = as.integer(s),
      engine = draws$engine,
      lower = sh$lower,
      upper = sh$upper,
      width = sh$width,
      retained_mass = sh$retained_mass,
      retained_mean = sh$retained_mean,
      full_mean = sh$full_mean,
      lower_tail_mass = sh$lower_tail_mass,
      tilt = sh$tilt,
      tie_count = sh$tie_count,
      tie_rule = sh$tie_rule,
      boundary_status = sh$boundary_status,
      residual_mass = d$residual_mass,
      atom_count = d$atom_count
    )
  })
  out <- do.call(rbind, rows)
  attr(out, "schema_version") <- "rqrgibbs_dp_shortest_draws/1.0.0"
  attr(out, "target_content") <- q
  out
}

