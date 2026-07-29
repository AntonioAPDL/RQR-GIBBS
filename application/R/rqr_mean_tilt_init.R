.rqr_mt_tilt_pilot_schema <- function() {
  "rqrgibbs_mean_tilt_pilot/1.1.0"
}

.rqr_mt_tilt_screen_schema <- function() {
  "rqrgibbs_mean_tilt_screen/1.0.0"
}

.rqr_mt_tilt_selection_schema <- function() {
  "rqrgibbs_mean_tilt_selection/1.1.0"
}

.rqr_mt_assert_coverage <- function(coverage_level) {
  coverage_level <- as.numeric(coverage_level)
  if (length(coverage_level) != 1L || is.na(coverage_level) ||
      !is.finite(coverage_level) ||
      coverage_level <= 0 || coverage_level >= 1) {
    stop("coverage_level must be one finite scalar in (0, 1).",
         call. = FALSE)
  }
  coverage_level
}

.rqr_mt_assert_logical <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop(sprintf("%s must be TRUE or FALSE.", name), call. = FALSE)
  }
  x
}

.rqr_mt_assert_count <- function(x, name, minimum = 0L) {
  x <- as.numeric(x)
  if (length(x) != 1L || is.na(x) || !is.finite(x) ||
      x != floor(x) || x < minimum || x > .Machine$integer.max) {
    stop(sprintf("%s must be one finite integer not smaller than %d.",
                 name, minimum), call. = FALSE)
  }
  as.integer(x)
}

.rqr_mt_clean_sample <- function(y, na_rm = FALSE) {
  na_rm <- .rqr_mt_assert_logical(na_rm, "na_rm")
  y <- as.numeric(y)
  if (!length(y)) stop("y must contain observations.", call. = FALSE)
  nonfinite <- !is.finite(y)
  removed <- 0L
  if (any(nonfinite)) {
    if (!na_rm) {
      stop(
        "Nonfinite observations found; set na_rm = TRUE to remove them.",
        call. = FALSE
      )
    }
    removed <- sum(nonfinite)
    y <- y[!nonfinite]
  }
  if (length(y) < 3L) {
    stop("At least three finite observations are required.", call. = FALSE)
  }
  y_sd <- stats::sd(y)
  if (!is.finite(y_sd) || y_sd <= 0) {
    stop("Sample standard deviation must be positive and finite.",
         call. = FALSE)
  }
  list(y = y, n_removed = as.integer(removed))
}

.rqr_mt_adjusted_fisher_pearson_skewness <- function(y) {
  n <- length(y)
  centered <- y - mean(y)
  m2 <- mean(centered^2)
  m3 <- mean(centered^3)
  if (!is.finite(m2) || !is.finite(m3) || m2 <= 0) {
    stop("Sample central moments must be finite with positive variance.",
         call. = FALSE)
  }
  sqrt(n * (n - 1)) / (n - 2) * m3 / m2^(3 / 2)
}

.rqr_mt_standardized_moments <- function(y) {
  centered <- y - mean(y)
  m2 <- mean(centered^2)
  if (!is.finite(m2) || m2 <= 0) {
    stop("Sample central moments must be finite with positive variance.",
         call. = FALSE)
  }
  m3 <- mean(centered^3)
  m4 <- mean(centered^4)
  m6 <- mean(centered^6)
  list(
    skewness_moment = m3 / m2^(3 / 2),
    excess_kurtosis = m4 / m2^2 - 3,
    standardized_sixth_moment = m6 / m2^3
  )
}

.rqr_mt_cf_probability_window <- function(coverage_level, gamma1,
                                          target) {
  q_c <- stats::qnorm((1 + coverage_level) / 2)
  lower_equal <- (1 - coverage_level) / 2
  lower_shortest <- lower_equal - gamma1 * stats::dnorm(q_c) / 3
  lower_unclipped <- if (identical(target, "shortest")) {
    lower_shortest
  } else {
    lower_equal
  }
  lower <- min(max(lower_unclipped, 0), 1 - coverage_level)
  upper <- lower + coverage_level
  list(
    u_lower_unclipped = lower_unclipped,
    u_upper_unclipped = lower_unclipped + coverage_level,
    u_lower = lower,
    u_upper = upper,
    clipped = !isTRUE(all.equal(lower_unclipped, lower, tolerance = 0)),
    distance_to_boundary = min(lower, 1 - coverage_level - lower)
  )
}

.rqr_mt_seed_scope <- function(seed) {
  if (is.null(seed)) return(function() invisible(FALSE))
  seed <- as.numeric(seed)
  if (length(seed) != 1L || is.na(seed) || !is.finite(seed) ||
      seed != floor(seed)) {
    stop("seed must be one finite integer.", call. = FALSE)
  }
  old_kind <- RNGkind()
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (had_seed) {
    get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  } else {
    NULL
  }
  set.seed(as.integer(seed))
  function() {
    do.call(RNGkind, as.list(old_kind))
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
    invisible(TRUE)
  }
}

.rqr_mt_boundary_label <- function(start, end, n) {
  lower <- identical(start, 1L)
  upper <- identical(end, n)
  if (lower && upper) return("both")
  if (lower) return("lower")
  if (upper) return("upper")
  "interior"
}

.rqr_mt_extract_standardized_tilt <- function(x, name) {
  if (is.null(x)) return(NA_real_)
  if (is.list(x) && !is.null(x$delta_standardized)) {
    value <- as.numeric(x$delta_standardized)[1L]
  } else {
    value <- as.numeric(x)[1L]
  }
  if (!is.finite(value)) {
    stop(sprintf("%s must provide one finite standardized tilt.", name),
         call. = FALSE)
  }
  value
}

.rqr_mt_extract_raw_tilt <- function(x) {
  if (is.list(x) && !is.null(x$delta_raw)) {
    value <- as.numeric(x$delta_raw)[1L]
    if (is.finite(value)) return(value)
  }
  NA_real_
}

.rqr_mt_unique_sorted <- function(x, tolerance = 1e-12) {
  x <- sort(as.numeric(x[is.finite(x)]))
  if (!length(x)) return(numeric(0))
  keep <- c(TRUE, diff(x) > tolerance)
  x[keep]
}

#' Cornish--Fisher constant for mean-tilt initialization
#'
#' Computes \eqn{K_c = q_c \phi(q_c) / c}, where
#' \eqn{q_c = \Phi^{-1}\{(1+c)/2\}}.  This constant appears in the
#' first-order Cornish--Fisher approximation to the shortest-oriented
#' standardized mean tilt.
#'
#' @param coverage_level Target interval coverage in `(0, 1)`.
#' @return A numeric scalar.
#' @export
rqr_mt_cf_constant <- function(coverage_level) {
  coverage_level <- .rqr_mt_assert_coverage(coverage_level)
  q_c <- stats::qnorm((1 + coverage_level) / 2)
  q_c * stats::dnorm(q_c) / coverage_level
}

#' Cornish--Fisher fixed mean-tilt pilot
#'
#' Estimates a fixed mean-tilt anchor for mean-tilted RQR from the adjusted
#' Fisher--Pearson sample skewness.  The shortest-oriented pilot uses the
#' first-order approximation
#' \deqn{\hat d_{\mathrm{SH}}^{\mathrm{CF}} =
#' -\hat\gamma_1 q_c\phi(q_c)/c,}
#' and the equal-tailed pilot is one third of that value.  These pilots are
#' initialization and screening anchors.  They are not posterior draws, do not
#' sample the tilt, and do not propagate tilt-estimation uncertainty through an
#' RQR generalized posterior.  The returned moment, boundary, and bootstrap
#' fields are diagnostics for the approximation, not validity guarantees.
#'
#' @param y Training responses.
#' @param coverage_level Target interval coverage in `(0, 1)`.
#' @param target Either `"shortest"` or `"equal_tailed"`.
#' @param na_rm If `TRUE`, remove nonfinite observations before computing the
#'   pilot.  If `FALSE`, nonfinite observations are an error.
#' @param bootstrap Optional nonnegative number of training-sample bootstrap
#'   replicates for a diagnostic uncertainty sidecar.
#' @param seed Optional seed used only for the bootstrap diagnostic.
#' @return An `rqr_mt_tilt_pilot` object.
#' @export
rqr_mt_tilt_cf <- function(
    y, coverage_level, target = c("shortest", "equal_tailed"),
    na_rm = FALSE, bootstrap = 0L, seed = NULL) {
  coverage_level <- .rqr_mt_assert_coverage(coverage_level)
  target <- match.arg(target)
  clean <- .rqr_mt_clean_sample(y, na_rm = na_rm)
  y <- clean$y
  n <- length(y)
  y_mean <- mean(y)
  y_sd <- stats::sd(y)
  gamma1 <- .rqr_mt_adjusted_fisher_pearson_skewness(y)
  moment_diag <- .rqr_mt_standardized_moments(y)
  constant <- rqr_mt_cf_constant(coverage_level)
  multiplier <- if (identical(target, "shortest")) 1 else 1 / 3
  d_hat <- -gamma1 * constant * multiplier
  delta_hat <- y_sd * d_hat
  if (!is.finite(d_hat) || !is.finite(delta_hat)) {
    stop("The Cornish--Fisher tilt is nonfinite.", call. = FALSE)
  }

  bootstrap <- .rqr_mt_assert_count(bootstrap, "bootstrap", 0L)
  bootstrap_summary <- list(
    requested = bootstrap,
    used = 0L,
    failures = 0L,
    sd_standardized = NA_real_,
    q025_standardized = NA_real_,
    q975_standardized = NA_real_,
    draws_standardized = numeric(0)
  )
  if (bootstrap > 0L) {
    restore_rng <- .rqr_mt_seed_scope(seed)
    on.exit(restore_rng(), add = TRUE)
    draws <- rep(NA_real_, bootstrap)
    failures <- 0L
    for (index in seq_len(bootstrap)) {
      sample_y <- sample(y, size = n, replace = TRUE)
      draws[[index]] <- tryCatch(
        rqr_mt_tilt_cf(
          sample_y, coverage_level = coverage_level,
          target = target, na_rm = FALSE, bootstrap = 0L
        )$delta_standardized,
        error = function(e) {
          failures <<- failures + 1L
          NA_real_
        }
      )
    }
    finite_draws <- draws[is.finite(draws)]
    bootstrap_summary <- list(
      requested = bootstrap,
      used = length(finite_draws),
      failures = failures,
      sd_standardized = if (length(finite_draws) > 1L) {
        stats::sd(finite_draws)
      } else {
        NA_real_
      },
      q025_standardized = if (length(finite_draws)) {
        as.numeric(stats::quantile(finite_draws, 0.025, names = FALSE,
                                   type = 8))
      } else {
        NA_real_
      },
      q975_standardized = if (length(finite_draws)) {
        as.numeric(stats::quantile(finite_draws, 0.975, names = FALSE,
                                   type = 8))
      } else {
        NA_real_
      },
      draws_standardized = finite_draws
    )
  }
  probability_window <- .rqr_mt_cf_probability_window(
    coverage_level, gamma1, target
  )
  reliability_flags <- list(
    boundary_clipped = isTRUE(probability_window$clipped),
    near_probability_boundary =
      is.finite(probability_window$distance_to_boundary) &&
      probability_window$distance_to_boundary < 0.02,
    large_abs_adjusted_skewness = abs(gamma1) > 1,
    large_abs_excess_kurtosis = abs(moment_diag$excess_kurtosis) > 2,
    large_standardized_sixth_moment =
      moment_diag$standardized_sixth_moment > 60,
    bootstrap_unstable = is.finite(bootstrap_summary$sd_standardized) &&
      bootstrap_summary$sd_standardized >
        max(0.1, abs(d_hat) / 2)
  )
  reliability_status <- if (any(unlist(reliability_flags, use.names = FALSE))) {
    "diagnostic_caution"
  } else {
    "nominal"
  }

  out <- list(
    schema_version = .rqr_mt_tilt_pilot_schema(),
    method = "cornish_fisher_first_order",
    approximation_order = "first_order_skewness",
    approximation_scope = paste(
      "Formal first-order near-Normal Cornish--Fisher anchor;",
      "not a finite-sample empirical optimizer."
    ),
    target = target,
    coverage_level = coverage_level,
    n = n,
    na_rm = isTRUE(na_rm),
    n_removed = clean$n_removed,
    training_mean = y_mean,
    training_sd = y_sd,
    adjusted_skewness = gamma1,
    skewness_moment = moment_diag$skewness_moment,
    excess_kurtosis = moment_diag$excess_kurtosis,
    standardized_sixth_moment =
      moment_diag$standardized_sixth_moment,
    cf_constant = constant,
    probability_window = probability_window,
    u_lower_cf_unclipped = probability_window$u_lower_unclipped,
    u_upper_cf_unclipped = probability_window$u_upper_unclipped,
    u_lower_cf = probability_window$u_lower,
    u_upper_cf = probability_window$u_upper,
    boundary_warning = isTRUE(probability_window$clipped) ||
      reliability_flags$near_probability_boundary,
    reliability_status = reliability_status,
    reliability_flags = reliability_flags,
    delta_raw = delta_hat,
    delta_standardized = d_hat,
    bootstrap = bootstrap_summary,
    fixed_tilt = TRUE,
    sampled_tilt = FALSE,
    interpretation = paste(
      "Cornish--Fisher fixed mean-tilt pilot for initialization or screening;",
      "not a posterior draw, not a finite-sample empirical optimizer, and",
      "not an automatic shortest-interval guarantee."
    )
  )
  class(out) <- c("rqr_mt_tilt_pilot", "list")
  out
}

#' Empirical shortest-window fixed mean-tilt pilot
#'
#' Finds the shortest adjacent order-statistic window retaining
#' `ceiling(coverage_level * n)` observations and reports its retained-mean
#' displacement from the full training mean.  This is a shape-robust
#' diagnostic anchor for fixed-tilt screening, especially when the
#' Cornish--Fisher near-Normal approximation is suspect.
#'
#' @inheritParams rqr_mt_tilt_cf
#' @return An `rqr_mt_tilt_pilot` object.
#' @export
rqr_mt_tilt_empirical_shortest <- function(
    y, coverage_level, na_rm = FALSE) {
  coverage_level <- .rqr_mt_assert_coverage(coverage_level)
  clean <- .rqr_mt_clean_sample(y, na_rm = na_rm)
  y <- clean$y
  n <- length(y)
  y_sorted <- sort(y)
  retained_n <- as.integer(ceiling(coverage_level * n))
  starts <- seq_len(n - retained_n + 1L)
  ends <- starts + retained_n - 1L
  widths <- y_sorted[ends] - y_sorted[starts]
  min_width <- min(widths)
  tolerance <- 100 * .Machine$double.eps * max(1, abs(min_width))
  ties <- which(abs(widths - min_width) <= tolerance)
  start <- ties[[1L]]
  end <- start + retained_n - 1L
  retained <- y_sorted[start:end]
  y_mean <- mean(y)
  y_sd <- stats::sd(y)
  window_means <- vapply(
    starts,
    function(index) mean(y_sorted[index:(index + retained_n - 1L)]),
    numeric(1L)
  )
  deltas <- window_means - y_mean
  out <- list(
    schema_version = .rqr_mt_tilt_pilot_schema(),
    method = "empirical_shortest_window",
    target = "shortest",
    coverage_level = coverage_level,
    n = n,
    m = retained_n,
    realized_content = retained_n / n,
    na_rm = isTRUE(na_rm),
    n_removed = clean$n_removed,
    training_mean = y_mean,
    training_sd = y_sd,
    lower_endpoint = retained[[1L]],
    upper_endpoint = retained[[length(retained)]],
    width = min_width,
    retained_mean = mean(retained),
    start_index = start,
    end_index = end,
    boundary = .rqr_mt_boundary_label(start, end, n),
    tie_count = length(ties),
    tie_rule = "first",
    feasible_delta_raw_range = range(deltas),
    feasible_delta_standardized_range = range(deltas / y_sd),
    delta_raw = mean(retained) - y_mean,
    delta_standardized = (mean(retained) - y_mean) / y_sd,
    fixed_tilt = TRUE,
    sampled_tilt = FALSE,
    interpretation = paste(
      "Empirical shortest-window fixed mean-tilt pilot for initialization",
      "or screening; discontinuous and not a posterior draw."
    )
  )
  class(out) <- c("rqr_mt_tilt_pilot", "list")
  out
}

#' Empirical equal-tailed-window fixed mean-tilt pilot
#'
#' Retains the central `ceiling(coverage_level * n)` order statistics after
#' splitting the omitted observations as evenly as possible between the lower
#' and upper tails.  The retained mean gives an empirical equal-tailed
#' fixed-tilt anchor.
#'
#' @inheritParams rqr_mt_tilt_cf
#' @return An `rqr_mt_tilt_pilot` object.
#' @export
rqr_mt_tilt_empirical_equal_tailed <- function(
    y, coverage_level, na_rm = FALSE) {
  coverage_level <- .rqr_mt_assert_coverage(coverage_level)
  clean <- .rqr_mt_clean_sample(y, na_rm = na_rm)
  y <- clean$y
  n <- length(y)
  y_sorted <- sort(y)
  retained_n <- as.integer(ceiling(coverage_level * n))
  omitted <- as.integer(n - retained_n)
  lower_omitted <- as.integer(floor(omitted / 2))
  upper_omitted <- as.integer(omitted - lower_omitted)
  start <- lower_omitted + 1L
  end <- start + retained_n - 1L
  retained <- y_sorted[start:end]
  y_mean <- mean(y)
  y_sd <- stats::sd(y)
  out <- list(
    schema_version = .rqr_mt_tilt_pilot_schema(),
    method = "empirical_equal_tailed_window",
    target = "equal_tailed",
    coverage_level = coverage_level,
    n = n,
    m = retained_n,
    realized_content = retained_n / n,
    lower_omitted = lower_omitted,
    upper_omitted = upper_omitted,
    na_rm = isTRUE(na_rm),
    n_removed = clean$n_removed,
    training_mean = y_mean,
    training_sd = y_sd,
    lower_endpoint = retained[[1L]],
    upper_endpoint = retained[[length(retained)]],
    width = retained[[length(retained)]] - retained[[1L]],
    retained_mean = mean(retained),
    start_index = start,
    end_index = end,
    boundary = .rqr_mt_boundary_label(start, end, n),
    delta_raw = mean(retained) - y_mean,
    delta_standardized = (mean(retained) - y_mean) / y_sd,
    fixed_tilt = TRUE,
    sampled_tilt = FALSE,
    interpretation = paste(
      "Empirical equal-tailed fixed mean-tilt pilot for initialization",
      "or screening; not a posterior draw."
    )
  )
  class(out) <- c("rqr_mt_tilt_pilot", "list")
  out
}

#' Build a compact fixed mean-tilt screening grid
#'
#' Builds a deterministic grid of fixed standardized tilts for later external
#' validation.  The grid includes zero, the supplied or computed
#' Cornish--Fisher equal-tailed pilot, one-half of the Cornish--Fisher
#' shortest pilot, the full Cornish--Fisher shortest pilot, the empirical
#' shortest-window pilot, optional extra candidates, and a small expansion
#' around the anchor range.  Each candidate is a separate fixed target; the
#' grid does not sample or learn the tilt inside an RQR chain.
#'
#' @param y Optional training responses.  When supplied, missing pilot objects
#'   are computed from `y`.
#' @param coverage_level Target interval coverage in `(0, 1)`.
#' @param cf_shortest,cf_equal_tailed,empirical_shortest Optional pilot objects
#'   or numeric standardized tilts.
#' @param extra_candidates Optional numeric standardized tilts to include.
#' @param include_half_shortest Include one-half of the CF shortest pilot.
#' @param expansion_points Number of equally spaced expansion-grid points.
#' @param expansion_fraction Fraction of the anchor range used as padding.
#' @param min_radius Minimum padding on the standardized-tilt scale.
#' @param clip_to_empirical If `TRUE`, clip candidates to the empirical
#'   feasible standardized-tilt range when that range is available.
#' @param na_rm Passed to pilot constructors when `y` is supplied.
#' @return An `rqr_mt_tilt_screen` object.
#' @export
rqr_mt_tilt_screen <- function(
    y = NULL, coverage_level,
    cf_shortest = NULL, cf_equal_tailed = NULL,
    empirical_shortest = NULL, extra_candidates = numeric(0),
    include_half_shortest = TRUE, expansion_points = 9L,
    expansion_fraction = 0.25, min_radius = 0.05,
    clip_to_empirical = TRUE, na_rm = FALSE) {
  coverage_level <- .rqr_mt_assert_coverage(coverage_level)
  include_half_shortest <- .rqr_mt_assert_logical(
    include_half_shortest, "include_half_shortest"
  )
  clip_to_empirical <- .rqr_mt_assert_logical(
    clip_to_empirical, "clip_to_empirical"
  )
  expansion_points <- .rqr_mt_assert_count(
    expansion_points, "expansion_points", 2L
  )
  expansion_fraction <- as.numeric(expansion_fraction)[1L]
  min_radius <- as.numeric(min_radius)[1L]
  if (!is.finite(expansion_fraction) || expansion_fraction < 0) {
    stop("expansion_fraction must be finite and nonnegative.",
         call. = FALSE)
  }
  if (!is.finite(min_radius) || min_radius < 0) {
    stop("min_radius must be finite and nonnegative.", call. = FALSE)
  }

  if (!is.null(y)) {
    if (is.null(cf_shortest)) {
      cf_shortest <- rqr_mt_tilt_cf(
        y, coverage_level, target = "shortest", na_rm = na_rm
      )
    }
    if (is.null(cf_equal_tailed)) {
      cf_equal_tailed <- rqr_mt_tilt_cf(
        y, coverage_level, target = "equal_tailed", na_rm = na_rm
      )
    }
    if (is.null(empirical_shortest)) {
      empirical_shortest <- rqr_mt_tilt_empirical_shortest(
        y, coverage_level, na_rm = na_rm
      )
    }
  }

  cf_sh <- .rqr_mt_extract_standardized_tilt(cf_shortest, "cf_shortest")
  cf_et <- .rqr_mt_extract_standardized_tilt(
    cf_equal_tailed, "cf_equal_tailed"
  )
  emp_sh <- .rqr_mt_extract_standardized_tilt(
    empirical_shortest, "empirical_shortest"
  )
  anchors <- c(
    ordinary_zero = 0,
    cornish_fisher_equal_tailed = cf_et,
    cornish_fisher_half_shortest = if (include_half_shortest) cf_sh / 2 else NA_real_,
    cornish_fisher_shortest = cf_sh,
    empirical_shortest_window = emp_sh,
    extra_candidates = as.numeric(extra_candidates)
  )
  anchors <- anchors[is.finite(anchors)]
  if (!length(anchors)) {
    stop("At least one finite tilt anchor is required.", call. = FALSE)
  }
  anchor_min <- min(anchors)
  anchor_max <- max(anchors)
  radius <- max(min_radius, expansion_fraction * (anchor_max - anchor_min))
  expanded <- seq(anchor_min - radius, anchor_max + radius,
                  length.out = expansion_points)
  unclipped <- .rqr_mt_unique_sorted(c(anchors, expanded))

  empirical_range <- c(NA_real_, NA_real_)
  if (is.list(empirical_shortest) &&
      !is.null(empirical_shortest$feasible_delta_standardized_range)) {
    empirical_range <- as.numeric(
      empirical_shortest$feasible_delta_standardized_range
    )
  }
  clipped <- FALSE
  candidates <- unclipped
  if (clip_to_empirical && length(empirical_range) == 2L &&
      all(is.finite(empirical_range))) {
    candidates <- pmin(pmax(candidates, empirical_range[[1L]]),
                       empirical_range[[2L]])
    candidates <- .rqr_mt_unique_sorted(candidates)
    clipped <- TRUE
  }
  anchor_table <- data.frame(
    anchor = names(anchors),
    delta_standardized = unname(anchors),
    stringsAsFactors = FALSE
  )
  out <- list(
    schema_version = .rqr_mt_tilt_screen_schema(),
    coverage_level = coverage_level,
    candidates = candidates,
    candidates_unclipped = unclipped,
    anchors = anchor_table,
    empirical_feasible_range = empirical_range,
    clipped_to_empirical_range = clipped,
    expansion_points = expansion_points,
    expansion_fraction = expansion_fraction,
    min_radius = min_radius,
    fixed_tilt = TRUE,
    sampled_tilt = FALSE,
    selection_rule = paste(
      "Evaluate each candidate as a separate fixed target and select by",
      "held-out width subject to held-out coverage."
    ),
    interpretation = paste(
      "Screening grid for fixed mean tilts; not an in-chain tilt sampler",
      "and not an automatic shortest-interval guarantee."
    )
  )
  class(out) <- c("rqr_mt_tilt_screen", "list")
  out
}

#' Select a fixed tilt from held-out validation summaries
#'
#' Selects the minimum-width candidate among those satisfying a predeclared
#' held-out coverage constraint.  If no candidate satisfies the constraint, the
#' function returns a declared failure object rather than silently selecting an
#' undercovered interval.
#'
#' @param candidates Numeric standardized tilts.
#' @param mean_width Held-out mean widths for the candidates.
#' @param empirical_coverage Held-out empirical coverages for the candidates.
#' @param coverage_level Target interval coverage in `(0, 1)`.
#' @param tolerance Nonnegative coverage tolerance; candidates must satisfy
#'   `empirical_coverage >= coverage_level - tolerance`.
#' @param coverage_guard Coverage rule. `"point"` uses the supplied point
#'   empirical coverage. `"simultaneous_binomial"` uses a Bonferroni one-sided
#'   Normal lower bound for all candidates.
#' @param validation_n Validation-set size, required for
#'   `"simultaneous_binomial"`.
#' @param confidence_level Simultaneous confidence level used by the binomial
#'   lower-bound guard.
#' @return An `rqr_mt_tilt_selection` object.
#' @export
rqr_mt_select_tilt_candidate <- function(
    candidates, mean_width, empirical_coverage, coverage_level,
    tolerance = 0,
    coverage_guard = c("point", "simultaneous_binomial"),
    validation_n = NULL, confidence_level = 0.95) {
  coverage_level <- .rqr_mt_assert_coverage(coverage_level)
  coverage_guard <- match.arg(coverage_guard)
  candidates <- as.numeric(candidates)
  mean_width <- as.numeric(mean_width)
  empirical_coverage <- as.numeric(empirical_coverage)
  if (!length(candidates) ||
      length(mean_width) != length(candidates) ||
      length(empirical_coverage) != length(candidates) ||
      any(!is.finite(candidates)) ||
      any(!is.finite(mean_width)) ||
      any(!is.finite(empirical_coverage)) ||
      any(mean_width < 0)) {
    stop(
      "candidates, mean_width, and empirical_coverage must be finite compatible vectors.",
      call. = FALSE
    )
  }
  tolerance <- as.numeric(tolerance)[1L]
  if (!is.finite(tolerance) || tolerance < 0 || tolerance >= coverage_level) {
    stop("tolerance must be finite, nonnegative, and smaller than coverage_level.",
         call. = FALSE)
  }
  confidence_level <- as.numeric(confidence_level)[1L]
  if (!is.finite(confidence_level) || confidence_level <= 0 ||
      confidence_level >= 1) {
    stop("confidence_level must be one finite scalar in (0, 1).",
         call. = FALSE)
  }
  coverage_lower_bound <- empirical_coverage
  if (identical(coverage_guard, "simultaneous_binomial")) {
    if (is.null(validation_n)) {
      stop(
        "validation_n is required when coverage_guard = 'simultaneous_binomial'.",
        call. = FALSE
      )
    }
    validation_n <- .rqr_mt_assert_count(validation_n, "validation_n", 1L)
    alpha_family <- 1 - confidence_level
    z <- stats::qnorm(1 - alpha_family / length(candidates))
    se <- sqrt(pmax(empirical_coverage * (1 - empirical_coverage), 0) /
                 validation_n)
    coverage_lower_bound <- pmax(0, empirical_coverage - z * se)
  } else {
    validation_n <- if (is.null(validation_n)) NA_integer_ else
      .rqr_mt_assert_count(validation_n, "validation_n", 1L)
  }
  admissible <- coverage_lower_bound >= coverage_level - tolerance
  if (!any(admissible)) {
    out <- list(
      schema_version = .rqr_mt_tilt_selection_schema(),
      status = "failed_no_coverage_candidate",
      selected_index = NA_integer_,
      selected_delta_standardized = NA_real_,
      coverage_level = coverage_level,
      tolerance = tolerance,
      coverage_guard = coverage_guard,
      validation_n = validation_n,
      confidence_level = confidence_level,
      candidates = candidates,
      mean_width = mean_width,
      empirical_coverage = empirical_coverage,
      coverage_lower_bound = coverage_lower_bound,
      admissible = admissible,
      interpretation = paste(
        "No fixed tilt satisfied the held-out coverage constraint;",
        "do not select the narrowest undercovered candidate."
      )
    )
    class(out) <- c("rqr_mt_tilt_selection", "list")
    return(out)
  }
  admissible_indices <- which(admissible)
  best <- admissible_indices[which.min(mean_width[admissible_indices])]
  out <- list(
    schema_version = .rqr_mt_tilt_selection_schema(),
    status = "selected",
    selected_index = best,
    selected_delta_standardized = candidates[[best]],
    selected_mean_width = mean_width[[best]],
    selected_empirical_coverage = empirical_coverage[[best]],
    selected_coverage_lower_bound = coverage_lower_bound[[best]],
    coverage_level = coverage_level,
    tolerance = tolerance,
    coverage_guard = coverage_guard,
    validation_n = validation_n,
    confidence_level = confidence_level,
    candidates = candidates,
    mean_width = mean_width,
    empirical_coverage = empirical_coverage,
    coverage_lower_bound = coverage_lower_bound,
    admissible = admissible,
    interpretation = paste(
      "Selected fixed tilt by held-out mean width subject to held-out",
      "coverage; this is external target selection, not posterior tilt sampling."
    )
  )
  class(out) <- c("rqr_mt_tilt_selection", "list")
  out
}
