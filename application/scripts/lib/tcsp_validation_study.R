`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

tcspv_schema <- function() {
  "rqrgibbs_tcsp_validation_study/1.0.0"
}

tcspv_stop <- function(...) {
  stop(paste0(...), call. = FALSE)
}

tcspv_find_repo_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(current, "AGENTS.md")) &&
        file.exists(file.path(current, "application", "DESCRIPTION"))) {
      return(current)
    }
    parent <- dirname(current)
    if (identical(parent, current)) tcspv_stop("Could not locate repo root.")
    current <- parent
  }
}

tcspv_read_config <- function(path) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    tcspv_stop("jsonlite is required for TCSP validation configs.")
  }
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

tcspv_scalar_probability <- function(x, name) {
  x <- as.numeric(x)[1L]
  if (!is.finite(x) || x <= 0 || x >= 1) {
    tcspv_stop(name, " must be one finite scalar in (0, 1).")
  }
  x
}

tcspv_scalar_count <- function(x, name, lower = 1L) {
  x <- as.integer(x)[1L]
  if (!is.finite(x) || x < lower) {
    tcspv_stop(name, " must be an integer at least ", lower, ".")
  }
  x
}

tcspv_frame <- function(records) {
  if (!length(records)) return(data.frame())
  do.call(rbind, lapply(records, function(x) {
    as.data.frame(x, stringsAsFactors = FALSE, optional = TRUE)
  }))
}

tcspv_finite <- function(x) {
  x <- as.numeric(x)
  x[is.finite(x)]
}

tcspv_mean_or_na <- function(x) {
  x <- tcspv_finite(x)
  if (length(x)) mean(x) else NA_real_
}

tcspv_median_or_na <- function(x) {
  x <- tcspv_finite(x)
  if (length(x)) stats::median(x) else NA_real_
}

tcspv_quantile_or_na <- function(x, probability) {
  x <- tcspv_finite(x)
  if (length(x)) {
    as.numeric(stats::quantile(x, probability, names = FALSE, type = 8))
  } else {
    NA_real_
  }
}

tcspv_dgps <- function(config) {
  rows <- lapply(config$dgps, function(x) {
    data.frame(
      dgp_id = as.character(x$dgp_id),
      label = as.character(x$label %||% x$dgp_id),
      family = as.character(x$family),
      enabled = isTRUE(x$enabled),
      parametric_normal_correctly_specified =
        isTRUE(x$parametric_normal_correctly_specified),
      stringsAsFactors = FALSE
    )
  })
  tcspv_frame(rows)
}

tcspv_methods <- function(config) {
  rows <- lapply(config$methods, function(x) {
    data.frame(
      method_id = as.character(x$method_id),
      label = as.character(x$label %||% x$method_id),
      family = as.character(x$family),
      enabled = isTRUE(x$enabled),
      formal_tolerance_action = isTRUE(x$formal_tolerance_action),
      benchmark_only = isTRUE(x$benchmark_only),
      disabled_reason = as.character(x$disabled_reason %||% ""),
      stringsAsFactors = FALSE
    )
  })
  tcspv_frame(rows)
}

tcspv_validate_config <- function(config) {
  if (!is.list(config) || !identical(config$schema_version, tcspv_schema())) {
    tcspv_stop("Unsupported TCSP validation schema.")
  }
  if (!isTRUE(config$generalized_bayes) ||
      isTRUE(config$response_likelihood) ||
      isTRUE(config$posterior_predictive_response_draws)) {
    tcspv_stop("TCSP validation config violates generalized-Bayes scope.")
  }
  if (!isTRUE(config$claim_scope$iid_univariate_continuous_only) ||
      isTRUE(config$claim_scope$regression_tolerance) ||
      isTRUE(config$claim_scope$dynamic_tolerance) ||
      isTRUE(config$claim_scope$posterior_summary_is_formal_tolerance_action) ||
      isTRUE(config$claim_scope$monte_carlo_scan_is_exact)) {
    tcspv_stop("TCSP validation claim scope is not fail closed.")
  }
  execution <- config$execution
  has_full_pilot <- !is.null(config$modes$full_pilot)
  if (!isTRUE(execution$preflight_authorized) ||
      !isTRUE(execution$tiny_authorized) ||
      !isTRUE(execution$pilot_authorized) ||
      isTRUE(execution$confirmatory_authorized) ||
      !isTRUE(execution$confirmatory_requires_new_config)) {
    tcspv_stop("TCSP execution flags are not in the expected fail-closed state.")
  }
  if ((has_full_pilot && !isTRUE(execution$full_pilot_authorized)) ||
      (!has_full_pilot && isTRUE(execution$full_pilot_authorized))) {
    tcspv_stop("TCSP full-pilot execution flag is inconsistent with modes.")
  }
  dgps <- tcspv_dgps(config)
  methods <- tcspv_methods(config)
  if (anyDuplicated(dgps$dgp_id) || anyDuplicated(methods$method_id)) {
    tcspv_stop("Duplicate DGP or method IDs in TCSP config.")
  }
  supported_families <- c(
    "normal", "standardized_lognormal", "standardized_student_t",
    "standardized_normal_mixture", "beta"
  )
  if (!all(dgps$family %in% supported_families) || !all(dgps$enabled)) {
    tcspv_stop("Unsupported or disabled DGP in active TCSP config.")
  }
  active_methods <- methods[methods$enabled, , drop = FALSE]
  required <- c(
    "tcsp_dkw", "tcsp_mc", "wilks_symmetric", "wilks_minmax",
    "equal_tailed_tcsp_content", "normal_howe", "oracle_shortest"
  )
  if (!all(required %in% active_methods$method_id)) {
    tcspv_stop("The active TCSP method contract is incomplete.")
  }
  disabled <- methods[!methods$enabled, , drop = FALSE]
  if (!all(c("young_mathew", "calibrated_bnp_gibbs") %in%
           disabled$method_id) ||
      any(!nzchar(disabled$disabled_reason))) {
    tcspv_stop("Disabled TCSP competitors must be explicit and justified.")
  }
  for (value in config$design$guaranteed_contents) {
    tcspv_scalar_probability(value, "guaranteed_contents")
  }
  for (value in config$design$tolerance_confidences) {
    tcspv_scalar_probability(value, "tolerance_confidences")
  }
  for (value in config$design$sample_sizes) {
    tcspv_scalar_count(value, "sample_sizes", 2L)
  }
  if (!all(c("calibration", "data", "method", "audit") %in%
           config$seed_contract$streams)) {
    tcspv_stop("TCSP seed streams are incomplete.")
  }
  invisible(TRUE)
}

tcspv_seed <- function(config, stream, ...) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    tcspv_stop("digest is required for deterministic TCSP seeds.")
  }
  if (!stream %in% config$seed_contract$streams) {
    tcspv_stop("Unknown TCSP seed stream: ", stream)
  }
  key <- paste(
    config$seed_contract$schema_version,
    config$seed_contract$master_seed,
    stream,
    paste(..., collapse = "::"),
    sep = "|"
  )
  value <- strtoi(substr(digest::digest(key, algo = "sha256",
                                        serialize = FALSE), 1L, 7L), 16L)
  as.integer(max(1L, value))
}

tcspv_dgp_spec <- function(config, dgp_id) {
  hit <- Filter(function(x) identical(as.character(x$dgp_id), dgp_id),
                config$dgps)
  if (length(hit) != 1L) tcspv_stop("Unknown DGP ID: ", dgp_id)
  hit[[1L]]
}

tcspv_lognormal_moments <- function(spec) {
  logmean <- as.numeric(spec$logmean %||% 0)
  logsd <- as.numeric(spec$logsd)
  mean_raw <- exp(logmean + 0.5 * logsd^2)
  sd_raw <- sqrt((exp(logsd^2) - 1) * exp(2 * logmean + logsd^2))
  list(mean = mean_raw, sd = sd_raw)
}

tcspv_mixture_moments <- function(spec) {
  w <- as.numeric(unlist(spec$weights))
  mu <- as.numeric(unlist(spec$means))
  sig <- as.numeric(unlist(spec$sds))
  mean_raw <- sum(w * mu)
  second <- sum(w * (sig^2 + mu^2))
  list(mean = mean_raw, sd = sqrt(second - mean_raw^2))
}

tcspv_sample <- function(config, dgp_id, n, seed) {
  spec <- tcspv_dgp_spec(config, dgp_id)
  n <- tcspv_scalar_count(n, "n", 2L)
  set.seed(as.integer(seed))
  family <- as.character(spec$family)
  if (identical(family, "normal")) {
    return(stats::rnorm(n, as.numeric(spec$location %||% 0),
                        as.numeric(spec$scale %||% 1)))
  }
  if (identical(family, "standardized_lognormal")) {
    mom <- tcspv_lognormal_moments(spec)
    raw <- stats::rlnorm(n, as.numeric(spec$logmean %||% 0),
                         as.numeric(spec$logsd))
    return((raw - mom$mean) / mom$sd)
  }
  if (identical(family, "standardized_student_t")) {
    df <- as.numeric(spec$df)
    return(stats::rt(n, df = df) / sqrt(df / (df - 2)))
  }
  if (identical(family, "standardized_normal_mixture")) {
    w <- as.numeric(unlist(spec$weights))
    mu <- as.numeric(unlist(spec$means))
    sig <- as.numeric(unlist(spec$sds))
    comp <- sample.int(length(w), n, replace = TRUE, prob = w)
    raw <- stats::rnorm(n, mu[comp], sig[comp])
    mom <- tcspv_mixture_moments(spec)
    return((raw - mom$mean) / mom$sd)
  }
  if (identical(family, "beta")) {
    return(stats::rbeta(n, as.numeric(spec$shape1), as.numeric(spec$shape2)))
  }
  tcspv_stop("Unsupported DGP family: ", family)
}

tcspv_cdf <- function(config, dgp_id, x) {
  spec <- tcspv_dgp_spec(config, dgp_id)
  x <- as.numeric(x)
  family <- as.character(spec$family)
  if (identical(family, "normal")) {
    return(stats::pnorm(x, as.numeric(spec$location %||% 0),
                        as.numeric(spec$scale %||% 1)))
  }
  if (identical(family, "standardized_lognormal")) {
    mom <- tcspv_lognormal_moments(spec)
    raw <- x * mom$sd + mom$mean
    return(ifelse(raw <= 0, 0, stats::plnorm(
      raw, as.numeric(spec$logmean %||% 0), as.numeric(spec$logsd)
    )))
  }
  if (identical(family, "standardized_student_t")) {
    df <- as.numeric(spec$df)
    return(stats::pt(x * sqrt(df / (df - 2)), df = df))
  }
  if (identical(family, "standardized_normal_mixture")) {
    w <- as.numeric(unlist(spec$weights))
    mu <- as.numeric(unlist(spec$means))
    sig <- as.numeric(unlist(spec$sds))
    mom <- tcspv_mixture_moments(spec)
    raw <- x * mom$sd + mom$mean
    out <- Reduce(`+`, Map(function(weight, m, s) {
      weight * stats::pnorm(raw, m, s)
    }, w, mu, sig))
    return(out)
  }
  if (identical(family, "beta")) {
    return(stats::pbeta(x, as.numeric(spec$shape1), as.numeric(spec$shape2)))
  }
  tcspv_stop("Unsupported DGP family: ", family)
}

tcspv_quantile <- function(config, dgp_id, p) {
  spec <- tcspv_dgp_spec(config, dgp_id)
  p <- pmin(pmax(as.numeric(p), 1e-12), 1 - 1e-12)
  family <- as.character(spec$family)
  if (identical(family, "normal")) {
    return(stats::qnorm(p, as.numeric(spec$location %||% 0),
                        as.numeric(spec$scale %||% 1)))
  }
  if (identical(family, "standardized_lognormal")) {
    mom <- tcspv_lognormal_moments(spec)
    return((stats::qlnorm(p, as.numeric(spec$logmean %||% 0),
                          as.numeric(spec$logsd)) - mom$mean) / mom$sd)
  }
  if (identical(family, "standardized_student_t")) {
    df <- as.numeric(spec$df)
    return(stats::qt(p, df = df) / sqrt(df / (df - 2)))
  }
  if (identical(family, "beta")) {
    return(stats::qbeta(p, as.numeric(spec$shape1), as.numeric(spec$shape2)))
  }
  vapply(p, function(pp) {
    lower <- -12
    upper <- 12
    while (tcspv_cdf(config, dgp_id, lower) > pp) lower <- lower * 2
    while (tcspv_cdf(config, dgp_id, upper) < pp) upper <- upper * 2
    stats::uniroot(
      function(z) tcspv_cdf(config, dgp_id, z) - pp,
      interval = c(lower, upper), tol = 1e-10
    )$root
  }, numeric(1L))
}

tcspv_oracle_shortest <- function(config, dgp_id, content) {
  content <- tcspv_scalar_probability(content, "content")
  width_at <- function(u) {
    tcspv_quantile(config, dgp_id, u + content) -
      tcspv_quantile(config, dgp_id, u)
  }
  opt <- stats::optimize(width_at, c(1e-8, 1 - content - 1e-8))
  lower <- tcspv_quantile(config, dgp_id, opt$minimum)
  upper <- tcspv_quantile(config, dgp_id, opt$minimum + content)
  list(
    lower = lower, upper = upper, width = upper - lower,
    lower_omitted = opt$minimum, upper_omitted = 1 - opt$minimum - content
  )
}

tcspv_scan_max_count <- function(u, content, tolerance = 1e-14) {
  u <- sort(as.numeric(u))
  n <- length(u)
  best <- 0L
  right <- 1L
  for (left in seq_len(n)) {
    if (right < left) right <- left
    while (right + 1L <= n &&
           u[[right + 1L]] - u[[left]] <= content + tolerance) {
      right <- right + 1L
    }
    best <- max(best, right - left + 1L)
  }
  as.integer(best)
}

tcspv_binom_lower <- function(successes, trials, confidence_level) {
  stats::binom.test(successes, trials, conf.level = confidence_level)$conf.int[[1L]]
}

tcspv_dkw_count <- function(n, content, confidence) {
  eps <- sqrt(log(2 / (1 - confidence)) / (2 * n))
  k <- as.integer(floor(n * (content + 2 * eps)) + 1L)
  list(
    retained_count = k,
    infeasible = k > n,
    certified_lower_probability = as.numeric(k <= n),
    dkw_epsilon = eps
  )
}

tcspv_mc_count <- function(config, n, content, confidence, mode) {
  n_sim <- as.integer(if (identical(mode, "tiny")) {
    config$scan_calibration$tiny_n_sim
  } else if (identical(mode, "full_pilot")) {
    config$scan_calibration$full_pilot_n_sim
  } else {
    config$scan_calibration$pilot_n_sim
  })
  numerical_confidence <- tcspv_scalar_probability(
    config$scan_calibration$numerical_confidence,
    "scan_calibration$numerical_confidence"
  )
  seed <- tcspv_seed(config, "calibration", mode, n, content, confidence)
  set.seed(seed)
  max_counts <- replicate(
    n_sim, tcspv_scan_max_count(stats::runif(n), content)
  )
  for (k in seq_len(n + 1L)) {
    successes <- sum(max_counts < k)
    lower <- tcspv_binom_lower(successes, n_sim, numerical_confidence)
    if (is.finite(lower) && lower >= confidence) {
      return(list(
        retained_count = as.integer(k),
        infeasible = k > n,
        certified_lower_probability = lower,
        n_sim = n_sim,
        successes = successes,
        numerical_confidence = numerical_confidence,
        seed = seed
      ))
    }
  }
  tcspv_stop("Monte Carlo TCSP calibration failed closed.")
}

tcspv_order_gap_confidence <- function(n, gap, content) {
  if (!is.finite(gap) || gap < 1L || gap >= n) return(0)
  1 - stats::pbeta(content, gap, n + 1L - gap)
}

tcspv_wilks_symmetric_indices <- function(n, content, confidence) {
  best <- NULL
  for (trim in 0:floor((n - 2L) / 2L)) {
    left <- trim + 1L
    right <- n - trim
    gap <- right - left
    cert <- tcspv_order_gap_confidence(n, gap, content)
    if (cert >= confidence) {
      best <- list(left = left, right = right, gap = gap, certificate = cert)
    }
  }
  if (is.null(best)) {
    best <- list(left = NA_integer_, right = NA_integer_, gap = NA_integer_,
                 certificate = tcspv_order_gap_confidence(n, n - 1L, content))
  }
  best$infeasible <- is.na(best$left)
  best
}

tcspv_critical_counts <- function(config, mode) {
  tcspv_validate_config(config)
  mode_cfg <- config$modes[[mode]]
  if (is.null(mode_cfg)) tcspv_stop("Unknown TCSP validation mode: ", mode)
  rows <- list()
  ii <- 0L
  for (n in as.integer(unlist(mode_cfg$sample_sizes))) {
    for (content in as.numeric(unlist(mode_cfg$guaranteed_contents))) {
      for (confidence in as.numeric(unlist(mode_cfg$tolerance_confidences))) {
        dkw <- tcspv_dkw_count(n, content, confidence)
        ii <- ii + 1L
        rows[[ii]] <- data.frame(
          method_id = "tcsp_dkw", n = n, guaranteed_content = content,
          tolerance_confidence = confidence,
          retained_count = dkw$retained_count,
          order_left = NA_integer_, order_right = NA_integer_,
          certificate = dkw$certified_lower_probability,
          infeasible = dkw$infeasible,
          calibration = "dkw_conservative",
          n_sim = 0L, seed = NA_integer_,
          stringsAsFactors = FALSE
        )
        if ("tcsp_mc" %in% unlist(mode_cfg$method_ids) && !identical(mode, "preflight")) {
          mc <- tcspv_mc_count(config, n, content, confidence, mode)
          ii <- ii + 1L
          rows[[ii]] <- data.frame(
            method_id = "tcsp_mc", n = n, guaranteed_content = content,
            tolerance_confidence = confidence,
            retained_count = mc$retained_count,
            order_left = NA_integer_, order_right = NA_integer_,
            certificate = mc$certified_lower_probability,
            infeasible = mc$infeasible,
            calibration = "monte_carlo_conservative",
            n_sim = mc$n_sim, seed = mc$seed,
            stringsAsFactors = FALSE
          )
        }
        wilks <- tcspv_wilks_symmetric_indices(n, content, confidence)
        ii <- ii + 1L
        rows[[ii]] <- data.frame(
          method_id = "wilks_symmetric", n = n,
          guaranteed_content = content,
          tolerance_confidence = confidence,
          retained_count = if (is.na(wilks$gap)) NA_integer_ else wilks$gap + 1L,
          order_left = wilks$left, order_right = wilks$right,
          certificate = wilks$certificate,
          infeasible = wilks$infeasible,
          calibration = "exact_beta_spacing",
          n_sim = 0L, seed = NA_integer_,
          stringsAsFactors = FALSE
        )
        minmax_cert <- tcspv_order_gap_confidence(n, n - 1L, content)
        ii <- ii + 1L
        rows[[ii]] <- data.frame(
          method_id = "wilks_minmax", n = n,
          guaranteed_content = content,
          tolerance_confidence = confidence,
          retained_count = n,
          order_left = 1L, order_right = n,
          certificate = minmax_cert,
          infeasible = minmax_cert < confidence,
          calibration = "exact_beta_range",
          n_sim = 0L, seed = NA_integer_,
          stringsAsFactors = FALSE
        )
        k <- dkw$retained_count
        left <- if (k <= n) floor((n - k) / 2L) + 1L else NA_integer_
        ii <- ii + 1L
        rows[[ii]] <- data.frame(
          method_id = "equal_tailed_tcsp_content", n = n,
          guaranteed_content = content,
          tolerance_confidence = confidence,
          retained_count = k,
          order_left = left,
          order_right = if (is.na(left)) NA_integer_ else left + k - 1L,
          certificate = NA_real_,
          infeasible = k > n,
          calibration = "none_uses_tcsp_dkw_retained_count",
          n_sim = 0L, seed = NA_integer_,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  do.call(rbind, rows)
}

tcspv_design_grid <- function(config, mode) {
  mode_cfg <- config$modes[[mode]]
  expand.grid(
    dgp_id = unlist(mode_cfg$dgp_ids),
    n = as.integer(unlist(mode_cfg$sample_sizes)),
    guaranteed_content = as.numeric(unlist(mode_cfg$guaranteed_contents)),
    tolerance_confidence =
      as.numeric(unlist(mode_cfg$tolerance_confidences)),
    method_id = unlist(mode_cfg$method_ids),
    stringsAsFactors = FALSE
  )
}

tcspv_interval_for_method <- function(config, method_id, y, dgp_id, content,
                                      confidence, criticals,
                                      oracle_cache = NULL) {
  n <- length(y)
  ys <- sort(y)
  row <- criticals[
    criticals$method_id == method_id & criticals$n == n &
      abs(criticals$guaranteed_content - content) < 1e-12 &
      abs(criticals$tolerance_confidence - confidence) < 1e-12,
    , drop = FALSE
  ]
  failed <- FALSE
  reason <- ""
  lower <- upper <- NA_real_
  retained_count <- NA_integer_
  certificate <- NA_real_
  if (method_id %in% c("tcsp_dkw", "tcsp_mc")) {
    if (nrow(row) != 1L || isTRUE(row$infeasible[[1L]])) {
      failed <- TRUE; reason <- "critical_count_infeasible"
    } else {
      retained_count <- as.integer(row$retained_count[[1L]])
      win <- rqrgibbs::rqr_tcsp_shortest_window(y, retained_count)
      lower <- win$lower_endpoint
      upper <- win$upper_endpoint
      certificate <- row$certificate[[1L]]
    }
  } else if (method_id %in% c("wilks_symmetric", "wilks_minmax",
                              "equal_tailed_tcsp_content")) {
    if (nrow(row) != 1L || isTRUE(row$infeasible[[1L]]) ||
        is.na(row$order_left[[1L]]) || is.na(row$order_right[[1L]])) {
      failed <- TRUE; reason <- "order_statistic_infeasible"
    } else {
      lower <- ys[[as.integer(row$order_left[[1L]])]]
      upper <- ys[[as.integer(row$order_right[[1L]])]]
      retained_count <- as.integer(row$retained_count[[1L]])
      certificate <- row$certificate[[1L]]
    }
  } else if (identical(method_id, "normal_howe")) {
    s <- stats::sd(y)
    if (!is.finite(s) || s <= 0) {
      failed <- TRUE; reason <- "sample_sd_degenerate"
    } else {
      alpha <- 1 - confidence
      factor <- stats::qnorm((1 + content) / 2) *
        sqrt((n - 1) * (1 + 1 / n) / stats::qchisq(alpha, df = n - 1))
      lower <- mean(y) - factor * s
      upper <- mean(y) + factor * s
      certificate <- NA_real_
    }
  } else if (identical(method_id, "oracle_shortest")) {
    key <- paste(dgp_id, format(content, digits = 16), sep = "/")
    if (!is.null(oracle_cache) &&
        exists(key, envir = oracle_cache, inherits = FALSE)) {
      oracle <- get(key, envir = oracle_cache, inherits = FALSE)
    } else {
      oracle <- tcspv_oracle_shortest(config, dgp_id, content)
      if (!is.null(oracle_cache)) {
        assign(key, oracle, envir = oracle_cache)
      }
    }
    lower <- oracle$lower
    upper <- oracle$upper
    certificate <- 1
  } else {
    failed <- TRUE; reason <- paste0("unsupported_method:", method_id)
  }
  list(
    lower = lower, upper = upper, failed = failed, failure_reason = reason,
    retained_count = retained_count, certificate = certificate
  )
}

tcspv_evaluate_interval <- function(config, dgp_id, lower, upper, content,
                                    tolerance = 1e-12) {
  if (!is.finite(lower) || !is.finite(upper) || upper < lower) {
    return(list(
      true_content = NA_real_, lower_omitted = NA_real_,
      upper_omitted = NA_real_, width = NA_real_, tolerance_success = FALSE,
      content_deficit = NA_real_
    ))
  }
  f_lower <- tcspv_cdf(config, dgp_id, lower)
  f_upper <- tcspv_cdf(config, dgp_id, upper)
  true_content <- max(0, min(1, f_upper - f_lower))
  list(
    true_content = true_content,
    lower_omitted = f_lower,
    upper_omitted = 1 - f_upper,
    width = upper - lower,
    tolerance_success = true_content + tolerance >= content,
    content_deficit = max(0, content - true_content)
  )
}

tcspv_run_repeated_sample <- function(config, mode, criticals = NULL) {
  if (!requireNamespace("rqrgibbs", quietly = TRUE)) {
    tcspv_stop("rqrgibbs must be installed for TCSP validation.")
  }
  tcspv_validate_config(config)
  mode_cfg <- config$modes[[mode]]
  reps <- as.integer(mode_cfg$replications)
  if (reps <= 0L) tcspv_stop("Mode ", mode, " has no replications.")
  if (is.null(criticals)) criticals <- tcspv_critical_counts(config, mode)
  grid <- tcspv_design_grid(config, mode)
  rows <- vector("list", nrow(grid) * reps)
  index <- 0L
  oracle_cache <- new.env(parent = emptyenv())
  for (replication in seq_len(reps)) {
    generated <- new.env(parent = emptyenv())
    for (dgp_id in unique(grid$dgp_id)) {
      for (n in unique(grid$n)) {
        key <- paste(dgp_id, n, replication, sep = "/")
        seed <- tcspv_seed(config, "data", mode, dgp_id, n, replication)
        generated[[key]] <- list(
          y = tcspv_sample(config, dgp_id, n, seed),
          seed = seed
        )
      }
    }
    for (ii in seq_len(nrow(grid))) {
      g <- grid[ii, ]
      key <- paste(g$dgp_id, g$n, replication, sep = "/")
      y <- generated[[key]]$y
      timed <- system.time({
        interval <- tcspv_interval_for_method(
          config, g$method_id, y, g$dgp_id, g$guaranteed_content,
          g$tolerance_confidence, criticals, oracle_cache = oracle_cache
        )
      })
      eval <- tcspv_evaluate_interval(
        config, g$dgp_id, interval$lower, interval$upper,
        g$guaranteed_content, config$design$success_tolerance
      )
      index <- index + 1L
      rows[[index]] <- data.frame(
        schema_version = tcspv_schema(),
        mode = mode,
        replication = replication,
        data_seed = generated[[key]]$seed,
        dgp_id = g$dgp_id,
        n = g$n,
        guaranteed_content = g$guaranteed_content,
        tolerance_confidence = g$tolerance_confidence,
        method_id = g$method_id,
        lower = interval$lower,
        upper = interval$upper,
        width = eval$width,
        true_content = eval$true_content,
        lower_omitted = eval$lower_omitted,
        upper_omitted = eval$upper_omitted,
        tolerance_success = eval$tolerance_success,
        content_deficit = eval$content_deficit,
        retained_count = interval$retained_count,
        certificate = interval$certificate,
        failed = interval$failed,
        failure_reason = interval$failure_reason,
        runtime_sec = unname(timed[["elapsed"]]),
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

tcspv_summary <- function(results, summary_confidence = 0.95) {
  keys <- c(
    "mode", "dgp_id", "n", "guaranteed_content", "tolerance_confidence",
    "method_id"
  )
  split_rows <- split(results, results[keys], drop = TRUE)
  rows <- lapply(split_rows, function(z) {
    successes <- sum(z$tolerance_success & !z$failed, na.rm = TRUE)
    attempted <- nrow(z)
    lower <- if (attempted > 0L) {
      tcspv_binom_lower(successes, attempted, summary_confidence)
    } else {
      NA_real_
    }
    finite_width <- z$width[is.finite(z$width)]
    finite_content <- z$true_content[is.finite(z$true_content)]
    data.frame(
      mode = z$mode[[1L]],
      dgp_id = z$dgp_id[[1L]],
      n = z$n[[1L]],
      guaranteed_content = z$guaranteed_content[[1L]],
      tolerance_confidence = z$tolerance_confidence[[1L]],
      method_id = z$method_id[[1L]],
      replications = attempted,
      failures = sum(z$failed),
      failure_rate = mean(z$failed),
      tolerance_success_rate = successes / attempted,
      tolerance_success_lower = lower,
      summary_confidence = summary_confidence,
      mean_width = tcspv_mean_or_na(finite_width),
      median_width = tcspv_median_or_na(finite_width),
      q90_width = tcspv_quantile_or_na(finite_width, 0.90),
      mean_true_content = tcspv_mean_or_na(finite_content),
      q10_true_content = tcspv_quantile_or_na(finite_content, 0.10),
      mean_content_deficit = tcspv_mean_or_na(z$content_deficit),
      mean_lower_omitted = tcspv_mean_or_na(z$lower_omitted),
      mean_upper_omitted = tcspv_mean_or_na(z$upper_omitted),
      mean_runtime_sec = tcspv_mean_or_na(z$runtime_sec),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

tcspv_atomic_csv <- function(value, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(paste0(".", basename(path), "-"), dirname(path))
  on.exit(unlink(tmp, force = TRUE), add = TRUE)
  utils::write.csv(value, tmp, row.names = FALSE, na = "")
  if (!file.rename(tmp, path)) tcspv_stop("Could not write ", path)
  invisible(path)
}

tcspv_atomic_json <- function(value, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(paste0(".", basename(path), "-"), dirname(path))
  on.exit(unlink(tmp, force = TRUE), add = TRUE)
  jsonlite::write_json(
    value, tmp, pretty = TRUE, auto_unbox = TRUE, null = "null",
    na = "null", digits = NA
  )
  if (!file.rename(tmp, path)) tcspv_stop("Could not write ", path)
  invisible(path)
}

tcspv_atomic_text <- function(value, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(paste0(".", basename(path), "-"), dirname(path))
  on.exit(unlink(tmp, force = TRUE), add = TRUE)
  writeLines(as.character(value), tmp, useBytes = TRUE)
  if (!file.rename(tmp, path)) tcspv_stop("Could not write ", path)
  invisible(path)
}

tcspv_file_manifest <- function(root, exclude = character(0)) {
  root_norm <- normalizePath(root, winslash = "/", mustWork = TRUE)
  files <- list.files(root, recursive = TRUE, full.names = TRUE,
                      all.files = TRUE, no.. = TRUE)
  files <- files[file.info(files)$isdir %in% FALSE]
  files <- normalizePath(files, winslash = "/", mustWork = TRUE)
  rel <- substring(files, nchar(root_norm) + 2L)
  keep <- !rel %in% exclude
  files <- files[keep]
  rel <- rel[keep]
  data.frame(
    path = rel,
    bytes = unname(file.info(files)$size),
    sha256 = vapply(files, digest::digest, character(1L),
                    file = TRUE, algo = "sha256"),
    stringsAsFactors = FALSE
  )
}

tcspv_git_state <- function(repo_root) {
  git <- function(args) {
    out <- suppressWarnings(system2(
      "git", c("-C", repo_root, "-c", "core.hooksPath=/dev/null", args),
      stdout = TRUE, stderr = TRUE
    ))
    status <- attr(out, "status")
    if (!is.null(status) && status != 0L) return(NA_character_)
    paste(out, collapse = "\n")
  }
  list(
    commit = trimws(git(c("rev-parse", "HEAD"))),
    branch = trimws(git(c("branch", "--show-current"))),
    status_short = git(c("status", "--short", "--untracked-files=all"))
  )
}

tcspv_write_preflight <- function(config, mode, output_dir, repo_root) {
  criticals <- tcspv_critical_counts(config, mode)
  gates <- data.frame(
    gate = config$gates$required_preflight_gates,
    pass = TRUE,
    stringsAsFactors = FALSE
  )
  tcspv_atomic_json(config, file.path(output_dir, "config.json"))
  tcspv_atomic_json(tcspv_git_state(repo_root),
                    file.path(output_dir, "source_state.json"))
  tcspv_atomic_csv(gates, file.path(output_dir, "preflight_gates.csv"))
  tcspv_atomic_csv(tcspv_dgps(config), file.path(output_dir, "dgp_contract.csv"))
  tcspv_atomic_csv(tcspv_methods(config),
                   file.path(output_dir, "method_contract.csv"))
  tcspv_atomic_csv(tcspv_design_grid(config, mode),
                   file.path(output_dir, "design_grid.csv"))
  tcspv_atomic_csv(criticals, file.path(output_dir, "critical_counts.csv"))
  tcspv_atomic_csv(
    tcspv_file_manifest(output_dir, exclude = "artifact_manifest.csv"),
    file.path(output_dir, "artifact_manifest.csv")
  )
  invisible(output_dir)
}

tcspv_write_run <- function(config, mode, output_dir, repo_root) {
  tcspv_atomic_json(config, file.path(output_dir, "config.json"))
  tcspv_atomic_json(tcspv_git_state(repo_root),
                    file.path(output_dir, "source_state.json"))
  message("[tcsp-validation] computing critical counts for ", mode)
  criticals <- tcspv_critical_counts(config, mode)
  message("[tcsp-validation] running repeated-sample grid for ", mode)
  results <- tcspv_run_repeated_sample(config, mode, criticals = criticals)
  message("[tcsp-validation] summarizing ", nrow(results), " rows")
  summary <- tcspv_summary(results, config$design$summary_confidence)
  failure_log <- results[results$failed, , drop = FALSE]
  tcspv_atomic_csv(tcspv_design_grid(config, mode),
                   file.path(output_dir, "design_grid.csv"))
  tcspv_atomic_csv(criticals, file.path(output_dir, "critical_counts.csv"))
  tcspv_atomic_csv(results, file.path(output_dir, "replication_results.csv"))
  tcspv_atomic_csv(summary, file.path(output_dir, "cell_summary.csv"))
  tcspv_atomic_csv(failure_log, file.path(output_dir, "failure_log.csv"))
  tcspv_atomic_json(list(
    schema_version = tcspv_schema(),
    mode = mode,
    replications = as.integer(config$modes[[mode]]$replications),
    rows = nrow(results),
    summary_rows = nrow(summary),
    failures = nrow(failure_log),
    closed_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    interpretation = config$interpretation
  ), file.path(output_dir, "closeout.json"))
  tcspv_atomic_csv(
    tcspv_file_manifest(output_dir, exclude = "artifact_manifest.csv"),
    file.path(output_dir, "artifact_manifest.csv")
  )
  invisible(output_dir)
}

tcspv_verify_run <- function(run_dir) {
  manifest_path <- file.path(run_dir, "artifact_manifest.csv")
  if (!file.exists(manifest_path)) {
    tcspv_stop("Missing artifact manifest: ", manifest_path)
  }
  manifest <- utils::read.csv(manifest_path, stringsAsFactors = FALSE)
  ok <- nrow(manifest) > 0L && !anyDuplicated(manifest$path) &&
    all(vapply(seq_len(nrow(manifest)), function(ii) {
      path <- file.path(run_dir, manifest$path[[ii]])
      file.exists(path) &&
        unname(file.info(path)$size) == manifest$bytes[[ii]] &&
        identical(digest::digest(file = path, algo = "sha256"),
                  manifest$sha256[[ii]])
    }, logical(1L)))
  if (!ok) tcspv_stop("Artifact manifest verification failed.")
  invisible(manifest)
}

tcspv_audit_schema <- function() {
  "rqrgibbs_tcsp_validation_pilot_audit/1.0.0"
}

tcspv_read_csv_required <- function(run_dir, name) {
  path <- file.path(run_dir, name)
  if (!file.exists(path)) tcspv_stop("Missing required TCSP run file: ", name)
  utils::read.csv(path, stringsAsFactors = FALSE)
}

tcspv_read_json_required <- function(run_dir, name) {
  path <- file.path(run_dir, name)
  if (!file.exists(path)) tcspv_stop("Missing required TCSP run file: ", name)
  jsonlite::read_json(path, simplifyVector = TRUE)
}

tcspv_assert_completed_repeated_run <- function(run_dir) {
  run_dir <- normalizePath(run_dir, winslash = "/", mustWork = TRUE)
  manifest <- tcspv_verify_run(run_dir)
  required <- c(
    "config.json", "source_state.json", "design_grid.csv",
    "critical_counts.csv", "replication_results.csv", "cell_summary.csv",
    "failure_log.csv", "closeout.json"
  )
  missing <- required[!file.exists(file.path(run_dir, required))]
  if (length(missing)) {
    tcspv_stop("TCSP run is incomplete; missing: ", paste(missing, collapse = ", "))
  }
  config <- tcspv_read_config(file.path(run_dir, "config.json"))
  tcspv_validate_config(config)
  closeout <- tcspv_read_json_required(run_dir, "closeout.json")
  if (!identical(closeout$schema_version, tcspv_schema())) {
    tcspv_stop("TCSP run closeout has an unsupported schema.")
  }
  results <- tcspv_read_csv_required(run_dir, "replication_results.csv")
  cell_summary <- tcspv_read_csv_required(run_dir, "cell_summary.csv")
  failure_log <- tcspv_read_csv_required(run_dir, "failure_log.csv")
  design_grid <- tcspv_read_csv_required(run_dir, "design_grid.csv")
  criticals <- tcspv_read_csv_required(run_dir, "critical_counts.csv")
  if (nrow(results) != as.integer(closeout$rows) ||
      nrow(cell_summary) != as.integer(closeout$summary_rows) ||
      nrow(failure_log) != as.integer(closeout$failures)) {
    tcspv_stop("TCSP run closeout accounting does not match artifact rows.")
  }
  list(
    run_dir = run_dir,
    manifest = manifest,
    config = config,
    source_state = tcspv_read_json_required(run_dir, "source_state.json"),
    closeout = closeout,
    design_grid = design_grid,
    critical_counts = criticals,
    replication_results = results,
    cell_summary = cell_summary,
    failure_log = failure_log
  )
}

tcspv_method_summary <- function(results) {
  split_rows <- split(results, results$method_id, drop = TRUE)
  rows <- lapply(split_rows, function(z) {
    failed <- as.logical(z$failed)
    success <- as.logical(z$tolerance_success) & !failed
    attempted <- !failed
    data.frame(
      method_id = z$method_id[[1L]],
      total_rows = nrow(z),
      attempted_rows = sum(attempted, na.rm = TRUE),
      failed_rows = sum(failed, na.rm = TRUE),
      failure_rate_all_rows = sum(failed, na.rm = TRUE) / nrow(z),
      tolerance_success_rows = sum(success, na.rm = TRUE),
      tolerance_success_rate_all_rows = sum(success, na.rm = TRUE) / nrow(z),
      tolerance_success_rate_attempted =
        if (sum(attempted, na.rm = TRUE) > 0L) {
          sum(success, na.rm = TRUE) / sum(attempted, na.rm = TRUE)
        } else {
          NA_real_
        },
      mean_width_attempted = tcspv_mean_or_na(z$width[attempted]),
      median_width_attempted = tcspv_median_or_na(z$width[attempted]),
      mean_true_content_attempted = tcspv_mean_or_na(z$true_content[attempted]),
      q10_true_content_attempted =
        tcspv_quantile_or_na(z$true_content[attempted], 0.10),
      mean_content_deficit_all_rows = tcspv_mean_or_na(z$content_deficit),
      max_content_deficit_all_rows =
        if (length(tcspv_finite(z$content_deficit))) {
          max(tcspv_finite(z$content_deficit))
        } else {
          NA_real_
        },
      mean_runtime_sec = tcspv_mean_or_na(z$runtime_sec),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out[order(out$method_id), , drop = FALSE]
}

tcspv_critical_count_summary <- function(criticals) {
  split_rows <- split(criticals, criticals$method_id, drop = TRUE)
  rows <- lapply(split_rows, function(z) {
    feasible <- !as.logical(z$infeasible)
    all_certificates <- tcspv_finite(z$certificate)
    feasible_certificates <- tcspv_finite(z$certificate[feasible])
    data.frame(
      method_id = z$method_id[[1L]],
      critical_rows = nrow(z),
      feasible_rows = sum(feasible, na.rm = TRUE),
      infeasible_rows = sum(!feasible, na.rm = TRUE),
      min_certificate_all_rows = if (length(all_certificates)) {
        min(all_certificates)
      } else {
        NA_real_
      },
      min_certificate_feasible = if (length(feasible_certificates)) {
        min(feasible_certificates)
      } else {
        NA_real_
      },
      min_n_sim = if (any(z$n_sim > 0L, na.rm = TRUE)) {
        min(z$n_sim[z$n_sim > 0L], na.rm = TRUE)
      } else {
        0L
      },
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out[order(out$method_id), , drop = FALSE]
}

tcspv_dkw_feasibility <- function(criticals) {
  dkw <- criticals[criticals$method_id == "tcsp_dkw", , drop = FALSE]
  dkw$retained_fraction <- dkw$retained_count / dkw$n
  dkw$retained_fraction_excess_over_content <-
    dkw$retained_fraction - dkw$guaranteed_content
  dkw$usable_action <- !as.logical(dkw$infeasible)
  dkw[order(dkw$n, dkw$guaranteed_content, dkw$tolerance_confidence),
      c(
        "n", "guaranteed_content", "tolerance_confidence", "retained_count",
        "retained_fraction", "retained_fraction_excess_over_content",
        "certificate", "infeasible", "usable_action"
      ), drop = FALSE]
}

tcspv_mc_calibration_health <- function(criticals) {
  mc <- criticals[criticals$method_id == "tcsp_mc", , drop = FALSE]
  if (!nrow(mc)) return(data.frame())
  mc$certificate_margin <- mc$certificate - mc$tolerance_confidence
  mc$certificate_gate_pass <- mc$certificate_margin >= -1e-12
  mc[order(mc$n, mc$guaranteed_content, mc$tolerance_confidence),
     c(
       "n", "guaranteed_content", "tolerance_confidence", "retained_count",
       "certificate", "certificate_margin", "certificate_gate_pass",
       "n_sim", "seed", "infeasible"
     ), drop = FALSE]
}

tcspv_normal_howe_sensitivity <- function(results, config) {
  normal <- results[results$method_id == "normal_howe", , drop = FALSE]
  if (!nrow(normal)) return(data.frame())
  dgp_contract <- tcspv_dgps(config)[
    , c("dgp_id", "parametric_normal_correctly_specified"), drop = FALSE
  ]
  split_rows <- split(normal, normal$dgp_id, drop = TRUE)
  rows <- lapply(split_rows, function(z) {
    failed <- as.logical(z$failed)
    success <- as.logical(z$tolerance_success) & !failed
    data.frame(
      dgp_id = z$dgp_id[[1L]],
      rows = nrow(z),
      failures = sum(failed, na.rm = TRUE),
      tolerance_success_rate_all_rows = sum(success, na.rm = TRUE) / nrow(z),
      mean_width_attempted = tcspv_mean_or_na(z$width[!failed]),
      mean_true_content_attempted = tcspv_mean_or_na(z$true_content[!failed]),
      mean_content_deficit_all_rows = tcspv_mean_or_na(z$content_deficit),
      stringsAsFactors = FALSE
    )
  })
  out <- merge(do.call(rbind, rows), dgp_contract, by = "dgp_id", all.x = TRUE)
  out[order(out$parametric_normal_correctly_specified, out$dgp_id,
            decreasing = TRUE), , drop = FALSE]
}

tcspv_next_stage_plan <- function() {
  data.frame(
    stage = c(
      "source-control gate",
      "full pilot design",
      "calibration budget",
      "sample-size grid",
      "competitor scope",
      "confirmatory gate",
      "manuscript integration"
    ),
    decision = c(
      "Relaunch any promotion-eligible run from a committed source state with a recorded branch and commit.",
      "Run a full iid univariate pilot before confirmatory promotion.",
      "Increase Uniform scan calibration beyond the compact pilot budget.",
      "Keep n=80 and n=250 as stress and transition points; add n=1200 before assessing DKW efficiency at high content/confidence.",
      "Keep Young-Mathew and calibrated BNP Gibbs disabled until tracked, tested implementations are added.",
      "Leave confirmatory execution fail-closed and require a new frozen config after the full pilot audit.",
      "Use only audited full-pilot or confirmatory summaries in article prose; never reuse local pilot output as theorem evidence."
    ),
    minimum_next_action = c(
      "Commit the audit plumbing, then launch the next run from that commit.",
      "Use all five tracked DGPs, both contents, both tolerance confidences, and active non-oracle comparators.",
      "Use at least 5000 Uniform calibration simulations per n/content/confidence cell for the full pilot; exact recursion remains a later theory task.",
      "Evaluate n in {80,250,600,1200}; n=600 is near range-wide for c=0.90 at high confidence under DKW.",
      "Add competitors only through source-controlled code, tests, and dependency declarations.",
      "Require passing manifest, source-state, failure-accounting, and precision gates before a confirmatory config can be authorized.",
      "Promote only claim-scoped empirical content/width statements, not response-likelihood or posterior-predictive claims."
    ),
    stringsAsFactors = FALSE
  )
}

tcspv_audit_gates <- function(bundle) {
  config <- bundle$config
  criticals <- bundle$critical_counts
  results <- bundle$replication_results
  source_state <- bundle$source_state
  closeout <- bundle$closeout
  disabled <- tcspv_methods(config)
  disabled <- disabled[!disabled$enabled, , drop = FALSE]
  source_status <- as.character(source_state$status_short %||% "")
  dkw <- criticals[criticals$method_id == "tcsp_dkw", , drop = FALSE]
  mc <- criticals[criticals$method_id == "tcsp_mc", , drop = FALSE]
  gates <- list(
    data.frame(
      gate_id = "artifact_manifest_verified",
      pass = TRUE,
      severity = "required",
      finding = "The source run artifact manifest verified by byte count and SHA-256.",
      next_action = "Retain the ignored source run locally; commit only compact audit artifacts.",
      stringsAsFactors = FALSE
    ),
    data.frame(
      gate_id = "closeout_accounting_matches",
      pass = nrow(results) == as.integer(closeout$rows) &&
        nrow(bundle$cell_summary) == as.integer(closeout$summary_rows) &&
        nrow(bundle$failure_log) == as.integer(closeout$failures),
      severity = "required",
      finding = "Closeout row counts match replication, summary, and failure artifacts.",
      next_action = "Block audit publication if this gate fails.",
      stringsAsFactors = FALSE
    ),
    data.frame(
      gate_id = "claim_scope_fail_closed",
      pass = isTRUE(config$generalized_bayes) &&
        !isTRUE(config$response_likelihood) &&
        !isTRUE(config$posterior_predictive_response_draws) &&
        isTRUE(config$claim_scope$iid_univariate_continuous_only) &&
        !isTRUE(config$claim_scope$regression_tolerance) &&
        !isTRUE(config$claim_scope$dynamic_tolerance),
      severity = "required",
      finding = "The config evaluates iid univariate population content, not response-likelihood or posterior-predictive coverage.",
      next_action = "Keep manuscript language within this scope.",
      stringsAsFactors = FALSE
    ),
    data.frame(
      gate_id = "confirmatory_fail_closed",
      pass = !isTRUE(config$execution$confirmatory_authorized) &&
        isTRUE(config$execution$confirmatory_requires_new_config),
      severity = "required",
      finding = "Confirmatory execution remains disabled in the pilot config.",
      next_action = "Create a separate frozen confirmatory config only after full-pilot audit.",
      stringsAsFactors = FALSE
    ),
    data.frame(
      gate_id = "pilot_rehearsal_size",
      pass = as.integer(closeout$replications) >= 8L,
      severity = "rehearsal",
      finding = paste0("The source run has ", closeout$replications,
                       " replications per cell, enough for plumbing but not precision claims."),
      next_action = "Use the run to validate wiring only.",
      stringsAsFactors = FALSE
    ),
    data.frame(
      gate_id = "publication_precision_not_met",
      pass = FALSE,
      severity = "promotion_blocker",
      finding = "Eight replications per cell are not enough for publication-quality Monte Carlo precision.",
      next_action = "Run a full pilot with a larger replication and calibration budget.",
      stringsAsFactors = FALSE
    ),
    data.frame(
      gate_id = "source_state_committed_for_rehearsal",
      pass = grepl("^[0-9a-f]{40}$", as.character(source_state$commit)),
      severity = "required",
      finding = paste0("The source run recorded commit ", source_state$commit, "."),
      next_action = "For promotion-eligible runs, require the exact committed audit and runner source.",
      stringsAsFactors = FALSE
    ),
    data.frame(
      gate_id = "source_state_clean_for_promotion",
      pass = identical(trimws(source_status), ""),
      severity = "promotion_blocker",
      finding = if (identical(trimws(source_status), "")) {
        "The source run recorded no local status changes."
      } else {
        "The source run recorded local status changes; treat it as a rehearsal."
      },
      next_action = "Relaunch the full pilot from a committed branch after this audit plumbing lands.",
      stringsAsFactors = FALSE
    ),
    data.frame(
      gate_id = "dkw_feasibility_mapped",
      pass = nrow(dkw) > 0L && any(as.logical(dkw$infeasible)) &&
        any(!as.logical(dkw$infeasible)),
      severity = "required",
      finding = "The pilot includes both infeasible and feasible DKW retained-count cells.",
      next_action = "Use the DKW map to choose full-pilot sample sizes.",
      stringsAsFactors = FALSE
    ),
    data.frame(
      gate_id = "mc_calibration_conservative",
      pass = nrow(mc) > 0L &&
        all(mc$certificate + 1e-12 >= mc$tolerance_confidence) &&
        all(!as.logical(mc$infeasible)),
      severity = "required",
      finding = "Monte Carlo scan retained counts meet the one-sided lower-bound certificate in the pilot.",
      next_action = "Increase calibration simulations before the full pilot.",
      stringsAsFactors = FALSE
    ),
    data.frame(
      gate_id = "disabled_competitors_documented",
      pass = all(c("young_mathew", "calibrated_bnp_gibbs") %in%
                   disabled$method_id) &&
        all(nzchar(disabled$disabled_reason)),
      severity = "required",
      finding = "Unavailable competitors are disabled with explicit reasons.",
      next_action = "Do not include untracked competitors in the next launch.",
      stringsAsFactors = FALSE
    )
  )
  do.call(rbind, gates)
}

tcspv_audit_readme <- function(summary, gates) {
  failing_required <- gates[!gates$pass & gates$severity == "required", ,
                            drop = FALSE]
  promotion_blockers <- gates[gates$severity == "promotion_blocker", ,
                              drop = FALSE]
  c(
    "# TCSP validation pilot audit",
    "",
    "This compact bundle audits the ignored local TCSP validation pilot run.",
    "It is a wiring and diagnosis record, not manuscript evidence and not a theorem proof.",
    "",
    "## Verdict",
    "",
    paste0("- status: `", summary$status, "`"),
    paste0("- source run: `", summary$source_run_dir, "`"),
    paste0("- rows: ", summary$rows),
    paste0("- summary rows: ", summary$summary_rows),
    paste0("- failures: ", summary$failures),
    paste0("- required gate failures: ", nrow(failing_required)),
    paste0("- promotion blockers recorded: ", nrow(promotion_blockers)),
    "",
    "The pilot validated the run plumbing, artifact manifest, DGP/method contracts,",
    "failure accounting, and conservative TCSP scan-calibration behavior. It did",
    "not meet promotion conditions because the source run was a compact",
    "8-replication rehearsal and its recorded source state included local changes.",
    "",
    "## Next Step",
    "",
    "Launch a full iid univariate pilot only after this audit plumbing is merged.",
    "The full pilot should use a committed source state, a larger Monte Carlo scan",
    "calibration budget, all tracked DGPs, both tolerance-confidence levels, and",
    "a sample-size grid that adds `n=1200` so high-content DKW intervals are not",
    "only range-wide stress cases.",
    "",
    "## Files",
    "",
    "- `audit_summary.json`: machine-readable verdict and source-run accounting.",
    "- `audit_gates.csv`: pass/fail gates and promotion blockers.",
    "- `method_summary.csv`: method-level results from replication-level data.",
    "- `cell_summary_compact.csv`: cell-level pilot summaries with failure cells retained.",
    "- `critical_count_summary.csv`: calibration feasibility summary by method.",
    "- `dkw_feasibility.csv`: DKW retained-count feasibility by sample size/content/confidence.",
    "- `mc_calibration_health.csv`: Uniform Monte Carlo scan calibration certificates.",
    "- `normal_howe_sensitivity.csv`: normal-theory competitor behavior by DGP.",
    "- `next_stage_plan.csv`: source-controlled launch plan for the full pilot.",
    "- `source_run_manifest.csv`: copied manifest for the ignored source run.",
    "- `artifact_hashes.csv`: SHA-256 hashes for this audit bundle."
  )
}

tcspv_write_pilot_audit <- function(run_dir, output_dir, replace = FALSE) {
  bundle <- tcspv_assert_completed_repeated_run(run_dir)
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
  if ((dir.exists(output_dir) || file.exists(output_dir)) && !isTRUE(replace)) {
    tcspv_stop("TCSP pilot audit output already exists: ", output_dir)
  }
  method_summary <- tcspv_method_summary(bundle$replication_results)
  critical_summary <- tcspv_critical_count_summary(bundle$critical_counts)
  dkw_feasibility <- tcspv_dkw_feasibility(bundle$critical_counts)
  mc_health <- tcspv_mc_calibration_health(bundle$critical_counts)
  normal_sensitivity <- tcspv_normal_howe_sensitivity(
    bundle$replication_results, bundle$config
  )
  gates <- tcspv_audit_gates(bundle)
  status <- if (any(!gates$pass & gates$severity == "required")) {
    "audit_failed_required_gate"
  } else {
    "audited_rehearsal_not_promoted"
  }
  summary <- list(
    schema_version = tcspv_audit_schema(),
    status = status,
    source_run_dir = bundle$run_dir,
    source_run_commit = as.character(bundle$source_state$commit),
    source_run_branch = as.character(bundle$source_state$branch),
    source_state_clean = identical(
      trimws(as.character(bundle$source_state$status_short %||% "")), ""
    ),
    mode = as.character(bundle$closeout$mode),
    replications = as.integer(bundle$closeout$replications),
    rows = as.integer(bundle$closeout$rows),
    summary_rows = as.integer(bundle$closeout$summary_rows),
    failures = as.integer(bundle$closeout$failures),
    required_gate_failures = sum(!gates$pass & gates$severity == "required"),
    promotion_blockers = sum(gates$severity == "promotion_blocker"),
    full_pilot_recommended = TRUE,
    confirmatory_ready = FALSE,
    source_run_reusable_as_confirmatory_evidence = FALSE,
    response_likelihood = FALSE,
    posterior_predictive_response_draws = FALSE,
    audited_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
  )
  dir.create(dirname(output_dir), recursive = TRUE, showWarnings = FALSE)
  stage <- tempfile(paste0(".", basename(output_dir), "-"),
                    tmpdir = dirname(output_dir))
  dir.create(stage, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(stage, recursive = TRUE, force = TRUE), add = TRUE)
  tcspv_atomic_json(summary, file.path(stage, "audit_summary.json"))
  tcspv_atomic_csv(gates, file.path(stage, "audit_gates.csv"))
  tcspv_atomic_csv(method_summary, file.path(stage, "method_summary.csv"))
  compact_cols <- c(
    "mode", "dgp_id", "n", "guaranteed_content", "tolerance_confidence",
    "method_id", "replications", "failures", "failure_rate",
    "tolerance_success_rate", "tolerance_success_lower", "mean_width",
    "mean_true_content", "mean_content_deficit", "mean_runtime_sec"
  )
  tcspv_atomic_csv(
    bundle$cell_summary[, compact_cols, drop = FALSE],
    file.path(stage, "cell_summary_compact.csv")
  )
  tcspv_atomic_csv(critical_summary,
                   file.path(stage, "critical_count_summary.csv"))
  tcspv_atomic_csv(dkw_feasibility, file.path(stage, "dkw_feasibility.csv"))
  tcspv_atomic_csv(mc_health, file.path(stage, "mc_calibration_health.csv"))
  tcspv_atomic_csv(normal_sensitivity,
                   file.path(stage, "normal_howe_sensitivity.csv"))
  tcspv_atomic_csv(tcspv_next_stage_plan(),
                   file.path(stage, "next_stage_plan.csv"))
  tcspv_atomic_csv(bundle$manifest, file.path(stage, "source_run_manifest.csv"))
  tcspv_atomic_text(tcspv_audit_readme(summary, gates),
                    file.path(stage, "README.md"))
  tcspv_atomic_csv(
    tcspv_file_manifest(stage, exclude = "artifact_hashes.csv"),
    file.path(stage, "artifact_hashes.csv")
  )
  if (dir.exists(output_dir) || file.exists(output_dir)) {
    unlink(output_dir, recursive = TRUE, force = TRUE)
  }
  dir.create(dirname(output_dir), recursive = TRUE, showWarnings = FALSE)
  if (!file.rename(stage, output_dir)) {
    tcspv_stop("Could not publish TCSP pilot audit bundle.")
  }
  invisible(output_dir)
}
