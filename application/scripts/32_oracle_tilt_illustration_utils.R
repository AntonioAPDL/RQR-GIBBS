# Utilities for single-data-set ordinary and mean-tilted RQR illustrations.
#
# These helpers intentionally live outside the package namespace. They generate
# reproducible, lightweight examples for the manuscript workflow and do not
# define a production simulation protocol.

`%||%` <- function(x, y) if (is.null(x)) y else x

oti_stop <- function(..., call. = FALSE) {
  stop(paste0(...), call. = call.)
}

oti_scalar <- function(x, name, lower = -Inf, upper = Inf) {
  x <- as.numeric(x)[1L]
  if (!is.finite(x) || x < lower || x > upper) {
    oti_stop(name, " must be finite and in [", lower, ", ", upper, "].")
  }
  x
}

oti_integer <- function(x, name, lower = 0L) {
  x <- as.numeric(x)[1L]
  if (!is.finite(x) || x != floor(x) || x < lower ||
      x > .Machine$integer.max) {
    oti_stop(name, " must be an integer not smaller than ", lower, ".")
  }
  as.integer(x)
}

oti_now_id <- function(prefix = "oracle_tilt_illustrations") {
  paste0(prefix, "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
}

oti_ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  normalizePath(path, mustWork = TRUE)
}

oti_write_csv <- function(x, path, row.names = FALSE) {
  oti_ensure_dir(dirname(path))
  write.csv(x, path, row.names = row.names, na = "")
  invisible(path)
}

oti_write_json <- function(x, path, pretty = TRUE) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    oti_stop("jsonlite is required to write illustration JSON artifacts.")
  }
  oti_ensure_dir(dirname(path))
  txt <- jsonlite::toJSON(
    x, auto_unbox = TRUE, pretty = pretty, digits = NA, null = "null"
  )
  writeLines(txt, path, useBytes = TRUE)
  invisible(path)
}

oti_read_json <- function(path) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    oti_stop("jsonlite is required to read the illustration config.")
  }
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

oti_file_sha256 <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  if (!requireNamespace("digest", quietly = TRUE)) {
    return(NA_character_)
  }
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

oti_git_state <- function(repo_root = ".") {
  run_git <- function(args) {
    out <- tryCatch(
      system2("git", c("-C", repo_root, args), stdout = TRUE, stderr = TRUE),
      error = function(e) NA_character_
    )
    attr(out, "status") <- attr(out, "status") %||% 0L
    out
  }
  branch <- run_git(c("rev-parse", "--abbrev-ref", "HEAD"))
  commit <- run_git(c("rev-parse", "HEAD"))
  status <- run_git(c("status", "--short"))
  list(
    branch = if (length(branch)) branch[1L] else NA_character_,
    commit = if (length(commit)) commit[1L] else NA_character_,
    clean = length(status) == 0L,
    status_short = status
  )
}

oti_merge_control <- function(base, override) {
  out <- base %||% list()
  if (!is.null(override)) {
    for (nm in names(override)) out[[nm]] <- override[[nm]]
  }
  out
}

oti_normalize_targets <- function(x) {
  out <- toupper(as.character(x %||% c("RQR", "ET", "SH")))
  bad <- setdiff(out, c("RQR", "ET", "SH"))
  if (length(bad)) {
    oti_stop("Unknown target labels: ", paste(bad, collapse = ", "))
  }
  unique(out)
}

oti_al_law <- function(tau = 0.65, standardized = TRUE) {
  tau <- oti_scalar(tau, "tau", lower = .Machine$double.eps,
                    upper = 1 - .Machine$double.eps)
  mean_raw <- (1 - 2 * tau) / (tau * (1 - tau))
  var_raw <- (1 - 2 * tau + 2 * tau^2) / (tau^2 * (1 - tau)^2)
  sd_raw <- sqrt(var_raw)
  q_raw <- function(p) {
    p <- pmin(pmax(as.numeric(p), .Machine$double.eps),
              1 - .Machine$double.eps)
    ifelse(
      p < tau,
      log(p / tau) / (1 - tau),
      -log((1 - p) / (1 - tau)) / tau
    )
  }
  p_raw <- function(x) {
    x <- as.numeric(x)
    ifelse(
      x < 0,
      tau * exp((1 - tau) * x),
      1 - (1 - tau) * exp(-tau * x)
    )
  }
  d_raw <- function(x) {
    x <- as.numeric(x)
    tau * (1 - tau) * ifelse(
      x < 0,
      exp((1 - tau) * x),
      exp(-tau * x)
    )
  }
  if (isTRUE(standardized)) {
    q <- function(p) (q_raw(p) - mean_raw) / sd_raw
    p <- function(x) p_raw(as.numeric(x) * sd_raw + mean_raw)
    d <- function(x) sd_raw * d_raw(as.numeric(x) * sd_raw + mean_raw)
    r <- function(n) q(stats::runif(n))
    mean <- 0
    sd <- 1
  } else {
    q <- q_raw
    p <- p_raw
    d <- d_raw
    r <- function(n) q_raw(stats::runif(n))
    mean <- mean_raw
    sd <- sd_raw
  }
  structure(
    list(
      family = "asymmetric_laplace",
      tau = tau,
      standardized = isTRUE(standardized),
      mean = mean,
      sd = sd,
      q = q,
      p = p,
      d = d,
      r = r,
      raw_mean = mean_raw,
      raw_sd = sd_raw
    ),
    class = "oti_distribution_law"
  )
}

oti_law_from_config <- function(config) {
  innovation <- config$innovation %||% list()
  family <- tolower(as.character(innovation$family %||% "asymmetric_laplace"))
  if (!family %in% c("asymmetric_laplace", "al")) {
    oti_stop("Only asymmetric_laplace is currently supported for illustrations.")
  }
  oti_al_law(
    tau = innovation$tau %||% 0.65,
    standardized = isTRUE(innovation$standardized %||% TRUE)
  )
}

oti_retained_mean <- function(law, lower, upper) {
  if (!is.finite(lower) || !is.finite(upper) || lower >= upper) {
    return(NA_real_)
  }
  stats::integrate(
    function(z) z * law$d(z), lower = lower, upper = upper,
    rel.tol = 5e-10, subdivisions = 1000L
  )$value
}

oti_interval_from_u <- function(law, u, coverage_level) {
  coverage_level <- oti_scalar(coverage_level, "coverage_level", 1e-8, 1 - 1e-8)
  u <- as.numeric(u)[1L]
  if (!is.finite(u) || u < 0 || u > 1 - coverage_level) {
    oti_stop("u must be finite and in [0, 1 - coverage_level].")
  }
  lower <- law$q(u)
  upper <- law$q(u + coverage_level)
  content <- law$p(upper) - law$p(lower)
  retained_mean <- oti_retained_mean(law, lower, upper)
  data.frame(
    u = u,
    lower_innovation = lower,
    upper_innovation = upper,
    width_innovation = upper - lower,
    retained_mean_innovation = retained_mean,
    delta_innovation = retained_mean,
    content = content,
    stringsAsFactors = FALSE
  )
}

oti_oracle_equal_tailed_interval <- function(law, coverage_level) {
  u <- (1 - coverage_level) / 2
  cbind(target = "ET", oti_interval_from_u(law, u, coverage_level))
}

oti_oracle_shortest_interval <- function(law, coverage_level,
                                         grid_size = 2001L) {
  grid_size <- oti_integer(grid_size, "grid_size", 101L)
  coverage_level <- oti_scalar(coverage_level, "coverage_level", 1e-8, 1 - 1e-8)
  objective <- function(u) {
    law$q(u + coverage_level) - law$q(u)
  }
  upper_u <- 1 - coverage_level
  grid <- seq(0, upper_u, length.out = grid_size)
  width <- vapply(grid, objective, numeric(1L))
  best <- grid[which.min(width)]
  bracket_half <- upper_u / max(20, grid_size - 1L)
  lo <- max(0, best - bracket_half)
  hi <- min(upper_u, best + bracket_half)
  opt <- stats::optimize(objective, interval = c(lo, hi))$minimum
  cbind(target = "SH", oti_interval_from_u(law, opt, coverage_level))
}

oti_oracle_rqr_interval <- function(law, coverage_level) {
  coverage_level <- oti_scalar(coverage_level, "coverage_level", 1e-8, 1 - 1e-8)
  upper_u <- 1 - coverage_level
  balance <- function(u) {
    z <- oti_interval_from_u(law, u, coverage_level)
    z$retained_mean_innovation
  }
  eps <- max(.Machine$double.eps^0.25, upper_u * 1e-8)
  f0 <- balance(0)
  f1 <- balance(upper_u)
  if (abs(f0) < 1e-9) {
    u <- 0
  } else if (abs(f1) < 1e-9) {
    u <- upper_u
  } else if (sign(f0) == sign(f1)) {
    # Some extreme laws can place the retained-mean zero outside the finite
    # root path. Select the numerically closest path point and record the
    # residual; the diagnostics will make this explicit.
    grid <- seq(0, upper_u, length.out = 5001L)
    vals <- vapply(grid, balance, numeric(1L))
    u <- grid[which.min(abs(vals))]
  } else {
    u <- stats::uniroot(balance, c(eps, upper_u - eps),
                        extendInt = "yes", tol = 1e-12)$root
  }
  cbind(target = "RQR", oti_interval_from_u(law, u, coverage_level))
}

oti_oracle_targets <- function(law, coverage_level, targets = c("RQR", "ET", "SH")) {
  targets <- oti_normalize_targets(targets)
  rows <- list(
    RQR = oti_oracle_rqr_interval(law, coverage_level),
    ET = oti_oracle_equal_tailed_interval(law, coverage_level),
    SH = oti_oracle_shortest_interval(law, coverage_level)
  )
  out <- do.call(rbind, rows[targets])
  rownames(out) <- NULL
  out$coverage_level <- coverage_level
  out$law_family <- law$family
  out$law_tau <- law$tau
  out$law_standardized <- law$standardized
  out
}

oti_targets_by_index <- function(mu, scale, oracle_targets,
                                 observed = rep(TRUE, length(mu))) {
  mu <- as.numeric(mu)
  scale <- as.numeric(scale)
  observed <- as.logical(observed)
  if (length(mu) != length(scale) || length(mu) != length(observed)) {
    oti_stop("mu, scale, and observed must have the same length.")
  }
  if (any(!is.finite(mu)) || any(!is.finite(scale)) || any(scale <= 0)) {
    oti_stop("mu and scale must be finite and scale must be positive.")
  }
  rows <- lapply(seq_len(nrow(oracle_targets)), function(i) {
    z <- oracle_targets[i, ]
    delta <- scale * z$delta_innovation
    data.frame(
      target = z$target,
      index = seq_along(mu),
      observed = observed,
      mean_truth = mu,
      scale_truth = scale,
      oracle_lower = mu + scale * z$lower_innovation,
      oracle_upper = mu + scale * z$upper_innovation,
      oracle_width = scale * z$width_innovation,
      mean_tilt = ifelse(observed, delta, NA_real_),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

oti_fixed_design_dgp <- function(config, law) {
  cfg <- config$fixed_design %||% list()
  n <- oti_integer(cfg$n %||% 90L, "fixed_design$n", 20L)
  seed <- oti_integer(cfg$seed %||% 202607281L, "fixed_design$seed", 0L)
  set.seed(seed)
  x <- seq(-1, 1, length.out = n)
  X <- cbind(`(Intercept)` = 1, x = x, x2 = x^2)
  mu <- 0.35 + 0.85 * x - 0.30 * x^2
  scale <- 0.52 + 0.15 * (x + 1) / 2
  y <- mu + scale * law$r(n)
  list(
    family = "fixed_design",
    seed = seed,
    x = x,
    X = X,
    y = y,
    mean_truth = mu,
    scale_truth = scale,
    observed = rep(TRUE, n)
  )
}

oti_dlm_dgp <- function(config, law) {
  cfg <- config$dlm %||% list()
  T <- oti_integer(cfg$T %||% 100L, "dlm$T", 20L)
  seed <- oti_integer(cfg$seed %||% 202607282L, "dlm$seed", 0L)
  missing_times <- as.integer(unlist(cfg$missing_times %||% c(35L, 36L, 70L)))
  missing_times <- missing_times[missing_times >= 1L & missing_times <= T]
  set.seed(seed)
  level_sd <- oti_scalar(cfg$state_sd_level %||% 0.035, "dlm$state_sd_level", 0)
  slope_sd <- oti_scalar(cfg$state_sd_slope %||% 0.006, "dlm$state_sd_slope", 0)
  theta <- matrix(NA_real_, T, 2L)
  theta[1L, ] <- c(0.15, 0.015)
  if (T > 1L) {
    for (tt in 2:T) {
      theta[tt, 2L] <- theta[tt - 1L, 2L] + stats::rnorm(1L, 0, slope_sd)
      theta[tt, 1L] <- theta[tt - 1L, 1L] + theta[tt - 1L, 2L] +
        stats::rnorm(1L, 0, level_sd)
    }
  }
  scale <- rep(oti_scalar(cfg$scale %||% 0.55, "dlm$scale", 1e-8), T)
  y_full <- theta[, 1L] + scale * law$r(T)
  y <- y_full
  if (length(missing_times)) y[missing_times] <- NA_real_
  model <- rqrgibbs::rqr_polytrend(
    order = 2L,
    m0 = c(0, 0),
    C0 = diag(c(4, 1)),
    name = "local_linear"
  )
  W <- diag(c(level_sd^2, slope_sd^2))
  list(
    family = "dlm",
    seed = seed,
    time = seq_len(T),
    y = y,
    y_full = y_full,
    model = model,
    W = W,
    mean_truth = theta[, 1L],
    state_truth = theta,
    scale_truth = scale,
    observed = !is.na(y),
    missing_times = missing_times
  )
}

oti_desn_signal <- function(T, seed) {
  set.seed(seed)
  driver <- sin(seq_len(T) / 8) + 0.3 * cos(seq_len(T) / 17)
  mu <- numeric(T)
  mu[1L] <- driver[1L]
  if (T > 1L) {
    for (tt in 2:T) {
      mu[tt] <- 0.65 * mu[tt - 1L] + 0.45 * tanh(driver[tt]) +
        0.10 * sin(mu[tt - 1L])
    }
  }
  mu
}

oti_desn_dgp <- function(config, law) {
  cfg <- config$desn %||% list()
  T <- oti_integer(cfg$T %||% 150L, "desn$T", 30L)
  seed <- oti_integer(cfg$seed %||% 202607283L, "desn$seed", 0L)
  washout <- oti_integer(cfg$washout %||% 20L, "desn$washout", 0L)
  if (washout >= T - 5L) oti_stop("desn$washout must leave fitted observations.")
  mu <- oti_desn_signal(T, seed)
  scale <- 0.45 + 0.08 * (seq_len(T) / T)
  set.seed(seed + 1009L)
  y <- mu + scale * law$r(T)
  list(
    family = "desn",
    seed = seed,
    time = seq_len(T),
    y = y,
    mean_truth = mu,
    scale_truth = scale,
    observed = rep(TRUE, T),
    washout = washout
  )
}

oti_mcmc_control <- function(config, family, quick = FALSE, seed = NULL) {
  family_cfg <- config[[family]] %||% list()
  base <- config$mcmc_control %||% list()
  fam <- family_cfg$mcmc_control %||% list()
  quick_ctl <- if (isTRUE(quick)) {
    config$quick_mcmc_control %||% list(n_burn = 30L, n_mcmc = 60L, thin = 1L)
  } else {
    list()
  }
  out <- oti_merge_control(oti_merge_control(base, fam), quick_ctl)
  out$seed <- seed %||% out$seed %||% 1L
  out$verbose <- isTRUE(out$verbose %||% FALSE)
  out
}

oti_ridge_prior <- function(tau2 = 100) {
  rqrgibbs::beta_prior("ridge", ridge = list(tau2 = tau2))
}

oti_target_row <- function(targets_by_index, target) {
  out <- targets_by_index[targets_by_index$target == target, , drop = FALSE]
  if (!nrow(out)) oti_stop("No target rows found for ", target, ".")
  out
}

oti_interval_metrics <- function(pred, truth, y, index_name = "index") {
  lower <- as.numeric(pred$lower_mean)
  upper <- as.numeric(pred$upper_mean)
  ok <- is.finite(y)
  data.frame(
    endpoint_rmse = sqrt(mean(
      (lower[ok] - truth$oracle_lower[ok])^2 +
        (upper[ok] - truth$oracle_upper[ok])^2,
      na.rm = TRUE
    ) / 2),
    width_rmse = sqrt(mean(
      (as.numeric(pred$width_mean)[ok] - truth$oracle_width[ok])^2,
      na.rm = TRUE
    )),
    realized_coverage = mean(y[ok] >= lower[ok] & y[ok] <= upper[ok]),
    mean_width = mean(as.numeric(pred$width_mean)[ok]),
    oracle_mean_width = mean(truth$oracle_width[ok]),
    n_observed = sum(ok),
    stringsAsFactors = FALSE
  )
}

oti_row_quantile <- function(x, probs) {
  x <- as.matrix(x)
  out <- t(apply(x, 1L, stats::quantile, probs = probs, na.rm = TRUE,
                 names = FALSE, type = 8))
  colnames(out) <- paste0("q", sprintf("%02d", round(100 * probs)))
  out
}

oti_curve_frame <- function(family, target, x, y, pred, truth) {
  lower_q <- oti_row_quantile(pred$lower_draws, c(0.05, 0.95))
  upper_q <- oti_row_quantile(pred$upper_draws, c(0.05, 0.95))
  midpoint_q <- oti_row_quantile(pred$midpoint_draws, c(0.05, 0.95))
  width_q <- oti_row_quantile(pred$width_draws, c(0.05, 0.95))
  data.frame(
    family = family,
    target = target,
    index = seq_along(x),
    x = as.numeric(x),
    y = as.numeric(y),
    mean_truth = truth$mean_truth,
    oracle_lower = truth$oracle_lower,
    oracle_upper = truth$oracle_upper,
    fit_lower = as.numeric(pred$lower_mean),
    fit_upper = as.numeric(pred$upper_mean),
    fit_midpoint = as.numeric(pred$midpoint_mean),
    fit_width = as.numeric(pred$width_mean),
    fit_lower_q05 = lower_q[, 1L],
    fit_lower_q95 = lower_q[, 2L],
    fit_upper_q05 = upper_q[, 1L],
    fit_upper_q95 = upper_q[, 2L],
    fit_midpoint_q05 = midpoint_q[, 1L],
    fit_midpoint_q95 = midpoint_q[, 2L],
    fit_width_q05 = width_q[, 1L],
    fit_width_q95 = width_q[, 2L],
    observed = is.finite(y),
    stringsAsFactors = FALSE
  )
}

oti_fit_fixed_design_target <- function(dgp, targets_by_index, target,
                                        config, quick = FALSE) {
  ctl <- oti_mcmc_control(
    config, "fixed_design", quick = quick,
    seed = (config$fixed_design$seed %||% 202607281L) +
      match(target, c("RQR", "ET", "SH")) * 100L
  )
  tau2 <- as.numeric((config$fixed_design %||% list())$ridge_tau2 %||% 250)[1L]
  truth <- oti_target_row(targets_by_index, target)
  fit <- rqrgibbs::rqr_mcmc_fit(
    y = dgp$y,
    X = dgp$X,
    coverage_level = config$coverage_level %||% 0.8,
    learning_rate = config$learning_rate %||% 1,
    learning_rate_mode = "fixed_rate",
    mean_tilt = truth$mean_tilt,
    beta_prior_obj = oti_ridge_prior(tau2),
    numerical_policy = "fail",
    mcmc_control = ctl
  )
  pred <- rqrgibbs::predict_interval(fit, X_new = dgp$X)
  metrics <- oti_interval_metrics(pred, truth, dgp$y)
  summary <- cbind(
    data.frame(
      family = "fixed_design",
      target = target,
      fit_status = "ok",
      mean_tilt_mode = fit$model_spec$mean_tilt_mode,
      loss_name = fit$model_spec$loss_name,
      n_draws = nrow(fit$samp.beta_root1),
      numerical_repair_count = fit$model_spec$numerical_repair_count,
      stringsAsFactors = FALSE
    ),
    metrics
  )
  list(
    fit = fit,
    summary = summary,
    curves = oti_curve_frame("fixed_design", target, dgp$x, dgp$y, pred, truth)
  )
}

oti_fit_dlm_target <- function(dgp, targets_by_index, target, config,
                               quick = FALSE) {
  ctl <- oti_mcmc_control(
    config, "dlm", quick = quick,
    seed = (config$dlm$seed %||% 202607282L) +
      match(target, c("RQR", "ET", "SH")) * 100L
  )
  ctl$backend <- ctl$backend %||% "cpp"
  ctl$store_state_draws <- isTRUE(ctl$store_state_draws %||% FALSE)
  truth <- oti_target_row(targets_by_index, target)
  fit <- rqrgibbs::rqr_dlm_fit(
    y = dgp$y,
    model = dgp$model,
    coverage_level = config$coverage_level %||% 0.8,
    evolution_mode = "fixed_W",
    W = dgp$W,
    learning_rate = config$learning_rate %||% 1,
    learning_rate_mode = "fixed_rate",
    mean_tilt = truth$mean_tilt,
    numerical_policy = "fail",
    mcmc_control = ctl
  )
  pred <- rqrgibbs::predict_interval(fit)
  metrics <- oti_interval_metrics(pred, truth, dgp$y)
  summary <- cbind(
    data.frame(
      family = "dlm",
      target = target,
      fit_status = "ok",
      mean_tilt_mode = fit$model_spec$mean_tilt_mode,
      loss_name = fit$model_spec$loss_name,
      evolution_mode = fit$model_spec$evolution_mode,
      n_draws = ncol(fit$samp.eta_root1),
      numerical_repair_count = fit$model_spec$numerical_repair_count,
      stringsAsFactors = FALSE
    ),
    metrics
  )
  list(
    fit = fit,
    summary = summary,
    curves = oti_curve_frame("dlm", target, dgp$time, dgp$y, pred, truth)
  )
}

oti_exdqlm_available_for_desn <- function() {
  requireNamespace("exdqlm", quietly = TRUE) &&
    "qdesn_fit_vb" %in% getNamespaceExports("exdqlm")
}

oti_fit_desn_target <- function(dgp, targets_by_index, target, config,
                                quick = FALSE) {
  if (!oti_exdqlm_available_for_desn()) {
    oti_stop(
      "DESN illustrations require an available exdqlm namespace exporting ",
      "qdesn_fit_vb. Materialize the pinned runtime before requesting DESN."
    )
  }
  ctl <- oti_mcmc_control(
    config, "desn", quick = quick,
    seed = (config$desn$seed %||% 202607283L) +
      match(target, c("RQR", "ET", "SH")) * 100L
  )
  truth_full <- oti_target_row(targets_by_index, target)
  idx <- seq.int(dgp$washout + 1L, length(dgp$y))
  truth <- truth_full[idx, , drop = FALSE]
  desn_cfg <- config$desn %||% list()
  fit <- rqrgibbs::rqr_desn_fit(
    y = dgp$y,
    coverage_level = config$coverage_level %||% 0.8,
    inference = "mcmc",
    learning_rate = config$learning_rate %||% 1,
    learning_rate_mode = "fixed_rate",
    mean_tilt = truth$mean_tilt,
    washout = dgp$washout,
    n_reservoir = desn_cfg$n_reservoir %||% 40L,
    spectral_radius = desn_cfg$spectral_radius %||% 0.75,
    leak_rate = desn_cfg$leak_rate %||% 0.25,
    input_scale = desn_cfg$input_scale %||% 0.35,
    seed = dgp$seed,
    mcmc_args = list(
      beta_prior_type = "ridge",
      beta_ridge_tau2 = desn_cfg$ridge_tau2 %||% 100,
      mcmc_control = ctl,
      numerical_policy = "fail"
    )
  )
  pred <- rqrgibbs::predict_interval(fit, X_new = fit$X)
  metrics <- oti_interval_metrics(pred, truth, fit$fit$y)
  summary <- cbind(
    data.frame(
      family = "desn",
      target = target,
      fit_status = "ok",
      mean_tilt_mode = fit$model_spec$mean_tilt_mode,
      loss_name = fit$model_spec$loss_name,
      n_draws = nrow(fit$fit$samp.beta_root1),
      numerical_repair_count = fit$fit$model_spec$numerical_repair_count,
      stringsAsFactors = FALSE
    ),
    metrics
  )
  list(
    fit = fit,
    summary = summary,
    curves = oti_curve_frame(
      "desn", target, dgp$time[idx], fit$fit$y, pred, truth
    )
  )
}

oti_plot_curve_panels <- function(curves, file, title,
                                  xlab = "Index", ylab = "Response scale",
                                  caption_note = NULL,
                                  show_legend_each_panel = FALSE) {
  oti_ensure_dir(dirname(file))
  targets <- c("RQR", "ET", "SH")
  targets <- targets[targets %in% unique(curves$target)]
  grDevices::png(file, width = 2100, height = 1450, res = 210)
  old <- graphics::par(no.readonly = TRUE)
  on.exit({
    graphics::par(old)
    grDevices::dev.off()
  }, add = TRUE)
  graphics::par(
    family = "serif",
    mfrow = c(length(targets), 1L),
    mar = c(2.0, 4.3, 2.0, 1.2),
    oma = c(3.3, 0.4, 2.4, 0)
  )
  col_truth <- "#222222"
  col_data <- "#6b6b6b"
  col_oracle <- "#0072B2"
  col_fit <- "#D55E00"
  col_missing <- "#CC79A7"
  col_ribbon <- grDevices::adjustcolor(col_fit, alpha.f = 0.18)
  for (panel_id in seq_along(targets)) {
    target <- targets[panel_id]
    z <- curves[curves$target == target, , drop = FALSE]
    z <- z[order(z$x), , drop = FALSE]
    yr <- range(
      z$y, z$mean_truth, z$oracle_lower, z$oracle_upper,
      z$fit_lower, z$fit_upper, z$fit_lower_q05, z$fit_lower_q95,
      z$fit_upper_q05, z$fit_upper_q95, na.rm = TRUE
    )
    pad <- diff(yr) * 0.06
    yr <- yr + c(-pad, pad)
    graphics::plot(
      z$x, z$y, type = "n",
      xlab = "", ylab = ylab, ylim = yr,
      main = paste0(target, " target"), las = 1,
      cex.lab = 0.96, cex.axis = 0.88, cex.main = 1.02
    )
    graphics::grid(col = "#e9e9e9", lwd = 0.8)
    if (all(is.finite(z$fit_lower_q05)) && all(is.finite(z$fit_lower_q95))) {
      graphics::polygon(
        c(z$x, rev(z$x)),
        c(z$fit_lower_q05, rev(z$fit_lower_q95)),
        col = col_ribbon, border = NA
      )
      graphics::polygon(
        c(z$x, rev(z$x)),
        c(z$fit_upper_q05, rev(z$fit_upper_q95)),
        col = col_ribbon, border = NA
      )
    }
    graphics::points(
      z$x[z$observed], z$y[z$observed],
      pch = 16, cex = 0.48, col = grDevices::adjustcolor(col_data, 0.85)
    )
    graphics::lines(z$x, z$mean_truth, col = col_truth, lwd = 2.1)
    graphics::lines(z$x, z$oracle_lower, col = col_oracle, lwd = 2.15)
    graphics::lines(z$x, z$oracle_upper, col = col_oracle, lwd = 2.15)
    graphics::lines(z$x, z$fit_lower, col = col_fit, lwd = 1.95)
    graphics::lines(z$x, z$fit_upper, col = col_fit, lwd = 1.95)
    if (any(!z$observed)) {
      graphics::points(z$x[!z$observed], z$mean_truth[!z$observed],
                       pch = 4, col = col_missing, cex = 0.82, lwd = 1.2)
    }
    show_legend <- isTRUE(show_legend_each_panel) || panel_id == 1L
    if (!show_legend) next
    legend <- c(
      "observed data", "truth", "population-oracle interval",
      "fit mean interval", "90% endpoint ribbon"
    )
    legend_col <- c(col_data, col_truth, col_oracle, col_fit, col_ribbon)
    legend_pch <- c(16, NA, NA, NA, 15)
    legend_lty <- c(NA, 1, 1, 1, NA)
    legend_lwd <- c(NA, 2.1, 2.15, 1.95, NA)
    if (any(!z$observed)) {
      legend <- c(legend, "missing time")
      legend_col <- c(legend_col, col_missing)
      legend_pch <- c(legend_pch, 4)
      legend_lty <- c(legend_lty, NA)
      legend_lwd <- c(legend_lwd, NA)
    }
    graphics::legend(
      "topleft", bty = "n", cex = 0.77,
      legend = legend, col = legend_col,
      pch = legend_pch, lty = legend_lty, lwd = legend_lwd
    )
  }
  graphics::mtext(xlab, side = 1, outer = TRUE, line = 1.4, cex = 0.96)
  graphics::mtext(title, outer = TRUE, cex = 1.12, font = 2)
  if (!is.null(caption_note) && nzchar(caption_note)) {
    graphics::mtext(caption_note, side = 1, outer = TRUE, line = 2.55,
                    cex = 0.72, col = "#444444")
  }
  invisible(file)
}

oti_plan_rows <- function(families, targets) {
  do.call(rbind, lapply(families, function(fam) {
    data.frame(
      family = fam,
      target = targets,
      learning_rate_mode = "fixed_rate",
      mean_tilt_source = "population_oracle",
      stringsAsFactors = FALSE
    )
  }))
}

oti_rbind_fill <- function(x) {
  x <- Filter(Negate(is.null), x)
  if (!length(x)) return(data.frame())
  cols <- unique(unlist(lapply(x, names), use.names = FALSE))
  rows <- lapply(x, function(z) {
    missing <- setdiff(cols, names(z))
    for (nm in missing) z[[nm]] <- NA
    z[, cols, drop = FALSE]
  })
  do.call(rbind, rows)
}

oti_artifact_manifest <- function(paths, root = getwd()) {
  paths <- unique(paths[file.exists(paths)])
  data.frame(
    path = paths,
    relative_path = sub(paste0("^", normalizePath(root), "/?"), "",
                        normalizePath(paths, mustWork = TRUE)),
    bytes = file.info(paths)$size,
    sha256 = vapply(paths, oti_file_sha256, character(1L)),
    stringsAsFactors = FALSE
  )
}
