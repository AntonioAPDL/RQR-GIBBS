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

oti_run_schema <- function() "rqrgibbs_oracle_tilt_illustration_run/1.1.0"

oti_package_state <- function(package) {
  available <- requireNamespace(package, quietly = TRUE)
  list(
    package = package,
    available = available,
    version = if (available) {
      as.character(utils::packageVersion(package))
    } else {
      NA_character_
    },
    namespace_path = if (available) {
      normalizePath(system.file(package = package), winslash = "/", mustWork = TRUE)
    } else {
      NA_character_
    }
  )
}

oti_file_hashes <- function(paths, root = ".") {
  paths <- unique(paths[file.exists(paths)])
  data.frame(
    relative_path = sub(
      paste0("^", normalizePath(root, winslash = "/", mustWork = TRUE), "/?"),
      "",
      normalizePath(paths, winslash = "/", mustWork = TRUE)
    ),
    sha256 = vapply(paths, oti_file_sha256, character(1L)),
    bytes = file.info(paths)$size,
    stringsAsFactors = FALSE
  )
}

oti_runtime_state <- function(repo_root, config_path, script_path, args,
                              run_control, extra_files = character(0)) {
  ext <- extSoftVersion()
  source_files <- unique(c(
    config_path,
    script_path,
    file.path(dirname(script_path), "32_oracle_tilt_illustration_utils.R"),
    file.path(repo_root, "application", "tests", "testthat",
              "test-rqr-oracle-tilt-illustrations.R"),
    file.path(repo_root, "Makefile"),
    file.path(repo_root, "main.tex"),
    file.path(repo_root, "rqr-gibbs-supplement.tex"),
    extra_files
  ))
  list(
    schema_version = oti_run_schema(),
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    command_args = as.character(args),
    repo_state = oti_git_state(repo_root),
    run_control = run_control,
    config_path = normalizePath(config_path, winslash = "/", mustWork = TRUE),
    script_path = normalizePath(script_path, winslash = "/", mustWork = TRUE),
    file_hashes = oti_file_hashes(source_files, root = repo_root),
    packages = list(
      rqrgibbs = oti_package_state("rqrgibbs"),
      jsonlite = oti_package_state("jsonlite"),
      digest = oti_package_state("digest"),
      posterior = oti_package_state("posterior"),
      testthat = oti_package_state("testthat")
    ),
    R = list(
      version = R.version.string,
      platform = R.version$platform,
      blas = if ("BLAS" %in% names(ext)) ext[["BLAS"]] else NA_character_,
      lapack = if ("LAPACK" %in% names(ext)) ext[["LAPACK"]] else NA_character_
    ),
    interpretation = paste(
      "Oracle-tilt figures summarize interval-root generalized posteriors;",
      "they are not response-predictive simulations."
    )
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
  opt <- stats::optimize(
    objective,
    interval = c(lo, hi),
    tol = max(.Machine$double.eps^0.75, upper_u * 1e-12)
  )$minimum
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
  out$oracle_construction <- "population_quantile_truncated_moment"
  out$uses_cornish_fisher <- FALSE
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
  initial_level_variance <- oti_scalar(
    cfg$initial_level_variance %||% 4,
    "dlm$initial_level_variance",
    .Machine$double.eps
  )
  initial_slope_variance <- oti_scalar(
    cfg$initial_slope_variance %||% 1,
    "dlm$initial_slope_variance",
    .Machine$double.eps
  )
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
    C0 = diag(c(initial_level_variance, initial_slope_variance)),
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
    initial_level_variance = initial_level_variance,
    initial_slope_variance = initial_slope_variance,
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

oti_run_control <- function(config, quick = FALSE, paper_figures = FALSE,
                            one_chain = FALSE) {
  paper <- config$paper_mcmc_control %||% list()
  paper_enabled <- isTRUE(paper$enabled %||% TRUE)
  paper_mode <- isTRUE(paper_figures) && !isTRUE(quick) &&
    !isTRUE(one_chain) && paper_enabled
  n_chains <- if (paper_mode) {
    oti_integer(paper$n_chains %||% 4L, "paper_mcmc_control$n_chains", 1L)
  } else {
    1L
  }
  diagnostics <- paper$diagnostics %||% list()
  list(
    schema_version = oti_run_schema(),
    quick = isTRUE(quick),
    paper_figures = isTRUE(paper_figures),
    one_chain_override = isTRUE(one_chain),
    paper_mode = paper_mode,
    n_chains = n_chains,
    diagnostics = list(
      enabled = isTRUE(diagnostics$enabled %||% TRUE) && n_chains >= 2L,
      provider = "posterior",
      rhat_max = as.numeric(diagnostics$rhat_max %||% 1.05)[1L],
      bulk_ess_min = as.numeric(diagnostics$bulk_ess_min %||% 400)[1L],
      tail_ess_min = as.numeric(diagnostics$tail_ess_min %||% 200)[1L],
      mcse_over_sd_max = as.numeric(diagnostics$mcse_over_sd_max %||% 0.10)[1L]
    )
  )
}

oti_fit_control_fields <- function(x) {
  x <- x %||% list()
  x$enabled <- NULL
  x$n_chains <- NULL
  x$diagnostics <- NULL
  x
}

oti_mcmc_control <- function(config, family, quick = FALSE, seed = NULL,
                             paper_mode = FALSE) {
  family_cfg <- config[[family]] %||% list()
  base <- config$mcmc_control %||% list()
  fam <- family_cfg$mcmc_control %||% list()
  paper <- if (isTRUE(paper_mode)) {
    oti_fit_control_fields(config$paper_mcmc_control %||% list())
  } else {
    list()
  }
  family_paper <- if (isTRUE(paper_mode)) {
    oti_fit_control_fields(family_cfg$paper_mcmc_control %||% list())
  } else {
    list()
  }
  quick_ctl <- if (isTRUE(quick)) {
    config$quick_mcmc_control %||% list(n_burn = 30L, n_mcmc = 60L, thin = 1L)
  } else {
    list()
  }
  out <- oti_merge_control(
    oti_merge_control(
      oti_merge_control(oti_merge_control(base, fam), paper),
      family_paper
    ),
    quick_ctl
  )
  out$seed <- seed %||% out$seed %||% 1L
  out$verbose <- isTRUE(out$verbose %||% FALSE)
  out
}

oti_chain_seed <- function(config, family, target, chain) {
  target_index <- match(target, c("RQR", "ET", "SH"))
  if (is.na(target_index)) oti_stop("Unknown target for seed rule: ", target)
  default_seed <- switch(
    family,
    fixed_design = 202607281L,
    dlm = 202607282L,
    desn = 202607283L,
    1L
  )
  base <- oti_integer((config[[family]] %||% list())$seed %||% default_seed,
                      paste0(family, "$seed"), 0L)
  chain <- oti_integer(chain, "chain", 1L)
  base + target_index * 100L + (chain - 1L) * 10000L
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
  lower_q <- oti_row_quantile(pred$lower_draws, c(0.025, 0.975))
  upper_q <- oti_row_quantile(pred$upper_draws, c(0.025, 0.975))
  midpoint_q <- oti_row_quantile(pred$midpoint_draws, c(0.025, 0.975))
  width_q <- oti_row_quantile(pred$width_draws, c(0.025, 0.975))
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
    fit_lower_q025 = lower_q[, 1L],
    fit_lower_q975 = lower_q[, 2L],
    fit_upper_q025 = upper_q[, 1L],
    fit_upper_q975 = upper_q[, 2L],
    fit_midpoint_q025 = midpoint_q[, 1L],
    fit_midpoint_q975 = midpoint_q[, 2L],
    fit_width_q025 = width_q[, 1L],
    fit_width_q975 = width_q[, 2L],
    observed = is.finite(y),
    stringsAsFactors = FALSE
  )
}

oti_endpoint_error_vectors <- function(pred, truth) {
  lower <- sweep(as.matrix(pred$lower_draws), 1L, truth$oracle_lower, "-")
  upper <- sweep(as.matrix(pred$upper_draws), 1L, truth$oracle_upper, "-")
  list(
    lower = as.numeric(lower[is.finite(lower)]),
    upper = as.numeric(upper[is.finite(upper)])
  )
}

oti_endpoint_error_density_frame <- function(family, target, pred, truth,
                                             n_grid = 512L) {
  errs <- oti_endpoint_error_vectors(pred, truth)
  rows <- lapply(names(errs), function(endpoint) {
    e <- errs[[endpoint]]
    if (!length(e)) {
      return(data.frame())
    }
    den <- stats::density(e, n = n_grid, na.rm = TRUE)
    qs <- stats::quantile(e, c(0.025, 0.5, 0.975),
                          names = FALSE, type = 8)
    data.frame(
      family = family,
      target = target,
      endpoint = endpoint,
      error = den$x,
      density = den$y,
      q025 = qs[1L],
      median = qs[2L],
      q975 = qs[3L],
      mean_error = mean(e),
      rmse = sqrt(mean(e^2)),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

oti_endpoint_error_summary_frame <- function(family, target, pred, truth) {
  errs <- oti_endpoint_error_vectors(pred, truth)
  rows <- lapply(names(errs), function(endpoint) {
    e <- errs[[endpoint]]
    qs <- stats::quantile(e, c(0.025, 0.25, 0.5, 0.75, 0.975),
                          names = FALSE, type = 8)
    data.frame(
      family = family,
      target = target,
      endpoint = endpoint,
      mean_error = mean(e),
      median_error = qs[3L],
      q025_error = qs[1L],
      q25_error = qs[2L],
      q75_error = qs[4L],
      q975_error = qs[5L],
      rmse = sqrt(mean(e^2)),
      mean_abs_error = mean(abs(e)),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

oti_endpoint_error_by_index_frame <- function(family, target, pred, truth) {
  build <- function(endpoint, draws, oracle) {
    err <- sweep(as.matrix(draws), 1L, oracle, "-")
    qs <- oti_row_quantile(err, c(0.025, 0.5, 0.975))
    data.frame(
      family = family,
      target = target,
      endpoint = endpoint,
      index = truth$index,
      observed = truth$observed,
      mean_error = rowMeans(err),
      median_error = qs[, 2L],
      q025_error = qs[, 1L],
      q975_error = qs[, 3L],
      rmse = sqrt(rowMeans(err^2)),
      stringsAsFactors = FALSE
    )
  }
  rbind(
    build("lower", pred$lower_draws, truth$oracle_lower),
    build("upper", pred$upper_draws, truth$oracle_upper)
  )
}

oti_combine_predictions <- function(predictions) {
  mats <- c("lower_draws", "upper_draws", "midpoint_draws", "width_draws")
  out <- lapply(mats, function(nm) do.call(cbind, lapply(predictions, `[[`, nm)))
  names(out) <- mats
  out$lower_mean <- rowMeans(out$lower_draws)
  out$upper_mean <- rowMeans(out$upper_draws)
  out$midpoint_mean <- rowMeans(out$midpoint_draws)
  out$width_mean <- rowMeans(out$width_draws)
  out
}

oti_diagnostic_indices <- function(family, truth) {
  observed <- which(as.logical(truth$observed))
  if (!length(observed)) return(integer(0))
  missing <- which(!as.logical(truth$observed))
  candidates <- c(observed[1L], observed[ceiling(length(observed) / 2)], observed[length(observed)])
  if (length(missing)) {
    before <- observed[observed < min(missing)]
    after <- observed[observed > max(missing)]
    candidates <- c(
      candidates,
      if (length(before)) before[length(before)] else integer(0),
      if (length(after)) after[1L] else integer(0)
    )
  }
  unique(candidates)
}

oti_scalar_draw_matrix <- function(family, pred, truth, y, coverage_level) {
  lower <- as.matrix(pred$lower_draws)
  upper <- as.matrix(pred$upper_draws)
  observed <- as.logical(truth$observed) & is.finite(y)
  values <- data.frame(
    mean_lower_observed = colMeans(lower[observed, , drop = FALSE]),
    mean_upper_observed = colMeans(upper[observed, , drop = FALSE]),
    mean_width_observed = colMeans((upper - lower)[observed, , drop = FALSE]),
    mean_midpoint_observed = colMeans(
      (0.5 * (lower + upper))[observed, , drop = FALSE]
    ),
    stringsAsFactors = FALSE
  )
  values$observed_mean_tilted_loss <- vapply(
    seq_len(ncol(lower)),
    function(j) {
      sum(rqrgibbs::rqr_mean_tilt_loss(
        y, lower[, j], upper[, j], coverage_level,
        mean_tilt = truth$mean_tilt
      ))
    },
    numeric(1L)
  )
  idx <- oti_diagnostic_indices(family, truth)
  for (ii in idx) {
    prefix <- if (identical(family, "fixed_design")) {
      paste0("index_", sprintf("%03d", ii))
    } else {
      paste0("time_", sprintf("%03d", ii))
    }
    values[[paste0("lower_", prefix)]] <- lower[ii, ]
    values[[paste0("upper_", prefix)]] <- upper[ii, ]
    values[[paste0("width_", prefix)]] <- upper[ii, ] - lower[ii, ]
  }
  as.matrix(values)
}

oti_mcmc_diagnostics <- function(family, target, scalar_chains, run_control) {
  if (!isTRUE(run_control$diagnostics$enabled) || length(scalar_chains) < 2L) {
    return(data.frame())
  }
  if (!requireNamespace("posterior", quietly = TRUE)) {
    variables <- colnames(scalar_chains[[1L]])
    return(data.frame(
      provider = "posterior_unavailable",
      family = family,
      target = target,
      estimand = variables,
      n_chains = length(scalar_chains),
      n_draws_per_chain = nrow(scalar_chains[[1L]]),
      rhat = NA_real_,
      ess_bulk = NA_real_,
      ess_tail = NA_real_,
      mcse_mean = NA_real_,
      sd = NA_real_,
      mcse_over_sd = NA_real_,
      pass = FALSE,
      stringsAsFactors = FALSE
    ))
  }
  variables <- colnames(scalar_chains[[1L]])
  if (!all(vapply(scalar_chains, function(x) identical(colnames(x), variables), logical(1L)))) {
    oti_stop("Scalar diagnostic chains do not share an identical schema.")
  }
  n_iter <- nrow(scalar_chains[[1L]])
  if (!all(vapply(scalar_chains, nrow, integer(1L)) == n_iter)) {
    oti_stop("Scalar diagnostic chains must have the same retained length.")
  }
  arr <- array(
    NA_real_,
    dim = c(n_iter, length(scalar_chains), length(variables)),
    dimnames = list(NULL, NULL, variables)
  )
  for (j in seq_along(scalar_chains)) arr[, j, ] <- scalar_chains[[j]]
  draws <- posterior::as_draws_array(arr)
  flat <- do.call(rbind, scalar_chains)
  sd_values <- apply(flat, 2L, stats::sd)
  diagnostics <- posterior::summarise_draws(
    draws, "rhat", "ess_bulk", "ess_tail", "mcse_mean"
  )
  diagnostics <- diagnostics[match(variables, diagnostics$variable), ]
  if (any(is.na(diagnostics$variable))) {
    oti_stop("posterior diagnostics did not return all requested variables.")
  }
  out <- data.frame(
    provider = "posterior",
    family = family,
    target = target,
    estimand = variables,
    n_chains = length(scalar_chains),
    n_draws_per_chain = n_iter,
    rhat = unname(diagnostics$rhat),
    ess_bulk = unname(diagnostics$ess_bulk),
    ess_tail = unname(diagnostics$ess_tail),
    mcse_mean = unname(diagnostics$mcse_mean),
    sd = unname(sd_values),
    mcse_over_sd = ifelse(
      sd_values > 0,
      unname(diagnostics$mcse_mean) / sd_values,
      NA_real_
    ),
    stringsAsFactors = FALSE
  )
  out$pass <- with(
    out,
    is.finite(rhat) &
      rhat <= run_control$diagnostics$rhat_max &
      is.finite(ess_bulk) &
      ess_bulk >= run_control$diagnostics$bulk_ess_min &
      is.finite(ess_tail) &
      ess_tail >= run_control$diagnostics$tail_ess_min &
      (is.na(mcse_over_sd) | mcse_over_sd <= run_control$diagnostics$mcse_over_sd_max)
  )
  out
}

oti_chain_summary <- function(family, target, chain, seed, fit, elapsed) {
  n_draws <- if (inherits(fit, "rqr_dlm_mcmc")) {
    ncol(fit$samp.eta_root1)
  } else if (inherits(fit, "rqr_desn_fit")) {
    nrow(fit$fit$samp.beta_root1)
  } else {
    nrow(fit$samp.beta_root1)
  }
  repairs <- if (inherits(fit, "rqr_desn_fit")) {
    fit$fit$model_spec$numerical_repair_count
  } else {
    fit$model_spec$numerical_repair_count
  }
  root_swap <- if (inherits(fit, "rqr_desn_fit")) {
    fit$fit$diagnostics$root_swap_trace %||% logical(0)
  } else {
    fit$diagnostics$root_swap_trace %||% logical(0)
  }
  data.frame(
    family = family,
    target = target,
    chain = chain,
    seed = seed,
    n_draws = n_draws,
    numerical_repair_count = repairs %||% NA_integer_,
    elapsed_seconds = as.numeric(elapsed[["elapsed"]]),
    root_swap_fraction = if (length(root_swap)) mean(root_swap) else NA_real_,
    stringsAsFactors = FALSE
  )
}

oti_fit_fixed_design_target_chain <- function(dgp, targets_by_index, target,
                                              config, quick = FALSE,
                                              chain = 1L,
                                              run_control = oti_run_control(config, quick)) {
  ctl <- oti_mcmc_control(
    config, "fixed_design", quick = quick,
    seed = oti_chain_seed(config, "fixed_design", target, chain),
    paper_mode = isTRUE(run_control$paper_mode)
  )
  tau2 <- as.numeric((config$fixed_design %||% list())$ridge_tau2 %||% 250)[1L]
  truth <- oti_target_row(targets_by_index, target)
  elapsed <- system.time(fit <- rqrgibbs::rqr_mcmc_fit(
    y = dgp$y,
    X = dgp$X,
    coverage_level = config$coverage_level %||% 0.8,
    learning_rate = config$learning_rate %||% 1,
    learning_rate_mode = "fixed_rate",
    mean_tilt = truth$mean_tilt,
    beta_prior_obj = oti_ridge_prior(tau2),
    numerical_policy = "fail",
    mcmc_control = ctl
  ))
  pred <- rqrgibbs::predict_interval(fit, X_new = dgp$X)
  list(
    fit = fit,
    pred = pred,
    scalar_draws = oti_scalar_draw_matrix(
      "fixed_design", pred, truth, dgp$y, config$coverage_level %||% 0.8
    ),
    chain_summary = oti_chain_summary(
      "fixed_design", target, chain, ctl$seed, fit, elapsed
    )
  )
}

oti_fit_fixed_design_target <- function(dgp, targets_by_index, target,
                                        config, quick = FALSE,
                                        run_control = oti_run_control(config, quick)) {
  truth <- oti_target_row(targets_by_index, target)
  chains <- lapply(seq_len(run_control$n_chains), function(chain) {
    oti_fit_fixed_design_target_chain(
      dgp, targets_by_index, target, config, quick = quick,
      chain = chain, run_control = run_control
    )
  })
  pred <- oti_combine_predictions(lapply(chains, `[[`, "pred"))
  metrics <- oti_interval_metrics(pred, truth, dgp$y)
  diagnostics <- oti_mcmc_diagnostics(
    "fixed_design", target, lapply(chains, `[[`, "scalar_draws"), run_control
  )
  repair_count <- sum(vapply(
    chains, function(x) x$chain_summary$numerical_repair_count, numeric(1L)
  ))
  figure_quality_pass <- repair_count == 0L &&
    (!nrow(diagnostics) || all(diagnostics$pass))
  summary <- cbind(
    data.frame(
      family = "fixed_design",
      target = target,
      fit_status = "ok",
      mean_tilt_mode = chains[[1L]]$fit$model_spec$mean_tilt_mode,
      loss_name = chains[[1L]]$fit$model_spec$loss_name,
      n_chains = run_control$n_chains,
      n_draws = ncol(pred$lower_draws),
      numerical_repair_count = repair_count,
      diagnostics_provider = if (nrow(diagnostics)) diagnostics$provider[1L] else "not_run",
      figure_quality_pass = figure_quality_pass,
      stringsAsFactors = FALSE
    ),
    metrics
  )
  list(
    fit = chains[[1L]]$fit,
    summary = summary,
    curves = oti_curve_frame("fixed_design", target, dgp$x, dgp$y, pred, truth),
    error_density = oti_endpoint_error_density_frame(
      "fixed_design", target, pred, truth
    ),
    error_summary = oti_endpoint_error_summary_frame(
      "fixed_design", target, pred, truth
    ),
    endpoint_error_by_index = oti_endpoint_error_by_index_frame(
      "fixed_design", target, pred, truth
    ),
    chain_summary = do.call(rbind, lapply(chains, `[[`, "chain_summary")),
    mcmc_diagnostics = diagnostics
  )
}

oti_fit_dlm_target_chain <- function(dgp, targets_by_index, target, config,
                                     quick = FALSE, chain = 1L,
                                     run_control = oti_run_control(config, quick)) {
  ctl <- oti_mcmc_control(
    config, "dlm", quick = quick,
    seed = oti_chain_seed(config, "dlm", target, chain),
    paper_mode = isTRUE(run_control$paper_mode)
  )
  ctl$backend <- ctl$backend %||% "cpp"
  ctl$store_state_draws <- isTRUE(ctl$store_state_draws %||% FALSE)
  truth <- oti_target_row(targets_by_index, target)
  elapsed <- system.time(fit <- rqrgibbs::rqr_dlm_fit(
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
  ))
  pred <- rqrgibbs::predict_interval(fit)
  list(
    fit = fit,
    pred = pred,
    scalar_draws = oti_scalar_draw_matrix(
      "dlm", pred, truth, dgp$y, config$coverage_level %||% 0.8
    ),
    chain_summary = oti_chain_summary("dlm", target, chain, ctl$seed, fit, elapsed)
  )
}

oti_fit_dlm_target <- function(dgp, targets_by_index, target, config,
                               quick = FALSE,
                               run_control = oti_run_control(config, quick)) {
  truth <- oti_target_row(targets_by_index, target)
  chains <- lapply(seq_len(run_control$n_chains), function(chain) {
    oti_fit_dlm_target_chain(
      dgp, targets_by_index, target, config, quick = quick,
      chain = chain, run_control = run_control
    )
  })
  pred <- oti_combine_predictions(lapply(chains, `[[`, "pred"))
  metrics <- oti_interval_metrics(pred, truth, dgp$y)
  diagnostics <- oti_mcmc_diagnostics(
    "dlm", target, lapply(chains, `[[`, "scalar_draws"), run_control
  )
  repair_count <- sum(vapply(
    chains, function(x) x$chain_summary$numerical_repair_count, numeric(1L)
  ))
  figure_quality_pass <- repair_count == 0L &&
    (!nrow(diagnostics) || all(diagnostics$pass))
  summary <- cbind(
    data.frame(
      family = "dlm",
      target = target,
      fit_status = "ok",
      mean_tilt_mode = chains[[1L]]$fit$model_spec$mean_tilt_mode,
      loss_name = chains[[1L]]$fit$model_spec$loss_name,
      evolution_mode = chains[[1L]]$fit$model_spec$evolution_mode,
      n_chains = run_control$n_chains,
      n_draws = ncol(pred$lower_draws),
      numerical_repair_count = repair_count,
      diagnostics_provider = if (nrow(diagnostics)) diagnostics$provider[1L] else "not_run",
      figure_quality_pass = figure_quality_pass,
      stringsAsFactors = FALSE
    ),
    metrics
  )
  list(
    fit = chains[[1L]]$fit,
    summary = summary,
    curves = oti_curve_frame("dlm", target, dgp$time, dgp$y, pred, truth),
    error_density = oti_endpoint_error_density_frame("dlm", target, pred, truth),
    error_summary = oti_endpoint_error_summary_frame("dlm", target, pred, truth),
    endpoint_error_by_index = oti_endpoint_error_by_index_frame(
      "dlm", target, pred, truth
    ),
    chain_summary = do.call(rbind, lapply(chains, `[[`, "chain_summary")),
    mcmc_diagnostics = diagnostics
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
    ),
    error_density = oti_endpoint_error_density_frame(
      "desn", target, pred, truth
    ),
    error_summary = oti_endpoint_error_summary_frame(
      "desn", target, pred, truth
    )
  )
}

oti_open_figure_device <- function(file, width, height, res = 210) {
  extension <- tolower(tools::file_ext(file))
  if (identical(extension, "pdf")) {
    grDevices::pdf(
      file, width = width / res, height = height / res,
      family = "serif", useDingbats = FALSE
    )
  } else if (identical(extension, "png")) {
    grDevices::png(file, width = width, height = height, res = res)
  } else {
    oti_stop("Figure output must use a .pdf or .png extension.")
  }
  invisible(file)
}

oti_plot_curve_panels <- function(curves, file, title,
                                  xlab = "Index", ylab = "Response scale",
                                  caption_note = NULL,
                                  show_legend_each_panel = FALSE) {
  oti_ensure_dir(dirname(file))
  targets <- c("RQR", "ET", "SH")
  targets <- targets[targets %in% unique(curves$target)]
  oti_open_figure_device(file, width = 2100, height = 1450, res = 210)
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
      z$y, z$oracle_lower, z$oracle_upper,
      z$fit_lower, z$fit_upper, z$fit_lower_q025, z$fit_lower_q975,
      z$fit_upper_q025, z$fit_upper_q975, na.rm = TRUE
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
    miss_x <- z$x[!z$observed]
    if (length(miss_x)) {
      ux <- sort(unique(z$x))
      dx <- if (length(ux) > 1L) min(diff(ux)) else diff(range(z$x)) * 0.01
      if (!is.finite(dx) || dx <= 0) dx <- 0.50
      graphics::rect(
        miss_x - 0.36 * dx, yr[1L],
        miss_x + 0.36 * dx, yr[2L],
        col = grDevices::adjustcolor(col_missing, alpha.f = 0.10),
        border = NA
      )
      graphics::abline(
        v = miss_x, col = grDevices::adjustcolor(col_missing, alpha.f = 0.65),
        lty = 2, lwd = 1.15
      )
    }
    if (all(is.finite(z$fit_lower_q025)) && all(is.finite(z$fit_lower_q975))) {
      graphics::polygon(
        c(z$x, rev(z$x)),
        c(z$fit_lower_q025, rev(z$fit_lower_q975)),
        col = col_ribbon, border = NA
      )
      graphics::polygon(
        c(z$x, rev(z$x)),
        c(z$fit_upper_q025, rev(z$fit_upper_q975)),
        col = col_ribbon, border = NA
      )
    }
    graphics::points(
      z$x[z$observed], z$y[z$observed],
      pch = 16, cex = 0.48, col = grDevices::adjustcolor(col_data, 0.85)
    )
    graphics::lines(z$x, z$oracle_lower, col = col_oracle, lwd = 2.15)
    graphics::lines(z$x, z$oracle_upper, col = col_oracle, lwd = 2.15)
    graphics::lines(z$x, z$fit_lower, col = col_fit, lwd = 1.95)
    graphics::lines(z$x, z$fit_upper, col = col_fit, lwd = 1.95)
    if (length(miss_x)) {
      graphics::points(
        miss_x, rep(yr[1L] + 0.035 * diff(yr), length(miss_x)),
        pch = 24, cex = 0.78, col = col_missing, bg = "white", lwd = 1.2
      )
    }
    show_legend <- isTRUE(show_legend_each_panel) || panel_id == 1L
    if (!show_legend) next
    legend <- c(
      "observed data", "population-oracle interval",
      "fitted endpoint mean", "95% endpoint summary"
    )
    legend_col <- c(col_data, col_oracle, col_fit, col_ribbon)
    legend_pch <- c(16, NA, NA, 15)
    legend_lty <- c(NA, 1, 1, NA)
    legend_lwd <- c(NA, 2.15, 1.95, NA)
    if (length(miss_x)) {
      legend <- c(legend, "omitted response time")
      legend_col <- c(legend_col, col_missing)
      legend_pch <- c(legend_pch, 24)
      legend_lty <- c(legend_lty, 2)
      legend_lwd <- c(legend_lwd, 1.15)
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

oti_plot_endpoint_error_panels <- function(error_density, file, title,
                                           xlab = "Endpoint error") {
  oti_ensure_dir(dirname(file))
  targets <- c("RQR", "ET", "SH")
  targets <- targets[targets %in% unique(error_density$target)]
  oti_open_figure_device(file, width = 2100, height = 1300, res = 210)
  old <- graphics::par(no.readonly = TRUE)
  on.exit({
    graphics::par(old)
    grDevices::dev.off()
  }, add = TRUE)
  graphics::par(
    family = "serif",
    mfrow = c(length(targets), 1L),
    mar = c(2.6, 4.3, 2.6, 1.2),
    oma = c(3.2, 0.4, 2.7, 0)
  )
  col_lower <- "#0072B2"
  col_upper <- "#D55E00"
  for (panel_id in seq_along(targets)) {
    target <- targets[panel_id]
    z <- error_density[error_density$target == target, , drop = FALSE]
    z_lower <- z[z$endpoint == "lower", , drop = FALSE]
    z_upper <- z[z$endpoint == "upper", , drop = FALSE]
    xr <- range(z$error, z$q025, z$q975, 0, na.rm = TRUE)
    yr <- range(z$density, na.rm = TRUE)
    xr <- xr + c(-0.06, 0.06) * diff(xr)
    yr <- c(0, yr[2L] * 1.10)
    graphics::plot(
      z_lower$error, z_lower$density, type = "n",
      xlim = xr, ylim = yr, xlab = "", ylab = "Density",
      main = paste0(target, " target"), las = 1,
      cex.lab = 0.96, cex.axis = 0.88, cex.main = 1.02
    )
    graphics::grid(col = "#e9e9e9", lwd = 0.8)
    graphics::abline(v = 0, col = "#222222", lwd = 1.25)
    graphics::lines(z_lower$error, z_lower$density, col = col_lower, lwd = 2.1)
    graphics::lines(z_upper$error, z_upper$density, col = col_upper, lwd = 2.1)
    q_lower <- z_lower[1L, c("q025", "median", "q975")]
    q_upper <- z_upper[1L, c("q025", "median", "q975")]
    y_lower <- yr[2L] * 0.075
    y_upper <- yr[2L] * 0.135
    graphics::segments(q_lower$q025, y_lower, q_lower$q975, y_lower,
                       col = col_lower, lwd = 3)
    graphics::points(q_lower$median, y_lower, pch = 16, col = col_lower)
    graphics::segments(q_upper$q025, y_upper, q_upper$q975, y_upper,
                       col = col_upper, lwd = 3)
    graphics::points(q_upper$median, y_upper, pch = 16, col = col_upper)
    if (panel_id == 1L) {
      graphics::legend(
        "topright", bty = "n", cex = 0.78,
        legend = c("lower endpoint error", "upper endpoint error",
                   "zero error", "central 95% endpoint-error summary"),
        col = c(col_lower, col_upper, "#222222", "#555555"),
        lty = c(1, 1, 1, 1), lwd = c(2.1, 2.1, 1.25, 3)
      )
    }
  }
  graphics::mtext(xlab, side = 1, outer = TRUE, line = 1.35, cex = 0.96)
  graphics::mtext(title, outer = TRUE, cex = 1.12, font = 2)
  invisible(file)
}

oti_plan_rows <- function(families, targets, run_control = NULL) {
  run_control <- run_control %||% list()
  diagnostics <- run_control$diagnostics %||% list()
  do.call(rbind, lapply(families, function(fam) {
    data.frame(
      family = fam,
      target = targets,
      learning_rate_mode = "fixed_rate",
      mean_tilt_source = "population_oracle",
      n_chains = run_control$n_chains %||% 1L,
      paper_mode = isTRUE(run_control$paper_mode),
      diagnostics_enabled = isTRUE(diagnostics$enabled),
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
