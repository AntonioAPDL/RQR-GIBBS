#!/usr/bin/env Rscript

# Deterministic and Monte Carlo validation for Cornish--Fisher fixed
# mean-tilt initialization.  This script is intentionally outside the sampler:
# it computes population targets, calls the production initializer functions,
# and writes compact validation artifacts to an explicit output directory.

stopf <- function(fmt, ...) stop(sprintf(fmt, ...), call. = FALSE)

`%||%` <- function(a, b) if (is.null(a)) b else a

load_mean_tilt_initializers <- function(repo_root = ".") {
  exports <- c(
    "rqr_mt_cf_constant",
    "rqr_mt_tilt_cf",
    "rqr_mt_tilt_empirical_shortest",
    "rqr_mt_tilt_empirical_equal_tailed",
    "rqr_mt_tilt_screen",
    "rqr_mt_select_tilt_candidate"
  )
  if (requireNamespace("rqrgibbs", quietly = TRUE) &&
      all(vapply(exports, exists, logical(1L),
                 envir = asNamespace("rqrgibbs"), inherits = FALSE))) {
    env <- asNamespace("rqrgibbs")
  } else {
    env <- new.env(parent = baseenv())
    sys.source(
      file.path(repo_root, "application", "R", "rqr_mean_tilt_init.R"),
      envir = env
    )
  }
  setNames(lapply(exports, get, envir = env, inherits = FALSE), exports)
}

assert_coverage <- function(coverage) {
  coverage <- as.numeric(coverage)
  if (length(coverage) != 1L || is.na(coverage) ||
      !is.finite(coverage) || coverage <= 0 || coverage >= 1) {
    stop("coverage must be one finite scalar in (0, 1).", call. = FALSE)
  }
  coverage
}

beta_skewness <- function(a, b) {
  2 * (b - a) * sqrt(a + b + 1) / ((a + b + 2) * sqrt(a * b))
}

make_dgp <- function(id, label, q, p, d, r, mean, sd, skewness,
                     support) {
  list(
    id = id, label = label, q = q, p = p, d = d, r = r,
    mean = mean, sd = sd, skewness = skewness, support = support
  )
}

dgp_registry <- function() {
  list(
    make_dgp(
      "normal", "Normal(0,1)",
      stats::qnorm, stats::pnorm, stats::dnorm, stats::rnorm,
      mean = 0, sd = 1, skewness = 0, support = c(-Inf, Inf)
    ),
    make_dgp(
      "gamma16", "Gamma(shape=16, scale=0.25)",
      function(p) stats::qgamma(p, shape = 16, scale = 0.25),
      function(x) stats::pgamma(x, shape = 16, scale = 0.25),
      function(x) stats::dgamma(x, shape = 16, scale = 0.25),
      function(n) stats::rgamma(n, shape = 16, scale = 0.25),
      mean = 4, sd = 1, skewness = 0.5, support = c(0, Inf)
    ),
    make_dgp(
      "gamma4", "Gamma(shape=4, scale=1)",
      function(p) stats::qgamma(p, shape = 4, scale = 1),
      function(x) stats::pgamma(x, shape = 4, scale = 1),
      function(x) stats::dgamma(x, shape = 4, scale = 1),
      function(n) stats::rgamma(n, shape = 4, scale = 1),
      mean = 4, sd = 2, skewness = 1, support = c(0, Inf)
    ),
    make_dgp(
      "lognormal", "Lognormal(logmean=0, logsd=0.5)",
      function(p) stats::qlnorm(p, meanlog = 0, sdlog = 0.5),
      function(x) stats::plnorm(x, meanlog = 0, sdlog = 0.5),
      function(x) stats::dlnorm(x, meanlog = 0, sdlog = 0.5),
      function(n) stats::rlnorm(n, meanlog = 0, sdlog = 0.5),
      mean = exp(0.5^2 / 2),
      sd = sqrt((exp(0.5^2) - 1) * exp(0.5^2)),
      skewness = (exp(0.5^2) + 2) * sqrt(exp(0.5^2) - 1),
      support = c(0, Inf)
    ),
    make_dgp(
      "exponential", "Exponential(rate=1)",
      function(p) stats::qexp(p, rate = 1),
      function(x) stats::pexp(x, rate = 1),
      function(x) stats::dexp(x, rate = 1),
      function(n) stats::rexp(n, rate = 1),
      mean = 1, sd = 1, skewness = 2, support = c(0, Inf)
    ),
    make_dgp(
      "beta_right", "Beta(2,5)",
      function(p) stats::qbeta(p, 2, 5),
      function(x) stats::pbeta(x, 2, 5),
      function(x) stats::dbeta(x, 2, 5),
      function(n) stats::rbeta(n, 2, 5),
      mean = 2 / 7,
      sd = sqrt(2 * 5 / (7^2 * 8)),
      skewness = beta_skewness(2, 5),
      support = c(0, 1)
    ),
    make_dgp(
      "beta_left", "Beta(5,2)",
      function(p) stats::qbeta(p, 5, 2),
      function(x) stats::pbeta(x, 5, 2),
      function(x) stats::dbeta(x, 5, 2),
      function(n) stats::rbeta(n, 5, 2),
      mean = 5 / 7,
      sd = sqrt(5 * 2 / (7^2 * 8)),
      skewness = beta_skewness(5, 2),
      support = c(0, 1)
    )
  )
}

retained_mean <- function(dgp, lower, upper) {
  value <- stats::integrate(
    f = function(y) y * dgp$d(y),
    lower = lower, upper = upper,
    rel.tol = 1e-10, subdivisions = 1000L, stop.on.error = TRUE
  )$value
  as.numeric(value)
}

interval_from_u <- function(dgp, u, coverage) {
  lower <- dgp$q(u)
  upper <- dgp$q(u + coverage)
  list(lower = lower, upper = upper, width = upper - lower)
}

oracle_shortest_interval <- function(dgp, coverage) {
  upper_u <- 1 - coverage
  width_at <- function(u) interval_from_u(dgp, u, coverage)$width
  candidates <- c(0, upper_u)
  if (upper_u > 0) {
    interior <- stats::optimize(width_at, c(0, upper_u), tol = 1e-15)
    candidates <- c(candidates, interior$minimum)
  }
  widths <- vapply(candidates, width_at, numeric(1L))
  u <- candidates[[which.min(widths)]]
  interval <- interval_from_u(dgp, u, coverage)
  retained_first <- retained_mean(dgp, interval$lower, interval$upper)
  retained <- retained_first / coverage
  content <- dgp$p(interval$upper) - dgp$p(interval$lower)
  end_tol <- 1e-8
  boundary <- if (u <= end_tol) {
    "lower"
  } else if ((upper_u - u) <= end_tol) {
    "upper"
  } else {
    "interior"
  }
  list(
    u = u, boundary = boundary,
    lower = interval$lower, upper = interval$upper,
    width = interval$width, retained_mean = retained,
    delta_raw = retained - dgp$mean,
    delta_standardized = (retained - dgp$mean) / dgp$sd,
    content = content
  )
}

oracle_equal_tailed_interval <- function(dgp, coverage) {
  u <- (1 - coverage) / 2
  interval <- interval_from_u(dgp, u, coverage)
  retained_first <- retained_mean(dgp, interval$lower, interval$upper)
  retained <- retained_first / coverage
  list(
    u = u, lower = interval$lower, upper = interval$upper,
    width = interval$width, retained_mean = retained,
    delta_raw = retained - dgp$mean,
    delta_standardized = (retained - dgp$mean) / dgp$sd,
    content = dgp$p(interval$upper) - dgp$p(interval$lower)
  )
}

oracle_rqr_interval <- function(dgp, coverage) {
  upper_u <- 1 - coverage
  balance <- function(u) {
    interval <- interval_from_u(dgp, u, coverage)
    retained_mean(dgp, interval$lower, interval$upper) - coverage * dgp$mean
  }
  eps <- min(1e-8, upper_u / 10)
  root <- stats::uniroot(balance, c(eps, upper_u - eps), tol = 1e-10)$root
  interval <- interval_from_u(dgp, root, coverage)
  retained_first <- retained_mean(dgp, interval$lower, interval$upper)
  list(
    u = root, lower = interval$lower, upper = interval$upper,
    width = interval$width,
    retained_mean = retained_first / coverage,
    delta_raw = retained_first / coverage - dgp$mean,
    delta_standardized = (retained_first / coverage - dgp$mean) / dgp$sd,
    content = dgp$p(interval$upper) - dgp$p(interval$lower)
  )
}

make_oracle_table <- function(coverage, init, dgps = dgp_registry()) {
  coverage <- assert_coverage(coverage)
  rows <- lapply(dgps, function(dgp) {
    sh <- oracle_shortest_interval(dgp, coverage)
    et <- oracle_equal_tailed_interval(dgp, coverage)
    rqr <- oracle_rqr_interval(dgp, coverage)
    data.frame(
      dgp_id = dgp$id,
      dgp = dgp$label,
      coverage = coverage,
      mean = dgp$mean,
      sd = dgp$sd,
      skewness = dgp$skewness,
      u_shortest = sh$u,
      shortest_boundary = sh$boundary,
      L_shortest = sh$lower,
      U_shortest = sh$upper,
      width_shortest = sh$width,
      delta_shortest = sh$delta_raw,
      d_shortest = sh$delta_standardized,
      content_shortest = sh$content,
      u_equal_tailed = et$u,
      width_equal_tailed = et$width,
      delta_equal_tailed = et$delta_raw,
      d_equal_tailed = et$delta_standardized,
      u_rqr = rqr$u,
      width_rqr = rqr$width,
      delta_rqr = rqr$delta_raw,
      d_rqr = rqr$delta_standardized,
      retained_mean_error_rqr = rqr$retained_mean - dgp$mean,
      d_cf_shortest = -dgp$skewness * init$rqr_mt_cf_constant(coverage),
      d_cf_equal_tailed =
        -dgp$skewness * init$rqr_mt_cf_constant(coverage) / 3,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

make_oracle_check_table <- function(oracle_table, coverage,
                                    dgps = dgp_registry()) {
  rows <- lapply(seq_len(nrow(oracle_table)), function(index) {
    row <- oracle_table[index, ]
    dgp <- dgps[[which(vapply(
      dgps, function(x) identical(x$id, row$dgp_id), logical(1L)
    ))]]
    grid <- seq(0, 1 - coverage, length.out = 5001L)
    widths <- vapply(
      grid, function(u) interval_from_u(dgp, u, coverage)$width, numeric(1L)
    )
    data.frame(
      dgp_id = row$dgp_id,
      dgp = row$dgp,
      coverage_pass = abs(row$content_shortest - coverage) <= 5e-8,
      rqr_mean_pass = abs(row$retained_mean_error_rqr) <= 5e-8,
      shortest_grid_pass =
        row$width_shortest <= min(widths, na.rm = TRUE) + 1e-6,
      finite_oracle_pass = all(is.finite(unlist(row[
        c("d_shortest", "d_equal_tailed", "d_rqr",
          "d_cf_shortest", "d_cf_equal_tailed")
      ]))),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

simulate_replicates <- function(oracle_table, coverage, n_values, reps,
                                seed, init, dgps = dgp_registry()) {
  set.seed(seed)
  oracle_lookup <- split(oracle_table, oracle_table$dgp_id)
  out <- vector("list", length(dgps) * length(n_values) * reps * 4L)
  index <- 0L
  for (dgp in dgps) {
    oracle <- oracle_lookup[[dgp$id]]
    for (n in n_values) {
      for (replication in seq_len(reps)) {
        y <- dgp$r(n)
        pilots <- list(
          cf_shortest = init$rqr_mt_tilt_cf(
            y, coverage, target = "shortest"
          ),
          cf_equal_tailed = init$rqr_mt_tilt_cf(
            y, coverage, target = "equal_tailed"
          ),
          empirical_shortest_window =
            init$rqr_mt_tilt_empirical_shortest(y, coverage),
          empirical_equal_tailed_window =
            init$rqr_mt_tilt_empirical_equal_tailed(y, coverage)
        )
        for (name in names(pilots)) {
          pilot <- pilots[[name]]
          target <- if (grepl("equal_tailed", name)) {
            "equal_tailed"
          } else {
            "shortest"
          }
          oracle_delta <- if (identical(target, "shortest")) {
            oracle$delta_shortest
          } else {
            oracle$delta_equal_tailed
          }
          oracle_d <- if (identical(target, "shortest")) {
            oracle$d_shortest
          } else {
            oracle$d_equal_tailed
          }
          index <- index + 1L
          out[[index]] <- data.frame(
            dgp_id = dgp$id,
            dgp = dgp$label,
            n = n,
            replication = replication,
            estimator = name,
            target = target,
            delta = pilot$delta_raw,
            d = pilot$delta_standardized,
            oracle_delta = oracle_delta,
            oracle_d = oracle_d,
            error_delta = pilot$delta_raw - oracle_delta,
            error_d = pilot$delta_standardized - oracle_d,
            boundary = pilot$boundary %||% NA_character_,
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  do.call(rbind, out)
}

summarize_replicates <- function(replicates) {
  groups <- split(
    replicates,
    list(replicates$dgp_id, replicates$n, replicates$estimator),
    drop = TRUE
  )
  rows <- lapply(groups, function(x) {
    data.frame(
      dgp_id = x$dgp_id[[1L]],
      dgp = x$dgp[[1L]],
      n = x$n[[1L]],
      estimator = x$estimator[[1L]],
      target = x$target[[1L]],
      oracle_delta = x$oracle_delta[[1L]],
      oracle_d = x$oracle_d[[1L]],
      bias_delta = mean(x$error_delta),
      bias_d = mean(x$error_d),
      rmse_delta = sqrt(mean(x$error_delta^2)),
      rmse_d = sqrt(mean(x$error_d^2)),
      mean_d = mean(x$d),
      sd_d = stats::sd(x$d),
      empirical_shortest_boundary_rate =
        mean(x$boundary %in% c("lower", "upper", "both")),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out[order(out$dgp_id, out$n, out$estimator), ]
}

plot_oracle_comparison <- function(oracle_table, output_pdf, output_png) {
  labels <- oracle_table$dgp
  values <- cbind(
    shortest = oracle_table$d_shortest,
    equal_tailed = oracle_table$d_equal_tailed,
    cornish_fisher_shortest = oracle_table$d_cf_shortest
  )
  draw <- function() {
    graphics::matplot(
      values, type = "b", pch = 19, lty = 1,
      xaxt = "n", xlab = "", ylab = "standardized tilt",
      main = sprintf("Mean-tilt oracle checks, c = %.2f",
                     oracle_table$coverage[[1L]])
    )
    graphics::axis(1, at = seq_along(labels), labels = FALSE)
    graphics::text(
      seq_along(labels), par("usr")[3L], labels = labels,
      srt = 35, adj = 1, xpd = NA, cex = 0.65
    )
    graphics::legend("topright", legend = colnames(values),
                     col = seq_len(ncol(values)), lty = 1, pch = 19,
                     cex = 0.75)
  }
  grDevices::pdf(output_pdf, width = 8, height = 5)
  draw()
  grDevices::dev.off()
  grDevices::png(output_png, width = 1200, height = 800, res = 150)
  draw()
  grDevices::dev.off()
}

plot_rmse_summary <- function(summary_table, output_pdf, output_png) {
  target <- summary_table[summary_table$target == "shortest", ]
  wide <- stats::reshape(
    target[, c("dgp", "estimator", "rmse_d")],
    idvar = "dgp", timevar = "estimator", direction = "wide"
  )
  labels <- wide$dgp
  mat <- as.matrix(wide[, setdiff(names(wide), "dgp"), drop = FALSE])
  colnames(mat) <- sub("^rmse_d\\.", "", colnames(mat))
  draw <- function() {
    graphics::matplot(
      mat, type = "b", pch = 19, lty = 1,
      xaxt = "n", xlab = "", ylab = "RMSE of standardized tilt",
      main = "Monte Carlo tilt-initializer smoke summary"
    )
    graphics::axis(1, at = seq_along(labels), labels = FALSE)
    graphics::text(
      seq_along(labels), par("usr")[3L], labels = labels,
      srt = 35, adj = 1, xpd = NA, cex = 0.65
    )
    graphics::legend("topright", legend = colnames(mat),
                     col = seq_len(ncol(mat)), lty = 1, pch = 19,
                     cex = 0.75)
  }
  grDevices::pdf(output_pdf, width = 8, height = 5)
  draw()
  grDevices::dev.off()
  grDevices::png(output_png, width = 1200, height = 800, res = 150)
  draw()
  grDevices::dev.off()
}

parse_logical <- function(x) {
  value <- tolower(as.character(x))
  if (value %in% c("true", "t", "1", "yes")) return(TRUE)
  if (value %in% c("false", "f", "0", "no")) return(FALSE)
  stop("logical arguments must be true or false.", call. = FALSE)
}

parse_args <- function(args) {
  cfg <- list(
    out_dir = NULL, coverage = 0.90, n_values = c(100L, 250L, 500L),
    reps = 500L, seed = 20260726L, save_replicates = FALSE,
    repo_root = "."
  )
  for (arg in args) {
    if (grepl("^--out-dir=", arg)) {
      cfg$out_dir <- sub("^--out-dir=", "", arg)
    } else if (grepl("^--coverage=", arg)) {
      cfg$coverage <- as.numeric(sub("^--coverage=", "", arg))
    } else if (grepl("^--n-values=", arg)) {
      cfg$n_values <- as.integer(strsplit(
        sub("^--n-values=", "", arg), ",", fixed = TRUE
      )[[1L]])
    } else if (grepl("^--reps=", arg)) {
      cfg$reps <- as.integer(sub("^--reps=", "", arg))
    } else if (grepl("^--seed=", arg)) {
      cfg$seed <- as.integer(sub("^--seed=", "", arg))
    } else if (grepl("^--save-replicates=", arg)) {
      cfg$save_replicates <- parse_logical(
        sub("^--save-replicates=", "", arg)
      )
    } else if (grepl("^--repo-root=", arg)) {
      cfg$repo_root <- sub("^--repo-root=", "", arg)
    } else if (identical(arg, "--help")) {
      cat(
        "Usage: Rscript validate_mt_rqr_cf_dgps.R --out-dir=PATH [options]\n",
        "--coverage=0.90 --n-values=100,250,500 --reps=500\n",
        "--seed=20260726 --save-replicates=false --repo-root=.\n",
        sep = ""
      )
      quit(save = "no", status = 0)
    } else {
      stopf("Unknown argument: %s", arg)
    }
  }
  if (is.null(cfg$out_dir) || !nzchar(cfg$out_dir)) {
    stop("--out-dir is required; validation outputs must be explicit.",
         call. = FALSE)
  }
  cfg$coverage <- assert_coverage(cfg$coverage)
  if (!length(cfg$n_values) || anyNA(cfg$n_values) ||
      any(cfg$n_values < 3L)) {
    stop("All n-values must be integers at least 3.", call. = FALSE)
  }
  if (is.na(cfg$reps) || cfg$reps < 1L) {
    stop("reps must be at least 1.", call. = FALSE)
  }
  if (is.na(cfg$seed)) stop("seed must be finite.", call. = FALSE)
  cfg
}

run_validation <- function(out_dir, coverage = 0.90,
                           n_values = c(100L, 250L, 500L),
                           reps = 500L, seed = 20260726L,
                           save_replicates = FALSE, repo_root = ".") {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  init <- load_mean_tilt_initializers(repo_root)
  dgps <- dgp_registry()
  oracle <- make_oracle_table(coverage, init, dgps)
  checks <- make_oracle_check_table(oracle, coverage, dgps)
  replicates <- simulate_replicates(
    oracle, coverage, n_values, reps, seed, init, dgps
  )
  summary <- summarize_replicates(replicates)

  utils::write.csv(oracle, file.path(out_dir, "cf_dgp_oracle_targets.csv"),
                   row.names = FALSE)
  utils::write.csv(checks, file.path(out_dir, "cf_dgp_oracle_checks.csv"),
                   row.names = FALSE)
  utils::write.csv(summary,
                   file.path(out_dir, "cf_dgp_monte_carlo_summary.csv"),
                   row.names = FALSE)
  if (isTRUE(save_replicates)) {
    utils::write.csv(
      replicates,
      file.path(out_dir, "cf_dgp_monte_carlo_replicates.csv"),
      row.names = FALSE
    )
  }
  config <- data.frame(
    field = c("coverage", "n_values", "reps", "seed", "save_replicates"),
    value = c(
      format(coverage, scientific = FALSE),
      paste(n_values, collapse = ","),
      as.character(reps),
      as.character(seed),
      as.character(save_replicates)
    ),
    stringsAsFactors = FALSE
  )
  utils::write.csv(config, file.path(out_dir, "cf_dgp_run_config.csv"),
                   row.names = FALSE)
  old_tz <- Sys.getenv("TZ", unset = NA_character_)
  Sys.setenv(TZ = "UTC")
  session_text <- capture.output(suppressWarnings(utils::sessionInfo()))
  if (is.na(old_tz)) {
    Sys.unsetenv("TZ")
  } else {
    Sys.setenv(TZ = old_tz)
  }
  writeLines(session_text, file.path(out_dir, "cf_dgp_session_info.txt"))
  plot_oracle_comparison(
    oracle,
    file.path(out_dir, "cf_dgp_oracle_comparison.pdf"),
    file.path(out_dir, "cf_dgp_oracle_comparison.png")
  )
  plot_rmse_summary(
    summary,
    file.path(out_dir, "cf_dgp_rmse_comparison.pdf"),
    file.path(out_dir, "cf_dgp_rmse_comparison.png")
  )
  if (!all(checks$coverage_pass) || !all(checks$rqr_mean_pass) ||
      !all(checks$shortest_grid_pass) || !all(checks$finite_oracle_pass)) {
    stop("One or more deterministic oracle validation checks failed.",
         call. = FALSE)
  }
  invisible(list(oracle = oracle, checks = checks, summary = summary))
}

main <- function() {
  cfg <- parse_args(commandArgs(trailingOnly = TRUE))
  run_validation(
    out_dir = cfg$out_dir,
    coverage = cfg$coverage,
    n_values = cfg$n_values,
    reps = cfg$reps,
    seed = cfg$seed,
    save_replicates = cfg$save_replicates,
    repo_root = cfg$repo_root
  )
  cat(sprintf(
    "Validation outputs written to: %s\n",
    normalizePath(cfg$out_dir, mustWork = FALSE)
  ))
}

if (sys.nframe() == 0L) {
  main()
}
