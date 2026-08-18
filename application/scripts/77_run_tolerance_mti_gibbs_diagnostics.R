#!/usr/bin/env Rscript

arguments <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_path_override <- Sys.getenv("RQRGIBBS_GIBBS_DIAGNOSTIC_SCRIPT_PATH",
                                   unset = "")
script_path <- if (nzchar(script_path_override)) {
  script_path_override
} else {
  sub(file_arg, "", arguments[startsWith(arguments, file_arg)][1L])
}
if (!nzchar(script_path_override) &&
    (identical(Sys.getenv("RQRGIBBS_GIBBS_DIAGNOSTIC_SOURCE_ONLY"), "true") ||
     !length(script_path) || is.na(script_path) ||
     !identical(basename(script_path),
                "77_run_tolerance_mti_gibbs_diagnostics.R"))) {
  script_path <- "application/scripts/77_run_tolerance_mti_gibbs_diagnostics.R"
}
script_path <- normalizePath(script_path, winslash = "/", mustWork = TRUE)
repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."),
                           winslash = "/", mustWork = TRUE)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

gdiag_stop <- function(...) stop(paste0(...), call. = FALSE)

gdiag_arg_value <- function(args, prefix, default = NULL) {
  hit <- args[startsWith(args, prefix)]
  if (length(hit)) sub(prefix, "", hit[[1L]], fixed = TRUE) else default
}

gdiag_bool_arg <- function(value, default = FALSE) {
  if (is.null(value) || !nzchar(value)) return(default)
  value <- tolower(trimws(value))
  if (value %in% c("true", "t", "1", "yes", "y")) return(TRUE)
  if (value %in% c("false", "f", "0", "no", "n")) return(FALSE)
  gdiag_stop("Invalid logical argument value: ", value)
}

gdiag_split_arg <- function(value, default = NULL) {
  if (is.null(value) || !nzchar(value)) return(default)
  value <- unlist(strsplit(value, ",", fixed = TRUE), use.names = FALSE)
  value <- trimws(value)
  value[nzchar(value)]
}

gdiag_require_packages <- function(packages) {
  for (package in packages) {
    if (!requireNamespace(package, quietly = TRUE)) {
      gdiag_stop("Required package is not installed: ", package)
    }
  }
}

gdiag_git_commit <- function(root = repo_root) {
  tryCatch(
    system2("git", c("-C", root, "rev-parse", "HEAD"),
            stdout = TRUE, stderr = FALSE)[[1L]],
    error = function(e) NA_character_
  )
}

gdiag_git_status <- function(root = repo_root) {
  tryCatch(
    paste(system2("git", c("-C", root, "status", "--short", "--branch"),
                  stdout = TRUE, stderr = FALSE), collapse = "\n"),
    error = function(e) NA_character_
  )
}

gdiag_hash_file <- function(path) {
  digest::digest(path, algo = "sha256", file = TRUE)
}

gdiag_hash_to_seed <- function(text, base = 862100L) {
  bytes <- as.integer(charToRaw(as.character(text)))
  value <- as.integer(base)
  for (byte in bytes) {
    value <- as.integer((as.double(value) * 131 + byte) %% 2147483647)
  }
  if (value <= 0L) value <- value + 1L
  value
}

gdiag_num <- function(x) suppressWarnings(as.numeric(x))

gdiag_mean <- function(x) {
  x <- gdiag_num(x)
  x <- x[is.finite(x)]
  if (length(x)) mean(x) else NA_real_
}

gdiag_median <- function(x) {
  x <- gdiag_num(x)
  x <- x[is.finite(x)]
  if (length(x)) stats::median(x) else NA_real_
}

gdiag_quantile <- function(x, probability) {
  x <- gdiag_num(x)
  x <- x[is.finite(x)]
  if (length(x)) {
    as.numeric(stats::quantile(x, probability, names = FALSE, type = 8))
  } else {
    NA_real_
  }
}

gdiag_read_config <- function(path) {
  jsonlite::read_json(normalizePath(path, winslash = "/", mustWork = TRUE),
                      simplifyVector = FALSE)
}

gdiag_config_dgp_by_id <- function(config) {
  setNames(config$dgps, vapply(config$dgps, `[[`, character(1L), "dgp_id"))
}

gdiag_dgp_meta <- function(dgp) {
  if (identical(dgp$family, "normal")) {
    return(list(r = function(n) stats::rnorm(n), p = stats::pnorm))
  }
  if (identical(dgp$family, "standardized_lognormal")) {
    logsd <- as.numeric(dgp$logsd %||% 0.75)[1L]
    mean_raw <- exp(logsd^2 / 2)
    sd_raw <- sqrt((exp(logsd^2) - 1) * exp(logsd^2))
    return(list(
      r = function(n) (stats::rlnorm(n, 0, logsd) - mean_raw) / sd_raw,
      p = function(x) stats::plnorm(x * sd_raw + mean_raw, 0, logsd)
    ))
  }
  if (identical(dgp$family, "standardized_normal_mixture")) {
    weights <- as.numeric(dgp$weights)
    means <- as.numeric(dgp$means)
    sds <- as.numeric(dgp$sds)
    mean_mix <- sum(weights * means)
    second <- sum(weights * (sds^2 + means^2))
    sd_mix <- sqrt(second - mean_mix^2)
    return(list(
      r = function(n) {
        comp <- sample.int(length(weights), n, replace = TRUE, prob = weights)
        (stats::rnorm(n, means[comp], sds[comp]) - mean_mix) / sd_mix
      },
      p = function(x) {
        raw <- x * sd_mix + mean_mix
        out <- numeric(length(raw))
        for (j in seq_along(weights)) {
          out <- out + weights[[j]] * stats::pnorm(raw, means[[j]], sds[[j]])
        }
        out
      }
    ))
  }
  if (identical(dgp$family, "standardized_student_t")) {
    df <- as.numeric(dgp$df %||% 3)[1L]
    if (!is.finite(df) || df <= 2) {
      gdiag_stop("standardized_student_t requires df > 2.")
    }
    sd_raw <- sqrt(df / (df - 2))
    return(list(
      r = function(n) stats::rt(n, df = df) / sd_raw,
      p = function(x) stats::pt(x * sd_raw, df = df)
    ))
  }
  if (identical(dgp$family, "standardized_beta")) {
    shape1 <- as.numeric(dgp$shape1 %||% dgp$a %||% 2)[1L]
    shape2 <- as.numeric(dgp$shape2 %||% dgp$b %||% 5)[1L]
    mean_raw <- shape1 / (shape1 + shape2)
    sd_raw <- sqrt(
      shape1 * shape2 /
        ((shape1 + shape2)^2 * (shape1 + shape2 + 1))
    )
    return(list(
      r = function(n) {
        (stats::rbeta(n, shape1, shape2) - mean_raw) / sd_raw
      },
      p = function(x) {
        stats::pbeta(x * sd_raw + mean_raw, shape1, shape2)
      }
    ))
  }
  gdiag_stop("Unsupported DGP family: ", dgp$family)
}

gdiag_dataset_seed <- function(config, dgp_id, n, content, replication) {
  base_seed <- as.integer(config$base_seed %||% 862100L)
  gdiag_hash_to_seed(
    paste("data", dgp_id, n, content, replication, sep = "|"),
    base = base_seed
  )
}

gdiag_scan_seed <- function(mode, n, content, confidence, n_sim,
                            numerical_confidence, scan_seed_base) {
  key <- paste(mode, "monte_carlo_conservative", n, content, confidence,
               n_sim, numerical_confidence, sep = "|")
  gdiag_hash_to_seed(key, base = scan_seed_base)
}

gdiag_default_plan <- function() {
  dgps <- c("normal", "lognormal_hard", "sharp_mixture",
            "contaminated_normal", "student_t3")
  rbind(
    data.frame(
      mode_source = "main",
      cell_role = "hard_feasible_large",
      dgp_id = dgps,
      n = 1000L,
      guaranteed_content = 0.99,
      tolerance_confidence = 0.95,
      replications = 1L,
      run_gibbs = TRUE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      mode_source = "followup",
      cell_role = "small_feasible",
      dgp_id = dgps,
      n = 100L,
      guaranteed_content = 0.90,
      tolerance_confidence = 0.95,
      replications = 1L,
      run_gibbs = TRUE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      mode_source = "main",
      cell_role = "expected_fail_closed",
      dgp_id = dgps,
      n = 500L,
      guaranteed_content = 0.99,
      tolerance_confidence = 0.95,
      replications = 1L,
      run_gibbs = FALSE,
      stringsAsFactors = FALSE
    )
  )
}

gdiag_trace_indices <- function(n_burn, n_mcmc, thin) {
  n_burn + seq.int(thin, by = thin, length.out = n_mcmc)
}

gdiag_basic_rhat <- function(values, chain) {
  values <- as.numeric(values)
  chain <- as.integer(chain)
  ok <- is.finite(values) & !is.na(chain)
  values <- values[ok]
  chain <- chain[ok]
  chains <- split(values, chain)
  chains <- chains[vapply(chains, length, integer(1L)) >= 2L]
  if (length(chains) < 2L) return(NA_real_)
  n <- min(vapply(chains, length, integer(1L)))
  chains <- lapply(chains, function(x) tail(x, n))
  means <- vapply(chains, mean, numeric(1L))
  vars <- vapply(chains, stats::var, numeric(1L))
  W <- mean(vars)
  if (!is.finite(W) || W <= 0) return(NA_real_)
  B <- n * stats::var(means)
  var_plus <- ((n - 1) / n) * W + B / n
  sqrt(var_plus / W)
}

gdiag_effective_size <- function(values, chain) {
  if (!requireNamespace("coda", quietly = TRUE)) return(NA_real_)
  values <- as.numeric(values)
  chain <- as.integer(chain)
  ok <- is.finite(values) & !is.na(chain)
  values <- values[ok]
  chain <- chain[ok]
  chains <- split(values, chain)
  chains <- chains[vapply(chains, length, integer(1L)) >= 2L]
  if (length(chains) < 1L) return(NA_real_)
  mcmc_list <- coda::mcmc.list(lapply(chains, coda::mcmc))
  as.numeric(coda::effectiveSize(mcmc_list)[[1L]])
}

gdiag_draw_diagnostics <- function(draws) {
  if (!nrow(draws)) return(data.frame())
  estimands <- c("lower", "upper", "width", "midpoint", "loss",
                 "target_loss")
  rows <- lapply(estimands, function(est) {
    if (!est %in% names(draws)) return(NULL)
    data.frame(
      mode_source = draws$mode_source[[1L]],
      cell_role = draws$cell_role[[1L]],
      dgp_id = draws$dgp_id[[1L]],
      n = draws$n[[1L]],
      guaranteed_content = draws$guaranteed_content[[1L]],
      tolerance_confidence = draws$tolerance_confidence[[1L]],
      replication = draws$replication[[1L]],
      estimand = est,
      chains = length(unique(draws$chain)),
      draws_per_chain = min(table(draws$chain)),
      mean = gdiag_mean(draws[[est]]),
      sd = stats::sd(gdiag_num(draws[[est]]), na.rm = TRUE),
      q05 = gdiag_quantile(draws[[est]], 0.05),
      median = gdiag_median(draws[[est]]),
      q95 = gdiag_quantile(draws[[est]], 0.95),
      rhat = gdiag_basic_rhat(draws[[est]], draws$chain),
      ess = gdiag_effective_size(draws[[est]], draws$chain),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

gdiag_run_task <- function(task) {
  config <- task$config
  dgp_by_id <- gdiag_config_dgp_by_id(config)
  dgp <- dgp_by_id[[task$dgp_id]]
  if (is.null(dgp)) gdiag_stop("Unknown DGP ID: ", task$dgp_id)
  meta <- gdiag_dgp_meta(dgp)
  data_seed <- gdiag_dataset_seed(
    config, task$dgp_id, task$n, task$guaranteed_content, task$replication
  )
  set.seed(data_seed)
  y <- meta$r(task$n)
  scan_seed <- gdiag_scan_seed(
    task$mode_source, task$n, task$guaranteed_content,
    task$tolerance_confidence, task$n_sim, task$numerical_confidence,
    task$scan_seed_base
  )
  calibration <- tryCatch(
    rqrgibbs::tcsp_calibrate_count(
      n = task$n,
      guaranteed_content = task$guaranteed_content,
      tolerance_confidence = task$tolerance_confidence,
      method = "monte_carlo_conservative",
      n_sim = task$n_sim,
      numerical_confidence = task$numerical_confidence,
      seed = scan_seed
    ),
    error = function(e) {
      list(infeasible = TRUE, message = conditionMessage(e))
    }
  )
  if (isTRUE(calibration$infeasible) ||
      is.null(calibration$retained_count) ||
      calibration$retained_count > task$n) {
    return(list(
      draws = data.frame(),
      chain_summary = data.frame(),
      failclosed = data.frame(
        mode_source = task$mode_source,
        cell_role = task$cell_role,
        dgp_id = task$dgp_id,
        n = task$n,
        guaranteed_content = task$guaranteed_content,
        tolerance_confidence = task$tolerance_confidence,
        replication = task$replication,
        chain = task$chain,
        data_seed = data_seed,
        scan_seed = scan_seed,
        retained_count = calibration$retained_count %||% NA_integer_,
        target_content = calibration$target_content %||% NA_real_,
        content_buffer = calibration$content_buffer %||% NA_real_,
        reason = calibration$message %||% "scan_calibration_infeasible",
        stringsAsFactors = FALSE
      )
    ))
  }
  window <- rqrgibbs::tcsp_shortest_window(
    y, retained_count = calibration$retained_count, na_rm = FALSE
  )
  tilt <- rqrgibbs::tcsp_tilt_from_window(window)
  target_content <- calibration$target_content
  if (!is.finite(target_content) || target_content >= 1) {
    return(list(
      draws = data.frame(),
      chain_summary = data.frame(),
      failclosed = data.frame(
        mode_source = task$mode_source,
        cell_role = task$cell_role,
        dgp_id = task$dgp_id,
        n = task$n,
        guaranteed_content = task$guaranteed_content,
        tolerance_confidence = task$tolerance_confidence,
        replication = task$replication,
        chain = task$chain,
        data_seed = data_seed,
        scan_seed = scan_seed,
        retained_count = calibration$retained_count,
        target_content = target_content,
        content_buffer = calibration$content_buffer %||% NA_real_,
        reason = "target_content_not_in_open_unit_interval",
        stringsAsFactors = FALSE
      )
    ))
  }
  if (!isTRUE(task$run_gibbs)) {
    return(list(
      draws = data.frame(),
      chain_summary = data.frame(),
      failclosed = data.frame(
        mode_source = task$mode_source,
        cell_role = task$cell_role,
        dgp_id = task$dgp_id,
        n = task$n,
        guaranteed_content = task$guaranteed_content,
        tolerance_confidence = task$tolerance_confidence,
        replication = task$replication,
        chain = task$chain,
        data_seed = data_seed,
        scan_seed = scan_seed,
        retained_count = calibration$retained_count,
        target_content = target_content,
        content_buffer = calibration$content_buffer %||% NA_real_,
        reason = "diagnostic_failclosed_cell_no_gibbs_requested",
        stringsAsFactors = FALSE
      )
    ))
  }

  X <- matrix(1, task$n, 1L, dimnames = list(NULL, "(Intercept)"))
  prior <- rqrgibbs::beta_prior(
    "ridge", ridge = list(tau2 = as.numeric(task$beta_ridge_tau2))
  )
  chain_seed <- gdiag_hash_to_seed(
    paste("gibbs_diagnostic", task$mode_source, task$dgp_id, task$n,
          task$guaranteed_content, task$tolerance_confidence,
          task$replication, task$chain, sep = "|"),
    base = task$chain_seed_base
  )
  elapsed <- system.time({
    fit <- rqrgibbs::mti_mcmc_fit(
      y = y,
      X = X,
      coverage_level = target_content,
      learning_rate = as.numeric(task$learning_rate),
      mean_tilt = tilt$delta_raw,
      learning_rate_mode = "fixed_rate",
      beta_prior_obj = prior,
      mcmc_control = list(
        n_burn = task$n_burn,
        n_mcmc = task$n_mcmc,
        thin = task$thin,
        seed = chain_seed,
        store_latent_draws = FALSE
      )
    )
  })[["elapsed"]]
  pred <- rqrgibbs::predict_interval(fit, X_new = X[1L, , drop = FALSE])
  trace_idx <- gdiag_trace_indices(task$n_burn, task$n_mcmc, task$thin)
  loss <- fit$diagnostics$ordinary_product_check_loss_trace[trace_idx]
  target_loss <- fit$diagnostics$mean_tilted_target_loss_trace[trace_idx]
  lambda <- fit$diagnostics$lambda_trace[trace_idx]
  root_swaps <- fit$diagnostics$root_swap_count_trace[trace_idx]
  draws <- data.frame(
    mode_source = task$mode_source,
    cell_role = task$cell_role,
    dgp_id = task$dgp_id,
    n = task$n,
    guaranteed_content = task$guaranteed_content,
    tolerance_confidence = task$tolerance_confidence,
    replication = task$replication,
    chain = task$chain,
    draw = seq_len(task$n_mcmc),
    data_seed = data_seed,
    chain_seed = chain_seed,
    scan_seed = scan_seed,
    retained_count = calibration$retained_count,
    target_content = target_content,
    target_mean_tilt = tilt$delta_raw,
    formal_action_lower = window$lower_endpoint,
    formal_action_upper = window$upper_endpoint,
    formal_action_width = window$width,
    lower = as.numeric(pred$lower_draws[1L, ]),
    upper = as.numeric(pred$upper_draws[1L, ]),
    midpoint = as.numeric(pred$midpoint_draws[1L, ]),
    width = as.numeric(pred$width_draws[1L, ]),
    loss = as.numeric(loss),
    target_loss = as.numeric(target_loss),
    lambda = as.numeric(lambda),
    root_swap_count = as.integer(root_swaps),
    stringsAsFactors = FALSE
  )
  chain_summary <- data.frame(
    mode_source = task$mode_source,
    cell_role = task$cell_role,
    dgp_id = task$dgp_id,
    n = task$n,
    guaranteed_content = task$guaranteed_content,
    tolerance_confidence = task$tolerance_confidence,
    replication = task$replication,
    chain = task$chain,
    data_seed = data_seed,
    chain_seed = chain_seed,
    scan_seed = scan_seed,
    retained_count = calibration$retained_count,
    target_content = target_content,
    target_mean_tilt = tilt$delta_raw,
    n_burn = task$n_burn,
    n_mcmc = task$n_mcmc,
    thin = task$thin,
    elapsed_sec = as.numeric(elapsed),
    lower_median = gdiag_median(draws$lower),
    upper_median = gdiag_median(draws$upper),
    width_median = gdiag_median(draws$width),
    midpoint_median = gdiag_median(draws$midpoint),
    target_loss_median = gdiag_median(draws$target_loss),
    root_swap_total = sum(draws$root_swap_count, na.rm = TRUE),
    fit_class = paste(class(fit), collapse = "|"),
    response_likelihood = isTRUE(fit$model_spec$response_likelihood),
    model_spec_digest = digest::digest(fit$model_spec, algo = "sha256",
                                       serialize = TRUE),
    stringsAsFactors = FALSE
  )
  list(draws = draws, chain_summary = chain_summary, failclosed = data.frame())
}

gdiag_bind <- function(items, name) {
  frames <- lapply(items, `[[`, name)
  frames <- frames[vapply(frames, nrow, integer(1L)) > 0L]
  if (!length(frames)) return(data.frame())
  cols <- Reduce(union, lapply(frames, names))
  frames <- lapply(frames, function(x) {
    missing <- setdiff(cols, names(x))
    for (col in missing) x[[col]] <- NA
    x[, cols, drop = FALSE]
  })
  do.call(rbind, frames)
}

gdiag_write_plots <- function(draws, estimator, path) {
  if (!nrow(draws) || !estimator %in% names(draws)) return(FALSE)
  cells <- unique(draws[, c("mode_source", "cell_role", "dgp_id", "n",
                            "guaranteed_content"), drop = FALSE])
  cells <- head(cells, 6L)
  grDevices::png(path, width = 1200, height = 850, res = 120)
  on.exit(grDevices::dev.off(), add = TRUE)
  par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))
  for (ii in seq_len(nrow(cells))) {
    cell <- cells[ii, , drop = FALSE]
    keep <- draws$mode_source == cell$mode_source &
      draws$dgp_id == cell$dgp_id &
      draws$n == cell$n &
      abs(draws$guaranteed_content - cell$guaranteed_content) < 1e-12
    z <- draws[keep, , drop = FALSE]
    plot(range(z$draw), range(z[[estimator]], finite = TRUE), type = "n",
         xlab = "Draw", ylab = estimator,
         main = paste(cell$dgp_id, cell$n,
                      sprintf("c=%.2f", cell$guaranteed_content)))
    for (chain in sort(unique(z$chain))) {
      zz <- z[z$chain == chain, , drop = FALSE]
      lines(zz$draw, zz[[estimator]], col = chain)
    }
  }
  par(mfrow = c(1, 1))
  TRUE
}

gdiag_artifact_hashes <- function(output_dir, files) {
  data.frame(
    artifact = files,
    path = normalizePath(file.path(output_dir, files), winslash = "/",
                         mustWork = TRUE),
    size_bytes = file.info(file.path(output_dir, files))$size,
    sha256 = vapply(file.path(output_dir, files), gdiag_hash_file,
                    character(1L)),
    stringsAsFactors = FALSE
  )
}

gdiag_scalarize_task_row <- function(row, config, n_burn, n_mcmc, thin,
                                     n_sim, numerical_confidence,
                                     scan_seed_base, chain_seed_base,
                                     learning_rate, beta_ridge_tau2) {
  out <- as.list(row[1L, , drop = FALSE])
  out <- lapply(out, function(value) value[[1L]])
  out$config <- config
  out$n <- as.integer(out$n)
  out$replications <- as.integer(out$replications)
  out$run_gibbs <- isTRUE(out$run_gibbs)
  out$replication <- as.integer(out$replication)
  out$chain <- as.integer(out$chain)
  out$task_id <- as.integer(out$task_id)
  out$n_burn <- as.integer(n_burn)
  out$n_mcmc <- as.integer(n_mcmc)
  out$thin <- as.integer(thin)
  out$n_sim <- as.integer(n_sim)
  out$numerical_confidence <- as.numeric(numerical_confidence)
  out$scan_seed_base <- as.integer(scan_seed_base)
  out$chain_seed_base <- as.integer(chain_seed_base)
  out$learning_rate <- as.numeric(learning_rate)
  out$beta_ridge_tau2 <- as.numeric(beta_ridge_tau2)
  out
}

gdiag_run <- function(main_config_path, followup_config_path, output_dir,
                      chains = 4L, replications = 1L, workers = 4L,
                      n_burn = 500L, n_mcmc = 1000L, thin = 1L,
                      n_sim = 5000L, numerical_confidence = 0.995,
                      learning_rate = 1, beta_ridge_tau2 = 10000,
                      include_failclosed = TRUE, dgp_ids = NULL,
                      cell_roles = NULL) {
  gdiag_require_packages(c("rqrgibbs", "jsonlite", "digest"))
  main_config <- gdiag_read_config(main_config_path)
  followup_config <- gdiag_read_config(followup_config_path)
  plan <- gdiag_default_plan()
  if (!is.null(dgp_ids)) {
    plan <- plan[plan$dgp_id %in% dgp_ids, , drop = FALSE]
  }
  if (!is.null(cell_roles)) {
    plan <- plan[plan$cell_role %in% cell_roles, , drop = FALSE]
  }
  if (!include_failclosed) {
    plan <- plan[plan$run_gibbs %in% TRUE, , drop = FALSE]
  }
  if (!nrow(plan)) {
    gdiag_stop("No diagnostic plan rows remain after filtering.")
  }
  plan$replications <- as.integer(replications)
  plan <- do.call(rbind, lapply(seq_len(nrow(plan)), function(ii) {
    row <- plan[ii, , drop = FALSE]
    row[rep(1L, row$replications), , drop = FALSE]
  }))
  plan$replication <- unlist(lapply(split(plan, seq_len(nrow(plan))),
                                    function(x) seq_len(x$replications)))
  plan <- plan[rep(seq_len(nrow(plan)), each = chains), , drop = FALSE]
  plan$chain <- rep(seq_len(chains), length.out = nrow(plan))
  plan$task_id <- seq_len(nrow(plan))

  scan_seed_base <- as.integer(
    main_config$scan_calibration$seed %||%
      followup_config$scan_calibration$seed %||% 1512600L
  )
  chain_seed_base <- as.integer(main_config$base_seed %||% 963100L) + 700000L
  tasks <- lapply(seq_len(nrow(plan)), function(ii) {
    row <- plan[ii, , drop = FALSE]
    config <- if (identical(row$mode_source, "main")) {
      main_config
    } else {
      followup_config
    }
    gdiag_scalarize_task_row(
      row = row,
      config = config,
      n_burn = n_burn,
      n_mcmc = n_mcmc,
      thin = thin,
      n_sim = n_sim,
      numerical_confidence = numerical_confidence,
      scan_seed_base = scan_seed_base,
      chain_seed_base = chain_seed_base,
      learning_rate = learning_rate,
      beta_ridge_tau2 = beta_ridge_tau2
    )
  })

  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
  if (file.exists(output_dir) || dir.exists(output_dir)) {
    gdiag_stop("The output directory must be fresh: ", output_dir)
  }
  dir.create(dirname(output_dir), recursive = TRUE, showWarnings = FALSE)
  staging <- tempfile(paste0(".", basename(output_dir), "-"),
                      tmpdir = dirname(output_dir))
  dir.create(staging, recursive = TRUE, showWarnings = FALSE)
  published <- FALSE
  on.exit({
    if (!published) unlink(staging, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  utils::write.csv(plan, file.path(staging, "gibbs_diagnostic_plan.csv"),
                   row.names = FALSE)

  workers <- max(1L, as.integer(workers))
  if (.Platform$OS.type == "unix" && workers > 1L) {
    results <- parallel::mclapply(tasks, gdiag_run_task, mc.cores = workers,
                                  mc.preschedule = FALSE)
  } else {
    results <- lapply(tasks, gdiag_run_task)
  }
  draws <- gdiag_bind(results, "draws")
  chain_summary <- gdiag_bind(results, "chain_summary")
  failclosed <- gdiag_bind(results, "failclosed")
  diagnostics <- if (nrow(draws)) {
    do.call(rbind, lapply(
      split(draws, interaction(draws$mode_source, draws$dgp_id, draws$n,
                               draws$guaranteed_content,
                               draws$tolerance_confidence,
                               draws$replication, drop = TRUE,
                               lex.order = TRUE)),
      gdiag_draw_diagnostics
    ))
  } else {
    data.frame()
  }
  if (nrow(diagnostics)) {
    diagnostics <- diagnostics[order(
      diagnostics$mode_source, diagnostics$dgp_id, diagnostics$n,
      diagnostics$guaranteed_content, diagnostics$estimand
    ), , drop = FALSE]
  }

  utils::write.csv(draws, file.path(staging, "gibbs_chain_draws.csv"),
                   row.names = FALSE)
  utils::write.csv(chain_summary, file.path(staging, "gibbs_chain_summary.csv"),
                   row.names = FALSE)
  utils::write.csv(diagnostics,
                   file.path(staging, "gibbs_estimator_diagnostics.csv"),
                   row.names = FALSE)
  utils::write.csv(failclosed, file.path(staging, "gibbs_failclosed_audit.csv"),
                   row.names = FALSE)

  plot_files <- character()
  if (gdiag_write_plots(draws, "width",
                        file.path(staging, "gibbs_width_trace_plot.png"))) {
    plot_files <- c(plot_files, "gibbs_width_trace_plot.png")
  }
  if (gdiag_write_plots(draws, "target_loss",
                        file.path(staging, "gibbs_target_loss_trace_plot.png"))) {
    plot_files <- c(plot_files, "gibbs_target_loss_trace_plot.png")
  }
  readme <- c(
    "# MTI Gibbs Targeted Diagnostics",
    "",
    "This ignored bundle runs independent fixed-target MTI Gibbs chains on selected tolerance-validation cells.",
    "It is a convergence and stability audit for fitted MTI summaries, not a new full validation study.",
    "",
    "The formal tolerance action remains the scan-selected TCSP interval. The Gibbs draws summarize the conditional MTI fitted target."
  )
  writeLines(readme, file.path(staging, "README.md"))
  artifact_files <- c(
    "README.md", "gibbs_diagnostic_plan.csv", "gibbs_chain_draws.csv",
    "gibbs_chain_summary.csv", "gibbs_estimator_diagnostics.csv",
    "gibbs_failclosed_audit.csv", plot_files
  )
  artifact_hashes <- gdiag_artifact_hashes(staging, artifact_files)
  utils::write.csv(artifact_hashes, file.path(staging, "artifact_hashes.csv"),
                   row.names = FALSE)
  manifest <- list(
    schema_version = "rqrgibbs_tolerance_mti_gibbs_diagnostics/1.0.0",
    generated_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    repo_root = repo_root,
    git_commit = gdiag_git_commit(repo_root),
    git_status = gdiag_git_status(repo_root),
    script_path = script_path,
    script_sha256 = gdiag_hash_file(script_path),
    main_config_path = normalizePath(main_config_path, winslash = "/",
                                     mustWork = TRUE),
    followup_config_path = normalizePath(followup_config_path, winslash = "/",
                                         mustWork = TRUE),
    input_hashes = list(
      main_config_sha256 = gdiag_hash_file(main_config_path),
      followup_config_sha256 = gdiag_hash_file(followup_config_path)
    ),
    controls = list(
      chains = chains,
      replications = replications,
      workers = workers,
      n_burn = n_burn,
      n_mcmc = n_mcmc,
      thin = thin,
      n_sim = n_sim,
      numerical_confidence = numerical_confidence,
      learning_rate = learning_rate,
      beta_ridge_tau2 = beta_ridge_tau2,
      dgp_ids = dgp_ids %||% "all",
      cell_roles = cell_roles %||% "all"
    ),
    rows = list(
      plan_rows = nrow(plan),
      draw_rows = nrow(draws),
      chain_summary_rows = nrow(chain_summary),
      estimator_diagnostic_rows = nrow(diagnostics),
      failclosed_rows = nrow(failclosed)
    ),
    gates = list(
      all_fits_response_likelihood_false =
        if (nrow(chain_summary)) all(!chain_summary$response_likelihood) else TRUE,
      all_requested_chains_returned =
        sum(plan$run_gibbs) == nrow(chain_summary),
      any_rhat_above_1_05 =
        if (nrow(diagnostics)) any(diagnostics$rhat > 1.05, na.rm = TRUE)
        else NA,
      any_rhat_above_1_10 =
        if (nrow(diagnostics)) any(diagnostics$rhat > 1.10, na.rm = TRUE)
        else NA
    ),
    artifact_hashes = artifact_hashes
  )
  jsonlite::write_json(manifest, file.path(staging, "manifest.json"),
                       pretty = TRUE, auto_unbox = TRUE)
  artifact_files <- c(artifact_files, "manifest.json")
  artifact_hashes <- gdiag_artifact_hashes(staging, artifact_files)
  utils::write.csv(artifact_hashes, file.path(staging, "artifact_hashes.csv"),
                   row.names = FALSE)

  file.rename(staging, output_dir)
  published <- TRUE
  output_dir
}

gdiag_main <- function(args = commandArgs(trailingOnly = TRUE)) {
  gdiag_require_packages(c("rqrgibbs", "jsonlite", "digest"))
  setwd(repo_root)
  default_output <- file.path(
    "application", "outputs", "tolerance_mti_gibbs_diagnostics",
    paste0("gibbs_diagnostics_", format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"))
  )
  out <- gdiag_run(
    main_config_path = gdiag_arg_value(
      args, "--main-config=",
      file.path("application", "config",
                "rqr_bayes_uq_validation_main_20260813.json")
    ),
    followup_config_path = gdiag_arg_value(
      args, "--followup-config=",
      file.path("application", "config",
                "rqr_bayes_uq_followup_20260816.json")
    ),
    output_dir = gdiag_arg_value(args, "--output-dir=", default_output),
    chains = as.integer(gdiag_arg_value(args, "--chains=", "4")),
    replications = as.integer(gdiag_arg_value(args, "--replications=", "1")),
    workers = as.integer(gdiag_arg_value(args, "--workers=", "4")),
    n_burn = as.integer(gdiag_arg_value(args, "--n-burn=", "500")),
    n_mcmc = as.integer(gdiag_arg_value(args, "--n-mcmc=", "1000")),
    thin = as.integer(gdiag_arg_value(args, "--thin=", "1")),
    n_sim = as.integer(gdiag_arg_value(args, "--n-sim=", "5000")),
    numerical_confidence = as.numeric(gdiag_arg_value(
      args, "--numerical-confidence=", "0.995"
    )),
    include_failclosed = gdiag_bool_arg(gdiag_arg_value(
      args, "--include-failclosed=", "true"
    ), default = TRUE),
    dgp_ids = gdiag_split_arg(gdiag_arg_value(args, "--dgp-ids=", "")),
    cell_roles = gdiag_split_arg(gdiag_arg_value(args, "--cell-roles=", ""))
  )
  cat("MTI Gibbs diagnostics written to:", out, "\n")
  invisible(out)
}

if (!identical(Sys.getenv("RQRGIBBS_GIBBS_DIAGNOSTIC_SOURCE_ONLY"), "true")) {
  gdiag_main()
}
