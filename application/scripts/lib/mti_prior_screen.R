mps_spec <- function(y, kappa) {
  stopifnot(length(y) >= 2, all(is.finite(y)), length(kappa) == 1L, is.finite(kappa), kappa > 0)
  s <- stats::sd(y); m <- mean(y)
  mwd_require(is.finite(s) && s > 0, "A constant response needs a separate interval specification.")
  list(center = m, scale = s, prior_sd = kappa * s, learning_rate = 1 / s^2,
    kappa = kappa, z = (y - m) / s, data_adaptive_prior = TRUE)
}

mps_programs <- c("application/scripts/118_select_mti_prior.R", "application/scripts/lib/mti_prior_screen.R",
  "application/config/mti_prior_screen_20261113.json", mwd_programs)

mps_disk_check <- function(output, reserve) {
  rows <- system2("df", c("-Pk", shQuote(output)), stdout = TRUE)
  fields <- strsplit(trimws(tail(rows, 1L)), "[[:space:]]+")[[1]]
  available <- suppressWarnings(as.numeric(fields[4])) / 1024^2
  mwd_require(is.finite(available) && available >= reserve,
    "The prior study stopped to preserve its declared free-disk reserve.")
  invisible(available)
}

mps_package_check <- function() {
  ns <- asNamespace("rqrgibbs"); e <- new.env()
  files <- c("rqr_ecm_fit.R", "rqr_ecm_utils.R", "rqr_mti_ecm_profile.R", "rqr_mti_ecm_adaptive.R")
  for (p in files) sys.source(file.path("application/R", p), e)
  for (name in ls(e, all.names = TRUE)) mwd_require(exists(name, ns, inherits = FALSE) &&
    identical(deparse(e[[name]]), deparse(get(name, ns))), paste("Installed computation differs:", name))
  root <- system.file(package = "rqrgibbs")
  binary <- list.files(file.path(root, "libs"), full.names = TRUE, recursive = TRUE)
  mwd_require(length(binary) > 0, "The installed native library is missing.")
  installed <- list.files(root, full.names = TRUE, recursive = TRUE)
  list(version = as.character(packageVersion("rqrgibbs")), libraries = mgfp_hashes(binary),
    installed_files = mgfp_hashes(installed),
    r_sources = mgfp_hashes(file.path("application/R", files)),
    native_source = mgfp_hashes("application/src/rqr_ecm_intercept.cpp"))
}

mps_context <- function(cfg, output) {
  dc <- mwd_json(cfg$diagnostic_config); context <- mwd_context(dc, output)
  article <- mwd_json(dc$article_config)
  validation <- jsonlite::read_json(article$validation_config, simplifyVector = FALSE)
  context$profile <- validation$fit_control$mti_ecm_dp_profile
  context$dp <- validation$fit_control$direct_dp
  # The configuration nests numerical settings under engine, not fit_control, in older studies.
  if (is.null(context$profile)) {
    find <- function(x, name) {
      if (!is.list(x)) return(NULL)
      if (!is.null(x[[name]])) return(x[[name]])
      for (z in x) { ans <- find(z, name); if (!is.null(ans)) return(ans) }
      NULL
    }
    context$profile <- find(validation, "mti_ecm_dp_profile")
    context$dp <- find(validation, "direct_dp")
  }
  mwd_require(!is.null(context$profile) && !is.null(context$dp), "Missing historical profile settings.")
  context$policy <- read.csv(article$policy_csv)
  context$scan <- read.csv("tables/tolerance_validation_article_scan_calibration.csv")
  context
}

mps_design <- function(cfg, context) {
  cells <- context$scan[rep(seq_len(nrow(context$scan)), each = length(context$laws)),
    c("n", "content", "tolerance_confidence", "retained_fraction")]
  cells$response_id <- rep(names(context$laws), nrow(context$scan))
  cells$cell_id <- seq_len(nrow(cells))
  mwd_require(nrow(cells) == cfg$expected_cells, "The prior screening design differs.")
  tasks <- cells[rep(seq_len(nrow(cells)), each = cfg$replications_per_cell), ]
  tasks$replication <- rep(seq_len(cfg$replications_per_cell), nrow(cells))
  tasks$dataset_id <- seq_len(nrow(tasks))
  tasks$data_seed <- as.integer(cfg$data_seed_base + 10000L * tasks$cell_id + tasks$replication)
  out <- tasks[rep(seq_len(nrow(tasks)), each = length(cfg$kappa_grid)), ]
  out$kappa <- rep(cfg$kappa_grid, nrow(tasks)); out$job_id <- seq_len(nrow(out))
  out <- out[order(out$replication, out$kappa, out$cell_id), ]
  rownames(out) <- NULL
  out
}

mps_fit <- function(task, cfg, context) {
  set.seed(task$data_seed); y <- context$laws[[task$response_id]]$r(task$n)
  spec <- mps_spec(y, task$kappa); z <- spec$z
  rule <- context$policy[context$policy$n == task$n & context$policy$content == task$content, ]
  mwd_require(nrow(rule) == 1L, "There must be one inherited probability cutoff per design.")
  menu <- rqrgibbs::rqr_mti_ecm_adaptive_profile_menu(z, task$content, task$tolerance_confidence,
    scan_target_content = task$retained_fraction, policy = "cell", policy_config = context$profile,
    calibration_rule = as.list(rule[1, ]))
  control <- context$profile$ecm_control
  control$max_iter <- cfg$ecm_max_iter; control$store_iteration_trace <- TRUE
  control$seed <- as.integer(cfg$chain_seed_base + 100L * task$job_id)
  dp_base <- context$dp$base
  # Transform the original fixed response prior along with the observations.
  base <- rqrgibbs::rqr_dp_base_normal((dp_base$mean - spec$center) / spec$scale, dp_base$sd / spec$scale)
  action <- rqrgibbs::rqr_mti_ecm_dp_profile_action(z, task$content, menu$posterior_confidence,
    dp_concentration = menu$dp_concentration, dp_base_measure = base, strict_bayes = TRUE,
    scan_target_content = task$retained_fraction, q_grid = menu$q_grid,
    tilt_grid_control = menu$tilt_grid_control, learning_rate = 1,
    beta_prior_obj = rqrgibbs::beta_prior("ridge", ridge = list(tau2 = task$kappa^2)),
    ecm_control = control, expand_if_empty = TRUE, return_candidate_traces = TRUE)
  if (!nrow(action$selected)) return(list(returned = FALSE, task = task, sample_sha256 = mwd_hash(y),
    reason = action$posterior_constraint_status, candidates = action$candidates))
  a <- action$selected
  q <- a$target_content; delta <- a$mean_tilt
  initial <- mwd_initial(z, a$lower, a$upper, task$kappa^2, 0)
  sampling <- list(n_burn = cfg$n_burn, n_mcmc = cfg$n_mcmc, learning_rate = 1,
    prior_variance = task$kappa^2, initial_midpoint_offsets_sd = initial$offsets,
    initial_halfwidth_multipliers = initial$radii)
  fits <- lapply(seq_len(cfg$chains), function(chain) {
    t <- data.frame(chain = chain, seed = as.integer(cfg$chain_seed_base + 100L * task$job_id + chain),
      q = q, delta = delta, ecm_lower = a$lower, ecm_upper = a$upper)
    tryCatch(mgp_slice_fit(z, t, sampling), error = function(e) list(error = conditionMessage(e)))
  })
  failed_chains <- vapply(fits, function(f) !is.null(f$error), FALSE)
  if (any(failed_chains)) return(list(returned = FALSE, task = task, sample_sha256 = mwd_hash(y),
    reason = paste("Incomplete Gibbs comparison:", paste(vapply(fits[failed_chains], `[[`, "", "error"), collapse = "; ")),
    selected_ecm = a, candidates = action$candidates, selected_ecm_trace = action$candidate_traces,
    partial_fits = fits))
  t <- data.frame(case_id = paste0("prior_", task$job_id), dataset_id = task$dataset_id, source = "simulation",
    response_id = task$response_id, n = task$n, content = task$content, q = q, delta = delta,
    ecm_lower = a$lower, ecm_upper = a$upper)
  policy <- mwd_json(mwd_json(cfg$diagnostic_config)$practical_config)$policy
  parameters <- mgp_summaries(fits, t, policy)
  means <- setNames(parameters$mean, parameters$variable)
  ecm <- c(a$lower, a$upper); gibbs <- means[c("lower", "upper")]
  coordinate <- c(mgp_conditional(z, a$upper, q, delta, precision = 1 / task$kappa^2)$mode,
    mgp_conditional(z, a$lower, q, delta, precision = 1 / task$kappa^2)$mode)
  objective <- function(a, b) mgp_objective(z, a, b, q, delta, precision = 1 / task$kappa^2)
  gains <- objective(ecm[1], ecm[2]) - c(objective(coordinate[1], ecm[2]), objective(ecm[1], coordinate[2]))
  endpoints <- rbind(ecm, gibbs) * spec$scale + spec$center
  cdf <- context$laws[[task$response_id]]$p
  actual <- cdf(endpoints[, 2]) - cdf(endpoints[, 1])
  rows <- cbind(task[rep(1, 2), ], method = c("ecm", "gibbs"), prior_mean = spec$center,
    prior_sd = spec$prior_sd, raw_learning_rate = spec$learning_rate, selected_q = q,
    selected_delta = delta * spec$scale, lower = endpoints[, 1], upper = endpoints[, 2],
    width = endpoints[, 2] - endpoints[, 1], width_in_training_sd = endpoints[, 2] / spec$scale - endpoints[, 1] / spec$scale,
    population_content = actual, attained = actual >= task$content, returned = TRUE,
    ecm_coordinate_gap = max(gains), ecm_coordinate_ok = max(gains) <= cfg$coordinate_gap_tolerance,
    ecm_flag = a$ecm_converged, ecm_iterations = a$ecm_iterations,
    gibbs_preferred = all(parameters$preferred_pass), max_rhat = max(parameters$rhat),
    min_bulk_ess = min(parameters$ess_bulk), min_tail_ess = min(parameters$ess_tail))
  list(returned = TRUE, task = task, sample_sha256 = mwd_hash(y), rows = rows,
    candidates = action$candidates, selected_ecm_trace = action$candidate_traces[action$candidate_traces$candidate_selected, ],
    fits = fits, parameters = parameters, prior = spec[c("center", "scale", "prior_sd", "learning_rate")])
}

mps_collect <- function(tasks, output, cfg, complete = FALSE) {
  paths <- file.path(output, "fits", sprintf("fit_%06d.rds", tasks$job_id))
  identity <- mwd_json(file.path(output, "manifest.json"))$identity_sha256
  rows <- failures <- list()
  for (j in which(file.exists(paths) & file.exists(paste0(paths, ".json")))) {
    r <- readRDS(paths[j]); receipt <- mwd_json(paste0(paths[j], ".json"))
    mwd_require(identical(receipt$sha256, mwd_file_hash(paths[j])) &&
      identical(receipt$identity_sha256, identity) &&
      identical(mwd_hash(r$task), mwd_hash(tasks[j, ])), "A prior screening result changed.")
    if (isTRUE(r$returned)) rows[[length(rows) + 1L]] <- r$rows else {
      failures[[length(failures) + 1L]] <- cbind(tasks[j, ], reason = r$reason)
      rows[[length(rows) + 1L]] <- cbind(tasks[rep(j, 2), ], method = c("ecm", "gibbs"),
        width = NA_real_, width_in_training_sd = NA_real_, population_content = NA_real_,
        attained = FALSE, returned = FALSE, ecm_coordinate_ok = FALSE, gibbs_preferred = FALSE)
    }
  }
  if (length(rows)) {
    data <- data.table::rbindlist(rows, fill = TRUE)
    mwd_write(data, file.path(output, "interval_results.csv"))
    cells <- data[, .(replications = .N, paired_fits_complete = sum(returned),
      successes = sum(attained), attainment = mean(attained),
      mean_width = if (any(returned)) mean(width[returned]) else NA_real_,
      width_q025 = quantile(width, .025, na.rm = TRUE), width_q975 = quantile(width, .975, na.rm = TRUE),
      standardized_width_median = median(width_in_training_sd, na.rm = TRUE),
      accurate_ecm = sum(ecm_coordinate_ok), adequate_gibbs = sum(gibbs_preferred)),
      by = .(cell_id, response_id, n, content, tolerance_confidence, kappa, method)]
    cells$lower_95 <- ifelse(cells$successes == 0, 0,
      qbeta(.025, cells$successes, cells$replications - cells$successes + 1))
    cells$upper_95 <- ifelse(cells$successes == cells$replications, 1,
      qbeta(.975, cells$successes + 1, cells$replications - cells$successes))
    mwd_write(cells, file.path(output, "cell_summary.csv"))
    summary <- cells[, .(evaluated_cells = .N, minimum_attainment = min(attainment),
      minimum_lower_95 = min(lower_95), cells_with_upper_bound_below_requested_p = sum(upper_95 < tolerance_confidence),
      coordinate_accuracy_rate = sum(accurate_ecm) / sum(replications),
      adequate_gibbs_rate = sum(adequate_gibbs) / sum(replications),
      median_standardized_width = median(standardized_width_median, na.rm = TRUE)), by = .(kappa, method)]
    mwd_write(summary, file.path(output, "prior_comparison.csv"))
  }
  if (length(failures)) mwd_write(data.table::rbindlist(failures), file.path(output, "failed_fits.csv"))
  mgfp_json_write(list(time = mgfp_time(), completed = sum(file.exists(paths)), expected = nrow(tasks),
    failed = length(failures), stage_complete = complete, selected_prior = NULL,
    ready_for_full_rerun = FALSE, new_tolerance_guarantee = FALSE), file.path(output, "status.json"))
}
