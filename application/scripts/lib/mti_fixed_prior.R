mfp_spec <- function(y, kappa, reference) {
  mwd_require(is.list(reference) && identical(reference$mode, "fixed") &&
    identical(reference$estimated_from_analysis_sample, FALSE), "A fixed independent reference is required.")
  m <- reference$center; s <- reference$scale
  mwd_require(length(m) == 1L && is.finite(m) && length(s) == 1L && is.finite(s) && s > 0 &&
    length(kappa) == 1L && is.finite(kappa) && kappa > 0 && nzchar(reference$source),
    "Invalid prior location, scale, multiplier, or reference source.")
  mwd_require(length(y) >= 2L && all(is.finite(y)), "Finite training observations are required.")
  prior <- list(center = m, scale = s, prior_sd = kappa * s, prior_variance = (kappa * s)^2,
    learning_rate = 1 / s^2, kappa = kappa, source = reference$source,
    data_adaptive_prior = FALSE)
  c(prior, list(z = (y - m) / s, prior_sha256 = mwd_hash(prior)))
}

# Compute a one-root objective increment without subtracting two cancelling totals.
mfp_increment <- function(y, roots, coordinate, proposed, q, delta, variance) {
  old <- roots[coordinate]; other <- roots[3L - coordinate]; step <- proposed - old
  e0 <- (y - old) * (y - other); e1 <- (y - proposed) * (y - other)
  de <- -step * (y - other); same <- (e0 < 0) == (e1 < 0)
  change <- numeric(length(y))
  change[same] <- (q - (e0[same] < 0)) * de[same]
  change[!same] <- e1[!same] * (q - (e1[!same] < 0)) - e0[!same] * (q - (e0[!same] < 0))
  terms <- c(change, -length(y) * q * delta * step, step * (old + proposed) / (2 * variance))
  original <- c(e0 * (q - (e0 < 0)), -q * delta * (roots[1] + roots[2] - 2 * y),
    roots^2 / (2 * variance))
  list(change = sum(terms), error_bound = 64 * .Machine$double.eps *
    max(.Machine$double.xmin, sum(abs(original)), sum(abs(terms))))
}

mfp_restore <- function(y, roots, q, delta, variance) {
  original <- roots; adjustments <- list()
  for (j in rep(1:2, 2L)) {
    mode <- mgp_conditional(y, roots[3L - j], q, delta, precision = 1 / variance)$mode
    if (!(mode %in% y) || mode == roots[j] ||
      abs(mode - roots[j]) > sqrt(.Machine$double.eps) * max(1, abs(roots))) next
    difference <- mfp_increment(y, roots, j, mode, q, delta, variance)
    mwd_require(difference$change <= difference$error_bound,
      "An exact conditional-knot proposal increases the posterior objective beyond numerical error.")
    adjustments[[length(adjustments) + 1L]] <- data.frame(coordinate = j, before = roots[j],
      after = mode, objective_change = difference$change, error_bound = difference$error_bound)
    roots[j] <- mode
  }
  list(roots = sort(roots), displacement = max(abs(roots - original)),
    adjustments = if (length(adjustments)) do.call(rbind, adjustments) else data.frame())
}

mfp_accuracy <- function(y, roots, q, delta, variance, controls) {
  modes <- c(mgp_conditional(y, roots[2], q, delta, precision = 1 / variance)$mode,
    mgp_conditional(y, roots[1], q, delta, precision = 1 / variance)$mode)
  improvements <- vapply(1:2, function(j)
    max(0, -mfp_increment(y, roots, j, modes[j], q, delta, variance)$change), 0)
  count <- sum(y >= roots[1] & y <= roots[2])
  alternate <- c(sum(y >= modes[1] & y <= roots[2]), sum(y >= roots[1] & y <= modes[2]))
  displacement <- max(abs(modes - roots))
  list(pass = max(improvements) <= controls$objective_tolerance &&
      displacement <= controls$endpoint_tolerance * max(1, diff(roots)) && all(alternate == count),
    objective_gap = max(improvements), endpoint_gap = displacement,
    count_stable = all(alternate == count), observed_count = count)
}

mfp_polish <- function(y, roots, q, delta, variance, controls) {
  trace <- list()
  for (cycle in seq_len(controls$conditional_mode_cycles)) {
    changes <- bounds <- numeric(2)
    for (j in 1:2) {
      proposed <- mgp_conditional(y, roots[3L - j], q, delta, precision = 1 / variance)$mode
      difference <- mfp_increment(y, roots, j, proposed, q, delta, variance)
      mwd_require(difference$change <= difference$error_bound,
        "An exact conditional-mode update increases the posterior objective.")
      roots[j] <- proposed; changes[j] <- difference$change; bounds[j] <- difference$error_bound
    }
    roots <- sort(roots)
    trace[[cycle]] <- data.frame(iteration = cycle, root1 = roots[1], root2 = roots[2], width = diff(roots),
      objective = mgp_objective(y, roots[1], roots[2], q, delta, precision = 1 / variance),
      stable_objective_change = sum(changes), objective_change_error_bound = sum(bounds),
      numerical_stage = "conditional_mode_refinement")
    accuracy <- mfp_accuracy(y, roots, q, delta, variance, controls)
    if (accuracy$pass) break
  }
  list(roots = roots, accuracy = accuracy, trace = do.call(rbind, trace), cycles = cycle)
}

mfp_refine <- function(action, y, kappa, controls, ecm_control) {
  d <- action$candidates; d$initial_lower <- d$lower; d$initial_upper <- d$upper
  d$initial_objective <- d$ecm_objective; d$initial_ecm_flag <- d$ecm_converged
  d$coordinate_gap <- NA_real_; d$endpoint_gap <- NA_real_; d$count_stable <- FALSE
  d$numerically_accurate <- FALSE; d$additional_iterations <- 0L
  d$conditional_mode_cycles <- 0L
  d$knot_adjustments <- 0L; d$knot_displacement <- 0
  traces <- adjustments <- vector("list", nrow(d))
  for (i in which(d$finite_interval)) {
    q <- d$target_content[i]; delta <- d$mean_tilt[i]; roots <- c(d$lower[i], d$upper[i])
    direct <- mgp_objective(y, roots[1], roots[2], q, delta, precision = 1 / kappa^2)
    mwd_require(abs(direct - d$ecm_objective[i]) <= 1e-7 * max(1, abs(direct)),
      "The initial ECM objective differs from the fixed-prior target.")
    trace <- action$candidate_traces[action$candidate_traces$candidate_index == d$candidate_index[i], ]
    trace$numerical_stage <- rep("initial_ecm", nrow(trace)); blocks <- list(trace)
    checks <- list(); budgets <- c(0L, controls$continuation_budgets)
    for (budget in budgets) {
      if (budget > 0) {
        control <- ecm_control; control$max_iter <- budget; control$multistart <- FALSE
        control$store_iteration_trace <- TRUE; control$fail_on_nonconvergence <- FALSE
        fit <- rqrgibbs::rqr_ecm_fit(y, matrix(1, length(y), 1L), q, mean_tilt = delta,
          learning_rate = 1, beta_prior_obj = rqrgibbs::beta_prior("ridge", ridge = list(tau2 = kappa^2)),
          ecm_control = control, init = list(beta1 = roots[1], beta2 = roots[2]))
        roots <- sort(c(fit$beta_root1, fit$beta_root2))
        extra <- fit$objective_trace
        extra$iteration <- extra$iteration + d$ecm_iterations[i] + d$additional_iterations[i]
        extra$candidate_index <- d$candidate_index[i]; extra$target_content <- q; extra$mean_tilt <- delta
        extra$numerical_stage <- paste0("continued_ecm_", budget)
        blocks[[length(blocks) + 1L]] <- extra
        d$additional_iterations[i] <- d$additional_iterations[i] + fit$iterations
        d$ecm_converged[i] <- isTRUE(fit$converged)
      }
      restored <- mfp_restore(y, roots, q, delta, kappa^2)
      d$knot_displacement[i] <- max(d$knot_displacement[i], restored$displacement)
      d$knot_adjustments[i] <- d$knot_adjustments[i] + nrow(restored$adjustments)
      if (nrow(restored$adjustments)) checks[[length(checks) + 1L]] <- restored$adjustments
      roots <- restored$roots
      accuracy <- mfp_accuracy(y, roots, q, delta, kappa^2, controls)
      if (accuracy$pass) break
    }
    if (!accuracy$pass) {
      polished <- mfp_polish(y, roots, q, delta, kappa^2, controls)
      roots <- polished$roots; accuracy <- polished$accuracy
      extra <- polished$trace
      extra$iteration <- extra$iteration + d$ecm_iterations[i] + d$additional_iterations[i]
      extra$candidate_index <- d$candidate_index[i]; extra$target_content <- q; extra$mean_tilt <- delta
      blocks[[length(blocks) + 1L]] <- extra
      d$conditional_mode_cycles[i] <- polished$cycles
      if (!accuracy$pass) {
        restored <- mfp_restore(y, roots, q, delta, kappa^2)
        d$knot_displacement[i] <- max(d$knot_displacement[i], restored$displacement)
        d$knot_adjustments[i] <- d$knot_adjustments[i] + nrow(restored$adjustments)
        if (nrow(restored$adjustments)) checks[[length(checks) + 1L]] <- restored$adjustments
        roots <- restored$roots
        accuracy <- mfp_accuracy(y, roots, q, delta, kappa^2, controls)
        if (nrow(restored$adjustments)) blocks[[length(blocks) + 1L]] <- data.frame(
          iteration = d$ecm_iterations[i] + d$additional_iterations[i] + polished$cycles,
          root1 = roots[1], root2 = roots[2], width = diff(roots),
          objective = mgp_objective(y, roots[1], roots[2], q, delta, precision = 1 / kappa^2),
          candidate_index = d$candidate_index[i], target_content = q, mean_tilt = delta,
          numerical_stage = "final_conditional_knot_representation")
      }
    }
    d$lower[i] <- roots[1]; d$upper[i] <- roots[2]; d$width[i] <- diff(roots)
    d$ecm_objective[i] <- mgp_objective(y, roots[1], roots[2], q, delta, precision = 1 / kappa^2)
    d$observed_count[i] <- accuracy$observed_count; d$retained_fraction[i] <- accuracy$observed_count / length(y)
    d$coordinate_gap[i] <- accuracy$objective_gap; d$endpoint_gap[i] <- accuracy$endpoint_gap
    d$count_stable[i] <- accuracy$count_stable; d$numerically_accurate[i] <- accuracy$pass
    traces[[i]] <- data.table::rbindlist(blocks, fill = TRUE)
    adjustments[[i]] <- if (length(checks)) data.table::rbindlist(checks) else data.frame()
  }
  p <- rqrgibbs::rqr_dp_content_probability(action$posterior_distribution_fit,
    d$lower[d$finite_interval], d$upper[d$finite_interval], action$content)
  for (name in c("posterior_content_probability", "posterior_mean_content", "posterior_var_content", "base_mass"))
    d[[name]][d$finite_interval] <- p[[name]]
  d$posterior_constraint_satisfied <- is.finite(d$posterior_content_probability) &
    d$posterior_content_probability >= action$posterior_confidence
  eligible <- d[d$finite_interval & d$numerically_accurate & d$posterior_constraint_satisfied, ]
  if (nrow(eligible)) eligible <- eligible[order(eligible$width, eligible$target_content,
    abs(eligible$mean_tilt - eligible$central_tilt), eligible$lower, eligible$candidate_index), ]
  list(candidates = d, selected = head(eligible, 1L), traces = traces, adjustments = adjustments,
    unresolved_candidates = sum(!d$finite_interval | !d$numerically_accurate))
}

mfp_fit_data <- function(y, content, confidence, scan_q, kappa, reference, profile, dp,
                         rule, controls, sampling, seed, policy, sample_gibbs = TRUE) {
  spec <- mfp_spec(y, kappa, reference); z <- spec$z
  menu <- rqrgibbs::rqr_mti_ecm_adaptive_profile_menu(z, content, confidence,
    scan_target_content = scan_q, policy = "cell", policy_config = profile, calibration_rule = rule)
  ec <- profile$ecm_control; ec$max_iter <- controls$initial_iterations
  ec$store_iteration_trace <- TRUE; ec$seed <- seed; ec$multistart <- FALSE
  base <- rqrgibbs::rqr_dp_base_normal((dp$base$mean - spec$center) / spec$scale, dp$base$sd / spec$scale)
  action <- rqrgibbs::rqr_mti_ecm_dp_profile_action(z, content, menu$posterior_confidence,
    dp_concentration = menu$dp_concentration, dp_base_measure = base, strict_bayes = TRUE,
    scan_target_content = scan_q, q_grid = menu$q_grid, tilt_grid_control = menu$tilt_grid_control,
    learning_rate = 1, beta_prior_obj = rqrgibbs::beta_prior("ridge", ridge = list(tau2 = kappa^2)),
    ecm_control = ec, expand_if_empty = FALSE, return_candidate_traces = TRUE)
  refined <- tryCatch(mfp_refine(action, z, kappa, controls, ec), error = function(e) e)
  if (inherits(refined, "error")) return(list(returned = FALSE, reason = conditionMessage(refined),
    prior = spec[setdiff(names(spec), "z")], sample_sha256 = mwd_hash(y),
    candidates = action$candidates, initial_candidate_traces = action$candidate_traces,
    failed_call = deparse(conditionCall(refined))))
  common <- list(prior = spec[setdiff(names(spec), "z")], sample_sha256 = mwd_hash(y),
    candidates = refined$candidates, unresolved_candidates = refined$unresolved_candidates,
    target_selection = "training_ecm_profile_then_fixed_target_gibbs", reference = reference,
    dp_prior = list(concentration = menu$dp_concentration, raw_base = dp$base,
      estimated_from_analysis_sample = FALSE))
  if (refined$unresolved_candidates > 0L) return(c(common,
    list(returned = FALSE, reason = "unresolved_profile_candidates",
      unresolved_traces = refined$traces[which(!refined$candidates$numerically_accurate)])))
  if (!nrow(refined$selected)) return(c(common, list(returned = FALSE, reason = "no_accurate_eligible_candidate")))
  selected <- refined$selected; i <- match(selected$candidate_index, refined$candidates$candidate_index)
  trace <- refined$traces[[i]]
  # Original trajectories and the final checked endpoint representation stay distinct.
  trace$candidate_final_lower <- selected$lower; trace$candidate_final_upper <- selected$upper
  trace$candidate_final_probability <- selected$posterior_content_probability
  trace$posterior_content_probability <- NULL; trace$posterior_constraint_satisfied <- NULL
  ecm <- c(selected$lower, selected$upper); q <- selected$target_content; delta <- selected$mean_tilt
  output <- c(common, list(selected = selected, selected_ecm_trace = trace,
    selected_knot_adjustments = refined$adjustments[[i]], q = q, delta_reference = delta))
  if (!sample_gibbs) return(c(output, list(returned = TRUE, reference_endpoints = rbind(ecm = ecm))))
  initial <- mwd_initial(z, ecm[1], ecm[2], kappa^2, 0)
  sc <- c(sampling, list(learning_rate = 1, prior_variance = kappa^2,
    initial_midpoint_offsets_sd = initial$offsets, initial_halfwidth_multipliers = initial$radii))
  history <- list()
  for (keep in unique(pmin(sampling$maximum_retained, sampling$n_mcmc * c(1L, 2L, 4L)))) {
    fits <- lapply(seq_len(sampling$chains), function(chain) {
      task <- data.frame(chain = chain, seed = as.integer(seed + chain), q = q, delta = delta,
        ecm_lower = ecm[1], ecm_upper = ecm[2])
      tryCatch(mgp_slice_fit(z, task, sc, n_keep = keep), error = function(e) list(error = conditionMessage(e)))
    })
    if (any(vapply(fits, function(f) !is.null(f$error), FALSE)))
      return(c(output, list(returned = FALSE, reason = "incomplete_gibbs_sampling", partial_fits = fits)))
    task <- data.frame(case_id = "fixed_prior", dataset_id = 0L, source = "fixed_prior",
      response_id = "not_supplied_to_fitter", n = length(y), content = content,
      q = q, delta = delta, ecm_lower = ecm[1], ecm_upper = ecm[2])
    parameters <- mgp_summaries(fits, task, policy)
    adequate <- all(parameters$preferred_pass & parameters$mean_precision)
    history[[length(history) + 1L]] <- data.frame(retained = keep, adequate = adequate,
      max_rhat = max(parameters$rhat), min_bulk_ess = min(parameters$ess_bulk),
      min_tail_ess = min(parameters$ess_tail), max_mean_mcse = max(parameters$mcse_mean))
    if (adequate) break
  }
  means <- setNames(parameters$mean, parameters$variable)
  endpoints <- rbind(ecm = ecm, gibbs = means[c("lower", "upper")])
  prob <- rqrgibbs::rqr_dp_content_probability(action$posterior_distribution_fit,
    endpoints[, 1], endpoints[, 2], content)
  raw <- endpoints * spec$scale + spec$center
  rows <- data.frame(method = c("ecm", "gibbs"), lower = raw[, 1], upper = raw[, 2],
    width = raw[, 2] - raw[, 1], prior_mean = spec$center, prior_sd = spec$prior_sd,
    raw_learning_rate = spec$learning_rate, data_adaptive_prior = FALSE,
    selected_q = q, selected_delta = delta * spec$scale,
    dp_content_probability = prob$posterior_content_probability,
    ecm_coordinate_gap = selected$coordinate_gap, ecm_coordinate_ok = selected$numerically_accurate,
    ecm_flag = selected$ecm_converged, ecm_iterations = selected$ecm_iterations + selected$additional_iterations,
    conditional_mode_cycles = selected$conditional_mode_cycles,
    gibbs_preferred = all(parameters$preferred_pass), gibbs_mean_precision = all(parameters$mean_precision),
    gibbs_adequate = adequate, max_rhat = max(parameters$rhat), min_bulk_ess = min(parameters$ess_bulk),
    min_tail_ess = min(parameters$ess_tail), retained_per_chain = keep, returned = TRUE)
  c(output, list(returned = TRUE, rows = rows, fits = fits, parameters_reference_units = parameters,
    sampling_history = do.call(rbind, history)))
}

mfp_evaluate <- function(result, cdf, content) {
  if (!isTRUE(result$returned)) return(result)
  result$rows$population_content <- cdf(result$rows$upper) - cdf(result$rows$lower)
  result$rows$attained <- result$rows$population_content >= content
  result
}

mfp_comparators <- function(y, content, confidence) {
  ym <- pta_fit_young_mathew(y, content, confidence)
  wilks_probability <- stats::pbeta(content, length(y) - 1, 2, lower.tail = FALSE)
  feasible <- wilks_probability >= confidence
  data.frame(method = c("young_mathew", "wilks"), returned = c(isTRUE(ym$returned), feasible),
    lower = c(ym$lower, if (feasible) min(y) else NA_real_),
    upper = c(ym$upper, if (feasible) max(y) else NA_real_),
    width = c(ym$width, if (feasible) diff(range(y)) else NA_real_),
    failure_reason = c(ym$failure_reason, if (feasible) "" else "finite_sample_minmax_bound_below_requested_confidence"))
}

mfp_design <- function(stage, cfg, context) {
  spec <- cfg$stages[[stage]]
  mwd_require(!is.null(spec), "Unknown fixed-prior study stage.")
  d <- mps_design(list(expected_cells = 72L, replications_per_cell = spec$replications,
    data_seed_base = spec$data_seed_base, kappa_grid = spec$kappa), context)
  d$stage <- stage; d
}

mfp_check_designs <- function(cfg, context) {
  designs <- lapply(names(cfg$stages), function(stage) mfp_design(stage, cfg, context))
  names(designs) <- names(cfg$stages)
  streams <- lapply(names(designs), function(stage) {
    d <- designs[[stage]]
    mwd_require(!anyDuplicated(d$job_id) && !anyDuplicated(d[c("data_seed", "kappa")]),
      "Duplicate fixed-prior task or data/prior pair.")
    chain <- as.vector(outer(cfg$stages[[stage]]$chain_seed_base + 100 * d$job_id,
      seq_len(cfg$sampling$chains), "+"))
    mwd_require(all(is.finite(chain) & chain > 0 & chain <= .Machine$integer.max) &&
      !anyDuplicated(chain), "Invalid or duplicate Gibbs chain seeds.")
    list(data = unique(d$data_seed), chain = chain)
  })
  names(streams) <- names(designs)
  for (pair in utils::combn(names(streams), 2L, simplify = FALSE)) {
    a <- streams[[pair[1]]]; b <- streams[[pair[2]]]
    mwd_require(!any(a$data %in% b$data) && !any(a$chain %in% b$chain),
      "Independent stage data or Gibbs chain seeds overlap.")
  }
  app <- mfp_application_design(cfg, context)
  app_seeds <- as.vector(outer(1670000000 + 100 * app$job_id, seq_len(cfg$sampling$chains), "+"))
  for (s in streams) mwd_require(!any(app_seeds %in% s$chain), "Application chain seeds overlap.")
  invisible(designs)
}

mfp_job <- function(task, cfg, context) {
  set.seed(task$data_seed); y <- context$laws[[task$response_id]]$r(task$n)
  benchmarks <- mfp_comparators(y, task$content, task$tolerance_confidence)
  benchmarks$population_content <- context$laws[[task$response_id]]$p(benchmarks$upper) -
    context$laws[[task$response_id]]$p(benchmarks$lower)
  benchmarks$attained <- benchmarks$returned & benchmarks$population_content >= task$content
  rule <- context$policy[context$policy$n == task$n & context$policy$content == task$content, ]
  mwd_require(nrow(rule) == 1L, "Missing fixed-design content-probability rule.")
  result <- mfp_fit_data(y, task$content, task$tolerance_confidence, task$retained_fraction,
    task$kappa, cfg$simulation_reference, context$profile, context$dp, as.list(rule[1, ]),
    cfg$numerical, cfg$sampling, as.integer(cfg$stages[[task$stage]]$chain_seed_base + 100L * task$job_id),
    mwd_json(mwd_json(cfg$diagnostic_config)$practical_config)$policy)
  result$task <- task
  result$comparators <- cbind(task[rep(1, nrow(benchmarks)), ], benchmarks)
  if (isTRUE(result$returned)) {
    result <- mfp_evaluate(result, context$laws[[task$response_id]]$p, task$content)
    result$rows <- cbind(task[rep(1, nrow(result$rows)), ], result$rows)
  }
  result
}

mfp_collect <- function(tasks, directory, identity, cfg) {
  rows <- failures <- benchmarks <- list(); unresolved <- 0L
  for (j in seq_len(nrow(tasks))) {
    path <- file.path(directory, "fits", sprintf("fit_%06d.rds", tasks$job_id[j]))
    r <- readRDS(path); receipt <- mwd_json(paste0(path, ".json"))
    mwd_require(identical(receipt$sha256, mwd_file_hash(path)) &&
      identical(receipt$identity, identity) && identical(mwd_hash(r$task), mwd_hash(tasks[j, ])),
      "A fixed-prior result or task identity changed.")
    unresolved <- unresolved + if (is.null(r$unresolved_candidates)) 0L else r$unresolved_candidates
    if (!is.null(r$comparators)) benchmarks[[length(benchmarks) + 1L]] <- r$comparators
    if (isTRUE(r$returned)) rows[[j]] <- r$rows else {
      failures[[length(failures) + 1L]] <- cbind(tasks[j, ], reason = r$reason)
      rows[[j]] <- cbind(tasks[rep(j, 2), ], method = c("ecm", "gibbs"), returned = FALSE,
        attained = FALSE, width = NA_real_, ecm_coordinate_ok = FALSE, gibbs_adequate = FALSE)
    }
  }
  d <- data.table::rbindlist(rows, fill = TRUE)
  mwd_write(d, file.path(directory, "interval_results.csv"))
  if (length(benchmarks)) mwd_write(data.table::rbindlist(benchmarks), file.path(directory, "comparator_results.csv"))
  cells <- d[, .(attempts = .N, returned = sum(returned), successes = sum(attained),
    attainment = mean(attained), mean_width = mean(width, na.rm = TRUE),
    width_q025 = quantile(width, .025, na.rm = TRUE), width_q975 = quantile(width, .975, na.rm = TRUE)),
    by = .(response_id, n, content, tolerance_confidence, kappa, method)]
  cells[, lower_one_sided_95 := ifelse(successes == 0, 0, qbeta(.05, successes, attempts - successes + 1))]
  cells[, upper_one_sided_95 := ifelse(successes == attempts, 1, qbeta(.95, successes + 1, attempts - successes))]
  primary_cells <- cells[kappa == cfg$primary_kappa]
  family_size <- nrow(primary_cells)
  cells[, simultaneous_upper_95 := ifelse(successes == attempts, 1,
    qbeta(1 - .05 / family_size, successes + 1, attempts - successes))]
  mwd_write(cells, file.path(directory, "cell_summary.csv"))
  if (length(failures)) mwd_write(data.table::rbindlist(failures), file.path(directory, "failed_fits.csv"))
  primary <- d[kappa == cfg$primary_kappa]
  numerical_pass <- nrow(primary) > 0L && isTRUE(all(primary$returned & primary$ecm_coordinate_ok & primary$gibbs_adequate))
  lower_pass <- all(cells[kappa == cfg$primary_kappa, lower_one_sided_95 >= tolerance_confidence])
  clear_underattainment <- any(cells[kappa == cfg$primary_kappa, simultaneous_upper_95 < tolerance_confidence])
  status <- list(completed = nrow(tasks), expected = nrow(tasks), failed_pairs = length(failures),
    unresolved_candidates = unresolved, numerical_pass = numerical_pass,
    pointwise_calibration_lower_bounds_pass = lower_pass,
    calibration_clearly_underattains = clear_underattainment,
    ready_for_final_validation = numerical_pass && !clear_underattainment,
    simultaneous_tolerance_guarantee = FALSE, prior_uses_analysis_data = FALSE,
    article_replaced = FALSE, time = mgfp_time())
  mgfp_json_write(status, file.path(directory, "status.json")); status
}

mfp_application_design <- function(cfg, context) {
  keys <- names(context$training_ids)
  d <- expand.grid(training_key = keys, response_id = c("fct_tensile", "tbl_rsd_weight"),
    stringsAsFactors = FALSE)
  d$n <- vapply(d$training_key, function(k) length(context$training_ids[[k]]), 0L)
  d$content <- .9; d$tolerance_confidence <- .95; d$kappa <- cfg$primary_kappa
  d$job_id <- seq_len(nrow(d)); d$stage <- "application"; d
}

mfp_application_job <- function(task, cfg, context) {
  ids <- context$training_ids[[task$training_key]]
  train <- context$observed[[task$response_id]][match(ids, context$observed$.analysis_row)]
  rule <- context$policy[context$policy$n == task$n & context$policy$content == task$content, ]
  scan <- context$scan[context$scan$n == task$n & context$scan$content == task$content, ]
  mwd_require(nrow(rule) == 1L && nrow(scan) == 1L, "Application design lacks a fixed probability rule.")
  r <- mfp_fit_data(train, task$content, task$tolerance_confidence, scan$retained_fraction,
    task$kappa, cfg$application_reference, context$profile, context$dp, as.list(rule[1, ]),
    cfg$numerical, cfg$sampling, as.integer(1670000000L + 100L * task$job_id),
    mwd_json(mwd_json(cfg$diagnostic_config)$practical_config)$policy)
  r$task <- task
  if (isTRUE(r$returned)) {
    heldout <- context$observed[[task$response_id]][!context$observed$.analysis_row %in% ids]
    r$rows$heldout_empirical_content <- vapply(seq_len(nrow(r$rows)), function(j)
      mean(heldout >= r$rows$lower[j] & heldout <= r$rows$upper[j]), 0)
    r$rows$heldout_n <- length(heldout)
    r$rows <- cbind(task[rep(1, nrow(r$rows)), ], r$rows)
  }
  benchmarks <- mfp_comparators(train, task$content, task$tolerance_confidence)
  heldout <- context$observed[[task$response_id]][!context$observed$.analysis_row %in% ids]
  benchmarks$heldout_empirical_content <- vapply(seq_len(nrow(benchmarks)), function(j)
    if (benchmarks$returned[j]) mean(heldout >= benchmarks$lower[j] & heldout <= benchmarks$upper[j]) else NA_real_, 0)
  r$comparators <- cbind(task[rep(1, nrow(benchmarks)), ], benchmarks)
  r
}

mfp_collect_application <- function(tasks, directory, identity) {
  rows <- failures <- benchmarks <- list()
  for (j in seq_len(nrow(tasks))) {
    path <- file.path(directory, "fits", sprintf("fit_%06d.rds", tasks$job_id[j]))
    r <- readRDS(path); receipt <- mwd_json(paste0(path, ".json"))
    mwd_require(identical(receipt$identity, identity) && identical(receipt$sha256, mwd_file_hash(path)) &&
      identical(mwd_hash(r$task), mwd_hash(tasks[j, ])), "An application fit identity differs.")
    if (isTRUE(r$returned)) rows[[length(rows) + 1L]] <- r$rows else {
      failures[[length(failures) + 1L]] <- cbind(tasks[j, ], reason = r$reason)
      rows[[length(rows) + 1L]] <- cbind(tasks[rep(j, 2), ], method = c("ecm", "gibbs"),
        returned = FALSE, lower = NA_real_, upper = NA_real_, width = NA_real_,
        heldout_empirical_content = NA_real_, ecm_coordinate_ok = FALSE, gibbs_adequate = FALSE)
    }
    if (!is.null(r$comparators)) benchmarks[[length(benchmarks) + 1L]] <- r$comparators
  }
  if (length(rows)) mwd_write(data.table::rbindlist(rows, fill = TRUE), file.path(directory, "interval_results.csv"))
  if (length(failures)) mwd_write(data.table::rbindlist(failures), file.path(directory, "failed_fits.csv"))
  if (length(benchmarks)) mwd_write(data.table::rbindlist(benchmarks), file.path(directory, "comparator_results.csv"))
  numerical_pass <- !length(failures) && length(rows) == nrow(tasks) &&
    isTRUE(all(vapply(rows, function(x) all(x$ecm_coordinate_ok & x$gibbs_adequate), FALSE)))
  status <- list(completed = nrow(tasks), expected = nrow(tasks), failed_pairs = length(failures),
    numerical_pass = numerical_pass,
    heldout_results_are_descriptive = TRUE, prior_uses_analysis_data = FALSE,
    new_tolerance_guarantee = FALSE, article_replaced = FALSE, time = mgfp_time())
  mgfp_json_write(status, file.path(directory, "status.json")); status
}

mfp_review <- function(directory, cfg) {
  d <- data.table::fread(file.path(directory, "interval_results.csv"))
  path <- file.path(directory, "comparator_results.csv")
  if (file.exists(path)) d <- data.table::rbindlist(list(d, data.table::fread(path)), fill = TRUE)
  d <- d[kappa == cfg$primary_kappa]
  if (!"attained" %in% names(d)) return(invisible(NULL))
  d[is.na(attained), attained := FALSE]
  cells <- d[, .(attempts = .N, returned = sum(returned), successes = sum(attained),
    attainment = mean(attained), mean_width = mean(width, na.rm = TRUE),
    width_q025 = quantile(width, .025, na.rm = TRUE), width_q975 = quantile(width, .975, na.rm = TRUE)),
    by = .(response_id, n, content, tolerance_confidence, method)]
  cells[, lower_95 := ifelse(successes == 0, 0, qbeta(.025, successes, attempts - successes + 1))]
  cells[, upper_95 := ifelse(successes == attempts, 1, qbeta(.975, successes + 1, attempts - successes))]
  mwd_write(cells, file.path(directory, "comparison_summary.csv"))
  labels <- c(ecm = "MTI-ECM", gibbs = "MTI-Gibbs", young_mathew = "Young--Mathew", wilks = "Wilks")
  colors <- c(ecm = "#A04416", gibbs = "#32734D", young_mathew = "#286A9B", wilks = "#000000")
  grDevices::pdf(file.path(directory, "study_review.pdf"), width = 10, height = 8, onefile = TRUE)
  on.exit(grDevices::dev.off())
  designs <- unique(cells[, .(n, content, tolerance_confidence)])
  for (j in seq_len(nrow(designs))) {
    z <- cells[n == designs$n[j] & content == designs$content[j]]
    laws <- unique(z$response_id); methods <- names(labels)
    y <- match(z$response_id, laws) * 5 + match(z$method, methods) - 2.5
    for (kind in c("attainment", "width")) {
      graphics::par(mar = c(5, 12, 4, 2))
      lower <- if (kind == "width") z$width_q025 else z$lower_95
      upper <- if (kind == "width") z$width_q975 else z$upper_95
      center <- if (kind == "width") z$mean_width else z$attainment
      xr <- if (kind == "width") range(c(0, lower, upper), finite = TRUE) else c(0, 1)
      if (diff(xr) == 0) xr <- c(0, 1)
      graphics::plot(NA, xlim = xr, ylim = c(2, length(laws) * 5 + 2), yaxt = "n", ylab = "",
        xlab = if (kind == "width") "Interval width: 2.5th to 97.5th percentiles; crosses show means" else
          "Content attainment probability: pointwise 95% binomial confidence intervals",
        main = sprintf("n = %d; content = %.2f; confidence = %.2f", designs$n[j], designs$content[j], designs$tolerance_confidence[j]))
      graphics::axis(2, at = seq_along(laws) * 5, labels = laws, las = 1, tick = FALSE)
      if (kind == "attainment") graphics::abline(v = designs$tolerance_confidence[j], lty = 2, col = "#777777")
      graphics::segments(lower, y, upper, y, col = colors[z$method], lwd = 2)
      graphics::points(center, y, pch = if (kind == "width") 4 else 16, col = colors[z$method])
      graphics::legend("bottomright", legend = labels, col = colors, lty = 1, bty = "n", cex = .8)
    }
  }
  invisible(cells)
}

mfp_storage_estimate <- function(bytes, pending) {
  mwd_require(length(bytes) > 0L && all(is.finite(bytes) & bytes >= 0),
    "Completed development fits are required for the storage estimate.")
  uncertainty <- if (length(bytes) > 1L) 3 * stats::sd(bytes) / sqrt(length(bytes)) else 0
  pending * (mean(bytes) + uncertainty) * 1.25
}

mfp_capacity <- function(directory, tasks, cfg) {
  pending <- sum(!file.exists(file.path(directory, "fits", sprintf("fit_%06d.rds.json", tasks$job_id))))
  paths <- list.files(file.path(cfg$output_root, "development", "fits"), pattern = "\\.rds$", full.names = TRUE)
  bytes <- if (length(paths)) file.info(paths)$size else numeric()
  expected <- mfp_storage_estimate(bytes, pending)
  expected_gib <- expected / 1024^3
  free <- mps_disk_check(directory, cfg$disk_reserve_gib)
  status <- list(pending = pending, reference_files = length(bytes),
    estimated_additional_gib = expected_gib, free_gib = free, reserve_gib = cfg$disk_reserve_gib,
    feasible = is.finite(expected_gib) && free - expected_gib >= cfg$disk_reserve_gib)
  mgfp_json_write(status, file.path(directory, "storage_check.json"))
  mwd_require(status$feasible, "Predicted trace storage would violate the free-disk reserve.")
  invisible(status)
}
