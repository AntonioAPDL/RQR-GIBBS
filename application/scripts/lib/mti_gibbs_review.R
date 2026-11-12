mgr_agreement <- function(reference, comparison, label, reliable) {
  mpr_require(identical(as.character(reference$variable), as.character(comparison$variable)),
    "Sampler comparison variables differ.")
  data.frame(case_id = reference$case_id, dataset_id = reference$dataset_id,
    variable = reference$variable, reference_sampler = label, reference_reliable = reliable,
    reference_mean = reference$mean, marginal_mean = comparison$mean,
    combined_mcse = sqrt(reference$mcse_mean^2 + comparison$mcse_mean^2),
    standardized_difference = (comparison$mean - reference$mean) /
      sqrt(reference$mcse_mean^2 + comparison$mcse_mean^2))
}

mgr_geometry <- function(y, q, delta) {
  minimum <- function(tau) {
    r <- y - unname(quantile(y, tau, type = 1))
    sum(r * (tau - (r < 0)))
  }
  low <- -minimum(q) / (length(y) * q)
  high <- minimum(1 - q) / (length(y) * q)
  slopes <- c(length(y) * q * (delta - low), length(y) * q * (high - delta))
  # Verify the exact outward slopes independently at endpoints outside the sample.
  unit <- max(1, sd(y), diff(range(y)))
  a <- min(y) - c(10, 20) * unit; b <- max(y) + c(10, 20) * unit
  loss <- function(a, b) mgp_objective(y, a, b, q, delta, precision = 0)
  fixed_b <- unname(quantile(y, q, type = 1))
  fixed_a <- unname(quantile(y, 1 - q, type = 1))
  observed <- c((loss(a[2], fixed_b) - loss(a[1], fixed_b)) / (a[1] - a[2]),
    (loss(fixed_a, b[2]) - loss(fixed_a, b[1])) / (b[2] - b[1]))
  mpr_require(max(abs(observed - slopes)) < 1e-7 * max(1, abs(slopes)), "Outward slope check failed.")
  data.frame(target_class = mgp_target_class(y, q, delta),
    tilt_lower_limit = low, tilt_upper_limit = high,
    lower_ray_slope = slopes[1], upper_ray_slope = slopes[2],
    sample_min = min(y), sample_max = max(y), sample_sd = sd(y))
}

mgr_reweight <- function(fits, base, factors, minimum_ess) {
  a <- mgp_array(fits); n <- dim(a)[1]; width <- a[, , "width"]
  squared <- a[, , "lower"]^2 + a[, , "upper"]^2
  do.call(rbind, lapply(factors, function(factor) {
    stopifnot(is.finite(factor), factor > 0, factor <= 1)
    logw <- -.5 * (1 / (base$prior_variance * factor) - 1 / base$prior_variance) * squared
    w <- exp(logw - max(logw)); normalized <- w / sum(w)
    estimate <- sum(normalized * width)
    deviation <- sqrt(sum(normalized * (width - estimate)^2))
    mcse <- posterior::mcse_mean(w * (width - estimate)) / mean(w)
    weight_ess <- 1 / sum(normalized^2)
    data.frame(prior_variance_factor = factor, prior_sd = sqrt(base$prior_variance * factor),
      width_mean = estimate, width_sd = deviation, width_mean_mcse = mcse,
      kish_weight_ess = weight_ess,
      sensitivity_supported = is.finite(mcse) && weight_ess >= minimum_ess && mcse <= .05 * deviation)
  }))
}

mgr_read <- function(directory, task, n_mcmc, y, base, count, analysis_identity = NULL) {
  path <- mgc_chain_path(directory, task$job_id)
  receipt <- mgc_receipt(path, task, list(n_mcmc = n_mcmc), verify = TRUE)
  if (!is.null(analysis_identity)) mpr_require(identical(receipt$analysis_identity,
    mgc_hash(analysis_identity)), "Chain analysis identity differs.")
  fit <- readRDS(path)
  mpr_require(identical(fit$sample_sha256, mgc_hash(y)), "Reconstructed observations differ.")
  indices <- unique(round(seq(1, n_mcmc, length.out = min(count, n_mcmc))))
  expected <- mgp_objective(y, fit$draws[indices, 1], fit$draws[indices, 2], task$q, task$delta,
    base$learning_rate, 1 / base$prior_variance)
  discrepancy <- max(abs(expected - fit$retained_objective[indices]))
  mpr_require(discrepancy <= 1e-8 * max(1, abs(expected)), "Independent objective check failed.")
  list(fit = fit, audit = data.frame(dataset_id = task$dataset_id, case_id = task$case_id,
    job_id = task$job_id, chain = task$chain, elapsed_seconds = fit$elapsed_seconds,
    sampler = if (is.null(fit$sampler)) "gaussian_gig_original" else fit$sampler,
    draws = n_mcmc, repairs = fit$repair_count, checked_objectives = length(indices),
    maximum_objective_error = discrepancy, sha256 = receipt$sha256))
}

mgr_compare_saved_summary <- function(actual, saved) {
  columns <- c("mean", "median", "sd", "rhat", "ess_bulk", "ess_tail", "mcse_mean",
    "lower_025", "upper_975")
  saved <- saved[match(actual$variable, saved$variable), ]
  for (column in columns) mpr_require(isTRUE(all.equal(actual[[column]], saved[[column]],
    tolerance = 1e-8, check.attributes = FALSE)), paste("Saved summary differs:", column))
  invisible(TRUE)
}

mgr_case <- function(s, cfg, practical, base, directory, parent, parent_jobs, extension_jobs,
                     saved_summary, original_summary, context, identity) {
  task <- s[1, ]; y <- mgc_sample(task, context)
  records <- lapply(seq_len(nrow(s)), function(i)
    mgr_read(file.path(directory, "slice"), s[i, , drop = FALSE], base$n_mcmc, y, base,
      cfg$objective_checks_per_chain, identity))
  fits <- lapply(records, "[[", "fit")
  mpr_require(all(vapply(fits, function(f) identical(f$sampler, practical$sampler), logical(1))),
    "Unexpected marginal sampler.")
  d <- mgp_summaries(fits, task, practical$policy)
  mgr_compare_saved_summary(d, saved_summary[saved_summary$dataset_id == task$dataset_id, ])
  geometry <- mgr_geometry(y, task$q, task$delta)
  mode <- rqrgibbs::rqr_mti_intercept_fit(y, task$q, task$delta,
    learning_rate = base$learning_rate, prior_precision = 1 / base$prior_variance, backend = "cpp")
  mpr_require(isTRUE(mode$converged) && is.finite(mode$relative_objective_gap),
    "Independent penalized-loss minimum was not resolved.")
  mode_objective <- mgp_objective(y, mode$lower, mode$upper, task$q, task$delta,
    base$learning_rate, 1 / base$prior_variance)
  mpr_require(abs(mode$objective - mode_objective) <= 1e-7 * max(1, abs(mode_objective)),
    "Independent minimum objective differs.")
  ecm_objective <- mgp_objective(y, task$ecm_lower, task$ecm_upper, task$q, task$delta,
    base$learning_rate, 1 / base$prior_variance)
  mpr_require(mode_objective <= ecm_objective + 1e-7 * max(1, abs(ecm_objective)),
    "The minimum reference has larger objective than the archived ECM iterate.")
  w <- d[d$variable == "width", ]
  primary <- d[d$variable %in% c("lower", "upper", "width"), ]
  dataset <- data.frame(task[, c("dataset_id", "case_id", "source", "response_id", "n", "content", "q", "delta",
    "diagnostic_role", "ecm_lower", "ecm_upper")], geometry,
    ecm_width = task$ecm_upper - task$ecm_lower, ecm_objective = ecm_objective,
    minimum_lower = mode$lower, minimum_upper = mode$upper, minimum_width = mode$width,
    minimum_objective = mode_objective, reference_relative_objective_gap = mode$relative_objective_gap,
    ecm_objective_gap = ecm_objective - mode_objective,
    ecm_relative_objective_gap = (ecm_objective - mode_objective) / max(1, abs(ecm_objective)),
    posterior_width_mean = w$mean, posterior_width_median = w$median,
    posterior_width_q025 = w$lower_025, posterior_width_q975 = w$upper_975,
    width_mean_ecm_ratio = w$mean / w$ecm_value, width_mean_minimum_ratio = w$mean / mode$width,
    maximum_rhat = max(d$rhat), minimum_bulk_ess = min(d$ess_bulk), minimum_tail_ess = min(d$ess_tail),
    maximum_mean_mcse_sd = max(d$mcse_sd_ratio),
    preferred_diagnostics = all(primary$preferred_pass),
    mean_precision = all(primary$mean_precision), median_precision = all(primary$median_precision),
    credible_endpoint_precision = all(primary$interval_precision))
  d$diagnostic_role <- task$diagnostic_role; d$target_class <- geometry$target_class
  d$minimum_reference <- c(mode$lower, mode$upper, mode$width, (mode$lower + mode$upper) / 2)
  sensitivity <- mgr_reweight(fits, base, cfg$tighter_prior_variance_factors, cfg$minimum_importance_ess)
  sensitivity$dataset_id <- task$dataset_id; sensitivity$case_id <- task$case_id
  sensitivity$diagnostic_role <- task$diagnostic_role; sensitivity$target_class <- geometry$target_class
  audits <- lapply(records, "[[", "audit"); comparisons <- list()
  old_jobs <- parent_jobs[parent_jobs$dataset_id == task$dataset_id, ]
  if (nrow(old_jobs)) {
    fields <- c("case_id", "dataset_sha256", "q", "delta", "ecm_lower", "ecm_upper")
    mpr_require(isTRUE(all.equal(old_jobs[1, fields], task[, fields], check.attributes = FALSE)),
      "Old and new samplers do not share the identical dataset and target.")
    old_records <- lapply(seq_len(nrow(old_jobs)), function(i)
      mgr_read(parent, old_jobs[i, , drop = FALSE], base$n_mcmc, y, base, cfg$objective_checks_per_chain))
    old_fits <- lapply(old_records, "[[", "fit")
    original <- mgp_summaries(old_fits, old_jobs[1, ], practical$policy)
    mgr_compare_saved_summary(original, original_summary[original_summary$dataset_id == task$dataset_id, ])
    original_reliable <- unique(original_summary$diagnostics_pass[
      original_summary$dataset_id == task$dataset_id])
    comparisons[[1]] <- mgr_agreement(original, d, "gaussian_gig_original", original_reliable)
    audits <- c(audits, lapply(old_records, "[[", "audit"))
    ext_jobs <- extension_jobs[extension_jobs$dataset_id == task$dataset_id, ]
    if (nrow(ext_jobs)) {
      extended <- lapply(seq_len(nrow(ext_jobs)), function(i) {
        z <- mgr_read(file.path(directory, "continuations"), ext_jobs[i, , drop = FALSE],
          practical$continuation_draws, y, base, cfg$objective_checks_per_chain, identity)
        mpr_require(identical(z$fit$parent_sha256, old_records[[i]]$audit$sha256),
          "Continuation parent digest differs.")
        z
      })
      combined <- lapply(seq_along(extended), function(i)
        list(draws = rbind(old_fits[[i]]$draws, extended[[i]]$fit$draws)))
      ext <- mgp_summaries(combined, task, practical$policy)
      comparisons[[2]] <- mgr_agreement(ext, d, "gaussian_gig_extended", all(ext$preferred_pass))
      audits <- c(audits, lapply(extended, "[[", "audit"))
    }
  }
  list(dataset = dataset, parameters = d, sensitivity = sensitivity,
    audit = do.call(rbind, audits), agreement = if (length(comparisons)) do.call(rbind, comparisons) else NULL)
}

mgr_overview <- function(d) {
  groups <- split(d, interaction(d$diagnostic_role, d$source, d$target_class, drop = TRUE))
  do.call(rbind, lapply(groups, function(z) data.frame(diagnostic_role = z$diagnostic_role[1],
    source = z$source[1], target_class = z$target_class[1], datasets = nrow(z),
    preferred_diagnostics = sum(z$preferred_diagnostics),
    maximum_rhat = max(z$maximum_rhat), minimum_bulk_ess = min(z$minimum_bulk_ess),
    median_width_mean_ecm_ratio = median(z$width_mean_ecm_ratio),
    median_width_mean_minimum_ratio = median(z$width_mean_minimum_ratio),
    median_ecm_relative_objective_gap = median(z$ecm_relative_objective_gap))))
}

mgr_cells <- function(d) {
  d <- d[d$source == "simulation", ]
  groups <- split(d, interaction(d$diagnostic_role, d$response_id, d$n, d$content, drop = TRUE))
  do.call(rbind, lapply(groups, function(z) data.frame(diagnostic_role = z$diagnostic_role[1],
    distribution = z$response_id[1], n = z$n[1], content = z$content[1], datasets = nrow(z),
    interior = sum(z$target_class == "interior"), boundary = sum(z$target_class == "boundary"),
    outside = sum(z$target_class == "outside"), maximum_rhat = max(z$maximum_rhat),
    minimum_bulk_ess = min(z$minimum_bulk_ess),
    objective_gaps_above_1e_minus4 = sum(z$ecm_relative_objective_gap > 1e-4),
    ecm_width_mean = mean(z$ecm_width), minimum_reference_width_mean = mean(z$minimum_width),
    posterior_width_mean_min = min(z$posterior_width_mean),
    posterior_width_mean_max = max(z$posterior_width_mean))))
}

mgr_width_figure <- function(datasets, jobs, directory, selected, path) {
  grDevices::pdf(path, width = 9, height = 7.8, onefile = TRUE)
  on.exit(grDevices::dev.off(), add = TRUE)
  layout(matrix(c(1, 2, 3, 4, 5, 5), nrow = 3, byrow = TRUE), heights = c(1, 1, .12))
  par(mar = c(4, 4, 3, 1), cex = .85)
  for (id in selected) {
    t <- datasets[datasets$case_id == id, ]; s <- jobs[jobs$dataset_id == t$dataset_id, ]
    widths <- unlist(lapply(s$job_id, function(k) {
      z <- readRDS(mgc_chain_path(file.path(directory, "slice"), k))$draws
      z[, 2] - z[, 1]
    }))
    labels <- c(normal = "Normal", lognormal_hard = "Lognormal",
      fct_tensile = "Tablet tensile metric")
    label <- unname(labels[t$response_id])
    mpr_require(length(label) == 1L && !is.na(label), "Missing figure response label.")
    limits <- range(c(widths, t$ecm_width, t$minimum_width))
    plot(ecdf(widths), do.points = FALSE, verticals = TRUE, log = "x", xlim = limits,
      xlab = "Interval width (log scale)", ylab = "Posterior cumulative probability",
      main = if (t$source == "simulation") sprintf("%s; n = %d, c = %.2f", label, t$n, t$content)
        else sprintf("%s; n = %d", label, t$n),
      col = "#0072B2", lwd = 1.5, ylim = c(0, 1), sub = "")
    abline(v = t$ecm_width, col = "#D55E00", lty = 2, lwd = 1.5)
    abline(v = t$minimum_width, col = "black", lty = 3, lwd = 1.5)
    mtext(sprintf("q = %.6f; %s tilt", t$q, t$target_class), side = 3, line = .2, cex = .8)
  }
  par(mar = c(0, 0, 0, 0))
  plot.new()
  legend("center", c("Posterior width", "ECM iterate", "Penalized-loss minimum"),
    col = c("#0072B2", "#D55E00", "black"), lty = c(1, 2, 3), lwd = 1.5,
    bty = "n", horiz = TRUE, cex = .85)
}
