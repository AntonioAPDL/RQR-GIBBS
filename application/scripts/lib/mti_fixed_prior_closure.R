mfc_settings_check <- function(prior, parent_manifest) {
  mwd_require(identical(mwd_hash(prior), mwd_hash(parent_manifest$identity$configuration)),
    "The fixed-prior statistical settings differ from the parent study.")
  mwd_require(prior$primary_kappa == 2 && !prior$automatic_prior_tuning &&
    !prior$automatic_article_replacement, "The approved fixed-prior settings are required.")
  for (reference in c("simulation_reference", "application_reference"))
    mfp_spec(c(0, 1), prior$primary_kappa, prior[[reference]])
  invisible(TRUE)
}

mfc_result_check <- function(result, task, prior) {
  mwd_require(identical(mwd_hash(result$task), mwd_hash(task)), "The repaired task differs.")
  reference <- if (task$stage == "application") prior$application_reference else prior$simulation_reference
  expected <- mfp_spec(c(0, 1), task$kappa, reference)
  fields <- setdiff(names(expected), c("z", "prior_sha256"))
  mwd_require(identical(result$prior$prior_sha256, expected$prior_sha256) &&
    isTRUE(all.equal(result$prior[fields], expected[fields], tolerance = 0)) &&
    identical(result$prior$data_adaptive_prior, FALSE), "A result does not use the declared independent prior.")
  mwd_require(identical(result$dp_prior$estimated_from_analysis_sample, FALSE),
    "The DP prior must not be estimated from this sample.")
  invisible(TRUE)
}

mfc_save <- function(result, path, identity) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- paste0(path, ".tmp")
  saveRDS(result, temporary)
  mwd_require(file.rename(temporary, path), "Cannot save a continuation fit.")
  mgfp_json_write(list(identity = identity, sha256 = mwd_file_hash(path)), paste0(path, ".json"))
  invisible(result)
}

mfc_read <- function(path, identity, task = NULL) {
  receipt <- mwd_json(paste0(path, ".json"))
  mwd_require(identical(receipt$identity, identity) && identical(receipt$sha256, mwd_file_hash(path)),
    "A continuation fit or receipt differs.")
  result <- readRDS(path)
  if (!is.null(task)) mwd_require(identical(mwd_hash(result$task), mwd_hash(task)), "A continuation task differs.")
  result
}

mfc_repair_check <- function(original, repaired, task, prior) {
  mwd_require(!isTRUE(original$returned) && identical(original$reason, "unresolved_profile_candidates"),
    "Only unresolved numerical candidates may be repaired; an accepted result cannot be replaced.")
  mfc_result_check(repaired, task, prior)
  mwd_require(isTRUE(repaired$returned) && all(repaired$rows$ecm_coordinate_ok & repaired$rows$gibbs_adequate),
    "The numerical repair has not passed the unchanged accuracy checks.")
  mwd_require(identical(original$sample_sha256, repaired$sample_sha256) &&
    identical(original$prior$prior_sha256, repaired$prior$prior_sha256) &&
    identical(mwd_hash(original$dp_prior), mwd_hash(repaired$dp_prior)), "The repaired sample or prior changed.")
  keys <- c("candidate_index", "target_content", "mean_tilt", "central_tilt")
  mwd_require(identical(mwd_hash(original$candidates[keys]), mwd_hash(repaired$candidates[keys])),
    "The repaired profile target menu differs.")
  good <- which(original$candidates$numerically_accurate)
  stable <- c("lower", "upper", "ecm_objective", "observed_count")
  mwd_require(isTRUE(all.equal(original$candidates[good, stable], repaired$candidates[good, stable],
    tolerance = 0)), "An already accurate candidate changed during numerical repair.")
  invisible(TRUE)
}

mfc_merge <- function(original, replacements) {
  original <- data.table::as.data.table(original)
  replacement_ids <- vapply(replacements, function(r) r$task$job_id, 0L)
  mwd_require(!anyDuplicated(replacement_ids), "Duplicate numerical repairs.")
  for (r in replacements) {
    before <- original[job_id == r$task$job_id]
    mwd_require(nrow(before) == 2L && all(!before$returned) && isTRUE(r$returned) &&
      nrow(r$rows) == 2L && setequal(r$rows$method, c("ecm", "gibbs")), "Invalid replacement rows.")
  }
  data.table::rbindlist(c(list(original[!job_id %in% replacement_ids]), lapply(replacements, `[[`, "rows")), fill = TRUE)
}

mfc_summary <- function(directory, prior) {
  cells <- mfp_review(directory, prior)
  cells <- data.table::as.data.table(cells)
  cells[, upper_one_sided_95 := ifelse(successes == attempts, 1,
    stats::qbeta(.95, successes + 1, attempts - successes))]
  family_size <- nrow(cells[method %in% c("ecm", "gibbs")])
  cells[, simultaneous_upper_95 := ifelse(successes == attempts, 1,
    stats::qbeta(1 - .05 / family_size, successes + 1, attempts - successes))]
  mwd_write(cells, file.path(directory, "comparison_summary.csv"))
  d <- data.table::fread(file.path(directory, "interval_results.csv"))
  numerical <- isTRUE(all(d$returned & d$ecm_coordinate_ok & d$gibbs_adequate))
  status <- list(expected_pairs = length(unique(d$job_id)), numerical_pass = numerical,
    pointwise_underattainment_warnings = as.data.frame(cells[
      method %in% c("ecm", "gibbs") & upper_one_sided_95 < tolerance_confidence]),
    simultaneous_underattainment_warnings = as.data.frame(cells[
      method %in% c("ecm", "gibbs") & simultaneous_upper_95 < tolerance_confidence]),
    simultaneous_tolerance_guarantee = FALSE, prior_uses_analysis_data = FALSE,
    article_replaced = FALSE, time = mgfp_time())
  mgfp_json_write(status, file.path(directory, "status.json")); status
}

mfc_diagnostic_design <- function(rows, controls) {
  d <- data.table::as.data.table(rows)[response_id == controls$response_id & n == controls$n &
    content == controls$content & returned == TRUE]
  z <- data.table::dcast(d, job_id + data_seed ~ method, value.var = "attained")
  z[, pattern := ifelse(ecm & !gibbs, "ecm_only", ifelse(ecm & gibbs, "both",
    ifelse(!ecm & !gibbs, "neither", "gibbs_only")))]
  z <- z[pattern %in% controls$patterns][order(job_id)]
  z <- z[, head(.SD, controls$per_pattern), by = pattern]
  z[, diagnostic_id := seq_len(.N)]
  z[, seed := as.integer(controls$chain_seed_base + 100L * job_id)]
  mwd_require(!anyDuplicated(z$job_id) && all(z$seed > 0), "Invalid diagnostic cases.")
  as.data.frame(z)
}

mfc_diagnostic_job <- function(task, parent_fit, prior, context, controls, policy, parent_identity = NULL) {
  source <- if (is.null(parent_identity)) readRDS(parent_fit) else mfc_read(parent_fit, parent_identity)
  mwd_require(isTRUE(source$returned), "A diagnostic needs a returned original fit.")
  mfc_result_check(source, source$task, prior)
  set.seed(source$task$data_seed)
  y <- context$laws[[source$task$response_id]]$r(source$task$n)
  mwd_require(identical(mwd_hash(y), source$sample_sha256), "A diagnostic sample changed.")
  specification <- mfp_spec(y, source$task$kappa, prior$simulation_reference)
  mwd_require(identical(specification$prior_sha256, source$prior$prior_sha256), "The diagnostic prior changed.")
  ecm <- unlist(source$rows[source$rows$method == "ecm", c("lower", "upper")], use.names = FALSE)
  ecm <- (ecm - specification$center) / specification$scale
  initial <- mwd_initial(specification$z, ecm[1], ecm[2], source$task$kappa^2, 0)
  sampling <- list(n_burn = controls$n_burn, n_mcmc = controls$n_mcmc, learning_rate = 1,
    prior_variance = source$task$kappa^2, initial_midpoint_offsets_sd = initial$offsets,
    initial_halfwidth_multipliers = initial$radii)
  fits <- lapply(seq_len(controls$chains), function(chain) {
    t <- data.frame(chain = chain, seed = as.integer(task$seed + chain), q = source$q,
      delta = source$delta_reference, ecm_lower = ecm[1], ecm_upper = ecm[2])
    mgp_slice_fit(specification$z, t, sampling)
  })
  t <- data.frame(case_id = "fixed_prior_diagnostic", dataset_id = source$task$dataset_id,
    source = "selected_diagnostic_only", response_id = source$task$response_id, n = source$task$n,
    content = source$task$content, q = source$q, delta = source$delta_reference,
    ecm_lower = ecm[1], ecm_upper = ecm[2])
  parameters <- mgp_summaries(fits, t, policy)
  old <- source$parameters_reference_units[match(parameters$variable, source$parameters_reference_units$variable), ]
  comparison <- data.frame(variable = parameters$variable, original_mean = old$mean, original_mcse = old$mcse_mean,
    diagnostic_mean = parameters$mean, diagnostic_mcse = parameters$mcse_mean,
    standardized_difference = (parameters$mean - old$mean) / sqrt(parameters$mcse_mean^2 + old$mcse_mean^2))
  means <- setNames(parameters$mean, parameters$variable)
  endpoints <- means[c("lower", "upper")] * specification$scale + specification$center
  # Population truth enters only after the posterior calculation has finished.
  content <- context$laws[[source$task$response_id]]$p(endpoints[2]) -
    context$laws[[source$task$response_id]]$p(endpoints[1])
  distribution <- rqrgibbs::rqr_dp_fit(y, concentration = source$dp_prior$concentration,
    base_measure = rqrgibbs::rqr_dp_base_normal(source$dp_prior$raw_base$mean, source$dp_prior$raw_base$sd))
  probability <- rqrgibbs::rqr_dp_content_probability(distribution, endpoints[1], endpoints[2], source$task$content)
  raw_ecm <- ecm * specification$scale + specification$center
  list(task = task, source_task = source$task, returned = TRUE, fits = fits, parameters = parameters, comparison = comparison,
    original_fit_sha256 = mwd_file_hash(parent_fit), sample_sha256 = source$sample_sha256,
    prior = source$prior, q = source$q, delta_reference = source$delta_reference,
    original_population_content = source$rows$population_content[source$rows$method == "gibbs"],
    lower = endpoints[1], upper = endpoints[2], width = diff(endpoints),
    original_dp_probability = source$rows$dp_content_probability[source$rows$method == "gibbs"],
    dp_probability = probability$posterior_content_probability,
    ecm_contains_diagnostic = raw_ecm[1] <= endpoints[1] && raw_ecm[2] >= endpoints[2],
    diagnostic_contains_ecm = endpoints[1] <= raw_ecm[1] && endpoints[2] >= raw_ecm[2],
    population_content = content, requested_content = source$task$content,
    adequate = all(parameters$preferred_pass & parameters$mean_precision),
    interpretation = "outcome_stratified_numerical_diagnosis_not_independent_performance_validation")
}

mfc_diagnostic_pdf <- function(results, path) {
  grDevices::pdf(path, width = 11, height = 8.5, onefile = TRUE)
  on.exit(grDevices::dev.off())
  for (r in results) {
    if (!isTRUE(r$returned)) next
    a <- mgp_array(r$fits)
    variables <- c("lower", "upper", "width", "objective")
    graphics::par(mfrow = c(2, 2), mar = c(4, 4, 3, 1), oma = c(0, 0, 3, 0))
    for (variable in variables) {
      values <- if (variable == "objective") do.call(cbind, lapply(r$fits, `[[`, "retained_objective")) else
        a[, , variable]
      keep <- unique(round(seq(1, nrow(values), length.out = min(1500, nrow(values)))))
      graphics::matplot(keep, values[keep, , drop = FALSE], type = "l", lty = 1,
        col = c("#286A9B", "#A04416", "#32734D", "#555555"), xlab = "Retained iteration",
        ylab = variable, main = paste("Four-chain trace:", variable))
    }
    graphics::mtext(sprintf("%s: n = %d, c = %.2f; sample %d; q = %.4f; tilt = %.5f",
      r$source_task$response_id, r$source_task$n, r$requested_content,
      r$task$job_id, r$q, r$delta_reference), outer = TRUE, cex = 1)
  }
}

mfc_application_review <- function(directory) {
  path <- file.path(directory, "interval_results.csv")
  if (!file.exists(path)) return(invisible(NULL))
  d <- data.table::fread(path)
  other <- file.path(directory, "comparator_results.csv")
  if (file.exists(other)) d <- data.table::rbindlist(list(d, data.table::fread(other)), fill = TRUE)
  d[, partition := sub(":.*", "", training_key)]
  cells <- d[, .(attempts = .N, returned = sum(returned),
    heldout_content_q025 = quantile(heldout_empirical_content, .025, na.rm = TRUE),
    heldout_content_median = median(heldout_empirical_content, na.rm = TRUE),
    heldout_content_q975 = quantile(heldout_empirical_content, .975, na.rm = TRUE),
    width_q025 = quantile(width, .025, na.rm = TRUE), mean_width = mean(width, na.rm = TRUE),
    width_q975 = quantile(width, .975, na.rm = TRUE)), by = .(response_id, partition, method)]
  mwd_write(cells, file.path(directory, "descriptive_summary.csv"))
  grDevices::pdf(file.path(directory, "application_review.pdf"), width = 11, height = 7, onefile = TRUE)
  on.exit(grDevices::dev.off())
  groups <- unique(cells[, .(response_id, partition)])
  colors <- c(ecm = "#A04416", gibbs = "#32734D", young_mathew = "#286A9B", wilks = "#000000")
  for (i in seq_len(nrow(groups))) {
    z <- cells[response_id == groups$response_id[i] & partition == groups$partition[i]]
    graphics::par(mfrow = c(1, 2), mar = c(5, 8, 4, 1))
    for (kind in c("width", "heldout_content")) {
      lower <- z[[paste0(kind, "_q025")]]; upper <- z[[paste0(kind, "_q975")]]
      center <- if (kind == "width") z$mean_width else z$heldout_content_median
      bounds <- if (kind == "width") range(c(0, lower, upper), finite = TRUE) else c(0, 1)
      graphics::plot(NA, xlim = bounds, ylim = c(.5, nrow(z) + .5), yaxt = "n", ylab = "",
        xlab = if (kind == "width") "Width: 2.5th to 97.5th percentiles" else "Held-out empirical content: partition variation",
        main = paste(groups$response_id[i], groups$partition[i]))
      graphics::axis(2, at = seq_len(nrow(z)), labels = z$method, las = 1, tick = FALSE)
      graphics::segments(lower, seq_len(nrow(z)), upper, seq_len(nrow(z)), col = colors[z$method], lwd = 2)
      graphics::points(center, seq_len(nrow(z)), col = colors[z$method], pch = 4)
      if (kind == "heldout_content") graphics::abline(v = .9, lty = 2, col = "#777777")
    }
  }
  invisible(cells)
}

mfc_parent_ready <- function(directory, expected) {
  exit <- file.path(directory, "controller_exit.json")
  if (!file.exists(exit)) return(FALSE)
  mwd_require(mwd_json(exit)$exit_code == 0, "The parent controller stopped unsuccessfully; review its preserved results.")
  path <- file.path(directory, "validation/status.json")
  mwd_require(file.exists(path), "The parent stopped before independent validation was collected.")
  status <- mwd_json(path)
  mwd_require(status$completed == expected && status$expected == expected,
    "The parent validation is incomplete.")
  TRUE
}

mfc_run_tasks <- function(tasks, directory, identity, workers, reserve, fit) {
  dir.create(file.path(directory, "fits"), recursive = TRUE, showWarnings = FALSE)
  paths <- file.path(directory, "fits", sprintf("fit_%06d.rds", tasks$job_id))
  present <- which(file.exists(paste0(paths, ".json")))
  for (j in present) mfc_read(paths[j], identity, tasks[j, ])
  pending <- setdiff(seq_len(nrow(tasks)), present)
  workers <- min(workers, parallelly::availableCores())
  for (batch in split(pending, ceiling(seq_along(pending) / workers))) {
    mps_disk_check(directory, reserve)
    done <- parallel::mclapply(batch, function(j) {
      result <- tryCatch(fit(tasks[j, ]), error = function(e)
        list(task = tasks[j, ], returned = FALSE, reason = conditionMessage(e)))
      mfc_save(result, paths[j], identity)
      TRUE
    }, mc.cores = workers, mc.set.seed = FALSE)
    mwd_require(all(vapply(done, isTRUE, FALSE)), "A continuation worker did not save its result.")
    mgfp_json_write(list(completed = sum(file.exists(paste0(paths, ".json"))), expected = nrow(tasks),
      workers = workers, prior_uses_analysis_data = FALSE, time = mgfp_time()), file.path(directory, "progress.json"))
  }
  invisible(paths)
}

mfc_completion_report <- function(output, validation, application, diagnostics, parent_commit, commit) {
  cells <- data.table::fread(file.path(output, "validation/comparison_summary.csv"))
  summary <- cells[, .(attempts = sum(attempts), successes = sum(successes),
    attainment = sum(successes) / sum(attempts), minimum_cell_attainment = min(attainment)), by = method]
  mwd_write(summary, file.path(output, "method_summary.csv"))
  text <- c("# Fixed-Prior ECM/Gibbs Study Assessment", "",
    paste("Original simulation source:", parent_commit), paste("Numerical repair and application source:", commit), "",
    "Both methods use fixed independent Normal(0,2^2) root priors in declared reporting units and learning rate one.",
    "The root prior and DP base are not estimated from fitting or held-out observations.",
    "Fitted content and tilt remain training-selected; Gibbs targets their fixed-target generalized posterior.",
    "Posterior propriety and numerical adequacy do not imply frequentist tolerance validity.", "",
    "## Independent Validation", "", "| Method | Successes / Attempts | Attainment | Minimum Cell |",
    "| --- | ---: | ---: | ---: |")
  for (i in seq_len(nrow(summary))) text <- c(text, sprintf("| %s | %d / %d | %.2f%% | %.2f%% |",
    summary$method[i], summary$successes[i], summary$attempts[i], 100 * summary$attainment[i],
    100 * summary$minimum_cell_attainment[i]))
  text <- c(text, "", paste("Combined numerical assessment:", validation$numerical_pass),
    "The original failed attempts and their receipts remain unchanged; repaired calculations are attributed separately.",
    "Overall attainment describes the balanced design and is not a statement about every case.",
    paste("Pointwise underattainment warnings:", nrow(validation$pointwise_underattainment_warnings)),
    paste("Application numerical assessment:", application$numerical_pass),
    "Application partitions overlap; held-out content summaries are descriptive.",
    paste("Additional fixed-target diagnostic cases:", diagnostics$completed),
    "Diagnostic cases are outcome-stratified; they are not new performance validation.", "",
    "## Reporting Decision", "",
    "Review the full cell-level results and the Student-t n=500,c=0.99 diagnostic before manuscript promotion.",
    "A persistent Gibbs-mean attainment deficit limits claims for that reported interval summary.",
    "Keep fixed-target posterior uncertainty distinct from the ECM interval's DP eligibility condition.",
    "No prior, probability threshold, or manuscript result was changed automatically.")
  writeLines(text, file.path(output, "assessment.md"))
  invisible(summary)
}
