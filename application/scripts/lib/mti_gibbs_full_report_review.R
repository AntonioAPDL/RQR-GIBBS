mgfa_require <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
  invisible(TRUE)
}

mgfa_json <- function(path) jsonlite::read_json(path, simplifyVector = TRUE)
mgfa_hash <- function(path) digest::digest(file = path, algo = "sha256", serialize = FALSE)
mgfa_columns <- function(x, columns, label) {
  mgfa_require(all(columns %in% names(x)), paste(label, "has missing columns."))
}
mgfa_equal <- function(x, y, label) {
  mgfa_require(isTRUE(all.equal(x, y, tolerance = 1e-8, check.attributes = FALSE)),
    paste("Inconsistent", label))
}
mgfa_fields <- function(x, y, columns, label) {
  mgfa_columns(x, columns, label); mgfa_columns(y, columns, label)
  for (column in columns) mgfa_equal(x[[column]], y[[column]], paste(label, column))
}
mgfa_key <- function(x, columns) do.call(paste, c(x[columns], sep = "\034"))

mgfa_artifacts <- function(directory, manifest) {
  names <- c("dataset_summary.csv", "parameter_summary.csv", "interval_comparison.csv",
    "paired_simulation_comparison.csv", "comparison_summary.csv", "gibbs_ecm_comparison.pdf",
    "numerical_reference_examples.csv", "final_summary.json")
  paths <- file.path(directory, names)
  mgfa_require(all(file.exists(paths)) && all(file.info(paths)$size > 0),
    "Completed report has missing or empty files.")
  provenance <- mgfa_json(file.path(directory, "report_provenance.json"))
  mgfa_require(identical(provenance$source_commit, manifest$identity$commit) &&
    identical(provenance$manifest_sha256, mgfa_hash(file.path(directory, "manifest.json"))),
    "Report source or manifest hash differs.")
  hashes <- unlist(provenance$artifacts, use.names = TRUE)
  # The frozen writer stores this vector as an array, in the documented file order.
  if (!is.null(names(hashes))) {
    mgfa_require(setequal(names(hashes), names), "Report hash file set differs.")
    hashes <- hashes[names]
  }
  mgfa_require(length(hashes) == length(paths) &&
    identical(unname(hashes), unname(vapply(paths, mgfa_hash, ""))), "Report file hash differs.")
  paths
}

mgfa_cells <- function(rows) {
  keys <- c("source", "response_id", "n", "content", "tolerance_confidence", "method", "split_scheme")
  groups <- split(seq_len(nrow(rows)), mgfa_key(rows, keys))
  do.call(rbind, lapply(groups, function(i) {
    z <- rows[i, ]; total <- nrow(z); successes <- sum(z$attained)
    ci <- c(NA_real_, NA_real_)
    if (z$source[1] == "simulation") {
      ci <- c(if (successes == 0) 0 else stats::qbeta(.025, successes, total - successes + 1),
        if (successes == total) 1 else stats::qbeta(.975, successes + 1, total - successes))
    }
    w <- z$width[is.finite(z$width)]
    data.frame(z[1, keys], datasets = total, available = sum(z$available), attained = successes,
      attainment_rate = successes / total, lower_95 = ci[1], upper_95 = ci[2],
      width_mean = if (length(w)) mean(w) else NA_real_,
      width_q025 = if (length(w)) unname(stats::quantile(w, .025)) else NA_real_,
      width_q975 = if (length(w)) unname(stats::quantile(w, .975)) else NA_real_)
  }))
}

mgfa_tables <- function(manifest, jobs, datasets, parameters, intervals, paired, summary, final, policy) {
  cfg <- manifest$configuration; base <- manifest$base_configuration
  frozen <- jobs[!duplicated(jobs$dataset_id), ]
  mgfa_require(nrow(jobs) == manifest$expected_chains &&
    nrow(frozen) == manifest$expected_datasets && !anyDuplicated(jobs$job_id) &&
    !anyDuplicated(jobs$seed) && all(is.finite(jobs$seed)) &&
    !anyDuplicated(paste(jobs$dataset_id, jobs$chain, sep = ":")) &&
    all(jobs$chain %in% seq_len(base$chains)) && all(table(jobs$dataset_id) == base$chains),
    "Frozen design has missing or duplicated chains.")
  mgfa_require(nrow(datasets) == nrow(frozen) && !anyDuplicated(datasets$dataset_id) &&
    setequal(datasets$dataset_id, frozen$dataset_id), "Dataset set is incomplete or duplicated.")
  d <- datasets[match(frozen$dataset_id, datasets$dataset_id), ]
  identity <- c("dataset_id", "case_id", "source", "response_id", "n", "content",
    "tolerance_confidence", "replication", "dataset_sha256", "q", "delta", "ecm_lower",
    "ecm_upper", "split_scheme", "split_id")
  mgfa_fields(d, frozen, identity, "frozen dataset identity")
  mgfa_require(sum(d$source == "simulation") == cfg$expected_simulation_datasets &&
    sum(d$source == "application") == cfg$expected_application_datasets,
    "Simulation/application dataset counts differ.")
  mgfa_columns(d, c("mean_lower", "mean_upper", "mean_width", "mean_content", "mean_attained",
    "median_lower", "median_upper", "median_interval_width", "posterior_width_q025",
    "posterior_width_median", "posterior_width_q975", "extended", "retained_per_chain",
    "screen_pass", "preferred_pass", "midpoint_screen_pass", "all_primary_precision",
    "maximum_rhat", "minimum_bulk_ess", "minimum_tail_ess"), "Dataset summaries")
  mgfa_require(all(is.finite(d$mean_lower) & is.finite(d$mean_upper) & d$mean_upper > d$mean_lower) &&
    all(is.finite(d$mean_content) & d$mean_content >= 0 & d$mean_content <= 1) &&
    all(!is.na(d$mean_attained)) &&
    !any(d$mean_attained & d$mean_content < d$content - 1e-10) &&
    !any(!d$mean_attained & d$mean_content > d$content + 1e-10),
    "Invalid posterior-mean endpoints or content attainment.")
  mgfa_equal(d$mean_width, d$mean_upper - d$mean_lower, "mean interval width")
  mgfa_equal(d$median_interval_width, d$median_upper - d$median_lower, "median-endpoint interval width")
  mgfa_require(all(!is.na(d$extended)) && all(d$retained_per_chain ==
    base$n_mcmc + ifelse(d$extended, cfg$extension_draws, 0)), "Retained draw counts differ.")
  mgfa_require(all(is.finite(d$posterior_width_q025) & d$posterior_width_q025 >= 0 &
    d$posterior_width_q025 <= d$posterior_width_median &
    d$posterior_width_median <= d$posterior_width_q975 & is.finite(d$posterior_width_q975)),
    "Invalid posterior width quantiles.")

  variables <- c("lower", "upper", "width", "midpoint")
  mgfa_columns(parameters, c("dataset_id", "variable", "q", "delta", "mean", "median", "rhat",
    "ess_bulk", "ess_tail", "mcse_sd_ratio", "screen_pass", "preferred_pass",
    "mean_precision", "median_precision", "interval_precision", "lower_025", "upper_975"),
    "Parameter summaries")
  wanted <- paste(rep(frozen$dataset_id, each = 4L), rep(variables, nrow(frozen)), sep = ":")
  key <- paste(parameters$dataset_id, parameters$variable, sep = ":")
  mgfa_require(length(key) == length(wanted) && !anyDuplicated(key) && setequal(key, wanted),
    "Parameter set is incomplete or duplicated.")
  p <- parameters[match(wanted, key), ]
  mgfa_equal(p$q, rep(d$q, each = 4L), "parameter fitted content")
  mgfa_equal(p$delta, rep(d$delta, each = 4L), "parameter tilt")
  adequate <- function(rhat_limit, ess_limit, mcse_limit) is.finite(p$rhat) &
    is.finite(p$ess_bulk) & is.finite(p$ess_tail) & is.finite(p$mcse_sd_ratio) &
    p$rhat <= rhat_limit & p$ess_bulk >= ess_limit & p$ess_tail >= ess_limit &
    p$mcse_sd_ratio <= mcse_limit
  screen <- adequate(policy$exploratory_rhat, policy$exploratory_ess, policy$exploratory_mcse_ratio)
  preferred <- adequate(policy$preferred_rhat, policy$preferred_ess, policy$preferred_mcse_ratio)
  mgfa_equal(p$screen_pass, screen, "practical parameter flags")
  mgfa_equal(p$preferred_pass, preferred, "preferred parameter flags")
  screen <- matrix(screen, ncol = 4L, byrow = TRUE)
  preferred <- matrix(preferred, ncol = 4L, byrow = TRUE)
  mgfa_equal(d$screen_pass, rowSums(screen[, 1:3, drop = FALSE]) == 3L, "practical dataset flags")
  mgfa_equal(d$preferred_pass, rowSums(preferred[, 1:3, drop = FALSE]) == 3L, "preferred dataset flags")
  mgfa_equal(d$midpoint_screen_pass, screen[, 4], "secondary midpoint flags")
  for (v in variables) {
    z <- p[p$variable == v, ]
    value <- switch(v, lower = d$mean_lower, upper = d$mean_upper, width = d$mean_width,
      midpoint = (d$mean_lower + d$mean_upper) / 2)
    mgfa_equal(z$mean, value, paste(v, "posterior mean"))
    if (v %in% c("lower", "upper"))
      mgfa_equal(z$median, d[[paste0("median_", v)]], paste(v, "posterior median"))
    if (v == "width") {
      mgfa_equal(z$median, d$posterior_width_median, "posterior median width")
      mgfa_equal(z$lower_025, d$posterior_width_q025, "posterior width lower quantile")
      mgfa_equal(z$upper_975, d$posterior_width_q975, "posterior width upper quantile")
    }
  }
  for (field in c("rhat", "ess_bulk", "ess_tail")) {
    x <- matrix(p[[field]], ncol = 4L, byrow = TRUE)
    expected <- if (field == "rhat") apply(x, 1, max) else apply(x, 1, min)
    target <- switch(field, rhat = "maximum_rhat", ess_bulk = "minimum_bulk_ess", ess_tail = "minimum_tail_ess")
    mgfa_equal(d[[target]], expected, paste("dataset", field))
  }
  precision <- (p$mean_precision & p$median_precision & p$interval_precision) %in% TRUE
  mgfa_equal(d$all_primary_precision, rowSums(matrix(precision, ncol = 4L, byrow = TRUE)[, 1:3]) == 3L,
    "additional reporting precision flags")

  interval_identity <- c("dataset_id", "case_id", "source", "response_id", "n", "content",
    "tolerance_confidence", "replication", "split_scheme")
  mgfa_columns(intervals, c(interval_identity, "method", "lower", "upper", "width",
    "observed_content", "attained", "available"), "Interval comparisons")
  ikey <- paste(intervals$dataset_id, intervals$method, sep = ":")
  mgfa_require(!anyDuplicated(ikey) && all(!is.na(intervals$available)) &&
    all(!is.na(intervals$attained)) && !any(intervals$attained & !intervals$available),
    "Duplicate interval comparisons or invalid availability flags.")
  methods <- list(simulation = c("mti_gibbs_mean", "mti_ecm_adaptive_cell", "young_mathew", "wilks"),
    application = c("mti_gibbs_mean", "mti_ecm_adaptive_cell", "young_mathew", "wilks_minmax"))
  for (source in names(methods)) {
    z <- intervals[intervals$source == source, ]; ids <- d$dataset_id[d$source == source]
    if (!length(ids)) { mgfa_require(nrow(z) == 0L, "Unexpected comparison source."); next }
    mgfa_require(setequal(z$method, methods[[source]]), "Comparator method set differs.")
    for (method in methods[[source]]) mgfa_require(
      setequal(z$dataset_id[z$method == method], ids), "A comparator has missing datasets.")
  }
  mgfa_require(all(intervals$source %in% names(methods)), "Unknown comparison source.")
  mgfa_fields(intervals, d[match(intervals$dataset_id, d$dataset_id), ], interval_identity,
    "paired comparator identity")
  returned <- intervals$available
  mgfa_require(all(is.finite(intervals$lower[returned]) & is.finite(intervals$upper[returned]) &
    intervals$upper[returned] > intervals$lower[returned] & is.finite(intervals$observed_content[returned]) &
    intervals$observed_content[returned] >= 0 & intervals$observed_content[returned] <= 1),
    "Invalid returned comparator endpoints or content.")
  mgfa_require(!any(intervals$attained[returned] &
    intervals$observed_content[returned] < intervals$content[returned] - 1e-10) &&
    !any(!intervals$attained[returned] &
    intervals$observed_content[returned] > intervals$content[returned] + 1e-10),
    "Comparator content attainment differs.")
  mgfa_equal(intervals$width[returned], intervals$upper[returned] - intervals$lower[returned],
    "returned comparator widths")
  gibbs <- intervals[match(paste(d$dataset_id, "mti_gibbs_mean", sep = ":"), ikey), ]
  mgfa_equal(gibbs$lower, d$mean_lower, "reported Gibbs lower endpoints")
  mgfa_equal(gibbs$upper, d$mean_upper, "reported Gibbs upper endpoints")
  mgfa_equal(gibbs$observed_content, d$mean_content, "reported Gibbs content")
  mgfa_equal(gibbs$attained, d$mean_attained, "reported Gibbs attainment")
  sim <- d[d$source == "simulation", ]
  mgfa_require(nrow(paired) == nrow(sim) && !anyDuplicated(paired$dataset_id) &&
    setequal(paired$dataset_id, sim$dataset_id), "Paired simulation dataset set differs.")
  paired <- paired[match(sim$dataset_id, paired$dataset_id), ]
  mgfa_fields(paired, sim, c("case_id", "response_id", "n", "content", "replication", "mean_width",
    "mean_content", "mean_attained", "screen_pass", "preferred_pass"), "paired simulation summaries")
  for (method in setdiff(methods$simulation, "mti_gibbs_mean")) {
    z <- intervals[match(paste(sim$dataset_id, method, sep = ":"), ikey), ]
    mgfa_equal(paired[[paste0(method, "_width")]], z$width, paste(method, "paired width"))
    mgfa_equal(paired[[paste0(method, "_attained")]][z$available], z$attained[z$available],
      paste(method, "paired attainment"))
    mgfa_equal(paired[[paste0("width_difference_", method)]], sim$mean_width - z$width,
      paste(method, "paired width difference"))
  }
  expected <- mgfa_cells(intervals)
  keys <- c("source", "response_id", "n", "content", "tolerance_confidence", "method", "split_scheme")
  key <- mgfa_key(summary, keys); wanted <- mgfa_key(expected, keys)
  mgfa_require(nrow(summary) == nrow(expected) && !anyDuplicated(key) && setequal(key, wanted),
    "Comparison cell set is incomplete or duplicated.")
  mgfa_fields(summary[match(wanted, key), ], expected, names(expected), "recomputed cell summaries")
  counts <- list(datasets = nrow(d), simulation_datasets = nrow(sim),
    application_datasets = sum(d$source == "application"), preferred_datasets = sum(d$preferred_pass),
    practical_datasets = sum(d$screen_pass), unresolved_datasets = sum(!d$screen_pass),
    all_primary_precision_datasets = sum(d$all_primary_precision), extended_datasets = sum(d$extended),
    maximum_rhat = max(p$rhat), minimum_bulk_ess = min(p$ess_bulk), minimum_tail_ess = min(p$ess_tail))
  for (name in names(counts)) {
    value <- final[[name]]
    if (length(counts[[name]]) == 1L && !is.finite(counts[[name]])) {
      if (is.null(value) || identical(value, "NA")) value <- NA_real_
      else if (is.character(value) && value %in% c("NaN", "Inf", "-Inf")) value <- as.numeric(value)
    }
    mgfa_equal(value, counts[[name]], paste("final summary", name))
  }
  mgfa_require(identical(final$new_tolerance_guarantee, FALSE) && identical(final$article_replaced, FALSE),
    "Unexpected automatic promotion or tolerance-guarantee claim.")
  counts
}

mgfa_review <- function(directory, expected_commit = NULL) {
  directory <- normalizePath(directory, mustWork = TRUE)
  manifest <- mgfa_json(file.path(directory, "manifest.json"))
  mgfa_require(is.null(manifest$role) && !is.null(manifest$configuration),
    "Integration examples are not a completed full campaign.")
  if (!is.null(expected_commit)) mgfa_require(identical(manifest$identity$commit, expected_commit),
    "Campaign commit differs from the requested frozen source.")
  mgfa_require(identical(manifest$sampler, "intercept_marginal_slice_within_gibbs") &&
    identical(manifest$configuration$target_selection, "hold_archived_ecm_selected_q_and_delta_fixed") &&
    identical(manifest$new_tolerance_guarantee, FALSE) && identical(manifest$article_replaced, FALSE),
    "Campaign scientific specification differs.")
  status <- mgfa_json(file.path(directory, "campaign_status.json"))
  mgfa_require(identical(status$commit, manifest$identity$commit) ||
    identical(status$status, "qualifying") && is.null(status$commit), "Controller source identity differs.")
  mgfa_require(status$status %in% c("qualifying", "base_running", "assessing_base", "extensions_running",
    "assessing_extensions", "reporting", "calculation_complete_review_required", "stopped_on_request",
    "stopped_with_error"), "Unknown campaign state.")
  exit_path <- file.path(directory, "controller_exit.json")
  complete <- identical(status$status, "calculation_complete_review_required")
  if (!complete || !file.exists(exit_path)) {
    progress <- file.path(directory, "base_progress.json")
    mgfa_require(!startsWith(status$status, "stopped"), paste("Campaign", status$status, status$error))
    return(list(status = "pending", campaign_state = if (complete) "awaiting_controller_exit" else status$status,
      source_commit = manifest$identity$commit,
      scientific_review_required = TRUE, full_results_checked = FALSE,
      progress = if (file.exists(progress)) mgfa_json(progress) else NULL))
  }
  mgfa_require(identical(mgfa_json(exit_path)$exit_code, 0L), "Controller did not exit successfully.")
  progress <- mgfa_json(file.path(directory, "base_progress.json"))
  mgfa_require(progress$completed_chains == manifest$expected_chains && progress$remaining_chains == 0 &&
    progress$completed_datasets == manifest$expected_datasets, "Base queue is incomplete.")
  paths <- mgfa_artifacts(directory, manifest)
  jobs <- readRDS(file.path(directory, "jobs.rds"))
  mgfa_require(identical(digest::digest(jobs, algo = "sha256"), manifest$identity$design_sha256),
    "Frozen design hash differs.")
  root <- dirname(dirname(dirname(directory)))
  policy_path <- file.path(root, manifest$configuration$practical_config)
  mgfa_require(file.exists(policy_path) && mgfa_hash(policy_path) %in% manifest$inputs,
    "Frozen diagnostic policy is missing or changed.")
  policy <- mgfa_json(policy_path)$policy
  read <- function(name) as.data.frame(data.table::fread(file.path(directory, name),
    nThread = 1L, na.strings = "NA", showProgress = FALSE))
  result <- mgfa_tables(manifest, jobs, read("dataset_summary.csv"), read("parameter_summary.csv"),
    read("interval_comparison.csv"), read("paired_simulation_comparison.csv"),
    read("comparison_summary.csv"), mgfa_json(file.path(directory, "final_summary.json")), policy)
  mgfa_equal(status$completed_base_chains, manifest$expected_chains, "terminal base-chain count")
  mgfa_equal(status$completed_extension_chains,
    result$extended_datasets * manifest$base_configuration$chains, "terminal extension-chain count")
  list(status = "report_consistency_passed", source_commit = manifest$identity$commit,
    full_results_checked = TRUE, scientific_review_required = TRUE,
    raw_chains_reassessed = FALSE, new_tolerance_guarantee = FALSE,
    checked_report_files = basename(paths), results = result)
}
