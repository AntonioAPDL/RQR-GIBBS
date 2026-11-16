mff_methods <- c(ecm = "MTI-ECM", gibbs = "MTI-Gibbs", young_mathew = "Young--Mathew", wilks = "Wilks")
mff_ids <- c(ecm = "mti_ecm_adaptive_cell", gibbs = "mti_gibbs_mean", young_mathew = "young_mathew", wilks = "wilks_minmax")

mff_simulation_cells <- function(rows, expected = 1000L, settings = 72L) {
  d <- data.table::as.data.table(rows)
  mwd_require(!anyDuplicated(d[, .(job_id, method)]) && setequal(unique(d$method), names(mff_methods)),
    "Duplicate simulation observations or missing comparison methods.")
  cells <- d[, .(attempts = .N, returned = sum(returned), successes = sum(returned & attained),
    attainment = mean(returned & attained), mean_width = mean(width, na.rm = TRUE),
    width_q025 = quantile(width, .025, na.rm = TRUE), width_q975 = quantile(width, .975, na.rm = TRUE)),
    by = .(response_id, n, content, tolerance_confidence, method)]
  mwd_require(nrow(cells) == 4L * settings && all(cells$attempts == expected), "Incomplete simulation design.")
  cells[, lower_95 := ifelse(successes == 0, 0, qbeta(.025, successes, attempts - successes + 1))]
  cells[, upper_95 := ifelse(successes == attempts, 1, qbeta(.975, successes + 1, attempts - successes))]
  cells[, upper_one_sided_95 := ifelse(successes == attempts, 1, qbeta(.95, successes + 1, attempts - successes))]
  cells[, simultaneous_lower := ifelse(successes == 0, 0, qbeta(.05 / (2 * settings), successes, attempts - successes + 1))]
  cells[, simultaneous_upper := ifelse(successes == attempts, 1,
    qbeta(1 - .05 / (2 * settings), successes + 1, attempts - successes))]
  cells
}

mff_primary_cells <- function(cells) {
  d <- data.table::copy(cells[method != "gibbs"])
  families <- c(normal = "Gaussian", student_t3 = "Student t(3)", exponential = "Exponential",
    asym_laplace_tau010 = "Asymmetric Laplace (tau=0.10)", two_piece_normal_1_12 = "Two-piece Normal (1:12)",
    beta18 = "Beta(1,8)", gamma05 = "Gamma(0.5,1)", lognormal_hard = "Log-normal (log sd 1.25)")
  d[, `:=`(dgp_id = response_id, dgp = unname(families[response_id]),
    method_id = unname(mff_ids[method]), method = unname(mff_methods[method]),
    replications = attempts, width_mean = mean_width)]
  d
}

mff_application_values <- function(rows, context) {
  d <- data.table::as.data.table(rows)
  mwd_require(!anyDuplicated(d[, .(job_id, method)]) && setequal(unique(d$method), names(mff_methods)),
    "Duplicate application observations or missing comparison methods.")
  d[, split_scheme := sub(":.*", "", training_key)]
  d[, split_id := as.integer(sub("^[^:]+:", "", training_key))]
  details <- lapply(seq_len(nrow(d)), function(j) {
    r <- d[j]; ids <- context$training_ids[[r$training_key]]
    heldout <- context$observed[[r$response_id]][!context$observed$.analysis_row %in% ids]
    value <- if (r$returned) mean(heldout >= r$lower & heldout <= r$upper) else NA_real_
    if (r$returned) mwd_require(abs(value - r$heldout_empirical_content) < 1e-12,
      "Saved held-out evaluation differs from the original partition.")
    data.table::data.table(heldout_n = length(heldout), heldout_content = value,
      heldout_attained = isTRUE(r$returned) && value >= r$content,
      lower_omitted = if (r$returned) mean(heldout < r$lower) else NA_real_,
      upper_omitted = if (r$returned) mean(heldout > r$upper) else NA_real_)
  })
  extra <- data.table::rbindlist(details)
  d$heldout_n <- NULL
  cbind(d, extra)
}

mff_application_cells <- function(d) {
  d[, .(attempts = .N, returned = sum(returned), attainment = mean(heldout_attained),
    mean_width = mean(width, na.rm = TRUE), median_width = median(width, na.rm = TRUE),
    width_q025 = quantile(width, .025, na.rm = TRUE), width_q975 = quantile(width, .975, na.rm = TRUE),
    lower_median = median(lower, na.rm = TRUE), upper_median = median(upper, na.rm = TRUE),
    content_median = median(heldout_content, na.rm = TRUE), content_q025 = quantile(heldout_content, .025, na.rm = TRUE),
    content_q975 = quantile(heldout_content, .975, na.rm = TRUE),
    lower_omitted_median = median(lower_omitted, na.rm = TRUE), upper_omitted_median = median(upper_omitted, na.rm = TRUE)),
    by = .(response_id, split_scheme, method)]
}

mff_prepare_evidence <- function(cfg, prior, context, continuation, application, identity) {
  output <- file.path(cfg$output_root, "staging", cfg$evidence_dir)
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  read <- function(directory, name) data.table::fread(file.path(directory, name))
  validation <- file.path(continuation, "validation")
  s <- data.table::rbindlist(list(read(validation, "interval_results.csv"),
    read(validation, "comparator_results.csv")), fill = TRUE)
  cells <- mff_simulation_cells(s)
  mwd_write(cells, file.path(output, "simulation_cells.csv"))
  main <- mff_primary_cells(cells)
  mwd_write(main, file.path(output, "primary_widths.csv"))
  ranges <- main[, .(delivery_min = min(attainment), delivery_max = max(attainment)),
    by = .(n, content, method_id, method)]
  mwd_write(ranges, file.path(output, "primary_ranges.csv"))
  app <- data.table::rbindlist(list(read(application, "interval_results.csv"),
    read(application, "comparator_results.csv")), fill = TRUE)
  mwd_require(nrow(app) == 4 * 3004 && all(table(app$method) == 3004), "Incomplete application comparison.")
  values <- mff_application_values(app, context)
  pma_write_csv(as.data.frame(values), file.path(output, "application_values.csv"))
  mwd_write(mff_application_cells(values), file.path(output, "application_cells.csv"))
  numerical <- data.table::rbindlist(list(
    unique(s[method == "ecm"], by = "job_id")[, stage := "simulation"],
    unique(app[method == "ecm"], by = "job_id")[, stage := "application"]), fill = TRUE)
  assessment <- numerical[, .(attempts = .N, returned = sum(returned),
    accurate = sum(returned & ecm_coordinate_ok & gibbs_adequate), max_rhat = max(max_rhat, na.rm = TRUE),
    min_bulk_ess = min(min_bulk_ess, na.rm = TRUE), min_tail_ess = min(min_tail_ess, na.rm = TRUE),
    retained_1000 = sum(retained_per_chain == 1000, na.rm = TRUE),
    retained_2000 = sum(retained_per_chain == 2000, na.rm = TRUE),
    retained_4000 = sum(retained_per_chain == 4000, na.rm = TRUE),
    retained_longer = sum(retained_per_chain > 4000, na.rm = TRUE),
    max_coordinate_gap = max(ecm_coordinate_gap, na.rm = TRUE)), by = stage]
  mwd_write(assessment, file.path(output, "numerical_assessment.csv"))
  diagnostic <- file.path(continuation, "diagnostics")
  for (name in c("tasks.csv", "case_summary.csv", "mean_comparison.csv"))
    mwd_write(read(diagnostic, name), file.path(output, paste0("diagnostic_", name)))
  dp <- s[method %in% c("ecm", "gibbs"), .(attempts = .N,
    below_095 = sum(dp_content_probability < .95), minimum = min(dp_content_probability)), by = method]
  mwd_write(dp, file.path(output, "interval_dp_probabilities.csv"))
  source_files <- file.path(c(rep(validation, 2), rep(application, 2)),
    rep(c("interval_results.csv", "comparator_results.csv"), 2))
  files <- list.files(output, full.names = TRUE, pattern = "[.]csv$")
  hashes <- mgfp_hashes(files); names(hashes) <- basename(files)
  mgfp_json_write(list(prior = prior, source_commit = identity$source_commit,
    simulation_source = mwd_json(file.path(continuation, "manifest.json"))$identity$parent_commit,
    repair_and_application_source = cfg$continuation_commit, quantile_type = 7L,
    selected_target_posterior = TRUE, generalized_bayes_response_likelihood = FALSE,
    new_distribution_free_guarantee = FALSE, automatic_article_replacement = FALSE,
    diagnostic_cases_are_outcome_stratified = TRUE,
    inputs = mgfp_hashes(c(source_files, file.path(continuation, "repair_provenance.csv"),
      file.path(cfg$output_root, "application_fit_provenance.csv"))),
    data_files = hashes), file.path(output, "provenance.json"))
  output
}

mff_verify_evidence <- function(directory) {
  p <- mwd_json(file.path(directory, "provenance.json"))
  mwd_require(p$prior$primary_kappa == 2 &&
    identical(p$prior$simulation_reference$estimated_from_analysis_sample, FALSE) &&
    identical(p$prior$application_reference$estimated_from_analysis_sample, FALSE) &&
    !p$new_distribution_free_guarantee && !p$automatic_article_replacement,
    "Reporting interpretation or prior differs from the fixed-prior study.")
  for (name in names(p$data_files)) mwd_require(identical(mwd_file_hash(file.path(directory, name)), p$data_files[[name]]),
    paste("Reporting evidence changed:", name))
  invisible(p)
}
