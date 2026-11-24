mgsp_config <- function(path = "application/config/mti_gibbs_skewness_publication_20261122.json") {
  cfg <- mwd_json(path)
  cfg$primary <- msu_config(cfg$primary_config)
  cfg$study <- mgs_config(cfg$study_config)
  mwd_require(!cfg$refit && !cfg$change_primary_comparison && !cfg$change_calibrated_gibbs_study &&
    cfg$cases == 108L && cfg$added_cases == 36L && cfg$replications == 1000L &&
    identical(cfg$methods, c("ecm", "gibbs", "young_mathew", "wilks")) &&
    setequal(cfg$figure_order, cfg$primary$law_order) && length(cfg$figure_order) == 12L,
    "Invalid fixed-target Gibbs publication specification.")
  cfg$configuration_path <- path
  cfg
}

mgsp_programs <- c("application/scripts/151_publish_mti_gibbs_skewness.R",
  paste0("application/scripts/lib/", c("mti_gibbs_practical", "mti_gibbs_full_report_review",
    "mti_gibbs_full_publication", "mti_gibbs_width_diagnosis", "pharma_tolerance_application",
    "pharma_mti_application", "mti_fixed_prior_publication", "mti_fixed_prior_reporting",
    "mti_skewness_distributions", "mti_skewness_reporting", "mti_skewness_publication",
    "mti_gibbs_skewness", "mti_gibbs_skewness_reporting", "mti_gibbs_skewness_publication",
    "mti_gibbs_full_publication_figures"), ".R"))
mgsp_methods <- c(ecm = "MTI-ECM", gibbs = "MTI-Gibbs", young_mathew = "Young--Mathew", wilks = "Wilks (range)")
mgsp_colors <- c(ecm = "#009E73", gibbs = "#0072B2", young_mathew = "#CC79A7", wilks = "black")
mgsp_read <- function(cfg, name) data.table::fread(file.path(cfg$evidence_dir, paste0(name, ".csv")))

mgsp_validate <- function(cells, cfg) {
  validation <- cfg$primary; validation$methods <- cfg$methods
  msu_validate_cells(cells, validation)
  primary <- msu_read(cfg$primary, "primary_cells")
  msu_equal(cells[method != "gibbs", names(primary), with = FALSE], primary,
    c("law_id", "n", "content", "method"))
  old <- mgs_historical_cells(cfg$study)
  fields <- names(old)
  msu_equal(cells[law_id %in% old$law_id, ..fields], old,
    c("law_id", "n", "content", "method"))
  invisible(TRUE)
}

mgsp_endpoints <- function(rows, tasks, cfg) {
  g <- rows[method == "gibbs"][order(job_id)]
  e <- rows[method == "ecm"][order(job_id)]
  mwd_require(identical(g$job_id, e$job_id) && all(g$q == e$q & g$delta == e$delta) &&
    all(g$q == tasks$q[g$job_id] & g$delta == tasks$delta[g$job_id]), "Selected Gibbs targets changed.")
  spec <- mwd_json(cfg$study$parent_config)$laws
  supports <- setNames(vapply(seq_len(nrow(spec)), function(i) msx_law(as.list(spec[i, ]))$support[1], 0), spec$id)
  d <- g[, .(law_id, n, content, lower, upper, width, attained, dp_probability,
    ecm_probability_threshold, lower_below_support)]
  d[, `:=`(ecm_width = e$width, lower_extension = e$lower - lower,
    upper_extension = upper - e$upper, width_difference = width - e$width,
    below_support_extension = pmax(supports[law_id] - lower, 0) - pmax(supports[law_id] - e$lower, 0))]
  mwd_require(max(abs(d$width_difference - d$lower_extension - d$upper_extension)) < 1e-12,
    "Endpoint changes do not reconstruct the width difference.")
  d[, .(samples = .N, mean_width_gibbs = mean(width), mean_width_ecm = mean(ecm_width),
    mean_width_ratio = mean(width) / mean(ecm_width),
    mean_lower_extension = mean(lower_extension), mean_upper_extension = mean(upper_extension),
    mean_width_difference = mean(width_difference),
    lower_share = mean(lower_extension) / mean(width_difference),
    below_support_share = mean(below_support_extension) / mean(width_difference),
    lower_below_support = sum(lower_below_support),
    shorter_than_ecm = sum(width_difference < 0),
    below_ecm_cutoff = sum(dp_probability < ecm_probability_threshold),
    below_095 = sum(dp_probability < .95)), by = .(law_id, n, content)]
}

mgsp_diagnostic_extract <- function(cfg, directory, manifest, tasks) {
  example <- cfg$diagnostic_example
  task <- tasks[tasks$law_id == example$law_id & tasks$n == example$n &
    tasks$content == example$content & tasks$replication == example$replication, , drop = FALSE]
  mwd_require(nrow(task) == 1L && example$display_every == 4L, "Invalid diagnostic example.")
  parent <- file.path(manifest$parent, "fits", sprintf("fit_%06d.rds", task$job_id))
  mwd_require(mwd_file_hash(parent) == task$parent_sha256, "Diagnostic sample source changed.")
  y <- readRDS(parent)$sample$y
  result_path <- mgs_result_path(directory, task$job_id)
  mwd_require(file.exists(paste0(result_path, ".json")), "Missing diagnostic assessment receipt.")
  assessment <- mgs_read(result_path, task, cfg$study_identity, function(x)
    mwd_require(isTRUE(x$adequate) && x$sample_sha256 == task$sample_sha256 &&
      all(x$parameters$preferred_pass & x$parameters$mean_precision), "Inadequate diagnostic example."))
  count <- assessment$row$retained_per_chain
  paths <- vapply(1:4, function(chain) mgs_chain_path(directory, task$job_id, chain, count), "")
  draws <- data.table::rbindlist(lapply(1:4, function(chain) {
    t <- data.frame(chain = chain, seed = as.integer(cfg$study$chain_seed_base + 10L * task$job_id + chain),
      q = task$q, delta = task$delta, ecm_lower = task$ecm_lower, ecm_upper = task$ecm_upper)
    mwd_require(file.exists(paste0(paths[chain], ".json")), "Missing diagnostic chain receipt.")
    saved <- mgs_read(paths[chain], t, cfg$study_identity,
      function(x) mgs_validate_chain(x, y, cfg$study, count))
    d <- data.table::as.data.table(saved$fit$draws)
    d[, `:=`(width = upper - lower, midpoint = (upper + lower) / 2,
      chain = chain, iteration = seq_len(.N))]
    d
  }))
  mwd_require(max(abs(colMeans(draws[, .(lower, upper)]) -
    unlist(assessment$row[c("lower", "upper")], use.names = FALSE))) < 1e-12,
    "Diagnostic endpoint means differ from the completed assessment.")
  ranks <- data.table::rbindlist(lapply(c("lower", "upper", "width", "midpoint"), function(v) {
    bins <- pmin(20L, ceiling(rank(draws[[v]], ties.method = "average") * 20 / nrow(draws)))
    counts <- data.table::data.table(chain = draws$chain, bin = bins)[, .(count = .N), by = .(chain, bin)]
    out <- merge(data.table::CJ(chain = 1:4, bin = 1:20), counts, all.x = TRUE)
    out[is.na(count), count := 0L]; out[, variable := v]; out
  }))
  metadata <- data.table::data.table(job_id = task$job_id, law_id = task$law_id, n = task$n,
    content = task$content, replication = task$replication, q = task$q, delta = task$delta,
    sample_sha256 = task$sample_sha256, retained_per_chain = count, display_every = example$display_every,
    lower_mean = assessment$row$lower, upper_mean = assessment$row$upper)
  list(metadata = metadata, trace = draws[iteration %% example$display_every == 1L], ranks = ranks,
    sources = mgfp_hashes(c(parent, result_path, paste0(result_path, ".json"), paths, paste0(paths, ".json"))))
}

mgsp_collect <- function(cfg, directory) {
  verified <- mwd_json(file.path(directory, "verification.json"))
  manifest <- mwd_json(file.path(directory, "manifest.json"))
  exit <- mwd_json(file.path(directory, "controller_exit.json"))
  mwd_require(verified$completed && verified$numerically_adequate &&
    verified$datasets == 36000L && verified$adequate_datasets == 36000L &&
    verified$sampling_errors == 0L && verified$warning_datasets == 0L &&
    verified$original_primary_unchanged && !verified$article_replaced && exit$exit_code == 0L &&
    manifest$identity_hash == cfg$study_identity && verified$identity == cfg$study_identity &&
    manifest$identity$source_commit == cfg$study_source_commit &&
    !manifest$identity$prior_uses_analysis_data && !manifest$identity$q_delta_resampled,
    "The required completed, fixed-target Gibbs assessment is unavailable.")
  mgfp_check_hashes(verified$files)
  mwd_require(mwd_file_hash(file.path(directory, "tasks.rds")) == manifest$tasks_sha256,
    "The completed Gibbs task index changed.")
  tasks <- readRDS(file.path(directory, "tasks.rds"))
  raw <- data.table::fread(file.path(directory, "report/added_interval_results.csv"))
  rows <- mgs_complete_cases(raw, tasks)
  mwd_require(nrow(rows) == 144000L && !anyDuplicated(rows[, .(job_id, method)]) &&
    all(rows$returned) && all(rows[, .N, by = job_id]$N == 4L), "Incomplete paired Gibbs export.")
  g <- rows[method == "gibbs"]
  mwd_require(all(g$numerical_adequate & g$screen_pass), "A completed Gibbs mean is unresolved.")
  added <- msx_summary(rows)
  old <- mgs_historical_cells(cfg$study); fields <- names(old)
  cells <- mgs_bounds(data.table::rbindlist(list(old, added[, ..fields])))
  original <- data.table::fread(file.path(directory, "report/combined_cells.csv"))
  msu_equal(cells, original, c("law_id", "n", "content", "method"))
  mgsp_validate(cells, cfg)
  pairs <- mgs_paired(rows)
  msu_equal(pairs, data.table::fread(file.path(directory, "report/paired_added_comparison.csv")),
    c("law_id", "n", "content", "comparator"))
  diagnostic <- data.table::fread(file.path(directory, "report/numerical_diagnostics.csv"))
  parameters <- data.table::fread(file.path(directory, "report/parameter_summary.csv"))
  mwd_require(nrow(diagnostic) == 36000L && all(diagnostic$adequate & !diagnostic$sampling_error) &&
    all(diagnostic$warnings == 0L) && nrow(parameters) == 144000L &&
    all(parameters$preferred_pass & parameters$mean_precision), "Gibbs numerical assessment differs.")
  budget <- g$retained_per_chain[match(diagnostic$job_id, g$job_id)]
  diagnostic[, retained := budget]
  numerical <- diagnostic[, .(datasets = .N, adequate = sum(adequate), errors = sum(sampling_error),
    warnings = sum(warnings), max_rhat = max(max_rhat), min_bulk_ess = min(min_bulk_ess),
    min_tail_ess = min(min_tail_ess), retained_1000 = sum(retained == 1000L),
    retained_2000 = sum(retained == 2000L), retained_4000 = sum(retained == 4000L)),
    by = .(law_id, n, content)]
  endpoint <- mgsp_endpoints(rows, tasks, cfg)
  conditional <- data.table::rbindlist(lapply(which(tasks$replication == 1L), function(i) {
    t <- tasks[i, , drop = FALSE]
    path <- file.path(manifest$parent, "fits", sprintf("fit_%06d.rds", t$job_id))
    mwd_require(mwd_file_hash(path) == t$parent_sha256, "A conditional-mode reference fit changed.")
    sample <- readRDS(path)$sample$y
    mwd_require(mwd_hash(sample) == t$sample_sha256, "A conditional-mode reference sample changed.")
    lower <- mgp_conditional(sample, t$ecm_upper, t$q, t$delta, 1, .25)$mode
    upper <- mgp_conditional(sample, t$ecm_lower, t$q, t$delta, 1, .25)$mode
    data.table::data.table(law_id = t$law_id, n = t$n, content = t$content,
      replication = t$replication, lower_mode_gap = abs(lower - t$ecm_lower),
      upper_mode_gap = abs(upper - t$ecm_upper))
  }))
  example <- mgsp_diagnostic_extract(cfg, directory, manifest, tasks)
  dir.create(cfg$evidence_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(cfg$local_output, recursive = TRUE, showWarnings = FALSE)
  export <- file.path(cfg$local_output, "interval_results_complete_cases.csv")
  msu_write(rows, export)
  restored <- data.table::fread(export)
  for (field in c("lower", "upper", "width", "population_content", "q", "delta", "case_id"))
    mwd_require(identical(rows[[field]], restored[[field]]), paste("Gibbs export round trip differs:", field))
  values <- list(combined_cells = cells, added_cells = cells[law_id %in% cfg$study$law_ids],
    paired_comparison = pairs, numerical_summary = numerical, endpoint_comparison = endpoint,
    conditional_mode_checks = conditional, diagnostic_example = example$metadata,
    diagnostic_trace = example$trace, diagnostic_ranks = example$ranks)
  for (name in names(values)) msu_write(values[[name]], file.path(cfg$evidence_dir, paste0(name, ".csv")))
  for (name in c("manifest", "verification", "controller_exit"))
    mwd_require(file.copy(file.path(directory, paste0(name, ".json")),
      file.path(cfg$evidence_dir, paste0("completed_", name, ".json")), overwrite = TRUE), "Could not preserve study receipt.")
  files <- c(paste0(names(values), ".csv"), paste0("completed_", c("manifest", "verification", "controller_exit"), ".json"))
  hashes <- mgfp_hashes(file.path(cfg$evidence_dir, files)); names(hashes) <- files
  protected <- c(file.path(cfg$study$historical_evidence, c("provenance.json", "simulation_cells.csv", "application_cells.csv")),
    file.path(cfg$primary$evidence_dir, c("provenance.json", "primary_cells.csv", "population_content_references.csv")),
    "figures/data/mti_gibbs_decisions_20261118/provenance.json")
  mgfp_json_write(list(study_source_commit = cfg$study_source_commit, study_identity = cfg$study_identity,
    collection_base_commit = mgfp_git("rev-parse", "HEAD"),
    exdqlm_reference_commit = manifest$identity$exdqlm_reference_commit,
    cases = cfg$cases, added_cases = cfg$added_cases, replications = cfg$replications,
    prior_uses_analysis_data = FALSE, targets_resampled = FALSE, fits_rerun = FALSE,
    primary_comparison_changed = FALSE, calibrated_gibbs_study_changed = FALSE,
    endpoint_digits = 17L, quantile_type = 7L,
    posterior_tail_precision_failures = sum(!parameters$interval_precision),
    parameter_summaries = nrow(parameters), corrected_local_export = mgfp_hashes(export),
    collection_programs = mgfp_hashes(mgsp_programs), diagnostic_sources = example$sources,
    configuration = mgfp_hashes(c(cfg$configuration_path, cfg$primary_config, cfg$study_config,
      cfg$study$prior_config, cfg$study$diagnostic_config, cfg$study$parent_config)),
    protected_evidence = mgfp_hashes(protected), data_files = hashes), file.path(cfg$evidence_dir, "provenance.json"))
  mgsp_verify(cfg)
  cat("Collected 108 fixed-target Gibbs cases without refitting or modifying the frozen report.\n")
}

mgsp_verify <- function(cfg, rendered = FALSE) {
  msu_verify(cfg$primary)
  p <- mwd_json(file.path(cfg$evidence_dir, "provenance.json"))
  mwd_require(p$study_source_commit == cfg$study_source_commit && p$study_identity == cfg$study_identity &&
    p$cases == 108L && p$added_cases == 36L && p$replications == 1000L &&
    !p$prior_uses_analysis_data && !p$targets_resampled && !p$fits_rerun &&
    !p$primary_comparison_changed && !p$calibrated_gibbs_study_changed &&
    p$endpoint_digits == 17L && p$quantile_type == 7L, "Gibbs publication interpretation changed.")
  mgfp_check_hashes(p$configuration); mgfp_check_hashes(p$protected_evidence)
  for (name in names(p$data_files)) mwd_require(mwd_file_hash(file.path(cfg$evidence_dir, name)) == p$data_files[[name]],
    paste("Gibbs publication data changed:", name))
  cells <- mgsp_read(cfg, "combined_cells"); mgsp_validate(cells, cfg)
  added <- mgsp_read(cfg, "added_cells")
  msu_equal(added, cells[law_id %in% cfg$study$law_ids], c("law_id", "n", "content", "method"))
  d <- mgsp_read(cfg, "numerical_summary")
  mwd_require(nrow(d) == 36L && all(d$datasets == 1000L & d$adequate == 1000L & d$errors == 0L & d$warnings == 0L) &&
    all(d$max_rhat <= 1.01 & d$min_bulk_ess >= 400 & d$min_tail_ess >= 400) &&
    all(d$retained_1000 + d$retained_2000 + d$retained_4000 == 1000L), "Invalid compact Gibbs diagnostics.")
  endpoints <- mgsp_read(cfg, "endpoint_comparison")
  mwd_require(nrow(endpoints) == 36L && all(endpoints$samples == 1000L) &&
    all(abs(endpoints$mean_width_difference - endpoints$mean_lower_extension - endpoints$mean_upper_extension) < 1e-12) &&
    all(abs(endpoints$mean_width_ratio - endpoints$mean_width_gibbs / endpoints$mean_width_ecm) < 1e-12),
    "Endpoint decomposition is inconsistent.")
  conditional <- mgsp_read(cfg, "conditional_mode_checks")
  mwd_require(nrow(conditional) == 36L && all(conditional$replication == 1L) &&
    all(conditional$lower_mode_gap == 0 & conditional$upper_mode_gap == 0), "Conditional-mode checks differ.")
  example <- mgsp_read(cfg, "diagnostic_example")
  trace <- mgsp_read(cfg, "diagnostic_trace"); ranks <- mgsp_read(cfg, "diagnostic_ranks")
  specification <- cfg$diagnostic_example
  mwd_require(nrow(example) == 1L && example$law_id == specification$law_id &&
    example$n == specification$n && example$content == specification$content &&
    example$replication == specification$replication && example$display_every == specification$display_every &&
    example$q > example$content && example$q < 1 &&
    setequal(trace$chain, 1:4) && !anyDuplicated(trace[, .(chain, iteration)]) &&
    all(trace$iteration %% example$display_every == 1L) &&
    all(trace$iteration > 0 & trace$iteration <= example$retained_per_chain) &&
    all(is.finite(as.matrix(trace))) && all(trace$upper >= trace$lower) &&
    all(table(trace$chain) == ceiling(example$retained_per_chain / example$display_every)) &&
    all(abs(trace$width - trace$upper + trace$lower) < 1e-12) &&
    all(abs(trace$midpoint - (trace$lower + trace$upper) / 2) < 1e-12) &&
    nrow(ranks) == 320L && !anyDuplicated(ranks[, .(chain, bin, variable)]) &&
    setequal(ranks$chain, 1:4) && setequal(ranks$bin, 1:20) &&
    setequal(ranks$variable, c("lower", "upper", "width", "midpoint")) &&
    all(ranks$count >= 0L) && all(ranks[, .(count = sum(count)), by = .(chain, variable)]$count == example$retained_per_chain),
    "Diagnostic display does not match the specified saved-draw example.")
  wide <- data.table::dcast(added, law_id + n + content ~ method, value.var = "mean_width")
  msu_equal(endpoints[, .(law_id, n, content, mean_width_gibbs, mean_width_ecm)],
    wide[, .(law_id, n, content, mean_width_gibbs = gibbs, mean_width_ecm = ecm)], c("law_id", "n", "content"))
  pair <- mgsp_read(cfg, "paired_comparison")
  mwd_require(nrow(pair) == 108L && all(pair$samples == 1000L & pair$paired_widths == 1000L) &&
    !anyDuplicated(pair[, .(law_id, n, content, comparator)]), "Invalid paired Gibbs comparison.")
  for (m in c("ecm", "young_mathew", "wilks")) {
    a <- added[method == "gibbs"]; b <- added[method == m]
    z <- merge(a, b, by = c("law_id", "n", "content"), suffixes = c("_g", "_b"))
    expected <- z[, .(law_id, n, content, mean_width_difference = mean_width_g - mean_width_b,
      attainment_difference = attainment_g - attainment_b)]
    fields <- names(expected)
    msu_equal(pair[comparator == m, ..fields], expected, c("law_id", "n", "content"))
  }
  if (rendered) {
    r <- mwd_json(file.path(cfg$evidence_dir, "rendered.json"))
    mgfp_check_hashes(r$outputs); mgfp_check_hashes(r$programs)
    mwd_require(identical(readLines(file.path(cfg$table_dir, "numbers.tex")),
      mgsp_number_lines(mgsp_numbers(cfg))), "Displayed Gibbs numbers differ from evidence.")
  }
  invisible(TRUE)
}

mgsp_numbers <- function(cfg) {
  cells <- mgsp_read(cfg, "combined_cells"); g <- cells[method == "gibbs"]
  a <- mgsp_read(cfg, "added_cells")[method == "gibbs"]
  e <- mgsp_read(cfg, "endpoint_comparison"); n <- mgsp_read(cfg, "numerical_summary")
  w <- data.table::dcast(cells, law_id + n + content ~ method, value.var = "mean_width")
  number <- function(x) sprintf("%.1f", x)
  shares <- e[n == 1000 & content == .90, 100 * lower_share]
  assessment <- mgsp_assessment(cfg)
  c(MTIGibbsExpandedCases = "108", MTIGibbsExpandedSamples = "108,000",
    MTIGibbsAssessedDatasets = formatC(sum(assessment$datasets), format = "d", big.mark = ","),
    MTIGibbsAllRhat = sprintf("%.4f", max(assessment$max_rhat)),
    MTIGibbsAllBulkESS = as.character(floor(min(assessment$min_bulk_ess))),
    MTIGibbsAllTailESS = as.character(floor(min(assessment$min_tail_ess))),
    MTIGibbsExpandedWider = as.character(sum(w$gibbs > w$ecm)),
    MTIGibbsExpandedMin = number(100 * min(g$attainment)), MTIGibbsExpandedMax = number(100 * max(g$attainment)),
    MTIGibbsAddedMin = number(100 * min(a$attainment)), MTIGibbsAddedMax = number(100 * max(a$attainment)),
    MTIGibbsAddedWidthMin = number(100 * min(e$mean_width_ratio - 1)),
    MTIGibbsAddedWidthMax = number(100 * max(e$mean_width_ratio - 1)),
    MTIGibbsLowerShareMin = number(min(shares)), MTIGibbsLowerShareMax = number(max(shares)),
    MTIGibbsAddedRhat = sprintf("%.4f", max(n$max_rhat)),
    MTIGibbsAddedBulkESS = as.character(floor(min(n$min_bulk_ess))),
    MTIGibbsAddedTailESS = as.character(floor(min(n$min_tail_ess))),
    MTIGibbsAddedBelowCutoff = formatC(sum(e$below_ecm_cutoff), format = "d", big.mark = ","),
    MTIGibbsAddedBelowNominal = as.character(sum(e$below_095)),
    MTIGibbsExpandedStudentUpper = sprintf("%.4f", g[law_id == "student_t3" & n == 500 & content == .99, simultaneous_upper]))
}

mgsp_assessment <- function(cfg) {
  old <- data.table::fread(file.path(cfg$study$historical_evidence, "numerical_assessment.csv"))
  extra <- mgsp_read(cfg, "numerical_summary")
  modes <- msu_read(cfg$primary, "numerical_diagnostics")
  old[stage == "simulation", `:=`(attempts = attempts + sum(extra$datasets),
    max_rhat = max(max_rhat, extra$max_rhat), min_bulk_ess = min(min_bulk_ess, extra$min_bulk_ess),
    min_tail_ess = min(min_tail_ess, extra$min_tail_ess),
    max_coordinate_gap = max(max_coordinate_gap, modes$max_objective_gap))]
  old[, .(stage, datasets = attempts, max_rhat, min_bulk_ess, min_tail_ess, max_coordinate_gap)]
}

mgsp_number_lines <- function(values) c("% Generated from the completed fixed-target Gibbs extension.",
  sprintf("\\newcommand{\\%s}{%s}", names(values), values))

mgsp_renderer <- function(cfg) {
  renderer <- new.env(parent = environment())
  sys.source("application/scripts/lib/mti_gibbs_full_publication_figures.R", envir = renderer)
  labels <- setNames(cfg$primary$labels, cfg$primary$law_order)
  renderer$mgfp_families <- labels[cfg$figure_order]
  renderer$mgfp_methods <- mgsp_methods
  renderer
}

mgsp_tables <- function(cfg) {
  dir.create(cfg$table_dir, recursive = TRUE, showWarnings = FALSE)
  d <- mgsp_read(cfg, "combined_cells")
  d[, `:=`(response_id = law_id, datasets = samples, attainment_rate = attainment)]
  mgsp_renderer(cfg)$mgfp_simulation_table(as.data.frame(d), file.path(cfg$table_dir, "simulation_comparison.tex"))
  writeLines(mgsp_number_lines(mgsp_numbers(cfg)), file.path(cfg$table_dir, "numbers.tex"))
  assessment <- mgsp_assessment(cfg)
  lines <- c("\\begin{tabular}{@{}lrrrr@{}}", "\\toprule",
    "Data & Fits & Maximum $\\widehat R$ & Minimum ESS & Maximum gain\\\\",
    " & & & Bulk / tail & \\\\", "\\midrule")
  for (i in seq_len(nrow(assessment))) {
    a <- assessment[i]
    exponent <- floor(log10(a$max_coordinate_gap))
    mantissa <- signif(a$max_coordinate_gap / 10^exponent, 3L)
    if (mantissa >= 10) { mantissa <- mantissa / 10; exponent <- exponent + 1L }
    lines <- c(lines, sprintf("%s & %s & %.4f & %d / %d & $%.2f\\times10^{%d}$\\\\",
      if (a$stage == "simulation") "Simulation" else "Application", formatC(a$datasets, format = "d", big.mark = ","),
      a$max_rhat, floor(a$min_bulk_ess), floor(a$min_tail_ess),
      mantissa, exponent))
  }
  writeLines(c(lines, "\\bottomrule", "\\end{tabular}"), file.path(cfg$table_dir, "numerical_assessment.tex"))
}

mgsp_width_figures <- function(cfg) {
  renderer <- mgsp_renderer(cfg)
  cells <- mgsp_read(cfg, "combined_cells")
  references <- msu_read(cfg$primary, "population_content_references")
  paths <- character()
  for (part in 1:6) {
    path <- file.path(cfg$figure_dir, sprintf("width_ranges_%02d.pdf", part))
    grDevices::pdf(path, width = 8.4, height = 8, useDingbats = FALSE)
    graphics::par(mfrow = c(1, 2), mar = c(4.3, 4.8, 2.8, .8), oma = c(2.2, 0, .2, 0),
      cex = 1, xpd = FALSE)
    for (family in cfg$figure_order[(2 * part - 1):(2 * part)]) {
      z <- as.data.frame(cells[law_id == family])
      layout <- renderer$mgfp_layout(z)
      reference <- references[dgp_id == family]
      limits <- range(c(z$width_q025, z$width_q975, reference$width)) * c(.9, 1.1)
      graphics::plot(NA, xlim = limits, ylim = c(1, max(layout$design$center) + 2), log = "x",
        axes = FALSE, xlab = "Interval width (log scale)", ylab = "",
        main = renderer$mgfp_families[family], cex.main = .95)
      graphics::axis(1, cex.axis = .9)
      graphics::axis(2, at = layout$design$center,
        labels = sprintf("n = %d\nc = %.2f", layout$design$n, layout$design$content),
        las = 1, tick = FALSE, cex.axis = .9)
      graphics::abline(h = layout$design$center, col = "grey94")
      oracle <- reference$width[match(layout$design$content, reference$content)]
      graphics::segments(oracle, layout$design$center - 1.15, oracle, layout$design$center + 1.15,
        col = "grey40", lty = 2)
      for (i in seq_len(nrow(z))) {
        color <- mgsp_colors[z$method[i]]
        graphics::segments(z$width_q025[i], layout$y[i], z$width_q975[i], layout$y[i], col = color, lwd = 1.6)
        graphics::points(z$mean_width[i], layout$y[i], col = color, pch = 4, cex = .65)
      }
      graphics::box()
    }
    mgs_legend(gsub("--", "-", mgsp_methods), mgsp_colors)
    grDevices::dev.off()
    paths <- c(paths, path)
  }
  paths
}

mgsp_attainment_figure <- function(cfg) {
  d <- mgsp_read(cfg, "combined_cells")
  z <- d[, .(lower = min(attainment), upper = max(attainment)), by = .(n, content, method)]
  layout <- mgsp_renderer(cfg)$mgfp_layout(as.data.frame(z))
  path <- file.path(cfg$figure_dir, "attainment_ranges.pdf")
  grDevices::pdf(path, width = 7.5, height = 8, useDingbats = FALSE)
  graphics::par(mar = c(4.4, 5, 1.5, 1), oma = c(2.2, 0, 0, 0), cex = 1, xpd = FALSE)
  graphics::plot(NA, xlim = c(.85, 1), ylim = c(1, max(layout$design$center) + 2),
    axes = FALSE, xlab = "Content-attainment proportion", ylab = "")
  graphics::axis(1, at = seq(.85, 1, .025))
  graphics::axis(2, at = layout$design$center,
    labels = sprintf("n = %d\nc = %.2f", layout$design$n, layout$design$content),
    las = 1, tick = FALSE, cex.axis = .85)
  graphics::abline(h = layout$design$center, col = "grey94")
  graphics::abline(v = .95, col = "grey40", lty = 2)
  for (i in seq_len(nrow(z))) {
    color <- mgsp_colors[z$method[i]]
    graphics::segments(z$lower[i], layout$y[i], z$upper[i], layout$y[i], col = color, lwd = 2)
    graphics::segments(c(z$lower[i], z$upper[i]), layout$y[i] - .08,
      c(z$lower[i], z$upper[i]), layout$y[i] + .08, col = color)
  }
  graphics::box()
  mgs_legend(gsub("--", "-", mgsp_methods), mgsp_colors)
  grDevices::dev.off()
  path
}

mgsp_population_figure <- function(cfg) {
  d <- msu_read(cfg$primary, "population_content_references")
  mwd_require(nrow(d) == 36L && setequal(d$dgp_id, cfg$figure_order), "Incomplete population references.")
  path <- file.path(cfg$figure_dir, "population_shortest_intervals.pdf")
  grDevices::pdf(path, width = 8.4, height = 10, useDingbats = FALSE)
  graphics::par(mfrow = c(4, 3), mar = c(3, 2, 2, .5), oma = c(1.5, 1.5, 0, 0))
  labels <- setNames(cfg$primary$labels, cfg$primary$law_order)
  colors <- c("#009E73", "#0072B2", "#D55E00")
  for (law in cfg$figure_order) {
    z <- d[dgp_id == law][order(content)]
    graphics::plot(NA, xlim = range(c(z$lower, z$upper)), ylim = c(.5, 3.5),
      axes = FALSE, xlab = "", ylab = "", main = labels[law], cex.main = .9)
    graphics::axis(1, cex.axis = .8)
    graphics::axis(2, at = 1:3, labels = sprintf("%.2f", z$content), las = 1, tick = FALSE, cex.axis = .8)
    graphics::segments(z$lower, 1:3, z$upper, 1:3, col = colors, lwd = 2)
    graphics::points(z$lower, 1:3, col = colors, pch = 16, cex = .6)
    graphics::points(z$upper, 1:3, col = colors, pch = 16, cex = .6)
    graphics::box()
  }
  graphics::mtext("Standardized response", side = 1, outer = TRUE)
  graphics::mtext("Population content", side = 2, outer = TRUE)
  grDevices::dev.off()
  path
}

mgsp_diagnostic_figures <- function(cfg) {
  trace <- mgsp_read(cfg, "diagnostic_trace"); ranks <- mgsp_read(cfg, "diagnostic_ranks")
  labels <- c(lower = "Lower endpoint", upper = "Upper endpoint", width = "Width", midpoint = "Midpoint")
  colors <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7")
  paths <- file.path(cfg$figure_dir, c("diagnostic_traces.pdf", "diagnostic_ranks.pdf"))
  grDevices::pdf(paths[1], width = 8, height = 6, useDingbats = FALSE)
  graphics::par(mfrow = c(2, 2), mar = c(3.6, 4.2, 2, .5), oma = c(2.2, 0, 0, 0),
    mgp = c(2.1, .65, 0))
  for (v in names(labels)) {
    graphics::plot(NA, xlim = range(trace$iteration), ylim = range(trace[[v]]),
      xlab = "Retained iteration", ylab = labels[v], main = labels[v], cex.main = 1)
    for (ch in 1:4) {
      z <- trace[chain == ch]
      graphics::lines(z$iteration, z[[v]], col = grDevices::adjustcolor(colors[ch], .65), lwd = .5)
    }
  }
  mgs_legend(paste("Chain", 1:4), colors); grDevices::dev.off()
  grDevices::pdf(paths[2], width = 8, height = 8, useDingbats = FALSE)
  graphics::par(mfrow = c(4, 4), mar = c(2.5, 2.5, 2, .5), oma = c(1.8, 2, 0, 0))
  for (v in names(labels)) for (ch in 1:4) {
    z <- ranks[variable == v & chain == ch][order(bin)]
    graphics::barplot(z$count, col = colors[ch], border = NA,
      main = paste(labels[v], "- chain", ch), cex.main = .75, space = 0,
      ylim = c(0, max(ranks[variable == v, count]) * 1.05), axes = FALSE)
    graphics::axis(2, cex.axis = .65, las = 1)
    graphics::axis(1, at = c(.5, 9.5, 19.5), labels = c(1, 10, 20), cex.axis = .7)
    graphics::abline(h = sum(z$count) / nrow(z), lty = 2, col = "grey35")
  }
  graphics::mtext("Pooled-rank bin", side = 1, outer = TRUE)
  graphics::mtext("Count", side = 2, outer = TRUE)
  grDevices::dev.off()
  paths
}

mgsp_render <- function(cfg) {
  mgsp_verify(cfg)
  mgsp_tables(cfg)
  dir.create(cfg$figure_dir, recursive = TRUE, showWarnings = FALSE)
  figures <- c(mgsp_width_figures(cfg), mgsp_attainment_figure(cfg),
    mgsp_population_figure(cfg), mgsp_diagnostic_figures(cfg))
  dir.create(cfg$local_output, recursive = TRUE, showWarnings = FALSE)
  mgs_comparison_pdf(mgsp_read(cfg, "combined_cells"), cfg$study,
    file.path(cfg$local_output, "four_method_comparison.pdf"))
  outputs <- c(figures, file.path(cfg$table_dir, c("numbers.tex", "simulation_comparison.tex", "numerical_assessment.tex")))
  mgfp_json_write(list(outputs = mgfp_hashes(outputs),
    programs = mgfp_hashes(c(mgsp_programs, cfg$configuration_path))),
    file.path(cfg$evidence_dir, "rendered.json"))
  mgsp_verify(cfg, rendered = TRUE)
  cat("Rendered twelve-distribution fixed-target Gibbs tables and figures.\n")
}
