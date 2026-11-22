msu_config <- function(path = "application/config/mti_skewness_publication_20261122.json") {
  cfg <- mwd_json(path)
  mwd_require(!cfg$sample_gibbs && !cfg$refit && cfg$replications == 1000L &&
    cfg$primary_cases == cfg$historical_cases + cfg$added_cases &&
    identical(cfg$methods, c("ecm", "young_mathew", "wilks")) &&
    length(cfg$law_order) == 12L && !anyDuplicated(cfg$law_order) &&
    length(cfg$labels) == length(cfg$law_order), "Invalid publication design.")
  cfg$configuration_path <- path
  cfg
}

msu_programs <- c("application/scripts/147_publish_mti_skewness_extension.R",
  paste0("application/scripts/lib/", c("mti_gibbs_practical", "mti_gibbs_full_report_review",
    "mti_gibbs_full_publication", "mti_gibbs_width_diagnosis", "pharma_tolerance_application",
    "pharma_mti_application", "mti_fixed_prior_publication", "mti_fixed_prior_reporting",
    "mti_skewness_distributions", "mti_skewness_extension", "mti_skewness_reporting",
    "mti_skewness_publication", "mti_skewness_publication_figures"), ".R"),
  "figures/generate_tolerance_validation_primary_figure.R",
  "application/config/mti_skewness_extension_20261121.json",
  "application/config/mti_fixed_prior_20261114.json")
msu_methods <- c(ecm = "MTI-ECM", young_mathew = "Young--Mathew", wilks = "Wilks")
msu_method_ids <- c(ecm = "mti_ecm_adaptive_cell", young_mathew = "young_mathew", wilks = "wilks_minmax")

msu_read <- function(cfg, name) data.table::fread(file.path(cfg$evidence_dir, paste0(name, ".csv")))
msu_write <- function(x, path) pma_write_csv(as.data.frame(x), path)

msu_equal <- function(actual, expected, keys, tolerance = 1e-12) {
  a <- data.table::copy(data.table::as.data.table(actual))
  b <- data.table::copy(data.table::as.data.table(expected))
  data.table::setorderv(a, keys); data.table::setorderv(b, keys)
  mwd_require(isTRUE(all.equal(as.data.frame(a), as.data.frame(b), tolerance = tolerance)),
    "Publication summaries differ from the completed assessment.")
}

msu_validate_cells <- function(cells, cfg) {
  keys <- c("law_id", "n", "content", "tolerance_confidence", "method")
  mwd_require(nrow(cells) == cfg$primary_cases * length(cfg$methods) &&
    !anyDuplicated(cells[, ..keys]) && setequal(cells$law_id, cfg$law_order) &&
    setequal(cells$method, cfg$methods) && all(cells$samples == cfg$replications) &&
    all(cells$returned == cells$samples & cells$width_samples == cells$samples) &&
    all(cells$tolerance_confidence == .95) &&
    all(cells$attained >= 0 & cells$attained <= cells$samples) &&
    all(cells$attainment == cells$attained / cells$samples) &&
    all(is.finite(cells$mean_width) & cells$mean_width > 0 & cells$width_q025 > 0 &
      cells$width_q975 >= cells$width_q025) && all(cells$family_size == cfg$primary_cases),
    "Incomplete primary comparison or incorrect Monte Carlo family.")
  designs <- unique(cells[, .(n, content, tolerance_confidence)])
  expected_designs <- data.table::data.table(n = c(50L, 100L, 100L, rep(c(500L, 1000L), each = 3L)),
    content = c(.90, .90, .95, rep(c(.90, .95, .99), 2L)), tolerance_confidence = .95)
  msu_equal(designs, expected_designs, c("n", "content"))
  mwd_require(all(cells[, .N, by = .(law_id, method)]$N == 9L),
    "A distribution/design comparison is missing.")
  expected <- data.table::copy(cells)
  expected[, `:=`(lower_95 = 0, upper_95 = 1, simultaneous_lower = 0, simultaneous_upper = 1)]
  expected[attained > 0, lower_95 := qbeta(.025, attained, samples - attained + 1)]
  expected[attained < samples, upper_95 := qbeta(.975, attained + 1, samples - attained)]
  expected[attained > 0, simultaneous_lower := qbeta(.05 / (2 * cfg$primary_cases), attained, samples - attained + 1)]
  expected[attained < samples, simultaneous_upper := qbeta(1 - .05 / (2 * cfg$primary_cases), attained + 1, samples - attained)]
  msu_equal(cells, expected, keys)
  invisible(TRUE)
}

msu_primary_widths <- function(cells, cfg) {
  d <- data.table::copy(cells)
  d[, `:=`(dgp_id = law_id, dgp = cfg$labels[match(law_id, cfg$law_order)],
    method_id = unname(msu_method_ids[method]), method = unname(msu_methods[method]))]
  d
}

msu_ranges <- function(cells) {
  cells[, .(delivery_min = min(attainment), delivery_max = max(attainment)),
    by = .(n, content, method_id, method)][order(n, content, match(method_id, unname(msu_method_ids)))]
}

msu_collect <- function(cfg, directory, checkout) {
  input <- mwd_json(cfg$study_config)
  manifest <- mwd_json(file.path(directory, "manifest.json"))
  verified <- mwd_json(file.path(directory, "verification.json"))
  status <- mwd_json(file.path(directory, "study_status.json"))
  exit <- mwd_json(file.path(directory, "controller_exit.json"))
  source <- trimws(system2("git", c("--no-optional-locks", "-C", shQuote(checkout), "rev-parse", "HEAD"), stdout = TRUE))
  mwd_require(identical(source, cfg$study_source_commit) &&
    manifest$identity$source_commit == cfg$study_source_commit &&
    verified$passed && verified$datasets == cfg$added_cases * cfg$replications &&
    verified$method_records == 3L * cfg$added_cases * cfg$replications &&
    !verified$prior_uses_analysis_data && !verified$sample_gibbs &&
    status$stage == "complete_requires_scientific_review" && exit$exit_code == 0L,
    "A successfully completed frozen assessment is required.")
  report <- mwd_json(file.path(directory, "report/provenance.json"))
  mgfp_check_hashes(report$files)
  mwd_require(mwd_file_hash(file.path(directory, "interval_results.csv")) == verified$interval_results_sha256,
    "Completed interval export changed.")
  design <- readRDS(file.path(directory, "design.rds"))
  laws <- setNames(lapply(seq_len(nrow(input$laws)), function(i) msx_law(as.list(input$laws[i, ]))), input$laws$id)
  groups <- split(seq_len(nrow(design$tasks)), design$tasks$case_id)
  pieces <- parallel::mclapply(groups, function(jobs) {
    rows <- diagnostics <- vector("list", length(jobs))
    for (k in seq_along(jobs)) {
      id <- jobs[k]; task <- design$tasks[id, , drop = FALSE]
      value <- msx_read(file.path(directory, "fits", sprintf("fit_%06d.rds", id)), manifest$identity_hash, task)
      d <- value$anchor$candidates
      mass <- msx_content(laws[[task$law_id]], value$rows$lower, value$rows$upper)
      mwd_require(value$resolved && all(value$rows$returned & value$rows$resolved) &&
        value$anchor$prior$prior_sd == 2 && !value$anchor$prior$data_adaptive_prior &&
        identical(mass, value$rows$population_content) && all(value$rows$attained == (mass >= task$content)) &&
        all(d$numerically_accurate & d$count_stable), "An original binary fit failed publication checks.")
      rows[[k]] <- value$rows
      ecm <- value$rows[value$rows$method == "ecm", ]
      wilks <- value$rows[value$rows$method == "wilks", ]
      same <- abs(ecm$lower - wilks$lower) <= 1e-12 * pmax(1, abs(wilks$lower)) &&
        abs(ecm$upper - wilks$upper) <= 1e-12 * pmax(1, abs(wilks$upper))
      diagnostics[[k]] <- data.table::data.table(law_id = task$law_id, n = task$n, content = task$content,
        candidates = nrow(d), max_objective_gap = max(d$coordinate_gap),
        max_scaled_endpoint_gap = max(d$endpoint_gap / pmax(1, d$width)),
        initial_flag = value$anchor$selected$ecm_converged, same_wilks = same, warnings = length(value$warnings),
        trace_rows = nrow(value$anchor$selected_ecm_trace))
    }
    list(rows = data.table::rbindlist(rows), diagnostics = data.table::rbindlist(diagnostics))
  }, mc.cores = min(cfg$workers, parallel::detectCores()), mc.set.seed = FALSE)
  mwd_require(all(vapply(pieces, is.list, FALSE)), "A saved-fit collection failed.")
  new <- data.table::rbindlist(lapply(pieces, `[[`, "rows"))
  data.table::setorder(new, job_id, method)
  dir.create(cfg$local_output, recursive = TRUE, showWarnings = FALSE)
  corrected <- file.path(cfg$local_output, "interval_results_full_precision.csv")
  msu_write(new, corrected)
  restored <- data.table::fread(corrected)
  for (field in c("lower", "upper", "width", "population_content", "q", "delta"))
    mwd_require(identical(new[[field]], restored[[field]]), paste("Decimal round trip differs:", field))
  completed <- data.table::fread(file.path(directory, "interval_results.csv"))
  msu_equal(new[, .(task_key, method, returned, attained)], completed[, .(task_key, method, returned, attained)],
    c("task_key", "method"), tolerance = 0)
  history <- mwd_json(file.path(cfg$historical_evidence, "provenance.json"))
  historical <- msx_historical_rows(input, list(historical = history))
  columns <- c("study_id", "task_key", "law_id", "n", "content", "tolerance_confidence",
    "replication", "method", "returned", "lower", "upper", "width", "population_content", "attained")
  combined <- data.table::rbindlist(list(historical[, ..columns], new[, ..columns]))
  cells <- msx_summary(combined); paired <- msx_paired(combined)
  msu_validate_cells(cells, cfg)
  msu_equal(cells, data.table::fread(file.path(directory, "report/combined_case_summary.csv")),
    c("law_id", "n", "content", "method"))
  msu_equal(paired, data.table::fread(file.path(directory, "report/paired_comparison.csv")),
    c("law_id", "n", "content", "comparator"))
  original <- data.table::fread(file.path(cfg$historical_evidence, "simulation_cells.csv"))[method != "gibbs"]
  original[, law_id := response_id]
  compare <- c("law_id", "n", "content", "method", "attainment", "mean_width", "width_q025", "width_q975")
  msu_equal(cells[law_id %in% original$law_id, ..compare], original[, ..compare], compare[1:4])
  diagnostics <- data.table::rbindlist(lapply(pieces, `[[`, "diagnostics"))[, .(datasets = .N,
    candidates = sum(candidates), max_objective_gap = max(max_objective_gap),
    max_scaled_endpoint_gap = max(max_scaled_endpoint_gap), strict_initial_flags_true = sum(initial_flag),
    same_wilks_intervals = sum(same_wilks), warnings = sum(warnings),
    trace_rows_min = min(trace_rows), trace_rows_max = max(trace_rows)),
    by = .(law_id, n, content)]
  numerical <- mwd_json("application/config/mti_fixed_prior_20261114.json")$numerical
  mwd_require(sum(diagnostics$datasets) == cfg$added_cases * cfg$replications &&
    all(diagnostics$max_objective_gap <= numerical$objective_tolerance &
      diagnostics$max_scaled_endpoint_gap <= numerical$endpoint_tolerance), "Numerical accuracy limits exceeded.")
  dir.create(cfg$evidence_dir, recursive = TRUE, showWarnings = FALSE)
  values <- list(primary_cells = cells, primary_widths = msu_primary_widths(cells, cfg),
    paired_comparison = paired, added_cells = cells[law_id %in% unique(new$law_id)],
    numerical_diagnostics = diagnostics,
    population_content_references = data.table::fread(file.path(directory, "report/population_content_references.csv")),
    distribution_admission = data.table::fread(file.path(directory, "distribution_admission.csv")))
  values$primary_ranges <- msu_ranges(values$primary_widths)
  for (name in names(values)) msu_write(values[[name]], file.path(cfg$evidence_dir, paste0(name, ".csv")))
  files <- file.path(cfg$evidence_dir, paste0(names(values), ".csv"))
  hashes <- mgfp_hashes(files); names(hashes) <- basename(files)
  mgfp_json_write(list(study_source_commit = cfg$study_source_commit,
    exdqlm_reference_commit = manifest$identity$exdqlm_reference_commit,
    numerical_package_source_commit = manifest$identity$numerical_package_source_commit,
    original_study_identity = manifest$identity_hash,
    original_verification = verified, original_report = mgfp_hashes(file.path(directory, "report/provenance.json")),
    packages = lapply(manifest$identity$packages[setdiff(names(manifest$identity$packages), "runtime_inventory")],
      function(x) x$version), R = manifest$identity$R,
    original_study_directory = directory, local_full_precision_export = mgfp_hashes(corrected),
    original_exports_unchanged = TRUE, endpoint_digits = 17L, quantile_type = 7L,
    added_cases = cfg$added_cases, primary_cases = cfg$primary_cases, replications = cfg$replications,
    prior_uses_analysis_data = FALSE, new_gibbs_fits = FALSE, fits_rerun = FALSE,
    historical_hashes = mgfp_hashes(file.path(cfg$historical_evidence,
      c("provenance.json", "reporting_provenance.json", "simulation_cells.csv", "application_cells.csv"))),
    configuration = mgfp_hashes(cfg$configuration_path), collection_programs = mgfp_hashes(msu_programs),
    data_files = hashes), file.path(cfg$evidence_dir, "provenance.json"))
  msu_verify(cfg)
  cat("Collected 108 primary cases; original Gibbs and application evidence preserved.\n")
}

msu_verify <- function(cfg, rendered = FALSE) {
  p <- mwd_json(file.path(cfg$evidence_dir, "provenance.json"))
  mwd_require(p$study_source_commit == cfg$study_source_commit && p$endpoint_digits == 17L &&
    p$primary_cases == cfg$primary_cases && p$added_cases == cfg$added_cases &&
    p$replications == cfg$replications &&
    p$quantile_type == 7L && !p$prior_uses_analysis_data && !p$new_gibbs_fits && !p$fits_rerun &&
    p$original_verification$passed && p$original_exports_unchanged, "Publication method or provenance changed.")
  mgfp_check_hashes(p$configuration); mgfp_check_hashes(p$historical_hashes)
  for (name in names(p$data_files)) mwd_require(mwd_file_hash(file.path(cfg$evidence_dir, name)) == p$data_files[[name]],
    paste("Publication data changed:", name))
  s <- msu_read(cfg, "primary_cells")
  msu_validate_cells(s, cfg)
  msu_equal(msu_read(cfg, "primary_widths"), msu_primary_widths(s, cfg), c("law_id", "n", "content", "method_id"))
  msu_equal(msu_read(cfg, "primary_ranges"), msu_ranges(msu_primary_widths(s, cfg)), c("n", "content", "method_id"))
  added <- msu_read(cfg, "added_cells")
  mwd_require(nrow(added) == cfg$added_cases * 3L && data.table::uniqueN(added$law_id) == 4L &&
    all(added$law_id %in% cfg$law_order), "The added distribution comparison changed.")
  msu_equal(added, s[law_id %in% added$law_id], c("law_id", "n", "content", "method"))
  d <- msu_read(cfg, "numerical_diagnostics")
  mwd_require(nrow(d) == cfg$added_cases && all(d$datasets == cfg$replications & d$warnings == 0L) &&
    all(d$max_objective_gap <= 1e-6 & d$max_scaled_endpoint_gap <= 1e-5), "Invalid original fit diagnostics.")
  boundary <- d[(n == 50 & content == .90) | (n == 100 & content == .95) | (n == 500 & content == .99)]
  mwd_require(nrow(boundary) == 12L && all(boundary$same_wilks_intervals == cfg$replications),
    "Boundary endpoint agreement differs from the saved fits.")
  old <- data.table::fread(file.path(cfg$historical_evidence, "simulation_cells.csv"))[method != "gibbs"]
  old[, law_id := response_id]
  columns <- c("law_id", "n", "content", "method", "attainment", "mean_width", "width_q025", "width_q975")
  msu_equal(s[law_id %in% old$law_id, ..columns], old[, ..columns], columns[1:4])
  mwd_require(!any(added$law_id %in% old$law_id), "Added and historical distributions overlap.")
  pairs <- msu_read(cfg, "paired_comparison")
  mwd_require(nrow(pairs) == 2L * cfg$primary_cases && all(pairs$samples == cfg$replications) &&
    all(pairs$width_paired_samples == cfg$replications) &&
    setequal(pairs$comparator, c("young_mathew", "wilks")) &&
    !anyDuplicated(pairs[, .(law_id, n, content, comparator)]), "Invalid paired comparisons.")
  for (comparator_name in unique(pairs$comparator)) {
    a <- s[method == "ecm"]; b <- s[method == comparator_name]
    keys <- c("law_id", "n", "content", "tolerance_confidence")
    joined <- merge(a, b, by = keys, suffixes = c("_ecm", "_other"))
    expected <- joined[, .(law_id, n, content, tolerance_confidence,
      attainment_difference = attainment_ecm - attainment_other,
      mean_width_difference = mean_width_ecm - mean_width_other)]
    fields <- names(expected)
    msu_equal(pairs[comparator == comparator_name, ..fields], expected, keys)
  }
  references <- msu_read(cfg, "population_content_references")
  mwd_require(nrow(references) == 36L && !anyDuplicated(references[, .(dgp_id, content)]) &&
    setequal(references$dgp_id, cfg$law_order) && all(is.finite(references$width) & references$width > 0) &&
    all(abs(references$width - (references$upper - references$lower)) < 1e-12),
    "Population width references are incomplete or inconsistent.")
  if (rendered) {
    receipt <- mwd_json(file.path(cfg$evidence_dir, "rendered.json"))
    mgfp_check_hashes(receipt$outputs); mgfp_check_hashes(receipt$programs)
    expected <- msu_numbers(cfg)
    text <- readLines(file.path(cfg$table_dir, "numbers.tex"))
    mwd_require(identical(text, msu_number_lines(expected)), "Displayed numerical macros differ from the evidence.")
  }
  invisible(TRUE)
}

msu_numbers <- function(cfg) {
  s <- msu_read(cfg, "primary_cells"); added <- msu_read(cfg, "added_cells")
  p <- msu_read(cfg, "paired_comparison")
  e <- s[method == "ecm"]; a <- added[method == "ecm"]
  wide <- data.table::dcast(added, law_id + n + content ~ method, value.var = "mean_width")
  change <- 100 * (1 - wide$ecm / wide$young_mathew)
  warning <- s[method == "young_mathew" & law_id == "invgauss_shape025" & n == 500 & content == .95]
  number <- function(x) sprintf("%.1f", x)
  c(MTIExpandedSettings = as.character(length(cfg$law_order)), MTIExpandedCases = as.character(cfg$primary_cases),
    MTIExpandedECMMin = number(100 * min(e$attainment)), MTIExpandedECMMax = number(100 * max(e$attainment)),
    MTIExpandedYMMin = number(100 * min(s[method == "young_mathew", attainment])),
    MTIExpandedYMMax = number(100 * max(s[method == "young_mathew", attainment])),
    MTIExpandedWilksMin = number(100 * min(s[method == "wilks", attainment])),
    MTIExpandedWilksMax = number(100 * max(s[method == "wilks", attainment])),
    MTIExpandedECMShorter = as.character(sum(p[comparator == "young_mathew", mean_width_difference] < 0)),
    MTIExpandedECMLower = as.character(sum(e$simultaneous_lower > .95)),
    MTIExpandedECMInclude = as.character(sum(e$simultaneous_lower <= .95 & e$simultaneous_upper >= .95)),
    MTIExpandedNewMin = number(100 * min(a$attainment)), MTIExpandedNewMax = number(100 * max(a$attainment)),
    MTIExpandedNewGainMin = number(min(change[change > 0])), MTIExpandedNewGainMax = number(max(change)),
    MTIExpandedNewLossMin = number(min(-change[change < 0])), MTIExpandedNewLossMax = number(max(-change)),
    MTIExpandedYMWarning = number(100 * warning$attainment),
    MTIExpandedYMWarningLower = number(100 * warning$lower_95),
    MTIExpandedYMWarningUpper = number(100 * warning$upper_95),
    MTIExpandedYMWarningAdjustedUpper = number(100 * warning$simultaneous_upper))
}

msu_number_lines <- function(values) c("% Generated from the expanded fixed-prior comparison.",
  sprintf("\\newcommand{\\%s}{%s}", names(values), values))

msu_tables <- function(cfg) {
  dir.create(cfg$table_dir, recursive = TRUE, showWarnings = FALSE)
  mfr_primary_table(msu_read(cfg, "primary_ranges"), file.path(cfg$table_dir, "primary_ranges.tex"))
  writeLines(msu_number_lines(msu_numbers(cfg)), file.path(cfg$table_dir, "numbers.tex"))
  d <- msu_read(cfg, "added_cells")
  labels <- setNames(cfg$labels, cfg$law_order)
  heading <- "Distribution & Method & Attainment (95\\% CI) & 95\\% width range \\\\"
  lines <- c("\\begin{longtable}{@{}llrr@{}}",
    "\\caption{Additional asymmetric distributions. Attainment percentages have pointwise exact 95\\% binomial confidence intervals. Width ranges are empirical 2.5\\%--97.5\\% quantiles over 1000 replications within each distribution, design, and method. All lengths use the population-standardized response scale.}",
    "\\label{tab:supp-mti-added-simulation}\\\\", "\\toprule", heading, "\\midrule", "\\endfirsthead",
    "\\multicolumn{4}{l}{\\tablename~\\thetable{} (continued)}\\\\",
    "\\toprule", heading, "\\midrule", "\\endhead")
  designs <- unique(d[, .(n, content)])[order(n, content)]
  for (i in seq_len(nrow(designs))) {
    design <- designs[i]
    lines <- c(lines, sprintf("\\multicolumn{4}{l}{\\textbf{\\(n=%d, c=%.2f, p=0.95\\)}} \\\\*", design$n, design$content))
    for (id in cfg$law_order[cfg$law_order %in% d$law_id]) for (method_name in cfg$methods) {
      z <- d[law_id == id & n == design$n & content == design$content & method == method_name]
      lines <- c(lines, sprintf("%s & %s & %.1f [%.1f, %.1f] & [%.3f, %.3f]%s",
        if (method_name == cfg$methods[1]) labels[[id]] else "", msu_methods[[method_name]],
        100*z$attainment, 100*z$lower_95, 100*z$upper_95, z$width_q025, z$width_q975,
        if (method_name == tail(cfg$methods, 1)) " \\\\" else " \\\\*"))
    }
    if (i %% 3L == 0L && i < nrow(designs)) lines <- c(lines, "\\newpage") else
      if (i < nrow(designs)) lines <- c(lines, "\\addlinespace[0.4em]")
  }
  writeLines(c(lines, "\\bottomrule", "\\end{longtable}"), file.path(cfg$table_dir, "added_comparison.tex"))
}

msu_render <- function(cfg) {
  msu_verify(cfg)
  msu_tables(cfg)
  dir.create(cfg$figure_dir, recursive = TRUE, showWarnings = FALSE)
  arguments <- c("figures/generate_tolerance_validation_primary_figure.R", "--primary-results=", "--mti-ecm-results=",
    paste0("--scenario-range-csv=", cfg$evidence_dir, "/primary_ranges.csv"),
    paste0("--output-dir=", cfg$figure_dir), "--delivery-y-min=0.85", "--mti-label=MTI-ECM")
  mwd_require(system2("Rscript", shQuote(arguments)) == 0L, "Expanded attainment figure failed.")
  msu_width_figures(cfg)
  programs <- c(msu_programs, cfg$configuration_path)
  files <- c(list.files(cfg$table_dir, full.names = TRUE), list.files(cfg$figure_dir, full.names = TRUE))
  mgfp_json_write(list(outputs = mgfp_hashes(files), programs = mgfp_hashes(programs)),
    file.path(cfg$evidence_dir, "rendered.json"))
  msu_verify(cfg, rendered = TRUE)
  cat("Rendered expanded primary figures and tables.\n")
}
