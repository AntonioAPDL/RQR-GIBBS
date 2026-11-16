mfr_read <- function(directory, name) data.table::fread(file.path(directory, paste0(name, ".csv")))
mfr_labels <- c(ecm = "MTI-ECM", gibbs = "MTI-Gibbs", young_mathew = "Young--Mathew", wilks = "Wilks")
mfr_colors <- c(ecm = "#009E73", gibbs = "#0072B2", young_mathew = "#CC79A7", wilks = "black")
mfr_number <- function(x, digits = 3) sprintf(paste0("%.", digits, "f"), x)
mfr_range <- function(x, y, digits = 3) paste0(mfr_number(x, digits), "--", mfr_number(y, digits))

mfr_application_input <- function(values) {
  d <- as.data.frame(values[values$method != "gibbs", ])
  d$method_id <- unname(mff_ids[d$method]); d$method <- unname(mfr_labels[d$method])
  d$interval_returned <- d$returned
  d$heldout_empirical_content <- d$heldout_content
  d$heldout_lower_omitted <- d$lower_omitted; d$heldout_upper_omitted <- d$upper_omitted
  d$response_role <- ifelse(d$response_id == "fct_tensile", "primary", "supplement")
  d$method_role <- ifelse(d$method_id == "mti_ecm_adaptive_cell", "proposed", "benchmark")
  d
}

mfr_collect <- function(cfg) {
  final <- file.path(mgfp_common(), cfg$finalization_worktree, cfg$output_root)
  status <- mwd_json(file.path(final, "status.json"))
  exit <- mwd_json(file.path(final, "controller_exit.json"))
  mwd_require(status$phase == "ready_for_manuscript_review" && exit$exit_code == 0 &&
    status$application_numerical_pass && status$diagnostic_numerical_pass,
    "Completed numerical assessment is required before manuscript preparation.")
  manifest <- mwd_json(file.path(final, "manifest.json"))
  mwd_require(manifest$identity$source_commit == cfg$finalization_commit, "Finalization source differs.")
  staged <- file.path(final, "staging", cfg$evidence_dir)
  p <- mff_verify_evidence(staged)
  dir.create(cfg$evidence_dir, recursive = TRUE, showWarnings = FALSE)
  for (name in c(names(p$data_files), "provenance.json"))
    mwd_require(file.copy(file.path(staged, name), file.path(cfg$evidence_dir, name), overwrite = TRUE),
      "Cannot copy reporting evidence.")
  diagnostic <- mwd_json(mwd_json(cfg$prior_config)$diagnostic_config)
  app_cfg <- mwd_json(diagnostic$application_config)
  observed_path <- file.path(app_cfg$paths$canonical_dir, "observed_batches.csv")
  continuation <- manifest$identity$configuration
  original_manifest <- mwd_json(file.path(mgfp_common(), continuation$continuation_worktree,
    continuation$continuation_output, "manifest.json"))
  mwd_require(mwd_file_hash(observed_path) == original_manifest$identity$parent_runtime$application_data[[observed_path]],
    "Application observations differ from those used for fitting.")
  observed <- data.table::fread(observed_path)
  pma_write_csv(as.data.frame(observed), file.path(cfg$evidence_dir, "observed_batches.csv"))
  values <- mfr_read(cfg$evidence_dir, "application_values")
  sharing_path <- "figures/data/pharma_source_structure_ecm_20261111/partition_sharing.csv"
  sharing <- data.table::fread(sharing_path)
  shared <- merge(values, sharing, by = c("split_scheme", "split_id"), all.x = TRUE)
  mwd_require(!anyNA(shared$campaign_shared) && nrow(shared) == nrow(values), "Application partitions differ from the campaign assessment.")
  subset <- shared[split_scheme == "month_grouped" & campaign_shared == 0]
  subset[, split_scheme := "campaign_disjoint_months"]
  mwd_write(mff_application_cells(subset), file.path(cfg$evidence_dir, "campaign_cells.csv"))
  representatives <- pma_representative(mfr_application_input(values))
  mwd_write(representatives, file.path(cfg$evidence_dir, "representative_splits.csv"))
  parent <- file.path(mgfp_common(), ".codex_work", paste0("mti_fixed_prior_", p$simulation_source),
    "application/outputs/mti_fixed_prior_20261114")
  calibration <- mfr_read(file.path(parent, "calibration"), "cell_summary")
  mwd_write(calibration, file.path(cfg$evidence_dir, "calibration_cells.csv"))
  tasks <- data.table::fread(file.path(parent, "validation/tasks.csv"))
  selected <- tasks[(response_id == "normal" & n == 1000 & content == .9) |
    (response_id == "student_t3" & n == 500 & content == .99) |
    (response_id == "exponential" & n == 1000 & content == .9), .SD[which.min(job_id)],
    by = .(response_id, n, content)]
  paths <- file.path(parent, "validation/fits", sprintf("fit_%06d.rds", selected$job_id))
  app <- values[response_id == "fct_tensile" & method == "ecm" & split_scheme == "random" &
    split_id == representatives$split_id[representatives$response_id == "fct_tensile"]]
  provenance <- data.table::fread(file.path(final, "application_fit_provenance.csv"))
  app_path <- provenance$original_path[match(app$job_id, provenance$job_id)]
  extended <- provenance$refined_path[match(app$job_id, provenance$job_id)]
  if (!is.na(extended) && nzchar(extended)) app_path <- extended
  paths <- c(paths, app_path)
  examples <- cdf <- trace <- ranks <- list()
  for (i in seq_along(paths)) {
    r <- readRDS(paths[i]); e <- r$rows[r$rows$method == "ecm", ]
    mwd_require(isTRUE(r$returned) && all(r$rows$gibbs_adequate) && r$prior$prior_sd == 2,
      "A displayed posterior does not use the adequate fixed-prior calculation.")
    draws <- data.table::rbindlist(lapply(seq_along(r$fits), function(chain) {
      z <- as.data.frame(r$fits[[chain]]$draws)
      z$width <- z$upper - z$lower; z$midpoint <- (z$upper + z$lower) / 2
      z$chain <- chain; z$iteration <- seq_len(nrow(z)); z
    }))
    examples[[i]] <- data.frame(case_id = i, response_id = r$task$response_id, n = r$task$n,
      content = r$task$content, q = r$q, delta = r$delta_reference, ecm_width = e$width,
      retained = nrow(r$fits[[1]]$draws))
    cdf[[i]] <- data.frame(case_id = i, probability = seq(0, 1, length.out = 201),
      width = quantile(draws$width, seq(0, 1, length.out = 201), names = FALSE))
    if (i == 1L) {
      trace <- draws[iteration %% 4L == 1L]
      ranks <- data.table::rbindlist(lapply(c("lower", "upper", "width", "midpoint"), function(v) {
        bins <- pmin(20L, ceiling(rank(draws[[v]], ties.method = "average") * 20 / nrow(draws)))
        data.table::data.table(chain = draws$chain, bin = bins)[, .(count = .N), by = .(chain, bin)][, variable := v]
      }))
    }
  }
  for (name in c("examples", "cdf", "trace", "ranks")) {
    value <- get(name)
    if (name %in% c("examples", "cdf")) value <- data.table::rbindlist(value)
    mwd_write(value, file.path(cfg$evidence_dir, paste0(name, ".csv")))
  }
  files <- list.files(cfg$evidence_dir, pattern = "[.]csv$", full.names = TRUE)
  hashes <- mgfp_hashes(files); names(hashes) <- basename(files)
  mgfp_json_write(list(finalization_commit = cfg$finalization_commit,
    staged_provenance_sha256 = mwd_file_hash(file.path(staged, "provenance.json")),
    extra_sources = mgfp_hashes(c(observed_path, sharing_path, file.path(parent, "calibration/cell_summary.csv"), paths)),
    data_files = hashes, quantile_type = 7L, prior_uses_analysis_data = FALSE),
    file.path(cfg$evidence_dir, "reporting_provenance.json"))
  invisible(cfg$evidence_dir)
}

mfr_verify <- function(cfg, rendered = FALSE) {
  mff_verify_evidence(cfg$evidence_dir)
  p <- mwd_json(file.path(cfg$evidence_dir, "reporting_provenance.json"))
  mwd_require(!p$prior_uses_analysis_data && p$finalization_commit == cfg$finalization_commit,
    "Reporting prior or source differs.")
  mwd_require(mwd_file_hash(file.path(cfg$evidence_dir, "provenance.json")) == p$staged_provenance_sha256,
    "The original evidence attribution changed.")
  for (name in names(p$data_files)) mwd_require(
    mwd_file_hash(file.path(cfg$evidence_dir, name)) == p$data_files[[name]], "Reporting observations changed.")
  s <- mfr_read(cfg$evidence_dir, "simulation_cells")
  a <- mfr_read(cfg$evidence_dir, "application_cells")
  z <- mfr_read(cfg$evidence_dir, "numerical_assessment")
  mwd_require(nrow(s) == 288L && all(s$attempts == 1000L & s$returned == 1000L) &&
    all(a$attempts == a$returned) && all(z$attempts == z$accurate) &&
    sum(a$attempts) == 4 * 3004, "Reporting requires complete, numerically adequate comparisons.")
  mwd_require(setequal(s$response_id, names(mgfp_families)) &&
    all(s$successes >= 0 & s$successes <= s$attempts & is.finite(s$mean_width) & s$width_q025 > 0 &
      s$width_q025 <= s$width_q975) && all(z$attempts[z$stage == "simulation"] == 72000L) &&
    all(z$attempts[z$stage == "application"] == 3004L), "Invalid distributional or numerical summaries.")
  if (rendered) {
    p <- mwd_json(file.path(cfg$evidence_dir, "rendered.json"))
    mgfp_check_hashes(p$outputs)
    mgfp_check_hashes(p$programs)
  }
  invisible(TRUE)
}

mfr_primary_table <- function(ranges, path) {
  d <- ranges[order(n, content, match(method_id, unname(mff_ids[c("ecm", "young_mathew", "wilks")])))]
  lines <- c("\\begin{tabular}{@{}l@{\\hspace{0.75em}}l@{\\hspace{0.75em}}r@{}}",
    "\\toprule", "Design & Method & Content-attainment range (\\%)\\\\", "\\midrule")
  for (i in seq_len(nrow(d))) {
    r <- d[i]; first <- i == 1L || r$n != d$n[i-1L] || r$content != d$content[i-1L]
    if (first && i > 1L) lines <- c(lines, "\\addlinespace[0.3em]")
    lines <- c(lines, sprintf("%s & %s & %s\\\\", if (first)
      sprintf("\\(n=%d, c=%.2f\\)", r$n, r$content) else "", r$method,
      mfr_range(100 * r$delivery_min, 100 * r$delivery_max, 1)))
  }
  writeLines(c(lines, "\\bottomrule", "\\end{tabular}"), path)
}

mfr_application_table <- function(d, response, path) {
  z <- d[response_id == response & split_scheme == "random" & method != "gibbs"]
  z <- z[match(c("ecm", "young_mathew", "wilks"), method)]
  lines <- c("\\begin{tabular}{@{}lrrr@{}}", "\\toprule",
    "Method & Width range & Held-out content (\\%) & Held-out $\\ge c$ (\\%)\\\\", "\\midrule")
  for (i in seq_len(nrow(z))) {
    r <- z[i]
    lines <- c(lines, sprintf("%s & %s & %.1f [%s] & %.1f\\\\", mfr_labels[r$method],
      mfr_range(r$width_q025, r$width_q975), 100*r$content_median,
      mfr_range(100*r$content_q025, 100*r$content_q975, 1), 100*r$attainment))
  }
  writeLines(c(lines, "\\bottomrule", "\\end{tabular}"), path)
}

mfr_numbers <- function(cfg) {
  s <- mfr_read(cfg$evidence_dir, "simulation_cells")
  a <- mfr_read(cfg$evidence_dir, "application_cells")
  v <- mfr_read(cfg$evidence_dir, "application_values")
  z <- mfr_read(cfg$evidence_dir, "numerical_assessment")
  reps <- mfr_read(cfg$evidence_dir, "representative_splits")
  wide <- data.table::dcast(s, response_id + n + content ~ method, value.var = "mean_width")
  mean_at <- function(family, wanted_method) s[response_id == family & n == 1000 & content == .9 &
    method == wanted_method, mean_width]
  app <- function(wanted_method, variable) a[response_id == "fct_tensile" & split_scheme == "random" &
    method == wanted_method][[variable]]
  count <- function(x) formatC(x, digits = 0, format = "f", big.mark = ",")
  values <- c(MTIFullDatasets = count(sum(z$attempts)),
    MTIFullSimulations = count(z[stage == "simulation", attempts]),
    MTIFullApplications = count(z[stage == "application", attempts]),
    MTIFullRhat = mfr_number(max(z$max_rhat), 4), MTIFullBulkESS = as.character(floor(min(z$min_bulk_ess))),
    MTIFullTailESS = as.character(floor(min(z$min_tail_ess))),
    MTIFullAttainmentMin = mfr_number(100*min(s[method == "gibbs", attainment]), 1),
    MTIFullAttainmentMax = mfr_number(100*max(s[method == "gibbs", attainment]), 1),
    MTIFullNormalECM = mfr_number(mean_at("normal", "ecm"), 2),
    MTIFullNormalGibbs = mfr_number(mean_at("normal", "gibbs"), 2),
    MTIFullGammaECM = mfr_number(mean_at("gamma05", "ecm"), 2),
    MTIFullGammaGibbs = mfr_number(mean_at("gamma05", "gibbs"), 2),
    MTIFullTensileECM = mfr_number(app("ecm", "mean_width")),
    MTIFullTensileGibbs = mfr_number(app("gibbs", "mean_width")),
    MTIFullTensileECMAttainment = mfr_number(100*app("ecm", "attainment"), 1),
    MTIFullTensileGibbsAttainment = mfr_number(100*app("gibbs", "attainment"), 1),
    MTIFixedECMShorter = as.character(sum(wide$ecm < wide$young_mathew)),
    MTIFixedGibbsWider = as.character(sum(wide$gibbs > wide$ecm)),
    MTIFixedECMLower = as.character(sum(s[method == "ecm", simultaneous_lower] > .95)),
    MTIFixedECMInclude = as.character(sum(s[method == "ecm", simultaneous_lower] <= .95 &
      s[method == "ecm", simultaneous_upper] >= .95)),
    PharmaBatches = "187", PharmaRawBatches = "1005", PharmaRawFields = "55", PharmaProductCode = "23",
    PharmaTrain = "100", PharmaHeldout = "87", PharmaSplits = "1000", PharmaContent = "0.90",
    PharmaConfidence = "0.95", PharmaScreen = "0.995", PharmaRetained = "98", PharmaGroupedSplits = "250")
  for (response in c("fct_tensile", "tbl_rsd_weight")) {
    prefix <- if (response == "fct_tensile") "PharmaTensile" else "PharmaRsd"
    for (wanted_method in c("ecm", "young_mathew", "wilks")) {
      r <- a[response_id == response & split_scheme == "random" & method == wanted_method]
      mwd_require(nrow(r) == 1L, "A method-specific application summary is missing.")
      stem <- paste0(prefix, c(ecm = "Mti", young_mathew = "Ym", wilks = "Wilks")[wanted_method])
      quantities <- c(Width = "median_width", Content = "content_median",
        LowerOmitted = "lower_omitted_median", UpperOmitted = "upper_omitted_median")
      for (suffix in names(quantities)) {
        quantity <- quantities[suffix]
        values[paste0(stem, suffix)] <- mfr_number(r[[quantity]] * if (quantity == "median_width") 1 else 100,
          if (quantity == "median_width") 3 else 1)
      }
    }
    vv <- v[response_id == response & split_scheme == "random"]
    paired <- data.table::dcast(vv[method %in% c("ecm", "young_mathew")], split_id ~ method, value.var = "width")
    values[paste0(prefix, "PairedYmWidthDifference")] <- mfr_number(median(paired$ecm - paired$young_mathew))
    values[paste0(prefix, "Representative")] <- as.character(reps[response_id == response, split_id])
    for (scheme in c(Forward = "chronological_forward", Reverse = "chronological_reverse")) {
      suffix <- if (scheme == "chronological_forward") "ForwardContent" else "ReverseContent"
      values[paste0(prefix, suffix)] <- mfr_number(100*a[response_id == response & split_scheme == scheme & method == "ecm", content_median], 1)
    }
    observed <- mfr_read(cfg$evidence_dir, "observed_batches")[[response]]
    values[paste0(prefix, "Skewness")] <- mfr_number(mean((observed - mean(observed))^3) / sd(observed)^3, 2)
  }
  campaign <- mfr_read(cfg$evidence_dir, "campaign_cells")
  values["MTIFixedCampaignTensileContent"] <- mfr_number(100*campaign[response_id == "fct_tensile" & method == "ecm", content_median], 1)
  values["MTIFixedCampaignRsdContent"] <- mfr_number(100*campaign[response_id == "tbl_rsd_weight" & method == "ecm", content_median], 1)
  writeLines(c("% Generated from the fixed-prior study; no analysis-sample prior estimates.",
    sprintf("\\newcommand{\\%s}{%s}", names(values), values)), file.path(cfg$table_dir, "numbers.tex"))
}

mfr_extra_tables <- function(cfg) {
  z <- mfr_read(cfg$evidence_dir, "numerical_assessment")
  lines <- c("\\begin{tabular}{@{}lrrrr@{}}", "\\toprule",
    "Data & Fits & Maximum $\\widehat R$ & Minimum bulk/tail ESS & Maximum coordinate gain\\\\", "\\midrule")
  for (i in seq_len(nrow(z))) {
    r <- z[i]
    lines <- c(lines, sprintf("%s & %d & %.4f & %d / %d & $%.2f\\times10^{%d}$\\\\",
      if (r$stage == "simulation") "Simulation" else "Application", r$attempts, r$max_rhat,
      floor(r$min_bulk_ess), floor(r$min_tail_ess), r$max_coordinate_gap / 10^floor(log10(r$max_coordinate_gap)),
      floor(log10(r$max_coordinate_gap))))
  }
  writeLines(c(lines, "\\bottomrule", "\\end{tabular}"), file.path(cfg$table_dir, "numerical_assessment.tex"))
  z <- mfr_read(cfg$evidence_dir, "campaign_cells")
  z <- z[order(response_id, match(method, names(mfr_labels)))]
  lines <- c("\\begin{tabular}{@{}llrrr@{}}", "\\toprule",
    "Response & Method & Width range & Content (\\%) & Held-out $\\ge c$ (\\%)\\\\", "\\midrule")
  for (i in seq_len(nrow(z))) {
    r <- z[i]; first <- i == 1L || r$response_id != z$response_id[i-1L]
    lines <- c(lines, sprintf("%s & %s & %s & %.1f & %.1f\\\\",
      if (!first) "" else if (r$response_id == "fct_tensile") "Tensile metric" else "Weight RSD",
      mfr_labels[r$method], mfr_range(r$width_q025, r$width_q975), 100*r$content_median, 100*r$attainment))
    if (i %% 4 == 0) lines <- c(lines, "\\addlinespace[0.3em]")
  }
  writeLines(c(lines, "\\bottomrule", "\\end{tabular}"), file.path(cfg$table_dir, "campaign_sensitivity.tex"))
}
