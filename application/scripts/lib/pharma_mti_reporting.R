pma_read_report_inputs <- function(config, results_path) {
  if (!nzchar(results_path) || !file.exists(results_path)) {
    pta_stop("Expected canonical application results are absent: ", results_path)
  }
  directory <- dirname(results_path)
  if(is.null(config$mti_profile)) {
    pma_verify_hash(config$mti_ecm$policy_csv, config$mti_ecm$policy_sha256)
    pma_verify_hash(config$mti_ecm$validation_config, config$mti_ecm$validation_config_sha256)
  } else {
    pma_verify_hash(config$mti_profile$adoption_config,config$mti_profile$adoption_sha256)
    a <- pta_read_config(config$mti_profile$adoption_config)
    pma_verify_hash(a$policy_csv,a$policy_sha256)
  }
  manifest <- pma_verify_run(directory, config)
  if (manifest$mode != "confirmatory") pta_stop("Article inputs require a confirmatory run.")
  results <- pta_read_csv(results_path)
  representatives <- pma_representative(results)
  stored <- pta_read_csv(file.path(directory, "representative_splits.csv"))
  if (!isTRUE(all.equal(representatives, stored, check.attributes = FALSE))) {
    pta_stop("Representative-split record does not match the current results.")
  }
  list(config = config, results = results, manifest = manifest,
    observed = pta_read_csv(file.path(directory, "observed_batches.csv")),
    membership = pta_read_csv(file.path(directory, "split_membership.csv.gz")),
    representatives = representatives, results_path = results_path)
}

pma_metric_labels <- c(
  width_difference = "Width difference", width_ratio = "Width ratio",
  lower_difference = "Lower-endpoint difference",
  upper_difference = "Upper-endpoint difference",
  heldout_empirical_content_difference = "Held-out content difference",
  heldout_lower_omitted_difference = "Lower-omission difference",
  heldout_upper_omitted_difference = "Upper-omission difference")
pma_scheme_labels <- c(random = "Random splits", chronological_forward = "Forward chronology",
  chronological_reverse = "Reverse chronology", month_grouped = "Production-month groups",
  api_grouped = "API-batch groups")
pma_response_labels <- c(fct_tensile = "Tensile strength", tbl_rsd_weight = "Weight RSD")
pma_method_labels <- c(mti_ecm_adaptive_cell = "MTI-ECM", young_mathew = "Young--Mathew",
                       wilks_minmax = "Wilks")
pma_number <- function(x, digits = 3) {
  x[is.finite(x) & abs(x) < .5 * 10^(-digits)] <- 0
  pta_format_num(x, digits)
}
pma_bracket <- function(lo, hi, digits = 3) paste0("[", pma_number(lo, digits),
                                                   ", ", pma_number(hi, digits), "]")
pma_median_range <- function(m, lo, hi, digits = 3) {
  paste0(pma_number(m, digits), " ", pma_bracket(lo, hi, digits))
}
pma_write_tex <- function(path, lines) writeLines(lines, path, useBytes = TRUE)

pma_method_table <- function(rows, path) {
  rows <- rows[match(pma_methods, rows$method_id), ]
  if (anyNA(rows$method_id)) pta_stop("Incomplete method-summary table.")
  labels <- paste0(rows$method, ifelse(rows$method_role == "proposed",
                                      " (proposed)", " (benchmark)"))
  lines <- c("\\begin{tabularx}{\\textwidth}{@{}lrr>{\\raggedright\\arraybackslash}X>{\\raggedright\\arraybackslash}X@{}}",
    "\\toprule", "Method & Splits & Returned (\\%) & Median lower/upper & Width median [95\\% range]\\\\",
    "\\midrule")
  for (i in seq_len(nrow(rows))) {
    z <- rows[i, ]
    lines <- c(lines, sprintf("%s & %d & %s & %s / %s & %s \\\\",
      labels[i], z$splits, pta_format_pct(z$interval_return_rate),
      pma_number(z$lower_median), pma_number(z$upper_median),
      pma_median_range(z$width_median, z$width_q025, z$width_q975)))
  }
  lines <- c(lines, "\\bottomrule", "\\end{tabularx}", "\\par\\smallskip",
    "\\begin{tabularx}{\\textwidth}{@{}l>{\\raggedright\\arraybackslash}Xrr@{}}",
    "\\toprule", "Method & Held-out content median [95\\% range] (\\%) & Held-out $\\ge c$ (\\%) & Omitted below/above (\\%)\\\\",
    "\\midrule")
  for (i in seq_len(nrow(rows))) {
    z <- rows[i, ]
    lines <- c(lines, sprintf("%s & %s & %s & %s / %s \\\\",
      z$method, pma_median_range(100*z$heldout_content_median,
        100*z$heldout_content_q025, 100*z$heldout_content_q975, 1),
      pta_format_pct(z$heldout_attainment_rate),
      pta_format_pct(z$lower_omitted_median), pta_format_pct(z$upper_omitted_median)))
  }
  pma_write_tex(path, c(lines, "\\bottomrule", "\\end{tabularx}"))
}

pma_paired_table <- function(rows, path, label, response_label,proposed_label="MTI-ECM") {
  lines <- c("\\begingroup", "\\footnotesize", "\\setlength{\\tabcolsep}{3pt}",
    "\\begin{longtable}{@{}llrrrr@{}}",
    paste0("\\caption{\\textbf{Paired comparisons for ", response_label,
      ".} Differences are ",proposed_label," minus the benchmark; the width ratio divides the corresponding widths. Content and omission differences are percentage points. Entries describe paired random splits of one finite dataset. IQR is the interquartile range.}\\label{", label, "}\\\\"),
    "\\toprule", "Quantity & Benchmark & Pairs & Median & IQR & 95\\% range\\\\",
    "\\midrule", "\\endfirsthead", "\\toprule",
    "Quantity & Benchmark & Pairs & Median & IQR & 95\\% range\\\\", "\\midrule", "\\endhead")
  for (comparator in pma_methods[-1]) {
    z <- rows[rows$comparator_id == comparator, ]
    z <- z[match(names(pma_metric_labels), z$metric), ]
    for (i in seq_len(nrow(z))) {
      r <- z[i, ]
      multiplier <- if (grepl("content|omitted", r$metric)) 100 else 1
      lines <- c(lines, sprintf("%s & %s & %d & %s & %s & %s \\\\",
        pma_metric_labels[r$metric], r$comparator, r$finite_pairs,
        pma_number(multiplier*r$median), pma_number(multiplier*r$iqr),
        pma_bracket(multiplier*r$q025, multiplier*r$q975)))
    }
    lines <- c(lines, "\\addlinespace")
  }
  pma_write_tex(path, c(lines, "\\bottomrule", "\\end{longtable}", "\\endgroup"))
}

pma_sensitivity_table <- function(rows, path, label, response_label) {
  lines <- c("\\begingroup", "\\footnotesize", "\\setlength{\\tabcolsep}{3pt}",
    "\\begin{longtable}{@{}lrrrrr@{}}",
    paste0("\\caption{\\textbf{Partition sensitivities for ", response_label,
      ".} Each heading gives the number of splits and median training/held-out sizes. Width, content, and omissions are medians within the labeled design. Content, attainment, omissions, and return rates are percentages. The chronological cases each contain one partition.}\\label{", label, "}\\\\"),
    "\\toprule", "Method & Width & Held-out content & Held-out $\\ge c$ & Omitted below/above & Returned\\\\",
    "\\midrule", "\\endfirsthead", "\\toprule",
    "Method & Width & Held-out content & Held-out $\\ge c$ & Omitted below/above & Returned\\\\",
    "\\midrule", "\\endhead")
  for (scheme in pma_schemes) {
    z <- rows[rows$split_scheme == scheme, ]
    z <- z[match(pma_methods, z$method_id), ]
    lines <- c(lines, sprintf("\\multicolumn{6}{@{}l}{\\textit{%s: %d splits; training/held-out %d/%d}}\\\\",
      pma_scheme_labels[scheme], z$splits[1], z$train_n[1], z$heldout_n[1]))
    for (i in seq_len(nrow(z))) {
      r <- z[i, ]
      lines <- c(lines, sprintf("%s & %s & %s & %s & %s / %s & %s \\\\",
        r$method, pma_number(r$width_median), pta_format_pct(r$heldout_content_median),
        pta_format_pct(r$heldout_attainment_rate), pta_format_pct(r$lower_omitted_median),
        pta_format_pct(r$upper_omitted_median), pta_format_pct(r$interval_return_rate)))
    }
    lines <- c(lines, "\\addlinespace")
  }
  pma_write_tex(path, c(lines, "\\bottomrule", "\\end{longtable}", "\\endgroup"))
}

pma_numerical_table <- function(diagnostics, path) {
  labels <- c(mti_candidates_evaluated = "Candidates evaluated", mti_feasible_count = "Eligible candidates",
    mti_selected_q = "Selected fitted content", mti_selected_tilt = "Selected tilt",
    mti_posterior_content_probability = "Posterior content probability",
    mti_ecm_iterations = "ECM iterations", mti_ecm_final_stationarity = "Final stationarity")
  labels <- c(labels,mti_profile_iterations="Profile iterations",
    mti_relative_objective_gap="Relative objective gap")
  lines <- c("\\begin{tabularx}{\\textwidth}{@{}ll>{\\raggedright\\arraybackslash}X@{}}",
    "\\toprule", "Response & Diagnostic & Summary\\\\", "\\midrule")
  for (response in names(pma_response_labels)) {
    z <- diagnostics[diagnostics$response_id == response & diagnostics$split_scheme == "random", ]
    lines <- c(lines, sprintf("%s & Returned / all splits & %d / %d \\\\",
      pma_response_labels[response], z$returned[1], z$splits[1]),
      sprintf(" & Convergence flag / returned & %d / %d (%s\\%%) \\\\",
        z$convergence_count[1], z$returned[1], pta_format_pct(z$convergence_rate[1])))
    for (i in seq_len(nrow(z))) {
      r <- z[i, ]
      digits <- switch(r$metric, mti_candidates_evaluated = 0,
        mti_feasible_count = 1, mti_ecm_iterations = 0,
        mti_ecm_final_stationarity = 6, 4)
      value <- if(r$metric=="mti_relative_objective_gap")sprintf("%.2e [%.2e, %.2e]",
        r$median,r$q025,r$q975) else pma_median_range(r$median,r$q025,r$q975,digits)
      lines <- c(lines, sprintf(" & %s & %s \\\\", labels[r$metric],value))
    }
    lines <- c(lines, "\\addlinespace")
  }
  pma_write_tex(path, c(lines, "\\bottomrule", "\\end{tabularx}"))
}

pma_convergence_table <- function(rows, path) {
  lines <- c("\\begin{tabular}{@{}llrrr@{}}", "\\toprule",
    "Response & Method & Included splits & Width median & Held-out content (\\%)\\\\", "\\midrule")
  for (response in names(pma_response_labels)) {
    z <- rows[rows$response_id == response & rows$split_scheme == "random", ]
    z <- z[match(pma_methods, z$method_id), ]
    for (i in seq_len(nrow(z))) {
      r <- z[i, ]
      lines <- c(lines, sprintf("%s & %s & %d / %d & %s & %s \\\\",
        if (i == 1) pma_response_labels[response] else "", r$method,
        r$splits, r$original_splits, pma_number(r$width_median),
        pta_format_pct(r$heldout_content_median)))
    }
    lines <- c(lines, "\\addlinespace")
  }
  pma_write_tex(path, c(lines, "\\bottomrule", "\\end{tabular}"))
}

pma_numeric_macros <- function(inputs, summary, paired, diagnostics) {
  cfg <- inputs$config
  profiled <- !is.null(cfg$mti_profile)
  if(profiled) {
    a <- pta_read_config(cfg$mti_profile$adoption_config)
    policy <- read.csv(a$policy_csv)
    cutoff <- policy$threshold[policy$n==cfg$analysis$training_size &
      abs(policy$content-cfg$analysis$content)<1e-12]
    if(length(cutoff)!=1L)pta_stop("No unique application cutoff.")
  } else {
    validation <- pta_read_config(cfg$mti_ecm$validation_config)
    controls <- validation$engine_defaults$mti_ecm_dp_profile$ecm_control
    cutoff <- cfg$mti_ecm$required_screen
  }
  values <- c(PharmaBatches = as.character(nrow(inputs$observed)),
    PharmaRawBatches = as.character(cfg$data_source$expected_rows),
    PharmaRawFields = as.character(cfg$data_source$expected_columns),
    PharmaProductCode = as.character(cfg$analysis$product_code),
    PharmaTrain = as.character(cfg$analysis$training_size), PharmaHeldout = as.character(cfg$analysis$heldout_size),
    PharmaSplits = as.character(cfg$analysis$confirmatory_splits),
    PharmaContent = sprintf("%.2f", cfg$analysis$content),
    PharmaConfidence = sprintf("%.2f", cfg$analysis$tolerance_confidence),
    PharmaScreen = sprintf(if(profiled)"%.6f" else "%.3f",cutoff),
    PharmaRetained = as.character(cfg$scan_reference$expected_retained_count),
    PharmaGroupedSplits = as.character(cfg$sensitivity$requested_unique_splits))
  if(profiled) {
    values["PharmaObjectiveTolerance"] <- "10^{-8}"
    values["PharmaMaxObjectiveGap"] <- sprintf("%.2e",
      max(abs(inputs$results$mti_relative_objective_gap),na.rm=TRUE))
  } else {
    values["PharmaStationarityThreshold"] <- pma_number(controls$tol_stationarity, 3)
    values["PharmaMaxIter"] <- as.character(controls$max_iter)
  }
  for (response in names(pma_response_labels)) {
    prefix <- if(response == "fct_tensile") "PharmaTensile" else "PharmaRsd"
    z <- summary[summary$response_id == response & summary$split_scheme == "random", ]
    for (method in pma_methods) {
      r <- z[z$method_id == method, ]
      tag <- c(mti_ecm_adaptive_cell = "Mti", young_mathew = "Ym", wilks_minmax = "Wilks")[[method]]
      fields <- c(Width = pma_number(r$width_median), Lower = pma_number(r$lower_median),
        Upper = pma_number(r$upper_median), Content = pta_format_pct(r$heldout_content_median),
        Attainment = pta_format_pct(r$heldout_attainment_rate),
        LowerOmitted = pta_format_pct(r$lower_omitted_median),
        UpperOmitted = pta_format_pct(r$upper_omitted_median))
      names(fields) <- paste0(prefix, tag, names(fields))
      values <- c(values, fields)
    }
    d <- diagnostics[diagnostics$response_id == response & diagnostics$split_scheme == "random", ]
    values[paste0(prefix, "Converged")] <- as.character(d$convergence_count[1])
    values[paste0(prefix, "Returned")] <- as.character(d$returned[1])
    values[paste0(prefix, "ConvergenceRate")] <- pta_format_pct(d$convergence_rate[1])
    values[paste0(prefix, "Skewness")] <- pma_number(pta_skewness(inputs$observed[[response]]), 2)
    representative <- inputs$representatives[inputs$representatives$response_id == response, ]
    values[paste0(prefix, "Representative")] <- as.character(representative$split_id)
    p <- paired[paired$response_id == response & paired$split_scheme == "random" &
                  paired$comparator_id == "young_mathew" & paired$metric == "width_difference", ]
    values[paste0(prefix, "PairedYmWidthDifference")] <- pma_number(p$median)
    for (scheme in c("chronological_forward", "chronological_reverse")) {
      r <- summary[summary$response_id == response & summary$split_scheme == scheme &
                     summary$method_id == pma_methods[1], ]
      tag <- if(scheme == "chronological_forward") "ForwardContent" else "ReverseContent"
      values[paste0(prefix, tag)] <- pta_format_pct(r$heldout_content_median)
    }
  }
  values
}

pma_generate_tables <- function(inputs, output_dir) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  summary <- pma_summary(inputs$results)
  paired <- pma_paired_summary(inputs$results)
  diagnostics <- pma_diagnostics(inputs$results)
  convergence <- pma_convergence_sensitivity(inputs$results)
  timing_key <- interaction(inputs$results$response_id, inputs$results$split_scheme,
                            inputs$results$method_id, drop = TRUE)
  timing <- do.call(rbind, lapply(split(inputs$results, timing_key), function(z) {
    data.frame(response_id = z$response_id[1], split_scheme = z$split_scheme[1],
      method_id = z$method_id[1], splits = nrow(z),
      total_worker_sec = sum(z$elapsed_sec), mean_sec = mean(z$elapsed_sec),
      median_sec = median(z$elapsed_sec), q95_sec = pta_quantile(z$elapsed_sec, .95))
  }))
  pta_atomic_write_csv(timing, file.path(output_dir, "pharma_application_runtime_summary.csv"))
  pta_atomic_write_csv(summary, file.path(output_dir, "pharma_application_summary.csv"))
  pta_atomic_write_csv(paired, file.path(output_dir, "pharma_application_paired_summary.csv"))
  pta_atomic_write_csv(diagnostics, file.path(output_dir, "pharma_application_numerical_diagnostics.csv"))
  pta_atomic_write_csv(convergence, file.path(output_dir, "pharma_application_convergence_sensitivity.csv"))
  pta_atomic_write_csv(summary, file.path(output_dir, "pharma_application_dependence_sensitivity.csv"))
  primary <- summary[summary$response_role == "primary" & summary$split_scheme == "random", ]
  supplement <- summary[summary$response_role == "supplement" & summary$split_scheme == "random", ]
  pta_atomic_write_csv(primary, file.path(output_dir, "pharma_application_primary_summary.csv"))
  pta_atomic_write_csv(supplement, file.path(output_dir, "pharma_application_supplement_summary.csv"))
  pma_method_table(primary, file.path(output_dir, "pharma_application_primary_summary.tex"))
  pma_method_table(supplement, file.path(output_dir, "pharma_application_supplement_summary.tex"))
  for (response in names(pma_response_labels)) {
    suffix <- if (response == "fct_tensile") "primary" else "rsd"
    p <- paired[paired$response_id == response & paired$split_scheme == "random", ]
    pta_atomic_write_csv(p, file.path(output_dir, paste0("pharma_application_", suffix, "_paired_summary.csv")))
    pma_paired_table(p, file.path(output_dir, paste0("pharma_application_", suffix, "_paired_summary.tex")),
      paste0("tab:supp-pharma-", suffix, "-paired"), pma_response_labels[response],
      inputs$config$method_labels[[pma_methods[1]]])
    pma_sensitivity_table(summary[summary$response_id == response, ],
      file.path(output_dir, paste0("pharma_application_", suffix, "_sensitivity.tex")),
      paste0("tab:supp-pharma-", suffix, "-sensitivity"), pma_response_labels[response])
  }
  pma_numerical_table(diagnostics, file.path(output_dir, "pharma_application_numerical_diagnostics.tex"))
  pma_convergence_table(convergence, file.path(output_dir, "pharma_application_convergence_sensitivity.tex"))
  failures <- inputs$results[!inputs$results$interval_returned, ]
  failures <- if (nrow(failures)) aggregate(split_id ~ response_id + split_scheme + method_id + failure_reason,
    failures, length) else data.frame(response_id=character(), split_scheme=character(),
      method_id=character(), failure_reason=character(), split_id=integer())
  names(failures)[names(failures)=="split_id"] <- "count"
  pta_atomic_write_csv(failures, file.path(output_dir, "pharma_application_failure_categories.csv"))
  values <- pma_numeric_macros(inputs, summary, paired, diagnostics)
  pta_atomic_write_csv(data.frame(command = names(values), value = unname(values)),
                       file.path(output_dir, "pharma_application_numbers.csv"))
  pma_write_tex(file.path(output_dir, "pharma_application_numbers.tex"),
    sprintf("\\newcommand{\\%s}{%s}", names(values), values))
  invisible(summary)
}
