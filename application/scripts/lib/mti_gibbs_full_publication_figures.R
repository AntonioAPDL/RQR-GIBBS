mgfp_families <- c(normal = "Normal", student_t3 = "Student t (3 df)", exponential = "Exponential",
  asym_laplace_tau010 = "Asymmetric Laplace", two_piece_normal_1_12 = "Two-piece Normal",
  beta18 = "Beta(1,8)", gamma05 = "Gamma(0.5,1)", lognormal_hard = "Lognormal")
mgfp_methods <- c(mti_ecm_adaptive_cell = "MTI-ECM", mti_gibbs_mean = "MTI-Gibbs",
  young_mathew = "Young--Mathew", wilks = "Wilks")
mgfp_colors <- c(mti_ecm_adaptive_cell = "#0072B2", mti_gibbs_mean = "#CC79A7",
  young_mathew = "#D55E00", wilks = "black", wilks_minmax = "black")
mgfp_range <- function(lo, hi, digits = 3L) paste0(formatC(lo, digits = digits, format = "f"),
  "--", formatC(hi, digits = digits, format = "f"))
mgfp_count <- function(x) formatC(x, format = "d", big.mark = ",")

mgfp_numbers <- function(cfg, data) {
  s <- data$summary$completion; cells <- data$simulation_cells
  g <- cells[cells$method == "mti_gibbs_mean", ]; e <- cells[cells$method == "mti_ecm_adaptive_cell", ]
  median <- data$median_sensitivity
  geometry <- function(source, target) sum(data$geometry$datasets[
    data$geometry$source == source & data$geometry$target_class == target])
  width <- function(family, method) cells$width_mean[cells$response_id == family & cells$n == 1000L &
    cells$content == .9 & cells$method == method]
  app <- function(response, method, column) data$application_partitions[[column]][
    data$application_partitions$response_id == response & data$application_partitions$split_scheme == "random" &
      data$application_partitions$method == method]
  a <- data$agreement$standardized_difference
  values <- c(Datasets = mgfp_count(s$datasets), Simulations = mgfp_count(s$simulation_datasets),
    Applications = mgfp_count(s$application_datasets), Chains = mgfp_count(data$summary$chains),
    Rhat = sprintf("%.4f", s$maximum_rhat), BulkESS = mgfp_count(round(s$minimum_bulk_ess)),
    TailESS = mgfp_count(round(s$minimum_tail_ess)), Precision = mgfp_count(s$all_primary_precision_datasets),
    PrecisionOther = mgfp_count(s$datasets - s$all_primary_precision_datasets),
    MaximumMCSE = sprintf("%.1f", 100 * max(data$precision$maximum_mcse_sd_ratio)),
    AttainmentMin = sprintf("%.1f", 100 * min(g$attainment_rate)),
    AttainmentMax = sprintf("%.1f", 100 * max(g$attainment_rate)),
    SimultaneousLower = sprintf("%.4f", min(g$simultaneous_lower)),
    WiderECM = as.character(sum(data$paired_cells$mean_width_difference[
      data$paired_cells$comparator == "mti_ecm_adaptive_cell"] > 0)),
    ShorterWilks = as.character(sum(data$paired_cells$mean_width_difference[data$paired_cells$comparator == "wilks"] < 0)),
    MedianAttainmentMin = sprintf("%.1f", 100 * min(median$median_endpoint_attainment)),
    MedianAttainmentMax = sprintf("%.1f", 100 * max(median$median_endpoint_attainment)),
    Interior = mgfp_count(geometry("simulation", "interior")),
    Limiting = mgfp_count(geometry("simulation", "boundary")), Outside = mgfp_count(geometry("simulation", "outside")),
    NormalECM = sprintf("%.2f", width("normal", "mti_ecm_adaptive_cell")),
    NormalGibbs = sprintf("%.2f", width("normal", "mti_gibbs_mean")),
    GammaECM = sprintf("%.2f", width("gamma05", "mti_ecm_adaptive_cell")),
    GammaGibbs = sprintf("%.2f", width("gamma05", "mti_gibbs_mean")),
    TensileECM = sprintf("%.3f", app("fct_tensile", "mti_ecm_adaptive_cell", "width_mean")),
    TensileGibbs = sprintf("%.3f", app("fct_tensile", "mti_gibbs_mean", "width_mean")),
    TensileECMAttainment = sprintf("%.1f", 100 * app("fct_tensile", "mti_ecm_adaptive_cell", "attainment_rate")),
    TensileGibbsAttainment = sprintf("%.1f", 100 * app("fct_tensile", "mti_gibbs_mean", "attainment_rate")),
    AgreementCount = mgfp_count(length(a)), AgreementTwo = mgfp_count(sum(abs(a) <= 2)),
    AgreementThree = mgfp_count(sum(abs(a) <= 3)), AgreementMaximum = sprintf("%.2f", max(abs(a))))
  path <- "tables/mti_gibbs_full_numbers.tex"
  writeLines(c("% Generated from the completed fixed-target Gibbs comparison.",
    sprintf("\\newcommand{\\MTIFull%s}{%s}", names(values), values)), path)
  path
}

mgfp_simulation_table <- function(cells, path) {
  lines <- c("\\begingroup", "\\footnotesize", "\\setlength{\\tabcolsep}{4pt}",
    "\\renewcommand{\\arraystretch}{1.06}")
  for (family in names(mgfp_families)) {
    d <- cells[cells$response_id == family, ]
    d <- d[order(d$n, d$content, match(d$method, names(mgfp_methods))), ]
    mgfa_require(nrow(d) > 0 && all(d$datasets == 1000L), "Incomplete simulation table.")
    label <- if (family == names(mgfp_families)[1]) "tab:supp-mti-full-simulation" else paste0("tab:supp-mti-full-", family)
    lines <- c(lines, "\\clearpage", "\\begin{longtable}{@{}rrlrr@{}}",
      sprintf("\\caption{\\textbf{%s: paired interval comparison.} Each design uses 1000 independent samples at tolerance confidence 0.95. Attainment is the percentage with true population content at least \\(c\\). Width ranges are empirical 2.5\\%%--97.5\\%% quantiles across those samples. MTI-Gibbs uses posterior-mean endpoints at the ECM-selected fitted content and tilt.}\\label{%s}\\\\", mgfp_families[family], label),
      "\\toprule", "\\(n\\) & \\(c\\) & Method & Attainment (\\%) & Width range\\\\", "\\midrule", "\\endfirsthead",
      sprintf("\\caption[]{%s: paired interval comparison (continued).}\\\\", mgfp_families[family]),
      "\\toprule", "\\(n\\) & \\(c\\) & Method & Attainment (\\%) & Width range\\\\", "\\midrule", "\\endhead",
      "\\bottomrule", "\\endfoot")
    key <- paste(d$n, d$content)
    for (id in unique(key)) {
      z <- d[key == id, ]; mgfa_require(nrow(z) == 4L, "Missing comparison method in a design.")
      for (i in seq_len(nrow(z))) lines <- c(lines, sprintf("%s & %s & %s & %.1f & %s \\\\",
        if (i == 1L) as.character(z$n[i]) else "", if (i == 1L) sprintf("%.2f", z$content[i]) else "",
        mgfp_methods[z$method[i]], 100 * z$attainment_rate[i], mgfp_range(z$width_q025[i], z$width_q975[i])))
      lines <- c(lines, "\\addlinespace[0.3em]")
    }
    lines <- c(lines, "\\end{longtable}")
  }
  writeLines(c(lines, "\\endgroup"), path)
  path
}

mgfp_application_table <- function(cells, path) {
  methods <- c(mgfp_methods[names(mgfp_methods) != "wilks"], wilks_minmax = "Wilks")
  responses <- c(fct_tensile = "Reported tensile metric", tbl_rsd_weight = "Tablet-core weight RSD")
  schemes <- c(random = "Random partitions", chronological_forward = "Forward recorded order",
    chronological_reverse = "Reverse recorded order", api_grouped = "API-batch groups", month_grouped = "Production months")
  lines <- c("\\begingroup", "\\footnotesize", "\\setlength{\\tabcolsep}{5pt}")
  for (response in names(responses)) {
    label <- if (response == names(responses)[1]) "tab:supp-mti-full-application" else "tab:supp-mti-full-application-rsd"
    lines <- c(lines, "\\clearpage", "\\begin{longtable}{@{}lrr@{}}",
      sprintf("\\caption{\\textbf{%s: same-partition comparisons.} Each fit uses 100 training batches at \\(c=0.90\\). Held-out attainment is the percentage of partitions whose empirical held-out content is at least \\(c\\). Ranges are empirical 2.5\\%%--97.5\\%% width quantiles within a partition scheme; single chronological partitions show their actual widths. Overlapping partitions are summarized descriptively.}\\label{%s}\\\\", responses[response], label),
      "\\toprule", "Method & Held-out attainment (\\%) & Width range\\\\", "\\midrule", "\\endfirsthead",
      "\\toprule", "Method & Held-out attainment (\\%) & Width range\\\\", "\\midrule", "\\endhead", "\\bottomrule", "\\endfoot")
    for (scheme in names(schemes)) {
      z <- cells[cells$response_id == response & cells$split_scheme == scheme, ]
      z <- z[match(names(methods), z$method), ]
      mgfa_require(nrow(z) == 4L && !anyNA(z$method), "Missing application comparison.")
      lines <- c(lines, sprintf("\\multicolumn{3}{@{}l}{\\textit{%s (%d partition%s)}}\\\\",
        schemes[scheme], z$datasets[1], if (z$datasets[1] == 1) "" else "s"))
      for (i in seq_len(nrow(z))) lines <- c(lines, sprintf("%s & %.1f & %s \\\\",
        methods[z$method[i]], 100 * z$attainment_rate[i], if (z$datasets[i] == 1L)
          sprintf("%.3f", z$width_mean[i]) else mgfp_range(z$width_q025[i], z$width_q975[i])))
      lines <- c(lines, "\\addlinespace[0.4em]")
    }
    lines <- c(lines, "\\end{longtable}")
  }
  writeLines(c(lines, "\\endgroup"), path)
  path
}

mgfp_legend <- function(labels, colors, lty = rep(1, length(labels))) {
  par(xpd = NA)
  legend(grconvertX(.5, "ndc", "user"), grconvertY(.035, "ndc", "user"),
    legend = labels, col = colors, lty = lty, lwd = 1.7, xjust = .5, yjust = .5,
    horiz = TRUE, bty = "n", cex = .78, seg.len = 1.5)
}
mgfp_layout <- function(cells) {
  design <- unique(cells[, c("n", "content")]); design <- design[order(design$n, design$content), ]
  design$center <- rev(seq_len(nrow(design))) * 3.3
  centers <- design$center[match(paste(cells$n, cells$content), paste(design$n, design$content))]
  list(design = design, y = centers + c(.9, .3, -.3, -.9)[match(cells$method, names(mgfp_methods))])
}

mgfp_width_figures <- function(cells) {
  paths <- character()
  for (part in 1:4) {
    path <- sprintf("figures/generated/figS_mti_full_width_ranges_%02d.pdf", part)
    grDevices::pdf(path, width = 7.4, height = 7.2, family = "Helvetica", useDingbats = FALSE)
    par(mfrow = c(1, 2), mar = c(4.2, 4.6, 2.8, .7), oma = c(2.5, 0, .2, 0), cex = .84)
    for (family in names(mgfp_families)[(2L * part - 1L):(2L * part)]) {
      z <- cells[cells$response_id == family, ]; a <- mgfp_layout(z)
      plot(NA, xlim = range(c(z$width_q025, z$width_q975)), ylim = c(1, max(a$design$center) + 2),
        log = "x", axes = FALSE, xlab = "Interval width (log scale)", ylab = "", main = mgfp_families[family], cex.main = 1)
      axis(1, cex.axis = .85)
      axis(2, at = a$design$center, labels = sprintf("n = %d\nc = %.2f", a$design$n, a$design$content),
        las = 1, tick = FALSE, cex.axis = .78)
      abline(h = a$design$center, col = "#EEEEEE")
      for (i in seq_len(nrow(z))) {
        color <- mgfp_colors[z$method[i]]
        segments(z$width_q025[i], a$y[i], z$width_q975[i], a$y[i], col = color, lwd = 1.5)
        points(z$width_mean[i], a$y[i], col = color, pch = 4, cex = .65, lwd = 1)
      }
      box()
    }
    mgfp_legend(gsub("--", "-", mgfp_methods), mgfp_colors[names(mgfp_methods)])
    grDevices::dev.off(); paths <- c(paths, path)
  }
  paths
}

mgfp_attainment_figure <- function(cells) {
  groups <- split(cells, paste(cells$n, cells$content, cells$method))
  z <- do.call(rbind, lapply(groups, function(x) data.frame(n = x$n[1], content = x$content[1],
    method = x$method[1], lower = min(x$attainment_rate), upper = max(x$attainment_rate))))
  a <- mgfp_layout(z); path <- "figures/generated/figS_mti_full_attainment.pdf"
  grDevices::pdf(path, width = 6.6, height = 7.2, family = "Helvetica", useDingbats = FALSE)
  par(mar = c(4.3, 5, 2, 1), oma = c(2.5, 0, 0, 0), cex = .87)
  plot(NA, xlim = c(.85, 1), ylim = c(1, max(a$design$center) + 2), axes = FALSE,
    xlab = "Content-attainment proportion", ylab = "")
  axis(1, at = seq(.85, 1, .025)); axis(2, at = a$design$center,
    labels = sprintf("n = %d\nc = %.2f", a$design$n, a$design$content), las = 1, tick = FALSE, cex.axis = .85)
  abline(h = a$design$center, col = "#EEEEEE"); abline(v = .95, col = "#777777", lty = 2)
  for (i in seq_len(nrow(z))) {
    segments(z$lower[i], a$y[i], z$upper[i], a$y[i], col = mgfp_colors[z$method[i]], lwd = 2)
    points(c(z$lower[i], z$upper[i]), rep(a$y[i], 2), col = mgfp_colors[z$method[i]], pch = 16, cex = .5)
  }
  box(); mgfp_legend(gsub("--", "-", mgfp_methods), mgfp_colors[names(mgfp_methods)])
  grDevices::dev.off(); path
}

mgfp_cdf_figure <- function(data) {
  path <- "figures/generated/figS_mti_full_fixed_target_width.pdf"
  grDevices::pdf(path, width = 7.4, height = 6.4, family = "Helvetica", useDingbats = FALSE)
  par(mfrow = c(2, 2), mar = c(3.8, 3.7, 3.3, .8), oma = c(2.4, 0, .2, 0),
    mgp = c(2.1, .65, 0), cex = .84)
  for (i in seq_len(nrow(data$examples))) {
    e <- data$examples[i, ]; z <- data$cdf[data$cdf$case_id == e$case_id, ]
    title <- if (e$source == "application") "Reported tensile metric" else mgfp_families[e$response_id]
    plot(z$width, z$probability, type = "s", log = "x", ylim = c(0, 1),
      xlim = range(c(z$width, e$ecm_width, e$minimum_width)), col = "#CC79A7", lwd = 1.5,
      xlab = "Interval width", ylab = "Posterior probability", main = title, cex.main = 1)
    mtext(sprintf("n = %d, c = %.2f, q = %.5g", e$n, e$content, e$q), side = 3, line = .35, cex = .75)
    abline(v = e$ecm_width, col = "#0072B2", lty = 2, lwd = 1.4)
    abline(v = e$minimum_width, col = "black", lty = 3, lwd = 1.4)
  }
  mgfp_legend(c("Posterior width", "Finite ECM estimate", "Width at loss minimum"),
    c("#CC79A7", "#0072B2", "black"), c(1, 2, 3))
  grDevices::dev.off(); path
}

mgfp_diagnostic_figures <- function(data) {
  colors <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7")
  labels <- c(lower = "Lower endpoint", upper = "Upper endpoint", width = "Width", midpoint = "Midpoint")
  paths <- c("figures/generated/figS_mti_full_trace.pdf", "figures/generated/figS_mti_full_ranks.pdf")
  for (type in c("trace", "ranks")) {
    grDevices::pdf(paths[if (type == "trace") 1 else 2], width = 7.4, height = 5.8,
      family = "Helvetica", useDingbats = FALSE)
    par(mfrow = c(2, 2), mar = c(3.7, 3.8, 2.6, .8), oma = c(2, 0, .4, 0),
      mgp = c(2.1, .65, 0), cex = .82)
    for (v in names(labels)) {
      if (type == "trace") {
        z <- data$trace_example
        plot(NA, xlim = range(z$iteration), ylim = range(z[[v]]), xlab = "Retained iteration",
          ylab = labels[v], main = labels[v], cex.main = 1)
        for (chain in 1:4) lines(z$iteration[z$chain == chain], z[[v]][z$chain == chain],
          col = grDevices::adjustcolor(colors[chain], .65), lwd = .5)
      } else {
        z <- data$rank_example[data$rank_example$variable == v, ]
        plot(NA, xlim = range(z$bin), ylim = range(c(0, z$count)), xlab = "Pooled-rank bin",
          ylab = "Draw count", main = labels[v], cex.main = 1)
        abline(h = sum(z$count) / length(unique(z$chain)) / length(unique(z$bin)), col = "#777777", lty = 2)
        for (chain in 1:4) lines(z$bin[z$chain == chain], z$count[z$chain == chain], col = colors[chain], lwd = 1.1)
      }
    }
    mgfp_legend(paste("Chain", 1:4), colors)
    grDevices::dev.off()
  }
  paths
}

mgfp_render <- function(cfg) {
  mgfp_verify(cfg, rendered = FALSE)
  read <- function(name) as.data.frame(data.table::fread(file.path(cfg$evidence_dir, paste0(name, ".csv")),
    nThread = 1L, na.strings = "NA"))
  names <- c("simulation_cells", "application_partitions", "paired_cells", "geometry", "geometry_cells",
    "median_sensitivity", "precision", "agreement", "cdf", "examples", "trace_example", "rank_example")
  data <- setNames(lapply(names, read), names)
  data$summary <- mgfa_json(file.path(cfg$evidence_dir, "summary.json"))
  paths <- c(mgfp_numbers(cfg, data),
    mgfp_simulation_table(data$simulation_cells, "tables/mti_gibbs_full_simulation_comparison.tex"),
    mgfp_application_table(data$application_partitions, "tables/mti_gibbs_full_application_comparison.tex"),
    mgfp_width_figures(data$simulation_cells), mgfp_attainment_figure(data$simulation_cells),
    mgfp_cdf_figure(data), mgfp_diagnostic_figures(data))
  mgfp_json_write(list(data_provenance_sha256 = mgfa_hash(file.path(cfg$evidence_dir, "provenance.json")),
    outputs = mgfp_hashes(paths), rendered_utc = mgfp_time(), session = capture.output(sessionInfo())),
    file.path(cfg$evidence_dir, "render_provenance.json"))
  invisible(paths)
}
