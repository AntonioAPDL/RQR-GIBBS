mfr_application_figure <- function(cfg, response, path) {
  values <- mfr_read(cfg$evidence_dir, "application_values")
  d <- values[response_id == response & split_scheme == "random" & method != "gibbs"]
  reps <- mfr_read(cfg$evidence_dir, "representative_splits")
  fitted <- d[split_id == reps[response_id == response, split_id]]
  methods <- c("ecm", "young_mathew", "wilks")
  fitted <- fitted[match(methods, method)]
  y <- mfr_read(cfg$evidence_dir, "observed_batches")[[response]]
  labels <- gsub("--", "-", mfr_labels[methods])
  png(path, width = 3000, height = 2400, res = 240, pointsize = 18)
  on.exit(dev.off())
  par(mar = c(5.0, 6.2, 3.0, 1), mgp = c(3.3, .9, 0), cex = .88)
  layout(matrix(1:4, 2, byrow = TRUE))
  plot(ecdf(y), xlim = range(c(y, fitted$lower, fitted$upper)), ylim = c(-.6, 1.05),
    axes = FALSE, verticals = TRUE, do.points = FALSE, xlab = "Observed response", ylab = "",
    main = "A. Observed distribution\nand representative intervals", col = "gray35")
  axis(1); axis(2, at = c(0, .5, 1), las = 1)
  mtext("Empirical CDF", 2, line = 4.3, at = .6)
  yy <- c(-.17, -.34, -.51)
  axis(2, at = yy, labels = labels, las = 1, tick = FALSE, cex.axis = .83)
  for (i in 1:3) {
    segments(fitted$lower[i], yy[i], fitted$upper[i], yy[i], col = mfr_colors[methods[i]], lwd = 3)
    points(c(fitted$lower[i], fitted$upper[i]), rep(yy[i], 2), pch = 16, cex = .6,
      col = mfr_colors[methods[i]])
  }
  box(bty = "l")
  for (i in 2:3) {
    pair <- merge(d[method == "ecm"], d[method == methods[i]], by = "split_id", suffixes = c("_e", "_b"))
    x <- pair$width_e - pair$width_b
    y <- 100 * (pair$heldout_content_e - pair$heldout_content_b)
    plot(x, y, pch = 16, cex = .55, col = adjustcolor(mfr_colors[methods[i]], .16),
      xlim = range(c(x, 0)), ylim = range(c(y, 0)), bty = "l",
      xlab = "Width difference\n(MTI-ECM minus benchmark)",
      ylab = "Held-out content difference\n(percentage points)",
      main = paste0(LETTERS[i], ". Paired comparison\nwith ", labels[i]))
    abline(h = 0, v = 0, col = "gray55", lty = 2)
    points(median(x), median(y), pch = 4, col = mfr_colors["ecm"], cex = 1.5, lwd = 2)
    legend("topleft", "Paired median", pch = 4, col = mfr_colors["ecm"], bty = "n", cex = .8)
  }
  upper <- 100 * max(vapply(methods, function(wanted_method) max(quantile(d[method == wanted_method, lower_omitted], .975),
    quantile(d[method == wanted_method, upper_omitted], .975)), 0), .05)
  plot(NA, xlim = c(0, upper * 1.1), ylim = c(.7, 7.7), axes = FALSE, xlab = "Held-out omission (%)",
    ylab = "", main = "D. Lower- and upper-tail\nomission")
  axis(1); axis(2, at = c(6.8, 4.3, 1.8), labels = labels, las = 1, tick = FALSE)
  yy <- c(7.1, 6.5, 4.6, 4, 2.1, 1.5)
  for (i in 1:3) for (j in 1:2) {
    q <- quantile(100 * d[method == methods[i]][[c("lower_omitted", "upper_omitted")[j]]], c(.025, .25, .5, .75, .975))
    at <- yy[(i-1)*2+j]; color <- mfr_colors[methods[i]]
    segments(q[1], at, q[5], at, col = color, lwd = 1.5)
    segments(q[2], at, q[4], at, col = color, lwd = 5)
    points(q[3], at, col = color, pch = if (j == 1) 1 else 16)
  }
  legend("bottomright", c("Lower tail", "Upper tail"), pch = c(1, 16), bty = "n", cex = .8)
  box(bty = "l")
}

mfr_posterior_figure <- function(cfg) {
  examples <- mfr_read(cfg$evidence_dir, "examples"); cdf <- mfr_read(cfg$evidence_dir, "cdf")
  path <- file.path(cfg$figure_dir, "fixed_target_width.pdf")
  pdf(path, width = 7.4, height = 6.4, useDingbats = FALSE)
  par(mfrow = c(2, 2), mar = c(3.8, 3.7, 3.3, .8), oma = c(2.5, 0, 0, 0), mgp = c(2.1, .65, 0), cex = .84)
  for (i in seq_len(nrow(examples))) {
    e <- examples[i]; z <- cdf[case_id == e$case_id]
    label <- if (e$response_id == "fct_tensile") "Reported tensile metric" else mgfp_families[e$response_id]
    plot(z$width, z$probability, type = "s", xlim = range(c(z$width, e$ecm_width)), ylim = c(0, 1),
      log = "x", col = mfr_colors["gibbs"], lwd = 1.5, xlab = "Interval width", ylab = "Posterior probability",
      main = label, cex.main = 1)
    mtext(sprintf("n = %d, c = %.2f, q = %.5g", e$n, e$content, e$q), 3, line = .35, cex = .75)
    abline(v = e$ecm_width, col = mfr_colors["ecm"], lty = 2, lwd = 1.5)
  }
  mgfp_legend(c("Posterior width", "ECM width"), mfr_colors[c("gibbs", "ecm")], c(1, 2))
  dev.off(); path
}

mfr_sensitivity_figure <- function(cfg) {
  d <- mfr_read(cfg$evidence_dir, "application_cells")[method != "gibbs"]
  methods <- c("ecm", "young_mathew", "wilks")
  schemes <- c("random", "chronological_forward", "chronological_reverse", "month_grouped", "api_grouped")
  labels <- c("Random splits", "Forward order", "Reverse order", "Month groups", "API groups")
  path <- file.path(cfg$figure_dir, "pharma_partition_sensitivity.png")
  png(path, width = 3000, height = 2300, res = 240, pointsize = 18)
  on.exit(dev.off())
  par(mfrow = c(2, 2), mar = c(4.5, 7, 2.8, 1), oma = c(2.5, 0, 0, 0), mgp = c(2.8, .8, 0), cex = .9)
  for (response in c("fct_tensile", "tbl_rsd_weight")) for (quantity in c("width", "content")) {
    z <- d[response_id == response]
    lo <- z[[paste0(quantity, "_q025")]]; hi <- z[[paste0(quantity, "_q975")]]
    midpoint <- z[[if (quantity == "width") "median_width" else "content_median"]]
    title <- if (response == "fct_tensile") "Reported tensile metric" else "Weight RSD"
    plot(NA, xlim = if (quantity == "width") range(c(lo, hi)) else c(0, 1), ylim = c(.5, 5.5),
      axes = FALSE, xlab = if (quantity == "width") "Interval width" else "Held-out empirical content",
      ylab = "", main = paste(title, "-", quantity))
    axis(1); axis(2, at = 5:1, labels = labels, las = 1, tick = FALSE, cex.axis = .82)
    if (quantity == "content") abline(v = .9, lty = 2, col = "gray50")
    yy <- 6 - match(z$split_scheme, schemes) + c(.2, 0, -.2)[match(z$method, methods)]
    for (i in seq_len(nrow(z))) {
      segments(lo[i], yy[i], hi[i], yy[i], col = mfr_colors[z$method[i]], lwd = 2)
      points(midpoint[i], yy[i], col = mfr_colors[z$method[i]], pch = 16, cex = .7)
    }
    box(bty = "l")
  }
  mgfp_legend(gsub("--", "-", mfr_labels[methods]), mfr_colors[methods])
}

mfr_render <- function(cfg) {
  mfr_verify(cfg)
  dir.create(cfg$table_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(cfg$figure_dir, recursive = TRUE, showWarnings = FALSE)
  cells <- mfr_read(cfg$evidence_dir, "simulation_cells")
  app <- mfr_read(cfg$evidence_dir, "application_cells")
  mfr_primary_table(mfr_read(cfg$evidence_dir, "primary_ranges"), file.path(cfg$table_dir, "primary_ranges.tex"))
  for (response in c("fct_tensile", "tbl_rsd_weight"))
    mfr_application_table(app, response, file.path(cfg$table_dir, paste0(response, ".tex")))
  mfr_numbers(cfg)
  mfr_extra_tables(cfg)
  cells[, `:=`(method = c(ecm = "mti_ecm_adaptive_cell", gibbs = "mti_gibbs_mean",
    young_mathew = "young_mathew", wilks = "wilks")[method], datasets = attempts,
    attainment_rate = attainment, width_mean = mean_width)]
  app[, `:=`(method = unname(mff_ids[method]), datasets = attempts, attainment_rate = attainment, width_mean = mean_width)]
  mgfp_simulation_table(as.data.frame(cells), file.path(cfg$table_dir, "simulation_comparison.tex"))
  mgfp_application_table(as.data.frame(app), file.path(cfg$table_dir, "application_comparison.tex"))
  # Existing plotting routines retain the established grouping and typography.
  for (entry in list(c("figures/generate_tolerance_validation_primary_figure.R", "--scenario-range-csv=primary_ranges.csv"),
    c("figures/generate_tolerance_validation_width_figure.R", "--width-csv=primary_widths.csv"))) {
    flag <- sub("=.*", "=", entry[2]); name <- sub("^[^=]+=", "", entry[2])
    arguments <- c(entry[1], paste0(flag, file.path(cfg$evidence_dir, name)), paste0("--output-dir=", cfg$figure_dir),
      "--mti-method-id=mti_ecm_adaptive_cell", "--mti-label=MTI-ECM")
    if (grepl("primary_figure", entry[1])) arguments <- c(arguments, "--delivery-y-min=0.85")
    mwd_require(system2("Rscript", shQuote(arguments)) == 0, "A primary comparison figure failed.")
  }
  mfr_application_figure(cfg, "fct_tensile", file.path(cfg$figure_dir, "pharma_tensile.png"))
  mfr_application_figure(cfg, "tbl_rsd_weight", file.path(cfg$figure_dir, "pharma_rsd.png"))
  mfr_sensitivity_figure(cfg)
  mfr_posterior_figure(cfg)
  comparison <- new.env(parent = environment())
  sys.source("application/scripts/lib/mti_gibbs_full_publication_figures.R", envir = comparison)
  comparison$mgfp_colors <- setNames(unname(mfr_colors),
    c("mti_ecm_adaptive_cell", "mti_gibbs_mean", "young_mathew", "wilks"))
  temporary <- tempfile(); dir.create(file.path(temporary, "figures/generated"), recursive = TRUE)
  previous <- getwd(); on.exit({setwd(previous); unlink(temporary, recursive = TRUE)}, add = TRUE)
  setwd(temporary)
  generated <- c(comparison$mgfp_width_figures(as.data.frame(cells)), comparison$mgfp_attainment_figure(as.data.frame(cells)),
    mgfp_diagnostic_figures(list(trace_example = as.data.frame(mfr_read(file.path(previous, cfg$evidence_dir), "trace")),
      rank_example = as.data.frame(mfr_read(file.path(previous, cfg$evidence_dir), "ranks")))))
  for (file in generated) mwd_require(file.copy(file, file.path(previous, cfg$figure_dir, basename(file)), overwrite = TRUE),
    "Cannot copy a posterior comparison figure.")
  setwd(previous)
  files <- c(list.files(cfg$table_dir, full.names = TRUE), list.files(cfg$figure_dir, full.names = TRUE))
  programs <- c("application/scripts/125_report_mti_fixed_prior.R", "application/scripts/lib/mti_fixed_prior_reporting.R",
    "application/config/mti_fixed_prior_reporting_20261115.json", "application/config/mti_fixed_prior_20261114.json",
    "application/scripts/lib/mti_fixed_prior_reporting_figures.R", "application/scripts/lib/mti_fixed_prior_publication.R",
    "application/scripts/lib/mti_gibbs_full_publication_figures.R", "application/scripts/lib/pharma_mti_application.R",
    "figures/generate_tolerance_validation_primary_figure.R", "figures/generate_tolerance_validation_width_figure.R",
    "figures/data/tolerance_population_shortest_content_oracle.csv")
  mgfp_json_write(list(outputs = mgfp_hashes(files), programs = mgfp_hashes(programs)), file.path(cfg$evidence_dir, "rendered.json"))
  mfr_verify(cfg, rendered = TRUE)
  cat("Rendered fixed-prior figures and tables in", cfg$table_dir, "and", cfg$figure_dir, "\n")
}
