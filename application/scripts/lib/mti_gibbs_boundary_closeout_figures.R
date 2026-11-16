mbx_verify <- function(cfg, rendered = TRUE) {
  p <- mwd_json(file.path(cfg$evidence_root, "provenance.json"))
  archived <- basename(names(p$inputs))
  archived[archived == "manifest.json"] <- "completed_manifest.json"
  mwd_require(!anyDuplicated(archived), "Completed-study input names are ambiguous.")
  mgfp_check_hashes(p$inputs, file.path(cfg$evidence_root, archived))
  mgfp_check_hashes(p$artifacts)
  mgfp_check_hashes(p$source$programs)
  old <- mwd_json(file.path(cfg$evidence_root, "completed_manifest.json"))$identity$programs
  configurations <- grepl("^application/config/.*[.]json$", names(old))
  mgfp_check_hashes(old[configurations])
  s <- mwd_json(file.path(cfg$evidence_root, "summary.json"))
  mwd_require(s$replay_datasets == 112 && s$confirmation_datasets == 2000 &&
    s$replay_maximum_selected_difference <= cfg$replay$endpoint_tolerance &&
    s$prior_sd == 2 && !s$prior_uses_sample, "Closeout evidence differs from its specification.")
  if (rendered) {
    r <- mwd_json(file.path(cfg$evidence_root, "rendered.json"))
    mgfp_check_hashes(r$inputs); mgfp_check_hashes(r$programs); mgfp_check_hashes(r$artifacts)
  }
  invisible(s)
}

mbx_render <- function(cfg) {
  s <- mbx_verify(cfg, rendered = FALSE)
  dir.create(cfg$figure_root, recursive = TRUE, showWarnings = FALSE)
  table_root <- "tables/mti_gibbs_boundary_20261116"
  dir.create(table_root, recursive = TRUE, showWarnings = FALSE)
  d <- data.table::fread(file.path(cfg$evidence_root, "comparison_summary.csv"))
  labels <- c(gibbs_calibrated = "Gibbs: calibrated interval", gibbs_unscaled = "Gibbs: posterior means",
    ecm = "MTI-ECM", young_mathew = "Young-Mathew", wilks = "Wilks")
  colors <- setNames(c("#0072B2", "#56B4E9", "#009E73", "#CC79A7", "black"), names(labels))
  laws <- c(normal = "Gaussian", student_t3 = "Student t (3 df)", exponential = "Exponential",
    asym_laplace_tau010 = "Asymmetric Laplace", two_piece_normal_1_12 = "Two-piece normal",
    beta18 = "Beta (1, 8)", gamma05 = "Gamma (shape 0.5)", lognormal_hard = "Log-normal (log SD 1.25)")
  path <- file.path(cfg$figure_root, "gibbs_boundary_pilot_review.pdf")
  grDevices::pdf(path, width = 11, height = 8)
  designs <- unique(d[, .(n, content, tolerance_confidence)])
  for (i in seq_len(nrow(designs))) for (kind in c("attainment", "width")) {
    z <- d[n == designs$n[i] & content == designs$content[i]]
    y <- match(z$response_id, names(laws))*7 + match(z$method, names(labels))-3
    lo <- if (kind == "width") z$width_q025 else z$lower_95
    hi <- if (kind == "width") z$width_q975 else z$upper_95
    layout(matrix(1:2, 2, 1), heights = c(1, 7))
    par(mar = c(0, 0, 0, 0)); plot.new()
    legend("center", labels, col = colors, lty = 1, lwd = 2, ncol = 3, bty = "n", cex = .9)
    par(mar = c(5, 12, 3, 2), mgp = c(2.5, .7, 0), cex.axis = .9, cex.lab = 1)
    plot(NA, xlim = if (kind == "width") range(c(lo, hi)) else c(min(.8, lo), 1),
      ylim = c(3, length(laws)*7+3), axes = FALSE, ylab = "",
      xlab = if (kind == "width") "Interval width: empirical 2.5%--97.5% range" else
        "Content attainment: exact pointwise 95% confidence interval",
      main = sprintf("Independent pilot: n = %d, c = %.2f, confidence = %.2f",
        designs$n[i], designs$content[i], designs$tolerance_confidence[i]))
    axis(1); axis(2, at = seq_along(laws)*7, labels = laws, las = 1, tick = FALSE)
    abline(h = (seq_len(length(laws)-1)+.5)*7, col = "gray90")
    segments(lo, y, hi, y, col = colors[z$method], lwd = 1.5)
    points(if (kind == "width") z$mean_width else z$attainment, y,
      pch = if (kind == "width") 4 else 16, col = colors[z$method], cex = .7)
    if (kind == "attainment") abline(v = designs$tolerance_confidence[i], lty = 2, col = "gray45")
  }
  grDevices::dev.off()
  conf <- data.table::as.data.table(s$confirmation)[method == "ecm"]
  macros <- c(MTIBoundaryConfirmationSuccesses = conf$successes,
    MTIBoundaryConfirmationSamples = conf$attempts,
    MTIBoundaryConfirmationLower = sprintf("%.3f", conf$lower_95),
    MTIBoundaryConfirmationUpper = sprintf("%.3f", conf$upper_95))
  number_file <- file.path(table_root, "numbers.tex")
  writeLines(sprintf("\\newcommand{\\%s}{%s}", names(macros), macros), number_file)
  inputs <- file.path(cfg$evidence_root, c("summary.json", "comparison_summary.csv"))
  programs <- "application/scripts/lib/mti_gibbs_boundary_closeout_figures.R"
  mgfp_json_write(list(inputs = mgfp_hashes(inputs), programs = mgfp_hashes(programs),
    artifacts = mgfp_hashes(c(path, number_file))), file.path(cfg$evidence_root, "rendered.json"))
  mbx_verify(cfg)
  invisible(path)
}
