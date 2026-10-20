#!/usr/bin/env Rscript

args <- commandArgs(TRUE)
arguments <- commandArgs(FALSE)
path <- sub("--file=", "", arguments[startsWith(arguments, "--file=")][1], fixed = TRUE)
root <- normalizePath(file.path(dirname(path), ".."), mustWork = TRUE)
setwd(root)
for (name in c("pharma_tolerance_application.R", "pharma_mti_application.R", "pharma_mti_reporting.R")) {
  source(file.path("application/scripts/lib", name))
}
config <- pma_read_config(pta_arg_value(args, "--config=", pma_default_config))
source_path <- pta_arg_value(args, "--results-csv=", file.path(pma_canonical_dir, "results.csv"))
inputs <- pma_read_report_inputs(config, source_path)
output_dir <- pta_arg_value(args, "--output-dir=", "figures/generated")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
colors <- c(mti_ecm_adaptive_cell = "#009E73", young_mathew = "#CC79A7", wilks_minmax = "#000000")
labels <- c(mti_ecm_adaptive_cell = "MTI-ECM", young_mathew = "Young-Mathew", wilks_minmax = "Wilks")
paired <- pma_paired_values(inputs$results)
plotted_intervals <- list()

draw_application <- function(response, path) {
  rows <- inputs$results[inputs$results$response_id == response &
                          inputs$results$split_scheme == "random", ]
  representative <- inputs$representatives[inputs$representatives$response_id == response, ]
  fitted <- rows[rows$split_id == representative$split_id, ]
  fitted <- fitted[match(pma_methods, fitted$method_id), ]
  plotted_intervals[[response]] <<- fitted[, c("response_id", "method_id", "method_role",
    "split_id", "lower", "upper", "width")]
  y <- inputs$observed[[response]]
  png(path, width = 3000, height = 2400, res = 240, pointsize = 18)
  old <- par(mar = c(5.0, 6.2, 2.8, 1.0),
              oma = c(0, 0, 3.2, 0), mgp = c(3.3, .9, 0), cex = .88)
  layout(matrix(c(1,2,3,4,5,5), nrow = 3, byrow = TRUE), heights = c(1,1,.18))
  on.exit({ par(old); dev.off() })
  xr <- range(c(y, fitted$lower, fitted$upper))
  pad <- diff(xr) * .04
  plot(stats::ecdf(y), xlim = xr + c(-pad, pad), ylim = c(-.60, 1.06),
       axes = FALSE, xlab = "Observed response", ylab = "", main = "A. Observed distribution\nand representative intervals",
       verticals = TRUE, do.points = FALSE, col = "gray35", lwd = 1.4)
  axis(1)
  axis(2, at = c(0, .5, 1), labels = c("0", "0.5", "1"), las = 1)
  mtext("Empirical CDF", side = 2, line = 4.3, at = .55)
  abline(h = -.06, col = "gray85")
  positions <- c(-.17, -.34, -.51)
  axis(2, at = positions, labels = labels[pma_methods], las = 1, tick = FALSE, cex.axis = .83)
  for (i in seq_len(3)) {
    segments(fitted$lower[i], positions[i], fitted$upper[i], positions[i],
             col = colors[pma_methods[i]], lwd = 3.5)
    points(c(fitted$lower[i], fitted$upper[i]), rep(positions[i], 2),
           pch = 16, col = colors[pma_methods[i]], cex = .65)
  }
  box(bty = "l")
  for (i in seq_along(pma_methods[-1])) {
    comparator <- pma_methods[-1][i]
    z <- paired[paired$response_id == response & paired$split_scheme == "random" &
                  paired$method_id == comparator & paired$joint_return, ]
    x <- z$width_difference
    difference <- 100 * z$heldout_empirical_content_difference
    if (!length(x)) {
      plot.new(); title(main = paste0(LETTERS[i+1], ". ", labels[comparator]))
      text(.5, .5, "No jointly returned intervals")
    } else {
      plot(x, difference, pch = 16, cex = .55,
        col = grDevices::adjustcolor(colors[comparator], .16),
        xlim = range(c(x, 0)), ylim = range(c(difference, 0)),
        xlab = "Width difference\n(MTI-ECM minus benchmark)",
        ylab = "Held-out content difference\n(percentage points)",
        main = paste0(LETTERS[i+1], ". Paired comparison\nwith ", labels[comparator]), bty = "l")
      abline(h = 0, v = 0, col = "gray55", lty = 2)
      points(stats::median(x), stats::median(difference), pch = 4,
             cex = 1.5, lwd = 2.3, col = colors[1])
      legend("topleft", "Paired median", pch = 4, col = colors[1],
             bty = "n", cex = .78, pt.lwd = 2)
    }
  }
  # The final panel uses empirical quantiles, with paired lower/upper rows.
  positions <- c(7.1, 6.5, 4.6, 4.0, 2.1, 1.5)
  tail_limits <- unlist(lapply(pma_methods, function(method) {
    z <- rows[rows$method_id == method & rows$interval_returned, ]
    c(pta_quantile(z$heldout_lower_omitted, .975),
      pta_quantile(z$heldout_upper_omitted, .975))
  }))
  upper <- max(.05, tail_limits, na.rm = TRUE) * 100
  plot(NA, xlim = c(0, upper * 1.07), ylim = c(.7, 7.7), axes = FALSE,
       xlab = "Held-out omission (%)", ylab = "", main = "D. Lower- and upper-tail\nomission")
  axis(1)
  axis(2, at = c(6.8, 4.3, 1.8), labels = labels[pma_methods], las = 1, tick = FALSE)
  for (i in seq_along(pma_methods)) {
    z <- rows[rows$method_id == pma_methods[i] & rows$interval_returned, ]
    for (j in seq_len(2)) {
      vals <- 100 * z[[c("heldout_lower_omitted", "heldout_upper_omitted")[j]]]
      q <- vapply(c(.025, .25, .5, .75, .975), function(p) pta_quantile(vals, p), numeric(1))
      yy <- positions[(i - 1L)*2L + j]
      segments(q[1], yy, q[5], yy, col = colors[pma_methods[i]], lwd = 1.5)
      segments(q[2], yy, q[4], yy, col = colors[pma_methods[i]], lwd = 5)
      points(q[3], yy, pch = if(j == 1) 1 else 16,
             col = colors[pma_methods[i]], cex = .9)
    }
  }
  legend("bottomright", c("Lower tail", "Upper tail"), pch = c(1,16), bty = "n", cex = .78)
  box(bty = "l")
  mtext(paste0(pma_response_labels[response], ": placement, width, and held-out content"),
        outer = TRUE, line = .7, font = 2, cex = 1.05)
  par(mar = c(0,0,0,0))
  plot.new()
  legend("center", c("MTI-ECM (proposed)", "Young-Mathew (benchmark)", "Wilks (benchmark)"),
         col = colors[pma_methods], lwd = 3, bty = "n", ncol = 3, cex = .84)
}

draw_sensitivity <- function(path) {
  summary <- pma_summary(inputs$results)
  png(path, width = 3000, height = 2300, res = 240, pointsize = 18)
  old <- par(mar = c(4.5, 7.0, 2.3, 1.0),
              oma = c(0, 0, 1.3, 0), mgp = c(2.8, .8, 0), cex = .9)
  layout(matrix(c(1,2,3,4,5,5), nrow = 3, byrow = TRUE), heights = c(1,1,.18))
  on.exit({ par(old); dev.off() })
  offsets <- c(.20, 0, -.20)
  for (response in names(pma_response_labels)) {
    d <- summary[summary$response_id == response, ]
    for (quantity in c("width", "content")) {
      if (quantity == "width") {
        lo <- d$width_q025; hi <- d$width_q975; center <- d$width_median
        xlim <- range(c(lo,hi), finite = TRUE)
        xlab <- "Interval width"
      } else {
        lo <- d$heldout_content_q025; hi <- d$heldout_content_q975
        center <- d$heldout_content_median
        xlim <- c(0,1); xlab <- "Held-out empirical content"
      }
      plot(NA, xlim = xlim, ylim = c(.5, 5.5), yaxt = "n", ylab = "",
           xlab = xlab, main = paste(pma_response_labels[response], "-", quantity), bty = "l")
      plot_labels <- c("Random splits", "Forward chronology", "Reverse chronology",
                       "Month groups", "API groups")
      axis(2, at = 5:1, labels = plot_labels, las = 1, tick = FALSE, cex.axis = .82)
      if (quantity == "content") abline(v = config$analysis$content, lty = 2, col = "gray50")
      for (i in seq_len(nrow(d))) {
        method <- d$method_id[i]
        yy <- 6 - match(d$split_scheme[i], pma_schemes) + offsets[match(method,pma_methods)]
        segments(lo[i], yy, hi[i], yy, col = colors[method], lwd = 2)
        points(center[i], yy, pch = 16, col = colors[method], cex = .7)
      }
    }
  }
  par(mar = c(0,0,0,0))
  plot.new()
  legend("center", labels[pma_methods], col = colors[pma_methods],
         lwd = 2, ncol = 3, bty = "n", cex = .9)
}

files <- file.path(output_dir, c("fig06_pharma_application_tensile_intervals.png",
  "figS04_pharma_application_rsd_intervals.png", "figS05_pharma_application_partition_sensitivity.png"))
draw_application("fct_tensile", files[1])
draw_application("tbl_rsd_weight", files[2])
draw_sensitivity(files[3])
plotted_path <- file.path(output_dir, "pharma_application_plotted_intervals.csv")
pma_write_csv(do.call(rbind, plotted_intervals), plotted_path)
reps <- inputs$representatives$split_id[match(c("fct_tensile", "tbl_rsd_weight"),
                                            inputs$representatives$response_id)]
manifest <- data.frame(relative_path = file.path("figures/generated", basename(files)),
  source_results = source_path, source_results_sha256 = pta_file_sha256(source_path),
  source_membership_sha256 = pta_file_sha256(file.path(dirname(source_path), "split_membership.csv.gz")),
  source_observed_sha256 = pta_file_sha256(file.path(dirname(source_path), "observed_batches.csv")),
  generator = "figures/generate_pharma_mti_application_figures.R",
  response = c("fct_tensile", "tbl_rsd_weight", "both"),
  method_ids = paste(pma_methods, collapse = ";"), representative_split = c(reps, NA_integer_),
  representative_record_sha256 = pta_file_sha256(file.path(dirname(source_path), "representative_splits.json")),
  plotted_intervals_file = "figures/generated/pharma_application_plotted_intervals.csv",
  plotted_intervals_sha256 = pta_file_sha256(plotted_path),
  sha256 = vapply(files, pta_file_sha256, character(1)), bytes = as.numeric(file.info(files)$size),
  stringsAsFactors = FALSE)
pta_atomic_write_csv(manifest, file.path(output_dir, "pharma_application_figure_manifest.csv"))
cat("Generated three pharmaceutical figures from:", source_path, "\n")
