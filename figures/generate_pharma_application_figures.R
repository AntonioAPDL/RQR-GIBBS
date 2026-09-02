#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(prefix, default = NULL) {
  hit <- args[startsWith(args, prefix)]
  if (length(hit)) sub(prefix, "", hit[[1L]], fixed = TRUE) else default
}
stopf <- function(...) stop(paste0(...), call. = FALSE)

arguments <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_path <- sub(file_arg, "", arguments[startsWith(arguments, file_arg)][1L])
if (!length(script_path) || is.na(script_path)) {
  script_path <- "figures/generate_pharma_application_figures.R"
}
script_path <- normalizePath(script_path, winslash = "/", mustWork = TRUE)
repo_root <- normalizePath(file.path(dirname(script_path), ".."),
                           winslash = "/", mustWork = TRUE)
setwd(repo_root)

source(file.path(repo_root, "application", "scripts", "lib",
                 "pharma_tolerance_application.R"))

summary_csv <- arg_value(
  "--summary-csv=",
  file.path("tables", "pharma_application_summary.csv")
)
output_dir <- normalizePath(arg_value("--output-dir=", "figures/generated"),
                            winslash = "/", mustWork = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

summary <- pta_read_csv(summary_csv)
required <- c(
  "response_id", "response_role", "method_id", "method",
  "lower_median", "upper_median", "lower_q025", "upper_q975",
  "heldout_content_median", "width_median"
)
missing <- setdiff(required, names(summary))
if (length(missing)) {
  stopf("Application summary is missing column(s): ",
        paste(missing, collapse = ", "))
}

method_order <- c("tcsp_mc", "mti_ecm_adaptive_cell",
                  "young_mathew", "wilks_minmax")
method_colors <- c(
  tcsp_mc = "#0072B2",
  mti_ecm_adaptive_cell = "#009E73",
  young_mathew = "#CC79A7",
  wilks_minmax = "#000000"
)

draw_response <- function(rows, path, x_label, title) {
  rows <- rows[order(match(rows$method_id, method_order)), , drop = FALSE]
  png(path, width = 1800, height = 980, res = 180)
  on.exit(dev.off(), add = TRUE)
  op <- par(mar = c(4.6, 8.8, 2.0, 1.4), xaxs = "i")
  on.exit(par(op), add = TRUE)
  y <- rev(seq_len(nrow(rows)))
  x_min <- min(rows$lower_q025, rows$lower_median, na.rm = TRUE)
  x_max <- max(rows$upper_q975, rows$upper_median, na.rm = TRUE)
  pad <- 0.06 * (x_max - x_min)
  plot(
    NA,
    xlim = c(x_min - pad, x_max + pad),
    ylim = c(0.5, nrow(rows) + 0.5),
    yaxt = "n",
    xlab = x_label,
    ylab = "",
    main = title,
    bty = "n"
  )
  axis(2, at = y, labels = rows$method, las = 1, tick = FALSE)
  abline(v = pretty(c(x_min, x_max)), col = "gray92", lwd = 1)
  for (ii in seq_len(nrow(rows))) {
    col <- method_colors[[rows$method_id[[ii]]]]
    segments(rows$lower_q025[[ii]], y[[ii]], rows$upper_q975[[ii]], y[[ii]],
             col = grDevices::adjustcolor(col, alpha.f = 0.24), lwd = 12,
             lend = "butt")
    segments(rows$lower_median[[ii]], y[[ii]], rows$upper_median[[ii]], y[[ii]],
             col = col, lwd = 4, lend = "butt")
    points(c(rows$lower_median[[ii]], rows$upper_median[[ii]]),
           rep(y[[ii]], 2), pch = 19, col = col, cex = 0.8)
  }
  legend(
    "bottomright",
    legend = c("Median endpoints", "95% endpoint envelope"),
    lwd = c(4, 8),
    col = c("gray30", grDevices::adjustcolor("gray30", alpha.f = 0.24)),
    bty = "n",
    cex = 0.78
  )
}

primary_path <- file.path(output_dir,
                          "fig06_pharma_application_tensile_intervals.png")
supp_path <- file.path(output_dir,
                       "figS04_pharma_application_rsd_intervals.png")
draw_response(
  summary[summary$response_role == "primary", , drop = FALSE],
  primary_path,
  "Reported film-coated-tablet tensile-strength measure",
  "Held-out interval placement for code-23 tensile strength"
)
draw_response(
  summary[summary$response_role == "supplement", , drop = FALSE],
  supp_path,
  "Tablet-core weight relative standard deviation",
  "Held-out interval placement for code-23 weight RSD"
)

manifest <- data.frame(
  relative_path = c(
    file.path("figures", "generated", basename(primary_path)),
    file.path("figures", "generated", basename(supp_path))
  ),
  source_summary = summary_csv,
  sha256 = c(pta_file_sha256(primary_path), pta_file_sha256(supp_path)),
  bytes = c(file.info(primary_path)$size, file.info(supp_path)$size),
  generator = "figures/generate_pharma_application_figures.R",
  stringsAsFactors = FALSE
)
pta_atomic_write_csv(
  manifest,
  file.path(output_dir, "pharma_application_figure_manifest.csv")
)

cat("Wrote pharmaceutical application figures:\n")
cat("  ", primary_path, "\n", sep = "")
cat("  ", supp_path, "\n", sep = "")
