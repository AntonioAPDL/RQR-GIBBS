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
  script_path <- "figures/generate_tolerance_validation_width_figure.R"
}
script_path <- normalizePath(script_path, winslash = "/", mustWork = TRUE)
repo_root <- normalizePath(file.path(dirname(script_path), ".."),
                           winslash = "/", mustWork = TRUE)
setwd(repo_root)

width_csv <- arg_value(
  "--width-csv=",
  file.path("tables", "tolerance_validation_article_dgp_width_ranges.csv")
)
output_dir <- normalizePath(arg_value("--output-dir=", "figures/generated"),
                            winslash = "/", mustWork = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

figure_path <- file.path(output_dir, "fig05_tolerance_validation_width_ranges.png")
manifest_path <- file.path(
  output_dir, "tolerance_validation_width_figure_manifest.csv"
)

selected_mti_ecm_method <- "mti_ecm_adaptive_cell"
method_order <- c(
  "tcsp_mc",
  selected_mti_ecm_method,
  "young_mathew",
  "wilks_minmax"
)
method_labels <- c(
  tcsp_mc = "TCSP",
  mti_ecm_adaptive_cell = "MTI-ECM",
  young_mathew = "Young--Mathew",
  wilks_minmax = "Wilks"
)
method_colors <- c(
  tcsp_mc = "#0072B2",
  mti_ecm_adaptive_cell = "#009E73",
  young_mathew = "#CC79A7",
  wilks_minmax = "#000000"
)
method_pch <- c(
  tcsp_mc = 16,
  mti_ecm_adaptive_cell = 17,
  young_mathew = 16,
  wilks_minmax = 1
)
dgp_order <- c(
  "normal",
  "student_t3",
  "exponential",
  "asym_laplace_tau010",
  "two_piece_normal_1_12",
  "beta18",
  "gamma05",
  "lognormal_hard"
)
dgp_rank <- function(id, label) {
  rank <- match(id, dgp_order)
  missing <- is.na(rank)
  if (any(missing)) {
    rank[missing] <- length(dgp_order) +
      match(label[missing], sort(unique(label[missing])))
  }
  rank
}

num <- function(x) suppressWarnings(as.numeric(x))
format_content <- function(x) sprintf("%.2f", num(x))
format_width_tick <- function(x) {
  x <- num(x)
  ifelse(
    x < 1,
    format(signif(x, 2), trim = TRUE, scientific = FALSE),
    ifelse(
      x < 10,
      format(signif(x, 2), trim = TRUE, scientific = FALSE),
      format(round(x), trim = TRUE, scientific = FALSE)
    )
  )
}

if (!file.exists(width_csv)) {
  if (file.exists(figure_path) && file.exists(manifest_path)) {
    cat("Using committed tolerance validation width figure;",
        "provide --width-csv to regenerate.\n")
    quit(status = 0)
  }
  stopf("Missing width-range CSV: ", width_csv)
}

data <- utils::read.csv(width_csv, stringsAsFactors = FALSE,
                        check.names = FALSE)
required <- c("dgp", "n", "content", "method_id", "width_q025", "width_q975")
missing <- setdiff(required, names(data))
if (length(missing)) {
  stopf("Width-range CSV is missing column(s): ", paste(missing, collapse = ", "))
}
data <- data[data$method_id %in% method_order, , drop = FALSE]
if (!"dgp_id" %in% names(data)) data$dgp_id <- data$dgp
data$n <- as.integer(data$n)
data$content <- num(data$content)
data$width_q025 <- num(data$width_q025)
data$width_q975 <- num(data$width_q975)
data$mean_width <- if ("mean_width" %in% names(data)) {
  num(data$mean_width)
} else {
  rep(NA_real_, nrow(data))
}
data <- data[is.finite(data$width_q025) & is.finite(data$width_q975) &
               data$width_q025 > 0 & data$width_q975 > 0, , drop = FALSE]
if (!nrow(data)) stopf("Width-range figure has no finite selected rows.")
method_order <- method_order[method_order %in% unique(data$method_id)]

cells <- unique(data[, c("n", "content"), drop = FALSE])
cells <- cells[order(cells$n, cells$content), , drop = FALSE]
dgp_rows <- unique(data[, c("dgp_id", "dgp"), drop = FALSE])
dgp_rows <- dgp_rows[order(dgp_rank(dgp_rows$dgp_id, dgp_rows$dgp)), ,
                     drop = FALSE]
dgp_levels <- rev(dgp_rows$dgp)
method_offsets <- stats::setNames(
  seq(-0.30, 0.30, length.out = length(method_order)),
  method_order
)

panel_count <- nrow(cells)
panel_cols <- min(3L, panel_count)
panel_rows <- ceiling(panel_count / panel_cols)
panel_matrix <- matrix(seq_len(panel_rows * panel_cols), nrow = panel_rows,
                       ncol = panel_cols, byrow = TRUE)
panel_matrix[panel_matrix > panel_count] <- 0L
legend_row <- rep(panel_count + 1L, panel_cols)

png(figure_path, width = 3800, height = 900 * panel_rows + 360, res = 220,
    pointsize = 17)
old_par <- par(no.readonly = TRUE)
device_open <- TRUE
on.exit({
  if (isTRUE(device_open)) {
    par(old_par)
    dev.off()
  }
}, add = TRUE)

layout(rbind(panel_matrix, legend_row),
       heights = c(rep(1, panel_rows), 0.22))
par(oma = c(0, 0, 1.3, 0), mar = c(4.2, 10.6, 2.4, 0.8),
    mgp = c(1.85, 0.55, 0), xaxs = "i", yaxs = "i")

for (ii in seq_len(nrow(cells))) {
  nn <- cells$n[[ii]]
  cc <- cells$content[[ii]]
  panel <- data[data$n == nn & abs(data$content - cc) < 1e-12, , drop = FALSE]
  positive_mean <- panel$mean_width[is.finite(panel$mean_width) &
                                      panel$mean_width > 0]
  x_range <- range(c(panel$width_q025, panel$width_q975, positive_mean),
                   finite = TRUE)
  xlim <- log10(x_range)
  pad <- 0.04 * diff(xlim)
  if (!is.finite(pad) || pad <= 0) pad <- 0.05
  y_base <- seq_along(dgp_levels)
  plot(NA_real_, NA_real_, xlim = xlim + c(-pad, pad),
       ylim = c(0.45, length(dgp_levels) + 0.55),
       xaxt = "n", yaxt = "n", xlab = "Interval width (log scale)",
       ylab = "", main = sprintf("n = %s, c = %s", nn, format_content(cc)),
       bty = "l", cex.lab = 0.98, cex.main = 1.02)
  ticks <- pretty(xlim, n = 3)
  ticks <- ticks[ticks >= xlim[[1L]] & ticks <= xlim[[2L]]]
  axis(1, at = ticks, labels = format_width_tick(10^ticks), cex.axis = 0.86)
  axis(2, at = y_base, labels = dgp_levels, las = 1, cex.axis = 0.82)
  abline(h = y_base, col = "gray92", lwd = 0.7)
  for (method in method_order) {
    block <- panel[panel$method_id == method, , drop = FALSE]
    for (jj in seq_len(nrow(block))) {
      y <- match(block$dgp[[jj]], dgp_levels) + method_offsets[[method]]
      lower <- log10(block$width_q025[[jj]])
      upper <- log10(block$width_q975[[jj]])
      col <- method_colors[[method]]
      segments(lower, y, upper, y, col = col, lwd = 2)
      points(c(lower, upper), c(y, y), col = col, pch = method_pch[[method]],
             cex = 0.55)
      if (is.finite(block$mean_width[[jj]]) && block$mean_width[[jj]] > 0) {
        points(log10(block$mean_width[[jj]]), y, col = col, pch = 4,
               cex = 0.82, lwd = 1.5)
      }
    }
  }
}

par(mar = c(0, 0, 0, 0))
plot.new()
legend("center", legend = c(unname(method_labels[method_order]), "Mean width"),
       col = c(unname(method_colors[method_order]), "gray20"),
       pch = c(unname(method_pch[method_order]), 4),
       lwd = c(rep(2, length(method_order)), NA),
       ncol = length(method_order) + 1L, bty = "n", xpd = NA, cex = 0.95)
mtext("Interval-width ranges by distribution",
      outer = TRUE, cex = 1.05, font = 2)

par(old_par)
dev.off()
device_open <- FALSE

if (!requireNamespace("digest", quietly = TRUE)) {
  stopf("Package 'digest' is required for validation-figure manifests.")
}
sha256 <- function(path) digest::digest(file = path, algo = "sha256",
                                        serialize = FALSE)
rel_path <- function(path) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  root_prefix <- paste0(repo_root, "/")
  if (startsWith(path, root_prefix)) {
    substr(path, nchar(root_prefix) + 1L, nchar(path))
  } else {
    path
  }
}
manifest_inputs <- c(figure_path, width_csv)
manifest <- data.frame(
  relative_path = vapply(manifest_inputs, rel_path, character(1L)),
  sha256 = vapply(manifest_inputs, sha256, character(1L)),
  bytes = as.numeric(file.info(manifest_inputs)$size),
  generator = "figures/generate_tolerance_validation_width_figure.R",
  stringsAsFactors = FALSE
)
utils::write.csv(manifest, manifest_path, row.names = FALSE)

cat("Wrote tolerance validation width figure:\n")
cat("  figure: ", figure_path, "\n", sep = "")
cat("  manifest: ", manifest_path, "\n", sep = "")
