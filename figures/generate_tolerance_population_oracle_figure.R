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
  script_path <- "figures/generate_tolerance_population_oracle_figure.R"
}
script_path <- normalizePath(script_path, winslash = "/", mustWork = TRUE)
repo_root <- normalizePath(file.path(dirname(script_path), ".."),
                           winslash = "/", mustWork = TRUE)
setwd(repo_root)

oracle_width_csv <- arg_value(
  "--oracle-width-csv=",
  file.path("figures", "data", "tolerance_population_shortest_content_oracle.csv")
)
output_dir <- normalizePath(arg_value("--output-dir=", "figures/generated"),
                            winslash = "/", mustWork = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

figure_path <- file.path(
  output_dir, "figS03_tolerance_population_shortest_intervals.png"
)
manifest_path <- file.path(
  output_dir, "tolerance_population_shortest_figure_manifest.csv"
)

if (!file.exists(oracle_width_csv)) {
  stopf("Missing population shortest-content interval CSV: ", oracle_width_csv)
}

num <- function(x) suppressWarnings(as.numeric(x))
format_content <- function(x) sprintf("%.2f", num(x))

oracle <- utils::read.csv(oracle_width_csv, stringsAsFactors = FALSE,
                          check.names = FALSE)
required <- c("dgp_id", "dgp_label", "content", "lower", "upper", "width")
missing <- setdiff(required, names(oracle))
if (length(missing)) {
  stopf("Population shortest-content CSV is missing column(s): ",
        paste(missing, collapse = ", "))
}
if ("status" %in% names(oracle)) {
  oracle <- oracle[oracle$status == "ok", , drop = FALSE]
}
oracle$content <- num(oracle$content)
oracle$lower <- num(oracle$lower)
oracle$upper <- num(oracle$upper)
oracle$width <- num(oracle$width)
oracle <- oracle[is.finite(oracle$content) & is.finite(oracle$lower) &
                   is.finite(oracle$upper) & is.finite(oracle$width),
                 , drop = FALSE]
if (!nrow(oracle)) stopf("No finite population shortest-content intervals.")

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
dgp_display <- c(
  normal = "Gaussian",
  student_t3 = "Student t3",
  exponential = "Exponential",
  asym_laplace_tau010 = "Asym. Laplace",
  two_piece_normal_1_12 = "Two-piece Normal",
  beta18 = "Beta(1,8)",
  gamma05 = "Gamma(0.5,1)",
  lognormal_hard = "Log-normal"
)
dgp_rank <- match(oracle$dgp_id, dgp_order)
missing_rank <- is.na(dgp_rank)
if (any(missing_rank)) {
  dgp_rank[missing_rank] <- length(dgp_order) +
    match(oracle$dgp_label[missing_rank],
          sort(unique(oracle$dgp_label[missing_rank])))
}
oracle <- oracle[order(dgp_rank, oracle$content), , drop = FALSE]
dgp_ids <- unique(oracle$dgp_id)
dgp_labels <- unname(dgp_display[dgp_ids])
dgp_labels[is.na(dgp_labels)] <- oracle$dgp_label[match(dgp_ids, oracle$dgp_id)]
content_levels <- sort(unique(oracle$content))
content_colors <- stats::setNames(
  c("#0072B2", "#009E73", "#D55E00")[seq_along(content_levels)],
  format_content(content_levels)
)
content_offsets <- stats::setNames(
  seq(-0.26, 0.26, length.out = length(content_levels)),
  format_content(content_levels)
)

png(figure_path, width = 4200, height = 2600, res = 220, pointsize = 18)
old_par <- par(no.readonly = TRUE)
device_open <- TRUE
on.exit({
  if (isTRUE(device_open)) {
    par(old_par)
    dev.off()
  }
}, add = TRUE)

par(mar = c(4.7, 9.5, 2.2, 0.8), mgp = c(2.2, 0.65, 0),
    xaxs = "i", yaxs = "i")
xlim <- range(c(oracle$lower, oracle$upper), finite = TRUE)
xpad <- 0.04 * diff(xlim)
if (!is.finite(xpad) || xpad <= 0) xpad <- 0.2
y_base <- seq_along(dgp_ids)
plot(NA_real_, NA_real_, xlim = xlim + c(-xpad, xpad),
     ylim = c(0.45, length(dgp_ids) + 0.55),
     xaxt = "n", yaxt = "n",
     xlab = "Interval endpoint (centered and scaled units)",
     ylab = "", bty = "l")
axis(1, cex.axis = 0.9)
axis(2, at = y_base, labels = rev(dgp_labels), las = 1, cex.axis = 0.9)
abline(v = 0, col = "gray88", lwd = 0.8)
abline(h = y_base, col = "gray92", lwd = 0.7)

for (ii in seq_len(nrow(oracle))) {
  content_key <- format_content(oracle$content[[ii]])
  y_group <- match(oracle$dgp_id[[ii]], rev(dgp_ids))
  y <- y_group + content_offsets[[content_key]]
  col <- content_colors[[content_key]]
  segments(oracle$lower[[ii]], y, oracle$upper[[ii]], y, col = col, lwd = 2.5)
  points(c(oracle$lower[[ii]], oracle$upper[[ii]]), c(y, y),
         col = col, pch = 16, cex = 0.58)
}

legend("bottomright", legend = paste0("c = ", format_content(content_levels)),
       col = unname(content_colors[format_content(content_levels)]),
       lty = 1, lwd = 2.5, pch = 16, bty = "n", cex = 0.92)
title("Shortest population content intervals", cex.main = 1.05, font.main = 2)

par(old_par)
dev.off()
device_open <- FALSE

if (!requireNamespace("digest", quietly = TRUE)) {
  stopf("Package 'digest' is required for population-figure hash records.")
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
manifest_inputs <- c(figure_path, oracle_width_csv)
manifest <- data.frame(
  relative_path = vapply(manifest_inputs, rel_path, character(1L)),
  sha256 = vapply(manifest_inputs, sha256, character(1L)),
  bytes = as.numeric(file.info(manifest_inputs)$size),
  generator = "figures/generate_tolerance_population_oracle_figure.R",
  stringsAsFactors = FALSE
)
utils::write.csv(manifest, manifest_path, row.names = FALSE)

cat("Wrote population shortest-content interval figure:\n")
cat("  figure: ", figure_path, "\n", sep = "")
cat("  hash record: ", manifest_path, "\n", sep = "")
