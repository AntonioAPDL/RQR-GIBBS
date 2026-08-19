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
  script_path <- "figures/generate_tolerance_validation_primary_figure.R"
}
script_path <- normalizePath(script_path, winslash = "/", mustWork = TRUE)
repo_root <- normalizePath(file.path(dirname(script_path), ".."),
                           winslash = "/", mustWork = TRUE)
setwd(repo_root)

source_summary <- arg_value(
  "--source-summary-csv=",
  Sys.getenv("RQR_BAYES_UQ_PRIMARY_BY_N_CONTENT", unset = "")
)
stratified_csv <- arg_value(
  "--stratified-csv=",
  file.path("tables", "tolerance_validation_by_n_content.csv")
)
output_dir <- normalizePath(arg_value("--output-dir=", "figures/generated"),
                            winslash = "/", mustWork = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

figure_path <- file.path(output_dir, "fig04_tolerance_validation_primary.png")
manifest_path <- file.path(output_dir,
                           "tolerance_validation_primary_figure_manifest.csv")

method_order <- c(
  "tcsp_mc",
  "tcsp_mti_ecm_map_mc",
  "young_mathew",
  "wilks_minmax",
  "tcsp_dkw"
)
method_labels <- c(
  tcsp_mc = "TCSP",
  tcsp_mti_ecm_map_mc = "MTI ECM",
  young_mathew = "Young--Mathew",
  wilks_minmax = "Wilks",
  tcsp_dkw = "DKW"
)
method_colors <- c(
  tcsp_mc = "#0072B2",
  tcsp_mti_ecm_map_mc = "#D55E00",
  young_mathew = "#CC79A7",
  wilks_minmax = "#000000",
  tcsp_dkw = "#666666"
)
method_pch <- c(
  tcsp_mc = 16,
  tcsp_mti_ecm_map_mc = 15,
  young_mathew = 18,
  wilks_minmax = 4,
  tcsp_dkw = 1
)

derive_from_source <- function(path) {
  summary <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  required <- c(
    "method_id", "n", "c", "infeasible_rate", "success_rate",
    "median_width_ratio_to_tcsp", "median_elapsed_sec"
  )
  missing <- setdiff(required, names(summary))
  if (length(missing)) {
    stopf("Primary stratified summary is missing column(s): ",
          paste(missing, collapse = ", "))
  }
  selected <- summary[summary$method_id %in% method_order, , drop = FALSE]
  feasible_fraction <- pmax(0, 1 - as.numeric(selected$infeasible_rate))
  success_rate <- as.numeric(selected$success_rate)
  delivery <- feasible_fraction * success_rate
  delivery[!is.finite(delivery) & feasible_fraction == 0] <- 0
  returned_success <- ifelse(feasible_fraction > 0, success_rate, NA_real_)
  out <- data.frame(
    n = as.integer(selected$n),
    content = as.numeric(selected$c),
    method_id = selected$method_id,
    method = unname(method_labels[selected$method_id]),
    infeasible_rate = as.numeric(selected$infeasible_rate),
    delivery_success = delivery,
    returned_success = returned_success,
    median_width_ratio_to_tcsp =
      as.numeric(selected$median_width_ratio_to_tcsp),
    median_elapsed_sec = as.numeric(selected$median_elapsed_sec),
    stringsAsFactors = FALSE
  )
  out[order(out$n, out$content, match(out$method_id, method_order)), ]
}

if (nzchar(source_summary) && file.exists(source_summary)) {
  data <- derive_from_source(normalizePath(source_summary, winslash = "/",
                                          mustWork = TRUE))
  input_path <- normalizePath(source_summary, winslash = "/", mustWork = TRUE)
} else if (file.exists(stratified_csv)) {
  input_path <- normalizePath(stratified_csv, winslash = "/", mustWork = TRUE)
  data <- utils::read.csv(input_path, stringsAsFactors = FALSE,
                          check.names = FALSE)
} else if (file.exists(figure_path) && file.exists(manifest_path)) {
  cat("Using committed tolerance validation primary figure;",
      "provide --source-summary-csv or --stratified-csv to regenerate.\n")
  quit(status = 0)
} else {
  stopf("Missing source summary and stratified CSV for validation figure.")
}

required_data <- c(
  "n", "content", "method_id", "delivery_success",
  "median_width_ratio_to_tcsp"
)
missing_data <- setdiff(required_data, names(data))
if (length(missing_data)) {
  stopf("Validation figure data are missing column(s): ",
        paste(missing_data, collapse = ", "))
}
data <- data[data$method_id %in% method_order, , drop = FALSE]
if (!nrow(data)) stopf("Validation figure has no selected methods.")

png(figure_path, width = 2400, height = 1500, res = 220)
old_par <- par(no.readonly = TRUE)
device_open <- TRUE
on.exit({
  if (isTRUE(device_open)) {
    par(old_par)
    dev.off()
  }
}, add = TRUE)

layout(matrix(c(1, 2, 3, 4, 5, 5), nrow = 3, byrow = TRUE),
       heights = c(1, 1, 0.32))
par(oma = c(0, 0, 1.2, 0), mar = c(4.1, 4.4, 2.5, 0.9),
    xaxs = "i", yaxs = "i")

contents <- sort(unique(data$content))
offsets <- seq(-0.018, 0.018, length.out = length(method_order))
names(offsets) <- method_order

draw_panel <- function(nn, metric, ylab, main, reference, ylim) {
  plot(NA_real_, NA_real_, xlim = range(contents) + c(-0.035, 0.035),
       ylim = ylim, xaxt = "n", xlab = "Target content",
       ylab = ylab, main = main, bty = "l")
  axis(1, at = contents, labels = sprintf("%.2f", contents))
  abline(h = reference, col = "gray55", lty = 2, lwd = 1.2)
  panel <- data[data$n == nn, , drop = FALSE]
  for (method in method_order) {
    block <- panel[panel$method_id == method, , drop = FALSE]
    block <- block[order(block$content), , drop = FALSE]
    if (!nrow(block)) next
    x <- block$content + offsets[[method]]
    y <- as.numeric(block[[metric]])
    finite <- is.finite(y)
    if (sum(finite) >= 2L) {
      lines(x[finite], y[finite], col = method_colors[[method]], lwd = 1.5)
    }
    points(x[finite], y[finite], pch = method_pch[[method]],
           col = method_colors[[method]], bg = method_colors[[method]],
           cex = 1.05, lwd = 1.4)
  }
}

width_values <- data$median_width_ratio_to_tcsp
width_values <- width_values[is.finite(width_values)]
width_ylim <- c(0.75, max(1.15, width_values, na.rm = TRUE) * 1.08)

draw_panel(500, "delivery_success", "Delivery probability",
           "Delivery, n = 500", 0.95, c(0, 1.05))
draw_panel(1000, "delivery_success", "Delivery probability",
           "Delivery, n = 1000", 0.95, c(0, 1.05))
draw_panel(500, "median_width_ratio_to_tcsp", "Median width / TCSP",
           "Width, n = 500", 1, width_ylim)
draw_panel(1000, "median_width_ratio_to_tcsp", "Median width / TCSP",
           "Width, n = 1000", 1, width_ylim)

par(mar = c(0, 0, 0, 0))
plot.new()
legend("center", legend = unname(method_labels[method_order]),
       col = unname(method_colors[method_order]),
       pch = unname(method_pch[method_order]), lwd = 1.5,
       ncol = 3, bty = "n", xpd = NA, cex = 0.9)
mtext("Primary iid tolerance validation at tolerance confidence 0.95",
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
manifest <- data.frame(
  relative_path = c(rel_path(figure_path), rel_path(input_path)),
  sha256 = c(sha256(figure_path), sha256(input_path)),
  bytes = as.numeric(file.info(c(figure_path, input_path))$size),
  generator = "figures/generate_tolerance_validation_primary_figure.R",
  stringsAsFactors = FALSE
)
utils::write.csv(manifest, manifest_path, row.names = FALSE)

cat("Wrote tolerance validation primary figure:\n")
cat("  figure: ", figure_path, "\n", sep = "")
cat("  manifest: ", manifest_path, "\n", sep = "")
