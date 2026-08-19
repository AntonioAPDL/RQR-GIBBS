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

default_primary_dir <- file.path(
  "application", "runs", "rqr_bayes_uq_validation_main_20260813",
  "wave_main_20260813T103232Z"
)
primary_results <- arg_value(
  "--primary-results=",
  Sys.getenv("RQR_BAYES_UQ_PRIMARY_RESULTS",
             unset = file.path(default_primary_dir,
                               "bayes_uq_validation_results.csv"))
)
young_mathew_results <- arg_value(
  "--young-mathew-results=",
  Sys.getenv("RQR_BAYES_UQ_PRIMARY_YM_RESULTS",
             unset = file.path(default_primary_dir,
                               "young_mathew_addon_20260815T064224Z",
                               "bayes_uq_validation_results.csv"))
)
scenario_range_csv <- arg_value(
  "--scenario-range-csv=",
  file.path("tables", "tolerance_validation_article_scenario_ranges.csv")
)
legacy_stratified_csv <- arg_value("--stratified-csv=", "")
output_dir <- normalizePath(arg_value("--output-dir=", "figures/generated"),
                            winslash = "/", mustWork = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

figure_path <- file.path(output_dir, "fig04_tolerance_validation_primary.png")
manifest_path <- file.path(output_dir,
                           "tolerance_validation_primary_figure_manifest.csv")

method_order <- c("tcsp_mc", "young_mathew", "wilks_minmax")
method_labels <- c(
  tcsp_mc = "TCSP",
  young_mathew = "Young--Mathew",
  wilks_minmax = "Wilks"
)
method_colors <- c(
  tcsp_mc = "#0072B2",
  young_mathew = "#CC79A7",
  wilks_minmax = "#000000"
)
method_lty <- c(
  tcsp_mc = 1,
  young_mathew = 1,
  wilks_minmax = 2
)

num <- function(x) suppressWarnings(as.numeric(x))
truthy <- function(x) {
  if (is.logical(x)) return(!is.na(x) & x)
  if (is.numeric(x)) return(!is.na(x) & x != 0)
  x <- tolower(trimws(as.character(x)))
  !is.na(x) & x %in% c("true", "t", "1", "yes", "y")
}
quantile_or_na <- function(x, prob) {
  x <- num(x)
  x <- x[is.finite(x)]
  if (length(x)) {
    unname(stats::quantile(x, prob, names = FALSE, type = 8))
  } else {
    NA_real_
  }
}
min_or_na <- function(x) {
  x <- num(x)
  x <- x[is.finite(x)]
  if (length(x)) min(x) else NA_real_
}
max_or_na <- function(x) {
  x <- num(x)
  x <- x[is.finite(x)]
  if (length(x)) max(x) else NA_real_
}

bind_fill <- function(...) {
  frames <- list(...)
  frames <- frames[vapply(frames, nrow, integer(1L)) > 0L]
  if (!length(frames)) return(data.frame())
  cols <- Reduce(union, lapply(frames, names))
  frames <- lapply(frames, function(x) {
    missing <- setdiff(cols, names(x))
    for (col in missing) x[[col]] <- NA
    x[, cols, drop = FALSE]
  })
  do.call(rbind, frames)
}

read_result_file <- function(path, label) {
  out <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  required <- c(
    "dgp_id", "n", "guaranteed_content", "method_id", "success",
    "infeasible"
  )
  missing <- setdiff(required, names(out))
  if (length(missing)) {
    stopf(label, " is missing column(s): ", paste(missing, collapse = ", "))
  }
  out
}

derive_ranges_from_raw <- function(primary_path, ym_path) {
  primary <- read_result_file(primary_path, "Primary validation results")
  frames <- list(primary)
  if (file.exists(ym_path) && !any(primary$method_id == "young_mathew")) {
    frames <- c(frames, list(read_result_file(ym_path, "Young--Mathew results")))
  }
  raw <- do.call(bind_fill, frames)
  raw <- raw[raw$method_id %in% method_order, , drop = FALSE]
  if (!nrow(raw)) stopf("No selected methods are present in raw figure inputs.")
  raw$n <- as.integer(raw$n)
  raw$content <- num(raw$guaranteed_content)
  raw$success_bool <- truthy(raw$success)
  raw$infeasible_bool <- truthy(raw$infeasible)
  if ("tolerance_confidence" %in% names(raw)) {
    raw$tolerance_confidence <- num(raw$tolerance_confidence)
  } else {
    raw$tolerance_confidence <- NA_real_
  }
  if ("posterior_confidence" %in% names(raw)) {
    raw$posterior_confidence <- num(raw$posterior_confidence)
  } else {
    raw$posterior_confidence <- NA_real_
  }
  if ("replication" %in% names(raw)) {
    raw$replication <- as.integer(raw$replication)
    ord <- order(raw$dgp_id, raw$n, raw$content,
                 raw$tolerance_confidence, raw$method_id, raw$replication,
                 raw$posterior_confidence, na.last = TRUE)
    raw <- raw[ord, , drop = FALSE]
    rep_key <- paste(
      raw$dgp_id, raw$n, sprintf("%.4f", raw$content),
      sprintf("%.4f", raw$tolerance_confidence), raw$method_id,
      raw$replication,
      sep = "||"
    )
    raw <- raw[!duplicated(rep_key), , drop = FALSE]
  }

  dgp_key <- paste(raw$dgp_id, raw$n, sprintf("%.4f", raw$content),
                   raw$method_id, sep = "||")
  dgp_rows <- lapply(split(raw, dgp_key), function(df) {
    data.frame(
      dgp_id = df$dgp_id[[1L]],
      n = as.integer(df$n[[1L]]),
      content = num(df$content[[1L]]),
      method_id = df$method_id[[1L]],
      delivery_success = mean(!df$infeasible_bool & df$success_bool),
      stringsAsFactors = FALSE
    )
  })
  dgp_detail <- do.call(rbind, dgp_rows)

  key <- paste(dgp_detail$n, sprintf("%.4f", dgp_detail$content),
               dgp_detail$method_id, sep = "||")
  rows <- lapply(split(dgp_detail, key), function(df) {
    data.frame(
      n = as.integer(df$n[[1L]]),
      content = num(df$content[[1L]]),
      method_id = df$method_id[[1L]],
      method = unname(method_labels[df$method_id[[1L]]]),
      delivery_min = min_or_na(df$delivery_success),
      delivery_max = max_or_na(df$delivery_success),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out[order(out$n, out$content, match(out$method_id, method_order)), ]
}

read_range_table <- function(path) {
  out <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  required <- c(
    "n", "content", "method_id", "delivery_min", "delivery_max"
  )
  missing <- setdiff(required, names(out))
  if (length(missing)) {
    stopf("Scenario-range figure input is missing column(s): ",
          paste(missing, collapse = ", "))
  }
  out <- out[out$method_id %in% method_order, , drop = FALSE]
  out$method <- unname(method_labels[out$method_id])
  out[order(out$n, out$content, match(out$method_id, method_order)), ]
}

input_paths <- character()
if (file.exists(primary_results)) {
  data <- derive_ranges_from_raw(primary_results, young_mathew_results)
  input_paths <- c(primary_results,
                   young_mathew_results[file.exists(young_mathew_results)])
} else if (file.exists(scenario_range_csv)) {
  data <- read_range_table(scenario_range_csv)
  input_paths <- scenario_range_csv
} else if (nzchar(legacy_stratified_csv) && file.exists(legacy_stratified_csv)) {
  stopf("The validation figure now requires scenario ranges or raw result CSVs; ",
        "the legacy stratified summary lacks range endpoints.")
} else if (file.exists(figure_path) && file.exists(manifest_path)) {
  cat("Using committed tolerance validation primary figure;",
      "provide raw results or --scenario-range-csv to regenerate.\n")
  quit(status = 0)
} else {
  stopf("Missing raw result CSVs and scenario-range CSV for validation figure.")
}

if (!nrow(data)) stopf("Validation figure has no selected methods.")

contents <- sort(unique(data$content))
sample_sizes <- sort(unique(data$n))
offsets <- seq(-0.018, 0.018, length.out = length(method_order))
names(offsets) <- method_order

panel_count <- length(sample_sizes)
panel_cols <- min(2L, panel_count)
panel_rows <- ceiling(panel_count / panel_cols)
panel_matrix <- matrix(seq_len(panel_rows * panel_cols), nrow = panel_rows,
                       ncol = panel_cols, byrow = TRUE)
panel_matrix[panel_matrix > panel_count] <- 0L
legend_row <- rep(panel_count + 1L, panel_cols)

png(figure_path, width = 2400, height = 650 * panel_rows + 260, res = 220)
old_par <- par(no.readonly = TRUE)
device_open <- TRUE
on.exit({
  if (isTRUE(device_open)) {
    par(old_par)
    dev.off()
  }
}, add = TRUE)

layout(rbind(panel_matrix, legend_row),
       heights = c(rep(1, panel_rows), 0.28))
par(oma = c(0, 0, 1.2, 0), mar = c(4.1, 4.6, 2.5, 0.9),
    xaxs = "i", yaxs = "i")

draw_interval_panel <- function(nn, lower_col, upper_col, ylab, main,
                                reference = NA_real_, ylim) {
  plot(NA_real_, NA_real_, xlim = range(contents) + c(-0.035, 0.035),
       ylim = ylim, xaxt = "n", xlab = "Target content",
       ylab = ylab, main = main, bty = "l")
  axis(1, at = contents, labels = sprintf("%.2f", contents))
  if (is.finite(reference)) {
    abline(h = reference, col = "gray55", lty = 3, lwd = 1.2)
  }
  panel <- data[data$n == nn, , drop = FALSE]
  cap <- 0.004
  for (method in method_order) {
    block <- panel[panel$method_id == method, , drop = FALSE]
    block <- block[order(block$content), , drop = FALSE]
    if (!nrow(block)) next
    x <- block$content + offsets[[method]]
    lower <- num(block[[lower_col]])
    upper <- num(block[[upper_col]])
    ok <- is.finite(lower) & is.finite(upper)
    if (!any(ok)) next
    col <- method_colors[[method]]
    lty <- method_lty[[method]]
    segments(x[ok], lower[ok], x[ok], upper[ok], col = col, lty = lty, lwd = 2.4)
    segments(x[ok] - cap, lower[ok], x[ok] + cap, lower[ok],
             col = col, lty = lty, lwd = 2.0)
    segments(x[ok] - cap, upper[ok], x[ok] + cap, upper[ok],
             col = col, lty = lty, lwd = 2.0)
  }
}

for (nn in sample_sizes) {
  draw_interval_panel(nn, "delivery_min", "delivery_max",
                      "Delivery probability",
                      sprintf("Delivery range, n = %s", nn),
                      0.95, c(0.9, 1.0))
}
if (any(panel_matrix == 0L)) {
  plot.new()
}

par(mar = c(0, 0, 0, 0))
plot.new()
legend("center", legend = unname(method_labels[method_order]),
       col = unname(method_colors[method_order]),
       lty = unname(method_lty[method_order]), lwd = 2.4,
       ncol = 3, bty = "n", xpd = NA, cex = 0.9)
mtext("Article-facing iid tolerance validation at tolerance confidence 0.95",
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
manifest_inputs <- c(figure_path, input_paths)
manifest <- data.frame(
  relative_path = vapply(manifest_inputs, rel_path, character(1L)),
  sha256 = vapply(manifest_inputs, sha256, character(1L)),
  bytes = as.numeric(file.info(manifest_inputs)$size),
  generator = "figures/generate_tolerance_validation_primary_figure.R",
  stringsAsFactors = FALSE
)
utils::write.csv(manifest, manifest_path, row.names = FALSE)

cat("Wrote tolerance validation primary figure:\n")
cat("  figure: ", figure_path, "\n", sep = "")
cat("  manifest: ", manifest_path, "\n", sep = "")
