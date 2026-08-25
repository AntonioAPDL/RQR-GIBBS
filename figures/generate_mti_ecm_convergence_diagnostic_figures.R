#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(prefix, default = NULL) {
  hit <- args[startsWith(args, prefix)]
  if (length(hit)) sub(prefix, "", hit[[1L]], fixed = TRUE) else default
}
stopf <- function(...) stop(paste0(...), call. = FALSE)
num <- function(x) suppressWarnings(as.numeric(x))

arguments <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_path <- sub(file_arg, "", arguments[startsWith(arguments, file_arg)][1L])
if (!length(script_path) || is.na(script_path)) {
  script_path <- "figures/generate_mti_ecm_convergence_diagnostic_figures.R"
}
script_path <- normalizePath(script_path, winslash = "/", mustWork = TRUE)
repo_root <- normalizePath(file.path(dirname(script_path), ".."),
                           winslash = "/", mustWork = TRUE)
setwd(repo_root)

trace_dir <- arg_value("--trace-dir=", NULL)
if (is.null(trace_dir)) {
  stopf("Provide --trace-dir=<MTI-ECM trace diagnostic output directory>.")
}
trace_dir <- normalizePath(trace_dir, winslash = "/", mustWork = TRUE)
output_dir <- normalizePath(
  arg_value("--output-dir=", file.path(trace_dir, "figures")),
  winslash = "/", mustWork = FALSE
)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
stationarity_tolerance <- num(arg_value("--stationarity-tolerance=", "0.001"))[1L]

trace_path <- file.path(trace_dir, "mti_ecm_iteration_traces.csv")
jobs_path <- file.path(trace_dir, "mti_ecm_trace_jobs.csv")
cell_path <- file.path(trace_dir, "mti_ecm_trace_cell_summary.csv")
for (path in c(trace_path, jobs_path, cell_path)) {
  if (!file.exists(path)) stopf("Missing diagnostic file: ", path)
}
traces <- utils::read.csv(trace_path, stringsAsFactors = FALSE,
                          check.names = FALSE)
jobs <- utils::read.csv(jobs_path, stringsAsFactors = FALSE,
                        check.names = FALSE)
cell_summary <- utils::read.csv(cell_path, stringsAsFactors = FALSE,
                                check.names = FALSE)
if (!nrow(traces)) stopf("Trace file contains no rows.")
traces <- traces[isTRUE(traces$candidate_selected) |
                   traces$candidate_selected %in% TRUE, , drop = FALSE]
if (!nrow(traces)) stopf("No selected-candidate traces were found.")

traces$n <- as.integer(traces$n)
traces$content <- num(traces$content)
traces$iteration <- as.integer(traces$iteration)
traces$stationarity <- num(traces$stationarity)
traces$width <- num(traces$width)
jobs$n <- as.integer(jobs$n)
jobs$content <- num(jobs$content)
cell_summary$n <- as.integer(cell_summary$n)
cell_summary$content <- num(cell_summary$content)
cell_summary$selected_stationarity_max <-
  num(cell_summary$selected_stationarity_max)
cell_summary$selected_convergence_rate <-
  num(cell_summary$selected_convergence_rate)

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
dgp_label <- function(id, fallback) {
  labels <- c(
    normal = "Gaussian",
    student_t3 = "Student t(3)",
    exponential = "Exponential",
    asym_laplace_tau010 = "Asym. Laplace",
    two_piece_normal_1_12 = "Two-piece",
    beta18 = "Beta(1,8)",
    gamma05 = "Gamma(0.5,1)",
    lognormal_hard = "Log-normal"
  )
  out <- unname(labels[as.character(id)])
  missing <- is.na(out)
  out[missing] <- fallback[missing]
  out
}
traces$dgp <- dgp_label(traces$dgp_id, traces$dgp)
jobs$dgp <- dgp_label(jobs$dgp_id, jobs$dgp)
cell_summary$dgp <- dgp_label(cell_summary$dgp_id, cell_summary$dgp)
dgp_rank <- function(id) {
  rank <- match(id, dgp_order)
  rank[is.na(rank)] <- length(dgp_order) +
    match(id[is.na(rank)], sort(unique(id[is.na(rank)])))
  rank
}
cell_label <- function(n, content) {
  sprintf("n=%d, c=%.2f", as.integer(n), num(content))
}
plot_groups <- function(dat, y_column, ylab, threshold = NULL,
                        main_prefix, file_prefix) {
  cells <- unique(dat[, c("n", "content"), drop = FALSE])
  cells <- cells[order(cells$n, cells$content), , drop = FALSE]
  paths <- character()
  for (ii in seq_len(nrow(cells))) {
    nn <- cells$n[[ii]]
    cc <- cells$content[[ii]]
    panel <- dat[dat$n == nn & abs(dat$content - cc) < 1e-12, , drop = FALSE]
    dgps <- unique(panel[, c("dgp_id", "dgp"), drop = FALSE])
    dgps <- dgps[order(dgp_rank(dgps$dgp_id)), , drop = FALSE]
    path <- file.path(
      output_dir,
      sprintf("%s_n%04d_c%03d.png", file_prefix, nn, round(100 * cc))
    )
    png(path, width = 2600, height = 2100, res = 220, pointsize = 13)
    old_par <- par(no.readonly = TRUE)
    device_open <- TRUE
    on.exit({
      if (isTRUE(device_open)) {
        par(old_par)
        dev.off()
      }
    }, add = TRUE)
    par(mfrow = c(4, 2), oma = c(0, 0, 2.0, 0),
        mar = c(3.2, 3.8, 2.0, 0.8), mgp = c(1.9, 0.6, 0))
    for (jj in seq_len(nrow(dgps))) {
      dgp_panel <- panel[panel$dgp_id == dgps$dgp_id[[jj]], , drop = FALSE]
      values <- num(dgp_panel[[y_column]])
      finite <- is.finite(values)
      ylim <- range(values[finite], finite = TRUE)
      if (!all(is.finite(ylim)) || diff(ylim) <= 0) {
        ylim <- ylim + c(-0.5, 0.5)
      }
      if (!is.null(threshold) && is.finite(threshold)) {
        ylim <- range(c(ylim, threshold), finite = TRUE)
      }
      plot(NA_real_, NA_real_,
           xlim = range(dgp_panel$iteration, finite = TRUE),
           ylim = ylim,
           xlab = "ECM iteration", ylab = ylab,
           main = dgps$dgp[[jj]], bty = "l")
      groups <- split(dgp_panel, dgp_panel$job_id)
      for (gg in groups) {
        gg <- gg[order(gg$iteration), , drop = FALSE]
        lines(gg$iteration, num(gg[[y_column]]), col = "#0072B2", lwd = 1.2)
      }
      if (!is.null(threshold) && is.finite(threshold)) {
        abline(h = threshold, col = "#D55E00", lty = 2, lwd = 1.1)
      }
      grid(col = "gray90")
    }
    mtext(paste(main_prefix, cell_label(nn, cc)), outer = TRUE,
          font = 2, cex = 1.0)
    par(old_par)
    dev.off()
    device_open <- FALSE
    paths <- c(paths, path)
  }
  paths
}

stationarity_traces <- traces
stationarity_traces$log10_stationarity <-
  log10(pmax(stationarity_traces$stationarity, 1e-12))
stationarity_paths <- plot_groups(
  stationarity_traces,
  y_column = "log10_stationarity",
  ylab = expression(log[10]("stationarity")),
  threshold = log10(stationarity_tolerance),
  main_prefix = "Selected MTI-ECM stationarity:",
  file_prefix = "mti_ecm_stationarity"
)
width_paths <- plot_groups(
  traces,
  y_column = "width",
  ylab = "Interval width",
  threshold = NULL,
  main_prefix = "Selected MTI-ECM width:",
  file_prefix = "mti_ecm_width"
)

overview_path <- file.path(output_dir, "mti_ecm_convergence_overview.png")
cells <- unique(cell_summary[, c("n", "content"), drop = FALSE])
cells <- cells[order(cells$n, cells$content), , drop = FALSE]
dgps <- unique(cell_summary[, c("dgp_id", "dgp"), drop = FALSE])
dgps <- dgps[order(dgp_rank(dgps$dgp_id)), , drop = FALSE]
z <- matrix(NA_real_, nrow = nrow(dgps), ncol = nrow(cells))
txt <- matrix("", nrow = nrow(dgps), ncol = nrow(cells))
for (ii in seq_len(nrow(dgps))) {
  for (jj in seq_len(nrow(cells))) {
    hit <- cell_summary[
      cell_summary$dgp_id == dgps$dgp_id[[ii]] &
        cell_summary$n == cells$n[[jj]] &
        abs(cell_summary$content - cells$content[[jj]]) < 1e-12,
      ,
      drop = FALSE
    ]
    if (nrow(hit)) {
      z[ii, jj] <- log10(pmax(hit$selected_stationarity_max[[1L]], 1e-12))
      txt[ii, jj] <- sprintf("%.0f%%", 100 * hit$selected_convergence_rate[[1L]])
    }
  }
}
png(overview_path, width = 2800, height = 1550, res = 220, pointsize = 14)
old_par <- par(no.readonly = TRUE)
par(mar = c(7.0, 6.0, 3.5, 1.0), mgp = c(2.1, 0.7, 0))
pal <- grDevices::colorRampPalette(c("#009E73", "#F0E442", "#D55E00"))(101)
zlim <- range(z, finite = TRUE)
image(seq_len(nrow(cells)), seq_len(nrow(dgps)), t(z),
      col = pal, zlim = zlim, axes = FALSE,
      xlab = "", ylab = "",
      main = "MTI-ECM selected-candidate convergence diagnostics")
axis(1, at = seq_len(nrow(cells)),
     labels = mapply(cell_label, cells$n, cells$content),
     las = 2, cex.axis = 0.74)
axis(2, at = seq_len(nrow(dgps)), labels = dgps$dgp, las = 1,
     cex.axis = 0.82)
mtext("Validation cell", side = 1, line = 5.8, cex = 0.95)
for (ii in seq_len(nrow(dgps))) {
  for (jj in seq_len(nrow(cells))) {
    if (nzchar(txt[ii, jj])) text(jj, ii, txt[ii, jj], cex = 0.72)
  }
}
box()
par(old_par)
dev.off()

paths <- c(overview_path, stationarity_paths, width_paths)
manifest <- data.frame(
  figure = basename(paths),
  path = paths,
  kind = c("overview", rep("stationarity", length(stationarity_paths)),
           rep("width", length(width_paths))),
  stringsAsFactors = FALSE
)
utils::write.csv(manifest,
                 file.path(output_dir, "mti_ecm_convergence_figure_manifest.csv"),
                 row.names = FALSE)
cat("MTI-ECM convergence figures written to: ", output_dir, "\n", sep = "")
