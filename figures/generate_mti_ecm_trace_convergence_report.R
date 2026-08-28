#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(prefix, default = NULL) {
  hit <- args[startsWith(args, prefix)]
  if (length(hit)) sub(prefix, "", hit[[1L]], fixed = TRUE) else default
}
stopf <- function(...) stop(paste0(...), call. = FALSE)
num <- function(x) suppressWarnings(as.numeric(x))
as_flag <- function(x) {
  if (is.logical(x)) return(x)
  tolower(as.character(x)) %in% c("true", "t", "1", "yes", "y")
}

arguments <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_path <- sub(file_arg, "", arguments[startsWith(arguments, file_arg)][1L])
if (!length(script_path) || is.na(script_path)) {
  script_path <- "figures/generate_mti_ecm_trace_convergence_report.R"
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
output_pdf <- normalizePath(
  arg_value(
    "--output-pdf=",
    file.path(trace_dir, "figures", "mti_ecm_trace_convergence_report.pdf")
  ),
  winslash = "/", mustWork = FALSE
)
output_dir <- dirname(output_pdf)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

stationarity_tolerance <- num(arg_value("--stationarity-tolerance=", "0.001"))[1L]
objective_tolerance <- num(arg_value("--objective-tolerance=", "1e-8"))[1L]
parameter_tolerance <- num(arg_value("--parameter-tolerance=", "1e-7"))[1L]
eps <- num(arg_value("--log-eps=", "1e-12"))[1L]
if (!is.finite(eps) || eps <= 0) eps <- 1e-12

trace_path <- file.path(trace_dir, "mti_ecm_iteration_traces.csv")
jobs_path <- file.path(trace_dir, "mti_ecm_trace_jobs.csv")
cell_path <- file.path(trace_dir, "mti_ecm_trace_cell_summary.csv")
for (path in c(trace_path, jobs_path, cell_path)) {
  if (!file.exists(path)) stopf("Missing diagnostic file: ", path)
}

needed_trace_cols <- c(
  "job_id", "dgp_id", "dgp", "n", "content", "replication",
  "iteration", "objective", "relative_objective_change",
  "relative_parameter_change", "stationarity",
  "minimum_absolute_residual_product", "backtracking_steps", "step_size",
  "root_swap_after_cycle", "precision_repairs", "condition_number_root1",
  "condition_number_root2", "root1", "root2", "width",
  "candidate_selected", "target_content", "mean_tilt", "central_tilt",
  "posterior_content_probability", "posterior_constraint_satisfied"
)
read_table <- function(path, select = NULL) {
  if (requireNamespace("data.table", quietly = TRUE)) {
    available <- names(data.table::fread(path, nrows = 0, showProgress = FALSE))
    select <- intersect(select %||% available, available)
    return(as.data.frame(data.table::fread(
      path, select = select, showProgress = FALSE
    )))
  }
  out <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  if (!is.null(select)) out <- out[, intersect(select, names(out)), drop = FALSE]
  out
}
`%||%` <- function(x, y) if (is.null(x)) y else x

traces_all <- read_table(trace_path, needed_trace_cols)
if (!"candidate_selected" %in% names(traces_all)) {
  stopf("Trace file does not contain candidate_selected.")
}
all_trace_rows <- nrow(traces_all)
traces <- traces_all[as_flag(traces_all$candidate_selected), , drop = FALSE]
rm(traces_all)
if (!nrow(traces)) stopf("No selected-candidate traces were found.")
jobs <- utils::read.csv(jobs_path, stringsAsFactors = FALSE, check.names = FALSE)
cell_summary <- utils::read.csv(cell_path, stringsAsFactors = FALSE,
                                check.names = FALSE)

dgp_order <- c(
  "normal", "student_t3", "exponential", "asym_laplace_tau010",
  "two_piece_normal_1_12", "beta18", "gamma05", "lognormal_hard"
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
dgp_rank <- function(id) {
  rank <- match(id, dgp_order)
  missing <- is.na(rank)
  rank[missing] <- length(dgp_order) +
    match(id[missing], sort(unique(id[missing])))
  rank
}
cell_label <- function(n, content) {
  sprintf("n = %d, c = %.2f", as.integer(n), num(content))
}
format_num <- function(x, digits = 3) {
  ifelse(is.finite(x), format(signif(x, digits), trim = TRUE), "NA")
}

traces$n <- as.integer(traces$n)
traces$content <- num(traces$content)
traces$iteration <- as.integer(traces$iteration)
for (col in intersect(needed_trace_cols, names(traces))) {
  if (!col %in% c("job_id", "dgp_id", "dgp", "candidate_selected",
                  "posterior_constraint_satisfied",
                  "root_swap_after_cycle")) {
    traces[[col]] <- num(traces[[col]])
  }
}
traces$root_swap_numeric <- as.integer(as_flag(traces$root_swap_after_cycle))
traces$posterior_constraint_satisfied <-
  as_flag(traces$posterior_constraint_satisfied)
traces$dgp <- dgp_label(traces$dgp_id, traces$dgp)
jobs$dgp <- dgp_label(jobs$dgp_id, jobs$dgp)
cell_summary$dgp <- dgp_label(cell_summary$dgp_id, cell_summary$dgp)
jobs$n <- as.integer(jobs$n)
jobs$content <- num(jobs$content)
cell_summary$n <- as.integer(cell_summary$n)
cell_summary$content <- num(cell_summary$content)

traces <- traces[order(traces$job_id, traces$iteration), , drop = FALSE]
split_jobs <- split(seq_len(nrow(traces)), traces$job_id)
traces$objective_gap <- NA_real_
for (idx in split_jobs) {
  objective <- traces$objective[idx]
  finite <- is.finite(objective)
  final <- if (any(finite)) utils::tail(objective[finite], 1L) else NA_real_
  gap <- if (is.finite(final)) pmax(objective - final, 0) else NA_real_
  traces$objective_gap[idx] <- gap
}
traces$log10_objective_gap <- log10(pmax(traces$objective_gap, eps))
traces$log10_stationarity <- log10(pmax(traces$stationarity, eps))
traces$log10_relative_objective_change <-
  log10(pmax(abs(traces$relative_objective_change), eps))
traces$log10_relative_parameter_change <-
  log10(pmax(abs(traces$relative_parameter_change), eps))
traces$log10_min_abs_residual_product <-
  log10(pmax(traces$minimum_absolute_residual_product, eps))
traces$log10_condition_root1 <-
  log10(pmax(traces$condition_number_root1, eps))
traces$log10_condition_root2 <-
  log10(pmax(traces$condition_number_root2, eps))

cells <- unique(traces[, c("n", "content"), drop = FALSE])
cells <- cells[order(cells$n, cells$content), , drop = FALSE]
dgps <- unique(traces[, c("dgp_id", "dgp"), drop = FALSE])
dgps <- dgps[order(dgp_rank(dgps$dgp_id)), , drop = FALSE]

page_count <- 0L
new_page <- function() {
  page_count <<- page_count + 1L
}
plot_title_page <- function() {
  new_page()
  plot.new()
  title <- "MTI-ECM Trace-Convergence Diagnostic Report"
  text(0.5, 0.88, title, cex = 1.55, font = 2)
  text(0.5, 0.80, "Current selected adaptive MTI-ECM validation run",
       cex = 1.0)
  lines <- c(
    paste("Trace directory:", trace_dir),
    paste("Output PDF:", output_pdf),
    paste("Created:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    paste("All candidate trace rows:", format(all_trace_rows, big.mark = ",")),
    paste("Selected-candidate trace rows:",
          format(nrow(traces), big.mark = ",")),
    paste("Diagnostic jobs:", nrow(jobs)),
    paste("Validation cells:", nrow(cell_summary)),
    paste("Stationarity tolerance:", stationarity_tolerance),
    paste("Objective-change tolerance:", objective_tolerance),
    paste("Parameter-change tolerance:", parameter_tolerance)
  )
  y <- 0.68
  for (line in lines) {
    text(0.06, y, line, adj = 0, cex = 0.78)
    y <- y - 0.052
  }
  text(
    0.06, 0.12,
    paste(
      "Diagnostic contents: selected-candidate ECM paths for the interval that the",
      "adaptive MTI-ECM rule returned in each diagnostic replay."
    ),
    adj = 0, cex = 0.82
  )
}

plot_heatmap <- function(value, label, main, transform = identity,
                         text_fun = function(x) format_num(x, 2)) {
  new_page()
  value <- as.character(value)
  if (!all(c(value, "dgp_id", "dgp", "n", "content") %in%
           names(cell_summary))) {
    plot.new()
    text(0.5, 0.5, paste("Missing heatmap column:", value), cex = 1.1)
    return(invisible())
  }
  mat <- matrix(NA_real_, nrow = nrow(dgps), ncol = nrow(cells))
  labels <- matrix("", nrow = nrow(dgps), ncol = nrow(cells))
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
        raw <- num(hit[[value]])[1L]
        mat[ii, jj] <- transform(raw)
        labels[ii, jj] <- text_fun(raw)
      }
    }
  }
  op <- par(no.readonly = TRUE)
  on.exit(par(op), add = TRUE)
  par(mar = c(7.0, 5.7, 3.5, 1.0), mgp = c(2.1, 0.7, 0))
  pal <- grDevices::colorRampPalette(c("#009E73", "#F0E442", "#D55E00"))(101)
  zlim <- range(mat, finite = TRUE)
  if (!all(is.finite(zlim)) || diff(zlim) <= 0) zlim <- zlim + c(-0.5, 0.5)
  image(seq_len(nrow(cells)), seq_len(nrow(dgps)), t(mat),
        col = pal, zlim = zlim, axes = FALSE, xlab = "", ylab = "",
        main = main)
  axis(1, at = seq_len(nrow(cells)),
       labels = mapply(cell_label, cells$n, cells$content),
       las = 2, cex.axis = 0.72)
  axis(2, at = seq_len(nrow(dgps)), labels = dgps$dgp, las = 1,
       cex.axis = 0.78)
  mtext("Validation cell", side = 1, line = 5.8, cex = 0.9)
  mtext(label, side = 3, line = 0.35, cex = 0.75)
  for (ii in seq_len(nrow(dgps))) {
    for (jj in seq_len(nrow(cells))) {
      if (nzchar(labels[ii, jj])) text(jj, ii, labels[ii, jj], cex = 0.68)
    }
  }
  box()
}

plot_metric_pages <- function(metric, ylab, section_title, threshold = NULL,
                              threshold_label = NULL) {
  if (!metric %in% names(traces)) return(invisible())
  for (cell_idx in seq_len(nrow(cells))) {
    new_page()
    nn <- cells$n[[cell_idx]]
    cc <- cells$content[[cell_idx]]
    panel <- traces[traces$n == nn &
                      abs(traces$content - cc) < 1e-12, , drop = FALSE]
    op <- par(no.readonly = TRUE)
    on.exit(par(op), add = TRUE)
    par(mfrow = c(4, 2), oma = c(0, 0, 2.0, 0),
        mar = c(3.1, 3.8, 2.0, 0.8), mgp = c(1.9, 0.6, 0))
    for (dgp_idx in seq_len(nrow(dgps))) {
      dgp_panel <- panel[panel$dgp_id == dgps$dgp_id[[dgp_idx]], ,
                         drop = FALSE]
      values <- num(dgp_panel[[metric]])
      finite <- is.finite(values)
      ylim <- range(values[finite], finite = TRUE)
      if (!all(is.finite(ylim))) ylim <- c(-0.5, 0.5)
      if (diff(ylim) <= 0) ylim <- ylim + c(-0.5, 0.5)
      if (!is.null(threshold) && is.finite(threshold)) {
        ylim <- range(c(ylim, threshold), finite = TRUE)
      }
      xlim <- range(dgp_panel$iteration, finite = TRUE)
      if (!all(is.finite(xlim))) xlim <- c(0, 1)
      if (diff(xlim) <= 0) xlim <- xlim + c(-0.5, 0.5)
      plot(NA_real_, NA_real_, xlim = xlim, ylim = ylim,
           xlab = "ECM iteration", ylab = ylab,
           main = dgps$dgp[[dgp_idx]], bty = "l")
      for (job in split(dgp_panel, dgp_panel$job_id)) {
        job <- job[order(job$iteration), , drop = FALSE]
        lines(job$iteration, num(job[[metric]]), col = "#0072B2", lwd = 1.1)
      }
      if (!is.null(threshold) && is.finite(threshold)) {
        abline(h = threshold, col = "#D55E00", lty = 2, lwd = 1)
        if (!is.null(threshold_label)) {
          mtext(threshold_label, side = 3, line = -1.1, adj = 1,
                cex = 0.58, col = "#D55E00")
        }
      }
      grid(col = "gray90")
    }
    mtext(paste(section_title, cell_label(nn, cc)), outer = TRUE,
          font = 2, cex = 1.0)
    par(op)
  }
}

plot_pair_pages <- function(metric1, metric2, ylab, section_title,
                            label1 = "root 1", label2 = "root 2") {
  if (!all(c(metric1, metric2) %in% names(traces))) return(invisible())
  for (cell_idx in seq_len(nrow(cells))) {
    new_page()
    nn <- cells$n[[cell_idx]]
    cc <- cells$content[[cell_idx]]
    panel <- traces[traces$n == nn &
                      abs(traces$content - cc) < 1e-12, , drop = FALSE]
    op <- par(no.readonly = TRUE)
    on.exit(par(op), add = TRUE)
    par(mfrow = c(4, 2), oma = c(0, 0, 2.0, 0),
        mar = c(3.1, 3.8, 2.0, 0.8), mgp = c(1.9, 0.6, 0))
    for (dgp_idx in seq_len(nrow(dgps))) {
      dgp_panel <- panel[panel$dgp_id == dgps$dgp_id[[dgp_idx]], ,
                         drop = FALSE]
      values <- c(num(dgp_panel[[metric1]]), num(dgp_panel[[metric2]]))
      ylim <- range(values, finite = TRUE)
      if (!all(is.finite(ylim))) ylim <- c(-0.5, 0.5)
      if (diff(ylim) <= 0) ylim <- ylim + c(-0.5, 0.5)
      xlim <- range(dgp_panel$iteration, finite = TRUE)
      if (!all(is.finite(xlim))) xlim <- c(0, 1)
      if (diff(xlim) <= 0) xlim <- xlim + c(-0.5, 0.5)
      plot(NA_real_, NA_real_, xlim = xlim, ylim = ylim,
           xlab = "ECM iteration", ylab = ylab,
           main = dgps$dgp[[dgp_idx]], bty = "l")
      for (job in split(dgp_panel, dgp_panel$job_id)) {
        job <- job[order(job$iteration), , drop = FALSE]
        lines(job$iteration, num(job[[metric1]]), col = "#0072B2", lwd = 1.0)
        lines(job$iteration, num(job[[metric2]]), col = "#009E73", lwd = 1.0,
              lty = 2)
      }
      if (dgp_idx == 1L) {
        legend("topright", legend = c(label1, label2),
               col = c("#0072B2", "#009E73"), lty = c(1, 2),
               lwd = 1, bty = "n", cex = 0.65)
      }
      grid(col = "gray90")
    }
    mtext(paste(section_title, cell_label(nn, cc)), outer = TRUE,
          font = 2, cex = 1.0)
    par(op)
  }
}

plot_selected_summary_pages <- function(metric, ylab, section_title) {
  if (!metric %in% names(jobs)) return(invisible())
  new_page()
  op <- par(no.readonly = TRUE)
  on.exit(par(op), add = TRUE)
  par(mfrow = c(3, 3), oma = c(0, 0, 2.0, 0),
      mar = c(5.0, 4.2, 2.0, 0.7), mgp = c(2.0, 0.6, 0))
  for (cell_idx in seq_len(nrow(cells))) {
    nn <- cells$n[[cell_idx]]
    cc <- cells$content[[cell_idx]]
    panel <- jobs[jobs$n == nn & abs(jobs$content - cc) < 1e-12, ,
                  drop = FALSE]
    panel <- panel[order(dgp_rank(panel$dgp_id), panel$replication), ,
                   drop = FALSE]
    dgp_levels <- unique(panel$dgp)
    y_pos <- seq_along(dgp_levels)
    values <- num(panel[[metric]])
    xlim <- range(values, finite = TRUE)
    if (!all(is.finite(xlim))) xlim <- c(-0.5, 0.5)
    if (diff(xlim) <= 0) xlim <- xlim + c(-0.5, 0.5)
    plot(NA_real_, NA_real_, xlim = xlim,
         ylim = c(0.5, length(dgp_levels) + 0.5),
         yaxt = "n", xlab = ylab, ylab = "",
         main = cell_label(nn, cc), bty = "l")
    axis(2, at = y_pos, labels = dgp_levels, las = 1, cex.axis = 0.62)
    abline(h = y_pos, col = "gray92")
    for (jj in seq_along(dgp_levels)) {
      block <- panel[panel$dgp == dgp_levels[[jj]], , drop = FALSE]
      points(num(block[[metric]]), rep(y_pos[[jj]], nrow(block)),
             pch = 16, col = "#0072B2", cex = 0.8)
      q <- stats::quantile(num(block[[metric]]), c(0, 0.5, 1),
                           na.rm = TRUE, names = FALSE)
      segments(q[[1L]], y_pos[[jj]], q[[3L]], y_pos[[jj]],
               col = "#0072B2", lwd = 1.3)
      points(q[[2L]], y_pos[[jj]], pch = 4, col = "#D55E00", cex = 0.8)
    }
    grid(col = "gray90")
  }
  mtext(section_title, outer = TRUE, font = 2, cex = 1.0)
  par(op)
}

pdf(output_pdf, width = 11, height = 8.5, onefile = TRUE,
    paper = "special")
plot_title_page()
plot_heatmap(
  "selected_convergence_rate",
  label = "Text is the selected-candidate convergence rate in traced jobs.",
  main = "Selected-candidate convergence rate by validation cell",
  transform = identity,
  text_fun = function(x) paste0(round(100 * x), "%")
)
plot_heatmap(
  "selected_stationarity_max",
  label = "Text is max selected-candidate stationarity across traced jobs.",
  main = "Worst selected-candidate stationarity by validation cell",
  transform = function(x) log10(pmax(x, eps)),
  text_fun = function(x) format_num(x, 2)
)
plot_selected_summary_pages(
  "selected_q",
  ylab = "Selected fitted content q",
  section_title = "Selected fitted content q across diagnostic replays"
)
plot_selected_summary_pages(
  "selected_tilt",
  ylab = "Selected mean tilt",
  section_title = "Selected mean tilt across diagnostic replays"
)
plot_selected_summary_pages(
  "selected_width",
  ylab = "Selected interval width",
  section_title = "Selected interval width across diagnostic replays"
)
plot_metric_pages(
  "log10_stationarity",
  ylab = expression(log[10]("stationarity")),
  section_title = "Stationarity trace:",
  threshold = log10(stationarity_tolerance),
  threshold_label = "tol"
)
plot_metric_pages(
  "log10_objective_gap",
  ylab = expression(log[10]("objective gap")),
  section_title = "Objective-gap trace:"
)
plot_metric_pages(
  "log10_relative_objective_change",
  ylab = expression(log[10]("relative objective change")),
  section_title = "Relative objective-change trace:",
  threshold = log10(objective_tolerance),
  threshold_label = "tol"
)
plot_metric_pages(
  "log10_relative_parameter_change",
  ylab = expression(log[10]("relative parameter change")),
  section_title = "Relative parameter-change trace:",
  threshold = log10(parameter_tolerance),
  threshold_label = "tol"
)
plot_pair_pages(
  "root1", "root2",
  ylab = "Endpoint root",
  section_title = "Endpoint-root traces:",
  label1 = "lower root", label2 = "upper root"
)
plot_metric_pages(
  "width",
  ylab = "Interval width",
  section_title = "Width trace:"
)
plot_metric_pages(
  "log10_min_abs_residual_product",
  ylab = expression(log[10]("minimum abs. residual product")),
  section_title = "Residual-product proximity trace:"
)
plot_metric_pages(
  "step_size",
  ylab = "Accepted step size",
  section_title = "Step-size trace:"
)
plot_metric_pages(
  "backtracking_steps",
  ylab = "Backtracking steps",
  section_title = "Backtracking trace:"
)
plot_metric_pages(
  "root_swap_numeric",
  ylab = "Root swap indicator",
  section_title = "Root-swap trace:"
)
plot_metric_pages(
  "precision_repairs",
  ylab = "Precision repairs",
  section_title = "Precision-repair trace:"
)
plot_pair_pages(
  "log10_condition_root1", "log10_condition_root2",
  ylab = expression(log[10]("condition number")),
  section_title = "Condition-number traces:",
  label1 = "root 1", label2 = "root 2"
)
dev.off()

manifest_path <- file.path(output_dir, "mti_ecm_trace_report_manifest.csv")
manifest <- data.frame(
  report = basename(output_pdf),
  path = output_pdf,
  trace_dir = trace_dir,
  all_candidate_trace_rows = all_trace_rows,
  selected_candidate_trace_rows = nrow(traces),
  jobs = nrow(jobs),
  cells = nrow(cell_summary),
  pages = page_count,
  created_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  stringsAsFactors = FALSE
)
utils::write.csv(manifest, manifest_path, row.names = FALSE)
cat("MTI-ECM trace-convergence PDF written to: ", output_pdf, "\n", sep = "")
cat("Pages: ", page_count, "\n", sep = "")
