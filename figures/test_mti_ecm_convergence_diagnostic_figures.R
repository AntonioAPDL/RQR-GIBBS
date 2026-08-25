#!/usr/bin/env Rscript

script <- normalizePath(
  "figures/generate_mti_ecm_convergence_diagnostic_figures.R",
  winslash = "/", mustWork = TRUE
)
work_dir <- tempfile("mti-ecm-trace-figure-test-")
dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
on.exit(unlink(work_dir, recursive = TRUE, force = TRUE), add = TRUE)

cells <- expand.grid(
  dgp_id = c("normal", "exponential"),
  n = c(50L, 500L),
  content = c(0.90, 0.99),
  job_rep = 1:2,
  stringsAsFactors = FALSE
)
cells$dgp <- ifelse(cells$dgp_id == "normal", "Gaussian",
                    "centered standardized exponential")
jobs <- data.frame(
  job_id = sprintf("job_%03d", seq_len(nrow(cells))),
  dgp_id = cells$dgp_id,
  dgp = cells$dgp,
  n = cells$n,
  content = cells$content,
  tolerance_confidence = 0.95,
  posterior_confidence = 0.95,
  replication = cells$job_rep,
  selected_convergence_rate = 1,
  selected_stationarity = 1e-4,
  stationarity_tolerance = 1e-3,
  status = "ok",
  stringsAsFactors = FALSE
)
trace_rows <- list()
for (ii in seq_len(nrow(jobs))) {
  iter <- 0:6
  trace_rows[[ii]] <- data.frame(
    job_id = jobs$job_id[[ii]],
    dgp_id = jobs$dgp_id[[ii]],
    dgp = jobs$dgp[[ii]],
    n = jobs$n[[ii]],
    content = jobs$content[[ii]],
    tolerance_confidence = 0.95,
    posterior_confidence = 0.95,
    replication = jobs$replication[[ii]],
    candidate_index = 1L,
    iteration = iter,
    objective = rev(seq_along(iter)),
    stationarity = c(NA, 10^seq(-1, -4, length.out = length(iter) - 1)),
    width = 2 + exp(-iter / 3),
    candidate_selected = TRUE,
    stringsAsFactors = FALSE
  )
}
traces <- do.call(rbind, trace_rows)
cell_summary <- do.call(rbind, lapply(
  split(jobs, paste(jobs$dgp_id, jobs$n, jobs$content)),
  function(x) {
    data.frame(
      dgp_id = x$dgp_id[[1L]],
      dgp = x$dgp[[1L]],
      n = x$n[[1L]],
      content = x$content[[1L]],
      tolerance_confidence = 0.95,
      posterior_confidence = 0.95,
      traced_replications = nrow(x),
      selected_convergence_rate = 1,
      selected_stationarity_max = 1e-4,
      stationarity_tolerance = 1e-3,
      stringsAsFactors = FALSE
    )
  }
))
utils::write.csv(jobs, file.path(work_dir, "mti_ecm_trace_jobs.csv"),
                 row.names = FALSE)
utils::write.csv(traces, file.path(work_dir, "mti_ecm_iteration_traces.csv"),
                 row.names = FALSE)
utils::write.csv(cell_summary,
                 file.path(work_dir, "mti_ecm_trace_cell_summary.csv"),
                 row.names = FALSE)

out_dir <- file.path(work_dir, "figures")
status <- system2(
  "Rscript",
  c(script, paste0("--trace-dir=", work_dir),
    paste0("--output-dir=", out_dir)),
  stdout = TRUE,
  stderr = TRUE
)
if (!identical(attr(status, "status"), NULL)) {
  cat(status, sep = "\n")
  stop("MTI-ECM convergence figure generator failed.", call. = FALSE)
}
manifest <- file.path(out_dir, "mti_ecm_convergence_figure_manifest.csv")
stopifnot(file.exists(manifest))
figures <- utils::read.csv(manifest, stringsAsFactors = FALSE)
stopifnot(nrow(figures) == 9L)
stopifnot(all(file.exists(figures$path)))
stopifnot(all(file.info(figures$path)$size > 5000))
cat("MTI-ECM convergence figure test passed.\n")
