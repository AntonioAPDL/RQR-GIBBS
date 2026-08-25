#!/usr/bin/env Rscript

script <- normalizePath(
  "figures/generate_mti_ecm_trace_convergence_report.R",
  winslash = "/", mustWork = TRUE
)
work_dir <- tempfile("mti-ecm-trace-report-test-")
dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
on.exit(unlink(work_dir, recursive = TRUE, force = TRUE), add = TRUE)

dgps <- data.frame(
  dgp_id = c("normal", "exponential"),
  dgp = c("Gaussian", "Exponential"),
  stringsAsFactors = FALSE
)
jobs <- expand.grid(
  dgp_id = dgps$dgp_id,
  n = c(50L, 100L),
  content = c(0.90, 0.95),
  replication = 1:2,
  stringsAsFactors = FALSE
)
jobs$dgp <- dgps$dgp[match(jobs$dgp_id, dgps$dgp_id)]
jobs$job_id <- sprintf("job_%03d", seq_len(nrow(jobs)))
jobs$tolerance_confidence <- 0.95
jobs$posterior_confidence <- 0.95
jobs$selected_q <- jobs$content + 0.02
jobs$selected_tilt <- ifelse(jobs$dgp_id == "normal", 0, 0.2)
jobs$selected_width <- 2 + seq_len(nrow(jobs)) / 10
jobs$selected_stationarity <- 1e-4
jobs$selected_ecm_converged <- TRUE
jobs$status <- "ok"
jobs$seed <- seq_len(nrow(jobs))
jobs$method_seed <- jobs$seed + 1000L
jobs$selection_reason <- "test"
jobs$scan_target_content <- jobs$content + 0.03
jobs$policy_screen <- 0.98
jobs$q_grid_size <- 2L
jobs$candidates_evaluated <- 2L
jobs$feasible_count <- 1L
jobs$selected_candidate_index <- 1L
jobs$original_width <- jobs$selected_width
jobs$original_content <- jobs$content
jobs$original_success <- TRUE
jobs$original_ecm_converged <- TRUE
jobs$original_final_stationarity <- jobs$selected_stationarity
jobs$stationarity_tolerance <- 1e-3

trace_blocks <- list()
for (ii in seq_len(nrow(jobs))) {
  iter <- 0:5
  trace_blocks[[ii]] <- data.frame(
    job_id = jobs$job_id[[ii]],
    dgp_id = jobs$dgp_id[[ii]],
    dgp = jobs$dgp[[ii]],
    n = jobs$n[[ii]],
    content = jobs$content[[ii]],
    tolerance_confidence = 0.95,
    posterior_confidence = 0.95,
    replication = jobs$replication[[ii]],
    iteration = iter,
    objective = rev(seq_along(iter)) + ii / 10,
    relative_objective_change = c(NA, 10^seq(-2, -9, length.out = 5)),
    relative_parameter_change = c(NA, 10^seq(-2, -8, length.out = 5)),
    stationarity = c(NA, 10^seq(-1, -4, length.out = 5)),
    minimum_absolute_residual_product = 10^seq(-2, -5, length.out = 6),
    backtracking_steps = c(0L, rep(0L, 5)),
    step_size = c(0, rep(1, 5)),
    root_swap_after_cycle = FALSE,
    precision_repairs = 0L,
    condition_number_root1 = 1,
    condition_number_root2 = 1,
    root1 = -1 - exp(-iter),
    root2 = 1 + exp(-iter),
    width = 2 + 2 * exp(-iter),
    candidate_selected = TRUE,
    target_content = jobs$selected_q[[ii]],
    mean_tilt = jobs$selected_tilt[[ii]],
    central_tilt = jobs$selected_tilt[[ii]],
    posterior_content_probability = 0.99,
    posterior_constraint_satisfied = TRUE,
    stringsAsFactors = FALSE
  )
}
traces <- do.call(rbind, trace_blocks)
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
      errored_jobs = 0L,
      selected_convergence_rate = 1,
      selected_stationarity_median = 1e-4,
      selected_stationarity_max = 1e-4,
      selected_width_median = stats::median(x$selected_width),
      selected_q_min = min(x$selected_q),
      selected_q_max = max(x$selected_q),
      selected_tilt_min = min(x$selected_tilt),
      selected_tilt_max = max(x$selected_tilt),
      median_candidates_evaluated = 2,
      median_feasible_count = 1,
      original_success_rate = 1,
      original_ecm_convergence_rate = 1,
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

out_pdf <- file.path(work_dir, "figures", "report.pdf")
status <- system2(
  "Rscript",
  c(script, paste0("--trace-dir=", work_dir),
    paste0("--output-pdf=", out_pdf)),
  stdout = TRUE,
  stderr = TRUE
)
if (!identical(attr(status, "status"), NULL)) {
  cat(status, sep = "\n")
  stop("MTI-ECM trace report generator failed.", call. = FALSE)
}
manifest <- file.path(work_dir, "figures", "mti_ecm_trace_report_manifest.csv")
stopifnot(file.exists(out_pdf), file.exists(manifest))
stopifnot(file.info(out_pdf)$size > 20000)
report_manifest <- utils::read.csv(manifest, stringsAsFactors = FALSE)
stopifnot(report_manifest$pages[[1L]] >= 15L)
cat("MTI-ECM trace convergence report test passed.\n")
