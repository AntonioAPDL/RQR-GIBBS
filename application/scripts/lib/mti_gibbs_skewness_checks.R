mgs_check <- function(cfg, context, directory, workers = 4L) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  old <- system2("git", c("--no-optional-locks", "show",
    paste0(cfg$historical_sampler_commit, ":application/scripts/lib/mti_gibbs_practical.R")), stdout = TRUE)
  mwd_require(is.null(attr(old, "status")), "Historical Gibbs source is missing.")
  original <- new.env(parent = globalenv()); eval(parse(text = old), envir = original)
  for (name in c("mgp_conditional", "mgp_slice_interval", "mgp_slice_update", "mgp_slice_fit", "mgp_summaries"))
    mwd_require(identical(deparse(original[[name]]), deparse(get(name))), "The original Gibbs kernel changed.")
  tasks <- context$design$tasks[context$design$tasks$replication == cfg$qualification_replication, , drop = FALSE]
  rows <- parallel::mclapply(seq_len(nrow(tasks)), function(i) {
    task <- tasks[i, , drop = FALSE]; input <- mgs_input(task, context, cfg); s <- input$selected
    initial <- mwd_initial(input$y, s$lower, s$upper, 4, 0)
    settings <- c(cfg$sampling, list(initial_midpoint_offsets_sd = initial$offsets,
      initial_halfwidth_multipliers = initial$radii))
    t <- data.frame(chain = 1L + (i - 1L) %% 4L, seed = as.integer(1750000000L + task$job_id),
      q = s$target_content, delta = s$mean_tilt, ecm_lower = s$lower, ecm_upper = s$upper)
    conditional <- mgp_conditional(input$y, s$upper, t$q, t$delta, precision = .25)
    x <- c(s$lower, conditional$mode, quantile(input$y, c(0, .25, .5, .75, 1)), -8, 8)
    direct <- mgp_objective(input$y, x, rep(s$upper, length(x)), t$q, t$delta, precision = .25) -
      mgp_objective(input$y, conditional$mode, s$upper, t$q, t$delta, precision = .25)
    error <- max(abs(conditional$energy(x) - direct))
    mwd_require(error <= 1e-8 * max(1, abs(direct)), "Conditional energy differs from the MTI loss.")
    start <- proc.time()[3]
    fit <- mgp_slice_fit(input$y, t, settings, n_burn = settings$n_burn, n_keep = settings$budgets[1])
    seconds <- unname(proc.time()[3] - start)
    mgs_validate_chain(list(task = t, fit = fit, sample_sha256 = input$sample_sha256),
      input$y, cfg, settings$budgets[1])
    data.frame(law_id = task$law_id, n = task$n, content = task$content,
      sample_sha256 = input$sample_sha256, conditional_energy_error = error,
      one_chain_seconds = seconds, object_bytes = as.numeric(object.size(fit)))
  }, mc.cores = min(workers, nrow(tasks)), mc.set.seed = FALSE)
  mwd_require(all(vapply(rows, is.data.frame, FALSE)), "Saved-target numerical checks failed.")
  rows <- data.table::rbindlist(rows)
  pma_write_csv(as.data.frame(rows), file.path(directory, "saved_target_checks.csv"))
  receipt <- list(passed = TRUE, cases = nrow(rows), sampler_unchanged = TRUE,
    assessment_results_produced = FALSE, inputs = mgfp_hashes(c(mgs_sources, cfg$configuration_path)),
    parent_identity = cfg$parent_identity, median_chain_seconds = median(rows$one_chain_seconds),
    mean_chain_seconds = mean(rows$one_chain_seconds), full_draws_per_chain = cfg$sampling$budgets[1],
    source_commit = mgfp_git("rev-parse", "HEAD"), time = mgfp_time())
  mgfp_json_write(receipt, file.path(directory, "prelaunch_checks.json"))
  receipt
}

mgs_qualify <- function(prepared, cfg, context, directory, workers) {
  tasks <- prepared$tasks[prepared$tasks$replication == cfg$qualification_replication, , drop = FALSE]
  mwd_require(nrow(tasks) == cfg$expected_cases, "Qualification must cover every added case.")
  mgs_run_jobs(tasks, cfg, context, directory, prepared$identity, min(workers, nrow(tasks)))
  values <- lapply(seq_len(nrow(tasks)), function(i) mgs_read(mgs_result_path(directory, tasks$job_id[i]),
    tasks[i, , drop = FALSE], prepared$identity, function(x) invisible(TRUE)))
  passed <- all(vapply(values, function(x) is.null(x$error) && all(x$parameters$screen_pass), FALSE))
  report <- list(passed = passed, datasets = length(values), official_results_reused = TRUE,
    numerically_adequate = sum(vapply(values, function(x) isTRUE(x$adequate), FALSE)),
    sampling_errors = sum(vapply(values, function(x) !is.null(x$error), FALSE)),
    mean_dataset_seconds = mean(vapply(values, `[[`, 0, "elapsed_seconds")),
    identity = prepared$identity, time = mgfp_time())
  mgfp_json_write(report, file.path(directory, "qualification.json"))
  mwd_require(passed, "Qualification found a sampler error or inadequate exploratory diagnostics; inspect saved results.")
  report
}
