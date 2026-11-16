mbx_candidate_comparison <- function(saved, replay, tolerance) {
  columns <- c("candidate_index", "target_content", "mean_tilt", "lower", "upper", "observed_count",
    "ecm_objective", "posterior_content_probability", "numerically_accurate", "posterior_constraint_satisfied")
  mwd_require(all(columns %in% intersect(names(saved), names(replay))) && nrow(saved) == nrow(replay),
    "Replay candidate definitions differ.")
  a <- saved[order(saved$candidate_index), columns]; b <- replay[order(replay$candidate_index), columns]
  mwd_require(identical(a$candidate_index, b$candidate_index), "Replay candidate indices differ.")
  numeric <- columns[vapply(a, is.numeric, FALSE)]
  difference <- vapply(numeric, function(name) max(abs(a[[name]]-b[[name]])), 0)
  flags <- setdiff(columns, numeric)
  mwd_require(all(is.finite(difference)) && all(difference <= tolerance) &&
    all(vapply(flags, function(name) identical(a[[name]], b[[name]]), FALSE)), "Replay candidate calculations differ.")
  data.frame(quantity = names(difference), maximum_difference = unname(difference))
}

mbx_context <- function(cfg) {
  mwd_require(!cfg$prior_tuning && !cfg$replace_original_results && cfg$confirmation$prior_sd == 2,
    "The closeout must preserve the fixed independent prior and existing results.")
  boundary <- mwd_json(cfg$boundary_config)
  completed <- file.path(mgfp_common(), cfg$completed_worktree, boundary$output_root)
  manifest <- mwd_json(file.path(completed, "manifest.json"))
  mwd_require(manifest$identity$source_commit == cfg$completed_commit &&
    mwd_json(file.path(completed, "controller_exit.json"))$exit_code == 0L,
    "The completed study identity or exit status differs.")
  old <- getwd(); on.exit(setwd(old))
  setwd(file.path(mgfp_common(), cfg$completed_worktree))
  mgfp_check_hashes(manifest$identity$programs); mgfp_check_hashes(manifest$identity$inputs)
  mgfp_check_hashes(manifest$identity$package$installed_files)
  frozen <- mwd_json(file.path(completed, "frozen_policy.json"))
  mgfp_check_hashes(frozen$target_policy); mgfp_check_hashes(frozen$interval_policy)
  setwd(old)
  boundary$output_root <- cfg$output_root
  context <- mbc_context(boundary)
  context$completed <- completed; context$boundary <- boundary
  context$completed_manifest <- manifest
  context
}

mbx_fit <- function(task, cfg, context, seed, comparators_first = TRUE) {
  set.seed(task$data_seed); y <- context$laws[[task$response_id]]$r(task$n)
  benchmarks <- if (comparators_first) mfp_comparators(y, task$content, task$tolerance_confidence) else NULL
  rule <- context$policy[context$policy$n == task$n & context$policy$content == task$content, ]
  mwd_require(nrow(rule) == 1L, "The inherited ECM rule is ambiguous.")
  fit <- mfp_fit_data(y, task$content, task$tolerance_confidence, task$retained_fraction,
    2, context$prior$simulation_reference, context$profile, context$dp, as.list(rule[1, ]),
    context$prior$numerical, context$prior$sampling, as.integer(seed), context$policy_diagnostics,
    sample_gibbs = FALSE)
  mwd_require(isTRUE(fit$returned), paste("ECM replay/confirmation requires review:", fit$reason))
  if (is.null(benchmarks)) benchmarks <- mfp_comparators(y, task$content, task$tolerance_confidence)
  selected <- fit$selected
  intervals <- data.table::rbindlist(list(data.frame(method = "ecm", returned = TRUE,
    lower = selected$lower, upper = selected$upper, width = selected$width),
    benchmarks[, c("method", "returned", "lower", "upper", "width")]))
  cdf <- context$laws[[task$response_id]]$p
  intervals[, `:=`(job_id = task$job_id, response_id = task$response_id, n = task$n,
    content = task$content, tolerance_confidence = task$tolerance_confidence,
    population_content = cdf(upper)-cdf(lower))]
  intervals[, attained := returned & population_content >= content]
  list(ok = TRUE, task = task, sample_sha256 = mwd_hash(y), sample_sd = sd(y),
    selected = selected, candidates = fit$candidates, q = fit$q, delta = fit$delta_reference,
    intervals = intervals, prior_sd = fit$prior$prior_sd, prior_uses_sample = fit$prior$data_adaptive_prior)
}

mbx_replay_tasks <- function(cfg, context) {
  pilot <- as.data.frame(data.table::fread(file.path(context$completed, "pilot/tasks.csv")))
  pilot <- pilot[pilot$response_id == "normal" & pilot$n == 500 & pilot$content == .95, ]
  old <- as.data.frame(data.table::fread(file.path(context$parent, "validation/tasks.csv")))
  old <- old[old$response_id == "normal" & old$n == 500 & old$content == .95 &
    old$replication <= cfg$replay$historical_samples, ]
  mwd_require(nrow(pilot) == cfg$replay$pilot_samples && nrow(old) == cfg$replay$historical_samples,
    "The preassigned replay sample panel differs.")
  pilot$source_stage <- "pilot"; old$source_stage <- "historical"
  pilot$source_job_id <- pilot$job_id; old$source_job_id <- old$job_id
  d <- as.data.frame(data.table::rbindlist(list(pilot, old), fill = TRUE))
  d$job_id <- seq_len(nrow(d)); d
}

mbx_replay <- function(task, cfg, context) {
  stage <- task$source_stage
  directory <- if (stage == "pilot") file.path(context$completed, "pilot") else file.path(context$parent, "validation")
  path <- file.path(directory, "fits", sprintf("fit_%06d.rds", task$source_job_id))
  receipt <- mwd_json(paste0(path, ".json"))
  mwd_require(receipt$sha256 == mwd_file_hash(path), "The saved replay anchor checksum differs.")
  saved <- readRDS(path); anchor <- if (stage == "pilot") saved$anchor else saved
  old_seed <- context$prior$stages$validation$chain_seed_base + 100*task$source_job_id
  replay <- mbx_fit(task, cfg, context, old_seed, comparators_first = TRUE)
  sample_hash <- if (is.null(saved$sample_sha256)) anchor$sample_sha256 else saved$sample_sha256
  mwd_require(sample_hash == replay$sample_sha256,
    "Replay sample identity differs.")
  checks <- mbx_candidate_comparison(anchor$candidates, replay$candidates, cfg$replay$endpoint_tolerance)
  gap <- max(abs(c(anchor$q-replay$q, anchor$delta_reference-replay$delta,
    anchor$selected$lower-replay$selected$lower, anchor$selected$upper-replay$selected$upper)))
  mwd_require(is.finite(gap) && gap <= cfg$replay$endpoint_tolerance, "Selected replay endpoints differ.")
  replay$comparison <- checks
  replay$comparison$source_stage <- stage; replay$comparison$job_id <- task$job_id
  replay$maximum_selected_difference <- gap
  replay
}

mbx_queue <- function(tasks, stage, cfg, context, identity, workers) {
  directory <- file.path(cfg$output_root, stage)
  dir.create(file.path(directory, "fits"), recursive = TRUE, showWarnings = FALSE)
  data.table::fwrite(tasks, file.path(directory, "tasks.csv"))
  id <- mwd_hash(list(identity, stage, tasks))
  manifest <- file.path(directory, "manifest.json")
  if (file.exists(manifest)) mwd_require(mwd_json(manifest)$identity == id, "Closeout stage identity changed.") else
    mgfp_json_write(list(identity = id, source = identity), manifest)
  paths <- file.path(directory, "fits", sprintf("fit_%06d.rds", tasks$job_id))
  present <- file.exists(paste0(paths, ".json"))
  mwd_require(!any(file.exists(paths) & !present), "Unreceipted closeout fit requires review.")
  for (i in which(present)) mwd_require(mbc_read_fit(paths[i], id, tasks[i, ])$ok,
    "A preserved failed fit requires review.")
  pending <- which(!present); completed <- sum(present)
  for (batch in split(pending, ceiling(seq_along(pending)/workers))) {
    mps_disk_check(directory, cfg$disk_reserve_gib)
    values <- parallel::mclapply(batch, function(i) {
      task <- tasks[i, ]
      value <- tryCatch(if (stage == "replay") mbx_replay(task, cfg, context) else
        mbx_fit(task, cfg, context, cfg$confirmation$engine_seed_base+100*task$job_id),
        error = function(e) list(ok = FALSE, task = task, reason = conditionMessage(e)))
      mbc_atomic_fit(value, paths[i], id, task); isTRUE(value$ok)
    }, mc.cores = workers, mc.set.seed = FALSE)
    completed <- completed + length(batch)
    mgfp_json_write(list(stage = stage, completed = completed, expected = nrow(tasks),
      passed = all(vapply(values, isTRUE, FALSE)), time = mgfp_time()), file.path(directory, "progress.json"))
    mwd_require(all(vapply(values, isTRUE, FALSE)), "Closeout batch requires review; all results are preserved.")
    cat(stage, completed, "/", nrow(tasks), "saved\n"); flush.console()
  }
  lapply(seq_len(nrow(tasks)), function(i) mbc_read_fit(paths[i], id, tasks[i, ]))
}

mbx_confirmation_tasks <- function(cfg, context) {
  s <- cfg$confirmation
  scan <- context$scan[context$scan$n == s$n & context$scan$content == s$content, ]
  mwd_require(nrow(scan) == 1L, "Missing confirmation scan reference.")
  data.frame(job_id = seq_len(s$replications), response_id = "normal", n = as.integer(s$n),
    content = s$content, tolerance_confidence = s$tolerance_confidence,
    retained_fraction = scan$retained_fraction, data_seed = s$data_seed_base+seq_len(s$replications),
    stage = "confirmation")
}

mbx_bounds <- function(rows) {
  d <- data.table::as.data.table(rows)
  z <- d[, .(attempts = .N, successes = sum(attained), returned = sum(returned),
    attainment = mean(attained), mean_width = mean(width),
    width_q025 = quantile(width, .025), width_q975 = quantile(width, .975)),
    by = .(response_id, n, content, tolerance_confidence, method)]
  z[, `:=`(lower_95 = ifelse(successes == 0, 0, qbeta(.025, successes, attempts-successes+1)),
    upper_95 = ifelse(successes == attempts, 1, qbeta(.975, successes+1, attempts-successes)))]
  z
}

mbx_collect <- function(cfg, context, replay, confirmation, identity) {
  dir.create(cfg$evidence_root, recursive = TRUE, showWarnings = FALSE)
  write <- function(x, name) mwd_write(x, file.path(cfg$evidence_root, name))
  write(data.table::rbindlist(lapply(replay, `[[`, "comparison")), "replay_differences.csv")
  values <- data.table::rbindlist(lapply(confirmation, `[[`, "intervals"))
  write(values, "confirmation_intervals.csv")
  confirmation_summary <- mbx_bounds(values)
  write(confirmation_summary, "confirmation_summary.csv")
  for (name in c("pilot/comparison_summary.csv", "pilot/interval_results.csv", "calibration_policy.csv"))
    write(data.table::fread(file.path(context$completed, name)), basename(name))
  cells <- data.table::fread(file.path(context$completed, "pilot/comparison_summary.csv"))
  g <- cells[method == "gibbs_calibrated", .(response_id, n, content, gibbs_width = mean_width)]
  paired <- merge(g, cells[method %in% c("ecm", "young_mathew", "wilks"),
    .(response_id, n, content, method, mean_width)], by = c("response_id", "n", "content"))
  overview <- paired[, .(cases = .N, shorter_mean_cases = sum(gibbs_width < mean_width),
    median_case_percent_width_difference = median(100*(gibbs_width/mean_width-1))), by = method]
  write(overview, "width_comparison.csv")
  stats <- lapply(c("development", "calibration", "pilot"), function(stage) {
    d <- data.table::fread(file.path(context$completed, stage, "posterior_summary.csv"))
    mwd_require(all(d$adequate), "Completed study numerical checks differ.")
    data.frame(stage = stage, datasets = data.table::uniqueN(d$job_id), targets = nrow(d),
      maximum_rhat = max(d$max_rhat), minimum_bulk_ess = min(d$min_bulk_ess), minimum_tail_ess = min(d$min_tail_ess))
  })
  write(do.call(rbind, stats), "numerical_summary.csv")
  summary <- list(source = identity, completed_study_commit = cfg$completed_commit,
    replay_datasets = length(replay), replay_maximum_selected_difference = max(vapply(replay,
      `[[`, 0, "maximum_selected_difference")), confirmation_datasets = length(confirmation),
    confirmation = as.data.frame(confirmation_summary), pilot = as.data.frame(cells[,
      .(minimum_attainment = min(attainment), maximum_attainment = max(attainment)), by = method]),
    width_comparison = as.data.frame(overview), prior_sd = 2, prior_uses_sample = FALSE,
    scope = "finite_panel_calibration_not_arbitrary_distribution_or_application_guarantee")
  mgfp_json_write(summary, file.path(cfg$evidence_root, "summary.json"))
  inputs <- c(file.path(context$completed, c("manifest.json", "frozen_policy.json", "calibration_policy.csv",
    "development_policy.csv", "pilot/comparison_summary.csv", "pilot/interval_results.csv")))
  evidence <- list.files(cfg$evidence_root, full.names = TRUE)
  evidence <- evidence[!basename(evidence) %in% c("provenance.json", "rendered.json")]
  mgfp_json_write(list(source = identity, inputs = mgfp_hashes(inputs), artifacts = mgfp_hashes(evidence)),
    file.path(cfg$evidence_root, "provenance.json"))
  invisible(summary)
}
