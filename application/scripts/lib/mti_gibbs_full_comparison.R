mgf_sources <- c("application/scripts/111_run_mti_gibbs_full_comparison.R",
  "application/scripts/112_launch_mti_gibbs_full_comparison.sh",
  "application/config/mti_gibbs_full_comparison_20261111.json", "Makefile",
  "application/tests/testthat/test-mti-gibbs-full-comparison.R",
  file.path("application/scripts/lib", c("pharma_tolerance_application.R",
    "pharma_mti_application.R", "mti_profile_revision.R", "mti_profile_review.R",
    "mti_final_reporting.R", "mti_ecm_recovery.R", "pinned_exdqlm_runtime.R",
    "mti_gibbs_comparison.R", "mti_gibbs_practical.R", "mti_gibbs_review.R",
    "mti_gibbs_full_comparison.R")))

mgf_json_write <- function(value, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(".full-gibbs-", tmpdir = dirname(path))
  on.exit(unlink(tmp))
  jsonlite::write_json(value, tmp, pretty = TRUE, auto_unbox = TRUE, digits = NA, null = "null")
  mpr_require(file.rename(tmp, path), "Cannot atomically publish a full-study JSON record.")
  invisible(path)
}

mgf_path <- function(directory, id, extension = FALSE) {
  file.path(directory, if (extension) "extensions" else "chains",
    sprintf("%04d", as.integer((id - 1L) %/% 1000L)), sprintf("chain_%06d.rds", id))
}
mgf_assessment_path <- function(directory, id) file.path(directory, "assessments",
  sprintf("%04d", as.integer((id - 1L) %/% 1000L)), sprintf("dataset_%06d.rds", id))

mgf_continue <- function(y, task, base, previous, count) {
  # Continuations restore both raw roots and RNG, without a new seed or burn-in.
  mpr_require(identical(previous$sampler, "intercept_marginal_slice_within_gibbs") &&
    !is.null(previous$last$rng_state), "Incompatible marginal continuation state.")
  assign(".Random.seed", previous$last$rng_state, envir = .GlobalEnv)
  a <- previous$last$beta_root1; b <- previous$last$beta_root2
  raw <- matrix(NA_real_, count, 2L)
  for (i in seq_len(count)) {
    a <- mgp_slice_update(a, mgp_conditional(y, b, task$q, task$delta,
      base$learning_rate, 1 / base$prior_variance))
    b <- mgp_slice_update(b, mgp_conditional(y, a, task$q, task$delta,
      base$learning_rate, 1 / base$prior_variance))
    raw[i, ] <- c(a, b)
  }
  list(draws = cbind(lower = pmin(raw[, 1], raw[, 2]), upper = pmax(raw[, 1], raw[, 2])),
    retained_objective = mgp_objective(y, raw[, 1], raw[, 2], task$q, task$delta,
      base$learning_rate, 1 / base$prior_variance),
    last = list(beta_root1 = a, beta_root2 = b, rng_state = .Random.seed),
    sampler = previous$sampler)
}

mgf_compact <- function(fit, task, y, identity, elapsed, checks, parent_sha256 = NULL) {
  indices <- unique(as.integer(round(seq(1, nrow(fit$draws), length.out =
    min(checks, nrow(fit$draws))))))
  list(format = "mti_gibbs_compact/1", task = task, draws = fit$draws, last = fit$last,
    sampler = fit$sampler, sample_sha256 = mgc_hash(y), analysis_identity = mgc_hash(identity),
    elapsed_seconds = elapsed, repair_count = 0L, passed = TRUE,
    objective_summary = c(mean = mean(fit$retained_objective),
      min = min(fit$retained_objective), max = max(fit$retained_objective)),
    objective_checks = list(indices = indices, values = fit$retained_objective[indices]),
    parent_sha256 = parent_sha256)
}

mgf_validate <- function(record, task, identity, count, y = NULL, base = NULL) {
  mgc_validate_record(record, task, list(n_mcmc = count))
  mpr_require(identical(record$format, "mti_gibbs_compact/1") &&
    identical(record$analysis_identity, mgc_hash(identity)) &&
    identical(record$sampler, "intercept_marginal_slice_within_gibbs") &&
    length(record$last$beta_root1) == 1L && length(record$last$beta_root2) == 1L &&
    all(is.finite(c(record$last$beta_root1, record$last$beta_root2))) &&
    is.integer(record$last$rng_state) && length(record$last$rng_state) > 2L,
    "Incompatible sampler identity or terminal state.")
  indices <- record$objective_checks$indices
  mpr_require(length(indices) > 0L && all(indices >= 1L & indices <= count) &&
    length(record$objective_checks$values) == length(indices) &&
    all(is.finite(record$objective_checks$values)), "Invalid objective check indices or values.")
  if (!is.null(y)) {
    mpr_require(identical(mgc_hash(y), record$sample_sha256), "Reconstructed sample differs.")
    expected <- mgp_objective(y, record$draws[indices, 1], record$draws[indices, 2],
      task$q, task$delta, base$learning_rate, 1 / base$prior_variance)
    mpr_require(max(abs(expected - record$objective_checks$values)) <=
      1e-8 * max(1, abs(expected)), "Direct objective check failed.")
  }
  invisible(TRUE)
}

mgf_write <- function(record, path, task, identity, count, y, base) {
  mgf_validate(record, task, identity, count, y, base)
  mgc_atomic_rds(record, path)
  mpr_require(identical(readRDS(path), record), "Compact record round trip differs.")
  mgf_json_write(list(passed = TRUE, task_sha256 = mgc_hash(task),
    analysis_identity = mgc_hash(identity), sha256 = mgc_file_hash(path),
    n_mcmc = count, parent_sha256 = record$parent_sha256, bytes = file.info(path)$size,
    elapsed_seconds = record$elapsed_seconds, finished_utc = mgc_time()), paste0(path, ".json"))
  unlink(paste0(path, ".error.json"))
  invisible(TRUE)
}

mgf_read <- function(path, task, identity, count, y = NULL, base = NULL) {
  if (!file.exists(paste0(path, ".json"))) return(NULL)
  receipt <- mgc_json(paste0(path, ".json"))
  mpr_require(file.exists(path) && isTRUE(receipt$passed) && receipt$n_mcmc == count &&
    identical(receipt$task_sha256, mgc_hash(task)) &&
    identical(receipt$analysis_identity, mgc_hash(identity)) &&
    identical(receipt$sha256, mgc_file_hash(path)), "Corrupt or incompatible compact completion record.")
  record <- readRDS(path)
  mgf_validate(record, task, identity, count, y, base)
  mpr_require(identical(receipt$parent_sha256, record$parent_sha256), "Continuation parent differs.")
  record
}

mgf_root <- function() dirname(normalizePath(pma_git("rev-parse", "--git-common-dir"), mustWork = TRUE))

mgf_output_guard <- function(directory) {
  ancestor <- directory
  while (!dir.exists(ancestor) && dirname(ancestor) != ancestor) ancestor <- dirname(ancestor)
  mpr_require(rqr_path_within(normalizePath(ancestor, mustWork = TRUE), mgf_root()) &&
    grepl("/application/outputs(/|$)", directory),
    "Campaign output must be in an RQR-owned application/outputs directory.")
  invisible(TRUE)
}

mgf_inputs <- function(cfg, base) {
  review <- mgc_json(cfg$review_config)
  paths <- c(cfg$base_config, cfg$practical_config, cfg$review_config,
    file.path(review$evidence_dir, c("provenance.json", "summary.json")))
  c(mgc_inputs(base), setNames(vapply(paths, mgc_file_hash, ""), paths))
}

mgf_identity <- function(cfg, base, jobs) {
  packages <- c("digest", "jsonlite", "posterior", "parallelly", "data.table")
  list(commit = pma_git("rev-parse", "HEAD"), settings_sha256 = mgc_hash(cfg),
    base_sha256 = mgc_hash(base), design_sha256 = mgc_hash(jobs),
    inputs_sha256 = mgc_hash(mgf_inputs(cfg, base)),
    sampler_sources_sha256 = mgc_hash(setNames(vapply(mgf_sources, mgc_file_hash, ""), mgf_sources)),
    reference_runtime_sha256 = mgc_runtime(mgc_json(cfg$practical_config)$parent_commit)$sha256,
    r_version = R.version.string,
    dependencies_sha256 = mgc_hash(setNames(vapply(packages,
      function(p) as.character(packageVersion(p)), ""), packages)), rng = paste(RNGkind(), collapse = ";"))
}

mgf_settings <- function(cfg, base, practical, jobs) {
  mpr_require(base$chains == 4L && base$n_burn == 2000L && base$n_mcmc == 4000L &&
    base$thin == 1L && base$learning_rate == 1 && base$prior_variance == 10000 &&
    cfg$maximum_extensions == 1L && cfg$extension_draws == 4000L &&
    identical(cfg$sampler, practical$sampler) && identical(cfg$target_selection, base$target_selection) &&
    identical(cfg$primary_interval, "posterior_mean_ordered_endpoints") &&
    !cfg$new_tolerance_guarantee && !cfg$automatic_article_replacement,
    "The full comparison changes the approved posterior or interval summary.")
  tasks <- jobs[!duplicated(jobs$dataset_id), ]
  mpr_require(nrow(jobs) == cfg$expected_chains && nrow(tasks) == cfg$expected_datasets &&
    sum(tasks$source == "simulation") == cfg$expected_simulation_datasets &&
    sum(tasks$source == "application") == cfg$expected_application_datasets &&
    all(table(jobs$dataset_id) == base$chains) && !anyDuplicated(jobs$seed),
    "The full comparison design is incomplete or duplicated.")
  invisible(TRUE)
}

mgf_space <- function(cfg, remaining, directory, available = mgc_free_bytes(directory),
                      free_inodes = NULL) {
  required <- as.double(remaining) * cfg$record_budget_bytes * (1 + cfg$disk_overhead_fraction) +
    (cfg$disk_reserve_gib + cfg$report_reserve_gib) * 1024^3
  mpr_require(available >= required, sprintf("Storage limit: need %.2f GiB; available %.2f GiB.",
    required / 1024^3, available / 1024^3))
  if (is.null(free_inodes)) {
    path <- directory
    while (!dir.exists(path)) path <- dirname(path)
    out <- system2("df", c("-Pi", shQuote(path)), stdout = TRUE)
    free_inodes <- as.numeric(strsplit(trimws(tail(out, 1)), " +")[[1]][4])
  }
  mpr_require(is.finite(free_inodes) && free_inodes >= ceiling(2.5 * remaining) + cfg$inode_reserve,
    "Insufficient free inodes for the remaining chain records.")
  invisible(list(required_gib = required / 1024^3, available_gib = available / 1024^3,
    free_inodes = free_inodes))
}

mgf_workers <- function(cfg, base, requested) {
  workers <- min(as.integer(requested), cfg$maximum_workers, as.integer(parallelly::availableCores()))
  mpr_require(is.finite(workers) && workers >= 1L, "No eligible fit workers.")
  mgc_resources(base, workers)
  workers
}

mgf_status <- function(directory, state, ...) {
  mgf_json_write(c(list(status = state, pid = Sys.getpid(),
    pid_token = mgc_pid_token(Sys.getpid()), time_utc = mgc_time()), list(...)),
    file.path(directory, "campaign_status.json"))
}

mgf_qualification_gate <- function(qualification, identity) {
  mpr_require(isTRUE(qualification$passed) &&
    identical(qualification$analysis_identity, mgc_hash(identity)),
    "The computational qualification has not passed for this frozen study.")
  invisible(TRUE)
}

mgf_qualify <- function(cfg, base, practical, identity, directory, workers) {
  path <- file.path(directory, "qualification.json")
  if (file.exists(path)) {
    qualification <- mgc_json(path)
    mgf_qualification_gate(qualification, identity)
    pma_verify_hash(file.path(directory, "bridge.csv"), qualification$bridge_sha256)
    return(invisible(qualification))
  }
  mgf_status(directory, "qualifying", workers = workers)
  commands <- list(c("application/scripts/110_review_mti_gibbs.R", "--mode=verify"),
    c("application/scripts/104_recover_mti_ecm_article.R", "--mode=verify"))
  for (command in commands) mpr_require(system2(file.path(R.home("bin"), "Rscript"), command) == 0L,
    "Existing scientific evidence verification failed.")
  mpr_require(system2("make", c("test-mti-gibbs-practical",
    "test-mti-gibbs-review", "test-mti-gibbs-full")) == 0L, "Full-study tests failed.")
  review <- mgc_json(cfg$review_config)
  source <- file.path(mgf_root(), review$practical_directory)
  manifest <- mgc_json(file.path(source, "manifest.json"))
  mpr_require(identical(manifest$identity$commit, review$practical_commit) &&
    identical(manifest$identity$base_sha256, mgc_hash(base)) &&
    identical(manifest$identity$settings_sha256, mgc_hash(practical)) &&
    identical(manifest$identity$continuation_runtime_sha256, identity$reference_runtime_sha256),
    "The bridge reference has a different target or source.")
  jobs <- mgc_jobs(file.path(source, "slice"))
  mpr_require(identical(mgc_hash(jobs), manifest$identity$design_sha256), "Bridge design changed.")
  examples <- read.csv(file.path(source, "diagnostic_examples.csv"))
  ids <- examples$dataset_id
  mpr_require(length(ids) == cfg$bridge_datasets && !anyDuplicated(ids), "Unexpected bridge examples.")
  jobs <- jobs[jobs$dataset_id %in% ids, ]
  context <- mgc_context(base)
  rows <- parallel::mclapply(seq_len(nrow(jobs)), function(i) {
    t <- jobs[i, , drop = FALSE]; y <- mgc_sample(t, context)
    original <- mgp_read(file.path(source, "slice"), t, list(n_mcmc = base$n_mcmc))
    receipt <- mgc_receipt(mgc_chain_path(file.path(source, "slice"), t$job_id),
      t, list(n_mcmc = base$n_mcmc))
    mpr_require(!is.null(original) && identical(original$sampler, cfg$sampler) &&
      identical(receipt$analysis_identity, mgc_hash(manifest$identity)), "Missing or incompatible bridge reference chain.")
    begun <- proc.time()[[3L]]
    fit <- mgp_slice_fit(y, t, base)
    mpr_require(identical(fit$draws, original$draws) && identical(fit$last, original$last),
      "The new wrapper differs from the frozen marginal chain.")
    value <- mgf_compact(fit, t, y, identity, proc.time()[[3L]] - begun, cfg$objective_checks)
    dest <- mgf_path(file.path(directory, "bridge"), t$job_id)
    mgf_write(value, dest, t, identity, base$n_mcmc, y, base)
    read <- mgf_read(dest, t, identity, base$n_mcmc, y, base)
    mpr_require(identical(read$draws, original$draws) && identical(read$last, original$last),
      "Compact bridge record differs.")
    data.frame(dataset_id = t$dataset_id, job_id = t$job_id, chain = t$chain,
      exact_draws_and_state = TRUE, bytes = file.info(dest)$size,
      elapsed_seconds = value$elapsed_seconds, reference_sha256 =
        mgc_file_hash(mgc_chain_path(file.path(source, "slice"), t$job_id)))
  }, mc.cores = workers, mc.set.seed = FALSE)
  mpr_require(!any(vapply(rows, inherits, logical(1), "try-error")), "Integration bridge failed.")
  bridge <- do.call(rbind, rows)
  mpr_require(nrow(bridge) == cfg$bridge_datasets * base$chains &&
    all(bridge$exact_draws_and_state) && max(bridge$bytes) <= cfg$record_budget_bytes,
    "Bridge incomplete or compact record budget underestimated.")
  pma_write_csv(bridge, file.path(directory, "bridge.csv"))
  # Exercise the final reporting path on the replay subset before the full run.
  bridge_directory <- file.path(directory, "bridge")
  mgc_atomic_rds(jobs, file.path(bridge_directory, "jobs.rds"))
  mgf_json_write(list(identity = identity, role = "integration_bridge_only",
    bridge_jobs_sha256 = mgc_hash(jobs)), file.path(bridge_directory, "manifest.json"))
  pma_write_csv(bridge, file.path(bridge_directory, "bridge.csv"))
  checked <- mgf_assess(jobs, cfg, base, practical, context, identity, bridge_directory, workers)
  mpr_require(all(checked$datasets$preferred_pass), "Integration bridge diagnostics differ from the completed assessment.")
  pma_write_csv(checked$datasets, file.path(bridge_directory, "dataset_summary.csv"))
  pma_write_csv(checked$parameters, file.path(bridge_directory, "parameter_summary.csv"))
  bridge_cfg <- cfg; bridge_cfg$expected_datasets <- cfg$bridge_datasets
  mgf_report(bridge_cfg, base, bridge_directory)
  qualification <- list(passed = TRUE, analysis_identity = mgc_hash(identity),
    source_reference_commit = review$practical_commit, bridge_chains = nrow(bridge),
    bridge_sha256 = mgc_file_hash(file.path(directory, "bridge.csv")),
    report_check_sha256 = mgc_file_hash(file.path(bridge_directory, "report_provenance.json")),
    evidence_sha256 = mgc_file_hash(file.path(review$evidence_dir, "provenance.json")),
    maximum_record_bytes = max(bridge$bytes), completed_utc = mgc_time())
  mgf_json_write(qualification, path)
  mgf_qualification_gate(qualification, identity)
  invisible(qualification)
}

mgf_chain <- function(t, cfg, base, context, identity, directory, extension = FALSE) {
  path <- mgf_path(directory, t$job_id, extension)
  tryCatch({
    y <- mgc_sample(t, context)
    count <- if (extension) cfg$extension_draws else base$n_mcmc
    existing <- mgf_read(path, t, identity, count, y, base)
    if (!is.null(existing)) return(TRUE)
    if (file.exists(path)) {
      mpr_require(file.rename(path, paste0(path, ".orphan-", Sys.getpid())),
        "Cannot preserve an unreceipted record.")
    }
    parent_sha256 <- NULL; begun <- proc.time()[[3L]]
    if (extension) {
      parent_path <- mgf_path(directory, t$job_id)
      parent <- mgf_read(parent_path, t, identity, base$n_mcmc, y, base)
      mpr_require(!is.null(parent), "Missing continuation parent.")
      parent_sha256 <- mgc_file_hash(parent_path)
      fit <- mgf_continue(y, t, base, parent, count)
    } else fit <- mgp_slice_fit(y, t, base)
    record <- mgf_compact(fit, t, y, identity, proc.time()[[3L]] - begun,
      cfg$objective_checks, parent_sha256)
    mgf_write(record, path, t, identity, count, y, base)
    TRUE
  }, error = function(e) {
    mgf_json_write(list(error = conditionMessage(e), task = t,
      analysis_identity = mgc_hash(identity), time_utc = mgc_time()), paste0(path, ".error.json"))
    FALSE
  })
}

mgf_queue <- function(jobs, cfg, base, context, identity, directory, workers, extension = FALSE) {
  mpr_require(!file.exists(file.path(directory, "STOP_REQUESTED")), "Graceful stop requested; remove the request explicitly before resume.")
  count <- if (extension) cfg$extension_draws else base$n_mcmc
  paths <- vapply(jobs$job_id, function(id) mgf_path(directory, id, extension), "")
  done <- file.exists(paste0(paths, ".json"))
  for (i in which(done)) mgf_read(paths[i], jobs[i, , drop = FALSE], identity, count)
  pending <- which(!done)
  pending <- pending[order(jobs$replication[pending], jobs$source[pending] == "application",
    -jobs$n[pending], jobs$dataset_id[pending], jobs$chain[pending])]
  batches <- split(pending, ceiling(seq_along(pending) / cfg$batch_chains))
  started <- Sys.time(); newly_done <- 0L
  for (indices in batches) {
    mpr_require(!file.exists(file.path(directory, "STOP_REQUESTED")), "Graceful stop requested; resume preserves completed work.")
    current_workers <- mgf_workers(cfg, base, workers)
    mgf_space(cfg, length(pending), directory)
    result <- parallel::mclapply(indices, function(i) mgf_chain(jobs[i, , drop = FALSE],
      cfg, base, context, identity, directory, extension), mc.cores = current_workers,
      mc.preschedule = FALSE, mc.set.seed = FALSE)
    passed <- vapply(result, isTRUE, logical(1))
    done[indices[passed]] <- TRUE; newly_done <- newly_done + sum(passed)
    pending <- setdiff(pending, indices[passed])
    elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
    progress <- list(stage = if (extension) "extensions" else "base",
      expected_chains = nrow(jobs), completed_chains = sum(done), remaining_chains = sum(!done),
      expected_datasets = nrow(jobs) / base$chains,
      completed_datasets = sum(colSums(matrix(done, nrow = base$chains)) == base$chains),
      workers = current_workers, elapsed_seconds = elapsed,
      estimated_remaining_hours = if (newly_done) elapsed * sum(!done) / newly_done / 3600 else NA_real_,
      time_utc = mgc_time())
    mgf_json_write(progress, file.path(directory, "progress.json"))
    mgf_json_write(progress, file.path(directory,
      if (extension) "extension_progress.json" else "base_progress.json"))
    message(sprintf("[%s] %s: %d / %d chains complete.", mgc_time(),
      if (extension) "extensions" else "base", sum(done), nrow(jobs)))
    mpr_require(all(passed), "A worker failed; completed records and error details are preserved for resume.")
  }
  invisible(sum(done))
}

mgf_assess_one <- function(s, cfg, base, practical, context, identity, directory, use_extensions) {
  t <- s[1L, ]; y <- mgc_sample(t, context)
  paths <- vapply(s$job_id, function(id) mgf_path(directory, id), "")
  extra_paths <- vapply(s$job_id, function(id) mgf_path(directory, id, TRUE), "")
  cached_path <- mgf_assessment_path(directory, t$dataset_id)
  if (use_extensions && !any(file.exists(paste0(extra_paths, ".json"))) && file.exists(cached_path)) {
    cached <- readRDS(cached_path)
    mpr_require(identical(cached$analysis_identity, mgc_hash(identity)) &&
      identical(cached$chain_sha256, vapply(paths, mgc_file_hash, "")), "Cached assessment identity changed.")
    return(cached$result)
  }
  fits <- lapply(seq_len(nrow(s)), function(i)
    mgf_read(paths[i], s[i, , drop = FALSE], identity, base$n_mcmc, y, base))
  if (any(vapply(fits, is.null, logical(1)))) return(NULL)
  extended <- FALSE
  if (use_extensions) {
    extra <- lapply(seq_len(nrow(s)), function(i) mgf_read(mgf_path(directory, s$job_id[i], TRUE),
      s[i, , drop = FALSE], identity, cfg$extension_draws, y, base))
    mpr_require(all(vapply(extra, is.null, logical(1))) || all(!vapply(extra, is.null, logical(1))),
      "Incomplete continuation chain set.")
    if (all(!vapply(extra, is.null, logical(1)))) {
      for (i in seq_along(extra)) mpr_require(identical(extra[[i]]$parent_sha256,
        mgc_file_hash(paths[i])), "Extension parent digest changed.")
      fits <- lapply(seq_along(fits), function(i)
        list(draws = rbind(fits[[i]]$draws, extra[[i]]$draws)))
      extended <- TRUE
    }
  }
  d <- mgp_summaries(fits, t, practical$policy)
  primary <- d[d$variable %in% c("lower", "upper", "width"), ]
  lo <- d[d$variable == "lower", ]; hi <- d[d$variable == "upper", ]
  width <- d[d$variable == "width", ]
  mu <- c(lo$mean, hi$mean); med <- c(lo$median, hi$median)
  evaluate <- function(endpoints) {
    if (t$source == "simulation") {
      law <- context$laws[[t$response_id]]
      return(as.numeric(law$p(endpoints[2]) - law$p(endpoints[1])))
    }
    training <- context$training_ids[[paste(t$split_scheme, t$split_id, sep = ":")]]
    heldout <- context$observed[!context$observed$.analysis_row %in% training, t$response_id]
    mpr_require(length(heldout) > 0L && all(is.finite(heldout)), "Invalid held-out observations.")
    mean(heldout >= endpoints[1] & heldout <= endpoints[2])
  }
  geometry <- mgr_geometry(y, t$q, t$delta)
  result <- list(parameters = d, dataset = data.frame(t[, c("dataset_id", "case_id", "source",
    "response_id", "n", "content", "tolerance_confidence", "replication", "dataset_sha256",
    "q", "delta", "ecm_lower", "ecm_upper", "split_scheme", "split_id")], geometry,
    extended = extended, retained_per_chain = nrow(fits[[1L]]$draws),
    mean_lower = mu[1], mean_upper = mu[2], mean_width = diff(mu),
    mean_content = evaluate(mu), mean_attained = evaluate(mu) >= t$content,
    median_lower = med[1], median_upper = med[2], median_interval_width = diff(med),
    median_content = evaluate(med), posterior_width_q025 = width$lower_025,
    posterior_width_median = width$median, posterior_width_q975 = width$upper_975,
    screen_pass = all(primary$screen_pass), preferred_pass = all(primary$preferred_pass),
    midpoint_screen_pass = d$screen_pass[d$variable == "midpoint"],
    all_primary_precision = all((primary$mean_precision & primary$median_precision & primary$interval_precision) %in% TRUE),
    maximum_rhat = max(d$rhat), minimum_bulk_ess = min(d$ess_bulk), minimum_tail_ess = min(d$ess_tail)))
  mgc_atomic_rds(list(analysis_identity = mgc_hash(identity), use_extensions = use_extensions,
    chain_sha256 = vapply(paths, mgc_file_hash, ""), result = result),
    mgf_assessment_path(directory, t$dataset_id))
  result
}

mgf_assess <- function(jobs, cfg, base, practical, context, identity, directory, workers,
                       use_extensions = FALSE) {
  groups <- split(seq_len(nrow(jobs)), jobs$dataset_id)
  chunks <- split(groups, ceiling(seq_along(groups) / 200L))
  rows <- list(); completed <- 0L
  for (chunk in chunks) {
    batch <- parallel::mclapply(chunk, function(i) mgf_assess_one(jobs[i, , drop = FALSE], cfg,
      base, practical, context, identity, directory, use_extensions),
      mc.cores = min(workers, cfg$assessment_workers), mc.set.seed = FALSE)
    mpr_require(!any(vapply(batch, inherits, logical(1), "try-error")), "A posterior assessment failed.")
    rows <- c(rows, Filter(Negate(is.null), batch)); completed <- completed + length(chunk)
    if (completed %% 2000L == 0L) message(sprintf("Assessed %d / %d datasets.", completed, length(groups)))
  }
  mpr_require(length(rows) == length(groups), "Full assessment has missing chain sets.")
  datasets <- do.call(rbind, lapply(rows, "[[", "dataset"))
  parameters <- do.call(rbind, lapply(rows, "[[", "parameters"))
  pma_write_csv(datasets, file.path(directory, if (use_extensions) "dataset_summary.csv" else "base_dataset_summary.csv"))
  pma_write_csv(parameters, file.path(directory, if (use_extensions) "parameter_summary.csv" else "base_parameter_summary.csv"))
  list(datasets = datasets, parameters = parameters)
}

mgf_cell_summary <- function(rows) {
  groups <- split(seq_len(nrow(rows)), interaction(rows$source, rows$response_id,
    rows$n, rows$content, rows$tolerance_confidence, rows$method, rows$split_scheme, drop = TRUE))
  do.call(rbind, lapply(groups, function(i) {
    z <- rows[i, ]; success <- sum(z$attained, na.rm = TRUE)
    ci <- if (z$source[1] == "simulation") binom.test(success, nrow(z))$conf.int else c(NA_real_, NA_real_)
    w <- z$width[is.finite(z$width)]
    data.frame(source = z$source[1], response_id = z$response_id[1], n = z$n[1],
      content = z$content[1], tolerance_confidence = z$tolerance_confidence[1], method = z$method[1],
      split_scheme = z$split_scheme[1],
      datasets = nrow(z), available = sum(z$available), attained = success,
      attainment_rate = success / nrow(z), lower_95 = ci[1], upper_95 = ci[2],
      width_mean = if (length(w)) mean(w) else NA_real_,
      width_q025 = if (length(w)) unname(quantile(w, .025)) else NA_real_,
      width_q975 = if (length(w)) unname(quantile(w, .975)) else NA_real_)
  }))
}

mgf_report <- function(cfg, base, directory) {
  d <- read.csv(file.path(directory, "dataset_summary.csv"))
  p <- read.csv(file.path(directory, "parameter_summary.csv"))
  mpr_require(nrow(d) == cfg$expected_datasets && !anyDuplicated(d$dataset_id),
    "Final report needs all full-study datasets.")
  article <- mre_config(base$ecm_article_config)
  frozen <- mgc_json(file.path(article$evidence_dir, "provenance.json"))
  pma_verify_hash(file.path(article$cache_dir, "results.csv"), frozen$results_sha256)
  competitors <- read.csv(file.path(article$cache_dir, "results.csv"))
  sim <- d[d$source == "simulation", ]
  rows <- data.frame(d[, c("dataset_id", "case_id", "source", "response_id", "n", "content",
    "tolerance_confidence", "replication", "split_scheme")], method = "mti_gibbs_mean",
    lower = d$mean_lower, upper = d$mean_upper, width = d$mean_width,
    observed_content = d$mean_content, attained = d$mean_attained, available = TRUE)
  key <- paste0("sim_", competitors$cell_index, "_", competitors$replication)
  paired <- sim[, c("dataset_id", "case_id", "response_id", "n", "content", "replication",
    "mean_width", "mean_content", "mean_attained", "screen_pass", "preferred_pass")]
  for (method in unique(competitors$method)) {
    z <- competitors[competitors$method == method, ]; k <- key[competitors$method == method]
    z <- z[match(sim$case_id, k), ]
    mpr_require(nrow(z) == nrow(sim) && !anyNA(z$dataset_sha256) &&
      identical(as.character(z$dataset_sha256), as.character(sim$dataset_sha256)),
      "Comparator observations differ from Gibbs observations.")
    rows <- rbind(rows, data.frame(sim[, c("dataset_id", "case_id", "source", "response_id", "n",
      "content", "tolerance_confidence", "replication", "split_scheme")], method = method, lower = z$lower,
      upper = z$upper, width = z$width, observed_content = z$true_content,
      attained = z$content_attained & z$returned, available = z$returned))
    paired[[paste0(method, "_width")]] <- z$width
    paired[[paste0(method, "_attained")]] <- z$content_attained
    paired[[paste0("width_difference_", method)]] <- sim$mean_width - z$width
  }
  app <- pma_read_config(base$application_config)
  old_app <- pta_read_csv(file.path(app$paths$canonical_dir, "results.csv"))
  ad <- d[d$source == "application", ]
  for (method in unique(old_app$method_id)) {
    z <- old_app[old_app$method_id == method, ]
    key <- paste(z$response_id, z$split_scheme, z$split_id, sep = ":")
    z <- z[match(paste(ad$response_id, ad$split_scheme, ad$split_id, sep = ":"), key), ]
    mpr_require(!anyNA(z$train_n) && all(z$train_n == ad$n), "Application comparator partition differs.")
    rows <- rbind(rows, data.frame(ad[, c("dataset_id", "case_id", "source", "response_id", "n",
      "content", "tolerance_confidence", "replication", "split_scheme")], method = method,
      lower = z$lower, upper = z$upper, width = z$width,
      observed_content = z$heldout_empirical_content, attained = z$heldout_attains_content,
      available = z$interval_returned))
  }
  summary <- mgf_cell_summary(rows)
  pma_write_csv(rows, file.path(directory, "interval_comparison.csv"))
  pma_write_csv(paired, file.path(directory, "paired_simulation_comparison.csv"))
  pma_write_csv(summary, file.path(directory, "comparison_summary.csv"))
  pdf <- file.path(directory, "gibbs_ecm_comparison.pdf")
  grDevices::pdf(pdf, width = 9, height = 7)
  device <- grDevices::dev.cur()
  on.exit(if (device %in% grDevices::dev.list()) grDevices::dev.off(device), add = TRUE)
  groups <- split(sim, interaction(sim$response_id, sim$n, sim$content, drop = TRUE))
  labels <- c(normal = "Normal", student_t3 = "Student t (3 df)", exponential = "Exponential",
    asym_laplace_tau010 = "Asymmetric Laplace", two_piece_normal_1_12 = "Two-piece Normal",
    beta18 = "Beta(1,8)", gamma05 = "Gamma(0.5,1)", lognormal_hard = "Lognormal")
  for (z in groups) {
    par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3.5, 1), cex = .85)
    width <- z$ecm_upper - z$ecm_lower
    plot(width, z$mean_width, log = "xy", pch = 16, cex = .35, col = "#0072B260",
      xlab = "ECM interval width", ylab = "Posterior-mean interval width",
      main = sprintf("%s; n = %d, c = %.2f", labels[z$response_id[1]], z$n[1], z$content[1]))
    abline(0, 1, col = "black", lty = 2)
    hist(z$mean_content, breaks = 30, xlim = range(c(z$mean_content, z$content)),
      xlab = "Population content of posterior-mean interval", main = "Repeated-sampling content",
      col = "#56B4E9", border = "white")
    abline(v = z$content[1], col = "#D55E00", lty = 2, lwd = 2)
    mtext(sprintf("%d datasets; %d pass preferred diagnostics", nrow(z), sum(z$preferred_pass)),
      side = 3, line = .3, cex = .75)
  }
  for (response in unique(ad$response_id)) {
    z <- ad[ad$response_id == response, ]
    par(mfrow = c(1, 2), mar = c(6, 4, 3, 1), cex = .85)
    boxplot(log10(mean_width) ~ split_scheme, data = z, las = 2,
      ylab = "Log10 interval width", xlab = "", main = paste(response, "partition sensitivity"))
    boxplot(mean_content ~ split_scheme, data = z, las = 2,
      ylab = "Held-out empirical content", xlab = "", main = "Overlapping historical partitions")
    abline(h = z$content[1], col = "#D55E00", lty = 2)
  }
  examples <- read.csv(file.path(directory, "bridge.csv"))
  jobs <- mgc_jobs(directory); manifest <- mgc_json(file.path(directory, "manifest.json"))
  for (id in unique(examples$dataset_id)) {
    s <- jobs[jobs$dataset_id == id, ]; t <- s[1, ]
    fits <- lapply(seq_len(nrow(s)), function(i) {
      value <- mgf_read(mgf_path(directory, s$job_id[i]), s[i, , drop = FALSE], manifest$identity, base$n_mcmc)
      extra <- mgf_read(mgf_path(directory, s$job_id[i], TRUE), s[i, , drop = FALSE],
        manifest$identity, cfg$extension_draws)
      if (!is.null(extra)) value$draws <- rbind(value$draws, extra$draws)
      value
    })
    draws <- mgp_array(fits)
    par(mfrow = c(2, 2), mar = c(4, 4, 3, 1), cex = .85)
    for (v in dimnames(draws)[[3]]) {
      matplot(draws[, , v], type = "l", lty = 1, col = c("#0072B2", "#D55E00", "#009E73", "#CC79A7"),
        xlab = "Retained iteration", ylab = v, main = paste(t$case_id, v))
    }
    par(mfrow = c(2, 2))
    for (v in dimnames(draws)[[3]]) {
      bins <- 20L; values <- draws[, , v]; ranks <- matrix(rank(values), nrow(values))
      cuts <- seq(0, length(values), length.out = bins + 1L)
      counts <- sapply(seq_len(ncol(values)), function(j) hist(ranks[, j], breaks = cuts, plot = FALSE)$counts)
      matplot(seq_len(bins), counts, type = "l", lty = 1,
        col = c("#0072B2", "#D55E00", "#009E73", "#CC79A7"),
        xlab = "Pooled rank bin", ylab = "Draw count", main = paste(t$case_id, v))
    }
  }
  context <- mgc_context(base)
  references <- lapply(unique(examples$dataset_id), function(id) {
    t <- jobs[jobs$dataset_id == id, ][1, ]; y <- mgc_sample(t, context)
    tryCatch({
      fit <- rqrgibbs::rqr_mti_intercept_fit(y, t$q, t$delta,
        learning_rate = base$learning_rate, prior_precision = 1 / base$prior_variance, backend = "cpp")
      data.frame(case_id = t$case_id, resolved = isTRUE(fit$converged),
        reference_lower = fit$lower, reference_upper = fit$upper, reference_width = fit$width,
        reference_relative_objective_gap = fit$relative_objective_gap,
        ecm_objective = mgp_objective(y, t$ecm_lower, t$ecm_upper, t$q, t$delta,
          base$learning_rate, 1 / base$prior_variance), reference_objective = fit$objective, error = "")
    }, error = function(e) data.frame(case_id = t$case_id, resolved = FALSE,
      reference_lower = NA_real_, reference_upper = NA_real_, reference_width = NA_real_,
      reference_relative_objective_gap = NA_real_, ecm_objective = NA_real_,
      reference_objective = NA_real_, error = conditionMessage(e)))
  })
  pma_write_csv(do.call(rbind, references), file.path(directory, "numerical_reference_examples.csv"))
  result <- list(datasets = nrow(d), simulation_datasets = nrow(sim),
    application_datasets = nrow(ad), preferred_datasets = sum(d$preferred_pass),
    practical_datasets = sum(d$screen_pass), unresolved_datasets = sum(!d$screen_pass),
    all_primary_precision_datasets = sum(d$all_primary_precision), extended_datasets = sum(d$extended),
    maximum_rhat = max(p$rhat), minimum_bulk_ess = min(p$ess_bulk),
    minimum_tail_ess = min(p$ess_tail), new_tolerance_guarantee = FALSE,
    article_replaced = FALSE, completed_utc = mgc_time())
  mgf_json_write(result, file.path(directory, "final_summary.json"))
  grDevices::dev.off(device)
  paths <- file.path(directory, c("dataset_summary.csv", "parameter_summary.csv",
    "interval_comparison.csv", "paired_simulation_comparison.csv", "comparison_summary.csv",
    "gibbs_ecm_comparison.pdf", "numerical_reference_examples.csv", "final_summary.json"))
  mgf_json_write(list(source_commit = manifest$identity$commit,
    manifest_sha256 = mgc_file_hash(file.path(directory, "manifest.json")),
    artifacts = setNames(vapply(paths, mgc_file_hash, ""), basename(paths)),
    created_utc = mgc_time()), file.path(directory, "report_provenance.json"))
  result
}

mgf_campaign <- function(cfg, base, practical, directory, workers, resume = FALSE) {
  mpr_require(mpr_git_clean(pma_git("status", "--porcelain")), "Commit the full-study source before launch.")
  mgf_output_guard(directory)
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  lock <- mgc_lock(directory, recover = resume)
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  external <- Sys.getenv("EXDQLM_RQR_REPO",
    "/data/muscat_data/jaguir26/exdqlm__wt__qdesn_0p4p0_integration")
  before <- rqr_capture_external_checkout(external)
  on.exit(rqr_assert_external_checkout_unchanged(before), add = TRUE)
  mpr_require(identical(before$commit, base$exdqlm_reference_commit), "Protected reference commit differs.")
  jobs <- mgc_design(base, "full"); mgf_settings(cfg, base, practical, jobs)
  old <- mgc_jobs(file.path(mgf_root(), mgc_json(cfg$review_config)$practical_directory, "slice"))
  mpr_require(!any(jobs$seed %in% old$seed), "Full seeds overlap the practical study.")
  diagnostic <- mgc_design(base, "diagnostic"); benchmark <- mgc_design(base, "benchmark")
  mpr_require(!any(jobs$seed %in% c(diagnostic$seed, benchmark$seed)), "Full seeds overlap previous stages.")
  identity <- mgf_identity(cfg, base, jobs)
  reference_runtime <- mgc_runtime(practical$parent_commit)
  path <- file.path(directory, "manifest.json")
  if (file.exists(path)) {
    manifest <- mgc_json(path)
    mpr_require(resume && mgc_match_identity(manifest$identity, identity) &&
      identical(mgc_hash(mgc_jobs(directory)), identity$design_sha256), "Resume changes the frozen study.")
    rqr_assert_external_checkout_unchanged(manifest$external_reference)
  } else {
    mpr_require(!resume, "No existing campaign is available to resume.")
    mgc_atomic_rds(jobs, file.path(directory, "jobs.rds"))
    pma_write_csv(jobs[!duplicated(jobs$dataset_id), ], file.path(directory, "design.csv"))
    manifest <- list(identity = identity, configuration = cfg, base_configuration = base,
      sampler = cfg$sampler, expected_datasets = cfg$expected_datasets,
      expected_chains = cfg$expected_chains, external_reference = before,
      reference_package_runtime = reference_runtime, inputs = mgf_inputs(cfg, base),
      resources = mgc_resources(base, workers), session = capture.output(sessionInfo()),
      created_utc = mgc_time(), new_tolerance_guarantee = FALSE, article_replaced = FALSE)
    mgf_json_write(manifest, path)
  }
  mgf_qualify(cfg, base, practical, identity, directory, workers)
  rqr_assert_external_checkout_unchanged(before)
  mgf_space(cfg, cfg$expected_chains - sum(file.exists(vapply(jobs$job_id,
    function(id) paste0(mgf_path(directory, id), ".json"), ""))), directory)
  context <- mgc_context(base)
  mgf_status(directory, "base_running", commit = identity$commit, workers = workers,
    expected_datasets = cfg$expected_datasets, expected_base_chains = cfg$expected_chains)
  mgf_queue(jobs, cfg, base, context, identity, directory, workers)
  mgf_status(directory, "assessing_base", commit = identity$commit, workers = min(workers, cfg$assessment_workers))
  assessed <- mgf_assess(jobs, cfg, base, practical, context, identity, directory, workers)
  ids <- assessed$datasets$dataset_id[!assessed$datasets$screen_pass]
  extensions <- jobs[jobs$dataset_id %in% ids, ]
  mgc_atomic_rds(extensions, file.path(directory, "extension_jobs.rds"))
  if (nrow(extensions)) {
    mgf_space(cfg, nrow(extensions), directory)
    mgf_status(directory, "extensions_running", commit = identity$commit, workers = workers,
      extension_datasets = length(ids), expected_extension_chains = nrow(extensions))
    mgf_queue(extensions, cfg, base, context, identity, directory, workers, TRUE)
    mgf_status(directory, "assessing_extensions", commit = identity$commit)
    assessed <- mgf_assess(jobs, cfg, base, practical, context, identity, directory, workers, TRUE)
  } else {
    pma_write_csv(assessed$datasets, file.path(directory, "dataset_summary.csv"))
    pma_write_csv(assessed$parameters, file.path(directory, "parameter_summary.csv"))
  }
  mpr_require(identical(mgf_identity(cfg, base, jobs), identity), "Source or scientific inputs changed during sampling.")
  rqr_assert_external_checkout_unchanged(before)
  mgf_status(directory, "reporting", commit = identity$commit)
  result <- mgf_report(cfg, base, directory)
  mgf_status(directory, "calculation_complete_review_required", commit = identity$commit,
    completed_base_chains = nrow(jobs), completed_extension_chains = nrow(extensions), results = result)
  invisible(result)
}

mgf_health <- function(directory) {
  read <- function(name) {
    path <- file.path(directory, name)
    if (file.exists(path)) mgc_json(path) else NULL
  }
  status <- read("campaign_status.json")
  if (!is.null(status)) status$controller_alive <- mgc_alive(status$pid, status$pid_token)
  list(status = status, base = read("base_progress.json"), extensions = read("extension_progress.json"),
    exit = read("controller_exit.json"), final = read("final_summary.json"))
}
