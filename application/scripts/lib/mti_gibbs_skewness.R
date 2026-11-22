mgs_config <- function(path = "application/config/mti_gibbs_skewness_20261122.json") {
  cfg <- mwd_json(path); cfg$configuration_path <- path
  prior <- mwd_json(cfg$prior_config)
  mwd_require(cfg$expected_datasets == 36000L && cfg$expected_cases == 36L &&
    cfg$replications == 1000L && cfg$sampling$chains == 4L && cfg$sampling$n_burn == 500L &&
    identical(as.integer(cfg$sampling$budgets), c(1000L, 2000L, 4000L)) &&
    cfg$sampling$prior_variance == prior$primary_kappa^2 && prior$primary_kappa == 2 &&
    cfg$sampling$learning_rate == 1 && !prior$simulation_reference$estimated_from_analysis_sample &&
    prior$simulation_reference$center == 0 && prior$simulation_reference$scale == 1 &&
    !cfg$refit_ecm && !cfg$automatic_prior_tuning && !cfg$automatic_article_replacement &&
    !cfg$new_tolerance_guarantee && cfg$maximum_workers <= 40L && cfg$maximum_workers >= 1L &&
    cfg$sampler == "intercept_marginal_slice_within_gibbs" &&
    cfg$interval_summary == "posterior_mean_ordered_endpoints", "Invalid fixed-target Gibbs specification.")
  cfg
}

mgs_sources <- c("application/scripts/148_run_mti_gibbs_skewness.R",
  "application/scripts/149_launch_mti_gibbs_skewness.sh",
  "application/scripts/150_prepare_mti_gibbs_skewness_runtime.R",
  "application/tests/testthat/test-mti-gibbs-skewness.R",
  paste0("application/scripts/lib/", c("mti_gibbs_skewness", "mti_gibbs_skewness_reporting", "mti_gibbs_skewness_checks",
    "mti_gibbs_practical", "mti_gibbs_full_comparison", "mti_profile_revision", "mti_profile_review",
    "mti_gibbs_full_publication_figures",
    "mti_gibbs_width_diagnosis", "mti_skewness_extension", "mti_skewness_distributions",
    "mti_skewness_reporting", "mti_gibbs_full_publication", "mti_gibbs_full_report_review",
    "pharma_mti_application", "pharma_tolerance_application"), ".R"))

mgs_context <- function(cfg, parent) {
  parent <- normalizePath(parent, mustWork = TRUE)
  publication <- mwd_json(file.path(cfg$primary_evidence, "provenance.json"))
  mwd_require(publication$study_source_commit == cfg$parent_source_commit &&
    !publication$prior_uses_analysis_data, "The primary publication specification differs.")
  mgfp_check_hashes(publication$historical_hashes)
  for (name in names(publication$data_files)) mwd_require(
    mwd_file_hash(file.path(cfg$primary_evidence, name)) == publication$data_files[[name]],
    "The primary numerical evidence changed before the Gibbs extension.")
  manifest <- mwd_json(file.path(parent, "manifest.json"))
  verified <- mwd_json(file.path(parent, "verification.json"))
  exit <- mwd_json(file.path(parent, "controller_exit.json"))
  mwd_require(manifest$identity_hash == cfg$parent_identity &&
    manifest$identity$source_commit == cfg$parent_source_commit && verified$passed &&
    verified$datasets == cfg$expected_datasets && verified$method_records == 3L * cfg$expected_datasets &&
    !verified$sample_gibbs && exit$exit_code == 0L &&
    manifest$identity$configuration$prior_sd == 2 && manifest$identity$configuration$dp_base_sd == 4 &&
    manifest$identity$configuration$dp_concentration == 1, "The completed parent assessment differs.")
  mwd_require(mwd_file_hash(file.path(parent, "interval_results.csv")) == verified$interval_results_sha256,
    "The original parent export changed.")
  design <- readRDS(file.path(parent, "design.rds"))
  tasks <- design$tasks
  mwd_require(nrow(tasks) == cfg$expected_datasets && !anyDuplicated(tasks$task_key) &&
    setequal(tasks$law_id, cfg$law_ids) && all(table(tasks$case_id) == cfg$replications),
    "Incomplete parent sample index.")
  definition <- mwd_json(cfg$parent_config)
  mwd_require(identical(definition, manifest$identity$configuration[setdiff(names(manifest$identity$configuration), "design")]),
    "The parent distribution specification changed.")
  laws <- setNames(lapply(seq_len(nrow(definition$laws)), function(i)
    msx_law(as.list(definition$laws[i, ]))), definition$laws$id)
  list(parent = parent, manifest = manifest, design = design, laws = laws,
    policy = mwd_json(cfg$diagnostic_config)$policy)
}

mgs_input <- function(task, context, cfg) {
  original_task <- context$design$tasks[task$job_id, , drop = FALSE]
  value <- msx_read(file.path(context$parent, "fits", sprintf("fit_%06d.rds", task$job_id)),
    cfg$parent_identity, original_task)
  y <- value$sample$y; selected <- value$anchor$selected
  e <- value$rows[value$rows$method == "ecm", ]
  mwd_require(value$resolved && length(y) == task$n && all(is.finite(y)) &&
    identical(mwd_hash(y), value$sample$sample_sha256) &&
    value$anchor$prior$prior_sd == 2 && !value$anchor$prior$data_adaptive_prior &&
    nrow(selected) == 1L && nrow(e) == 1L &&
    is.finite(selected$target_content) && selected$target_content > 0 && selected$target_content < 1 &&
    is.finite(selected$mean_tilt) && is.finite(selected$lower) && is.finite(selected$upper) && selected$lower < selected$upper &&
    selected$target_content == e$q && selected$mean_tilt == e$delta &&
    selected$lower == e$lower && selected$upper == e$upper &&
    all(value$rows$returned & value$rows$resolved) &&
    setequal(value$rows$method, c("ecm", "young_mathew", "wilks")),
    "Saved sample, targets, prior or ECM result differs.")
  if ("sample_sha256" %in% names(task)) mwd_require(
    identical(value$sample$sample_sha256, task$sample_sha256) &&
    selected$target_content == task$q && selected$mean_tilt == task$delta &&
    selected$lower == task$ecm_lower && selected$upper == task$ecm_upper,
    "The frozen Gibbs target changed.")
  list(y = y, selected = selected, sample_sha256 = value$sample$sample_sha256,
    rows = value$rows, parent_sha256 = mwd_file_hash(file.path(context$parent, "fits",
      sprintf("fit_%06d.rds", task$job_id))))
}

mgs_index <- function(cfg, context, workers = 4L) {
  tasks <- context$design$tasks
  groups <- split(seq_len(nrow(tasks)), tasks$case_id)
  pieces <- parallel::mclapply(groups, function(ids) data.table::rbindlist(lapply(ids, function(i) {
    t <- tasks[i, , drop = FALSE]; x <- mgs_input(t, context, cfg)
    data.table::data.table(job_id = t$job_id, case_id = t$case_id, law_id = t$law_id, n = t$n,
      content = t$content, tolerance_confidence = t$tolerance_confidence, replication = t$replication,
      parent_task_key = t$task_key, q = x$selected$target_content, delta = x$selected$mean_tilt,
      ecm_lower = x$selected$lower, ecm_upper = x$selected$upper,
      sample_sha256 = x$sample_sha256, parent_sha256 = x$parent_sha256)
  })), mc.cores = min(workers, length(groups)), mc.set.seed = FALSE)
  mwd_require(all(vapply(pieces, is.data.frame, FALSE)), "Parent fit verification failed before sampling.")
  d <- data.table::rbindlist(pieces)[order(job_id)]
  seeds <- as.integer(outer(cfg$chain_seed_base + 10L * d$job_id, seq_len(cfg$sampling$chains), "+"))
  mwd_require(!anyNA(seeds) && !anyDuplicated(seeds) && min(seeds) > 1730000000L &&
    max(seeds) < 1800000000L && nrow(d) == cfg$expected_datasets,
    "Sampling seed stream overlaps the historical seed ranges.")
  as.data.frame(d)
}

mgs_runtime <- function(library) {
  library <- normalizePath(library, mustWork = TRUE)
  inventory <- mwd_json(file.path(dirname(library), "gibbs_skewness_runtime.json"))
  files <- file.path(library, names(inventory$installed_hashes))
  mwd_require(all(file.exists(files)) && identical(unname(unlist(mgfp_hashes(files))),
    unname(unlist(inventory$installed_hashes))), "Frozen Gibbs library changed.")
  for (package in names(inventory$packages)) mwd_require(
    normalizePath(find.package(package)) == file.path(library, package) &&
    as.character(packageVersion(package)) == inventory$packages[[package]], "A Gibbs dependency escaped its frozen library.")
  list(inventory_sha256 = mwd_file_hash(file.path(dirname(library), "gibbs_skewness_runtime.json")),
    packages = inventory$packages, R = R.version.string)
}

mgs_prepare <- function(cfg, context, directory, library) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  receipt <- file.path(directory, "manifest.json"); index <- file.path(directory, "tasks.rds")
  files <- c(mgs_sources, cfg$configuration_path, cfg$parent_config, cfg$prior_config, cfg$diagnostic_config,
    file.path(cfg$historical_evidence, c("provenance.json", "simulation_cells.csv")),
    file.path(cfg$primary_evidence, c("provenance.json", "primary_cells.csv")),
    file.path(cfg$primary_evidence, "population_content_references.csv"),
    "application/config/mti_skewness_publication_20261122.json",
    file.path(context$parent, c("manifest.json", "verification.json", "design.rds")))
  identity <- list(source_commit = mgfp_git("rev-parse", "HEAD"),
    exdqlm_reference_commit = cfg$exdqlm_reference_commit, parent_identity = cfg$parent_identity,
    historical_sampler_commit = cfg$historical_sampler_commit,
    configuration = cfg, inputs = mgfp_hashes(files), runtime = mgs_runtime(library),
    sampler = cfg$sampler, prior_uses_analysis_data = FALSE, q_delta_resampled = FALSE,
    refit_ecm = FALSE, automatic_article_replacement = FALSE)
  if (file.exists(receipt)) {
    old <- mwd_json(receipt)
    mwd_require(old$identity_hash == mwd_hash(identity) && mwd_file_hash(index) == old$tasks_sha256,
      "This run has a different source, runtime or input index; do not overwrite it.")
    tasks <- readRDS(index)
  } else {
    tasks <- mgs_index(cfg, context)
    mp_atomic_rds(tasks, index)
    pma_write_csv(tasks, file.path(directory, "tasks.csv"))
    mgfp_json_write(list(identity = identity, identity_hash = mwd_hash(identity),
      tasks_sha256 = mwd_file_hash(index), datasets = nrow(tasks), chains = 4L * nrow(tasks),
      parent = context$parent, created_utc = mgfp_time()), receipt)
  }
  list(tasks = tasks, identity = mwd_hash(identity), manifest = identity)
}

mgs_chain_path <- function(directory, id, chain, count) file.path(directory, "chains",
  sprintf("%04d", (id - 1L) %/% 1000L), sprintf("dataset_%06d_chain_%d_n%d.rds", id, chain, count))
mgs_result_path <- function(directory, id) file.path(directory, "assessments",
  sprintf("%04d", (id - 1L) %/% 1000L), sprintf("dataset_%06d.rds", id))

mgs_write <- function(value, path, identity) {
  mwd_require(!file.exists(path) && !file.exists(paste0(path, ".json")), "Refusing to overwrite a completed result.")
  mp_atomic_rds(value, path)
  mgfp_json_write(list(identity = identity, task_sha256 = mwd_hash(value$task),
    sha256 = mwd_file_hash(path)), paste0(path, ".json"))
}

mgs_read <- function(path, task, identity, validate) {
  if (!file.exists(path)) {
    mwd_require(!file.exists(paste0(path, ".json")), "A completion receipt has lost its fitted object.")
    return(NULL)
  }
  value <- readRDS(path)
  mwd_require(identical(value$task, task) && identical(value$identity, identity), "Checkpoint identity differs.")
  if (file.exists(paste0(path, ".json"))) {
    receipt <- mwd_json(paste0(path, ".json"))
    mwd_require(receipt$identity == identity && receipt$task_sha256 == mwd_hash(task) &&
      receipt$sha256 == mwd_file_hash(path), "Checkpoint checksum differs.")
  }
  validate(value)
  if (!file.exists(paste0(path, ".json"))) mgfp_json_write(list(identity = identity,
    task_sha256 = mwd_hash(task), sha256 = mwd_file_hash(path), recovered_atomic_write = TRUE), paste0(path, ".json"))
  value
}

mgs_validate_chain <- function(value, y, cfg, count) {
  fit <- value$fit; task <- value$task
  mwd_require(value$sample_sha256 == mwd_hash(y) && nrow(fit$draws) == count &&
    ncol(fit$draws) == 2L && all(is.finite(fit$draws)) && all(fit$draws[, 1] <= fit$draws[, 2]) &&
    fit$sampler == cfg$sampler && length(fit$retained_objective) == count &&
    length(fit$target_loss_trace) == cfg$sampling$n_burn + count &&
    all(is.finite(fit$target_loss_trace)) &&
    nrow(fit$warmup_root1) == cfg$sampling$n_burn && nrow(fit$warmup_root2) == cfg$sampling$n_burn &&
    is.integer(fit$last$rng_state) && all(is.finite(fit$retained_objective)) &&
    all(is.finite(fit$warmup_root1)) && all(is.finite(fit$warmup_root2)) &&
    identical(unname(sort(c(fit$last$beta_root1, fit$last$beta_root2))), unname(fit$draws[count, ])),
    "Invalid Gibbs draws, warm-up or terminal state.")
  ids <- unique(as.integer(round(seq(1, count, length.out = min(16L, count)))))
  objective <- mgp_objective(y, fit$draws[ids, 1], fit$draws[ids, 2], task$q, task$delta,
    cfg$sampling$learning_rate, 1 / cfg$sampling$prior_variance)
  mwd_require(max(abs(objective - fit$retained_objective[ids])) <= 1e-10 * max(1, abs(objective)),
    "Saved Gibbs objective differs from the direct MTI posterior.")
  invisible(TRUE)
}

mgs_extend <- function(y, task, settings, old, count) {
  mwd_require(count > nrow(old$draws), "Continuation must add retained iterations.")
  extra <- mgf_continue(y, task, settings, old, count - nrow(old$draws))
  fit <- old
  fit$draws <- rbind(old$draws, extra$draws)
  fit$retained_objective <- c(old$retained_objective, extra$retained_objective)
  fit$target_loss_trace <- c(old$target_loss_trace,
    (extra$retained_objective - rowSums(extra$draws^2) / (2 * settings$prior_variance)) / settings$learning_rate)
  fit$last <- extra$last
  fit
}

mgs_summary_task <- function(task) data.frame(case_id = task$case_id, dataset_id = task$job_id,
  source = "simulation", response_id = task$law_id, n = task$n, content = task$content,
  q = task$q, delta = task$delta, ecm_lower = task$ecm_lower, ecm_upper = task$ecm_upper)

mgs_sample <- function(task, input, cfg, context, directory, identity) {
  initial <- mwd_initial(input$y, task$ecm_lower, task$ecm_upper, cfg$sampling$prior_variance, 0)
  settings <- c(cfg$sampling, list(initial_midpoint_offsets_sd = initial$offsets,
    initial_halfwidth_multipliers = initial$radii))
  history <- list(); fits <- vector("list", cfg$sampling$chains)
  for (count in cfg$sampling$budgets) {
    for (chain in seq_len(cfg$sampling$chains)) {
      t <- data.frame(chain = chain, seed = as.integer(cfg$chain_seed_base + 10L * task$job_id + chain),
        q = task$q, delta = task$delta, ecm_lower = task$ecm_lower, ecm_upper = task$ecm_upper)
      path <- mgs_chain_path(directory, task$job_id, chain, count)
      saved <- mgs_read(path, t, identity, function(x) mgs_validate_chain(x, input$y, cfg, count))
      if (is.null(saved)) {
        fit <- if (is.null(fits[[chain]])) mgp_slice_fit(input$y, t, settings,
          n_burn = settings$n_burn, n_keep = count) else
          mgs_extend(input$y, t, settings, fits[[chain]], count)
        saved <- list(task = t, identity = identity, fit = fit, sample_sha256 = input$sample_sha256)
        mgs_validate_chain(saved, input$y, cfg, count)
        mgs_write(saved, path, identity)
      }
      fits[[chain]] <- saved$fit
    }
    parameters <- mgp_summaries(fits, mgs_summary_task(task), context$policy)
    adequate <- all(parameters$preferred_pass & parameters$mean_precision)
    history[[length(history) + 1L]] <- data.frame(retained_per_chain = count, adequate = adequate,
      screen_pass = all(parameters$screen_pass), max_rhat = max(parameters$rhat),
      min_bulk_ess = min(parameters$ess_bulk), min_tail_ess = min(parameters$ess_tail),
      max_mean_mcse = max(parameters$mcse_mean))
    if (adequate) break
  }
  endpoints <- parameters$mean[match(c("lower", "upper"), parameters$variable)]
  distribution <- rqrgibbs::rqr_dp_fit(input$y, concentration = 1,
    base_measure = rqrgibbs::rqr_dp_base_normal(0, 4))
  probability <- rqrgibbs::rqr_dp_content_probability(distribution, endpoints[1], endpoints[2], task$content)
  mass <- msx_content(context$laws[[task$law_id]], endpoints[1], endpoints[2])
  row <- data.frame(job_id = task$job_id, task_key = paste(cfg$study_id, task$job_id, sep = ":"),
    law_id = task$law_id, n = task$n, content = task$content, tolerance_confidence = task$tolerance_confidence,
    replication = task$replication, method = "gibbs", returned = TRUE,
    lower = endpoints[1], upper = endpoints[2], width = endpoints[2] - endpoints[1],
    population_content = mass, attained = mass >= task$content, q = task$q, delta = task$delta,
    dp_probability = probability$posterior_content_probability, numerical_adequate = adequate,
    screen_pass = all(parameters$screen_pass), retained_per_chain = count)
  row$ecm_probability_threshold <- if ("gamma" %in% names(input$rows)) input$rows$gamma[input$rows$method == "ecm"] else NA_real_
  support <- if (is.function(context$laws[[task$law_id]]$q)) context$laws[[task$law_id]]$q(0) else NA_real_
  row$lower_below_support <- endpoints[1] < support
  row$posterior_lower_below_support <- mean(vapply(fits, function(x) mean(x$draws[, 1] < support), 0))
  list(task = task, identity = identity, row = row, parameters = parameters,
    sampling_history = do.call(rbind, history), sample_sha256 = input$sample_sha256,
    parent_sha256 = input$parent_sha256, adequate = adequate)
}

mgs_resources <- function(cfg, directory, remaining, requested = cfg$maximum_workers) {
  cores <- if (requireNamespace("parallelly", quietly = TRUE)) as.integer(parallelly::availableCores()) else parallel::detectCores()
  load <- as.numeric(strsplit(readLines("/proc/loadavg"), " ")[[1]][1])
  memory <- as.numeric(strsplit(trimws(sub("MemAvailable:", "", grep("^MemAvailable:",
    readLines("/proc/meminfo"), value = TRUE))), "[[:space:]]+")[[1]][1]) / 1024^2
  workers <- min(as.integer(requested), cfg$maximum_workers, max(0L, floor(cores - load)),
    max(0L, floor((memory - cfg$memory_reserve_gib) / cfg$memory_per_worker_gib)))
  free <- as.numeric(strsplit(trimws(tail(system2("df", c("-Pk", shQuote(directory)), stdout = TRUE), 1)), " +")[[1]][4]) * 1024
  inodes <- as.numeric(strsplit(trimws(tail(system2("df", c("-Pi", shQuote(directory)), stdout = TRUE), 1)), " +")[[1]][4])
  required <- cfg$disk_reserve_gib * 1024^3 + remaining * cfg$worst_case_bytes_per_dataset
  mwd_require(is.finite(workers) && workers >= 1L && free >= required &&
    inodes >= remaining * 26L + cfg$inode_reserve, "Insufficient free CPU, memory, disk or inodes.")
  list(workers = as.integer(workers), available_gib = free / 1024^3, required_gib = required / 1024^3,
    available_memory_gib = memory)
}

mgs_run_jobs <- function(tasks, cfg, context, directory, identity, workers) {
  paths <- vapply(tasks$job_id, function(id) mgs_result_path(directory, id), "")
  pending <- which(!file.exists(paths) | !file.exists(paste0(paths, ".json")))
  # Existing records are validated before reuse, including recoverable atomic writes.
  for (i in which(file.exists(paths))) mgs_read(paths[i], tasks[i, , drop = FALSE], identity,
    function(x) mwd_require(x$sample_sha256 == tasks$sample_sha256[i], "Saved assessment sample differs."))
  pending <- which(!file.exists(paste0(paths, ".json")))
  start <- Sys.time(); completed <- 0L; errors <- 0L
  for (batch in split(pending, ceiling(seq_along(pending) / (2L * workers)))) {
    mwd_require(!file.exists(file.path(directory, "STOP_REQUESTED")), "Graceful stop requested; completed chains remain resumable.")
    mgs_resources(cfg, directory, length(pending) - completed, workers)
    results <- parallel::mclapply(batch, function(i) {
      task <- tasks[i, , drop = FALSE]; input <- mgs_input(task, context, cfg)
      mwd_require(input$parent_sha256 == task$parent_sha256, "A parent fit changed during sampling.")
      started <- proc.time()[3]; warnings <- character()
      value <- tryCatch(withCallingHandlers(mgs_sample(task, input, cfg, context, directory, identity),
        warning = function(w) {warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning")}),
        error = function(e) list(task = task, identity = identity, sample_sha256 = input$sample_sha256,
          parent_sha256 = input$parent_sha256, error = conditionMessage(e), adequate = FALSE))
      value$warnings <- warnings
      value$elapsed_seconds <- unname(proc.time()[3] - started)
      mgs_write(value, paths[i], identity)
      list(error = !is.null(value$error), adequate = value$adequate, seconds = value$elapsed_seconds)
    }, mc.cores = min(workers, length(batch)), mc.preschedule = FALSE, mc.set.seed = FALSE)
    mwd_require(all(vapply(results, is.list, FALSE)), "A worker or input check failed; saved results remain reusable.")
    completed <- completed + length(batch); errors <- errors + sum(vapply(results, `[[`, FALSE, "error"))
    elapsed <- as.numeric(difftime(Sys.time(), start, units = "secs"))
    mgfp_json_write(list(completed_datasets = nrow(tasks) - length(pending) + completed,
      expected_datasets = nrow(tasks), remaining_datasets = length(pending) - completed,
      sampling_errors_this_session = errors, workers = workers,
      eta_seconds = elapsed / completed * (length(pending) - completed), time = mgfp_time()),
      file.path(directory, "progress.json"))
    cat(mgfp_time(), nrow(tasks) - length(pending) + completed, "/", nrow(tasks), "datasets saved\n"); flush.console()
  }
  invisible(paths)
}
