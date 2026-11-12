mgc_default <- "application/config/mti_gibbs_ecm_comparison_20261111.json"

mgc_authorized <- function(confirmation, commit) {
  mpr_require(nchar(commit) == 40L && identical(confirmation, commit),
    "Gibbs sampling is not authorized: explicit confirmation of the article commit is required.")
  invisible(TRUE)
}
mgc_hash <- function(x) digest::digest(x, algo = "sha256")
mgc_file_hash <- function(path) digest::digest(file = path, algo = "sha256", serialize = FALSE)
mgc_json <- function(path) jsonlite::read_json(path, simplifyVector = TRUE)
mgc_time <- function() format(Sys.time(), tz = "UTC", usetz = TRUE)
mgc_jobs <- function(output) readRDS(file.path(output, "jobs.rds"))

mgc_atomic_rds <- function(value, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(".gibbs-", tmpdir = dirname(path))
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(value, tmp, compress = TRUE)
  mpr_require(file.rename(tmp, path), "Could not publish a Gibbs checkpoint.")
  invisible(path)
}
mgc_pid_token <- function(pid) {
  path <- sprintf("/proc/%d/stat", as.integer(pid))
  if (!file.exists(path)) return(NA_character_)
  fields <- strsplit(sub("^.*\\) ", "", readLines(path, warn = FALSE)), " +")[[1L]]
  fields[20L]
}
mgc_alive <- function(pid, token) {
  current <- mgc_pid_token(pid)
  !is.na(current) && identical(as.character(current), as.character(token))
}
mgc_lock <- function(output, recover = FALSE) {
  dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
  path <- paste0(output, ".lock")
  if (dir.exists(path) && isTRUE(recover)) {
    owner <- tryCatch(mgc_json(file.path(path, "owner.json")), error = function(e) NULL)
    mpr_require(!is.null(owner) && identical(owner$host, Sys.info()[["nodename"]]) &&
      !mgc_alive(owner$pid, owner$token), "Cannot recover a live or unidentified study lock.")
    stale <- tempfile(".stale-lock-", tmpdir = dirname(path))
    mpr_require(file.rename(path, stale), "Another controller changed the study lock.")
    unlink(stale, recursive = TRUE)
  }
  mpr_require(dir.create(path, showWarnings = FALSE),
    "Study directory is locked; do not start a second controller.")
  pta_atomic_write_json(list(pid = Sys.getpid(), token = mgc_pid_token(Sys.getpid()),
    host = Sys.info()[["nodename"]], started_utc = mgc_time()), file.path(path, "owner.json"))
  path
}
mgc_context <- function(cfg) {
  article <- mre_config(cfg$ecm_article_config); f <- mre_frozen_functions(article)
  validation <- jsonlite::read_json(article$validation_config, simplifyVector = FALSE)
  laws <- setNames(lapply(validation$dgps, f$laws$dgp_meta),
    vapply(validation$dgps, "[[", "", "dgp_id"))
  app <- pma_read_config(cfg$application_config)
  pma_verify_run(app$paths$canonical_dir, app)
  membership <- pta_read_csv(file.path(app$paths$canonical_dir, "split_membership.csv.gz"))
  training <- membership[membership$training, ]
  list(laws = laws,
    observed = pta_read_csv(file.path(app$paths$canonical_dir, "observed_batches.csv")),
    training_ids = split(training$analysis_row_id,
      paste(training$split_scheme, training$split_id, sep = ":")))
}
mgc_sample <- function(task, context) {
  if (task$source == "simulation") {
    set.seed(task$data_seed)
    y <- context$laws[[task$response_id]]$r(task$n)
    mpr_require(identical(mgc_hash(y), task$dataset_sha256),
      "Gibbs sample differs from the paired ECM sample.")
    return(y)
  }
  if (!is.null(context$training_ids)) {
    ids <- context$training_ids[[paste(task$split_scheme, task$split_id, sep = ":")]]
  } else {
    m <- context$membership
    ids <- m$analysis_row_id[m$split_scheme == task$split_scheme &
      m$split_id == task$split_id & m$training]
  }
  index <- match(ids, context$observed$.analysis_row)
  mpr_require(length(ids) == task$n && !anyNA(index), "Application training membership is incomplete.")
  y <- as.numeric(context$observed[[task$response_id]][index])
  if ("dataset_sha256" %in% names(task) && nzchar(task$dataset_sha256)) {
    mpr_require(identical(mgc_hash(y), task$dataset_sha256), "Application sample digest changed.")
  }
  y
}
mgc_design <- function(cfg, stage = "diagnostic", context = NULL) {
  mpr_require(stage %in% c("diagnostic", "full", "benchmark"), "Unknown comparison stage.")
  article <- mre_config(cfg$ecm_article_config)
  provenance <- mgc_json(file.path(article$evidence_dir, "provenance.json"))
  pma_verify_hash(file.path(article$cache_dir, "results.csv"), provenance$results_sha256)
  evidence <- read.csv(file.path(article$cache_dir, "results.csv"))
  evidence <- evidence[evidence$method == article$primary_method, ]
  simulation <- data.frame(source = "simulation", case_id = paste0("sim_", evidence$cell_index,
    "_", evidence$replication), response_id = evidence$dgp_id, n = evidence$n,
    content = evidence$content, tolerance_confidence = evidence$tolerance_confidence,
    replication = evidence$replication, data_seed = evidence$seed,
    dataset_sha256 = evidence$dataset_sha256, q = evidence$selected_q,
    delta = evidence$selected_delta, ecm_lower = evidence$lower, ecm_upper = evidence$upper,
    split_scheme = "", split_id = NA_integer_, stringsAsFactors = FALSE)
  app_cfg <- pma_read_config(cfg$application_config)
  pma_verify_run(app_cfg$paths$canonical_dir, app_cfg)
  results <- pta_read_csv(file.path(app_cfg$paths$canonical_dir, "results.csv"))
  app <- results[results$method_id == pma_methods[1], ]
  application <- data.frame(source = "application", case_id = paste("app", app$response_id,
    app$split_scheme, app$split_id, sep = "_"), response_id = app$response_id, n = app$train_n,
    content = app$content, tolerance_confidence = app$tolerance_confidence,
    replication = app$split_id, data_seed = NA_integer_, dataset_sha256 = "",
    q = app$mti_selected_q, delta = app$mti_selected_tilt, ecm_lower = app$lower,
    ecm_upper = app$upper, split_scheme = app$split_scheme, split_id = app$split_id)
  if (is.null(context)) context <- mgc_context(cfg)
  application$dataset_sha256 <- vapply(seq_len(nrow(application)), function(i)
    mgc_hash(mgc_sample(application[i, ], context)), "")
  tasks <- rbind(simulation, application)
  mpr_require(!anyDuplicated(tasks$case_id) && all(is.finite(tasks$q) & tasks$q < 1 &
    tasks$q > tasks$content & is.finite(tasks$delta) & is.finite(tasks$ecm_lower) &
    is.finite(tasks$ecm_upper) & tasks$ecm_upper > tasks$ecm_lower), "Invalid fixed-target study design.")
  tasks$dataset_id <- seq_len(nrow(tasks))
  tasks$diagnostic_role <- "full"
  base <- tasks$replication <= cfg$diagnostic_replications_per_case
  if (stage == "diagnostic") {
    keep <- base
    if (isTRUE(cfg$diagnostic_include_extremes)) {
      groups <- split(seq_len(nrow(simulation)), evidence$cell_index)
      extremes <- unique(unlist(lapply(groups, function(i)
        c(i[which.max(simulation$q[i])], i[which.max(abs(simulation$delta[i]))]))))
      keep[extremes] <- TRUE
    }
    tasks$diagnostic_role <- ifelse(base, "base", "extreme")
    tasks <- tasks[keep, ]
  } else if (stage == "benchmark") {
    candidates <- which(base & tasks$source == "simulation")
    selected <- candidates[unique(round(seq(1, length(candidates),
      length.out = min(length(candidates), cfg$benchmark_datasets))))]
    tasks <- tasks[selected, ]
    tasks$diagnostic_role <- "timing_only"
  }
  jobs <- tasks[rep(seq_len(nrow(tasks)), each = cfg$chains), ]
  jobs$chain <- rep(seq_len(cfg$chains), nrow(tasks))
  offset <- if (stage == "benchmark") 4000000 else cfg$stage_seed_offsets[[stage]]
  jobs$seed <- cfg$seed_base + offset + 17L * jobs$dataset_id + jobs$chain
  jobs$job_id <- seq_len(nrow(jobs)); rownames(jobs) <- NULL
  mpr_require(!anyDuplicated(jobs$seed) && !any(jobs$seed %in% simulation$data_seed),
    "Chain seeds overlap or collide with dataset seeds.")
  jobs
}
mgc_budget <- function(cfg, stage) {
  if (stage == "benchmark") list(n_burn = cfg$benchmark_n_burn, n_mcmc = cfg$benchmark_n_mcmc) else
    list(n_burn = cfg$n_burn, n_mcmc = cfg$n_mcmc)
}
mgc_inputs <- function(cfg) {
  article <- mre_config(cfg$ecm_article_config); app <- pma_read_config(cfg$application_config)
  paths <- c(cfg$ecm_article_config, cfg$application_config, article$validation_config,
    file.path(article$evidence_dir, "provenance.json"),
    file.path(app$paths$canonical_dir, c("results.csv", "observed_batches.csv", "split_membership.csv.gz")))
  setNames(vapply(paths, mgc_file_hash, ""), paths)
}
mgc_runtime <- function(commit = pma_git("rev-parse", "HEAD")) {
  path <- file.path("application/cache", paste0("mti_gibbs_runtime_", commit))
  mpr_require(file.exists(file.path(path, "commit.txt")) &&
    identical(readLines(file.path(path, "commit.txt")), commit),
    "Prepare the installed runtime from this exact commit first.")
  library <- normalizePath(file.path(path, "library"), mustWork = TRUE)
  mpr_require(requireNamespace("rqrgibbs", quietly = TRUE) &&
    identical(normalizePath(dirname(find.package("rqrgibbs"))), library) &&
    identical(normalizePath(getNamespaceInfo(asNamespace("rqrgibbs"), "path")),
      normalizePath(file.path(library, "rqrgibbs"))), "Loaded sampler is not the archived-commit runtime.")
  mpr_require(requireNamespace("posterior", quietly = TRUE) &&
    requireNamespace("parallelly", quietly = TRUE), "posterior and parallelly packages are required.")
  files <- file.path(library, "rqrgibbs", c("DESCRIPTION", "R/rqrgibbs.rdb", "libs/rqrgibbs.so"))
  attestation <- readLines(file.path(path, "attestation_path.txt"), warn = FALSE)
  mpr_require(length(attestation) == 1L && file.exists(attestation), "Runtime attestation is missing.")
  binding <- readRDS(attestation)
  mpr_require(identical(binding$source_commit, commit) &&
    identical(normalizePath(binding$runtime_package_path), file.path(library, "rqrgibbs")),
    "Runtime attestation does not match the committed package.")
  list(library = library, commit = commit, repo_root = binding$source_repo_root,
    attestation = attestation,
    sha256 = mgc_hash(c(vapply(files, mgc_file_hash, ""), mgc_file_hash(attestation))))
}
mgc_identity <- function(cfg, jobs, commit, runtime) {
  list(commit = commit, settings_sha256 = mgc_hash(cfg), design_sha256 = mgc_hash(jobs),
    inputs_sha256 = mgc_hash(mgc_inputs(cfg)), runtime_sha256 = runtime$sha256)
}
mgc_match_identity <- function(actual, expected) {
  all(vapply(names(expected), function(k) identical(actual[[k]], expected[[k]]), logical(1)))
}
mgc_check_diagnostic <- function(cfg, commit, runtime, directory, diagnostic_jobs) {
  path <- file.path(directory, "assessment.json")
  mpr_require(file.exists(path), "Diagnostic assessment is missing.")
  assessment <- mgc_json(path)
  expected <- mgc_identity(cfg, diagnostic_jobs, commit, runtime)
  mpr_require(isTRUE(assessment$passed) && isTRUE(assessment$complete) &&
    mgc_match_identity(assessment, expected) && assessment$chains == nrow(diagnostic_jobs),
    "The diagnostic assessment has not passed for these settings, inputs, and runtime.")
  invisible(TRUE)
}
mgc_free_bytes <- function(path) {
  while (!dir.exists(path)) path <- dirname(path)
  out <- system2("df", c("-Pk", shQuote(path)), stdout = TRUE)
  as.numeric(strsplit(trimws(out[length(out)]), " +")[[1L]][4L]) * 1024
}
mgc_storage_check <- function(cfg, remaining, path, stage, available = mgc_free_bytes(path)) {
  estimate <- if (stage == "full") cfg$full_chain_storage_bytes else
    8 * (4 * cfg$n_mcmc + 3 * cfg$n_burn) + 20000
  needed <- remaining * estimate * (1 + cfg$disk_overhead_fraction)
  mpr_require(is.finite(available) && available >= needed + cfg$disk_reserve_gib * 1024^3,
    sprintf("Insufficient disk: projected %.2f GiB plus %.2f GiB reserve, available %.2f GiB.",
      needed / 1024^3, cfg$disk_reserve_gib, available / 1024^3))
  invisible(needed)
}
mgc_chain_path <- function(output, id) file.path(output, "chains", sprintf("chain_%06d.rds", id))
mgc_resources <- function(cfg, workers) {
  cores <- as.integer(parallelly::availableCores())
  memory <- readLines("/proc/meminfo", warn = FALSE)
  available <- as.numeric(strsplit(trimws(sub("^MemAvailable:", "",
    memory[startsWith(memory, "MemAvailable:")])), " +")[[1L]][1L]) / 1024^2
  mpr_require(workers <= cores && available >=
    cfg$controller_memory_gib + workers * cfg$worker_memory_gib,
    "The worker count exceeds the available CPU or memory allocation.")
  list(available_cpus = cores, available_memory_gib = available,
    load_average = scan("/proc/loadavg", nmax = 1L, quiet = TRUE), checked_utc = mgc_time())
}
mgc_validate_record <- function(record, task, budget) {
  mpr_require(isTRUE(record$passed) && isTRUE(all.equal(record$task, task, check.attributes = FALSE)) &&
    identical(record$sample_sha256, task$dataset_sha256) && identical(record$repair_count, 0L) &&
    is.matrix(record$draws) && nrow(record$draws) == budget$n_mcmc &&
    identical(colnames(record$draws), c("lower", "upper")) && all(is.finite(record$draws)) &&
    all(record$draws[, "lower"] <= record$draws[, "upper"]) &&
    all(is.finite(record$objective_summary)), "Incompatible target, sample, numerical status, or draws.")
  invisible(TRUE)
}
mgc_receipt <- function(path, task, budget, verify = TRUE) {
  mpr_require(file.exists(path) && file.exists(paste0(path, ".json")), "Incomplete chain checkpoint.")
  receipt <- mgc_json(paste0(path, ".json"))
  mpr_require(isTRUE(receipt$passed) && receipt$job_id == task$job_id &&
    identical(receipt$task_sha256, mgc_hash(task)) && receipt$n_mcmc == budget$n_mcmc &&
    identical(receipt$sample_sha256, task$dataset_sha256), "Incompatible chain completion record.")
  if (verify) {
    mpr_require(identical(mgc_file_hash(path), receipt$sha256), "Corrupted chain checkpoint.")
    mgc_validate_record(readRDS(path), task, budget)
  }
  receipt
}
mgc_fit <- function(cfg, task, y, budget, trace = FALSE, runtime = mgc_runtime()) {
  scale <- max(sd(y), 1e-4)
  midpoint <- (task$ecm_lower + task$ecm_upper) / 2 +
    cfg$initial_midpoint_offsets_sd[task$chain] * scale
  halfwidth <- (task$ecm_upper - task$ecm_lower) / 2 * cfg$initial_halfwidth_multipliers[task$chain]
  rqrgibbs::rqr_mcmc_fit(y, matrix(1, length(y), 1), task$q,
    learning_rate = cfg$learning_rate, mean_tilt = task$delta,
    beta_prior_obj = rqrgibbs::beta_prior("ridge", ridge = list(tau2 = cfg$prior_variance)),
    numerical_policy = "fail", mcmc_control = list(n_burn = budget$n_burn,
      n_mcmc = budget$n_mcmc, thin = cfg$thin, seed = task$seed, verbose = FALSE,
      store_latent_draws = FALSE, store_warmup_roots = trace,
      root_label_control = list(canonicalize_draws = FALSE)),
    provenance_control = list(repo_root = runtime$repo_root, expected_git_commit = runtime$commit,
      primary_runtime_attestation = runtime$attestation),
    init = list(beta1 = midpoint - halfwidth, beta2 = midpoint + halfwidth))
}
mgc_chain <- function(cfg, task, context, budget, output, stage) {
  started <- proc.time()[[3L]]; path <- mgc_chain_path(output, task$job_id)
  tryCatch({
    y <- mgc_sample(task, context)
    fit <- mgc_fit(cfg, task, y, budget, trace = stage == "diagnostic", runtime = context$runtime)
    mpr_require(isTRUE(fit$model_spec$target_numerical_eligible) &&
      fit$model_spec$numerical_repair_count == 0L && isTRUE(fit$provenance$reproducibility_eligible),
      "The sampler reports a numerical or reproducibility failure.")
    a <- as.numeric(fit$samp.beta_root1); b <- as.numeric(fit$samp.beta_root2)
    loss <- fit$diagnostics$mean_tilted_target_loss_trace
    retained <- budget$n_burn + seq_len(budget$n_mcmc) * cfg$thin
    objective <- cfg$learning_rate * loss[retained] + (a^2 + b^2) / (2 * cfg$prior_variance)
    mpr_require(all(is.finite(objective)), "Nonfinite posterior objective.")
    record <- list(task = task, draws = cbind(lower = pmin(a, b), upper = pmax(a, b)),
      objective_summary = c(mean = mean(objective), min = min(objective), max = max(objective)),
      repair_count = 0L, elapsed_seconds = proc.time()[[3L]] - started,
      sample_sha256 = mgc_hash(y), passed = TRUE)
    if (stage == "diagnostic") {
      record$target_loss_trace <- loss; record$retained_objective <- objective
      record$warmup_root1 <- fit$diagnostics$warmup_root1
      record$warmup_root2 <- fit$diagnostics$warmup_root2
      record$last <- fit$last
    }
    mgc_validate_record(record, task, budget); mgc_atomic_rds(record, path)
    pta_atomic_write_json(list(job_id = task$job_id, dataset_id = task$dataset_id,
      task_sha256 = mgc_hash(task), sample_sha256 = record$sample_sha256,
      sha256 = mgc_file_hash(path), bytes = unname(file.info(path)$size),
      elapsed_seconds = record$elapsed_seconds, n_mcmc = budget$n_mcmc,
      n = task$n, source = task$source, passed = TRUE, finished_utc = mgc_time()), paste0(path, ".json"))
    unlink(paste0(path, ".error.json")); TRUE
  }, error = function(e) {
    pta_atomic_write_json(list(job = task$job_id, error = conditionMessage(e),
      task = task, failed_utc = mgc_time()), paste0(path, ".error.json"))
    FALSE
  })
}
mgc_dataset_summary <- function(cfg, s, budget, output) {
  mpr_require(nrow(s) == cfg$chains && !anyDuplicated(s$chain), "Incomplete or duplicate chain identities.")
  fits <- lapply(seq_len(nrow(s)), function(i) {
    path <- mgc_chain_path(output, s$job_id[i]); mgc_receipt(path, s[i, , drop = FALSE], budget)
    readRDS(path)
  })
  variables <- c("lower", "upper", "width", "midpoint")
  a <- array(NA_real_, c(budget$n_mcmc, cfg$chains, 4L), dimnames = list(NULL, NULL, variables))
  for (i in seq_along(fits)) {
    d <- fits[[i]]$draws
    a[, i, ] <- cbind(d[, "lower"], d[, "upper"], d[, "upper"] - d[, "lower"], rowMeans(d))
  }
  d <- posterior::summarise_draws(posterior::as_draws_array(a),
    "mean", "median", "sd", "rhat", "ess_bulk", "ess_tail", "mcse_mean")
  intervals <- t(vapply(seq_along(variables), function(i)
    quantile(a[, , i], c(.025, .975), names = FALSE), numeric(2)))
  passed <- all(is.finite(d$rhat) & d$rhat <= cfg$rhat_limit &
    d$ess_bulk >= cfg$minimum_bulk_ess & d$ess_tail >= cfg$minimum_tail_ess &
    is.finite(d$mcse_mean) & d$mcse_mean <= cfg$maximum_mcse_sd_ratio * d$sd)
  data.frame(case_id = s$case_id[1], dataset_id = s$dataset_id[1], source = s$source[1],
    response_id = s$response_id[1], n = s$n[1], content = s$content[1], q = s$q[1], delta = s$delta[1],
    d, lower_025 = intervals[, 1], upper_975 = intervals[, 2],
    ecm_value = c(s$ecm_lower[1], s$ecm_upper[1], s$ecm_upper[1] - s$ecm_lower[1],
      (s$ecm_lower[1] + s$ecm_upper[1]) / 2),
    elapsed_seconds = sum(vapply(fits, "[[", numeric(1), "elapsed_seconds")),
    diagnostics_pass = passed, row.names = NULL)
}
mgc_assess <- function(cfg, output, workers = 1L, complete_required = FALSE,
                       dataset_ids = NULL, jobs = NULL) {
  manifest <- mgc_json(file.path(output, "manifest.json"))
  if (is.null(jobs)) jobs <- mgc_jobs(output)
  mpr_require(identical(manifest$identity$settings_sha256, mgc_hash(cfg)) &&
    identical(manifest$identity$design_sha256, mgc_hash(jobs)), "Assessment settings or job identities changed.")
  chosen <- if (is.null(dataset_ids)) jobs else jobs[jobs$dataset_id %in% dataset_ids, ]
  groups <- split(chosen, chosen$dataset_id)
  dir.create(file.path(output, "assessments"), showWarnings = FALSE)
  assess_one <- function(s) {
    path <- file.path(output, "assessments", sprintf("dataset_%06d.rds", s$dataset_id[1]))
    if (file.exists(path)) {
      cached <- readRDS(path)
      mpr_require(identical(cached$identity, manifest$identity), "Cached assessment identity changed.")
      if (is.null(dataset_ids)) {
        for (i in seq_len(nrow(s))) mgc_receipt(mgc_chain_path(output, s$job_id[i]),
          s[i, , drop = FALSE], manifest$budget)
      }
      return(cached$result)
    }
    files <- vapply(s$job_id, function(id) paste0(mgc_chain_path(output, id), ".json"), "")
    if (!all(file.exists(files))) return(NULL)
    result <- mgc_dataset_summary(cfg, s, manifest$budget, output)
    mgc_atomic_rds(list(identity = manifest$identity, result = result), path)
    result
  }
  rows <- parallel::mclapply(groups, assess_one, mc.cores = max(1L, as.integer(workers)),
    mc.preschedule = TRUE, mc.set.seed = FALSE)
  mpr_require(!any(vapply(rows, inherits, logical(1), "try-error")), "A dataset assessment failed.")
  rows <- Filter(Negate(is.null), rows)
  result <- if (length(rows)) do.call(rbind, rows) else data.frame()
  if (!is.null(dataset_ids)) return(invisible(result))
  if (nrow(result)) pma_write_csv(result, file.path(output, "posterior_summary.csv"))
  complete <- length(rows) == length(groups)
  if (isTRUE(manifest$external_guard_passed) && !is.null(manifest$external_reference)) {
    rqr_assert_external_checkout_unchanged(manifest$external_reference)
  }
  if (!is.null(manifest$inputs)) {
    mpr_require(identical(mgc_hash(mgc_inputs(cfg)), manifest$identity$inputs_sha256),
      "Study inputs changed before assessment publication.")
  }
  pta_atomic_write_json(c(manifest$identity, list(complete = complete,
    passed = complete && isTRUE(manifest$external_guard_passed) && nrow(result) > 0 &&
      all(result$diagnostics_pass), datasets = length(rows), expected_datasets = length(groups),
    chains = nrow(jobs), new_tolerance_guarantee = FALSE, article_replaced = FALSE,
    assessed_utc = mgc_time())), file.path(output, "assessment.json"))
  if (complete_required) mpr_require(complete, "The assessment has incomplete datasets.")
  invisible(result)
}
mgc_run <- function(cfg, jobs, stage, output, workers, resume = FALSE,
                    diagnostic_dir = file.path(cfg$output_root, "diagnostic")) {
  commit <- pma_git("rev-parse", "HEAD")
  mpr_require(mpr_git_clean(pma_git("status", "--porcelain")), "Commit the study source before Gibbs sampling.")
  runtime <- mgc_runtime(commit)
  mpr_require(is.finite(workers) && workers == as.integer(workers) && workers >= 1L &&
    workers <= cfg$maximum_workers && workers <= parallelly::availableCores(), "Invalid worker count.")
  resources <- mgc_resources(cfg, workers)
  mpr_require(length(cfg$initial_midpoint_offsets_sd) == cfg$chains &&
    length(cfg$initial_halfwidth_multipliers) == cfg$chains && cfg$thin == 1L &&
    all(cfg$initial_halfwidth_multipliers > 0), "Invalid chain settings.")
  lock <- mgc_lock(output, recover = resume)
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  mpr_require(!dir.exists(output) || resume, "Use a fresh study directory or explicitly resume.")
  external <- Sys.getenv("EXDQLM_RQR_REPO",
    "/data/muscat_data/jaguir26/exdqlm__wt__qdesn_0p4p0_integration")
  before <- rqr_capture_external_checkout(external)
  external_checked <- FALSE
  on.exit(if (!external_checked) rqr_assert_external_checkout_unchanged(before), add = TRUE)
  mpr_require(identical(before$commit, cfg$exdqlm_reference_commit), "External reference commit changed.")
  context <- mgc_context(cfg); context$runtime <- runtime
  if (stage == "full") mgc_check_diagnostic(cfg, commit, runtime, diagnostic_dir,
    mgc_design(cfg, "diagnostic", context))
  identity <- mgc_identity(cfg, jobs, commit, runtime); budget <- mgc_budget(cfg, stage)
  dir.create(file.path(output, "chains"), recursive = TRUE, showWarnings = FALSE)
  manifest_path <- file.path(output, "manifest.json")
  if (file.exists(manifest_path)) {
    manifest <- mgc_json(manifest_path)
    mpr_require(mgc_match_identity(manifest$identity, identity) && identical(manifest$stage, stage),
      "The requested resume changes the frozen study.")
  } else {
    pma_write_csv(jobs, file.path(output, "jobs.csv")); mgc_atomic_rds(jobs, file.path(output, "jobs.rds"))
    manifest <- list(identity = identity, commit = commit, stage = stage, workers = workers,
      threads_per_worker = 1, chains = nrow(jobs), datasets = length(unique(jobs$dataset_id)),
      settings = cfg, budget = budget, runtime = runtime, session = capture.output(sessionInfo()),
      resources = resources,
      rng = RNGkind(), external_reference = before, external_guard_passed = FALSE,
      exdqlm_loaded = FALSE, inputs = mgc_inputs(cfg))
    pta_atomic_write_json(manifest, manifest_path)
  }
  pending <- integer()
  for (i in seq_len(nrow(jobs))) {
    path <- mgc_chain_path(output, jobs$job_id[i])
    if (file.exists(path) && file.exists(paste0(path, ".json"))) {
      mgc_receipt(path, jobs[i, , drop = FALSE], budget)
    } else pending <- c(pending, i)
  }
  mgc_storage_check(cfg, length(pending), output, stage)
  started <- Sys.time()
  execution <- list(workers = workers, pid = Sys.getpid(), token = mgc_pid_token(Sys.getpid()),
    started_utc = mgc_time(), status = "running", initial_pending = length(pending))
  pta_atomic_write_json(execution, file.path(output, "execution.json")); finished <- FALSE
  on.exit({
    execution$status <- if (finished) "completed" else "stopped_with_error"
    execution$finished_utc <- mgc_time()
    execution$wall_seconds <- as.numeric(difftime(Sys.time(), started, units = "secs"))
    pta_atomic_write_json(execution, file.path(output, "execution.json"))
  }, add = TRUE)
  pending <- pending[order(-jobs$n[pending], jobs$dataset_id[pending], jobs$chain[pending])]
  batches <- split(pending, ceiling(seq_along(pending) / cfg$chain_batch_size)); passed <- TRUE
  for (indices in batches) {
    mgc_storage_check(cfg, length(pending), output, stage)
    result <- parallel::mclapply(indices, function(i)
      mgc_chain(cfg, jobs[i, , drop = FALSE], context, budget, output, stage),
      mc.cores = workers, mc.preschedule = FALSE, mc.set.seed = FALSE)
    passed <- passed && all(vapply(result, isTRUE, logical(1)))
    pending <- setdiff(pending, indices)
    mgc_assess(cfg, output, workers = min(workers, cfg$assessment_workers),
      dataset_ids = unique(jobs$dataset_id[indices]), jobs = jobs)
    pta_atomic_write_json(list(finished_utc = mgc_time(), remaining_queued = length(pending),
      batch_success = all(vapply(result, isTRUE, logical(1)))), file.path(output, "progress.json"))
    cat(sprintf("[%s] %s: %d queued chains remain.\n", mgc_time(), stage, length(pending)))
    flush.console()
  }
  mpr_require(passed, "Some chains failed; error records and completed work are preserved.")
  rqr_assert_external_checkout_unchanged(before)
  manifest$external_guard_passed <- TRUE; pta_atomic_write_json(manifest, manifest_path)
  mgc_assess(cfg, output, workers = min(workers, cfg$assessment_workers), complete_required = TRUE)
  external_checked <- TRUE
  finished <- TRUE
  invisible(mgc_health(output))
}
mgc_health <- function(output) {
  jobs <- mgc_jobs(output); manifest <- mgc_json(file.path(output, "manifest.json"))
  receipts <- lapply(seq_len(nrow(jobs)), function(i) {
    path <- mgc_chain_path(output, jobs$job_id[i])
    if (!file.exists(paste0(path, ".json"))) return(NULL)
    tryCatch(mgc_receipt(path, jobs[i, , drop = FALSE], manifest$budget, verify = FALSE),
      error = function(e) NULL)
  })
  good <- !vapply(receipts, is.null, logical(1))
  errors <- vapply(jobs$job_id, function(id)
    file.exists(paste0(mgc_chain_path(output, id), ".error.json")), logical(1)) & !good
  execution_path <- file.path(output, "execution.json")
  execution <- if (file.exists(execution_path)) mgc_json(execution_path) else list()
  workers <- if (!is.null(execution$workers)) execution$workers else manifest$workers
  hours <- c(NA_real_, NA_real_, NA_real_)
  if (any(good)) {
    times <- vapply(receipts[good], "[[", numeric(1), "elapsed_seconds")
    strata <- paste(jobs$source, jobs$n)
    estimates <- lapply(unique(strata[!good]), function(s) {
      t <- times[strata[good] == s]
      if (!length(t)) return(rep(NA_real_, 3))
      sum(!good & strata == s) * c(mean(t), quantile(t, .1), quantile(t, .9)) / workers / 3600
    })
    if (!length(estimates)) hours <- c(0, 0, 0) else {
      mat <- do.call(rbind, estimates)
      if (all(is.finite(mat))) hours <- colSums(mat)
    }
  }
  assessment_path <- file.path(output, "assessment.json")
  assessment <- if (file.exists(assessment_path)) mgc_json(assessment_path) else list()
  data.frame(completed_chains = sum(good), expected_chains = nrow(jobs),
    remaining_chains = sum(!good), failed_chains = sum(errors),
    completed_datasets = sum(table(jobs$dataset_id[good]) == manifest$settings$chains),
    assessed_datasets = length(list.files(file.path(output, "assessments"), pattern = "[.]rds$")),
    controller_alive = !is.null(execution$pid) && mgc_alive(execution$pid, execution$token),
    status = if (is.null(execution$status)) "not_started" else execution$status,
    estimated_remaining_hours = hours[1], estimated_hours_low = hours[2], estimated_hours_high = hours[3],
    assessment_complete = isTRUE(assessment$complete), diagnostics_pass = isTRUE(assessment$passed),
    available_disk_gib = mgc_free_bytes(output) / 1024^3)
}
mgc_smoke <- function(cfg, output) {
  runtime <- mgc_runtime(); dir.create(output, recursive = TRUE, showWarnings = FALSE)
  cases <- expand.grid(n = c(50L, 1000L), q = c(.95, .9999), delta = c(0, .2))
  run_one <- function(i) {
    y <- qnorm((seq_len(cases$n[i]) - .5) / cases$n[i])
    task <- data.frame(chain = 1L, q = cases$q[i], delta = cases$delta[i],
      ecm_lower = -2, ecm_upper = 2, seed = 1800000000L + i)
    fit <- mgc_fit(cfg, task, y, list(n_burn = 20L, n_mcmc = 40L), trace = TRUE, runtime = runtime)
    a <- as.numeric(fit$samp.beta_root1); b <- as.numeric(fit$samp.beta_root2)
    loss <- vapply(seq_along(a), function(k) {
      e <- (y - a[k]) * (y - b[k])
      sum(e * (task$q - (e < 0)) - task$q * task$delta * (a[k] + b[k] - 2 * y))
    }, numeric(1))
    mpr_require(all(is.finite(a)) && all(is.finite(b)) && fit$model_spec$numerical_repair_count == 0L &&
      isTRUE(fit$provenance$reproducibility_eligible) &&
      isTRUE(all.equal(loss, tail(fit$diagnostics$mean_tilted_target_loss_trace, 40L), tolerance = 1e-9)) &&
      nrow(fit$diagnostics$warmup_root1) == 20L, "Sampler smoke test failed.")
    if (i == 1L) {
      without_trace <- mgc_fit(cfg, task, y, list(n_burn = 20L, n_mcmc = 40L), runtime = runtime)
      mpr_require(identical(fit$samp.beta_root1, without_trace$samp.beta_root1) &&
        identical(fit$samp.beta_root2, without_trace$samp.beta_root2),
        "Warmup trace storage changed the Gibbs draws.")
    }
    list(draws = cbind(a, b), warmup = fit$diagnostics$warmup_root1)
  }
  serial <- lapply(seq_len(nrow(cases)), run_one)
  concurrent <- parallel::mclapply(seq_len(nrow(cases)), run_one, mc.cores = 2L, mc.set.seed = FALSE)
  mpr_require(identical(serial, concurrent), "Serial and parallel seeds produce different draws.")
  pta_atomic_write_json(list(passed = TRUE, cases = nrow(cases), commit = pma_git("rev-parse", "HEAD"),
    numerical_repairs = 0, scheduling_invariant = TRUE, time_utc = mgc_time()), file.path(output, "smoke.json"))
}
mgc_report <- function(output) {
  jobs <- mgc_jobs(output); manifest <- mgc_json(file.path(output, "manifest.json"))
  summaries <- pta_read_csv(file.path(output, "posterior_summary.csv"))
  path <- file.path(output, "gibbs_convergence_report.pdf")
  grDevices::pdf(path, width = 11, height = 8.5, onefile = TRUE)
  on.exit(grDevices::dev.off(), add = TRUE)
  for (id in unique(summaries$dataset_id)) {
    s <- jobs[jobs$dataset_id == id, ]
    fits <- lapply(s$job_id, function(i) readRDS(mgc_chain_path(output, i)))
    par(mfrow = c(2, 2), mar = c(4, 4, 3, 1), oma = c(0, 0, 2, 0))
    for (variable in c("lower", "upper", "width", "midpoint")) {
      traces <- lapply(fits, function(f) {
        d <- f$draws
        retained <- switch(variable, lower = d[, "lower"], upper = d[, "upper"],
          width = d[, "upper"] - d[, "lower"], midpoint = rowMeans(d))
        if (is.null(f$warmup_root1)) return(retained)
        a <- as.numeric(f$warmup_root1); b <- as.numeric(f$warmup_root2)
        c(switch(variable, lower = pmin(a, b), upper = pmax(a, b),
          width = abs(b - a), midpoint = (a + b) / 2), retained)
      })
      matplot(do.call(cbind, traces), type = "l", lty = 1,
        col = c("#0072B2", "#D55E00", "#009E73", "#CC79A7"),
        xlab = "Gibbs cycle", ylab = variable, main = variable)
      if (!is.null(fits[[1]]$warmup_root1)) abline(v = manifest$budget$n_burn, col = "gray40", lty = 2)
      value <- summaries[summaries$dataset_id == id & summaries$variable == variable, ]
      abline(h = value$ecm_value, lty = 3)
      mtext(sprintf("R-hat %.3f; bulk ESS %.0f; tail ESS %.0f", value$rhat,
        value$ess_bulk, value$ess_tail), side = 3, line = .1, cex = .7)
    }
    mtext(sprintf("%s; n=%d; c=%.2f; q=%.6f; tilt=%.5f", s$case_id[1], s$n[1],
      s$content[1], s$q[1], s$delta[1]), outer = TRUE, cex = .9)
    par(mfrow = c(2, 2), mar = c(4, 4, 3, 1), oma = c(0, 0, 2, 0))
    objectives <- lapply(fits, function(f) {
      if (is.null(f$target_loss_trace)) return(NULL)
      a <- c(as.numeric(f$warmup_root1), f$draws[, "lower"])
      b <- c(as.numeric(f$warmup_root2), f$draws[, "upper"])
      manifest$settings$learning_rate * f$target_loss_trace +
        (a^2 + b^2) / (2 * manifest$settings$prior_variance)
    })
    if (all(vapply(objectives, is.null, logical(1)))) {
      plot.new(); title("Objective traces not retained")
    } else {
      matplot(do.call(cbind, objectives), type = "l", lty = 1,
        col = c("#0072B2", "#D55E00", "#009E73", "#CC79A7"),
        xlab = "Gibbs cycle", ylab = "Negative log posterior", main = "Posterior objective")
      abline(v = manifest$budget$n_burn, col = "gray40", lty = 2)
    }
    widths <- vapply(fits, function(f) f$draws[, "upper"] - f$draws[, "lower"],
      numeric(manifest$budget$n_mcmc))
    correlations <- apply(widths, 2, function(x)
      as.numeric(acf(x, lag.max = 60, plot = FALSE)$acf))
    matplot(0:60, correlations, type = "l", lty = 1,
      col = c("#0072B2", "#D55E00", "#009E73", "#CC79A7"),
      xlab = "Lag", ylab = "Autocorrelation", main = "Retained width")
    ranks <- matrix(rank(as.vector(widths)), nrow = nrow(widths))
    boxplot(as.data.frame(ranks), names = paste("Chain", seq_len(ncol(ranks))),
      xlab = "", ylab = "Pooled rank", main = "Width ranks by chain")
    densities <- lapply(seq_len(ncol(widths)), function(i) density(widths[, i]))
    plot(densities[[1]], xlim = range(widths), ylim = c(0, max(vapply(densities,
      function(d) max(d$y), numeric(1)))), xlab = "Width", ylab = "Density", main = "Retained width density")
    for (i in seq_along(densities)) lines(densities[[i]],
      col = c("#0072B2", "#D55E00", "#009E73", "#CC79A7")[i])
    abline(v = s$ecm_upper[1] - s$ecm_lower[1], lty = 3)
    mtext(s$case_id[1], outer = TRUE, cex = .9)
  }
  invisible(path)
}

mgc_campaign <- function(cfg, output, allow_full = FALSE, maximum_workers = 50L) {
  mpr_require(maximum_workers >= 1L && maximum_workers <= cfg$maximum_workers,
    "Requested worker limit is outside the study limit.")
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  status <- function(value, ...) pta_atomic_write_json(c(list(status = value,
    commit = pma_git("rev-parse", "HEAD"), time_utc = mgc_time()), list(...)),
    file.path(output, "campaign_status.json"))
  status("sampler_smoke")
  mgc_smoke(cfg, file.path(output, "smoke"))
  context <- mgc_context(cfg)
  jobs <- mgc_design(cfg, "benchmark", context)
  counts <- cfg$benchmark_workers[cfg$benchmark_workers <=
    min(maximum_workers, parallelly::availableCores())]
  mpr_require(length(counts) > 0, "No benchmark worker count fits the available allocation.")
  timings <- lapply(counts, function(workers) {
    status("benchmark", workers = workers)
    directory <- file.path(output, paste0("benchmark_", workers))
    mgc_run(cfg, jobs, "benchmark", directory, workers, resume = dir.exists(directory))
    execution <- mgc_json(file.path(directory, "execution.json"))
    data.frame(workers = workers, wall_seconds = execution$wall_seconds,
      chains = nrow(jobs), chains_per_hour = nrow(jobs) / execution$wall_seconds * 3600)
  })
  timings <- do.call(rbind, timings)
  pma_write_csv(timings, file.path(output, "worker_benchmark.csv"))
  workers <- timings$workers[which.max(timings$chains_per_hour)]
  status("diagnostic", workers = workers)
  jobs <- mgc_design(cfg, "diagnostic", context)
  diagnostic <- file.path(output, "diagnostic")
  mgc_run(cfg, jobs, "diagnostic", diagnostic, workers, resume = dir.exists(diagnostic))
  mgc_report(diagnostic)
  assessment <- mgc_json(file.path(diagnostic, "assessment.json"))
  if (!isTRUE(assessment$passed)) {
    status("diagnostics_not_passed", workers = workers, datasets = assessment$datasets,
      report = file.path(diagnostic, "gibbs_convergence_report.pdf"))
    return(2L)
  }
  if (!allow_full) {
    status("diagnostic_complete_full_not_authorized", workers = workers)
    return(0L)
  }
  status("full_comparison", workers = workers)
  jobs <- mgc_design(cfg, "full", context)
  full <- file.path(output, "full")
  mgc_run(cfg, jobs, "full", full, workers, resume = dir.exists(full), diagnostic_dir = diagnostic)
  result <- mgc_json(file.path(full, "assessment.json"))
  status(if (isTRUE(result$passed)) "completed" else "completed_with_diagnostic_failures",
    workers = workers, datasets = result$datasets)
  if (isTRUE(result$passed)) 0L else 2L
}
