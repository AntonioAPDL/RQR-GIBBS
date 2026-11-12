#!/usr/bin/env Rscript
arguments <- commandArgs(FALSE)
script <- sub("--file=", "", arguments[startsWith(arguments, "--file=")][1], fixed = TRUE)
setwd(normalizePath(file.path(dirname(script), "..", ".."), mustWork = TRUE))
Sys.setenv(OMP_NUM_THREADS = 1, OPENBLAS_NUM_THREADS = 1, MKL_NUM_THREADS = 1)
RNGkind("Mersenne-Twister", "Inversion", "Rejection")
for (file in c("pharma_tolerance_application.R", "pharma_mti_application.R",
    "mti_profile_revision.R", "mti_profile_review.R", "mti_final_reporting.R",
    "mti_ecm_recovery.R", "pinned_exdqlm_runtime.R", "mti_gibbs_comparison.R",
    "mti_gibbs_practical.R")) source(file.path("application/scripts/lib", file))
args <- commandArgs(TRUE)
cfg <- mgc_json(pta_arg_value(args, "--config=", "application/config/mti_gibbs_practical_20261112.json"))
base <- mgc_json(cfg$base_config)
mode <- pta_arg_value(args, "--mode=", "preflight")
common <- normalizePath(pma_git("rev-parse", "--git-common-dir"), mustWork = TRUE)
parent <- normalizePath(pta_arg_value(args, "--parent-dir=",
  file.path(dirname(common), cfg$parent_directory)), mustWork = TRUE)
output <- pta_arg_value(args, "--output-dir=", cfg$output_root)
workers <- as.integer(pta_arg_value(args, "--workers=", as.character(cfg$maximum_workers)))
if (mode == "health") {
  if (file.exists(file.path(output, "campaign_status.json"))) {
    status <- mgc_json(file.path(output, "campaign_status.json"))
    status$controller_alive <- mgc_alive(status$pid, status$pid_token)
    print(status)
  }
  for (name in c("continuations", "slice")) {
    directory <- file.path(output, name)
    if (!file.exists(file.path(directory, "jobs.rds"))) next
    jobs <- mgc_jobs(directory)
    receipts <- vapply(jobs$job_id, function(id)
      file.exists(paste0(mgc_chain_path(directory, id), ".json")), logical(1))
    errors <- vapply(jobs$job_id, function(id)
      file.exists(paste0(mgc_chain_path(directory, id), ".error.json")), logical(1)) & !receipts
    cat(sprintf("%s: %d / %d chains; %d remaining; %d error records\n", name,
      sum(receipts), length(receipts), sum(!receipts), sum(errors)))
    if (any(receipts)) {
      times <- vapply(jobs$job_id[receipts], function(id)
        mgc_json(paste0(mgc_chain_path(directory, id), ".json"))$elapsed_seconds, 0)
      cat(sprintf("Approximate remaining sampling hours at %d workers: %.2f (stage only).\n",
        workers, sum(!receipts) * mean(times) / workers / 3600))
    }
  }
  quit(status = 0)
}

parent_manifest <- mgc_json(file.path(parent, "manifest.json"))
parent_jobs <- mgc_jobs(parent)
stopifnot(identical(parent_manifest$identity$commit, cfg$parent_commit),
  identical(parent_manifest$identity$settings_sha256, mgc_hash(base)),
  identical(parent_manifest$identity$design_sha256, mgc_hash(parent_jobs)),
  identical(parent_manifest$identity$inputs_sha256, mgc_hash(mgc_inputs(base))))
invisible(rqr_assert_external_checkout_unchanged(parent_manifest$external_reference))
context <- mgc_context(base)
full <- mgc_design(base, "full", context)
jobs <- mgp_design(full, parent_jobs, cfg)
tasks <- jobs[!duplicated(jobs$dataset_id), ]
parent_tasks <- parent_jobs[!duplicated(parent_jobs$dataset_id), ]
parent_tasks$target_class <- vapply(seq_len(nrow(parent_tasks)), function(i) {
  t <- parent_tasks[i, ]; mgp_target_class(mgc_sample(t, context), t$q, t$delta)
}, "")
examples <- mgp_example_ids(parent_tasks, cfg$diagnostic_examples)
old <- read.csv(file.path(parent, "posterior_summary.csv"))
old$practical <- with(old, is.finite(rhat) & rhat <= cfg$policy$exploratory_rhat &
  ess_bulk >= cfg$policy$exploratory_ess & ess_tail >= cfg$policy$exploratory_ess &
  is.finite(mcse_mean / sd) & mcse_mean / sd <= cfg$policy$exploratory_mcse_ratio)
extend_ids <- as.integer(names(which(tapply(old$practical & !old$diagnostics_pass,
  old$dataset_id, all))))
extensions <- parent_jobs[parent_jobs$dataset_id %in% extend_ids, ]
cat(sprintf("Prepared %d balanced datasets plus %d stress datasets; %d chains; %d examples; %d original-chain extensions.\n",
  sum(tasks$diagnostic_role == "balanced"), sum(tasks$diagnostic_role == "stress"),
  nrow(jobs), length(examples), nrow(extensions)))
if (mode == "preflight") quit(status = 0)
stopifnot(mode %in% c("assess", "campaign"))
if (mode == "campaign") {
  mgc_authorized(pta_arg_value(args, "--confirm-article-commit=", ""), pma_git("rev-parse", "HEAD"))
  mpr_require(mpr_git_clean(pma_git("status", "--porcelain")), "Commit the source before sampling.")
}
stopifnot(workers >= 1L, workers <= cfg$maximum_workers)
invisible(mgc_resources(base, workers))
lock <- mgc_lock(output, recover = identical(pta_arg_value(args, "--recover-lock=", "false"), "true"))

campaign <- function() {
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  on.exit(invisible(rqr_assert_external_checkout_unchanged(parent_manifest$external_reference)), add = TRUE)
  commit <- pma_git("rev-parse", "HEAD")
  runtime <- mgc_runtime(cfg$parent_commit)
  # The old installed runtime is used only for exact original-kernel continuation.
  identity <- list(commit = commit, settings_sha256 = mgc_hash(cfg),
    base_sha256 = mgc_hash(base), parent_manifest_sha256 = mgc_file_hash(file.path(parent, "manifest.json")),
    parent_design_sha256 = mgc_hash(parent_jobs), design_sha256 = mgc_hash(jobs),
    inputs_sha256 = mgc_hash(mgc_inputs(base)), continuation_runtime_sha256 = runtime$sha256)
  manifest_path <- file.path(output, "manifest.json")
  if (file.exists(manifest_path)) {
    existing <- mgc_json(manifest_path)
    stopifnot(mgc_match_identity(existing$identity, identity))
  } else pta_atomic_write_json(list(identity = identity, configuration = cfg,
    original_configuration = base, parent_directory = parent, workers = workers,
    original_sampler_commit = cfg$parent_commit, marginal_sampler_commit = commit,
    sampler = cfg$sampler, created_utc = mgc_time(), session = capture.output(sessionInfo()),
    resources = mgc_resources(base, workers), external_reference = parent_manifest$external_reference,
    expected_datasets = nrow(tasks), balanced_datasets = sum(tasks$diagnostic_role == "balanced"),
    chains = nrow(jobs), new_tolerance_guarantee = FALSE, article_replaced = FALSE), manifest_path)
  pma_write_csv(tasks, file.path(output, "design.csv"))
  pma_write_csv(parent_tasks[parent_tasks$dataset_id %in% examples, ], file.path(output, "diagnostic_examples.csv"))
  status <- function(state, ...) pta_atomic_write_json(c(list(status = state, pid = Sys.getpid(),
    pid_token = mgc_pid_token(Sys.getpid()), commit = commit, time_utc = mgc_time(),
    workers = workers), list(...)), file.path(output, "campaign_status.json"))
  status("assessing_saved_original_chains")
  parent_summary <- mgp_assess(parent_jobs,
    function(t) mgp_read(parent, t, list(n_mcmc = base$n_mcmc)), cfg,
    file.path(output, "original_assessment"), workers)
  if (mode == "assess") { status("saved_draw_assessment_complete"); return(invisible(NULL)) }
  mgp_continuation_check(base, runtime, output)
  deadline <- as.numeric(Sys.time()) + cfg$sampling_time_limit_seconds
  for (name in c("continuations", "slice")) {
    directory <- file.path(output, name)
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
    design <- if (name == "slice") jobs else extensions
    path <- file.path(directory, "jobs.rds")
    if (file.exists(path)) stopifnot(identical(mgc_hash(mgc_jobs(directory)), mgc_hash(design))) else
      mgc_atomic_rds(design, path)
    pta_atomic_write_json(list(n_mcmc = if (name == "slice") base$n_mcmc else cfg$continuation_draws),
      file.path(directory, "budget.json"))
  }
  mgc_storage_check(base, nrow(jobs) + nrow(extensions), output, "diagnostic")
  status("bounded_original_continuations", expected_chains = nrow(extensions))
  mgp_queue(extensions, file.path(output, "continuations"), cfg, base, context,
    workers, deadline, identity, "extension", parent, runtime)
  if (nrow(extensions)) mgp_assess(extensions, function(t) {
    extension <- mgp_read(file.path(output, "continuations"), t,
      list(n_mcmc = cfg$continuation_draws))
    if (is.null(extension)) return(NULL)
    original <- mgp_read(parent, t, list(n_mcmc = base$n_mcmc))
    stopifnot(identical(extension$parent_sha256, mgc_file_hash(mgc_chain_path(parent, t$job_id))))
    list(draws = rbind(original$draws, extension$draws))
  }, cfg, file.path(output, "continuation_assessment"), workers)
  if (as.numeric(Sys.time()) >= deadline) {
    status("sampling_budget_reached_after_continuations"); return(invisible(NULL))
  }

  status("independent_conditional_references", datasets = length(examples))
  reference_rows <- parallel::mclapply(examples, function(id) {
    t <- parent_tasks[parent_tasks$dataset_id == id, ]
    y <- mgc_sample(t, context)
    rows <- lapply(seq_len(2L), function(side) {
      other <- c(t$ecm_lower, t$ecm_upper)[side]
      reference <- mgp_conditional_reference(y, other, t$q, t$delta, base)
      conditional <- mgp_conditional(y, other, t$q, t$delta, base$learning_rate, 1 / base$prior_variance)
      set.seed(base$seed_base + cfg$seed_offset + 2L * id + side)
      current <- reference$mode
      draws <- numeric(cfg$conditional_reference_draws + cfg$conditional_reference_warmup)
      for (i in seq_along(draws)) draws[i] <- current <- mgp_slice_update(current, conditional)
      draws <- tail(draws, cfg$conditional_reference_draws)
      probabilities <- c(.025, .5, .975)
      probabilities_at_quantiles <- vapply(quantile(draws, probabilities), reference$cdf, 0)
      data.frame(dataset_id = id, case_id = t$case_id, other_root = other,
        probability = probabilities, reference_probability = probabilities_at_quantiles,
        difference = probabilities_at_quantiles - probabilities)
    })
    do.call(rbind, rows)
  }, mc.cores = min(workers, 12L), mc.set.seed = FALSE)
  stopifnot(!any(vapply(reference_rows, inherits, logical(1), "try-error")))
  references <- do.call(rbind, reference_rows)
  pma_write_csv(references, file.path(output, "independent_conditional_reference.csv"))
  # This bounds gross reference discrepancies, rather than claiming exact Monte Carlo equality.
  stopifnot(all(is.finite(references$difference)),
    max(abs(references$difference)) <= cfg$conditional_reference_cdf_tolerance)
  status("marginal_slice_diagnostic_examples", datasets = length(examples))
  directory <- file.path(output, "slice")
  mgp_queue(jobs[jobs$dataset_id %in% examples, ], directory, cfg, base, context,
    workers, deadline, identity, "slice", parent, runtime)
  pilot <- jobs[jobs$dataset_id %in% examples, ]
  pilot_errors <- vapply(pilot$job_id, function(id)
    file.exists(paste0(mgc_chain_path(directory, id), ".error.json")), logical(1))
  if (any(pilot_errors)) {
    status("diagnostic_example_numerical_errors", errors = sum(pilot_errors)); return(invisible(NULL))
  }
  status("balanced_and_stress_comparison", expected_datasets = nrow(tasks), expected_chains = nrow(jobs))
  mgp_queue(jobs, directory, cfg, base, context, workers, deadline, identity, "slice", parent, runtime)
  status("assessing_marginal_slice")
  d <- mgp_assess(jobs, function(t) mgp_read(directory, t, list(n_mcmc = base$n_mcmc)),
    cfg, directory, workers)
  if (nrow(d)) {
    d$diagnostic_role <- jobs$diagnostic_role[match(d$dataset_id, jobs$dataset_id)]
    d$sampler <- cfg$sampler
    pma_write_csv(d, file.path(directory, "parameter_summary.csv"))
    w <- d[d$variable == "width", ]
    w$posterior_mean_ecm_ratio <- w$mean / w$ecm_value
    pma_write_csv(w, file.path(output, "width_comparison.csv"))
    mgp_report(directory, examples, file.path(output, "marginal_slice_examples.pdf"))
  }
  done <- if (nrow(d)) length(unique(d$dataset_id)) else 0L
  datasets <- if (done) read.csv(file.path(directory, "dataset_summary.csv")) else data.frame()
  status(if (done == nrow(tasks)) "calculation_complete_review_required" else "bounded_run_incomplete",
    completed_datasets = done, expected_datasets = nrow(tasks), completed_chains = 4L * done,
    expected_chains = nrow(jobs), screen_pass_datasets = if (done) sum(datasets$screen_pass) else 0L,
    adequate_for_recorded_summaries = if (done)
      sum(datasets$status == "adequate_for_recorded_summaries") else 0L,
    new_tolerance_guarantee = FALSE, article_replaced = FALSE)
}
tryCatch(campaign(), error = function(e) {
  pta_atomic_write_json(list(status = "stopped_with_error", error = conditionMessage(e),
    pid = Sys.getpid(), pid_token = mgc_pid_token(Sys.getpid()), time_utc = mgc_time()),
    file.path(output, "campaign_status.json"))
  stop(e)
})
