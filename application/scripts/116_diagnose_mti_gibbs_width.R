#!/usr/bin/env Rscript
options(warn = 1)
Sys.setenv(OMP_NUM_THREADS = 1, OPENBLAS_NUM_THREADS = 1, MKL_NUM_THREADS = 1,
  VECLIB_MAXIMUM_THREADS = 1, NUMEXPR_NUM_THREADS = 1)
for (file in c("mti_gibbs_practical", "mti_gibbs_full_report_review", "mti_gibbs_full_publication",
  "mti_profile_revision", "mti_gibbs_width_diagnosis", "mti_gibbs_width_diagnosis_figures"))
  source(file.path("application/scripts/lib", paste0(file, ".R")))
data.table::setDTthreads(1L)
args <- commandArgs(trailingOnly = TRUE)
config <- if (length(args)) args[1] else "application/config/mti_gibbs_width_diagnosis_20261113.json"
cfg <- mwd_json(config)
mwd_require(!cfg$promote_results && !cfg$new_tolerance_guarantee && cfg$chains == 4L,
  "This study is diagnostic only and requires four chains.")
mwd_require(!nzchar(mgfp_git("status", "--porcelain", "--", paste(mwd_programs, collapse = " "))),
  "Commit the diagnostic source before sampling.")
output <- cfg$output_dir; dir.create(output, recursive = TRUE, showWarnings = FALSE)
campaign <- file.path(mgfp_common(), cfg$campaign)
inputs <- file.path(campaign, c("dataset_summary.csv", "interval_comparison.csv", "jobs.rds", "manifest.json"))
application <- mwd_json(cfg$application_config)
inputs <- c(inputs, file.path(application$paths$canonical_dir,
  c("observed_batches.csv", "split_membership.csv.gz")))
identity <- list(source_commit = mgfp_git("rev-parse", "HEAD"), configuration = cfg,
  programs = mgfp_hashes(mwd_programs), inputs = mgfp_hashes(inputs))
manifest_path <- file.path(output, "manifest.json")
if (file.exists(manifest_path)) {
  mwd_require(identical(mwd_json(manifest_path)$identity_sha256, mwd_hash(identity)),
    "The diagnostic source or original numerical evidence changed.")
} else mgfp_json_write(list(identity_sha256 = mwd_hash(identity), identity = identity,
  started = mgfp_time(), R = R.version.string, new_tolerance_guarantee = FALSE), manifest_path)
original_manifest <- mwd_json(file.path(campaign, "manifest.json"))
mwd_require(identical(original_manifest$identity$commit, cfg$sampling_commit),
  "The original sampling commit differs.")
jobs <- readRDS(file.path(campaign, "jobs.rds"))
d <- data.table::fread(file.path(campaign, "dataset_summary.csv"), data.table = FALSE)
intervals <- data.table::fread(file.path(campaign, "interval_comparison.csv"), data.table = FALSE)
mwd_require(nrow(d) == 75004L && nrow(jobs) == 300016L, "The completed original study differs.")
context <- mwd_context(cfg, output)
cases <- mwd_cases(d, jobs, cfg)
samples <- lapply(seq_len(nrow(cases)), function(i) mwd_sample(cases[i, ], context))
mwd_write(cases, file.path(output, "selected_cases.csv"))
invisible(mwd_global(d, intervals, context, output))
original <- lapply(seq_len(nrow(cases)), function(i) {
  rows <- jobs[jobs$dataset_id == cases$dataset_id[i], ]
  lapply(seq_len(nrow(rows)), function(j) mgfp_read_chain(campaign, rows[j, , drop = FALSE], original_manifest))
})
variants <- do.call(rbind, lapply(seq_len(nrow(cases)), function(i) mwd_variants(cases[i, ], samples[[i]], cfg)))
mwd_write(variants, file.path(output, "diagnostic_specifications.csv"))
new_jobs <- list()
for (s in seq_len(nrow(variants))) for (chain in seq_len(cfg$chains)) {
  task <- cases[match(variants$case_id[s], cases$case_id), ]
  task$seed <- as.integer(cfg$seed_base + 1000L * s + chain); task$chain <- chain
  new_jobs[[length(new_jobs) + 1L]] <- list(task = task, variant = variants[s, ],
    sample_index = match(task$case_id, cases$case_id))
}
seeds <- vapply(new_jobs, function(j) j$task$seed, 0L)
mwd_require(!anyDuplicated(seeds) && !any(seeds %in% c(jobs$seed, jobs$data_seed)),
  "Diagnostic seeds overlap existing random streams.")
saveRDS(new_jobs, file.path(output, "jobs.rds"))
dir.create(file.path(output, "chains"), showWarnings = FALSE)
paths <- file.path(output, "chains", sprintf("chain_%04d.rds", seq_along(new_jobs)))
workers <- min(cfg$maximum_workers, parallelly::availableCores())
batches <- split(seq_along(new_jobs), ceiling(seq_along(new_jobs) / workers))
for (batch in batches) {
  outcomes <- parallel::mclapply(batch, function(j) {
    job <- new_jobs[[j]]; y <- samples[[job$sample_index]]
    tryCatch({
      if (file.exists(paste0(paths[j], ".json"))) mwd_read(paths[j], job, y) else
        mwd_chain(job, y, cfg, paths[j])
      list(ok = TRUE, job_id = j)
    }, error = function(e) list(ok = FALSE, job_id = j, error = conditionMessage(e)))
  }, mc.cores = workers, mc.set.seed = FALSE)
  bad <- Filter(function(x) !is.list(x) || !isTRUE(x$ok), outcomes)
  if (length(bad)) {
    saveRDS(bad, file.path(output, "failed_jobs.rds")); stop("Diagnostic chains failed; original results are unchanged.")
  }
  cat(sprintf("%s: checked %d / %d diagnostic chains\n", mgfp_time(), max(batch), length(new_jobs)))
  flush.console()
}
policy <- mwd_json(cfg$practical_config)$policy
fits <- list(); summaries <- parameters <- stein <- agreement <- list()
for (s in seq_len(nrow(variants))) {
  idx <- (s - 1L) * cfg$chains + seq_len(cfg$chains)
  v <- variants[s, ]; i <- match(v$case_id, cases$case_id); task <- cases[i, ]
  f <- lapply(idx, function(j) mwd_read(paths[j], new_jobs[[j]], samples[[i]]))
  fits[[paste(v$case_id, v$variant, sep = ":")]] <- f
  summaries[[s]] <- mwd_summarize(task, v, f, context, policy)
  task$q <- v$q; task$delta <- v$delta
  parameters[[s]] <- cbind(variant = v$variant, mgp_summaries(f, task, policy))
  stein[[s]] <- cbind(case_id = task$case_id, variant = v$variant,
    mwd_stein(samples[[i]], f, v$q, v$delta, v$omega, v$variance, v$center))
  if (v$variant == "prior_var_10000")
    agreement[[i]] <- mwd_old_agreement(task, original[[i]], f)
}
summaries <- do.call(rbind, summaries)
mwd_write(summaries, file.path(output, "sensitivity_summary.csv"))
mwd_write(do.call(rbind, parameters), file.path(output, "parameter_summary.csv"))
mwd_write(do.call(rbind, stein), file.path(output, "fresh_score_identities.csv"))
mwd_write(do.call(rbind, agreement), file.path(output, "original_fresh_agreement.csv"))
old_stein <- do.call(rbind, lapply(seq_len(nrow(cases)), function(i)
  cbind(case_id = cases$case_id[i], mwd_stein(samples[[i]], original[[i]], cases$q[i], cases$delta[i]))))
mwd_write(old_stein, file.path(output, "original_score_identities.csv"))
boundary <- parallel::mclapply(seq_len(nrow(cases)), function(i)
  mwd_boundary_probe(cases[i, ], samples[[i]], cfg), mc.cores = min(workers, nrow(cases)), mc.set.seed = FALSE)
mwd_require(!any(vapply(boundary, inherits, logical(1), "try-error")), "Conditional boundary experiment failed.")
mwd_write(do.call(rbind, boundary), file.path(output, "boundary_conditionals.csv"))
mwd_figures(cases, summaries, samples, fits, original, output)
mgfp_json_write(list(completed = mgfp_time(), cases = nrow(cases), specifications = nrow(variants),
  chains = length(new_jobs), all_preferred = all(summaries$preferred),
  maximum_rhat = max(summaries$maximum_rhat), minimum_bulk_ess = min(summaries$minimum_bulk_ess),
  minimum_tail_ess = min(summaries$minimum_tail_ess), identity_sha256 = mwd_hash(identity),
  new_tolerance_guarantee = FALSE, article_replaced = FALSE), file.path(output, "completed.json"))
cat("Diagnostic assessment complete:", normalizePath(output), "\n")
