#!/usr/bin/env Rscript

# Development-only transition comparison for the hard M01 local-level
# component-scale cases.  This is computational transition diagnosis, not
# promotion evidence and not a main simulation launch.

args <- commandArgs(trailingOnly = TRUE)
repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(repo_root, "application", "DESCRIPTION"))) {
  stop("Run this script from the RQR-GIBBS repository root.", call. = FALSE)
}

parse_arg <- function(name, default = NULL) {
  prefix <- paste0("--", name, "=")
  matched <- grep(paste0("^", prefix), args, value = TRUE)
  if (!length(matched)) return(default)
  sub(prefix, "", matched[[length(matched)]])
}

if (any(args %in% c("-h", "--help"))) {
  cat(paste(
    "Usage:",
    "  Rscript application/scripts/27_compare_rqr_dlm_transition_kernels.R",
    "    [--output-root=application/cache/rqr_dlm_transition_comparison_20260727]",
    "",
    "The script fits only S03 replications 13, 94, and guard 55 for M01.",
    "It compares fixed target-preserving transition schedules and exits",
    "nonzero only for fit/artifact failures, not for diagnostic failures.",
    sep = "\n"
  ))
  quit(status = 0L)
}

output_root <- parse_arg(
  "output-root",
  file.path(repo_root, "application", "cache",
            "rqr_dlm_transition_comparison_20260727")
)
output_root <- normalizePath(output_root, winslash = "/", mustWork = FALSE)
if (file.exists(output_root) || dir.exists(output_root)) {
  stop(
    sprintf("The transition-comparison output root must be new: %s",
            output_root),
    call. = FALSE
  )
}
dir.create(output_root, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  if (requireNamespace("pkgload", quietly = TRUE)) {
    pkgload::load_all(file.path(repo_root, "application"), quiet = TRUE)
  } else {
    library(rqrgibbs)
  }
})
source(file.path(
  repo_root, "application", "scripts", "lib",
  "rqr_dlm_confirmatory_simulation.R"
))

git_value <- function(arguments) {
  output <- system2(
    Sys.which("git"), c("-C", shQuote(repo_root), arguments),
    stdout = TRUE, stderr = TRUE,
    env = c("GIT_OPTIONAL_LOCKS=0", "GIT_TERMINAL_PROMPT=0")
  )
  status <- attr(output, "status") %||% 0L
  if (!identical(as.integer(status), 0L)) {
    stop("Could not read the primary Git state.", call. = FALSE)
  }
  paste(output, collapse = "\n")
}

source_commit <- tolower(git_value(c("rev-parse", "HEAD")))
source_status <- git_value(c(
  "status", "--porcelain=v2", "--untracked-files=all"
))

contract <- rqr_confirm_read_contract(repo_root)
rqr_confirm_validate_contract(contract, require_closed = TRUE)
ledger <- rqr_confirm_seed_ledger(contract, planning = "maximum")

base_schedule <- contract$config$schedules$
  dynamic_rqr_component_scale_standard
increased_schedule <- list(burn = 1500L, retain = 9000L, thin = 1L)
candidates <- data.frame(
  candidate_id = c(
    "current_rootwise1_ASIS2",
    "rootwise2_ASIS1",
    "rootwise2_ASIS2",
    "ASIS1_then_rootwise2",
    "current_rootwise1_ASIS2_uniform9000"
  ),
  candidate_order = seq_len(5L),
  collapsed_cycles = c(1L, 2L, 2L, 2L, 1L),
  interweave_cycles = c(2L, 1L, 2L, 1L, 2L),
  transition_order = c(
    "rootwise_then_interweave",
    "rootwise_then_interweave",
    "rootwise_then_interweave",
    "interweave_then_rootwise",
    "rootwise_then_interweave"
  ),
  burn = c(
    base_schedule$burn, base_schedule$burn, base_schedule$burn,
    base_schedule$burn, increased_schedule$burn
  ),
  retain = c(
    base_schedule$retain, base_schedule$retain, base_schedule$retain,
    base_schedule$retain, increased_schedule$retain
  ),
  thin = c(
    base_schedule$thin, base_schedule$thin, base_schedule$thin,
    base_schedule$thin, increased_schedule$thin
  ),
  role = c(
    "current_failed_candidate_replicated",
    "repeat_full_rootwise_composition",
    "repeat_full_rootwise_composition_and_two_ASIS",
    "test_transition_order_ASIS_before_rootwise",
    "uniform_schedule_diagnostic_comparator"
  ),
  target_change = FALSE,
  threshold_change = FALSE,
  adaptive_extension = FALSE,
  stringsAsFactors = FALSE
)

replications <- data.frame(
  DGP = "S03",
  replication = c(13L, 94L, 55L),
  case_role = c("persistent_hard", "persistent_hard", "repaired_guard"),
  stringsAsFactors = FALSE
)

jobs <- merge(candidates, replications, by = NULL, sort = FALSE)
jobs$job_id <- sprintf(
  "%02d_%s__%s_rep%04d",
  jobs$candidate_order, jobs$candidate_id, jobs$DGP, jobs$replication
)
jobs <- jobs[order(jobs$candidate_order, jobs$replication), ]
rownames(jobs) <- NULL

workers <- as.integer(Sys.getenv(
  "RQR_TRANSITION_COMPARISON_WORKERS", unset = "6"
))
if (is.na(workers) || workers < 1L || workers > nrow(jobs)) {
  stop("RQR_TRANSITION_COMPARISON_WORKERS is invalid.", call. = FALSE)
}

thread_variables <- c(
  "OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS",
  "VECLIB_MAXIMUM_THREADS", "NUMEXPR_NUM_THREADS"
)
thread_environment <- setNames(
  vapply(thread_variables, Sys.getenv, character(1L), unset = ""),
  thread_variables
)

started_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
results <- parallel::mclapply(
  seq_len(nrow(jobs)),
  mc.cores = workers,
  mc.preschedule = TRUE,
  FUN = function(index) {
    job <- jobs[index, , drop = FALSE]
    tryCatch({
      generated <- rqr_confirm_generate_dgp(
        contract, job$DGP[[1L]], job$replication[[1L]], ledger
      )
      override <- list(
        n_burn = job$burn[[1L]],
        n_mcmc = job$retain[[1L]],
        thin = job$thin[[1L]],
        component_scale_collapsed_update = TRUE,
        component_scale_collapsed_cycles = job$collapsed_cycles[[1L]],
        component_scale_interweave = TRUE,
        component_scale_interweave_cycles = job$interweave_cycles[[1L]],
        component_scale_transition_order = job$transition_order[[1L]]
      )
      fit_started <- proc.time()[["elapsed"]]
      value <- rqr_confirm_dynamic_fit(
        contract = contract,
        generated = generated,
        method = "M01",
        chain = 1L,
        ledger = ledger,
        provenance_control = list(),
        profile_name = "standard",
        mcmc_control_override = override
      )
      scalars <- rqr_confirm_scalar_draws(
        value, generated, contract, "M01"
      )
      diagnostics <- rqr_confirm_chain_diagnostics(
        list(scalars), contract = contract, sentinel = FALSE,
        method = "M01", generated = generated
      )
      diagnostics$DGP <- job$DGP[[1L]]
      diagnostics$replication <- job$replication[[1L]]
      diagnostics$sentinel <- FALSE
      list(
        ok = TRUE,
        job = as.list(job),
        fit_elapsed_seconds =
          as.numeric(proc.time()[["elapsed"]] - fit_started),
        diagnostics = diagnostics,
        scalars = scalars,
        component_scale_transition_kernel =
          value$fit$model_spec$component_scale_transition_kernel,
        root_swap_rate = mean(value$fit$diagnostics$root_swap_trace),
        numerical_repair_count =
          value$fit$model_spec$numerical_repair_count,
        exact_joint_target = value$fit$model_spec$exact_joint_target,
        peak_RSS_KiB = rqr_confirm_process_peak_rss_kib()
      )
    }, error = function(error) {
      list(
        ok = FALSE,
        job = as.list(job),
        error_class = class(error)[[1L]],
        message = conditionMessage(error),
        peak_RSS_KiB = rqr_confirm_process_peak_rss_kib()
      )
    })
  }
)
completed_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)

diagnostics <- do.call(rbind, lapply(results, function(result) {
  job <- result$job
  if (!isTRUE(result$ok)) {
    data.frame(
      candidate_id = job$candidate_id,
      candidate_order = job$candidate_order,
      DGP = job$DGP,
      replication = job$replication,
      case_role = job$case_role,
      estimand = "fit_error",
      chains = 1L,
      rhat = NA_real_,
      ess_bulk = NA_real_,
      ess_tail = NA_real_,
      mcse_mean = NA_real_,
      mcse_over_sd = NA_real_,
      pass = FALSE,
      stringsAsFactors = FALSE
    )
  } else {
    rows <- result$diagnostics
    rows$candidate_id <- job$candidate_id
    rows$candidate_order <- job$candidate_order
    rows$case_role <- job$case_role
    rows
  }
}))
diagnostics <- diagnostics[order(
  diagnostics$candidate_order, diagnostics$DGP,
  diagnostics$replication, diagnostics$estimand
), , drop = FALSE]

summary <- do.call(rbind, lapply(results, function(result) {
  job <- result$job
  rows <- diagnostics[
    diagnostics$candidate_id == job$candidate_id &
      diagnostics$DGP == job$DGP &
      diagnostics$replication == job$replication,
    ,
    drop = FALSE
  ]
  q_row <- rows[rows$estimand == "log_q_1", , drop = FALSE]
  loss_row <- rows[rows$estimand == "observed_loss", , drop = FALSE]
  data.frame(
    candidate_id = job$candidate_id,
    candidate_order = job$candidate_order,
    DGP = job$DGP,
    replication = job$replication,
    case_role = job$case_role,
    ok = isTRUE(result$ok),
    diagnostics = nrow(rows),
    diagnostics_passed = sum(rows$pass),
    all_pass = all(rows$pass),
    log_q_1_ess_bulk =
      if (nrow(q_row)) q_row$ess_bulk[[1L]] else NA_real_,
    log_q_1_ess_tail =
      if (nrow(q_row)) q_row$ess_tail[[1L]] else NA_real_,
    log_q_1_mcse_over_sd =
      if (nrow(q_row)) q_row$mcse_over_sd[[1L]] else NA_real_,
    observed_loss_ess_bulk =
      if (nrow(loss_row)) loss_row$ess_bulk[[1L]] else NA_real_,
    observed_loss_ess_tail =
      if (nrow(loss_row)) loss_row$ess_tail[[1L]] else NA_real_,
    observed_loss_mcse_over_sd =
      if (nrow(loss_row)) loss_row$mcse_over_sd[[1L]] else NA_real_,
    root_swap_rate = as.numeric(result$root_swap_rate %||% NA_real_),
    numerical_repair_count =
      as.integer(result$numerical_repair_count %||% NA_integer_),
    exact_joint_target =
      as.logical(result$exact_joint_target %||% NA),
    fit_elapsed_seconds =
      as.numeric(result$fit_elapsed_seconds %||% NA_real_),
    peak_RSS_KiB = as.numeric(result$peak_RSS_KiB %||% NA_real_),
    error_class = as.character(result$error_class %||% ""),
    message = as.character(result$message %||% ""),
    stringsAsFactors = FALSE
  )
}))
summary <- summary[order(
  summary$candidate_order, summary$DGP, summary$replication
), , drop = FALSE]

scalar_evidence <- lapply(results, function(result) {
  if (!isTRUE(result$ok)) return(result)
  list(
    ok = TRUE,
    job = result$job,
    fit_elapsed_seconds = result$fit_elapsed_seconds,
    diagnostics = result$diagnostics,
    scalars = result$scalars,
    component_scale_transition_kernel =
      result$component_scale_transition_kernel,
    root_swap_rate = result$root_swap_rate,
    numerical_repair_count = result$numerical_repair_count,
    exact_joint_target = result$exact_joint_target,
    peak_RSS_KiB = result$peak_RSS_KiB
  )
})

atomic_rds <- function(value, path) {
  temporary <- tempfile(paste0(".", basename(path), "-"), tmpdir = dirname(path))
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  saveRDS(value, temporary, version = 3L)
  if (!file.rename(temporary, path)) {
    stop("Could not publish the transition-comparison RDS.", call. = FALSE)
  }
  invisible(path)
}

rqr_confirm_atomic_write_csv(
  candidates, file.path(output_root, "transition_candidates.csv")
)
rqr_confirm_atomic_write_csv(
  jobs, file.path(output_root, "targeted_jobs.csv")
)
rqr_confirm_atomic_write_csv(
  diagnostics, file.path(output_root, "targeted_diagnostics.csv")
)
rqr_confirm_atomic_write_csv(
  summary, file.path(output_root, "targeted_summary.csv")
)
atomic_rds(
  list(candidates = candidates, jobs = jobs, results = scalar_evidence),
  file.path(output_root, "transition_comparison_scalar_evidence.rds")
)

manifest <- list(
  schema_version = "rqrgibbs_dlm_transition_comparison/1.0.0",
  source_commit = source_commit,
  source_clean = !nzchar(source_status),
  package_version = as.character(utils::packageVersion("rqrgibbs")),
  fit_schema_version = rqrgibbs:::.rqr_schema_version(),
  mode = "development_only_transition_comparison",
  DGP = "S03",
  replications = as.list(replications$replication),
  jobs = nrow(jobs),
  workers = workers,
  thread_environment = as.list(thread_environment),
  target_change = FALSE,
  threshold_change = FALSE,
  adaptive_extension = FALSE,
  main_simulation_launch = FALSE,
  confirmatory_execution_authorized =
    isTRUE(contract$config$confirmatory_execution_authorized),
  all_fits_succeeded = all(vapply(results, `[[`, logical(1L), "ok")),
  all_exact_joint_target = all(summary$exact_joint_target %in% TRUE),
  all_zero_repairs = all(summary$numerical_repair_count %in% 0L),
  all_diagnostics_passed = all(summary$all_pass),
  started_at_utc = started_at,
  completed_at_utc = completed_at
)
rqr_confirm_atomic_write_json(
  manifest, file.path(output_root, "transition_comparison_manifest.json")
)
hashes <- rqr_confirm_recursive_manifest(output_root)
rqr_confirm_atomic_write_csv(
  hashes, file.path(output_root, "artifact_hashes.csv")
)

cat(sprintf(
  paste0(
    "RQR-DLM transition comparison complete: %d jobs, %d/%d diagnostics ",
    "passed. Output root: %s\n"
  ),
  nrow(jobs), sum(diagnostics$pass), nrow(diagnostics), output_root
))
if (!isTRUE(manifest$all_fits_succeeded) ||
    !isTRUE(manifest$all_exact_joint_target) ||
    !isTRUE(manifest$all_zero_repairs)) {
  stop("The transition comparison had fit, target, or repair failures.",
       call. = FALSE)
}
