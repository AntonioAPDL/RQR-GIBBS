#!/usr/bin/env Rscript

# Development-only comparison for the skewed-wave RQR-DLM MCMC failures.
# The candidate set, hard cells, guards, thresholds, seeds, and selection rule
# are fixed below.  This script cannot authorize or launch a confirmatory run.

args <- commandArgs(trailingOnly = TRUE)
parse_arg <- function(name, default = NULL) {
  prefix <- paste0("--", name, "=")
  value <- grep(paste0("^", prefix), args, value = TRUE)
  if (!length(value)) return(default)
  sub(prefix, "", value[[length(value)]], fixed = TRUE)
}
if (any(args %in% c("-h", "--help"))) {
  cat(paste(
    "Usage: 47_compare_rqr_dlm_skewed_failure_candidates.R",
    "  --mode=<preflight|execute>",
    "  --seed-ledger=<reviewed seed_ledger_maximum.csv>",
    "  --exdqlm-attestation=<attested CRAN 1.1.0 runtime JSON>",
    "  --output-root=<fresh ignored directory>",
    "  [--workers=8]",
    sep = "\n"
  ), "\n")
  quit(save = "no", status = 0L, runLast = FALSE)
}

repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(repo_root, "application", "DESCRIPTION"))) {
  stop("Run this script from the RQR-GIBBS repository root.", call. = FALSE)
}
mode <- parse_arg("mode", "preflight")
if (!mode %in% c("preflight", "execute")) {
  stop("--mode must be preflight or execute.", call. = FALSE)
}
seed_ledger_path <- parse_arg("seed-ledger", "")
exdqlm_attestation_path <- parse_arg("exdqlm-attestation", "")
output_root <- parse_arg("output-root", "")
if (!nzchar(seed_ledger_path) || !file.exists(seed_ledger_path) ||
    !nzchar(exdqlm_attestation_path) ||
    !file.exists(exdqlm_attestation_path) ||
    !nzchar(output_root)) {
  stop("The seed ledger, exdqlm attestation, and output root are required.",
       call. = FALSE)
}
seed_ledger_path <- normalizePath(
  seed_ledger_path, winslash = "/", mustWork = TRUE
)
exdqlm_attestation_path <- normalizePath(
  exdqlm_attestation_path, winslash = "/", mustWork = TRUE
)
output_root <- normalizePath(output_root, winslash = "/", mustWork = FALSE)
workers <- suppressWarnings(as.integer(parse_arg("workers", "8")))
if (length(workers) != 1L || is.na(workers) || workers < 1L ||
    workers > 12L) {
  stop("--workers must be one integer in [1, 12].", call. = FALSE)
}

suppressPackageStartupMessages({
  pkgload::load_all(file.path(repo_root, "application"), quiet = FALSE)
})
source(file.path(
  repo_root, "application", "scripts", "lib",
  "rqr_dlm_confirmatory_simulation.R"
))

git_read <- function(arguments) {
  value <- system2(
    Sys.which("git"), c("-C", shQuote(repo_root), arguments),
    stdout = TRUE, stderr = TRUE,
    env = c("GIT_OPTIONAL_LOCKS=0", "GIT_TERMINAL_PROMPT=0")
  )
  status <- attr(value, "status") %||% 0L
  if (!identical(as.integer(status), 0L)) {
    stop("Could not authenticate the primary Git checkout.", call. = FALSE)
  }
  trimws(paste(value, collapse = "\n"))
}
source_commit <- tolower(git_read(c("rev-parse", "HEAD")))
source_status <- git_read(c(
  "status", "--porcelain=v2", "--untracked-files=all"
))
if (nzchar(source_status)) {
  stop("Candidate comparison requires a clean committed checkout.",
       call. = FALSE)
}

expected_seed_sha256 <-
  "3dc8483f4a777ab766704b901997295bed1c89db0590429a70f3116b233e948f"
if (!identical(rqr_confirm_sha256(seed_ledger_path),
               expected_seed_sha256)) {
  stop("The reviewed maximum seed ledger did not authenticate.",
       call. = FALSE)
}
contract <- rqr_confirm_read_contract(repo_root)
rqr_confirm_validate_contract(contract, require_closed = TRUE)
if (isTRUE(contract$config$confirmatory_execution_authorized)) {
  stop("Development comparison requires a fail-closed contract.",
       call. = FALSE)
}
ledger <- utils::read.csv(
  seed_ledger_path, stringsAsFactors = FALSE, check.names = FALSE
)
invisible(rqr_confirm_read_attestation(
  exdqlm_attestation_path, "exdqlm",
  contract$config$comparator$exdqlm$version,
  contract$config$comparator$exdqlm$source_sha256
))

# Candidates change only the number of complete target-preserving transitions.
# For native dynamic fits, retained-array size stays fixed and `thin` controls
# whole-scan distance.  The exdqlm comparator has no thinning argument, so its
# retained transition count is multiplied and the scalar diagnostics are then
# thinned back to the frozen retained size.  This makes the diagnostic sample
# size identical across candidates.
candidates <- data.frame(
  candidate_id = c("whole_scan_x2", "whole_scan_x4", "whole_scan_x8"),
  candidate_order = 1:3,
  transition_multiplier = c(2L, 4L, 8L),
  target_change = FALSE,
  threshold_change = FALSE,
  adaptive_chain_extension = FALSE,
  stringsAsFactors = FALSE
)
cases <- data.frame(
  method = c(
    "M01", "M01", "M02", "M02", "M06", "M06",
    "M09", "M09", "M10", "M10", "M11", "M11", "M11"
  ),
  DGP = c(
    "S05", "S05", "S06", "S06", "S06", "S06",
    "S05", "S05", "S05", "S05", "S05", "S05", "S05"
  ),
  replication = c(
    186L, 188L, 194L, 41L, 196L, 104L,
    77L, 113L, 104L, 129L, 74L, 172L, 190L
  ),
  case_role = c(
    rep(c("hard", "guard"), 5L), "hard", "hard", "guard"
  ),
  diagnostic_role = c(
    "one_chain_standard", "four_chain_sentinel",
    "one_chain_standard", "four_chain_sentinel",
    "one_chain_standard", "four_chain_sentinel",
    "one_chain_standard", "four_chain_sentinel",
    "one_chain_standard", "four_chain_sentinel",
    "four_chain_sentinel", "one_chain_standard", "one_chain_standard"
  ),
  selection_role = c(
    "component_scale_hard", "component_scale_guard",
    "endpoint_hard", "endpoint_guard",
    "frozen_discount_hard", "frozen_discount_guard",
    "low_rate_component_scale_hard", "low_rate_component_scale_guard",
    "high_rate_component_scale_hard", "high_rate_component_scale_guard",
    "learned_rate_component_scale_sentinel_hard",
    "learned_rate_component_scale_standard_hard",
    "learned_rate_component_scale_guard"
  ),
  guard_selection_rule = c(
    NA, "nearest_seeded_method_passing_same_DGP_response_path",
    NA, "nearest_seeded_method_passing_same_DGP_response_path",
    NA, "nearest_seeded_method_passing_same_DGP_response_path",
    NA, "nearest_seeded_method_passing_same_DGP_response_path",
    NA, "nearest_seeded_method_passing_same_DGP_response_path",
    NA, NA, "nearest_fully_passing_same_DGP_response_path"
  ),
  guard_standardized_distance = c(
    NA, 4.842241333, NA, 5.125196888, NA, 1.502989020,
    NA, 2.953863567, NA, 2.491990196, NA, NA, 2.818763292
  ),
  stringsAsFactors = FALSE
)
jobs <- merge(candidates, cases, by = NULL, sort = FALSE)
jobs <- do.call(rbind, lapply(seq_len(nrow(jobs)), function(index) {
  job <- jobs[index, , drop = FALSE]
  if (identical(job$diagnostic_role[[1L]], "four_chain_sentinel")) {
    job[rep(1L, 4L), , drop = FALSE] |>
      transform(chain = 1:4, profile = c("A", "B", "C", "D"))
  } else {
    transform(job, chain = 1L, profile = "standard")
  }
}))
jobs <- jobs[order(
  jobs$method, jobs$candidate_order, jobs$case_role,
  jobs$DGP, jobs$replication, jobs$diagnostic_role, jobs$chain
), , drop = FALSE]
jobs$job_id <- sprintf(
  "%s__%s__%s_rep%04d__%s__chain%02d",
  jobs$candidate_id, jobs$method, jobs$DGP,
  jobs$replication, jobs$diagnostic_role, jobs$chain
)
rownames(jobs) <- NULL
candidate_digest <- digest::digest(
  list(candidates = candidates, cases = cases, jobs = jobs),
  algo = "sha256", serialize = TRUE
)

required_rng_keys <- unique(unlist(lapply(seq_len(nrow(jobs)), function(index) {
  job <- jobs[index, , drop = FALSE]
  cell_id <- contract$incidence$cell_id[
    contract$incidence$DGP == job$DGP[[1L]] &
      contract$incidence$method == job$method[[1L]]
  ]
  if (length(cell_id) != 1L) {
    stop("A candidate job does not map to one incidence cell.", call. = FALSE)
  }
  endpoints <- if (identical(job$method[[1L]], "M02")) {
    c("lower", "upper")
  } else {
    "interval"
  }
  unlist(lapply(c("method", "forecast"), function(stage) {
    vapply(endpoints, function(endpoint) {
      rqr_confirm_rng_task_key(
        stage, cell_id, job$replication[[1L]], endpoint, job$chain[[1L]]
      )
    }, character(1L))
  }), use.names = FALSE)
}), use.names = FALSE))
missing_rng_keys <- setdiff(required_rng_keys, ledger$task_key)
if (length(missing_rng_keys)) {
  stop(
    paste(
      "The reviewed seed ledger lacks candidate task keys:",
      paste(head(missing_rng_keys, 8L), collapse = ", ")
    ),
    call. = FALSE
  )
}
# Regenerating each fixed case is a read-only preflight of its DGP stream keys.
invisible(lapply(seq_len(nrow(cases)), function(index) {
  rqr_confirm_generate_dgp(
    contract, cases$DGP[[index]], cases$replication[[index]], ledger
  )
}))
rng_key_digest <- digest::digest(
  sort(required_rng_keys, method = "radix"),
  algo = "sha256", serialize = TRUE
)

plan_paths <- file.path(output_root, c(
  "transition_candidates.csv", "targeted_cases.csv", "jobs.csv",
  "preflight_manifest.json"
))
if (identical(mode, "preflight")) {
  if (file.exists(output_root) || dir.exists(output_root)) {
    stop("Preflight output root must be new.", call. = FALSE)
  }
  dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(output_root, "job_results"), showWarnings = FALSE)
  rqr_confirm_atomic_write_csv(
    candidates, file.path(output_root, "transition_candidates.csv")
  )
  rqr_confirm_atomic_write_csv(
    cases, file.path(output_root, "targeted_cases.csv")
  )
  rqr_confirm_atomic_write_csv(jobs, file.path(output_root, "jobs.csv"))
  manifest <- list(
    schema_version = "rqrgibbs_dlm_skewed_candidate_preflight/1.0.0",
    source_commit = source_commit,
    source_clean = TRUE,
    seed_ledger_path = seed_ledger_path,
    seed_ledger_sha256 = rqr_confirm_sha256(seed_ledger_path),
    exdqlm_attestation_path = exdqlm_attestation_path,
    exdqlm_attestation_sha256 =
      rqr_confirm_sha256(exdqlm_attestation_path),
    candidate_digest = candidate_digest,
    rng_task_keys = length(required_rng_keys),
    rng_task_key_digest = rng_key_digest,
    all_rng_task_keys_present = TRUE,
    candidates = nrow(candidates),
    cases = nrow(cases),
    one_chain_standard_cases = sum(
      cases$diagnostic_role == "one_chain_standard"
    ),
    four_chain_sentinel_cases = sum(
      cases$diagnostic_role == "four_chain_sentinel"
    ),
    jobs = nrow(jobs),
    workers_requested = workers,
    exact_target_required = TRUE,
    zero_repairs_required = TRUE,
    thresholds_changed = FALSE,
    retries_allowed = FALSE,
    reseeding_allowed = FALSE,
    scientific_metrics_used = FALSE,
    confirmatory_launch_authorized = FALSE,
    created_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
  )
  rqr_confirm_atomic_write_json(
    manifest, file.path(output_root, "preflight_manifest.json")
  )
  hashes <- rqr_confirm_recursive_manifest(output_root)
  rqr_confirm_atomic_write_csv(
    hashes, file.path(output_root, "preflight_artifact_hashes.csv")
  )
  cat(sprintf(
    "Candidate preflight complete: %d jobs, %d candidates, %d cases.\n",
    nrow(jobs), nrow(candidates), nrow(cases)
  ))
  quit(save = "no", status = 0L, runLast = FALSE)
}

preflight_hash_path <- file.path(
  output_root, "preflight_artifact_hashes.csv"
)
if (!dir.exists(output_root) || any(!file.exists(plan_paths)) ||
    !file.exists(preflight_hash_path)) {
  stop("Execute mode requires a completed preflight root.", call. = FALSE)
}
preflight_hashes <- utils::read.csv(
  preflight_hash_path, stringsAsFactors = FALSE, check.names = FALSE
)
expected_preflight_paths <- basename(plan_paths)
if (!identical(sort(preflight_hashes$path),
               sort(expected_preflight_paths)) ||
    any(!vapply(seq_len(nrow(preflight_hashes)), function(index) {
      path <- file.path(output_root, preflight_hashes$path[[index]])
      file.exists(path) &&
        identical(as.numeric(file.info(path)$size),
                  as.numeric(preflight_hashes$bytes[[index]])) &&
        identical(rqr_confirm_sha256(path),
                  preflight_hashes$sha256[[index]])
    }, logical(1L)))) {
  stop("The preflight artifact bundle did not authenticate.", call. = FALSE)
}
preflight <- jsonlite::read_json(
  file.path(output_root, "preflight_manifest.json"), simplifyVector = TRUE
)
stored_candidates <- utils::read.csv(
  file.path(output_root, "transition_candidates.csv"),
  stringsAsFactors = FALSE, check.names = FALSE
)
stored_cases <- utils::read.csv(
  file.path(output_root, "targeted_cases.csv"),
  stringsAsFactors = FALSE, check.names = FALSE
)
stored_jobs <- utils::read.csv(
  file.path(output_root, "jobs.csv"),
  stringsAsFactors = FALSE, check.names = FALSE
)
if (!identical(preflight$source_commit, source_commit) ||
    !identical(preflight$seed_ledger_sha256,
               rqr_confirm_sha256(seed_ledger_path)) ||
    !identical(preflight$exdqlm_attestation_sha256,
               rqr_confirm_sha256(exdqlm_attestation_path)) ||
    !identical(preflight$candidate_digest, candidate_digest) ||
    !identical(preflight$rng_task_key_digest, rng_key_digest) ||
    !isTRUE(preflight$all_rng_task_keys_present) ||
    !identical(stored_candidates, candidates) ||
    !identical(stored_cases, cases) ||
    !identical(stored_jobs, jobs)) {
  stop("Execute inputs differ from the authenticated preflight.",
       call. = FALSE)
}

atomic_rds <- function(value, path) {
  temporary <- tempfile(paste0(".", basename(path), "-"),
                        tmpdir = dirname(path))
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  saveRDS(value, temporary, version = 3L)
  if (!file.rename(temporary, path)) {
    stop("Could not atomically publish a candidate job result.",
         call. = FALSE)
  }
  invisible(path)
}

job_path <- function(job_id) file.path(
  output_root, "job_results", paste0(job_id, ".rds")
)
incomplete <- which(!file.exists(vapply(
  jobs$job_id, job_path, character(1L)
)))
started_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
if (length(incomplete)) {
  parallel::mclapply(
    incomplete, mc.cores = min(workers, length(incomplete)),
    mc.preschedule = FALSE,
    FUN = function(index) {
      job <- jobs[index, , drop = FALSE]
      result <- tryCatch({
        generated <- rqr_confirm_generate_dgp(
          contract, job$DGP[[1L]], job$replication[[1L]], ledger
        )
        base_schedule <- rqr_confirm_method_schedule(
          contract, job$method[[1L]], job$profile[[1L]]
        )
        multiplier <- job$transition_multiplier[[1L]]
        fit_started <- proc.time()[["elapsed"]]
        fitted <- if (identical(job$method[[1L]], "M02")) {
          rqr_confirm_dynamic_quantile(
            contract, generated, job$chain[[1L]], ledger,
            exdqlm_attestation_path,
            profile_name = job$profile[[1L]],
            schedule_override = list(
              burn = as.integer(base_schedule$burn * multiplier),
              retain = as.integer(base_schedule$retain * multiplier)
            )
          )
        } else {
          rqr_confirm_dynamic_fit(
            contract, generated, job$method[[1L]], job$chain[[1L]],
            ledger, provenance_control = list(),
            profile_name = job$profile[[1L]],
            mcmc_control_override = list(
              n_burn = as.integer(base_schedule$burn * multiplier),
              n_mcmc = as.integer(base_schedule$retain),
              thin = as.integer(base_schedule$thin * multiplier)
            )
          )
        }
        scalars <- rqr_confirm_scalar_draws(
          fitted, generated, contract, job$method[[1L]]
        )
        if (identical(job$method[[1L]], "M02")) {
          retained_index <- seq.int(
            from = multiplier, to = nrow(scalars), by = multiplier
          )
          scalars <- scalars[retained_index, , drop = FALSE]
          if (nrow(scalars) != base_schedule$retain) {
            stop(
              "The thinned M02 diagnostic sample has the wrong size.",
              call. = FALSE
            )
          }
        }
        exact <- if (identical(job$method[[1L]], "M02")) TRUE else
          isTRUE(fitted$fit$model_spec$exact_joint_target)
        repairs <- if (identical(job$method[[1L]], "M02")) 0L else
          as.integer(fitted$fit$model_spec$numerical_repair_count)
        list(
          ok = TRUE,
          job = as.list(job),
          scalars = scalars,
          exact_joint_target = exact,
          numerical_repair_count = repairs,
          elapsed_seconds =
            as.numeric(proc.time()[["elapsed"]] - fit_started),
          peak_RSS_KiB = rqr_confirm_process_peak_rss_kib(),
          retry_count = 0L,
          reseeded = FALSE
        )
      }, error = function(error) {
        list(
          ok = FALSE, job = as.list(job),
          error_class = class(error)[[1L]],
          message = conditionMessage(error),
          peak_RSS_KiB = rqr_confirm_process_peak_rss_kib(),
          retry_count = 0L, reseeded = FALSE
        )
      })
      atomic_rds(result, job_path(job$job_id[[1L]]))
      isTRUE(result$ok)
    }
  )
}

paths <- vapply(jobs$job_id, job_path, character(1L))
if (any(!file.exists(paths))) {
  stop("Candidate execution is incomplete; rerun execute mode to resume.",
       call. = FALSE)
}
results <- lapply(paths, readRDS)
job_status <- do.call(rbind, lapply(results, function(result) {
  job <- result$job
  data.frame(
    job_id = job$job_id,
    candidate_id = job$candidate_id,
    method = job$method,
    DGP = job$DGP,
    replication = job$replication,
    case_role = job$case_role,
    chain = job$chain,
    profile = job$profile,
    diagnostic_role = job$diagnostic_role,
    ok = isTRUE(result$ok),
    exact_joint_target = as.logical(
      result$exact_joint_target %||% FALSE
    ),
    numerical_repair_count = as.integer(
      result$numerical_repair_count %||% NA_integer_
    ),
    elapsed_seconds = as.numeric(result$elapsed_seconds %||% NA_real_),
    peak_RSS_KiB = as.numeric(result$peak_RSS_KiB %||% NA_real_),
    retry_count = as.integer(result$retry_count %||% NA_integer_),
    reseeded = as.logical(result$reseeded %||% NA),
    error_class = as.character(result$error_class %||% ""),
    message = as.character(result$message %||% ""),
    stringsAsFactors = FALSE
  )
}))

group_key <- paste(
  jobs$candidate_id, jobs$method, jobs$DGP, jobs$replication,
  jobs$diagnostic_role, sep = "|"
)
group_levels <- unique(group_key)
diagnostic_list <- vector("list", length(group_levels))
for (index in seq_along(group_levels)) {
  selected <- which(group_key == group_levels[[index]])
  selected_results <- results[selected]
  job <- jobs[selected[[1L]], , drop = FALSE]
  if (!all(vapply(selected_results, `[[`, logical(1L), "ok"))) {
    diagnostic_list[[index]] <- data.frame(
      estimand = "fit_error", chains = 4L,
      rhat = NA_real_, ess_bulk = NA_real_, ess_tail = NA_real_,
      mcse_mean = NA_real_, mcse_over_sd = NA_real_, pass = FALSE,
      candidate_id = job$candidate_id,
      transition_multiplier = job$transition_multiplier,
      method = job$method, DGP = job$DGP,
      replication = job$replication, case_role = job$case_role,
      diagnostic_role = job$diagnostic_role,
      stringsAsFactors = FALSE
    )
  } else {
    generated <- rqr_confirm_generate_dgp(
      contract, job$DGP[[1L]], job$replication[[1L]], ledger
    )
    value <- rqr_confirm_chain_diagnostics(
      lapply(selected_results, `[[`, "scalars"),
      contract = contract,
      sentinel = identical(
        job$diagnostic_role[[1L]], "four_chain_sentinel"
      ),
      method = job$method[[1L]], generated = generated
    )
    value$candidate_id <- job$candidate_id
    value$transition_multiplier <- job$transition_multiplier
    value$method <- job$method
    value$DGP <- job$DGP
    value$replication <- job$replication
    value$case_role <- job$case_role
    value$diagnostic_role <- job$diagnostic_role
    diagnostic_list[[index]] <- value
  }
}
diagnostics <- do.call(rbind, diagnostic_list)
diagnostics <- diagnostics[order(
  diagnostics$method, diagnostics$transition_multiplier,
  diagnostics$case_role, diagnostics$DGP,
  diagnostics$replication, diagnostics$estimand
), , drop = FALSE]

candidate_summary <- do.call(rbind, lapply(
  split(diagnostics, list(diagnostics$method, diagnostics$candidate_id),
        drop = TRUE),
  function(value) {
    status <- job_status[
      job_status$method == value$method[[1L]] &
        job_status$candidate_id == value$candidate_id[[1L]],
      , drop = FALSE
    ]
    data.frame(
      method = value$method[[1L]],
      candidate_id = value$candidate_id[[1L]],
      transition_multiplier = value$transition_multiplier[[1L]],
      cases = length(unique(paste(value$DGP, value$replication))),
      jobs = nrow(status),
      fits_succeeded = sum(status$ok),
      exact_target_fits = sum(status$exact_joint_target),
      total_repairs = sum(status$numerical_repair_count, na.rm = TRUE),
      diagnostics = nrow(value),
      diagnostics_passed = sum(value$pass),
      all_hard_and_guard_diagnostics_pass = all(value$pass),
      elapsed_seconds = sum(status$elapsed_seconds, na.rm = TRUE),
      max_peak_RSS_KiB = max(status$peak_RSS_KiB, na.rm = TRUE),
      eligible = all(status$ok) && all(status$exact_joint_target) &&
        all(status$numerical_repair_count == 0L) &&
        all(status$retry_count == 0L) && !any(status$reseeded) &&
        all(value$pass),
      stringsAsFactors = FALSE
    )
  }
))
candidate_summary <- candidate_summary[order(
  candidate_summary$method, candidate_summary$transition_multiplier
), , drop = FALSE]

decisions <- do.call(rbind, lapply(
  sort(unique(candidate_summary$method), method = "radix"),
  function(method) {
    value <- candidate_summary[
      candidate_summary$method == method, , drop = FALSE
    ]
    eligible <- value[value$eligible, , drop = FALSE]
    selected <- if (nrow(eligible)) {
      eligible[order(
        eligible$transition_multiplier, eligible$elapsed_seconds
      ), , drop = FALSE][1L, ]
    } else NULL
    data.frame(
      method = method,
      selected_candidate = if (is.null(selected)) "none" else
        selected$candidate_id,
      selected_transition_multiplier = if (is.null(selected)) NA_integer_
        else selected$transition_multiplier,
      selection_status = if (is.null(selected)) "no_eligible_candidate"
        else "minimum_transition_multiplier_eligible",
      thresholds_changed = FALSE,
      target_changed = FALSE,
      replication_specific_schedule = FALSE,
      confirmatory_authorization = FALSE,
      stringsAsFactors = FALSE
    )
  }
))

rqr_confirm_atomic_write_csv(
  job_status, file.path(output_root, "job_status.csv")
)
rqr_confirm_atomic_write_csv(
  diagnostics, file.path(output_root, "candidate_diagnostics.csv")
)
rqr_confirm_atomic_write_csv(
  candidate_summary, file.path(output_root, "candidate_summary.csv")
)
rqr_confirm_atomic_write_csv(
  decisions, file.path(output_root, "candidate_decisions.csv")
)
completed_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
manifest <- list(
  schema_version = "rqrgibbs_dlm_skewed_candidate_comparison/1.0.0",
  source_commit = source_commit,
  source_clean = TRUE,
  seed_ledger_sha256 = rqr_confirm_sha256(seed_ledger_path),
  exdqlm_attestation_sha256 = rqr_confirm_sha256(exdqlm_attestation_path),
  candidate_digest = candidate_digest,
  jobs = nrow(jobs),
  jobs_succeeded = sum(job_status$ok),
  all_exact_joint_target = all(job_status$exact_joint_target),
  all_zero_repairs = all(job_status$numerical_repair_count == 0L),
  all_methods_selected = all(decisions$selection_status ==
                               "minimum_transition_multiplier_eligible"),
  threshold_changes = FALSE,
  target_changes = FALSE,
  retries = sum(job_status$retry_count),
  reseeding = any(job_status$reseeded),
  scientific_metrics_used = FALSE,
  scientific_promotion = FALSE,
  confirmatory_launch_authorized = FALSE,
  started_at_utc = started_at,
  completed_at_utc = completed_at
)
rqr_confirm_atomic_write_json(
  manifest, file.path(output_root, "comparison_manifest.json")
)
hashes <- rqr_confirm_recursive_manifest(
  output_root
)
hashes <- hashes[
  hashes$path != "candidate_artifact_hashes.csv", , drop = FALSE
]
rqr_confirm_atomic_write_csv(
  hashes, file.path(output_root, "candidate_artifact_hashes.csv")
)
cat(sprintf(
  "Candidate comparison complete: %d/%d jobs succeeded; %d/%d methods selected.\n",
  sum(job_status$ok), nrow(job_status),
  sum(decisions$selection_status == "minimum_transition_multiplier_eligible"),
  nrow(decisions)
))
if (!isTRUE(manifest$all_methods_selected)) {
  stop("No eligible predeclared candidate exists for at least one method.",
       call. = FALSE)
}
