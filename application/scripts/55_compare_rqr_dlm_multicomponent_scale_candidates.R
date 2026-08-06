#!/usr/bin/env Rscript

# Development-only, predeclared recovery comparison for the failed M11 S10
# multicomponent guard. The script compares exact target-preserving kernels on
# two independently selected M11 sentinels and one already-passing M10 guard.
# It cannot authorize or launch the confirmatory simulation.

args <- commandArgs(trailingOnly = TRUE)
parse_arg <- function(name, default = "") {
  prefix <- paste0("--", name, "=")
  value <- grep(paste0("^", prefix), args, value = TRUE)
  if (!length(value)) return(default)
  sub(prefix, "", value[[length(value)]], fixed = TRUE)
}
if (any(args %in% c("-h", "--help"))) {
  cat(paste(
    "Usage: 55_compare_rqr_dlm_multicomponent_scale_candidates.R",
    "  --mode=<preflight|execute>",
    "  --output-root=<fresh ignored directory>",
    "  [--workers=8]",
    sep = "\n"
  ), "\n")
  quit(save = "no", status = 0L, runLast = FALSE)
}

repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(repo_root, "application", "DESCRIPTION"))) {
  stop("Run the recovery comparison from the repository root.",
       call. = FALSE)
}
mode <- parse_arg("mode", "preflight")
output_root <- parse_arg("output-root")
workers <- suppressWarnings(as.integer(parse_arg("workers", "8")))
expected_commit <- tolower(Sys.getenv(
  "RQR_EXPECTED_PRIMARY_COMMIT", unset = ""
))
primary_attestation <- Sys.getenv(
  "RQR_PRIMARY_RUNTIME_ATTESTATION", unset = ""
)
if (!mode %in% c("preflight", "execute") || !nzchar(output_root) ||
    file.exists(output_root) || dir.exists(output_root) ||
    length(workers) != 1L || is.na(workers) ||
    workers < 1L || workers > 8L ||
    !grepl("^[0-9a-f]{40}$", expected_commit) ||
    !file.exists(primary_attestation)) {
  stop(
    paste(
      "preflight/execute mode, a fresh output root, 1--8 workers,",
      "an exact expected commit, and a primary attestation are required."
    ),
    call. = FALSE
  )
}
output_root <- normalizePath(output_root, winslash = "/", mustWork = FALSE)
if (!startsWith(
    output_root,
    paste0(normalizePath(
      file.path(repo_root, "application", "cache"),
      winslash = "/", mustWork = TRUE
    ), "/")
  )) {
  stop("The recovery output must be under application/cache/.",
       call. = FALSE)
}

suppressPackageStartupMessages(library(rqrgibbs))
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
    stop("Could not authenticate the recovery source.", call. = FALSE)
  }
  trimws(paste(value, collapse = "\n"))
}
source_commit <- tolower(git_read(c("rev-parse", "HEAD")))
source_status <- git_read(c(
  "status", "--porcelain=v2", "--untracked-files=all"
))
if (!identical(source_commit, expected_commit) || nzchar(source_status)) {
  stop("Recovery comparison requires the exact clean source commit.",
       call. = FALSE)
}

thread_names <- c(
  "OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS",
  "VECLIB_MAXIMUM_THREADS", "NUMEXPR_NUM_THREADS"
)
thread_values <- setNames(
  vapply(thread_names, Sys.getenv, character(1L), unset = ""),
  thread_names
)
if (any(thread_values != "1")) {
  stop("Every numerical-library thread control must equal one.",
       call. = FALSE)
}

contract <- rqr_confirm_read_contract(repo_root)
rqr_confirm_validate_contract(contract, require_closed = TRUE)
rqr_confirm_validate_budget(contract)
if (isTRUE(contract$config$confirmatory_execution_authorized)) {
  stop("The recovery comparison requires a fail-closed contract.",
       call. = FALSE)
}
provenance_control <- rqr_confirm_primary_provenance_control(
  repo_root, expected_commit, primary_attestation
)
memory_ceiling_KiB <- as.numeric(
  contract$config$resources$per_worker_memory_GiB
) * 1024^2
if (!is.finite(memory_ceiling_KiB) || memory_ceiling_KiB <= 0) {
  stop("The per-worker memory contract is invalid.", call. = FALSE)
}

ledger <- rqr_confirm_seed_ledger(contract, planning = "maximum")
invisible(rqr_confirm_validate_seed_ledger(
  ledger, contract, planning = "maximum", require_complete = TRUE
))
invisible(rqr_confirm_validate_planned_method_rng_bindings(
  ledger, contract, planning = "maximum"
))
expected_seed_sha256 <-
  "3dc8483f4a777ab766704b901997295bed1c89db0590429a70f3116b233e948f"

# Candidate order is the deterministic least-added-work selection order. The
# baseline is included to reproduce the failure under the new exact runtime.
# Directional and joint-state moves are separate before their composition is
# tested. No candidate changes the target, seed, schedule, or thresholds.
candidates <- data.frame(
  candidate_id = c(
    "baseline_joint1_coordinate",
    "directional1_joint1",
    "joint2_coordinate",
    "directional1_joint2"
  ),
  candidate_order = 1:4,
  directional_interweave = c(FALSE, TRUE, FALSE, TRUE),
  directional_sweeps = 1L,
  joint_state_elliptical_cycles = c(1L, 1L, 2L, 2L),
  transition_order = "rootwise_then_interweave",
  target_change = FALSE,
  schedule_change = FALSE,
  threshold_change = FALSE,
  adaptive_extension = FALSE,
  stringsAsFactors = FALSE
)
cases <- data.frame(
  method = c("M11", "M11", "M10"),
  DGP = "S10",
  replication = c(166L, 167L, 77L),
  case_role = c(
    "failed_preselected_M11_sentinel",
    "independent_preselected_M11_sentinel_guard",
    "previously_passing_fixed_rate_guard"
  ),
  chains = 4L,
  stringsAsFactors = FALSE
)
sentinels <- rqr_confirm_sentinel_map(contract, planning = "maximum")
for (index in seq_len(nrow(cases))) {
  cell <- contract$incidence$cell_id[
    contract$incidence$DGP == cases$DGP[[index]] &
      contract$incidence$method == cases$method[[index]]
  ]
  if (length(cell) != 1L || !any(
      sentinels$cell_id == cell &
        sentinels$replication == cases$replication[[index]]
    )) {
    stop("A recovery case is not a preselected sentinel replication.",
         call. = FALSE)
  }
}
jobs <- merge(candidates, cases, by = NULL, sort = FALSE)
jobs <- jobs[rep(seq_len(nrow(jobs)), each = 4L), , drop = FALSE]
jobs$chain <- rep(1:4, times = nrow(jobs) / 4L)
jobs$profile <- c("A", "B", "C", "D")[jobs$chain]
jobs <- jobs[order(
  jobs$candidate_order, match(jobs$method, c("M11", "M10")),
  jobs$replication, jobs$chain, method = "radix"
), , drop = FALSE]
jobs$job_id <- sprintf(
  "%s__%s__S10_rep%04d__chain%02d",
  jobs$candidate_id, jobs$method, jobs$replication, jobs$chain
)
rownames(jobs) <- NULL
if (nrow(jobs) != 48L || anyDuplicated(jobs$job_id)) {
  stop("The fixed 48-job recovery contract is malformed.",
       call. = FALSE)
}
plan_digest <- digest::digest(
  list(candidates = candidates, cases = cases, jobs = jobs),
  algo = "sha256", serialize = TRUE
)

dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
rqr_confirm_atomic_write_csv(
  candidates, file.path(output_root, "transition_candidates.csv")
)
rqr_confirm_atomic_write_csv(
  cases, file.path(output_root, "targeted_cases.csv")
)
rqr_confirm_atomic_write_csv(jobs, file.path(output_root, "jobs.csv"))
seed_path <- file.path(output_root, "seed_ledger_maximum.csv")
rqr_confirm_atomic_write_csv(ledger, seed_path)
if (!identical(rqr_confirm_sha256(seed_path), expected_seed_sha256)) {
  stop("The frozen maximum seed ledger digest changed.", call. = FALSE)
}

preflight_manifest <- list(
  schema_version =
    "rqrgibbs_dlm_multicomponent_recovery_preflight/1.0.0",
  source_commit = source_commit,
  source_clean = TRUE,
  primary_runtime_attestation_sha256 =
    rqr_confirm_sha256(primary_attestation),
  plan_digest = plan_digest,
  candidates = nrow(candidates), cases = nrow(cases), jobs = nrow(jobs),
  methods = unique(cases$method), DGP = "S10",
  replications = cases$replication,
  all_cases_preselected_sentinels = TRUE,
  seed_ledger_sha256 = expected_seed_sha256,
  target_change = FALSE, schedule_change = FALSE,
  threshold_change = FALSE, adaptive_extension = FALSE,
  scientific_metrics_used = FALSE, scientific_promotion = FALSE,
  confirmatory_launch_authorized = FALSE,
  thread_environment = as.list(thread_values)
)
rqr_confirm_atomic_write_json(
  preflight_manifest, file.path(output_root, "preflight_manifest.json")
)
if (identical(mode, "preflight")) {
  hashes <- rqr_confirm_recursive_manifest(output_root)
  hashes <- hashes[hashes$path != "artifact_hashes.csv", , drop = FALSE]
  rqr_confirm_atomic_write_csv(
    hashes, file.path(output_root, "artifact_hashes.csv")
  )
  cat(sprintf(
    "Recovery preflight passed: %d candidates, %d cases, %d jobs.\n",
    nrow(candidates), nrow(cases), nrow(jobs)
  ))
  quit(save = "no", status = 0L, runLast = FALSE)
}

dir.create(file.path(output_root, "job_results"), showWarnings = FALSE)
peak_rss_kib <- function() {
  status <- readLines("/proc/self/status", warn = FALSE)
  value <- sub(
    "^VmHWM:[[:space:]]*", "",
    grep("^VmHWM:", status, value = TRUE)
  )
  as.numeric(sub("[[:space:]]*kB$", "", value))
}
run_job <- function(index) {
  job <- jobs[index, , drop = FALSE]
  started <- proc.time()[["elapsed"]]
  result <- tryCatch({
    generated <- rqr_confirm_generate_dgp(
      contract, job$DGP[[1L]], job$replication[[1L]], ledger
    )
    fitted <- rqr_confirm_dynamic_fit(
      contract, generated, job$method[[1L]], job$chain[[1L]], ledger,
      provenance_control = provenance_control,
      profile_name = job$profile[[1L]],
      mcmc_control_override = list(
        component_scale_directional_interweave =
          job$directional_interweave[[1L]],
        component_scale_directional_sweeps =
          job$directional_sweeps[[1L]],
        component_scale_joint_elliptical_slice = TRUE,
        component_scale_joint_elliptical_cycles =
          job$joint_state_elliptical_cycles[[1L]]
      )
    )
    scalars <- rqr_confirm_scalar_draws(
      fitted, generated, contract, job$method[[1L]]
    )
    telemetry <- rqr_confirm_transition_telemetry(fitted$fit)
    list(
      ok = TRUE, message = "", job = job, scalars = scalars,
      telemetry = telemetry,
      exact_joint_target = isTRUE(
        fitted$fit$model_spec$exact_joint_target
      ),
      numerical_repair_count =
        fitted$fit$model_spec$numerical_repair_count,
      transition_kernel =
        fitted$fit$model_spec$component_scale_transition_kernel
    )
  }, error = function(error) {
    list(
      ok = FALSE, message = conditionMessage(error), job = job,
      scalars = NULL, telemetry = NULL, exact_joint_target = FALSE,
      numerical_repair_count = NA_integer_, transition_kernel = NULL
    )
  })
  result$elapsed_seconds <- proc.time()[["elapsed"]] - started
  result$peak_RSS_KiB <- peak_rss_kib()
  path <- file.path(
    output_root, "job_results", paste0(job$job_id[[1L]], ".rds")
  )
  temporary <- tempfile(paste0(".", basename(path), "-"), dirname(path))
  saveRDS(result, temporary, version = 3L)
  if (!file.rename(temporary, path)) {
    stop("Could not atomically publish a recovery result.",
         call. = FALSE)
  }
  result
}

started_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
results <- parallel::mclapply(
  seq_len(nrow(jobs)), run_job,
  mc.cores = min(workers, nrow(jobs)), mc.preschedule = FALSE
)
status <- do.call(rbind, lapply(results, function(value) {
  telemetry <- value$telemetry
  data.frame(
    job_id = value$job$job_id,
    candidate_id = value$job$candidate_id,
    candidate_order = value$job$candidate_order,
    method = value$job$method, DGP = value$job$DGP,
    replication = value$job$replication,
    case_role = value$job$case_role,
    chain = value$job$chain, profile = value$job$profile,
    ok = value$ok, exact_joint_target = value$exact_joint_target,
    numerical_repair_count = value$numerical_repair_count,
    elapsed_seconds = value$elapsed_seconds,
    peak_RSS_KiB = value$peak_RSS_KiB,
    target_digest = if (is.null(telemetry)) NA_character_ else {
      telemetry$target_digest
    },
    model_digest = if (is.null(telemetry)) NA_character_ else {
      telemetry$model_digest
    },
    evolution_digest = if (is.null(telemetry)) NA_character_ else {
      telemetry$evolution_digest
    },
    transition_kernel_digest = if (is.null(telemetry)) {
      NA_character_
    } else {
      telemetry$transition_kernel_digest
    },
    coordinate_evaluations = if (is.null(telemetry)) NA_real_ else {
      telemetry$coordinate_evaluations
    },
    directional_updates = if (is.null(telemetry)) NA_real_ else {
      telemetry$directional_updates
    },
    directional_evaluations = if (is.null(telemetry)) NA_real_ else {
      telemetry$directional_evaluations
    },
    directional_max_distance = if (is.null(telemetry)) NA_real_ else {
      telemetry$directional_max_distance
    },
    joint_updates = if (is.null(telemetry)) NA_real_ else {
      telemetry$joint_updates
    },
    joint_evaluations = if (is.null(telemetry)) NA_real_ else {
      telemetry$joint_evaluations
    },
    all_transition_updates_exact = if (is.null(telemetry)) FALSE else {
      isTRUE(telemetry$all_directional_updates_exact) &&
        isTRUE(telemetry$all_joint_updates_exact)
    },
    retry_count = 0L, reseeded = FALSE, message = value$message,
    stringsAsFactors = FALSE
  )
}))
rqr_confirm_atomic_write_csv(status, file.path(output_root, "job_status.csv"))

identity_groups <- split(
  status,
  interaction(
    status$method, status$DGP, status$replication, status$chain,
    drop = TRUE, lex.order = TRUE
  )
)
target_identity <- do.call(rbind, lapply(identity_groups, function(value) {
  data.frame(
    method = value$method[[1L]], DGP = value$DGP[[1L]],
    replication = value$replication[[1L]], chain = value$chain[[1L]],
    candidates = nrow(value),
    target_digests = length(unique(stats::na.omit(value$target_digest))),
    model_digests = length(unique(stats::na.omit(value$model_digest))),
    evolution_digests =
      length(unique(stats::na.omit(value$evolution_digest))),
    target_identity_pass = nrow(value) == nrow(candidates) &&
      !anyNA(value$target_digest) && !anyNA(value$model_digest) &&
      !anyNA(value$evolution_digest) &&
      length(unique(stats::na.omit(value$target_digest))) == 1L &&
      length(unique(stats::na.omit(value$model_digest))) == 1L &&
      length(unique(stats::na.omit(value$evolution_digest))) == 1L,
    stringsAsFactors = FALSE
  )
}))
rownames(target_identity) <- NULL
rqr_confirm_atomic_write_csv(
  target_identity, file.path(output_root, "target_identity.csv")
)

diagnostic_rows <- list()
row_index <- 0L
if (all(status$ok) && all(status$exact_joint_target) &&
    all(status$numerical_repair_count == 0L)) {
  for (candidate in candidates$candidate_id) {
    for (case_index in seq_len(nrow(cases))) {
      selected <- which(
        jobs$candidate_id == candidate &
          jobs$method == cases$method[[case_index]] &
          jobs$replication == cases$replication[[case_index]]
      )
      generated <- rqr_confirm_generate_dgp(
        contract, cases$DGP[[case_index]],
        cases$replication[[case_index]], ledger
      )
      value <- rqr_confirm_chain_diagnostics(
        lapply(results[selected], `[[`, "scalars"),
        contract, sentinel = TRUE,
        method = cases$method[[case_index]], generated = generated
      )
      value$candidate_id <- candidate
      value$candidate_order <- candidates$candidate_order[
        candidates$candidate_id == candidate
      ]
      value$method <- cases$method[[case_index]]
      value$DGP <- cases$DGP[[case_index]]
      value$replication <- cases$replication[[case_index]]
      value$case_role <- cases$case_role[[case_index]]
      row_index <- row_index + 1L
      diagnostic_rows[[row_index]] <- value
    }
  }
}
diagnostics <- if (length(diagnostic_rows)) {
  do.call(rbind, diagnostic_rows)
} else {
  data.frame()
}
rqr_confirm_atomic_write_csv(
  diagnostics, file.path(output_root, "candidate_diagnostics.csv")
)

core_estimands <- c(
  "log_q_1", "log_q_2", "log_lambda", "observed_loss",
  "mean_width", "mean_midpoint", "terminal_width",
  "future_h20_width"
)
lags <- c(1L, 5L, 10L, 25L, 50L)
chain_rows <- list()
row_index <- 0L
for (result in results) {
  if (!isTRUE(result$ok)) next
  scalars <- result$scalars
  for (estimand in intersect(core_estimands, colnames(scalars))) {
    value <- scalars[, estimand]
    half <- length(value) %/% 2L
    autocorrelation <- stats::acf(
      value, lag.max = max(lags), plot = FALSE
    )$acf[lags + 1L]
    draws <- array(
      value, dim = c(length(value), 1L, 1L),
      dimnames = list(NULL, NULL, estimand)
    )
    ess <- posterior::ess_bulk(draws)
    row_index <- row_index + 1L
    chain_rows[[row_index]] <- data.frame(
      job_id = result$job$job_id,
      candidate_id = result$job$candidate_id,
      method = result$job$method, DGP = result$job$DGP,
      replication = result$job$replication,
      chain = result$job$chain, profile = result$job$profile,
      estimand = estimand, draws = length(value),
      mean = mean(value), sd = stats::sd(value),
      q05 = unname(stats::quantile(value, 0.05)),
      median = stats::median(value),
      q95 = unname(stats::quantile(value, 0.95)),
      first_half_mean = mean(value[seq_len(half)]),
      second_half_mean = mean(value[(half + 1L):length(value)]),
      acf_lag01 = autocorrelation[[1L]],
      acf_lag05 = autocorrelation[[2L]],
      acf_lag10 = autocorrelation[[3L]],
      acf_lag25 = autocorrelation[[4L]],
      acf_lag50 = autocorrelation[[5L]],
      single_chain_bulk_ess = ess,
      effective_draws_per_second = ess / result$elapsed_seconds,
      stringsAsFactors = FALSE
    )
  }
}
chain_forensics <- if (length(chain_rows)) {
  do.call(rbind, chain_rows)
} else {
  data.frame()
}
rqr_confirm_atomic_write_csv(
  chain_forensics, file.path(output_root, "chain_forensics.csv")
)

candidate_summary <- do.call(rbind, lapply(
  seq_len(nrow(candidates)), function(index) {
    candidate <- candidates$candidate_id[[index]]
    job_value <- status[status$candidate_id == candidate, , drop = FALSE]
    diagnostic_value <- diagnostics[
      diagnostics$candidate_id == candidate, , drop = FALSE
    ]
    identity_value <- target_identity
    data.frame(
      candidate_id = candidate,
      candidate_order = candidates$candidate_order[[index]],
      jobs = nrow(job_value), jobs_succeeded = sum(job_value$ok),
      exact_target_jobs = sum(job_value$exact_joint_target),
      zero_repair_jobs = sum(job_value$numerical_repair_count == 0L),
      resource_pass_jobs = sum(
        job_value$peak_RSS_KiB <= memory_ceiling_KiB
      ),
      diagnostics = nrow(diagnostic_value),
      diagnostics_passed = if (nrow(diagnostic_value)) {
        sum(diagnostic_value$pass)
      } else {
        0L
      },
      failed_diagnostics = if (nrow(diagnostic_value)) {
        sum(!diagnostic_value$pass)
      } else {
        NA_integer_
      },
      max_rhat = if (nrow(diagnostic_value)) {
        max(diagnostic_value$rhat, na.rm = TRUE)
      } else {
        NA_real_
      },
      min_bulk_ess = if (nrow(diagnostic_value)) {
        min(diagnostic_value$ess_bulk, na.rm = TRUE)
      } else {
        NA_real_
      },
      min_tail_ess = if (nrow(diagnostic_value)) {
        min(diagnostic_value$ess_tail, na.rm = TRUE)
      } else {
        NA_real_
      },
      max_mcse_over_sd = if (nrow(diagnostic_value)) {
        max(diagnostic_value$mcse_over_sd, na.rm = TRUE)
      } else {
        NA_real_
      },
      median_elapsed_seconds = stats::median(job_value$elapsed_seconds),
      max_peak_RSS_KiB = max(job_value$peak_RSS_KiB),
      eligible = nrow(job_value) == 12L && all(job_value$ok) &&
        all(job_value$exact_joint_target) &&
        all(job_value$numerical_repair_count == 0L) &&
        all(job_value$peak_RSS_KiB <= memory_ceiling_KiB) &&
        all(job_value$all_transition_updates_exact) &&
        nrow(diagnostic_value) > 0L && all(diagnostic_value$pass) &&
        all(identity_value$target_identity_pass),
      stringsAsFactors = FALSE
    )
  }
))
selected <- candidate_summary[
  candidate_summary$eligible,
  , drop = FALSE
]
selected_id <- if (nrow(selected)) {
  selected$candidate_id[[which.min(selected$candidate_order)]]
} else {
  "none"
}
candidate_summary$selected <-
  candidate_summary$candidate_id == selected_id
rqr_confirm_atomic_write_csv(
  candidate_summary, file.path(output_root, "candidate_summary.csv")
)

failed_diagnostics <- diagnostics[!diagnostics$pass, , drop = FALSE]
rqr_confirm_atomic_write_csv(
  failed_diagnostics, file.path(output_root, "failed_diagnostics.csv")
)
manifest <- list(
  schema_version =
    "rqrgibbs_dlm_multicomponent_recovery/1.0.0",
  source_commit = source_commit,
  plan_digest = plan_digest,
  candidates = nrow(candidates), cases = nrow(cases), jobs = nrow(jobs),
  jobs_succeeded = sum(status$ok),
  diagnostics = nrow(diagnostics),
  diagnostics_passed = if (nrow(diagnostics)) {
    sum(diagnostics$pass)
  } else {
    0L
  },
  all_exact_joint_target = all(status$exact_joint_target),
  all_zero_repairs = all(status$numerical_repair_count == 0L),
  all_workers_within_memory_ceiling = all(
    status$peak_RSS_KiB <= memory_ceiling_KiB
  ),
  all_target_identities_passed = all(
    target_identity$target_identity_pass
  ),
  selected_candidate = selected_id,
  selection_rule =
    "first_all_case_eligible_candidate_by_predeclared_order",
  target_change = FALSE, schedule_change = FALSE,
  threshold_change = FALSE, adaptive_extension = FALSE,
  retries = 0L, reseeding = FALSE,
  scientific_metrics_used = FALSE, scientific_promotion = FALSE,
  confirmatory_launch_authorized = FALSE,
  seed_ledger_sha256 = expected_seed_sha256,
  primary_runtime_attestation_sha256 =
    rqr_confirm_sha256(primary_attestation),
  started_at_utc = started_at,
  completed_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
)
rqr_confirm_atomic_write_json(
  manifest, file.path(output_root, "comparison_manifest.json")
)
hashes <- rqr_confirm_recursive_manifest(output_root)
hashes <- hashes[hashes$path != "artifact_hashes.csv", , drop = FALSE]
rqr_confirm_atomic_write_csv(
  hashes, file.path(output_root, "artifact_hashes.csv")
)
cat(sprintf(
  paste0(
    "Multicomponent recovery comparison: %d/%d jobs and %d/%d ",
    "diagnostics passed; selected=%s.\n"
  ),
  sum(status$ok), nrow(status),
  if (nrow(diagnostics)) sum(diagnostics$pass) else 0L,
  nrow(diagnostics), selected_id
))
if (identical(selected_id, "none")) {
  stop("No predeclared multicomponent recovery candidate passed.",
       call. = FALSE)
}
