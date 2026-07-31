#!/usr/bin/env Rscript

# Development-only, predeclared comparison for the two MCMC failures in the
# first confirmatory RQR-DLM attempt. This script is not promotion evidence and
# cannot authorize a confirmatory launch. It compares exact target-preserving
# schedules on fixed hard and guard replications, then applies a deterministic
# least-cost selection rule without weakening any frozen diagnostic threshold.

args <- commandArgs(trailingOnly = TRUE)
repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(repo_root, "application", "DESCRIPTION"))) {
  stop("Run this script from the RQR-GIBBS repository root.", call. = FALSE)
}

parse_arg <- function(name, default = NULL) {
  prefix <- paste0("--", name, "=")
  match <- grep(paste0("^", prefix), args, value = TRUE)
  if (!length(match)) return(default)
  sub(prefix, "", match[[length(match)]])
}

if (any(args %in% c("-h", "--help"))) {
  cat(paste(
    "Usage:",
    "  Rscript application/scripts/37_compare_rqr_dlm_wave2_failure_candidates.R",
    "    --mode=<preflight|execute>",
    "    --seed-ledger=<authorization seed_ledger_maximum.csv>",
    "    [--output-root=application/cache/rqr_dlm_wave2_failure_candidates_<id>]",
    "    [--workers=6]",
    "",
    "The source checkout must be clean and fail-closed. Outputs are local-only",
    "development evidence; this script never edits the simulation contract.",
    sep = "\n"
  ))
  quit(save = "no", status = 0L, runLast = FALSE)
}

seed_ledger_path <- parse_arg("seed-ledger", "")
if (!nzchar(seed_ledger_path) || !file.exists(seed_ledger_path)) {
  stop("--seed-ledger must name the reviewed maximum seed ledger.",
       call. = FALSE)
}
seed_ledger_path <- normalizePath(
  seed_ledger_path, winslash = "/", mustWork = TRUE
)
expected_seed_ledger_sha256 <-
  "3dc8483f4a777ab766704b901997295bed1c89db0590429a70f3116b233e948f"
mode <- parse_arg("mode", "preflight")
if (!mode %in% c("preflight", "execute")) {
  stop("--mode must be preflight or execute.", call. = FALSE)
}
output_root <- parse_arg(
  "output-root",
  file.path(
    repo_root, "application", "cache",
    paste0("rqr_dlm_wave2_failure_candidates_", format(
      Sys.time(), tz = "UTC", format = "%Y%m%dT%H%M%SZ"
    ))
  )
)
output_root <- normalizePath(output_root, winslash = "/", mustWork = FALSE)
if (file.exists(output_root) || dir.exists(output_root)) {
  stop("The candidate-comparison output root must be new.", call. = FALSE)
}

workers <- suppressWarnings(as.integer(parse_arg("workers", "6")))
if (length(workers) != 1L || is.na(workers) || workers < 1L ||
    workers > 12L) {
  stop("--workers must be one integer in [1, 12].", call. = FALSE)
}

suppressPackageStartupMessages({
  pkgload::load_all(file.path(repo_root, "application"), quiet = TRUE)
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
    stop("Could not read the primary Git state.", call. = FALSE)
  }
  paste(value, collapse = "\n")
}
source_commit <- tolower(trimws(git_read(c("rev-parse", "HEAD"))))
source_status <- git_read(c(
  "status", "--porcelain=v2", "--untracked-files=all"
))
if (nzchar(source_status)) {
  stop("The candidate comparison requires a clean committed checkout.",
       call. = FALSE)
}

contract <- rqr_confirm_read_contract(repo_root)
rqr_confirm_validate_contract(contract, require_closed = TRUE)
if (isTRUE(contract$config$confirmatory_execution_authorized)) {
  stop("The candidate comparison requires a fail-closed contract.",
       call. = FALSE)
}
seed_ledger_sha256 <- rqr_confirm_sha256(seed_ledger_path)
if (!identical(seed_ledger_sha256, expected_seed_ledger_sha256)) {
  stop("The supplied seed ledger is not the reviewed authorization ledger.",
       call. = FALSE)
}

m03_candidates <- data.frame(
  method = "M03",
  candidate_id = c(
    "M03_current_B500_R1500_K1",
    "M03_burn_B3000_R1500_K1",
    "M03_retain_B500_R6000_K1",
    "M03_long_B3000_R6000_K1",
    "M03_compose_B1500_R3000_K2"
  ),
  candidate_order = 1:5,
  burn = c(500L, 3000L, 500L, 3000L, 1500L),
  retain = c(1500L, 1500L, 6000L, 6000L, 3000L),
  thin = 1L,
  kernel_repetitions = c(1L, 1L, 1L, 1L, 2L),
  role = c(
    "failed_baseline_reproduction",
    "burn_in_diagnostic",
    "retained_draw_diagnostic",
    "uniform_long_schedule",
    "complete_exact_kernel_composition"
  ),
  stringsAsFactors = FALSE
)
m08_candidates <- data.frame(
  method = "M08",
  candidate_id = c(
    "M08_current_B1000_R2000",
    "M08_uniform_B1000_R4000"
  ),
  candidate_order = 1:2,
  burn = 1000L,
  retain = c(2000L, 4000L),
  thin = 1L,
  kernel_repetitions = 1L,
  role = c("failed_baseline_reproduction", "uniform_retained_draws"),
  stringsAsFactors = FALSE
)
candidates <- rbind(m03_candidates, m08_candidates)
candidates$transition_cost <-
  (candidates$burn + candidates$retain * candidates$thin) *
  candidates$kernel_repetitions
candidates$target_change <- FALSE
candidates$threshold_change <- FALSE
candidates$adaptive_extension <- FALSE

cases <- rbind(
  data.frame(
    method = "M03", DGP = "S03",
    replication = c(117L, 13L, 90L, 185L),
    case_role = c("hard_four_chain", rep("guard_four_chain", 3L)),
    chains = 4L,
    stringsAsFactors = FALSE
  ),
  data.frame(
    method = "M08", DGP = "S03",
    replication = c(13L, 55L, 94L),
    case_role = c("hard_one_chain", rep("guard_one_chain", 2L)),
    chains = 1L,
    stringsAsFactors = FALSE
  )
)
jobs <- merge(candidates, cases, by = "method", sort = FALSE)
jobs <- jobs[order(
  match(jobs$method, c("M03", "M08")),
  jobs$candidate_order, jobs$replication,
  method = "radix"
), , drop = FALSE]
jobs$job_id <- sprintf(
  "%s__%02d__S03_rep%04d",
  jobs$method, jobs$candidate_order, jobs$replication
)
rownames(jobs) <- NULL

required_seed_keys <- character()
for (index in seq_len(nrow(cases))) {
  case <- cases[index, , drop = FALSE]
  scenario <- contract$config$scenarios[[case$DGP]]
  data_id <- paste(scenario$dgp, scenario$T, sep = "_T")
  required_seed_keys <- c(
    required_seed_keys,
    paste("training_state", scenario$pair, case$replication, sep = "|"),
    paste("training_response", data_id, case$replication, sep = "|")
  )
  for (subreplication in seq_len(
      contract$config$design$future_subreplications)) {
    required_seed_keys <- c(
      required_seed_keys,
      paste(
        "future_state", scenario$pair, case$replication,
        "subrep", subreplication, sep = "|"
      ),
      paste(
        "future_response", data_id, case$replication,
        "subrep", subreplication, sep = "|"
      )
    )
  }
  cell_id <- contract$incidence$cell_id[
    contract$incidence$DGP == case$DGP &
      contract$incidence$method == case$method
  ]
  required_seed_keys <- c(
    required_seed_keys,
    paste(
      "method", cell_id, case$replication,
      "interval", seq_len(case$chains), sep = "|"
    )
  )
}
required_seed_keys <- unique(required_seed_keys)

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("The development comparison requires data.table for seed selection.",
       call. = FALSE)
}
ledger_all <- data.table::fread(
  seed_ledger_path, data.table = FALSE, showProgress = FALSE,
  colClasses = "character"
)
ledger <- ledger_all[ledger_all$task_key %in% required_seed_keys, ,
                     drop = FALSE]
ledger_all <- NULL
invisible(gc(full = TRUE))
missing_seed_keys <- setdiff(required_seed_keys, ledger$task_key)
if (length(missing_seed_keys)) {
  stop(
    "The reviewed seed ledger lacks a required hard/guard state.",
    call. = FALSE
  )
}
ledger$substream <- suppressWarnings(as.integer(ledger$substream))
ledger <- rqr_confirm_validate_seed_ledger(
  ledger, contract, planning = "maximum", require_complete = FALSE
)
ledger <- ledger[match(required_seed_keys, ledger$task_key), , drop = FALSE]
chain_seed_bindings <- ledger[
  grepl("^method\\|", ledger$task_key),
  c("task_key", "state_digest"), drop = FALSE
]
chain_seed_bindings$origin <- "reviewed_authorization_seed_ledger"

thread_names <- c(
  "OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS",
  "VECLIB_MAXIMUM_THREADS", "NUMEXPR_NUM_THREADS"
)
thread_values <- setNames(
  vapply(thread_names, Sys.getenv, character(1L), unset = ""),
  thread_names
)
if (any(thread_values != "1")) {
  stop("All declared numerical thread variables must equal one.",
       call. = FALSE)
}

dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
rqr_confirm_atomic_write_csv(candidates, file.path(output_root, "candidates.csv"))
rqr_confirm_atomic_write_csv(cases, file.path(output_root, "cases.csv"))
rqr_confirm_atomic_write_csv(jobs, file.path(output_root, "jobs.csv"))
rqr_confirm_atomic_write_csv(
  chain_seed_bindings,
  file.path(output_root, "chain_seed_bindings.csv")
)

if (identical(mode, "preflight")) {
  rqr_confirm_atomic_write_json(
    list(
      schema_version =
        "rqrgibbs_dlm_wave2_candidate_preflight/1.0.0",
      source_commit = source_commit,
      source_clean = TRUE,
      fail_closed = TRUE,
      seed_ledger_sha256 = seed_ledger_sha256,
      jobs = nrow(jobs),
      M03_fit_executions = sum(
        jobs$method == "M03" & jobs$chains == 4L
      ) * 4L,
      M08_fit_executions = sum(
        jobs$method == "M08" & jobs$chains == 1L
      ),
      thresholds_unchanged = TRUE,
      exact_target_candidates_only = TRUE,
      confirmatory_authorization_changed = FALSE,
      fits_executed = 0L,
      generalized_bayes = TRUE,
      response_likelihood = FALSE,
      completed_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
    ),
    file.path(output_root, "preflight_manifest.json")
  )
  hashes <- rqr_confirm_recursive_manifest(output_root)
  rqr_confirm_atomic_write_csv(
    hashes, file.path(output_root, "artifact_hashes.csv")
  )
  invisible(rqr_confirm_verify_recursive_manifest(output_root))
  cat("Development candidate preflight passed; no fits executed.\n")
  cat("  source commit:", source_commit, "\n")
  cat("  output root:", output_root, "\n")
  quit(save = "no", status = 0L, runLast = FALSE)
}

started_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
results <- parallel::mclapply(
  seq_len(nrow(jobs)), mc.cores = min(workers, nrow(jobs)),
  mc.preschedule = TRUE,
  FUN = function(index) {
    job <- jobs[index, , drop = FALSE]
    tryCatch({
      generated <- rqr_confirm_generate_dgp(
        contract, job$DGP[[1L]], job$replication[[1L]], ledger
      )
      scalar_chains <- vector("list", job$chains[[1L]])
      fit_elapsed <- numeric(job$chains[[1L]])
      exact <- logical(job$chains[[1L]])
      repairs <- integer(job$chains[[1L]])
      profiles <- vapply(seq_len(job$chains[[1L]]), function(chain) {
        rqr_confirm_initialization_profile_name(
          job$chains[[1L]] == 4L, chain
        )
      }, character(1L))
      for (chain in seq_len(job$chains[[1L]])) {
        override <- list(
          n_burn = job$burn[[1L]],
          n_mcmc = job$retain[[1L]],
          thin = job$thin[[1L]]
        )
        started <- proc.time()[["elapsed"]]
        if (identical(job$method[[1L]], "M03")) {
          override$kernel_repetitions <-
            job$kernel_repetitions[[1L]]
          value <- rqr_confirm_fixed_design(
            contract, generated, chain, ledger,
            provenance_control = list(),
            profile_name = profiles[[chain]],
            mcmc_control_override = override
          )
        } else {
          value <- rqr_confirm_dynamic_fit(
            contract, generated, job$method[[1L]], chain, ledger,
            provenance_control = list(),
            profile_name = profiles[[chain]],
            mcmc_control_override = override
          )
        }
        fit_elapsed[[chain]] <-
          proc.time()[["elapsed"]] - started
        scalar_chains[[chain]] <- rqr_confirm_scalar_draws(
          value, generated, contract, job$method[[1L]]
        )
        exact[[chain]] <- isTRUE(value$fit$model_spec$exact_joint_target)
        repairs[[chain]] <-
          as.integer(value$fit$model_spec$numerical_repair_count)
        value <- NULL
        invisible(gc(full = TRUE))
      }
      diagnostics <- rqr_confirm_chain_diagnostics(
        scalar_chains, contract,
        sentinel = job$chains[[1L]] == 4L,
        method = job$method[[1L]], generated = generated
      )
      diagnostics$DGP <- job$DGP[[1L]]
      diagnostics$replication <- job$replication[[1L]]
      diagnostics$method <- job$method[[1L]]
      diagnostics$sentinel <- job$chains[[1L]] == 4L
      list(
        ok = TRUE,
        job = as.list(job),
        profiles = profiles,
        fit_elapsed_seconds = fit_elapsed,
        scalar_chains = scalar_chains,
        diagnostics = diagnostics,
        exact_joint_target = exact,
        numerical_repair_count = repairs,
        peak_RSS_KiB = rqr_confirm_process_peak_rss_kib()
      )
    }, error = function(error) {
      list(
        ok = FALSE, job = as.list(job),
        error_class = class(error)[[1L]],
        message_digest = digest::digest(
          conditionMessage(error), algo = "sha256", serialize = FALSE
        ),
        peak_RSS_KiB = rqr_confirm_process_peak_rss_kib()
      )
    })
  }
)
completed_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)

diagnostic_rows <- lapply(results, function(result) {
  job <- result$job
  if (!isTRUE(result$ok)) {
    rows <- data.frame(
      estimand = "fit_error", chains = job$chains,
      rhat = NA_real_, ess_bulk = NA_real_, ess_tail = NA_real_,
      mcse_mean = NA_real_, mcse_over_sd = NA_real_, pass = FALSE,
      DGP = job$DGP, replication = job$replication,
      method = job$method, sentinel = job$chains == 4L,
      stringsAsFactors = FALSE
    )
  } else {
    rows <- result$diagnostics
  }
  rows$candidate_id <- job$candidate_id
  rows$candidate_order <- job$candidate_order
  rows$case_role <- job$case_role
  rows$transition_cost <- job$transition_cost
  rows$effective_draws_per_second <- if (isTRUE(result$ok)) {
    rows$ess_bulk / sum(result$fit_elapsed_seconds)
  } else {
    NA_real_
  }
  rows
})
diagnostics <- do.call(rbind, diagnostic_rows)
diagnostics <- diagnostics[order(
  match(diagnostics$method, c("M03", "M08")),
  diagnostics$candidate_order, diagnostics$replication,
  diagnostics$estimand, method = "radix"
), , drop = FALSE]

job_summary <- do.call(rbind, lapply(results, function(result) {
  job <- result$job
  rows <- diagnostics[
    diagnostics$candidate_id == job$candidate_id &
      diagnostics$replication == job$replication, , drop = FALSE
  ]
  data.frame(
    method = job$method,
    candidate_id = job$candidate_id,
    candidate_order = job$candidate_order,
    DGP = job$DGP,
    replication = job$replication,
    case_role = job$case_role,
    chains = job$chains,
    ok = isTRUE(result$ok),
    diagnostics = nrow(rows),
    diagnostics_passed = sum(rows$pass),
    all_diagnostics_pass = all(rows$pass),
    min_bulk_ess = suppressWarnings(min(rows$ess_bulk, na.rm = TRUE)),
    min_tail_ess = suppressWarnings(min(rows$ess_tail, na.rm = TRUE)),
    max_rhat = suppressWarnings(max(rows$rhat, na.rm = TRUE)),
    max_mcse_over_sd =
      suppressWarnings(max(rows$mcse_over_sd, na.rm = TRUE)),
    exact_joint_target = isTRUE(result$ok) &&
      all(result$exact_joint_target),
    numerical_repair_count = if (isTRUE(result$ok)) {
      sum(result$numerical_repair_count)
    } else {
      NA_integer_
    },
    fit_elapsed_seconds = if (isTRUE(result$ok)) {
      sum(result$fit_elapsed_seconds)
    } else {
      NA_real_
    },
    peak_RSS_KiB = as.numeric(result$peak_RSS_KiB %||% NA_real_),
    error_class = as.character(result$error_class %||% ""),
    message_digest = as.character(result$message_digest %||% ""),
    stringsAsFactors = FALSE
  )
}))
numeric_nonfinite <- c(
  "min_bulk_ess", "min_tail_ess", "max_rhat", "max_mcse_over_sd"
)
for (field in numeric_nonfinite) {
  job_summary[[field]][!is.finite(job_summary[[field]])] <- NA_real_
}

candidate_decisions <- do.call(rbind, lapply(
  split(candidates, candidates$method),
  function(method_candidates) {
    rows <- lapply(seq_len(nrow(method_candidates)), function(index) {
      candidate <- method_candidates[index, , drop = FALSE]
      jobs_for_candidate <- job_summary[
        job_summary$candidate_id == candidate$candidate_id, , drop = FALSE
      ]
      eligible <- nrow(jobs_for_candidate) > 0L &&
        all(jobs_for_candidate$ok) &&
        all(jobs_for_candidate$all_diagnostics_pass) &&
        all(jobs_for_candidate$exact_joint_target) &&
        all(jobs_for_candidate$numerical_repair_count == 0L)
      data.frame(
        method = candidate$method,
        candidate_id = candidate$candidate_id,
        candidate_order = candidate$candidate_order,
        transition_cost = candidate$transition_cost,
        hard_and_guard_pass = eligible,
        selected = FALSE,
        selection_rule = paste(
          "all fixed cases pass unchanged gates; zero repairs; exact target;",
          "then minimum transition cost and candidate order"
        ),
        stringsAsFactors = FALSE
      )
    })
    rows <- do.call(rbind, rows)
    eligible <- which(rows$hard_and_guard_pass)
    if (length(eligible)) {
      selected <- eligible[order(
        rows$transition_cost[eligible],
        rows$candidate_order[eligible], method = "radix"
      )[[1L]]]
      rows$selected[[selected]] <- TRUE
    }
    rows
  }
))
candidate_decisions <- candidate_decisions[order(
  match(candidate_decisions$method, c("M03", "M08")),
  candidate_decisions$candidate_order
), , drop = FALSE]

acf_lags <- c(1L, 5L, 10L, 25L, 50L)
acf_rows <- list()
acf_index <- 0L
for (result in results) {
  if (!isTRUE(result$ok)) next
  for (chain in seq_along(result$scalar_chains)) {
    scalar <- result$scalar_chains[[chain]]
    variables <- intersect(
      c(
        "observed_loss", "mean_width", "mean_midpoint",
        "terminal_width", "terminal_midpoint", "log_q_1"
      ),
      colnames(scalar)
    )
    for (variable in variables) {
      acf_index <- acf_index + 1L
      value <- as.numeric(scalar[, variable])
      estimate <- rep(NA_real_, length(acf_lags))
      if (length(value) > max(acf_lags) && stats::sd(value) > 0) {
        raw <- stats::acf(
          value, lag.max = max(acf_lags), plot = FALSE,
          na.action = stats::na.pass
        )$acf
        estimate <- as.numeric(raw[acf_lags + 1L])
      }
      acf_rows[[acf_index]] <- data.frame(
        method = result$job$method,
        candidate_id = result$job$candidate_id,
        DGP = result$job$DGP,
        replication = result$job$replication,
        case_role = result$job$case_role,
        chain = chain,
        profile = result$profiles[[chain]],
        estimand = variable,
        lag = acf_lags,
        acf = estimate,
        stringsAsFactors = FALSE
      )
    }
  }
}
acf_table <- if (length(acf_rows)) do.call(rbind, acf_rows) else data.frame()

rqr_confirm_atomic_write_csv(
  diagnostics, file.path(output_root, "diagnostics.csv")
)
rqr_confirm_atomic_write_csv(
  job_summary, file.path(output_root, "job_summary.csv")
)
rqr_confirm_atomic_write_csv(
  candidate_decisions, file.path(output_root, "candidate_decisions.csv")
)
rqr_confirm_atomic_write_csv(
  acf_table, file.path(output_root, "autocorrelation.csv")
)

atomic_rds <- function(value, path) {
  temporary <- tempfile(".rqr-candidate-", tmpdir = dirname(path))
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  saveRDS(value, temporary, compress = "xz")
  if (!file.rename(temporary, path)) {
    stop("Could not atomically publish candidate chain evidence.",
         call. = FALSE)
  }
}
atomic_rds(
  list(
    schema_version = "rqrgibbs_dlm_wave2_candidate_evidence/1.0.0",
    source_commit = source_commit,
    seed_ledger_sha256 = seed_ledger_sha256,
    candidates = candidates,
    cases = cases,
    chain_seed_bindings = chain_seed_bindings,
    jobs = jobs,
    results = results,
    generalized_bayes = TRUE,
    response_likelihood = FALSE,
    promotion_evidence = FALSE
  ),
  file.path(output_root, "chain_evidence_ignored.rds")
)

manifest <- list(
  schema_version = "rqrgibbs_dlm_wave2_candidate_comparison/1.0.0",
  source_commit = source_commit,
  source_clean = TRUE,
  config_sha256 = rqr_confirm_sha256(file.path(
    repo_root, "application", "config", "rqr_dlm",
    "rqr_dlm_main_simulation_20260724.R"
  )),
  seed_ledger_path = seed_ledger_path,
  seed_ledger_sha256 = seed_ledger_sha256,
  workers = workers,
  thread_environment = as.list(thread_values),
  jobs = nrow(jobs),
  successful_jobs = sum(vapply(results, function(x) isTRUE(x$ok), logical(1L))),
  selected_M03 = candidate_decisions$candidate_id[
    candidate_decisions$method == "M03" & candidate_decisions$selected
  ] %||% character(),
  selected_M08 = candidate_decisions$candidate_id[
    candidate_decisions$method == "M08" & candidate_decisions$selected
  ] %||% character(),
  thresholds_unchanged = TRUE,
  exact_target_candidates_only = TRUE,
  no_adaptive_extension = TRUE,
  confirmatory_authorization_changed = FALSE,
  promotion_evidence = FALSE,
  generalized_bayes = TRUE,
  response_likelihood = FALSE,
  started_at_utc = started_at,
  completed_at_utc = completed_at
)
rqr_confirm_atomic_write_json(
  manifest, file.path(output_root, "manifest.json")
)
hashes <- rqr_confirm_recursive_manifest(output_root)
rqr_confirm_atomic_write_csv(
  hashes, file.path(output_root, "artifact_hashes.csv")
)
rqr_confirm_verify_recursive_manifest(output_root)

cat("Development-only RQR-DLM wave-2 comparison completed.\n")
cat("  source commit:", source_commit, "\n")
cat("  output root:", output_root, "\n")
cat("  successful jobs:", manifest$successful_jobs, "/", nrow(jobs), "\n")
cat("  selected M03:", paste(manifest$selected_M03, collapse = ","), "\n")
cat("  selected M08:", paste(manifest$selected_M08, collapse = ","), "\n")
