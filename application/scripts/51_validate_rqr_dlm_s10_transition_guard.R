#!/usr/bin/env Rscript

# Four-chain, higher-dimensional guard for the selected M10/M11 transition
# policies. This is bounded computational validation only; it cannot authorize
# or contribute scientific simulation results.

args <- commandArgs(trailingOnly = TRUE)
parse_arg <- function(name, default = "") {
  prefix <- paste0("--", name, "=")
  value <- grep(paste0("^", prefix), args, value = TRUE)
  if (!length(value)) return(default)
  sub(prefix, "", value[[length(value)]], fixed = TRUE)
}

repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
output_root <- parse_arg("output-root")
workers <- suppressWarnings(as.integer(parse_arg("workers", "8")))
expected_commit <- tolower(Sys.getenv(
  "RQR_EXPECTED_PRIMARY_COMMIT", unset = ""
))
primary_attestation <- Sys.getenv(
  "RQR_PRIMARY_RUNTIME_ATTESTATION", unset = ""
)
if (!nzchar(output_root) || file.exists(output_root) || dir.exists(output_root) ||
    length(workers) != 1L || is.na(workers) || workers < 1L || workers > 8L ||
    !grepl("^[0-9a-f]{40}$", expected_commit) ||
    !file.exists(primary_attestation)) {
  stop(
    "Fresh output, 1--8 workers, exact commit, and primary attestation are required.",
    call. = FALSE
  )
}
output_root <- normalizePath(output_root, winslash = "/", mustWork = FALSE)

suppressPackageStartupMessages(library(rqrgibbs))
source(file.path(
  repo_root, "application", "scripts", "lib",
  "rqr_dlm_confirmatory_simulation.R"
))
contract <- rqr_confirm_read_contract(repo_root)
rqr_confirm_validate_contract(contract, require_closed = TRUE)
rqr_confirm_validate_budget(contract)
memory_ceiling_KiB <- as.numeric(
  contract$config$resources$per_worker_memory_GiB
) * 1024^2
if (length(memory_ceiling_KiB) != 1L ||
    !is.finite(memory_ceiling_KiB) || memory_ceiling_KiB <= 0) {
  stop("The frozen per-worker memory ceiling is invalid.", call. = FALSE)
}

git_read <- function(arguments) {
  value <- system2(
    Sys.which("git"), c("-C", shQuote(repo_root), arguments),
    stdout = TRUE, stderr = TRUE,
    env = c("GIT_OPTIONAL_LOCKS=0", "GIT_TERMINAL_PROMPT=0")
  )
  status <- attr(value, "status") %||% 0L
  if (!identical(as.integer(status), 0L)) {
    stop("Could not authenticate the primary source.", call. = FALSE)
  }
  trimws(paste(value, collapse = "\n"))
}
if (!identical(tolower(git_read(c("rev-parse", "HEAD"))),
               expected_commit) ||
    nzchar(git_read(c(
      "status", "--porcelain=v2", "--untracked-files=all"
    )))) {
  stop("The S10 guard requires the exact clean source commit.",
       call. = FALSE)
}
thread_names <- c(
  "OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS",
  "VECLIB_MAXIMUM_THREADS", "NUMEXPR_NUM_THREADS"
)
if (any(vapply(thread_names, Sys.getenv, character(1L), unset = "") != "1")) {
  stop("Every numerical-library thread control must equal one.",
       call. = FALSE)
}
provenance_control <- rqr_confirm_primary_provenance_control(
  repo_root, expected_commit, primary_attestation
)
ledger <- rqr_confirm_seed_ledger(contract, planning = "maximum")
invisible(rqr_confirm_validate_seed_ledger(
  ledger, contract, planning = "maximum", require_complete = TRUE
))
invisible(rqr_confirm_validate_planned_method_rng_bindings(
  ledger, contract, planning = "maximum"
))

cases <- data.frame(
  method = c("M10", "M11"),
  DGP = "S10",
  replication = c(77L, 166L),
  case_role = "higher_dimensional_sentinel_guard",
  stringsAsFactors = FALSE
)
sentinels <- rqr_confirm_sentinel_map(contract, planning = "maximum")
if (any(!vapply(seq_len(nrow(cases)), function(index) {
    cell <- contract$incidence$cell_id[
      contract$incidence$DGP == cases$DGP[[index]] &
        contract$incidence$method == cases$method[[index]]
    ]
    any(sentinels$cell_id == cell &
          sentinels$replication == cases$replication[[index]])
  }, logical(1L)))) {
  stop("The S10 guard cases are not frozen sentinel replications.",
       call. = FALSE)
}
jobs <- do.call(rbind, lapply(seq_len(nrow(cases)), function(index) {
  data.frame(
    job_id = sprintf(
      "%s__S10_rep%04d__chain%02d",
      cases$method[[index]], cases$replication[[index]], 1:4
    ),
    method = cases$method[[index]], DGP = "S10",
    replication = cases$replication[[index]], chain = 1:4,
    profile = c("A", "B", "C", "D"),
    case_role = cases$case_role[[index]],
    stringsAsFactors = FALSE
  )
}))
if (nrow(jobs) != 8L || anyDuplicated(jobs$job_id)) {
  stop("The S10 guard job contract is invalid.", call. = FALSE)
}

dir.create(file.path(output_root, "job_results"),
           recursive = TRUE, showWarnings = FALSE)
rqr_confirm_atomic_write_csv(
  ledger, file.path(output_root, "seed_ledger_maximum.csv")
)
expected_seed_sha256 <-
  "3dc8483f4a777ab766704b901997295bed1c89db0590429a70f3116b233e948f"
if (!identical(
    rqr_confirm_sha256(file.path(output_root, "seed_ledger_maximum.csv")),
    expected_seed_sha256
  )) {
  stop("The frozen seed ledger byte digest changed.", call. = FALSE)
}
rqr_confirm_atomic_write_csv(cases, file.path(output_root, "cases.csv"))
rqr_confirm_atomic_write_csv(jobs, file.path(output_root, "jobs.csv"))

peak_rss_kib <- function() {
  status <- readLines("/proc/self/status", warn = FALSE)
  value <- sub("^VmHWM:[[:space:]]*", "", grep("^VmHWM:", status, value = TRUE))
  as.numeric(sub("[[:space:]]*kB$", "", value))
}
run_job <- function(index) {
  job <- jobs[index, , drop = FALSE]
  started <- proc.time()[["elapsed"]]
  observed <- tryCatch({
    generated <- rqr_confirm_generate_dgp(
      contract, job$DGP[[1L]], job$replication[[1L]], ledger
    )
    fitted <- rqr_confirm_dynamic_fit(
      contract, generated, job$method[[1L]], job$chain[[1L]], ledger,
      provenance_control = provenance_control,
      profile_name = job$profile[[1L]]
    )
    scalars <- rqr_confirm_scalar_draws(
      fitted, generated, contract, job$method[[1L]]
    )
    list(
      ok = TRUE, message = "", job = job, scalars = scalars,
      exact_joint_target = isTRUE(
        fitted$fit$model_spec$exact_joint_target
      ),
      numerical_repair_count =
        fitted$fit$model_spec$numerical_repair_count,
      transition_policy = fitted$diagnostics$transition_policy
    )
  }, error = function(error) {
    list(
      ok = FALSE, message = conditionMessage(error), job = job,
      scalars = NULL, exact_joint_target = FALSE,
      numerical_repair_count = NA_integer_, transition_policy = NULL
    )
  })
  observed$elapsed_seconds <- proc.time()[["elapsed"]] - started
  observed$peak_RSS_KiB <- peak_rss_kib()
  path <- file.path(
    output_root, "job_results", paste0(job$job_id[[1L]], ".rds")
  )
  temporary <- tempfile(paste0(".", basename(path), "-"), dirname(path))
  saveRDS(observed, temporary, version = 3L)
  if (!file.rename(temporary, path)) {
    stop("Could not atomically publish an S10 guard result.",
         call. = FALSE)
  }
  observed
}

started_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
results <- parallel::mclapply(
  seq_len(nrow(jobs)), run_job,
  mc.cores = min(workers, nrow(jobs)), mc.preschedule = FALSE
)
status <- do.call(rbind, lapply(results, function(value) {
  data.frame(
    job_id = value$job$job_id, method = value$job$method,
    DGP = value$job$DGP, replication = value$job$replication,
    chain = value$job$chain, profile = value$job$profile,
    ok = value$ok, exact_joint_target = value$exact_joint_target,
    numerical_repair_count = value$numerical_repair_count,
    elapsed_seconds = value$elapsed_seconds,
    peak_RSS_KiB = value$peak_RSS_KiB,
    message = value$message, retry_count = 0L, reseeded = FALSE,
    stringsAsFactors = FALSE
  )
}))
rqr_confirm_atomic_write_csv(status, file.path(output_root, "job_status.csv"))

diagnostic_rows <- list()
if (all(status$ok) && all(status$exact_joint_target) &&
    all(status$numerical_repair_count == 0L)) {
  for (index in seq_len(nrow(cases))) {
    selected <- which(
      jobs$method == cases$method[[index]] &
        jobs$replication == cases$replication[[index]]
    )
    generated <- rqr_confirm_generate_dgp(
      contract, cases$DGP[[index]], cases$replication[[index]], ledger
    )
    value <- rqr_confirm_chain_diagnostics(
      lapply(results[selected], `[[`, "scalars"), contract,
      sentinel = TRUE, method = cases$method[[index]],
      generated = generated
    )
    value$method <- cases$method[[index]]
    value$DGP <- cases$DGP[[index]]
    value$replication <- cases$replication[[index]]
    value$case_role <- cases$case_role[[index]]
    diagnostic_rows[[index]] <- value
  }
}
diagnostics <- if (length(diagnostic_rows)) {
  do.call(rbind, diagnostic_rows)
} else {
  data.frame()
}
rqr_confirm_atomic_write_csv(
  diagnostics, file.path(output_root, "fit_diagnostics.csv")
)
passed <- nrow(diagnostics) > 0L && all(diagnostics$pass) &&
  all(status$ok) && all(status$exact_joint_target) &&
  all(status$numerical_repair_count == 0L) &&
  all(is.finite(status$peak_RSS_KiB)) &&
  all(status$peak_RSS_KiB <= memory_ceiling_KiB) &&
  all(status$retry_count == 0L) && !any(status$reseeded)
manifest <- list(
  schema_version = "rqrgibbs_dlm_s10_transition_guard/1.0.0",
  source_commit = expected_commit,
  cases = nrow(cases), jobs = nrow(jobs),
  jobs_succeeded = sum(status$ok), diagnostics = nrow(diagnostics),
  diagnostics_passed = if (nrow(diagnostics)) sum(diagnostics$pass) else 0L,
  all_exact_joint_target = all(status$exact_joint_target),
  all_zero_repairs = all(status$numerical_repair_count == 0L),
  peak_worker_RSS_KiB = max(status$peak_RSS_KiB),
  per_worker_memory_ceiling_KiB = memory_ceiling_KiB,
  all_workers_within_memory_ceiling = all(
    status$peak_RSS_KiB <= memory_ceiling_KiB
  ),
  selected_policies_passed = passed,
  seed_ledger_sha256 = expected_seed_sha256,
  retries = 0L, reseeding = FALSE,
  scientific_metrics_used = FALSE, scientific_promotion = FALSE,
  confirmatory_launch_authorized = FALSE,
  started_at_utc = started_at,
  completed_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
)
rqr_confirm_atomic_write_json(
  manifest, file.path(output_root, "guard_manifest.json")
)
hashes <- rqr_confirm_recursive_manifest(output_root)
hashes <- hashes[hashes$path != "artifact_hashes.csv", , drop = FALSE]
rqr_confirm_atomic_write_csv(
  hashes, file.path(output_root, "artifact_hashes.csv")
)
cat(sprintf(
  "S10 transition guard: %d/%d jobs and %d/%d diagnostics passed.\n",
  sum(status$ok), nrow(status),
  if (nrow(diagnostics)) sum(diagnostics$pass) else 0L,
  nrow(diagnostics)
))
if (!passed) stop("The selected S10 transition guard failed.", call. = FALSE)
