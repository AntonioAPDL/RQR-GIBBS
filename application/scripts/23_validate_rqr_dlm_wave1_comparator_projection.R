#!/usr/bin/env Rscript

# Re-execute every M02 interval-chain job represented in the first failed
# confirmatory wave. This validates the corrected state-to-ordinate projection
# and the original M02 diagnostic contract. It is computational validation,
# not a comparative simulation result.

args <- commandArgs(trailingOnly = TRUE)
repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(repo_root, "application", "DESCRIPTION"))) {
  stop("Run this script from the RQR-GIBBS repository root.", call. = FALSE)
}
output_root <- if (length(args)) {
  normalizePath(args[[1L]], winslash = "/", mustWork = FALSE)
} else {
  file.path(
    repo_root, "application", "cache",
    "rqr_dlm_wave1_comparator_projection_validation"
  )
}
if (file.exists(output_root) || dir.exists(output_root)) {
  stop("The comparator-validation output root must be new.", call. = FALSE)
}
dir.create(output_root, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages(library(rqrgibbs))
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
expected_commit <- tolower(Sys.getenv(
  "RQR_EXPECTED_PRIMARY_COMMIT", unset = ""
))
primary_attestation <- Sys.getenv(
  "RQR_PRIMARY_RUNTIME_ATTESTATION", unset = ""
)
exdqlm_attestation <- Sys.getenv(
  "RQR_EXDQLM_RUNTIME_ATTESTATION", unset = ""
)
if (!nzchar(exdqlm_attestation) ||
    !file.exists(exdqlm_attestation)) {
  stop(
    "RQR_EXDQLM_RUNTIME_ATTESTATION must name an existing attestation.",
    call. = FALSE
  )
}
exdqlm_attestation <- normalizePath(
  exdqlm_attestation, winslash = "/", mustWork = TRUE
)
if (nzchar(expected_commit) &&
    (!grepl("^[0-9a-f]{40}$", expected_commit) ||
      !identical(source_commit, expected_commit) ||
      nzchar(source_status))) {
  stop(
    "The comparator gate is not at the exact clean expected commit.",
    call. = FALSE
  )
}
if (xor(nzchar(expected_commit), nzchar(primary_attestation))) {
  stop(
    paste(
      "RQR_EXPECTED_PRIMARY_COMMIT and",
      "RQR_PRIMARY_RUNTIME_ATTESTATION must be supplied together."
    ),
    call. = FALSE
  )
}

thread_variables <- c(
  "OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS",
  "VECLIB_MAXIMUM_THREADS", "NUMEXPR_NUM_THREADS"
)
thread_environment <- setNames(
  vapply(
    thread_variables,
    Sys.getenv,
    character(1L),
    unset = ""
  ),
  thread_variables
)
if (nzchar(expected_commit) &&
    any(thread_environment != "1")) {
  stop(
    "Every declared numerical-library thread limit must equal one.",
    call. = FALSE
  )
}

contract <- rqr_confirm_read_contract(repo_root)
rqr_confirm_validate_contract(contract, require_closed = TRUE)
specification <- contract$config$comparator$exdqlm
external_attestation <- rqr_confirm_read_attestation(
  exdqlm_attestation, "exdqlm", specification$version,
  specification$source_sha256
)
primary_provenance <- NULL
if (nzchar(expected_commit)) {
  control <- rqr_confirm_primary_provenance_control(
    repo_root, expected_commit, primary_attestation
  )
  primary_provenance <- rqrgibbs:::.rqr_provenance(
    data = numeric(),
    repo_root = control$repo_root,
    expected_git_commit = control$expected_git_commit,
    numerical_policy = "fail",
    backend = "cpp",
    primary_runtime_attestation =
      control$primary_runtime_attestation
  )
  if (!isTRUE(primary_provenance$reproducibility_eligible)) {
    stop(
      "The executing primary package is not bound to the exact source.",
      call. = FALSE
    )
  }
}

ledger <- rqr_confirm_seed_ledger(contract, planning = "maximum")
wave_id <- "static_gaussian_T200__target0200__sentinel"
wave_plan <- rqr_confirm_wave_plan(contract, planning = "maximum")
wave_tasks <- wave_plan[
  wave_plan$wave_id == wave_id &
    grepl("(^|\\|)M02(\\||$)", wave_plan$methods),
  ,
  drop = FALSE
]
if (nrow(wave_tasks) != 20L ||
    !all(wave_tasks$embedded_sentinel)) {
  stop("The canonical first-wave M02 task contract changed.",
       call. = FALSE)
}
sentinels <- rqr_confirm_sentinel_map(contract, planning = "maximum")

jobs <- vector("list", 0L)
for (row in seq_len(nrow(wave_tasks))) {
  scenario_id <- wave_tasks$DGP[[row]]
  replication <- wave_tasks$replication[[row]]
  cell_id <- contract$incidence$cell_id[
    contract$incidence$DGP == scenario_id &
      contract$incidence$method == "M02"
  ]
  embedded <- any(
    sentinels$cell_id == cell_id &
      sentinels$replication == replication
  )
  chains <- if (embedded) 4L else 1L
  for (chain in seq_len(chains)) {
    jobs[[length(jobs) + 1L]] <- list(
      scenario_id = scenario_id,
      replication = replication,
      chain = chain,
      sentinel = embedded,
      profile = if (embedded) {
        c("A", "B", "C", "D")[[chain]]
      } else {
        "standard"
      }
    )
  }
}
if (length(jobs) != 44L) {
  stop("The canonical comparator gate must contain 44 interval chains.",
       call. = FALSE)
}
workers <- as.integer(Sys.getenv(
  "RQR_COMPARATOR_CORRECTION_WORKERS", unset = "8"
))
if (is.na(workers) || workers < 1L || workers > length(jobs)) {
  stop(
    "RQR_COMPARATOR_CORRECTION_WORKERS must be an integer from 1 through 44.",
    call. = FALSE
  )
}

started_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
results <- parallel::mclapply(
  jobs,
  mc.cores = workers,
  mc.preschedule = TRUE,
  FUN = function(job) {
    tryCatch({
      generated <- rqr_confirm_generate_dgp(
        contract, job$scenario_id, job$replication, ledger
      )
      fit_started <- proc.time()[["elapsed"]]
      value <- rqr_confirm_dynamic_quantile(
        contract = contract,
        generated = generated,
        chain = job$chain,
        ledger = ledger,
        exdqlm_attestation_path = exdqlm_attestation,
        profile_name = job$profile
      )
      list(
        ok = TRUE,
        job = job,
        fit_elapsed_seconds =
          as.numeric(proc.time()[["elapsed"]] - fit_started),
        scalars = rqr_confirm_scalar_draws(
          value, generated, contract, "M02"
        )
      )
    }, error = function(error) {
      list(
        ok = FALSE, job = job,
        error_class = class(error)[[1L]],
        message = conditionMessage(error)
      )
    })
  }
)
completed_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)

job_key <- function(job) {
  paste(job$scenario_id, sprintf("%04d", job$replication), sep = "|")
}
result_keys <- vapply(
  results, function(result) job_key(result$job), character(1L)
)
task_keys <- unique(result_keys)
diagnostics <- vector("list", length(task_keys))
summary_rows <- vector("list", length(task_keys))
for (index in seq_along(task_keys)) {
  selected <- results[result_keys == task_keys[[index]]]
  first <- selected[[1L]]$job
  successful <- vapply(selected, `[[`, logical(1L), "ok")
  if (!all(successful)) {
    diagnostics[[index]] <- data.frame(
      DGP = first$scenario_id,
      replication = first$replication,
      sentinel = first$sentinel,
      estimand = "fit_error",
      chains = length(selected),
      rhat = NA_real_, ess_bulk = NA_real_, ess_tail = NA_real_,
      mcse_mean = NA_real_, mcse_over_sd = NA_real_, pass = FALSE,
      stringsAsFactors = FALSE
    )
  } else {
    generated <- rqr_confirm_generate_dgp(
      contract, first$scenario_id, first$replication, ledger
    )
    value <- rqr_confirm_chain_diagnostics(
      lapply(selected, `[[`, "scalars"),
      contract = contract, sentinel = first$sentinel,
      method = "M02", generated = generated
    )
    value$DGP <- first$scenario_id
    value$replication <- first$replication
    value$sentinel <- first$sentinel
    diagnostics[[index]] <- value
  }
  value <- diagnostics[[index]]
  summary_rows[[index]] <- data.frame(
    DGP = first$scenario_id,
    replication = first$replication,
    sentinel = first$sentinel,
    chains = length(selected),
    diagnostics = nrow(value),
    diagnostics_passed = sum(value$pass),
    all_pass = all(value$pass),
    fit_elapsed_seconds = sum(vapply(
      selected,
      function(result) {
        as.numeric(result$fit_elapsed_seconds %||% NA_real_)
      },
      numeric(1L)
    )),
    minimum_bulk_ess = suppressWarnings(min(value$ess_bulk, na.rm = TRUE)),
    minimum_tail_ess = suppressWarnings(min(value$ess_tail, na.rm = TRUE)),
    maximum_mcse_over_sd =
      suppressWarnings(max(value$mcse_over_sd, na.rm = TRUE)),
    stringsAsFactors = FALSE
  )
}
diagnostics <- do.call(rbind, diagnostics)
summary <- do.call(rbind, summary_rows)
summary[!is.finite(summary$minimum_bulk_ess), "minimum_bulk_ess"] <-
  NA_real_
summary[!is.finite(summary$minimum_tail_ess), "minimum_tail_ess"] <-
  NA_real_
summary[
  !is.finite(summary$maximum_mcse_over_sd),
  "maximum_mcse_over_sd"
] <- NA_real_

atomic_rds <- function(value, path) {
  temporary <- tempfile(
    paste0(".", basename(path), "-"), tmpdir = dirname(path)
  )
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  saveRDS(value, temporary, version = 3L)
  if (!file.rename(temporary, path)) {
    stop(
      "Could not atomically publish the comparator-validation RDS.",
      call. = FALSE
    )
  }
}
atomic_rds(
  list(jobs = jobs, results = results, diagnostics = diagnostics),
  file.path(output_root, "wave1_M02_chain_evidence.rds")
)
rqr_confirm_atomic_write_csv(
  diagnostics, file.path(output_root, "wave1_M02_diagnostics.csv")
)
rqr_confirm_atomic_write_csv(
  summary, file.path(output_root, "wave1_M02_summary.csv")
)
manifest <- list(
  schema_version =
    "rqrgibbs_dlm_wave1_comparator_projection_validation/1.0.0",
  source_commit = source_commit,
  source_clean = !nzchar(source_status),
  package_version = as.character(utils::packageVersion("rqrgibbs")),
  primary_runtime_attestation_sha256 =
    if (nzchar(primary_attestation)) {
      rqr_confirm_sha256(primary_attestation)
    } else {
      NA_character_
    },
  primary_reproducibility_eligible =
    if (is.null(primary_provenance)) {
      NA
    } else {
      isTRUE(primary_provenance$reproducibility_eligible)
    },
  exdqlm_runtime_attestation_sha256 =
    rqr_confirm_sha256(exdqlm_attestation),
  exdqlm_runtime_tree_digest =
    external_attestation$runtime_tree_digest,
  exdqlm_source_package_sha256 =
    external_attestation$source_package_sha256,
  config_digest = digest::digest(
    contract$config, algo = "sha256", serialize = TRUE
  ),
  incidence_digest = digest::digest(
    contract$incidence, algo = "sha256", serialize = TRUE
  ),
  seed_ledger_digest = digest::digest(
    ledger[, c("task_key", "state_digest"), drop = FALSE],
    algo = "sha256", serialize = TRUE
  ),
  wave_id = wave_id,
  wave_task_count = nrow(wave_tasks),
  interval_chain_job_count = length(jobs),
  logical_endpoint_fit_count = 2L * length(jobs),
  workers = workers,
  thread_environment = as.list(thread_environment),
  comparator_projection =
    "colSums(FF * posterior_state_mean_or_draw)",
  comparative_simulation_metrics_used = FALSE,
  failed_outputs_reused = FALSE,
  all_fits_succeeded = all(vapply(results, `[[`, logical(1L), "ok")),
  all_diagnostics_passed = all(diagnostics$pass),
  total_fit_elapsed_seconds = sum(vapply(
    results,
    function(result) {
      as.numeric(result$fit_elapsed_seconds %||% NA_real_)
    },
    numeric(1L)
  )),
  started_at_utc = started_at,
  completed_at_utc = completed_at
)
rqr_confirm_atomic_write_json(
  manifest, file.path(output_root, "validation_manifest.json")
)
hashes <- rqr_confirm_recursive_manifest(output_root)
rqr_confirm_atomic_write_csv(
  hashes, file.path(output_root, "artifact_hashes.csv")
)

cat(sprintf(
  paste0(
    "Wave-1 M02 projection gate: %d interval chains, %d endpoint fits, ",
    "%d tasks, %d/%d diagnostics passed.\\n"
  ),
  length(jobs), 2L * length(jobs), length(task_keys),
  sum(diagnostics$pass), nrow(diagnostics)
))
if (!isTRUE(manifest$all_fits_succeeded) ||
    !isTRUE(manifest$all_diagnostics_passed) ||
    (nzchar(expected_commit) &&
      !isTRUE(manifest$primary_reproducibility_eligible))) {
  stop("The wave-1 M02 projection gate failed.", call. = FALSE)
}
