#!/usr/bin/env Rscript

# Re-execute every M01 chain represented in one declared confirmatory wave.
# This is a target-preserving implementation-correction gate, not an
# inferential simulation run. Heavy chain objects remain under an ignored
# output root; only compact summaries are candidates for promotion.

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
    "rqr_dlm_wave1_correction_validation"
  )
}
if (file.exists(output_root) || dir.exists(output_root)) {
  stop("The correction-validation output root must be new.", call. = FALSE)
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
if (nzchar(expected_commit) &&
    (!grepl("^[0-9a-f]{40}$", expected_commit) ||
      !identical(source_commit, expected_commit) ||
      nzchar(source_status))) {
  stop(
    "The correction gate is not at the exact clean expected commit.",
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
provenance_control <- if (nzchar(expected_commit)) {
  rqr_confirm_primary_provenance_control(
    repo_root, expected_commit, primary_attestation
  )
} else {
  list()
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
ledger <- rqr_confirm_seed_ledger(contract, planning = "maximum")
wave_id <- Sys.getenv(
  "RQR_CORRECTION_WAVE_ID",
  unset = "static_gaussian_T200__target0200__sentinel"
)
reviewed_waves <- c(
  wave1 = "static_gaussian_T200__target0200__sentinel",
  wave2 = "local_level_gaussian_T200__target0200__sentinel"
)
if (!wave_id %in% unname(reviewed_waves)) {
  stop(
    "RQR_CORRECTION_WAVE_ID is not one of the two reviewed waves.",
    call. = FALSE
  )
}
wave_tag <- names(reviewed_waves)[match(wave_id, reviewed_waves)]
expected_component_name <- switch(
  wave_tag, wave1 = "regression", wave2 = "level"
)

# This is intentionally reconstructed from the frozen M01 protocol rather
# than copied from a fitted object.  It is duplicated in the compact evidence
# collector so that a producer-side implementation drift cannot redefine its
# own acceptance boundary.
expected_transition_kernel <- list(
  schema_version = "rqrgibbs_dlm_transition_kernel/1.0.0",
  evolution_mode = "component_scale",
  learning_rate_mode = "fixed_rate",
  time0_state_completion = TRUE,
  one_root_partially_collapsed = TRUE,
  collapsed_integrated_root = "root1",
  collapsed_conditioned_root = "root2",
  collapsed_log_q_coordinate_order = expected_component_name,
  scan_order = c(
    "lambda_fixed", "latent_v_refresh",
    "component_scale_root1_collapsed", "root1_ffbs",
    "root1_time0", "root2_ffbs", "root2_time0",
    "component_scale_centered_noncentered_cycles_1",
    "global_root_swap"
  ),
  collapsed_slice_width = 1,
  collapsed_slice_sweeps = 3L,
  collapsed_slice_max_steps = 100L,
  collapsed_slice_max_shrink = 1000L,
  centered_inverse_gamma = TRUE,
  noncentered_slice_interweave = TRUE,
  interweave_cycles = 1L,
  interweave_slice_width = 1,
  interweave_slice_sweeps_per_cycle = 3L,
  interweave_slice_max_steps = 100L,
  interweave_slice_max_shrink = 1000L,
  global_root_swap_probability = 0.5,
  target_change = FALSE
)
expected_transition_kernel_digest <- digest::digest(
  expected_transition_kernel, algo = "sha256", serialize = TRUE
)
expected_transition_kernel_invariant <- list(
  schema_version = "rqrgibbs_dlm_transition_kernel_invariant/1.0.0",
  transition_kernel = expected_transition_kernel[
    setdiff(
      names(expected_transition_kernel),
      "collapsed_log_q_coordinate_order"
    )
  ]
)
expected_transition_kernel_invariant_digest <- digest::digest(
  expected_transition_kernel_invariant,
  algo = "sha256", serialize = TRUE
)
wave_plan <- rqr_confirm_wave_plan(contract, planning = "maximum")
wave_tasks <- wave_plan[
  wave_plan$wave_id == wave_id &
    grepl("(^|\\|)M01(\\||$)", wave_plan$methods),
  ,
  drop = FALSE
]
if (!nrow(wave_tasks) ||
    !all(wave_tasks$embedded_sentinel)) {
  stop("The canonical M01 correction-wave task contract is empty or invalid.",
       call. = FALSE)
}
sentinels <- rqr_confirm_sentinel_map(contract, planning = "maximum")

jobs <- vector("list", 0L)
for (row in seq_len(nrow(wave_tasks))) {
  scenario_id <- wave_tasks$DGP[[row]]
  replication <- wave_tasks$replication[[row]]
  cell_id <- contract$incidence$cell_id[
    contract$incidence$DGP == scenario_id &
      contract$incidence$method == "M01"
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
expected_jobs <- sum(vapply(
  seq_len(nrow(wave_tasks)),
  function(row) {
    scenario_id <- wave_tasks$DGP[[row]]
    replication <- wave_tasks$replication[[row]]
    cell_id <- contract$incidence$cell_id[
      contract$incidence$DGP == scenario_id &
        contract$incidence$method == "M01"
    ]
    if (any(
        sentinels$cell_id == cell_id &
          sentinels$replication == replication
      )) 4L else 1L
  },
  integer(1L)
))
if (!identical(length(jobs), expected_jobs)) {
  stop("The canonical correction gate has an incorrect M01 chain count.",
       call. = FALSE)
}
workers <- as.integer(Sys.getenv("RQR_CORRECTION_WORKERS", unset = "8"))
if (is.na(workers) || workers < 1L || workers > length(jobs)) {
  stop("RQR_CORRECTION_WORKERS must not exceed the correction job count.",
       call. = FALSE)
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
      value <- rqr_confirm_dynamic_fit(
        contract = contract,
        generated = generated,
        method = "M01",
        chain = job$chain,
        ledger = ledger,
        provenance_control = provenance_control,
        profile_name = job$profile
      )
      observed_transition_kernel <-
        value$fit$checkpoint_state$transition_kernel
      observed_transition_kernel_digest <- digest::digest(
        observed_transition_kernel, algo = "sha256", serialize = TRUE
      )
      transition_kernel_contract_match <-
        identical(observed_transition_kernel, expected_transition_kernel) &&
        identical(
          value$fit$checkpoint_state$transition_kernel_digest,
          expected_transition_kernel_digest
        ) &&
        identical(
          observed_transition_kernel_digest,
          expected_transition_kernel_digest
        )
      if (!isTRUE(transition_kernel_contract_match)) {
        stop(
          paste(
            "The fitted M01 transition kernel does not match the",
            "independently frozen complete role contract."
          ),
          call. = FALSE
        )
      }
      scalars <- rqr_confirm_scalar_draws(
        value, generated, contract, "M01"
      )
      list(
        ok = TRUE,
        job = job,
        fit_elapsed_seconds =
          as.numeric(proc.time()[["elapsed"]] - fit_started),
        reproducibility_eligible =
          isTRUE(value$fit$provenance$reproducibility_eligible),
        runtime_tree_digest =
          value$fit$provenance$primary_runtime_tree_digest,
        numerical_repair_count =
          value$fit$model_spec$numerical_repair_count,
        cumulative_numerical_repair_count =
          value$fit$model_spec$cumulative_numerical_repair_count,
        exact_joint_target =
          isTRUE(value$fit$model_spec$exact_joint_target),
        target_numerical_eligible =
          isTRUE(value$fit$model_spec$target_numerical_eligible),
        transition_kernel_schema =
          value$fit$checkpoint_state$transition_kernel_schema,
        transition_kernel_digest =
          value$fit$checkpoint_state$transition_kernel_digest,
        transition_kernel_contract = observed_transition_kernel,
        transition_kernel_contract_digest =
          observed_transition_kernel_digest,
        transition_kernel_contract_match =
          transition_kernel_contract_match,
        one_root_partially_collapsed = isTRUE(
          value$fit$checkpoint_state$transition_kernel$
            one_root_partially_collapsed
        ),
        collapsed_integrated_root =
          value$fit$checkpoint_state$transition_kernel$
            collapsed_integrated_root,
        scalars = scalars,
        peak_RSS_KiB = rqr_confirm_process_peak_rss_kib()
      )
    }, error = function(error) {
      list(
        ok = FALSE, job = job,
        error_class = class(error)[[1L]],
        message = conditionMessage(error),
        peak_RSS_KiB = rqr_confirm_process_peak_rss_kib()
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
collapse_result_field <- function(selected, field, default = "") {
  paste(
    vapply(
      selected,
      function(result) {
        value <- result[[field]] %||% default
        if (is.logical(value)) {
          if (length(value) != 1L || is.na(value)) {
            return(default)
          }
          return(if (isTRUE(value)) "TRUE" else "FALSE")
        }
        as.character(value)[[1L]]
      },
      character(1L)
    ),
    collapse = "|"
  )
}
peak_values <- vapply(
  results,
  function(result) as.numeric(result$peak_RSS_KiB %||% NA_real_),
  numeric(1L)
)
maximum_peak_RSS_KiB <- if (any(is.finite(peak_values))) {
  max(peak_values[is.finite(peak_values)])
} else {
  NA_real_
}
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
      method = "M01", generated = generated
    )
    value$DGP <- first$scenario_id
    value$replication <- first$replication
    value$sentinel <- first$sentinel
    diagnostics[[index]] <- value
  }
  value <- diagnostics[[index]]
  q_row <- value[value$estimand == "log_q_1", , drop = FALSE]
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
    maximum_peak_RSS_KiB = {
      selected_peaks <- vapply(
        selected,
        function(result) as.numeric(result$peak_RSS_KiB %||% NA_real_),
        numeric(1L)
      )
      if (any(is.finite(selected_peaks))) {
        max(selected_peaks[is.finite(selected_peaks)])
      } else {
        NA_real_
      }
    },
    log_q_1_rhat = if (nrow(q_row)) q_row$rhat[[1L]] else NA_real_,
    log_q_1_ess_bulk =
      if (nrow(q_row)) q_row$ess_bulk[[1L]] else NA_real_,
    log_q_1_ess_tail =
      if (nrow(q_row)) q_row$ess_tail[[1L]] else NA_real_,
    log_q_1_mcse_over_sd =
      if (nrow(q_row)) q_row$mcse_over_sd[[1L]] else NA_real_,
    transition_kernel_fit_count = length(selected),
    transition_kernel_schemas = collapse_result_field(
      selected, "transition_kernel_schema"
    ),
    transition_kernel_digests = collapse_result_field(
      selected, "transition_kernel_digest"
    ),
    transition_kernel_contract_digests = collapse_result_field(
      selected, "transition_kernel_contract_digest"
    ),
    transition_kernel_contract_matches = collapse_result_field(
      selected, "transition_kernel_contract_match"
    ),
    stringsAsFactors = FALSE
  )
}
diagnostics <- do.call(rbind, diagnostics)
summary <- do.call(rbind, summary_rows)
successful_results <- results[
  vapply(results, function(result) isTRUE(result$ok), logical(1L))
]
all_fits_exact_target_preserving <-
  length(successful_results) == length(results) &&
  all(vapply(
    successful_results,
    function(result) {
      identical(result$numerical_repair_count, 0L) &&
        identical(result$cumulative_numerical_repair_count, 0L) &&
        isTRUE(result$exact_joint_target) &&
        isTRUE(result$target_numerical_eligible) &&
        identical(
          result$transition_kernel_schema,
          "rqrgibbs_dlm_transition_kernel/1.0.0"
        ) &&
        grepl(
          "^[0-9a-f]{64}$",
          result$transition_kernel_digest %||% ""
        ) &&
        identical(
          result$transition_kernel_contract,
          expected_transition_kernel
        ) &&
        identical(
          result$transition_kernel_contract_digest,
          expected_transition_kernel_digest
        ) &&
        isTRUE(result$transition_kernel_contract_match) &&
        isTRUE(result$one_root_partially_collapsed) &&
        identical(result$collapsed_integrated_root, "root1")
    },
    logical(1L)
  ))
transition_kernel_digests <- unique(vapply(
  successful_results,
  function(result) {
    as.character(result$transition_kernel_digest %||% NA_character_)
  },
  character(1L)
))
all_fit_transition_contracts_complete <-
  length(successful_results) == length(results) &&
  all(vapply(
    successful_results,
    function(result) {
      is.list(result$transition_kernel_contract) &&
        identical(
          names(result$transition_kernel_contract),
          names(expected_transition_kernel)
        ) &&
        grepl(
          "^[0-9a-f]{64}$",
          result$transition_kernel_contract_digest %||% ""
        )
    },
    logical(1L)
  ))
all_fit_transition_contracts_match_expected <-
  all_fit_transition_contracts_complete &&
  all(vapply(
    successful_results,
    function(result) {
      isTRUE(result$transition_kernel_contract_match) &&
        identical(
          result$transition_kernel_contract,
          expected_transition_kernel
        ) &&
        identical(
          result$transition_kernel_contract_digest,
          expected_transition_kernel_digest
        )
    },
    logical(1L)
  ))

atomic_rds <- function(value, path) {
  temporary <- tempfile(
    paste0(".", basename(path), "-"), tmpdir = dirname(path)
  )
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  saveRDS(value, temporary, version = 3L)
  if (!file.rename(temporary, path)) {
    stop("Could not atomically publish the correction-validation RDS.",
         call. = FALSE)
  }
}
atomic_rds(
  list(jobs = jobs, results = results, diagnostics = diagnostics),
  file.path(output_root, paste0(wave_tag, "_M01_chain_evidence.rds"))
)
rqr_confirm_atomic_write_csv(
  diagnostics,
  file.path(output_root, paste0(wave_tag, "_M01_diagnostics.csv"))
)
rqr_confirm_atomic_write_csv(
  summary, file.path(output_root, paste0(wave_tag, "_M01_summary.csv"))
)
manifest <- list(
  schema_version = "rqrgibbs_dlm_wave_correction_validation/2.2.0",
  source_commit = source_commit,
  source_clean = !nzchar(source_status),
  package_version = as.character(utils::packageVersion("rqrgibbs")),
  primary_runtime_attestation_sha256 =
    if (nzchar(primary_attestation)) {
      rqr_confirm_sha256(primary_attestation)
    } else {
      NA_character_
    },
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
  chain_job_count = length(jobs),
  workers = workers,
  thread_environment = as.list(thread_environment),
  component_scale_kernel =
    contract$config$frozen_tuning$component_scale_kernel,
  standard_component_scale_schedule =
    contract$config$schedules$dynamic_rqr_component_scale_standard,
  sentinel_component_scale_schedule =
    contract$config$schedules$dynamic_rqr_component_scale_sentinel,
  exact_target_preserving_kernel = all_fits_exact_target_preserving,
  transition_kernel_schema =
    "rqrgibbs_dlm_transition_kernel/1.0.0",
  unique_transition_kernel_digests = transition_kernel_digests,
  expected_transition_kernel_contract = expected_transition_kernel,
  expected_transition_kernel_contract_digest =
    expected_transition_kernel_digest,
  transition_kernel_invariant_schema =
    "rqrgibbs_dlm_transition_kernel_invariant/1.0.0",
  expected_transition_kernel_invariant =
    expected_transition_kernel_invariant,
  expected_transition_kernel_invariant_digest =
    expected_transition_kernel_invariant_digest,
  all_fit_transition_contracts_complete =
    all_fit_transition_contracts_complete,
  all_fit_transition_contracts_match_expected =
    all_fit_transition_contracts_match_expected,
  comparative_simulation_metrics_used = FALSE,
  failed_outputs_reused = FALSE,
  all_fits_succeeded = all(vapply(results, `[[`, logical(1L), "ok")),
  all_fits_reproducibility_eligible =
    if (nzchar(expected_commit)) {
      all(vapply(
        results,
        function(result) {
          isTRUE(result$ok) &&
            isTRUE(result$reproducibility_eligible)
        },
        logical(1L)
      ))
    } else {
      NA
    },
  unique_runtime_tree_digests =
    unique(vapply(
      results,
      function(result) {
        as.character(result$runtime_tree_digest %||% NA_character_)
      },
      character(1L)
    )),
  total_fit_elapsed_seconds = sum(vapply(
    results,
    function(result) {
      as.numeric(result$fit_elapsed_seconds %||% NA_real_)
    },
    numeric(1L)
  )),
  maximum_process_peak_RSS_KiB = maximum_peak_RSS_KiB,
  declared_worker_memory_ceiling_KiB =
    contract$config$resources$per_worker_memory_GiB * 1024^2,
  resource_margin_pass = is.finite(maximum_peak_RSS_KiB) &&
    maximum_peak_RSS_KiB <=
      0.80 * contract$config$resources$per_worker_memory_GiB * 1024^2,
  all_diagnostics_passed = all(diagnostics$pass),
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
    "M01 correction gate for %s: %d chains, %d tasks, ",
    "%d/%d diagnostics passed.\\n"
  ),
  wave_id, length(jobs), length(task_keys), sum(diagnostics$pass),
  nrow(diagnostics)
))
if (!isTRUE(manifest$all_fits_succeeded) ||
    !isTRUE(manifest$exact_target_preserving_kernel) ||
    length(manifest$unique_transition_kernel_digests) != 1L ||
    !identical(
      manifest$unique_transition_kernel_digests,
      manifest$expected_transition_kernel_contract_digest
    ) ||
    !isTRUE(manifest$all_fit_transition_contracts_complete) ||
    !isTRUE(
      manifest$all_fit_transition_contracts_match_expected
    ) ||
    !isTRUE(manifest$all_diagnostics_passed) ||
    !isTRUE(manifest$resource_margin_pass) ||
    (nzchar(expected_commit) &&
      !isTRUE(manifest$all_fits_reproducibility_eligible))) {
  stop(
    sprintf("The M01 correction gate for %s failed.", wave_id),
    call. = FALSE
  )
}
