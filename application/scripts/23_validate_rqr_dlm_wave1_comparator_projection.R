#!/usr/bin/env Rscript

# Re-execute every M02 interval-chain job represented in one declared
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
wave_id <- Sys.getenv(
  "RQR_CORRECTION_WAVE_ID",
  unset = "static_gaussian_T200__target0200__sentinel"
)
allowed_waves <- c(
  wave1 = "static_gaussian_T200__target0200__sentinel",
  wave2 = "local_level_gaussian_T200__target0200__sentinel"
)
if (!wave_id %in% unname(allowed_waves)) {
  stop(
    paste0(
      "RQR_CORRECTION_WAVE_ID must be one of the two frozen M02 waves: ",
      paste(unname(allowed_waves), collapse = ", "), "."
    ),
    call. = FALSE
  )
}
wave_tag <- names(allowed_waves)[match(wave_id, allowed_waves)]
wave_plan <- rqr_confirm_wave_plan(contract, planning = "maximum")
wave_tasks <- wave_plan[
  wave_plan$wave_id == wave_id &
    grepl("(^|\\|)M02(\\||$)", wave_plan$methods),
  ,
  drop = FALSE
]
if (!nrow(wave_tasks) ||
    !all(wave_tasks$embedded_sentinel)) {
  stop("The canonical M02 correction-wave task contract is empty or invalid.",
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
job_schedule_roles <- vapply(
  jobs,
  function(job) if (isTRUE(job$sentinel)) "sentinel" else "standard",
  character(1L)
)
if (!setequal(unique(job_schedule_roles), c("standard", "sentinel"))) {
  stop(
    "Each frozen M02 wave must contain standard and sentinel schedule roles.",
    call. = FALSE
  )
}
expected_jobs <- sum(vapply(
  seq_len(nrow(wave_tasks)),
  function(row) {
    scenario_id <- wave_tasks$DGP[[row]]
    replication <- wave_tasks$replication[[row]]
    cell_id <- contract$incidence$cell_id[
      contract$incidence$DGP == scenario_id &
        contract$incidence$method == "M02"
    ]
    if (any(
        sentinels$cell_id == cell_id &
          sentinels$replication == replication
      )) 4L else 1L
  },
  integer(1L)
))
if (!identical(length(jobs), expected_jobs)) {
  stop("The canonical comparator gate has an incorrect interval-chain count.",
       call. = FALSE)
}
workers <- as.integer(Sys.getenv(
  "RQR_COMPARATOR_CORRECTION_WORKERS", unset = "8"
))
if (is.na(workers) || workers < 1L || workers > length(jobs)) {
  stop(
    "RQR_COMPARATOR_CORRECTION_WORKERS must not exceed the job count.",
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
      scalars <- rqr_confirm_scalar_draws(
        value, generated, contract, "M02"
      )
      list(
        ok = TRUE,
        job = job,
        fit_elapsed_seconds =
          as.numeric(proc.time()[["elapsed"]] - fit_started),
        common_target_digest =
          value$diagnostics$common_target_digest,
        initialization_digest =
          value$diagnostics$initialization_digest,
        profile_changed_target =
          value$diagnostics$profile_changed_target,
        schedule_role = if (isTRUE(job$sentinel)) {
          "sentinel"
        } else {
          "standard"
        },
        applied_schedule = value$diagnostics$schedule,
        schedule_applied = value$diagnostics$schedule_applied,
        state_draw_dimensions =
          value$diagnostics$state_draw_dimensions,
        scale_draw_lengths =
          value$diagnostics$scale_draw_lengths,
        training_horizon = as.integer(generated$T),
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
common_target_pass <- all(vapply(
  split(results, result_keys),
  function(selected) {
    all(vapply(selected, function(result) {
      isTRUE(result$ok) &&
        identical(result$profile_changed_target, FALSE) &&
        is.character(result$common_target_digest) &&
        length(result$common_target_digest) == 1L &&
        grepl("^[0-9a-f]{64}$", result$common_target_digest)
    }, logical(1L))) &&
      length(unique(vapply(
        selected, `[[`, character(1L), "common_target_digest"
      ))) == 1L
  },
  logical(1L)
))
overdispersed_initialization_pass <- all(vapply(
  split(results, result_keys),
  function(selected) {
    valid <- all(vapply(selected, function(result) {
      isTRUE(result$ok) &&
        is.character(result$initialization_digest) &&
        length(result$initialization_digest) == 1L &&
        grepl("^[0-9a-f]{64}$", result$initialization_digest)
    }, logical(1L)))
    if (!valid) return(FALSE)
    digests <- vapply(
      selected, `[[`, character(1L), "initialization_digest"
    )
    if (isTRUE(selected[[1L]]$job$sentinel)) {
      length(selected) == 4L && length(unique(digests)) == 4L
    } else {
      length(selected) == 1L && length(unique(digests)) == 1L
    }
  },
  logical(1L)
))
frozen_schedules <- list(
  standard =
    contract$config$schedules$dynamic_quantile_endpoint_standard,
  sentinel =
    contract$config$schedules$dynamic_quantile_endpoint_sentinel
)
valid_nonnegative_integer <- function(value) {
  is.numeric(value) &&
    !is.object(value) &&
    is.null(dim(value)) &&
    length(value) == 1L &&
    !is.na(value) &&
    is.finite(value) &&
    value == floor(value) &&
    value >= 0 &&
    value <= .Machine$integer.max
}
valid_schedule <- function(value) {
  is.list(value) &&
    identical(names(value), c("burn", "retain", "thin")) &&
    all(vapply(value, valid_nonnegative_integer, logical(1L))) &&
    value$retain >= 1 &&
    value$thin >= 1
}
if (!identical(names(frozen_schedules), c("standard", "sentinel")) ||
    !all(vapply(frozen_schedules, valid_schedule, logical(1L)))) {
  stop("The two frozen M02 endpoint schedules are malformed.", call. = FALSE)
}
schedule_result_pass <- function(result) {
  if (!isTRUE(result$ok) ||
      !is.character(result$schedule_role) ||
      length(result$schedule_role) != 1L ||
      is.na(result$schedule_role) ||
      !result$schedule_role %in% names(frozen_schedules) ||
      !valid_schedule(result$applied_schedule) ||
      !valid_nonnegative_integer(result$training_horizon) ||
      result$training_horizon < 1L) {
    return(FALSE)
  }
  expected <- frozen_schedules[[result$schedule_role]]
  observed <- result$applied_schedule
  isTRUE(result$schedule_applied) &&
    identical(
      unname(unlist(observed[c("burn", "retain", "thin")])),
      unname(unlist(expected[c("burn", "retain", "thin")]))
    ) &&
    length(result$state_draw_dimensions) == 2L &&
    all(vapply(
      result$state_draw_dimensions,
      function(dimensions) {
        is.numeric(dimensions) &&
          length(dimensions) == 3L &&
          all(vapply(
            as.list(dimensions),
            valid_nonnegative_integer,
            logical(1L)
          )) &&
          all(dimensions >= 1L) &&
          identical(
            dimensions[[2L]], result$training_horizon
          ) &&
          identical(
            dimensions[[3L]], as.integer(expected$retain)
          )
      },
      logical(1L)
    )) &&
    identical(
      result$state_draw_dimensions[[1L]],
      result$state_draw_dimensions[[2L]]
    ) &&
    is.numeric(result$scale_draw_lengths) &&
    length(result$scale_draw_lengths) == 2L &&
    all(vapply(
      as.list(result$scale_draw_lengths),
      valid_nonnegative_integer,
      logical(1L)
    )) &&
    all(
      result$scale_draw_lengths == as.integer(expected$retain)
    )
}
schedule_contract_pass <- all(vapply(
  results, schedule_result_pass, logical(1L)
))
schedule_evidence <- setNames(
  lapply(names(frozen_schedules), function(role) {
    selected <- results[vapply(
      results,
      function(result) {
        isTRUE(result$ok) && identical(result$schedule_role, role)
      },
      logical(1L)
    )]
    dimension_text <- if (length(selected)) {
      sort(unique(unlist(lapply(selected, function(result) {
        vapply(
          result$state_draw_dimensions,
          function(value) paste(value, collapse = "x"),
          character(1L)
        )
      }))), method = "radix")
    } else {
      character()
    }
    scale_lengths <- if (length(selected)) {
      sort(unique(unlist(lapply(
        selected, `[[`, "scale_draw_lengths"
      ))), method = "radix")
    } else {
      integer()
    }
    list(
      configured_schedule = frozen_schedules[[role]],
      interval_chain_job_count = length(selected),
      realized_state_draw_dimensions = dimension_text,
      realized_scale_draw_lengths = as.integer(scale_lengths),
      all_applied = length(selected) > 0L &&
        all(vapply(selected, schedule_result_pass, logical(1L)))
    )
  }),
  names(frozen_schedules)
)
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
  schedule_roles <- unique(vapply(
    selected,
    function(result) {
      if (isTRUE(result$job$sentinel)) "sentinel" else "standard"
    },
    character(1L)
  ))
  schedule_role <- if (length(schedule_roles) == 1L) {
    schedule_roles[[1L]]
  } else {
    NA_character_
  }
  configured_schedule <- if (!is.na(schedule_role)) {
    frozen_schedules[[schedule_role]]
  } else {
    list(burn = NA_integer_, retain = NA_integer_, thin = NA_integer_)
  }
  successful_results <- selected[vapply(
    selected, function(result) isTRUE(result$ok), logical(1L)
  )]
  applied_schedule <- if (length(successful_results) &&
      all(vapply(
        successful_results,
        function(result) {
          identical(
            result$applied_schedule,
            successful_results[[1L]]$applied_schedule
          )
        },
        logical(1L)
      ))) {
    successful_results[[1L]]$applied_schedule
  } else {
    list(burn = NA_integer_, retain = NA_integer_, thin = NA_integer_)
  }
  realized_state_dimensions <- if (length(successful_results)) {
    paste(sort(unique(unlist(lapply(
      successful_results,
      function(result) {
        vapply(
          result$state_draw_dimensions,
          function(dimensions) paste(dimensions, collapse = "x"),
          character(1L)
        )
      }
    ))), method = "radix"), collapse = ";")
  } else {
    NA_character_
  }
  realized_scale_lengths <- if (length(successful_results)) {
    paste(sort(unique(unlist(lapply(
      successful_results, `[[`, "scale_draw_lengths"
    ))), method = "radix"), collapse = ";")
  } else {
    NA_character_
  }
  summary_rows[[index]] <- data.frame(
    DGP = first$scenario_id,
    replication = first$replication,
    sentinel = first$sentinel,
    schedule_role = schedule_role,
    configured_burn = as.integer(configured_schedule$burn),
    configured_retain = as.integer(configured_schedule$retain),
    configured_thin = as.integer(configured_schedule$thin),
    applied_burn = as.integer(applied_schedule$burn),
    applied_retain = as.integer(applied_schedule$retain),
    applied_thin = as.integer(applied_schedule$thin),
    realized_state_draw_dimensions = realized_state_dimensions,
    realized_scale_draw_lengths = realized_scale_lengths,
    schedule_contract_pass = all(vapply(
      selected, schedule_result_pass, logical(1L)
    )),
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
  file.path(output_root, paste0(wave_tag, "_M02_chain_evidence.rds"))
)
rqr_confirm_atomic_write_csv(
  diagnostics,
  file.path(output_root, paste0(wave_tag, "_M02_diagnostics.csv"))
)
rqr_confirm_atomic_write_csv(
  summary, file.path(output_root, paste0(wave_tag, "_M02_summary.csv"))
)
manifest <- list(
  schema_version =
    "rqrgibbs_dlm_wave_comparator_projection_validation/2.1.0",
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
  primary_runtime_tree_digest =
    primary_provenance$primary_runtime_tree_digest %||%
      NA_character_,
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
  common_target_across_initialization_profiles = common_target_pass,
  overdispersed_initialization_profiles_verified =
    overdispersed_initialization_pass,
  frozen_schedules = frozen_schedules,
  applied_schedule_evidence = schedule_evidence,
  all_applied_schedules_verified = schedule_contract_pass,
  schedule_evidence_fields = c(
    "schedule_role", "applied_schedule", "schedule_applied",
    "state_draw_dimensions", "scale_draw_lengths"
  ),
  initialization_contract =
    "target_preserving_precomputed_mcmc_state",
  target_fields_held_fixed =
    paste(
      "y;m0;C0;FF;GG;discounts;component_dimensions;",
      "dqlm_ind;fix_sigma;PriorSigma;quantile_probability",
      sep = ""
    ),
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
  maximum_process_peak_RSS_KiB = maximum_peak_RSS_KiB,
  declared_worker_memory_ceiling_KiB =
    contract$config$resources$per_worker_memory_GiB * 1024^2,
  resource_margin_pass = is.finite(maximum_peak_RSS_KiB) &&
    maximum_peak_RSS_KiB <=
      0.80 * contract$config$resources$per_worker_memory_GiB * 1024^2,
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
    "M02 projection gate for %s: %d interval chains, %d endpoint fits, ",
    "%d tasks, %d/%d diagnostics passed.\\n"
  ),
  wave_id, length(jobs), 2L * length(jobs), length(task_keys),
  sum(diagnostics$pass), nrow(diagnostics)
))
if (!isTRUE(manifest$all_fits_succeeded) ||
    !isTRUE(manifest$all_diagnostics_passed) ||
    !isTRUE(manifest$common_target_across_initialization_profiles) ||
    !isTRUE(manifest$overdispersed_initialization_profiles_verified) ||
    !isTRUE(manifest$all_applied_schedules_verified) ||
    !isTRUE(manifest$resource_margin_pass) ||
    (nzchar(expected_commit) &&
      !isTRUE(manifest$primary_reproducibility_eligible))) {
  stop(
    sprintf("The M02 correction gate for %s failed.", wave_id),
    call. = FALSE
  )
}
