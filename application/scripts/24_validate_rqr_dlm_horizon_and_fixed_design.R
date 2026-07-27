#!/usr/bin/env Rscript

# Validate the forecast-horizon materialization boundary and the fixed
# role-specific M03 schedule exposed by the first two waves of the stopped
# confirmatory run. This is computational correction evidence only. It does
# not estimate or compare scientific performance.

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
    "rqr_dlm_horizon_fixed_design_validation"
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
    thread_variables, Sys.getenv, character(1L), unset = ""
  ),
  thread_variables
)
if (nzchar(expected_commit) && any(thread_environment != "1")) {
  stop(
    "Every declared numerical-library thread limit must equal one.",
    call. = FALSE
  )
}

contract <- rqr_confirm_read_contract(repo_root)
rqr_confirm_validate_contract(contract, require_closed = TRUE)
rqr_confirm_validate_budget(contract)
ledger <- rqr_confirm_seed_ledger(contract, planning = "maximum")

# The complete model family must expose exact training and forecasting
# horizons. Time-invariant observation designs are materialized explicitly so
# the public forecast API cannot infer a one-step horizon from a one-column FF.
horizon_rows <- lapply(
  names(contract$config$scenarios),
  function(scenario_id) {
    generated <- rqr_confirm_generate_dgp(
      contract, scenario_id, 1L, ledger
    )
    bundle <- rqr_confirm_model_bundle(generated)
    training_partition <- identical(
      bundle$training$FF,
      bundle$full$FF[
        , seq_len(generated$T), drop = FALSE
      ]
    )
    future_partition <- identical(
      bundle$future$FF,
      bundle$full$FF[
        , generated$T + seq_len(generated$H), drop = FALSE
      ]
    )
    data.frame(
      DGP = scenario_id,
      state_dimension = nrow(bundle$training$FF),
      training_horizon_expected = generated$T,
      training_horizon_observed = ncol(bundle$training$FF),
      future_horizon_expected = generated$H,
      future_horizon_observed = ncol(bundle$future$FF),
      full_horizon_expected = generated$T + generated$H,
      full_horizon_observed = ncol(bundle$full$FF),
      training_partition_exact = training_partition,
      future_partition_exact = future_partition,
      pass =
        identical(ncol(bundle$training$FF), generated$T) &&
        identical(ncol(bundle$future$FF), generated$H) &&
        identical(
          ncol(bundle$full$FF), generated$T + generated$H
        ) &&
        training_partition && future_partition,
      stringsAsFactors = FALSE
    )
  }
)
horizon_checks <- do.call(rbind, horizon_rows)

# Re-execute the eight one-chain M03 seed streams from the first canonical
# wave. The four M03-specific sentinels keep their reviewed shorter schedule
# and already passed; this gate targets only the longer standard role.
wave_id <- "static_gaussian_T200__target0200__sentinel"
wave_plan <- rqr_confirm_wave_plan(contract, planning = "maximum")
wave_tasks <- wave_plan[
  wave_plan$wave_id == wave_id &
    grepl("(^|\\|)M03(\\||$)", wave_plan$methods),
  ,
  drop = FALSE
]
sentinels <- rqr_confirm_sentinel_map(contract, planning = "maximum")
method_sentinels <- sentinels[
  sentinels$method == "M03", c("cell_id", "replication"),
  drop = FALSE
]
is_method_sentinel <- vapply(
  seq_len(nrow(wave_tasks)),
  function(index) {
    cell_id <- contract$incidence$cell_id[
      contract$incidence$DGP == wave_tasks$DGP[[index]] &
        contract$incidence$method == "M03"
    ]
    any(
      method_sentinels$cell_id == cell_id &
        method_sentinels$replication ==
          wave_tasks$replication[[index]]
    )
  },
  logical(1L)
)
standard_tasks <- wave_tasks[!is_method_sentinel, , drop = FALSE]
if (nrow(wave_tasks) != 12L ||
    nrow(standard_tasks) != 8L ||
    !identical(
      rqr_confirm_fixed_design_schedule(contract, "standard"),
      list(burn = 500L, retain = 3000L, thin = 1L)
    )) {
  stop("The fixed-design correction contract changed.", call. = FALSE)
}
workers <- as.integer(Sys.getenv(
  "RQR_FIXED_DESIGN_CORRECTION_WORKERS", unset = "8"
))
if (is.na(workers) || workers < 1L ||
    workers > nrow(standard_tasks)) {
  stop(
    paste(
      "RQR_FIXED_DESIGN_CORRECTION_WORKERS must be an integer",
      "from 1 through 8."
    ),
    call. = FALSE
  )
}

started_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
fixed_results <- parallel::mclapply(
  seq_len(nrow(standard_tasks)),
  mc.cores = workers,
  mc.preschedule = TRUE,
  FUN = function(index) {
    task <- standard_tasks[index, , drop = FALSE]
    tryCatch({
      generated <- rqr_confirm_generate_dgp(
        contract, task$DGP[[1L]], task$replication[[1L]], ledger
      )
      fit_started <- proc.time()[["elapsed"]]
      value <- rqr_confirm_fixed_design(
        contract = contract, generated = generated, chain = 1L,
        ledger = ledger, provenance_control = provenance_control,
        profile_name = "standard"
      )
      list(
        ok = TRUE,
        DGP = task$DGP[[1L]],
        replication = task$replication[[1L]],
        elapsed_seconds =
          as.numeric(proc.time()[["elapsed"]] - fit_started),
        reproducibility_eligible =
          isTRUE(value$fit$provenance$reproducibility_eligible),
        runtime_tree_digest =
          value$fit$provenance$primary_runtime_tree_digest,
        scalars = rqr_confirm_scalar_draws(
          value, generated, contract, "M03"
        )
      )
    }, error = function(error) {
      list(
        ok = FALSE,
        DGP = task$DGP[[1L]],
        replication = task$replication[[1L]],
        error_class = class(error)[[1L]],
        message = conditionMessage(error)
      )
    })
  }
)

fixed_diagnostics <- lapply(
  fixed_results,
  function(result) {
    if (!isTRUE(result$ok)) {
      return(data.frame(
        DGP = result$DGP, replication = result$replication,
        estimand = "fit_error", chains = 1L,
        rhat = NA_real_, ess_bulk = NA_real_, ess_tail = NA_real_,
        mcse_mean = NA_real_, mcse_over_sd = NA_real_,
        pass = FALSE, stringsAsFactors = FALSE
      ))
    }
    generated <- rqr_confirm_generate_dgp(
      contract, result$DGP, result$replication, ledger
    )
    value <- rqr_confirm_chain_diagnostics(
      list(result$scalars), contract = contract, sentinel = FALSE,
      method = "M03", generated = generated
    )
    value$DGP <- result$DGP
    value$replication <- result$replication
    value
  }
)
fixed_diagnostics <- do.call(rbind, fixed_diagnostics)
fixed_summary <- do.call(
  rbind,
  lapply(
    fixed_results,
    function(result) {
      rows <- fixed_diagnostics[
        fixed_diagnostics$DGP == result$DGP &
          fixed_diagnostics$replication == result$replication,
        ,
        drop = FALSE
      ]
      data.frame(
        DGP = result$DGP,
        replication = result$replication,
        fit_succeeded = isTRUE(result$ok),
        diagnostics = nrow(rows),
        diagnostics_passed = sum(rows$pass),
        all_diagnostics_passed = all(rows$pass),
        minimum_bulk_ess =
          suppressWarnings(min(rows$ess_bulk, na.rm = TRUE)),
        minimum_tail_ess =
          suppressWarnings(min(rows$ess_tail, na.rm = TRUE)),
        maximum_mcse_over_sd =
          suppressWarnings(max(rows$mcse_over_sd, na.rm = TRUE)),
        elapsed_seconds =
          as.numeric(result$elapsed_seconds %||% NA_real_),
        stringsAsFactors = FALSE
      )
    }
  )
)
numeric_summary <- c(
  "minimum_bulk_ess", "minimum_tail_ess", "maximum_mcse_over_sd"
)
for (name in numeric_summary) {
  fixed_summary[!is.finite(fixed_summary[[name]]), name] <- NA_real_
}

# Run one exact chain from the failed second wave through the public dynamic
# fit and forecast path. This checks the corrected endpoint boundary without
# treating the resulting interval as simulation evidence.
generated <- rqr_confirm_generate_dgp(contract, "S03", 13L, ledger)
dynamic_started <- proc.time()[["elapsed"]]
dynamic_result <- tryCatch(
  rqr_confirm_dynamic_fit(
    contract = contract, generated = generated, method = "M01",
    chain = 1L, ledger = ledger,
    provenance_control = provenance_control, profile_name = "A"
  ),
  error = identity
)
dynamic_elapsed <- as.numeric(
  proc.time()[["elapsed"]] - dynamic_started
)
dynamic_ok <- !inherits(dynamic_result, "error")
dynamic_check <- data.frame(
  DGP = "S03",
  replication = 13L,
  method = "M01",
  training_horizon_expected = generated$T,
  training_lower_length = if (dynamic_ok) {
    length(dynamic_result$training_lower)
  } else {
    NA_integer_
  },
  training_upper_length = if (dynamic_ok) {
    length(dynamic_result$training_upper)
  } else {
    NA_integer_
  },
  future_horizon_expected = generated$H,
  future_lower_length = if (dynamic_ok) {
    length(dynamic_result$future_lower)
  } else {
    NA_integer_
  },
  future_upper_length = if (dynamic_ok) {
    length(dynamic_result$future_upper)
  } else {
    NA_integer_
  },
  numerical_repairs = if (dynamic_ok) {
    dynamic_result$fit$model_spec$numerical_repair_count
  } else {
    NA_integer_
  },
  exact_joint_target = if (dynamic_ok) {
    isTRUE(dynamic_result$fit$model_spec$exact_joint_target)
  } else {
    FALSE
  },
  reproducibility_eligible = if (dynamic_ok) {
    isTRUE(dynamic_result$fit$provenance$reproducibility_eligible)
  } else {
    FALSE
  },
  elapsed_seconds = dynamic_elapsed,
  error_message = if (dynamic_ok) {
    ""
  } else {
    conditionMessage(dynamic_result)
  },
  pass = dynamic_ok &&
    identical(length(dynamic_result$training_lower), generated$T) &&
    identical(length(dynamic_result$training_upper), generated$T) &&
    identical(length(dynamic_result$future_lower), generated$H) &&
    identical(length(dynamic_result$future_upper), generated$H) &&
    all(is.finite(c(
      dynamic_result$training_lower, dynamic_result$training_upper,
      dynamic_result$future_lower, dynamic_result$future_upper
    ))) &&
    all(dynamic_result$training_upper >= dynamic_result$training_lower) &&
    all(dynamic_result$future_upper >= dynamic_result$future_lower) &&
    identical(
      dynamic_result$fit$model_spec$numerical_repair_count, 0L
    ) &&
    isTRUE(dynamic_result$fit$model_spec$exact_joint_target) &&
    (!nzchar(expected_commit) ||
      isTRUE(
        dynamic_result$fit$provenance$reproducibility_eligible
      )),
  stringsAsFactors = FALSE
)
completed_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)

atomic_rds <- function(value, path) {
  temporary <- tempfile(
    paste0(".", basename(path), "-"), tmpdir = dirname(path)
  )
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  saveRDS(value, temporary, version = 3L)
  if (!file.rename(temporary, path)) {
    stop(
      "Could not atomically publish correction chain evidence.",
      call. = FALSE
    )
  }
}
atomic_rds(
  list(
    fixed_design_results = fixed_results,
    fixed_design_diagnostics = fixed_diagnostics
  ),
  file.path(output_root, "fixed_design_chain_evidence.rds")
)
rqr_confirm_atomic_write_csv(
  horizon_checks, file.path(output_root, "horizon_checks.csv")
)
rqr_confirm_atomic_write_csv(
  fixed_diagnostics,
  file.path(output_root, "fixed_design_diagnostics.csv")
)
rqr_confirm_atomic_write_csv(
  fixed_summary, file.path(output_root, "fixed_design_summary.csv")
)
rqr_confirm_atomic_write_csv(
  dynamic_check, file.path(output_root, "dynamic_endpoint_check.csv")
)

all_fixed_fits <- all(vapply(
  fixed_results, function(result) isTRUE(result$ok), logical(1L)
))
all_fixed_reproducible <- if (nzchar(expected_commit)) {
  all(vapply(
    fixed_results,
    function(result) {
      isTRUE(result$ok) &&
        isTRUE(result$reproducibility_eligible)
    },
    logical(1L)
  ))
} else {
  NA
}
manifest <- list(
  schema_version =
    "rqrgibbs_dlm_horizon_fixed_design_validation/1.0.0",
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
  horizon_scenarios = nrow(horizon_checks),
  horizon_checks_passed = all(horizon_checks$pass),
  fixed_design_standard_tasks = nrow(standard_tasks),
  fixed_design_standard_schedule =
    contract$config$schedules$fixed_design_rqr_standard,
  fixed_design_fits_succeeded = all_fixed_fits,
  fixed_design_diagnostics_passed = all(fixed_diagnostics$pass),
  fixed_design_reproducibility_eligible = all_fixed_reproducible,
  dynamic_endpoint_check_passed = isTRUE(dynamic_check$pass),
  comparative_simulation_metrics_used = FALSE,
  failed_outputs_reused = FALSE,
  workers = workers,
  thread_environment = as.list(thread_environment),
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
    "Horizon/fixed-design gate: %d/%d horizon scenarios, ",
    "%d/%d M03 diagnostics, dynamic endpoint %s.\n"
  ),
  sum(horizon_checks$pass), nrow(horizon_checks),
  sum(fixed_diagnostics$pass), nrow(fixed_diagnostics),
  if (isTRUE(dynamic_check$pass)) "passed" else "failed"
))
if (!all(horizon_checks$pass) ||
    !all_fixed_fits ||
    !all(fixed_diagnostics$pass) ||
    (nzchar(expected_commit) && !isTRUE(all_fixed_reproducible)) ||
    !isTRUE(dynamic_check$pass)) {
  stop("The horizon/fixed-design correction gate failed.",
       call. = FALSE)
}
