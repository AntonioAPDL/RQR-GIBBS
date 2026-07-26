#!/usr/bin/env Rscript

# Fail-closed main RQR-DLM simulation runner.
#
# Output-15 authorizes implementation and reference work only.  The checked-in
# config keeps both execution flags false, so sentinel-core and
# execute-confirmatory stop before creating an output directory.

arguments <- commandArgs(trailingOnly = TRUE)
if (!length(arguments) %in% c(1L, 2L)) {
  stop(
    paste(
      "Usage: 15_run_rqr_dlm_confirmatory_simulation.R",
      "<preflight|oracle-reference|sentinel-core|execute-confirmatory|",
      "collect|audit> [output_dir]"
    ),
    call. = FALSE
  )
}
mode <- arguments[[1L]]
modes <- c(
  "preflight", "oracle-reference", "sentinel-core",
  "execute-confirmatory", "collect", "audit"
)
if (!mode %in% modes) stop("Unknown confirmatory runner mode.",
                           call. = FALSE)

repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(repo_root, "application", "DESCRIPTION"))) {
  stop("Run the confirmatory runner from the RQR-GIBBS root.",
       call. = FALSE)
}
sys.source(
  file.path(
    repo_root, "application", "scripts", "lib",
    "rqr_dlm_confirmatory_simulation.R"
  ),
  envir = environment()
)
contract <- rqr_confirm_read_contract(repo_root)
rqr_confirm_validate_contract(
  contract,
  require_closed = FALSE
)
rqr_confirm_validate_budget(contract)

expected_commit <- Sys.getenv("RQR_EXPECTED_PRIMARY_COMMIT", unset = "")
primary_attestation_path <- Sys.getenv(
  "RQR_PRIMARY_RUNTIME_ATTESTATION", unset = ""
)
primary_binding <- NULL
if (nzchar(expected_commit)) {
  if (!nzchar(primary_attestation_path) ||
      !requireNamespace("rqrgibbs", quietly = TRUE)) {
    stop(
      "An expected commit requires the exact isolated primary runtime.",
      call. = FALSE
    )
  }
  sys.source(
    file.path(
      repo_root, "application", "scripts", "lib",
      "isolated_runtime_lineage.R"
    ),
    envir = environment()
  )
  sys.source(
    file.path(
      repo_root, "application", "scripts", "lib",
      "rqr_dlm_main_simulation.R"
    ),
    envir = environment()
  )
  primary_binding <- rqr_main_primary_runtime_binding(
    repo_root, expected_commit, primary_attestation_path
  )
}
authorization_path <- Sys.getenv(
  "RQR_CONFIRMATORY_AUTHORIZATION_BUNDLE", unset = ""
)
authorization <- if (nzchar(authorization_path)) {
  jsonlite::read_json(authorization_path, simplifyVector = TRUE)
} else {
  NULL
}
if (mode %in% c("sentinel-core", "execute-confirmatory") &&
    !isTRUE(contract$config$confirmatory_execution_authorized)) {
  rqr_confirm_authorized(
    contract, mode, expected_commit, authorization
  )
}
output_dir <- if (length(arguments) == 2L) {
  normalizePath(arguments[[2L]], winslash = "/", mustWork = FALSE)
} else {
  file.path(
    repo_root, "application", "outputs",
    "rqr_dlm_main_simulation_20260724", mode
  )
}
if (file.exists(output_dir) || dir.exists(output_dir)) {
  stop("The requested output directory must be fresh.", call. = FALSE)
}
dir.create(dirname(output_dir), recursive = TRUE, showWarnings = FALSE)
staging <- tempfile(
  paste0(".", basename(output_dir), "-"), tmpdir = dirname(output_dir)
)
dir.create(staging)
published <- FALSE
on.exit({
  if (!published) unlink(staging, recursive = TRUE, force = TRUE)
}, add = TRUE)

write_csv <- function(value, filename) {
  rqr_confirm_atomic_write_csv(value, file.path(staging, filename))
}
write_json <- function(value, filename) {
  rqr_confirm_atomic_write_json(value, file.path(staging, filename))
}

source_commit <- if (nzchar(expected_commit)) {
  tolower(expected_commit)
} else {
  trimws(system2(
    "git", c("-C", shQuote(repo_root), "rev-parse", "HEAD"),
    stdout = TRUE, env = c("GIT_OPTIONAL_LOCKS=0")
  ))
}
config_path <- file.path(
  repo_root, "application", "config", "rqr_dlm",
  "rqr_dlm_main_simulation_20260724.R"
)
contract_digests <- list(
  source_commit = source_commit,
  config_sha256 = rqr_confirm_sha256(config_path),
  incidence_sha256 = contract$config$review_contract$incidence_sha256,
  budget_sha256 = contract$config$review_contract$budget_sha256,
  gates_sha256 = contract$config$review_contract$gates_sha256
)
stage_status <- "passed"
stage_exit_status <- 0L

if (mode == "preflight") {
  ledger <- rqr_confirm_seed_ledger(contract, planning = "maximum")
  ledger <- rqr_confirm_validate_seed_ledger(
    ledger, contract, planning = "maximum", require_complete = TRUE
  )
  fit_plan <- rqr_confirm_fit_plan(contract, planning = "maximum")
  sentinels <- rqr_confirm_sentinel_map(contract, planning = "maximum")
  replication_plan <- rqr_confirm_replication_plan(
    contract, planning = "maximum"
  )
  initial_replication_plan <- rqr_confirm_replication_plan(
    contract, planning = "initial"
  )
  central_replication_plan <- rqr_confirm_replication_plan(
    contract, planning = "central"
  )
  wave_plan <- rqr_confirm_wave_plan(contract, planning = "maximum")
  sentinel_plan <- replication_plan[
    replication_plan$embedded_sentinel, , drop = FALSE
  ]
  sentinel_state_rows <- match(
    sentinels$selection_task_key, ledger$task_key
  )
  sentinel_states_bound <- !anyNA(sentinel_state_rows) &&
    identical(
      sentinels$selection_state_digest,
      ledger$state_digest[sentinel_state_rows]
    )
  budget_rows <- do.call(rbind, lapply(
    c("initial", "central", "maximum"),
    function(planning) {
      value <- rqr_confirm_budget_summary(contract, planning)
      value$planning <- planning
      value
    }
  ))
  disk <- system2(
    "df", c("-Pk", shQuote(file.path(repo_root, "application"))),
    stdout = TRUE
  )
  disk_fields <- strsplit(trimws(tail(disk, 1L)), "[[:space:]]+")[[1L]]
  available_GiB <- as.numeric(disk_fields[[4L]]) / 1024^2
  resources <- data.frame(
    workers = contract$config$resources$workers,
    sentinel_workers = contract$config$resources$sentinel_workers,
    threads_per_worker = contract$config$resources$threads_per_worker,
    sampled_process_group_thread_ceiling =
      contract$config$resources$sampled_process_group_thread_ceiling,
    sampled_reference_process_group_thread_ceiling =
      contract$config$resources$
        sampled_reference_process_group_thread_ceiling,
    sampled_thread_ceiling_role =
      contract$config$resources$sampled_thread_ceiling_role,
    per_worker_memory_GiB =
      contract$config$resources$per_worker_memory_GiB,
    available_disk_GiB = available_GiB,
    disk_required_GiB =
      contract$config$resources$free_space_required_GiB,
    process_wave_ceiling_hours =
      contract$config$resources$process_wave_ceiling_hours,
    batch_boundary_resume_only =
      contract$config$resources$
        resumable_only_at_completed_batch_boundaries,
    stringsAsFactors = FALSE
  )
  if (available_GiB <
      contract$config$resources$free_space_required_GiB) {
    stop("The main study has less than 50 GiB available.", call. = FALSE)
  }
  artifact_schemas <- rqr_confirm_artifact_schemas()
  schema_rows <- do.call(rbind, lapply(
    names(artifact_schemas),
    function(name) data.frame(
      artifact = name,
      position = seq_along(artifact_schemas[[name]]),
      field = artifact_schemas[[name]],
      stringsAsFactors = FALSE
    )
  ))
  initialization_profiles <- c(
    list(standard = contract$config$standard_initialization),
    contract$config$initialization_profiles
  )
  initialization_manifest <- do.call(rbind, lapply(
    names(initialization_profiles),
    function(profile_name) {
      profile <- initialization_profiles[[profile_name]]
      data.frame(
        profile = profile_name,
        role = if (profile_name == "standard") {
          "standard_single_chain"
        } else {
          "preselected_four_chain_sentinel"
        },
        midpoint_rule = profile$midpoint_rule,
        midpoint_shift_training_sd =
          profile$midpoint_shift_training_sd,
        half_width_multiplier = profile$half_width_multiplier,
        lambda_initial = profile$lambda_initial,
        component_scale_multiplier =
          profile$component_scale_multiplier,
        component_scale_reference =
          profile$component_scale_reference %||%
            "inverse_gamma_prior_median_multiplier",
        stringsAsFactors = FALSE
      )
    }
  ))
  scenario_contract <- do.call(rbind, lapply(
    names(contract$config$scenarios),
    function(scenario_id) {
      scenario <- contract$config$scenarios[[scenario_id]]
      data.frame(
        DGP = scenario_id, dgp = scenario$dgp, T = scenario$T,
        coverage = scenario$coverage, common_random_number_pair =
          scenario$pair, precision_batch_group = scenario$batch_group,
        forecast_horizon =
          contract$config$design$forecast_horizon,
        future_subreplications =
          contract$config$design$future_subreplications,
        generalized_bayes = TRUE,
        response_prediction_contract = FALSE,
        stringsAsFactors = FALSE
      )
    }
  ))
  metric_manifest <- data.frame(
    metric = c(
      "heldout_rqr_loss", "aggregate_coverage", "mean_width",
      "central_interval_score", "endpoint_rmse_lower",
      "endpoint_rmse_upper", "cross_target_distance",
      "realized_root_rmse", "coverage_h01", "coverage_h05",
      "coverage_h10", "coverage_h20"
    ),
    role = c(
      "primary_home_target", "operating_characteristic",
      "operating_characteristic", "secondary_equal_tailed_score",
      "target_aligned", "target_aligned",
      "quantile_to_RQR_cross_target_only", "dynamic_path_diagnostic",
      rep("horizon_operating_characteristic", 4L)
    ),
    response_predictive_score = FALSE,
    stringsAsFactors = FALSE
  )
  gates <- data.frame(
    gate = c(
      "schema_1_0", "incidence_208_rows", "included_89",
      "omitted_119", "budget_reproduced", "candidate_fits_zero",
      "LEcuyer_full_state_unique", "future_substreams_20",
      "sentinels_preselected", "execution_state_valid",
      "disk_at_least_50_GiB", "workers_at_most_32",
      "one_thread_per_worker", "sampled_worker_thread_envelope_four",
      "sampled_reference_thread_envelope_four",
      "artifact_schemas_frozen",
      "primary_runtime_binding_when_requested",
      "replication_task_ids_unique", "sentinel_tasks_preselected",
      "sentinel_selection_states_bound", "wave_tasks_exact",
      "wave_worker_limits"
    ),
    value = c(
      TRUE, nrow(contract$incidence) == 208L,
      sum(rqr_confirm_included(contract$incidence)) == 89L,
      sum(!rqr_confirm_included(contract$incidence)) == 119L,
      TRUE, contract$config$design$candidate_tuning_fits == 0L,
      !anyDuplicated(ledger$state_digest),
      contract$config$design$future_subreplications == 20L,
      all(sentinels$selected_before_data),
      !contract$config$diagnostic_pilot_execution_authorized &&
        (
          !contract$config$confirmatory_execution_authorized ||
            (
              nzchar(expected_commit) &&
                !is.null(primary_binding)
            )
        ),
      available_GiB >= 50,
      contract$config$resources$workers <= 32L,
      contract$config$resources$threads_per_worker == 1L,
      contract$config$resources$sampled_process_group_thread_ceiling == 4L &&
        identical(
          contract$config$resources$sampled_thread_ceiling_role,
          "hard_OS_thread_envelope_not_compute_parallelism"
        ),
      contract$config$resources$
        sampled_reference_process_group_thread_ceiling == 4L,
      TRUE, !nzchar(expected_commit) || !is.null(primary_binding),
      !anyDuplicated(replication_plan$replication_task_id),
      nrow(sentinel_plan) > 0L &&
        all(sentinel_plan$embedded_sentinel),
      sentinel_states_bound,
      identical(
        sort(wave_plan$replication_task_id, method = "radix"),
        sort(replication_plan$replication_task_id, method = "radix")
      ),
      all(wave_plan$worker_slot >= 1L) &&
        all(wave_plan$worker_slot <= wave_plan$worker_limit) &&
        all(
          wave_plan$worker_limit[wave_plan$phase == "sentinel"] ==
            contract$config$resources$sentinel_workers
        ) &&
        all(
          wave_plan$worker_limit[wave_plan$phase == "standard"] ==
            contract$config$resources$workers
        )
    ),
    stringsAsFactors = FALSE
  )
  if (!all(gates$value)) stop("One or more preflight gates failed.",
                              call. = FALSE)

  fault_root <- file.path(staging, ".atomic_fault_probe")
  dir.create(fault_root)
  atomic_fault <- function(kind) {
    path <- file.path(fault_root, paste0("forbidden.", kind))
    observed <- tryCatch({
      if (kind == "csv") {
        rqr_confirm_atomic_write_csv(
          data.frame(value = 1), path, inject_failure = TRUE
        )
      } else {
        rqr_confirm_atomic_write_json(
          list(value = 1), path, inject_failure = TRUE
        )
      }
      FALSE
    }, error = function(error) {
      grepl("Injected atomic", conditionMessage(error), fixed = TRUE)
    })
    residual <- list.files(
      fault_root, all.files = TRUE, no.. = TRUE, full.names = TRUE
    )
    data.frame(
      fault = paste0("atomic_", kind, "_before_rename"),
      injected_error_observed = observed,
      final_path_absent = !file.exists(path),
      temporary_paths_absent = length(residual) == 0L,
      pass = observed && !file.exists(path) && length(residual) == 0L,
      stringsAsFactors = FALSE
    )
  }
  fault_rows <- rbind(atomic_fault("csv"), atomic_fault("json"))
  unlink(fault_root, recursive = TRUE, force = TRUE)
  if (!all(fault_rows$pass) || dir.exists(fault_root)) {
    stop("An atomic rollback fault gate failed.", call. = FALSE)
  }
  negative_execute <- tryCatch({
    rqr_confirm_authorized(
      contract, "execute-confirmatory",
      paste(rep("a", 40L), collapse = ""),
      authorization_bundle = NULL
    )
    data.frame(
      test = "execute_without_complete_authorization_bundle",
      rejected = FALSE, message = "", pass = FALSE,
      stringsAsFactors = FALSE
    )
  }, error = function(error) {
    message <- conditionMessage(error)
    data.frame(
      test = "execute_without_complete_authorization_bundle",
      rejected = TRUE, message = message,
      pass =
        grepl(
          "execution is disabled", message,
          fixed = TRUE, ignore.case = TRUE
        ) ||
        grepl(
          "authorization is incomplete", message,
          fixed = TRUE, ignore.case = TRUE
        ),
      stringsAsFactors = FALSE
    )
  })
  if (!isTRUE(negative_execute$pass[[1L]])) {
    stop("The negative execute-authorization test failed.",
         call. = FALSE)
  }

  write_csv(gates, "preflight_gates.csv")
  write_csv(contract$incidence, "incidence_matrix.csv")
  write_csv(fit_plan, "fit_plan_maximum.csv")
  write_csv(replication_plan, "replication_plan_maximum.csv")
  write_csv(initial_replication_plan, "replication_plan_initial.csv")
  write_csv(central_replication_plan, "replication_plan_central.csv")
  write_csv(wave_plan, "execution_wave_plan_maximum.csv")
  write_csv(sentinel_plan, "sentinel_task_plan_maximum.csv")
  write_csv(sentinels, "sentinel_ledger_maximum.csv")
  write_csv(budget_rows, "recomputed_budget.csv")
  write_csv(ledger, "seed_ledger_maximum.csv")
  write_csv(resources, "resource_preflight.csv")
  write_csv(schema_rows, "artifact_schemas.csv")
  write_csv(initialization_manifest, "initialization_manifest.csv")
  write_csv(scenario_contract, "scenario_contract.csv")
  write_csv(metric_manifest, "metric_manifest.csv")
  write_csv(fault_rows, "atomic_fault_test.csv")
  write_csv(negative_execute, "negative_execute_test.csv")
  write_json(contract_digests, "contract_digests.json")
}

if (mode == "oracle-reference") {
  if (!requireNamespace("rqrgibbs", quietly = TRUE) ||
      is.null(primary_binding)) {
    stop("oracle-reference requires the isolated rqrgibbs runtime.",
         call. = FALSE)
  }
  suppressPackageStartupMessages(library(rqrgibbs))
  ledger_path <- Sys.getenv("RQR_CONFIRM_SEED_LEDGER", unset = "")
  if (!nzchar(ledger_path) || !file.exists(ledger_path)) {
    stop(
      "oracle-reference requires RQR_CONFIRM_SEED_LEDGER from preflight.",
      call. = FALSE
    )
  }
  ledger <- utils::read.csv(
    ledger_path, stringsAsFactors = FALSE, check.names = FALSE
  )
  ledger <- rqr_confirm_validate_seed_ledger(
    ledger, contract, planning = "maximum", require_complete = TRUE
  )
  families <- c("gaussian", "skewed", "t5", "break_mixture")
  oracle_rows <- do.call(rbind, lapply(families, function(family) {
    oracle_spec <- rqr_confirm_oracle_spec(family)
    do.call(rbind, lapply(c(0.80, 0.90), function(coverage) {
      certificate <- rqr_oracle_certificate(
        oracle_spec$family, coverage, params = oracle_spec$params,
        tol = contract$config$oracle$primary_tolerance,
        grid_size = contract$config$oracle$profile_grid_size
      )
      higher_precision <- rqr_oracle_certificate(
        oracle_spec$family, coverage, params = oracle_spec$params,
        tol = contract$config$oracle$higher_precision_tolerance,
        grid_size = contract$config$oracle$higher_precision_grid_size
      )
      data.frame(
        family = family, coverage = coverage,
        lower_root = certificate$lower_root,
        upper_root = certificate$upper_root,
        profile_objective = certificate$profile_objective,
        unrestricted_objective = certificate$unrestricted_objective,
        objective_gap = certificate$global_objective_gap,
        coverage_residual = certificate$coverage_residual,
        moment_residual = certificate$moment_residual,
        unique_minimizer = certificate$unique_minimizer,
        grid_size = contract$config$oracle$profile_grid_size,
        higher_precision_grid_size =
          contract$config$oracle$higher_precision_grid_size,
        higher_precision_lower_root =
          higher_precision$lower_root,
        higher_precision_upper_root =
          higher_precision$upper_root,
        higher_precision_endpoint_difference = max(abs(c(
          certificate$lower_root - higher_precision$lower_root,
          certificate$upper_root - higher_precision$upper_root
        ))),
        higher_precision_objective_difference = abs(
          certificate$profile_objective -
            higher_precision$profile_objective
        ),
        estimated_numerical_error =
          certificate$estimated_quadrature_error,
        numerical_error_is_rigorous_bound = FALSE,
        stringsAsFactors = FALSE
      )
    }))
  }))
  oracle_pass <- all(
    abs(oracle_rows$coverage_residual) <=
      contract$config$oracle$coverage_residual_max
  ) && all(
    abs(oracle_rows$moment_residual) <=
      contract$config$oracle$moment_residual_max
  ) && all(
    oracle_rows$objective_gap <=
      contract$config$oracle$objective_gap_max
  ) && all(
    oracle_rows$higher_precision_objective_difference <=
      contract$config$oracle$higher_precision_objective_max
  ) && all(
    oracle_rows$unique_minimizer
  )
  if (!oracle_pass) stop("A high-precision oracle gate failed.",
                         call. = FALSE)
  location_scale_rows <- do.call(rbind, lapply(
    seq_len(nrow(oracle_rows)), function(index) {
      family <- oracle_rows$family[[index]]
      coverage <- oracle_rows$coverage[[index]]
      oracle_spec <- rqr_confirm_oracle_spec(family)
      mu <- c(-1, 0.5, 2)
      scale <- c(0.25, 2, 5)
      endpoints <- rqr_oracle_endpoints(
        mu, scale, oracle_spec$family, coverage,
        params = oracle_spec$params,
        tol = contract$config$oracle$primary_tolerance
      )
      base_roots <- rqr_oracle_roots(
        oracle_spec$family, coverage,
        params = oracle_spec$params,
        tol = contract$config$oracle$primary_tolerance
      )
      expected_lower <-
        mu + scale * base_roots$lower_root
      expected_upper <-
        mu + scale * base_roots$upper_root
      data.frame(
        family = family, coverage = coverage,
        case = seq_along(mu), location = mu, scale = scale,
        maximum_endpoint_error = pmax(
          abs(endpoints$lower - expected_lower),
          abs(endpoints$upper - expected_upper)
        ),
        pass = pmax(
          abs(endpoints$lower - expected_lower),
          abs(endpoints$upper - expected_upper)
        ) <= 1e-12,
        stringsAsFactors = FALSE
      )
    }
  ))
  if (!all(location_scale_rows$pass)) {
    stop("The location-scale oracle gate failed.", call. = FALSE)
  }

  dgp_rows <- vector("list", length(contract$config$scenarios))
  scenario_ids <- names(contract$config$scenarios)
  for (index in seq_along(scenario_ids)) {
    generated <- rqr_confirm_generate_dgp(
      contract, scenario_ids[[index]], 1L, ledger
    )
    delta <- sweep(
      generated$realized_root_path,
      c(2L, 3L), generated$oracle_conditional_mean_root, "-"
    )
    dgp_rows[[index]] <- data.frame(
      scenario = scenario_ids[[index]],
      training_n = length(generated$training_y),
      future_subreplications = nrow(generated$generated_future_response),
      horizon = ncol(generated$generated_future_response),
      finite = all(is.finite(c(
        generated$training_y, generated$training_roots,
        generated$oracle_conditional_mean_root,
        generated$quantile_conditional_mean_root,
        generated$realized_root_path,
        generated$realized_quantile_path,
        generated$generated_future_response
      ))),
      ordered = all(
        generated$training_roots[, "upper"] >
          generated$training_roots[, "lower"]
      ),
      conditional_realized_distinct =
        generated$dgp == "static_gaussian" || any(abs(delta) > 0),
      response_prediction_contract = FALSE,
      stringsAsFactors = FALSE
    )
  }
  dgp_rows <- do.call(rbind, dgp_rows)
  if (!all(dgp_rows$finite & dgp_rows$ordered &
           dgp_rows$conditional_realized_distinct)) {
    stop("One or more canonical DGP references failed.", call. = FALSE)
  }

  byte_rows <- do.call(rbind, lapply(
    contract$config$reference$byte_reproduction_replications,
    function(replication) {
      first <- rqr_confirm_generate_dgp(
        contract,
        contract$config$reference$byte_reproduction_scenario,
        replication, ledger
      )
      second <- rqr_confirm_generate_dgp(
        contract,
        contract$config$reference$byte_reproduction_scenario,
        replication, ledger
      )
      first_bytes <- serialize(
        first, NULL,
        version = contract$config$reference$serialization_version
      )
      second_bytes <- serialize(
        second, NULL,
        version = contract$config$reference$serialization_version
      )
      first_sha <- digest::digest(
        first_bytes, algo = "sha256", serialize = FALSE
      )
      second_sha <- digest::digest(
        second_bytes, algo = "sha256", serialize = FALSE
      )
      data.frame(
        scenario =
          contract$config$reference$byte_reproduction_scenario,
        replication = replication,
        serialization_version =
          contract$config$reference$serialization_version,
        first_sha256 = first_sha,
        second_sha256 = second_sha,
        identical_bytes = identical(first_bytes, second_bytes),
        pass = identical(first_sha, second_sha) &&
          identical(first_bytes, second_bytes),
        stringsAsFactors = FALSE
      )
    }
  ))
  if (!all(byte_rows$pass)) {
    stop("The two-replication byte-reproduction gate failed.",
         call. = FALSE)
  }

  exdqlm_path <- Sys.getenv("RQR_EXDQLM_CRAN_ATTESTATION", unset = "")
  quantreg_path <- Sys.getenv("RQR_QUANTREG_CRAN_ATTESTATION", unset = "")
  if (!nzchar(exdqlm_path) || !nzchar(quantreg_path)) {
    stop("Both comparator attestation paths are required.", call. = FALSE)
  }
  exdqlm <- rqr_confirm_exdqlm_reference(
    contract, exdqlm_path, full_schedule = TRUE
  )
  quantreg <- rqr_confirm_quantreg_reference(contract, quantreg_path)
  comparator_summary <- rbind(
    data.frame(
      package = exdqlm$summary$package,
      version = exdqlm$summary$version,
      fitting_function = exdqlm$summary$fitting_function,
      method = "reduced_AL_dqlm_ind_TRUE",
      formals_digest = exdqlm$summary$formals_digest,
      runtime_tree_digest = exdqlm$summary$runtime_tree_digest,
      pass = exdqlm$summary$pass,
      stringsAsFactors = FALSE
    ),
    data.frame(
      package = quantreg$summary$package,
      version = quantreg$summary$version,
      fitting_function = quantreg$summary$fitting_function,
      method = quantreg$summary$method,
      formals_digest = quantreg$summary$formals_digest,
      runtime_tree_digest = quantreg$summary$runtime_tree_digest,
      pass = quantreg$summary$pass,
      stringsAsFactors = FALSE
    )
  )
  write_csv(oracle_rows, "oracle_certificates.csv")
  write_csv(
    location_scale_rows, "oracle_location_scale_checks.csv"
  )
  write_csv(dgp_rows, "canonical_dgp_references.csv")
  write_csv(byte_rows, "two_replication_byte_reproduction.csv")
  write_csv(comparator_summary, "comparator_reference.csv")
  write_csv(exdqlm$summary, "exdqlm_reference_contract.csv")
  write_csv(exdqlm$raw_forecasts, "exdqlm_raw_forecasts.csv")
  exdqlm_dependencies <- exdqlm$dependency_manifest
  exdqlm_dependencies$root_package <- "exdqlm"
  quantreg_dependencies <- quantreg$dependency_manifest
  quantreg_dependencies$root_package <- "quantreg"
  dependency_manifest <- rbind(
    exdqlm_dependencies, quantreg_dependencies
  )
  toolchain_manifest <- rqr_confirm_toolchain_manifest()
  write_csv(
    dependency_manifest,
    "comparator_dependency_manifest.csv"
  )
  write_csv(toolchain_manifest, "toolchain_manifest.csv")
  write_csv(quantreg$summary, "quantreg_reference_contract.csv")
  write_csv(quantreg$raw_forecasts, "quantreg_raw_forecasts.csv")
  reference_gates <- data.frame(
    gate = c(
      "oracle_coverage_residual", "oracle_moment_residual",
      "oracle_objective_gap", "oracle_higher_precision_objective",
      "oracle_unique_minimizer",
      "oracle_location_scale_equivariance", "canonical_DGPs",
      "two_replication_byte_reproduction", "exdqlm_actual_fit",
      "quantreg_actual_fit", "dependency_runtime_digests",
      "toolchain_manifest_complete", "primary_runtime_binding",
      "protected_checkout_unused",
      "seed_ledger_complete"
    ),
    value = c(
      all(abs(oracle_rows$coverage_residual) <=
          contract$config$oracle$coverage_residual_max),
      all(abs(oracle_rows$moment_residual) <=
          contract$config$oracle$moment_residual_max),
      all(oracle_rows$objective_gap <=
          contract$config$oracle$objective_gap_max),
      all(oracle_rows$higher_precision_objective_difference <=
          contract$config$oracle$higher_precision_objective_max),
      all(oracle_rows$unique_minimizer),
      all(location_scale_rows$pass),
      all(dgp_rows$finite & dgp_rows$ordered &
          dgp_rows$conditional_realized_distinct),
      all(byte_rows$pass),
      isTRUE(exdqlm$summary$pass),
      isTRUE(quantreg$summary$pass),
      nrow(dependency_manifest) > 2L &&
        all(grepl(
          "^[0-9a-f]{64}$",
          dependency_manifest$runtime_tree_digest
        )),
      nrow(toolchain_manifest) >= 10L &&
        !anyNA(toolchain_manifest) &&
        all(nzchar(toolchain_manifest$value)),
      isTRUE(primary_binding$match),
      !isTRUE(exdqlm$attestation$protected_exdqlm_checkout_used) &&
        !isTRUE(
          quantreg$attestation$protected_exdqlm_checkout_used
        ),
      TRUE
    ),
    stringsAsFactors = FALSE
  )
  if (!all(reference_gates$value)) {
    stop("One or more complete reference gates failed.",
         call. = FALSE)
  }
  write_csv(reference_gates, "reference_gates.csv")
  write_json(
    list(
      schema_version = "rqrgibbs_dlm_runtime_bundle/1.0.0",
      primary = primary_binding,
      exdqlm = list(
        source_package_sha256 =
          exdqlm$attestation$source_package_sha256,
        runtime_tree_digest =
          exdqlm$attestation$runtime_tree_digest,
        protected_checkout_used =
          exdqlm$attestation$protected_exdqlm_checkout_used
      ),
      quantreg = list(
        source_package_sha256 =
          quantreg$attestation$source_package_sha256,
        runtime_tree_digest =
          quantreg$attestation$runtime_tree_digest,
        protected_checkout_used =
          quantreg$attestation$protected_exdqlm_checkout_used
      )
    ),
    "runtime_bundle.json"
  )
  write_json(contract_digests, "contract_digests.json")
}

if (mode %in% c("sentinel-core", "execute-confirmatory")) {
  if (!requireNamespace("rqrgibbs", quietly = TRUE) ||
      !requireNamespace("posterior", quietly = TRUE) ||
      is.null(primary_binding)) {
    stop(
      "Execution requires the exact isolated rqrgibbs runtime and posterior.",
      call. = FALSE
    )
  }
  suppressPackageStartupMessages(library(rqrgibbs))
  task_path <- Sys.getenv("RQR_CONFIRM_TASK_FILE", unset = "")
  canonical_task_path <- Sys.getenv(
    "RQR_CONFIRMATORY_CANONICAL_TASK_PLAN", unset = ""
  )
  ledger_path <- Sys.getenv("RQR_CONFIRM_SEED_LEDGER", unset = "")
  exdqlm_path <- Sys.getenv("RQR_EXDQLM_CRAN_ATTESTATION", unset = "")
  quantreg_path <- Sys.getenv("RQR_QUANTREG_CRAN_ATTESTATION", unset = "")
  preflight_hashes_path <- Sys.getenv(
    "RQR_CONFIRMATORY_PREFLIGHT_ARTIFACT_HASHES", unset = ""
  )
  reference_hashes_path <- Sys.getenv(
    "RQR_CONFIRMATORY_REFERENCE_ARTIFACT_HASHES", unset = ""
  )
  reference_runtime_bundle_path <- Sys.getenv(
    "RQR_CONFIRMATORY_REFERENCE_RUNTIME_BUNDLE", unset = ""
  )
  reference_dependency_manifest_path <- Sys.getenv(
    "RQR_CONFIRMATORY_REFERENCE_DEPENDENCY_MANIFEST", unset = ""
  )
  reference_toolchain_manifest_path <- Sys.getenv(
    "RQR_CONFIRMATORY_REFERENCE_TOOLCHAIN_MANIFEST", unset = ""
  )
  required_paths <- c(
    task_path, canonical_task_path, ledger_path, exdqlm_path, quantreg_path,
    primary_attestation_path, preflight_hashes_path,
    reference_hashes_path, reference_runtime_bundle_path,
    reference_dependency_manifest_path, reference_toolchain_manifest_path
  )
  if (any(!nzchar(required_paths)) || any(!file.exists(required_paths))) {
    stop("Execution input, seed, runtime, or attestation files are missing.",
      call. = FALSE)
  }
  disk <- system2(
    "df", c("-Pk", shQuote(file.path(repo_root, "application"))),
    stdout = TRUE
  )
  disk_fields <- strsplit(
    trimws(tail(disk, 1L)), "[[:space:]]+"
  )[[1L]]
  available_GiB <- as.numeric(disk_fields[[4L]]) / 1024^2
  if (!is.finite(available_GiB) ||
      available_GiB <
        contract$config$resources$free_space_required_GiB) {
    stop("The execution wave has less than 50 GiB available.",
         call. = FALSE)
  }
  exdqlm_attestation <- rqr_confirm_read_attestation(
    exdqlm_path, "exdqlm",
    contract$config$comparator$exdqlm$version,
    contract$config$comparator$exdqlm$source_sha256
  )
  quantreg_attestation <- rqr_confirm_read_attestation(
    quantreg_path, "quantreg",
    contract$config$comparator$quantreg$version,
    contract$config$comparator$quantreg$source_sha256
  )
  rqr_confirm_load_attested_namespace("exdqlm", exdqlm_attestation)
  rqr_confirm_load_attested_namespace("quantreg", quantreg_attestation)
  current_exdqlm_dependencies <- rqr_confirm_dependency_manifest(
    "exdqlm", dirname(exdqlm_attestation$runtime_path)
  )
  current_exdqlm_dependencies$root_package <- "exdqlm"
  current_quantreg_dependencies <- rqr_confirm_dependency_manifest(
    "quantreg", dirname(quantreg_attestation$runtime_path)
  )
  current_quantreg_dependencies$root_package <- "quantreg"
  current_dependencies <- rbind(
    current_exdqlm_dependencies, current_quantreg_dependencies
  )
  reference_dependencies <- utils::read.csv(
    reference_dependency_manifest_path,
    stringsAsFactors = FALSE, check.names = FALSE
  )
  rownames(current_dependencies) <- rownames(reference_dependencies) <- NULL
  comparator_dependency_runtime_match <- identical(
    current_dependencies, reference_dependencies
  )
  current_toolchain <- rqr_confirm_toolchain_manifest()
  reference_toolchain <- utils::read.csv(
    reference_toolchain_manifest_path,
    stringsAsFactors = FALSE, check.names = FALSE
  )
  rownames(current_toolchain) <- rownames(reference_toolchain) <- NULL
  toolchain_match <- identical(current_toolchain, reference_toolchain)
  reference_runtime_bundle <- jsonlite::read_json(
    reference_runtime_bundle_path, simplifyVector = TRUE
  )
  reference_runtime_bundle_match <-
    identical(
      reference_runtime_bundle$schema_version,
      "rqrgibbs_dlm_runtime_bundle/1.0.0"
    ) &&
    identical(
      reference_runtime_bundle$primary$runtime_tree_digest,
      primary_binding$runtime_tree_digest
    ) &&
    identical(
      reference_runtime_bundle$exdqlm$source_package_sha256,
      exdqlm_attestation$source_package_sha256
    ) &&
    identical(
      reference_runtime_bundle$exdqlm$runtime_tree_digest,
      exdqlm_attestation$runtime_tree_digest
    ) &&
    identical(
      reference_runtime_bundle$quantreg$source_package_sha256,
      quantreg_attestation$source_package_sha256
    ) &&
    identical(
      reference_runtime_bundle$quantreg$runtime_tree_digest,
      quantreg_attestation$runtime_tree_digest
    ) &&
    !isTRUE(reference_runtime_bundle$exdqlm$protected_checkout_used) &&
    !isTRUE(reference_runtime_bundle$quantreg$protected_checkout_used)
  git_status <- system2(
    "git",
    c(
      "-C", shQuote(repo_root), "status", "--porcelain=v2",
      "--untracked-files=all"
    ),
    stdout = TRUE, stderr = TRUE,
    env = c("GIT_OPTIONAL_LOCKS=0", "GIT_TERMINAL_PROMPT=0")
  )
  git_status_code <- attr(git_status, "status")
  if (is.null(git_status_code)) git_status_code <- 0L
  reviewed_implementation_commit <-
    authorization$reviewed_implementation_commit %||% ""
  observed_authorization <- list(
    reviewed_implementation_commit =
      reviewed_implementation_commit,
    authorization_diff_only_flag =
      rqr_confirm_flag_only_authorization_diff(
        repo_root, reviewed_implementation_commit, expected_commit
      ),
    primary_worktree_clean =
      identical(as.integer(git_status_code), 0L) &&
      !length(git_status),
    primary_runtime_tree_digest =
      primary_binding$runtime_tree_digest,
    preflight_artifact_hashes_sha256 =
      rqr_confirm_sha256(preflight_hashes_path),
    reference_artifact_hashes_sha256 =
      rqr_confirm_sha256(reference_hashes_path),
    seed_ledger_sha256 = rqr_confirm_sha256(ledger_path),
    task_plan_sha256 = rqr_confirm_sha256(canonical_task_path),
    exdqlm_source_sha256 =
      exdqlm_attestation$source_package_sha256,
    quantreg_source_sha256 =
      quantreg_attestation$source_package_sha256,
    reference_runtime_bundle_match = reference_runtime_bundle_match,
    comparator_dependency_runtime_match =
      comparator_dependency_runtime_match,
    toolchain_match = toolchain_match,
    protected_checkout_used =
      isTRUE(exdqlm_attestation$protected_exdqlm_checkout_used) ||
      isTRUE(quantreg_attestation$protected_exdqlm_checkout_used)
  )
  rqr_confirm_authorized(
    contract, mode, expected_commit, authorization,
    observed = observed_authorization
  )
  tasks <- utils::read.csv(
    task_path, stringsAsFactors = FALSE, check.names = FALSE
  )
  tasks <- rqr_confirm_validate_task_subset(tasks, contract)
  canonical_tasks <- rqr_confirm_replication_plan(
    contract, planning = "maximum"
  )
  canonical_task_file <- utils::read.csv(
    canonical_task_path, stringsAsFactors = FALSE, check.names = FALSE
  )
  rownames(canonical_task_file) <- rownames(canonical_tasks) <- NULL
  if (!identical(canonical_task_file, canonical_tasks)) {
    stop(
      "The authorization-bound task plan is not the complete canonical plan.",
      call. = FALSE
    )
  }
  if (mode == "sentinel-core" &&
      any(!tasks$embedded_sentinel)) {
    stop("sentinel-core accepts embedded-sentinel replications only.",
         call. = FALSE)
  }
  ledger <- utils::read.csv(
    ledger_path, stringsAsFactors = FALSE, check.names = FALSE
  )
  ledger <- rqr_confirm_validate_seed_ledger(
    ledger, contract, planning = "maximum", require_complete = TRUE
  )
  provenance_control <- list(
    repo_root = repo_root,
    expected_git_commit = expected_commit,
    primary_runtime_attestation = readRDS(primary_attestation_path)
  )
  run_root <- file.path(staging, "replications")
  dir.create(run_root, recursive = TRUE)
  run_status <- transform(
    tasks,
    status = "planned", started_at = NA_character_,
    ended_at = NA_character_, message = ""
  )
  failure_rows <- list()
  failure_index <- 0L
  all_result_rows <- all_diagnostic_rows <- list()
  result_index <- diagnostic_index <- 0L

  atomic_rds <- function(value, path) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    temporary <- tempfile(
      paste0(".", basename(path), "-"), tmpdir = dirname(path)
    )
    on.exit(unlink(temporary, force = TRUE), add = TRUE)
    saveRDS(value, temporary, compress = "xz")
    readRDS(temporary)
    rqr_confirm_sha256(temporary)
    if (file.exists(path) || !file.rename(temporary, path)) {
      stop("Atomic RDS publication failed.", call. = FALSE)
    }
    invisible(path)
  }
  sentinel_map <- rqr_confirm_sentinel_map(
    contract, planning = "maximum"
  )
  blocked_DGPs <- character()

  for (task_index in seq_len(nrow(tasks))) {
    task <- tasks[task_index, , drop = FALSE]
    if (task$DGP[[1L]] %in% blocked_DGPs) {
      run_status$status[[task_index]] <- "not_run_cell_stop"
      run_status$message[[task_index]] <-
        "scenario_coverage_cell_previously_stopped"
      next
    }
    run_status$status[[task_index]] <- "running"
    run_status$started_at[[task_index]] <- format(
      Sys.time(), tz = "UTC", usetz = TRUE
    )
    generation_failure <- NULL
    generated <- tryCatch(
      rqr_confirm_generate_dgp(
        contract, task$DGP[[1L]], task$replication[[1L]], ledger
      ),
      error = function(error) {
        generation_failure <<- error
        NULL
      }
    )
    if (!is.null(generation_failure)) {
      failure_index <- failure_index + 1L
      failure_rows[[failure_index]] <- data.frame(
        run_id = basename(output_dir),
        cell_id = paste0(task$DGP[[1L]], "__DATA"),
        replication = task$replication[[1L]],
        method = "DGP",
        failure_class = class(generation_failure)[[1L]],
        message_digest = digest::digest(
          conditionMessage(generation_failure),
          algo = "sha256", serialize = FALSE
        ),
        intention_to_run_denominator = TRUE,
        retry_count = 0L,
        stringsAsFactors = FALSE
      )
      run_status$status[[task_index]] <- "infrastructure_invalid"
      run_status$message[[task_index]] <-
        failure_rows[[failure_index]]$message_digest
      run_status$ended_at[[task_index]] <- format(
        Sys.time(), tz = "UTC", usetz = TRUE
      )
      if (task_index < nrow(run_status)) {
        remaining <- seq.int(task_index + 1L, nrow(run_status))
        run_status$status[remaining] <- "not_run_global_stop"
        run_status$message[remaining] <-
          failure_rows[[failure_index]]$message_digest
      }
      stage_status <- "failed_global_stop"
      stage_exit_status <- 1L
      break
    }
    methods <- strsplit(task$methods[[1L]], "|", fixed = TRUE)[[1L]]
    replication_results <- list()
    replication_diagnostics <- list()
    replication_failures <- list()
    replication_result_index <- replication_diagnostic_index <- 0L
    replication_failure_index <- 0L
    sentinel_objects <- list()
    cell_stop <- FALSE
    global_stop <- FALSE
    cell_stop_message <- ""
    for (method in methods) {
      method_cell_id <- contract$incidence$cell_id[
        contract$incidence$DGP == task$DGP[[1L]] &
          contract$incidence$method == method
      ]
      if (length(method_cell_id) != 1L) {
        stop("A replication method does not map to one incidence cell.",
             call. = FALSE)
      }
      mcmc_method <- rqr_confirm_method_mcmc_chains(method) > 0L
      method_is_sentinel <- mcmc_method && any(
        sentinel_map$cell_id == method_cell_id &
          sentinel_map$replication == task$replication[[1L]]
      )
      chains <- if (method_is_sentinel) {
        4L
      } else {
        1L
      }
      chain_results <- vector("list", chains)
      failure <- NULL
      started <- proc.time()[["elapsed"]]
      for (chain in seq_len(chains)) {
        profile_name <- rqr_confirm_initialization_profile_name(
          method_is_sentinel, chain
        )
        chain_results[[chain]] <- tryCatch(
          rqr_confirm_execute_method(
            contract, generated, method, chain, ledger,
            provenance_control = provenance_control,
            exdqlm_attestation_path = exdqlm_path,
            quantreg_attestation_path = quantreg_path,
            profile_name = profile_name
          ),
          error = function(error) {
            failure <<- error
            NULL
          }
        )
        if (!is.null(failure)) break
      }
      elapsed <- proc.time()[["elapsed"]] - started
      if (!is.null(failure)) {
        replication_failure_index <- replication_failure_index + 1L
        failure_index <- failure_index + 1L
        failure_row <- data.frame(
          run_id = basename(output_dir),
          cell_id = method_cell_id,
          replication = task$replication[[1L]],
          method = method,
          failure_class = class(failure)[[1L]],
          message_digest = digest::digest(
            conditionMessage(failure), algo = "sha256",
            serialize = FALSE
          ),
          intention_to_run_denominator = TRUE,
          retry_count = 0L,
          stringsAsFactors = FALSE
        )
        replication_failures[[replication_failure_index]] <- failure_row
        failure_rows[[failure_index]] <- failure_row
        replication_result_index <- replication_result_index + 1L
        replication_results[[replication_result_index]] <- data.frame(
          run_id = basename(output_dir),
          cell_id = method_cell_id,
          replication = task$replication[[1L]],
          method = method, status = "failed",
          failure_class = class(failure)[[1L]],
          training_loss = NA_real_, heldout_rqr_loss = NA_real_,
          aggregate_coverage = NA_real_, mean_width = NA_real_,
          central_interval_score = NA_real_,
          future_mean_lower = NA_real_,
          future_mean_upper = NA_real_,
          future_mean_midpoint = NA_real_,
          endpoint_rmse_lower = NA_real_,
          endpoint_rmse_upper = NA_real_,
          cross_target_distance = NA_real_,
          realized_root_rmse = NA_real_,
          training_response_sd = stats::sd(generated$training_y),
          mean_oracle_width = mean(
            generated$oracle_conditional_mean_root[, "upper"] -
              generated$oracle_conditional_mean_root[, "lower"]
          ),
          elapsed_seconds = elapsed,
          peak_RSS_bytes = NA_real_,
          stringsAsFactors = FALSE
        )
        for (horizon in seq_len(generated$H)) {
          replication_results[[replication_result_index]][[
            sprintf("coverage_h%02d", horizon)
          ]] <- NA_real_
        }
        failure_message <- conditionMessage(failure)
        global_stop <- grepl(
          "provenance|runtime|source|seed|artifact|exact joint target",
          failure_message, ignore.case = TRUE
        )
        nonfinite_cell_stop <- grepl(
          "nonfinite primary outputs|unordered interval",
          failure_message, ignore.case = TRUE
        )
        if (isTRUE(global_stop)) {
          cell_stop <- TRUE
          cell_stop_message <- paste(
            "systemic fit failure:", method,
            digest::digest(
              failure_message, algo = "sha256", serialize = FALSE
            )
          )
          stage_status <- "failed_global_stop"
          stage_exit_status <- 1L
          break
        }
        if (isTRUE(nonfinite_cell_stop)) {
          cell_stop <- TRUE
          cell_stop_message <- paste(
            "nonfinite cell failure:", method,
            digest::digest(
              failure_message, algo = "sha256", serialize = FALSE
            )
          )
          stage_status <- "failed_cell_stop"
          stage_exit_status <- 1L
          break
        }
        if (identical(stage_status, "passed")) {
          stage_status <- "completed_with_fit_failures"
        }
        next
      }
      combined <- chain_results[[1L]]
      if (chains > 1L) {
        combined$training_lower <- Reduce(
          `+`, lapply(chain_results, `[[`, "training_lower")
        ) / chains
        combined$training_upper <- Reduce(
          `+`, lapply(chain_results, `[[`, "training_upper")
        ) / chains
        combined$future_lower <- Reduce(
          `+`, lapply(chain_results, `[[`, "future_lower")
        ) / chains
        combined$future_upper <- Reduce(
          `+`, lapply(chain_results, `[[`, "future_upper")
        ) / chains
      }
      metrics <- rqr_confirm_replication_metrics(generated, combined)
      replication_result_index <- replication_result_index + 1L
      replication_results[[replication_result_index]] <- data.frame(
        run_id = basename(output_dir),
        cell_id = method_cell_id,
        replication = task$replication[[1L]],
        method = method, status = "completed", failure_class = "",
        training_loss = metrics$training_loss,
        heldout_rqr_loss = metrics$heldout_rqr_loss,
        aggregate_coverage = metrics$aggregate_coverage,
        mean_width = metrics$mean_width,
        central_interval_score = metrics$central_interval_score,
        future_mean_lower = metrics$future_mean_lower,
        future_mean_upper = metrics$future_mean_upper,
        future_mean_midpoint = metrics$future_mean_midpoint,
        endpoint_rmse_lower = metrics$endpoint_rmse_lower,
        endpoint_rmse_upper = metrics$endpoint_rmse_upper,
        cross_target_distance = metrics$cross_target_distance,
        realized_root_rmse = metrics$realized_root_rmse,
        training_response_sd = stats::sd(generated$training_y),
        mean_oracle_width = mean(
          generated$oracle_conditional_mean_root[, "upper"] -
            generated$oracle_conditional_mean_root[, "lower"]
        ),
        elapsed_seconds = elapsed, peak_RSS_bytes = NA_real_,
        stringsAsFactors = FALSE
      )
      for (horizon in seq_len(generated$H)) {
        replication_results[[replication_result_index]][[
          sprintf("coverage_h%02d", horizon)
        ]] <- metrics[[sprintf("coverage_h%02d", horizon)]]
      }
      if (mcmc_method) {
        scalar_chains <- lapply(chain_results, function(value) {
          rqr_confirm_scalar_draws(
            value, generated, contract, method
          )
        })
        diagnostics <- rqr_confirm_chain_diagnostics(
          scalar_chains, contract, sentinel = method_is_sentinel,
          method = method, generated = generated
        )
        diagnostics$DGP <- task$DGP[[1L]]
        diagnostics$replication <- task$replication[[1L]]
        diagnostics$method <- method
        diagnostics$sentinel <- method_is_sentinel
        replication_diagnostic_index <-
          replication_diagnostic_index + 1L
        replication_diagnostics[[replication_diagnostic_index]] <-
          diagnostics
        if (!all(diagnostics$pass)) {
          diagnostic_message <- sprintf(
            "Frozen MCMC diagnostics failed for %s/%s/rep%d.",
            task$DGP[[1L]], method, task$replication[[1L]]
          )
          replication_result_index <-
            length(replication_results)
          replication_results[[replication_result_index]]$status <-
            "diagnostic_failed"
          replication_results[[replication_result_index]]$failure_class <-
            "mcmc_diagnostic_failure"
          replication_failure_index <- replication_failure_index + 1L
          failure_index <- failure_index + 1L
          failure_row <- data.frame(
            run_id = basename(output_dir),
            cell_id = method_cell_id,
            replication = task$replication[[1L]],
            method = method,
            failure_class = "mcmc_diagnostic_failure",
            message_digest = digest::digest(
              diagnostic_message, algo = "sha256", serialize = FALSE
            ),
            intention_to_run_denominator = TRUE,
            retry_count = 0L,
            stringsAsFactors = FALSE
          )
          replication_failures[[replication_failure_index]] <-
            failure_row
          failure_rows[[failure_index]] <- failure_row
          if (method_is_sentinel) {
            cell_stop <- TRUE
            cell_stop_message <- diagnostic_message
            stage_status <- "failed_cell_stop"
            stage_exit_status <- 1L
            break
          }
          if (identical(stage_status, "passed")) {
            stage_status <- "completed_with_fit_failures"
          }
        }
      }
      if (method_is_sentinel) {
        sentinel_objects[[method]] <- lapply(
          chain_results, function(value) {
            value[c("fit", "fits", "forecast", "forecasts", "diagnostics")]
          }
        )
      }
    }
    replication_directory <- file.path(
      run_root, task$DGP[[1L]],
      sprintf("rep%04d", task$replication[[1L]])
    )
    if (dir.exists(replication_directory)) {
      stop("A replication artifact directory already exists.",
           call. = FALSE)
    }
    scenario_root <- file.path(run_root, task$DGP[[1L]])
    dir.create(scenario_root, recursive = TRUE, showWarnings = FALSE)
    replication_staging <- tempfile(
      sprintf(".rep%04d-", task$replication[[1L]]),
      tmpdir = scenario_root
    )
    dir.create(replication_staging, recursive = TRUE)
    result_table <- do.call(rbind, replication_results)
    result_schema <- rqr_confirm_artifact_schemas()$replication_results
    if (!setequal(names(result_table), result_schema)) {
      stop("A replication result violates its frozen compact schema.",
           call. = FALSE)
    }
    result_table <- result_table[result_schema]
    rqr_confirm_atomic_write_csv(
      result_table,
      file.path(replication_staging, "replication_results.csv")
    )
    if (length(replication_diagnostics)) {
      diagnostic_table <- do.call(rbind, replication_diagnostics)
      diagnostic_schema <- rqr_confirm_artifact_schemas()$fit_diagnostics
      if (!setequal(names(diagnostic_table), diagnostic_schema)) {
        stop("Fit diagnostics violate their frozen compact schema.",
             call. = FALSE)
      }
      diagnostic_table <- diagnostic_table[diagnostic_schema]
      rqr_confirm_atomic_write_csv(
        diagnostic_table,
        file.path(replication_staging, "fit_diagnostics.csv")
      )
    }
    if (length(replication_failures)) {
      failure_table <- do.call(rbind, replication_failures)
      failure_schema <- rqr_confirm_artifact_schemas()$failure_ledger
      if (!identical(names(failure_table), failure_schema)) {
        stop("A failure ledger violates its frozen compact schema.",
             call. = FALSE)
      }
      rqr_confirm_atomic_write_csv(
        failure_table,
        file.path(replication_staging, "failure_log.csv")
      )
    }
    if (length(sentinel_objects)) {
      atomic_rds(
        sentinel_objects,
        file.path(
          replication_staging, "sentinel_chains_ignored.rds"
        )
      )
    }
    replication_manifest <- list(
      schema_version = "rqrgibbs_dlm_replication/1.0.0",
      source_commit = source_commit,
      config_digest = contract_digests$config_sha256,
      incidence_digest = contract_digests$incidence_sha256,
      seed_ledger_digest = rqr_confirm_sha256(ledger_path),
      runtime_digest = primary_binding$runtime_tree_digest,
      DGP = task$DGP[[1L]],
      replication = task$replication[[1L]],
      embedded_sentinel = task$embedded_sentinel[[1L]],
      no_retry = TRUE,
      generalized_bayes = TRUE,
      response_likelihood = FALSE,
      response_prediction_contract = FALSE
    )
    if (!identical(
        names(replication_manifest),
        rqr_confirm_artifact_schemas()$replication_manifest
      )) {
      stop("A replication manifest violates its frozen schema.",
           call. = FALSE)
    }
    rqr_confirm_atomic_write_json(
      replication_manifest,
      file.path(replication_staging, "replication_manifest.json")
    )
    artifact_manifest <- rqr_confirm_recursive_manifest(
      replication_staging
    )
    rqr_confirm_atomic_write_csv(
      artifact_manifest,
      file.path(
        replication_staging, "replication_artifact_hashes.csv"
      )
    )
    if (!file.rename(replication_staging, replication_directory)) {
      unlink(replication_staging, recursive = TRUE, force = TRUE)
      stop("Atomic replication-directory publication failed.",
           call. = FALSE)
    }
    result_index <- result_index + 1L
    all_result_rows[[result_index]] <- result_table
    if (length(replication_diagnostics)) {
      diagnostic_index <- diagnostic_index + 1L
      all_diagnostic_rows[[diagnostic_index]] <-
        do.call(rbind, replication_diagnostics)
    }
    run_status$status[[task_index]] <- if (cell_stop) {
      if (global_stop) "global_stop_failure" else "cell_stop_failure"
    } else if (length(replication_failures)) {
      "completed_with_fit_failure"
    } else {
      "completed"
    }
    run_status$message[[task_index]] <- cell_stop_message
    run_status$ended_at[[task_index]] <- format(
      Sys.time(), tz = "UTC", usetz = TRUE
    )
    if (cell_stop) {
      if (global_stop && task_index < nrow(run_status)) {
        remaining <- seq.int(task_index + 1L, nrow(run_status))
        run_status$status[remaining] <- "not_run_global_stop"
        run_status$message[remaining] <-
          digest::digest(
            cell_stop_message, algo = "sha256", serialize = FALSE
          )
        break
      }
      blocked_DGPs <- unique(c(blocked_DGPs, task$DGP[[1L]]))
      if (task_index < nrow(run_status)) {
        remaining <- seq.int(task_index + 1L, nrow(run_status))
        same_cell <- remaining[
          tasks$DGP[remaining] == task$DGP[[1L]]
        ]
        run_status$status[same_cell] <- "not_run_cell_stop"
        run_status$message[same_cell] <-
          digest::digest(
            cell_stop_message, algo = "sha256", serialize = FALSE
          )
      }
    }
  }
  run_status$message <- as.character(run_status$message)
  write_csv(run_status, "run_status.csv")
  combined_result_rows <- if (length(all_result_rows)) {
    do.call(rbind, all_result_rows)
  } else {
    value <- as.data.frame(
      matrix(
        nrow = 0L,
        ncol = length(rqr_confirm_artifact_schemas()$replication_results)
      )
    )
    names(value) <- rqr_confirm_artifact_schemas()$replication_results
    value
  }
  write_csv(combined_result_rows, "replication_results.csv")
  if (length(all_diagnostic_rows)) {
    write_csv(
      do.call(rbind, all_diagnostic_rows), "fit_diagnostics.csv"
    )
  }
  if (length(failure_rows)) {
    write_csv(do.call(rbind, failure_rows), "failure_log.csv")
  } else {
    write_csv(
      data.frame(
        run_id = character(), cell_id = character(),
        replication = integer(), method = character(),
        failure_class = character(), message_digest = character(),
        intention_to_run_denominator = logical(),
        retry_count = integer()
      ),
      "failure_log.csv"
    )
  }
}

if (mode %in% c("collect", "audit")) {
  run_root <- Sys.getenv("RQR_CONFIRMATORY_RUN_ROOT", unset = "")
  collection_task_path <- Sys.getenv(
    "RQR_CONFIRMATORY_COLLECTION_TASK_PLAN", unset = ""
  )
  if (!nzchar(run_root) || !dir.exists(run_root) ||
      !nzchar(collection_task_path) ||
      !file.exists(collection_task_path)) {
    stop(
      sprintf(
        paste(
          "%s requires an existing RQR_CONFIRMATORY_RUN_ROOT and",
          "RQR_CONFIRMATORY_COLLECTION_TASK_PLAN."
        ),
        mode
      ),
      call. = FALSE
    )
  }
  manifest <- rqr_confirm_recursive_manifest(run_root)
  if (!nrow(manifest) || anyDuplicated(manifest$path)) {
    stop("The run-root artifact set is empty or duplicated.",
         call. = FALSE)
  }
  write_csv(manifest, paste0(mode, "_recursive_manifest.csv"))
  collection_tasks <- utils::read.csv(
    collection_task_path, stringsAsFactors = FALSE, check.names = FALSE
  )
  collected <- rqr_confirm_collect_outputs(
    run_root, collection_tasks, contract
  )
  write_csv(collected$statuses, "collected_run_status.csv")
  write_csv(collected$stages, "collected_worker_stages.csv")
  write_csv(
    data.frame(
      wave_directory = collected$wave_directories,
      artifact_bundle_verified = TRUE,
      stringsAsFactors = FALSE
    ),
    "collected_wave_inventory.csv"
  )
  write_csv(collected$replications, "collected_replication_integrity.csv")
  write_csv(collected$failures, "collected_failure_log.csv")
  if (nrow(collected$diagnostics)) {
    write_csv(collected$diagnostics, "collected_fit_diagnostics.csv")
  }
  results <- collected$results
  write_csv(results, "collected_replication_results.csv")
  if (isTRUE(collected$analysis_complete)) {
    summaries <- rqr_confirm_summarize_results(results, contract)
    contrasts <- rqr_confirm_paired_contrasts(results, contract)
    decisions <- if (nrow(summaries$precision)) {
      rqr_confirm_batch_decisions(
        results, summaries$precision, contrasts, contract
      )
    } else {
      data.frame()
    }
    write_csv(summaries$summary, "analysis_summary.csv")
    if (nrow(summaries$precision)) {
      write_csv(summaries$precision, "precision_summary.csv")
    }
    if (nrow(summaries$coverage_qualification)) {
      write_csv(
        summaries$coverage_qualification,
        "coverage_qualification.csv"
      )
    }
    if (nrow(contrasts)) {
      write_csv(contrasts, "paired_contrasts.csv")
    }
    if (nrow(decisions)) {
      if (!identical(
          names(decisions),
          rqr_confirm_artifact_schemas()$batch_decision
        )) {
        stop("Batch decisions violate their frozen schema.", call. = FALSE)
      }
      write_csv(decisions, "batch_decisions.csv")
    }
  } else {
    stage_status <- "integrity_verified_run_incomplete"
    stage_exit_status <- 1L
  }
  write_json(
    list(
      mode = mode, source_commit = source_commit,
      response_prediction_contract = FALSE,
      collection_task_plan_sha256 =
        rqr_confirm_sha256(collection_task_path),
      exact_task_set_verified = TRUE,
      analysis_complete = collected$analysis_complete,
      status = if (collected$analysis_complete) {
        "integrity_and_analysis_complete"
      } else {
        "integrity_verified_run_incomplete"
      }
    ),
    paste0(mode, "_manifest.json")
  )
}

run_manifest <- list(
  schema_version = "rqrgibbs_dlm_confirmatory_stage/1.0.0",
  mode = mode,
  config_schema_version = contract$config$schema_version,
  source_commit = source_commit,
  diagnostic_pilot_execution_authorized =
    contract$config$diagnostic_pilot_execution_authorized,
  confirmatory_execution_authorized =
    contract$config$confirmatory_execution_authorized,
  primary_runtime_binding = primary_binding,
  generalized_bayes = TRUE,
  response_likelihood = FALSE,
  response_prediction_contract = FALSE,
  status = stage_status
)
write_json(run_manifest, "run_manifest.json")
manifest <- rqr_confirm_recursive_manifest(staging)
rqr_confirm_atomic_write_csv(
  manifest, file.path(staging, "artifact_hashes.csv")
)
if (!file.rename(staging, output_dir)) {
  stop("Could not atomically publish the completed runner stage.",
       call. = FALSE)
}
published <- TRUE
cat("RQR-DLM confirmatory runner stage finalized.\n")
cat("  mode:", mode, "\n")
cat("  status:", stage_status, "\n")
cat("  output:", normalizePath(output_dir), "\n")
if (stage_exit_status != 0L) {
  quit(save = "no", status = stage_exit_status, runLast = FALSE)
}
