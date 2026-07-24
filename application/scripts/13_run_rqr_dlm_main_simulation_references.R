#!/usr/bin/env Rscript

# Fail-closed preliminary RQR-DLM simulation design and reference runner.
#
# Implemented modes construct contracts and tiny deterministic fixtures only.
# Diagnostic-pilot and confirmatory execution are deliberately unavailable.

args <- commandArgs(trailingOnly = TRUE)
if (!length(args) %in% c(1L, 2L)) {
  stop(
    paste(
      "Usage: 13_run_rqr_dlm_main_simulation_references.R",
      "<preflight|oracle-reference|tiny-end-to-end|",
      "diagnostic-pilot-preflight> [output_dir]"
    ),
    call. = FALSE
  )
}
mode <- args[[1L]]
implemented <- c(
  "preflight", "oracle-reference", "tiny-end-to-end",
  "diagnostic-pilot-preflight"
)
if (mode %in% c("diagnostic-pilot", "execute-confirmatory")) {
  stop(
    sprintf(
      "%s is not implemented or authorized by the preliminary contract.",
      mode
    ),
    call. = FALSE
  )
}
if (!mode %in% implemented) {
  stop("Unknown preliminary main-simulation mode.", call. = FALSE)
}

repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(repo_root, "application", "DESCRIPTION"))) {
  stop("Run this script from the RQR-GIBBS repository root.", call. = FALSE)
}
if (!requireNamespace("rqrgibbs", quietly = TRUE)) {
  stop("Install the current rqrgibbs package before running references.",
       call. = FALSE)
}
suppressPackageStartupMessages(library(rqrgibbs))
sys.source(
  file.path(
    repo_root, "application", "scripts", "lib",
    "rqr_dlm_main_simulation.R"
  ),
  envir = environment()
)
contract <- rqr_main_read_contract(repo_root)
rqr_main_validate_contract(contract)
expected_primary_commit <- Sys.getenv(
  "RQR_EXPECTED_PRIMARY_COMMIT", unset = ""
)
primary_attestation_path <- Sys.getenv(
  "RQR_PRIMARY_RUNTIME_ATTESTATION", unset = ""
)
primary_binding <- NULL
if (nzchar(expected_primary_commit)) {
  if (!nzchar(primary_attestation_path)) {
    stop(
      "RQR_PRIMARY_RUNTIME_ATTESTATION is required with an expected commit.",
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
  primary_binding <- rqr_main_primary_runtime_binding(
    repo_root, expected_primary_commit, primary_attestation_path
  )
}
if (identical(mode, "diagnostic-pilot-preflight") &&
    is.null(primary_binding)) {
  stop(
    "Diagnostic-pilot preflight requires an exact isolated primary runtime.",
    call. = FALSE
  )
}

output_dir <- if (length(args) == 2L) {
  normalizePath(args[[2L]], winslash = "/", mustWork = FALSE)
} else {
  file.path(
    repo_root, "application", "outputs",
    "rqr_dlm_main_simulation_preliminary_20260724", mode
  )
}
if (file.exists(output_dir) || dir.exists(output_dir)) {
  stop("The requested output directory already exists.", call. = FALSE)
}
parent <- dirname(output_dir)
dir.create(parent, recursive = TRUE, showWarnings = FALSE)
staging <- tempfile(paste0(".", basename(output_dir), "-"), tmpdir = parent)
dir.create(staging)
published <- FALSE
on.exit({
  if (!published) unlink(staging, recursive = TRUE, force = TRUE)
}, add = TRUE)

write_stage_csv <- function(value, name) {
  rqr_main_atomic_write_csv(value, file.path(staging, name))
}

if (identical(mode, "preflight")) {
  seed_ledger <- rqr_main_seed_ledger(contract)
  gates <- data.frame(
    gate = c(
      "contract_schema", "execution_flags_false", "scenario_IDs",
      "method_IDs", "positive_scale_and_separation",
      "matched_trend_seasonal_pair", "common_evolution_ablation",
      "pinned_exdqlm_source", "independent_seed_streams"
    ),
    pass = TRUE,
    stringsAsFactors = FALSE
  )
  write_stage_csv(gates, "preflight_gates.csv")
  write_stage_csv(contract$scenarios, "scenario_contract.csv")
  write_stage_csv(contract$methods, "method_contract.csv")
  write_stage_csv(seed_ledger, "diagnostic_seed_ledger.csv")
}

if (identical(mode, "oracle-reference")) {
  families <- unique(contract$scenarios$error_family)
  references <- do.call(rbind, lapply(families, function(error_family) {
    oracle_spec <- rqr_main_oracle_family(error_family)
    do.call(rbind, lapply(
      contract$config$design$coverage_levels,
      function(coverage_level) {
        certificate <- rqr_oracle_certificate(
          oracle_spec$family, coverage_level,
          params = oracle_spec$params, grid_size = 201L
        )
        data.frame(
          schema_version = certificate$schema_version,
          error_family = error_family,
          oracle_family = certificate$family,
          coverage_level = coverage_level,
          lower_root = certificate$lower_root,
          upper_root = certificate$upper_root,
          objective = certificate$profile_objective,
          unrestricted_objective =
            certificate$unrestricted_objective,
          global_objective_gap = certificate$global_objective_gap,
          coverage_residual = certificate$coverage_residual,
          moment_residual = certificate$moment_residual,
          local_profile_curvature =
            certificate$local_profile_curvature,
          estimated_quadrature_error =
            certificate$estimated_quadrature_error,
          quadrature_error_is_rigorous_bound =
            certificate$quadrature_error_is_rigorous_bound,
          endpoint_separation = certificate$endpoint_separation,
          unique_minimizer = certificate$unique_minimizer,
          distribution_digest = certificate$distribution_digest,
          solver_digest = certificate$solver_digest,
          stringsAsFactors = FALSE
        )
      }
    ))
  }))
  numeric_reference_fields <- c(
        "lower_root", "upper_root", "objective",
        "unrestricted_objective", "global_objective_gap",
        "coverage_residual", "moment_residual",
        "local_profile_curvature", "estimated_quadrature_error",
        "endpoint_separation"
      )
  if (any(!is.finite(as.matrix(
        references[numeric_reference_fields]
      ))) ||
      any(references$endpoint_separation <= 0) ||
      any(abs(references$coverage_residual) > 1e-6) ||
      any(abs(references$moment_residual) > 1e-5) ||
      any(references$global_objective_gap > 1e-5)) {
    stop("One or more oracle-reference gates failed.", call. = FALSE)
  }
  write_stage_csv(references, "oracle_references.csv")
}

tiny_fit_rows <- function() {
  do.call(rbind, lapply(1:2, function(replication) {
    generated <- rqr_main_generate_dgp(
      contract, "local_level_skewed", replication, 0.80,
      n_time = 24L, horizon = 4L
    )
    model <- rqr_polytrend(1L, m0 = 0, C0 = 4)
    fit_seed <- rqr_main_seed(
      contract$config, "method", "tiny", replication
    )
    fit <- rqr_dlm_fit(
      generated$training_y,
      model = model,
      coverage_level = 0.80,
      evolution_mode = "fixed_W",
      W = matrix(0.02, 1L, 1L),
      learning_rate = 1,
      learning_rate_mode = "fixed_rate",
      numerical_policy = "fail",
      mcmc_control = list(
        n_burn = 20L, n_mcmc = 40L, thin = 1L,
        seed = fit_seed, backend = "cpp",
        store_state_draws = FALSE, store_latent_draws = FALSE,
        verbose = FALSE
      )
    )
    interval <- predict_interval(fit)
    forecast <- rqr_forecast_roots(
      fit,
      FF_future = matrix(1, 1L, generated$H),
      GG_future = matrix(1, 1L, 1L),
      W_future = matrix(0.02, 1L, 1L),
      nd = 40L,
      seed = rqr_main_seed(
        contract$config, "forecast", "tiny_fit", replication
      ),
      numerical_policy = "fail"
    )
    future_lower <- rowMeans(forecast$lower_draws)
    future_upper <- rowMeans(forecast$upper_draws)
    data.frame(
      schema_version =
        "rqrgibbs_dlm_main_simulation_tiny_result/1.0.0",
      replication = replication,
      data_seed = generated$seeds[["state"]],
      error_seed = generated$seeds[["error"]],
      method_seed = fit_seed,
      training_loss = sum(rqr_check_loss(
        rqr_residual_product(
          generated$training_y,
          interval$lower_mean, interval$upper_mean
        ),
        0.80
      )),
      mean_training_lower = mean(interval$lower_mean),
      mean_training_upper = mean(interval$upper_mean),
      mean_future_lower = mean(future_lower),
      mean_future_upper = mean(future_upper),
      future_coverage = mean(
        generated$future_y > future_lower &
          generated$future_y < future_upper
      ),
      numerical_repairs = fit$model_spec$numerical_repair_count,
      forecast_repairs = nrow(forecast$diagnostics$repair_records),
      exact_joint_target = fit$model_spec$exact_joint_target,
      generalized_bayes = fit$model_spec$generalized_bayes,
      response_likelihood = fit$model_spec$response_likelihood,
      response_prediction_contract = FALSE,
      stringsAsFactors = FALSE
    )
  }))
}

if (identical(mode, "tiny-end-to-end")) {
  first <- tiny_fit_rows()
  second <- tiny_fit_rows()
  if (!identical(first, second) ||
      any(first$numerical_repairs != 0L) ||
      any(first$forecast_repairs != 0L) ||
      !all(first$exact_joint_target) ||
      !all(first$generalized_bayes) ||
      any(first$response_likelihood) ||
      any(first$response_prediction_contract)) {
    stop("The two-replication byte-reproduction fixture failed.",
         call. = FALSE)
  }
  first_path <- file.path(staging, "tiny_results.csv")
  second_path <- file.path(staging, ".tiny_results_reproduction.csv")
  utils::write.csv(first, first_path, row.names = FALSE, quote = TRUE)
  utils::write.csv(second, second_path, row.names = FALSE, quote = TRUE)
  first_hash <- digest::digest(
    file = first_path, algo = "sha256", serialize = FALSE
  )
  second_hash <- digest::digest(
    file = second_path, algo = "sha256", serialize = FALSE
  )
  unlink(second_path, force = TRUE)
  if (!identical(first_hash, second_hash)) {
    stop("The repeated tiny compact output is not byte identical.",
         call. = FALSE)
  }
  write_stage_csv(
    data.frame(
      artifact = "tiny_results.csv",
      first_sha256 = first_hash,
      repeated_sha256 = second_hash,
      byte_identical = TRUE,
      stringsAsFactors = FALSE
    ),
    "byte_reproduction.csv"
  )
}

if (identical(mode, "diagnostic-pilot-preflight")) {
  exdqlm_attestation_path <- Sys.getenv(
    "RQR_EXDQLM_CRAN_ATTESTATION",
    unset = file.path(
      repo_root, "application", "cache", "exdqlm_cran_1.1.0",
      "exdqlm_1.1.0_runtime_attestation.json"
    )
  )
  quantreg_attestation_path <- Sys.getenv(
    "RQR_QUANTREG_CRAN_ATTESTATION",
    unset = file.path(
      repo_root, "application", "cache", "quantreg_cran_6.1",
      "quantreg_6.1_runtime_attestation.json"
    )
  )
  if (!file.exists(exdqlm_attestation_path)) {
    stop("The isolated CRAN exdqlm attestation is missing.", call. = FALSE)
  }
  if (!file.exists(quantreg_attestation_path)) {
    stop("The isolated CRAN quantreg attestation is missing.", call. = FALSE)
  }
  exdqlm_attestation <- jsonlite::read_json(
    exdqlm_attestation_path, simplifyVector = TRUE
  )
  quantreg_attestation <- jsonlite::read_json(
    quantreg_attestation_path, simplifyVector = TRUE
  )
  if (!identical(
        exdqlm_attestation$schema_version,
        "rqrgibbs_external_cran_runtime/1.0.0"
      ) ||
      !identical(
        exdqlm_attestation$source_package_sha256,
        contract$config$methods$external_source$sha256
      ) ||
      !identical(exdqlm_attestation$install_exit_status, 0L) ||
      !identical(exdqlm_attestation$install_input_count, 1L) ||
      isTRUE(exdqlm_attestation$protected_exdqlm_checkout_used) ||
      !dir.exists(exdqlm_attestation$runtime_path) ||
      !identical(
        quantreg_attestation$schema_version,
        "rqrgibbs_external_cran_runtime/1.0.0"
      ) ||
      !identical(
        quantreg_attestation$source_package_sha256,
        contract$config$methods$static_external_source$sha256
      ) ||
      !identical(quantreg_attestation$install_exit_status, 0L) ||
      !identical(quantreg_attestation$install_input_count, 1L) ||
      !dir.exists(quantreg_attestation$runtime_path)) {
    stop("An isolated comparator attestation failed.", call. = FALSE)
  }
  sys.source(
    file.path(
      repo_root, "application", "scripts", "lib",
      "isolated_runtime_lineage.R"
    ),
    envir = environment()
  )
  if (!identical(
        rqr_directory_digest(exdqlm_attestation$runtime_path),
        exdqlm_attestation$runtime_tree_digest
      ) ||
      !identical(
        rqr_directory_digest(quantreg_attestation$runtime_path),
        quantreg_attestation$runtime_tree_digest
      )) {
    stop("An isolated comparator runtime digest changed.", call. = FALSE)
  }
  exdqlm_adapter <- rqr_main_validate_exdqlm_adapter(exdqlm_attestation)
  quantreg_adapter <- rqr_main_validate_quantreg_adapter(
    quantreg_attestation
  )
  mcmc_methods <- contract$methods[
    grepl("MCMC", contract$methods$engine, fixed = TRUE) &
      contract$methods$competitive_status != "noncompetitive_oracle",
    ,
    drop = FALSE
  ]
  plan <- expand.grid(
    dgp_id = contract$config$design$primary_dgp_ids,
    coverage_level = contract$config$design$coverage_levels,
    method_id = mcmc_methods$method_id,
    replication = seq_len(
      contract$config$monte_carlo$
        diagnostic_pilot_replications_per_mechanism_coverage_method
    ),
    chain = seq_len(contract$config$mcmc$diagnostic_pilot_chains),
    stringsAsFactors = FALSE
  )
  plan$seed <- vapply(seq_len(nrow(plan)), function(index) {
    rqr_main_seed(
      contract$config, "method",
      plan$dgp_id[[index]], plan$coverage_level[[index]],
      plan$method_id[[index]], plan$replication[[index]],
      plan$chain[[index]]
    )
  }, integer(1L))
  plan$execution_authorized <- FALSE
  gates <- data.frame(
    gate = c(
      "contract_valid", "oracle_reference_required",
      "tiny_end_to_end_required", "external_comparators_attested",
      "diagnostic_pilot_authorization_false",
      "confirmatory_authorization_false"
    ),
    pass = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  write_stage_csv(plan, "diagnostic_pilot_plan.csv")
  write_stage_csv(gates, "diagnostic_pilot_preflight_gates.csv")
  write_stage_csv(
    rbind(
      data.frame(
        package = exdqlm_attestation$package,
        version = exdqlm_attestation$version,
        source_package_sha256 =
          exdqlm_attestation$source_package_sha256,
        runtime_tree_digest = exdqlm_attestation$runtime_tree_digest,
        protected_checkout_used =
          exdqlm_attestation$protected_exdqlm_checkout_used,
        stringsAsFactors = FALSE
      ),
      data.frame(
        package = quantreg_attestation$package,
        version = quantreg_attestation$version,
        source_package_sha256 =
          quantreg_attestation$source_package_sha256,
        runtime_tree_digest = quantreg_attestation$runtime_tree_digest,
        protected_checkout_used = FALSE,
        stringsAsFactors = FALSE
      )
    ),
    "comparator_runtime.csv"
  )
  write_stage_csv(
    rbind(
      data.frame(
        package = exdqlm_adapter$package,
        version = exdqlm_adapter$version,
        fitting_function = "exdqlmMCMC",
        method = "dqlm.ind=TRUE;init.from.vb=FALSE",
        raw_quantiles_retained =
          exdqlm_adapter$raw_quantile_forecasts_retained,
        ordering_applied_only_for_interval_scoring = TRUE,
        response_predictive_draws_used =
          exdqlm_adapter$response_predictive_draws_used,
        adapter_pass = exdqlm_adapter$pass,
        stringsAsFactors = FALSE
      ),
      quantreg_adapter
    ),
    "comparator_adapter.csv"
  )
}

run_manifest <- list(
  schema_version =
    "rqrgibbs_dlm_main_simulation_reference_run/1.0.0",
  mode = mode,
  config_schema_version = contract$config$schema_version,
  diagnostic_pilot_execution_authorized =
    contract$config$diagnostic_pilot_execution_authorized,
  confirmatory_execution_authorized =
    contract$config$confirmatory_execution_authorized,
  primary_runtime_binding_verified = !is.null(primary_binding),
  primary_runtime_binding = primary_binding,
  generalized_bayes = TRUE,
  response_likelihood = FALSE,
  response_prediction_contract = FALSE,
  status = "passed"
)
jsonlite::write_json(
  run_manifest, file.path(staging, "run_manifest.json"),
  auto_unbox = TRUE, pretty = TRUE
)
manifest <- rqr_main_recursive_manifest(staging)
rqr_main_atomic_write_csv(
  manifest, file.path(staging, "artifact_hashes.csv")
)
if (!file.rename(staging, output_dir)) {
  stop("Could not atomically publish the completed reference stage.",
       call. = FALSE)
}
published <- TRUE
cat("Preliminary main-simulation stage passed.\n")
cat("  mode:", mode, "\n")
cat("  output:", normalizePath(output_dir), "\n")
