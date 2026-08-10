#!/usr/bin/env Rscript

arguments <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_path <- sub(file_arg, "", arguments[startsWith(arguments, file_arg)][1L])
if (!length(script_path) || is.na(script_path)) {
  script_path <- "application/scripts/60_run_oracle_mean_tilt_validation.R"
}
script_path <- normalizePath(script_path, winslash = "/", mustWork = TRUE)
script_dir <- dirname(script_path)
repo_root <- normalizePath(file.path(script_dir, "..", ".."), mustWork = TRUE)
setwd(repo_root)
source(file.path(script_dir, "60_oracle_mean_tilt_validation_utils.R"))

trailing <- commandArgs(trailingOnly = TRUE)
arg_value <- function(prefix, default = NULL) {
  hit <- trailing[startsWith(trailing, prefix)]
  if (length(hit)) sub(prefix, "", hit[[1L]], fixed = TRUE) else default
}
mode <- tolower(arg_value("--mode=", "preflight"))
allowed <- c(
  "preflight", "reference-only", "benchmark", "sentinel", "execute-wave",
  "precision-check", "verify-closeout", "health-check-read-only"
)
if (!mode %in% allowed) {
  omtv_stop("Unsupported validation mode: ", mode)
}
if (!requireNamespace("rqrgibbs", quietly = TRUE) ||
    !requireNamespace("jsonlite", quietly = TRUE) ||
    !requireNamespace("digest", quietly = TRUE)) {
  omtv_stop("rqrgibbs, jsonlite, and digest are required.")
}
if (mode %in% c("benchmark", "sentinel", "execute-wave") &&
    (!requireNamespace("posterior", quietly = TRUE) ||
     utils::packageVersion("posterior") < "1.7.0")) {
  omtv_stop(
    "Promotion fit modes require posterior >= 1.7.0 for maintained diagnostics."
  )
}

config_path <- normalizePath(arg_value(
  "--config=",
  file.path("application", "config", "oracle_mean_tilt_validation_v1.json")
), winslash = "/", mustWork = TRUE)
config <- omtv_read_config(config_path)
omtv_validate_config(config)
config_sha256 <- digest::digest(file = config_path, algo = "sha256")

git_output <- function(args) {
  result <- suppressWarnings(system2(
    "git", c("-c", "core.hooksPath=/dev/null", args),
    stdout = TRUE, stderr = TRUE
  ))
  status <- attr(result, "status")
  if (!is.null(status) && status != 0L) omtv_stop("Git provenance read failed.")
  result
}
source_commit <- trimws(git_output(c("rev-parse", "HEAD"))[[1L]])
source_status <- git_output(c("status", "--short", "--untracked-files=all"))

atomic_csv <- function(value, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(paste0(".", basename(path), "-"), dirname(path))
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  utils::write.csv(value, temporary, row.names = FALSE, na = "")
  if (!file.rename(temporary, path)) omtv_stop("Atomic CSV write failed: ", path)
  invisible(path)
}
atomic_json <- function(value, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(paste0(".", basename(path), "-"), dirname(path))
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  jsonlite::write_json(
    value, temporary, pretty = TRUE, auto_unbox = TRUE, digits = NA,
    null = "null", na = "null"
  )
  if (!file.rename(temporary, path)) omtv_stop("Atomic JSON write failed: ", path)
  invisible(path)
}
atomic_rds <- function(value, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(paste0(".", basename(path), "-"), dirname(path))
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  saveRDS(value, temporary, compress = FALSE)
  if (!file.rename(temporary, path)) omtv_stop("Atomic RDS write failed: ", path)
  invisible(path)
}
file_manifest <- function(root, exclude = character(0)) {
  files <- list.files(root, recursive = TRUE, full.names = TRUE,
                      all.files = TRUE, no.. = TRUE)
  files <- files[file.info(files)$isdir %in% FALSE]
  relative <- substring(files, nchar(normalizePath(root)) + 2L)
  keep <- !relative %in% exclude
  files <- files[keep]; relative <- relative[keep]
  data.frame(
    path = relative,
    bytes = unname(file.info(files)$size),
    sha256 = vapply(files, digest::digest, character(1L),
                    file = TRUE, algo = "sha256"),
    stringsAsFactors = FALSE
  )
}
verify_manifest <- function(root) {
  path <- file.path(root, "artifact_manifest.csv")
  if (!file.exists(path)) omtv_stop("Artifact manifest is missing: ", root)
  manifest <- utils::read.csv(path, stringsAsFactors = FALSE)
  valid <- nrow(manifest) > 0L && !anyDuplicated(manifest$path) &&
    all(vapply(seq_len(nrow(manifest)), function(ii) {
      candidate <- file.path(root, manifest$path[[ii]])
      file.exists(candidate) && !dir.exists(candidate) &&
        unname(file.info(candidate)$size) == manifest$bytes[[ii]] &&
        identical(
          digest::digest(file = candidate, algo = "sha256"),
          manifest$sha256[[ii]]
        )
    }, logical(1L)))
  if (!valid) omtv_stop("Artifact manifest verification failed: ", root)
  invisible(manifest)
}

promotion_mode <- mode %in% c("benchmark", "sentinel", "execute-wave")
expected_commit <- tolower(Sys.getenv("RQR_EXPECTED_PRIMARY_COMMIT", ""))
attestation_path <- Sys.getenv("RQR_PRIMARY_RUNTIME_ATTESTATION", "")
binding_requested <- nzchar(expected_commit) || nzchar(attestation_path)
runtime_binding <- list(
  match = FALSE, runtime_tree_digest = NA_character_,
  package_version = as.character(utils::packageVersion("rqrgibbs"))
)
provenance_control <- list()
if (promotion_mode && !binding_requested) {
  omtv_stop("Promotion modes require the reviewed full HEAD SHA and an isolated-runtime attestation.")
}
if (binding_requested) {
  if (!grepl("^[0-9a-f]{40}$", expected_commit) ||
      !identical(expected_commit, source_commit) ||
      !nzchar(attestation_path)) {
    omtv_stop("Runtime binding requires the reviewed full HEAD SHA and its isolated-runtime attestation.")
  }
  source(file.path(
    "application", "scripts", "lib", "isolated_runtime_lineage.R"
  ))
  source(file.path(
    "application", "scripts", "lib", "rqr_dlm_main_simulation.R"
  ))
  source(file.path(
    "application", "scripts", "lib", "rqr_dlm_confirmatory_simulation.R"
  ))
  runtime_binding <- rqr_main_primary_runtime_binding(
    repo_root, expected_commit, attestation_path
  )
  provenance_control <- rqr_confirm_primary_provenance_control(
    repo_root, expected_commit, attestation_path
  )
  if (!isTRUE(runtime_binding$match)) {
    omtv_stop("The executing primary runtime is not bound to the reviewed source.")
  }
}

default_output <- file.path(
  "application", "outputs", "oracle_mean_tilt_validation_v1",
  paste0(mode, "_", format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"))
)
output_dir <- arg_value("--output-dir=", default_output)
if (mode %in% c("precision-check", "verify-closeout",
                "health-check-read-only")) {
  run_dir <- normalizePath(
    arg_value("--run-dir=", ""), winslash = "/", mustWork = TRUE
  )
} else {
  if (file.exists(output_dir) && Sys.readlink(output_dir) != "") {
    omtv_stop("The output directory must not be a symbolic link.")
  }
  existing <- if (dir.exists(output_dir)) {
    list.files(output_dir, all.files = TRUE, no.. = TRUE)
  } else character(0L)
  wrapper_placeholders <- c(
    "process_group_monitor.csv", "runner.stdout.log", "runner.stderr.log"
  )
  substantive_existing <- setdiff(existing, wrapper_placeholders)
  resumable <- identical(mode, "execute-wave") &&
    identical(Sys.getenv("RQR_OMTV_RESUME", ""), "YES")
  if (length(substantive_existing) && !resumable) {
    omtv_stop("The output directory must be fresh.")
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)
}

write_source_contract <- function(root) {
  atomic_json(config, file.path(root, "config.json"))
  atomic_json(list(
    schema_version = omtv_schema(), mode = mode,
    source_commit = source_commit, source_clean = !length(source_status),
    config_sha256 = config_sha256,
    runtime_tree_digest = runtime_binding$runtime_tree_digest,
    exact_runtime_bound = isTRUE(runtime_binding$match),
    package_version = as.character(utils::packageVersion("rqrgibbs")),
    recorded_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    interpretation = paste(
      "Known-DGP repeated-sample recovery for generalized-Bayes interval",
      "roots; no response-likelihood or posterior-predictive contract."
    )
  ), file.path(root, "source_state.json"))
}
close_bundle <- function(root, details) {
  closeout <- c(list(
    schema_version = omtv_schema(), mode = mode,
    source_commit = source_commit, config_sha256 = config_sha256,
    runtime_tree_digest = runtime_binding$runtime_tree_digest,
    closed_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
  ), details)
  atomic_json(closeout, file.path(root, "closeout.json"))
  atomic_csv(
    file_manifest(root, exclude = c(
      "artifact_manifest.csv", "process_group_monitor.csv",
      "runner.stdout.log", "runner.stderr.log", "resource_summary.csv",
      "wrapper_closeout.csv", "wrapper_artifact_manifest.csv",
      "wrapper_failure_log.csv"
    )),
    file.path(root, "artifact_manifest.csv")
  )
  verify_manifest(root)
  invisible(closeout)
}
write_preflight <- function(root, preflight) {
  atomic_csv(preflight$scenarios, file.path(root, "scenarios.csv"))
  atomic_csv(preflight$oracle, file.path(root, "oracle_certificates.csv"))
  atomic_csv(preflight$incidence, file.path(root, "incidence_matrix.csv"))
  atomic_csv(preflight$tail_information,
             file.path(root, "tail_information.csv"))
  atomic_csv(preflight$projection_audit,
             file.path(root, "representability_audit.csv"))
  atomic_csv(preflight$gates, file.path(root, "preflight_gates.csv"))
  blueprint <- do.call(rbind, lapply(preflight$blueprints, function(value) {
    data.frame(
      scenario_id = value$scenario_id, model_family = value$family,
      design_digest = value$design_digest,
      population_digest = value$population_digest,
      n_index = if (identical(value$family, "dlm")) value$T else nrow(value$X),
      n_missing = if (identical(value$family, "dlm")) {
        length(value$missing_times)
      } else 0L,
      stringsAsFactors = FALSE
    )
  }))
  atomic_csv(blueprint, file.path(root, "dgp_blueprint_manifest.csv"))
  atomic_json(list(
    schema_version = omtv_schema(), protocol_digest = preflight$protocol_digest,
    stream_family = config$rng$kind,
    common_dgp_across_target_triplet = TRUE,
    seed_selection_prohibited = TRUE,
    replacement_of_failed_replication_prohibited = TRUE
  ), file.path(root, "rng_contract.json"))
}

if (identical(mode, "preflight")) {
  write_source_contract(output_dir)
  preflight <- omtv_preflight(config)
  write_preflight(output_dir, preflight)
  close_bundle(output_dir, list(
    pass = preflight$pass, oracle_certificates = nrow(preflight$oracle),
    prospective_cells = nrow(preflight$incidence),
    fits_started = 0L, execution_authorized = FALSE,
    compact_evidence_eligible = preflight$pass
  ))
  if (!preflight$pass) omtv_stop("Validation preflight failed.")
  message("[oracle-mean-tilt-validation] preflight passed: ", output_dir)
  quit(save = "no", status = 0L)
}

verify_wrapper_bundle <- function(path, expected_mode) {
  wrapper_manifest_path <- file.path(path, "wrapper_artifact_manifest.csv")
  required <- c(
    "process_group_monitor.csv", "resource_summary.csv",
    "wrapper_closeout.csv", "runner.stdout.log", "runner.stderr.log"
  )
  if (!file.exists(wrapper_manifest_path)) {
    omtv_stop("The monitored-wrapper artifact manifest is missing: ", path)
  }
  wrapper_manifest <- utils::read.csv(
    wrapper_manifest_path, stringsAsFactors = FALSE
  )
  if (!all(c("path", "bytes", "sha256") %in% names(wrapper_manifest)) ||
      anyDuplicated(wrapper_manifest$path) ||
      !all(required %in% wrapper_manifest$path)) {
    omtv_stop("The monitored-wrapper artifact manifest is incomplete: ", path)
  }
  valid <- all(vapply(seq_len(nrow(wrapper_manifest)), function(ii) {
    candidate <- file.path(path, wrapper_manifest$path[[ii]])
    file.exists(candidate) && !dir.exists(candidate) &&
      unname(file.info(candidate)$size) == wrapper_manifest$bytes[[ii]] &&
      identical(
        digest::digest(file = candidate, algo = "sha256"),
        wrapper_manifest$sha256[[ii]]
      )
  }, logical(1L)))
  if (!valid) {
    omtv_stop("Monitored-wrapper artifact verification failed: ", path)
  }
  resource <- utils::read.csv(
    file.path(path, "resource_summary.csv"), stringsAsFactors = FALSE
  )
  wrapper <- utils::read.csv(
    file.path(path, "wrapper_closeout.csv"), stringsAsFactors = FALSE
  )
  if (nrow(resource) != 1L || nrow(wrapper) != 1L ||
      !identical(resource$mode[[1L]], expected_mode) ||
      !identical(wrapper$mode[[1L]], expected_mode) ||
      resource$runner_status[[1L]] != 0L ||
      wrapper$runner_status[[1L]] != 0L ||
      !isTRUE(resource$pass[[1L]]) ||
      !isTRUE(wrapper$wrapper_pass[[1L]]) ||
      !isTRUE(resource$final_pgid_empty[[1L]]) ||
      !isTRUE(wrapper$final_pgid_empty[[1L]]) ||
      isTRUE(resource$timed_out[[1L]]) ||
      isTRUE(resource$limit_triggered[[1L]]) ||
      !identical(wrapper$signal_received[[1L]], "NONE")) {
    omtv_stop("The monitored-wrapper closeout is not passing: ", expected_mode)
  }
  invisible(list(resource = resource, wrapper = wrapper))
}

bind_input <- function(path, expected_mode) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  verify_manifest(path)
  verify_wrapper_bundle(path, expected_mode)
  closeout <- jsonlite::read_json(
    file.path(path, "closeout.json"), simplifyVector = TRUE
  )
  if (!identical(closeout$mode, expected_mode) || !isTRUE(closeout$pass) ||
      !identical(closeout$source_commit, source_commit) ||
      !identical(closeout$config_sha256, config_sha256) ||
      !isTRUE(runtime_binding$match) ||
      !identical(
        closeout$runtime_tree_digest, runtime_binding$runtime_tree_digest
      )) {
    omtv_stop("An input bundle is not bound to the current source/config: ", expected_mode)
  }
  data.frame(
    mode = expected_mode, path = path,
    closeout_sha256 = digest::digest(
      file = file.path(path, "closeout.json"), algo = "sha256"
    ),
    manifest_sha256 = digest::digest(
      file = file.path(path, "artifact_manifest.csv"), algo = "sha256"
    ),
    wrapper_manifest_sha256 = digest::digest(
      file = file.path(path, "wrapper_artifact_manifest.csv"), algo = "sha256"
    ),
    runtime_tree_digest = closeout$runtime_tree_digest,
    stringsAsFactors = FALSE
  )
}

if (identical(mode, "reference-only")) {
  write_source_contract(output_dir)
  preflight <- omtv_preflight(config)
  source(file.path(script_dir, "32_oracle_tilt_illustration_utils.R"))
  source(file.path(script_dir, "33_oracle_tilt_forensic_utils.R"))
  source(file.path(script_dir, "34_oracle_tilt_publication_utils.R"))
  source(file.path(script_dir, "42_oracle_tilt_publication_v3_utils.R"))
  source(file.path(script_dir, "58_oracle_tilt_v5_utils.R"))
  v5_config <- oti_read_json(file.path(
    "application", "config", "oracle_tilt_c095_v5_exact_delta_20260810.json"
  ))
  inherited <- otv5_reference_suite(v5_config)
  streams <- omtv_assign_streams(config, 2L)
  blueprint <- preflight$blueprints[["S03_primary_skew/dlm"]]
  first <- omtv_generate_replication(
    config, blueprint, 0.80, streams$seed_serialized[[1L]]
  )
  second <- omtv_generate_replication(
    config, blueprint, 0.80, streams$seed_serialized[[1L]]
  )
  content_gap <- max(vapply(seq_len(nrow(preflight$oracle)), function(ii) {
    row <- preflight$oracle[ii, , drop = FALSE]
    endpoints <- rqrgibbs::rqr_interval_oracle_endpoints(
      0, 1, rqrgibbs::rqr_interval_oracle(
        "asymmetric_laplace", row$coverage_level, row$target,
        params = list(
          tau = row$tau, scale = 1, variance_standardized = TRUE
        )
      )
    )
    abs(rqrgibbs::rqr_oracle_conditional_content(
      endpoints$lower, endpoints$upper, 0, 1,
      family = "asymmetric_laplace",
      params = list(
        tau = row$tau, scale = 1, variance_standardized = TRUE
      )
    ) - row$coverage_level)
  }, numeric(1L)))
  risk_gap <- max(vapply(seq_len(nrow(preflight$oracle)), function(ii) {
    row <- preflight$oracle[ii, , drop = FALSE]
    params <- list(tau = row$tau, scale = 1, variance_standardized = TRUE)
    base <- rqrgibbs::rqr_oracle_tilted_risk(
      row$lower_root, row$upper_root, row$coverage_level,
      row$mean_tilt, 0, 1, "asymmetric_laplace", params
    )$mean_tilted_risk
    perturbation <- max(1e-4, 1e-3 * row$width)
    candidates <- rbind(
      c(row$lower_root - perturbation, row$upper_root),
      c(row$lower_root + perturbation, row$upper_root),
      c(row$lower_root, row$upper_root - perturbation),
      c(row$lower_root, row$upper_root + perturbation)
    )
    candidate_risk <- apply(candidates, 1L, function(pair) {
      rqrgibbs::rqr_oracle_tilted_risk(
        pair[[1L]], pair[[2L]], row$coverage_level,
        row$mean_tilt, 0, 1, "asymmetric_laplace", params
      )$mean_tilted_risk
    })
    max(base - candidate_risk)
  }, numeric(1L)))
  extra <- data.frame(
    gate = c(
      "validation_preflight", "exact_oracle_conditional_content",
      "exact_oracle_local_risk_minimum", "dgp_stream_determinism",
      "canonical_missing_observation_count", "deterministic_seasonal_signal",
      "target_triplet_shared_dgp_contract", "new_execution_disabled"
    ),
    value = c(
      as.numeric(preflight$pass), content_gap, risk_gap,
      as.numeric(identical(first, second)), sum(is.na(first$y)),
      diff(range(blueprint$deterministic_seasonal_state[1L, ])),
      as.numeric(config$rng$common_dgp_across_target_triplet),
      as.numeric(!config$execution_authorized)
    ),
    threshold = c(1, 1e-10, 1e-10, 1, length(blueprint$missing_times),
                  1, 1, 1),
    comparison = c("==", "<=", "<=", "==", "==", ">=", "==", "=="),
    stringsAsFactors = FALSE
  )
  extra$pass <- mapply(function(value, threshold, comparison) {
    switch(comparison, "==" = abs(value - threshold) <= 1e-12,
           "<=" = value <= threshold, ">=" = value >= threshold, FALSE)
  }, extra$value, extra$threshold, extra$comparison)
  reference <- rbind(
    transform(inherited, reference_family = "sampler_and_oracle_v5"),
    transform(extra, reference_family = "validation_protocol")
  )
  atomic_csv(reference, file.path(output_dir, "reference_gates.csv"))
  passed <- nrow(reference) == 44L && all(reference$pass)
  close_bundle(output_dir, list(
    pass = passed, reference_gates = nrow(reference),
    passed_reference_gates = sum(reference$pass), fits_started = 0L,
    compact_evidence_eligible = passed
  ))
  if (!passed) omtv_stop("Validation reference-only gates failed.")
  message("[oracle-mean-tilt-validation] reference-only passed: ", output_dir)
  quit(save = "no", status = 0L)
}

if (identical(mode, "benchmark")) {
  if (!identical(Sys.getenv("RQR_OMTV_BENCHMARK_CONFIRM", ""), "YES")) {
    omtv_stop("benchmark is fail-closed; set RQR_OMTV_BENCHMARK_CONFIRM=YES after review.")
  }
  write_source_contract(output_dir)
  preflight <- omtv_preflight(config)
  preflight_binding <- bind_input(
    Sys.getenv("RQR_OMTV_PREFLIGHT_DIR", ""), "preflight"
  )
  reference_binding <- bind_input(
    Sys.getenv("RQR_OMTV_REFERENCE_DIR", ""), "reference-only"
  )
  atomic_csv(rbind(preflight_binding, reference_binding),
             file.path(output_dir, "input_bundle_binding.csv"))
  specifications <- data.frame(
    scenario_id = c(
      "S01_symmetric_control", "S03_primary_skew", "S05_article_stress",
      "S01_symmetric_control", "S03_primary_skew", "S05_article_stress"
    ),
    model_family = rep(c("fixed_design", "dlm"), each = 3L),
    target = "SH", stringsAsFactors = FALSE
  )
  streams <- omtv_assign_streams(config, 2L * nrow(specifications))
  rows <- lapply(seq_len(nrow(specifications)), function(ii) {
    specification <- specifications[ii, , drop = FALSE]
    key <- paste(
      specification$scenario_id, specification$model_family, sep = "/"
    )
    blueprint <- preflight$blueprints[[key]]
    scenario <- preflight$scenarios[
      preflight$scenarios$scenario_id == specification$scenario_id,
      , drop = FALSE
    ]
    oracle <- preflight$oracle[
      preflight$oracle$scenario_id == specification$scenario_id &
        preflight$oracle$target == specification$target,
      , drop = FALSE
    ]
    generated <- omtv_generate_replication(
      config, blueprint, scenario$tau,
      streams$seed_serialized[[2L * ii - 1L]]
    )
    result <- omtv_fit_replication(
      config, blueprint, generated, oracle,
      streams$seed_serialized[[2L * ii]], provenance_control
    )
    data.frame(
      scenario_id = specification$scenario_id,
      model_family = specification$model_family,
      target = specification$target,
      n_index = length(generated$y), elapsed_seconds = result$elapsed_seconds,
      numerical_repair_count = result$numerical_repair_count,
      exact_joint_target = result$exact_joint_target,
      target_numerical_eligible = result$target_numerical_eligible,
      reproducibility_eligible = result$reproducibility_eligible,
      minimum_bulk_ess = min(result$diagnostics$ess_bulk, na.rm = TRUE),
      minimum_tail_ess = min(result$diagnostics$ess_tail, na.rm = TRUE),
      maximum_mcse_over_sd = max(
        result$diagnostics$mcse_over_sd, na.rm = TRUE
      ),
      fit_pass = result$pass, stringsAsFactors = FALSE
    )
  })
  benchmark <- do.call(rbind, rows)
  atomic_csv(benchmark, file.path(output_dir, "benchmark_summary.csv"))
  passed <- nrow(benchmark) == 6L && all(benchmark$fit_pass) &&
    all(benchmark$reproducibility_eligible)
  close_bundle(output_dir, list(
    pass = passed, benchmark_fits = nrow(benchmark),
    fits_started = nrow(benchmark), compact_evidence_eligible = passed
  ))
  if (!passed) omtv_stop("Production-shape benchmark failed.")
  message("[oracle-mean-tilt-validation] benchmark passed: ", output_dir)
  quit(save = "no", status = 0L)
}

if (identical(mode, "sentinel")) {
  if (!isTRUE(config$replication_schedule_frozen)) {
    omtv_stop("sentinel is disabled until the replication/resource schedule is frozen.")
  }
  if (!identical(Sys.getenv("RQR_OMTV_SENTINEL_CONFIRM", ""), "YES")) {
    omtv_stop("sentinel is fail-closed; set RQR_OMTV_SENTINEL_CONFIRM=YES after review.")
  }
  write_source_contract(output_dir)
  preflight <- omtv_preflight(config)
  bindings <- rbind(
    bind_input(Sys.getenv("RQR_OMTV_PREFLIGHT_DIR", ""), "preflight"),
    bind_input(Sys.getenv("RQR_OMTV_REFERENCE_DIR", ""), "reference-only"),
    bind_input(Sys.getenv("RQR_OMTV_BENCHMARK_DIR", ""), "benchmark")
  )
  atomic_csv(bindings, file.path(output_dir, "input_bundle_binding.csv"))
  cells <- preflight$incidence[, c(
    "scenario_id", "model_family", "target"
  )]
  streams <- omtv_assign_streams(config, nrow(cells) * 5L)
  cell_rows <- list()
  failure <- NULL
  for (ii in seq_len(nrow(cells))) {
    cell <- cells[ii, , drop = FALSE]
    key <- paste(cell$scenario_id, cell$model_family, sep = "/")
    blueprint <- preflight$blueprints[[key]]
    scenario <- preflight$scenarios[
      preflight$scenarios$scenario_id == cell$scenario_id, , drop = FALSE
    ]
    oracle <- preflight$oracle[
      preflight$oracle$scenario_id == cell$scenario_id &
        preflight$oracle$target == cell$target, , drop = FALSE
    ]
    offset <- (ii - 1L) * 5L
    generated <- omtv_generate_replication(
      config, blueprint, scenario$tau,
      streams$seed_serialized[[offset + 1L]]
    )
    chain_results <- lapply(seq_len(config$mcmc$sentinel_chains), function(chain) {
      omtv_fit_replication(
        config, blueprint, generated, oracle,
        streams$seed_serialized[[offset + 1L + chain]],
        provenance_control, retain_diagnostic_draws = TRUE
      )
    })
    matrices <- lapply(chain_results, function(result) {
      attr(result$diagnostics, "draw_matrix")
    })
    if (any(vapply(matrices, is.null, logical(1L))) ||
        length(unique(vapply(matrices, nrow, integer(1L)))) != 1L ||
        length(unique(vapply(matrices, ncol, integer(1L)))) != 1L) {
      omtv_stop("Sentinel diagnostic draw matrices are incomplete.")
    }
    draws <- array(
      NA_real_, c(nrow(matrices[[1L]]), length(matrices), ncol(matrices[[1L]])),
      dimnames = list(NULL, NULL, colnames(matrices[[1L]]))
    )
    for (chain in seq_along(matrices)) draws[, chain, ] <- matrices[[chain]]
    posterior_draws <- posterior::as_draws_array(draws)
    rhat <- posterior::rhat(posterior_draws)
    bulk <- posterior::ess_bulk(posterior_draws)
    tail <- posterior::ess_tail(posterior_draws)
    mcse <- posterior::mcse_mean(posterior_draws)
    posterior_sd <- apply(draws, 3L, stats::sd)
    chain_contract_pass <- all(vapply(chain_results, function(result) {
      result$numerical_repair_count == 0L && result$exact_joint_target &&
        result$target_numerical_eligible && result$reproducibility_eligible &&
        is.finite(result$loss_identity_maximum_absolute_error) &&
        result$loss_identity_maximum_absolute_error <= 1e-8 &&
        result$elapsed_seconds <= config$resources$maximum_worker_seconds
    }, logical(1L)))
    cell_pass <- chain_contract_pass && all(is.finite(rhat)) &&
      all(is.finite(bulk)) && all(is.finite(tail)) &&
      all(is.finite(mcse)) && max(rhat) <= config$diagnostics$rhat_max &&
      min(bulk) >= config$diagnostics$bulk_ess_min &&
      min(tail) >= config$diagnostics$tail_ess_min &&
      max(ifelse(posterior_sd > 0, mcse / posterior_sd, 0)) <=
        config$diagnostics$mcse_over_sd_max
    cell_root <- file.path(
      output_dir, "cells",
      paste(cell$scenario_id, cell$model_family, tolower(cell$target), sep = "_")
    )
    dir.create(cell_root, recursive = TRUE, showWarnings = FALSE)
    for (chain in seq_along(chain_results)) {
      atomic_rds(
        chain_results[[chain]],
        file.path(cell_root, sprintf("chain_%02d.rds", chain))
      )
    }
    diagnostic <- data.frame(
      variable = names(rhat), rhat = as.numeric(rhat),
      ess_bulk = as.numeric(bulk), ess_tail = as.numeric(tail),
      mcse_mean = as.numeric(mcse), posterior_sd = posterior_sd,
      mcse_over_sd = ifelse(posterior_sd > 0, as.numeric(mcse) / posterior_sd, 0),
      stringsAsFactors = FALSE
    )
    atomic_csv(diagnostic, file.path(cell_root, "diagnostics.csv"))
    atomic_csv(
      file_manifest(cell_root, exclude = "artifact_manifest.csv"),
      file.path(cell_root, "artifact_manifest.csv")
    )
    verify_manifest(cell_root)
    cell_rows[[ii]] <- data.frame(
      scenario_id = cell$scenario_id, model_family = cell$model_family,
      target = cell$target, chains = length(chain_results),
      maximum_chain_elapsed_seconds = max(vapply(
        chain_results, `[[`, numeric(1L), "elapsed_seconds"
      )),
      maximum_rhat = max(rhat), minimum_bulk_ess = min(bulk),
      minimum_tail_ess = min(tail),
      maximum_mcse_over_sd = max(
        ifelse(posterior_sd > 0, mcse / posterior_sd, 0)
      ), pass = cell_pass, stringsAsFactors = FALSE
    )
    if (!cell_pass) {
      failure <- data.frame(
        scenario_id = cell$scenario_id, model_family = cell$model_family,
        target = cell$target, stage = "four_chain_sentinel",
        message = "Prespecified sentinel diagnostics failed; later cells were not started.",
        stringsAsFactors = FALSE
      )
      break
    }
  }
  sentinel <- do.call(rbind, cell_rows)
  atomic_csv(sentinel, file.path(output_dir, "sentinel_summary.csv"))
  if (!is.null(failure)) {
    atomic_csv(failure, file.path(output_dir, "failure_ledger.csv"))
  }
  passed <- nrow(sentinel) == nrow(cells) && all(sentinel$pass)
  close_bundle(output_dir, list(
    pass = passed, planned_cells = nrow(cells),
    completed_cells = nrow(sentinel), fits_started = 4L * nrow(sentinel),
    stopped_after_first_failed_cell = !is.null(failure),
    compact_evidence_eligible = passed
  ))
  if (!passed) omtv_stop("One or more sentinel cells failed.")
  message("[oracle-mean-tilt-validation] all sentinels passed: ", output_dir)
  quit(save = "no", status = 0L)
}

if (identical(mode, "execute-wave")) {
  if (!isTRUE(config$execution_authorized) ||
      !isTRUE(config$replication_schedule_frozen) ||
      !identical(Sys.getenv("RQR_OMTV_EXECUTE_CONFIRM", ""), "YES")) {
    omtv_stop("execute-wave is fail-closed: authorization and frozen schedule are required.")
  }
  preflight <- omtv_preflight(config)
  bindings <- rbind(
    bind_input(Sys.getenv("RQR_OMTV_PREFLIGHT_DIR", ""), "preflight"),
    bind_input(Sys.getenv("RQR_OMTV_REFERENCE_DIR", ""), "reference-only"),
    bind_input(Sys.getenv("RQR_OMTV_BENCHMARK_DIR", ""), "benchmark"),
    bind_input(Sys.getenv("RQR_OMTV_SENTINEL_DIR", ""), "sentinel")
  )
  plan <- omtv_task_plan(config)
  wave <- omtv_scalar_integer(
    as.numeric(arg_value("--wave=", "0")), "wave", 1L
  )
  selected <- plan[plan$wave == wave, , drop = FALSE]
  if (!nrow(selected)) omtv_stop("The requested wave has no planned tasks.")
  if (!file.exists(file.path(output_dir, "source_state.json"))) {
    write_source_contract(output_dir)
    atomic_csv(bindings, file.path(output_dir, "input_bundle_binding.csv"))
    atomic_csv(plan, file.path(output_dir, "fit_plan.csv"))
    max_replications <- max(plan$replication)
    atomic_csv(
      omtv_rng_ledger(config, max_replications),
      file.path(output_dir, "rng_ledger.csv")
    )
  } else {
    prior <- jsonlite::read_json(
      file.path(output_dir, "source_state.json"), simplifyVector = TRUE
    )
    if (!identical(prior$source_commit, source_commit) ||
        !identical(prior$config_sha256, config_sha256) ||
        !identical(prior$runtime_tree_digest, runtime_binding$runtime_tree_digest)) {
      omtv_stop("Resume source/config/runtime binding changed.")
    }
  }
  ledger <- utils::read.csv(
    file.path(output_dir, "rng_ledger.csv"), stringsAsFactors = FALSE
  )
  task_root <- file.path(output_dir, "tasks")
  dir.create(task_root, recursive = TRUE, showWarnings = FALSE)
  run_one <- function(index) {
    task <- selected[index, , drop = FALSE]
    destination <- file.path(task_root, paste0(task$task_id, ".rds"))
    dgp_row <- ledger[
      ledger$scenario_id == task$scenario_id &
        ledger$model_family == task$model_family &
        ledger$target == "DGP" & ledger$replication == task$replication,
      , drop = FALSE
    ]
    mcmc_row <- ledger[
      ledger$scenario_id == task$scenario_id &
        ledger$model_family == task$model_family &
        ledger$target == task$target &
        ledger$replication == task$replication,
      , drop = FALSE
    ]
    if (nrow(dgp_row) != 1L || nrow(mcmc_row) != 1L) {
      return(data.frame(
        task_id = task$task_id, task_key = task$task_key,
        status = "contract_failure", pass = FALSE,
        message = "RNG ledger lookup was not unique.", stringsAsFactors = FALSE
      ))
    }
    if (file.exists(destination)) {
      existing <- tryCatch(readRDS(destination), error = function(error) NULL)
      valid_existing <- is.list(existing) &&
        identical(
          existing$schema_version,
          "rqrgibbs_oracle_mean_tilt_task/1.0.0"
        ) && is.list(existing$result) &&
        identical(
          existing$result$schema_version,
          "rqrgibbs_oracle_mean_tilt_fit/1.0.0"
        ) && identical(existing$task_key, task$task_key) &&
          identical(existing$source_commit, source_commit) &&
          identical(existing$config_sha256, config_sha256) &&
          identical(
            existing$runtime_tree_digest, runtime_binding$runtime_tree_digest
          ) && identical(existing$dgp_stream_digest, dgp_row$seed_digest) &&
          identical(existing$mcmc_stream_digest, mcmc_row$seed_digest) &&
          identical(existing$result$scenario_id, task$scenario_id) &&
          identical(existing$result$family, task$model_family) &&
          identical(existing$result$target, task$target)
      if (valid_existing) {
        resource_eligible <- existing$result$elapsed_seconds <=
          config$resources$maximum_worker_seconds
        eligible <- isTRUE(existing$result$pass) &&
          isTRUE(existing$result$reproducibility_eligible) &&
          resource_eligible
        return(data.frame(
          task_id = task$task_id, task_key = task$task_key,
          status = if (eligible) {
            "resumed_complete"
          } else if (!resource_eligible) {
            "resource_failure"
          } else "diagnostic_failure",
          pass = eligible,
          message = "", stringsAsFactors = FALSE
        ))
      }
      return(data.frame(
        task_id = task$task_id, task_key = task$task_key,
        status = "contract_failure", pass = FALSE,
        message = "Existing task artifact failed its resume contract.",
        stringsAsFactors = FALSE
      ))
    }
    key <- paste(task$scenario_id, task$model_family, sep = "/")
    blueprint <- preflight$blueprints[[key]]
    scenario <- preflight$scenarios[
      preflight$scenarios$scenario_id == task$scenario_id, , drop = FALSE
    ]
    oracle <- preflight$oracle[
      preflight$oracle$scenario_id == task$scenario_id &
        preflight$oracle$target == task$target, , drop = FALSE
    ]
    result <- tryCatch({
      generated <- omtv_generate_replication(
        config, blueprint, scenario$tau, dgp_row$seed_serialized
      )
      omtv_fit_replication(
        config, blueprint, generated, oracle, mcmc_row$seed_serialized,
        provenance_control
      )
    }, error = function(error) error)
    if (inherits(result, "error")) {
      return(data.frame(
        task_id = task$task_id, task_key = task$task_key,
        status = "fit_failure", pass = FALSE,
        message = conditionMessage(result), stringsAsFactors = FALSE
      ))
    }
    result$estimands$scenario_id <- task$scenario_id
    result$estimands$model_family <- task$model_family
    result$estimands$target <- task$target
    result$estimands$replication <- task$replication
    envelope <- list(
      schema_version = "rqrgibbs_oracle_mean_tilt_task/1.0.0",
      task_id = task$task_id, task_key = task$task_key,
      source_commit = source_commit, config_sha256 = config_sha256,
      runtime_tree_digest = runtime_binding$runtime_tree_digest,
      dgp_stream_digest = dgp_row$seed_digest,
      mcmc_stream_digest = mcmc_row$seed_digest,
      worker_seconds_eligible =
        result$elapsed_seconds <= config$resources$maximum_worker_seconds,
      result = result
    )
    atomic_rds(envelope, destination)
    data.frame(
      task_id = task$task_id, task_key = task$task_key,
      status = if (result$elapsed_seconds >
                   config$resources$maximum_worker_seconds) {
        "resource_failure"
      } else if (result$pass && result$reproducibility_eligible) {
        "completed"
      } else "diagnostic_failure",
      pass = result$pass && result$reproducibility_eligible &&
        result$elapsed_seconds <= config$resources$maximum_worker_seconds,
      message = "", stringsAsFactors = FALSE
    )
  }
  run_one_safe <- function(index) {
    tryCatch(
      run_one(index),
      error = function(error) {
        task <- selected[index, , drop = FALSE]
        data.frame(
          task_id = task$task_id, task_key = task$task_key,
          status = "infrastructure_failure", pass = FALSE,
          message = conditionMessage(error), stringsAsFactors = FALSE
        )
      }
    )
  }
  workers <- min(
    as.integer(config$resources$maximum_fit_workers), nrow(selected)
  )
  statuses <- if (.Platform$OS.type == "unix" && workers > 1L) {
    parallel::mclapply(seq_len(nrow(selected)), run_one_safe, mc.cores = workers,
                       mc.preschedule = FALSE)
  } else {
    lapply(seq_len(nrow(selected)), run_one_safe)
  }
  statuses <- do.call(rbind, statuses)
  wave_root <- file.path(output_dir, sprintf("waves/wave_%04d", wave))
  dir.create(wave_root, recursive = TRUE, showWarnings = FALSE)
  atomic_csv(statuses, file.path(wave_root, "wave_status.csv"))
  failures <- statuses[!statuses$pass, , drop = FALSE]
  if (nrow(failures)) {
    atomic_csv(failures, file.path(wave_root, "failure_ledger.csv"))
  }
  atomic_csv(
    file_manifest(wave_root, exclude = "artifact_manifest.csv"),
    file.path(wave_root, "artifact_manifest.csv")
  )
  verify_manifest(wave_root)
  if (any(statuses$status %in% c(
    "contract_failure", "infrastructure_failure"
  ))) {
    omtv_stop("The wave encountered a source, artifact, or infrastructure contract failure.")
  }
  message(
    "[oracle-mean-tilt-validation] wave ", wave, " closed: ",
    sum(statuses$pass), "/", nrow(statuses), " task fits passed."
  )
  quit(save = "no", status = 0L)
}

if (identical(mode, "precision-check")) {
  path <- file.path(run_dir, "replication_estimands.csv")
  if (!file.exists(path)) omtv_stop("replication_estimands.csv is missing.")
  rows <- utils::read.csv(path, stringsAsFactors = FALSE)
  checkpoint <- as.integer(arg_value("--checkpoint=", "0"))
  decision <- omtv_precision_decision(rows, config, checkpoint)
  atomic_csv(
    decision,
    file.path(run_dir, sprintf("precision_decision_%04d.csv", checkpoint))
  )
  message("[oracle-mean-tilt-validation] precision cells passed: ",
          sum(decision$primary_precision_pass), "/", nrow(decision))
  quit(save = "no", status = 0L)
}

if (identical(mode, "health-check-read-only")) {
  task_files <- list.files(
    file.path(run_dir, "tasks"), pattern = "\\.rds$", recursive = TRUE,
    full.names = TRUE
  )
  failures <- file.path(run_dir, "failure_ledger.csv")
  failure_count <- if (file.exists(failures)) {
    nrow(utils::read.csv(failures, stringsAsFactors = FALSE))
  } else 0L
  cat(sprintf(
    "run=%s\ncompleted_task_artifacts=%d\nstructured_failures=%d\n",
    run_dir, length(task_files), failure_count
  ))
  quit(save = "no", status = 0L)
}

if (identical(mode, "verify-closeout")) {
  verify_manifest(run_dir)
  closeout <- jsonlite::read_json(
    file.path(run_dir, "closeout.json"), simplifyVector = TRUE
  )
  if (!isTRUE(closeout$pass)) omtv_stop("The closeout is not passing.")
  message("[oracle-mean-tilt-validation] closeout verified: ", run_dir)
  quit(save = "no", status = 0L)
}
