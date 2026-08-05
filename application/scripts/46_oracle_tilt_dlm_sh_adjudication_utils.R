otad_config_schema <- function() {
  "rqrgibbs_oracle_tilt_dlm_sh_adjudication_config/1.1.0"
}

otad_worker_schema <- function() {
  "rqrgibbs_oracle_tilt_dlm_sh_adjudication_worker/1.1.0"
}

otad_closeout_schema <- function() {
  "rqrgibbs_oracle_tilt_dlm_sh_adjudication/1.1.0"
}

otad_sha256 <- function(x, name) {
  x <- as.character(x)
  if (length(x) != 1L || is.na(x) ||
      !grepl("^[0-9a-f]{64}$", x)) {
    oti_stop(name, " must be one lowercase SHA-256 value.")
  }
  x
}

otad_validate_config <- function(config, repo_root) {
  if (!identical(as.character(config$schema_version), otad_config_schema())) {
    oti_stop("Unsupported DLM/SH adjudication configuration schema.")
  }
  otv3_required_logical(config$execution_authorized, "execution_authorized")
  if (otv3_integer_scalar(
        config$execution_attempt, "execution_attempt", 1L
      ) != 2L ||
      otv3_integer_scalar(
        config$maximum_execution_attempts, "maximum_execution_attempts", 1L
      ) != 2L ||
      otv3_integer_scalar(
        config$statistical_attempt, "statistical_attempt", 1L
      ) != 1L ||
      otv3_integer_scalar(
        config$maximum_statistical_attempts,
        "maximum_statistical_attempts", 1L
      ) != 1L ||
      !identical(as.character(config$family), "dlm") ||
      !identical(as.character(config$target), "SH") ||
      otv3_integer_scalar(config$n_chains, "n_chains", 2L) != 5L ||
      otv3_integer_scalar(config$workers, "workers", 1L) != 2L ||
      otv3_integer_scalar(
        config$baseline_retained_draws, "baseline_retained_draws", 1L
      ) != 6000L) {
    oti_stop("The one-shot DLM/SH adjudication identity changed.")
  }
  recovery <- config$software_recovery_contract %||% list()
  expected_recovery <- list(
    invalidated_execution_source_commit =
      "3ac3a05db420bf17cdeffbb41f0b6b8947b373f4",
    failure_class = "prepublication_worker_contract_validation_defect",
    invalidated_wrapper_manifest_sha256 =
      "2e41a28dab0d201e34672b05e47a5a2045a2511367bb8bb6189bfa23b4732f61",
    invalidated_runner_stderr_sha256 =
      "2e65a88ce504ee2625763decf946ca74833f68cc62c3f7dbdeffb7d7de246094",
    invalidated_resource_summary_sha256 =
      "65059192c22e7aaf4b67b1db5a27c31f402728b4b41878589ca7dffcf8c69b8b",
    invalidated_wrapper_closeout_sha256 =
      "d86189c7d1bd349221555dc0da22cbedcc54dc78ec0c81fd831677feaf2ce04b",
    invalidated_worker_artifact_count = 0L,
    scientific_contract_changed = FALSE,
    failed_execution_is_statistical_attempt = FALSE,
    replacement_is_automatic_rerun = FALSE
  )
  if (!identical(recovery, expected_recovery)) {
    oti_stop("The software-recovery evidence contract changed.")
  }
  staging <- config$staging_contract %||% list()
  if (otv3_integer_scalar(
        staging$acceptance_chain, "acceptance_chain", 1L
      ) != 1L ||
      !identical(as.integer(unlist(staging$remaining_chains)), 2:5) ||
      !identical(staging$acceptance_prefix_required, TRUE) ||
      !identical(staging$fail_before_remaining_on_acceptance_failure, TRUE) ||
      !identical(staging$all_workers_persist_before_summary, TRUE)) {
    oti_stop("The staged software-recovery execution contract changed.")
  }
  base_path <- normalizePath(
    file.path(repo_root, as.character(config$base_config)),
    winslash = "/", mustWork = TRUE
  )
  manifest_path <- normalizePath(
    file.path(repo_root, as.character(config$baseline_manifest)),
    winslash = "/", mustWork = TRUE
  )
  if (!identical(
        oti_file_sha256(base_path),
        otad_sha256(config$base_config_sha256, "base_config_sha256")
      ) ||
      !grepl("^[0-9a-f]{40}$", as.character(config$base_source_commit))) {
    oti_stop("The frozen base source or configuration binding changed.")
  }
  invisible(lapply(c(
    "base_runtime_tree_digest", "base_dgp_digest", "base_target_digest",
    "baseline_source_state_sha256", "baseline_runtime_binding_sha256",
    "baseline_cell_receipt_sha256", "baseline_cell_manifest_sha256",
    "baseline_wrapper_manifest_sha256"
  ), function(name) otad_sha256(config[[name]], name)))
  control <- config$mcmc_override %||% list()
  if (otv3_integer_scalar(control$n_burn, "n_burn", 1L) != 2500L ||
      otv3_integer_scalar(control$n_mcmc, "n_mcmc", 1L) != 12000L ||
      otv3_integer_scalar(control$thin, "thin", 1L) != 1L ||
      !identical(as.character(control$backend), "cpp") ||
      !identical(control$store_state_draws, FALSE) ||
      !identical(control$store_latent_draws, FALSE)) {
    oti_stop("The adjudication MCMC extension contract changed.")
  }
  prefix <- config$prefix_contract %||% list()
  if (!identical(prefix$bitwise_required, TRUE) ||
      !identical(
        as.character(unlist(prefix$objects)),
        c("lower_draws", "upper_draws", "scalar_draws")
      )) {
    oti_stop("The bitwise prefix contract changed.")
  }
  decision <- config$decision_contract %||% list()
  required_decisions <- c(
    "strict_thresholds_unchanged",
    "strict_pass_required_for_automatic_promotion",
    "qualified_descriptive_is_not_a_strict_pass",
    "no_additional_automatic_rerun", "no_gate_relaxation",
    "no_model_or_dgp_retuning"
  )
  if (!all(vapply(required_decisions, function(name) {
    identical(decision[[name]], TRUE)
  }, logical(1L)))) {
    oti_stop("The fail-closed adjudication decision contract changed.")
  }
  interpretation <- config$interpretation %||% list()
  if (!identical(interpretation$response_likelihood, FALSE) ||
      !identical(interpretation$response_predictive_draws, FALSE) ||
      !identical(interpretation$population_oracle_tilt, TRUE) ||
      !identical(interpretation$cornish_fisher_used, FALSE) ||
      !identical(interpretation$simulation_study, FALSE) ||
      !identical(interpretation$single_dataset_adjudication, TRUE)) {
    oti_stop("The adjudication interpretation contract changed.")
  }
  list(base_path = base_path, manifest_path = manifest_path)
}

otad_read_baseline_manifest <- function(path) {
  manifest <- utils::read.csv(path, stringsAsFactors = FALSE)
  required <- c(
    "chain", "path", "sha256", "bytes", "contract_digest", "seed",
    "profile", "retained_draws"
  )
  if (!identical(names(manifest), required) || nrow(manifest) != 5L ||
      !identical(as.integer(manifest$chain), 1:5) ||
      anyDuplicated(manifest$path) ||
      any(!grepl("^[0-9a-f]{64}$", manifest$sha256)) ||
      any(!grepl("^[0-9a-f]{64}$", manifest$contract_digest)) ||
      any(manifest$bytes <= 0) ||
      any(manifest$retained_draws != 6000L)) {
    oti_stop("The frozen DLM/SH baseline manifest is invalid.")
  }
  manifest
}

otad_verify_baseline <- function(root, manifest, config) {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  fixed_files <- data.frame(
    path = c(
      "source_state.json", "runtime_binding.json",
      "cells/dlm_sh/cell_receipt.json",
      "cells/dlm_sh/artifact_manifest.csv",
      "wrapper_artifact_manifest.csv"
    ),
    sha256 = c(
      config$baseline_source_state_sha256,
      config$baseline_runtime_binding_sha256,
      config$baseline_cell_receipt_sha256,
      config$baseline_cell_manifest_sha256,
      config$baseline_wrapper_manifest_sha256
    ), stringsAsFactors = FALSE
  )
  rows <- lapply(seq_len(nrow(fixed_files)), function(index) {
    path <- file.path(root, fixed_files$path[index])
    pass <- file.exists(path) && !dir.exists(path) &&
      identical(oti_file_sha256(path), fixed_files$sha256[index])
    data.frame(
      object = fixed_files$path[index], expected_sha256 = fixed_files$sha256[index],
      observed_sha256 = if (file.exists(path)) oti_file_sha256(path) else NA_character_,
      bytes = if (file.exists(path)) unname(file.info(path)$size) else NA_real_,
      pass = pass, stringsAsFactors = FALSE
    )
  })
  worker_rows <- lapply(seq_len(nrow(manifest)), function(index) {
    path <- file.path(root, manifest$path[index])
    pass <- file.exists(path) && !dir.exists(path) &&
      unname(file.info(path)$size) == manifest$bytes[index] &&
      identical(oti_file_sha256(path), manifest$sha256[index])
    envelope <- if (pass) tryCatch(readRDS(path), error = function(error) NULL)
      else NULL
    pass <- pass && is.list(envelope) &&
      identical(envelope$contract_digest, manifest$contract_digest[index]) &&
      identical(as.integer(envelope$contract$chain), manifest$chain[index]) &&
      identical(as.integer(envelope$contract$seed), manifest$seed[index]) &&
      identical(as.character(envelope$contract$profile), manifest$profile[index]) &&
      identical(envelope$contract$dgp_digest, config$base_dgp_digest) &&
      identical(envelope$contract$target_digest, config$base_target_digest) &&
      ncol(envelope$result$pred$lower_draws) == 6000L &&
      ncol(envelope$result$pred$upper_draws) == 6000L &&
      nrow(envelope$result$scalar_draws) == 6000L
    data.frame(
      object = manifest$path[index], expected_sha256 = manifest$sha256[index],
      observed_sha256 = if (file.exists(path)) oti_file_sha256(path) else NA_character_,
      bytes = if (file.exists(path)) unname(file.info(path)$size) else NA_real_,
      pass = pass, stringsAsFactors = FALSE
    )
  })
  audit <- do.call(rbind, c(rows, worker_rows))
  if (!all(audit$pass)) {
    oti_stop("The immutable DLM/SH baseline binding failed.")
  }
  list(root = root, audit = audit)
}

otad_prefix_parity <- function(baseline, extended, chain, n_prefix = 6000L) {
  baseline_result <- baseline$result
  extended_result <- extended$result
  objects <- list(
    lower_draws = list(
      baseline_result$pred$lower_draws,
      extended_result$pred$lower_draws[, seq_len(n_prefix), drop = FALSE]
    ),
    upper_draws = list(
      baseline_result$pred$upper_draws,
      extended_result$pred$upper_draws[, seq_len(n_prefix), drop = FALSE]
    ),
    scalar_draws = list(
      baseline_result$scalar_draws,
      extended_result$scalar_draws[seq_len(n_prefix), , drop = FALSE]
    )
  )
  do.call(rbind, lapply(names(objects), function(name) {
    before <- objects[[name]][[1L]]
    after <- objects[[name]][[2L]]
    exact <- identical(before, after)
    data.frame(
      chain = as.integer(chain), object = name,
      baseline_digest = otf_object_sha256(before),
      extended_prefix_digest = otf_object_sha256(after),
      maximum_absolute_difference = if (identical(dim(before), dim(after))) {
        max(abs(before - after))
      } else Inf,
      bitwise_identical = exact, pass = exact,
      stringsAsFactors = FALSE
    )
  }))
}

otad_build_worker_contract <- function(source_commit, config_sha256,
                                       runtime_digest, base_config_sha256,
                                       chain, seed, profile, dgp_digest,
                                       target_digest, mcmc_override,
                                       baseline_row) {
  list(
    schema_version = otad_worker_schema(), source_commit = source_commit,
    adjudication_config_sha256 = config_sha256,
    base_config_sha256 = base_config_sha256,
    runtime_tree_digest = runtime_digest,
    family = "dlm", target = "SH", chain = as.integer(chain),
    seed = as.integer(seed), profile = as.character(profile),
    dgp_digest = dgp_digest, target_digest = target_digest,
    mcmc_override = mcmc_override,
    baseline_worker_sha256 = baseline_row$sha256,
    baseline_contract_digest = baseline_row$contract_digest,
    prefix_draws = as.integer(baseline_row$retained_draws),
    prediction_storage_contract = "ordered_endpoints_only"
  )
}

otad_validate_worker <- function(envelope, expected_contract) {
  expected_draws <- tryCatch(
    otv3_integer_scalar(
      expected_contract$mcmc_override$n_mcmc, "worker retained draws", 1L
    ),
    error = function(error) NA_integer_
  )
  prediction_valid <- is.list(envelope) && is.list(envelope$result) &&
    tryCatch({
      prediction <- envelope$result$pred
      lower <- prediction$lower_draws
      upper <- prediction$upper_draws
      isTRUE(otv3_prediction_storage_contract(prediction)) &&
        is.matrix(lower) && is.numeric(lower) &&
        is.matrix(upper) && is.numeric(upper) &&
        identical(dim(lower), dim(upper)) &&
        ncol(lower) == expected_draws &&
        all(is.finite(lower)) && all(is.finite(upper)) &&
        !any(upper < lower)
    }, error = function(error) FALSE)
  scalar_valid <- is.list(envelope) && is.list(envelope$result) &&
    tryCatch({
      scalar <- envelope$result$scalar_draws
      is.matrix(scalar) && is.numeric(scalar) &&
        nrow(scalar) == expected_draws && ncol(scalar) >= 1L &&
        all(is.finite(scalar))
    }, error = function(error) FALSE)
  provenance_valid <- is.list(envelope) && is.list(envelope$result) &&
    tryCatch({
      audit <- envelope$result$provenance_audit
      otv3_validate_provenance_audit(
        audit, expected_contract$family, expected_contract$target,
        expected_contract$chain
      )
      source_match <- all(
        audit$git_commit == expected_contract$source_commit
      )
      runtime_match <- if (grepl(
        "^[0-9a-f]{64}$", expected_contract$runtime_tree_digest %||% ""
      )) {
        all(
          audit$runtime_package_tree_digest ==
            expected_contract$runtime_tree_digest
        )
      } else TRUE
      source_match && runtime_match
    }, error = function(error) FALSE)
  summary_valid <- is.list(envelope) && is.list(envelope$result) &&
    is.data.frame(envelope$result$chain_summary) &&
    nrow(envelope$result$chain_summary) == 1L &&
    all(vapply(c(
      "worker_entry_provenance_match", "fit_recorded_provenance_match",
      "worker_exit_provenance_match"
    ), function(name) {
      name %in% names(envelope$result$chain_summary) &&
        isTRUE(envelope$result$chain_summary[[name]][1L])
    }, logical(1L)))
  pass <- is.list(envelope) &&
    identical(envelope$schema_version, otad_worker_schema()) &&
    identical(envelope$contract, expected_contract) &&
    identical(envelope$contract_digest, otf_object_sha256(expected_contract)) &&
    identical(
      otf_object_sha256(envelope$contract),
      otf_object_sha256(expected_contract)
    ) &&
    is.list(envelope$result) && prediction_valid && scalar_valid &&
    provenance_valid && summary_valid
  if (!pass) oti_stop("The adjudication worker envelope is invalid.")
  invisible(TRUE)
}

otad_worker_error <- function(chain, stage, error) {
  structure(
    list(
      chain = as.integer(chain), stage = as.character(stage),
      message = conditionMessage(error)
    ),
    class = "otad_worker_error"
  )
}

otad_run_batches <- function(chains, workers, worker, stage = "worker") {
  chains <- as.integer(chains)
  if (!is.function(worker)) oti_stop("worker must be a function.")
  run_one <- function(chain) {
    tryCatch(
      worker(chain),
      error = function(error) otad_worker_error(chain, stage, error)
    )
  }
  batches <- otv3_chain_batches(chains, workers)
  unlist(lapply(batches, function(batch) {
    if (length(batch) > 1L && .Platform$OS.type != "windows") {
      parallel::mclapply(
        batch, run_one, mc.cores = length(batch), mc.preschedule = TRUE,
        mc.set.seed = FALSE
      )
    } else lapply(batch, run_one)
  }), recursive = FALSE, use.names = FALSE)
}

otad_worker_contract_self_test <- function(expected_contract) {
  retained <- expected_contract$mcmc_override$n_mcmc
  state <- as.list(setNames(
    rep(TRUE, length(otv3_provenance_gate_names())),
    otv3_provenance_gate_names()
  ))
  state$git_dirty <- FALSE
  state$git_commit <- expected_contract$source_commit
  state$expected_git_commit <- expected_contract$source_commit
  state$runtime_package_path <- "/synthetic/preflight/rqrgibbs"
  state$runtime_package_version <- "preflight-self-test"
  state$runtime_attestation <- "/synthetic/preflight/attestation.rds"
  state$runtime_attestation_schema <- "preflight-self-test/1.0.0"
  state$source_tree_digest <- strrep("a", 40L)
  state$runtime_package_tree_digest <- if (grepl(
    "^[0-9a-f]{64}$", expected_contract$runtime_tree_digest %||% ""
  )) expected_contract$runtime_tree_digest else strrep("b", 64L)
  audit <- do.call(rbind, lapply(
    c("worker_entry", "fit_recorded", "worker_exit"),
    function(phase) otv3_provenance_snapshot(
      state, phase, expected_contract$family, expected_contract$target,
      expected_contract$chain
    )
  ))
  envelope <- list(
    schema_version = otad_worker_schema(), contract = expected_contract,
    contract_digest = otf_object_sha256(expected_contract),
    result = list(
      pred = otv3_endpoint_only_prediction(list(
        lower_draws = matrix(-1, 1L, retained),
        upper_draws = matrix(1, 1L, retained)
      )),
      scalar_draws = matrix(
        0, retained, 1L, dimnames = list(NULL, "observed_loss")
      ),
      provenance_audit = audit,
      chain_summary = data.frame(
        worker_entry_provenance_match = TRUE,
        fit_recorded_provenance_match = TRUE,
        worker_exit_provenance_match = TRUE
      )
    )
  )
  accepted <- tryCatch({
    otad_validate_worker(envelope, expected_contract)
    TRUE
  }, error = function(error) FALSE)
  malformed <- envelope
  malformed$result$pred$midpoint_draws <- matrix(0, 1L, retained)
  rejected <- inherits(
    try(otad_validate_worker(malformed, expected_contract), silent = TRUE),
    "try-error"
  )
  data.frame(
    gate = c(
      "production_shaped_worker_acceptance",
      "forbidden_prediction_payload_rejection"
    ),
    pass = c(accepted, rejected), stringsAsFactors = FALSE
  )
}

otad_block_stability <- function(chain_results, dgp, targets) {
  truth <- oti_target_row(targets, "SH")
  n_draw <- ncol(chain_results[[1L]]$pred$lower_draws)
  blocks <- list(
    first_half = seq_len(n_draw / 2L),
    second_half = (n_draw / 2L + 1L):n_draw,
    quarter_1 = seq_len(n_draw / 4L),
    quarter_2 = (n_draw / 4L + 1L):(n_draw / 2L),
    quarter_3 = (n_draw / 2L + 1L):(3L * n_draw / 4L),
    quarter_4 = (3L * n_draw / 4L + 1L):n_draw
  )
  rows <- lapply(names(blocks), function(label) {
    index <- blocks[[label]]
    lower <- Reduce(`+`, lapply(chain_results, function(result) {
      rowSums(result$pred$lower_draws[, index, drop = FALSE])
    })) / (length(chain_results) * length(index))
    upper <- Reduce(`+`, lapply(chain_results, function(result) {
      rowSums(result$pred$upper_draws[, index, drop = FALSE])
    })) / (length(chain_results) * length(index))
    curves <- data.frame(
      index = seq_along(lower), fit_lower = lower, fit_upper = upper,
      fit_width = upper - lower, stringsAsFactors = FALSE
    )
    heterogeneity <- otv3_heterogeneity_summary("dlm", curves, truth, dgp)
    cbind(data.frame(block = label, draws_per_chain = length(index)),
          heterogeneity)
  })
  do.call(rbind, rows)
}

otad_decision <- function(cell, prefix_parity) {
  summary <- cell$fit_summary[1L, , drop = FALSE]
  prefix_pass <- nrow(prefix_parity) == 15L && all(prefix_parity$pass)
  strict_pass <- prefix_pass &&
    isTRUE(summary$manuscript_illustration_evidence_eligible) &&
    identical(as.character(summary$disposition), "strict_pass")
  hard_integrity_pass <- prefix_pass && isTRUE(summary$provenance_pass) &&
    isTRUE(summary$provenance_snapshots_pass) &&
    isTRUE(summary$conditional_parity_pass) &&
    isTRUE(summary$pathology_pass) && isTRUE(summary$recovery_pass) &&
    summary$numerical_repair_count == 0L
  data.frame(
    prefix_parity_pass = prefix_pass,
    hard_integrity_pass = hard_integrity_pass,
    strict_diagnostics_pass = isTRUE(summary$strict_diagnostics_pass),
    heterogeneity_pass = isTRUE(summary$heterogeneity_pass),
    strict_pass = strict_pass,
    automatic_promotion_eligible = strict_pass,
    descriptive_review_required = hard_integrity_pass && !strict_pass,
    disposition = if (strict_pass) "strict_pass" else if (hard_integrity_pass) {
      "descriptive_review_required"
    } else "hard_fail",
    stringsAsFactors = FALSE
  )
}
