# Reviewed bounded-cell stage for the RQR-DLM validation runner.
#
# This file is sourced only by 08_run_rqr_dlm_bounded_validation.R after the
# exact source/runtime/config/fixture checks have passed. It handles either one
# representative full four-chain cell or the separately authorized 24-fit
# bounded grid. It is not a matched or production simulation.

if (!mode %in% c("benchmark-one-cell", "execute-bounded")) {
  stop("The bounded-cell stage received an invalid mode.", call. = FALSE)
}

file_sha256 <- function(path) {
  digest::digest(
    file = path, algo = "sha256", serialize = FALSE
  )
}

verify_reference_bundle <- function() {
  reference_dir <- Sys.getenv(
    "RQR_REVIEWED_REFERENCE_DIR", unset = ""
  )
  artifact_manifest_sha <- tolower(Sys.getenv(
    "RQR_REVIEWED_REFERENCE_ARTIFACT_HASHES_SHA256", unset = ""
  ))
  fail <- function(detail) {
    list(verified = FALSE, detail = detail)
  }
  if (!nzchar(reference_dir) || !dir.exists(reference_dir) ||
      !grepl("^[0-9a-f]{64}$", artifact_manifest_sha)) {
    return(fail("reference directory or artifact-manifest SHA is absent"))
  }
  reference_dir <- normalizePath(
    reference_dir, winslash = "/", mustWork = TRUE
  )
  artifact_path <- file.path(reference_dir, "artifact_hashes.csv")
  if (!file.exists(artifact_path) ||
      !identical(file_sha256(artifact_path), artifact_manifest_sha)) {
    return(fail("reference artifact manifest hash does not match"))
  }
  artifacts <- tryCatch(
    utils::read.csv(
      artifact_path, stringsAsFactors = FALSE, check.names = FALSE
    ),
    error = function(error) NULL
  )
  if (!is.data.frame(artifacts) ||
      !identical(names(artifacts), c("sha256", "bytes", "path")) ||
      anyDuplicated(artifacts$path) ||
      any(!grepl("^[0-9a-f]{64}$", artifacts$sha256)) ||
      any(!is.finite(artifacts$bytes)) ||
      any(artifacts$bytes < 0) ||
      any(startsWith(artifacts$path, "/")) ||
      any(grepl("(^|/)\\.\\.(/|$)", artifacts$path))) {
    return(fail("reference artifact manifest schema is invalid"))
  }
  actual_files <- sort(list.files(
    reference_dir, recursive = TRUE, all.files = TRUE,
    include.dirs = FALSE, no.. = TRUE
  ))
  actual_files <- setdiff(actual_files, "artifact_hashes.csv")
  if (!identical(sort(artifacts$path), actual_files)) {
    return(fail("reference artifact file set is incomplete or unexpected"))
  }
  artifact_verified <- vapply(seq_len(nrow(artifacts)), function(index) {
    path <- file.path(reference_dir, artifacts$path[index])
    file.exists(path) &&
      isTRUE(
        as.numeric(file.info(path)$size) ==
          as.numeric(artifacts$bytes[index])
      ) &&
      identical(file_sha256(path), artifacts$sha256[index])
  }, logical(1L))
  if (!all(artifact_verified)) {
    return(fail("one or more reference artifacts failed rehashing"))
  }
  required_files <- c(
    "run_manifest.json", "reference_bundle.json",
    "reference_gates.csv", "resource_summary.csv",
    "runtime_toolchain.json", "session_info.txt",
    "monitor_fault_test.csv", "wrapper_closeout.csv"
  )
  if (any(!required_files %in% artifacts$path)) {
    return(fail("the reference evidence bundle is incomplete"))
  }
  manifest <- tryCatch(
    jsonlite::read_json(
      file.path(reference_dir, "run_manifest.json"),
      simplifyVector = TRUE
    ),
    error = function(error) NULL
  )
  bundle <- tryCatch(
    jsonlite::read_json(
      file.path(reference_dir, "reference_bundle.json"),
      simplifyVector = FALSE
    ),
    error = function(error) NULL
  )
  toolchain <- tryCatch(
    jsonlite::read_json(
      file.path(reference_dir, "runtime_toolchain.json"),
      simplifyVector = TRUE
    ),
    error = function(error) NULL
  )
  gates <- tryCatch(
    utils::read.csv(
      file.path(reference_dir, "reference_gates.csv"),
      stringsAsFactors = FALSE, check.names = FALSE
    ),
    error = function(error) NULL
  )
  resource <- tryCatch(
    utils::read.csv(
      file.path(reference_dir, "resource_summary.csv"),
      stringsAsFactors = FALSE, check.names = FALSE
    ),
    error = function(error) NULL
  )
  bundle_files <- if (is.list(bundle)) bundle$files else NULL
  bundle_file_match <- is.list(bundle_files) &&
    length(bundle_files) > 0L &&
    all(vapply(names(bundle_files), function(relative) {
      path <- file.path(reference_dir, relative)
      file.exists(path) &&
        identical(
          file_sha256(path),
          tolower(as.character(bundle_files[[relative]])[1L])
        )
    }, logical(1L)))
  gate_pass <- is.data.frame(gates) &&
    identical(
      names(gates),
      c("gate", "pass", "value", "tolerance", "detail")
    ) &&
    nrow(gates) > 0L &&
    all(toupper(as.character(gates$pass)) == "TRUE")
  required_resource_metrics <- c(
    "sampled_process_group_peak_processes",
    "sampled_process_group_peak_threads",
    "sampled_process_group_peak_rss_kib",
    "hard_timeout_triggered", "sampled_limit_triggered",
    "final_pgid_empty", "runner_exit_status",
    "monitor_fault_test_pass"
  )
  resource_pass <- is.data.frame(resource) &&
    identical(
      names(resource), c("metric", "value", "limit", "pass")
    ) &&
    all(required_resource_metrics %in% resource$metric) &&
    all(toupper(as.character(resource$pass)) == "TRUE")
  verified <- is.list(manifest) &&
    identical(manifest$schema_version, "rqrgibbs_dlm_bounded_run/2.0.0") &&
    identical(manifest$mode, "reference-only") &&
    identical(manifest$status, "passed") &&
    identical(tolower(manifest$primary_commit), expected_commit) &&
    identical(manifest$config_digest, base_manifest$config_digest) &&
    identical(
      manifest$primary_runtime_tree_digest,
      runtime_state$runtime_package_tree_digest
    ) &&
    identical(
      manifest$primary_runtime_attestation_sha256,
      runtime_contract$primary_runtime_attestation_sha256
    ) &&
    identical(
      manifest$runtime_toolchain_digest, runtime_contract$digest
    ) &&
    identical(
      manifest$reference_gates_sha256,
      file_sha256(file.path(reference_dir, "reference_gates.csv"))
    ) &&
    identical(
      manifest$reference_bundle_sha256,
      file_sha256(file.path(reference_dir, "reference_bundle.json"))
    ) &&
    is.list(bundle) &&
    identical(
      bundle$schema_version, "rqrgibbs_reference_bundle/1.0.0"
    ) &&
    identical(tolower(bundle$primary_commit), expected_commit) &&
    identical(bundle$config_digest, base_manifest$config_digest) &&
    identical(
      bundle$runtime_tree_digest,
      runtime_state$runtime_package_tree_digest
    ) &&
    identical(
      bundle$runtime_attestation_sha256,
      runtime_contract$primary_runtime_attestation_sha256
    ) &&
    identical(bundle$runtime_toolchain_digest, runtime_contract$digest) &&
    is.list(toolchain) &&
    identical(toolchain$digest, runtime_contract$digest) &&
    bundle_file_match && gate_pass && resource_pass
  list(
    verified = isTRUE(verified),
    detail = if (isTRUE(verified)) {
      "complete reference bundle and current runtime/toolchain match"
    } else {
      "reference manifest, gates, resources, lineage, or toolchain mismatch"
    },
    directory = reference_dir,
    artifact_manifest_sha256 = artifact_manifest_sha
  )
}

reference_binding <- verify_reference_bundle()
reviewed_commit <- tolower(Sys.getenv(
  "RQR_REVIEWED_BOUNDED_RUNNER_COMMIT", unset = ""
))
authorization_pass <- identical(reviewed_commit, expected_commit) &&
  isTRUE(reference_binding$verified) &&
  isTRUE(base_manifest$process_tree_monitor_active)
if (identical(mode, "benchmark-one-cell")) {
  authorization_pass <- authorization_pass &&
    isTRUE(config$benchmark_one_cell_authorized) &&
    identical(
      Sys.getenv("RQR_CONFIRM_ONE_CELL_BENCHMARK", unset = ""),
      "I_CONFIRM_ONE_BOUNDED_RQR_DLM_CELL"
    )
} else {
  authorization_pass <- authorization_pass &&
    isTRUE(config$bounded_dynamic_execution_authorized) &&
    identical(
      Sys.getenv(
        "RQR_CONFIRM_BOUNDED_DYNAMIC_EXECUTION", unset = ""
      ),
      "I_CONFIRM_24_BOUNDED_RQR_DLM_FITS"
    )
}
if (!authorization_pass) {
  write_manifest(list(
    status = "blocked_by_execution_contract",
    reference_binding_verified = isTRUE(reference_binding$verified),
    reference_binding_detail = reference_binding$detail,
    bounded_dynamic_execution_authorized = FALSE,
    benchmark_one_cell_authorized =
      config$benchmark_one_cell_authorized
  ))
  stop(
    paste(
      mode, "is disabled. It requires the reviewed full source SHA,",
      "the mode-specific confirmation phrase, a complete hash-verified",
      "reference bundle from the identical runtime/toolchain, and the",
      "corresponding frozen configuration authorization."
    ),
    call. = FALSE
  )
}

append_matrix_estimands <- function(
    values, matrix_value, prefix, time_label = "t") {
  matrix_value <- as.matrix(matrix_value)
  block <- t(matrix_value)
  colnames(block) <- sprintf(
    "%s_%s%03d", prefix, time_label, seq_len(ncol(matrix_value))
  )
  cbind(values, block)
}

chain_estimands <- function(fit, forecast) {
  lower <- pmin(fit$samp.eta_root1, fit$samp.eta_root2)
  upper <- pmax(fit$samp.eta_root1, fit$samp.eta_root2)
  midpoint <- 0.5 * (lower + upper)
  width <- upper - lower
  n_save <- ncol(lower)
  values <- matrix(numeric(0), n_save, 0L)
  values <- append_matrix_estimands(values, lower, "train_lower")
  values <- append_matrix_estimands(values, upper, "train_upper")
  values <- append_matrix_estimands(values, midpoint, "train_midpoint")
  values <- append_matrix_estimands(values, width, "train_width")
  observed <- !is.na(fit$y)
  loss <- vapply(seq_len(n_save), function(draw) {
    sum(rqrgibbs::rqr_check_loss(
      rqrgibbs::rqr_residual_product(
        fit$y[observed],
        fit$samp.eta_root1[observed, draw],
        fit$samp.eta_root2[observed, draw]
      ),
      fit$model_spec$coverage_level
    ))
  }, numeric(1L))
  values <- cbind(values, observed_loss = loss)
  terminal_midpoint <- 0.5 * (
    fit$samp.theta_terminal_root1 +
      fit$samp.theta_terminal_root2
  )
  terminal_separation <- abs(
    fit$samp.theta_terminal_root1 -
      fit$samp.theta_terminal_root2
  )
  terminal_block <- cbind(
    t(terminal_midpoint), t(terminal_separation)
  )
  colnames(terminal_block) <- c(
    paste0(
      "terminal_state_midpoint_",
      seq_len(nrow(terminal_midpoint))
    ),
    paste0(
      "terminal_state_abs_separation_",
      seq_len(nrow(terminal_separation))
    )
  )
  values <- cbind(values, terminal_block)
  if (!is.null(fit$samp.theta0_root1)) {
    theta0_midpoint <- 0.5 * (
      fit$samp.theta0_root1 + fit$samp.theta0_root2
    )
    theta0_separation <- abs(
      fit$samp.theta0_root1 - fit$samp.theta0_root2
    )
    theta0_block <- cbind(
      t(theta0_midpoint), t(theta0_separation)
    )
    colnames(theta0_block) <- c(
      paste0(
        "time0_state_midpoint_", seq_len(nrow(theta0_midpoint))
      ),
      paste0(
        "time0_state_abs_separation_",
        seq_len(nrow(theta0_separation))
      )
    )
    values <- cbind(values, theta0_block)
  }
  values <- append_matrix_estimands(
    values, forecast$lower_draws, "future_lower"
  )
  values <- append_matrix_estimands(
    values, forecast$upper_draws, "future_upper"
  )
  values <- append_matrix_estimands(
    values, forecast$midpoint_draws, "future_midpoint"
  )
  values <- append_matrix_estimands(
    values, forecast$width_draws, "future_width"
  )
  if (identical(
        fit$model_spec$learning_rate_mode, "fixed_rate"
      )) {
    expected_lambda <- fit$model_spec$fixed_learning_rate *
      fit$model_spec$loss_reference_scale
    if (!all(fit$samp.lambda == expected_lambda)) {
      stop("Fixed-rate lambda failed exact identity.", call. = FALSE)
    }
  } else {
    values <- cbind(values, log_lambda = log(fit$samp.lambda))
  }
  if (!is.null(fit$samp.evolution_scale)) {
    q <- log(fit$samp.evolution_scale)
    colnames(q) <- paste0(
      "log_component_scale_", fit$evolution$component_names
    )
    energy <- 2 * sweep(
      fit$samp.evolution_scale_rate, 2L,
      fit$evolution$prior$rate, `-`
    )
    colnames(energy) <- paste0(
      "component_innovation_energy_",
      fit$evolution$component_names
    )
    values <- cbind(values, q, energy)
  }
  if (anyDuplicated(colnames(values)) ||
      any(!is.finite(values))) {
    stop("The explicit estimand schema is duplicated or nonfinite.", call. = FALSE)
  }
  values
}

diagnose_cell <- function(
    cell_chains, fixture_id, learning_rate_mode) {
  schemas <- lapply(cell_chains, colnames)
  if (!all(vapply(
        schemas[-1L], identical, logical(1L), schemas[[1L]]
      ))) {
    stop(
      "A required estimand disappeared or changed order across chains.",
      call. = FALSE
    )
  }
  rows <- lapply(schemas[[1L]], function(variable) {
    matrix_values <- do.call(cbind, lapply(
      cell_chains, function(values) values[, variable]
    ))
    draws <- posterior::as_draws_array(array(
      as.numeric(matrix_values),
      dim = c(nrow(matrix_values), ncol(matrix_values), 1L),
      dimnames = list(
        iteration = NULL,
        chain = paste0("chain", seq_len(ncol(matrix_values))),
        variable = variable
      )
    ))
    data.frame(
      fixture_id = fixture_id,
      learning_rate_mode = learning_rate_mode,
      estimand = variable,
      rhat = unname(posterior::rhat(draws)),
      ess_bulk = unname(posterior::ess_bulk(draws)),
      ess_tail = unname(posterior::ess_tail(draws)),
      mcse_mean = unname(posterior::mcse_mean(draws)),
      stringsAsFactors = FALSE
    )
  })
  diagnostics <- do.call(rbind, rows)
  diagnostics$pass <- with(
    diagnostics,
    is.finite(rhat) & is.finite(ess_bulk) &
      is.finite(ess_tail) & is.finite(mcse_mean) &
      rhat <= config$gates$maximum_rank_normalized_rhat &
      ess_bulk >= config$gates$minimum_bulk_ess &
      ess_tail >= config$gates$minimum_tail_ess
  )
  diagnostics
}

summarize_estimands <- function(
    values, fixture_id, learning_rate_mode, chain) {
  data.frame(
    fixture_id = fixture_id,
    learning_rate_mode = learning_rate_mode,
    chain = chain,
    estimand = colnames(values),
    mean = colMeans(values),
    sd = apply(values, 2L, stats::sd),
    q05 = apply(values, 2L, stats::quantile, probs = 0.05),
    q50 = apply(values, 2L, stats::quantile, probs = 0.50),
    q95 = apply(values, 2L, stats::quantile, probs = 0.95),
    stringsAsFactors = FALSE
  )
}

grid_rows <- list()
grid_index <- 0L
fixture_ids <- if (identical(mode, "benchmark-one-cell")) {
  config$benchmark$fixture_id
} else {
  names(constructed)
}
learning_modes <- if (identical(mode, "benchmark-one-cell")) {
  config$benchmark$learning_rate_mode
} else {
  config$learning_rate_modes
}
for (fixture_id in fixture_ids) {
  for (learning_rate_mode in learning_modes) {
    for (chain in seq_len(config$mcmc$chains)) {
      grid_index <- grid_index + 1L
      grid_rows[[grid_index]] <- data.frame(
        fixture_id = fixture_id,
        learning_rate_mode = learning_rate_mode,
        chain = chain,
        seed = config$mcmc$seeds[chain],
        fit_id = sprintf(
          "%s__%s__chain%02d",
          fixture_id, learning_rate_mode, chain
        ),
        stringsAsFactors = FALSE
      )
    }
  }
}
fit_plan <- do.call(rbind, grid_rows)
atomic_write_csv(fit_plan, file.path(output_dir, "fit_plan.csv"))

initialization_rows <- lapply(seq_len(nrow(fit_plan)), function(index) {
  row <- fit_plan[index, ]
  fixture <- constructed[[row$fixture_id]]
  initial <- rqr_bounded_initialization(
    fixture,
    config$mcmc$initialization_profiles[[row$chain]],
    config$coverage_level
  )
  eta1 <- rqrgibbs:::.rqr_state_ordinates(
    fixture$expanded_model$FF, initial$state_root1
  )
  eta2 <- rqrgibbs:::.rqr_state_ordinates(
    fixture$expanded_model$FF, initial$state_root2
  )
  data.frame(
    fit_id = row$fit_id,
    initialization_profile =
      names(config$mcmc$initialization_profiles)[row$chain],
    initialization_digest = rqrgibbs:::.rqr_digest(initial),
    mean_initial_midpoint = mean(0.5 * (eta1 + eta2)),
    mean_initial_width = mean(abs(eta2 - eta1)),
    lambda_initial = initial$lambda,
    component_scale_initial = if (
        is.null(initial$evolution_scale)) {
      NA_character_
    } else {
      paste(format(initial$evolution_scale, digits = 17), collapse = ",")
    },
    stringsAsFactors = FALSE
  )
})
initialization_manifest <- do.call(rbind, initialization_rows)
if (anyDuplicated(initialization_manifest$initialization_digest[
      fit_plan$fixture_id == fit_plan$fixture_id[1L] &
        fit_plan$learning_rate_mode ==
          fit_plan$learning_rate_mode[1L]
    ])) {
  stop("The four initialization profiles are not distinct.", call. = FALSE)
}
atomic_write_csv(
  initialization_manifest,
  file.path(output_dir, "initialization_manifest.csv")
)

run_status <- transform(
  fit_plan,
  status = "planned",
  started_at = NA_character_,
  ended_at = NA_character_,
  elapsed_seconds = NA_real_,
  message = ""
)
atomic_write_csv(run_status, file.path(output_dir, "run_status.csv"))

fit_audit_rows <- posterior_summary_rows <- future_summary_rows <- list()
conditional_rows <- swap_rows <- provenance_rows <- checkpoint_rows <- list()
chain_hash_rows <- diagnostic_rows <- missing_future_rows <- list()
fit_root <- file.path(output_dir, "full_chains_ignored")
dir.create(fit_root, recursive = TRUE, showWarnings = FALSE)

cell_counter <- 0L
for (fixture_id in fixture_ids) {
  for (learning_rate_mode in learning_modes) {
    cell_counter <- cell_counter + 1L
    cell_rows <- which(
      fit_plan$fixture_id == fixture_id &
        fit_plan$learning_rate_mode == learning_rate_mode
    )
    cell_chains <- vector("list", length(cell_rows))
    for (cell_chain_index in seq_along(cell_rows)) {
      plan_index <- cell_rows[cell_chain_index]
      row <- fit_plan[plan_index, ]
      run_status$status[plan_index] <- "running"
      run_status$started_at[plan_index] <- format(
        Sys.time(), tz = "UTC", usetz = TRUE
      )
      atomic_write_csv(
        run_status, file.path(output_dir, "run_status.csv")
      )
      started <- proc.time()[["elapsed"]]
      result <- tryCatch({
        fixture <- constructed[[fixture_id]]
        fit_arguments <- rqr_bounded_fit_arguments(
          fixture, config, learning_rate_mode, row$chain,
          provenance_control
        )
        fit <- do.call(rqrgibbs::rqr_dlm_fit, fit_arguments)
        target_gate <- isTRUE(fit$model_spec$exact_joint_target) &&
          isTRUE(fit$model_spec$target_numerical_eligible) &&
          identical(fit$model_spec$numerical_repair_count, 0L) &&
          isTRUE(fit$provenance$primary_runtime_source_match) &&
          isTRUE(fit$provenance$reproducibility_eligible)
        if (!target_gate) {
          stop("The fit failed its exact-target or provenance gate.")
        }
        future <- fixture$future
        mode_index <- match(
          learning_rate_mode, config$learning_rate_modes
        )
        forecast_seed <- unname(
          config$seeds$forecast_by_fixture[[fixture_id]]
        ) + 100L * (mode_index - 1L) + row$chain
        forecast_arguments <- list(
          object = fit,
          FF_future = future$FF,
          GG_future = future$GG,
          nd = ncol(fit$samp.eta_root1),
          seed = forecast_seed,
          numerical_policy = "fail"
        )
        if (is.null(future$component_templates)) {
          forecast_arguments$W_future <- future$W
        } else {
          forecast_arguments$component_templates_future <-
            future$component_templates
        }
        forecast <- do.call(
          rqrgibbs::rqr_forecast_roots, forecast_arguments
        )
        if (forecast$diagnostics$repair_count != 0L ||
            any(!is.finite(forecast$lower_draws)) ||
            any(!is.finite(forecast$upper_draws)) ||
            !grepl(
              "no response simulation", forecast$interpretation
            )) {
          stop("The future interval-root gate failed.")
        }
        estimands <- chain_estimands(fit, forecast)
        chain_path <- file.path(
          fit_root, paste0(row$fit_id, ".rds")
        )
        atomic_save_rds(fit, chain_path, compress = "xz")
        list(
          fit = fit, forecast = forecast, estimands = estimands,
          forecast_seed = forecast_seed, chain_path = chain_path
        )
      }, error = function(error) {
        record_failure(
          "fit_and_forecast", error, fixture_id,
          learning_rate_mode, row$chain
        )
        error
      })
      ended <- proc.time()[["elapsed"]]
      run_status$ended_at[plan_index] <- format(
        Sys.time(), tz = "UTC", usetz = TRUE
      )
      run_status$elapsed_seconds[plan_index] <- ended - started
      if (inherits(result, "error")) {
        run_status$status[plan_index] <- "failed"
        run_status$message[plan_index] <- conditionMessage(result)
        atomic_write_csv(
          run_status, file.path(output_dir, "run_status.csv")
        )
        stop(result)
      }
      run_status$status[plan_index] <- "completed"
      atomic_write_csv(
        run_status, file.path(output_dir, "run_status.csv")
      )
      fit <- result$fit
      forecast <- result$forecast
      cell_chains[[cell_chain_index]] <- result$estimands
      fit_audit_rows[[length(fit_audit_rows) + 1L]] <- data.frame(
        fit_id = row$fit_id,
        fixture_id = fixture_id,
        learning_rate_mode = learning_rate_mode,
        chain = row$chain,
        seed = row$seed,
        forecast_seed = result$forecast_seed,
        elapsed_seconds = ended - started,
        chain_bytes = file.info(result$chain_path)$size,
        forecast_repair_count = forecast$diagnostics$repair_count,
        numerical_repair_count =
          fit$model_spec$numerical_repair_count,
        exact_joint_target = fit$model_spec$exact_joint_target,
        target_numerical_eligible =
          fit$model_spec$target_numerical_eligible,
        reproducibility_eligible =
          fit$provenance$reproducibility_eligible,
        promotion_eligible = fit$model_spec$promotion_eligible,
        stringsAsFactors = FALSE
      )
      posterior_summary_rows[[length(posterior_summary_rows) + 1L]] <-
        summarize_estimands(
          result$estimands, fixture_id,
          learning_rate_mode, row$chain
        )
      future_summary_rows[[length(future_summary_rows) + 1L]] <-
        data.frame(
          fit_id = row$fit_id,
          horizon = seq_len(nrow(forecast$lower_draws)),
          lower_mean = rowMeans(forecast$lower_draws),
          upper_mean = rowMeans(forecast$upper_draws),
          midpoint_mean = rowMeans(forecast$midpoint_draws),
          width_mean = rowMeans(forecast$width_draws),
          interpretation = forecast$interpretation,
          stringsAsFactors = FALSE
        )
      missing_future_rows[[length(missing_future_rows) + 1L]] <-
        data.frame(
          fit_id = row$fit_id,
          missing_indices = if (any(!fit$misc$observed)) {
            paste(which(!fit$misc$observed), collapse = ",")
          } else {
            ""
          },
          missing_ordinates_finite = all(is.finite(
            fit$samp.eta_root1[!fit$misc$observed, , drop = FALSE]
          )) && all(is.finite(
            fit$samp.eta_root2[!fit$misc$observed, , drop = FALSE]
          )),
          future_horizon = nrow(forecast$lower_draws),
          future_values_finite =
            all(is.finite(forecast$lower_draws)) &&
              all(is.finite(forecast$upper_draws)),
          future_repair_count =
            forecast$diagnostics$repair_count,
          response_simulation_contract = FALSE,
          stringsAsFactors = FALSE
        )
      if (!is.null(fit$samp.evolution_scale)) {
        conditional_rows[[length(conditional_rows) + 1L]] <-
          data.frame(
            fit_id = row$fit_id,
            draw = rep(
              seq_len(nrow(fit$samp.evolution_scale)),
              each = ncol(fit$samp.evolution_scale)
            ),
            component = rep(
              fit$evolution$component_names,
              times = nrow(fit$samp.evolution_scale)
            ),
            scale = as.vector(t(fit$samp.evolution_scale)),
            posterior_shape = as.vector(t(
              fit$samp.evolution_scale_shape
            )),
            posterior_rate = as.vector(t(
              fit$samp.evolution_scale_rate
            )),
            stringsAsFactors = FALSE
          )
      }
      swap_rows[[length(swap_rows) + 1L]] <- data.frame(
        fit_id = row$fit_id,
        swap_count = sum(fit$diagnostics$root_swap_trace),
        total_iterations = length(fit$diagnostics$root_swap_trace),
        swap_fraction = mean(fit$diagnostics$root_swap_trace),
        role = "sidecar_only_not_mixing_evidence",
        stringsAsFactors = FALSE
      )
      provenance_rows[[length(provenance_rows) + 1L]] <- data.frame(
        fit_id = row$fit_id,
        primary_runtime_source_match =
          fit$provenance$primary_runtime_source_match,
        runtime_tree_digest =
          fit$provenance$primary_runtime_tree_digest,
        expected_commit_match =
          fit$provenance$expected_git_commit_match,
        reproducibility_eligible =
          fit$provenance$reproducibility_eligible,
        stringsAsFactors = FALSE
      )
      checkpoint_rows[[length(checkpoint_rows) + 1L]] <- data.frame(
        fit_id = row$fit_id,
        checkpoint_digest = fit$checkpoint_digest,
        history_digest = fit$continuation_history_digest,
        completed_iterations =
          fit$checkpoint_state$completed_iterations,
        stringsAsFactors = FALSE
      )
      chain_hash_rows[[length(chain_hash_rows) + 1L]] <- data.frame(
        fit_id = row$fit_id,
        relative_path = file.path(
          "full_chains_ignored", basename(result$chain_path)
        ),
        bytes = file.info(result$chain_path)$size,
        sha256 = file_sha256(result$chain_path),
        stringsAsFactors = FALSE
      )
    }
    cell_diagnostics <- diagnose_cell(
      cell_chains, fixture_id, learning_rate_mode
    )
    diagnostic_rows[[length(diagnostic_rows) + 1L]] <-
      cell_diagnostics
    atomic_write_csv(
      do.call(rbind, diagnostic_rows),
      file.path(output_dir, "chain_diagnostics.csv")
    )
    if (identical(mode, "execute-bounded") &&
        !all(cell_diagnostics$pass)) {
      error <- simpleError(paste(
        "Cell-level diagnostics failed for", fixture_id,
        learning_rate_mode, "and later cells were not run."
      ))
      record_failure(
        "cell_diagnostics", error, fixture_id,
        learning_rate_mode, NA_integer_
      )
      stop(error)
    }
  }
}

fit_audit <- do.call(rbind, fit_audit_rows)
posterior_summaries <- do.call(rbind, posterior_summary_rows)
future_summaries <- do.call(rbind, future_summary_rows)
diagnostics <- do.call(rbind, diagnostic_rows)
atomic_write_csv(fit_audit, file.path(output_dir, "fit_audit.csv"))
atomic_write_csv(
  posterior_summaries,
  file.path(output_dir, "posterior_summaries.csv")
)
atomic_write_csv(
  future_summaries,
  file.path(output_dir, "future_root_summaries.csv")
)
atomic_write_csv(
  do.call(rbind, missing_future_rows),
  file.path(output_dir, "missing_future_checks.csv")
)
atomic_write_csv(
  do.call(rbind, swap_rows),
  file.path(output_dir, "root_swap_sidecar.csv")
)
atomic_write_csv(
  do.call(rbind, provenance_rows),
  file.path(output_dir, "provenance_checks.csv")
)
atomic_write_csv(
  do.call(rbind, checkpoint_rows),
  file.path(output_dir, "checkpoint_manifest.csv")
)
atomic_write_csv(
  do.call(rbind, chain_hash_rows),
  file.path(output_dir, "local_chain_hashes.csv")
)
if (length(conditional_rows)) {
  atomic_write_csv(
    do.call(rbind, conditional_rows),
    file.path(output_dir, "component_scale_conditionals.csv")
  )
}

target_pass <- all(fit_audit$numerical_repair_count == 0L) &&
  all(fit_audit$exact_joint_target) &&
  all(fit_audit$target_numerical_eligible) &&
  all(fit_audit$reproducibility_eligible)
diagnostic_pass <- all(diagnostics$pass)
run_pass <- target_pass && (
  identical(mode, "benchmark-one-cell") || diagnostic_pass
)
write_manifest(list(
  status = if (run_pass) "passed" else "failed",
  reference_binding_verified = reference_binding$verified,
  reference_artifact_manifest_sha256 =
    reference_binding$artifact_manifest_sha256,
  benchmark_one_cell = identical(mode, "benchmark-one-cell"),
  bounded_dynamic_execution_authorized =
    identical(mode, "execute-bounded"),
  bounded_fit_count = nrow(fit_audit),
  requested_fit_count = nrow(fit_plan),
  diagnostic_count = nrow(diagnostics),
  diagnostic_pass_count = sum(diagnostics$pass),
  diagnostic_result_is_gate =
    identical(mode, "execute-bounded"),
  full_chain_total_bytes = sum(fit_audit$chain_bytes),
  maximum_fit_elapsed_seconds = max(fit_audit$elapsed_seconds),
  interpretation = paste(
    "Generalized-Bayes interval-root validation;",
    "no posterior-predictive response simulation contract."
  )
))
if (!run_pass) {
  stop("The bounded cell stage failed its required gates.", call. = FALSE)
}
cat(
  if (identical(mode, "benchmark-one-cell")) {
    "Representative one-cell benchmark completed.\n"
  } else {
    "Bounded RQR-DLM 24-fit validation completed.\n"
  }
)
cat("  fits:", nrow(fit_audit), "\n")
cat("  matched or production simulation: no\n")
quit(save = "no", status = 0L)
