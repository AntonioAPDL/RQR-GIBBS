#!/usr/bin/env Rscript

# Prospective ordinary (zero-tilt) RQR validation runner.
#
# The four supported modes are:
#   preflight          source/runtime/configuration/fixture checks only;
#   reference-only     deterministic and tiny reference calculations;
#   benchmark-one-cell one representative four-chain resource benchmark;
#   execute-bounded    the reviewed 48-fit mechanics grid.
#
# execute-bounded is fail-closed in both the tracked configuration and the
# process environment.  This runner never defines or simulates a response
# distribution; its outputs are interval-root generalized-Bayes functionals.

`%||%` <- function(x, y) if (is.null(x)) y else x

rqr_ordinary_v1_schema <- function() {
  "rqrgibbs_ordinary_v1_evidence/1.0.0"
}

rqr_ordinary_v1_find_repo <- function(path = getwd()) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  repeat {
    marker <- file.path(path, "application", "DESCRIPTION")
    if (file.exists(marker)) return(path)
    parent <- dirname(path)
    if (identical(parent, path)) {
      stop("Cannot locate the RQR-GIBBS repository root.", call. = FALSE)
    }
    path <- parent
  }
}

rqr_ordinary_v1_load_config <- function(repo_root) {
  path <- file.path(
    repo_root, "application", "config", "rqr_ordinary_v1",
    "rqr_ordinary_v1_bounded_validation_20260726.R"
  )
  if (!file.exists(path)) stop("Ordinary-v1 configuration is absent.", call. = FALSE)
  env <- new.env(parent = baseenv())
  sys.source(path, envir = env)
  config <- env$rqr_ordinary_v1_bounded_validation
  rqr_ordinary_v1_validate_config(config)
  attr(config, "path") <- normalizePath(path, winslash = "/", mustWork = TRUE)
  config
}

rqr_ordinary_v1_is_integer_scalar <- function(x, minimum = 0) {
  is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x) &&
    x == floor(x) && x >= minimum && x <= .Machine$integer.max
}

rqr_ordinary_v1_protected_dlm_paths <- function() {
  c(
    "application/R/rqr_dlm_fit.R",
    "application/R/rqr_dlm_model.R",
    "application/R/rqr_evolution.R",
    "application/R/rqr_ffbs.R",
    "application/R/rqr_utils.R",
    "application/R/rqr_numerics.R",
    "application/src/rqr_ffbs.cpp",
    paste0(
      "application/config/rqr_dlm/",
      "rqr_dlm_bounded_dynamic_fixtures_20260723.R"
    ),
    paste0(
      "application/config/rqr_dlm/",
      "rqr_dlm_main_simulation_20260724.R"
    ),
    paste0(
      "application/config/rqr_dlm/",
      "rqr_dlm_main_simulation_preliminary_20260724.R"
    ),
    paste0(
      "application/config/rqr_dlm/",
      "rqr_dlm_main_simulation_preliminary_methods_20260724.csv"
    ),
    paste0(
      "application/config/rqr_dlm/",
      "rqr_dlm_main_simulation_preliminary_scenarios_20260724.csv"
    ),
    paste0(
      "application/config/rqr_dlm/",
      "rqr_dlm_output13_bounded_expected_bundle_20260724.json"
    ),
    "application/scripts/15_run_rqr_dlm_confirmatory_simulation.R",
    "application/scripts/15_run_rqr_dlm_confirmatory_simulation.sh",
    "application/scripts/17_launch_rqr_dlm_confirmatory_wave.R",
    "application/DESCRIPTION",
    "application/NAMESPACE",
    "application/R/RcppExports.R",
    "application/src/RcppExports.cpp",
    "application/src/rqr_interweave.cpp",
    "application/src/Makevars",
    "application/src/Makevars.win"
  )
}

rqr_ordinary_v1_validate_config <- function(config) {
  fail <- function(message) stop(message, call. = FALSE)
  if (!is.list(config) ||
      !identical(
        config$schema_version, "rqrgibbs_ordinary_v1_validation/1.0.0"
      )) {
    fail("Unsupported ordinary-v1 configuration schema.")
  }
  if (!identical(
        config$runner_modes,
        c(
          "preflight", "reference-only", "benchmark-one-cell",
          "execute-bounded"
        )
      )) {
    fail("The runner must expose exactly the four reviewed modes.")
  }
  if (!isTRUE(config$generalized_bayes) ||
      isTRUE(config$response_likelihood) ||
      isTRUE(config$response_prediction_contract) ||
      isTRUE(config$matched_simulation_authorized)) {
    fail("The inferential-scope flags are invalid.")
  }
  if (!identical(
        config$accepted_learning_rate_modes,
        c("fixed_rate", "learned_pseudoresidual_normalized")
      )) {
    fail("The accepted learning-rate modes changed.")
  }
  plan <- config$fit_plan
  required_plan <- c(
    "cell_id", "family", "fixture_id", "prior_id",
    "learning_rate_mode", "chain", "seed"
  )
  if (!is.data.frame(plan) ||
      !identical(names(plan), required_plan) ||
      nrow(plan) != 48L ||
      anyDuplicated(plan[, c("cell_id", "chain")]) ||
      any(!plan$learning_rate_mode %in%
            config$accepted_learning_rate_modes) ||
      any(!plan$family %in% c("fixed_design", "desn")) ||
      any(!vapply(plan$chain, rqr_ordinary_v1_is_integer_scalar,
                  logical(1L), minimum = 1)) ||
      any(!vapply(plan$seed, rqr_ordinary_v1_is_integer_scalar,
                  logical(1L), minimum = 0)) ||
      anyDuplicated(plan$seed)) {
    fail("The 48-fit bounded plan is malformed or reuses a seed.")
  }
  cell_sizes <- table(plan$cell_id)
  if (length(cell_sizes) != 12L || any(cell_sizes != 4L) ||
      !identical(sort(unique(plan$chain)), 1:4)) {
    fail("Each bounded cell must contain exactly four chains.")
  }
  cell_contract <- unique(plan[c(
    "cell_id", "family", "fixture_id", "prior_id",
    "learning_rate_mode"
  )])
  rownames(cell_contract) <- NULL
  expected_cells <- data.frame(
    cell_id = c(
      sprintf("S%02d", seq_len(8L)),
      sprintf("DESN%02d", seq_len(4L))
    ),
    family = c(rep("fixed_design", 8L), rep("desn", 4L)),
    fixture_id = c(
      "F03", "F03", "F02", "F02", "F04", "F04", "F05", "F05",
      rep("D02", 4L)
    ),
    prior_id = c(
      "ridge", "ridge", "gaussian", "gaussian",
      "rhs_ns_sampled", "rhs_ns_sampled", "rhs_ns_fixed", "rhs_ns_fixed",
      "ridge", "ridge", "rhs_ns_fixed", "rhs_ns_fixed"
    ),
    learning_rate_mode = rep(
      c("fixed_rate", "learned_pseudoresidual_normalized"), 6L
    ),
    stringsAsFactors = FALSE
  )
  if (!identical(cell_contract, expected_cells)) {
    fail(
      paste(
        "The bounded cell contract changed. D01 is reference-only;",
        "all sixteen DESN fits must use the attested D02 design."
      )
    )
  }
  make_expected_rows <- function(index, seeds) {
    row <- expected_cells[index, , drop = FALSE]
    data.frame(
      cell_id = rep(row$cell_id, 4L),
      family = rep(row$family, 4L),
      fixture_id = rep(row$fixture_id, 4L),
      prior_id = rep(row$prior_id, 4L),
      learning_rate_mode = rep(row$learning_rate_mode, 4L),
      chain = 1:4,
      seed = as.integer(seeds),
      stringsAsFactors = FALSE
    )
  }
  expected_seed_ranges <- list(
    82611:82614, 82621:82624, 82631:82634, 82641:82644,
    82651:82654, 82661:82664, 82671:82674, 82681:82684,
    82711:82714, 82731:82734, 82721:82724, 82741:82744
  )
  expected_plan <- do.call(
    rbind,
    lapply(seq_len(nrow(expected_cells)), function(index) {
      make_expected_rows(index, expected_seed_ranges[[index]])
    })
  )
  rownames(expected_plan) <- NULL
  if (!identical(plan, expected_plan)) {
    fail(
      paste(
        "The 48-fit plan rows, chain order, and seed assignments must match",
        "the frozen reviewed plan exactly."
      )
    )
  }
  benchmark <- config$benchmark_plan
  expected_benchmark <- data.frame(
    cell_id = rep("BENCH01", 4L),
    family = rep("desn", 4L),
    fixture_id = rep("D02", 4L),
    prior_id = rep("rhs_ns_fixed", 4L),
    learning_rate_mode = rep(
      "learned_pseudoresidual_normalized", 4L
    ),
    chain = 1:4,
    seed = 82961:82964,
    stringsAsFactors = FALSE
  )
  if (!is.data.frame(benchmark) ||
      !identical(benchmark, expected_benchmark) ||
      anyDuplicated(rbind(
        plan[, c("cell_id", "chain", "seed")],
        benchmark[, c("cell_id", "chain", "seed")]
      )$seed)) {
    fail("The representative one-cell benchmark contract changed.")
  }
  ledger <- config$seed_ledger
  required_ledger <- c(
    "seed_id", "stage", "purpose", "fixture_id", "prior_id",
    "learning_rate_mode", "chain", "seed"
  )
  if (!is.data.frame(ledger) ||
      !identical(names(ledger), required_ledger) ||
      anyNA(ledger$seed_id) || any(!nzchar(ledger$seed_id)) ||
      anyDuplicated(ledger$seed_id)) {
    fail("The deterministic seed ledger is malformed.")
  }
  used <- ledger$seed[!is.na(ledger$seed)]
  if (any(!vapply(used, rqr_ordinary_v1_is_integer_scalar,
                  logical(1L), minimum = 0)) ||
      anyDuplicated(used) ||
      !identical(sort(as.integer(plan$seed)),
                 sort(as.integer(
                   ledger$seed[ledger$purpose == "bounded_chain"]
                 ))) ||
      !identical(
        sort(as.integer(benchmark$seed)),
        sort(as.integer(
          ledger$seed[ledger$purpose == "benchmark_chain"]
        ))
      )) {
    fail("Every stochastic use must have one unique ledger row.")
  }
  deterministic <- ledger$purpose == "deterministic_oracle"
  if (any(!is.na(ledger$seed[deterministic]))) {
    fail("Deterministic oracles must record NA seeds.")
  }
  expected_bounded_ledger <- transform(
    plan,
    seed_id = sprintf("bounded_%s_chain_%02d", cell_id, chain),
    stage = "execute-bounded",
    purpose = "bounded_chain"
  )[, required_ledger]
  expected_benchmark_ledger <- transform(
    benchmark,
    seed_id = sprintf("benchmark_%s_chain_%02d", cell_id, chain),
    stage = "benchmark-one-cell",
    purpose = "benchmark_chain"
  )[, required_ledger]
  observed_bounded_ledger <- ledger[
    ledger$purpose == "bounded_chain", required_ledger, drop = FALSE
  ]
  observed_benchmark_ledger <- ledger[
    ledger$purpose == "benchmark_chain", required_ledger, drop = FALSE
  ]
  rownames(expected_bounded_ledger) <- NULL
  rownames(expected_benchmark_ledger) <- NULL
  rownames(observed_bounded_ledger) <- NULL
  rownames(observed_benchmark_ledger) <- NULL
  if (!identical(observed_bounded_ledger, expected_bounded_ledger) ||
      !identical(observed_benchmark_ledger, expected_benchmark_ledger)) {
    fail(
      paste(
        "Bounded and benchmark seed-ledger rows must preserve the exact",
        "plan, purpose, stage, chain, and seed mapping."
      )
    )
  }
  mcmc <- config$mcmc
  if (!is.list(mcmc) ||
      !identical(
        names(mcmc),
        c(
          "chains", "burn_in", "retained_per_chain", "thin",
          "numerical_policy", "root_swap_probability",
          "store_latent_draws", "prior_state_draw_storage",
          "sequential_execution", "initialization_profiles"
        )
      ) ||
      !identical(mcmc$chains, 4L) ||
      !identical(mcmc$burn_in, 1000L) ||
      !identical(mcmc$retained_per_chain, 3000L) ||
      !identical(mcmc$thin, 1L) ||
      !identical(mcmc$numerical_policy, "fail") ||
      !identical(mcmc$root_swap_probability, 0.5) ||
      length(mcmc$initialization_profiles) != 4L ||
      !isTRUE(mcmc$sequential_execution) ||
      !identical(mcmc$store_latent_draws, FALSE) ||
      !identical(
        mcmc$prior_state_draw_storage, "rhs_ns_only"
      )) {
    fail("The bounded MCMC schedule changed.")
  }
  if (!identical(
        names(mcmc$initialization_profiles),
        c("low_wide", "high_wide", "low_narrow", "high_narrow")
      ) ||
      any(!vapply(
        mcmc$initialization_profiles,
        function(profile) {
          is.list(profile) &&
            identical(
              names(profile),
              c(
                "midpoint_shift", "half_width", "rhs_scale_multiplier"
              )
            ) &&
            all(vapply(profile, function(value) {
              is.numeric(value) && length(value) == 1L && !is.na(value) &&
                is.finite(value)
            }, logical(1L))) &&
            profile$half_width > 0 && profile$rhs_scale_multiplier > 0
        },
        logical(1L)
      ))) {
    fail("The four overdispersed initialization profiles are malformed.")
  }
  resources <- config$resources
  if (!identical(resources$hard_timeout_minutes, 45L) ||
      !identical(resources$maximum_processes, 3L) ||
      !identical(resources$maximum_threads, 4L) ||
      !identical(resources$maximum_artifact_bytes, 1024^3) ||
      !identical(resources$monitor_interval_seconds, 0.20) ||
      !isTRUE(resources$require_external_process_group_monitor) ||
      !isTRUE(resources$sampled_resource_maxima_are_telemetry) ||
      !identical(resources$automatic_retries, FALSE)) {
    fail("The bounded resource contract changed.")
  }
  gates <- config$gates
  if (!identical(gates$maximum_rank_normalized_rhat, 1.01) ||
      !identical(gates$minimum_bulk_ess, 1000) ||
      !identical(gates$minimum_tail_ess, 1000) ||
      !identical(gates$maximum_numerical_repairs, 0L) ||
      !isTRUE(gates$require_exact_joint_target) ||
      !isTRUE(gates$require_isolated_primary_runtime) ||
      !isTRUE(gates$require_intact_checkpoint) ||
      !isTRUE(gates$require_intact_continuation_history) ||
      !identical(gates$mcse_provider, "posterior_mcse_mean") ||
      !identical(gates$fixed_parameters_gate, "exact_identity") ||
      !identical(gates$root_swap_activity_role, "sidecar_only") ||
      !isTRUE(gates$stop_after_first_failing_four_chain_cell)) {
    fail("The bounded diagnostic gates changed.")
  }
  if (!identical(
        config$pinned_exdqlm$commit,
        "dffb71ee70b597d6a716ee74be1cbc99731cd453"
      ) ||
      !identical(
        config$pinned_exdqlm$branch,
        "feature/rqr-desn-readout-20260716"
      )) {
    fail("The pinned exdqlm reference commit changed.")
  }
  expected_desn_schemas <- c(
    design = "rqrgibbs_desn_design/1.0.0",
    materialization_receipt =
      "rqrgibbs_desn_materialization_receipt/2.0.0",
    materialization_receipt_status =
      "rqrgibbs_desn_materialization_receipt_status/1.0.0",
    materialization_verification =
      "rqrgibbs_desn_materialization_verification/1.0.0",
    fit = "rqrgibbs_desn_fit/1.1.0",
    future_design = "rqrgibbs_desn_future_design/1.1.0",
    future_verification =
      "rqrgibbs_desn_future_verification/1.0.0"
  )
  if (!identical(config$desn_schema_contract, expected_desn_schemas)) {
    fail("The frozen DESN schema contract changed.")
  }
  d02 <- config$fixtures$D02
  if (!is.list(d02) ||
      !rqr_ordinary_v1_is_integer_scalar(
        d02$materializer_seed, minimum = 0
      ) ||
      !is.numeric(d02$response_history) ||
      length(d02$response_history) < 20L ||
      any(!is.finite(d02$response_history)) ||
      !identical(
        names(d02$effective_arguments),
        c("D", "n", "n_tilde", "m", "washout", "seed")
      ) ||
      !identical(
        d02$effective_arguments$seed, d02$materializer_seed
      ) ||
      "fit_readout" %in% names(d02$effective_arguments) ||
      !isTRUE(
        d02$promotion_requires_isolated_attested_exdqlm_runtime
      ) ||
      !identical(
        d02$bounded_grid_role,
        "attested_frozen_design_for_all_16_desn_fits"
      ) ||
      isTRUE(d02$future_extension$response_simulation)) {
    fail("The D02 attested materialization contract is malformed.")
  }
  protected <- config$protected_dlm_sha256
  if (!is.character(protected) ||
      !identical(
        names(protected), rqr_ordinary_v1_protected_dlm_paths()
      ) ||
      anyNA(names(protected)) || any(!nzchar(names(protected))) ||
      anyDuplicated(names(protected)) ||
      any(!grepl("^[0-9a-f]{64}$", protected))) {
    fail("The protected RQR-DLM candidate-integrity contract is malformed.")
  }
  if (!is.character(config$compact_evidence_files) ||
      anyNA(config$compact_evidence_files) ||
      any(!nzchar(config$compact_evidence_files)) ||
      anyDuplicated(config$compact_evidence_files) ||
      !all(c(
        "failure_log.csv", "run_status.csv", "artifact_hashes.csv",
        "closeout.md", "rhs_root_trace_sidecar.csv",
        "fixed_parameter_checks.csv"
      ) %in% config$compact_evidence_files)) {
    fail("The compact evidence contract is malformed.")
  }
  if (isTRUE(config$ordinary_v1_execute_enabled)) {
    commit <- config$reviewed_implementation_commit
    if (!is.character(commit) || length(commit) != 1L ||
        is.na(commit) || !grepl("^[0-9a-f]{40}$", commit)) {
      fail("Enabled execution requires one reviewed implementation SHA.")
    }
  } else if (!is.null(config$reviewed_implementation_commit) &&
             !(length(config$reviewed_implementation_commit) == 1L &&
               is.na(config$reviewed_implementation_commit))) {
    fail(
      paste(
        "Disabled execution must leave reviewed_implementation_commit",
        "unset."
      )
    )
  }
  invisible(TRUE)
}

rqr_ordinary_v1_sha256_object <- function(x) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("The digest package is required.", call. = FALSE)
  }
  digest::digest(x, algo = "sha256", serialize = TRUE)
}

rqr_ordinary_v1_sha256_file <- function(path) {
  if (!file.exists(path)) stop("Cannot hash an absent file.", call. = FALSE)
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

rqr_ordinary_v1_git <- function(repo_root, args) {
  output <- suppressWarnings(system2(
    "git", c("-C", shQuote(repo_root), args),
    stdout = TRUE, stderr = TRUE,
    env = c("GIT_OPTIONAL_LOCKS=0", "GIT_TERMINAL_PROMPT=0")
  ))
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  if (!identical(as.integer(status), 0L)) {
    stop("A read-only Git command failed.", call. = FALSE)
  }
  trimws(paste(output, collapse = "\n"))
}

rqr_ordinary_v1_atomic_file <- function(path, writer, validator) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(paste0(".", basename(path), "-"), tmpdir = dirname(path))
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  writer(temporary)
  if (!file.exists(temporary) || !isTRUE(validator(temporary))) {
    stop(sprintf("Atomic artifact validation failed for %s.", basename(path)),
         call. = FALSE)
  }
  # The temporary file is in the destination directory, so this is a
  # same-filesystem rename. On the Jerez POSIX runtime it replaces an existing
  # regular file atomically; do not unlink the prior evidence first.
  if (!file.rename(temporary, path)) {
    stop(sprintf("Cannot publish %s atomically.", basename(path)), call. = FALSE)
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

rqr_ordinary_v1_atomic_csv <- function(data, path, schema = rqr_ordinary_v1_schema()) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  if (!"schema_version" %in% names(data)) {
    data <- cbind(
      schema_version = rep(schema, nrow(data)),
      data,
      stringsAsFactors = FALSE
    )
  }
  rqr_ordinary_v1_atomic_file(
    path,
    function(tmp) utils::write.csv(data, tmp, row.names = FALSE, na = ""),
    function(tmp) {
      value <- utils::read.csv(
        tmp, stringsAsFactors = FALSE, check.names = FALSE,
        na.strings = c("", "NA")
      )
      nrow(value) == nrow(data) && identical(names(value), names(data))
    }
  )
}

rqr_ordinary_v1_atomic_lines <- function(lines, path) {
  lines <- enc2utf8(as.character(lines))
  rqr_ordinary_v1_atomic_file(
    path,
    function(tmp) writeLines(lines, tmp, useBytes = TRUE),
    function(tmp) identical(readLines(tmp, warn = FALSE), lines)
  )
}

rqr_ordinary_v1_atomic_rds <- function(object, path) {
  digest <- rqr_ordinary_v1_sha256_object(object)
  rqr_ordinary_v1_atomic_file(
    path,
    function(tmp) saveRDS(object, tmp, version = 3),
    function(tmp) identical(
      rqr_ordinary_v1_sha256_object(readRDS(tmp)), digest
    )
  )
}

rqr_ordinary_v1_artifact_manifest <- function(directory) {
  files <- list.files(
    directory, recursive = TRUE, full.names = TRUE, all.files = FALSE
  )
  files <- files[file.info(files)$isdir %in% FALSE]
  files <- files[basename(files) != "artifact_hashes.csv"]
  relative <- substring(files, nchar(normalizePath(
    directory, winslash = "/", mustWork = TRUE
  )) + 2L)
  ordering <- order(relative)
  files <- files[ordering]
  relative <- relative[ordering]
  out <- data.frame(
    relative_path = relative,
    byte_count = as.numeric(file.info(files)$size),
    sha256 = unname(vapply(
      files, rqr_ordinary_v1_sha256_file, character(1L)
    )),
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  out
}

rqr_ordinary_v1_validate_artifact_manifest <- function(directory, manifest) {
  actual <- rqr_ordinary_v1_artifact_manifest(directory)
  required <- c("relative_path", "byte_count", "sha256")
  is.data.frame(manifest) &&
    all(required %in% names(manifest)) &&
    identical(
      as.character(actual$relative_path),
      as.character(manifest$relative_path)
    ) &&
    identical(
      as.numeric(actual$byte_count),
      as.numeric(manifest$byte_count)
    ) &&
    identical(as.character(actual$sha256), as.character(manifest$sha256))
}

rqr_ordinary_v1_build_fixtures <- function(config) {
  get_public <- function(name) {
    if (!requireNamespace("rqrgibbs", quietly = TRUE)) {
      stop("The rqrgibbs namespace is required to construct fixtures.",
           call. = FALSE)
    }
    getExportedValue("rqrgibbs", name)
  }
  sha <- rqr_ordinary_v1_sha256_object
  specification <- config$fixtures$D01
  design <- get_public("rqr_desn_design")(
    X = specification$X,
    y = specification$y,
    time_index = specification$time_index,
    intercept = specification$intercept,
    builder = specification$builder,
    reservoir = list(
      digest = sha(list(
        seed = specification$reservoir_seed,
        depth = specification$reservoir_depth,
        feature_names = colnames(specification$X)
      )),
      seed = specification$reservoir_seed,
      depth = specification$reservoir_depth
    ),
    driver = list(
      type = "observed_history",
      response_simulation = FALSE,
      history_digest = sha(specification$y)
    ),
    causal = specification$causal,
    terminal = list(
      available = TRUE,
      state_digest = sha(tail(specification$X[, -1L, drop = FALSE], 1L)),
      lag_buffer_digest = sha(tail(
        specification$y[!is.na(specification$y)], 2L
      ))
    )
  )
  future_driver <- list(
    precomputed_design = list(source = "frozen_validation_matrix"),
    teacher_forced_one_step = list(
      path_digest = sha(c(0.4, 0.5))
    ),
    external_driver_path = list(
      path_digest = sha(c(0.25, 0.30)),
      generator_id = "separate_validation_driver"
    )
  )
  future <- lapply(config$fixtures$D03$semantics, function(semantics) {
    get_public("rqr_desn_future_design")(
      parent_design = design,
      X = specification$future_X,
      time_index = specification$future_time_index,
      semantics = semantics,
      driver = future_driver[[semantics]]
    )
  })
  names(future) <- config$fixtures$D03$semantics
  list(
    F01 = config$fixtures$F01,
    F02 = config$fixtures$F02,
    F03 = config$fixtures$F03,
    F04 = config$fixtures$F04,
    F05 = config$fixtures$F05,
    D01 = design,
    D03 = future
  )
}

rqr_ordinary_v1_fixture_manifest <- function(fixtures) {
  data.frame(
    fixture_id = names(fixtures),
    class = vapply(
      fixtures, function(x) paste(class(x), collapse = "|"), character(1L)
    ),
    digest = vapply(fixtures, rqr_ordinary_v1_sha256_object, character(1L)),
    stringsAsFactors = FALSE
  )
}

rqr_ordinary_v1_prior <- function(config, prior_id) {
  constructor <- getExportedValue("rqrgibbs", "rqr_beta_prior")
  switch(
    prior_id,
    ridge = constructor(
      "ridge", ridge = list(tau2 = config$fixtures$F03$ridge$tau2)
    ),
    gaussian = constructor(
      "gaussian", gaussian = config$fixtures$F02$prior[-1L]
    ),
    rhs_ns_sampled = constructor(
      "rhs_ns", rhs_ns = config$fixtures$F04$prior[-1L]
    ),
    rhs_ns_fixed = constructor(
      "rhs_ns", rhs_ns = config$fixtures$F05$prior[-1L]
    ),
    stop(sprintf("Unknown prior_id %s.", prior_id), call. = FALSE)
  )
}

rqr_ordinary_v1_cell_fixture <- function(
    config, fixtures, cell, execution_design = NULL) {
  prior_id <- cell$prior_id[[1L]]
  if (identical(cell$family[[1L]], "desn")) {
    design <- execution_design %||% fixtures$D01
    getExportedValue("rqrgibbs", "rqr_validate_desn_design")(design)
    return(list(
      family = "desn", design = design,
      X = design$X, y = design$y,
      prior = rqr_ordinary_v1_prior(config, prior_id)
    ))
  }
  fixture <- switch(
    prior_id,
    ridge = config$fixtures$F03,
    gaussian = config$fixtures$F02,
    rhs_ns_sampled = config$fixtures$F04,
    rhs_ns_fixed = config$fixtures$F05
  )
  list(
    family = "fixed_design", X = fixture$X, y = fixture$y,
    prior = rqr_ordinary_v1_prior(config, prior_id)
  )
}

rqr_ordinary_v1_bounded_future_design <- function(
    design, config) {
  getExportedValue("rqrgibbs", "rqr_validate_desn_design")(design)
  contract <- config$fixtures$D02$future_extension
  horizon <- contract$horizon
  if (!rqr_ordinary_v1_is_integer_scalar(horizon, minimum = 1) ||
      !identical(contract$semantics, "precomputed_design") ||
      isTRUE(contract$response_simulation)) {
    stop("The D02 bounded future-extension contract is invalid.",
         call. = FALSE)
  }
  last <- design$X[nrow(design$X), , drop = FALSE]
  X <- last[rep(1L, horizon), , drop = FALSE]
  intercept <- design$feature_schema$intercept
  active <- seq_len(ncol(X))
  if (isTRUE(intercept$present)) {
    active <- setdiff(active, intercept$index)
  }
  if (length(active)) {
    offsets <- outer(
      seq_len(horizon), seq_along(active),
      function(step, feature) step * feature * 0.01
    )
    scale <- pmax(abs(last[1L, active]), 1)
    X[, active] <- X[, active, drop = FALSE] +
      sweep(offsets, 2L, scale, "*")
  }
  if (isTRUE(intercept$present)) {
    X[, intercept$index] <- 1
  }
  getExportedValue("rqrgibbs", "rqr_desn_future_design")(
    parent_design = design,
    X = X,
    time_index = max(design$time_index) + seq_len(horizon),
    semantics = contract$semantics,
    driver = list(
      source = contract$role,
      construction_digest = rqr_ordinary_v1_sha256_object(list(
        parent = design$semantic_digest,
        horizon = horizon,
        algorithm =
          "last_row_plus_deterministic_feature_scaled_offsets_v1"
      ))
    )
  )
}

rqr_ordinary_v1_retain_prior_state_draws <- function(config, prior) {
  policy <- config$mcmc$prior_state_draw_storage
  if (!identical(policy, "rhs_ns_only") ||
      !is.list(prior) ||
      !is.character(prior$type) ||
      length(prior$type) != 1L ||
      is.na(prior$type) ||
      !prior$type %in% c("ridge", "gaussian", "rhs_ns")) {
    stop("The prior-state draw-storage contract is invalid.",
         call. = FALSE)
  }
  identical(prior$type, "rhs_ns")
}

rqr_ordinary_v1_initial_state <- function(X, profile, prior) {
  if (!is.matrix(X) || !is.numeric(X) || anyNA(X) ||
      any(!is.finite(X)) || !ncol(X) ||
      is.null(colnames(X)) || anyNA(colnames(X)) ||
      any(!nzchar(colnames(X))) || anyDuplicated(colnames(X)) ||
      !is.list(profile) ||
      !identical(
        names(profile),
        c("midpoint_shift", "half_width", "rhs_scale_multiplier")
      ) ||
      any(!vapply(profile, function(value) {
        is.numeric(value) && length(value) == 1L &&
          !is.na(value) && is.finite(value)
      }, logical(1L))) ||
      profile$half_width <= 0 ||
      profile$rhs_scale_multiplier <= 0 ||
      !is.list(prior) ||
      !is.character(prior$type) ||
      length(prior$type) != 1L ||
      is.na(prior$type) ||
      !prior$type %in% c("ridge", "gaussian", "rhs_ns")) {
    stop("The ordinary-v1 initial-state inputs are invalid.",
         call. = FALSE)
  }
  p <- ncol(X)
  beta1 <- beta2 <- numeric(p)
  intercept_name <- if (identical(prior$type, "rhs_ns")) {
    prior$hypers$intercept_name
  } else {
    "intercept"
  }
  intercept <- match(intercept_name, colnames(X))
  if (!is.character(intercept_name) ||
      length(intercept_name) != 1L ||
      is.na(intercept_name) ||
      !nzchar(intercept_name) ||
      is.na(intercept) ||
      !all(X[, intercept] == 1)) {
    stop(
      paste(
        "The ordinary-v1 initial-state design must contain its exact",
        "declared intercept column, constant and equal to one."
      ),
      call. = FALSE
    )
  }
  beta1[intercept] <- profile$midpoint_shift - profile$half_width
  beta2[intercept] <- profile$midpoint_shift + profile$half_width
  init <- list(
    beta_root1 = beta1,
    beta_root2 = beta2
  )
  if (!identical(prior$type, "rhs_ns")) return(init)

  if (is.null(prior$hypers) ||
      !is.character(prior$hypers$intercept_name) ||
      length(prior$hypers$intercept_name) != 1L ||
      is.na(prior$hypers$intercept_name) ||
      is.na(match(prior$hypers$intercept_name, colnames(X))) ||
      !is.numeric(prior$hypers$tau0) ||
      length(prior$hypers$tau0) != 1L ||
      is.na(prior$hypers$tau0) ||
      !is.finite(prior$hypers$tau0) ||
      prior$hypers$tau0 <= 0) {
    stop("The RHS-NS prior is not compatible with the initial-state design.",
         call. = FALSE)
  }
  active <- setdiff(seq_len(p), intercept)
  active_names <- colnames(X)[active]
  make_rhs_state <- function(multiplier) {
    state <- list(
      lambda2 = stats::setNames(rep(multiplier, length(active)), active_names),
      nu = stats::setNames(rep(1 / multiplier, length(active)), active_names),
      tau2 = prior$hypers$tau0^2 * multiplier,
      xi = 1 / multiplier
    )
    if (is.null(prior$hypers$zeta2_fixed)) {
      state$zeta2 <- multiplier
    }
    state$update_count <- 0L
    state$numerical_repair_count <- 0L
    state
  }
  multiplier <- profile$rhs_scale_multiplier
  init$beta_prior_state1 <- make_rhs_state(multiplier)
  init$beta_prior_state2 <- make_rhs_state(1 / multiplier)
  init
}

rqr_ordinary_v1_validate_desn_prediction <- function(
    prediction, fit, schema_contract) {
  if (!inherits(fit, "rqr_desn_fit") ||
      !identical(fit$schema_version, schema_contract[["fit"]]) ||
      !is.list(prediction) ||
      !inherits(
        prediction$future_design, "rqr_desn_future_design"
      ) ||
      !identical(
        prediction$future_design$schema_version,
        schema_contract[["future_design"]]
      ) ||
      !identical(
        prediction$future_design$verification$schema_version,
        schema_contract[["future_verification"]]
      ) ||
      !isTRUE(
        prediction$future_design$verification$contract_verified
      ) ||
      !identical(
        prediction$future_design$verification$legacy_explicit_matrix,
        FALSE
      ) ||
      !isTRUE(prediction$future_contract_verified) ||
      !identical(prediction$legacy_future_matrix, FALSE) ||
      !identical(
        prediction$
          parent_design_materialization_external_binding_verified,
        TRUE
      ) ||
      !identical(
        prediction$parent_fit_reproducibility_eligible, TRUE
      ) ||
      !identical(prediction$parent_fit_promotion_eligible, TRUE) ||
      !identical(
        prediction$future_external_provenance_bound, FALSE
      ) ||
      !identical(
        prediction$future_reproducibility_eligible, FALSE
      ) ||
      !identical(prediction$reproducibility_eligible, FALSE) ||
      !identical(prediction$promotion_eligible, FALSE) ||
      !identical(
        prediction$promotion_status,
        "verified_future_contract_unattested_materialization"
      ) ||
      !identical(prediction$response_predictive_draws, FALSE) ||
      !is.matrix(prediction$lower_draws) ||
      !is.matrix(prediction$upper_draws) ||
      !identical(dim(prediction$lower_draws),
                 dim(prediction$upper_draws)) ||
      any(!is.finite(prediction$lower_draws)) ||
      any(!is.finite(prediction$upper_draws))) {
    stop(
      paste(
        "A DESN future evaluation does not preserve the verified",
        "conditional-root contract, parent-fit sidecars, and explicit",
        "future-materialization nonpromotion boundary."
      ),
      call. = FALSE
    )
  }
  data.frame(
    fit_schema = fit$schema_version,
    future_design_schema =
      prediction$future_design$schema_version,
    future_verification_schema =
      prediction$future_design$verification$schema_version,
    future_contract_verified =
      prediction$future_contract_verified,
    legacy_future_matrix = prediction$legacy_future_matrix,
    parent_design_materialization_external_binding_verified =
      prediction$
        parent_design_materialization_external_binding_verified,
    parent_fit_reproducibility_eligible =
      prediction$parent_fit_reproducibility_eligible,
    parent_fit_promotion_eligible =
      prediction$parent_fit_promotion_eligible,
    future_external_provenance_bound =
      prediction$future_external_provenance_bound,
    future_reproducibility_eligible =
      prediction$future_reproducibility_eligible,
    future_promotion_eligible = prediction$promotion_eligible,
    future_promotion_status = prediction$promotion_status,
    response_predictive_draws =
      prediction$response_predictive_draws,
    future_design_digest =
      prediction$future_design$semantic_digest,
    status = "pass",
    stringsAsFactors = FALSE
  )
}

rqr_ordinary_v1_expected_estimand_columns <- function(
    family, learning_rate_mode, prior_id, n_training,
    coefficient_names, horizon = 0L, rhs_active_names = character(0)) {
  choices <- list(
    family = c("fixed_design", "desn"),
    learning_rate_mode = c(
      "fixed_rate", "learned_pseudoresidual_normalized"
    ),
    prior_id = c(
      "ridge", "gaussian", "rhs_ns_sampled", "rhs_ns_fixed"
    )
  )
  values <- list(
    family = family,
    learning_rate_mode = learning_rate_mode,
    prior_id = prior_id
  )
  for (name in names(choices)) {
    value <- values[[name]]
    if (!is.character(value) || length(value) != 1L ||
        is.na(value) || !value %in% choices[[name]]) {
      stop(sprintf("The estimand-schema %s is invalid.", name),
           call. = FALSE)
    }
  }
  if (!rqr_ordinary_v1_is_integer_scalar(n_training, minimum = 1L) ||
      !rqr_ordinary_v1_is_integer_scalar(horizon, minimum = 0L)) {
    stop("Estimand-schema dimensions must be nonnegative integers.",
         call. = FALSE)
  }
  n_training <- as.integer(n_training)
  horizon <- as.integer(horizon)
  if ((identical(family, "fixed_design") && horizon != 0L) ||
      (identical(family, "desn") && horizon < 1L)) {
    stop("The family and future-horizon estimand contract is invalid.",
         call. = FALSE)
  }
  if (!is.character(coefficient_names) ||
      !length(coefficient_names) || anyNA(coefficient_names) ||
      any(!nzchar(coefficient_names)) ||
      anyDuplicated(coefficient_names)) {
    stop("Estimand schemas require unique nonempty coefficient names.",
         call. = FALSE)
  }
  rhs_prior <- startsWith(prior_id, "rhs_ns")
  if (rhs_prior) {
    if (!is.character(rhs_active_names) ||
        !length(rhs_active_names) || anyNA(rhs_active_names) ||
        any(!nzchar(rhs_active_names)) ||
        anyDuplicated(rhs_active_names) ||
        !all(rhs_active_names %in% coefficient_names)) {
      stop(
        "RHS estimand schemas require exact named active coefficients.",
        call. = FALSE
      )
    }
  } else if (length(rhs_active_names)) {
    stop("Non-RHS estimand schemas cannot declare RHS active names.",
         call. = FALSE)
  }
  names_by_row <- function(prefix, n) {
    sprintf("%s_t%03d", prefix, seq_len(n))
  }
  columns <- c(
    names_by_row("lower", n_training),
    names_by_row("upper", n_training),
    names_by_row("midpoint", n_training),
    names_by_row("width", n_training),
    "observed_loss",
    paste0("beta_midpoint_", coefficient_names),
    paste0("beta_abs_separation_", coefficient_names)
  )
  if (identical(
        learning_rate_mode, "learned_pseudoresidual_normalized"
      )) {
    columns <- c(columns, "log_lambda")
  }
  if (rhs_prior) {
    columns <- c(
      columns,
      "log_tau2_ordered_lower", "log_tau2_ordered_upper"
    )
    if (identical(prior_id, "rhs_ns_sampled")) {
      columns <- c(
        columns,
        "log_zeta2_ordered_lower", "log_zeta2_ordered_upper"
      )
    }
    columns <- c(
      columns,
      paste0("log_lambda2_ordered_lower_", rhs_active_names),
      paste0("log_lambda2_ordered_upper_", rhs_active_names)
    )
  }
  if (identical(family, "desn")) {
    columns <- c(
      columns,
      names_by_row("future_lower", horizon),
      names_by_row("future_upper", horizon),
      names_by_row("future_midpoint", horizon),
      names_by_row("future_width", horizon)
    )
  }
  if (anyDuplicated(columns)) {
    stop("The expected estimand-column schema is not unique.",
         call. = FALSE)
  }
  columns
}

rqr_ordinary_v1_empty_fixed_parameter_checks <- function() {
  data.frame(
    parameter = character(0),
    root = character(0),
    expected_value = numeric(0),
    retained_draws = integer(0),
    exact_identity = logical(0),
    status = character(0),
    stringsAsFactors = FALSE
  )
}

rqr_ordinary_v1_expected_rhs_sidecar_columns <- function(
    prior_id, active_names) {
  if (!is.character(prior_id) || length(prior_id) != 1L ||
      is.na(prior_id) ||
      !prior_id %in% c("rhs_ns_sampled", "rhs_ns_fixed") ||
      !is.character(active_names) || !length(active_names) ||
      anyNA(active_names) || any(!nzchar(active_names)) ||
      anyDuplicated(active_names)) {
    stop("The RHS root-sidecar schema request is invalid.",
         call. = FALSE)
  }
  c(
    "log_tau2_root1", "log_tau2_root2",
    if (identical(prior_id, "rhs_ns_sampled")) {
      c("log_zeta2_root1", "log_zeta2_root2")
    },
    paste0("log_lambda2_", active_names, "_root1"),
    paste0("log_lambda2_", active_names, "_root2")
  )
}

rqr_ordinary_v1_rhs_diagnostic_blocks <- function(static) {
  states1 <- static$samp.beta_prior_state_root1
  states2 <- static$samp.beta_prior_state_root2
  n_draws <- nrow(static$samp.beta_root1)
  if (!is.list(states1) || !is.list(states2) ||
      length(states1) != n_draws || length(states2) != n_draws ||
      !identical(static$beta_prior$type, "rhs_ns")) {
    stop("RHS diagnostic states are missing or dimensionally invalid.",
         call. = FALSE)
  }
  active_names <- static$beta_prior$design_contract$column_names[
    static$beta_prior$design_contract$active_index
  ]
  validate_states <- function(states) {
    all(vapply(states, function(state) {
      is.list(state) &&
        identical(state$active_names, active_names) &&
        identical(names(state$lambda2), active_names) &&
        length(state$lambda2) == length(active_names) &&
        all(is.finite(state$lambda2)) && all(state$lambda2 > 0) &&
        is.numeric(state$tau2) && length(state$tau2) == 1L &&
        is.finite(state$tau2) && state$tau2 > 0 &&
        is.numeric(state$zeta2) && length(state$zeta2) == 1L &&
        is.finite(state$zeta2) && state$zeta2 > 0
    }, logical(1L)))
  }
  if (!length(active_names) || anyNA(active_names) ||
      any(!nzchar(active_names)) || anyDuplicated(active_names) ||
      !validate_states(states1) || !validate_states(states2)) {
    stop(
      "RHS retained states do not preserve named active coefficients.",
      call. = FALSE
    )
  }
  extract_scalar <- function(states, field) {
    log(vapply(states, `[[`, numeric(1L), field))
  }
  extract_local <- function(states) {
    out <- do.call(rbind, lapply(states, function(state) {
      log(state$lambda2)
    }))
    colnames(out) <- active_names
    out
  }
  tau1 <- extract_scalar(states1, "tau2")
  tau2 <- extract_scalar(states2, "tau2")
  local1 <- extract_local(states1)
  local2 <- extract_local(states2)
  primary <- cbind(
    log_tau2_ordered_lower = pmin(tau1, tau2),
    log_tau2_ordered_upper = pmax(tau1, tau2)
  )
  sidecar <- cbind(
    log_tau2_root1 = tau1,
    log_tau2_root2 = tau2
  )
  fixed_checks <- rqr_ordinary_v1_empty_fixed_parameter_checks()
  fixed <- static$beta_prior$hypers$zeta2_fixed
  if (is.null(fixed)) {
    zeta1 <- extract_scalar(states1, "zeta2")
    zeta2 <- extract_scalar(states2, "zeta2")
    primary <- cbind(
      primary,
      log_zeta2_ordered_lower = pmin(zeta1, zeta2),
      log_zeta2_ordered_upper = pmax(zeta1, zeta2)
    )
    sidecar <- cbind(
      sidecar,
      log_zeta2_root1 = zeta1,
      log_zeta2_root2 = zeta2
    )
  } else {
    identity <- c(
      root1 = all(vapply(states1, function(state) {
        identical(as.numeric(state$zeta2), as.numeric(fixed)) &&
          identical(state$zeta2_is_fixed, TRUE)
      }, logical(1L))),
      root2 = all(vapply(states2, function(state) {
        identical(as.numeric(state$zeta2), as.numeric(fixed)) &&
          identical(state$zeta2_is_fixed, TRUE)
      }, logical(1L)))
    )
    if (!all(identity)) {
      stop(
        "Fixed RHS zeta2 failed exact identity in retained states.",
        call. = FALSE
      )
    }
    fixed_checks <- data.frame(
      parameter = rep("zeta2_fixed", 2L),
      root = names(identity),
      expected_value = rep(as.numeric(fixed), 2L),
      retained_draws = rep(as.integer(n_draws), 2L),
      exact_identity = unname(identity),
      status = rep("pass", 2L),
      stringsAsFactors = FALSE
    )
  }
  local_lower <- pmin(local1, local2)
  local_upper <- pmax(local1, local2)
  colnames(local_lower) <- paste0(
    "log_lambda2_ordered_lower_", active_names
  )
  colnames(local_upper) <- paste0(
    "log_lambda2_ordered_upper_", active_names
  )
  local_sidecar1 <- local1
  local_sidecar2 <- local2
  colnames(local_sidecar1) <- paste0(
    "log_lambda2_", active_names, "_root1"
  )
  colnames(local_sidecar2) <- paste0(
    "log_lambda2_", active_names, "_root2"
  )
  primary <- cbind(primary, local_lower, local_upper)
  sidecar <- cbind(sidecar, local_sidecar1, local_sidecar2)
  if (anyDuplicated(colnames(primary)) ||
      anyDuplicated(colnames(sidecar)) ||
      any(!is.finite(primary)) || any(!is.finite(sidecar))) {
    stop("RHS diagnostic blocks are malformed.", call. = FALSE)
  }
  list(
    primary = primary,
    root_specific_sidecar = sidecar,
    fixed_parameter_checks = fixed_checks,
    active_names = active_names
  )
}

rqr_ordinary_v1_chain_estimands <- function(
    fit, X, y, learning_rate_mode, future_design = NULL,
    schema_contract = NULL, expected_family = NULL,
    expected_prior_id = NULL) {
  static <- if (inherits(fit, "rqr_desn_fit")) fit$fit else fit
  family <- if (inherits(fit, "rqr_desn_fit")) "desn" else "fixed_design"
  prior_id <- if (identical(static$beta_prior$type, "rhs_ns")) {
    if (is.null(static$beta_prior$hypers$zeta2_fixed)) {
      "rhs_ns_sampled"
    } else {
      "rhs_ns_fixed"
    }
  } else {
    static$beta_prior$type
  }
  if (!is.null(expected_family) ||
      !is.null(expected_prior_id)) {
    if (!identical(family, expected_family) ||
        !identical(prior_id, expected_prior_id)) {
      stop("The fitted family/prior does not match its frozen plan row.",
           call. = FALSE)
    }
  }
  beta1 <- as.matrix(static$samp.beta_root1)
  beta2 <- as.matrix(static$samp.beta_root2)
  eta1 <- beta1 %*% t(X)
  eta2 <- beta2 %*% t(X)
  lower <- pmin(eta1, eta2)
  upper <- pmax(eta1, eta2)
  midpoint <- 0.5 * (lower + upper)
  width <- upper - lower
  names_by_row <- function(prefix, n) {
    sprintf("%s_t%03d", prefix, seq_len(n))
  }
  values <- cbind(lower, upper, midpoint, width)
  colnames(values) <- c(
    names_by_row("lower", nrow(X)),
    names_by_row("upper", nrow(X)),
    names_by_row("midpoint", nrow(X)),
    names_by_row("width", nrow(X))
  )
  observed <- !is.na(y)
  check_loss <- getExportedValue("rqrgibbs", "rqr_check_loss")
  residual_product <- getExportedValue("rqrgibbs", "rqr_residual_product")
  loss <- vapply(seq_len(nrow(values)), function(index) {
    sum(check_loss(
      residual_product(
        y[observed], eta1[index, observed], eta2[index, observed]
      ),
      static$model_spec$coverage_level
    ))
  }, numeric(1L))
  coefficient_midpoint <- 0.5 * (beta1 + beta2)
  coefficient_separation <- abs(beta1 - beta2)
  colnames(coefficient_midpoint) <- paste0(
    "beta_midpoint_", colnames(X)
  )
  colnames(coefficient_separation) <- paste0(
    "beta_abs_separation_", colnames(X)
  )
  values <- cbind(
    values, observed_loss = loss,
    coefficient_midpoint, coefficient_separation
  )
  if (identical(
        learning_rate_mode, "learned_pseudoresidual_normalized"
      )) {
    values <- cbind(values, log_lambda = log(static$samp.lambda))
  } else {
    expected <- static$model_spec$fixed_learning_rate *
      static$model_spec$loss_reference_scale
    if (!all(static$samp.lambda == expected)) {
      stop("Fixed-rate lambda failed exact identity.", call. = FALSE)
    }
  }
  rhs_sidecar <- matrix(
    numeric(0), nrow = nrow(values), ncol = 0L
  )
  fixed_parameter_checks <-
    rqr_ordinary_v1_empty_fixed_parameter_checks()
  rhs_active_names <- character(0)
  if (identical(static$beta_prior$type, "rhs_ns")) {
    rhs <- rqr_ordinary_v1_rhs_diagnostic_blocks(static)
    values <- cbind(values, rhs$primary)
    rhs_sidecar <- rhs$root_specific_sidecar
    fixed_parameter_checks <- rhs$fixed_parameter_checks
    rhs_active_names <- rhs$active_names
  }
  future_check <- NULL
  if (!is.null(future_design)) {
    forecast <- getExportedValue("rqrgibbs", "forecast_paths")(
      fit, future_design = future_design
    )
    future_check <- rqr_ordinary_v1_validate_desn_prediction(
      forecast, fit, schema_contract
    )
    future_lower <- t(forecast$lower_draws)
    future_upper <- t(forecast$upper_draws)
    future_midpoint <- 0.5 * (future_lower + future_upper)
    future_width <- future_upper - future_lower
    colnames(future_lower) <- names_by_row(
      "future_lower", ncol(future_lower)
    )
    colnames(future_upper) <- names_by_row(
      "future_upper", ncol(future_upper)
    )
    colnames(future_midpoint) <- names_by_row(
      "future_midpoint", ncol(future_midpoint)
    )
    colnames(future_width) <- names_by_row(
      "future_width", ncol(future_width)
    )
    values <- cbind(
      values, future_lower, future_upper, future_midpoint, future_width
    )
  }
  horizon <- if (is.null(future_design)) 0L else ncol(future_lower)
  expected_columns <- rqr_ordinary_v1_expected_estimand_columns(
    family = family,
    learning_rate_mode = learning_rate_mode,
    prior_id = prior_id,
    n_training = nrow(X),
    coefficient_names = colnames(X),
    horizon = horizon,
    rhs_active_names = rhs_active_names
  )
  if (!identical(colnames(values), expected_columns) ||
      anyDuplicated(colnames(values)) || any(!is.finite(values))) {
    stop("The label-invariant estimand matrix is invalid.", call. = FALSE)
  }
  if (startsWith(prior_id, "rhs_ns") &&
      !identical(
        colnames(rhs_sidecar),
        rqr_ordinary_v1_expected_rhs_sidecar_columns(
          prior_id, rhs_active_names
        )
      )) {
    stop("The RHS root-specific sidecar schema is invalid.",
         call. = FALSE)
  }
  list(
    values = values,
    future_check = future_check,
    rhs_root_specific_sidecar = rhs_sidecar,
    fixed_parameter_checks = fixed_parameter_checks
  )
}

rqr_ordinary_v1_diagnose_cell <- function(
    chains, gates, posterior_namespace = asNamespace("posterior"),
    expected_schema = NULL) {
  if (!is.list(chains) || length(chains) != 4L ||
      any(!vapply(chains, is.matrix, logical(1L)))) {
    stop("A diagnostic cell requires four estimand matrices.", call. = FALSE)
  }
  schema <- colnames(chains[[1L]])
  iterations <- nrow(chains[[1L]])
  if (!length(schema) ||
      (!is.null(expected_schema) &&
       !identical(schema, expected_schema)) ||
      any(vapply(
        chains, function(x) {
          !identical(dim(x), c(iterations, length(schema))) ||
            !identical(colnames(x), schema) || any(!is.finite(x))
        }, logical(1L)
      ))) {
    stop("Four-chain estimand schemas must be finite and identical.",
         call. = FALSE)
  }
  array <- array(
    NA_real_,
    dim = c(iterations, 4L, length(schema)),
    dimnames = list(NULL, paste0("chain", 1:4), schema)
  )
  for (chain in seq_len(4L)) array[, chain, ] <- chains[[chain]]
  draws <- posterior_namespace$as_draws_array(array)
  rhat <- posterior_namespace$rhat(draws)
  bulk <- posterior_namespace$ess_bulk(draws)
  tail <- posterior_namespace$ess_tail(draws)
  mcse <- posterior_namespace$mcse_mean(draws)
  if (!identical(names(rhat), schema) ||
      !identical(names(bulk), schema) ||
      !identical(names(tail), schema) ||
      !identical(names(mcse), schema)) {
    stop(
      "Maintained diagnostic outputs do not match the exact estimand schema.",
      call. = FALSE
    )
  }
  diagnostics <- data.frame(
    estimand = schema,
    rhat = as.numeric(rhat),
    ess_bulk = as.numeric(bulk),
    ess_tail = as.numeric(tail),
    mcse_mean = as.numeric(mcse),
    stringsAsFactors = FALSE
  )
  diagnostics$pass <- with(
    diagnostics,
    is.finite(rhat) & rhat <= gates$maximum_rank_normalized_rhat &
      is.finite(ess_bulk) & ess_bulk >= gates$minimum_bulk_ess &
      is.finite(ess_tail) & ess_tail >= gates$minimum_tail_ess &
      is.finite(mcse_mean)
  )
  list(pass = all(diagnostics$pass), diagnostics = diagnostics)
}

rqr_ordinary_v1_validate_fit <- function(fit) {
  if (inherits(fit, "rqr_desn_fit")) {
    getFromNamespace(
      ".rqr_validate_desn_fit_envelope", "rqrgibbs"
    )(fit)
    if (!isTRUE(
          fit$model_spec$
            design_materialization_reproducibility_eligible
        ) ||
        !isTRUE(fit$model_spec$promotion_eligible)) {
      stop(
        "A bounded DESN fit is not promotion eligible.",
        call. = FALSE
      )
    }
  }
  static <- if (inherits(fit, "rqr_desn_fit")) fit$fit else fit
  if (!inherits(static, "rqr_mcmc") ||
      !isTRUE(static$model_spec$generalized_bayes) ||
      isTRUE(static$model_spec$response_likelihood) ||
      isTRUE(static$model_spec$response_prediction_contract) ||
      !isTRUE(static$model_spec$exact_joint_target) ||
      !identical(static$model_spec$numerical_policy, "fail") ||
      !identical(
        static$model_spec$cumulative_numerical_repair_count, 0L
      ) ||
      !isTRUE(static$model_spec$chain_history_numerically_exact) ||
      !isTRUE(static$model_spec$promotion_eligible) ||
      !isTRUE(static$provenance$reproducibility_eligible) ||
      !static$model_spec$learning_rate_mode %in%
        c("fixed_rate", "learned_pseudoresidual_normalized") ||
      any(!is.finite(static$samp.beta_root1)) ||
      any(!is.finite(static$samp.beta_root2)) ||
      any(!is.finite(static$samp.lambda))) {
    stop("A bounded fit failed the exact-target fit contract.", call. = FALSE)
  }
  validator <- getFromNamespace(
    ".rqr_validate_continuation_history", "rqrgibbs"
  )
  validator(static)
  checkpoint <- getFromNamespace(
    ".rqr_validate_static_checkpoint", "rqrgibbs"
  )
  checkpoint(static, allow_environment_mismatch = FALSE)
  invisible(TRUE)
}

rqr_ordinary_v1_validate_attested_desn_design <- function(
    path, config, external_runtime) {
  if (!is.character(path) || length(path) != 1L || is.na(path) ||
      !nzchar(path) || !file.exists(path)) {
    stop("An existing attested DESN design RDS is required.", call. = FALSE)
  }
  if (!is.list(external_runtime) ||
      !isTRUE(external_runtime$state$runtime_source_match) ||
      !isTRUE(external_runtime$state$reproducibility_eligible)) {
    stop(
      "Current pinned exdqlm runtime verification is required.",
      call. = FALSE
    )
  }
  design <- readRDS(path)
  getExportedValue("rqrgibbs", "rqr_validate_desn_design")(design)
  receipt <- design$builder$materialization_receipt %||% NULL
  state <- external_runtime$state
  attestation_sha <- rqr_ordinary_v1_sha256_file(
    external_runtime$attestation
  )
  receipt_status <- getFromNamespace(
    ".rqr_desn_materialization_receipt_status", "rqrgibbs"
  )(design)
  verification <- getFromNamespace(
    ".rqr_desn_materialization_verification", "rqrgibbs"
  )(
    design,
    external_state = state,
    runtime_attestation = external_runtime$attestation
  )
  arguments_digest <- rqr_ordinary_v1_sha256_object(
    config$fixtures$D02$effective_arguments
  )
  if (!identical(
        design$builder$id, "exdqlm_qdesn_fit_vb_design_adapter"
      ) ||
      !is.list(receipt) ||
      !identical(
        receipt$schema_version,
        config$desn_schema_contract[["materialization_receipt"]]
      ) ||
      !identical(receipt$source_commit, config$pinned_exdqlm$commit) ||
      !identical(
        receipt$package_version,
        as.character(utils::packageVersion("exdqlm"))
      ) ||
      !identical(receipt$source_tree_digest, state$source_tree_digest) ||
      !identical(
        receipt$runtime_tree_digest,
        state$runtime_package_tree_digest
      ) ||
      !identical(
        receipt$runtime_attestation_schema,
        state$runtime_attestation_schema
      ) ||
      !identical(
        receipt$runtime_attestation_sha256, attestation_sha
      ) ||
      !isTRUE(receipt$runtime_source_match) ||
      !isTRUE(receipt$reproducibility_eligible) ||
      !identical(
        design$builder$version,
        as.character(utils::packageVersion("exdqlm"))
      ) ||
      !identical(
        design$builder$source_commit,
        config$pinned_exdqlm$commit
      ) ||
      !identical(
        design$builder$adapter,
        "rqrgibbs_frozen_design_materializer/2.0.0"
      ) ||
      !identical(
        design$builder$arguments_digest,
        arguments_digest
      ) ||
      !identical(
        receipt$materializer_arguments_digest,
        arguments_digest
      ) ||
      !identical(design$reservoir$source_package, "exdqlm") ||
      !identical(
        design$reservoir$source_commit,
        config$pinned_exdqlm$commit
      ) ||
      !isTRUE(receipt_status$receipt_valid) ||
      !identical(
        receipt_status$materialized_design_payload_digest,
        receipt$materialized_design_payload_digest
      ) ||
      !isTRUE(verification$external_state_match) ||
      !isTRUE(verification$runtime_attestation_sha256_verified) ||
      !isTRUE(
        verification$materialization_reproducibility_eligible
      ) ||
      !identical(
        verification$status,
        "verified_current_isolated_materialization"
      ) ||
      !identical(
        design$schema_version,
        config$desn_schema_contract[["design"]]
      )) {
    stop("The DESN design does not carry the required pinned receipt.",
         call. = FALSE)
  }
  list(
    design = design,
    digest = rqr_ordinary_v1_sha256_object(design),
    file_sha256 = rqr_ordinary_v1_sha256_file(path),
    receipt = receipt,
    receipt_status = receipt_status,
    verification = verification,
    external_runtime = external_runtime
  )
}

rqr_ordinary_v1_attested_desn_references <- function(
    config, attested_design, provenance_control) {
  design <- attested_design$design
  rows <- config$seed_ledger[
    config$seed_ledger$purpose == "attested_desn_end_to_end",
    ,
    drop = FALSE
  ]
  if (nrow(rows) != 4L) {
    stop("Exactly four attested-DESN reference cells are required.",
         call. = FALSE)
  }
  future <- rqr_ordinary_v1_bounded_future_design(design, config)
  checks <- lapply(seq_len(nrow(rows)), function(index) {
    row <- rows[index, , drop = FALSE]
    prior <- rqr_ordinary_v1_prior(config, row$prior_id[[1L]])
    fit <- getExportedValue("rqrgibbs", "rqr_desn_fit")(
      design = design,
      coverage_level = config$coverage_level,
      design_engine = "frozen",
      inference = "mcmc",
      learning_rate = config$fixed_learning_rate,
      lambda_initial = 1,
      loss_reference_scale = config$loss_reference_scale,
      learning_rate_mode = row$learning_rate_mode[[1L]],
      lambda_prior = config$lambda_prior,
      numerical_policy = "fail",
      provenance_control = provenance_control,
      mcmc_args = list(
        beta_prior_obj = prior,
        root_swap_probability = 0.5,
        n_burn = 0L, n_mcmc = 2L, thin = 1L,
        seed = row$seed[[1L]],
        store_latent_draws = FALSE,
        store_prior_state_draws =
          rqr_ordinary_v1_retain_prior_state_draws(config, prior)
      )
    )
    rqr_ordinary_v1_validate_fit(fit)
    continued <- getExportedValue(
      "rqrgibbs", "rqr_desn_continue"
    )(fit, n_mcmc = 1L)
    rqr_ordinary_v1_validate_fit(continued)
    prediction <- getExportedValue(
      "rqrgibbs", "forecast_paths"
    )(continued, future_design = future)
    future_check <- rqr_ordinary_v1_validate_desn_prediction(
      prediction, continued, config$desn_schema_contract
    )
    external <- continued$fit$provenance$
      external_repositories$exdqlm
    pass <- isTRUE(
      continued$model_spec$
        design_materialization_reproducibility_eligible
    ) &&
      isTRUE(continued$model_spec$promotion_eligible) &&
      isTRUE(external$runtime_source_match) &&
      isTRUE(external$reproducibility_eligible) &&
      identical(
        continued$fit$checkpoint_state$completed_iterations, 3L
      ) &&
      identical(
        continued$fit$
          continuation_history_contract$generation, 1L
      ) &&
      all(is.finite(prediction$lower_draws)) &&
      all(is.finite(prediction$upper_draws))
    cbind(data.frame(
      reference_id = row$seed_id[[1L]],
      prior_id = row$prior_id[[1L]],
      learning_rate_mode = row$learning_rate_mode[[1L]],
      seed = row$seed[[1L]],
      design_digest = design$semantic_digest,
      completed_iterations =
        continued$fit$checkpoint_state$completed_iterations,
      continuation_generation =
        continued$fit$continuation_history_contract$generation,
      promotion_eligible =
        continued$model_spec$promotion_eligible,
      stringsAsFactors = FALSE
    ), transform(
      future_check,
      status = if (pass) "pass" else "fail"
    ))
  })
  out <- do.call(rbind, checks)
  if (any(out$status != "pass")) {
    stop("An attested-DESN end-to-end reference cell failed.",
         call. = FALSE)
  }
  out
}

rqr_ordinary_v1_family_provenance_control <- function(
    provenance_control, family) {
  if (all(c("fixed_design", "desn") %in% names(provenance_control))) {
    provenance_control <- provenance_control[[family]]
  }
  if (!is.list(provenance_control) ||
      is.null(provenance_control$repo_root) ||
      is.null(provenance_control$expected_git_commit) ||
      is.null(provenance_control$primary_runtime_attestation)) {
    stop(
      "The selected family lacks a complete primary provenance control.",
      call. = FALSE
    )
  }
  if (identical(family, "fixed_design") &&
      length(provenance_control$required_external_repositories %||%
             character(0))) {
    stop(
      "Standalone fixed-design validation must not require exdqlm.",
      call. = FALSE
    )
  }
  if (identical(family, "desn") &&
      !identical(
        provenance_control$required_external_repositories,
        "exdqlm"
      )) {
    stop(
      "Attested DESN validation must require the pinned exdqlm runtime.",
      call. = FALSE
    )
  }
  provenance_control
}

rqr_ordinary_v1_fit_chain <- function(
    row, config, fixtures, provenance_control, execution_design = NULL) {
  provenance_control <- rqr_ordinary_v1_family_provenance_control(
    provenance_control, row$family[[1L]]
  )
  cell <- rqr_ordinary_v1_cell_fixture(
    config, fixtures, row, execution_design = execution_design
  )
  profile <- config$mcmc$initialization_profiles[[row$chain[[1L]]]]
  init <- rqr_ordinary_v1_initial_state(cell$X, profile, cell$prior)
  retain_prior_states <- rqr_ordinary_v1_retain_prior_state_draws(
    config, cell$prior
  )
  control <- list(
    n_burn = config$mcmc$burn_in,
    n_mcmc = config$mcmc$retained_per_chain,
    thin = config$mcmc$thin,
    seed = row$seed[[1L]],
    store_latent_draws = FALSE,
    store_prior_state_draws = retain_prior_states
  )
  if (identical(cell$prior$type, "rhs_ns")) {
    control$intercept_name <- "intercept"
  }
  if (identical(row$family[[1L]], "fixed_design")) {
    fit <- getExportedValue("rqrgibbs", "rqr_mcmc_fit")(
      y = cell$y, X = cell$X,
      coverage_level = config$coverage_level,
      learning_rate = config$fixed_learning_rate,
      lambda_initial = 1,
      loss_reference_scale = config$loss_reference_scale,
      learning_rate_mode = row$learning_rate_mode[[1L]],
      lambda_prior = config$lambda_prior,
      beta_prior_obj = cell$prior,
      numerical_policy = config$mcmc$numerical_policy,
      root_swap_probability = config$mcmc$root_swap_probability,
      provenance_control = provenance_control,
      mcmc_control = control,
      init = init
    )
    future <- NULL
  } else {
    if (is.null(execution_design)) {
      stop(
        paste(
          "Bounded DESN execution requires the separately materialized,",
          "isolated-runtime-attested D02 design."
        ),
        call. = FALSE
      )
    }
    getExportedValue("rqrgibbs", "rqr_validate_desn_design")(execution_design)
    fit <- getExportedValue("rqrgibbs", "rqr_desn_fit")(
      design = execution_design,
      coverage_level = config$coverage_level,
      design_engine = "frozen",
      inference = "mcmc",
      learning_rate = config$fixed_learning_rate,
      lambda_initial = 1,
      loss_reference_scale = config$loss_reference_scale,
      learning_rate_mode = row$learning_rate_mode[[1L]],
      lambda_prior = config$lambda_prior,
      numerical_policy = config$mcmc$numerical_policy,
      provenance_control = provenance_control,
      mcmc_args = c(
        list(
          beta_prior_obj = cell$prior,
          root_swap_probability = config$mcmc$root_swap_probability,
          init = init
        ),
        control
      )
    )
    future <- rqr_ordinary_v1_bounded_future_design(
      execution_design, config
    )
  }
  rqr_ordinary_v1_validate_fit(fit)
  estimand_result <- rqr_ordinary_v1_chain_estimands(
    fit, cell$X, cell$y, row$learning_rate_mode[[1L]], future,
    schema_contract = config$desn_schema_contract,
    expected_family = row$family[[1L]],
    expected_prior_id = row$prior_id[[1L]]
  )
  list(
    fit = fit,
    estimands = estimand_result$values,
    future_check = estimand_result$future_check,
    rhs_root_specific_sidecar =
      estimand_result$rhs_root_specific_sidecar,
    fixed_parameter_checks =
      estimand_result$fixed_parameter_checks
  )
}

rqr_ordinary_v1_run_cells <- function(
    plan, fit_chain, diagnose_cell, publish_chain = function(...) NULL,
    publish_cell = function(...) NULL) {
  cell_order <- unique(plan$cell_id)
  completed <- character(0)
  for (cell_id in cell_order) {
    rows <- plan[plan$cell_id == cell_id, , drop = FALSE]
    rows <- rows[order(rows$chain), , drop = FALSE]
    if (!identical(rows$chain, 1:4)) {
      stop(sprintf("Cell %s does not contain chains 1:4.", cell_id),
           call. = FALSE)
    }
    chains <- vector("list", 4L)
    for (index in seq_len(4L)) {
      result <- tryCatch(
        fit_chain(rows[index, , drop = FALSE]),
        error = function(error) {
          stop(structure(
            list(
              message = conditionMessage(error),
              call = NULL,
              cell_id = cell_id,
              chain = rows$chain[[index]],
              parent_class = class(error)[[1L]]
            ),
            class = c(
              "rqr_ordinary_v1_chain_error", "error", "condition"
            )
          ))
        }
      )
      if (!is.list(result) || !is.matrix(result$estimands)) {
        stop("The chain callback returned an invalid result.", call. = FALSE)
      }
      chains[[index]] <- result$estimands
      publish_chain(rows[index, , drop = FALSE], result)
    }
    diagnosis <- tryCatch(
      diagnose_cell(chains, rows),
      error = function(error) {
        stop(structure(
          list(
            message = conditionMessage(error),
            call = NULL,
            cell_id = cell_id,
            chain = NA_integer_,
            parent_class = class(error)[[1L]]
          ),
          class = c(
            "rqr_ordinary_v1_cell_error", "error", "condition"
          )
        ))
      }
    )
    publish_cell(rows, diagnosis)
    if (!is.list(diagnosis) || !is.logical(diagnosis$pass) ||
        length(diagnosis$pass) != 1L || is.na(diagnosis$pass)) {
      stop("The cell diagnostic callback returned an invalid decision.",
           call. = FALSE)
    }
    if (!isTRUE(diagnosis$pass)) {
      stop(structure(
        list(
          message = sprintf(
            "Cell %s failed; later cells were not started.", cell_id
          ),
          call = NULL,
          cell_id = cell_id,
          chain = NA_integer_
        ),
        class = c(
          "rqr_ordinary_v1_cell_gate_failure", "error", "condition"
        )
      ))
    }
    completed <- c(completed, cell_id)
  }
  completed
}

rqr_ordinary_v1_reference_oracles <- function(config, fixtures, repo_root) {
  coverage <- config$coverage_level
  f01 <- fixtures$F01
  root1 <- -0.75
  root2 <- 1.25
  residual_product <- (f01$y - root1) * (f01$y - root2)
  independent <- residual_product * (
    coverage - as.numeric(residual_product < 0)
  )
  package_loss <- getExportedValue("rqrgibbs", "rqr_check_loss")(
    getExportedValue("rqrgibbs", "rqr_residual_product")(
      f01$y, rep(root1, length(f01$y)), rep(root2, length(f01$y))
    ),
    coverage
  )
  loss_error <- max(abs(independent - package_loss))

  observed_f02 <- sum(!is.na(fixtures$F02$y))
  expected_shape <- config$lambda_prior$shape + observed_f02
  expected_rate <- config$lambda_prior$rate +
    sum(getExportedValue("rqrgibbs", "rqr_check_loss")(
      getExportedValue("rqrgibbs", "rqr_residual_product")(
        fixtures$F02$y[!is.na(fixtures$F02$y)],
        rep(-0.5, observed_f02), rep(1.5, observed_f02)
      ),
      coverage
    )) / config$loss_reference_scale

  protected <- data.frame(
    relative_path = names(config$protected_dlm_sha256),
    expected_sha256 = unname(config$protected_dlm_sha256),
    actual_sha256 = vapply(
      names(config$protected_dlm_sha256),
      function(path) rqr_ordinary_v1_sha256_file(file.path(repo_root, path)),
      character(1L)
    ),
    stringsAsFactors = FALSE
  )
  protected$pass <- protected$actual_sha256 == protected$expected_sha256

  comparisons <- data.frame(
    oracle_id = c(
      "loss_sign_partition", "normalized_lambda_shape",
      "normalized_lambda_rate_finite", "desn_training_missing_rows",
      "desn_future_contracts"
    ),
    expected = c(
      "0", as.character(expected_shape), "TRUE", "3|9", "3"
    ),
    actual = c(
      format(loss_error, digits = 17),
      as.character(config$lambda_prior$shape + observed_f02),
      as.character(is.finite(expected_rate) && expected_rate > 0),
      paste(which(is.na(fixtures$D01$y)), collapse = "|"),
      as.character(length(fixtures$D03))
    ),
    tolerance = c("0", "exact", "finite_positive", "exact", "exact"),
    pass = c(
      identical(loss_error, 0),
      identical(config$lambda_prior$shape + observed_f02, expected_shape),
      is.finite(expected_rate) && expected_rate > 0,
      identical(which(is.na(fixtures$D01$y)), c(3L, 9L)),
      identical(length(fixtures$D03), 3L)
    ),
    stringsAsFactors = FALSE
  )
  gates <- data.frame(
    gate_id = c(
      "deterministic_oracles", "protected_dlm_candidate_sha256"
    ),
    status = c(
      if (all(comparisons$pass)) "pass" else "fail",
      if (all(protected$pass)) "pass" else "fail"
    ),
    detail = c(
      "No MCMC was run.",
      paste(
        "Every protected candidate file matches the exact frozen",
        "candidate SHA-256 inventory; this is an integrity check,",
        "not a byte-level noninterference claim."
      )
    ),
    stringsAsFactors = FALSE
  )
  list(
    pass = all(gates$status == "pass"),
    gates = gates,
    comparisons = comparisons,
    protected_dlm_hashes = protected
  )
}

rqr_ordinary_v1_reference_test_files <- function(repo_root) {
  file.path(
    repo_root, "application", "tests", "testthat",
    rqr_ordinary_v1_reference_test_names()
  )
}

rqr_ordinary_v1_reference_test_names <- function() {
  c(
    "test-rqr-native-beta-prior.R",
    "test-rqr-native-rhs-ns.R",
    "test-rqr-native-fixed-design-v1.R",
    "test-rqr-native-ordinary-v1-materializer.R",
    "test-rqr-native-ordinary-v1-reference-cells.R",
    "test-rqr-native-desn-design.R",
    "test-rqr-native-desn-fit-v1.R",
    "test-rqr-native-desn-future-contract.R",
    "test-rqr-native-package-integration.R",
    "test-rqr-native-model.R",
    "test-rqr-native-ffbs.R",
    "test-rqr-native-sampler.R",
    "test-rqr-native-oracle.R",
    "test-rqr-native-ordinary-v1-validation-runner.R"
  )
}

rqr_ordinary_v1_run_reference_tests <- function(repo_root) {
  if (!requireNamespace("testthat", quietly = TRUE)) {
    stop("testthat is required for reference-only source gates.",
         call. = FALSE)
  }
  files <- rqr_ordinary_v1_reference_test_files(repo_root)
  if (any(!file.exists(files))) {
    stop("A required ordinary-v1 reference test file is absent.",
         call. = FALSE)
  }
  rows <- lapply(files, function(path) {
    started <- proc.time()[["elapsed"]]
    result <- tryCatch(
      testthat::test_file(path, reporter = "silent"),
      error = function(error) error
    )
    elapsed <- proc.time()[["elapsed"]] - started
    if (inherits(result, "error")) {
      return(data.frame(
        test_file = basename(path), test_blocks = 0L, expectations = 0L,
        failures = 0L, errors = 1L, warnings = 0L, skipped = 0L,
        elapsed_seconds = elapsed, status = "fail",
        detail = conditionMessage(result), stringsAsFactors = FALSE
      ))
    }
    summary <- as.data.frame(result)
    failure_count <- sum(summary$failed, na.rm = TRUE)
    error_count <- sum(summary$error %in% TRUE, na.rm = TRUE)
    warning_count <- sum(summary$warning, na.rm = TRUE)
    skip_count <- sum(summary$skipped %in% TRUE, na.rm = TRUE)
    passed <- failure_count == 0L && error_count == 0L &&
      warning_count == 0L && skip_count == 0L
    data.frame(
      test_file = basename(path),
      test_blocks = nrow(summary),
      expectations = sum(summary$nb, na.rm = TRUE),
      failures = failure_count,
      errors = error_count,
      warnings = warning_count,
      skipped = skip_count,
      elapsed_seconds = elapsed,
      status = if (passed) "pass" else "fail",
      detail = if (passed) "all expectations passed" else
        "failure, error, warning, or skip present",
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

rqr_ordinary_v1_reference_evidence_tables <- function(
    package_checks, oracle_comparisons, attested_references) {
  test_row <- function(file, check_id, detail) {
    row <- package_checks[package_checks$test_file == file, , drop = FALSE]
    if (nrow(row) != 1L || !identical(row$status[[1L]], "pass")) {
      stop(
        sprintf("Required reference test evidence is absent: %s.", file),
        call. = FALSE
      )
    }
    data.frame(
      check_id = check_id,
      evidence_source = file,
      expectations = row$expectations[[1L]],
      status = row$status[[1L]],
      detail = detail,
      stringsAsFactors = FALSE
    )
  }
  missing_oracle <- oracle_comparisons[
    oracle_comparisons$oracle_id == "desn_training_missing_rows",
    ,
    drop = FALSE
  ]
  if (nrow(missing_oracle) != 1L || !isTRUE(missing_oracle$pass[[1L]])) {
    stop("The deterministic missingness oracle is absent.", call. = FALSE)
  }
  missingness <- rbind(
    test_row(
      "test-rqr-native-fixed-design-v1.R",
      "fixed_design_observed_mask_and_rng_contract",
      paste(
        "Source-bound native tests cover dropped-row equality,",
        "placeholder invariance, missing-site RNG omission, learned",
        "n_obs, checkpoint mask integrity, and all-missing rejection."
      )
    ),
    data.frame(
      check_id = "desn_training_missing_indices",
      evidence_source = "oracle_comparisons.csv",
      expectations = 1L,
      status = "pass",
      detail = paste0(
        "Expected and observed missing indices: ",
        missing_oracle$actual[[1L]], "."
      ),
      stringsAsFactors = FALSE
    )
  )
  rhs <- test_row(
    "test-rqr-native-rhs-ns.R",
    "native_rhs_ns_joint_and_full_conditionals",
    paste(
      "Source-bound native tests cover joint-kernel ratios,",
      "conditional parameters, an independently reproduced seeded sweep,",
      "fixed/random shoulders, intercept policy, and no clipping."
    )
  )
  continuation <- rbind(
    test_row(
      "test-rqr-native-ordinary-v1-reference-cells.R",
      "static_and_custom_desn_6_equals_2_plus_2_plus_2",
      paste(
        "Eight static prior/rate cells and four custom frozen-DESN",
        "prior/rate cells compare every retained stochastic field,",
        "root-swap trace, RNG state, and final checkpoint."
      )
    ),
    data.frame(
      check_id = paste0(
        "attested_desn_", attested_references$reference_id
      ),
      evidence_source = "desn_future_checks.csv",
      expectations = 1L,
      status = attested_references$status,
      detail = paste(
        "Receipt-v2 D02 fit and continuation remained promotable; its",
        "future contract was verified and explicitly nonpromotable."
      ),
      stringsAsFactors = FALSE
    )
  )
  history <- rbind(
    test_row(
      "test-rqr-native-fixed-design-v1.R",
      "static_checkpoint_and_history_mutations",
      paste(
        "Source-bound mutations cover data, mask, target, prior,",
        "checkpoint, continuation bookkeeping, and RHS state."
      )
    ),
    test_row(
      "test-rqr-native-desn-fit-v1.R",
      "desn_envelope_and_materialization_mutations",
      paste(
        "Source-bound mutations cover the outer fit envelope,",
        "materialization receipt, external state, and continuation."
      )
    ),
    test_row(
      "test-rqr-native-ordinary-v1-validation-runner.R",
      "authorization_and_evidence_mutations",
      paste(
        "Source-bound mutations cover strict-ancestor authorization,",
        "config-only deltas, future promotion flags, and artifacts."
      )
    )
  )
  list(
    missingness_checks = missingness,
    rhs_ns_conditional_checks = rhs,
    continuation_checks = continuation,
    history_mutation_checks = history
  )
}

rqr_ordinary_v1_config_at_commit <- function(repo_root, commit) {
  path <- paste0(
    commit,
    ":application/config/rqr_ordinary_v1/",
    "rqr_ordinary_v1_bounded_validation_20260726.R"
  )
  text <- rqr_ordinary_v1_git(repo_root, c("show", path))
  environment <- new.env(parent = baseenv())
  eval(parse(text = text, keep.source = FALSE), envir = environment)
  value <- environment$rqr_ordinary_v1_bounded_validation
  if (!is.list(value)) {
    stop(
      "The reviewed implementation does not contain the frozen config.",
      call. = FALSE
    )
  }
  value
}

rqr_ordinary_v1_authorization_status <- function(
    repo_root, config, actual_commit) {
  if (!isTRUE(config$ordinary_v1_execute_enabled)) {
    return(list(
      status = "source_candidate_execution_disabled",
      reviewed_implementation_commit = NA_character_,
      authorization_delta_verified = FALSE
    ))
  }
  reviewed <- tolower(config$reviewed_implementation_commit)
  merge_base <- tolower(rqr_ordinary_v1_git(
    repo_root, c("merge-base", reviewed, actual_commit)
  ))
  if (!identical(merge_base, reviewed) ||
      identical(reviewed, actual_commit)) {
    stop(
      paste(
        "The reviewed implementation must be a strict ancestor of the",
        "authorization commit."
      ),
      call. = FALSE
    )
  }
  changed <- rqr_ordinary_v1_git(
    repo_root, c("diff", "--name-only", reviewed, actual_commit)
  )
  changed <- if (nzchar(changed)) strsplit(changed, "\n", fixed = TRUE)[[1L]]
    else character(0)
  config_path <- paste0(
    "application/config/rqr_ordinary_v1/",
    "rqr_ordinary_v1_bounded_validation_20260726.R"
  )
  if (!identical(changed, config_path)) {
    stop(
      paste(
        "The authorization commit must differ from the reviewed",
        "implementation only in the frozen ordinary-v1 config."
      ),
      call. = FALSE
    )
  }
  base <- rqr_ordinary_v1_config_at_commit(repo_root, reviewed)
  if (isTRUE(base$ordinary_v1_execute_enabled) ||
      !is.na(base$reviewed_implementation_commit)) {
    stop(
      "The reviewed implementation base is not fail-closed.",
      call. = FALSE
    )
  }
  authorized <- base
  authorized$ordinary_v1_execute_enabled <- TRUE
  authorized$reviewed_implementation_commit <- reviewed
  if (!identical(config, authorized)) {
    stop(
      paste(
        "The authorization config contains changes beyond the execution",
        "flag and reviewed implementation SHA."
      ),
      call. = FALSE
    )
  }
  list(
    status = "flag_only_authorization_verified",
    reviewed_implementation_commit = reviewed,
    authorization_delta_verified = TRUE
  )
}

rqr_ordinary_v1_source_preflight <- function(repo_root, config) {
  expected <- tolower(Sys.getenv("RQR_EXPECTED_PRIMARY_COMMIT", unset = ""))
  if (!grepl("^[0-9a-f]{40}$", expected)) {
    stop("RQR_EXPECTED_PRIMARY_COMMIT must be the current full SHA.",
         call. = FALSE)
  }
  actual <- tolower(rqr_ordinary_v1_git(repo_root, c("rev-parse", "HEAD")))
  branch <- rqr_ordinary_v1_git(
    repo_root, c("rev-parse", "--abbrev-ref", "HEAD")
  )
  status <- rqr_ordinary_v1_git(
    repo_root, c("status", "--porcelain=v2", "--untracked-files=all")
  )
  if (!identical(actual, expected) || !identical(branch, "main") ||
      nzchar(status)) {
    stop("Ordinary-v1 validation requires clean main at the reviewed SHA.",
         call. = FALSE)
  }
  authorization <- rqr_ordinary_v1_authorization_status(
    repo_root, config, actual
  )
  thread_variables <- c(
    "OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS",
    "BLIS_NUM_THREADS", "VECLIB_MAXIMUM_THREADS",
    "NUMEXPR_NUM_THREADS"
  )
  thread_values <- Sys.getenv(thread_variables, unset = "")
  if (any(thread_values != "1")) {
    stop(
      paste(
        "All BLAS/OpenMP thread limits must equal one before R starts:",
        paste(thread_variables[thread_values != "1"], collapse = ", ")
      ),
      call. = FALSE
    )
  }
  data.frame(
    repository = "RQR-GIBBS",
    branch = branch,
    commit = actual,
    clean = !nzchar(status),
    config_digest = rqr_ordinary_v1_sha256_object(config),
    authorization_status = authorization$status,
    reviewed_implementation_commit =
      authorization$reviewed_implementation_commit,
    authorization_delta_verified =
      authorization$authorization_delta_verified,
    rng_kind = paste(RNGkind(), collapse = "|"),
    stringsAsFactors = FALSE
  )
}

rqr_ordinary_v1_runtime_preflight <- function(repo_root, expected_commit) {
  if ("rqrgibbs" %in% loadedNamespaces()) {
    stop("rqrgibbs was loaded before isolated-runtime selection.",
         call. = FALSE)
  }
  root <- Sys.getenv("RQR_PRIMARY_RUNTIME_ROOT", unset = "")
  if (!nzchar(root)) {
    stop("RQR_PRIMARY_RUNTIME_ROOT is required.", call. = FALSE)
  }
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  commit_root <- file.path(root, expected_commit)
  library <- normalizePath(
    file.path(commit_root, "library"), winslash = "/", mustWork = TRUE
  )
  attestation <- normalizePath(
    file.path(
      commit_root, "attestations",
      paste0("rqrgibbs_", expected_commit, ".rds")
    ),
    winslash = "/", mustWork = TRUE
  )
  .libPaths(c(library, .libPaths()))
  required <- c("digest", "jsonlite", "posterior", "rqrgibbs")
  missing <- required[!vapply(
    required, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1L)
  )]
  if (length(missing)) {
    stop("Missing isolated-runtime packages: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  if (utils::packageVersion("posterior") < "1.7.0") {
    stop("posterior >= 1.7.0 is required.", call. = FALSE)
  }
  state <- getFromNamespace(".rqr_repository_provenance", "rqrgibbs")(list(
    repo_root = repo_root,
    expected_git_commit = expected_commit,
    runtime_package = "rqrgibbs",
    runtime_attestation = attestation,
    require_isolated_runtime = TRUE,
    source_subdir = "application"
  ))
  required_gates <- c(
    "runtime_attestation_match", "source_archive_tree_match",
    "source_package_verified", "source_package_archive_match",
    "build_evidence_verified", "install_evidence_verified",
    "runtime_lineage_marker_match", "runtime_install_receipt_match",
    "runtime_source_match", "reproducibility_eligible"
  )
  values <- vapply(
    required_gates, function(name) isTRUE(state[[name]]), logical(1L)
  )
  if (!all(values)) {
    stop(
      "Isolated primary-runtime lineage failed: ",
      paste(required_gates[!values], collapse = ", "),
      call. = FALSE
    )
  }
  list(
    state = state,
    library = library,
    attestation = attestation,
    table = data.frame(
      package = "rqrgibbs",
      version = as.character(utils::packageVersion("rqrgibbs")),
      source_commit = state$git_commit,
      runtime_path = normalizePath(
        getNamespaceInfo(asNamespace("rqrgibbs"), "path"),
        winslash = "/", mustWork = TRUE
      ),
      runtime_tree_digest =
        state$runtime_package_tree_digest %||% NA_character_,
      runtime_source_match = state$runtime_source_match,
      reproducibility_eligible = state$reproducibility_eligible,
      runtime_attestation_schema =
        state$runtime_attestation_schema %||% NA_character_,
      attestation_sha256 = rqr_ordinary_v1_sha256_file(attestation),
      R_version = R.version.string,
      platform = R.version$platform,
      compiler = getFromNamespace(
        ".rqr_compiler_info", "rqrgibbs"
      )(),
      BLAS = as.character(utils::sessionInfo()$BLAS),
      LAPACK = as.character(utils::sessionInfo()$LAPACK),
      posterior_version = as.character(utils::packageVersion("posterior")),
      stringsAsFactors = FALSE
    )
  )
}

rqr_ordinary_v1_external_runtime_preflight <- function(
    repo_root, config) {
  checkout <- Sys.getenv(
    "RQR_EXDQLM_REFERENCE_ROOT",
    unset = "/data/muscat_data/jaguir26/exdqlm__wt__qdesn_0p4p0_integration"
  )
  checkout <- normalizePath(checkout, winslash = "/", mustWork = TRUE)
  cache_root <- Sys.getenv(
    "RQR_EXDQLM_RUNTIME_ROOT",
    unset = file.path(
      repo_root, "application", "cache", "exdqlm_runtime"
    )
  )
  cache_root <- normalizePath(
    cache_root, winslash = "/", mustWork = TRUE
  )
  library <- normalizePath(
    file.path(cache_root, "library"),
    winslash = "/", mustWork = TRUE
  )
  attestation_default <- file.path(
    cache_root, "attestations",
    paste0(
      "exdqlm_", substr(config$pinned_exdqlm$commit, 1L, 12L),
      ".rds"
    )
  )
  attestation <- Sys.getenv(
    "RQR_EXDQLM_RUNTIME_ATTESTATION",
    unset = attestation_default
  )
  attestation <- normalizePath(
    attestation, winslash = "/", mustWork = TRUE
  )
  if ("exdqlm" %in% loadedNamespaces()) {
    executing <- normalizePath(
      getNamespaceInfo(asNamespace("exdqlm"), "path"),
      winslash = "/", mustWork = TRUE
    )
    if (!identical(executing, file.path(library, "exdqlm"))) {
      stop(
        "exdqlm was loaded from outside the pinned isolated library.",
        call. = FALSE
      )
    }
  } else {
    .libPaths(c(library, .libPaths()))
    if (!requireNamespace("exdqlm", quietly = TRUE)) {
      stop("The pinned isolated exdqlm runtime is unavailable.",
           call. = FALSE)
    }
  }
  spec <- list(
    repo_root = checkout,
    expected_git_commit = config$pinned_exdqlm$commit,
    runtime_package = "exdqlm",
    runtime_attestation = attestation,
    require_isolated_runtime = TRUE,
    source_subdir = "."
  )
  state <- getFromNamespace(
    ".rqr_repository_provenance", "rqrgibbs"
  )(spec)
  required <- c(
    "runtime_attestation_match", "source_archive_tree_match",
    "source_package_verified", "source_package_archive_match",
    "build_evidence_verified", "install_evidence_verified",
    "runtime_lineage_marker_match", "runtime_install_receipt_match",
    "runtime_source_match", "reproducibility_eligible"
  )
  pass <- vapply(required, function(field) {
    isTRUE(state[[field]])
  }, logical(1L))
  if (!all(pass) ||
      !identical(state$git_commit, config$pinned_exdqlm$commit)) {
    stop(
      paste(
        "The pinned isolated exdqlm runtime failed:",
        paste(c(required[!pass], if (
          !identical(state$git_commit, config$pinned_exdqlm$commit)
        ) "source_commit" else character(0)), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  list(
    state = state,
    library = library,
    attestation = attestation,
    spec = spec,
    table = data.frame(
      package = "exdqlm",
      version = as.character(utils::packageVersion("exdqlm")),
      source_commit = state$git_commit,
      runtime_path = normalizePath(
        getNamespaceInfo(asNamespace("exdqlm"), "path"),
        winslash = "/", mustWork = TRUE
      ),
      runtime_tree_digest =
        state$runtime_package_tree_digest %||% NA_character_,
      runtime_source_match = state$runtime_source_match,
      reproducibility_eligible = state$reproducibility_eligible,
      runtime_attestation_schema =
        state$runtime_attestation_schema %||% NA_character_,
      attestation_sha256 =
        rqr_ordinary_v1_sha256_file(attestation),
      R_version = R.version.string,
      platform = R.version$platform,
      compiler = getFromNamespace(
        ".rqr_compiler_info", "rqrgibbs"
      )(),
      BLAS = as.character(utils::sessionInfo()$BLAS),
      LAPACK = as.character(utils::sessionInfo()$LAPACK),
      posterior_version =
        as.character(utils::packageVersion("posterior")),
      stringsAsFactors = FALSE
    )
  )
}

rqr_ordinary_v1_external_guard <- function(config) {
  checkout <- Sys.getenv(
    "RQR_EXDQLM_REFERENCE_ROOT",
    unset = "/data/muscat_data/jaguir26/exdqlm__wt__qdesn_0p4p0_integration"
  )
  checkout <- normalizePath(checkout, winslash = "/", mustWork = TRUE)
  read_git <- function(args) {
    output <- suppressWarnings(system2(
      "git", c("-C", shQuote(checkout), args),
      stdout = TRUE, stderr = TRUE,
      env = c("GIT_OPTIONAL_LOCKS=0", "GIT_TERMINAL_PROMPT=0")
    ))
    status <- attr(output, "status")
    if (is.null(status)) status <- 0L
    if (!identical(as.integer(status), 0L)) {
      stop("Pinned exdqlm provenance read failed.", call. = FALSE)
    }
    paste(output, collapse = "\n")
  }
  commit <- trimws(read_git(c("rev-parse", "HEAD")))
  branch <- trimws(read_git(c("rev-parse", "--abbrev-ref", "HEAD")))
  snapshot <- rqr_ordinary_v1_sha256_object(list(
    commit = commit,
    branch = branch,
    status = read_git(c(
      "status", "--porcelain=v2", "--untracked-files=all", "--ignored=matching"
    )),
    diff = read_git(c("diff", "--binary", "--no-ext-diff", "HEAD", "--"))
  ))
  if (!identical(commit, config$pinned_exdqlm$commit) ||
      !identical(branch, config$pinned_exdqlm$branch)) {
    stop("The pinned exdqlm reference checkout is at the wrong source state.",
         call. = FALSE)
  }
  list(path = checkout, commit = commit, branch = branch, snapshot = snapshot)
}

rqr_ordinary_v1_verify_external_guard <- function(config, before) {
  after <- rqr_ordinary_v1_external_guard(config)
  if (!identical(before, after)) {
    stop("The protected exdqlm checkout changed during validation.",
         call. = FALSE)
  }
  invisible(TRUE)
}

rqr_ordinary_v1_monitor_preflight <- function(config, required = TRUE) {
  values <- list(
    active = Sys.getenv("RQR_ORDINARY_V1_MONITOR_ACTIVE", unset = ""),
    timeout_seconds = suppressWarnings(as.numeric(Sys.getenv(
      "RQR_ORDINARY_V1_MONITOR_TIMEOUT_SECONDS", unset = NA_character_
    ))),
    maximum_processes = suppressWarnings(as.numeric(Sys.getenv(
      "RQR_ORDINARY_V1_MONITOR_MAX_PROCESSES", unset = NA_character_
    ))),
    maximum_threads = suppressWarnings(as.numeric(Sys.getenv(
      "RQR_ORDINARY_V1_MONITOR_MAX_THREADS", unset = NA_character_
    ))),
    maximum_artifact_bytes = suppressWarnings(as.numeric(Sys.getenv(
      "RQR_ORDINARY_V1_MONITOR_MAX_ARTIFACT_BYTES", unset = NA_character_
    ))),
    cleanup_traps = Sys.getenv(
      "RQR_ORDINARY_V1_MONITOR_CLEANUP_TRAPS", unset = ""
    ),
    final_sweep = Sys.getenv(
      "RQR_ORDINARY_V1_MONITOR_FINAL_PGID_SWEEP", unset = ""
    )
  )
  expected <- list(
    timeout_seconds = 60 * config$resources$hard_timeout_minutes,
    maximum_processes = config$resources$maximum_processes,
    maximum_threads = config$resources$maximum_threads,
    maximum_artifact_bytes = config$resources$maximum_artifact_bytes
  )
  pass <- identical(values$active, "YES") &&
    identical(values$cleanup_traps, "YES") &&
    identical(values$final_sweep, "YES") &&
    all(vapply(
      names(expected),
      function(name) identical(values[[name]], as.numeric(expected[[name]])),
      logical(1L)
    ))
  if (required && !pass) {
    stop(
      paste(
        "The active external process-group monitor does not satisfy the",
        "45-minute, cleanup-trap, final-sweep, process, thread, and artifact",
        "contract."
      ),
      call. = FALSE
    )
  }
  data.frame(
    monitor_active = identical(values$active, "YES"),
    cleanup_traps = identical(values$cleanup_traps, "YES"),
    final_pgid_sweep = identical(values$final_sweep, "YES"),
    timeout_seconds = values$timeout_seconds,
    maximum_processes = values$maximum_processes,
    maximum_threads = values$maximum_threads,
    maximum_artifact_bytes = values$maximum_artifact_bytes,
    contract_pass = pass,
    stringsAsFactors = FALSE
  )
}

rqr_ordinary_v1_disabled_config_digest <- function(config) {
  candidate <- config
  candidate$ordinary_v1_execute_enabled <- FALSE
  candidate$reviewed_implementation_commit <- NA_character_
  rqr_ordinary_v1_sha256_object(candidate)
}

rqr_ordinary_v1_validate_runtime_compatibility <- function(
    recorded, current, reviewed_commit, current_commit, pinned_exdqlm_commit) {
  required <- c(
    "package", "version", "source_commit", "runtime_path",
    "runtime_tree_digest", "runtime_source_match",
    "reproducibility_eligible", "runtime_attestation_schema",
    "attestation_sha256", "R_version", "platform", "compiler",
    "BLAS", "LAPACK", "posterior_version"
  )
  stable <- c(
    "package", "version", "runtime_tree_digest", "runtime_source_match",
    "reproducibility_eligible", "runtime_attestation_schema",
    "R_version", "platform", "compiler", "BLAS", "LAPACK",
    "posterior_version"
  )
  normalize <- function(table) {
    table <- table[order(table$package), stable, drop = FALSE]
    rownames(table) <- NULL
    table[] <- lapply(table, as.character)
    table
  }
  expected_recorded_commits <- c(
    rqrgibbs = tolower(reviewed_commit),
    exdqlm = tolower(pinned_exdqlm_commit)
  )
  expected_current_commits <- c(
    rqrgibbs = tolower(current_commit),
    exdqlm = tolower(pinned_exdqlm_commit)
  )
  valid_table <- function(table, expected_commits) {
    is.data.frame(table) &&
      all(required %in% names(table)) &&
      nrow(table) == 2L &&
      setequal(as.character(table$package), names(expected_commits)) &&
      !anyDuplicated(table$package) &&
      all(vapply(seq_len(nrow(table)), function(index) {
        package <- as.character(table$package[[index]])
        identical(
          tolower(as.character(table$source_commit[[index]])),
          unname(expected_commits[[package]])
        ) &&
          grepl(
            "^[0-9a-f]{64}$",
            tolower(as.character(table$runtime_tree_digest[[index]]))
          ) &&
          grepl(
            "^[0-9a-f]{64}$",
            tolower(as.character(table$attestation_sha256[[index]]))
          ) &&
          identical(
            as.character(table$runtime_source_match[[index]]), "TRUE"
          ) &&
          identical(
            as.character(table$reproducibility_eligible[[index]]), "TRUE"
          ) &&
          nzchar(as.character(table$runtime_path[[index]])) &&
          nzchar(as.character(
            table$runtime_attestation_schema[[index]]
          ))
      }, logical(1L)))
  }
  if (!valid_table(recorded, expected_recorded_commits) ||
      !valid_table(current, expected_current_commits) ||
      !identical(normalize(recorded), normalize(current))) {
    stop(
      paste(
        "The reviewed-source runtime is not compatible with the current",
        "flag-only authorization runtime and toolchain."
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

rqr_ordinary_v1_validate_wrapper_manifest <- function(
    output_dir, monitor_dir, manifest) {
  monitor_names <- c(
    "process_group_monitor.csv", "process_group_resource_summary.csv",
    "runner.stderr.log", "runner.stdout.log", "wrapper_closeout.csv"
  )
  output_names <- c(
    "artifact_hashes.csv", "run_status.csv", "source_state.csv",
    "validation_config_digest.csv"
  )
  expected <- rbind(
    data.frame(
      role = "monitor_evidence",
      path = paste0("monitor/", monitor_names),
      stringsAsFactors = FALSE
    ),
    data.frame(
      role = "r_evidence_binding",
      path = paste0("output/", output_names),
      stringsAsFactors = FALSE
    )
  )
  expected <- expected[order(expected$role, expected$path), , drop = FALSE]
  rownames(expected) <- NULL
  actual_monitor_names <- sort(list.files(
    monitor_dir, all.files = TRUE, no.. = TRUE
  ))
  if (!identical(
        actual_monitor_names,
        sort(c(monitor_names, "wrapper_artifact_hashes.csv"))
      ) ||
      !is.data.frame(manifest) ||
      !identical(names(manifest), c("role", "path", "bytes", "sha256")) ||
      nrow(manifest) != nrow(expected) ||
      anyNA(manifest) ||
      anyDuplicated(paste(manifest$role, manifest$path, sep = "|"))) {
    stop("The wrapper artifact manifest has an invalid closed file set.",
         call. = FALSE)
  }
  observed <- manifest[order(manifest$role, manifest$path), , drop = FALSE]
  rownames(observed) <- NULL
  if (!identical(
        observed[, c("role", "path"), drop = FALSE], expected
      ) ||
      any(!vapply(observed$bytes, rqr_ordinary_v1_is_integer_scalar,
                  logical(1L), minimum = 0)) ||
      any(!grepl(
        "^[0-9a-f]{64}$", tolower(as.character(observed$sha256))
      ))) {
    stop("The wrapper artifact manifest has invalid rows.", call. = FALSE)
  }
  for (index in seq_len(nrow(observed))) {
    relative <- as.character(observed$path[[index]])
    path <- if (identical(
      observed$role[[index]], "monitor_evidence"
    )) {
      file.path(monitor_dir, sub("^monitor/", "", relative))
    } else {
      file.path(output_dir, sub("^output/", "", relative))
    }
    if (!file.exists(path) || nzchar(Sys.readlink(path)) ||
        !identical(
          as.numeric(file.info(path)$size),
          as.numeric(observed$bytes[[index]])
        ) ||
        !identical(
          rqr_ordinary_v1_sha256_file(path),
          tolower(as.character(observed$sha256[[index]]))
        )) {
      stop("The wrapper artifact manifest does not match current bytes.",
           call. = FALSE)
    }
  }
  invisible(TRUE)
}

rqr_ordinary_v1_validate_benchmark_bundle <- function(
    config, source_state, runtime_table, benchmark_dir,
    benchmark_monitor_dir, attested_design) {
  required_output <- c(
    "artifact_hashes.csv", "bounded_diagnostics.csv",
    "checkpoint_manifest.csv", "closeout.md",
    "compact_posterior_summaries.csv", "desn_future_checks.csv",
    "failure_log.csv", "fit_plan.csv", "fit_plan_status.csv",
    "fixed_parameter_checks.csv",
    "fixture_manifest.csv", "initialization_manifest.csv",
    "local_chain_hashes.csv", "provenance_checks.csv",
    "rhs_root_trace_sidecar.csv",
    "resource_summary.csv", "root_swap_sidecar.csv",
    "run_status.csv", "runtime_attestations.csv", "seed_ledger.csv",
    "session_info.txt", "source_state.csv",
    "validation_config_digest.csv"
  )
  required_monitor <- c(
    "process_group_monitor.csv", "process_group_resource_summary.csv",
    "runner.stderr.log", "runner.stdout.log", "wrapper_closeout.csv",
    "wrapper_artifact_hashes.csv"
  )
  reviewed <- tolower(config$reviewed_implementation_commit %||% "")
  if (!isTRUE(config$ordinary_v1_execute_enabled) ||
      !grepl("^[0-9a-f]{40}$", reviewed) ||
      !is.data.frame(source_state) || nrow(source_state) != 1L ||
      !identical(
        source_state$authorization_status[[1L]],
        "flag_only_authorization_verified"
      ) ||
      !isTRUE(source_state$authorization_delta_verified[[1L]]) ||
      !dir.exists(benchmark_dir) ||
      !dir.exists(benchmark_monitor_dir) ||
      !identical(
        sort(list.files(
          benchmark_dir, all.files = TRUE, no.. = TRUE,
          recursive = TRUE
        )),
        sort(required_output)
      ) ||
      !identical(
        sort(list.files(
          benchmark_monitor_dir, all.files = TRUE, no.. = TRUE
        )),
        sort(required_monitor)
      ) ||
      any(vapply(
        file.path(benchmark_dir, required_output),
        function(path) nzchar(Sys.readlink(path)), logical(1L)
      )) ||
      any(vapply(
        file.path(benchmark_monitor_dir, required_monitor),
        function(path) nzchar(Sys.readlink(path)), logical(1L)
      ))) {
    stop("The one-cell benchmark evidence bundle is incomplete.",
         call. = FALSE)
  }
  read_output <- function(name) {
    utils::read.csv(
      file.path(benchmark_dir, name),
      stringsAsFactors = FALSE, check.names = FALSE,
      na.strings = c("", "NA")
    )
  }
  exact_schema <- function(table, fields) {
    is.data.frame(table) &&
      identical(names(table), c("schema_version", fields)) &&
      all(
        as.character(table$schema_version) ==
          rqr_ordinary_v1_schema()
      )
  }
  comparable <- function(table, fields) {
    out <- table[, fields, drop = FALSE]
    out[] <- lapply(out, as.character)
    rownames(out) <- NULL
    out
  }
  manifest <- utils::read.csv(
    file.path(benchmark_dir, "artifact_hashes.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  if (!identical(
        names(manifest),
        c("schema_version", "relative_path", "byte_count", "sha256")
      ) ||
      any(
        as.character(manifest$schema_version) !=
          rqr_ordinary_v1_schema()
      ) ||
      !rqr_ordinary_v1_validate_artifact_manifest(
        benchmark_dir, manifest
      )) {
    stop("The one-cell benchmark output hashes do not match.",
         call. = FALSE)
  }
  status <- read_output("run_status.csv")
  source <- read_output("source_state.csv")
  recorded_config <- read_output("validation_config_digest.csv")
  recorded_runtime <- read_output("runtime_attestations.csv")
  output_resources <- read_output("resource_summary.csv")
  plan <- read_output("fit_plan.csv")
  plan_status <- read_output("fit_plan_status.csv")
  initialization <- read_output("initialization_manifest.csv")
  diagnostics <- read_output("bounded_diagnostics.csv")
  summaries <- read_output("compact_posterior_summaries.csv")
  checkpoints <- read_output("checkpoint_manifest.csv")
  provenance <- read_output("provenance_checks.csv")
  swaps <- read_output("root_swap_sidecar.csv")
  future <- read_output("desn_future_checks.csv")
  fixed_parameters <- read_output("fixed_parameter_checks.csv")
  rhs_sidecar <- read_output("rhs_root_trace_sidecar.csv")
  chain_hashes <- read_output("local_chain_hashes.csv")
  failure <- read_output("failure_log.csv")
  expected_plan <- config$benchmark_plan
  plan_fields <- names(expected_plan)
  plan_status_fields <- c(plan_fields, "status", "fit_sha256")
  chain_hash_fields <- c(
    "cell_id", "chain", "seed", "relative_path", "byte_count", "sha256"
  )
  expected_chain_names <- sprintf("BENCH01_chain_%02d.rds", 1:4)
  expected_config_digest <- rqr_ordinary_v1_disabled_config_digest(config)
  design_X <- attested_design$design$X
  if (!is.matrix(design_X) || !is.numeric(design_X) ||
      !nrow(design_X) || !ncol(design_X) ||
      is.null(colnames(design_X)) ||
      anyNA(colnames(design_X)) ||
      any(!nzchar(colnames(design_X))) ||
      anyDuplicated(colnames(design_X))) {
    stop("The benchmark design cannot define its estimand schema.",
         call. = FALSE)
  }
  rhs_intercept <- config$fixtures$F05$prior$intercept_name
  rhs_active_names <- setdiff(colnames(design_X), rhs_intercept)
  expected_estimands <- rqr_ordinary_v1_expected_estimand_columns(
    family = "desn",
    learning_rate_mode = "learned_pseudoresidual_normalized",
    prior_id = "rhs_ns_fixed",
    n_training = nrow(design_X),
    coefficient_names = colnames(design_X),
    horizon = config$fixtures$D02$future_extension$horizon,
    rhs_active_names = rhs_active_names
  )
  expected_rhs_sidecar <-
    rqr_ordinary_v1_expected_rhs_sidecar_columns(
      "rhs_ns_fixed", rhs_active_names
    )
  rqr_ordinary_v1_validate_runtime_compatibility(
    recorded_runtime, runtime_table, reviewed,
    source_state$commit[[1L]], config$pinned_exdqlm$commit
  )

  resources <- utils::read.csv(
    file.path(
      benchmark_monitor_dir, "process_group_resource_summary.csv"
    ),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  wrapper <- utils::read.csv(
    file.path(benchmark_monitor_dir, "wrapper_closeout.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  wrapper_manifest <- utils::read.csv(
    file.path(benchmark_monitor_dir, "wrapper_artifact_hashes.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  monitor_trace <- utils::read.csv(
    file.path(benchmark_monitor_dir, "process_group_monitor.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  rqr_ordinary_v1_validate_wrapper_manifest(
    benchmark_dir, benchmark_monitor_dir, wrapper_manifest
  )
  expected_wrapper_fields <- c(
    "schema_version", "mode", "expected_primary_commit",
    "process_group_id", "runner_exit_status", "resource_pass",
    "monitor_kind", "sampled_resource_maxima_are_kernel_hard",
    "signal_received", "final_pgid_empty", "residual_group_cleanup",
    "completed_at"
  )
  expected_resource_metrics <- c(
    "sampled_peak_processes", "sampled_peak_threads",
    "sampled_peak_rss_kib", "sampled_peak_output_run_bytes",
    "hard_wall_timeout_triggered", "process_limit_triggered",
    "thread_limit_triggered", "artifact_limit_triggered",
    "monitor_error", "final_pgid_empty", "residual_group_cleanup",
    "kill_escalation_used", "runner_exit_status",
    "wrapper_incoming_status"
  )
  if (!identical(names(wrapper), c("field", "value")) ||
      nrow(wrapper) != length(expected_wrapper_fields) ||
      !setequal(wrapper$field, expected_wrapper_fields) ||
      anyNA(wrapper) || anyDuplicated(wrapper$field) ||
      !identical(
        names(resources),
        c("metric", "value", "limit", "pass", "enforcement")
      ) ||
      nrow(resources) != length(expected_resource_metrics) ||
      !setequal(resources$metric, expected_resource_metrics) ||
      anyNA(resources$metric) || anyDuplicated(resources$metric) ||
      any(as.character(resources$pass) != "TRUE") ||
      !identical(
        names(monitor_trace),
        c(
          "elapsed_seconds", "processes", "threads", "rss_kib",
          "output_run_bytes"
        )
      ) ||
      !nrow(monitor_trace) ||
      any(!vapply(
        monitor_trace,
        function(value) {
          is.numeric(value) && all(is.finite(value)) && all(value >= 0)
        },
        logical(1L)
      ))) {
    stop("The one-cell benchmark monitor evidence is malformed.",
         call. = FALSE)
  }
  wrapper_fields <- stats::setNames(
    as.character(wrapper$value), wrapper$field
  )
  resource_fields <- stats::setNames(
    as.character(resources$value), resources$metric
  )
  resource_limits <- stats::setNames(
    as.character(resources$limit), resources$metric
  )
  trace_maximum <- function(name) max(monitor_trace[[name]])
  runtime_primary <- recorded_runtime[
    recorded_runtime$package == "rqrgibbs", , drop = FALSE
  ]
  runtime_external <- recorded_runtime[
    recorded_runtime$package == "exdqlm", , drop = FALSE
  ]
  expected_plan_status <- comparable(expected_plan, plan_fields)
  observed_plan_status <- comparable(plan_status, plan_fields)
  expected_plan_keys <- paste(
    expected_plan$cell_id, expected_plan$chain, expected_plan$seed,
    sep = "|"
  )
  chain_hash_keys <- paste(
    chain_hashes$cell_id, chain_hashes$chain, chain_hashes$seed,
    sep = "|"
  )
  status_hash_keys <- paste(
    plan_status$cell_id, plan_status$chain, plan_status$seed,
    sep = "|"
  )
  initialization_profiles <- config$mcmc$initialization_profiles
  expected_initialization_profile <- names(initialization_profiles)[
    expected_plan$chain
  ]
  expected_midpoint_shift <- vapply(
    expected_initialization_profile,
    function(name) initialization_profiles[[name]]$midpoint_shift,
    numeric(1L)
  )
  expected_initial_half_width <- vapply(
    expected_initialization_profile,
    function(name) initialization_profiles[[name]]$half_width,
    numeric(1L)
  )
  expected_rhs_multiplier <- vapply(
    expected_initialization_profile,
    function(name) initialization_profiles[[name]]$rhs_scale_multiplier,
    numeric(1L)
  )
  expected_rhs_initialization <- startsWith(
    expected_plan$prior_id, "rhs_ns"
  )
  initialization_contract_pass <-
    nrow(initialization) == nrow(expected_plan) &&
    identical(
      as.character(initialization$profile),
      expected_initialization_profile
    ) &&
    all(vapply(
      initialization$n_features,
      rqr_ordinary_v1_is_integer_scalar,
      logical(1L), minimum = 1
    )) &&
    identical(
      as.numeric(initialization$midpoint_shift),
      unname(expected_midpoint_shift)
    ) &&
    identical(
      as.numeric(initialization$initial_half_width),
      unname(expected_initial_half_width)
    ) &&
    identical(
      as.character(initialization$rhs_prior_state_initialized),
      ifelse(expected_rhs_initialization, "TRUE", "FALSE")
    ) &&
    identical(
      as.numeric(initialization$rhs_scale_multiplier),
      ifelse(expected_rhs_initialization, expected_rhs_multiplier, NA_real_)
    ) &&
    identical(
      as.character(initialization$prior_state_draws_retained),
      ifelse(expected_rhs_initialization, "TRUE", "FALSE")
    ) &&
    !anyDuplicated(initialization$initial_state_digest) &&
    identical(
      as.character(initialization$design_digest),
      rep(attested_design$design$semantic_digest, nrow(expected_plan))
    )
  if (!exact_schema(
        status,
        c(
          "mode", "status", "source_commit", "fits_executed",
          "benchmark_fits_executed", "bounded_fits_executed",
          "matched_simulation_authorized"
        )
      ) ||
      !exact_schema(source, c(
        "repository", "branch", "commit", "clean", "config_digest",
        "authorization_status", "reviewed_implementation_commit",
        "authorization_delta_verified", "rng_kind"
      )) ||
      !exact_schema(recorded_config, c(
        "config_id", "config_digest", "config_path", "execute_enabled"
      )) ||
      !exact_schema(output_resources, c(
        "monitor_active", "cleanup_traps", "final_pgid_sweep",
        "timeout_seconds", "maximum_processes", "maximum_threads",
        "maximum_artifact_bytes", "contract_pass"
      )) ||
      !exact_schema(plan, plan_fields) ||
      !exact_schema(plan_status, plan_status_fields) ||
      !exact_schema(initialization, c(
        "cell_id", "chain", "seed", "fixture_id", "prior_id",
        "learning_rate_mode", "profile", "n_features",
        "midpoint_shift", "initial_half_width",
        "rhs_prior_state_initialized", "rhs_scale_multiplier",
        "prior_state_draws_retained", "initial_state_digest",
        "design_digest"
      )) ||
      !exact_schema(diagnostics, c(
        "estimand", "rhat", "ess_bulk", "ess_tail", "mcse_mean",
        "pass", "cell_id", "family", "fixture_id", "prior_id",
        "learning_rate_mode"
      )) ||
      !exact_schema(summaries, c(
        "estimand", "mean", "sd", "q05", "q25", "median", "q75",
        "q95", "cell_id", "chain", "seed"
      )) ||
      !exact_schema(checkpoints, c(
        "cell_id", "chain", "seed", "completed_iterations",
        "checkpoint_digest", "continuation_history_digest",
        "continuation_generation", "numerical_repair_count",
        "exact_joint_target", "promotion_eligible"
      )) ||
      !exact_schema(provenance, c(
        "cell_id", "chain", "family", "primary_source_commit",
        "primary_runtime_tree_digest", "primary_runtime_source_match",
        "required_external_repositories",
        "external_exdqlm_runtime_tree_digest",
        "external_exdqlm_runtime_source_match",
        "reproducibility_eligible", "outer_fit_schema", "design_schema",
        "materialization_receipt_schema",
        "materialization_receipt_valid",
        "materialization_external_binding_verified",
        "outer_reproducibility_eligible", "outer_promotion_eligible"
      )) ||
      !exact_schema(swaps, c(
        "cell_id", "chain", "transitions", "swaps", "swap_fraction", "role"
      )) ||
      !exact_schema(rhs_sidecar, c(
        "estimand", "mean", "sd", "q05", "q25", "median", "q75",
        "q95", "cell_id", "chain", "seed", "role"
      )) ||
      !exact_schema(fixed_parameters, c(
        "parameter", "root", "expected_value", "retained_draws",
        "exact_identity", "status", "cell_id", "chain", "seed"
      )) ||
      !exact_schema(future, c(
        "cell_id", "chain", "seed", "design_digest",
        "fit_schema", "future_design_schema", "future_verification_schema",
        "future_contract_verified", "legacy_future_matrix",
        "parent_design_materialization_external_binding_verified",
        "parent_fit_reproducibility_eligible",
        "parent_fit_promotion_eligible",
        "future_external_provenance_bound",
        "future_reproducibility_eligible", "future_promotion_eligible",
        "future_promotion_status", "response_predictive_draws",
        "future_design_digest", "status"
      )) ||
      !exact_schema(chain_hashes, chain_hash_fields) ||
      !exact_schema(failure, c(
        "timestamp_utc", "mode", "cell_id", "chain",
        "error_class", "message"
      )) ||
      nrow(status) != 1L ||
      !identical(status$mode[[1L]], "benchmark-one-cell") ||
      !identical(status$status[[1L]], "pass") ||
      !identical(tolower(status$source_commit[[1L]]), reviewed) ||
      !identical(as.integer(status$fits_executed[[1L]]), 4L) ||
      !identical(
        as.integer(status$benchmark_fits_executed[[1L]]), 4L
      ) ||
      !identical(as.integer(status$bounded_fits_executed[[1L]]), 0L) ||
      !identical(
        as.character(status$matched_simulation_authorized[[1L]]), "FALSE"
      ) ||
      nrow(source) != 1L ||
      !identical(source$repository[[1L]], "RQR-GIBBS") ||
      !identical(source$branch[[1L]], "main") ||
      !identical(as.character(source$clean[[1L]]), "TRUE") ||
      !identical(tolower(source$commit[[1L]]), reviewed) ||
      !identical(source$config_digest[[1L]], expected_config_digest) ||
      !identical(
        source$authorization_status[[1L]],
        "source_candidate_execution_disabled"
      ) ||
      !identical(
        as.character(source$authorization_delta_verified[[1L]]), "FALSE"
      ) ||
      nrow(recorded_config) != 1L ||
      !identical(
        recorded_config$config_digest[[1L]], expected_config_digest
      ) ||
      !identical(
        as.character(recorded_config$execute_enabled[[1L]]), "FALSE"
      ) ||
      !identical(
        comparable(plan, plan_fields),
        comparable(expected_plan, plan_fields)
      ) ||
      nrow(plan_status) != 4L ||
      !identical(observed_plan_status, expected_plan_status) ||
      any(plan_status$status != "pass") ||
      any(!grepl("^[0-9a-f]{64}$", plan_status$fit_sha256)) ||
      nrow(chain_hashes) != 4L ||
      !identical(chain_hash_keys, expected_plan_keys) ||
      !identical(status_hash_keys, expected_plan_keys) ||
      !identical(
        as.character(chain_hashes$relative_path), expected_chain_names
      ) ||
      any(!vapply(
        chain_hashes$byte_count, rqr_ordinary_v1_is_integer_scalar,
        logical(1L), minimum = 1
      )) ||
      any(!grepl("^[0-9a-f]{64}$", chain_hashes$sha256)) ||
      !identical(
        as.character(plan_status$fit_sha256),
        as.character(chain_hashes$sha256)
      ) ||
      nrow(initialization) != 4L ||
      !initialization_contract_pass ||
      !identical(
        paste(
          initialization$cell_id, initialization$chain,
          initialization$seed, sep = "|"
        ),
        expected_plan_keys
      ) ||
      !identical(
        comparable(
          initialization,
          c(
            "cell_id", "chain", "seed", "fixture_id", "prior_id",
            "learning_rate_mode"
          )
        ),
        comparable(
          expected_plan,
          c(
            "cell_id", "chain", "seed", "fixture_id", "prior_id",
            "learning_rate_mode"
          )
        )
      ) ||
      any(!grepl(
        "^[0-9a-f]{64}$",
        c(initialization$initial_state_digest, initialization$design_digest)
      )) ||
      !identical(
        as.character(diagnostics$estimand), expected_estimands
      ) ||
      any(diagnostics$cell_id != "BENCH01") ||
      any(diagnostics$family != "desn") ||
      any(diagnostics$fixture_id != "D02") ||
      any(diagnostics$prior_id != "rhs_ns_fixed") ||
      any(
        diagnostics$learning_rate_mode !=
          "learned_pseudoresidual_normalized"
      ) ||
      anyDuplicated(diagnostics$estimand) ||
      any(!is.finite(diagnostics$rhat)) ||
      any(diagnostics$rhat > config$gates$maximum_rank_normalized_rhat) ||
      any(!is.finite(diagnostics$ess_bulk)) ||
      any(diagnostics$ess_bulk < config$gates$minimum_bulk_ess) ||
      any(!is.finite(diagnostics$ess_tail)) ||
      any(diagnostics$ess_tail < config$gates$minimum_tail_ess) ||
      any(!is.finite(diagnostics$mcse_mean)) ||
      any(!diagnostics$pass) ||
      nrow(summaries) != 4L * length(expected_estimands) ||
      !identical(
        as.character(summaries$estimand),
        rep(expected_estimands, 4L)
      ) ||
      !identical(
        as.integer(summaries$chain),
        rep(1:4, each = length(expected_estimands))
      ) ||
      !identical(
        as.integer(summaries$seed),
        rep(expected_plan$seed, each = length(expected_estimands))
      ) ||
      any(summaries$cell_id != "BENCH01") ||
      any(!is.finite(as.matrix(summaries[c(
        "mean", "sd", "q05", "q25", "median", "q75", "q95"
      )]))) ||
      nrow(checkpoints) != 4L ||
      !identical(
        paste(
          checkpoints$cell_id, checkpoints$chain, checkpoints$seed,
          sep = "|"
        ),
        expected_plan_keys
      ) ||
      any(checkpoints$numerical_repair_count != 0L) ||
      any(
        checkpoints$completed_iterations !=
          config$mcmc$burn_in +
            config$mcmc$retained_per_chain * config$mcmc$thin
      ) ||
      any(checkpoints$continuation_generation != 0L) ||
      any(!checkpoints$exact_joint_target) ||
      any(!checkpoints$promotion_eligible) ||
      any(!grepl(
        "^[0-9a-f]{64}$",
        c(
          checkpoints$checkpoint_digest,
          checkpoints$continuation_history_digest
        )
      )) ||
      nrow(provenance) != 4L ||
      any(provenance$family != "desn") ||
      any(tolower(provenance$primary_source_commit) != reviewed) ||
      any(
        provenance$primary_runtime_tree_digest !=
          runtime_primary$runtime_tree_digest[[1L]]
      ) ||
      any(!provenance$primary_runtime_source_match) ||
      any(provenance$required_external_repositories != "exdqlm") ||
      any(
        provenance$external_exdqlm_runtime_tree_digest !=
          runtime_external$runtime_tree_digest[[1L]]
      ) ||
      any(!provenance$external_exdqlm_runtime_source_match) ||
      any(!provenance$reproducibility_eligible) ||
      any(
        provenance$outer_fit_schema !=
          config$desn_schema_contract[["fit"]]
      ) ||
      any(
        provenance$design_schema !=
          config$desn_schema_contract[["design"]]
      ) ||
      any(
        provenance$materialization_receipt_schema !=
          config$desn_schema_contract[["materialization_receipt"]]
      ) ||
      any(!provenance$materialization_receipt_valid) ||
      any(!provenance$materialization_external_binding_verified) ||
      any(!provenance$outer_reproducibility_eligible) ||
      any(!provenance$outer_promotion_eligible) ||
      nrow(swaps) != 4L ||
      !identical(
        paste(swaps$cell_id, swaps$chain, sep = "|"),
        paste("BENCH01", 1:4, sep = "|")
      ) ||
      any(swaps$role != "sidecar_only") ||
      any(!is.finite(swaps$swap_fraction)) ||
      nrow(rhs_sidecar) != 4L * length(expected_rhs_sidecar) ||
      !identical(
        as.character(rhs_sidecar$estimand),
        rep(expected_rhs_sidecar, 4L)
      ) ||
      !identical(
        as.integer(rhs_sidecar$chain),
        rep(1:4, each = length(expected_rhs_sidecar))
      ) ||
      !identical(
        as.integer(rhs_sidecar$seed),
        rep(expected_plan$seed, each = length(expected_rhs_sidecar))
      ) ||
      any(rhs_sidecar$cell_id != "BENCH01") ||
      any(rhs_sidecar$role != "root_specific_sidecar_only") ||
      any(!is.finite(as.matrix(rhs_sidecar[c(
        "mean", "sd", "q05", "q25", "median", "q75", "q95"
      )]))) ||
      nrow(fixed_parameters) != 8L ||
      !identical(
        as.character(fixed_parameters$parameter),
        rep("zeta2_fixed", 8L)
      ) ||
      !identical(
        as.character(fixed_parameters$root),
        rep(c("root1", "root2"), 4L)
      ) ||
      !identical(
        as.integer(fixed_parameters$chain),
        rep(1:4, each = 2L)
      ) ||
      !identical(
        as.integer(fixed_parameters$seed),
        rep(expected_plan$seed, each = 2L)
      ) ||
      any(fixed_parameters$cell_id != "BENCH01") ||
      any(
        as.numeric(fixed_parameters$expected_value) !=
          config$fixtures$F05$prior$zeta2_fixed
      ) ||
      any(
        as.integer(fixed_parameters$retained_draws) !=
          config$mcmc$retained_per_chain
      ) ||
      any(!fixed_parameters$exact_identity) ||
      any(fixed_parameters$status != "pass") ||
      nrow(future) != 4L ||
      !identical(
        paste(future$cell_id, future$chain, future$seed, sep = "|"),
        expected_plan_keys
      ) ||
      any(
        future$design_digest != attested_design$design$semantic_digest
      ) ||
      any(future$status != "pass") ||
      any(!future$future_contract_verified) ||
      any(future$legacy_future_matrix) ||
      any(
        !future$
          parent_design_materialization_external_binding_verified
      ) ||
      any(!future$parent_fit_reproducibility_eligible) ||
      any(!future$parent_fit_promotion_eligible) ||
      any(future$future_external_provenance_bound) ||
      any(future$future_reproducibility_eligible) ||
      any(future$future_promotion_eligible) ||
      any(
        future$future_promotion_status !=
          "verified_future_contract_unattested_materialization"
      ) ||
      any(future$response_predictive_draws) ||
      nrow(failure) != 0L ||
      nrow(output_resources) != 1L ||
      !isTRUE(output_resources$contract_pass[[1L]]) ||
      !isTRUE(output_resources$monitor_active[[1L]]) ||
      !isTRUE(output_resources$cleanup_traps[[1L]]) ||
      !isTRUE(output_resources$final_pgid_sweep[[1L]]) ||
      !identical(
        as.numeric(output_resources$timeout_seconds[[1L]]),
        60 * as.numeric(config$resources$hard_timeout_minutes)
      ) ||
      !identical(
        as.numeric(output_resources$maximum_processes[[1L]]),
        as.numeric(config$resources$maximum_processes)
      ) ||
      !identical(
        as.numeric(output_resources$maximum_threads[[1L]]),
        as.numeric(config$resources$maximum_threads)
      ) ||
      !identical(
        as.numeric(output_resources$maximum_artifact_bytes[[1L]]),
        as.numeric(config$resources$maximum_artifact_bytes)
      ) ||
      !identical(
        wrapper_fields[["schema_version"]],
        "rqrgibbs_ordinary_v1_wrapper/1.0.0"
      ) ||
      !identical(wrapper_fields[["mode"]], "benchmark-one-cell") ||
      !identical(wrapper_fields[["runner_exit_status"]], "0") ||
      !identical(wrapper_fields[["resource_pass"]], "TRUE") ||
      !identical(
        wrapper_fields[["monitor_kind"]], "pgid_sampled_fail_closed"
      ) ||
      !identical(
        wrapper_fields[["sampled_resource_maxima_are_kernel_hard"]],
        "FALSE"
      ) ||
      !identical(wrapper_fields[["signal_received"]], "NONE") ||
      !identical(wrapper_fields[["final_pgid_empty"]], "TRUE") ||
      !identical(wrapper_fields[["residual_group_cleanup"]], "FALSE") ||
      !identical(
        tolower(wrapper_fields[["expected_primary_commit"]]),
        reviewed
      ) ||
      !identical(resource_limits[["sampled_peak_processes"]],
                 as.character(config$resources$maximum_processes)) ||
      !identical(resource_limits[["sampled_peak_threads"]],
                 as.character(config$resources$maximum_threads)) ||
      !identical(resource_limits[["sampled_peak_output_run_bytes"]],
                 as.character(config$resources$maximum_artifact_bytes)) ||
      as.numeric(resource_fields[["sampled_peak_processes"]]) <
        trace_maximum("processes") ||
      as.numeric(resource_fields[["sampled_peak_threads"]]) <
        trace_maximum("threads") ||
      as.numeric(resource_fields[["sampled_peak_rss_kib"]]) <
        trace_maximum("rss_kib") ||
      as.numeric(resource_fields[["sampled_peak_output_run_bytes"]]) <
        trace_maximum("output_run_bytes") ||
      as.numeric(resource_fields[["sampled_peak_processes"]]) >
        config$resources$maximum_processes ||
      as.numeric(resource_fields[["sampled_peak_threads"]]) >
        config$resources$maximum_threads ||
      as.numeric(resource_fields[["sampled_peak_output_run_bytes"]]) >
        config$resources$maximum_artifact_bytes ||
      !identical(resource_fields[["hard_wall_timeout_triggered"]], "FALSE") ||
      !identical(resource_fields[["process_limit_triggered"]], "FALSE") ||
      !identical(resource_fields[["thread_limit_triggered"]], "FALSE") ||
      !identical(resource_fields[["artifact_limit_triggered"]], "FALSE") ||
      !identical(resource_fields[["monitor_error"]], "FALSE") ||
      !identical(resource_fields[["final_pgid_empty"]], "TRUE") ||
      !identical(resource_fields[["residual_group_cleanup"]], "FALSE") ||
      !identical(resource_fields[["kill_escalation_used"]], "FALSE") ||
      !identical(resource_fields[["runner_exit_status"]], "0") ||
      !identical(resource_fields[["wrapper_incoming_status"]], "0")) {
    stop(
      "The representative one-cell benchmark is not a passing bound bundle.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

rqr_ordinary_v1_validate_reference_tables <- function(
    config, tables, attested_design, expected_fixture_manifest) {
  fail <- function(artifact) {
    stop(
      sprintf(
        "The reference-only %s evidence violates the frozen contract.",
        artifact
      ),
      call. = FALSE
    )
  }
  schema <- rqr_ordinary_v1_schema()
  exact_schema <- function(table, fields, rows = NULL) {
    is.data.frame(table) &&
      identical(names(table), c("schema_version", fields)) &&
      (is.null(rows) || identical(nrow(table), as.integer(rows))) &&
      !anyNA(table$schema_version) &&
      all(as.character(table$schema_version) == schema)
  }
  true <- function(x) {
    length(x) > 0L && !anyNA(x) && all(as.character(x) == "TRUE")
  }
  false <- function(x) {
    length(x) > 0L && !anyNA(x) && all(as.character(x) == "FALSE")
  }
  nonnegative_integer <- function(x, positive = FALSE) {
    length(x) > 0L && !anyNA(x) &&
      all(vapply(
        x, rqr_ordinary_v1_is_integer_scalar, logical(1L),
        minimum = if (positive) 1 else 0
      ))
  }
  hex <- function(x, width) {
    length(x) > 0L && !anyNA(x) &&
      all(grepl(sprintf("^[0-9a-fA-F]{%d}$", width), as.character(x)))
  }
  same_values <- function(observed, expected, fields) {
    if (!identical(nrow(observed), nrow(expected))) return(FALSE)
    observed <- observed[, fields, drop = FALSE]
    expected <- expected[, fields, drop = FALSE]
    observed[] <- lapply(observed, as.character)
    expected[] <- lapply(expected, as.character)
    rownames(observed) <- NULL
    rownames(expected) <- NULL
    identical(observed, expected)
  }
  unique_key <- function(table, fields) {
    !anyNA(table[, fields, drop = FALSE]) &&
      !anyDuplicated(do.call(
        paste, c(table[, fields, drop = FALSE], sep = "\r")
      ))
  }
  required_tables <- c(
    "run_status", "source_state", "runtime_attestations",
    "validation_config_digest", "reference_gates",
    "oracle_comparisons", "protected_dlm_hashes", "package_checks",
    "desn_design_checks", "desn_future_checks",
    "missingness_checks", "rhs_ns_conditional_checks",
    "continuation_checks", "history_mutation_checks",
    "fixture_manifest", "seed_ledger", "failure_log",
    "resource_summary"
  )
  if (!is.list(tables) ||
      !identical(sort(names(tables)), sort(required_tables)) ||
      !is.list(attested_design) ||
      !is.data.frame(expected_fixture_manifest)) {
    fail("table-set")
  }
  reviewed <- tolower(config$reviewed_implementation_commit %||% "")
  config_digest <- rqr_ordinary_v1_disabled_config_digest(config)

  run <- tables$run_status
  run_fields <- c(
    "mode", "status", "source_commit", "fits_executed",
    "benchmark_fits_executed", "bounded_fits_executed",
    "matched_simulation_authorized"
  )
  if (!exact_schema(run, run_fields, 1L) ||
      !identical(as.character(run$mode[[1L]]), "reference-only") ||
      !identical(as.character(run$status[[1L]]), "pass") ||
      !identical(tolower(as.character(run$source_commit[[1L]])), reviewed) ||
      !all(vapply(
        run[c(
          "fits_executed", "benchmark_fits_executed",
          "bounded_fits_executed"
        )],
        function(x) identical(as.numeric(x[[1L]]), 0),
        logical(1L)
      )) ||
      !false(run$matched_simulation_authorized)) {
    fail("run_status.csv")
  }

  source <- tables$source_state
  source_fields <- c(
    "repository", "branch", "commit", "clean", "config_digest",
    "authorization_status", "reviewed_implementation_commit",
    "authorization_delta_verified", "rng_kind"
  )
  if (!exact_schema(source, source_fields, 1L) ||
      !identical(as.character(source$repository[[1L]]), "RQR-GIBBS") ||
      !identical(as.character(source$branch[[1L]]), "main") ||
      !identical(tolower(as.character(source$commit[[1L]])), reviewed) ||
      !true(source$clean) ||
      !identical(as.character(source$config_digest[[1L]]), config_digest) ||
      !identical(
        as.character(source$authorization_status[[1L]]),
        "source_candidate_execution_disabled"
      ) ||
      !is.na(source$reviewed_implementation_commit[[1L]]) ||
      !false(source$authorization_delta_verified) ||
      is.na(source$rng_kind[[1L]]) ||
      !nzchar(as.character(source$rng_kind[[1L]]))) {
    fail("source_state.csv")
  }

  configuration <- tables$validation_config_digest
  config_fields <- c(
    "config_id", "config_digest", "config_path", "execute_enabled"
  )
  config_path <- attr(config, "path") %||% NA_character_
  if (!exact_schema(configuration, config_fields, 1L) ||
      !identical(
        as.character(configuration$config_id[[1L]]), config$config_id
      ) ||
      !identical(
        as.character(configuration$config_digest[[1L]]), config_digest
      ) ||
      is.na(configuration$config_path[[1L]]) ||
      !nzchar(as.character(configuration$config_path[[1L]])) ||
      (!is.na(config_path) && !identical(
        normalizePath(
          as.character(configuration$config_path[[1L]]),
          winslash = "/", mustWork = FALSE
        ),
        normalizePath(config_path, winslash = "/", mustWork = FALSE)
      )) ||
      !false(configuration$execute_enabled)) {
    fail("validation_config_digest.csv")
  }

  runtime <- tables$runtime_attestations
  runtime_fields <- c(
    "package", "version", "source_commit", "runtime_path",
    "runtime_tree_digest", "runtime_source_match",
    "reproducibility_eligible", "runtime_attestation_schema",
    "attestation_sha256", "R_version", "platform", "compiler",
    "BLAS", "LAPACK", "posterior_version"
  )
  if (!exact_schema(runtime, runtime_fields, 2L) ||
      !setequal(as.character(runtime$package), c("rqrgibbs", "exdqlm")) ||
      !unique_key(runtime, "package") ||
      !true(runtime$runtime_source_match) ||
      !true(runtime$reproducibility_eligible) ||
      !hex(runtime$runtime_tree_digest, 64L) ||
      !hex(runtime$attestation_sha256, 64L) ||
      anyNA(runtime[, setdiff(runtime_fields, c(
        "runtime_source_match", "reproducibility_eligible"
      )), drop = FALSE]) ||
      any(!vapply(
        runtime[c(
          "version", "runtime_path", "runtime_attestation_schema",
          "R_version", "platform", "compiler", "BLAS", "LAPACK",
          "posterior_version"
        )],
        function(x) all(nzchar(as.character(x))),
        logical(1L)
      ))) {
    fail("runtime_attestations.csv")
  }
  runtime_commit <- setNames(
    tolower(as.character(runtime$source_commit)),
    as.character(runtime$package)
  )
  if (!identical(unname(runtime_commit[["rqrgibbs"]]), reviewed) ||
      !identical(
        unname(runtime_commit[["exdqlm"]]),
        tolower(config$pinned_exdqlm$commit)
      )) {
    fail("runtime_attestations.csv")
  }

  gates <- tables$reference_gates
  gate_fields <- c("gate_id", "status", "detail")
  expected_gate_ids <- c(
    "deterministic_oracles", "protected_dlm_candidate_sha256",
    "ordinary_v1_native_reference_tests",
    "pinned_attested_desn_materialization",
    "attested_desn_end_to_end_reference_cells"
  )
  if (!exact_schema(gates, gate_fields, length(expected_gate_ids)) ||
      !identical(as.character(gates$gate_id), expected_gate_ids) ||
      !unique_key(gates, "gate_id") ||
      any(as.character(gates$status) != "pass") ||
      anyNA(gates$detail) || any(!nzchar(as.character(gates$detail)))) {
    fail("reference_gates.csv")
  }

  oracle <- tables$oracle_comparisons
  oracle_fields <- c(
    "oracle_id", "expected", "actual", "tolerance", "pass"
  )
  oracle_ids <- c(
    "loss_sign_partition", "normalized_lambda_shape",
    "normalized_lambda_rate_finite", "desn_training_missing_rows",
    "desn_future_contracts"
  )
  oracle_tolerances <- c(
    "0", "exact", "finite_positive", "exact", "exact"
  )
  observed_shape <- config$lambda_prior$shape +
    sum(!is.na(config$fixtures$F02$y))
  oracle_expected <- c(
    "0", as.character(observed_shape), "TRUE", "3|9", "3"
  )
  if (!exact_schema(oracle, oracle_fields, length(oracle_ids)) ||
      !identical(as.character(oracle$oracle_id), oracle_ids) ||
      !unique_key(oracle, "oracle_id") ||
      !identical(as.character(oracle$tolerance), oracle_tolerances) ||
      !identical(as.character(oracle$expected), oracle_expected) ||
      !identical(as.character(oracle$actual), oracle_expected) ||
      !true(oracle$pass)) {
    fail("oracle_comparisons.csv")
  }

  dlm <- tables$protected_dlm_hashes
  dlm_fields <- c(
    "relative_path", "expected_sha256", "actual_sha256", "pass"
  )
  expected_dlm <- data.frame(
    relative_path = names(config$protected_dlm_sha256),
    expected_sha256 = unname(config$protected_dlm_sha256),
    actual_sha256 = unname(config$protected_dlm_sha256),
    pass = TRUE,
    stringsAsFactors = FALSE
  )
  if (!exact_schema(dlm, dlm_fields, nrow(expected_dlm)) ||
      !unique_key(dlm, "relative_path") ||
      !same_values(dlm, expected_dlm, dlm_fields) ||
      !true(dlm$pass)) {
    fail("protected_dlm_hashes.csv")
  }

  packages <- tables$package_checks
  package_fields <- c(
    "test_file", "test_blocks", "expectations", "failures", "errors",
    "warnings", "skipped", "elapsed_seconds", "status", "detail"
  )
  expected_test_files <- rqr_ordinary_v1_reference_test_names()
  if (!exact_schema(packages, package_fields, length(expected_test_files)) ||
      !identical(as.character(packages$test_file), expected_test_files) ||
      !unique_key(packages, "test_file") ||
      !nonnegative_integer(packages$test_blocks, positive = TRUE) ||
      !nonnegative_integer(packages$expectations, positive = TRUE) ||
      !nonnegative_integer(packages$failures) ||
      !nonnegative_integer(packages$errors) ||
      !nonnegative_integer(packages$warnings) ||
      !nonnegative_integer(packages$skipped) ||
      any(packages$failures != 0) || any(packages$errors != 0) ||
      any(packages$warnings != 0) || any(packages$skipped != 0) ||
      anyNA(packages$elapsed_seconds) ||
      any(!is.finite(as.numeric(packages$elapsed_seconds))) ||
      any(as.numeric(packages$elapsed_seconds) < 0) ||
      any(as.character(packages$status) != "pass") ||
      any(as.character(packages$detail) != "all expectations passed")) {
    fail("package_checks.csv")
  }

  design <- tables$desn_design_checks
  design_fields <- c(
    "design_digest", "file_sha256", "source_commit",
    "runtime_tree_digest", "runtime_attestation_sha256",
    "runtime_source_match", "design_schema", "receipt_schema",
    "receipt_digest", "materializer_arguments_digest",
    "materialized_design_payload_digest", "receipt_valid",
    "external_state_match", "runtime_attestation_sha256_verified",
    "materialization_reproducibility_eligible",
    "materialization_status", "fit_schema", "future_design_schema"
  )
  exdqlm_runtime <- runtime[
    runtime$package == "exdqlm", , drop = FALSE
  ]
  receipt <- attested_design$receipt %||% list()
  receipt_status <- attested_design$receipt_status %||% list()
  if (!exact_schema(design, design_fields, 1L) ||
      !identical(
        as.character(design$design_digest[[1L]]),
        as.character(attested_design$digest)
      ) ||
      !identical(
        as.character(design$file_sha256[[1L]]),
        as.character(attested_design$file_sha256)
      ) ||
      !identical(
        as.character(design$source_commit[[1L]]),
        config$pinned_exdqlm$commit
      ) ||
      !identical(
        as.character(design$runtime_tree_digest[[1L]]),
        as.character(exdqlm_runtime$runtime_tree_digest[[1L]])
      ) ||
      !identical(
        as.character(design$runtime_attestation_sha256[[1L]]),
        as.character(exdqlm_runtime$attestation_sha256[[1L]])
      ) ||
      !identical(
        as.character(design$receipt_schema[[1L]]),
        config$desn_schema_contract[["materialization_receipt"]]
      ) ||
      !identical(
        as.character(design$design_schema[[1L]]),
        config$desn_schema_contract[["design"]]
      ) ||
      !identical(
        as.character(design$fit_schema[[1L]]),
        config$desn_schema_contract[["fit"]]
      ) ||
      !identical(
        as.character(design$future_design_schema[[1L]]),
        config$desn_schema_contract[["future_design"]]
      ) ||
      !identical(
        as.character(design$receipt_digest[[1L]]),
        as.character(receipt_status$receipt_digest)
      ) ||
      !identical(
        as.character(design$materializer_arguments_digest[[1L]]),
        as.character(receipt$materializer_arguments_digest)
      ) ||
      !identical(
        as.character(design$materialized_design_payload_digest[[1L]]),
        as.character(receipt$materialized_design_payload_digest)
      ) ||
      !hex(unlist(design[c(
        "design_digest", "file_sha256", "runtime_tree_digest",
        "runtime_attestation_sha256", "receipt_digest",
        "materializer_arguments_digest",
        "materialized_design_payload_digest"
      )], use.names = FALSE), 64L) ||
      !true(design$runtime_source_match) ||
      !true(design$receipt_valid) ||
      !true(design$external_state_match) ||
      !true(design$runtime_attestation_sha256_verified) ||
      !true(design$materialization_reproducibility_eligible) ||
      !identical(
        as.character(design$materialization_status[[1L]]),
        "verified_current_isolated_materialization"
      )) {
    fail("desn_design_checks.csv")
  }

  future <- tables$desn_future_checks
  future_fields <- c(
    "reference_id", "prior_id", "learning_rate_mode", "seed",
    "design_digest", "completed_iterations", "continuation_generation",
    "promotion_eligible", "fit_schema", "future_design_schema",
    "future_verification_schema", "future_contract_verified",
    "legacy_future_matrix",
    "parent_design_materialization_external_binding_verified",
    "parent_fit_reproducibility_eligible",
    "parent_fit_promotion_eligible",
    "future_external_provenance_bound",
    "future_reproducibility_eligible", "future_promotion_eligible",
    "future_promotion_status", "response_predictive_draws",
    "future_design_digest", "status"
  )
  reference_ledger <- config$seed_ledger[
    config$seed_ledger$purpose == "attested_desn_end_to_end",
    , drop = FALSE
  ]
  expected_reference <- data.frame(
    reference_id = reference_ledger$seed_id,
    prior_id = reference_ledger$prior_id,
    learning_rate_mode = reference_ledger$learning_rate_mode,
    seed = reference_ledger$seed,
    stringsAsFactors = FALSE
  )
  if (!exact_schema(future, future_fields, 4L) ||
      !unique_key(future, "reference_id") ||
      !same_values(
        future, expected_reference,
        c("reference_id", "prior_id", "learning_rate_mode", "seed")
      ) ||
      any(as.character(future$design_digest) !=
            as.character(attested_design$design$semantic_digest)) ||
      !nonnegative_integer(future$completed_iterations, positive = TRUE) ||
      any(as.numeric(future$completed_iterations) != 3) ||
      !nonnegative_integer(future$continuation_generation) ||
      any(as.numeric(future$continuation_generation) != 1) ||
      !true(future$promotion_eligible) ||
      any(as.character(future$fit_schema) !=
            config$desn_schema_contract[["fit"]]) ||
      any(as.character(future$future_design_schema) !=
            config$desn_schema_contract[["future_design"]]) ||
      any(as.character(future$future_verification_schema) !=
            config$desn_schema_contract[["future_verification"]]) ||
      !true(future$future_contract_verified) ||
      !false(future$legacy_future_matrix) ||
      !true(
        future$parent_design_materialization_external_binding_verified
      ) ||
      !true(future$parent_fit_reproducibility_eligible) ||
      !true(future$parent_fit_promotion_eligible) ||
      !false(future$future_external_provenance_bound) ||
      !false(future$future_reproducibility_eligible) ||
      !false(future$future_promotion_eligible) ||
      any(
        as.character(future$future_promotion_status) !=
          "verified_future_contract_unattested_materialization"
      ) ||
      !false(future$response_predictive_draws) ||
      !hex(future$future_design_digest, 64L) ||
      any(as.character(future$status) != "pass")) {
    fail("desn_future_checks.csv")
  }

  category_fields <- c(
    "check_id", "evidence_source", "expectations", "status", "detail"
  )
  expected_categories <- list(
    missingness_checks = data.frame(
      check_id = c(
        "fixed_design_observed_mask_and_rng_contract",
        "desn_training_missing_indices"
      ),
      evidence_source = c(
        "test-rqr-native-fixed-design-v1.R",
        "oracle_comparisons.csv"
      ),
      stringsAsFactors = FALSE
    ),
    rhs_ns_conditional_checks = data.frame(
      check_id = "native_rhs_ns_joint_and_full_conditionals",
      evidence_source = "test-rqr-native-rhs-ns.R",
      stringsAsFactors = FALSE
    ),
    continuation_checks = data.frame(
      check_id = c(
        "static_and_custom_desn_6_equals_2_plus_2_plus_2",
        paste0("attested_desn_", expected_reference$reference_id)
      ),
      evidence_source = c(
        "test-rqr-native-ordinary-v1-reference-cells.R",
        rep("desn_future_checks.csv", 4L)
      ),
      stringsAsFactors = FALSE
    ),
    history_mutation_checks = data.frame(
      check_id = c(
        "static_checkpoint_and_history_mutations",
        "desn_envelope_and_materialization_mutations",
        "authorization_and_evidence_mutations"
      ),
      evidence_source = c(
        "test-rqr-native-fixed-design-v1.R",
        "test-rqr-native-desn-fit-v1.R",
        "test-rqr-native-ordinary-v1-validation-runner.R"
      ),
      stringsAsFactors = FALSE
    )
  )
  package_expectations <- setNames(
    as.numeric(packages$expectations), as.character(packages$test_file)
  )
  for (name in names(expected_categories)) {
    observed <- tables[[name]]
    expected <- expected_categories[[name]]
    if (!exact_schema(observed, category_fields, nrow(expected)) ||
        !unique_key(observed, "check_id") ||
        !same_values(
          observed, expected, c("check_id", "evidence_source")
        ) ||
        !nonnegative_integer(observed$expectations, positive = TRUE) ||
        any(as.character(observed$status) != "pass") ||
        anyNA(observed$detail) ||
        any(!nzchar(as.character(observed$detail)))) {
      fail(paste0(name, ".csv"))
    }
    expected_count <- vapply(
      as.character(observed$evidence_source),
      function(source_name) {
        if (source_name %in% names(package_expectations)) {
          package_expectations[[source_name]]
        } else {
          1
        }
      },
      numeric(1L)
    )
    if (!identical(
          as.numeric(observed$expectations), as.numeric(expected_count)
        )) {
      fail(paste0(name, ".csv"))
    }
  }

  fixture <- tables$fixture_manifest
  fixture_fields <- c("fixture_id", "class", "digest")
  if (!exact_schema(
        fixture, fixture_fields, nrow(expected_fixture_manifest)
      ) ||
      !unique_key(fixture, "fixture_id") ||
      !same_values(
        fixture, expected_fixture_manifest, fixture_fields
      ) ||
      !hex(fixture$digest, 64L)) {
    fail("fixture_manifest.csv")
  }

  seed <- tables$seed_ledger
  seed_fields <- names(config$seed_ledger)
  if (!exact_schema(seed, seed_fields, nrow(config$seed_ledger)) ||
      !unique_key(seed, "seed_id") ||
      !same_values(seed, config$seed_ledger, seed_fields)) {
    fail("seed_ledger.csv")
  }

  failure <- tables$failure_log
  failure_fields <- c(
    "timestamp_utc", "mode", "cell_id", "chain",
    "error_class", "message"
  )
  if (!exact_schema(failure, failure_fields, 0L)) {
    fail("failure_log.csv")
  }

  resources <- tables$resource_summary
  resource_fields <- c(
    "monitor_active", "cleanup_traps", "final_pgid_sweep",
    "timeout_seconds", "maximum_processes", "maximum_threads",
    "maximum_artifact_bytes", "contract_pass"
  )
  if (!exact_schema(resources, resource_fields, 1L) ||
      !true(resources$monitor_active) ||
      !true(resources$cleanup_traps) ||
      !true(resources$final_pgid_sweep) ||
      !true(resources$contract_pass) ||
      !identical(
        as.numeric(resources$timeout_seconds[[1L]]),
        60 * as.numeric(config$resources$hard_timeout_minutes)
      ) ||
      !identical(
        as.numeric(resources$maximum_processes[[1L]]),
        as.numeric(config$resources$maximum_processes)
      ) ||
      !identical(
        as.numeric(resources$maximum_threads[[1L]]),
        as.numeric(config$resources$maximum_threads)
      ) ||
      !identical(
        as.numeric(resources$maximum_artifact_bytes[[1L]]),
        as.numeric(config$resources$maximum_artifact_bytes)
      )) {
    fail("resource_summary.csv")
  }
  invisible(TRUE)
}

rqr_ordinary_v1_authorize_execute <- function(
    config, source_state, runtime_table, reference_dir,
    benchmark_dir, benchmark_monitor_dir, attested_design) {
  if (!isTRUE(config$ordinary_v1_execute_enabled)) {
    stop(
      "execute-bounded is disabled in the reviewed tracked configuration.",
      call. = FALSE
    )
  }
  if (!identical(Sys.getenv("RQR_ORDINARY_V1_CONFIRM", unset = ""), "YES")) {
    stop("RQR_ORDINARY_V1_CONFIRM=YES is required.", call. = FALSE)
  }
  rqr_ordinary_v1_validate_benchmark_bundle(
    config, source_state, runtime_table, benchmark_dir,
    benchmark_monitor_dir, attested_design
  )
  if (!dir.exists(reference_dir)) {
    stop("The exact reference-only bundle is required.", call. = FALSE)
  }
  required <- c(
    "artifact_hashes.csv", "closeout.md", "continuation_checks.csv",
    "desn_design_checks.csv", "desn_future_checks.csv",
    "protected_dlm_hashes.csv", "failure_log.csv",
    "fixture_manifest.csv", "history_mutation_checks.csv",
    "missingness_checks.csv", "oracle_comparisons.csv",
    "package_checks.csv", "reference_gates.csv",
    "resource_summary.csv", "rhs_ns_conditional_checks.csv",
    "run_status.csv", "runtime_attestations.csv", "seed_ledger.csv",
    "session_info.txt", "source_state.csv",
    "validation_config_digest.csv"
  )
  if (!identical(
        sort(list.files(
          reference_dir, all.files = TRUE, no.. = TRUE,
          recursive = TRUE
        )),
        sort(required)
      ) ||
      any(vapply(
        file.path(reference_dir, required),
        function(path) nzchar(Sys.readlink(path)), logical(1L)
      )) ||
      any(file.info(file.path(
        reference_dir, c("closeout.md", "session_info.txt")
      ))$size <= 0)) {
    stop("The reference-only bundle is incomplete.", call. = FALSE)
  }
  manifest <- utils::read.csv(
    file.path(reference_dir, "artifact_hashes.csv"),
    stringsAsFactors = FALSE, check.names = FALSE,
    colClasses = c(
      schema_version = "character", relative_path = "character",
      sha256 = "character"
    )
  )
  if (!identical(
        names(manifest),
        c("schema_version", "relative_path", "byte_count", "sha256")
      ) ||
      any(
        as.character(manifest$schema_version) !=
          rqr_ordinary_v1_schema()
      ) ||
      !rqr_ordinary_v1_validate_artifact_manifest(reference_dir, manifest)) {
    stop("The reference-only artifact manifest does not match the bytes.",
         call. = FALSE)
  }
  status <- utils::read.csv(
    file.path(reference_dir, "run_status.csv"),
    stringsAsFactors = FALSE, check.names = FALSE,
    colClasses = c(source_commit = "character")
  )
  gates <- utils::read.csv(
    file.path(reference_dir, "reference_gates.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  recorded_source <- utils::read.csv(
    file.path(reference_dir, "source_state.csv"),
    stringsAsFactors = FALSE, check.names = FALSE,
    colClasses = c(
      commit = "character", config_digest = "character",
      reviewed_implementation_commit = "character"
    ),
    na.strings = c("", "NA")
  )
  recorded_runtime <- utils::read.csv(
    file.path(reference_dir, "runtime_attestations.csv"),
    stringsAsFactors = FALSE, check.names = FALSE,
    colClasses = c(
      source_commit = "character",
      runtime_tree_digest = "character",
      attestation_sha256 = "character"
    )
  )
  recorded_config <- utils::read.csv(
    file.path(reference_dir, "validation_config_digest.csv"),
    stringsAsFactors = FALSE, check.names = FALSE,
    colClasses = c(config_digest = "character")
  )
  design_checks <- utils::read.csv(
    file.path(reference_dir, "desn_design_checks.csv"),
    stringsAsFactors = FALSE, check.names = FALSE,
    colClasses = c(
      design_digest = "character", file_sha256 = "character",
      source_commit = "character", runtime_tree_digest = "character",
      runtime_attestation_sha256 = "character",
      receipt_digest = "character",
      materializer_arguments_digest = "character",
      materialized_design_payload_digest = "character"
    )
  )
  package_checks <- utils::read.csv(
    file.path(reference_dir, "package_checks.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  oracle_comparisons <- utils::read.csv(
    file.path(reference_dir, "oracle_comparisons.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  protected_dlm_hashes <- utils::read.csv(
    file.path(reference_dir, "protected_dlm_hashes.csv"),
    stringsAsFactors = FALSE, check.names = FALSE,
    colClasses = c(
      expected_sha256 = "character", actual_sha256 = "character"
    )
  )
  desn_reference_checks <- utils::read.csv(
    file.path(reference_dir, "desn_future_checks.csv"),
    stringsAsFactors = FALSE, check.names = FALSE,
    colClasses = c(
      design_digest = "character",
      future_design_digest = "character"
    )
  )
  fixture_manifest <- utils::read.csv(
    file.path(reference_dir, "fixture_manifest.csv"),
    stringsAsFactors = FALSE, check.names = FALSE,
    colClasses = c(digest = "character")
  )
  seed_ledger <- utils::read.csv(
    file.path(reference_dir, "seed_ledger.csv"),
    stringsAsFactors = FALSE, check.names = FALSE,
    na.strings = c("", "NA")
  )
  failure_log <- utils::read.csv(
    file.path(reference_dir, "failure_log.csv"),
    stringsAsFactors = FALSE, check.names = FALSE,
    na.strings = c("", "NA")
  )
  resource_summary <- utils::read.csv(
    file.path(reference_dir, "resource_summary.csv"),
    stringsAsFactors = FALSE, check.names = FALSE,
    na.strings = c("", "NA")
  )
  category_files <- c(
    "missingness_checks.csv", "rhs_ns_conditional_checks.csv",
    "continuation_checks.csv", "history_mutation_checks.csv"
  )
  category_checks <- lapply(category_files, function(file) {
    utils::read.csv(
      file.path(reference_dir, file),
      stringsAsFactors = FALSE, check.names = FALSE
    )
  })
  names(category_checks) <- category_files
  reviewed <- tolower(config$reviewed_implementation_commit)
  rqr_ordinary_v1_validate_runtime_compatibility(
    recorded_runtime, runtime_table, reviewed,
    source_state$commit[[1L]], config$pinned_exdqlm$commit
  )
  expected_fixture_manifest <- rqr_ordinary_v1_fixture_manifest(
    rqr_ordinary_v1_build_fixtures(config)
  )
  rqr_ordinary_v1_validate_reference_tables(
    config,
    tables = c(
      list(
        run_status = status,
        source_state = recorded_source,
        runtime_attestations = recorded_runtime,
        validation_config_digest = recorded_config,
        reference_gates = gates,
        oracle_comparisons = oracle_comparisons,
        protected_dlm_hashes = protected_dlm_hashes,
        package_checks = package_checks,
        desn_design_checks = design_checks,
        desn_future_checks = desn_reference_checks,
        fixture_manifest = fixture_manifest,
        seed_ledger = seed_ledger,
        failure_log = failure_log,
        resource_summary = resource_summary
      ),
      setNames(category_checks, sub("\\.csv$", "", category_files))
    ),
    attested_design = attested_design,
    expected_fixture_manifest = expected_fixture_manifest
  )
  invisible(TRUE)
}

rqr_ordinary_v1_write_common_evidence <- function(
    output_dir, config, source_state, runtime_table, fixtures,
    resource_summary) {
  rqr_ordinary_v1_atomic_csv(
    source_state, file.path(output_dir, "source_state.csv")
  )
  rqr_ordinary_v1_atomic_csv(
    runtime_table, file.path(output_dir, "runtime_attestations.csv")
  )
  rqr_ordinary_v1_atomic_csv(
    data.frame(
      config_id = config$config_id,
      config_digest = rqr_ordinary_v1_sha256_object(config),
      config_path = attr(config, "path") %||% NA_character_,
      execute_enabled = config$ordinary_v1_execute_enabled,
      stringsAsFactors = FALSE
    ),
    file.path(output_dir, "validation_config_digest.csv")
  )
  rqr_ordinary_v1_atomic_csv(
    rqr_ordinary_v1_fixture_manifest(fixtures),
    file.path(output_dir, "fixture_manifest.csv")
  )
  ledger <- config$seed_ledger
  ledger$seed <- ifelse(is.na(ledger$seed), NA, ledger$seed)
  rqr_ordinary_v1_atomic_csv(
    ledger, file.path(output_dir, "seed_ledger.csv")
  )
  rqr_ordinary_v1_atomic_csv(
    resource_summary, file.path(output_dir, "resource_summary.csv")
  )
}

rqr_ordinary_v1_write_manifest <- function(output_dir) {
  manifest <- rqr_ordinary_v1_artifact_manifest(output_dir)
  path <- rqr_ordinary_v1_atomic_csv(
    manifest, file.path(output_dir, "artifact_hashes.csv")
  )
  reread <- utils::read.csv(
    path, stringsAsFactors = FALSE, check.names = FALSE
  )
  if (!rqr_ordinary_v1_validate_artifact_manifest(output_dir, reread)) {
    stop("The final compact artifact manifest failed readback.",
         call. = FALSE)
  }
  path
}

rqr_ordinary_v1_empty_failure <- function() {
  data.frame(
    timestamp_utc = character(0),
    mode = character(0),
    cell_id = character(0),
    chain = integer(0),
    error_class = character(0),
    message = character(0),
    stringsAsFactors = FALSE
  )
}

rqr_ordinary_v1_compact_summary <- function(values) {
  if (!is.matrix(values) || is.null(colnames(values)) ||
      any(!is.finite(values))) {
    stop("Compact summaries require a finite named matrix.",
         call. = FALSE)
  }
  quantiles <- apply(
    values, 2L, stats::quantile,
    probs = c(0.05, 0.25, 0.50, 0.75, 0.95),
    names = FALSE, type = 8
  )
  if (is.null(dim(quantiles))) {
    quantiles <- matrix(quantiles, ncol = 1L)
  }
  data.frame(
    estimand = colnames(values),
    mean = colMeans(values),
    sd = apply(values, 2L, stats::sd),
    q05 = quantiles[1L, ],
    q25 = quantiles[2L, ],
    median = quantiles[3L, ],
    q75 = quantiles[4L, ],
    q95 = quantiles[5L, ],
    stringsAsFactors = FALSE
  )
}

rqr_ordinary_v1_initialization_manifest <- function(
    config, fixtures, execution_design) {
  rows <- lapply(seq_len(nrow(config$fit_plan)), function(index) {
    row <- config$fit_plan[index, , drop = FALSE]
    cell <- rqr_ordinary_v1_cell_fixture(
      config, fixtures, row, execution_design
    )
    profile_name <- names(config$mcmc$initialization_profiles)[
      row$chain[[1L]]
    ]
    profile <- config$mcmc$initialization_profiles[[profile_name]]
    init <- rqr_ordinary_v1_initial_state(
      cell$X, profile, cell$prior
    )
    rhs_state_initialized <- identical(cell$prior$type, "rhs_ns")
    retain_prior_states <- rqr_ordinary_v1_retain_prior_state_draws(
      config, cell$prior
    )
    data.frame(
      cell_id = row$cell_id[[1L]],
      chain = row$chain[[1L]],
      seed = row$seed[[1L]],
      fixture_id = row$fixture_id[[1L]],
      prior_id = row$prior_id[[1L]],
      learning_rate_mode = row$learning_rate_mode[[1L]],
      profile = profile_name,
      n_features = ncol(cell$X),
      midpoint_shift = profile$midpoint_shift,
      initial_half_width = profile$half_width,
      rhs_prior_state_initialized = rhs_state_initialized,
      rhs_scale_multiplier = if (rhs_state_initialized) {
        profile$rhs_scale_multiplier
      } else {
        NA_real_
      },
      prior_state_draws_retained = retain_prior_states,
      initial_state_digest =
        rqr_ordinary_v1_sha256_object(init),
      design_digest = if (identical(
        row$family[[1L]], "desn"
      )) execution_design$semantic_digest else
        rqr_ordinary_v1_sha256_object(cell$X),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

rqr_ordinary_v1_execution_plan_kind <- function(config, fit_plan) {
  if (identical(fit_plan, config$fit_plan)) {
    return("execute-bounded")
  }
  if (identical(fit_plan, config$benchmark_plan)) {
    return("benchmark-one-cell")
  }
  stop(
    paste(
      "Execution accepts only the exact frozen 48-fit plan or the exact",
      "four-chain benchmark plan; subsets, hybrids, and seed changes are",
      "not permitted."
    ),
    call. = FALSE
  )
}

rqr_ordinary_v1_execute <- function(
    config, fixtures, output_dir, run_dir, runtime, source_state,
    external_runtime, attested_design, fit_plan = config$fit_plan) {
  plan_kind <- rqr_ordinary_v1_execution_plan_kind(config, fit_plan)
  execution_design <- attested_design$design
  primary_control <- list(
    repo_root = rqr_ordinary_v1_find_repo(),
    expected_git_commit = source_state$commit[[1L]],
    primary_runtime_attestation = runtime$attestation
  )
  provenance_controls <- list(
    fixed_design = c(
      primary_control,
      list(
        external_repositories = list(),
        required_external_repositories = character(0)
      )
    ),
    desn = c(
      primary_control,
      list(
        external_repositories = list(
          exdqlm = external_runtime$spec
        ),
        required_external_repositories = "exdqlm"
      )
    )
  )
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
  diagnostics <- list()
  chain_hashes <- list()
  posterior_summaries <- list()
  checkpoint_rows <- list()
  provenance_rows <- list()
  root_swap_rows <- list()
  rhs_root_sidecar_rows <- list()
  fixed_parameter_rows <- list()
  future_check_rows <- list()
  plan_status <- fit_plan
  plan_status$status <- "pending"
  plan_status$fit_sha256 <- NA_character_
  execution_config <- config
  execution_config$fit_plan <- fit_plan
  initialization_manifest <- rqr_ordinary_v1_initialization_manifest(
    execution_config, fixtures, execution_design
  )
  rqr_ordinary_v1_atomic_csv(
    fit_plan, file.path(output_dir, "fit_plan.csv")
  )
  rqr_ordinary_v1_atomic_csv(
    initialization_manifest,
    file.path(output_dir, "initialization_manifest.csv")
  )
  started <- Sys.time()
  publish_chain <- function(row, result) {
    cell <- row$cell_id[[1L]]
    chain <- row$chain[[1L]]
    path <- file.path(
      run_dir, sprintf("%s_chain_%02d.rds", cell, chain)
    )
    rqr_ordinary_v1_atomic_rds(result$fit, path)
    chain_hashes[[length(chain_hashes) + 1L]] <<- data.frame(
      cell_id = cell, chain = chain, seed = row$seed[[1L]],
      relative_path = basename(path),
      byte_count = as.numeric(file.info(path)$size),
      sha256 = rqr_ordinary_v1_sha256_file(path),
      stringsAsFactors = FALSE
    )
    static <- if (inherits(result$fit, "rqr_desn_fit")) {
      result$fit$fit
    } else {
      result$fit
    }
    compact <- rqr_ordinary_v1_compact_summary(result$estimands)
    compact$cell_id <- cell
    compact$chain <- chain
    compact$seed <- row$seed[[1L]]
    posterior_summaries[[length(posterior_summaries) + 1L]] <<-
      compact
    if (startsWith(row$prior_id[[1L]], "rhs_ns")) {
      sidecar <- result$rhs_root_specific_sidecar
      if (!is.matrix(sidecar) || !ncol(sidecar) ||
          nrow(sidecar) != nrow(result$estimands) ||
          is.null(colnames(sidecar)) ||
          anyDuplicated(colnames(sidecar)) ||
          any(!is.finite(sidecar))) {
        stop("An RHS chain lacks its root-specific diagnostic sidecar.",
             call. = FALSE)
      }
      sidecar_summary <- rqr_ordinary_v1_compact_summary(sidecar)
      sidecar_summary$cell_id <- cell
      sidecar_summary$chain <- chain
      sidecar_summary$seed <- row$seed[[1L]]
      sidecar_summary$role <- "root_specific_sidecar_only"
      rhs_root_sidecar_rows[[
        length(rhs_root_sidecar_rows) + 1L
      ]] <<- sidecar_summary
      fixed_checks <- result$fixed_parameter_checks
      if (!is.data.frame(fixed_checks) ||
          !identical(
            names(fixed_checks),
            names(rqr_ordinary_v1_empty_fixed_parameter_checks())
          )) {
        stop("An RHS chain returned malformed fixed-parameter checks.",
             call. = FALSE)
      }
      if (identical(row$prior_id[[1L]], "rhs_ns_fixed")) {
        if (nrow(fixed_checks) != 2L ||
            !identical(fixed_checks$root, c("root1", "root2")) ||
            any(!fixed_checks$exact_identity) ||
            any(fixed_checks$status != "pass")) {
          stop("Fixed RHS shoulder identity evidence failed.",
               call. = FALSE)
        }
        fixed_checks$cell_id <- cell
        fixed_checks$chain <- chain
        fixed_checks$seed <- row$seed[[1L]]
        fixed_parameter_rows[[
          length(fixed_parameter_rows) + 1L
        ]] <<- fixed_checks
      } else if (nrow(fixed_checks)) {
        stop("Sampled RHS chains returned fixed-parameter evidence.",
             call. = FALSE)
      }
    } else if (!is.matrix(result$rhs_root_specific_sidecar) ||
               ncol(result$rhs_root_specific_sidecar) != 0L ||
               nrow(result$fixed_parameter_checks) != 0L) {
      stop("A non-RHS chain returned RHS diagnostic sidecars.",
           call. = FALSE)
    }
    checkpoint_rows[[length(checkpoint_rows) + 1L]] <<- data.frame(
      cell_id = cell,
      chain = chain,
      seed = row$seed[[1L]],
      completed_iterations =
        static$checkpoint_state$completed_iterations,
      checkpoint_digest = static$checkpoint_digest,
      continuation_history_digest =
        static$continuation_history_digest,
      continuation_generation =
        static$continuation_history_contract$generation,
      numerical_repair_count =
        static$model_spec$cumulative_numerical_repair_count,
      exact_joint_target =
        static$model_spec$exact_joint_target,
      promotion_eligible =
        static$model_spec$promotion_eligible,
      stringsAsFactors = FALSE
    )
    provenance_rows[[length(provenance_rows) + 1L]] <<- data.frame(
      cell_id = cell,
      chain = chain,
      family = row$family[[1L]],
      primary_source_commit =
        static$provenance$primary_source_commit,
      primary_runtime_tree_digest =
        static$provenance$primary_runtime_tree_digest,
      primary_runtime_source_match =
        static$provenance$primary_runtime_source_match,
      required_external_repositories = paste(
        static$provenance$required_external_repositories,
        collapse = "|"
      ),
      external_exdqlm_runtime_tree_digest = if (
        "exdqlm" %in%
          static$provenance$required_external_repositories
      ) {
        static$provenance$external_repositories$exdqlm$
          runtime_package_tree_digest
      } else {
        NA_character_
      },
      external_exdqlm_runtime_source_match = if (
        "exdqlm" %in%
          static$provenance$required_external_repositories
      ) {
        static$provenance$external_repositories$exdqlm$
          runtime_source_match
      } else {
        NA
      },
      reproducibility_eligible =
        static$provenance$reproducibility_eligible,
      outer_fit_schema = if (inherits(
        result$fit, "rqr_desn_fit"
      )) result$fit$schema_version else NA_character_,
      design_schema = if (inherits(
        result$fit, "rqr_desn_fit"
      )) result$fit$design$schema_version else NA_character_,
      materialization_receipt_schema = if (inherits(
        result$fit, "rqr_desn_fit"
      )) {
        result$fit$design$builder$materialization_receipt$
          schema_version
      } else {
        NA_character_
      },
      materialization_receipt_valid = if (inherits(
        result$fit, "rqr_desn_fit"
      )) {
        result$fit$model_spec$design_materialization_receipt_valid
      } else {
        NA
      },
      materialization_external_binding_verified = if (inherits(
        result$fit, "rqr_desn_fit"
      )) {
        result$fit$model_spec$
          design_materialization_external_binding_verified
      } else {
        NA
      },
      outer_reproducibility_eligible = if (inherits(
        result$fit, "rqr_desn_fit"
      )) result$fit$model_spec$reproducibility_eligible else NA,
      outer_promotion_eligible = if (inherits(
        result$fit, "rqr_desn_fit"
      )) result$fit$model_spec$promotion_eligible else NA,
      stringsAsFactors = FALSE
    )
    if (inherits(result$fit, "rqr_desn_fit")) {
      if (!is.data.frame(result$future_check) ||
          nrow(result$future_check) != 1L ||
          !identical(result$future_check$status[[1L]], "pass")) {
        stop(
          "A DESN chain lacks verified future-contract evidence.",
          call. = FALSE
        )
      }
      future_check_rows[[
        length(future_check_rows) + 1L
      ]] <<- cbind(
        data.frame(
          cell_id = cell,
          chain = chain,
          seed = row$seed[[1L]],
          design_digest = result$fit$design$semantic_digest,
          stringsAsFactors = FALSE
        ),
        result$future_check
      )
    } else if (!is.null(result$future_check)) {
      stop(
        "A fixed-design chain returned unexpected DESN future evidence.",
        call. = FALSE
      )
    }
    swaps <- static$diagnostics$root_swap_trace %||% logical(0)
    root_swap_rows[[length(root_swap_rows) + 1L]] <<- data.frame(
      cell_id = cell,
      chain = chain,
      transitions = length(swaps),
      swaps = sum(as.logical(swaps)),
      swap_fraction = if (length(swaps)) {
        mean(as.logical(swaps))
      } else {
        NA_real_
      },
      role = "sidecar_only",
      stringsAsFactors = FALSE
    )
    plan_index <- which(
      plan_status$cell_id == cell &
        plan_status$chain == chain
    )
    plan_status$status[plan_index] <<- "fit_complete_pending_cell_gate"
    plan_status$fit_sha256[plan_index] <<-
      chain_hashes[[length(chain_hashes)]]$sha256
    rqr_ordinary_v1_atomic_csv(
      plan_status,
      file.path(output_dir, "fit_plan_status_in_progress.csv")
    )
    artifact_bytes <- sum(file.info(list.files(
      run_dir, recursive = TRUE, full.names = TRUE
    ))$size, na.rm = TRUE)
    if (artifact_bytes > config$resources$maximum_artifact_bytes) {
      stop("The ignored run directory exceeded its frozen byte ceiling.",
           call. = FALSE)
    }
    if (as.numeric(difftime(Sys.time(), started, units = "mins")) >
        config$resources$hard_timeout_minutes) {
      stop("The bounded run exceeded its 45-minute wall-time ceiling.",
           call. = FALSE)
    }
  }
  publish_cell <- function(rows, diagnosis) {
    block <- diagnosis$diagnostics
    block$cell_id <- rows$cell_id[[1L]]
    block$family <- rows$family[[1L]]
    block$fixture_id <- rows$fixture_id[[1L]]
    block$prior_id <- rows$prior_id[[1L]]
    block$learning_rate_mode <- rows$learning_rate_mode[[1L]]
    diagnostics[[length(diagnostics) + 1L]] <<- block
    rqr_ordinary_v1_atomic_csv(
      do.call(rbind, diagnostics),
      file.path(output_dir, "bounded_diagnostics_in_progress.csv")
    )
    indices <- plan_status$cell_id == rows$cell_id[[1L]]
    plan_status$status[indices] <<- if (
      isTRUE(diagnosis$pass)
    ) "pass" else "fail"
    rqr_ordinary_v1_atomic_csv(
      plan_status,
      file.path(output_dir, "fit_plan_status_in_progress.csv")
    )
  }
  completed <- rqr_ordinary_v1_run_cells(
    fit_plan,
    fit_chain = function(row) {
      rqr_ordinary_v1_fit_chain(
        row, config, fixtures, provenance_controls, execution_design
      )
    },
    diagnose_cell = function(chains, rows) {
      cell <- rqr_ordinary_v1_cell_fixture(
        config, fixtures, rows[1L, , drop = FALSE],
        execution_design
      )
      rhs_active_names <- if (startsWith(
        rows$prior_id[[1L]], "rhs_ns"
      )) {
        setdiff(
          colnames(cell$X), cell$prior$hypers$intercept_name
        )
      } else {
        character(0)
      }
      expected_schema <- rqr_ordinary_v1_expected_estimand_columns(
        family = rows$family[[1L]],
        learning_rate_mode = rows$learning_rate_mode[[1L]],
        prior_id = rows$prior_id[[1L]],
        n_training = nrow(cell$X),
        coefficient_names = colnames(cell$X),
        horizon = if (identical(
          rows$family[[1L]], "desn"
        )) {
          config$fixtures$D02$future_extension$horizon
        } else {
          0L
        },
        rhs_active_names = rhs_active_names
      )
      rqr_ordinary_v1_diagnose_cell(
        chains, config$gates, expected_schema = expected_schema
      )
    },
    publish_chain = publish_chain,
    publish_cell = publish_cell
  )
  future_checks <- do.call(rbind, future_check_rows)
  expected_chains <- nrow(fit_plan)
  expected_desn_chains <- sum(fit_plan$family == "desn")
  expected_rhs_chains <- sum(startsWith(fit_plan$prior_id, "rhs_ns"))
  expected_fixed_rhs_chains <- sum(
    fit_plan$prior_id == "rhs_ns_fixed"
  )
  if (!identical(completed, unique(fit_plan$cell_id)) ||
      length(chain_hashes) != expected_chains ||
      length(checkpoint_rows) != expected_chains ||
      length(provenance_rows) != expected_chains ||
      length(root_swap_rows) != expected_chains ||
      length(rhs_root_sidecar_rows) != expected_rhs_chains ||
      length(fixed_parameter_rows) != expected_fixed_rhs_chains ||
      any(plan_status$status != "pass") ||
      !is.data.frame(future_checks) ||
      nrow(future_checks) != expected_desn_chains ||
      !identical(
        sort(unique(future_checks$cell_id)),
        sort(unique(
          fit_plan$cell_id[
            fit_plan$family == "desn"
          ]
        ))
      ) ||
      any(future_checks$status != "pass") ||
      any(!future_checks$future_contract_verified) ||
      any(future_checks$legacy_future_matrix) ||
      any(
        !future_checks$
          parent_design_materialization_external_binding_verified
      ) ||
      any(!future_checks$parent_fit_reproducibility_eligible) ||
      any(!future_checks$parent_fit_promotion_eligible) ||
      any(future_checks$future_external_provenance_bound) ||
      any(future_checks$future_reproducibility_eligible) ||
      any(future_checks$future_promotion_eligible) ||
      any(future_checks$response_predictive_draws)) {
    stop(
      sprintf(
        "The completed %s plan failed its exact artifact-set contract.",
        plan_kind
      ),
      call. = FALSE
    )
  }
  list(
    plan_kind = plan_kind,
    completed_cells = completed,
    diagnostics = do.call(rbind, diagnostics),
    chain_hashes = do.call(rbind, chain_hashes),
    posterior_summaries = do.call(rbind, posterior_summaries),
    checkpoint_manifest = do.call(rbind, checkpoint_rows),
    provenance_checks = do.call(rbind, provenance_rows),
    root_swap_sidecar = do.call(rbind, root_swap_rows),
    rhs_root_trace_sidecar =
      do.call(rbind, rhs_root_sidecar_rows),
    fixed_parameter_checks =
      do.call(rbind, fixed_parameter_rows),
    future_checks = future_checks,
    fit_plan_status = plan_status,
    initialization_manifest = initialization_manifest
  )
}

rqr_ordinary_v1_write_execution_result <- function(output_dir, result) {
  if (!is.list(result) ||
      !is.character(result$plan_kind) ||
      length(result$plan_kind) != 1L ||
      is.na(result$plan_kind) ||
      !result$plan_kind %in% c("benchmark-one-cell", "execute-bounded")) {
    stop("The execution result lacks a valid frozen-plan identity.",
         call. = FALSE)
  }
  artifacts <- list(
    bounded_diagnostics = result$diagnostics,
    local_chain_hashes = result$chain_hashes,
    fit_plan_status = result$fit_plan_status,
    compact_posterior_summaries = result$posterior_summaries,
    checkpoint_manifest = result$checkpoint_manifest,
    provenance_checks = result$provenance_checks,
    root_swap_sidecar = result$root_swap_sidecar,
    rhs_root_trace_sidecar = result$rhs_root_trace_sidecar,
    fixed_parameter_checks = result$fixed_parameter_checks,
    desn_future_checks = result$future_checks
  )
  for (name in names(artifacts)) {
    rqr_ordinary_v1_atomic_csv(
      artifacts[[name]], file.path(output_dir, paste0(name, ".csv"))
    )
  }
  progress <- file.path(
    output_dir,
    c(
      "fit_plan_status_in_progress.csv",
      "bounded_diagnostics_in_progress.csv"
    )
  )
  unlink(progress[file.exists(progress)], force = TRUE)
  if (any(file.exists(progress))) {
    stop("Final execution evidence retained an in-progress artifact.",
         call. = FALSE)
  }
  invisible(TRUE)
}

rqr_ordinary_v1_main <- function(arguments = commandArgs(trailingOnly = TRUE)) {
  mode <- if (length(arguments)) arguments[[1L]] else "preflight"
  repo_root <- rqr_ordinary_v1_find_repo()
  config <- rqr_ordinary_v1_load_config(repo_root)
  if (!mode %in% config$runner_modes || length(arguments) > 1L) {
    stop(
      paste(
        "Usage: 25_validate_rqr_ordinary_v1.R",
        "{preflight|reference-only|benchmark-one-cell|execute-bounded}"
      ),
      call. = FALSE
    )
  }
  source_state <- rqr_ordinary_v1_source_preflight(repo_root, config)
  expected_commit <- source_state$commit[[1L]]
  runtime <- rqr_ordinary_v1_runtime_preflight(repo_root, expected_commit)
  external_runtime <- if (identical(mode, "preflight")) {
    NULL
  } else {
    rqr_ordinary_v1_external_runtime_preflight(repo_root, config)
  }
  runtime_table <- if (is.null(external_runtime)) {
    runtime$table
  } else {
    rbind(runtime$table, external_runtime$table)
  }
  external_before <- rqr_ordinary_v1_external_guard(config)
  on.exit(
    rqr_ordinary_v1_verify_external_guard(config, external_before),
    add = TRUE
  )
  monitor <- rqr_ordinary_v1_monitor_preflight(
    config,
    required = mode %in% c("benchmark-one-cell", "execute-bounded")
  )
  fixtures <- rqr_ordinary_v1_build_fixtures(config)
  fit_count <- 0L

  output_dir <- Sys.getenv("RQR_ORDINARY_V1_OUTPUT_DIR", unset = "")
  if (!nzchar(output_dir)) {
    output_dir <- file.path(
      repo_root, "application", "outputs",
      paste0(
        "ordinary_v1_", gsub("-", "_", mode), "_",
        format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"), "_",
        substr(expected_commit, 1L, 12L)
      )
    )
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)
  run_dir <- Sys.getenv(
    "RQR_ORDINARY_V1_RUN_DIR",
    unset = file.path(repo_root, "application", "runs", basename(output_dir))
  )
  tryCatch({
    rqr_ordinary_v1_write_common_evidence(
      output_dir, config, source_state, runtime_table, fixtures, monitor
    )
    if (identical(mode, "preflight")) {
      gates <- data.frame(
        gate_id = c(
          "source_state", "isolated_runtime", "configuration",
          "canonical_fixtures", "protected_exdqlm_guard",
          "execution_disabled"
        ),
        status = "pass",
        detail = c(
          expected_commit,
          runtime$table$runtime_tree_digest[[1L]],
          config$config_id,
          as.character(length(fixtures)),
          external_before$commit,
          as.character(!config$ordinary_v1_execute_enabled)
        ),
        stringsAsFactors = FALSE
      )
      rqr_ordinary_v1_atomic_csv(
        gates, file.path(output_dir, "reference_gates.csv")
      )
      claim <- paste(
        "Preflight only: source, isolated runtime, configuration, fixtures,",
        "and protected-reference guard passed. No MCMC or simulation ran."
      )
    } else if (identical(mode, "reference-only")) {
      references <- rqr_ordinary_v1_reference_oracles(
        config, fixtures, repo_root
      )
      package_checks <- rqr_ordinary_v1_run_reference_tests(repo_root)
      design_path <- Sys.getenv(
        "RQR_ORDINARY_V1_ATTESTED_DESN_DESIGN_RDS", unset = ""
      )
      attested_design <- rqr_ordinary_v1_validate_attested_desn_design(
        design_path, config, external_runtime
      )
      attested_references <- rqr_ordinary_v1_attested_desn_references(
        config,
        attested_design,
        provenance_control = list(
          repo_root = repo_root,
          expected_git_commit = expected_commit,
          primary_runtime_attestation = runtime$attestation,
          external_repositories = list(
            exdqlm = external_runtime$spec
          ),
          required_external_repositories = "exdqlm"
        )
      )
      reference_evidence <- rqr_ordinary_v1_reference_evidence_tables(
        package_checks,
        references$comparisons,
        attested_references
      )
      design_gate <- data.frame(
        gate_id = "pinned_attested_desn_materialization",
        status = "pass",
        detail = paste(
          attested_design$receipt$source_commit,
          attested_design$file_sha256, sep = "|"
        ),
        stringsAsFactors = FALSE
      )
      end_to_end_gate <- data.frame(
        gate_id = "attested_desn_end_to_end_reference_cells",
        status = if (
          all(attested_references$status == "pass")
        ) "pass" else "fail",
        detail = paste(
          nrow(attested_references),
          "prior/rate cells fit, continued, and forecast"
        ),
        stringsAsFactors = FALSE
      )
      test_gate <- data.frame(
        gate_id = "ordinary_v1_native_reference_tests",
        status = if (all(package_checks$status == "pass")) "pass" else "fail",
        detail = paste(
          sum(package_checks$expectations), "expectations across",
          nrow(package_checks), "source-bound test files"
        ),
        stringsAsFactors = FALSE
      )
      references$gates <- rbind(
        references$gates, test_gate, design_gate,
        end_to_end_gate
      )
      references$pass <- isTRUE(references$pass) &&
        all(package_checks$status == "pass") &&
        all(attested_references$status == "pass")
      if (!isTRUE(references$pass)) {
        stop("At least one reference-only gate failed.",
             call. = FALSE)
      }
      rqr_ordinary_v1_atomic_csv(
        references$gates, file.path(output_dir, "reference_gates.csv")
      )
      rqr_ordinary_v1_atomic_csv(
        references$comparisons,
        file.path(output_dir, "oracle_comparisons.csv")
      )
      rqr_ordinary_v1_atomic_csv(
        references$protected_dlm_hashes,
        file.path(output_dir, "protected_dlm_hashes.csv")
      )
      rqr_ordinary_v1_atomic_csv(
        package_checks, file.path(output_dir, "package_checks.csv")
      )
      rqr_ordinary_v1_atomic_csv(
        attested_references,
        file.path(output_dir, "desn_future_checks.csv")
      )
      for (name in names(reference_evidence)) {
        rqr_ordinary_v1_atomic_csv(
          reference_evidence[[name]],
          file.path(output_dir, paste0(name, ".csv"))
        )
      }
      rqr_ordinary_v1_atomic_csv(
        data.frame(
          design_digest = attested_design$digest,
          file_sha256 = attested_design$file_sha256,
          source_commit = attested_design$receipt$source_commit,
          runtime_tree_digest =
            attested_design$receipt$runtime_tree_digest,
          runtime_attestation_sha256 =
            attested_design$receipt$runtime_attestation_sha256,
          runtime_source_match =
            attested_design$receipt$runtime_source_match,
          design_schema = attested_design$design$schema_version,
          receipt_schema =
            attested_design$receipt$schema_version,
          receipt_digest =
            attested_design$receipt_status$receipt_digest,
          materializer_arguments_digest =
            attested_design$receipt$materializer_arguments_digest,
          materialized_design_payload_digest =
            attested_design$receipt$
              materialized_design_payload_digest,
          receipt_valid =
            attested_design$receipt_status$receipt_valid,
          external_state_match =
            attested_design$verification$external_state_match,
          runtime_attestation_sha256_verified =
            attested_design$verification$
              runtime_attestation_sha256_verified,
          materialization_reproducibility_eligible =
            attested_design$verification$
              materialization_reproducibility_eligible,
          materialization_status =
            attested_design$verification$status,
          fit_schema = config$desn_schema_contract[["fit"]],
          future_design_schema =
            config$desn_schema_contract[["future_design"]],
          stringsAsFactors = FALSE
        ),
        file.path(output_dir, "desn_design_checks.csv")
      )
      claim <- paste(
        "Reference-only: deterministic ordinary-RQR and protected-DLM",
        "candidate-integrity checks passed. The separately documented",
        "baseline reconciliation and DLM regression remain required.",
        "No bounded grid or matched simulation ran."
      )
    } else if (identical(mode, "benchmark-one-cell")) {
      if (!identical(
            Sys.getenv(
              "RQR_ORDINARY_V1_BENCHMARK_CONFIRM", unset = ""
            ),
            "I_CONFIRM_ORDINARY_V1_ONE_CELL_BENCHMARK"
          ) ||
          isTRUE(config$ordinary_v1_execute_enabled)) {
        stop(
          paste(
            "The one-cell benchmark requires its exact confirmation and",
            "the source-candidate execution flag must remain disabled."
          ),
          call. = FALSE
        )
      }
      design_path <- Sys.getenv(
        "RQR_ORDINARY_V1_ATTESTED_DESN_DESIGN_RDS", unset = ""
      )
      attested_design <- rqr_ordinary_v1_validate_attested_desn_design(
        design_path, config, external_runtime
      )
      result <- rqr_ordinary_v1_execute(
        config, fixtures, output_dir, run_dir, runtime, source_state,
        external_runtime, attested_design,
        fit_plan = config$benchmark_plan
      )
      if (!identical(result$plan_kind, "benchmark-one-cell") ||
          !identical(nrow(result$fit_plan_status), 4L)) {
        stop("The benchmark branch did not execute exactly four frozen fits.",
             call. = FALSE)
      }
      rqr_ordinary_v1_write_execution_result(output_dir, result)
      fit_count <- nrow(config$benchmark_plan)
      claim <- paste(
        "The representative four-chain DESN RHS-NS learned-rate benchmark",
        "passed its mechanics, diagnostics, and resource-wrapper contracts.",
        "It is not part of the 48-fit validation grid or a matched simulation."
      )
    } else {
      reference_dir <- Sys.getenv(
        "RQR_ORDINARY_V1_REFERENCE_DIR", unset = ""
      )
      benchmark_dir <- Sys.getenv(
        "RQR_ORDINARY_V1_BENCHMARK_DIR", unset = ""
      )
      benchmark_monitor_dir <- Sys.getenv(
        "RQR_ORDINARY_V1_BENCHMARK_MONITOR_DIR", unset = ""
      )
      design_path <- Sys.getenv(
        "RQR_ORDINARY_V1_ATTESTED_DESN_DESIGN_RDS", unset = ""
      )
      attested_design <- rqr_ordinary_v1_validate_attested_desn_design(
        design_path, config, external_runtime
      )
      rqr_ordinary_v1_authorize_execute(
        config, source_state, runtime_table, reference_dir,
        benchmark_dir, benchmark_monitor_dir, attested_design
      )
      result <- rqr_ordinary_v1_execute(
        config, fixtures, output_dir, run_dir, runtime, source_state,
        external_runtime, attested_design
      )
      if (!identical(result$plan_kind, "execute-bounded") ||
          !identical(nrow(result$fit_plan_status), 48L)) {
        stop("The bounded branch did not execute exactly 48 frozen fits.",
             call. = FALSE)
      }
      rqr_ordinary_v1_write_execution_result(output_dir, result)
      fit_count <- nrow(config$fit_plan)
      claim <- paste(
        "The prospective 48-fit mechanics/mixing grid passed. This is not",
        "a response model, calibration study, or matched simulation."
      )
    }
    rqr_ordinary_v1_atomic_csv(
      rqr_ordinary_v1_empty_failure(),
      file.path(output_dir, "failure_log.csv")
    )
    rqr_ordinary_v1_atomic_csv(
      data.frame(
        mode = mode, status = "pass", source_commit = expected_commit,
        fits_executed = fit_count,
        benchmark_fits_executed = if (
          identical(mode, "benchmark-one-cell")
        ) fit_count else 0L,
        bounded_fits_executed = if (
          identical(mode, "execute-bounded")
        ) fit_count else 0L,
        matched_simulation_authorized = FALSE,
        stringsAsFactors = FALSE
      ),
      file.path(output_dir, "run_status.csv")
    )
    rqr_ordinary_v1_atomic_lines(
      c(capture.output(sessionInfo()), "", claim),
      file.path(output_dir, "session_info.txt")
    )
    rqr_ordinary_v1_atomic_lines(
      c(
        "# Ordinary RQR version-1 validation closeout", "",
        claim, "",
        "The update is loss-based generalized Bayes. It does not define",
        "posterior-predictive response draws. Nonzero tilt, VB/CAVI/ELBO,",
        "adaptive discounts, and matched simulations remain outside scope."
      ),
      file.path(output_dir, "closeout.md")
    )
    rqr_ordinary_v1_write_manifest(output_dir)
    message("ordinary-v1 ", mode, " PASS: ", output_dir)
    invisible(output_dir)
  }, error = function(error) {
    error_cell <- error$cell_id %||% NA_character_
    error_chain <- error$chain %||% NA_integer_
    failure <- data.frame(
      timestamp_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      mode = mode,
      cell_id = error_cell,
      chain = error_chain,
      error_class = class(error)[[1L]],
      message = conditionMessage(error),
      stringsAsFactors = FALSE
    )
    try(rqr_ordinary_v1_atomic_csv(
      failure, file.path(output_dir, "failure_log.csv")
    ), silent = TRUE)
    try(rqr_ordinary_v1_atomic_csv(
      data.frame(
        mode = mode, status = "fail", source_commit = expected_commit,
        fits_executed = NA_integer_,
        benchmark_fits_executed = NA_integer_,
        bounded_fits_executed = NA_integer_,
        matched_simulation_authorized = FALSE,
        stringsAsFactors = FALSE
      ),
      file.path(output_dir, "run_status.csv")
    ), silent = TRUE)
    try(rqr_ordinary_v1_write_manifest(output_dir), silent = TRUE)
    stop(error)
  })
}

if (!identical(Sys.getenv("RQR_ORDINARY_V1_SOURCE_ONLY", unset = ""), "YES")) {
  rqr_ordinary_v1_main()
}
