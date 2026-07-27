ordinary_v1_runner_environment <- function() {
  path <- testthat::test_path(
    "..", "..", "scripts", "25_validate_rqr_ordinary_v1.R"
  )
  old <- Sys.getenv("RQR_ORDINARY_V1_SOURCE_ONLY", unset = NA_character_)
  on.exit({
    if (is.na(old)) {
      Sys.unsetenv("RQR_ORDINARY_V1_SOURCE_ONLY")
    } else {
      Sys.setenv(RQR_ORDINARY_V1_SOURCE_ONLY = old)
    }
  }, add = TRUE)
  Sys.setenv(RQR_ORDINARY_V1_SOURCE_ONLY = "YES")
  environment <- new.env(parent = globalenv())
  sys.source(path, envir = environment)
  environment
}

ordinary_v1_config <- function(environment) {
  repo_root <- normalizePath(
    testthat::test_path("..", "..", ".."),
    winslash = "/", mustWork = TRUE
  )
  environment$rqr_ordinary_v1_load_config(repo_root)
}

ordinary_v1_authorization_test_git <- function(repo_root, arguments) {
  git <- Sys.which("git")
  if (!nzchar(git)) {
    stop("git is required for authorization-contract tests.", call. = FALSE)
  }
  output <- suppressWarnings(system2(
    git,
    c("-C", shQuote(repo_root), vapply(arguments, shQuote, character(1L))),
    stdout = TRUE,
    stderr = TRUE,
    env = c("GIT_OPTIONAL_LOCKS=0", "GIT_TERMINAL_PROMPT=0")
  ))
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  if (!identical(as.integer(status), 0L)) {
    stop(
      paste(
        "Temporary Git command failed:",
        paste(arguments, collapse = " "),
        paste(output, collapse = "\n"),
        sep = "\n"
      ),
      call. = FALSE
    )
  }
  trimws(paste(output, collapse = "\n"))
}

ordinary_v1_authorization_write_config <- function(repo_root, config) {
  path <- file.path(
    repo_root, "application", "config", "rqr_ordinary_v1",
    "rqr_ordinary_v1_bounded_validation_20260726.R"
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(
    c(
      "rqr_ordinary_v1_bounded_validation <-",
      capture.output(dput(config))
    ),
    path,
    useBytes = TRUE
  )
  invisible(path)
}

ordinary_v1_authorization_commit <- function(repo_root, message) {
  ordinary_v1_authorization_test_git(repo_root, c("add", "--all"))
  ordinary_v1_authorization_test_git(
    repo_root, c("commit", "--quiet", "-m", message)
  )
  tolower(ordinary_v1_authorization_test_git(
    repo_root, c("rev-parse", "HEAD")
  ))
}

ordinary_v1_authorization_repository <- function(
    repo_root, base_config = list(
      ordinary_v1_execute_enabled = FALSE,
      reviewed_implementation_commit = NA_character_,
      immutable_contract_field = "frozen"
    )) {
  ordinary_v1_authorization_test_git(repo_root, c("init", "--quiet", "-b", "main"))
  ordinary_v1_authorization_test_git(
    repo_root, c("config", "user.name", "Ordinary V1 Test")
  )
  ordinary_v1_authorization_test_git(
    repo_root, c("config", "user.email", "ordinary-v1-test@example.invalid")
  )
  ordinary_v1_authorization_test_git(
    repo_root, c("config", "commit.gpgsign", "false")
  )
  ordinary_v1_authorization_write_config(repo_root, base_config)
  list(
    repo_root = repo_root,
    base_config = base_config,
    base_commit = ordinary_v1_authorization_commit(
      repo_root, "reviewed implementation"
    )
  )
}

ordinary_v1_benchmark_runtime_table <- function(
    primary_commit, pinned_exdqlm_commit,
    primary_attestation = paste(rep("a", 64L), collapse = "")) {
  data.frame(
    package = c("rqrgibbs", "exdqlm"),
    version = c("0.1.0.9022", "0.4.0"),
    source_commit = c(primary_commit, pinned_exdqlm_commit),
    runtime_path = c(
      "/isolated/rqrgibbs-reviewed",
      "/isolated/exdqlm-pinned"
    ),
    runtime_tree_digest = c(
      paste(rep("d", 64L), collapse = ""),
      paste(rep("e", 64L), collapse = "")
    ),
    runtime_source_match = TRUE,
    reproducibility_eligible = TRUE,
    runtime_attestation_schema = "runtime/4.0.0",
    attestation_sha256 = c(
      primary_attestation,
      paste(rep("b", 64L), collapse = "")
    ),
    R_version = "R benchmark-test",
    platform = "benchmark-test-platform",
    compiler = "benchmark-test-compiler",
    BLAS = "benchmark-test-blas",
    LAPACK = "benchmark-test-lapack",
    posterior_version = "1.7.0",
    stringsAsFactors = FALSE
  )
}

ordinary_v1_benchmark_refresh_wrapper_manifest <- function(
    runner, output_dir, monitor_dir) {
  monitor_names <- c(
    "process_group_monitor.csv", "process_group_resource_summary.csv",
    "runner.stderr.log", "runner.stdout.log", "wrapper_closeout.csv"
  )
  output_names <- c(
    "artifact_hashes.csv", "run_status.csv", "source_state.csv",
    "validation_config_digest.csv"
  )
  rows <- rbind(
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
  resolve <- function(role, path) {
    if (identical(role, "monitor_evidence")) {
      file.path(monitor_dir, sub("^monitor/", "", path))
    } else {
      file.path(output_dir, sub("^output/", "", path))
    }
  }
  paths <- vapply(seq_len(nrow(rows)), function(index) {
    resolve(rows$role[[index]], rows$path[[index]])
  }, character(1L))
  rows$bytes <- as.numeric(file.info(paths)$size)
  rows$sha256 <- unname(vapply(
    paths, runner$rqr_ordinary_v1_sha256_file, character(1L)
  ))
  utils::write.csv(
    rows, file.path(monitor_dir, "wrapper_artifact_hashes.csv"),
    row.names = FALSE
  )
  invisible(rows)
}

ordinary_v1_benchmark_refresh_integrity <- function(
    runner, output_dir, monitor_dir) {
  runner$rqr_ordinary_v1_atomic_csv(
    runner$rqr_ordinary_v1_artifact_manifest(output_dir),
    file.path(output_dir, "artifact_hashes.csv")
  )
  ordinary_v1_benchmark_refresh_wrapper_manifest(
    runner, output_dir, monitor_dir
  )
  invisible(TRUE)
}

ordinary_v1_benchmark_rewrite_csv <- function(
    runner, path, mutate) {
  value <- utils::read.csv(
    path, stringsAsFactors = FALSE, check.names = FALSE,
    na.strings = c("", "NA")
  )
  value <- mutate(value)
  runner$rqr_ordinary_v1_atomic_csv(value, path)
  invisible(value)
}

ordinary_v1_benchmark_bundle_fixture <- function(
    runner, base_config, local_envir = parent.frame()) {
  reviewed_commit <- paste(rep("a", 40L), collapse = "")
  authorization_commit <- paste(rep("b", 40L), collapse = "")
  config <- base_config
  config$ordinary_v1_execute_enabled <- TRUE
  config$reviewed_implementation_commit <- reviewed_commit
  output_dir <- withr::local_tempdir(.local_envir = local_envir)
  monitor_dir <- withr::local_tempdir(.local_envir = local_envir)
  plan <- config$benchmark_plan
  schema <- runner$rqr_ordinary_v1_schema()
  digest64 <- function(value) {
    paste0(substr(as.character(value), 1L, 1L),
           paste(rep("a", 63L), collapse = ""))
  }
  write_csv <- function(value, name) {
    runner$rqr_ordinary_v1_atomic_csv(
      value, file.path(output_dir, name)
    )
  }

  recorded_runtime <- ordinary_v1_benchmark_runtime_table(
    reviewed_commit, config$pinned_exdqlm$commit
  )
  current_runtime <- ordinary_v1_benchmark_runtime_table(
    authorization_commit, config$pinned_exdqlm$commit,
    primary_attestation = digest64(3)
  )
  source_state <- data.frame(
    authorization_status = "flag_only_authorization_verified",
    authorization_delta_verified = TRUE,
    commit = authorization_commit,
    stringsAsFactors = FALSE
  )
  design_digest <- digest64("a")
  design_X <- cbind(
    intercept = 1,
    h_last_001 = seq(-1, 1, length.out = 48L),
    h_last_002 = cos(seq_len(48L) / 5),
    reduced_h_001 = sin(seq_len(48L) / 4),
    reduced_h_002 = cos(seq_len(48L) / 7),
    reduced_h_003 = sin(seq_len(48L) / 9)
  )
  attested_design <- list(
    design = list(semantic_digest = design_digest, X = design_X)
  )
  expected_config_digest <-
    runner$rqr_ordinary_v1_disabled_config_digest(config)

  write_csv(
    data.frame(
      mode = "benchmark-one-cell",
      status = "pass",
      source_commit = reviewed_commit,
      fits_executed = 4L,
      benchmark_fits_executed = 4L,
      bounded_fits_executed = 0L,
      matched_simulation_authorized = FALSE,
      stringsAsFactors = FALSE
    ),
    "run_status.csv"
  )
  write_csv(
    data.frame(
      repository = "RQR-GIBBS",
      branch = "main",
      commit = reviewed_commit,
      clean = TRUE,
      config_digest = expected_config_digest,
      authorization_status = "source_candidate_execution_disabled",
      reviewed_implementation_commit = reviewed_commit,
      authorization_delta_verified = FALSE,
      rng_kind = "L'Ecuyer-CMRG",
      stringsAsFactors = FALSE
    ),
    "source_state.csv"
  )
  write_csv(
    data.frame(
      config_id = config$config_id,
      config_digest = expected_config_digest,
      config_path = "application/config/rqr_ordinary_v1/test.R",
      execute_enabled = FALSE,
      stringsAsFactors = FALSE
    ),
    "validation_config_digest.csv"
  )
  write_csv(recorded_runtime, "runtime_attestations.csv")
  write_csv(
    data.frame(
      monitor_active = TRUE,
      cleanup_traps = TRUE,
      final_pgid_sweep = TRUE,
      timeout_seconds =
        60 * as.numeric(config$resources$hard_timeout_minutes),
      maximum_processes = config$resources$maximum_processes,
      maximum_threads = config$resources$maximum_threads,
      maximum_artifact_bytes =
        config$resources$maximum_artifact_bytes,
      contract_pass = TRUE,
      stringsAsFactors = FALSE
    ),
    "resource_summary.csv"
  )
  write_csv(plan, "fit_plan.csv")

  fit_hashes <- vapply(1:4, digest64, character(1L))
  plan_status <- plan
  plan_status$status <- "pass"
  plan_status$fit_sha256 <- fit_hashes
  write_csv(plan_status, "fit_plan_status.csv")

  initialization <- transform(
    plan,
    profile = names(config$mcmc$initialization_profiles)[chain],
    n_features = 6L,
    midpoint_shift = vapply(
      config$mcmc$initialization_profiles,
      function(profile) profile$midpoint_shift,
      numeric(1L)
    ),
    initial_half_width = vapply(
      config$mcmc$initialization_profiles,
      function(profile) profile$half_width,
      numeric(1L)
    ),
    rhs_prior_state_initialized = TRUE,
    rhs_scale_multiplier = vapply(
      config$mcmc$initialization_profiles,
      function(profile) profile$rhs_scale_multiplier,
      numeric(1L)
    ),
    prior_state_draws_retained = TRUE,
    initial_state_digest = vapply(5:8, digest64, character(1L)),
    design_digest = design_digest
  )
  initialization <- initialization[c(
    "cell_id", "chain", "seed", "fixture_id", "prior_id",
    "learning_rate_mode", "profile", "n_features",
    "midpoint_shift", "initial_half_width",
    "rhs_prior_state_initialized", "rhs_scale_multiplier",
    "prior_state_draws_retained", "initial_state_digest",
    "design_digest"
  )]
  write_csv(initialization, "initialization_manifest.csv")

  active_names <- setdiff(colnames(design_X), "intercept")
  expected_estimands <-
    runner$rqr_ordinary_v1_expected_estimand_columns(
      family = "desn",
      learning_rate_mode = "learned_pseudoresidual_normalized",
      prior_id = "rhs_ns_fixed",
      n_training = nrow(design_X),
      coefficient_names = colnames(design_X),
      horizon = config$fixtures$D02$future_extension$horizon,
      rhs_active_names = active_names
    )
  write_csv(
    data.frame(
      estimand = expected_estimands,
      rhat = rep(1.001, length(expected_estimands)),
      ess_bulk = rep(2000, length(expected_estimands)),
      ess_tail = rep(1800, length(expected_estimands)),
      mcse_mean = rep(0.01, length(expected_estimands)),
      pass = TRUE,
      cell_id = "BENCH01",
      family = "desn",
      fixture_id = "D02",
      prior_id = "rhs_ns_fixed",
      learning_rate_mode = "learned_pseudoresidual_normalized",
      stringsAsFactors = FALSE
    ),
    "bounded_diagnostics.csv"
  )
  summaries <- do.call(rbind, lapply(1:4, function(chain) {
    data.frame(
      estimand = expected_estimands,
      mean = seq(-0.2, 0.2, length.out = length(expected_estimands)) +
        chain / 100,
      sd = 1,
      q05 = -1.5,
      q25 = -0.5,
      median = 0,
      q75 = 0.5,
      q95 = 1.5,
      cell_id = "BENCH01",
      chain = chain,
      seed = plan$seed[[chain]],
      stringsAsFactors = FALSE
    )
  }))
  write_csv(summaries, "compact_posterior_summaries.csv")
  expected_sidecar <-
    runner$rqr_ordinary_v1_expected_rhs_sidecar_columns(
      "rhs_ns_fixed", active_names
    )
  sidecar <- do.call(rbind, lapply(1:4, function(chain) {
    data.frame(
      estimand = expected_sidecar,
      mean = seq(-0.1, 0.1, length.out = length(expected_sidecar)) +
        chain / 100,
      sd = 1,
      q05 = -1.5,
      q25 = -0.5,
      median = 0,
      q75 = 0.5,
      q95 = 1.5,
      cell_id = "BENCH01",
      chain = chain,
      seed = plan$seed[[chain]],
      role = "root_specific_sidecar_only",
      stringsAsFactors = FALSE
    )
  }))
  write_csv(sidecar, "rhs_root_trace_sidecar.csv")
  fixed_parameters <- do.call(rbind, lapply(1:4, function(chain) {
    data.frame(
      parameter = rep("zeta2_fixed", 2L),
      root = c("root1", "root2"),
      expected_value = config$fixtures$F05$prior$zeta2_fixed,
      retained_draws = config$mcmc$retained_per_chain,
      exact_identity = TRUE,
      status = "pass",
      cell_id = "BENCH01",
      chain = chain,
      seed = plan$seed[[chain]],
      stringsAsFactors = FALSE
    )
  }))
  write_csv(fixed_parameters, "fixed_parameter_checks.csv")

  completed_iterations <-
    config$mcmc$burn_in +
      config$mcmc$retained_per_chain * config$mcmc$thin
  write_csv(
    data.frame(
      cell_id = "BENCH01",
      chain = 1:4,
      seed = plan$seed,
      completed_iterations = completed_iterations,
      checkpoint_digest = vapply(1:4, digest64, character(1L)),
      continuation_history_digest =
        vapply(5:8, digest64, character(1L)),
      continuation_generation = 0L,
      numerical_repair_count = 0L,
      exact_joint_target = TRUE,
      promotion_eligible = TRUE,
      stringsAsFactors = FALSE
    ),
    "checkpoint_manifest.csv"
  )
  write_csv(
    data.frame(
      cell_id = "BENCH01",
      chain = 1:4,
      family = "desn",
      primary_source_commit = reviewed_commit,
      primary_runtime_tree_digest =
        recorded_runtime$runtime_tree_digest[[1L]],
      primary_runtime_source_match = TRUE,
      required_external_repositories = "exdqlm",
      external_exdqlm_runtime_tree_digest =
        recorded_runtime$runtime_tree_digest[[2L]],
      external_exdqlm_runtime_source_match = TRUE,
      reproducibility_eligible = TRUE,
      outer_fit_schema = config$desn_schema_contract[["fit"]],
      design_schema = config$desn_schema_contract[["design"]],
      materialization_receipt_schema =
        config$desn_schema_contract[["materialization_receipt"]],
      materialization_receipt_valid = TRUE,
      materialization_external_binding_verified = TRUE,
      outer_reproducibility_eligible = TRUE,
      outer_promotion_eligible = TRUE,
      stringsAsFactors = FALSE
    ),
    "provenance_checks.csv"
  )
  write_csv(
    data.frame(
      cell_id = "BENCH01",
      chain = 1:4,
      transitions = completed_iterations,
      swaps = completed_iterations %/% 2L,
      swap_fraction = 0.5,
      role = "sidecar_only",
      stringsAsFactors = FALSE
    ),
    "root_swap_sidecar.csv"
  )
  write_csv(
    data.frame(
      cell_id = "BENCH01",
      chain = 1:4,
      seed = plan$seed,
      design_digest = design_digest,
      fit_schema = config$desn_schema_contract[["fit"]],
      future_design_schema =
        config$desn_schema_contract[["future_design"]],
      future_verification_schema =
        config$desn_schema_contract[["future_verification"]],
      future_contract_verified = TRUE,
      legacy_future_matrix = FALSE,
      parent_design_materialization_external_binding_verified = TRUE,
      parent_fit_reproducibility_eligible = TRUE,
      parent_fit_promotion_eligible = TRUE,
      future_external_provenance_bound = FALSE,
      future_reproducibility_eligible = FALSE,
      future_promotion_eligible = FALSE,
      future_promotion_status =
        "verified_future_contract_unattested_materialization",
      response_predictive_draws = FALSE,
      future_design_digest = digest64("b"),
      status = "pass",
      stringsAsFactors = FALSE
    ),
    "desn_future_checks.csv"
  )
  write_csv(
    data.frame(
      cell_id = "BENCH01",
      chain = 1:4,
      seed = plan$seed,
      relative_path = sprintf("BENCH01_chain_%02d.rds", 1:4),
      byte_count = 1000L + 1:4,
      sha256 = fit_hashes,
      stringsAsFactors = FALSE
    ),
    "local_chain_hashes.csv"
  )
  write_csv(
    runner$rqr_ordinary_v1_empty_failure(), "failure_log.csv"
  )
  write_csv(
    data.frame(
      fixture_id = "D02",
      class = "rqr_desn_design|list",
      digest = design_digest,
      stringsAsFactors = FALSE
    ),
    "fixture_manifest.csv"
  )
  write_csv(config$seed_ledger, "seed_ledger.csv")
  runner$rqr_ordinary_v1_atomic_lines(
    "Synthetic benchmark closeout for validator-boundary testing.",
    file.path(output_dir, "closeout.md")
  )
  runner$rqr_ordinary_v1_atomic_lines(
    "Synthetic benchmark session information.",
    file.path(output_dir, "session_info.txt")
  )
  runner$rqr_ordinary_v1_atomic_csv(
    runner$rqr_ordinary_v1_artifact_manifest(output_dir),
    file.path(output_dir, "artifact_hashes.csv")
  )

  monitor_trace <- data.frame(
    elapsed_seconds = c(0, 1),
    processes = c(2, 2),
    threads = c(3, 3),
    rss_kib = c(900, 1000),
    output_run_bytes = c(4000, 5000),
    stringsAsFactors = FALSE
  )
  utils::write.csv(
    monitor_trace,
    file.path(monitor_dir, "process_group_monitor.csv"),
    row.names = FALSE
  )
  metrics <- c(
    sampled_peak_processes = "2",
    sampled_peak_threads = "3",
    sampled_peak_rss_kib = "1000",
    sampled_peak_output_run_bytes = "5000",
    hard_wall_timeout_triggered = "FALSE",
    process_limit_triggered = "FALSE",
    thread_limit_triggered = "FALSE",
    artifact_limit_triggered = "FALSE",
    monitor_error = "FALSE",
    final_pgid_empty = "TRUE",
    residual_group_cleanup = "FALSE",
    kill_escalation_used = "FALSE",
    runner_exit_status = "0",
    wrapper_incoming_status = "0"
  )
  limits <- rep("", length(metrics))
  names(limits) <- names(metrics)
  limits[["sampled_peak_processes"]] <-
    as.character(config$resources$maximum_processes)
  limits[["sampled_peak_threads"]] <-
    as.character(config$resources$maximum_threads)
  limits[["sampled_peak_output_run_bytes"]] <-
    as.character(config$resources$maximum_artifact_bytes)
  utils::write.csv(
    data.frame(
      metric = names(metrics),
      value = unname(metrics),
      limit = unname(limits),
      pass = TRUE,
      enforcement = "sampled_fail_closed",
      stringsAsFactors = FALSE
    ),
    file.path(monitor_dir, "process_group_resource_summary.csv"),
    row.names = FALSE, na = ""
  )
  wrapper_fields <- c(
    schema_version = "rqrgibbs_ordinary_v1_wrapper/1.0.0",
    mode = "benchmark-one-cell",
    expected_primary_commit = reviewed_commit,
    process_group_id = "12345",
    runner_exit_status = "0",
    resource_pass = "TRUE",
    monitor_kind = "pgid_sampled_fail_closed",
    sampled_resource_maxima_are_kernel_hard = "FALSE",
    signal_received = "NONE",
    final_pgid_empty = "TRUE",
    residual_group_cleanup = "FALSE",
    completed_at = "2026-07-26T00:00:00Z"
  )
  utils::write.csv(
    data.frame(
      field = names(wrapper_fields),
      value = unname(wrapper_fields),
      stringsAsFactors = FALSE
    ),
    file.path(monitor_dir, "wrapper_closeout.csv"),
    row.names = FALSE
  )
  writeLines(
    "synthetic stdout", file.path(monitor_dir, "runner.stdout.log"),
    useBytes = TRUE
  )
  writeLines(
    character(0), file.path(monitor_dir, "runner.stderr.log"),
    useBytes = TRUE
  )
  ordinary_v1_benchmark_refresh_wrapper_manifest(
    runner, output_dir, monitor_dir
  )

  list(
    config = config,
    source_state = source_state,
    runtime_table = current_runtime,
    output_dir = output_dir,
    monitor_dir = monitor_dir,
    attested_design = attested_design,
    schema = schema
  )
}

ordinary_v1_reference_tables_fixture <- function(runner, base_config) {
  hash <- function(character) {
    paste(rep(character, 64L), collapse = "")
  }
  reviewed <- paste(rep("a", 40L), collapse = "")
  config <- base_config
  config$ordinary_v1_execute_enabled <- TRUE
  config$reviewed_implementation_commit <- reviewed
  schema <- runner$rqr_ordinary_v1_schema()
  stamp <- function(data) {
    data <- as.data.frame(data, stringsAsFactors = FALSE)
    cbind(
      schema_version = rep(schema, nrow(data)),
      data,
      stringsAsFactors = FALSE
    )
  }
  config_digest <- runner$rqr_ordinary_v1_disabled_config_digest(config)
  runtime <- ordinary_v1_benchmark_runtime_table(
    reviewed, config$pinned_exdqlm$commit
  )
  runtime <- stamp(runtime)
  external <- runtime[runtime$package == "exdqlm", , drop = FALSE]
  receipt <- list(
    materializer_arguments_digest = hash("a"),
    materialized_design_payload_digest = hash("b")
  )
  receipt_status <- list(receipt_digest = hash("f"))
  attested_design <- list(
    design = list(semantic_digest = hash("d")),
    digest = hash("c"),
    file_sha256 = hash("b"),
    receipt = receipt,
    receipt_status = receipt_status
  )
  expected_fixture_manifest <- data.frame(
    fixture_id = c("F01", "F02", "F03", "F04", "F05", "D01", "D03"),
    class = c(rep("list", 5L), "rqr_desn_design|list", "list"),
    digest = vapply(c("a", "b", "c", "d", "e", "f", "a"),
                    hash, character(1L)),
    stringsAsFactors = FALSE
  )
  package_files <- runner$rqr_ordinary_v1_reference_test_names()
  package_checks <- data.frame(
    test_file = package_files,
    test_blocks = seq_along(package_files),
    expectations = 100L + seq_along(package_files),
    failures = 0L,
    errors = 0L,
    warnings = 0L,
    skipped = 0L,
    elapsed_seconds = seq_along(package_files) / 100,
    status = "pass",
    detail = "all expectations passed",
    stringsAsFactors = FALSE
  )
  expectation <- setNames(
    package_checks$expectations, package_checks$test_file
  )
  references <- config$seed_ledger[
    config$seed_ledger$purpose == "attested_desn_end_to_end",
    , drop = FALSE
  ]
  future <- data.frame(
    reference_id = references$seed_id,
    prior_id = references$prior_id,
    learning_rate_mode = references$learning_rate_mode,
    seed = references$seed,
    design_digest = hash("d"),
    completed_iterations = 3L,
    continuation_generation = 1L,
    promotion_eligible = TRUE,
    fit_schema = config$desn_schema_contract[["fit"]],
    future_design_schema =
      config$desn_schema_contract[["future_design"]],
    future_verification_schema =
      config$desn_schema_contract[["future_verification"]],
    future_contract_verified = TRUE,
    legacy_future_matrix = FALSE,
    parent_design_materialization_external_binding_verified = TRUE,
    parent_fit_reproducibility_eligible = TRUE,
    parent_fit_promotion_eligible = TRUE,
    future_external_provenance_bound = FALSE,
    future_reproducibility_eligible = FALSE,
    future_promotion_eligible = FALSE,
    future_promotion_status =
      "verified_future_contract_unattested_materialization",
    response_predictive_draws = FALSE,
    future_design_digest = hash("e"),
    status = "pass",
    stringsAsFactors = FALSE
  )
  category <- function(
      check_id, evidence_source, expectations) {
    stamp(data.frame(
      check_id = check_id,
      evidence_source = evidence_source,
      expectations = expectations,
      status = "pass",
      detail = paste("Synthetic evidence for", check_id),
      stringsAsFactors = FALSE
    ))
  }
  oracle_shape <- config$lambda_prior$shape +
    sum(!is.na(config$fixtures$F02$y))
  oracle_expected <- c(
    "0", as.character(oracle_shape), "TRUE", "3|9", "3"
  )
  tables <- list(
    run_status = stamp(data.frame(
      mode = "reference-only",
      status = "pass",
      source_commit = reviewed,
      fits_executed = 0L,
      benchmark_fits_executed = 0L,
      bounded_fits_executed = 0L,
      matched_simulation_authorized = FALSE,
      stringsAsFactors = FALSE
    )),
    source_state = stamp(data.frame(
      repository = "RQR-GIBBS",
      branch = "main",
      commit = reviewed,
      clean = TRUE,
      config_digest = config_digest,
      authorization_status = "source_candidate_execution_disabled",
      reviewed_implementation_commit = NA_character_,
      authorization_delta_verified = FALSE,
      rng_kind = "Mersenne-Twister|Inversion|Rejection",
      stringsAsFactors = FALSE
    )),
    runtime_attestations = runtime,
    validation_config_digest = stamp(data.frame(
      config_id = config$config_id,
      config_digest = config_digest,
      config_path = attr(config, "path"),
      execute_enabled = FALSE,
      stringsAsFactors = FALSE
    )),
    reference_gates = stamp(data.frame(
      gate_id = c(
        "deterministic_oracles", "protected_dlm_candidate_sha256",
        "ordinary_v1_native_reference_tests",
        "pinned_attested_desn_materialization",
        "attested_desn_end_to_end_reference_cells"
      ),
      status = "pass",
      detail = paste("Synthetic gate", seq_len(5L)),
      stringsAsFactors = FALSE
    )),
    oracle_comparisons = stamp(data.frame(
      oracle_id = c(
        "loss_sign_partition", "normalized_lambda_shape",
        "normalized_lambda_rate_finite", "desn_training_missing_rows",
        "desn_future_contracts"
      ),
      expected = oracle_expected,
      actual = oracle_expected,
      tolerance = c(
        "0", "exact", "finite_positive", "exact", "exact"
      ),
      pass = TRUE,
      stringsAsFactors = FALSE
    )),
    protected_dlm_hashes = stamp(data.frame(
      relative_path = names(config$protected_dlm_sha256),
      expected_sha256 = unname(config$protected_dlm_sha256),
      actual_sha256 = unname(config$protected_dlm_sha256),
      pass = TRUE,
      stringsAsFactors = FALSE
    )),
    package_checks = stamp(package_checks),
    desn_design_checks = stamp(data.frame(
      design_digest = attested_design$digest,
      file_sha256 = attested_design$file_sha256,
      source_commit = config$pinned_exdqlm$commit,
      runtime_tree_digest = external$runtime_tree_digest[[1L]],
      runtime_attestation_sha256 = external$attestation_sha256[[1L]],
      runtime_source_match = TRUE,
      design_schema = config$desn_schema_contract[["design"]],
      receipt_schema =
        config$desn_schema_contract[["materialization_receipt"]],
      receipt_digest = receipt_status$receipt_digest,
      materializer_arguments_digest =
        receipt$materializer_arguments_digest,
      materialized_design_payload_digest =
        receipt$materialized_design_payload_digest,
      receipt_valid = TRUE,
      external_state_match = TRUE,
      runtime_attestation_sha256_verified = TRUE,
      materialization_reproducibility_eligible = TRUE,
      materialization_status =
        "verified_current_isolated_materialization",
      fit_schema = config$desn_schema_contract[["fit"]],
      future_design_schema =
        config$desn_schema_contract[["future_design"]],
      stringsAsFactors = FALSE
    )),
    desn_future_checks = stamp(future),
    missingness_checks = category(
      c(
        "fixed_design_observed_mask_and_rng_contract",
        "desn_training_missing_indices"
      ),
      c(
        "test-rqr-native-fixed-design-v1.R",
        "oracle_comparisons.csv"
      ),
      c(
        expectation[["test-rqr-native-fixed-design-v1.R"]], 1L
      )
    ),
    rhs_ns_conditional_checks = category(
      "native_rhs_ns_joint_and_full_conditionals",
      "test-rqr-native-rhs-ns.R",
      expectation[["test-rqr-native-rhs-ns.R"]]
    ),
    continuation_checks = category(
      c(
        "static_and_custom_desn_6_equals_2_plus_2_plus_2",
        paste0("attested_desn_", references$seed_id)
      ),
      c(
        "test-rqr-native-ordinary-v1-reference-cells.R",
        rep("desn_future_checks.csv", 4L)
      ),
      c(
        expectation[[
          "test-rqr-native-ordinary-v1-reference-cells.R"
        ]],
        rep(1L, 4L)
      )
    ),
    history_mutation_checks = category(
      c(
        "static_checkpoint_and_history_mutations",
        "desn_envelope_and_materialization_mutations",
        "authorization_and_evidence_mutations"
      ),
      c(
        "test-rqr-native-fixed-design-v1.R",
        "test-rqr-native-desn-fit-v1.R",
        "test-rqr-native-ordinary-v1-validation-runner.R"
      ),
      c(
        expectation[["test-rqr-native-fixed-design-v1.R"]],
        expectation[["test-rqr-native-desn-fit-v1.R"]],
        expectation[[
          "test-rqr-native-ordinary-v1-validation-runner.R"
        ]]
      )
    ),
    fixture_manifest = stamp(expected_fixture_manifest),
    seed_ledger = stamp(config$seed_ledger),
    failure_log = stamp(runner$rqr_ordinary_v1_empty_failure()),
    resource_summary = stamp(data.frame(
      monitor_active = TRUE,
      cleanup_traps = TRUE,
      final_pgid_sweep = TRUE,
      timeout_seconds =
        60 * as.numeric(config$resources$hard_timeout_minutes),
      maximum_processes = config$resources$maximum_processes,
      maximum_threads = config$resources$maximum_threads,
      maximum_artifact_bytes =
        config$resources$maximum_artifact_bytes,
      contract_pass = TRUE,
      stringsAsFactors = FALSE
    ))
  )
  list(
    config = config,
    tables = tables,
    attested_design = attested_design,
    expected_fixture_manifest = expected_fixture_manifest
  )
}

test_that("ordinary-v1 configuration freezes a unique disabled 48-fit grid", {
  runner <- ordinary_v1_runner_environment()
  config <- ordinary_v1_config(runner)

  expect_identical(
    config$runner_modes,
    c(
      "preflight", "reference-only", "benchmark-one-cell",
      "execute-bounded"
    )
  )
  expect_false(config$ordinary_v1_execute_enabled)
  expect_false(config$matched_simulation_authorized)
  expect_true(config$generalized_bayes)
  expect_false(config$response_likelihood)
  expect_false(config$response_prediction_contract)
  expect_identical(nrow(config$fit_plan), 48L)
  expect_identical(length(unique(config$fit_plan$cell_id)), 12L)
  expect_true(all(table(config$fit_plan$cell_id) == 4L))
  expect_identical(length(unique(config$fit_plan$seed)), 48L)
  expect_identical(
    unique(config$fit_plan$cell_id[
      config$fit_plan$family == "desn"
    ]),
    sprintf("DESN%02d", 1:4)
  )
  expect_identical(
    unique(config$fit_plan$fixture_id[
      config$fit_plan$family == "desn"
    ]),
    "D02"
  )
  expect_identical(
    config$fixtures$D02$effective_arguments$seed,
    config$fixtures$D02$materializer_seed
  )
  expect_identical(nrow(config$benchmark_plan), 4L)
  expect_identical(unique(config$benchmark_plan$cell_id), "BENCH01")
  expect_identical(
    config$benchmark_plan$seed, as.integer(82961:82964)
  )
  expect_identical(
    config$desn_schema_contract,
    c(
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
  )
  expect_false(
    "fit_readout" %in%
      names(config$fixtures$D02$effective_arguments)
  )
  expect_identical(config$mcmc$burn_in, 1000L)
  expect_identical(config$mcmc$retained_per_chain, 3000L)
  expect_identical(config$mcmc$root_swap_probability, 0.5)
  expect_identical(
    config$mcmc$prior_state_draw_storage, "rhs_ns_only"
  )
  expect_true(all(vapply(
    config$mcmc$initialization_profiles,
    function(profile) {
      identical(
        names(profile),
        c("midpoint_shift", "half_width", "rhs_scale_multiplier")
      )
    },
    logical(1L)
  )))
  expect_match(config$seed_ledger_note, "82731:82734")
  expect_match(config$seed_ledger_note, "requires independent review")
  expect_identical(
    names(config$protected_dlm_sha256),
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
  )
  expect_true(all(grepl(
    "^[[:xdigit:]]{64}$", unname(config$protected_dlm_sha256)
  )))
  bad <- config
  names(bad$protected_dlm_sha256)[[5L]] <-
    "application/R/an_unreviewed_replacement.R"
  expect_error(
    runner$rqr_ordinary_v1_validate_config(bad),
    "protected RQR-DLM"
  )

  bad <- config
  bad$fit_plan$seed[[2L]] <- bad$fit_plan$seed[[1L]]
  expect_error(
    runner$rqr_ordinary_v1_validate_config(bad),
    "reuses a seed"
  )
  bad <- config
  bad$fit_plan$seed[c(1L, 2L)] <- rev(bad$fit_plan$seed[c(1L, 2L)])
  expect_error(
    runner$rqr_ordinary_v1_validate_config(bad),
    "frozen reviewed plan exactly"
  )
  bad <- config
  bad$benchmark_plan$seed[[1L]] <- bad$fit_plan$seed[[1L]]
  expect_error(
    runner$rqr_ordinary_v1_validate_config(bad),
    "benchmark contract changed"
  )
  bad <- config
  benchmark_row <- which(bad$seed_ledger$purpose == "benchmark_chain")[[1L]]
  bad$seed_ledger$stage[[benchmark_row]] <- "execute-bounded"
  expect_error(
    runner$rqr_ordinary_v1_validate_config(bad),
    "seed-ledger rows"
  )
  bad <- config
  bad$ordinary_v1_execute_enabled <- TRUE
  expect_error(
    runner$rqr_ordinary_v1_validate_config(bad),
    "reviewed implementation SHA"
  )
  bad <- config
  bad$fit_plan$fixture_id[bad$fit_plan$family == "desn"] <- "D01"
  expect_error(
    runner$rqr_ordinary_v1_validate_config(bad),
    "all sixteen DESN fits"
  )
  bad <- config
  bad$fixtures$D02$effective_arguments$seed <- 1L
  expect_error(
    runner$rqr_ordinary_v1_validate_config(bad),
    "D02 attested materialization"
  )
  bad <- config
  bad$desn_schema_contract[["fit"]] <- "rqrgibbs_desn_fit/0.0.0"
  expect_error(
    runner$rqr_ordinary_v1_validate_config(bad),
    "DESN schema contract"
  )
  bad <- config
  bad$mcmc$prior_state_draw_storage <- FALSE
  expect_error(
    runner$rqr_ordinary_v1_validate_config(bad),
    "MCMC schedule changed"
  )
  bad <- config
  bad$mcmc$store_prior_state_draws <- FALSE
  expect_error(
    runner$rqr_ordinary_v1_validate_config(bad),
    "MCMC schedule changed"
  )
  bad <- config
  bad$mcmc$initialization_profiles$low_wide$lambda_initial <- 0.5
  expect_error(
    runner$rqr_ordinary_v1_validate_config(bad),
    "initialization profiles are malformed"
  )
})

test_that("initial-state evidence hashes the exact RHS state passed to fits", {
  runner <- ordinary_v1_runner_environment()
  config <- ordinary_v1_config(runner)
  X <- cbind(intercept = 1, x = c(-1, 0, 1), z = c(1, -1, 0))
  rhs <- list(
    type = "rhs_ns",
    hypers = list(
      intercept_name = "intercept",
      tau0 = 0.7,
      zeta2_fixed = NULL
    )
  )
  profile <- config$mcmc$initialization_profiles$low_wide
  init <- runner$rqr_ordinary_v1_initial_state(X, profile, rhs)

  expect_identical(
    names(init),
    c(
      "beta_root1", "beta_root2",
      "beta_prior_state1", "beta_prior_state2"
    )
  )
  expect_false("lambda" %in% names(init))
  expect_identical(
    names(init$beta_prior_state1$lambda2), c("x", "z")
  )
  expect_identical(
    names(init$beta_prior_state2$lambda2), c("x", "z")
  )
  expect_equal(
    init$beta_prior_state1$lambda2,
    c(x = profile$rhs_scale_multiplier, z = profile$rhs_scale_multiplier)
  )
  expect_equal(
    init$beta_prior_state2$lambda2,
    c(
      x = 1 / profile$rhs_scale_multiplier,
      z = 1 / profile$rhs_scale_multiplier
    )
  )
  expect_true(
    runner$rqr_ordinary_v1_retain_prior_state_draws(config, rhs)
  )
  expect_false(
    runner$rqr_ordinary_v1_retain_prior_state_draws(
      config, list(type = "ridge")
    )
  )

  ridge_init <- runner$rqr_ordinary_v1_initial_state(
    X, profile, list(type = "ridge")
  )
  expect_identical(names(ridge_init), c("beta_root1", "beta_root2"))
  expect_error(
    runner$rqr_ordinary_v1_initial_state(
      unname(X), profile, list(type = "ridge")
    ),
    "initial-state inputs are invalid"
  )
  wrong_name <- X
  colnames(wrong_name)[[1L]] <- "constant"
  expect_error(
    runner$rqr_ordinary_v1_initial_state(
      wrong_name, profile, list(type = "ridge")
    ),
    "exact declared intercept"
  )
  nonconstant <- X
  nonconstant[[1L, 1L]] <- 0
  expect_error(
    runner$rqr_ordinary_v1_initial_state(
      nonconstant, profile, list(type = "ridge")
    ),
    "constant and equal to one"
  )
  wrong_rhs_name <- rhs
  wrong_rhs_name$hypers$intercept_name <- "constant"
  expect_error(
    runner$rqr_ordinary_v1_initial_state(
      X, profile, wrong_rhs_name
    ),
    "exact declared intercept"
  )
})

test_that("execution accepts only the two exact frozen plans", {
  runner <- ordinary_v1_runner_environment()
  config <- ordinary_v1_config(runner)

  expect_identical(
    runner$rqr_ordinary_v1_execution_plan_kind(
      config, config$fit_plan
    ),
    "execute-bounded"
  )
  expect_identical(
    runner$rqr_ordinary_v1_execution_plan_kind(
      config, config$benchmark_plan
    ),
    "benchmark-one-cell"
  )

  subset <- config$fit_plan[-nrow(config$fit_plan), , drop = FALSE]
  expect_error(
    runner$rqr_ordinary_v1_execution_plan_kind(config, subset),
    "subsets, hybrids, and seed changes"
  )
  mutated <- config$benchmark_plan
  mutated$seed[c(1L, 2L)] <- rev(mutated$seed[c(1L, 2L)])
  expect_error(
    runner$rqr_ordinary_v1_execution_plan_kind(config, mutated),
    "subsets, hybrids, and seed changes"
  )
  hybrid <- rbind(
    config$benchmark_plan[1:2, , drop = FALSE],
    config$fit_plan[1:2, , drop = FALSE]
  )
  expect_error(
    runner$rqr_ordinary_v1_execution_plan_kind(config, hybrid),
    "subsets, hybrids, and seed changes"
  )
})

test_that("family provenance keeps static fits standalone", {
  runner <- ordinary_v1_runner_environment()
  primary <- list(
    repo_root = "/tmp/repository",
    expected_git_commit = paste(rep("a", 40L), collapse = ""),
    primary_runtime_attestation = "/tmp/primary-attestation.csv"
  )
  controls <- list(
    fixed_design = c(
      primary,
      list(
        external_repositories = list(),
        required_external_repositories = character(0)
      )
    ),
    desn = c(
      primary,
      list(
        external_repositories = list(exdqlm = list()),
        required_external_repositories = "exdqlm"
      )
    )
  )
  static <- runner$rqr_ordinary_v1_family_provenance_control(
    controls, "fixed_design"
  )
  desn <- runner$rqr_ordinary_v1_family_provenance_control(
    controls, "desn"
  )
  expect_length(static$required_external_repositories, 0L)
  expect_length(static$external_repositories, 0L)
  expect_identical(desn$required_external_repositories, "exdqlm")

  bad_static <- controls$fixed_design
  bad_static$required_external_repositories <- "exdqlm"
  expect_error(
    runner$rqr_ordinary_v1_family_provenance_control(
      bad_static, "fixed_design"
    ),
    "must not require exdqlm"
  )
  bad_desn <- controls$desn
  bad_desn$required_external_repositories <- character(0)
  expect_error(
    runner$rqr_ordinary_v1_family_provenance_control(
      bad_desn, "desn"
    ),
    "must require the pinned exdqlm"
  )
})

test_that("authorization accepts only a strict config-only flag commit", {
  skip_if(!nzchar(Sys.which("git")), "git is required")
  runner <- ordinary_v1_runner_environment()
  fixture <- ordinary_v1_authorization_repository(withr::local_tempdir())

  authorized <- fixture$base_config
  authorized$ordinary_v1_execute_enabled <- TRUE
  authorized$reviewed_implementation_commit <- fixture$base_commit
  ordinary_v1_authorization_write_config(fixture$repo_root, authorized)
  authorization_commit <- ordinary_v1_authorization_commit(
    fixture$repo_root, "flag-only authorization"
  )

  observed <- runner$rqr_ordinary_v1_authorization_status(
    fixture$repo_root, authorized, authorization_commit
  )
  expect_identical(observed$status, "flag_only_authorization_verified")
  expect_identical(
    observed$reviewed_implementation_commit, fixture$base_commit
  )
  expect_true(observed$authorization_delta_verified)

  same_commit <- fixture$base_config
  same_commit$ordinary_v1_execute_enabled <- TRUE
  same_commit$reviewed_implementation_commit <- fixture$base_commit
  expect_error(
    runner$rqr_ordinary_v1_authorization_status(
      fixture$repo_root, same_commit, fixture$base_commit
    ),
    "strict ancestor"
  )

  writeLines(
    "unauthorized source change",
    file.path(fixture$repo_root, "unexpected-source.txt"),
    useBytes = TRUE
  )
  extra_file_commit <- ordinary_v1_authorization_commit(
    fixture$repo_root, "unauthorized extra file"
  )
  expect_error(
    runner$rqr_ordinary_v1_authorization_status(
      fixture$repo_root, authorized, extra_file_commit
    ),
    "only in the frozen ordinary-v1 config"
  )

  ordinary_v1_authorization_test_git(
    fixture$repo_root,
    c("checkout", "--quiet", "-b", "extra-config", fixture$base_commit)
  )
  extra_config <- authorized
  extra_config$unauthorized_field <- "changed"
  ordinary_v1_authorization_write_config(fixture$repo_root, extra_config)
  extra_config_commit <- ordinary_v1_authorization_commit(
    fixture$repo_root, "unauthorized config field"
  )
  expect_error(
    runner$rqr_ordinary_v1_authorization_status(
      fixture$repo_root, extra_config, extra_config_commit
    ),
    "changes beyond the execution flag and reviewed implementation SHA"
  )

  nonclosed_base <- list(
    ordinary_v1_execute_enabled = TRUE,
    reviewed_implementation_commit = NA_character_,
    immutable_contract_field = "frozen"
  )
  nonclosed <- ordinary_v1_authorization_repository(
    withr::local_tempdir(), nonclosed_base
  )
  nonclosed_child <- nonclosed$base_config
  nonclosed_child$reviewed_implementation_commit <- nonclosed$base_commit
  ordinary_v1_authorization_write_config(
    nonclosed$repo_root, nonclosed_child
  )
  nonclosed_child_commit <- ordinary_v1_authorization_commit(
    nonclosed$repo_root, "child of enabled base"
  )
  expect_error(
    runner$rqr_ordinary_v1_authorization_status(
      nonclosed$repo_root, nonclosed_child, nonclosed_child_commit
    ),
    "base is not fail-closed"
  )
})

test_that("future-DESN evidence preserves parent sidecars and nonpromotion", {
  runner <- ordinary_v1_runner_environment()
  config <- ordinary_v1_config(runner)
  future <- structure(
    list(
      schema_version =
        config$desn_schema_contract[["future_design"]],
      verification = list(
        schema_version =
          config$desn_schema_contract[["future_verification"]],
        contract_verified = TRUE,
        legacy_explicit_matrix = FALSE
      ),
      semantic_digest = paste(rep("a", 64L), collapse = "")
    ),
    class = c("rqr_desn_future_design", "list")
  )
  fit <- structure(
    list(schema_version = config$desn_schema_contract[["fit"]]),
    class = c("rqr_desn_fit", "list")
  )
  valid <- list(
    future_design = future,
    future_contract_verified = TRUE,
    legacy_future_matrix = FALSE,
    parent_design_materialization_external_binding_verified = TRUE,
    parent_fit_reproducibility_eligible = TRUE,
    parent_fit_promotion_eligible = TRUE,
    future_external_provenance_bound = FALSE,
    future_reproducibility_eligible = FALSE,
    reproducibility_eligible = FALSE,
    promotion_eligible = FALSE,
    promotion_status =
      "verified_future_contract_unattested_materialization",
    response_predictive_draws = FALSE,
    lower_draws = matrix(c(-1, -0.5), nrow = 1L),
    upper_draws = matrix(c(1, 1.5), nrow = 1L)
  )
  evidence <- runner$rqr_ordinary_v1_validate_desn_prediction(
    valid, fit, config$desn_schema_contract
  )
  expect_identical(evidence$status, "pass")
  for (field in c(
      "future_contract_verified", "legacy_future_matrix",
      "parent_design_materialization_external_binding_verified",
      "parent_fit_reproducibility_eligible",
      "parent_fit_promotion_eligible",
      "future_external_provenance_bound",
      "future_reproducibility_eligible", "reproducibility_eligible",
      "promotion_eligible", "response_predictive_draws"
    )) {
    bad <- valid
    bad[[field]] <- !bad[[field]]
    expect_error(
      runner$rqr_ordinary_v1_validate_desn_prediction(
        bad, fit, config$desn_schema_contract
      ),
      "conditional-root contract"
    )
  }
  bad <- valid
  bad$promotion_status <- "mutated"
  expect_error(
    runner$rqr_ordinary_v1_validate_desn_prediction(
      bad, fit, config$desn_schema_contract
    ),
    "conditional-root contract"
  )
})

test_that("canonical fixed-design and future-DESN fixtures construct exactly", {
  runner <- ordinary_v1_runner_environment()
  config <- ordinary_v1_config(runner)
  fixtures <- runner$rqr_ordinary_v1_build_fixtures(config)

  expect_identical(names(fixtures), c(
    "F01", "F02", "F03", "F04", "F05", "D01", "D03"
  ))
  expect_identical(dim(fixtures$F02$X), c(9L, 3L))
  expect_identical(which(is.na(fixtures$F02$y)), c(3L, 7L))
  expect_identical(dim(fixtures$F04$X), c(10L, 4L))
  expect_identical(which(is.na(fixtures$F04$y)), c(4L, 8L))
  expect_s3_class(fixtures$D01, "rqr_desn_design")
  expect_identical(which(is.na(fixtures$D01$y)), c(3L, 9L))
  expect_identical(
    names(fixtures$D03),
    c(
      "precomputed_design", "teacher_forced_one_step",
      "external_driver_path"
    )
  )
  expect_true(all(vapply(
    fixtures$D03,
    function(x) {
      inherits(x, "rqr_desn_future_design") &&
        nrow(x$X) == 2L &&
        identical(x$parent$semantic_digest, fixtures$D01$semantic_digest)
    },
    logical(1L)
  )))
})

test_that("cell diagnostics preserve iteration-chain-variable orientation", {
  runner <- ordinary_v1_runner_environment()
  config <- ordinary_v1_config(runner)
  observed_dim <- NULL
  posterior_stub <- list(
    as_draws_array = function(x) {
      observed_dim <<- dim(x)
      structure(x, class = c("draws_array", "draws", "array"))
    },
    rhat = function(x) setNames(rep(1, dim(x)[3L]), dimnames(x)[[3L]]),
    ess_bulk = function(x) {
      setNames(rep(1200, dim(x)[3L]), dimnames(x)[[3L]])
    },
    ess_tail = function(x) {
      setNames(rep(1100, dim(x)[3L]), dimnames(x)[[3L]])
    },
    mcse_mean = function(x) {
      setNames(rep(0.01, dim(x)[3L]), dimnames(x)[[3L]])
    }
  )
  chains <- lapply(seq_len(4L), function(chain) {
    matrix(
      c(seq_len(5L) + chain, 10 + seq_len(5L) + chain),
      nrow = 5L,
      dimnames = list(NULL, c("midpoint_t001", "width_t001"))
    )
  })
  result <- runner$rqr_ordinary_v1_diagnose_cell(
    chains, config$gates, posterior_namespace = posterior_stub
  )
  expect_true(result$pass)
  expect_identical(observed_dim, c(5L, 4L, 2L))
  expect_identical(result$diagnostics$estimand,
                   c("midpoint_t001", "width_t001"))

  posterior_stub$rhat <- function(x) {
    setNames(c(1, 1.02), dimnames(x)[[3L]])
  }
  result <- runner$rqr_ordinary_v1_diagnose_cell(
    chains, config$gates, posterior_namespace = posterior_stub
  )
  expect_false(result$pass)
  expect_false(result$diagnostics$pass[[2L]])
  expect_error(
    runner$rqr_ordinary_v1_diagnose_cell(
      chains, config$gates,
      posterior_namespace = posterior_stub,
      expected_schema = rev(colnames(chains[[1L]]))
    ),
    "schemas must be finite and identical"
  )
  posterior_stub$mcse_mean <- function(x) {
    setNames(
      rep(0.01, dim(x)[3L]),
      rev(dimnames(x)[[3L]])
    )
  }
  expect_error(
    runner$rqr_ordinary_v1_diagnose_cell(
      chains, config$gates, posterior_namespace = posterior_stub
    ),
    "do not match the exact estimand schema"
  )
})

test_that("estimand schemas are exact across families, modes, and priors", {
  runner <- ordinary_v1_runner_environment()
  coefficient_names <- c("intercept", "x", "z")
  active_names <- c("x", "z")
  cases <- expand.grid(
    family = c("fixed_design", "desn"),
    mode = c("fixed_rate", "learned_pseudoresidual_normalized"),
    prior = c(
      "ridge", "gaussian", "rhs_ns_sampled", "rhs_ns_fixed"
    ),
    stringsAsFactors = FALSE
  )
  for (index in seq_len(nrow(cases))) {
    row <- cases[index, , drop = FALSE]
    rhs <- startsWith(row$prior[[1L]], "rhs_ns")
    horizon <- if (identical(row$family[[1L]], "desn")) 2L else 0L
    schema <- runner$rqr_ordinary_v1_expected_estimand_columns(
      family = row$family[[1L]],
      learning_rate_mode = row$mode[[1L]],
      prior_id = row$prior[[1L]],
      n_training = 5L,
      coefficient_names = coefficient_names,
      horizon = horizon,
      rhs_active_names = if (rhs) active_names else character(0)
    )
    expect_identical(anyDuplicated(schema), 0L)
    expect_false(any(grepl("_root[12]$", schema)))
    expect_identical(
      "log_lambda" %in% schema,
      identical(
        row$mode[[1L]], "learned_pseudoresidual_normalized"
      )
    )
    expect_identical(
      any(startsWith(schema, "future_")),
      identical(row$family[[1L]], "desn")
    )
    expect_identical(
      any(startsWith(schema, "log_zeta2_ordered_")),
      identical(row$prior[[1L]], "rhs_ns_sampled")
    )
    if (rhs) {
      expect_true(all(paste0(
        "log_lambda2_ordered_lower_", active_names
      ) %in% schema))
      sidecar <-
        runner$rqr_ordinary_v1_expected_rhs_sidecar_columns(
          row$prior[[1L]], active_names
        )
      expect_true(all(grepl("_root[12]$", sidecar)))
      expect_false(any(sidecar %in% schema))
    }
  }

  expect_error(
    runner$rqr_ordinary_v1_expected_estimand_columns(
      "fixed_design", "fixed_rate", "ridge", 5L,
      coefficient_names, horizon = 2L
    ),
    "family and future-horizon"
  )
  expect_error(
    runner$rqr_ordinary_v1_expected_estimand_columns(
      "desn", "fixed_rate", "rhs_ns_fixed", 5L,
      coefficient_names, horizon = 2L,
      rhs_active_names = c("x", "x")
    ),
    "exact named active"
  )
})

test_that("RHS diagnostics are label invariant and fixed shoulders are exact", {
  runner <- ordinary_v1_runner_environment()
  config <- ordinary_v1_config(runner)
  specifications <- list(
    ridge = list(fixture = config$fixtures$F03, mode = "fixed_rate"),
    gaussian = list(
      fixture = config$fixtures$F02,
      mode = "learned_pseudoresidual_normalized"
    ),
    rhs_ns_sampled = list(
      fixture = config$fixtures$F04,
      mode = "learned_pseudoresidual_normalized"
    ),
    rhs_ns_fixed = list(
      fixture = config$fixtures$F05,
      mode = "fixed_rate"
    )
  )
  results <- list()
  for (index in seq_along(specifications)) {
    prior_id <- names(specifications)[[index]]
    specification <- specifications[[index]]
    X <- specification$fixture$X
    y <- specification$fixture$y
    prior <- runner$rqr_ordinary_v1_prior(config, prior_id)
    init <- runner$rqr_ordinary_v1_initial_state(
      X, config$mcmc$initialization_profiles[[1L]], prior
    )
    fit <- rqrgibbs::rqr_mcmc_fit(
      y = y, X = X, coverage_level = config$coverage_level,
      learning_rate = config$fixed_learning_rate,
      loss_reference_scale = config$loss_reference_scale,
      learning_rate_mode = specification$mode,
      lambda_prior = config$lambda_prior,
      beta_prior_obj = prior,
      numerical_policy = "fail",
      mcmc_control = list(
        n_burn = 0L, n_mcmc = 4L, thin = 1L,
        seed = 9310L + index,
        store_prior_state_draws =
          startsWith(prior_id, "rhs_ns")
      ),
      init = init
    )
    result <- runner$rqr_ordinary_v1_chain_estimands(
      fit, X, y, specification$mode,
      expected_family = "fixed_design",
      expected_prior_id = prior_id
    )
    expected <- runner$rqr_ordinary_v1_expected_estimand_columns(
      family = "fixed_design",
      learning_rate_mode = specification$mode,
      prior_id = prior_id,
      n_training = nrow(X),
      coefficient_names = colnames(X),
      rhs_active_names = if (startsWith(prior_id, "rhs_ns")) {
        setdiff(colnames(X), "intercept")
      } else {
        character(0)
      }
    )
    expect_identical(colnames(result$values), expected)
    expect_false(any(grepl("_root[12]$", colnames(result$values))))
    results[[prior_id]] <- list(fit = fit, result = result, X = X, y = y)
  }

  sampled <- results$rhs_ns_sampled$result
  expect_true(any(startsWith(
    colnames(sampled$values), "log_zeta2_ordered_"
  )))
  expect_true(all(grepl(
    "_(x1|x2|x3)(_root[12])?$",
    grep(
      "log_lambda2", c(
        colnames(sampled$values),
        colnames(sampled$rhs_root_specific_sidecar)
      ),
      value = TRUE
    )
  )))
  expect_equal(nrow(sampled$fixed_parameter_checks), 0L)

  fixed <- results$rhs_ns_fixed
  expect_false(any(grepl(
    "zeta2", c(
      colnames(fixed$result$values),
      colnames(fixed$result$rhs_root_specific_sidecar)
    )
  )))
  expect_identical(
    fixed$result$fixed_parameter_checks$root,
    c("root1", "root2")
  )
  expect_true(all(
    fixed$result$fixed_parameter_checks$exact_identity
  ))
  expect_true(all(
    fixed$result$fixed_parameter_checks$expected_value ==
      config$fixtures$F05$prior$zeta2_fixed
  ))

  mutated <- fixed$fit
  mutated$samp.beta_prior_state_root1[[2L]]$zeta2 <-
    mutated$samp.beta_prior_state_root1[[2L]]$zeta2 + 1
  expect_error(
    runner$rqr_ordinary_v1_chain_estimands(
      mutated, fixed$X, fixed$y, "fixed_rate",
      expected_family = "fixed_design",
      expected_prior_id = "rhs_ns_fixed"
    ),
    "failed exact identity"
  )
})

test_that("the bounded driver stops before a later cell after cell failure", {
  runner <- ordinary_v1_runner_environment()
  config <- ordinary_v1_config(runner)
  plan <- config$fit_plan[config$fit_plan$cell_id %in% c("S01", "S02"), ]
  calls <- character(0)
  published <- character(0)
  expect_error(
    runner$rqr_ordinary_v1_run_cells(
      plan,
      fit_chain = function(row) {
        calls <<- c(calls, paste(row$cell_id, row$chain, sep = ":"))
        list(
          estimands = matrix(
            row$seed[[1L]], nrow = 2L, ncol = 1L,
            dimnames = list(NULL, "midpoint_t001")
          )
        )
      },
      diagnose_cell = function(chains, rows) {
        list(
          pass = FALSE,
          diagnostics = data.frame(estimand = "midpoint_t001")
        )
      },
      publish_cell = function(rows, diagnosis) {
        published <<- c(published, rows$cell_id[[1L]])
      }
    ),
    "later cells were not started"
  )
  expect_identical(calls, paste("S01", 1:4, sep = ":"))
  expect_identical(published, "S01")
})

test_that("compact artifacts are read back, hashed, and atomically published", {
  runner <- ordinary_v1_runner_environment()
  directory <- withr::local_tempdir()
  path <- file.path(directory, "reference_gates.csv")
  runner$rqr_ordinary_v1_atomic_csv(
    data.frame(gate_id = "fixture", status = "pass"),
    path
  )
  expect_true(file.exists(path))
  runner$rqr_ordinary_v1_atomic_csv(
    data.frame(gate_id = "replacement", status = "pass"),
    path
  )
  expect_identical(
    utils::read.csv(path, stringsAsFactors = FALSE)$gate_id,
    "replacement"
  )
  manifest <- runner$rqr_ordinary_v1_artifact_manifest(directory)
  expect_identical(manifest$relative_path, "reference_gates.csv")
  expect_true(runner$rqr_ordinary_v1_validate_artifact_manifest(
    directory, manifest
  ))
  runner$rqr_ordinary_v1_atomic_csv(
    manifest, file.path(directory, "artifact_hashes.csv")
  )
  reread <- utils::read.csv(
    file.path(directory, "artifact_hashes.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  expect_true(runner$rqr_ordinary_v1_validate_artifact_manifest(
    directory, reread
  ))
  empty_path <- file.path(directory, "failure_log.csv")
  runner$rqr_ordinary_v1_atomic_csv(
    runner$rqr_ordinary_v1_empty_failure(), empty_path
  )
  empty <- utils::read.csv(
    empty_path, stringsAsFactors = FALSE, check.names = FALSE
  )
  expect_identical(nrow(empty), 0L)
  expect_identical(
    names(empty),
    c(
      "schema_version", "timestamp_utc", "mode", "cell_id", "chain",
      "error_class", "message"
    )
  )
})

test_that("compact posterior summaries retain named estimands", {
  runner <- ordinary_v1_runner_environment()
  values <- cbind(
    midpoint_t001 = c(-1, 0, 1),
    width_t001 = c(1, 2, 3)
  )
  summary <- runner$rqr_ordinary_v1_compact_summary(values)
  expect_identical(
    summary$estimand, c("midpoint_t001", "width_t001")
  )
  expect_equal(summary$mean, c(0, 2))
  expect_true(all(is.finite(as.matrix(summary[-1L]))))
})

test_that("wrapper manifest binds a closed monitor and R-evidence file set", {
  runner <- ordinary_v1_runner_environment()
  root <- withr::local_tempdir()
  output <- file.path(root, "output")
  monitor <- file.path(root, "monitor")
  dir.create(output)
  dir.create(monitor)
  output_names <- c(
    "artifact_hashes.csv", "run_status.csv", "source_state.csv",
    "validation_config_digest.csv"
  )
  monitor_names <- c(
    "process_group_monitor.csv", "process_group_resource_summary.csv",
    "runner.stderr.log", "runner.stdout.log", "wrapper_closeout.csv"
  )
  for (name in output_names) {
    writeLines(paste("output", name), file.path(output, name))
  }
  for (name in monitor_names) {
    writeLines(paste("monitor", name), file.path(monitor, name))
  }
  rows <- rbind(
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
  actual_path <- function(role, path) {
    if (identical(role, "monitor_evidence")) {
      file.path(monitor, sub("^monitor/", "", path))
    } else {
      file.path(output, sub("^output/", "", path))
    }
  }
  rows$bytes <- vapply(seq_len(nrow(rows)), function(index) {
    as.numeric(file.info(actual_path(
      rows$role[[index]], rows$path[[index]]
    ))$size)
  }, numeric(1L))
  rows$sha256 <- vapply(seq_len(nrow(rows)), function(index) {
    runner$rqr_ordinary_v1_sha256_file(actual_path(
      rows$role[[index]], rows$path[[index]]
    ))
  }, character(1L))
  utils::write.csv(
    rows, file.path(monitor, "wrapper_artifact_hashes.csv"),
    row.names = FALSE
  )
  expect_invisible(
    runner$rqr_ordinary_v1_validate_wrapper_manifest(
      output, monitor, rows
    )
  )

  writeLines("mutated", file.path(output, "run_status.csv"))
  expect_error(
    runner$rqr_ordinary_v1_validate_wrapper_manifest(
      output, monitor, rows
    ),
    "does not match current bytes"
  )
  writeLines(paste("output", "run_status.csv"),
             file.path(output, "run_status.csv"))
  writeLines("unexpected", file.path(monitor, "unexpected.log"))
  expect_error(
    runner$rqr_ordinary_v1_validate_wrapper_manifest(
      output, monitor, rows
    ),
    "invalid closed file set"
  )
})

test_that("candidate evidence requires the same runtime content and toolchain", {
  runner <- ordinary_v1_runner_environment()
  reviewed <- paste(rep("a", 40L), collapse = "")
  current_commit <- paste(rep("b", 40L), collapse = "")
  pinned <- paste(rep("c", 40L), collapse = "")
  make_runtime <- function(primary_commit) {
    data.frame(
      package = c("rqrgibbs", "exdqlm"),
      version = c("0.1.0.9000", "0.4.0"),
      source_commit = c(primary_commit, pinned),
      runtime_path = c("/isolated/rqrgibbs", "/isolated/exdqlm"),
      runtime_tree_digest = c(
        paste(rep("d", 64L), collapse = ""),
        paste(rep("e", 64L), collapse = "")
      ),
      runtime_source_match = TRUE,
      reproducibility_eligible = TRUE,
      runtime_attestation_schema = "runtime/4.0.0",
      attestation_sha256 = c(
        paste(rep("1", 64L), collapse = ""),
        paste(rep("2", 64L), collapse = "")
      ),
      R_version = "R test",
      platform = "test-platform",
      compiler = "test-compiler",
      BLAS = "test-blas",
      LAPACK = "test-lapack",
      posterior_version = "1.7.0",
      stringsAsFactors = FALSE
    )
  }
  recorded <- make_runtime(reviewed)
  current <- make_runtime(current_commit)
  current$attestation_sha256[[1L]] <-
    paste(rep("3", 64L), collapse = "")
  expect_invisible(
    runner$rqr_ordinary_v1_validate_runtime_compatibility(
      recorded, current, reviewed, current_commit, pinned
    )
  )

  bad <- current
  bad$runtime_tree_digest[[1L]] <- paste(rep("f", 64L), collapse = "")
  expect_error(
    runner$rqr_ordinary_v1_validate_runtime_compatibility(
      recorded, bad, reviewed, current_commit, pinned
    ),
    "not compatible"
  )
  bad <- current
  bad$posterior_version[[1L]] <- "1.8.0"
  expect_error(
    runner$rqr_ordinary_v1_validate_runtime_compatibility(
      recorded, bad, reviewed, current_commit, pinned
    ),
    "not compatible"
  )
})

test_that("benchmark authorization accepts only a fully bound evidence bundle", {
  runner <- ordinary_v1_runner_environment()
  config <- ordinary_v1_config(runner)
  test_environment <- environment()
  make_bundle <- function() {
    ordinary_v1_benchmark_bundle_fixture(
      runner, config, local_envir = test_environment
    )
  }
  validate <- function(bundle) {
    runner$rqr_ordinary_v1_validate_benchmark_bundle(
      bundle$config,
      bundle$source_state,
      bundle$runtime_table,
      bundle$output_dir,
      bundle$monitor_dir,
      bundle$attested_design
    )
  }
  refresh <- function(bundle) {
    ordinary_v1_benchmark_refresh_integrity(
      runner, bundle$output_dir, bundle$monitor_dir
    )
  }
  rewrite_output <- function(bundle, name, mutate) {
    ordinary_v1_benchmark_rewrite_csv(
      runner, file.path(bundle$output_dir, name), mutate
    )
    refresh(bundle)
  }

  passing <- make_bundle()
  expect_invisible(validate(passing))

  diagnostic_bytes <- make_bundle()
  writeLines(
    "independent byte mutation",
    file.path(diagnostic_bytes$output_dir, "bounded_diagnostics.csv"),
    useBytes = TRUE
  )
  expect_error(validate(diagnostic_bytes), "output hashes do not match")

  diagnostic_gate <- make_bundle()
  rewrite_output(
    diagnostic_gate, "bounded_diagnostics.csv",
    function(value) {
      value$rhat[[1L]] <-
        diagnostic_gate$config$gates$maximum_rank_normalized_rhat + 0.01
      value
    }
  )
  expect_error(validate(diagnostic_gate), "not a passing bound bundle")

  missing_estimand <- make_bundle()
  rewrite_output(
    missing_estimand, "bounded_diagnostics.csv",
    function(value) value[-1L, , drop = FALSE]
  )
  expect_error(validate(missing_estimand), "not a passing bound bundle")

  reordered_estimand <- make_bundle()
  rewrite_output(
    reordered_estimand, "bounded_diagnostics.csv",
    function(value) value[rev(seq_len(nrow(value))), , drop = FALSE]
  )
  expect_error(validate(reordered_estimand), "not a passing bound bundle")

  fixed_identity <- make_bundle()
  rewrite_output(
    fixed_identity, "fixed_parameter_checks.csv",
    function(value) {
      value$exact_identity[[1L]] <- FALSE
      value
    }
  )
  expect_error(validate(fixed_identity), "not a passing bound bundle")

  root_trace_promoted <- make_bundle()
  rewrite_output(
    root_trace_promoted, "bounded_diagnostics.csv",
    function(value) {
      value$estimand[[1L]] <- "log_tau2_root1"
      value
    }
  )
  expect_error(validate(root_trace_promoted), "not a passing bound bundle")

  checkpoint <- make_bundle()
  rewrite_output(
    checkpoint, "checkpoint_manifest.csv",
    function(value) {
      value$numerical_repair_count[[1L]] <- 1L
      value
    }
  )
  expect_error(validate(checkpoint), "not a passing bound bundle")

  initialization_profile <- make_bundle()
  rewrite_output(
    initialization_profile, "initialization_manifest.csv",
    function(value) {
      value$profile[[1L]] <- value$profile[[2L]]
      value
    }
  )
  expect_error(
    validate(initialization_profile), "not a passing bound bundle"
  )

  initialization_state <- make_bundle()
  rewrite_output(
    initialization_state, "initialization_manifest.csv",
    function(value) {
      value$initial_state_digest[[1L]] <-
        value$initial_state_digest[[2L]]
      value
    }
  )
  expect_error(
    validate(initialization_state), "not a passing bound bundle"
  )

  initialization_storage <- make_bundle()
  rewrite_output(
    initialization_storage, "initialization_manifest.csv",
    function(value) {
      value$prior_state_draws_retained[[1L]] <- FALSE
      value
    }
  )
  expect_error(
    validate(initialization_storage), "not a passing bound bundle"
  )

  provenance <- make_bundle()
  rewrite_output(
    provenance, "provenance_checks.csv",
    function(value) {
      value$materialization_receipt_valid[[1L]] <- FALSE
      value
    }
  )
  expect_error(validate(provenance), "not a passing bound bundle")

  future <- make_bundle()
  rewrite_output(
    future, "desn_future_checks.csv",
    function(value) {
      value$future_contract_verified[[1L]] <- FALSE
      value
    }
  )
  expect_error(validate(future), "not a passing bound bundle")

  failure <- make_bundle()
  runner$rqr_ordinary_v1_atomic_csv(
    data.frame(
      timestamp_utc = "2026-07-26T00:00:00Z",
      mode = "benchmark-one-cell",
      cell_id = "BENCH01",
      chain = 1L,
      error_class = "synthetic_failure",
      message = "A nonempty failure ledger must reject promotion.",
      stringsAsFactors = FALSE
    ),
    file.path(failure$output_dir, "failure_log.csv")
  )
  refresh(failure)
  expect_error(validate(failure), "not a passing bound bundle")

  runtime <- make_bundle()
  runtime$runtime_table$runtime_tree_digest[
    runtime$runtime_table$package == "rqrgibbs"
  ] <- paste(rep("f", 64L), collapse = "")
  expect_error(validate(runtime), "not compatible")

  toolchain <- make_bundle()
  toolchain$runtime_table$posterior_version[
    toolchain$runtime_table$package == "rqrgibbs"
  ] <- "1.8.0"
  expect_error(validate(toolchain), "not compatible")

  wrapper_manifest <- make_bundle()
  wrapper_path <- file.path(
    wrapper_manifest$monitor_dir, "wrapper_artifact_hashes.csv"
  )
  wrapper_rows <- utils::read.csv(
    wrapper_path, stringsAsFactors = FALSE, check.names = FALSE
  )
  wrapper_rows$sha256[[1L]] <- paste(rep("f", 64L), collapse = "")
  utils::write.csv(wrapper_rows, wrapper_path, row.names = FALSE)
  expect_error(validate(wrapper_manifest), "does not match current bytes")

  monitor_maximum <- make_bundle()
  monitor_resource_path <- file.path(
    monitor_maximum$monitor_dir,
    "process_group_resource_summary.csv"
  )
  monitor_resources <- utils::read.csv(
    monitor_resource_path, stringsAsFactors = FALSE, check.names = FALSE
  )
  monitor_resources$value[
    monitor_resources$metric == "sampled_peak_processes"
  ] <- "1"
  utils::write.csv(
    monitor_resources, monitor_resource_path, row.names = FALSE
  )
  ordinary_v1_benchmark_refresh_wrapper_manifest(
    runner, monitor_maximum$output_dir, monitor_maximum$monitor_dir
  )
  expect_error(validate(monitor_maximum), "not a passing bound bundle")

  closed_set <- make_bundle()
  writeLines(
    "unexpected monitor evidence",
    file.path(closed_set$monitor_dir, "unexpected.log"),
    useBytes = TRUE
  )
  expect_error(validate(closed_set), "evidence bundle is incomplete")
})

test_that("reference authorization validates exact schemas and semantics", {
  runner <- ordinary_v1_runner_environment()
  config <- ordinary_v1_config(runner)
  make_fixture <- function() {
    ordinary_v1_reference_tables_fixture(runner, config)
  }
  validate <- function(fixture) {
    runner$rqr_ordinary_v1_validate_reference_tables(
      fixture$config,
      fixture$tables,
      fixture$attested_design,
      fixture$expected_fixture_manifest
    )
  }
  expect_invisible(validate(make_fixture()))

  extra_column <- make_fixture()
  extra_column$tables$reference_gates$unreviewed <- "pass"
  expect_error(validate(extra_column), "frozen contract")

  missing_column <- make_fixture()
  missing_column$tables$oracle_comparisons$actual <- NULL
  expect_error(validate(missing_column), "frozen contract")

  duplicate_row <- make_fixture()
  duplicate_row$tables$package_checks <- rbind(
    duplicate_row$tables$package_checks,
    duplicate_row$tables$package_checks[1L, , drop = FALSE]
  )
  expect_error(validate(duplicate_row), "frozen contract")

  missing_row <- make_fixture()
  missing_row$tables$desn_future_checks <-
    missing_row$tables$desn_future_checks[-1L, , drop = FALSE]
  expect_error(validate(missing_row), "frozen contract")

  self_consistent_false_oracle <- make_fixture()
  self_consistent_false_oracle$tables$oracle_comparisons$expected[[1L]] <-
    "1"
  self_consistent_false_oracle$tables$oracle_comparisons$actual[[1L]] <-
    "1"
  self_consistent_false_oracle$tables$oracle_comparisons$pass[[1L]] <-
    TRUE
  expect_error(validate(self_consistent_false_oracle), "frozen contract")

  self_consistent_false_dlm <- make_fixture()
  self_consistent_false_dlm$tables$protected_dlm_hashes$
    expected_sha256[[1L]] <- paste(rep("f", 64L), collapse = "")
  self_consistent_false_dlm$tables$protected_dlm_hashes$
    actual_sha256[[1L]] <- paste(rep("f", 64L), collapse = "")
  self_consistent_false_dlm$tables$protected_dlm_hashes$pass[[1L]] <- TRUE
  expect_error(validate(self_consistent_false_dlm), "frozen contract")

  false_future_claim <- make_fixture()
  false_future_claim$tables$desn_future_checks$
    future_promotion_eligible[[1L]] <- TRUE
  false_future_claim$tables$desn_future_checks$status[[1L]] <- "pass"
  expect_error(validate(false_future_claim), "frozen contract")

  false_category_claim <- make_fixture()
  false_category_claim$tables$missingness_checks$
    evidence_source[[1L]] <- "unreviewed-test.R"
  false_category_claim$tables$missingness_checks$status[[1L]] <- "pass"
  expect_error(validate(false_category_claim), "frozen contract")

  false_seed_claim <- make_fixture()
  false_seed_claim$tables$seed_ledger$seed[[1L]] <-
    false_seed_claim$tables$seed_ledger$seed[[1L]] + 1L
  expect_error(validate(false_seed_claim), "frozen contract")

  false_fixture_claim <- make_fixture()
  false_fixture_claim$tables$fixture_manifest$digest[[1L]] <-
    paste(rep("f", 64L), collapse = "")
  expect_error(validate(false_fixture_claim), "frozen contract")

  false_resource_claim <- make_fixture()
  false_resource_claim$tables$resource_summary$contract_pass[[1L]] <-
    FALSE
  expect_error(validate(false_resource_claim), "frozen contract")
})

test_that("execute authorization fails before reading a bundle", {
  runner <- ordinary_v1_runner_environment()
  config <- ordinary_v1_config(runner)
  expect_error(
    runner$rqr_ordinary_v1_authorize_execute(
      config,
      source_state = data.frame(commit = paste(rep("a", 40), collapse = "")),
      runtime_table = data.frame(runtime_tree_digest = "unused"),
      reference_dir = file.path(tempdir(), "does-not-exist")
    ),
    "disabled in the reviewed tracked configuration"
  )
})

test_that("monitor contract is fail-closed for execution", {
  runner <- ordinary_v1_runner_environment()
  config <- ordinary_v1_config(runner)
  withr::local_envvar(c(
    RQR_ORDINARY_V1_MONITOR_ACTIVE = "",
    RQR_ORDINARY_V1_MONITOR_TIMEOUT_SECONDS = "",
    RQR_ORDINARY_V1_MONITOR_MAX_PROCESSES = "",
    RQR_ORDINARY_V1_MONITOR_MAX_THREADS = "",
    RQR_ORDINARY_V1_MONITOR_MAX_ARTIFACT_BYTES = "",
    RQR_ORDINARY_V1_MONITOR_CLEANUP_TRAPS = "",
    RQR_ORDINARY_V1_MONITOR_FINAL_PGID_SWEEP = ""
  ))
  expect_error(
    runner$rqr_ordinary_v1_monitor_preflight(config, required = TRUE),
    "external process-group monitor"
  )
  evidence <- runner$rqr_ordinary_v1_monitor_preflight(
    config, required = FALSE
  )
  expect_false(evidence$contract_pass)
})
