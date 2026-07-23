#!/usr/bin/env Rscript

# Three-mode bounded RQR-DLM validation runner.
#
# preflight       construct and hash all canonical objects;
# reference-only  execute deterministic and small Monte Carlo reference gates;
# execute-bounded run the frozen 3 x 2 x 4 fits only after a separately reviewed
#                 source commit and explicit dual authorization.
#
# This is target and computation validation for interval-root generalized
# Bayes. It does not simulate posterior-predictive responses and is not the
# matched simulation study.

arguments <- commandArgs(trailingOnly = TRUE)
mode <- if (length(arguments)) arguments[1L] else "preflight"
allowed_modes <- c("preflight", "reference-only", "execute-bounded")
if (!mode %in% allowed_modes) {
  stop(
    "Mode must be one of: ", paste(allowed_modes, collapse = ", "),
    call. = FALSE
  )
}

repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
config_path <- file.path(
  repo_root, "application", "config", "rqr_dlm",
  "rqr_dlm_bounded_dynamic_fixtures_20260723.R"
)
helper_path <- file.path(
  repo_root, "application", "scripts", "lib",
  "rqr_dlm_bounded_fixtures.R"
)
if (!file.exists(config_path) || !file.exists(helper_path)) {
  stop("Run this script from the RQR-GIBBS repository root.", call. = FALSE)
}

git_value <- function(args) {
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

expected_commit <- tolower(Sys.getenv(
  "RQR_EXPECTED_PRIMARY_COMMIT", unset = ""
))
if (!grepl("^[0-9a-f]{40}$", expected_commit)) {
  stop(
    "RQR_EXPECTED_PRIMARY_COMMIT must be the reviewed full source SHA.",
    call. = FALSE
  )
}
actual_commit <- tolower(git_value(c("rev-parse", "HEAD")))
branch <- git_value(c("rev-parse", "--abbrev-ref", "HEAD"))
status <- git_value(c(
  "status", "--porcelain=v2", "--untracked-files=all"
))
if (!identical(branch, "main") ||
    !identical(actual_commit, expected_commit) ||
    nzchar(status)) {
  stop(
    "The bounded runner requires clean main at the exact reviewed SHA.",
    call. = FALSE
  )
}

primary_runtime_root <- normalizePath(
  Sys.getenv(
    "RQR_PRIMARY_RUNTIME_ROOT",
    unset = file.path(dirname(repo_root), ".rqr_gibbs_primary_runtime")
  ),
  winslash = "/", mustWork = TRUE
)
primary_commit_root <- file.path(primary_runtime_root, expected_commit)
primary_library <- normalizePath(
  file.path(primary_commit_root, "library"),
  winslash = "/", mustWork = TRUE
)
primary_attestation <- normalizePath(
  file.path(
    primary_commit_root, "attestations",
    paste0("rqrgibbs_", expected_commit, ".rds")
  ),
  winslash = "/", mustWork = TRUE
)
if ("rqrgibbs" %in% loadedNamespaces()) {
  stop(
    "rqrgibbs was loaded before the isolated library was selected.",
    call. = FALSE
  )
}
.libPaths(c(primary_library, .libPaths()))
required_packages <- c("digest", "jsonlite", "posterior", "rqrgibbs")
missing_packages <- required_packages[!vapply(
  required_packages, requireNamespace, quietly = TRUE,
  FUN.VALUE = logical(1L)
)]
if (length(missing_packages)) {
  stop(
    "Missing bounded-runner packages: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}
if (utils::packageVersion("posterior") < "1.7.0") {
  stop("posterior >= 1.7.0 is required.", call. = FALSE)
}

runtime_state <- rqrgibbs:::.rqr_repository_provenance(list(
  repo_root = repo_root,
  expected_git_commit = expected_commit,
  runtime_package = "rqrgibbs",
  runtime_attestation = primary_attestation,
  require_isolated_runtime = TRUE,
  source_subdir = "application"
))
runtime_gates <- c(
  runtime_attestation_match = runtime_state$runtime_attestation_match,
  source_archive_tree_match = runtime_state$source_archive_tree_match,
  source_package_verified = runtime_state$source_package_verified,
  source_package_archive_match =
    runtime_state$source_package_archive_match,
  build_evidence_verified = runtime_state$build_evidence_verified,
  install_evidence_verified = runtime_state$install_evidence_verified,
  runtime_lineage_marker_match =
    runtime_state$runtime_lineage_marker_match,
  runtime_install_receipt_match =
    runtime_state$runtime_install_receipt_match,
  runtime_source_match = runtime_state$runtime_source_match,
  reproducibility_eligible = runtime_state$reproducibility_eligible
)
if (!all(runtime_gates)) {
  stop(
    "The isolated primary runtime lineage gate failed: ",
    paste(names(runtime_gates)[!runtime_gates], collapse = ", "),
    call. = FALSE
  )
}

config_environment <- new.env(parent = baseenv())
sys.source(config_path, envir = config_environment)
config <- config_environment$rqr_dlm_bounded_dynamic_fixtures
if (!is.list(config) ||
    !identical(
      config$schema_version,
      "rqrgibbs_dlm_bounded_fixtures/3.0.0"
    ) ||
    !identical(config$mcmc$backend, "cpp") ||
    !identical(config$mcmc$numerical_policy, "fail") ||
    !identical(config$mcmc$chains, 4L) ||
    length(config$mcmc$initialization_profiles) != 4L ||
    !isTRUE(config$mcmc$store_state_draws) ||
    isTRUE(config$mcmc$store_latent_draws) ||
    !identical(config$continuation$generation_indices, 0:2) ||
    !identical(
      config$continuation$retained_by_segment, c(2L, 2L, 2L)
    ) ||
    !identical(config$continuation$uninterrupted_retained, 6L)) {
  stop("The frozen bounded-runner configuration is invalid.", call. = FALSE)
}
sys.source(helper_path, envir = environment())
rqr_validate_bounded_dlm_config(config)
monitor_contract <- c(
  timeout_seconds = as.numeric(Sys.getenv(
    "RQR_MONITOR_TIMEOUT_SECONDS", unset = NA_character_
  )),
  maximum_rss_kib = as.numeric(Sys.getenv(
    "RQR_MONITOR_MAX_RSS_KIB", unset = NA_character_
  )),
  maximum_threads = as.numeric(Sys.getenv(
    "RQR_MONITOR_MAX_THREADS", unset = NA_character_
  )),
  maximum_processes = as.numeric(Sys.getenv(
    "RQR_MONITOR_MAX_PROCESSES", unset = NA_character_
  )),
  interval_seconds = as.numeric(Sys.getenv(
    "RQR_MONITOR_INTERVAL_SECONDS", unset = NA_character_
  ))
)
expected_monitor_contract <- c(
  timeout_seconds = 60 * config$resources$hard_timeout_minutes,
  maximum_rss_kib =
    1024^2 * config$resources$maximum_process_tree_rss_gib,
  maximum_threads =
    config$resources$maximum_process_tree_threads,
  maximum_processes =
    config$resources$maximum_process_tree_processes,
  interval_seconds = config$resources$monitor_interval_seconds
)
if (mode != "preflight" &&
    (!all(is.finite(monitor_contract)) ||
      !identical(
        unname(monitor_contract), unname(expected_monitor_contract)
      ))) {
  stop(
    "The active process monitor does not match the frozen resource contract.",
    call. = FALSE
  )
}
constructed <- rqr_build_all_bounded_dlm_fixtures(config)
if (length(constructed) != 3L ||
    any(!vapply(
      constructed,
      function(item) isTRUE(item$evolution$exact_joint_target),
      logical(1L)
    ))) {
  stop("Canonical fixture construction failed.", call. = FALSE)
}

output_dir <- Sys.getenv("RQR_DLM_OUTPUT_DIR", unset = "")
if (!nzchar(output_dir)) {
  run_id <- paste0(
    "rqr_dlm_bounded_", gsub("-", "_", mode),
    "_", format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"),
    "_", substr(actual_commit, 1L, 12L)
  )
  output_dir <- file.path(
    repo_root, "application", "outputs", run_id
  )
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)

construction <- do.call(rbind, lapply(constructed, function(item) {
  audit <- item$construction_audit
  data.frame(
    fixture_id = item$fixture_id,
    state_dimension = audit$state_dimension,
    component_dims = paste(audit$component_dims, collapse = ","),
    component_names = paste(audit$component_names, collapse = ","),
    observed_count = audit$observed_count,
    missing_count = audit$missing_count,
    training_horizon = audit$training_horizon,
    future_horizon = audit$future_horizon,
    evolution_mode = audit$evolution_mode,
    exact_joint_target = audit$exact_joint_target,
    extension_reproduces_training =
      audit$extension_reproduces_training,
    model_digest = audit$model_digest,
    evolution_digest = audit$evolution_digest,
    missing_response_digest = audit$missing_response_digest,
    future_digest = audit$future_digest,
    stringsAsFactors = FALSE
  )
}))
utils::write.csv(
  construction, file.path(output_dir, "fixture_construction.csv"),
  row.names = FALSE
)

base_manifest <- list(
  schema_version = "rqrgibbs_dlm_bounded_run/1.0.0",
  mode = mode,
  config_id = config$config_id,
  config_digest = digest::digest(
    config, algo = "sha256", serialize = TRUE
  ),
  primary_commit = actual_commit,
  primary_application_tree = runtime_state$source_tree_digest,
  primary_runtime_path = runtime_state$runtime_package_path,
  primary_runtime_tree_digest =
    runtime_state$runtime_package_tree_digest,
  primary_runtime_attestation = primary_attestation,
  primary_runtime_attestation_schema =
    runtime_state$runtime_attestation_schema,
  runtime_gates = as.list(runtime_gates),
  requested_fit_count =
    length(constructed) *
    length(config$learning_rate_modes) *
    config$mcmc$chains,
  full_chain_files_ignored = TRUE,
  generalized_bayes = TRUE,
  response_likelihood = FALSE,
  response_prediction_contract = FALSE,
  production_simulation = FALSE,
  process_tree_monitor_active = identical(
    Sys.getenv("RQR_RESOURCE_MONITOR_ACTIVE", unset = "FALSE"),
    "TRUE"
  ),
  recorded_at = format(Sys.time(), tz = "UTC", usetz = TRUE)
)

write_manifest <- function(values) {
  jsonlite::write_json(
    utils::modifyList(base_manifest, values),
    file.path(output_dir, "run_manifest.json"),
    auto_unbox = TRUE, pretty = TRUE, digits = NA, null = "null"
  )
}

if (identical(mode, "preflight")) {
  write_manifest(list(
    status = "passed",
    fixture_construction_passed = TRUE,
    reference_gates_executed = FALSE,
    bounded_dynamic_execution_authorized = FALSE
  ))
  cat("Bounded RQR-DLM runner preflight passed.\n")
  cat("  fixtures:", length(constructed), "\n")
  cat("  prospective fits:", base_manifest$requested_fit_count, "\n")
  cat("  execution authorization: false\n")
  quit(save = "no", status = 0L)
}

reference_gate <- function(name, pass, value = NA_real_,
                           tolerance = NA_real_, detail = "") {
  data.frame(
    gate = name,
    pass = isTRUE(pass),
    value = as.numeric(value),
    tolerance = as.numeric(tolerance),
    detail = as.character(detail),
    stringsAsFactors = FALSE
  )
}

run_dense_ffbs_reference <- function() {
  set.seed(config$seeds$conditional_reference)
  p <- 2L
  T <- 5L
  GG <- array(0, c(p, p, T))
  for (time in seq_len(T)) {
    GG[, , time] <- matrix(
      c(1, 0.15, 0, 0.88), 2L, 2L, byrow = TRUE
    )
  }
  W <- array(rep(diag(c(0.07, 0.025)), T), c(p, p, T))
  m0 <- c(0.2, -0.1)
  C0 <- matrix(c(1, -0.2, -0.2, 0.7), 2L, 2L)
  H <- matrix(stats::rnorm(p * T), p, T)
  V <- stats::runif(T, 0.5, 1.1)
  z <- stats::rnorm(T)
  z[3L] <- NA_real_

  prior_mean <- matrix(0, p, T)
  prior_cov <- matrix(0, p * T, p * T)
  previous_mean <- m0
  previous_cov <- C0
  for (time in seq_len(T)) {
    rows <- ((time - 1L) * p + 1L):(time * p)
    prior_mean[, time] <- drop(GG[, , time] %*% previous_mean)
    prior_cov[rows, rows] <-
      GG[, , time] %*% previous_cov %*% t(GG[, , time]) +
      W[, , time]
    if (time > 1L) {
      prior_rows <- seq_len((time - 1L) * p)
      previous_rows <- ((time - 2L) * p + 1L):((time - 1L) * p)
      previous_block <- prior_cov[
        prior_rows, previous_rows, drop = FALSE
      ]
      prior_cov[prior_rows, rows] <-
        previous_block %*% t(GG[, , time])
      prior_cov[rows, prior_rows] <-
        t(prior_cov[prior_rows, rows])
    }
    previous_mean <- prior_mean[, time]
    previous_cov <- prior_cov[rows, rows]
  }
  observation <- matrix(0, T, p * T)
  for (time in seq_len(T)) {
    observation[
      time, ((time - 1L) * p + 1L):(time * p)
    ] <- H[, time]
  }
  observed <- !is.na(z)
  observation_observed <- observation[observed, , drop = FALSE]
  V_observed <- V[observed]
  prior_precision <- solve(prior_cov)
  post_cov <- solve(
    prior_precision +
      crossprod(observation_observed / sqrt(V_observed))
  )
  post_mean <- drop(post_cov %*% (
    prior_precision %*% as.vector(prior_mean) +
      crossprod(
        observation_observed,
        z[observed] / V_observed
      )
  ))
  evolution <- list(mode = "fixed_W", W = W)
  fit_r <- rqrgibbs::rqr_ffbs_smooth(
    z, H, V, GG, m0, C0, evolution,
    backend = "R", numerical_policy = "fail"
  )
  fit_cpp <- rqrgibbs::rqr_ffbs_smooth(
    z, H, V, GG, m0, C0, evolution,
    backend = "cpp", numerical_policy = "fail"
  )
  max_dense_mean_error <- max(abs(
    as.vector(fit_cpp$smooth_mean) - post_mean
  ))
  max_dense_cov_error <- max(vapply(seq_len(T), function(time) {
    rows <- ((time - 1L) * p + 1L):(time * p)
    max(abs(fit_cpp$smooth_cov[, , time] - post_cov[rows, rows]))
  }, numeric(1L)))
  max_backend_error <- max(
    abs(fit_cpp$smooth_mean - fit_r$smooth_mean),
    abs(fit_cpp$smooth_cov - fit_r$smooth_cov)
  )

  set.seed(config$seeds$ffbs_parity)
  n_path <- 1500L
  sampled <- replicate(
    n_path,
    as.vector(rqrgibbs::rqr_ffbs_sample(
      z, H, V, GG, m0, C0, evolution,
      backend = "cpp", numerical_policy = "fail"
    )$path)
  )
  mean_z <- max(abs(rowMeans(sampled) - post_mean) /
    sqrt(diag(post_cov) / n_path))
  list(
    gates = rbind(
      reference_gate(
        "dense_conditional_mean", max_dense_mean_error <= 1e-10,
        max_dense_mean_error, 1e-10,
        "NA measurement omitted from the dense observation operator"
      ),
      reference_gate(
        "dense_conditional_covariance", max_dense_cov_error <= 1e-10,
        max_dense_cov_error, 1e-10
      ),
      reference_gate(
        "R_cpp_smoother_parity", max_backend_error <= 1e-12,
        max_backend_error, 1e-12
      ),
      reference_gate(
        "cpp_sampled_mean", mean_z <= 5, mean_z, 5,
        "1500 conditional FFBS paths"
      ),
      reference_gate(
        "missing_measurement_omission",
        is.na(fit_cpp$residual[3L]) &&
          is.na(fit_cpp$forecast_variance[3L]),
        detail = "missing observation contributes no measurement update"
      )
    ),
    fixture = list(
      z = z, H = H, V = V, GG = GG, m0 = m0, C0 = C0,
      W = W, post_mean = post_mean, post_cov = post_cov
    )
  )
}

run_future_moment_reference <- function() {
  set.seed(config$seeds$future_state)
  terminal_mean <- c(0.4, -0.2)
  terminal_cov <- matrix(c(0.6, 0.1, 0.1, 0.4), 2L, 2L)
  GG <- matrix(c(1, 1, 0, 1), 2L, 2L, byrow = TRUE)
  W <- diag(c(0.03, 0.01))
  horizon <- 3L
  analytic_mean <- matrix(0, 2L, horizon)
  analytic_cov <- array(0, c(2L, 2L, horizon))
  mean_now <- terminal_mean
  cov_now <- terminal_cov
  for (time in seq_len(horizon)) {
    mean_now <- drop(GG %*% mean_now)
    cov_now <- GG %*% cov_now %*% t(GG) + W
    analytic_mean[, time] <- mean_now
    analytic_cov[, , time] <- cov_now
  }
  n_draw <- 8000L
  initial <- replicate(
    n_draw,
    rqrgibbs:::.rqr_sample_mvnorm_covariance(
      terminal_mean, terminal_cov, numerical_policy = "fail"
    )$draw
  )
  paths <- array(0, c(2L, horizon, n_draw))
  current <- initial
  for (time in seq_len(horizon)) {
    current <- vapply(seq_len(n_draw), function(draw) {
      rqrgibbs:::.rqr_sample_mvnorm_covariance(
        drop(GG %*% current[, draw]), W,
        numerical_policy = "fail"
      )$draw
    }, numeric(2L))
    paths[, time, ] <- current
  }
  mean_z <- max(vapply(seq_len(horizon), function(time) {
    empirical <- rowMeans(paths[, time, , drop = FALSE])
    max(abs(empirical - analytic_mean[, time]) /
      sqrt(diag(analytic_cov[, , time]) / n_draw))
  }, numeric(1L)))
  covariance_z <- max(vapply(seq_len(horizon), function(time) {
    empirical <- stats::cov(t(paths[, time, ]))
    target <- analytic_cov[, , time]
    max(abs(empirical - target)) /
      max(sqrt(diag(target)^2 / (n_draw - 1L)))
  }, numeric(1L)))
  rbind(
    reference_gate(
      "analytic_future_state_mean", mean_z <= 5, mean_z, 5,
      "unconditional Gaussian root-state propagation"
    ),
    reference_gate(
      "analytic_future_state_covariance",
      covariance_z <= 6, covariance_z, 6
    )
  )
}

run_component_scale_reference <- function() {
  evolution <- rqrgibbs::rqr_evolution_component_scale(
    templates = list(matrix(2, 1L, 1L)),
    component_dims = 1L,
    prior = list(shape = 3, rate = 4),
    initial = 0.5,
    component_names = "level"
  )
  theta1 <- matrix(c(1, 2, 4), 1L, 3L)
  theta2 <- matrix(c(-1, 0, 1), 1L, 3L)
  posterior <- rqrgibbs:::.rqr_component_scale_posterior(
    theta1, theta2, theta01 = 0, theta02 = 0,
    GG = 1, evolution = evolution
  )
  innovations <- c(1, 1, 2, -1, 1, 1)
  expected_shape <- 6
  expected_rate <- 4 + 0.5 * sum(innovations^2 / 2)
  rbind(
    reference_gate(
      "component_scale_inverse_gamma_shape",
      identical(posterior$shape, expected_shape),
      posterior$shape, expected_shape
    ),
    reference_gate(
      "component_scale_inverse_gamma_rate",
      abs(posterior$rate - expected_rate) <= 1e-14,
      abs(posterior$rate - expected_rate), 1e-14
    )
  )
}

provenance_control <- list(
  repo_root = repo_root,
  expected_git_commit = expected_commit,
  primary_runtime_attestation = primary_attestation
)

run_continuation_reference <- function() {
  fixture <- constructed$fixed_W_local_level
  arguments <- rqr_bounded_fit_arguments(
    fixture, config, "learned_pseudoresidual_normalized", 1L,
    provenance_control, n_burn = 0L, n_mcmc = 6L
  )
  arguments$mcmc_control$seed <- config$seeds$continuation
  full <- do.call(rqrgibbs::rqr_dlm_fit, arguments)
  arguments$mcmc_control$n_mcmc <- 2L
  first <- do.call(rqrgibbs::rqr_dlm_fit, arguments)
  second <- rqrgibbs::rqr_dlm_continue(first, n_mcmc = 2L)
  third <- rqrgibbs::rqr_dlm_continue(second, n_mcmc = 2L)
  bind_columns <- function(field) {
    do.call(cbind, lapply(list(first, second, third), `[[`, field))
  }
  draw_fields <- c(
    "samp.eta_root1", "samp.eta_root2",
    "samp.theta_terminal_root1", "samp.theta_terminal_root2"
  )
  bitwise_draws <- all(vapply(draw_fields, function(field) {
    identical(full[[field]], bind_columns(field))
  }, logical(1L))) &&
    identical(
      full$samp.lambda,
      c(first$samp.lambda, second$samp.lambda, third$samp.lambda)
    )
  bitwise_checkpoint <- identical(
    full$checkpoint_state, third$checkpoint_state
  ) && identical(full$checkpoint_digest, third$checkpoint_digest)
  history_shape <- identical(
    third$continuation_history_contract$generation, 2L
  ) && length(third$continuation_history_contract$segments) == 3L

  rehash <- function(object) {
    object$continuation_history_digest <-
      rqrgibbs:::.rqr_digest(object$continuation_history_contract)
    object
  }
  invalid_history <- function(object) {
    inherits(
      try(
        rqrgibbs:::.rqr_validate_continuation_history(object),
        silent = TRUE
      ),
      "try-error"
    )
  }
  mutations <- list(
    generation0_repair_exactness = function(x) {
      x$continuation_history_contract$segments[[1L]]$
        segment_numerical_repair_count <- 1L
      rehash(x)
    },
    generation1_target_status = function(x) {
      x$continuation_history_contract$segments[[2L]]$
        segment_target_numerical_eligible <- FALSE
      rehash(x)
    },
    generation0_mismatch_without_override = function(x) {
      x$continuation_history_contract$segments[[1L]]$
        environment_mismatches <- "package_version"
      rehash(x)
    },
    generation1_backend_without_mismatch = function(x) {
      x$continuation_history_contract$segments[[2L]]$
        backend_resolved <- "R"
      x$continuation_history_contract$segments[[3L]]$
        parent_backend_resolved <- "R"
      rehash(x)
    },
    generation0_target_digest = function(x) {
      x$continuation_history_contract$segments[[1L]]$
        segment_target_contract_digest <- paste(rep("0", 64L), collapse = "")
      rehash(x)
    }
  )
  mutation_rejected <- vapply(
    mutations,
    function(mutation) invalid_history(mutation(third)),
    logical(1L)
  )
  mutation_table <- data.frame(
    mutation = names(mutation_rejected),
    rejected = unname(mutation_rejected),
    stringsAsFactors = FALSE
  )
  utils::write.csv(
    mutation_table,
    file.path(output_dir, "continuation_history_mutations.csv"),
    row.names = FALSE
  )
  saveRDS(
    list(
      full_checkpoint_digest = full$checkpoint_digest,
      segmented_checkpoint_digest = third$checkpoint_digest,
      continuation_history_digest =
        third$continuation_history_digest,
      generation = third$continuation_history_contract$generation
    ),
    file.path(output_dir, "continuation_reference_digests.rds"),
    version = 3
  )
  rbind(
    reference_gate(
      "six_vs_2_plus_2_plus_2_draws", bitwise_draws,
      detail = "three history segments with generation indices 0, 1, 2"
    ),
    reference_gate(
      "six_vs_2_plus_2_plus_2_checkpoint", bitwise_checkpoint
    ),
    reference_gate(
      "three_segment_history_shape", history_shape
    ),
    reference_gate(
      "rehashed_early_history_mutations",
      all(mutation_rejected),
      sum(mutation_rejected), length(mutation_rejected)
    )
  )
}

if (identical(mode, "reference-only")) {
  dense <- run_dense_ffbs_reference()
  gates <- rbind(
    dense$gates,
    run_future_moment_reference(),
    run_component_scale_reference(),
    run_continuation_reference(),
    reference_gate(
      "active_process_tree_monitor",
      isTRUE(base_manifest$process_tree_monitor_active),
      detail = "runner must be launched through the monitored shell wrapper"
    )
  )
  utils::write.csv(
    gates, file.path(output_dir, "reference_gates.csv"),
    row.names = FALSE
  )
  pass <- all(gates$pass)
  write_manifest(list(
    status = if (pass) "passed" else "failed",
    fixture_construction_passed = TRUE,
    reference_gates_executed = TRUE,
    reference_gate_count = nrow(gates),
    reference_gate_pass_count = sum(gates$pass),
    bounded_dynamic_execution_authorized = FALSE
  ))
  if (!pass) {
    stop(
      "Reference-only gates failed: ",
      paste(gates$gate[!gates$pass], collapse = ", "),
      call. = FALSE
    )
  }
  cat("Bounded RQR-DLM reference-only validation passed.\n")
  cat("  gates:", nrow(gates), "\n")
  cat("  bounded 24-fit grid executed: 0\n")
  quit(save = "no", status = 0L)
}

confirmation <- Sys.getenv(
  "RQR_CONFIRM_BOUNDED_DYNAMIC_EXECUTION", unset = ""
)
reviewed_commit <- tolower(Sys.getenv(
  "RQR_REVIEWED_BOUNDED_RUNNER_COMMIT", unset = ""
))
reference_manifest_path <- Sys.getenv(
  "RQR_REVIEWED_REFERENCE_MANIFEST", unset = ""
)
reference_manifest_sha <- tolower(Sys.getenv(
  "RQR_REVIEWED_REFERENCE_MANIFEST_SHA256", unset = ""
))
reference_resource_path <- Sys.getenv(
  "RQR_REVIEWED_REFERENCE_RESOURCE_SUMMARY", unset = ""
)
reference_resource_sha <- tolower(Sys.getenv(
  "RQR_REVIEWED_REFERENCE_RESOURCE_SUMMARY_SHA256", unset = ""
))
reference_manifest_verified <-
  nzchar(reference_manifest_path) &&
  file.exists(reference_manifest_path) &&
  grepl("^[0-9a-f]{64}$", reference_manifest_sha) &&
  identical(
    digest::digest(
      file = reference_manifest_path,
      algo = "sha256", serialize = FALSE
    ),
    reference_manifest_sha
  )
reference_manifest <- if (reference_manifest_verified) {
  tryCatch(
    jsonlite::read_json(reference_manifest_path, simplifyVector = TRUE),
    error = function(e) NULL
  )
} else {
  NULL
}
reference_manifest_verified <- reference_manifest_verified &&
  is.list(reference_manifest) &&
  identical(reference_manifest$mode, "reference-only") &&
  identical(reference_manifest$status, "passed") &&
  identical(
    tolower(reference_manifest$primary_commit), expected_commit
  ) &&
  identical(reference_manifest$config_digest, base_manifest$config_digest) &&
  isTRUE(reference_manifest$process_tree_monitor_active)
reference_resource_verified <-
  reference_manifest_verified &&
  nzchar(reference_resource_path) &&
  file.exists(reference_resource_path) &&
  grepl("^[0-9a-f]{64}$", reference_resource_sha) &&
  identical(
    digest::digest(
      file = reference_resource_path,
      algo = "sha256", serialize = FALSE
    ),
    reference_resource_sha
  ) &&
  identical(
    normalizePath(
      dirname(reference_resource_path), winslash = "/", mustWork = TRUE
    ),
    normalizePath(
      dirname(reference_manifest_path), winslash = "/", mustWork = TRUE
    )
  )
reference_resource <- if (reference_resource_verified) {
  tryCatch(
    utils::read.csv(
      reference_resource_path, stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    error = function(e) NULL
  )
} else {
  NULL
}
reference_resource_verified <- reference_resource_verified &&
  is.data.frame(reference_resource) &&
  identical(
    names(reference_resource), c("metric", "value", "limit", "pass")
  ) &&
  nrow(reference_resource) == 6L &&
  setequal(
    reference_resource$metric,
    c(
      "process_tree_peak_processes", "process_tree_peak_threads",
      "process_tree_peak_rss_kib", "hard_timeout_triggered",
      "resource_limit_triggered", "runner_exit_status"
    )
  ) &&
  all(toupper(as.character(reference_resource$pass)) == "TRUE")
if (!isTRUE(config$bounded_dynamic_execution_authorized) ||
    !identical(
      confirmation, "I_CONFIRM_24_BOUNDED_RQR_DLM_FITS"
    ) ||
    !identical(reviewed_commit, expected_commit) ||
    !reference_manifest_verified ||
    !reference_resource_verified ||
    !isTRUE(base_manifest$process_tree_monitor_active)) {
  write_manifest(list(
    status = "blocked_by_execution_contract",
    fixture_construction_passed = TRUE,
    reference_gates_executed = FALSE,
    bounded_dynamic_execution_authorized = FALSE
  ))
  stop(
    paste(
      "execute-bounded is disabled. It requires a reviewed commit, the",
      "exact confirmation phrase, a hash-verified passing reference-only",
      "manifest and monitor summary from the same run directory, an active",
      "process-tree monitor, and",
      "bounded_dynamic_execution_authorized=TRUE in the reviewed config."
    ),
    call. = FALSE
  )
}

chain_estimands <- function(fit) {
  lower <- pmin(fit$samp.eta_root1, fit$samp.eta_root2)
  upper <- pmax(fit$samp.eta_root1, fit$samp.eta_root2)
  observed <- !is.na(fit$y)
  alpha <- fit$model_spec$coverage_level
  loss <- vapply(seq_len(ncol(lower)), function(draw) {
    sum(rqrgibbs::rqr_check_loss(
      rqrgibbs::rqr_residual_product(
        fit$y[observed],
        fit$samp.eta_root1[observed, draw],
        fit$samp.eta_root2[observed, draw]
      ),
      alpha
    ))
  }, numeric(1L))
  terminal_midpoint <- 0.5 * (
    fit$samp.theta_terminal_root1 +
      fit$samp.theta_terminal_root2
  )
  terminal_difference <- abs(
    fit$samp.theta_terminal_root1 -
      fit$samp.theta_terminal_root2
  )
  values <- cbind(
    mean_lower = colMeans(lower),
    mean_upper = colMeans(upper),
    mean_width = colMeans(upper - lower),
    mean_midpoint = colMeans(0.5 * (lower + upper)),
    observed_loss = loss,
    t(terminal_midpoint),
    t(terminal_difference)
  )
  midpoint_columns <- seq_len(nrow(terminal_midpoint)) + 5L
  difference_columns <- seq_len(nrow(terminal_difference)) +
    5L + nrow(terminal_midpoint)
  colnames(values)[midpoint_columns] <- paste0(
    "terminal_midpoint_", seq_len(nrow(terminal_midpoint))
  )
  colnames(values)[difference_columns] <- paste0(
    "terminal_abs_difference_", seq_len(nrow(terminal_difference))
  )
  if (fit$model_spec$learning_rate_mode == "fixed_rate") {
    expected <- fit$model_spec$fixed_learning_rate *
      fit$model_spec$loss_reference_scale
    if (!all(fit$samp.lambda == expected)) {
      stop("Fixed-rate lambda failed its exact-identity gate.", call. = FALSE)
    }
  } else {
    values <- cbind(values, log_lambda = log(fit$samp.lambda))
  }
  if (!is.null(fit$samp.evolution_scale) &&
      ncol(fit$samp.evolution_scale) > 0L) {
    q_values <- log(fit$samp.evolution_scale)
    colnames(q_values) <- paste0(
      "log_component_scale_", seq_len(ncol(q_values))
    )
    values <- cbind(values, q_values)
  }
  values
}

fit_grid <- expand.grid(
  fixture_id = names(constructed),
  learning_rate_mode = config$learning_rate_modes,
  chain = seq_len(config$mcmc$chains),
  stringsAsFactors = FALSE
)
fit_grid$fit_id <- sprintf(
  "%s__%s__chain%02d",
  fit_grid$fixture_id, fit_grid$learning_rate_mode, fit_grid$chain
)
chains <- vector("list", nrow(fit_grid))
fit_audit <- vector("list", nrow(fit_grid))
fit_root <- file.path(output_dir, "full_chains_ignored")
dir.create(fit_root, recursive = TRUE)
for (index in seq_len(nrow(fit_grid))) {
  row <- fit_grid[index, ]
  fit_arguments <- rqr_bounded_fit_arguments(
    constructed[[row$fixture_id]], config,
    row$learning_rate_mode, row$chain,
    provenance_control
  )
  fit <- do.call(rqrgibbs::rqr_dlm_fit, fit_arguments)
  if (!isTRUE(fit$model_spec$exact_joint_target) ||
      !isTRUE(fit$model_spec$target_numerical_eligible) ||
      fit$model_spec$numerical_repair_count != 0L ||
      !isTRUE(fit$provenance$primary_runtime_source_match) ||
      !isTRUE(fit$provenance$reproducibility_eligible)) {
    stop("A bounded fit failed its target or provenance gate.", call. = FALSE)
  }
  future <- constructed[[row$fixture_id]]$future
  mode_index <- match(
    row$learning_rate_mode, config$learning_rate_modes
  )
  forecast_seed <- unname(
    config$seeds$forecast_by_fixture[[row$fixture_id]]
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
      nrow(forecast$lower_draws) != future$H ||
      any(!is.finite(forecast$lower_draws)) ||
      any(!is.finite(forecast$upper_draws)) ||
      !grepl("no response simulation", forecast$interpretation)) {
    stop("A bounded future root-state gate failed.", call. = FALSE)
  }
  chains[[index]] <- chain_estimands(fit)
  saveRDS(
    fit, file.path(fit_root, paste0(row$fit_id, ".rds")),
    version = 3, compress = "xz"
  )
  fit_audit[[index]] <- data.frame(
    fit_id = row$fit_id,
    fixture_id = row$fixture_id,
    learning_rate_mode = row$learning_rate_mode,
    chain = row$chain,
    seed = config$mcmc$seeds[row$chain],
    forecast_seed = forecast_seed,
    forecast_repair_count = forecast$diagnostics$repair_count,
    forecast_horizon = future$H,
    numerical_repair_count = fit$model_spec$numerical_repair_count,
    exact_joint_target = fit$model_spec$exact_joint_target,
    target_numerical_eligible =
      fit$model_spec$target_numerical_eligible,
    reproducibility_eligible =
      fit$provenance$reproducibility_eligible,
    promotion_eligible = fit$model_spec$promotion_eligible,
    checkpoint_digest = fit$checkpoint_digest,
    history_digest = fit$continuation_history_digest,
    root_swap_count = sum(fit$diagnostics$root_swap_trace),
    stringsAsFactors = FALSE
  )
}
fit_audit <- do.call(rbind, fit_audit)
utils::write.csv(
  fit_audit, file.path(output_dir, "fit_audit.csv"),
  row.names = FALSE
)

diagnostics <- list()
for (fixture_id in names(constructed)) {
  for (learning_rate_mode in config$learning_rate_modes) {
    rows <- fit_grid$fixture_id == fixture_id &
      fit_grid$learning_rate_mode == learning_rate_mode
    selected <- chains[rows]
    variables <- Reduce(intersect, lapply(selected, colnames))
    for (variable in variables) {
      matrix_values <- do.call(cbind, lapply(
        selected, function(values) values[, variable]
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
      diagnostics[[length(diagnostics) + 1L]] <- data.frame(
        fixture_id = fixture_id,
        learning_rate_mode = learning_rate_mode,
        estimand = variable,
        rhat = unname(posterior::rhat(draws)),
        ess_bulk = unname(posterior::ess_bulk(draws)),
        ess_tail = unname(posterior::ess_tail(draws)),
        mcse_mean = unname(posterior::mcse_mean(draws)),
        stringsAsFactors = FALSE
      )
    }
  }
}
diagnostics <- do.call(rbind, diagnostics)
diagnostics$pass <- with(
  diagnostics,
  is.finite(rhat) & is.finite(ess_bulk) & is.finite(ess_tail) &
    is.finite(mcse_mean) &
    rhat <= config$gates$maximum_rank_normalized_rhat &
    ess_bulk >= config$gates$minimum_bulk_ess &
    ess_tail >= config$gates$minimum_tail_ess
)
utils::write.csv(
  diagnostics, file.path(output_dir, "chain_diagnostics.csv"),
  row.names = FALSE
)
all_pass <- all(diagnostics$pass) &&
  all(fit_audit$numerical_repair_count == 0L) &&
  all(fit_audit$exact_joint_target) &&
  all(fit_audit$target_numerical_eligible) &&
  all(fit_audit$reproducibility_eligible)
write_manifest(list(
  status = if (all_pass) "passed" else "failed",
  fixture_construction_passed = TRUE,
  reference_gates_executed = FALSE,
  bounded_dynamic_execution_authorized = TRUE,
  bounded_fit_count = nrow(fit_audit),
  diagnostic_count = nrow(diagnostics),
  diagnostic_pass_count = sum(diagnostics$pass)
))
if (!all_pass) {
  stop("The bounded dynamic fit gates did not all pass.", call. = FALSE)
}
cat("Bounded RQR-DLM dynamic execution passed.\n")
cat("  fits:", nrow(fit_audit), "\n")
cat("  this was not the matched or production simulation\n")
