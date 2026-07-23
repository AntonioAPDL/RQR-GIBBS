#!/usr/bin/env Rscript

# Four-mode bounded RQR-DLM validation runner.
#
# preflight       construct and hash all canonical objects;
# reference-only  execute deterministic and small Monte Carlo reference gates;
# benchmark-one-cell
#                 execute one full four-chain representative cell only;
# execute-bounded run the frozen 3 x 2 x 4 fits only after a separately reviewed
#                 source commit and explicit dual authorization.
#
# This is target and computation validation for interval-root generalized
# Bayes. It does not simulate posterior-predictive responses and is not the
# matched simulation study.

arguments <- commandArgs(trailingOnly = TRUE)
mode <- if (length(arguments)) arguments[1L] else "preflight"
allowed_modes <- c(
  "preflight", "reference-only", "benchmark-one-cell",
  "execute-bounded"
)
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
      "rqrgibbs_dlm_bounded_fixtures/4.0.0"
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
    1024^2 *
      config$resources$maximum_sampled_process_group_rss_gib,
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
      ) ||
      !identical(
        Sys.getenv("RQR_PROCESS_MONITOR_KIND", unset = ""),
        config$resources$monitor_kind
      ) ||
      !identical(
        Sys.getenv(
          "RQR_MONITOR_KERNEL_HARD_MEMORY", unset = ""
        ),
        "FALSE"
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

atomic_path <- function(path) {
  tempfile(
    paste0(".", basename(path), "-"),
    tmpdir = dirname(path)
  )
}
atomic_write_csv <- function(value, path) {
  temporary <- atomic_path(path)
  on.exit(unlink(temporary), add = TRUE)
  utils::write.csv(value, temporary, row.names = FALSE)
  if (!file.rename(temporary, path)) {
    stop("Could not atomically publish ", basename(path), ".", call. = FALSE)
  }
  invisible(path)
}
atomic_write_json <- function(value, path) {
  temporary <- atomic_path(path)
  on.exit(unlink(temporary), add = TRUE)
  jsonlite::write_json(
    value, temporary, auto_unbox = TRUE, pretty = TRUE,
    digits = NA, null = "null"
  )
  if (!file.rename(temporary, path)) {
    stop("Could not atomically publish ", basename(path), ".", call. = FALSE)
  }
  invisible(path)
}
atomic_save_rds <- function(value, path, compress = TRUE) {
  temporary <- atomic_path(path)
  on.exit(unlink(temporary), add = TRUE)
  saveRDS(value, temporary, version = 3, compress = compress)
  if (!file.rename(temporary, path)) {
    stop("Could not atomically publish ", basename(path), ".", call. = FALSE)
  }
  invisible(path)
}
atomic_write_lines <- function(value, path) {
  temporary <- atomic_path(path)
  on.exit(unlink(temporary), add = TRUE)
  writeLines(value, temporary, useBytes = TRUE)
  if (!file.rename(temporary, path)) {
    stop("Could not atomically publish ", basename(path), ".", call. = FALSE)
  }
  invisible(path)
}

failure_columns <- c(
  "recorded_at", "mode", "stage", "fixture_id",
  "learning_rate_mode", "chain", "message"
)
failure_log <- as.data.frame(
  setNames(replicate(
    length(failure_columns), character(0), simplify = FALSE
  ), failure_columns),
  stringsAsFactors = FALSE
)
failure_path <- file.path(output_dir, "failure_log.csv")
atomic_write_csv(failure_log, failure_path)
record_failure <- function(
    stage, message, fixture_id = NA_character_,
    learning_rate_mode = NA_character_, chain = NA_integer_) {
  failure_log <<- rbind(failure_log, data.frame(
    recorded_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
    mode = mode,
    stage = as.character(stage),
    fixture_id = as.character(fixture_id),
    learning_rate_mode = as.character(learning_rate_mode),
    chain = as.character(chain),
    message = conditionMessage(message),
    stringsAsFactors = FALSE
  ))
  atomic_write_csv(failure_log, failure_path)
}

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
atomic_write_csv(
  construction, file.path(output_dir, "fixture_construction.csv")
)

dependency_versions <- vapply(
  required_packages,
  function(package) as.character(utils::packageVersion(package)),
  character(1L)
)
external_software <- extSoftVersion()
session_runtime <- utils::sessionInfo()
external_or_session <- function(name, fallback) {
  if (name %in% names(external_software)) {
    return(unname(external_software[[name]]))
  }
  unname(fallback)
}
runtime_contract <- list(
  schema_version = "rqrgibbs_runtime_toolchain/1.0.0",
  R_version = R.version.string,
  platform = R.version$platform,
  compiler = R.version$compiler,
  BLAS = external_or_session("BLAS", session_runtime$BLAS),
  LAPACK = external_or_session("LAPACK", session_runtime$LAPACK),
  dependency_versions = as.list(dependency_versions),
  primary_runtime_tree_digest =
    runtime_state$runtime_package_tree_digest,
  primary_runtime_attestation_sha256 = digest::digest(
    file = primary_attestation, algo = "sha256", serialize = FALSE
  )
)
runtime_contract$digest <- digest::digest(
  runtime_contract, algo = "sha256", serialize = TRUE
)
atomic_write_json(
  runtime_contract, file.path(output_dir, "runtime_toolchain.json")
)
session_capture <- capture.output(utils::sessionInfo())
atomic_write_lines(
  session_capture, file.path(output_dir, "session_info.txt")
)

base_manifest <- list(
  schema_version = "rqrgibbs_dlm_bounded_run/2.0.0",
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
  primary_runtime_attestation_sha256 =
    runtime_contract$primary_runtime_attestation_sha256,
  runtime_toolchain_digest = runtime_contract$digest,
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
  process_tree_monitor_kind = Sys.getenv(
    "RQR_PROCESS_MONITOR_KIND", unset = "none"
  ),
  kernel_hard_memory_ceiling = identical(
    Sys.getenv("RQR_MONITOR_KERNEL_HARD_MEMORY", unset = "FALSE"),
    "TRUE"
  ),
  recorded_at = format(Sys.time(), tz = "UTC", usetz = TRUE)
)

write_manifest <- function(values) {
  atomic_write_json(
    utils::modifyList(base_manifest, values),
    file.path(output_dir, "run_manifest.json")
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
  sampled_cov <- stats::cov(t(sampled))
  covariance_se <- sqrt((
    post_cov^2 + outer(diag(post_cov), diag(post_cov))
  ) / (n_path - 1L))
  covariance_z <- abs(sampled_cov - post_cov) / covariance_se
  adjacent_pairs <- unlist(lapply(seq_len(T - 1L), function(time) {
    first <- ((time - 1L) * p + 1L):(time * p)
    second <- (time * p + 1L):((time + 1L) * p)
    as.vector(outer(first, second, function(x, y) {
      x + (y - 1L) * nrow(covariance_z)
    }))
  }))
  max_full_covariance_z <- max(covariance_z)
  max_adjacent_covariance_z <- max(covariance_z[adjacent_pairs])
  atomic_save_rds(
    list(
      posterior_mean = post_mean,
      posterior_covariance = post_cov,
      sampled_mean = rowMeans(sampled),
      sampled_covariance = sampled_cov
    ),
    file.path(output_dir, "dense_ffbs_reference.rds")
  )
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
        "cpp_sampled_full_cross_time_covariance",
        max_full_covariance_z <= 6,
        max_full_covariance_z, 6,
        paste(
          "Gaussian sample-covariance standard errors;",
          "all state-time pairs"
        )
      ),
      reference_gate(
        "cpp_sampled_adjacent_time_covariance",
        max_adjacent_covariance_z <= 6,
        max_adjacent_covariance_z, 6,
        "selected adjacent-time state covariance blocks"
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

run_canonical_missing_references <- function() {
  rows <- lapply(constructed, function(fixture) {
    missing <- which(!fixture$observed)
    if (!length(missing)) return(NULL)
    expanded <- fixture$expanded_model
    T <- length(fixture$y)
    p <- expanded$p
    W <- if (!is.null(fixture$evolution$W)) {
      fixture$evolution$W
    } else {
      rqrgibbs:::.rqr_materialize_component_evolution(
        fixture$evolution, fixture$evolution$initial, T, p
      )$W
    }
    evolution <- list(mode = "fixed_W", W = W)
    H <- expanded$FF
    H_alternative <- H
    H_alternative[, missing] <- H_alternative[, missing, drop = FALSE] +
      matrix(
        seq_len(p * length(missing)) * 1e6,
        p, length(missing)
      )
    z <- fixture$y
    V <- rep(1, T)
    baseline <- rqrgibbs::rqr_ffbs_smooth(
      z, H, V, expanded$GG, expanded$m0, expanded$C0,
      evolution, backend = "cpp", numerical_policy = "fail"
    )
    alternative <- rqrgibbs::rqr_ffbs_smooth(
      z, H_alternative, V, expanded$GG, expanded$m0,
      expanded$C0, evolution, backend = "cpp",
      numerical_policy = "fail"
    )
    data.frame(
      fixture_id = fixture$fixture_id,
      expected_missing_indices = paste(missing, collapse = ","),
      detected_missing_indices = paste(
        which(is.na(baseline$residual)), collapse = ","
      ),
      maximum_placeholder_invariance_error = max(
        abs(baseline$smooth_mean - alternative$smooth_mean),
        abs(baseline$smooth_cov - alternative$smooth_cov)
      ),
      pass = identical(which(is.na(baseline$residual)), missing) &&
        identical(which(is.na(baseline$forecast_variance)), missing) &&
        identical(baseline$smooth_mean, alternative$smooth_mean) &&
        identical(baseline$smooth_cov, alternative$smooth_cov),
      stringsAsFactors = FALSE
    )
  })
  table <- do.call(rbind, Filter(Negate(is.null), rows))
  atomic_write_csv(
    table, file.path(output_dir, "canonical_missing_checks.csv")
  )
  do.call(rbind, lapply(seq_len(nrow(table)), function(index) {
    reference_gate(
      paste0(
        "canonical_missing_placeholder_invariance__",
        table$fixture_id[index]
      ),
      table$pass[index],
      table$maximum_placeholder_invariance_error[index],
      0,
      paste("missing indices", table$expected_missing_indices[index])
    )
  }))
}

run_public_future_references <- function() {
  n_draw <- 4000L
  rows <- lapply(seq_along(constructed), function(fixture_index) {
    fixture <- constructed[[fixture_index]]
    p <- fixture$expanded_model$p
    H <- fixture$future$H
    terminal <- seq_len(p) / 10
    terminal_draws <- matrix(terminal, p, n_draw)
    component_mode <- identical(
      fixture$evolution$mode, "component_scale"
    )
    fake_fit <- structure(list(
      samp.theta_terminal_root1 = terminal_draws,
      samp.theta_terminal_root2 = terminal_draws,
      samp.evolution_scale = if (component_mode) {
        matrix(
          rep(fixture$evolution$initial, each = n_draw),
          n_draw, length(fixture$evolution$initial)
        )
      } else {
        NULL
      },
      evolution = fixture$evolution,
      model_spec = list(
        evolution_mode = fixture$evolution$mode,
        numerical_policy = "fail"
      ),
      misc = list(jitter_ladder = 0)
    ), class = c("rqr_dlm_mcmc", "rqr_fit"))
    forecast_arguments <- list(
      object = fake_fit,
      FF_future = fixture$future$FF,
      GG_future = fixture$future$GG,
      nd = NULL,
      seed = unname(
        config$seeds$forecast_by_fixture[[fixture$fixture_id]]
      ),
      numerical_policy = "fail",
      jitter_ladder = 0
    )
    W <- if (component_mode) {
      forecast_arguments$component_templates_future <-
        fixture$future$component_templates
      future_evolution <- rqrgibbs::rqr_evolution_component_scale(
        templates = fixture$future$component_templates,
        component_dims = fixture$evolution$component_dims,
        prior = fixture$evolution$prior,
        initial = fixture$evolution$initial,
        component_names = fixture$evolution$component_names
      )
      rqrgibbs:::.rqr_materialize_component_evolution(
        future_evolution, fixture$evolution$initial, H, p
      )$W
    } else {
      forecast_arguments$W_future <- fixture$future$W
      fixture$future$W
    }
    forecast <- do.call(
      rqrgibbs::rqr_forecast_roots, forecast_arguments
    )
    GG <- rqrgibbs:::.rqr_expand_cube(
      fixture$future$GG, H, p, "GG_future"
    )
    state_mean <- terminal
    state_covariance <- matrix(0, p, p)
    eta_mean <- eta_variance <- numeric(H)
    for (time in seq_len(H)) {
      state_mean <- drop(GG[, , time] %*% state_mean)
      state_covariance <-
        GG[, , time] %*% state_covariance %*%
          t(GG[, , time]) + W[, , time]
      direction <- fixture$future$FF[, time]
      eta_mean[time] <- drop(crossprod(direction, state_mean))
      eta_variance[time] <- drop(
        crossprod(direction, state_covariance %*% direction)
      )
    }
    empirical_mean <- rowMeans(forecast$eta_root1)
    empirical_variance <- apply(
      forecast$eta_root1, 1L, stats::var
    )
    mean_z <- max(
      abs(empirical_mean - eta_mean) /
        sqrt(eta_variance / n_draw)
    )
    variance_z <- max(
      abs(empirical_variance - eta_variance) /
        sqrt(2 * eta_variance^2 / (n_draw - 1L))
    )
    data.frame(
      fixture_id = fixture$fixture_id,
      mean_standardized_error = mean_z,
      variance_standardized_error = variance_z,
      repair_count = forecast$diagnostics$repair_count,
      interpretation_pass = grepl(
        "no response simulation", forecast$interpretation
      ),
      pass = mean_z <= 5 && variance_z <= 6 &&
        forecast$diagnostics$repair_count == 0L &&
        grepl("no response simulation", forecast$interpretation),
      stringsAsFactors = FALSE
    )
  })
  table <- do.call(rbind, rows)
  atomic_write_csv(
    table, file.path(output_dir, "public_future_root_checks.csv")
  )
  do.call(rbind, lapply(seq_len(nrow(table)), function(index) {
    rbind(
      reference_gate(
        paste0(
          "public_future_mean__", table$fixture_id[index]
        ),
        table$mean_standardized_error[index] <= 5,
        table$mean_standardized_error[index], 5
      ),
      reference_gate(
        paste0(
          "public_future_variance__", table$fixture_id[index]
        ),
        table$variance_standardized_error[index] <= 6,
        table$variance_standardized_error[index], 6,
        "exact Gaussian sample-variance standard error"
      )
    )
  }))
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
  scalar_gates <- rbind(
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
  fixture <- constructed$shared_component_scale_trend_regression
  arguments <- rqr_bounded_fit_arguments(
    fixture, config, "learned_pseudoresidual_normalized", 1L,
    provenance_control, n_burn = 0L, n_mcmc = 3L
  )
  arguments$mcmc_control$seed <- config$seeds$component_scale
  fit <- do.call(rqrgibbs::rqr_dlm_fit, arguments)
  recomputed <- lapply(seq_len(nrow(fit$samp.evolution_scale)), function(draw) {
    rqrgibbs:::.rqr_component_scale_posterior(
      fit$samp.theta_root1[, , draw],
      fit$samp.theta_root2[, , draw],
      fit$samp.theta0_root1[, draw],
      fit$samp.theta0_root2[, draw],
      fit$expanded_model$GG,
      fit$evolution
    )
  })
  recomputed_shape <- do.call(rbind, lapply(
    recomputed, `[[`, "shape"
  ))
  recomputed_rate <- do.call(rbind, lapply(
    recomputed, `[[`, "rate"
  ))
  shape_error <- max(abs(
    fit$samp.evolution_scale_shape - recomputed_shape
  ))
  rate_error <- max(abs(
    fit$samp.evolution_scale_rate - recomputed_rate
  ))
  orientation_pass <-
    identical(
      dim(fit$samp.evolution_scale),
      c(3L, length(fit$evolution$component_dims))
    ) &&
    identical(
      dim(fit$samp.evolution_scale_shape),
      dim(fit$samp.evolution_scale)
    ) &&
    identical(
      dim(fit$samp.evolution_scale_rate),
      dim(fit$samp.evolution_scale)
    ) &&
    identical(
      colnames(fit$samp.evolution_scale),
      fit$evolution$component_names
    )
  atomic_write_csv(
    data.frame(
      draw = rep(
        seq_len(nrow(recomputed_shape)),
        each = ncol(recomputed_shape)
      ),
      component = rep(
        fit$evolution$component_names,
        times = nrow(recomputed_shape)
      ),
      saved_shape = as.vector(t(
        fit$samp.evolution_scale_shape
      )),
      recomputed_shape = as.vector(t(recomputed_shape)),
      saved_rate = as.vector(t(fit$samp.evolution_scale_rate)),
      recomputed_rate = as.vector(t(recomputed_rate)),
      stringsAsFactors = FALSE
    ),
    file.path(output_dir, "component_scale_conditionals.csv")
  )
  rbind(
    scalar_gates,
    reference_gate(
      "canonical_component_scale_shape", shape_error <= 1e-14,
      shape_error, 1e-14, "two components with dimensions 2 and 1"
    ),
    reference_gate(
      "canonical_component_scale_rate", rate_error <= 1e-12,
      rate_error, 1e-12,
      "retained-iteration conditionals recomputed from saved paths"
    ),
    reference_gate(
      "canonical_component_scale_orientation", orientation_pass,
      detail = "retained draws by named component"
    )
  )
}

provenance_control <- list(
  repo_root = repo_root,
  expected_git_commit = expected_commit,
  primary_runtime_attestation = primary_attestation
)

run_continuation_reference <- function() {
  column_fields <- c(
    "samp.eta_root1", "samp.eta_root2",
    "samp.theta_terminal_root1", "samp.theta_terminal_root2",
    "samp.theta0_root1", "samp.theta0_root2"
  )
  row_fields <- c(
    "samp.evolution_scale", "samp.evolution_scale_shape",
    "samp.evolution_scale_rate"
  )
  array_fields <- c("samp.theta_root1", "samp.theta_root2")
  vector_fields <- "samp.lambda"
  bind_field <- function(segments, field, margin) {
    values <- lapply(segments, `[[`, field)
    if (all(vapply(values, is.null, logical(1L)))) return(NULL)
    if (any(vapply(values, is.null, logical(1L)))) return(structure(
      NA, mismatch = TRUE
    ))
    if (identical(margin, "columns")) return(do.call(cbind, values))
    if (identical(margin, "rows")) return(do.call(rbind, values))
    if (identical(margin, "vector")) return(do.call(c, values))
    dimensions <- dim(values[[1L]])
    array(
      do.call(c, values),
      dim = c(
        dimensions[1:2],
        sum(vapply(values, function(value) dim(value)[3L], integer(1L)))
      )
    )
  }
  cell_rows <- list()
  gates <- list()
  last_segmented <- NULL
  cell_index <- 0L
  for (fixture_id in names(constructed)) {
    for (learning_rate_mode in config$learning_rate_modes) {
      cell_index <- cell_index + 1L
      fixture <- constructed[[fixture_id]]
      arguments <- rqr_bounded_fit_arguments(
        fixture, config, learning_rate_mode, 1L,
        provenance_control, n_burn = 0L, n_mcmc = 6L
      )
      arguments$mcmc_control$seed <-
        config$seeds$continuation + cell_index - 1L
      full <- do.call(rqrgibbs::rqr_dlm_fit, arguments)
      arguments$mcmc_control$n_mcmc <- 2L
      first <- do.call(rqrgibbs::rqr_dlm_fit, arguments)
      second <- rqrgibbs::rqr_dlm_continue(
        first, n_mcmc = 2L, store_state_draws = TRUE
      )
      third <- rqrgibbs::rqr_dlm_continue(
        second, n_mcmc = 2L, store_state_draws = TRUE
      )
      segments <- list(first, second, third)
      saved_equal <- all(c(
        vapply(column_fields, function(field) {
          identical(
            full[[field]], bind_field(segments, field, "columns")
          )
        }, logical(1L)),
        vapply(row_fields, function(field) {
          identical(full[[field]], bind_field(segments, field, "rows"))
        }, logical(1L)),
        vapply(array_fields, function(field) {
          identical(full[[field]], bind_field(segments, field, "array"))
        }, logical(1L)),
        vapply(vector_fields, function(field) {
          identical(
            full[[field]], bind_field(segments, field, "vector")
          )
        }, logical(1L))
      ))
      checkpoint_equal <- identical(
        full$checkpoint_state, third$checkpoint_state
      ) && identical(full$checkpoint_digest, third$checkpoint_digest)
      history_shape <- identical(
        third$continuation_history_contract$generation, 2L
      ) &&
        identical(
          vapply(
            third$continuation_history_contract$segments,
            `[[`, integer(1L), "generation"
          ),
          0:2
        )
      cell_id <- paste(fixture_id, learning_rate_mode, sep = "__")
      cell_rows[[cell_index]] <- data.frame(
        fixture_id = fixture_id,
        learning_rate_mode = learning_rate_mode,
        seed = arguments$mcmc_control$seed,
        all_saved_stochastic_fields_bitwise = saved_equal,
        final_checkpoint_bitwise = checkpoint_equal,
        three_segment_history = history_shape,
        full_checkpoint_digest = full$checkpoint_digest,
        segmented_checkpoint_digest = third$checkpoint_digest,
        continuation_history_digest =
          third$continuation_history_digest,
        stringsAsFactors = FALSE
      )
      gates[[length(gates) + 1L]] <- reference_gate(
        paste0("all_saved_fields_6_vs_2_plus_2_plus_2__", cell_id),
        saved_equal,
        detail = paste(
          "root ordinates, full and terminal states, time-zero states,",
          "lambda, component scales, and retained conditional parameters"
        )
      )
      gates[[length(gates) + 1L]] <- reference_gate(
        paste0("checkpoint_6_vs_2_plus_2_plus_2__", cell_id),
        checkpoint_equal
      )
      gates[[length(gates) + 1L]] <- reference_gate(
        paste0("history_shape_2_plus_2_plus_2__", cell_id),
        history_shape
      )
      last_segmented <- third
    }
  }
  cell_table <- do.call(rbind, cell_rows)
  atomic_write_csv(
    cell_table, file.path(output_dir, "continuation_cells.csv")
  )

  invalid_history <- function(object) {
    object$continuation_history_digest <- rqrgibbs:::.rqr_digest(
      object$continuation_history_contract
    )
    inherits(
      try(
        rqrgibbs:::.rqr_validate_continuation_history(object),
        silent = TRUE
      ),
      "try-error"
    )
  }
  mutation_rows <- list()
  mutation_index <- 0L
  invalid_values <- c(0.5, -0.5, Inf, .Machine$integer.max + 1)
  for (generation_index in 1:2) {
    for (field in c(
        "generation", "segment_numerical_repair_count",
        "cumulative_numerical_repair_count"
      )) {
      for (value in invalid_values) {
        mutation_index <- mutation_index + 1L
        mutated <- last_segmented
        mutated$continuation_history_contract$
          segments[[generation_index]][[field]] <- value
        mutation_rows[[mutation_index]] <- data.frame(
          generation = generation_index - 1L,
          field = field,
          value = as.character(value),
          rejected = invalid_history(mutated),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  semantic_mutations <- list(
    generation0_target_status = function(x) {
      x$continuation_history_contract$segments[[1L]]$
        segment_target_numerical_eligible <- FALSE
      x
    },
    generation0_mismatch_without_override = function(x) {
      x$continuation_history_contract$segments[[1L]]$
        environment_mismatches <- "package_version"
      x
    },
    generation1_backend_without_mismatch = function(x) {
      x$continuation_history_contract$segments[[2L]]$
        backend_resolved <- "R"
      x$continuation_history_contract$segments[[3L]]$
        parent_backend_resolved <- "R"
      x
    }
  )
  for (name in names(semantic_mutations)) {
    mutation_index <- mutation_index + 1L
    mutation_rows[[mutation_index]] <- data.frame(
      generation = NA_integer_,
      field = name,
      value = "semantic",
      rejected = invalid_history(
        semantic_mutations[[name]](last_segmented)
      ),
      stringsAsFactors = FALSE
    )
  }
  mutation_table <- do.call(rbind, mutation_rows)
  atomic_write_csv(
    mutation_table,
    file.path(output_dir, "continuation_history_mutations.csv")
  )
  atomic_save_rds(
    cell_table,
    file.path(output_dir, "continuation_reference_digests.rds")
  )
  gates[[length(gates) + 1L]] <- reference_gate(
    "rehashed_early_history_raw_and_semantic_mutations",
    all(mutation_table$rejected),
    sum(mutation_table$rejected), nrow(mutation_table)
  )
  do.call(rbind, gates)
}

if (identical(mode, "reference-only")) {
  run_reference_stage <- function(stage, function_to_run) {
    tryCatch(
      function_to_run(),
      error = function(error) {
        record_failure(stage, error)
        stop(error)
      }
    )
  }
  dense <- run_reference_stage(
    "dense_ffbs_reference", run_dense_ffbs_reference
  )
  gates <- rbind(
    dense$gates,
    run_reference_stage(
      "canonical_missing_references",
      run_canonical_missing_references
    ),
    run_reference_stage(
      "public_future_references", run_public_future_references
    ),
    run_reference_stage(
      "component_scale_reference", run_component_scale_reference
    ),
    run_reference_stage(
      "continuation_reference", run_continuation_reference
    ),
    reference_gate(
      "active_process_tree_monitor",
      isTRUE(base_manifest$process_tree_monitor_active) &&
        identical(
          base_manifest$process_tree_monitor_kind,
          "pgid_sampled_fallback"
        ) &&
        !isTRUE(base_manifest$kernel_hard_memory_ceiling),
      detail = paste(
        "PGID sampling fallback with traps and a final sweep;",
        "sampled RSS is telemetry, not a kernel-hard peak"
      )
    )
  )
  reference_gates_path <- file.path(output_dir, "reference_gates.csv")
  atomic_write_csv(
    gates, reference_gates_path
  )
  pass <- all(gates$pass)
  reference_bundle_files <- c(
    "fixture_construction.csv", "runtime_toolchain.json",
    "session_info.txt", "failure_log.csv", "reference_gates.csv",
    "dense_ffbs_reference.rds", "canonical_missing_checks.csv",
    "public_future_root_checks.csv",
    "component_scale_conditionals.csv", "continuation_cells.csv",
    "continuation_history_mutations.csv",
    "continuation_reference_digests.rds"
  )
  missing_bundle_files <- reference_bundle_files[
    !file.exists(file.path(output_dir, reference_bundle_files))
  ]
  if (length(missing_bundle_files)) {
    stop(
      "Reference bundle is incomplete: ",
      paste(missing_bundle_files, collapse = ", "),
      call. = FALSE
    )
  }
  reference_bundle <- list(
    schema_version = "rqrgibbs_reference_bundle/1.0.0",
    primary_commit = actual_commit,
    config_digest = base_manifest$config_digest,
    runtime_tree_digest =
      runtime_state$runtime_package_tree_digest,
    runtime_attestation_sha256 =
      runtime_contract$primary_runtime_attestation_sha256,
    runtime_toolchain_digest = runtime_contract$digest,
    files = as.list(stats::setNames(
      vapply(reference_bundle_files, function(file) {
        digest::digest(
          file = file.path(output_dir, file),
          algo = "sha256", serialize = FALSE
        )
      }, character(1L)),
      reference_bundle_files
    ))
  )
  atomic_write_json(
    reference_bundle,
    file.path(output_dir, "reference_bundle.json")
  )
  write_manifest(list(
    status = if (pass) "passed" else "failed",
    fixture_construction_passed = TRUE,
    reference_gates_executed = TRUE,
    reference_gate_count = nrow(gates),
    reference_gate_pass_count = sum(gates$pass),
    reference_gates_sha256 = digest::digest(
      file = reference_gates_path,
      algo = "sha256", serialize = FALSE
    ),
    reference_bundle_sha256 = digest::digest(
      file = file.path(output_dir, "reference_bundle.json"),
      algo = "sha256", serialize = FALSE
    ),
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

# Benchmark and bounded-grid execution are isolated in a separate source file
# so the reviewed reference calculations above cannot drift with cell-running
# mechanics. The sourced stage must terminate explicitly; returning is a
# fail-closed error.
sys.source(
  file.path(
    repo_root, "application", "scripts",
    "09_run_rqr_dlm_bounded_cells.R"
  ),
  envir = environment()
)
stop("The bounded cell stage returned without a terminal status.", call. = FALSE)
