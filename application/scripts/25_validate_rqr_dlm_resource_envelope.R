#!/usr/bin/env Rscript

# Exercise a conservative upper bound that keeps four complete retained-state
# shapes live, even though the runner now compacts and releases each fit before
# advancing to the next chain.  No model is fitted and no scientific metric is
# evaluated.  The separate deserialization step checks the local RDS mechanism
# only after the source object has been released.

arguments <- commandArgs(trailingOnly = TRUE)
repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
output_root <- if (length(arguments)) {
  normalizePath(arguments[[1L]], winslash = "/", mustWork = FALSE)
} else {
  file.path(
    repo_root, "application", "cache",
    "rqr_dlm_resource_envelope_validation"
  )
}
if (file.exists(output_root) || dir.exists(output_root)) {
  stop("The resource-envelope output root must be fresh.", call. = FALSE)
}
dir.create(output_root, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages(library(rqrgibbs))
source(file.path(
  repo_root, "application", "scripts", "lib",
  "rqr_dlm_confirmatory_simulation.R"
))
contract <- rqr_confirm_read_contract(repo_root)
rqr_confirm_validate_contract(contract, require_closed = TRUE)

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
if (!grepl("^[0-9a-f]{40}$", source_commit)) {
  stop("The primary Git commit is not a complete SHA.", call. = FALSE)
}
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
  stop("The resource gate is not at the exact clean expected commit.",
       call. = FALSE)
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
if (nzchar(primary_attestation)) {
  primary_attestation <- normalizePath(
    primary_attestation, winslash = "/", mustWork = TRUE
  )
}
thread_variables <- c(
  "OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS",
  "VECLIB_MAXIMUM_THREADS", "NUMEXPR_NUM_THREADS"
)
thread_environment <- setNames(
  vapply(
    thread_variables,
    Sys.getenv,
    character(1L),
    unset = ""
  ),
  thread_variables
)
if (nzchar(expected_commit) && any(thread_environment != "1")) {
  stop(
    "Every declared numerical-library thread limit must equal one.",
    call. = FALSE
  )
}
primary_provenance <- NULL
if (nzchar(expected_commit)) {
  control <- rqr_confirm_primary_provenance_control(
    repo_root, expected_commit, primary_attestation
  )
  primary_provenance <- rqrgibbs:::.rqr_provenance(
    data = numeric(),
    repo_root = control$repo_root,
    expected_git_commit = control$expected_git_commit,
    numerical_policy = "fail",
    backend = "cpp",
    primary_runtime_attestation =
      control$primary_runtime_attestation
  )
  if (!isTRUE(primary_provenance$reproducibility_eligible)) {
    stop(
      "The resource gate is not executing the attested primary runtime.",
      call. = FALSE
    )
  }
}

ledger <- rqr_confirm_seed_ledger(contract, planning = "maximum")
config_digest <- digest::digest(
  contract$config, algo = "sha256", serialize = TRUE
)
incidence_digest <- digest::digest(
  contract$incidence, algo = "sha256", serialize = TRUE
)
seed_ledger_digest <- digest::digest(
  ledger[, c("task_key", "state_digest"), drop = FALSE],
  algo = "sha256", serialize = TRUE
)
toolchain <- rqr_confirm_toolchain_manifest()
required_toolchain_keys <- c(
  "R_version", "platform", "R_compiler", "BLAS", "LAPACK",
  "package_rqrgibbs", "package_posterior", "package_digest",
  "package_jsonlite"
)
toolchain_complete <- nrow(toolchain) >= 10L &&
  !anyNA(toolchain) &&
  all(nzchar(toolchain$key)) &&
  all(nzchar(toolchain$value)) &&
  !anyDuplicated(toolchain$key) &&
  all(required_toolchain_keys %in% toolchain$key)
if (!toolchain_complete) {
  stop("The resource-gate toolchain manifest is incomplete.", call. = FALSE)
}

cases <- data.frame(
  case = c(
    "four_state_component_scale",
    "three_state_learned_component_scale",
    "long_horizon_single_state"
  ),
  state_dimension = c(4L, 3L, 1L),
  training_horizon = c(200L, 200L, 400L),
  retained_draws = c(6000L, 9000L, 6000L),
  component_count = c(2L, 2L, 1L),
  chains = 4L,
  stringsAsFactors = FALSE
)

shape_contract <- data.frame(
  field = c(
    "samp.theta_root1", "samp.theta_root2",
    "samp.theta_terminal_root1", "samp.theta_terminal_root2",
    "samp.theta0_root1", "samp.theta0_root2",
    "samp.eta_root1", "samp.eta_root2", "samp.lambda",
    "samp.evolution_scale", "samp.evolution_scale_shape",
    "samp.evolution_scale_rate"
  ),
  orientation = c(
    "state_by_time_by_draw", "state_by_time_by_draw",
    "state_by_draw", "state_by_draw",
    "state_by_draw", "state_by_draw",
    "time_by_draw", "time_by_draw", "draw",
    "draw_by_component", "draw_by_component", "draw_by_component"
  ),
  dimension_formula = c(
    "p x T x retained", "p x T x retained",
    "p x retained", "p x retained",
    "p x retained", "p x retained",
    "T x retained", "T x retained", "retained",
    "retained x components", "retained x components",
    "retained x components"
  ),
  stringsAsFactors = FALSE
)

child_script <- tempfile(
  ".rqr-resource-child-", tmpdir = tempdir(), fileext = ".R"
)
writeLines(c(
  "args <- commandArgs(trailingOnly = TRUE)",
  "mode <- args[[1L]]",
  "data_path <- args[[2L]]",
  "metric_path <- args[[3L]]",
  "p <- as.integer(args[[4L]])",
  "T <- as.integer(args[[5L]])",
  "draws <- as.integer(args[[6L]])",
  "chains <- as.integer(args[[7L]])",
  "components <- as.integer(args[[8L]])",
  "peak_rss <- function() {",
  "  path <- '/proc/self/status'",
  "  if (!file.exists(path)) return(NA_real_)",
  "  line <- grep('^VmHWM:', readLines(path, warn = FALSE), value = TRUE)",
  "  if (length(line) != 1L) return(NA_real_)",
  "  as.numeric(sub('^VmHWM:[[:space:]]*([0-9]+).*$','\\\\1', line))",
  "}",
  "valid_dimensions <- function(value) {",
  "  length(value) == chains && all(vapply(value, function(chain) {",
  "    fit <- chain$fit",
  "    identical(dim(fit$samp.theta_root1), c(p, T, draws)) &&",
  "      identical(dim(fit$samp.theta_root2), c(p, T, draws)) &&",
  "      identical(dim(fit$samp.theta_terminal_root1), c(p, draws)) &&",
  "      identical(dim(fit$samp.theta_terminal_root2), c(p, draws)) &&",
  "      identical(dim(fit$samp.theta0_root1), c(p, draws)) &&",
  "      identical(dim(fit$samp.theta0_root2), c(p, draws)) &&",
  "      identical(dim(fit$samp.eta_root1), c(T, draws)) &&",
  "      identical(dim(fit$samp.eta_root2), c(T, draws)) &&",
  "      identical(length(fit$samp.lambda), draws) &&",
  "      identical(dim(fit$samp.evolution_scale), c(draws, components)) &&",
  "      identical(dim(fit$samp.evolution_scale_shape), c(draws, components)) &&",
  "      identical(dim(fit$samp.evolution_scale_rate), c(draws, components)) &&",
  "      all(is.finite(c(",
  "        fit$samp.theta_root1[1L, 1L, 1L],",
  "        fit$samp.theta_root2[1L, 1L, 1L],",
  "        fit$samp.theta_terminal_root1[1L, 1L],",
  "        fit$samp.theta_terminal_root2[1L, 1L],",
  "        fit$samp.theta0_root1[1L, 1L],",
  "        fit$samp.theta0_root2[1L, 1L],",
  "        fit$samp.eta_root1[1L, 1L],",
  "        fit$samp.eta_root2[1L, 1L],",
  "        fit$samp.lambda[[1L]],",
  "        fit$samp.evolution_scale[1L, 1L],",
  "        fit$samp.evolution_scale_shape[1L, 1L],",
  "        fit$samp.evolution_scale_rate[1L, 1L]",
  "      )))",
  "  }, logical(1L)))",
  "}",
  "if (identical(mode, 'write')) {",
  "  make_chain <- function() list(",
  "    fit = list(",
  "      samp.theta_root1 = array(0, c(p, T, draws)),",
  "      samp.theta_root2 = array(0, c(p, T, draws)),",
  "      samp.theta_terminal_root1 = matrix(0, p, draws),",
  "      samp.theta_terminal_root2 = matrix(0, p, draws),",
  "      samp.theta0_root1 = matrix(0, p, draws),",
  "      samp.theta0_root2 = matrix(0, p, draws),",
  "      samp.eta_root1 = matrix(0, T, draws),",
  "      samp.eta_root2 = matrix(0, T, draws),",
  "      samp.lambda = rep(1, draws),",
  "      samp.evolution_scale = matrix(1, draws, components),",
  "      samp.evolution_scale_shape = matrix(1, draws, components),",
  "      samp.evolution_scale_rate = matrix(1, draws, components)",
  "    ),",
  "    diagnostics = list(",
  "      generalized_bayes = TRUE,",
  "      response_predictive_draws = FALSE",
  "    )",
  "  )",
  "  value <- lapply(seq_len(chains), function(index) make_chain())",
  "  valid <- valid_dimensions(value)",
  "  temporary <- paste0(data_path, '.tmp-', Sys.getpid())",
  "  saveRDS(value, temporary, compress = 'gzip', version = 3L)",
  "  if (!file.rename(temporary, data_path)) stop('atomic write failed')",
  "} else if (identical(mode, 'read')) {",
  "  value <- readRDS(data_path)",
  "  valid <- valid_dimensions(value)",
  "} else {",
  "  stop('unknown child mode')",
  "}",
  "metrics <- list(",
  "  mode = mode, valid = valid, peak_RSS_KiB = peak_rss(),",
  "  pid = Sys.getpid(), bytes = file.info(data_path)$size",
  ")",
  "temporary_metric <- paste0(metric_path, '.tmp-', Sys.getpid())",
  "saveRDS(metrics, temporary_metric, version = 3L)",
  "if (!file.rename(temporary_metric, metric_path)) {",
  "  stop('atomic metric write failed')",
  "}"
), child_script)
on.exit(unlink(child_script, force = TRUE), add = TRUE)

run_child <- function(mode, data_path, metric_path, case) {
  status <- system2(
    file.path(R.home("bin"), "Rscript"),
    c(
      "--vanilla", shQuote(child_script), mode,
      shQuote(data_path), shQuote(metric_path),
      as.character(case$state_dimension[[1L]]),
      as.character(case$training_horizon[[1L]]),
      as.character(case$retained_draws[[1L]]),
      as.character(case$chains[[1L]]),
      as.character(case$component_count[[1L]])
    ),
    stdout = TRUE, stderr = TRUE
  )
  exit_status <- attr(status, "status") %||% 0L
  if (!identical(as.integer(exit_status), 0L) ||
      !file.exists(metric_path)) {
    stop(
      sprintf(
        "The clean %s process failed for %s: %s",
        mode, case$case[[1L]], paste(status, collapse = "\n")
      ),
      call. = FALSE
    )
  }
  value <- readRDS(metric_path)
  unlink(metric_path, force = TRUE)
  if (!is.list(value) ||
      !identical(value$mode, mode) ||
      !is.numeric(value$pid) ||
      length(value$pid) != 1L ||
      is.na(value$pid) ||
      !is.finite(value$pid) ||
      value$pid <= 0 ||
      value$pid != floor(value$pid) ||
      !is.numeric(value$bytes) ||
      length(value$bytes) != 1L ||
      is.na(value$bytes) ||
      !is.finite(value$bytes) ||
      value$bytes <= 0) {
    stop(
      sprintf("The clean %s process returned malformed metrics.", mode),
      call. = FALSE
    )
  }
  value
}

rows <- vector("list", nrow(cases))
for (index in seq_len(nrow(cases))) {
  case <- cases[index, , drop = FALSE]
  path <- file.path(output_root, paste0(case$case[[1L]], ".rds"))
  writer_metrics <- run_child(
    "write", path,
    tempfile(".writer-metrics-", tmpdir = output_root),
    case
  )
  if (!file.exists(path) || file.info(path)$size <= 0) {
    stop("The clean writer did not publish its RDS.", call. = FALSE)
  }
  sha256 <- rqr_confirm_sha256(path)
  reader_metrics <- run_child(
    "read", path,
    tempfile(".reader-metrics-", tmpdir = output_root),
    case
  )
  sha256_after_reader <- rqr_confirm_sha256(path)
  bytes <- file.info(path)$size
  unlink(path, force = TRUE)
  rows[[index]] <- data.frame(
    case = case$case,
    state_dimension = case$state_dimension,
    training_horizon = case$training_horizon,
    retained_draws = case$retained_draws,
    component_count = case$component_count,
    chains = case$chains,
    writer_pid = as.integer(writer_metrics$pid),
    clean_reader_pid = as.integer(reader_metrics$pid),
    writer_peak_RSS_KiB = as.numeric(writer_metrics$peak_RSS_KiB),
    clean_reader_peak_RSS_KiB =
      as.numeric(reader_metrics$peak_RSS_KiB),
    bytes = bytes,
    sha256 = sha256,
    clean_reader_observed_bytes = as.numeric(reader_metrics$bytes),
    clean_reader_observed_sha256 = sha256_after_reader,
    writer_shape_valid = isTRUE(writer_metrics$valid),
    clean_deserialization_valid = isTRUE(reader_metrics$valid),
    clean_process_pair_valid =
      !identical(as.integer(writer_metrics$pid), Sys.getpid()) &&
      !identical(as.integer(reader_metrics$pid), Sys.getpid()) &&
      !identical(
        as.integer(writer_metrics$pid),
        as.integer(reader_metrics$pid)
      ) &&
      identical(as.numeric(writer_metrics$bytes), as.numeric(bytes)) &&
      identical(as.numeric(reader_metrics$bytes), as.numeric(bytes)) &&
      identical(sha256_after_reader, sha256),
    stringsAsFactors = FALSE
  )
}
results <- do.call(rbind, rows)
ceiling <- contract$config$resources$per_worker_memory_GiB * 1024^2
telemetry <- c(
  results$writer_peak_RSS_KiB,
  results$clean_reader_peak_RSS_KiB
)
telemetry_complete <- length(telemetry) == 2L * nrow(cases) &&
  all(is.finite(telemetry)) &&
  all(telemetry > 0)
maximum_peak <- if (telemetry_complete) max(telemetry) else NA_real_
margin_pass <- telemetry_complete &&
  is.finite(maximum_peak) &&
  maximum_peak <= 0.80 * ceiling
results$declared_worker_memory_ceiling_KiB <- ceiling
results$eighty_percent_margin_KiB <- 0.80 * ceiling
results$serialization_margin_pass <- margin_pass
rqr_confirm_atomic_write_csv(
  shape_contract, file.path(output_root, "fit_shape_contract.csv")
)
rqr_confirm_atomic_write_csv(
  results, file.path(output_root, "resource_envelope.csv")
)
toolchain_digest <- digest::digest(
  toolchain, algo = "sha256", serialize = TRUE
)
rqr_confirm_atomic_write_csv(
  toolchain, file.path(output_root, "toolchain_manifest.csv")
)
primary_runtime_tree_digest <-
  primary_provenance$primary_runtime_tree_digest %||% NA_character_
primary_attestation_sha256 <- if (nzchar(primary_attestation)) {
  rqr_confirm_sha256(primary_attestation)
} else {
  NA_character_
}
exact_runtime_pair <- nzchar(expected_commit) &&
  !is.null(primary_provenance) &&
  identical(source_commit, expected_commit) &&
  !nzchar(source_status) &&
  identical(
    tolower(as.character(primary_provenance$primary_source_commit)),
    expected_commit
  ) &&
  isTRUE(primary_provenance$expected_git_commit_match) &&
  identical(
    as.character(primary_provenance$package_version),
    as.character(utils::packageVersion("rqrgibbs"))
  ) &&
  isTRUE(primary_provenance$primary_runtime_source_match) &&
  isTRUE(primary_provenance$reproducibility_eligible) &&
  is.character(primary_runtime_tree_digest) &&
  length(primary_runtime_tree_digest) == 1L &&
  grepl("^[0-9a-f]{64}$", primary_runtime_tree_digest) &&
  is.character(primary_attestation_sha256) &&
  length(primary_attestation_sha256) == 1L &&
  grepl("^[0-9a-f]{64}$", primary_attestation_sha256)
all_shapes_valid <- all(results$writer_shape_valid) &&
  all(results$clean_deserialization_valid)
clean_process_contract_pass <-
  all(results$clean_process_pair_valid)
promotion_evidence_eligible <- exact_runtime_pair &&
  !nzchar(source_status) &&
  telemetry_complete &&
  margin_pass &&
  all_shapes_valid &&
  clean_process_contract_pass &&
  toolchain_complete
manifest <- list(
    schema_version =
      "rqrgibbs_dlm_resource_envelope_validation/2.0.0",
    source_commit = source_commit,
    source_clean = !nzchar(source_status),
    package_version = as.character(utils::packageVersion("rqrgibbs")),
    expected_commit = if (nzchar(expected_commit)) {
      expected_commit
    } else {
      NA_character_
    },
    config_digest = config_digest,
    incidence_digest = incidence_digest,
    maximum_seed_ledger_digest = seed_ledger_digest,
    primary_runtime_attestation_sha256 =
      primary_attestation_sha256,
    primary_runtime_tree_digest = primary_runtime_tree_digest,
    primary_runtime_source_match =
      if (is.null(primary_provenance)) {
        NA
      } else {
        isTRUE(primary_provenance$primary_runtime_source_match)
      },
    primary_reproducibility_eligible =
      if (is.null(primary_provenance)) {
        NA
      } else {
        isTRUE(primary_provenance$reproducibility_eligible)
      },
    exact_commit_attestation_pair_verified = exact_runtime_pair,
    promotion_evidence_eligible = promotion_evidence_eligible,
    development_execution =
      !nzchar(expected_commit),
    configured_workers = contract$config$resources$workers,
    configured_sentinel_workers =
      contract$config$resources$sentinel_workers,
    resource_gate_worker_processes = 1L,
    modeled_chains_per_worker = unique(cases$chains),
    thread_environment = as.list(thread_environment),
    toolchain_manifest_digest = toolchain_digest,
    toolchain_manifest_complete = toolchain_complete,
    scientific_metrics_used = FALSE,
    response_prediction_contract = FALSE,
    fit_shape_contract =
      "p_by_T_by_draw;T_by_draw;p_by_draw;draw_by_component",
    synthetic_heavy_objects_retained = FALSE,
    writer_measurement_process = "fresh_Rscript",
    reader_measurement_process = "fresh_Rscript",
    distinct_clean_processes_verified = clean_process_contract_pass,
    telemetry_complete = telemetry_complete,
    maximum_writer_or_clean_reader_peak_RSS_KiB = maximum_peak,
    declared_worker_memory_ceiling_KiB = ceiling,
    required_margin_fraction = 0.80,
    resource_margin_pass = margin_pass,
    all_writer_shapes_valid = all(results$writer_shape_valid),
    all_clean_deserializations_valid =
      all(results$clean_deserialization_valid),
    completed_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
  )
rqr_confirm_atomic_write_json(
  manifest,
  file.path(output_root, "validation_manifest.json")
)
resource_closeout <- data.frame(
  schema_version =
    "rqrgibbs_dlm_resource_envelope_closeout/1.0.0",
  source_commit = source_commit,
  package_version = as.character(utils::packageVersion("rqrgibbs")),
  primary_runtime_tree_digest = primary_runtime_tree_digest,
  primary_runtime_attestation_sha256 = primary_attestation_sha256,
  config_digest = config_digest,
  incidence_digest = incidence_digest,
  maximum_seed_ledger_digest = seed_ledger_digest,
  toolchain_manifest_digest = toolchain_digest,
  exact_commit_attestation_pair_verified = exact_runtime_pair,
  promotion_evidence_eligible = promotion_evidence_eligible,
  telemetry_complete = telemetry_complete,
  resource_margin_pass = margin_pass,
  all_writer_shapes_valid = all(results$writer_shape_valid),
  all_clean_deserializations_valid =
    all(results$clean_deserialization_valid),
  maximum_writer_or_clean_reader_peak_RSS_KiB = maximum_peak,
  status = if (promotion_evidence_eligible) {
    "passed"
  } else if (!nzchar(expected_commit)) {
    "development_only"
  } else {
    "failed"
  },
  stringsAsFactors = FALSE
)
rqr_confirm_atomic_write_csv(
  resource_closeout, file.path(output_root, "resource_closeout.csv")
)
expected_pre_manifest_files <- sort(c(
  "fit_shape_contract.csv", "resource_closeout.csv",
  "resource_envelope.csv", "toolchain_manifest.csv",
  "validation_manifest.json"
), method = "radix")
observed_pre_manifest_files <- sort(list.files(
  output_root, recursive = TRUE, all.files = TRUE, no.. = TRUE,
  include.dirs = FALSE
), method = "radix")
if (!identical(observed_pre_manifest_files, expected_pre_manifest_files) ||
    any(grepl("\\.rds$", observed_pre_manifest_files, ignore.case = TRUE))) {
  stop(
    "The resource gate did not close over exactly five compact pre-manifest files.",
    call. = FALSE
  )
}
hashes <- rqr_confirm_recursive_manifest(output_root)
if (!identical(
    sort(as.character(hashes$path), method = "radix"),
    expected_pre_manifest_files
  )) {
  stop("The resource-gate hash manifest has the wrong file set.",
       call. = FALSE)
}
rqr_confirm_atomic_write_csv(
  hashes, file.path(output_root, "artifact_hashes.csv")
)
final_files <- sort(list.files(
  output_root, recursive = TRUE, all.files = TRUE, no.. = TRUE,
  include.dirs = FALSE
), method = "radix")
if (!identical(
    final_files,
    sort(c(expected_pre_manifest_files, "artifact_hashes.csv"),
         method = "radix")
  )) {
  stop("The resource gate did not publish exactly six compact files.",
       call. = FALSE)
}
if (!telemetry_complete || !margin_pass || !all_shapes_valid ||
    !clean_process_contract_pass) {
  stop("The worst-case resource-envelope gate failed.", call. = FALSE)
}
if (nzchar(expected_commit) &&
    !isTRUE(promotion_evidence_eligible)) {
  stop("The exact primary-runtime resource gate failed.", call. = FALSE)
}
cat(sprintf(
  paste0(
    "Resource-envelope gate passed: maximum clean writer/reader peak ",
    "%.0f KiB; promotion evidence eligible: %s.\n"
  ),
  maximum_peak,
  if (promotion_evidence_eligible) "yes" else "no"
))
