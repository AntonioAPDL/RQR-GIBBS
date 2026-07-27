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

source_commit <- tolower(trimws(system2(
  "git", c("-C", shQuote(repo_root), "rev-parse", "HEAD"),
  stdout = TRUE, env = c("GIT_OPTIONAL_LOCKS=0")
))[[1L]])
source_status <- system2(
  "git",
  c(
    "-C", shQuote(repo_root), "status", "--porcelain=v2",
    "--untracked-files=all"
  ),
  stdout = TRUE, env = c("GIT_OPTIONAL_LOCKS=0")
)
expected_commit <- tolower(Sys.getenv(
  "RQR_EXPECTED_PRIMARY_COMMIT", unset = ""
))
primary_attestation <- Sys.getenv(
  "RQR_PRIMARY_RUNTIME_ATTESTATION", unset = ""
)
if (nzchar(expected_commit) &&
    (!identical(source_commit, expected_commit) ||
      length(source_status))) {
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

cases <- data.frame(
  case = c(
    "four_state_component_scale",
    "three_state_learned_component_scale",
    "long_horizon_single_state"
  ),
  state_dimension = c(4L, 3L, 1L),
  training_horizon = c(200L, 200L, 400L),
  retained_draws = c(6000L, 9000L, 6000L),
  chains = 4L,
  stringsAsFactors = FALSE
)

make_chain <- function(p, T, draws) {
  list(
    fit = list(
      samp.theta_root1 = array(0, c(draws, p, T)),
      samp.theta_root2 = array(0, c(draws, p, T)),
      samp.eta_root1 = matrix(0, draws, T),
      samp.eta_root2 = matrix(0, draws, T),
      samp.evolution_scale = matrix(1, draws, max(1L, p))
    ),
    diagnostics = list(
      generalized_bayes = TRUE,
      response_predictive_draws = FALSE
    )
  )
}

rows <- vector("list", nrow(cases))
for (index in seq_len(nrow(cases))) {
  case <- cases[index, , drop = FALSE]
  gc()
  baseline <- rqr_confirm_process_peak_rss_kib()
  value <- lapply(
    seq_len(case$chains[[1L]]),
    function(chain) make_chain(
      case$state_dimension[[1L]],
      case$training_horizon[[1L]],
      case$retained_draws[[1L]]
    )
  )
  path <- file.path(output_root, paste0(case$case[[1L]], ".rds"))
  temporary <- tempfile(
    paste0(".", basename(path), "-"), tmpdir = output_root
  )
  saveRDS(value, temporary, compress = "xz", version = 3L)
  if (!file.exists(temporary) || file.info(temporary)$size <= 0 ||
      !file.rename(temporary, path)) {
    stop("Resource-gate RDS publication failed.", call. = FALSE)
  }
  written_peak <- rqr_confirm_process_peak_rss_kib()
  rm(value)
  gc()
  checked <- readRDS(path)
  valid <- length(checked) == case$chains[[1L]] &&
    all(vapply(
      checked,
      function(chain) {
        identical(
          dim(chain$fit$samp.theta_root1),
          c(
            case$retained_draws[[1L]],
            case$state_dimension[[1L]],
            case$training_horizon[[1L]]
          )
        ) &&
          all(is.finite(c(
            chain$fit$samp.theta_root1[1L, 1L, 1L],
            chain$fit$samp.theta_root2[1L, 1L, 1L],
            chain$fit$samp.eta_root1[1L, 1L],
            chain$fit$samp.eta_root2[1L, 1L]
          )))
      },
      logical(1L)
    ))
  checked_peak <- rqr_confirm_process_peak_rss_kib()
  rm(checked)
  gc()
  rows[[index]] <- data.frame(
    case = case$case,
    state_dimension = case$state_dimension,
    training_horizon = case$training_horizon,
    retained_draws = case$retained_draws,
    chains = case$chains,
    baseline_peak_RSS_KiB = baseline,
    written_peak_RSS_KiB = written_peak,
    checked_peak_RSS_KiB = checked_peak,
    bytes = file.info(path)$size,
    sha256 = rqr_confirm_sha256(path),
    deserialization_valid = valid,
    stringsAsFactors = FALSE
  )
}
results <- do.call(rbind, rows)
ceiling <- contract$config$resources$per_worker_memory_GiB * 1024^2
maximum_peak <- max(results$written_peak_RSS_KiB, na.rm = TRUE)
margin_pass <- is.finite(maximum_peak) && maximum_peak <= 0.80 * ceiling
results$declared_worker_memory_ceiling_KiB <- ceiling
results$eighty_percent_margin_KiB <- 0.80 * ceiling
results$serialization_margin_pass <- margin_pass
rqr_confirm_atomic_write_csv(
  results, file.path(output_root, "resource_envelope.csv")
)
rqr_confirm_atomic_write_json(
  list(
    schema_version =
      "rqrgibbs_dlm_resource_envelope_validation/1.0.0",
    source_commit = source_commit,
    source_clean = !length(source_status),
    package_version = as.character(utils::packageVersion("rqrgibbs")),
    primary_runtime_attestation_sha256 =
      if (nzchar(primary_attestation)) {
        rqr_confirm_sha256(primary_attestation)
      } else {
        NA_character_
      },
    primary_runtime_tree_digest =
      primary_provenance$primary_runtime_tree_digest %||%
        NA_character_,
    primary_reproducibility_eligible =
      if (is.null(primary_provenance)) {
        NA
      } else {
        isTRUE(primary_provenance$reproducibility_eligible)
      },
    thread_environment = as.list(thread_environment),
    scientific_metrics_used = FALSE,
    response_prediction_contract = FALSE,
    maximum_written_peak_RSS_KiB = maximum_peak,
    declared_worker_memory_ceiling_KiB = ceiling,
    required_margin_fraction = 0.80,
    resource_margin_pass = margin_pass,
    all_deserializations_valid =
      all(results$deserialization_valid),
    completed_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
  ),
  file.path(output_root, "validation_manifest.json")
)
hashes <- rqr_confirm_recursive_manifest(output_root)
rqr_confirm_atomic_write_csv(
  hashes, file.path(output_root, "artifact_hashes.csv")
)
if (!margin_pass || !all(results$deserialization_valid)) {
  stop("The worst-case resource-envelope gate failed.", call. = FALSE)
}
if (nzchar(expected_commit) &&
    !isTRUE(primary_provenance$reproducibility_eligible)) {
  stop("The exact primary-runtime resource gate failed.", call. = FALSE)
}
cat(sprintf(
  "Resource-envelope gate passed: maximum written peak %.0f KiB.\n",
  maximum_peak
))
