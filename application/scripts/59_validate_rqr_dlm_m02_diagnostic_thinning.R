#!/usr/bin/env Rscript

# Execute one predeclared non-sentinel M02 chain with the frozen production
# schedule. This is a diagnostic-construction canary, not simulation evidence.

arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) != 1L) {
  stop(
    paste(
      "Usage: 59_validate_rqr_dlm_m02_diagnostic_thinning.R",
      "<new-output-root>"
    ),
    call. = FALSE
  )
}
repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(repo_root, "application", "DESCRIPTION"))) {
  stop("Run the M02 canary from the repository root.", call. = FALSE)
}
output_root <- normalizePath(
  arguments[[1L]], winslash = "/", mustWork = FALSE
)
if (file.exists(output_root) || dir.exists(output_root)) {
  stop("The M02 canary output root must be new.", call. = FALSE)
}
dir.create(output_root, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages(library(rqrgibbs))
source(file.path(
  repo_root, "application", "scripts", "lib",
  "rqr_dlm_confirmatory_simulation.R"
))

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

expected_commit <- tolower(Sys.getenv(
  "RQR_EXPECTED_PRIMARY_COMMIT", unset = ""
))
primary_attestation <- Sys.getenv(
  "RQR_PRIMARY_RUNTIME_ATTESTATION", unset = ""
)
exdqlm_attestation <- Sys.getenv(
  "RQR_EXDQLM_RUNTIME_ATTESTATION", unset = ""
)
if (!grepl("^[0-9a-f]{40}$", expected_commit) ||
    !file.exists(primary_attestation) ||
    !file.exists(exdqlm_attestation)) {
  stop(
    paste(
      "The exact primary commit and existing primary/exdqlm runtime",
      "attestations are required."
    ),
    call. = FALSE
  )
}
source_commit <- tolower(git_value(c("rev-parse", "HEAD")))
source_status <- git_value(c(
  "status", "--porcelain=v2", "--untracked-files=all"
))
if (!identical(source_commit, expected_commit) || nzchar(source_status)) {
  stop("The M02 canary source is not exact and clean.", call. = FALSE)
}

thread_variables <- c(
  "OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS",
  "VECLIB_MAXIMUM_THREADS", "NUMEXPR_NUM_THREADS"
)
thread_environment <- setNames(
  vapply(thread_variables, Sys.getenv, character(1L), unset = ""),
  thread_variables
)
if (any(thread_environment != "1")) {
  stop("Every numerical-library thread limit must equal one.", call. = FALSE)
}

contract <- rqr_confirm_read_contract(repo_root)
rqr_confirm_validate_contract(contract, require_closed = TRUE)
primary_control <- rqr_confirm_primary_provenance_control(
  repo_root, expected_commit, primary_attestation
)
primary_provenance <- rqrgibbs:::.rqr_provenance(
  data = numeric(),
  repo_root = primary_control$repo_root,
  expected_git_commit = primary_control$expected_git_commit,
  numerical_policy = "fail", backend = "cpp",
  primary_runtime_attestation =
    primary_control$primary_runtime_attestation
)
if (!isTRUE(primary_provenance$reproducibility_eligible)) {
  stop("The M02 canary primary runtime is not source bound.", call. = FALSE)
}
exdqlm_specification <- contract$config$comparator$exdqlm
exdqlm_runtime <- rqr_confirm_read_attestation(
  exdqlm_attestation, "exdqlm",
  exdqlm_specification$version,
  exdqlm_specification$source_sha256
)

ledger <- rqr_confirm_seed_ledger(contract, planning = "maximum")
wave_id <- "static_gaussian_T200__target0200__sentinel"
wave_plan <- rqr_confirm_wave_plan(contract, planning = "maximum")
tasks <- wave_plan[
  wave_plan$wave_id == wave_id &
    grepl("(^|\\|)M02(\\||$)", wave_plan$methods),
  , drop = FALSE
]
sentinels <- rqr_confirm_sentinel_map(contract, planning = "maximum")
is_m02_sentinel <- vapply(seq_len(nrow(tasks)), function(row) {
  cell_id <- contract$incidence$cell_id[
    contract$incidence$DGP == tasks$DGP[[row]] &
      contract$incidence$method == "M02"
  ]
  any(
    sentinels$cell_id == cell_id &
      sentinels$replication == tasks$replication[[row]]
  )
}, logical(1L))
tasks <- tasks[!is_m02_sentinel, , drop = FALSE]
if (!nrow(tasks)) {
  stop("Wave 1 contains no non-sentinel M02 canary task.", call. = FALSE)
}
task <- tasks[1L, , drop = FALSE]
cell_id <- contract$incidence$cell_id[
  contract$incidence$DGP == task$DGP[[1L]] &
    contract$incidence$method == "M02"
]
started_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
started_elapsed <- proc.time()[["elapsed"]]
generated <- rqr_confirm_generate_dgp(
  contract, task$DGP[[1L]], task$replication[[1L]], ledger
)
result <- rqr_confirm_dynamic_quantile(
  contract = contract, generated = generated, chain = 1L,
  ledger = ledger,
  exdqlm_attestation_path = exdqlm_attestation,
  profile_name = "standard"
)
raw_counts <- vapply(
  result$fits,
  function(fit) dim(unclass(fit$samp.theta))[[3L]],
  integer(1L)
)
scalar <- rqr_confirm_scalar_draws(
  result, generated, contract, "M02"
)
schedule <- rqr_confirm_method_schedule(
  contract, "M02", "standard"
)
expected_rows <- length(seq.int(
  from = result$diagnostic_thin,
  to = raw_counts[[1L]],
  by = result$diagnostic_thin
))
expected_schema <- rqr_confirm_diagnostic_schema(
  "M02", generated, contract
)
future_columns <- grep("^future_h", colnames(scalar), value = TRUE)
checks <- data.frame(
  check = c(
    "raw_endpoint_draw_counts_equal",
    "raw_draw_count_matches_frozen_schedule",
    "diagnostic_thin_matches_transition_multiplier",
    "scalar_row_count_matches_common_retained_index",
    "exact_diagnostic_schema",
    "all_reported_future_functions_present",
    "all_scalar_draws_finite",
    "primary_runtime_source_bound",
    "protected_exdqlm_checkout_not_used"
  ),
  pass = c(
    length(unique(raw_counts)) == 1L,
    all(raw_counts == schedule$retain),
    identical(
      result$diagnostic_thin,
      rqr_confirm_method_transition_policy(
        contract, "M02"
      )$transition_multiplier
    ),
    nrow(scalar) == expected_rows,
    identical(colnames(scalar), expected_schema),
    length(future_columns) ==
      4L * length(contract$config$design$reported_horizons),
    all(is.finite(scalar)),
    isTRUE(primary_provenance$reproducibility_eligible),
    !isTRUE(exdqlm_runtime$protected_checkout_used)
  ),
  stringsAsFactors = FALSE
)
if (!all(checks$pass)) {
  stop("The production-schedule M02 diagnostic canary failed.", call. = FALSE)
}

diagnostics <- rqr_confirm_chain_diagnostics(
  list(scalar), contract, sentinel = FALSE,
  method = "M02", generated = generated
)
summary <- data.frame(
  DGP = task$DGP[[1L]], replication = task$replication[[1L]],
  cell_id = cell_id, profile = "standard", chain = 1L,
  raw_lower_draws = raw_counts[[1L]],
  raw_upper_draws = raw_counts[[2L]],
  diagnostic_thin = result$diagnostic_thin,
  diagnostic_rows = nrow(scalar),
  diagnostic_columns = ncol(scalar),
  diagnostic_passes = sum(diagnostics$pass),
  diagnostic_warnings = sum(!diagnostics$pass),
  elapsed_seconds = as.numeric(
    proc.time()[["elapsed"]] - started_elapsed
  ),
  peak_RSS_KiB = rqr_confirm_process_peak_rss_kib(),
  stringsAsFactors = FALSE
)
rqr_confirm_atomic_write_csv(
  checks, file.path(output_root, "canary_checks.csv")
)
rqr_confirm_atomic_write_csv(
  summary, file.path(output_root, "canary_summary.csv")
)
rqr_confirm_atomic_write_csv(
  diagnostics, file.path(output_root, "canary_diagnostics.csv")
)
rqr_confirm_atomic_write_csv(
  data.frame(
    estimand = colnames(scalar),
    mean = colMeans(scalar),
    sd = apply(scalar, 2L, stats::sd),
    stringsAsFactors = FALSE
  ),
  file.path(output_root, "canary_scalar_summary.csv")
)
rqr_confirm_atomic_write_json(
  list(
    schema_version = "rqrgibbs_dlm_m02_diagnostic_canary/1.0.0",
    source_commit = source_commit,
    package_version = as.character(utils::packageVersion("rqrgibbs")),
    primary_runtime_attestation_sha256 =
      rqr_confirm_sha256(primary_attestation),
    primary_runtime_tree_digest =
      primary_provenance$primary_runtime_tree_digest,
    exdqlm_runtime_attestation_sha256 =
      rqr_confirm_sha256(exdqlm_attestation),
    exdqlm_runtime_tree_digest = exdqlm_runtime$runtime_tree_digest,
    exdqlm_source_package_sha256 =
      exdqlm_runtime$source_package_sha256,
    config_digest = digest::digest(
      contract$config, algo = "sha256", serialize = TRUE
    ),
    incidence_digest = digest::digest(
      contract$incidence, algo = "sha256", serialize = TRUE
    ),
    seed_ledger_digest = digest::digest(
      ledger[, c("task_key", "state_digest"), drop = FALSE],
      algo = "sha256", serialize = TRUE
    ),
    wave_id = wave_id,
    DGP = task$DGP[[1L]],
    replication = task$replication[[1L]],
    cell_id = cell_id,
    transition_multiplier = result$diagnostic_thin,
    raw_draws = raw_counts[[1L]],
    diagnostic_rows = nrow(scalar),
    exact_schema = identical(colnames(scalar), expected_schema),
    diagnostic_thresholds_nonblocking = TRUE,
    comparative_simulation_metrics_used = FALSE,
    failed_outputs_reused = FALSE,
    response_predictive_draws = FALSE,
    thread_environment = as.list(thread_environment),
    started_at_utc = started_at,
    completed_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
  ),
  file.path(output_root, "canary_manifest.json")
)
rqr_confirm_atomic_write_csv(
  rqr_confirm_recursive_manifest(output_root),
  file.path(output_root, "artifact_hashes.csv")
)

cat(sprintf(
  paste0(
    "M02 diagnostic canary passed: %d raw draws -> %d aligned rows; ",
    "%d/%d frozen diagnostics passed.\n"
  ),
  raw_counts[[1L]], nrow(scalar),
  sum(diagnostics$pass), nrow(diagnostics)
))

