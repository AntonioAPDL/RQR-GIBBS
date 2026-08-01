#!/usr/bin/env Rscript

arguments <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_path <- sub(file_arg, "", arguments[startsWith(arguments, file_arg)][1L])
if (!length(script_path) || is.na(script_path)) {
  script_path <- "application/scripts/43_package_oracle_tilt_v3_evidence.R"
}
script_dir <- dirname(normalizePath(script_path, mustWork = TRUE))
source(file.path(script_dir, "32_oracle_tilt_illustration_utils.R"))
source(file.path(script_dir, "33_oracle_tilt_forensic_utils.R"))
source(file.path(script_dir, "34_oracle_tilt_publication_utils.R"))
source(file.path(script_dir, "42_oracle_tilt_publication_v3_utils.R"))

trailing <- commandArgs(trailingOnly = TRUE)
arg_value <- function(prefix, default = NULL) {
  hit <- trailing[startsWith(trailing, prefix)]
  if (length(hit)) sub(prefix, "", hit[1L], fixed = TRUE) else default
}
replace <- any(trailing == "--replace")
repo_root <- normalizePath(file.path(script_dir, "..", ".."), mustWork = TRUE)
run_dir <- arg_value("--run-dir=", "")
if (!nzchar(run_dir)) oti_stop("--run-dir is required.")
run_dir <- normalizePath(run_dir, winslash = "/", mustWork = TRUE)
output_dir <- normalizePath(
  arg_value(
    "--output-dir=",
    file.path(repo_root, "figures", "data", "oracle_tilt_c095_v3")
  ),
  winslash = "/", mustWork = FALSE
)

otp_verify_manifest(run_dir)
closeout <- jsonlite::read_json(
  file.path(run_dir, "closeout.json"), simplifyVector = TRUE
)
required_closeout <- c(
  identical(closeout$schema_version, otv3_schema()),
  identical(closeout$mode, "execute"), isTRUE(closeout$pass),
  isTRUE(closeout$exact_runtime_bound),
  isTRUE(closeout$all_chains_completed), isTRUE(closeout$all_cells_pass),
  isTRUE(closeout$compact_evidence_eligible),
  as.integer(closeout$completed_chains) == 27L,
  as.integer(closeout$passed_cells) == 6L,
  as.integer(closeout$failed_cells) == 0L,
  isTRUE(closeout$exact_population_oracle_tilts),
  identical(closeout$cornish_fisher_used, FALSE),
  identical(closeout$response_predictive_analysis, FALSE),
  identical(closeout$simulation_study, FALSE)
)
if (!all(required_closeout)) {
  oti_stop("The run does not satisfy the v3 compact-evidence contract.")
}
disposition <- utils::read.csv(
  file.path(run_dir, "cell_disposition.csv"), stringsAsFactors = FALSE
)
if (nrow(disposition) != 6L ||
    !all(disposition$manuscript_illustration_evidence_eligible) ||
    !all(disposition$disposition == "strict_pass")) {
  oti_stop("Exactly six strict-passing family/target cells are required.")
}
runtime_binding <- jsonlite::read_json(
  file.path(run_dir, "runtime_binding.json"), simplifyVector = TRUE
)
source_state <- jsonlite::read_json(
  file.path(run_dir, "source_state.json"), simplifyVector = TRUE
)
input_binding <- utils::read.csv(
  file.path(run_dir, "input_bundle_binding.csv"), stringsAsFactors = FALSE
)
worker_manifest <- utils::read.csv(
  file.path(run_dir, "worker_manifest.csv"), stringsAsFactors = FALSE
)
run_status <- utils::read.csv(
  file.path(run_dir, "run_status.csv"), stringsAsFactors = FALSE
)
if (!isTRUE(runtime_binding$match) || !isTRUE(source_state$exact_runtime_bound) ||
    !identical(source_state$source_commit, closeout$source_commit) ||
    !identical(source_state$config_sha256, closeout$config_sha256) ||
    !identical(source_state$runtime_tree_digest, closeout$runtime_tree_digest) ||
    nrow(input_binding) != 3L ||
    !setequal(input_binding$mode, c("preflight", "reference-only", "benchmark")) ||
    nrow(worker_manifest) != 27L || anyDuplicated(worker_manifest$path) ||
    nrow(run_status) != 6L || !all(run_status$disposition == "strict_pass")) {
  oti_stop("The runtime, prior-bundle, worker, or cell ledger is incomplete.")
}
reference <- utils::read.csv(
  file.path(run_dir, "reference_gates.csv"), stringsAsFactors = FALSE
)
benchmark <- utils::read.csv(
  file.path(run_dir, "benchmark_summary.csv"), stringsAsFactors = FALSE
)
if (nrow(reference) != 24L || !all(reference$pass) ||
    nrow(benchmark) != 2L || !all(benchmark$pass)) {
  oti_stop("The bound reference or benchmark evidence is incomplete.")
}
wrapper_required <- c(
  "resource_summary.csv", "wrapper_closeout.csv",
  "wrapper_artifact_manifest.csv"
)
if (any(!file.exists(file.path(run_dir, wrapper_required)))) {
  oti_stop("The monitored wrapper evidence is incomplete.")
}
resource <- utils::read.csv(
  file.path(run_dir, "resource_summary.csv"), stringsAsFactors = FALSE
)
wrapper <- utils::read.csv(
  file.path(run_dir, "wrapper_closeout.csv"), stringsAsFactors = FALSE
)
if (nrow(resource) != 1L || nrow(wrapper) != 1L ||
    !isTRUE(resource$pass) || !isTRUE(wrapper$wrapper_pass) ||
    resource$runner_status != 0L || wrapper$runner_status != 0L ||
    !isTRUE(resource$final_pgid_empty)) {
  oti_stop("The monitored process-group resource contract did not pass.")
}
wrapper_hashes <- utils::read.csv(
  file.path(run_dir, "wrapper_artifact_manifest.csv"),
  stringsAsFactors = FALSE
)
wrapper_hash_pass <- vapply(seq_len(nrow(wrapper_hashes)), function(index) {
  path <- file.path(run_dir, wrapper_hashes$path[index])
  file.exists(path) && !dir.exists(path) &&
    unname(file.info(path)$size) == wrapper_hashes$bytes[index] &&
    identical(oti_file_sha256(path), wrapper_hashes$sha256[index])
}, logical(1L))
if (!nrow(wrapper_hashes) || !all(wrapper_hash_pass)) {
  oti_stop("The wrapper artifact manifest failed verification.")
}

stage <- tempfile(paste0(".", basename(output_dir), "-"), dirname(output_dir))
dir.create(stage, recursive = TRUE, showWarnings = FALSE)
on.exit(unlink(stage, recursive = TRUE, force = TRUE), add = TRUE)
files <- setdiff(otv3_compact_files(), "failure_log.csv")
missing <- files[!file.exists(file.path(run_dir, files))]
if (length(missing)) {
  oti_stop("Compact v3 artifacts are missing: ", paste(missing, collapse = ", "))
}
for (file in files) {
  source_path <- file.path(run_dir, file)
  destination <- file.path(stage, file)
  if (identical(file, "runtime_binding.json")) {
    binding <- jsonlite::read_json(source_path, simplifyVector = TRUE)
    binding$runtime_path <- NULL
    jsonlite::write_json(
      binding, destination, pretty = TRUE, auto_unbox = TRUE,
      digits = NA, null = "null", na = "null"
    )
  } else if (identical(file, "input_bundle_binding.csv")) {
    binding <- utils::read.csv(source_path, stringsAsFactors = FALSE)
    binding$path <- basename(binding$path)
    utils::write.csv(binding, destination, row.names = FALSE)
  } else if (!file.copy(source_path, destination, overwrite = FALSE)) {
    oti_stop("Could not stage compact v3 evidence file: ", file)
  }
}
for (file in wrapper_required) {
  if (!file.copy(file.path(run_dir, file), file.path(stage, file),
                 overwrite = FALSE)) {
    oti_stop("Could not stage monitored v3 evidence file: ", file)
  }
}

config <- jsonlite::read_json(
  file.path(stage, "config.json"), simplifyVector = FALSE
)
otv3_validate_config(config)
receipt <- list(
  schema_version = "rqrgibbs_oracle_tilt_evidence/3.0.0",
  source_commit = closeout$source_commit,
  config_sha256 = closeout$config_sha256,
  runtime_tree_digest = closeout$runtime_tree_digest,
  coverage_level = 0.95,
  innovation_contract = "affinely standardized AL_0.80(0,1)",
  fixed_design_n = 2400L, dlm_T = 1200L, dlm_n_observed = 1178L,
  fixed_design_contract = "eight-dimensional orthogonalized cubic B-spline",
  dynamic_contract = paste(
    "four-state local-linear plus regularized Fourier seasonal harmonic",
    "with covariate-dependent population scale"
  ),
  target_cells = 6L, completed_chains = 27L,
  all_cells_strict_pass = TRUE,
  exact_population_oracle_tilts = TRUE, cornish_fisher_used = FALSE,
  response_predictive_analysis = FALSE, simulation_study = FALSE,
  source_wrapper_artifact_manifest_sha256 = oti_file_sha256(
    file.path(run_dir, "wrapper_artifact_manifest.csv")
  ),
  manuscript_illustration_evidence_eligible = TRUE
)
jsonlite::write_json(
  receipt, file.path(stage, "evidence_receipt.json"),
  pretty = TRUE, auto_unbox = TRUE, digits = NA
)
writeLines(c(
  "# Oracle-tilt illustration evidence (version 3)",
  "",
  paste(
    "This directory contains compact, hashed evidence for the single-data",
    "95% fixed-design and dynamic-linear interval-root illustrations."
  ),
  paste(
    "The illustrative innovations follow an affinely standardized",
    "asymmetric-Laplace law with source index 0.80."
  ),
  paste(
    "Ordinary RQR, equal-tailed, and shortest-interval tilts are computed",
    "from the exact population law; no Cornish--Fisher approximation is used."
  ),
  paste(
    "The static construction uses an orthogonalized cubic B-spline basis;",
    "the dynamic construction combines an exact continuous-time local-linear",
    "block with one regularized Fourier seasonal harmonic."
  ),
  paste(
    "These artifacts summarize loss-based generalized posteriors; they do",
    "not define a response likelihood or posterior-predictive response law."
  )
), file.path(stage, "README.md"))

manifest_files <- list.files(stage, full.names = TRUE, recursive = TRUE)
manifest <- oti_file_hashes(manifest_files, stage)
names(manifest)[names(manifest) == "relative_path"] <- "path"
utils::write.csv(
  manifest, file.path(stage, "evidence_manifest.csv"), row.names = FALSE
)

if (file.exists(output_dir) || dir.exists(output_dir)) {
  if (!replace) oti_stop("Evidence output exists; use --replace explicitly.")
  backup <- paste0(output_dir, ".backup-", format(Sys.time(), "%Y%m%d%H%M%S"))
  if (!file.rename(output_dir, backup)) {
    oti_stop("Could not preserve the existing v3 evidence directory.")
  }
  message("[oracle-tilt-v3-evidence] preserved prior evidence at: ", backup)
}
dir.create(dirname(output_dir), recursive = TRUE, showWarnings = FALSE)
if (!file.rename(stage, output_dir)) {
  oti_stop("Could not atomically publish compact v3 evidence.")
}
message("[oracle-tilt-v3-evidence] published: ", output_dir)
