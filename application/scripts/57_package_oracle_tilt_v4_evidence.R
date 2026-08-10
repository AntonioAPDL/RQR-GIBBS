#!/usr/bin/env Rscript

arguments <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_path <- sub(file_arg, "", arguments[startsWith(arguments, file_arg)][1L])
script_path <- normalizePath(script_path, winslash = "/", mustWork = TRUE)
script_dir <- dirname(script_path)
source(file.path(script_dir, "32_oracle_tilt_illustration_utils.R"))
source(file.path(script_dir, "33_oracle_tilt_forensic_utils.R"))
source(file.path(script_dir, "34_oracle_tilt_publication_utils.R"))
source(file.path(script_dir, "42_oracle_tilt_publication_v3_utils.R"))
source(file.path(script_dir, "52_oracle_tilt_publication_v4_utils.R"))

trailing <- commandArgs(trailingOnly = TRUE)
arg_value <- function(prefix, default = NULL) {
  hit <- trailing[startsWith(trailing, prefix)]
  if (length(hit)) sub(prefix, "", hit[1L], fixed = TRUE) else default
}
replace <- any(trailing == "--replace")
repo_root <- normalizePath(file.path(script_dir, "..", ".."), mustWork = TRUE)
run_dir <- normalizePath(arg_value("--run-dir=", ""), mustWork = TRUE)
output_dir <- normalizePath(
  arg_value("--output-dir=", file.path(
    repo_root, "figures", "data", "oracle_tilt_c095_v4_selected"
  )), mustWork = FALSE
)
otp_verify_manifest(run_dir)
otv4_verify_wrapper_manifest(run_dir)
closeout <- jsonlite::read_json(
  file.path(run_dir, "closeout.json"), simplifyVector = TRUE
)
required <- c(
  identical(closeout$schema_version, otv4_schema()),
  identical(closeout$mode, "execute"), isTRUE(closeout$pass),
  isTRUE(closeout$exact_runtime_bound),
  as.integer(closeout$completed_cells) == 18L,
  as.integer(closeout$completed_chains) == 81L,
  as.integer(closeout$family_winners) == 2L,
  isTRUE(closeout$selection_complete),
  isTRUE(closeout$target_specific_selection_prohibited),
  identical(closeout$automatic_manuscript_promotion, FALSE),
  identical(closeout$response_predictive_analysis, FALSE),
  identical(closeout$simulation_study, FALSE)
)
if (!all(required)) oti_stop("The V4 run is not eligible for compact packaging.")
for (file in c("resource_summary.csv", "wrapper_closeout.csv",
               "wrapper_artifact_manifest.csv")) {
  if (!file.exists(file.path(run_dir, file))) {
    oti_stop("The V4 run lacks wrapper evidence: ", file)
  }
}
resource <- utils::read.csv(
  file.path(run_dir, "resource_summary.csv"), stringsAsFactors = FALSE
)
wrapper <- utils::read.csv(
  file.path(run_dir, "wrapper_closeout.csv"), stringsAsFactors = FALSE
)
if (nrow(resource) != 1L || nrow(wrapper) != 1L ||
    !isTRUE(resource$pass) || !isTRUE(resource$final_pgid_empty) ||
    !isTRUE(wrapper$wrapper_pass)) {
  oti_stop("The V4 monitored wrapper did not pass.")
}
selected <- utils::read.csv(
  file.path(run_dir, "selected_candidates.csv"), stringsAsFactors = FALSE
)
if (nrow(selected) != 2L ||
    !setequal(selected$family, c("fixed_design", "dlm")) ||
    anyDuplicated(selected$family) ||
    any(!selected$selected_candidate_id %in% otv4_candidate_ids())) {
  oti_stop("The V4 family-level candidate selection is incomplete.")
}
cell_audit <- utils::read.csv(
  file.path(run_dir, "cell_audit.csv"), stringsAsFactors = FALSE
)
chosen <- merge(
  cell_audit, selected[, c("family", "selected_candidate_id")],
  by = "family", all = FALSE
)
chosen <- chosen[chosen$candidate_id == chosen$selected_candidate_id, , drop = FALSE]
if (nrow(chosen) != 6L || !all(chosen$selection_eligible) ||
    !setequal(chosen$target, c("RQR", "ET", "SH"))) {
  oti_stop("The two selected candidates do not provide six eligible cells.")
}

dir.create(dirname(output_dir), recursive = TRUE, showWarnings = FALSE)
stage <- tempfile(paste0(".", basename(output_dir), "-"), dirname(output_dir))
if (!dir.create(stage, recursive = TRUE, showWarnings = FALSE)) {
  oti_stop("Could not create V4 evidence staging directory.")
}
on.exit(unlink(stage, recursive = TRUE, force = TRUE), add = TRUE)
files <- unique(c(
  setdiff(otv4_compact_files(), "failure_log.csv"),
  "artifact_manifest.csv", "resource_summary.csv", "wrapper_closeout.csv",
  "wrapper_artifact_manifest.csv"
))
files <- files[file.exists(file.path(run_dir, files))]
for (file in files) {
  if (!file.copy(file.path(run_dir, file), file.path(stage, file),
                 overwrite = FALSE)) {
    oti_stop("Could not stage V4 compact evidence: ", file)
  }
}
receipt <- list(
  schema_version = "rqrgibbs_oracle_tilt_v4_evidence/1.0.0",
  source_commit = closeout$source_commit,
  config_sha256 = closeout$config_sha256,
  runtime_tree_digest = closeout$runtime_tree_digest,
  candidate_count = 3L, fitted_cells = 18L, fitted_chains = 81L,
  selected_family_candidates = setNames(
    as.list(selected$selected_candidate_id), selected$family
  ),
  selected_cells = 6L,
  selection_unit = "one candidate per family shared across RQR, ET, and SH",
  exact_population_oracle_tilts = TRUE, cornish_fisher_used = FALSE,
  response_predictive_analysis = FALSE, simulation_study = FALSE,
  typical_performance_claim = FALSE,
  automatic_manuscript_promotion = FALSE,
  disclosure = paste(
    "Best-of-three prospective single-data candidate selection is",
    "truth-informed and is packaged for review, not automatically promoted."
  )
)
jsonlite::write_json(
  receipt, file.path(stage, "evidence_receipt.json"),
  pretty = TRUE, auto_unbox = TRUE, digits = NA
)
hashes <- oti_file_hashes(list.files(stage, full.names = TRUE), stage)
names(hashes)[names(hashes) == "relative_path"] <- "path"
otf_atomic_write_csv(hashes, file.path(stage, "evidence_artifact_manifest.csv"))
backup <- NULL
if (dir.exists(output_dir)) {
  if (!replace) oti_stop("The V4 evidence output already exists; use --replace.")
  backup <- paste0(output_dir, ".backup-", format(Sys.time(), "%Y%m%d%H%M%S"))
  if (!file.rename(output_dir, backup)) oti_stop("Could not back up V4 evidence.")
  on.exit(if (dir.exists(backup)) file.rename(backup, output_dir), add = TRUE)
}
if (!file.rename(stage, output_dir)) oti_stop("Could not publish V4 evidence.")
if (!is.null(backup) && dir.exists(backup)) unlink(backup, recursive = TRUE)
on.exit(NULL, add = FALSE)
message("[oracle-tilt-v4-package] compact selected-candidate evidence: ",
        output_dir)
