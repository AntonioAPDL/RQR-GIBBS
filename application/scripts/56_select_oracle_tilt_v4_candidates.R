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
run_dir <- normalizePath(arg_value("--run-dir=", ""), mustWork = TRUE)
output_dir <- normalizePath(
  arg_value("--output-dir=", file.path(run_dir, "selector_audit")),
  mustWork = FALSE
)
if (dir.exists(output_dir) && length(list.files(output_dir, all.files = TRUE,
                                                no.. = TRUE))) {
  oti_stop("The selector-audit output directory must be fresh.")
}
oti_ensure_dir(output_dir)
otp_verify_manifest(run_dir)
otv4_verify_wrapper_manifest(run_dir)
config <- oti_read_json(file.path(run_dir, "config.json"))
otv4_validate_config(config)
closeout <- jsonlite::read_json(
  file.path(run_dir, "closeout.json"), simplifyVector = TRUE
)
source_state <- jsonlite::read_json(
  file.path(run_dir, "source_state.json"), simplifyVector = TRUE
)
closeout_checks <- c(
  identical(closeout$schema_version, otv4_schema()),
  identical(closeout$mode, "execute"), isTRUE(closeout$pass),
  isTRUE(closeout$exact_runtime_bound),
  as.integer(closeout$completed_cells) == 18L,
  as.integer(closeout$completed_chains) == 81L,
  identical(closeout$source_commit, source_state$source_commit),
  identical(closeout$config_sha256, source_state$config_sha256),
  identical(closeout$runtime_tree_digest, source_state$runtime_tree_digest),
  identical(closeout$response_predictive_analysis, FALSE),
  identical(closeout$simulation_study, FALSE),
  identical(closeout$automatic_manuscript_promotion, FALSE)
)
if (!all(closeout_checks)) {
  oti_stop("The selector requires a complete, exact-runtime V4 execute bundle.")
}
fit_summary <- utils::read.csv(
  file.path(run_dir, "fit_summary.csv"), stringsAsFactors = FALSE
)
selection <- otv4_select_candidates(fit_summary, config)
recorded <- utils::read.csv(
  file.path(run_dir, "selected_candidates.csv"), stringsAsFactors = FALSE
)
canonical <- function(x) {
  x <- x[order(x$family), , drop = FALSE]
  rownames(x) <- NULL
  x
}
recorded_match <- identical(canonical(selection$selected), canonical(recorded))
permuted <- fit_summary[rev(seq_len(nrow(fit_summary))), , drop = FALSE]
permuted_selection <- otv4_select_candidates(permuted, config)
permutation_invariant <- identical(
  canonical(selection$selected), canonical(permuted_selection$selected)
)
otf_atomic_write_csv(
  selection$cell_audit, file.path(output_dir, "cell_audit.csv")
)
otf_atomic_write_csv(
  selection$score_components,
  file.path(output_dir, "selection_score_components.csv")
)
otf_atomic_write_csv(
  selection$family_ranking, file.path(output_dir, "family_ranking.csv")
)
otf_atomic_write_csv(
  selection$selected, file.path(output_dir, "selected_candidates.csv")
)
audit <- data.frame(
  schema_version = "rqrgibbs_oracle_tilt_v4_selector_audit/1.0.0",
  complete = selection$complete, family_winners = nrow(selection$selected),
  recorded_match = recorded_match,
  row_permutation_invariant = permutation_invariant,
  target_specific_selection_prohibited =
    selection$target_specific_selection_prohibited,
  realized_content_used = selection$realized_content_used,
  aesthetic_judgment_used = selection$aesthetic_judgment_used,
  pass = selection$complete && recorded_match && permutation_invariant,
  stringsAsFactors = FALSE
)
otf_atomic_write_csv(audit, file.path(output_dir, "selector_audit.csv"))
files <- list.files(output_dir, full.names = TRUE)
manifest <- oti_file_hashes(files, output_dir)
names(manifest)[names(manifest) == "relative_path"] <- "path"
otf_atomic_write_csv(manifest, file.path(output_dir, "artifact_manifest.csv"))
otp_verify_manifest(output_dir)
if (!isTRUE(audit$pass)) oti_stop("The independent V4 selector audit failed.")
message("[oracle-tilt-v4-selector] deterministic family selection verified: ",
        output_dir)
