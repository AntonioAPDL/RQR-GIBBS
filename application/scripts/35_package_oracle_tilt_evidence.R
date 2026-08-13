#!/usr/bin/env Rscript

arguments <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_path <- sub(file_arg, "", arguments[startsWith(arguments, file_arg)][1L])
if (!length(script_path) || is.na(script_path)) {
  script_path <- "application/scripts/35_package_oracle_tilt_evidence.R"
}
script_dir <- dirname(normalizePath(script_path, mustWork = TRUE))
source(file.path(script_dir, "32_oracle_tilt_illustration_utils.R"))
source(file.path(script_dir, "34_oracle_tilt_publication_utils.R"))

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
    file.path(repo_root, "figures", "data", "oracle_tilt_c095")
  ),
  winslash = "/", mustWork = FALSE
)

otp_verify_manifest(run_dir)
closeout <- jsonlite::read_json(
  file.path(run_dir, "closeout.json"), simplifyVector = TRUE
)
if (!isTRUE(closeout$all_chains_completed) ||
    !isTRUE(closeout$all_cells_hard_pass) ||
    as.integer(closeout$failed_cells) != 0L) {
  oti_stop("The run did not satisfy the compact-evidence hard gate.")
}
disposition <- utils::read.csv(
  file.path(run_dir, "cell_disposition.csv"), stringsAsFactors = FALSE
)
if (nrow(disposition) != 6L || !all(disposition$hard_pass)) {
  oti_stop("Exactly six hard-passing family/target cells are required.")
}

stage <- tempfile(
  paste0(".", basename(output_dir), "-"), dirname(output_dir)
)
dir.create(stage, recursive = TRUE, showWarnings = FALSE)
on.exit(unlink(stage, recursive = TRUE, force = TRUE), add = TRUE)
files <- otp_compact_files()
missing <- files[!file.exists(file.path(run_dir, files))]
if (length(missing)) {
  oti_stop("Compact run artifacts are missing: ", paste(missing, collapse = ", "))
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
  } else if (!file.copy(source_path, destination, overwrite = FALSE)) {
    oti_stop("Could not stage compact evidence file: ", file)
  }
}

receipt <- list(
  schema_version = "rqrgibbs_oracle_tilt_evidence/1.0.0",
  source_commit = closeout$source_commit %||%
    jsonlite::read_json(
      file.path(stage, "source_state.json"), simplifyVector = TRUE
    )$source_commit,
  coverage_level = 0.95,
  target_cells = 6L,
  all_cells_hard_pass = TRUE,
  strict_pass_cells = as.integer(closeout$strict_pass_cells),
  warning_cells = as.integer(closeout$warning_cells),
  warning_scope = if (as.integer(closeout$warning_cells) > 0L) {
    "ESS-only; all hard validity, R-hat, MCSE, reference, and pathology gates pass"
  } else {
    "none"
  },
  exact_population_oracle_tilts = TRUE,
  cornish_fisher_used = FALSE,
  response_predictive_analysis = FALSE,
  simulation_study = FALSE,
  manuscript_illustration_evidence_eligible = TRUE
)
jsonlite::write_json(
  receipt, file.path(stage, "evidence_receipt.json"),
  pretty = TRUE, auto_unbox = TRUE, digits = NA
)
writeLines(
  c(
    "# Oracle-tilt illustration evidence",
    "",
    paste(
      "This directory contains compact, hashed evidence for the single-data",
      "95% fixed-design and dynamic-linear interval-root illustrations."
    ),
    paste(
      "The three columns of each figure target MPI, the",
      "equal-tailed oracle tilt, and the shortest-interval oracle tilt."
    ),
    paste(
      "The tilts are computed from the known standardized asymmetric-Laplace",
      "innovation law; no Cornish--Fisher approximation is used."
    ),
    paste(
      "These artifacts summarize loss-based generalized posteriors and do not",
      "define or evaluate a posterior-predictive response distribution."
    )
  ),
  file.path(stage, "README.md")
)
manifest_files <- list.files(stage, full.names = TRUE, recursive = TRUE)
manifest <- oti_file_hashes(manifest_files, stage)
names(manifest)[names(manifest) == "relative_path"] <- "path"
write.csv(
  manifest, file.path(stage, "evidence_manifest.csv"), row.names = FALSE
)

if (file.exists(output_dir) || dir.exists(output_dir)) {
  if (!replace) oti_stop("Evidence output exists; use --replace explicitly.")
  backup <- paste0(
    output_dir, ".backup-", format(Sys.time(), "%Y%m%d%H%M%S")
  )
  if (!file.rename(output_dir, backup)) {
    oti_stop("Could not preserve the existing evidence directory.")
  }
  message("[oracle-tilt-evidence] preserved prior evidence at: ", backup)
}
dir.create(dirname(output_dir), recursive = TRUE, showWarnings = FALSE)
if (!file.rename(stage, output_dir)) {
  oti_stop("Could not atomically publish compact evidence.")
}
message("[oracle-tilt-evidence] published: ", output_dir)
