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

trailing <- commandArgs(trailingOnly = TRUE)
arg_value <- function(prefix, default = NULL) {
  hit <- trailing[startsWith(trailing, prefix)]
  if (length(hit)) sub(prefix, "", hit[1L], fixed = TRUE) else default
}
root <- normalizePath(
  arg_value("--output-dir=", ""), winslash = "/", mustWork = TRUE
)
cell <- arg_value("--cell=", "")
n_index <- otv3_integer_scalar(
  arg_value("--n-index=", "2400"), "n-index", 10L
)
n_draw <- otv3_integer_scalar(
  arg_value("--n-draw=", "6000"), "n-draw", 10L
)
n_chain <- otv3_integer_scalar(
  arg_value("--n-chain=", "4"), "n-chain", 2L
)
if (!grepl("^[a-z0-9_]+$", cell)) oti_stop("Invalid rehearsal cell name.")

final <- file.path(root, "resource_cells", cell)
if (dir.exists(final)) oti_stop("The rehearsal cell already exists: ", cell)
cells_root <- oti_ensure_dir(file.path(root, "resource_cells"))
stage <- tempfile(paste0(".", cell, "-"), cells_root)
if (!dir.create(stage, recursive = TRUE, showWarnings = FALSE)) {
  oti_stop("Could not create the rehearsal staging directory.")
}
on.exit(unlink(stage, recursive = TRUE, force = TRUE), add = TRUE)

paths <- character(n_chain)
row_term <- seq(-0.25, 0.25, length.out = n_index)
column_term <- ((seq_len(n_draw) - 1L) %% 101L) / 10000
for (chain in seq_len(n_chain)) {
  lower <- outer(row_term - 1 - chain / 1000, column_term, `+`)
  upper <- lower + 2 + outer(abs(row_term), column_term * 0, `+`) / 10
  prediction <- otv3_endpoint_only_prediction(list(
    lower_draws = lower, upper_draws = upper
  ))
  envelope <- list(
    schema_version = otv3_worker_schema(),
    cell = cell, chain = chain, pred = prediction
  )
  path <- file.path(stage, sprintf("worker_%02d.rds", chain))
  otf_atomic_save_rds(envelope, path, compress = FALSE)
  paths[chain] <- path
  rm(lower, upper, prediction, envelope)
  invisible(gc(full = TRUE))
}

envelopes <- lapply(paths, readRDS)
combined <- otv3_combine_endpoint_predictions(
  lapply(envelopes, `[[`, "pred")
)
summary <- data.frame(
  schema_version = otv3_lifecycle_schema(), cell = cell,
  pid = Sys.getpid(), n_index = nrow(combined$lower_draws),
  n_draw_per_chain = n_draw, n_chains = n_chain,
  combined_draws = ncol(combined$lower_draws),
  lower_mean_checksum = sum(combined$lower_mean),
  upper_mean_checksum = sum(combined$upper_mean),
  midpoint_identity_max_error = max(abs(
    combined$midpoint_draws -
      0.5 * (combined$lower_draws + combined$upper_draws)
  )),
  width_identity_max_error = max(abs(
    combined$width_draws -
      (combined$upper_draws - combined$lower_draws)
  )),
  worker_bytes = sum(file.info(paths)$size),
  endpoint_only = all(vapply(
    envelopes,
    function(envelope) otv3_prediction_storage_contract(envelope$pred),
    logical(1L)
  )),
  stringsAsFactors = FALSE
)
summary$pass <- with(
  summary,
  n_index > 0L & combined_draws == n_draw_per_chain * n_chains &
    midpoint_identity_max_error == 0 & width_identity_max_error == 0 &
    endpoint_only
)
otf_atomic_write_csv(summary, file.path(stage, "cell_summary.csv"))
files <- list.files(stage, full.names = TRUE, recursive = TRUE)
manifest <- oti_file_hashes(files, stage)
names(manifest)[names(manifest) == "relative_path"] <- "path"
otf_atomic_write_csv(manifest, file.path(stage, "artifact_manifest.csv"))
otp_verify_manifest(stage)
if (!file.rename(stage, final)) oti_stop("Could not publish rehearsal cell.")
on.exit(NULL, add = FALSE)
if (!isTRUE(summary$pass)) oti_stop("The rehearsal cell failed.")
