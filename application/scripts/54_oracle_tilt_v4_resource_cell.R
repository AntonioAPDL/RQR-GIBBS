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
root <- normalizePath(arg_value("--output-dir=", ""), mustWork = TRUE)
candidate_id <- arg_value("--candidate=", "")
family <- arg_value("--family=", "")
target <- toupper(arg_value("--target=", ""))
config_path <- normalizePath(arg_value("--config=", ""), mustWork = TRUE)
scale <- as.numeric(arg_value("--scale=", "1"))
if (!candidate_id %in% otv4_candidate_ids() ||
    !family %in% c("fixed_design", "dlm") ||
    !target %in% c("RQR", "ET", "SH") ||
    length(scale) != 1L || is.na(scale) || !is.finite(scale) ||
    scale <= 0 || scale > 1) {
  oti_stop("Invalid V4 resource-cell contract.")
}
config <- oti_read_json(config_path)
otv4_validate_config(config)

cell_key <- otv4_cell_key(candidate_id, family, target)
final <- file.path(root, "resource_cells", cell_key)
if (dir.exists(final)) oti_stop("The V4 rehearsal cell already exists.")
cell_root <- oti_ensure_dir(file.path(root, "resource_cells"))
stage <- tempfile(paste0(".", cell_key, "-"), cell_root)
if (!dir.create(stage, recursive = TRUE, showWarnings = FALSE)) {
  oti_stop("Could not create the V4 rehearsal-cell staging directory.")
}
on.exit(unlink(stage, recursive = TRUE, force = TRUE), add = TRUE)

n_index_full <- if (family == "fixed_design") {
  as.integer(config$fixed_design$n)
} else {
  as.integer(config$dlm$T)
}
n_chain <- if (family == "fixed_design") {
  as.integer(config$fixed_design$n_chains)
} else {
  as.integer(config$dlm$n_chains)
}
n_draw_full <- if (family == "fixed_design") {
  as.integer(config$fixed_design$mcmc_control$n_mcmc)
} else {
  as.integer(config$dlm$target_retained_draws[[target]])
}
n_index <- max(20L, as.integer(ceiling(n_index_full * scale)))
n_draw <- max(20L, as.integer(ceiling(n_draw_full * scale)))
seed <- as.integer(config$candidate_contract$rehearsal_seed) +
  1000L * match(candidate_id, otv4_candidate_ids()) +
  100L * match(family, c("fixed_design", "dlm")) +
  10L * match(target, c("RQR", "ET", "SH"))

paths <- character(n_chain)
row_term <- seq(-0.25, 0.25, length.out = n_index)
for (chain in seq_len(n_chain)) {
  set.seed(seed + chain)
  column_term <- stats::rnorm(n_draw, sd = 0.005)
  lower <- outer(row_term - 1 - chain / 1000, column_term, `+`)
  upper <- lower + 2 + outer(abs(row_term), rep(0.1, n_draw), `*`)
  prediction <- otv3_endpoint_only_prediction(list(
    lower_draws = lower, upper_draws = upper
  ))
  envelope <- list(
    schema_version = "rqrgibbs_oracle_tilt_v4_resource_worker/1.0.0",
    candidate_id = candidate_id, family = family, target = target,
    chain = chain, seed = seed + chain, pred = prediction
  )
  path <- file.path(stage, sprintf("worker_%02d.rds", chain))
  otf_atomic_save_rds(envelope, path, compress = FALSE)
  paths[chain] <- path
  rm(lower, upper, prediction, envelope)
  invisible(gc(full = TRUE))
}

envelopes <- lapply(paths, readRDS)
combined <- otv3_combine_endpoint_predictions(lapply(envelopes, `[[`, "pred"))
summary <- data.frame(
  schema_version = "rqrgibbs_oracle_tilt_v4_resource_cell/1.0.0",
  candidate_id = candidate_id, family = family, target = target,
  cell_key = cell_key, pid = Sys.getpid(), scale = scale,
  n_index = nrow(combined$lower_draws), n_index_full = n_index_full,
  n_draw_per_chain = n_draw, n_draw_per_chain_full = n_draw_full,
  n_chains = n_chain, combined_draws = ncol(combined$lower_draws),
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
    envelopes, function(value) otv3_prediction_storage_contract(value$pred),
    logical(1L)
  )), stringsAsFactors = FALSE
)
summary$pass <- with(
  summary,
  n_index > 0L && combined_draws == n_draw_per_chain * n_chains &&
    midpoint_identity_max_error == 0 && width_identity_max_error == 0 &&
    endpoint_only
)
otf_atomic_write_csv(summary, file.path(stage, "cell_summary.csv"))
files <- list.files(stage, full.names = TRUE, recursive = TRUE)
manifest <- oti_file_hashes(files, stage)
names(manifest)[names(manifest) == "relative_path"] <- "path"
otf_atomic_write_csv(manifest, file.path(stage, "artifact_manifest.csv"))
otp_verify_manifest(stage)
if (!file.rename(stage, final)) oti_stop("Could not publish V4 rehearsal cell.")
on.exit(NULL, add = FALSE)
if (!isTRUE(summary$pass)) oti_stop("The V4 rehearsal cell failed.")
