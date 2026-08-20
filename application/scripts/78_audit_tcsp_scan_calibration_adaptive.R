#!/usr/bin/env Rscript

# Scan-only TCSP calibration audit.
# This script does not fit MTI, Gibbs, ECM, or Bayesian distribution models.
# It compares retained-count feasibility and seed stability for the selected
# scan calibration rule before any expensive validation relaunch.

arguments <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_path <- sub(file_arg, "", arguments[startsWith(arguments, file_arg)][1L])
if (!length(script_path) || is.na(script_path)) {
  script_path <- "application/scripts/78_audit_tcsp_scan_calibration_adaptive.R"
}
script_path <- normalizePath(script_path, winslash = "/", mustWork = TRUE)
repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."),
                           winslash = "/", mustWork = TRUE)
setwd(repo_root)

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(prefix, default = NULL) {
  hit <- args[startsWith(args, prefix)]
  if (length(hit)) sub(prefix, "", hit[[1L]], fixed = TRUE) else default
}
stopf <- function(...) stop(paste0(...), call. = FALSE)
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

csv_values <- function(value) {
  if (is.null(value) || !nzchar(value)) return(NULL)
  trimws(strsplit(value, ",", fixed = TRUE)[[1L]])
}

numeric_values <- function(value, default = NULL) {
  x <- csv_values(value)
  if (is.null(x)) return(default)
  as.numeric(x)
}

integer_values <- function(value, default = NULL) {
  x <- csv_values(value)
  if (is.null(x)) return(default)
  as.integer(x)
}

character_values <- function(x) {
  as.character(unlist(x, use.names = FALSE))
}

design_cell_frame <- function(mode_cfg) {
  design_cells <- mode_cfg$design_cells %||% NULL
  if (!is.null(design_cells)) {
    rows <- lapply(seq_along(design_cells), function(ii) {
      cell <- design_cells[[ii]]
      data.frame(
        cell_id = as.character(cell$cell_id %||% sprintf("cell%03d", ii)),
        n = as.integer(cell$n)[1L],
        guaranteed_content = as.numeric(cell$guaranteed_content)[1L],
        tolerance_confidence = as.numeric(cell$tolerance_confidence)[1L],
        stringsAsFactors = FALSE
      )
    })
    return(do.call(rbind, rows))
  }
  expand.grid(
    cell_id = NA_character_,
    n = integer_values(
      arg_value("--sample-sizes=", NULL),
      default = as.integer(character_values(mode_cfg$sample_sizes %||%
                                              c(100L, 500L, 1000L)))
    ),
    guaranteed_content = numeric_values(
      arg_value("--contents=", NULL),
      default = as.numeric(character_values(mode_cfg$guaranteed_contents %||%
                                              c(0.90, 0.95, 0.99)))
    ),
    tolerance_confidence = numeric_values(
      arg_value("--tolerance-confidences=", NULL),
      default = as.numeric(character_values(mode_cfg$tolerance_confidences %||%
                                              c(0.95)))
    ),
    stringsAsFactors = FALSE
  )
}

audit_cell_grid <- function(cells, method, n_sim, numerical_confidence, seed,
                            adaptive_control) {
  rows <- vector("list", nrow(cells))
  for (ii in seq_len(nrow(cells))) {
    cell_seed <- if (is.null(seed) || is.na(seed)) NULL else as.integer(seed) + ii
    one <- rqr_tcsp_calibration_boundary_map(
      sample_sizes = as.integer(cells$n[[ii]]),
      guaranteed_contents = as.numeric(cells$guaranteed_content[[ii]]),
      tolerance_confidences =
        as.numeric(cells$tolerance_confidence[[ii]]),
      method = method,
      n_sim = n_sim,
      numerical_confidence = numerical_confidence,
      seed = cell_seed,
      adaptive_control = adaptive_control
    )
    one$cell_id <- cells$cell_id[[ii]]
    rows[[ii]] <- one
  }
  out <- do.call(rbind, rows)
  out <- out[, c("cell_id", setdiff(names(out), "cell_id")), drop = FALSE]
  rownames(out) <- NULL
  out
}

sha256_file <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  digest::digest(file = path, algo = "sha256")
}

git_commit <- function() {
  out <- tryCatch(
    suppressWarnings(system2("git", c("rev-parse", "HEAD"),
                             stdout = TRUE, stderr = FALSE)),
    error = function(e) character(0)
  )
  status <- attr(out, "status") %||% 0L
  if (!length(out) || is.na(status) || status != 0L) return(NA_character_)
  out[[1L]]
}

git_status_short <- function() {
  out <- tryCatch(
    suppressWarnings(system2("git", c("status", "--short"),
                             stdout = TRUE, stderr = FALSE)),
    error = function(e) character(0)
  )
  status <- attr(out, "status") %||% 0L
  if (is.na(status) || status != 0L) {
    return(NA_character_)
  }
  as.character(out)
}

for (package in c("rqrgibbs", "jsonlite", "digest")) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stopf("Required package is not installed: ", package)
  }
}
library(rqrgibbs)

config_path <- arg_value("--config=", "")
mode <- arg_value("--mode=", "confirmatory")
method <- arg_value("--method=", "monte_carlo_cp_adaptive")
output_dir <- normalizePath(
  arg_value(
    "--output-dir=",
    file.path("application", "outputs", "tcsp_scan_calibration_audit",
              paste0("audit_", format(Sys.time(), "%Y%m%dT%H%M%SZ",
                                      tz = "UTC")))
  ),
  winslash = "/", mustWork = FALSE
)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

config <- NULL
mode_cfg <- list()
scan_cfg <- list()
if (nzchar(config_path)) {
  config_path <- normalizePath(config_path, winslash = "/", mustWork = TRUE)
  config <- jsonlite::read_json(config_path, simplifyVector = FALSE)
  scan_cfg <- config$scan_calibration %||% list()
  mode_cfg <- config$modes[[mode]] %||% list()
  if (!length(mode_cfg)) {
    stopf("Mode not found in config: ", mode)
  }
}

design_cells_from_config <- !is.null(mode_cfg$design_cells %||% NULL)
cells <- design_cell_frame(mode_cfg)
if (!nrow(cells)) {
  stopf("No scan calibration cells were selected.")
}
if (any(!is.finite(cells$n)) || any(cells$n < 1L) ||
    any(!is.finite(cells$guaranteed_content)) ||
    any(cells$guaranteed_content <= 0 | cells$guaranteed_content >= 1) ||
    any(!is.finite(cells$tolerance_confidence)) ||
    any(cells$tolerance_confidence <= 0 | cells$tolerance_confidence >= 1)) {
  stopf("Scan calibration cells require finite n and probabilities in (0, 1).")
}
if (any(is.na(cells$cell_id))) {
  cells$cell_id <- sprintf(
    "n%04d_c%s_t%s",
    cells$n,
    gsub("\\.", "", sprintf("%.3f", cells$guaranteed_content)),
    gsub("\\.", "", sprintf("%.3f", cells$tolerance_confidence))
  )
}

n_sim <- as.integer(
  arg_value(
    "--n-sim=",
    scan_cfg[[paste0(mode, "_n_sim")]] %||%
      scan_cfg$n_sim %||%
      10000L
  )
)[1L]
numerical_confidence <- as.numeric(
  arg_value(
    "--numerical-confidence=",
    scan_cfg[[paste0(mode, "_numerical_confidence")]] %||%
      scan_cfg$numerical_confidence %||%
      0.999
  )
)[1L]
seed <- as.integer(arg_value("--seed=", scan_cfg$seed %||% 1513600L))[1L]
seeds <- integer_values(
  arg_value("--seeds=", NULL),
  default = seed + c(0L, 1009L, 2003L, 3001L, 4001L)
)

adaptive_control <- scan_cfg$adaptive_control %||% list()
adaptive_control <- utils::modifyList(
  adaptive_control,
  mode_cfg$scan_adaptive_control %||% mode_cfg$adaptive_control %||% list()
)
cli_control <- list()
for (field in c("initial_n_sim", "batch_n_sim", "max_n_sim",
                "max_looks", "stable_looks")) {
  cli_value <- arg_value(paste0("--", gsub("_", "-", field), "="), NULL)
  if (!is.null(cli_value)) {
    cli_control[[field]] <- as.integer(cli_value)[1L]
  }
}
adaptive_control <- utils::modifyList(adaptive_control, cli_control)

boundary <- audit_cell_grid(
  cells = cells,
  method = method,
  n_sim = n_sim,
  numerical_confidence = numerical_confidence,
  seed = seed,
  adaptive_control = adaptive_control
)

stability <- do.call(rbind, lapply(seeds, function(one_seed) {
  one <- audit_cell_grid(
    cells = cells,
    method = method,
    n_sim = n_sim,
    numerical_confidence = numerical_confidence,
    seed = one_seed,
    adaptive_control = adaptive_control
  )
  one$seed <- one_seed
  one
}))

cell_key <- paste(stability$n, stability$guaranteed_content,
                  stability$tolerance_confidence, stability$scan_critical_method,
                  sep = "|")
split_rows <- split(stability, cell_key)
stability_summary <- do.call(rbind, lapply(split_rows, function(x) {
  finite_k <- as.integer(x$retained_count[is.finite(x$retained_count)])
  data.frame(
    n = x$n[[1L]],
    guaranteed_content = x$guaranteed_content[[1L]],
    tolerance_confidence = x$tolerance_confidence[[1L]],
    scan_critical_method = x$scan_critical_method[[1L]],
    seeds = nrow(x),
    min_retained_count = if (length(finite_k)) min(finite_k) else NA_integer_,
    max_retained_count = if (length(finite_k)) max(finite_k) else NA_integer_,
    unique_retained_counts = length(unique(finite_k)),
    any_infeasible = any(x$infeasible),
    structural_statuses = paste(sort(unique(x$structural_status)),
                                collapse = ";"),
    stringsAsFactors = FALSE
  )
}))
rownames(stability_summary) <- NULL

utils::write.csv(boundary, file.path(output_dir, "boundary_map.csv"),
                 row.names = FALSE)
utils::write.csv(cells, file.path(output_dir, "calibration_cells.csv"),
                 row.names = FALSE)
utils::write.csv(stability, file.path(output_dir, "stability_by_seed.csv"),
                 row.names = FALSE)
utils::write.csv(stability_summary,
                 file.path(output_dir, "stability_summary.csv"),
                 row.names = FALSE)

manifest <- list(
  schema_version = "rqrgibbs_tcsp_scan_calibration_audit/1.0.0",
  generated_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  repo_root = repo_root,
  script_path = script_path,
  script_sha256 = sha256_file(script_path),
  git_commit = git_commit(),
  git_status_short = git_status_short(),
  package_version = as.character(utils::packageVersion("rqrgibbs")),
  config_path = if (nzchar(config_path)) config_path else NA_character_,
  config_sha256 = if (nzchar(config_path)) sha256_file(config_path) else
    NA_character_,
  mode = mode,
  method = method,
  design_cells_from_config = design_cells_from_config,
  n_design_cells = nrow(cells),
  sample_sizes = sort(unique(cells$n)),
  guaranteed_contents = sort(unique(cells$guaranteed_content)),
  tolerance_confidences = sort(unique(cells$tolerance_confidence)),
  n_sim = n_sim,
  numerical_confidence = numerical_confidence,
  seed = seed,
  seeds = seeds,
  adaptive_control = adaptive_control,
  outputs = list(
    calibration_cells = "calibration_cells.csv",
    boundary_map = "boundary_map.csv",
    stability_by_seed = "stability_by_seed.csv",
    stability_summary = "stability_summary.csv"
  )
)
jsonlite::write_json(
  manifest,
  file.path(output_dir, "manifest.json"),
  auto_unbox = TRUE,
  pretty = TRUE
)

cat("TCSP scan-calibration audit written to:\n")
cat(output_dir, "\n")
