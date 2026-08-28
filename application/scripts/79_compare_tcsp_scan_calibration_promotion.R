#!/usr/bin/env Rscript

# Scan-only promotion audit for TCSP calibration.
# This script compares the current conservative Monte Carlo scan certificate
# with a candidate adaptive certified Monte Carlo certificate. It does not fit
# tolerance intervals or Bayesian endpoint models.

arguments <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_path <- sub(file_arg, "", arguments[startsWith(arguments, file_arg)][1L])
if (!length(script_path) || is.na(script_path)) {
  script_path <- "application/scripts/79_compare_tcsp_scan_calibration_promotion.R"
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

integer_values <- function(value, default = NULL) {
  x <- csv_values(value)
  if (is.null(x)) return(default)
  as.integer(x)
}

character_values <- function(x) {
  as.character(unlist(x, use.names = FALSE))
}

sha256_file <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  digest::digest(file = path, algo = "sha256")
}

git_output <- function(args) {
  out <- tryCatch(
    suppressWarnings(system2("git", args, stdout = TRUE, stderr = FALSE)),
    error = function(e) character(0)
  )
  status <- attr(out, "status") %||% 0L
  if (is.na(status) || status != 0L) return(NA_character_)
  as.character(out)
}

for (package in c("rqrgibbs", "jsonlite", "digest", "parallel")) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stopf("Required package is not installed: ", package)
  }
}
library(rqrgibbs)

config_path <- normalizePath(
  arg_value(
    "--config=",
    file.path("application", "config",
              "rqr_bayes_uq_validation_main_3method_1000_20260819.json")
  ),
  winslash = "/", mustWork = TRUE
)
mode <- arg_value("--mode=", "confirmatory")
old_method <- arg_value("--old-method=", "monte_carlo_conservative")
new_method <- arg_value("--new-method=", "monte_carlo_cp_adaptive")
output_dir <- normalizePath(
  arg_value(
    "--output-dir=",
    file.path("application", "outputs", "tcsp_scan_calibration_promotion",
              paste0("promotion_", format(Sys.time(), "%Y%m%dT%H%M%SZ",
                                           tz = "UTC")))
  ),
  winslash = "/", mustWork = FALSE
)
if (file.exists(output_dir) || dir.exists(output_dir)) {
  stopf("The output directory must be fresh: ", output_dir)
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

config <- jsonlite::read_json(config_path, simplifyVector = FALSE)
mode_cfg <- config$modes[[mode]] %||% list()
if (!length(mode_cfg)) {
  stopf("Mode not found in config: ", mode)
}
scan_cfg <- config$scan_calibration %||% list()

design_cells <- mode_cfg$design_cells %||% NULL
if (is.null(design_cells)) {
  stopf("Promotion audit requires explicit mode design_cells.")
}
cells <- do.call(rbind, lapply(seq_along(design_cells), function(ii) {
  cell <- design_cells[[ii]]
  data.frame(
    cell_id = as.character(cell$cell_id %||% sprintf("cell%03d", ii)),
    n = as.integer(cell$n)[1L],
    guaranteed_content = as.numeric(cell$guaranteed_content)[1L],
    tolerance_confidence = as.numeric(cell$tolerance_confidence)[1L],
    stringsAsFactors = FALSE
  )
}))
if (!nrow(cells)) stopf("No design cells found.")
if (any(!is.finite(cells$n)) || any(cells$n < 1L) ||
    any(!is.finite(cells$guaranteed_content)) ||
    any(cells$guaranteed_content <= 0 | cells$guaranteed_content >= 1) ||
    any(!is.finite(cells$tolerance_confidence)) ||
    any(cells$tolerance_confidence <= 0 | cells$tolerance_confidence >= 1)) {
  stopf("Design cells require finite n and probabilities in (0, 1).")
}

n_sim <- as.integer(
  arg_value("--n-sim=",
            scan_cfg[[paste0(mode, "_n_sim")]] %||% scan_cfg$n_sim %||%
              10000L)
)[1L]
numerical_confidence <- as.numeric(
  arg_value("--numerical-confidence=",
            scan_cfg[[paste0(mode, "_numerical_confidence")]] %||%
              scan_cfg$numerical_confidence %||% 0.999)
)[1L]
seed <- as.integer(arg_value("--seed=", scan_cfg$seed %||% 1513600L))[1L]
seeds <- integer_values(
  arg_value("--seeds=", NULL),
  default = seed + c(0L, 1009L, 2003L, 3001L, 4001L)
)
workers <- as.integer(arg_value("--workers=", 1L))[1L]
workers <- max(1L, workers)

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

cell_calibration <- function(cell, method, seed_offset) {
  one <- rqr_tcsp_calibration_boundary_map(
    sample_sizes = as.integer(cell$n),
    guaranteed_contents = as.numeric(cell$guaranteed_content),
    tolerance_confidences = as.numeric(cell$tolerance_confidence),
    method = method,
    n_sim = n_sim,
    numerical_confidence = numerical_confidence,
    seed = as.integer(seed_offset),
    adaptive_control = if (identical(method, new_method)) adaptive_control else
      list()
  )
  one$cell_id <- as.character(cell$cell_id)
  one[, c("cell_id", setdiff(names(one), "cell_id")), drop = FALSE]
}

run_tasks <- function(tasks) {
  worker <- function(task) {
    cell_calibration(task$cell, task$method, task$seed)
  }
  if (.Platform$OS.type == "unix" && workers > 1L && length(tasks) > 1L) {
    rows <- parallel::mclapply(
      tasks, worker, mc.cores = min(workers, length(tasks)),
      mc.preschedule = FALSE
    )
  } else {
    rows <- lapply(tasks, worker)
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

base_tasks <- unlist(lapply(seq_len(nrow(cells)), function(ii) {
  cell <- cells[ii, , drop = FALSE]
  list(
    list(cell = cell, method = old_method, seed = seed + ii),
    list(cell = cell, method = new_method, seed = seed + 100000L + ii)
  )
}), recursive = FALSE)

base_results <- run_tasks(base_tasks)
old <- base_results[base_results$scan_critical_method == old_method, ,
                    drop = FALSE]
adaptive <- base_results[base_results$scan_critical_method == new_method, ,
                         drop = FALSE]

stability_tasks <- unlist(lapply(seeds, function(one_seed) {
  lapply(seq_len(nrow(cells)), function(ii) {
    list(
      cell = cells[ii, , drop = FALSE],
      method = new_method,
      seed = as.integer(one_seed) + ii
    )
  })
}), recursive = FALSE)
stability <- run_tasks(stability_tasks)
stability$seed <- rep(seeds, each = nrow(cells))

cell_key <- paste(stability$cell_id, stability$n,
                  stability$guaranteed_content,
                  stability$tolerance_confidence, sep = "|")
stability_summary <- do.call(rbind, lapply(split(stability, cell_key),
                                           function(x) {
  k <- as.integer(x$retained_count[is.finite(x$retained_count)])
  k_range <- if (length(k)) max(k) - min(k) else NA_integer_
  status <- if (!length(k)) {
    "no_finite_k"
  } else if (k_range == 0L) {
    "stable"
  } else if (k_range == 1L) {
    "one_count_range_use_larger"
  } else {
    "unstable"
  }
  data.frame(
    cell_id = x$cell_id[[1L]],
    n = x$n[[1L]],
    guaranteed_content = x$guaranteed_content[[1L]],
    tolerance_confidence = x$tolerance_confidence[[1L]],
    seeds = nrow(x),
    min_retained_count = if (length(k)) min(k) else NA_integer_,
    max_retained_count = if (length(k)) max(k) else NA_integer_,
    unique_retained_counts = length(unique(k)),
    stability_status = status,
    any_infeasible = any(x$infeasible),
    max_n_sim_total = max(x$n_sim_total, na.rm = TRUE),
    structural_statuses = paste(sort(unique(x$structural_status)),
                                collapse = ";"),
    stringsAsFactors = FALSE
  )
}))
rownames(stability_summary) <- NULL

join_keys <- c("cell_id", "n", "guaranteed_content", "tolerance_confidence")
comparison <- merge(
  old, adaptive, by = join_keys, suffixes = c("_old", "_adaptive"),
  all = TRUE
)
comparison <- merge(
  comparison,
  stability_summary[, c("cell_id", "min_retained_count",
                        "max_retained_count", "unique_retained_counts",
                        "stability_status", "any_infeasible")],
  by = "cell_id", all.x = TRUE
)
comparison$delta_k <- comparison$retained_count_adaptive -
  comparison$retained_count_old
comparison$delta_target_content <- comparison$target_content_adaptive -
  comparison$target_content_old
comparison$delta_content_buffer <- comparison$content_buffer_adaptive -
  comparison$content_buffer_old
adaptive_infeasible <- !is.na(comparison$infeasible_adaptive) &
  as.logical(comparison$infeasible_adaptive)
comparison$promotion_relevance <- ifelse(
  adaptive_infeasible,
  "reject_adaptive_infeasible",
  ifelse(
    !is.finite(comparison$delta_k),
    "incomplete_comparison",
    ifelse(
      comparison$delta_k < 0,
      "candidate_sharper",
      ifelse(comparison$delta_k == 0,
             "unchanged",
             "investigate_more_conservative")
    )
  )
)
comparison <- comparison[order(comparison$n, comparison$guaranteed_content,
                               comparison$tolerance_confidence), ,
                         drop = FALSE]

changed <- comparison[is.finite(comparison$delta_k) &
                        comparison$delta_k != 0, , drop = FALSE]
sharper <- comparison[is.finite(comparison$delta_k) &
                        comparison$delta_k < 0, , drop = FALSE]
unstable <- stability_summary[
  stability_summary$stability_status == "unstable", , drop = FALSE
]
gate_status <- if (nrow(unstable)) {
  "hold_unstable_calibration"
} else if (!nrow(changed)) {
  "no_validation_relaunch_needed"
} else if (nrow(sharper)) {
  "targeted_tcsp_relaunch_recommended"
} else {
  "investigate_before_promotion"
}

utils::write.csv(cells, file.path(output_dir, "article_design_cells.csv"),
                 row.names = FALSE)
utils::write.csv(old, file.path(output_dir, "old_boundary_map.csv"),
                 row.names = FALSE)
utils::write.csv(adaptive, file.path(output_dir, "adaptive_boundary_map.csv"),
                 row.names = FALSE)
utils::write.csv(stability,
                 file.path(output_dir, "adaptive_stability_by_seed.csv"),
                 row.names = FALSE)
utils::write.csv(stability_summary,
                 file.path(output_dir, "adaptive_stability_summary.csv"),
                 row.names = FALSE)
utils::write.csv(comparison,
                 file.path(output_dir, "old_vs_adaptive_comparison.csv"),
                 row.names = FALSE)

manifest <- list(
  schema_version = "rqrgibbs_tcsp_scan_calibration_promotion/1.0.0",
  generated_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  repo_root = repo_root,
  script_path = script_path,
  script_sha256 = sha256_file(script_path),
  git_commit = git_output(c("rev-parse", "HEAD"))[[1L]],
  git_status_short = git_output(c("status", "--short")),
  package_version = as.character(utils::packageVersion("rqrgibbs")),
  config_path = config_path,
  config_sha256 = sha256_file(config_path),
  mode = mode,
  old_method = old_method,
  new_method = new_method,
  n_design_cells = nrow(cells),
  n_sim = n_sim,
  numerical_confidence = numerical_confidence,
  seed = seed,
  seeds = seeds,
  workers = workers,
  adaptive_control = adaptive_control,
  changed_cells = nrow(changed),
  sharper_cells = nrow(sharper),
  unstable_cells = nrow(unstable),
  gate_status = gate_status,
  outputs = list(
    article_design_cells = "article_design_cells.csv",
    old_boundary_map = "old_boundary_map.csv",
    adaptive_boundary_map = "adaptive_boundary_map.csv",
    adaptive_stability_by_seed = "adaptive_stability_by_seed.csv",
    adaptive_stability_summary = "adaptive_stability_summary.csv",
    old_vs_adaptive_comparison = "old_vs_adaptive_comparison.csv"
  )
)
jsonlite::write_json(
  manifest, file.path(output_dir, "manifest.json"),
  auto_unbox = TRUE, pretty = TRUE
)

readme <- c(
  "# TCSP Scan Calibration Promotion Audit",
  "",
  paste0("- Old method: `", old_method, "`"),
  paste0("- Candidate method: `", new_method, "`"),
  paste0("- Mode: `", mode, "`"),
  paste0("- Design cells: `", nrow(cells), "`"),
  paste0("- Numerical confidence: `", numerical_confidence, "`"),
  paste0("- Base n_sim: `", n_sim, "`"),
  paste0("- Gate status: `", gate_status, "`"),
  "",
  "This audit is scan-only. It compares retained-count calibration and seed",
  "stability before any expensive tolerance-validation relaunch. It does not",
  "fit Young-Mathew, Wilks, MTI, Gibbs, ECM, DP, or DPM interval models.",
  "",
  "Promotion requires certified lower probabilities, stable retained counts,",
  "and a material retained-count change in manuscript cells."
)
writeLines(readme, file.path(output_dir, "README.md"))

cat("TCSP scan-calibration promotion audit written to:\n")
cat(output_dir, "\n")
cat("Gate status:", gate_status, "\n")
