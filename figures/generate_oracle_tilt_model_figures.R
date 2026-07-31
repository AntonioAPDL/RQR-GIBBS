#!/usr/bin/env Rscript

arguments <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_path <- sub(file_arg, "", arguments[startsWith(arguments, file_arg)][1L])
if (!length(script_path) || is.na(script_path)) {
  script_path <- "figures/generate_oracle_tilt_model_figures.R"
}
repo_root <- normalizePath(
  file.path(dirname(normalizePath(script_path, mustWork = TRUE)), ".."),
  winslash = "/", mustWork = TRUE
)
source(file.path(
  repo_root, "application", "scripts",
  "32_oracle_tilt_illustration_utils.R"
))

trailing <- commandArgs(trailingOnly = TRUE)
arg_value <- function(prefix, default = NULL) {
  hit <- trailing[startsWith(trailing, prefix)]
  if (length(hit)) sub(prefix, "", hit[1L], fixed = TRUE) else default
}
evidence_dir <- normalizePath(
  arg_value(
    "--evidence-dir=",
    file.path(repo_root, "figures", "data", "oracle_tilt_c095")
  ),
  winslash = "/", mustWork = TRUE
)
output_dir <- normalizePath(
  arg_value("--output-dir=", file.path(repo_root, "figures", "generated")),
  winslash = "/", mustWork = FALSE
)
table_dir <- normalizePath(
  arg_value("--table-dir=", file.path(repo_root, "tables")),
  winslash = "/", mustWork = FALSE
)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

manifest <- utils::read.csv(
  file.path(evidence_dir, "evidence_manifest.csv"),
  stringsAsFactors = FALSE, check.names = FALSE
)
if (!all(c("path", "bytes", "sha256") %in% names(manifest))) {
  oti_stop("The compact evidence manifest is invalid.")
}
paths <- file.path(evidence_dir, manifest$path)
if (any(!file.exists(paths)) ||
    !identical(as.numeric(file.info(paths)$size), as.numeric(manifest$bytes)) ||
    !identical(
      tolower(vapply(paths, oti_file_sha256, character(1L))),
      tolower(manifest$sha256)
    )) {
  oti_stop("Compact evidence failed byte-count or SHA-256 verification.")
}
receipt <- jsonlite::read_json(
  file.path(evidence_dir, "evidence_receipt.json"), simplifyVector = TRUE
)
if (!isTRUE(receipt$manuscript_illustration_evidence_eligible) ||
    !isTRUE(receipt$exact_population_oracle_tilts) ||
    isTRUE(receipt$cornish_fisher_used)) {
  oti_stop("The evidence receipt does not authorize figure rendering.")
}
curves <- utils::read.csv(
  file.path(evidence_dir, "fit_curves.csv"), stringsAsFactors = FALSE
)
errors <- utils::read.csv(
  file.path(evidence_dir, "endpoint_error_density.csv"),
  stringsAsFactors = FALSE
)
summary <- utils::read.csv(
  file.path(evidence_dir, "fit_summary.csv"), stringsAsFactors = FALSE
)
required_cells <- expand.grid(
  family = c("fixed_design", "dlm"),
  target = c("RQR", "ET", "SH"),
  stringsAsFactors = FALSE
)
cell_key <- function(x) paste(x$family, x$target, sep = "/")
if (!setequal(cell_key(unique(curves[c("family", "target")])),
              cell_key(required_cells)) ||
    !setequal(cell_key(unique(errors[c("family", "target")])),
              cell_key(required_cells))) {
  oti_stop("Figure evidence does not contain the six required cells.")
}

figure_specs <- list(
  fixed_design = list(
    fit = "fig04_fixed_design_oracle_tilt_c095",
    error = "figS03_fixed_design_endpoint_error_c095",
    title = "Fixed-design interval-root fits at 95% content",
    error_title = "Fixed-design endpoint errors at 95% content",
    xlab = "Covariate x",
    note = paste(
      "Blue curves are population-oracle endpoints; orange curves and",
      "ribbons are generalized-posterior means and central 95% summaries."
    )
  ),
  dlm = list(
    fit = "fig05_dlm_oracle_tilt_c095",
    error = "figS04_dlm_endpoint_error_c095",
    title = "Dynamic linear interval-root fits at 95% content",
    error_title = "Dynamic linear endpoint errors at 95% content",
    xlab = "Time",
    note = paste(
      "Missing responses are omitted from the loss and tilt sites and are",
      "marked by magenta bands and triangles."
    )
  )
)
written <- character(0)
for (family in names(figure_specs)) {
  spec <- figure_specs[[family]]
  family_curves <- curves[curves$family == family, , drop = FALSE]
  family_errors <- errors[errors$family == family, , drop = FALSE]
  for (extension in c("pdf", "png")) {
    fit_file <- file.path(
      output_dir, paste0(spec$fit, ".", extension)
    )
    error_file <- file.path(
      output_dir, paste0(spec$error, ".", extension)
    )
    oti_plot_curve_panels(
      family_curves, fit_file, spec$title,
      xlab = spec$xlab, caption_note = spec$note
    )
    oti_plot_endpoint_error_panels(
      family_errors, error_file, spec$error_title,
      xlab = expression(
        generalized-posterior~endpoint~draw - population-oracle~endpoint
      )
    )
    written <- c(written, fit_file, error_file)
  }
}

family_label <- ifelse(
  summary$family == "fixed_design", "Fixed design", "Dynamic linear roots"
)
disposition_label <- ifelse(
  summary$disposition == "strict_pass", "Strict pass", "ESS warning"
)
rows <- sprintf(
  "%s & %s & %.3f & %.3f & %.3f & %.3f & %s \\\\",
  family_label, summary$target,
  summary$endpoint_rmse, summary$width_rmse,
  summary$realized_coverage, summary$mean_width,
  disposition_label
)
table_path <- file.path(table_dir, "oracle_tilt_c095_illustration_summary.tex")
writeLines(
  c(
    "% Generated by figures/generate_oracle_tilt_model_figures.R.",
    "\\begin{tabular}{@{}llrrrrl@{}}",
    "\\toprule",
    paste(
      "Family & Target & Endpoint RMSE & Width RMSE & Realized coverage",
      "& Mean width & Gate \\\\"
    ),
    "\\midrule",
    rows,
    "\\bottomrule",
    "\\end{tabular}"
  ),
  table_path
)
written <- c(written, table_path)
manifest_out <- oti_file_hashes(written, repo_root)
write.csv(
  manifest_out,
  file.path(output_dir, "oracle_tilt_c095_figure_manifest.csv"),
  row.names = FALSE
)
message("[oracle-tilt-figures] rendered ", length(written), " artifacts.")
