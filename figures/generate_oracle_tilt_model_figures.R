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
    file.path(
      repo_root, "figures", "data", "oracle_tilt_c095_v5_exact_delta"
    )
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
    !all(as.numeric(unname(file.info(paths)$size)) ==
           as.numeric(manifest$bytes)) ||
    !all(
      tolower(unname(vapply(paths, oti_file_sha256, character(1L)))) ==
        tolower(manifest$sha256)
    )) {
  oti_stop("Compact evidence failed byte-count or SHA-256 verification.")
}
receipt <- jsonlite::read_json(
  file.path(evidence_dir, "evidence_receipt.json"), simplifyVector = TRUE
)
v3_schema <- "rqrgibbs_oracle_tilt_evidence/3.2.0"
v5_schema <- "rqrgibbs_oracle_tilt_evidence/5.1.0"
if (!receipt$schema_version %in% c(v3_schema, v5_schema)) {
  oti_stop("The compact evidence schema is not authorized for rendering.")
}
if (!isTRUE(receipt$manuscript_illustration_evidence_eligible) ||
    !isTRUE(receipt$exact_population_oracle_tilts) ||
    isTRUE(receipt$cornish_fisher_used)) {
  oti_stop("The evidence receipt does not authorize figure rendering.")
}
if (identical(receipt$schema_version, v3_schema)) {
  if (!isTRUE(receipt$all_cells_accepted_for_illustration)) {
    oti_stop("The historical version-3 evidence is not fully accepted.")
  }
} else {
  expected_v5 <- c(
    identical(
      receipt$source_commit,
      "24065941c44a836d2f385b9fe4cf28fcd18d08bd"
    ),
    identical(
      receipt$config_sha256,
      "e0a603d05e01aecc8f6402d3303d90f62de20b4cdee1fa69f7118419b438f893"
    ),
    identical(
      receipt$runtime_tree_digest,
      "20ca720b6d0874b11cdab342fcdfddd9be3c271fe81c5724ea6c3ca43a9c3614"
    ),
    identical(receipt$oracle_schema, "rqrgibbs_interval_oracle/2.0.0"),
    identical(
      receipt$tilt_definition,
      "conditional_retained_mean_minus_population_mean"
    ),
    identical(as.integer(receipt$target_cells), 6L),
    identical(as.integer(receipt$completed_chains), 27L),
    identical(as.integer(receipt$strict_pass_cells), 5L),
    identical(as.integer(receipt$diagnostic_warning_cells), 1L),
    isTRUE(receipt$all_cells_hard_computational_pass),
    identical(receipt$all_cells_strict_pass, FALSE),
    identical(receipt$strict_diagnostic_thresholds_relabelled, FALSE),
    identical(receipt$reseeded_or_selectively_extended, FALSE),
    identical(receipt$legacy_oracle_schemas_authorized, FALSE),
    identical(receipt$response_predictive_analysis, FALSE),
    identical(receipt$simulation_study, FALSE)
  )
  if (!all(expected_v5)) {
    oti_stop("The corrected version-5 evidence contract is inconsistent.")
  }
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
              cell_key(required_cells)) ||
    nrow(summary) != 6L ||
    !setequal(cell_key(summary[c("family", "target")]),
              cell_key(required_cells))) {
  oti_stop("Figure evidence does not contain the six required cells.")
}

figure_specs <- list(
  fixed_design = list(
    fit = "fig04_fixed_design_oracle_tilt_c095",
    error = "figS03_fixed_design_endpoint_error_c095",
    title = "Nonlinear heteroscedastic interval-root fits at 95% content",
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
    title = "Seasonal dynamic interval-root fits at 95% content",
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
  summary$family == "fixed_design", "Fixed design", "Dynamic roots"
)
target_label <- oti_display_target(summary$target)
if (identical(receipt$schema_version, v3_schema)) {
  allowed_dispositions <- c("strict_pass", "accepted_revised_tolerance")
  disposition <- summary$promotion_disposition
  warning_disposition <- "accepted_revised_tolerance"
  warning_label <- "Accepted (0.21)"
} else {
  allowed_dispositions <- c("strict_pass", "diagnostic_aware_pass")
  disposition <- summary$disposition
  warning_disposition <- "diagnostic_aware_pass"
  warning_label <- "Diagnostic-aware"
  if (!all(summary$hard_computational_pass) ||
      !all(summary$broad_recovery_pass) ||
      !all(summary$broad_heterogeneity_pass) ||
      !all(summary$completion_eligible) ||
      !all(summary$manuscript_illustration_evidence_eligible) ||
      sum(summary$diagnostic_warning_count) != 5L) {
    oti_stop("The version-5 six-cell completion evidence is invalid.")
  }
}
if (length(disposition) != nrow(summary) ||
    !all(disposition %in% allowed_dispositions) ||
    sum(disposition == warning_disposition) != 1L ||
    !identical(
      paste(
        summary$family[disposition == warning_disposition],
        summary$target[disposition == warning_disposition], sep = "/"
      ), "dlm/SH"
    )) {
  oti_stop("The six-cell illustration disposition is invalid.")
}
disposition_label <- ifelse(
  disposition == "strict_pass", "Strict", warning_label
)
rows <- sprintf(
  "%s & %s & %.3f & %.3f & %.3f & %.3f & %s \\\\",
  family_label, target_label,
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
      "& Mean width & Disposition \\\\"
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
manifest_out$evidence_schema <- receipt$schema_version
manifest_out$evidence_source_commit <- receipt$source_commit
write.csv(
  manifest_out,
  file.path(output_dir, "oracle_tilt_c095_figure_manifest.csv"),
  row.names = FALSE
)
message("[oracle-tilt-figures] rendered ", length(written), " artifacts.")
