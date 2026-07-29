#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_path <- sub(file_arg, "", args[startsWith(args, file_arg)][1L])
if (!length(script_path) || is.na(script_path)) {
  script_path <- "application/scripts/32_run_oracle_tilt_illustrations.R"
}
script_dir <- dirname(normalizePath(script_path, mustWork = TRUE))
source(file.path(script_dir, "32_oracle_tilt_illustration_utils.R"))

trailing <- commandArgs(trailingOnly = TRUE)
arg_value <- function(prefix, default = NULL) {
  hit <- trailing[startsWith(trailing, prefix)]
  if (length(hit)) sub(prefix, "", hit[1L], fixed = TRUE) else default
}
has_flag <- function(flag) any(trailing %in% flag)

config_path <- arg_value(
  "--config=",
  "application/config/oracle_tilt_illustrations_20260728.json"
)
output_dir_arg <- arg_value("--output-dir=", NULL)
families_arg <- arg_value("--families=", NULL)
figure_output_dir_arg <- arg_value("--figure-output-dir=", NULL)
dry_run <- has_flag("--dry-run")
quick <- has_flag("--quick")
strict_desn <- has_flag("--strict-desn")
paper_figures <- has_flag("--paper-figures")
one_chain <- has_flag("--one-chain")

repo_root <- normalizePath(file.path(script_dir, "..", ".."), mustWork = TRUE)
setwd(repo_root)
config <- oti_read_json(config_path)
coverage_level <- oti_scalar(config$coverage_level %||% 0.8, "coverage_level",
                             1e-8, 1 - 1e-8)
config$coverage_level <- coverage_level
run_control <- oti_run_control(
  config, quick = quick, paper_figures = paper_figures, one_chain = one_chain
)
targets <- oti_normalize_targets(config$targets %||% c("RQR", "ET", "SH"))
families <- if (is.null(families_arg)) {
  tolower(as.character(config$families %||% c("fixed_design", "dlm")))
} else {
  tolower(trimws(strsplit(families_arg, ",", fixed = TRUE)[[1L]]))
}
families <- unique(families[nzchar(families)])
bad_families <- setdiff(families, c("fixed_design", "dlm", "desn"))
if (length(bad_families)) {
  oti_stop("Unknown illustration families: ", paste(bad_families, collapse = ", "))
}
if ("desn" %in% families && !strict_desn && !oti_exdqlm_available_for_desn()) {
  message(
    "[oracle-tilt] DESN requested but pinned exdqlm runtime is unavailable; ",
    "recording skipped status. Use --strict-desn to fail instead."
  )
  families <- setdiff(families, "desn")
  desn_skipped <- TRUE
} else {
  desn_skipped <- FALSE
}

if (!requireNamespace("rqrgibbs", quietly = TRUE)) {
  oti_stop("The rqrgibbs package must be installed before running fits.")
}

run_id <- arg_value("--run-id=", oti_now_id())
output_root <- output_dir_arg %||%
  file.path("application", "outputs", "oracle_tilt_illustrations", run_id)
output_root <- oti_ensure_dir(output_root)
figure_dir <- oti_ensure_dir(figure_output_dir_arg %||% file.path(output_root, "figures"))

law <- oti_law_from_config(config)
oracle_targets <- oti_oracle_targets(law, coverage_level, targets)
fit_plan <- oti_plan_rows(
  c(families, if (desn_skipped) "desn_skipped" else character(0)),
  targets,
  run_control = run_control
)

paths <- character(0)
paths <- c(paths, oti_write_json(config, file.path(output_root, "config.json")))
paths <- c(paths, oti_write_json(
  list(
    schema_version = oti_run_schema(),
    run_id = run_id,
    dry_run = dry_run,
    quick = quick,
    paper_figures = paper_figures,
    one_chain = one_chain,
    run_control = run_control,
    repo_state = oti_git_state(repo_root),
    config_path = normalizePath(config_path, mustWork = TRUE),
    law = list(
      family = law$family,
      tau = law$tau,
      standardized = law$standardized,
      mean = law$mean,
      sd = law$sd
    )
  ),
  file.path(output_root, "source_state.json")
))
paths <- c(paths, oti_write_json(
  oti_runtime_state(
    repo_root = repo_root,
    config_path = config_path,
    script_path = normalizePath(script_path, mustWork = TRUE),
    args = trailing,
    run_control = run_control
  ),
  file.path(output_root, "runtime_state.json")
))
paths <- c(paths, oti_write_csv(
  oracle_targets, file.path(output_root, "oracle_targets.csv")
))
paths <- c(paths, oti_write_csv(fit_plan, file.path(output_root, "fit_plan.csv")))

all_summaries <- list()
all_curves <- list()
all_error_densities <- list()
all_error_summaries <- list()
all_endpoint_errors_by_index <- list()
all_chain_summaries <- list()
all_mcmc_diagnostics <- list()
skip_rows <- list()

if (dry_run) {
  message("[oracle-tilt] dry run completed: oracle targets and fit plan only.")
} else {
  if ("fixed_design" %in% families) {
    dgp <- oti_fixed_design_dgp(config, law)
    tbi <- oti_targets_by_index(
      dgp$mean_truth, dgp$scale_truth, oracle_targets, dgp$observed
    )
    for (target in targets) {
      message("[oracle-tilt] fixed_design / ", target)
      res <- oti_fit_fixed_design_target(
        dgp, tbi, target, config, quick = quick, run_control = run_control
      )
      all_summaries[[length(all_summaries) + 1L]] <- res$summary
      all_curves[[length(all_curves) + 1L]] <- res$curves
      all_error_densities[[length(all_error_densities) + 1L]] <- res$error_density
      all_error_summaries[[length(all_error_summaries) + 1L]] <- res$error_summary
      all_endpoint_errors_by_index[[length(all_endpoint_errors_by_index) + 1L]] <-
        res$endpoint_error_by_index
      all_chain_summaries[[length(all_chain_summaries) + 1L]] <- res$chain_summary
      if (nrow(res$mcmc_diagnostics)) {
        all_mcmc_diagnostics[[length(all_mcmc_diagnostics) + 1L]] <-
          res$mcmc_diagnostics
      }
    }
  }
  if ("dlm" %in% families) {
    dgp <- oti_dlm_dgp(config, law)
    tbi <- oti_targets_by_index(
      dgp$mean_truth, dgp$scale_truth, oracle_targets, dgp$observed
    )
    for (target in targets) {
      message("[oracle-tilt] dlm / ", target)
      res <- oti_fit_dlm_target(
        dgp, tbi, target, config, quick = quick, run_control = run_control
      )
      all_summaries[[length(all_summaries) + 1L]] <- res$summary
      all_curves[[length(all_curves) + 1L]] <- res$curves
      all_error_densities[[length(all_error_densities) + 1L]] <- res$error_density
      all_error_summaries[[length(all_error_summaries) + 1L]] <- res$error_summary
      all_endpoint_errors_by_index[[length(all_endpoint_errors_by_index) + 1L]] <-
        res$endpoint_error_by_index
      all_chain_summaries[[length(all_chain_summaries) + 1L]] <- res$chain_summary
      if (nrow(res$mcmc_diagnostics)) {
        all_mcmc_diagnostics[[length(all_mcmc_diagnostics) + 1L]] <-
          res$mcmc_diagnostics
      }
    }
  }
  if ("desn" %in% families) {
    dgp <- oti_desn_dgp(config, law)
    tbi <- oti_targets_by_index(
      dgp$mean_truth, dgp$scale_truth, oracle_targets, dgp$observed
    )
    for (target in targets) {
      message("[oracle-tilt] desn / ", target)
      res <- oti_fit_desn_target(dgp, tbi, target, config, quick = quick)
      all_summaries[[length(all_summaries) + 1L]] <- res$summary
      all_curves[[length(all_curves) + 1L]] <- res$curves
      all_error_densities[[length(all_error_densities) + 1L]] <- res$error_density
      all_error_summaries[[length(all_error_summaries) + 1L]] <- res$error_summary
    }
  }
}

if (desn_skipped) {
  skip_rows[[length(skip_rows) + 1L]] <- data.frame(
    family = "desn",
    target = targets,
    fit_status = "skipped_prerequisite_missing",
    reason = "pinned exdqlm runtime exporting qdesn_fit_vb was unavailable",
    stringsAsFactors = FALSE
  )
}

if (length(all_summaries)) {
  summary <- oti_rbind_fill(all_summaries)
  paths <- c(paths, oti_write_csv(summary, file.path(output_root, "fit_summary.csv")))
}
if (length(skip_rows)) {
  skips <- oti_rbind_fill(skip_rows)
  paths <- c(paths, oti_write_csv(skips, file.path(output_root, "fit_skips.csv")))
}
if (length(all_chain_summaries)) {
  chain_summary <- oti_rbind_fill(all_chain_summaries)
  paths <- c(paths, oti_write_csv(
    chain_summary, file.path(output_root, "chain_summary.csv")
  ))
  paths <- c(paths, oti_write_csv(
    chain_summary[, c("family", "target", "chain", "seed"), drop = FALSE],
    file.path(output_root, "seed_ledger.csv")
  ))
}
if (length(all_mcmc_diagnostics)) {
  mcmc_diagnostics <- oti_rbind_fill(all_mcmc_diagnostics)
  paths <- c(paths, oti_write_csv(
    mcmc_diagnostics, file.path(output_root, "mcmc_diagnostics.csv")
  ))
  quality_gates <- aggregate(
    pass ~ family + target,
    data = mcmc_diagnostics,
    FUN = function(x) all(as.logical(x))
  )
  names(quality_gates)[names(quality_gates) == "pass"] <- "diagnostics_pass"
  paths <- c(paths, oti_write_csv(
    quality_gates, file.path(output_root, "figure_quality_gates.csv")
  ))
}
if (length(all_curves)) {
  curves <- do.call(rbind, all_curves)
  paths <- c(paths, oti_write_csv(curves, file.path(output_root, "fit_curves.csv")))
  for (fam in unique(curves$family)) {
    fcurves <- curves[curves$family == fam, , drop = FALSE]
    fig_name <- if (isTRUE(paper_figures)) {
      switch(
        fam,
        fixed_design = "fig04_oracle_tilt_fixed_design_fit.png",
        dlm = "fig05_oracle_tilt_dlm_fit.png",
        desn = "fig08_oracle_tilt_desn_fit.png",
        paste0("oracle_tilt_", fam, ".png")
      )
    } else {
      paste0("oracle_tilt_", fam, ".png")
    }
    file <- file.path(figure_dir, fig_name)
    title <- switch(
      fam,
      fixed_design = "Fixed-design mean-tilted RQR illustration",
      dlm = "Dynamic linear root mean-tilted RQR illustration",
      desn = "Reservoir readout mean-tilted RQR illustration",
      paste("Mean-tilted RQR illustration:", fam)
    )
    xlab <- if (identical(fam, "fixed_design")) "Covariate x" else "Time"
    note <- switch(
      fam,
      fixed_design = "Intervals compare population-oracle endpoints with generalized-posterior endpoint summaries from one illustrative data set.",
      dlm = "Missing responses are omitted from the loss and tilt sites; magenta ticks mark omitted response times.",
      desn = "The DESN panel uses a frozen deterministic feature design and the same static readout scan.",
      NULL
    )
    paths <- c(paths, oti_plot_curve_panels(
      fcurves, file, title, xlab = xlab, caption_note = note
    ))
  }
}

if (length(all_error_densities)) {
  error_density <- oti_rbind_fill(all_error_densities)
  paths <- c(paths, oti_write_csv(
    error_density, file.path(output_root, "endpoint_error_density.csv")
  ))
  error_summary <- oti_rbind_fill(all_error_summaries)
  paths <- c(paths, oti_write_csv(
    error_summary, file.path(output_root, "endpoint_error_summary.csv")
  ))
  for (fam in unique(error_density$family)) {
    ferr <- error_density[error_density$family == fam, , drop = FALSE]
    fig_name <- if (isTRUE(paper_figures)) {
      switch(
        fam,
        fixed_design = "fig06_oracle_tilt_fixed_design_endpoint_errors.png",
        dlm = "fig07_oracle_tilt_dlm_endpoint_errors.png",
        desn = "fig09_oracle_tilt_desn_endpoint_errors.png",
        paste0("oracle_tilt_", fam, "_endpoint_errors.png")
      )
    } else {
      paste0("oracle_tilt_", fam, "_endpoint_errors.png")
    }
    title <- switch(
      fam,
      fixed_design = "Fixed-design endpoint error distributions",
      dlm = "Dynamic linear root endpoint error distributions",
      desn = "Reservoir readout endpoint error distributions",
      paste("Endpoint error distributions:", fam)
    )
    paths <- c(paths, oti_plot_endpoint_error_panels(
      ferr, file.path(figure_dir, fig_name), title,
      xlab = expression(fitted~endpoint~draw - population~oracle~endpoint)
    ))
  }
}
if (length(all_endpoint_errors_by_index)) {
  endpoint_error_by_index <- oti_rbind_fill(all_endpoint_errors_by_index)
  paths <- c(paths, oti_write_csv(
    endpoint_error_by_index,
    file.path(output_root, "endpoint_error_by_index.csv")
  ))
}

manifest <- oti_artifact_manifest(paths, root = repo_root)
manifest_path <- file.path(output_root, "artifact_manifest.csv")
oti_write_csv(manifest, manifest_path)

message("[oracle-tilt] wrote artifacts under: ", output_root)
message("[oracle-tilt] manifest: ", manifest_path)
