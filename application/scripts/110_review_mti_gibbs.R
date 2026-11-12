#!/usr/bin/env Rscript
arguments <- commandArgs(FALSE)
script <- sub("--file=", "", arguments[startsWith(arguments, "--file=")][1], fixed = TRUE)
setwd(normalizePath(file.path(dirname(script), "..", ".."), mustWork = TRUE))
Sys.setenv(OMP_NUM_THREADS = 1, OPENBLAS_NUM_THREADS = 1, MKL_NUM_THREADS = 1)
for (file in c("pharma_tolerance_application.R", "pharma_mti_application.R",
    "mti_profile_revision.R", "mti_profile_review.R", "mti_final_reporting.R",
    "mti_ecm_recovery.R", "pinned_exdqlm_runtime.R", "mti_gibbs_comparison.R",
    "mti_gibbs_practical.R", "mti_gibbs_review.R")) source(file.path("application/scripts/lib", file))
args <- commandArgs(TRUE)
cfg <- mgc_json(pta_arg_value(args, "--config=", "application/config/mti_gibbs_review_20261112.json"))
mode <- pta_arg_value(args, "--mode=", "verify")
if (mode == "verify") {
  provenance <- mgc_json(file.path(cfg$evidence_dir, "provenance.json"))
  for (name in names(c(provenance$artifacts, provenance$analysis_programs))) {
    expected <- c(provenance$artifacts, provenance$analysis_programs)[[name]]
    mpr_require(identical(mgc_file_hash(name), expected),
      paste("Review artifact changed:", name))
  }
  summary <- mgc_json(file.path(cfg$evidence_dir, "summary.json"))
  mpr_require(summary$datasets == 456L && summary$balanced_datasets == 370L &&
    summary$preferred_diagnostics == 456L && !summary$new_tolerance_guarantee &&
    !summary$replace_tolerance_results, "Unexpected posterior study scope or conclusion.")
  cat("Verified fixed-target posterior assessment artifacts.\n")
  quit(status = 0)
}
stopifnot(mode == "analyze")
practical <- mgc_json(cfg$practical_config); base <- mgc_json(practical$base_config)
common <- normalizePath(pma_git("rev-parse", "--git-common-dir"), mustWork = TRUE)
directory <- normalizePath(file.path(dirname(common), cfg$practical_directory), mustWork = TRUE)
parent <- normalizePath(file.path(dirname(common), practical$parent_directory), mustWork = TRUE)
library <- file.path(dirname(common), ".codex_work", paste0("mti_gibbs_launch_", practical$parent_commit),
  "application/cache", paste0("mti_gibbs_runtime_", practical$parent_commit), "library")
.libPaths(c(normalizePath(library, mustWork = TRUE), .libPaths()))
runtime <- mgc_runtime(practical$parent_commit)
manifest <- mgc_json(file.path(directory, "manifest.json"))
parent_manifest <- mgc_json(file.path(parent, "manifest.json"))
status <- mgc_json(file.path(directory, "campaign_status.json"))
jobs <- mgc_jobs(file.path(directory, "slice"))
parent_jobs <- mgc_jobs(parent)
extension_jobs <- mgc_jobs(file.path(directory, "continuations"))
mpr_require(identical(manifest$identity$commit, cfg$practical_commit) &&
  identical(manifest$identity$settings_sha256, mgc_hash(practical)) &&
  identical(manifest$identity$base_sha256, mgc_hash(base)) &&
  identical(manifest$identity$design_sha256, mgc_hash(jobs)) &&
  identical(manifest$identity$parent_design_sha256, mgc_hash(parent_jobs)) &&
  identical(manifest$identity$inputs_sha256, mgc_hash(mgc_inputs(base))) &&
  identical(manifest$identity$continuation_runtime_sha256, runtime$sha256) &&
  identical(manifest$identity$parent_manifest_sha256, mgc_file_hash(file.path(parent, "manifest.json"))) &&
  identical(status$status, "calculation_complete_review_required"), "Completed study identity mismatch.")
invisible(rqr_assert_external_checkout_unchanged(parent_manifest$external_reference))
dir.create(cfg$evidence_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(cfg$output_dir, recursive = TRUE, showWarnings = FALSE)
context <- mgc_context(base)
saved <- read.csv(file.path(directory, "slice/parameter_summary.csv"))
old <- read.csv(file.path(parent, "posterior_summary.csv"))
started <- Sys.time()
groups <- split(jobs, jobs$dataset_id)
rows <- parallel::mclapply(groups, mgr_case, cfg = cfg, practical = practical, base = base,
  directory = directory, parent = parent, parent_jobs = parent_jobs, extension_jobs = extension_jobs,
  saved_summary = saved, original_summary = old, context = context, identity = manifest$identity,
  mc.cores = cfg$workers, mc.set.seed = FALSE)
mpr_require(!any(vapply(rows, inherits, logical(1), "try-error")), "A posterior review case failed.")
bind <- function(name) do.call(rbind, Filter(Negate(is.null), lapply(rows, "[[", name)))
datasets <- bind("dataset"); parameters <- bind("parameters"); sensitivity <- bind("sensitivity")
audit <- bind("audit"); agreement <- bind("agreement")
expected <- nrow(jobs) + nrow(parent_jobs) + nrow(extension_jobs)
mpr_require(nrow(audit) == expected && all(audit$repairs == 0L) &&
  nrow(datasets) == length(groups), "Incomplete posterior review.")
for (name in c("datasets", "parameters", "sensitivity", "audit", "agreement")) {
  pma_write_csv(get(name), file.path(cfg$evidence_dir, paste0(name, ".csv")))
}
overview <- mgr_overview(datasets)
pma_write_csv(overview, file.path(cfg$evidence_dir, "overview.csv"))
figure <- "figures/generated/figS_mti_fixed_target_width.pdf"
mgr_width_figure(datasets, jobs, directory, cfg$example_case_ids, figure)
mgr_width_figure(datasets, jobs, directory, cfg$example_case_ids,
  file.path(cfg$output_dir, "posterior_width_review.pdf"))
common_reliable <- agreement[agreement$reference_sampler == "gaussian_gig_original" &
  agreement$reference_reliable, ]
summary <- list(study = cfg$study_id, datasets = nrow(datasets),
  balanced_datasets = sum(datasets$diagnostic_role == "balanced"),
  simulation_datasets = sum(datasets$source == "simulation"),
  application_datasets = sum(datasets$source == "application"),
  chains = nrow(jobs), audited_chain_records = nrow(audit),
  maximum_rhat = max(parameters$rhat), minimum_bulk_ess = min(parameters$ess_bulk),
  minimum_tail_ess = min(parameters$ess_tail),
  preferred_diagnostics = sum(datasets$preferred_diagnostics),
  means_at_reporting_precision = sum(datasets$mean_precision),
  medians_at_reporting_precision = sum(datasets$median_precision),
  credible_endpoints_at_reporting_precision = sum(datasets$credible_endpoint_precision),
  target_classes = as.data.frame(table(datasets$diagnostic_role, datasets$source, datasets$target_class)),
  reliable_original_datasets = length(unique(common_reliable$dataset_id)),
  reliable_original_comparisons = nrow(common_reliable),
  reliable_original_within_two_mcse = sum(abs(common_reliable$standardized_difference) <= 2),
  reliable_original_maximum_standardized_difference = max(abs(common_reliable$standardized_difference)),
  maximum_reference_objective_gap = max(datasets$reference_relative_objective_gap),
  ecm_relative_objective_gap_above_1e_minus4 = sum(datasets$ecm_relative_objective_gap > 1e-4),
  elapsed_analysis_seconds = as.numeric(difftime(Sys.time(), started, units = "secs")),
  new_tolerance_guarantee = FALSE, replace_tolerance_results = FALSE)
pta_atomic_write_json(summary, file.path(cfg$evidence_dir, "summary.json"))

main <- sprintf(paste0("\\paragraph{Fixed-target posterior assessment.}\n",
  "An additional computation study used five samples in each of the 72 simulation settings\n",
  "in Section~\\ref{sec:evaluation}, ten application training partitions, and 86\n",
  "additional samples with extreme fitted contents or tilts. Holding the selected\n",
  "\\(q,\\delta\\) and Gaussian priors fixed, we sampled the root posterior with\n",
  "the intercept-only marginal slice updates described in the supplement.\n",
  "Each of four\n",
  "chains retained 4000 draws after 2000 burn-in cycles. Across all %d samples,\n",
  "the largest rank-normalized \\(\\widehat R\\) was %.3f and the smallest bulk\n",
  "effective sample size was %d \\citep{VehtariEtAl2021Rhat}.\n",
  "Posterior width distributions can be broad when the selected tilt is near an\n",
  "empirical limiting value. Independent minimization of the same penalized\n",
  "loss also reveals objective gaps in some finite-iteration ECM estimates.\n",
  "These findings call for prior and optimization sensitivity assessment\n",
  "before using posterior summaries to define new tolerance intervals.\n",
  "The tolerance comparison below continues\n",
  "to use the specified ECM endpoint estimates.\n"), nrow(datasets),
  summary$maximum_rhat, floor(summary$minimum_bulk_ess))
writeLines(main, "tables/mti_gibbs_comparison_main.tex")
supplement <- sprintf(paste0("\\subsection{Fixed-Target Posterior Assessment}\n",
  "\\label{sec:supp-mti-posterior-assessment}\n",
  "The computation study contains 360 simulation samples, with five in each\n",
  "distribution and sample-size/content setting, and ten application training\n",
  "partitions, one for each response and partition scheme. An additional 86\n",
  "samples retain the extreme-content and extreme-tilt cases examined with\n",
  "the augmented sampler. All calculations use the archived ECM-selected\n",
  "\\(q,\\delta\\), \\(\\omega_{\\mathrm R}=1\\), and \\(\\lambda=10^{-4}\\).\n",
  "Four marginal slice-within-Gibbs chains use 2000 burn-in and 4000 retained\n",
  "iterations per sample. Every one of the %d samples has \\(\\widehat R<1.01\\),\n",
  "bulk and tail effective sample sizes above 400, and mean Monte Carlo error\n",
  "below 5\\%% of posterior standard deviation. The largest \\(\\widehat R\\)\n",
  "is %.4f; the smallest bulk and tail effective sample sizes are %d and %d.\n",
  "These diagnostics use ordered endpoints, width, and midpoint\n",
  "\\citep{VehtariEtAl2021Rhat}. Quantile-specific Monte Carlo errors are\n",
  "recorded separately. All primary means, medians, and credible endpoints\n",
  "meet the additional 1\\%%-of-median-width reporting resolution in %d samples;\n",
  "the remaining samples retain their numerical-error summaries for review.\n\n",
  "The original augmented sampler was run on 174 of these samples, of which\n",
  "20 met its original diagnostic criteria at the same chain length. One\n",
  "4000-iteration extension per chain was applied to 18 borderline samples.\n",
  "The marginal updates preserve the same generalized posterior while reducing\n",
  "the dependence associated with latent-scale augmentation in these examples.\n",
  "Posterior summaries from poorly mixed augmented chains are retained as\n",
  "diagnostics. Comparisons of posterior means are restricted to original\n",
  "chain sets meeting the stated diagnostic criteria. Both samplers use\n",
  "identical observations and fixed targets.\n\n",
  "The archived ECM objective exceeds the independently minimized value by\n",
  "more than \\(10^{-4}\\max\\{1,|J_{\\mathrm{ECM}}|\\}\\) in %d samples.\n",
  "Of these, %d have tilts outside the empirical limits. This comparison\n",
  "assesses the optimization accuracy of the specified finite iteration;\n",
  "the independent reference attains a relative objective bound below\n",
  "\\(5\\times10^{-13}\\) in every sample. Among the 20 original augmented\n",
  "chain sets meeting their stated diagnostics, %d of 80 posterior-mean\n",
  "comparisons lie within twice the combined Monte Carlo standard error.\n",
  "The largest standardized difference is %.2f.\n\n",
  "Figure~\\ref{fig:supp-mti-posterior-width} separates a finite ECM iterate,\n",
  "a separately minimized penalized-loss reference, and posterior uncertainty\n",
  "at the same fixed target. This sample-dependent minimum serves only to\n",
  "assess optimization accuracy; the tolerance study retains its original\n",
  "ECM estimates. At an empirical limiting tilt, one endpoint can move beyond the\n",
  "data with little loss change, leaving the Gaussian prior to constrain\n",
  "posterior dispersion. Slightly outside the limiting range, further movement\n",
  "can reduce the unpenalized loss. These effects explain why improved mixing\n",
  "can reveal substantial width uncertainty even when a short ECM sequence\n",
  "appears stable. A change to the prior, learning rate, or target-selection\n",
  "rule requires a separately specified tolerance calibration.\n\n",
  "A local prior-sensitivity calculation reweights the saved draws to\n",
  "Gaussian root variances equal to one half and one tenth of the original\n",
  "variance. It records weight effective sample size and the Monte Carlo\n",
  "error of each weighted mean. The results show substantial prior influence\n",
  "in many limiting-tilt examples. Inadequate importance overlap leaves\n",
  "some sensitivity estimates unresolved; these estimates are excluded\n",
  "from substantive comparisons.\n\n",
  "\\begin{figure}[!htbp]\n\\centering\n",
  "\\includegraphics[width=\\textwidth]{figures/generated/figS_mti_fixed_target_width.pdf}\n",
  "\\caption{\\textbf{Posterior interval width at fixed content and tilt.}\n",
  "Each curve is the empirical posterior cumulative distribution of width\n",
  "from four marginal slice-within-Gibbs chains. Vertical lines mark the\n",
  "archived finite-iteration ECM width and a separately minimized value of\n",
  "the same penalized loss. All quantities within a panel use the same\n",
  "observations, fitted content, tilt, learning rate, and root priors. The\n",
  "horizontal axis is logarithmic; widths remain in the original response\n",
  "units. The selected examples illustrate interior, limiting, and\n",
  "outside-range empirical tilts. These are conditional posterior\n",
  "distributions, distinct from repeated-sampling tolerance assessments.}\n",
  "\\label{fig:supp-mti-posterior-width}\n\\end{figure}\n"),
  nrow(datasets), summary$maximum_rhat, floor(summary$minimum_bulk_ess),
  floor(summary$minimum_tail_ess), sum(datasets$mean_precision & datasets$median_precision &
    datasets$credible_endpoint_precision), summary$ecm_relative_objective_gap_above_1e_minus4,
  sum(datasets$ecm_relative_objective_gap > 1e-4 & datasets$target_class == "outside"),
  summary$reliable_original_within_two_mcse,
  summary$reliable_original_maximum_standardized_difference)
writeLines(supplement, "tables/mti_gibbs_comparison_supplement.tex")
artifact_paths <- c(file.path(cfg$evidence_dir, c("datasets.csv", "parameters.csv", "sensitivity.csv",
  "audit.csv", "agreement.csv", "overview.csv", "summary.json")), figure,
  "tables/mti_gibbs_comparison_main.tex", "tables/mti_gibbs_comparison_supplement.tex")
code_paths <- c("application/scripts/110_review_mti_gibbs.R",
  "application/scripts/lib/mti_gibbs_review.R", "application/config/mti_gibbs_review_20261112.json")
pta_atomic_write_json(list(analysis_commit = pma_git("rev-parse", "HEAD"),
  analysis_programs = setNames(vapply(code_paths, mgc_file_hash, ""), code_paths),
  practical_source_commit = cfg$practical_commit, original_sampler_commit = practical$parent_commit,
  original_sampler_runtime_sha256 = runtime$sha256,
  input_manifest_sha256 = mgc_file_hash(file.path(directory, "manifest.json")),
  artifacts = setNames(vapply(artifact_paths, mgc_file_hash, ""), artifact_paths),
  created_utc = mgc_time(), session = capture.output(sessionInfo())),
  file.path(cfg$evidence_dir, "provenance.json"))
invisible(rqr_assert_external_checkout_unchanged(parent_manifest$external_reference))
print(summary)
