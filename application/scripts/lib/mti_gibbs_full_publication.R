mgfp_programs <- c("application/scripts/114_report_mti_gibbs_full_assessment.R",
  "application/scripts/lib/mti_gibbs_full_publication.R",
  "application/scripts/lib/mti_gibbs_full_publication_figures.R",
  "application/scripts/lib/mti_gibbs_full_report_review.R",
  "application/config/mti_gibbs_full_reporting_20261113.json")
mgfp_time <- function() format(Sys.time(), tz = "UTC", usetz = TRUE)
mgfp_git <- function(...) {
  paste(system2("git", c("--no-optional-locks", ...), stdout = TRUE), collapse = "\n")
}
mgfp_common <- function() dirname(normalizePath(mgfp_git("rev-parse", "--git-common-dir"), mustWork = TRUE))
mgfp_json_write <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(".full-publication-", tmpdir = dirname(path))
  on.exit(unlink(tmp))
  jsonlite::write_json(x, tmp, pretty = TRUE, auto_unbox = TRUE, null = "null", digits = NA)
  mgfa_require(file.rename(tmp, path), "Could not publish numerical evidence.")
}
mgfp_hashes <- function(paths) as.list(setNames(vapply(paths, mgfa_hash, ""), paths))
mgfp_check_hashes <- function(expected, paths = names(expected)) {
  mgfa_require(length(paths) > 0L && length(expected) == length(paths) && all(file.exists(paths)),
    "Missing or unnamed publication evidence.")
  mgfa_require(identical(unname(unlist(expected)), unname(vapply(paths, mgfa_hash, ""))),
    "Publication evidence hash differs.")
  invisible(TRUE)
}

mgfp_prior_verify <- function(cfg) {
  prior <- mgfa_json(cfg$prior_config)
  p <- mgfa_json(file.path(prior$evidence_dir, "provenance.json"))
  files <- c(file.path(prior$evidence_dir, c("datasets.csv", "parameters.csv", "sensitivity.csv",
    "audit.csv", "agreement.csv", "overview.csv", "cells.csv", "summary.json")),
    "figures/generated/figS_mti_fixed_target_width.pdf", "tables/mti_gibbs_comparison_main.tex",
    "tables/mti_gibbs_comparison_supplement.tex")
  programs <- c("application/scripts/110_review_mti_gibbs.R", "application/scripts/lib/mti_gibbs_review.R",
    cfg$prior_config)
  # Historical JSON arrays preserve order but omit the original vector names.
  mgfp_check_hashes(p$artifacts, files)
  mgfp_check_hashes(p$analysis_programs, programs)
  s <- mgfa_json(file.path(prior$evidence_dir, "summary.json"))
  mgfa_require(s$datasets == 456L && s$preferred_diagnostics == 456L &&
    identical(s$new_tolerance_guarantee, FALSE), "Historical assessment scope differs.")
  invisible(list(configuration = prior, summary = s, files = c(files, programs)))
}

mgfp_bounds <- function(cells, alpha = .05) {
  x <- data.table::as.data.table(cells)
  x <- data.table::copy(x[x$source == "simulation", ])
  mgfa_require(all(x$datasets > 0 & x$attained >= 0 & x$attained <= x$datasets) &&
    is.finite(alpha) && alpha > 0 && alpha < 1, "Invalid simulation binomial counts.")
  x[, family_size := .N, by = method]
  x[, simultaneous_lower := 0]
  x[, simultaneous_upper := 1]
  x[attained > 0, simultaneous_lower := stats::qbeta(alpha / (2 * family_size), attained, datasets - attained + 1)]
  x[attained < datasets, simultaneous_upper := stats::qbeta(1 - alpha / (2 * family_size), attained + 1, datasets - attained)]
  x
}

mgfp_aggregate <- function(d, p, intervals, cells) {
  for (name in c("d", "p", "intervals", "cells"))
    assign(name, data.table::as.data.table(get(name)))
  d <- data.table::copy(d); d[, ecm_width := ecm_upper - ecm_lower]
  keys <- c("response_id", "n", "content", "tolerance_confidence")
  simulation <- d[source == "simulation"]
  bounds <- mgfp_bounds(cells)
  wide <- data.table::dcast(bounds, response_id + n + content + tolerance_confidence ~ method,
    value.var = c("width_mean", "attainment_rate"))
  comparators <- c("mti_ecm_adaptive_cell", "young_mathew", "wilks")
  paired <- data.table::rbindlist(lapply(comparators, function(comparator_name) {
    g <- data.table::copy(intervals[source == "simulation" & method == "mti_gibbs_mean"])
    other <- intervals[source == "simulation" & method == comparator_name]
    other <- other[match(g$dataset_id, other$dataset_id)]
    mgfa_require(identical(g$dataset_id, other$dataset_id), "Paired observations differ.")
    g[, comparator := comparator_name]
    g[, difference := width - other$width]
    g[, content_difference := as.numeric(attained) - as.numeric(other$attained)]
    g[, .(datasets = .N, mean_width_difference = mean(difference),
      width_difference_mcse = sd(difference) / sqrt(.N),
      gibbs_shorter_datasets = sum(difference < 0), attainment_difference = mean(content_difference),
      attainment_difference_mcse = sd(content_difference) / sqrt(.N)), by = c(keys, "comparator")]
  }))
  for (comparator_name in comparators) {
    z <- paired[comparator == comparator_name]
    k <- function(x) do.call(paste, c(x[, ..keys], sep = ":"))
    matched <- wide[match(k(z), k(wide))]
    mgfa_equal(z$mean_width_difference,
      matched$width_mean_mti_gibbs_mean - matched[[paste0("width_mean_", comparator_name)]],
      "publication paired width differences")
    mgfa_equal(z$attainment_difference,
      matched$attainment_rate_mti_gibbs_mean - matched[[paste0("attainment_rate_", comparator_name)]],
      "publication paired attainment differences")
  }
  geometry <- d[, .(datasets = .N, preferred = sum(preferred_pass)), by = .(source, target_class)]
  geometry_cells <- simulation[, .(datasets = .N, interior = sum(target_class == "interior"),
    limiting = sum(target_class == "boundary"), outside = sum(target_class == "outside"),
    median_lower_expansion = median(ecm_lower - mean_lower),
    median_upper_expansion = median(mean_upper - ecm_upper)), by = c(keys, "target_class")]
  sensitivity <- simulation[, .(datasets = .N, mean_endpoint_attainment = mean(mean_attained),
    median_endpoint_attainment = mean(median_content >= content),
    mean_endpoint_width_mean = mean(mean_width), median_endpoint_width_mean = mean(median_interval_width),
    median_endpoint_width_q025 = unname(quantile(median_interval_width, .025)),
    median_endpoint_width_q975 = unname(quantile(median_interval_width, .975)),
    ecm_width_mean = mean(ecm_width)), by = keys]
  precision <- p[, .(parameters = .N, preferred = sum(preferred_pass), mean_precision = sum(mean_precision),
    median_precision = sum(median_precision), interval_precision = sum(interval_precision),
    maximum_mcse_sd_ratio = max(mcse_sd_ratio)), by = .(source, variable)]
  list(simulation_cells = bounds, application_partitions = cells[source == "application"],
    paired_cells = paired, geometry = geometry, geometry_cells = geometry_cells,
    median_sensitivity = sensitivity, precision = precision)
}

mgfp_read_chain <- function(directory, task, manifest) {
  path <- file.path(directory, "chains", sprintf("%04d", as.integer((task$job_id - 1L) %/% 1000L)),
    sprintf("chain_%06d.rds", task$job_id))
  receipt <- mgfa_json(paste0(path, ".json")); fit <- readRDS(path)
  mgfa_require(identical(receipt$sha256, mgfa_hash(path)) &&
    identical(receipt$task_sha256, digest::digest(task, algo = "sha256")) &&
    identical(receipt$analysis_identity, digest::digest(manifest$identity, algo = "sha256")) &&
    identical(fit$sample_sha256, task$dataset_sha256) &&
    nrow(fit$draws) == manifest$base_configuration$n_mcmc && all(is.finite(fit$draws)) &&
    all(fit$draws[, 2] > fit$draws[, 1]), "Illustrative chain receipt or observations differ.")
  list(draws = fit$draws, receipt = data.frame(case_id = task$case_id, job_id = task$job_id, sha256 = receipt$sha256))
}

mgfp_examples <- function(cfg, directory, manifest, d, prior) {
  jobs <- readRDS(file.path(directory, "jobs.rds"))
  old_jobs <- readRDS(file.path(mgfp_common(), prior$configuration$practical_directory, "slice/jobs.rds"))
  old <- data.table::fread(file.path(prior$configuration$evidence_dir, "datasets.csv"), nThread = 1L)
  mgfa_require(identical(as.character(cfg$example_case_ids), as.character(prior$configuration$example_case_ids)),
    "Posterior examples differ from the prespecified historical examples.")
  points <- list(); examples <- list(); receipts <- list(); display <- NULL; ranks <- NULL
  for (id in cfg$example_case_ids) {
    tasks <- jobs[jobs$case_id == id, ]; task <- tasks[1, ]
    previous <- old_jobs[old_jobs$case_id == id, ][1, ]
    reference <- old[case_id == id]
    mgfa_require(nrow(tasks) == manifest$base_configuration$chains && nrow(reference) == 1L,
      "An illustrative chain set or reference is missing.")
    mgfa_fields(task, previous, c("dataset_sha256", "q", "delta", "ecm_lower", "ecm_upper"),
      "historical example identity")
    mgfa_require(manifest$base_configuration$learning_rate == 1 &&
      manifest$base_configuration$prior_variance == 10000, "Example prior or learning rate changed.")
    fits <- lapply(seq_len(nrow(tasks)), function(i) {
      value <- mgfp_read_chain(directory, tasks[i, , drop = FALSE], manifest)
      receipts[[length(receipts) + 1L]] <<- value$receipt
      value$draws
    })
    widths <- sort(unlist(lapply(fits, function(x) x[, 2] - x[, 1])))
    mgfa_require(all(widths > 0), "Nonpositive illustrative posterior width.")
    ix <- unique(as.integer(round(seq(1, length(widths), length.out = cfg$cdf_points))))
    points[[id]] <- data.frame(case_id = id, width = widths[ix], probability = ix / length(widths))
    row <- d[d$case_id == id, ]; mgfa_require(nrow(row) == 1L, "Duplicate illustrative dataset.")
    examples[[id]] <- data.frame(row[, c("case_id", "source", "response_id", "n", "content", "q", "delta",
      "target_class", "mean_width", "posterior_width_q025", "posterior_width_q975")],
      ecm_width = task$ecm_upper - task$ecm_lower, minimum_width = reference$minimum_width,
      minimum_objective = reference$minimum_objective)
    if (id == cfg$example_case_ids[1]) {
      display <- do.call(rbind, lapply(seq_along(fits), function(i) {
        ix <- seq.int(1L, nrow(fits[[i]]), by = cfg$trace_display_stride)
        x <- fits[[i]][ix, , drop = FALSE]
        data.frame(chain = i, iteration = ix, lower = x[, 1], upper = x[, 2],
          width = x[, 2] - x[, 1], midpoint = rowMeans(x))
      }))
      quantities <- lapply(fits, function(x) cbind(lower = x[, 1], upper = x[, 2],
        width = x[, 2] - x[, 1], midpoint = rowMeans(x)))
      ranks <- do.call(rbind, lapply(colnames(quantities[[1]]), function(variable) {
        values <- do.call(cbind, lapply(quantities, function(x) x[, variable]))
        ranked <- matrix(rank(values), nrow(values))
        breaks <- seq(0, length(values), length.out = cfg$rank_bins + 1L)
        do.call(rbind, lapply(seq_len(ncol(values)), function(i) data.frame(variable = variable,
          chain = i, bin = seq_len(cfg$rank_bins), count = hist(ranked[, i], breaks, plot = FALSE)$counts)))
      }))
    }
  }
  list(cdf = do.call(rbind, points), examples = do.call(rbind, examples),
    chain_receipts = do.call(rbind, receipts), trace_example = display, rank_example = ranks)
}

mgfp_collect <- function(cfg) {
  directory <- file.path(mgfp_common(), cfg$campaign)
  review <- mgfa_review(directory, cfg$sampling_commit)
  mgfa_require(identical(review$status, "report_consistency_passed"), "Full study is not complete.")
  mgfa_require(review$results$datasets == cfg$expected_datasets &&
    review$results$simulation_datasets == cfg$expected_simulation_datasets &&
    review$results$application_datasets == cfg$expected_application_datasets, "Unexpected full-study design.")
  prior <- mgfp_prior_verify(cfg)
  read <- function(name) data.table::fread(file.path(directory, name), nThread = 1L, na.strings = "NA")
  d <- read("dataset_summary.csv"); p <- read("parameter_summary.csv")
  data <- mgfp_aggregate(d, p, read("interval_comparison.csv"), read("comparison_summary.csv"))
  old <- data.table::fread(file.path(prior$configuration$evidence_dir, "parameters.csv"), nThread = 1L)
  newer <- p[match(paste(old$case_id, old$variable), paste(p$case_id, p$variable))]
  mgfa_fields(newer, old, c("case_id", "variable", "q", "delta"), "independent-chain target")
  agreement <- data.frame(case_id = old$case_id, variable = old$variable,
    standardized_difference = (newer$mean - old$mean) / sqrt(newer$mcse_mean^2 + old$mcse_mean^2))
  data$agreement <- agreement
  illustration <- mgfp_examples(cfg, directory, mgfa_json(file.path(directory, "manifest.json")),
    as.data.frame(d), prior)
  data <- c(data, illustration)
  directory_out <- cfg$evidence_dir
  dir.create(directory_out, recursive = TRUE, showWarnings = FALSE)
  for (name in names(data)) data.table::fwrite(data[[name]], file.path(directory_out, paste0(name, ".csv")), na = "NA")
  summary <- list(completion = review$results, sampling_commit = cfg$sampling_commit,
    chains = cfg$expected_datasets * 4L, primary_summary = cfg$primary_summary,
    new_tolerance_guarantee = FALSE, replace_ecm_results = FALSE,
    diagnostic_policy = list(rhat = 1.01, ess = 400, mean_mcse_sd = .05),
    reference_prior_study = prior$summary, display_stride = cfg$trace_display_stride)
  mgfp_json_write(summary, file.path(directory_out, "summary.json"))
  files <- c(file.path(directory_out, paste0(names(data), ".csv")), file.path(directory_out, "summary.json"))
  input <- c(file.path(directory, c("manifest.json", "jobs.rds", "report_provenance.json",
    "dataset_summary.csv", "parameter_summary.csv", "interval_comparison.csv", "comparison_summary.csv")),
    prior$files)
  mgfp_json_write(list(analysis_commit = mgfp_git("rev-parse", "HEAD"), sampling_commit = cfg$sampling_commit,
    analysis_programs = mgfp_hashes(mgfp_programs), inputs = mgfp_hashes(unique(input)),
    data_files = mgfp_hashes(files), created_utc = mgfp_time(), session = capture.output(sessionInfo())),
    file.path(directory_out, "provenance.json"))
  invisible(summary)
}

mgfp_verify <- function(cfg, rendered = TRUE) {
  p <- mgfa_json(file.path(cfg$evidence_dir, "provenance.json"))
  mgfa_require(identical(p$sampling_commit, cfg$sampling_commit), "Publication source study differs.")
  mgfp_check_hashes(p$analysis_programs)
  mgfp_check_hashes(p$data_files)
  mgfp_prior_verify(cfg)
  summary <- mgfa_json(file.path(cfg$evidence_dir, "summary.json"))
  mgfa_require(summary$completion$datasets == cfg$expected_datasets &&
    identical(summary$new_tolerance_guarantee, FALSE) && identical(summary$replace_ecm_results, FALSE),
    "Publication summary has incompatible scope or interpretation.")
  if (rendered) {
    r <- mgfa_json(file.path(cfg$evidence_dir, "render_provenance.json"))
    mgfa_require(identical(r$data_provenance_sha256, mgfa_hash(file.path(cfg$evidence_dir, "provenance.json"))),
      "Rendered figures use different numerical evidence.")
    mgfp_check_hashes(r$outputs)
  }
  invisible(summary)
}
