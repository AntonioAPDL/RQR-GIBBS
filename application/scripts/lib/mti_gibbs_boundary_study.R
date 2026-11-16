mbc_context <- function(cfg) {
  prior <- mwd_json(cfg$prior_config)
  mwd_require(prior$primary_kappa == 2 && !cfg$fixed_prior_uses_analysis_sample &&
    !cfg$known_support_used_in_fitting && !cfg$automatic_article_replacement &&
    !cfg$automatic_full_validation_launch && prior$simulation_reference$center == 0 &&
    prior$simulation_reference$scale == 1 && !prior$simulation_reference$estimated_from_analysis_sample,
    "The bounded study specification changed.")
  context <- mps_context(prior, cfg$output_root)
  context$prior <- prior
  context$policy_diagnostics <- mwd_json(mwd_json(cfg$diagnostic_config)$practical_config)$policy
  context$parent <- file.path(mgfp_common(), cfg$parent_worktree, cfg$parent_output)
  development <- mwd_json(file.path(context$parent, "development/manifest.json"))
  mwd_require(development$identity$source_commit == cfg$parent_commit &&
    context$generator_sha256 == development$identity$generators,
    "Development source or data generator changed.")
  article <- mwd_json(mwd_json(cfg$diagnostic_config)$article_config)
  for (path in c(article$validation_config, article$policy_csv, "tables/tolerance_validation_article_scan_calibration.csv"))
    mwd_require(!is.null(development$identity$inputs[[path]]) &&
      development$identity$inputs[[path]] == mwd_file_hash(path), "An inherited anchor input changed.")
  package <- mps_package_check()
  mwd_require(identical(unname(unlist(package$libraries)),
    unname(unlist(development$identity$package$libraries))), "The fixed-prior native runtime changed.")
  context$package <- package
  context
}

mbc_designs <- function(cfg, context) {
  old <- as.data.frame(data.table::fread(file.path(context$parent, "development/tasks.csv")))
  dev <- old[old$kappa == 2, ]; dev$parent_job_id <- dev$job_id; dev$job_id <- seq_len(nrow(dev))
  stages <- lapply(names(cfg$stages), function(stage) {
    s <- cfg$stages[[stage]]
    d <- mps_design(list(expected_cells = cfg$expected_cells, replications_per_cell = s$replications,
      data_seed_base = s$data_seed_base, kappa_grid = 2), context)
    d$stage <- stage; d
  })
  names(stages) <- names(cfg$stages)
  out <- c(list(development = dev), stages)
  old_seeds <- unique(unlist(lapply(c("development", "calibration", "validation"), function(stage)
    data.table::fread(file.path(context$parent, stage, "tasks.csv"), select = "data_seed")$data_seed)))
  historical_jobs <- readRDS(file.path(mgfp_common(), mwd_json(cfg$diagnostic_config)$campaign, "jobs.rds"))
  old_seeds <- union(old_seeds, historical_jobs$data_seed)
  fresh <- unlist(lapply(stages, function(d) unique(d$data_seed)))
  mwd_require(!anyDuplicated(fresh) && !any(fresh %in% old_seeds), "Fresh stage data seeds overlap.")
  chain_seeds <- lapply(names(out), function(stage) {
    d <- out[[stage]]
    base <- if (stage == "development") cfg$development_chain_seed_base else cfg$stages[[stage]]$chain_seed_base
    values <- unlist(lapply(seq_along(cfg$variants), function(v)
      as.vector(outer(base + 100 * d$job_id + 10 * v, seq_len(cfg$sampling$chains), "+"))))
    mwd_require(all(values > 0 & values < .Machine$integer.max) && !anyDuplicated(values),
      "Invalid or duplicate study chain seeds.")
    values
  })
  all_chains <- unlist(chain_seeds)
  old_chains <- unlist(lapply(names(context$prior$stages), function(stage) {
    old_tasks <- data.table::fread(file.path(context$parent, stage, "tasks.csv"))
    as.vector(outer(context$prior$stages[[stage]]$chain_seed_base + 100 * old_tasks$job_id,
      seq_len(context$prior$sampling$chains), "+"))
  }))
  old_chains <- union(old_chains, historical_jobs$seed)
  mwd_require(!anyDuplicated(all_chains) && !any(all_chains %in% c(old_seeds, fresh, old_chains)) &&
    !any(fresh %in% old_chains), "Data and chain streams overlap.")
  rehearsal_seeds <- unlist(lapply(seq_along(cfg$variants), function(v)
    cfg$rehearsal$chain_seed_base + 100 + 10*v + seq_len(cfg$sampling$chains)))
  mwd_require(!cfg$rehearsal$data_seed %in% c(old_seeds, fresh, old_chains, all_chains) &&
    !any(rehearsal_seeds %in% c(old_seeds, fresh, old_chains, all_chains, cfg$rehearsal$data_seed)),
    "Rehearsal streams overlap study streams.")
  out
}

mbc_audit <- function(cfg, context, workers) {
  output <- file.path(cfg$output_root, "audit"); dir.create(output, recursive = TRUE, showWarnings = FALSE)
  evidence <- mwd_json(file.path(cfg$completed_evidence, "provenance.json"))
  files <- names(evidence$inputs)[grepl("/validation/(interval|comparator)_results[.]csv$", names(evidence$inputs))]
  mwd_require(length(files) == 2L, "Completed paired validation inputs are missing.")
  mgfp_check_hashes(evidence$inputs[files])
  raw <- data.table::fread(files[grepl("/interval_results", files)])
  e <- raw[method == "ecm", .(job_id, response_id, n, content, data_seed, q = selected_q,
    delta = selected_delta, ecm_lower = lower, ecm_upper = upper, ecm_width = width)]
  g <- raw[method == "gibbs", .(job_id, gibbs_lower = lower, gibbs_upper = upper, gibbs_width = width)]
  d <- merge(e, g, by = "job_id")
  mwd_require(nrow(d) == 72000L && !anyDuplicated(d$job_id), "The completed paired panel differs.")
  groups <- split(d, interaction(d$response_id, d$n, d$content, drop = TRUE))
  result <- parallel::mclapply(groups, function(z) {
    law <- context$laws[[z$response_id[1]]]; support <- mwd_support(context$dgps[[z$response_id[1]]])
    summaries <- lapply(seq_len(nrow(z)), function(i) {
      r <- z[i]; set.seed(r$data_seed); y <- law$r(r$n)
      if (i == 1L) {
        path <- file.path(context$parent, "validation/fits", sprintf("fit_%06d.rds", r$job_id))
        receipt <- mwd_json(paste0(path, ".json"))
        mwd_require(receipt$sha256 == mwd_file_hash(path) && readRDS(path)$sample_sha256 == mwd_hash(y),
          "Reconstructed audit sample differs from its saved fit.")
      }
      geometry <- mbc_project_tilt(y, r$q, r$delta, "original")
      wasted <- if (is.finite(support[1])) max(0, min(r$gibbs_upper, support[1]) - r$gibbs_lower) else 0
      if (is.finite(support[2])) wasted <- wasted + max(0, r$gibbs_upper - max(r$gibbs_lower, support[2]))
      data.frame(r, margin = geometry$original_margin, target_class = geometry$target_class,
        outward_slope = geometry$outward_slope, width_beyond_known_support = wasted)
    })
    data.table::rbindlist(summaries)
  }, mc.cores = workers, mc.set.seed = FALSE)
  mwd_require(!any(vapply(result, inherits, FALSE, "try-error")), "The boundary audit failed.")
  rows <- data.table::rbindlist(result)
  mwd_write(rows, file.path(output, "sample_geometry.csv"))
  cells <- rows[, .(samples = .N, interior = sum(target_class == "interior"),
    boundary = sum(target_class == "boundary"), outside = sum(target_class == "outside"),
    mean_ecm_width = mean(ecm_width), mean_gibbs_width = mean(gibbs_width),
    mean_width_beyond_support = mean(width_beyond_known_support)), by = .(response_id, n, content)]
  mwd_write(cells, file.path(output, "cell_geometry.csv"))
  mgfp_json_write(list(source_commit = mgfp_git("rev-parse", "HEAD"), input_hashes = evidence$inputs[files],
    generator = context$generator_sha256, audited = nrow(rows), sample_hashes_checked = length(groups),
    target_counts = as.list(table(factor(rows$target_class, levels = c("interior", "boundary", "outside")))),
    support_used_only_for_diagnostics = TRUE, refits = 0L), file.path(output, "summary.json"))
  grDevices::pdf(file.path(output, "geometry_review.pdf"), width = 11, height = 8)
  on.exit(grDevices::dev.off())
  for (n0 in unique(rows$n)) for (c0 in unique(rows[n == n0, content])) {
    z <- rows[n == n0 & content == c0]; laws <- unique(z$response_id)
    par(mfrow = c(2, 4), mar = c(4, 4, 3, 1))
    for (law in laws) {
      a <- z[response_id == law]
      plot(a$margin, a$gibbs_width - a$ecm_width, pch = 16, cex = .35,
        col = grDevices::adjustcolor("#0072B2", .25), xlab = "Normalized empirical tilt margin",
        ylab = "Gibbs minus ECM width", main = paste(law, n0, c0), cex.main = .8)
      abline(h = 0, v = 0, lty = 2, col = "gray50")
    }
  }
  invisible(cells)
}

mbc_job <- function(task, stage, variants, cfg, context) {
  set.seed(task$data_seed); y <- context$laws[[task$response_id]]$r(task$n)
  if (stage == "development") {
    path <- file.path(context$parent, "development/fits", sprintf("fit_%06d.rds", task$parent_job_id))
    receipt <- mwd_json(paste0(path, ".json")); mwd_require(receipt$sha256 == mwd_file_hash(path), "Development fit changed.")
    anchor <- readRDS(path)
    mwd_require(isTRUE(anchor$returned) && all(anchor$rows$gibbs_adequate) &&
      anchor$prior$prior_sd == 2 && anchor$sample_sha256 == mwd_hash(y), "Development anchor differs.")
    anchor_source <- receipt$sha256
  } else {
    rule <- context$policy[context$policy$n == task$n & context$policy$content == task$content, ]
    mwd_require(nrow(rule) == 1L, "Missing frozen ECM anchor rule.")
    base <- cfg$stages[[stage]]$chain_seed_base
    anchor <- mfp_fit_data(y, task$content, task$tolerance_confidence, task$retained_fraction,
      2, context$prior$simulation_reference, context$profile, context$dp, as.list(rule[1, ]),
      context$prior$numerical, cfg$sampling, as.integer(base + 100 * task$job_id),
      context$policy_diagnostics, sample_gibbs = FALSE)
    mwd_require(isTRUE(anchor$returned), paste("ECM anchor failed:", anchor$reason))
    anchor_source <- "fresh_sample_fixed_ecm_dp_rule"
  }
  records <- rows <- list()
  for (variant in variants) {
    geometry <- mbc_project_tilt(y, anchor$q, anchor$delta_reference, variant)
    reusable <- if (stage == "development") anchor else NULL
    if (length(records)) for (record in records)
      if (identical(record$geometry$delta, geometry$delta)) reusable <- record
    if (!is.null(reusable) && (variant == "original" ||
      identical(geometry$delta, if (!is.null(reusable$geometry)) reusable$geometry$delta else anchor$delta_reference))) {
      fits <- reusable$fits
      parameters <- if (!is.null(reusable$parameters)) reusable$parameters else reusable$parameters_reference_units
      record <- list(geometry = geometry, fits = lapply(fits, function(f) f[setdiff(names(f), "target_loss_trace")]),
        parameters = parameters, adequate = all(parameters$preferred_pass & parameters$mean_precision), reused = TRUE)
    } else {
      base <- if (stage == "development") cfg$development_chain_seed_base else cfg$stages[[stage]]$chain_seed_base
      record <- mbc_sample_target(y, task, anchor, variant, cfg, context$policy_diagnostics,
        base + 100 * task$job_id + 10 * match(variant, cfg$variants))
    }
    records[[variant]] <- record
    rows[[variant]] <- mbc_rows(task, variant, geometry, record$fits, record$parameters,
      anchor, y, context$laws[[task$response_id]]$p)
  }
  compact_anchor <- anchor[intersect(names(anchor), c("prior", "reference", "q", "delta_reference", "selected",
    "candidates", "selected_ecm_trace", "selected_knot_adjustments", "sample_sha256", "dp_prior"))]
  list(ok = all(vapply(records, function(r) isTRUE(r$adequate), FALSE)), task = task,
    anchor = compact_anchor, anchor_source = anchor_source, records = records,
    rows = data.table::rbindlist(rows), comparators = mfp_comparators(y, task$content, task$tolerance_confidence))
}

mbc_collect_rows <- function(directory, tasks, identity) {
  data.table::rbindlist(lapply(seq_len(nrow(tasks)), function(i) {
    r <- mbc_read_fit(file.path(directory, "fits", sprintf("fit_%06d.rds", tasks$job_id[i])), identity, tasks[i, ])
    mwd_require(isTRUE(r$ok), "A study sample has unresolved numerical calculations.")
    r$rows
  }))
}

mbc_queue <- function(tasks, stage, cfg, context, identity, workers, selection = NULL) {
  mwd_require(nrow(tasks) > 0 && !anyDuplicated(tasks$job_id), "Stage task identifiers must be unique.")
  directory <- file.path(cfg$output_root, stage); dir.create(file.path(directory, "fits"), recursive = TRUE, showWarnings = FALSE)
  analysis_id <- mwd_hash(list(identity, stage, selection))
  manifest <- file.path(directory, "manifest.json")
  if (file.exists(manifest)) mwd_require(mwd_json(manifest)$analysis_id == analysis_id, "Stage identity changed.") else
    mgfp_json_write(list(analysis_id = analysis_id, identity = identity, stage = stage, selection = selection), manifest)
  mwd_write(tasks, file.path(directory, "tasks.csv"))
  paths <- file.path(directory, "fits", sprintf("fit_%06d.rds", tasks$job_id))
  mwd_require(!any(file.exists(paths) & !file.exists(paste0(paths, ".json"))),
    "An interrupted result lacks a receipt and requires review before restart.")
  present <- which(file.exists(paste0(paths, ".json")))
  for (i in present) mwd_require(isTRUE(mbc_read_fit(paths[i], analysis_id, tasks[i, ])$ok),
    "A preserved unresolved fit requires review; it will not be replaced.")
  pending <- setdiff(seq_len(nrow(tasks)), present)
  completed <- length(present)
  for (batch in split(pending, ceiling(seq_along(pending) / workers))) {
    mps_disk_check(directory, cfg$disk_reserve_gib)
    done <- parallel::mclapply(batch, function(i) {
      started <- proc.time()[3]; task <- tasks[i, ]
      variants <- if (is.null(selection)) cfg$variants else selection$variant[
        selection$n == task$n & selection$content == task$content &
          selection$tolerance_confidence == task$tolerance_confidence]
      value <- tryCatch({
        mwd_require(length(variants) > 0L && (is.null(selection) || length(variants) == 1L), "Target policy is ambiguous.")
        mbc_job(task, stage, variants, cfg, context)
      }, error = function(e) list(ok = FALSE, task = task, reason = conditionMessage(e)))
      value$elapsed_seconds <- unname(proc.time()[3] - started)
      mbc_atomic_fit(value, paths[i], analysis_id, task)
      isTRUE(value$ok)
    }, mc.cores = workers, mc.set.seed = FALSE)
    completed <- completed + length(batch)
    passed <- all(vapply(done, isTRUE, FALSE))
    mgfp_json_write(list(stage = stage, completed = completed, expected = nrow(tasks), workers = workers,
      numerical_pass = passed, article_replaced = FALSE, time = mgfp_time()), file.path(directory, "progress.json"))
    cat(mgfp_time(), stage, completed, "/", nrow(tasks), "saved\n"); flush.console()
    mwd_require(passed, "A completed batch requires numerical review; original results are preserved.")
  }
  rows <- mbc_collect_rows(directory, tasks, analysis_id)
  mwd_write(rows, file.path(directory, "posterior_summary.csv"))
  mgfp_json_write(list(completed = nrow(tasks), expected = nrow(tasks), posterior_records = nrow(rows),
    numerical_pass = all(rows$adequate), time = mgfp_time()), file.path(directory, "status.json"))
  list(rows = rows, analysis_id = analysis_id)
}

mbc_capacity <- function(cfg, tasks, identity, remaining) {
  directory <- file.path(cfg$output_root, "development")
  ids <- seq_len(min(12L, nrow(tasks))); sizes <- numeric(length(ids))
  for (j in ids) {
    r <- mbc_read_fit(file.path(directory, "fits", sprintf("fit_%06d.rds", tasks$job_id[j])), identity, tasks[j, ])
    r$records <- r$records[1]; r$rows <- r$rows[1]
    temp <- tempfile(tmpdir = cfg$output_root); saveRDS(r, temp); sizes[j] <- file.info(temp)$size; unlink(temp)
  }
  free <- mps_disk_check(cfg$output_root, cfg$disk_reserve_gib)
  estimate <- mfp_storage_estimate(sizes, remaining) / 1024^3
  record <- list(free_gib = free, reserve_gib = cfg$disk_reserve_gib,
    projected_additional_gib = estimate, feasible = free - estimate >= cfg$disk_reserve_gib)
  mgfp_json_write(record, file.path(cfg$output_root, "storage_check.json"))
  mwd_require(record$feasible, "Projected study storage exceeds the protected disk reserve.")
}

mbc_rehearsal_report <- function(checks, path) {
  grDevices::pdf(path, width = 11, height = 8)
  on.exit(grDevices::dev.off())
  colors <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7")
  for (check in checks) for (variant in names(check$records)) {
    record <- check$records[[variant]]; fits <- record$fits
    values <- lapply(fits, function(f) {
      width <- f$draws[, 2] - f$draws[, 1]
      cbind(lower = f$draws[, 1], upper = f$draws[, 2], width = width,
        midpoint = rowMeans(f$draws), objective = f$retained_objective,
        cumulative_width = cumsum(width) / seq_along(width))
    })
    labels <- c("Lower endpoint", "Upper endpoint", "Width", "Midpoint",
      "Negative log generalized posterior", "Cumulative mean width")
    par(mfrow = c(3, 2), mar = c(3.5, 4.5, 1, 1), oma = c(0, 0, 3, 0),
      mgp = c(2, .6, 0), cex.lab = .85, cex.axis = .75)
    for (j in seq_len(6L)) {
      x <- do.call(cbind, lapply(values, function(v) v[, j]))
      matplot(x, type = "l", lty = 1, col = colors, lwd = .5,
        xlab = if (j > 4L) "Retained iteration" else "", ylab = labels[j])
      if (j == 1L) legend("bottomleft", paste("Chain", seq_along(fits)), col = colors,
        lty = 1, bty = "n", cex = .65, horiz = TRUE)
    }
    mtext(sprintf("%s: n=%d, c=%.2f, %s (four chains)", check$task$response_id,
      check$task$n, check$task$content, variant), outer = TRUE, line = 1)
  }
  invisible(path)
}

mbc_pilot_report <- function(cfg, context, tasks, analysis_id, calibration) {
  directory <- file.path(cfg$output_root, "pilot")
  results <- lapply(seq_len(nrow(tasks)), function(i) {
    task <- tasks[i, ]; r <- mbc_read_fit(file.path(directory, "fits", sprintf("fit_%06d.rds", task$job_id)),
      analysis_id, task)
    a <- r$rows
    rule <- calibration[calibration$n == task$n & calibration$content == task$content &
      calibration$tolerance_confidence == task$tolerance_confidence, ]
    mwd_require(nrow(rule) == 1L && rule$variant == a$variant, "Pilot and frozen calibration targets differ.")
    ends <- mbc_interval(a$center, a$halfwidth, rule$multiplier)
    values <- data.table::rbindlist(list(
      data.frame(method = "gibbs_calibrated", lower = ends[1], upper = ends[2], returned = TRUE),
      data.frame(method = "gibbs_unscaled", lower = a$lower, upper = a$upper, returned = TRUE),
      data.frame(method = "ecm", lower = a$ecm_lower, upper = a$ecm_upper, returned = TRUE),
      r$comparators[, c("method", "lower", "upper", "returned")]))
    cdf <- context$laws[[task$response_id]]$p
    values[, `:=`(job_id = task$job_id, response_id = task$response_id, n = task$n, content = task$content,
      tolerance_confidence = task$tolerance_confidence, variant = a$variant,
      multiplier = rule$multiplier, width = upper - lower,
      population_content = cdf(upper) - cdf(lower))]
    values[, attained := returned & population_content >= content]
    values
  })
  rows <- data.table::rbindlist(results)
  mwd_write(rows, file.path(directory, "interval_results.csv"))
  cells <- rows[, .(attempts = .N, returned = sum(returned), successes = sum(attained),
    attainment = mean(attained), mean_width = mean(width), width_q025 = quantile(width, .025),
    width_q975 = quantile(width, .975)), by = .(response_id, n, content, tolerance_confidence, method)]
  cells[, `:=`(lower_95 = ifelse(successes == 0, 0, qbeta(.025, successes, attempts - successes + 1)),
    upper_95 = ifelse(successes == attempts, 1, qbeta(.975, successes + 1, attempts - successes)))]
  mwd_write(cells, file.path(directory, "comparison_summary.csv"))
  labels <- c(gibbs_calibrated = "Gibbs: calibrated interval", gibbs_unscaled = "Gibbs: selected target means",
    ecm = "MTI-ECM", young_mathew = "Young-Mathew", wilks = "Wilks")
  colors <- c("#0072B2", "#56B4E9", "#009E73", "#CC79A7", "black"); names(colors) <- names(labels)
  grDevices::pdf(file.path(directory, "pilot_review.pdf"), width = 11, height = 8)
  on.exit(grDevices::dev.off())
  designs <- unique(cells[, .(n, content)])
  for (j in seq_len(nrow(designs))) {
    z <- cells[n == designs$n[j] & content == designs$content[j]]
    laws <- unique(z$response_id)
    for (kind in c("attainment", "width")) {
      lo <- if (kind == "width") z$width_q025 else z$lower_95
      hi <- if (kind == "width") z$width_q975 else z$upper_95
      yy <- match(z$response_id, laws) * 6 + match(z$method, names(labels)) - 3
      par(mar = c(5, 12, 4, 1))
      plot(NA, xlim = if (kind == "width") range(c(lo, hi)) else c(min(.75, lo), 1),
        ylim = c(2, length(laws) * 6 + 3), axes = FALSE, ylab = "",
        xlab = if (kind == "width") "Interval width: empirical 2.5%--97.5% ranges" else
          "Content attainment: pointwise exact 95% confidence intervals",
        main = sprintf("Independent pilot: n=%d, c=%.2f", designs$n[j], designs$content[j]))
      axis(1); axis(2, at = seq_along(laws) * 6, labels = laws, las = 1, tick = FALSE)
      segments(lo, yy, hi, yy, col = colors[z$method], lwd = 2)
      points(if (kind == "width") z$mean_width else z$attainment, yy, pch = if (kind == "width") 4 else 16,
        col = colors[z$method], cex = .65)
      if (kind == "attainment") abline(v = .95, lty = 2, col = "gray50")
      legend("bottomright", labels, col = colors, lty = 1, bty = "n", cex = .7)
    }
  }
  summary <- list(status = "independent_pilot_complete_requires_review", datasets = nrow(tasks),
    target_rules = as.data.frame(calibration), numerical_pass = TRUE,
    calibrated_attainment_min = min(cells[method == "gibbs_calibrated", attainment]),
    pointwise_upper_below_target = sum(cells[method == "gibbs_calibrated", upper_95 < tolerance_confidence]),
    assurance_scope = "finite_declared_simulation_panel_only_not_all_distributions",
    article_replaced = FALSE, full_validation_launched = FALSE)
  mgfp_json_write(summary, file.path(cfg$output_root, "study_summary.json"))
  invisible(cells)
}
