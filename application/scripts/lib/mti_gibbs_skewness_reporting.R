mgs_validate_result <- function(value, input, task, cfg) {
  mwd_require(identical(value$task, task) && value$sample_sha256 == input$sample_sha256 &&
    value$parent_sha256 == input$parent_sha256, "Result sample or parent fit changed.")
  if (!is.null(value$error)) return(invisible(FALSE))
  p <- value$parameters; r <- value$row
  endpoint <- p$mean[match(c("lower", "upper"), p$variable)]
  mass <- msx_content(input$law, endpoint[1], endpoint[2])
  mwd_require(nrow(p) == 4L && all(p$q == task$q & p$delta == task$delta) &&
    nrow(r) == 1L && r$returned && r$lower == endpoint[1] && r$upper == endpoint[2] &&
    r$width == r$upper - r$lower && abs(r$population_content - mass) < 1e-14 &&
    r$attained == (mass >= task$content) &&
    value$adequate == all(p$preferred_pass & p$mean_precision), "Gibbs endpoint or adequacy summaries differ.")
  invisible(TRUE)
}

mgs_collect <- function(prepared, cfg, context, directory, workers = 4L) {
  tasks <- prepared$tasks
  groups <- split(seq_len(nrow(tasks)), tasks$case_id)
  pieces <- parallel::mclapply(groups, function(ids) {
    rows <- parameters <- diagnostics <- vector("list", length(ids))
    for (j in seq_along(ids)) {
      i <- ids[j]; task <- tasks[i, , drop = FALSE]; input <- mgs_input(task, context, cfg)
      input$law <- context$laws[[task$law_id]]
      value <- mgs_read(mgs_result_path(directory, task$job_id), task, prepared$identity,
        function(x) mgs_validate_result(x, input, task, cfg))
      mwd_require(!is.null(value), "A Gibbs assessment is missing.")
      if (is.null(value$error)) {
        fits <- lapply(seq_len(cfg$sampling$chains), function(chain) {
          t <- data.frame(chain = chain, seed = as.integer(cfg$chain_seed_base + 10L * task$job_id + chain),
            q = task$q, delta = task$delta, ecm_lower = task$ecm_lower, ecm_upper = task$ecm_upper)
          saved <- mgs_read(mgs_chain_path(directory, task$job_id, chain, value$row$retained_per_chain),
            t, prepared$identity, function(x) mgs_validate_chain(x, input$y, cfg, value$row$retained_per_chain))
          mwd_require(!is.null(saved), "A completed assessment is missing a chain.")
          saved$fit
        })
        endpoint <- colMeans(do.call(rbind, lapply(fits, `[[`, "draws")))
        mwd_require(isTRUE(all.equal(unname(endpoint), unname(c(value$row$lower, value$row$upper)), tolerance = 1e-14)),
          "Posterior endpoint means do not reproduce their chains.")
        probability <- rqrgibbs::rqr_dp_content_probability(rqrgibbs::rqr_dp_fit(input$y, concentration = 1,
          base_measure = rqrgibbs::rqr_dp_base_normal(0, 4)), endpoint[1], endpoint[2], task$content)
        mwd_require(probability$posterior_content_probability == value$row$dp_probability,
          "Gibbs DP probability does not reproduce the saved endpoints.")
        r <- data.table::as.data.table(value$row); parameters[[j]] <- value$parameters
      } else {
        r <- data.table::data.table(job_id = task$job_id, law_id = task$law_id, n = task$n,
          content = task$content, tolerance_confidence = task$tolerance_confidence, replication = task$replication,
          method = "gibbs", returned = FALSE, lower = NA_real_, upper = NA_real_, width = NA_real_,
          population_content = NA_real_, attained = FALSE, numerical_adequate = FALSE)
      }
      old <- data.table::copy(input$rows)
      old[, `:=`(numerical_adequate = TRUE, screen_pass = TRUE)]
      rows[[j]] <- data.table::rbindlist(list(old, r), fill = TRUE)
      rows[[j]][, `:=`(case_id = task$case_id, study_id = cfg$study_id, task_key = paste(cfg$study_id, task$job_id, sep = ":"),
        sample_sha256 = input$sample_sha256)]
      diagnostics[[j]] <- data.table::data.table(job_id = task$job_id, case_id = task$case_id,
        law_id = task$law_id, n = task$n, content = task$content, adequate = isTRUE(value$adequate),
        sampling_error = !is.null(value$error), error = value$error %||% "",
        warnings = length(value$warnings), warning_text = paste(unique(value$warnings), collapse = " | "),
        seconds = value$elapsed_seconds,
        max_rhat = if (is.null(value$error)) max(value$parameters$rhat) else NA_real_,
        min_bulk_ess = if (is.null(value$error)) min(value$parameters$ess_bulk) else NA_real_,
        min_tail_ess = if (is.null(value$error)) min(value$parameters$ess_tail) else NA_real_)
    }
    list(rows = data.table::rbindlist(rows), parameters = data.table::rbindlist(parameters, fill = TRUE),
      diagnostics = data.table::rbindlist(diagnostics))
  }, mc.cores = min(workers, length(groups)), mc.set.seed = FALSE)
  mwd_require(all(vapply(pieces, is.list, FALSE)), "Completed Gibbs result verification failed.")
  list(rows = data.table::rbindlist(lapply(pieces, `[[`, "rows"), fill = TRUE),
    parameters = data.table::rbindlist(lapply(pieces, `[[`, "parameters"), fill = TRUE),
    diagnostics = data.table::rbindlist(lapply(pieces, `[[`, "diagnostics")))
}

mgs_historical_cells <- function(cfg) {
  d <- data.table::fread(file.path(cfg$historical_evidence, "simulation_cells.csv"))
  d[, .(law_id = response_id, n, content, tolerance_confidence, method, samples = attempts,
    returned, attained = successes, attainment, mcse = sqrt(attainment * (1 - attainment) / attempts), mean_width, width_q025, width_q975,
    width_samples = returned)]
}

mgs_bounds <- function(d) {
  d <- data.table::copy(d)
  d[, `:=`(lower_95 = 0, upper_95 = 1, simultaneous_lower = 0, simultaneous_upper = 1)]
  d[, family_size := .N, by = method]
  d[attained > 0, `:=`(lower_95 = qbeta(.025, attained, samples - attained + 1),
    simultaneous_lower = qbeta(.05 / (2 * family_size), attained, samples - attained + 1))]
  d[attained < samples, `:=`(upper_95 = qbeta(.975, attained + 1, samples - attained),
    simultaneous_upper = qbeta(1 - .05 / (2 * family_size), attained + 1, samples - attained))]
  d
}

mgs_paired <- function(rows) {
  g <- rows[method == "gibbs"]
  data.table::rbindlist(lapply(c("ecm", "young_mathew", "wilks"), function(name) {
    b <- rows[method == name]; b <- b[match(g$job_id, b$job_id)]
    mwd_require(identical(g$job_id, b$job_id), "Gibbs comparison samples differ.")
    a <- data.table::copy(g)
    a[, `:=`(comparator = name, difference = width - b$width,
      attainment_difference = as.numeric(attained) - as.numeric(b$attained))]
    a[, .(samples = .N, paired_widths = sum(is.finite(difference)),
      mean_width_difference = mean(difference, na.rm = TRUE),
      width_difference_mcse = sd(difference, na.rm = TRUE) / sqrt(sum(is.finite(difference))),
      attainment_difference = mean(attainment_difference),
      attainment_difference_mcse = sd(attainment_difference) / sqrt(.N)),
      by = .(law_id, n, content, tolerance_confidence, comparator)]
  }))
}

mgs_complete_cases <- function(rows, tasks) {
  d <- data.table::copy(data.table::as.data.table(rows))
  fields <- c("job_id", "law_id", "n", "content", "replication", "sample_sha256")
  mwd_require(all(fields %in% names(d)) && all(c(fields, "case_id") %in% names(tasks)) &&
    !anyDuplicated(tasks$job_id) && all(!is.na(tasks$case_id) & nzchar(tasks$case_id)),
    "Missing interval fields or invalid frozen case index.")
  index <- match(d$job_id, tasks$job_id)
  mwd_require(!anyNA(index), "An exported interval has no frozen task.")
  for (field in c("law_id", "n", "content", "replication", "sample_sha256"))
    mwd_require(all(d[[field]] == tasks[[field]][index]), paste("Interval task differs:", field))
  expected <- tasks$case_id[index]
  if ("case_id" %in% names(d)) mwd_require(all(is.na(d$case_id) | d$case_id == "" | d$case_id == expected),
    "An exported case identifier conflicts with its frozen task.")
  d[, case_id := expected]
  d
}

mgs_comparison_layout <- function() graphics::par(mfrow = c(1, 2), mar = c(4.5, 12, 3, 1),
  oma = c(2, 0, 2, 0), las = 1, cex = .9, xpd = FALSE)

mgs_legend <- function(labels, colors) {
  old <- graphics::par(xpd = NA)
  on.exit(graphics::par(old))
  graphics::legend(graphics::grconvertX(.5, "ndc", "user"), graphics::grconvertY(.035, "ndc", "user"),
    labels, col = colors, lty = 1, bty = "n", cex = .9, horiz = TRUE, xjust = .5, yjust = .5)
}

mgs_comparison_pdf <- function(cells, cfg, path) {
  labels <- mwd_json("application/config/mti_skewness_publication_20261122.json")
  laws <- rev(labels$law_order); names_laws <- setNames(labels$labels, labels$law_order)
  methods <- c("ecm", "gibbs", "young_mathew", "wilks")
  colors <- c(ecm = "#009E73", gibbs = "#0072B2", young_mathew = "#CC79A7", wilks = "black")
  names_methods <- c("MTI-ECM", "MTI-Gibbs", "Young-Mathew", "Wilks")
  designs <- unique(cells[, .(n, content)])[order(n, content)]
  references <- data.table::fread(file.path(cfg$primary_evidence, "population_content_references.csv"))
  grDevices::pdf(path, width = 13, height = 9, useDingbats = FALSE)
  on.exit(grDevices::dev.off())
  for (i in seq_len(nrow(designs))) {
    z <- cells[n == designs$n[i] & content == designs$content[i]]
    mgs_comparison_layout()
    for (metric in c("attainment", "width")) {
      reference <- references[content == designs$content[i]]
      limits <- if (metric == "attainment") c(.85, 1.003) else
        range(c(z$width_q025, z$width_q975, reference$width), finite = TRUE) * c(.9, 1.1)
      graphics::plot(NA, xlim = limits, ylim = c(.5, length(laws) + .5), yaxt = "n", ylab = "",
        log = if (metric == "width") "x" else "", xlab = if (metric == "width") "Interval width (log scale)" else "Content-attainment probability",
        main = if (metric == "width") "95% empirical width ranges" else "Attainment and 95% Monte Carlo intervals")
      graphics::axis(2, at = seq_along(laws), labels = names_laws[laws], tick = FALSE, cex.axis = .85)
      graphics::abline(h = seq(.5, length(laws) + .5, 1), col = "grey93")
      if (metric == "attainment") graphics::abline(v = .95, lty = 2, col = "grey40")
      if (metric == "width") {
        y <- match(reference$dgp_id, laws)
        graphics::segments(reference$width, y - .4, reference$width, y + .4, lty = 2, col = "grey35")
      }
      for (j in seq_along(methods)) {
        a <- z[method == methods[j]]; y <- match(a$law_id, laws) + c(.3, .1, -.1, -.3)[j]
        lo <- if (metric == "width") a$width_q025 else a$lower_95
        hi <- if (metric == "width") a$width_q975 else a$upper_95
        mid <- if (metric == "width") a$mean_width else a$attainment
        graphics::segments(pmax(lo, limits[1]), y, pmin(hi, limits[2]), y, col = colors[j], lwd = 1.6)
        graphics::points(pmax(mid, limits[1]), y, pch = if (metric == "width") 4 else 16, col = colors[j], cex = .55)
        below <- which(lo < limits[1]); if (length(below)) graphics::points(rep(limits[1], length(below)), y[below], pch = 17, col = colors[j], cex = .6)
      }
    }
    mgs_legend(names_methods, colors)
    graphics::mtext(sprintf("n = %d; required content = %.2f; tolerance confidence = .95", designs$n[i], designs$content[i]),
      outer = TRUE, side = 3, line = .2, cex = 1.05)
    graphics::mtext("Width crosses mark means. Triangles indicate attainment intervals extending below 0.85.", outer = TRUE, side = 1, line = .3, cex = .7)
  }
}

mgs_trace_pdf <- function(prepared, cfg, directory, diagnostics, path) {
  ids <- prepared$tasks$job_id[prepared$tasks$replication == 1L]
  worst <- diagnostics[!sampling_error & is.finite(max_rhat)][order(-max_rhat), head(job_id, 12L)]
  ids <- unique(c(ids, worst))
  grDevices::pdf(path, width = 11, height = 8.5, useDingbats = FALSE)
  on.exit(grDevices::dev.off())
  for (id in ids) {
    task <- prepared$tasks[prepared$tasks$job_id == id, , drop = FALSE]
    value <- readRDS(mgs_result_path(directory, id)); if (!is.null(value$error)) next
    fits <- lapply(seq_len(cfg$sampling$chains), function(chain)
      readRDS(mgs_chain_path(directory, id, chain, value$row$retained_per_chain))$fit)
    graphics::par(mfrow = c(2, 3), mar = c(4, 4.5, 2.5, .8), oma = c(2, 0, 3, 0))
    display <- lapply(fits, function(f) list(draws = rbind(cbind(
      lower = pmin(f$warmup_root1[, 1], f$warmup_root2[, 1]),
      upper = pmax(f$warmup_root1[, 1], f$warmup_root2[, 1])), f$draws)))
    arrays <- mgp_array(display)
    iteration <- seq_len(dim(arrays)[1]) - cfg$sampling$n_burn
    colors <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7")
    for (j in 1:4) {
      p <- value$parameters[j, ]
      graphics::matplot(iteration, arrays[, , j], type = "l", lty = 1, col = colors,
        xlab = "Iteration (0 = end of warm-up)", ylab = dimnames(arrays)[[3]][j], main = sprintf("R-hat %.3f; bulk ESS %.0f", p$rhat, p$ess_bulk))
      graphics::abline(h = p$ecm_value, lty = 2, col = "grey30")
      graphics::abline(v = 0, lty = 3, col = "grey50")
    }
    objective <- vapply(fits, function(f) c(f$target_loss_trace[seq_len(cfg$sampling$n_burn)] * cfg$sampling$learning_rate +
      (f$warmup_root1[, 1]^2 + f$warmup_root2[, 1]^2) / (2 * cfg$sampling$prior_variance), f$retained_objective),
      numeric(length(iteration)))
    graphics::matplot(iteration, objective, type = "l", lty = 1, col = colors,
      xlab = "Iteration (0 = end of warm-up)", ylab = "Negative log generalized posterior", main = "Posterior objective")
    graphics::abline(v = 0, lty = 3, col = "grey50")
    graphics::plot.new()
    graphics::legend("topleft", paste("Chain", 1:4), col = colors, lty = 1, bty = "n")
    graphics::text(0, .45, sprintf("Mean-endpoint accuracy: %s\nRetained per chain: %d\nContent attained: %s\nDP content probability: %.4f",
      if (value$adequate) "adequate" else "requires review", value$row$retained_per_chain,
      if (value$row$attained) "yes" else "no", value$row$dp_probability), adj = c(0, 1), cex = .85)
    graphics::mtext(sprintf("%s; n=%d; c=%.2f; replication=%d; q=%.8g; delta=%.5g", task$law_id, task$n,
      task$content, task$replication, task$q, task$delta), outer = TRUE, side = 3, line = .6, cex = 1)
    graphics::mtext("Warm-up appears before zero; diagnostics use retained draws. Horizontal dashed lines show ECM summaries.", outer = TRUE, side = 1, line = .5, cex = .8)
  }
}

mgs_report <- function(prepared, cfg, context, directory, workers = 4L) {
  result <- mgs_collect(prepared, cfg, context, directory, workers)
  d <- result$rows
  mwd_require(nrow(d) == cfg$expected_datasets * 4L && !anyDuplicated(d[, .(job_id, method)]) &&
    all(d[, .N, by = .(law_id, n, content, method)]$N == cfg$replications), "Incomplete four-method comparison.")
  added <- msx_summary(d)
  fields <- c("law_id", "n", "content", "tolerance_confidence", "method", "samples", "returned", "attained",
    "attainment", "mcse", "mean_width", "width_q025", "width_q975", "width_samples")
  combined <- mgs_bounds(data.table::rbindlist(list(mgs_historical_cells(cfg), added[, ..fields])))
  primary <- data.table::fread(file.path(cfg$primary_evidence, "primary_cells.csv"))
  keys <- c("law_id", "n", "content", "method")
  a <- combined[method != "gibbs", ..fields]; b <- primary[, ..fields]
  data.table::setorderv(a, keys); data.table::setorderv(b, keys)
  mwd_require(isTRUE(all.equal(as.data.frame(a), as.data.frame(b), tolerance = 1e-12)), "The primary comparison changed.")
  output <- file.path(directory, "report"); dir.create(output, showWarnings = FALSE)
  values <- list(added_interval_results = d, added_cells = added, combined_cells = combined,
    paired_added_comparison = mgs_paired(d), parameter_summary = result$parameters,
    numerical_diagnostics = result$diagnostics)
  for (name in names(values)) pma_write_csv(as.data.frame(values[[name]]), file.path(output, paste0(name, ".csv")))
  labels <- mwd_json("application/config/mti_skewness_publication_20261122.json")
  renderer <- new.env(parent = environment())
  sys.source("application/scripts/lib/mti_gibbs_full_publication_figures.R", envir = renderer)
  renderer$mgfp_families <- setNames(labels$labels, labels$law_order)
  renderer$mgfp_methods <- c(ecm = "MTI-ECM", gibbs = "MTI-Gibbs", young_mathew = "Young--Mathew", wilks = "Wilks")
  display <- data.table::copy(combined)[, `:=`(response_id = law_id, datasets = samples, attainment_rate = attainment)]
  renderer$mgfp_simulation_table(as.data.frame(display), file.path(output, "four_method_cases.tex"))
  mgs_comparison_pdf(combined, cfg, file.path(output, "four_method_comparison.pdf"))
  mgs_trace_pdf(prepared, cfg, directory, result$diagnostics, file.path(output, "convergence_examples.pdf"))
  numerical <- all(!result$diagnostics$sampling_error & result$diagnostics$adequate)
  wide <- data.table::dcast(combined, law_id + n + content ~ method, value.var = "mean_width")
  writeLines(c("# Fixed-target Gibbs comparison: numerical and scientific review", "",
    "The added 36,000 samples and ECM-selected content/tilt targets are unchanged. The original 72-case Gibbs comparison is preserved.",
    sprintf("Adequate added datasets: %d/%d; sampling errors: %d.", sum(result$diagnostics$adequate), cfg$expected_datasets,
      sum(result$diagnostics$sampling_error)),
    sprintf("Across 108 cases, Gibbs observed attainment ranges from %.3f to %.3f.",
      min(combined[method == "gibbs", attainment]), max(combined[method == "gibbs", attainment])),
    sprintf("Gibbs mean width is greater than ECM in %d cases and smaller in %d.", sum(wide$gibbs > wide$ecm), sum(wide$gibbs < wide$ecm)),
    "Widths and their 95% empirical ranges retain distribution/design groups. Attainment confidence intervals quantify Monte Carlo uncertainty.",
    "DP content probabilities are re-evaluated at the Gibbs mean endpoints. ECM eligibility does not transfer to these intervals.",
    "The numerical flags, interval summaries, paired differences, below-support diagnostics and traces require review before manuscript changes.",
    "No prior, threshold or interval calibration has been selected using these results; no new distribution-free theorem is asserted."),
    file.path(output, "scientific_review.md"))
  mgfp_json_write(list(completed = TRUE, numerically_adequate = numerical, datasets = cfg$expected_datasets,
    adequate_datasets = sum(result$diagnostics$adequate), sampling_errors = sum(result$diagnostics$sampling_error),
    warning_datasets = sum(result$diagnostics$warnings > 0),
    primary_cases = 108L, gibbs_cases = 108L, original_gibbs_cases = 72L,
    identity = prepared$identity, files = mgfp_hashes(list.files(output, full.names = TRUE)),
    original_primary_unchanged = TRUE, article_replaced = FALSE, time = mgfp_time()), file.path(directory, "verification.json"))
  list(complete = TRUE, numerically_adequate = numerical, report = output)
}
