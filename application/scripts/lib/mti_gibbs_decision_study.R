mdc_designs <- function(cfg, context) {
  out <- lapply(names(cfg$stages), function(stage) {
    s <- cfg$stages[[stage]]
    tasks <- mps_design(list(expected_cells = cfg$expected_cells, replications_per_cell = s$replications,
      data_seed_base = s$data_seed_base, kappa_grid = 2), context)
    # High-content cases start first under the same fixed rule.
    tasks <- tasks[order(!(tasks$n == 500 & tasks$content == .99), tasks$replication, tasks$cell_id), ]
    tasks$stage <- stage; rownames(tasks) <- NULL
    tasks
  })
  names(out) <- names(cfg$stages)
  out$shortlist <- context$old_tasks[context$old_tasks$replication <= cfg$exploratory_replications, ]
  mwd_require(nrow(out$shortlist) == cfg$expected_cells*cfg$exploratory_replications, "Exploratory cases are incomplete.")
  out
}

mdc_seed_blocks <- function(base, jobs, forbidden) {
  occupied <- unique(floor((forbidden-base)/100))
  bad <- which(jobs %in% occupied)
  pool <- setdiff(80001:99999, c(occupied, jobs))
  mwd_require(length(pool) >= length(bad), "Reserved independent seed blocks are exhausted.")
  jobs[bad] <- head(pool, length(bad))
  jobs
}

mdc_seed_plan <- function(designs, cfg, context) {
  old <- lapply(c("development", "calibration", "validation"), function(stage)
    data.table::fread(file.path(context$parent, stage, "tasks.csv")))
  old_data <- unique(c(unlist(lapply(old, `[[`, "data_seed")), context$old_tasks$data_seed,
    data.table::fread(file.path(context$boundary, "pilot/tasks.csv"))$data_seed))
  old_chains <- unlist(lapply(seq_along(old), function(j) as.vector(outer(
    context$prior$stages[[c("development", "calibration", "validation")[j]]]$chain_seed_base +
      100*old[[j]]$job_id, seq_len(cfg$sampling$chains), "+"))))
  boundary <- mwd_json(cfg$boundary_config)
  boundary_designs <- mbc_designs(boundary, context)
  for (stage in names(boundary_designs)) {
    base <- if (stage == "development") boundary$development_chain_seed_base else boundary$stages[[stage]]$chain_seed_base
    old_chains <- c(old_chains, unlist(lapply(1:3, function(j) as.vector(outer(
      base + 100*boundary_designs[[stage]]$job_id + 10*j, seq_len(cfg$sampling$chains), "+")))))
  }
  historical <- readRDS(file.path(mgfp_common(), mwd_json(boundary$diagnostic_config)$campaign, "jobs.rds"))
  old_chains <- c(old_chains, historical$seed)
  # Prior diagnostic probes used the reserved 2000--2019 million stream blocks.
  forbidden <- unique(c(old_data, old_chains, historical$data_seed))
  forbidden <- forbidden[is.finite(forbidden)]
  replacements <- 0L
  for (stage in names(designs)) {
    base <- if (stage == "shortlist") cfg$shortlist_chain_seed_base else cfg$stages[[stage]]$chain_seed_base
    tasks <- designs[[stage]]
    if (stage != "shortlist") {
      for (cell in unique(tasks$cell_id)) {
        ids <- which(tasks$cell_id == cell)
        bad <- ids[tasks$data_seed[ids] %in% forbidden]
        if (length(bad)) {
          pool <- cfg$stages[[stage]]$data_seed_base + 10000*cell +
            seq.int(cfg$stages[[stage]]$replications+1L, 9999L)
          pool <- pool[!pool %in% c(forbidden, tasks$data_seed[ids])]
          mwd_require(length(pool) >= length(bad), "A data-seed slot is exhausted.")
          tasks$data_seed[bad] <- as.integer(head(pool, length(bad)))
          replacements <- replacements + length(bad)
        }
      }
    }
    blocks <- mdc_seed_blocks(base, tasks$job_id, forbidden)
    replacements <- replacements + sum(blocks != tasks$job_id)
    for (j in 1:3) tasks[[paste0("chain_base", j)]] <- base + 100*blocks + 10*j
    if (stage != "shortlist") {
      tasks$ecm_seed <- as.integer(base + 100*blocks + 90)
    }
    designs[[stage]] <- tasks
  }
  fresh_data <- unlist(lapply(designs[c("calibration", "validation")], `[[`, "data_seed"))
  chains <- unlist(lapply(designs, function(tasks) unlist(lapply(1:3, function(j)
    as.vector(outer(tasks[[paste0("chain_base", j)]], seq_len(cfg$sampling$chains), "+"))))))
  ecm <- unlist(lapply(c("calibration", "validation"), function(stage)
    designs[[stage]]$ecm_seed))
  all_new <- c(fresh_data, chains, ecm)
  mwd_require(!anyDuplicated(all_new), "New data, chain, or ECM seeds overlap one another.")
  mwd_require(all(all_new > 0 & all_new < .Machine$integer.max), "A new seed is outside the integer range.")
  mwd_require(!any(all_new >= 2000000000 & all_new < 2020000000), "New seeds overlap the reserved prior probes.")
  overlap <- intersect(all_new, forbidden)
  mwd_require(!length(overlap), paste("New seeds overlap previous study seeds:", paste(head(overlap, 8), collapse = ", ")))
  list(designs = designs, audit = list(new_data_streams = length(fresh_data), new_chain_streams_reserved = length(chains),
    new_ecm_streams = length(ecm), collision_slots_reassigned = replacements,
    previous_seed_set_sha256 = mwd_hash(sort(forbidden)),
    tasks_sha256 = lapply(designs, mwd_hash), all_seeds_distinct_from_checked_prior_studies = TRUE))
}

mdc_frozen_selection <- function(gate, shortlist) {
  proposed <- merge(gate$policy[, .(n, content, selected_rule = rule, advance)],
    shortlist, by = c("n", "content"))
  proposed <- proposed[rule == selected_rule]
  proposed$rule <- "proposed"
  baseline <- data.table::copy(shortlist[rule == "baseline"])
  baseline$selected_rule <- "baseline"; baseline$advance <- TRUE
  data.table::rbindlist(list(baseline, proposed), use.names = TRUE, fill = TRUE)
}

mdc_final_rows <- function(value, task, calibrated, frozen, cfg, context) {
  set.seed(task$data_seed); y <- context$laws[[task$response_id]]$r(task$n)
  cdf <- context$laws[[task$response_id]]$p
  rules <- calibrated[calibrated$n == task$n & calibrated$content == task$content, ]
  report <- lapply(seq_len(nrow(value$rows)), function(j) {
    z <- as.data.frame(value$rows[j, ])
    multiplier <- rules$multiplier[rules$rule == z$rule]
    selected <- frozen[frozen$n == task$n & frozen$content == task$content & frozen$rule == z$rule, ]
    z$exploratory_advancement <- selected$advance
    mwd_require(length(multiplier) == 1, "Missing radius rule.")
    z$multiplier <- multiplier
    z$method <- paste0("gibbs_", z$rule)
    z$returned <- z$returned && is.finite(multiplier)
    if (z$returned) {
      ends <- mbc_interval(z$center, z$halfwidth, multiplier)
      z$lower <- ends[1]; z$upper <- ends[2]; z$width <- diff(ends)
      z$population_content <- unname(cdf(ends[2])-cdf(ends[1]))
    } else {
      z$width <- z$lower <- z$upper <- z$population_content <- z$dp_probability <- NA_real_
    }
    z$attained <- z$returned && z$population_content >= task$content
    z
  })
  base <- value$rows[value$rows$rule == "baseline", ]
  old <- as.data.frame(base)
  scale <- context$old_radius$multiplier[context$old_radius$n == task$n & context$old_radius$content == task$content]
  old$method <- "gibbs_original_radius"; old$multiplier <- scale
  if (old$returned) {
    ends <- mbc_interval(old$center, old$halfwidth, scale)
    old$lower <- ends[1]; old$upper <- ends[2]; old$width <- diff(ends)
    old$population_content <- unname(cdf(ends[2])-cdf(ends[1]))
  }
  old$attained <- old$returned && old$population_content >= task$content
  report[[length(report)+1L]] <- old
  ecm <- value$anchor$selected
  comparisons <- data.table::as.data.table(value$comparators)
  if (!is.null(ecm) && nrow(ecm)) comparisons <- data.table::rbindlist(list(comparisons,
    data.frame(method = "ecm", returned = TRUE, lower = ecm$lower, upper = ecm$upper,
      width = ecm$width, failure_reason = "")), fill = TRUE)
  comparisons <- cbind(task[rep(1L, nrow(comparisons)), ], comparisons)
  comparisons$population_content <- cdf(comparisons$upper)-cdf(comparisons$lower)
  comparisons$attained <- comparisons$returned & comparisons$population_content >= task$content
  result <- data.table::rbindlist(c(report, list(comparisons)), fill = TRUE)
  result[, dp_probability:=NA_real_]
  selected <- which(grepl("^gibbs_",result$method) & result$returned)
  if(length(selected)) result$dp_probability[selected] <- mdc_dp(y,value$anchor,
    result$lower[selected],result$upper[selected],task$content)
  result
}

mdc_summarize <- function(rows) data.table::as.data.table(rows)[, {
  ci <- binom.test(sum(attained), .N)$conf.int
  widths <- width[returned & is.finite(width)]
  .(samples = .N, returned = sum(returned), attained = sum(attained), attainment = mean(attained),
    attainment_lower = ci[1], attainment_upper = ci[2], mean_width = mean(widths),
    width_q025 = unname(quantile(widths, .025)), width_q975 = unname(quantile(widths, .975)),
    mean_population_content = mean(population_content[returned]),
    mean_dp_probability = if (all(is.na(dp_probability))) NA_real_ else mean(dp_probability[returned], na.rm = TRUE))
}, by = .(n, content, tolerance_confidence, response_id, method)]

mdc_report <- function(tasks, stage, identity, frozen, calibrated, cfg, context) {
  directory <- file.path(cfg$output_root, stage)
  manifest <- mwd_json(file.path(directory, "manifest.json"))
  mwd_require(identical(manifest$analysis_id, mdc_stage_id(identity, stage, frozen)), "Reporting identity changed.")
  rows <- data.table::rbindlist(lapply(seq_len(nrow(tasks)), function(i) {
    value <- mbc_read_fit(file.path(directory, "fits", sprintf("fit_%06d.rds", tasks$job_id[i])),
      manifest$analysis_id, tasks[i, ])
    mdc_final_rows(value, tasks[i, ], calibrated, frozen, cfg, context)
  }), fill = TRUE)
  summary <- mdc_summarize(rows)
  mwd_write(rows, file.path(directory, "interval_results.csv"))
  mwd_write(summary, file.path(directory, "comparison_summary.csv"))
  paired <- merge(rows[method == "gibbs_proposed"], rows[method == "gibbs_baseline"],
    by = c("job_id", "n", "content", "response_id", "data_seed"), suffixes = c("_new", "_base"))
  differences <- paired[, .(samples = .N, mean_paired_width_change = mean(width_new-width_base),
    mean_width_ratio = mean(width_new)/mean(width_base),
    newly_attained = sum(attained_new & !attained_base), lost_attainment = sum(!attained_new & attained_base)),
    by = .(n, content, response_id)]
  mwd_write(differences, file.path(directory, "paired_comparison.csv"))
  grDevices::pdf(file.path(directory, "width_and_attainment.pdf"), width = 12, height = 9)
  on.exit(grDevices::dev.off())
  for (n0 in unique(summary$n)) for (c0 in unique(summary[n == n0, content])) {
    z <- summary[n == n0 & content == c0]
    par(mfrow = c(2, 4), mar = c(7, 4, 3, 1))
    for (law in names(context$laws)) {
      a <- z[response_id == law]; x <- seq_len(nrow(a))
      plot(x, a$mean_width, ylim = range(c(a$width_q025, a$width_q975), finite = TRUE),
        xaxt = "n", xlab = "", ylab = "Interval width", pch = 4,
        main = paste(law, "n =", n0, "c =", c0), cex.main = .75)
      segments(x, a$width_q025, x, a$width_q975); axis(1, at = x, labels = a$method, las = 2, cex.axis = .65)
    }
    par(mfrow = c(2, 4), mar = c(7, 4, 3, 1))
    for (law in names(context$laws)) {
      a <- z[response_id == law]; x <- seq_len(nrow(a))
      plot(x, a$attainment, ylim = c(min(.85, a$attainment_lower), 1), xaxt = "n", xlab = "",
        ylab = "Attainment probability", pch = 16, main = law)
      segments(x, a$attainment_lower, x, a$attainment_upper)
      abline(h = a$tolerance_confidence[1], lty = 2, col = "#D55E00")
      axis(1, at = x, labels = a$method, las = 2, cex.axis = .65)
    }
  }
  invisible(summary)
}

mdc_preserve_evidence <- function(cfg, identity) {
  files <- c("manifest.json","replay/summary.json","replay/shortlist.csv","replay/radius_law_comparison.csv",
    "replay/radius_design_comparison.csv","materiality_decision.json","exploratory_policy.csv",
    "exploratory_scores.csv","exploratory_cells.csv","frozen_selection.rds","frozen_policy.json",
    "frozen_radius.json","calibration_policy.csv","calibration_cells.csv","calibrated_radius.rds",
    "validation/tasks.csv","validation/posterior_summary.csv","validation/interval_results.csv",
    "validation/comparison_summary.csv","validation/paired_comparison.csv","validation/width_and_attainment.pdf")
  files <- files[file.exists(file.path(cfg$output_root,files))]
  mgfp_json_write(list(source_commit=identity$source_commit, identity_hash=mwd_hash(identity),
    artifacts=as.list(setNames(vapply(file.path(cfg$output_root,files),mwd_file_hash,""),files)),
    original_article_unchanged=TRUE, statistical_assurance="declared_finite_panel_only",
    created=mgfp_time()),file.path(cfg$output_root,"evidence_receipt.json"))
}

mdc_verify_evidence <- function(directory) {
  receipt <- mwd_json(file.path(directory,"evidence_receipt.json"))
  for (name in names(receipt$artifacts)) mwd_require(file.exists(file.path(directory,name)) &&
    mwd_file_hash(file.path(directory,name)) == receipt$artifacts[[name]],paste("Evidence changed:",name))
  path <- file.path(directory,"validation/interval_results.csv")
  if (file.exists(path)) {
    rows <- data.table::fread(path)
    expected <- data.table::fread(file.path(directory,"validation/comparison_summary.csv"))
    actual <- mdc_summarize(rows)
    keys <- c("n","content","tolerance_confidence","response_id","method")
    data.table::setorderv(expected,keys);data.table::setorderv(actual,keys)
    mwd_require(isTRUE(all.equal(as.data.frame(expected),as.data.frame(actual),tolerance=1e-10,
      check.attributes=FALSE)),"Summary does not reproduce the retained interval data.")
  }
  cat("Decision-study evidence checks passed.\n")
  invisible(TRUE)
}

mdc_health <- function(cfg) {
  pointer <- file.path(mgfp_common(), ".codex_work/mti_gibbs_decision_latest.json")
  if (file.exists(pointer)) {
    latest <- mwd_json(pointer)
    mwd_require(latest$study_id == cfg$study_id, "Latest study identifier differs.")
    cfg$output_root <- latest$output_root
    cat("Frozen checkout:", latest$checkout, "\nSource:", latest$source_commit, "\n")
  }
  cat("Outputs:", normalizePath(cfg$output_root, mustWork = FALSE), "\n")
  for (name in c("study_summary.json", "replay/summary.json", "shortlist/progress.json",
    "calibration/progress.json", "validation/progress.json", "controller_exit.json")) {
    path <- file.path(cfg$output_root, name)
    if (file.exists(path)) { cat(name, "\n"); print(mwd_json(path)) }
  }
  invisible(cfg$output_root)
}
