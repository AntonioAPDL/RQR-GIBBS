# Candidate selection contains no population CDF or distribution-family input.
mdc_path <- function(candidates) {
  d <- as.data.frame(candidates)
  needed <- c("candidate_index", "target_content", "mean_tilt", "central_tilt", "lower",
    "upper", "width", "finite_interval", "numerically_accurate", "posterior_content_probability")
  mwd_require(all(needed %in% names(d)) && !anyDuplicated(d$candidate_index), "Invalid candidate table.")
  d <- d[which(d$finite_interval & d$numerically_accurate & is.finite(d$width) & d$width > 0 &
    is.finite(d$posterior_content_probability) & d$posterior_content_probability > 0), ]
  if (!nrow(d)) return(d)
  d <- d[order(d$width, d$target_content, abs(d$mean_tilt-d$central_tilt), d$lower, d$candidate_index), ]
  d <- d[d$posterior_content_probability > c(-Inf, head(cummax(d$posterior_content_probability), -1)), ]
  rownames(d) <- NULL
  d
}

mdc_select <- function(candidates, gamma) {
  mwd_require(length(gamma) == 1L && is.finite(gamma) && gamma > 0 && gamma < 1,
    "A probability threshold must lie strictly between zero and one.")
  path <- mdc_path(candidates)
  head(path[path$posterior_content_probability >= gamma, ], 1L)
}

mdc_dp <- function(y, anchor, lower, upper, content) {
  prior <- anchor$dp_prior
  mwd_require(!prior$estimated_from_analysis_sample && prior$raw_base$family == "normal",
    "An independent normal DP base is required.")
  fit <- rqrgibbs::rqr_dp_fit(y, prior$concentration,
    rqrgibbs::rqr_dp_base_normal(prior$raw_base$mean, prior$raw_base$sd), strict_bayes = TRUE)
  rqrgibbs::rqr_dp_content_probability(fit, lower, upper, content)$posterior_content_probability
}

mdc_context <- function(cfg) {
  boundary <- mwd_json(cfg$boundary_config)
  boundary$output_root <- cfg$output_root
  context <- mbc_context(boundary)
  mwd_require(cfg$prior_sd == 2 && cfg$learning_rate == 1 && !cfg$analysis_sample_used_for_prior &&
    !cfg$automatic_article_replacement && cfg$maximum_alternatives == 2 &&
    cfg$protected_comparisons >= 2*cfg$expected_cells &&
    setequal(cfg$asymmetric_laws, setdiff(names(context$laws), c("normal", "student_t3"))),
    "The fixed-prior comparison specification changed.")
  context$boundary <- file.path(mgfp_common(), cfg$boundary_worktree, cfg$boundary_output)
  context$old_manifest <- mwd_json(file.path(context$boundary, "calibration/manifest.json"))
  context$old_tasks <- as.data.frame(data.table::fread(file.path(context$boundary, "calibration/tasks.csv")))
  context$old_radius <- data.table::fread(file.path(context$boundary, "calibration_policy.csv"))
  context$old_tilts <- data.table::fread(file.path(context$boundary, "development_policy.csv"))
  context
}

mdc_old <- function(task, context) {
  path <- file.path(context$boundary, "calibration/fits", sprintf("fit_%06d.rds", task$job_id))
  receipt <- mwd_json(paste0(path, ".json"))
  mwd_require(identical(receipt$identity, context$old_manifest$analysis_id) &&
    identical(receipt$sha256, mwd_file_hash(path)), "A saved exploratory calculation changed.")
  value <- readRDS(path)
  mwd_require(value$ok && identical(receipt$task_sha256, mwd_hash(value$task)) &&
    value$task$data_seed == task$data_seed && value$task$job_id == task$job_id,
    "A saved exploratory sample identity changed.")
  value
}

# Events occur just above, not at, the previous candidate's probability.
mdc_curve <- function(paths, baseline_gamma, content, p) {
  laws <- unique(vapply(paths, function(z) z$response_id[1], ""))
  gamma <- sort(unique(c(baseline_gamma, unlist(lapply(paths, function(z)
    z$posterior_content_probability[z$posterior_content_probability <= baseline_gamma])))))
  gamma <- gamma[gamma > 0 & gamma <= baseline_gamma]
  widths <- attainment <- matrix(NA_real_, length(gamma), length(laws), dimnames = list(NULL, laws))
  for (j in seq_along(laws)) {
    pieces <- paths[vapply(paths, function(z) z$response_id[1] == laws[j], FALSE)]
    baseline <- mean(vapply(pieces, function(z) z$width[which(z$posterior_content_probability >= baseline_gamma)[1]], 0))
    events <- data.table::rbindlist(lapply(pieces, function(z) {
      if (nrow(z) < 2) return(NULL)
      data.frame(breakpoint = head(z$posterior_content_probability, -1), dw = diff(z$width),
        da = diff(as.numeric(z$population_content >= content)))
    }))
    initial_width <- sum(vapply(pieces, function(z) z$width[1], 0))
    initial_attainment <- sum(vapply(pieces, function(z) as.numeric(z$population_content[1] >= content), 0))
    if (nrow(events)) {
      events <- events[, .(dw = sum(dw), da = sum(da)), by = breakpoint][order(breakpoint)]
      index <- findInterval(gamma, events$breakpoint, left.open = TRUE)
      widths[, j] <- (initial_width + c(0, cumsum(events$dw))[index+1L]) / length(pieces) / baseline
      attainment[, j] <- (initial_attainment + c(0, cumsum(events$da))[index+1L]) / length(pieces)
    } else {
      widths[, j] <- initial_width / length(pieces) / baseline
      attainment[, j] <- initial_attainment / length(pieces)
    }
  }
  data.frame(gamma = gamma, worst_log_ratio = apply(log(widths), 1, max),
    mean_log_ratio = rowMeans(log(widths)), minimum_attainment = apply(attainment, 1, min),
    exploratory_attainment_pass = apply(attainment, 1, min) >= p - 1e-12)
}

mdc_shortlist <- function(curve, baseline_gamma) {
  choose <- function(d) {
    if (!nrow(d)) return(NA_real_)
    d$gamma[order(d$worst_log_ratio, d$mean_log_ratio, -d$gamma)[1L]]
  }
  alternatives <- unique(c(choose(curve[curve$exploratory_attainment_pass, ]), choose(curve)))
  alternatives <- alternatives[is.finite(alternatives) & alternatives != baseline_gamma]
  data.frame(rule = c("baseline", if (length(alternatives)) paste0("alternative", seq_along(alternatives))),
    gamma = c(baseline_gamma, alternatives))
}

mdc_replay <- function(tasks, cfg, context, workers) {
  output <- file.path(cfg$output_root, "replay")
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  groups <- split(tasks, interaction(tasks$n, tasks$content, drop = TRUE))
  result <- parallel::mclapply(groups, function(d) {
    paths <- lapply(seq_len(nrow(d)), function(i) {
      old <- mdc_old(d[i, ], context); a <- old$anchor
      rule <- context$policy[context$policy$n == d$n[i] & context$policy$content == d$content[i], ]
      selected <- mdc_select(a$candidates, rule$screen)
      mwd_require(nrow(selected) == 1 && selected$candidate_index == a$selected$candidate_index &&
        selected$target_content == a$q && selected$mean_tilt == a$delta_reference,
        "The complete candidate table does not reproduce the current selected target.")
      set.seed(d$data_seed[i]); y <- context$laws[[d$response_id[i]]]$r(d$n[i])
      mwd_require(identical(mwd_hash(y), a$sample_sha256), "The exploratory observations changed.")
      z <- mdc_path(a$candidates)
      z$population_content <- with(z, context$laws[[d$response_id[i]]]$p(upper) -
        context$laws[[d$response_id[i]]]$p(lower))
      z$response_id <- d$response_id[i]
      z
    })
    gamma <- context$policy$screen[context$policy$n == d$n[1] & context$policy$content == d$content[1]]
    curve <- mdc_curve(paths, gamma, d$content[1], d$tolerance_confidence[1])
    list(curve = cbind(n = d$n[1], content = d$content[1], curve),
      policy = cbind(n = d$n[1], content = d$content[1], tolerance_confidence = d$tolerance_confidence[1],
        mdc_shortlist(curve, gamma)), replayed = nrow(d))
  }, mc.cores = min(workers, length(groups)), mc.set.seed = FALSE)
  mwd_require(!any(vapply(result, inherits, FALSE, "try-error")), "Candidate replay requires review.")
  curves <- data.table::rbindlist(lapply(result, `[[`, "curve"))
  policy <- data.table::rbindlist(lapply(result, `[[`, "policy"))
  mwd_require(all(policy[, .N, by = .(n, content)]$N <= cfg$maximum_alternatives+1), "Short list is too large.")
  mwd_write(curves, file.path(output, "threshold_comparison.csv"))
  mwd_write(policy, file.path(output, "shortlist.csv"))
  mgfp_json_write(list(replayed = sum(vapply(result, `[[`, 1L, "replayed")),
    thresholds = nrow(curves), frozen_shortlist = mgfp_hashes(file.path(output, "shortlist.csv")),
    probability_choices_from_saved_breakpoints = TRUE, exploratory_only = TRUE), file.path(output, "summary.json"))
  policy
}

mdc_radius_review <- function(cfg, context) {
  d <- data.table::fread(file.path(context$boundary, "calibration/posterior_summary.csv"))
  confidence <- unique(d$tolerance_confidence)
  mwd_require(length(confidence)==1L,"The radius review requires one requested attainment probability.")
  probability <- mbc_rank(cfg$stages$calibration$replications, confidence,
    cfg$calibration_error, cfg$protected_comparisons)$rank/(cfg$stages$calibration$replications+1)
  cells <- d[, {
    m <- .N; ordered <- sort(required_scale)
    lo <- qbinom(.025,m,probability); hi <- qbinom(.975,m,probability)+1
    .(samples=m, old_rank=mbc_rank(m,tolerance_confidence[1],cfg$calibration_error,cfg$expected_cells)$rank,
      p_quantile=unname(quantile(required_scale,tolerance_confidence[1],type=1)),
      proposed_probability_scale=probability,
      proposed_descriptive_scale=unname(quantile(required_scale,probability,type=1)),
      quantile_lower=if(lo==0) 0 else ordered[lo], quantile_upper=if(hi>m) Inf else ordered[hi],
      old_law_scale=ordered[mbc_rank(m,tolerance_confidence[1],cfg$calibration_error,cfg$expected_cells)$rank])
  }, by=.(n,content,response_id)]
  designs <- cells[, .(old_multiplier=max(old_law_scale), estimated_new_multiplier=max(proposed_descriptive_scale),
    worst_law_quantile_lower=max(quantile_lower), worst_law_quantile_upper=max(quantile_upper),
    old_controlling_law=response_id[which.max(old_law_scale)],
    proposed_controlling_law=response_id[which.max(proposed_descriptive_scale)]), by=.(n,content)]
  designs[, estimated_radius_ratio:=estimated_new_multiplier/old_multiplier]
  mwd_write(cells,file.path(cfg$output_root,"replay/radius_law_comparison.csv"))
  mwd_write(designs,file.path(cfg$output_root,"replay/radius_design_comparison.csv"))
  invisible(designs)
}

mdc_target_key <- function(y, anchor, geometry, cfg) mwd_hash(list(sample = mwd_hash(y),
  q = anchor$q, delta = geometry$delta, prior_sd = cfg$prior_sd,
  learning_rate = cfg$learning_rate, sampling = cfg$sampling))

mdc_stage_id <- function(identity, stage, selection) mwd_hash(list(identity, stage,
  lapply(as.list(selection)[sort(names(selection))], function(x) unname(as.vector(x)))))

mdc_anchor <- function(anchor, gamma) {
  selected <- mdc_select(anchor$candidates, gamma)
  if (!nrow(selected)) return(NULL)
  anchor$selected <- selected; anchor$q <- selected$target_content
  anchor$delta_reference <- selected$mean_tilt
  anchor
}

mdc_job <- function(task, stage, selection, cfg, context) {
  set.seed(task$data_seed); y <- context$laws[[task$response_id]]$r(task$n)
  rules <- selection[selection$n == task$n & selection$content == task$content, ]
  variant <- context$old_tilts$variant[context$old_tilts$n == task$n & context$old_tilts$content == task$content]
  mwd_require(length(variant) == 1 && nrow(rules) > 0, "A design lacks its fixed selection rule.")
  if (stage == "shortlist") {
    old <- mdc_old(task, context); anchor <- old$anchor
  } else {
    rule <- as.list(context$policy[context$policy$n == task$n & context$policy$content == task$content, ][1, ])
    anchor <- mfp_fit_data(y, task$content, task$tolerance_confidence, task$retained_fraction, 2,
      context$prior$simulation_reference, context$profile, context$dp, rule, context$prior$numerical,
      cfg$sampling, task$ecm_seed,
      context$policy_diagnostics, sample_gibbs = FALSE)
    mwd_require(!identical(anchor$reason, "unresolved_profile_candidates") &&
      (is.null(anchor$unresolved_candidates) || anchor$unresolved_candidates == 0),
      "A fresh ECM candidate requires numerical review.")
    mwd_require(!is.null(anchor$candidates) && !is.null(anchor$dp_prior), "The ECM profile calculation failed.")
  }
  mwd_require(identical(anchor$sample_sha256, mwd_hash(y)), "The target sample differs.")
  records <- rows <- list(); new_targets <- 0L
  for (j in seq_len(nrow(rules))) {
    a <- mdc_anchor(anchor, rules$gamma[j])
    if (is.null(a)) {
      rows[[j]] <- data.frame(task, rule = rules$rule[j], gamma = rules$gamma[j], returned = FALSE,
        adequate = TRUE, required_scale = Inf, width = NA_real_, center = NA_real_, halfwidth = NA_real_,
        failure_reason = "no_accurate_eligible_candidate")
      next
    }
    geometry <- mbc_project_tilt(y, a$q, a$delta_reference, variant)
    key <- mdc_target_key(y, a, geometry, cfg)
    if (is.null(records[[key]])) {
      if (stage == "shortlist" && a$q == anchor$q && geometry$delta == old$rows$delta[1]) {
        record <- old$records[[variant]]
        mwd_require(!is.null(record) && record$adequate, "The reusable posterior result differs.")
        record$origin <- list(stage = "old_calibration", job_id = task$job_id,
          source_commit = mwd_json(file.path(context$boundary, "manifest.json"))$identity$source_commit)
      } else {
        record <- mbc_sample_target(y, task, a, variant, cfg, context$policy_diagnostics,
          as.integer(task[[paste0("chain_base", j)]]))
        new_targets <- new_targets + 1L
        record$origin <- list(stage = stage, job_id = task$job_id, rule = rules$rule[j])
      }
      records[[key]] <- record
    }
    record <- records[[key]]
    row <- mbc_rows(task, variant, geometry, record$fits, record$parameters, a, y,
      context$laws[[task$response_id]]$p)
    row$rule <- rules$rule[j]; row$gamma <- rules$gamma[j]; row$returned <- TRUE
    row$target_key <- key; row$dp_probability <- mdc_dp(y, a, row$lower, row$upper, task$content)
    row$max_mcse_mean <- max(record$parameters$mcse_mean)
    row$max_mcse_sd_ratio <- max(record$parameters$mcse_sd_ratio)
    row$mean_precision <- all(record$parameters$mean_precision)
    if (!record$adequate) row$required_scale <- Inf
    rows[[j]] <- row
  }
  list(ok = all(vapply(records, function(r) isTRUE(r$adequate), FALSE)), task = task,
    anchor = anchor[intersect(names(anchor), c("prior", "reference", "q", "delta_reference", "selected",
      "candidates", "selected_ecm_trace", "selected_knot_adjustments", "sample_sha256", "dp_prior"))],
    records = records, rows = data.table::rbindlist(rows, fill = TRUE), new_targets = new_targets,
    comparators = if (stage == "shortlist") old$comparators else mfp_comparators(y, task$content, task$tolerance_confidence))
}

mdc_capacity <- function(directory, cfg, remaining, paths) {
  free <- mps_disk_check(directory, cfg$disk_reserve_gib)
  if (!length(paths)) return(invisible(free))
  # The upper observed file size allows for heterogeneous chain lengths.
  projected <- max(file.info(paths)$size) * remaining * 1.5 / 1024^3
  mwd_require(free > cfg$disk_reserve_gib + projected,
    sprintf("Storage projection %.1f GiB plus reserve exceeds %.1f GiB available.", projected, free))
  invisible(free)
}

mdc_queue <- function(tasks, stage, selection, cfg, context, identity, workers) {
  directory <- file.path(cfg$output_root, stage)
  dir.create(file.path(directory, "fits"), recursive = TRUE, showWarnings = FALSE)
  id <- mdc_stage_id(identity, stage, selection)
  manifest <- file.path(directory, "manifest.json")
  if (file.exists(manifest)) mwd_require(mwd_json(manifest)$analysis_id == id, "Stage identity changed.") else
    mgfp_json_write(list(analysis_id = id, selection = selection, identity = identity), manifest)
  mwd_write(tasks, file.path(directory, "tasks.csv"))
  paths <- file.path(directory, "fits", sprintf("fit_%06d.rds", tasks$job_id))
  present <- which(file.exists(paste0(paths, ".json")))
  mwd_require(!any(file.exists(paths) & !file.exists(paste0(paths, ".json"))), "An interrupted fit lacks a receipt.")
  rows <- vector("list", nrow(tasks)); seconds <- targets <- numeric(nrow(tasks))
  for (i in present) {
    value <- mbc_read_fit(paths[i], id, tasks[i, ])
    mwd_require(value$ok, "A preserved numerical failure requires review and will not be overwritten.")
    rows[[i]] <- value$rows; seconds[i] <- value$elapsed_seconds; targets[i] <- value$new_targets
  }
  started <- Sys.time(); resumed <- length(present)
  pending <- setdiff(seq_len(nrow(tasks)), present)
  for (batch in split(pending, ceiling(seq_along(pending)/workers))) {
    mdc_capacity(directory, cfg, nrow(tasks)-length(present), paths[unique(c(head(present, 200), tail(present, 200)))])
    done <- parallel::mclapply(batch, function(i) {
      tic <- proc.time()[3]
      value <- tryCatch(mdc_job(tasks[i, ], stage, selection, cfg, context),
        error = function(e) list(ok = FALSE, task = tasks[i, ], reason = conditionMessage(e)))
      value$elapsed_seconds <- unname(proc.time()[3]-tic)
      mbc_atomic_fit(value, paths[i], id, tasks[i, ])
      value[c("ok", "rows", "new_targets", "elapsed_seconds", "reason")]
    }, mc.cores = workers, mc.set.seed = FALSE)
    for (j in seq_along(batch)) {
      i <- batch[j]; value <- done[[j]]
      mwd_require(!inherits(value, "try-error") && is.list(value), "A worker stopped without a saved result.")
      rows[i] <- list(value$rows); seconds[i] <- value$elapsed_seconds; targets[i] <- value$new_targets %||% 0
    }
    present <- c(present, batch)
    elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
    newly_done <- length(present)-resumed
    passed <- all(vapply(done, function(x) isTRUE(x$ok), FALSE))
    mgfp_json_write(list(stage = stage, completed_datasets = length(present), expected_datasets = nrow(tasks),
      new_posterior_targets = sum(targets), workers = workers, numerical_pass = passed,
      remaining_datasets = nrow(tasks)-length(present),
      eta_seconds = if (newly_done > 0) elapsed/newly_done*(nrow(tasks)-length(present)) else NA_real_,
      time = mgfp_time()), file.path(directory, "progress.json"))
    cat(mgfp_time(), stage, length(present), "/", nrow(tasks), "datasets saved\n"); flush.console()
    mwd_require(passed, "A saved calculation requires numerical review; no data have been replaced.")
  }
  all_rows <- data.table::rbindlist(rows, fill = TRUE)
  mwd_write(all_rows, file.path(directory, "posterior_summary.csv"))
  list(rows = all_rows, analysis_id = id, targets = sum(targets))
}

mdc_gate <- function(rows, old_policy, cfg) {
  d <- data.table::copy(data.table::as.data.table(rows))
  confidence <- unique(d$tolerance_confidence)
  mwd_require(length(confidence)==1L,"The exploratory comparison requires one requested attainment probability.")
  rank <- mbc_rank(cfg$stages$calibration$replications, confidence, cfg$calibration_error, cfg$protected_comparisons)$rank
  probability <- rank/(cfg$stages$calibration$replications+1)
  cells <- d[, .(samples = .N, returned = sum(returned), adequate = all(adequate),
    radius = unname(quantile(required_scale, probability, type = 1)), mean_base_width = mean(width)),
    by = .(n, content, tolerance_confidence, response_id, rule, gamma)]
  scales <- cells[, .(radius = max(radius)), by = .(n, content, rule)]
  cells[, radius := NULL]; cells <- merge(cells, scales, by = c("n", "content", "rule"))
  baseline <- merge(cells[rule == "baseline", .(n, content, response_id, mean_base_width)],
    old_policy[, .(n, content, multiplier)], by = c("n", "content"))
  baseline[, old_width := mean_base_width*multiplier]
  cells <- merge(cells, baseline[, .(n, content, response_id, old_width)], by = c("n", "content", "response_id"))
  cells[, width_ratio := radius*mean_base_width/old_width]
  scores <- cells[, .(worst_ratio = max(width_ratio),
    asymmetric_ratio = exp(mean(log(width_ratio[response_id %in% cfg$asymmetric_laws]))),
    all_returned = all(returned == samples & adequate) && all(is.finite(width_ratio))),
    by = .(n, content, tolerance_confidence, rule, gamma)]
  scores[, eligible := all_returned & worst_ratio <= 1+cfg$materiality$maximum_case_increase &
    asymmetric_ratio <= 1-cfg$materiality$minimum_asymmetric_reduction]
  policy <- scores[, {
    z <- .SD[eligible]
    if (!nrow(z)) {
      z <- .SD[rule == "baseline"]; z$advance <- FALSE
    } else {
      data.table::setorderv(z, c("worst_ratio", "asymmetric_ratio", "gamma"), c(1, 1, -1))
      best <- z[1]
      same <- z[rule == "baseline"]
      z <- if (nrow(same) && same$asymmetric_ratio <= best$asymmetric_ratio*(1+cfg$materiality$prefer_unchanged_within)) same else best
      z$advance <- TRUE
    }
    z
  }, by = .(n, content, tolerance_confidence)]
  list(policy = policy, scores = scores, cells = cells, exploratory_probability = probability)
}

mdc_calibrate <- function(rows, cfg) {
  d <- data.table::as.data.table(rows)
  counts <- d[, .N, by = .(n, content, response_id, rule)]
  mwd_require(nrow(counts) == 2*cfg$expected_cells && all(counts$N == cfg$stages$calibration$replications) &&
    all(d$adequate), "Independent calibration is incomplete or numerically unresolved.")
  cells <- d[, {
    rank <- mbc_rank(.N, tolerance_confidence[1], cfg$calibration_error, cfg$protected_comparisons)
    .(samples = .N, rank = rank$rank, assurance = rank$assurance, scale = sort(required_scale)[rank$rank])
  }, by = .(n, content, tolerance_confidence, response_id, rule)]
  policy <- cells[, .(multiplier = max(scale), cases = .N, rank = unique(rank)),
    by = .(n, content, tolerance_confidence, rule)]
  mwd_require(all(policy$cases == 8), "The calibration panel changed.")
  list(policy = policy, cells = cells)
}
