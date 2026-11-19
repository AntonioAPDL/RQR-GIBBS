mjs_dependency_status <- function(directory) {
  read <- function(name) {
    path <- file.path(directory, name)
    if (file.exists(path)) tryCatch(mwd_json(path), error = function(e) NULL) else NULL
  }
  exit <- read("controller_exit.json"); summary <- read("study_summary.json")
  if (is.null(exit)) return("waiting_for_predecessor")
  if (identical(exit$exit_code, 0L) && identical(summary$status, "complete_requires_scientific_review"))
    "ready" else "dependency_failed"
}

mjs_context <- function(cfg, dependency) {
  mjc_validate(cfg)
  predecessor <- mwd_json(cfg$predecessor_config)
  predecessor$output_root <- cfg$output_root
  context <- mdc_context(predecessor)
  mwd_require(dependency$source_commit == mwd_json(file.path(dependency$output_root, "manifest.json"))$identity$source_commit &&
    dependency$selection_sha256 == mwd_file_hash(file.path(dependency$output_root, "frozen_selection.rds")),
    "The frozen predecessor target rule changed.")
  reference <- data.table::as.data.table(readRDS(file.path(dependency$output_root, "frozen_selection.rds")))
  reference <- reference[rule == "proposed"]
  mwd_require(nrow(reference) == 9 && !anyDuplicated(reference[, .(n, content)]), "The R0 target rule is incomplete.")
  context$reference <- reference[, .(n, content, tolerance_confidence, rule = "R0", gamma,
    prior_sd = 2, concentration = 1, base_sd = 4, model = "sd2_a1_h4", reference_dp = TRUE)]
  context$dependency <- dependency
  radius_path <- file.path(dependency$output_root, "calibrated_radius.rds")
  frozen_radius <- mwd_json(file.path(dependency$output_root, "frozen_radius.json"))
  mwd_require(identical(frozen_radius$radius_sha256, mwd_file_hash(radius_path)) &&
    identical(frozen_radius$selection_sha256, dependency$selection_sha256), "Frozen predecessor radius changed.")
  context$radius <- readRDS(radius_path)
  mjc_reference_radius(context$radius, context$reference)
  context$frozen_radius <- frozen_radius
  saved <- mwd_json(file.path(context$boundary, "manifest.json"))$identity
  mwd_require(identical(saved$configuration$sampling, cfg$sampling), "Saved Gibbs sampling settings differ.")
  mgfp_check_hashes(saved$programs[c("application/scripts/lib/mti_gibbs_practical.R",
    "application/scripts/lib/mti_gibbs_width_diagnosis.R", "application/config/mti_gibbs_practical_20261112.json",
    "application/scripts/lib/mti_fixed_prior.R")])
  context
}

mjs_designs <- function(cfg, context) {
  d <- context$old_tasks[context$old_tasks$replication <= cfg$development_replications, ]
  d$old_job_id <- d$job_id; d$job_id <- seq_len(nrow(d))
  d$ecm_seed <- as.integer(cfg$seeds$development_ecm_base + 10 * d$job_id)
  d$chain_seed <- as.integer(cfg$seeds$development_chain_base + 200 * d$job_id)
  v <- mps_design(list(expected_cells = cfg$expected_cells,
    replications_per_cell = cfg$validation_replications, data_seed_base = cfg$seeds$validation_data_base,
    kappa_grid = 2), context)
  v$ecm_seed <- as.integer(cfg$seeds$validation_ecm_base + 10 * v$job_id)
  v$chain_seed <- as.integer(cfg$seeds$validation_chain_base + 200 * v$job_id)
  list(development = d, validation = v)
}

mjs_reserved_blocks <- function(base, blocks, stride, forbidden, pool) {
  occupied <- unique(floor((forbidden - base) / stride))
  bad <- which(blocks %in% occupied)
  available <- setdiff(pool, c(occupied, blocks))
  mwd_require(length(available) >= length(bad), "Independent stream reserve exhausted.")
  blocks[bad] <- head(available, length(bad))
  base + stride * blocks
}

mjs_seed_audit <- function(designs, cfg, context) {
  predecessor <- mwd_json(cfg$predecessor_config)
  previous <- mdc_seed_plan(mdc_designs(predecessor, context), predecessor, context)$designs
  occupied <- unlist(lapply(previous, function(d) c(d$data_seed, d$ecm_seed,
    unlist(lapply(1:3, function(j) as.vector(outer(d[[paste0("chain_base", j)]], 1:4, "+")))))))
  for (directory in c(context$parent, context$boundary)) for (stage in c("development", "calibration", "validation", "pilot")) {
    path <- file.path(directory, stage, "tasks.csv")
    if (file.exists(path)) occupied <- c(occupied, data.table::fread(path)$data_seed)
  }
  for (stage in names(context$prior$stages)) {
    d <- data.table::fread(file.path(context$parent, stage, "tasks.csv"))
    base <- context$prior$stages[[stage]]$chain_seed_base + 100 * d$job_id
    occupied <- c(occupied, base, as.vector(outer(base, 1:4, "+")))
  }
  boundary <- mwd_json(predecessor$boundary_config)
  bd <- mbc_designs(boundary, context)
  for (stage in names(bd)) {
    base <- if (stage == "development") boundary$development_chain_seed_base else boundary$stages[[stage]]$chain_seed_base
    base <- base + 100 * bd[[stage]]$job_id
    occupied <- c(occupied, base, unlist(lapply(1:3, function(j) as.vector(outer(base + 10 * j, 1:4, "+")))))
  }
  historical <- readRDS(file.path(mgfp_common(), mwd_json(boundary$diagnostic_config)$campaign, "jobs.rds"))
  occupied <- c(occupied, historical$seed, historical$data_seed)
  # Reassign occupied slots rather than assuming old diagnostic seeds stay in blocks.
  mwd_require(min(unlist(cfg$seeds)) > 2090000000, "New study streams must use their separate reserved blocks.")
  replacements <- 0L
  for (stage in names(designs)) {
    d <- designs[[stage]]
    if (stage == "validation") for (cell in unique(d$cell_id)) {
      ids <- which(d$cell_id == cell); bad <- ids[d$data_seed[ids] %in% occupied]
      pool <- cfg$seeds$validation_data_base + 10000 * cell + 1001:9999
      pool <- setdiff(pool, c(occupied, d$data_seed[ids]))
      mwd_require(length(pool) >= length(bad), "Independent observation-seed reserve exhausted.")
      d$data_seed[bad] <- as.integer(head(pool, length(bad))); replacements <- replacements + length(bad)
    }
    chain_base <- cfg$seeds[[paste0(stage, "_chain_base")]]
    ecm_base <- cfg$seeds[[paste0(stage, "_ecm_base")]]
    pool <- if (stage == "development") 8001:9999 else 80001:99999
    chain <- mjs_reserved_blocks(chain_base, d$job_id, 200, occupied, pool)
    ecm <- mjs_reserved_blocks(ecm_base, d$job_id, 10, occupied, pool)
    replacements <- replacements + sum(d$chain_seed != chain) + sum(d$ecm_seed != ecm)
    d$chain_seed <- as.integer(chain); d$ecm_seed <- as.integer(ecm)
    designs[[stage]] <- d
  }
  streams <- lapply(names(designs), function(stage) {
    d <- designs[[stage]]; slots <- if (stage == "development") 7 else 2
    c(if (stage == "validation") d$data_seed,
      as.vector(outer(d$ecm_seed, seq_along(cfg$root_sd), "+")),
      unlist(lapply(seq_len(slots), function(j) as.vector(outer(d$chain_seed + 10 * j, 1:4, "+")))))
  })
  seeds <- c(unlist(streams), cfg$seeds$rehearsal_chain_base + seq_len(100))
  overlap <- intersect(seeds, occupied)
  duplicate <- unique(seeds[duplicated(seeds)])
  mwd_require(all(is.finite(seeds) & seeds > 0 & seeds < .Machine$integer.max) &&
    !length(duplicate) && !length(overlap), paste("Data, ECM or chain seed allocations overlap.",
      "Repeated:", paste(head(duplicate), collapse = ","), "Historical:", paste(head(overlap), collapse = ",")))
  list(designs = designs, audit = list(streams_reserved = length(seeds),
    collision_slots_reassigned = replacements, previous_seed_set_sha256 = mwd_hash(sort(unique(occupied))),
    study_seed_set_sha256 = mwd_hash(sort(seeds)), tasks_sha256 = lapply(designs, mwd_hash),
    development_observations_reused_deliberately = TRUE, fresh_validation_separate = TRUE))
}

mjs_old <- function(task, context) {
  original <- context$old_tasks[context$old_tasks$job_id == task$old_job_id, ]
  mwd_require(nrow(original) == 1 && original$data_seed == task$data_seed, "Development source task changed.")
  mdc_old(original, context)
}

mjs_modes <- function(task, cfg, context) {
  set.seed(task$data_seed); y <- context$laws[[task$response_id]]$r(task$n)
  old <- mjs_old(task, context)
  mwd_require(old$anchor$prior$prior_sd == 2 && old$anchor$sample_sha256 == mwd_hash(y),
    "The reusable ECM candidates or observations differ.")
  anchors <- list()
  for (sd in cfg$root_sd) {
    a <- if (sd == 2) old$anchor else mjc_fit_anchor(y, task, sd, cfg, context)
    mwd_require(all(a$candidates$finite_interval & a$candidates$numerically_accurate), "Unresolved saved ECM candidates.")
    anchors[[as.character(sd)]] <- a[intersect(names(a), c("prior", "sample_sha256", "candidates", "dp_prior",
      "selected", "selected_ecm_trace", "selected_knot_adjustments", "q", "delta_reference"))]
  }
  list(ok = TRUE, task = task, anchors = anchors,
    old_receipt = mwd_file_hash(file.path(context$boundary, "calibration/fits", sprintf("fit_%06d.rds.json", task$old_job_id))))
}

mjs_queue <- function(tasks, stage, cfg, identity, workers, fit) {
  directory <- file.path(cfg$output_root, stage)
  dir.create(file.path(directory, "fits"), recursive = TRUE, showWarnings = FALSE)
  id <- mwd_hash(list(identity, stage, tasks))
  path <- file.path(directory, "manifest.json")
  if (file.exists(path)) mwd_require(mwd_json(path)$analysis_id == id, "Saved stage inputs changed.") else
    mgfp_json_write(list(analysis_id = id, identity = identity), path)
  mwd_write(tasks, file.path(directory, "tasks.csv"))
  paths <- file.path(directory, "fits", sprintf("fit_%06d.rds", tasks$job_id))
  present <- which(file.exists(paste0(paths, ".json")))
  mwd_require(!any(file.exists(paths) & !file.exists(paste0(paths, ".json"))), "An incomplete saved fit requires review.")
  for (i in present) mwd_require(isTRUE(mbc_read_fit(paths[i], id, tasks[i, ])$ok),
    "A saved numerical failure requires review; it will not be overwritten.")
  started <- Sys.time(); resumed <- length(present)
  pending <- setdiff(seq_len(nrow(tasks)), present)
  for (batch in split(pending, ceiling(seq_along(pending) / workers))) {
    mdc_capacity(directory, cfg, nrow(tasks) - length(present), paths[head(present, 200)])
    result <- parallel::mclapply(batch, function(i) {
      tic <- proc.time()[3]
      value <- tryCatch(fit(tasks[i, ]), error = function(e) list(ok = FALSE, task = tasks[i, ], reason = conditionMessage(e)))
      value$elapsed_seconds <- unname(proc.time()[3] - tic)
      mbc_atomic_fit(value, paths[i], id, tasks[i, ])
      value[c("ok", "reason")]
    }, mc.cores = min(workers, length(batch)), mc.set.seed = FALSE)
    passed <- all(vapply(result, function(x) is.list(x) && isTRUE(x$ok), FALSE))
    present <- c(present, batch)
    elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
    mgfp_json_write(list(stage = stage, completed_datasets = length(present), expected_datasets = nrow(tasks),
      remaining_datasets = nrow(tasks) - length(present), workers = workers, numerical_pass = passed,
      eta_seconds = elapsed / (length(present) - resumed) * (nrow(tasks) - length(present)), time = mgfp_time()),
      file.path(directory, "progress.json"))
    cat(mgfp_time(), stage, length(present), "/", nrow(tasks), "saved\n"); flush.console()
    mwd_require(passed, "A saved numerical calculation requires review; no dataset has been replaced.")
  }
  list(paths = paths, analysis_id = id)
}

mjs_read <- function(queue, tasks, i) mbc_read_fit(queue$paths[i], queue$analysis_id, tasks[i, ])

mjs_replay <- function(tasks, modes, cfg, context, workers) {
  models <- mjc_models(cfg)
  groups <- split(seq_len(nrow(tasks)), interaction(tasks$n, tasks$content, drop = TRUE))
  result <- parallel::mclapply(groups, function(ids) {
    d <- tasks[ids, ]; r0 <- as.data.frame(context$reference[n == d$n[1] & content == d$content[1]])
    values <- lapply(ids, function(i) mjs_read(modes, tasks, i))
    ys <- lapply(seq_len(nrow(d)), function(i) { set.seed(d$data_seed[i]); context$laws[[d$response_id[i]]]$r(d$n[i]) })
    base_widths <- vapply(seq_len(nrow(d)), function(i) mjc_select(ys[[i]], values[[i]]$anchors[["2"]],
      r0, d$content[i], "original")$selected$width, 0)
    reference <- tapply(base_widths, d$response_id, mean)
    curves <- lapply(seq_len(nrow(models)), function(j) {
      model <- models[j, ]
      paths <- lapply(seq_len(nrow(d)), function(i) {
        z <- mdc_path(mjc_reweight(values[[i]]$anchors[[as.character(model$prior_sd)]]$candidates,
          ys[[i]], d$content[i], model))
        mwd_require(nrow(z) > 0, "A DP comparison has no finite candidate path.")
        z$response_id <- d$response_id[i]
        cdf <- context$laws[[d$response_id[i]]]$p
        z$population_content <- cdf(z$upper) - cdf(z$lower)
        z
      })
      curve <- mjc_curve(paths, reference, r0$gamma, d$content[1], d$tolerance_confidence[1])
      curve <- data.frame(n = d$n[1], content = d$content[1], tolerance_confidence = d$tolerance_confidence[1],
        model[rep(1L, nrow(curve)), , drop = FALSE], curve, row.names = NULL)
      curve$rule <- vapply(curve$gamma, function(g) paste0(model$model, "_", substr(mwd_hash(g), 1, 16)), "")
      curve
    })
    curves <- data.table::rbindlist(curves)
    nominated <- mjc_nominate(curves)
    nominated$slot <- seq_len(nrow(nominated)) + 1L
    r0$slot <- 1L
    list(curves = curves, selection = data.table::rbindlist(list(r0, nominated), fill = TRUE),
      candidate_count = sum(vapply(values, function(v) sum(vapply(v$anchors, function(a) nrow(a$candidates), 1L)), 1L)))
  }, mc.cores = min(workers, length(groups)), mc.set.seed = FALSE)
  mwd_require(!any(vapply(result, inherits, FALSE, "try-error")), "The joint DP comparison requires review.")
  curves <- data.table::rbindlist(lapply(result, `[[`, "curves"))
  selection <- data.table::rbindlist(lapply(result, `[[`, "selection"), fill = TRUE)
  mwd_require(all(selection[, .N, by = .(n, content)]$N <= cfg$maximum_initial_rules), "Initial rule limit exceeded.")
  mwd_write(curves, file.path(cfg$output_root, "ecm_threshold_comparison.csv"))
  mwd_write(selection, file.path(cfg$output_root, "initial_selection.csv"))
  mgfp_json_write(list(status = "separate_scientific_review", full_profile_automatic = FALSE,
    checked_ecm_candidates = sum(vapply(result, `[[`, 1L, "candidate_count")),
    reason = "Assess the bounded ECM-selected Gibbs rules before authorizing multiple posterior targets per rule."),
    file.path(cfg$output_root, "candidate_profile_review.json"))
  list(curves = curves, selection = selection)
}

mjs_gibbs <- function(task, anchors, selection, cfg, context, previous = NULL, old = NULL) {
  set.seed(task$data_seed); y <- context$laws[[task$response_id]]$r(task$n)
  variant <- context$old_tilts$variant[context$old_tilts$n == task$n & context$old_tilts$content == task$content]
  rules <- as.data.frame(selection[selection$n == task$n & selection$content == task$content, ])
  mwd_require(length(variant) == 1 && nrow(rules) > 0, "Missing complete target selection rule.")
  records <- previous$records %||% list(); rows <- list(); fresh <- 0L
  for (j in seq_len(nrow(rules))) {
    rule <- rules[j, ]; anchor <- anchors[[as.character(rule$prior_sd)]]
    mwd_require(anchor$sample_sha256 == mwd_hash(y), "The selected observations changed.")
    target <- mjc_select(y, anchor, rule, task$content, variant)
    row <- data.frame(task, rule = rule$rule, prior_sd = rule$prior_sd, concentration = rule$concentration,
      base_sd = rule$base_sd, gamma = rule$gamma, method = rule$rule, radius_multiplier = 1,
      returned = FALSE, adequate = TRUE, lower = NA_real_, upper = NA_real_, width = NA_real_,
      population_content = NA_real_, attained = FALSE, failure_reason = "no_accurate_eligible_candidate")
    if (is.null(target)) { rows[[j]] <- row; next }
    key <- mjc_target_key(y, target, rule$prior_sd, cfg, context$policy_diagnostics)
    if (is.null(records[[key]])) {
      saved <- if (!is.null(old)) old$records[[variant]] else NULL
      reusable <- !is.null(saved) && rule$prior_sd == 2 && isTRUE(saved$adequate) &&
        all(c("q", "delta") %in% names(saved$parameters)) && nrow(saved$parameters) > 0 &&
        all(saved$parameters$q == target$q) && all(saved$parameters$delta == target$geometry$delta)
      if (reusable) {
        saved$origin <- list(type = "checked_boundary_fit", old_job_id = task$old_job_id,
          source_receipt = mwd_file_hash(file.path(context$boundary, "calibration/fits", sprintf("fit_%06d.rds.json", task$old_job_id))))
        records[[key]] <- saved
      } else {
        records[[key]] <- mjr_sample_checked(y, target, rule$prior_sd, cfg, context$policy_diagnostics,
          as.integer(task$chain_seed + 10 * rule$slot))
        fresh <- fresh + 1L
      }
    }
    record <- records[[key]]; endpoints <- mjc_interval(record)
    row$lower <- endpoints[1]; row$upper <- endpoints[2]; row$width <- diff(endpoints)
    row$returned <- TRUE; row$adequate <- record$adequate; row$failure_reason <- if (record$adequate) "" else "sampling_accuracy_requires_review"
    row$q <- target$q; row$original_delta <- target$delta_reference; row$delta <- target$geometry$delta
    row$tilt_variant <- variant; row$original_margin <- target$geometry$original_margin
    row$selected_candidate <- target$selected$candidate_index
    row$ecm_lower <- target$selected$lower; row$ecm_upper <- target$selected$upper
    row$ecm_dp_probability <- target$selected$posterior_content_probability
    row$dp_probability <- mjc_probabilities(y, endpoints[1], endpoints[2], task$content, rule$concentration, rule$base_sd)
    row$target_key <- key; row$retained_per_chain <- nrow(record$fits[[1]]$draws)
    row$max_rhat <- max(record$parameters$rhat); row$min_bulk_ess <- min(record$parameters$ess_bulk)
    row$min_tail_ess <- min(record$parameters$ess_tail); row$max_mcse_mean <- max(record$parameters$mcse_mean)
    row$sample_sha256 <- mwd_hash(y)
    rows[[j]] <- row
  }
  # Population information enters only after every fitting decision is complete.
  rows <- data.table::rbindlist(rows, fill = TRUE)
  cdf <- context$laws[[task$response_id]]$p
  rows[returned == TRUE, population_content := cdf(upper) - cdf(lower)]
  rows[, attained := returned & population_content >= content]
  list(ok = all(vapply(records, function(r) isTRUE(r$adequate), FALSE)), task = task,
    records = records, rows = rows, new_targets = fresh)
}

mjs_collect <- function(queue, tasks) data.table::rbindlist(lapply(seq_len(nrow(tasks)), function(i)
  mjs_read(queue, tasks, i)$rows), fill = TRUE)

mjs_refinement <- function(curves, initial, scores, cfg) {
  out <- list()
  for (n0 in unique(initial$n)) for (c0 in unique(initial[initial$n == n0, ]$content)) {
    s <- scores[n == n0 & content == c0]
    if (any(s$advance)) next
    previous <- initial[initial$n == n0 & initial$content == c0, ]
    alternatives <- mjc_nominate(curves[n == n0 & content == c0], previous$rule, refinement = TRUE)
    if (nrow(alternatives)) {
      alternatives$slot <- max(previous$slot) + seq_len(nrow(alternatives))
      out[[length(out) + 1L]] <- alternatives
    }
  }
  if (!length(out)) return(initial[0, ])
  data.table::rbindlist(out, fill = TRUE)
}

mjs_freeze <- function(scores, selections, cfg) {
  choices <- list()
  for (n0 in unique(selections$n)) for (c0 in unique(selections[selections$n == n0, ]$content)) {
    d <- scores[n == n0 & content == c0 & advance == TRUE]
    d <- d[order(worst_ratio, mean_log_ratio, rule)]
    selected_rule <- if (nrow(d)) d$rule[1] else "R0"
    chosen <- selections[selections$n == n0 & selections$content == c0 & selections$rule == selected_rule, ]
    reference <- selections[selections$n == n0 & selections$content == c0 & selections$rule == "R0", ]
    mwd_require(nrow(chosen) == 1 && nrow(reference) == 1, "A design selection is ambiguous.")
    reference$method <- "gibbs_R0_unscaled"; reference$advanced <- FALSE; reference$slot <- 1L
    if (selected_rule != "R0") {
      chosen$method <- "gibbs_new_unscaled"; chosen$advanced <- TRUE; chosen$slot <- 2L
      choices[[length(choices) + 1L]] <- data.table::rbindlist(list(reference, chosen), fill = TRUE)
    } else choices[[length(choices) + 1L]] <- reference
  }
  data.table::rbindlist(choices, fill = TRUE)
}

mjs_validation_domain <- function(tasks, frozen) {
  keys <- c("n", "content", "tolerance_confidence")
  advancing <- unique(data.table::as.data.table(frozen)[advanced == TRUE, ..keys])
  d <- data.table::as.data.table(tasks)
  selected <- merge(d, advancing, by = keys)
  data.table::setorder(selected, job_id)
  as.data.frame(selected[, names(d), with = FALSE])
}

mjs_final_job <- function(task, frozen, cfg, context) {
  set.seed(task$data_seed); y <- context$laws[[task$response_id]]$r(task$n)
  rules <- as.data.frame(frozen[frozen$n == task$n & frozen$content == task$content, ])
  anchors <- lapply(unique(c(2, rules$prior_sd)), function(sd) mjc_fit_anchor(y, task, sd, cfg, context))
  names(anchors) <- as.character(unique(c(2, rules$prior_sd)))
  mwd_require(sum(rules$advanced) == 1 && nrow(rules) == 2,
    "Independent assessment requires an advancing alternative, without substitution.")
  rules$rule <- rules$method
  value <- mjs_gibbs(task, anchors, rules, cfg, context)
  r0 <- data.table::copy(value$rows[rule == "gibbs_R0_unscaled"])
  radius <- context$radius[context$radius$n == task$n & context$radius$content == task$content & context$radius$rule == "proposed", ]
  mwd_require(nrow(radius) == 1 && is.finite(radius$multiplier) && radius$multiplier > 0, "Missing frozen R0 radius.")
  r0$method <- r0$rule <- "gibbs_R0_scaled"; r0$radius_multiplier <- radius$multiplier
  if (r0$returned) {
    ends <- mbc_interval((r0$lower + r0$upper) / 2, r0$width / 2, radius$multiplier)
    r0$lower <- ends[1]; r0$upper <- ends[2]; r0$width <- diff(ends)
    r0$population_content <- diff(context$laws[[task$response_id]]$p(ends))
    r0$attained <- r0$population_content >= task$content
    r0$dp_probability <- mjc_probabilities(y, ends[1], ends[2], task$content, 1, 4)
  }
  primary <- mdc_select(anchors[["2"]]$candidates,
    context$policy$screen[context$policy$n == task$n & context$policy$content == task$content])
  comparators <- mfp_comparators(y, task$content, task$tolerance_confidence)
  comparators <- data.table::rbindlist(list(comparators, data.frame(method = "ecm_primary",
    returned = nrow(primary) == 1, lower = if (nrow(primary)) primary$lower else NA_real_,
    upper = if (nrow(primary)) primary$upper else NA_real_, width = if (nrow(primary)) primary$width else NA_real_,
    failure_reason = if (nrow(primary)) "" else "no_accurate_eligible_candidate")), fill = TRUE)
  comparators <- cbind(task[rep(1, nrow(comparators)), ], comparators)
  comparators$population_content <- context$laws[[task$response_id]]$p(comparators$upper) -
    context$laws[[task$response_id]]$p(comparators$lower)
  comparators$attained <- comparators$returned & comparators$population_content >= task$content
  value$rows <- data.table::rbindlist(list(value$rows, r0, comparators), fill = TRUE)
  value
}

mjs_preserve <- function(cfg, identity) {
  files <- c("manifest.json", "dependency.json", "initial_selection.csv", "ecm_threshold_comparison.csv",
    "frozen_data_generators.R", "modes/tasks.csv", "modes/manifest.json",
    "development/tasks.csv", "development/manifest.json", "refinement/tasks.csv", "refinement/manifest.json",
    "development_results.csv", "development_scores.csv", "development_cells.csv", "refinement_selection.csv",
    "frozen_selection.csv", "frozen_selection.rds", "frozen_policy.json", "candidate_profile_review.json",
    "reuse_source.json", "modes/reuse_receipt.json", "development/reuse_receipt.json", "eligible_validation_tasks.rds",
    "validation/interval_results.csv", "validation/comparison_summary.csv", "validation/paired_comparison.csv",
    "validation/tasks.csv", "validation/manifest.json", "validation/width_and_attainment.pdf",
    "study_summary.json", "rehearsal.json", "rehearsal_traces.pdf")
  files <- files[file.exists(file.path(cfg$output_root, files))]
  mgfp_json_write(list(source_commit = identity$source_commit, identity_hash = mwd_hash(identity),
    artifacts = as.list(setNames(vapply(file.path(cfg$output_root, files), mwd_file_hash, ""), files)),
    article_unchanged = TRUE, time = mgfp_time()), file.path(cfg$output_root, "evidence_receipt.json"))
}

mjs_verify <- function(directory, cfg) {
  receipt <- mwd_json(file.path(directory, "evidence_receipt.json"))
  for (name in names(receipt$artifacts)) mwd_require(file.exists(file.path(directory, name)) &&
    mwd_file_hash(file.path(directory, name)) == receipt$artifacts[[name]], paste("Evidence changed:", name))
  path <- file.path(directory, "validation/interval_results.csv")
  if (file.exists(path)) {
    rows <- data.table::fread(path); expected <- data.table::fread(file.path(directory, "validation/comparison_summary.csv"))
    mwd_require(!anyDuplicated(rows[, .(job_id, method)]) && !anyNA(rows$attained) &&
      all(rows$attained == (rows$returned & rows$population_content >= rows$content)) &&
      all(abs(rows$width[rows$returned] - (rows$upper[rows$returned] - rows$lower[rows$returned])) <
        1e-9 * pmax(1, rows$width[rows$returned])), "Interval or attainment records are inconsistent.")
    actual <- mjc_summary(rows, cfg)
    keys <- c("n", "content", "response_id", "method")
    data.table::setorderv(actual, keys); data.table::setorderv(expected, keys)
    domain <- readRDS(file.path(directory, "eligible_validation_tasks.rds"))
    frozen <- mwd_json(file.path(directory, "frozen_policy.json"))
    mwd_require(frozen$validation_tasks_sha256 == mwd_hash(domain), "Frozen validation domain changed.")
    expected_cases <- unique(data.table::as.data.table(domain)[, .(n, content, response_id)])
    methods <- c("gibbs_new_unscaled", "gibbs_R0_unscaled", "gibbs_R0_scaled", "ecm_primary", "young_mathew", "wilks")
    mwd_require(nrow(rows) == nrow(domain) * length(methods) && setequal(rows$method, methods) &&
      setequal(rows$job_id, domain$job_id) &&
      nrow(actual) == nrow(expected_cases) * length(methods) &&
      all(actual$samples == cfg$validation_replications) &&
      isTRUE(all.equal(as.data.frame(actual), as.data.frame(expected), tolerance = 1e-10, check.attributes = FALSE)),
      "The independent comparison does not reproduce its interval data.")
  }
  cat("Joint-study evidence checks passed.\n"); invisible(TRUE)
}

mjs_report <- function(rows, cfg) {
  d <- data.table::as.data.table(rows)
  reference <- d[method == "gibbs_R0_scaled", .(job_id, reference_width = width,
    reference_returned = returned, reference_attained = attained)]
  paired <- merge(d[method == "gibbs_new_unscaled"], reference, by = "job_id")
  comparison <- paired[, {
    differences <- width - reference_width
    se <- if (all(returned & reference_returned)) sd(differences) / sqrt(.N) else NA_real_
    .(samples = .N, complete_pairs = sum(returned & reference_returned),
      mean_paired_width_change = if (all(returned & reference_returned)) mean(differences) else NA_real_,
      paired_width_mcse = se,
      paired_width_lower = if (is.finite(se)) mean(differences) - qt(.975, .N - 1) * se else NA_real_,
      paired_width_upper = if (is.finite(se)) mean(differences) + qt(.975, .N - 1) * se else NA_real_,
      newly_attained = sum(attained & !reference_attained), lost_attainment = sum(!attained & reference_attained))
  }, by = .(n, content, response_id)]
  comparison[, reference_method := "gibbs_R0_scaled"]
  mwd_write(comparison, file.path(cfg$output_root, "validation/paired_comparison.csv"))
  summary <- mjc_summary(rows, cfg)
  methods <- c("gibbs_new_unscaled", "gibbs_R0_unscaled", "gibbs_R0_scaled", "ecm_primary", "young_mathew", "wilks")
  colors <- c("#0072B2", "#CC79A7", "#E69F00", "#009E73", "#D55E00", "black")
  grDevices::pdf(file.path(cfg$output_root, "validation/width_and_attainment.pdf"), width = 13, height = 9)
  on.exit(grDevices::dev.off())
  for (n0 in unique(summary$n)) for (c0 in unique(summary[n == n0, content])) {
    for (kind in c("width", "attainment")) {
      par(mfrow = c(2, 4), mar = c(7, 4, 3, 1))
      for (law in unique(summary$response_id)) {
        a <- summary[n == n0 & content == c0 & response_id == law]
        a <- a[match(methods, method)]
        estimate <- if (kind == "width") a$mean_width else a$attainment
        lower <- if (kind == "width") a$width_q025 else a$attainment_lower
        upper <- if (kind == "width") a$width_q975 else a$attainment_upper
        ylim <- if (kind == "width") range(c(lower, upper), finite = TRUE) else c(min(.85, lower), 1)
        if (!all(is.finite(ylim))) ylim <- c(0, 1)
        plot(seq_along(methods), estimate, ylim = ylim, xaxt = "n", xlab = "",
          ylab = if (kind == "width") "Interval width" else "Attainment probability", pch = 4, col = colors,
          main = paste(law, "n =", n0, "c =", c0), cex.main = .8)
        segments(seq_along(methods), lower, seq_along(methods), upper, col = colors)
        if (kind == "attainment") abline(h = a$tolerance_confidence[1], lty = 2, col = "#D55E00")
        axis(1, at = seq_along(methods), labels = methods, las = 2, cex.axis = .65)
      }
    }
  }
  invisible(comparison)
}
