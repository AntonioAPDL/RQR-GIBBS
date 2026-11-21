mac_context <- function(cfg, directory) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  dependency <- file.path(directory, "dependency.json")
  if (file.exists(dependency)) sources <- mwd_json(dependency) else {
    sources <- list(sampling = mwd_json(file.path(mgfp_common(), ".codex_work/mti_ecm_nominal_latest.json")),
      finalization = mwd_json(file.path(mgfp_common(), ".codex_work/mti_ecm_nominal_finalized_latest.json")))
    mgfp_json_write(sources, dependency)
  }
  mwd_require(sources$sampling$source_commit == cfg$source_assessment_commit &&
    sources$finalization$analysis_commit == cfg$source_analysis_commit &&
    identical(sources$finalization$sampling_output_root, sources$sampling$output_root),
    "The declared completed development source differs.")
  source <- mec_sampling_source(sources$sampling)
  final <- mwd_json(file.path(sources$finalization$output_root, "verification.json"))
  mwd_require(final$passed && final$datasets == 72000 && !final$prior_uses_analysis_data,
    "The historical finalization has not been verified.")
  prior <- mwd_json("application/config/mti_fixed_prior_20261114.json")
  context <- mps_context(prior, directory)
  context$prior <- prior
  context$package <- mps_package_check()
  context$policy_diagnostics <- mwd_json("application/config/mti_gibbs_practical_20261112.json")$policy
  mwd_require(context$generator_sha256 == source$manifest$generators &&
    context$dp$concentration == cfg$dp_concentration && context$dp$base$mean == 0 &&
    context$dp$base$sd == cfg$dp_base_sd && prior$primary_kappa == cfg$prior_sd &&
    prior$simulation_reference$center == 0 && prior$simulation_reference$scale == 1 &&
    !prior$simulation_reference$estimated_from_analysis_sample,
    "A generator, DP prior, root prior or independent coordinate specification changed.")
  context$sources <- sources
  context$source <- source
  context$source_tasks <- readRDS(file.path(source$directory, "validation_tasks.rds"))
  mwd_require(nrow(context$source_tasks) == cfg$expected_cells*cfg$development_replications &&
    !anyDuplicated(context$source_tasks$job_id) && !anyDuplicated(context$source_tasks$data_seed),
    "Historical development tasks are missing or duplicated.")
  context$source_stage <- mwd_json(file.path(source$directory, "validation/manifest.json"))$analysis_id
  context
}

mac_cases <- function(context) {
  d <- unique(context$source_tasks[, c("n", "content", "tolerance_confidence", "response_id")])
  rownames(d) <- NULL; d$job_id <- seq_len(nrow(d)); d
}

mac_designs <- function(context) {
  d <- unique(context$source_tasks[, c("n", "content", "tolerance_confidence")])
  rownames(d) <- NULL; d$job_id <- seq_len(nrow(d)); d
}

mac_old <- function(task, context) {
  file <- file.path(context$source$directory, "validation/fits", sprintf("fit_%06d.rds", task$job_id))
  value <- mbc_read_fit(file, context$source_stage, task)
  set.seed(task$data_seed); y <- context$laws[[task$response_id]]$r(task$n)
  mwd_require(value$ok && value$unresolved_candidates == 0 &&
    identical(value$sample_sha256, mwd_hash(y)) && value$prior$prior_sd == 2 &&
    value$prior$center == 0 && value$prior$scale == 1 && value$prior$learning_rate == 1 &&
    identical(value$prior$data_adaptive_prior, FALSE) && all(value$candidates$numerically_accurate) &&
    all(value$candidates$finite_interval), "Saved sample, prior or candidate accuracy differs.")
  list(value = value, y = y, file = file, receipt = mwd_file_hash(paste0(file, ".json")))
}

mac_fitting_inputs <- function(task, context) {
  fields <- c("n", "content", "tolerance_confidence", "retained_fraction", "ecm_seed")
  mwd_require(all(fields %in% names(task)), "An ECM fitting specification is incomplete.")
  list(task = task[, fields, drop = FALSE], context = list(policy = context$policy,
    prior = context$prior[c("simulation_reference", "numerical")], profile = context$profile,
    dp = context$dp, policy_diagnostics = context$policy_diagnostics))
}

mac_dp_check <- function(y, candidates, content, cfg) {
  z <- sort(y)
  counts <- findInterval(candidates$upper, z) - findInterval(candidates$lower, z, left.open = TRUE)
  mass <- pnorm(candidates$upper, sd = cfg$dp_base_sd) - pnorm(candidates$lower, sd = cfg$dp_base_sd)
  a <- counts + cfg$dp_concentration*mass
  b <- length(y) - counts + cfg$dp_concentration*(1 - mass)
  probability <- numeric(length(a)); good <- a > 0 & b > 0
  probability[good] <- pbeta(content, a[good], b[good], lower.tail = FALSE)
  probability[b == 0 & a > 0] <- 1
  mwd_require(all(counts == candidates$observed_count) &&
    max(abs(probability - candidates$posterior_content_probability)) < 1e-12,
    "Reconstructed closed-interval DP probabilities differ.")
  invisible(TRUE)
}

mac_audit_case <- function(case, cfg, context) {
  ids <- which(context$source_tasks$n == case$n & context$source_tasks$content == case$content &
    context$source_tasks$response_id == case$response_id)
  tasks <- context$source_tasks[ids, , drop = FALSE]
  mwd_require(nrow(tasks) == cfg$development_replications &&
    setequal(tasks$replication, seq_len(cfg$development_replications)), "A development case is incomplete.")
  gamma <- mec_primary(context, case$n, case$content)
  paths <- features <- references <- vector("list", nrow(tasks))
  hashes <- receipts <- character(nrow(tasks)); candidate_count <- integer(nrow(tasks))
  for (i in seq_len(nrow(tasks))) {
    task <- tasks[i, , drop = FALSE]; old <- mac_old(task, context)
    mac_dp_check(old$y, old$value$candidates, task$content, cfg)
    paths[[i]] <- mac_path(old$value$candidates, context$laws[[case$response_id]]$p, case$content, gamma)
    features[[i]] <- mac_features(old$y)
    references[[i]] <- mac_references(list(paths[[i]]), gamma)
    hashes[i] <- old$value$sample_sha256; receipts[i] <- old$receipt
    candidate_count[i] <- nrow(old$value$candidates)
  }
  list(ok = TRUE, task = case, tasks = tasks, paths = paths,
    features = as.data.frame(data.table::rbindlist(features)),
    references = as.data.frame(data.table::rbindlist(references)),
    sample_hashes = hashes, receipt_set_sha256 = mwd_hash(receipts),
    candidate_count = sum(candidate_count), prior_uses_analysis_data = FALSE,
    original_assessment_now_used_for_development = TRUE)
}

mac_design_data <- function(design, audit, cases, refined = NULL, full_tasks = NULL) {
  ids <- which(cases$n == design$n & cases$content == design$content)
  pieces <- lapply(ids, function(i) mjs_read(audit, cases, i))
  out <- list(paths = unlist(lapply(pieces, `[[`, "paths"), recursive = FALSE),
    features = as.data.frame(data.table::rbindlist(lapply(pieces, `[[`, "features"))),
    references = as.data.frame(data.table::rbindlist(lapply(pieces, `[[`, "references"))),
    tasks = as.data.frame(data.table::rbindlist(lapply(pieces, `[[`, "tasks"))))
  out$laws <- out$tasks$response_id
  if (!is.null(refined)) {
    indices <- match(out$tasks$job_id, full_tasks$job_id)
    mwd_require(!anyNA(indices), "Refined development samples are incomplete.")
    out$paths <- lapply(indices, function(i) mjs_read(refined, full_tasks, i)$path)
  }
  out
}

mac_development_job <- function(design, audit, cases, cfg, context, refined = NULL, full_tasks = NULL) {
  d <- mac_design_data(design, audit, cases, refined, full_tasks)
  gamma <- mec_primary(context, design$n, design$content)
  result <- mac_develop(d$paths, d$features, d$laws, d$tasks$replication,
    gamma, design$content, design$tolerance_confidence, cfg, d$references)
  fixed <- mac_evaluate(d$paths, d$features, d$laws, mac_constant(gamma), gamma, d$references)
  fixed$fold <- (d$tasks$replication - 1L) %% cfg$folds + 1L
  fixed_scores <- mac_case_scores(fixed)
  gain_folds <- fixed[, .(gain = mean(log(width/reference_width))), by = fold]
  result$fixed_grid <- list(rule = mac_constant(gamma),
    advance = all(fixed_scores$attainment >= design$tolerance_confidence - 1e-12) &&
      min(fixed_scores$mean_width_ratio) <= 1 - cfg$minimum_case_width_gain &&
      sum(gain_folds$gain < -1e-12) >= cfg$minimum_folds_with_gain,
    mean_log_ratio = mean(log(fixed_scores$mean_width_ratio)), scores = fixed_scores)
  list(ok = TRUE, task = design, result = result, datasets = nrow(d$tasks),
    refined_grid = !is.null(refined))
}

mac_refinement_job <- function(task, cfg, context, previous = NULL) {
  if (!is.null(previous)) {
    i <- match(task$job_id, previous$tasks$job_id)
    if (!is.na(i)) {
      value <- mjs_read(previous$queue, previous$tasks, i)
      value$newly_fitted_candidates <- 0L
      value$reused_refinement <- TRUE
      return(value)
    }
  }
  old <- mac_old(task, context)
  anchor <- list(candidates = old$value$candidates)
  inputs <- mac_fitting_inputs(task, context)
  result <- mac_refine(old$y, inputs$task, anchor, cfg, inputs$context)
  gamma <- mec_primary(context, task$n, task$content)
  path <- mac_path(result$candidates, context$laws[[task$response_id]]$p, task$content, gamma)
  original_path <- mac_path(old$value$candidates, context$laws[[task$response_id]]$p, task$content, gamma)
  potential <- 0
  for (i in which(path$attained)) {
    g <- min(gamma, path$posterior_content_probability[i])
    reference <- mdc_select(original_path, g)
    potential <- max(potential, 1 - path$width[i]/reference$width)
  }
  # This oracle-assisted screen measures search headroom, not deployed performance.
  list(ok = TRUE, task = task, candidates = result$candidates, path = path,
    sample_sha256 = old$value$sample_sha256, source_receipt = old$receipt,
    original_candidates_sha256 = mwd_hash(old$value$candidates),
    new_pairs = result$new_pairs, traces = result$traces,
    added_candidates = result$added_candidates, newly_fitted_candidates = result$added_candidates,
    reused_refinement = FALSE, potential_grid_gain = potential,
    primary_gamma_width_gain = 1 - mdc_select(path, gamma)$width/mdc_select(original_path, gamma)$width)
}

mac_refinement_screen <- function(queue, tasks, cfg) {
  rows <- data.table::rbindlist(lapply(seq_len(nrow(tasks)), function(i) {
    value <- mjs_read(queue, tasks, i)
    data.frame(n = tasks$n[i], content = tasks$content[i], response_id = tasks$response_id[i],
      potential_grid_gain = value$potential_grid_gain,
      primary_gamma_width_gain = value$primary_gamma_width_gain,
      added_candidates = value$added_candidates)
  }))
  cells <- rows[, .(samples = .N, mean_potential_grid_gain = mean(potential_grid_gain),
    mean_primary_gamma_width_gain = mean(primary_gamma_width_gain),
    added_candidates = sum(added_candidates)), by = .(n, content, response_id)]
  designs <- cells[, .(refine = max(mean_potential_grid_gain) >= cfg$minimum_case_width_gain,
    maximum_case_mean_potential_gain = max(mean_potential_grid_gain)), by = .(n, content)]
  mwd_write(cells, file.path(cfg$output_root, "refinement_screen/case_summary.csv"))
  mwd_write(designs, file.path(cfg$output_root, "refinement_screen/design_decisions.csv"))
  designs
}

mac_select_policy <- function(designs, base, refined, cfg, context) {
  decisions <- vector("list", nrow(designs)); diagnostics <- vector("list", nrow(designs))
  for (i in seq_len(nrow(designs))) {
    design <- designs[i, , drop = FALSE]
    gamma <- mec_primary(context, design$n, design$content)
    original <- mjs_read(base$queue, base$tasks, match(design$job_id, base$tasks$job_id))$result
    options <- list()
    if (original$advance) options[[length(options) + 1L]] <- list(name = "original_grid_calibrated",
      rule = original$rule, refine = FALSE, complexity = if (original$kind == "constant") 1L else 2L,
      mean_log_ratio = original$criteria[original$criteria$kind == original$kind, ]$mean_log_ratio)
    if (!is.null(refined) && design$job_id %in% refined$tasks$job_id) {
      newer <- mjs_read(refined$queue, refined$tasks, match(design$job_id, refined$tasks$job_id))$result
      if (newer$fixed_grid$advance) options[[length(options) + 1L]] <- list(name = "refined_grid_original_cutoff",
        rule = mac_constant(gamma), refine = TRUE, complexity = 1L,
        mean_log_ratio = newer$fixed_grid$mean_log_ratio)
      if (newer$advance) options[[length(options) + 1L]] <- list(name = "refined_grid_calibrated",
        rule = newer$rule, refine = TRUE, complexity = if (newer$kind == "constant") 2L else 3L,
        mean_log_ratio = newer$criteria[newer$criteria$kind == newer$kind, ]$mean_log_ratio)
    }
    selected <- list(name = "unchanged_reference", rule = mac_constant(gamma), refine = FALSE,
      complexity = 0L, mean_log_ratio = 0)
    if (length(options)) {
      cost <- vapply(options, `[[`, 0, "mean_log_ratio")
      close <- which(exp(cost - min(cost)) <= 1/(1 - cfg$minimum_case_width_gain))
      ranked <- close[order(vapply(options[close], `[[`, 0L, "complexity"), cost[close], close)]
      selected <- options[[ranked[1]]]
    }
    decisions[[i]] <- list(n = design$n, content = design$content, tolerance_confidence = design$tolerance_confidence,
      gamma_reference = gamma, cutoff = selected$rule, refine = selected$refine, choice = selected$name)
    diagnostics[[i]] <- list(design = design, options = options, selection = selected$name)
  }
  list(designs = decisions, diagnostics = diagnostics,
    advancing_designs = sum(vapply(decisions, function(d) d$choice != "unchanged_reference", FALSE)))
}

mac_policy_design <- function(policy, n, content, confidence) {
  keep <- vapply(policy$designs, function(d) d$n == n && d$content == content && d$tolerance_confidence == confidence, FALSE)
  mwd_require(sum(keep) == 1, "A complete frozen design rule is missing or duplicated.")
  policy$designs[[which(keep)]]
}

mac_policy_check <- function(policy) {
  allowed <- c("n", "content", "tolerance_confidence", "gamma_reference", "cutoff", "refine", "choice")
  keys <- character(length(policy$designs))
  for (i in seq_along(policy$designs)) {
    d <- policy$designs[[i]]
    mwd_require(setequal(names(d), allowed) && d$n >= 2 && d$content > 0 && d$content < 1 &&
      d$tolerance_confidence > 0 && d$tolerance_confidence < 1 &&
      is.logical(d$refine) && length(d$refine) == 1 && !is.na(d$refine),
      "A deployment design has invalid fields or contains development information.")
    mac_predict(d$cutoff, data.frame(absolute_bowley = 0, log_tail_weight = 0))
    thresholds <- unlist(d$cutoff[c("gamma_low", "gamma_high", "gamma_invalid")])
    mwd_require(is.finite(d$gamma_reference) && d$gamma_reference > 0 && d$gamma_reference < 1 &&
      all(thresholds <= d$gamma_reference) && d$cutoff$gamma_invalid == d$gamma_reference,
      "A frozen cutoff is more restrictive than the reference or changes the degenerate-feature rule.")
    keys[i] <- paste(d$n, d$content, d$tolerance_confidence)
  }
  mwd_require(length(keys) > 0 && !anyDuplicated(keys), "Frozen designs are empty or duplicated.")
  invisible(TRUE)
}

mac_seed_check <- function(tasks, occupied) {
  seeds <- c(tasks$data_seed, tasks$ecm_seed)
  mwd_require(length(seeds) == 2*nrow(tasks) && all(is.finite(seeds) & seeds > 0 & seeds < .Machine$integer.max) &&
    !anyDuplicated(seeds) && !length(intersect(seeds, occupied)), "Fresh random-seed allocations overlap recorded work.")
  invisible(TRUE)
}

mac_seed_plan <- function(cfg, context) {
  tasks <- mps_design(list(expected_cells = cfg$expected_cells, replications_per_cell = cfg$validation_replications,
    data_seed_base = cfg$data_seed_base, kappa_grid = cfg$prior_sd), context)
  tasks$ecm_seed <- as.integer(cfg$ecm_seed_base + 10*tasks$job_id)
  # Include all recorded studies, not just the immediate predecessor.
  common <- file.path(mgfp_common(), ".codex_work")
  pointers <- list.files(common, pattern = "_latest[.]json$", full.names = TRUE)
  directories <- unique(c(context$source$directory, vapply(pointers, function(p)
    tryCatch(mwd_json(p)$output_root %||% "", error = function(e) ""), "")))
  occupied <- c(context$source_tasks$data_seed, context$source_tasks$ecm_seed)
  files <- character()
  for (directory in directories[nzchar(directories)]) {
    for (stage in c("development", "calibration", "validation", "pilot", "shortlist", "confirmation", "screening", "modes")) {
      path <- file.path(directory, stage, "tasks.csv")
      if (file.exists(path) && normalizePath(directory, mustWork = FALSE) != normalizePath(cfg$output_root, mustWork = FALSE)) {
        old <- data.table::fread(path)
        fields <- grep("(^seed$|_seed$|_base$)", names(old), value = TRUE)
        occupied <- c(occupied, unlist(old[, ..fields], use.names = FALSE)); files <- c(files, path)
      }
    }
  }
  source_jobs <- file.path(mgfp_common(), mwd_json("application/config/mti_gibbs_width_diagnosis_20261113.json")$campaign, "jobs.rds")
  if (file.exists(source_jobs)) {
    jobs <- readRDS(source_jobs); occupied <- c(occupied, jobs$data_seed, jobs$seed)
    files <- c(files, source_jobs)
  }
  mac_seed_check(tasks, occupied)
  list(tasks = tasks, audit = list(tasks_sha256 = mwd_hash(tasks),
    occupied_seed_set_sha256 = mwd_hash(sort(unique(occupied))), no_recorded_overlap = TRUE,
    historical_files = mgfp_hashes(files), intended_use = "final_validation_only"))
}

mac_freeze <- function(policy, tasks, cfg, identity) {
  mac_policy_check(policy)
  directory <- cfg$output_root
  deploy <- list(designs = policy$designs, advancing_designs = policy$advancing_designs)
  path <- file.path(directory, "frozen_rule.rds")
  task_path <- file.path(directory, "validation_tasks.rds")
  for (name in c("rule", "tasks")) {
    file <- if (name == "rule") path else task_path
    value <- if (name == "rule") deploy else tasks
    if (file.exists(file)) mwd_require(identical(readRDS(file), value), "Frozen rule or validation tasks changed.") else {
      mwd_require(!length(Sys.glob(file.path(directory, "validation/fits/*.rds"))),
        "Cannot freeze a procedure after final validation has started.")
      temporary <- tempfile(".ecm-frozen-", tmpdir = directory); saveRDS(value, temporary)
      mwd_require(file.rename(temporary, file), "Cannot preserve frozen assessment inputs.")
    }
  }
  record <- list(rule_sha256 = mwd_file_hash(path), tasks_sha256 = mwd_file_hash(task_path),
    study_identity = mwd_hash(identity), source_commit = identity$source_commit,
    development_only_selection = TRUE, frozen_before_fresh_data = TRUE,
    automatic_article_replacement = FALSE, fresh_selection_assessment = FALSE)
  receipt <- file.path(directory, "frozen_rule.json")
  if (file.exists(receipt)) mwd_require(identical(mwd_json(receipt), record), "Frozen assessment receipt changed.") else
    mgfp_json_write(record, receipt)
  deploy
}

mac_interval <- function(candidates, gamma, method) {
  z <- mdc_select(candidates, gamma); returned <- nrow(z) == 1L
  data.frame(method = method, gamma = gamma, returned = returned,
    lower = if (returned) z$lower else NA_real_, upper = if (returned) z$upper else NA_real_,
    width = if (returned) z$width else NA_real_, q = if (returned) z$target_content else NA_real_,
    delta = if (returned) z$mean_tilt else NA_real_,
    candidate_index = if (returned) z$candidate_index else NA_integer_,
    dp_probability = if (returned) z$posterior_content_probability else NA_real_,
    numerically_accurate = if (returned) z$numerically_accurate else TRUE,
    failure_reason = if (returned) "" else "no_eligible_candidate", prior_sd = 2, radius_multiplier = 1)
}

mac_validation_job <- function(task, policy, cfg, context) {
  set.seed(task$data_seed); y <- context$laws[[task$response_id]]$r(task$n)
  design <- mac_policy_design(policy, task$n, task$content, task$tolerance_confidence)
  inputs <- mac_fitting_inputs(task, context)
  anchor <- mec_fit(y, inputs$task, cfg, inputs$context)
  candidates <- anchor$candidates; traces <- list(); new_pairs <- data.frame()
  if (design$refine && isTRUE(anchor$returned)) {
    new <- mac_refine(y, inputs$task, anchor, cfg, inputs$context)
    candidates <- new$candidates; traces <- new$traces; new_pairs <- new$new_pairs
  }
  gamma <- mac_predict(design$cutoff, mac_features(y))
  rows <- data.table::rbindlist(list(mac_interval(anchor$candidates, design$gamma_reference, "ecm_primary"),
    mac_interval(candidates, gamma, "ecm_proposed")))
  if (rows$returned[1]) mwd_require(rows$returned[2] && rows$width[2] <=
    rows$width[1] + 64*.Machine$double.eps*max(1, rows$width[1]), "The proposal violates paired width nonincrease.")
  for (i in which(rows$returned)) mwd_require(mfp_accuracy(y, c(rows$lower[i], rows$upper[i]),
    rows$q[i], rows$delta[i], cfg$prior_sd^2, context$prior$numerical)$pass,
    "A selected interval failed final coordinate accuracy.")
  comparison <- mfp_comparators(y, task$content, task$tolerance_confidence)
  mwd_require(all(comparison$returned & is.finite(comparison$width) & comparison$width > 0),
    "A comparator failed in a declared feasible case.")
  comparison$numerically_accurate <- TRUE
  rows <- data.table::rbindlist(list(rows, comparison), fill = TRUE)
  rows[, population_content := context$laws[[task$response_id]]$p(upper) - context$laws[[task$response_id]]$p(lower)]
  rows[, attained := returned & population_content >= task$content]
  for (name in c("job_id", "response_id", "n", "content", "tolerance_confidence", "replication", "data_seed"))
    data.table::set(rows, j = name, value = task[[name]])
  list(ok = TRUE, task = task, rows = rows, sample_sha256 = mwd_hash(y), prior = anchor$prior,
    unresolved_candidates = anchor$unresolved_candidates, original_candidates = anchor$candidates,
    candidates = candidates, new_pairs = new_pairs, new_traces = traces,
    baseline_selected_ecm_trace = anchor$selected_ecm_trace,
    frozen_design = design, rule_sha256 = mwd_hash(design))
}

mac_summary <- function(rows, cfg) {
  d <- data.table::as.data.table(rows)
  result <- mec_summary(d, cfg)
  content <- d[, .(width_samples = sum(returned),
    mean_population_content = if (any(returned)) mean(population_content[returned]) else NA_real_,
    content_q025 = if (any(returned)) unname(quantile(population_content[returned], .025)) else NA_real_,
    content_median = if (any(returned)) median(population_content[returned]) else NA_real_,
    content_q975 = if (any(returned)) unname(quantile(population_content[returned], .975)) else NA_real_),
    by = .(n, content, tolerance_confidence, response_id, method)]
  merge(result, content, by = c("n", "content", "tolerance_confidence", "response_id", "method"), sort = TRUE)
}

mac_read_table <- function(path, prototype) {
  types <- vapply(prototype, typeof, "")
  mwd_require(all(types %in% c("double", "integer", "logical", "character")), "Unsupported assessment column type.")
  types[types == "double"] <- "numeric"
  # Explicit classes preserve empty success reasons and all-missing numerical columns.
  data.table::fread(path, colClasses = split(names(prototype), types), na.strings = "NA")
}

mac_pairs <- function(rows) {
  d <- data.table::as.data.table(rows)
  paired <- mec_pairs(d)
  reference <- d[method == "ecm_primary", .(job_id, reference_width = width)]
  differences <- merge(d[method != "ecm_primary"], reference, by = "job_id")
  quantiles <- differences[returned & is.finite(reference_width), .(
    width_change_q025 = unname(quantile(width - reference_width, .025)),
    width_change_q975 = unname(quantile(width - reference_width, .975)),
    narrower = sum(width < reference_width - 1e-10*pmax(1, reference_width)),
    same_width = sum(abs(width - reference_width) <= 1e-10*pmax(1, reference_width))),
    by = .(n, content, response_id, method)]
  merge(paired, quantiles, by = c("n", "content", "response_id", "method"), all.x = TRUE)
}

mac_report <- function(rows, cfg, stage = "validation") {
  directory <- file.path(cfg$output_root, stage)
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  d <- data.table::as.data.table(rows); summary <- mac_summary(d, cfg)
  paired <- mac_pairs(d)
  mec_write_intervals(d, file.path(directory, "interval_results.csv"))
  mec_write_intervals(summary, file.path(directory, "comparison_summary.csv"))
  mec_write_intervals(paired, file.path(directory, "paired_comparison.csv"))
  overview <- summary[, .(cases = .N, datasets = sum(samples), returned = sum(returned),
    minimum_attainment = min(attainment), maximum_attainment = max(attainment),
    pointwise_underattainment_cases = sum(upper < tolerance_confidence),
    simultaneous_underattainment_cases = sum(simultaneous_upper < tolerance_confidence)), by = method]
  mwd_write(overview, file.path(directory, "method_overview.csv"))
  grDevices::pdf(file.path(directory, "ecm_calibration_diagnostics.pdf"), width = 13, height = 10, useDingbats = FALSE)
  on.exit(grDevices::dev.off())
  methods <- c("ecm_primary", "ecm_proposed", "young_mathew", "wilks")
  labels <- c("Current ECM", "Proposed ECM", "Young-Mathew", "Wilks")
  colors <- c("#0072B2", "#009E73", "#D55E00", "black")
  designs <- unique(summary[, .(n, content)])
  for (i in seq_len(nrow(designs))) for (kind in c("width", "attainment")) {
    laws <- unique(summary$response_id)
    for (group in split(laws, ceiling(seq_along(laws)/4))) {
      par(mfrow = c(2, 2), mar = c(5, 8, 3, 2), oma = c(2, 0, 3, 0), mgp = c(3, .8, 0), las = 1)
      for (law in group) {
        z <- summary[n == designs$n[i] & content == designs$content[i] & response_id == law]
        z <- z[match(methods, z$method)]; y <- rev(seq_len(nrow(z)))
        estimate <- if (kind == "width") z$mean_width else z$attainment
        lo <- if (kind == "width") z$width_q025 else z$lower
        hi <- if (kind == "width") z$width_q975 else z$upper
        xlim <- if (kind == "width") range(c(lo, hi), finite = TRUE) else c(min(.85, lo, na.rm = TRUE), 1.005)
        plot(estimate, y, type = "n", xlim = xlim, ylim = c(.5, 4.5), yaxt = "n", ylab = "",
          xlab = if (kind == "width") "Interval width" else "Probability of attaining required content",
          main = gsub("_", " ", law), cex.lab = 1, cex.main = 1)
        abline(h = y, col = "grey92")
        if (kind == "attainment") abline(v = z$tolerance_confidence[1], lty = 2, col = "grey40")
        segments(lo, y, hi, y, col = colors, lwd = 2)
        points(estimate, y, col = colors, pch = if (kind == "width") 4 else 16, cex = 1.1)
        axis(2, y, labels = labels, tick = FALSE, cex.axis = 1)
      }
      mtext(sprintf("MTI-ECM: n = %d, c = %.2f, p = %.2f", designs$n[i], designs$content[i], z$tolerance_confidence[1]),
        outer = TRUE, side = 3, line = 1, font = 2)
      mtext(if (kind == "width") "Bars: empirical 2.5th-97.5th width percentiles; crosses: means." else
        "Bars: pointwise exact 95% Monte Carlo confidence intervals.", outer = TRUE, side = 1, line = .5)
    }
  }
  invisible(list(summary = summary, paired = paired, overview = overview))
}

mac_workers <- function(cfg) {
  available <- parallelly::availableCores()
  load <- scan("/proc/loadavg", what = "", quiet = TRUE)[1]
  cpu <- max(1, floor(available - as.numeric(load)))
  memory <- readLines("/proc/meminfo")
  gib <- as.numeric(strsplit(trimws(sub("MemAvailable:", "", memory[grepl("^MemAvailable:", memory)])), "[[:space:]]+")[[1]][1])/1024^2
  room <- max(1, floor((gib - cfg$memory_reserve_gib)/cfg$memory_per_worker_gib))
  request <- suppressWarnings(as.integer(Sys.getenv("MTI_ECM_CALIBRATION_WORKERS", "")))
  if (!is.finite(request)) request <- cfg$maximum_workers
  as.integer(max(1, min(request, cfg$maximum_workers, cpu, room)))
}

mac_check <- function(cfg, context) {
  cases <- mac_cases(context)
  wanted <- (cases$n == 50 & cases$content == .9 & cases$response_id == "exponential") |
    (cases$n == 100 & cases$content == .95 & cases$response_id == "normal") |
    (cases$n == 500 & cases$content == .99 & cases$response_id == "lognormal_hard") |
    (cases$n == 1000 & cases$content == .99 & cases$response_id == "student_t3")
  checks <- lapply(which(wanted), function(i) {
    case <- cases[i, , drop = FALSE]
    j <- which(context$source_tasks$n == case$n & context$source_tasks$content == case$content &
      context$source_tasks$response_id == case$response_id & context$source_tasks$replication == 1)
    mwd_require(length(j) == 1, "An integrated-check sample is missing.")
    task <- context$source_tasks[j, , drop = FALSE]; old <- mac_old(task, context)
    tic <- proc.time()[3]
    inputs <- mac_fitting_inputs(task, context)
    anchor <- mec_fit(old$y, inputs$task, cfg, inputs$context)
    keys <- c("candidate_index", "target_content", "mean_tilt", "central_tilt", "lower", "upper", "width",
      "posterior_content_probability", "observed_count")
    mwd_require(nrow(anchor$candidates) == nrow(old$value$candidates) &&
      isTRUE(all.equal(anchor$candidates[, keys], old$value$candidates[, keys], tolerance = 1e-10, check.attributes = FALSE)),
      "The current fixed-scale numerical computation does not reproduce its historical reference.")
    refined <- mac_refine(old$y, inputs$task, anchor, cfg, inputs$context)
    mac_dp_check(old$y, refined$candidates, task$content, cfg)
    mwd_require(identical(refined$candidates[seq_len(nrow(anchor$candidates)), names(anchor$candidates)], anchor$candidates),
      "An integrated refinement changed reference candidates.")
    target <- mdc_select(refined$candidates, mec_primary(context, task$n, task$content))
    roots <- c(target$lower, target$upper)
    starts <- list(c(min(roots[1], -2*cfg$prior_sd), roots[2]),
      c(roots[1], max(roots[2], 2*cfg$prior_sd)))
    objective <- mgp_objective(old$y, roots[1], roots[2], target$target_content, target$mean_tilt, precision = 1/4)
    checks <- lapply(starts, function(start) {
      control <- context$profile$ecm_control; control$max_iter <- context$prior$numerical$initial_iterations
      control$ecm_backend <- "cpp"; control$multistart <- FALSE
      fit <- rqrgibbs::rqr_ecm_fit(old$y, matrix(1, task$n, 1), target$target_content,
        mean_tilt = target$mean_tilt, learning_rate = 1,
        beta_prior_obj = rqrgibbs::beta_prior("ridge", ridge = list(tau2 = 4)),
        ecm_control = control, init = list(beta1 = start[1], beta2 = start[2]))
      polished <- mfp_polish(old$y, sort(c(fit$beta_root1, fit$beta_root2)),
        target$target_content, target$mean_tilt, 4, context$prior$numerical)
      mwd_require(polished$accuracy$pass, "An integrated additional-start check is numerically unresolved.")
      alternate <- mgp_objective(old$y, polished$roots[1], polished$roots[2],
        target$target_content, target$mean_tilt, precision = 1/4)
      mwd_require(alternate >= objective - context$prior$numerical$objective_tolerance,
        "An additional start found a better selected-target mode; review fitting before calibration.")
      list(start = start, final_roots = polished$roots, objective_change = alternate - objective)
    })
    list(n = task$n, content = task$content, response_id = task$response_id,
      reused_sample_hash = mwd_hash(old$y), original_candidates = nrow(anchor$candidates),
      added_candidates = refined$added_candidates, additional_start_checks = checks,
      elapsed_seconds = unname(proc.time()[3] - tic))
  })
  mwd_require(length(checks) == 4, "The integrated-check cases changed.")
  record <- list(passed = TRUE, source_commit = mgfp_git("rev-parse", "HEAD"),
    uncommitted_changes = nzchar(mgfp_git("status", "--porcelain")),
    implementation_files = mgfp_hashes(c("application/scripts/lib/mti_ecm_adaptive_calibration.R",
      "application/scripts/lib/mti_ecm_adaptive_study.R", "application/config/mti_ecm_adaptive_calibration_20261120.json")),
    development_observations_only = TRUE, numerical_reference_reproduced = TRUE,
    prior_uses_analysis_data = FALSE, checks = checks, time = mgfp_time())
  mgfp_json_write(record, file.path(cfg$output_root, "rehearsal.json"))
  record
}

mac_verify <- function(cfg, context, workers) {
  directory <- cfg$output_root
  manifest <- mwd_json(file.path(directory, "manifest.json"))
  identity <- manifest$identity
  mwd_require(identical(manifest$identity_hash, mwd_hash(identity)), "Study identity checksum changed.")
  mgfp_check_hashes(identity$programs); mgfp_check_hashes(identity$article_files)
  mgfp_check_hashes(identity$package$installed_files)
  summary <- mwd_json(file.path(directory, "study_summary.json"))
  mwd_require(summary$status %in% c("complete_no_development_gain", "complete_requires_scientific_review"),
    "The study has not completed its declared decision path.")
  audit_tasks <- readRDS(file.path(directory, "audit_tasks.rds"))
  audit_id <- mwd_json(file.path(directory, "audit/manifest.json"))$analysis_id
  mwd_require(audit_id == mwd_hash(list(identity, "audit", audit_tasks)), "The development audit identity differs.")
  # Every historical fit is rechecked, including its sample identity and raw checksum.
  old_checks <- parallel::mclapply(seq_len(nrow(audit_tasks)), function(i) {
    case <- audit_tasks[i, , drop = FALSE]
    saved <- mbc_read_fit(file.path(directory, "audit/fits", sprintf("fit_%06d.rds", case$job_id)), audit_id, case)
    fresh <- mac_audit_case(case, cfg, context)
    mwd_require(saved$receipt_set_sha256 == fresh$receipt_set_sha256 &&
      identical(saved$sample_hashes, fresh$sample_hashes) && identical(saved$paths, fresh$paths) &&
      identical(saved$features, fresh$features), "Development observations or selection paths changed.")
    TRUE
  }, mc.cores = min(workers, nrow(audit_tasks)), mc.set.seed = FALSE)
  mwd_require(all(vapply(old_checks, isTRUE, FALSE)), "Historical evidence verification failed.")
  policy <- readRDS(file.path(directory, "selected_development_rule.rds"))
  mwd_require(policy$advancing_designs == summary$advancing_designs,
    "The recorded development decision differs.")
  count <- 0L
  if (summary$status == "complete_requires_scientific_review") {
    freeze <- mwd_json(file.path(directory, "frozen_rule.json"))
    mwd_require(freeze$rule_sha256 == mwd_file_hash(file.path(directory, "frozen_rule.rds")) &&
      freeze$tasks_sha256 == mwd_file_hash(file.path(directory, "validation_tasks.rds")) &&
      freeze$study_identity == mwd_hash(identity) && freeze$frozen_before_fresh_data &&
      freeze$development_only_selection && !freeze$automatic_article_replacement,
      "The frozen final-validation inputs changed.")
    tasks <- readRDS(file.path(directory, "validation_tasks.rds"))
    stage_id <- mwd_json(file.path(directory, "validation/manifest.json"))$analysis_id
    frozen <- readRDS(file.path(directory, "frozen_rule.rds"))
    mac_policy_check(frozen)
    mwd_require(stage_id == mwd_hash(list(c(identity, list(frozen_rule = mwd_hash(frozen))), "validation", tasks)),
      "The final-validation stage identity differs.")
    groups <- split(seq_len(nrow(tasks)), tasks$cell_id)
    chunks <- parallel::mclapply(groups, function(ids) {
      values <- lapply(seq_along(ids), function(j) {
        i <- ids[j]; task <- tasks[i, , drop = FALSE]
        value <- mbc_read_fit(file.path(directory, "validation/fits", sprintf("fit_%06d.rds", task$job_id)), stage_id, task)
        set.seed(task$data_seed); y <- context$laws[[task$response_id]]$r(task$n)
        design <- mac_policy_design(frozen, task$n, task$content, task$tolerance_confidence)
        mwd_require(value$ok && value$unresolved_candidates == 0 &&
          value$prior$prior_sd == cfg$prior_sd && !value$prior$data_adaptive_prior &&
          value$sample_sha256 == mwd_hash(y) && value$rule_sha256 == mwd_hash(design),
          "A final-validation sample, prior or rule differs.")
        mac_dp_check(y, value$candidates, task$content, cfg)
        expected <- data.table::rbindlist(list(mac_interval(value$original_candidates, design$gamma_reference, "ecm_primary"),
          mac_interval(value$candidates, mac_predict(design$cutoff, mac_features(y)), "ecm_proposed")))
        actual <- as.data.frame(value$rows[value$rows$method %in% expected$method, names(expected), with = FALSE])
        mwd_require(isTRUE(all.equal(as.data.frame(expected), actual, check.attributes = FALSE, tolerance = 0)),
          "Final selection does not reproduce from the frozen candidate set.")
        for (k in which(expected$returned)) mwd_require(mfp_accuracy(y, c(expected$lower[k], expected$upper[k]),
          expected$q[k], expected$delta[k], cfg$prior_sd^2, context$prior$numerical)$pass,
          "A final selected target no longer passes coordinate accuracy.")
        benchmarks <- mfp_comparators(y, task$content, task$tolerance_confidence)
        classical <- as.data.frame(value$rows[match(benchmarks$method, value$rows$method), names(benchmarks), with = FALSE])
        mwd_require(isTRUE(all.equal(benchmarks, classical, check.attributes = FALSE, tolerance = 0)),
          "A final classical comparator does not reproduce.")
        rows <- data.table::as.data.table(value$rows)
        mass <- context$laws[[task$response_id]]$p(rows$upper) - context$laws[[task$response_id]]$p(rows$lower)
        mwd_require(nrow(rows) == 4 && !anyDuplicated(rows$method) && all(rows$numerically_accurate) &&
          max(abs(mass - rows$population_content), na.rm = TRUE) < 1e-12 &&
          all(rows$attained == (rows$returned & mass >= task$content)),
          "A final interval content, return, or method count differs.")
        list(rows = rows, sample_sha256 = value$sample_sha256)
      })
      list(rows = data.table::rbindlist(lapply(values, `[[`, "rows")),
        sample_hashes = vapply(values, `[[`, "", "sample_sha256"))
    }, mc.cores = min(workers, length(groups)), mc.set.seed = FALSE)
    mwd_require(all(vapply(chunks, function(x) is.list(x) && is.data.frame(x$rows), FALSE)),
      "A final-validation verification job failed.")
    rows <- data.table::rbindlist(lapply(chunks, `[[`, "rows"))
    hashes <- unlist(lapply(chunks, `[[`, "sample_hashes"), use.names = FALSE)
    development_hashes <- unlist(lapply(seq_len(nrow(audit_tasks)), function(i) {
      case <- audit_tasks[i, , drop = FALSE]
      mbc_read_fit(file.path(directory, "audit/fits", sprintf("fit_%06d.rds", case$job_id)), audit_id, case)$sample_hashes
    }), use.names = FALSE)
    mwd_require(!anyDuplicated(hashes) && !length(intersect(hashes, development_hashes)),
      "Final assessment reuses observations from another dataset or development.")
    observed <- mac_read_table(file.path(directory, "validation/interval_results.csv"), rows)
    keys <- c("job_id", "method")
    data.table::setorderv(rows, keys); data.table::setorderv(observed, keys)
    agreement <- all.equal(as.data.frame(rows), as.data.frame(observed), check.attributes = FALSE, tolerance = 0)
    mwd_require(isTRUE(agreement), paste("Full-precision interval exports differ from saved fits:", paste(agreement, collapse = "; ")))
    reproduced <- mac_summary(rows, cfg)
    reported <- mac_read_table(file.path(directory, "validation/comparison_summary.csv"), reproduced)
    keys <- c("n", "content", "response_id", "method")
    data.table::setorderv(reproduced, keys); data.table::setorderv(reported, keys)
    mwd_require(nrow(tasks) == cfg$expected_cells*cfg$validation_replications &&
      isTRUE(all.equal(as.data.frame(reproduced), as.data.frame(reported), check.attributes = FALSE, tolerance = 1e-12)),
      "Final case counts or reported summaries differ.")
    pairs <- mac_pairs(rows)
    reported_pairs <- mac_read_table(file.path(directory, "validation/paired_comparison.csv"), pairs)
    data.table::setorderv(pairs, keys); data.table::setorderv(reported_pairs, keys)
    mwd_require(isTRUE(all.equal(as.data.frame(pairs), as.data.frame(reported_pairs),
      check.attributes = FALSE, tolerance = 1e-12)), "The paired comparison summary does not reproduce.")
    count <- nrow(tasks)
  } else mwd_require(!length(Sys.glob(file.path(directory, "validation/fits/*.rds"))),
    "Fresh validation was fitted despite a no-gain stopping decision.")
  mgfp_check_hashes(identity$article_files)
  result <- list(passed = TRUE, development_datasets = cfg$expected_cells*cfg$development_replications,
    independent_validation_datasets = count,
    advancing_designs = policy$advancing_designs, article_unchanged = TRUE,
    no_gibbs_sampling = TRUE, prior_uses_analysis_data = FALSE, time = mgfp_time())
  mgfp_json_write(result, file.path(directory, "verification.json"))
  result
}

mac_pipeline <- function(cfg, context, identity, workers) {
  directory <- cfg$output_root
  stage <- "audit"
  tryCatch({
    cases <- mac_cases(context); designs <- mac_designs(context)
    saveRDS(cases, file.path(directory, "audit_tasks.rds"))
    audit <- mjs_queue(cases, stage, cfg, identity, workers, function(task) mac_audit_case(task, cfg, context))
    audited <- lapply(seq_len(nrow(cases)), function(i) mjs_read(audit, cases, i))
    hashes <- unlist(lapply(audited, `[[`, "sample_hashes"), use.names = FALSE)
    mwd_require(!anyDuplicated(hashes), "Development datasets overlap across cases.")
    mgfp_json_write(list(passed = TRUE, datasets = length(hashes),
      candidates = sum(vapply(audited, `[[`, 0L, "candidate_count")),
      used_as_development_only = TRUE, raw_fits_refitted = 0, time = mgfp_time()),
      file.path(directory, "audit/summary.json"))
    rm(audited); gc(FALSE)
    stage <- "original_development"
    base <- mjs_queue(designs, stage, cfg, identity, min(workers, nrow(designs)),
      function(task) mac_development_job(task, audit, cases, cfg, context))
    base <- list(queue = base, tasks = designs)
    refined_development <- NULL
    if (cfg$refinement$enabled) {
      stage <- "refinement_screen"
      screen_tasks <- context$source_tasks[context$source_tasks$replication <= cfg$refinement$screen_replications, , drop = FALSE]
      screen <- mjs_queue(screen_tasks, stage, cfg, identity, workers,
        function(task) mac_refinement_job(task, cfg, context))
      decisions <- mac_refinement_screen(screen, screen_tasks, cfg)
      advancing <- decisions[refine == TRUE]
      if (nrow(advancing)) {
        design_key <- function(x) paste(x$n, x$content)
        stage <- "refined_development_fits"
        full_tasks <- context$source_tasks[design_key(context$source_tasks) %in% design_key(advancing), , drop = FALSE]
        full <- mjs_queue(full_tasks, stage, cfg, identity, workers,
          function(task) mac_refinement_job(task, cfg, context, list(queue = screen, tasks = screen_tasks)))
        stage <- "refined_development"
        full_designs <- designs[design_key(designs) %in% design_key(advancing), , drop = FALSE]
        refined <- mjs_queue(full_designs, stage, cfg, identity, min(workers, nrow(full_designs)),
          function(task) mac_development_job(task, audit, cases, cfg, context, full, full_tasks))
        refined_development <- list(queue = refined, tasks = full_designs)
      }
    }
    stage <- "freeze"
    policy <- mac_select_policy(designs, base, refined_development, cfg, context)
    rule_file <- file.path(directory, "selected_development_rule.rds")
    if (file.exists(rule_file)) mwd_require(identical(readRDS(rule_file), policy), "The saved development selection changed.") else
      saveRDS(policy, rule_file)
    mgfp_json_write(policy, file.path(directory, "selected_development_rule.json"))
    diagnostics <- data.table::rbindlist(lapply(seq_len(nrow(designs)), function(i) {
      result <- mjs_read(base$queue, base$tasks, i)$result
      d <- data.table::copy(result$scores); d$n <- designs$n[i]; d$content <- designs$content[i]
      d$grid <- "original"; d
    }))
    if (!is.null(refined_development)) for (i in seq_len(nrow(refined_development$tasks))) {
      design <- refined_development$tasks[i, , drop = FALSE]
      result <- mjs_read(refined_development$queue, refined_development$tasks, i)$result
      d <- data.table::copy(result$scores); d$n <- design$n; d$content <- design$content; d$grid <- "refined"
      diagnostics <- data.table::rbindlist(list(diagnostics, d), fill = TRUE)
    }
    mec_write_intervals(diagnostics, file.path(directory, "development_case_scores.csv"))
    if (policy$advancing_designs == 0) {
      mgfp_json_write(list(status = "complete_no_development_gain", advancing_designs = 0L,
        development_datasets = 72000L, independent_validation_datasets = 0L,
        remaining_dataset_runs = 0L, reason = "No rule met the frozen casewise cross-validation and width-gain criteria.",
        article_replaced = FALSE, time = mgfp_time()), file.path(directory, "study_summary.json"))
      return(invisible(policy))
    }
    plan <- mac_seed_plan(cfg, context)
    mgfp_json_write(plan$audit, file.path(directory, "seed_audit.json"))
    policy <- mac_freeze(policy, plan$tasks, cfg, identity)
    stage <- "validation"
    queue <- mjs_queue(plan$tasks, stage, cfg, c(identity, list(frozen_rule = mwd_hash(policy))), workers,
      function(task) mac_validation_job(task, policy, cfg, context))
    rows <- data.table::rbindlist(lapply(seq_len(nrow(plan$tasks)), function(i) mjs_read(queue, plan$tasks, i)$rows))
    mac_report(rows, cfg)
    mgfp_json_write(list(status = "complete_requires_scientific_review", advancing_designs = policy$advancing_designs,
      development_datasets = 72000L, independent_validation_datasets = nrow(plan$tasks),
      remaining_dataset_runs = 0L, numerical_pass = TRUE, article_replaced = FALSE,
      no_rule_reselection_on_validation = TRUE, time = mgfp_time()), file.path(directory, "study_summary.json"))
    invisible(policy)
  }, error = function(e) {
    mgfp_json_write(list(status = "stopped_requires_review", stage = stage, reason = conditionMessage(e),
      article_replaced = FALSE, time = mgfp_time()), file.path(directory, "study_summary.json"))
    stop(e)
  })
}
