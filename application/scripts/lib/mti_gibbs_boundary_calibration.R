mbc_project_tilt <- function(y, q, delta, variant) {
  limits <- mwd_limits(y, q); span <- diff(limits); n <- length(y)
  mwd_require(n >= 2L && q > 0 && q < 1 && is.finite(delta) && span > 0,
    "A finite, nonconstant sample and interior fitted content are required.")
  fraction <- switch(variant, original = 0, log_rank = log(n) / n,
    sqrt_rank = 1 / sqrt(n), stop("Unknown interior-tilt rule.", call. = FALSE))
  mwd_require(fraction < .5, "Interior-tilt margins overlap at this sample size.")
  proposed <- if (variant == "original") delta else
    max(limits[1] + fraction * span, min(delta, limits[2] - fraction * span))
  margin <- min(delta - limits[1], limits[2] - delta) / span
  scale <- 64 * .Machine$double.eps * max(1, abs(limits), abs(delta)) / span
  list(delta = unname(proposed), lower_limit = unname(limits[1]), upper_limit = unname(limits[2]),
    fraction = unname(fraction), original_margin = unname(margin),
    target_class = if (margin < -scale) "outside" else if (margin <= scale) "boundary" else "interior",
    outward_slope = unname(n * q * min(proposed - limits[1], limits[2] - proposed)))
}

mbc_rank <- function(samples, confidence, error, comparisons) {
  mwd_require(length(samples) == 1L && samples >= 1 && samples == as.integer(samples) &&
    confidence > 0 && confidence < 1 && error > 0 && error < 1 && comparisons >= 1,
    "Invalid calibration sample size, confidence, or error allocation.")
  bounds <- stats::pbinom(seq_len(samples) - 1L, samples, confidence)
  index <- which(bounds >= 1 - error / comparisons)[1L]
  mwd_require(!is.na(index), "Calibration sample size cannot support the allocated confidence.")
  list(rank = index, assurance = bounds[index], samples = samples,
    confidence = confidence, per_case_error = error / comparisons)
}

mbc_interval <- function(center, halfwidth, multiplier = 1) {
  mwd_require(all(is.finite(c(center, halfwidth, multiplier))) && halfwidth > 0 && multiplier > 0,
    "Reported intervals require finite centers and positive radii and multipliers.")
  c(lower = center - halfwidth * multiplier, upper = center + halfwidth * multiplier)
}

# The upper bracket always meets content; no below-target root approximation is used.
mbc_required_scale <- function(cdf, center, halfwidth, content) {
  mbc_interval(center, halfwidth)
  mwd_require(content > 0 && content < 1, "Requested content must be between zero and one.")
  mass <- function(s) {
    ends <- if (s == 0) c(center, center) else mbc_interval(center, halfwidth, s)
    value <- cdf(ends[2]) - cdf(ends[1])
    mwd_require(length(value) == 1L && is.finite(value) && value >= 0 && value <= 1,
      "The calibration CDF returned invalid content.")
    value
  }
  lo <- 0; hi <- 1
  while (mass(hi) < content) {
    hi <- 2 * hi
    mwd_require(is.finite(hi) && hi < 1e100, "The content radius could not be bracketed.")
  }
  while (hi - lo > sqrt(.Machine$double.eps) * max(1, hi)) {
    middle <- (lo + hi) / 2
    if (mass(middle) >= content) hi <- middle else lo <- middle
  }
  hi
}

mbc_rows <- function(task, variant, geometry, fits, parameters, anchor, y, cdf) {
  means <- setNames(parameters$mean, parameters$variable)
  center <- unname(means["midpoint"]); halfwidth <- unname(means["width"] / 2)
  ends <- mbc_interval(center, halfwidth)
  adequate <- all(parameters$preferred_pass & parameters$mean_precision)
  data.frame(job_id = task$job_id, response_id = task$response_id, n = task$n, content = task$content,
    tolerance_confidence = task$tolerance_confidence, replication = task$replication,
    data_seed = task$data_seed, variant = variant, q = anchor$q,
    original_delta = anchor$delta_reference, delta = geometry$delta,
    lower_limit = geometry$lower_limit, upper_limit = geometry$upper_limit,
    original_margin = geometry$original_margin, target_class = geometry$target_class,
    margin_fraction = geometry$fraction, outward_slope = geometry$outward_slope,
    center = center, halfwidth = halfwidth, lower = ends[1], upper = ends[2],
    width = diff(ends), ecm_lower = anchor$selected$lower, ecm_upper = anchor$selected$upper,
    required_scale = mbc_required_scale(cdf, center, halfwidth, task$content),
    population_content = unname(cdf(ends[2]) - cdf(ends[1])),
    retained_per_chain = nrow(fits[[1]]$draws), adequate = adequate,
    max_rhat = max(parameters$rhat), min_bulk_ess = min(parameters$ess_bulk),
    min_tail_ess = min(parameters$ess_tail), sample_sha256 = mwd_hash(y),
    lower_below_sample_fraction = mean(unlist(lapply(fits, function(f) f$draws[, 1] < min(y)))))
}

mbc_sample_target <- function(y, task, anchor, variant, cfg, policy, seed) {
  geometry <- mbc_project_tilt(y, anchor$q, anchor$delta_reference, variant)
  ec <- c(anchor$selected$lower, anchor$selected$upper)
  initial <- mwd_initial(y, ec[1], ec[2], 4, 0)
  settings <- c(cfg$sampling, list(learning_rate = 1, prior_variance = 4,
    initial_midpoint_offsets_sd = initial$offsets, initial_halfwidth_multipliers = initial$radii))
  history <- list()
  for (keep in unique(pmin(settings$maximum_retained, settings$n_mcmc * 2^(0:3)))) {
    fits <- lapply(seq_len(settings$chains), function(chain) mgp_slice_fit(y,
      data.frame(chain = chain, seed = as.integer(seed + chain), q = anchor$q, delta = geometry$delta,
        ecm_lower = ec[1], ecm_upper = ec[2]), settings, n_keep = keep))
    summary_task <- data.frame(case_id = paste0("boundary_", task$job_id), dataset_id = task$job_id,
      source = "simulation", response_id = "not_supplied_to_sampler", n = length(y),
      content = task$content, q = anchor$q, delta = geometry$delta, ecm_lower = ec[1], ecm_upper = ec[2])
    parameters <- mgp_summaries(fits, summary_task, policy)
    adequate <- all(parameters$preferred_pass & parameters$mean_precision)
    history[[length(history) + 1L]] <- data.frame(retained = keep, adequate = adequate)
    if (adequate) break
  }
  # Retain recoverable diagnostics without duplicating the full loss trajectory.
  fits <- lapply(fits, function(f) f[setdiff(names(f), "target_loss_trace")])
  list(geometry = geometry, fits = fits, parameters = parameters,
    history = do.call(rbind, history), adequate = adequate)
}

mbc_development_choice <- function(rows, variants) {
  d <- data.table::copy(data.table::as.data.table(rows))
  mwd_require(all(d$adequate) && all(is.finite(d$required_scale) & d$required_scale > 0),
    "Development contains unresolved posterior calculations.")
  cells <- d[, .(scale = unname(quantile(required_scale, tolerance_confidence[1], type = 1)),
    base_width = mean(width), ecm_width = mean(ecm_upper - ecm_lower)),
    by = .(n, content, tolerance_confidence, response_id, variant)]
  design_scales <- cells[, .(scale = max(scale)), by = .(n, content, tolerance_confidence, variant)]
  cells <- merge(cells[, !"scale"], design_scales, by = c("n", "content", "tolerance_confidence", "variant"))
  cells[, log_width_ratio := log(scale * base_width / ecm_width)]
  scores <- cells[, .(worst_log_ratio = max(log_width_ratio), mean_log_ratio = mean(log_width_ratio)),
    by = .(n, content, tolerance_confidence, variant)]
  scores[, variant_order := match(variant, variants)]
  data.table::setorderv(scores, c("n", "content", "worst_log_ratio", "mean_log_ratio", "variant_order"))
  policy <- scores[, .SD[1], by = .(n, content, tolerance_confidence)]
  list(policy = policy, scores = scores, descriptive_cells = cells)
}

mbc_calibrate <- function(rows, cfg) {
  d <- data.table::as.data.table(rows)
  mwd_require(all(d$adequate) && all(is.finite(d$required_scale) & d$required_scale > 0),
    "Calibration contains unresolved calculations or invalid radii.")
  counts <- d[, .N, by = .(response_id, n, content, tolerance_confidence)]
  mwd_require(nrow(counts) == cfg$expected_cells && all(counts$N == cfg$stages$calibration$replications),
    "Calibration must retain every planned sample in every case.")
  cells <- d[, {
    rank <- mbc_rank(.N, tolerance_confidence[1], cfg$calibration_error, cfg$expected_cells)
    .(variant = unique(variant), samples = .N, rank = rank$rank, assurance = rank$assurance,
      scale = sort(required_scale)[rank$rank])
  }, by = .(response_id, n, content, tolerance_confidence)]
  policy <- cells[, .(variant = unique(variant), multiplier = max(scale),
    cases = .N, calibration_samples_per_case = unique(samples), rank = unique(rank)),
    by = .(n, content, tolerance_confidence)]
  mwd_require(nrow(policy) == 9L && all(policy$cases == 8L), "The calibrated design panel differs.")
  list(policy = policy, cells = cells)
}

mbc_atomic_fit <- function(value, path, identity, task) {
  tmp <- tempfile(".boundary-fit-", tmpdir = dirname(path)); on.exit(unlink(tmp))
  saveRDS(value, tmp, compress = "gzip")
  mwd_require(file.rename(tmp, path), "Cannot preserve a boundary-study result.")
  mgfp_json_write(list(identity = identity, task_sha256 = mwd_hash(task),
    sha256 = mwd_file_hash(path), finished = mgfp_time()), paste0(path, ".json"))
}

mbc_read_fit <- function(path, identity, task) {
  receipt <- mwd_json(paste0(path, ".json"))
  mwd_require(identical(receipt$identity, identity) && identical(receipt$task_sha256, mwd_hash(task)) &&
    file.exists(path) && identical(receipt$sha256, mwd_file_hash(path)), "Saved study identity or checksum differs.")
  readRDS(path)
}
