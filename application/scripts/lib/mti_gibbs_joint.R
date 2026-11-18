# The fitting functions take observations and fixed rules, never a true CDF.
mjc_validate <- function(cfg) {
  mwd_require(identical(as.numeric(cfg$root_sd), c(2, 1)) &&
    setequal(cfg$dp_concentration, c(.5, 1, 2)) && setequal(cfg$dp_base_sd, c(2, 4, 8)) &&
    cfg$independent_coordinates$center == 0 && cfg$independent_coordinates$scale == 1 &&
    !cfg$independent_coordinates$estimated_from_analysis_sample && cfg$learning_rate == 1 &&
    cfg$radius_multiplier == 1 && !cfg$automatic_article_replacement && !cfg$application_transfer &&
    cfg$assessment_revision == 2 && cfg$width_reference == "frozen_calibrated_R0" &&
    cfg$validation_domain == "advancing_designs_only" &&
    cfg$maximum_initial_rules == 5 && cfg$maximum_refinement_rules == 2 &&
    cfg$development_replications == 100 && cfg$initial_check_replications == 12 &&
    cfg$expected_cells == 72 && cfg$validation_replications == 1000 &&
    cfg$maximum_workers <= 40 && cfg$sampling$chains == 4 &&
    cfg$sampling$maximum_retained >= cfg$sampling$n_mcmc &&
    cfg$selection$minimum_worst_law_width_reduction > 0 &&
    cfg$selection$minimum_worst_law_width_reduction < 1 &&
    cfg$selection$paired_interval_probability > 0 && cfg$selection$paired_interval_probability < 1 &&
    cfg$selection$require_remaining_development_attainment &&
    cfg$selection$require_remaining_development_width_gain,
    "The independent-prior developmental specification changed.")
  invisible(TRUE)
}

mjc_models <- function(cfg) {
  d <- expand.grid(prior_sd = cfg$root_sd, concentration = cfg$dp_concentration,
    base_sd = cfg$dp_base_sd, KEEP.OUT.ATTRS = FALSE)
  d$model <- sprintf("sd%g_a%g_h%g", d$prior_sd, d$concentration, d$base_sd)
  d$reference_dp <- d$concentration == 1 & d$base_sd == 4
  d
}

mjc_probabilities <- function(y, lower, upper, content, concentration, base_sd) {
  mwd_require(length(y) >= 2 && all(is.finite(y)) && all(is.finite(c(lower, upper))) &&
    length(lower) == length(upper) && length(lower) > 0 &&
    all(lower <= upper) && concentration > 0 && base_sd > 0 && content > 0 && content < 1,
    "Invalid DP content calculation.")
  counts <- vapply(seq_along(lower), function(i) sum(y >= lower[i] & y <= upper[i]), 0)
  mass <- pnorm(upper, sd = base_sd) - pnorm(lower, sd = base_sd)
  a <- counts + concentration * mass
  b <- length(y) - counts + concentration * (1 - mass)
  out <- numeric(length(a))
  interior <- a > 0 & b > 0
  out[interior] <- pbeta(content, a[interior], b[interior], lower.tail = FALSE)
  out[b == 0 & a > 0] <- 1
  out
}

mjc_reweight <- function(candidates, y, content, model) {
  d <- as.data.frame(candidates)
  valid <- d$finite_interval & is.finite(d$lower) & is.finite(d$upper) & d$lower <= d$upper
  d$posterior_content_probability <- NA_real_
  d$posterior_content_probability[valid] <- mjc_probabilities(y, d$lower[valid], d$upper[valid],
    content, model$concentration, model$base_sd)
  d
}

# Each region is (lo, hi]; equality stays with the less restrictive selection.
mjc_regions <- function(paths, reference_gamma) {
  events <- sort(unique(c(0, 1, unlist(lapply(paths, function(z) z$posterior_content_probability)))))
  events <- events[is.finite(events) & events >= 0 & events <= 1]
  lo <- head(events, -1); hi <- tail(events, -1)
  gamma <- lo + (hi - lo) / 2
  use_reference <- reference_gamma > lo & reference_gamma <= hi
  gamma[use_reference] <- reference_gamma
  gamma[gamma <= lo & hi < 1] <- hi[gamma <= lo & hi < 1]
  keep <- gamma > lo & gamma <= hi & gamma > 0 & gamma < 1
  data.frame(gamma = gamma[keep], region_lower = lo[keep], region_upper = hi[keep],
    region_length = hi[keep] - lo[keep])
}

mjc_curve <- function(paths, reference_widths, reference_gamma, content, confidence) {
  regions <- mjc_regions(paths, reference_gamma)
  laws <- unique(vapply(paths, function(z) z$response_id[1], ""))
  widths <- attained <- returned <- matrix(NA_real_, nrow(regions), length(laws))
  for (j in seq_along(laws)) {
    law <- laws[j]; pieces <- paths[vapply(paths, function(z) z$response_id[1] == law, FALSE)]
    mwd_require(length(pieces) > 0 && all(vapply(pieces, nrow, 1L) > 0), "An ECM path is empty.")
    initial <- vapply(pieces, function(z) z$width[1], 0)
    initial_a <- vapply(pieces, function(z) as.numeric(z$population_content[1] >= content), 0)
    events <- data.table::rbindlist(lapply(pieces, function(z) data.frame(
      breakpoint = z$posterior_content_probability,
      dw = diff(c(z$width, 0)), da = diff(c(as.numeric(z$population_content >= content), 0)),
      dr = c(rep(0, nrow(z) - 1L), -1))))
    events <- events[, .(dw = sum(dw), da = sum(da), dr = sum(dr)), by = breakpoint][order(breakpoint)]
    index <- findInterval(regions$gamma, events$breakpoint, left.open = TRUE)
    baseline <- reference_widths[[law]]
    mwd_require(is.finite(baseline) && baseline > 0, "Reference ECM widths are incomplete.")
    widths[, j] <- (sum(initial) + c(0, cumsum(events$dw))[index + 1L]) / length(pieces) / baseline
    attained[, j] <- (sum(initial_a) + c(0, cumsum(events$da))[index + 1L]) / length(pieces)
    returned[, j] <- (length(pieces) + c(0, cumsum(events$dr))[index + 1L]) / length(pieces)
  }
  complete <- apply(returned, 1, min) == 1
  widths[!complete, ] <- Inf
  cbind(regions, worst_ratio = apply(widths, 1, max), mean_log_ratio = rowMeans(log(widths)),
    minimum_attainment = apply(attained, 1, min), complete = complete,
    attainment_pass = complete & apply(attained, 1, min) >= confidence - 1e-12)
}

mjc_order <- function(d) order(signif(d$worst_ratio, 12), signif(d$mean_log_ratio, 12), !d$reference_dp,
  -d$region_length, d$model, -d$gamma)

mjc_nominate <- function(curves, excluded = character(), refinement = FALSE) {
  d <- as.data.frame(curves)
  d <- d[d$complete & is.finite(d$worst_ratio) & !d$rule %in% excluded, ]
  if (!nrow(d)) return(d)
  selections <- list()
  if (!refinement) {
    for (sd in unique(d$prior_sd)) {
      z <- d[d$prior_sd == sd, ]; z <- z[mjc_order(z), ]
      selections[[length(selections) + 1L]] <- head(z[z$attainment_pass, ], 1L)
      selections[[length(selections) + 1L]] <- head(z, 1L)
    }
    return(unique(data.table::rbindlist(selections, fill = TRUE), by = "rule"))
  }
  # One further attainable and one unrestricted alternative, not an open search.
  d <- d[mjc_order(d), ]
  a <- head(d[d$attainment_pass, ], 1L)
  b <- head(d[!d$rule %in% a$rule, ], 1L)
  unique(data.table::rbindlist(list(a, b), fill = TRUE), by = "rule")
}

mjc_fit_anchor <- function(y, task, sd, cfg, context) {
  reference <- context$prior$simulation_reference
  mwd_require(reference$center == 0 && reference$scale == 1 && !reference$estimated_from_analysis_sample,
    "Root priors require independently fixed response coordinates.")
  rule <- as.list(context$policy[context$policy$n == task$n & context$policy$content == task$content, ][1, ])
  a <- mfp_fit_data(y, task$content, task$tolerance_confidence, task$retained_fraction, sd,
    reference, context$profile, context$dp, rule, context$prior$numerical, cfg$sampling,
    as.integer(task$ecm_seed + match(sd, cfg$root_sd)), context$policy_diagnostics, sample_gibbs = FALSE)
  mwd_require(!is.null(a$candidates) && !is.null(a$unresolved_candidates) && a$unresolved_candidates == 0 &&
    all(a$candidates$finite_interval & a$candidates$numerically_accurate),
    paste("An ECM candidate requires numerical review:", a$reason %||% "incomplete modes"))
  mwd_require(a$prior$prior_sd == sd && identical(a$sample_sha256, mwd_hash(y)), "The fitted prior or data differ.")
  a
}

mjc_select <- function(y, anchor, rule, content, variant) {
  mwd_require(anchor$prior$prior_sd == rule$prior_sd, "An ECM table was fitted under a different prior.")
  d <- mjc_reweight(anchor$candidates, y, content, rule)
  selected <- mdc_select(d, rule$gamma)
  if (!nrow(selected)) return(NULL)
  geometry <- mbc_project_tilt(y, selected$target_content, selected$mean_tilt, variant)
  list(selected = selected, q = selected$target_content, content = content, delta_reference = selected$mean_tilt,
    geometry = geometry)
}

mjc_sampler_hashes <- function() {
  paths <- c("application/scripts/lib/mti_gibbs_practical.R",
    "application/scripts/lib/mti_gibbs_width_diagnosis.R")
  root <- if (file.exists(paths[1])) "." else "../../.."
  list(version = 2L, files = as.list(setNames(vapply(file.path(root, paths), mwd_file_hash, ""), paths)),
    sampling_function = mwd_hash(list(formals(mjc_sample), body(mjc_sample))))
}

mjc_target_key <- function(y, target, sd, cfg, policy) mwd_hash(list(
  observations = mwd_hash(y), coordinates = cfg$independent_coordinates,
  prior_sd = sd, learning_rate = cfg$learning_rate, q = target$q, delta = target$geometry$delta,
  sampling = cfg$sampling, diagnostics = policy,
  sampler_source = mjc_sampler_hashes()))

mjc_sample <- function(y, target, sd, cfg, policy, seed) {
  ec <- unlist(target$selected[c("lower", "upper")], use.names = FALSE)
  initial <- mwd_initial(y, ec[1], ec[2], sd^2, 0)
  settings <- c(cfg$sampling, list(learning_rate = cfg$learning_rate, prior_variance = sd^2,
    initial_midpoint_offsets_sd = initial$offsets, initial_halfwidth_multipliers = initial$radii))
  history <- list()
  schedule <- unique(c(pmin(cfg$sampling$maximum_retained, cfg$sampling$n_mcmc * 2^(0:3)),
    cfg$sampling$maximum_retained))
  for (keep in schedule) {
    fits <- lapply(seq_len(settings$chains), function(chain) mgp_slice_fit(y,
      data.frame(chain = chain, seed = as.integer(seed + chain), q = target$q,
        delta = target$geometry$delta, ecm_lower = ec[1], ecm_upper = ec[2]), settings, n_keep = keep))
    task <- data.frame(case_id = "joint_prior", dataset_id = 0L, source = "simulation",
      response_id = "not_supplied_to_sampler", n = length(y), content = target$content %||% NA_real_, q = target$q,
      delta = target$geometry$delta, ecm_lower = ec[1], ecm_upper = ec[2])
    parameters <- mgp_summaries(fits, task, policy)
    adequate <- all(parameters$preferred_pass & parameters$mean_precision)
    history[[length(history) + 1L]] <- data.frame(retained = keep, adequate = adequate,
      max_rhat = max(parameters$rhat), min_bulk_ess = min(parameters$ess_bulk),
      min_tail_ess = min(parameters$ess_tail), max_mcse_mean = max(parameters$mcse_mean))
    if (adequate) break
  }
  fits <- lapply(fits, function(f) f[setdiff(names(f), "target_loss_trace")])
  list(fits = fits, parameters = parameters, history = do.call(rbind, history), adequate = adequate,
    prior_sd = sd, q = target$q, delta = target$geometry$delta, seed = seed)
}

mjc_interval <- function(record) {
  p <- setNames(record$parameters$mean, record$parameters$variable)
  interval <- unname(p[c("lower", "upper")])
  mwd_require(length(interval) == 2 && all(is.finite(interval)) && diff(interval) > 0 &&
    abs(diff(interval) - p["width"]) < 1e-9 * max(1, p["width"]),
    "The ordered-root mean interval is inconsistent with mean width.")
  interval
}

mjc_reference_radius <- function(radius, designs) {
  r <- data.table::as.data.table(radius)
  keys <- c("n", "content", "tolerance_confidence")
  mwd_require(all(c(keys, "rule", "multiplier") %in% names(r)), "Incomplete reference radius policy.")
  r <- r[rule == "proposed"]
  mwd_require(!anyDuplicated(r[, ..keys]) && all(is.finite(r$multiplier) & r$multiplier > 0),
    "Duplicate or invalid reference radius policy.")
  d <- unique(data.table::as.data.table(designs)[, ..keys])
  out <- merge(d, r[, c(keys, "multiplier"), with = FALSE], by = keys, all.x = TRUE)
  mwd_require(nrow(out) == nrow(d) && !anyNA(out$multiplier), "Missing design or confidence in reference radius policy.")
  out
}

mjc_scores <- function(rows, cfg, radius) {
  d <- data.table::as.data.table(rows)
  policy <- mjc_reference_radius(radius, d)
  reference <- d[rule == "R0", .(job_id, response_id, n, content, tolerance_confidence, reference_width = width,
    reference_returned = returned, reference_attained = attained)]
  mwd_require(!anyDuplicated(reference[, .(job_id, response_id)]), "Duplicate R0 developmental results.")
  reference <- merge(reference, policy, by = c("n", "content", "tolerance_confidence"))
  reference[, reference_width := reference_width * multiplier]
  reference[, c("n", "content", "tolerance_confidence", "multiplier") := NULL]
  z <- merge(d[rule != "R0"], reference, by = c("job_id", "response_id"))
  mwd_require(nrow(z) == nrow(d[rule != "R0"]), "Missing R0 developmental results.")
  if (!nrow(z)) return(list(scores = data.table::data.table(), cells = data.table::data.table()))
  bound <- function(x) {
    if (length(x) < 2 || any(!is.finite(x))) return(Inf)
    mean(x) + qt((1 + cfg$selection$paired_interval_probability) / 2, length(x) - 1) * sd(x) / sqrt(length(x))
  }
  cells <- z[, .(complete = all(returned & adequate & reference_returned),
    attainment = mean(attained), remaining_attainment = mean(attained[replication > cfg$initial_check_replications]),
    mean_ratio = if (all(returned & reference_returned)) mean(width) / mean(reference_width) else Inf,
    remaining_ratio = if (all(returned & reference_returned))
      mean(width[replication > cfg$initial_check_replications]) /
        mean(reference_width[replication > cfg$initial_check_replications]) else Inf,
    paired_log_upper = bound(log(width / reference_width))), by = .(n, content, tolerance_confidence, rule, response_id)]
  out <- cells[, .(complete = all(complete), minimum_attainment = min(attainment),
    minimum_remaining_attainment = min(remaining_attainment), worst_ratio = max(mean_ratio),
    mean_log_ratio = mean(log(mean_ratio)), worst_remaining_ratio = max(remaining_ratio),
    worst_paired_log_upper = max(paired_log_upper), laws = .N), by = .(n, content, tolerance_confidence, rule)]
  out[, advance := complete & laws == 8 & minimum_attainment >= tolerance_confidence - 1e-12 &
    minimum_remaining_attainment >= tolerance_confidence - 1e-12 &
    worst_ratio <= 1 - cfg$selection$minimum_worst_law_width_reduction &
    worst_remaining_ratio < 1 & worst_paired_log_upper < 0]
  list(scores = out, cells = cells)
}

mjc_summary <- function(rows, cfg) data.table::as.data.table(rows)[, {
  success <- sum(attained); m <- .N
  interval <- binom.test(success, m)$conf.int
  widths <- width[returned & is.finite(width)]
  assurance <- if (method[1] != "gibbs_new_unscaled") NA_real_ else if (success == 0) 0 else
    qbeta(cfg$assurance$error / cfg$assurance$comparisons, success, m - success + 1)
  .(samples = m, returned = sum(returned), attained = success, attainment = success / m,
    attainment_mcse = sqrt(success / m * (1 - success / m) / m),
    attainment_lower = interval[1], attainment_upper = interval[2], simultaneous_lower = assurance,
    width_samples = length(widths), mean_width = if (length(widths)) mean(widths) else NA_real_,
    width_q025 = if (length(widths)) unname(quantile(widths, .025)) else NA_real_,
    width_q975 = if (length(widths)) unname(quantile(widths, .975)) else NA_real_,
    mean_population_content = if (any(returned)) mean(population_content[returned]) else NA_real_)
}, by = .(n, content, tolerance_confidence, response_id, method)]
