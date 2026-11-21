# Statistical choices are learned on development samples, never assessment samples.
mac_validate <- function(cfg) {
  mwd_require(cfg$prior_sd == 2 && cfg$learning_rate == 1 && cfg$radius_multiplier == 1 &&
    cfg$dp_concentration == 1 && cfg$dp_base_sd == 4 && cfg$coordinates$center == 0 &&
    cfg$coordinates$scale == 1 && identical(cfg$coordinates$estimated_from_analysis_sample, FALSE) &&
    cfg$expected_cells == 72 && cfg$development_replications == 1000 && cfg$validation_replications == 1000 &&
    !cfg$sample_gibbs && !cfg$automatic_article_replacement && !cfg$application_transfer &&
    identical(cfg$fresh_selection_assessment, FALSE), "The fixed-prior ECM assessment specification changed.")
  integers <- c(cfg$maximum_workers, cfg$folds, cfg$maximum_cutoffs, cfg$minimum_folds_with_gain,
    cfg$refinement$screen_replications, cfg$refinement$maximum_new_pairs,
    cfg$refinement$competitive_candidates, cfg$refinement$trace_candidates)
  mwd_require(all(is.finite(integers) & integers >= 1 & integers == floor(integers)) &&
    cfg$maximum_workers <= 40 && cfg$folds >= 2 && cfg$maximum_cutoffs >= 2 &&
    cfg$minimum_folds_with_gain <= cfg$folds && cfg$refinement$rounds == 1 &&
    cfg$refinement$screen_replications <= cfg$development_replications &&
    cfg$minimum_case_width_gain > 0 && cfg$minimum_case_width_gain < 1 &&
    cfg$minimum_branch_fraction > 0 && cfg$minimum_branch_fraction < .5 &&
    all(cfg$split_quantiles > 0 & cfg$split_quantiles < 1) &&
    setequal(cfg$features, c("absolute_bowley", "log_tail_weight")) &&
    cfg$report_error > 0 && cfg$report_error < 1 && cfg$disk_reserve_gib >= 32 &&
    cfg$memory_per_worker_gib > 0 && cfg$memory_reserve_gib >= 0,
    "Invalid bounded search, numerical resources or Monte Carlo controls.")
  invisible(TRUE)
}

mac_features <- function(y) {
  mwd_require(length(y) >= 2 && all(is.finite(y)), "Finite observations are required for sample features.")
  q <- unname(quantile(y, c(.1, .25, .5, .75, .9), type = 8))
  spread <- q[4] - q[2]
  good <- is.finite(spread) && spread > 0 && q[5] > q[1]
  data.frame(absolute_bowley = if (good) abs((q[4] + q[2] - 2*q[3])/spread) else NA_real_,
    log_tail_weight = if (good) log((q[5] - q[1])/spread) else NA_real_, degenerate = !good)
}

mac_constant <- function(gamma, reference = gamma) list(feature = "constant", split = NA_real_,
  gamma_low = gamma, gamma_high = gamma, gamma_invalid = reference)

# A deployment rule contains no family label, CDF, or population-content score.
mac_predict <- function(rule, features) {
  needed <- c("feature", "split", "gamma_low", "gamma_high", "gamma_invalid")
  mwd_require(setequal(names(rule), needed) && length(rule$feature) == 1 &&
    rule$feature %in% c("constant", "absolute_bowley", "log_tail_weight") &&
    all(is.finite(unlist(rule[c("gamma_low", "gamma_high", "gamma_invalid")])) &
      unlist(rule[c("gamma_low", "gamma_high", "gamma_invalid")]) > 0 &
      unlist(rule[c("gamma_low", "gamma_high", "gamma_invalid")]) < 1), "Invalid frozen cutoff rule.")
  if (rule$feature == "constant") return(rep(rule$gamma_low, nrow(features)))
  mwd_require(is.finite(rule$split) && rule$feature %in% names(features), "Undefined sample-feature split.")
  z <- features[[rule$feature]]
  ifelse(!is.finite(z), rule$gamma_invalid, ifelse(z <= rule$split, rule$gamma_low, rule$gamma_high))
}

mac_path <- function(candidates, cdf, content, gamma) {
  p <- mdc_path(candidates)
  mwd_require(nrow(p) > 0 && any(p$posterior_content_probability >= gamma),
    "The primary reference has no eligible accurate interval.")
  # Nothing above the first reference-eligible row can be selected at gamma <= reference.
  p <- head(p, which(p$posterior_content_probability >= gamma)[1])
  p$population_content <- cdf(p$upper) - cdf(p$lower)
  p$attained <- p$population_content >= content
  p
}

mac_references <- function(paths, gamma) data.frame(
  reference_width = vapply(paths, function(p) mdc_select(p, gamma)$width, 0),
  reference_attained = vapply(paths, function(p) mdc_select(p, gamma)$attained, FALSE))

mac_cutoffs <- function(paths, gamma, maximum, content, confidence, laws, references, error) {
  candidates <- sort(unique(c(gamma, unlist(lapply(paths, function(p)
    p$posterior_content_probability[p$posterior_content_probability > 0 & p$posterior_content_probability <= gamma])))))
  # Rank compression is explicit; constant-cutoff optima are also retained below.
  if (length(candidates) > maximum) candidates <- candidates[unique(round(seq(1, length(candidates), length.out = maximum)))]
  means <- tapply(references$reference_width, laws, mean)
  annotated <- lapply(seq_along(paths), function(i) {p <- paths[[i]]; p$response_id <- laws[i]; p})
  curve <- mec_curve(annotated, means, gamma, content, confidence, error)
  eligible <- curve[curve$gamma <= gamma & curve$complete & curve$minimum_attainment >= confidence - 1e-12, ]
  if (nrow(eligible)) {
    best <- eligible[order(signif(eligible$worst_ratio, 12), signif(eligible$mean_log_ratio, 12), -eligible$gamma), ][1, ]
    candidates <- c(candidates, best$gamma)
  }
  sort(unique(c(candidates, gamma)))
}

# Event sums avoid selecting every dataset separately for every cutoff pair.
mac_moments <- function(paths, laws, use, gammas, law_names) {
  width <- success <- matrix(0, length(gammas), length(law_names))
  counts <- integer(length(law_names))
  for (j in seq_along(law_names)) {
    ids <- which(use & laws == law_names[j]); counts[j] <- length(ids)
    if (!length(ids)) next
    pieces <- paths[ids]
    initial_width <- sum(vapply(pieces, function(p) p$width[1], 0))
    initial_success <- sum(vapply(pieces, function(p) as.numeric(p$attained[1]), 0))
    events <- data.table::rbindlist(lapply(pieces, function(p) {
      if (nrow(p) < 2) return(NULL)
      data.frame(breakpoint = head(p$posterior_content_probability, -1),
        dw = diff(p$width), da = diff(as.numeric(p$attained)))
    }))
    if (nrow(events)) {
      events <- events[, .(dw = sum(dw), da = sum(da)), by = breakpoint][order(breakpoint)]
      index <- findInterval(gammas, events$breakpoint, left.open = TRUE)
      width[, j] <- initial_width + c(0, cumsum(events$dw))[index + 1L]
      success[, j] <- round(initial_success + c(0, cumsum(events$da))[index + 1L])
    } else {
      width[, j] <- initial_width; success[, j] <- initial_success
    }
  }
  list(width = width, success = success, counts = counts)
}

mac_case_scores <- function(rows) {
  data.table::as.data.table(rows)[, .(samples = .N, attainment = mean(attained),
    reference_attainment = mean(reference_attained), mean_width = mean(width),
    reference_mean_width = mean(reference_width), mean_width_ratio = mean(width)/mean(reference_width),
    mean_width_change = mean(width - reference_width),
    width_change_mcse = if (.N > 1) sd(width - reference_width)/sqrt(.N) else NA_real_,
    newly_attained = sum(attained & !reference_attained),
    lost_attainment = sum(!attained & reference_attained)), by = response_id]
}

mac_evaluate <- function(paths, features, laws, rule, gamma, references = NULL) {
  if (is.null(references)) references <- mac_references(paths, gamma)
  mwd_require(nrow(references) == length(paths), "Paired reference observations differ.")
  cutoffs <- mac_predict(rule, features)
  mwd_require(all(cutoffs <= gamma), "A proposed cutoff exceeds the reference.")
  data.table::rbindlist(lapply(seq_along(paths), function(i) {
    chosen <- mdc_select(paths[[i]], cutoffs[i])
    mwd_require(nrow(chosen) == 1 &&
      chosen$width <= references$reference_width[i] + 64*.Machine$double.eps*max(1, references$reference_width[i]),
      "Retaining the reference candidates failed width nonincrease.")
    data.frame(response_id = laws[i], width = chosen$width, attained = chosen$attained,
      reference_width = references$reference_width[i], reference_attained = references$reference_attained[i],
      gamma = cutoffs[i], candidate_index = chosen$candidate_index)
  }))
}

mac_train <- function(paths, features, laws, gamma, content, confidence, cfg, allow_feature = TRUE, references = NULL) {
  mwd_require(length(paths) == nrow(features) && length(paths) == length(laws) && length(paths) > 0,
    "Development observations and candidate paths differ.")
  names_laws <- sort(unique(laws))
  if (is.null(references)) references <- mac_references(paths, gamma)
  baseline <- references$reference_width
  totals <- as.numeric(tapply(baseline, factor(laws, levels = names_laws), sum))
  counts <- as.numeric(table(factor(laws, levels = names_laws)))
  gammas <- mac_cutoffs(paths, gamma, cfg$maximum_cutoffs, content, confidence, laws, references, cfg$report_error)
  best <- NULL
  consider <- function(w, s, rule, complexity) {
    ratios <- as.numeric(w)/totals; rates <- as.numeric(s)/counts
    if (any(!is.finite(ratios)) || any(rates < confidence - 1e-12) || any(ratios > 1 + 1e-12)) return()
    score <- c(signif(max(ratios), 12), signif(mean(log(ratios)), 12), complexity,
      -min(rule$gamma_low, rule$gamma_high), -max(rule$gamma_low, rule$gamma_high))
    if (is.null(best) || do.call(order, lapply(seq_along(score), function(j) c(score[j], best$score[j])))[1] == 1L) {
      best <<- list(rule = rule, score = score, minimum_attainment = min(rates),
        mean_log_ratio = mean(log(ratios)), minimum_ratio = min(ratios),
        maximum_ratio = max(ratios), available = TRUE)
    }
  }
  moments <- mac_moments(paths, laws, rep(TRUE, length(paths)), gammas, names_laws)
  for (i in seq_along(gammas)) consider(moments$width[i, ], moments$success[i, ], mac_constant(gammas[i], gamma), 0)
  if (allow_feature) for (feature in cfg$features) {
    z <- features[[feature]]; valid <- is.finite(z)
    if (sum(valid) < 2) next
    splits <- unique(unname(quantile(z[valid], cfg$split_quantiles, type = 8)))
    invalid <- mac_moments(paths, laws, !valid, gamma, names_laws)
    for (split in splits) {
      low <- valid & z <= split; high <- valid & z > split
      if (min(sum(low), sum(high))/length(paths) < cfg$minimum_branch_fraction) next
      a <- mac_moments(paths, laws, low, gammas, names_laws)
      b <- mac_moments(paths, laws, high, gammas, names_laws)
      # Enumerate a bounded, probability-event-derived pair menu; no monotonicity assumption.
      for (i in seq_along(gammas)) for (j in seq_along(gammas)) consider(
        a$width[i, ] + b$width[j, ] + invalid$width[1, ],
        a$success[i, ] + b$success[j, ] + invalid$success[1, ],
        list(feature = feature, split = split, gamma_low = gammas[i], gamma_high = gammas[j], gamma_invalid = gamma), 1)
    }
  }
  if (is.null(best)) best <- list(rule = mac_constant(gamma), available = FALSE,
    reason = "no_casewise_point_target_in_bounded_development_search")
  best$cutoffs_evaluated <- length(gammas)
  best
}

mac_develop <- function(paths, features, laws, replications, gamma, content, confidence, cfg, references = NULL) {
  if (is.null(references)) references <- mac_references(paths, gamma)
  fold <- (replications - 1L) %% cfg$folds + 1L
  evaluated <- list(); trained <- list()
  for (k in seq_len(cfg$folds)) {
    test <- fold == k; train <- !test
    for (kind in c("constant", "sample_adaptive")) {
      fit <- mac_train(paths[train], features[train, , drop = FALSE], laws[train], gamma,
        content, confidence, cfg, allow_feature = kind == "sample_adaptive", references = references[train, , drop = FALSE])
      value <- mac_evaluate(paths[test], features[test, , drop = FALSE], laws[test], fit$rule, gamma,
        references[test, , drop = FALSE])
      value$fold <- k; value$kind <- kind
      evaluated[[length(evaluated) + 1L]] <- value
      trained[[paste(kind, k)]] <- fit
    }
  }
  rows <- data.table::rbindlist(evaluated)
  scores <- rows[, mac_case_scores(.SD), by = kind]
  passes <- scores[, .(point_target = all(attainment >= confidence - 1e-12),
    minimum_ratio = min(mean_width_ratio), mean_log_ratio = mean(log(mean_width_ratio))), by = kind]
  passes[, gain_folds := vapply(kind, function(which_kind) {
    z <- rows[kind == which_kind, .(gain = mean(log(width/reference_width))), by = fold]
    sum(z$gain < -1e-12)
  }, 0L)]
  passes[, advance := point_target & minimum_ratio <= 1 - cfg$minimum_case_width_gain &
    gain_folds >= cfg$minimum_folds_with_gain]
  permitted <- passes[advance == TRUE]
  chosen_kind <- "reference"
  if (nrow(permitted)) {
    permitted[, complexity := match(kind, c("constant", "sample_adaptive"))]
    data.table::setorder(permitted, mean_log_ratio, complexity)
    chosen_kind <- permitted$kind[1]
    # Prefer the constant rule unless feature adaptation adds a meaningful extra gain.
    if (chosen_kind == "sample_adaptive" && "constant" %in% permitted$kind) {
      a <- permitted[kind == "sample_adaptive", mean_log_ratio]
      b <- permitted[kind == "constant", mean_log_ratio]
      if (exp(a - b) > 1 - cfg$minimum_case_width_gain) chosen_kind <- "constant"
    }
  }
  fit <- if (chosen_kind == "reference") list(rule = mac_constant(gamma), available = FALSE) else
    mac_train(paths, features, laws, gamma, content, confidence, cfg, chosen_kind == "sample_adaptive", references)
  if (!fit$available) chosen_kind <- "reference"
  list(rule = if (chosen_kind == "reference") mac_constant(gamma) else fit$rule,
    kind = chosen_kind, advance = chosen_kind != "reference", scores = scores,
    fold_rows = rows, criteria = passes, training = fit, fold_rules = trained)
}

mac_propose_pairs <- function(y, candidates, content, gamma, context, cfg) {
  mwd_require(cfg$refinement$rounds == 1, "Only one frozen refinement round is supported.")
  selected <- mdc_select(candidates, gamma)
  mwd_require(nrow(selected) == 1, "Candidate refinement requires the current reference interval.")
  d <- as.data.frame(candidates)
  priority <- order(abs(d$posterior_content_probability - gamma), d$width, d$candidate_index)
  seeds <- unique(c(selected$target_content, head(d$target_content[priority], cfg$refinement$competitive_candidates)))
  q_values <- sort(unique(d$target_content))
  middle <- head(q_values, -1) + diff(q_values)/2
  middle <- middle[order(vapply(middle, function(q) min(abs(q - seeds)), 0))]
  center <- getFromNamespace(".rqr_mti_profile_tilt_values", "rqrgibbs")
  pairs <- list()
  add <- function(q, delta) {
    if (is.finite(q) && q > content && q < 1 && is.finite(delta))
      pairs[[length(pairs) + 1L]] <<- data.frame(target_content = q, mean_tilt = delta)
  }
  # Interleave tilt and content refinements, rather than exhausting one axis first.
  for (i in seq_len(max(length(seeds), length(middle)))) {
    if (i <= length(seeds)) {
      q <- seeds[i]; deltas <- sort(unique(d$mean_tilt[d$target_content == q]))
      if (length(deltas) > 1) for (delta in head(deltas, -1) + diff(deltas)/2) add(q, delta)
    }
    if (i <= length(middle)) {
      q <- middle[i]
      central <- do.call(center, c(list(y = y, target_content = q), context$profile$tilt_grid_control))
      for (delta in unique(c(central$central_tilt, 0, central$values))) add(q, delta)
    }
  }
  if (!length(pairs)) return(data.frame(target_content = numeric(), mean_tilt = numeric()))
  out <- unique(data.table::rbindlist(pairs))
  key <- function(z) paste(sprintf("%.17g", z$target_content), sprintf("%.17g", z$mean_tilt))
  out <- out[!key(out) %in% key(d)]
  as.data.frame(head(out, cfg$refinement$maximum_new_pairs))
}

# Reuse the existing numerical row fitter and exact conditional-mode polishing.
mac_refine <- function(y, task, anchor, cfg, context) {
  gamma <- mec_primary(context, task$n, task$content)
  original <- anchor$candidates
  pairs <- mac_propose_pairs(y, original, task$content, gamma, context, cfg)
  if (!nrow(pairs)) return(list(candidates = original, new_pairs = pairs, traces = list(), added_candidates = 0L))
  rowfit <- getFromNamespace(".rqr_mti_profile_ecm_row", "rqrgibbs")
  centerfit <- getFromNamespace(".rqr_mti_profile_tilt_values", "rqrgibbs")
  ec <- context$profile$ecm_control; ec$max_iter <- context$prior$numerical$initial_iterations
  ec$seed <- task$ecm_seed; ec$multistart <- FALSE; ec$store_iteration_trace <- TRUE
  ec$ecm_backend <- "cpp"
  prior <- rqrgibbs::beta_prior("ridge", ridge = list(tau2 = cfg$prior_sd^2))
  fitted <- lapply(seq_len(nrow(pairs)), function(i) {
    q <- pairs$target_content[i]; delta <- pairs$mean_tilt[i]
    central <- do.call(centerfit, c(list(y = y, target_content = q), context$profile$tilt_grid_control))
    rowfit(y, matrix(1, length(y), 1), q, delta, central, 1, prior, ec,
      max(original$candidate_index) + i, return_trace = TRUE)
  })
  rows <- as.data.frame(data.table::rbindlist(lapply(fitted, `[[`, "row"), fill = TRUE))
  mwd_require(all(rows$finite_interval), "An added ECM candidate failed before numerical polishing.")
  action <- list(candidates = rows, candidate_traces = as.data.frame(data.table::rbindlist(lapply(fitted, `[[`, "trace"), fill = TRUE)),
    posterior_distribution_fit = rqrgibbs::rqr_dp_fit(y, cfg$dp_concentration,
      rqrgibbs::rqr_dp_base_normal(0, cfg$dp_base_sd), strict_bayes = TRUE),
    content = task$content, posterior_confidence = gamma)
  result <- mfp_refine(action, y, cfg$prior_sd, context$prior$numerical, ec)
  mwd_require(result$unresolved_candidates == 0L && all(result$candidates$numerically_accurate),
    "An added ECM candidate failed the unchanged final accuracy checks.")
  union <- as.data.frame(data.table::rbindlist(list(original, result$candidates), fill = TRUE))
  mwd_require(identical(union[seq_len(nrow(original)), names(original)], as.data.frame(original)),
    "Candidate refinement changed an original fit.")
  keep <- head(order(abs(result$candidates$posterior_content_probability - gamma), result$candidates$width),
    cfg$refinement$trace_candidates)
  chosen <- mdc_select(union, gamma)$candidate_index
  keep <- unique(c(match(chosen, result$candidates$candidate_index), keep)); keep <- keep[!is.na(keep)]
  traces <- result$traces[keep]
  names(traces) <- as.character(result$candidates$candidate_index[keep])
  list(candidates = union, new_pairs = pairs, traces = traces, added_candidates = nrow(pairs))
}
