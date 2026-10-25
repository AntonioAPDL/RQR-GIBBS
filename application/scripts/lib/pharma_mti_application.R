pma_methods <- c("mti_ecm_adaptive_cell", "young_mathew", "wilks_minmax")
pma_schemes <- c("random", "chronological_forward", "chronological_reverse",
                 "month_grouped", "api_grouped")
pma_default_config <- "application/config/pharma_tolerance_application_mti_only_20261020.json"
pma_canonical_dir <- "figures/data/pharma_mti_application_20261020"

pma_read_config <- function(path = pma_default_config) {
  config <- pta_read_config(path)
  attr(config, "config_path") <- normalizePath(path, mustWork = TRUE)
  pma_validate_config(config)
  config
}

pma_validate_config <- function(config) {
  methods <- unlist(config$methods, use.names = FALSE)
  if (!identical(methods, pma_methods)) {
    pta_stop("The revised application requires exactly the three approved methods.")
  }
  roles <- unlist(config$method_roles)[pma_methods]
  if (!identical(unname(roles), c("proposed", "benchmark", "benchmark"))) {
    pta_stop("MTI-ECM must be the sole proposed method.")
  }
  a <- config$analysis
  if (a$product_code != 23 || a$training_size != 100 || a$heldout_size != 87 ||
      a$content != 0.90 || a$tolerance_confidence != 0.95 ||
      a$confirmatory_splits != 1000 || a$endpoint_convention != "closed") {
    pta_stop("The approved application design has changed.")
  }
  s <- config$sensitivity
  if (s$training_size_min != a$training_size ||
      s$training_size_max != a$training_size) {
    pta_stop("Grouped splits must retain the frozen n=100 calibration cell.")
  }
  if (config$scan_reference$expected_retained_count != 98 ||
      config$scan_reference$expected_retained_fraction != 0.98) {
    pta_stop("The internal calibration anchor has changed.")
  }
  if (s$representative_rule != "minimum_squared_median_MAD_distance_then_smallest_split_id") {
    pta_stop("The representative-split selection rule has changed.")
  }
  invisible(TRUE)
}

pma_verify_hash <- function(path, expected) {
  if (!file.exists(path) || !identical(pta_file_sha256(path), expected)) {
    pta_stop("Frozen input hash mismatch: ", path)
  }
  invisible(TRUE)
}

pma_context <- function(config, repo_root, prepare = TRUE) {
  pma_validate_config(config)
  pma_verify_hash(pta_path(repo_root, config$mti_ecm$policy_csv),
                  config$mti_ecm$policy_sha256)
  pma_verify_hash(pta_path(repo_root, config$mti_ecm$validation_config),
                  config$mti_ecm$validation_config_sha256)
  pma_verify_hash(pta_path(repo_root, config$scan_reference$scan_calibration_csv),
                  config$scan_reference$expected_sha256)
  if (prepare) pta_prepare_data(config, repo_root)
  raw_path <- pta_download_data(config, repo_root)
  raw <- pta_load_raw_data(raw_path)
  data <- pta_code23_data(config, raw)
  if (nrow(raw) != config$data_source$expected_rows ||
      ncol(raw) - 1L != config$data_source$expected_columns ||
      nrow(data) != config$analysis$product_rows) {
    pta_stop("Unexpected raw or product-stratum dimensions.")
  }
  variables <- c(config$analysis$primary_response,
                 config$analysis$supplement_response,
                 unlist(config$sensitivity$grouped_variables))
  if (anyNA(data[, variables, drop = FALSE]) ||
      any(!is.finite(data[[config$analysis$primary_response]])) ||
      any(!is.finite(data[[config$analysis$supplement_response]]))) {
    pta_stop("Responses and grouping variables must be fully observed.")
  }
  legacy <- config
  legacy$tcsp <- config$scan_reference
  list(config = config, data = data, scan = pta_scan_calibration(legacy, repo_root),
       policy = pta_mti_policy_row(config, repo_root),
       profile = pta_mti_profile_config(config, repo_root), repo_root = repo_root)
}

# Suffix subset counts support response-independent whole-group allocation.
pma_subset_counts <- function(weights, target) {
  g <- length(weights)
  counts <- matrix(0, g + 1L, target + 1L)
  counts[g + 1L, 1L] <- 1
  for (i in rev(seq_len(g))) {
    counts[i, ] <- counts[i + 1L, ]
    w <- weights[[i]]
    if (w <= target) {
      for (s in w:target) {
        counts[i, s + 1L] <- counts[i, s + 1L] + counts[i + 1L, s - w + 1L]
      }
    }
  }
  counts
}

pma_group_partitions <- function(groups, target, requested, seed,
                                  max_draws = 250000L, min_groups = 2L) {
  if (anyNA(groups)) pta_stop("Grouped partitions require observed groups.")
  groups <- as.character(groups)
  labels <- sort(unique(groups))
  weights <- tabulate(match(groups, labels), nbins = length(labels))
  counts <- pma_subset_counts(weights, target)
  total <- counts[1L, target + 1L]
  if (total == 0) pta_stop("No whole-group partition matches the frozen training size.")
  # Guard against group-count restrictions silently reducing the counted space.
  if (sum(sort(weights, decreasing = TRUE)[seq_len(min_groups - 1L)]) >=
      min(target, length(groups) - target)) {
    pta_stop("Group sizes do not guarantee the configured minimum group counts.")
  }
  want <- min(as.integer(requested), total)
  out <- list()
  seen <- new.env(hash = TRUE, parent = emptyenv())
  for (attempt in seq_len(max_draws)) {
    draw_seed <- as.integer(seed + attempt)
    set.seed(draw_seed)
    remaining <- target
    selected <- logical(length(weights))
    for (i in seq_along(weights)) {
      w <- weights[[i]]
      include <- if (remaining >= w) counts[i + 1L, remaining - w + 1L] else 0
      exclude <- counts[i + 1L, remaining + 1L]
      if (include + exclude <= 0) pta_stop("Invalid subset allocation state.")
      if (stats::runif(1) < include / (include + exclude)) {
        selected[[i]] <- TRUE
        remaining <- remaining - w
      }
    }
    train <- which(groups %in% labels[selected])
    key <- paste(which(selected), collapse = ",")
    if (!exists(key, envir = seen, inherits = FALSE)) {
      assign(key, TRUE, envir = seen)
      out[[length(out) + 1L]] <- list(train = train, seed = draw_seed,
                                     train_groups = sum(selected),
                                     heldout_groups = sum(!selected))
    }
    if (length(out) == want) break
  }
  if (length(out) != want) pta_stop("Unique grouped partition draw limit reached.")
  attr(out, "available_partitions") <- total
  out
}

pma_make_splits <- function(data, config, mode = "confirmatory") {
  n <- config$analysis$training_size
  N <- nrow(data)
  base <- config$analysis$base_seed
  requested <- if (mode == "smoke") config$analysis$smoke_splits else
    config$analysis$confirmatory_splits
  out <- list()
  add <- function(scheme, id, seed, train, train_groups = NA_integer_,
                  heldout_groups = NA_integer_) {
    out[[length(out) + 1L]] <<- list(
      scheme = scheme, split_id = as.integer(id), seed = as.integer(seed),
      train = sort(train), heldout = setdiff(seq_len(N), train),
      train_groups = as.integer(train_groups),
      heldout_groups = as.integer(heldout_groups))
  }
  for (i in seq_len(requested)) {
    seed <- base + config$analysis$random_seed_offset + i
    set.seed(seed)
    add("random", i, seed, sample.int(N, n))
  }
  chronological <- do.call(order, data[unlist(config$sensitivity$order_variables)])
  offsets <- config$sensitivity$seed_offsets
  if (isTRUE(config$sensitivity$chronological_forward)) {
    add("chronological_forward", 1L, base + offsets$chronological_forward,
        head(chronological, n))
  }
  if (isTRUE(config$sensitivity$chronological_reverse)) {
    add("chronological_reverse", 1L, base + offsets$chronological_reverse,
        tail(chronological, n))
  }
  grouped_n <- if (mode == "smoke") config$sensitivity$smoke_grouped_splits else
    config$sensitivity$requested_unique_splits
  for (scheme in names(config$sensitivity$grouped_variables)) {
    variable <- config$sensitivity$grouped_variables[[scheme]]
    partitions <- pma_group_partitions(
      data[[variable]], n, grouped_n, base + offsets[[scheme]],
      max_draws = config$sensitivity$maximum_partition_draws,
      min_groups = max(config$sensitivity$minimum_training_groups,
                       config$sensitivity$minimum_heldout_groups))
    for (i in seq_along(partitions)) {
      z <- partitions[[i]]
      add(scheme, i, z$seed, z$train, z$train_groups, z$heldout_groups)
    }
  }
  out
}

pma_membership <- function(splits, data) {
  do.call(rbind, lapply(splits, function(s) data.frame(
    split_scheme = s$scheme, split_id = s$split_id, split_seed = s$seed,
    analysis_row_id = data$.analysis_row, source_row_id = data$.source_row,
    training = data$.analysis_row %in% s$train, stringsAsFactors = FALSE)))
}

pma_fit <- function(method, y, context, seed) {
  if (!method %in% pma_methods) pta_stop("Unapproved application method: ", method)
  config <- context$config
  set.seed(seed)
  if (method == "mti_ecm_adaptive_cell") {
    if (!is.null(context$adopted)) return(mpa_application_fit(y, context))
    return(pta_fit_mti_ecm(y, config$analysis$content,
      config$analysis$tolerance_confidence, context$scan, context$policy,
      context$profile, seed))
  }
  if (method == "young_mathew") {
    return(pta_fit_young_mathew(y, config$analysis$content,
                                config$analysis$tolerance_confidence))
  }
  list(returned = TRUE, lower = min(y), upper = max(y),
       width = diff(range(y)), failure_reason = "")
}

pma_run_task <- function(task, context, fit_function = pma_fit) {
  split <- task$split
  data <- context$data
  config <- context$config
  y <- data[[task$response]]
  do.call(rbind, lapply(pma_methods, function(method) {
    seed <- as.integer(split$seed + config$method_seed_offsets[[method]])
    start <- proc.time()[["elapsed"]]
    fit <- tryCatch(fit_function(method, y[split$train], context, seed),
      error = function(e) list(returned = FALSE, lower = NA_real_,
        upper = NA_real_, width = NA_real_, failure_reason = conditionMessage(e)))
    elapsed <- proc.time()[["elapsed"]] - start
    returned <- isTRUE(fit$returned) && length(fit$lower) == 1L &&
      length(fit$upper) == 1L && is.finite(fit$lower) && is.finite(fit$upper) &&
      fit$lower <= fit$upper
    reason <- fit$failure_reason %||% ""
    if (!returned && !nzchar(reason)) reason <- "Nonfinite or unordered fitted endpoints"
    evaluated <- if (returned) {
      rqrgibbs::rqr_interval_empirical_content(y[split$heldout], fit$lower, fit$upper)
    } else data.frame(empirical_content = NA_real_, lower_omitted = NA_real_,
                       upper_omitted = NA_real_)
    data.frame(
      response_id = task$response, response_role = task$role,
      split_scheme = split$scheme, split_id = split$split_id,
      split_seed = split$seed, fit_seed = seed,
      train_n = length(split$train), heldout_n = length(split$heldout),
      train_groups = split$train_groups, heldout_groups = split$heldout_groups,
      train_month_groups = length(unique(data$start[split$train])),
      heldout_month_groups = length(unique(data$start[split$heldout])),
      train_api_groups = length(unique(data$api_batch[split$train])),
      heldout_api_groups = length(unique(data$api_batch[split$heldout])),
      method_id = method, method = config$method_labels[[method]],
      method_role = config$method_roles[[method]],
      content = config$analysis$content,
      tolerance_confidence = config$analysis$tolerance_confidence,
      interval_returned = returned,
      lower = if (returned) fit$lower else NA_real_,
      upper = if (returned) fit$upper else NA_real_,
      width = if (returned) fit$upper - fit$lower else NA_real_,
      heldout_empirical_content = evaluated$empirical_content,
      heldout_lower_omitted = evaluated$lower_omitted,
      heldout_upper_omitted = evaluated$upper_omitted,
      heldout_attains_content = returned &&
        evaluated$empirical_content >= config$analysis$content,
      scan_reference_retained_count = context$scan$retained_count,
      scan_reference_retained_fraction = context$scan$retained_fraction,
      mti_selected_q = fit$selected_q %||% NA_real_,
      mti_selected_tilt = fit$selected_tilt %||% NA_real_,
      mti_central_tilt = fit$central_tilt %||% NA_real_,
      mti_posterior_content_probability = fit$posterior_content_probability %||% NA_real_,
      mti_posterior_confidence = fit$posterior_confidence %||% NA_real_,
      mti_candidates_evaluated = fit$candidates_evaluated %||% NA_integer_,
      mti_feasible_count = fit$feasible_count %||% NA_integer_,
      mti_ecm_converged = fit$ecm_converged %||% NA,
      mti_ecm_iterations = fit$ecm_iterations %||% NA_integer_,
      mti_ecm_final_stationarity = fit$ecm_final_stationarity %||% NA_real_,
      mti_computation = if(method == pma_methods[1]) fit$computation %||% "ecm" else NA_character_,
      mti_profile_converged = fit$profile_converged %||% NA,
      mti_profile_iterations = fit$profile_iterations %||% NA_integer_,
      mti_relative_objective_gap = fit$relative_objective_gap %||% NA_real_,
      mti_maximum_candidate_objective_gap = fit$maximum_candidate_objective_gap %||% NA_real_,
      benchmark_package_version = if (method == "young_mathew")
        fit$source_version %||% NA_character_ else NA_character_,
      failure_reason = reason, elapsed_sec = elapsed, stringsAsFactors = FALSE)
  }))
}

pma_validate_results <- function(results, splits = NULL) {
  required <- c("response_id", "response_role", "split_scheme", "split_id",
    "split_seed", "method_id", "method_role", "interval_returned", "lower",
    "upper", "width", "heldout_empirical_content", "heldout_attains_content",
    "heldout_lower_omitted", "heldout_upper_omitted", "train_n", "heldout_n",
    "failure_reason", "mti_ecm_converged", "mti_ecm_iterations",
    "mti_ecm_final_stationarity", "mti_posterior_content_probability",
    "mti_posterior_confidence")
  if (length(setdiff(required, names(results)))) pta_stop("Incomplete canonical result schema.")
  if (!setequal(unique(results$method_id), pma_methods)) {
    pta_stop("Canonical results must contain exactly the approved methods.")
  }
  key <- paste(results$response_id, results$split_scheme, results$split_id,
               results$method_id, sep = "|")
  if (anyDuplicated(key)) pta_stop("Duplicate response-method-split result.")
  counts <- table(paste(results$response_id, results$split_scheme, results$split_id))
  if (any(counts != 3L)) pta_stop("A split is missing a method or failure row.")
  grouped <- split(results, paste(results$split_scheme, results$split_id))
  if (any(vapply(grouped, function(z) {
    length(unique(z$split_seed)) != 1L || length(unique(z$response_id)) != 2L ||
      nrow(z) != 6L
  }, logical(1L)))) pta_stop("Responses or methods do not share complete split definitions.")
  if (!is.null(splits) && nrow(results) != length(splits) * 2L * 3L) {
    pta_stop("Observed result-row count differs from the prespecified tasks.")
  }
  if (anyNA(results$interval_returned) || anyNA(results$heldout_attains_content) ||
      any(results$train_n != 100) || any(results$heldout_n != 87)) {
    pta_stop("Invalid return indicators or split sample sizes.")
  }
  ok <- results$interval_returned
  if (any(!is.finite(results$lower[ok])) || any(!is.finite(results$upper[ok])) ||
      any(results$upper[ok] < results$lower[ok]) ||
      any(abs(results$width[ok] - (results$upper[ok] - results$lower[ok])) > 1e-12) ||
      any(results$heldout_attains_content[!ok]) ||
      any(!nzchar(results$failure_reason[!ok]))) {
    pta_stop("Invalid interval endpoints, widths, or failure handling.")
  }
  if (any(abs(results$heldout_empirical_content[ok] +
              results$heldout_lower_omitted[ok] +
              results$heldout_upper_omitted[ok] - 1) > 1e-12)) {
    pta_stop("Held-out content and tail omissions do not sum to one.")
  }
  mti <- results$method_id == "mti_ecm_adaptive_cell" & ok
  profiled <- if("mti_computation" %in% names(results))
    mti & !is.na(results$mti_computation) & results$mti_computation=="fixed_target_profile" else rep(FALSE,nrow(results))
  if(any(profiled) && !all(c("mti_profile_converged","mti_profile_iterations",
    "mti_relative_objective_gap") %in% names(results)))pta_stop("Missing profile diagnostics.")
  ecm <- mti & !profiled
  if (anyNA(results$mti_ecm_converged[ecm]) ||
      any(!is.finite(results$mti_ecm_final_stationarity[ecm])) ||
      any(results$mti_ecm_iterations[ecm] < 1L) ||
      any(results$mti_posterior_content_probability[mti] + 1e-12 <
            results$mti_posterior_confidence[mti])) {
    pta_stop("Incomplete MTI-ECM diagnostics or unsatisfied posterior content check.")
  }
  if(any(profiled) && (anyNA(results$mti_profile_converged[profiled]) ||
    any(!results$mti_profile_converged[profiled]) ||
    any(!is.finite(results$mti_relative_objective_gap[profiled])) ||
    any(abs(results$mti_relative_objective_gap[profiled])>1e-8) ||
    any(!is.na(results$mti_ecm_converged[profiled]))))
    pta_stop("Invalid profiled MTI numerical diagnostics.")
  invisible(TRUE)
}

pma_fit_converged <- function(results) {
  flag <- results$mti_ecm_converged
  if("mti_computation" %in% names(results)) {
    profile <- !is.na(results$mti_computation) & results$mti_computation=="fixed_target_profile"
    flag[profile] <- results$mti_profile_converged[profile]
  }
  flag
}

pma_summary <- function(results) {
  key <- interaction(results$response_id, results$split_scheme,
                     results$method_id, drop = TRUE, lex.order = TRUE)
  out <- lapply(split(results, key), function(z) {
    r <- z[z$interval_returned, , drop = FALSE]
    data.frame(
      response_id = z$response_id[1], response_role = z$response_role[1],
      split_scheme = z$split_scheme[1], method_id = z$method_id[1],
      method = z$method[1], method_role = z$method_role[1], splits = nrow(z),
      train_n = pta_median(z$train_n), heldout_n = pta_median(z$heldout_n),
      interval_return_rate = mean(z$interval_returned),
      lower_median = pta_median(r$lower), upper_median = pta_median(r$upper),
      width_median = pta_median(r$width), width_q025 = pta_quantile(r$width, .025),
      width_q975 = pta_quantile(r$width, .975),
      heldout_content_median = pta_median(r$heldout_empirical_content),
      heldout_content_q025 = pta_quantile(r$heldout_empirical_content, .025),
      heldout_content_q975 = pta_quantile(r$heldout_empirical_content, .975),
      heldout_attainment_rate = mean(z$heldout_attains_content),
      lower_omitted_median = pta_median(r$heldout_lower_omitted),
      upper_omitted_median = pta_median(r$heldout_upper_omitted),
      elapsed_sec_total = sum(z$elapsed_sec),
      elapsed_sec_median = pta_median(z$elapsed_sec), stringsAsFactors = FALSE)
  })
  if (!length(out)) return(data.frame())
  out <- do.call(rbind, out)
  rownames(out) <- NULL
  out[order(out$response_role, match(out$split_scheme, pma_schemes),
             match(out$method_id, pma_methods)), , drop = FALSE]
}

pma_paired_values <- function(results) {
  keys <- c("response_id", "response_role", "split_scheme", "split_id")
  vars <- c("interval_returned", "lower", "upper", "width",
            "heldout_empirical_content", "heldout_lower_omitted",
            "heldout_upper_omitted")
  mti <- results[results$method_id == pma_methods[1], c(keys, vars)]
  other <- results[results$method_id != pma_methods[1],
                   c(keys, "method_id", "method", vars)]
  both <- merge(mti, other, by = keys, suffixes = c("_mti", "_benchmark"))
  ok <- both$interval_returned_mti & both$interval_returned_benchmark
  both$joint_return <- ok
  for (v in setdiff(vars, "interval_returned")) {
    both[[paste0(v, "_difference")]] <- ifelse(ok,
      both[[paste0(v, "_mti")]] - both[[paste0(v, "_benchmark")]], NA_real_)
  }
  both$width_ratio <- ifelse(ok & both$width_benchmark > 0,
                            both$width_mti / both$width_benchmark, NA_real_)
  both
}

pma_paired_summary <- function(results) {
  both <- pma_paired_values(results)
  metrics <- c("width_difference", "width_ratio", "lower_difference",
    "upper_difference", "heldout_empirical_content_difference",
    "heldout_lower_omitted_difference", "heldout_upper_omitted_difference")
  key <- interaction(both$response_id, both$split_scheme, both$method_id, drop = TRUE)
  do.call(rbind, lapply(split(both, key), function(z) {
    do.call(rbind, lapply(metrics, function(v) data.frame(
      response_id = z$response_id[1], response_role = z$response_role[1],
      split_scheme = z$split_scheme[1], comparator_id = z$method_id[1],
      comparator = z$method[1], metric = v, splits = nrow(z),
      paired_returns = sum(z$joint_return), finite_pairs = sum(is.finite(z[[v]])),
      median = pta_median(z[[v]]), q025 = pta_quantile(z[[v]], .025),
      q25 = pta_quantile(z[[v]], .25), q75 = pta_quantile(z[[v]], .75),
      q975 = pta_quantile(z[[v]], .975),
      iqr = pta_quantile(z[[v]], .75) - pta_quantile(z[[v]], .25),
      stringsAsFactors = FALSE)))
  }))
}

pma_diagnostics <- function(results) {
  mti <- results[results$method_id == pma_methods[1], , drop = FALSE]
  metrics <- c("mti_candidates_evaluated", "mti_feasible_count", "mti_selected_q",
    "mti_selected_tilt", "mti_posterior_content_probability",
    "mti_ecm_iterations", "mti_ecm_final_stationarity")
  if("mti_computation" %in% names(mti) &&
    all(mti$mti_computation=="fixed_target_profile"))
    metrics <- c(head(metrics,-2L),"mti_profile_iterations","mti_relative_objective_gap")
  key <- interaction(mti$response_id, mti$split_scheme, drop = TRUE)
  do.call(rbind, lapply(split(mti, key), function(z) {
    r <- z[z$interval_returned, , drop = FALSE]
    do.call(rbind, lapply(metrics, function(v) data.frame(
      response_id = z$response_id[1], response_role = z$response_role[1],
      split_scheme = z$split_scheme[1], metric = v, splits = nrow(z),
      returned = nrow(r), return_rate = mean(z$interval_returned),
      convergence_count = sum(z$interval_returned & pma_fit_converged(z), na.rm = TRUE),
      convergence_rate = if (nrow(r)) mean(pma_fit_converged(r)) else NA_real_,
      median = pta_median(r[[v]]), q025 = pta_quantile(r[[v]], .025),
      q25 = pta_quantile(r[[v]], .25), q75 = pta_quantile(r[[v]], .75),
      q975 = pta_quantile(r[[v]], .975), stringsAsFactors = FALSE)))
  }))
}

pma_convergence_sensitivity <- function(results) {
  k <- function(z) paste(z$response_id, z$split_scheme, z$split_id, sep = "|")
  flag <- pma_fit_converged(results)
  keep <- results$method_id == pma_methods[1] & results$interval_returned &
    !is.na(flag) & flag
  included <- k(results) %in% k(results[keep, , drop = FALSE])
  out <- pma_summary(results[included, , drop = FALSE])
  all <- pma_summary(results)
  if (!nrow(out)) out <- all[0L, ]
  keys <- c("response_id", "response_role", "split_scheme", "method_id",
            "method", "method_role")
  denom <- all[, c(keys, "splits")]
  names(denom)[names(denom) == "splits"] <- "original_splits"
  out <- merge(denom, out, by = keys, all.x = TRUE, sort = FALSE)
  out$splits[is.na(out$splits)] <- 0L
  out
}

pma_representative <- function(results) {
  r <- results[results$split_scheme == "random", , drop = FALSE]
  vars <- c("lower", "upper", "width", "heldout_empirical_content",
            "heldout_lower_omitted", "heldout_upper_omitted")
  out <- lapply(split(r, r$response_id), function(z) {
    ids <- sort(unique(z$split_id))
    complete <- vapply(ids, function(i) {
      x <- z[z$split_id == i, ]
      nrow(x) == 3L && all(x$interval_returned) &&
        all(is.finite(as.matrix(x[, vars])))
    }, logical(1L))
    ids <- ids[complete]
    if (!length(ids)) pta_stop("No fully returned primary split for ", z$response_id[1])
    features <- t(vapply(ids, function(i) {
      x <- z[z$split_id == i, ]
      x <- x[match(pma_methods, x$method_id), ]
      as.vector(t(as.matrix(x[, vars])))
    }, numeric(length(vars) * 3L)))
    center <- apply(features, 2L, stats::median)
    scale <- vapply(seq_len(ncol(features)), function(j) {
      s <- stats::mad(features[, j], center = center[j])
      if (!is.finite(s) || s == 0) s <- diff(stats::quantile(
        features[, j], c(.25, .75), type = 8, names = FALSE))
      if (!is.finite(s) || s == 0) s <- 1
      s
    }, numeric(1L))
    distance <- rowSums(sweep(sweep(features, 2L, center, "-"), 2L, scale, "/")^2)
    best <- order(distance, ids)[1L]
    data.frame(response_id = z$response_id[1], response_role = z$response_role[1],
      split_id = ids[best], score = distance[best], eligible_splits = length(ids),
      rule = "minimum_squared_median_MAD_distance_then_smallest_split_id",
      stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

pma_git <- function(...) {
  system2("git", c("--no-optional-locks", ...), stdout = TRUE, stderr = FALSE)
}

pma_file_record <- function(paths) {
  data.frame(path = paths, sha256 = vapply(paths, pta_file_sha256, character(1L)),
             bytes = as.numeric(file.info(paths)$size), stringsAsFactors = FALSE)
}

pma_write_csv <- function(x, path) {
  # Preserve double precision: rounding an endpoint onto a tie changes membership.
  portable <- x
  for (name in names(portable)) {
    if (is.double(portable[[name]])) {
      value <- portable[[name]]
      portable[[name]] <- sprintf("%.17g", value)
      portable[[name]][is.na(value)] <- NA_character_
    }
  }
  pta_atomic_write_csv(portable, path)
}

pma_write_membership <- function(membership, path) {
  con <- gzfile(path, "wt")
  on.exit(close(con))
  utils::write.csv(membership, con, row.names = FALSE)
}

pma_run <- function(context, mode, output_dir, workers) {
  pta_require_packages(c("rqrgibbs", "tolerance", "digest", "jsonlite"))
  if (file.exists(output_dir)) pta_stop("A fresh output directory is required: ", output_dir)
  dir.create(output_dir, recursive = TRUE)
  started <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  start_time <- proc.time()[["elapsed"]]
  config <- context$config
  launch_commit <- pma_git("rev-parse", "HEAD")
  launch_branch <- pma_git("branch", "--show-current")
  launch_status <- pma_git("status", "--porcelain")
  calculation_paths <- c("application/scripts/lib/pharma_tolerance_application.R",
    "application/scripts/lib/pharma_mti_application.R", "application/scripts/86_run_pharma_mti_application.R")
  if(!is.null(context$adopted))calculation_paths <- unique(c(calculation_paths,
    context$adopted$calculation_paths))
  calculation_hashes <- pma_file_record(calculation_paths)
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  splits <- pma_make_splits(context$data, config, mode)
  membership <- pma_membership(splits, context$data)
  pma_write_membership(membership, file.path(output_dir, "split_membership.csv.gz"))
  observed <- context$data[, c(".analysis_row", ".source_row", "batch", "start",
    "api_batch", config$analysis$primary_response, config$analysis$supplement_response)]
  pma_write_csv(observed, file.path(output_dir, "observed_batches.csv"))
  responses <- c(primary = config$analysis$primary_response,
                 supplement = config$analysis$supplement_response)
  tasks <- unlist(lapply(splits, function(s) lapply(names(responses), function(role)
    list(split = s, response = responses[[role]], role = role))), recursive = FALSE)
  workers <- min(as.integer(workers), length(tasks))
  if (!is.finite(workers) || workers < 1L) pta_stop("Invalid worker count.")
  Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1",
             MKL_NUM_THREADS = "1", VECLIB_MAXIMUM_THREADS = "1")
  blocks <- list()
  chunks <- split(seq_along(tasks), ceiling(seq_along(tasks) / config$execution$checkpoint_tasks))
  for (chunk in chunks) {
    one <- function(i) pma_run_task(tasks[[i]], context)
    rows <- if (workers > 1L && .Platform$OS.type != "windows") {
      parallel::mclapply(chunk, one, mc.cores = workers, mc.set.seed = FALSE)
    } else lapply(chunk, one)
    if (any(vapply(rows, inherits, logical(1L), "try-error"))) {
      pta_stop("An application worker failed; partial outputs remain for diagnosis.")
    }
    blocks <- c(blocks, rows)
    results <- do.call(rbind, blocks)
    pma_write_csv(results, file.path(output_dir, "results.csv"))
    pta_atomic_write_json(list(completed_tasks = length(blocks), total_tasks = length(tasks),
      result_rows = nrow(results)), file.path(output_dir, "progress.json"))
  }
  rownames(results) <- NULL
  pma_validate_results(results, splits)
  representatives <- pma_representative(results)
  representative_records <- lapply(seq_len(nrow(representatives)), function(i) {
    r <- as.list(representatives[i, ])
    s <- Filter(function(x) x$scheme == "random" && x$split_id == r$split_id, splits)[[1]]
    r$training_indices <- s$train
    r$heldout_indices <- s$heldout
    r$canonical_results_sha256 <- pta_file_sha256(file.path(output_dir, "results.csv"))
    r$split_membership_sha256 <- pta_file_sha256(file.path(output_dir, "split_membership.csv.gz"))
    r
  })
  pta_atomic_write_json(representative_records, file.path(output_dir, "representative_splits.json"))
  pta_atomic_write_csv(representatives, file.path(output_dir, "representative_splits.csv"))
  pta_atomic_write_csv(pma_summary(results), file.path(output_dir, "method_summary.csv"))
  pta_atomic_write_csv(pma_paired_summary(results), file.path(output_dir, "paired_summary.csv"))
  pta_atomic_write_csv(pma_diagnostics(results), file.path(output_dir, "numerical_diagnostics.csv"))
  pta_atomic_write_csv(pma_convergence_sensitivity(results),
                       file.path(output_dir, "convergence_sensitivity.csv"))
  failures <- results[!results$interval_returned, c("response_id", "split_scheme",
                                                 "method_id", "failure_reason")]
  pta_atomic_write_csv(failures, file.path(output_dir, "failures.csv"))
  writeLines(capture.output(sessionInfo()), file.path(output_dir, "sessionInfo.txt"))
  copy_config <- file.path(output_dir, "config_frozen.json")
  file.copy(attr(config, "config_path"), copy_config, overwrite = FALSE)
  versions <- vapply(c("rqrgibbs", "tolerance", "Rcpp", "RcppArmadillo", "digest", "jsonlite"),
                     function(p) as.character(utils::packageVersion(p)), character(1L))
  if (!identical(calculation_hashes, pma_file_record(calculation_paths))) {
    pta_stop("Calculation sources changed during the run; preserve outputs for review.")
  }
  source_paths <- if(!is.null(context$adopted))c(attr(config,"config_path"),
    context$adopted$input_paths) else c(attr(config, "config_path"),
    pta_path(context$repo_root, config$paths$raw_file),
    pta_path(context$repo_root, config$paths$clean_file),
    pta_path(context$repo_root, config$mti_ecm$policy_csv),
    pta_path(context$repo_root, config$mti_ecm$validation_config),
    pta_path(context$repo_root, config$scan_reference$scan_calibration_csv))
  artifacts <- list.files(output_dir, full.names = TRUE)
  artifact_record <- pma_file_record(artifacts)
  artifact_record$path <- basename(artifact_record$path)
  manifest <- list(
    schema_version = config$schema_version, study_id = config$study_id, mode = mode,
    repository_commit = launch_commit, branch = launch_branch,
    repository_clean = !length(launch_status), git_status = launch_status,
    calculation_sources = calculation_hashes, source_files = pma_file_record(source_paths),
    expected_result_rows = length(tasks) * 3L, observed_result_rows = nrow(results),
    split_counts = as.list(table(vapply(splits, `[[`, character(1L), "scheme"))),
    available_group_partitions = lapply(config$sensitivity$grouped_variables, function(v) {
      w <- as.integer(table(context$data[[v]]))
      pma_subset_counts(w, config$analysis$training_size)[1L, config$analysis$training_size + 1L]
    }),
    return_counts = as.list(tapply(results$interval_returned, results$method_id, sum)),
    failure_counts = as.list(tapply(!results$interval_returned, results$method_id, sum)),
    ecm_convergence_counts = list(converged = sum(results$mti_ecm_converged, na.rm = TRUE),
      not_converged = sum(!results$mti_ecm_converged, na.rm = TRUE),
      not_applicable = sum(results$method_id != pma_methods[1]),
      missing_mti_flag = sum(results$method_id == pma_methods[1] & is.na(results$mti_ecm_converged))),
    package_versions = as.list(versions), R_version = R.version.string,
    operating_system = Sys.info(), rng_kind = RNGkind(), base_seed = config$analysis$base_seed,
    workers = workers, threads_per_worker = 1L, started_utc = started,
    completed_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    elapsed_sec = proc.time()[["elapsed"]] - start_time,
    compiler = system2(file.path(R.home("bin"), "R"), c("CMD", "config", "CXX17"), stdout = TRUE),
    BLAS_LAPACK = extSoftVersion(), LAPACK_library = La_library(),
    rqrgibbs_library = find.package("rqrgibbs"),
    rqrgibbs_shared_library_sha256 = pta_file_sha256(file.path(
      find.package("rqrgibbs"), "libs", paste0("rqrgibbs", .Platform$dynlib.ext))),
    pinned_exdqlm_commit = "dffb71ee70b597d6a716ee74be1cbc99731cd453",
    exdqlm_loaded = "exdqlm" %in% loadedNamespaces(),
    artifacts = artifact_record,
    derived_tables_and_figures = list(status = "pending_reporting"))
  if(!is.null(context$adopted)) {
    manifest$computation <- "fixed_target_profile"
    manifest$scientific_method_id <- context$adopted$spec$primary_method
    manifest$policy_sha256 <- context$adopted$adoption$policy_sha256
    manifest$profile_convergence_count <- sum(results$mti_profile_converged,na.rm=TRUE)
    manifest$maximum_relative_objective_gap <- max(abs(results$mti_relative_objective_gap),na.rm=TRUE)
    manifest$ecm_convergence_counts <- NULL
  }
  pta_atomic_write_json(manifest, file.path(output_dir, "manifest.json"))
  output_dir
}

pma_verify_run <- function(run_dir, config) {
  manifest <- jsonlite::read_json(file.path(run_dir, "manifest.json"), simplifyVector = TRUE)
  for (i in seq_len(nrow(manifest$artifacts))) {
    pma_verify_hash(file.path(run_dir, manifest$artifacts$path[i]), manifest$artifacts$sha256[i])
  }
  pma_verify_hash(file.path(run_dir, "config_frozen.json"),
                  pta_file_sha256(attr(config, "config_path")))
  results <- pta_read_csv(file.path(run_dir, "results.csv"))
  pma_validate_results(results)
  if (manifest$observed_result_rows != nrow(results) ||
      manifest$expected_result_rows != nrow(results)) pta_stop("Incomplete application run.")
  manifest
}

pma_publish <- function(run_dir, config, repo_root) {
  manifest <- pma_verify_run(run_dir, config)
  if (manifest$mode != "confirmatory") pta_stop("Only a confirmatory run can supply the article.")
  destination <- pta_path(repo_root, config$paths$canonical_dir)
  if (dir.exists(destination)) pta_stop("Canonical destination already exists; preserve its history.")
  dir.create(destination, recursive = TRUE)
  files <- list.files(run_dir, full.names = TRUE)
  if (!all(file.copy(files, destination, overwrite = FALSE))) pta_stop("Could not copy canonical results.")
  destination
}
