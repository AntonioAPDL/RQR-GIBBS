mff_precision_only <- function(result) {
  isTRUE(result$returned) && all(result$rows$ecm_coordinate_ok) &&
    all(result$rows$gibbs_preferred) && !all(result$rows$gibbs_mean_precision)
}

mff_prefix_check <- function(original, extended) {
  mwd_require(length(original) == length(extended), "The chain count changed during precision refinement.")
  for (i in seq_along(original)) {
    n <- nrow(original[[i]]$draws)
    mwd_require(nrow(extended[[i]]$draws) > n &&
      identical(original[[i]]$draws, extended[[i]]$draws[seq_len(n), , drop = FALSE]) &&
      identical(original[[i]]$warmup_root1, extended[[i]]$warmup_root1) &&
      identical(original[[i]]$warmup_root2, extended[[i]]$warmup_root2) &&
      identical(original[[i]]$retained_objective, extended[[i]]$retained_objective[seq_len(n)]),
      "A saved sampling prefix changed; preserve the original calculation.")
  }
  invisible(TRUE)
}

mff_extend <- function(original, prior, context, budgets, policy) {
  mwd_require(mff_precision_only(original) && original$task$stage == "application",
    "Only application mean-precision flags with adequate mixing may be extended.")
  mfc_result_check(original, original$task, prior)
  task <- original$task; ids <- context$training_ids[[task$training_key]]
  y <- context$observed[[task$response_id]][match(ids, context$observed$.analysis_row)]
  mwd_require(length(y) == task$n && identical(mwd_hash(y), original$sample_sha256),
    "The application training observations changed.")
  specification <- mfp_spec(y, task$kappa, prior$application_reference)
  raw_ecm <- unlist(original$rows[original$rows$method == "ecm", c("lower", "upper")], use.names = FALSE)
  ecm <- (raw_ecm - specification$center) / specification$scale
  initial <- mwd_initial(specification$z, ecm[1], ecm[2], task$kappa^2, 0)
  sampling <- c(prior$sampling, list(learning_rate = 1, prior_variance = task$kappa^2,
    initial_midpoint_offsets_sd = initial$offsets, initial_halfwidth_multipliers = initial$radii))
  mwd_require(length(original$fits) == prior$sampling$chains && length(budgets) > 0 &&
    all(is.finite(budgets) & budgets == as.integer(budgets)) && all(diff(budgets) > 0) &&
    all(budgets > nrow(original$fits[[1]]$draws)), "Invalid fixed-target precision budgets.")
  summary_task <- data.frame(case_id = "fixed_prior", dataset_id = 0L, source = "fixed_prior",
    response_id = "not_supplied_to_fitter", n = length(y), content = task$content,
    q = original$q, delta = original$delta_reference, ecm_lower = ecm[1], ecm_upper = ecm[2])
  history <- list()
  for (keep in budgets) {
    fits <- lapply(seq_len(prior$sampling$chains), function(chain) {
      t <- data.frame(chain = chain, seed = as.integer(1670000000 + 100L * task$job_id + chain),
        q = original$q, delta = original$delta_reference, ecm_lower = ecm[1], ecm_upper = ecm[2])
      mgp_slice_fit(specification$z, t, sampling, n_keep = keep)
    })
    mff_prefix_check(original$fits, fits)
    parameters <- mgp_summaries(fits, summary_task, policy)
    adequate <- all(parameters$preferred_pass & parameters$mean_precision)
    history[[length(history) + 1L]] <- data.frame(retained = keep, adequate = adequate,
      max_rhat = max(parameters$rhat), min_bulk_ess = min(parameters$ess_bulk),
      min_tail_ess = min(parameters$ess_tail), max_mean_mcse = max(parameters$mcse_mean))
    if (adequate) break
  }
  result <- original
  result$fits <- fits; result$parameters_reference_units <- parameters
  result$sampling_history <- rbind(original$sampling_history, do.call(rbind, history))
  means <- setNames(parameters$mean, parameters$variable)
  endpoints <- means[c("lower", "upper")] * specification$scale + specification$center
  g <- which(result$rows$method == "gibbs")
  result$rows$lower[g] <- endpoints[1]; result$rows$upper[g] <- endpoints[2]
  result$rows$width[g] <- diff(endpoints)
  # Held-out values enter only after the same-target sampling and prefix checks.
  heldout <- context$observed[[task$response_id]][!context$observed$.analysis_row %in% ids]
  result$rows$heldout_empirical_content[g] <- mean(heldout >= endpoints[1] & heldout <= endpoints[2])
  dp <- rqrgibbs::rqr_dp_fit(y, concentration = original$dp_prior$concentration,
    base_measure = rqrgibbs::rqr_dp_base_normal(original$dp_prior$raw_base$mean, original$dp_prior$raw_base$sd))
  probability <- rqrgibbs::rqr_dp_content_probability(dp, endpoints[1], endpoints[2], task$content)
  result$rows$dp_content_probability[g] <- probability$posterior_content_probability
  result$rows$gibbs_preferred <- all(parameters$preferred_pass)
  result$rows$gibbs_mean_precision <- all(parameters$mean_precision)
  result$rows$gibbs_adequate <- adequate
  result$rows$max_rhat <- max(parameters$rhat)
  result$rows$min_bulk_ess <- min(parameters$ess_bulk)
  result$rows$min_tail_ess <- min(parameters$ess_tail)
  result$rows$retained_per_chain <- keep
  result$precision_refinement <- list(original_retained = nrow(original$fits[[1]]$draws),
    refined_retained = keep, original_prefix_preserved = TRUE, target_unchanged = TRUE,
    prior_uses_analysis_data = FALSE)
  immutable <- c("task", "prior", "sample_sha256", "candidates", "selected", "selected_ecm_trace",
    "selected_knot_adjustments", "q", "delta_reference", "dp_prior", "comparators")
  mwd_require(identical(mwd_hash(original[immutable]), mwd_hash(result[immutable])) &&
    identical(original$rows[original$rows$method == "ecm", c("lower", "upper", "width")],
      result$rows[result$rows$method == "ecm", c("lower", "upper", "width")]),
    "The numerical extension changed a target, prior, comparator, or ECM endpoint.")
  result
}

mff_merge <- function(original, refinements) {
  d <- data.table::as.data.table(original)
  ids <- vapply(refinements, function(x) x$task$job_id, 0L)
  mwd_require(!anyDuplicated(ids), "Duplicate precision refinements.")
  for (r in refinements) {
    before <- d[job_id == r$task$job_id]
    mwd_require(nrow(before) == 2 && all(before$returned & before$ecm_coordinate_ok & before$gibbs_preferred) &&
      !all(before$gibbs_mean_precision) && isTRUE(r$precision_refinement$original_prefix_preserved),
      "An adequate result or incompatible target cannot be replaced by precision refinement.")
    fields <- c("lower", "upper", "width", "selected_q", "selected_delta", "prior_mean", "prior_sd")
    a <- as.matrix(before[method == "ecm", ..fields])
    b <- as.matrix(r$rows[r$rows$method == "ecm", fields])
    # Decimal CSV summaries need not preserve the final binary digit of an RDS value.
    mwd_require(all(is.finite(a) & is.finite(b)) &&
      all(abs(a - b) <= 32 * .Machine$double.eps * pmax(1, abs(a), abs(b))),
      "The ECM interval changed in a precision refinement.")
  }
  data.table::rbindlist(c(list(d[!job_id %in% ids]), lapply(refinements, `[[`, "rows")), fill = TRUE)
}

mff_ready <- function(directory) {
  exit <- file.path(directory, "controller_exit.json")
  if (!file.exists(exit)) return(FALSE)
  mwd_require(mwd_json(exit)$exit_code == 0, "The numerical continuation stopped unsuccessfully.")
  phase <- mwd_json(file.path(directory, "status.json"))$phase
  mwd_require(phase %in% c("complete_requires_scientific_review", "completed_attempts_require_numerical_review"),
    "The continuation did not finish its planned attempts.")
  for (stage in c("application", "diagnostics")) {
    status <- mwd_json(file.path(directory, stage, "status.json"))
    mwd_require(status$completed == status$expected, paste("Incomplete", stage, "attempts."))
  }
  TRUE
}
