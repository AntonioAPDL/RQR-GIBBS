# Additional computation is permitted only for a fixed target with adequate mixing.
mjp_precision_only <- function(record) {
  p <- record$parameters
  !isTRUE(record$adequate) && is.data.frame(p) && nrow(p) == 4L &&
    !anyNA(p$preferred_pass) && all(p$preferred_pass) &&
    !anyNA(p$mean_precision) && !all(p$mean_precision)
}

mjp_extend <- function(record, y, target, sd, cfg, policy, seed) {
  mwd_require(mjp_precision_only(record), "Only mean-precision failures with adequate mixing may be extended.")
  mwd_require(identical(record$seed, seed) && record$prior_sd == sd &&
    record$q == target$q && record$delta == target$geometry$delta &&
    all(record$parameters$q == target$q) && all(record$parameters$delta == target$geometry$delta),
    "The saved precision target or seed changed.")
  budgets <- cfg$precision_extension$retained_per_chain
  n <- vapply(record$fits, function(f) nrow(f$draws), 1L)
  mwd_require(length(n) == cfg$sampling$chains && length(unique(n)) == 1L &&
    length(budgets) > 0 && all(is.finite(budgets) & budgets == as.integer(budgets)) &&
    all(diff(budgets) > 0) && all(budgets > cfg$sampling$maximum_retained) &&
    max(budgets) <= 64000, "Invalid bounded precision-extension budgets.")
  budgets <- budgets[budgets > n[1]]
  mwd_require(length(budgets) > 0, "The fixed-target precision budget is exhausted.")
  ec <- unlist(target$selected[c("lower", "upper")], use.names = FALSE)
  initial <- mwd_initial(y, ec[1], ec[2], sd^2, 0)
  settings <- c(cfg$sampling, list(learning_rate = cfg$learning_rate, prior_variance = sd^2,
    initial_midpoint_offsets_sd = initial$offsets, initial_halfwidth_multipliers = initial$radii))
  task <- data.frame(case_id = "joint_prior", dataset_id = 0L, source = "simulation",
    response_id = "not_supplied_to_sampler", n = length(y), content = target$content %||% NA_real_,
    q = target$q, delta = target$geometry$delta, ecm_lower = ec[1], ecm_upper = ec[2])
  original <- record
  for (keep in budgets) {
    fits <- lapply(seq_len(settings$chains), function(chain) mgp_slice_fit(y,
      data.frame(chain = chain, seed = as.integer(seed + chain), q = target$q,
        delta = target$geometry$delta, ecm_lower = ec[1], ecm_upper = ec[2]), settings, n_keep = keep))
    mff_prefix_check(record$fits, fits)
    parameters <- mgp_summaries(fits, task, policy)
    adequate <- all(parameters$preferred_pass & parameters$mean_precision)
    record$history <- rbind(record$history, data.frame(retained = keep, adequate = adequate,
      max_rhat = max(parameters$rhat), min_bulk_ess = min(parameters$ess_bulk),
      min_tail_ess = min(parameters$ess_tail), max_mcse_mean = max(parameters$mcse_mean)))
    record$fits <- lapply(fits, function(f) f[setdiff(names(f), "target_loss_trace")])
    record$parameters <- parameters; record$adequate <- adequate
    if (adequate || !mjp_precision_only(record)) break
  }
  record$precision_extension <- list(original_record_sha256 = mwd_hash(original),
    observations_sha256 = mwd_hash(y), target_key = mjc_target_key(y, target, sd, cfg, policy),
    original_retained = n[1], retained_per_chain = keep, budgets = budgets,
    seed = seed, exact_prefix_preserved = TRUE, target_unchanged = TRUE,
    eligibility = "preferred_mixing_pass_mean_precision_fail")
  record
}

mjr_sample_checked <- function(y, target, sd, cfg, policy, seed) {
  record <- mjc_sample(y, target, sd, cfg, policy, seed)
  if (mjp_precision_only(record)) record <- mjp_extend(record, y, target, sd, cfg, policy, seed)
  record
}
