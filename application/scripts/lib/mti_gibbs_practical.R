# Intercept-only posterior diagnostics. The original Gaussian/GIG sampler is unchanged.
mgp_conditional <- function(y, other, q, delta, omega = 1, precision = 1e-4) {
  stopifnot(length(y) >= 2L, all(is.finite(y)), is.finite(other),
    q > 0, q < 1, is.finite(delta), omega > 0, precision > 0)
  ys <- sort(y); n <- length(ys)
  difference <- ys - other
  weights <- abs(difference)
  mass <- c(0, cumsum(weights)); moment <- c(0, cumsum(weights * ys))
  slope <- -q * sum(difference) + sum(pmin(difference, 0)) - n * q * delta
  after <- precision * ys + omega * (slope + mass[-1L])
  crossing <- which(after >= 0)[1L]
  if (is.na(crossing)) mode <- -omega * (slope + mass[n + 1L]) / precision else {
    before <- precision * ys[crossing] + omega * (slope + mass[crossing])
    mode <- if (before <= 0) ys[crossing] else
      -omega * (slope + mass[crossing]) / precision
  }
  at_mode <- findInterval(mode, ys) + 1L
  energy <- function(x) {
    k <- findInterval(x, ys) + 1L
    shift <- x - mode
    precision * shift^2 / 2 +
      (precision * mode + omega * (slope + mass[k])) * shift +
      omega * (mode * (mass[k] - mass[at_mode]) - (moment[k] - moment[at_mode]))
  }
  list(y = ys, mode = mode, precision = precision, omega = omega, slope = slope,
    mass = mass, energy = energy, knot_energy = energy(ys))
}

mgp_slice_interval <- function(conditional, height) {
  z <- conditional; y <- z$y; n <- length(y)
  stopifnot(length(height) == 1L, is.finite(height), height > 0)
  # Convexity makes the slice one interval. Locate its two quadratic segments.
  left <- which(y < z$mode & z$knot_energy > height)
  left_segment <- if (length(left)) tail(left, 1L) else 0L
  right <- which(y > z$mode & z$knot_energy > height)
  right_segment <- if (length(right)) right[1L] - 1L else n
  solve_side <- function(segment, side) {
    anchor <- z$mode
    if (side < 0 && segment < n && y[segment + 1L] < anchor) anchor <- y[segment + 1L]
    if (side > 0 && segment > 0 && y[segment] > anchor) anchor <- y[segment]
    difference <- height - z$energy(anchor)
    stopifnot(difference >= -1e-8 * max(1, height))
    difference <- max(0, difference)
    gradient <- z$precision * anchor + z$omega * (z$slope + z$mass[segment + 1L])
    radical <- sqrt(gradient^2 + 2 * z$precision * difference)
    denominator <- radical + side * gradient
    step <- if (difference == 0) 0 else 2 * difference / denominator
    answer <- anchor + side * step
    stopifnot(is.finite(answer), denominator >= 0)
    answer
  }
  bounds <- c(solve_side(left_segment, -1), solve_side(right_segment, 1))
  stopifnot(bounds[1] < bounds[2], bounds[1] <= z$mode, bounds[2] >= z$mode,
    max(abs(z$energy(bounds) - height)) <= 1e-6 * max(1, height))
  bounds
}

mgp_slice_update <- function(current, conditional) {
  energy <- conditional$energy(current)
  stopifnot(is.finite(energy), energy >= -1e-7 * max(1, abs(energy)))
  bounds <- mgp_slice_interval(conditional, max(0, energy) + rexp(1L))
  runif(1L, bounds[1], bounds[2])
}

mgp_objective <- function(y, a, b, q, delta, omega = 1, precision = 1e-4) {
  vapply(seq_along(a), function(i) {
    e <- (y - a[i]) * (y - b[i])
    omega * sum(e * (q - (e < 0)) - q * delta * (a[i] + b[i] - 2 * y)) +
      precision * (a[i]^2 + b[i]^2) / 2
  }, 0)
}

mgp_slice_fit <- function(y, task, cfg, n_burn = cfg$n_burn, n_keep = cfg$n_mcmc) {
  set.seed(task$seed)
  scale <- max(sd(y), 1e-4)
  midpoint <- (task$ecm_lower + task$ecm_upper) / 2 +
    cfg$initial_midpoint_offsets_sd[task$chain] * scale
  radius <- (task$ecm_upper - task$ecm_lower) / 2 *
    cfg$initial_halfwidth_multipliers[task$chain]
  a <- midpoint - radius; b <- midpoint + radius
  raw <- matrix(NA_real_, n_burn + n_keep, 2L)
  for (i in seq_len(nrow(raw))) {
    a <- mgp_slice_update(a, mgp_conditional(y, b, task$q, task$delta,
      cfg$learning_rate, 1 / cfg$prior_variance))
    b <- mgp_slice_update(b, mgp_conditional(y, a, task$q, task$delta,
      cfg$learning_rate, 1 / cfg$prior_variance))
    raw[i, ] <- c(a, b)
  }
  keep <- n_burn + seq_len(n_keep)
  objective <- mgp_objective(y, raw[, 1], raw[, 2], task$q, task$delta,
    cfg$learning_rate, 1 / cfg$prior_variance)
  list(draws = cbind(lower = pmin(raw[keep, 1], raw[keep, 2]),
    upper = pmax(raw[keep, 1], raw[keep, 2])),
    warmup_root1 = matrix(raw[seq_len(n_burn), 1], ncol = 1),
    warmup_root2 = matrix(raw[seq_len(n_burn), 2], ncol = 1),
    retained_objective = objective[keep],
    target_loss_trace = (objective - rowSums(raw^2) / (2 * cfg$prior_variance)) / cfg$learning_rate,
    last = list(beta_root1 = a, beta_root2 = b, rng_state = .Random.seed),
    sampler = "intercept_marginal_slice_within_gibbs")
}

mgp_array <- function(fits) {
  variables <- c("lower", "upper", "width", "midpoint")
  a <- array(NA_real_, c(nrow(fits[[1]]$draws), length(fits), 4L),
    dimnames = list(NULL, NULL, variables))
  for (i in seq_along(fits)) {
    x <- fits[[i]]$draws
    a[, i, ] <- cbind(x[, 1], x[, 2], x[, 2] - x[, 1], rowMeans(x))
  }
  a
}

mgp_summaries <- function(fits, task, policy) {
  a <- mgp_array(fits)
  d <- as.data.frame(posterior::summarise_draws(posterior::as_draws_array(a),
    "mean", "median", "sd", "rhat", "ess_bulk", "ess_tail", "mcse_mean"))
  intervals <- t(vapply(seq_len(dim(a)[3]), function(i)
    quantile(a[, , i], c(.025, .975), names = FALSE), numeric(2)))
  errors <- t(vapply(seq_len(dim(a)[3]), function(i)
    posterior::mcse_quantile(a[, , i], probs = c(.025, .5, .975), names = FALSE), numeric(3)))
  halves <- lapply(list(seq_len(floor(dim(a)[1] / 2)),
    seq.int(floor(dim(a)[1] / 2) + 1L, dim(a)[1])), function(k)
      apply(a[k, , , drop = FALSE], 3, mean))
  d$lower_025 <- intervals[, 1]; d$upper_975 <- intervals[, 2]
  d$mcse_q025 <- errors[, 1]; d$mcse_median <- errors[, 2]; d$mcse_q975 <- errors[, 3]
  d$mcse_sd_ratio <- d$mcse_mean / d$sd
  d$half_mean_change_sd <- abs(halves[[1]] - halves[[2]]) / d$sd
  # Response-unit tolerances follow a declared relative-width reporting resolution.
  width_scale <- max(d$median[d$variable == "width"], .Machine$double.eps)
  d$reporting_error_tolerance <- policy$relative_width_resolution * width_scale
  d$mean_precision <- d$mcse_mean <= d$reporting_error_tolerance
  d$median_precision <- d$mcse_median <= d$reporting_error_tolerance
  d$interval_precision <- pmax(d$mcse_q025, d$mcse_q975) <= d$reporting_error_tolerance
  finite <- with(d, is.finite(rhat) & is.finite(ess_bulk) & is.finite(ess_tail) &
    is.finite(mcse_sd_ratio))
  d$screen_pass <- finite & with(d, is.finite(rhat) & rhat <= policy$exploratory_rhat &
    ess_bulk >= policy$exploratory_ess & ess_tail >= policy$exploratory_ess &
    is.finite(mcse_sd_ratio) & mcse_sd_ratio <= policy$exploratory_mcse_ratio)
  d$preferred_pass <- finite & with(d, is.finite(rhat) & rhat <= policy$preferred_rhat &
    ess_bulk >= policy$preferred_ess & ess_tail >= policy$preferred_ess &
    is.finite(mcse_sd_ratio) & mcse_sd_ratio <= policy$preferred_mcse_ratio)
  d$status <- ifelse(!d$screen_pass, "unresolved",
    ifelse(d$preferred_pass & d$mean_precision & d$median_precision & d$interval_precision,
      "adequate_for_recorded_summaries", "provisional"))
  data.frame(case_id = task$case_id, dataset_id = task$dataset_id, source = task$source,
    response_id = task$response_id, n = task$n, content = task$content, q = task$q,
    delta = task$delta, d, ecm_value = c(task$ecm_lower, task$ecm_upper,
      task$ecm_upper - task$ecm_lower, (task$ecm_lower + task$ecm_upper) / 2))
}

mgp_target_class <- function(y, q, delta) {
  minimum <- function(tau) {
    r <- y - unname(quantile(y, tau, type = 1))
    sum(r * (tau - (r < 0)))
  }
  slopes <- c(minimum(q) + length(y) * q * delta,
    minimum(1 - q) - length(y) * q * delta)
  tolerance <- 1e-10 * max(1, sum(abs(y - mean(y))))
  if (min(slopes) < -tolerance) "outside" else
    if (min(abs(slopes)) < 1e-8 * max(1, abs(slopes))) "boundary" else "interior"
}

mgp_design <- function(full_jobs, parent_jobs, cfg) {
  full <- full_jobs[!duplicated(full_jobs$dataset_id), ]
  balanced <- full$replication <= cfg$replications_per_cell & full$source == "simulation" |
    full$replication == 1L & full$source == "application"
  selected <- full$dataset_id[balanced]
  diagnostic <- unique(parent_jobs$dataset_id)
  keep <- full_jobs$dataset_id %in% union(selected, diagnostic)
  jobs <- full_jobs[keep, ]
  jobs$seed <- jobs$seed + cfg$seed_offset
  jobs$diagnostic_role <- ifelse(jobs$dataset_id %in% selected, "balanced", "stress")
  stopifnot(!anyDuplicated(jobs$seed),
    !any(jobs$seed %in% union(parent_jobs$seed, full_jobs$seed)),
    !any(jobs$seed %in% full_jobs$data_seed))
  jobs
}

mgp_example_ids <- function(tasks, count) {
  features <- lapply(seq_len(nrow(tasks)), function(i) c(
    paste0("response:", tasks$response_id[i]), paste0("source:", tasks$source[i]),
    paste0("cell:", tasks$source[i], ":", tasks$n[i], ":", tasks$content[i]),
    paste0("class:", tasks$target_class[i])))
  chosen <- integer(); covered <- character()
  for (i in seq_len(min(count, nrow(tasks)))) {
    gain <- vapply(seq_len(nrow(tasks)), function(k)
      if (k %in% chosen) -1L else length(setdiff(features[[k]], covered)), 0L)
    next_id <- which.max(gain)
    chosen <- c(chosen, next_id); covered <- union(covered, features[[next_id]])
  }
  tasks$dataset_id[chosen]
}

mgp_assess <- function(jobs, loader, cfg, output, workers) {
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  groups <- split(jobs, jobs$dataset_id)
  rows <- parallel::mclapply(groups, function(s) {
    fits <- lapply(seq_len(nrow(s)), function(i) loader(s[i, , drop = FALSE]))
    if (any(vapply(fits, is.null, logical(1)))) return(NULL)
    mgp_summaries(fits, s[1L, ], cfg$policy)
  }, mc.cores = min(workers, 12L), mc.set.seed = FALSE)
  mpr_require(!any(vapply(rows, inherits, logical(1), "try-error")), "Practical assessment failed.")
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(data.frame())
  d <- do.call(rbind, rows)
  pma_write_csv(d, file.path(output, "parameter_summary.csv"))
  datasets <- lapply(split(d, d$dataset_id), function(z) {
    primary <- z[z$variable %in% c("lower", "upper", "width"), ]
    data.frame(z[1, c("dataset_id", "case_id", "source", "response_id", "n", "content", "q", "delta")],
      screen_pass = all(primary$screen_pass), preferred_pass = all(primary$preferred_pass),
      status = if (any(primary$status == "unresolved")) "unresolved" else
        if (all(primary$status == "adequate_for_recorded_summaries"))
          "adequate_for_recorded_summaries" else "provisional")
  })
  pma_write_csv(do.call(rbind, datasets), file.path(output, "dataset_summary.csv"))
  d
}

mgp_read <- function(directory, task, budget) {
  path <- mgc_chain_path(directory, task$job_id)
  if (!file.exists(paste0(path, ".json"))) return(NULL)
  mgc_receipt(path, task, budget, verify = TRUE)
  readRDS(path)
}

mgp_queue <- function(jobs, directory, cfg, base, context, workers, deadline,
                      identity, kind, parent, runtime) {
  dir.create(file.path(directory, "chains"), recursive = TRUE, showWarnings = FALSE)
  budget <- list(n_mcmc = if (kind == "extension") cfg$continuation_draws else base$n_mcmc)
  paths <- vapply(jobs$job_id, function(i) paste0(mgc_chain_path(directory, i), ".json"), "")
  for (i in which(file.exists(paths))) {
    receipt <- mgc_receipt(mgc_chain_path(directory, jobs$job_id[i]), jobs[i, , drop = FALSE], budget)
    mpr_require(identical(receipt$analysis_identity, mgc_hash(identity)), "Different study identity in chain receipt.")
  }
  pending <- which(!file.exists(paths))
  batches <- split(pending, ceiling(seq_along(pending) / workers))
  for (batch in batches) {
    if (as.numeric(Sys.time()) >= deadline) break
    results <- parallel::mclapply(batch, function(i) {
      task <- jobs[i, , drop = FALSE]; path <- mgc_chain_path(directory, task$job_id)
      started <- proc.time()[[3L]]
      tryCatch({
        y <- mgc_sample(task, context)
        if (kind == "extension") {
          old <- mgp_read(parent, task, list(n_mcmc = base$n_mcmc))
          stopifnot(!is.null(old), !is.null(old$last$rng_state))
          extended <- mgp_continue(base, task, old, y, runtime, cfg$continuation_draws)
          value <- list(draws = extended$draws, retained_objective = extended$objective,
            last = extended$last, sampler = "gaussian_gig_continuation",
            parent_sha256 = mgc_file_hash(mgc_chain_path(parent, task$job_id)),
            parent_draws = nrow(old$draws), iteration_offset = base$n_burn + base$n_mcmc)
        } else value <- mgp_slice_fit(y, task, base)
        value$task <- task; value$sample_sha256 <- mgc_hash(y)
        value$elapsed_seconds <- proc.time()[[3L]] - started
        value$repair_count <- 0L; value$passed <- TRUE
        value$objective_summary <- c(mean = mean(value$retained_objective),
          min = min(value$retained_objective), max = max(value$retained_objective))
        mgc_validate_record(value, task, budget)
        mgc_atomic_rds(value, path)
        pta_atomic_write_json(list(passed = TRUE, job_id = task$job_id, dataset_id = task$dataset_id,
          task_sha256 = mgc_hash(task), sample_sha256 = value$sample_sha256,
          sha256 = mgc_file_hash(path), n_mcmc = nrow(value$draws),
          elapsed_seconds = value$elapsed_seconds, sampler = value$sampler,
          analysis_identity = mgc_hash(identity), finished_utc = mgc_time()), paste0(path, ".json"))
        unlink(paste0(path, ".error.json")); TRUE
      }, error = function(e) {
        pta_atomic_write_json(list(error = conditionMessage(e), task = task,
          failed_utc = mgc_time()), paste0(path, ".error.json")); FALSE
      })
    }, mc.cores = workers, mc.preschedule = FALSE, mc.set.seed = FALSE)
    done <- sum(file.exists(paths))
    pta_atomic_write_json(list(completed_chains = done, expected_chains = nrow(jobs),
      remaining_chains = nrow(jobs) - done, checked_utc = mgc_time()), file.path(directory, "progress.json"))
    message(sprintf("[%s] %s: %d / %d chains recorded.", mgc_time(), kind, done, nrow(jobs)))
    if (!all(vapply(results, isTRUE, logical(1)))) {
      message("Some chains did not complete; their errors are retained, and other datasets remain included.")
    }
  }
  invisible(sum(file.exists(paths)))
}

mgp_continuation_check <- function(base, runtime, output) {
  y <- c(-2, -.3, .1, .7, 2)
  rows <- lapply(c(0, -.02), function(delta) {
    task <- data.frame(chain = 1L, seed = 19123456L, q = .95, delta = delta,
      ecm_lower = -2, ecm_upper = 2)
    first <- mgc_fit(base, task, y, list(n_burn = 10L, n_mcmc = 20L), runtime = runtime)
    continuation <- mgp_continue(base, task, list(last = first$last), y, runtime, 30L)
    whole <- mgc_fit(base, task, y, list(n_burn = 10L, n_mcmc = 50L), runtime = runtime)
    first_draws <- cbind(lower = pmin(as.numeric(first$samp.beta_root1), as.numeric(first$samp.beta_root2)),
      upper = pmax(as.numeric(first$samp.beta_root1), as.numeric(first$samp.beta_root2)))
    whole_draws <- cbind(lower = pmin(as.numeric(whole$samp.beta_root1), as.numeric(whole$samp.beta_root2)),
      upper = pmax(as.numeric(whole$samp.beta_root1), as.numeric(whole$samp.beta_root2)))
    exact <- identical(unname(rbind(first_draws, continuation$draws)), unname(whole_draws)) &&
      identical(continuation$last$rng_state, whole$last$rng_state)
    stopifnot(exact)
    data.frame(delta = delta, exact_draws_and_rng = exact)
  })
  pma_write_csv(do.call(rbind, rows), file.path(output, "continuation_equivalence.csv"))
}

mgp_report <- function(directory, example_ids, output) {
  jobs <- mgc_jobs(directory)
  budget <- mgc_json(file.path(directory, "budget.json"))
  summaries <- read.csv(file.path(directory, "parameter_summary.csv"))
  grDevices::pdf(output, width = 11, height = 8.5, onefile = TRUE)
  on.exit(grDevices::dev.off(), add = TRUE)
  for (id in example_ids) {
    s <- jobs[jobs$dataset_id == id, ]
    fits <- lapply(seq_len(nrow(s)), function(i) mgp_read(directory, s[i, ], budget))
    if (any(vapply(fits, is.null, logical(1)))) next
    a <- mgp_array(fits)
    par(mfrow = c(2, 2), mar = c(4, 4, 3, 1), oma = c(0, 0, 3, 0))
    for (variable in dimnames(a)[[3]]) {
      d <- summaries[summaries$dataset_id == id & summaries$variable == variable, ]
      matplot(a[, , variable], type = "l", lty = 1, col = c("#0072B2", "#D55E00", "#009E73", "#CC79A7"),
        xlab = "Retained iteration", ylab = variable, main = variable)
      abline(h = d$ecm_value, lty = 3, col = "gray40")
      mtext(sprintf("R-hat %.3f; bulk ESS %.0f; %s", d$rhat, d$ess_bulk, d$status),
        side = 3, line = .2, cex = .65)
    }
    mtext(sprintf("Marginal slice-within-Gibbs: %s; n=%d, c=%.2f, q=%.6f", s$case_id[1],
      s$n[1], s$content[1], s$q[1]), outer = TRUE, cex = .9)
  }
}

mgp_conditional_reference <- function(y, other, q, delta, cfg) {
  z <- mgp_conditional(y, other, q, delta, cfg$learning_rate, 1 / cfg$prior_variance)
  # Direct product loss, independent of the prefix-sum evaluation used by the sampler.
  anchor <- mgp_objective(y, z$mode, other, q, delta, cfg$learning_rate, 1 / cfg$prior_variance)
  density <- function(x) exp(-mgp_objective(y, x, rep(other, length(x)), q, delta,
    cfg$learning_rate, 1 / cfg$prior_variance) + anchor)
  points <- sort(unique(c(-Inf, y, z$mode, Inf)))
  area <- vapply(seq_len(length(points) - 1L), function(i)
    integrate(density, points[i], points[i + 1L], rel.tol = 1e-7,
      abs.tol = 1e-10, subdivisions = 500L)$value, 0)
  normalizer <- sum(area)
  stopifnot(is.finite(normalizer), normalizer > 0)
  cdf <- function(x) {
    k <- findInterval(x, points)
    sum(head(area, k - 1L)) / normalizer +
      integrate(density, points[k], x, rel.tol = 1e-7,
        abs.tol = 1e-10, subdivisions = 500L)$value / normalizer
  }
  list(mode = z$mode, normalizer = normalizer, cdf = cdf)
}

mgp_continue <- function(cfg, task, record, y, runtime, n_keep) {
  fit <- rqrgibbs::rqr_mcmc_fit(y, matrix(1, length(y), 1), task$q,
    learning_rate = cfg$learning_rate, mean_tilt = task$delta,
    beta_prior_obj = rqrgibbs::beta_prior("ridge", ridge = list(tau2 = cfg$prior_variance)),
    numerical_policy = "fail", mcmc_control = list(n_burn = 0L, n_mcmc = n_keep,
      thin = 1L, verbose = FALSE, store_latent_draws = FALSE,
      root_label_control = list(canonicalize_draws = FALSE)),
    init = record$last,
    provenance_control = list(repo_root = runtime$repo_root, expected_git_commit = runtime$commit,
      primary_runtime_attestation = runtime$attestation))
  stopifnot(isTRUE(fit$provenance$reproducibility_eligible),
    fit$model_spec$numerical_repair_count == 0L)
  a <- as.numeric(fit$samp.beta_root1); b <- as.numeric(fit$samp.beta_root2)
  list(draws = cbind(lower = pmin(a, b), upper = pmax(a, b)),
    objective = mgp_objective(y, a, b, task$q, task$delta,
      cfg$learning_rate, 1 / cfg$prior_variance), last = fit$last)
}
