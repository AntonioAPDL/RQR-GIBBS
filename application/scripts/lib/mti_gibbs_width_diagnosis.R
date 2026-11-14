mwd_require <- function(ok, message) if (!isTRUE(ok)) stop(message, call. = FALSE)
mwd_hash <- function(x) digest::digest(x, algo = "sha256")
mwd_file_hash <- function(path) digest::digest(file = path, algo = "sha256", serialize = FALSE)
mwd_json <- function(path) jsonlite::read_json(path, simplifyVector = TRUE)
mwd_write <- function(x, path) data.table::fwrite(x, path, na = "NA")
mwd_programs <- c("application/scripts/116_diagnose_mti_gibbs_width.R",
  "application/scripts/lib/mti_gibbs_width_diagnosis.R",
  "application/scripts/lib/mti_gibbs_width_diagnosis_figures.R",
  "application/config/mti_gibbs_width_diagnosis_20261113.json",
  "application/scripts/lib/mti_gibbs_practical.R",
  "application/scripts/lib/mti_gibbs_full_report_review.R",
  "application/scripts/lib/mti_gibbs_full_publication.R",
  "application/scripts/lib/mti_profile_revision.R",
  "application/config/mti_ecm_article_20261111.json",
  "application/config/pharma_tolerance_application_mti_only_20261020.json",
  "application/config/mti_gibbs_full_reporting_20261113.json",
  "application/config/mti_gibbs_practical_20261112.json")

mwd_loss <- function(y, a, b, q, delta) {
  r <- (y - a) * (y - b)
  sum(r * (q - (r < 0)) - q * delta * (a + b - 2 * y))
}

mwd_limits <- function(y, q) {
  check_min <- function(tau) {
    b <- unname(stats::quantile(y, tau, type = 1))
    r <- y - b
    sum(r * (tau - (r < 0)))
  }
  c(lower = -check_min(q) / (length(y) * q), upper = check_min(1 - q) / (length(y) * q))
}

mwd_gradient <- function(y, a, b, q, delta, omega = 1, variance = 10000, center = 0) {
  z <- sort(y); prefix <- c(0, cumsum(z))
  left <- findInterval(a, z); right <- findInterval(b, z)
  mass <- length(y) * q - (right - left)
  moment <- q * sum(y) - (prefix[right + 1L] - prefix[left + 1L])
  cbind(lower = omega * (-moment + b * mass - length(y) * q * delta) + (a - center) / variance,
    upper = omega * (-moment + a * mass - length(y) * q * delta) + (b - center) / variance)
}

mwd_stein <- function(y, fits, q, delta, omega = 1, variance = 10000, center = 0) {
  scores <- lapply(fits, function(f) {
    a <- f$draws[, 1]; b <- f$draws[, 2]
    g <- mwd_gradient(y, a, b, q, delta, omega, variance, center)
    cbind(translation = rowSums(g), radial = (a - center) * g[, 1] + (b - center) * g[, 2] - 2)
  })
  do.call(rbind, lapply(colnames(scores[[1]]), function(name) {
    x <- do.call(cbind, lapply(scores, function(z) z[, name]))
    se <- posterior::mcse_mean(x)
    data.frame(identity = name, residual_mean = mean(x), mcse = se, standardized = mean(x) / se)
  }))
}

mwd_support <- function(dgp) {
  switch(dgp$family,
    standardized_gamma = c(-sqrt(dgp$shape), Inf),
    standardized_exponential = c(-1, Inf), centered_exponential = c(-1, Inf),
    standardized_lognormal = c(-1 / sqrt(exp(dgp$logsd^2) - 1), Inf),
    standardized_beta = {
      a <- dgp$shape1; b <- dgp$shape2; mean <- a / (a + b)
      sd <- sqrt(a * b / ((a + b)^2 * (a + b + 1)))
      c(-mean / sd, (1 - mean) / sd)
    }, c(-Inf, Inf))
}

mwd_initial <- function(y, lower, upper, variance, center) {
  sd <- sqrt(variance)
  starts <- rbind(c(lower, upper), c(min(lower, center - 3 * sd), upper),
    c(lower, max(upper, center + 3 * sd)),
    c(min(lower, center - 3 * sd), max(upper, center + 3 * sd)))
  list(offsets = (rowMeans(starts) - (lower + upper) / 2) / stats::sd(y),
    radii = (starts[, 2] - starts[, 1]) / (upper - lower), roots = starts)
}

mwd_context <- function(cfg, output) {
  article <- mwd_json(cfg$article_config)
  mwd_require(identical(mwd_file_hash(article$validation_config), article$validation_config_sha256),
    "The frozen simulation specification changed.")
  file <- file.path(output, "frozen_data_generators.R")
  status <- system2("git", c("--no-optional-locks", "show",
    shQuote(paste0(article$archived_commit, ":application/scripts/69_validate_rqr_bayes_uq.R"))), stdout = file)
  mwd_require(status == 0L, "Cannot read the frozen data generators.")
  generators <- mp_dgp_environment(file)
  validation <- jsonlite::read_json(article$validation_config, simplifyVector = FALSE)
  dgps <- setNames(validation$dgps, vapply(validation$dgps, "[[", "", "dgp_id"))
  laws <- lapply(dgps, generators$dgp_meta)
  app <- mwd_json(cfg$application_config)
  observed <- read.csv(file.path(app$paths$canonical_dir, "observed_batches.csv"))
  membership <- read.csv(file.path(app$paths$canonical_dir, "split_membership.csv.gz"))
  training <- membership[membership$training, ]
  list(laws = laws, dgps = dgps, observed = observed,
    training_ids = split(training$analysis_row_id, paste(training$split_scheme, training$split_id, sep = ":")),
    generator_sha256 = mwd_file_hash(file), archived_commit = article$archived_commit)
}

mwd_sample <- function(task, context) {
  if (task$source == "simulation") {
    set.seed(task$data_seed); y <- context$laws[[task$response_id]]$r(task$n)
  } else {
    ids <- context$training_ids[[paste(task$split_scheme, task$split_id, sep = ":")]]
    y <- as.numeric(context$observed[[task$response_id]][match(ids, context$observed$.analysis_row)])
  }
  mwd_require(length(y) == task$n && all(is.finite(y)) && identical(mwd_hash(y), task$dataset_sha256),
    paste("Observation identity differs:", task$case_id))
  y
}

mwd_cases <- function(d, jobs, cfg) {
  simulations <- d$source == "simulation"
  selected <- d$case_id[simulations & d$n == 1000 & d$content == .9 & d$replication == 1]
  selected <- unique(c(selected, mwd_json(cfg$publication_config)$example_case_ids,
    d$case_id[simulations & d$response_id == "normal" & d$replication == 1 &
      ((d$n == 100 & d$content == .9) | (d$n == 1000 & d$content == .99))]))
  tasks <- jobs[!duplicated(jobs$dataset_id), ]; tasks <- tasks[match(selected, tasks$case_id), ]
  mwd_require(nrow(tasks) == cfg$expected_cases && !anyNA(tasks$case_id), "Diagnostic selection differs.")
  tasks
}

mwd_variants <- function(task, y, cfg) {
  limits <- mwd_limits(y, task$q)
  priors <- data.frame(variant = paste0("prior_var_", cfg$prior_variances), variance = cfg$prior_variances,
    center = 0, omega = 1, delta = task$delta, contrast = "prior")
  other <- data.frame(variant = c("tilt_zero", "tilt_interior_midpoint", paste0("learning_rate_", cfg$learning_rate_contrasts),
      "sample_centered_sample_scale"),
    variance = c(10000, 10000, rep(10000, length(cfg$learning_rate_contrasts)), stats::var(y)),
    center = c(0, 0, rep(0, length(cfg$learning_rate_contrasts)), mean(y)),
    omega = c(1, 1, cfg$learning_rate_contrasts, 1),
    delta = c(0, mean(limits), rep(task$delta, length(cfg$learning_rate_contrasts) + 1L)),
    contrast = c("tilt", "tilt", rep("learning_rate", length(cfg$learning_rate_contrasts)), "prior_center_scale"))
  cbind(case_id = task$case_id, dataset_id = task$dataset_id, q = task$q, rbind(priors, other))
}

mwd_global <- function(d, intervals, context, output) {
  z <- data.table::copy(data.table::as.data.table(d)[source == "simulation"])
  e <- intervals[intervals$source == "simulation" & intervals$method == "mti_ecm_adaptive_cell", ]
  e <- e[match(z$dataset_id, e$dataset_id), ]; mwd_require(identical(z$dataset_id, e$dataset_id), "ECM pairs differ.")
  z$ecm_content <- e$observed_content
  bounds <- t(vapply(z$response_id, function(id) mwd_support(context$dgps[[id]]), numeric(2)))
  z$support_lower <- bounds[, 1]; z$support_upper <- bounds[, 2]
  z$outside_support_width <- pmax(z$support_lower - z$mean_lower, 0) + pmax(z$mean_upper - z$support_upper, 0)
  summaries <- z[, .(datasets = .N, selected_q_mean = mean(q), population_content_mean = mean(mean_content),
    population_content_median = median(mean_content), population_content_q025 = quantile(mean_content, .025),
    population_content_q975 = quantile(mean_content, .975), fraction_content_ge_999 = mean(mean_content >= .999),
    attainment = mean(mean_attained), ecm_population_content_mean = mean(ecm_content),
    gibbs_width_mean = mean(mean_width), ecm_width_mean = mean(ecm_upper - ecm_lower),
    outside_support_width_mean = mean(outside_support_width), lower_mean = mean(mean_lower), upper_mean = mean(mean_upper),
    interior = sum(target_class == "interior"), limiting = sum(target_class == "boundary"), outside = sum(target_class == "outside")),
    by = .(response_id, n, content)]
  mwd_write(summaries, file.path(output, "all_simulation_cells.csv"))
  geometry <- z[, .(datasets = .N, median_width = median(mean_width),
    prior_sd_over_sample_sd_median = median(100 / sample_sd)), by = .(response_id, target_class)]
  mwd_write(geometry, file.path(output, "geometry_by_family.csv"))
  content <- z[, .(datasets = .N, mean_population_content = mean(mean_content),
    fraction_content_ge_999 = mean(mean_content >= .999), attainment = mean(mean_attained)), by = content]
  mwd_write(content, file.path(output, "content_vs_attainment.csv"))
  summaries
}

mwd_chain <- function(job, y, cfg, path) {
  task <- job$task; v <- job$variant
  initial <- mwd_initial(y, task$ecm_lower, task$ecm_upper, v$variance, v$center)
  base <- list(n_burn = cfg$n_burn, n_mcmc = cfg$n_mcmc, learning_rate = v$omega,
    prior_variance = v$variance, initial_midpoint_offsets_sd = initial$offsets,
    initial_halfwidth_multipliers = initial$radii)
  task$q <- v$q; task$delta <- v$delta
  task$ecm_lower <- task$ecm_lower - v$center; task$ecm_upper <- task$ecm_upper - v$center
  fit <- mgp_slice_fit(y - v$center, task, base)
  fit$draws <- fit$draws + v$center
  fit$warmup_root1 <- fit$warmup_root1 + v$center; fit$warmup_root2 <- fit$warmup_root2 + v$center
  fit$last$beta_root1 <- fit$last$beta_root1 + v$center
  fit$last$beta_root2 <- fit$last$beta_root2 + v$center
  check <- unique(as.integer(round(seq.int(1L, nrow(fit$draws), length.out = 31L))))
  expected <- vapply(check, function(i) v$omega * mwd_loss(y, fit$draws[i, 1], fit$draws[i, 2], v$q, v$delta) +
    sum((fit$draws[i, ] - v$center)^2) / (2 * v$variance), 0)
  mwd_require(max(abs(expected - fit$retained_objective[check])) <= 1e-7 * max(1, abs(expected)),
    "The diagnostic chain targets a different objective.")
  fit$diagnostic_job <- job; fit$sample_sha256 <- mwd_hash(y)
  saveRDS(fit, path)
  jsonlite::write_json(list(job_sha256 = mwd_hash(job), sample_sha256 = mwd_hash(y),
    sha256 = mwd_file_hash(path)), paste0(path, ".json"), auto_unbox = TRUE)
  TRUE
}

mwd_read <- function(path, job, y) {
  receipt <- mwd_json(paste0(path, ".json"))
  mwd_require(identical(receipt$job_sha256, mwd_hash(job)) &&
    identical(receipt$sample_sha256, mwd_hash(y)) && identical(receipt$sha256, mwd_file_hash(path)),
    "Diagnostic chain receipt differs.")
  readRDS(path)
}

mwd_summarize <- function(task, variant, fits, context, policy) {
  task$q <- variant$q; task$delta <- variant$delta
  p <- mgp_summaries(fits, task, policy)
  means <- setNames(p$mean, p$variable)
  probability <- if (task$source == "simulation") context$laws[[task$response_id]]$p else
    function(x) stats::ecdf(context$observed[[task$response_id]])(x)
  actual_content <- probability(means["upper"]) - probability(means["lower"])
  cbind(task[, c("case_id", "source", "response_id", "n", "content")], variant[, c("variant", "contrast", "q", "delta", "variance", "center", "omega")],
    lower = means["lower"], upper = means["upper"], width = means["width"],
    population_content = if (task$source == "simulation") actual_content else NA_real_,
    width_q025 = p$lower_025[p$variable == "width"], width_q975 = p$upper_975[p$variable == "width"],
    maximum_rhat = max(p$rhat), minimum_bulk_ess = min(p$ess_bulk), minimum_tail_ess = min(p$ess_tail),
    preferred = all(p$preferred_pass), maximum_mcse_sd = max(p$mcse_sd_ratio),
    width_mcse = p$mcse_mean[p$variable == "width"])
}

mwd_boundary_probe <- function(task, y, cfg) {
  q <- task$q; delta <- unname(mwd_limits(y, q)[1]); other <- unname(quantile(y, q, type = 1))
  sigma <- 100; bound <- min(y)
  x <- bound - sigma * c(.1, .5, 1, 2, 3)
  loss <- vapply(x, function(a) mwd_loss(y, a, other, q, delta), 0)
  conditional <- mgp_conditional(y, other, q, delta, precision = 1 / sigma^2)
  set.seed(cfg$seed_base - task$dataset_id)
  a <- conditional$mode; draws <- numeric(cfg$conditional_draws)
  for (i in seq_along(draws)) draws[i] <- a <- mgp_slice_update(a, conditional)
  keep <- draws < bound
  mwd_require(sum(keep) > 100, "Too few conditional draws below the observations.")
  expected <- truncnorm::etruncnorm(a = -Inf, b = bound, mean = 0, sd = sigma)
  centered <- (draws - expected) * keep
  error <- posterior::mcse_mean(matrix(centered, ncol = 1)) / mean(keep)
  z <- quantile(draws[keep], c(.025, .5, .975))
  cdf <- stats::pnorm(z / sigma) / stats::pnorm(bound / sigma)
  data.frame(case_id = task$case_id, q = q, original_delta = task$delta, boundary_delta = delta,
    bound = bound, loss_range_on_ray = diff(range(loss)), expected_tail_mean = expected,
    observed_tail_mean = mean(draws[keep]), tail_mean_mcse = error,
    standardized_mean_difference = (mean(draws[keep]) - expected) / error,
    tail_draws = sum(keep), maximum_cdf_error = max(abs(cdf - c(.025, .5, .975))))
}

mwd_old_agreement <- function(task, original, fresh) {
  summarize <- function(fits) {
    a <- mgp_array(fits)
    lapply(seq_len(dim(a)[3]), function(i) c(mean = mean(a[, , i]), mcse = posterior::mcse_mean(a[, , i])))
  }
  old <- summarize(original); new <- summarize(fresh)
  do.call(rbind, lapply(seq_along(old), function(i) {
    se <- sqrt(old[[i]][2]^2 + new[[i]][2]^2)
    data.frame(case_id = task$case_id, variable = dimnames(mgp_array(original))[[3]][i],
      original_mean = old[[i]][1], fresh_mean = new[[i]][1], difference = new[[i]][1] - old[[i]][1],
      combined_mcse = se, standardized = (new[[i]][1] - old[[i]][1]) / se)
  }))
}
