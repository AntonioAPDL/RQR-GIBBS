#!/usr/bin/env Rscript
Sys.setenv(OMP_NUM_THREADS = 1, OPENBLAS_NUM_THREADS = 1, MKL_NUM_THREADS = 1)
for (file in c("mti_gibbs_practical", "mti_gibbs_full_report_review", "mti_gibbs_full_publication",
  "mti_profile_revision", "mti_gibbs_width_diagnosis"))
  source(file.path("application/scripts/lib", paste0(file, ".R")))
cfg <- mwd_json("application/config/mti_gibbs_width_diagnosis_20261113.json")
directory <- cfg$output_dir
manifest <- mwd_json(file.path(directory, "manifest.json"))
mgfp_check_hashes(manifest$identity$programs)
mgfp_check_hashes(manifest$identity$inputs)
output <- file.path(directory, "independent_references")
dir.create(output, showWarnings = FALSE)
context <- mwd_context(cfg, directory)
cases <- read.csv(file.path(directory, "selected_cases.csv"))
samples <- lapply(seq_len(nrow(cases)), function(i) mwd_sample(cases[i, ], context))
sensitivities <- read.csv(file.path(directory, "sensitivity_summary.csv"))
empirical_content <- do.call(rbind, lapply(seq_len(nrow(sensitivities)), function(i) {
  z <- sensitivities[i, ]; y <- samples[[match(z$case_id, cases$case_id)]]
  data.frame(case_id = z$case_id, variant = z$variant, source = z$source,
    empirical_training_content = mean(y >= z$lower & y <= z$upper),
    population_content = z$population_content)
}))
mwd_write(empirical_content, file.path(output, "interval_content_diagnostics.csv"))
coordinates <- do.call(rbind, lapply(seq_len(nrow(cases)), function(i) {
  t <- cases[i, ]; y <- samples[[i]]
  a <- t$ecm_lower; b <- t$ecm_upper
  lower <- mgp_conditional(y, b, t$q, t$delta)$mode
  upper <- mgp_conditional(y, a, t$q, t$delta)$mode
  loss <- function(a, b) mgp_objective(y, a, b, t$q, t$delta)
  initial <- loss(a, b)
  data.frame(case_id = t$case_id, q = t$q, delta = t$delta,
    target_class = mgp_target_class(y, t$q, t$delta), ecm_lower = a, ecm_upper = b,
    exact_conditional_lower_mode = lower, exact_conditional_upper_mode = upper,
    initial_objective = initial, lower_coordinate_gain = initial - loss(lower, b),
    upper_coordinate_gain = initial - loss(a, upper),
    lower_coordinate_width = abs(b - lower), upper_coordinate_width = abs(upper - a))
}))
mwd_require(all(coordinates$lower_coordinate_gain >= -1e-7) &&
  all(coordinates$upper_coordinate_gain >= -1e-7), "Conditional minimization increased the direct objective.")
mwd_write(coordinates, file.path(output, "ecm_coordinate_objective_checks.csv"))

# The reference uses direct product-loss integration, not the slice endpoint calculation.
ids <- c("sim_52_1", "sim_54_1", "sim_45_394")
reference <- parallel::mclapply(seq_along(ids), function(j) {
  i <- match(ids[j], cases$case_id); t <- cases[i, ]; y <- samples[[i]]; b <- t$ecm_upper
  reference <- mgp_conditional_reference(y, b, t$q, t$delta,
    list(learning_rate = 1, prior_variance = 10000))
  conditional <- mgp_conditional(y, b, t$q, t$delta)
  set.seed(cfg$seed_base + 1000000L + j)
  a <- conditional$mode; draws <- numeric(8500L)
  for (k in seq_along(draws)) draws[k] <- a <- mgp_slice_update(a, conditional)
  draws <- tail(draws, 8000L)
  p <- c(.025, .5, .975); x <- unname(quantile(draws, p))
  data.frame(case_id = t$case_id, probability = p, conditional_quantile = x,
    direct_loss_cdf = vapply(x, reference$cdf, 0), cdf_error = vapply(x, reference$cdf, 0) - p)
}, mc.cores = 3L, mc.set.seed = FALSE)
mwd_require(!any(vapply(reference, inherits, logical(1), "try-error")), "Independent conditional integration failed.")
reference <- do.call(rbind, reference)
mwd_write(reference, file.path(output, "direct_loss_conditional_cdfs.csv"))

pdf(file.path(output, "limiting_loss_and_prior.pdf"), width = 10, height = 5, useDingbats = FALSE)
par(mfrow = c(1, 2), mar = c(4.5, 4.8, 2.5, 1), oma = c(1, 0, 1, 0))
i <- match("sim_52_1", cases$case_id); y <- samples[[i]]; t <- cases[i, ]
b <- unname(quantile(y, t$q, type = 1)); delta <- unname(mwd_limits(y, t$q)[1])
x <- seq(-300, min(y) - 1e-8, length.out = 400)
loss <- vapply(x, function(a) mwd_loss(y, a, b, t$q, delta), 0)
plot(x, loss - loss[1], type = "l", ylim = c(-1e-8, 1e-8), xlab = "Lower endpoint",
  ylab = "Change in cumulative loss", main = "Limiting tilt: flat outward loss")
cols <- c("#0072B2", "#D55E00", "#009E73")
x <- seq(-300, min(y), length.out = 3000)
cdfs <- vapply(c(1, 10, 100), function(s) pnorm(x, 0, s) / pnorm(min(y), 0, s), x)
matplot(x, cdfs, type = "l", lty = 1, col = cols,
  xlab = "Lower endpoint", ylab = "Conditional distribution function", main = "Truncated Gaussian prior determines this tail")
legend("topleft", c("Prior SD 1", "Prior SD 10", "Prior SD 100"), col = cols, lty = 1, bty = "n")
mtext("Gamma diagnostic sample; upper endpoint fixed at the empirical q-quantile.", outer = TRUE, side = 1, cex = .8)
dev.off()
files <- c("ecm_coordinate_objective_checks.csv", "direct_loss_conditional_cdfs.csv",
  "interval_content_diagnostics.csv", "limiting_loss_and_prior.pdf")
mgfp_json_write(list(source_commit = mgfp_git("rev-parse", "HEAD"),
  script_sha256 = mwd_file_hash("application/scripts/117_check_mti_gibbs_width_references.R"),
  diagnostic_identity_sha256 = manifest$identity_sha256, artifacts = mgfp_hashes(file.path(output, files)),
  maximum_cdf_error = max(abs(reference$cdf_error)), new_tolerance_guarantee = FALSE),
  file.path(output, "provenance.json"))
cat("Independent conditional and ECM objective checks complete.\n")
