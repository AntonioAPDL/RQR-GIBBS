#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_path <- sub(file_arg, "", args[startsWith(args, file_arg)][1L])
if (!length(script_path) || is.na(script_path)) {
  script_path <- "application/scripts/67_validate_rqr_ecm_fixed_target.R"
}
script_path <- normalizePath(script_path, winslash = "/", mustWork = TRUE)
repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."),
                           winslash = "/", mustWork = TRUE)
setwd(repo_root)

cli <- commandArgs(trailingOnly = TRUE)
arg_value <- function(prefix, default = NULL) {
  hit <- cli[startsWith(cli, prefix)]
  if (length(hit)) sub(prefix, "", hit[[1L]], fixed = TRUE) else default
}
stopf <- function(...) stop(paste0(...), call. = FALSE)

for (package in c("rqrgibbs", "jsonlite")) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stopf("Required package is not installed: ", package)
  }
}
library(rqrgibbs)
`%||%` <- function(a, b) if (is.null(a)) b else a

mode <- tolower(arg_value("--mode=", "smoke"))
config_path <- normalizePath(
  arg_value("--config=", file.path("application", "config",
                                   "rqr_ecm_validation_v1.json")),
  winslash = "/", mustWork = TRUE
)
config <- jsonlite::read_json(config_path, simplifyVector = FALSE)
if (!mode %in% names(config$modes)) stopf("Unsupported ECM validation mode: ", mode)
if (!isTRUE(config$execution[[paste0(mode, "_authorized")]])) {
  stopf("ECM validation mode is not authorized: ", mode)
}
output_dir <- normalizePath(
  arg_value("--output-dir=", file.path(
    "application", "outputs", "rqr_ecm_validation_v1",
    paste0(mode, "_", format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"))
  )),
  winslash = "/", mustWork = FALSE
)
if (file.exists(output_dir) || dir.exists(output_dir)) {
  stopf("The output directory must be fresh: ", output_dir)
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

git_commit <- tryCatch(
  system2("git", c("rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE)[[1L]],
  error = function(e) NA_character_
)
mode_cfg <- config$modes[[mode]]
dgp_by_id <- setNames(config$dgps, vapply(config$dgps, `[[`, character(1L), "dgp_id"))

simulate_y <- function(dgp, n, seed) {
  set.seed(seed)
  if (identical(dgp$family, "normal")) return(stats::rnorm(n))
  if (identical(dgp$family, "standardized_lognormal")) {
    z <- stats::rlnorm(n, meanlog = 0, sdlog = dgp$logsd %||% 0.75)
    return(as.numeric(scale(z)))
  }
  if (identical(dgp$family, "standardized_normal_mixture")) {
    weights <- as.numeric(dgp$weights)
    means <- as.numeric(dgp$means)
    sds <- as.numeric(dgp$sds)
    comp <- sample.int(length(weights), n, replace = TRUE, prob = weights)
    z <- stats::rnorm(n, mean = means[comp], sd = sds[comp])
    return(as.numeric(scale(z)))
  }
  stopf("Unsupported DGP family: ", dgp$family)
}

direct_objective <- function(y, q, prior, start) {
  X <- matrix(1, length(y), 1L, dimnames = list(NULL, "(Intercept)"))
  constants <- rqr_constants(q, 1)
  prior_prec <- rqrgibbs:::.rqr_prior_precision(prior, list(), 1L)
  fn <- function(par) {
    rqrgibbs:::.rqr_ecm_objective(
      y, X, par[[1L]], par[[2L]], constants,
      mean_tilt_observed = rep(0, length(y)),
      prior_prec1 = prior_prec,
      prior_prec2 = prior_prec
    )$total
  }
  opt <- stats::optim(
    par = as.numeric(start),
    fn = fn,
    method = config$direct_optim$method %||% "Nelder-Mead",
    control = list(
      maxit = as.integer(config$direct_optim$maxit %||% 1000),
      reltol = as.numeric(config$direct_optim$reltol %||% 1e-10)
    )
  )
  list(value = opt$value, convergence = opt$convergence)
}

rows <- list()
counter <- 0L
base_seed <- as.integer(config$base_seed %||% 812500)
for (dgp_id in as.character(mode_cfg$dgp_ids)) {
  dgp <- dgp_by_id[[dgp_id]]
  for (n in as.integer(mode_cfg$sample_sizes)) {
    for (q in as.numeric(mode_cfg$target_contents)) {
      for (rep in seq_len(as.integer(mode_cfg$replications))) {
        counter <- counter + 1L
        seed <- base_seed + counter
        y <- simulate_y(dgp, n, seed)
        X <- matrix(1, length(y), 1L, dimnames = list(NULL, "(Intercept)"))
        prior <- beta_prior("ridge", ridge = list(tau2 = 1e4))
        timing <- system.time({
          ecm <- rqr_ecm_fit(
            y, X, q,
            beta_prior_obj = prior,
            ecm_control = as.list(config$ecm_control)
          )
        })
        direct <- direct_objective(y, q, prior, c(ecm$beta_root1, ecm$beta_root2))
        vb_objective <- NA_real_
        if (isTRUE(mode_cfg$include_vb)) {
          vb <- rqr_vb_fit(
            y, X, q,
            beta_prior_obj = prior,
            vb_control = list(max_iter = 30, n_draws = 20, seed = seed)
          )
          vb_objective <- rqrgibbs:::.rqr_ecm_objective(
            y, X,
            vb$qbeta_root1$mean,
            vb$qbeta_root2$mean,
            rqr_constants(q, 1),
            mean_tilt_observed = rep(0, length(y)),
            prior_prec1 = rqrgibbs:::.rqr_prior_precision(prior, list(), 1L),
            prior_prec2 = rqrgibbs:::.rqr_prior_precision(prior, list(), 1L)
          )$total
        }
        mcmc_objective <- NA_real_
        if (isTRUE(mode_cfg$include_mcmc_reference)) {
          mcmc <- rqr_mcmc_fit(
            y, X, q,
            beta_prior_obj = prior,
            mcmc_control = utils::modifyList(
              as.list(config$mcmc_control), list(seed = seed)
            )
          )
          mcmc_draws <- rqr_posterior_draws(mcmc)
          b1 <- colMeans(as.matrix(mcmc_draws$beta_root1))
          b2 <- colMeans(as.matrix(mcmc_draws$beta_root2))
          mcmc_objective <- rqrgibbs:::.rqr_ecm_objective(
            y, X, b1, b2, rqr_constants(q, 1),
            mean_tilt_observed = rep(0, length(y)),
            prior_prec1 = rqrgibbs:::.rqr_prior_precision(prior, list(), 1L),
            prior_prec2 = rqrgibbs:::.rqr_prior_precision(prior, list(), 1L)
          )$total
        }
        rows[[length(rows) + 1L]] <- data.frame(
          mode = mode,
          dgp_id = dgp_id,
          n = n,
          target_content = q,
          replication = rep,
          seed = seed,
          ecm_objective = ecm$objective,
          direct_objective = direct$value,
          direct_gap = ecm$objective - direct$value,
          direct_convergence = direct$convergence,
          vb_objective = vb_objective,
          mcmc_posterior_mean_objective = mcmc_objective,
          ecm_iterations = ecm$iterations,
          ecm_converged = ecm$converged,
          ecm_convergence_code = ecm$convergence_code,
          ecm_backtracking_count = ecm$backtracking_count,
          ecm_selected_start = ecm$selected_start$label,
          ecm_width_mean = ecm$ordered_endpoint_summary$width_mean,
          elapsed_sec = unname(timing[["elapsed"]])
        )
      }
    }
  }
}
results <- do.call(rbind, rows)
write.csv(results, file.path(output_dir, "ecm_validation_results.csv"),
          row.names = FALSE)
summary <- aggregate(
  cbind(direct_gap, ecm_width_mean, elapsed_sec) ~ mode + dgp_id + n + target_content,
  data = results,
  FUN = function(x) c(mean = mean(x), max = max(x))
)
write.csv(summary, file.path(output_dir, "ecm_validation_summary.csv"),
          row.names = FALSE)
manifest <- list(
  schema_version = config$schema_version,
  study_id = config$study_id,
  mode = mode,
  git_commit = git_commit,
  config_path = config_path,
  output_dir = output_dir,
  rows = nrow(results),
  generated_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  response_likelihood = FALSE,
  interpretation = config$interpretation
)
jsonlite::write_json(
  manifest, file.path(output_dir, "manifest.json"),
  pretty = TRUE, auto_unbox = TRUE
)
writeLines(c(
  "# MT-RQR-ECM Validation Smoke",
  "",
  paste("Mode:", mode),
  paste("Git commit:", git_commit),
  paste("Rows:", nrow(results)),
  "",
  "This run checks deterministic fixed-target ECM computation. It is not a tolerance-confidence campaign."
), file.path(output_dir, "README.md"))
cat("RQR ECM validation", mode, "completed:", output_dir, "\n")
