#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_path <- sub(file_arg, "", args[startsWith(args, file_arg)][1L])
if (!length(script_path) || is.na(script_path)) {
  script_path <- "application/scripts/33_run_oracle_tilt_forensics.R"
}
script_dir <- dirname(normalizePath(script_path, mustWork = TRUE))
source(file.path(script_dir, "32_oracle_tilt_illustration_utils.R"))
source(file.path(script_dir, "33_oracle_tilt_forensic_utils.R"))

trailing <- commandArgs(trailingOnly = TRUE)
arg_value <- function(prefix, default = NULL) {
  hit <- trailing[startsWith(trailing, prefix)]
  if (length(hit)) sub(prefix, "", hit[1L], fixed = TRUE) else default
}

mode <- tolower(arg_value("--mode=", "preflight"))
if (!mode %in% c("preflight", "execute")) {
  oti_stop("--mode must be preflight or execute.")
}
config_path <- arg_value(
  "--config=",
  "application/config/oracle_tilt_forensics_20260730.json"
)
output_dir_arg <- arg_value("--output-dir=", NULL)
repo_root <- normalizePath(file.path(script_dir, "..", ".."), mustWork = TRUE)
setwd(repo_root)

if (!requireNamespace("rqrgibbs", quietly = TRUE)) {
  oti_stop("The rqrgibbs package must be installed before running forensics.")
}
config <- oti_read_json(config_path)
if (!identical(
    as.character(config$schema_version),
    "rqrgibbs_oracle_tilt_forensics_config/1.1.0"
  )) {
  oti_stop("The forensic configuration schema is unsupported.")
}
coverage_level <- oti_scalar(
  config$coverage_level, "coverage_level", 1e-8, 1 - 1e-8
)
learning_rate <- oti_scalar(
  config$learning_rate %||% 1, "learning_rate", .Machine$double.eps
)
run_id <- arg_value(
  "--run-id=",
  paste0("oracle-tilt-forensics-", format(Sys.time(), "%Y%m%d-%H%M%S"))
)
output_root <- oti_ensure_dir(
  output_dir_arg %||%
    file.path("application", "outputs", "oracle_tilt_forensics", run_id)
)

if (identical(mode, "execute")) {
  authorized <- isTRUE(config$execution_authorized) &&
    identical(Sys.getenv("RQR_ORACLE_TILT_FORENSICS_CONFIRM"), "YES")
  if (!authorized) {
    oti_stop(
      paste(
        "Execution is fail-closed. Use an ignored local config with",
        "execution_authorized=true and set",
        "RQR_ORACLE_TILT_FORENSICS_CONFIRM=YES."
      )
    )
  }
}

paths <- character(0)
paths <- c(paths, oti_write_json(config, file.path(output_root, "config.json")))
paths <- c(paths, oti_write_json(
  list(
    schema_version = otf_schema(),
    mode = mode,
    run_id = run_id,
    started_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    repository = oti_git_state(repo_root),
    config_path = normalizePath(config_path, mustWork = TRUE),
    execution_confirmed = identical(mode, "execute"),
    interpretation = paste(
      "Bounded interval-root generalized-posterior forensics;",
      "not a response-predictive or coverage simulation."
    )
  ),
  file.path(output_root, "source_state.json")
))
paths <- c(paths, oti_write_json(
  oti_runtime_state(
    repo_root = repo_root,
    config_path = config_path,
    script_path = normalizePath(script_path, mustWork = TRUE),
    args = trailing,
    run_control = list(schema_version = otf_schema(), mode = mode),
    extra_files = c(
      file.path(script_dir, "33_oracle_tilt_forensic_utils.R"),
      file.path(
        repo_root, "application", "tests", "testthat",
        "test-rqr-oracle-tilt-forensics.R"
      ),
      file.path(
        repo_root, "docs", "implementation_notes",
        "oracle_tilt_high_content_forensic_protocol_20260730.md"
      )
    )
  ),
  file.path(output_root, "runtime_state.json")
))

law <- oti_law_from_config(config)
oracle <- oti_oracle_targets(law, coverage_level)
geometry <- otf_tilt_geometry(
  law, coverage_level, oracle,
  caution_fraction =
    config$population_boundary_caution_fraction %||% 0.02
)
paths <- c(paths, oti_write_csv(
  oracle, file.path(output_root, "oracle_targets.csv")
))
paths <- c(paths, oti_write_csv(
  geometry, file.path(output_root, "population_tilt_geometry.csv")
))

dlm_dgp <- oti_dlm_dgp(config, law)
dlm_targets <- oti_targets_by_index(
  dlm_dgp$mean_truth, dlm_dgp$scale_truth, oracle, dlm_dgp$observed
)
prior_shift <- otf_prior_shift_summary(
  dlm_dgp, dlm_targets, oracle, coverage_level, learning_rate
)
paths <- c(paths, oti_write_csv(
  prior_shift, file.path(output_root, "dlm_prior_shift_summary.csv")
))

direction_profiles <- list()
direction_optima <- list()
for (target in oracle$target) {
  truth <- oti_target_row(dlm_targets, target)
  profile <- otf_direction_profile(
    dlm_dgp, truth, coverage_level, learning_rate
  )
  profile$target <- target
  direction_profiles[[target]] <- profile
  optimum <- attr(profile, "optimum")
  optimum$target <- target
  direction_optima[[target]] <- optimum
}
direction_profile <- do.call(rbind, direction_profiles)
direction_optimum <- do.call(rbind, direction_optima)
rownames(direction_profile) <- NULL
rownames(direction_optimum) <- NULL
paths <- c(paths, oti_write_csv(
  direction_profile,
  file.path(output_root, "dlm_escaping_direction_profile.csv")
))
paths <- c(paths, oti_write_csv(
  direction_optimum,
  file.path(output_root, "dlm_escaping_direction_optimum.csv")
))

declared_cases <- rbind(
  data.frame(
    stage = "fixed_design_acceptance",
    family = "fixed_design",
    target = as.character(config$fixed_design$target %||% "SH"),
    chain_or_profile = seq_len(as.integer(
      config$fixed_design$mcmc_control$n_chains %||% 4L
    )),
    stringsAsFactors = FALSE
  ),
  data.frame(
    stage = "dlm_trace_forensics",
    family = "dlm",
    target = rep(
      as.character(config$dlm$targets %||% c("ET", "SH")),
      each = length(config$dlm$initial_profiles)
    ),
    chain_or_profile = rep(
      as.character(config$dlm$initial_profiles),
      times = length(config$dlm$targets %||% c("ET", "SH"))
    ),
    stringsAsFactors = FALSE
  )
)
if (!isTRUE((config$fixed_design %||% list())$enabled %||% TRUE)) {
  declared_cases <- declared_cases[
    declared_cases$stage != "fixed_design_acceptance",
    ,
    drop = FALSE
  ]
}
prior_cfg <- config$prior_sensitivity %||% list()
if (isTRUE(prior_cfg$enabled %||% FALSE)) {
  prior_variances <- as.numeric(prior_cfg$initial_slope_variances)
  prior_profiles <- as.character(prior_cfg$initial_profiles)
  if (!length(prior_variances) ||
      any(!is.finite(prior_variances) | prior_variances <= 0)) {
    oti_stop("Prior-sensitivity slope variances must be finite and positive.")
  }
  if (!length(prior_profiles) ||
      !all(prior_profiles %in% c("oracle", "prior_shift_stress"))) {
    oti_stop(
      paste(
        "Prior-sensitivity profiles must be oracle and/or",
        "prior_shift_stress."
      )
    )
  }
  declared_cases <- otf_rbind_fill(list(
    declared_cases,
    expand.grid(
      stage = "dlm_prior_sensitivity",
      family = "dlm",
      target = toupper(as.character(prior_cfg$target %||% "SH")),
      chain_or_profile = prior_profiles,
      initial_slope_variance = prior_variances,
      stringsAsFactors = FALSE
    )
  ))
}
paths <- c(paths, oti_write_csv(
  declared_cases, file.path(output_root, "fit_plan.csv")
))

execution_summary <- data.frame()
diagnostics_all <- data.frame()
chain_summary_all <- data.frame()
trace_summary_all <- data.frame()
conditional_all <- data.frame()
curve_all <- data.frame()
tilt_path_summary <- data.frame()
prior_sensitivity <- data.frame()
prior_sensitivity_summary <- data.frame()
prior_sensitivity_diagnostics <- data.frame()

if (identical(mode, "execute")) {
  diagnostics_cfg <- config$diagnostics %||% list()
  diagnostic_contract <- list(
    diagnostics = list(
      enabled = TRUE,
      provider = "posterior",
      rhat_max = as.numeric(diagnostics_cfg$rhat_max %||% 1.05),
      bulk_ess_min = as.numeric(diagnostics_cfg$bulk_ess_min %||% 400),
      tail_ess_min = as.numeric(diagnostics_cfg$tail_ess_min %||% 200),
      mcse_over_sd_max =
        as.numeric(diagnostics_cfg$mcse_over_sd_max %||% 0.1)
    )
  )

  fixed_cfg <- config$fixed_design %||% list()
  if (isTRUE(fixed_cfg$enabled %||% TRUE)) {
    fixed_target <- toupper(as.character(fixed_cfg$target %||% "SH"))
    fixed_dgp <- oti_fixed_design_dgp(config, law)
    fixed_targets <- oti_targets_by_index(
      fixed_dgp$mean_truth, fixed_dgp$scale_truth, oracle,
      fixed_dgp$observed
    )
    fixed_control <- fixed_cfg$mcmc_control %||% list()
    fixed_run_control <- diagnostic_contract
    fixed_run_control$paper_mode <- TRUE
    fixed_run_control$n_chains <- oti_integer(
      fixed_control$n_chains %||% 4L,
      "fixed_design$mcmc_control$n_chains", 2L
    )
    fixed_fit_config <- config
    fixed_fit_config$mcmc_control <- list()
    fixed_fit_config$paper_mcmc_control <- c(
      list(
        enabled = TRUE,
        n_chains = fixed_run_control$n_chains,
        diagnostics = diagnostic_contract$diagnostics
      ),
      fixed_control[setdiff(names(fixed_control), "n_chains")]
    )
    fixed_result <- oti_fit_fixed_design_target(
      fixed_dgp, fixed_targets, fixed_target, fixed_fit_config,
      quick = FALSE, run_control = fixed_run_control
    )
    fixed_summary <- fixed_result$summary
    fixed_summary$stage <- "fixed_design_acceptance"
    execution_summary <- otf_rbind_fill(list(
      execution_summary, fixed_summary
    ))
    diagnostics_all <- otf_rbind_fill(list(
      diagnostics_all, fixed_result$mcmc_diagnostics
    ))
    chain_summary_all <- otf_rbind_fill(list(
      chain_summary_all, fixed_result$chain_summary
    ))
    curve_all <- otf_rbind_fill(list(curve_all, fixed_result$curves))
  }

  dlm_cfg <- config$dlm %||% list()
  profiles <- as.character(
    dlm_cfg$initial_profiles %||% c("default", "oracle", "narrow", "wide")
  )
  targets <- toupper(as.character(dlm_cfg$targets %||% c("ET", "SH")))
  if (length(profiles) < 2L) {
    oti_stop("At least two DLM initial profiles are required.")
  }
  dlm_control <- dlm_cfg$mcmc_control %||% list()
  dlm_control$store_state_draws <- TRUE
  dlm_control$store_latent_draws <- TRUE
  dlm_control$verbose <- FALSE
  dlm_workers <- oti_integer(dlm_cfg$workers %||% 1L, "dlm$workers", 1L)
  dlm_workers <- min(dlm_workers, length(profiles))
  if (dlm_workers > 1L && .Platform$OS.type == "windows") {
    oti_stop("Parallel DLM forensic chains require a fork-capable platform.")
  }
  target_index <- setNames(seq_along(c("RQR", "ET", "SH")),
                           c("RQR", "ET", "SH"))
  worker_root <- oti_ensure_dir(file.path(output_root, "worker_results"))
  worker_source_contract <- list(
    schema_version = otf_schema(),
    config_sha256 = oti_file_sha256(config_path),
    runner_sha256 = oti_file_sha256(script_path),
    illustration_utils_sha256 = oti_file_sha256(file.path(
      script_dir, "32_oracle_tilt_illustration_utils.R"
    )),
    forensic_utils_sha256 = oti_file_sha256(file.path(
      script_dir, "33_oracle_tilt_forensic_utils.R"
    )),
    package_version = as.character(utils::packageVersion("rqrgibbs")),
    coverage_level = coverage_level,
    learning_rate = learning_rate,
    mcmc_control = dlm_control,
    initial_level_variance = dlm_dgp$initial_level_variance,
    initial_slope_variance = dlm_dgp$initial_slope_variance
  )
  for (target in targets) {
    truth <- oti_target_row(dlm_targets, target)
    run_chain <- function(chain) {
      profile <- profiles[chain]
      seed <- as.integer(dlm_cfg$seed %||% 202607282L) +
        100L * target_index[[target]] + 10000L * (chain - 1L)
      worker_contract <- c(
        worker_source_contract,
        list(
          target = target,
          chain = chain,
          profile = profile,
          seed = seed
        )
      )
      worker_contract_digest <- otf_object_sha256(worker_contract)
      result_path <- file.path(
        worker_root,
        sprintf(
          "dlm_%s_chain%02d_%s.rds",
          tolower(target), chain, profile
        )
      )
      trace_path <- file.path(
        output_root,
        sprintf(
          "dlm_%s_chain%02d_%s_trace.csv",
          tolower(target), chain, profile
        )
      )
      if (file.exists(result_path)) {
        existing <- tryCatch(readRDS(result_path), error = function(e) NULL)
        valid_existing <- !is.null(existing) &&
          identical(
            existing$worker_contract_digest,
            worker_contract_digest
          ) &&
          file.exists(trace_path) &&
          identical(
            existing$trace_sha256,
            oti_file_sha256(trace_path)
          )
        if (valid_existing) {
          return(list(path = result_path, resumed = TRUE))
        }
      }
      control <- dlm_control
      control$seed <- seed
      init <- otf_initial_state_paths(
        profile, dlm_dgp, truth,
        coverage_level = coverage_level,
        learning_rate = learning_rate
      )
      elapsed <- system.time(fit <- rqrgibbs::rqr_dlm_fit(
        y = dlm_dgp$y,
        model = dlm_dgp$model,
        coverage_level = coverage_level,
        evolution_mode = "fixed_W",
        W = dlm_dgp$W,
        learning_rate = learning_rate,
        learning_rate_mode = "fixed_rate",
        mean_tilt = truth$mean_tilt,
        numerical_policy = "fail",
        mcmc_control = control,
        init = init
      ))
      pred <- rqrgibbs::predict_interval(fit)
      trace <- otf_dlm_trace_frame(
        fit, dlm_dgp, truth, target, chain, profile,
        coverage_level, learning_rate
      )
      conditional <- otf_conditional_reference(
        fit, dlm_dgp, truth, coverage_level
      )
      conditional$target <- target
      conditional$chain <- chain
      conditional$profile <- profile
      result <- list(
        pred = pred,
        scalar_draws = oti_scalar_draw_matrix(
          "dlm", pred, truth, dlm_dgp$y, coverage_level
        ),
        trace_summary = otf_trace_summary(trace),
        conditional = conditional,
        chain_summary = oti_chain_summary(
          "dlm", target, chain, seed, fit, elapsed
        )
      )
      otf_atomic_write_csv(trace, trace_path)
      envelope <- list(
        worker_contract = worker_contract,
        worker_contract_digest = worker_contract_digest,
        trace_sha256 = oti_file_sha256(trace_path),
        result = result
      )
      otf_atomic_save_rds(envelope, result_path, compress = FALSE)
      list(path = result_path, resumed = FALSE)
    }
    chain_indices <- seq_along(profiles)
    worker_returns <- if (dlm_workers > 1L) {
      parallel::mclapply(
        chain_indices,
        run_chain,
        mc.cores = dlm_workers,
        mc.preschedule = FALSE,
        mc.set.seed = FALSE
      )
    } else {
      lapply(chain_indices, run_chain)
    }
    failed_chains <- which(vapply(
      worker_returns, inherits, logical(1L), what = "try-error"
    ))
    if (length(failed_chains)) {
      oti_stop(
        "Parallel DLM chains failed: ",
        paste(failed_chains, collapse = ", "),
        "."
      )
    }
    worker_paths <- vapply(
      worker_returns, `[[`, character(1L), "path"
    )
    worker_resumed <- vapply(
      worker_returns, `[[`, logical(1L), "resumed"
    )
    worker_envelopes <- lapply(worker_paths, readRDS)
    chain_results <- lapply(worker_envelopes, `[[`, "result")
    for (chain in chain_indices) {
      chain_results[[chain]]$chain_summary[[
        "resumed_from_worker_artifact"
      ]] <- worker_resumed[chain]
    }
    for (chain in chain_indices) {
      profile <- profiles[chain]
      trace_path <- file.path(
        output_root,
        sprintf(
          "dlm_%s_chain%02d_%s_trace.csv",
          tolower(target), chain, profile
        )
      )
      paths <- c(paths, trace_path)
    }
    worker_manifest <- data.frame(
      target = target,
      chain = chain_indices,
      profile = profiles,
      resumed = worker_resumed,
      result_relative_path = sub(
        paste0("^", normalizePath(
          repo_root, winslash = "/", mustWork = TRUE
        ), "/?"),
        "",
        normalizePath(worker_paths, winslash = "/", mustWork = TRUE)
      ),
      result_sha256 = vapply(
        worker_paths, oti_file_sha256, character(1L)
      ),
      result_bytes = file.info(worker_paths)$size,
      trace_sha256 = vapply(
        worker_envelopes, `[[`, character(1L), "trace_sha256"
      ),
      stringsAsFactors = FALSE
    )
    paths <- c(paths, oti_write_csv(
      worker_manifest,
      file.path(
        output_root,
        paste0("dlm_", tolower(target), "_worker_manifest.csv")
      )
    ))
    predictions <- lapply(chain_results, `[[`, "pred")
    combined <- oti_combine_predictions(predictions)
    target_diagnostics <- oti_mcmc_diagnostics(
      "dlm", target,
      lapply(chain_results, `[[`, "scalar_draws"),
      diagnostic_contract
    )
    metrics <- oti_interval_metrics(combined, truth, dlm_dgp$y)
    repairs <- sum(vapply(
      chain_results,
      function(x) x$chain_summary$numerical_repair_count,
      numeric(1L)
    ))
    shift <- otf_prior_canonical_shift(
      dlm_dgp, truth$mean_tilt, coverage_level, learning_rate
    )$ordinate_shift
    target_summary <- cbind(
      data.frame(
        stage = "dlm_trace_forensics",
        family = "dlm",
        target = target,
        n_chains = length(profiles),
        n_draws = ncol(combined$lower_draws),
        numerical_repair_count = repairs,
        diagnostics_pass = all(target_diagnostics$pass),
        posterior_upper_prior_shift_correlation =
          stats::cor(combined$upper_mean, shift),
        posterior_upper_prior_shift_zero_intercept_scale =
          unname(stats::coef(stats::lm(
            combined$upper_mean ~ 0 + shift
          ))[1L]),
        maximum_absolute_endpoint_mean =
          max(abs(c(combined$lower_mean, combined$upper_mean))),
        width_inflation_ratio =
          metrics$mean_width / metrics$oracle_mean_width,
        stringsAsFactors = FALSE
      ),
      metrics
    )
    execution_summary <- otf_rbind_fill(list(
      execution_summary, target_summary
    ))
    diagnostics_all <- otf_rbind_fill(list(
      diagnostics_all, target_diagnostics
    ))
    chain_summary_all <- otf_rbind_fill(c(
      list(chain_summary_all),
      lapply(chain_results, `[[`, "chain_summary")
    ))
    trace_summary_all <- otf_rbind_fill(c(
      list(trace_summary_all),
      lapply(chain_results, `[[`, "trace_summary")
    ))
    conditional_all <- otf_rbind_fill(c(
      list(conditional_all),
      lapply(chain_results, `[[`, "conditional")
    ))
    curve_all <- otf_rbind_fill(list(
      curve_all,
      oti_curve_frame(
        "dlm", target, dlm_dgp$time, dlm_dgp$y, combined, truth
      )
    ))
  }

  if (isTRUE(prior_cfg$enabled %||% FALSE)) {
    prior_target <- toupper(as.character(prior_cfg$target %||% "SH"))
    if (!prior_target %in% oracle$target) {
      oti_stop("The prior-sensitivity target is not an oracle target.")
    }
    truth <- oti_target_row(dlm_targets, prior_target)
    prior_control <- prior_cfg$mcmc_control %||% list()
    prior_control$store_state_draws <- TRUE
    prior_control$store_latent_draws <- TRUE
    prior_control$verbose <- FALSE
    level_variance <- oti_scalar(
      prior_cfg$initial_level_variance %||%
        dlm_dgp$initial_level_variance,
      "prior_sensitivity$initial_level_variance",
      .Machine$double.eps
    )
    width_ratio_threshold <- oti_scalar(
      prior_cfg$remote_width_ratio_threshold %||% 20,
      "prior_sensitivity$remote_width_ratio_threshold",
      1
    )
    endpoint_sd_threshold <- oti_scalar(
      prior_cfg$remote_endpoint_sd_threshold %||% 20,
      "prior_sensitivity$remote_endpoint_sd_threshold",
      1
    )
    maximum_remote_fraction <- otf_assert_probability(
      prior_cfg$maximum_remote_draw_fraction %||% 0.01,
      "prior_sensitivity$maximum_remote_draw_fraction"
    )
    for (variance_index in seq_along(prior_variances)) {
      slope_variance <- prior_variances[variance_index]
      variance_trace_chains <- list()
      sensitivity_dgp <- otf_dlm_with_initial_prior(
        dlm_dgp, level_variance, slope_variance
      )
      prior_displacement <- otf_prior_canonical_shift(
        sensitivity_dgp, truth$mean_tilt, coverage_level, learning_rate
      )
      for (profile_index in seq_along(prior_profiles)) {
        profile <- prior_profiles[profile_index]
        control <- prior_control
        control$seed <- as.integer(dlm_cfg$seed %||% 202607282L) +
          800000L + 10000L * variance_index + profile_index
        init <- otf_initial_state_paths(
          profile, sensitivity_dgp, truth,
          coverage_level = coverage_level,
          learning_rate = learning_rate
        )
        elapsed <- system.time(fit <- rqrgibbs::rqr_dlm_fit(
          y = sensitivity_dgp$y,
          model = sensitivity_dgp$model,
          coverage_level = coverage_level,
          evolution_mode = "fixed_W",
          W = sensitivity_dgp$W,
          learning_rate = learning_rate,
          learning_rate_mode = "fixed_rate",
          mean_tilt = truth$mean_tilt,
          numerical_policy = "fail",
          mcmc_control = control,
          init = init
        ))
        pred <- rqrgibbs::predict_interval(fit)
        trace <- otf_dlm_trace_frame(
          fit, sensitivity_dgp, truth, prior_target,
          profile_index, profile, coverage_level, learning_rate
        )
        pathology <- otf_scale_pathology_summary(
          pred, sensitivity_dgp, truth, trace,
          width_ratio_threshold = width_ratio_threshold,
          endpoint_sd_threshold = endpoint_sd_threshold
        )
        metrics <- oti_interval_metrics(
          pred, truth, sensitivity_dgp$y
        )
        repairs <- fit$model_spec$numerical_repair_count %||% 0L
        row <- cbind(
          data.frame(
            target = prior_target,
            initial_level_variance = level_variance,
            initial_slope_variance = slope_variance,
            profile = profile,
            seed = control$seed,
            elapsed_seconds = unname(elapsed[["elapsed"]]),
            retained_draws = ncol(pred$lower_draws),
            numerical_repair_count = repairs,
            terminal_prior_ordinate_shift =
              tail(prior_displacement$ordinate_shift, 1L),
            maximum_absolute_endpoint_mean =
              max(abs(c(pred$lower_mean, pred$upper_mean))),
            width_inflation_ratio =
              metrics$mean_width / metrics$oracle_mean_width,
            stringsAsFactors = FALSE
          ),
          metrics,
          pathology
        )
        prior_sensitivity <- otf_rbind_fill(list(
          prior_sensitivity, row
        ))
        variance_trace_chains[[profile]] <-
          otf_trace_diagnostic_matrix(trace)
        conditional <- otf_conditional_reference(
          fit, sensitivity_dgp, truth, coverage_level
        )
        conditional$stage <- "dlm_prior_sensitivity"
        conditional$target <- prior_target
        conditional$initial_slope_variance <- slope_variance
        conditional$profile <- profile
        conditional_all <- otf_rbind_fill(list(
          conditional_all, conditional
        ))
        trace_path <- file.path(
          output_root,
          sprintf(
            "dlm_%s_priorvar_%s_%s_trace.csv",
            tolower(prior_target),
            gsub("\\.", "p", format(slope_variance, scientific = FALSE)),
            profile
          )
        )
        paths <- c(paths, oti_write_csv(trace, trace_path))
      }
      variance_diagnostics <- oti_mcmc_diagnostics(
        "dlm_prior_sensitivity",
        prior_target,
        unname(variance_trace_chains),
        diagnostic_contract
      )
      variance_diagnostics$initial_slope_variance <- slope_variance
      prior_sensitivity_diagnostics <- otf_rbind_fill(list(
        prior_sensitivity_diagnostics, variance_diagnostics
      ))
    }
    grouped <- split(
      prior_sensitivity, prior_sensitivity$initial_slope_variance
    )
    prior_sensitivity_summary <- do.call(rbind, lapply(
      grouped,
      function(z) {
        variance_diagnostics <- prior_sensitivity_diagnostics[
          prior_sensitivity_diagnostics$initial_slope_variance ==
            z$initial_slope_variance[1L],
          ,
          drop = FALSE
        ]
        diagnostic_pass <- nrow(variance_diagnostics) > 0L &&
          all(variance_diagnostics$pass)
        scale_stability_pass <-
          all(z$remote_draw_fraction <= maximum_remote_fraction) &&
          all(z$numerical_repair_count == 0L)
        data.frame(
          target = z$target[1L],
          initial_level_variance = z$initial_level_variance[1L],
          initial_slope_variance = z$initial_slope_variance[1L],
          n_profiles = nrow(z),
          maximum_remote_draw_fraction = max(z$remote_draw_fraction),
          maximum_endpoint_over_response_sd =
            max(z$maximum_endpoint_over_response_sd),
          maximum_width_over_oracle_width =
            max(z$maximum_width_over_oracle_width),
          maximum_width_inflation_ratio =
            max(z$width_inflation_ratio),
          numerical_repair_count = sum(z$numerical_repair_count),
          scale_stability_pass = scale_stability_pass,
          diagnostics_pass = diagnostic_pass,
          sensitivity_pass = scale_stability_pass && diagnostic_pass,
          stringsAsFactors = FALSE
        )
      }
    ))
    rownames(prior_sensitivity_summary) <- NULL
  }

  tilt_cfg <- config$tilt_path %||% list()
  if (isTRUE(tilt_cfg$enabled %||% FALSE)) {
    et_delta <- oracle$delta_innovation[oracle$target == "ET"]
    sh_delta <- oracle$delta_innovation[oracle$target == "SH"]
    fractions <- as.numeric(tilt_cfg$fractions_from_et_to_sh)
    if (!length(fractions) || any(!is.finite(fractions)) ||
        any(fractions < 0 | fractions > 1)) {
      oti_stop("Tilt-path fractions must be finite values in [0, 1].")
    }
    path_control <- tilt_cfg$mcmc_control %||% list()
    path_control$verbose <- FALSE
    for (index in seq_along(fractions)) {
      fraction <- fractions[index]
      delta_innovation <- et_delta + fraction * (sh_delta - et_delta)
      interval <- otf_interval_from_delta(
        law, coverage_level, delta_innovation
      )
      path_oracle <- data.frame(
        target = sprintf("PATH_%03d", round(100 * fraction)),
        interval,
        coverage_level = coverage_level,
        law_family = law$family,
        law_tau = law$tau,
        law_standardized = law$standardized,
        oracle_construction = "population_quantile_truncated_moment",
        uses_cornish_fisher = FALSE,
        stringsAsFactors = FALSE
      )
      path_targets <- oti_targets_by_index(
        dlm_dgp$mean_truth, dlm_dgp$scale_truth,
        path_oracle, dlm_dgp$observed
      )
      truth <- path_targets
      control <- path_control
      control$seed <- as.integer(dlm_cfg$seed %||% 202607282L) +
        500000L + index
      fit <- rqrgibbs::rqr_dlm_fit(
        y = dlm_dgp$y,
        model = dlm_dgp$model,
        coverage_level = coverage_level,
        evolution_mode = "fixed_W",
        W = dlm_dgp$W,
        learning_rate = learning_rate,
        learning_rate_mode = "fixed_rate",
        mean_tilt = truth$mean_tilt,
        numerical_policy = "fail",
        mcmc_control = control
      )
      pred <- rqrgibbs::predict_interval(fit)
      metrics <- oti_interval_metrics(pred, truth, dlm_dgp$y)
      shift <- otf_prior_canonical_shift(
        dlm_dgp, truth$mean_tilt, coverage_level, learning_rate
      )$ordinate_shift
      row <- cbind(
        data.frame(
          fraction_et_to_sh = fraction,
          delta_innovation = delta_innovation,
          u = interval$u,
          terminal_prior_shift = tail(shift, 1L),
          maximum_absolute_endpoint_mean =
            max(abs(c(pred$lower_mean, pred$upper_mean))),
          posterior_upper_prior_shift_correlation =
            stats::cor(pred$upper_mean, shift),
          numerical_repair_count =
            fit$model_spec$numerical_repair_count %||% 0L,
          stringsAsFactors = FALSE
        ),
        metrics
      )
      tilt_path_summary <- otf_rbind_fill(list(tilt_path_summary, row))
    }
  }

  paths <- c(paths, oti_write_csv(
    execution_summary, file.path(output_root, "execution_summary.csv")
  ))
  paths <- c(paths, oti_write_csv(
    diagnostics_all, file.path(output_root, "mcmc_diagnostics.csv")
  ))
  paths <- c(paths, oti_write_csv(
    chain_summary_all, file.path(output_root, "chain_summary.csv")
  ))
  paths <- c(paths, oti_write_csv(
    trace_summary_all, file.path(output_root, "trace_summary.csv")
  ))
  paths <- c(paths, oti_write_csv(
    conditional_all, file.path(output_root, "conditional_references.csv")
  ))
  paths <- c(paths, oti_write_csv(
    curve_all, file.path(output_root, "fit_curves.csv")
  ))
  if (nrow(tilt_path_summary)) {
    paths <- c(paths, oti_write_csv(
      tilt_path_summary, file.path(output_root, "tilt_path_summary.csv")
    ))
  }
  if (nrow(prior_sensitivity)) {
    paths <- c(paths, oti_write_csv(
      prior_sensitivity,
      file.path(output_root, "dlm_prior_sensitivity.csv")
    ))
    paths <- c(paths, oti_write_csv(
      prior_sensitivity_summary,
      file.path(output_root, "dlm_prior_sensitivity_summary.csv")
    ))
    paths <- c(paths, oti_write_csv(
      prior_sensitivity_diagnostics,
      file.path(output_root, "dlm_prior_sensitivity_diagnostics.csv")
    ))
  }
}

conditional_relative_tolerance <- as.numeric(
  (config$diagnostics %||% list())$
    conditional_reference_relative_tolerance %||% 1e-7
)
R_cpp_absolute_tolerance <- as.numeric(
  (config$diagnostics %||% list())$R_cpp_absolute_tolerance %||% 1e-10
)
closeout <- list(
  schema_version = otf_schema(),
  run_id = run_id,
  mode = mode,
  finished_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  preflight_pass =
    all(is.finite(prior_shift$terminal_ordinate_shift)) &&
    all(is.finite(direction_optimum$optimum_slope)),
  execution_completed = identical(mode, "execute"),
  fixed_design_acceptance_pass = if (identical(mode, "execute")) {
    rows <- diagnostics_all$family == "fixed_design"
    any(rows) && all(diagnostics_all$pass[rows])
  } else {
    FALSE
  },
  dlm_et_acceptance_pass = if (identical(mode, "execute")) {
    rows <- diagnostics_all$family == "dlm" &
      diagnostics_all$target == "ET"
    any(rows) && all(diagnostics_all$pass[rows])
  } else {
    FALSE
  },
  dlm_sh_acceptance_pass = if (identical(mode, "execute")) {
    rows <- diagnostics_all$family == "dlm" &
      diagnostics_all$target == "SH"
    any(rows) && all(diagnostics_all$pass[rows])
  } else {
    FALSE
  },
  conditional_reference_pass = if (identical(mode, "execute")) {
    otf_conditional_reference_pass(
      conditional_all,
      relative_tolerance = conditional_relative_tolerance,
      R_cpp_absolute_tolerance = R_cpp_absolute_tolerance
    )
  } else {
    FALSE
  },
  prior_sensitivity_pass = if (identical(mode, "execute") &&
      isTRUE(prior_cfg$enabled %||% FALSE)) {
    nrow(prior_sensitivity_summary) > 0L &&
      any(prior_sensitivity_summary$sensitivity_pass)
  } else {
    FALSE
  },
  manuscript_promotion_authorized = FALSE
)
paths <- c(paths, oti_write_json(
  closeout, file.path(output_root, "closeout.json")
))
manifest <- oti_artifact_manifest(paths, root = repo_root)
manifest_path <- file.path(output_root, "artifact_manifest.csv")
oti_write_csv(manifest, manifest_path)

message("[oracle-tilt-forensics] mode: ", mode)
message("[oracle-tilt-forensics] output: ", output_root)
message("[oracle-tilt-forensics] manifest: ", manifest_path)
