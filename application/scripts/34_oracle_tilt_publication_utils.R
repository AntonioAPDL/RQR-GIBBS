otp_schema <- function() "rqrgibbs_oracle_tilt_publication/1.0.0"

otp_config_schema <- function() {
  "rqrgibbs_oracle_tilt_publication_config/1.0.0"
}

otp_validate_config <- function(config) {
  if (!identical(as.character(config$schema_version), otp_config_schema())) {
    oti_stop("Unsupported oracle-tilt publication configuration schema.")
  }
  coverage <- oti_scalar(
    config$coverage_level, "coverage_level", 1e-8, 1 - 1e-8
  )
  if (abs(coverage - 0.95) > 1e-12) {
    oti_stop("The publication illustration contract requires coverage 0.95.")
  }
  targets <- oti_normalize_targets(config$targets)
  if (!identical(targets, c("RQR", "ET", "SH"))) {
    oti_stop("targets must be ordered exactly as RQR, ET, SH.")
  }
  law <- config$innovation %||% list()
  if (!identical(as.character(law$family), "asymmetric_laplace") ||
      abs(as.numeric(law$tau) - 0.99) > 1e-12 ||
      !isTRUE(law$standardized)) {
    oti_stop("The frozen innovation contract is standardized AL_0.99.")
  }
  fixed <- config$fixed_design %||% list()
  dlm <- config$dlm %||% list()
  if (oti_integer(fixed$n_chains, "fixed_design$n_chains", 2L) != 4L) {
    oti_stop("The fixed-design publication grid requires four chains.")
  }
  profiles <- as.character(unlist(dlm$initial_profiles))
  expected_profiles <- c(
    "default", "oracle_centered", "narrow", "wide", "slope_stress"
  )
  if (!identical(profiles, expected_profiles)) {
    oti_stop("The DLM initialization profile contract has changed.")
  }
  if (abs(as.numeric(dlm$initial_level_variance) - 4) > 1e-12 ||
      abs(as.numeric(dlm$initial_slope_variance) - 0.001) > 1e-12) {
    oti_stop("All DLM targets must share C0=diag(4, 0.001).")
  }
  interpretation <- config$interpretation %||% list()
  required_false <- c(
    "response_likelihood", "response_predictive_draws",
    "cornish_fisher_used", "simulation_study"
  )
  if (any(vapply(required_false, function(nm) {
    !identical(interpretation[[nm]], FALSE)
  }, logical(1L))) || !isTRUE(interpretation$population_oracle_tilts)) {
    oti_stop("The interpretation contract is incomplete or inconsistent.")
  }
  invisible(config)
}

otp_plan <- function(config) {
  fixed_chains <- seq_len(as.integer(config$fixed_design$n_chains))
  profiles <- as.character(unlist(config$dlm$initial_profiles))
  rows <- list()
  for (target in as.character(unlist(config$targets))) {
    rows[[length(rows) + 1L]] <- data.frame(
      family = "fixed_design", target = target,
      chain = fixed_chains, profile = "seed_only",
      n_burn = as.integer(config$fixed_design$mcmc_control$n_burn),
      n_mcmc = as.integer(config$fixed_design$mcmc_control$n_mcmc),
      stringsAsFactors = FALSE
    )
    rows[[length(rows) + 1L]] <- data.frame(
      family = "dlm", target = target,
      chain = seq_along(profiles), profile = profiles,
      n_burn = as.integer(config$dlm$mcmc_control$n_burn),
      n_mcmc = as.integer(config$dlm$mcmc_control$n_mcmc),
      stringsAsFactors = FALSE
    )
  }
  out <- do.call(rbind, rows)
  out$cell <- paste(out$family, out$target, sep = "/")
  out$learning_rate_mode <- "fixed_rate"
  out$tilt_source <- "exact_population_oracle"
  out$cornish_fisher_used <- FALSE
  rownames(out) <- NULL
  out
}

otp_seed <- function(config, family, target, chain) {
  base <- as.integer(config[[family]]$seed)
  target_id <- match(target, c("RQR", "ET", "SH"))
  if (is.na(target_id)) oti_stop("Unknown target in seed contract.")
  as.integer(base + 100L * target_id + 10000L * (chain - 1L))
}

otp_diagnostic_contract <- function(config) {
  diagnostics <- config$diagnostics %||% list()
  list(diagnostics = list(
    enabled = TRUE,
    provider = "posterior",
    rhat_max = as.numeric(diagnostics$rhat_max),
    bulk_ess_min = as.numeric(diagnostics$bulk_ess_min),
    tail_ess_min = as.numeric(diagnostics$tail_ess_min),
    mcse_over_sd_max = as.numeric(diagnostics$mcse_over_sd_max)
  ))
}

otp_provenance_summary <- function(fit) {
  data.frame(
    exact_joint_target = isTRUE(fit$model_spec$exact_joint_target),
    target_numerical_eligible =
      isTRUE(fit$model_spec$target_numerical_eligible),
    reproducibility_eligible =
      isTRUE(fit$model_spec$reproducibility_eligible),
    promotion_eligible = isTRUE(fit$model_spec$promotion_eligible),
    primary_runtime_source_match =
      isTRUE(fit$provenance$primary_runtime_source_match),
    stringsAsFactors = FALSE
  )
}

otp_fixed_chain <- function(config, dgp, targets_by_index, target, chain,
                            provenance_control) {
  truth <- oti_target_row(targets_by_index, target)
  seed <- otp_seed(config, "fixed_design", target, chain)
  control <- config$fixed_design$mcmc_control
  control$seed <- seed
  elapsed <- system.time(fit <- rqrgibbs::rqr_mcmc_fit(
    y = dgp$y,
    X = dgp$X,
    coverage_level = config$coverage_level,
    learning_rate = config$learning_rate,
    learning_rate_mode = "fixed_rate",
    mean_tilt = truth$mean_tilt,
    beta_prior_obj = oti_ridge_prior(config$fixed_design$ridge_tau2),
    numerical_policy = "fail",
    provenance_control = provenance_control,
    mcmc_control = control
  ))
  pred <- rqrgibbs::predict_interval(fit, X_new = dgp$X)
  list(
    pred = pred,
    scalar_draws = oti_scalar_draw_matrix(
      "fixed_design", pred, truth, dgp$y, config$coverage_level
    ),
    chain_summary = cbind(
      oti_chain_summary(
        "fixed_design", target, chain, seed, fit, elapsed
      ),
      profile = "seed_only",
      otp_provenance_summary(fit)
    )
  )
}

otp_dlm_chain <- function(config, dgp, targets_by_index, target, chain,
                          provenance_control) {
  truth <- oti_target_row(targets_by_index, target)
  profile <- as.character(unlist(config$dlm$initial_profiles))[chain]
  seed <- otp_seed(config, "dlm", target, chain)
  control <- config$dlm$mcmc_control
  control$seed <- seed
  init <- otf_initial_state_paths(
    profile, dgp, truth,
    coverage_level = config$coverage_level,
    learning_rate = config$learning_rate
  )
  elapsed <- system.time(fit <- rqrgibbs::rqr_dlm_fit(
    y = dgp$y,
    model = dgp$model,
    coverage_level = config$coverage_level,
    evolution_mode = "fixed_W",
    W = dgp$W,
    learning_rate = config$learning_rate,
    learning_rate_mode = "fixed_rate",
    mean_tilt = truth$mean_tilt,
    numerical_policy = "fail",
    provenance_control = provenance_control,
    mcmc_control = control,
    init = init
  ))
  pred <- rqrgibbs::predict_interval(fit)
  trace <- otf_dlm_trace_frame(
    fit, dgp, truth, target, chain, profile,
    config$coverage_level, config$learning_rate
  )
  conditional <- otf_conditional_reference(
    fit, dgp, truth, config$coverage_level
  )
  conditional$target <- target
  conditional$chain <- chain
  conditional$profile <- profile
  pathology <- otf_scale_pathology_summary(
    pred, dgp, truth, trace,
    width_ratio_threshold =
      config$diagnostics$remote_width_ratio_threshold,
    endpoint_sd_threshold =
      config$diagnostics$remote_endpoint_sd_threshold
  )
  pathology$target <- target
  pathology$chain <- chain
  pathology$profile <- profile
  list(
    pred = pred,
    scalar_draws = oti_scalar_draw_matrix(
      "dlm", pred, truth, dgp$y, config$coverage_level
    ),
    chain_summary = cbind(
      oti_chain_summary("dlm", target, chain, seed, fit, elapsed),
      profile = profile,
      otp_provenance_summary(fit)
    ),
    trace = trace,
    trace_summary = otf_trace_summary(trace),
    conditional = conditional,
    pathology = pathology
  )
}

otp_cell_disposition <- function(family, target, diagnostics, chains,
                                 config, conditional = data.frame(),
                                 pathology = data.frame()) {
  gate <- config$diagnostics
  hard_diag <- nrow(diagnostics) > 0L && all(
    is.finite(diagnostics$rhat) & diagnostics$rhat <= gate$rhat_max &
      (is.na(diagnostics$mcse_over_sd) |
         diagnostics$mcse_over_sd <= gate$mcse_over_sd_max)
  )
  strict_diag <- nrow(diagnostics) > 0L && all(diagnostics$pass)
  provenance_pass <- all(
    chains$numerical_repair_count == 0L &
      chains$exact_joint_target &
      chains$target_numerical_eligible &
      chains$reproducibility_eligible &
      chains$promotion_eligible &
      chains$primary_runtime_source_match
  )
  conditional_pass <- if (identical(family, "dlm")) {
    nrow(conditional) > 0L && otf_conditional_reference_pass(
      conditional,
      relative_tolerance = gate$conditional_reference_relative_tolerance,
      R_cpp_absolute_tolerance = gate$R_cpp_absolute_tolerance
    )
  } else {
    TRUE
  }
  pathology_pass <- if (identical(family, "dlm")) {
    nrow(pathology) > 0L &&
      all(pathology$remote_draw_fraction <=
            gate$maximum_remote_draw_fraction)
  } else {
    TRUE
  }
  hard_pass <- provenance_pass && hard_diag && conditional_pass &&
    pathology_pass
  disposition <- if (hard_pass && strict_diag) {
    "strict_pass"
  } else if (hard_pass) {
    "illustration_warning_ess_only"
  } else {
    "fail"
  }
  data.frame(
    family = family,
    target = target,
    provenance_pass = provenance_pass,
    hard_diagnostics_pass = hard_diag,
    strict_diagnostics_pass = strict_diag,
    conditional_reference_pass = conditional_pass,
    pathology_pass = pathology_pass,
    hard_pass = hard_pass,
    disposition = disposition,
    scientifically_usable_for_illustration = hard_pass,
    stringsAsFactors = FALSE
  )
}

otp_summarize_cell <- function(family, target, chain_results, dgp,
                               targets_by_index, config) {
  truth <- oti_target_row(targets_by_index, target)
  pred <- oti_combine_predictions(lapply(chain_results, `[[`, "pred"))
  chains <- do.call(rbind, lapply(chain_results, `[[`, "chain_summary"))
  diagnostics <- oti_mcmc_diagnostics(
    family, target, lapply(chain_results, `[[`, "scalar_draws"),
    otp_diagnostic_contract(config)
  )
  conditional <- if (identical(family, "dlm")) {
    do.call(rbind, lapply(chain_results, `[[`, "conditional"))
  } else data.frame()
  pathology <- if (identical(family, "dlm")) {
    do.call(rbind, lapply(chain_results, `[[`, "pathology"))
  } else data.frame()
  disposition <- otp_cell_disposition(
    family, target, diagnostics, chains, config, conditional, pathology
  )
  x <- if (identical(family, "fixed_design")) dgp$x else dgp$time
  metrics <- oti_interval_metrics(pred, truth, dgp$y)
  fit_summary <- cbind(
    disposition,
    n_chains = nrow(chains),
    retained_draws = ncol(pred$lower_draws),
    numerical_repair_count = sum(chains$numerical_repair_count),
    metrics
  )
  list(
    fit_summary = fit_summary,
    fit_curves = oti_curve_frame(family, target, x, dgp$y, pred, truth),
    endpoint_error_density = oti_endpoint_error_density_frame(
      family, target, pred, truth
    ),
    endpoint_error_summary = oti_endpoint_error_summary_frame(
      family, target, pred, truth
    ),
    endpoint_error_by_index = oti_endpoint_error_by_index_frame(
      family, target, pred, truth
    ),
    chain_summary = chains,
    mcmc_diagnostics = diagnostics,
    conditional_references = conditional,
    pathology_summary = pathology,
    trace_summary = if (identical(family, "dlm")) {
      do.call(rbind, lapply(chain_results, `[[`, "trace_summary"))
    } else data.frame()
  )
}

otp_compact_files <- function() {
  c(
    "config.json", "source_state.json", "runtime_binding.json",
    "fit_plan.csv", "oracle_targets.csv", "dgp_contract.csv",
    "fit_summary.csv", "fit_curves.csv", "endpoint_error_density.csv",
    "endpoint_error_summary.csv", "endpoint_error_by_index.csv",
    "chain_summary.csv", "mcmc_diagnostics.csv",
    "conditional_references.csv", "pathology_summary.csv",
    "trace_summary.csv", "cell_disposition.csv", "closeout.json"
  )
}

otp_verify_manifest <- function(root, manifest_path =
                                file.path(root, "artifact_manifest.csv")) {
  manifest <- utils::read.csv(
    manifest_path, stringsAsFactors = FALSE, check.names = FALSE
  )
  required <- c("path", "bytes", "sha256")
  if (!all(required %in% names(manifest))) {
    oti_stop("Artifact manifest schema is invalid.")
  }
  actual <- vapply(manifest$path, function(relative) {
    oti_file_sha256(file.path(root, relative))
  }, character(1L))
  sizes <- file.info(file.path(root, manifest$path))$size
  if (!identical(tolower(actual), tolower(manifest$sha256)) ||
      !identical(as.numeric(sizes), as.numeric(manifest$bytes))) {
    oti_stop("Artifact-manifest verification failed.")
  }
  invisible(TRUE)
}
