# Corrected exact-mean-tilt adapters for the append-only V5 illustration.
#
# This file is sourced after the historical illustration and V3 utility
# modules. It changes only the oracle construction and V5 evidence schemas.
# Historical V1--V4 code and artifacts remain unchanged and reproducible from
# their recorded commits.

otv5_schema <- function() "rqrgibbs_oracle_tilt_publication/5.0.0"

otv5_config_schema <- function() {
  "rqrgibbs_oracle_tilt_publication_config/5.0.0"
}

otv5_worker_schema <- function() {
  "rqrgibbs_oracle_tilt_publication_worker/5.0.0"
}

otv5_cell_schema <- function() {
  "rqrgibbs_oracle_tilt_publication_cell/5.0.0"
}

otv5_preflight_schema <- function() {
  "rqrgibbs_oracle_tilt_publication_preflight/5.0.0"
}

otv5_oracle_schema <- function() "rqrgibbs_interval_oracle/2.0.0"

otv5_tilt_definition <- function() {
  "conditional_retained_mean_minus_population_mean"
}

# Historical validators resolve these schema helpers dynamically. Rebinding
# them inside the isolated V5 runner gives every newly created worker, cell,
# and preflight artifact a V5 identity without editing the frozen V3 source.
otv3_worker_schema <- otv5_worker_schema
otv3_cell_schema <- otv5_cell_schema
otv3_preflight_schema <- otv5_preflight_schema

otv5_validate_config <- function(config) {
  if (!identical(
    as.character(config$schema_version), otv5_config_schema()
  )) {
    oti_stop("Unsupported corrected oracle-tilt V5 configuration schema.")
  }
  required <- c(
    identical(as.character(config$campaign_id),
              "oracle_tilt_c095_v5_exact_delta"),
    identical(as.character(config$tilt_source),
              "exact_population_conditional_mean_oracle"),
    identical(as.character(config$tilt_definition),
              otv5_tilt_definition()),
    identical(as.character(config$oracle_schema), otv5_oracle_schema()),
    identical(config$legacy_oracle_schemas_authorized, FALSE),
    identical(as.character(config$historical_design_source),
              "oracle_tilt_c095_publication_v3_20260801"),
    identical(config$interpretation$response_likelihood, FALSE),
    identical(config$interpretation$response_predictive_draws, FALSE),
    identical(config$interpretation$cornish_fisher_used, FALSE),
    identical(config$interpretation$simulation_study, FALSE),
    identical(config$interpretation$single_dataset_illustration, TRUE),
    identical(config$interpretation$historical_v1_v4_evidence_mutated, FALSE),
    identical(config$interpretation$automatic_manuscript_promotion, FALSE)
  )
  if (!all(required)) {
    oti_stop("The corrected V5 oracle or interpretation contract changed.")
  }

  # Reuse the frozen V3 design/MCMC validator after changing only the two
  # versioned fields that identify the corrected campaign. This proves that
  # V5 isolates the oracle normalization change rather than silently changing
  # the illustrative DGP, prior, kernel, diagnostics, or resource contract.
  legacy <- config
  legacy$schema_version <- otv3_config_schema()
  legacy$tilt_source <- "exact_population_oracle"
  otv3_validate_config(legacy)
  invisible(config)
}

otv5_design_preflight <- function(config) {
  otv5_validate_config(config)
  legacy <- config
  legacy$schema_version <- otv3_config_schema()
  legacy$tilt_source <- "exact_population_oracle"
  out <- otv3_design_preflight(legacy)
  out$config_schema <- otv5_config_schema()
  out$oracle_schema <- otv5_oracle_schema()
  out$tilt_definition <- otv5_tilt_definition()
  out
}

otv5_oracle_targets <- function(
    law, coverage_level, targets = c("RQR", "ET", "SH")) {
  if (!requireNamespace("rqrgibbs", quietly = TRUE)) {
    oti_stop("rqrgibbs is required for corrected V5 oracle construction.")
  }
  targets <- oti_normalize_targets(targets)
  if (!inherits(law, "oti_distribution_law") ||
      !identical(law$family, "asymmetric_laplace")) {
    oti_stop("V5 currently requires the declared asymmetric-Laplace law.")
  }
  params <- list(
    tau = law$tau, scale = 1,
    variance_standardized = isTRUE(law$standardized)
  )
  certificates <- lapply(targets, function(target) {
    rqrgibbs::rqr_interval_oracle(
      family = "asymmetric_laplace",
      coverage_level = coverage_level,
      target = target,
      params = params,
      tol = 1e-10,
      grid_size = 1601L
    )
  })
  rows <- Map(function(target, certificate) {
    if (!inherits(certificate, "rqr_interval_oracle") ||
        !identical(certificate$schema_version, otv5_oracle_schema()) ||
        !identical(certificate$tilt_definition, otv5_tilt_definition()) ||
        isTRUE(certificate$uses_cornish_fisher)) {
      oti_stop("A corrected V5 target certificate failed its schema contract.")
    }
    data.frame(
      target = target,
      u = certificate$lower_probability,
      lower_innovation = certificate$lower_root,
      upper_innovation = certificate$upper_root,
      width_innovation = certificate$width,
      truncated_first_moment_innovation =
        certificate$truncated_first_moment,
      conditional_retained_mean_innovation =
        certificate$conditional_retained_mean,
      population_mean_innovation = certificate$population_mean,
      delta_innovation = certificate$mean_tilt,
      content = certificate$content,
      coverage_level = certificate$coverage_level,
      law_family = law$family,
      law_tau = law$tau,
      law_standardized = law$standardized,
      oracle_schema = certificate$schema_version,
      tilt_definition = certificate$tilt_definition,
      oracle_construction = certificate$target_method,
      oracle_certificate_digest = certificate$certificate_digest,
      oracle_distribution_digest = certificate$distribution_digest,
      oracle_solver_digest = certificate$solver_digest,
      content_residual = certificate$content_residual,
      retained_mean_residual = certificate$retained_mean_residual,
      unique_minimizer = certificate$unique_minimizer,
      uses_cornish_fisher = FALSE,
      stringsAsFactors = FALSE
    )
  }, targets, certificates)
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  if (nrow(out) != length(targets) || anyDuplicated(out$target) ||
      any(abs(out$content_residual) > 1e-10) ||
      any(abs(out$retained_mean_residual) > 1e-10) ||
      any(!out$unique_minimizer) ||
      any(!grepl("^[0-9a-f]{64}$", out$oracle_certificate_digest))) {
    oti_stop("The corrected V5 oracle table failed its numerical contract.")
  }
  out
}

# The frozen V3 computation functions call this helper dynamically. V5
# replaces it only inside the V5 runner process, after historical source files
# have been loaded, so no legacy artifact is reinterpreted.
oti_oracle_targets <- otv5_oracle_targets

# V5 stores the ordinary target as exact zero only after its independent
# retained-mean certificate has passed. No ambiguous unnormalized
# `retained_mean_innovation` compatibility column is accepted.
otv3_exact_zero_rqr_oracle <- function(oracle) {
  required <- c(
    "target", "delta_innovation", "conditional_retained_mean_innovation",
    "population_mean_innovation", "retained_mean_residual",
    "oracle_schema", "tilt_definition"
  )
  if (!is.data.frame(oracle) || !all(required %in% names(oracle)) ||
      any(oracle$oracle_schema != otv5_oracle_schema()) ||
      any(oracle$tilt_definition != otv5_tilt_definition())) {
    oti_stop("A legacy or malformed oracle table cannot authorize V5.")
  }
  selected <- as.character(oracle$target) == "RQR"
  if (sum(selected) != 1L ||
      abs(oracle$delta_innovation[selected]) > 1e-10 ||
      abs(oracle$conditional_retained_mean_innovation[selected] -
          oracle$population_mean_innovation[selected]) > 1e-10 ||
      abs(oracle$retained_mean_residual[selected]) > 1e-10) {
    oti_stop("The certified V5 RQR oracle is not compatible with zero tilt.")
  }
  oracle$delta_innovation[selected] <- 0
  oracle$conditional_retained_mean_innovation[selected] <-
    oracle$population_mean_innovation[selected]
  oracle
}

otv5_reject_legacy_oracle <- function(oracle) {
  valid <- is.data.frame(oracle) && all(c(
    "oracle_schema", "tilt_definition", "oracle_certificate_digest",
    "conditional_retained_mean_innovation", "population_mean_innovation"
  ) %in% names(oracle)) &&
    all(oracle$oracle_schema == otv5_oracle_schema()) &&
    all(oracle$tilt_definition == otv5_tilt_definition()) &&
    all(grepl("^[0-9a-f]{64}$", oracle$oracle_certificate_digest))
  if (!valid) oti_stop("Legacy oracle evidence is not valid V5 evidence.")
  invisible(TRUE)
}

otv5_reference_suite <- function(config) {
  otv5_validate_config(config)
  base <- otv3_reference_suite(config)
  matrix_rows <- list()
  index <- 0L
  for (tau in c(0.20, 0.50, 0.80)) {
    for (coverage in c(0.80, 0.90, 0.95)) {
      law <- oti_al_law(tau, standardized = TRUE)
      oracle <- otv5_oracle_targets(
        law, coverage, c("RQR", "ET", "SH")
      )
      for (row in seq_len(nrow(oracle))) {
        index <- index + 1L
        matrix_rows[[index]] <- cbind(
          data.frame(tau = tau, coverage_level = coverage),
          oracle[row, , drop = FALSE]
        )
      }
    }
  }
  matrix <- do.call(rbind, matrix_rows)
  symmetric <- matrix[matrix$tau == 0.5, , drop = FALSE]
  symmetric_groups <- split(symmetric, symmetric$coverage_level)
  symmetric_endpoint_gap <- max(vapply(symmetric_groups, function(rows) {
    max(c(diff(range(rows$lower_innovation)),
          diff(range(rows$upper_innovation)),
          max(abs(rows$delta_innovation))))
  }, numeric(1L)))
  reflection_gap <- 0
  for (coverage in c(0.80, 0.90, 0.95)) {
    for (target in c("RQR", "ET", "SH")) {
      left <- matrix[
        matrix$tau == 0.80 & matrix$coverage_level == coverage &
          matrix$target == target, , drop = FALSE
      ]
      right <- matrix[
        matrix$tau == 0.20 & matrix$coverage_level == coverage &
          matrix$target == target, , drop = FALSE
      ]
      reflection_gap <- max(reflection_gap, abs(c(
        left$lower_innovation + right$upper_innovation,
        left$upper_innovation + right$lower_innovation,
        left$width_innovation - right$width_innovation,
        left$delta_innovation + right$delta_innovation
      )))
    }
  }
  et_index_gap <- max(abs(
    matrix$u[matrix$target == "ET"] -
      (1 - matrix$coverage_level[matrix$target == "ET"]) / 2
  ))
  sh <- matrix[matrix$target == "SH", , drop = FALSE]
  sh_index_gap <- max(abs(sh$u - sh$tau * (1 - sh$coverage_level)))
  zero <- matrix[matrix$target == "RQR", "delta_innovation"]
  current <- otv5_oracle_targets(
    oti_al_law(config$innovation$tau, standardized = TRUE),
    config$coverage_level, config$targets
  )
  legacy <- utils::read.csv(
    file.path("figures", "data", "oracle_tilt_c095_v3", "oracle_targets.csv"),
    stringsAsFactors = FALSE
  )
  legacy_rejected <- inherits(try(
    otv5_reject_legacy_oracle(legacy), silent = TRUE
  ), "try-error")

  extra <- data.frame(
    gate = c(
      "oracle_matrix_rows",
      "oracle_matrix_content_residual",
      "oracle_matrix_retained_mean_residual",
      "oracle_matrix_partial_moment_quadrature_gap",
      "oracle_matrix_RQR_exact_zero",
      "oracle_matrix_ET_index",
      "oracle_matrix_AL_SH_index",
      "oracle_matrix_symmetric_collapse",
      "oracle_matrix_reflection",
      "legacy_oracle_rejected",
      "V5_ET_corrected_tilt",
      "V5_SH_corrected_tilt"
    ),
    value = c(
      nrow(matrix),
      max(abs(matrix$content_residual)),
      max(abs(matrix$retained_mean_residual)),
      max(abs(matrix$truncated_first_moment_innovation -
                matrix$coverage_level *
                (matrix$population_mean_innovation +
                   matrix$delta_innovation))),
      as.numeric(all(zero == 0)),
      et_index_gap,
      sh_index_gap,
      symmetric_endpoint_gap,
      reflection_gap,
      as.numeric(legacy_rejected),
      current$delta_innovation[current$target == "ET"],
      current$delta_innovation[current$target == "SH"]
    ),
    threshold = c(
      27, 1e-10, 1e-10, 1e-10, 1, 1e-14, 1e-14, 1e-7, 1e-7, 1,
      0.0560608464325982, 0.11472186306441
    ),
    comparison = c(
      "==", rep("<=", 3L), "==", rep("<=", 4L), "==", "==", "=="
    ),
    stringsAsFactors = FALSE
  )
  extra$pass <- mapply(function(value, threshold, comparison) {
    if (comparison == "<=") value <= threshold else
      abs(value - threshold) <= 1e-13
  }, extra$value, extra$threshold, extra$comparison)
  rbind(base, extra)
}
