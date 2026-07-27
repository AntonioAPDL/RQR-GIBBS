native_desn_v1_sha <- function(x) {
  digest::digest(x, algo = "sha256", serialize = TRUE)
}

native_desn_v1_design <- function(y = NULL, seed = 9201L) {
  n <- 12L
  X <- cbind(
    intercept = 1,
    h_last_001 = seq(-0.9, 0.9, length.out = n),
    reduced_h_001 = sin(seq_len(n) / 3)
  )
  if (is.null(y)) {
    y <- 0.3 + 0.4 * X[, 2] - 0.2 * X[, 3] +
      0.05 * cos(seq_len(n))
  }
  rqrgibbs:::rqr_desn_design(
    X = X,
    y = y,
    time_index = 101 + seq_len(n),
    intercept = "intercept",
    builder = list(
      id = "native_v1_test_fixture",
      version = "1.0.0"
    ),
    reservoir = list(
      digest = native_desn_v1_sha(list(seed = seed, D = 2L)),
      seed = seed,
      depth = 2L
    ),
    driver = list(
      type = "observed_history",
      response_simulation = FALSE,
      history_digest = native_desn_v1_sha(y)
    ),
    causal = list(
      uses_current_response = FALSE,
      uses_future_response = FALSE,
      minimum_response_lag = 1L
    ),
    terminal = list(
      available = TRUE,
      state_digest = native_desn_v1_sha(c(0.2, -0.1, 0.3)),
      lag_buffer_digest = native_desn_v1_sha(tail(y[!is.na(y)], 2L))
    )
  )
}

native_desn_v1_fit <- function(
    design = native_desn_v1_design(),
    prior = "ridge", seed = 9202L,
    n_burn = 2L, n_mcmc = 5L) {
  prior_controls <- if (identical(prior, "rhs_ns")) {
    list(beta_rhs = list(
      tau0 = 0.5,
      a_zeta = 2,
      b_zeta = 1,
      zeta2_fixed = 1,
      intercept_precision = 0.25
    ))
  } else {
    list(beta_ridge_tau2 = 10)
  }
  rqrgibbs:::rqr_desn_fit(
    design = design,
    coverage_level = 0.8,
    design_engine = "frozen",
    inference = "mcmc",
    learning_rate_mode = "fixed_rate",
    mcmc_args = c(
      list(beta_prior_type = prior),
      prior_controls,
      list(
        n_burn = n_burn,
        n_mcmc = n_mcmc,
        thin = 1L,
        seed = seed,
        store_prior_state_draws = identical(prior, "rhs_ns")
      )
    )
  )
}

native_desn_v1_future_X <- function() {
  cbind(
    intercept = 1,
    h_last_001 = c(1.0, 1.1),
    reduced_h_001 = c(-0.3, -0.2)
  )
}

native_desn_v1_future <- function(
    design, semantics = "precomputed_design") {
  driver <- switch(
    semantics,
    precomputed_design = list(
      source = "fixed_test_matrix"
    ),
    teacher_forced_one_step = list(
      path_digest = native_desn_v1_sha(c(0.4, 0.5))
    ),
    external_driver_path = list(
      path_digest = native_desn_v1_sha(c(0.25, 0.30)),
      generator_id = "separate_driver_fixture"
    )
  )
  origin <- max(design$time_index)
  rqrgibbs:::rqr_desn_future_design(
    parent_design = design,
    X = native_desn_v1_future_X(),
    time_index = origin + 1:2,
    semantics = semantics,
    driver = driver
  )
}

native_desn_v1_reference_design <- function(
    attestation_bytes = charToRaw("native DESN attestation fixture")) {
  ordinary <- native_desn_v1_design()
  arguments_digest <- native_desn_v1_sha(list(
    D = 1L, n = 4L, washout = 3L
  ))
  builder <- list(
    id = rqrgibbs:::.rqr_desn_reference_builder_id(),
    version = "1.1.0",
    source_commit = rqrgibbs:::.rqr_pinned_exdqlm_commit(),
    arguments_digest = arguments_digest,
    adapter = "rqrgibbs_frozen_design_materializer/2.0.0"
  )
  reservoir <- ordinary$reservoir
  reservoir$source_package <- "exdqlm"
  reservoir$source_commit <- rqrgibbs:::.rqr_pinned_exdqlm_commit()
  source_response <- ordinary$y
  keep_idx <- seq_along(source_response)
  manifest <- rqrgibbs:::.rqr_desn_materialization_manifest(
    source_response = source_response,
    keep_idx = keep_idx,
    X = ordinary$X,
    y_fit = ordinary$y,
    feature_names = colnames(ordinary$X),
    reservoir_digest = reservoir$digest
  )
  builder$materialization_manifest <- manifest
  preliminary <- rqrgibbs:::rqr_desn_design(
    X = ordinary$X,
    y = ordinary$y,
    time_index = keep_idx,
    intercept = ordinary$feature_schema$intercept$name,
    builder = builder,
    reservoir = reservoir,
    driver = ordinary$driver,
    causal = ordinary$causal,
    time = list(),
    terminal = ordinary$terminal
  )
  attestation_path <- tempfile(fileext = ".rds")
  writeBin(attestation_bytes, attestation_path)
  receipt <- list(
    schema_version =
      rqrgibbs:::.rqr_desn_materialization_receipt_schema(),
    package = "exdqlm",
    package_version = builder$version,
    source_commit = builder$source_commit,
    source_tree_digest = strrep("a", 40L),
    runtime_tree_digest = strrep("b", 64L),
    runtime_attestation_schema = "rqrgibbs_runtime_attestation/5.0.0",
    runtime_attestation_sha256 = digest::digest(
      file = attestation_path, algo = "sha256", serialize = FALSE
    ),
    materializer_arguments_digest = arguments_digest,
    materialized_design_payload_digest = native_desn_v1_sha(
      rqrgibbs:::.rqr_desn_materialization_payload(preliminary)
    ),
    source_response_digest = manifest$source_response_digest,
    source_response_length = manifest$source_response_length,
    keep_idx_digest = manifest$keep_idx_digest,
    materialization_manifest_digest =
      native_desn_v1_sha(manifest),
    runtime_source_match = TRUE,
    reproducibility_eligible = TRUE
  )
  builder$materialization_receipt <- receipt
  design <- rqrgibbs:::rqr_desn_design(
    X = ordinary$X,
    y = ordinary$y,
    time_index = keep_idx,
    intercept = ordinary$feature_schema$intercept$name,
    builder = builder,
    reservoir = reservoir,
    driver = ordinary$driver,
    causal = ordinary$causal,
    time = list(),
    terminal = ordinary$terminal
  )
  state <- list(
    runtime_package = "exdqlm",
    runtime_package_version = receipt$package_version,
    git_commit = receipt$source_commit,
    expected_git_commit = receipt$source_commit,
    source_tree_digest = receipt$source_tree_digest,
    runtime_package_tree_digest = receipt$runtime_tree_digest,
    runtime_attestation_schema = receipt$runtime_attestation_schema,
    require_isolated_runtime = TRUE,
    runtime_attestation_match = TRUE,
    runtime_source_match = TRUE,
    reproducibility_eligible = TRUE
  )
  list(
    design = design, receipt = receipt,
    external_state = state,
    attestation_path = attestation_path
  )
}

native_desn_v1_text <- function(x) {
  out <- character(0)
  visit <- function(value) {
    if (is.character(value)) out <<- c(out, value[!is.na(value)])
    if (is.list(value)) {
      for (item in value) visit(item)
    }
    invisible(NULL)
  }
  visit(x)
  out
}

test_that("frozen-design ridge fit does not touch the exdqlm namespace", {
  testthat::local_mocked_bindings(
    qdesn_fit_vb = function(...) {
      stop("unexpected exdqlm design call", call. = FALSE)
    },
    .rqr_installed_namespace = function(...) {
      stop("unexpected exdqlm namespace call", call. = FALSE)
    },
    .package = "rqrgibbs"
  )

  design <- native_desn_v1_design()
  fit <- native_desn_v1_fit(design, prior = "ridge")

  expect_s3_class(fit, "rqr_desn_fit")
  expect_s3_class(fit$fit, "rqr_mcmc")
  expect_identical(fit$model_spec$family, "rqr_desn")
  expect_identical(fit$model_spec$embedding, "frozen_desn_design")
  expect_identical(fit$model_spec$design_engine, "frozen")
  expect_identical(
    fit$model_spec$design_semantic_digest,
    design$semantic_digest
  )
  expect_true(all(is.finite(fit$fit$samp.beta_root1)))
  expect_true(all(is.finite(fit$fit$samp.beta_root2)))
  expect_true(all(fit$summary$upper_mean >= fit$summary$lower_mean))
  expect_false(fit$meta$response_likelihood)
  expect_false(fit$meta$response_simulation)
  expect_true(fit$model_spec$design_contract_verified)
  expect_false(
    fit$model_spec$design_materialization_receipt_valid
  )
  expect_false(
    fit$model_spec$
      design_materialization_reproducibility_eligible
  )
  expect_false(fit$model_spec$reproducibility_eligible)
  expect_false(fit$model_spec$promotion_eligible)
  expect_identical(
    fit$fit$embedding_contract$materialization_verification$status,
    "custom_frozen_design_unattested"
  )
})

test_that("DESN MCMC controls reject typos and ambiguous nesting", {
  design <- native_desn_v1_design()
  default_rate <- rqrgibbs:::rqr_desn_fit(
    design = design,
    coverage_level = 0.8,
    design_engine = "frozen",
    mcmc_args = list(n_burn = 0L, n_mcmc = 1L, seed = 9203L)
  )
  expect_identical(
    default_rate$model_spec$learning_rate_mode,
    "fixed_rate"
  )

  expect_error(
    rqrgibbs:::rqr_desn_fit(
      design = design,
      coverage_level = 0.8,
      design_engine = "frozen",
      mcmc_args = list(n_mcm = 2L)
    ),
    "unsupported fields: n_mcm"
  )
  expect_error(
    rqrgibbs:::rqr_desn_fit(
      design = design,
      coverage_level = 0.8,
      design_engine = "frozen",
      mcmc_args = list(
        n_mcmc = 2L,
        mcmc_control = list(n_mcmc = 2L)
      )
    ),
    "cannot mix nested mcmc_control"
  )
  expect_error(
    rqrgibbs:::rqr_desn_fit(
      design = design,
      coverage_level = 0.8,
      design_engine = "frozen",
      mcmc_args = list(
        mcmc_control = list(n_mcm = 2L)
      )
    ),
    "unsupported fields: n_mcm"
  )
  for (field in c(
      "lambda_initial", "loss_reference_scale",
      "learning_rate_mode", "lambda_prior",
      "numerical_policy", "provenance_control"
    )) {
    value <- list(1)
    names(value) <- field
    expect_error(
      rqrgibbs:::rqr_desn_fit(
        design = design,
        coverage_level = 0.8,
        design_engine = "frozen",
        mcmc_args = value
      ),
      paste0("unsupported fields: ", field)
    )
  }
  expect_error(
    rqrgibbs:::rqr_desn_fit(
      design = design,
      coverage_level = 0.8,
      design_engine = "frozen",
      learning_rate_mode = "learned_pure"
    ),
    "accepts only fixed_rate"
  )
  expect_error(
    rqrgibbs:::rqr_desn_fit(
      design = design,
      coverage_level = 0.8,
      design_engine = "frozen",
      inference = "vb"
    ),
    "supports exact MCMC only"
  )
  expect_error(
    rqrgibbs:::rqr_desn_fit(
      design = design,
      coverage_level = 0.8,
      design_engine = "frozen",
      inference = c("mcmc", "vb")
    ),
    "supports exact MCMC only"
  )
  expect_error(
    rqrgibbs:::rqr_desn_fit(
      design = design,
      coverage_level = 0.8,
      design_engine = "frozen",
      mcmc_args = list(
        beta_prior_type = c("ridge", "rhs_ns"),
        n_burn = 0L, n_mcmc = 1L
      )
    ),
    "must be one prior name"
  )
  expect_error(
    rqrgibbs:::rqr_desn_fit(
      design = design,
      coverage_level = 0.8,
      design_engine = "frozen",
      mcmc_args = list(
        beta_prior_type = "ridge",
        beta_ridge_tau2 = 2,
        tau2 = 2,
        n_burn = 0L, n_mcmc = 1L
      )
    ),
    "cannot supply both beta_ridge_tau2"
  )
  expect_error(
    rqrgibbs:::rqr_desn_fit(
      design = design,
      coverage_level = 0.8,
      design_engine = "frozen",
      mcmc_args = list(
        beta_prior_type = "ridge",
        beta_rhs = list(tau0 = 0.5),
        n_burn = 0L, n_mcmc = 1L
      )
    ),
    "do not match beta_prior_type"
  )
  expect_error(
    rqrgibbs:::rqr_desn_fit(
      design = design,
      coverage_level = 0.8,
      design_engine = "frozen",
      mcmc_args = list(
        beta_prior_obj = rqrgibbs:::rqr_beta_prior(
          "ridge", ridge = list(tau2 = 2)
        ),
        beta_prior_type = "ridge",
        n_burn = 0L, n_mcmc = 1L
      )
    ),
    "cannot be combined"
  )
})

test_that("reference receipts bind payload and current isolated state", {
  fixture <- native_desn_v1_reference_design()
  status <- rqrgibbs:::.rqr_desn_materialization_receipt_status(
    fixture$design
  )
  verified <- rqrgibbs:::.rqr_desn_materialization_verification(
    fixture$design,
    external_state = fixture$external_state,
    runtime_attestation = fixture$attestation_path
  )

  expect_true(status$reference_materializer)
  expect_true(status$receipt_valid)
  expect_true(verified$external_state_match)
  expect_true(verified$runtime_attestation_sha256_verified)
  expect_true(verified$materialization_reproducibility_eligible)
  expect_identical(
    verified$status,
    "verified_current_isolated_materialization"
  )

  altered <- fixture$design
  altered$X[1L, 2L] <- altered$X[1L, 2L] + 0.1
  altered$digests <- rqrgibbs:::.rqr_desn_design_digests(
    rqrgibbs:::.rqr_desn_design_payload(altered)
  )
  altered$semantic_digest <- altered$digests$semantic
  expect_false(
    rqrgibbs:::.rqr_desn_materialization_receipt_status(
      altered
    )$receipt_valid
  )

  wrong_state <- fixture$external_state
  wrong_state$runtime_package_tree_digest <- strrep("c", 64L)
  mismatch <- rqrgibbs:::.rqr_desn_materialization_verification(
    fixture$design,
    external_state = wrong_state,
    runtime_attestation = fixture$attestation_path
  )
  expect_false(mismatch$external_state_match)
  expect_false(mismatch$materialization_reproducibility_eligible)
})

test_that("serialized reference designs remain usable but nonpromotable without current provenance", {
  fixture <- native_desn_v1_reference_design()
  fit <- native_desn_v1_fit(fixture$design)

  expect_true(
    fit$model_spec$design_materialization_receipt_valid
  )
  expect_false(
    fit$model_spec$
      design_materialization_external_binding_verified
  )
  expect_false(
    fit$model_spec$
      design_materialization_reproducibility_eligible
  )
  expect_false(fit$model_spec$promotion_eligible)
  expect_false(
    "exdqlm" %in%
      fit$fit$provenance$required_external_repositories
  )
})

test_that("frozen-design embedding is semantically bound in provenance", {
  design <- native_desn_v1_design()
  fit <- native_desn_v1_fit(design)
  embedding <- fit$fit$embedding_contract

  expect_identical(embedding$semantic_digest, design$semantic_digest)
  expect_identical(embedding$component_digests, design$digests)
  expect_identical(
    fit$fit$provenance$object_digests$embedding,
    rqrgibbs:::.rqr_digest(embedding)
  )
  expect_identical(
    fit$fit$checkpoint_state$target_digest,
    fit$fit$provenance$object_digests$target
  )
  expect_false(embedding$response_simulation)
})

test_that("frozen-design ordinary v1 supports missing responses exactly", {
  y <- native_desn_v1_design()$y
  y[c(3L, 9L)] <- NA_real_
  design <- native_desn_v1_design(y = y)
  fit <- native_desn_v1_fit(design, prior = "ridge")

  expect_identical(fit$fit$model_spec$n_total, length(y))
  expect_identical(fit$fit$model_spec$n_observed, length(y) - 2L)
  expect_identical(fit$fit$model_spec$missing_response_count, 2L)
  expect_identical(which(!fit$fit$data_contract$observed), c(3L, 9L))
  expect_true(all(is.finite(fit$summary$lower_mean)))
  expect_true(all(is.finite(fit$summary$upper_mean)))
})

test_that("native RHS-NS DESN fit uses the verified intercept contract", {
  design <- native_desn_v1_design()
  fit <- native_desn_v1_fit(
    design, prior = "rhs_ns",
    n_burn = 2L, n_mcmc = 4L
  )

  expect_identical(fit$fit$beta_prior$type, "rhs_ns")
  expect_identical(
    fit$fit$beta_prior$design_contract$intercept_name,
    "intercept"
  )
  expect_identical(
    fit$fit$beta_prior$design_contract$intercept_index,
    1L
  )
  expect_true(all(is.finite(fit$fit$samp.beta_root1)))
  expect_true(all(is.finite(fit$fit$samp.beta_root2)))
  expect_length(fit$fit$samp.beta_prior_state_root1, 4L)
  expect_length(fit$fit$samp.beta_prior_state_root2, 4L)

  no_intercept <- design
  no_intercept$feature_schema$intercept <- list(
    present = FALSE,
    index = NA_integer_,
    name = NA_character_,
    verified_constant_one = FALSE
  )
  no_intercept$digests <- rqrgibbs:::.rqr_desn_design_digests(
    rqrgibbs:::.rqr_desn_design_payload(no_intercept)
  )
  no_intercept$semantic_digest <- no_intercept$digests$semantic
  expect_error(
    native_desn_v1_fit(no_intercept, prior = "rhs_ns"),
    "require a declared intercept"
  )
})

test_that("DESN wrapper is transition-equivalent for named Gaussian priors", {
  design <- native_desn_v1_design()
  coefficient_names <- colnames(design$X)
  precision <- matrix(c(
    2.0, 0.15, -0.05,
    0.15, 1.6, 0.10,
    -0.05, 0.10, 1.3
  ), 3L, 3L, byrow = TRUE)
  dimnames(precision) <- list(
    coefficient_names, coefficient_names
  )
  prior <- rqrgibbs:::rqr_beta_prior(
    "gaussian",
    gaussian = list(
      mean = setNames(c(0.2, -0.1, 0.05), coefficient_names),
      precision = precision
    )
  )
  control <- list(
    n_burn = 1L, n_mcmc = 4L, thin = 1L,
    seed = 9204L, store_latent_draws = TRUE
  )

  wrapped <- rqrgibbs:::rqr_desn_fit(
    design = design,
    coverage_level = 0.8,
    design_engine = "frozen",
    inference = "mcmc",
    learning_rate_mode = "fixed_rate",
    mcmc_args = c(list(beta_prior_obj = prior), control)
  )
  direct <- rqrgibbs:::rqr_mcmc_fit(
    y = design$y,
    X = design$X,
    coverage_level = 0.8,
    learning_rate_mode = "fixed_rate",
    beta_prior_obj = prior,
    mcmc_control = control
  )

  expect_identical(wrapped$fit$beta_prior$type, "gaussian")
  expect_identical(
    wrapped$fit$beta_prior$coefficient_binding, "exact_names"
  )
  expect_identical(
    wrapped$fit$beta_prior$coefficient_names, coefficient_names
  )
  expect_identical(
    wrapped$fit$samp.beta_root1, direct$samp.beta_root1
  )
  expect_identical(
    wrapped$fit$samp.beta_root2, direct$samp.beta_root2
  )
  expect_identical(wrapped$fit$samp.lambda, direct$samp.lambda)
  expect_identical(
    wrapped$fit$samp.latent_v, direct$samp.latent_v
  )
  expect_identical(
    wrapped$fit$diagnostics,
    direct$diagnostics
  )
})

test_that("DESN wrapper is transition-equivalent for sampled RHS-NS", {
  design <- native_desn_v1_design()
  prior <- rqrgibbs:::rqr_beta_prior(
    "rhs_ns",
    rhs_ns = list(
      intercept_name = "intercept",
      tau0 = 0.5,
      a_zeta = 2,
      b_zeta = 1,
      intercept_precision = 0.25
    )
  )
  control <- list(
    n_burn = 1L, n_mcmc = 4L, thin = 1L,
    seed = 9205L, store_latent_draws = TRUE,
    store_prior_state_draws = TRUE
  )

  wrapped <- rqrgibbs:::rqr_desn_fit(
    design = design,
    coverage_level = 0.8,
    design_engine = "frozen",
    inference = "mcmc",
    learning_rate_mode = "fixed_rate",
    mcmc_args = c(list(beta_prior_obj = prior), control)
  )
  direct <- rqrgibbs:::rqr_mcmc_fit(
    y = design$y,
    X = design$X,
    coverage_level = 0.8,
    learning_rate_mode = "fixed_rate",
    beta_prior_obj = prior,
    mcmc_control = control
  )

  expect_identical(wrapped$fit$beta_prior$type, "rhs_ns")
  expect_null(wrapped$fit$beta_prior$hypers$zeta2_fixed)
  expect_identical(
    wrapped$fit$samp.beta_root1, direct$samp.beta_root1
  )
  expect_identical(
    wrapped$fit$samp.beta_root2, direct$samp.beta_root2
  )
  expect_identical(wrapped$fit$samp.lambda, direct$samp.lambda)
  expect_identical(
    wrapped$fit$samp.latent_v, direct$samp.latent_v
  )
  expect_identical(
    wrapped$fit$samp.beta_prior_state_root1,
    direct$samp.beta_prior_state_root1
  )
  expect_identical(
    wrapped$fit$samp.beta_prior_state_root2,
    direct$samp.beta_prior_state_root2
  )
  expect_identical(
    wrapped$fit$diagnostics,
    direct$diagnostics
  )
})

test_that("all future-design semantics predict interval-root functions", {
  fit <- native_desn_v1_fit()

  for (semantics in c(
    "precomputed_design",
    "teacher_forced_one_step",
    "external_driver_path"
  )) {
    future <- native_desn_v1_future(fit$design, semantics)
    pred <- predict_interval(
      fit, future_design = future, nd = 3L, seed = 9210L
    )
    forecast <- rqrgibbs:::forecast_paths.rqr_desn_fit(
      fit, H = 2L, future_design = future,
      nd = 3L, seed = 9210L
    )

    expect_identical(pred$future_semantics, semantics)
    expect_identical(
      pred$design_parent_digest,
      fit$design$semantic_digest
    )
    expect_false(pred$response_predictive_draws)
    expect_true(pred$future_contract_verified)
    expect_false(pred$legacy_future_matrix)
    expect_false(
      pred$
        parent_design_materialization_external_binding_verified
    )
    expect_false(pred$parent_fit_reproducibility_eligible)
    expect_false(pred$parent_fit_promotion_eligible)
    expect_false(pred$future_external_provenance_bound)
    expect_false(pred$future_reproducibility_eligible)
    expect_false(pred$reproducibility_eligible)
    expect_false(pred$promotion_eligible)
    expect_identical(
      pred$promotion_status,
      "verified_future_contract_unattested_materialization"
    )
    expect_true(all(pred$upper_draws >= pred$lower_draws))
    expect_identical(forecast$evaluation_semantics, semantics)
    expect_false(forecast$response_predictive_draws)
    expect_true(forecast$future_contract_verified)
    expect_false(forecast$legacy_future_matrix)
    expect_false(forecast$promotion_eligible)
    expect_true(all(forecast$upper_draws >= forecast$lower_draws))
    expect_identical(
      forecast$origin_fixed,
      isTRUE(future$driver$origin_fixed)
    )
    expect_error(
      rqrgibbs:::forecast_paths.rqr_desn_fit(
        fit, future_design = future,
        response_predictive_draws = TRUE
      ),
      "unsupported arguments: response_predictive_draws"
    )
  }
})

test_that("raw DESN future matrices are named legacy nonpromotion paths", {
  fit <- native_desn_v1_fit()
  X_future <- native_desn_v1_future_X()
  pred <- predict_interval(
    fit, X_new = X_future, nd = 2L, seed = 9211L
  )
  forecast <- rqrgibbs:::forecast_paths.rqr_desn_fit(
    fit, H = 2L, X_future = X_future,
    nd = 2L, seed = 9211L
  )

  expect_identical(
    pred$future_semantics, "legacy_explicit_matrix"
  )
  expect_false(pred$future_contract_verified)
  expect_true(pred$legacy_future_matrix)
  expect_false(pred$future_external_provenance_bound)
  expect_false(pred$future_reproducibility_eligible)
  expect_false(pred$reproducibility_eligible)
  expect_false(pred$promotion_eligible)
  expect_identical(
    pred$promotion_status,
    "legacy_explicit_matrix_nonpromotable"
  )
  expect_identical(
    forecast$evaluation_semantics,
    "legacy_explicit_matrix"
  )
  expect_true(is.na(forecast$origin_fixed))
  expect_false(forecast$promotion_eligible)

  unnamed <- unname(X_future)
  expect_error(
    predict_interval(fit, X_new = unnamed),
    "exact parent DESN feature names"
  )
  expect_error(
    rqrgibbs:::forecast_paths.rqr_desn_fit(
      fit, X_future = unnamed
    ),
    "exact parent DESN feature names"
  )
  renamed <- X_future
  colnames(renamed)[2L] <- "wrong_feature"
  expect_error(
    predict_interval(fit, X_new = renamed),
    "exact parent DESN feature names"
  )
})

test_that("DESN response, metadata, and reservoir boundaries fail closed", {
  design <- native_desn_v1_design()
  expect_error(
    rqrgibbs:::rqr_desn_fit(
      y = factor(design$y),
      design = design,
      design_engine = "frozen",
      coverage_level = 0.8
    ),
    "numeric vector"
  )
  expect_error(
    rqrgibbs:::rqr_desn_fit(
      y = matrix(design$y, ncol = 1L),
      design = design,
      design_engine = "frozen",
      coverage_level = 0.8
    ),
    "numeric vector"
  )
  expect_error(
    rqrgibbs:::rqr_desn_fit(
      y = design$y,
      coverage_level = 0.8,
      design_engine = "exdqlm_reference",
      design_metadata = list(buidler = list())
    ),
    "unsupported fields: buidler"
  )
  expect_error(
    rqrgibbs:::rqr_desn_fit(
      design$y, 0.8, 17,
      design_engine = "exdqlm_reference"
    ),
    "fully named"
  )
})

test_that("fit and forecast outputs make no response-simulation claim", {
  fit <- native_desn_v1_fit()
  future <- native_desn_v1_future(
    fit$design, "external_driver_path"
  )
  pred <- predict_interval(fit, future_design = future, nd = 2L)
  forecast <- rqrgibbs:::forecast_paths.rqr_desn_fit(
    fit, future_design = future, nd = 2L
  )

  expect_false(fit$meta$response_simulation)
  expect_false(fit$fit$embedding_contract$response_simulation)
  expect_false(pred$response_predictive_draws)
  expect_false(forecast$response_predictive_draws)
  text <- tolower(native_desn_v1_text(
    list(fit$note, pred$interpretation, forecast$interpretation)
  ))
  expect_false(any(grepl(
    "posterior[ _-]*predictive[ _-]*response|response[ _-]*simulation",
    text,
    perl = TRUE
  )))
})

test_that("DESN continuation preserves the design and exact transition stream", {
  design <- native_desn_v1_design()
  first <- native_desn_v1_fit(
    design, seed = 9220L, n_burn = 0L, n_mcmc = 2L
  )
  continued <- rqrgibbs:::rqr_desn_continue(
    first, n_mcmc = 2L
  )
  uninterrupted <- native_desn_v1_fit(
    design, seed = 9220L, n_burn = 0L, n_mcmc = 4L
  )

  expect_s3_class(continued, "rqr_desn_fit")
  expect_identical(
    continued$design$semantic_digest,
    first$design$semantic_digest
  )
  expect_identical(
    continued$fit$embedding_contract,
    first$fit$embedding_contract
  )
  expect_identical(
    rbind(
      first$fit$samp.beta_root1,
      continued$fit$samp.beta_root1
    ),
    uninterrupted$fit$samp.beta_root1
  )
  expect_identical(
    rbind(
      first$fit$samp.beta_root2,
      continued$fit$samp.beta_root2
    ),
    uninterrupted$fit$samp.beta_root2
  )
  expect_identical(
    c(first$fit$samp.lambda, continued$fit$samp.lambda),
    uninterrupted$fit$samp.lambda
  )
  expect_identical(
    continued$fit$continuation_contract$parent_checkpoint_digest,
    first$fit$checkpoint_digest
  )
  expect_identical(
    continued$meta,
    rqrgibbs:::.rqr_desn_outer_meta(continued$model_spec)
  )
  expect_identical(
    continued$model_spec$design_engine,
    continued$fit$embedding_contract$design_engine
  )
  expect_false(continued$model_spec$promotion_eligible)
})

test_that("DESN fit, future, and continuation reject semantic mutation", {
  design <- native_desn_v1_design()

  altered_design <- design
  altered_design$X[1L, 2L] <- altered_design$X[1L, 2L] + 0.25
  expect_error(
    native_desn_v1_fit(altered_design),
    "semantic digest mismatch"
  )

  fit <- native_desn_v1_fit(design)
  future <- native_desn_v1_future(design)
  future$X[1L, 2L] <- future$X[1L, 2L] + 0.25
  expect_error(
    predict_interval(fit, future_design = future),
    "semantic digest mismatch"
  )

  altered_embedding <- fit
  altered_embedding$fit$embedding_contract$semantic_digest <-
    native_desn_v1_sha("altered")
  expect_error(
    rqrgibbs:::rqr_desn_continue(
      altered_embedding, n_mcmc = 1L
    ),
    "target|digest|semantically invalid"
  )

  altered_envelope <- fit
  altered_envelope$design$X[1L, 2L] <-
    altered_envelope$design$X[1L, 2L] + 0.25
  expect_error(
    rqrgibbs:::rqr_desn_continue(
      altered_envelope, n_mcmc = 1L
    ),
    "semantic digest mismatch"
  )

  altered_duplicate <- fit
  altered_duplicate$X[1L, 2L] <-
    altered_duplicate$X[1L, 2L] + 0.25
  expect_error(
    predict_interval(
      altered_duplicate,
      future_design = native_desn_v1_future(design)
    ),
    "fit envelope"
  )

  altered_status <- fit
  altered_status$model_spec$promotion_eligible <- TRUE
  expect_error(
    predict_interval(
      altered_status,
      future_design = native_desn_v1_future(design)
    ),
    "fit envelope"
  )

  altered_meta <- fit
  altered_meta$meta$response_likelihood <- TRUE
  expect_error(
    rqrgibbs:::rqr_desn_continue(
      altered_meta, n_mcmc = 1L
    ),
    "fit envelope"
  )

  altered_summary <- fit
  altered_summary$summary$lower_mean[1L] <-
    altered_summary$summary$lower_mean[1L] + 1
  expect_error(
    predict_interval(
      altered_summary,
      future_design = native_desn_v1_future(design)
    ),
    "fit envelope"
  )
})

test_that("all DESN consumers validate the embedded static fit envelope", {
  fit <- native_desn_v1_fit()
  future <- native_desn_v1_future(fit$design)

  altered_terminal <- fit
  terminal_index <- nrow(
    altered_terminal$fit$samp.beta_root1
  )
  altered_terminal$fit$samp.beta_root1[
    terminal_index, 1L
  ] <- altered_terminal$fit$samp.beta_root1[
    terminal_index, 1L
  ] + 0.25
  expect_error(
    rqr_posterior_draws(altered_terminal),
    "terminal checkpoint roots|retained draws|content digests"
  )

  altered_schedule <- fit
  final_index <- length(
    altered_schedule$fit$segment_schedule_contract$segments
  )
  final_segment <-
    altered_schedule$fit$segment_schedule_contract$
      segments[[final_index]]
  final_segment$n_retained_draws <-
    final_segment$n_retained_draws + 1L
  final_segment$end_completed_iterations <-
    final_segment$end_completed_iterations + final_segment$thin
  altered_schedule$fit$segment_schedule_contract$
    segments[[final_index]] <- final_segment
  altered_schedule$fit$segment_schedule_digest <-
    rqrgibbs:::.rqr_digest(
      altered_schedule$fit$segment_schedule_contract
    )
  expect_error(
    predict_interval(
      altered_schedule, future_design = future
    ),
    "segment schedule"
  )

  altered_draw <- fit
  altered_draw$fit$samp.beta_root2[1L, 1L] <- NaN
  expect_error(
    print(altered_draw),
    "retained static root or lambda draws|retained draws|content digests"
  )
})

test_that("DESN draw APIs inherit strict static argument contracts", {
  fit <- native_desn_v1_fit()
  future <- native_desn_v1_future(fit$design)
  draws <- rqr_posterior_draws(fit, nd = 2L, seed = 9251L)

  expect_error(
    rqr_posterior_draws(fit, nd = 0L),
    "nd"
  )
  expect_error(
    rqr_posterior_draws(fit, seed = 9252L),
    "seed.*nd"
  )
  expect_error(
    rqr_posterior_draws(fit, typo = TRUE),
    "unsupported arguments: typo"
  )
  expect_error(
    predict_interval(
      fit, future_design = future,
      nd = 2L, draws = draws
    ),
    "draws.*nd|nd.*draws"
  )
  expect_error(
    predict_interval(
      fit, future_design = future,
      draws = draws, seed = 9253L
    ),
    "draws.*seed|seed.*draws"
  )
  expect_error(
    predict_interval(
      fit, future_design = future, typo = TRUE
    ),
    "unsupported arguments: typo"
  )

  invalid_draws <- draws
  invalid_draws$lambda[1L] <- -1
  invalid_draws$semantic_digest <- rqrgibbs:::.rqr_desn_sha256(
    invalid_draws[
      setdiff(names(invalid_draws), "semantic_digest")
    ]
  )
  expect_error(
    predict_interval(
      fit, future_design = future, draws = invalid_draws
    ),
    "lambda draws"
  )

  expect_error(
    print(fit, typo = TRUE),
    "unsupported arguments: typo"
  )
})
