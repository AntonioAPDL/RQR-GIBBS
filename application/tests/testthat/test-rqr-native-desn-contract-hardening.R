desn_hardening_sha <- function(x) {
  digest::digest(x, algo = "sha256", serialize = TRUE)
}

desn_hardening_design <- function(seed = 9601L) {
  X <- cbind(
    intercept = 1,
    reservoir_001 = seq(-0.7, 0.7, length.out = 9L),
    reservoir_002 = cos(seq_len(9L) / 4)
  )
  rqrgibbs:::rqr_desn_design(
    X = X,
    y = 0.2 + 0.3 * X[, 2L] - 0.1 * X[, 3L],
    time_index = 31:39,
    intercept = "intercept",
    builder = list(id = "hardening_fixture", version = "1.0.0"),
    reservoir = list(
      digest = desn_hardening_sha(list(seed = seed)),
      seed = seed
    ),
    driver = list(
      type = "observed_history",
      response_simulation = FALSE,
      history_digest = desn_hardening_sha(seq_len(39L))
    ),
    causal = list(
      uses_current_response = FALSE,
      uses_future_response = FALSE,
      minimum_response_lag = 1L
    )
  )
}

desn_hardening_fit <- function(seed = 9602L) {
  rqrgibbs:::rqr_desn_fit(
    design = desn_hardening_design(),
    design_engine = "frozen",
    coverage_level = 0.8,
    mcmc_args = list(
      beta_prior_type = "ridge",
      n_burn = 1L,
      n_mcmc = 4L,
      thin = 1L,
      seed = seed
    )
  )
}

desn_hardening_future <- function(design) {
  X <- design$X[c(nrow(design$X), nrow(design$X)), , drop = FALSE]
  X[, 2L] <- X[, 2L] + c(0.1, 0.2)
  rqrgibbs:::rqr_desn_future_design(
    parent_design = design,
    X = X,
    time_index = max(design$time_index) + 1:2,
    semantics = "precomputed_design",
    driver = list(
      source = "hardening_fixture",
      construction_digest = desn_hardening_sha(
        list(parent = design$semantic_digest, horizon = 2L)
      )
    )
  )
}

desn_hardening_reference_design <- function() {
  source_response <- seq(-1, 1, length.out = 12L)
  keep_idx <- 5:12
  X <- cbind(
    intercept = 1,
    reservoir_001 = source_response[keep_idx - 1L]
  )
  y_fit <- source_response[keep_idx]
  reservoir <- list(
    digest = desn_hardening_sha(list(seed = 9603L)),
    source_package = "exdqlm",
    source_commit = rqrgibbs:::.rqr_pinned_exdqlm_commit()
  )
  manifest <- rqrgibbs:::.rqr_desn_materialization_manifest(
    source_response = source_response,
    keep_idx = keep_idx,
    X = X,
    y_fit = y_fit,
    feature_names = colnames(X),
    reservoir_digest = reservoir$digest
  )
  builder <- list(
    id = rqrgibbs:::.rqr_desn_reference_builder_id(),
    version = "1.1.0",
    source_commit = rqrgibbs:::.rqr_pinned_exdqlm_commit(),
    arguments_digest = desn_hardening_sha(
      list(D = 1L, n = 1L, m = 1L, washout = 4L, seed = 9603L)
    ),
    adapter = "rqrgibbs_frozen_design_materializer/2.0.0",
    materialization_manifest = manifest
  )
  preliminary <- rqrgibbs:::rqr_desn_design(
    X = X,
    y = y_fit,
    time_index = keep_idx,
    intercept = "intercept",
    builder = builder,
    reservoir = reservoir
  )
  receipt <- list(
    schema_version =
      rqrgibbs:::.rqr_desn_materialization_receipt_schema(),
    package = "exdqlm",
    package_version = builder$version,
    source_commit = builder$source_commit,
    source_tree_digest = strrep("a", 40L),
    runtime_tree_digest = strrep("b", 64L),
    runtime_attestation_schema =
      "rqrgibbs_runtime_attestation/5.0.0",
    runtime_attestation_sha256 = strrep("c", 64L),
    materializer_arguments_digest = builder$arguments_digest,
    materialized_design_payload_digest = desn_hardening_sha(
      rqrgibbs:::.rqr_desn_materialization_payload(preliminary)
    ),
    source_response_digest = manifest$source_response_digest,
    source_response_length = manifest$source_response_length,
    keep_idx_digest = manifest$keep_idx_digest,
    materialization_manifest_digest =
      desn_hardening_sha(manifest),
    runtime_source_match = TRUE,
    reproducibility_eligible = TRUE
  )
  builder$materialization_receipt <- receipt
  rqrgibbs:::rqr_desn_design(
    X = X,
    y = y_fit,
    time_index = keep_idx,
    intercept = "intercept",
    builder = builder,
    reservoir = reservoir
  )
}

test_that("DESN designs, futures, and fit envelopes require exact classes and attributes", {
  design <- desn_hardening_design()
  future <- desn_hardening_future(design)
  fit <- desn_hardening_fit()

  expect_identical(
    class(design), c("rqr_desn_design", "list")
  )
  expect_identical(
    class(future), c("rqr_desn_future_design", "list")
  )
  expect_identical(
    class(fit), c("rqr_desn_fit", "rqr_fit")
  )
  expect_identical(
    class(fit$fit), c("rqr_mcmc", "rqr_fit")
  )

  bad <- design
  attr(bad, "foreign") <- TRUE
  expect_error(
    rqrgibbs:::rqr_validate_desn_design(bad),
    "exact canonical class and attributes"
  )
  bad <- future
  class(bad) <- c("hostile", class(bad))
  expect_error(
    rqrgibbs:::rqr_validate_desn_future_design(
      bad, design
    ),
    "exact canonical class and attributes"
  )
  bad <- fit
  attr(bad, "foreign") <- TRUE
  expect_error(
    rqrgibbs:::.rqr_validate_desn_fit_envelope(bad),
    "exact canonical class and attributes"
  )
  bad <- fit
  class(bad$fit) <- c("hostile", class(bad$fit))
  expect_error(
    rqrgibbs:::.rqr_validate_desn_fit_envelope(bad),
    "exact canonical class and attributes"
  )
})

test_that("reference materialization is strict, seeded, causal, and prefix-safe", {
  accepted <- rqrgibbs:::.rqr_desn_reference_materializer_arguments(
    list(
      D = 1L, n = 4L, n_tilde = integer(0), m = 2L,
      washout = 3L, seed = 9604L
    )
  )
  expect_identical(accepted$seed, 9604L)

  expect_error(
    rqrgibbs:::.rqr_desn_reference_materializer_arguments(
      list(m = 2L, weights = rep(1, 10), seed = 1L)
    ),
    "unsupported fields"
  )
  expect_error(
    rqrgibbs:::.rqr_desn_reference_materializer_arguments(
      list(m = 2L, standardize_inputs = TRUE, seed = 1L)
    ),
    "not prefix-safe"
  )
  expect_error(
    rqrgibbs:::.rqr_desn_reference_materializer_arguments(
      list(m = 2L, input_mode = "dlm_decomp_lags", seed = 1L)
    ),
    "raw_y_lags"
  )
  expect_error(
    rqrgibbs:::.rqr_desn_reference_materializer_arguments(
      list(m = 2L)
    ),
    "explicit seed"
  )
  expect_error(
    rqrgibbs:::.rqr_desn_reference_materializer_arguments(
      list(m = 2L, act_f = identity, seed = 1L)
    ),
    "data only|not permitted|plain"
  )
})

test_that("reference receipts bind the complete response and actual keep_idx manifest", {
  design <- desn_hardening_reference_design()
  manifest <- design$builder$materialization_manifest
  receipt <- design$builder$materialization_receipt
  status <- rqrgibbs:::.rqr_desn_materialization_receipt_status(
    design
  )

  expect_true(status$receipt_valid)
  expect_identical(manifest$keep_idx, 5:12)
  expect_identical(
    receipt$source_response_digest,
    manifest$source_response_digest
  )
  expect_identical(
    receipt$source_response_length, 12L
  )
  expect_identical(
    receipt$keep_idx_digest,
    desn_hardening_sha(5:12)
  )

  path <- tempfile(fileext = ".rds")
  saveRDS(design, path)
  restored <- readRDS(path)
  expect_true(rqrgibbs:::rqr_validate_desn_design(restored))
  expect_true(
    rqrgibbs:::.rqr_desn_materialization_receipt_status(
      restored
    )$receipt_valid
  )

  bad <- design
  bad$builder$materialization_manifest$keep_idx[1L] <- 4L
  bad$builder$materialization_manifest$keep_idx_digest <-
    desn_hardening_sha(
      bad$builder$materialization_manifest$keep_idx
    )
  bad$builder$materialization_receipt$keep_idx_digest <-
    bad$builder$materialization_manifest$keep_idx_digest
  bad$builder$materialization_receipt$
    materialization_manifest_digest <- desn_hardening_sha(
      bad$builder$materialization_manifest
    )
  payload <- rqrgibbs:::.rqr_desn_design_payload(bad)
  bad$digests <- rqrgibbs:::.rqr_desn_design_digests(payload)
  bad$semantic_digest <- bad$digests$semantic
  expect_error(
    rqrgibbs:::rqr_validate_desn_design(bad),
    "actual keep_idx|stored design payload"
  )
})

test_that("RHS-NS is bound to one and only one design-declared intercept", {
  design <- desn_hardening_design()
  expect_error(
    rqrgibbs:::rqr_desn_fit(
      design = design,
      design_engine = "frozen",
      coverage_level = 0.8,
      mcmc_args = list(
        beta_prior_type = "rhs_ns",
        beta_rhs = list(intercept_name = "reservoir_001"),
        n_burn = 1L, n_mcmc = 1L, seed = 9605L
      )
    ),
    "must exactly equal"
  )

  X <- cbind(intercept = 1, duplicate = 1, h = 1:5)
  expect_error(
    rqrgibbs:::rqr_desn_design(
      X = X, y = 1:5, time_index = 1:5,
      intercept = "intercept",
      builder = list(id = "duplicate", version = "1"),
      reservoir = list(digest = desn_hardening_sha("duplicate"))
    ),
    "exactly one constant-one"
  )
})

test_that("future driver types and causal fields are exact semantic contracts", {
  design <- desn_hardening_design()
  X <- design$X[1:2, , drop = FALSE]

  expect_error(
    rqrgibbs:::rqr_desn_future_design(
      design, X, 40:41,
      semantics = "precomputed_design",
      driver = list(type = "external_driver_path")
    ),
    "precomputed_design semantics"
  )
  expect_error(
    rqrgibbs:::rqr_desn_future_design(
      design, X, 40:41,
      semantics = "precomputed_design",
      causal = list(prefix_safe = FALSE)
    ),
    "prefix-safe"
  )
  expect_error(
    rqrgibbs:::rqr_desn_future_design(
      design, X, 40:41,
      semantics = "precomputed_design",
      causal = list(minimum_response_lag = 0L)
    ),
    "positive integer"
  )
  expect_error(
    rqrgibbs:::rqr_desn_future_design(
      design, X, 40:41,
      semantics = "precomputed_design",
      causal = list(
        contract =
          "row_t_uses_only_information_available_before_t"
      )
    ),
    "canonical strict-prefix"
  )

  external <- rqrgibbs:::rqr_desn_future_design(
    design, X, 40:41,
    semantics = "external_driver_path",
    driver = list(
      path_digest = desn_hardening_sha(c(0.1, 0.2)),
      generator_id = "external_fixture"
    )
  )
  expect_identical(
    external$driver$type, "external_driver_path"
  )
  expect_identical(
    external$driver$evaluation_mode,
    "origin_fixed_external_path"
  )
  expect_true(external$causal$prefix_safe)
})

test_that("DESN draws and predictions are typed, digested, and source-bound", {
  fit <- desn_hardening_fit()
  other <- desn_hardening_fit(seed = 9606L)
  future <- desn_hardening_future(fit$design)
  draws <- rqrgibbs:::rqr_posterior_draws.rqr_desn_fit(
    fit, nd = 2L, seed = 9607L
  )

  expect_identical(
    class(draws), c("rqr_desn_draws", "list")
  )
  expect_identical(
    names(attributes(draws)), c("names", "class")
  )
  expect_true(draws$source_bound)
  expect_false(draws$response_predictive_draws)
  expect_true(
    rqrgibbs:::.rqr_validate_desn_draws(fit, draws)
  )
  expect_error(
    rqrgibbs:::.rqr_validate_desn_draws(other, draws),
    "identify|source fit"
  )
  attributed_draws <- draws
  attr(attributed_draws, "foreign") <- TRUE
  expect_error(
    rqrgibbs:::.rqr_validate_desn_draws(
      fit, attributed_draws
    ),
    "exact canonical class and attributes"
  )

  bare <- list(
    beta_root1 = draws$beta_root1,
    beta_root2 = draws$beta_root2
  )
  expect_error(
    rqrgibbs:::predict_interval.rqr_desn_fit(
      fit, future_design = future, draws = bare
    ),
    "source-bound"
  )
  bad_draws <- draws
  bad_draws$source$design_semantic_digest <-
    desn_hardening_sha("foreign design")
  bad_draws$semantic_digest <- desn_hardening_sha(
    bad_draws[
      setdiff(names(bad_draws), "semantic_digest")
    ]
  )
  expect_error(
    rqrgibbs:::.rqr_validate_desn_draws(fit, bad_draws),
    "source fit"
  )

  prediction <- rqrgibbs:::predict_interval.rqr_desn_fit(
    fit, future_design = future, draws = draws
  )
  expect_identical(
    class(prediction), c("rqr_desn_prediction", "list")
  )
  expect_identical(
    names(attributes(prediction)), c("names", "class")
  )
  expect_false(prediction$response_predictive_draws)
  expect_false(prediction$promotion_eligible)
  expect_true(
    rqrgibbs:::.rqr_validate_desn_prediction(
      fit, prediction
    )
  )
  attributed_prediction <- prediction
  attr(attributed_prediction, "foreign") <- TRUE
  expect_error(
    rqrgibbs:::.rqr_validate_desn_prediction(
      fit, attributed_prediction
    ),
    "exact canonical class and attributes"
  )

  bad_prediction <- prediction
  bad_prediction$lower_draws[1L, 1L] <-
    bad_prediction$lower_draws[1L, 1L] + 1
  bad_prediction$semantic_digest <- desn_hardening_sha(
    bad_prediction[
      setdiff(names(bad_prediction), "semantic_digest")
    ]
  )
  expect_error(
    rqrgibbs:::.rqr_validate_desn_prediction(
      fit, bad_prediction
    ),
    "roots|inconsistent"
  )

  forecast <- rqrgibbs:::forecast_paths.rqr_desn_fit(
    fit, H = 2L, future_design = future,
    nd = 2L, seed = 9607L
  )
  expect_identical(
    class(forecast), c("rqr_desn_prediction", "list")
  )
  expect_identical(forecast$evaluation_api, "forecast_paths")
  expect_true(
    rqrgibbs:::.rqr_validate_desn_prediction(
      fit, forecast
    )
  )
})

test_that("validated DESN consumers use direct implementations without redispatch", {
  posterior_body <- paste(
    deparse(body(
      rqrgibbs:::rqr_posterior_draws.rqr_desn_fit
    )),
    collapse = "\n"
  )
  prediction_body <- paste(
    deparse(body(
      rqrgibbs:::predict_interval.rqr_desn_fit
    )),
    collapse = "\n"
  )
  forecast_body <- paste(
    deparse(body(
      rqrgibbs:::forecast_paths.rqr_desn_fit
    )),
    collapse = "\n"
  )

  expect_false(grepl("UseMethod", posterior_body, fixed = TRUE))
  expect_false(grepl("rqr_posterior_draws(", posterior_body, fixed = TRUE))
  expect_false(grepl("UseMethod", prediction_body, fixed = TRUE))
  expect_false(grepl("predict_interval(", prediction_body, fixed = TRUE))
  expect_false(grepl("UseMethod", forecast_body, fixed = TRUE))
  expect_false(grepl("forecast_paths(", forecast_body, fixed = TRUE))
})
