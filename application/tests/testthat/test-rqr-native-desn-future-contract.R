native_desn_future_sha <- function(x) {
  digest::digest(x, algo = "sha256", serialize = TRUE)
}

native_desn_future_parent <- function(seed = 8101L) {
  X <- cbind(
    intercept = 1,
    h_last_001 = seq(-0.5, 0.5, length.out = 6)
  )
  rqrgibbs:::rqr_desn_design(
    X = X,
    y = sin(seq_len(6)),
    time_index = 21:26,
    intercept = 1L,
    builder = list(id = "reference_adapter", version = "1"),
    reservoir = list(
      digest = native_desn_future_sha(list(seed = seed, D = 1L)),
      seed = seed
    ),
    terminal = list(
      available = TRUE,
      state_digest = native_desn_future_sha(c(0.1, 0.2)),
      lag_buffer_digest = native_desn_future_sha(c(-0.2, -0.1))
    )
  )
}

native_desn_future_X <- function() {
  cbind(
    intercept = 1,
    h_last_001 = c(0.6, 0.7, 0.8)
  )
}

test_that("precomputed future design is linked to parent schema and reservoir", {
  parent <- native_desn_future_parent()
  future <- rqrgibbs:::rqr_desn_future_design(
    parent_design = parent,
    X = native_desn_future_X(),
    time_index = 27:29,
    semantics = "precomputed_design",
    driver = list(
      source = "recorded_feature_matrix",
      origin_fixed = TRUE,
      uses_realized_post_origin_history = FALSE
    )
  )

  expect_s3_class(future, "rqr_desn_future_design")
  expect_identical(future$semantics, "precomputed_design")
  expect_true(future$driver$origin_fixed)
  expect_false(future$driver$uses_realized_post_origin_history)
  expect_identical(
    future$parent$semantic_digest,
    parent$semantic_digest
  )
  expect_identical(
    future$feature_schema$feature_names,
    parent$feature_schema$feature_names
  )
  expect_true(future$verification$contract_verified)
  expect_false(future$verification$legacy_explicit_matrix)
  expect_false(
    future$verification$external_provenance_bound
  )
  expect_false(
    future$verification$promotion_evidence_complete
  )
  expect_false(future$verification$promotion_eligible)
  expect_identical(
    future$verification$promotion_status,
    "requires_verified_parent_fit_provenance"
  )
  expect_true(
    rqrgibbs:::rqr_validate_desn_future_design(
      future, parent_design = parent
    )
  )
})

test_that("teacher-forced semantics are rolling one-step rather than origin-fixed", {
  parent <- native_desn_future_parent()
  path_digest <- native_desn_future_sha(c(0.2, 0.3, 0.4))
  future <- rqrgibbs:::rqr_desn_future_design(
    parent_design = parent,
    X = native_desn_future_X(),
    time_index = 27:29,
    semantics = "teacher_forced_one_step",
    driver = list(path_digest = path_digest)
  )

  expect_false(future$driver$origin_fixed)
  expect_true(future$driver$uses_realized_post_origin_history)
  expect_identical(future$driver$evaluation_mode, "rolling_one_step")
  expect_false(future$driver$response_simulation)
  expect_true(rqrgibbs:::rqr_validate_desn_future_design(future, parent))

  expect_error(
    rqrgibbs:::rqr_desn_future_design(
      parent,
      native_desn_future_X(),
      27:29,
      semantics = "teacher_forced_one_step",
      driver = list(
        path_digest = path_digest,
        origin_fixed = TRUE
      )
    ),
    "origin-fixed"
  )
})

test_that("external driver paths are fixed at origin without realized leakage", {
  parent <- native_desn_future_parent()
  path_digest <- native_desn_future_sha(c(-0.1, 0, 0.1))
  future <- rqrgibbs:::rqr_desn_future_design(
    parent,
    native_desn_future_X(),
    27:29,
    semantics = "external_driver_path",
    driver = list(
      path_digest = path_digest,
      generator_id = "separate_companion_model"
    )
  )

  expect_true(future$driver$origin_fixed)
  expect_false(future$driver$uses_realized_post_origin_history)
  expect_identical(future$driver$path_digest, path_digest)
  expect_true(rqrgibbs:::rqr_validate_desn_future_design(future, parent))

  expect_error(
    rqrgibbs:::rqr_desn_future_design(
      parent,
      native_desn_future_X(),
      27:29,
      semantics = "external_driver_path",
      driver = list(
        path_digest = path_digest,
        uses_realized_post_origin_history = TRUE
      )
    ),
    "origin-fixed"
  )
})

test_that("future contracts reject leakage, response simulation, and mismatches", {
  parent <- native_desn_future_parent()
  X <- native_desn_future_X()

  expect_error(
    rqrgibbs:::rqr_desn_future_design(
      parent, X, 27:29,
      semantics = "precomputed_design",
      driver = list(
        origin_fixed = TRUE,
        uses_realized_post_origin_history = TRUE
      )
    ),
    "origin-fixed"
  )
  expect_error(
    rqrgibbs:::rqr_desn_future_design(
      parent, X, 27:29,
      semantics = "precomputed_design",
      driver = list(
        response_simulation = TRUE
      )
    ),
    "cannot enable"
  )
  expect_error(
    rqrgibbs:::rqr_desn_future_design(
      parent, X, 27:29,
      semantics = "precomputed_design",
      driver = list(
        note = "posterior predictive response draws"
      )
    ),
    "response-simulation language"
  )

  reordered <- X[, 2:1, drop = FALSE]
  expect_error(
    rqrgibbs:::rqr_desn_future_design(
      parent, reordered, 27:29,
      semantics = "precomputed_design"
    ),
    "must match"
  )
  wrong_reservoir <- parent$reservoir
  wrong_reservoir$digest <- native_desn_future_sha("different")
  expect_error(
    rqrgibbs:::rqr_desn_future_design(
      parent, X, 27:29,
      semantics = "precomputed_design",
      reservoir = wrong_reservoir
    ),
    "reservoir digest"
  )
  bad_intercept <- X
  bad_intercept[2, 1] <- 0
  expect_error(
    rqrgibbs:::rqr_desn_future_design(
      parent, bad_intercept, 27:29,
      semantics = "precomputed_design"
    ),
    "constant one"
  )
  expect_error(
    rqrgibbs:::rqr_desn_future_design(
      parent, X, 26:28,
      semantics = "precomputed_design"
    ),
    "strictly after"
  )
  expect_error(
    rqrgibbs:::rqr_desn_future_design(
      parent, X, 27:29,
      semantics = "precomputed_design",
      causal = list(
        uses_current_response = FALSE,
        uses_future_response = TRUE
      )
    ),
    "current or a future response"
  )
})

test_that("future semantic and parent links detect ordinary tampering", {
  parent <- native_desn_future_parent()
  future <- rqrgibbs:::rqr_desn_future_design(
    parent,
    native_desn_future_X(),
    27:29,
    semantics = "precomputed_design"
  )

  changed_X <- future
  changed_X$X[1, 2] <- changed_X$X[1, 2] + 0.5
  expect_error(
    rqrgibbs:::rqr_validate_desn_future_design(changed_X, parent),
    "semantic digest mismatch"
  )

  changed_parent <- future
  changed_parent$parent$semantic_digest <- native_desn_future_sha("other")
  expect_error(
    rqrgibbs:::rqr_validate_desn_future_design(changed_parent, parent),
    "does not match"
  )

  other_parent <- native_desn_future_parent(seed = 8102L)
  expect_error(
    rqrgibbs:::rqr_validate_desn_future_design(future, other_parent),
    "does not match"
  )

  changed_semantics <- future
  changed_semantics$semantics <- "teacher_forced_one_step"
  expect_error(
    rqrgibbs:::rqr_validate_desn_future_design(changed_semantics, parent),
    "path_digest|teacher_forced"
  )

  changed_verification <- future
  changed_verification$verification$promotion_eligible <- TRUE
  expect_error(
    rqrgibbs:::rqr_validate_desn_future_design(
      changed_verification, parent
    ),
    "verification metadata"
  )

  extra <- future
  extra$response_simulation <- TRUE
  expect_error(
    rqrgibbs:::rqr_validate_desn_future_design(extra, parent),
    "noncanonical top-level fields"
  )
})
