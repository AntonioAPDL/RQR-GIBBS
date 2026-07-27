native_desn_sha <- function(x) {
  digest::digest(x, algo = "sha256", serialize = TRUE)
}

native_desn_training_design <- function() {
  X <- cbind(
    intercept = 1,
    h_last_001 = seq(-0.8, 0.8, length.out = 8),
    reduced_h_001 = cos(seq_len(8) / 3)
  )
  rqrgibbs:::rqr_desn_design(
    X = X,
    y = sin(seq_len(8) / 4),
    time_index = 11:18,
    intercept = "intercept",
    builder = list(
      id = "pinned_exdqlm_frozen_design",
      version = "dffb71ee70b597d6a716ee74be1cbc99731cd453"
    ),
    reservoir = list(
      digest = native_desn_sha(list(seed = 7301L, D = 2L)),
      D = 2L,
      seed = 7301L
    ),
    driver = list(
      type = "observed_history",
      response_simulation = FALSE,
      history_digest = native_desn_sha(seq_len(18))
    ),
    causal = list(
      uses_current_response = FALSE,
      uses_future_response = FALSE,
      minimum_response_lag = 1L
    ),
    time = list(series_id = "fixture-d1"),
    terminal = list(
      available = TRUE,
      state_digest = native_desn_sha(matrix(1:4, 2, 2)),
      lag_buffer_digest = native_desn_sha(c(0.2, 0.1))
    )
  )
}

test_that("native frozen DESN design binds aligned data and semantic metadata", {
  design <- native_desn_training_design()

  expect_s3_class(design, "rqr_desn_design")
  expect_identical(design$schema_version, "rqrgibbs_desn_design/1.0.0")
  expect_identical(
    design$feature_schema$schema_version,
    "rqrgibbs_desn_feature_schema/1.0.0"
  )
  expect_identical(
    design$feature_schema$feature_names,
    c("intercept", "h_last_001", "reduced_h_001")
  )
  expect_true(design$feature_schema$intercept$verified_constant_one)
  expect_identical(design$feature_schema$intercept$index, 1L)
  expect_identical(design$time$start, 11)
  expect_identical(design$time$end, 18)
  expect_true(rqrgibbs:::rqr_validate_desn_design(design))
  expect_match(design$semantic_digest, "^[0-9a-f]{64}$")
  expect_identical(design$semantic_digest, design$digests$semantic)
  expect_named(
    design$digests,
    c(
      "X", "y", "time_index", "feature_schema", "builder", "reservoir",
      "driver", "causal", "time", "terminal", "semantic"
    )
  )
})

test_that("native frozen DESN design rejects alignment and schema ambiguity", {
  X <- cbind(intercept = 1, h = 1:4)
  common <- list(
    builder = list(id = "fixture", version = "1"),
    reservoir = list(digest = native_desn_sha("reservoir"))
  )

  expect_error(
    do.call(
      rqrgibbs:::rqr_desn_design,
      c(list(X = X, y = 1:3, time_index = 1:4), common)
    ),
    "aligned"
  )
  expect_error(
    do.call(
      rqrgibbs:::rqr_desn_design,
      c(list(X = X, y = 1:4, time_index = c(1, 2, 2, 4)), common)
    ),
    "strictly increasing"
  )
  expect_error(
    do.call(
      rqrgibbs:::rqr_desn_design,
      c(
        list(
          X = unname(X), y = 1:4, time_index = 1:4,
          feature_names = c("x", "x")
        ),
        common
      )
    ),
    "unique"
  )
  expect_error(
    do.call(
      rqrgibbs:::rqr_desn_design,
      c(
        list(
          X = X, y = 1:4, time_index = 1:4,
          feature_names = c("h", "intercept")
        ),
        common
      )
    ),
    "column order"
  )
  bad_intercept <- X
  bad_intercept[3, 1] <- 0.99
  expect_error(
    do.call(
      rqrgibbs:::rqr_desn_design,
      c(
        list(
          X = bad_intercept, y = 1:4, time_index = 1:4,
          intercept = "intercept"
        ),
        common
      )
    ),
    "exactly constant one"
  )
  expect_error(
    do.call(
      rqrgibbs:::rqr_desn_design,
      c(
        list(
          X = as.data.frame(X), y = 1:4,
          time_index = 1:4
        ),
        common
      )
    ),
    "numeric matrix"
  )
  expect_error(
    do.call(
      rqrgibbs:::rqr_desn_design,
      c(
        list(
          X = X, y = matrix(1:4, ncol = 1L),
          time_index = 1:4
        ),
        common
      )
    ),
    "numeric vector"
  )
  expect_error(
    do.call(
      rqrgibbs:::rqr_desn_design,
      c(
        list(
          X = unname(X), y = 1:4, time_index = 1:4,
          feature_names = factor(c("intercept", "h"))
        ),
        common
      )
    ),
    "feature_names"
  )
  expect_error(
    do.call(
      rqrgibbs:::rqr_desn_design,
      c(
        list(
          X = X, y = 1:4, time_index = 1:4,
          intercept = list(
            name = c("intercept", "h")
          )
        ),
        common
      )
    ),
    "one nonempty feature name"
  )
  expect_error(
    do.call(
      rqrgibbs:::rqr_desn_design,
      c(
        list(
          X = X, y = 1:4, time_index = 1:4,
          intercept = list(index = 1L, typo = "intercept")
        ),
        common
      )
    ),
    "unsupported fields"
  )
})

test_that("native frozen DESN metadata fail closed on unsafe claims", {
  X <- cbind(intercept = 1, h = 1:4)
  base <- list(
    X = X,
    y = 1:4,
    time_index = 1:4,
    intercept = "intercept",
    builder = list(id = "fixture", version = "1"),
    reservoir = list(digest = native_desn_sha("reservoir"))
  )

  expect_error(
    do.call(
      rqrgibbs:::rqr_desn_design,
      c(base, list(driver = list(type = "bad", response_simulation = TRUE)))
    ),
    "cannot enable"
  )
  expect_error(
    do.call(
      rqrgibbs:::rqr_desn_design,
      c(
        base,
        list(driver = list(
          type = "bad",
          response_simulation = FALSE,
          note = "simulate future response"
        ))
      )
    ),
    "response-simulation language"
  )
  expect_error(
    do.call(
      rqrgibbs:::rqr_desn_design,
      c(
        base,
        list(causal = list(
          uses_current_response = TRUE,
          uses_future_response = FALSE
        ))
      )
    ),
    "current or a future response"
  )
  unsafe_builder <- base
  unsafe_builder$builder <- list(
    id = "fixture", version = "1", callback = identity
  )
  expect_error(
    do.call(rqrgibbs:::rqr_desn_design, unsafe_builder),
    "non-plain|data only|not permitted"
  )
})

test_that("native frozen DESN semantic digests detect ordinary tampering", {
  design <- native_desn_training_design()

  altered_X <- design
  altered_X$X[1, 2] <- altered_X$X[1, 2] + 0.1
  expect_error(
    rqrgibbs:::rqr_validate_desn_design(altered_X),
    "semantic digest mismatch"
  )

  altered_builder <- design
  altered_builder$builder$version <- "different"
  expect_error(
    rqrgibbs:::rqr_validate_desn_design(altered_builder),
    "semantic digest mismatch"
  )

  altered_schema <- design
  altered_schema$feature_schema$feature_names[2] <- "renamed"
  expect_error(
    rqrgibbs:::rqr_validate_desn_design(altered_schema),
    "column order"
  )

  altered_intercept <- design
  altered_intercept$X[1, 1] <- 0
  expect_error(
    rqrgibbs:::rqr_validate_desn_design(altered_intercept),
    "constant one"
  )

  extra <- design
  extra$response_simulation <- TRUE
  expect_error(
    rqrgibbs:::rqr_validate_desn_design(extra),
    "noncanonical top-level fields"
  )
})
