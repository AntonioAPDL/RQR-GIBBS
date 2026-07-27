test_that("ordinary-v1 public API and legacy DLM API are package wired", {
  exports <- getNamespaceExports("rqrgibbs")
  ordinary_exports <- c(
    "rqr_beta_prior",
    "rqr_mcmc_fit",
    "rqr_mcmc_continue",
    "rqr_desn_design",
    "rqr_validate_desn_design",
    "rqr_desn_future_design",
    "rqr_validate_desn_future_design",
    "rqr_desn_fit",
    "rqr_desn_continue",
    "forecast_paths"
  )
  dlm_exports <- c(
    "rqr_as_dlm_model",
    "rqr_dlm_fit",
    "rqr_dlm_continue",
    "rqr_forecast_roots",
    "rqr_ffbs_sample",
    "rqr_ffbs_smooth"
  )

  expect_setequal(intersect(exports, ordinary_exports), ordinary_exports)
  expect_setequal(intersect(exports, dlm_exports), dlm_exports)
  expect_true(is.function(
    getS3method("forecast_paths", "rqr_desn_fit", optional = TRUE)
  ))
  expect_true(is.function(
    getS3method("predict_interval", "rqr_mcmc", optional = TRUE)
  ))
  expect_true(is.function(
    getS3method("predict_interval", "rqr_dlm_mcmc", optional = TRUE)
  ))
})

test_that("ordinary-v1 dependency boundary leaves exdqlm optional", {
  description <- packageDescription("rqrgibbs")
  imports <- trimws(strsplit(description$Imports, ",", fixed = TRUE)[[1L]])
  suggests <- trimws(strsplit(description$Suggests, ",", fixed = TRUE)[[1L]])

  imports <- sub("\\s*\\(.*$", "", imports)
  suggests <- sub("\\s*\\(.*$", "", suggests)
  expect_false("exdqlm" %in% imports)
  expect_true("exdqlm" %in% suggests)
  expect_match(
    description$Description,
    "not a response likelihood",
    fixed = TRUE
  )
})

test_that("ordinary-v1 schemas are additive to the DLM fit schema", {
  namespace <- asNamespace("rqrgibbs")
  schema <- function(name) {
    get(name, envir = namespace, inherits = FALSE)()
  }

  expect_identical(
    schema(".rqr_static_fit_schema"),
    "rqrgibbs_static_fit/1.0.0"
  )
  expect_identical(
    schema(".rqr_static_checkpoint_schema"),
    "rqrgibbs_static_checkpoint/1.0.0"
  )
  expect_identical(
    schema(".rqr_desn_fit_schema"),
    "rqrgibbs_desn_fit/1.1.0"
  )
  expect_identical(
    schema(".rqr_desn_future_design_schema"),
    "rqrgibbs_desn_future_design/1.1.0"
  )
  expect_identical(
    schema(".rqr_desn_materialization_receipt_schema"),
    "rqrgibbs_desn_materialization_receipt/2.0.0"
  )
  expect_identical(schema(".rqr_schema_version"), "rqrgibbs_fit/1.11.0")
  expect_identical(
    schema(".rqr_continuation_history_schema"),
    "rqrgibbs_continuation_history/4.1.0"
  )
})

test_that("installed/source static continuation accepts absent repository metadata", {
  X <- cbind(
    "(Intercept)" = 1,
    x = seq(-1, 1, length.out = 6L)
  )
  fit <- rqr_mcmc_fit(
    y = seq(-0.4, 0.5, length.out = 6L),
    X = X,
    coverage_level = 0.8,
    mcmc_control = list(
      n_burn = 0L, n_mcmc = 2L, thin = 1L, seed = 1901L
    )
  )
  fit$provenance$repo_root <- NA_character_
  fit$provenance$expected_git_commit <- NA_character_
  fit$provenance$primary_runtime_attestation <- NA_character_

  continued <- rqr_mcmc_continue(fit, n_mcmc = 1L)
  expect_s3_class(continued, "rqr_mcmc")
  expect_identical(
    continued$checkpoint_state$completed_iterations, 3L
  )
  expect_length(
    continued$continuation_contract$environment_mismatches, 0L
  )
})

test_that("ordinary-v1 exported help topics are installed", {
  topics <- c(
    "rqr_beta_prior",
    "rqr_mcmc_fit",
    "rqr_mcmc_continue",
    "rqr_desn_design",
    "rqr_desn_future_design",
    "rqr_desn_fit",
    "rqr_desn_continue",
    "forecast_paths"
  )
  available <- vapply(topics, function(topic) {
    length(utils::help(topic, package = "rqrgibbs")) > 0L
  }, logical(1L))

  expect_true(all(available), info = paste(topics[!available], collapse = ", "))
})
