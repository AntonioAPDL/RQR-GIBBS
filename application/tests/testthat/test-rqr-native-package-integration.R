.ordinary_v1_supported_exports <- c(
  "forecast_paths",
  "predict_interval",
  "rqr_as_dlm_model",
  "rqr_beta_prior",
  "rqr_check_loss",
  "rqr_constants",
  "rqr_desn_continue",
  "rqr_desn_design",
  "rqr_desn_fit",
  "rqr_desn_future_design",
  "rqr_discount_matrix",
  "rqr_dlm_continue",
  "rqr_dlm_fit",
  "rqr_evolution_component_scale",
  "rqr_evolution_fixed",
  "rqr_ffbs_sample",
  "rqr_ffbs_smooth",
  "rqr_forecast_roots",
  "rqr_freeze_discount_template",
  "rqr_gig_params",
  "rqr_mcmc_continue",
  "rqr_mcmc_fit",
  "rqr_oracle_certificate",
  "rqr_oracle_endpoints",
  "rqr_oracle_risk",
  "rqr_oracle_roots",
  "rqr_order_endpoints",
  "rqr_polytrend",
  "rqr_posterior_draws",
  "rqr_pseudo_residual",
  "rqr_regression",
  "rqr_residual_product",
  "rqr_sample_gig_half",
  "rqr_seasonal",
  "rqr_validate_desn_design",
  "rqr_validate_desn_future_design"
)

.ordinary_v1_compatibility_exports <- "beta_prior"

.ordinary_v1_experimental_exports <- "rqr_evolution_adaptive_working"

test_that("public exports form an explicit supported/compatibility/experimental partition", {
  exports <- getNamespaceExports("rqrgibbs")
  groups <- list(
    supported = .ordinary_v1_supported_exports,
    compatibility = .ordinary_v1_compatibility_exports,
    experimental = .ordinary_v1_experimental_exports
  )
  classified <- unlist(groups, use.names = FALSE)

  expect_identical(anyDuplicated(classified), 0L)
  expect_setequal(exports, classified)
  expect_false("rqr_vb_fit" %in% exports)
  expect_true(exists(
    "rqr_vb_fit",
    envir = asNamespace("rqrgibbs"),
    inherits = FALSE
  ))
})

test_that("ordinary-v1 S3 methods and native registrations are exact", {
  expected_s3 <- list(
    c("+", "rqr_dlm_model"),
    c("forecast_paths", "rqr_desn_fit"),
    c("predict_interval", "rqr_desn_fit"),
    c("predict_interval", "rqr_dlm_mcmc"),
    c("predict_interval", "rqr_mcmc"),
    c("predict_interval", "rqr_vb"),
    c("print", "rqr_desn_fit"),
    c("print", "rqr_dlm_mcmc"),
    c("print", "rqr_mcmc"),
    c("print", "rqr_vb"),
    c("rqr_posterior_draws", "rqr_desn_fit"),
    c("rqr_posterior_draws", "rqr_dlm_mcmc"),
    c("rqr_posterior_draws", "rqr_mcmc"),
    c("rqr_posterior_draws", "rqr_vb")
  )
  registered <- vapply(expected_s3, function(method) {
    is.function(getS3method(method[[1L]], method[[2L]], optional = TRUE))
  }, logical(1L))
  expect_true(
    all(registered),
    info = paste(vapply(
      expected_s3[!registered],
      paste,
      collapse = ".",
      FUN.VALUE = character(1L)
    ), collapse = ", ")
  )

  routines <- getDLLRegisteredRoutines(
    getLoadedDLLs()[["rqrgibbs"]]
  )$.Call
  expected_arity <- c(
    `_rqrgibbs_rqr_mvn_draw_cpp` = 4L,
    `_rqrgibbs_rqr_filter_log_marginal_cpp` = 7L,
    `_rqrgibbs_rqr_ffbs_cpp` = 13L,
    `_rqrgibbs_rqr_noncentered_basis_cpp` = 5L
  )
  observed_arity <- vapply(
    routines,
    function(routine) routine$numParameters,
    integer(1L)
  )
  expect_identical(observed_arity[sort(names(observed_arity))],
                   expected_arity[sort(names(expected_arity))])
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

test_that("ordinary-v1 fit and public-output schemas are exact", {
  namespace <- asNamespace("rqrgibbs")
  schema <- function(name) {
    get(name, envir = namespace, inherits = FALSE)()
  }

  expected <- c(
    .rqr_static_fit_schema =
      "rqrgibbs_static_fit/1.2.0",
    .rqr_static_checkpoint_schema =
      "rqrgibbs_static_checkpoint/1.0.0",
    .rqr_static_schedule_schema =
      "rqrgibbs_static_segment_schedule/2.0.0",
    .rqr_static_draws_schema =
      "rqrgibbs_static_draws/1.0.0",
    .rqr_static_draw_source_schema =
      "rqrgibbs_static_draw_source/1.0.0",
    .rqr_static_draw_selection_schema =
      "rqrgibbs_static_draw_selection/1.0.0",
    .rqr_static_prediction_schema =
      "rqrgibbs_interval_prediction/2.0.0",
    .rqr_static_prediction_source_schema =
      "rqrgibbs_static_prediction_source/1.0.0",
    .rqr_desn_design_schema =
      "rqrgibbs_desn_design/1.1.0",
    .rqr_desn_feature_schema =
      "rqrgibbs_desn_feature_schema/1.0.0",
    .rqr_desn_fit_schema =
      "rqrgibbs_desn_fit/1.2.0",
    .rqr_desn_draws_schema =
      "rqrgibbs_desn_draws/1.0.0",
    .rqr_desn_prediction_schema =
      "rqrgibbs_desn_prediction/1.0.0",
    .rqr_desn_future_design_schema =
      "rqrgibbs_desn_future_design/1.2.0",
    .rqr_desn_future_verification_schema =
      "rqrgibbs_desn_future_verification/1.1.0",
    .rqr_desn_materialization_receipt_schema =
      "rqrgibbs_desn_materialization_receipt/3.0.0",
    .rqr_desn_materialization_manifest_schema =
      "rqrgibbs_desn_materialization_manifest/1.0.0",
    .rqr_desn_materialization_verification_schema =
      "rqrgibbs_desn_materialization_verification/1.1.0",
    .rqr_schema_version =
      "rqrgibbs_fit/1.14.0",
    .rqr_dlm_schedule_schema =
      "rqrgibbs_dlm_segment_schedule/2.0.0",
    .rqr_dlm_draws_schema =
      "rqrgibbs_dlm_draws/1.0.0",
    .rqr_dlm_draw_source_schema =
      "rqrgibbs_dlm_draw_source/1.0.0",
    .rqr_dlm_rng_binding_schema =
      "rqrgibbs_dlm_rng_binding/1.0.0",
    .rqr_dlm_prediction_schema =
      "rqrgibbs_dlm_prediction/1.0.0",
    .rqr_dlm_forecast_schema =
      "rqrgibbs_dlm_forecast/1.0.0",
    .rqr_dlm_forecast_source_schema =
      "rqrgibbs_dlm_forecast_source/1.0.0",
    .rqr_dlm_future_contract_schema =
      "rqrgibbs_dlm_future_contract/1.0.0",
    .rqr_continuation_history_schema =
      "rqrgibbs_continuation_history/5.0.0"
  )

  observed <- vapply(names(expected), schema, character(1L))
  expect_identical(observed, expected)

  desn_draw_source <- get(
    ".rqr_desn_draw_source",
    envir = namespace,
    inherits = FALSE
  )(list(
    fit = list(
      checkpoint_digest = strrep("a", 64L),
      retained_draws_digest = strrep("b", 64L),
      checkpoint_state = list(target_digest = strrep("c", 64L)),
      embedding_contract = list(kind = "frozen_design")
    ),
    design = list(semantic_digest = strrep("d", 64L))
  ))
  expect_identical(
    desn_draw_source$schema_version,
    "rqrgibbs_desn_draw_source/1.0.0"
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

test_that("every classified export and the model-composition method has help", {
  topics <- c(
    .ordinary_v1_supported_exports,
    .ordinary_v1_compatibility_exports,
    .ordinary_v1_experimental_exports,
    "Ops.rqr_dlm_model",
    "rqr_vb_fit",
    "rqrgibbs-package"
  )
  available <- vapply(topics, function(topic) {
    length(utils::help(topic, package = "rqrgibbs")) > 0L
  }, logical(1L))

  expect_true(all(available), info = paste(topics[!available], collapse = ", "))
})

test_that("DLM transition-kernel help names all controls and their defaults", {
  help_file <- utils:::.getHelpFile(
    utils::help("rqr_dlm_fit", package = "rqrgibbs")
  )
  help_text <- paste(
    strsplit(
      paste(capture.output(tools::Rd2txt(help_file)), collapse = " "),
      "[[:space:]]+"
    )[[1L]],
    collapse = " "
  )
  help_text <- gsub("'", "", help_text, fixed = TRUE)
  documented_defaults <- c(
    component_scale_collapsed_update = "FALSE",
    component_scale_interweave = "FALSE",
    component_scale_slice_width = "1",
    component_scale_slice_max_steps = "100",
    component_scale_slice_max_shrink = "1000",
    component_scale_slice_sweeps = "1",
    component_scale_interweave_cycles = "1"
  )
  for (control in names(documented_defaults)) {
    expect_match(
      help_text,
      paste0(control, " (", documented_defaults[[control]], ")"),
      fixed = TRUE,
      info = control
    )
  }

  kernel_constructor <- get(
    ".rqr_dlm_transition_kernel_contract",
    envir = asNamespace("rqrgibbs"),
    inherits = FALSE
  )
  kernel <- kernel_constructor(
    evolution_mode = "component_scale",
    learning_rate_mode = "fixed_rate",
    component_names = c("level", "regression"),
    component_scale_collapsed_update = TRUE,
    component_scale_interweave = TRUE
  )
  expect_identical(kernel$collapsed_slice_width, 1)
  expect_identical(kernel$collapsed_slice_sweeps, 1L)
  expect_identical(kernel$collapsed_slice_max_steps, 100L)
  expect_identical(kernel$collapsed_slice_max_shrink, 1000L)
  expect_identical(kernel$interweave_cycles, 1L)
  expect_identical(kernel$global_root_swap_probability, 0.5)
})
