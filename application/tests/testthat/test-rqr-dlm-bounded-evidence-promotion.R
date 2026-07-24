test_that("bounded promotion requires the externally frozen bundle", {
  helper <- testthat::test_path(
    "..", "..", "scripts", "lib", "rqr_dlm_evidence_promotion.R"
  )
  env <- new.env(parent = globalenv())
  sys.source(helper, envir = env)
  expected <- list(
    schema_version = env$rqr_bounded_expected_bundle_schema(),
    expected = as.list(stats::setNames(
      paste0("value_", seq_along(env$rqr_bounded_required_bundle_fields())),
      env$rqr_bounded_required_bundle_fields()
    ))
  )
  run <- expected$expected
  toolchain <- list(
    digest = run$runtime_toolchain_digest,
    primary_runtime_tree_digest = run$primary_runtime_tree_digest,
    primary_runtime_attestation_sha256 =
      run$primary_runtime_attestation_sha256
  )
  expect_invisible(
    env$rqr_bounded_validate_expected_bundle(run, toolchain, expected)
  )
  altered <- expected
  altered$expected$config_digest <- paste(rep("0", 64), collapse = "")
  expect_error(
    env$rqr_bounded_validate_expected_bundle(run, toolchain, altered),
    "config_digest"
  )
  omitted <- expected
  omitted$expected$primary_commit <- NULL
  expect_error(
    env$rqr_bounded_validate_expected_bundle(run, toolchain, omitted),
    "exactly the frozen fields"
  )
})

test_that("bounded promotion requires exact unique fit-ID sets", {
  helper <- testthat::test_path(
    "..", "..", "scripts", "lib", "rqr_dlm_evidence_promotion.R"
  )
  env <- new.env(parent = globalenv())
  sys.source(helper, envir = env)
  plan <- data.frame(
    fixture_id = rep("fixture", 24L),
    learning_rate_mode = rep("fixed", 24L),
    chain = seq_len(24L),
    seed = 100L + seq_len(24L),
    fit_id = sprintf("fit%02d", seq_len(24L)),
    stringsAsFactors = FALSE
  )
  ids <- env$rqr_bounded_expected_fit_ids(plan)
  tables <- list(
    fit_audit = data.frame(fit_id = plan$fit_id),
    run_status = data.frame(fit_id = rev(plan$fit_id))
  )
  expect_invisible(env$rqr_bounded_require_exact_fit_id_sets(ids, tables))

  omitted <- tables
  omitted$run_status <- omitted$run_status[-1L, , drop = FALSE]
  expect_error(
    env$rqr_bounded_require_exact_fit_id_sets(ids, omitted),
    "run_status"
  )
  duplicated <- tables
  duplicated$fit_audit$fit_id[[1L]] <- duplicated$fit_audit$fit_id[[2L]]
  expect_error(
    env$rqr_bounded_require_exact_fit_id_sets(ids, duplicated),
    "fit_audit"
  )
  extra <- tables
  extra$run_status$fit_id[[1L]] <- "unexpected"
  expect_error(
    env$rqr_bounded_require_exact_fit_id_sets(ids, extra),
    "run_status"
  )
})

test_that("bounded promotion recomputes state and history digests", {
  helper <- testthat::test_path(
    "..", "..", "scripts", "lib", "rqr_dlm_evidence_promotion.R"
  )
  env <- new.env(parent = globalenv())
  sys.source(helper, envir = env)
  fit <- structure(
    list(
      checkpoint_state = list(iteration = 6L, value = 1),
      continuation_history_contract = list(
        schema_version = "test", segments = list(list(generation = 0L))
      )
    ),
    class = c("rqr_dlm_mcmc", "list")
  )
  fit$checkpoint_digest <- digest::digest(
    fit$checkpoint_state, algo = "sha256", serialize = TRUE
  )
  fit$continuation_history_digest <- digest::digest(
    fit$continuation_history_contract, algo = "sha256", serialize = TRUE
  )
  checkpoint <- data.frame(
    checkpoint_digest = fit$checkpoint_digest,
    history_digest = fit$continuation_history_digest,
    published_object_digest = digest::digest(
      fit, algo = "sha256", serialize = TRUE
    ),
    stringsAsFactors = FALSE
  )
  local <- checkpoint[c("checkpoint_digest", "history_digest")]
  validator <- function(object) list(valid = TRUE)
  expect_invisible(env$rqr_bounded_validate_reopened_fit(
    fit, checkpoint, local, continuation_validator = validator
  ))

  altered <- fit
  altered$checkpoint_state$value <- 2
  expect_error(
    env$rqr_bounded_validate_reopened_fit(
      altered, checkpoint, local, continuation_validator = validator
    ),
    "checkpoint-state"
  )
  altered <- fit
  altered$continuation_history_contract$segments[[1L]]$generation <- 1L
  expect_error(
    env$rqr_bounded_validate_reopened_fit(
      altered, checkpoint, local, continuation_validator = validator
    ),
    "continuation-history"
  )
  expect_error(
    env$rqr_bounded_validate_reopened_fit(
      fit, checkpoint, local,
      continuation_validator = function(object) list(valid = FALSE)
    ),
    "failed continuation-history"
  )
})
