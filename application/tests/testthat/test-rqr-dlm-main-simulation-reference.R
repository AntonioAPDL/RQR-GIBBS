load_main_simulation_helpers <- function() {
  helper <- testthat::test_path(
    "..", "..", "scripts", "lib", "rqr_dlm_main_simulation.R"
  )
  env <- new.env(parent = globalenv())
  sys.source(helper, envir = env)
  env
}

test_that("main-simulation helper validates the frozen contract", {
  env <- load_main_simulation_helpers()
  contract <- env$rqr_main_read_contract(
    testthat::test_path("..", "..", "..")
  )
  expect_invisible(env$rqr_main_validate_contract(contract))
  altered <- contract
  altered$config$diagnostic_pilot_execution_authorized <- TRUE
  expect_error(
    env$rqr_main_validate_contract(altered), "not fail closed"
  )
  altered <- contract
  altered$scenarios$scale_floor[[1L]] <- 0
  expect_error(
    env$rqr_main_validate_contract(altered), "positivity"
  )
})

test_that("DGP construction is deterministic and matched by design", {
  env <- load_main_simulation_helpers()
  contract <- env$rqr_main_read_contract(
    testthat::test_path("..", "..", "..")
  )
  first <- env$rqr_main_generate_dgp(
    contract, "trend_seasonal_gaussian", 1L, 0.80,
    n_time = 32L, horizon = 4L
  )
  repeated <- env$rqr_main_generate_dgp(
    contract, "trend_seasonal_gaussian", 1L, 0.80,
    n_time = 32L, horizon = 4L
  )
  skewed <- env$rqr_main_generate_dgp(
    contract, "trend_seasonal_skewed", 1L, 0.80,
    n_time = 32L, horizon = 4L
  )
  expect_identical(first, repeated)
  expect_identical(first$mu, skewed$mu)
  expect_identical(first$scale, skewed$scale)
  expect_false(identical(first$y, skewed$y))
  expect_true(all(first$scale > 0))
  expect_true(all(
    first$realized_root_path[, "upper"] >
      first$realized_root_path[, "lower"]
  ))
  expect_false(first$response_prediction_contract)
})

test_that("all frozen DGPs pass finite and separation gates", {
  env <- load_main_simulation_helpers()
  contract <- env$rqr_main_read_contract(
    testthat::test_path("..", "..", "..")
  )
  draws <- lapply(contract$scenarios$dgp_id, function(dgp_id) {
    env$rqr_main_generate_dgp(
      contract, dgp_id, 1L, 0.80,
      n_time = 32L, horizon = 4L
    )
  })
  expect_true(all(vapply(draws, function(draw) {
    all(is.finite(c(
      draw$y, draw$mu, draw$scale, draw$realized_root_path
    ))) &&
      all(draw$scale > 0) &&
      all(draw$realized_root_path[, "upper"] >
          draw$realized_root_path[, "lower"])
  }, logical(1L))))
})

test_that("independent-root sensitivity uses one response law", {
  env <- load_main_simulation_helpers()
  contract <- env$rqr_main_read_contract(
    testthat::test_path("..", "..", "..")
  )
  c80 <- env$rqr_main_generate_dgp(
    contract, "independent_root_prior_alignment", 2L, 0.80,
    n_time = 32L, horizon = 4L
  )
  c90 <- env$rqr_main_generate_dgp(
    contract, "independent_root_prior_alignment", 2L, 0.90,
    n_time = 32L, horizon = 4L
  )
  expect_identical(c80$y, c90$y)
  expect_identical(c80$mu, c90$mu)
  expect_identical(c80$scale, c90$scale)
  expect_false(identical(
    c80$realized_root_path, c90$realized_root_path
  ))
})

test_that("seed streams and coverage equivalence are deterministic", {
  env <- load_main_simulation_helpers()
  contract <- env$rqr_main_read_contract(
    testthat::test_path("..", "..", "..")
  )
  first <- env$rqr_main_seed_ledger(contract)
  second <- env$rqr_main_seed_ledger(contract)
  expect_identical(first, second)
  expect_identical(anyDuplicated(first[c(
    "dgp_id", "coverage_level", "replication", "stream"
  )]), 0L)
  expect_identical(anyDuplicated(first$seed), 0L)
  expect_true(all(first$seed > 0L))
  qualified <- env$rqr_main_coverage_qualified(
    0.80, 0.005, 0.80
  )
  noisy <- env$rqr_main_coverage_qualified(
    0.80, 0.02, 0.80
  )
  expect_true(qualified$qualified)
  expect_false(noisy$qualified)
})

test_that("atomic compact publication rolls back on faults", {
  env <- load_main_simulation_helpers()
  directory <- tempfile("rqr-main-atomic-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE, force = TRUE), add = TRUE)
  path <- file.path(directory, "result.csv")
  expect_error(
    env$rqr_main_atomic_write_csv(
      data.frame(value = 1), path, inject_failure = TRUE
    ),
    "Injected"
  )
  expect_false(file.exists(path))
  expect_length(list.files(directory, all.files = TRUE, no.. = TRUE), 0L)
  expect_invisible(env$rqr_main_atomic_write_csv(
    data.frame(value = 1), path
  ))
  expect_true(file.exists(path))
  manifest <- env$rqr_main_recursive_manifest(directory)
  expect_identical(manifest$path, "result.csv")
  expect_match(manifest$sha256, "^[0-9a-f]{64}$")
  expect_error(
    env$rqr_main_atomic_write_csv(data.frame(value = 2), path),
    "refuses to overwrite"
  )
})

test_that("pilot and confirmatory runner modes fail before output", {
  script <- normalizePath(testthat::test_path(
    "..", "..", "scripts",
    "13_run_rqr_dlm_main_simulation_references.R"
  ))
  root <- normalizePath(testthat::test_path("..", "..", ".."))
  for (mode in c("diagnostic-pilot", "execute-confirmatory")) {
    output <- tempfile(paste0("rqr-main-", mode, "-"))
    result <- suppressWarnings(system2(
      file.path(R.home("bin"), "Rscript"),
      c(shQuote(script), mode, shQuote(output)),
      stdout = TRUE, stderr = TRUE
    ))
    expect_false(is.null(attr(result, "status")))
    expect_match(paste(result, collapse = "\n"), "not implemented")
    expect_false(dir.exists(output))
  }
  preflight_output <- tempfile("rqr-main-diagnostic-preflight-")
  previous_directory <- setwd(root)
  on.exit(setwd(previous_directory), add = TRUE)
  preflight_result <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"),
    c(
      shQuote(script), "diagnostic-pilot-preflight",
      shQuote(preflight_output)
    ),
    stdout = TRUE, stderr = TRUE
  ))
  expect_false(is.null(attr(preflight_result, "status")))
  expect_match(
    paste(preflight_result, collapse = "\n"),
    "requires an exact isolated primary runtime"
  )
  expect_false(dir.exists(preflight_output))
  expect_true(file.exists(file.path(root, "AGENTS.md")))
})

test_that("isolated exdqlm adapter preserves component and horizon orientation", {
  root <- normalizePath(testthat::test_path("..", "..", ".."))
  attestation_path <- file.path(
    root, "application", "cache", "exdqlm_cran_1.1.0",
    "exdqlm_1.1.0_runtime_attestation.json"
  )
  testthat::skip_if_not(file.exists(attestation_path))
  helper <- file.path(
    root, "application", "scripts", "lib",
    "rqr_dlm_main_simulation.R"
  )
  expression <- paste0(
    "source(", dQuote(helper), ");",
    "a<-jsonlite::read_json(", dQuote(attestation_path),
    ",simplifyVector=TRUE);",
    "x<-rqr_main_validate_exdqlm_adapter(a);",
    "stopifnot(isTRUE(x$pass),",
    "identical(x$reduced_AL_flag,'dqlm.ind=TRUE'),",
    "identical(x$forecast_orientation,'horizon_vector'),",
    "isTRUE(x$raw_quantile_forecasts_retained),",
    "!isTRUE(x$response_predictive_draws_used),",
    "!isTRUE(x$protected_checkout_used));",
    "cat('adapter_passed\\n')"
  )
  result <- system2(
    file.path(R.home("bin"), "Rscript"),
    c("-e", shQuote(expression)),
    stdout = TRUE, stderr = TRUE
  )
  expect_null(attr(result, "status"))
  expect_match(paste(result, collapse = "\n"), "adapter_passed")
})

test_that("isolated quantreg adapter preserves endpoint orientation", {
  root <- normalizePath(testthat::test_path("..", "..", ".."))
  attestation_path <- file.path(
    root, "application", "cache", "quantreg_cran_6.1",
    "quantreg_6.1_runtime_attestation.json"
  )
  testthat::skip_if_not(file.exists(attestation_path))
  helper <- file.path(
    root, "application", "scripts", "lib",
    "rqr_dlm_main_simulation.R"
  )
  expression <- paste0(
    "source(", dQuote(helper), ");",
    "a<-jsonlite::read_json(", dQuote(attestation_path),
    ",simplifyVector=TRUE);",
    "x<-rqr_main_validate_quantreg_adapter(a);",
    "stopifnot(isTRUE(x$adapter_pass),",
    "identical(x$fitting_function,'rq'),",
    "isTRUE(x$raw_quantiles_retained),",
    "isTRUE(x$ordering_applied_only_for_interval_scoring),",
    "!isTRUE(x$response_predictive_draws_used));",
    "cat('adapter_passed\\n')"
  )
  result <- system2(
    file.path(R.home("bin"), "Rscript"),
    c("-e", shQuote(expression)),
    stdout = TRUE, stderr = TRUE
  )
  expect_null(attr(result, "status"))
  expect_match(paste(result, collapse = "\n"), "adapter_passed")
})
