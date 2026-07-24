load_confirmatory_helpers <- function() {
  environment <- new.env(parent = globalenv())
  sys.source(
    testthat::test_path(
      "..", "..", "scripts", "lib",
      "rqr_dlm_confirmatory_simulation.R"
    ),
    envir = environment
  )
  environment
}

confirmatory_contract <- function(environment) {
  environment$rqr_confirm_read_contract(
    testthat::test_path("..", "..", "..")
  )
}

small_confirmatory_ledger <- function(environment, contract) {
  keys <- character()
  for (scenario in contract$config$scenarios) {
    data_id <- paste(scenario$dgp, scenario$T, sep = "_T")
    for (replication in 1:2) {
      keys <- c(
        keys,
        paste("training_state", scenario$pair, replication, sep = "|"),
        paste("training_response", data_id, replication, sep = "|"),
        paste("future_state", scenario$pair, replication, sep = "|"),
        paste("future_response", data_id, replication, sep = "|")
      )
    }
  }
  keys <- sort(unique(keys), method = "radix")
  old_kind <- RNGkind()
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit({
    do.call(RNGkind, as.list(old_kind))
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  RNGkind("L'Ecuyer-CMRG")
  set.seed(contract$config$rng$master_seed)
  state <- get(".Random.seed", envir = .GlobalEnv)
  states <- vector("list", length(keys))
  for (index in seq_along(keys)) {
    if (index > 1L) state <- parallel::nextRNGStream(state)
    states[[index]] <- state
  }
  names(states) <- keys
  future <- grep("^future_(state|response)\\|", keys, value = TRUE)
  for (parent in future) {
    substate <- states[[parent]]
    for (subreplication in seq_len(
        contract$config$design$future_subreplications)) {
      substate <- parallel::nextRNGSubStream(substate)
      states[[paste(
        parent, "subrep", subreplication, sep = "|"
      )]] <- substate
    }
  }
  ledger <- data.frame(
    task_key = names(states),
    state_digest = vapply(
      states, environment$rqr_confirm_state_digest, character(1L)
    ),
    state = vapply(
      states, environment$rqr_confirm_state_text, character(1L)
    ),
    stringsAsFactors = FALSE
  )
  attr(ledger, "states") <- states
  ledger
}

test_that("confirmatory contract imports Output-15 exactly and stays closed", {
  environment <- load_confirmatory_helpers()
  contract <- confirmatory_contract(environment)
  expect_invisible(environment$rqr_confirm_validate_contract(
    contract, require_closed = TRUE
  ))
  expect_invisible(environment$rqr_confirm_validate_budget(contract))
  expect_identical(
    contract$config$schema_version,
    "rqrgibbs_dlm_main_simulation/1.0.0"
  )
  expect_false(contract$config$diagnostic_pilot_execution_authorized)
  expect_false(contract$config$confirmatory_execution_authorized)
  expect_identical(nrow(contract$incidence), 208L)
  included <- environment$rqr_confirm_included(contract$incidence)
  expect_identical(sum(included), 89L)
  expect_identical(sum(!included), 119L)
  expect_setequal(
    unique(contract$incidence$include_or_omit_reason[included]),
    c("i", "x")
  )
  altered <- contract
  altered$config$confirmatory_execution_authorized <- TRUE
  expect_error(
    environment$rqr_confirm_validate_contract(
      altered, require_closed = TRUE
    ),
    "not fail closed"
  )
  expect_invisible(environment$rqr_confirm_validate_contract(altered))
})

test_that("all Output-15 budgets and sentinel counts are reproduced", {
  environment <- load_confirmatory_helpers()
  contract <- confirmatory_contract(environment)
  expected <- list(
    initial = c(15800, 17600, 15800, 14000, 882, 14882),
    central = c(29800, 33400, 29800, 26200, 1710, 27910),
    maximum = c(43800, 49200, 43800, 38400, 2538, 40938)
  )
  for (planning in names(expected)) {
    actual <- environment$rqr_confirm_budget_summary(
      contract, planning
    )
    expect_identical(as.numeric(actual$value), expected[[planning]])
  }
  sentinels <- environment$rqr_confirm_sentinel_map(contract, "maximum")
  expect_identical(
    sentinels,
    environment$rqr_confirm_sentinel_map(contract, "maximum")
  )
  expect_true(all(sentinels$selected_before_data))
  expect_identical(anyDuplicated(
    sentinels[c("cell_id", "replication")]
  ), 0L)
  plan <- environment$rqr_confirm_replication_plan(
    contract, "maximum"
  )
  capped_cell <- contract$incidence$cell_id[
    contract$incidence$DGP == "S03" &
      contract$incidence$method == "M08"
  ]
  capped <- sentinels[sentinels$cell_id == capped_cell, , drop = FALSE]
  expect_identical(length(capped$replication), 2L)
  expect_true(all(capped$replication %in% seq_len(200L)))
  expect_true(all(plan$embedded_sentinel[
    plan$DGP == "S03" &
      plan$replication %in% capped$replication
  ]))
  selection_keys <- sort(
    unique(sentinels$selection_task_key), method = "radix"
  )
  selection_states <- environment$rqr_confirm_stream_states(
    selection_keys, contract$config$rng$master_seed
  )
  expected_selection_digest <- vapply(
    sentinels$selection_task_key,
    function(key) environment$rqr_confirm_state_digest(
      selection_states[[key]]
    ),
    character(1L)
  )
  expect_identical(
    sentinels$selection_state_digest,
    unname(expected_selection_digest)
  )
  for (scenario in unique(plan$DGP)) {
    scenario_plan <- plan[plan$DGP == scenario, , drop = FALSE]
    for (batch in unique(scenario_plan$batch)) {
      block <- scenario_plan[
        scenario_plan$batch == batch, , drop = FALSE
      ]
      sentinel_positions <- which(block$embedded_sentinel)
      nonsentinel_positions <- which(!block$embedded_sentinel)
      if (length(sentinel_positions) && length(nonsentinel_positions)) {
        expect_lt(max(sentinel_positions), min(nonsentinel_positions))
      }
    }
  }
})

test_that("integer validation occurs before coercion", {
  environment <- load_confirmatory_helpers()
  expect_identical(
    environment$rqr_confirm_strict_integer(3, "value", 1L), 3L
  )
  for (value in list(0.5, -0.5, Inf, NA_real_, 2^31)) {
    expect_error(
      environment$rqr_confirm_strict_integer(value, "value", 0L),
      "whole number"
    )
  }
  expect_error(
    rqr_oracle_certificate("gaussian", 0.80, grid_size = 1601.5),
    "whole number"
  )
  expect_identical(
    formals(rqr_oracle_certificate)$grid_size, 1601L
  )
  contract <- confirmatory_contract(environment)
  expect_identical(
    contract$config$oracle$higher_precision_grid_size, 3201L
  )
  expect_lt(
    contract$config$oracle$higher_precision_tolerance,
    contract$config$oracle$primary_tolerance
  )
})

test_that("full L'Ecuyer states and future substreams are distinct", {
  environment <- load_confirmatory_helpers()
  contract <- confirmatory_contract(environment)
  ledger <- small_confirmatory_ledger(environment, contract)
  expect_identical(anyDuplicated(ledger$task_key), 0L)
  expect_identical(anyDuplicated(ledger$state_digest), 0L)
  expect_true(all(vapply(
    strsplit(ledger$state, ";", fixed = TRUE),
    length, integer(1L)
  ) == 7L))
  expect_true(any(grepl("\\|subrep\\|20$", ledger$task_key)))
})

test_that("complete seed ledgers reject altered serialized states", {
  environment <- load_confirmatory_helpers()
  contract <- confirmatory_contract(environment)
  source <- small_confirmatory_ledger(environment, contract)
  ledger <- data.frame(
    task_key = source$task_key,
    parent_task_key = NA_character_,
    stream_type = "stream",
    substream = NA_integer_,
    state_digest = source$state_digest,
    state = source$state,
    stringsAsFactors = FALSE
  )
  expect_silent(environment$rqr_confirm_validate_seed_ledger(
    ledger, contract, planning = "initial", require_complete = FALSE
  ))
  bad <- ledger
  bad$state[[1L]] <- sub(";", ".5;", bad$state[[1L]], fixed = TRUE)
  expect_error(
    environment$rqr_confirm_validate_seed_ledger(
      bad, contract, planning = "initial", require_complete = FALSE
    ),
    "exact integers"
  )
  bad <- ledger
  fields <- strsplit(bad$state[[1L]], ";", fixed = TRUE)[[1L]]
  fields[[2L]] <- as.character(as.numeric(fields[[2L]]) + 1)
  bad$state[[1L]] <- paste(fields, collapse = ";")
  expect_error(
    environment$rqr_confirm_validate_seed_ledger(
      bad, contract, planning = "initial", require_complete = FALSE
    ),
    "digest does not match"
  )
})

test_that("canonical DGPs separate state, response, and oracle quantities", {
  environment <- load_confirmatory_helpers()
  contract <- confirmatory_contract(environment)
  ledger <- small_confirmatory_ledger(environment, contract)
  draws <- lapply(names(contract$config$scenarios), function(scenario) {
    environment$rqr_confirm_generate_dgp(
      contract, scenario, 1L, ledger
    )
  })
  names(draws) <- names(contract$config$scenarios)
  expect_true(all(vapply(draws, function(draw) {
    all(is.finite(c(
      draw$training_y, draw$training_roots,
      draw$oracle_conditional_mean_root,
      draw$quantile_conditional_mean_root,
      draw$realized_root_path, draw$realized_quantile_path,
      draw$generated_future_response
    ))) &&
      all(draw$training_roots[, "upper"] >
          draw$training_roots[, "lower"]) &&
      identical(dim(draw$realized_root_path), c(20L, 20L, 2L)) &&
      identical(
        dim(draw$realized_quantile_path), c(20L, 20L, 2L)
      ) &&
      identical(dim(draw$generated_future_response), c(20L, 20L))
  }, logical(1L))))
  expect_identical(
    draws$S07$latent$seasonal,
    draws$S08$latent$seasonal
  )
  expect_identical(draws$S07$training_mu, draws$S08$training_mu)
  expect_identical(draws$S07$training_scale, draws$S08$training_scale)
  expect_false(identical(draws$S07$training_y, draws$S08$training_y))
  expect_true(any(
    abs(draws$S05$realized_root_path -
        array(
          rep(draws$S05$oracle_conditional_mean_root, each = 20L),
          c(20L, 20L, 2L)
        )) > 0
  ))
  hetero <- draws$S13
  q <- contract$config$dgp
  mean_z <- q$heteroscedastic_log_scale_ar *
    hetero$latent$terminal$z
  variance_z <- q$heteroscedastic_log_scale_innovation_variance
  exponent <- q$heteroscedastic_log_scale_coefficient
  threshold <- log(q$scale_floor) / exponent
  expected_scale <- q$scale_floor *
    stats::pnorm((threshold - mean_z) / sqrt(variance_z)) +
    exp(exponent * mean_z + 0.5 * exponent^2 * variance_z) *
    (1 - stats::pnorm(
      (threshold - mean_z - exponent * variance_z) /
        sqrt(variance_z)
    ))
  oracle <- rqr_oracle_roots(
    "student_t", hetero$coverage_level,
    params = list(df = 5, scale = sqrt(3 / 5))
  )
  recovered_scale <- diff(
    hetero$oracle_conditional_mean_root[1L, ]
  ) / (oracle$upper_root - oracle$lower_root)
  expect_equal(
    unname(recovered_scale), expected_scale, tolerance = 1e-12
  )
  root <- contract$config$dgp$root_alignment
  expect_equal(root$lower_initial, -2)
  expect_equal(root$upper_initial, 2)
  expect_equal(root$lower_variance, 0.001)
  expect_equal(root$upper_variance, 0.001)
  expect_equal(root$minimum_separation, 0.10)

  model <- environment$rqr_confirm_model_bundle(draws$S05)$training
  expanded <- rqrgibbs:::.rqr_expand_model(model, draws$S05$T)
  standard_profile <- contract$config$standard_initialization
  standard_endpoints <- environment$rqr_confirm_profile_interval(
    draws$S05, standard_profile
  )
  empirical_endpoints <- as.numeric(stats::quantile(
    draws$S05$training_y,
    c(
      (1 - draws$S05$coverage_level) / 2,
      1 - (1 - draws$S05$coverage_level) / 2
    ),
    names = FALSE, type = 8
  ))
  prior <- contract$config$frozen_tuning$component_scale_prior
  prior_median <- prior$rate /
    stats::qgamma(0.5, shape = prior$shape, rate = 1)
  standard_initial <- environment$rqr_confirm_initialization(
    draws$S05, model, standard_profile, component_scale = TRUE,
    component_scale_base = prior_median
  )
  expect_equal(mean(standard_endpoints), stats::median(draws$S05$training_y))
  expect_equal(
    unname(diff(standard_endpoints)), diff(empirical_endpoints)
  )
  expect_equal(
    standard_initial$evolution_scale,
    rep(prior_median, length(model$component_dims))
  )
  expect_identical(
    environment$rqr_confirm_initialization_profile_name(FALSE, 1L),
    "standard"
  )
  expect_identical(
    vapply(
      seq_len(4L),
      function(chain) {
        environment$rqr_confirm_initialization_profile_name(TRUE, chain)
      },
      character(1L)
    ),
    c("A", "B", "C", "D")
  )
  for (profile_name in c("A", "B", "C", "D")) {
    profile <- contract$config$initialization_profiles[[profile_name]]
    endpoints <- environment$rqr_confirm_profile_interval(
      draws$S05, profile
    )
    initial <- environment$rqr_confirm_initialization(
      draws$S05, model, profile, component_scale = TRUE
    )
    root1 <- colSums(expanded$FF * initial$state_root1)
    root2 <- colSums(expanded$FF * initial$state_root2)
    expect_equal(root1, rep(endpoints[["lower"]], draws$S05$T))
    expect_equal(root2, rep(endpoints[["upper"]], draws$S05$T))
    expect_true(endpoints[["upper"]] > endpoints[["lower"]])
  }

  for (replication in c(1L, 2L)) {
    first <- environment$rqr_confirm_generate_dgp(
      contract, "S05", replication, ledger
    )
    second <- environment$rqr_confirm_generate_dgp(
      contract, "S05", replication, ledger
    )
    expect_identical(
      serialize(first, NULL, version = 3L),
      serialize(second, NULL, version = 3L)
    )
  }

  quantile_result <- list(
    future_lower =
      draws$S05$quantile_conditional_mean_root[, "lower"],
    future_upper =
      draws$S05$quantile_conditional_mean_root[, "upper"],
    training_lower = draws$S05$training_roots[, "lower"],
    training_upper = draws$S05$training_roots[, "upper"],
    endpoint_target = "quantile"
  )
  quantile_metrics <- environment$rqr_confirm_replication_metrics(
    draws$S05, quantile_result
  )
  expect_equal(quantile_metrics$endpoint_rmse_lower, 0)
  expect_equal(quantile_metrics$endpoint_rmse_upper, 0)
  expect_gt(quantile_metrics$cross_target_distance, 0)
})

test_that("external-runtime attestations reject fractional raw counts", {
  environment <- load_confirmatory_helpers()
  directory <- tempfile("rqr-confirm-attestation-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE, force = TRUE), add = TRUE)
  source_path <- file.path(directory, "source.tar.gz")
  writeBin(charToRaw("source fixture"), source_path)
  source_sha <- environment$rqr_confirm_sha256(source_path)
  runtime_path <- file.path(directory, "runtime")
  dir.create(runtime_path)
  attestation_path <- file.path(directory, "attestation.json")
  jsonlite::write_json(
    list(
      schema_version = "rqrgibbs_external_cran_runtime/1.0.0",
      package = "fixture", version = "1.0",
      source_package_path = source_path,
      source_package_sha256 = source_sha,
      install_input_count = 1.5,
      install_exit_status = 0,
      runtime_path = runtime_path,
      runtime_tree_digest = "unreachable",
      protected_exdqlm_checkout_used = FALSE
    ),
    attestation_path, auto_unbox = TRUE
  )
  expect_error(
    environment$rqr_confirm_read_attestation(
      attestation_path, "fixture", "1.0", source_sha
    ),
    "install_input_count must be one finite whole number"
  )
})

test_that("analysis keeps failures in denominators and freezes contrasts", {
  environment <- load_confirmatory_helpers()
  contract <- confirmatory_contract(environment)
  pairs <- environment$rqr_confirm_contrast_pairs()
  expect_identical(
    names(pairs)[vapply(
      pairs, function(value) identical(value$role, "primary"), logical(1L)
    )],
    c(
      "M01_vs_M02", "M01_vs_M05", "M01_vs_M03",
      "M01_vs_M06", "M01_vs_M07"
    )
  )
  required <- environment$rqr_confirm_artifact_schemas()$
    replication_results
  make_row <- function(replication, status) {
    row <- as.list(setNames(rep(1, length(required)), required))
    row$run_id <- "fixture"
    row$cell_id <- "C01M01"
    row$replication <- replication
    row$method <- "M01"
    row$status <- status
    row$failure_class <- if (status == "completed") {
      ""
    } else {
      "fixture_failure"
    }
    row$cross_target_distance <- NA_real_
    if (status != "completed") {
      metric_names <- setdiff(
        required,
        c(
          "run_id", "cell_id", "replication", "method", "status",
          "failure_class", "training_response_sd", "mean_oracle_width",
          "elapsed_seconds", "peak_RSS_bytes"
        )
      )
      row[metric_names] <- NA_real_
    }
    as.data.frame(row, stringsAsFactors = FALSE, check.names = FALSE)
  }
  results <- do.call(rbind, list(
    make_row(1L, "completed"),
    make_row(2L, "diagnostic_failed"),
    make_row(3L, "completed")
  ))
  summarized <- environment$rqr_confirm_summarize_results(results, contract)
  failure <- summarized$summary[
    summarized$summary$cell_id == "C01M01" &
      summarized$summary$measure == "fit_failure_probability",
    ,
    drop = FALSE
  ]
  expect_identical(nrow(failure), 1L)
  expect_equal(failure$planned_denominator, 3)
  expect_equal(failure$completed, 2)
  expect_equal(failure$failed, 1)
  expect_equal(failure$mean, 1 / 3)
})

test_that("atomic publication rolls back and refuses overwrite", {
  environment <- load_confirmatory_helpers()
  directory <- tempfile("rqr-confirm-atomic-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE, force = TRUE), add = TRUE)
  path <- file.path(directory, "result.csv")
  expect_error(
    environment$rqr_confirm_atomic_write_csv(
      data.frame(value = 1), path, inject_failure = TRUE
    ),
    "Injected"
  )
  expect_false(file.exists(path))
  expect_invisible(environment$rqr_confirm_atomic_write_csv(
    data.frame(value = 1), path
  ))
  expect_error(
    environment$rqr_confirm_atomic_write_csv(
      data.frame(value = 2), path
    ),
    "refused"
  )
})

test_that("execution authorization is commit bound and fail closed", {
  environment <- load_confirmatory_helpers()
  contract <- confirmatory_contract(environment)
  commit <- paste(rep("a", 40L), collapse = "")
  digest_values <- setNames(
    vapply(
      seq_len(7L),
      function(index) paste(rep(as.character(index), 64L), collapse = ""),
      character(1L)
    ),
    c(
      "primary_runtime_tree_digest",
      "preflight_artifact_hashes_sha256",
      "reference_artifact_hashes_sha256", "seed_ledger_sha256",
      "task_plan_sha256", "exdqlm_source_sha256",
      "quantreg_source_sha256"
    )
  )
  digest_values[["exdqlm_source_sha256"]] <-
    contract$config$comparator$exdqlm$source_sha256
  digest_values[["quantreg_source_sha256"]] <-
    contract$config$comparator$quantreg$source_sha256
  observed <- as.list(digest_values)
  observed$reviewed_implementation_commit <-
    paste(rep("b", 40L), collapse = "")
  observed$authorization_diff_only_flag <- TRUE
  observed$primary_worktree_clean <- TRUE
  observed$reference_runtime_bundle_match <- TRUE
  observed$comparator_dependency_runtime_match <- TRUE
  observed$toolchain_match <- TRUE
  observed$protected_checkout_used <- FALSE
  bundle <- c(
    list(
      schema_version =
        "rqrgibbs_dlm_confirmatory_authorization/1.0.0",
      reviewed_implementation_commit =
        paste(rep("b", 40L), collapse = ""),
      authorization_commit = commit,
      authorization_diff_only_flag = TRUE,
      explicit_user_confirmation = TRUE,
      all_reference_gates_pass = TRUE,
      primary_worktree_clean = TRUE,
      reference_runtime_bundle_match = TRUE,
      comparator_dependency_runtime_match = TRUE,
      toolchain_match = TRUE,
      protected_checkout_used = FALSE
    ),
    as.list(digest_values)
  )
  expect_error(
    environment$rqr_confirm_authorized(
      contract, "execute-confirmatory", commit, bundle,
      observed = observed
    ),
    "disabled"
  )
  contract$config$confirmatory_execution_authorized <- TRUE
  expect_true(environment$rqr_confirm_authorized(
    contract, "execute-confirmatory", commit, bundle,
    observed = observed
  ))
  bad_observed <- observed
  bad_observed$seed_ledger_sha256 <- paste(rep("f", 64L), collapse = "")
  expect_error(
    environment$rqr_confirm_authorized(
      contract, "execute-confirmatory", commit, bundle,
      observed = bad_observed
    ),
    "seed_ledger_sha256"
  )
})
