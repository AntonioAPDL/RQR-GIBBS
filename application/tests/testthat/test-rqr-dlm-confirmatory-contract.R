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
  expect_identical(
    contract$config$implementation_correction$schema_version,
    "rqrgibbs_dlm_main_correction/1.2.0"
  )
  expect_identical(
    contract$config$implementation_correction$
      failed_authorization_commit,
    "b8b7748ab181a006611b602f64d4edf5be591de6"
  )
  expect_false(
    contract$config$implementation_correction$failed_outputs_reused
  )
  expect_false(
    contract$config$implementation_correction$
      comparative_simulation_metrics_used
  )
  expect_true(
    contract$config$implementation_correction$
      failed_wave_diagnostics_used_for_computational_correction
  )
  expect_false(
    contract$config$implementation_correction$
      failed_wave_scientific_metrics_used_for_correction
  )
  expect_identical(
    contract$config$implementation_correction$correction_validation_role,
    "computational_transition_and_fixed_schedule_only"
  )
  expect_true(
    contract$config$implementation_correction$
      uniform_role_specific_schedule_no_adaptive_extension
  )
  expect_true(
    contract$config$implementation_correction$fresh_relaunch_required
  )
  expect_identical(
    contract$config$implementation_correction$
      comparator_standard_schedule_correction,
    "retain_4000_after_projection_correct_full_wave_diagnostic_gate"
  )
  expect_identical(
    contract$config$implementation_correction$
      second_failed_authorization_commit,
    "bb966299bb298ee31ec65d167edf53c44ce48b03"
  )
  expect_identical(
    contract$config$implementation_correction$second_failed_wave_id,
    "local_level_gaussian_T200__target0200__sentinel"
  )
  expect_false(
    contract$config$implementation_correction$
      second_failed_outputs_reused
  )
  expect_false(
    contract$config$implementation_correction$
      second_failed_scientific_metrics_used
  )
  expect_identical(
    contract$config$implementation_correction$correction_budget_sha256,
    environment$rqr_confirm_sha256(file.path(
      contract$repo_root,
      contract$config$implementation_correction$correction_budget_path
    ))
  )
  correction_budget <- utils::read.csv(
    file.path(
      contract$repo_root,
      contract$config$implementation_correction$correction_budget_path
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  expect_identical(
    correction_budget$corrected_value[
      correction_budget$section == "mcmc" &
        correction_budget$planning == "maximum"
    ],
    199098000
  )
  expect_false(
    contract$config$implementation_correction$
      target_prior_seed_or_diagnostic_threshold_changed
  )
  expect_true(
    contract$config$implementation_correction$
      mcmc_transition_and_standard_schedule_changed
  )
  expect_identical(
    contract$config$frozen_tuning$component_scale_kernel,
    list(
      centered_inverse_gamma = TRUE,
      noncentered_slice_interweave = TRUE,
      interweave_cycles = 1L,
      slice_width = 1,
      slice_sweeps_per_cycle = 2L,
      slice_max_steps = 100L,
      slice_max_shrink = 1000L,
      target_change = FALSE
    )
  )
  expect_identical(
    contract$config$schedules$dynamic_rqr_component_scale_standard,
    list(burn = 1000L, retain = 6000L, thin = 1L)
  )
  expect_identical(
    contract$config$schedules$
      learned_dynamic_rqr_component_scale_standard,
    list(burn = 1500L, retain = 9000L, thin = 1L)
  )
  expect_identical(
    contract$config$schedules$dynamic_quantile_endpoint_standard,
    list(burn = 1000L, retain = 4000L, thin = 1L)
  )
  expect_identical(
    contract$config$schedules$fixed_design_rqr_standard,
    list(burn = 500L, retain = 3000L, thin = 1L)
  )
  expect_identical(
    environment$rqr_confirm_dynamic_schedule(
      contract, "M01", TRUE, "standard"
    ),
    contract$config$schedules$dynamic_rqr_component_scale_standard
  )
  expect_identical(
    environment$rqr_confirm_dynamic_schedule(
      contract, "M01", TRUE, "A"
    ),
    contract$config$schedules$dynamic_rqr
  )
  expect_identical(
    environment$rqr_confirm_dynamic_schedule(
      contract, "M11", TRUE, "standard"
    ),
    contract$config$schedules$
      learned_dynamic_rqr_component_scale_standard
  )
  expect_identical(
    environment$rqr_confirm_dynamic_schedule(
      contract, "M11", TRUE, "D"
    ),
    contract$config$schedules$learned_dynamic_rqr
  )
  expect_identical(
    environment$rqr_confirm_dynamic_quantile_schedule(
      contract, "standard"
    ),
    contract$config$schedules$dynamic_quantile_endpoint_standard
  )
  expect_identical(
    environment$rqr_confirm_dynamic_quantile_schedule(
      contract, "A"
    ),
    contract$config$schedules$dynamic_quantile_endpoint
  )
  expect_identical(
    environment$rqr_confirm_fixed_design_schedule(
      contract, "standard"
    ),
    contract$config$schedules$fixed_design_rqr_standard
  )
  expect_identical(
    environment$rqr_confirm_fixed_design_schedule(
      contract, "A"
    ),
    contract$config$schedules$fixed_design_rqr
  )
  expect_identical(contract$config$resources$threads_per_worker, 1L)
  expect_identical(
    contract$config$resources$sampled_process_group_thread_ceiling, 4L
  )
  expect_identical(
    contract$config$resources$
      sampled_reference_process_group_thread_ceiling, 4L
  )
  expect_identical(
    contract$config$resources$sampled_thread_ceiling_role,
    "hard_OS_thread_envelope_not_compute_parallelism"
  )
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

test_that("fit provenance retains the primary attestation file path", {
  environment <- load_confirmatory_helpers()
  root <- tempfile("rqr-confirm-primary-provenance-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  commit <- paste(rep("a", 40L), collapse = "")
  attestation_path <- file.path(root, "primary-runtime-attestation.rds")
  saveRDS(
    list(
      schema_version = "rqrgibbs_runtime_attestation/5.0.0",
      source_commit = commit
    ),
    attestation_path,
    version = 3L
  )
  control <- environment$rqr_confirm_primary_provenance_control(
    root, commit, attestation_path
  )
  expect_identical(
    control$primary_runtime_attestation,
    normalizePath(attestation_path, winslash = "/", mustWork = TRUE)
  )
  expect_true(file.exists(control$primary_runtime_attestation))
  expect_error(
    environment$rqr_confirm_primary_provenance_control(
      root, commit, readRDS(attestation_path)
    ),
    "must be one existing RDS file path"
  )
  wrong_commit <- paste(rep("b", 40L), collapse = "")
  expect_error(
    environment$rqr_confirm_primary_provenance_control(
      root, wrong_commit, attestation_path
    ),
    "wrong schema or source commit"
  )
})

test_that("the native model adapter preserves the exdqlm state contract", {
  environment <- load_confirmatory_helpers()
  namespace <- new.env(parent = emptyenv())
  namespace$as.exdqlm <- function(model) {
    model$m0 <- matrix(model$m0, ncol = 1L)
    class(model) <- "exdqlm"
    model
  }
  namespace$is.exdqlm <- function(model) {
    inherits(model, "exdqlm")
  }
  native <- rqr_polytrend(
    2L, m0 = c(0, 0), C0 = diag(c(4, 1)), name = "trend"
  ) + rqr_regression(
    matrix(seq(-1, 1, length.out = 20L), 20L, 1L),
    m0 = 0, C0 = matrix(2, 1L, 1L), name = "regression"
  )
  converted <- environment$rqr_confirm_as_exdqlm_model(
    native, namespace
  )
  expect_s3_class(converted, "exdqlm")
  expect_identical(dim(converted$FF), dim(native$FF))
  expect_identical(dim(converted$GG), dim(native$GG))
  expect_equal(as.numeric(converted$FF), as.numeric(native$FF))
  expect_equal(as.numeric(converted$GG), as.numeric(native$GG))
  broken <- new.env(parent = emptyenv())
  broken$as.exdqlm <- function(model) {
    model$m0 <- matrix(model$m0, ncol = 1L)
    model$FF <- model$FF[, -1L, drop = FALSE]
    class(model) <- "exdqlm"
    model
  }
  broken$is.exdqlm <- namespace$is.exdqlm
  expect_error(
    environment$rqr_confirm_as_exdqlm_model(native, broken),
    "changed the native state-space contract"
  )
})

test_that("multistate exdqlm means are projected to observation ordinates", {
  environment <- load_confirmatory_helpers()
  FF <- rbind(
    intercept = rep(1, 5L),
    predictor = c(-1, -0.5, 0, 0.5, 1)
  )
  state_mean <- rbind(
    intercept = c(1, 2, 3, 4, 5),
    predictor = c(0.5, 1, 1.5, 2, 2.5)
  )
  expected <- as.numeric(colSums(FF * state_mean))
  expect_identical(
    environment$rqr_confirm_state_ordinate_mean(FF, state_mean),
    expected
  )
  expect_identical(length(expected), ncol(FF))
  expect_false(identical(
    length(as.numeric(state_mean)), length(expected)
  ))

  fit <- list(
    model = list(FF = FF),
    theta.out = list(fm = state_mean)
  )
  class(fit) <- "exdqlmMCMC"
  expect_identical(
    environment$rqr_confirm_exdqlm_ordinate_mean(fit),
    expected
  )
  expect_error(
    environment$rqr_confirm_state_ordinate_mean(
      FF, state_mean[, -1L, drop = FALSE]
    ),
    "identical dimensions"
  )
  broken <- state_mean
  broken[1L, 1L] <- Inf
  expect_error(
    environment$rqr_confirm_state_ordinate_mean(FF, broken),
    "finite matrices"
  )
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
  expected_iterations <- c(
    initial = 74182000,
    central = 136640000,
    maximum = 199098000
  )
  for (planning in names(expected_iterations)) {
    actual <- environment$rqr_confirm_iteration_budget_summary(
      contract, planning
    )
    expect_identical(
      actual$value[actual$item == "total_MCMC_iterations"],
      unname(expected_iterations[[planning]])
    )
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

test_that("wave assignments are canonical and respect frozen worker limits", {
  environment <- load_confirmatory_helpers()
  contract <- confirmatory_contract(environment)
  tasks <- environment$rqr_confirm_replication_plan(contract, "maximum")
  waves <- environment$rqr_confirm_wave_plan(contract, "maximum")
  expect_identical(
    sort(waves$replication_task_id),
    sort(tasks$replication_task_id)
  )
  expect_true(all(waves$worker_slot >= 1L))
  expect_true(all(waves$worker_slot <= waves$worker_limit))
  expect_true(all(
    waves$mode[waves$phase == "sentinel"] == "sentinel-core"
  ))
  expect_true(all(
    waves$mode[waves$phase == "standard"] == "execute-confirmatory"
  ))
  expect_true(all(
    waves$worker_limit[waves$phase == "sentinel"] ==
      contract$config$resources$sentinel_workers
  ))
  expect_true(all(
    waves$worker_limit[waves$phase == "standard"] ==
      contract$config$resources$workers
  ))
  by_wave <- split(waves, waves$wave_id)
  expect_true(all(vapply(by_wave, function(value) {
    length(unique(value$batch_group)) == 1L &&
      length(unique(value$batch_target)) == 1L &&
      length(unique(value$phase)) == 1L &&
      length(unique(value$mode)) == 1L
  }, logical(1L))))
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
  for (scenario in names(draws)) {
    draw <- draws[[scenario]]
    bundle <- environment$rqr_confirm_model_bundle(draw)
    expect_identical(
      ncol(bundle$training$FF), draw$T,
      info = paste(scenario, "training horizon")
    )
    expect_identical(
      ncol(bundle$future$FF), draw$H,
      info = paste(scenario, "future horizon")
    )
    expect_identical(
      ncol(bundle$full$FF), draw$T + draw$H,
      info = paste(scenario, "full horizon")
    )
    expect_identical(
      bundle$training$FF,
      bundle$full$FF[, seq_len(draw$T), drop = FALSE],
      info = paste(scenario, "training/full partition")
    )
    expect_identical(
      bundle$future$FF,
      bundle$full$FF[
        , draw$T + seq_len(draw$H), drop = FALSE
      ],
      info = paste(scenario, "future/full partition")
    )
  }
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

test_that("precision batches require exact contiguous paired replications", {
  environment <- load_confirmatory_helpers()
  contract <- confirmatory_contract(environment)
  scenarios <- c("S01", "S02")
  incidence <- contract$incidence[
    contract$incidence$DGP %in% scenarios &
      contract$incidence$include_or_omit_reason == "i",
    ,
    drop = FALSE
  ]
  schema <- environment$rqr_confirm_artifact_schemas()$replication_results
  rows <- vector("list", nrow(incidence) * 200L)
  index <- 0L
  for (cell_index in seq_len(nrow(incidence))) {
    for (replication in seq_len(200L)) {
      index <- index + 1L
      row <- as.list(setNames(rep(1, length(schema)), schema))
      row$run_id <- "batch-fixture"
      row$cell_id <- incidence$cell_id[[cell_index]]
      row$replication <- replication
      row$method <- incidence$method[[cell_index]]
      row$status <- "completed"
      row$failure_class <- ""
      row$aggregate_coverage <-
        contract$config$scenarios[[incidence$DGP[[cell_index]]]]$coverage +
        (replication - 100.5) / 1e6
      row$cross_target_distance <- if (
          incidence$method[[cell_index]] %in% c("M02", "M04")) {
        0.1
      } else {
        NA_real_
      }
      row$training_response_sd <- 1
      row$mean_oracle_width <- 1
      rows[[index]] <- as.data.frame(
        row, stringsAsFactors = FALSE, check.names = FALSE
      )
    }
  }
  results <- do.call(rbind, rows)
  summaries <- environment$rqr_confirm_summarize_results(results, contract)
  contrasts <- environment$rqr_confirm_paired_contrasts(results, contract)
  decisions <- environment$rqr_confirm_batch_decisions(
    results, summaries$precision, contrasts, contract
  )
  decision <- decisions[
    decisions$batch_group == "static_gaussian_T200", , drop = FALSE
  ]
  expect_identical(nrow(decision), 1L)
  expect_true(decision$paired_batch_complete)
  incomplete <- results[!(
    results$cell_id == incidence$cell_id[[1L]] &
      results$replication == 100L
  ), , drop = FALSE]
  incomplete_summaries <- environment$rqr_confirm_summarize_results(
    incomplete, contract
  )
  incomplete_contrasts <- environment$rqr_confirm_paired_contrasts(
    incomplete, contract
  )
  incomplete_decision <- environment$rqr_confirm_batch_decisions(
    incomplete, incomplete_summaries$precision,
    incomplete_contrasts, contract
  )
  incomplete_decision <- incomplete_decision[
    incomplete_decision$batch_group == "static_gaussian_T200",
    ,
    drop = FALSE
  ]
  expect_false(incomplete_decision$paired_batch_complete)
  expect_false(incomplete_decision$precision_pass)
})

test_that("collection verifies exact tasks, artifacts, and fit IDs", {
  environment <- load_confirmatory_helpers()
  contract <- confirmatory_contract(environment)
  plan <- environment$rqr_confirm_replication_plan(contract, "maximum")
  plan <- plan[1L, , drop = FALSE]
  root <- tempfile("rqr-confirm-collection-")
  stage <- file.path(root, "wave", "worker-01")
  replication_dir <- file.path(
    stage, "replications", plan$DGP[[1L]],
    sprintf("rep%04d", plan$replication[[1L]])
  )
  dir.create(replication_dir, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  result_schema <- environment$rqr_confirm_artifact_schemas()$
    replication_results
  methods <- strsplit(plan$methods[[1L]], "|", fixed = TRUE)[[1L]]
  result <- do.call(rbind, lapply(methods, function(method) {
    row <- as.list(setNames(rep(1, length(result_schema)), result_schema))
    row$run_id <- "worker-01"
    row$cell_id <- contract$incidence$cell_id[
      contract$incidence$DGP == plan$DGP[[1L]] &
        contract$incidence$method == method
    ]
    row$replication <- plan$replication[[1L]]
    row$method <- method
    row$status <- "completed"
    row$failure_class <- ""
    row$cross_target_distance <- if (method %in% c("M02", "M04")) {
      0.1
    } else {
      NA_real_
    }
    as.data.frame(row, stringsAsFactors = FALSE, check.names = FALSE)
  }))
  utils::write.csv(
    result, file.path(replication_dir, "replication_results.csv"),
    row.names = FALSE, quote = TRUE
  )
  replication_manifest <- list(
    schema_version = "rqrgibbs_dlm_replication/1.0.0",
    source_commit = paste(rep("a", 40L), collapse = ""),
    config_digest = paste(rep("b", 64L), collapse = ""),
    incidence_digest = paste(rep("c", 64L), collapse = ""),
    seed_ledger_digest = paste(rep("d", 64L), collapse = ""),
    runtime_digest = paste(rep("e", 64L), collapse = ""),
    DGP = plan$DGP[[1L]], replication = plan$replication[[1L]],
    embedded_sentinel = plan$embedded_sentinel[[1L]],
    no_retry = TRUE, generalized_bayes = TRUE,
    response_likelihood = FALSE, response_prediction_contract = FALSE
  )
  jsonlite::write_json(
    replication_manifest,
    file.path(replication_dir, "replication_manifest.json"),
    auto_unbox = TRUE, pretty = TRUE
  )
  replication_hashes <- environment$rqr_confirm_recursive_manifest(
    replication_dir
  )
  utils::write.csv(
    replication_hashes,
    file.path(replication_dir, "replication_artifact_hashes.csv"),
    row.names = FALSE, quote = TRUE
  )

  status <- transform(
    plan, status = "completed", started_at = "fixture",
    ended_at = "fixture", message = ""
  )
  utils::write.csv(
    status, file.path(stage, "run_status.csv"),
    row.names = FALSE, quote = TRUE
  )
  jsonlite::write_json(
    list(
      schema_version = "rqrgibbs_dlm_confirmatory_stage/1.0.0",
      mode = if (plan$embedded_sentinel[[1L]]) {
        "sentinel-core"
      } else {
        "execute-confirmatory"
      },
      source_commit = paste(rep("a", 40L), collapse = ""),
      primary_runtime_binding = list(
        runtime_tree_digest = paste(rep("e", 64L), collapse = "")
      ),
      generalized_bayes = TRUE, response_likelihood = FALSE,
      response_prediction_contract = FALSE, status = "passed"
    ),
    file.path(stage, "run_manifest.json"),
    auto_unbox = TRUE, pretty = TRUE
  )
  stage_hashes <- environment$rqr_confirm_recursive_manifest(stage)
  utils::write.csv(
    stage_hashes, file.path(stage, "artifact_hashes.csv"),
    row.names = FALSE, quote = TRUE
  )
  jsonlite::write_json(
    list(
      schema_version = "rqrgibbs_dlm_wave/2.0.0",
      canonical_wave_index = 1L,
      wave_id = "fixture", mode = if (plan$embedded_sentinel[[1L]]) {
        "sentinel-core"
      } else {
        "execute-confirmatory"
      },
      phase = if (plan$embedded_sentinel[[1L]]) {
        "sentinel"
      } else {
        "standard"
      },
      batch_group = "fixture", batch_target = 1L,
      binding_digest = paste(rep("f", 64L), collapse = ""),
      start_sha256 = paste(rep("1", 64L), collapse = ""),
      same_batch_sentinel_pass =
        if (plan$embedded_sentinel[[1L]]) NA else TRUE,
      prior_batch_decision_sha256 = "",
      worker_limit = 1L, workers_used = 1L, task_count = 1L,
      all_workers_passed = TRUE, no_retry = TRUE, no_reseed = TRUE,
      source_commit = paste(rep("a", 40L), collapse = ""),
      runtime_tree_digest = paste(rep("e", 64L), collapse = "")
    ),
    file.path(root, "wave_manifest.json"),
    auto_unbox = TRUE, pretty = TRUE
  )
  wave_hashes <- environment$rqr_confirm_recursive_manifest(root)
  utils::write.csv(
    wave_hashes, file.path(root, "wave_artifact_hashes.csv"),
    row.names = FALSE, quote = TRUE
  )
  collected <- environment$rqr_confirm_collect_outputs(
    root, plan, contract
  )
  expect_true(collected$analysis_complete)
  expect_identical(nrow(collected$results), length(methods))
  expect_true(all(collected$replications$exact_method_set))

  write("tamper", file.path(replication_dir, "replication_results.csv"))
  expect_error(
    environment$rqr_confirm_collect_outputs(root, plan, contract),
    "byte verification"
  )
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

test_that("wave plans and task subsets preserve the frozen execution order", {
  environment <- load_confirmatory_helpers()
  contract <- confirmatory_contract(environment)
  plan <- environment$rqr_confirm_wave_plan(contract, "maximum")
  tasks <- environment$rqr_confirm_replication_plan(contract, "maximum")
  expect_identical(
    sort(plan$replication_task_id, method = "radix"),
    sort(tasks$replication_task_id, method = "radix")
  )
  expect_identical(plan$execution_order, seq_len(nrow(plan)))
  expect_true(all(plan$worker_slot >= 1L))
  expect_true(all(plan$worker_slot <= plan$worker_limit))
  for (wave_id in unique(plan$wave_id)) {
    block <- plan[plan$wave_id == wave_id, , drop = FALSE]
    expect_identical(length(unique(block$phase)), 1L)
    expect_identical(length(unique(block$mode)), 1L)
    expect_identical(length(unique(block$worker_limit)), 1L)
  }
  subset <- tasks[c(1L, 7L, 13L), , drop = FALSE]
  expect_identical(
    environment$rqr_confirm_validate_task_subset(subset, contract),
    subset
  )
  bad <- subset
  bad$replication[[1L]] <- bad$replication[[1L]] + 1L
  expect_error(
    environment$rqr_confirm_validate_task_subset(bad, contract),
    "not an exact canonical subset"
  )
  expect_error(
    environment$rqr_confirm_validate_task_subset(
      rbind(subset, subset[1L, , drop = FALSE]), contract
    ),
    "invalid schema or task IDs"
  )
})

test_that("wave transitions enforce sentinel, predecessor, and batch evidence", {
  environment <- load_confirmatory_helpers()
  digest_a <- paste(rep("a", 64L), collapse = "")
  digest_b <- paste(rep("b", 64L), collapse = "")
  catalog <- data.frame(
    canonical_wave_index = 1:4,
    wave_id = c(
      "group__target0100__sentinel",
      "group__target0100__standard",
      "group__target0200__sentinel",
      "group__target0200__standard"
    ),
    mode = c(
      "sentinel-core", "execute-confirmatory",
      "sentinel-core", "execute-confirmatory"
    ),
    phase = rep(c("sentinel", "standard"), 2L),
    batch_group = "group",
    batch_target = rep(c(100L, 200L), each = 2L),
    batch_sequence = rep(1:2, each = 2L),
    study_stage = "core",
    worker_limit = 1L, task_count = 1L,
    same_batch_sentinel_wave_id = c(
      "", "group__target0100__sentinel",
      "", "group__target0200__sentinel"
    ),
    prior_batch_target = c(NA, NA, 100L, 100L),
    required_predecessor_wave_ids = c(
      "",
      "group__target0100__sentinel",
      paste(
        "group__target0100__sentinel",
        "group__target0100__standard", sep = "|"
      ),
      paste(
        "group__target0100__sentinel",
        "group__target0100__standard",
        "group__target0200__sentinel", sep = "|"
      )
    ),
    stringsAsFactors = FALSE
  )
  first <- environment$rqr_confirm_wave_state_transition(
    catalog, NULL, catalog$wave_id[[1L]], digest_a
  )
  expect_identical(first$action, "launch")
  expect_error(
    environment$rqr_confirm_wave_state_transition(
      catalog, NULL, catalog$wave_id[[2L]], digest_a
    ),
    "Only the next"
  )
  completion <- function(indices, decisions = rep("passed", length(indices)),
                         binding = digest_a) {
    data.frame(
      canonical_wave_index = indices,
      wave_id = catalog$wave_id[indices],
      binding_digest = binding,
      decision = decisions,
      completion_sha256 = vapply(
        indices,
        function(index) paste(rep(as.character(index), 64L),
                              collapse = ""),
        character(1L)
      ),
      artifact_manifest_sha256 = vapply(
        indices,
        function(index) paste(rep("f", 64L), collapse = ""),
        character(1L)
      ),
      stringsAsFactors = FALSE
    )
  }
  failed <- completion(1L, "failed")
  expect_error(
    environment$rqr_confirm_wave_state_transition(
      catalog, failed, catalog$wave_id[[2L]], digest_a
    ),
    "permanently blocks"
  )
  expect_error(
    environment$rqr_confirm_wave_state_transition(
      catalog, completion(1L), catalog$wave_id[[1L]], digest_a
    ),
    "Only the next"
  )
  second <- environment$rqr_confirm_wave_state_transition(
    catalog, completion(1L), catalog$wave_id[[2L]], digest_a
  )
  expect_true(second$same_batch_sentinel_pass)
  expect_error(
    environment$rqr_confirm_wave_state_transition(
      catalog, completion(1:2), catalog$wave_id[[3L]], digest_a
    ),
    "prior batch decision"
  )
  decision <- data.frame(
    batch_group = "group", replications = 100L,
    next_action = "add_complete_paired_DGP_batch",
    next_replications = 200L, binding_digest = digest_a,
    decision_sha256 = digest_b, stringsAsFactors = FALSE
  )
  third <- environment$rqr_confirm_wave_state_transition(
    catalog, completion(1:2), catalog$wave_id[[3L]], digest_a,
    decision
  )
  expect_identical(third$action, "launch")
  precision_stop <- decision
  precision_stop$next_action <- "precision_pass_stop"
  precision_stop$next_replications <- 100L
  third_stop <- environment$rqr_confirm_wave_state_transition(
    catalog, completion(1:2), catalog$wave_id[[3L]], digest_a,
    precision_stop
  )
  expect_identical(third_stop$action, "skip")
  stopped_history <- completion(
    1:3, c("passed", "passed", "skipped_precision_stop")
  )
  fourth_stop <- environment$rqr_confirm_wave_state_transition(
    catalog, stopped_history, catalog$wave_id[[4L]], digest_a,
    precision_stop
  )
  expect_identical(fourth_stop$action, "skip")
  fractional <- decision
  fractional$replications <- 100.5
  expect_error(
    environment$rqr_confirm_wave_state_transition(
      catalog, completion(1:2), catalog$wave_id[[3L]], digest_a,
      fractional
    ),
    "finite whole number"
  )
  expect_error(
    environment$rqr_confirm_wave_state_transition(
      catalog, completion(1:2, binding = digest_b),
      catalog$wave_id[[3L]], digest_a, decision
    ),
    "completion history"
  )
})

test_that("append-only wave records reject incomplete or altered history", {
  environment <- load_confirmatory_helpers()
  contract <- confirmatory_contract(environment)
  catalog <- environment$rqr_confirm_wave_catalog(contract, "maximum")
  root <- tempfile("rqr-wave-state-")
  dir.create(root)
  wave_output_base <- file.path(root, "wave-outputs")
  dir.create(wave_output_base)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  binding <- list(
    schema_version = "rqrgibbs_dlm_wave_run/1.1.0",
    run_id = "fixture",
    authorization_commit = paste(rep("a", 40L), collapse = ""),
    reviewed_implementation_commit = paste(rep("b", 40L), collapse = ""),
    runtime_tree_digest = paste(rep("c", 64L), collapse = ""),
    config_sha256 = paste(rep("d", 64L), collapse = ""),
    incidence_sha256 = paste(rep("e", 64L), collapse = ""),
    seed_ledger_sha256 = paste(rep("f", 64L), collapse = ""),
    task_plan_sha256 = paste(rep("1", 64L), collapse = ""),
    wave_plan_sha256 = paste(rep("2", 64L), collapse = ""),
    wave_output_base = normalizePath(
      wave_output_base, winslash = "/", mustWork = TRUE
    ),
    binding_digest = paste(rep("3", 64L), collapse = "")
  )
  dir.create(file.path(root, "starts"))
  dir.create(file.path(root, "completions"))
  environment$rqr_confirm_atomic_write_json(
    c(binding, list(canonical_wave_count = nrow(catalog))),
    file.path(root, "run_contract.json")
  )
  wave <- catalog[1L, , drop = FALSE]
  filename <- sprintf("0001__%s.json", wave$wave_id)
  start_path <- file.path(root, "starts", filename)
  completion_path <- file.path(root, "completions", filename)
  output_root <- file.path(
    wave_output_base, sprintf("0001__%s", wave$wave_id)
  )
  dir.create(output_root)
  wave_manifest_path <- file.path(
    output_root, "wave_artifact_hashes.csv"
  )
  environment$rqr_confirm_atomic_write_csv(
    data.frame(
      path = "fixture.txt", bytes = 1,
      sha256 = paste(rep("a", 64L), collapse = "")
    ),
    wave_manifest_path
  )
  wave_manifest_digest <- environment$rqr_confirm_sha256(
    wave_manifest_path
  )
  task_digest <- paste(rep("4", 64L), collapse = "")
  started <- list(
    schema_version = "rqrgibbs_dlm_wave_start/1.0.0",
    canonical_wave_index = 1L, wave_id = wave$wave_id,
    mode = wave$mode, phase = wave$phase,
    batch_group = wave$batch_group,
    batch_target = wave$batch_target,
    binding_digest = binding$binding_digest,
    action = "launch",
    required_predecessor_wave_ids = character(),
    predecessor_completion_sha256 = character(),
    predecessor_artifact_manifest_sha256 = character(),
    same_batch_sentinel_pass = NA,
    prior_batch_decision_sha256 = "",
    prior_batch_next_action = "",
    worker_limit = wave$worker_limit,
    task_count = wave$task_count,
    wave_task_plan_sha256 = task_digest,
    output_root = output_root,
    started_at_utc = "2026-07-25 12:00:00 UTC"
  )
  environment$rqr_confirm_atomic_write_json(started, start_path)
  expect_error(
    environment$rqr_confirm_wave_state_records(
      root, catalog, binding
    ),
    "incomplete wave start"
  )
  active_records <- environment$rqr_confirm_wave_state_records(
    root, catalog, binding, allow_active_start = TRUE
  )
  expect_length(active_records$completion_values, 0L)
  expect_identical(
    as.character(active_records$active_start$wave_id),
    as.character(wave$wave_id)
  )
  completed <- c(
    started[c(
      "canonical_wave_index", "wave_id", "mode", "phase",
      "batch_group", "batch_target", "binding_digest", "action"
    )],
    list(
      schema_version =
        "rqrgibbs_dlm_wave_completion/1.0.0",
      decision = "passed",
      start_sha256 =
        environment$rqr_confirm_sha256(start_path)
    ),
    started[c(
      "required_predecessor_wave_ids",
      "predecessor_completion_sha256",
      "predecessor_artifact_manifest_sha256",
      "same_batch_sentinel_pass",
      "prior_batch_decision_sha256", "prior_batch_next_action",
      "worker_limit", "task_count",
      "wave_task_plan_sha256", "output_root"
    )],
    list(
      workers_used = 1L,
      wave_artifact_hashes_sha256 =
        wave_manifest_digest,
      all_workers_passed = TRUE,
      completed_at_utc = "2026-07-25 12:01:00 UTC"
    )
  )
  environment$rqr_confirm_atomic_write_json(
    completed, completion_path
  )
  records <- environment$rqr_confirm_wave_state_records(
    root, catalog, binding
  )
  expect_identical(nrow(records$completions), 1L)
  write("tampered", wave_manifest_path)
  expect_error(
    environment$rqr_confirm_wave_state_records(
      root, catalog, binding
    ),
    "detached from its artifact manifest"
  )
  unlink(wave_manifest_path)
  environment$rqr_confirm_atomic_write_csv(
    data.frame(
      path = "fixture.txt", bytes = 1,
      sha256 = paste(rep("a", 64L), collapse = "")
    ),
    wave_manifest_path
  )
  unlink(completion_path)
  completed$wave_id <- "altered-wave"
  jsonlite::write_json(
    completed, completion_path,
    auto_unbox = TRUE, pretty = TRUE, null = "null"
  )
  expect_error(
    environment$rqr_confirm_wave_state_records(
      root, catalog, binding
    ),
    "immutable schema"
  )
  correct_completion <- completed
  correct_completion$wave_id <- wave$wave_id
  unlink(completion_path)
  environment$rqr_confirm_atomic_write_json(
    correct_completion, completion_path
  )
  expect_identical(
    nrow(environment$rqr_confirm_wave_state_records(
      root, catalog, binding
    )$completions),
    1L
  )
  second_wave <- catalog[2L, , drop = FALSE]
  second_filename <- sprintf("0002__%s.json", second_wave$wave_id)
  second_start_path <- file.path(
    root, "starts", second_filename
  )
  second_completion_path <- file.path(
    root, "completions", second_filename
  )
  second_output_root <- file.path(
    wave_output_base, sprintf("0002__%s", second_wave$wave_id)
  )
  dir.create(second_output_root)
  second_manifest_path <- file.path(
    second_output_root, "wave_artifact_hashes.csv"
  )
  environment$rqr_confirm_atomic_write_csv(
    data.frame(
      path = "failed-fixture.txt", bytes = 1,
      sha256 = paste(rep("b", 64L), collapse = "")
    ),
    second_manifest_path
  )
  predecessor_completion <- environment$rqr_confirm_sha256(
    completion_path
  )
  second_started <- list(
    schema_version = "rqrgibbs_dlm_wave_start/1.0.0",
    canonical_wave_index = 2L,
    wave_id = second_wave$wave_id,
    mode = second_wave$mode, phase = second_wave$phase,
    batch_group = second_wave$batch_group,
    batch_target = second_wave$batch_target,
    binding_digest = binding$binding_digest,
    action = "launch",
    required_predecessor_wave_ids = wave$wave_id,
    predecessor_completion_sha256 = predecessor_completion,
    predecessor_artifact_manifest_sha256 =
      wave_manifest_digest,
    same_batch_sentinel_pass = NA,
    prior_batch_decision_sha256 = "",
    prior_batch_next_action = "",
    worker_limit = second_wave$worker_limit,
    task_count = second_wave$task_count,
    wave_task_plan_sha256 = paste(rep("5", 64L), collapse = ""),
    output_root = second_output_root,
    started_at_utc = "2026-07-25 12:02:00 UTC"
  )
  environment$rqr_confirm_atomic_write_json(
    second_started, second_start_path
  )
  second_completed <- c(
    second_started[c(
      "canonical_wave_index", "wave_id", "mode", "phase",
      "batch_group", "batch_target", "binding_digest", "action"
    )],
    list(
      schema_version =
        "rqrgibbs_dlm_wave_completion/1.0.0",
      decision = "failed",
      start_sha256 =
        environment$rqr_confirm_sha256(second_start_path)
    ),
    second_started[c(
      "required_predecessor_wave_ids",
      "predecessor_completion_sha256",
      "predecessor_artifact_manifest_sha256",
      "same_batch_sentinel_pass",
      "prior_batch_decision_sha256", "prior_batch_next_action",
      "worker_limit", "task_count",
      "wave_task_plan_sha256", "output_root"
    )],
    list(
      workers_used = 1L,
      wave_artifact_hashes_sha256 =
        environment$rqr_confirm_sha256(second_manifest_path),
      all_workers_passed = FALSE,
      completed_at_utc = "2026-07-25 12:03:00 UTC"
    )
  )
  environment$rqr_confirm_atomic_write_json(
    second_completed, second_completion_path
  )
  failed_records <- environment$rqr_confirm_wave_state_records(
    root, catalog, binding
  )
  expect_identical(nrow(failed_records$completions), 2L)
  expect_identical(
    failed_records$completions$decision,
    c("passed", "failed")
  )
  expect_error(
    environment$rqr_confirm_wave_state_transition(
      catalog, failed_records$completions,
      catalog$wave_id[[3L]], binding$binding_digest
    ),
    "failed wave permanently blocks"
  )
  unlink(c(
    start_path, completion_path,
    second_start_path, second_completion_path
  ))
  expect_error(
    environment$rqr_confirm_wave_state_records(
      root, catalog, binding
    ),
    "orphaned or missing"
  )
  alternate_root <- file.path(root, "fresh-alternate-output")
  expect_error(
    environment$rqr_confirm_require_wave_output_root(
      alternate_root, binding, wave
    ),
    "authorization-bound canonical path"
  )
  expect_false(file.exists(alternate_root))
})

test_that("the direct wave launcher rejects alternate output before publication", {
  launcher_path <- testthat::test_path(
    "..", "..", "scripts",
    "17_launch_rqr_dlm_confirmatory_wave.R"
  )
  launcher <- readLines(launcher_path, warn = FALSE)
  guard_line <- grep(
    "^output_root <- rqr_confirm_require_wave_output_root\\(",
    launcher
  )
  start_line <- grep("^start_record <- list\\(", launcher)
  output_creation_line <- grep(
    "^dir\\.create\\(output_root,", launcher
  )
  expect_length(guard_line, 1L)
  expect_length(start_line, 1L)
  expect_length(output_creation_line, 1L)
  expect_lt(guard_line, start_line)
  expect_lt(guard_line, output_creation_line)
  expect_true(any(grepl(
    'wave_output_base = "RQR_CONFIRMATORY_WAVE_OUTPUT_BASE"',
    launcher, fixed = TRUE
  )))
  primary_start <- grep(
    "^primary_attestation <- readRDS\\(", launcher
  )
  authorization_fields <- grep(
    "^required_authorization <- c\\(", launcher
  )
  expect_length(primary_start, 1L)
  expect_length(authorization_fields, 1L)
  primary_block <- launcher[
    primary_start[[1L]]:(authorization_fields[[1L]] - 1L)
  ]
  expect_false(any(grepl(
    "jsonlite::read_json", primary_block, fixed = TRUE
  )))
  expect_true(any(grepl(
    "required_files[[\"primary_attestation\"]]",
    primary_block, fixed = TRUE
  )))
  expect_true(any(grepl(
    "primary_attestation$runtime_package_tree_digest",
    launcher, fixed = TRUE
  )))
  expect_true(any(grepl(
    '"bash", c(wrapper, mode, worker_output)',
    launcher, fixed = TRUE
  )))
  expect_false(any(grepl(
    "file.access(wrapper", launcher, fixed = TRUE
  )))
})

test_that("the coordinator invokes monitored shell stages through bash", {
  coordinator_path <- testthat::test_path(
    "..", "..", "scripts",
    "18_orchestrate_rqr_dlm_confirmatory_simulation.R"
  )
  coordinator <- readLines(coordinator_path, warn = FALSE)
  expect_true(any(grepl(
    '"bash", c(wrapper, "collect", output)',
    coordinator, fixed = TRUE
  )))
  expect_true(any(grepl(
    '"bash", c(wrapper, "audit", final_audit)',
    coordinator, fixed = TRUE
  )))
  expect_false(any(grepl(
    "file.access(wrapper", coordinator, fixed = TRUE
  )))
})

test_that("the detached launcher reads the official primary RDS attestation", {
  launcher_path <- testthat::test_path(
    "..", "..", "scripts",
    "20_launch_rqr_dlm_confirmatory_simulation.sh"
  )
  launcher <- readLines(launcher_path, warn = FALSE)
  primary_start <- grep(
    '^primary_runtime_path="\\$\\($', launcher
  )
  exdqlm_start <- grep(
    "^exdqlm_runtime_path=", launcher
  )
  expect_length(primary_start, 1L)
  expect_length(exdqlm_start, 1L)
  primary_block <- launcher[
    primary_start[[1L]]:(exdqlm_start[[1L]] - 1L)
  ]
  expect_true(any(grepl(
    "attestation <- readRDS", primary_block, fixed = TRUE
  )))
  expect_true(any(grepl(
    "attestation$runtime_package_path",
    primary_block, fixed = TRUE
  )))
  expect_false(any(grepl(
    "jq ", primary_block, fixed = TRUE
  )))
})

test_that("toolchain binding is subprocess-free inside monitored workers", {
  environment <- load_confirmatory_helpers()
  manifest <- environment$rqr_confirm_toolchain_manifest()
  expect_true(all(c(
    "CC", "CXX17", "R_Makeconf_sha256"
  ) %in% manifest$key))
  expect_match(
    manifest$value[manifest$key == "R_Makeconf_sha256"],
    "^[0-9a-f]{64}$"
  )
  expect_false(grepl(
    "system2",
    paste(deparse(body(
      environment$rqr_confirm_toolchain_manifest
    )), collapse = "\n"),
    fixed = TRUE
  ))
})

test_that("diagnostics require time-local terminal and future estimands", {
  skip_if_not_installed("posterior")
  environment <- load_confirmatory_helpers()
  contract <- confirmatory_contract(environment)
  ledger <- small_confirmatory_ledger(environment, contract)
  generated <- environment$rqr_confirm_generate_dgp(
    contract, "S05", 1L, ledger
  )
  required <- environment$rqr_confirm_diagnostic_schema(
    "M11", generated, contract
  )
  expect_true(all(c(
    "training_t0001_lower", "terminal_width",
    "future_h20_midpoint", "log_lambda", "log_q_1"
  ) %in% required))
  chain <- matrix(
    seq_len(40L * length(required)),
    nrow = 40L, ncol = length(required),
    dimnames = list(NULL, required)
  )
  missing_future <- chain[
    , !grepl("^future_h", colnames(chain)), drop = FALSE
  ]
  expect_error(
    environment$rqr_confirm_chain_diagnostics(
      rep(list(missing_future), 4L), contract,
      sentinel = TRUE, method = "M11", generated = generated
    ),
    "exact required estimand schema"
  )
  missing_terminal <- chain[
    , !grepl("^terminal_", colnames(chain)), drop = FALSE
  ]
  expect_error(
    environment$rqr_confirm_chain_diagnostics(
      rep(list(missing_terminal), 4L), contract,
      sentinel = TRUE, method = "M11", generated = generated
    ),
    "exact required estimand schema"
  )
  model_bundle <- environment$rqr_confirm_model_bundle(generated)
  draws <- 40L
  p <- length(model_bundle$training$m0)
  components <- length(model_bundle$training$component_dims)
  fit <- structure(
    list(
      samp.eta_root1 = matrix(-1, generated$T, draws),
      samp.eta_root2 = matrix(2, generated$T, draws),
      samp.theta_terminal_root1 = matrix(0, p, draws),
      samp.theta_terminal_root2 = matrix(1, p, draws),
      samp.lambda = rep(1, draws),
      samp.evolution_scale = matrix(
        1, draws, components
      )
    ),
    class = "rqr_dlm_mcmc"
  )
  extracted <- environment$rqr_confirm_scalar_draws(
    list(fit = fit), generated, contract, "M11"
  )
  expect_identical(colnames(extracted), required)
  fit_missing_lambda <- fit
  fit_missing_lambda$samp.lambda <- NULL
  expect_error(
    environment$rqr_confirm_scalar_draws(
      list(fit = fit_missing_lambda), generated, contract, "M11"
    ),
    "exact schema"
  )
  fit$samp.evolution_scale <- NULL
  expect_error(
    environment$rqr_confirm_scalar_draws(
      list(fit = fit), generated, contract, "M11"
    ),
    "exact schema"
  )
  lower_a <- rbind(rep(-1, generated$T), rep(-2, generated$T))
  lower_a <- t(lower_a)
  upper_a <- lower_a + 3
  lower_b <- lower_a
  upper_b <- upper_a
  lower_b[1L, ] <- lower_b[1L, ] + c(1, -1)
  lower_b[2L, ] <- lower_b[2L, ] + c(-1, 1)
  upper_b[1L, ] <- upper_b[1L, ] + c(1, -1)
  upper_b[2L, ] <- upper_b[2L, ] + c(-1, 1)
  expect_equal(colMeans(lower_a), colMeans(lower_b))
  local_a <- environment$rqr_confirm_interval_function_draws(
    lower_a, upper_a, 1L, "training_t0001"
  )
  local_b <- environment$rqr_confirm_interval_function_draws(
    lower_b, upper_b, 1L, "training_t0001"
  )
  expect_false(identical(local_a, local_b))
  terminal <- rbind(c(1, 2), c(0.5, 1.5))
  conditional <- environment$rqr_confirm_conditional_root_draws(
    terminal,
    FF_future = matrix(c(1, 0, 1, 0), 2L, 2L),
    GG_future = diag(2L)
  )
  expect_identical(conditional, matrix(c(1, 1, 2, 2), 2L, 2L))
})

test_that("recursive manifests reject altered bytes, file sets, and symlinks", {
  environment <- load_confirmatory_helpers()
  directory <- tempfile("rqr-confirm-manifest-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE, force = TRUE), add = TRUE)
  writeLines("one", file.path(directory, "one.txt"))
  manifest <- environment$rqr_confirm_recursive_manifest(directory)
  environment$rqr_confirm_atomic_write_csv(
    manifest, file.path(directory, "artifact_hashes.csv")
  )
  expect_identical(
    environment$rqr_confirm_verify_recursive_manifest(directory),
    manifest
  )
  writeLines("changed", file.path(directory, "one.txt"))
  expect_error(
    environment$rqr_confirm_verify_recursive_manifest(directory),
    "byte verification"
  )
  unlink(file.path(directory, "artifact_hashes.csv"))
  writeLines("one", file.path(directory, "one.txt"))
  manifest <- environment$rqr_confirm_recursive_manifest(directory)
  environment$rqr_confirm_atomic_write_csv(
    manifest, file.path(directory, "artifact_hashes.csv")
  )
  writeLines("extra", file.path(directory, "extra.txt"))
  expect_error(
    environment$rqr_confirm_verify_recursive_manifest(directory),
    "wrong exact file set"
  )
  unlink(c(
    file.path(directory, "artifact_hashes.csv"),
    file.path(directory, "extra.txt")
  ))
  unlink(file.path(directory, "one.txt"))
  writeLines("target", file.path(directory, "target.txt"))
  file.symlink("target.txt", file.path(directory, "one.txt"))
  manifest <- environment$rqr_confirm_recursive_manifest(directory)
  environment$rqr_confirm_atomic_write_csv(
    manifest, file.path(directory, "artifact_hashes.csv")
  )
  expect_error(
    environment$rqr_confirm_verify_recursive_manifest(directory),
    "byte verification"
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
