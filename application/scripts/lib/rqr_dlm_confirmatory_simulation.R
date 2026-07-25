# Frozen helpers for the confirmatory RQR-DLM simulation.
#
# These helpers implement design, RNG, DGP, reference, fit-plan, and artifact
# contracts.  Execution remains controlled by two false flags in the versioned
# config and by a later commit-bound authorization bundle.

`%||%` <- function(a, b) if (is.null(a)) b else a

rqr_confirm_schema <- function() {
  "rqrgibbs_dlm_main_simulation/1.0.0"
}

rqr_confirm_strict_integer <- function(x, name, minimum = 0L,
                                       maximum = .Machine$integer.max) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) ||
      !is.finite(x) || x != floor(x) || x < minimum ||
      x > maximum) {
    stop(
      sprintf("%s must be one finite whole number in [%s, %s].",
              name, minimum, maximum),
      call. = FALSE
    )
  }
  as.integer(x)
}

rqr_confirm_sha256 <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("Required artifact is missing: %s.", path), call. = FALSE)
  }
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

rqr_confirm_read_contract <- function(repo_root) {
  repo_root <- normalizePath(repo_root, winslash = "/", mustWork = TRUE)
  environment <- new.env(parent = baseenv())
  sys.source(
    file.path(
      repo_root, "application", "config", "rqr_dlm",
      "rqr_dlm_main_simulation_20260724.R"
    ),
    envir = environment
  )
  config <- environment$rqr_dlm_main_simulation
  paths <- lapply(
    config$review_contract[c("incidence_path", "budget_path", "gates_path")],
    function(path) file.path(repo_root, path)
  )
  incidence <- utils::read.csv(
    paths$incidence_path, stringsAsFactors = FALSE, check.names = FALSE
  )
  budget <- utils::read.csv(
    paths$budget_path, stringsAsFactors = FALSE, check.names = FALSE
  )
  gates <- utils::read.csv(
    paths$gates_path, stringsAsFactors = FALSE, check.names = FALSE
  )
  list(
    repo_root = repo_root, config = config, incidence = incidence,
    budget = budget, gates = gates, paths = paths
  )
}

rqr_confirm_included <- function(incidence) {
  incidence$role != "O" & incidence$replication_rule != "0"
}

rqr_confirm_validate_contract <- function(contract, require_closed = FALSE) {
  if (!is.list(contract) ||
      !all(c("config", "incidence", "budget", "gates", "paths") %in%
           names(contract))) {
    stop("The confirmatory contract is incomplete.", call. = FALSE)
  }
  config <- contract$config
  if (!identical(config$schema_version, rqr_confirm_schema()) ||
      !is.logical(config$diagnostic_pilot_execution_authorized) ||
      length(config$diagnostic_pilot_execution_authorized) != 1L ||
      is.na(config$diagnostic_pilot_execution_authorized) ||
      !is.logical(config$confirmatory_execution_authorized) ||
      length(config$confirmatory_execution_authorized) != 1L ||
      is.na(config$confirmatory_execution_authorized) ||
      !isTRUE(config$generalized_bayes) ||
      isTRUE(config$response_likelihood) ||
      isTRUE(config$response_prediction_contract) ||
      !identical(config$design$candidate_tuning_fits, 0L) ||
      !identical(
        config$authorization_contract$schema_version,
        "rqrgibbs_dlm_confirmatory_authorization/1.0.0"
      ) ||
      !isTRUE(
        config$authorization_contract$
          authorization_diff_must_only_flip_confirmatory_flag
      ) ||
      !isTRUE(
        config$authorization_contract$
          explicit_user_confirmation_required
      ) ||
      !identical(config$resources$threads_per_worker, 1L) ||
      !identical(
        config$resources$sampled_process_group_thread_ceiling, 2L
      ) ||
      !identical(
        config$resources$
          sampled_reference_process_group_thread_ceiling, 4L
      ) ||
      !identical(
        config$resources$sampled_thread_ceiling_role,
        "hard_OS_thread_envelope_not_compute_parallelism"
      )) {
    stop("The confirmatory configuration is invalid.", call. = FALSE)
  }
  if (isTRUE(require_closed) &&
      (isTRUE(config$diagnostic_pilot_execution_authorized) ||
       isTRUE(config$confirmatory_execution_authorized))) {
    stop("The confirmatory configuration is not fail closed.", call. = FALSE)
  }
  expected_modes <- c(
    "preflight", "oracle-reference", "sentinel-core",
    "execute-confirmatory", "collect", "audit"
  )
  if (!identical(config$implemented_modes, expected_modes)) {
    stop("Runner modes differ from the frozen contract.", call. = FALSE)
  }
  hashes <- c(
    incidence = rqr_confirm_sha256(contract$paths$incidence_path),
    budget = rqr_confirm_sha256(contract$paths$budget_path),
    gates = rqr_confirm_sha256(contract$paths$gates_path)
  )
  expected_hashes <- unlist(config$review_contract[
    c("incidence_sha256", "budget_sha256", "gates_sha256")
  ], use.names = FALSE)
  if (!identical(unname(hashes), unname(expected_hashes))) {
    stop("An Output-15 design artifact digest changed.", call. = FALSE)
  }
  incidence <- contract$incidence
  required <- c(
    "cell_id", "DGP", "coverage", "method", "role",
    "replication_rule", "chains_per_replication", "tuning_rule",
    "forecast_horizons", "primary_estimands",
    "paired_contrast_group", "include_or_omit_reason"
  )
  if (!identical(names(incidence), required) ||
      nrow(incidence) != config$review_contract$incidence_rows ||
      anyDuplicated(incidence$cell_id) ||
      !identical(sort(unique(incidence$DGP)), names(config$scenarios)) ||
      !identical(sort(unique(incidence$method)), names(config$methods))) {
    stop("The incidence matrix has the wrong schema or ID sets.",
         call. = FALSE)
  }
  included <- rqr_confirm_included(incidence)
  if (sum(included) != config$review_contract$included_rows ||
      sum(!included) != config$review_contract$omitted_rows ||
      any(included != incidence$include_or_omit_reason %in% c("i", "x")) ||
      any(incidence$include_or_omit_reason == "x" &
          incidence$replication_rule != "F")) {
    stop("Incidence inclusion and fixed-row markers are inconsistent.",
         call. = FALSE)
  }
  if (any(incidence$tuning_rule[included] == "-") ||
      any(incidence$tuning_rule[!included] != "-")) {
    stop("Included and omitted tuning markers are inconsistent.",
         call. = FALSE)
  }
  scenario_table <- do.call(rbind, lapply(
    names(config$scenarios),
    function(scenario_id) {
      scenario <- config$scenarios[[scenario_id]]
      data.frame(
        DGP = scenario_id, dgp = scenario$dgp, T = scenario$T,
        coverage = scenario$coverage,
        batch_group = scenario$batch_group %||% "",
        stringsAsFactors = FALSE
      )
    }
  ))
  if (any(!nzchar(scenario_table$batch_group)) ||
      anyDuplicated(scenario_table[c("batch_group", "coverage")])) {
    stop("Scenario batch groups are incomplete or duplicate a coverage.",
         call. = FALSE)
  }
  group_signatures <- split(
    paste(scenario_table$dgp, scenario_table$T, sep = "|"),
    scenario_table$batch_group
  )
  if (any(vapply(group_signatures, function(value) {
      length(unique(value)) != 1L
    }, logical(1L)))) {
    stop("A paired batch group mixes DGP mechanisms or training horizons.",
         call. = FALSE)
  }
  counts <- table(incidence$DGP)
  if (!all(counts == 13L)) {
    stop("Every scenario must have exactly 13 method rows.", call. = FALSE)
  }
  if (!identical(config$rng$kind, "L'Ecuyer-CMRG") ||
      config$oracle$profile_grid_size < 1601L ||
      config$oracle$profile_grid_size %% 2L != 1L ||
      config$oracle$higher_precision_grid_size <=
        config$oracle$profile_grid_size ||
      config$oracle$higher_precision_grid_size %% 2L != 1L ||
      config$oracle$higher_precision_tolerance >=
        config$oracle$primary_tolerance ||
      !isTRUE(config$oracle$higher_precision_crosscheck)) {
    stop("RNG or oracle hardening is incomplete.", call. = FALSE)
  }
  q <- config$dgp
  if (!identical(q$seasonal_period, 12L) ||
      !identical(q$seasonal_variance, 0.002) ||
      !identical(q$heteroscedastic_log_scale_ar, 0.80) ||
      !identical(
        q$heteroscedastic_log_scale_innovation_variance, 0.36
      ) ||
      !identical(q$heteroscedastic_log_scale_coefficient, 0.25) ||
      !identical(q$root_alignment$lower_initial, -2) ||
      !identical(q$root_alignment$upper_initial, 2) ||
      !identical(q$root_alignment$lower_variance, 0.001) ||
      !identical(q$root_alignment$upper_variance, 0.001) ||
      !identical(q$root_alignment$minimum_separation, 0.10)) {
    stop("The frozen seasonal, scale, or root-alignment DGP changed.",
         call. = FALSE)
  }
  reference_replications <- vapply(
    config$reference$byte_reproduction_replications,
    rqr_confirm_strict_integer, integer(1L),
    name = "byte_reproduction_replication", minimum = 1L
  )
  if (!config$reference$byte_reproduction_scenario %in%
      names(config$scenarios) ||
      !identical(reference_replications, c(1L, 2L)) ||
      !identical(
        rqr_confirm_strict_integer(
          config$reference$serialization_version,
          "serialization_version", 3L, 3L
        ),
        3L
      )) {
    stop("The byte-reproduction reference contract changed.",
         call. = FALSE)
  }
  profile_table <- do.call(rbind, lapply(
    config$initialization_profiles,
    function(profile) {
      c(
        midpoint_rule = profile$midpoint_rule,
        midpoint_shift_training_sd =
          as.character(profile$midpoint_shift_training_sd),
        half_width_multiplier =
          as.character(profile$half_width_multiplier),
        lambda_initial = as.character(profile$lambda_initial),
        component_scale_multiplier =
          as.character(profile$component_scale_multiplier)
      )
    }
  ))
  expected_profiles <- rbind(
    A = c("empirical_interval_midpoint", "0", "1", "0.5", "0.5"),
    B = c("empirical_interval_midpoint", "-0.5", "0.75", "1", "1"),
    C = c("empirical_interval_midpoint", "0.5", "1.25", "2", "2"),
    D = c("training_median", "0", "1.75", "4", "4")
  )
  colnames(expected_profiles) <- colnames(profile_table)
  if (!identical(profile_table, expected_profiles)) {
    stop("The frozen A-D initialization profiles changed.",
         call. = FALSE)
  }
  standard <- config$standard_initialization
  if (!identical(standard$midpoint_rule, "training_median") ||
      !identical(standard$midpoint_shift_training_sd, 0) ||
      !identical(standard$half_width_multiplier, 1) ||
      !identical(standard$lambda_initial, 1) ||
      !identical(standard$component_scale_multiplier, 1) ||
      !identical(
        standard$component_scale_reference,
        "inverse_gamma_prior_median"
      )) {
    stop("The frozen standard initialization changed.", call. = FALSE)
  }
  invisible(TRUE)
}

rqr_confirm_replication_count <- function(rule, planning) {
  planning <- match.arg(planning, c("initial", "central", "maximum"))
  switch(
    rule,
    C = c(initial = 200L, central = 400L, maximum = 600L)[[planning]],
    S = c(initial = 100L, central = 200L, maximum = 300L)[[planning]],
    F = 200L,
    `0` = 0L,
    stop("Unknown replication rule.", call. = FALSE)
  )
}

rqr_confirm_method_logical_fits <- function(method) {
  if (method == "M02") return(2L)
  if (method == "M04") return(2L)
  if (method == "M05") return(0L)
  1L
}

rqr_confirm_method_mcmc_chains <- function(method) {
  if (method == "M02") return(2L)
  if (method %in% c("M01", "M03", "M06", "M07", "M08",
                    "M09", "M10", "M11")) return(1L)
  0L
}

rqr_confirm_fit_plan <- function(contract, planning = "maximum") {
  rqr_confirm_validate_contract(contract)
  planning <- match.arg(planning, c("initial", "central", "maximum"))
  rows <- contract$incidence[rqr_confirm_included(contract$incidence), ,
                             drop = FALSE]
  rows$replications <- vapply(
    rows$replication_rule, rqr_confirm_replication_count,
    integer(1L), planning = planning
  )
  rows$method_name <- unname(unlist(
    contract$config$methods[rows$method], use.names = FALSE
  ))
  rows$logical_fits_per_replication <- vapply(
    rows$method, rqr_confirm_method_logical_fits, integer(1L)
  )
  rows$mcmc_chains_per_replication <- vapply(
    rows$method, rqr_confirm_method_mcmc_chains, integer(1L)
  )
  rows
}

rqr_confirm_stream_states <- function(keys, master_seed) {
  keys <- as.character(keys)
  if (!length(keys) || anyNA(keys) || any(!nzchar(keys)) ||
      anyDuplicated(keys)) {
    stop("RNG stream keys must be nonempty and unique.", call. = FALSE)
  }
  master_seed <- rqr_confirm_strict_integer(
    master_seed, "master_seed", 1L
  )
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
  set.seed(master_seed)
  state <- get(".Random.seed", envir = .GlobalEnv)
  states <- vector("list", length(keys))
  for (index in seq_along(keys)) {
    if (index > 1L) state <- parallel::nextRNGStream(state)
    states[[index]] <- state
  }
  setNames(states, keys)
}

rqr_confirm_sentinel_blueprint <- function(
    contract, planning = "maximum") {
  planning <- match.arg(planning, c("initial", "central", "maximum"))
  plan <- rqr_confirm_fit_plan(contract, planning)
  plan <- plan[plan$mcmc_chains_per_replication > 0L, , drop = FALSE]
  records <- vector("list", 0L)
  index <- 0L
  for (row_index in seq_len(nrow(plan))) {
    rule <- plan$replication_rule[[row_index]]
    n <- plan$replications[[row_index]]
    batches <- if (rule == "C") {
      split(seq_len(n), ceiling(seq_len(n) / 100))
    } else if (rule == "S") {
      split(seq_len(n), ceiling(seq_len(n) / 50))
    } else {
      list(seq_len(n))
    }
    for (batch_index in seq_along(batches)) {
      batch <- batches[[batch_index]]
      index <- index + 1L
      records[[index]] <- data.frame(
        cell_id = plan$cell_id[[row_index]],
        DGP = plan$DGP[[row_index]],
        method = plan$method[[row_index]],
        batch = batch_index,
        batch_start = min(batch),
        batch_end = max(batch),
        selection_task_key = paste(
          "sentinel_selection", plan$cell_id[[row_index]], batch_index,
          sep = "|"
        ),
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, records)
}

rqr_confirm_sentinel_map <- function(contract, planning = "maximum") {
  planning <- match.arg(planning, c("initial", "central", "maximum"))
  blueprint <- rqr_confirm_sentinel_blueprint(contract, planning)
  selection_keys <- sort(
    unique(blueprint$selection_task_key), method = "radix"
  )
  selection_states <- rqr_confirm_stream_states(
    selection_keys, contract$config$rng$master_seed
  )
  records <- vector("list", 0L)
  index <- 0L
  for (row_index in seq_len(nrow(blueprint))) {
    row <- blueprint[row_index, , drop = FALSE]
    state <- selection_states[[row$selection_task_key[[1L]]]]
    batch <- seq.int(row$batch_start[[1L]], row$batch_end[[1L]])
    chosen <- sort(rqr_confirm_with_state(
      state, sample(batch, size = min(2L, length(batch)), replace = FALSE)
    ))
    for (replication in chosen) {
      index <- index + 1L
      records[[index]] <- data.frame(
        cell_id = row$cell_id[[1L]],
        DGP = row$DGP[[1L]],
        method = row$method[[1L]],
        replication = replication,
        batch = row$batch[[1L]],
        profiles = "A|B|C|D",
        selection_task_key = row$selection_task_key[[1L]],
        selection_state_digest = rqr_confirm_state_digest(state),
        selected_before_data = TRUE,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, records)
}

rqr_confirm_budget_summary <- function(contract, planning = "maximum") {
  plan <- rqr_confirm_fit_plan(contract, planning)
  sentinel <- rqr_confirm_sentinel_map(contract, planning)
  method_calls <- sum(plan$replications)
  logical_fits <- sum(
    plan$replications * plan$logical_fits_per_replication
  )
  standard_chains <- sum(
    plan$replications * plan$mcmc_chains_per_replication
  )
  extra_sentinel <- sum(
    vapply(sentinel$method, rqr_confirm_method_mcmc_chains, integer(1L))
  ) * 3L
  data.frame(
    item = c(
      "method_interval_evaluations", "logical_endpoint_or_model_fits",
      "software_fit_calls", "standard_MCMC_chain_executions",
      "extra_preselected_sentinel_chains", "total_MCMC_chain_executions"
    ),
    value = c(
      method_calls, logical_fits, method_calls, standard_chains,
      extra_sentinel, standard_chains + extra_sentinel
    ),
    stringsAsFactors = FALSE
  )
}

rqr_confirm_validate_budget <- function(contract) {
  lookup <- c(
    method_interval_evaluations = "method_interval_evaluations",
    logical_endpoint_or_model_fits = "logical_endpoint_or_model_fits",
    software_fit_calls = "software_fit_calls",
    standard_MCMC_chain_executions = "standard_MCMC_chain_executions",
    extra_preselected_sentinel_chains =
      "extra_preselected_sentinel_chains",
    total_MCMC_chain_executions = "total_MCMC_chain_executions"
  )
  for (planning in c("initial", "central", "maximum")) {
    actual <- rqr_confirm_budget_summary(contract, planning)
    expected <- contract$budget[
      contract$budget$section %in% c("fits", "mcmc") &
        contract$budget$item %in% lookup,
      ,
      drop = FALSE
    ]
    expected <- expected[match(actual$item, expected$item), , drop = FALSE]
    if (anyNA(expected$item) ||
        !identical(
          as.numeric(actual$value),
          as.numeric(expected[[planning]])
        )) {
      stop(sprintf("The %s run budget was not reproduced.", planning),
           call. = FALSE)
    }
  }
  invisible(TRUE)
}

rqr_confirm_state_text <- function(state) {
  paste(as.integer(state), collapse = ";")
}

rqr_confirm_state_digest <- function(state) {
  digest::digest(as.integer(state), algo = "sha256", serialize = TRUE)
}

rqr_confirm_with_state <- function(state, expression) {
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
  assign(".Random.seed", as.integer(state), envir = .GlobalEnv)
  force(expression)
}

rqr_confirm_rng_task_keys <- function(contract, planning = "maximum") {
  plan <- rqr_confirm_fit_plan(contract, planning)
  sentinel_blueprint <- rqr_confirm_sentinel_blueprint(contract, planning)
  sentinel_selection_keys <- sort(
    unique(sentinel_blueprint$selection_task_key), method = "radix"
  )
  sentinel <- rqr_confirm_sentinel_map(contract, planning)
  chunks <- list(sentinel_selection_keys)
  chunk_index <- 1L
  append_chunk <- function(value) {
    chunk_index <<- chunk_index + 1L
    chunks[[chunk_index]] <<- value
  }
  scenario_rows <- unique(plan[c("DGP", "replications")])
  for (row_index in seq_len(nrow(scenario_rows))) {
    scenario_id <- scenario_rows$DGP[[row_index]]
    scenario <- contract$config$scenarios[[scenario_id]]
    data_id <- paste(scenario$dgp, scenario$T, sep = "_T")
    replication <- seq_len(scenario_rows$replications[[row_index]])
    append_chunk(c(
      paste("training_state", scenario$pair, replication, sep = "|"),
      paste("training_response", data_id, replication, sep = "|"),
      paste("future_state", scenario$pair, replication, sep = "|"),
      paste("future_response", data_id, replication, sep = "|")
    ))
  }
  for (row_index in seq_len(nrow(plan))) {
    cell <- plan$cell_id[[row_index]]
    method <- plan$method[[row_index]]
    endpoint_ids <- if (method == "M02") {
      c("lower", "upper")
    } else {
      "interval"
    }
    replications <- seq_len(plan$replications[[row_index]])
    sentinel_replications <- sentinel$replication[
      sentinel$cell_id == cell
    ]
    standard_replications <- setdiff(
      replications, sentinel_replications
    )
    for (endpoint in endpoint_ids) {
      if (length(standard_replications)) {
        append_chunk(c(
          paste(
            "method", cell, standard_replications, endpoint, 1L,
            sep = "|"
          ),
          paste(
            "initialization", cell, standard_replications, endpoint,
            "standard", sep = "|"
          ),
          paste(
            "forecast", cell, standard_replications, endpoint, 1L,
            sep = "|"
          )
        ))
      }
      if (length(sentinel_replications)) {
        for (chain in seq_len(4L)) {
          profile <- c("A", "B", "C", "D")[[chain]]
          append_chunk(c(
            paste(
              "method", cell, sentinel_replications, endpoint, chain,
              sep = "|"
            ),
            paste(
              "initialization", cell, sentinel_replications, endpoint,
              profile, sep = "|"
            ),
            paste(
              "forecast", cell, sentinel_replications, endpoint, chain,
              sep = "|"
            )
          ))
        }
      }
    }
  }
  append_chunk(paste("oracle", unique(plan$DGP), sep = "|"))
  keys <- unique(unlist(chunks, use.names = FALSE))
  c(
    sentinel_selection_keys,
    sort(
      setdiff(unique(keys), sentinel_selection_keys),
      method = "radix"
    )
  )
}

rqr_confirm_seed_ledger <- function(contract, planning = "maximum",
                                    include_state = TRUE) {
  rqr_confirm_validate_contract(contract)
  if (!isTRUE(include_state)) {
    stop(
      "Promotion-grade seed ledgers must retain every complete RNG state.",
      call. = FALSE
    )
  }
  keys <- rqr_confirm_rng_task_keys(contract, planning)
  states <- unname(rqr_confirm_stream_states(
    keys, contract$config$rng$master_seed
  ))
  ledger <- data.frame(
    task_key = keys,
    parent_task_key = NA_character_,
    stream_type = "stream",
    substream = NA_integer_,
    state_digest = vapply(
      states, rqr_confirm_state_digest, character(1L)
    ),
    stringsAsFactors = FALSE
  )
  if (isTRUE(include_state)) {
    ledger$state <- vapply(states, rqr_confirm_state_text, character(1L))
  }
  future_indices <- grep("^future_(state|response)\\|", keys)
  n_substreams <- contract$config$design$future_subreplications
  n_future_rows <- length(future_indices) * n_substreams
  future_task_key <- future_parent_key <-
    future_digest <- future_state_text <- character(n_future_rows)
  future_substream <- integer(n_future_rows)
  future_states <- vector("list", n_future_rows)
  for (position in seq_along(future_indices)) {
    parent_index <- future_indices[[position]]
    substate <- states[[parent_index]]
    offset <- (position - 1L) * n_substreams
    for (subreplication in seq_len(n_substreams)) {
      row <- offset + subreplication
      substate <- parallel::nextRNGSubStream(substate)
      future_states[[row]] <- substate
      future_task_key[[row]] <- paste0(
        keys[[parent_index]], "|subrep|", subreplication
      )
      future_parent_key[[row]] <- keys[[parent_index]]
      future_substream[[row]] <- subreplication
      future_digest[[row]] <- rqr_confirm_state_digest(substate)
      future_state_text[[row]] <- rqr_confirm_state_text(substate)
    }
  }
  future_rows <- data.frame(
    task_key = future_task_key,
    parent_task_key = future_parent_key,
    stream_type = rep("substream", n_future_rows),
    substream = future_substream,
    state_digest = future_digest,
    state = future_state_text,
    stringsAsFactors = FALSE
  )
  ledger <- rbind(ledger, future_rows)
  if (anyDuplicated(ledger$task_key) ||
      anyDuplicated(ledger$state_digest)) {
    stop("The full L'Ecuyer state ledger contains a collision.",
         call. = FALSE)
  }
  attr(ledger, "states") <- c(
    setNames(states, keys),
    setNames(future_states, future_task_key)
  )
  ledger
}

rqr_confirm_state_from_ledger <- function(ledger, key) {
  states <- attr(ledger, "states")
  state <- states[[key]]
  if (is.null(state)) {
    if (!"state" %in% names(ledger)) {
      stop("The seed ledger omitted full states.", call. = FALSE)
    }
    row <- match(key, ledger$task_key)
    if (is.na(row)) stop(sprintf("Unknown RNG task key: %s.", key),
                         call. = FALSE)
    fields <- strsplit(
      ledger$state[[row]], ";", fixed = TRUE
    )[[1L]]
    values <- suppressWarnings(as.numeric(fields))
    if (length(values) != 7L || anyNA(values) ||
        any(!is.finite(values)) ||
        any(values != floor(values)) ||
        any(values < -.Machine$integer.max) ||
        any(values > .Machine$integer.max)) {
      stop("A serialized RNG state is not seven exact integers.",
           call. = FALSE)
    }
    state <- as.integer(values)
  }
  state
}

rqr_confirm_validate_seed_ledger <- function(
    ledger, contract, planning = "maximum", require_complete = TRUE) {
  planning <- match.arg(planning, c("initial", "central", "maximum"))
  required <- c(
    "task_key", "parent_task_key", "stream_type", "substream",
    "state_digest", "state"
  )
  if (!is.data.frame(ledger) ||
      !identical(names(ledger), required) ||
      !nrow(ledger) ||
      anyNA(ledger[c("task_key", "stream_type", "state_digest", "state")]) ||
      anyDuplicated(ledger$task_key) ||
      anyDuplicated(ledger$state_digest)) {
    stop("The full-state seed ledger has an invalid schema or IDs.",
         call. = FALSE)
  }
  cached_states <- attr(ledger, "states")
  states <- if (
      is.list(cached_states) &&
      identical(names(cached_states), ledger$task_key)) {
    unname(cached_states)
  } else {
    lapply(seq_len(nrow(ledger)), function(row) {
      fields <- strsplit(
        ledger$state[[row]], ";", fixed = TRUE
      )[[1L]]
      values <- suppressWarnings(as.numeric(fields))
      if (length(values) != 7L || anyNA(values) ||
          any(!is.finite(values)) ||
          any(values != floor(values)) ||
          any(values < -.Machine$integer.max) ||
          any(values > .Machine$integer.max)) {
        stop("A serialized RNG state is not seven exact integers.",
             call. = FALSE)
      }
      as.integer(values)
    })
  }
  digests <- vapply(states, rqr_confirm_state_digest, character(1L))
  if (!identical(digests, ledger$state_digest)) {
    stop("A full-state seed digest does not match its serialized state.",
         call. = FALSE)
  }
  stream <- ledger$stream_type == "stream"
  substream <- ledger$stream_type == "substream"
  if (any(!stream & !substream) ||
      any(!is.na(ledger$parent_task_key[stream]) &
          nzchar(ledger$parent_task_key[stream])) ||
      any(!is.na(ledger$substream[stream])) ||
      any(is.na(ledger$parent_task_key[substream]) |
          !nzchar(ledger$parent_task_key[substream]))) {
    stop("Seed-ledger stream and substream relationships are invalid.",
         call. = FALSE)
  }
  substream_numbers <- ledger$substream[substream]
  if (length(substream_numbers)) {
    if (anyNA(substream_numbers) ||
        any(!is.finite(substream_numbers)) ||
        any(substream_numbers != floor(substream_numbers)) ||
        any(substream_numbers < 1L) ||
        any(substream_numbers >
            contract$config$design$future_subreplications)) {
      stop("Seed-ledger substream indices are invalid.", call. = FALSE)
    }
    expected_substream_key <- paste0(
      ledger$parent_task_key[substream],
      "|subrep|", as.integer(substream_numbers)
    )
    if (!identical(expected_substream_key, ledger$task_key[substream])) {
      stop("Seed-ledger substream keys do not match their parents.",
           call. = FALSE)
    }
  }
  if (isTRUE(require_complete)) {
    expected_streams <- rqr_confirm_rng_task_keys(contract, planning)
    expected_keys <- expected_streams
    future <- grep(
      "^future_(state|response)\\|", expected_streams, value = TRUE
    )
    expected_keys <- c(
      expected_keys,
      unlist(lapply(future, function(parent) {
        paste0(
          parent, "|subrep|",
          seq_len(contract$config$design$future_subreplications)
        )
      }), use.names = FALSE)
    )
    expected_keys <- sort(expected_keys, method = "radix")
    observed_keys <- sort(ledger$task_key, method = "radix")
    if (!identical(observed_keys, expected_keys)) {
      stop("The full-state seed ledger is not the canonical complete set.",
           call. = FALSE)
    }
    sentinel <- rqr_confirm_sentinel_map(contract, planning)
    selection_rows <- match(
      sentinel$selection_task_key, ledger$task_key
    )
    if (anyNA(selection_rows) ||
        !identical(
          ledger$state_digest[selection_rows],
          sentinel$selection_state_digest
        )) {
      stop(
        "Sentinel selections are not bound to the full-state seed ledger.",
        call. = FALSE
      )
    }
  }
  attr(ledger, "states") <- setNames(states, ledger$task_key)
  ledger
}

rqr_confirm_error_draw <- function(family, n) {
  n <- rqr_confirm_strict_integer(n, "n", 1L)
  switch(
    family,
    gaussian = stats::rnorm(n),
    skewed = {
      logsd <- 0.75
      mean_raw <- exp(0.5 * logsd^2)
      sd_raw <- sqrt((exp(logsd^2) - 1) * exp(logsd^2))
      (stats::rlnorm(n, 0, logsd) - mean_raw) / sd_raw
    },
    t5 = stats::rt(n, df = 5) * sqrt(3 / 5),
    break_mixture = {
      component <- stats::runif(n) > 0.90
      raw <- stats::rnorm(n)
      raw[component] <- 2 + stats::rt(sum(component), df = 3)
      (raw - 0.2) / sqrt(1.6 - 0.2^2)
    },
    stop("Unknown error family.", call. = FALSE)
  )
}

rqr_confirm_error_family <- function(dgp) {
  if (grepl("gaussian$", dgp)) return("gaussian")
  if (dgp %in% c("local_level_skewed", "trend_seasonal_skewed",
                 "trend_regression_unequal", "root_alignment")) {
    return("skewed")
  }
  if (dgp == "heteroscedastic_t5") return("t5")
  if (dgp == "break_heavy_tail") return("break_mixture")
  stop("No error family is defined for the DGP.", call. = FALSE)
}

rqr_confirm_oracle_spec <- function(family) {
  switch(
    family,
    gaussian = list(family = "gaussian", params = list()),
    skewed = list(
      family = "centered_standardized_lognormal",
      params = list(logmean = 0, logsd = 0.75)
    ),
    t5 = list(
      family = "student_t",
      params = list(df = 5, scale = sqrt(3 / 5))
    ),
    break_mixture = list(
      family = "normal_t_mixture",
      params = list(
        normal_weight = 0.90, t_weight = 0.10, t_df = 3,
        t_shift = 2, t_scale = 1, variance_standardized = TRUE
      )
    ),
    stop("No oracle specification is defined.", call. = FALSE)
  )
}

rqr_confirm_harmonic_transition <- function(period = 12L) {
  angle <- 2 * pi / period
  matrix(
    c(cos(angle), sin(angle), -sin(angle), cos(angle)),
    2L, 2L, byrow = TRUE
  )
}

rqr_confirm_generate_training_state <- function(config, scenario, state) {
  T <- scenario$T
  dgp <- scenario$dgp
  q <- config$dgp
  rqr_confirm_with_state(state, {
    if (dgp == "static_gaussian") {
      x <- stats::rnorm(T)
      return(list(mu = 0.5 + 0.75 * x, scale = rep(1, T),
                  x = x, terminal = list()))
    }
    if (dgp %in% c("local_level_gaussian", "local_level_skewed")) {
      level <- numeric(T)
      level[[1L]] <- stats::rnorm(1)
      if (T > 1L) for (tt in 2:T) {
        level[[tt]] <- level[[tt - 1L]] +
          stats::rnorm(1, sd = sqrt(q$local_level_variance))
      }
      return(list(mu = level, scale = rep(1, T), x = numeric(T),
                  terminal = list(level = level[[T]]), level = level))
    }
    if (dgp %in% c(
        "trend_seasonal_gaussian", "trend_seasonal_skewed")) {
      transition <- rqr_confirm_harmonic_transition(q$seasonal_period)
      level <- slope <- numeric(T)
      seasonal <- matrix(0, 2L, T)
      level[[1L]] <- stats::rnorm(1)
      slope[[1L]] <- stats::rnorm(1, sd = sqrt(0.1))
      seasonal[, 1L] <- stats::rnorm(
        2L, sd = sqrt(q$seasonal_initial_covariance)
      )
      if (T > 1L) for (tt in 2:T) {
        slope[[tt]] <- slope[[tt - 1L]] +
          stats::rnorm(1, sd = sqrt(q$trend_slope_variance))
        level[[tt]] <- level[[tt - 1L]] + slope[[tt - 1L]] +
          stats::rnorm(1, sd = sqrt(q$trend_level_variance))
        seasonal[, tt] <- drop(transition %*% seasonal[, tt - 1L]) +
          stats::rnorm(2L, sd = sqrt(q$seasonal_variance))
      }
      scale <- pmax(
        q$scale_floor,
        1 + 0.30 * sin(2 * pi * seq_len(T) / q$seasonal_period)
      )
      return(list(
        mu = level + seasonal[1L, ], scale = scale, x = numeric(T),
        terminal = list(
          level = level[[T]], slope = slope[[T]],
          seasonal = seasonal[, T]
        ),
        level = level, slope = slope, seasonal = seasonal
      ))
    }
    if (dgp == "trend_regression_unequal") {
      level <- slope <- beta <- x <- numeric(T)
      level[[1L]] <- stats::rnorm(1)
      slope[[1L]] <- stats::rnorm(1, sd = sqrt(0.1))
      beta[[1L]] <- stats::rnorm(1, 0.5, 0.5)
      x[[1L]] <- stats::rnorm(1)
      if (T > 1L) for (tt in 2:T) {
        slope[[tt]] <- slope[[tt - 1L]] +
          stats::rnorm(1, sd = sqrt(q$trend_slope_variance))
        level[[tt]] <- level[[tt - 1L]] + slope[[tt - 1L]] +
          stats::rnorm(1, sd = sqrt(q$trend_level_variance))
        beta[[tt]] <- beta[[tt - 1L]] +
          stats::rnorm(1, sd = sqrt(q$regression_variance))
        x[[tt]] <- q$regression_predictor_ar * x[[tt - 1L]] +
          stats::rnorm(
            1, sd = sqrt(q$regression_predictor_innovation_variance)
          )
      }
      return(list(
        mu = level + beta * x, scale = rep(1, T), x = x,
        terminal = list(
          level = level[[T]], slope = slope[[T]],
          beta = beta[[T]], x = x[[T]]
        ),
        level = level, slope = slope, beta = beta
      ))
    }
    if (dgp == "break_heavy_tail") {
      level <- beta <- numeric(T)
      x <- stats::rnorm(T)
      level[[1L]] <- stats::rnorm(1)
      beta[[1L]] <- 0.5
      if (T > 1L) for (tt in 2:T) {
        level[[tt]] <- level[[tt - 1L]] +
          stats::rnorm(1, sd = sqrt(0.02))
        beta[[tt]] <- beta[[tt - 1L]] +
          stats::rnorm(1, sd = sqrt(0.01))
      }
      break_time <- max(2L, floor(0.70 * T))
      beta[break_time:T] <- beta[break_time:T] + 1.5
      return(list(
        mu = level + beta * x, scale = rep(1, T), x = x,
        terminal = list(level = level[[T]], beta = beta[[T]], x = x[[T]]),
        level = level, beta = beta, break_time = break_time
      ))
    }
    if (dgp == "heteroscedastic_t5") {
      level <- z <- numeric(T)
      level[[1L]] <- stats::rnorm(1)
      z[[1L]] <- stats::rnorm(1)
      if (T > 1L) for (tt in 2:T) {
        level[[tt]] <- level[[tt - 1L]] +
          stats::rnorm(1, sd = sqrt(q$local_level_variance))
        z[[tt]] <- q$heteroscedastic_log_scale_ar * z[[tt - 1L]] +
          stats::rnorm(
            1,
            sd = sqrt(
              q$heteroscedastic_log_scale_innovation_variance
            )
          )
      }
      scale <- pmax(
        q$scale_floor,
        exp(q$heteroscedastic_log_scale_coefficient * z)
      )
      return(list(
        mu = level, scale = scale, x = z,
        terminal = list(level = level[[T]], z = z[[T]]),
        level = level, z = z
      ))
    }
    if (dgp == "root_alignment") {
      root <- q$root_alignment
      lower <- upper <- numeric(T)
      lower[[1L]] <- root$lower_initial
      upper[[1L]] <- root$upper_initial
      if (T > 1L) for (tt in 2:T) {
        lower[[tt]] <- lower[[tt - 1L]] +
          stats::rnorm(1, sd = sqrt(root$lower_variance))
        upper[[tt]] <- upper[[tt - 1L]] +
          stats::rnorm(1, sd = sqrt(root$upper_variance))
      }
      if (min(upper - lower) <= root$minimum_separation) {
        stop("The root-alignment training path crossed.", call. = FALSE)
      }
      oracle <- rqr_oracle_roots(
        "centered_standardized_lognormal", root$reference_coverage,
        params = list(logmean = 0, logsd = 0.75)
      )
      denominator <- oracle$upper_root - oracle$lower_root
      scale <- (upper - lower) / denominator
      mu <- (
        oracle$upper_root * lower - oracle$lower_root * upper
      ) / denominator
      return(list(
        mu = mu, scale = scale, x = numeric(T),
        terminal = list(lower = lower[[T]], upper = upper[[T]]),
        reference_lower = lower, reference_upper = upper
      ))
    }
    stop("The training DGP is not implemented.", call. = FALSE)
  })
}

rqr_confirm_future_state <- function(config, scenario, training, H, state) {
  dgp <- scenario$dgp
  q <- config$dgp
  rqr_confirm_with_state(state, {
    if (dgp == "static_gaussian") {
      x <- stats::rnorm(H)
      return(list(mu = 0.5 + 0.75 * x, scale = rep(1, H), x = x))
    }
    if (dgp %in% c("local_level_gaussian", "local_level_skewed")) {
      level <- numeric(H)
      previous <- training$terminal$level
      for (hh in seq_len(H)) {
        level[[hh]] <- previous +
          stats::rnorm(1, sd = sqrt(q$local_level_variance))
        previous <- level[[hh]]
      }
      return(list(mu = level, scale = rep(1, H), x = numeric(H)))
    }
    if (dgp %in% c(
        "trend_seasonal_gaussian", "trend_seasonal_skewed")) {
      transition <- rqr_confirm_harmonic_transition(q$seasonal_period)
      level <- slope <- numeric(H)
      seasonal <- matrix(0, 2L, H)
      previous_level <- training$terminal$level
      previous_slope <- training$terminal$slope
      previous_seasonal <- training$terminal$seasonal
      for (hh in seq_len(H)) {
        slope[[hh]] <- previous_slope +
          stats::rnorm(1, sd = sqrt(q$trend_slope_variance))
        level[[hh]] <- previous_level + previous_slope +
          stats::rnorm(1, sd = sqrt(q$trend_level_variance))
        seasonal[, hh] <- drop(transition %*% previous_seasonal) +
          stats::rnorm(2L, sd = sqrt(q$seasonal_variance))
        previous_level <- level[[hh]]
        previous_slope <- slope[[hh]]
        previous_seasonal <- seasonal[, hh]
      }
      time <- scenario$T + seq_len(H)
      scale <- pmax(
        q$scale_floor,
        1 + 0.30 * sin(2 * pi * time / q$seasonal_period)
      )
      return(list(
        mu = level + seasonal[1L, ], scale = scale, x = numeric(H)
      ))
    }
    if (dgp == "trend_regression_unequal") {
      level <- slope <- beta <- x <- numeric(H)
      previous <- training$terminal
      for (hh in seq_len(H)) {
        slope[[hh]] <- previous$slope +
          stats::rnorm(1, sd = sqrt(q$trend_slope_variance))
        level[[hh]] <- previous$level + previous$slope +
          stats::rnorm(1, sd = sqrt(q$trend_level_variance))
        beta[[hh]] <- previous$beta +
          stats::rnorm(1, sd = sqrt(q$regression_variance))
        x[[hh]] <- q$regression_predictor_ar * previous$x +
          stats::rnorm(
            1, sd = sqrt(q$regression_predictor_innovation_variance)
          )
        previous <- list(
          level = level[[hh]], slope = slope[[hh]],
          beta = beta[[hh]], x = x[[hh]]
        )
      }
      return(list(
        mu = level + beta * x, scale = rep(1, H), x = x
      ))
    }
    if (dgp == "break_heavy_tail") {
      level <- beta <- numeric(H)
      x <- stats::rnorm(H)
      previous_level <- training$terminal$level
      previous_beta <- training$terminal$beta
      for (hh in seq_len(H)) {
        level[[hh]] <- previous_level +
          stats::rnorm(1, sd = sqrt(0.02))
        beta[[hh]] <- previous_beta +
          stats::rnorm(1, sd = sqrt(0.01))
        previous_level <- level[[hh]]
        previous_beta <- beta[[hh]]
      }
      return(list(
        mu = level + beta * x, scale = rep(1, H), x = x
      ))
    }
    if (dgp == "heteroscedastic_t5") {
      level <- z <- numeric(H)
      previous_level <- training$terminal$level
      previous_z <- training$terminal$z
      for (hh in seq_len(H)) {
        level[[hh]] <- previous_level +
          stats::rnorm(1, sd = sqrt(q$local_level_variance))
        z[[hh]] <- q$heteroscedastic_log_scale_ar * previous_z +
          stats::rnorm(
            1,
            sd = sqrt(
              q$heteroscedastic_log_scale_innovation_variance
            )
          )
        previous_level <- level[[hh]]
        previous_z <- z[[hh]]
      }
      return(list(
        mu = level,
        scale = pmax(
          q$scale_floor,
          exp(q$heteroscedastic_log_scale_coefficient * z)
        ),
        x = z
      ))
    }
    if (dgp == "root_alignment") {
      root <- q$root_alignment
      lower <- upper <- numeric(H)
      previous_lower <- training$terminal$lower
      previous_upper <- training$terminal$upper
      for (hh in seq_len(H)) {
        lower[[hh]] <- previous_lower +
          stats::rnorm(1, sd = sqrt(root$lower_variance))
        upper[[hh]] <- previous_upper +
          stats::rnorm(1, sd = sqrt(root$upper_variance))
        previous_lower <- lower[[hh]]
        previous_upper <- upper[[hh]]
      }
      if (min(upper - lower) <= root$minimum_separation) {
        stop("The root-alignment future path crossed.", call. = FALSE)
      }
      oracle <- rqr_oracle_roots(
        "centered_standardized_lognormal", root$reference_coverage,
        params = list(logmean = 0, logsd = 0.75)
      )
      denominator <- oracle$upper_root - oracle$lower_root
      scale <- (upper - lower) / denominator
      mu <- (
        oracle$upper_root * lower - oracle$lower_root * upper
      ) / denominator
      return(list(
        mu = mu, scale = scale, x = numeric(H),
        reference_lower = lower, reference_upper = upper
      ))
    }
    stop("The future DGP is not implemented.", call. = FALSE)
  })
}

rqr_confirm_conditional_future <- function(config, scenario, training, H) {
  dgp <- scenario$dgp
  q <- config$dgp
  if (dgp == "static_gaussian") {
    return(list(
      mu = rep(0.5, H), scale = rep(1, H), x = rep(0, H)
    ))
  }
  if (dgp %in% c("local_level_gaussian", "local_level_skewed")) {
    return(list(mu = rep(training$terminal$level, H),
                scale = rep(1, H), x = rep(0, H)))
  }
  if (dgp %in% c(
      "trend_seasonal_gaussian", "trend_seasonal_skewed")) {
    transition <- rqr_confirm_harmonic_transition(q$seasonal_period)
    state <- training$terminal
    mu <- numeric(H)
    seasonal <- state$seasonal
    for (hh in seq_len(H)) {
      state$level <- state$level + state$slope
      seasonal <- drop(transition %*% seasonal)
      mu[[hh]] <- state$level + seasonal[[1L]]
    }
    time <- scenario$T + seq_len(H)
    return(list(
      mu = mu,
      scale = pmax(
        q$scale_floor,
        1 + 0.30 * sin(2 * pi * time / q$seasonal_period)
      ),
      x = rep(0, H)
    ))
  }
  if (dgp == "trend_regression_unequal") {
    state <- training$terminal
    mu <- numeric(H)
    for (hh in seq_len(H)) {
      state$level <- state$level + state$slope
      state$x <- q$regression_predictor_ar * state$x
      mu[[hh]] <- state$level + state$beta * state$x
    }
    return(list(mu = mu, scale = rep(1, H), x = {
      value <- numeric(H)
      current <- training$terminal$x
      for (hh in seq_len(H)) {
        current <- q$regression_predictor_ar * current
        value[[hh]] <- current
      }
      value
    }))
  }
  if (dgp == "break_heavy_tail") {
    return(list(
      mu = rep(training$terminal$level, H),
      scale = rep(1, H), x = rep(0, H)
    ))
  }
  if (dgp == "heteroscedastic_t5") {
    state <- training$terminal
    mu <- rep(state$level, H)
    z <- variance <- expected_scale <- numeric(H)
    for (hh in seq_len(H)) {
      state$z <- q$heteroscedastic_log_scale_ar * state$z
      z[[hh]] <- state$z
      variance[[hh]] <-
        q$heteroscedastic_log_scale_innovation_variance *
        sum(
          q$heteroscedastic_log_scale_ar^(2 * (0:(hh - 1L)))
        )
      sd <- sqrt(variance[[hh]])
      exponent <- q$heteroscedastic_log_scale_coefficient
      threshold <- log(q$scale_floor) / exponent
      below <- stats::pnorm((threshold - z[[hh]]) / sd)
      tilted_above <- 1 - stats::pnorm(
        (threshold - z[[hh]] - exponent * variance[[hh]]) / sd
      )
      expected_scale[[hh]] <- q$scale_floor * below +
        exp(
          exponent * z[[hh]] +
            0.5 * exponent^2 * variance[[hh]]
        ) * tilted_above
    }
    return(list(
      mu = mu, scale = expected_scale, x = z
    ))
  }
  if (dgp == "root_alignment") {
    root <- q$root_alignment
    oracle <- rqr_oracle_roots(
      "centered_standardized_lognormal", root$reference_coverage,
      params = list(logmean = 0, logsd = 0.75)
    )
    lower <- rep(training$terminal$lower, H)
    upper <- rep(training$terminal$upper, H)
    denominator <- oracle$upper_root - oracle$lower_root
    return(list(
      mu = (
        oracle$upper_root * lower - oracle$lower_root * upper
      ) / denominator,
      scale = (upper - lower) / denominator,
      x = rep(0, H)
    ))
  }
  stop("No conditional future is defined.", call. = FALSE)
}

rqr_confirm_generate_dgp <- function(contract, scenario_id, replication,
                                     ledger) {
  rqr_confirm_validate_contract(contract)
  replication <- rqr_confirm_strict_integer(
    replication, "replication", 1L
  )
  scenario <- contract$config$scenarios[[scenario_id]]
  if (is.null(scenario)) stop("Unknown scenario ID.", call. = FALSE)
  H <- rqr_confirm_strict_integer(
    contract$config$design$forecast_horizon, "forecast_horizon", 1L
  )
  S <- rqr_confirm_strict_integer(
    contract$config$design$future_subreplications,
    "future_subreplications", 1L
  )
  data_id <- paste(scenario$dgp, scenario$T, sep = "_T")
  train_state_key <- paste(
    "training_state", scenario$pair, replication, sep = "|"
  )
  train_response_key <- paste(
    "training_response", data_id, replication, sep = "|"
  )
  training <- rqr_confirm_generate_training_state(
    contract$config, scenario,
    rqr_confirm_state_from_ledger(ledger, train_state_key)
  )
  family <- rqr_confirm_error_family(scenario$dgp)
  training_error <- rqr_confirm_with_state(
    rqr_confirm_state_from_ledger(ledger, train_response_key),
    rqr_confirm_error_draw(family, scenario$T)
  )
  training_y <- training$mu + training$scale * training_error
  oracle_spec <- rqr_confirm_oracle_spec(family)
  oracle <- rqr_oracle_roots(
    oracle_spec$family, scenario$coverage, params = oracle_spec$params
  )
  distribution <- rqrgibbs:::.rqr_oracle_family_spec(
    oracle_spec$family, oracle_spec$params
  )
  tail_probability <- (1 - scenario$coverage) / 2
  quantile_roots <- distribution$q(
    c(tail_probability, 1 - tail_probability)
  )
  training_roots <- cbind(
    lower = training$mu + training$scale * oracle$lower_root,
    upper = training$mu + training$scale * oracle$upper_root
  )
  conditional <- rqr_confirm_conditional_future(
    contract$config, scenario, training, H
  )
  conditional_roots <- cbind(
    lower = conditional$mu + conditional$scale * oracle$lower_root,
    upper = conditional$mu + conditional$scale * oracle$upper_root
  )
  conditional_quantile_roots <- cbind(
    lower = conditional$mu + conditional$scale * quantile_roots[[1L]],
    upper = conditional$mu + conditional$scale * quantile_roots[[2L]]
  )
  realized_roots <- array(
    NA_real_, c(S, H, 2L),
    dimnames = list(
      subreplication = seq_len(S), horizon = seq_len(H),
      root = c("lower", "upper")
    )
  )
  realized_quantile_roots <- realized_roots
  future_y <- matrix(
    NA_real_, S, H,
    dimnames = list(subreplication = seq_len(S), horizon = seq_len(H))
  )
  for (subreplication in seq_len(S)) {
    future_state_key <- paste(
      "future_state", scenario$pair, replication,
      "subrep", subreplication, sep = "|"
    )
    future_response_key <- paste(
      "future_response", data_id, replication,
      "subrep", subreplication, sep = "|"
    )
    future <- rqr_confirm_future_state(
      contract$config, scenario, training, H,
      rqr_confirm_state_from_ledger(ledger, future_state_key)
    )
    future_error <- rqr_confirm_with_state(
      rqr_confirm_state_from_ledger(ledger, future_response_key),
      rqr_confirm_error_draw(family, H)
    )
    future_y[subreplication, ] <-
      future$mu + future$scale * future_error
    realized_roots[subreplication, , 1L] <-
      future$mu + future$scale * oracle$lower_root
    realized_roots[subreplication, , 2L] <-
      future$mu + future$scale * oracle$upper_root
    realized_quantile_roots[subreplication, , 1L] <-
      future$mu + future$scale * quantile_roots[[1L]]
    realized_quantile_roots[subreplication, , 2L] <-
      future$mu + future$scale * quantile_roots[[2L]]
  }
  if (any(!is.finite(c(
      training_y, training_roots, conditional_roots,
      realized_roots, realized_quantile_roots, future_y))) ||
      any(training_roots[, "upper"] <= training_roots[, "lower"]) ||
      any(realized_roots[, , "upper"] <=
          realized_roots[, , "lower"])) {
    stop("The generated DGP failed its finite or ordering gates.",
         call. = FALSE)
  }
  dynamic <- scenario$dgp != "static_gaussian"
  if (dynamic && isTRUE(all.equal(
      sweep(realized_roots, c(2L, 3L), conditional_roots, "-"),
      array(0, dim(realized_roots)), tolerance = 0
    ))) {
    stop("Conditional-mean and realized future roots were conflated.",
         call. = FALSE)
  }
  list(
    schema_version = "rqrgibbs_dlm_main_simulation_dgp/2.0.0",
    scenario_id = scenario_id,
    dgp = scenario$dgp,
    replication = replication,
    coverage_level = scenario$coverage,
    T = scenario$T,
    H = H,
    future_subreplications = S,
    training_y = training_y,
    training_mu = training$mu,
    training_scale = training$scale,
    training_roots = training_roots,
    oracle_conditional_mean_root = conditional_roots,
    quantile_conditional_mean_root = conditional_quantile_roots,
    realized_root_path = realized_roots,
    realized_quantile_path = realized_quantile_roots,
    generated_future_response = future_y,
    training_predictor = training$x,
    future_predictor = conditional$x,
    latent = training,
    task_keys = list(
      training_state = train_state_key,
      training_response = train_response_key
    ),
    response_law_shared_across_coverages = TRUE,
    generalized_bayes_target = TRUE,
    response_prediction_contract = FALSE
  )
}

rqr_confirm_artifact_schemas <- function() {
  list(
    replication_results = c(
      "run_id", "cell_id", "replication", "method", "status",
      "failure_class", "training_loss", "heldout_rqr_loss",
      "aggregate_coverage", "mean_width", "central_interval_score",
      "future_mean_lower", "future_mean_upper",
      "future_mean_midpoint",
      "endpoint_rmse_lower", "endpoint_rmse_upper",
      "cross_target_distance", "realized_root_rmse",
      sprintf("coverage_h%02d", seq_len(20L)),
      "training_response_sd", "mean_oracle_width",
      "elapsed_seconds", "peak_RSS_bytes"
    ),
    replication_manifest = c(
      "schema_version", "source_commit", "config_digest",
      "incidence_digest", "seed_ledger_digest", "runtime_digest",
      "DGP", "replication", "embedded_sentinel", "no_retry",
      "generalized_bayes", "response_likelihood",
      "response_prediction_contract"
    ),
    failure_ledger = c(
      "run_id", "cell_id", "replication", "method", "failure_class",
      "message_digest", "intention_to_run_denominator", "retry_count"
    ),
    fit_diagnostics = c(
      "estimand", "chains", "rhat", "ess_bulk", "ess_tail",
      "mcse_mean", "mcse_over_sd", "pass", "DGP", "replication",
      "method", "sentinel"
    ),
    batch_decision = c(
      "batch_group", "DGP", "replication_rule", "replications",
      "precision_pass", "cell_mean_precision_pass",
      "paired_contrast_precision_pass", "no_fit_failures",
      "paired_batch_complete", "performance_sign_used", "TOST_used",
      "next_action", "next_replications"
    )
  )
}

rqr_confirm_recursive_manifest <- function(directory) {
  directory <- normalizePath(directory, winslash = "/", mustWork = TRUE)
  paths <- list.files(
    directory, recursive = TRUE, full.names = TRUE,
    all.files = TRUE, no.. = TRUE
  )
  paths <- paths[file.info(paths)$isdir %in% FALSE]
  relative <- substring(paths, nchar(directory) + 2L)
  order_index <- order(relative, method = "radix")
  paths <- paths[order_index]
  relative <- relative[order_index]
  data.frame(
    path = relative,
    bytes = as.numeric(file.info(paths)$size),
    sha256 = unname(vapply(
      paths, rqr_confirm_sha256, character(1L)
    )),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}

rqr_confirm_directory_digest <- function(directory) {
  directory <- normalizePath(
    directory, winslash = "/", mustWork = TRUE
  )
  files <- list.files(
    directory, recursive = TRUE, all.files = TRUE, full.names = TRUE,
    include.dirs = FALSE, no.. = TRUE
  )
  files <- sort(files)
  relative <- substring(files, nchar(directory) + 2L)
  info <- file.info(files)
  links <- Sys.readlink(files)
  if (anyNA(info$mode) || anyDuplicated(relative)) {
    stop("A runtime directory is not a unique readable file set.",
         call. = FALSE)
  }
  kind <- ifelse(nzchar(links), "symlink", "file")
  mode <- sprintf("%04o", bitwAnd(as.integer(info$mode), 511L))
  content <- vapply(seq_along(files), function(index) {
    if (nzchar(links[[index]])) {
      digest::digest(
        links[[index]], algo = "sha256", serialize = FALSE
      )
    } else {
      rqr_confirm_sha256(files[[index]])
    }
  }, character(1L))
  payload <- paste(
    kind, mode, content, relative, sep = "\t", collapse = "\n"
  )
  digest::digest(
    payload, algo = "sha256", serialize = FALSE
  )
}

rqr_confirm_verify_recursive_manifest <- function(
    directory, manifest_name = "artifact_hashes.csv") {
  directory <- normalizePath(directory, winslash = "/", mustWork = TRUE)
  manifest_path <- file.path(directory, manifest_name)
  if (!file.exists(manifest_path)) {
    stop(sprintf("Missing recursive manifest: %s.", manifest_name),
         call. = FALSE)
  }
  listed <- utils::read.csv(
    manifest_path, stringsAsFactors = FALSE, check.names = FALSE
  )
  listed$path <- as.character(listed$path)
  listed$bytes <- as.numeric(listed$bytes)
  listed$sha256 <- as.character(listed$sha256)
  rownames(listed) <- NULL
  if (!identical(names(listed), c("path", "bytes", "sha256")) ||
      anyNA(listed[c("sha256", "bytes", "path")]) ||
      anyDuplicated(listed$path) ||
      any(!grepl("^[0-9a-f]{64}$", listed$sha256)) ||
      any(grepl("(^|/)\\.\\.?(/|$)", listed$path)) ||
      any(grepl("^/", listed$path))) {
    stop("A recursive artifact manifest has an invalid schema.",
         call. = FALSE)
  }
  actual <- list.files(
    directory, recursive = TRUE, all.files = TRUE, no.. = TRUE,
    include.dirs = FALSE, full.names = FALSE
  )
  actual <- sort(
    setdiff(gsub("\\\\", "/", actual), manifest_name),
    method = "radix"
  )
  if (!identical(sort(listed$path, method = "radix"), actual)) {
    stop("A recursive artifact manifest has the wrong exact file set.",
         call. = FALSE)
  }
  for (index in seq_len(nrow(listed))) {
    path <- file.path(directory, listed$path[[index]])
    if (!file.exists(path) || nzchar(Sys.readlink(path)) ||
        !identical(rqr_confirm_sha256(path), listed$sha256[[index]]) ||
        !identical(
          as.numeric(file.info(path)$size),
          as.numeric(listed$bytes[[index]])
        )) {
      stop("A recursive artifact failed byte verification.",
           call. = FALSE)
    }
  }
  listed
}

rqr_confirm_validate_task_subset <- function(tasks, contract) {
  canonical <- rqr_confirm_replication_plan(contract, planning = "maximum")
  required <- names(canonical)
  if (!is.data.frame(tasks) || !nrow(tasks) ||
      !identical(names(tasks), required) ||
      anyNA(tasks[c("replication_task_id", "DGP", "replication")]) ||
      anyDuplicated(tasks$replication_task_id)) {
    stop("An execution task plan has an invalid schema or task IDs.",
         call. = FALSE)
  }
  matched <- match(tasks$replication_task_id, canonical$replication_task_id)
  if (anyNA(matched)) {
    stop("An execution task plan contains a noncanonical task.",
         call. = FALSE)
  }
  expected <- canonical[matched, required, drop = FALSE]
  observed <- tasks[required]
  rownames(expected) <- rownames(observed) <- NULL
  if (!identical(observed, expected)) {
    stop("An execution task plan is not an exact canonical subset.",
         call. = FALSE)
  }
  tasks
}

rqr_confirm_collect_outputs <- function(run_root, planned_tasks, contract) {
  run_root <- normalizePath(run_root, winslash = "/", mustWork = TRUE)
  planned_tasks <- rqr_confirm_validate_task_subset(planned_tasks, contract)
  status_paths <- list.files(
    run_root, pattern = "^run_status\\.csv$", recursive = TRUE,
    full.names = TRUE
  )
  if (!length(status_paths)) {
    stop("The run root contains no worker run-status artifacts.",
         call. = FALSE)
  }
  wave_hash_paths <- list.files(
    run_root, pattern = "^wave_artifact_hashes\\.csv$",
    recursive = TRUE, full.names = TRUE
  )
  if (!length(wave_hash_paths)) {
    stop("The run root contains no monitored wave artifact bundle.",
         call. = FALSE)
  }
  wave_directories <- vapply(
    wave_hash_paths,
    function(path) normalizePath(dirname(path), winslash = "/"),
    character(1L)
  )
  for (wave_directory in wave_directories) {
    rqr_confirm_verify_recursive_manifest(
      wave_directory, "wave_artifact_hashes.csv"
    )
    wave_manifest_path <- file.path(wave_directory, "wave_manifest.json")
    if (!file.exists(wave_manifest_path)) {
      stop("A monitored wave omitted its wave manifest.", call. = FALSE)
    }
    wave_manifest <- jsonlite::read_json(
      wave_manifest_path, simplifyVector = TRUE
    )
    required_wave_fields <- c(
      "schema_version", "canonical_wave_index", "wave_id", "mode",
      "phase", "batch_group", "batch_target", "binding_digest",
      "start_sha256", "same_batch_sentinel_pass",
      "prior_batch_decision_sha256", "worker_limit",
      "workers_used", "task_count", "all_workers_passed",
      "no_retry", "no_reseed", "source_commit",
      "runtime_tree_digest"
    )
    if (!all(required_wave_fields %in% names(wave_manifest)) ||
      !identical(
        wave_manifest$schema_version, "rqrgibbs_dlm_wave/2.0.0"
      ) ||
      !isTRUE(wave_manifest$all_workers_passed) ||
      !isTRUE(wave_manifest$no_retry) ||
      !isTRUE(wave_manifest$no_reseed) ||
      !grepl(
        "^[0-9a-f]{40}$", as.character(wave_manifest$source_commit)
      ) ||
      !grepl(
        "^[0-9a-f]{64}$",
        as.character(wave_manifest$runtime_tree_digest)
      ) ||
      !grepl(
        "^[0-9a-f]{64}$",
        as.character(wave_manifest$binding_digest)
      ) ||
      !grepl(
        "^[0-9a-f]{64}$",
        as.character(wave_manifest$start_sha256)
      )) {
      stop("A monitored wave did not pass its frozen contract.",
           call. = FALSE)
    }
  }
  status_rows <- vector("list", length(status_paths))
  stage_rows <- vector("list", length(status_paths))
  task_fields <- names(planned_tasks)
  status_fields <- c(
    task_fields, "status", "started_at", "ended_at", "message"
  )
  for (index in seq_along(status_paths)) {
    stage_dir <- dirname(status_paths[[index]])
    rqr_confirm_verify_recursive_manifest(stage_dir)
    status <- utils::read.csv(
      status_paths[[index]], stringsAsFactors = FALSE, check.names = FALSE
    )
    if (!identical(names(status), status_fields) ||
        anyDuplicated(status$replication_task_id)) {
      stop("A worker run-status artifact violates its frozen schema.",
           call. = FALSE)
    }
    manifest_path <- file.path(stage_dir, "run_manifest.json")
    if (!file.exists(manifest_path)) {
      stop("A worker stage omitted its run manifest.", call. = FALSE)
    }
    manifest <- jsonlite::read_json(
      manifest_path, simplifyVector = TRUE
    )
    if (!manifest$mode %in% c("sentinel-core", "execute-confirmatory") ||
        !manifest$status %in% c(
          "passed", "completed_with_fit_failures",
          "failed_cell_stop", "failed_global_stop"
        ) ||
        !isTRUE(manifest$generalized_bayes) ||
        isTRUE(manifest$response_likelihood) ||
        isTRUE(manifest$response_prediction_contract) ||
        !grepl("^[0-9a-f]{40}$", manifest$source_commit) ||
        !grepl(
          "^[0-9a-f]{64}$",
          manifest$primary_runtime_binding$runtime_tree_digest %||% ""
        )) {
      stop("A worker run manifest is not a valid confirmatory stage.",
           call. = FALSE)
    }
    status_rows[[index]] <- status
    stage_rows[[index]] <- data.frame(
      stage_directory = normalizePath(stage_dir, winslash = "/"),
      mode = manifest$mode, status = manifest$status,
      source_commit = manifest$source_commit,
      runtime_tree_digest =
        manifest$primary_runtime_binding$runtime_tree_digest %||% "",
      stringsAsFactors = FALSE
    )
  }
  statuses <- do.call(rbind, status_rows)
  rownames(statuses) <- NULL
  if (anyDuplicated(statuses$replication_task_id) ||
      !setequal(
        statuses$replication_task_id,
        planned_tasks$replication_task_id
      )) {
    stop("Worker statuses do not equal the authorization-bound task set.",
         call. = FALSE)
  }
  matched <- match(
    statuses$replication_task_id, planned_tasks$replication_task_id
  )
  observed_tasks <- statuses[task_fields]
  expected_tasks <- planned_tasks[matched, task_fields, drop = FALSE]
  rownames(observed_tasks) <- rownames(expected_tasks) <- NULL
  if (!identical(observed_tasks, expected_tasks)) {
    stop("Worker statuses changed an authorization-bound task.",
         call. = FALSE)
  }
  allowed_status <- c(
    "completed", "completed_with_fit_failure", "cell_stop_failure",
    "global_stop_failure", "infrastructure_invalid",
    "not_run_cell_stop", "not_run_global_stop"
  )
  if (any(!statuses$status %in% allowed_status)) {
    stop("Collection found an unfinished or unknown worker status.",
         call. = FALSE)
  }

  result_paths <- list.files(
    run_root, pattern = "^replication_results\\.csv$",
    recursive = TRUE, full.names = TRUE
  )
  result_paths <- result_paths[
    grepl("/replications/", result_paths, fixed = TRUE)
  ]
  published_status <- statuses$status %in% c(
    "completed", "completed_with_fit_failure",
    "cell_stop_failure", "global_stop_failure"
  )
  if (length(result_paths) != sum(published_status)) {
    stop("Published replication directories do not match worker statuses.",
         call. = FALSE)
  }
  result_rows <- vector("list", length(result_paths))
  diagnostic_rows <- list()
  failure_rows <- list()
  replication_rows <- vector("list", length(result_paths))
  status_stage_directories <- vapply(
    status_paths,
    function(path) normalizePath(dirname(path), winslash = "/"),
    character(1L)
  )
  stage_wave_owner <- vapply(
    status_stage_directories,
    function(stage_directory) {
      owners <- wave_directories[vapply(
        wave_directories,
        function(wave_directory) {
          startsWith(
            paste0(stage_directory, "/"),
            paste0(wave_directory, "/")
          )
        },
        logical(1L)
      )]
      if (length(owners) != 1L) {
        stop("A worker stage has no unique monitored-wave owner.",
             call. = FALSE)
      }
      owners[[1L]]
    },
    character(1L)
  )
  for (index in seq_along(result_paths)) {
    replication_dir <- dirname(result_paths[[index]])
    stage_dir <- sub(
      "/replications/.*$", "",
      normalizePath(replication_dir, winslash = "/")
    )
    stage_index <- match(stage_dir, status_stage_directories)
    if (is.na(stage_index)) {
      stop("A replication directory has no owning worker stage.",
           call. = FALSE)
    }
    rqr_confirm_verify_recursive_manifest(
      replication_dir, "replication_artifact_hashes.csv"
    )
    replication_manifest_path <- file.path(
      replication_dir, "replication_manifest.json"
    )
    if (!file.exists(replication_manifest_path)) {
      stop("A replication directory omitted its manifest.",
           call. = FALSE)
    }
    replication_manifest <- jsonlite::read_json(
      replication_manifest_path, simplifyVector = TRUE
    )
    if (!identical(
        names(replication_manifest),
        rqr_confirm_artifact_schemas()$replication_manifest
      ) ||
      !identical(
        replication_manifest$schema_version,
        "rqrgibbs_dlm_replication/1.0.0"
      ) ||
      !isTRUE(replication_manifest$generalized_bayes) ||
      isTRUE(replication_manifest$response_likelihood) ||
      isTRUE(replication_manifest$response_prediction_contract) ||
      !isTRUE(replication_manifest$no_retry)) {
      stop("A replication manifest violates its frozen contract.",
           call. = FALSE)
    }
    stage_manifest <- jsonlite::read_json(
      file.path(stage_dir, "run_manifest.json"), simplifyVector = TRUE
    )
    if (!identical(
        replication_manifest$source_commit,
        stage_manifest$source_commit
      ) ||
      !identical(
        replication_manifest$runtime_digest,
        stage_manifest$primary_runtime_binding$runtime_tree_digest
      )) {
      stop("A replication is not bound to its worker source/runtime.",
           call. = FALSE)
    }
    task_id <- sprintf(
      "%s__rep%04d",
      replication_manifest$DGP,
      rqr_confirm_strict_integer(
        replication_manifest$replication, "replication", 1L
      )
    )
    task_row <- match(task_id, planned_tasks$replication_task_id)
    status_row <- match(task_id, statuses$replication_task_id)
    if (is.na(task_row) || is.na(status_row) || !published_status[[status_row]]) {
      stop("A published replication is absent from its task/status plan.",
           call. = FALSE)
    }
    result <- utils::read.csv(
      result_paths[[index]], stringsAsFactors = FALSE, check.names = FALSE
    )
    if (!identical(
        names(result),
        rqr_confirm_artifact_schemas()$replication_results
      ) ||
      any(result$cell_id == "") ||
      anyDuplicated(result[c("cell_id", "replication")]) ||
      any(result$replication != planned_tasks$replication[[task_row]])) {
      stop("A compact replication result violates its frozen schema.",
           call. = FALSE)
    }
    expected_methods <- strsplit(
      planned_tasks$methods[[task_row]], "|", fixed = TRUE
    )[[1L]]
    exact_method_set <- setequal(result$method, expected_methods)
    complete_status <- statuses$status[[status_row]] %in%
      c("completed", "completed_with_fit_failure")
    if ((complete_status && !exact_method_set) ||
        any(!result$method %in% expected_methods)) {
      stop("A completed replication has a missing or extra method.",
           call. = FALSE)
    }
    result_rows[[index]] <- result
    diagnostic_path <- file.path(replication_dir, "fit_diagnostics.csv")
    if (file.exists(diagnostic_path)) {
      diagnostic <- utils::read.csv(
        diagnostic_path, stringsAsFactors = FALSE, check.names = FALSE
      )
      if (!identical(
          names(diagnostic),
          rqr_confirm_artifact_schemas()$fit_diagnostics
        )) {
        stop("A fit-diagnostic artifact violates its frozen schema.",
             call. = FALSE)
      }
      diagnostic_rows[[length(diagnostic_rows) + 1L]] <- diagnostic
    }
    failure_path <- file.path(replication_dir, "failure_log.csv")
    if (file.exists(failure_path)) {
      failure <- utils::read.csv(
        failure_path, stringsAsFactors = FALSE, check.names = FALSE
      )
      if (!identical(
          names(failure),
          rqr_confirm_artifact_schemas()$failure_ledger
        ) ||
        any(!failure$intention_to_run_denominator) ||
        any(failure$retry_count != 0L)) {
        stop("A replication failure ledger violates its frozen schema.",
             call. = FALSE)
      }
      failure_rows[[length(failure_rows) + 1L]] <- failure
    }
    replication_rows[[index]] <- data.frame(
      replication_task_id = task_id,
      exact_method_set = exact_method_set,
      result_rows = nrow(result),
      artifact_manifest_verified = TRUE,
      source_commit = replication_manifest$source_commit,
      config_digest = replication_manifest$config_digest,
      incidence_digest = replication_manifest$incidence_digest,
      seed_ledger_digest = replication_manifest$seed_ledger_digest,
      runtime_digest = replication_manifest$runtime_digest,
      stringsAsFactors = FALSE
    )
  }
  results <- if (length(result_rows)) {
    do.call(rbind, result_rows)
  } else {
    value <- as.data.frame(
      matrix(
        nrow = 0L,
        ncol = length(rqr_confirm_artifact_schemas()$replication_results)
      )
    )
    names(value) <- rqr_confirm_artifact_schemas()$replication_results
    value
  }
  rownames(results) <- NULL
  if (anyDuplicated(results[c("cell_id", "replication")])) {
    stop("Collected compact results contain duplicate fit IDs.",
         call. = FALSE)
  }
  failures <- if (length(failure_rows)) {
    do.call(rbind, failure_rows)
  } else {
    data.frame(
      run_id = character(), cell_id = character(),
      replication = integer(), method = character(),
      failure_class = character(), message_digest = character(),
      intention_to_run_denominator = logical(), retry_count = integer(),
      stringsAsFactors = FALSE
    )
  }
  noncompleted <- results[results$status != "completed", , drop = FALSE]
  failure_ids <- paste(failures$cell_id, failures$replication, sep = "|")
  noncompleted_ids <- paste(
    noncompleted$cell_id, noncompleted$replication, sep = "|"
  )
  if (anyDuplicated(failure_ids) ||
      !setequal(failure_ids, noncompleted_ids)) {
    stop("Failure records do not equal noncompleted compact fit results.",
         call. = FALSE)
  }
  stages <- do.call(rbind, stage_rows)
  if (length(unique(stages$source_commit)) != 1L ||
      length(unique(stages$runtime_tree_digest)) != 1L ||
      !nzchar(stages$runtime_tree_digest[[1L]])) {
    stop("Worker stages do not share one source and primary runtime.",
         call. = FALSE)
  }
  replications <- if (length(replication_rows)) {
    do.call(rbind, replication_rows)
  } else {
    data.frame(
      replication_task_id = character(),
      exact_method_set = logical(), result_rows = integer(),
      artifact_manifest_verified = logical(),
      source_commit = character(), config_digest = character(),
      incidence_digest = character(), seed_ledger_digest = character(),
      runtime_digest = character(), stringsAsFactors = FALSE
    )
  }
  digest_fields <- c(
    "source_commit", "config_digest", "incidence_digest",
    "seed_ledger_digest", "runtime_digest"
  )
  if (nrow(replications) &&
      (any(vapply(
        replications[digest_fields],
        function(value) length(unique(value)) != 1L,
        logical(1L)
      )) ||
       !identical(
         unique(replications$source_commit), unique(stages$source_commit)
       ) ||
       !identical(
         unique(replications$runtime_digest),
         unique(stages$runtime_tree_digest)
       ))) {
    stop("Replication artifacts do not share the worker source/runtime bundle.",
         call. = FALSE)
  }
  list(
    results = results,
    diagnostics = if (length(diagnostic_rows)) {
      do.call(rbind, diagnostic_rows)
    } else {
      data.frame()
    },
    failures = failures,
    statuses = statuses,
    stages = stages,
    wave_directories = unique(stage_wave_owner),
    replications = replications,
    analysis_complete = all(statuses$status %in% c(
      "completed", "completed_with_fit_failure"
    )) && all(vapply(
      replication_rows, `[[`, logical(1L), "exact_method_set"
    ))
  )
}

rqr_confirm_atomic_write_csv <- function(value, path,
                                         inject_failure = FALSE) {
  directory <- dirname(path)
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(
    paste0(".", basename(path), "-"), tmpdir = directory
  )
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  utils::write.csv(value, temporary, row.names = FALSE, quote = TRUE)
  read_back <- utils::read.csv(
    temporary, stringsAsFactors = FALSE, check.names = FALSE
  )
  if (!identical(names(read_back), names(value)) ||
      nrow(read_back) != nrow(value)) {
    stop("Atomic CSV read-back validation failed.", call. = FALSE)
  }
  rqr_confirm_sha256(temporary)
  if (isTRUE(inject_failure)) {
    stop("Injected atomic CSV publication failure.", call. = FALSE)
  }
  if (file.exists(path) ||
      !file.rename(temporary, path)) {
    stop("Atomic CSV publication failed or refused an overwrite.",
         call. = FALSE)
  }
  invisible(path)
}

rqr_confirm_atomic_write_json <- function(value, path,
                                          inject_failure = FALSE) {
  directory <- dirname(path)
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(
    paste0(".", basename(path), "-"), tmpdir = directory
  )
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  jsonlite::write_json(
    value, temporary, auto_unbox = TRUE, pretty = TRUE, null = "null"
  )
  jsonlite::read_json(temporary, simplifyVector = FALSE)
  rqr_confirm_sha256(temporary)
  if (isTRUE(inject_failure)) {
    stop("Injected atomic JSON publication failure.", call. = FALSE)
  }
  if (file.exists(path) ||
      !file.rename(temporary, path)) {
    stop("Atomic JSON publication failed or refused an overwrite.",
         call. = FALSE)
  }
  invisible(path)
}

rqr_confirm_read_attestation <- function(path, package, version,
                                         source_sha256) {
  attestation <- jsonlite::read_json(path, simplifyVector = TRUE)
  required <- c(
    "schema_version", "package", "version", "source_package_path",
    "source_package_sha256", "install_input_count",
    "install_exit_status", "runtime_path", "runtime_tree_digest",
    "protected_exdqlm_checkout_used"
  )
  if (!all(required %in% names(attestation)) ||
      !identical(
        attestation$schema_version,
        "rqrgibbs_external_cran_runtime/1.0.0"
      ) ||
      !identical(attestation$package, package) ||
      !identical(attestation$version, version) ||
      !identical(attestation$source_package_sha256, source_sha256) ||
      !identical(
        rqr_confirm_strict_integer(
          attestation$install_input_count, "install_input_count", 1L, 1L
        ),
        1L
      ) ||
      !identical(
        rqr_confirm_strict_integer(
          attestation$install_exit_status, "install_exit_status", 0L, 0L
        ),
        0L
      ) ||
      isTRUE(attestation$protected_exdqlm_checkout_used) ||
      !identical(
        rqr_confirm_sha256(attestation$source_package_path),
        source_sha256
      ) ||
      !identical(
        rqr_confirm_directory_digest(attestation$runtime_path),
        attestation$runtime_tree_digest
      )) {
    stop(sprintf("The %s isolated-runtime attestation failed.", package),
         call. = FALSE)
  }
  attestation
}

rqr_confirm_load_attested_namespace <- function(package, attestation) {
  expected_path <- normalizePath(
    attestation$runtime_path, winslash = "/", mustWork = TRUE
  )
  if (package %in% loadedNamespaces()) {
    loaded_path <- normalizePath(
      getNamespaceInfo(asNamespace(package), "path"),
      winslash = "/", mustWork = TRUE
    )
    if (!identical(loaded_path, expected_path)) {
      stop(
        sprintf("A nonattested %s namespace is already loaded.", package),
        call. = FALSE
      )
    }
  } else {
    loadNamespace(package, lib.loc = dirname(expected_path))
    loaded_path <- normalizePath(
      getNamespaceInfo(asNamespace(package), "path"),
      winslash = "/", mustWork = TRUE
    )
    if (!identical(loaded_path, expected_path)) {
      stop(sprintf("The wrong %s runtime was loaded.", package),
           call. = FALSE)
    }
  }
  invisible(asNamespace(package))
}

rqr_confirm_dependency_manifest <- function(package, runtime_library) {
  runtime_library <- normalizePath(
    runtime_library, winslash = "/", mustWork = TRUE
  )
  description <- utils::packageDescription(package, lib.loc = runtime_library)
  fields <- paste(
    description[c("Depends", "Imports", "LinkingTo")],
    collapse = ","
  )
  dependencies <- unique(gsub(
    "\\s*\\(.*\\)", "",
    trimws(unlist(strsplit(fields, ",", fixed = TRUE)))
  ))
  dependencies <- dependencies[
    !is.na(dependencies) & nzchar(dependencies) &
      !dependencies %in% c("R", "NULL", "NA")
  ]
  packages <- unique(c(package, dependencies))
  rows <- lapply(packages, function(dependency) {
    path <- system.file(package = dependency)
    if (!nzchar(path)) {
      path <- system.file(package = dependency, lib.loc = runtime_library)
    }
    if (!nzchar(path) || !dir.exists(path)) {
      stop(sprintf("Dependency %s is not installed.", dependency),
           call. = FALSE)
    }
    data.frame(
      package = dependency,
      version = as.character(utils::packageVersion(dependency)),
      runtime_path = normalizePath(path, winslash = "/", mustWork = TRUE),
      runtime_tree_digest = rqr_confirm_directory_digest(path),
      source_package_sha256 = if (dependency == package) {
        NA_character_
      } else {
        "not_available_for_preinstalled_dependency"
      },
      stringsAsFactors = FALSE
    )
  })
  manifest <- do.call(rbind, rows)
  manifest[order(manifest$package, method = "radix"), , drop = FALSE]
}

rqr_confirm_toolchain_manifest <- function() {
  r_config <- function(key) {
    output <- suppressWarnings(system2(
      file.path(R.home("bin"), "R"),
      c("CMD", "config", key),
      stdout = TRUE, stderr = TRUE
    ))
    status <- attr(output, "status")
    if (is.null(status)) status <- 0L
    if (!identical(as.integer(status), 0L)) {
      return(sprintf("unavailable_status_%d", as.integer(status)))
    }
    paste(output, collapse = " ")
  }
  package_names <- c(
    "rqrgibbs", "posterior", "digest", "jsonlite", "Rcpp",
    "RcppArmadillo"
  )
  package_versions <- vapply(package_names, function(package) {
    if (!requireNamespace(package, quietly = TRUE)) {
      "not_installed"
    } else {
      as.character(utils::packageVersion(package))
    }
  }, character(1L))
  session <- utils::sessionInfo()
  blas <- as.character(
    session$BLAS %||% tryCatch(La_library(), error = function(error) NA_character_)
  )
  lapack <- as.character(
    session$LAPACK %||% tryCatch(La_library(), error = function(error) NA_character_)
  )
  if (!length(blas) || is.na(blas) || !nzchar(blas)) {
    blas <- "unavailable"
  }
  if (!length(lapack) || is.na(lapack) || !nzchar(lapack)) {
    lapack <- "unavailable"
  }
  values <- c(
    R_version = R.version.string,
    platform = R.version$platform,
    architecture = R.version$arch,
    operating_system = R.version$os,
    CC = r_config("CC"),
    CXX17 = r_config("CXX17"),
    BLAS = blas,
    LAPACK = lapack,
    setNames(package_versions, paste0("package_", package_names))
  )
  data.frame(
    key = names(values), value = unname(values),
    stringsAsFactors = FALSE
  )
}

rqr_confirm_exdqlm_reference <- function(contract, attestation_path,
                                         full_schedule = TRUE) {
  specification <- contract$config$comparator$exdqlm
  attestation <- rqr_confirm_read_attestation(
    attestation_path, "exdqlm", specification$version,
    specification$source_sha256
  )
  library_root <- dirname(attestation$runtime_path)
  rqr_confirm_load_attested_namespace("exdqlm", attestation)
  namespace <- asNamespace("exdqlm")
  mcmc <- get("exdqlmMCMC", envir = namespace)
  forecast <- get("exdqlmForecast", envir = namespace)
  polytrend <- get("polytrendMod", envir = namespace)
  formals_digest <- digest::digest(
    formals(mcmc), algo = "sha256", serialize = TRUE
  )
  resolved_prior <- list(a_sig = 2.1, b_sig = 1.1)
  schedule <- if (isTRUE(full_schedule)) {
    contract$config$schedules$dynamic_quantile_endpoint
  } else {
    list(burn = 20L, retain = 40L, thin = 1L)
  }
  time <- seq_len(32L)
  y <- as.numeric(scale(
    0.03 * time + sin(time / 3) +
      c(rep(-0.15, 16L), rep(0.15, 16L))
  ))
  model <- polytrend(
    1L, m0 = 0, C0 = matrix(4, 1L, 1L), backend = "R"
  )
  probabilities <- c(lower = 0.10, upper = 0.90)
  fits <- vector("list", 2L)
  forecasts <- vector("list", 2L)
  for (index in seq_along(probabilities)) {
    set.seed(2026072401L + index, kind = "L'Ecuyer-CMRG")
    fits[[index]] <- mcmc(
      y = y, p0 = unname(probabilities[[index]]), model = model,
      df = 0.95, dim.df = 1L, dqlm.ind = TRUE,
      init.from.vb = FALSE, fix.sigma = FALSE, sig.init = 1,
      PriorSigma = NULL, n.burn = schedule$burn,
      n.mcmc = schedule$retain, verbose = FALSE,
      trace.diagnostics = FALSE
    )
    forecasts[[index]] <- forecast(
      start.t = length(y), k = 4L, m1 = fits[[index]],
      fFF = matrix(1, 1L, 4L), fGG = matrix(1, 1L, 1L),
      plot = FALSE, return.draws = FALSE,
      seed = 2026072410L + index
    )
  }
  raw <- cbind(
    lower = as.numeric(forecasts[[1L]]$ff),
    upper = as.numeric(forecasts[[2L]]$ff)
  )
  ordered <- cbind(
    lower = pmin(raw[, 1L], raw[, 2L]),
    upper = pmax(raw[, 1L], raw[, 2L])
  )
  pass <- all(vapply(fits, function(fit) {
    isTRUE(fit$dqlm.ind) &&
      identical(fit$init.from.vb, FALSE) &&
      length(fit$samp.sigma) == schedule$retain &&
      all(is.finite(fit$samp.sigma)) &&
      all(fit$samp.sigma > 0)
  }, logical(1L))) &&
    all(is.finite(raw)) && all(is.finite(ordered)) &&
    all(ordered[, "upper"] >= ordered[, "lower"])
  if (!pass) stop("The actual exdqlm fit/forecast reference failed.",
                  call. = FALSE)
  list(
    summary = data.frame(
      package = "exdqlm", version = specification$version,
      fitting_function = "exdqlmMCMC",
      lower_probability = probabilities[["lower"]],
      upper_probability = probabilities[["upper"]],
      burn = schedule$burn, retained = schedule$retain,
      dqlm_ind = TRUE, init_from_vb = FALSE,
      fix_sigma = FALSE, sig_init = 1,
      PriorSigma_argument = "NULL",
      PriorSigma_resolved = sprintf(
        "a_sig=%.1f;b_sig=%.1f",
        resolved_prior$a_sig, resolved_prior$b_sig
      ),
      discount = 0.95, component_dimension = 1L,
      raw_endpoint_forecasts_retained = TRUE,
      ordering_only_for_interval_metrics = TRUE,
      response_predictive_draws_used = FALSE,
      formals_digest = formals_digest,
      runtime_tree_digest = attestation$runtime_tree_digest,
      pass = TRUE,
      stringsAsFactors = FALSE
    ),
    raw_forecasts = data.frame(
      horizon = seq_len(nrow(raw)), raw_lower = raw[, 1L],
      raw_upper = raw[, 2L], ordered_lower = ordered[, 1L],
      ordered_upper = ordered[, 2L]
    ),
    dependency_manifest =
      rqr_confirm_dependency_manifest("exdqlm", library_root),
    attestation = attestation
  )
}

rqr_confirm_quantreg_reference <- function(contract, attestation_path) {
  specification <- contract$config$comparator$quantreg
  attestation <- rqr_confirm_read_attestation(
    attestation_path, "quantreg", specification$version,
    specification$source_sha256
  )
  library_root <- dirname(attestation$runtime_path)
  rqr_confirm_load_attested_namespace("quantreg", attestation)
  rq <- get("rq", envir = asNamespace("quantreg"))
  time <- seq_len(32L)
  fixture <- data.frame(
    y = 0.2 + 0.04 * time + sin(time / 3) +
      c(rep(-0.2, 16L), rep(0.2, 16L)) +
      c(
        0.011, -0.023, 0.037, -0.041, 0.053, -0.067, 0.071, -0.083,
        0.097, -0.101, 0.113, -0.127, 0.131, -0.149, 0.157, -0.163,
        0.179, -0.181, 0.193, -0.211, 0.223, -0.227, 0.239, -0.251,
        0.263, -0.277, 0.281, -0.293, 0.307, -0.311, 0.317, -0.331
      ),
    x = time
  )
  probabilities <- c(0.10, 0.90)
  fit <- rq(
    y ~ x, tau = probabilities, data = fixture,
    method = specification$method
  )
  future <- data.frame(x = 33:36)
  raw <- as.matrix(stats::predict(fit, newdata = future))
  ordered <- cbind(
    lower = pmin(raw[, 1L], raw[, 2L]),
    upper = pmax(raw[, 1L], raw[, 2L])
  )
  if (!identical(dim(raw), c(4L, 2L)) ||
      any(!is.finite(raw)) || any(ordered[, 2L] < ordered[, 1L])) {
    stop("The actual quantreg fit reference failed.", call. = FALSE)
  }
  list(
    summary = data.frame(
      package = "quantreg", version = specification$version,
      fitting_function = "rq", method = specification$method,
      lower_probability = probabilities[[1L]],
      upper_probability = probabilities[[2L]],
      raw_endpoint_forecasts_retained = TRUE,
      ordering_only_for_interval_metrics = TRUE,
      response_predictive_draws_used = FALSE,
      formals_digest = digest::digest(
        formals(rq), algo = "sha256", serialize = TRUE
      ),
      runtime_tree_digest = attestation$runtime_tree_digest,
      pass = TRUE,
      stringsAsFactors = FALSE
    ),
    raw_forecasts = data.frame(
      horizon = seq_len(nrow(raw)), raw_lower = raw[, 1L],
      raw_upper = raw[, 2L], ordered_lower = ordered[, 1L],
      ordered_upper = ordered[, 2L]
    ),
    dependency_manifest =
      rqr_confirm_dependency_manifest("quantreg", library_root),
    attestation = attestation
  )
}

rqr_confirm_model_bundle <- function(generated) {
  T <- generated$T
  H <- generated$H
  dgp <- generated$dgp
  train_x <- generated$training_predictor
  future_x <- generated$future_predictor
  build <- function(n_time, x, role) {
    if (dgp == "static_gaussian") {
      return(rqr_regression(
        cbind(intercept = 1, predictor = x),
        m0 = c(0, 0), C0 = diag(4, 2), name = "regression"
      ))
    }
    if (dgp %in% c(
        "local_level_gaussian", "local_level_skewed",
        "heteroscedastic_t5", "root_alignment")) {
      return(rqr_polytrend(
        1L, m0 = 0, C0 = matrix(4, 1L, 1L), name = "level"
      ))
    }
    if (dgp %in% c(
        "trend_seasonal_gaussian", "trend_seasonal_skewed")) {
      return(
        rqr_polytrend(
          2L, m0 = c(0, 0), C0 = diag(c(4, 1)),
          name = "trend"
        ) +
          rqr_seasonal(
            12L, 1L, m0 = c(0, 0), C0 = diag(2, 2),
            name = "seasonal"
          )
      )
    }
    if (dgp == "trend_regression_unequal") {
      return(
        rqr_polytrend(
          2L, m0 = c(0, 0), C0 = diag(c(4, 1)),
          name = "trend"
        ) +
          rqr_regression(
            matrix(x, n_time, 1L), m0 = 0, C0 = matrix(2, 1L, 1L),
            name = "regression"
          )
      )
    }
    if (dgp == "break_heavy_tail") {
      return(
        rqr_polytrend(
          1L, m0 = 0, C0 = matrix(4, 1L, 1L), name = "level"
        ) +
          rqr_regression(
            matrix(x, n_time, 1L), m0 = 0, C0 = matrix(2, 1L, 1L),
            name = "regression"
          )
      )
    }
    stop(sprintf("No %s model exists for %s.", role, dgp),
         call. = FALSE)
  }
  training <- build(T, train_x, "training")
  future <- build(H, future_x, "future")
  full <- build(T + H, c(train_x, future_x), "full")
  list(training = training, future = future, full = full)
}

rqr_confirm_discount_profile <- function(config, generated, model) {
  dgp <- generated$dgp
  discounts <- config$frozen_tuning$discounts
  if (dgp %in% c(
      "local_level_gaussian", "local_level_skewed",
      "heteroscedastic_t5", "root_alignment", "static_gaussian")) {
    return(rep(discounts$local_level, length(model$component_dims)))
  }
  if (dgp %in% c(
      "trend_seasonal_gaussian", "trend_seasonal_skewed")) {
    return(unname(discounts$trend_seasonal))
  }
  if (dgp == "trend_regression_unequal") {
    return(unname(discounts$trend_regression))
  }
  if (dgp == "break_heavy_tail") {
    return(unname(discounts$break_regression))
  }
  stop("No frozen discount profile is defined.", call. = FALSE)
}

rqr_confirm_true_W <- function(generated, model, n_time) {
  q <- switch(
    generated$dgp,
    local_level_gaussian = 0.02,
    local_level_skewed = 0.02,
    trend_regression_unequal = c(0.005, 0.0005, 0.05),
    stop("True-W is not defined for this selected DGP.", call. = FALSE)
  )
  if (length(q) == 1L) {
    matrix(q, 1L, 1L)
  } else {
    diag(q, length(q))
  }
}

rqr_confirm_profile_interval <- function(generated, profile) {
  probabilities <- c(
    (1 - generated$coverage_level) / 2,
    1 - (1 - generated$coverage_level) / 2
  )
  empirical <- as.numeric(stats::quantile(
    generated$training_y, probabilities, names = FALSE, type = 8
  ))
  empirical_midpoint <- mean(empirical)
  empirical_half_width <- diff(empirical) / 2
  training_sd <- stats::sd(generated$training_y)
  if (!is.finite(training_sd) || training_sd <= 0 ||
      !is.finite(empirical_half_width) || empirical_half_width <= 0) {
    stop("The empirical initialization interval is degenerate.",
         call. = FALSE)
  }
  midpoint <- if (
      identical(profile$midpoint_rule, "empirical_interval_midpoint")) {
    empirical_midpoint +
      profile$midpoint_shift_training_sd * training_sd
  } else if (identical(profile$midpoint_rule, "training_median")) {
    stats::median(generated$training_y)
  } else {
    stop("An initialization midpoint rule is unknown.", call. = FALSE)
  }
  half_width <- profile$half_width_multiplier * empirical_half_width
  c(lower = midpoint - half_width, upper = midpoint + half_width)
}

rqr_confirm_initialization_profile_name <- function(is_sentinel, chain) {
  if (!is.logical(is_sentinel) || length(is_sentinel) != 1L ||
      is.na(is_sentinel)) {
    stop("is_sentinel must be one nonmissing logical value.", call. = FALSE)
  }
  chain <- rqr_confirm_strict_integer(chain, "chain", 1L, 4L)
  if (isTRUE(is_sentinel)) c("A", "B", "C", "D")[[chain]] else "standard"
}

rqr_confirm_initialization <- function(generated, model, profile,
                                       component_scale = FALSE,
                                       component_scale_base = 1) {
  expanded <- rqrgibbs:::.rqr_expand_model(model, generated$T)
  paths <- rqrgibbs:::.rqr_init_state_paths(
    generated$training_y, expanded$FF, expanded$m0,
    generated$coverage_level, init = list()
  )
  endpoints <- rqr_confirm_profile_interval(generated, profile)
  align <- function(path, target) {
    for (time in seq_len(ncol(path))) {
      direction <- expanded$FF[, time]
      norm2 <- sum(direction^2)
      if (!is.finite(norm2) || norm2 <= 0) {
        stop("An initial observation direction is zero.", call. = FALSE)
      }
      current <- sum(direction * path[, time])
      path[, time] <- path[, time] +
        direction * (target - current) / norm2
    }
    path
  }
  initial <- list(
    state_root1 = align(paths$theta1, endpoints[["lower"]]),
    state_root2 = align(paths$theta2, endpoints[["upper"]]),
    lambda = profile$lambda_initial
  )
  first <- expanded$FF[, 1L]
  first_norm2 <- sum(first^2)
  initial_projection <- sum(first * expanded$m0)
  initial$theta0_root1 <- expanded$m0 +
    first * (endpoints[["lower"]] - initial_projection) / first_norm2
  initial$theta0_root2 <- expanded$m0 +
    first * (endpoints[["upper"]] - initial_projection) / first_norm2
  if (isTRUE(component_scale)) {
    initial$evolution_scale <- rep(
      component_scale_base * profile$component_scale_multiplier,
      length(model$component_dims)
    )
  }
  initial
}

rqr_confirm_dynamic_fit <- function(
    contract, generated, method, chain, ledger,
    provenance_control = list(), profile_name = NULL) {
  chain <- rqr_confirm_strict_integer(chain, "chain", 1L, 4L)
  model_bundle <- rqr_confirm_model_bundle(generated)
  model <- model_bundle$training
  cell_id <- contract$incidence$cell_id[
    contract$incidence$DGP == generated$scenario_id &
      contract$incidence$method == method
  ]
  if (length(cell_id) != 1L) stop("The dynamic fit cell is not unique.",
                                  call. = FALSE)
  profile_name <- profile_name %||% c("A", "B", "C", "D")[[chain]]
  profile <- if (identical(profile_name, "standard")) {
    contract$config$standard_initialization
  } else {
    contract$config$initialization_profiles[[profile_name]]
  }
  if (is.null(profile)) stop("Unknown initialization profile.",
                             call. = FALSE)
  component_method <- method %in% c("M01", "M09", "M10", "M11")
  prior <- if (method == "M07") {
    contract$config$frozen_tuning$common_scale_prior
  } else {
    contract$config$frozen_tuning$component_scale_prior
  }
  prior_median <- prior$rate /
    stats::qgamma(0.5, shape = prior$shape, rate = 1)
  initial <- rqr_confirm_initialization(
    generated, model, profile, component_scale = component_method,
    component_scale_base = prior_median
  )
  method_key <- paste(
    "method", cell_id, generated$replication, "interval", chain,
    sep = "|"
  )
  initial$rng_state <- rqr_confirm_state_from_ledger(ledger, method_key)
  learning_rate <- switch(
    method, M09 = 0.5, M10 = 2, 1
  )
  learning_mode <- if (method == "M11") {
    "learned_pseudoresidual_normalized"
  } else {
    "fixed_rate"
  }
  schedule <- if (method == "M11") {
    contract$config$schedules$learned_dynamic_rqr
  } else {
    contract$config$schedules$dynamic_rqr
  }
  common <- list(
    y = generated$training_y,
    model = model,
    coverage_level = generated$coverage_level,
    learning_rate = learning_rate,
    learning_rate_mode = learning_mode,
    loss_reference_scale = stats::sd(generated$training_y)^2,
    lambda_prior = list(shape = 4, rate = 4),
    numerical_policy = "fail",
    provenance_control = provenance_control,
    mcmc_control = list(
      n_burn = schedule$burn, n_mcmc = schedule$retain,
      thin = schedule$thin, backend = "cpp",
      store_state_draws = chain > 1L,
      store_latent_draws = FALSE, verbose = FALSE
    ),
    init = initial
  )
  forecast_W <- NULL
  forecast_templates <- NULL
  if (component_method) {
    templates <- lapply(
      model$component_dims, function(dimension) diag(1, dimension)
    )
    common$evolution_mode <- "component_scale"
    common$component_templates <- templates
    common$evolution_scale_prior <-
      contract$config$frozen_tuning$component_scale_prior
    common$evolution_scale_initial <- rep(
      1, length(model$component_dims)
    )
    forecast_templates <- lapply(
      model_bundle$future$component_dims,
      function(dimension) diag(1, dimension)
    )
  } else if (method == "M07") {
    common_model <- model
    common_model$component_dims <- length(common_model$m0)
    common_model$component_names <- "common"
    common$model <- rqr_as_dlm_model(common_model)
    common$evolution_mode <- "component_scale"
    common$component_templates <- list(diag(1, length(model$m0)))
    common$evolution_scale_prior <-
      contract$config$frozen_tuning$common_scale_prior
    common$evolution_scale_initial <- 1
    initial$evolution_scale <-
      prior_median * profile$component_scale_multiplier
    common$init <- initial
    forecast_templates <- list(diag(1, length(model$m0)))
  } else if (method == "M06") {
    df <- rqr_confirm_discount_profile(
      contract$config, generated, model
    )
    reference_variance <- stats::var(generated$training_y)
    full_template <- rqr_freeze_discount_template(
      model_bundle$full, generated$T + generated$H,
      df = df, dim.df = model$component_dims,
      reference_variance = reference_variance,
      reference_design = model_bundle$full$FF,
      numerical_policy = "fail"
    )
    train_template <- rqr_freeze_discount_template(
      model, generated$T, df = df,
      dim.df = model$component_dims,
      reference_variance = reference_variance,
      reference_design = model$FF,
      numerical_policy = "fail"
    )
    if (!identical(
        train_template$W,
        full_template$W[, , seq_len(generated$T), drop = FALSE]
      )) {
      stop("The frozen discount future recursion changed training slices.",
           call. = FALSE)
    }
    common$evolution_spec <- train_template
    forecast_W <- full_template$W[
      , , generated$T + seq_len(generated$H), drop = FALSE
    ]
  } else if (method == "M08") {
    W <- rqr_confirm_true_W(generated, model, generated$T)
    common$evolution_mode <- "fixed_W"
    common$W <- W
    forecast_W <- W
  } else {
    stop("Unsupported dynamic RQR method.", call. = FALSE)
  }
  fit <- do.call(rqr_dlm_fit, common)
  forecast_key <- paste(
    "forecast", cell_id, generated$replication, "interval", chain,
    sep = "|"
  )
  forecast <- rqr_confirm_with_state(
    rqr_confirm_state_from_ledger(ledger, forecast_key),
    rqr_forecast_roots(
      fit, FF_future = model_bundle$future$FF,
      GG_future = model_bundle$future$GG,
      W_future = forecast_W,
      component_templates_future = forecast_templates,
      nd = NULL, seed = NULL,
      numerical_policy = "fail"
    )
  )
  interval <- predict_interval(fit)
  if (fit$model_spec$numerical_repair_count != 0L ||
      forecast$diagnostics$repair_count != 0L ||
      !isTRUE(fit$model_spec$exact_joint_target) ||
      any(!is.finite(c(
        interval$lower_mean, interval$upper_mean,
        forecast$lower_mean, forecast$upper_mean
      )))) {
    stop("A dynamic RQR fit failed its exact numerical contract.",
         call. = FALSE)
  }
  list(
    training_lower = interval$lower_mean,
    training_upper = interval$upper_mean,
    future_lower = forecast$lower_mean,
    future_upper = forecast$upper_mean,
    fit = fit, forecast = forecast,
    diagnostics = list(
      numerical_repairs = 0L,
      exact_joint_target = TRUE,
      promotion_eligible =
        isTRUE(fit$provenance$reproducibility_eligible),
      profile = profile_name,
      learned_lambda =
        identical(method, "M11")
    )
  )
}

rqr_confirm_fixed_design <- function(
    contract, generated, chain, ledger, provenance_control = list(),
    profile_name = NULL) {
  chain <- rqr_confirm_strict_integer(chain, "chain", 1L, 4L)
  profile_name <- profile_name %||% c("A", "B", "C", "D")[[chain]]
  profile <- if (identical(profile_name, "standard")) {
    contract$config$standard_initialization
  } else {
    contract$config$initialization_profiles[[profile_name]]
  }
  if (is.null(profile)) stop("Unknown initialization profile.",
                             call. = FALSE)
  endpoints <- rqr_confirm_profile_interval(generated, profile)
  cell_id <- contract$incidence$cell_id[
    contract$incidence$DGP == generated$scenario_id &
      contract$incidence$method == "M03"
  ]
  time_train <- seq_len(generated$T) / generated$T
  time_future <- (generated$T + seq_len(generated$H)) / generated$T
  X <- cbind(
    intercept = 1, time = time_train,
    predictor = generated$training_predictor
  )
  X_future <- cbind(
    intercept = 1, time = time_future,
    predictor = generated$future_predictor
  )
  state <- rqr_confirm_state_from_ledger(
    ledger,
    paste(
      "method", cell_id, generated$replication, "interval", chain,
      sep = "|"
    )
  )
  schedule <- contract$config$schedules$fixed_design_rqr
  fit <- rqr_mcmc_fit(
    generated$training_y, X,
    coverage_level = generated$coverage_level,
    learning_rate = 1, learning_rate_mode = "fixed_rate",
    beta_prior_obj = beta_prior(
      "ridge", ridge = list(
        tau2 = contract$config$frozen_tuning$
          fixed_design_ridge_variance
      )
    ),
    numerical_policy = "fail",
    provenance_control = provenance_control,
    mcmc_control = list(
      n_burn = schedule$burn, n_mcmc = schedule$retain,
      thin = schedule$thin, verbose = FALSE
    ),
    init = list(
      rng_state = state,
      beta_root1 = c(endpoints[["lower"]], rep(0, ncol(X) - 1L)),
      beta_root2 = c(endpoints[["upper"]], rep(0, ncol(X) - 1L)),
      lambda = profile$lambda_initial
    )
  )
  training <- predict_interval(fit, X_new = X)
  future <- predict_interval(fit, X_new = X_future)
  list(
    training_lower = training$lower_mean,
    training_upper = training$upper_mean,
    future_lower = future$lower_mean,
    future_upper = future$upper_mean,
    fit = fit,
    diagnostics = list(
      numerical_repairs = fit$model_spec$numerical_repair_count,
      exact_joint_target = TRUE,
      promotion_eligible =
        isTRUE(fit$provenance$reproducibility_eligible)
    )
  )
}

rqr_confirm_empirical_interval <- function(contract, generated) {
  window <- contract$config$frozen_tuning$empirical_window
  values <- tail(generated$training_y, min(window, generated$T))
  probabilities <- c(
    (1 - generated$coverage_level) / 2,
    1 - (1 - generated$coverage_level) / 2
  )
  endpoints <- as.numeric(stats::quantile(
    values, probabilities, names = FALSE, type = 8
  ))
  list(
    training_lower = rep(endpoints[[1L]], generated$T),
    training_upper = rep(endpoints[[2L]], generated$T),
    future_lower = rep(endpoints[[1L]], generated$H),
    future_upper = rep(endpoints[[2L]], generated$H),
    diagnostics = list(
      deterministic = TRUE, window = length(values),
      response_predictive_draws = FALSE
    )
  )
}

rqr_confirm_static_quantile <- function(
    contract, generated, quantreg_attestation_path) {
  specification <- contract$config$comparator$quantreg
  attestation <- rqr_confirm_read_attestation(
    quantreg_attestation_path, "quantreg", specification$version,
    specification$source_sha256
  )
  rqr_confirm_load_attested_namespace("quantreg", attestation)
  rq <- get("rq", envir = asNamespace("quantreg"))
  time_train <- seq_len(generated$T) / generated$T
  time_future <- (generated$T + seq_len(generated$H)) / generated$T
  data <- data.frame(
    y = generated$training_y, time = time_train,
    predictor = generated$training_predictor
  )
  formula <- if (
      stats::sd(data$predictor) >
        sqrt(.Machine$double.eps)) {
    y ~ time + predictor
  } else {
    y ~ time
  }
  probabilities <- c(
    (1 - generated$coverage_level) / 2,
    1 - (1 - generated$coverage_level) / 2
  )
  fit <- rq(
    formula, tau = probabilities,
    data = data, method = specification$method
  )
  raw_training <- as.matrix(stats::predict(fit, newdata = data))
  raw_future <- as.matrix(stats::predict(
    fit,
    newdata = data.frame(
      time = time_future, predictor = generated$future_predictor
    )
  ))
  list(
    training_raw_lower = raw_training[, 1L],
    training_raw_upper = raw_training[, 2L],
    training_lower = pmin(raw_training[, 1L], raw_training[, 2L]),
    training_upper = pmax(raw_training[, 1L], raw_training[, 2L]),
    future_raw_lower = raw_future[, 1L],
    future_raw_upper = raw_future[, 2L],
    future_lower = pmin(raw_future[, 1L], raw_future[, 2L]),
    future_upper = pmax(raw_future[, 1L], raw_future[, 2L]),
    fit = fit,
    diagnostics = list(
      deterministic = TRUE, raw_endpoints_retained = TRUE,
      response_predictive_draws = FALSE
    )
  )
}

rqr_confirm_dynamic_quantile <- function(
    contract, generated, chain, ledger, exdqlm_attestation_path,
    profile_name = NULL) {
  chain <- rqr_confirm_strict_integer(chain, "chain", 1L, 4L)
  specification <- contract$config$comparator$exdqlm
  attestation <- rqr_confirm_read_attestation(
    exdqlm_attestation_path, "exdqlm", specification$version,
    specification$source_sha256
  )
  rqr_confirm_load_attested_namespace("exdqlm", attestation)
  namespace <- asNamespace("exdqlm")
  mcmc <- get("exdqlmMCMC", envir = namespace)
  forecast_function <- get("exdqlmForecast", envir = namespace)
  model_bundle <- rqr_confirm_model_bundle(generated)
  model <- unclass(model_bundle$training)
  future_model <- unclass(model_bundle$future)
  profile_name <- profile_name %||% c("A", "B", "C", "D")[[chain]]
  profile <- if (identical(profile_name, "standard")) {
    contract$config$standard_initialization
  } else {
    contract$config$initialization_profiles[[profile_name]]
  }
  if (is.null(profile)) stop("Unknown initialization profile.",
                             call. = FALSE)
  initial_interval <- rqr_confirm_profile_interval(generated, profile)
  df <- rqr_confirm_discount_profile(
    contract$config, generated, model_bundle$training
  )
  probabilities <- c(
    lower = (1 - generated$coverage_level) / 2,
    upper = 1 - (1 - generated$coverage_level) / 2
  )
  schedule <- contract$config$schedules$dynamic_quantile_endpoint
  cell_id <- contract$incidence$cell_id[
    contract$incidence$DGP == generated$scenario_id &
      contract$incidence$method == "M02"
  ]
  fits <- forecasts <- vector("list", 2L)
  for (index in seq_along(probabilities)) {
    endpoint <- names(probabilities)[[index]]
    endpoint_model <- model
    direction <- as.numeric(endpoint_model$FF[, 1L])
    norm2 <- sum(direction^2)
    if (!is.finite(norm2) || norm2 <= 0) {
      stop("The dynamic-quantile initial direction is zero.",
           call. = FALSE)
    }
    current <- sum(direction * as.numeric(endpoint_model$m0))
    endpoint_model$m0 <- endpoint_model$m0 +
      direction * (
        initial_interval[[endpoint]] - current
      ) / norm2
    method_key <- paste(
      "method", cell_id, generated$replication, endpoint, chain,
      sep = "|"
    )
    fits[[index]] <- rqr_confirm_with_state(
      rqr_confirm_state_from_ledger(ledger, method_key),
      mcmc(
        y = generated$training_y,
        p0 = unname(probabilities[[index]]), model = endpoint_model,
        df = df, dim.df = model_bundle$training$component_dims,
        dqlm.ind = TRUE, init.from.vb = FALSE,
        fix.sigma = FALSE,
        sig.init = profile$component_scale_multiplier,
        PriorSigma = NULL,
        n.burn = schedule$burn, n.mcmc = schedule$retain,
        verbose = FALSE, trace.diagnostics = FALSE
      )
    )
    forecast_key <- paste(
      "forecast", cell_id, generated$replication, endpoint, chain,
      sep = "|"
    )
    forecasts[[index]] <- rqr_confirm_with_state(
      rqr_confirm_state_from_ledger(ledger, forecast_key),
      forecast_function(
        start.t = generated$T, k = generated$H,
        m1 = fits[[index]], fFF = future_model$FF,
        fGG = future_model$GG, plot = FALSE,
        return.draws = FALSE
      )
    )
  }
  raw_training <- cbind(
    lower = as.numeric(fits[[1L]]$theta.out$fm),
    upper = as.numeric(fits[[2L]]$theta.out$fm)
  )
  raw_future <- cbind(
    lower = as.numeric(forecasts[[1L]]$ff),
    upper = as.numeric(forecasts[[2L]]$ff)
  )
  list(
    training_raw_lower = raw_training[, 1L],
    training_raw_upper = raw_training[, 2L],
    training_lower = pmin(raw_training[, 1L], raw_training[, 2L]),
    training_upper = pmax(raw_training[, 1L], raw_training[, 2L]),
    future_raw_lower = raw_future[, 1L],
    future_raw_upper = raw_future[, 2L],
    future_lower = pmin(raw_future[, 1L], raw_future[, 2L]),
    future_upper = pmax(raw_future[, 1L], raw_future[, 2L]),
    fits = fits, forecasts = forecasts,
    diagnostics = list(
      dqlm_ind = all(vapply(
        fits, function(fit) isTRUE(fit$dqlm.ind), logical(1L)
      )),
      raw_endpoints_retained = TRUE,
      profile = profile_name,
      sig_init = profile$component_scale_multiplier,
      response_predictive_draws = FALSE
    )
  )
}

rqr_confirm_replication_metrics <- function(generated, result) {
  future_y <- generated$generated_future_response
  covered <- sweep(
    future_y, 2L, result$future_lower, ">="
  ) & sweep(future_y, 2L, result$future_upper, "<=")
  training_loss <- sum(rqr_check_loss(
    rqr_residual_product(
      generated$training_y,
      result$training_lower, result$training_upper
    ),
    generated$coverage_level
  ))
  target <- if (identical(result$endpoint_target, "quantile")) {
    generated$quantile_conditional_mean_root
  } else {
    generated$oracle_conditional_mean_root
  }
  cross_target <- if (identical(result$endpoint_target, "quantile")) {
    generated$oracle_conditional_mean_root
  } else {
    generated$quantile_conditional_mean_root
  }
  realized_target <- if (
      identical(result$endpoint_target, "quantile")) {
    generated$realized_quantile_path
  } else {
    generated$realized_root_path
  }
  alpha <- 1 - generated$coverage_level
  interval_score <- sweep(
    matrix(
      result$future_upper - result$future_lower,
      nrow(future_y), ncol(future_y), byrow = TRUE
    ),
    1L, rep(0, nrow(future_y)), "+"
  )
  interval_score <- interval_score +
    2 / alpha * pmax(
      matrix(result$future_lower, nrow(future_y), ncol(future_y),
             byrow = TRUE) - future_y,
      0
    ) +
    2 / alpha * pmax(
      future_y -
        matrix(result$future_upper, nrow(future_y), ncol(future_y),
               byrow = TRUE),
      0
    )
  heldout_loss <- rqr_check_loss(
    (future_y -
       matrix(result$future_lower, nrow(future_y), ncol(future_y),
              byrow = TRUE)) *
      (future_y -
         matrix(result$future_upper, nrow(future_y), ncol(future_y),
                byrow = TRUE)),
    generated$coverage_level
  )
  output <- data.frame(
    training_loss = training_loss,
    heldout_rqr_loss = mean(heldout_loss),
    aggregate_coverage = mean(covered),
    mean_width = mean(result$future_upper - result$future_lower),
    future_mean_lower = mean(result$future_lower),
    future_mean_upper = mean(result$future_upper),
    future_mean_midpoint =
      mean(0.5 * (result$future_lower + result$future_upper)),
    central_interval_score = mean(interval_score),
    endpoint_rmse_lower = sqrt(mean(
      (result$future_lower -
         target[, "lower"])^2
    )),
    endpoint_rmse_upper = sqrt(mean(
      (result$future_upper -
         target[, "upper"])^2
    )),
    cross_target_distance = if (
        identical(result$endpoint_target, "quantile")) {
      sqrt(mean(c(
      result$future_lower -
        cross_target[, "lower"],
      result$future_upper -
        cross_target[, "upper"]
      )^2))
    } else {
      NA_real_
    },
    realized_root_rmse = sqrt(mean(c(
      sweep(
        realized_target[, , "lower"], 2L,
        result$future_lower, "-"
      ),
      sweep(
        realized_target[, , "upper"], 2L,
        result$future_upper, "-"
      )
    )^2)),
    stringsAsFactors = FALSE
  )
  for (horizon in seq_len(generated$H)) {
    output[[sprintf("coverage_h%02d", horizon)]] <-
      mean(covered[, horizon])
  }
  output
}

rqr_confirm_execute_method <- function(
    contract, generated, method, chain, ledger,
    provenance_control = list(),
    exdqlm_attestation_path = NULL,
    quantreg_attestation_path = NULL,
    profile_name = NULL) {
  if (method %in% c("M01", "M06", "M07", "M08", "M09", "M10", "M11")) {
    result <- rqr_confirm_dynamic_fit(
      contract, generated, method, chain, ledger, provenance_control,
      profile_name = profile_name
    )
  } else if (method == "M02") {
    result <- rqr_confirm_dynamic_quantile(
      contract, generated, chain, ledger, exdqlm_attestation_path,
      profile_name = profile_name
    )
  } else if (method == "M03") {
    result <- rqr_confirm_fixed_design(
      contract, generated, chain, ledger, provenance_control,
      profile_name = profile_name
    )
  } else if (method == "M04") {
    result <- rqr_confirm_static_quantile(
      contract, generated, quantreg_attestation_path
    )
  } else if (method == "M05") {
    result <- rqr_confirm_empirical_interval(contract, generated)
  } else {
    stop("The incidence matrix requested an omitted method.",
         call. = FALSE)
  }
  result$endpoint_target <- if (method %in% c("M02", "M04")) {
    "quantile"
  } else {
    "rqr"
  }
  endpoint_values <- c(
    result$training_lower, result$training_upper,
    result$future_lower, result$future_upper
  )
  if (length(result$training_lower) != generated$T ||
      length(result$training_upper) != generated$T ||
      length(result$future_lower) != generated$H ||
      length(result$future_upper) != generated$H ||
      any(!is.finite(endpoint_values)) ||
      any(result$training_upper < result$training_lower) ||
      any(result$future_upper < result$future_lower)) {
    stop(
      "Nonfinite primary outputs or unordered interval endpoints.",
      call. = FALSE
    )
  }
  if (!is.null(result$diagnostics$promotion_eligible) &&
      !isTRUE(result$diagnostics$promotion_eligible)) {
    stop("A fitted method failed runtime provenance eligibility.",
         call. = FALSE)
  }
  result$metrics <- rqr_confirm_replication_metrics(generated, result)
  required_metrics <- setdiff(
    names(result$metrics), "cross_target_distance"
  )
  if (any(!is.finite(unlist(
      result$metrics[required_metrics], use.names = FALSE
    ))) ||
      (identical(result$endpoint_target, "quantile") &&
        !is.finite(result$metrics$cross_target_distance)) ||
      (identical(result$endpoint_target, "rqr") &&
        !is.na(result$metrics$cross_target_distance))) {
    stop("Nonfinite primary outputs were produced.", call. = FALSE)
  }
  result
}

rqr_confirm_replication_plan <- function(contract, planning = "maximum") {
  fit_plan <- rqr_confirm_fit_plan(contract, planning)
  sentinel_map <- rqr_confirm_sentinel_map(contract, planning)
  scenarios <- names(contract$config$scenarios)
  rows <- vector("list", 0L)
  index <- 0L
  for (scenario_id in scenarios) {
    scenario_rows <- fit_plan[
      fit_plan$DGP == scenario_id, , drop = FALSE
    ]
    maximum <- max(scenario_rows$replications)
    rule <- if (any(scenario_rows$replication_rule == "C")) {
      "C"
    } else if (any(scenario_rows$replication_rule == "S")) {
      "S"
    } else {
      "F"
    }
    batch_size <- switch(rule, C = 100L, S = 50L, F = 200L)
    for (replication in seq_len(maximum)) {
      index <- index + 1L
      batch <- as.integer(ceiling(replication / batch_size))
      rows[[index]] <- data.frame(
        replication_task_id = sprintf(
          "%s__rep%04d", scenario_id, replication
        ),
        DGP = scenario_id,
        replication = replication,
        replication_rule = rule,
        batch = batch,
        embedded_sentinel = any(
          sentinel_map$DGP == scenario_id &
            sentinel_map$replication == replication
        ),
        methods = paste(
          scenario_rows$method[
            replication <= scenario_rows$replications
          ],
          collapse = "|"
        ),
        execution_authorized = FALSE,
        stringsAsFactors = FALSE
      )
    }
  }
  output <- do.call(rbind, rows)
  output <- output[order(
    match(output$DGP, scenarios),
    output$batch,
    !output$embedded_sentinel,
    output$replication
  ), , drop = FALSE]
  rownames(output) <- NULL
  output
}

rqr_confirm_wave_plan <- function(contract, planning = "maximum") {
  planning <- match.arg(planning, c("initial", "central", "maximum"))
  tasks <- rqr_confirm_replication_plan(contract, planning)
  scenario_order <- names(contract$config$scenarios)
  tasks$batch_group <- vapply(
    contract$config$scenarios[tasks$DGP],
    `[[`, character(1L), "batch_group"
  )
  batch_target <- function(replication, rule) {
    batch <- switch(
      rule,
      C = contract$config$batching$core,
      S = contract$config$batching$sensitivity,
      F = contract$config$batching$frozen,
      stop("A wave task has an unknown replication rule.",
           call. = FALSE)
    )
    if (replication <= batch$initial || batch$increment == 0L) {
      return(as.integer(batch$initial))
    }
    as.integer(
      batch$initial +
        ceiling((replication - batch$initial) / batch$increment) *
          batch$increment
    )
  }
  tasks$batch_target <- mapply(
    batch_target, tasks$replication, tasks$replication_rule,
    USE.NAMES = FALSE
  )
  tasks$study_stage <- ifelse(
    tasks$replication_rule == "S", "sensitivity", "core"
  )
  tasks$batch_sequence <- mapply(
    function(target, rule) {
      batch <- switch(
        rule,
        C = contract$config$batching$core,
        S = contract$config$batching$sensitivity,
        F = contract$config$batching$frozen
      )
      if (batch$increment == 0L) {
        0L
      } else {
        as.integer((target - batch$initial) / batch$increment)
      }
    },
    tasks$batch_target, tasks$replication_rule,
    USE.NAMES = FALSE
  )
  tasks$phase <- ifelse(
    tasks$embedded_sentinel, "sentinel", "standard"
  )
  tasks$mode <- ifelse(
    tasks$embedded_sentinel, "sentinel-core", "execute-confirmatory"
  )
  tasks$wave_id <- sprintf(
    "%s__target%04d__%s",
    tasks$batch_group, tasks$batch_target, tasks$phase
  )
  tasks$worker_limit <- ifelse(
    tasks$embedded_sentinel,
    contract$config$resources$sentinel_workers,
    contract$config$resources$workers
  )
  tasks$worker_slot <- NA_integer_
  for (wave_id in unique(tasks$wave_id)) {
    indices <- which(tasks$wave_id == wave_id)
    indices <- indices[order(
      match(tasks$DGP[indices], scenario_order),
      tasks$replication[indices], method = "radix"
    )]
    tasks$worker_slot[indices] <-
      (seq_along(indices) - 1L) %% tasks$worker_limit[indices] + 1L
  }
  execution_order <- order(
    tasks$batch_sequence,
    match(tasks$study_stage, c("core", "sensitivity")),
    match(tasks$phase, c("sentinel", "standard")),
    match(tasks$batch_group, unique(tasks$batch_group)),
    tasks$worker_slot,
    tasks$replication,
    method = "radix"
  )
  tasks <- tasks[execution_order, , drop = FALSE]
  tasks$execution_order <- seq_len(nrow(tasks))
  rownames(tasks) <- NULL
  tasks
}

rqr_confirm_wave_catalog <- function(contract, planning = "maximum") {
  plan <- rqr_confirm_wave_plan(contract, planning)
  wave_ids <- unique(plan$wave_id)
  rows <- lapply(seq_along(wave_ids), function(index) {
    block <- plan[plan$wave_id == wave_ids[[index]], , drop = FALSE]
    scalar <- function(field) {
      values <- unique(block[[field]])
      if (length(values) != 1L) {
        stop("A canonical wave has inconsistent metadata.",
             call. = FALSE)
      }
      values[[1L]]
    }
    batch_group <- scalar("batch_group")
    batch_target <- as.integer(scalar("batch_target"))
    phase <- scalar("phase")
    same_batch_sentinel <- sprintf(
      "%s__target%04d__sentinel", batch_group, batch_target
    )
    earlier_group_targets <- unique(plan$batch_target[
      plan$batch_group == batch_group &
        plan$phase == "standard" &
        plan$batch_target < batch_target
    ])
    prior_batch_target <- if (length(earlier_group_targets)) {
      max(earlier_group_targets)
    } else {
      NA_integer_
    }
    data.frame(
      canonical_wave_index = as.integer(index),
      wave_id = wave_ids[[index]],
      mode = scalar("mode"),
      phase = phase,
      batch_group = batch_group,
      batch_target = batch_target,
      batch_sequence = as.integer(scalar("batch_sequence")),
      study_stage = scalar("study_stage"),
      worker_limit = as.integer(scalar("worker_limit")),
      task_count = nrow(block),
      same_batch_sentinel_wave_id = if (
          identical(phase, "standard")) {
        same_batch_sentinel
      } else {
        ""
      },
      prior_batch_target = prior_batch_target,
      required_predecessor_wave_ids = paste(
        wave_ids[seq_len(index - 1L)], collapse = "|"
      ),
      stringsAsFactors = FALSE
    )
  })
  output <- do.call(rbind, rows)
  rownames(output) <- NULL
  output
}

rqr_confirm_wave_binding <- function(
    run_id, expected_commit, authorization, config_sha256,
    incidence_sha256, seed_ledger_sha256, task_plan_sha256,
    wave_plan_sha256) {
  scalar_text <- function(value, name, pattern = NULL) {
    if (!is.character(value) || length(value) != 1L ||
        is.na(value) || !nzchar(value) ||
        (!is.null(pattern) && !grepl(pattern, tolower(value)))) {
      stop(sprintf("Invalid wave-state binding field: %s.", name),
           call. = FALSE)
    }
    tolower(value)
  }
  required_authorization <- c(
    "reviewed_implementation_commit", "authorization_commit",
    "primary_runtime_tree_digest"
  )
  if (is.null(authorization) ||
      !all(required_authorization %in% names(authorization))) {
    stop("The wave-state authorization bundle is incomplete.",
         call. = FALSE)
  }
  binding <- list(
    schema_version = "rqrgibbs_dlm_wave_run/1.0.0",
    run_id = scalar_text(
      run_id, "run_id", "^[a-z0-9][a-z0-9._-]{0,127}$"
    ),
    authorization_commit = scalar_text(
      authorization$authorization_commit,
      "authorization_commit", "^[0-9a-f]{40}$"
    ),
    reviewed_implementation_commit = scalar_text(
      authorization$reviewed_implementation_commit,
      "reviewed_implementation_commit", "^[0-9a-f]{40}$"
    ),
    runtime_tree_digest = scalar_text(
      authorization$primary_runtime_tree_digest,
      "runtime_tree_digest", "^[0-9a-f]{64}$"
    ),
    config_sha256 = scalar_text(
      config_sha256, "config_sha256", "^[0-9a-f]{64}$"
    ),
    incidence_sha256 = scalar_text(
      incidence_sha256, "incidence_sha256", "^[0-9a-f]{64}$"
    ),
    seed_ledger_sha256 = scalar_text(
      seed_ledger_sha256, "seed_ledger_sha256", "^[0-9a-f]{64}$"
    ),
    task_plan_sha256 = scalar_text(
      task_plan_sha256, "task_plan_sha256", "^[0-9a-f]{64}$"
    ),
    wave_plan_sha256 = scalar_text(
      wave_plan_sha256, "wave_plan_sha256", "^[0-9a-f]{64}$"
    )
  )
  expected_commit <- scalar_text(
    expected_commit, "expected_commit", "^[0-9a-f]{40}$"
  )
  if (!identical(binding$authorization_commit, expected_commit) ||
      !identical(
        binding$task_plan_sha256,
        tolower(authorization$task_plan_sha256 %||% "")
      ) ||
      !identical(
        binding$seed_ledger_sha256,
        tolower(authorization$seed_ledger_sha256 %||% "")
      )) {
    stop("The wave-state binding differs from the authorization bundle.",
         call. = FALSE)
  }
  binding$binding_digest <- digest::digest(
    binding, algo = "sha256", serialize = TRUE
  )
  binding
}

rqr_confirm_wave_state_transition <- function(
    catalog, completions, requested_wave_id, binding_digest,
    prior_batch_decision = NULL) {
  required_completion_fields <- c(
    "canonical_wave_index", "wave_id", "binding_digest",
    "decision", "completion_sha256", "artifact_manifest_sha256"
  )
  if (!is.data.frame(catalog) || !nrow(catalog) ||
      anyDuplicated(catalog$wave_id) ||
      !identical(
        catalog$canonical_wave_index,
        seq_len(nrow(catalog))
      )) {
    stop("The canonical wave catalog is invalid.", call. = FALSE)
  }
  if (is.null(completions)) {
    completions <- data.frame(
      canonical_wave_index = integer(), wave_id = character(),
      binding_digest = character(), decision = character(),
      completion_sha256 = character(),
      artifact_manifest_sha256 = character(),
      stringsAsFactors = FALSE
    )
  }
  if (!is.data.frame(completions) ||
      !identical(names(completions), required_completion_fields) ||
      anyDuplicated(completions$canonical_wave_index) ||
      anyDuplicated(completions$wave_id) ||
      (nrow(completions) && !identical(
        completions$canonical_wave_index, seq_len(nrow(completions))
      )) ||
      any(completions$binding_digest != binding_digest) ||
      any(!completions$decision %in% c(
        "passed", "failed", "skipped_precision_stop"
      )) ||
      any(!grepl(
        "^[0-9a-f]{64}$", completions$completion_sha256
      )) ||
      any(!grepl(
        "^$|^[0-9a-f]{64}$",
        completions$artifact_manifest_sha256
      ))) {
    stop("The append-only wave completion history is invalid.",
         call. = FALSE)
  }
  if (nrow(completions)) {
    expected_history <- catalog[
      seq_len(nrow(completions)),
      c("canonical_wave_index", "wave_id"), drop = FALSE
    ]
    observed_history <- completions[
      c("canonical_wave_index", "wave_id")
    ]
    rownames(expected_history) <- rownames(observed_history) <- NULL
    if (!identical(observed_history, expected_history)) {
      stop("The wave history skipped or replayed a canonical wave.",
           call. = FALSE)
    }
    if (any(completions$decision == "failed")) {
      stop("A failed wave permanently blocks later waves.",
           call. = FALSE)
    }
  }
  next_index <- nrow(completions) + 1L
  if (next_index > nrow(catalog)) {
    stop("Every canonical wave already has a terminal record.",
         call. = FALSE)
  }
  current <- catalog[next_index, , drop = FALSE]
  if (!identical(as.character(requested_wave_id), current$wave_id)) {
    stop("Only the next canonical wave may be requested.",
         call. = FALSE)
  }
  action <- "launch"
  decision_sha256 <- ""
  prior_action <- ""
  if (!is.na(current$prior_batch_target)) {
    required_decision_fields <- c(
      "batch_group", "replications", "next_action",
      "next_replications", "binding_digest", "decision_sha256"
    )
    if (is.null(prior_batch_decision) ||
        !all(required_decision_fields %in%
             names(prior_batch_decision))) {
      stop("A later batch requires its prior batch decision.",
           call. = FALSE)
    }
    prior_action <- as.character(
      prior_batch_decision$next_action[[1L]]
    )
    decision_sha256 <- tolower(as.character(
      prior_batch_decision$decision_sha256[[1L]]
    ))
    if (!identical(
        as.character(prior_batch_decision$batch_group[[1L]]),
        current$batch_group
      ) ||
        !identical(
          as.character(prior_batch_decision$binding_digest[[1L]]),
          binding_digest
        ) ||
        !grepl("^[0-9a-f]{64}$", decision_sha256)) {
      stop("Prior batch evidence belongs to another run or target.",
           call. = FALSE)
    }
    prior_replications <- rqr_confirm_strict_integer(
      prior_batch_decision$replications[[1L]],
      "prior batch replications", 1L
    )
    if (prior_action == "add_complete_paired_DGP_batch") {
      if (!identical(
          prior_replications,
          as.integer(current$prior_batch_target)
        ) ||
          !identical(
          rqr_confirm_strict_integer(
            prior_batch_decision$next_replications[[1L]],
            "prior batch next replications", 1L
          ),
          as.integer(current$batch_target)
        )) {
        stop("The prior batch decision authorizes another target.",
             call. = FALSE)
      }
    } else if (prior_action %in% c(
        "precision_pass_stop",
        "maximum_reached_report_unmet_precision")) {
      if (prior_replications > as.integer(current$prior_batch_target)) {
        stop("The precision-stop decision is from a future target.",
             call. = FALSE)
      }
      action <- "skip"
    } else {
      stop("The prior batch decision has an unknown action.",
           call. = FALSE)
    }
  }
  same_batch_sentinel_pass <- NA
  if (identical(action, "launch") &&
      identical(current$phase, "standard")) {
    sentinel_index <- match(
      current$same_batch_sentinel_wave_id, completions$wave_id
    )
    same_batch_sentinel_pass <-
      !is.na(sentinel_index) &&
      identical(completions$decision[[sentinel_index]], "passed")
    if (!isTRUE(same_batch_sentinel_pass)) {
      stop("A standard wave requires its same-batch sentinel pass.",
           call. = FALSE)
    }
  }
  list(
    action = action,
    current = current,
    required_predecessor_wave_ids = completions$wave_id,
    predecessor_completion_sha256 =
      completions$completion_sha256,
    predecessor_artifact_manifest_sha256 =
      completions$artifact_manifest_sha256,
    same_batch_sentinel_pass = same_batch_sentinel_pass,
    prior_batch_decision_sha256 = decision_sha256,
    prior_batch_next_action = prior_action
  )
}

rqr_confirm_wave_state_records <- function(
    state_root, catalog, binding) {
  if (!dir.exists(state_root)) {
    return(list(
      starts = list(), completion_values = list(), completions = NULL
    ))
  }
  run_contract_path <- file.path(state_root, "run_contract.json")
  if (!file.exists(run_contract_path)) {
    stop("The wave-state root omitted its run contract.",
         call. = FALSE)
  }
  stored <- jsonlite::read_json(
    run_contract_path, simplifyVector = TRUE
  )
  binding_fields <- names(binding)
  if (!all(binding_fields %in% names(stored)) ||
      !identical(
        rqr_confirm_strict_integer(
          stored$canonical_wave_count,
          "canonical wave count", 1L
        ),
        nrow(catalog)
      ) ||
      any(vapply(binding_fields, function(field) {
        !identical(
          as.character(stored[[field]]),
          as.character(binding[[field]])
        )
      }, logical(1L)))) {
    stop("The wave-state root belongs to another run binding.",
         call. = FALSE)
  }
  start_root <- file.path(state_root, "starts")
  completion_root <- file.path(state_root, "completions")
  if (!dir.exists(start_root) || !dir.exists(completion_root)) {
    stop("The wave-state record directories are incomplete.",
         call. = FALSE)
  }
  start_paths <- sort(list.files(
    start_root, pattern = "\\.json$", full.names = TRUE
  ), method = "radix")
  completion_paths <- sort(list.files(
    completion_root, pattern = "\\.json$", full.names = TRUE
  ), method = "radix")
  if (length(start_paths) != length(completion_paths)) {
    stop(
      "An incomplete wave start permanently blocks replay and continuation.",
      call. = FALSE
    )
  }
  starts <- lapply(start_paths, function(path) {
    jsonlite::read_json(path, simplifyVector = TRUE)
  })
  completion_values <- lapply(
      seq_along(completion_paths), function(index) {
    value <- jsonlite::read_json(
      completion_paths[[index]], simplifyVector = TRUE
    )
    start <- starts[[index]]
    catalog_row <- catalog[index, , drop = FALSE]
    required_start <- c(
      "schema_version", "canonical_wave_index", "wave_id", "mode",
      "phase", "batch_group", "batch_target", "binding_digest",
      "action", "required_predecessor_wave_ids",
      "predecessor_completion_sha256",
      "predecessor_artifact_manifest_sha256",
      "same_batch_sentinel_pass",
      "prior_batch_decision_sha256", "prior_batch_next_action",
      "worker_limit", "task_count", "wave_task_plan_sha256",
      "output_root", "started_at_utc"
    )
    required_completion <- c(
      "schema_version", "canonical_wave_index", "wave_id", "mode",
      "phase", "batch_group", "batch_target", "binding_digest",
      "action", "decision", "start_sha256",
      "required_predecessor_wave_ids",
      "predecessor_completion_sha256",
      "predecessor_artifact_manifest_sha256",
      "same_batch_sentinel_pass",
      "prior_batch_decision_sha256", "prior_batch_next_action",
      "worker_limit", "workers_used", "task_count",
      "wave_task_plan_sha256", "output_root",
      "wave_artifact_hashes_sha256", "all_workers_passed",
      "completed_at_utc"
    )
    scalar_equal <- function(observed, expected) {
      identical(as.character(observed), as.character(expected))
    }
    start_index <- rqr_confirm_strict_integer(
      start$canonical_wave_index,
      "wave start canonical index", 1L, nrow(catalog)
    )
    completion_index <- rqr_confirm_strict_integer(
      value$canonical_wave_index,
      "wave completion canonical index", 1L, nrow(catalog)
    )
    expected_predecessor_ids <- if (index > 1L) {
      catalog$wave_id[seq_len(index - 1L)]
    } else {
      character()
    }
    expected_predecessor_hashes <- if (index > 1L) {
      vapply(
        completion_paths[seq_len(index - 1L)],
        rqr_confirm_sha256, character(1L)
      )
    } else {
      character()
    }
    expected_predecessor_artifact_hashes <- if (index > 1L) {
      vapply(
        completion_paths[seq_len(index - 1L)],
        function(previous_path) {
          previous <- jsonlite::read_json(
            previous_path, simplifyVector = TRUE
          )
          as.character(previous$wave_artifact_hashes_sha256)
        },
        character(1L)
      )
    } else {
      character()
    }
    start_predecessor_ids <- as.character(unlist(
      start$required_predecessor_wave_ids, use.names = FALSE
    ))
    start_predecessor_hashes <- as.character(unlist(
      start$predecessor_completion_sha256, use.names = FALSE
    ))
    completion_predecessor_ids <- as.character(unlist(
      value$required_predecessor_wave_ids, use.names = FALSE
    ))
    completion_predecessor_hashes <- as.character(unlist(
      value$predecessor_completion_sha256, use.names = FALSE
    ))
    start_predecessor_artifact_hashes <- as.character(unlist(
      start$predecessor_artifact_manifest_sha256,
      use.names = FALSE
    ))
    completion_predecessor_artifact_hashes <- as.character(unlist(
      value$predecessor_artifact_manifest_sha256,
      use.names = FALSE
    ))
    if (!all(required_start %in% names(start)) ||
        !all(required_completion %in% names(value)) ||
        !identical(
          start$schema_version, "rqrgibbs_dlm_wave_start/1.0.0"
        ) ||
        !identical(
          value$schema_version,
          "rqrgibbs_dlm_wave_completion/1.0.0"
        ) ||
        !identical(start_index, as.integer(index)) ||
        !identical(completion_index, as.integer(index)) ||
        !scalar_equal(start$wave_id, catalog_row$wave_id) ||
        !scalar_equal(value$wave_id, catalog_row$wave_id) ||
        !scalar_equal(start$mode, catalog_row$mode) ||
        !scalar_equal(value$mode, catalog_row$mode) ||
        !scalar_equal(start$phase, catalog_row$phase) ||
        !scalar_equal(value$phase, catalog_row$phase) ||
        !scalar_equal(start$batch_group, catalog_row$batch_group) ||
        !scalar_equal(value$batch_group, catalog_row$batch_group) ||
        !identical(
          rqr_confirm_strict_integer(
            start$batch_target, "wave start batch target", 1L
          ),
          as.integer(catalog_row$batch_target)
        ) ||
        !identical(
          rqr_confirm_strict_integer(
            value$batch_target, "wave completion batch target", 1L
          ),
          as.integer(catalog_row$batch_target)
        ) ||
        !scalar_equal(start$binding_digest, binding$binding_digest) ||
        !scalar_equal(value$binding_digest, binding$binding_digest) ||
        !identical(
          rqr_confirm_strict_integer(
            start$worker_limit, "wave start worker limit", 1L
          ),
          as.integer(catalog_row$worker_limit)
        ) ||
        !identical(
          rqr_confirm_strict_integer(
            value$worker_limit, "wave completion worker limit", 1L
          ),
          as.integer(catalog_row$worker_limit)
        ) ||
        !identical(
          rqr_confirm_strict_integer(
            start$task_count, "wave start task count", 1L
          ),
          as.integer(catalog_row$task_count)
        ) ||
        !identical(
          rqr_confirm_strict_integer(
            value$task_count, "wave completion task count", 1L
          ),
          as.integer(catalog_row$task_count)
        ) ||
        !identical(
          start_predecessor_ids, expected_predecessor_ids
        ) ||
        !identical(
          completion_predecessor_ids, expected_predecessor_ids
        ) ||
        !identical(
          start_predecessor_hashes, expected_predecessor_hashes
        ) ||
        !identical(
          completion_predecessor_hashes,
          expected_predecessor_hashes
        ) ||
        !identical(
          start_predecessor_artifact_hashes,
          expected_predecessor_artifact_hashes
        ) ||
        !identical(
          completion_predecessor_artifact_hashes,
          expected_predecessor_artifact_hashes
        ) ||
        !identical(as.character(start$action),
                   as.character(value$action)) ||
        !as.character(start$action) %in% c("launch", "skip") ||
        !scalar_equal(
          start$wave_task_plan_sha256,
          value$wave_task_plan_sha256
        ) ||
        !grepl(
          "^[0-9a-f]{64}$",
          as.character(value$wave_task_plan_sha256)
        ) ||
        !scalar_equal(start$output_root, value$output_root) ||
        !scalar_equal(
          start$prior_batch_decision_sha256,
          value$prior_batch_decision_sha256
        ) ||
        !scalar_equal(
          start$prior_batch_next_action,
          value$prior_batch_next_action
        ) ||
        !identical(
          as.logical(start$same_batch_sentinel_pass),
          as.logical(value$same_batch_sentinel_pass)
        ) ||
        !grepl(
          "^[0-9]{4}-[0-9]{2}-[0-9]{2}T",
          as.character(start$started_at_utc)
        ) ||
        !grepl(
          "^[0-9]{4}-[0-9]{2}-[0-9]{2}T",
          as.character(value$completed_at_utc)
        )) {
      stop("A wave-state record violates its immutable schema.",
           call. = FALSE)
    }
    if (!identical(
        as.character(value$start_sha256),
        rqr_confirm_sha256(start_paths[[index]])
      )) {
      stop("A wave completion is not bound to its start record.",
           call. = FALSE)
    }
    if (identical(as.character(value$action), "skip")) {
      if (!identical(
          as.character(value$decision), "skipped_precision_stop"
        ) ||
          !identical(
            rqr_confirm_strict_integer(
              value$workers_used, "skipped wave workers used", 0L
            ),
            0L
          ) ||
          isTRUE(value$all_workers_passed) ||
          nzchar(as.character(value$wave_artifact_hashes_sha256))) {
        stop("A skipped wave has invalid terminal evidence.",
             call. = FALSE)
      }
    } else if (!as.character(value$decision) %in% c(
        "passed", "failed"
      ) ||
        rqr_confirm_strict_integer(
          value$workers_used, "launched wave workers used", 1L
        ) > as.integer(catalog_row$worker_limit) ||
        !grepl(
          "^[0-9a-f]{64}$",
          as.character(value$wave_artifact_hashes_sha256)
        ) ||
        !identical(
          isTRUE(value$all_workers_passed),
          identical(as.character(value$decision), "passed")
        )) {
      stop("A launched wave has invalid terminal evidence.",
           call. = FALSE)
    }
    has_prior_batch <- !is.na(catalog_row$prior_batch_target)
    prior_action <- as.character(value$prior_batch_next_action)
    prior_digest <- tolower(as.character(
      value$prior_batch_decision_sha256
    ))
    sentinel_pass_raw <- as.logical(value$same_batch_sentinel_pass)
    sentinel_pass <- if (length(sentinel_pass_raw) == 1L) {
      sentinel_pass_raw
    } else {
      NA
    }
    if (!has_prior_batch) {
      if (nzchar(prior_action) || nzchar(prior_digest)) {
        stop("An initial-batch wave asserted prior-batch evidence.",
             call. = FALSE)
      }
    } else if (!prior_action %in% c(
        "add_complete_paired_DGP_batch",
        "precision_pass_stop",
        "maximum_reached_report_unmet_precision"
      ) ||
        !grepl("^[0-9a-f]{64}$", prior_digest) ||
        !identical(
          as.character(value$action),
          if (prior_action == "add_complete_paired_DGP_batch") {
            "launch"
          } else {
            "skip"
          }
        )) {
      stop("A later wave has inconsistent prior-batch evidence.",
           call. = FALSE)
    }
    if (identical(as.character(value$action), "launch") &&
        identical(as.character(value$phase), "standard")) {
      if (!isTRUE(sentinel_pass)) {
        stop("A launched standard wave lacks its sentinel pass.",
             call. = FALSE)
      }
    } else if (!is.na(sentinel_pass)) {
      stop("A nonstandard or skipped wave asserted a sentinel pass.",
           call. = FALSE)
    }
    if (identical(as.character(value$action), "launch")) {
      artifact_path <- file.path(
        as.character(value$output_root),
        "wave_artifact_hashes.csv"
      )
      if (!file.exists(artifact_path) ||
          !identical(
            rqr_confirm_sha256(artifact_path),
            as.character(value$wave_artifact_hashes_sha256)
          )) {
        stop("A launched wave is detached from its artifact manifest.",
             call. = FALSE)
      }
    }
    value
  })
  completions <- lapply(
    seq_along(completion_values), function(index) {
      value <- completion_values[[index]]
      data.frame(
        canonical_wave_index = rqr_confirm_strict_integer(
          value$canonical_wave_index,
          "wave completion canonical index", 1L, nrow(catalog)
        ),
        wave_id = as.character(value$wave_id),
        binding_digest = as.character(value$binding_digest),
        decision = as.character(value$decision),
        completion_sha256 = rqr_confirm_sha256(
          completion_paths[[index]]
        ),
        artifact_manifest_sha256 =
          as.character(value$wave_artifact_hashes_sha256),
        stringsAsFactors = FALSE
      )
    }
  )
  completions <- if (length(completions)) {
    do.call(rbind, completions)
  } else {
    NULL
  }
  if (length(start_paths)) {
    expected_names <- sprintf(
      "%04d__%s.json",
      seq_along(start_paths),
      catalog$wave_id[seq_along(start_paths)]
    )
    if (!identical(basename(start_paths), expected_names) ||
        !identical(basename(completion_paths), expected_names)) {
      stop("Wave-state record names do not follow canonical order.",
           call. = FALSE)
    }
  }
  list(
    starts = starts, completion_values = completion_values,
    completions = completions
  )
}

rqr_confirm_read_prior_batch_decision <- function(
    path, current, binding, completion_values) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  if (!identical(basename(path), "batch_decisions.csv")) {
    stop("The prior batch decision must be the canonical artifact.",
         call. = FALSE)
  }
  directory <- dirname(path)
  rqr_confirm_verify_recursive_manifest(directory)
  run_manifest_path <- file.path(directory, "run_manifest.json")
  if (!file.exists(run_manifest_path)) {
    stop("The prior batch decision omitted its run manifest.",
         call. = FALSE)
  }
  run_manifest <- jsonlite::read_json(
    run_manifest_path, simplifyVector = TRUE
  )
  mode <- as.character(run_manifest$mode)
  detail_path <- file.path(directory, paste0(mode, "_manifest.json"))
  recursive_path <- file.path(
    directory, paste0(mode, "_recursive_manifest.csv")
  )
  if (!mode %in% c("collect", "audit") ||
      !file.exists(detail_path) || !file.exists(recursive_path) ||
      !identical(
        as.character(run_manifest$source_commit),
        binding$authorization_commit
      ) ||
      !identical(
        as.character(
          run_manifest$primary_runtime_binding$runtime_tree_digest
        ),
        binding$runtime_tree_digest
      )) {
    stop("The prior batch decision has the wrong source or runtime.",
         call. = FALSE)
  }
  detail <- jsonlite::read_json(detail_path, simplifyVector = TRUE)
  if (!isTRUE(detail$analysis_complete) ||
      !identical(
        as.character(detail$status),
        "integrity_and_analysis_complete"
      ) ||
      !identical(
        as.character(detail$source_commit),
        binding$authorization_commit
      )) {
    stop("The prior batch analysis was not completed successfully.",
         call. = FALSE)
  }
  recursive <- utils::read.csv(
    recursive_path, stringsAsFactors = FALSE, check.names = FALSE
  )
  if (!identical(names(recursive), c("path", "bytes", "sha256")) ||
      anyNA(recursive) ||
      any(!grepl("^[0-9a-f]{64}$", recursive$sha256))) {
    stop("The prior batch recursive evidence is invalid.",
         call. = FALSE)
  }
  completed_wave_hashes <- vapply(
    completion_values,
    function(value) as.character(value$wave_artifact_hashes_sha256),
    character(1L)
  )
  completed_wave_hashes <- completed_wave_hashes[
    nzchar(completed_wave_hashes)
  ]
  if (!length(completed_wave_hashes) ||
      !all(completed_wave_hashes %in% recursive$sha256)) {
    stop(
      "The prior batch decision does not cover every launched predecessor.",
      call. = FALSE
    )
  }
  decisions <- utils::read.csv(
    path, stringsAsFactors = FALSE, check.names = FALSE
  )
  if (!identical(
      names(decisions), rqr_confirm_artifact_schemas()$batch_decision
    )) {
    stop("The prior batch decision schema changed.", call. = FALSE)
  }
  replications <- vapply(
    decisions$replications,
    rqr_confirm_strict_integer, integer(1L),
    name = "batch decision replications", minimum = 1L
  )
  selected <- which(
    decisions$batch_group == current$batch_group &
      replications <= as.integer(current$prior_batch_target)
  )
  if (!length(selected)) {
    stop("The required prior batch decision is absent.",
         call. = FALSE)
  }
  selected_replications <- replications[selected]
  selected <- selected[
    selected_replications == max(selected_replications)
  ]
  if (length(selected) != 1L) {
    stop("The latest prior batch decision is duplicated.",
         call. = FALSE)
  }
  value <- decisions[selected, , drop = FALSE]
  value$binding_digest <- binding$binding_digest
  value$decision_sha256 <- rqr_confirm_sha256(path)
  value
}

rqr_confirm_diagnostic_training_times <- function(generated) {
  T <- rqr_confirm_strict_integer(generated$T, "generated$T", 1L)
  indices <- unique(as.integer(round(c(
    1, 0.25 * T, 0.50 * T, 0.75 * T, T
  ))))
  break_time <- generated$latent$break_time %||% integer()
  if (length(break_time)) {
    break_time <- rqr_confirm_strict_integer(
      break_time, "generated$latent$break_time", 1L, T
    )
    indices <- c(indices, break_time + (-1L:1L))
  }
  missing <- which(is.na(generated$training_y))
  if (length(missing)) {
    indices <- c(indices, as.integer(unlist(
      lapply(missing, function(index) index + (-1L:1L)),
      use.names = FALSE
    )))
  }
  scale <- as.numeric(generated$training_scale)
  if (length(scale) == T && all(is.finite(scale)) && T > 1L &&
      any(diff(scale) != 0)) {
    scale_boundary <- which.max(abs(diff(log(scale)))) + 1L
    indices <- c(indices, scale_boundary + (-1L:1L))
  }
  sort(unique(indices[indices >= 1L & indices <= T]), method = "radix")
}

rqr_confirm_interval_function_draws <- function(
    lower, upper, indices, labels) {
  lower <- as.matrix(lower)
  upper <- as.matrix(upper)
  if (!identical(dim(lower), dim(upper)) ||
      !nrow(lower) || !ncol(lower) ||
      any(!is.finite(lower)) || any(!is.finite(upper)) ||
      any(upper < lower)) {
    stop("Interval-function diagnostic draws are invalid.",
         call. = FALSE)
  }
  indices <- as.integer(indices)
  labels <- as.character(labels)
  if (!length(indices) || length(labels) != length(indices) ||
      anyNA(indices) || anyNA(labels) || any(!nzchar(labels)) ||
      any(indices < 1L) || any(indices > nrow(lower)) ||
      anyDuplicated(indices) || anyDuplicated(labels)) {
    stop("Diagnostic endpoint indices or labels are invalid.",
         call. = FALSE)
  }
  output <- matrix(
    NA_real_, nrow = ncol(lower), ncol = 4L * length(indices)
  )
  output_names <- character(ncol(output))
  column <- 0L
  for (position in seq_along(indices)) {
    index <- indices[[position]]
    values <- list(
      lower = lower[index, ],
      upper = upper[index, ],
      midpoint = 0.5 * (lower[index, ] + upper[index, ]),
      width = upper[index, ] - lower[index, ]
    )
    for (function_name in names(values)) {
      column <- column + 1L
      output[, column] <- values[[function_name]]
      output_names[[column]] <- paste(
        labels[[position]], function_name, sep = "_"
      )
    }
  }
  colnames(output) <- output_names
  output
}

rqr_confirm_conditional_root_draws <- function(
    terminal, FF_future, GG_future, horizon = NULL) {
  terminal <- as.matrix(terminal)
  FF_future <- as.matrix(FF_future)
  if (!nrow(terminal) || !ncol(terminal) ||
      nrow(FF_future) != nrow(terminal) ||
      !ncol(FF_future) ||
      any(!is.finite(terminal)) || any(!is.finite(FF_future))) {
    stop("Conditional-root diagnostic inputs are invalid.",
         call. = FALSE)
  }
  p <- nrow(terminal)
  GG_dimensions <- dim(GG_future)
  inferred_horizon <- max(c(
    ncol(FF_future),
    if (length(GG_dimensions) == 3L) GG_dimensions[[3L]] else 1L
  ))
  H <- if (is.null(horizon)) {
    inferred_horizon
  } else {
    rqr_confirm_strict_integer(
      horizon, "conditional-root horizon", 1L
    )
  }
  if (ncol(FF_future) == 1L && H > 1L) {
    FF_future <- matrix(
      rep(FF_future[, 1L], H), nrow = p, ncol = H
    )
  } else if (ncol(FF_future) != H) {
    stop("FF_future has the wrong diagnostic horizon.",
         call. = FALSE)
  }
  GG <- if (is.matrix(GG_future)) {
    if (!identical(dim(GG_future), c(p, p))) {
      stop("GG_future has the wrong diagnostic dimension.",
           call. = FALSE)
    }
    array(rep(GG_future, H), dim = c(p, p, H))
  } else {
    GG_future <- as.array(GG_future)
    if (identical(dim(GG_future), c(p, p, 1L)) && H > 1L) {
      array(rep(GG_future[, , 1L], H), dim = c(p, p, H))
    } else if (!identical(dim(GG_future), c(p, p, H))) {
      stop("GG_future has the wrong diagnostic dimension.",
           call. = FALSE)
    } else {
      GG_future
    }
  }
  if (any(!is.finite(GG))) {
    stop("GG_future contains nonfinite diagnostic values.",
         call. = FALSE)
  }
  state <- terminal
  output <- matrix(NA_real_, H, ncol(terminal))
  for (horizon in seq_len(H)) {
    state <- GG[, , horizon] %*% state
    output[horizon, ] <- drop(crossprod(
      FF_future[, horizon], state
    ))
  }
  output
}

rqr_confirm_diagnostic_schema <- function(
    method, generated, contract) {
  method <- as.character(method)[[1L]]
  mcmc_methods <- c(
    "M01", "M02", "M03", "M06", "M07",
    "M08", "M09", "M10", "M11"
  )
  if (!method %in% mcmc_methods) {
    stop("No MCMC diagnostic schema exists for this method.",
         call. = FALSE)
  }
  base <- c(
    "mean_lower", "mean_upper", "mean_midpoint",
    "mean_width", "observed_loss"
  )
  training <- unlist(lapply(
    rqr_confirm_diagnostic_training_times(generated),
    function(index) paste0(
      sprintf("training_t%04d", index), "_",
      c("lower", "upper", "midpoint", "width")
    )
  ), use.names = FALSE)
  horizons <- as.integer(contract$config$design$reported_horizons)
  if (!length(horizons) || anyNA(horizons) ||
      any(horizons < 1L) || any(horizons > generated$H) ||
      anyDuplicated(horizons)) {
    stop("Reported diagnostic horizons are invalid.", call. = FALSE)
  }
  future <- unlist(lapply(
    horizons,
    function(index) paste0(
      sprintf("future_h%02d", index), "_",
      c("lower", "upper", "midpoint", "width")
    )
  ), use.names = FALSE)
  dynamic <- if (method %in% c(
      "M01", "M02", "M06", "M07",
      "M08", "M09", "M10", "M11")) {
    paste0(
      "terminal_",
      c("lower", "upper", "midpoint", "width")
    )
  } else {
    character()
  }
  learned <- if (identical(method, "M11")) "log_lambda" else character()
  component_count <- if (method %in% c(
      "M01", "M09", "M10", "M11")) {
    length(rqr_confirm_model_bundle(generated)$training$component_dims)
  } else if (identical(method, "M07")) {
    1L
  } else {
    0L
  }
  component <- if (component_count) {
    paste0("log_q_", seq_len(component_count))
  } else {
    character()
  }
  c(base, training, dynamic, future, learned, component)
}

rqr_confirm_scalar_draws <- function(
    result, generated, contract, method) {
  training_times <- rqr_confirm_diagnostic_training_times(generated)
  reported_horizons <- as.integer(
    contract$config$design$reported_horizons
  )
  loss_draws <- function(lower, upper) {
    observed <- which(is.finite(generated$training_y))
    if (!length(observed)) {
      stop("MCMC diagnostics require observed training responses.",
           call. = FALSE)
    }
    vapply(seq_len(ncol(lower)), function(index) {
      sum(rqr_check_loss(
        rqr_residual_product(
          generated$training_y[observed],
          lower[observed, index], upper[observed, index]
        ),
        generated$coverage_level
      ))
    }, numeric(1L))
  }
  append_functions <- function(values, lower, upper, future_lower,
                               future_upper, dynamic = TRUE) {
    training <- rqr_confirm_interval_function_draws(
      lower, upper, training_times,
      sprintf("training_t%04d", training_times)
    )
    values <- cbind(values, training)
    if (isTRUE(dynamic)) {
      values <- cbind(
        values,
        rqr_confirm_interval_function_draws(
          lower, upper, nrow(lower), "terminal"
        )
      )
    }
    cbind(
      values,
      rqr_confirm_interval_function_draws(
        future_lower, future_upper, reported_horizons,
        sprintf("future_h%02d", reported_horizons)
      )
    )
  }
  finalize <- function(values, label) {
    expected <- rqr_confirm_diagnostic_schema(
      method, generated, contract
    )
    if (!identical(colnames(values), expected) ||
        any(!is.finite(values))) {
      stop(
        sprintf("%s diagnostic draws violate the exact schema.", label),
        call. = FALSE
      )
    }
    values[, expected, drop = FALSE]
  }
  if (!is.null(result$fit) &&
      inherits(result$fit, "rqr_dlm_mcmc")) {
    root1 <- result$fit$samp.eta_root1
    root2 <- result$fit$samp.eta_root2
    lower <- pmin(root1, root2)
    upper <- pmax(root1, root2)
    values <- cbind(
      mean_lower = colMeans(lower),
      mean_upper = colMeans(upper),
      mean_midpoint = colMeans(0.5 * (lower + upper)),
      mean_width = colMeans(upper - lower),
      observed_loss = loss_draws(lower, upper)
    )
    model_bundle <- rqr_confirm_model_bundle(generated)
    future_root1 <- rqr_confirm_conditional_root_draws(
      result$fit$samp.theta_terminal_root1,
      model_bundle$future$FF, model_bundle$future$GG,
      horizon = generated$H
    )
    future_root2 <- rqr_confirm_conditional_root_draws(
      result$fit$samp.theta_terminal_root2,
      model_bundle$future$FF, model_bundle$future$GG,
      horizon = generated$H
    )
    values <- append_functions(
      values, lower, upper,
      pmin(future_root1, future_root2),
      pmax(future_root1, future_root2)
    )
    if (identical(method, "M11")) {
      if (!is.numeric(result$fit$samp.lambda) ||
          length(result$fit$samp.lambda) != nrow(values) ||
          any(!is.finite(result$fit$samp.lambda)) ||
          any(result$fit$samp.lambda <= 0)) {
        stop("Dynamic RQR diagnostic draws violate the exact schema.",
             call. = FALSE)
      }
      values <- cbind(
        values, log_lambda = log(result$fit$samp.lambda)
      )
    }
    if (!is.null(result$fit$samp.evolution_scale)) {
      raw_scale <- as.matrix(result$fit$samp.evolution_scale)
      if (nrow(raw_scale) != nrow(values) ||
          any(!is.finite(raw_scale)) || any(raw_scale <= 0)) {
        stop("Dynamic RQR diagnostic draws violate the exact schema.",
             call. = FALSE)
      }
      scale <- log(raw_scale)
      colnames(scale) <- paste0(
        "log_q_", seq_len(ncol(scale))
      )
      values <- cbind(values, scale)
    }
    return(finalize(values, "Dynamic RQR"))
  }
  if (!is.null(result$fit) &&
      inherits(result$fit, "rqr_mcmc")) {
    X <- result$fit$X
    root1 <- X %*% t(result$fit$samp.beta_root1)
    root2 <- X %*% t(result$fit$samp.beta_root2)
    lower <- pmin(root1, root2)
    upper <- pmax(root1, root2)
    values <- cbind(
      mean_lower = colMeans(lower),
      mean_upper = colMeans(upper),
      mean_midpoint = colMeans(0.5 * (lower + upper)),
      mean_width = colMeans(upper - lower),
      observed_loss = loss_draws(lower, upper)
    )
    time_future <- (
      generated$T + seq_len(generated$H)
    ) / generated$T
    X_future <- cbind(
      intercept = 1, time = time_future,
      predictor = generated$future_predictor
    )
    future_root1 <- X_future %*% t(result$fit$samp.beta_root1)
    future_root2 <- X_future %*% t(result$fit$samp.beta_root2)
    values <- append_functions(
      values, lower, upper,
      pmin(future_root1, future_root2),
      pmax(future_root1, future_root2),
      dynamic = FALSE
    )
    return(finalize(values, "Fixed-design RQR"))
  }
  if (!is.null(result$fits) &&
      length(result$fits) == 2L &&
      all(vapply(
        result$fits, function(fit) inherits(fit, "exdqlmMCMC"),
        logical(1L)
      ))) {
    ordinate_draws <- function(fit) {
      state <- unclass(fit$samp.theta)
      dimensions <- dim(state)
      FF <- fit$model$FF
      output <- matrix(NA_real_, dimensions[[2L]], dimensions[[3L]])
      for (draw in seq_len(dimensions[[3L]])) {
        output[, draw] <- colSums(FF * state[, , draw])
      }
      output
    }
    raw_lower <- ordinate_draws(result$fits[[1L]])
    raw_upper <- ordinate_draws(result$fits[[2L]])
    lower <- pmin(raw_lower, raw_upper)
    upper <- pmax(raw_lower, raw_upper)
    values <- cbind(
      mean_lower = colMeans(lower),
      mean_upper = colMeans(upper),
      mean_midpoint = colMeans(0.5 * (lower + upper)),
      mean_width = colMeans(upper - lower),
      observed_loss = loss_draws(lower, upper)
    )
    terminal_draws <- function(fit) {
      state <- unclass(fit$samp.theta)
      dimensions <- dim(state)
      matrix(
        state[, dimensions[[2L]], ],
        nrow = dimensions[[1L]], ncol = dimensions[[3L]]
      )
    }
    model_bundle <- rqr_confirm_model_bundle(generated)
    future_root1 <- rqr_confirm_conditional_root_draws(
      terminal_draws(result$fits[[1L]]),
      model_bundle$future$FF, model_bundle$future$GG,
      horizon = generated$H
    )
    future_root2 <- rqr_confirm_conditional_root_draws(
      terminal_draws(result$fits[[2L]]),
      model_bundle$future$FF, model_bundle$future$GG,
      horizon = generated$H
    )
    values <- append_functions(
      values, lower, upper,
      pmin(future_root1, future_root2),
      pmax(future_root1, future_root2)
    )
    return(finalize(values, "Dynamic-quantile"))
  }
  NULL
}

rqr_confirm_chain_diagnostics <- function(
    scalar_chains, contract, sentinel, method, generated) {
  if (!length(scalar_chains) ||
      any(vapply(scalar_chains, is.null, logical(1L)))) {
    stop("MCMC diagnostics require retained scalar draws.",
         call. = FALSE)
  }
  required <- rqr_confirm_diagnostic_schema(
    method, generated, contract
  )
  schemas <- lapply(scalar_chains, colnames)
  if (any(!vapply(
      schemas, identical, logical(1L), required
    ))) {
    stop(
      "A diagnostic chain does not match the exact required estimand schema.",
      call. = FALSE
    )
  }
  lengths <- vapply(scalar_chains, nrow, integer(1L))
  if (length(unique(lengths)) != 1L) {
    stop("Diagnostic chains have unequal retained lengths.",
         call. = FALSE)
  }
  rows <- lapply(required, function(variable) {
    matrix_values <- do.call(cbind, lapply(
      scalar_chains, function(values) values[, variable]
    ))
    draws <- posterior::as_draws_array(array(
      as.numeric(matrix_values),
      dim = c(nrow(matrix_values), ncol(matrix_values), 1L),
      dimnames = list(
        iteration = NULL,
        chain = paste0("chain", seq_len(ncol(matrix_values))),
        variable = variable
      )
    ))
    standard_deviation <- stats::sd(as.numeric(matrix_values))
    data.frame(
      estimand = variable,
      chains = ncol(matrix_values),
      rhat = if (ncol(matrix_values) > 1L) {
        unname(posterior::rhat(draws))
      } else {
        NA_real_
      },
      ess_bulk = unname(posterior::ess_bulk(draws)),
      ess_tail = unname(posterior::ess_tail(draws)),
      mcse_mean = unname(posterior::mcse_mean(draws)),
      mcse_over_sd = if (
          is.finite(standard_deviation) &&
          standard_deviation > 0) {
        unname(posterior::mcse_mean(draws)) / standard_deviation
      } else {
        0
      },
      stringsAsFactors = FALSE
    )
  })
  diagnostics <- do.call(rbind, rows)
  if (isTRUE(sentinel)) {
    diagnostics$pass <- with(
      diagnostics,
      is.finite(rhat) &
        rhat <= contract$config$diagnostics$sentinel_rhat_max &
        ess_bulk >= contract$config$diagnostics$sentinel_bulk_ess_min &
        ess_tail >= contract$config$diagnostics$sentinel_tail_ess_min
    )
  } else {
    diagnostics$pass <- with(
      diagnostics,
      ess_bulk >= contract$config$diagnostics$single_bulk_ess_min &
        ess_tail >= contract$config$diagnostics$single_tail_ess_min &
        mcse_over_sd <= contract$config$diagnostics$single_mcse_sd_max
    )
  }
  diagnostics
}

rqr_confirm_mcse <- function(values) {
  values <- as.numeric(values)
  if (length(values) < 2L || any(!is.finite(values))) {
    stop("MCSE requires at least two finite replication-level values.",
         call. = FALSE)
  }
  stats::sd(values) / sqrt(length(values))
}

rqr_confirm_coverage_qualification <- function(
    successes, total, nominal, margin = 0.02,
    confidence_level = 0.90) {
  successes <- rqr_confirm_strict_integer(successes, "successes", 0L)
  total <- rqr_confirm_strict_integer(total, "total", 1L)
  if (successes > total || !is.numeric(nominal) || length(nominal) != 1L ||
      !is.finite(nominal) || nominal <= 0 || nominal >= 1) {
    stop("Coverage-qualification inputs are invalid.", call. = FALSE)
  }
  estimate <- successes / total
  standard_error <- sqrt(estimate * (1 - estimate) / total)
  critical <- stats::qt(
    1 - (1 - confidence_level) / 2, df = total - 1L
  )
  error_interval <- estimate - nominal +
    c(-1, 1) * critical * standard_error
  list(
    estimate = estimate, standard_error = standard_error,
    lower_error = error_interval[[1L]],
    upper_error = error_interval[[2L]],
    qualified = error_interval[[1L]] > -margin &&
      error_interval[[2L]] < margin,
    stopping_rule = FALSE
  )
}

rqr_confirm_coverage_qualification_values <- function(
    replication_coverage, nominal, margin = 0.02,
    confidence_level = 0.90) {
  replication_coverage <- as.numeric(replication_coverage)
  if (length(replication_coverage) < 2L ||
      any(!is.finite(replication_coverage)) ||
      any(replication_coverage < 0 | replication_coverage > 1) ||
      !is.numeric(nominal) || length(nominal) != 1L ||
      !is.finite(nominal) || nominal <= 0 || nominal >= 1 ||
      !is.finite(margin) || margin <= 0 ||
      !is.finite(confidence_level) ||
      confidence_level <= 0 || confidence_level >= 1) {
    stop("Replication-level coverage inputs are invalid.",
         call. = FALSE)
  }
  estimate <- mean(replication_coverage)
  standard_error <- rqr_confirm_mcse(replication_coverage)
  critical <- stats::qt(
    1 - (1 - confidence_level) / 2,
    df = length(replication_coverage) - 1L
  )
  error_interval <- estimate - nominal +
    c(-1, 1) * critical * standard_error
  list(
    estimate = estimate,
    standard_error = standard_error,
    lower_error = error_interval[[1L]],
    upper_error = error_interval[[2L]],
    qualified = error_interval[[1L]] > -margin &&
      error_interval[[2L]] < margin,
    stopping_rule = FALSE,
    independent_unit = "training_DGP_replication"
  )
}

rqr_confirm_summarize_results <- function(results, contract) {
  required <- rqr_confirm_artifact_schemas()$replication_results
  if (!is.data.frame(results) || !all(required %in% names(results)) ||
      anyDuplicated(results[c("cell_id", "replication")])) {
    stop("Compact replication results are incomplete or duplicated.",
         call. = FALSE)
  }
  completed <- results[results$status == "completed", , drop = FALSE]
  cells <- sort(unique(results$cell_id), method = "radix")
  summary_rows <- precision_rows <- qualification_rows <- list()
  for (index in seq_along(cells)) {
    cell <- cells[[index]]
    values <- completed[completed$cell_id == cell, , drop = FALSE]
    incidence <- contract$incidence[
      contract$incidence$cell_id == cell, , drop = FALSE
    ]
    planned <- results[results$cell_id == cell, , drop = FALSE]
    if (nrow(incidence) != 1L) {
      stop("A compact result has no unique incidence row.",
           call. = FALSE)
    }
    measures <- c(
      "heldout_rqr_loss", "aggregate_coverage", "mean_width",
      "future_mean_lower", "future_mean_upper",
      "future_mean_midpoint",
      "central_interval_score", "endpoint_rmse_lower",
      "endpoint_rmse_upper", "cross_target_distance",
      "realized_root_rmse",
      sprintf("coverage_h%02d", c(1L, 5L, 10L, 20L))
    )
    for (measure in measures) {
      finite <- values[[measure]][is.finite(values[[measure]])]
      summary_rows[[length(summary_rows) + 1L]] <- data.frame(
        cell_id = cell, DGP = incidence$DGP,
        method = incidence$method, measure = measure,
        planned_denominator = nrow(planned),
        completed = length(finite),
        failed = sum(planned$status != "completed"),
        mean = if (length(finite)) mean(finite) else NA_real_,
        mcse = if (length(finite) >= 2L) {
          rqr_confirm_mcse(finite)
        } else {
          NA_real_
        },
        stringsAsFactors = FALSE
      )
    }
    failure_indicator <- planned$status != "completed"
    summary_rows[[length(summary_rows) + 1L]] <- data.frame(
      cell_id = cell, DGP = incidence$DGP,
      method = incidence$method, measure = "fit_failure_probability",
      planned_denominator = nrow(planned),
      completed = sum(!failure_indicator),
      failed = sum(failure_indicator),
      mean = mean(failure_indicator),
      mcse = if (length(failure_indicator) >= 2L) {
        rqr_confirm_mcse(as.numeric(failure_indicator))
      } else {
        NA_real_
      },
      stringsAsFactors = FALSE
    )
    if (nrow(values) >= 2L) {
      nominal <- contract$config$scenarios[[incidence$DGP]]$coverage
      qualification <-
        rqr_confirm_coverage_qualification_values(
          values$aggregate_coverage, nominal,
          margin = contract$config$analysis$
            coverage_equivalence_margin,
          confidence_level = contract$config$analysis$
            coverage_equivalence_confidence
        )
      qualification_rows[[length(qualification_rows) + 1L]] <-
        data.frame(
          cell_id = cell, DGP = incidence$DGP,
          method = incidence$method, nominal = nominal,
          estimate = qualification$estimate,
          mcse = qualification$standard_error,
          lower_error = qualification$lower_error,
          upper_error = qualification$upper_error,
          qualified = qualification$qualified,
          used_for_stopping = FALSE,
          stringsAsFactors = FALSE
        )
      sensitivity <- incidence$replication_rule == "S"
      aggregate_threshold <- if (sensitivity) {
        contract$config$precision$sensitivity_aggregate_coverage_mcse
      } else {
        contract$config$precision$core_aggregate_coverage_mcse
      }
      horizon_threshold <- if (sensitivity) {
        contract$config$precision$sensitivity_horizon_coverage_mcse
      } else {
        contract$config$precision$core_horizon_coverage_mcse
      }
      horizon_mcse <- vapply(
        sprintf("coverage_h%02d", c(1L, 5L, 10L, 20L)),
        function(name) rqr_confirm_mcse(values[[name]]),
        numeric(1L)
      )
      precision_rows[[length(precision_rows) + 1L]] <- data.frame(
        cell_id = cell, DGP = incidence$DGP,
        method = incidence$method,
        aggregate_coverage_mcse =
          rqr_confirm_mcse(values$aggregate_coverage),
        aggregate_threshold = aggregate_threshold,
        maximum_reported_horizon_mcse = max(horizon_mcse),
        horizon_threshold = horizon_threshold,
        endpoint_lower_normalized_mcse =
          rqr_confirm_mcse(values$future_mean_lower) /
            mean(values$training_response_sd),
        endpoint_upper_normalized_mcse =
          rqr_confirm_mcse(values$future_mean_upper) /
            mean(values$training_response_sd),
        midpoint_normalized_mcse =
          rqr_confirm_mcse(values$future_mean_midpoint) /
            mean(values$training_response_sd),
        width_normalized_mcse =
          rqr_confirm_mcse(values$mean_width) /
            mean(values$mean_oracle_width),
        performance_sign_used = FALSE,
        TOST_used = FALSE,
        stringsAsFactors = FALSE
      )
    }
  }
  summary <- do.call(rbind, summary_rows)
  precision <- if (length(precision_rows)) {
    do.call(rbind, precision_rows)
  } else {
    data.frame()
  }
  qualification <- if (length(qualification_rows)) {
    do.call(rbind, qualification_rows)
  } else {
    data.frame()
  }
  list(
    summary = summary, precision = precision,
    coverage_qualification = qualification
  )
}

rqr_confirm_contrast_pairs <- function() {
  list(
    M01_vs_M02 = list(methods = c("M01", "M02"), role = "primary"),
    M01_vs_M05 = list(methods = c("M01", "M05"), role = "primary"),
    M01_vs_M03 = list(methods = c("M01", "M03"), role = "primary"),
    M01_vs_M06 = list(methods = c("M01", "M06"), role = "primary"),
    M01_vs_M07 = list(methods = c("M01", "M07"), role = "primary"),
    M03_vs_M04 = list(
      methods = c("M03", "M04"), role = "secondary_static"
    )
  )
}

rqr_confirm_paired_contrasts <- function(results, contract) {
  pairs <- rqr_confirm_contrast_pairs()
  measures <- c(
    "heldout_rqr_loss", "aggregate_coverage", "mean_width",
    "future_mean_lower", "future_mean_upper",
    "future_mean_midpoint", "endpoint_rmse_lower",
    "endpoint_rmse_upper"
  )
  rows <- list()
  index <- 0L
  for (scenario in names(contract$config$scenarios)) {
    scenario_rows <- contract$incidence[
      contract$incidence$DGP == scenario &
        rqr_confirm_included(contract$incidence),
      ,
      drop = FALSE
    ]
    for (pair_name in names(pairs)) {
      pair_spec <- pairs[[pair_name]]
      pair <- pair_spec$methods
      if (!all(pair %in% scenario_rows$method)) next
      left_cell <- scenario_rows$cell_id[
        scenario_rows$method == pair[[1L]]
      ]
      right_cell <- scenario_rows$cell_id[
        scenario_rows$method == pair[[2L]]
      ]
      left <- results[
        results$cell_id == left_cell & results$status == "completed",
        ,
        drop = FALSE
      ]
      right <- results[
        results$cell_id == right_cell & results$status == "completed",
        ,
        drop = FALSE
      ]
      matched <- merge(
        left, right, by = "replication", suffixes = c("_left", "_right")
      )
      if (nrow(matched) < 2L) next
      nominal <- contract$config$scenarios[[scenario]]$coverage
      left_coverage <- rqr_confirm_coverage_qualification_values(
        matched$aggregate_coverage_left, nominal,
        margin = contract$config$analysis$coverage_equivalence_margin,
        confidence_level =
          contract$config$analysis$coverage_equivalence_confidence
      )
      right_coverage <- rqr_confirm_coverage_qualification_values(
        matched$aggregate_coverage_right, nominal,
        margin = contract$config$analysis$coverage_equivalence_margin,
        confidence_level =
          contract$config$analysis$coverage_equivalence_confidence
      )
      for (measure in measures) {
        difference <- matched[[paste0(measure, "_left")]] -
          matched[[paste0(measure, "_right")]]
        if (any(!is.finite(difference))) next
        standard_error <- rqr_confirm_mcse(difference)
        scale <- if (measure == "heldout_rqr_loss") {
          mean(matched$training_response_sd_left^2)
        } else if (measure == "mean_width") {
          mean(matched$mean_oracle_width_left)
        } else if (measure %in% c(
            "future_mean_lower", "future_mean_upper",
            "future_mean_midpoint")) {
          mean(matched$training_response_sd_left)
        } else {
          NA_real_
        }
        critical <- stats::qt(0.975, df = length(difference) - 1L)
        lower_95 <- mean(difference) - critical * standard_error
        upper_95 <- mean(difference) + critical * standard_error
        width_claim_allowed <- measure == "mean_width" &&
          left_coverage$qualified && right_coverage$qualified &&
          (upper_95 < 0 || lower_95 > 0)
        width_claim_direction <- if (!width_claim_allowed) {
          "none"
        } else if (upper_95 < 0) {
          "left_narrower"
        } else {
          "right_narrower"
        }
        index <- index + 1L
        rows[[index]] <- data.frame(
          DGP = scenario, contrast = pair_name,
          contrast_role = pair_spec$role,
          left_method = pair[[1L]], right_method = pair[[2L]],
          measure = measure, paired_replications = length(difference),
          estimate = mean(difference), mcse = standard_error,
          precision_scale = scale,
          normalized_mcse = if (
              is.finite(scale) && scale > 0) {
            standard_error / scale
          } else {
            NA_real_
          },
          lower_95 = lower_95, upper_95 = upper_95,
          left_coverage_qualified = left_coverage$qualified,
          right_coverage_qualified = right_coverage$qualified,
          width_claim_allowed = width_claim_allowed,
          width_claim_direction = width_claim_direction,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  if (!length(rows)) return(data.frame())
  output <- do.call(rbind, rows)
  output$directional_p_two_sided <- vapply(
    seq_len(nrow(output)), function(index) {
    estimate <- output$estimate[[index]]
    standard_error <- output$mcse[[index]]
    if (!is.finite(standard_error) || standard_error <= 0) return(NA_real_)
    degrees <- output$paired_replications[[index]] - 1L
    2 * stats::pt(
      -abs(estimate / standard_error), df = degrees
    )
  }, numeric(1L))
  output$holm_directional_p <- ave(
    output$directional_p_two_sided,
    interaction(output$DGP, output$measure, drop = TRUE),
    FUN = function(value) stats::p.adjust(value, method = "holm")
  )
  output$width_claim_allowed <-
    output$measure == "mean_width" &
    output$left_coverage_qualified &
    output$right_coverage_qualified &
    is.finite(output$holm_directional_p) &
    output$holm_directional_p < 0.05
  output$width_claim_direction <- ifelse(
    !output$width_claim_allowed,
    "none",
    ifelse(output$estimate < 0, "left_narrower", "right_narrower")
  )
  output
}

rqr_confirm_batch_decisions <- function(
    results, precision, contrasts, contract) {
  rows <- list()
  observed_scenarios <- unique(contract$incidence$DGP[
    contract$incidence$cell_id %in% unique(results$cell_id)
  ])
  batch_groups <- unique(vapply(
    contract$config$scenarios[observed_scenarios],
    `[[`, character(1L), "batch_group"
  ))
  primary_pairs <- rqr_confirm_contrast_pairs()
  primary_pairs <- primary_pairs[
    vapply(
      primary_pairs,
      function(value) identical(value$role, "primary"),
      logical(1L)
    )
  ]
  precision_measures <- c(
    "heldout_rqr_loss", "mean_width", "future_mean_lower",
    "future_mean_upper", "future_mean_midpoint"
  )
  for (batch_group in batch_groups) {
    scenarios <- names(contract$config$scenarios)[vapply(
      contract$config$scenarios,
      function(value) identical(value$batch_group, batch_group),
      logical(1L)
    )]
    incidence <- contract$incidence[
      contract$incidence$DGP %in% scenarios &
        contract$incidence$include_or_omit_reason == "i",
      ,
      drop = FALSE
    ]
    relevant <- precision[
      precision$cell_id %in% incidence$cell_id, , drop = FALSE
    ]
    rule <- unique(incidence$replication_rule)
    rule <- rule[rule %in% c("C", "S")]
    if (length(rule) != 1L || !nrow(relevant)) next
    contract_batch <- if (rule == "C") {
      contract$config$batching$core
    } else {
      contract$config$batching$sensitivity
    }
    cell_replications <- lapply(incidence$cell_id, function(cell_id) {
      sort(
        unique(results$replication[results$cell_id == cell_id]),
        method = "radix"
      )
    })
    names(cell_replications) <- incidence$cell_id
    cell_counts <- vapply(cell_replications, length, integer(1L))
    cell_contiguous <- vapply(
      cell_replications,
      function(value) {
        length(value) > 0L &&
          identical(as.integer(value), seq_len(max(value)))
      },
      logical(1L)
    )
    scenario_replications <- vapply(scenarios, function(scenario) {
      scenario_cells <- incidence$cell_id[incidence$DGP == scenario]
      counts <- unname(cell_counts[scenario_cells])
      if (length(counts) && length(unique(counts)) == 1L) {
        counts[[1L]]
      } else {
        0
      }
    }, numeric(1L))
    replications <- min(scenario_replications)
    cell_pass <- nrow(relevant) == nrow(incidence) && all(with(
      relevant,
      aggregate_coverage_mcse <= aggregate_threshold &
        maximum_reported_horizon_mcse <= horizon_threshold &
        endpoint_lower_normalized_mcse <=
          contract$config$precision$
            endpoint_midpoint_training_sd_fraction &
        endpoint_upper_normalized_mcse <=
          contract$config$precision$
            endpoint_midpoint_training_sd_fraction &
        midpoint_normalized_mcse <=
          contract$config$precision$
            endpoint_midpoint_training_sd_fraction &
        width_normalized_mcse <=
          contract$config$precision$width_oracle_width_fraction
    ))
    contrast_relevant <- contrasts[
      contrasts$DGP %in% scenarios &
        contrasts$contrast_role == "primary" &
        contrasts$measure %in% precision_measures,
      ,
      drop = FALSE
    ]
    expected_contrast_rows <- sum(vapply(scenarios, function(scenario) {
      methods <- incidence$method[incidence$DGP == scenario]
      sum(vapply(primary_pairs, function(pair) {
        all(pair$methods %in% methods)
      }, logical(1L))) * length(precision_measures)
    }, numeric(1L)))
    contrast_pass <- nrow(contrast_relevant) == expected_contrast_rows &&
      expected_contrast_rows > 0L
    if (contrast_pass) {
      thresholds <- ifelse(
        contrast_relevant$measure == "heldout_rqr_loss",
        contract$config$precision$standardized_rqr_loss_contrast_mcse,
        ifelse(
          contrast_relevant$measure == "mean_width",
          contract$config$precision$width_oracle_width_fraction,
          contract$config$precision$
            endpoint_midpoint_training_sd_fraction
        )
      )
      contrast_pass <- all(
        is.finite(contrast_relevant$normalized_mcse) &
          contrast_relevant$normalized_mcse <= thresholds
      )
    }
    relevant_results <- results[
      results$cell_id %in% incidence$cell_id, , drop = FALSE
    ]
    no_fit_failures <- nrow(relevant_results) > 0L &&
      all(relevant_results$status == "completed")
    replications_on_boundary <- function(value) {
      value >= contract_batch$initial &&
        value <= contract_batch$maximum &&
        (value == contract_batch$initial ||
          (value - contract_batch$initial) %% contract_batch$increment == 0)
    }
    paired_batch_complete <-
      all(cell_contiguous) &&
      length(unique(cell_counts)) == 1L &&
      length(unique(scenario_replications)) == 1L &&
      all(vapply(
        scenario_replications, replications_on_boundary, logical(1L)
      ))
    overall_pass <- cell_pass && contrast_pass &&
      no_fit_failures && paired_batch_complete
    next_action <- if (overall_pass) {
      "precision_pass_stop"
    } else if (min(scenario_replications) >= contract_batch$maximum) {
      "maximum_reached_report_unmet_precision"
    } else {
      "add_complete_paired_DGP_batch"
    }
    rows[[length(rows) + 1L]] <- data.frame(
      batch_group = batch_group,
      DGP = paste(scenarios, collapse = "|"),
      replication_rule = rule,
      replications = replications,
      precision_pass = overall_pass,
      cell_mean_precision_pass = cell_pass,
      paired_contrast_precision_pass = contrast_pass,
      no_fit_failures = no_fit_failures,
      paired_batch_complete = paired_batch_complete,
      performance_sign_used = FALSE, TOST_used = FALSE,
      next_action = next_action,
      next_replications = if (
          next_action == "add_complete_paired_DGP_batch") {
        min(
          replications + contract_batch$increment,
          contract_batch$maximum
        )
      } else {
        replications
      },
      stringsAsFactors = FALSE
    )
  }
  if (!length(rows)) return(data.frame())
  do.call(rbind, rows)
}

rqr_confirm_flag_only_authorization_diff <- function(
    repo_root, implementation_commit, authorization_commit) {
  commits <- tolower(c(implementation_commit, authorization_commit))
  if (any(!grepl("^[0-9a-f]{40}$", commits))) return(FALSE)
  git <- Sys.which("git")
  if (!nzchar(git)) return(FALSE)
  run_git <- function(arguments) {
    output <- suppressWarnings(system2(
      git,
      c("-C", shQuote(repo_root), arguments),
      stdout = TRUE, stderr = TRUE,
      env = c("GIT_OPTIONAL_LOCKS=0", "GIT_TERMINAL_PROMPT=0")
    ))
    status <- attr(output, "status")
    if (is.null(status)) status <- 0L
    if (!identical(as.integer(status), 0L)) return(NULL)
    output
  }
  changed <- run_git(c(
    "diff", "--name-only", paste0(commits[[1L]], "..", commits[[2L]])
  ))
  expected_path <- paste(
    "application", "config", "rqr_dlm",
    "rqr_dlm_main_simulation_20260724.R", sep = "/"
  )
  if (is.null(changed) || !identical(changed, expected_path)) return(FALSE)
  patch <- run_git(c(
    "diff", "--unified=0", "--no-ext-diff",
    paste0(commits[[1L]], "..", commits[[2L]]), "--", expected_path
  ))
  if (is.null(patch)) return(FALSE)
  changed_lines <- patch[
    grepl("^[+-]", patch) &
      !grepl("^(---|\\+\\+\\+)", patch)
  ]
  identical(
    changed_lines,
    c(
      "-  confirmatory_execution_authorized = FALSE,",
      "+  confirmatory_execution_authorized = TRUE,"
    )
  )
}

rqr_confirm_authorized <- function(
    contract, mode, expected_commit, authorization_bundle = NULL,
    observed = list()) {
  if (!mode %in% c("sentinel-core", "execute-confirmatory")) return(TRUE)
  if (!isTRUE(contract$config$confirmatory_execution_authorized)) {
    stop(
      "Confirmatory execution is disabled in the reviewed configuration.",
      call. = FALSE
    )
  }
  expected_commit <- tolower(as.character(expected_commit)[1L])
  required_bundle <- c(
    "schema_version", "reviewed_implementation_commit",
    "authorization_commit", "authorization_diff_only_flag",
    "explicit_user_confirmation", "all_reference_gates_pass",
    "primary_worktree_clean", "primary_runtime_tree_digest",
    "preflight_artifact_hashes_sha256",
    "reference_artifact_hashes_sha256", "seed_ledger_sha256",
    "task_plan_sha256", "exdqlm_source_sha256",
    "quantreg_source_sha256", "reference_runtime_bundle_match",
    "comparator_dependency_runtime_match", "toolchain_match",
    "protected_checkout_used"
  )
  required_observed <- c(
    "reviewed_implementation_commit", "authorization_diff_only_flag",
    "primary_worktree_clean", "primary_runtime_tree_digest",
    "preflight_artifact_hashes_sha256",
    "reference_artifact_hashes_sha256", "seed_ledger_sha256",
    "task_plan_sha256", "exdqlm_source_sha256",
    "quantreg_source_sha256", "reference_runtime_bundle_match",
    "comparator_dependency_runtime_match", "toolchain_match",
    "protected_checkout_used"
  )
  valid_sha256 <- function(value) {
    is.character(value) && length(value) == 1L &&
      grepl("^[0-9a-f]{64}$", tolower(value))
  }
  valid_commit <- function(value) {
    is.character(value) && length(value) == 1L &&
      grepl("^[0-9a-f]{40}$", tolower(value))
  }
  if (!grepl("^[0-9a-f]{40}$", expected_commit) ||
      is.null(authorization_bundle) ||
      !all(required_bundle %in% names(authorization_bundle)) ||
      !all(required_observed %in% names(observed)) ||
      !identical(
        authorization_bundle$schema_version,
        "rqrgibbs_dlm_confirmatory_authorization/1.0.0"
      ) ||
      !valid_commit(authorization_bundle$reviewed_implementation_commit) ||
      !identical(
        tolower(authorization_bundle$authorization_commit),
        expected_commit
      ) ||
      !isTRUE(authorization_bundle$authorization_diff_only_flag) ||
      !isTRUE(authorization_bundle$explicit_user_confirmation) ||
      !isTRUE(authorization_bundle$all_reference_gates_pass) ||
      !isTRUE(authorization_bundle$primary_worktree_clean) ||
      !isTRUE(authorization_bundle$reference_runtime_bundle_match) ||
      !isTRUE(authorization_bundle$comparator_dependency_runtime_match) ||
      !isTRUE(authorization_bundle$toolchain_match) ||
      isTRUE(authorization_bundle$protected_checkout_used) ||
      !all(vapply(
        authorization_bundle[c(
          "primary_runtime_tree_digest",
          "preflight_artifact_hashes_sha256",
          "reference_artifact_hashes_sha256", "seed_ledger_sha256",
          "task_plan_sha256", "exdqlm_source_sha256",
          "quantreg_source_sha256"
        )],
        valid_sha256, logical(1L)
      )) ||
      !isTRUE(observed$primary_worktree_clean) ||
      !isTRUE(observed$authorization_diff_only_flag) ||
      !isTRUE(observed$reference_runtime_bundle_match) ||
      !isTRUE(observed$comparator_dependency_runtime_match) ||
      !isTRUE(observed$toolchain_match) ||
      isTRUE(observed$protected_checkout_used)) {
    stop("The commit-bound confirmatory authorization is incomplete.",
         call. = FALSE)
  }
  comparison_fields <- setdiff(
    required_observed,
    c(
      "primary_worktree_clean", "reference_runtime_bundle_match",
      "comparator_dependency_runtime_match", "toolchain_match",
      "protected_checkout_used"
    )
  )
  for (field in comparison_fields) {
    if (!identical(
        tolower(as.character(authorization_bundle[[field]])),
        tolower(as.character(observed[[field]]))
      )) {
      stop(
        sprintf("Authorization artifact binding failed for %s.", field),
        call. = FALSE
      )
    }
  }
  if (!identical(
      tolower(authorization_bundle$exdqlm_source_sha256),
      tolower(contract$config$comparator$exdqlm$source_sha256)
    ) ||
      !identical(
        tolower(authorization_bundle$quantreg_source_sha256),
        tolower(contract$config$comparator$quantreg$source_sha256)
      )) {
    stop("Authorization comparator source hashes changed.",
         call. = FALSE)
  }
  TRUE
}
