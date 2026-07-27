# Integrity and continuation helpers for ordinary-RQR fixed-design chains.

.rqr_static_provenance_objects <- function(target, prior) {
  objects <- list(
    target = target,
    beta_prior = .rqr_prior_target_contract(prior)
  )
  if (!is.null(target$embedding_contract)) {
    objects$embedding <- target$embedding_contract
  }
  objects
}

.rqr_static_checkpoint <- function(
    state, completed_iterations, data, target_digest, prior_digest) {
  completed_iterations <- .rqr_history_count(
    completed_iterations, "completed_iterations"
  )
  checkpoint <- list(
    schema_version = .rqr_static_checkpoint_schema(),
    fit_schema_version = .rqr_static_fit_schema(),
    transition_version = .rqr_static_transition_version(),
    completed_iterations = completed_iterations,
    data_contract_digest = .rqr_digest(unclass(data)),
    data_digest = data$data_digest,
    design_digest = data$design_digest,
    target_digest = target_digest,
    prior_digest = prior_digest,
    beta_root1 = as.numeric(state$beta_root1),
    beta_root2 = as.numeric(state$beta_root2),
    lambda = as.numeric(state$lambda)[1L],
    latent_v = as.numeric(state$latent_v),
    beta_prior_state1 = state$beta_prior_state1,
    beta_prior_state2 = state$beta_prior_state2,
    rng_state = .rqr_rng_state()
  )
  checkpoint
}

.rqr_static_schedule_prefix <- function(segments) {
  list(
    schema_version = .rqr_static_schedule_schema(),
    fit_schema_version = .rqr_static_fit_schema(),
    transition_version = .rqr_static_transition_version(),
    generation = as.integer(length(segments) - 1L),
    segments = segments
  )
}

.rqr_validate_static_schedule_value <- function(
    contract, stored_digest = NULL) {
  required_contract_fields <- c(
    "schema_version", "fit_schema_version", "transition_version",
    "generation", "segments"
  )
  required_segment_fields <- c(
    "generation", "start_completed_iterations", "n_burn",
    "n_retained_draws", "thin", "raw_iterations",
    "end_completed_iterations", "ends_on_retained_draw",
    "parent_checkpoint_digest", "parent_schedule_digest",
    "checkpoint_digest"
  )
  if (!is.list(contract) ||
      !identical(names(contract), required_contract_fields) ||
      !identical(
        contract$schema_version, .rqr_static_schedule_schema()
      ) ||
      !identical(
        contract$fit_schema_version, .rqr_static_fit_schema()
      ) ||
      !identical(
        contract$transition_version, .rqr_static_transition_version()
      ) ||
      !is.list(contract$segments) || !length(contract$segments)) {
    stop("The static segment-schedule contract is unsupported.",
         call. = FALSE)
  }
  if (!is.null(stored_digest)) {
    if (!is.character(stored_digest) ||
        length(stored_digest) != 1L ||
        is.na(stored_digest) ||
        !grepl("^[0-9a-f]{64}$", stored_digest) ||
        !identical(.rqr_digest(contract), stored_digest)) {
      stop("The static segment-schedule digest is invalid.",
           call. = FALSE)
    }
  }

  generation <- tryCatch(
    .rqr_history_count(
      contract$generation, "segment_schedule_contract$generation"
    ),
    error = function(error) NA_integer_
  )
  if (is.na(generation) ||
      !identical(generation, as.integer(length(contract$segments) - 1L))) {
    stop("The static segment-schedule generation is invalid.",
         call. = FALSE)
  }

  previous_end <- 0L
  previous_checkpoint <- NA_character_
  for (index in seq_along(contract$segments)) {
    segment <- contract$segments[[index]]
    if (!is.list(segment) ||
        !identical(names(segment), required_segment_fields)) {
      stop(
        sprintf(
          "Static segment schedule %d is incomplete.",
          as.integer(index - 1L)
        ),
        call. = FALSE
      )
    }
    count <- function(field, minimum = 0L) {
      tryCatch(
        .rqr_scalar_integer(
          segment[[field]],
          sprintf("segment_schedule$%s", field),
          minimum = minimum
        ),
        error = function(error) NA_integer_
      )
    }
    segment_generation <- count("generation")
    start <- count("start_completed_iterations")
    burn <- count("n_burn")
    retained <- count("n_retained_draws", minimum = 1L)
    thin <- count("thin", minimum = 1L)
    raw <- count("raw_iterations", minimum = 1L)
    end <- count("end_completed_iterations", minimum = 1L)
    expected_raw_double <-
      as.double(burn) + as.double(retained) * as.double(thin)
    expected_end_double <- as.double(start) + expected_raw_double
    valid_arithmetic <-
      is.finite(expected_raw_double) &&
      is.finite(expected_end_double) &&
      expected_raw_double <= .Machine$integer.max &&
      expected_end_double <= .Machine$integer.max &&
      identical(raw, as.integer(expected_raw_double)) &&
      identical(end, as.integer(expected_end_double))
    valid_checkpoint <-
      is.character(segment$checkpoint_digest) &&
      length(segment$checkpoint_digest) == 1L &&
      !is.na(segment$checkpoint_digest) &&
      grepl("^[0-9a-f]{64}$", segment$checkpoint_digest)
    valid_first_links <- if (index == 1L) {
      is.character(segment$parent_checkpoint_digest) &&
        length(segment$parent_checkpoint_digest) == 1L &&
        is.na(segment$parent_checkpoint_digest) &&
        is.character(segment$parent_schedule_digest) &&
        length(segment$parent_schedule_digest) == 1L &&
        is.na(segment$parent_schedule_digest)
    } else {
      expected_parent_schedule_digest <- .rqr_digest(
        .rqr_static_schedule_prefix(
          contract$segments[seq_len(index - 1L)]
        )
      )
      identical(
        segment$parent_checkpoint_digest, previous_checkpoint
      ) &&
        identical(
          segment$parent_schedule_digest,
          expected_parent_schedule_digest
        )
    }
    valid_endpoint <-
      is.logical(segment$ends_on_retained_draw) &&
      length(segment$ends_on_retained_draw) == 1L &&
      !is.na(segment$ends_on_retained_draw) &&
      isTRUE(segment$ends_on_retained_draw)
    if (anyNA(c(
        segment_generation, start, burn, retained, thin, raw, end
      )) ||
        !identical(segment_generation, as.integer(index - 1L)) ||
        !identical(start, previous_end) ||
        (index > 1L && burn != 0L) ||
        !valid_arithmetic || !valid_checkpoint ||
        !valid_first_links || !valid_endpoint) {
      stop(
        sprintf(
          "Static segment schedule %d is structurally invalid.",
          as.integer(index - 1L)
        ),
        call. = FALSE
      )
    }
    previous_end <- end
    previous_checkpoint <- segment$checkpoint_digest
  }
  invisible(contract)
}

.rqr_make_static_schedule_contract <- function(
    start_completed_iterations, n_burn, n_retained_draws, thin,
    checkpoint_digest, parent = NULL, parent_digest = NULL) {
  start_completed_iterations <- .rqr_history_count(
    start_completed_iterations, "start_completed_iterations"
  )
  n_burn <- .rqr_history_count(n_burn, "n_burn")
  n_retained_draws <- .rqr_scalar_integer(
    n_retained_draws, "n_retained_draws", minimum = 1L
  )
  thin <- .rqr_scalar_integer(thin, "thin", minimum = 1L)
  checkpoint_digest <- tolower(as.character(checkpoint_digest)[1L])
  if (!grepl("^[0-9a-f]{64}$", checkpoint_digest)) {
    stop("checkpoint_digest must be a complete SHA-256 digest.",
         call. = FALSE)
  }
  raw_iterations <- .rqr_history_count(
    as.double(n_burn) +
      as.double(n_retained_draws) * as.double(thin),
    "raw_iterations"
  )
  end_completed_iterations <- .rqr_history_count(
    as.double(start_completed_iterations) +
      as.double(raw_iterations),
    "end_completed_iterations"
  )

  if (is.null(parent)) {
    if (!is.null(parent_digest) || start_completed_iterations != 0L) {
      stop(
        "An initial segment schedule must start at zero without a parent.",
        call. = FALSE
      )
    }
    generation <- 0L
    segments <- list()
    parent_checkpoint_digest <- NA_character_
    stored_parent_digest <- NA_character_
  } else {
    .rqr_validate_static_schedule_value(parent, parent_digest)
    generation <- .rqr_history_count(
      as.double(parent$generation) + 1,
      "continued schedule generation"
    )
    segments <- parent$segments
    prior <- utils::tail(segments, 1L)[[1L]]
    if (!identical(
        start_completed_iterations,
        prior$end_completed_iterations
      )) {
      stop(
        "A continuation schedule must start at the parent end count.",
        call. = FALSE
      )
    }
    if (n_burn != 0L) {
      stop("A continuation schedule cannot add burn-in.",
           call. = FALSE)
    }
    parent_checkpoint_digest <- prior$checkpoint_digest
    stored_parent_digest <- parent_digest
  }
  segment <- list(
    generation = generation,
    start_completed_iterations = start_completed_iterations,
    n_burn = n_burn,
    n_retained_draws = n_retained_draws,
    thin = thin,
    raw_iterations = raw_iterations,
    end_completed_iterations = end_completed_iterations,
    ends_on_retained_draw = TRUE,
    parent_checkpoint_digest = parent_checkpoint_digest,
    parent_schedule_digest = stored_parent_digest,
    checkpoint_digest = checkpoint_digest
  )
  contract <- .rqr_static_schedule_prefix(c(segments, list(segment)))
  .rqr_validate_static_schedule_value(contract)
  contract
}

.rqr_static_external_specs <- function(states) {
  if (is.null(states) || !length(states)) return(list())
  lapply(states, function(state) {
    list(
      repo_root = if (is.na(state$repo_root %||% NA_character_)) {
        NULL
      } else {
        state$repo_root
      },
      expected_git_commit = if (
          is.na(state$expected_git_commit %||% NA_character_)) {
        NULL
      } else {
        state$expected_git_commit
      },
      runtime_package = if (
          is.na(state$runtime_package %||% NA_character_)) {
        NULL
      } else {
        state$runtime_package
      },
      runtime_attestation = if (
          is.na(state$runtime_attestation %||% NA_character_)) {
        NULL
      } else {
        state$runtime_attestation
      },
      require_isolated_runtime =
        isTRUE(state$require_isolated_runtime),
      source_subdir = state$source_subdir %||% "."
    )
  })
}

.rqr_static_current_provenance <- function(
    object, target, prior, seed = NULL) {
  stored_repo_root <- .rqr_stored_optional_provenance_text(
    object$provenance$repo_root, "repo_root"
  )
  stored_expected <- .rqr_stored_optional_provenance_text(
    object$provenance$expected_git_commit, "expected_git_commit"
  )
  primary_attestation <- .rqr_stored_optional_provenance_text(
    object$provenance$primary_runtime_attestation,
    "primary_runtime_attestation"
  )
  .rqr_provenance(
    data = .rqr_static_provenance_data(object$data_contract),
    matrices = list(X = object$X),
    numerical_policy = object$model_spec$numerical_policy,
    initial_seed = seed,
    repo_root = stored_repo_root,
    expected_git_commit = stored_expected,
    backend = "R_precision_cholesky",
    backend_requested = "R_precision_cholesky",
    backend_resolved = "R_precision_cholesky",
    objects = .rqr_static_provenance_objects(target, prior),
    external_repositories = .rqr_static_external_specs(
      object$provenance$external_repositories
    ),
    required_external_repositories =
      object$provenance$required_external_repositories,
    primary_runtime_attestation = primary_attestation
  )
}

.rqr_static_environment_mismatches <- function(stored, current) {
  fields <- c(
    "package_version", "R_version", "platform", "compiler", "BLAS",
    "LAPACK", "git_commit", "git_commit_available",
    "git_status_available", "git_dirty", "expected_git_commit",
    "expected_git_commit_match", "basic_provenance_complete",
    "provenance_complete", "primary_runtime_source_match",
    "primary_runtime_package_path", "primary_source_commit",
    "primary_source_tree_digest", "primary_runtime_tree_digest",
    "backend_requested", "backend_resolved", "RNGkind"
  )
  mismatches <- fields[!vapply(fields, function(field) {
    identical(stored[[field]], current[[field]])
  }, logical(1L))]
  if (!identical(
      stored$dependency_versions, current$dependency_versions
  )) {
    mismatches <- c(mismatches, "dependency_versions")
  }
  if (!identical(
      stored$external_repositories, current$external_repositories
  )) {
    mismatches <- c(mismatches, "external_repositories")
  }
  unique(mismatches)
}

.rqr_validate_static_fit_envelope <- function(object) {
  if (!inherits(object, "rqr_mcmc")) {
    stop("Expected an rqr_mcmc object.", call. = FALSE)
  }
  if (!identical(object$schema_version, .rqr_static_fit_schema()) ||
      !identical(object$method, "mcmc") ||
      !identical(object$family, "rqr_fixed_design") ||
      !identical(
        object$model_spec$schema_version, .rqr_static_fit_schema()
      )) {
    stop("The static fitted-object schema is unsupported.",
         call. = FALSE)
  }
  checkpoint <- object$checkpoint_state
  if (!is.list(checkpoint) ||
      !identical(
        checkpoint$schema_version, .rqr_static_checkpoint_schema()
      ) ||
      !identical(
        checkpoint$fit_schema_version, .rqr_static_fit_schema()
      ) ||
      !identical(
        checkpoint$transition_version,
        .rqr_static_transition_version()
      )) {
    stop("The static checkpoint schema is unsupported.", call. = FALSE)
  }
  .rqr_history_count(
    checkpoint$completed_iterations,
    "checkpoint$completed_iterations"
  )
  stored_digest <- object$checkpoint_digest %||% NA_character_
  if (!grepl("^[0-9a-f]{64}$", stored_digest) ||
      !identical(.rqr_digest(checkpoint), stored_digest)) {
    stop("The static checkpoint digest is invalid.", call. = FALSE)
  }
  if (!is.list(object$last) ||
      !identical(object$last, checkpoint)) {
    stop(
      paste(
        "The compatibility last-state alias does not match the",
        "authoritative checkpoint."
      ),
      call. = FALSE
    )
  }
  continuation_supported <- isTRUE(
    object$model_spec$continuation_supported
  )
  history <- if (continuation_supported) {
    .rqr_validate_continuation_history(object)
  } else {
    history_digest <- object$continuation_history_digest %||%
      NA_character_
    if (!is.null(object$continuation_history_contract) ||
        !is.character(history_digest) ||
        length(history_digest) != 1L ||
        !is.na(history_digest)) {
      stop(
        paste(
          "A noncontinuable static fit cannot carry a continuation",
          "history contract."
        ),
        call. = FALSE
      )
    }
    NULL
  }
  schedule_digest <- object$segment_schedule_digest %||% NA_character_
  schedule <- object$segment_schedule_contract
  .rqr_validate_static_schedule_value(schedule, schedule_digest)
  final_schedule <- utils::tail(schedule$segments, 1L)[[1L]]
  if (!identical(
      final_schedule$checkpoint_digest, stored_digest
    ) ||
      !identical(
        final_schedule$end_completed_iterations,
        checkpoint$completed_iterations
      )) {
    stop(
      paste(
        "The final segment schedule does not bind the authoritative",
        "checkpoint and completed-iteration count."
      ),
      call. = FALSE
    )
  }
  if (continuation_supported) {
    if (!identical(
        schedule$generation, history$generation
      ) ||
        length(schedule$segments) != length(history$segments) ||
        !all(vapply(
          seq_along(schedule$segments),
          function(index) {
            identical(
              schedule$segments[[index]]$checkpoint_digest,
              history$segments[[index]]$checkpoint_digest
            )
          },
          logical(1L)
        ))) {
      stop(
        paste(
          "The segment schedule does not match the continuation-history",
          "checkpoint chain."
        ),
        call. = FALSE
      )
    }
  } else if (length(schedule$segments) != 1L ||
      schedule$generation != 0L) {
    stop(
      "A noncontinuable static fit must contain exactly one schedule segment.",
      call. = FALSE
    )
  }

  data <- .rqr_fixed_design_data(object$y, object$X)
  if (!identical(data$data_digest, checkpoint$data_digest) ||
      !identical(data$design_digest, checkpoint$design_digest) ||
      !identical(
        .rqr_digest(unclass(data)),
        checkpoint$data_contract_digest
      ) ||
      !identical(
        .rqr_digest(unclass(object$data_contract)),
        checkpoint$data_contract_digest
      ) ||
      !identical(data$data_digest, object$data_contract$data_digest) ||
      !identical(
        data$design_digest, object$data_contract$design_digest
      )) {
    stop("The continuation data or design digest changed.",
         call. = FALSE)
  }
  prior <- .rqr_beta_prior_coerce(object$beta_prior, X = data$X)
  target <- .rqr_static_target_contract(
    coverage_level = object$model_spec$coverage_level,
    learning_rate_mode = object$model_spec$learning_rate_mode,
    fixed_learning_rate = object$model_spec$fixed_learning_rate,
    loss_reference_scale = object$model_spec$loss_reference_scale,
    lambda_prior = object$model_spec$lambda_prior,
    beta_prior = prior,
    numerical_policy = object$model_spec$numerical_policy,
    precision_beta = object$misc$precision_beta,
    root_swap_probability =
      object$model_spec$root_swap_probability,
    embedding_contract = object$embedding_contract
  )
  objects <- .rqr_static_provenance_objects(target, prior)
  object_digests <- lapply(objects, .rqr_digest)
  if (!identical(object_digests, object$provenance$object_digests) ||
      !identical(checkpoint$target_digest, object_digests$target) ||
      !identical(checkpoint$prior_digest, object_digests$beta_prior)) {
    stop("The static model, target, or prior digest changed.",
         call. = FALSE)
  }
  .rqr_prior_canonical(
    prior, checkpoint$beta_prior_state1, p = data$p
  )
  .rqr_prior_canonical(
    prior, checkpoint$beta_prior_state2, p = data$p
  )
  if (length(checkpoint$beta_root1) != data$p ||
      length(checkpoint$beta_root2) != data$p ||
      length(checkpoint$latent_v) != data$n_total ||
      any(!is.finite(checkpoint$beta_root1)) ||
      any(!is.finite(checkpoint$beta_root2)) ||
      !is.finite(checkpoint$lambda) || checkpoint$lambda <= 0 ||
      is.null(checkpoint$rng_state)) {
    stop("The static checkpoint state is incomplete or invalid.",
         call. = FALSE)
  }
  preliminary_beta1 <- object$samp.beta_root1
  preliminary_beta2 <- object$samp.beta_root2
  preliminary_lambda <- object$samp.lambda
  if (!is.matrix(preliminary_beta1) ||
      !is.numeric(preliminary_beta1) ||
      !is.matrix(preliminary_beta2) ||
      !is.numeric(preliminary_beta2) ||
      nrow(preliminary_beta1) < 1L ||
      !identical(dim(preliminary_beta1), dim(preliminary_beta2)) ||
      ncol(preliminary_beta1) != data$p ||
      !is.numeric(preliminary_lambda) ||
      !is.null(dim(preliminary_lambda)) ||
      length(preliminary_lambda) != nrow(preliminary_beta1)) {
    stop("The retained static root or lambda draws are invalid.",
         call. = FALSE)
  }
  preliminary_last <- nrow(preliminary_beta1)
  if (!identical(
      as.numeric(preliminary_beta1[preliminary_last, ]),
      as.numeric(checkpoint$beta_root1)
    ) ||
      !identical(
        as.numeric(preliminary_beta2[preliminary_last, ]),
        as.numeric(checkpoint$beta_root2)
      ) ||
      !identical(
        as.numeric(preliminary_lambda[preliminary_last]),
        as.numeric(checkpoint$lambda)
      )) {
    stop(
      paste(
        "The terminal checkpoint roots or lambda do not match the final",
        "retained draw."
      ),
      call. = FALSE
    )
  }
  if (identical(target$learning_rate_mode, "fixed_rate")) {
    expected_lambda <-
      target$fixed_learning_rate * target$loss_reference_scale
    if (!identical(
        as.numeric(checkpoint$lambda),
        as.numeric(expected_lambda)
      )) {
      stop(
        paste(
          "The fixed-rate checkpoint lambda is inconsistent with",
          "fixed_learning_rate and loss_reference_scale."
        ),
        call. = FALSE
      )
    }
  }
  checkpoint_sigma <- rqr_constants(
    target$coverage_level,
    checkpoint$lambda / target$loss_reference_scale
  )$sigma
  .rqr_static_full_latent(
    checkpoint$latent_v, data,
    placeholder = checkpoint_sigma
  )
  if (any(!data$observed) &&
      !identical(
        as.numeric(checkpoint$latent_v[!data$observed]),
        rep(checkpoint_sigma, sum(!data$observed))
      )) {
    stop(
      paste(
        "Missing-site checkpoint latent placeholders are",
        "inconsistent with the current inverse loss scale."
      ),
      call. = FALSE
    )
  }
  if (identical(prior$type, "rhs_ns")) {
    expected_updates <- checkpoint$completed_iterations
    for (state_name in c(
        "beta_prior_state1", "beta_prior_state2"
      )) {
      state <- checkpoint[[state_name]]
      state_updates <- .rqr_history_count(
        state$update_count,
        sprintf("checkpoint$%s$update_count", state_name)
      )
      if (!identical(state_updates, expected_updates)) {
        stop(
          paste(
            "The RHS-NS checkpoint update_count is inconsistent",
            "with completed_iterations."
          ),
          call. = FALSE
        )
      }
      state_repairs <- .rqr_history_count(
        state$numerical_repair_count,
        sprintf(
          "checkpoint$%s$numerical_repair_count", state_name
        )
      )
      if (!identical(state_repairs, 0L)) {
        stop(
          "RHS-NS checkpoint repair counts must be zero.",
          call. = FALSE
        )
      }
    }
  }

  n_burn <- .rqr_history_count(
    object$misc$n_burn, "misc$n_burn"
  )
  n_mcmc <- .rqr_scalar_integer(
    object$misc$n_mcmc, "misc$n_mcmc", minimum = 1L
  )
  thin <- .rqr_scalar_integer(
    object$misc$thin, "misc$thin", minimum = 1L
  )
  store_latent_draws <- .rqr_scalar_logical(
    object$misc$store_latent_draws, "misc$store_latent_draws"
  )
  store_prior_state_draws <- .rqr_scalar_logical(
    object$misc$store_prior_state_draws,
    "misc$store_prior_state_draws"
  )
  if (!identical(final_schedule$n_burn, n_burn) ||
      !identical(final_schedule$n_retained_draws, n_mcmc) ||
      !identical(final_schedule$thin, thin)) {
    stop(
      "The final segment schedule conflicts with the fitted MCMC controls.",
      call. = FALSE
    )
  }
  beta1_draws <- object$samp.beta_root1
  beta2_draws <- object$samp.beta_root2
  lambda_draws <- object$samp.lambda
  if (!is.matrix(beta1_draws) || !is.numeric(beta1_draws) ||
      !is.matrix(beta2_draws) || !is.numeric(beta2_draws) ||
      !identical(dim(beta1_draws), c(n_mcmc, data$p)) ||
      !identical(dim(beta2_draws), c(n_mcmc, data$p)) ||
      any(!is.finite(beta1_draws)) ||
      any(!is.finite(beta2_draws)) ||
      !is.numeric(lambda_draws) || !is.null(dim(lambda_draws)) ||
      length(lambda_draws) != n_mcmc ||
      any(!is.finite(lambda_draws)) || any(lambda_draws <= 0)) {
    stop("The retained static root or lambda draws are invalid.",
         call. = FALSE)
  }
  if (!identical(
      as.numeric(beta1_draws[n_mcmc, ]),
      as.numeric(checkpoint$beta_root1)
    ) ||
      !identical(
        as.numeric(beta2_draws[n_mcmc, ]),
        as.numeric(checkpoint$beta_root2)
      ) ||
      !identical(
        as.numeric(lambda_draws[n_mcmc]),
        as.numeric(checkpoint$lambda)
      )) {
    stop(
      paste(
        "The terminal checkpoint roots or lambda do not match the final",
        "retained draw."
      ),
      call. = FALSE
    )
  }
  if (identical(target$learning_rate_mode, "fixed_rate")) {
    expected_lambda <-
      target$fixed_learning_rate * target$loss_reference_scale
    if (!identical(
        as.numeric(lambda_draws),
        rep(as.numeric(expected_lambda), n_mcmc)
      )) {
      stop(
        "Fixed-rate retained lambda draws are not exact constants.",
        call. = FALSE
      )
    }
  }
  if (store_latent_draws) {
    latent_draws <- object$samp.latent_v
    if (!is.matrix(latent_draws) || !is.numeric(latent_draws) ||
        !identical(dim(latent_draws), c(n_mcmc, data$n_total)) ||
        any(!is.finite(latent_draws)) ||
        any(latent_draws <= 0) ||
        !identical(
          as.numeric(latent_draws[n_mcmc, ]),
          as.numeric(checkpoint$latent_v)
        )) {
      stop(
        paste(
          "The stored terminal latent state does not match the final",
          "retained draw."
        ),
        call. = FALSE
      )
    }
  } else if (!is.null(object$samp.latent_v)) {
    stop(
      "Latent draws are present although latent storage is disabled.",
      call. = FALSE
    )
  }
  if (store_prior_state_draws) {
    prior_draws1 <- object$samp.beta_prior_state_root1
    prior_draws2 <- object$samp.beta_prior_state_root2
    if (!is.list(prior_draws1) || !is.list(prior_draws2) ||
        length(prior_draws1) != n_mcmc ||
        length(prior_draws2) != n_mcmc ||
        !identical(
          prior_draws1[[n_mcmc]], checkpoint$beta_prior_state1
        ) ||
        !identical(
          prior_draws2[[n_mcmc]], checkpoint$beta_prior_state2
        )) {
      stop(
        paste(
          "The stored terminal coefficient-prior state does not match",
          "the final retained draw."
        ),
        call. = FALSE
      )
    }
  } else if (!is.null(object$samp.beta_prior_state_root1) ||
      !is.null(object$samp.beta_prior_state_root2)) {
    stop(
      paste(
        "Coefficient-prior state draws are present although prior-state",
        "storage is disabled."
      ),
      call. = FALSE
    )
  }

  invisible(list(
    checkpoint = checkpoint,
    checkpoint_digest = stored_digest,
    history = history,
    schedule = schedule,
    data = data,
    prior = prior,
    target = target
  ))
}

.rqr_validate_static_checkpoint <- function(
    object, allow_environment_mismatch = FALSE) {
  if (!is.logical(allow_environment_mismatch) ||
      length(allow_environment_mismatch) != 1L ||
      is.na(allow_environment_mismatch)) {
    stop("allow_environment_mismatch must be TRUE or FALSE.",
         call. = FALSE)
  }
  envelope <- .rqr_validate_static_fit_envelope(object)
  checkpoint <- envelope$checkpoint
  stored_digest <- envelope$checkpoint_digest
  history <- envelope$history
  data <- envelope$data
  prior <- envelope$prior
  target <- envelope$target
  current <- .rqr_static_current_provenance(
    object, target, prior
  )
  mismatches <- .rqr_static_environment_mismatches(
    object$provenance, current
  )
  if (length(mismatches) && !allow_environment_mismatch) {
    stop(
      sprintf(
        paste0(
          "Continuation environment differs in: %s. Set ",
          "allow_environment_mismatch=TRUE only for a non-bitwise ",
          "portability run."
        ),
        paste(mismatches, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  if (length(mismatches)) {
    warning(
      paste(
        "Continuation environment differs in:",
        paste(mismatches, collapse = ", "),
        "Exact bitwise continuation is not claimed for this segment."
      ),
      call. = FALSE
    )
  }
  invisible(list(
    checkpoint = checkpoint,
    checkpoint_digest = stored_digest,
    history = history,
    schedule = envelope$schedule,
    data = data,
    prior = prior,
    target = target,
    current_provenance = current,
    environment_mismatches = mismatches
  ))
}
