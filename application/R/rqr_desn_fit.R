# Frozen-design ordinary RQR-DESN integration.

.rqr_desn_fit_schema <- function() {
  "rqrgibbs_desn_fit/1.1.0"
}

.rqr_desn_materialization_verification_schema <- function() {
  "rqrgibbs_desn_materialization_verification/1.0.0"
}

.rqr_desn_materialization_state_matches <- function(receipt, state) {
  is.list(receipt) &&
    is.list(state) &&
    identical(state$runtime_package, "exdqlm") &&
    identical(
      state$runtime_package_version,
      receipt$package_version
    ) &&
    identical(state$git_commit, receipt$source_commit) &&
    identical(
      state$expected_git_commit,
      receipt$source_commit
    ) &&
    identical(
      state$source_tree_digest,
      receipt$source_tree_digest
    ) &&
    identical(
      state$runtime_package_tree_digest,
      receipt$runtime_tree_digest
    ) &&
    identical(
      state$runtime_attestation_schema,
      receipt$runtime_attestation_schema
    ) &&
    isTRUE(state$require_isolated_runtime) &&
    isTRUE(state$runtime_attestation_match) &&
    isTRUE(state$runtime_source_match) &&
    isTRUE(state$reproducibility_eligible)
}

.rqr_desn_materialization_verification <- function(
    design, external_state = NULL, runtime_attestation = NULL) {
  receipt_status <-
    .rqr_desn_materialization_receipt_status(design)
  receipt <- design$builder$materialization_receipt %||% NULL
  external_state_present <- is.list(external_state)
  external_state_match <- receipt_status$receipt_valid &&
    .rqr_desn_materialization_state_matches(
      receipt, external_state
    )
  attestation_sha256 <- NA_character_
  attestation_sha256_verified <- FALSE
  if (external_state_match &&
      is.character(runtime_attestation) &&
      length(runtime_attestation) == 1L &&
      !is.na(runtime_attestation) &&
      nzchar(runtime_attestation) &&
      file.exists(runtime_attestation)) {
    attestation_sha256 <- digest::digest(
      file = runtime_attestation, algo = "sha256",
      serialize = FALSE
    )
    attestation_sha256_verified <- identical(
      attestation_sha256,
      receipt$runtime_attestation_sha256
    )
  }
  eligible <- receipt_status$receipt_valid &&
    external_state_match &&
    attestation_sha256_verified
  status <- if (!receipt_status$reference_materializer) {
    "custom_frozen_design_unattested"
  } else if (!receipt_status$receipt_valid) {
    "reference_receipt_invalid"
  } else if (!external_state_present) {
    "current_external_provenance_not_bound"
  } else if (!external_state_match) {
    "current_external_provenance_mismatch"
  } else if (!attestation_sha256_verified) {
    "current_runtime_attestation_hash_mismatch"
  } else {
    "verified_current_isolated_materialization"
  }
  list(
    schema_version =
      .rqr_desn_materialization_verification_schema(),
    reference_materializer =
      receipt_status$reference_materializer,
    receipt_valid = receipt_status$receipt_valid,
    receipt_digest = receipt_status$receipt_digest,
    materialized_design_payload_digest =
      receipt_status$materialized_design_payload_digest,
    external_state_present = external_state_present,
    external_state_digest = if (external_state_present) {
      .rqr_digest(external_state)
    } else {
      NA_character_
    },
    external_state_match = external_state_match,
    runtime_attestation_sha256 = attestation_sha256,
    runtime_attestation_sha256_verified =
      attestation_sha256_verified,
    materialization_reproducibility_eligible = eligible,
    status = status
  )
}

.rqr_desn_prepare_materialization <- function(
    design, design_engine, provenance_control) {
  receipt_status <-
    .rqr_desn_materialization_receipt_status(design)
  external_state <- NULL
  runtime_attestation <- NULL
  existing_spec <-
    provenance_control$external_repositories$exdqlm %||% NULL
  bind_external <- receipt_status$reference_materializer &&
    (identical(design_engine, "exdqlm_reference") ||
      !is.null(existing_spec))
  if (bind_external) {
    provenance_control <- .rqr_require_external_repository(
      provenance_control, "exdqlm",
      .rqr_pinned_exdqlm_commit(), runtime_package = "exdqlm"
    )
    spec <- provenance_control$external_repositories$exdqlm
    external_state <- .rqr_repository_provenance(spec)
    runtime_attestation <- spec$runtime_attestation %||% NULL
  }
  verification <- .rqr_desn_materialization_verification(
    design, external_state = external_state,
    runtime_attestation = runtime_attestation
  )
  if (identical(design_engine, "exdqlm_reference") &&
      !isTRUE(
        verification$materialization_reproducibility_eligible
      )) {
    stop(
      paste(
        "The exdqlm reference materializer did not produce a design",
        "bound to its current verified isolated runtime attestation."
      ),
      call. = FALSE
    )
  }
  list(
    provenance_control = provenance_control,
    verification = verification
  )
}

.rqr_desn_embedding_contract <- function(
    design, design_engine, materialization_verification) {
  rqr_validate_desn_design(design)
  if (!design_engine %in% c("frozen", "exdqlm_reference")) {
    stop("Unsupported DESN design engine.", call. = FALSE)
  }
  if (!is.list(materialization_verification) ||
      !identical(
        materialization_verification$schema_version,
        .rqr_desn_materialization_verification_schema()
      )) {
    stop(
      "A versioned DESN materialization verification is required.",
      call. = FALSE
    )
  }
  list(
    schema_version = design$schema_version,
    semantic_digest = design$semantic_digest,
    component_digests = design$digests,
    feature_schema = design$feature_schema,
    builder = design$builder,
    reservoir = design$reservoir,
    driver = design$driver,
    causal = design$causal,
    time = design$time,
    terminal = design$terminal,
    design_role = "frozen_deterministic_desn_features",
    design_engine = design_engine,
    materialization_verification =
      materialization_verification,
    reference_materializer =
      materialization_verification$reference_materializer,
    materialization_reproducibility_eligible =
      materialization_verification$
        materialization_reproducibility_eligible,
    response_simulation = FALSE
  )
}

.rqr_desn_validate_embedding_contract <- function(fit, design) {
  embedding <- fit$embedding_contract
  if (!is.list(embedding)) {
    stop("The DESN embedding contract is missing.", call. = FALSE)
  }
  verification <- embedding$materialization_verification
  status <- .rqr_desn_materialization_receipt_status(design)
  external_state <-
    fit$provenance$external_repositories$exdqlm %||% NULL
  current_state_present <- is.list(external_state)
  current_state_digest <- if (current_state_present) {
    .rqr_digest(external_state)
  } else {
    NA_character_
  }
  current_state_match <- status$receipt_valid &&
    .rqr_desn_materialization_state_matches(
      design$builder$materialization_receipt %||% NULL,
      external_state
    )
  hash_verified <- is.list(verification) &&
    isTRUE(verification$runtime_attestation_sha256_verified) &&
    status$receipt_valid &&
    identical(
      verification$runtime_attestation_sha256,
      design$builder$materialization_receipt$
        runtime_attestation_sha256
    )
  portability_binding <- is.list(verification) &&
    isTRUE(
      fit$continuation_history_contract$
        cumulative_environment_override_used
    ) &&
    !isTRUE(fit$provenance$reproducibility_eligible) &&
    isTRUE(verification$external_state_present) &&
    isTRUE(verification$external_state_match) &&
    .rqr_desn_is_sha256(
      verification$external_state_digest
    ) &&
    isTRUE(
      verification$materialization_reproducibility_eligible
    ) &&
    hash_verified &&
    !identical(
      verification$external_state_digest,
      current_state_digest
    )
  state_present <- if (portability_binding) {
    verification$external_state_present
  } else {
    current_state_present
  }
  state_match <- if (portability_binding) {
    verification$external_state_match
  } else {
    current_state_match
  }
  expected_state_digest <- if (portability_binding) {
    verification$external_state_digest
  } else {
    current_state_digest
  }
  expected_eligible <- status$receipt_valid &&
    state_match && hash_verified
  expected_status <- if (!status$reference_materializer) {
    "custom_frozen_design_unattested"
  } else if (!status$receipt_valid) {
    "reference_receipt_invalid"
  } else if (!state_present) {
    "current_external_provenance_not_bound"
  } else if (!state_match) {
    "current_external_provenance_mismatch"
  } else if (!hash_verified) {
    "current_runtime_attestation_hash_mismatch"
  } else {
    "verified_current_isolated_materialization"
  }
  verification_valid <- is.list(verification) &&
    identical(
      verification$schema_version,
      .rqr_desn_materialization_verification_schema()
    ) &&
    identical(
      verification$reference_materializer,
      status$reference_materializer
    ) &&
    identical(verification$receipt_valid, status$receipt_valid) &&
    identical(
      verification$receipt_digest, status$receipt_digest
    ) &&
    identical(
      verification$materialized_design_payload_digest,
      status$materialized_design_payload_digest
    ) &&
    identical(
      verification$external_state_present, state_present
    ) &&
    identical(
      verification$external_state_digest,
      expected_state_digest
    ) &&
    identical(verification$external_state_match, state_match) &&
    identical(
      verification$materialization_reproducibility_eligible,
      expected_eligible
    ) &&
    identical(verification$status, expected_status)
  expected_embedding <- list(
    schema_version = design$schema_version,
    semantic_digest = design$semantic_digest,
    component_digests = design$digests,
    feature_schema = design$feature_schema,
    builder = design$builder,
    reservoir = design$reservoir,
    driver = design$driver,
    causal = design$causal,
    time = design$time,
    terminal = design$terminal,
    design_role = "frozen_deterministic_desn_features",
    design_engine = embedding$design_engine,
    materialization_verification = verification,
    reference_materializer =
      status$reference_materializer,
    materialization_reproducibility_eligible =
      expected_eligible,
    response_simulation = FALSE
  )
  engine_valid <- is.character(embedding$design_engine) &&
    length(embedding$design_engine) == 1L &&
    !is.na(embedding$design_engine) &&
    embedding$design_engine %in% c("frozen", "exdqlm_reference")
  if (!engine_valid ||
      (identical(embedding$design_engine, "exdqlm_reference") &&
        !expected_eligible) ||
      !verification_valid ||
      !identical(embedding, expected_embedding) ||
      !identical(
        fit$provenance$object_digests$embedding,
        .rqr_digest(embedding)
      )) {
    stop(
      paste(
        "The DESN embedding is not bound to its design and",
        "materialization provenance."
      ),
      call. = FALSE
    )
  }
  invisible(verification)
}

.rqr_desn_outer_model_spec <- function(fit, design) {
  verification <-
    .rqr_desn_validate_embedding_contract(fit, design)
  model_spec <- fit$model_spec
  readout_reproducibility <-
    isTRUE(model_spec$reproducibility_eligible)
  materialization_eligible <- isTRUE(
    verification$materialization_reproducibility_eligible
  )
  model_spec$family <- "rqr_desn"
  model_spec$embedding <- "frozen_desn_design"
  model_spec$design_semantic_digest <- design$semantic_digest
  model_spec$design_engine <-
    fit$embedding_contract$design_engine
  model_spec$design_contract_verified <- TRUE
  model_spec$design_materialization_receipt_valid <-
    isTRUE(verification$receipt_valid)
  model_spec$design_materialization_external_binding_verified <-
    isTRUE(verification$external_state_match) &&
    isTRUE(verification$runtime_attestation_sha256_verified)
  model_spec$design_materialization_reproducibility_eligible <-
    materialization_eligible
  model_spec$readout_reproducibility_eligible <-
    readout_reproducibility
  model_spec$reproducibility_eligible <-
    readout_reproducibility && materialization_eligible
  model_spec$promotion_eligible <-
    isTRUE(fit$model_spec$promotion_eligible) &&
    materialization_eligible
  model_spec
}

.rqr_desn_outer_meta <- function(model_spec) {
  list(
    inference_method = "rqr_mcmc",
    rqr_coverage_level = model_spec$coverage_level,
    rqr_learning_rate = model_spec$learning_rate,
    rqr_effective_learning_rate =
      model_spec$effective_learning_rate,
    rqr_loss_reference_scale =
      model_spec$loss_reference_scale,
    rqr_learning_rate_mode =
      model_spec$learning_rate_mode,
    design_engine = model_spec$design_engine,
    design_semantic_digest =
      model_spec$design_semantic_digest,
    design_contract_verified =
      model_spec$design_contract_verified,
    design_materialization_receipt_valid =
      model_spec$design_materialization_receipt_valid,
    design_materialization_external_binding_verified =
      model_spec$
        design_materialization_external_binding_verified,
    design_materialization_reproducibility_eligible =
      model_spec$
        design_materialization_reproducibility_eligible,
    reproducibility_eligible =
      model_spec$reproducibility_eligible,
    promotion_eligible = model_spec$promotion_eligible,
    response_likelihood = FALSE,
    response_simulation = FALSE,
    generalized_bayes = TRUE
  )
}

.rqr_desn_note <- function() {
  paste(
    "RQR-DESN is ordinary RQR conditional on frozen deterministic",
    "features; it defines interval roots, not response draws."
  )
}

.rqr_desn_build_envelope <- function(fit, design) {
  model_spec <- .rqr_desn_outer_model_spec(fit, design)
  out <- list(
    schema_version = .rqr_desn_fit_schema(),
    fit = fit,
    design = design,
    X = design$X,
    y_fit = design$y,
    reservoir = design$reservoir,
    states = NULL,
    reference_shell = NULL,
    meta = .rqr_desn_outer_meta(model_spec),
    model_spec = model_spec,
    summary = fit$summary,
    note = .rqr_desn_note()
  )
  class(out) <- c("rqr_desn_fit", "rqr_fit")
  out
}

.rqr_validate_desn_fit_envelope <- function(object) {
  if (!inherits(object, "rqr_desn_fit") ||
      !identical(
        object$schema_version, .rqr_desn_fit_schema()
      ) ||
      !inherits(object$fit, "rqr_mcmc")) {
    stop("Expected a supported rqr_desn_fit object.", call. = FALSE)
  }
  rqr_validate_desn_design(object$design)
  fit <- object$fit
  .rqr_validate_static_fit_envelope(fit)
  accepted_modes <- c(
    "fixed_rate", "learned_pseudoresidual_normalized"
  )
  mode_valid <- is.character(
    fit$model_spec$learning_rate_mode
  ) &&
    length(fit$model_spec$learning_rate_mode) == 1L &&
    !is.na(fit$model_spec$learning_rate_mode) &&
    fit$model_spec$learning_rate_mode %in% accepted_modes
  inner_promotion <- isTRUE(
    fit$model_spec$target_numerical_eligible
  ) &&
    isTRUE(fit$provenance$reproducibility_eligible) &&
    isTRUE(fit$model_spec$ordinary_v1_scope_eligible)
  prior <- .rqr_beta_prior_coerce(
    fit$beta_prior, X = fit$X
  )
  target <- .rqr_static_target_contract(
    coverage_level = fit$model_spec$coverage_level,
    learning_rate_mode = fit$model_spec$learning_rate_mode,
    fixed_learning_rate = fit$model_spec$fixed_learning_rate,
    loss_reference_scale =
      fit$model_spec$loss_reference_scale,
    lambda_prior = fit$model_spec$lambda_prior,
    beta_prior = prior,
    numerical_policy = fit$model_spec$numerical_policy,
    precision_beta = fit$misc$precision_beta,
    root_swap_probability =
      fit$model_spec$root_swap_probability,
    embedding_contract = fit$embedding_contract
  )
  object_digests <- lapply(
    .rqr_static_provenance_objects(target, prior),
    .rqr_digest
  )
  if (!identical(fit$model_spec$family, "rqr_fixed_design") ||
      !mode_valid ||
      !identical(fit$model_spec$tilt, 0) ||
      !isTRUE(fit$model_spec$generalized_bayes) ||
      isTRUE(fit$model_spec$response_likelihood) ||
      isTRUE(fit$model_spec$response_prediction_contract) ||
      !isTRUE(fit$model_spec$exact_joint_target) ||
      !isTRUE(fit$model_spec$ordinary_v1_scope_eligible) ||
      !isTRUE(fit$model_spec$continuation_supported) ||
      !identical(
        fit$model_spec$reproducibility_eligible,
        fit$provenance$reproducibility_eligible
      ) ||
      !identical(
        fit$model_spec$promotion_eligible,
        inner_promotion
      ) ||
      !identical(
        object_digests, fit$provenance$object_digests
      ) ||
      !identical(
        fit$checkpoint_state$target_digest,
        object_digests$target
      ) ||
      !identical(
        fit$checkpoint_state$prior_digest,
        object_digests$beta_prior
      ) ||
      !identical(
        .rqr_digest(fit$checkpoint_state),
        fit$checkpoint_digest
      ) ||
      !identical(fit$X, object$design$X) ||
      !identical(fit$y, object$design$y)) {
    stop(
      paste(
        "The embedded ordinary-RQR readout target or integrity",
        "digest is inconsistent."
      ),
      call. = FALSE
    )
  }
  .rqr_validate_continuation_history(fit)
  expected <- .rqr_desn_build_envelope(fit, object$design)
  fields <- c(
    "schema_version", "design", "X", "y_fit", "reservoir",
    "states", "reference_shell", "meta", "model_spec",
    "summary", "note"
  )
  if (any(!vapply(
      fields,
      function(field) identical(object[[field]], expected[[field]]),
      logical(1L)
    ))) {
    stop(
      paste(
        "The DESN fit envelope is inconsistent with its frozen",
        "design, embedded target, or derived status fields."
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.rqr_desn_adapter_feature_names <- function(X) {
  feature_names <- colnames(X)
  if (is.null(feature_names) ||
      length(feature_names) != ncol(X) ||
      anyNA(feature_names) || any(!nzchar(feature_names)) ||
      anyDuplicated(feature_names)) {
    feature_names <- sprintf(
      "desn_feature_%04d", seq_len(ncol(X))
    )
  }
  feature_names
}

.rqr_desn_adapter_intercept <- function(X, feature_names) {
  matches <- which(vapply(seq_len(ncol(X)), function(index) {
    all(X[, index] == 1)
  }, logical(1L)))
  if (!length(matches)) return(NULL)
  if (length(matches) > 1L) {
    stop(
      "The reference DESN adapter produced multiple constant-one columns.",
      call. = FALSE
    )
  }
  feature_names[matches]
}

.rqr_desn_response_vector <- function(
    y, name = "y", require_complete = FALSE) {
  if (!is.numeric(y) || !is.null(dim(y))) {
    stop(sprintf("%s must be a numeric vector.", name),
         call. = FALSE)
  }
  y <- as.numeric(y)
  if (!length(y) || any(is.nan(y)) || any(is.infinite(y)) ||
      (require_complete && anyNA(y))) {
    qualifier <- if (require_complete) {
      "nonempty, complete, and finite"
    } else {
      "nonempty and finite apart from NA"
    }
    stop(
      sprintf("%s must be a %s numeric vector.", name, qualifier),
      call. = FALSE
    )
  }
  y
}

.rqr_materialize_exdqlm_desn_design <- function(
    y, args, provenance_control, design_metadata = list()) {
  y <- .rqr_desn_response_vector(
    y, "reference-adapter y", require_complete = TRUE
  )
  .rqr_validate_named_list_fields(
    design_metadata, "design_metadata",
    c("builder", "reservoir", "driver", "causal", "time", "terminal")
  )
  .rqr_validate_named_list_fields(
    args, "reservoir arguments",
    unique(names(args) %||% character(0))
  )
  provenance_control <- .rqr_require_external_repository(
    provenance_control, "exdqlm",
    .rqr_pinned_exdqlm_commit(), runtime_package = "exdqlm"
  )
  builder_overrides <- design_metadata$builder %||% list()
  reservoir_overrides <- design_metadata$reservoir %||% list()
  if (!is.list(builder_overrides) ||
      !is.list(reservoir_overrides)) {
    stop(
      "design_metadata builder and reservoir entries must be lists.",
      call. = FALSE
    )
  }
  protected_builder <- c(
    "id", "version", "source_commit", "arguments_digest",
    "adapter", "materialization_receipt"
  )
  protected_reservoir <- c(
    "digest", "source_package", "source_commit"
  )
  if (length(intersect(
      names(builder_overrides) %||% character(0),
      protected_builder
    )) ||
      length(intersect(
        names(reservoir_overrides) %||% character(0),
        protected_reservoir
      ))) {
    stop(
      paste(
        "design_metadata cannot override builder identity, source,",
        "arguments, materialization receipt, or reservoir identity."
      ),
      call. = FALSE
    )
  }
  exdqlm_spec <-
    provenance_control$external_repositories$exdqlm
  exdqlm_state <- .rqr_repository_provenance(exdqlm_spec)
  if (!isTRUE(exdqlm_state$reproducibility_eligible) ||
      !isTRUE(exdqlm_state$runtime_source_match) ||
      !identical(
        exdqlm_state$git_commit,
        .rqr_pinned_exdqlm_commit()
      )) {
    stop(
      paste(
        "The reference DESN materializer requires the clean pinned",
        "exdqlm checkout and its matching isolated runtime attestation."
      ),
      call. = FALSE
    )
  }
  attestation_path <- exdqlm_spec$runtime_attestation
  shell <- do.call(
    qdesn_fit_vb,
    c(
      list(
        y = as.numeric(y), p0 = 0.5,
        fit_readout = FALSE, vb_args = list()
      ),
      args
    )
  )
  X <- as.matrix(shell$X)
  storage.mode(X) <- "double"
  y_fit <- as.numeric(shell$y_fit)
  if (!nrow(X) || nrow(X) != length(y_fit) ||
      any(!is.finite(X)) || any(!is.finite(y_fit)) ||
      all(abs(X) <= sqrt(.Machine$double.eps))) {
    stop(
      "The exdqlm adapter returned an invalid or degenerate DESN design.",
      call. = FALSE
    )
  }
  feature_names <- .rqr_desn_adapter_feature_names(X)
  colnames(X) <- feature_names
  intercept <- .rqr_desn_adapter_intercept(X, feature_names)
  arguments_digest <- .rqr_digest(args)
  builder <- utils::modifyList(list(
    id = .rqr_desn_reference_builder_id(),
    version = exdqlm_state$runtime_package_version,
    source_commit = .rqr_pinned_exdqlm_commit(),
    arguments_digest = arguments_digest,
    adapter = "rqrgibbs_frozen_design_materializer/2.0.0"
  ), builder_overrides)
  reservoir <- utils::modifyList(list(
    digest = .rqr_digest(shell$reservoir %||% list()),
    source_package = "exdqlm",
    source_commit = .rqr_pinned_exdqlm_commit()
  ), reservoir_overrides)
  original_index <- shell$meta$fit_time_index %||%
    shell$meta$time_index %||% NULL
  if (is.null(original_index) ||
      length(original_index) != nrow(X) ||
      any(!is.finite(original_index)) ||
      any(diff(original_index) <= 0)) {
    original_index <- seq_len(nrow(X))
  }
  driver <- design_metadata$driver %||% list(
    type = "observed_lagged_response_history",
    response_simulation = FALSE
  )
  causal <- design_metadata$causal %||% list(
    uses_current_response = FALSE,
    uses_future_response = FALSE,
    contract =
      "row_t_uses_only_information_available_before_t"
  )
  time <- design_metadata$time %||% list()
  terminal <- design_metadata$terminal %||%
    list(available = FALSE)
  preliminary_design <- rqr_desn_design(
    X = X, y = y_fit, time_index = original_index,
    feature_names = feature_names, intercept = intercept,
    builder = builder, reservoir = reservoir,
    driver = driver, causal = causal, time = time,
    terminal = terminal
  )
  materialization_receipt <- list(
    schema_version =
      .rqr_desn_materialization_receipt_schema(),
    package = "exdqlm",
    package_version =
      exdqlm_state$runtime_package_version,
    source_commit = exdqlm_state$git_commit,
    source_tree_digest = exdqlm_state$source_tree_digest,
    runtime_tree_digest =
      exdqlm_state$runtime_package_tree_digest,
    runtime_attestation_schema =
      exdqlm_state$runtime_attestation_schema,
    runtime_attestation_sha256 = digest::digest(
      file = attestation_path, algo = "sha256",
      serialize = FALSE
    ),
    materializer_arguments_digest = arguments_digest,
    materialized_design_payload_digest = .rqr_desn_sha256(
      .rqr_desn_materialization_payload(preliminary_design)
    ),
    runtime_source_match =
      exdqlm_state$runtime_source_match,
    reproducibility_eligible =
      exdqlm_state$reproducibility_eligible
  )
  builder$materialization_receipt <- materialization_receipt
  design <- rqr_desn_design(
    X = X, y = y_fit, time_index = original_index,
    feature_names = feature_names, intercept = intercept,
    builder = builder, reservoir = reservoir,
    driver = driver, causal = causal, time = time,
    terminal = terminal
  )
  list(
    design = design,
    reference_shell = shell,
    provenance_control = provenance_control
  )
}

#' Fit ordinary RQR on a frozen DESN feature design
#'
#' The preferred interface supplies a validated [rqr_desn_design()] object.
#' The legacy adapter can materialize that object once through the pinned,
#' isolated exdqlm reference runtime. In either case, the fitted stochastic
#' model is the native ordinary-RQR fixed-design Gibbs sampler conditional on
#' the frozen features. No response-simulation distribution is introduced.
#'
#' @param y Optional response history for the legacy reference adapter. A
#'   supplied frozen `design` already contains its aligned response.
#' @param coverage_level Interval coverage level in `(0, 1)`.
#' @param ... Reservoir arguments used only by
#'   `design_engine="exdqlm_reference"`.
#' @param design Optional validated `rqr_desn_design`.
#' @param design_engine `"frozen"` or `"exdqlm_reference"`. `"auto"` chooses
#'   `"frozen"` when `design` is supplied.
#' @param design_metadata Plain metadata overrides used only while
#'   materializing a reference design.
#' @param inference Ordinary v1 accepts only `"mcmc"`.
#' @param learning_rate Positive fixed generalized-Bayes rate.
#' @param lambda_initial Positive initial inverse loss scale in learned mode.
#' @param loss_reference_scale Positive fixed reference scale.
#' @param learning_rate_mode Fixed or normalized learned-rate target.
#' @param lambda_prior Gamma prior for the normalized learned rate.
#' @param numerical_policy Either `"fail"` or `"record_repair"`.
#' @param provenance_control Provenance controls. The reference adapter binds
#'   the pinned exdqlm source/runtime; a supplied frozen design needs no
#'   executing exdqlm namespace.
#' @param mcmc_args MCMC controls, prior selection, initialization, and the
#'   root-swap probability. Target and provenance controls belong to their
#'   named top-level arguments and cannot be overridden here.
#' @param vb_args Reserved compatibility argument; it must remain empty.
#' @param fit_readout If `FALSE`, return the validated frozen design without
#'   fitting its readout.
#' @return An `rqr_desn_fit` or `rqr_desn_design`.
#' @export
rqr_desn_fit <- function(
    y = NULL, coverage_level, ...,
    design = NULL,
    design_engine = c("auto", "frozen", "exdqlm_reference"),
    design_metadata = list(),
    inference = "mcmc",
    learning_rate = 1, lambda_initial = 1,
    loss_reference_scale = 1,
    learning_rate_mode = "fixed_rate",
    lambda_prior = list(shape = 4, rate = 4),
    numerical_policy = c("fail", "record_repair"),
    provenance_control = list(), mcmc_args = list(),
    vb_args = list(), fit_readout = TRUE) {
  design_engine <- match.arg(design_engine)
  if (!is.character(inference) ||
      length(inference) != 1L || is.na(inference) ||
      !identical(inference, "mcmc")) {
    stop(
      paste(
        "Ordinary RQR-DESN v1 supports exact MCMC only;",
        "the experimental VB routine is outside this contract."
      ),
      call. = FALSE
    )
  }
  mode <- .rqr_learning_rate_mode(learning_rate_mode)
  if (!mode %in%
      c("fixed_rate", "learned_pseudoresidual_normalized")) {
    stop(
      paste(
        "Ordinary RQR-DESN v1 accepts only fixed_rate or",
        "learned_pseudoresidual_normalized."
      ),
      call. = FALSE
    )
  }
  lambda_prior <- .rqr_lambda_prior(lambda_prior, mode)
  numerical_policy <- .rqr_numerical_policy(numerical_policy)
  provenance_control <- .rqr_provenance_control(provenance_control)
  if (!is.list(mcmc_args) || !is.list(vb_args)) {
    stop("mcmc_args and vb_args must be lists.", call. = FALSE)
  }
  .rqr_validate_named_list_fields(
    design_metadata, "design_metadata",
    c("builder", "reservoir", "driver", "causal", "time", "terminal")
  )
  mcmc_control_fields <- c(
    "n_burn", "n_mcmc", "thin", "seed", "rng_seed",
    "verbose", "progress_every", "store_latent_draws",
    "store_prior_state_draws", "precision_beta", "precision",
    "intercept_name"
  )
  mcmc_readout_fields <- c(
    "beta_prior_obj", "beta_prior_type", "beta_rhs",
    "beta_ridge_tau2", "tau2", "beta_gaussian",
    "root_swap_probability", "init",
    "mcmc_control"
  )
  .rqr_validate_named_list_fields(
    mcmc_args, "mcmc_args",
    c(mcmc_control_fields, mcmc_readout_fields)
  )
  if ("mcmc_control" %in% names(mcmc_args) &&
      any(mcmc_control_fields %in% names(mcmc_args))) {
    stop(
      paste(
        "mcmc_args cannot mix nested mcmc_control with flat",
        "MCMC-control fields."
      ),
      call. = FALSE
    )
  }
  resolved_mcmc_control <- if ("mcmc_control" %in% names(mcmc_args)) {
    mcmc_args$mcmc_control
  } else {
    mcmc_args[intersect(names(mcmc_args), mcmc_control_fields)]
  }
  .rqr_validate_named_list_fields(
    resolved_mcmc_control, "mcmc_args$mcmc_control",
    mcmc_control_fields
  )
  if (length(vb_args)) {
    stop(
      "vb_args are unavailable under the ordinary RQR-DESN v1 MCMC contract.",
      call. = FALSE
    )
  }
  fit_readout <- .rqr_scalar_logical(fit_readout, "fit_readout")
  args <- list(...)
  .rqr_validate_named_list_fields(
    args, "reservoir arguments",
    unique(names(args) %||% character(0))
  )
  forbidden <- intersect(
    names(args) %||% character(0),
    c("p0", "target_p", "weights")
  )
  if (length(forbidden)) {
    stop(
      sprintf(
        "RQR-DESN does not accept these Q-DESN readout controls: %s.",
        paste(forbidden, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (inherits(y, "rqr_desn_design") && is.null(design)) {
    design <- y
    y <- NULL
  }
  if (identical(design_engine, "auto")) {
    design_engine <- if (is.null(design)) {
      "exdqlm_reference"
    } else {
      "frozen"
    }
  }
  if (identical(design_engine, "frozen")) {
    if (is.null(design)) {
      stop("design_engine='frozen' requires design.", call. = FALSE)
    }
    if (length(args)) {
      stop(
        "Reservoir-construction arguments are invalid for a frozen design.",
        call. = FALSE
      )
    }
    if (length(design_metadata)) {
      stop(
        paste(
          "design_metadata is used only while materializing an",
          "exdqlm_reference design."
        ),
        call. = FALSE
      )
    }
    rqr_validate_desn_design(design)
    if (!is.null(y)) {
      y <- .rqr_desn_response_vector(y, "y")
      if (!identical(y, design$y)) {
        stop("Supplied y conflicts with the frozen design.", call. = FALSE)
      }
    }
  } else {
    if (!is.null(design)) {
      stop(
        "Do not supply design with design_engine='exdqlm_reference'.",
        call. = FALSE
      )
    }
    if (is.null(y)) {
      stop(
        "The exdqlm reference adapter requires y.", call. = FALSE
      )
    }
    y <- .rqr_desn_response_vector(
      y, "y", require_complete = TRUE
    )
    materialized <- .rqr_materialize_exdqlm_desn_design(
      y = y, args = args,
      provenance_control = provenance_control,
      design_metadata = design_metadata
    )
    design <- materialized$design
    provenance_control <- materialized$provenance_control
  }
  if (!isTRUE(fit_readout)) return(design)
  materialization <- .rqr_desn_prepare_materialization(
    design, design_engine, provenance_control
  )
  provenance_control <- materialization$provenance_control
  embedding_contract <- .rqr_desn_embedding_contract(
    design, design_engine, materialization$verification
  )

  {
    beta_prior_obj <- mcmc_args$beta_prior_obj %||% NULL
    prior_construction_fields <- c(
      "beta_prior_type", "beta_rhs", "beta_ridge_tau2",
      "tau2", "beta_gaussian"
    )
    if (!is.null(beta_prior_obj) &&
        any(prior_construction_fields %in% names(mcmc_args))) {
      stop(
        paste(
          "mcmc_args$beta_prior_obj cannot be combined with",
          "coefficient-prior construction fields."
        ),
        call. = FALSE
      )
    }
    if (is.null(beta_prior_obj)) {
      beta_type <- mcmc_args$beta_prior_type %||% "ridge"
      if (!is.character(beta_type) ||
          length(beta_type) != 1L ||
          is.na(beta_type) || !nzchar(beta_type)) {
        stop(
          "mcmc_args$beta_prior_type must be one prior name.",
          call. = FALSE
        )
      }
      beta_type <- tolower(beta_type)
      if (all(c("beta_ridge_tau2", "tau2") %in% names(mcmc_args))) {
        stop(
          paste(
            "mcmc_args cannot supply both beta_ridge_tau2 and",
            "its legacy tau2 alias."
          ),
          call. = FALSE
        )
      }
      active_specific_fields <- switch(
        beta_type,
        ridge = c("beta_ridge_tau2", "tau2"),
        gaussian = "beta_gaussian",
        rhs_ns = "beta_rhs",
        character(0)
      )
      supplied_specific_fields <- intersect(
        c("beta_rhs", "beta_ridge_tau2", "tau2", "beta_gaussian"),
        names(mcmc_args)
      )
      inactive_specific_fields <- setdiff(
        supplied_specific_fields, active_specific_fields
      )
      if (length(inactive_specific_fields)) {
        stop(
          sprintf(
            "Prior controls do not match beta_prior_type='%s': %s.",
            beta_type,
            paste(inactive_specific_fields, collapse = ", ")
          ),
          call. = FALSE
        )
      }
      if (identical(beta_type, "rhs_ns")) {
        intercept <- design$feature_schema$intercept
        if (!isTRUE(intercept$present)) {
          stop(
            "Native RHS-NS DESN readouts require a declared intercept.",
            call. = FALSE
          )
        }
        rhs <- mcmc_args$beta_rhs %||% list()
        rhs$intercept_name <- rhs$intercept_name %||%
          intercept$name
        beta_prior_obj <- rqr_beta_prior(
          "rhs_ns", rhs_ns = rhs
        )
      } else if (identical(beta_type, "ridge")) {
        beta_prior_obj <- rqr_beta_prior(
          "ridge",
          ridge = list(
            tau2 = mcmc_args$beta_ridge_tau2 %||%
              mcmc_args$tau2 %||% 1e4
          )
        )
      } else if (identical(beta_type, "gaussian")) {
        beta_prior_obj <- rqr_beta_prior(
          "gaussian",
          gaussian = mcmc_args$beta_gaussian %||% list()
        )
      } else {
        stop(
          "beta_prior_type must be ridge, gaussian, or rhs_ns.",
          call. = FALSE
        )
      }
    }
    fit <- rqr_mcmc_fit(
      y = design$y, X = design$X,
      coverage_level = coverage_level,
      learning_rate = learning_rate,
      lambda_initial = lambda_initial,
      loss_reference_scale = loss_reference_scale,
      learning_rate_mode = mode,
      lambda_prior = lambda_prior,
      beta_prior_obj = beta_prior_obj,
      numerical_policy = numerical_policy,
      root_swap_probability =
        mcmc_args$root_swap_probability %||% 0.5,
      provenance_control = provenance_control,
      mcmc_control = resolved_mcmc_control,
      init = mcmc_args$init %||% list(),
      embedding_contract = embedding_contract
    )
  }
  .rqr_desn_build_envelope(fit, design)
}

#' Continue a frozen-design ordinary RQR-DESN chain
#'
#' @param object An `rqr_desn_fit` with an MCMC readout.
#' @param ... Arguments passed to [rqr_mcmc_continue()].
#' @return A new `rqr_desn_fit` segment.
#' @export
rqr_desn_continue <- function(object, ...) {
  .rqr_validate_desn_fit_envelope(object)
  fit <- rqr_mcmc_continue(object$fit, ...)
  .rqr_desn_build_envelope(fit, object$design)
}

#' Extract root-coefficient draws from a frozen RQR-DESN fit
#'
#' Draw extraction delegates to the validated fixed-design readout while
#' preserving the frozen-design envelope.
#'
#' @param object An `rqr_desn_fit`.
#' @param nd Number of retained draws to return. `NULL` keeps all draws.
#' @param seed Optional seed used only when draws are subsampled.
#' @param ... Reserved; passed to the fixed-design extraction method.
#' @return A list containing root-coefficient, loss-rate, and draw-index
#'   quantities from the fitted readout.
#' @export
rqr_posterior_draws.rqr_desn_fit <- function(
    object, nd = NULL, seed = NULL, ...) {
  .rqr_validate_desn_fit_envelope(object)
  rqr_posterior_draws(
    object$fit, nd = nd, seed = seed, ...
  )
}

#' Evaluate interval roots from a frozen RQR-DESN fit
#'
#' Prefer a validated [rqr_desn_future_design()] so that feature order,
#' parentage, and evaluation semantics are explicit. The legacy `X_new` route
#' accepts an explicit feature matrix but is labeled non-promotable. Neither
#' route simulates a response.
#'
#' @param object An `rqr_desn_fit`.
#' @param X_new Optional legacy explicit feature matrix in the exact fitted
#'   feature order.
#' @param future_design Preferred validated future-design contract.
#' @param nd Number of retained readout draws to use when `draws` is `NULL`.
#' @param draws Optional output from [rqr_posterior_draws()] for this fit.
#' @param seed Optional seed used only when draws are subsampled.
#' @param ... Reserved; passed to the fixed-design evaluation method.
#' @return A fixed-design interval-root evaluation augmented with frozen-design
#'   parentage, semantics, and eligibility metadata.
#' @export
predict_interval.rqr_desn_fit <- function(
    object, X_new = NULL, future_design = NULL,
    nd = NULL, draws = NULL, seed = NULL, ...) {
  .rqr_validate_desn_fit_envelope(object)
  legacy_matrix <- is.null(future_design)
  if (!is.null(future_design)) {
    if (!is.null(X_new)) {
      stop("Supply future_design or X_new, not both.", call. = FALSE)
    }
    rqr_validate_desn_future_design(
      future_design, parent_design = object$design
    )
    X_new <- future_design$X
  } else if (!is.null(X_new)) {
    X_new <- .rqr_desn_explicit_future_matrix(
      X_new, object$design, "X_new"
    )
  }
  if (is.null(X_new)) {
    stop(
      paste(
        "Supply a validated future_design or an explicit X_new matrix.",
        "No recursive response simulation is available."
      ),
      call. = FALSE
    )
  }
  result <- predict_interval(
    object$fit, X_new = X_new, nd = nd,
    draws = draws, seed = seed, ...
  )
  result$future_design <- future_design
  result$future_semantics <- if (legacy_matrix) {
    "legacy_explicit_matrix"
  } else {
    future_design$semantics
  }
  result$design_parent_digest <- object$design$semantic_digest
  result$future_contract_verified <- !legacy_matrix
  result$legacy_future_matrix <- legacy_matrix
  result$parent_design_materialization_external_binding_verified <-
    isTRUE(
      object$model_spec$
        design_materialization_external_binding_verified
    )
  result$parent_fit_reproducibility_eligible <-
    isTRUE(object$model_spec$reproducibility_eligible)
  result$parent_fit_promotion_eligible <-
    isTRUE(object$model_spec$promotion_eligible)
  # A semantic future-design contract proves alignment, causality
  # declarations, and content integrity. It does not prove how the supplied
  # future feature rows were materialized. In particular, a verified
  # training-design receipt cannot be inherited by arbitrary future rows.
  # A future-specific materialization receipt is deliberately outside v1, so
  # both versioned and legacy future evaluations remain non-promotable.
  result$future_external_provenance_bound <- FALSE
  result$future_reproducibility_eligible <- FALSE
  result$reproducibility_eligible <- FALSE
  result$promotion_eligible <- FALSE
  result$promotion_status <- if (legacy_matrix) {
    "legacy_explicit_matrix_nonpromotable"
  } else {
    "verified_future_contract_unattested_materialization"
  }
  result$response_predictive_draws <- FALSE
  result
}

#' Print a frozen RQR-DESN summary
#'
#' @param x An `rqr_desn_fit`.
#' @param ... Reserved; supplying an argument is an error.
#' @return `x`, invisibly.
#' @export
print.rqr_desn_fit <- function(x, ...) {
  .rqr_reject_dots(list(...), "print.rqr_desn_fit")
  .rqr_validate_desn_fit_envelope(x)
  cat("Ordinary RQR frozen-design DESN fit\n")
  cat(sprintf(
    "  inference:      %s\n",
    x$fit$method %||% x$meta$inference_method
  ))
  cat(sprintf(
    "  coverage_level: %.4f\n",
    x$model_spec$coverage_level
  ))
  cat(sprintf(
    "  rate_mode:      %s\n",
    x$model_spec$learning_rate_mode
  ))
  cat(sprintf(
    "  design:         %d rows x %d features\n",
    nrow(x$design$X), ncol(x$design$X)
  ))
  cat(sprintf(
    "  design engine:  %s\n",
    x$model_spec$design_engine
  ))
  cat(
    "  interpretation: generalized-Bayes interval roots, not response draws\n"
  )
  invisible(x)
}
