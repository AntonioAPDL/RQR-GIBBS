# Frozen DESN feature-design contracts.
#
# These objects bind RQR inference to an already materialized deterministic
# feature design. They do not construct a reservoir and do not define a
# response-simulation distribution.

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

.rqr_desn_design_schema <- function() {
  "rqrgibbs_desn_design/1.1.0"
}

.rqr_desn_future_design_schema <- function() {
  "rqrgibbs_desn_future_design/1.2.0"
}

.rqr_desn_feature_schema <- function() {
  "rqrgibbs_desn_feature_schema/1.0.0"
}

.rqr_desn_materialization_receipt_schema <- function() {
  "rqrgibbs_desn_materialization_receipt/3.0.0"
}

.rqr_desn_materialization_manifest_schema <- function() {
  "rqrgibbs_desn_materialization_manifest/1.0.0"
}

.rqr_desn_reference_builder_id <- function() {
  "exdqlm_qdesn_fit_vb_design_adapter"
}

.rqr_desn_future_verification_schema <- function() {
  "rqrgibbs_desn_future_verification/1.1.0"
}

.rqr_desn_sha256 <- function(x) {
  digest::digest(x, algo = "sha256", serialize = TRUE)
}

.rqr_desn_is_sha256 <- function(x) {
  is.character(x) &&
    length(x) == 1L &&
    !is.na(x) &&
    grepl("^[0-9a-f]{64}$", tolower(x))
}

.rqr_desn_assert_exact_list_object <- function(
    object, expected_class, name) {
  if (!is.list(object) ||
      !identical(class(object), expected_class) ||
      !identical(names(attributes(object)), c("names", "class"))) {
    stop(
      sprintf(
        "%s must have the exact canonical class and attributes.",
        name
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.rqr_desn_assert_named_list <- function(x, name, allow_empty = FALSE) {
  if (!is.list(x) || is.object(x) && !identical(class(x), "list")) {
    stop(sprintf("%s must be a plain named list.", name), call. = FALSE)
  }
  if (!length(x)) {
    if (isTRUE(allow_empty)) return(invisible(TRUE))
    stop(sprintf("%s must not be empty.", name), call. = FALSE)
  }
  nm <- names(x)
  if (is.null(nm) || anyNA(nm) || any(!nzchar(nm)) || anyDuplicated(nm)) {
    stop(sprintf("%s must have unique nonempty names.", name), call. = FALSE)
  }
  invisible(TRUE)
}

.rqr_desn_assert_plain <- function(x, name) {
  .rqr_assert_data_only_contract(x, name)
  visit <- function(value, path) {
    if (is.function(value) ||
        is.environment(value) ||
        typeof(value) %in% c("externalptr", "weakref", "closure", "special", "builtin") ||
        inherits(value, "connection")) {
      stop(
        sprintf("%s contains a non-plain value at %s.", name, path),
        call. = FALSE
      )
    }
    if (is.pairlist(value) || is.language(value)) {
      stop(
        sprintf("%s contains executable language at %s.", name, path),
        call. = FALSE
      )
    }
    if (is.list(value)) {
      nm <- names(value)
      if (!is.null(nm) && (anyNA(nm) || anyDuplicated(nm))) {
        stop(
          sprintf("%s contains invalid or duplicated names at %s.", name, path),
          call. = FALSE
        )
      }
      for (ii in seq_along(value)) {
        child <- if (!is.null(nm) && nzchar(nm[ii])) nm[ii] else as.character(ii)
        visit(value[[ii]], paste0(path, "$", child))
      }
      return(invisible(TRUE))
    }
    if (!is.atomic(value) && !is.null(value)) {
      stop(
        sprintf("%s contains an unsupported value at %s.", name, path),
        call. = FALSE
      )
    }
    invisible(TRUE)
  }
  visit(x, name)
}

.rqr_desn_text_values <- function(x) {
  out <- character(0)
  visit <- function(value) {
    if (is.character(value)) out <<- c(out, value[!is.na(value)])
    if (is.list(value)) {
      for (item in value) visit(item)
    }
    invisible(NULL)
  }
  visit(x)
  out
}

.rqr_desn_reject_response_simulation <- function(x, context) {
  inspect_flags <- function(value) {
    if (!is.list(value)) return(invisible(NULL))
    nm <- tolower(names(value) %||% rep("", length(value)))
    for (ii in seq_along(value)) {
      key <- nm[ii]
      item <- value[[ii]]
      if (grepl(
        "response.*(simulat|draw)|posterior.*predictive.*response",
        key
      ) && isTRUE(item)) {
        stop(
          sprintf("%s cannot enable or authorize response simulation or predictive response draws.", context),
          call. = FALSE
        )
      }
      inspect_flags(item)
    }
    invisible(NULL)
  }
  inspect_flags(x)

  text <- tolower(.rqr_desn_text_values(x))
  forbidden <- paste(
    c(
      "posterior[ _-]*predictive[ _-]*response",
      "response[ _-]*(simulation|draws?)",
      "simulat(e|ed|ing|ion)[ _-]*(a[ _-]*)?(future[ _-]*)?response",
      "pseudo[ _-]*al.{0,30}response"
    ),
    collapse = "|"
  )
  if (length(text) && any(grepl(forbidden, text, perl = TRUE))) {
    stop(
      sprintf(
        "%s contains response-simulation language; frozen DESN designs describe feature drivers only.",
        context
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.rqr_desn_assert_matrix <- function(X, name = "X") {
  if (!is.matrix(X) || !is.numeric(X) ||
      nrow(X) < 1L || ncol(X) < 1L || any(!is.finite(X))) {
    stop(sprintf("%s must be a nonempty finite numeric matrix.", name), call. = FALSE)
  }
  storage.mode(X) <- "double"
  X
}

.rqr_desn_feature_names <- function(X, feature_names = NULL) {
  supplied <- feature_names
  existing <- colnames(X)
  if (is.null(supplied)) supplied <- existing
  if (is.null(supplied)) {
    stop(
      "DESN features require explicit, ordered column names.",
      call. = FALSE
    )
  }
  if (!is.character(supplied) ||
      length(supplied) != ncol(X) || anyNA(supplied) ||
      any(!nzchar(supplied)) || anyDuplicated(supplied)) {
    stop(
      "feature_names must contain one unique nonempty name per design column.",
      call. = FALSE
    )
  }
  if (!is.null(existing) && !identical(as.character(existing), supplied)) {
    stop(
      "feature_names must preserve the existing design-column order.",
      call. = FALSE
    )
  }
  .rqr_desn_reject_response_simulation(supplied, "feature_names")
  supplied
}

.rqr_desn_time_index <- function(time_index, n, name = "time_index") {
  if (!is.numeric(time_index) || length(time_index) != n ||
      anyNA(time_index) || any(!is.finite(time_index)) ||
      any(diff(as.numeric(time_index)) <= 0)) {
    stop(
      sprintf("%s must be finite, strictly increasing, and aligned with the design rows.", name),
      call. = FALSE
    )
  }
  as.numeric(time_index)
}

.rqr_desn_logical_scalar <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop(sprintf("%s must be one nonmissing logical value.", name), call. = FALSE)
  }
  isTRUE(x)
}

.rqr_desn_intercept <- function(intercept, X, feature_names) {
  if (is.null(intercept) || identical(intercept, FALSE)) {
    return(list(
      present = FALSE,
      index = NA_integer_,
      name = NA_character_,
      verified_constant_one = FALSE
    ))
  }
  if (identical(intercept, TRUE)) {
    matches <- which(feature_names == "intercept")
    if (length(matches) != 1L) {
      stop(
        "intercept=TRUE requires exactly one feature named 'intercept'.",
        call. = FALSE
      )
    }
    index <- matches
  } else if (is.character(intercept) && length(intercept) == 1L && !is.na(intercept)) {
    index <- match(intercept, feature_names)
    if (is.na(index)) stop("The declared intercept name is absent from the design.", call. = FALSE)
  } else if (is.numeric(intercept) && length(intercept) == 1L &&
             !is.na(intercept) && is.finite(intercept) &&
             intercept == floor(intercept) &&
             intercept >= 1L && intercept <= ncol(X)) {
    index <- as.integer(intercept)
  } else if (is.list(intercept)) {
    .rqr_validate_named_list_fields(
      intercept, "intercept", c("index", "name")
    )
    if (!length(intercept)) {
      stop("intercept list must provide index or name.", call. = FALSE)
    }
    if (!is.null(intercept$index)) {
      index <- intercept$index
    } else if (!is.null(intercept$name)) {
      if (!is.character(intercept$name) ||
          length(intercept$name) != 1L ||
          is.na(intercept$name) || !nzchar(intercept$name)) {
        stop(
          "intercept$name must be one nonempty feature name.",
          call. = FALSE
        )
      }
      index <- match(intercept$name, feature_names)
    } else {
      stop("intercept list must provide index or name.", call. = FALSE)
    }
    if (!is.numeric(index) || length(index) != 1L || is.na(index) ||
        !is.finite(index) || index != floor(index) ||
        index < 1L || index > ncol(X)) {
      stop("The declared intercept is outside the feature schema.", call. = FALSE)
    }
    index <- as.integer(index)
    if (!is.null(intercept$name) &&
        (!is.character(intercept$name) ||
         length(intercept$name) != 1L ||
         is.na(intercept$name) || !nzchar(intercept$name) ||
         !identical(intercept$name, feature_names[index]))) {
      stop("The declared intercept name and index disagree.", call. = FALSE)
    }
  } else {
    stop("intercept must be NULL, FALSE, TRUE, one name, one index, or a list.", call. = FALSE)
  }
  index <- as.integer(index)
  if (!all(X[, index] == 1)) {
    stop(
      "The declared intercept column must be exactly constant one.",
      call. = FALSE
    )
  }
  exact_one_columns <- which(vapply(
    seq_len(ncol(X)),
    function(column) all(X[, column] == 1),
    logical(1L)
  ))
  if (length(exact_one_columns) != 1L ||
      !identical(exact_one_columns, index)) {
    stop(
      paste(
        "A declared DESN intercept requires exactly one",
        "constant-one design column, at the declared index."
      ),
      call. = FALSE
    )
  }
  list(
    present = TRUE,
    index = index,
    name = feature_names[index],
    verified_constant_one = TRUE
  )
}

.rqr_desn_builder <- function(builder) {
  .rqr_desn_assert_named_list(builder, "builder")
  .rqr_desn_assert_plain(builder, "builder")
  for (field in c("id", "version")) {
    value <- builder[[field]]
    if (!is.character(value) || length(value) != 1L ||
        is.na(value) || !nzchar(value)) {
      stop(sprintf("builder$%s must be one nonempty string.", field), call. = FALSE)
    }
  }
  .rqr_desn_reject_response_simulation(builder, "builder")
  builder
}

.rqr_desn_reservoir <- function(reservoir) {
  .rqr_desn_assert_named_list(reservoir, "reservoir")
  .rqr_desn_assert_plain(reservoir, "reservoir")
  if (!.rqr_desn_is_sha256(reservoir$digest)) {
    stop("reservoir$digest must be a lowercase SHA-256 value.", call. = FALSE)
  }
  reservoir$digest <- tolower(reservoir$digest)
  .rqr_desn_reject_response_simulation(reservoir, "reservoir")
  reservoir
}

.rqr_desn_driver <- function(driver, defaults, context = "driver") {
  if (is.null(driver)) driver <- list()
  .rqr_desn_assert_named_list(driver, context, allow_empty = TRUE)
  .rqr_desn_assert_plain(driver, context)
  driver <- utils::modifyList(defaults, driver, keep.null = TRUE)
  if (!is.character(driver$type) || length(driver$type) != 1L ||
      is.na(driver$type) || !nzchar(driver$type)) {
    stop(sprintf("%s$type must be one nonempty string.", context), call. = FALSE)
  }
  driver$response_simulation <- .rqr_desn_logical_scalar(
    driver$response_simulation %||% FALSE,
    sprintf("%s$response_simulation", context)
  )
  if (isTRUE(driver$response_simulation)) {
    stop(
      sprintf("%s cannot enable response simulation.", context),
      call. = FALSE
    )
  }
  .rqr_desn_reject_response_simulation(driver, context)
  driver
}

.rqr_desn_causal <- function(causal, defaults = list()) {
  if (is.null(causal)) causal <- list()
  .rqr_desn_assert_named_list(causal, "causal", allow_empty = TRUE)
  .rqr_desn_assert_plain(causal, "causal")
  allowed <- c(
    "uses_current_response", "uses_future_response",
    "minimum_response_lag", "prefix_safe", "contract"
  )
  .rqr_validate_named_list_fields(causal, "causal", allowed)
  .rqr_validate_named_list_fields(defaults, "causal defaults", allowed)
  causal <- utils::modifyList(defaults, causal, keep.null = TRUE)
  causal <- utils::modifyList(list(
    uses_current_response = FALSE,
    uses_future_response = FALSE,
    minimum_response_lag = 1L,
    prefix_safe = TRUE,
    contract =
      "row_t_uses_only_response_information_strictly_before_t"
  ), causal, keep.null = TRUE)
  causal$uses_current_response <- .rqr_desn_logical_scalar(
    causal$uses_current_response, "causal$uses_current_response"
  )
  causal$uses_future_response <- .rqr_desn_logical_scalar(
    causal$uses_future_response, "causal$uses_future_response"
  )
  causal$prefix_safe <- .rqr_desn_logical_scalar(
    causal$prefix_safe, "causal$prefix_safe"
  )
  lag <- causal$minimum_response_lag
  if (!is.numeric(lag) || length(lag) != 1L ||
      is.na(lag) || !is.finite(lag) ||
      lag != floor(lag) || lag < 1L ||
      lag > .Machine$integer.max) {
    stop(
      "causal$minimum_response_lag must be one positive integer.",
      call. = FALSE
    )
  }
  causal$minimum_response_lag <- as.integer(lag)
  expected_contract <-
    "row_t_uses_only_response_information_strictly_before_t"
  if (!is.character(causal$contract) ||
      length(causal$contract) != 1L ||
      is.na(causal$contract) ||
      !identical(causal$contract, expected_contract)) {
    stop(
      "causal$contract must state the canonical strict-prefix contract.",
      call. = FALSE
    )
  }
  if (isTRUE(causal$uses_current_response) ||
      isTRUE(causal$uses_future_response) ||
      !isTRUE(causal$prefix_safe)) {
    stop(
      paste(
        "A frozen DESN design must be prefix-safe and cannot use",
        "the current or a future response."
      ),
      call. = FALSE
    )
  }
  .rqr_desn_reject_response_simulation(causal, "causal")
  causal[c(
    "uses_current_response", "uses_future_response",
    "minimum_response_lag", "prefix_safe", "contract"
  )]
}

.rqr_desn_time_metadata <- function(time, time_index, role, origin_time = NULL) {
  if (is.null(time)) time <- list()
  .rqr_desn_assert_named_list(time, "time", allow_empty = TRUE)
  .rqr_desn_assert_plain(time, "time")
  derived <- list(
    role = role,
    start = time_index[1L],
    end = time_index[length(time_index)],
    n_time = length(time_index),
    strictly_increasing = TRUE
  )
  if (!is.null(origin_time)) derived$origin_time <- as.numeric(origin_time)[1L]
  .rqr_desn_reject_response_simulation(time, "time")
  protected <- intersect(names(time), names(derived))
  if (length(protected)) {
    for (field in protected) {
      if (!identical(time[[field]], derived[[field]])) {
        stop(sprintf("time$%s conflicts with the aligned time index.", field), call. = FALSE)
      }
    }
  }
  utils::modifyList(time, derived, keep.null = TRUE)
}

.rqr_desn_terminal <- function(terminal) {
  if (is.null(terminal)) terminal <- list(available = FALSE)
  .rqr_desn_assert_named_list(terminal, "terminal")
  .rqr_desn_assert_plain(terminal, "terminal")
  available <- terminal$available
  if (is.null(available)) {
    available <- !is.null(terminal$state_digest) || !is.null(terminal$lag_buffer_digest)
  }
  terminal$available <- .rqr_desn_logical_scalar(available, "terminal$available")
  if (isTRUE(terminal$available)) {
    for (field in c("state_digest", "lag_buffer_digest")) {
      if (!.rqr_desn_is_sha256(terminal[[field]])) {
        stop(
          sprintf("terminal$%s must be SHA-256 when terminal metadata are available.", field),
          call. = FALSE
        )
      }
      terminal[[field]] <- tolower(terminal[[field]])
    }
  } else if (!is.null(terminal$state_digest) || !is.null(terminal$lag_buffer_digest)) {
    stop("Unavailable terminal metadata cannot contain state or lag-buffer digests.", call. = FALSE)
  }
  .rqr_desn_reject_response_simulation(terminal, "terminal")
  terminal
}

.rqr_desn_design_payload <- function(object) {
  list(
    schema_version = object$schema_version,
    X = object$X,
    y = object$y,
    time_index = object$time_index,
    feature_schema = object$feature_schema,
    builder = object$builder,
    reservoir = object$reservoir,
    driver = object$driver,
    causal = object$causal,
    time = object$time,
    terminal = object$terminal
  )
}

.rqr_desn_design_digests <- function(payload) {
  components <- lapply(payload[-1L], .rqr_desn_sha256)
  components$semantic <- .rqr_desn_sha256(payload)
  components
}

.rqr_desn_materialization_payload <- function(object) {
  builder <- object$builder
  builder$materialization_receipt <- NULL
  list(
    schema_version = object$schema_version,
    X = object$X,
    y = object$y,
    time_index = object$time_index,
    feature_schema = object$feature_schema,
    builder = builder,
    reservoir = object$reservoir,
    driver = object$driver,
    causal = object$causal,
    time = object$time,
    terminal = object$terminal
  )
}

.rqr_desn_materialization_manifest <- function(
    source_response, keep_idx, X, y_fit, feature_names,
    reservoir_digest) {
  if (!is.numeric(source_response) || !is.null(dim(source_response)) ||
      !length(source_response) || anyNA(source_response) ||
      any(!is.finite(source_response))) {
    stop(
      "The materialization source response must be complete and finite.",
      call. = FALSE
    )
  }
  source_response <- as.numeric(source_response)
  if (!is.integer(keep_idx) || !length(keep_idx) ||
      anyNA(keep_idx) || any(keep_idx < 1L) ||
      any(keep_idx > length(source_response)) ||
      any(diff(keep_idx) <= 0L)) {
    stop(
      "The materialization keep_idx must be a strictly increasing integer subset of the source response.",
      call. = FALSE
    )
  }
  X <- .rqr_desn_assert_matrix(X, "materialized X")
  y_fit <- as.numeric(y_fit)
  if (nrow(X) != length(keep_idx) ||
      length(y_fit) != length(keep_idx) ||
      !identical(y_fit, source_response[keep_idx])) {
    stop(
      paste(
        "The materialized rows, aligned response, and actual keep_idx",
        "must identify the same source-response subset."
      ),
      call. = FALSE
    )
  }
  if (!identical(feature_names, colnames(X))) {
    stop(
      "The materialization feature names must exactly match X.",
      call. = FALSE
    )
  }
  if (!.rqr_desn_is_sha256(reservoir_digest)) {
    stop(
      "The materialization reservoir digest must be SHA-256.",
      call. = FALSE
    )
  }
  list(
    schema_version =
      .rqr_desn_materialization_manifest_schema(),
    source_response_digest =
      .rqr_desn_sha256(source_response),
    source_response_length = as.integer(length(source_response)),
    keep_idx = keep_idx,
    keep_idx_digest = .rqr_desn_sha256(keep_idx),
    design_matrix_digest = .rqr_desn_sha256(X),
    aligned_response_digest = .rqr_desn_sha256(y_fit),
    feature_names_digest = .rqr_desn_sha256(feature_names),
    reservoir_digest = tolower(reservoir_digest)
  )
}

.rqr_desn_validate_materialization_manifest <- function(
    manifest, object = NULL) {
  expected_fields <- c(
    "schema_version", "source_response_digest",
    "source_response_length", "keep_idx", "keep_idx_digest",
    "design_matrix_digest", "aligned_response_digest",
    "feature_names_digest", "reservoir_digest"
  )
  if (!is.list(manifest) || is.object(manifest) ||
      !identical(names(manifest), expected_fields) ||
      !identical(
        manifest$schema_version,
        .rqr_desn_materialization_manifest_schema()
      ) ||
      !all(vapply(
        manifest[c(
          "source_response_digest", "keep_idx_digest",
          "design_matrix_digest", "aligned_response_digest",
          "feature_names_digest", "reservoir_digest"
        )],
        .rqr_desn_is_sha256, logical(1L)
      ))) {
    stop(
      "The DESN materialization manifest is malformed.",
      call. = FALSE
    )
  }
  n_source <- manifest$source_response_length
  keep_idx <- manifest$keep_idx
  if (!is.integer(n_source) || length(n_source) != 1L ||
      is.na(n_source) || n_source < 1L ||
      !is.integer(keep_idx) || !length(keep_idx) ||
      anyNA(keep_idx) || any(keep_idx < 1L) ||
      any(keep_idx > n_source) || any(diff(keep_idx) <= 0L) ||
      !identical(
        manifest$keep_idx_digest,
        .rqr_desn_sha256(keep_idx)
      )) {
    stop(
      "The DESN materialization manifest has invalid source or keep-index metadata.",
      call. = FALSE
    )
  }
  if (!is.null(object)) {
    expected <- c(
      design_matrix_digest = .rqr_desn_sha256(object$X),
      aligned_response_digest = .rqr_desn_sha256(object$y),
      feature_names_digest = .rqr_desn_sha256(
        object$feature_schema$feature_names
      ),
      reservoir_digest = object$reservoir$digest
    )
    if (!identical(as.numeric(object$time_index), as.numeric(keep_idx)) ||
        length(keep_idx) != nrow(object$X) ||
        any(!vapply(
          names(expected),
          function(field) identical(
            manifest[[field]], unname(expected[[field]])
          ),
          logical(1L)
        ))) {
      stop(
        paste(
          "The DESN materialization manifest does not bind the",
          "stored design payload and actual keep_idx."
        ),
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}

.rqr_desn_materialization_receipt_status <- function(object) {
  reference_materializer <- is.list(object$builder) &&
    identical(
      object$builder$id,
      .rqr_desn_reference_builder_id()
    )
  receipt <- if (reference_materializer) {
    object$builder$materialization_receipt %||% NULL
  } else {
    NULL
  }
  receipt_digest <- if (is.list(receipt)) {
    .rqr_desn_sha256(receipt)
  } else {
    NA_character_
  }
  required <- c(
    "schema_version", "package", "package_version",
    "source_commit", "source_tree_digest", "runtime_tree_digest",
    "runtime_attestation_schema", "runtime_attestation_sha256",
    "materializer_arguments_digest", "materialized_design_payload_digest",
    "source_response_digest", "source_response_length",
    "keep_idx_digest", "materialization_manifest_digest",
    "runtime_source_match", "reproducibility_eligible"
  )
  sha_fields <- c(
    "runtime_tree_digest",
    "runtime_attestation_sha256", "materializer_arguments_digest",
    "materialized_design_payload_digest", "source_response_digest",
    "keep_idx_digest", "materialization_manifest_digest"
  )
  payload_digest <- .rqr_desn_sha256(
    .rqr_desn_materialization_payload(object)
  )
  manifest <- object$builder$materialization_manifest %||% NULL
  manifest_valid <- tryCatch({
    .rqr_desn_validate_materialization_manifest(
      manifest, object = object
    )
    TRUE
  }, error = function(error) FALSE)
  valid <- reference_materializer &&
    is.list(receipt) &&
    !is.object(receipt) &&
    identical(names(receipt), required) &&
    identical(
      receipt$schema_version,
      .rqr_desn_materialization_receipt_schema()
    ) &&
    identical(receipt$package, "exdqlm") &&
    is.character(receipt$package_version) &&
    length(receipt$package_version) == 1L &&
    !is.na(receipt$package_version) &&
    nzchar(receipt$package_version) &&
    identical(
      receipt$source_commit,
      .rqr_pinned_exdqlm_commit()
    ) &&
    is.character(receipt$source_tree_digest) &&
    length(receipt$source_tree_digest) == 1L &&
    !is.na(receipt$source_tree_digest) &&
    grepl(
      "^(?:[0-9a-f]{40}|[0-9a-f]{64})$",
      tolower(receipt$source_tree_digest)
    ) &&
    all(vapply(
      sha_fields,
      function(field) .rqr_desn_is_sha256(receipt[[field]]),
      logical(1L)
    )) &&
    is.integer(receipt$source_response_length) &&
    length(receipt$source_response_length) == 1L &&
    !is.na(receipt$source_response_length) &&
    receipt$source_response_length >= 1L &&
    manifest_valid &&
    identical(
      receipt$source_response_digest,
      manifest$source_response_digest
    ) &&
    identical(
      receipt$source_response_length,
      manifest$source_response_length
    ) &&
    identical(
      receipt$keep_idx_digest,
      manifest$keep_idx_digest
    ) &&
    identical(
      receipt$materialization_manifest_digest,
      .rqr_desn_sha256(manifest)
    ) &&
    is.character(receipt$runtime_attestation_schema) &&
    length(receipt$runtime_attestation_schema) == 1L &&
    !is.na(receipt$runtime_attestation_schema) &&
    nzchar(receipt$runtime_attestation_schema) &&
    isTRUE(receipt$runtime_source_match) &&
    isTRUE(receipt$reproducibility_eligible) &&
    identical(
      object$builder$version,
      receipt$package_version
    ) &&
    identical(
      object$builder$source_commit,
      receipt$source_commit
    ) &&
    identical(
      object$builder$arguments_digest,
      receipt$materializer_arguments_digest
    ) &&
    identical(
      object$builder$adapter,
      "rqrgibbs_frozen_design_materializer/2.0.0"
    ) &&
    identical(object$reservoir$source_package, "exdqlm") &&
    identical(
      object$reservoir$source_commit,
      receipt$source_commit
    ) &&
    identical(
      receipt$materialized_design_payload_digest,
      payload_digest
    )
  list(
    schema_version =
      "rqrgibbs_desn_materialization_receipt_status/1.1.0",
    reference_materializer = reference_materializer,
    receipt_valid = isTRUE(valid),
    receipt_digest = receipt_digest,
    materialized_design_payload_digest = payload_digest
  )
}

#' Construct a frozen RQR-DESN feature-design contract
#'
#' This constructor records an already materialized deterministic DESN feature
#' design. It does not generate reservoir states and does not authorize
#' response simulation.
#'
#' @param X Finite numeric feature matrix.
#' @param y Numeric response vector aligned with the rows of `X`. `NA` entries
#'   are allowed, but at least one response must be observed.
#' @param time_index Strictly increasing numeric index aligned with `X`.
#' @param feature_names Unique ordered feature names.
#' @param intercept Optional verified constant-one intercept declaration.
#' @param builder Plain metadata identifying the feature builder and version.
#' @param reservoir Plain reservoir metadata with a SHA-256 `digest`.
#' @param driver Plain training-history driver metadata.
#' @param causal Plain causal-contract metadata.
#' @param time Additional plain time metadata.
#' @param terminal Optional terminal-state and lag-buffer digest metadata.
#' @return A versioned `rqr_desn_design` object.
#' @export
rqr_desn_design <- function(
    X, y, time_index = seq_len(nrow(as.matrix(X))),
    feature_names = colnames(as.matrix(X)), intercept = NULL,
    builder, reservoir,
    driver = list(type = "observed_history", response_simulation = FALSE),
    causal = list(
      uses_current_response = FALSE,
      uses_future_response = FALSE
    ),
    time = list(),
    terminal = list(available = FALSE)) {
  X <- .rqr_desn_assert_matrix(X)
  feature_names <- .rqr_desn_feature_names(X, feature_names)
  colnames(X) <- feature_names
  if (!is.numeric(y) || !is.null(dim(y))) {
    stop("y must be a numeric vector.", call. = FALSE)
  }
  y <- as.numeric(y)
  if (length(y) != nrow(X) || any(is.nan(y)) ||
      any(is.infinite(y)) || all(is.na(y))) {
    stop(
      paste(
        "y must be aligned with the design rows, contain at least one",
        "observed value, and contain neither NaN nor infinity."
      ),
      call. = FALSE
    )
  }
  time_index <- .rqr_desn_time_index(time_index, nrow(X))
  intercept_contract <- .rqr_desn_intercept(intercept, X, feature_names)
  builder <- .rqr_desn_builder(builder)
  reservoir <- .rqr_desn_reservoir(reservoir)
  driver <- .rqr_desn_driver(
    driver,
    defaults = list(type = "observed_history", response_simulation = FALSE),
    context = "driver"
  )
  causal <- .rqr_desn_causal(causal)
  time <- .rqr_desn_time_metadata(time, time_index, role = "training")
  terminal <- .rqr_desn_terminal(terminal)

  feature_schema <- list(
    schema_version = .rqr_desn_feature_schema(),
    n_features = ncol(X),
    feature_names = feature_names,
    intercept = intercept_contract
  )
  out <- list(
    schema_version = .rqr_desn_design_schema(),
    X = X,
    y = y,
    time_index = time_index,
    feature_schema = feature_schema,
    builder = builder,
    reservoir = reservoir,
    driver = driver,
    causal = causal,
    time = time,
    terminal = terminal
  )
  payload <- .rqr_desn_design_payload(out)
  out$digests <- .rqr_desn_design_digests(payload)
  out$semantic_digest <- out$digests$semantic
  class(out) <- c("rqr_desn_design", "list")
  rqr_validate_desn_design(out)
  out
}

#' Validate a frozen RQR-DESN design contract
#'
#' @param object An `rqr_desn_design` object.
#' @return `TRUE` invisibly; errors on semantic or digest mismatch.
#' @export
rqr_validate_desn_design <- function(object) {
  .rqr_desn_assert_exact_list_object(
    object, c("rqr_desn_design", "list"),
    "RQR-DESN design"
  )
  expected_fields <- c(
    "schema_version", "X", "y", "time_index", "feature_schema",
    "builder", "reservoir", "driver", "causal", "time", "terminal",
    "digests", "semantic_digest"
  )
  if (!identical(names(object), expected_fields)) {
    stop(
      "The RQR-DESN design has noncanonical top-level fields.",
      call. = FALSE
    )
  }
  if (!identical(object$schema_version, .rqr_desn_design_schema())) {
    stop("Unsupported or altered RQR-DESN design schema.", call. = FALSE)
  }
  if (!is.list(object$feature_schema) ||
      is.object(object$feature_schema) ||
      !identical(
        names(object$feature_schema),
        c(
          "schema_version", "n_features", "feature_names",
          "intercept"
        )
      ) ||
      !is.list(object$feature_schema$intercept) ||
      is.object(object$feature_schema$intercept) ||
      !identical(
        names(object$feature_schema$intercept),
        c("present", "index", "name", "verified_constant_one")
      )) {
    stop(
      "Stored DESN feature/intercept schemas are noncanonical.",
      call. = FALSE
    )
  }
  X <- .rqr_desn_assert_matrix(object$X)
  names_now <- .rqr_desn_feature_names(X, object$feature_schema$feature_names)
  if (!identical(colnames(X), names_now)) {
    stop("Stored feature names do not match the design columns.", call. = FALSE)
  }
  if (!identical(object$feature_schema$schema_version, .rqr_desn_feature_schema()) ||
      !identical(object$feature_schema$n_features, ncol(X))) {
    stop("Stored DESN feature schema is inconsistent with X.", call. = FALSE)
  }
  if (!is.numeric(object$y) || !is.null(dim(object$y))) {
    stop("Stored y must be a numeric vector.", call. = FALSE)
  }
  y <- as.numeric(object$y)
  if (length(y) != nrow(X) || any(is.nan(y)) ||
      any(is.infinite(y)) || all(is.na(y))) {
    stop(
      "Stored y is not a valid observed-mask response aligned with X.",
      call. = FALSE
    )
  }
  time_index <- .rqr_desn_time_index(object$time_index, nrow(X))

  intercept <- object$feature_schema$intercept
  if (!is.list(intercept) || !is.logical(intercept$present) ||
      length(intercept$present) != 1L || is.na(intercept$present)) {
    stop("Stored intercept metadata are invalid.", call. = FALSE)
  }
  if (isTRUE(intercept$present)) {
    expected <- .rqr_desn_intercept(
      list(index = intercept$index, name = intercept$name),
      X,
      names_now
    )
    if (!identical(intercept, expected)) {
      stop("Stored intercept metadata do not match the verified column.", call. = FALSE)
    }
  } else {
    expected <- .rqr_desn_intercept(NULL, X, names_now)
    if (!identical(intercept, expected)) {
      stop("Stored absent-intercept metadata are inconsistent.", call. = FALSE)
    }
  }

  expected_builder <- .rqr_desn_builder(object$builder)
  expected_reservoir <- .rqr_desn_reservoir(object$reservoir)
  expected_driver <- .rqr_desn_driver(
    object$driver,
    defaults = list(type = "observed_history", response_simulation = FALSE),
    context = "driver"
  )
  expected_causal <- .rqr_desn_causal(object$causal)
  if (!identical(object$builder, expected_builder) ||
      !identical(object$reservoir, expected_reservoir) ||
      !identical(object$driver, expected_driver) ||
      !identical(object$causal, expected_causal)) {
    stop("Stored DESN metadata are not in canonical contract form.", call. = FALSE)
  }
  if (identical(
        object$builder$id,
        .rqr_desn_reference_builder_id()
      )) {
    .rqr_desn_validate_materialization_manifest(
      object$builder$materialization_manifest %||% NULL,
      object = object
    )
  }
  expected_time <- .rqr_desn_time_metadata(
    object$time,
    time_index,
    role = "training"
  )
  if (!identical(object$time, expected_time)) {
    stop("Stored training-time metadata are inconsistent.", call. = FALSE)
  }
  expected_terminal <- .rqr_desn_terminal(object$terminal)
  if (!identical(object$terminal, expected_terminal)) {
    stop("Stored terminal metadata are inconsistent.", call. = FALSE)
  }

  payload <- .rqr_desn_design_payload(object)
  expected_digests <- .rqr_desn_design_digests(payload)
  if (!is.list(object$digests) || is.object(object$digests) ||
      !identical(names(object$digests), names(expected_digests)) ||
      !identical(object$digests, expected_digests) ||
      !identical(object$semantic_digest, expected_digests$semantic)) {
    stop("RQR-DESN design semantic digest mismatch.", call. = FALSE)
  }
  invisible(TRUE)
}

.rqr_desn_future_driver <- function(semantics, driver) {
  if (is.null(driver)) driver <- list()
  .rqr_desn_assert_named_list(
    driver, "future driver", allow_empty = TRUE
  )
  .rqr_desn_assert_plain(driver, "future driver")
  .rqr_desn_reject_response_simulation(
    driver, "future driver"
  )
  allowed_supplied <- c(
    "type", "origin_fixed",
    "uses_realized_post_origin_history", "evaluation_mode",
    "path_digest", "source", "generator_id",
    "construction_digest", "response_simulation"
  )
  .rqr_validate_named_list_fields(
    driver, "future driver", allowed_supplied
  )
  defaults <- switch(
    semantics,
    precomputed_design = list(
      type = "precomputed_feature_design",
      origin_fixed = TRUE,
      uses_realized_post_origin_history = FALSE,
      evaluation_mode = "origin_fixed",
      path_digest = NA_character_,
      source = "explicit_precomputed_feature_matrix",
      generator_id = NA_character_,
      construction_digest = NA_character_,
      response_simulation = FALSE
    ),
    teacher_forced_one_step = list(
      type = "observed_history",
      origin_fixed = FALSE,
      uses_realized_post_origin_history = TRUE,
      evaluation_mode = "rolling_one_step",
      path_digest = NA_character_,
      source = "realized_lagged_response_history",
      generator_id = NA_character_,
      construction_digest = NA_character_,
      response_simulation = FALSE
    ),
    external_driver_path = list(
      type = "external_driver_path",
      origin_fixed = TRUE,
      uses_realized_post_origin_history = FALSE,
      evaluation_mode = "origin_fixed_external_path",
      path_digest = NA_character_,
      source = "external_driver",
      generator_id = NA_character_,
      construction_digest = NA_character_,
      response_simulation = FALSE
    )
  )
  driver <- .rqr_desn_driver(driver, defaults, context = "future driver")
  driver$origin_fixed <- .rqr_desn_logical_scalar(
    driver$origin_fixed, "future driver$origin_fixed"
  )
  driver$uses_realized_post_origin_history <- .rqr_desn_logical_scalar(
    driver$uses_realized_post_origin_history,
    "future driver$uses_realized_post_origin_history"
  )
  if (isTRUE(driver$origin_fixed) &&
      isTRUE(driver$uses_realized_post_origin_history)) {
    stop(
      "An origin-fixed future design cannot use realized post-origin responses.",
      call. = FALSE
    )
  }
  expected_type <- switch(
    semantics,
    precomputed_design = "precomputed_feature_design",
    teacher_forced_one_step = "observed_history",
    external_driver_path = "external_driver_path"
  )
  expected_mode <- switch(
    semantics,
    precomputed_design = "origin_fixed",
    teacher_forced_one_step = "rolling_one_step",
    external_driver_path = "origin_fixed_external_path"
  )
  if (!identical(driver$type, expected_type) ||
      !identical(driver$evaluation_mode, expected_mode)) {
    stop(
      sprintf(
        "Future-driver type/evaluation_mode must exactly match %s semantics.",
        semantics
      ),
      call. = FALSE
    )
  }
  scalar_text <- function(value, name, allow_na = FALSE) {
    valid_na <- allow_na && is.character(value) &&
      length(value) == 1L && is.na(value)
    if (!valid_na &&
        (!is.character(value) || length(value) != 1L ||
          is.na(value) || !nzchar(value))) {
      stop(
        sprintf("future driver$%s is malformed.", name),
        call. = FALSE
      )
    }
    invisible(TRUE)
  }
  scalar_text(driver$source, "source")
  scalar_text(driver$generator_id, "generator_id", allow_na = TRUE)
  if (!(is.character(driver$path_digest) &&
      length(driver$path_digest) == 1L &&
      (is.na(driver$path_digest) ||
        .rqr_desn_is_sha256(driver$path_digest)))) {
    stop(
      "future driver$path_digest must be SHA-256 or NA.",
      call. = FALSE
    )
  }
  if (!(is.character(driver$construction_digest) &&
      length(driver$construction_digest) == 1L &&
      (is.na(driver$construction_digest) ||
        .rqr_desn_is_sha256(driver$construction_digest)))) {
    stop(
      "future driver$construction_digest must be SHA-256 or NA.",
      call. = FALSE
    )
  }
  if (identical(semantics, "precomputed_design")) {
    if (!isTRUE(driver$origin_fixed) ||
        isTRUE(driver$uses_realized_post_origin_history) ||
        !is.na(driver$path_digest) ||
        !is.na(driver$generator_id)) {
      stop(
        paste(
          "precomputed_design must be fixed at the origin and",
          "independent of realized post-origin responses."
        ),
        call. = FALSE
      )
    }
  }
  if (identical(semantics, "teacher_forced_one_step")) {
    if (isTRUE(driver$origin_fixed) ||
        !isTRUE(driver$uses_realized_post_origin_history) ||
        !identical(driver$evaluation_mode, "rolling_one_step") ||
        !is.na(driver$generator_id) ||
        !is.na(driver$construction_digest)) {
      stop(
        "teacher_forced_one_step must be rolling one-step evaluation using realized post-origin history.",
        call. = FALSE
      )
    }
  }
  if (identical(semantics, "external_driver_path")) {
    if (!isTRUE(driver$origin_fixed) ||
        isTRUE(driver$uses_realized_post_origin_history) ||
        !is.na(driver$construction_digest)) {
      stop(
        "external_driver_path must be fixed at the origin and independent of realized post-origin responses.",
        call. = FALSE
      )
    }
  }
  if (semantics %in% c("teacher_forced_one_step", "external_driver_path") &&
      !.rqr_desn_is_sha256(driver$path_digest)) {
    stop(
      sprintf("%s requires future driver$path_digest.", semantics),
      call. = FALSE
    )
  }
  if (identical(semantics, "external_driver_path") &&
      (!is.character(driver$generator_id) ||
        length(driver$generator_id) != 1L ||
        is.na(driver$generator_id) ||
        !nzchar(driver$generator_id))) {
    stop(
      "external_driver_path requires one nonempty generator_id.",
      call. = FALSE
    )
  }
  if (!is.na(driver$path_digest)) {
    driver$path_digest <- tolower(driver$path_digest)
  }
  driver[c(
    "type", "origin_fixed", "uses_realized_post_origin_history",
    "evaluation_mode", "path_digest", "source", "generator_id",
    "construction_digest", "response_simulation"
  )]
}

.rqr_desn_future_payload <- function(object) {
  list(
    schema_version = object$schema_version,
    parent = object$parent,
    verification = object$verification,
    semantics = object$semantics,
    X = object$X,
    time_index = object$time_index,
    feature_schema = object$feature_schema,
    reservoir = object$reservoir,
    driver = object$driver,
    causal = object$causal,
    time = object$time,
    terminal = object$terminal
  )
}

.rqr_desn_future_verification <- function(parent_design) {
  receipt <- .rqr_desn_materialization_receipt_status(parent_design)
  list(
    schema_version = .rqr_desn_future_verification_schema(),
    contract_verified = TRUE,
    legacy_explicit_matrix = FALSE,
    parent_materialization_receipt_valid =
      receipt$receipt_valid,
    parent_materialization_receipt_digest =
      receipt$receipt_digest,
    external_provenance_bound = FALSE,
    promotion_evidence_complete = FALSE,
    promotion_eligible = FALSE,
    promotion_status =
      "requires_verified_parent_fit_provenance"
  )
}

.rqr_desn_explicit_future_matrix <- function(
    X, parent_design, name = "X_future") {
  rqr_validate_desn_design(parent_design)
  supplied_names <- colnames(X)
  X <- .rqr_desn_assert_matrix(X, name)
  expected_names <- parent_design$feature_schema$feature_names
  if (is.null(supplied_names) ||
      !identical(as.character(supplied_names), expected_names)) {
    stop(
      sprintf(
        "%s must carry the exact parent DESN feature names and order.",
        name
      ),
      call. = FALSE
    )
  }
  colnames(X) <- supplied_names
  intercept <- parent_design$feature_schema$intercept
  if (isTRUE(intercept$present)) {
    .rqr_desn_intercept(
      list(index = intercept$index, name = intercept$name),
      X,
      expected_names
    )
  }
  X
}

.rqr_desn_future_digests <- function(payload) {
  components <- lapply(payload[-1L], .rqr_desn_sha256)
  components$semantic <- .rqr_desn_sha256(payload)
  components
}

#' Construct a future frozen RQR-DESN feature-design contract
#'
#' Future features may be precomputed, teacher-forced for rolling one-step
#' evaluation, or conditional on a separately supplied external driver path.
#' No option creates future response draws.
#'
#' @param parent_design Valid training `rqr_desn_design`.
#' @param X Finite future feature matrix.
#' @param time_index Strictly increasing future time index.
#' @param semantics Future-design semantics.
#' @param feature_names Ordered feature names; must match the parent.
#' @param reservoir Reservoir metadata; must match the parent digest.
#' @param driver Future driver metadata.
#' @param causal Rowwise causal metadata.
#' @param time Additional future time metadata.
#' @param terminal Optional terminal metadata after future feature construction.
#' @param origin_time Forecast origin; must equal the last parent time.
#' @return A versioned `rqr_desn_future_design` object.
#' @export
rqr_desn_future_design <- function(
    parent_design, X, time_index,
    semantics = c(
      "precomputed_design",
      "teacher_forced_one_step",
      "external_driver_path"
    ),
    feature_names = colnames(as.matrix(X)),
    reservoir = parent_design$reservoir,
    driver = list(),
    causal = list(
      uses_current_response = FALSE,
      uses_future_response = FALSE
    ),
    time = list(),
    terminal = list(available = FALSE),
    origin_time = max(parent_design$time_index)) {
  rqr_validate_desn_design(parent_design)
  semantics <- match.arg(semantics)
  X <- .rqr_desn_assert_matrix(X, "future X")
  if (is.null(feature_names) && is.null(colnames(X))) {
    feature_names <- parent_design$feature_schema$feature_names
  }
  feature_names <- .rqr_desn_feature_names(X, feature_names)
  if (!identical(feature_names, parent_design$feature_schema$feature_names)) {
    stop(
      "Future feature names and order must match the parent DESN schema.",
      call. = FALSE
    )
  }
  colnames(X) <- feature_names
  parent_intercept <- parent_design$feature_schema$intercept
  if (isTRUE(parent_intercept$present)) {
    future_intercept <- .rqr_desn_intercept(
      list(index = parent_intercept$index, name = parent_intercept$name),
      X,
      feature_names
    )
    if (!identical(future_intercept, parent_intercept)) {
      stop("Future intercept metadata must match the parent schema.", call. = FALSE)
    }
  }
  reservoir <- .rqr_desn_reservoir(reservoir)
  if (!identical(reservoir, parent_design$reservoir)) {
    stop(
      paste(
        "Future reservoir digest and metadata must exactly match",
        "the parent design."
      ),
      call. = FALSE
    )
  }
  time_index <- .rqr_desn_time_index(time_index, nrow(X), "future time_index")
  if (!is.numeric(origin_time) || length(origin_time) != 1L ||
      is.na(origin_time) || !is.finite(origin_time) ||
      !identical(as.numeric(origin_time), max(parent_design$time_index))) {
    stop("origin_time must equal the last parent-design time.", call. = FALSE)
  }
  origin_time <- as.numeric(origin_time)
  if (any(time_index <= origin_time)) {
    stop("Every future time must be strictly after origin_time.", call. = FALSE)
  }
  driver <- .rqr_desn_future_driver(semantics, driver)
  causal <- .rqr_desn_causal(causal)
  time <- .rqr_desn_time_metadata(
    time, time_index, role = "future", origin_time = origin_time
  )
  terminal <- .rqr_desn_terminal(terminal)

  parent <- list(
    semantic_digest = parent_design$semantic_digest,
    feature_schema_digest = parent_design$digests$feature_schema,
    reservoir_digest = parent_design$reservoir$digest,
    terminal_digest = parent_design$digests$terminal
  )
  out <- list(
    schema_version = .rqr_desn_future_design_schema(),
    parent = parent,
    verification = .rqr_desn_future_verification(parent_design),
    semantics = semantics,
    X = X,
    time_index = time_index,
    feature_schema = parent_design$feature_schema,
    reservoir = reservoir,
    driver = driver,
    causal = causal,
    time = time,
    terminal = terminal
  )
  payload <- .rqr_desn_future_payload(out)
  out$digests <- .rqr_desn_future_digests(payload)
  out$semantic_digest <- out$digests$semantic
  class(out) <- c("rqr_desn_future_design", "list")
  rqr_validate_desn_future_design(out, parent_design = parent_design)
  out
}

#' Validate a future frozen RQR-DESN design contract
#'
#' @param object An `rqr_desn_future_design` object.
#' @param parent_design Optional parent contract for full link validation.
#' @return `TRUE` invisibly; errors on semantic, linkage, or digest mismatch.
#' @export
rqr_validate_desn_future_design <- function(object, parent_design = NULL) {
  .rqr_desn_assert_exact_list_object(
    object, c("rqr_desn_future_design", "list"),
    "future RQR-DESN design"
  )
  expected_fields <- c(
    "schema_version", "parent", "verification", "semantics", "X",
    "time_index", "feature_schema", "reservoir", "driver", "causal",
    "time", "terminal", "digests", "semantic_digest"
  )
  if (!identical(names(object), expected_fields)) {
    stop(
      "The future RQR-DESN design has noncanonical top-level fields.",
      call. = FALSE
    )
  }
  if (!identical(object$schema_version, .rqr_desn_future_design_schema())) {
    stop("Unsupported or altered future RQR-DESN design schema.", call. = FALSE)
  }
  if (!is.character(object$semantics) ||
      length(object$semantics) != 1L ||
      is.na(object$semantics) ||
      !object$semantics %in% c(
        "precomputed_design", "teacher_forced_one_step",
        "external_driver_path"
      )) {
    stop("Unsupported or altered future-design semantics.", call. = FALSE)
  }
  X <- .rqr_desn_assert_matrix(object$X, "future X")
  names_now <- .rqr_desn_feature_names(X, object$feature_schema$feature_names)
  if (!identical(colnames(X), names_now) ||
      !identical(object$feature_schema$schema_version, .rqr_desn_feature_schema()) ||
      !identical(object$feature_schema$n_features, ncol(X))) {
    stop("Stored future feature schema is inconsistent with X.", call. = FALSE)
  }
  future_intercept <- object$feature_schema$intercept
  if (!is.list(future_intercept) ||
      !is.logical(future_intercept$present) ||
      length(future_intercept$present) != 1L ||
      is.na(future_intercept$present)) {
    stop("Stored future intercept metadata are invalid.", call. = FALSE)
  }
  if (isTRUE(future_intercept$present)) {
    expected_intercept <- .rqr_desn_intercept(
      list(index = future_intercept$index, name = future_intercept$name),
      X,
      names_now
    )
  } else {
    expected_intercept <- .rqr_desn_intercept(NULL, X, names_now)
  }
  if (!identical(future_intercept, expected_intercept)) {
    stop("Stored future intercept metadata do not match X.", call. = FALSE)
  }
  time_index <- .rqr_desn_time_index(
    object$time_index, nrow(X), "future time_index"
  )
  reservoir <- .rqr_desn_reservoir(object$reservoir)
  if (!is.list(object$parent) || is.object(object$parent) ||
      !identical(
        names(object$parent),
        c(
          "semantic_digest", "feature_schema_digest",
          "reservoir_digest", "terminal_digest"
        )
      ) ||
      !.rqr_desn_is_sha256(object$parent$semantic_digest) ||
      !.rqr_desn_is_sha256(object$parent$feature_schema_digest) ||
      !.rqr_desn_is_sha256(object$parent$reservoir_digest) ||
      !.rqr_desn_is_sha256(object$parent$terminal_digest)) {
    stop("Stored parent-design linkage is invalid.", call. = FALSE)
  }
  expected_verification <- if (!is.null(parent_design)) {
    .rqr_desn_future_verification(parent_design)
  } else {
    NULL
  }
  verification <- object$verification
  expected_verification_fields <- c(
    "schema_version", "contract_verified",
    "legacy_explicit_matrix",
    "parent_materialization_receipt_valid",
    "parent_materialization_receipt_digest",
    "external_provenance_bound",
    "promotion_evidence_complete", "promotion_eligible",
    "promotion_status"
  )
  if (!is.list(verification) || is.object(verification) ||
      !identical(names(verification), expected_verification_fields) ||
      !identical(
        verification$schema_version,
        .rqr_desn_future_verification_schema()
      ) ||
      !isTRUE(verification$contract_verified) ||
      !identical(verification$legacy_explicit_matrix, FALSE) ||
      !identical(verification$external_provenance_bound, FALSE) ||
      !identical(verification$promotion_evidence_complete, FALSE) ||
      !identical(verification$promotion_eligible, FALSE) ||
      !identical(
        verification$promotion_status,
        "requires_verified_parent_fit_provenance"
      )) {
    stop("Stored future-design verification metadata are invalid.",
         call. = FALSE)
  }
  receipt_valid <- verification$parent_materialization_receipt_valid
  receipt_digest <- verification$parent_materialization_receipt_digest
  if (!is.logical(receipt_valid) || length(receipt_valid) != 1L ||
      is.na(receipt_valid) ||
      (isTRUE(receipt_valid) &&
        !.rqr_desn_is_sha256(receipt_digest)) ||
      (!isTRUE(receipt_valid) &&
        !(is.character(receipt_digest) &&
          length(receipt_digest) == 1L &&
          is.na(receipt_digest)))) {
    stop("Stored parent materialization verification is invalid.",
         call. = FALSE)
  }
  if (!identical(object$parent$reservoir_digest, reservoir$digest) ||
      !identical(
        object$parent$feature_schema_digest,
        .rqr_desn_sha256(object$feature_schema)
      )) {
    stop("Future schema or reservoir does not match its stored parent link.", call. = FALSE)
  }
  driver <- .rqr_desn_future_driver(object$semantics, object$driver)
  if (!identical(driver, object$driver)) {
    stop("Stored future-driver metadata are not normalized.", call. = FALSE)
  }
  causal <- .rqr_desn_causal(object$causal)
  if (!identical(causal, object$causal)) {
    stop("Stored future causal metadata are not normalized.", call. = FALSE)
  }
  origin_time <- object$time$origin_time
  if (!is.numeric(origin_time) || length(origin_time) != 1L ||
      is.na(origin_time) || !is.finite(origin_time) ||
      any(time_index <= origin_time)) {
    stop("Stored future origin/time ordering is invalid.", call. = FALSE)
  }
  expected_time <- .rqr_desn_time_metadata(
    object$time, time_index, role = "future", origin_time = origin_time
  )
  if (!identical(expected_time, object$time)) {
    stop("Stored future-time metadata are inconsistent.", call. = FALSE)
  }
  expected_terminal <- .rqr_desn_terminal(object$terminal)
  if (!identical(expected_terminal, object$terminal)) {
    stop("Stored future terminal metadata are inconsistent.", call. = FALSE)
  }

  if (!is.null(parent_design)) {
    rqr_validate_desn_design(parent_design)
    expected_parent <- list(
      semantic_digest = parent_design$semantic_digest,
      feature_schema_digest = parent_design$digests$feature_schema,
      reservoir_digest = parent_design$reservoir$digest,
      terminal_digest = parent_design$digests$terminal
    )
    if (!identical(object$parent, expected_parent) ||
        !identical(verification, expected_verification) ||
        !identical(object$feature_schema, parent_design$feature_schema) ||
        !identical(object$reservoir, parent_design$reservoir) ||
        !identical(origin_time, max(parent_design$time_index))) {
      stop("Future design does not match the supplied parent contract.", call. = FALSE)
    }
  }

  payload <- .rqr_desn_future_payload(object)
  expected_digests <- .rqr_desn_future_digests(payload)
  if (!is.list(object$digests) || is.object(object$digests) ||
      !identical(names(object$digests), names(expected_digests)) ||
      !identical(object$digests, expected_digests) ||
      !identical(object$semantic_digest, expected_digests$semantic)) {
    stop("Future RQR-DESN design semantic digest mismatch.", call. = FALSE)
  }
  invisible(TRUE)
}
