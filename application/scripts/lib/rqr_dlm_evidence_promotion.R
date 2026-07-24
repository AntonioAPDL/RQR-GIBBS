# Deterministic integrity helpers for bounded RQR-DLM evidence promotion.
#
# The helpers are kept separate from the command-line promoter so their
# fail-closed behavior can be tested without copying the completed local run.

rqr_bounded_expected_bundle_schema <- function() {
  "rqrgibbs_dlm_bounded_expected_bundle/1.0.0"
}

rqr_bounded_required_bundle_fields <- function() {
  c(
    "primary_commit",
    "primary_application_tree",
    "config_digest",
    "reference_artifact_manifest_sha256",
    "primary_runtime_tree_digest",
    "primary_runtime_attestation_schema",
    "primary_runtime_attestation_sha256",
    "runtime_toolchain_digest"
  )
}

rqr_bounded_validate_expected_bundle <- function(
    run_manifest, runtime_toolchain, expected_bundle) {
  if (!is.list(run_manifest) || !is.list(runtime_toolchain) ||
      !is.list(expected_bundle)) {
    stop("Run manifest, toolchain, and expected bundle must be lists.",
         call. = FALSE)
  }
  if (!identical(
        expected_bundle$schema_version,
        rqr_bounded_expected_bundle_schema()
      )) {
    stop("The bounded expected-bundle schema is not supported.", call. = FALSE)
  }
  fields <- rqr_bounded_required_bundle_fields()
  if (!identical(sort(names(expected_bundle$expected)), sort(fields))) {
    stop(
      "The bounded expected bundle must contain exactly the frozen fields.",
      call. = FALSE
    )
  }
  observed <- c(
    run_manifest[setdiff(fields, "runtime_toolchain_digest")],
    list(runtime_toolchain_digest = runtime_toolchain$digest)
  )
  observed <- observed[fields]
  expected <- expected_bundle$expected[fields]
  mismatches <- fields[!vapply(fields, function(field) {
    identical(as.character(observed[[field]]), as.character(expected[[field]]))
  }, logical(1L))]
  if (length(mismatches)) {
    stop(
      sprintf(
        "The run does not match the externally frozen expected bundle: %s.",
        paste(mismatches, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  if (!identical(
        as.character(runtime_toolchain$primary_runtime_tree_digest),
        as.character(expected$primary_runtime_tree_digest)
      ) ||
      !identical(
        as.character(runtime_toolchain$primary_runtime_attestation_sha256),
        as.character(expected$primary_runtime_attestation_sha256)
      )) {
    stop(
      "The runtime toolchain is not bound to the expected runtime.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

rqr_bounded_expected_fit_ids <- function(fit_plan, expected_count = 24L) {
  required <- c(
    "fixture_id", "learning_rate_mode", "chain", "seed", "fit_id"
  )
  if (!is.data.frame(fit_plan) ||
      !identical(names(fit_plan), required) ||
      nrow(fit_plan) != expected_count ||
      anyNA(fit_plan$fit_id) ||
      any(!nzchar(fit_plan$fit_id)) ||
      anyDuplicated(fit_plan$fit_id)) {
    stop("fit_plan.csv does not define one unique expected fit-ID set.",
         call. = FALSE)
  }
  sort(fit_plan$fit_id)
}

rqr_bounded_require_exact_fit_id_sets <- function(expected_ids, tables) {
  if (!is.character(expected_ids) || anyNA(expected_ids) ||
      any(!nzchar(expected_ids)) || anyDuplicated(expected_ids)) {
    stop("Expected fit IDs must be a unique nonmissing character vector.",
         call. = FALSE)
  }
  if (!is.list(tables) || is.null(names(tables)) ||
      any(!nzchar(names(tables)))) {
    stop("Fit-ID tables must be a named list.", call. = FALSE)
  }
  for (table_name in names(tables)) {
    table <- tables[[table_name]]
    if (!is.data.frame(table) || !"fit_id" %in% names(table) ||
        anyNA(table$fit_id) || any(!nzchar(table$fit_id)) ||
        anyDuplicated(table$fit_id) ||
        !identical(sort(table$fit_id), sort(expected_ids))) {
      stop(
        sprintf("%s does not contain exactly the expected fit-ID set.",
                table_name),
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}

rqr_bounded_validate_reopened_fit <- function(
    fit, checkpoint_row, local_hash_row,
    continuation_validator = NULL) {
  if (!inherits(fit, "rqr_dlm_mcmc")) {
    stop("A reopened bounded fit has the wrong class.", call. = FALSE)
  }
  if (!is.data.frame(checkpoint_row) || nrow(checkpoint_row) != 1L ||
      !is.data.frame(local_hash_row) || nrow(local_hash_row) != 1L) {
    stop("Reopened-fit manifests must each contain exactly one row.",
         call. = FALSE)
  }
  checkpoint_digest <- digest::digest(
    fit$checkpoint_state, algo = "sha256", serialize = TRUE
  )
  history_digest <- digest::digest(
    fit$continuation_history_contract, algo = "sha256", serialize = TRUE
  )
  expected_checkpoint <- c(
    as.character(fit$checkpoint_digest),
    as.character(checkpoint_row$checkpoint_digest),
    as.character(local_hash_row$checkpoint_digest)
  )
  expected_history <- c(
    as.character(fit$continuation_history_digest),
    as.character(checkpoint_row$history_digest),
    as.character(local_hash_row$history_digest)
  )
  if (!all(expected_checkpoint == checkpoint_digest)) {
    stop("A reopened fit has a checkpoint-state digest mismatch.",
         call. = FALSE)
  }
  if (!all(expected_history == history_digest)) {
    stop("A reopened fit has a continuation-history digest mismatch.",
         call. = FALSE)
  }
  if (is.null(continuation_validator)) {
    continuation_validator <- getFromNamespace(
      ".rqr_validate_continuation_history", "rqrgibbs"
    )
  }
  validation <- continuation_validator(fit)
  if (!is.list(validation) ||
      (!is.null(validation$valid) && !isTRUE(validation$valid))) {
    stop("A reopened fit failed continuation-history validation.",
         call. = FALSE)
  }
  object_digest <- digest::digest(
    fit, algo = "sha256", serialize = TRUE
  )
  if (!identical(
        object_digest,
        as.character(checkpoint_row$published_object_digest)
      )) {
    stop("A reopened fit has a published-object digest mismatch.",
         call. = FALSE)
  }
  invisible(TRUE)
}
