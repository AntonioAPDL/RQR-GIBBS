# Helpers for the explicitly authorized diagnostic-aware RQR-DLM completion
# lane.  The base confirmatory design remains unchanged and fail closed.

rqr_completion_policy_path <- function(repo_root) {
  file.path(
    normalizePath(repo_root, winslash = "/", mustWork = TRUE),
    "application", "config", "rqr_dlm",
    "rqr_dlm_diagnostic_aware_completion_20260807.R"
  )
}

rqr_completion_read_policy <- function(repo_root) {
  path <- rqr_completion_policy_path(repo_root)
  if (!file.exists(path)) {
    stop("The diagnostic-aware completion policy is missing.", call. = FALSE)
  }
  environment <- new.env(parent = baseenv())
  sys.source(path, envir = environment)
  policy <- environment$rqr_dlm_diagnostic_aware_completion
  required <- c(
    "schema_version", "policy_id", "execution_authorized",
    "explicit_user_direction_recorded", "user_direction_date",
    "interpretation", "generalized_bayes", "response_likelihood",
    "response_prediction_contract", "complete_maximum_design",
    "precision_stopping_disabled", "diagnostic_thresholds_changed",
    "diagnostic_failures", "diagnostic_construction_failures",
    "hard_stop_classes", "selected_transition", "bounded_evidence",
    "result_contract"
  )
  transition_required <- c(
    "method", "candidate", "transition_multiplier",
    "joint_state_elliptical_cycles",
    "component_scale_directional_interweave",
    "component_scale_directional_sweeps", "target_change",
    "schedule_change"
  )
  result_required <- c(
    "keep_primary_metrics_when_diagnostics_fail", "failure_class",
    "worker_status", "stage_status",
    "include_warning_rows_in_primary_aggregate",
    "publish_warning_stratified_sensitivity", "scientific_promotion"
  )
  valid_count <- function(value, expected) {
    is.numeric(value) && length(value) == 1L && !is.na(value) &&
      is.finite(value) && value == floor(value) && value == expected
  }
  if (!is.list(policy) || !identical(names(policy), required) ||
      !identical(
        policy$schema_version,
        "rqrgibbs_dlm_diagnostic_aware_completion/1.0.0"
      ) ||
      !identical(
        policy$policy_id,
        "rqr_dlm_diagnostic_aware_completion_20260807"
      ) ||
      !isTRUE(policy$execution_authorized) ||
      !isTRUE(policy$explicit_user_direction_recorded) ||
      !identical(policy$user_direction_date, "2026-08-07") ||
      !identical(
        policy$interpretation,
        "diagnostic_aware_not_convergence_validated"
      ) ||
      !isTRUE(policy$generalized_bayes) ||
      isTRUE(policy$response_likelihood) ||
      isTRUE(policy$response_prediction_contract) ||
      !isTRUE(policy$complete_maximum_design) ||
      !isTRUE(policy$precision_stopping_disabled) ||
      !identical(policy$diagnostic_thresholds_changed, FALSE) ||
      !identical(policy$diagnostic_failures, "record_and_continue") ||
      !identical(
        policy$diagnostic_construction_failures, "global_stop"
      ) ||
      !identical(names(policy$selected_transition), transition_required) ||
      !identical(policy$selected_transition$method, "M11") ||
      !identical(
        policy$selected_transition$candidate, "directional1_joint1"
      ) ||
      !valid_count(policy$selected_transition$transition_multiplier, 2L) ||
      !valid_count(
        policy$selected_transition$joint_state_elliptical_cycles, 1L
      ) ||
      !isTRUE(
        policy$selected_transition$component_scale_directional_interweave
      ) ||
      !valid_count(
        policy$selected_transition$component_scale_directional_sweeps, 1L
      ) ||
      isTRUE(policy$selected_transition$target_change) ||
      isTRUE(policy$selected_transition$schedule_change) ||
      !identical(names(policy$result_contract), result_required) ||
      !isTRUE(
        policy$result_contract$keep_primary_metrics_when_diagnostics_fail
      ) ||
      !identical(
        policy$result_contract$failure_class,
        "mcmc_diagnostic_warning"
      ) ||
      !identical(
        policy$result_contract$worker_status,
        "completed_with_diagnostic_warning"
      ) ||
      !identical(
        policy$result_contract$stage_status,
        "completed_with_diagnostic_warnings"
      ) ||
      !isTRUE(
        policy$result_contract$include_warning_rows_in_primary_aggregate
      ) ||
      !isTRUE(
        policy$result_contract$publish_warning_stratified_sensitivity
      ) ||
      isTRUE(policy$result_contract$scientific_promotion)) {
    stop("The diagnostic-aware completion policy is invalid.", call. = FALSE)
  }
  list(policy = policy, path = normalizePath(path, winslash = "/"))
}

rqr_completion_active <- function() {
  identical(
    Sys.getenv("RQR_DLM_EXECUTION_POLICY", unset = ""),
    "diagnostic-aware-completion"
  )
}

rqr_completion_apply_policy <- function(contract, policy) {
  if (!is.list(contract) || !is.list(policy)) {
    stop("Cannot apply an incomplete completion policy.", call. = FALSE)
  }
  contract$execution_policy <- policy
  contract$mcmc_control_overrides <- list(
    M11 = list(
      component_scale_directional_interweave = TRUE,
      component_scale_directional_sweeps = 1L
    )
  )
  contract
}

rqr_completion_force_maximum_decisions <- function(decisions, contract) {
  if (!nrow(decisions)) return(decisions)
  for (index in seq_len(nrow(decisions))) {
    rule <- as.character(decisions$replication_rule[[index]])
    batch <- if (identical(rule, "C")) {
      contract$config$batching$core
    } else if (identical(rule, "S")) {
      contract$config$batching$sensitivity
    } else {
      stop("A diagnostic-aware decision has an unknown batch rule.",
           call. = FALSE)
    }
    replications <- rqr_confirm_strict_integer(
      decisions$replications[[index]], "decision replications", 1L,
      batch$maximum
    )
    if (replications < batch$maximum) {
      decisions$next_action[[index]] <-
        "add_complete_paired_DGP_batch"
      decisions$next_replications[[index]] <- min(
        replications + batch$increment, batch$maximum
      )
    } else {
      decisions$next_action[[index]] <- if (isTRUE(
          decisions$precision_pass[[index]])) {
        "precision_pass_stop"
      } else {
        "maximum_reached_report_unmet_precision"
      }
      decisions$next_replications[[index]] <- replications
    }
  }
  decisions
}

rqr_completion_authorized <- function(
    policy_record, expected_commit, authorization_bundle) {
  policy <- policy_record$policy
  expected_commit <- tolower(as.character(expected_commit)[[1L]])
  required <- c(
    "schema_version", "source_commit", "reviewed_implementation_commit",
    "authorization_commit", "explicit_user_confirmation",
    "all_reference_gates_pass",
    "execution_policy_sha256", "execution_policy_id",
    "diagnostic_thresholds_changed", "complete_maximum_design",
    "precision_stopping_disabled", "primary_worktree_clean",
    "primary_runtime_tree_digest", "preflight_artifact_hashes_sha256",
    "reference_artifact_hashes_sha256", "seed_ledger_sha256",
    "task_plan_sha256", "exdqlm_source_sha256",
    "quantreg_source_sha256", "reference_runtime_bundle_match",
    "comparator_dependency_runtime_match", "toolchain_match",
    "protected_checkout_used", "scientific_promotion"
  )
  valid_sha <- function(value, size) {
    is.character(value) && length(value) == 1L && !is.na(value) &&
      grepl(sprintf("^[0-9a-f]{%d}$", size), tolower(value))
  }
  if (!isTRUE(policy$execution_authorized) ||
      !valid_sha(expected_commit, 40L) ||
      !is.list(authorization_bundle) ||
      !all(required %in% names(authorization_bundle)) ||
      !identical(
        authorization_bundle$schema_version,
        "rqrgibbs_dlm_diagnostic_aware_authorization/1.0.0"
      ) ||
      !identical(
        tolower(authorization_bundle$source_commit), expected_commit
      ) ||
      !identical(
        tolower(authorization_bundle$reviewed_implementation_commit),
        expected_commit
      ) ||
      !identical(
        tolower(authorization_bundle$authorization_commit), expected_commit
      ) ||
      !isTRUE(authorization_bundle$explicit_user_confirmation) ||
      !isTRUE(authorization_bundle$all_reference_gates_pass) ||
      !identical(
        authorization_bundle$execution_policy_id, policy$policy_id
      ) ||
      !identical(
        tolower(authorization_bundle$execution_policy_sha256),
        rqr_confirm_sha256(policy_record$path)
      ) ||
      !identical(
        authorization_bundle$diagnostic_thresholds_changed, FALSE
      ) ||
      !isTRUE(authorization_bundle$complete_maximum_design) ||
      !isTRUE(authorization_bundle$precision_stopping_disabled) ||
      !isTRUE(authorization_bundle$primary_worktree_clean) ||
      !isTRUE(authorization_bundle$reference_runtime_bundle_match) ||
      !isTRUE(authorization_bundle$comparator_dependency_runtime_match) ||
      !isTRUE(authorization_bundle$toolchain_match) ||
      isTRUE(authorization_bundle$protected_checkout_used) ||
      isTRUE(authorization_bundle$scientific_promotion) ||
      any(!vapply(
        authorization_bundle[c(
          "execution_policy_sha256", "primary_runtime_tree_digest",
          "preflight_artifact_hashes_sha256",
          "reference_artifact_hashes_sha256", "seed_ledger_sha256",
          "task_plan_sha256", "exdqlm_source_sha256",
          "quantreg_source_sha256"
        )], valid_sha, logical(1L), size = 64L
      ))) {
    stop("Diagnostic-aware execution authorization is invalid.",
         call. = FALSE)
  }
  TRUE
}
