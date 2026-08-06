#!/usr/bin/env Rscript

# Central acceptance boundary for closed oracle-tilt illustration campaigns.
# Historical source commits remain reproducible, but current-main heavy actions
# are permitted only when the tracked registry says so explicitly.

otcg_schema <- function() "rqrgibbs_oracle_tilt_campaign_registry/1.0.0"

otcg_stop <- function(...) {
  stop(paste0(...), call. = FALSE)
}

otcg_scalar_character <- function(value, label) {
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !nzchar(value)) {
    otcg_stop(label, " must be one nonempty string.")
  }
  value
}

otcg_scalar_logical <- function(value, label) {
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    otcg_stop(label, " must be one nonmissing logical value.")
  }
  value
}

otcg_nonnegative_integer <- function(value, label) {
  if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
      !is.finite(value) || value < 0 || value != floor(value) ||
      value > .Machine$integer.max) {
    otcg_stop(label, " must be one finite nonnegative integer.")
  }
  as.integer(value)
}

otcg_scalar_finite <- function(value, label) {
  if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
      !is.finite(value)) {
    otcg_stop(label, " must be one finite numeric value.")
  }
  as.numeric(value)
}

otcg_registry_path <- function(repo_root) {
  file.path(
    repo_root, "application", "config",
    "oracle_tilt_illustration_campaign_registry_20260805.json"
  )
}

otcg_validate_registry <- function(registry) {
  if (!is.list(registry) ||
      !identical(registry$schema_version, otcg_schema())) {
    otcg_stop("Unexpected oracle-tilt campaign registry schema.")
  }
  required <- c(
    "publication_v2", "publication_v3",
    "publication_v3_dlm_sh_adjudication"
  )
  if (!is.list(registry$campaigns) ||
      !identical(sort(names(registry$campaigns)), sort(required))) {
    otcg_stop("The campaign registry must contain exactly the three closed campaigns.")
  }
  if (!identical(registry$active_manuscript_campaign, "publication_v2") ||
      !identical(
        registry$active_manuscript_evidence_directory,
        "figures/data/oracle_tilt_c095_v2"
      )) {
    otcg_stop("The validated version-2 campaign must remain the manuscript source.")
  }

  expected <- list(
    publication_v2 = list(
      status = "validated_and_promoted",
      source_commit = "fec979f927c9039cf778ac09aef139ebd6761e8e",
      config_sha256 =
        "e037461a3adf3a98065af9718daf636e6fbbd54ec00cc2fd71c9404f6ba50587",
      runtime_tree_digest =
        "d7890468c42d046697a11fe6784e7f08ac26a72c4764d5adb9ecca37931158b9",
      cells = 6L, passed = 6L, chains = 27L, eligible = TRUE
    ),
    publication_v3 = list(
      status = "completed_not_promoted",
      source_commit = "99a088fbdd7c3f3ed18f99197294038f62dbfe41",
      config_sha256 =
        "585e7258451690c94a153fad50fc6a72a60137bca6447e1b851050b4ed8ca5d7",
      runtime_tree_digest =
        "c5a0cbffb384b777af873dfadae77d6a0909af6387c9f6ca001147f6ded568c9",
      cells = 6L, passed = 5L, chains = 27L, eligible = FALSE
    ),
    publication_v3_dlm_sh_adjudication = list(
      status = "completed_descriptive_only",
      source_commit = "a3b39b394c6aa928eb38e9ed461281cdf743d00b",
      config_sha256 =
        "bf41bf834e0ce878b9399f77155456fadd3b2feb0f7bcc99064617ab69d15c91",
      runtime_tree_digest =
        "ac1d402f8dc1c724032396b20862e273bf6ec34a005dc24dd2bdbbc3ba5f58f2",
      chains = 5L, eligible = FALSE
    )
  )
  for (name in names(expected)) {
    campaign <- registry$campaigns[[name]]
    truth <- expected[[name]]
    if (!identical(otcg_scalar_character(campaign$status, paste0(name, " status")),
                   truth$status) ||
        !identical(otcg_scalar_character(
          campaign$source_commit, paste0(name, " source commit")
        ), truth$source_commit) ||
        !identical(otcg_scalar_character(
          campaign$config_sha256, paste0(name, " config SHA-256")
        ), truth$config_sha256) ||
        !identical(otcg_scalar_character(
          campaign$runtime_tree_digest, paste0(name, " runtime digest")
        ), truth$runtime_tree_digest) ||
        !identical(otcg_nonnegative_integer(
          campaign$completed_chains, paste0(name, " completed chains")
        ), truth$chains) ||
        !identical(otcg_scalar_logical(
          campaign$manuscript_illustration_evidence_eligible,
          paste0(name, " manuscript eligibility")
        ), truth$eligible) ||
        isTRUE(otcg_scalar_logical(
          campaign$heavy_execution_authorized_on_current_main,
          paste0(name, " current-main execution authorization")
        ))) {
      otcg_stop("Campaign registry invariant failed for ", name, ".")
    }
    actions <- campaign$allowed_current_main_actions
    if (!is.character(actions) || anyNA(actions) || any(!nzchar(actions)) ||
        anyDuplicated(actions)) {
      otcg_stop(name, " must declare unique nonempty allowed actions.")
    }
    if (any(actions %in% c(
      "benchmark", "resource-rehearsal", "acceptance", "execute",
      "adjudication"
    ))) {
      otcg_stop(name, " improperly authorizes a closed heavy action.")
    }
  }
  for (name in c("publication_v2", "publication_v3")) {
    campaign <- registry$campaigns[[name]]
    if (!identical(
      otcg_nonnegative_integer(campaign$target_cells, paste0(name, " cells")),
      expected[[name]]$cells
    ) || !identical(
      otcg_nonnegative_integer(
        campaign$strict_pass_cells, paste0(name, " strict cells")
      ), expected[[name]]$passed
    ) || !isTRUE(all.equal(
      otcg_scalar_finite(campaign$coverage_level, paste0(name, " coverage")),
      0.95, tolerance = 0
    ))) {
      otcg_stop("Campaign cell-count invariant failed for ", name, ".")
    }
  }
  adjudication <- registry$campaigns$publication_v3_dlm_sh_adjudication
  if (!identical(
    adjudication$base_source_commit,
    expected$publication_v3$source_commit
  ) ||
      !identical(otcg_nonnegative_integer(
        adjudication$retained_draws, "adjudication retained draws"
      ), 60000L) ||
      !identical(otcg_nonnegative_integer(
        adjudication$bitwise_prefix_checks_passed,
        "adjudication passed prefix checks"
      ), 15L) ||
      !identical(otcg_nonnegative_integer(
        adjudication$bitwise_prefix_checks_total,
        "adjudication total prefix checks"
      ), 15L) ||
      !identical(otcg_nonnegative_integer(
        adjudication$numerical_repair_count,
        "adjudication repair count"
      ), 0L) ||
      !isTRUE(adjudication$strict_diagnostics_pass) ||
      isTRUE(adjudication$heterogeneity_pass) ||
      isTRUE(adjudication$automatic_promotion_eligible) ||
      !isTRUE(all.equal(
        otcg_scalar_finite(
          adjudication$width_contrast_relative_error,
          "adjudication width-contrast error"
        ), 0.202622544829516, tolerance = 1e-15
      )) ||
      !isTRUE(all.equal(
        otcg_scalar_finite(
          adjudication$maximum_width_contrast_relative_error,
          "adjudication maximum width-contrast error"
        ), 0.20, tolerance = 0
      )) ||
      !(adjudication$width_contrast_relative_error >
        adjudication$maximum_width_contrast_relative_error)) {
    otcg_stop("The adjudication closeout invariants are inconsistent.")
  }
  future <- registry$future_campaign_contract
  required_future <- c(
    "requires_new_campaign_identifier", "requires_new_frozen_configuration",
    "requires_new_exact_source_commit",
    "requires_prospective_gates_before_data_generation",
    "prohibits_same_data_seed_or_gate_tuning",
    "prohibits_automatic_reuse_of_closed_campaign_output"
  )
  if (!is.list(future) || !all(required_future %in% names(future)) ||
      !all(vapply(future[required_future], isTRUE, logical(1)))) {
    otcg_stop("The prospective future-campaign contract is incomplete.")
  }
  invisible(registry)
}

otcg_read_registry <- function(repo_root) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    otcg_stop("The jsonlite package is required for the campaign gate.")
  }
  path <- otcg_registry_path(repo_root)
  if (!file.exists(path)) otcg_stop("Campaign registry not found: ", path)
  registry <- jsonlite::read_json(path, simplifyVector = TRUE)
  otcg_validate_registry(registry)
  registry
}

otcg_assert_action <- function(repo_root, campaign, action) {
  registry <- otcg_read_registry(repo_root)
  campaign <- otcg_scalar_character(campaign, "campaign")
  action <- otcg_scalar_character(action, "action")
  if (!campaign %in% names(registry$campaigns)) {
    otcg_stop("Unknown oracle-tilt campaign: ", campaign, ".")
  }
  specification <- registry$campaigns[[campaign]]
  if (!action %in% specification$allowed_current_main_actions) {
    otcg_stop(
      "The ", campaign, " campaign is closed with status '",
      specification$status, "'. Current main does not authorize action '",
      action, "'. Use the compact closeout evidence for audit, or define a ",
      "new prospectively frozen campaign instead of rerunning or retuning ",
      "the closed data set."
    )
  }
  invisible(specification)
}

otcg_main <- function() {
  trailing <- commandArgs(trailingOnly = TRUE)
  value <- function(prefix, default = "") {
    hit <- trailing[startsWith(trailing, prefix)]
    if (length(hit)) sub(prefix, "", hit[1L], fixed = TRUE) else default
  }
  repo_root <- value("--repo-root=", getwd())
  repo_root <- normalizePath(repo_root, winslash = "/", mustWork = TRUE)
  campaign <- value("--campaign=")
  action <- value("--action=")
  otcg_assert_action(repo_root, campaign, action)
  cat("Campaign gate passed:", campaign, action, "\n")
}

if (sys.nframe() == 0L) otcg_main()
