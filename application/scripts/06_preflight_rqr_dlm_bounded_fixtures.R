#!/usr/bin/env Rscript

repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
config_path <- file.path(
  repo_root, "application", "config", "rqr_dlm",
  "rqr_dlm_bounded_dynamic_fixtures_20260723.R"
)
if (!file.exists(config_path)) {
  stop("Run this preflight from the RQR-GIBBS repository root.", call. = FALSE)
}

expected_commit <- tolower(Sys.getenv(
  "RQR_EXPECTED_PRIMARY_COMMIT", unset = ""
))
if (!grepl("^[0-9a-f]{40}$", expected_commit)) {
  stop(
    paste(
      "Set RQR_EXPECTED_PRIMARY_COMMIT to the reviewed source commit.",
      "This preflight never infers its own expected commit."
    ),
    call. = FALSE
  )
}
git_value <- function(args) {
  output <- suppressWarnings(system2(
    "git", c("-C", shQuote(repo_root), args),
    stdout = TRUE, stderr = TRUE,
    env = c("GIT_OPTIONAL_LOCKS=0", "GIT_TERMINAL_PROMPT=0")
  ))
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  if (!identical(as.integer(status), 0L)) {
    stop("A read-only Git preflight command failed.", call. = FALSE)
  }
  trimws(paste(output, collapse = "\n"))
}
actual_commit <- tolower(git_value(c("rev-parse", "HEAD")))
branch <- git_value(c("rev-parse", "--abbrev-ref", "HEAD"))
status <- git_value(c(
  "status", "--porcelain=v2", "--untracked-files=all"
))
if (!identical(branch, "main") ||
    !identical(actual_commit, expected_commit) || nzchar(status)) {
  stop(
    "The bounded-fixture source must be clean, on main, and exact.",
    call. = FALSE
  )
}

primary_runtime_root <- normalizePath(
  Sys.getenv(
    "RQR_PRIMARY_RUNTIME_ROOT",
    unset = file.path(dirname(repo_root), ".rqr_gibbs_primary_runtime")
  ),
  winslash = "/", mustWork = TRUE
)
primary_commit_root <- file.path(
  primary_runtime_root, substr(expected_commit, 1L, 12L)
)
primary_library <- normalizePath(
  file.path(primary_commit_root, "library"),
  winslash = "/", mustWork = TRUE
)
primary_attestation <- normalizePath(
  file.path(
    primary_commit_root, "attestations",
    paste0("rqrgibbs_", substr(expected_commit, 1L, 12L), ".rds")
  ),
  winslash = "/", mustWork = TRUE
)
.libPaths(c(primary_library, .libPaths()))
if ("rqrgibbs" %in% loadedNamespaces()) {
  stop(
    "rqrgibbs was loaded before its isolated library was selected.",
    call. = FALSE
  )
}
required_packages <- c("digest", "jsonlite", "posterior", "rqrgibbs")
missing_packages <- required_packages[!vapply(
  required_packages, requireNamespace, quietly = TRUE,
  FUN.VALUE = logical(1L)
)]
if (length(missing_packages)) {
  stop(
    "Missing bounded-preflight packages: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}
runtime_state <- rqrgibbs:::.rqr_repository_provenance(list(
  repo_root = repo_root,
  expected_git_commit = expected_commit,
  runtime_package = "rqrgibbs",
  runtime_attestation = primary_attestation,
  require_isolated_runtime = TRUE,
  source_subdir = "application"
))
if (!isTRUE(runtime_state$runtime_attestation_match) ||
    !isTRUE(runtime_state$source_archive_tree_match) ||
    !isTRUE(runtime_state$source_package_verified) ||
    !isTRUE(runtime_state$runtime_install_receipt_match) ||
    !isTRUE(runtime_state$runtime_source_match) ||
    !isTRUE(runtime_state$reproducibility_eligible)) {
  stop("The isolated primary runtime lineage gate failed.", call. = FALSE)
}

config_environment <- new.env(parent = baseenv())
sys.source(config_path, envir = config_environment)
config <- config_environment$rqr_dlm_bounded_dynamic_fixtures
required_top_level <- c(
  "schema_version", "config_id", "scope", "generalized_bayes",
  "response_likelihood", "production_simulation_authorized", "mcmc",
  "continuation", "gates", "fixtures"
)
if (!is.list(config) || !all(required_top_level %in% names(config))) {
  stop("The bounded-fixture configuration is incomplete.", call. = FALSE)
}
if (!identical(
      config$schema_version,
      "rqrgibbs_dlm_bounded_fixtures/2.0.0"
    ) ||
    !isTRUE(config$generalized_bayes) ||
    isTRUE(config$response_likelihood) ||
    isTRUE(config$production_simulation_authorized)) {
  stop("The bounded-fixture interpretation contract is invalid.", call. = FALSE)
}
if (!identical(config$mcmc$chains, 4L) ||
    length(config$mcmc$seeds) != config$mcmc$chains ||
    anyDuplicated(config$mcmc$seeds) ||
    !identical(config$mcmc$numerical_policy, "fail")) {
  stop("The bounded-fixture chain and seed contract is invalid.", call. = FALSE)
}
if (!identical(config$continuation$generations, 2L) ||
    !identical(
      config$gates$primary_diagnostics,
      "posterior_rank_normalized_rhat_bulk_tail_ess"
    ) ||
    !identical(config$gates$root_swap_activity_role, "sidecar_only")) {
  stop("The bounded-fixture diagnostic contract is invalid.", call. = FALSE)
}
exact_modes <- vapply(
  config$fixtures, function(fixture) fixture$evolution_mode,
  character(1L)
)
if (!setequal(
      exact_modes, c("fixed_W", "discount_template", "component_scale")
    ) ||
    any(exact_modes == "adaptive_discount")) {
  stop("Only the three fixed-joint evolution modes are allowed.", call. = FALSE)
}

sys.source(
  file.path(
    repo_root, "application", "scripts", "lib",
    "rqr_dlm_bounded_fixtures.R"
  ),
  envir = environment()
)
constructed <- rqr_build_all_bounded_dlm_fixtures(config)
audits <- do.call(rbind, lapply(constructed, function(item) {
  audit <- item$construction_audit
  data.frame(
    fixture_id = item$fixture_id,
    state_dimension = audit$state_dimension,
    component_dims = paste(audit$component_dims, collapse = ","),
    component_names = paste(audit$component_names, collapse = ","),
    observed_count = audit$observed_count,
    missing_count = audit$missing_count,
    training_horizon = audit$training_horizon,
    future_horizon = audit$future_horizon,
    evolution_mode = audit$evolution_mode,
    exact_joint_target = audit$exact_joint_target,
    training_evolution_slices = audit$training_evolution_slices,
    future_evolution_slices = paste(
      audit$future_evolution_slices, collapse = ","
    ),
    extension_reproduces_training =
      audit$extension_reproduces_training,
    model_digest = audit$model_digest,
    evolution_digest = audit$evolution_digest,
    missing_response_digest = audit$missing_response_digest,
    future_digest = audit$future_digest,
    stringsAsFactors = FALSE
  )
}))
if (nrow(audits) != 3L ||
    any(!audits$exact_joint_target) ||
    !all(audits$future_horizon > 0L) ||
    !identical(
      audits$extension_reproduces_training[
        audits$evolution_mode == "discount_template"
      ],
      TRUE
    )) {
  stop("Constructed bounded fixtures failed their object gates.", call. = FALSE)
}

manifest <- list(
  schema_version = "rqrgibbs_dlm_bounded_preflight/2.0.0",
  config_id = config$config_id,
  config_digest = digest::digest(
    config, algo = "sha256", serialize = TRUE
  ),
  primary_commit = actual_commit,
  primary_application_tree = runtime_state$source_tree_digest,
  primary_runtime_path = runtime_state$runtime_package_path,
  primary_runtime_tree_digest =
    runtime_state$runtime_package_tree_digest,
  primary_runtime_attestation = primary_attestation,
  primary_runtime_attestation_schema =
    runtime_state$runtime_attestation_schema,
  primary_runtime_source_match = runtime_state$runtime_source_match,
  source_archive_tree_match = runtime_state$source_archive_tree_match,
  source_package_verified = runtime_state$source_package_verified,
  runtime_install_receipt_match =
    runtime_state$runtime_install_receipt_match,
  fixture_ids = names(config$fixtures),
  evolution_modes = unname(exact_modes),
  learning_rate_modes = config$learning_rate_modes,
  chains = config$mcmc$chains,
  seeds = config$mcmc$seeds,
  requested_fits =
    length(config$fixtures) *
      length(config$learning_rate_modes) *
      config$mcmc$chains,
  diagnostic_provider = as.character(
    utils::packageVersion("posterior")
  ),
  fixture_construction = audits,
  production_simulation_authorized = FALSE,
  bounded_dynamic_execution_authorized = FALSE,
  recorded_at = format(Sys.time(), tz = "UTC", usetz = TRUE)
)
output_path <- file.path(
  repo_root, "application", "manifests",
  "rqr_dlm_bounded_dynamic_preflight.json"
)
jsonlite::write_json(
  manifest, output_path, auto_unbox = TRUE, pretty = TRUE, digits = NA
)
write.csv(
  audits,
  file.path(
    repo_root, "application", "manifests",
    "rqr_dlm_bounded_dynamic_fixture_construction.csv"
  ),
  row.names = FALSE
)
cat("Bounded RQR-DLM fixture construction preflight passed.\n")
cat("  constructed fixtures:", nrow(audits), "\n")
cat("  prospective bounded fits:", manifest$requested_fits, "\n")
cat("  execution authorization: false\n")
cat("Manifest:", output_path, "\n")
