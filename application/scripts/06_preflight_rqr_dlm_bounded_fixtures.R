#!/usr/bin/env Rscript

repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
config_path <- file.path(
  repo_root, "application", "config", "rqr_dlm",
  "rqr_dlm_bounded_dynamic_fixtures_20260722.R"
)
if (!file.exists(config_path)) {
  stop("Run this preflight from the RQR-GIBBS repository root.", call. = FALSE)
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
      "rqrgibbs_dlm_bounded_fixtures/1.0.0"
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
if (!identical(config$continuation$generations, 2L)) {
  stop("Two continuation generations are required.", call. = FALSE)
}
exact_modes <- vapply(
  config$fixtures, function(fixture) fixture$evolution_mode, character(1L)
)
if (!setequal(
      exact_modes, c("fixed_W", "discount_template", "component_scale")
    ) ||
    any(exact_modes == "adaptive_discount")) {
  stop("Only the three declared fixed-joint evolution modes are allowed.", call. = FALSE)
}
if (any(vapply(
      config$fixtures,
      function(fixture) length(fixture$y) != fixture$n_time,
      logical(1L)
    ))) {
  stop("Every bounded fixture must have n_time responses.", call. = FALSE)
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
actual_commit <- tolower(trimws(system2(
  "git", c("-C", shQuote(repo_root), "rev-parse", "HEAD"), stdout = TRUE
)))
status <- trimws(paste(system2(
  "git",
  c(
    "-C", shQuote(repo_root), "status", "--porcelain",
    "--untracked-files=all"
  ),
  stdout = TRUE
), collapse = "\n"))
if (!identical(actual_commit, expected_commit) || nzchar(status)) {
  stop("The bounded-fixture source commit is not exact and clean.", call. = FALSE)
}

if (!requireNamespace("digest", quietly = TRUE) ||
    !requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Packages digest and jsonlite are required.", call. = FALSE)
}
manifest <- list(
  schema_version = "rqrgibbs_dlm_bounded_preflight/1.0.0",
  config_id = config$config_id,
  config_digest = digest::digest(
    config, algo = "sha256", serialize = TRUE
  ),
  primary_commit = actual_commit,
  fixture_ids = names(config$fixtures),
  evolution_modes = unname(exact_modes),
  seeds = config$mcmc$seeds,
  production_simulation_authorized = FALSE,
  recorded_at = format(Sys.time(), tz = "UTC", usetz = TRUE)
)
output_path <- file.path(
  repo_root, "application", "manifests",
  "rqr_dlm_bounded_dynamic_preflight.json"
)
jsonlite::write_json(
  manifest, output_path, auto_unbox = TRUE, pretty = TRUE, digits = NA
)
cat("Bounded RQR-DLM fixture preflight passed.\n")
cat("Manifest:", output_path, "\n")
