#!/usr/bin/env Rscript

arguments <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_path <- sub(file_arg, "", arguments[startsWith(arguments, file_arg)][1L])
if (!length(script_path) || is.na(script_path)) {
  script_path <- "application/scripts/34_run_oracle_tilt_publication.R"
}
script_path <- normalizePath(script_path, winslash = "/", mustWork = TRUE)
script_dir <- dirname(script_path)
source(file.path(script_dir, "32_oracle_tilt_illustration_utils.R"))
source(file.path(script_dir, "33_oracle_tilt_forensic_utils.R"))
source(file.path(script_dir, "34_oracle_tilt_publication_utils.R"))

trailing <- commandArgs(trailingOnly = TRUE)
arg_value <- function(prefix, default = NULL) {
  hit <- trailing[startsWith(trailing, prefix)]
  if (length(hit)) sub(prefix, "", hit[1L], fixed = TRUE) else default
}
mode <- tolower(arg_value("--mode=", "preflight"))
if (!mode %in% c("preflight", "execute")) {
  oti_stop("--mode must be preflight or execute.")
}

repo_root <- normalizePath(file.path(script_dir, "..", ".."), mustWork = TRUE)
setwd(repo_root)
config_path <- normalizePath(
  arg_value(
    "--config=",
    file.path(
      repo_root, "application", "config",
      "oracle_tilt_c095_publication_20260731.json"
    )
  ),
  winslash = "/", mustWork = TRUE
)
config <- oti_read_json(config_path)
otp_validate_config(config)

if (!requireNamespace("rqrgibbs", quietly = TRUE)) {
  oti_stop("The rqrgibbs package must be installed.")
}
if (!requireNamespace("posterior", quietly = TRUE)) {
  oti_stop("The posterior package is required for maintained diagnostics.")
}

expected_commit <- tolower(Sys.getenv("RQR_EXPECTED_PRIMARY_COMMIT", ""))
attestation_path <- Sys.getenv("RQR_PRIMARY_RUNTIME_ATTESTATION", "")
provenance_control <- list()
runtime_binding <- list(
  mode = "exploratory_preflight",
  match = FALSE,
  expected_commit = if (nzchar(expected_commit)) expected_commit else NA_character_
)
if (identical(mode, "execute")) {
  if (!identical(Sys.getenv("RQR_ORACLE_TILT_PUBLICATION_CONFIRM"), "YES")) {
    oti_stop(
      paste(
        "Execution is fail-closed; set",
        "RQR_ORACLE_TILT_PUBLICATION_CONFIRM=YES after explicit review."
      )
    )
  }
  if (!grepl("^[0-9a-f]{40}$", expected_commit) ||
      !nzchar(attestation_path)) {
    oti_stop("Execution requires a full source SHA and runtime attestation.")
  }
  source(file.path(
    repo_root, "application", "scripts", "lib",
    "isolated_runtime_lineage.R"
  ))
  source(file.path(
    repo_root, "application", "scripts", "lib",
    "rqr_dlm_main_simulation.R"
  ))
  source(file.path(
    repo_root, "application", "scripts", "lib",
    "rqr_dlm_confirmatory_simulation.R"
  ))
  runtime_binding <- rqr_main_primary_runtime_binding(
    repo_root, expected_commit, attestation_path
  )
  provenance_control <- rqr_confirm_primary_provenance_control(
    repo_root, expected_commit, attestation_path
  )
}

default_output <- file.path(
  repo_root, "application", "outputs", "oracle_tilt_c095_publication",
  paste0(mode, "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
)
output_root <- normalizePath(
  arg_value("--output-dir=", default_output),
  winslash = "/", mustWork = FALSE
)
if (file.exists(file.path(output_root, "closeout.json"))) {
  oti_stop("The requested output root is already closed.")
}
oti_ensure_dir(output_root)
worker_root <- oti_ensure_dir(file.path(output_root, "worker_results"))

atomic_json <- function(value, path) {
  oti_ensure_dir(dirname(path))
  temporary <- tempfile(paste0(".", basename(path), "-"), dirname(path))
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  jsonlite::write_json(
    value, temporary, pretty = TRUE, auto_unbox = TRUE,
    digits = NA, null = "null", na = "null"
  )
  if (!file.rename(temporary, path)) oti_stop("Atomic JSON write failed.")
  invisible(path)
}

source_state <- list(
  schema_version = otp_schema(),
  mode = mode,
  started_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  repository = oti_git_state(repo_root),
  source_commit = if (nzchar(expected_commit)) expected_commit else
    oti_git_state(repo_root)$commit,
  config_sha256 = oti_file_sha256(config_path),
  execution_confirmed = identical(mode, "execute"),
  interpretation = paste(
    "Single-data interval-root generalized-posterior illustration;",
    "not a response likelihood, response-predictive analysis, or",
    "repeated-sample simulation study."
  )
)
atomic_json(source_state, file.path(output_root, "source_state.json"))
atomic_json(runtime_binding, file.path(output_root, "runtime_binding.json"))
atomic_json(config, file.path(output_root, "config.json"))

plan <- otp_plan(config)
plan$seed <- mapply(
  function(family, target, chain) {
    otp_seed(config, family, target, chain)
  },
  plan$family, plan$target, plan$chain
)
otf_atomic_write_csv(plan, file.path(output_root, "fit_plan.csv"))

law <- oti_law_from_config(config)
oracle <- oti_oracle_targets(law, config$coverage_level, config$targets)
otf_atomic_write_csv(oracle, file.path(output_root, "oracle_targets.csv"))
fixed_dgp <- oti_fixed_design_dgp(config, law)
dlm_dgp <- oti_dlm_dgp(config, law)
fixed_targets <- oti_targets_by_index(
  fixed_dgp$mean_truth, fixed_dgp$scale_truth, oracle, fixed_dgp$observed
)
dlm_targets <- oti_targets_by_index(
  dlm_dgp$mean_truth, dlm_dgp$scale_truth, oracle, dlm_dgp$observed
)
dgp_contract <- data.frame(
  family = c("fixed_design", "dlm"),
  seed = c(fixed_dgp$seed, dlm_dgp$seed),
  n_index = c(length(fixed_dgp$y), length(dlm_dgp$y)),
  n_observed = c(sum(fixed_dgp$observed), sum(dlm_dgp$observed)),
  missing_indices = c("", paste(dlm_dgp$missing_times, collapse = ";")),
  initial_level_variance = c(NA_real_, dlm_dgp$initial_level_variance),
  initial_slope_variance = c(NA_real_, dlm_dgp$initial_slope_variance),
  dgp_digest = c(
    otf_object_sha256(fixed_dgp), otf_object_sha256(dlm_dgp)
  ),
  target_digest = c(
    otf_object_sha256(fixed_targets), otf_object_sha256(dlm_targets)
  ),
  stringsAsFactors = FALSE
)
otf_atomic_write_csv(dgp_contract, file.path(output_root, "dgp_contract.csv"))

if (identical(mode, "preflight")) {
  closeout <- list(
    schema_version = otp_schema(), mode = mode,
    finished_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    preflight_pass = nrow(plan) == 27L && nrow(oracle) == 3L &&
      identical(dlm_dgp$initial_slope_variance, 0.001),
    planned_chains = nrow(plan), completed_chains = 0L,
    manuscript_promotion_authorized = FALSE
  )
  atomic_json(closeout, file.path(output_root, "closeout.json"))
  compact <- file.path(output_root, otp_compact_files())
  compact <- compact[file.exists(compact)]
  manifest <- oti_file_hashes(compact, output_root)
  names(manifest)[names(manifest) == "relative_path"] <- "path"
  otf_atomic_write_csv(manifest, file.path(output_root, "artifact_manifest.csv"))
  message("[oracle-tilt-publication] preflight passed: ", output_root)
  quit(save = "no", status = 0L)
}

worker_contract_base <- list(
  schema_version = otp_schema(),
  source_commit = expected_commit,
  config_sha256 = oti_file_sha256(config_path),
  runtime_tree_digest = runtime_binding$runtime_tree_digest,
  package_version = runtime_binding$package_version,
  coverage_level = config$coverage_level,
  learning_rate = config$learning_rate
)

run_worker <- function(family, target, chain) {
  profile <- plan$profile[
    plan$family == family & plan$target == target & plan$chain == chain
  ][1L]
  seed <- otp_seed(config, family, target, chain)
  contract <- c(worker_contract_base, list(
    family = family, target = target, chain = chain,
    profile = profile, seed = seed
  ))
  digest <- otf_object_sha256(contract)
  path <- file.path(
    worker_root,
    sprintf("%s_%s_chain%02d.rds", family, tolower(target), chain)
  )
  if (file.exists(path)) {
    existing <- tryCatch(readRDS(path), error = function(e) NULL)
    if (is.list(existing) && identical(existing$contract_digest, digest)) {
      return(list(path = path, resumed = TRUE))
    }
  }
  result <- if (identical(family, "fixed_design")) {
    otp_fixed_chain(
      config, fixed_dgp, fixed_targets, target, chain,
      provenance_control
    )
  } else {
    otp_dlm_chain(
      config, dlm_dgp, dlm_targets, target, chain,
      provenance_control
    )
  }
  envelope <- list(
    contract = contract,
    contract_digest = digest,
    result = result
  )
  otf_atomic_save_rds(envelope, path, compress = FALSE)
  list(path = path, resumed = FALSE)
}

families <- c("fixed_design", "dlm")
targets <- c("RQR", "ET", "SH")
cell_results <- list()
worker_manifest_rows <- list()
run_status <- data.frame()
for (family in families) {
  for (target in targets) {
    rows <- plan$family == family & plan$target == target
    chains <- plan$chain[rows]
    workers <- if (identical(family, "dlm")) {
      min(as.integer(config$dlm$workers), length(chains))
    } else {
      min(2L, length(chains))
    }
    message(
      "[oracle-tilt-publication] ", family, "/", target,
      ": ", length(chains), " chains; workers=", workers
    )
    started <- Sys.time()
    returns <- if (workers > 1L && .Platform$OS.type != "windows") {
      parallel::mclapply(
        chains,
        function(chain) run_worker(family, target, chain),
        mc.cores = workers, mc.preschedule = FALSE, mc.set.seed = FALSE
      )
    } else {
      lapply(chains, function(chain) run_worker(family, target, chain))
    }
    failed <- which(vapply(returns, inherits, logical(1L), "try-error"))
    if (length(failed)) {
      failure <- data.frame(
        family = family, target = target,
        chain = chains[failed],
        message = vapply(returns[failed], as.character, character(1L)),
        stringsAsFactors = FALSE
      )
      otf_atomic_write_csv(failure, file.path(output_root, "failure_log.csv"))
      oti_stop("One or more publication chains failed.")
    }
    paths <- vapply(returns, `[[`, character(1L), "path")
    envelopes <- lapply(paths, readRDS)
    results <- lapply(envelopes, `[[`, "result")
    worker_manifest_rows[[length(worker_manifest_rows) + 1L]] <- data.frame(
      family = family, target = target, chain = chains,
      profile = plan$profile[rows],
      resumed = vapply(returns, `[[`, logical(1L), "resumed"),
      path = basename(paths), bytes = file.info(paths)$size,
      sha256 = vapply(paths, oti_file_sha256, character(1L)),
      stringsAsFactors = FALSE
    )
    cell <- otp_summarize_cell(
      family, target, results,
      if (identical(family, "fixed_design")) fixed_dgp else dlm_dgp,
      if (identical(family, "fixed_design")) fixed_targets else dlm_targets,
      config
    )
    cell_results[[paste(family, target, sep = "/")]] <- cell
    run_status <- rbind(run_status, data.frame(
      family = family, target = target, status = "completed",
      chains_completed = length(chains),
      elapsed_seconds = as.numeric(difftime(
        Sys.time(), started, units = "secs"
      )),
      disposition = cell$fit_summary$disposition,
      stringsAsFactors = FALSE
    ))
    otf_atomic_write_csv(run_status, file.path(output_root, "run_status.csv"))
  }
}

bind_component <- function(name) {
  values <- lapply(cell_results, `[[`, name)
  values <- Filter(function(x) is.data.frame(x) && nrow(x), values)
  if (length(values)) oti_rbind_fill(values) else data.frame()
}
components <- c(
  "fit_summary", "fit_curves", "endpoint_error_density",
  "endpoint_error_summary", "endpoint_error_by_index", "chain_summary",
  "mcmc_diagnostics", "conditional_references", "pathology_summary",
  "trace_summary"
)
for (name in components) {
  value <- bind_component(name)
  if (nrow(value)) {
    otf_atomic_write_csv(value, file.path(output_root, paste0(name, ".csv")))
  }
}
fit_summary <- bind_component("fit_summary")
otf_atomic_write_csv(
  fit_summary[, c(
    "family", "target", "provenance_pass", "hard_diagnostics_pass",
    "strict_diagnostics_pass", "conditional_reference_pass",
    "pathology_pass", "hard_pass", "disposition",
    "scientifically_usable_for_illustration"
  )],
  file.path(output_root, "cell_disposition.csv")
)
worker_manifest <- do.call(rbind, worker_manifest_rows)
otf_atomic_write_csv(
  worker_manifest, file.path(output_root, "worker_manifest.csv")
)

all_completed <- nrow(worker_manifest) == nrow(plan)
all_usable <- all(fit_summary$scientifically_usable_for_illustration)
closeout <- list(
  schema_version = otp_schema(), mode = mode,
  finished_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  planned_chains = nrow(plan), completed_chains = nrow(worker_manifest),
  all_chains_completed = all_completed,
  all_cells_hard_pass = all_usable,
  strict_pass_cells = sum(fit_summary$disposition == "strict_pass"),
  warning_cells = sum(
    fit_summary$disposition == "illustration_warning_ess_only"
  ),
  failed_cells = sum(fit_summary$disposition == "fail"),
  exact_population_oracle_tilts = TRUE,
  cornish_fisher_used = FALSE,
  response_predictive_analysis = FALSE,
  simulation_study = FALSE,
  manuscript_promotion_authorized = FALSE
)
atomic_json(closeout, file.path(output_root, "closeout.json"))

compact <- file.path(output_root, otp_compact_files())
compact <- compact[file.exists(compact)]
manifest <- oti_file_hashes(compact, output_root)
names(manifest)[names(manifest) == "relative_path"] <- "path"
otf_atomic_write_csv(manifest, file.path(output_root, "artifact_manifest.csv"))
otp_verify_manifest(output_root)
message("[oracle-tilt-publication] execution closed: ", output_root)
message(
  "[oracle-tilt-publication] usable cells: ", sum(fit_summary$hard_pass),
  "/", nrow(fit_summary)
)
