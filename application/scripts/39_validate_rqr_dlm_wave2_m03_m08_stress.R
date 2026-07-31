#!/usr/bin/env Rscript

# Production-routed stress gate for the two failures exposed by the first
# confirmatory RQR-DLM run. This gate uses fresh fits from the frozen seed
# ledger. It never changes authorization and never emits scientific results.

args <- commandArgs(trailingOnly = TRUE)
repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(repo_root, "application", "DESCRIPTION"))) {
  stop("Run this script from the RQR-GIBBS repository root.", call. = FALSE)
}

parse_arg <- function(name, default = NULL) {
  prefix <- paste0("--", name, "=")
  value <- grep(paste0("^", prefix), args, value = TRUE)
  if (!length(value)) default else sub(prefix, "", value[[length(value)]])
}

mode <- parse_arg("mode", "preflight")
if (!mode %in% c("preflight", "execute")) {
  stop("--mode must be preflight or execute.", call. = FALSE)
}
scope <- parse_arg("scope", "targeted")
if (!scope %in% c("targeted", "full-wave")) {
  stop("--scope must be targeted or full-wave.", call. = FALSE)
}
seed_ledger_path <- parse_arg("seed-ledger", "")
if (!nzchar(seed_ledger_path) || !file.exists(seed_ledger_path)) {
  stop("--seed-ledger must name the reviewed maximum seed ledger.",
       call. = FALSE)
}
seed_ledger_path <- normalizePath(
  seed_ledger_path, winslash = "/", mustWork = TRUE
)
output_root <- parse_arg(
  "output-root",
  file.path(
    repo_root, "application", "cache",
    paste0("rqr_dlm_wave2_m03_m08_stress_", format(
      Sys.time(), tz = "UTC", format = "%Y%m%dT%H%M%SZ"
    ))
  )
)
output_root <- normalizePath(output_root, winslash = "/", mustWork = FALSE)
if (file.exists(output_root) || dir.exists(output_root)) {
  stop("The stress-gate output root must be new.", call. = FALSE)
}
workers <- suppressWarnings(as.integer(parse_arg("workers", "3")))
if (length(workers) != 1L || is.na(workers) || workers < 1L ||
    workers > 7L) {
  stop("--workers must be one integer in [1, 7].", call. = FALSE)
}

expected_seed_sha256 <-
  "3dc8483f4a777ab766704b901997295bed1c89db0590429a70f3116b233e948f"
expected_commit <- tolower(Sys.getenv(
  "RQR_EXPECTED_PRIMARY_COMMIT", unset = ""
))
primary_attestation <- Sys.getenv(
  "RQR_PRIMARY_RUNTIME_ATTESTATION", unset = ""
)
suppressPackageStartupMessages({
  if (nzchar(expected_commit)) {
    library(rqrgibbs)
  } else if (requireNamespace("pkgload", quietly = TRUE)) {
    pkgload::load_all(file.path(repo_root, "application"), quiet = TRUE)
  } else {
    library(rqrgibbs)
  }
})
source(file.path(
  repo_root, "application", "scripts", "lib",
  "rqr_dlm_confirmatory_simulation.R"
))

git_read <- function(arguments) {
  value <- system2(
    Sys.which("git"), c("-C", shQuote(repo_root), arguments),
    stdout = TRUE, stderr = TRUE,
    env = c("GIT_OPTIONAL_LOCKS=0", "GIT_TERMINAL_PROMPT=0")
  )
  status <- attr(value, "status") %||% 0L
  if (!identical(as.integer(status), 0L)) {
    stop("Could not read the primary Git state.", call. = FALSE)
  }
  paste(value, collapse = "\n")
}
source_commit <- tolower(trimws(git_read(c("rev-parse", "HEAD"))))
source_status <- git_read(c(
  "status", "--porcelain=v2", "--untracked-files=all"
))
if (nzchar(source_status)) {
  stop("The stress gate requires a clean committed checkout.",
       call. = FALSE)
}
if (xor(nzchar(expected_commit), nzchar(primary_attestation)) ||
    (nzchar(expected_commit) &&
      (!grepl("^[0-9a-f]{40}$", expected_commit) ||
        !identical(source_commit, expected_commit)))) {
  stop("The exact-runtime stress binding is incomplete.", call. = FALSE)
}
provenance_control <- if (nzchar(expected_commit)) {
  rqr_confirm_primary_provenance_control(
    repo_root, expected_commit, primary_attestation
  )
} else {
  list()
}

thread_names <- c(
  "OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS",
  "VECLIB_MAXIMUM_THREADS", "NUMEXPR_NUM_THREADS"
)
thread_environment <- setNames(vapply(
  thread_names, Sys.getenv, character(1L), unset = ""
), thread_names)
if (any(thread_environment != "1")) {
  stop("All declared numerical thread variables must equal one.",
       call. = FALSE)
}

contract <- rqr_confirm_read_contract(repo_root)
rqr_confirm_validate_contract(contract, require_closed = TRUE)
seed_ledger_sha256 <- rqr_confirm_sha256(seed_ledger_path)
if (!identical(seed_ledger_sha256, expected_seed_sha256)) {
  stop("The supplied seed ledger is not the reviewed maximum ledger.",
       call. = FALSE)
}

cases <- if (identical(scope, "targeted")) {
  rbind(
    data.frame(
      method = "M03", DGP = "S03",
      replication = c(117L, 13L, 90L, 185L),
      case_role = c("hard_four_chain", rep("guard_four_chain", 3L)),
      chains = 4L, stringsAsFactors = FALSE
    ),
    data.frame(
      method = "M08", DGP = "S03",
      replication = c(13L, 55L, 94L),
      case_role = c("hard_one_chain", rep("guard_one_chain", 2L)),
      chains = 1L, stringsAsFactors = FALSE
    )
  )
} else {
  wave_id <- "local_level_gaussian_T200__target0200__sentinel"
  wave <- rqr_confirm_wave_plan(contract, planning = "maximum")
  wave <- wave[
    wave$wave_id == wave_id & wave$DGP == "S03",
    , drop = FALSE
  ]
  sentinel <- rqr_confirm_sentinel_map(contract, planning = "maximum")
  rows <- list()
  for (method in c("M03", "M08")) {
    cell_id <- contract$incidence$cell_id[
      contract$incidence$DGP == "S03" &
        contract$incidence$method == method
    ]
    if (length(cell_id) != 1L) {
      stop("A full-wave stress method lacks one incidence cell.",
           call. = FALSE)
    }
    is_sentinel <- vapply(wave$replication, function(replication) {
      any(
        sentinel$cell_id == cell_id &
          sentinel$replication == replication
      )
    }, logical(1L))
    rows[[method]] <- data.frame(
      method = method, DGP = wave$DGP,
      replication = wave$replication,
      case_role = ifelse(
        is_sentinel, "full_wave_sentinel", "full_wave_standard"
      ),
      chains = ifelse(is_sentinel, 4L, 1L),
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}
cases <- cases[order(
  match(cases$method, c("M03", "M08")), cases$replication,
  method = "radix"
), , drop = FALSE]
rownames(cases) <- NULL

required_seed_keys <- character()
for (index in seq_len(nrow(cases))) {
  case <- cases[index, , drop = FALSE]
  scenario <- contract$config$scenarios[[case$DGP]]
  data_id <- paste(scenario$dgp, scenario$T, sep = "_T")
  required_seed_keys <- c(
    required_seed_keys,
    paste("training_state", scenario$pair, case$replication, sep = "|"),
    paste("training_response", data_id, case$replication, sep = "|")
  )
  for (subreplication in seq_len(
      contract$config$design$future_subreplications)) {
    required_seed_keys <- c(
      required_seed_keys,
      paste(
        "future_state", scenario$pair, case$replication,
        "subrep", subreplication, sep = "|"
      ),
      paste(
        "future_response", data_id, case$replication,
        "subrep", subreplication, sep = "|"
      )
    )
  }
  cell_id <- contract$incidence$cell_id[
    contract$incidence$DGP == case$DGP &
      contract$incidence$method == case$method
  ]
  required_seed_keys <- c(
    required_seed_keys,
    paste(
      "method", cell_id, case$replication,
      "interval", seq_len(case$chains), sep = "|"
    )
  )
  if (identical(case$method, "M08")) {
    required_seed_keys <- c(
      required_seed_keys,
      paste(
        "forecast", cell_id, case$replication,
        "interval", seq_len(case$chains), sep = "|"
      )
    )
  }
}
required_seed_keys <- unique(required_seed_keys)
if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("The stress gate requires data.table for bounded seed selection.",
       call. = FALSE)
}
ledger_all <- data.table::fread(
  seed_ledger_path, data.table = FALSE, showProgress = FALSE,
  colClasses = "character"
)
ledger <- ledger_all[ledger_all$task_key %in% required_seed_keys, ,
                     drop = FALSE]
rm(ledger_all)
if (length(setdiff(required_seed_keys, ledger$task_key))) {
  stop("The reviewed seed ledger lacks a stress-gate state.",
       call. = FALSE)
}
ledger$substream <- suppressWarnings(as.integer(ledger$substream))
ledger <- rqr_confirm_validate_seed_ledger(
  ledger, contract, planning = "maximum", require_complete = FALSE
)
ledger <- ledger[match(required_seed_keys, ledger$task_key), , drop = FALSE]
seed_bindings <- ledger[
  grepl("^(method|forecast)\\|", ledger$task_key),
  c("task_key", "state_digest"), drop = FALSE
]

dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
rqr_confirm_atomic_write_csv(cases, file.path(output_root, "cases.csv"))
rqr_confirm_atomic_write_csv(
  seed_bindings, file.path(output_root, "seed_bindings.csv")
)
if (identical(mode, "preflight")) {
  rqr_confirm_atomic_write_json(list(
    schema_version = "rqrgibbs_dlm_wave2_stress_preflight/1.0.0",
    source_commit = source_commit,
    source_clean = TRUE,
    fail_closed = TRUE,
    seed_ledger_sha256 = seed_ledger_sha256,
    scope = scope,
    cases = nrow(cases),
    M03_chain_fits = sum(cases$method == "M03") * 4L,
    M08_chain_fits = sum(cases$method == "M08"),
    production_routing = TRUE,
    thresholds_unchanged = TRUE,
    fits_executed = 0L,
    generalized_bayes = TRUE,
    response_likelihood = FALSE
  ), file.path(output_root, "preflight_manifest.json"))
  rqr_confirm_atomic_write_csv(
    rqr_confirm_recursive_manifest(output_root),
    file.path(output_root, "artifact_hashes.csv")
  )
  invisible(rqr_confirm_verify_recursive_manifest(output_root))
  cat("Production-routed M03/M08 stress preflight passed; no fits executed.\n")
  quit(save = "no", status = 0L, runLast = FALSE)
}

started_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
results <- parallel::mclapply(
  seq_len(nrow(cases)), mc.cores = min(workers, nrow(cases)),
  mc.preschedule = TRUE,
  FUN = function(index) {
    case <- cases[index, , drop = FALSE]
    tryCatch({
      generated <- rqr_confirm_generate_dgp(
        contract, case$DGP[[1L]], case$replication[[1L]], ledger
      )
      scalar_chains <- vector("list", case$chains[[1L]])
      profiles <- vapply(seq_len(case$chains[[1L]]), function(chain) {
        rqr_confirm_initialization_profile_name(
          case$chains[[1L]] == 4L, chain
        )
      }, character(1L))
      fits <- vector("list", case$chains[[1L]])
      elapsed <- numeric(case$chains[[1L]])
      for (chain in seq_len(case$chains[[1L]])) {
        fit_started <- proc.time()[["elapsed"]]
        value <- if (identical(case$method[[1L]], "M03")) {
          rqr_confirm_fixed_design(
            contract, generated, chain, ledger,
            provenance_control = provenance_control,
            profile_name = profiles[[chain]]
          )
        } else {
          rqr_confirm_dynamic_fit(
            contract, generated, "M08", chain, ledger,
            provenance_control = provenance_control,
            profile_name = profiles[[chain]]
          )
        }
        elapsed[[chain]] <- proc.time()[["elapsed"]] - fit_started
        scalar_chains[[chain]] <- rqr_confirm_scalar_draws(
          value, generated, contract, case$method[[1L]]
        )
        fits[[chain]] <- list(
          exact_joint_target =
            isTRUE(value$fit$model_spec$exact_joint_target),
          numerical_repair_count =
            as.integer(value$fit$model_spec$numerical_repair_count),
          reproducibility_eligible =
            isTRUE(value$fit$provenance$reproducibility_eligible),
          runtime_tree_digest =
            value$fit$provenance$primary_runtime_tree_digest,
          replica_exchange_operational =
            value$diagnostics$replica_exchange_operational %||% NA,
          replica_swap_attempts =
            value$fit$diagnostics$replica_swap_attempts,
          replica_swap_accepts =
            value$fit$diagnostics$replica_swap_accepts,
          replica_swap_acceptance =
            value$fit$diagnostics$replica_swap_acceptance,
          replica_round_trips =
            value$fit$diagnostics$replica_round_trips,
          cold_labels_visited = sort(unique(
            value$fit$diagnostics$replica_cold_label_trace
          ))
        )
        rm(value)
        invisible(gc(full = TRUE))
      }
      diagnostics <- rqr_confirm_chain_diagnostics(
        scalar_chains, contract,
        sentinel = case$chains[[1L]] == 4L,
        method = case$method[[1L]], generated = generated
      )
      list(
        ok = TRUE, case = as.list(case), profiles = profiles,
        scalar_chains = scalar_chains, fits = fits,
        diagnostics = diagnostics,
        fit_elapsed_seconds = elapsed,
        peak_RSS_KiB = rqr_confirm_process_peak_rss_kib()
      )
    }, error = function(error) {
      list(
        ok = FALSE, case = as.list(case),
        error_class = class(error)[[1L]],
        message_digest = digest::digest(
          conditionMessage(error), algo = "sha256", serialize = FALSE
        ),
        peak_RSS_KiB = rqr_confirm_process_peak_rss_kib()
      )
    })
  }
)
completed_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)

diagnostic_rows <- list()
summary_rows <- list()
replica_rows <- list()
for (index in seq_along(results)) {
  result <- results[[index]]
  case <- result$case
  diagnostics <- if (isTRUE(result$ok)) {
    result$diagnostics
  } else {
    data.frame(
      estimand = "fit_error", chains = case$chains,
      rhat = NA_real_, ess_bulk = NA_real_, ess_tail = NA_real_,
      mcse_mean = NA_real_, mcse_over_sd = NA_real_, pass = FALSE,
      stringsAsFactors = FALSE
    )
  }
  diagnostics$DGP <- case$DGP
  diagnostics$replication <- case$replication
  diagnostics$method <- case$method
  diagnostics$case_role <- case$case_role
  diagnostic_rows[[index]] <- diagnostics
  fit_exact <- isTRUE(result$ok) && all(vapply(
    result$fits, `[[`, logical(1L), "exact_joint_target"
  ))
  repairs <- if (isTRUE(result$ok)) sum(vapply(
    result$fits, `[[`, integer(1L), "numerical_repair_count"
  )) else NA_integer_
  reproducible <- isTRUE(result$ok) && all(vapply(
    result$fits, `[[`, logical(1L), "reproducibility_eligible"
  ))
  replica_operational <- TRUE
  if (isTRUE(result$ok) && identical(case$method, "M03")) {
    replica_operational <- all(vapply(result$fits, function(fit) {
      isTRUE(fit$replica_exchange_operational)
    }, logical(1L)))
    for (chain in seq_along(result$fits)) {
      fit <- result$fits[[chain]]
      for (edge in seq_along(fit$replica_swap_attempts)) {
        replica_rows[[length(replica_rows) + 1L]] <- data.frame(
          DGP = case$DGP, replication = case$replication,
          case_role = case$case_role, chain = chain,
          profile = result$profiles[[chain]], edge = edge,
          attempts = fit$replica_swap_attempts[[edge]],
          accepts = fit$replica_swap_accepts[[edge]],
          acceptance = fit$replica_swap_acceptance[[edge]],
          total_round_trips = sum(fit$replica_round_trips),
          cold_labels_visited = paste(
            fit$cold_labels_visited, collapse = ";"
          ), stringsAsFactors = FALSE
        )
      }
    }
  }
  summary_rows[[index]] <- data.frame(
    method = case$method, DGP = case$DGP,
    replication = case$replication, case_role = case$case_role,
    chains = case$chains, fit_success = isTRUE(result$ok),
    diagnostics = nrow(diagnostics),
    diagnostics_passed = sum(diagnostics$pass),
    all_diagnostics_pass = all(diagnostics$pass),
    exact_joint_target = fit_exact,
    numerical_repair_count = repairs,
    replica_exchange_operational = replica_operational,
    reproducibility_eligible = if (nzchar(expected_commit)) {
      reproducible
    } else {
      NA
    },
    fit_elapsed_seconds = if (isTRUE(result$ok)) {
      sum(result$fit_elapsed_seconds)
    } else {
      NA_real_
    },
    peak_RSS_KiB = as.numeric(result$peak_RSS_KiB %||% NA_real_),
    error_class = as.character(result$error_class %||% ""),
    message_digest = as.character(result$message_digest %||% ""),
    stringsAsFactors = FALSE
  )
}
diagnostics <- do.call(rbind, diagnostic_rows)
summary <- do.call(rbind, summary_rows)
replica_exchange <- if (length(replica_rows)) {
  do.call(rbind, replica_rows)
} else {
  data.frame()
}
rqr_confirm_atomic_write_csv(
  diagnostics, file.path(output_root, "diagnostics.csv")
)
rqr_confirm_atomic_write_csv(
  summary, file.path(output_root, "job_summary.csv")
)
if (nrow(replica_exchange)) {
  rqr_confirm_atomic_write_csv(
    replica_exchange, file.path(output_root, "replica_exchange.csv")
  )
}
temporary_rds <- tempfile(".stress-", tmpdir = output_root)
saveRDS(list(
  schema_version = "rqrgibbs_dlm_wave2_stress_chains/1.0.0",
  source_commit = source_commit, scope = scope, cases = cases,
  seed_ledger_sha256 = seed_ledger_sha256, results = results,
  promotion_evidence = FALSE
), temporary_rds, compress = "xz")
if (!file.rename(
    temporary_rds, file.path(output_root, "chain_evidence_ignored.rds")
  )) {
  stop("Could not atomically publish stress-chain evidence.",
       call. = FALSE)
}

all_pass <- all(summary$fit_success) &&
  all(summary$all_diagnostics_pass) &&
  all(summary$exact_joint_target) &&
  all(summary$numerical_repair_count == 0L) &&
  all(summary$replica_exchange_operational) &&
  (!nzchar(expected_commit) || all(summary$reproducibility_eligible))
manifest <- list(
  schema_version = "rqrgibbs_dlm_wave2_stress_validation/1.0.0",
  source_commit = source_commit,
  source_clean = TRUE,
  package_version = as.character(utils::packageVersion("rqrgibbs")),
  primary_runtime_attestation_sha256 = if (nzchar(primary_attestation)) {
    rqr_confirm_sha256(primary_attestation)
  } else {
    NA_character_
  },
  runtime_tree_digests = unique(unlist(lapply(results, function(result) {
    if (!isTRUE(result$ok)) return(NA_character_)
    vapply(result$fits, `[[`, character(1L), "runtime_tree_digest")
  }))),
  config_digest = digest::digest(
    contract$config, algo = "sha256", serialize = TRUE
  ),
  seed_ledger_sha256 = seed_ledger_sha256,
  scope = scope,
  workers = workers,
  thread_environment = as.list(thread_environment),
  production_routing = TRUE,
  cases = nrow(cases),
  chain_fits = sum(cases$chains),
  diagnostics = nrow(diagnostics),
  diagnostics_passed = sum(diagnostics$pass),
  all_pass = all_pass,
  thresholds_unchanged = TRUE,
  exact_target_required = TRUE,
  numerical_repairs_required = 0L,
  confirmatory_authorization_changed = FALSE,
  promotion_evidence = nzchar(expected_commit) && all_pass,
  comparative_simulation_metrics_used = FALSE,
  generalized_bayes = TRUE,
  response_likelihood = FALSE,
  started_at_utc = started_at,
  completed_at_utc = completed_at
)
rqr_confirm_atomic_write_json(
  manifest, file.path(output_root, "manifest.json")
)
rqr_confirm_atomic_write_csv(
  rqr_confirm_recursive_manifest(output_root),
  file.path(output_root, "artifact_hashes.csv")
)
invisible(rqr_confirm_verify_recursive_manifest(output_root))
cat(sprintf(
  "M03/M08 stress gate: %d/%d diagnostics passed across %d cases.\n",
  sum(diagnostics$pass), nrow(diagnostics), nrow(cases)
))
if (!all_pass) {
  stop("The production-routed M03/M08 stress gate failed.", call. = FALSE)
}
