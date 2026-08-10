#!/usr/bin/env Rscript

# Prepare exact worker task files for the fail-closed S05/S06 affected-wave
# validation. This script creates no fits and cannot authorize a scientific
# simulation run.

args <- commandArgs(trailingOnly = TRUE)
parse_arg <- function(name, default = "") {
  prefix <- paste0("--", name, "=")
  value <- grep(paste0("^", prefix), args, value = TRUE)
  if (!length(value)) return(default)
  sub(prefix, "", value[[length(value)]], fixed = TRUE)
}

repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
preflight_root <- parse_arg("preflight-root")
output_root <- parse_arg("output-root")
expected_commit <- tolower(Sys.getenv(
  "RQR_EXPECTED_PRIMARY_COMMIT", unset = ""
))
if (!dir.exists(preflight_root) || !nzchar(output_root) ||
    !grepl("^[0-9a-f]{40}$", expected_commit)) {
  stop(
    "An existing preflight root, fresh output root, and exact commit are required.",
    call. = FALSE
  )
}
preflight_root <- normalizePath(
  preflight_root, winslash = "/", mustWork = TRUE
)
output_root <- normalizePath(
  output_root, winslash = "/", mustWork = FALSE
)
if (file.exists(output_root) || dir.exists(output_root)) {
  stop("The affected-wave preparation output must be new.", call. = FALSE)
}

suppressPackageStartupMessages(library(rqrgibbs))
source(file.path(
  repo_root, "application", "scripts", "lib",
  "rqr_dlm_confirmatory_simulation.R"
))
contract <- rqr_confirm_read_contract(repo_root)
rqr_confirm_validate_contract(contract, require_closed = TRUE)
rqr_confirm_validate_budget(contract)

git_read <- function(arguments) {
  value <- system2(
    Sys.which("git"), c("-C", shQuote(repo_root), arguments),
    stdout = TRUE, stderr = TRUE,
    env = c("GIT_OPTIONAL_LOCKS=0", "GIT_TERMINAL_PROMPT=0")
  )
  status <- attr(value, "status") %||% 0L
  if (!identical(as.integer(status), 0L)) {
    stop("Could not authenticate the primary source.", call. = FALSE)
  }
  trimws(paste(value, collapse = "\n"))
}
if (!identical(tolower(git_read(c("rev-parse", "HEAD"))),
               expected_commit) ||
    nzchar(git_read(c(
      "status", "--porcelain=v2", "--untracked-files=all"
    )))) {
  stop("Affected-wave preparation requires the exact clean source commit.",
       call. = FALSE)
}

rqr_confirm_verify_recursive_manifest(preflight_root)
preflight_manifest <- jsonlite::read_json(
  file.path(preflight_root, "run_manifest.json"), simplifyVector = TRUE
)
gates <- utils::read.csv(
  file.path(preflight_root, "preflight_gates.csv"),
  stringsAsFactors = FALSE, check.names = FALSE
)
if (!identical(preflight_manifest$mode, "preflight") ||
    !identical(tolower(preflight_manifest$source_commit), expected_commit) ||
    !all(gates$value)) {
  stop("The exact-source preflight bundle did not pass.", call. = FALSE)
}

canonical_tasks <- utils::read.csv(
  file.path(preflight_root, "replication_plan_maximum.csv"),
  stringsAsFactors = FALSE, check.names = FALSE
)
canonical_tasks <- rqr_confirm_validate_task_subset(
  canonical_tasks, contract
)
ledger <- utils::read.csv(
  file.path(preflight_root, "seed_ledger_maximum.csv"),
  stringsAsFactors = FALSE, check.names = FALSE
)
rqr_confirm_validate_seed_ledger(
  ledger, contract, planning = "maximum", require_complete = TRUE
)
expected_seed_sha256 <-
  "3dc8483f4a777ab766704b901997295bed1c89db0590429a70f3116b233e948f"
if (!identical(
    rqr_confirm_sha256(file.path(preflight_root, "seed_ledger_maximum.csv")),
    expected_seed_sha256
  )) {
  stop("The frozen maximum seed ledger changed.", call. = FALSE)
}

wave_id <- "local_level_skewed_T200__target0200__sentinel"
wave_plan <- utils::read.csv(
  file.path(preflight_root, "execution_wave_plan_maximum.csv"),
  stringsAsFactors = FALSE, check.names = FALSE
)
affected <- wave_plan[wave_plan$wave_id == wave_id, , drop = FALSE]
method_evaluations <- sum(lengths(strsplit(affected$methods, "|", fixed = TRUE)))
if (nrow(affected) != 35L || method_evaluations != 278L ||
    !all(affected$embedded_sentinel) ||
    !identical(sort(unique(affected$worker_slot)), 1:8) ||
    anyDuplicated(affected$replication_task_id)) {
  stop("The frozen affected-wave incidence was not reproduced.", call. = FALSE)
}

dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
rqr_confirm_atomic_write_csv(
  affected, file.path(output_root, "affected_wave_plan.csv")
)
worker_rows <- vector("list", 8L)
for (slot in 1:8) {
  selected <- affected[affected$worker_slot == slot, , drop = FALSE]
  tasks <- canonical_tasks[match(
    selected$replication_task_id,
    canonical_tasks$replication_task_id
  ), , drop = FALSE]
  rownames(tasks) <- NULL
  tasks <- rqr_confirm_validate_task_subset(tasks, contract)
  worker_rows[[slot]] <- data.frame(
    worker_slot = slot, tasks = nrow(tasks),
    first_execution_order = min(selected$execution_order),
    last_execution_order = max(selected$execution_order),
    stringsAsFactors = FALSE
  )
  rqr_confirm_atomic_write_csv(
    tasks,
    file.path(output_root, sprintf("worker_%02d_tasks.csv", slot))
  )
}
worker_summary <- do.call(rbind, worker_rows)
rqr_confirm_atomic_write_csv(
  worker_summary, file.path(output_root, "worker_summary.csv")
)
manifest <- list(
  schema_version = "rqrgibbs_dlm_affected_wave_preparation/1.0.0",
  source_commit = expected_commit,
  wave_id = wave_id,
  tasks = nrow(affected),
  method_evaluations = method_evaluations,
  worker_slots = 8L,
  all_embedded_sentinel = TRUE,
  seed_ledger_sha256 = expected_seed_sha256,
  confirmatory_execution_authorized = FALSE,
  scientific_promotion = FALSE,
  development_outputs_reusable = FALSE,
  created_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
)
rqr_confirm_atomic_write_json(
  manifest, file.path(output_root, "preparation_manifest.json")
)
hashes <- rqr_confirm_recursive_manifest(output_root)
rqr_confirm_atomic_write_csv(
  hashes, file.path(output_root, "artifact_hashes.csv")
)
cat(sprintf(
  "Prepared %d affected-wave tasks (%d method evaluations) in 8 slots.\n",
  nrow(affected), method_evaluations
))
