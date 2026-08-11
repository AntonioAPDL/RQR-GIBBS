#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)"
repo_root="$(cd "$script_dir/../.." && pwd -P)"
cd "$repo_root"

config=""
output_dir=""
for argument in "$@"; do
  case "$argument" in
    --config=*) config="${argument#--config=}" ;;
    --output-dir=*) output_dir="${argument#--output-dir=}" ;;
    *) echo "Unknown V4 resource-rehearsal argument: $argument" >&2; exit 2 ;;
  esac
done
if [[ -z "$config" || ! -f "$config" || -z "$output_dir" ||
      ! -d "$output_dir" ]]; then
  echo "A valid --config and --output-dir are required." >&2
  exit 2
fi
if [[ "${RQR_ORACLE_TILT_V4_REHEARSAL_CONFIRM:-}" != YES ]]; then
  echo "V4 resource rehearsal is fail-closed." >&2
  exit 2
fi

scale="${RQR_ORACLE_TILT_V4_REHEARSAL_SCALE:-1}"
Rscript --vanilla -e '
  x <- as.numeric(commandArgs(TRUE)[1L]);
  if (length(x) != 1L || is.na(x) || !is.finite(x) || x <= 0 || x > 1)
    quit(status = 1L)
' "$scale"

mapfile -t cells < <(Rscript --vanilla - "$config" <<'RSCRIPT'
args <- commandArgs(trailingOnly = TRUE)
source("application/scripts/32_oracle_tilt_illustration_utils.R")
source("application/scripts/33_oracle_tilt_forensic_utils.R")
source("application/scripts/34_oracle_tilt_publication_utils.R")
source("application/scripts/42_oracle_tilt_publication_v3_utils.R")
source("application/scripts/52_oracle_tilt_publication_v4_utils.R")
config <- oti_read_json(args[[1L]])
plan <- otv4_cell_plan(otv4_plan(config))
for (i in seq_len(nrow(plan))) {
  cat(plan$candidate_id[i], plan$family[i], plan$target[i],
      plan$cell_key[i], sep = "\t")
  cat("\n")
}
RSCRIPT
)
if [[ "${#cells[@]}" -ne 18 ]]; then
  echo "The V4 rehearsal requires exactly 18 cells." >&2
  exit 1
fi

lifecycle="$output_dir/rehearsal_lifecycle.csv"
failure="$output_dir/rehearsal_failure_log.csv"
printf '%s\n' \
  "candidate_id,family,target,cell_key,child_pid,started_at_utc,finished_at_utc,elapsed_seconds,exit_status,pass" \
  >"$lifecycle"
printf '%s\n' "candidate_id,family,target,exit_status" >"$failure"
mkdir -p "$output_dir/rehearsal_logs"
declare -a pids candidates families targets keys starts epochs
for index in "${!cells[@]}"; do
  IFS=$'\t' read -r candidate family target key <<<"${cells[$index]}"
  candidates[$index]="$candidate"; families[$index]="$family"
  targets[$index]="$target"; keys[$index]="$key"
  starts[$index]="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
  epochs[$index]="$(date +%s)"
  Rscript application/scripts/54_oracle_tilt_v4_resource_cell.R \
    "--config=$config" "--output-dir=$output_dir" \
    "--candidate=$candidate" "--family=$family" "--target=$target" \
    "--scale=$scale" \
    >"$output_dir/rehearsal_logs/${key}.stdout.log" \
    2>"$output_dir/rehearsal_logs/${key}.stderr.log" &
  pids[$index]=$!
done

failed=0
for index in "${!pids[@]}"; do
  set +e
  wait "${pids[$index]}"
  status=$?
  set -e
  finished="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
  elapsed=$(( $(date +%s) - ${epochs[$index]} ))
  pass=TRUE
  if (( status != 0 )); then
    pass=FALSE; failed=$((failed + 1))
    printf '%s,%s,%s,%s\n' "${candidates[$index]}" \
      "${families[$index]}" "${targets[$index]}" "$status" >>"$failure"
  fi
  printf '%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n' \
    "${candidates[$index]}" "${families[$index]}" "${targets[$index]}" \
    "${keys[$index]}" "${pids[$index]}" "${starts[$index]}" "$finished" \
    "$elapsed" "$status" "$pass" >>"$lifecycle"
done
if (( failed > 0 )); then
  echo "$failed V4 resource cells failed." >&2
  exit 1
fi
rm -f "$failure"

Rscript --vanilla - "$output_dir" "$config" "$scale" <<'RSCRIPT'
args <- commandArgs(trailingOnly = TRUE)
root <- normalizePath(args[[1L]], mustWork = TRUE)
config_path <- normalizePath(args[[2L]], mustWork = TRUE)
scale <- as.numeric(args[[3L]])
source("application/scripts/32_oracle_tilt_illustration_utils.R")
source("application/scripts/33_oracle_tilt_forensic_utils.R")
source("application/scripts/34_oracle_tilt_publication_utils.R")
source("application/scripts/42_oracle_tilt_publication_v3_utils.R")
source("application/scripts/52_oracle_tilt_publication_v4_utils.R")
config <- oti_read_json(config_path)
otv4_validate_config(config)
expected_commit <- Sys.getenv("RQR_EXPECTED_PRIMARY_COMMIT", "")
attestation_path <- Sys.getenv("RQR_PRIMARY_RUNTIME_ATTESTATION", "")
source("application/scripts/lib/isolated_runtime_lineage.R")
source("application/scripts/lib/rqr_dlm_main_simulation.R")
runtime_binding <- rqr_main_primary_runtime_binding(
  normalizePath(".", mustWork = TRUE), expected_commit, attestation_path
)
if (!isTRUE(runtime_binding$match)) {
  oti_stop("The V4 rehearsal runtime does not match the reviewed source.")
}
cell_plan <- otv4_cell_plan(otv4_plan(config))
summaries <- lapply(cell_plan$cell_key, function(key) {
  cell_root <- file.path(root, "resource_cells", key)
  otp_verify_manifest(cell_root)
  utils::read.csv(file.path(cell_root, "cell_summary.csv"),
                  stringsAsFactors = FALSE)
})
summary <- do.call(rbind, summaries)
lifecycle <- utils::read.csv(file.path(root, "rehearsal_lifecycle.csv"),
                             stringsAsFactors = FALSE)
summary$distinct_processes <- length(unique(summary$pid)) == 18L
summary$lifecycle_pass <- lifecycle$pass[match(
  summary$cell_key, lifecycle$cell_key
)]
summary$full_production_shape <- with(
  summary, n_index == n_index_full & n_draw_per_chain == n_draw_per_chain_full
)
summary$pass <- summary$pass & summary$distinct_processes &
  summary$lifecycle_pass
otf_atomic_write_csv(summary, file.path(root, "rehearsal_summary.csv"))
closeout <- list(
  schema_version = "rqrgibbs_oracle_tilt_v4_resource_rehearsal/1.0.0",
  mode = "resource-rehearsal", pass = nrow(summary) == 18L && all(summary$pass),
  source_commit = expected_commit,
  config_sha256 = oti_file_sha256(config_path), scale = scale,
  runtime_tree_digest = runtime_binding$runtime_tree_digest,
  exact_runtime_bound = TRUE,
  compact_evidence_eligible = nrow(summary) == 18L && all(summary$pass) &&
    all(summary$full_production_shape),
  completed_cells = nrow(summary), expected_cells = 18L,
  distinct_processes = length(unique(summary$pid)),
  full_production_shape = all(summary$full_production_shape),
  endpoint_storage_contract = "ordered_endpoints_only",
  interpretation = paste(
    "process-concurrency and endpoint-storage rehearsal;",
    "not MCMC, not a response-predictive analysis, not a simulation study"
  )
)
jsonlite::write_json(closeout, file.path(root, "closeout.json"),
                     pretty = TRUE, auto_unbox = TRUE, digits = NA)
files <- list.files(root, recursive = TRUE, full.names = TRUE)
relative <- sub(paste0("^", root, "/?"), "", files)
wrapper_owned <- c(
  "process_group_monitor.csv", "runner.stdout.log", "runner.stderr.log",
  "resource_summary.csv", "wrapper_closeout.csv",
  "wrapper_artifact_manifest.csv", "wrapper_failure_log.csv"
)
files <- files[relative != "artifact_manifest.csv" &
                 !relative %in% wrapper_owned]
manifest <- oti_file_hashes(files, root)
names(manifest)[names(manifest) == "relative_path"] <- "path"
otf_atomic_write_csv(manifest, file.path(root, "artifact_manifest.csv"))
otp_verify_manifest(root)
if (!isTRUE(closeout$pass)) quit(status = 1L)
RSCRIPT

echo "[oracle-tilt-v4-rehearsal] 18 concurrent endpoint-storage cells passed"
