#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)"
repo_root="$(cd "$script_dir/../.." && pwd -P)"
cd "$repo_root"

output_dir=""
for argument in "$@"; do
  case "$argument" in
    --output-dir=*) output_dir="${argument#--output-dir=}" ;;
    *) echo "Unknown resource-rehearsal argument: $argument" >&2; exit 2 ;;
  esac
done
if [[ -z "$output_dir" || ! -d "$output_dir" ]]; then
  echo "A valid --output-dir is required." >&2
  exit 2
fi

n_index="${RQR_ORACLE_TILT_V3_REHEARSAL_N_INDEX:-2400}"
n_draw="${RQR_ORACLE_TILT_V3_REHEARSAL_N_DRAW:-6000}"
n_chain="${RQR_ORACLE_TILT_V3_REHEARSAL_N_CHAIN:-4}"
if [[ ! "$n_index" =~ ^[0-9]+$ || ! "$n_draw" =~ ^[0-9]+$ ||
      ! "$n_chain" =~ ^[0-9]+$ ]]; then
  echo "Rehearsal dimensions must be positive integers." >&2
  exit 2
fi

lifecycle="$output_dir/rehearsal_lifecycle.csv"
printf '%s\n' \
  "cell,child_pid,started_at_utc,finished_at_utc,elapsed_seconds,exit_status,post_stage_r_processes,pass" \
  >"$lifecycle"
group_id="$(ps -o pgid= -p $$ | awk '{print $1}')"

remaining_r() {
  ps -eo pgid=,stat=,comm= | awk -v group="$group_id" '
    $1 == group && $2 !~ /^Z/ && $3 == "R" { count += 1 }
    END { print count + 0 }
  '
}

for cell in production_shape_a production_shape_b; do
  started="$(date +%s)"
  started_utc="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
  Rscript application/scripts/44_oracle_tilt_v3_resource_cell.R \
    "--output-dir=$output_dir" "--cell=$cell" \
    "--n-index=$n_index" "--n-draw=$n_draw" "--n-chain=$n_chain" &
  child=$!
  set +e
  wait "$child"
  status=$?
  set -e
  finished="$(date +%s)"
  finished_utc="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
  r_left="$(remaining_r)"
  pass=TRUE
  if (( status != 0 || r_left != 0 )); then pass=FALSE; fi
  printf '%s,%s,%s,%s,%s,%s,%s,%s\n' \
    "$cell" "$child" "$started_utc" "$finished_utc" \
    "$((finished - started))" "$status" "$r_left" "$pass" >>"$lifecycle"
  if [[ "$pass" != TRUE ]]; then exit 1; fi
done

Rscript - "$output_dir" "$n_index" "$n_draw" "$n_chain" <<'RSCRIPT'
args <- commandArgs(trailingOnly = TRUE)
root <- normalizePath(args[1L], mustWork = TRUE)
source("application/scripts/32_oracle_tilt_illustration_utils.R")
source("application/scripts/34_oracle_tilt_publication_utils.R")
cells <- c("production_shape_a", "production_shape_b")
values <- lapply(cells, function(cell) {
  cell_root <- file.path(root, "resource_cells", cell)
  otp_verify_manifest(cell_root)
  utils::read.csv(file.path(cell_root, "cell_summary.csv"),
                  stringsAsFactors = FALSE)
})
summary <- do.call(rbind, values)
lifecycle <- utils::read.csv(file.path(root, "rehearsal_lifecycle.csv"),
                             stringsAsFactors = FALSE)
summary$distinct_process <- length(unique(summary$pid)) == nrow(summary)
summary$post_stage_r_processes <- lifecycle$post_stage_r_processes
summary$source_commit <- Sys.getenv("RQR_EXPECTED_PRIMARY_COMMIT", "")
summary$full_production_dimensions <-
  summary$n_index == as.integer(args[2L]) &
  summary$n_draw_per_chain == as.integer(args[3L]) &
  summary$n_chains == as.integer(args[4L])
summary$pass <- summary$pass & summary$distinct_process &
  summary$post_stage_r_processes == 0L & summary$full_production_dimensions
write.csv(summary, file.path(root, "rehearsal_summary.csv"), row.names = FALSE)
files <- list.files(root, recursive = TRUE, full.names = TRUE)
files <- files[basename(files) != "artifact_manifest.csv"]
manifest <- oti_file_hashes(files, root)
names(manifest)[names(manifest) == "relative_path"] <- "path"
write.csv(manifest, file.path(root, "artifact_manifest.csv"), row.names = FALSE)
otp_verify_manifest(root)
if (nrow(summary) != 2L || !all(summary$pass)) quit(status = 1L)
RSCRIPT

echo "[oracle-tilt-v3-rehearsal] two production-shape processes passed"
