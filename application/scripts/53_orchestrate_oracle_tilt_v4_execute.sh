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
    *) echo "Unknown V4 orchestrator argument: $argument" >&2; exit 2 ;;
  esac
done
if [[ -z "$config" || -z "$output_dir" || ! -f "$config" ]]; then
  echo "--config and --output-dir are required." >&2
  exit 2
fi
if [[ "${RQR_ORACLE_TILT_V4_CONFIRM:-}" != YES ]]; then
  echo "V4 production execution is fail-closed." >&2
  exit 2
fi

runner="application/scripts/52_run_oracle_tilt_publication_v4.R"
mkdir -p "$output_dir"
output_dir="$(cd "$output_dir" && pwd -P)"
logs="$output_dir/cell_logs"
mkdir -p "$logs"
lifecycle="$output_dir/process_lifecycle.csv"
failure="$output_dir/orchestrator_failure_log.csv"
current_stage="$output_dir/current_stage.csv"
attempt="$(date -u +%Y%m%dT%H%M%SZ)_$$"

if [[ ! -f "$lifecycle" ]]; then
  printf '%s\n' \
    "attempt,candidate_id,family,target,child_pid,started_at_utc,finished_at_utc,elapsed_seconds,exit_status,pass" \
    >"$lifecycle"
fi

atomic_append() {
  local row="$1"
  local temporary="${lifecycle}.tmp.$$"
  cp "$lifecycle" "$temporary"
  printf '%s\n' "$row" >>"$temporary"
  mv -T "$temporary" "$lifecycle"
}

write_stage() {
  local stage="$1"
  local status="$2"
  local temporary="${current_stage}.tmp.$$"
  printf '%s\n' "updated_at_utc,attempt,stage,status" >"$temporary"
  printf '%s,%s,%s,%s\n' \
    "$(date -u +%Y-%m-%dT%H:%M:%SZ)" "$attempt" "$stage" "$status" \
    >>"$temporary"
  mv -T "$temporary" "$current_stage"
}

write_stage prepare running
Rscript "$runner" --mode=execute --execute-stage=prepare \
  "--config=$config" "--output-dir=$output_dir"
write_stage prepare completed

cell_plan="$output_dir/cell_plan.csv"
mapfile -t planned_cells < <(
  Rscript --vanilla - "$cell_plan" <<'RSCRIPT'
args <- commandArgs(trailingOnly = TRUE)
plan <- utils::read.csv(args[[1L]], stringsAsFactors = FALSE)
required <- c(
  "candidate_id", "family", "target", "order", "cell_key",
  "process_isolation", "maximum_chain_workers"
)
valid <- nrow(plan) == 18L && all(required %in% names(plan)) &&
  identical(as.integer(plan$order), 1:18) &&
  !anyDuplicated(paste(plan$candidate_id, plan$family, plan$target)) &&
  all(plan$process_isolation) && all(plan$maximum_chain_workers == 1L)
if (!valid) quit(save = "no", status = 1L)
for (index in seq_len(nrow(plan))) {
  cat(plan$candidate_id[index], plan$family[index], plan$target[index],
      plan$cell_key[index], sep = "\t")
  cat("\n")
}
RSCRIPT
)
if [[ "${#planned_cells[@]}" -ne 18 ]]; then
  echo "The authoritative V4 cell plan is invalid." >&2
  exit 1
fi

declare -a pids candidates families targets keys starts start_epochs
write_stage cells running
for index in "${!planned_cells[@]}"; do
  IFS=$'\t' read -r candidate family target key <<<"${planned_cells[$index]}"
  starts[$index]="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
  start_epochs[$index]="$(date +%s)"
  candidates[$index]="$candidate"
  families[$index]="$family"
  targets[$index]="$target"
  keys[$index]="$key"
  Rscript "$runner" --mode=execute --execute-stage=cell \
    "--candidate=$candidate" "--family=$family" "--target=$target" \
    "--config=$config" "--output-dir=$output_dir" \
    >"$logs/${key}.stdout.log" 2>"$logs/${key}.stderr.log" &
  pids[$index]=$!
done

failed=0
printf '%s\n' \
  "recorded_at_utc,attempt,candidate_id,family,target,exit_status,stderr_log" \
  >"$failure"
for index in "${!pids[@]}"; do
  set +e
  wait "${pids[$index]}"
  status=$?
  set -e
  finished="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
  elapsed=$(( $(date +%s) - ${start_epochs[$index]} ))
  pass=TRUE
  if (( status != 0 )); then
    pass=FALSE
    failed=$((failed + 1))
    printf '%s,%s,%s,%s,%s,%s,%s\n' \
      "$finished" "$attempt" "${candidates[$index]}" \
      "${families[$index]}" "${targets[$index]}" "$status" \
      "cell_logs/${keys[$index]}.stderr.log" >>"$failure"
  fi
  atomic_append \
    "$attempt,${candidates[$index]},${families[$index]},${targets[$index]},${pids[$index]},${starts[$index]},$finished,$elapsed,$status,$pass"
done

if (( failed > 0 )); then
  write_stage cells failed
  echo "$failed V4 cells failed; finalize and selection were not run." >&2
  exit 1
fi
rm -f "$failure"
write_stage cells completed
write_stage finalize running
Rscript "$runner" --mode=execute --execute-stage=finalize \
  "--config=$config" "--output-dir=$output_dir"
write_stage complete completed

echo "[oracle-tilt-v4-orchestrator] 18 cells and deterministic selection completed"
